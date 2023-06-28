!= CPU 時間の計測
!= Monitor of CPU TIME
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_clock.F90,v 1.1 2009-03-20 09:09:53 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2006. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module dc_clock
  !
  != CPU 時間の計測
  != Monitor of CPU TIME
  !
  ! プログラムの処理に要した CPU 時間を計測して表示します.
  !
  !== Tutorial
  !
  ! * gtool5 オフィシャルチュートリアル: 
  !   {CPU 時間の計測}[link:../tutorial/dc_clock.htm]
  !
  !== Procedures list
  !
  ! DCClockCreate               :: CLOCK 型変数の初期設定
  ! DCClockStart                :: 計測の開始
  ! DCClockStop                 :: 計測の一時停止
  ! DCClockClose                :: 構造型 CLOCK 変数の終了処理
  ! DCClockGet, DCClockEvalSec  :: CPU 時間 (単位: 秒) の取得
  ! DCClockToChar               :: CPU 時間を適当に整形して文字型変数に変換
  ! DCClockPutLine              :: 構造型 CLOCK 変数の情報を表示
  ! DCClockResult               :: CPU 時間に関する総合的な情報を表示
  ! DCClockPredict              :: プログラムが終了するまでの予測 CPU 時間, 
  !                                および日時を表示
  ! DCClockSetName              :: 名称の再設定
  ! #operator(+)                :: 加算 (dc_clock#CLOCK 型同士)
  ! #operator(-)                :: 減算 (dc_clock#CLOCK 型同士)
  !
  !== Usage
  !
  ! 始めに, 構造型 CLOCK の変数を定義し, DCClockCreate で初期化します.
  ! プログラム中の計測開始地点で DCClockStart を呼び出し,
  ! 計測を一時停止する地点で DCClockStop を呼び出します.
  ! DCClockResult によって経過時間を表示します.
  ! DCClockPredict を使用することでプログラムが終了するまでの残り CPU 時間
  ! の予測値を表示することが可能です.
  !
  !     program dc_clock_sapmle1
  !       use dc_clock, only: CLOCK, DCClockCreate, DCClockClose, & 
  !         & DCClockStart, DCClockStop, DCClockResult, DCClockPredict, & 
  !         & operator(+)
  !       implicit none
  !       type(CLOCK):: clock1, clock2
  !       integer:: i, j
  !       integer, parameter:: loop_num = 8
  !       real:: a, b
  !
  !       call DCClockCreate( clk = clock1, & ! (out)
  !         & name = 'exponential' )          ! (in)
  !       call DCClockCreate( clk = clock2, & ! (out)
  !         & name = 'four-operations' )      ! (in)
  !       a = 2.0
  !       b = 1.0
  !       do i = 1, loop_num
  !         call DCClockStart( clk = clock1 ) ! (inout)
  !         do j = 1, 1000000
  !           a = (a**2)**0.3 + 1.0
  !         enddo
  !         call DCClockStop( clk = clock1 )  ! (inout)
  !         call DCClockStart( clk = clock2 ) ! (inout)
  !         do j = 1, 1000000
  !           b = b / 3.0 * 2.0 + 1.0 - 1.0e-1
  !         enddo
  !         call DCClockStop( clk = clock2 ) ! (inout)
  !         call DCClockPredict( &
  !           & clk = clock1 + clock2, &            ! (in)
  !           & progress = real(i)/real(loop_num) ) ! (in)
  !       enddo
  !       call DCClockResult( &
  !         & clks = (/clock1, clock2/), &  ! (in)
  !         & total_auto = .true. )         ! (in)
  !       call DCClockClose( clk = clock1 ) ! (inout)
  !       call DCClockClose( clk = clock2 ) ! (inout)
  !
  !       write(*,*) 'a = ', a
  !       write(*,*) 'b = ', b
  !     end program dc_clock_sapmle1
  !
  !== Note
  !
  ! CPU 時間はシステム CPU 時間とユーザ CPU 時間とに分けることが
  ! できます. dc_clock では CPU 時間の計測に *cpu_time* サブルーチン
  ! (Fortran 95 規格で導入された組込みサブルーチン) を使用しているため,
  ! 計測された CPU 時間がシステム CPU 時間なのかユーザ CPU 時間なのか,
  ! もしくは両方の合計なのかどうかは処理系の *cpu_time* に依存しています.
  ! (大抵は両方の合計である場合が多いようです).
  !
  !=== 後方互換
  !
  ! バージョン 20071009 以前に利用可能だった以下の手続きは, 
  ! 後方互換のため, しばらくは利用可能です. 
  ! 
  ! * Create, Close, Start, Stop, PutLine, Result, Set_Name
  !   Get, EvalSec, toChar, Predict

  use dc_types, only: STRING, DP
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  use dc_date_types, only: DC_DIFFTIME, DC_DATETIME
#ifdef LIB_MPI
  ! MPI ライブラリ
  ! MPI library
  !
  use mpi
#endif
  implicit none
  private

  public:: CLOCK
  public:: DCClockCreate, DCClockClose, DCClockStart, DCClockStop
  public:: DCClockPutLine, DCClockResult, DCClockSetName
  public:: operator(+), operator(-)
  public:: DCClockGet, DCClockEvalSec, DCClocktoChar, DCClockPredict

  !-----------------------------------------------
  ! 後方互換用
  ! For backward compatibility
  public:: Create, Close, Start, Stop, PutLine, Result, Set_Name
  public:: Get, EvalSec, toChar, Predict

  type CLOCK
    !
    ! CPU 時刻計測用の構造体です.
    ! 初期化には Create を, 終了処理には Close を用います.
    !
    ! 詳しい使い方は dc_clock の Usage を参照ください.
    !
    private
    character(STRING):: name
    real(DP):: start_time           ! 計測を開始した時間
                                    ! (計測の一時停止中には負の値が設定される)
    real(DP):: elapsed_time         ! 経過時間の累計値
    type(DC_DATETIME):: start_date  ! 計測を開始した日時
    logical:: initialized = .false. ! CLOCK 構造体の初期化チェック用フラグ
  end type CLOCK


  interface DCClockCreate
    module procedure DCClockCreate0
  end interface

  interface DCClockClose
    module procedure DCClockClose0
  end interface

  interface DCClockStart
    module procedure DCClockStart0
  end interface

  interface DCClockStop
    module procedure DCClockStop0
  end interface

  interface DCClockPutLine
    module procedure DCClockPutLine0
  end interface

  interface DCClockGet
    module procedure DCClockGetR
    module procedure DCClockGetD
  end interface

  interface DCClockEvalSec
    module procedure DCClockEvalSecD
  end interface

  interface DCClockToChar
    module procedure DCClockToChar0
  end interface

  interface DCClockResult
    module procedure DCClockResult0
  end interface

  interface operator(+)
    module procedure DCClockAdd
  end interface

  interface operator(-)
    module procedure DCClockSubtract
  end interface

  interface DCClockSetName
    module procedure DCClockSetName0
  end interface

  interface DCClockPredict
    module procedure DCClockPredict0
  end interface

  !-----------------------------------------------
  ! 後方互換用
  ! For backward compatibility
  interface Create
    module procedure DCClockCreate0
  end interface

  interface Close
    module procedure DCClockClose0
  end interface

  interface Start
    module procedure DCClockStart0
  end interface

  interface Stop
    module procedure DCClockStop0
  end interface

  interface PutLine
    module procedure DCClockPutLine0
  end interface

  interface Get
    module procedure DCClockGetR
    module procedure DCClockGetD
  end interface

  interface EvalSec
    module procedure DCClockEvalSecD
  end interface

  interface toChar
    module procedure DCClockToChar0
  end interface

  interface Result
    module procedure DCClockResult0
  end interface

  interface Set_Name
    module procedure DCClockSetName0
  end interface

  interface Predict
    module procedure DCClockPredict0
  end interface


  character(*), parameter:: version = &
    & '$Name:  $' // &
    & '$Id: dc_clock.F90,v 1.1 2009-03-20 09:09:53 morikawa Exp $'

contains

  subroutine DCClockCreate0(clk, name)
    !
    !=== CLOCK の初期化用サブルーチン
    !
    ! CLOCK 型の変数を利用する際にはまずこのサブルーチンによって
    ! 初期化を行ってください. *name* には計測内容を与えてください.
    !
    use dc_message, only: MessageNotify
    use dc_date, only: DCDateTimeCreate
    implicit none
    type(CLOCK), intent(out):: clk
    character(*), intent(in):: name
    character(*), parameter:: subname = 'DCClockCreate'
  continue
    call BeginSub(subname, 'name=%c', c1=trim(name), version=version)
    if (clk % initialized) then
      call MessageNotify('W', subname, 'This argument (type CLOCK) is already initialized.')
      call DbgMessage('already initialized')
      goto 999
    end if
    clk % name = name
    clk % elapsed_time = 0.0
    clk % start_time = - 1.0
    clk % initialized = .true.
    call DCDateTimeCreate(clk % start_date)
    call DbgMessage('normal initialized')
999 continue
    call EndSub(subname)
  end subroutine DCClockCreate0

  subroutine DCClockClose0(clk)
    !
    !=== CLOCK の終了サブルーチン
    !
    ! CLOCK 型の変数をクローズします.
    !
    implicit none
    type(CLOCK), intent(inout):: clk
    character(*), parameter:: subname = 'DCClockClose'
  continue
    call BeginSub(subname)
    if (clk % initialized) then
      clk % initialized = .false.
      clk % name = ''
    end if
    call EndSub(subname)
  end subroutine DCClockClose0

  subroutine DCClockStart0(clk, err)
    !
    !=== 計測の開始
    !
    ! このサブルーチンを呼んだ時点で計測を開始します.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます. *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_message, only: MessageNotify
    use dc_string, only: toChar
    use dc_types, only: DP
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    use dc_date, only: EvalSec
    implicit none
    type(CLOCK), intent(inout):: clk
    logical, intent(out), optional:: err
    character(STRING):: cause_c
    integer:: stat
    character(*), parameter:: subname = 'DCClockStart'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    if (.not. clk % initialized) then
      call MessageNotify('W', subname, 'Call Create before Start in dc_clock.')
      call DbgMessage('Ignored because input argument was not initialized.')
      stat = DC_ENOTINIT
      goto 999
    end if
    call cpu_time(clk % start_time) ! (out)
    call DbgMessage('name=%c, cpu_time=%f', &
      & c1=trim(clk % name), d=(/clk % start_time/) )
999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockStart0


  subroutine DCClockStop0(clk, err)
    !
    !=== 計測の一時停止
    !
    ! このサブルーチンを呼んだ時点で計測を一時停止します.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます. *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_message, only: MessageNotify
    use dc_string, only: toChar
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    use dc_date, only: EvalSec, operator(+), operator(-)
    use dc_date_types, only: DC_DIFFTIME
    use dc_types, only: DP
    implicit none
    type(CLOCK), intent(inout):: clk
    logical, intent(out), optional:: err
    character(STRING):: cause_c
    real:: stop_time
    integer:: stat
    character(*), parameter:: subname = 'DCClockStop'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    if (.not. clk % initialized) then
      call MessageNotify('W', subname, 'Call Create before Stop in dc_clock.')
      call DbgMessage('Ignored because input argument was not initialized.')
      stat = DC_ENOTINIT
      goto 999
    elseif (clk % start_time < 0.0_DP) then
      call MessageNotify('W', subname, 'Call Start before Stop in dc_clock.')
      call DbgMessage('Ignored because input argument was not started.')
      goto 999
    end if
    call cpu_time(stop_time)
    clk % elapsed_time = clk % elapsed_time + stop_time - clk % start_time
    clk % start_time = - 1.0
    call DbgMessage('name=%c, cpu_time=%r, elapsed_time=%f', &
      & c1=trim(clk % name), r=(/stop_time/), d=(/clk % elapsed_time/))
999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockStop0

  subroutine DCClockGetR(clk, sec, err) !:doc-priority 40:
    !
    !=== CPU 時間 (単位: 秒) の取得
    !
    ! CPU 時間 (単位: 秒) を *sec* に取得します.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます. *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_types, only: DP
    use dc_message, only: MessageNotify
    use dc_date, only: EvalSec
    use dc_string, only: CPrintf
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    implicit none
    type(CLOCK), intent(in):: clk
    real, intent(out):: sec
    logical, intent(out), optional:: err
    character(STRING):: cause_c
    integer:: stat
    character(*), parameter:: subname = 'DCClockGetR'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    if (.not. clk % initialized) then
      call MessageNotify('W', subname, 'Call Create before Get in dc_clock.')
      call DbgMessage('Ignored because input argument was not initialized.')
      stat = DC_ENOTINIT
      goto 999
    end if
    sec = clk % elapsed_time
    call DbgMessage('name=%c, return sec=<%r>', &
      & c1=trim(clk % name), r=(/sec/))
999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockGetR

  subroutine DCClockGetD(clk, sec, err) !:doc-priority 60:
    !
    !=== CPU 時間 (単位: 秒) の取得
    !
    ! CPU 時間 (単位: 秒) を *sec* に取得します.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます.
    ! 第二引数 *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_types, only: DP
    use dc_string, only: CPrintf
    use dc_message, only: MessageNotify
    use dc_date, only: EvalSec
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    implicit none
    type(CLOCK), intent(in):: clk
    real(DP), intent(out):: sec
    logical, intent(out), optional:: err
    character(STRING):: cause_c
    integer:: stat
    character(*), parameter:: subname = 'DCClockGetD'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    if (.not. clk % initialized) then
      call MessageNotify('W', subname, 'Call Create before Get in dc_clock.')
      call DbgMessage('Ignored because input argument was not initialized.')
      stat = DC_ENOTINIT
      goto 999
    end if
    sec = clk % elapsed_time
    call DbgMessage('name=%c, return sec=<%f>', &
      & c1=trim(clk % name), d=(/sec/))
999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockGetD

  function DCClockEvalSecD(clk) result(result)
    !
    !=== CPU 時間 (単位: 秒) の取得
    !
    ! CPU 時間 (単位: 秒) を返します.
    !
    ! 第一引数 *clk* に対して DCClockCreate 
    ! による初期化が行われていない場合, -1.0 が返ります.
    !
    use dc_types, only: DP
    implicit none
    type(CLOCK), intent(in):: clk
    real(DP):: result
    logical:: err
  continue
    call DCClockGetD(clk, result, err)
    if (err) result = -1.0_DP
  end function DCClockEvalSecD

  function DCClockToChar0(clk) result(result)
    !
    !=== CPU 時間を適当に整形して文字型変数に変換
    !
    ! CPU 時間に関して適当に整形を行い, 文字型変数に変換して返します.
    !
    ! 第一引数 *clk* に対して DCClockCreate
    ! による初期化が行われていない場合, 空文字が返ります.
    !
    use dc_string, only: CPrintf
    use dc_date, only: EvalSec
    implicit none
    type(CLOCK), intent(in):: clk
    character(STRING):: result
    character(20):: clk_name
  continue
    clk_name = clk % name
    if (clk % initialized) then
      result = CPrintf(' %c%c  %c', c1 = clk_name, &
        & c2=trim(result_value_form(clk % elapsed_time)), &
        & c3=trim(fit_unit_value(clk % elapsed_time)))
    else
      result = ''
    end if
  end function DCClockToChar0

  subroutine DCClockPutLine0(clk, unit, indent, err)
    !
    !=== 構造型 CLOCK 変数の情報を表示
    !
    ! 構造型 CLOCK 変数に関する情報を表示します. *unit* には出力先の装置番号を
    ! 与えてください. *unit* を与えない場合, 標準出力へ表示されます.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます. *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_types, only: STDOUT
    use dc_message, only: MessageNotify
    use dc_string, only: Printf, toChar, CPrintf
    use dc_date, only: EvalSec, EvalDay, toChar
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    use dc_types, only: DP
    implicit none
    type(CLOCK), intent(in):: clk
    integer, intent(in), optional:: unit
    character(*), intent(in), optional:: indent
                              ! 表示されるメッセージの字下げ.
                              !
                              ! Indent of displayed messages.
    logical, intent(out), optional:: err
    integer:: out_unit
    character(STRING):: cause_c
    integer:: stat
    integer:: indent_len
    character(STRING):: indent_str
    character(*), parameter:: subname = 'DCClockPutLine'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    if (.not. clk % initialized) then
      call MessageNotify('W', subname, 'Call Create before PutLine in dc_clock.')
      call DbgMessage('Ignored because input argument was not initialized.')
      stat = DC_ENOTINIT
      goto 999
    end if
    if (present(unit)) then
      out_unit = unit
    else
      out_unit = STDOUT
    end if
    indent_len = 0
    indent_str = ''
    if (present(indent)) then
      if (len(indent) /= 0) then
        indent_len = len(indent)
        indent_str(1:indent_len) = indent
      end if
    end if
    call Printf(out_unit, &
      & indent_str(1:indent_len) // &
      & '#<CLOCK:: @name=%c @clocking=%y @elapsed_time=%f sec. %c @start_date=%c>', &
      & c1=trim(clk % name), l=(/clk % start_time > 0.0_DP/), &
      & d=(/clk % elapsed_time/), &
      & c2=trim(fit_unit_value(clk % elapsed_time)), &
      & c3=trim(toChar(clk % start_date)))
    call DbgMessage('name=%c, output to device number %d', &
      & c1=trim(clk % name), i=(/out_unit/))
999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockPutLine0

  subroutine DCClockResult0(clks, unit, total_auto, clk_total, total_name, err)
    !
    !=== CPU 時間の総計を表示
    !
    ! CPU 時間の総計を表示します. *clks* へ, CLOCK 変数の配列を
    ! 与えてください. プログラムの最後で呼び出されることを
    ! 想定しています. *unit* には出力先の装置番号を
    ! 与えてください. *unit* を与えない場合, 標準出力へ表示されます.
    !
    ! 引数 *total_auto* に .true. を与えると, *clks* を全て足し合わせた
    ! 合計値を自動的に表示します. 下記の引数 *clk_total* が与えられている
    ! 場合は *clk_total* が優先されます.
    !
    ! 引数 *clk_total* に CLOCK 変数を与えると, この変数を合計値と
    ! して表示します.
    !
    ! 引数 *total_name* に文字型変数を与えると, 総計メッセージの
    ! 冒頭にこの文字列を出力します.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます. *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_types, only: STDOUT, STRING, DP
    use dc_message, only: MessageNotify
    use dc_string, only: Printf, toChar, CPrintf
    use dc_date, only: EvalSec
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    implicit none
    type(CLOCK), intent(in):: clks(:)
    integer, intent(in), optional:: unit
    logical, intent(in), optional:: total_auto
    type(CLOCK), intent(in), optional:: clk_total
    logical, intent(out), optional:: err
    character(*), intent(in), optional:: total_name
    integer:: out_unit, i, clks_size, ra
    character(20):: clk_name
    character(STRING):: cause_c
    character(STRING):: total_name_work
    type(CLOCK):: clk_auto_total
    logical:: total_print_complete
    real(DP):: elapsed_time_val_cor
    integer:: stat
    character(*), parameter:: total_time_mes = '       TOTAL TIME = '
#ifdef LIB_MPI
    logical:: initflag_mpi
    integer:: err_mpi
#endif
    integer:: myrank_mpi, nprocs_mpi
    character(*), parameter:: subname = 'DCClockResult'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    clks_size = size(clks)
    do i = 1, clks_size
      if (.not. clks(i) % initialized) then
        call MessageNotify('W', subname, 'Call Create before Result in dc_clock.')
        call DbgMessage('Ignored because input argument was not initialized.')
        stat = DC_ENOTINIT
        goto 999
      end if
    end do
    if (present(unit)) then
      out_unit = unit
    else
      out_unit = STDOUT
    end if
    if (present(total_name)) then
      total_name_work = ' (' // trim(total_name) // ')'
    else
      total_name_work = ''
    end if

    myrank_mpi = -1
    nprocs_mpi = 1
#ifdef LIB_MPI
    call MPI_Initialized(initflag_mpi, err_mpi)
    if ( initflag_mpi ) then
      call MPI_Comm_Rank(MPI_COMM_WORLD, myrank_mpi, err_mpi)
      call MPI_Comm_Size(MPI_COMM_WORLD, nprocs_mpi, err_mpi)
    end if
#endif

    do ra = 0, nprocs_mpi - 1

#ifdef LIB_MPI
      if ( initflag_mpi ) call MPI_Barrier(MPI_COMM_WORLD, err_mpi)
      if ( myrank_mpi > -1 .and. ra /= myrank_mpi ) cycle
#endif

      call Printf(out_unit, '')

      if ( myrank_mpi < 0 ) then
        call Printf(out_unit, &
          & ' ############## CPU TIME SUMMARY%c################', &
          & c1=trim(total_name_work) // ' ')
      else
        call Printf(out_unit, &
          & ' ####### CPU TIME SUMMARY%c#### [rank=%06d] ####', &
          & c1=trim(total_name_work) // ' ', &
          & i = (/myrank_mpi/) )
      end if
      do i = 1, clks_size
        clk_name = clks(i) % name
        elapsed_time_val_cor = clks(i) % elapsed_time
        if (elapsed_time_val_cor < 0.0_DP) elapsed_time_val_cor = 0.0_DP
        call Printf(out_unit, &
          & ' %c%c  %c', c1=clk_name, &
          & c2=trim(result_value_form(elapsed_time_val_cor)), &
          & c3=trim(fit_unit_value(clks(i) % elapsed_time)))
      end do
      total_print_complete = .false.
      if (present(clk_total)) then
        if (clk_total % initialized) then
          call Printf(out_unit, &
            & ' ------------------------------------------------')
          elapsed_time_val_cor = clk_total % elapsed_time
          if (elapsed_time_val_cor < 0.0_DP) elapsed_time_val_cor = 0.0_DP
          call Printf(out_unit, &
            & ' %c%c  %c', c1=total_time_mes, &
            & c2=trim(result_value_form(elapsed_time_val_cor)), &
            & c3=trim(fit_unit_value(clk_total % elapsed_time)))
          total_print_complete = .true.
        end if
      end if

      if (present(total_auto) .and. .not. total_print_complete) then
        if (total_auto) then
          clk_auto_total = clks(1)
          if (clks_size > 1) then
            do i = 2, clks_size
              clk_auto_total = clk_auto_total + clks(i)
            end do
          end if
          call Printf(out_unit, &
            & ' ------------------------------------------------')
          elapsed_time_val_cor = clk_auto_total % elapsed_time
          if (elapsed_time_val_cor < 0.0_DP) elapsed_time_val_cor = 0.0_DP
          call Printf(out_unit, &
            & ' %c%c  %c', c1=total_time_mes, &
            & c2=trim(result_value_form(elapsed_time_val_cor)), &
            & c3=trim(fit_unit_value(clk_auto_total % elapsed_time)))
        end if
      end if

      call DbgMessage('total results, output to device number %d', &
        & i=(/out_unit/))

    end do

999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockResult0

  function result_value_form(value) result(result)
    !
    ! 引数 value として与えられた倍精度実数型のデータを,
    ! 以下のフォーマットに整形して文字型として返します.
    !
    !     0.183400E+02
    !
    use dc_types, only: DP, TOKEN
    implicit none
    character(TOKEN):: result
    real(DP), intent(in):: value
  continue
    write(result, "(e15.6)") value
  end function result_value_form

  function fit_unit_value(sec, diff) result(result)
    !
    ! 引数 sec に与えられた数値を秒と扱い, 
    ! 以下のフォーマットに整形して文字型として返します.
    !
    !     (23.18 days)
    !
    ! 単位は days, hrs., minutes から適当に
    ! 選ばれます. (値が 1 以上の値となるように選ばれます).
    ! 1 分以内の場合は空文字を返します.
    !
    use dc_types, only: DP, TOKEN
    use dc_date_types, only: DC_DIFFTIME
    use dc_date, only: DCDiffTimeCreate, EvalDay, EvalHour, EvalMin, EvalSec
    use dc_types, only: DP
    implicit none
    character(TOKEN):: result
    real(DP), intent(in):: sec
    type(DC_DIFFTIME), intent(in), optional:: diff
    type(DC_DIFFTIME):: diffw
    character(TOKEN):: unit
    real(DP):: value
    character(TOKEN):: cval
  continue
    if ( present(diff) ) then
      diffw = diff
    else
      call DCDiffTimeCreate( diffw, sec = sec )
    end if
    if (EvalDay(diffw) > 1.0_DP) then
      unit = ' days'
      value = EvalDay(diffw)
    elseif (EvalHour(diffw) > 1.0_DP) then
      unit = ' hrs.'
      value = EvalHour(diffw)
    elseif (EvalMin(diffw) > 1.0_DP) then
      unit = ' minutes'
      value = EvalMin(diffw)
    else
      result = ''
      return
    end if
    cval = printf_g5_2(value)
    result = '(' // trim(adjustl(cval)) // trim(unit) // ')'
  end function fit_unit_value

  function printf_g5_2(value) result(result)
    !
    ! 引数 value に与えられた数値データを
    ! 以下のフォーマットに整形して文字型として返します.
    !
    !     23.18
    !      0.23
    !
    use dc_types, only: DP, TOKEN, STRING
    use dc_string, only: CPrintf
    implicit none
    character(TOKEN):: result
    real(DP), intent(in):: value
    character(TOKEN):: int_part, dem_part
    integer:: dem_int
  continue
    write(int_part, "(i20)") int(value)
    dem_int = nint((value - int(value)) * 100)
    if (dem_int < 0) dem_int = - dem_int
    if (dem_int == 100) then
      dem_int = 0
      write(int_part, "(i20)") int(value) + 1
    end if
    dem_part = CPrintf('%02d', i=(/dem_int/))
    result = trim(adjustl(int_part)) // '.' // trim(dem_part)
  end function printf_g5_2


  function DCClockAdd(clk1, clk2) result(clk_total)
    !
    !=== CLOCK 変数を足し合わせる
    !
    ! CLOCK 変数 <b>clk1</b> と <b>clk2</b> を足し合わせます.
    ! 与えられた 2 つの CLOCK  変数の CPU 時間を合計し,
    ! CLOCK 変数として返します. 計測内容の名称は <b>clk1</b> と <b>clk2</b>
    ! の名称を '+' で組み合わせたものとなります.
    !
    use dc_string, only: CPrintf
    use dc_date, only: operator(+), operator(<)
    implicit none
    type(CLOCK), intent(in):: clk1
    type(CLOCK), intent(in):: clk2
    type(CLOCK):: clk_total
  continue
    if (.not. clk1 % initialized .or. .not. clk2 % initialized) then
      clk_total % initialized = .false.
      return
    end if

    clk_total % name = CPrintf('%c+%c', &
      & c1=trim(clk1 % name), c2=trim(clk2 % name))
    clk_total % start_time = - 1.0
    clk_total % initialized = .true.
    clk_total % elapsed_time = 0.0

    if (clk1 % start_date < clk2 % start_date) then
      clk_total % start_date = clk1 % start_date
    else
      clk_total % start_date = clk2 % start_date
    end if

    clk_total % elapsed_time = &
      & clk1 % elapsed_time + clk2 % elapsed_time
  end function DCClockAdd

  function DCClockSubtract(clk1, clk2) result(clk_total)
    !
    !=== CLOCK 変数を足し合わせる
    !
    ! CLOCK 変数 <b>clk1</b> から <b>clk2</b> を引きます. 
    ! 1 つ目の CLOCK 変数の CPU 時間と
    ! 2 つ目の CLOCK 変数の CPU 時間との差を
    ! CLOCK 変数として返します. 計測内容の名称は <b>clk1</b> と <b>clk2</b>
    ! の名称を '-' で組み合わせたものとなります.
    !
    use dc_string, only: CPrintf
    use dc_date, only: operator(-), operator(<)
    implicit none
    type(CLOCK), intent(in):: clk1
    type(CLOCK), intent(in):: clk2
    type(CLOCK):: clk_total
  continue
    if (.not. clk1 % initialized .or. .not. clk2 % initialized) then
      clk_total % initialized = .false.
      return
    end if

    clk_total % name = CPrintf('%c-%c', &
      & c1=trim(clk1 % name), c2=trim(clk2 % name))
    clk_total % start_time = - 1.0
    clk_total % initialized = .true.
    clk_total % elapsed_time = 0.0

    if (clk1 % start_date < clk2 % start_date) then
      clk_total % start_date = clk1 % start_date
    else
      clk_total % start_date = clk2 % start_date
    end if

    clk_total % elapsed_time = &
      & clk1 % elapsed_time - clk2 % elapsed_time
  end function DCClockSubtract

  subroutine DCClockSetName0(clk, name, err)
    !
    !=== 測定内容の名称を変更する.
    !
    ! CLOCK 変数 *clk* の計測内容の名称を変更します.
    ! この名称は Create の *name* 引数で指定されたものです.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます. *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_message, only: MessageNotify
    use dc_string, only: toChar, CPrintf
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    implicit none
    type(CLOCK), intent(inout):: clk
    character(*), intent(in):: name
    logical, intent(out), optional:: err
    character(STRING):: cause_c
    integer:: stat
    character(*), parameter:: subname = 'DCClockSetName'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    if (.not. clk % initialized) then
      call MessageNotify('W', subname, 'Call Create before Set_Name in dc_clock.')
      call DbgMessage('Ignored because input argument was not initialized.')
      stat = DC_ENOTINIT
      goto 999
    end if
    clk % name = name
    call DbgMessage('set new name "%c"', c1=trim(clk % name))
999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockSetName0

  subroutine DCClockPredict0(clk, progress, unit, err)
    !
    !=== プログラムが終了するまでの予測 CPU 時間, および日時を表示
    !
    ! CLOCK 変数 *clk* と *progress* から, プログラムが
    ! 終了するまでの予測 CPU 時間, および日時を以下のように表示します.
    !
    !     ########## PREDICTION OF CALCULATION ###########
    !     Start Date             2007-03-08T16:49:25+09:00
    !     Current Date           2007-03-08T16:49:27+09:00
    !     Progress     66.67%  [****************         ]
    !     Remaining CPU TIME      0.100000E+01
    !     Completion Date        2007-03-08T16:49:28+09:00
    !
    ! 第2引数である *progress* には 0 〜 1 までの値を与えてください.
    ! プログラムの開始時を 0, 終了時を 1 とします. (例えば,
    ! プログラムが半分進んだ時には 0.5 を与えます).
    !
    ! ここで行う「予測」とは, これまでの経過時間および
    ! 終了したプログラムの分量から単純なアルゴリズムで割り出している
    ! ものなので, 正確な予測値を返すわけではありません.
    ! あくまで目安として利用してください.
    !
    ! 引数 *unit* には出力先の装置番号を
    ! 与えてください. *unit* を与えない場合, 標準出力へ表示されます.
    !
    ! 第一引数 *clk* に対して DCClockCreate による初期化が行われていない場合,
    ! エラーを発生させます. *err* を与える場合には *err* に .true. が返り,
    ! プログラムは続行されます.
    !
    use dc_types, only: STDOUT, DP
    use dc_message, only: MessageNotify
    use dc_string, only: toChar, CPrintf, Printf
    use dc_error, only: StoreError, DC_ENOTINIT, DC_NOERR
    use dc_date_types, only: DC_DIFFTIME, DC_DATETIME
    use dc_date, only: operator(+), DCDateTimeCreate, toChar, EvalSec, &
      & DCDiffTimeCreate
    implicit none
    type(CLOCK), intent(in):: clk
    real, intent(in):: progress
    integer, intent(in), optional:: unit
    logical, intent(out), optional:: err
    character(STRING):: cause_c
    integer:: stat, out_unit
    type(DC_DIFFTIME):: remain_diff
    type(DC_DATETIME):: comp_date, cur_date
    character(7):: prog_percent
    character(25):: prog_bar
    integer:: prog_bar_ptr
    real:: prog_valid
#ifdef LIB_MPI
    logical:: initflag_mpi
    integer:: err_mpi, myrank_mpi
#endif
    character(*), parameter:: subname = 'DCClockPredict'
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = 'CLOCK'
    if (.not. clk % initialized) then
      call MessageNotify('W', subname, 'Call Create before Predict in dc_clock.')
      call DbgMessage('Ignored because input argument was not initialized.')
      stat = DC_ENOTINIT
      goto 999
    end if
    if (progress <= 0.0) then
      call MessageNotify('W', subname, 'Specify 0.0 -- 1.0 value to "progress"')
      return
    elseif (progress > 1.0) then
      call MessageNotify('W', subname, 'Over 1.0 value to "progress" was modified to 1.0')
      prog_valid = 1.0
    else
      prog_valid = progress
    end if

    if (present(unit)) then
      out_unit = unit
    else
      out_unit = STDOUT
    end if

    call DCDiffTimeCreate( remain_diff, &
      & sec = real(nint(EvalSec(clk) / prog_valid * (1.0 - prog_valid)), DP) )
    call DCDateTimeCreate(cur_date)
    comp_date = cur_date + remain_diff
    prog_percent = ''
    prog_percent = adjustr(trim(printf_g5_2(real(prog_valid * 100, DP))) // '%')
    prog_bar = ''
    prog_bar_ptr = int(prog_valid * 25)
    if (prog_bar_ptr > 0) prog_bar(1:prog_bar_ptr) = '*************************'

#ifdef LIB_MPI
    call MPI_Initialized(initflag_mpi, err_mpi)
    if ( initflag_mpi ) then
      call MPI_Comm_Rank(MPI_COMM_WORLD, myrank_mpi, err_mpi)
      if ( myrank_mpi /= 0 ) goto 999
    end if
#endif

    call Printf(out_unit, '')
    call Printf(out_unit, &
      & ' ########## PREDICTION OF CALCULATION ###########')
    call Printf(out_unit, &
      & ' Start Date             %c', c1=trim(toChar(clk % start_date)))
    call Printf(out_unit, &
      & ' Current Date           %c', c1=trim(toChar(cur_date)))
    call Printf(out_unit, &
      & ' Progress     %c [%c]', c1=prog_percent, c2=prog_bar)
    call Printf(out_unit, &
      & ' Remaining CPU TIME   %c %c', &
      & c1=trim(result_value_form(EvalSec(remain_diff))), &
      & c2=trim(fit_unit_value(0.0_DP, remain_diff)))
    call Printf(out_unit, &
      & ' Completion Date        %c', c1=trim(toChar(comp_date)))

999 continue
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine DCClockPredict0


end module dc_clock
