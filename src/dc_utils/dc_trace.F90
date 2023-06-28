!= デバッグ時の追跡用モジュール
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dc_trace.F90,v 1.3 2010-04-11 14:13:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! This file provides dc_trace
!

module dc_trace
  !
  != デバッグ時の追跡用モジュール
  !
  ! dc_trace はデバッグ時の原因の追跡を補助するためのサブルーチン群
  ! を持つモジュールです。 このモジュールを利用する事で、
  ! 以下のようにサブルーチンの階層構造がそのまま分かるような
  ! デバッグメッセージを出力する事が可能です。
  !
  !         :
  !    #call HistoryPut0
  !    #| call HistoryPutEx : time
  !    #| | call TimeGoAhead : varname=time head=1.
  !    #| | | call lookup_dimension
  !    #| | | | call gtvarinquire : var.mapid=1
  !    #| | | | | call gdncvarinqurie : var.id=1
  !    #| | | | | end gdncvarinqurie : ok
  !    #| | | | |-name=time
  !    #| | | | end gtvarinquire
  !    #| | | end lookup_dimension : ord=1
  !    #| | | call gtvarslice : var%mapid=1 dimord=1
  !    #| | | |-[gt_variable 1: ndims=1, map.size=1]
  !    #| | | |-[dim1 dimno=1 ofs=0 step=1 all=0 start=1 count=0 stride=1 url=]
  !    #| | | |-[vartable 1: class=netcdf cid=1 ref=1]
  !    #| | | |-[GD_NC_VARIABLE(file=3, var=1, dim=1)]
  !    #| | | |-map(dimord): originally start=1 count=0 stride=1
  !    #| | | |-start=1 (1 specified)
  !    #| | | |-count=1 (1 specified)
  !    #| | | end gtvarslice
  !    #| | end TimeGoAhead
  !    #| |-gdncfiledefinemode
  !    #| end HistoryPutEx
  !    #end HistoryPut0
  !         :
  !
  !== Tutorial
  !
  ! * gtool5 オフィシャルチュートリアル: 
  !   {デバッグ補助}[link:../tutorial/dc_trace.htm]
  !
  !== Procedures list
  !
  ! SetDebug    :: デバッグモードをオンオフ
  ! BeginSub    :: 副プログラム開始のメッセージ出力
  ! EndSub      :: 副プログラム終了のメッセージ出力
  ! DbgMessage  :: デバッグ用メッセージ出力
  !
  !== Usage
  !
  ! dc_trace モジュールを利用するための一連の流れを解説します。
  ! 詳しくは各手続きの詳細を参照してください。
  !
  ! まず、以下の例のように副プログラムの実行文の先頭と最後で
  ! BeginSub と EndSub を使用します。
  !
  !     subroutine TestRoutine(file, var, times, db, url)
  !       use dc_types,  only: STRING, DP
  !       use dc_trace,  only: BeginSub, EndSub
  !       character(len = *), intent(in) :: file, var
  !       integer           , intent(in) :: times
  !       real(DP)          , intent(in) :: db(5)
  !       character(len = *), intent(out):: url
  !       character(len = STRING), parameter:: subname = "TestRoutine"
  !     continue
  !       call BeginSub(subname, 'file=%c, var=%c, times=%d', &
  !         &           c1=trim(file), c2=trim(var), i=(/times/) )
  !       
  !       url = trim(file) // trim(var) // ' ' // ','
  !       url = repeat(trim(url), times)
  !     
  !       call EndSub(subname, 'url=%c', c1=trim(url) )
  !     end subroutine TestRoutine
  !
  ! そして、主プログラムの実行文の先頭で SetDebug を使用します。
  ! 引数は必須ではありませんが、その場合デバッグメッセージは
  ! 標準エラー出力に表示されます。もしも標準出力などその他へ
  ! 出力したい場合は出力したい装置番号を引数として与えてください。
  !
  !     program main
  !       use dc_types,  only: STRING, DP
  !       use dc_trace,  only: SetDebug
  !       character(len = STRING), parameter:: file    = 'test.nc'
  !       character(len = STRING), parameter:: var     = 'div'
  !       integer                , parameter:: times   = 2
  !       character(len = STRING)           :: url
  !       real(DP)                          :: db(5) = (/1.1, 2.2, 3.3, 4.4, 5.5/)
  !       character(len = STRING), parameter:: subname = "TestProgram"
  !     
  !     continue
  !     
  !       call SetDebug
  !     
  !       call TestRoutine(file, var, times, db, url)
  !     
  !       stop
  !     end program main
  !
  ! 上記のプログラムからは以下のようなデバッグメッセージが
  ! 標準エラー出力に出力されます。
  !
  !     #SetDebug: dbg = 0
  !     #call TestRoutine : file=test.nc, var=div, times=2
  !     #end TestRoutine : url=test.ncdiv ,test.ncdiv ,
  !
  ! 以下に注意および補足を記します。
  !
  ! * 上記のように BeginSub よりも前に SetDebug が呼ばれている必要があります。
  ! * BeginSub と同じ回数だけ EndSub が呼ばれていなければなりません。
  ! * 副プログラムの最初と最後以外でデバッグメッセージ
  !   を出力したい場合には DbgMessage を用いて下さい。
  ! * デバッグメッセージとして多次元データを出力したい場合は
  !   DataDump がを用いてください。
  ! * 現在のデバッグモードの状態 (デバッグモードか否か、
  !   副プログラムの深度、出力装置番号) を調べたい場合は、
  !   それぞれ Debug, SubLevel, dbg を利用してください。
  !
  !== Example
  !
  !     program main
  !       use dc_types,  only: STRING, DP
  !       use dc_trace,  only: SetDebug
  !       character(len = STRING), parameter:: file    = 'test.nc'
  !       character(len = STRING), parameter:: var     = 'div'
  !       integer                , parameter:: times   = 2
  !       character(len = STRING)           :: url
  !       real(DP)                          :: db(5) = (/1.1, 2.2, 3.3, 4.4, 5.5/)
  !       character(len = STRING), parameter:: subname = "TestProgram"
  !     
  !     continue
  !     
  !       call SetDebug
  !     
  !       call TestRoutine(file, var, times, db, url)
  !     
  !       stop
  !     end program main
  !
  !     subroutine TestRoutine(file, var, times, db, url)
  !       use dc_types,  only: STRING, DP
  !       use dc_trace,  only: BeginSub, EndSub
  !       character(len = *), intent(in) :: file, var
  !       integer           , intent(in) :: times
  !       real(DP)          , intent(in) :: db(5)
  !       character(len = *), intent(out):: url
  !       character(len = STRING), parameter:: subname = "TestRoutine"
  !     continue
  !       call BeginSub(subname, 'file=%c, var=%c, times=%d', &
  !         &           c1=trim(file), c2=trim(var), i=(/times/) )
  !       
  !       url = trim(file) // trim(var) // ' ' // ','
  !       call DbgMessage('url=%c', c1=trim(url))
  !       url = repeat(trim(url), times)
  !       call DataDump('db', db, strlen=60)
  !
  !       call EndSub(subname, 'url=%c', c1=trim(url) )
  !     end subroutine TestRoutine
  !
  ! 上記のプログラムからは以下のようなデバッグメッセージが
  ! 標準エラー出力に出力されます。
  !
  !     #SetDebug: dbg = 0
  !     #call TestRoutine : file=test.nc, var=div, times=2
  !     #|-url=test.ncdiv ,
  !     #|-db(1-3)=1.1000000238418580000, 2.2000000476837160000, 3.2999999523162840000
  !     #|-db(4-5)=4.4000000953674320000, 5.5000000000000000000
  !     #end TestRoutine : url=test.ncdiv ,test.ncdiv ,
  !
  !
  use dc_types, only: TOKEN, STRING
#ifdef LIB_MPI
  ! MPI ライブラリ
  ! MPI library
  !
  use mpi
#endif
  implicit none

  private

  logical, save         :: lfirst = .true. 
                                        ! 初回フラグ
  integer, save, public :: dbg = -1     ! SetDebug で設定された
                                        ! デバッグメッセージの
                                        ! 出力される装置番号です。
  integer, save         :: level = 0    ! サブルーチンレベル
  integer, parameter    :: trace_stack_size = 128
                                        ! 最大階層数
  character(TOKEN), save:: table(trace_stack_size)
                                        ! 階層⇔プログラム名
  character(STRING), save, allocatable:: called_subname(:), &
    &                                    called_subname_tmp(:)
                                        ! 既に一度呼ばれており,
                                        ! *version* 引数を指定している
                                        ! 副プログラム名を格納する配列
#ifndef LIB_MPI
  character(1), parameter:: head    = '#'  ! 行頭文字
#else
  character(7):: head ! 行頭文字
#endif
  character(2), parameter :: indent  = '| ' ! 字下げ文字
  character(2), parameter :: meshead = '|-' ! DbgMessage 用行頭文字

  public:: BeginSub, EndSub, Debug, SetDebug, DbgMessage, Dbg_Scratch
  public:: SubLevel, DataDump

  interface Debug
    module procedure DCTraceDebug
  end interface

  interface DataDump
    module procedure DataD1Dump, DataD2Dump, DataD3Dump
  end interface

contains

  integer function SubLevel() result(result)
    !
    !== 副プログラムの階層レベルを返す
    !
    ! 副プログラムの階層レベルを返します。 レベルのデフォルトは 0 で、
    ! BeginSub によりレベルは 1 増え、 EndSub によりレベルは 1 減ります。
    !
    result = level
  end function SubLevel

  subroutine Dbg_Scratch(on)
    !
    !== デバッグメッセージの抹消
    !
    ! <b>動作未確認ですので利用の際にはご注意下さい。</b>
    !
    ! 論理型変数 on に .true. を与える事で、
    ! 以降の デバッグメッセージを抹消する事が出来ます。
    !
    ! なお、論理型変数 on に <tt>.false.</tt> を 与える事で、
    ! 直前に呼んだ Dbg_Scratch 以降のメッセージを
    ! デバッグメッセージとして再び出力し、
    ! 以降のデバッグメッセージも 出力されるようにします。
    !
    logical, intent(in):: on
    integer, save:: saved_dbg = -1
    logical:: x, p
    character(80):: line
    integer:: ios
  continue
    if (on) then
      if (dbg < 0) return
      saved_dbg = dbg
      ! 有効な 1 〜 99 の装置番号の内の大きめの値を設定 (?)
      dbg = 98
      do
        inquire(unit=dbg, exist=x, opened=p)
        ! 装置番号 dbg が接続可能で、かつ未接続の場合
        if (x .and. .not. p) then
          ! 装置番号 deg をスクラッチファイルとして開く。
          !   ※ スクラッチファイルとは、特殊な外部ファイルである。
          !      これは名前なしの一時ファイルであり、開いている
          !      間だけ存在する。つまり、プログラムが終了すると
          !      存在しなくなる。
          open(unit=dbg, status='SCRATCH')
          ! 開く事が出来ればそれで終了。
          return
        endif
        ! 装置番号 dbg が利用不可、または利用済の場合は 0 以下に
        ! なるまで dbg - 1 して繰り返す。
        dbg = dbg - 1
        if (dbg < 0) exit
      enddo
      ! 装置番号 dbg が開けない場合、dbg と saved_dbg を初期化
      dbg = saved_dbg
      saved_dbg = -1
    else
      ! 以前に装置番号 dbg = 98〜0 でスクラッチファイルを開けてい
      ! なければそれで終了
      if (saved_dbg < 0) return
      ! 装置番号 dbg に接続されたスクラッチファイルをその開始位置
      ! に位置付ける。エラーが生じたら「100 continue」へ
      rewind(dbg, err=100)
      do
        ! 装置番号 dbg に接続されたスクラッチファイルの一行を
        ! line へ
        read(dbg, '(A)', iostat=ios) line
        if (ios /= 0) exit
        ! line を装置番号 saved_dbg へ書き出す。
        write(saved_dbg, '(A)', iostat=ios) trim(line)
        if (ios /= 0) exit
      enddo
  100 continue
      close(dbg, iostat=ios)
      ! 最後に dbg と saved_dbg を初期化
      dbg = saved_dbg
      saved_dbg = -1
    endif
  end subroutine Dbg_Scratch

  subroutine SetDebug(debug)
    use dc_types, only: STDOUT, STDERR
    implicit none
    !
    !== デバッグモードをオンオフ
    !
    ! デバッグメッセージを出力したい時にこのサブルーチンを呼びます。
    !
    ! 整数型変数 debug が与えられる場合は、その装置番号 debug に、
    ! 以降のサブルーチンによるデバッグメッセージを出力するようにします。
    ! debug が与えられない場合、装置番号 0 (標準エラー出力)
    ! にデバッグメッセージが出力されるようになります。
    ! 装置番号 0 への出力が成功しない場合は代わりに
    ! 装置番号 6 (標準出力) にデバッグメッセージが出力されるようになります。
    !
    ! debug に負の整数を与える場合、デバッグモードが解除され、
    ! 以降デバッグメッセージは出力されません。
    !
    ! なお、この SetDebug を呼んだ際にも、装置番号 debug
    ! に以下のメッセージ が表示されます。
    !
    !     #SetDebug: dbg = debug
    !
    integer, intent(in), optional:: debug
    integer:: ios
#ifdef LIB_MPI
    logical:: initflag_mpi
    character(4):: myrank_str_mpi
    integer:: myrank_mpi, err_mpi
#endif
  continue

#ifdef LIB_MPI
    call MPI_Initialized(initflag_mpi, err_mpi)
    if ( initflag_mpi ) then
      call MPI_Comm_Rank(MPI_COMM_WORLD, myrank_mpi, err_mpi)
      if ( myrank_mpi > 9999 ) then
        head = '#rOVER#'
      else
      write(unit=myrank_str_mpi, fmt="(i4.4)") myrank_mpi
      head = '#r' // myrank_str_mpi // '#'
      end if
    else
      head = '#'
    end if
#endif

    if (present(debug)) then
      ! debug が与えられる時は装置番号として deg を用いる。
      dbg = debug
      write(dbg, "(A, 'SetDebug: dbg =', i4)", iostat=ios) &
        & trim(head), dbg
      if (ios == 0) return
    else
      ! debug が与えられ無い時は装置番号 0 (標準エラー出力)
      dbg = STDERR
      write(dbg, "(A, 'SetDebug: dbg = ', I0)", iostat=ios) trim(head), dbg
      if (ios == 0) return
      ! 装置番号 0 への出力が失敗したら装置番号 6 (標準出力)
      dbg = STDOUT
      write(dbg, "(A, 'SetDebug: dbg = ', I0)", iostat=ios) trim(head), dbg
      if (ios == 0) return
    endif
    ! 例外処理として dbg の初期化
    dbg = -1
  end subroutine SetDebug

  subroutine DCTraceDebug(dbg_mode)
    !
    !== デバックモードかどうかの診断
    !
    ! SetDebugでデバッグモードになっている場合には .true. が、
    ! デバッグモードでない場合には .false. が返ります。
    !
    logical, intent(out):: dbg_mode

    dbg_mode = dbg >= 0
  end subroutine DCTraceDebug

  subroutine initialize
    !
    ! 初期化
    !
    table(:) = ' '
    lfirst = .false.
  end subroutine initialize

  subroutine BeginSub(name, fmt, i, r, d, L, n, c1, c2, c3, ca, &
    & version)
    !
    !== 副プログラム開始のメッセージ出力
    !
    ! 文字型変数 *name* に与えた副プログラム名を以下のように出力します.
    !
    !     # call name
    !
    ! 複数回呼ぶ事で上記 (dc_trace の Overview 参照) 
    ! のようにメッセージが出力されます.
    ! 必ず BeginSub と同様な数だけ EndSub を呼ぶようにしてください.
    !
    ! また, 文字型変数 *fmt* およびそれ以降の引数を与える事で,
    ! 以下のように付加メッセージも出力可能です. *fmt*
    ! とそれ以降の引数に関する書式は dc_string#CPrintf
    ! の説明を参照して下さい.
    !
    !     # call name : fmt
    !
    ! 利用例に関しては dc_trace の Usage および Example を参照してください.
    !
    ! *version* には, 副プログラムのバージョンナンバーを与えます.
    ! *version* に与えられた文字列は, ある副プログラム
    ! が複数回呼び出されたうち, 初回に呼び出された時のみ表示されます.
    !
    !--
    !== 開発者向け解説
    !
    ! このサブルーチンにより, このモジュール内で内部的に保持される
    ! 整数型変数 level の値が 1 増えます。
    !
    !++
    use dc_types, only: STRING, DP
    use dc_string, only: cprintf, StrInclude
    character(*), intent(in)          :: name
    character(*), intent(in), optional:: fmt
    integer,      intent(in), optional:: i(:), n(:)
    real,         intent(in), optional:: r(:)
    real(DP),     intent(in), optional:: d(:)
    logical,      intent(in), optional:: L(:)
    character(*), intent(in), optional:: c1, c2, c3
    character(*), intent(in), optional:: ca(:)
    character(*), intent(in), optional:: version
    character(STRING) :: cbuf
    character(STRING) :: name_ver
    logical :: dbg_mode, print_version
    integer :: alloc_size
  continue
    if ( dbg < 0 ) return
    if (lfirst) call initialize
    call Debug( dbg_mode )
    if ( dbg_mode ) then
      name_ver = name
      print_version = .false.

      !---------------------------------
      !  Print Version check
      if (present(version)) then
        if (.not. allocated(called_subname)) then
          allocate(called_subname(1))
          called_subname(1) = name
          print_version = .true.
        else
          if (.not. StrInclude(called_subname, trim(name))) then
            alloc_size = size(called_subname)
            allocate(called_subname_tmp(alloc_size))
            called_subname_tmp = called_subname
            deallocate(called_subname)
            allocate(called_subname(alloc_size + 1))
            called_subname(1:alloc_size) = called_subname_tmp
            deallocate(called_subname_tmp)
            called_subname(alloc_size + 1) = name
            print_version = .true.
          end if
        end if

        if (print_version) then
          name_ver = cprintf('%c version=<%c>', &
            & c1=trim(name), c2=trim(version))
        end if
      end if

      !---------------------------------
      !  Print Debug message
      if (present(fmt)) then
        cbuf = cprintf(fmt, i, r, d, L, n, c1, c2, c3, ca)
        write(dbg, "(A, A, 'call ', A, ' : ', A)") trim(head), &
          & repeat(indent, level), trim(name_ver), trim(cbuf)
      else
        write(dbg, "(A, A, 'call ',A)") trim(head), & 
          & repeat(indent, level), trim(name_ver)
      endif
    endif
    ! call errtra ! --- for Fujitsu debug
    if (level > size(table)) return
    level = level + 1
    table(level) = name
  end subroutine BeginSub

  subroutine EndSub(name, fmt, i, r, d, L, n, c1, c2, c3, ca)
    !
    !== 副プログラム終了のメッセージ出力
    !
    ! 文字型変数 name に与えた副プログラム名を以下のように出力します。
    !
    !     # end name
    !
    ! BeginSub に対して一対一対応していますので、name には対応する
    ! BeginSub の引数 name と同じものを与えて下さい。
    !
    ! また、文字型変数 fmt およびそれ以降の引数を与える事で、
    ! 以下のように付加メッセージも出力可能です。 fmt
    ! とそれ以降の引数に関する書式は dc_string#CPrintf
    ! の説明を参照して下さい。
    !
    !     # end name fmt
    !
    ! 利用例に関しては dc_trace の Usage および Exampleを参照してください。
    !--
    !== 開発者向け解説
    !
    ! このサブルーチンにより, このモジュール内で内部的に保持される
    ! 整数型変数 level の値が 1 減ります。
    !
    !++
    use dc_types, only: STRING, DP
    use dc_string, only: cprintf
    character(*), intent(in)          :: name
    character(*), intent(in), optional:: fmt
    integer,      intent(in), optional:: i(:), n(:)
    real,         intent(in), optional:: r(:)
    real(DP),     intent(in), optional:: d(:)
    logical,      intent(in), optional:: L(:)
    character(*), intent(in), optional:: c1, c2, c3
    character(*), intent(in), optional:: ca(:)
    character(STRING):: cbuf
    logical:: debug_mode
  continue
    if ( dbg < 0 ) return
    if (lfirst) call initialize
    ! call errtra ! --- for Fujitsu debug
    if (level <= 0) then
      write(*, "(A, 'Warning EndSub[',A,'] without BeginSub')") &
        & trim(head), trim(name)
    else if (name /= table(level)) then
      write(*, "(A, 'Warning EndSub[',A,'] but tos[',A,']')") &
        & trim(head), trim(name), trim(table(level))
    else
      level = level - 1
    endif
    call Debug( debug_mode )
    if ( debug_mode ) then
      if (present(fmt)) then
        cbuf = cprintf(fmt, i, r, d, L, n, c1, c2, c3, ca)
        write(dbg, "(A, A, 'end ', A, ' : ', A)") trim(head), &
          & repeat(indent, level), trim(name), trim(cbuf)
      else
        write(dbg, "(A, A, 'end ', A)") trim(head), &
          & repeat(indent, level), trim(name)
      endif
    endif
  end subroutine EndSub

  subroutine DbgMessage(fmt, i, r, d, L, n, c1, c2, c3, ca)
    !
    !== デバッグ用メッセージ出力
    !
    ! フォーマット文字列 fmt に従ってデバッグメッセージを出力します。
    ! fmt とそれ以降の引数に関する書式は dc_string#CPrintf
    ! の説明を参照して下さい。
    !
    ! 利用例に関しては dc_trace の Example を参照して下さい。
    !
    !--
    !== 開発者向け解説
    !
    ! このサブルーチンを用いても、このモジュール内で内部的に保持される
    ! 整数型変数 level の値は変化しません。
    !
    !++
    use dc_types, only: STRING, DP
    use dc_string, only: cprintf, toChar
    character(*), intent(in)          :: fmt
    integer,      intent(in), optional:: i(:), n(:)
    real,         intent(in), optional:: r(:)
    real(DP),     intent(in), optional:: d(:)
    logical,      intent(in), optional:: L(:)
    character(*), intent(in), optional:: c1, c2, c3
    character(*), intent(in), optional:: ca(:)
    character(STRING):: cbuf
    character(STRING):: meshead_tmp
    integer          :: meshead_len
  continue
    if ( dbg < 0 ) return
    cbuf = cprintf(fmt, i, r, d, L, n, c1, c2, c3, ca)
    if (level < 1) then
      meshead_tmp = ''
      meshead_len = 0
    else
      meshead_tmp = meshead
      meshead_len = len(meshead)
    endif
    write(dbg, "(A, A, A, A)") & 
      & trim(head), repeat( indent, max(level-1, 0) ), &
      & meshead_tmp(1:meshead_len), trim(cbuf)
  end subroutine DbgMessage

  subroutine DataD1Dump(header, d, strlen, multi)
    !
    !== 1 次元データ出力
    !
    ! デバッグメッセージとして、多次元データ d (倍精度実数型)
    ! を出力します。 文字型変数 header は出力時の頭文字として利用されます。
    ! 整数型配列 strlen を与える事で、一行の文字数を指定できます
    ! (デフォルトの文字数は dc_types#STRING で指定されています)。
    ! 整数型配列 multi(:) を与えることで、
    ! header の後ろに次元添字をつける事が可能です。
    !
    ! 利用例に関しては dc_trace の Example を参照して下さい。
    !
    !--
    !== 開発者向け解説
    !
    ! このサブルーチンを用いても、このモジュール内で内部的に保持される
    ! 整数型変数 level の値は変化しません。
    !
    !++
    use dc_types,      only: STRING, DP
    use dc_string,     only: toChar
    character(*), intent(in)          :: header  ! データの名称
    real(DP),     intent(in)          :: d(:)    ! 倍精度実数１次元データ
    integer,      intent(in), optional:: strlen  ! 一行の文字数
    integer,      intent(in), optional:: multi(:)! 上位の次元添字

    integer          :: i, j

    character(STRING):: unit    ! データ文字列
    character(STRING):: unitbuf ! データ文字列バッファ
    integer          :: ucur    ! unit に書かれた文字数
    character(STRING):: cbuf    ! read/write 文のバッファ
    integer          :: stat    ! ステータス

    logical  :: first  ! 1つ目のデータかどうか
    integer  :: begini ! 1つ目のデータの添字
    integer  :: endi   ! 最後のデータの添字

    character(STRING):: cmulti ! 次元添字用文字列
    character(STRING):: cout   ! 出力する文字列

    character(STRING):: meshead_tmp
    integer          :: meshead_len
  continue
    if ( dbg < 0 ) return

    ! 初期化
    unit    = ''
    unitbuf = ''
    ucur    = 0
    stat    = 0
    first = .true.

    cmulti = ''

    ! デバッグメッセージヘッダの作成。
    if (level < 1) then
      meshead_tmp = ''
      meshead_len = 0
    else
      meshead_tmp = meshead
      meshead_len = len(meshead)
    endif

    ! 次元添字用文字列を作成
    if (present(multi)) then
      do j = 1, size(multi)
        cmulti = trim(cmulti) // ', ' // trim(  toChar( multi(j) )  )
      enddo
    endif

    i = 1
    Dim_1_Loop : do
      if (first) begini = i
      endi = i
      write(cbuf, "(g40.20)") d(i)
      if (.not. first) cbuf = ', ' // adjustl(cbuf)
      unitbuf = unit
      call append(unit, ucur, trim(adjustl(cbuf)), stat, strlen)

      if ( stat /= 0 .or. i == size( d(:) ) ) then
        ! 一回目は、文字数オーバーでもそのまま出力。
        if (first) then
          cout = header // '(' &
            &   // trim(toChar(begini)) &
            &   // trim(cmulti) &
            &   // ')=' // trim(unit)
          ! 二回目以降は、オーバーしたものは次回へ
        elseif (stat /= 0 .and. begini == endi-1) then
          cout = header // '(' &
            &   // trim(toChar(begini)) &
            &   // trim(cmulti) &
            &   // ')='// trim(unitbuf)
          ! 1つ巻戻す
          i = i - 1
        elseif (stat /= 0 .and. begini /= endi-1) then
          cout = header // '(' &
            &   // trim(toChar(begini)) // '-' &
            &   // trim(toChar(endi-1)) &
            &   // trim(cmulti) &
            &   // ')=' // trim(unitbuf)
          ! 1つ巻戻す
          i = i - 1
          ! i が size(d) まで到達した場合もそのまま出力。
        elseif ( i == size( d(:) ) ) then
          cout = header // '(' &
            &   // trim(toChar(begini)) // '-' &
            &   // trim(toChar(endi))   &
            &   // trim(cmulti) &
            &   // ')='// trim(unit)
        endif

        write(dbg, "(A, A, A, A)") & 
          & trim(head), repeat( indent, max(level-1, 0) ), &
          & meshead_tmp(1:meshead_len), trim(cout)

        ! unit, unitbuf をクリア
        unit    = ''
        unitbuf = ''
        ucur    = 0
        first = .true.
      else
        first = .false.
      endif
      if (i == size( d(:) ) ) exit Dim_1_Loop
      i = i + 1
    enddo Dim_1_Loop
  end subroutine DataD1Dump

  subroutine DataD2Dump(header, d, strlen, multi)
    !
    !== 2 次元データ出力
    !
    ! 詳しくは DataDump または DataD1Dump を参照ください。
    !
    use dc_types,      only: STRING, DP
    character(*), intent(in)          :: header  ! データの名称
    real(DP),     intent(in)          :: d(:,:)  ! 倍精度実数２次元データ
    integer,      intent(in), optional:: strlen  ! 一行の文字数
    integer,      intent(in), optional:: multi(:)! 上位の次元添字

    integer, allocatable :: total(:)
    integer              :: j

  continue
    if ( dbg < 0 ) return

    if (present(multi)) then
      allocate( total(size(multi)+1) )
      total(2:size(multi)+1) = multi(:)
    else
      allocate( total(1) )
    endif

    do j = 1, size( d(:,:), 2 )
      total(1) = j
      call DataDump(header, d(:,j), strlen=strlen, multi=total(:))
    enddo

    deallocate( total )

  end subroutine DataD2Dump

  subroutine DataD3Dump(header, d, strlen, multi)
    !
    !== 3 次元データ出力
    !
    ! 詳しくは DataDump または DataD1Dump を参照ください。
    !
    use dc_types,      only: STRING, DP
    character(*), intent(in)          :: header  ! データの名称
    real(DP),     intent(in)          :: d(:,:,:)! 倍精度実数３次元データ
    integer,      intent(in), optional:: strlen  ! 一行の文字数
    integer,      intent(in), optional:: multi(:)! 上位の次元添字

    integer, allocatable :: total(:)
    integer              :: k

  continue
    if ( dbg < 0 ) return

    if (present(multi)) then
      allocate( total(size(multi)+1) )
      total(2:size(multi)+1) = multi(:)
    else
      allocate( total(1) )
    endif

    do k = 1, size( d(:,:,:), 3 )
      total(1) = k
      call DataDump(header, d(:,:,k), strlen=strlen, multi=total(:))
    enddo

    deallocate( total )

  end subroutine DataD3Dump

  subroutine append(unit, ucur, val, stat, strlen)
    !
    ! DataD1Dump の内部関数。
    ! unit に val を付加。その際、unit がその最大文字列長を越えた場合
    ! には stat = 2 を返す。
    !
    character(*), intent(inout):: unit ! 最終的に返される文字列
    integer,      intent(inout):: ucur ! unit の文字数
    character(*), intent(in)   :: val  ! unit に付加される文字列
    integer,      intent(out)  :: stat ! ステータス
    integer,      intent(in), &
      &        optional     :: strlen ! 文字数の手動指定

    integer                    :: wrsz ! val の文字列
    continue
    ! unit の最大長を越えた場合には stat = 2 を返す。
    if (present(strlen)) then
      if (ucur >= strlen) then
        stat = 2
        return
      endif
    else
      if (ucur >= len(unit)) then
        stat = 2
        return
      endif
    endif
    ! 正常時の処理。
    ! unit の長さを越えた場合も考慮して unit に val を付加する。
    wrsz = min(len(val), len(unit) - ucur)
    unit(1+ucur: wrsz+ucur) = val(1: wrsz)
    ucur = ucur + wrsz
    stat = 0
    if (wrsz < len(val)) stat = 1
  end subroutine append

end module dc_trace
