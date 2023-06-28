!= GTHST_NMLINFO 型の変数の初期設定
!= Constructor of "GTHST_NMLINFO"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfocreate.f90,v 1.2 2009-10-10 10:59:00 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoCreate( gthstnml, &
    & interval_value, &
    & interval_unit, &
    & precision, &
    & time_average, average, &
    & fileprefix, &
    & origin_value, origin_unit, &
    & terminus_value, terminus_unit, &
    & slice_start, slice_end, slice_stride, &
    & space_average, &
    & newfile_intvalue, newfile_intunit, &
    & err )
    !
    ! GTHST_NMLINFO 型の変数の初期設定を行います.
    ! 他のサブルーチンを使用する前に必ずこのサブルーチンによって
    ! GTHST_NMLINFO 型の変数を初期設定してください.
    !
    ! *interval_value*,
    ! *interval_unit*,
    ! *precision*,
    ! *time_average* (旧 *average*) などの変数
    ! はデフォルト値として設定されます.
    ! *fileprefix* は各変数の出力ファイル名の接頭詞として
    ! 使用されます.
    !
    ! なお, 与えられた *gthstnml* が既に初期設定されている場合,
    ! プログラムはエラーを発生させます.
    !
    ! Constructor of "GTHST_NMLINFO".
    ! Initialize *gthstnml* by this subroutine,
    ! before other procedures are used,
    !
    ! *interval_value*,
    ! *interval_unit*,
    ! *precision*,
    ! *time_average* (now-defunct *average*), etc.
    ! are set as default values.
    ! *fileprefix* is used as prefixes of output filenames of
    ! each variable.
    !
    ! Note that if *gthstnml* is already initialized
    ! by this procedure, error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
    use gtool_history_nmlinfo_internal, only: version
    use gtool_history, only: GT_HISTORY
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_present, only: present_and_not_empty, present_and_true, present_select
    use dc_message, only: MessageNotify
    use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, &
      & DC_EARGLACK, DC_ENEGATIVE, DC_ENOFILEREAD, USR_ERRNO
    use dc_date_types, only: DC_DIFFTIME
    use dc_date, only: DCDiffTimeCreate
    use netcdf, only: NF90_MAX_DIMS
    implicit none
    type(GTHST_NMLINFO), intent(inout):: gthstnml
    real(DP), intent(in), optional:: interval_value
                              ! ヒストリデータの出力間隔の数値.
                              ! 負の値を与えると, 出力を抑止します.
                              !
                              ! Numerical value for interval of history data output.
                              ! Negative values suppresses output.
    character(*), intent(in), optional:: interval_unit
                              ! ヒストリデータの出力間隔の単位.
                              ! Unit for interval of history data output
    character(*), intent(in), optional:: precision
                              ! ヒストリデータの精度.
                              ! Precision of history data
    logical, intent(in), optional:: time_average
                              ! 出力データの時間平均化フラグ.
                              ! Flag for time average of output data.
    logical, intent(in), optional:: average
                              ! time_average の旧版.
                              ! Old version of "time_average"
    character(*), intent(in), optional:: fileprefix
                              ! ヒストリデータのファイル名の接頭詞.
                              ! Prefixes of history data filenames
    real(DP), intent(in), optional:: origin_value
                              ! 出力開始時刻.
                              ! Start time of output.
    character(*), intent(in), optional:: origin_unit
                              ! 出力開始時刻の単位.
                              ! Unit of start time of output.
    real(DP), intent(in), optional:: terminus_value
                              ! 出力終了時刻.
                              ! End time of output.
    character(*), intent(in), optional:: terminus_unit
                              ! 出力終了時刻の単位.
                              ! Unit of end time of output.
    integer, intent(in), optional:: slice_start(:)
                              ! 空間方向の開始点.
                              ! Start points of spaces.
    integer, intent(in), optional:: slice_end(:)
                              ! 空間方向の終了点.
                              ! End points of spaces.
    integer, intent(in), optional:: slice_stride(:)
                              ! 空間方向の刻み幅.
                              ! Strides of spaces.
    logical, intent(in), optional:: space_average(:)
                              ! 平均化のフラグ.
                              ! Flag of average.
    integer, intent(in), optional:: newfile_intvalue
                              ! ファイル分割時間間隔.
                              ! Interval of time of separation of a file.
    character(*), intent(in), optional:: newfile_intunit
                              ! ファイル分割時間間隔の単位.
                              ! Unit of interval of time of separation of a file.
    logical, intent(out), optional:: err
                              ! 例外処理用フラグ.
                              ! デフォルトでは, この手続き内でエラーが
                              ! 生じた場合, プログラムは強制終了します.
                              ! 引数 *err* が与えられる場合,
                              ! プログラムは強制終了せず, 代わりに
                              ! *err* に .true. が代入されます.
                              !
                              ! Exception handling flag.
                              ! By default, when error occur in
                              ! this procedure, the program aborts.
                              ! If this *err* argument is given,
                              ! .true. is substituted to *err* and
                              ! the program does not abort.

    !-----------------------------------
    !  作業変数
    !  Work variables
    type(DC_DIFFTIME):: interval_time
    integer:: stat, ary_size
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoCreate'
  continue
    call BeginSub( subname, &
      & fmt = '@interval_value=%r @interval_unit=%c @precision=%c @time_average=%y @fileprefix=%c', &
      & d  = (/ present_select(.true., -1.0_DP, interval_value) /), &
      & c1 = trim( present_select(.true., '<no>', interval_unit) ), &
      & c2 = trim( present_select(.true., '<no>', precision) ), &
      & l  = (/ present_and_true(time_average) /), &
      & c3 = trim( present_select(.true., '<no>', fileprefix) ), &
      & version = version )
    stat = DC_NOERR
    cause_c = ''

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
    !-----------------------------------------------------------------
    if ( gthstnml % initialized ) then
      stat = DC_EALREADYINIT
      cause_c = 'GTHST_NMLINFO'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  割付
    !  Allocate
    !-----------------------------------------------------------------
    allocate( gthstnml % gthstnml_list )
    nullify( gthstnml % gthstnml_list % next )

    !-----------------------------------------------------------------
    !  デフォルト値の設定
    !  Configure default values
    !-----------------------------------------------------------------
    gthstnml % gthstnml_list % name = ''
    gthstnml % gthstnml_list % file = ''

    allocate( gthstnml % gthstnml_list % interval_value )
    allocate( gthstnml % gthstnml_list % interval_unit  )
    allocate( gthstnml % gthstnml_list % precision      )
    allocate( gthstnml % gthstnml_list % time_average   )
    allocate( gthstnml % gthstnml_list % fileprefix     )

    allocate( gthstnml % gthstnml_list % origin_value                  )
    allocate( gthstnml % gthstnml_list % origin_unit                   )
    allocate( gthstnml % gthstnml_list % terminus_value                )
    allocate( gthstnml % gthstnml_list % terminus_unit                 )
    allocate( gthstnml % gthstnml_list % slice_start   (1:NF90_MAX_DIMS) )
    allocate( gthstnml % gthstnml_list % slice_end     (1:NF90_MAX_DIMS) )
    allocate( gthstnml % gthstnml_list % slice_stride  (1:NF90_MAX_DIMS) )
    allocate( gthstnml % gthstnml_list % space_average (1:NF90_MAX_DIMS) )
    allocate( gthstnml % gthstnml_list % newfile_intvalue              )
    allocate( gthstnml % gthstnml_list % newfile_intunit               )


    gthstnml % gthstnml_list % interval_value = -1.0
    gthstnml % gthstnml_list % interval_unit  = 'sec'
    gthstnml % gthstnml_list % precision      = 'float'
    gthstnml % gthstnml_list % time_average   = .false.
    gthstnml % gthstnml_list % fileprefix     = ''

    gthstnml % gthstnml_list % origin_value     = -1.0
    gthstnml % gthstnml_list % origin_unit      = 'sec'
    gthstnml % gthstnml_list % terminus_value   = -1.0
    gthstnml % gthstnml_list % terminus_unit    = 'sec'
    gthstnml % gthstnml_list % slice_start      =  1
    gthstnml % gthstnml_list % slice_end        = -1
    gthstnml % gthstnml_list % slice_stride     =  1
    gthstnml % gthstnml_list % space_average    = .false.
    gthstnml % gthstnml_list % newfile_intvalue = -1
    gthstnml % gthstnml_list % newfile_intunit  = 'sec'

    if ( present(interval_value) ) gthstnml % gthstnml_list % interval_value = interval_value
    if ( present(interval_unit)  ) gthstnml % gthstnml_list % interval_unit  = interval_unit
    if ( present(precision)      ) gthstnml % gthstnml_list % precision      = precision

    if ( present(average)        ) gthstnml % gthstnml_list % time_average        = average
    if ( present(time_average)   ) gthstnml % gthstnml_list % time_average        = time_average
    if ( present(fileprefix)     ) gthstnml % gthstnml_list % fileprefix     = fileprefix

    if ( present(origin_value    ) ) gthstnml % gthstnml_list % origin_value     = origin_value
    if ( present(origin_unit     ) ) gthstnml % gthstnml_list % origin_unit      = origin_unit
    if ( present(terminus_value  ) ) gthstnml % gthstnml_list % terminus_value   = terminus_value
    if ( present(terminus_unit   ) ) gthstnml % gthstnml_list % terminus_unit    = terminus_unit
    if ( present(slice_start     ) ) then
      ary_size = size(slice_start)
      gthstnml % gthstnml_list % slice_start(1:ary_size)  = slice_start
    end if
    if ( present(slice_end      ) ) then
      ary_size = size(slice_end)
      gthstnml % gthstnml_list % slice_end(1:ary_size)    = slice_end
    end if
    if ( present(slice_stride   ) ) then
      ary_size = size(slice_stride)
      gthstnml % gthstnml_list % slice_stride(1:ary_size) = slice_stride
    end if
    if ( present(space_average   ) ) then
      ary_size = size(space_average)
      gthstnml % gthstnml_list % space_average(1:ary_size) = space_average
    end if
    if ( present(newfile_intvalue) ) gthstnml % gthstnml_list % newfile_intvalue = newfile_intvalue
    if ( present(newfile_intunit ) ) gthstnml % gthstnml_list % newfile_intunit  = newfile_intunit

    !-----------------------------------------------------------------
    !  時間の単位のチェック
    !  Check unit of time
    !-----------------------------------------------------------------
    call DCDiffTimeCreate( &
      & diff = interval_time, &                                    ! (out)
      & value = &
      &   real( gthstnml % gthstnml_list % interval_value, DP ), & ! (in)
      & unit = gthstnml % gthstnml_list % interval_unit, &         ! (in)
      & err = err )                                                ! (out)
    if ( present_and_true( err ) ) then
      stat = USR_ERRNO
      goto 999
    end if

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
    gthstnml % initialized = .true.
    gthstnml % define_mode = .true.
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoCreate
