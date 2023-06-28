!= 暦の設定
!= Setting of calendar
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccalcreate.f90,v 1.4 2009-10-18 12:02:32 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!

subroutine DCCalCreate1( cal_type, cal, err )
  !
  ! 暦の設定を行います. 
  !
  ! このサブルーチンは "dc_calendar" モジュールで用意した
  ! 既定の暦を設定するものです. 1 ヶ月の日数, 1 日の秒数などを
  ! 任意に指定する場合には, 下記の同名のサブルーチンを使用して下さい. 
  !
  ! *cal_type* として以下のものが有効です. これ以外の文字列
  ! を与えた場合にはエラーが発生します. 大文字と小文字は区別しません. 
  !
  ! gregorian         :: グレゴリオ暦
  ! julian            :: ユリウス暦
  ! noleap            :: 閏年無しの暦
  ! 360day            :: 1ヶ月が 30 日の暦
  ! cyclic            :: ある月の日数を
  !                      「30.6 × 月数 − 前月までの総日数」
  !                      の小数点以下切捨とする暦
  !
  ! 省略可能引数 *cal* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL" 型の変数に暦が
  ! 設定されます. その後の手続きで *cal* を省略した場合には
  ! この暦が使用されます. 
  ! *cal* が省略されない場合にはその変数に暦が設定されます.
  ! その暦を使用する場合, 手続きにその "dc_calendar_types#DC_CAL" 型の変数
  ! を与えてください. 
  !
  ! Set calendar. 
  !
  ! This subroutine set previously-defined calendars by "dc_calendar" module. 
  ! If number of days of a month, number of seconds of a day, etc. 
  ! want to be specified arbitrarily, use a following homonymous subroutine. 
  ! 
  ! Following strings are valid as *cal_type*. 
  ! If any other strings is specified, an error is caused. 
  ! They are not case-sensitive. 
  !
  ! gregorian         :: Gregorian calendar. 
  ! julian            :: Julian calendar. 
  ! noleap            :: A calendar without leap year. 
  ! 360day            :: A calendar in which number of days of a month is 30. 
  ! cyclic            :: A calendar in which number of days of a year is
  !                      "30.6 x (number of months) - (total days until last month)"
  !                      (truncate fractional part). 
  !
  ! If an optional argument *cal* is omitted, 
  ! the calendar setting is stored to a "dc_calendar_types#DC_CAL" 
  ! variable that is saved in the "dc_calendar". 
  ! When *cal* is omitted in subsequent procedures, the internal calendar
  ! is used. 
  ! If *cal* is not omitted, the settings is stored to the *cal*. 
  ! In order to use the calendar setting, use the "dc_calendar_types#DC_CAL" 
  ! varieble to subsequent procedures. 
  !

  use dc_calendar_types, only: DC_CAL, &
    & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN, CAL_360DAY
  use dc_calendar_internal, only: default_cal
  use dc_message, only: MessageNotify
  use dc_string, only: LChar
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADCALTYPE
  use dc_types, only: STRING, DP
  implicit none
  character(*), intent(in):: cal_type
                              ! 既定の暦を指定する文字列. 
                              ! 
                              ! Strings that specify a previously-defined calendar. 
  type(DC_CAL), intent(out), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
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


  ! 作業変数
  ! Work variables
  !
  type(DC_CAL), pointer:: calp =>null()
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: version = &
    & '$Name:  $' // &
    & '$Id: dccalcreate.f90,v 1.4 2009-10-18 12:02:32 morikawa Exp $'
  character(*), parameter:: subname = 'DCCalCreate1'
continue
  call BeginSub( subname, version )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
  end if

!!$  ! 初期設定のチェック
!!$  ! Check initialization
!!$  !
!!$  if ( calp % initialized ) then
!!$    stat = DC_EALREADYINIT
!!$    cause_c = 'DC_CAL'
!!$    goto 999
!!$  end if

  ! 暦の種別の正当性のチェック
  ! Validate a kind of calendar
  !
  select case( LChar(trim(cal_type)) )
  case('cyclic')
    calp % cal_type = CAL_CYCLIC
  case('noleap')
    calp % cal_type = CAL_NOLEAP
  case('julian')
    calp % cal_type = CAL_JULIAN
  case('gregorian')
    calp % cal_type = CAL_GREGORIAN
  case('360day')
    calp % cal_type = CAL_360DAY
  case default
    stat = DC_EBADCALTYPE
    call MessageNotify('W', subname, &
      & 'cal_type=<%c> is invalid calender type.', &
      & c1 = trim(cal_type) )
    goto 999
  end select

  ! 各要素への値の設定
  ! Configure elements
  !
  allocate( calp % day_in_month(1:12) )
  calp % month_in_year = 12
  calp % hour_in_day   = 24
  calp % min_in_hour   = 60
  calp % sec_in_min    = 60.0_DP

  select case( calp % cal_type )
  case(CAL_CYCLIC)
    calp % day_in_month(1:12) = &
      & (/ 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 31, 30 /)
  case(CAL_NOLEAP)
    calp % day_in_month(1:12) = &
      & (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  case(CAL_JULIAN)
    calp % day_in_month(1:12) = &
      & (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  case(CAL_GREGORIAN)
    calp % day_in_month(1:12) = &
      & (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  case(CAL_360DAY)
    calp % day_in_month(1:12) = &
      & (/ 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30 /)
  case default
  end select

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
  calp % initialized = .true.
999 continue
  nullify( calp )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalCreate1

subroutine DCCalCreate2( month_in_year, day_in_month, &
  &                      hour_in_day, min_in_hour, sec_in_min, &
  &                      cal, err )
  !
  ! 暦の設定を行います. 
  !
  ! 1 ヶ月の日数, 1 日の秒数などを引数に指定して下さい. 
  ! グレゴリオ暦やユリウス暦などを利用する場合には
  ! 上記の同名のサブルーチンを使用して下さい. 
  !
  ! 省略可能引数 *cal* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL" 型の変数に暦が
  ! 設定されます. その後の手続きで *cal* を省略した場合には
  ! この暦が使用されます. 
  ! *cal* が省略されない場合にはその変数に暦が設定されます.
  ! その暦を使用する場合, 手続きにその "dc_calendar_types#DC_CAL" 型の変数
  ! を与えてください. 
  !
  ! Set calendar. 
  !
  ! Specify number of days of a month, number of seconds of a day, etc. 
  ! to arguments. If Gregorian calendar, Julian calendar are needed, 
  ! see a foregoing homonymous subroutine. 
  ! 
  ! If an optional argument *cal* is omitted. 
  ! The calendar setting is stored to a "dc_calendar_types#DC_CAL" 
  ! variable that is saved in the "dc_calendar". 
  ! When *cal* is omitted in subsequent procedures, the internal calendar
  ! is used. 
  ! If *cal* is not omitted, the settings is stored to the *cal*. 
  ! In order to use the calendar setting, use the "dc_calendar_types#DC_CAL" 
  ! varieble to subsequent procedures. 
  !
  use dc_calendar_types, only: DC_CAL, CAL_USER_DEFINED
  use dc_calendar_internal, only: default_cal
  use dc_message, only: MessageNotify
  use dc_types, only: DP
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADCALTYPE
  use dc_types, only: STRING
  implicit none
  integer, intent(in):: month_in_year
                              ! 1 年の月数. 
                              ! Months in a year. 
  integer, intent(in):: day_in_month(:)
                              ! 1 ヶ月の日数. 
                              ! Days in months. 
  integer, intent(in):: hour_in_day
                              ! 1 日の時間数. 
                              ! Hours in a day. 
  integer, intent(in):: min_in_hour
                              ! 1 時間の分数. 
                              ! Minutes in a hour. 
  real(DP), intent(in):: sec_in_min
                              ! 1 分の秒数. 
                              ! Seconds in a minute. 
  type(DC_CAL), intent(out), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
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


  ! 作業変数
  ! Work variables
  !
  type(DC_CAL), pointer:: calp =>null()
  integer:: size_day_in_month
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: version = &
    & '$Name:  $' // &
    & '$Id: dccalcreate.f90,v 1.4 2009-10-18 12:02:32 morikawa Exp $'
  character(*), parameter:: subname = 'DCCalCreate2'
continue
  call BeginSub( subname, version )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
  end if

!!$  ! 初期設定のチェック
!!$  ! Check initialization
!!$  !
!!$  if ( calp % initialized ) then
!!$    stat = DC_EALREADYINIT
!!$    cause_c = 'DC_CAL'
!!$    goto 999
!!$  end if

  ! 月数の算出
  ! Evaluate number of months
  !
  size_day_in_month = size ( day_in_month ) 

  ! 引数の正当性のチェック
  ! Validate arguments
  !
  if ( .not. month_in_year == size_day_in_month ) then
    stat = DC_EBADCALTYPE
    call MessageNotify('W', subname, &
      & 'month_in_year=<%d> is not equal to size of day_in_month=<%d>', &
      & i = (/ month_in_year, size_day_in_month /) )
    goto 999
  end if

  if ( month_in_year < 1 ) then
    stat = DC_EBADCALTYPE
    call MessageNotify('W', subname, 'month_in_year=<%d> must be positive', &
      & i = (/ month_in_year /) )
    goto 999
  end if

  if ( hour_in_day < 1 ) then
    stat = DC_EBADCALTYPE
    call MessageNotify('W', subname, 'hour_in_day=<%d> must be positive', &
      & i = (/ hour_in_day /) )
    goto 999
  end if

  if ( min_in_hour < 1 ) then
    stat = DC_EBADCALTYPE
    call MessageNotify('W', subname, 'min_in_hour=<%d> must be positive', &
      & i = (/ min_in_hour /) )
    goto 999
  end if

  if ( .not. sec_in_min > 0.0_DP ) then
    stat = DC_EBADCALTYPE
    call MessageNotify('W', subname, 'sec_in_min=<%f> must be positive', &
      & d = (/ sec_in_min /) )
    goto 999
  end if

  ! 各要素への値の設定
  ! Configure elements
  !
  calp % cal_type      = CAL_USER_DEFINED
  calp % month_in_year = month_in_year
  allocate( calp % day_in_month(1:size_day_in_month) )
  calp % day_in_month  = day_in_month 
  calp % hour_in_day   = hour_in_day  
  calp % min_in_hour   = min_in_hour  
  calp % sec_in_min    = sec_in_min   

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
  calp % initialized = .true.
999 continue
  nullify( calp )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalCreate2
