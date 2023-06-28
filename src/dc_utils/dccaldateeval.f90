!= 日時の算出
!= Evaluate date
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldateeval.f90,v 1.6 2010-09-24 00:28:18 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!
subroutine DCCalDateEvalYMDHMS1( &
  & year, month, day, hour, min, sec, elapse_sec, cal, date, err )
  !
  ! 日時の算出と設定を行います. 
  !
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL_DATE" 型の変数に日時が
  ! 設定されます. その後の手続きで *date* を省略した場合には
  ! この日時が使用されます. 
  ! *date* が省略されない場合にはその変数に日時が設定されます. 
  ! その日時を使用する場合, 手続きにその "dc_calendar_types#DC_CAL_DATE" 型の変数
  ! を与えてください. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate and set date. 
  !
  ! If an optional argument *date* is omitted, 
  ! the date setting is stored to a "dc_calendar_types#DC_CAL_DATE" 
  ! variable that is saved in the "dc_calendar". 
  ! When *date* is omitted in subsequent procedures, the internal date
  ! is used. 
  ! If *date* is not omitted, the settings is stored to the *date*. 
  ! In order to use the date setting, use the "dc_calendar_types#DC_CAL_DATE" 
  ! varieble to subsequent procedures. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed seconds *elapse_sec* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !

  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize
  use dc_calendar_generic, only: DCCalDateCreate, DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_EALREADYINIT, DC_EBADDATE, DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  use dc_types, only: DP
  implicit none
  integer, intent(in):: year  ! 起点の年. Year of origin.  
  integer, intent(in):: month ! 起点の月. Month of origin. 
  integer, intent(in):: day   ! 起点の日. Day of origin.   
  integer, intent(in):: hour  ! 起点の時. Hour of origin.  
  integer, intent(in):: min   ! 起点の分. Minute of origin.
  real(DP), intent(in):: sec  ! 起点の秒. Second of origin.
  real(DP), intent(in):: elapse_sec
                              ! *year* 〜 *sec* からの経過秒数. 
                              ! Elapsed seconds from *year* -- *sec*
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
  type(DC_CAL_DATE), intent(out), optional, target:: date
                              ! 経過時間後の日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date and time after elapsed time. 
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
  integer:: wyear, wmonth, wday, whour, wmin
  real(DP):: wsec
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  character(STRING):: e_date_str, e_cal_str
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateEvalYMDHMS1'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( date ) ) then
    datep => date
  else
    datep => default_date
  end if

  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
!!$  if ( .not. datep % initialized ) then
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  wyear  = year
  wmonth = month
  wday   = day
  whour  = hour
  wmin   = min
  wsec   = sec

  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
!!$  if ( elapse_sec < 0.0_DP ) then
!!$    stat = DC_ENEGATIVE
!!$    cause_c = 'elapse_sec'
!!$    goto 999
!!$  end if

  wsec = wsec + elapse_sec

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( wyear, wmonth, wday, whour, wmin, wsec, & ! (inout)
    &                         calp )                                    ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( wyear, wmonth, wday, whour, wmin, wsec, zone = "" )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! オブジェクトの作成
  ! Create an object
  !
  call DCCalDateCreate( &
    & wyear, wmonth, wday, whour, wmin, wsec, & ! (in)
    & datep, zone = "", err = err )             ! (out) optional
  if ( present(err) ) then
    if ( err ) then
      stat = DC_EBADDATE
      goto 999
    end if
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp, datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateEvalYMDHMS1

!-----------------------------------------------------------

subroutine DCCalDateEvalYMDHMS2( &
  & year, month, day, hour, min, sec, elapse_time, units, cal, date, err )
  !
  ! 日時の算出と設定を行います. 
  !
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL_DATE" 型の変数に日時が
  ! 設定されます. その後の手続きで *date* を省略した場合には
  ! この日時が使用されます. 
  ! *date* が省略されない場合にはその変数に日時が設定されます. 
  ! その日時を使用する場合, 手続きにその "dc_calendar_types#DC_CAL_DATE" 型の変数
  ! を与えてください. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過時間 *elapse_time* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate and set date. 
  !
  ! If an optional argument *date* is omitted, 
  ! the date setting is stored to a "dc_calendar_types#DC_CAL_DATE" 
  ! variable that is saved in the "dc_calendar". 
  ! When *date* is omitted in subsequent procedures, the internal date
  ! is used. 
  ! If *date* is not omitted, the settings is stored to the *date*. 
  ! In order to use the date setting, use the "dc_calendar_types#DC_CAL_DATE" 
  ! varieble to subsequent procedures. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed time *elapse_time* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !

  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE, &
    & UNIT_SYMBOL_DAY, UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize, dccaldate_str2usym
  use dc_calendar_generic, only: DCCalDateCreate, DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_EALREADYINIT, DC_EBADDATE, DC_EBADUNIT, &
    & DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  use dc_types, only: DP
  implicit none
  integer, intent(in):: year  ! 起点の年. Year of origin.  
  integer, intent(in):: month ! 起点の月. Month of origin. 
  integer, intent(in):: day   ! 起点の日. Day of origin.   
  integer, intent(in):: hour  ! 起点の時. Hour of origin.  
  integer, intent(in):: min   ! 起点の分. Minute of origin.
  real(DP), intent(in):: sec  ! 起点の秒. Second of origin.
  real(DP), intent(in):: elapse_time
                              ! *year* 〜 *sec* からの経過時間. 
                              ! 単位は *unit* で指定する. 
                              !
                              ! Elapsed time from *year* -- *sec*
                              ! Unit is specified as *unit*.
  character(*), intent(in):: units
                              ! *elapse_time* の単位. 
                              !
                              ! Unit of *elapse_time*. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
  type(DC_CAL_DATE), intent(out), optional, target:: date
                              ! 経過時間後の日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date and time after elapsed time. 
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
  integer:: wyear, wmonth, wday, whour, wmin
  real(DP):: wsec
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  character(STRING):: e_date_str, e_cal_str
  integer:: tusym
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateEvalYMDHMS2'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( date ) ) then
    datep => date
  else
    datep => default_date
  end if

  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
!!$  if ( .not. datep % initialized ) then
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  wyear  = year
  wmonth = month
  wday   = day
  whour  = hour
  wmin   = min
  wsec   = sec

  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
!!$  if ( elapse_time < 0.0_DP ) then
!!$    stat = DC_ENEGATIVE
!!$    cause_c = 'elapse_time'
!!$    goto 999
!!$  end if

  tusym = dccaldate_str2usym(units)
  select case(tusym)
  case(UNIT_SYMBOL_DAY)
    wsec = wsec + elapse_time * calp % hour_in_day &
      &                       * calp % min_in_hour &
      &                       * calp % sec_in_min
  case(UNIT_SYMBOL_HOUR)
    wsec = wsec + elapse_time * calp % min_in_hour &
      &                       * calp % sec_in_min
  case(UNIT_SYMBOL_MIN)
    wsec = wsec + elapse_time * calp % sec_in_min
  case(UNIT_SYMBOL_SEC)
    wsec = wsec + elapse_time
  case default
    cause_c = units
    call MessageNotify('W', subname, 'units=<%c> is invalid. (ONLY day,hrs,min,sec are valid)', &
      & c1 = trim(units) )
    stat = DC_EBADUNIT
    goto 999
  end select

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( wyear, wmonth, wday, whour, wmin, wsec, & ! (inout)
    &                         calp )                                    ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( wyear, wmonth, wday, whour, wmin, wsec, zone = "" )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! オブジェクトの作成
  ! Create an object
  !
  call DCCalDateCreate( &
    & wyear, wmonth, wday, whour, wmin, wsec, & ! (in)
    & datep, zone = "", err = err )             ! (out) optional
  if ( present(err) ) then
    if ( err ) then
      stat = DC_EBADDATE
      goto 999
    end if
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp, datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateEvalYMDHMS2

!-----------------------------------------------------------

subroutine DCCalDateEvalID1( init_date, elapse_sec, cal, date, err )
  !
  ! 日時の算出と設定を行います. 
  !
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL_DATE" 型の変数に日時が
  ! 設定されます. その後の手続きで *date* を省略した場合には
  ! この日時が使用されます. 
  ! *date* が省略されない場合にはその変数に日時が設定されます. 
  ! その日時を使用する場合, 手続きにその "dc_calendar_types#DC_CAL_DATE" 型の変数
  ! を与えてください. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate and set date. 
  !
  ! If an optional argument *date* is omitted, 
  ! the date setting is stored to a "dc_calendar_types#DC_CAL_DATE" 
  ! variable that is saved in the "dc_calendar". 
  ! When *date* is omitted in subsequent procedures, the internal date
  ! is used. 
  ! If *date* is not omitted, the settings is stored to the *date*. 
  ! In order to use the date setting, use the "dc_calendar_types#DC_CAL_DATE" 
  ! varieble to subsequent procedures. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed seconds *elapse_sec* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !

  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize
  use dc_calendar_generic, only: DCCalDateCreate, DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_EALREADYINIT, DC_EBADDATE, DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  use dc_types, only: DP
  implicit none
  type(DC_CAL_DATE), intent(in):: init_date
                              ! 起点となる日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date of origin. 
  real(DP), intent(in):: elapse_sec
                              ! *init_date* からの経過秒数. 
                              ! Elapsed seconds from *init_date*. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
  type(DC_CAL_DATE), intent(out), optional, target:: date
                              ! 経過時間後の日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date and time after elapsed time. 
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
  integer:: wyear, wmonth, wday, whour, wmin
  real(DP):: wsec
  character(TOKEN):: wzone
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  character(STRING):: e_date_str, e_cal_str
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateEvalID1'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( date ) ) then
    datep => date
  else
    datep => default_date
  end if

  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
!!$  if ( .not. datep % initialized ) then
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  wyear  = init_date % year
  wmonth = init_date % month
  wday   = init_date % day
  whour  = init_date % hour
  wmin   = init_date % min
  wsec   = init_date % sec
  wzone  = init_date % zone

  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
!!$  if ( elapse_sec < 0.0_DP ) then
!!$    stat = DC_ENEGATIVE
!!$    cause_c = 'elapse_sec'
!!$    goto 999
!!$  end if

  wsec = wsec + elapse_sec

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( wyear, wmonth, wday, whour, wmin, wsec, & ! (inout)
    &                         calp )                                    ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( wyear, wmonth, wday, whour, wmin, wsec, zone = "" )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! オブジェクトの作成
  ! Create an object
  !
  call DCCalDateCreate( &
    & wyear, wmonth, wday, whour, wmin, wsec, & ! (in)
    & datep, zone = wzone, err = err )          ! (out) optional
  if ( present(err) ) then
    if ( err ) then
      stat = DC_EBADDATE
      goto 999
    end if
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp, datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateEvalID1

!-----------------------------------------------------------

subroutine DCCalDateEvalID2( init_date, elapse_time, units, cal, date, err )
  !
  ! 日時の算出と設定を行います. 
  !
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL_DATE" 型の変数に日時が
  ! 設定されます. その後の手続きで *date* を省略した場合には
  ! この日時が使用されます. 
  ! *date* が省略されない場合にはその変数に日時が設定されます. 
  ! その日時を使用する場合, 手続きにその "dc_calendar_types#DC_CAL_DATE" 型の変数
  ! を与えてください. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過時間 *elapse_time* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate and set date. 
  !
  ! If an optional argument *date* is omitted, 
  ! the date setting is stored to a "dc_calendar_types#DC_CAL_DATE" 
  ! variable that is saved in the "dc_calendar". 
  ! When *date* is omitted in subsequent procedures, the internal date
  ! is used. 
  ! If *date* is not omitted, the settings is stored to the *date*. 
  ! In order to use the date setting, use the "dc_calendar_types#DC_CAL_DATE" 
  ! varieble to subsequent procedures. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed time *elapse_time* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !
  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE, &
    & UNIT_SYMBOL_DAY, UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize, dccaldate_str2usym
  use dc_calendar_generic, only: DCCalDateCreate, DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_EALREADYINIT, DC_EBADDATE, DC_EBADUNIT, &
    & DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  use dc_types, only: DP
  implicit none
  type(DC_CAL_DATE), intent(in):: init_date
                              ! 起点となる日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date of origin. 
  real(DP), intent(in):: elapse_time
                              ! *init_date* からの経過時間. 
                              ! 単位は *unit* で指定する. 
                              !
                              ! Elapsed time from *init_date*. 
                              ! Unit is specified as *unit*.
  character(*), intent(in):: units
                              ! *elapse_time* の単位. 
                              !
                              ! Unit of *elapse_time*. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
  type(DC_CAL_DATE), intent(out), optional, target:: date
                              ! 経過時間後の日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date and time after elapsed time. 
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
  integer:: wyear, wmonth, wday, whour, wmin
  real(DP):: wsec
  character(TOKEN):: wzone
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  character(STRING):: e_date_str, e_cal_str
  integer:: tusym
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateEvalID2'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( date ) ) then
    datep => date
  else
    datep => default_date
  end if

  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
!!$  if ( .not. datep % initialized ) then
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  wyear  = init_date % year
  wmonth = init_date % month
  wday   = init_date % day
  whour  = init_date % hour
  wmin   = init_date % min
  wsec   = init_date % sec
  wzone  = init_date % zone

  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
!!$  if ( elapse_time < 0.0_DP ) then
!!$    stat = DC_ENEGATIVE
!!$    cause_c = 'elapse_time'
!!$    goto 999
!!$  end if

  tusym = dccaldate_str2usym(units)
  select case(tusym)
  case(UNIT_SYMBOL_DAY)
    wsec = wsec + elapse_time * calp % hour_in_day &
      &                       * calp % min_in_hour &
      &                       * calp % sec_in_min
  case(UNIT_SYMBOL_HOUR)
    wsec = wsec + elapse_time * calp % min_in_hour &
      &                       * calp % sec_in_min
  case(UNIT_SYMBOL_MIN)
    wsec = wsec + elapse_time * calp % sec_in_min
  case(UNIT_SYMBOL_SEC)
    wsec = wsec + elapse_time
  case default
    cause_c = units
    call MessageNotify('W', subname, 'units=<%c> is invalid. (ONLY day,hrs,min,sec are valid)', &
      & c1 = trim(units) )
    stat = DC_EBADUNIT
    goto 999
  end select

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( wyear, wmonth, wday, whour, wmin, wsec, & ! (inout)
    &                         calp )                                    ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( wyear, wmonth, wday, whour, wmin, wsec, zone = "" )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! オブジェクトの作成
  ! Create an object
  !
  call DCCalDateCreate( &
    & wyear, wmonth, wday, whour, wmin, wsec, & ! (in)
    & datep, zone = wzone, err = err )          ! (out) optional
  if ( present(err) ) then
    if ( err ) then
      stat = DC_EBADDATE
      goto 999
    end if
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp, datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateEvalID2

!-----------------------------------------------------------

subroutine DCCalDateEvalYM2YM1( &
  & year1, month1, day1, hour1, min1, sec1, &
  & elapse_sec, &
  & year2, month2, day2, hour2, min2, sec2, &
  & cal, err )
  !
  ! 日時の算出と設定を行います. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate and set date. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed seconds *elapse_sec* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !
  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize
  use dc_calendar_generic, only: DCCalDateCreate, DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_EALREADYINIT, DC_EBADDATE, DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  use dc_types, only: DP
  implicit none
  integer, intent(in):: year1  ! 起点の年. Year of origin.
  integer, intent(in):: month1 ! 起点の月. Month of origin.
  integer, intent(in):: day1   ! 起点の日. Day of origin.
  integer, intent(in):: hour1  ! 起点の時. Hour of origin.
  integer, intent(in):: min1   ! 起点の分. Minute of origin.
  real(DP), intent(in):: sec1  ! 起点の秒. Second of origin.
  real(DP), intent(in):: elapse_sec
                              ! *year1* 〜 *sec1* からの経過秒数. 
                              ! Elapsed seconds from *year1* -- *sec1*
  integer, intent(out):: year2  ! 経過時間後の年. Year after elapsed time.
  integer, intent(out):: month2 ! 経過時間後の月. Month after elapsed time.
  integer, intent(out):: day2   ! 経過時間後の日. Day after elapsed time.
  integer, intent(out):: hour2  ! 経過時間後の時. Hour after elapsed time.
  integer, intent(out):: min2   ! 経過時間後の分. Minute after elapsed time.
  real(DP), intent(out):: sec2  ! 経過時間後の秒. Second after elapsed time.
  type(DC_CAL), intent(in), optional, target:: cal
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
  character(STRING):: e_date_str, e_cal_str
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateEvalYM2YM1'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
!!$  if ( .not. datep % initialized ) then
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  year2  = year1
  month2 = month1
  day2   = day1
  hour2  = hour1
  min2   = min1
  sec2   = sec1

  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
!!$  if ( elapse_sec < 0.0_DP ) then
!!$    stat = DC_ENEGATIVE
!!$    cause_c = 'elapse_sec'
!!$    goto 999
!!$  end if

  sec2 = sec2 + elapse_sec

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( year2, month2, day2, hour2, min2, sec2, & ! (inout)
    &                         calp )                                    ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( year2, month2, day2, hour2, min2, sec2, zone = "" )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateEvalYM2YM1

!-----------------------------------------------------------

subroutine DCCalDateEvalYM2YM2( &
  & year1, month1, day1, hour1, min1, sec1, &
  & elapse_time, units, &
  & year2, month2, day2, hour2, min2, sec2, &
  & cal, err )
  !
  ! 日時の算出と設定を行います. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過時間 *elapse_time* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate and set date. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed time *elapse_time* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !
  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE, &
    & UNIT_SYMBOL_DAY, UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize, dccaldate_str2usym
  use dc_calendar_generic, only: DCCalDateCreate, DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_EALREADYINIT, DC_EBADDATE, DC_EBADUNIT, &
    & DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  use dc_types, only: DP
  implicit none

  integer, intent(in):: year1  ! 起点の年. Year of origin.
  integer, intent(in):: month1 ! 起点の月. Month of origin.
  integer, intent(in):: day1   ! 起点の日. Day of origin.
  integer, intent(in):: hour1  ! 起点の時. Hour of origin.
  integer, intent(in):: min1   ! 起点の分. Minute of origin.
  real(DP), intent(in):: sec1  ! 起点の秒. Second of origin.
  real(DP), intent(in):: elapse_time
                              ! *year1* 〜 *sec1* からの経過時間. 
                              ! 単位は *unit* で指定する. 
                              !
                              ! Elapsed time from *year1* -- *sec1*
                              ! Unit is specified as *unit*.
  character(*), intent(in):: units
                              ! *elapse_time* の単位. 
                              !
                              ! Unit of *elapse_time*. 
  integer, intent(out):: year2  ! 経過時間後の年. Year after elapsed time.
  integer, intent(out):: month2 ! 経過時間後の月. Month after elapsed time.
  integer, intent(out):: day2   ! 経過時間後の日. Day after elapsed time.
  integer, intent(out):: hour2  ! 経過時間後の時. Hour after elapsed time.
  integer, intent(out):: min2   ! 経過時間後の分. Minute after elapsed time.
  real(DP), intent(out):: sec2  ! 経過時間後の秒. Second after elapsed time.
  type(DC_CAL), intent(in), optional, target:: cal
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
  character(STRING):: e_date_str, e_cal_str
  integer:: tusym
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateEvalYM2YM1'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
!!$  if ( .not. datep % initialized ) then
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  year2  = year1
  month2 = month1
  day2   = day1
  hour2  = hour1
  min2   = min1
  sec2   = sec1

  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
!!$  if ( elapse_time < 0.0_DP ) then
!!$    stat = DC_ENEGATIVE
!!$    cause_c = 'elapse_time'
!!$    goto 999
!!$  end if

  tusym = dccaldate_str2usym(units)
  select case(tusym)
  case(UNIT_SYMBOL_DAY)
    sec2 = sec2 + elapse_time * calp % hour_in_day &
      &                       * calp % min_in_hour &
      &                       * calp % sec_in_min
  case(UNIT_SYMBOL_HOUR)
    sec2 = sec2 + elapse_time * calp % min_in_hour &
      &                       * calp % sec_in_min
  case(UNIT_SYMBOL_MIN)
    sec2 = sec2 + elapse_time * calp % sec_in_min
  case(UNIT_SYMBOL_SEC)
    sec2 = sec2 + elapse_time
  case default
    cause_c = units
    call MessageNotify('W', subname, 'units=<%c> is invalid. (ONLY day,hour,min,sec are valid)', &
      & c1 = trim(units) )
    stat = DC_EBADUNIT
    goto 999
  end select

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( year2, month2, day2, hour2, min2, sec2, & ! (inout)
    &                         calp )                                    ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( year2, month2, day2, hour2, min2, sec2, zone = "" )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateEvalYM2YM2

!-----------------------------------------------------------

function DCCalDateEvalSecOfYear1( elapse_sec, date, cal ) result(result)
  ! 年始めからの通秒を算出します. 
  ! 
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される日時が起点の日時として用いられます. 
  ! *date* が省略されない場合にはその変数に設定された日時が
  ! 起点の日時として用いられます. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate second of year. 
  ! 
  ! If an optional argument *date* is omitted, 
  ! information of date that is stored in the "dc_calendar" 
  ! is used as date of origin, 
  ! If *date* is not omitted, information of the variable is used as 
  ! date of origin. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed seconds *elapse_sec* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !
  use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize, dccaldate_ym2d
  use dc_calendar_generic, only: DCCalDateInquire, DCCalDateEvalDayOfYear
  use dc_types, only: DP
  implicit none
  real(DP), intent(in):: elapse_sec
                              ! *date* からの経過秒数. 
                              ! Elapsed seconds from *date*. 
  type(DC_CAL_DATE), intent(in), optional, target:: date
                              ! 起点となる日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date of origin. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
  real(DP):: result
                              ! 年始めからの通秒. 
                              ! Second of year. 

  ! 作業変数
  ! Work variables
  !
  real(DP):: day_of_year
  integer:: stat
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  integer:: year, month, day, hour, min
  real(DP):: sec
continue

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( date ) ) then
    datep => date
  else
    datep => default_date
  end if

  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
  result = 0.0
  if ( .not. datep % initialized ) return
  if ( .not. calp % initialized ) return

  ! 経過時間を与えた場合の日時を取得
  ! Inquire date and time when elapse time is given
  !
  call DCCalDateInquire( year, month, day, hour, min, sec, & ! (out)
      & elapse_sec = elapse_sec, date = date , cal = calp )  ! (in)

  ! 年初めからの通日を取得
  ! Day of year is inquire
  !
  stat = dccaldate_ym2d( year, month, day, calp, & ! (in)
    &                    day_of_year )             ! (out)

  ! 通秒へ変換
  ! Convert into sec of year
  !
  result = ( day_of_year - 1 ) * calp % hour_in_day &
      &                        * calp % min_in_hour &
      &                        * calp % sec_in_min  &
      &                 + hour * calp % min_in_hour &
      &                        * calp % sec_in_min  &
      &                 + min  * calp % sec_in_min  &
      &                 + sec

end function DCCalDateEvalSecOfYear1

!-----------------------------------------------------------

function DCCalDateEvalDayOfYear1( elapse_sec, date, cal ) result(result)
  ! 年始めからの通日を算出します. 
  ! 
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される日時が起点の日時として用いられます. 
  ! *date* が省略されない場合にはその変数に設定された日時が
  ! 起点の日時として用いられます. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate day of year. 
  ! 
  ! If an optional argument *date* is omitted, 
  ! information of date that is stored in the "dc_calendar" 
  ! is used as date of origin, 
  ! If *date* is not omitted, information of the variable is used as 
  ! date of origin. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed seconds *elapse_sec* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !
  use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize, dccaldate_ym2d
  use dc_calendar_generic, only: DCCalDateInquire
  use dc_types, only: DP
  implicit none
  real(DP), intent(in):: elapse_sec
                              ! *date* からの経過秒数. 
                              ! Elapsed seconds from *date*. 
  type(DC_CAL_DATE), intent(in), optional, target:: date
                              ! 起点となる日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date of origin. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
  real(DP):: result
                              ! 年始めからの通日. 
                              ! Day of year. 

  ! 作業変数
  ! Work variables
  !
  integer:: year, month, day, hour, min
  real(DP):: sec
  integer:: stat
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()

continue

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( date ) ) then
    datep => date
  else
    datep => default_date
  end if

  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
  result = 0.0
  if ( .not. datep % initialized ) return
  if ( .not. calp % initialized ) return

  ! 経過時間を与えた場合の日時を取得
  ! Inquire date and time when elapse time is given
  !
  call DCCalDateInquire( year, month, day, hour, min, sec, & ! (out)
      & elapse_sec = elapse_sec, date = date , cal = calp )  ! (in)

  ! 年初めからの通日を取得
  ! Day of year is inquire
  !
  stat = dccaldate_ym2d( year, month, day, calp, & ! (in)
    &                    result )                  ! (out)

end function DCCalDateEvalDayOfYear1

!-----------------------------------------------------------

function DCCalDateEvalSecOfDay1( elapse_sec, date, cal ) result(result)
  ! 日始めからの通秒を算出します. 
  ! 
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される日時が起点の日時として用いられます. 
  ! *date* が省略されない場合にはその変数に設定された日時が
  ! 起点の日時として用いられます. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate second of day. 
  ! 
  ! If an optional argument *date* is omitted, 
  ! information of date that is stored in the "dc_calendar" 
  ! is used as date of origin, 
  ! If *date* is not omitted, information of the variable is used as 
  ! date of origin. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed seconds *elapse_sec* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !
  use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize, dccaldate_ym2d
  use dc_calendar_generic, only: DCCalDateInquire
  use dc_types, only: DP
  implicit none
  real(DP), intent(in):: elapse_sec
                              ! *date* からの経過秒数. 
                              ! Elapsed seconds from *date*. 
  type(DC_CAL_DATE), intent(in), optional, target:: date
                              ! 起点となる日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date of origin. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 
  real(DP):: result
                              ! 日始めからの通秒. 
                              ! Second of day. 

  ! 作業変数
  ! Work variables
  !
  integer:: stat
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  integer:: year, month, day, hour, min
  real(DP):: sec
continue

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( date ) ) then
    datep => date
  else
    datep => default_date
  end if

  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
  result = 0.0
  if ( .not. datep % initialized ) return
  if ( .not. calp % initialized ) return

  ! 経過時間を与えた場合の日時を取得
  ! Inquire date and time when elapse time is given
  !
  call DCCalDateInquire( year, month, day, hour, min, sec, & ! (out)
      & elapse_sec = elapse_sec, date = date , cal = calp )  ! (in)

  ! 通秒へ変換
  ! Convert into sec of year
  !
  result = &
      &                   hour * calp % min_in_hour &
      &                        * calp % sec_in_min  &
      &                 + min  * calp % sec_in_min  &
      &                 + sec

end function DCCalDateEvalSecOfDay1
