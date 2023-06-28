!= 日時情報の問い合わせ
!= Inquire information of date
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldateinquire.f90,v 1.3 2010-09-24 00:28:18 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!

subroutine DCCalDateInquire1( year, month, day, hour, min, sec, zone, &
  & elapse_sec, date, cal, err )
  !
  ! 日時情報の問い合わせを行います. 
  !
  ! 問い合わせの結果を
  ! YYYY-MM-DDThh:mm:ss.sTZD のような文字列 
  ! (YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒,
  ! TZD はタイムゾーン) で受け取りたい場合には, 
  ! 下記の同名のサブルーチンを使用して下さい. 
  ! 
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される日時に関する情報が得られます. 
  ! *date* が省略されない場合にはその変数に設定された日時の情報が得られます. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Inquire information of date. 
  !
  ! If a string like as "YYYY-MM-DDThh:mm:ss.sTZD"
  ! (YYYY is year, MM is month, DD is day, hh is hour, mm is minute, 
  ! ss.s is second, TZD is time zone) is needed, 
  ! use a following homonymous subroutine. 
  ! 
  ! If an optional argument *date* is omitted, 
  ! information of date that is stored in the "dc_calendar" 
  ! is returned, 
  ! If *date* is not omitted, information of the variable is returned. 
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
  use dc_calendar_generic, only: DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_string, only: LChar
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  implicit none
  integer, intent(out), optional:: year           ! 年. Year.  
  integer, intent(out), optional:: month          ! 月. Month. 
  integer, intent(out), optional:: day            ! 日. Day. 
  integer, intent(out), optional:: hour           ! 時. Hour. 
  integer, intent(out), optional:: min            ! 分. Minute. 
  real(DP), intent(out), optional:: sec           ! 秒. Sec. 
  character(*), intent(out), optional:: zone      ! UTC からの時差. Time-zone. 
  real(DP), intent(in), optional:: elapse_sec
                              ! *date* からの経過秒数. 
                              ! Elapsed seconds from *date*. 
  type(DC_CAL_DATE), intent(in), optional, target:: date
                              ! 日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date and time. 
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
  integer:: wyear, wmonth, wday, whour, wmin
  real(DP):: wsec
  character(TOKEN):: wzone
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  character(STRING):: e_date_str, e_cal_str
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalInquire1'
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
  if ( .not. datep % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL_DATE'
    goto 999
  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  wyear  = datep % year
  wmonth = datep % month
  wday   = datep % day
  whour  = datep % hour
  wmin   = datep % min
  wsec   = datep % sec
  wzone  = datep % zone

  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
  if ( present( elapse_sec ) ) then
!!$    if ( elapse_sec < 0.0_DP ) then
!!$      stat = DC_ENEGATIVE
!!$      cause_c = 'elapse_sec'
!!$      goto 999
!!$    end if

    wsec = wsec + elapse_sec
  end if

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( wyear, wmonth, wday, whour, wmin, wsec, & ! (inout)
    &                         calp )                                    ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( wyear, wmonth, wday, whour, wmin, wsec, wzone )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! 引数への代入
  ! Substitute arguments
  !
  if ( present(year ) ) year  = wyear
  if ( present(month) ) month = wmonth
  if ( present(day  ) ) day   = wday
  if ( present(hour ) ) hour  = whour
  if ( present(min  ) ) min   = wmin
  if ( present(sec  ) ) sec   = wsec
  if ( present(zone ) ) zone  = wzone

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp, datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateInquire1

subroutine DCCalDateInquire2( date_str, elapse_sec, date, cal, err )
  !
  ! 日時情報の問い合わせを行います. 
  ! 問い合わせ結果は YYYY-MM-DDThh:mm:ss.sTZD のような文字列 
  ! (YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒,
  ! TZD はタイムゾーン) で返ります. 
  ! 日時の文字列形式は 
  ! gtool4 netCDF 規約「5.5 日時形式」に準拠しています. 
  !
  ! 問い合わせの結果を年月日時分秒で各個変数で受け取りたい場合は
  ! 上記の同名のサブルーチンを使用して下さい. 
  !
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される日時に関する情報が得られます. 
  ! *date* が省略されない場合にはその変数に設定された日時の情報が得られます. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 経過秒数 *elapse_sec* 
  ! の年月日時分への変換に dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Inquire information of date. 
  ! A result is returned as a string like as 
  ! YYYY-MM-DDThh:mm:ss.sTZD 
  ! (YYYY is year, MM is month, DD is day, hh is hour, mm is minute, 
  ! ss.s is second, TZD is time zone). 
  ! Format of date is conformed to gtool4 netCDF Convention "5.5 Expression of date and time" 
  !
  ! If individual variables (year, month, day, hour, minute, second, zone) 
  ! are needed, use a foregoing homonymous subroutine. 
  !
  ! If an optional argument *date* is omitted, 
  ! information of date that is stored in the "dc_calendar" 
  ! is returned, 
  ! If *date* is not omitted, information of the variable is returned. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for conversion of elapsed seconds *elapse_sec* into 
  ! year-month-day etc.
  ! If *cal* is not omitted, information of the variable is used. 
  !
  use dc_calendar_generic, only: DCCalDateToChar
  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize
  use dc_calendar_generic, only: DCCalDateToChar, DCCalToChar
  use dc_message, only: MessageNotify
  use dc_string, only: LChar
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
    & DC_EINCONSISTCALDATE, DC_ENEGATIVE
  use dc_types, only: STRING, DP, TOKEN
  implicit none
  character(*), intent(out):: date_str
                              ! 日時情報を表す文字列. 
                              ! 表示形式については gtool4 netCDF 規約
                              ! 5.5 日時形式を参照のこと. 
                              ! 
                              ! Strings that express date and time. 
                              ! See gtool4 netCDF Convention 
                              ! 5.5 Expression of date and time for details. 
  real(DP), intent(in), optional:: elapse_sec
                              ! *date* からの経過秒数. 
                              ! Elapsed seconds from *date*. 
  type(DC_CAL_DATE), intent(in), optional, target:: date
                              ! 日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date and time. 
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
  integer:: year, month, day, hour, min
  real(DP):: sec
  character(TOKEN):: zone
  type(DC_CAL_DATE), pointer:: datep =>null()
  type(DC_CAL), pointer:: calp =>null()
  character(STRING):: e_date_str, e_cal_str
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalInquire2'
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
  if ( .not. datep % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL_DATE'
    goto 999
  end if

  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素の取得
  ! Get elements
  !
  year  = datep % year
  month = datep % month
  day   = datep % day
  hour  = datep % hour
  min   = datep % min
  sec   = datep % sec
  zone  = datep % zone


  ! 経過時間(秒)の追加
  ! Add elapsed time (seconds)
  !
  if ( present( elapse_sec ) ) then
!!$    if ( elapse_sec < 0.0_DP ) then
!!$      stat = DC_ENEGATIVE
!!$      cause_c = 'elapse_sec'
!!$      goto 999
!!$    end if

    sec = sec + elapse_sec
  end if

  ! 日時の正規化
  ! Normalize date and time 
  !
  stat = dccaldate_normalize( year, month, day, hour, min, sec, & ! (inout)
    &                         calp )                              ! (in)
  if ( stat == DC_EINCONSISTCALDATE ) then
    e_cal_str = DCCalToChar( calp )
    e_date_str = DCCalDateToChar( year, month, day, hour, min, sec, zone )
    call MessageNotify('W', subname, 'cal=<%c> and date=<%c> are inconsistency', &
      & c1 = trim(e_cal_str), c2 = trim(e_date_str) )
    goto 999
  end if

  ! 日時表記（gtool4 netCDF 規約 5.5 日時形式）への変換
  ! Convert expression of date (gtool4 netCDF Convention 5.5 Expression of date and time)
  !
  date_str = DCCalDateToChar( year, month, day, hour, min, sec, zone )

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp, datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateInquire2
