!= 日時の設定
!= Setting of date
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldatecreate.f90,v 1.3 2010-09-24 07:07:31 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!

subroutine DCCalDateCreate1( year, month, day, hour, min, sec, date, zone, err )
  !
  ! 日時の設定を行います. 
  ! 
  ! YYYY-MM-DDThh:mm:ss.sTZD のような文字列 
  ! (YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒,
  ! TZD はタイムゾーン) で指定する場合には
  ! 下記の同名のサブルーチンを使用して下さい. 
  !
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL_DATE" 型の変数に日時が
  ! 設定されます. その後の手続きで *date* を省略した場合には
  ! この日時が使用されます. 
  ! *date* が省略されない場合にはその変数に日時が設定されます. 
  ! その日時を使用する場合, 手続きにその "dc_calendar_types#DC_CAL_DATE" 型の変数
  ! を与えてください. 
  !
  ! Set date. 
  !
  ! If a string like as "YYYY-MM-DDThh:mm:ss.sTZD"
  ! (YYYY is year, MM is month, DD is day, hh is hour, mm is minute, 
  ! ss.s is second, TZD is time zone) is used, 
  ! use a following homonymous subroutine. 
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


  use dc_calendar_types, only: DC_CAL_DATE
  use dc_calendar_internal, only: default_date
  use dc_regex, only: match
  use dc_message, only: MessageNotify
  use dc_string, only: LChar
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADDATE
  use dc_types, only: STRING, DP
  implicit none
  integer, intent(in):: year  ! 年. Year.
  integer, intent(in):: month ! 月. Month.
  integer, intent(in):: day   ! 日. Day.
  integer, intent(in):: hour  ! 時. Hour.
  integer, intent(in):: min   ! 分. Minute.
  real(DP), intent(in):: sec  ! 秒. Second.
  type(DC_CAL_DATE), intent(out), optional, target:: date
                              ! 日時情報を収めたオブジェクト. 
                              ! 省略した場合には, デフォルトの日時として
                              ! 指定される. 
                              ! 
                              ! An object that stores information of 
                              ! date and time. 
                              ! If this is omitted, these information is 
                              ! set as default date and time. 
  character(*), intent(in), optional:: zone
                              ! UTC からの時差. Time-zone. 
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
  type(DC_CAL_DATE), pointer:: datep =>null()
  integer:: start, length
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: version = &
    & '$Name:  $' // &
    & '$Id: dccaldatecreate.f90,v 1.3 2010-09-24 07:07:31 morikawa Exp $'
  character(*), parameter:: subname = 'DCCalDateCreate1'
continue
  call BeginSub( subname, version )
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

!!$  ! 初期設定のチェック
!!$  ! Check initialization
!!$  !
!!$  if ( datep % initialized ) then
!!$    stat = DC_EALREADYINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  ! 日時の正当性のチェック
  ! Validate date and time
  !
!!$  if ( year < 1 ) then
!!$    stat = DC_EBADDATE
!!$    call MessageNotify('W', subname, 'year=<%d> must be natural number', &
!!$      & i = (/ year /) )
!!$    goto 999
!!$  end if

  if ( month < 1 ) then
    stat = DC_EBADDATE
    call MessageNotify('W', subname, 'month=<%d> must be natural number', &
      & i = (/ month /) )
    goto 999
  end if

  if ( day < 1 ) then
    stat = DC_EBADDATE
    call MessageNotify('W', subname, 'day=<%d> must be natural number', &
      & i = (/ day /) )
    goto 999
  end if

  if ( hour < 0 ) then
    stat = DC_EBADDATE
    call MessageNotify('W', subname, 'hour=<%d> must not be negative', &
      & i = (/ hour /) )
    goto 999
  end if

  if ( min < 0 ) then
    stat = DC_EBADDATE
    call MessageNotify('W', subname, 'min=<%d> must not be negative', &
      & i = (/ min /) )
    goto 999
  end if

  if ( sec < 0.0_DP ) then
    stat = DC_EBADDATE
    call MessageNotify('W', subname, 'sec=<%f> must not be negative', &
      & d = (/ sec /) )
    goto 999
  end if

  call match( '^[#+-]#d+:#d+$', zone, & ! (in)
    &         start, length )           ! (out)
  if ( length > 0 ) then
    datep % zone = zone
  else
    datep % zone = ''
  end if

  ! 各要素への値の設定
  ! Configure elements
  !
  datep % year  = year
  datep % month = month
  datep % day   = day
  datep % hour  = hour
  datep % min   = min
  datep % sec   = sec

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
  datep % initialized = .true.
999 continue
  nullify( datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateCreate1

subroutine DCCalDateCreate2( date_str, date, err )
  !
  ! 日時の設定を行います. 
  ! 
  ! *date_str* に YYYY-MM-DDThh:mm:ss.sTZD の形式の文字列 
  ! (YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒,
  ! TZD はタイムゾーン) を指定してください. 
  ! 年月日時分秒を各個変数で指定する場合には
  ! 上記の同名のサブルーチンを使用して下さい. 
  ! 日時の文字列形式は 
  ! gtool4 netCDF 規約「5.5 日時形式」に準拠しています. 
  ! 
  ! "s since 2009-06-17T11:23:45+09:00" のような文字列を与えた場合には, 
  ! "s since " の部分をサブルーチン内で自動的に切り取って, 
  ! "2009-06-17T11:23:45+09:00" の部分を設定します. 
  !
  ! 省略可能引数 *date* が省略された場合には, dc_calendar 内部で
  ! 保持される "dc_calendar_types#DC_CAL_DATE" 型の変数に日時が
  ! 設定されます. その後の手続きで *date* を省略した場合には
  ! この日時が使用されます. 
  ! *date* が省略されない場合にはその変数に日時が設定されます. 
  ! その日時を使用する場合, 手続きにその "dc_calendar_types#DC_CAL_DATE" 型の変数
  ! を与えてください. 
  !
  ! Set date. 
  !
  ! Specify a string like as "YYYY-MM-DDThh:mm:ss.sTZD"
  ! (YYYY is year, MM is month, DD is day, hh is hour, mm is minute, 
  ! ss.s is second, TZD is time zone). 
  ! If individual variables (year, month, day, hour, minute, second, zone) 
  ! are used, use a foregoing homonymous subroutine. 
  ! Format of date is conformed to gtool4 netCDF Convention "5.5 Expression of date and time" 
  !
  ! If a string like as "s since 2009-06-17T11:23:45+09:00" is specified, 
  ! A part "s since " is truncated automatically, and 
  ! a part "2009-06-17T11:23:45+09:00" is set. 
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
  use dc_calendar_generic, only: DCCalDateParseStr, DCCalDateCreate
  use dc_calendar_types, only: DC_CAL_DATE
  use dc_calendar_internal, only: default_date
  use dc_message, only: MessageNotify
  use dc_types, only: DP, TOKEN
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADDATE
  use dc_types, only: STRING
  implicit none
  character(*), intent(in):: date_str
                              ! 日時情報を表す文字列. 
                              ! 表示形式については gtool4 netCDF 規約
                              ! 5.5 日時形式を参照のこと. 
                              ! 
                              ! Strings that express date and time. 
                              ! See gtool4 netCDF Convention 
                              ! 5.5 Expression of date and time for details. 
  type(DC_CAL_DATE), intent(out), optional, target:: date
                              ! 日時情報を収めたオブジェクト. 
                              ! 省略した場合には, デフォルトの日時として
                              ! 指定される. 
                              ! 
                              ! An object that stores information of 
                              ! date and time. 
                              ! If this is omitted, these information is 
                              ! set as default date and time. 
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
  type(DC_CAL_DATE), pointer:: datep =>null()
  integer:: year  ! 年. Year.
  integer:: month ! 月. Month.
  integer:: day   ! 日. Day.
  integer:: hour  ! 時. Hour.
  integer:: min   ! 分. Minute.
  real(DP):: sec  ! 秒. Second.
  character(TOKEN):: zone
                  ! UTC からの時差. Time-zone.
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: version = &
    & '$Name:  $' // &
    & '$Id: dccaldatecreate.f90,v 1.3 2010-09-24 07:07:31 morikawa Exp $'
  character(*), parameter:: subname = 'DCCalDateCreate2'
continue
  call BeginSub( subname, version )
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

!!$  ! 初期設定のチェック
!!$  ! Check initialization
!!$  !
!!$  if ( datep % initialized ) then
!!$    stat = DC_EALREADYINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if

  ! 日時を表現した文字列の解釈
  ! Parse strings that express date and time
  !
  call DCCalDateParseStr( date_str, &           ! (in)
    & year, month, day, hour, min, sec, zone, & ! (out)
    & err = err )                               ! (out) optional
  if ( present(err) ) then
    if ( err ) then
      stat = DC_EBADDATE
      goto 999
    end if
  end if

  ! オブジェクトの作成
  ! Create an object
  !
  call DCCalDateCreate( &
    & year, month, day, hour, min, sec, & ! (in)
    & datep, zone, err = err )            ! (out) optional
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
  nullify( datep )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateCreate2
