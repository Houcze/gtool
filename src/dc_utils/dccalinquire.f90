!= 暦情報の問い合わせ
!= Inquire information of calendar
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccalinquire.f90,v 1.3 2010-08-26 10:50:08 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!

subroutine DCCalInquire1( cal_type, &
  &                       month_in_year, day_in_month, day_in_month_ptr, &
  &                       hour_in_day, min_in_hour, sec_in_min, &
  &                       cal, err )
  !
  ! 暦情報の問い合わせを行います. 
  !
  ! *cal_type* には以下の文字列が返ります. 
  !
  ! gregorian         :: グレゴリオ暦
  ! julian            :: ユリウス暦
  ! noleap            :: 閏年無しの暦
  ! 360day            :: 1ヶ月が 30 日の暦
  ! cyclic            :: ある月の日数を
  !                      「30.6 × 月数 − 前月までの総日数」
  !                      の小数点以下切捨とする暦
  ! user_defined      :: ユーザ定義の暦
  !
  ! 省略可能引数 *cal* が省略された場合には, dc_calendar 内部で
  ! 保持される暦に関する情報が得られます. 
  ! *cal* が省略されない場合にはその変数に設定された暦の情報が得られます. 
  !
  ! Inquire information of calendar. 
  !
  ! Following strings are returned to *cal_type*. 
  !
  ! gregorian         :: Gregorian calendar. 
  ! julian            :: Julian calendar. 
  ! noleap            :: A calendar without leap year. 
  ! 360day            :: A calendar in which number of days of a month is 30. 
  ! cyclic            :: A calendar in which number of days of a year is
  !                      "30.6 x (number of months) - (total days until last month)"
  !                      (truncate fractional part). 
  ! user_defined      :: User defined calendar
  !
  ! If an optional argument *cal* is omitted, 
  ! information of a calendar that is stored in the "dc_calendar"
  ! is returned, 
  ! If *cal* is not omitted, information of the variable is returned. 
  !

  use dc_calendar_types, only: DC_CAL
  use dc_calendar_internal, only: default_cal, default_cal_set, dccaltype_str
  use dc_message, only: MessageNotify
  use dc_string, only: LChar
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT
  use dc_types, only: STRING, DP
  implicit none
  character(*), intent(out), optional:: cal_type
                              ! 暦の種類を示す文字列. 
                              ! 
                              ! Strings that specify a kind of calendar. 
  integer, intent(out), optional:: month_in_year
                              ! 1 年の月数. 
                              ! Months in a year. 
  integer, intent(out), optional:: day_in_month(:)
                              ! 1 ヶ月の日数. 
                              ! グレゴリオ暦の場合, 配列の 2 番目の要素 (2月) 
                              ! には必ず 28 が返ります. 
                              !
                              ! Days in months. 
                              ! In Gregorian calendar, 28 is returned to 
                              ! 2nd position of the array (February) 
                              ! at all times. 
                              !
  integer, pointer, optional:: day_in_month_ptr(:)
                              ! 1 ヶ月の日数 (ポインタ). 
                              ! グレゴリオ暦の場合, 配列の 2 番目の要素 (2月) 
                              ! には必ず 28 が返ります. 
                              !
                              ! Days in months (pointer). 
                              ! In Gregorian calendar, 28 is returned to 
                              ! 2nd position of the array (February) 
                              ! at all times. 
                              !
  integer, intent(out), optional:: hour_in_day
                              ! 1 日の時間数. 
                              ! Hours in a day. 
  integer, intent(out), optional:: min_in_hour
                              ! 1 時間の分数. 
                              ! Minutes in a hour. 
  real(DP), intent(out), optional:: sec_in_min
                              ! 1 分の秒数. 
                              ! Seconds in a minute. 
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
  integer:: siz_dm
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
  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
  if ( .not. calp % initialized ) then
    stat = DC_ENOTINIT
    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 各要素への値の参照
  ! Refer elements
  !
  if ( present( cal_type      ) ) then 
    cal_type = dccaltype_str( calp % cal_type )
  end if
  if ( present( month_in_year ) ) month_in_year = calp % month_in_year
  if ( present( hour_in_day   ) ) hour_in_day   = calp % hour_in_day
  if ( present( min_in_hour   ) ) min_in_hour   = calp % min_in_hour
  if ( present( sec_in_min    ) ) sec_in_min    = calp % sec_in_min

  if ( present( day_in_month     ) ) then
    if ( size( day_in_month ) > 0 ) then
      day_in_month = 0
      siz_dm = min( size( day_in_month ), size( calp % day_in_month ) )
      day_in_month(1:siz_dm) = calp % day_in_month(1:siz_dm)
    end if
  end if

  if ( present( day_in_month_ptr ) ) then
    siz_dm = size( calp % day_in_month )
    allocate( day_in_month_ptr(1:siz_dm) )
    day_in_month_ptr(1:siz_dm) = calp % day_in_month(1:siz_dm)
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp )
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalInquire1
