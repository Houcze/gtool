!= 閏年かどうかの判定
!= Judge whether it is a leap year
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldatechkleapyear.f90,v 1.2 2010-10-06 01:48:49 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!
function DCCalDateChkLeapYear1( elapse_sec, date, cal ) result(result)
  ! 閏年かどうかの判定. 
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
  ! Judge whether it is a leap year. 
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
  use dc_calendar_types, only: DC_CAL_DATE, DC_CAL, &
    & CAL_USER_DEFINED, &
    & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN, CAL_360DAY
  use dc_calendar_internal, only: default_cal, default_cal_set, default_date, &
    & dccaldate_normalize, dccaldate_ym2d
  use dc_calendar_generic, only: DCCalDateInquire
  use dc_types, only: DP
  implicit none
  logical:: result
                              ! 閏年であれば .true., そうでなければ .false.
                              !
                              ! Leap year: .true., No leap year: .false.
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
  ! 作業変数
  ! Work variables
  !
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
  result = .false.
  if ( .not. datep % initialized ) return
  if ( .not. calp % initialized ) return

  ! 経過時間を与えた場合の日時を取得
  ! Inquire date and time when elapse time is given
  !
  call DCCalDateInquire( year, month, day, hour, min, sec, & ! (out)
    & elapse_sec = elapse_sec, date = datep , cal = calp )   ! (in)

  ! 閏年の判定
  ! Judge leap year
  !
  select case( calp % cal_type )
  case( CAL_JULIAN )
    if ( mod( year, 4 ) == 0 ) then
      result = .true.
    else
      result = .false.
    end if

  case( CAL_GREGORIAN )
    if ( mod( year, 400 ) == 0 ) then
      result = .true.
    elseif ( mod( year, 100 ) == 0 ) then
      result = .false.
    elseif ( mod( year, 4 ) == 0 ) then
      result = .true.
    else
      result = .false.
    end if

  case default
    result = .false.
  end select

end function DCCalDateChkLeapYear1
