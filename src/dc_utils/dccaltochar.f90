!= 暦情報の文字列変換
!= Convert calendar information into strings
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaltochar.f90,v 1.2 2009-10-17 14:08:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

function DCCalToChar1( cal ) result(result)
  !
  ! dc_calendar_types#DC_CAL 型変数 *cal* を文字型変数 *result* 
  ! へ変換して返します.
  ! 書式は下記のようになります. 
  !
  !   CCCC:month_in_year=MM,day_in_month=(/DD1,.../),hour_in_day=hh,min_in_hour=mm,sec_in_min=ss
  !
  ! CCCC が暦のタイプ (文字数は任意), 以降は一年の月数等の情報となります. 
  !
  ! Convert a "dc_calendar_types#DC_CAL" variable *cal* into
  ! a character variable *result* 
  ! The format is as follows. 
  ! 
  !   CCCC:month_in_year=MM,day_in_month=(/DD1,.../),hour_in_day=hh,min_in_hour=mm,sec_in_min=ss
  !
  ! CCCC is a type of calencar (number of strings is arbitrary). 
  ! The rest is information about number of months of a year, etc. 
  !
  use dc_calendar_types, only: DC_CAL
  use dc_calendar_internal,only: dccaltype_str
  use dc_types, only: STRING, TOKEN, DP
  use dc_string, only: toChar, CPrintf, StoA, toChar
  use dc_message, only: MessageNotify
  implicit none
  character(STRING):: result
  type(DC_CAL), intent(in):: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 

  character(TOKEN):: cal_str
continue

  cal_str = dccaltype_str( cal % cal_type )

  result = CPrintf('%c:month_in_year=%d,day_in_month=(/%c/),hour_in_day=%d,min_in_hour=%d,sec_in_min=%f', &
    & i = (/ cal % month_in_year, cal % hour_in_day, cal % min_in_hour /), &
    & d = (/ cal % sec_in_min /), &
    & c1=trim(cal_str), c2=trim(toChar(cal % day_in_month)))

end function DCCalToChar1
