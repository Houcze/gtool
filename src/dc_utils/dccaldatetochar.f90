!= 日時の文字列への変換
!= Convert date into a string
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldatetochar.f90,v 1.5 2009-10-18 02:34:48 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!

function DCCalDateToChar1( year, month, day, hour, min, sec, zone ) &
  & result(result)
  !
  ! 年月日時分秒を文字型変数 (gtool4 netCDF 規約「5.5 日時形式」に準拠) 
  ! へ変換して返します.
  !
  ! Convert year, month, day, hour, minute, second into a string
  ! (conformed to gtool4 netCDF Convention
  ! "5.5 Expression of date and time"). 
  !
  use dc_types, only: STRING, TOKEN, DP
  use dc_string, only: toChar, CPrintf, StoA, RoundNum
  use dc_message, only: MessageNotify
  implicit none
  character(STRING):: result
                              ! 日時情報を表す文字列. 
                              ! 表示形式については gtool4 netCDF 規約
                              ! 5.5 日時形式を参照のこと. 
                              ! 
                              ! Strings that express date and time. 
                              ! See gtool4 netCDF Convention 
                              ! 5.5 Expression of date and time for details. 
  integer, intent(in):: year                     ! 年. Year.  
  integer, intent(in):: month                    ! 月. Month. 
  integer, intent(in):: day                      ! 日. Day. 
  integer, intent(in):: hour                     ! 時. Hour. 
  integer, intent(in):: min                      ! 分. Minute. 
  real(DP), intent(in):: sec                     ! 秒. Sec. 
  character(*), intent(in), optional:: zone      ! UTC からの時差. Time-zone. 
  integer:: csec_len
  character(TOKEN):: csec, zonew
continue

  if ( present(zone) ) then
    zonew = zone
  else
    zonew = ''
  end if

  csec = toChar(sec)
  csec = RoundNum( csec )
  if ( trim(csec) == '-0.' ) csec = '0.'
  do while ( index('123456789.', csec(len_trim(csec):len_trim(csec)) ) == 0 )
    if ( len_trim(csec) < 2 ) exit
    csec = csec(1:len_trim(csec)-1)
  end do
  if (int(sec) > -1 .and. int(sec) < 10)  csec = '0' // csec
  csec_len = len(trim(adjustl(csec)))
  if (csec(csec_len:csec_len) == '.') csec = csec(1:csec_len-1)

  result = CPrintf('%04d-%02d-%02dT%02d:%02d:%c%c', &
    & i=(/year, month, day, hour, min/), &
    & c1=trim(csec), c2=trim(zonew))

end function DCCalDateToChar1
