!= 暦と日時モジュール
!= Calendar and date module
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_calendar.f90,v 1.4 2009-10-19 11:56:10 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_calendar
  != 暦と日時モジュール
  != Calendar and date module
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! 暦と時刻を扱うための手続と構造体を提供します. 
  !
  ! Procedures and derived types for handling of calendar and date 
  ! are provided. 
  !
  !== Tutorial
  !
  ! * Gtool5 オフィシャルチュートリアル: 
  !   * {暦および日時の操作 (基本編)}[link:../tutorial/dc_calendar1.htm]
  !   * {暦および日時の操作 (上級編)}[link:../tutorial/dc_calendar2.htm]
  !
  ! * Gtool5 Official Tutorial: 
  !   * {Calendar and Date Management (Basic)}[link:../tutorial/dc_calendar1.htm.en]
  !   * {Calendar and Date Management (Advanced)}[link:../tutorial/dc_calendar2.htm.en]
  !
  !== Procedures List
  !
  ! dc_calendar_generic を参照ください. 
  !
  ! See "dc_calendar_generic".
  ! 
  !== Derived types
  !
  ! dc_calendar_types を参照ください. 
  !
  ! See "dc_calendar_types".
  !

  use dc_calendar_generic
  use dc_calendar_types
  implicit none
end module dc_calendar
