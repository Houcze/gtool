!== 1 日の秒数の設定
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimesetsecofday.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

subroutine DCDateTimeSetSecOfDay(sec)
  !
  ! 1 日の秒数のデフォルトを設定します.
  !
  ! なお, この手続きを呼ばない場合, デフォルトの 1 日の秒数は
  ! dc_date_types#DAY_SECONDS_EARTH に設定されています.
  !
  use dc_date_types, only: day_seconds_default => day_seconds
  use dc_types, only: DP
  use dc_trace, only: BeginSub, EndSub
  implicit none
  real(DP), intent(in):: sec
  character(*), parameter :: subname = 'DCDateTimeSetSecOfDay'
continue
  call BeginSub(subname, 'sec=<%f>', d=(/sec/))
  day_seconds_default = sec
  call EndSub(subname, 'dc_date_types#day_seconds=<%f>', d=(/day_seconds_default/))
end subroutine DCDateTimeSetSecOfDay
