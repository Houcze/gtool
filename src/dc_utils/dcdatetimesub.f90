!= 利用者定義演算子 (-) のための関数
!= Functions for user defined operation (-)
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcdatetimesub.f90,v 1.2 2009-05-31 11:46:03 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  function dcdatetime_sub_tf(time, diff) result(result)
    !
    ! 2 つの日時 (DC_DATETIME 型) もしくは
    ! 日時差 (DC_DIFFTIME 型)の減算を行います.
    !
    use dc_date_generic, only: Eval, DCDateTimeCreate
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modulo, int, abs, sign
    implicit none
    type(DC_DATETIME):: result
    type(DC_DATETIME), intent(in):: time
    type(DC_DIFFTIME), intent(in):: diff
    type(DC_SCALED_SEC):: time_year, time_mon, time_day, time_sec
    integer:: time_caltype
    character(6):: time_zone
  continue
    call Eval(time, &
      & sclyear = time_year, sclmon = time_mon, sclday = time_day, &
      & sclsec = time_sec, caltype = time_caltype, zone = time_zone )
    call DCDateTimeCreate(result, &
      & sclyear = time_year, &
      & sclmon = time_mon - diff % mon, &
      & sclday = time_day - diff % day, &
      & sclsec = time_sec - diff % sec, &
      & caltype = time_caltype, zone = time_zone)
  end function dcdatetime_sub_tf

  function dcdatetime_sub_tt(time1, time2) result(result)
    use dc_date_generic, only: EvalSclSec, ZoneToDiff, operator(-)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_date_internal, only: dcdate_normalize
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modulo, int, abs, sign
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DATETIME), intent(in):: time1, time2
  continue
    result % day = time1 % day - time2 % day
    result % sec = time1 % sec - time2 % sec &
      & + EvalSclSec(ZoneToDiff(time1 % zone) - ZoneToDiff(time2 % zone))
    result % day_seconds = time1 % day_seconds
    call dcdate_normalize(result % day, result % sec, result % day_seconds, result % nondim_flag)
  end function dcdatetime_sub_tt

  function dcdatetime_sub_ff(diff1, diff2) result(result)
    use dc_date_internal, only: dcdate_normalize, dcdate_nondimcheck
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: operator(-)
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff1, diff2
  continue
    result % mon = diff1 % mon - diff2 % mon
    result % day = diff1 % day - diff2 % day
    result % sec = diff1 % sec - diff2 % sec
    result % day_seconds = diff1 % day_seconds
    call dcdate_nondimcheck('dc_date#operator(-)', diff1, diff2, result)
    call dcdate_normalize(result % day, result % sec, result % day_seconds, result % nondim_flag)
  end function dcdatetime_sub_ff

  function dcdatetime_sub_fd(diff, sec) result(result)
    use dc_date_internal, only: dcdate_normalize
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: operator(-)
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    real(DP), intent(in):: sec
  continue
    result % mon = diff % mon
    result % day = diff % day
    result % sec = diff % sec - sec
    result % day_seconds = diff % day_seconds
    call dcdate_normalize(result % day, result % sec, result % day_seconds, result % nondim_flag)
  end function dcdatetime_sub_fd

  function dcdatetime_sub_fr(diff, sec) result(result)
    use dc_date_generic, only: operator(-)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: operator(-)
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    real, intent(in):: sec
  continue
    result = diff - real( sec, DP )
  end function dcdatetime_sub_fr

  function dcdatetime_sub_fi(diff, sec) result(result)
    use dc_date_generic, only: operator(-)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: operator(-)
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    integer, intent(in):: sec
  continue
    result = diff - real( sec, DP )
  end function dcdatetime_sub_fi
