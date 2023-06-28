!= 利用者定義演算子 (*) のための関数
!= Functions for user defined operation (*)
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcdatetimemul.f90,v 1.2 2009-05-31 11:46:03 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  function dcdatetime_mul_if(factor, diff) result(result)
    !
    ! 日時差 *diff* と *facter* とを乗算した結果を返します.
    !
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_date_internal, only: dcdate_normalize
    use dc_scaledsec, only: operator(*)
    implicit none
    type(DC_DIFFTIME):: result
    integer, intent(in):: factor
    type(DC_DIFFTIME), intent(in):: diff
  continue
    result % mon = factor * diff % mon
    result % day = factor * diff % day
    result % sec = factor * diff % sec
    result % day_seconds = diff % day_seconds
    result % nondim_flag = diff % nondim_flag
    call dcdate_normalize(result % day, result % sec, result % day_seconds, result % nondim_flag)
  end function dcdatetime_mul_if

  function dcdatetime_mul_fi(diff, factor) result(result)
    use dc_date_generic, only: operator(*)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    integer, intent(in):: factor
  continue
    result = factor * diff
  end function dcdatetime_mul_fi

  function dcdatetime_mul_rf(factor, diff) result(result)
    !
    ! ※ 注意 : 月差を非整数倍すると近似的結果になるおそれがあります
    use dc_date_generic, only: operator(*)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    real, intent(in):: factor
    type(DC_DIFFTIME), intent(in):: diff
  continue
    result = real(factor, DP) * diff
  end function dcdatetime_mul_rf

  function dcdatetime_mul_fr(diff, factor) result(result)
    !
    ! ※ 注意 : 月差を非整数倍すると近似的結果になるおそれがあります
    use dc_date_generic, only: operator(*)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    real, intent(in):: factor
  continue
    result = factor * diff
  end function dcdatetime_mul_fr

  function dcdatetime_mul_df(factor, diff) result(result)
    !
    ! ※ 注意 : 月差を非整数倍すると近似的結果になるおそれがあります
    use dc_date_internal, only: dcdate_normalize
    use dc_date_types, only: CYCLIC_MDAYS, DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modulo, int, abs, sign
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    real(DP), intent(in):: factor
    type(DC_DIFFTIME), intent(in):: diff
    type(DC_SCALED_SEC):: month, day
  continue
    month = factor * diff % mon
    result % mon = int(month)
    day = factor * diff % day + int(CYCLIC_MDAYS * (month - result % mon))
    result % day = int(day)
    result % sec = &
      & factor * diff % sec + (day - result % day) * diff % day_seconds
    result % day_seconds = diff % day_seconds
    result % nondim_flag = diff % nondim_flag
    call dcdate_normalize(result % day, result % sec, result % day_seconds, result % nondim_flag)
  end function dcdatetime_mul_df

  function dcdatetime_mul_fd(diff, factor) result(result)
    !
    ! ※ 注意 : 月差を非整数倍すると近似的結果になるおそれがあります
    use dc_date_generic, only: operator(*)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    real(DP), intent(in):: factor
  continue
    result = factor * diff
  end function dcdatetime_mul_fd
