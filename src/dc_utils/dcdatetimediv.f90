!= 利用者定義演算子 (/) のための関数
!= Functions for user defined operation (/)
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcdatetimediv.f90,v 1.2 2009-05-31 11:46:03 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  function dcdatetime_div_fi(diff, denominator) result(result)
    !
    ! 日時差 *diff* を *denominator* で除算した結果を返します.
    !
    ! ※ 注意 : 月差を除算すると近似的結果になるおそれがあります
    use dc_date_types, only: CYCLIC_MDAYS
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modscl => mod, modulo, int, abs, sign
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    integer, intent(in):: denominator
  continue
    result % mon = int( diff % mon / denominator )
    ! 月からの近似的繰り下がりは日単位でしか行わない
    result % day = &
      &   int( diff % day / denominator ) &
      & + int( (CYCLIC_MDAYS * modscl(diff % mon, denominator)) / denominator )
    result % sec = diff % sec / denominator + &
      & (diff % day_seconds * modscl(diff % day, denominator)) / &
      & denominator
    result % nondim_flag = diff % nondim_flag
  end function dcdatetime_div_fi

  function dcdatetime_div_fr(diff, denominator) result(result)
    !
    ! ※ 注意 : 月差を除算すると近似的結果になるおそれがあります
    use dc_date_generic, only: operator(/)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    real, intent(in):: denominator
  continue
    result = diff / real(denominator, DP)
  end function dcdatetime_div_fr

  function dcdatetime_div_fd(diff, denominator) result(result)
    !
    ! ※ 注意 : 月差を除算すると近似的結果になるおそれがあります
    use dc_date_internal, only: dcdate_normalize
    use dc_date_types, only: CYCLIC_MDAYS, DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modulo, int, abs, sign
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff
    real(DP), intent(in):: denominator
    type(DC_SCALED_SEC):: month, day
  continue
    month = int( diff % mon / denominator )
    result % mon = int(month)
    day =   int( diff % day / denominator ) &
      &   + int(CYCLIC_MDAYS * (month - result % mon))
    result % day = int(day)
    result % sec = &
      & diff % sec / denominator + (day - result % day) * diff % day_seconds
    result % day_seconds = diff % day_seconds
    result % nondim_flag = diff % nondim_flag
    call dcdate_normalize(result % day, result % sec, result % day_seconds, result % nondim_flag)
  end function dcdatetime_div_fd

  function dcdatetime_div_ff(diff1, diff2) result(result)
    !
    ! ※ 注意 : 月差と日時の混在する除算は近似的結果になるおそれがあります
    use dc_date_types, only: CYCLIC_MDAYS
    use dc_scaledsec, only: DC_SCALED_SEC, assignment(=), &
      & operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modulo, int, abs, sign
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_types, only: DP
    implicit none
    real(DP):: result
    type(DC_DIFFTIME), intent(in):: diff1, diff2
  continue
    ! ゼロ割対応コードが必要か?
    result = &
      & (diff1 % day_seconds * (CYCLIC_MDAYS * diff1 % mon + diff1 % day) &
      & + diff1 % sec) / &
      & (diff2 % day_seconds * (CYCLIC_MDAYS * diff2 % mon + diff2 % day) &
      & + diff2 % sec)
  end function dcdatetime_div_ff
