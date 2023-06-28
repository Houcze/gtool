!= 利用者定義関数 mod の実体
!= User defined function "mod"
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcdatetimemod.f90,v 1.2 2009-05-31 11:46:03 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  function dcdatetime_mod_ff(diff1, diff2) result(result)
    !
    ! 引数 <b>diff1</b> を <b>diff2</b> で除算した際の余りを返します.
    !
    ! ※ 注意: 月差と日時の混在する除算は近似的結果になるおそれがあります
    !
    use dc_date_internal, only: dcdate_normalize, dcdate_nondimcheck
    use dc_date_types, only: CYCLIC_MDAYS, DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(==), operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modscl => mod, modulo, int, abs, sign
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff1, diff2
    type(DC_SCALED_SEC):: sec1, sec2
    type(DC_SCALED_SEC):: zero_sec
  continue
    result % day_seconds = diff1 % day_seconds
    if (diff1 % day == zero_sec .and. diff2 % day == zero_sec .and. &
      & diff1 % sec == zero_sec .and. diff2 % sec == zero_sec) then
      result % mon = modscl(diff1 % mon, diff2 % mon)
      result % day = zero_sec
      result % sec = zero_sec
    else if (diff1 % sec == zero_sec .and. diff2 % sec == zero_sec) then
      result % mon = zero_sec
      result % day = modscl((CYCLIC_MDAYS * diff1 % mon + diff1 % day), &
        & (CYCLIC_MDAYS * diff2 % mon + diff2 % day))
      result % sec = zero_sec
    else
      sec1 = diff1 % day_seconds * (CYCLIC_MDAYS * diff1 % mon + diff1 % day) &
        & + diff1 % sec
      sec2 = diff2 % day_seconds * (CYCLIC_MDAYS * diff2 % mon + diff2 % day) &
        & + diff2 % sec
      result % sec = modscl(sec1, sec2)
      result % day = zero_sec
      result % mon = zero_sec
      call dcdate_normalize(result % day, result % sec, result % day_seconds, result % nondim_flag)
    endif
    call dcdate_nondimcheck('dc_date#mod', diff1, diff2, result)
  end function dcdatetime_mod_ff
