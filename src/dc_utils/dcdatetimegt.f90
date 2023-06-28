!= 利用者定義演算子 (>) のための関数
!= Functions for user defined operation (>)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimegt.f90,v 1.1 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  logical function dcdatetime_gt_tt(time1, time2) result(result)
    !
    ! 2 つの引数の日時を比較します.
    ! 1 つ目の引数に格納される日時が 2 つ目の引数に格納される日時
    ! よりも進んでいる場合, .true. が返ります.
    !
    use dc_date_generic, only: Eval, EvalSclSec, ZoneToDiff
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(==), operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modulo, int, abs, sign
    implicit none
    type(DC_DATETIME), intent(in):: time1, time2
    type(DC_SCALED_SEC):: year1, year2, time1_sec, time2_sec
  continue
    call Eval(time1, sclyear=year1)
    call Eval(time2, sclyear=year2)
    if (year1 > year2) then
      result = .true.
    elseif (year1 < year2) then
      result = .false.
    else
      time1_sec = EvalSclSec(time1) + EvalSclSec(ZoneToDiff(time1 % zone))
      time2_sec = EvalSclSec(time2) + EvalSclSec(ZoneToDiff(time2 % zone))
      if (time1_sec > time2_sec) then
        result = .true.
      else
        result = .false.
      end if
    end if
  end function dcdatetime_gt_tt

  logical function dcdatetime_gt_ff(diff1, diff2) result(result)
    !
    ! 2 つの引数の日時差を比較します.
    ! 1 つ目の引数に格納される日時差が 2 つ目の引数に格納される日時差
    ! よりも大きい場合, .true. が返ります.
    !
    use dc_date_generic, only: EvalSec
    use dc_date_types, only: CYCLIC_MDAYS
    use dc_scaledsec, only: &
      & operator(<), operator(>), operator(<=), operator(>=), operator(==)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff1, diff2
  continue
    if ( diff1 % day_seconds == diff2 % day_seconds ) then

      if ( diff1 % mon > diff2 % mon ) then
        result = .true.  ; return
      elseif ( diff1 % mon < diff2 % mon ) then
        result = .false. ; return
      end if
      if ( diff1 % day > diff2 % day ) then
        result = .true.  ; return
      elseif ( diff1 % day < diff2 % day ) then
        result = .false. ; return
      end if
      if ( diff1 % sec > diff2 % sec ) then
        result = .true.  ; return
      elseif ( diff1 % sec < diff2 % sec ) then
        result = .false. ; return
      end if
      result = .false.
    else

      if (EvalSec(diff1) > EvalSec(diff2)) then
        result = .true.
      else
        result = .false.
      end if
    end if
  end function dcdatetime_gt_ff

  logical function dcdatetime_gt_fi(diff, factor) result(result)
    !
    ! 2 つの引数の日時差を比較します.
    ! 1 つ目の引数に格納される日時差が 2 つ目の引数に格納される日時差
    ! よりも大きい場合, .true. が返ります.
    !
    use dc_date_generic, only: EvalSec
    use dc_date_types, only: CYCLIC_MDAYS, DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    integer, intent(in):: factor
  continue
    result = EvalSec(diff) > factor
  end function dcdatetime_gt_fi

  logical function dcdatetime_gt_if(factor, diff) result(result)
    !
    ! 2 つの引数の日時差を比較します.
    ! 1 つ目の引数に格納される日時差が 2 つ目の引数に格納される日時差
    ! よりも大きい場合, .true. が返ります.
    !
    use dc_date_generic, only: EvalSec
    use dc_date_types, only: CYCLIC_MDAYS, DC_DATETIME, DC_DIFFTIME
    implicit none
    integer, intent(in):: factor
    type(DC_DIFFTIME), intent(in):: diff
  continue
    result = factor > EvalSec(diff)
  end function dcdatetime_gt_if
