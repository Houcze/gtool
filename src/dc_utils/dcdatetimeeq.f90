!= 利用者定義演算子 (==) のための関数
!= Functions for user defined operation (==)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimeeq.f90,v 1.1 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  logical function dcdatetime_eq_tt(time1, time2) result(result)
    !
    ! 2 つの引数の日時を比較します.
    ! 1 つ目の引数に格納される日時が 2 つ目の引数に格納される日時
    ! と同じ場合, .true. が返ります.
    !
    use dc_date_generic, only: Eval, EvalSclSec, ZoneToDiff
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(==), operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), assignment(=)
    implicit none
    type(DC_DATETIME), intent(in):: time1, time2
    type(DC_SCALED_SEC):: year1, year2, time1_sec, time2_sec
  continue
    call Eval(time1, sclyear=year1)
    call Eval(time2, sclyear=year2)
    time1_sec = EvalSclSec(time1) + EvalSclSec(ZoneToDiff(time1 % zone))
    time2_sec = EvalSclSec(time2) + EvalSclSec(ZoneToDiff(time2 % zone))
    if (year1 == year2 .and. time1_sec == time2_sec) then
      result = .true.
    else
      result = .false.
    end if
  end function dcdatetime_eq_tt


  logical function dcdatetime_eq_ff(diff1, diff2) result(result)
    !
    ! 2 つの引数の日時差を比較します.
    ! 1 つ目の引数に格納される日時差が 2 つ目の引数に格納される日時差
    ! と同じ場合, .true. が返ります.
    !
    use dc_scaledsec, only: operator(==)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff1, diff2
  continue
    if (       diff1 % mon == diff2 % mon &
      &  .and. diff1 % day == diff2 % day &
      &  .and. diff1 % sec == diff2 % sec   ) then
      result = .true.
    else
      result = .false.
    end if
  end function dcdatetime_eq_ff

  logical function dcdatetime_eq_if(i, diff) result(result)
    !
    ! 引数 *diff* の日時差が *i* と等しいかどうかを比較します. *diff*
    ! を秒数に換算した値と *i* とが等しい場合, .true. が返ります.
    !
    use dc_date_generic, only: operator(==)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    integer, intent(in):: i
  continue
    result = real(i) == diff
  end function dcdatetime_eq_if

  logical function dcdatetime_eq_fi(diff, i) result(result)
    use dc_date_generic, only: operator(==)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    integer, intent(in):: i
  continue
    result = i == diff
  end function dcdatetime_eq_fi

  logical function dcdatetime_eq_rf(r, diff) result(result)
    !
    ! 引数 *diff* の日時差が *r* と等しいかどうかを比較します. *diff*
    ! を秒数に換算した値と *r* とが等しい場合, .true. が返ります.
    !
    use dc_scaledsec, only: operator(==)
    use dc_date_generic, only: EvalSclSec
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    real, intent(in):: r
  continue
    if (EvalSclSec(diff) == r) then
      result = .true.
    else
      result = .false.
    end if
  end function dcdatetime_eq_rf

  logical function dcdatetime_eq_fr(diff, r) result(result)
    use dc_date_generic, only: operator(==)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    real, intent(in):: r
  continue
    result = r == diff
  end function dcdatetime_eq_fr

  logical function dcdatetime_eq_df(d, diff) result(result)
    !
    ! 引数 *diff* の日時差が *d* と等しいかどうかを比較します. *diff*
    ! を秒数に換算した値と *d* とが等しい場合, .true. が返ります.
    !
    use dc_date_generic, only: EvalSclSec
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_scaledsec, only: operator(==)
    use dc_types, only: DP
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    real(DP), intent(in):: d
  continue
    if (EvalSclSec(diff) == d) then
      result = .true.
    else
      result = .false.
    end if
  end function dcdatetime_eq_df

  logical function dcdatetime_eq_fd(diff, d) result(result)
    use dc_date_generic, only: operator(==)
    use dc_types, only: DP
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    real(DP), intent(in):: d
  continue
    result = d == diff
  end function dcdatetime_eq_fd
