!= 利用者定義演算子 (<=) のための関数
!= Functions for user defined operation (<=)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimele.f90,v 1.1 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  logical function dcdatetime_le_tt(time1, time2) result(result)
    !
    ! 2 つの引数の日時を比較します.
    ! 2 つ目の引数に格納される日時が 1 つ目の引数に格納される日時
    ! よりも進んでいる場合かもしくは等しい場合, .true. が返ります.
    !
    use dc_date_generic, only: operator(>)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DATETIME), intent(in):: time1, time2
  continue
    result = .not. time1 > time2
  end function dcdatetime_le_tt

  logical function dcdatetime_le_ff(diff1, diff2) result(result)
    !
    ! 2 つの引数の日時差を比較します.
    ! 2 つ目の引数に格納される日時差が 1 つ目の引数に格納される日時差
    ! よりも大きい場合かもしくは等しい場合, .true. が返ります.
    !
    use dc_date_generic, only: operator(>)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff1, diff2
  continue
    result = .not. diff1 > diff2
  end function dcdatetime_le_ff

  logical function dcdatetime_le_fi(diff, factor) result(result)
    !
    ! 2 つの引数の日時差を比較します.
    ! 2 つ目の引数に格納される日時差が 1 つ目の引数に格納される日時差
    ! よりも大きい場合かもしくは等しい場合, .true. が返ります.
    !
    use dc_date_generic, only: operator(>)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DIFFTIME), intent(in):: diff
    integer, intent(in):: factor
  continue
    result = .not. diff > factor
  end function dcdatetime_le_fi

  logical function dcdatetime_le_if(factor, diff) result(result)
    !
    ! 2 つの引数の日時差を比較します.
    ! 2 つ目の引数に格納される日時差が 1 つ目の引数に格納される日時差
    ! よりも大きい場合かもしくは等しい場合, .true. が返ります.
    !
    use dc_date_generic, only: operator(>)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    integer, intent(in):: factor
    type(DC_DIFFTIME), intent(in):: diff
  continue
    result = .not. factor > diff
  end function dcdatetime_le_if

