!= 利用者定義関数 max
!= User defined operation "max"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimemax.f90,v 1.2 2009-05-31 11:46:03 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  function dcdatetime_max_tt(time1, time2) result(result)
    !
    ! 2 つの引数の日時を比較し, より日時が進んでいる方を返します. 
    !
    use dc_date_generic, only: operator(>)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    implicit none
    type(DC_DATETIME):: result
    type(DC_DATETIME), intent(in):: time1, time2
  continue
    if ( time1 > time2 ) then
      result = time1
    else
      result = time2
    end if
  end function dcdatetime_max_tt

  function dcdatetime_max_ff(diff1, diff2) result(result)
    !
    ! 2 つの引数の日時差を比較し, より大きい方を返します. 
    !
    use dc_date_generic, only: operator(>)
    use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
    use dc_date_internal, only: dcdate_nondimcheck
    implicit none
    type(DC_DIFFTIME):: result
    type(DC_DIFFTIME), intent(in):: diff1, diff2
  continue
    if ( diff1 > diff2 ) then
      result = diff1
    else
      result = diff2
    end if
    call dcdate_nondimcheck('dc_date#max', diff1, diff2, result)
  end function dcdatetime_max_ff
