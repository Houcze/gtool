!= GTHST_NMLINFO 型の変数の定義モードチェック
!= Checker of define mode of "GTHST_NMLINFO"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfodefinemode.f90,v 1.1 2009-05-11 15:15:15 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  logical function HstNmlInfoDefineMode( gthstnml ) result(result)
    !
    ! *gthstnml* が定義モードであれば .true. が, 
    ! 定義モードでなければ .false. が返ります. 
    !
    ! If *gthstnml* is define mode, .true. is returned. 
    ! If *gthstnml* is not define mode, .false. is returned. 
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
  continue
    result = gthstnml % define_mode
  end function HstNmlInfoDefineMode
