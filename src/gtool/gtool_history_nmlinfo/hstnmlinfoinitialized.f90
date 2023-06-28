!= GTHST_NMLINFO 型の変数初期設定チェック
!= Checker of initialization of "GTHST_NMLINFO"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfoinitialized.f90,v 1.1 2009-05-11 15:15:15 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  logical function HstNmlInfoInitialized( gthstnml ) result(result)
    !
    ! *gthstnml* が初期設定されている場合には .true. が, 
    ! 初期設定されていない場合には .false. が返ります. 
    !
    ! If *gthstnml* is initialized, .true. is returned. 
    ! If *gthstnml* is not initialized, .false. is returned. 
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
  continue
    result = gthstnml % initialized
  end function HstNmlInfoInitialized
