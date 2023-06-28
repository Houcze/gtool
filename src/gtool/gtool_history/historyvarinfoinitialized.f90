!= GT_HISTORY_VARINFO の初期設定チェッカ
!= Checker of initialization of GT_HISTORY_VARINFO 
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyvarinfoinitialized.f90,v 1.1 2009-05-06 14:23:12 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  logical function HistoryVarinfoInitialized0( varinfo ) result(result)
    !
    ! *varinfo* が初期設定されている場合には .true. が, 
    ! 初期設定されていない場合には .false. が返ります. 
    !
    ! If *varinfo* is initialized, .true. is returned. 
    ! If *varinfo* is not initialized, .false. is returned. 
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    implicit none
    type(GT_HISTORY_VARINFO),intent(in) :: varinfo
  continue
    result = varinfo % initialized
  end function HistoryVarinfoInitialized0
