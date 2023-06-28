!= 初期設定チェッカ
!= Checker of initialization
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyinitialized.f90,v 1.1 2009-05-06 14:23:12 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  logical function HistoryInitialized0( history ) result(result)
    !
    ! *history* が初期設定されている場合には .true. が, 
    ! 初期設定されていない場合には .false. が返ります. 
    !
    ! If *history* is initialized, .true. is returned. 
    ! If *history* is not initialized, .false. is returned. 
    !
    use gtool_history_types, only: GT_HISTORY
    implicit none
    type(GT_HISTORY), intent(in):: history
  continue
    result = history % initialized
  end function HistoryInitialized0

  !-------------------------------------------------------------------

  logical function HistoryInitialized1( history ) result(result)
    !
    ! 使用方法は HistoryInitialized と同様です. 
    !
    ! Usage is same as "HistoryInitialized".
    !
    !--
    ! 総称名 initialized として提供するための関数です. 
    ! 機能は HistoryInitialized1 と同じです. 
    !++
    use gtool_history_types, only: GT_HISTORY
    use gtool_history_generic, only: HistoryInitialized
    implicit none
    type(GT_HISTORY), intent(in):: history
  continue
    result = HistoryInitialized( history )
  end function HistoryInitialized1

