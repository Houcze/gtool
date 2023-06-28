!= GT_HISTORY_AXIS 変数のクリア
!= Destructor of GT_HISTORY_AXIS
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyaxisclear.f90,v 1.2 2009-05-25 09:45:20 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryAxisClear(axis)
    !
    !== GT_HISTORY_AXIS 型変数初期化
    !
    ! *axis* で与えられた変数を HistoryAxisCreate による初期設定よりも
    ! さらに前の状態に初期化します。
    !
    ! Destructor of GT_HISTORY_AXIS
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_types, only: STRING, TOKEN, DP
    implicit none
    type(GT_HISTORY_AXIS),intent(inout) :: axis
    character(len = *), parameter:: subname = "HistoryAxisClear1"
    call BeginSub(subname)
    axis % name     = ""
    axis % length   = 0
    axis % longname = ""
    axis % units    = ""
    axis % xtype    = ""
    if (associated(axis % attrs)) then
      deallocate(axis % attrs)
    end if
    call EndSub(subname)
  end subroutine HistoryAxisClear
