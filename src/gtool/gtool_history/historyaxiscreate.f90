!= GT_HISTORY_AXIS 変数の作成
!= Constructor of GT_HISTORY_AXIS
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyaxiscreate.f90,v 1.2 2009-05-25 09:45:20 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryAxisCreate1( axis, &
    & name, size, longname, units, xtype)
    !
    !== GT_HISTORY_AXIS 型変数作成
    !
    ! GT_HISTORY_AXIS 型変数を作成します。
    ! このサブルーチンによる設定の後、
    ! HistoryCreate の *axes* に与えます。
    ! さらに属性を付加する場合には HistoryAxisAddAttr
    ! を用いてください。
    !
    ! Constructor of GT_HISTORY_AXIS
    !
    use dc_types, only: STRING, TOKEN, DP
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    implicit none
    type(GT_HISTORY_AXIS),intent(out) :: axis
    character(*) , intent(in):: name     ! 次元変数名
    integer, intent(in):: size     ! 次元長 (配列サイズ)
    character(*) , intent(in):: longname ! 次元変数の記述的名称
    character(*) , intent(in):: units    ! 次元変数の単位
    character(*) , intent(in):: xtype    ! 次元変数の型
    character(len = *), parameter:: subname = "HistoryAxisCreate1"
  continue
    call BeginSub(subname)
    axis % name = name
    axis % length = size
    axis % longname = longname
    axis % units = units
    axis % xtype = xtype
    call EndSub(subname)
  end subroutine HistoryAxisCreate1

  subroutine HistoryAxisCreate2( axis, &
    & name, size, longname, units, xtype)
    !
    ! 使用方法は HistoryAxisCreate と同様です. 
    !
    ! Usage is same as "HistoryAxisCreate".
    !
    !--
    ! 総称名 Create として提供するためのサブルーチンです. 
    ! 機能は HistoryAxisCreate1 と同じです. 
    !++
    use dc_types, only: STRING, TOKEN, DP
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    implicit none
    type(GT_HISTORY_AXIS),intent(out) :: axis
    character(*) , intent(in):: name     ! 次元変数名
    integer, intent(in):: size     ! 次元長 (配列サイズ)
    character(*) , intent(in):: longname ! 次元変数の記述的名称
    character(*) , intent(in):: units    ! 次元変数の単位
    character(*) , intent(in):: xtype    ! 次元変数の型
    character(len = *), parameter:: subname = "HistoryAxisCreate2"
  continue
    call BeginSub(subname)
    axis % name = name
    axis % length = size
    axis % longname = longname
    axis % units = units
    axis % xtype = xtype
    call EndSub(subname)
  end subroutine HistoryAxisCreate2
