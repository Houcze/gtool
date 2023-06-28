!= GT_HISTORY_AXIS 変数への問い合わせ
!= Inquire for a GT_HISTORY_AXIS variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyaxisinquire.f90,v 1.2 2009-05-25 09:45:20 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryAxisInquire1( axis, &
    & name, size, longname, units, xtype )
    !
    !== GT_HISTORY_AXIS 型変数への問い合わせ
    !
    ! GT_HISTORY_AXIS 型の変数内の各情報を参照します。
    !
    use dc_types, only: STRING, TOKEN, DP
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    implicit none
    type(GT_HISTORY_AXIS),intent(in) :: axis
    character(*) , intent(out), optional:: name     ! 次元変数名
    integer, intent(out), optional:: size     ! 次元長 (配列サイズ)
    character(*) , intent(out), optional:: longname ! 次元変数の記述的名称
    character(*) , intent(out), optional:: units    ! 次元変数の単位
    character(*) , intent(out), optional:: xtype    ! 次元変数の型
    character(len = *), parameter:: subname = "HistoryAxisInquire1"
  continue
    call BeginSub(subname)
    if (present(name)) then
      name     = axis % name
    end if
    if (present(size)) then
      size     = axis % length
    end if
    if (present(longname)) then
      longname = axis % longname
    end if
    if (present(units)) then
      units    = axis % units
    end if
    if (present(xtype)) then
      xtype    = axis % xtype
    end if
    call EndSub(subname)
  end subroutine HistoryAxisInquire1

  !-------------------------------------------------------------------

  subroutine HistoryAxisInquire2( axis, &
    & name, size, longname, units, xtype)
    !
    ! 使用方法は HistoryAxisInquire と同様です. 
    !
    ! Usage is same as "HistoryAxisInquire".
    !
    !--
    ! 総称名 Inquire として提供するためのサブルーチンです. 
    ! 機能は HistoryAxisInquire1 と同じです. 
    !++
    !
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_generic, only: HistoryAxisInquire
    implicit none
    type(GT_HISTORY_AXIS),intent(in) :: axis
    character(*) , intent(out), optional:: name     ! 次元変数名
    integer, intent(out), optional:: size     ! 次元長 (配列サイズ)
    character(*) , intent(out), optional:: longname ! 次元変数の記述的名称
    character(*) , intent(out), optional:: units    ! 次元変数の単位
    character(*) , intent(out), optional:: xtype    ! 次元変数の型
    character(len = *), parameter:: subname = "HistoryAxisInquire2"
  continue
    call BeginSub(subname)
    call HistoryAxisInquire( axis, &
      & name, size, longname, units, xtype)
    call EndSub(subname)
  end subroutine HistoryAxisInquire2
