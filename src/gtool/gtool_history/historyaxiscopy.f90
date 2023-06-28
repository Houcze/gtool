!= GT_HISTORY_AXIS のコピー
!= Copy GT_HISTORY_AXIS
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyaxiscopy.f90,v 1.2 2009-05-25 09:45:20 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryAxisCopy1(axis_dest, axis_src, err, &
    & name, length, longname, units, xtype)
    !
    !== GT_HISTORY_AXIS 型変数コピー
    !
    ! GT_HISTORY_AXIS 型の変数 *axis_src* を
    ! *axis_dest* にコピーします。
    ! *axis_src* は HistoryAxisCreate によって初期設定されている必要が
    ! あります。
    ! さらに属性を付加する場合には HistoryAxisAddAttr
    ! を用いてください。
    !
    ! *err* を与えておくと、コピーの際何らかの不具合が生じても
    ! 終了せずに err が真になって返ります。
    ! 
    ! *err* 以降の引数は、コピーの際に上書きする値です。
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default, copy_attrs
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_present,only: present_select
    implicit none
    type(GT_HISTORY_AXIS),intent(out) :: axis_dest ! コピー先 GT_HISTORY_AXIS
    type(GT_HISTORY_AXIS),intent(in)  :: axis_src  ! コピー元 GT_HISTORY_AXIS
    logical, intent(out), optional :: err
    character(*) , intent(in), optional:: name     ! 次元変数名
    integer, intent(in), optional:: length         ! 次元長 (配列サイズ)
    character(*) , intent(in), optional:: longname ! 次元変数の記述的名称
    character(*) , intent(in), optional:: units    ! 次元変数の単位
    character(*) , intent(in), optional:: xtype    ! 次元変数の型
    character(*), parameter:: subname = "HistoryAxisCopy1"
  continue
    call BeginSub(subname)
    axis_dest % name     = present_select('', axis_src % name, name)
    axis_dest % length   = present_select(.false., axis_src % length, length)
    axis_dest % longname = present_select('', axis_src % longname, longname)
    axis_dest % units    = present_select('', axis_src % units, units)
    axis_dest % xtype    = present_select('', axis_src % xtype, xtype)

    if (associated( axis_src % attrs ) ) then
      allocate(  axis_dest % attrs( size( axis_src % attrs) )  )
      call copy_attrs( from = axis_src % attrs, &
        &              to = axis_dest % attrs, err = err)
    end if
    call EndSub(subname)
  end subroutine HistoryAxisCopy1

  !-------------------------------------------------------------------

  subroutine HistoryAxisCopy2(axis_dest, axis_src, err, &
    & name, length, longname, units, xtype)
    !
    ! 使用方法は HistoryAxisCopy と同様です. 
    !
    ! Usage is same as "HistoryAxisCopy".
    !
    !--
    ! 総称名 Copy として提供するための関数です. 
    ! 機能は HistoryAxisCopy1 と同じです. 
    !++
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_generic, only: HistoryAxisCopy
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    type(GT_HISTORY_AXIS),intent(out) :: axis_dest ! コピー先 GT_HISTORY_AXIS
    type(GT_HISTORY_AXIS),intent(in)  :: axis_src  ! コピー元 GT_HISTORY_AXIS
    logical, intent(out), optional :: err
    character(*) , intent(in), optional:: name     ! 次元変数名
    integer, intent(in), optional:: length         ! 次元長 (配列サイズ)
    character(*) , intent(in), optional:: longname ! 次元変数の記述的名称
    character(*) , intent(in), optional:: units    ! 次元変数の単位
    character(*) , intent(in), optional:: xtype    ! 次元変数の型
    character(*), parameter:: subname = "HistoryAxisCopy2"
  continue
    call BeginSub(subname)
    call HistoryAxisCopy(axis_dest, axis_src, err, &
      & name, length, longname, units, xtype)
    call EndSub(subname)
  end subroutine HistoryAxisCopy2
