!
!= 変数入出力範囲限定情報を取得 (全ての次元の情報を一括取得)
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvargetsliceall.f90,v 1.1 2009-03-20 09:09:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Get_Slice
! として提供されます。

! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.

subroutine GTVarGetSliceAll(var, start, count, stride)
  !
  !== 変数入出力範囲限定情報を取得 (全ての次元の情報を一括取得)
  !
  ! 変数 *var* に関して、
  ! Slice によって設定された入出力範囲の情報を取得します。
  ! 全次元の入出力範囲について一括取得するため、
  ! あらかじめ Inquire(var, alldims) して次元の数を確保
  ! しなければなりません。
  !
  ! *start*, *count*, *stride* に関しては Slice を参照してください。
  !
  ! *Get_Slice* は 2 つのサブルーチンの総称名であり、
  ! 他にもある1つの次元に関して情報を取得する
  ! 方法もあります。下記のサブルーチンを参照ください。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: GTVarGetSlice
  implicit none
  type(GT_VARIABLE), intent(in):: var
  integer, intent(out), optional:: start(:), count(:), stride(:)
  integer:: nd, i
  logical:: all
  nd = HUGE(1)
  all = present(start) .and. present(count) .and. present(stride)
  if (present(start)) nd = min(nd, size(start))
  if (present(count)) nd = min(nd, size(count))
  if (present(stride)) nd = min(nd, size(stride))
  do, i = 1, nd
    if (all) then
      call GTVarGetSlice(var, i, start(i), count(i), stride(i))
      cycle
    endif
    if (present(start)) call GTVarGetSlice(var, i, start=start(i))
    if (present(count)) call GTVarGetSlice(var, i, count=count(i))
    if (present(stride)) call GTVarGetSlice(var, i, stride=stride(i))
  enddo
end subroutine GTVarGetSliceAll

