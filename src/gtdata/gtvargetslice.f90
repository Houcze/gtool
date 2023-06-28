!
!= 変数入出力範囲限定情報を取得
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvargetslice.f90,v 1.4 2009-10-12 04:02:30 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Get_Slice
! として提供されます。

subroutine GTVarGetSlice(var, dimord, start, count, stride, count_compact)
  !
  !== 変数入出力範囲限定情報を取得
  !
  ! 変数 *var* の *dimord* 番目の次元に関して、
  ! Slice によって設定された入出力範囲の情報を取得します。
  !
  ! *start*, *count*, *stride* に関しては Slice を参照してください。
  !
  ! *count_compact* に .true. に指定すると、縮退された次元も
  ! 含んで問い合わせを行います。
  !
  ! *Get_Slice* は 2 つのサブルーチンの総称名であり、
  ! 他にも変数の依存する全ての次元に関して一括で情報を取得する
  ! 方法もあります。上記のサブルーチンを参照ください。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: GT_DIMMAP, map_lookup, dimord_skip_compact
  implicit none
  type(GT_VARIABLE), intent(in):: var
  integer, intent(in):: dimord
  integer, intent(out), optional:: start
  integer, intent(out), optional:: count
  integer, intent(out), optional:: stride
  logical, intent(in), optional:: count_compact
  type(GT_DIMMAP), allocatable:: map(:)
  integer:: vid, udimord, ndims
  logical:: allmode
continue
  allmode = .true.
  if (present(count_compact)) allmode = count_compact
  call map_lookup(var, vid=vid, ndims=ndims)
  if (vid < 0 .or. ndims <= 0) goto 999
  allocate(map(ndims))
  call map_lookup(var, map=map)
  if (allmode) then
    udimord = dimord
  else
    udimord = dimord_skip_compact(dimord, map)
  endif
  if (udimord < 1 .or. udimord > size(map)) goto 997

  if (present(start)) start = map(udimord)%start
  if (present(count)) count = map(udimord)%count
  if (present(stride)) stride = map(udimord)%stride
  deallocate(map)
  return

997 continue
  deallocate(map)
999 continue
  if (present(start)) start = -1
  if (present(count)) count = -1
  if (present(stride)) stride = -1
end subroutine GTVarGetSlice
