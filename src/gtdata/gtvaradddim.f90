!
!= 次元の追加
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvaradddim.f90,v 1.3 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Add_dim
! として提供されます。

subroutine GTVarAddDim(var, dimord, dimvar, err)
  !
  !== 次元の追加
  !
  ! 変数 *var* の *dimord* 番目の位置に次元 *dimvar*
  ! を追加します。*dimord* 番目以降の次元は 1 つ後ろにずれます。
  ! もし *dimord* が *var* の有効次元数よりも大きい場合、
  ! (有効次元数 + 1) が与えられたものと見なされます。
  !
  ! エラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: inquire
  use gtdata_internal_map, only: map_lookup, gt_dimmap, map_set_ndims, map_set, map_resize
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(in):: var
  type(GT_VARIABLE), intent(in):: dimvar
  integer, intent(in):: dimord
  logical, intent(out):: err
  type(gt_dimmap), pointer:: map(:)
  type(gt_dimmap):: tmpmap
  integer:: id, nd, ndimsp, stat, vid
  character(*), parameter:: subname = 'GTVarAddDim'
continue
  err = .true.
  call beginsub(subname)

  if (dimord < 1) then
    call endsub(subname, "negative dimord=%d invalid", i=(/dimord/))
    return
  endif

  ! dimvar をチェックしマップ設定を tmpmap に保存
  call map_lookup(dimvar, vid=vid, ndims=nd)
  if (vid < 0) then
    call endsub(subname, "dimvar invalid")
    return
  endif
  if (nd <= 0) then
    call endsub(subname, "dimvar nondimensional")
    return
  else if (nd > 1) then
    call endsub(subname, "dimvar multidimensional")
    return
  endif
  allocate(map(nd))
  call map_lookup(dimvar, map=map)
  tmpmap = map(1)
  deallocate(map)

  ! dimord 番目 (ただし ndimsp + 1 を越えない) に挿入する隙間をあける
  call map_lookup(var, ndims=ndimsp)
  if (dimord > ndimsp + 1) then
    id = ndimsp + 1
  else
    id = dimord
  endif
  allocate(map(nd + 1))
  call map_resize(var, nd + 1)
  call map_lookup(var, map=map)
  map(id+1: nd+1) = map(id: nd)

  ! 新しい次元への参照を挿入
  map(id)%dimno = -1
  call inquire(dimvar, url=map(id)%url)
  map(id)%allcount = tmpmap%allcount
  map(id)%offset = tmpmap%offset
  map(id)%step = tmpmap%step
  map(id)%start = tmpmap%start
  map(id)%count = tmpmap%count
  map(id)%stride = tmpmap%stride

  ! 登録
  call map_set(var, map=map, stat=stat)
  if (stat /= 0) goto 999
  call map_set_ndims(var, ndims=ndimsp + 1, stat=stat)

999 continue
  err = (stat /= 0)
  call endsub(subname)
end subroutine GTVarAddDim
