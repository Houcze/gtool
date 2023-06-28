!
!= 次元の削除
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvardeldim.f90,v 1.3 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Del_dim
! として提供されます。

subroutine GTVarDelDim(var, dimord, err)
  !
  !== 次元の削除
  !
  ! 変数 *var* の次元 *dimord* を削除します。
  ! 次元対応表の順位を下げ有効次元数をデクリメントするだけなので、
  ! 当該次元がすでに縮退していれば、この操作のあとでも入出力が可能です。
  !
  ! エラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  !--
  ! 実際には、次元対応表の順位を下げ有効次元数をデクリメント
  ! するだけなので、入出力に支障はない。
  !
  ! NetCDF 実装においては、変数は削除されず、
  ! 別の名称に改名されるだけです。
  ! これは netCDF API に変数の削除が欠けているためです。
  !++
  use gtdata_types, only: gt_variable
  use gtdata_internal_map, only: map_lookup, gt_dimmap, map_set_ndims, map_set
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(gt_variable), intent(in):: var
  integer, intent(in):: dimord
  logical, intent(out):: err
  type(gt_dimmap), allocatable:: map(:)
  type(gt_dimmap):: tmpmap
  integer:: ndimsp, stat
  character(*), parameter:: subname = 'GTVarDelDim'
continue
  err = .true.
  call beginsub(subname)
  if (dimord < 1) then
    call endsub(subname, "negative dimord=%d invalid", i=(/dimord/))
    return
  endif
  call map_lookup(var, ndims=ndimsp)
  if (ndimsp <= 0) then
    call endsub(subname, "variable invalid")
    return
  else if (dimord > ndimsp) then
    call endsub(subname, "dimord=%d not exist", i=(/dimord/))
    return
  endif

  allocate(map(ndimsp))
  call map_lookup(var, map=map)
  tmpmap = map(dimord)
  map(dimord: ndimsp-1) = map(dimord+1: ndimsp)
  map(ndimsp) = tmpmap
  call map_set(var, map, stat)
  deallocate(map)

  call map_set_ndims(var, ndims = ndimsp - 1, stat=stat)
  err = stat /= 0
  call endsub(subname)
end subroutine GTVarDelDim
