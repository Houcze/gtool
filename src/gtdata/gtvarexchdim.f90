!
!= 次元順序番号の交換
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarexchdim.f90,v 1.3 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Exch_dim
! として提供されます。

subroutine GTVarExchDim(var, dimord1, dimord2, count_compact, err)
  !
  !== 次元順序番号の交換
  !
  ! 変数 *var* の次元順序番号 <b>dimord1</b>, <b>dimord2</b> のそれぞれに
  ! 対応する次元を入れ替えます。
  !
  ! *count_compact* に .true. を渡すと、縮退した次元も含めて
  ! 動作します。
  !
  ! エラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: map_lookup, gt_dimmap, map_set_ndims, map_set, &
    & dimord_skip_compact
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(in):: var
  integer, intent(in):: dimord1, dimord2
  logical, intent(in), optional:: count_compact
  logical, intent(out):: err
  type(gt_dimmap), allocatable:: map(:)
  type(gt_dimmap):: tmpmap
  integer:: ndimsp, stat, idim1, idim2
  logical:: direct_mode
  character(*), parameter:: subname = 'GTVarExchDim'
continue
  err = .true.
  direct_mode = .false.
  if (present(count_compact)) then
    direct_mode = count_compact
  endif
  call beginsub(subname)
  if (dimord1 < 1 .or. dimord2 < 1) then
    call endsub(subname, "negative dimord=%d %d invalid", i=(/dimord1, dimord2/))
    return
  endif
  call map_lookup(var, ndims=ndimsp)
  if (ndimsp <= 0) then
    call endsub(subname, "variable invalid")
    return
  else if (dimord1 > ndimsp .or. dimord2 > ndimsp) then
    call endsub(subname, "dimord=%d %d not exist", i=(/dimord1, dimord2/))
    return
  endif

  allocate(map(ndimsp))
  call map_lookup(var, map=map)

  if (.not. direct_mode) then
    idim1 = dimord_skip_compact(dimord1, map)
    idim2 = dimord_skip_compact(dimord2, map)
    if (idim1 < 0 .or. idim2 < 0) then
      call endsub(subname, "dimord=%d %d not found after compaction", &
        & i=(/dimord1, dimord2/))
      deallocate(map)
      return
    endif
  else
    idim1 = dimord1
    idim2 = dimord2
  endif

  tmpmap = map(idim1)
  map(idim1) = map(idim2)
  map(idim2) = tmpmap
  call map_set(var, map, stat)
  deallocate(map)

  err = stat /= 0
  call endsub(subname)
end subroutine GTVarExchDim
