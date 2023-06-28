!
!= 入出力範囲を移動
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarslicenext.f90,v 1.3 2009-05-25 09:55:57 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Slice_Next
! として提供されます。

subroutine GTVarSliceNext(var, dimord, err, stat)
  !
  !== 入出力範囲を移動
  !
  ! 変数 *var* の *dimord* 番目の次元の *start* 値を *stride* *
  ! *count* 個だけ増やすことによって次元範囲を移動します。*dimord*
  ! を省略すると、どれかの次元についてこの操作を行います。成功した
  ! 場合 *stat* が 0 になリます。
  !
  ! いずれかの次元について *start*, *stride* 値が 1 になるような
  ! Slice を設定しておいて、Slice_Next を順次呼び出すと変数全体
  ! を走査することができます。
  !
  ! 入出力範囲を移動する際にエラーが生じた場合、メッセージを出力
  ! してプログラムは 強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  use gtdata_types, only: GT_VARIABLE
  use dc_error, only: GT_EFAKE, StoreError, DC_NOERR, gt_enomoredims, &
    & NF90_EINVAL, NF90_ENOTVAR
  use gtdata_internal_map, only: map_lookup, gt_dimmap, map_set
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(in out):: var
  integer, intent(in), optional:: dimord
  logical, intent(out), optional:: err
  integer, intent(out), optional:: stat
  type(gt_dimmap), allocatable:: map(:)
  integer:: mystat, vid, id, nd, idim_lo, idim_hi, ilast
continue
  call beginsub('gtvarslicenext')
  if (present(dimord)) call DbgMessage('dimord=%d', i=(/dimord/))

  call map_lookup(var, vid=vid, ndims=nd)
  if (vid < 0) then
    mystat = NF90_ENOTVAR
    goto 999
  endif
  if (nd <= 0) then
    call DbgMessage('dimension map not associated')
    mystat = gt_enomoredims
    goto 999
  endif
  allocate(map(nd))
  call map_lookup(var, map=map)

  if (present(dimord)) then
    if (dimord < 0 .or. dimord <= size(map)) then
      call DbgMessage('dimord=%d is out of 1..%d', i=(/dimord, size(map)/))
      mystat = NF90_EINVAL
      goto 995
    endif
    idim_lo = dimord
    idim_hi = dimord
  else
    idim_lo = 1
    idim_hi = size(map)
  endif
  call DbgMessage('idim scan range=(%d:%d)', i=(/idim_lo, idim_hi/))

  mystat = gt_enomoredims
  do, id = idim_lo, idim_hi
    ilast = map(id)%start + (map(id)%count * 2 - 1) * map(id)%stride
    call DbgMessage('last_index=%d allcount=%d', &
      & i=(/ilast, map(id)%allcount/))
    if (ilast >= 1 .and. ilast <= map(id)%allcount) then
      map(id)%start = map(id)%start + map(id)%count * map(id)%stride
      mystat = dc_noerr
      exit
    endif
  enddo
  if (mystat /= dc_noerr) goto 995
  call map_set(var, map, mystat)

995 continue
  deallocate(map)

999 continue
  if (present(stat)) then
    stat = mystat
    if (present(err)) err = (mystat /= DC_NOERR)
  else
    call StoreError(mystat, "GTVarSliceNext", err)
  endif
  call endsub('gtvarslicenext', 'stat=%d', i=(/mystat/))
end subroutine GTVarSliceNext
