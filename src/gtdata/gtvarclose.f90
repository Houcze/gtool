!
!= 変数の終了処理
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvarclose.f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

subroutine GTVarClose(var, err)
  !
  !== 変数の終了処理
  !
  ! 変数 *var* の終了処理を行います。Open または Create されたものは
  ! プログラムの最後に必ずこのサブルーチンを用いて終了処理を行ってください。
  !
  ! 終了処理の際にエラーが生じた場合、メッセージを出力してプログラムは
  ! 強制終了します。*err* を与えてある場合にはこの引数に .true.
  ! が返り、プログラムは終了しません。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: vtb_class_netcdf, vtb_class_memory, &
    & maptabdelete, map_lookup
  use gtdata_internal_vartable, only: vartabledelete, vartablelookup
  use gtdata_netcdf_generic, only: Close
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_error, only: StoreError, GT_EBADVAR, dc_noerr
  use dc_trace, only: beginsub, endsub, DbgMessage
  use gtdata_memory_generic, only: Close
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  implicit none
  type(GT_VARIABLE), intent(in), target:: var
  logical, intent(out), optional:: err
  integer:: vid, class, cid
  logical:: action, myerr
continue
  call beginsub('gtvarclose', fmt='var=%d', i=(/var%mapid/))
  call map_lookup(var, vid=vid)
  call maptabdelete(var, myerr)
  if (myerr) goto 999
  ! vid が 0 になるのは dup_dimmap で作られたハンドル
  if (vid == 0) goto 999
  call vartablelookup(vid, class, cid)
  call vartabledelete(vid, action, myerr)
  if (myerr) goto 999
  if (.not. action) then
    call DbgMessage('refcount decrement only, no close internal var')
    goto 999
  else if (class == vtb_class_netcdf) then
    call Close(GD_NC_VARIABLE(cid), myerr)
    if (myerr) goto 999
  else if (class == vtb_class_memory) then
    call Close(GD_MEM_VARIABLE(cid))
    myerr = .false.
  else
    call StoreError(GT_EBADVAR, "GTVarClose", err)
    call endsub('GTVarClose', 'badvar')
    myerr = .true.
  endif
999 continue
  call endsub('gtvarclose')
  if (present(err)) err = myerr
end subroutine GTVarClose
