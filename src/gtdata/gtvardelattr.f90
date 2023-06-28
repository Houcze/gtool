!
!= 属性の削除
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvardelattr.f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Del_Attr
! として提供されます。

subroutine GTVarDelAttr(var, name, err)
  !
  !== 属性の削除
  !
  ! 変数 *var* の属性 *name* を削除します。
  !
  ! 属性の削除時にエラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: del_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: del_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_error, only: GT_EBADVAR, StoreError
  implicit none
  type(GT_VARIABLE), intent(inout):: var
  character(len = *), intent(in):: name
  logical, intent(out), optional:: err
  integer:: class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call del_attr(GD_NC_VARIABLE(cid), name, err)
  else if (class == vtb_class_memory) then
    call del_attr(GD_MEM_VARIABLE(cid), name, err)
  else
    call StoreError(GT_EBADVAR, 'GTVarDelAttr', err)
  endif
end subroutine GTVarDelAttr
