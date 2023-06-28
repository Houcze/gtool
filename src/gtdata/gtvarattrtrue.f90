!
!= 論理型属性の入力
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarattrtrue.f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

logical function GTVarAttrTrue(var, name, default) result(result)
  !
  !== 論理型属性の入力
  !
  ! 変数 *var* に付加されている属性 *name* の値を返します。
  ! 属性値が論理型属性の場合のみ用いることが出来ます。
  !
  ! 以下の場合には .false. が返ります。
  !
  ! * 属性の値が文字型で "", "0", "0.0", "0.", ".0", "FALSE", 
  !   "false", ".FALSE.",  ".false.", "F", "f", "0.0D0", "0.0d0"
  !   のいづれかであった場合
  ! * 属性の値が負の実数であった場合
  !
  ! 属性の値が正常に取得できず、且つ *default* が与えられて
  ! いた場合、その値が返ります。*default* が与えられていなかった
  ! 場合には .false. が返ります。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: Attr_True
  use gtdata_memory_types, only: memvar_t => GD_MEM_VARIABLE
  use dc_error, only: StoreError, GT_ENOTVAR
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  logical, intent(in), optional:: default
  integer:: class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call Get_Attr(GD_NC_VARIABLE(cid), name, result, default)
  else if (class == vtb_class_memory) then
    result = Attr_True(memvar_t(cid), name, default)
  else
    call StoreError(GT_ENOTVAR, "GTVarAttrTrue(NO VARIABLE)")
    result = .FALSE.
  endif
end function GTVarAttrTrue
