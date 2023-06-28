!
!= 文字型属性の入力
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvargetattrsc.f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

subroutine GTVarGetAttrCC(var, name, value, default)
  !--
  ! character 型で受け取る場合は通常の文字型代入と同様、
  ! 受け側変数の長さに合わせて切り捨て・空白埋めを行う。
  ! 属性が存在しない場合 default 値を使う。
  !++

  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: get_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_trace, only: beginsub, endsub
  use dc_types, only: string
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  character(len = *), intent(out):: value
  character(len = *), intent(in), optional:: default
  logical:: err
  integer:: class, cid, stat
  character(len = string):: p_default
  character(len = *), parameter:: subnam = "gtvargetattrcc"
continue
  call beginsub(subnam)
  call var_class(var, class, cid)
  p_default = ""
  if (present(default)) p_default = default
  if (class == vtb_class_netcdf) then
    call get_attr(GD_NC_VARIABLE(cid), name, value, p_default, stat)
  else if (class == vtb_class_memory) then
    call get_attr(GD_MEM_VARIABLE(cid), name, value, err)
    if (err) value = p_default
  endif
  call endsub(subnam, "%d:%d:%c = %c", i=(/class, cid/), &
    & c1=trim(name), c2=trim(value))
end subroutine GTVarGetAttrCC

