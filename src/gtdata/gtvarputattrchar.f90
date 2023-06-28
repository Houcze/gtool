!
!= 属性の付加
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarputattrchar.f90,v 1.6 2009-05-25 09:55:57 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Put_Attr
! として提供されます。

subroutine GTVarPutAttrLogical(var, name, value, err)
  !
  !== 属性の付加
  !
  ! 変数 *var* に, 属性名 *name* とその値 *value* を付加します。
  !
  ! *Put_Attr* は複数のサブルーチンの総称名なので、
  ! *value* には様々な型の変数を与えることが可能です。
  ! 以下のサブルーチンを参照してください。
  !
  ! 引数に *xtype* を持つものは、その引数に型を指定することで、
  ! 引数 *value* には文字型を与えても、
  ! 整数型、実数型 (単精度、倍精度) の値を付加することが可能です。
  ! 下記のサブルーチンを参照ください。
  !
  ! エラーが発生した場合、引数 *err* が与えられる場合は *err* が
  ! <tt>.true.</tt> となって返ります。
  ! 引数 *err* を与えなければプログラムは停止します。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: put_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: put_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_string, only: toChar
  implicit none
  type(GT_VARIABLE),  intent(inout)        :: var
  character(len = *), intent(in)           :: name
  logical,            intent(in)           :: value
  logical,            intent(out), optional:: err
  integer:: class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    if (value) then
      call put_attr(GD_NC_VARIABLE(cid), name, "true", err=err)
    else
      call put_attr(GD_NC_VARIABLE(cid), name, "false", err=err)
    endif
  else if (class == vtb_class_memory) then
    if (value) then
      call put_attr(GD_MEM_VARIABLE(cid), name, "true")
    else
      call put_attr(GD_MEM_VARIABLE(cid), name, "false")
    endif
    if (present(err)) err = .false.
  endif
end subroutine GTVarPutAttrLogical

!subroutine GTVarPutAttrString(var, name, value, err)
!  !--
!  ! VSTRING 型を引き取り上記 put_attr を呼び出す。下位層のことは関知しない
!  !++
!  use gtdata_types, only: GT_VARIABLE
!  use dc_string, only: VSTRING, vchar, operator(==), len
!  use gtdata_generic, only: put_attr
!  implicit none
!  type(GT_VARIABLE), intent(inout):: var
!  character(len = *), intent(in):: name
!  type(VSTRING), intent(in):: value
!  logical, intent(out), optional:: err
!continue
!  call put_attr(var, name, vchar(value, len(value)), err=err)
!end subroutine GTVarPutAttrString

subroutine GTVarPutAttrInt(var, name, value, err)
  !
  ! まずは上記の Put_Attr
  ! (または GTVarPutAttrChar および GTVarPutAttrReal)
  ! を参照してください。
  !
  ! *value* は配列を受け取るので、スカラーを書き出すには
  ! Fortran の配列構成子 <tt>(/ ... /)</tt> を使ってください。
  ! たとえば、スカラー a から長さ 1 の配列 <tt>(/a/)</tt>
  ! を作ることができます。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: put_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: put_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_string, only: toChar
  type(GT_VARIABLE), intent(inout):: var
  character(len = *), intent(in):: name
  integer, intent(in):: value(:)
  logical, intent(out), optional:: err
  integer:: class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call put_attr(GD_NC_VARIABLE(cid), name, value, err)
  else if (class == vtb_class_memory) then
    call put_attr(GD_MEM_VARIABLE(cid), name, trim(toChar(value)))
    if (present(err)) err = .false.
  endif
end subroutine GTVarPutAttrInt

subroutine GTVarPutAttrReal(var, name, value, err)
  !
  ! まずは上記の Put_Attr
  ! (または GTVarPutAttrChar および GTVarPutAttrReal)
  ! を参照してください。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: put_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: put_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_string, only: toChar
  implicit none
  type(GT_VARIABLE), intent(inout):: var
  character(len = *), intent(in):: name
  real, intent(in):: value(:)
  logical, intent(out), optional:: err
  integer:: class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call put_attr(GD_NC_VARIABLE(cid), name, value, err)
  else if (class == vtb_class_memory) then
    call put_attr(GD_MEM_VARIABLE(cid), name, trim(toChar(value)))
    if (present(err)) err = .false.
  endif
end subroutine GTVarPutAttrReal

subroutine GTVarPutAttrDouble(var, name, value, err)
  !
  ! まずは上記の Put_Attr
  ! (または GTVarPutAttrChar および GTVarPutAttrReal)
  ! を参照してください。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: put_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: put_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_string, only: toChar
  use dc_types, only: DP
  implicit none
  type(GT_VARIABLE), intent(inout):: var
  character(len = *), intent(in):: name
  real(DP), intent(in):: value(:)
  logical, intent(out), optional:: err
  integer:: class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call put_attr(GD_NC_VARIABLE(cid), name, value, err)
  else if (class == vtb_class_memory) then
    call put_attr(GD_MEM_VARIABLE(cid), name, trim(toChar(value)))
    if (present(err)) err = .false.
  endif
end subroutine GTVarPutAttrDouble

subroutine GTVarPutAttrChar(var, name, value, xtype, err)
  !
  ! まずは上記の Put_Attr
  ! (または GTVarPutAttrChar)
  ! を参照してください。
  !
  ! *xtype* に型を指定することで、引数 *value* には文字型を与えても、
  ! 整数型、実数型 (単精度、倍精度) の値を付加することが可能です。
  !
  ! *xtype* には与える文字列として、以下のものが有効です。
  ! これら以外の場合は文字型の値が与えられます。
  !
  ! 整数型          :: "INTEGER", "integer", "int"
  ! 実数型 (単精度) :: "REAL", "real", "float"
  ! 実数型 (倍精度) :: "DOUBLEPRECISION", "DOUBLE", "double"
  !--
  ! gtdata/gtdata_netcdf/gdncputattrchar.f90#GDNcVarPutAttrChar 参照
  !++
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: put_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: put_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_trace, only: beginsub, endsub
  implicit none
  type(GT_VARIABLE), intent(inout):: var
  character(len = *), intent(in):: name
  character(len = *), intent(in):: value
  character(len = *), intent(in), optional:: xtype
  logical, intent(out), optional:: err
  integer:: class, cid
  character(*), parameter:: subnam = "gtvarputattrchar"
continue
  call beginsub(subnam, "%d:%c = %c", i=(/var%mapid/), c1=trim(name), c2=trim(value))
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call put_attr(GD_NC_VARIABLE(cid), name, value, xtype, err)
  else if (class == vtb_class_memory) then
    call put_attr(GD_MEM_VARIABLE(cid), name, value)
    if (present(err)) err = .false.
  endif
  call endsub(subnam)
end subroutine GTVarPutAttrChar
