!
!= 数値型属性の入力
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvargetattr.f90,v 1.6 2010-06-17 00:41:41 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!
!--
! 引数の型に応じていろいろあるが、どうせ下部構造では同じモノを使っている。
!
! スカラで受け取るのが一番簡単。解釈可能な値がとられ、残りは捨てられる。
!++

subroutine GTVarGetAttrI(var, attrname, value, default)
  !
  !== 属性の入力
  !
  ! 変数 *var* に付加されている属性 *name* の値を返します。
  ! *Get_Attr* は複数のサブルーチンの総称名なので、
  ! *value* には様々な型の変数 (ポインタも可能)
  ! を与えることが可能です。
  ! 以下のサブルーチンを参照してください。
  !
  ! 属性の値が正常に取得できず、且つ *default* が与えられて
  ! いた場合、その値が返ります。
  ! *default* が与えられない場合のデフォルトの値はそれぞれ以下の
  ! 通りです。
  !
  ! character :: "" (空文字)
  ! real      :: netcdf_f77#NF90_FILL_REAL
  ! real(DP)  :: netcdf_f77#NF90_FILL_DOUBLE
  ! integer   :: netcdf_f77#NF90_FILL_INT
  !
  ! *value* がポインタの場合は、型に依らず空状態が返ります。
  !
  ! *value* にポインタを与えた場合、属性の値に応じて自動的に
  ! 割り付けが行われます。そのため、必ず空状態にしてから与えてください。
  !
  ! *value* に固定長配列を用意する場合 *default* が必須になりますが、
  ! これは Fortran の言語仕様上ポインタ方式と引用仕様が同じであっては
  ! ならないからです。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: get_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use netcdf, only: NF90_FILL_INT
  use dc_string, only: StoI
  use dc_error, only: GT_ENOTVAR, StoreError
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: attrname
  integer, intent(out):: value
  integer, intent(in), optional:: default
  integer:: stat, buffer(1), class, cid
  character(STRING):: cbuffer
  logical:: err
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call get_attr(GD_NC_VARIABLE(cid), attrname, buffer, stat, default)
    if (stat >= 1) then
      value = buffer(1)
      return
    end if
  else if (class == vtb_class_memory) then
    call get_attr(GD_MEM_VARIABLE(cid), attrname, cbuffer, err)
    if (.not. err) then
      value = StoI(cbuffer)
      return
    endif
  else
    call StoreError(GT_ENOTVAR, "GTVarGetAttrI")
  endif
  value = NF90_FILL_INT
  if (present(default)) value = default
end subroutine GTVarGetAttrI

subroutine GTVarGetAttrR(var, attrname, value, default)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: get_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use dc_error, only: GT_EBADVAR, StoreError
  use dc_string, only: StoD
  use netcdf, only: NF90_FILL_FLOAT
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: attrname
  real, intent(out):: value
  real, intent(in), optional:: default
  integer:: stat
  real:: buffer(1)
  character(STRING):: cbuffer
  integer:: class, cid
  logical:: err
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call get_attr(GD_NC_VARIABLE(cid), attrname, value=buffer, &
      & stat=stat, default=default)
    if (stat >= 1) then
      value = buffer(1)
      return
    endif
  else if (class == vtb_class_memory) then
    call get_attr(GD_MEM_VARIABLE(cid), attrname, cbuffer, err)
    if (.not. err) then
      value = StoD(cbuffer)
      return
    endif
  else
    call StoreError(GT_EBADVAR, "GTVarGetAttrR")
  endif
  if (present(default)) then
    value = default
  else
    value = NF90_FILL_FLOAT
  endif
end subroutine GTVarGetAttrR

subroutine GTVarGetAttrD(var, attrname, value, default)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: get_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_string, only: StoD
  use dc_error, only: GT_ENOTVAR, StoreError
  use dc_types, only: DP
  use netcdf, only: NF90_FILL_DOUBLE
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: attrname
  real(DP), intent(out):: value
  real(DP), intent(in), optional:: default
  integer:: stat
  real(DP):: buffer(1)
  character(STRING):: cbuffer
  integer:: class, cid
  logical:: err
continue
  call var_class(var, class, cid)
  select case(class)
  case (vtb_class_netcdf)
    call get_attr(GD_NC_VARIABLE(cid), attrname, value=buffer, &
      & stat=stat, default=default)
    if (stat >= 1) then
      value = buffer(1)
      return
    end if
  case (vtb_class_memory)
    call get_attr(GD_MEM_VARIABLE(cid), attrname, cbuffer, err)
    if (.not. err) then
      value = StoD(cbuffer)
      return
    endif
  case default
    call StoreError(GT_ENOTVAR, "GTVarGetAttrR")
  end select
  value = NF90_FILL_DOUBLE
  if (present(default)) value = default
end subroutine

!
! ポインタ配列を使って受け取る場合は解釈可能な数だけ実体が割り付けられる。
!

subroutine GTVarGetAttrIP(var, name, value)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: get_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_error, only: GT_ENOTVAR, StoreError
  use dc_string, only: get_array
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  integer, pointer:: value(:) !(out)
  integer:: stat, class, cid
  character(STRING):: cbuffer
  logical:: err
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    allocate(value(1))
    call get_attr(GD_NC_VARIABLE(cid), name, value(1:0), stat)
    deallocate(value)
    if (stat < 1) return
    allocate(value(stat))
    call get_attr(GD_NC_VARIABLE(cid), name, value, stat)
    if (stat < 1) deallocate(value)
  else if (class == vtb_class_memory) then
    call get_attr(GD_MEM_VARIABLE(cid), name, cbuffer, err)
    if (err) then
      nullify(value)
      return
    endif
    call get_array(value, cbuffer)
    cbuffer = ""
  else
    call StoreError(GT_ENOTVAR, "GTVarGetAttrIP")
  endif
end subroutine GTVarGetAttrIP

subroutine GTVarGetAttrRP(var, name, value)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: get_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_string, only: get_array
  use dc_error, only: GT_ENOTVAR, StoreError
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  real, pointer:: value(:) !(out)
  integer:: stat, class, cid
  character(STRING):: cbuffer
  logical:: err
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    allocate(value(1))
    call get_attr(GD_NC_VARIABLE(cid), name, value(1:0), stat)
    deallocate(value)
    if (stat < 1) return
    allocate(value(stat))
    call get_attr(GD_NC_VARIABLE(cid), name, value, stat)
    if (stat < 1) deallocate(value)
  else if (class == vtb_class_memory) then
    call get_attr(GD_MEM_VARIABLE(cid), name, cbuffer, err)
    if (err) then
      nullify(value)
      return
    endif
    call get_array(value, cbuffer)
    cbuffer = ""
  else
    nullify(value)
    call StoreError(GT_ENOTVAR, "GTVarGetAttrRP")
  endif
end subroutine GTVarGetAttrRP

subroutine GTVarGetAttrDP(var, name, value)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: get_attr
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use dc_types, only: DP
  use dc_error, only: GT_ENOTVAR, StoreError
  use dc_string, only: get_array
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  real(DP), pointer:: value(:) !(out)
  integer:: stat, class, cid
  character(STRING):: cbuffer
  logical:: err
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    allocate(value(1))
    call get_attr(GD_NC_VARIABLE(cid), name, value(1:0), stat)
    deallocate(value)
    if (stat < 1) return
    allocate(value(stat))
    call get_attr(GD_NC_VARIABLE(cid), name, value, stat)
    if (stat < 1) deallocate(value)
  else if (class == vtb_class_memory) then
    call get_attr(GD_MEM_VARIABLE(cid), name, cbuffer, err)
    if (err) then
      nullify(value)
      return
    endif
    call get_array(value, cbuffer)
    cbuffer = ""
  else
    call StoreError(GT_ENOTVAR, "GTVarGetAttrRP")
  endif
end subroutine GTVarGetAttrDP

! integer 配列, real 配列として受け取る
! 場合は属性長があまっている場合には切り捨てられ、
! 属性長が足りない場合は default 値 (ポインタと違い必須) を埋める。

subroutine GTVarGetAttrIA(var, name, value, default)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: friend => get_attr
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_error, only: GT_ENOTVAR, StoreError
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  integer, intent(out):: value(:)
  integer, intent(in):: default
  integer, pointer:: ptr(:)
  integer:: n, stat, class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call get_attr(GD_NC_VARIABLE(cid), name, value, stat, default)
  else if (class == vtb_class_memory) then
    call friend(var, name, ptr)
    if (.not. associated(ptr)) then
      value(:) = default
    else
      n = min(size(ptr), size(value))
      value(1:n) = ptr(1:n)
      if (n < size(ptr)) value(n+1: ) = default
      deallocate(ptr)
    endif
  else
    call StoreError(GT_ENOTVAR, "GTVarGetAttrIA")
  endif
end subroutine GTVarGetAttrIA

subroutine GTVarGetAttrRA(var, name, value, default)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: friend => get_attr
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_error, only: GT_ENOTVAR, StoreError
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  real, intent(out):: value(:)
  real, intent(in):: default
  real, pointer:: ptr(:)
  integer:: n, class, cid, stat
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call get_attr(GD_NC_VARIABLE(cid), name, value, stat, default)
  else if (class == vtb_class_memory) then
    call friend(var, name, ptr)
    if (.not. associated(ptr)) then
      value(:) = default
    else
      n = min(size(ptr), size(value))
      value(1:n) = ptr(1:n)
      if (n < size(ptr)) value(n+1: ) = default
      deallocate(ptr)
    endif
  else
    call StoreError(GT_ENOTVAR, "GTVarGetAttrRA")
  endif
end subroutine GTVarGetAttrRA

subroutine GTVarGetAttrDA(var, name, value, default)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: friend => get_attr
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: get_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_types, only: DP
  use dc_error, only: GT_ENOTVAR, StoreError
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  real(DP), intent(out):: value(:)
  real(DP), intent(in):: default
  real(DP), pointer:: ptr(:)
  integer:: n, stat, class, cid
continue
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    call get_attr(GD_NC_VARIABLE(cid), name, value, stat, default)
  else if (class == vtb_class_memory) then
    call friend(var, name, ptr)
    if (.not. associated(ptr)) then
      value(:) = default
    else
      n = min(size(ptr), size(value))
      value(1:n) = ptr(1:n)
      if (n < size(ptr)) value(n+1: ) = default
      deallocate(ptr)
    endif
  else
    call StoreError(GT_ENOTVAR, "GTVarGetAttrRA")
  endif
end subroutine GTVarGetAttrDA
