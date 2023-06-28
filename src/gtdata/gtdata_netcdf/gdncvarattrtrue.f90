! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.


subroutine GDNcVarGetAttrLogical(var, name, value, default)
  ! 真偽値の判定基準 ... 偽の例を示す。例を lowercase にしたもの以外の値は全部真。
  !   数値 0, 0.0
  !   文字列 "0", "0.0", ".0", "0.", "0.0D0", "FALSE", ".FALSE.", "F"

  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_lookup
  use gtdata_netcdf_generic, only: get_attr, InquirePlus
  use dc_types, only: string
  use netcdf, only: NF90_MAX_NAME, NF90_NOERR, NF90_CHAR, NF90_INQUIRE_ATTRIBUTE, NF90_GET_ATT
  use dc_error
  use dc_string
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  logical, intent(out):: value
  logical, intent(in), optional:: default
  type(GD_NC_VARIABLE_ENTRY):: ent
  character(len = STRING):: cbuffer
  character(len = 7):: c_default
  character(len = NF90_MAX_NAME):: aname
  real, allocatable:: rbuf(:)
  integer:: stat, xtype, attrlen
  integer:: varid
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 999
  ! 大域属性サポート
  call InquirePlus(var, name, &
    & varid=varid, nf_attrname=aname)
  stat = NF90_INQUIRE_ATTRIBUTE(ent%fileid, varid, aname, xtype=xtype, len=attrlen)
  if (stat /= NF90_NOERR) goto 999
  if (xtype == NF90_CHAR) then
    c_default = "0"
    if (present(default)) then
      if (default) c_default = "1"
    endif
    call get_attr(var, name, cbuffer, c_default, stat)
    ! もうちょっとましな方法があるべきだが。
    select case(cbuffer)
    case("", "0", "0.0", "0.", ".0", "FALSE", "false", ".FALSE.", &
      & ".false.", "F", "f", "0.0D0", "0.0d0")
      value = .FALSE.
    case default
      value = .TRUE.
    end select
  else
    allocate(rbuf(attrlen))
    stat = NF90_GET_ATT(ent%fileid, varid, aname, rbuf)
    if (stat /= NF90_NOERR) goto 999
    value = (abs(rbuf(1)) > tiny(0.0))
  endif
  return

999 continue
  value = .FALSE.
  if (present(default)) value = default
end subroutine GDNcVarGetAttrLogical
