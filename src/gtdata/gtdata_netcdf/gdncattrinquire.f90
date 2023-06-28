! -*- mode: f90; coding: utf-8 -*-
! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.

subroutine GDNcAttrInquire(var, attrname, xtype)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_lookup
  use gtdata_netcdf_generic, only: GDNcXTypeName, InquirePlus
  use netcdf, only: NF90_MAX_NAME, NF90_NOERR, NF90_INQUIRE_ATTRIBUTE
  use dc_url, only: GT_PLUS
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(len=*), intent(in):: attrname
  character(len=*), intent(out), optional:: xtype
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer:: varid, i_xtype, stat
  character(len=NF90_MAX_NAME):: anam
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) return
  if (present(xtype)) then
    call InquirePlus(var, attrname, varid, anam)
    stat = NF90_INQUIRE_ATTRIBUTE(ent%fileid, varid, anam, xtype=i_xtype)
    if (stat /= NF90_NOERR) i_xtype = 0
    call GDNcXTypeName(i_xtype, xtype)
  endif
end subroutine

subroutine GDNcAttrInquirePlus(var, attrname, varid, nf_attrname)
  !
  ! 実際にアクセスするときに使う varid, attrname を得る.
  ! attrname の先頭が '+' ---> 大域属性を強制指示.
  ! attrname の先頭が '-' ---> 変数属性、大域属性の順で検索.
  !
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_lookup
  use dc_url, only: GT_PLUS
  use netcdf, only: NF90_GLOBAL, NF90_NOERR, NF90_INQUIRE_ATTRIBUTE
  type(GD_NC_VARIABLE), intent(in):: var
  character(len=*), intent(in):: attrname
  integer, intent(out):: varid
  type(GD_NC_VARIABLE_ENTRY):: ent
  character(len=*), intent(out):: nf_attrname
  integer:: stat, n
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) return
  if (attrname(1:1) == GT_PLUS) then
    varid = NF90_GLOBAL
    nf_attrname = attrname(2: )
  else if (attrname(1:1) == '-') then
    varid = ent%varid
    nf_attrname = attrname(2: )
    stat = NF90_INQUIRE_ATTRIBUTE(ent%fileid, varid, nf_attrname, len = n)
    if (stat == NF90_NOERR) return
    varid = NF90_GLOBAL
  else
    varid = ent%varid
    nf_attrname = attrname
  endif
end subroutine GDNcAttrInquirePlus
