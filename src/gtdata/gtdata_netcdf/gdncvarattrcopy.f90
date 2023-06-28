! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.

subroutine GDNcVarAttrCopy(to, attrname, from, stat)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_file_generic, only: GDNcFileDefineMode
  use gtdata_netcdf_generic, only: InquirePlus
  use gtdata_netcdf_internal, only: vtable_lookup
  use netcdf, only: NF90_NOERR, NF90_MAX_NAME, NF90_COPY_ATT
  use dc_url, only: GT_PLUS
  use dc_error
  implicit none
  type(GD_NC_VARIABLE), intent(in):: to
  character(len = *), intent(in):: attrname
  type(GD_NC_VARIABLE), intent(in):: from
  integer, intent(out):: stat
  character(NF90_MAX_NAME):: nc_aname
  integer:: id_var_from, id_var_to
  logical:: myerr
  type(GD_NC_VARIABLE_ENTRY):: to_ent, from_ent
  stat = vtable_lookup(to, to_ent)
  if (stat /= NF90_NOERR) goto 999
  stat = vtable_lookup(from, from_ent)
  if (stat /= NF90_NOERR) goto 999
  stat = GDNcFileDefineMode(to_ent%fileid)
  if (stat /= NF90_NOERR) goto 999
  call InquirePlus(from, attrname, id_var_from, nc_aname)
  call InquirePlus(to, attrname, id_var_to, nc_aname)
  stat = NF90_COPY_ATT(from_ent%fileid, id_var_from, nc_aname, to_ent%fileid, id_var_to)
999 continue
  call StoreError(stat, "GDNcVarAttrCopy", myerr)
  return
end subroutine GDNcVarAttrCopy
