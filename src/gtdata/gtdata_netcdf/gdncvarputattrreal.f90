! Copyright (C) GFD Dennou Club, 2000.  All rights reserved

subroutine GDNcVarPutAttrReal(var, name, value, err)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_file_generic, only: GDNcFileDefineMode
  use gtdata_netcdf_internal, only: vtable_lookup
  use netcdf, only: &
    & NF90_GLOBAL, &
    & NF90_NOERR, &
    & NF90_PUT_ATT, &
    & NF90_DEL_ATT
  use dc_url, only: GT_PLUS
  use dc_error
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  real, intent(in):: value(:)
  logical, intent(out), optional:: err
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer:: stat
  continue
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 999
  if (size(value) == 0) then
    if (name(1:1) == GT_PLUS) then
      stat = NF90_DEL_ATT(ent%fileid, NF90_GLOBAL, name(2:))
    else
      stat = NF90_DEL_ATT(ent%fileid, ent%varid, name)
    endif
    goto 999
  endif
  stat = GDNcFileDefineMode(ent%fileid)
  if (stat /= NF90_NOERR) goto 999
  if (name(1:1) == GT_PLUS) then
    stat = NF90_PUT_ATT(ent%fileid, NF90_GLOBAL, name(2:), value)
  else
    stat = NF90_PUT_ATT(ent%fileid, ent%varid, name, value)
  endif
999 continue
  call StoreError(stat, 'GDNcVarPutAttrReal', err)
end subroutine GDNcVarPutAttrReal

subroutine GDNcVarPutAttrDouble(var, name, value, err)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_lookup
  use gtdata_netcdf_file_generic, only: GDNcFileDefineMode
  use netcdf, only: &
    & NF90_GLOBAL, &
    & NF90_NOERR, &
    & NF90_PUT_ATT, &
    & NF90_DEL_ATT
  use dc_url, only: GT_PLUS
  use dc_error
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  double precision, intent(in):: value(:)
  logical, intent(out), optional:: err
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer:: stat
  continue
  stat = vtable_lookup(var, ent)
  if (stat /= 0) goto 999
  if (size(value) == 0) then
    if (name(1:1) == GT_PLUS) then
      stat = NF90_DEL_ATT(ent%fileid, NF90_GLOBAL, name(2:))
    else
      stat = NF90_DEL_ATT(ent%fileid, ent%varid, name)
    endif
    goto 999
  endif
  stat = GDNcFileDefineMode(ent%fileid)
  if (stat /= NF90_NOERR) goto 999
  if (name(1:1) == GT_PLUS) then
    stat = NF90_PUT_ATT(ent%fileid, NF90_GLOBAL, name(2:), value)
  else
    stat = NF90_PUT_ATT(ent%fileid, ent%varid, name, value)
  endif
999 continue
  call StoreError(stat, 'GDNcVarPutAttrDouble', err)
end subroutine
