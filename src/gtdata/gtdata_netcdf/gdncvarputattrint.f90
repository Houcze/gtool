! Copyright (C) GFD Dennou Club, 2000.  All rights reserved

subroutine GDNcVarPutAttrInt(var, name, value, err)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_file_generic, only: GDNcFileDefineMode
  use gtdata_netcdf_internal, only: vtable_lookup
  use netcdf, only: &
    & NF90_PUT_ATT, &
    & NF90_NOERR, &
    & NF90_DEL_ATT, &
    & NF90_ENOTINDEFINE, &
    & NF90_GLOBAL
    use dc_url, only: GT_PLUS
    use dc_error
    implicit none
    type(GD_NC_VARIABLE), intent(in):: var
    character(len = *), intent(in):: name
    type(GD_NC_VARIABLE_ENTRY):: ent
    integer, intent(in):: value(:)
    logical, intent(out), optional:: err
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
    call StoreError(stat, 'GDNcVarPutAttrInt', err)
end subroutine
