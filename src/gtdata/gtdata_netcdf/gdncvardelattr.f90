! Copyright (C) GFD Dennou Club, 2000.  All rights reserved

subroutine GDNcVarDelAttr(var, name, err)
    use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
    use gtdata_netcdf_file_generic, only: GDNcFileDefineMode
    use gtdata_netcdf_internal, only: vtable_lookup
    use netcdf, only: NF90_NOERR, NF90_DEL_ATT, NF90_GLOBAL, NF90_ENOTINDEFINE
    use dc_url, only: GT_PLUS
    use dc_error
    implicit none
    type(GD_NC_VARIABLE), intent(in):: var
    character(len = *), intent(in):: name
    logical, intent(out), optional:: err
    type(GD_NC_VARIABLE_ENTRY):: ent
    integer:: stat
continue
    stat = vtable_lookup(var, ent)
    if (stat /= NF90_NOERR) goto 999
    stat = NF90_DEL_ATT(ent%fileid, ent%varid, name=name)
    if (stat == 0) goto 999
    if (stat /= NF90_ENOTINDEFINE) goto 999
    stat = GDNcFileDefineMode(ent%fileid)
    if (stat /= NF90_NOERR) goto 999
    if (name(1:1) == GT_PLUS) then
        stat = NF90_DEL_ATT(ent%fileid, NF90_GLOBAL, name=name(2:))
    else
        stat = NF90_DEL_ATT(ent%fileid, ent%varid, name=name)
    endif

999 continue
    call StoreError(stat, 'GDNcVarPutAttrChar', err)
end subroutine
