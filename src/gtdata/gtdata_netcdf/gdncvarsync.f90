! Copyright (C) GFD Dennou Club, 2000.  All rights reserved

subroutine GDNcVarSync(var, stat)
    use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
    use gtdata_netcdf_file_generic, only: GDNcFileSync
    use gtdata_netcdf_internal, only: vtable_lookup
    use netcdf, only: NF90_NOERR
    implicit none
    type(GD_NC_VARIABLE), intent(in), optional:: var
    integer, intent(out), optional:: stat
    type(GD_NC_VARIABLE_ENTRY):: ent
    integer:: mystat
    if (.not. present(var)) then
        call GDNcFileSync(stat=stat)
        return
    endif
    mystat = vtable_lookup(var, ent)
    if (mystat /= NF90_NOERR) then
        if (present(stat)) stat = mystat
    else
        call GDNcFileSync(ent%fileid, stat=stat)
    endif
end subroutine
