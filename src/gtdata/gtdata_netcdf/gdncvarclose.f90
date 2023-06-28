! -*- mode: f90; coding: utf-8 -*-
! Copyright (C) GFD Dennou Club, 2000.  All rights reserved
subroutine GDNcVarClose(var, err)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_netcdf_file_generic, only: GDNcFileClose, GDNcFileSync
  use gtdata_netcdf_internal, only: vtable_delete
  use dc_error
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  logical, intent(out), optional:: err
  integer:: fileid
continue
  fileid = vtable_delete(var)
  if (fileid < 0) then
    call StoreError(fileid, 'GDNcVarClose', err)
    return
  endif
  call GDNcFileSync(fileid)
  call GDNcFileClose(fileid, err)
end subroutine
