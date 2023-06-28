! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.
!
! 引数は ndims 個でなければならない。
subroutine GDNcVarInquireIA(var, dimlen)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  !    use gtdata_netcdf_internal, only: GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_lookup
  use netcdf, only: NF90_NOERR, NF90_INQUIRE_DIMENSION
  type(GD_NC_VARIABLE), intent(in):: var
  integer, intent(out):: dimlen(:)
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer:: stat, i

  dimlen(:) = -1

  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) return

  if (ent%varid > 0) then
    if (.not. associated(ent%dimids)) return
    do, i = 1, min(size(dimlen), size(ent%dimids))
      stat = NF90_INQUIRE_DIMENSION(ent%fileid, ent%dimids(i), len = dimlen(i))
      if (stat /= NF90_NOERR ) exit
    enddo
  else
    stat = NF90_INQUIRE_DIMENSION(ent%fileid, ent%dimid, len = dimlen(1))
    if (stat /= NF90_NOERR ) dimlen(1) = -1
  endif
end subroutine
