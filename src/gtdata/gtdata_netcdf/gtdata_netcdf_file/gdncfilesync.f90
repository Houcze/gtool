subroutine GDNcFileSync(fileid, stat)
  use gtdata_netcdf_file_types, only: GD_NC_FILE_ID_ENTRY
  use gtdata_netcdf_file_internal, only: id_head, id_used
  use gtdata_netcdf_file_generic, only: GDNcFileDataMode
  use netcdf, only: NF90_SYNC, NF90_NOERR
  use dc_error
  integer, intent(in), optional:: fileid
  integer, intent(out), optional:: stat
  integer:: ncid, mystat
  type(GD_NC_FILE_ID_ENTRY), pointer:: identptr
continue
  mystat = NF90_NOERR
  if (present(fileid)) then
    ncid = fileid
    mystat = GDNcFileDataMode(ncid)
    if (mystat /= NF90_NOERR) goto 999
    mystat = NF90_SYNC(ncid)
  else if (id_used) then
    identptr => id_head
    do
      if (.not. associated(identptr)) exit
      ncid = identptr % id
      mystat = GDNcFileDataMode(ncid)
      if (mystat /= NF90_NOERR) exit
      mystat = NF90_SYNC(ncid)
      if (mystat /= NF90_NOERR) exit
      identptr => identptr % next
    enddo
  endif
999 continue
  ! 自発的には StoreError しない。StoreError の SysdepAbort
  ! からも呼ばれる可能性があるためである。
  if (present(stat)) stat = mystat
end subroutine GDNcFileSync
