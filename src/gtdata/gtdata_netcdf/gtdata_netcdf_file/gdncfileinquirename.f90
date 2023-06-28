subroutine GDNcFileInquireName(fileid, name)
  use gtdata_netcdf_file_types, only: GD_NC_FILE_ID_ENTRY
  use gtdata_netcdf_file_internal, only: id_head, id_used
  use netcdf, only: NF90_ENOTNC
  use dc_error
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  integer, intent(in):: fileid
  character(len = *), intent(out):: name
  type(GD_NC_FILE_ID_ENTRY), pointer:: identptr
  character(*), parameter:: subname = "GDNcFileName"
  continue
  call BeginSub(subname, 'fileid=%d', i=(/fileid/))
  if (.not. id_used) goto 999
  identptr => id_head
  do
    if (.not. associated(identptr)) exit
    if (identptr % id == fileid) then
      name = identptr % filename
      call EndSub(subname, 'name=<%c>', c1=trim(name))
      return
    endif
    identptr => identptr % next
  enddo
999 continue
  call StoreError(NF90_ENOTNC, subname)
  call EndSub(subname, 'err')
end subroutine GDNcFileInquireName
