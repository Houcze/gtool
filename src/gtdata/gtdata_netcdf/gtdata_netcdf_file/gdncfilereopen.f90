subroutine GDNcFileReopen(fileid, err)
  ! 同じファイル番号の参照カウントを増加する。
  use gtdata_netcdf_file_types, only: GD_NC_FILE_ID_ENTRY
  use gtdata_netcdf_file_internal, only: id_head, id_used
  use netcdf, only: NF90_ENOTNC
  use dc_error, only: StoreError
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  implicit none
  integer, intent(in):: fileid
  logical, intent(out), optional:: err
  type(GD_NC_FILE_ID_ENTRY), pointer:: identptr
  character(*), parameter:: subname = "GDNcFileReopen"
continue
  call BeginSub(subname, 'file=%d', i=(/fileid/))
  if (id_used) then
    identptr => id_head
    do
      if (identptr % id == fileid) then
        identptr % count = identptr % count + 1
        if (present(err)) err = .FALSE.
        call EndSub(subname, 'count=%d', i=(/identptr % count/))
        return
      endif
      identptr => identptr % next
      if (.not. associated(identptr)) exit
    enddo
  endif
  call StoreError(NF90_ENOTNC, 'GDNcFileReopen', err, cause_i=fileid)
  call EndSub(subname, 'err')
end subroutine GDNcFileReopen
