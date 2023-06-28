subroutine GDNcFileClose(fileid, err)
  ! おなじ id のファイルの参照カウンタを減算し、ゼロになったら閉じる
  use gtdata_netcdf_file_types, only: GD_NC_FILE_ID_ENTRY
  use gtdata_netcdf_file_internal, only: id_head, id_used
  use netcdf, only: NF90_CLOSE, NF90_ENOTNC, NF90_NOERR
  use dc_error, only: StoreError
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  integer, intent(in):: fileid
  logical, intent(out), optional:: err
  type(GD_NC_FILE_ID_ENTRY), pointer:: identptr, prev
  integer:: stat
  character(*), parameter:: subname = "GDNcFileClose"
continue
  call BeginSub(subname)
  stat = NF90_ENOTNC
  if (.not. id_used) goto 999
  identptr => id_head
  nullify(prev)
  do
    if (.not. associated(identptr)) goto 999
    if (identptr % id == fileid) exit
    prev => identptr
    identptr => identptr % next
  enddo
  identptr % count = identptr % count - 1
  if (identptr % count <= 0) then
    stat = NF90_CLOSE(fileid)
    if (associated(prev)) then
      prev%next => identptr % next
    else
      id_head => identptr % next
      if (.not. associated(id_head)) id_used = .FALSE.
    endif
    call DbgMessage(subname // ': <%c> closed', c1=trim(identptr % filename))
    deallocate(identptr)
  else
    call DbgMessage(subname // ': %d<%c> skipped for refcount=%d', &
      & c1=trim(identptr % filename), i=(/fileid, identptr % count/))
    stat = NF90_NOERR
  endif
999 continue
  call EndSub(subname)
  call StoreError(stat, 'GDNcFileClose', err)
end subroutine GDNcFileClose
