integer function GDNcFileDefineMode(fileid) result(result)
  use netcdf, only: NF90_REDEF, NF90_EINDEFINE, NF90_NOERR
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  integer, intent(in):: fileid
  character(*), parameter:: subname = "GDNcFileDefineMode"
continue
  call DbgMessage(subname // ' %d', i=(/fileid/))
  result = NF90_REDEF(fileid)
  if (result == NF90_EINDEFINE) result = NF90_NOERR
end function GDNcFileDefineMode
