integer function GDNcFileDataMode(fileid) result(result)
  use netcdf, only: NF90_ENDDEF, NF90_ENOTINDEFINE, NF90_NOERR
  use dc_trace, only: DbgMessage
  implicit none
  integer, intent(in):: fileid
  character(*), parameter:: subname = "GDNcFileDataMode"
continue
  call DbgMessage(subname)
  result = NF90_ENDDEF(fileid)
  if (result == NF90_ENOTINDEFINE) result = NF90_NOERR
end function GDNcFileDataMode
