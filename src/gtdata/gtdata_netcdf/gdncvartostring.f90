function GDNcVarToString(var) result(result)
  use dc_types, only: STRING
  use dc_string, only: cprintf
  use gtdata_netcdf_internal, only: GD_NC_VARIABLE_ENTRY, vtable_lookup
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  implicit none
  character(string):: result
  type(GD_NC_VARIABLE), intent(in):: var
  type(GD_NC_VARIABLE_ENTRY):: ent
continue
  if (vtable_lookup(var, ent) /= 0) then
    result = cprintf("GD_NC_VARIABLE(bad id %d)", i=(/var%id/))
  else
    result = cprintf("GD_NC_VARIABLE(file=%d, var=%d, dim=%d)", &
      & i=(/ent%fileid, ent%varid, ent%dimid/))
  endif
end function GDNcVarToString
