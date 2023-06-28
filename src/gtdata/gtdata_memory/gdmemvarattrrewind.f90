subroutine GDMemVarAttrRewind(var)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY
  use gtdata_memory_internal, only: memtab_lookup
  use netcdf, only: NF90_NOERR
  type(GD_MEM_VARIABLE), intent(in):: var
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent

  if (memtab_lookup(var, ent) /= NF90_NOERR) return
  nullify(ent%current)
end subroutine GDMemVarAttrRewind
