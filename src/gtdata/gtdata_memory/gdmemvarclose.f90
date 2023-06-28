subroutine GDMemVarClose(var)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY
  use gtdata_memory_internal, only: memtab_lookup

  type(GD_MEM_VARIABLE), intent(in):: var
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent
  if (memtab_lookup(var, ent) /= 0) return
  deallocate(ent%dbuf)
  if (associated(ent%attr)) deallocate(ent%attr)
  if (associated(ent%current)) deallocate(ent%current)
  ent%name = ""
end subroutine GDMemVarClose
