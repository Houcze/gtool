subroutine GDMemVarAttrNext(var, name, err)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY
  use gtdata_memory_internal, only: memtab_lookup
  use netcdf, only: NF90_NOERR
  type(GD_MEM_VARIABLE), intent(in):: var
  character(len = *), intent(out):: name
  logical, intent(out), optional:: err
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent

  if (memtab_lookup(var, ent) /= NF90_NOERR) goto 999
  if (.not. associated(ent%current)) then
    ent%current => ent%attr
  else
    ent%current => ent%current%next
  endif
  if (.not. associated(ent%current)) goto 999
  name = ent%current%name
  if (present(err)) err = .false.
  return
      !
999 continue
  if (present(err)) err = .true.
end subroutine GDMemVarAttrNext
