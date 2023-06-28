subroutine GDMemVarAttrGet(var, name, value, err)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY, GD_MEM_ATTR_CHAIN
  use gtdata_memory_internal, only: memtab_lookup
  use dc_error, only: StoreError
  use netcdf, only: NF90_ENOTATT, NF90_NOERR
  type(GD_MEM_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  character(len = *), intent(out):: value
  logical, intent(out), optional:: err
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent
  type(GD_MEM_ATTR_CHAIN), pointer:: p
  integer:: i, stat
  stat = memtab_lookup(var, ent)
  if (stat == NF90_NOERR) then
    if (associated(ent%current)) then
      p => ent%current
      if (p%name == name) goto 100
    endif
    p => ent%attr
    do
      if (.not. associated(p)) exit
      if (p%name == name) goto 100
      p => p%next
    enddo
    stat = NF90_ENOTATT
  endif
  call StoreError(stat, "GDMemVarAttrGet", err, cause_c=name)
  return

100 continue
  if (associated(p%cbuf)) then
    do, i = 1, len(value)
      value(i:i) = p%cbuf(i)
    enddo
  else
    value = ""
  endif

end subroutine GDMemVarAttrGet
