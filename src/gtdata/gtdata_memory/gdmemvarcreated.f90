subroutine GDMemVarCreateD(var, url, length, xtype, long_name, overwrite, err)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY
  use gtdata_memory_generic, only: put_attr
  use gtdata_memory_internal, only: memtab, memtab_add

  type(GD_MEM_VARIABLE), intent(out):: var
  character(*), intent(in):: url
  integer, intent(in):: length
  character(*), intent(in), optional:: xtype, long_name
  logical, intent(in), optional:: overwrite
  logical, intent(out), optional:: err
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent
  integer:: stat
continue
  stat = memtab_add(var, url)
  if (stat /= 0) then
    if (present(err)) err = .true.
    return
  endif
  ent => memtab(var%id)
  if (present(xtype)) then
    ent%xtype = xtype
  else
    ent%xtype = "real"
  endif
  allocate(ent%dbuf(length))
  nullify(ent%attr, ent%current)
  if (present(long_name)) call put_attr(var, "long_name", long_name)
  if (present(err)) err = .false.
end subroutine GDMemVarCreateD
