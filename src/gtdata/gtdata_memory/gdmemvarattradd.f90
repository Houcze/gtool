subroutine GDMemVarAttrAdd(var, attrname, attrval)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY, GD_MEM_ATTR_CHAIN
  use gtdata_memory_internal, only: memtab_lookup
  use netcdf, only: NF90_NOERR, NF90_ENOTATT
  type(GD_MEM_VARIABLE), intent(in):: var
  character(*), intent(in):: attrname
  character(*), intent(in):: attrval
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent
  type(GD_MEM_ATTR_CHAIN), pointer:: p
  integer:: i, stat

  stat = memtab_lookup(var, ent)
  if (stat == NF90_NOERR) then
    if (associated(ent%current)) then
      if (ent%current%name == attrname) then
        p => ent%current
        goto 100
      endif
    endif
    p => ent%attr
    do
      if (.not. associated(p)) exit
      if (p%name == attrname) goto 100
      p => p%next
    enddo
    stat = NF90_ENOTATT
  endif
  allocate(p)
  nullify(p%next)
  goto 120

100 continue
  if (associated(p%cbuf)) then
    deallocate(p%cbuf)
  endif

120 continue
  allocate(p%cbuf(len(attrval)))
  do, i = 1, len(attrval)
    p%cbuf(i) = attrval(i:i)
  enddo
  return
end subroutine GDMemVarAttrAdd
