subroutine GDMemVarAttrDel(var, name, err)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY, GD_MEM_ATTR_CHAIN
  use gtdata_memory_internal, only: memtab_lookup
  use dc_error, only: StoreError
  use netcdf, only: NF90_ENOTATT, NF90_NOERR
  type(GD_MEM_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  logical, intent(out), optional:: err
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent
  type(GD_MEM_ATTR_CHAIN), pointer:: p, prev
  integer:: stat
  stat = memtab_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 999
  nullify(prev)
  p => ent%attr
  do
    if (.not. associated(p)) exit
    if (p%name == name) then
      if (associated(p%cbuf)) deallocate(p%cbuf)
      prev%next => p%next
      deallocate(p)
      call StoreError(NF90_NOERR, "GDMemVarAttrDel", err)
      return
    endif
    prev => p
    p => p%next
  enddo
  stat = NF90_ENOTATT
999 continue
  call StoreError(stat, "GDMemVarAttrDel", err, cause_c=name)
end subroutine GDMemVarAttrDel
