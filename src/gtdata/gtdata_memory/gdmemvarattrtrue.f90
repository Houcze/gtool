logical function GDMemVarAttrTrue(var, name, default) result(result)
  use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY, GD_MEM_ATTR_CHAIN
  use gtdata_memory_internal, only: memtab_lookup
  use dc_string, only: str_to_logical
  use netcdf, only: NF90_NOERR
  type(GD_MEM_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  logical, intent(in), optional:: default
  type(GD_MEM_VARIABLE_ENTRY), pointer:: ent
  type(GD_MEM_ATTR_CHAIN), pointer:: p
  character(10):: s
  integer:: stat, i

  stat = memtab_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 999
  p => ent%attr
  do
    if (.not. associated(p)) exit
    if (p%name == name) then
      if (associated(p%cbuf)) then
        s = ""
        do, i = 1, min(len(s), size(p%cbuf))
          s(i:i) = p%cbuf(i)
        enddo
        result = str_to_logical(s)
      else
        exit
      endif
      return
    endif
    p => p%next
  enddo
999 continue
  result = .false.
  if (present(default)) result = default
  return
end function GDMemVarAttrTrue
