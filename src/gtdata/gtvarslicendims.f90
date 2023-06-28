subroutine GTVarSliceNDims(var, ndims, shape, err)
    use gtdata_types, only: GT_VARIABLE
    use gtdata_generic, only: Inquire, Get_Slice
    implicit none
    type(GT_VARIABLE), intent(inout):: var
    integer, intent(in):: ndims
    integer, intent(in), optional:: shape(:)
    logical, intent(out), optional:: err
    integer:: nd
    integer, allocatable:: vcount(:)
    call Inquire(var, alldims=nd)
    allocate(vcount(nd))
    call Get_Slice(var, count=vcount(:))
    if (present(err)) err = .true.
    stop 'gtvarslicendims: not implemented'
end subroutine
