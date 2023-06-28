!= gtdata_memory 内で使用される内部向け定数, 変数, 手続き群
!= Internal constants, variables, procedures used in "gtdata_memory"
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_memory_internal.f90,v 1.1 2009-05-25 09:47:27 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2001-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module gtdata_memory_internal
  != gtdata_memory 内で使用される内部向け定数, 変数, 手続き群
  != Internal constants, variables, procedures used in "gtdata_memory"
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !

  use gtdata_memory_types, only: GD_MEM_VARIABLE_ENTRY
  implicit none
  private

  public:: memtab, memtab_add, memtab_lookup

  type(GD_MEM_VARIABLE_ENTRY), allocatable, save, target:: memtab(:)
                              ! メモリー変数情報テーブル.
                              ! A table of memory variables

  interface memtab_add
    module procedure memtab_add
  end interface

  interface memtab_lookup
    module procedure memtab_lookup
  end interface

contains

  integer function memtab_add(var, name) result(stat)
    use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY
    use dc_error, only: gt_enomem
    type(GD_MEM_VARIABLE), intent(out):: var
    character(len = *), intent(in):: name
    type(GD_MEM_VARIABLE_ENTRY), allocatable:: tmptab(:)
    integer:: i, n

    if (.not. allocated(memtab)) then
      allocate(memtab(16), stat=stat)
      if (stat /= 0) then
        stat = gt_enomem
        return
      endif
      do, i = 1, size(memtab)
        memtab(i)%name = ""
        memtab(i)%xtype = ""
        nullify(memtab(i)%dbuf)
        nullify(memtab(i)%attr, memtab(i)%current)
      enddo
    endif
    do, i = 1, size(memtab)
      if (memtab(i)%name == "") then
        stat = 0
        var = GD_MEM_VARIABLE(i)
        memtab(i)%name = name
        return
      endif
    end do

    n = size(memtab)
    allocate(tmptab(n), stat=stat)
    if (stat /= 0) then
      stat = gt_enomem
      return
    endif
    tmptab(:) = memtab(:)
    deallocate(memtab)
    allocate(memtab(n * 2), stat=stat)
    if (stat /= 0) then
      stat = gt_enomem
      return
    endif
    memtab(1:n) = tmptab(1:n)
    deallocate(tmptab)
    do, i = n + 1, n * 2
      memtab(i)%name = ""
      nullify(memtab(i)%dbuf)
      nullify(memtab(i)%attr, memtab(i)%current)
    enddo

    i = n + 1
    var = GD_MEM_VARIABLE(i)
    memtab(i)%name = name
  end function memtab_add

  integer function memtab_lookup(var, ent) result(stat)
    use gtdata_memory_types, only: GD_MEM_VARIABLE, GD_MEM_VARIABLE_ENTRY
    use netcdf, only: NF90_ENOTVAR
    type(GD_MEM_VARIABLE), intent(in):: var
    type(GD_MEM_VARIABLE_ENTRY), pointer:: ent

    if (.not. allocated(memtab)) goto 999
    if (var%id <= 0 .or. var%id > size(memtab)) goto 999
    if (memtab(var%id)%name == "") goto 999
    ent => memtab(var%id)
    stat = 0
999 continue
    nullify(ent)
    stat = NF90_ENOTVAR
  end function memtab_lookup

end module gtdata_memory_internal
