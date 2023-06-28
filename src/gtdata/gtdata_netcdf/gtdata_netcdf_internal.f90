!= gtdata_netcdf 内で使用される内部向け定数, 変数, 手続き群
!= Internal constants, variables, procedures used in "gtdata_netcdf"
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_netcdf_internal.f90,v 1.1 2009-05-25 09:51:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2001-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtdata_netcdf_internal
  != gtdata_netcdf 内で使用される内部向け定数, 変数, 手続き群
  != Internal constants, variables, procedures used in "gtdata_netcdf"
  !
  ! <b>Note that Japanese and English are described in parallel.</b>

  use netcdf
  use gtdata_netcdf_types, only: &
    & GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY, GD_NC_VARIABLE_SEARCH
  use dc_error
  use dc_trace, only: DbgMessage
  implicit none
  private

  type(GD_NC_VARIABLE_ENTRY), save, target, allocatable:: gdnctab(:)
  integer, parameter:: gdnctab_init_size = 16

  public:: vtable_add, vtable_delete, vtable_lookup, GD_NC_VARIABLE_ENTRY
  public:: vtable_set_attrid

contains

  integer function vtable_add(var, entry) result(result)
    type(GD_NC_VARIABLE), intent(out):: var
    type(GD_NC_VARIABLE_SEARCH), intent(in):: entry
    type(GD_NC_VARIABLE_ENTRY), allocatable:: tmp_table(:)
    integer:: i, n

    ! --- 必要なら初期確保 ---
    if (.not. allocated(gdnctab)) then
      allocate(gdnctab(gdnctab_init_size), stat=result)
      if (result /= 0) goto 999
      do, i = 1, gdnctab_init_size
        gdnctab(i)%fileid = 0
        gdnctab(i)%varid = 0
        gdnctab(i)%dimid = 0
        gdnctab(i)%attrid = 0
        nullify(gdnctab(i)%dimids)
      enddo
    endif
    ! --- 同じ内容が既登録ならばそれを返す (attrid は変更しない) ---
    do, i = 1, size(gdnctab)
      if (gdnctab(i)%fileid == entry%fileid &
        & .and. gdnctab(i)%varid == entry%varid &
        & .and. gdnctab(i)%dimid == entry%dimid) then
        var = GD_NC_VARIABLE(i)
        result = NF90_NOERR
        call DbgMessage('gtdata_netcdf_internal.add: found %d', i=(/i/))
        return
      endif
    enddo
    !
    ! --- 空き地があればそこに割り当て ---
    var = GD_NC_VARIABLE(-1)
    do, i = 1, size(gdnctab)
      if (gdnctab(i)%fileid == 0) then
        var = GD_NC_VARIABLE(i)
        exit
      endif
    enddo
    if (var%id == -1) then
      ! --- 空き地はなかったのだから倍幅確保 ---
      n = size(gdnctab)
      allocate(tmp_table(n), stat=result)
      if (result /= 0) goto 999
      tmp_table(:) = gdnctab(:)
      deallocate(gdnctab, stat=result)
      if (result /= 0) goto 999
      allocate(gdnctab(n * 2), stat=result)
      if (result /= 0) goto 999
      gdnctab(1:n) = tmp_table(1:n)
      deallocate(tmp_table, stat=result)
      if (result /= 0) goto 999
      !
      gdnctab(n+2)%fileid = 0
      gdnctab(n+2)%varid = 0
      gdnctab(n+2)%dimid = 0
      gdnctab(n+2)%attrid = 0
      nullify(gdnctab(n+2)%dimids)
      gdnctab(n+3: n*2) = gdnctab(n+2)
      ! 確保域の先頭を使用
      var = GD_NC_VARIABLE(n + 1)
    endif
    gdnctab(var%id)%fileid = entry%fileid
    gdnctab(var%id)%varid = entry%varid
    gdnctab(var%id)%dimid = entry%dimid
    !
    ! --- 次元表の確保 ---
    call internal_build_dimids(gdnctab(var%id), result)
    if (result /= NF90_NOERR) goto 999
    !
    result = NF90_NOERR
    call DbgMessage('gtdata_netcdf_internal.add: added %d', i=(/var%id/))
    return
    !
999 continue
    var = GD_NC_VARIABLE(-1)
    result = NF90_ENOMEM
    return

  contains

    subroutine internal_build_dimids(ent, stat)
!!      use netcdf, only: &
!!        & NF90_NOERR, NF90_ENOMEM, NF90_INQUIRE_VARIABLE
      type(GD_NC_VARIABLE_ENTRY), intent(inout):: ent
      integer, intent(out):: stat
      integer:: ndims
      if (ent%varid > 0) then
        stat = NF90_INQUIRE_VARIABLE(ent%fileid, ent%varid, ndims = ndims)
        if (stat /= NF90_NOERR) return
        if ((ent%dimid > 0) .and. (ndims /= 1)) goto 100
        if (ndims == 0) then
          nullify(ent%dimids)
          stat = NF90_NOERR
          return
        endif
        allocate(ent%dimids(ndims), stat=stat)
        if (stat /= 0) then
          stat = NF90_ENOMEM
          return
        endif
        stat = NF90_INQUIRE_VARIABLE(ent%fileid, ent%varid, dimids = ent%dimids)
        if (stat /= NF90_NOERR) return
        if ((ent%dimid > 0) .and. (ent%dimids(1) /= ent%dimid)) then
          deallocate(ent%dimids)
          goto 100
        endif
      else
        allocate(ent%dimids(1), stat=stat)
        if (stat /= 0) then
          stat = NF90_ENOMEM
          return
        endif
        ent%dimids(1) = ent%dimid
      endif
      stat = NF90_NOERR
      return

100   continue
      ent%varid = 0
      allocate(ent%dimids(1))
      ent%dimids(1) = ent%dimid
    end subroutine internal_build_dimids

  end function vtable_add

  ! 成功時は fileid を、失敗時は NF_ENOTVAR を返す
  !
  integer function vtable_delete(var) result(result)
    type(GD_NC_VARIABLE), intent(in):: var
    if (.not. allocated(gdnctab)) goto 999
    if (var%id <= 0 .or. var%id > size(gdnctab)) goto 999
    if (gdnctab(var%id)%fileid == 0) goto 999
    result = gdnctab(var%id)%fileid
    gdnctab(var%id)%fileid = 0
    gdnctab(var%id)%varid = 0
    gdnctab(var%id)%dimid = 0
    gdnctab(var%id)%attrid = 0
    if (associated(gdnctab(var%id)%dimids)) &
      & deallocate(gdnctab(var%id)%dimids)
    call DbgMessage('gtdata_netcdf_internal.delete: delete %d', i=(/var%id/))
    return
    !
999 continue
    result = NF90_ENOTVAR
  end function vtable_delete

  integer function vtable_lookup(var, entry) result(result)
    type(GD_NC_VARIABLE), intent(in):: var
    type(GD_NC_VARIABLE_ENTRY), intent(out):: entry
    if (.not. allocated(gdnctab)) goto 999
    if (var%id <= 0 .or. var%id > size(gdnctab)) goto 999
    if (gdnctab(var%id)%fileid == 0) goto 999
    entry = gdnctab(var%id)
    result = NF90_NOERR
    return
    !
999 continue
    nullify(entry%dimids)
    entry%fileid = -1
    entry%varid = -1
    entry%dimid = -1
    entry%attrid = -1
    result = NF90_ENOTVAR
  end function vtable_lookup

  integer function vtable_set_attrid(var, attrid) result(result)
    type(GD_NC_VARIABLE), intent(in):: var
    integer, intent(in):: attrid
    continue
    if (.not. allocated(gdnctab)) goto 999
    if (var%id <= 0 .or. var%id > size(gdnctab)) goto 999
    if (gdnctab(var%id)%fileid == 0) goto 999
    gdnctab(var%id)%attrid = attrid
    result = NF90_NOERR
    return
    !
999 continue
    result = NF90_ENOTVAR
  end function vtable_set_attrid

end module gtdata_netcdf_internal
