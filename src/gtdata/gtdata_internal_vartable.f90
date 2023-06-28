!
!= gtool 変数表
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtdata_internal_vartable.f90,v 1.2 2009-05-29 15:03:49 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtdata_internal_vartable
  !
  ! このモジュールは gtool モジュールから直接には引用されないため、
  ! 相当むちゃな名前の使い方をしている。ユーザは呼んではならない。
  !
  !=== gtool 変数表
  !
  ! gtool 変数というのは実は単なるハンドルと多次元イテレータであり、
  ! ハンドルは小さな整数値である。
  ! 実体にアクセスするためには、ハンドル値をキーにしてまずマップ表を引き、
  ! そこで得られた vid をキーにして変数表を引いて、
  ! 種別と種別ごとの変数番号を得る。これらはたかだかポインタ＋オフセット
  ! 参照程度のコストである。
  ! gtool 変数は実体変数からイテレータが必要なだけ作成されるが、
  ! この変数表は実体変数につき１エントリしか作成しないので、参照数を持つ。
  ! このため、実体変数は変数に付いて参照数管理をしなくてもよくなる。

  use gtdata_netcdf_types, only: GD_NC_VARIABLE_SEARCH
  use dc_types, only: STRING
  implicit none
  private

  integer, parameter, public :: vid_invalid = -1

  integer, parameter, public :: VTB_CLASS_UNUSED = 0
  integer, parameter, public :: VTB_CLASS_MEMORY = 1
  integer, parameter, public :: VTB_CLASS_NETCDF = 2
  integer, parameter, public :: CLASSES_MAX = 2

  type VAR_TABLE_ENTRY
    integer:: class
    integer:: cid
    integer:: refcount
  end type VAR_TABLE_ENTRY

  type(VAR_TABLE_ENTRY), save, allocatable:: table(:)
  integer, parameter:: table_ini_size = 16

  type(GD_NC_VARIABLE_SEARCH), public, save:: gdnc_search

  public:: VarTableAdd, VarTableDelete, VarTableMore, VarTableLookup
  public:: vartable_dump
  public:: dimrange, ndims, query_growable
  private:: var_table_entry, table, table_ini_size
  private:: entry_cleanup

  interface dimrange
    module procedure dimrange_direct
  end interface

contains

  subroutine vartable_dump(vid)
    use dc_trace, only: DbgMessage
    use gtdata_netcdf_generic, only: toString
    use gtdata_netcdf_types, only: GD_NC_VARIABLE
    integer, intent(in):: vid
    character(10):: class
    if (.not. allocated(table)) return
    if (vid <= 0 .or. vid > size(table)) return
    select case(table(vid)%class)
    case(vtb_class_netcdf)
      class = 'netcdf'
    case(vtb_class_memory)
      class = 'memory'
    case default
      write(class, fmt="(i10)") table(vid)%class
    end select
    call DbgMessage('[vartable %d: class=%c cid=%d ref=%d]', &
      & i=(/vid, table(vid)%cid, table(vid)%refcount/), &
      & c1=trim(class))
    select case(table(vid)%class)
    case(vtb_class_netcdf)
      call DbgMessage('[%c]', c1=trim(tostring(GD_NC_VARIABLE(table(vid)%cid))))
    end select
  end subroutine vartable_dump

  subroutine entry_cleanup(vtb_entry)
    type(VAR_TABLE_ENTRY), intent(out):: vtb_entry(:)
    vtb_entry(:)%class = VTB_CLASS_UNUSED
    vtb_entry(:)%cid = -1
    vtb_entry(:)%refcount = 0
  end subroutine entry_cleanup

  subroutine VarTableAdd(vid, class, cid)
    use dc_trace, only: DbgMessage
    integer, intent(out):: vid
    integer, intent(in):: class, cid
    type(VAR_TABLE_ENTRY), allocatable:: tmp_table(:)
    integer:: n
  continue
    ! 必要ならば初期幅確保
    if (.not. allocated(table)) then
      allocate(table(table_ini_size))
      call entry_cleanup(table(:))
    endif
    ! 該当があれば参照数増加
    do, n = 1, size(table)
      if (table(n)%class == class .and. table(n)%cid == cid) then
        table(n)%refcount = table(n)%refcount + 1
        call DbgMessage('gtdata_vartable.add(class=%d cid=%d) found (ref=%d)', &
          & i=(/table(n)%class, table(n)%cid, table(n)%refcount/))
        vid = n
        return
      endif
    enddo
    ! もし空きが無ければ表を拡張
    if (all(table(:)%class /= VTB_CLASS_UNUSED)) then
      n = size(table)
      allocate(tmp_table(n))
      tmp_table(:) = table(:)
      deallocate(table)
      allocate(table(n * 2))
      table(1:n) = tmp_table(1:n)
      deallocate(tmp_table)
      table(n+1:n*2) = var_table_entry(VTB_CLASS_UNUSED, -1, 0)
    endif
    do, n = 1, size(table)
      if (table(n)%class == VTB_CLASS_UNUSED) then
        table(n)%class = class
        table(n)%cid = cid
        table(n)%refcount = 1
        vid = n
        return
      endif
    enddo
    vid = vid_invalid
  end subroutine VarTableAdd

  subroutine VarTableDelete(vid, action, err)
    integer, intent(in):: vid
    logical, intent(out):: action
    logical, intent(out), optional:: err
    if (.not. allocated(table)) goto 999
    if (vid <= 0 .or. vid > size(table)) goto 999
    if (table(vid)%class <= VTB_CLASS_UNUSED) goto 999
    if (table(vid)%class > CLASSES_MAX) goto 999
    table(vid)%refcount = max(table(vid)%refcount - 1, 0)
    action = (table(vid)%refcount == 0)
    if (present(err)) err = .false.
    return
999 continue
    action = .false.
    if (present(err)) err = .true.
  end subroutine VarTableDelete

  subroutine VarTableLookup(vid, class, cid)
    ! 同じファイル番号の変数表の中身を返す
    integer, intent(in):: vid
    integer, intent(out), optional:: class, cid
    if (.not. allocated(table)) goto 999
    if (vid <= 0 .or. vid > size(table)) goto 999
    if (table(vid)%class <= VTB_CLASS_UNUSED) goto 999
    if (table(vid)%class > CLASSES_MAX) goto 999
    if (present(class)) class = table(vid)%class
    if (present(cid)) cid = table(vid)%cid
    return
999 continue
    if (present(class)) class = VTB_CLASS_UNUSED
  end subroutine VarTableLookup

  subroutine VarTableMore(vid, err)
    ! 同じファイル番号の参照カウントを増加する。
    integer, intent(in):: vid
    logical, intent(out), optional:: err
    if (.not. allocated(table)) goto 999
    if (vid <= 0 .or. vid > size(table)) goto 999
    if (table(vid)%class <= VTB_CLASS_UNUSED) goto 999
    if (table(vid)%class > CLASSES_MAX) goto 999
    table(vid)%refcount = table(vid)%refcount + 1
    if (present(err)) err = .false.
    return
999 continue
    if (present(err)) err = .true.
  end subroutine VarTableMore

  subroutine dimrange_direct(vid, dimlo, dimhi)
    use gtdata_netcdf_types, only: GD_NC_VARIABLE
    use gtdata_netcdf_generic, only: GDNcInquire => Inquire
    use dc_error, only: storeerror, NF90_EINVAL, gt_efake
    integer, intent(in):: vid
    integer, intent(out):: dimlo, dimhi
    integer:: class, cid
    call VarTableLookup(vid, class, cid)
    select case(class)
    case(VTB_CLASS_MEMORY)
      call storeerror(gt_efake, 'gtdata::dimrange')
    case(VTB_CLASS_NETCDF)
      dimlo = 1
      call GDNcInquire(GD_NC_VARIABLE(cid), dimlen=dimhi)
    case default
      call storeerror(NF90_EINVAL, 'gtdata::dimrange')
    end select
  end subroutine dimrange_direct

  integer function ndims(vid) result(result)
    use gtdata_netcdf_types, only: GD_NC_VARIABLE
    use gtdata_netcdf_generic, only: GDNcInquire => inquire
    use dc_error, only: storeerror, NF90_EINVAL
    integer, intent(in):: vid
    integer:: class, cid
    call VarTableLookup(vid, class, cid)
    select case(class)
    case(VTB_CLASS_MEMORY)
      result = 1
    case(VTB_CLASS_NETCDF)
      call GDNcInquire(GD_NC_VARIABLE(cid), ndims=result)
    case default
      call storeerror(NF90_EINVAL, 'gtdata::ndims')
    end select
  end function ndims

  subroutine query_growable(vid, result)
    use gtdata_netcdf_types, only: GD_NC_VARIABLE
    use gtdata_netcdf_generic, only: inquire
    use dc_error, only: storeerror, NF90_EINVAL
    integer, intent(in):: vid
    logical, intent(out):: result
    integer:: class, cid
    call vartablelookup(vid, class, cid)
    select case(class)
    case(vtb_class_memory)
      result = .false.
    case(vtb_class_netcdf)
      call inquire(GD_NC_VARIABLE(cid), growable=result)
    case default
      call storeerror(NF90_EINVAL, 'gtdata::ndims')
    end select
  end subroutine query_growable

end module gtdata_internal_vartable
