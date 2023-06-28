!
!= 属性の付加
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gdncvarputattrchar.f90,v 1.2 2009-05-25 09:51:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2007. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_netcdf_generic から gtdata_netcdf_generic#Put_Attr
! として提供されます。

subroutine GDNcVarPutAttrChar(var, name, val, xtype, err)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_lookup
  use gtdata_netcdf_file_generic, only: GDNcFileDefineMode
  use netcdf, only: &
    & NF90_GLOBAL, &
    & NF90_NOERR, &
    & NF90_PUT_ATT, &
    & NF90_DEL_ATT
  use dc_url, only: GT_PLUS
  use dc_error
  use dc_string, only: get_array
  use gtdata_netcdf_generic, only: put_attr
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  character(len = *), intent(in):: val
  character(len = *), intent(in), optional:: xtype
  logical, intent(out), optional:: err
  integer, pointer:: ip(:)
  real, pointer:: rp(:)
  double precision, pointer:: dp(:)
  integer:: stat
  type(GD_NC_VARIABLE_ENTRY):: ent
continue
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 999
  if (len(val) == 0) then
    if (name(1:1) == GT_PLUS) then
      stat = NF90_DEL_ATT(ent%fileid, NF90_GLOBAL, name = name(2:))
    else
      stat = NF90_DEL_ATT(ent%fileid, ent%varid, name = name)
    endif
    goto 999
  endif
  if ( present(xtype) ) then
    select case(xtype)
    case("INTEGER", "integer", "int")
      goto 200
    case("REAL", "real", "float")
      goto 300
    case("DOUBLEPRECISION", "DOUBLE", "double")
      goto 400
    end select
  end if

  stat = GDNcFileDefineMode( ent % fileid )
  if (stat /= NF90_NOERR) goto 999
  if (name(1:1) == GT_PLUS) then
    stat = NF90_PUT_ATT(ent%fileid, NF90_GLOBAL, name(2:), trim(val) )
  else
    stat = NF90_PUT_ATT(ent%fileid, ent%varid, name, trim(val) )
  endif

999 continue
  call StoreError(stat, 'GDNcVarPutAttrChar', err, cause_c=name)
  return

200 continue
  call get_array(ip, val)
  if (associated(ip)) then
    call put_attr(var, name, ip, err)
    deallocate(ip)
  endif
  return

300 continue
  call get_array(rp, val)
  if (associated(rp)) then
    call put_attr(var, name, rp, err)
    deallocate(rp)
  endif
  return

400 continue
  call get_array(dp, val)
  if (associated(dp)) then
    call put_attr(var, name, dp, err)
    deallocate(dp)
  endif
  return
end subroutine GDNcVarPutAttrChar
