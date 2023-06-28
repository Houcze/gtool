!
! 次元変数 ID の取得
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gdncsearchdim.f90,v 1.2 2009-05-25 09:51:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2001-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
integer function GDNcSearchDim(var, dimname) result(result)
  !
  ! 次元変数 ID の取得
  !
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY, GD_NC_VARIABLE_SEARCH
  use gtdata_netcdf_internal, only: vtable_lookup
  use gtdata_netcdf_file_generic
  use netcdf, only: NF90_EBADDIM, NF90_NOERR, NF90_INQ_DIMID
  use dc_error
  use dc_trace, only: beginsub, endsub
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(*), intent(in):: dimname
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer:: stat, dimid, i
  character(*), parameter:: subname = 'GDNcSearchDim'
continue
  call beginsub(subname, 'var=%d dimname=%c', i=(/var%id/), c1=trim(dimname))
  result = NF90_EBADDIM
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 999

  stat = NF90_INQ_DIMID(ent%fileid, dimname, dimid)
  if (stat /= NF90_NOERR) goto 999

  if (dimid == ent%dimid) then
    result = 1
    goto 999
  endif

  if (.not. associated(ent%dimids)) then
    stat = gt_enomoredims
    goto 999
  endif
  do, i = 1, size(ent%dimids)
    if (ent%dimids(i) == dimid) then
      result = i
      goto 999
    endif
  enddo

999 continue
  call endsub(subname, 'result=%d', i=(/result/))
  return
end function GDNcSearchDim
