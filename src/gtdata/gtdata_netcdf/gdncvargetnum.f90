! -*- coding: utf-8; mode: f90 -*-
!-------------------------------------------------------------------------------------
! Copyright (c) 2000-2016 Gtool Development Group. All rights reserved.
!-------------------------------------------------------------------------------------
! ** Important**
!
! This file is generated from gdncvargetnum.erb by ERB included Ruby 2.3.1.
! Please do not edit this file directly. @see "gdncvargetnum.erb"
!-------------------------------------------------------------------------------------
!
!= Get GD_NC_VARIABLES
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gdncvargetnum.rb2f90,v 1.3 2009-07-26 08:29:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_netcdf_generic から gtdata_generic#Get
! として提供されます。
!
subroutine GDNcVarGetReal(var, start, cnt, stride, imap, siz, val, iostat)
  use gtdata_netcdf_types,    only: GD_NC_VARIABLE
  use gtdata_netcdf_internal, only: GD_NC_VARIABLE_ENTRY, vtable_lookup
  use netcdf, only:   &
    & NF90_NOERR,     &
    & NF90_EINVAL,    &
    & NF90_EINDEFINE, &
    & NF90_GET_VAR,   &
    & NF90_REDEF,     &
    & NF90_ENDDEF
  use dc_types,    only: SP
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  integer, intent(in):: start(:)
  integer, intent(in):: cnt(:)
  integer, intent(in):: stride(:)
  integer, intent(in):: imap(:)
  ! NetCDF変数と内部データ配列のメモリ内構
  ! 造間のマッピングを指定する整数ベクトル.
  ! 詳しくは NetCDF マニュアル
  ! (NF_PUT_VARM_type 等 を参照のこと)
  integer, intent(in):: siz
  real(SP), intent(out):: val(siz)
  integer, intent(out):: iostat
  integer:: nd ! var が保持する変数が依存する次元変数の数
  integer:: ipos, i
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer, allocatable:: istart(:), istride(:), iimap(:)
  character(*), parameter:: subname = 'GDNcVarGetReal'
  continue
  iostat = vtable_lookup(var, ent)
  if (iostat /= NF90_NOERR) goto 999
  ! --- nd check ---
  nd = 0
  if (associated(ent%dimids)) nd = size(ent%dimids)
  if (min(size(start), size(cnt), size(stride), size(imap)) < nd) then
    iostat = NF90_EINVAL
    goto 999
  endif
  if (nd == 0) then
    iostat = NF90_GET_VAR(ent%fileid, ent%varid, val(1), start)
    goto 999
  endif
  ! --- stride ovarwrite buffer ---
  allocate(istart(nd), istride(nd), iimap(nd))
  istart(1:nd) = start(1:nd)
  istride(1:nd) = stride(1:nd)
  iimap(1:nd) = imap(1:nd)
  ipos = 1
  ! --- do read ---
  iostat = NF90_GET_VAR(ent%fileid, ent%varid, val, istart, cnt, istride, iimap)
  if (iostat == NF90_EINDEFINE) then
    iostat = NF90_ENDDEF(ent%fileid)
    if (iostat /= NF90_NOERR) return
    iostat = NF90_GET_VAR(ent%fileid, ent%varid, val, istart, cnt, istride, iimap)
    if (iostat /= NF90_NOERR) return
    iostat = NF90_REDEF(ent%fileid)
    if (iostat /= NF90_NOERR) return
  end if
  deallocate(istart, istride, iimap)
999 continue
end subroutine GDNcVarGetReal

subroutine GDNcVarGetDouble(var, start, cnt, stride, imap, siz, val, iostat)
  use gtdata_netcdf_types,    only: GD_NC_VARIABLE
  use gtdata_netcdf_internal, only: GD_NC_VARIABLE_ENTRY, vtable_lookup
  use netcdf, only:   &
    & NF90_NOERR,     &
    & NF90_EINVAL,    &
    & NF90_EINDEFINE, &
    & NF90_GET_VAR,   &
    & NF90_REDEF,     &
    & NF90_ENDDEF
  use dc_types,    only: DP
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  integer, intent(in):: start(:)
  integer, intent(in):: cnt(:)
  integer, intent(in):: stride(:)
  integer, intent(in):: imap(:)
  ! NetCDF変数と内部データ配列のメモリ内構
  ! 造間のマッピングを指定する整数ベクトル.
  ! 詳しくは NetCDF マニュアル
  ! (NF_PUT_VARM_type 等 を参照のこと)
  integer, intent(in):: siz
  real(DP), intent(out):: val(siz)
  integer, intent(out):: iostat
  integer:: nd ! var が保持する変数が依存する次元変数の数
  integer:: ipos, i
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer, allocatable:: istart(:), istride(:), iimap(:)
  character(*), parameter:: subname = 'GDNcVarGetDouble'
  continue
  iostat = vtable_lookup(var, ent)
  if (iostat /= NF90_NOERR) goto 999
  ! --- nd check ---
  nd = 0
  if (associated(ent%dimids)) nd = size(ent%dimids)
  if (min(size(start), size(cnt), size(stride), size(imap)) < nd) then
    iostat = NF90_EINVAL
    goto 999
  endif
  if (nd == 0) then
    iostat = NF90_GET_VAR(ent%fileid, ent%varid, val(1), start)
    goto 999
  endif
  ! --- stride ovarwrite buffer ---
  allocate(istart(nd), istride(nd), iimap(nd))
  istart(1:nd) = start(1:nd)
  istride(1:nd) = stride(1:nd)
  iimap(1:nd) = imap(1:nd)
  ipos = 1
  ! --- do read ---
  iostat = NF90_GET_VAR(ent%fileid, ent%varid, val, istart, cnt, istride, iimap)
  if (iostat == NF90_EINDEFINE) then
    iostat = NF90_ENDDEF(ent%fileid)
    if (iostat /= NF90_NOERR) return
    iostat = NF90_GET_VAR(ent%fileid, ent%varid, val, istart, cnt, istride, iimap)
    if (iostat /= NF90_NOERR) return
    iostat = NF90_REDEF(ent%fileid)
    if (iostat /= NF90_NOERR) return
  end if
  deallocate(istart, istride, iimap)
999 continue
end subroutine GDNcVarGetDouble

subroutine GDNcVarGetInt(var, start, cnt, stride, imap, siz, val, iostat)
  use gtdata_netcdf_types,    only: GD_NC_VARIABLE
  use gtdata_netcdf_internal, only: GD_NC_VARIABLE_ENTRY, vtable_lookup
  use netcdf, only:   &
    & NF90_NOERR,     &
    & NF90_EINVAL,    &
    & NF90_EINDEFINE, &
    & NF90_GET_VAR,   &
    & NF90_REDEF,     &
    & NF90_ENDDEF
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  integer, intent(in):: start(:)
  integer, intent(in):: cnt(:)
  integer, intent(in):: stride(:)
  integer, intent(in):: imap(:)
  ! NetCDF変数と内部データ配列のメモリ内構
  ! 造間のマッピングを指定する整数ベクトル.
  ! 詳しくは NetCDF マニュアル
  ! (NF_PUT_VARM_type 等 を参照のこと)
  integer, intent(in):: siz
  integer, intent(out):: val(siz)
  integer, intent(out):: iostat
  integer:: nd ! var が保持する変数が依存する次元変数の数
  integer:: ipos, i
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer, allocatable:: istart(:), istride(:), iimap(:)
  character(*), parameter:: subname = 'GDNcVarGetInt'
  continue
  iostat = vtable_lookup(var, ent)
  if (iostat /= NF90_NOERR) goto 999
  ! --- nd check ---
  nd = 0
  if (associated(ent%dimids)) nd = size(ent%dimids)
  if (min(size(start), size(cnt), size(stride), size(imap)) < nd) then
    iostat = NF90_EINVAL
    goto 999
  endif
  if (nd == 0) then
    iostat = NF90_GET_VAR(ent%fileid, ent%varid, val(1), start)
    goto 999
  endif
  ! --- stride ovarwrite buffer ---
  allocate(istart(nd), istride(nd), iimap(nd))
  istart(1:nd) = start(1:nd)
  istride(1:nd) = stride(1:nd)
  iimap(1:nd) = imap(1:nd)
  ipos = 1
  ! --- do read ---
  iostat = NF90_GET_VAR(ent%fileid, ent%varid, val, istart, cnt, istride, iimap)
  if (iostat == NF90_EINDEFINE) then
    iostat = NF90_ENDDEF(ent%fileid)
    if (iostat /= NF90_NOERR) return
    iostat = NF90_GET_VAR(ent%fileid, ent%varid, val, istart, cnt, istride, iimap)
    if (iostat /= NF90_NOERR) return
    iostat = NF90_REDEF(ent%fileid)
    if (iostat /= NF90_NOERR) return
  end if
  deallocate(istart, istride, iimap)
999 continue
end subroutine GDNcVarGetInt
