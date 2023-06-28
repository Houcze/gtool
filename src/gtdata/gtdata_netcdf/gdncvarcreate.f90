!
!= netCDF ファイルへ変数作成
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gdncvarcreate.f90,v 1.3 2009-10-11 07:36:35 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_netcdf_generic から gtdata_netcdf_generic#Create
! として提供されます。

subroutine GDNcVarCreate(var, url, xtype, dims, overwrite, err)
  !
  !== 変数作成
  !
  ! 変数 URL *url* に変数を作成します.
  ! 変数が依存する次元を *dims* に与えます.
  ! 返される引数 *var* には変数 ID などの情報が格納されます.
  !
  ! *overwrite* に .true. を設定すると上書き可能なモードになります.
  ! デフォルトは上書き不可です.
  ! *err* を与える場合, 次元変数生成時にエラーが生じても
  ! プログラムを終了せず, *err* に .false. が返ります.
  !
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY, GD_NC_VARIABLE_SEARCH
  use dc_types, only: string
  use dc_string, only: strieq
  use gtdata_netcdf_internal, only: vtable_add, vtable_lookup
  use gtdata_netcdf_file_generic, only: GDNcFileOpen, GDNcFileDefineMode
  use dc_url, only: UrlSplit
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  use gtdata_netcdf_generic, only: toString ! for debug
  use netcdf, only: &
    & NF90_NOERR, NF90_FLOAT, NF90_DOUBLE, NF90_INT, NF90_CHAR, NF90_EBADDIM, NF90_DEF_VAR
  use dc_error, only: StoreError, GT_ENOMEM, GT_EOTHERFILE, &
    & GT_EDIMNODIM, GT_EDIMMULTIDIM
  implicit none
  type(GD_NC_VARIABLE), intent(out):: var
  character(len = *), intent(in):: url
  character(len = *), intent(in):: xtype
  type(GD_NC_VARIABLE), intent(in):: dims(:)
  logical, intent(in), optional:: overwrite
  logical, intent(out), optional:: err
  type(GD_NC_VARIABLE_SEARCH):: ent
  type(GD_NC_VARIABLE_ENTRY):: ent_dim
  character(len = string):: filename, varname
  integer, allocatable:: dimids(:)
  integer:: stat, nvdims, i
  integer:: nc_xtype
  logical:: clobber
  intrinsic trim
  character(len = *), parameter:: subnam = "GDNcVarCreate"
continue
  clobber = .false.
  if (present(overwrite)) clobber = overwrite
  call BeginSub(subnam)
  call DbgMessage('url=%c', c1=trim(url))
  call DbgMessage('xtype=%c', c1=trim(xtype))
  call DbgMessage('dims=(/%*d/)', i=(/dims(:)%id/), n=(/size(dims)/))
  call DbgMessage('ovwr=%y', L=(/clobber/))

  ! もし必要ならファイル作成
  call UrlSplit(url, filename, varname)
  call GDNcFileOpen(ent%fileid, filename, stat=stat, writable=.TRUE., &
    & overwrite=clobber)
  if (stat /= NF90_NOERR) goto 999

  ! 次元にまつわる準備
  nvdims = size(dims)
  allocate(dimids(max(1, nvdims)), stat=stat)
  if (stat /= 0) then
    stat = GT_ENOMEM
    goto 999
  end if
  do, i = 1, nvdims
    stat = vtable_lookup(dims(i), ent_dim)
    if (stat /= NF90_NOERR) then
      stat = NF90_EBADDIM
      goto 999
    endif
    if (ent%fileid /= ent_dim%fileid) then
      stat = GT_EOTHERFILE
      goto 999
    endif
    if (ent_dim%dimid <= 0) then
      stat = GT_EDIMMULTIDIM
      goto 999
    endif
    dimids(i) = ent_dim%dimid
  enddo
  ent%dimid = 0

  ! 変数の型の判定
  nc_xtype = NF90_FLOAT
  if (strieq(xtype, "double") .or. strieq(xtype, "DOUBLEPRECISION")) then
    nc_xtype = NF90_DOUBLE
  endif
  if (strieq(xtype, "int") .or. strieq(xtype, "INTEGER")) then
    nc_xtype = NF90_INT
  endif
  if (strieq(xtype, "char") .or. strieq(xtype, "CHARACTER")) then
    nc_xtype = NF90_CHAR
  endif

  ! 本当の変数作成操作
  stat = GDNcFileDefineMode(ent%fileid)
  if (stat /= NF90_NOERR) goto 999
  if ( nvdims == 0 ) then
    stat = NF90_DEF_VAR(ent%fileid, name = trim(varname), &
      & xtype = nc_xtype, varid=ent%varid)
  else
    stat = NF90_DEF_VAR(ent%fileid, name = trim(varname), &
      & xtype = nc_xtype, dimids = dimids, varid=ent%varid)
  end if
  if (stat /= NF90_NOERR) goto 999

  ! 登録
  stat = vtable_add(var, ent)

999 continue
  if (allocated(dimids)) deallocate(dimids)
  if (stat /= NF90_NOERR) var % id = -1
  call StoreError(stat, subnam, err, cause_c=url)
  call EndSub(subnam, 'stat=%d, var.id=%d', i=(/stat, var % id/))
end subroutine
