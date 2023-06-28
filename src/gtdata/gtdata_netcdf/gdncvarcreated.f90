!
!= netCDF ファイルへ次元変数作成
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gdncvarcreated.f90,v 1.3 2009-10-11 07:36:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_netcdf_generic から gtdata_netcdf_generic#Create
! として提供されます。

subroutine GDNcVarCreateD(var, url, xtype, length, overwrite, err)
  !
  !== 次元変数作成
  !
  ! 変数 URL *url* に次元変数を作成します.
  ! 次元変数の長さを *length* に与えます.
  ! 返される引数 *var* には変数 ID などの情報が格納されます.
  !
  ! *overwrite* に .true. を設定すると上書き可能なモードになります.
  ! デフォルトは上書き不可です.
  ! *err* を与える場合, 次元変数生成時にエラーが生じても
  ! プログラムを終了せず, *err* に .false. が返ります.
  !
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_SEARCH
  use gtdata_netcdf_internal, only: vtable_add
  use dc_string, only: strieq
  use dc_types, only: string
  use dc_url, only: UrlSplit
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  use netcdf, only: NF90_NOERR, NF90_FLOAT, NF90_INT, NF90_DOUBLE, NF90_CHAR, &
    & NF90_DEF_VAR, NF90_DEF_DIM
  use gtdata_netcdf_file_generic, only: GDNcFileOpen, GDNcFileDefineMode
  use dc_error, only: StoreError, gt_enomem
  implicit none
  type(GD_NC_VARIABLE), intent(out):: var
  character(len = *), intent(in):: url
  character(len = *), intent(in):: xtype
  integer, intent(in):: length
  logical, intent(in), optional:: overwrite
  logical, intent(out), optional:: err
  type(GD_NC_VARIABLE_SEARCH):: ent
  character(len = string):: filename, varname, cause_c
  integer:: stat
  integer:: nc_xtype
  character(len = *), parameter:: subname = "GDNcVarCreateD"
continue
  call BeginSub(subname, 'url=<%c>, xtype=<%c>, length=<%d>', &
    & c1=trim(url), c2=trim(xtype), i=(/length/))
  cause_c = trim(url)
  !
  ! --- ファイルを用意 ---
  call UrlSplit(url, file=filename, var=varname)
  call GDNcFileOpen(ent%fileid, filename, stat=stat, writable=.TRUE., &
    & overwrite=overwrite)
  if (stat /= NF90_NOERR) goto 999
  stat = GDNcFileDefineMode(ent%fileid)
  if (stat /= NF90_NOERR) goto 999
  !
  ! --- 型の決定 ---
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
  !
  ! --- 次元変数の作成 ---
  stat = NF90_DEF_DIM(ent%fileid, trim(varname), len=length, dimid=ent%dimid)
  if (stat /= NF90_NOERR) goto 999
  stat = NF90_DEF_VAR(ent%fileid, trim(varname), &
    & xtype=nc_xtype, dimids=(/ent%dimid/), varid=ent%varid)
  if (stat /= NF90_NOERR) goto 999
  !
  stat = vtable_add(var, ent)
  if (stat /= NF90_NOERR) goto 999

999 continue
  call StoreError(stat, subname, err, cause_c=cause_c)
  if (stat /= NF90_NOERR) var = GD_NC_VARIABLE(-1)
  call EndSub(subname, 'stat=%d', i=(/stat/))
end subroutine GDNcVarCreateD
