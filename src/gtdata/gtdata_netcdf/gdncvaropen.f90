!= netCDF 変数のオープン
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gdncvaropen.f90,v 1.2 2009-05-25 09:51:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_netcdf_generic から提供されます。
!

recursive subroutine GDNcVarOpen(var, url, writable, err)
  use dc_types, only: STRING
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_SEARCH
  use gtdata_netcdf_internal, only: vtable_add
  use gtdata_netcdf_file_generic, only: GDNcFileOpen
  use dc_url, only: UrlSplit
  use dc_error, only: StoreError
  use dc_trace, only: BeginSub, EndSub
  use netcdf, only: NF90_NOERR, NF90_MAX_NAME, NF90_ENOTVAR, NF90_EBADDIM, &
    & NF90_INQ_VARID, NF90_INQ_DIMID, NF90_INQUIRE_VARIABLE, NF90_INQUIRE
  implicit none
  type(GD_NC_VARIABLE), intent(out):: var
  character(len = *), intent(in):: url
  logical, intent(in), optional:: writable
  logical, intent(out), optional:: err
  character(len = STRING):: filename, varname
  character(len = NF90_MAX_NAME):: dimname
  integer:: stat, nvars, i
  type(GD_NC_VARIABLE_SEARCH):: e
  character(len = *), parameter:: subname = 'GDNcVarOpen'
continue
  call BeginSub(subname)
  call UrlSplit(url, file=filename, var=varname)
  if (filename == "") filename = "gtool.nc"
  call GDNcFileOpen(e%fileid, trim(filename), stat=stat, writable=writable, err=err )
  if (stat /= 0) goto 999
  !
  ! 名前から変数を探し出す
  !
  if (varname /= '') then
    e%varid = 0
    stat = NF90_INQ_VARID(e%fileid, trim(varname), e%varid)
    if (stat == NF90_ENOTVAR) then
      e%varid = 0
      stat = NF90_NOERR
    endif
  else
    ! 名前が空ならできれば次元変数でない最初の変数をとりだす
    stat = NF90_INQUIRE(e%fileid, nVariables = nvars)
    if (stat /= 0) goto 999
    e%varid = 1
    do, i = 1, nvars
      stat = NF90_INQUIRE_VARIABLE(e%fileid, i, name = dimname)
      if (stat /= NF90_NOERR) goto 999
      stat = NF90_INQ_DIMID(e%fileid, dimname, e%dimid)
      if (stat == NF90_NOERR) cycle
      if (stat /= NF90_EBADDIM) goto 999
      e%varid = i
      stat = NF90_NOERR
      exit
    enddo
  endif
  if (stat /= NF90_NOERR) goto 999
  !
  ! 次元id を調べる
  !
  stat = NF90_INQ_DIMID(e%fileid, trim(varname), e%dimid)
  if (stat /= NF90_NOERR) then
    if (e%varid <= 0) goto 999
    e%dimid = 0
  endif
  !
  stat = vtable_add(var, e)
  if (stat /= NF90_NOERR) goto 999
  call EndSub(subname, 'an=%d file=%d var=%d', i=(/var%id, e%fileid, e%varid/))
  call StoreError(stat, subname, err)
  return
  !
  ! エラー処理 (正常完了時も呼ぶ)
  !
999 continue
  var = GD_NC_VARIABLE(-1)
  call EndSub(subname, 'an=%d err', i=(/var%id/))
  call StoreError(stat, subname, err, cause_c=url)
end subroutine GDNcVarOpen
