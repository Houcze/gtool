! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.

! あるファイル名 urlBase に依存する変数すべてを取得するにはまず
! var_search(iter, urlbase) を呼び出してから無限ループの中で
! attr_next(iter, url, end) を呼び出す。url がひとつ
! ひとつの属性名を与える。end が真にになったとき、
! すべての属性を探索し終えたことになる。

subroutine GDNcVarSearchInit(iter, urlBase)
  use dc_types, only: string
  use gtdata_netcdf_types, only: GD_NC_VARIABLE_SEARCH
  use dc_url, only: UrlSplit
  use gtdata_netcdf_file_generic, only: GDNcFileOpen
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GD_NC_VARIABLE_SEARCH), intent(out):: iter
  character(len = *), intent(in):: urlBase
  character(len = string):: file
  logical:: err
  character(len = *), parameter:: subname = 'GDNcVarSearchInit'

  call beginsub(subname, 'urlbase=<%c>', c1=trim(urlbase))
  call UrlSplit(trim(urlbase), file=file)
  call GDNcFileOpen(iter%fileid, filename=file, writable=.FALSE., err=err)
  if (err) iter%fileid = -1
  iter%varid = 0
  iter%dimid = 0
  call endsub(subname, 'file=%d', i=(/iter%fileid/))
end subroutine GDNcVarSearchInit

subroutine GDNcVarSearchNext(iter, url, end)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE_SEARCH
  use dc_types, only: string
  use netcdf, only: NF90_MAX_NAME, NF90_NOERR, &
    & NF90_INQUIRE_VARIABLE, NF90_INQUIRE_DIMENSION, NF90_INQ_VARID
  use dc_url, only: UrlMerge
  use gtdata_netcdf_file_generic, only: GDNcFileInquire, GDNcFileClose
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GD_NC_VARIABLE_SEARCH), intent(inout):: iter
  character(len = *), intent(out):: url
  logical, intent(out):: end
  character(len = string):: filename
  character(len = NF90_MAX_NAME):: varname
  integer:: stat, varid_tmp
  character(len = *), parameter:: subname = 'GDNcVarSearchNext'
continue
  call beginsub(subname)
  if (iter%fileid <= 0) then
    end = .TRUE.
    url = ''
    call endsub(subname, "bad file %d", i=(/iter%fileid/))
    return
  endif
  if (iter%varid >= 0) then
    iter%varid = iter%varid + 1
    stat = NF90_INQUIRE_VARIABLE(iter%fileid, iter%varid, name = varname)
    if (stat == NF90_NOERR) goto 900
    iter%varid = -1
  endif
  do while (iter%dimid >= 0)
    iter%dimid = iter%dimid + 1
    ! --- 指定番号の次元がなければエラー ---
    stat = NF90_INQUIRE_DIMENSION(iter%fileid, iter%dimid, name = varname)
    if (stat /= NF90_NOERR) exit
    ! --- 指定番号の次元と同名の変数があれば却下、次番号へ ---
    stat = NF90_INQ_VARID(iter%fileid, varname, varid_tmp)
    if (stat /= NF90_NOERR) goto 900
  enddo
  end = .TRUE.
  url = ""
  call GDNcFileClose(iter%fileid)
  iter%dimid = -1
  call endsub(subname, "end file %d", i=(/iter%fileid/))
  return

900 continue
  call GDNcFileInquire(iter%fileid, name=filename)
  url = UrlMerge(file=trim(filename), var=trim(varname))
  end = .FALSE.
  call endsub(subname, "file=%d url=<%c>", &
    & i=(/iter%fileid/), c1=trim(url))
  return
end subroutine GDNcVarSearchNext
