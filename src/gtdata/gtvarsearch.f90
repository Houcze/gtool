!
!= 変数探査
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvarsearch.f90,v 1.4 2009-05-29 15:03:49 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#GTVarSearch
! として提供されます。
!

subroutine GTVarSearchNext(url, end)
  !
  !== 変数探査サブルーチン
  !
  ! あるファイル名 urlBase に依存する変数すべてを取得するには、
  ! まず GTVarSearch(urlBase) (下記のサブルーチン) を呼び出し、
  ! その後無限ループの中で GTVarSearch(url, end) を呼び出します。
  ! そうすることで url に1つ1つの変数名が返ります。
  ! *end* が真になったとき、すべての変数名を探索し終えたことになります。
  !
  !=== 例
  !
  !    use gtool5
  !    character(len = STRING) :: filename, varname
  !    logical                 :: end
  !
  !    write(*,*) "Enter file name: "
  !    read(*,*) filename
  !
  !    call GTVarSearch(filename)
  !    do
  !      call GTVarSearch(varname, end)
  !      if (end) exit
  !      write(*, *) trim(varname)
  !    enddo
  !
  use gtdata_netcdf_generic, only: var_search
  use gtdata_internal_vartable, only: gdnc_search
  use dc_trace, only: beginsub, endsub
  implicit none
  character(len = *), intent(out):: url
  logical, intent(out):: end
continue
  call beginsub('gtvarsearchnext')
  call var_search(gdnc_search, &    ! (inout)
    &             url=url, end=end) ! (out)
  call endsub('gtvarsearchnext', 'url=%c end=%y', c1=trim(url), L=(/end/))
end subroutine GTVarSearchNext

subroutine GTVarSearchInit(urlBase)
  !
  !== 変数探査初期化サブルーチン
  !
  ! 上記の GTVarSearch を参照してください。
  !
  use gtdata_netcdf_generic, only: var_search
  use gtdata_internal_vartable, only: gdnc_search
  use dc_trace, only: beginsub, endsub
  implicit none
  character(len = *), intent(in):: urlBase
continue
  call beginsub('gtvarsearchinit', 'urlbase=<%c>', c1=trim(urlbase))
  call var_search(gdnc_search, &      ! (out)
    &             urlBase = urlBase ) ! (in)
  call endsub('gtvarsearchinit')
end subroutine GTVarSearchInit
