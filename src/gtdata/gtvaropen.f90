!= gtvaropen.f90 - gtool4 データのオープン
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvaropen.f90,v 1.4 2009-05-25 09:55:57 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

subroutine GTVarOpen(var, url, writable, err)
  !
  !== gtool4 データのオープン
  !
  ! *url* で識別される gtool 変数を開き、*var* に格納します。
  ! *writable* を <tt>.true.</tt> に指定すると書き込み可で開こうとします。
  ! デフォルトは書き込み不可で開きます。
  ! (まだ *writable* の動作は保障されていません)。
  !
  ! Open された変数は必ず Close されなければなりません。
  !
  ! エラーが発生した場合、引数 *err* が与えられる場合は *err* が
  ! <tt>.true.</tt> となって返ります。
  ! 引数 *err* を与えなければプログラムは停止します。
  !
  ! *Open* は 2 つのサブルーチンの総称名であり、
  ! ある変数の次元を指定することで開くことも可能です。
  ! 上記のサブルーチンを参照ください。
  !
  !=== 補足
  ! 
  ! 上記の Open を参照してください。
  !
  use dc_string, only: StrHead
  use dc_types, only: STRING
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: limit
  use gtdata_internal_map, only: map_create, vtb_class_netcdf, vtb_class_memory, gtvar_dump
  use gtdata_netcdf_generic, only: Open, Inquire
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_url, only: url_chop_iorange
  use dc_error, only: StoreError, GT_EFAKE, DC_NOERR, GT_ENOTVAR
  use dc_trace, only: beginsub, endsub
  use dc_present, only: present_and_true
  implicit none
  type(GT_VARIABLE), intent(out):: var
  character(*), intent(in):: url
  logical, intent(in), optional:: writable
  logical, intent(out), optional:: err
  integer:: ndims, stat, cause_i
  character(STRING):: cause_c
  integer, allocatable:: dimlen(:)
  type(GD_NC_VARIABLE):: gdnc
  character(STRING):: filevar, iorange
  character(*),      parameter:: subname = "GTVarOpen"
  character(*),      parameter:: version = &
    & '$Name:  $' // &
    & '$Id: gtvaropen.f90,v 1.4 2009-05-25 09:55:57 morikawa Exp $'
continue
  call beginsub(subname, fmt='<%c>', c1=trim(url), version=version)
  stat = DC_NOERR
  cause_i = 0
  cause_c = ''
  var = GT_VARIABLE(-1)
  call url_chop_iorange(url, iorange=iorange, remainder=filevar)
  if (StrHead(filevar, "memory:")) then
    stat = GT_EFAKE
    cause_c = 'GTVarOpen(memory:)'
    goto 999
  else
    call Open(gdnc, filevar, writable, err)
    if ( present_and_true(err) ) then
      stat = GT_ENOTVAR
      goto 999
    end if
    call inquire(gdnc, ndims=ndims)
    allocate(dimlen(max(1, ndims)))
    call inquire(gdnc, dimlen=dimlen)
    call map_create(var, vtb_class_netcdf, gdnc%id, ndims, dimlen, stat)
    if (stat /= DC_NOERR) then
      cause_i = ndims
      goto 999
    end if
    deallocate(dimlen)
  endif
  call limit(var, trim(iorange))
  call gtvar_dump(var)
999 continue
  call StoreError(stat, subname, err, cause_c = cause_c, cause_i = cause_i)
  call endsub(subname, 'mapid=%d', i=(/var%mapid/))
end subroutine GTVarOpen
