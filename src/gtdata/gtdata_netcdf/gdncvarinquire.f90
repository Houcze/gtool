!
!= 変数または属性に関する問い合わせ
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gdncvarinquire.f90,v 1.2 2009-05-25 09:51:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_netcdf_generic から gtdata_netcdf_generic#Inquire
! として提供されます。
!

!--
! 問い合わせは型ごとに手続をわけた。
!++

subroutine GDNcVarInquire(var, ndims, dimlen, growable, name, url, xtype)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_file_generic, only: GDNcFileInquire
  use gtdata_netcdf_internal, only: vtable_lookup
  use gtdata_netcdf_generic, only: GDNcXTypeName
  use dc_trace, only: beginsub, endsub, DbgMessage
  use netcdf, only: NF90_NOERR, NF90_MAX_NAME, &
    & NF90_INQUIRE_VARIABLE, NF90_INQUIRE_DIMENSION, NF90_INQUIRE
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  integer, intent(out), optional:: ndims
          ! 変数の次元数
  integer, intent(out), optional:: dimlen
          ! 変数が１次元である場合、次元長
  logical, intent(out), optional:: growable
          ! 変数が成長可能次元を持つか
  character(*), intent(out), optional:: name
          ! 文字型引数が短いと値の切り詰めが起こりうる。'?' のあとの変数名
  character(*), intent(out), optional:: url
          ! 変数名、少なくともファイル名を含む、なるべく長い名前
  character(*), intent(out), optional:: xtype
          ! 変数の型名

  ! 内部変数
  type(GD_NC_VARIABLE_ENTRY):: ent
  integer:: stat, length, i, i_xtype, idim_growable
  character(len = *), parameter:: subname = 'GDNcVarInquire'
  character(len = NF90_MAX_NAME):: buffer
  character(len = NF90_MAX_NAME):: fbuffer
continue
  call beginsub(subname, 'var.id=%d', i=(/var%id/))

  ! フェイルセーフ用にエラー値をまず入れる
  if (present(ndims)) ndims = -1
  if (present(dimlen)) dimlen = -1

  ! 変数実体の探索
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) then
    call endsub(subname, 'var not found')
    return
  endif

  ! 各引数が与えられている場合について値を取得する動作を

  if (present(ndims)) then
    if (associated(ent%dimids)) then
      ndims = size(ent%dimids)
    else
      ndims = 0
    endif
  endif

  if (present(dimlen)) then
    dimlen = 1
    if (ent%dimid > 0) then
      ! 実体に次元としての問い合わせが可能な場合
      stat = NF90_INQUIRE_DIMENSION(ent%fileid, ent%dimid, len = dimlen)
      if (stat /= NF90_NOERR) then
        dimlen = -1
        call endsub(subname, 'dimlen err')
        return
      endif
    else
      ! 実体が変数として問い合わせるしかない場合
      if (associated(ent%dimids)) then
        do, i = 1, size(ent%dimids)
          stat = NF90_INQUIRE_DIMENSION(ent%fileid, ent%dimids(i), len = length)
          if (stat /= NF90_NOERR) then
            dimlen = -1
            exit
          endif
          dimlen = dimlen * length
        enddo
      endif
    endif
  endif

  if (present(xtype)) then
    stat = NF90_INQUIRE_VARIABLE(ent%fileid, ent%varid, xtype=i_xtype)
    if (stat /= NF90_NOERR) i_xtype = 0
    call GDNcXTypeName(i_xtype, xtype)
  endif

  if (present(name)) then
    call local_getname(ent, buffer)
    name = buffer
  endif

  if (present(url)) then
    call local_getname(ent, buffer)
    call DbgMessage('ent%%fileid=%d', i=(/ent%fileid/))
    call GDNcFileInquire(ent%fileid, name=fbuffer)
    url = trim(fbuffer) // '?' // buffer
  endif

  if (present(growable)) then
    growable = .false.
    stat = vtable_lookup(var, ent)
    if (stat /= NF90_NOERR) return
    stat = NF90_INQUIRE(ent%fileid, unlimitedDimID = idim_growable)
    if (stat /= NF90_NOERR) return

    if (ent%varid > 0) then
      if (.not. associated(ent%dimids)) return
      do, i = 1, size(ent%dimids)
        if (ent%dimids(i) == idim_growable) growable = .true.
      enddo
    else
      growable = (ent%dimid == idim_growable)
    endif
  endif

  ! 安全に終った
  call endsub(subname, 'ok')
  return

contains

  subroutine local_getname(ent, varname)
    use netcdf, only: &
      & NF90_INQUIRE_DIMENSION, NF90_INQUIRE_VARIABLE, NF90_NOERR
    type(GD_NC_VARIABLE_ENTRY), intent(in):: ent
    character(len = *), intent(out):: varname
    if (ent%dimid > 0) then
      stat = NF90_INQUIRE_DIMENSION(ent%fileid, ent%dimid, name = varname)
    else
      stat = NF90_INQUIRE_VARIABLE(ent%fileid, ent%varid, name = varname)
    endif
    if (stat /= NF90_NOERR) varname = ""
  end subroutine local_getname

end subroutine GDNcVarInquire
