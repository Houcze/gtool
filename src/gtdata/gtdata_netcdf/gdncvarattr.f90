! Copyright (C) GFD Dennou Club, 2000.  All rights reserved.

! --- 属性の列挙 ---

! ある変数 var に付随した属性をすべて取得するにはまず
! attr_rewind(var) を呼び出してから無限ループの中で
! attr_next(var, name, [end]) を呼び出す。name がひとつ
! ひとつの属性名を与える。name が空文字列になったとき、
! すべての属性を探索し終えたことになる。このとき end を
! 与えていればそれが真になることでも判定できる。

! 大域属性は、その名前の先頭に "+" が付加された普通の変数属性で
! あるかのように見える。

subroutine GDNcVarAttrRewind(var)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_set_attrid
  use netcdf, only: NF90_NOERR
  use dc_trace, only: DbgMessage
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  integer:: stat
  character(len = *), parameter:: subname = 'GDNcVarAttrRewind'

  stat = vtable_set_attrid(var, 0)
  call DbgMessage("%c %d", c1=subname, i=(/stat/))
end subroutine GDNcVarAttrRewind

subroutine GDNcVarAttrNext(var, name, vend)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_set_attrid, vtable_lookup
  use netcdf, only: NF90_NOERR, NF90_MAX_NAME, NF90_INQ_ATTNAME, NF90_GLOBAL
  use dc_url, only: GT_PLUS
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(len = *), intent(out):: name
  type(GD_NC_VARIABLE_ENTRY):: ent
  logical, intent(out), optional:: vend
  character(len = NF90_MAX_NAME):: attrname
  integer:: stat
  integer:: new_attrid

  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 999

  new_attrid = ent%attrid
  ! 最初は変数属性の検索
  if (ent%attrid >= 0) then
    new_attrid = ent%attrid + 1
    stat = NF90_INQ_ATTNAME(ent%fileid, ent%varid, new_attrid, attrname)
    if (stat == NF90_NOERR) then
      name = attrname
      stat = vtable_set_attrid(var, new_attrid)
      vend = .FALSE.
      return
    end if
    new_attrid = -1
  endif

  ! 次は大域属性の検索
  stat = NF90_INQ_ATTNAME(ent%fileid, NF90_GLOBAL, -new_attrid, attrname)
  if (stat == NF90_NOERR) then
    new_attrid = new_attrid - 1
    name = GT_PLUS // attrname
    stat = vtable_set_attrid(var, new_attrid)
    vend = .FALSE.
    return
  endif

999 continue
  ! ここでは attrid の再設定はしない。次呼んでもエラーになるのが適当。
  vend = .TRUE.
  name = ""
  return
end subroutine GDNcVarAttrNext
