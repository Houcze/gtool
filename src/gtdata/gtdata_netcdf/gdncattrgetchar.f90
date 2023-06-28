! Copyright (C) GFD Dennou Club, 2000.  All rights reserved

subroutine GDNcAttrGetChar(var, name, value, default, stat)
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY
  use gtdata_netcdf_internal, only: vtable_lookup
  use netcdf, only: NF90_NOERR, NF90_GLOBAL, NF90_INQUIRE_ATTRIBUTE, NF90_CHAR, NF90_GET_ATT, &
    & NF90_DOUBLE, NF90_FLOAT
  use dc_url, only: GT_PLUS
  use dc_string, only: toChar
  use dc_trace, only: beginsub, endsub
  use dc_error
  implicit none
  type(GD_NC_VARIABLE), intent(in):: var
  character(len = *), intent(in):: name
  character(len = *), intent(out):: value
  character(len = *), intent(in):: default
  integer, intent(out):: stat
  type(GD_NC_VARIABLE_ENTRY):: ent
  character(len = 64):: buffer
  double precision, allocatable:: dbuf(:)
  integer, allocatable:: ibuf(:)
  character, allocatable:: cbuf(:)
  integer:: xtype, attrlen, i, iname, varid
  character(len = *), parameter:: subname = "GDNcAttrGetChar"
  continue
  call beginsub(subname, "var=%d name=%c default=%c", i=(/var%id/), &
    & c1=trim(name), c2=trim(default))
  stat = vtable_lookup(var, ent)
  if (stat /= NF90_NOERR) goto 900
  if (name(1:1) == GT_PLUS) then
    varid = NF90_GLOBAL
    iname = 2
  else
    varid = ent%varid
    iname = 1
  endif
  stat = NF90_INQUIRE_ATTRIBUTE(ent%fileid, varid, name(iname:), xtype=xtype, len=attrlen)
  if (stat /= NF90_NOERR) goto 900
  if (xtype == NF90_CHAR .and. attrlen <= len(buffer)) then
    stat = NF90_GET_ATT(ent%fileid, varid, name(iname:), buffer)
    if (stat /= NF90_NOERR) goto 900
    value = buffer(1: attrlen)
    if (attrlen > len(value)) stat = GT_ECHARSHORT
  else if (xtype == NF90_CHAR) then
    ! UNIDATA NetCDF ライブラリでは文字列引数の長さを
    ! まったく取得していないので先頭が結合していれば OK のはず
    allocate(cbuf(attrlen))
    stat = NF90_GET_ATT(ent%fileid, varid, name(iname:), cbuf(1))
    if (stat /= NF90_NOERR) goto 900
    do, i = 1, attrlen
      value(i:i) = cbuf(i)
    enddo
    if (attrlen < len(value)) value(attrlen + 1: ) = ' '
    if (attrlen > len(value)) stat = GT_ECHARSHORT
    deallocate(cbuf)
  else if (xtype == NF90_DOUBLE .or. xtype == NF90_FLOAT) then
    allocate(dbuf(attrlen))
    stat = NF90_GET_ATT(ent%fileid, varid, name(iname:), dbuf)
    if (stat /= NF90_NOERR) goto 900
    value = toChar(dbuf)
    deallocate(dbuf)
  else
    allocate(ibuf(attrlen))
    stat = NF90_GET_ATT(ent%fileid, varid, name(iname:), ibuf)
    if (stat /= NF90_NOERR) goto 900
    value = toChar(ibuf)
    deallocate(ibuf)
  endif
  call endsub(subname)
  return
  ! デフォルト処理
900 continue
  value = default
  call endsub(subname, "value := default")
  return
end subroutine GDNcAttrGetChar
