!
!= 属性のコピー
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarcopyattr.f90,v 1.4 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Copy_Attr
! として提供されます。

subroutine GTVarCopyAttr(to, attrname, from, err)
  !
  !== 属性のコピー
  !
  ! 変数 *from* の属性 *attrname* を 変数 *to* へコピーします。
  !
  ! 属性のコピー時にエラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  ! *Copy_Attr* は 2 つのサブルーチンの総称名であり、
  ! 他にも一括で変数の全ての属性をコピーする方法もあります。
  ! 下記のサブルーチンを参照ください。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: GET_ATTR, PUT_ATTR
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: copy_attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_trace, only: beginsub, endsub
  use dc_error, only: StoreError, DC_NOERR
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(inout):: to
  character(len = *), intent(in):: attrname
  type(GT_VARIABLE), intent(in):: from
  logical, intent(out), optional:: err
  character(STRING):: svalue
  integer:: from_class, from_cid, to_class, to_cid, stat
continue
  if (present(err)) err = .false.
  call beginsub('gtvarcopyattr', 'mapid to=%d from=%d name=%c', &
    & i=(/to%mapid, from%mapid/), c1=attrname)
  call var_class(from, from_class, from_cid)
  call var_class(to, to_class, to_cid)
  if (from_class == vtb_class_netcdf .and. to_class == vtb_class_netcdf) then
    call Copy_Attr(GD_NC_VARIABLE(to_cid), attrname, &
      & GD_NC_VARIABLE(from_cid), stat)
  else
    ! とりあえず文字列で入出力しておく
    call Get_Attr(from, attrname, svalue, default='')
    call Put_Attr(to, attrname, svalue, '', err)
    stat = DC_NOERR
  endif
  call StoreError(stat, "GTVarCopyAttr", err)
  call endsub('gtvarcopyattr', 'stat = %d', i=(/stat/))
end subroutine GTVarCopyAttr
