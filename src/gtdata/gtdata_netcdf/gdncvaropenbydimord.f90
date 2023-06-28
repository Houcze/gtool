!== Open GD_NC_VARIABLE of dimension by dimord
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gdncvaropenbydimord.f90,v 1.2 2009-05-25 09:51:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

subroutine GDNcVarOpenByDimOrd(var, src_var, dimord, err)
  !
  ! 既に開かれた変数 *src_var* の *dimord* 番目の次元にあたる変数を
  ! 開き *var* に格納します。
  !
  ! 変数を開く際にエラーが生じた場合、メッセージを出力してプログラムは
  ! 強制終了します。*err* を与えてある場合にはこの引数に .true.
  ! が返り、プログラムは終了しません。
  !
  use gtdata_netcdf_types, only: GD_NC_VARIABLE, GD_NC_VARIABLE_ENTRY, GD_NC_VARIABLE_SEARCH
  use gtdata_netcdf_internal, only: vtable_lookup, vtable_add
  use gtdata_netcdf_file_generic, only: GDNcFileReopen, GDNcFileClose
  use netcdf, only: NF90_MAX_NAME, NF90_NOERR, NF90_EINVAL, NF90_ENOTVAR, &
    & NF90_INQUIRE_DIMENSION, NF90_INQ_VARID
  use dc_error
  implicit none
  type(GD_NC_VARIABLE), intent(out):: var
  type(GD_NC_VARIABLE), intent(in):: src_var
  integer, intent(in):: dimord
  logical, intent(out), optional:: err
  type(GD_NC_VARIABLE_ENTRY):: src_ent
  type(GD_NC_VARIABLE_SEARCH):: ent
  character(len = NF90_MAX_NAME):: dimname
  integer:: stat
continue
  stat = vtable_lookup(src_var, src_ent)
  if (stat /= NF90_NOERR) goto 999
  !
  if (dimord <= 0) then
    !
    ! dimord == 0 の特別扱いは廃止 (gtdata 層でやる)。
    ! (同じハンドルを返すと参照カウントが増えないので多重クローズが
    ! 禁止されてしまい、Open のセマンティクスに不適合なため。)
    !
    var = src_var
    stat = NF90_EINVAL
    goto 999
  endif
  if (.not. associated(src_ent%dimids)) then
    stat = GT_ENOMOREDIMS
    goto 999
  else if (dimord > size(src_ent%dimids)) then
    stat = GT_ENOMOREDIMS
    goto 999
  endif
  !
  ! 決定された次元 id を使って変数 id の確定を試みる
  !
  ent%fileid = src_ent%fileid
  ent%dimid = src_ent%dimids(dimord)
  stat = NF90_INQUIRE_DIMENSION(ent%fileid, ent%dimid, name = dimname)
  if (stat /= NF90_NOERR) goto 999
  stat = NF90_INQ_VARID(ent%fileid, dimname, ent%varid)
  if (stat == NF90_ENOTVAR) then
    ! 変数が対応しなければ無視するだけ
    ent%varid = 0
    stat = NF90_NOERR
  else if (stat /= NF90_NOERR) then
    goto 999
  endif
  !
  ! 成功しそうなので参照カウント値を増やす
  call GDNcFileReopen(ent%fileid)
  ! 不適合な変数が開かれている場合は vtable_add が変数部を無視する。
  stat = vtable_add(var, ent)
  if (stat /= NF90_NOERR) then
    call GDNcFileClose(ent%fileid)
    goto 999
  endif

999 continue
  call StoreError(stat, 'GDNcVarOpenByDimOrd', err, cause_i=dimord)
end subroutine
