!== Open GT_VARIABLE of dimension by dimord
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvaropenbydimord.f90,v 1.5 2009-07-04 04:58:06 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

subroutine GTVarOpenByDimOrd(var, source_var, dimord, count_compact, err)
  !
  !== gtool4 データのオープン
  !
  ! 既に開かれた変数 *source_var* の *dimord* 番目の次元にあたる変数を
  ! 開き var に格納します。順序 *dimord* は現在の入出力範囲が
  ! 幅１になっている (コンパクト化している) を飛ばした
  ! 順序であすが、*count_compact* に <tt>.true.</tt>を指定すると
  ! すべての次元のなかの順序になります。
  !
  ! Open された変数は必ず Close されなければなりません。
  !
  ! *dimord* == 0 の場合は変数自体を再度開きます。これは参照カウンタを
  ! 増加させる手段です。
  !
  ! *Open* は 2 つのサブルーチンの総称名であり、
  ! 変数 URL を直接指定することで開くことも可能です。
  ! 下記のサブルーチンを参照ください。
  !
  !=== 補足
  !
  ! 変数 URL にファイル名部を指定しない場合、gtool.nc であるとみなされます。
  !
  ! 変数 URL にファイル名だけを指定した場合、開かれる変数は以下の規則
  ! で選択されます。
  !
  ! * 次元変数は選択されない
  ! * なるべく先に定義された変数が選択される
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory, &
    & map_dup, map_create, map_lookup, GT_DIMMAP, &
    & map_set, dimord_skip_compact
  use gtdata_netcdf_generic, only: Open, Inquire
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_generic, only: gt_open => open
  use dc_present, only: present_and_true
  use dc_trace, only: beginsub, endsub, DbgMessage
  use dc_string, only: CPrintf
  use dc_error, only: StoreError, GT_ENOMOREDIMS, GT_ENOMOREDIMS, &
    & DC_NOERR, GT_EFAKE
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(out):: var
  type(GT_VARIABLE), intent(in):: source_var
  integer, intent(in):: dimord
  logical, intent(in), optional:: count_compact
  logical, intent(out), optional:: err
  integer:: sclass, scid, ld, sndims, stat, udimord, idimord, cause_i
  type(GD_NC_VARIABLE):: gdnc
  type(GT_DIMMAP), allocatable:: map_src(:)
  type(GT_DIMMAP):: map_result(1)
  logical:: cnt_compact
  character(STRING) :: endsub_msg
  character(len = *),      parameter:: subname = "GTVarOpen-By-Dimord"
  character(len = *),      parameter:: version = &
    & '$Name:  $' // &
    & '$Id: gtvaropenbydimord.f90,v 1.5 2009-07-04 04:58:06 morikawa Exp $'
continue
  call beginsub(subname, 'var.mapid=%d dimord=%d ', &
    & i=(/source_var%mapid, dimord/), version=version)
  stat = DC_NOERR
  cause_i = dimord
  endsub_msg = ''

  ! 変数それ自体を開き直す処理
  if (dimord == 0) then
    call map_dup(var, source_var)
    if (present(err)) err = .false.
    endsub_msg = 'dup'
    goto 999
  endif

  ! 表を引き、dimord 番 (count_compact に注意) の次元の内部変数
  ! 次元番号を調べる。
  call map_lookup(source_var, ndims=sndims)
  if (sndims <= 0 .or. dimord > sndims) then
    stat = GT_ENOMOREDIMS
    goto 999
  endif
  allocate(map_src(sndims))
  call map_lookup(source_var, map=map_src)
  cnt_compact = .false.
  if (present_and_true(count_compact)) then
    cnt_compact = .true.
  else
    cnt_compact = .false.
  end if
  call DbgMessage('count_compact=%y', l=(/cnt_compact/))

  if (cnt_compact) then
    udimord = dimord
  else
    udimord = dimord_skip_compact(dimord, map=map_src)
  endif
  if (udimord <= 0 .or. udimord > size(map_src)) then
    stat = GT_ENOMOREDIMS
    goto 999
  endif

  idimord = map_src(udimord)%dimno
  if (idimord < 1) then
    call gt_open(var, map_src(udimord)%url, err=err)
    ! storeerror はしなくてよい
    deallocate(map_src)
    goto 999
  endif

  ! 実態種別に合わせ「次元変数オープン」処理
  call var_class(source_var, sclass, scid)
  if (sclass == vtb_class_netcdf) then
    call Open(gdnc, GD_NC_VARIABLE(scid), idimord, err)
    call inquire(gdnc, dimlen=ld)
    call map_create(var, vtb_class_netcdf, gdnc%id, 1, (/ld/), stat)
    if (stat /= DC_NOERR) then
      cause_i = 1
      goto 999
    end if
    call map_lookup(var, map=map_result)
    map_result(1)%offset = map_src(udimord)%offset
    map_result(1)%step = map_src(udimord)%step
    map_result(1)%allcount = map_src(udimord)%allcount
    map_result(1)%start = map_src(udimord)%start
    map_result(1)%count = map_src(udimord)%count
    map_result(1)%stride = map_src(udimord)%stride
    call map_set(var, map=map_result, stat=stat)
  else if (sclass == vtb_class_memory) then
    var = source_var
    stat = DC_NOERR
  else
    stat = GT_EFAKE
  endif

  deallocate(map_src)
  endsub_msg = CPrintf('result_var=%d', i=(/var%mapid/))
999 continue
  call StoreError(stat, subname, cause_i=cause_i, err=err)
  call endsub(subname, '%c', c1=trim(endsub_msg))
end subroutine GTVarOpenByDimOrd
