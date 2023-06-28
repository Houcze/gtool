!= gtool 変数表に関する各種手続き
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtdata_internal_map.f90,v 1.3 2010-04-11 14:13:50 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtdata_internal_map
  !
  !== gtool 変数表
  !
  ! gtool 変数というのは実はマップ表のキーとなる整数ハンドルである。
  ! マップ表 maptab には実体表のエントリ番号と次元書き換え／イテレータ
  ! の表が載っている。
  ! このレベルにおける参照カウントは作らないことにする。つまり、
  ! マップ表と実体表は一対一対応するし、
  ! ユーザがハンドルをコピーするのは勝手である。
  ! もちろんユーザには必ずただ１回
  ! 当該ハンドルを close すなわち maptabdelete する義務がある。

  use dc_types, only: STRING
  use gtdata_internal_vartable, only: vid_invalid, &
    & VTB_CLASS_UNUSED, VTB_CLASS_MEMORY, VTB_CLASS_NETCDF
    ! これらは, 外部から gtdata_internal_vartable を直接呼ばないようにするため,
    ! gtdata_internal_map を介して公開する.

  implicit none

  type GT_DIMMAP
    ! 次元書き換え表
    integer:: dimno
          ! 正ならば実体変数の次元番号, 他変数参照時は非正値
    character(len=STRING):: url
          ! 次元変数の url
    integer:: offset
          !=== 実体と gtool ユーザの格子番号対応
          !
          ! ユーザからみて 1..allcount が実体の
          ! (1..allcount) * step + offset に写像される。
          ! これらの値の変更の際は実体変数の許容添字範囲および
          ! 成長可能性を確認する必要あり。
          ! start 値に対するオフセット。
    integer:: step
          ! 1 か -1 の値をとることが期待される。
    integer:: allcount
          ! 見掛けの格子番号上限: start + count * stride <= allcount
    integer:: start
          !=== イテレータ本体
          ! 入出力範囲は (start:start+count*stride:stride) である。
          ! イテレータ start 値
    integer:: count
          ! イテレータ count 値
    integer:: stride
          ! イテレータ stride 値
    logical:: scalar
          ! スカラー変数である場合, .true.
  end type GT_DIMMAP

  type MAP_TABLE_ENTRY
    ! マップ表の型
    integer:: vid
    integer:: ndims
    type(GT_DIMMAP), pointer:: map(:)
  end type MAP_TABLE_ENTRY

  type(MAP_TABLE_ENTRY), save, target, allocatable:: maptab(:)
  integer, parameter:: maptab_init_size = 16

  public:: MapTabAdd, MapTabDelete, map_lookup
  public:: var_class, map_create, map_to_internal_specs, dimrange
  public:: vid_invalid, &
    &      VTB_CLASS_UNUSED, VTB_CLASS_MEMORY, VTB_CLASS_NETCDF
  private:: maptab, maptab_init_size

  interface dimrange
    module procedure dimrange_by_dimno
  end interface

contains

  subroutine dimrange_by_dimno(var, dimno, dimlo, dimhi)
    ! 変数と次元番号を指定して、当該次元の内部的添字番号範囲を得る
    use gtdata_types, only: gt_variable
    use gtdata_generic, only: open, close
    use gtdata_internal_vartable, only: dimrange
    type(gt_variable), intent(in):: var
    integer, intent(in):: dimno
    integer, intent(out):: dimlo, dimhi
    type(gt_variable):: dimvar
    integer:: vid
    call open(dimvar, var, dimno, count_compact=.true.)
    call map_lookup(dimvar, vid=vid)
    call dimrange(vid, dimlo, dimhi)
    call close(dimvar)
  end subroutine dimrange_by_dimno

  subroutine map_dup(var, source_var)
    ! 変数 source_var の複写 var を作成する
    use gtdata_types, only: gt_variable
    use gtdata_internal_vartable, only: VarTableAdd, VarTableLookup
    use dc_trace, only: DbgMessage
    type(gt_variable), intent(out):: var
    type(gt_variable), intent(in):: source_var
    integer:: vid, mid1, mid2, vid2, nd, class, cid
    call map_lookup(source_var, vid=vid)
    if (vid < 0) then
      var = gt_variable(-1)
      return
    endif
    if (vid == 0) then
      vid2 = 0
    else
      call VartableLookup(vid, class=class, cid=cid)
      call VarTableAdd(vid2, class, cid)
    endif
    call MapTabAdd(var%mapid, vid2)
    mid1 = source_var%mapid
    mid2 = var%mapid
    maptab(mid2)%ndims = maptab(mid1)%ndims
    if (associated(maptab(mid1)%map)) then
      nd = size(maptab(mid1)%map)
      allocate(maptab(mid2)%map(nd))
      maptab(mid2)%map(1:nd) = maptab(mid1)%map(1:nd)
    else
      nullify(maptab(mid2)%map)
    endif
    call DbgMessage('map_dup mapid(%d from %d) vid(%d from %d)', &
      & i=(/mid2, mid1, maptab(mid2)%vid, maptab(mid1)%vid/))
  end subroutine map_dup

  subroutine map_create(var, class, cid, ndims, allcount, stat)
    ! 変数 var を作成する。内部種別 class, 内部識別子 cid,
    ! 外見的次元数 ndims, 外見的次元長 allcount(:) を与える。
    ! オフセットゼロを仮定して諸元の初期化が行われる。
    use gtdata_types, only: gt_variable
    use gtdata_internal_vartable, only: VarTableAdd
    use dc_error, only: NF90_ENOTVAR, GT_ENOMOREDIMS, DC_NOERR
    type(gt_variable), intent(out):: var
    integer, intent(in):: class, cid, ndims, allcount(:)
    integer, intent(out):: stat
    type(GT_DIMMAP), pointer:: map(:)
    integer:: vid, i
  continue

    stat = DC_NOERR
    if ( ndims < 0 ) then
      stat = GT_ENOMOREDIMS
      goto 999
    end if
    call VarTableAdd(vid, class, cid)
    call MapTabAdd(var%mapid, vid)
    if (ndims > 0) then
      call map_allocate(map, ndims)
      maptab(var%mapid)%ndims = ndims
      maptab(var%mapid)%map => map

      do, i = 1, ndims
        map(i)%dimno = i
        map(i)%allcount = allcount(i)
        map(i)%count = allcount(i)
        map(i)%offset = 0
        map(i)%start = 1
        map(i)%step = 1
        map(i)%stride = 1
        map(i)%scalar = .false.
      enddo
    else
      ! スカラー変数 (ndims = 0) の場合
      call map_allocate(map, 1)
      maptab(var%mapid)%ndims = 0
      maptab(var%mapid)%map => map
      map(1)%dimno = 1
      map(1)%allcount = 1
      map(1)%count = 1
      map(1)%offset = 0
      map(1)%start = 1
      map(1)%step = 1
      map(1)%stride = 1
      map(1)%scalar = .true.
    end if

999 continue
    return
  end subroutine map_create

  subroutine MapTabAdd(mapid, vid)
    ! すでに実体表に追加されたエントリ番号 vid を指定して、
    ! マップ表にエントリを追加する。
    integer, intent(out):: mapid
    integer, intent(in):: vid
    type(MAP_TABLE_ENTRY), allocatable:: tmp_maptab(:)
    integer:: i, n
    ! 必要なら初期確保
    if (.not. allocated(maptab)) then
      allocate(maptab(maptab_init_size))
      maptab(:)%vid = vid_invalid
      do, n = 1, maptab_init_size
        nullify(maptab(n)%map)
      enddo
    endif
    ! 空き地があればそこに割り当て
    do, i = 1, size(maptab)
      if (maptab(i)%vid == vid_invalid) then
        mapid = i
        maptab(mapid)%vid = vid
        return
      endif
    enddo
    ! 空き地はなかったのだから倍幅確保
    n = size(maptab)
    allocate(tmp_maptab(n))
    tmp_maptab(:) = maptab(:)
    deallocate(maptab)
    allocate(maptab(n * 2))
    ! 確保したところはクリア
    maptab(1:n) = tmp_maptab(1:n)
    do, i = n + 1, (2 * size(tmp_maptab))
      maptab(i)%vid = vid_invalid
      nullify(maptab(i)%map)
    enddo
    deallocate(tmp_maptab)
    mapid = n + 1
    maptab(mapid)%vid = vid
  end subroutine MapTabAdd

  subroutine MapTabDelete(var, err)
    ! 変数 var をマップ表から削除する。
    ! 実体表には手をつけない。
    use dc_error, only: NF90_ENOTVAR, STOREERROR, DC_NOERR
    use gtdata_types, only: gt_variable
    use dc_trace, only: DbgMessage
    implicit none
    type(gt_variable), intent(in):: var
    logical, intent(out), optional:: err
    integer:: mapid
    mapid = var%mapid
    if (.not. allocated(maptab)) goto 999
    if (mapid <= 0 .or. mapid > size(maptab)) goto 999
    if (maptab(mapid)%vid == vid_invalid) goto 999
    maptab(mapid)%vid = vid_invalid
    if (associated(maptab(mapid)%map)) deallocate(maptab(mapid)%map)
    call storeerror(DC_NOERR, 'maptabdelete', err)
    call DbgMessage('gtdata_internal_map table %d deleted', i=(/mapid/))
    return
999 continue
    call storeerror(NF90_ENOTVAR, 'maptabdelete', err)
  end subroutine MapTabDelete

  subroutine map_lookup(var, vid, map, ndims)
    ! 同じファイル番号の変数表の中身を返す
    use gtdata_types, only: gt_variable
    type(GT_VARIABLE), intent(in):: var
    integer, intent(out), optional:: vid
    type(GT_DIMMAP), intent(out), optional:: map(:)
    integer, intent(out), optional:: ndims
    if (.not. allocated(maptab)) goto 999
    if (var%mapid <= 0 .or. var%mapid > size(maptab)) goto 999
    if (maptab(var%mapid)%vid == vid_invalid) goto 999
    if (present(vid)) vid = maptab(var%mapid)%vid
    if (present(map)) map(:) = maptab(var%mapid)%map(1:size(map))
    if (present(ndims)) ndims = maptab(var%mapid)%ndims
    return
999 continue
    if (present(vid)) vid = vid_invalid
    if (present(map)) then
      map(:)%dimno = -1
      map(:)%url = " "
    endif
    if (present(ndims)) ndims = 0
  end subroutine map_lookup

  subroutine map_set(var, map, stat)
    ! 同じファイル番号の変数表の値を設定する
    use gtdata_types, only: gt_variable
    use dc_error, only: NF90_ENOTVAR, GT_ENOMOREDIMS, DC_NOERR
    type(gt_variable), intent(in):: var
    type(GT_DIMMAP), intent(in):: map(:)
    integer, intent(out):: stat
    if (.not. allocated(maptab)) goto 999
    if (var%mapid <= 0 .or. var%mapid > size(maptab)) goto 999
    if (maptab(var%mapid)%vid == vid_invalid) goto 999
    if (size(map) > size(maptab(var%mapid)%map)) then
      stat = GT_ENOMOREDIMS
      return
    endif
    maptab(var%mapid)%map(1:size(map)) = map(:)
    stat = DC_NOERR
    return
999 continue
    stat = NF90_ENOTVAR
  end subroutine map_set


  subroutine var_class(var, class, cid)
    ! 変数 var を指定して、内部種別 class, 内部識別子 cid を得る。
    use gtdata_types, only: gt_variable
    use gtdata_internal_vartable, only: vartablelookup
    type(gt_variable), intent(in):: var
    integer, intent(out), optional:: class, cid
    integer:: vid
    call map_lookup(var, vid=vid)
    call vartablelookup(vid, class=class, cid=cid)
  end subroutine var_class

  subroutine map_set_ndims(var, ndims, stat)
    ! 変数 var の次元数を ndims に変える。
    use gtdata_types, only: gt_variable
    use gtdata_internal_vartable, only: vartablelookup
    use dc_error, only: NF90_ENOTVAR, GT_ENOMOREDIMS, DC_NOERR
    type(gt_variable), intent(in):: var
    integer, intent(in):: ndims
    integer, intent(out):: stat
    integer:: vid
    call map_lookup(var, vid=vid)
    if (vid == vid_invalid) then
      stat = NF90_ENOTVAR
      return
    endif
    if (.not. associated(maptab(var%mapid)%map)) then
      if (ndims == 0) then
        stat = DC_NOERR
        maptab(var%mapid)%ndims = 0
      else
        stat = GT_ENOMOREDIMS
      endif
    else
      if (ndims > size(maptab(var%mapid)%map)) then
        stat = GT_ENOMOREDIMS
      else
        stat = DC_NOERR
        maptab(var%mapid)%ndims = ndims
      endif
    endif
  end subroutine map_set_ndims

  subroutine map_set_rank(var, rank, stat)
    ! 変数 var のランク(非縮退次元数)を rank に減らすように
    ! count 値を1に減らす。ランクを増やすことや外見次元数の操作はしない。
    use gtdata_types, only: gt_variable
    use gtdata_internal_vartable, only: vartablelookup
    use dc_error, only: NF90_ENOTVAR, GT_ENOMOREDIMS, DC_NOERR
    type(gt_variable), intent(in):: var
    integer, intent(in):: rank
    integer, intent(out):: stat
    type(GT_DIMMAP), pointer:: tmpmap(:)
    integer:: ndims
    integer:: vid, nd
    call map_lookup(var, vid, ndims=ndims)
    if (vid == vid_invalid) then
      stat = NF90_ENOTVAR
      return
    endif
    if (ndims < rank) then
      stat = GT_ENOMOREDIMS
      return
    endif
    tmpmap => maptab(var%mapid)%map
    do, nd = ndims, 1, -1
      if (count(tmpmap(1:ndims)%count > 1) <= rank) exit
      tmpmap(nd)%count = 1
    enddo
    stat = DC_NOERR
  end subroutine map_set_rank

  subroutine map_to_internal_specs(var, specs, ndims)
    ! マップ表から netCDF の引数にふさわしい start, count, stride, imap
    ! を作成する。ただし、stride が負になるばあいは対策されていない。
    ! (暫定的に gdncvarget/gdncvarput が対応している)
    use gtdata_types, only: gt_variable
    use gtdata_internal_vartable, only: num_dimensions => ndims
    type(gt_variable), intent(in):: var
    integer, pointer:: specs(:, :)
    integer, intent(out), optional:: ndims
    type(GT_DIMMAP), pointer:: it
    integer:: vid, i, j, imap, internal_ndims
    integer:: external_ndims
  continue
    call map_lookup(var, vid, ndims=external_ndims)
    internal_ndims = num_dimensions(vid)
    if (present(ndims)) ndims = internal_ndims
    allocate(specs(max(1, internal_ndims), 4))
    specs(:, 1) = 1
    specs(:, 2) = 1
    specs(:, 3) = 1
    specs(:, 4) = 0
    imap = 1
    do, i = 1, size(maptab(var%mapid)%map)
      it => maptab(var%mapid)%map(i)
      j = it%dimno
      if (j > 0 .and. j <= internal_ndims) then
        specs(j, 1) = it%start + it%offset
        specs(j, 2) = it%count
        if (i > external_ndims) specs(j, 2) = 1
        specs(j, 3) = it%stride * it%step
        specs(j, 4) = imap
      endif
      imap = imap * it%count
    enddo
  end subroutine map_to_internal_specs

  subroutine map_allocate(map, ndims)
    ! 次元表エントリに ndims 個のエントリを割り付け初期化する。
    type(GT_DIMMAP), pointer:: map(:)
    integer, intent(in):: ndims
    if (ndims <= 0) then
      nullify(map)
      return
    endif
    allocate(map(1:ndims))
    map(1:ndims)%dimno = -1
    map(1:ndims)%url = ' '
    map(1:ndims)%allcount = 0
    map(1:ndims)%offset = 0
    map(1:ndims)%step = 1
    map(1:ndims)%start = 1
    map(1:ndims)%count = 0
    map(1:ndims)%stride = 1
    map(1:ndims)%scalar = .false.
  end subroutine map_allocate

  subroutine map_apply(var, map)
    ! 変数 var にマップ表 map を組み合わせる
    use gtdata_types, only: gt_variable
    type(GT_VARIABLE), intent(inout):: var
    type(GT_DIMMAP), pointer:: map(:)
    type(GT_DIMMAP), pointer:: tmpmap(:), varmap
    integer:: i, nd
    nd = size(map)
    allocate(tmpmap(nd))
    do, i = 1, nd
      tmpmap(i)%allcount = map(i)%allcount
      tmpmap(i)%count = map(i)%count
      if (map(i)%dimno > 0) then
        varmap => maptab(var%mapid)%map(map(i)%dimno)
        tmpmap(i)%url = varmap%url
        tmpmap(i)%dimno = varmap%dimno
        tmpmap(i)%offset = varmap%offset + map(i)%offset
        tmpmap(i)%step = varmap%step * map(i)%step
      else
        tmpmap(i)%url = map(i)%url
        tmpmap(i)%dimno = 0
        tmpmap(i)%offset = map(i)%offset
        tmpmap(i)%step = map(i)%step
      endif
    enddo
    deallocate(map)
    map => tmpmap
  end subroutine map_apply

  subroutine map_resize(var, ndims)
    ! 変数 var の次元表の大きさを変える
    use gtdata_types, only: gt_variable
    type(GT_VARIABLE), intent(in):: var
    integer, intent(in):: ndims
    type(GT_DIMMAP), pointer:: newmap(:)
    type(GT_DIMMAP), pointer:: tmpmap(:)
    integer:: n
    if (associated(maptab(var%mapid)%map)) then
      tmpmap => maptab(var%mapid)%map
      call map_allocate(newmap, ndims)
      n = min(size(tmpmap), ndims)
      newmap(1:n) = tmpmap(1:n)
      deallocate(tmpmap)
      maptab(var%mapid)%map => newmap
      newmap(n+1:ndims)%dimno = -1
      newmap(n+1:ndims)%url = ' '
      newmap(n+1:ndims)%allcount = 0
      newmap(n+1:ndims)%offset = 0
      newmap(n+1:ndims)%step = 1
      newmap(n+1:ndims)%start = 1
      newmap(n+1:ndims)%count = 0
      newmap(n+1:ndims)%stride = 1
    else
      call map_allocate(maptab(var%mapid)%map, ndims)
      n = 1
    endif
  end subroutine map_resize

  subroutine gtvar_dump(var)
    ! 変数のプロパティを出力
    use gtdata_types, only: gt_variable
    use gtdata_internal_vartable, only: vartable_dump
    use dc_trace, only: debug, DbgMessage
    type(gt_variable), intent(in):: var
    integer:: idim, imap
    logical:: dbg_mode
  continue
    call Debug( dbg_mode )
    if (.not. dbg_mode) return
    imap = var%mapid
    if (imap < 1 .or. imap > size(maptab)) then
      call DbgMessage('[gt_variable %d: invalid id]', i=(/imap/))
      return
    endif
    if (associated(maptab(imap)%map)) then
      call DbgMessage('[gt_variable %d: ndims=%d, map.size=%d]', &
        & i=(/imap, maptab(imap)%ndims, size(maptab(imap)%map)/))
      do, idim = 1, size(maptab(imap)%map)
        call DbgMessage('[dim%d dimno=%d ofs=%d step=%d' &
          &// ' all=%d start=%d count=%d stride=%d url=%c]', &
          & c1=trim(maptab(imap)%map(idim)%url), &
          & i=(/idim, maptab(imap)%map(idim)%dimno, &
          & maptab(imap)%map(idim)%offset, &
          & maptab(imap)%map(idim)%step, &
          & maptab(imap)%map(idim)%allcount, &
          & maptab(imap)%map(idim)%start, &
          & maptab(imap)%map(idim)%count, &
          & maptab(imap)%map(idim)%stride/))
      enddo
    else
      call DbgMessage('[gt_variable %d: ndims=%d, map=null]', &
        & i=(/imap, maptab(imap)%ndims/))
    endif
    call vartable_dump(maptab(imap)%vid)
  end subroutine gtvar_dump

  integer function dimord_skip_compact(dimord, map) result(result)
    ! 次元表の中で非縮退次元だけを数えた次元番号 dimord の次元を
    ! 特定し、外部向けの次元番号を返す。
    use dc_trace, only: DbgMessage
    integer, intent(in):: dimord
    type(GT_DIMMAP), intent(in):: map(:)
    integer:: nd, id
    result = -1
    nd = 0
    do, id = 1, size(map)
      if (map(id)%count < 2) cycle
      nd = nd + 1
      if (nd < dimord) cycle
      result = id
      call DbgMessage('compact dim skip: %d <= %d', i=(/result, dimord/))
      exit
    enddo
  end function dimord_skip_compact

end module gtdata_internal_map
