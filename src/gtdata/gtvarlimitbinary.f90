!
!= 2 つの変数の次元構成の共通化
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarlimitbinary.f90,v 1.3 2009-05-25 09:55:57 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Transform
! として提供されます。

subroutine GTVarXformBinary(var1, var2, err)
  !
  !== 2 つの変数の次元配置の共通化
  !
  ! 変数 <b>var1</b> の次元構成が <b>var2</b> の次元構成と同じになるように
  ! 範囲拘束を行います。過剰な次元が <b>var1</b> にある場合、隠蔽
  ! を行います。(追加もできるようにする予定です)。
  !
  ! エラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  !--
  ! 二つの変数 var1, var2 に入出力範囲拘束を加えて次元配置を共通化する。
  ! 結果の次元構成はとりあえずモデルで使えるように決めた。
  !    var2 の空間を保持する。var1 を変形する。
  !    var2 の次元は (有幅・縮退ともに) var2 における幅がとられる。
  !    したがって var1 においては存在しないか var2 をカバーする幅で
  !    なければならない。
  !    var2 にない var1 の次元は見えないようになるので縮退しているか
  !    存在しないのでなければならない。
  !++
  use gtdata_types, only: gt_variable
  use gtdata_generic, only: inquire, get_slice
  use gtdata_internal_map, only: map_allocate, map_apply, gt_dimmap, gtvar_dump
  use dc_error, only: StoreError, GT_ENOMATCHDIM, GT_EFAKE, DC_NOERR
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(inout):: var1, var2
  logical, intent(out), optional:: err
  integer:: ndim1, ndim2, ndimo
  integer, allocatable:: map1(:), map2(:)
  type(GT_DIMMAP), pointer:: newmap(:)
  integer:: i, j, stat
  character(*), parameter:: subnam = "GTVarXformBinary"
continue
  call beginsub(subnam, 'mapid=[%d, %d]', i=(/var1%mapid, var2%mapid/))
  call gtvar_dump(var1)
  call gtvar_dump(var2)
  !
  ! 二つの変数 var1, var2 から共有次元を調べ、対応表 map1, map2 をつくる。
  !
  if (present(err)) err = .false.
  call inquire(var1, alldims=ndim1)
  call inquire(var2, alldims=ndim2)
  ndimo = max(ndim1, ndim2, 0)
  allocate(map1(1:ndim1), map2(1:ndim2))
  call getmatch(var1, var2, ndim1, ndim2, map1, map2)
  call DbgMessage('map1=%*d map2=%*d', i=(/map1(1:ndim1), map2(1:ndim2)/), n=(/ndim1, ndim2/))
  if (all(map2(1:ndim2) == 0)) then
    stat = gt_enomatchdim
    goto 999
  endif
  !
  ! 再配置テーブル作成開始
  !
  ndimo = ndim2 + count(map1(1:ndim1) == 0)
  call map_allocate(newmap, ndimo)
  !
  ! 1..ndim2 は map2 によって var2 の次元たちにマップする
  !
  newmap(1:ndim2)%dimno = map2(1:ndim2)
  call inquire(var2, allcount=newmap(1:ndim2)%allcount)
  call get_slice(var2, count=newmap(1:ndim2)%count)
  do, j = 1, ndim2
    if (map2(j) == 0) then
      newmap(j)%start = 1
      newmap(j)%stride = 1
      call inquire(var2, j, url=newmap(j)%url)
    else
      ! 位置対応によって var1 側での開始位置を決定する
      call adjust_slice(var1, var2, map2(j), j, &
        & newmap(j)%start, newmap(j)%stride)
    endif
  enddo
  !
  ! ndim2+1.. ndimo は var2 に対応させられない var1 の次元をおく
  !
  j = 0
  loop1: do, i = ndim2 + 1, ndimo
    do
      j = j + 1
      if (j > ndim1) exit loop1
      if (map1(j) <= 0) exit
    enddo
    newmap(i)%dimno = j
    call inquire(var1, dimord=j, allcount=newmap(i)%allcount)
    call get_slice(var1, dimord=j, start=newmap(i)%start, &
      & count=newmap(i)%count, stride=newmap(i)%stride)
  end do loop1
  !
  call map_apply(var1, map=newmap)
  !
  stat = dc_noerr
999 continue
  call StoreError(stat, subnam, err)
  call endsub(subnam, 'stat=%d', i=(/stat/))
  deallocate(map1, map2)
  return
contains

  !
  ! 二つの次元変数を調べ、軸上位置が対応するように
  ! start シフト数と stride ファクタを決定する
  !
  subroutine adjust_slice(var1, var2, idim1, idim2, offset, stepfact)
    use gtdata_generic, only: get, open, close
    type(GT_VARIABLE), intent(in):: var1, var2
    integer, intent(in):: idim1, idim2
    integer, intent(out):: offset, stepfact
    type(GT_VARIABLE):: var_d
    integer:: n, buf(1)
    real, allocatable:: val1(:), val2(:)
  continue
    call beginsub('adjust_slice')
    call open(var_d, source_var=var1, dimord=idim1, count_compact=.true.)
    call inquire(var_d, size=n)
    allocate(val1(n))
    call get(var_d, val1, n)
    call close(var_d)
    !
    call open(var_d, source_var=var2, dimord=idim2, count_compact=.true.)
    call inquire(var_d, size=n)
    allocate(val2(n))
    call get(var_d, val2, n)
    call close(var_d)
    !
    buf(1:1) = minloc(abs(val1(:) - val2(1)))
    offset = buf(1) - 1
    if (size(val2) < 2 .or. size(val1) < 2) then
      stepfact = 1
    else
      buf(1:1) = minloc(abs(val1(:) - val2(2)))
      stepfact = buf(1) - (offset + 1)
    endif
    !
    deallocate(val1, val2)
    call endsub('adjust_slice')
  end subroutine adjust_slice

  !
  ! 二つの変数から共有次元を調べ、対応表 map1, map2 を作る。
  ! すなわち、それぞれの次元番号から相方の次元番号を得る表である。
  !
  subroutine getmatch(var1, var2, ndim1, ndim2, map1, map2)
    use dc_types, only: STRING
    use dc_units, only: UNITS, add_okay, assignment(=), clear, deallocate
    use gtdata_generic, only: get_attr, open, close
    type(GT_VARIABLE), intent(in):: var1, var2
    integer, intent(in):: ndim1, ndim2
    integer, intent(out):: map1(:), map2(:)
    type(GT_VARIABLE):: var_d
    integer, allocatable:: map(:, :)
    integer:: i, j
    character(STRING):: su1, su2
    type(UNITS), allocatable:: u1(:), u2(:)
  continue
    call beginsub('getmatch')
    ! 返却値はデフォルト 0
    map1(:) = 0
    map2(:) = 0
    ! 表の構築: 初期値は消去法をとることを示す
    allocate(map(ndim1, ndim2))
    map(:, :) = 1

    ! 単位による対応 --- 加算可能でなければ対にしない
    ! 単位の構成
    allocate(u1(ndim1), u2(ndim2))
    do, i = 1, ndim1
      call open(var_d, var1, i, count_compact=.true.)
      call get_attr(var_d, 'units', su1)
      call close(var_d)
      call clear(u1(i))
      u1(i) = su1
    enddo
    do, j = 1, ndim2
      call open(var_d, var2, j, count_compact=.true.)
      call get_attr(var_d, 'units', su2)
      call close(var_d)
      call clear(u2(j))
      u2(j) = su2
    enddo
    ! 処理
    do, i = 1, ndim1
      do, j = 1, ndim2
        if (.not. add_okay(u1(i), u2(j))) &
          & map(i, j) = 0
      enddo
    enddo
    ! 単位の廃棄
    do, i = 1, ndim1
      call deallocate(u1(i))
    enddo
    do, j = 1, ndim2
      call deallocate(u2(j))
    enddo
    deallocate(u1, u2)

    if (map_finished(map)) goto 1000

    ! --- it fails ---
    call endsub('getmatch', 'fail')
    return

1000 continue
    do, i = 1, ndim1
      call DbgMessage('map(%d, :)=%*d', i=(/i, map(i,:)/), n=(/ndim2/))
    enddo
    do, i = 1, ndim1
      if (all(map(i, :) <= 0)) then
        map1(i) = 0
      else
        map1(i:i) = maxloc(map(i, :))
      endif
    enddo
    do, j = 1, ndim2
      if (all(map(:, j) <= 0)) then 
        map2(j) = 0
      else
        map2(j:j) = maxloc(map(:, j), dim=1)
      endif
    enddo
    call endsub('getmatch', 'okay')
  end subroutine getmatch

  logical function map_finished(map) result(result)
    integer:: map(:, :)
    integer:: i, j, ni
  continue
    call beginsub('map_finished')
    ni = size(map, dim=1)
    do, i = 1, ni
      if (count(map(i, :) > 0) > 1) then
        result = .false.
        goto 999
      endif
    enddo
    do, j = 1, ni
      if (count(map(j, :) > 0) > 1) then
        result = .false.
        goto 999
      endif
    enddo
    result = .true.
999 continue
    call endsub('map_finished')
  end function map_finished

end subroutine GTVarXformBinary
