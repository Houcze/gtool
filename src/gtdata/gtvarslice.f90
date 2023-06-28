!
!= 入出力範囲の指定
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarslice.f90,v 1.3 2009-05-25 09:55:57 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Slice
! として提供されます。

subroutine GTVarSlice(var, dimord, start, count, stride)
  !
  !== 入出力範囲を数値で指定
  !
  ! 変数 *var* の入出力範囲を指定します。
  !
  ! 変数 *var* の *dimord* 番目の次元の入出力範囲を *start* から
  ! *stride* 個おきに *count* 個とします。*start*, *count*,
  ! *stride* のいずれを省略しても <b>1</b> が仮定されます。成功し
  ! たか否かを返す引数はありません。仮に指定できない範囲が指定さ
  ! た場合には、指定範囲を含むなるべく広い範囲を設定します。
  !
  ! *Slice* は複数のサブルーチンの総称名であり、
  ! 他にも文字列や番号で指定する方法があります。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_vartable, only: query_growable
  use gtdata_internal_map, only: map_lookup, map_set, gt_dimmap, gtvar_dump
  use dc_error, only: NF90_ENOTVAR, StoreError
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(in):: var
  integer, intent(in):: dimord
  integer, intent(in), optional:: start
  integer, intent(in), optional:: count
  integer, intent(in), optional:: stride
  type(gt_dimmap), allocatable:: map(:)
  integer:: vid, maxindex, maxcount, nd, stat
  logical:: growable_dimension
continue
  call beginsub('GTVarSlice', 'var%%mapid=%d dimord=%d', &
    & i=(/var%mapid, dimord/))
  call gtvar_dump(var)
  call map_lookup(var, vid=vid, ndims=nd)
  if (vid < 0) then
    call StoreError(NF90_ENOTVAR, "GTVarSlice")
  endif

  if (vid > 0) then
    call query_growable(vid, growable_dimension)
  else
    growable_dimension = .false.
  endif

  if (nd == 0) goto 999
  allocate(map(nd))
  call map_lookup(var, map=map)

  if (dimord <= 0 .or. dimord > size(map)) goto 998

  call DbgMessage('map(dimord): originally start=%d count=%d stride=%d', &
    & i=(/map(dimord)%start, map(dimord)%count, map(dimord)%stride/))
  if (.not. growable_dimension) then
    maxindex = map(dimord)%allcount
    call DbgMessage('maxindex=%d', i=(/maxindex/))
  endif

  if (present(start)) then
    if (start < 0) then
      map(dimord)%start = max(1, maxindex + 1 + start)
    else if (growable_dimension) then
      map(dimord)%start = max(1, start)
    else
      map(dimord)%start = min(maxindex, max(1, start))
    endif
    call DbgMessage('start=%d (%d specified)', i=(/map(dimord)%start, start/))
  endif

  if (present(stride)) then
    map(dimord)%stride = stride
    if (stride == 0) map(dimord)%stride = 1
    call DbgMessage('stride=%d (%d specified)', &
      & i=(/map(dimord)%stride, stride/))
  endif

  if (present(count)) then
    map(dimord)%count = abs(count)
    if (count == 0) map(dimord)%count = 1
    call DbgMessage('count=%d (%d specified)', &
      & i=(/map(dimord)%count, count/))
  endif

  if (.not. growable_dimension) then
    maxcount = 1 + (maxindex - map(dimord)%start) / map(dimord)%stride
    map(dimord)%count = max(1, min(maxcount, map(dimord)%count))
    call DbgMessage('count=%d ', i=(/map(dimord)%count/))
  endif
  call map_set(var, map, stat)
  if (stat /= 0) goto 998

  call endsub('GTVarSlice')
  deallocate(map)
  return

998 continue
  deallocate(map)
999 continue
  call endsub('GTVarSlice', 'err skipped')
end subroutine GTVarSlice

subroutine GTVarSliceC(var, string, err)
  !
  !== 入出力範囲を文字列で指定
  !
  ! 変数 *var* の入出力範囲を、*string* に応じて指定します。
  ! *string* には {gtool4 netCDF 規約}[link:../xref.htm#label-6] の
  ! 「5.4 コンマ記法」に述べられる範囲指定表現を用います。
  ! 凡例を以下に挙げます。
  !
  !     <dim>=<lower>
  !
  !     <dim>=<lower>:<upper>
  !
  !     <dim>=<lower>:<upper>:<stride>
  !
  ! ここで、<tt><dim></tt> は次元番号または次元名であり、
  ! <tt><lower></tt>, <tt><upper></tt>
  ! は座標値または "<tt>^</tt>" を前置した格子番号です。
  ! <tt><stride></tt> は格子数です。
  !
  ! 現在 *err* は必ず <tt>.false.</tt> を返すことになっています。
  !
  ! *Slice* は複数のサブルーチンの総称名であり、
  ! 他にも文字列や番号で指定する方法があります。
  !
  !
  !
  !--
  ! 変数 var に string によるマップ操作を行う。
  ! string はコンマで区切られた変換指定の列である。
  ! 変換指定は領域設定と次元順序変換のどちらかである。
  ! 領域設定は英数字で始まるもので、<dim>=<lower>, <dim>=<lower>:<upper>,
  !   <dim>=<lower>:<upper>:<stride> のような形式である。
  !   ここで、dim は次元番号または次元名であり、<lower>, <upper>
  !   は ^ を前置した座標即値または格子番号である。
  !   <stride> は格子数である。
  ! (未実装) 次元順序変換は = で始まるもので、
  !   IGN:<dim>=<pos>
  ! の形態をとる。
  !++
  !
  use gtdata_types,   only: GT_VARIABLE
  use gtdata_generic, only: slice
  use dc_trace,       only: beginsub, endsub
  use dc_url,         only: GT_COMMA
  use gtdata_internal_map,         only: gtvar_dump
  type(GT_VARIABLE),  intent(inout) :: var
  character(len = *), intent(in)    :: string
  logical,            intent(out)   :: err
  integer:: is, ie
continue
  call beginsub('GTVarSliceC', 'var=%d lim=<%c>', &
    & i=(/var%mapid/), c1=trim(string))
  call gtvar_dump(var)
  ! コンマで区切って解釈
  is = 1
  do
    ie = index(string(is: ), GT_COMMA)
    if (ie == 0) exit
    call limit_one(string(is: is+ie-2))
    is = is + ie
    if (is > len(string)) exit
  enddo
  call limit_one(string(is: ))
  err = .false.
  call endsub('GTVarSliceC')
  return
contains

  subroutine limit_one(string)
    use dc_url, only: gt_equal
    use dc_string, only: strieq, stoi
    use gtdata_generic, only: del_dim, dimname_to_dimord
    character(len = *), intent(in):: string
    integer:: equal, dimord
    integer:: start, count, stride
    logical:: myerr

    if (string == '') return

    if (strieq(string(1:4), "IGN:")) then
      ! 隠蔽型指定子 ign:<dim> または ign:<dim>=<start>
      equal = index(string, gt_equal)
      if (equal == 0) then
        start = 1
      else
        start = stoi(string(equal+1: ), default=1)
      endif
      dimord = dimname_to_dimord(var, string(5: equal-1))
      call slice(var, dimord, start, 1, 1)
      call del_dim(var, dimord, myerr)
      return
    endif

    ! 限定型指定子 <dim>=<start>:<finish>:<stride>
    !
    equal = index(string, gt_equal)
    if (equal == 0) return
    dimord = dimname_to_dimord(var, string(1: equal-1))
    if (dimord <= 0) return
    !
    call region_spec(dimord, string(equal+1: ), start, count, stride)
    call slice(var, dimord, start, count, stride)
  end subroutine limit_one

  !
  ! 範囲指定の = のあとを : で区切ってマップにいれる
  !
  subroutine region_spec(dimord, string, start, count, stride)
    use dc_types, only: token
    use dc_string, only: index_ofs, stoi
    use dc_url, only: gt_circumflex, gt_colon
    use gtdata_internal_map, only: dimrange
    integer, intent(in):: dimord
    integer, intent(out):: start, count, stride
    character(len = *), intent(in):: string
    integer:: colon, prev_colon, finish, dimlo, dimhi
    character(len = token):: val(3)
  continue
    colon = index(string, gt_colon)
    if (colon == 0) then
      ! コロンがない場合は上下端に同じ値
      val(1) = string(1: )
      val(2) = val(1)
      val(3) = ""
    else
      val(1) = string(1: colon - 1)
      prev_colon = colon
      colon = index_ofs(string, colon + 1, gt_colon)
      if (colon > 0) then
        val(2) = string(prev_colon + 1: colon - 1)
        val(3) = string(colon + 1: )
      else
        val(2) = string(prev_colon + 1: )
        val(3) = ""
      endif
    endif
    if (val(3) == "") val(3) = "^1"

    if (val(1)(1:1) == gt_circumflex) then
      start = stoi(val(1)(2: ))
    else if (val(1) == val(2)) then
      start = nint(value_to_index(dimord, val(1)))
    else
      start = floor(value_to_index(dimord, val(1)))
    endif
    if (val(2) == val(1)) then
      finish = start
    else if (val(2)(1:1) == gt_circumflex) then
      finish = stoi(val(2)(2: ))
    else
      finish = ceiling(value_to_index(dimord, val(2)))
    endif

    call dimrange(var, dimord, dimlo, dimhi)
    start = min(max(dimlo, start), dimhi)
    finish = min(max(dimlo, finish), dimhi)
    count = abs(finish - start) + 1

    if (val(3)(1:1) == gt_circumflex) then
      stride = stoi(val(3)(2: ))
    else
      stride = stoi(val(3))
    endif
    stride = sign(stride, finish - start)
  end subroutine region_spec

  real function value_to_index(dimord, value) result(result)
    !
    ! GTVarSlice の引数 *var* に格納される変数の次元 *dimord*
    ! に格納されるデータのうち, *value* が格納される
    ! 格子番号を整数値にして返します.
    !
    ! 例えば次元に以下のデータが格納されているとします.
    !
    !     0.05  0.1  0.15  0.20  0.25  0.30
    !
    ! この場合, *value* に 0.15 が与えられれば戻り値は 3. となります.
    ! また *value* に 0.225 が与えられれば戻り値は 4.5 となります.
    !
    !
    use gtdata_types, only: GT_VARIABLE
    use gtdata_generic, only: get, open, close
    use dc_string, only: stod
    use dc_trace, only: beginsub, endsub, DbgMessage
    integer, intent(in):: dimord
    character(len = *), intent(in):: value
    type(GT_VARIABLE):: axisvar
    real, pointer:: axisval(:)
    real:: val
    integer:: i
  continue
    call beginsub('value_to_index', 'var=%d dimord=%d value=%c', &
      & i=(/var%mapid, dimord/), c1=trim(value))

    call Open(axisvar, var, dimord, count_compact=.true.)
    call Get(axisvar, axisval)
    call Close(axisvar)
    if (.not. associated(axisval)) then
      result = -1.0
      return
    else if (size(axisval) < 2) then
      result = 1.0
      goto 900
    endif

    val = stod(value)

    ! call DbgMessage('value=%f axis=(/%*r/)', r=(/val, axisval(:)/), &
    !    & n=(/size(axisval)/))

    do, i = 1, size(axisval) - 1
      if (axisval(i + 1) == axisval(i)) then
        result = real(i) + 0.5
        goto 900
      endif
      result = i + (val - axisval(i)) / (axisval(i + 1) - axisval(i))
      if (result <= (i + 1)) goto 900
    enddo

900 continue
    call endsub('value_to_index', 'value(%c) =~ index(%r)', &
      & c1=trim(value), r=(/result/))
    deallocate(axisval)
  end function value_to_index

end subroutine GTVarSliceC
