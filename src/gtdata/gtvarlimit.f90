!
!= 入出力範囲の拘束
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarlimit.f90,v 1.3 2009-05-25 09:55:57 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Limit
! として提供されます。

subroutine GTVarLimit_iiii(var, dimord, start, count, stride, err)
  !
  !== 入出力範囲の拘束 (数値で指定)
  !
  ! 変数 *var* 次元の入出力範囲を拘束します。
  ! Limit を呼び出した後では Slice でその範囲の外に入出力範囲を
  ! 設定できなくなります。これにより、変数全体ではなく一部を
  ! Slice_Next サブルーチンを用いて走査できるようになります。
  !
  ! 指定方法は、変数 *var* の *dimord* 番目の次元を基点 *start*,
  ! 格子総数 *count* 間隔 *stride* に限定します。
  ! このあとでは、スライス指定の第1格子は *start* 番目の格子を
  ! 指示することになり、スライス指定での格子数は *count* 個を
  ! 越えることができなくなり、スライス指定で間引きなしを指定すると
  ! *stride* 個ごとの指定を指示することになります。
  !
  ! エラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  ! *Limit* は 2 つのサブルーチンの総称名であり、
  ! 他にも {gtool4 netCDF 規約}[link:../xref.htm#label-6] の
  ! 「5.4 コンマ記法」を用いて指定することも可能です。
  ! 下記のサブルーチンを参照してください。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: map_lookup, gt_dimmap, dimrange, map_set
  use dc_error, only: dc_noerr, NF90_EINVAL, storeerror
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(inout):: var
  integer, intent(in)             :: dimord
  integer, intent(in) , optional  :: start, count, stride
  logical, intent(out), optional  :: err
  type(gt_dimmap), allocatable:: map(:)
  integer:: iolo, iohi, uilo, uihi, lowerlim, upperlim, dimlo, dimhi
  integer:: ndims, stat

  stat = NF90_EINVAL
  call beginsub('GTVarLimit_iiii', &
    & 'var%d-dim%d start=%d count=%d stride=%d', &
    & i=(/var%mapid, dimord, start, count, stride/))
  ! エラーチェック
  if (dimord < 1) then
    print *, "dimord =", dimord, " < 1"
    goto 999
  endif
  if (stride == 0) then
    print *, "stride == 0"
    goto 999
  endif
  call map_lookup(var, ndims=ndims)
  if (ndims <= 0) then
    print *, "ndims =", ndims, " <= 0"
    goto 999
  endif
  if (dimord > ndims) then
    print *, "dimrod =", dimord, " > ndims =", ndims
    goto 999
  endif
  if (allocated(map)) then
    deallocate(map)
  end if
  allocate(map(ndims))
  call map_lookup(var, map=map)
  ! (/lowerlim, upperlim/) は内部格子の範囲 (降順可)
  lowerlim = min(start, start + (count - 1) * stride)
  upperlim = max(start, start + (count - 1) * stride)
  call dimrange(var, dimord, dimlo, dimhi)
  if (lowerlim < dimlo) then
    print *, "lowerlim = ", lowerlim, " < dimlo =", dimlo
    goto 999
  endif
  if (upperlim > dimhi) then
    print *, "upperlim = ", upperlim, " < dimhi =", dimhi
    goto 999
  endif

  call DbgMessage('@ lowerlim=%d upperlim=%d', i=(/lowerlim, upperlim/))

  ! 入出力範囲を内部格子番号に変えておく
  uilo = map(dimord)%start
  iolo = 1 + map(dimord)%step * (uilo - 1) + map(dimord)%offset
  uihi = map(dimord)%start + (map(dimord)%count - 1) * map(dimord)%stride
  iohi = 1 + map(dimord)%step * (uihi - 1) + map(dimord)%offset

  call DbgMessage('@ userindex=%d %d, internal=%d %d', &
    & i=(/uilo, uihi, iolo, iohi/))
  call DbgMessage('@ DbgMessage offset %d -> %d step=%d', &
    & i=(/map(dimord)%offset, (start-1), stride/))

  ! 制限を課す。offset が変わればユーザ格子番号の意味が変わる
  map(dimord)%offset = start - 1
  map(dimord)%allcount = count
  map(dimord)%step = stride

  ! 入出力範囲を内部格子番号からユーザ格子番号に戻す
  uilo = 1 + (iolo - 1 - map(dimord)%offset) / map(dimord)%step
  uihi = 1 + (iohi - 1 - map(dimord)%offset) / map(dimord)%step
  call DbgMessage('@ userindex=%d %d', i=(/uilo, uihi/))

  ! それぞれは制限 [1 .. allcount] の中になければならない
  uilo = max(1, min(map(dimord)%allcount, uilo))
  uihi = max(1, min(map(dimord)%allcount, uihi))

  call DbgMessage('@ userindex=%d %d orig_stride=%d', &
    & i=(/uilo, uihi, map(dimord)%stride/))

  ! 元のストライドの符号は無視し、正に固定する
  map(dimord)%stride = max(1, abs(map(dimord)%stride))
  map(dimord)%start = min(uilo, uihi)
  map(dimord)%count = 1 + abs(uihi - uilo) / map(dimord)%stride

  call map_set(var, map, stat)
  if (stat /= 0) call DbgMessage("map_set fail")

999 continue
  call storeerror(stat, 'GTVarLimit_iiii', err)
  call endsub('GTVarLimit_iiii')
end subroutine GTVarLimit_iiii

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

subroutine GTVarLimit(var, string, err)
  !
  !== 入出力範囲の拘束 (文字列で指定)
  !
  ! 変数 *var* 次元の入出力範囲を拘束します。
  ! *Limit* は 2 つのサブルーチンの総称名であり、
  ! 別の指定方法もあります。まずは上記のサブルーチンを参照してください。
  !
  ! 指定方法は、*string* に
  ! {gtool4 netCDF 規約}[link:../xref.htm#label-6] の
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
  ! エラーが生じた場合、メッセージを出力
  ! してプログラムは強制終了します。*err* を与えてある場合には
  ! の引数に .true. が返り、プログラムは終了しません。
  !
  use gtdata_types, only: GT_VARIABLE
  use dc_trace, only: beginsub, endsub
  use dc_url, only: gt_comma
  use gtdata_internal_map, only: gtvar_dump
  type(GT_VARIABLE),  intent(inout):: var
  character(len = *), intent(in)   :: string
  logical, intent(out), optional   :: err
  integer:: is, ie
continue
  call beginsub('GTVarLimit', 'var=%d lim=<%c>', i=(/var%mapid/), c1=trim(string))
  call gtvar_dump(var)
  ! コンマで区切って解釈
  is = 1
  do
    ie = index(string(is: ), gt_comma)
    if (ie == 0) exit
    call limit_one(string(is: is+ie-2))
    is = is + ie
    if (is > len(string)) exit
  enddo
  call limit_one(string(is: ))
  if (present(err)) err = .false.
  call endsub('GTVarLimit')
  return
contains

  subroutine limit_one(string)
    use dc_url,         only: gt_equal
    use dc_string,      only: strieq, stoi
    use gtdata_generic, only: del_dim, dimname_to_dimord
    use gtdata_generic, only: del_dim, dimname_to_dimord, limit
    character(len = *), intent(in):: string
    integer:: equal, dimord
    integer:: start, count, stride, strhead
    logical:: myerr

    if (string == '') return

    strhead = 4
    if (len(string) < 4) strhead = len(string)

    if (strieq(string(1:strhead), "IGN:")) then
      ! 隠蔽型指定子 ign:<dim> または ign:<dim>=<start>
      equal = index(string, gt_equal)
      if (equal == 0) then
        start = 1
      else
        start = stoi(string(equal+1: ), default=1)
      endif
      dimord = dimname_to_dimord(var, string(5: equal-1))
      call limit(var, dimord, start, 1, 1, err)
      call del_dim(var, dimord, myerr)
      return
    endif

    ! 限定型指定子 <dim>=<start>:<finish>:<stride>
    ! いまは実装がバグっていて <start>:<count>:<stride> になってる
    !
    equal = index(string, gt_equal)
    if (equal == 0) return
    dimord = dimname_to_dimord(var, string(1: equal-1))
    if (dimord <= 0) return
    !
    call region_spec(dimord, string(equal+1: ), start, count, stride)
    call limit(var, dimord, start, count, stride, err)
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
    ! GTVarLimit の引数 *var* に格納される変数の次元 *dimord*
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
    real, pointer:: axisval(:) => null()
    real:: val
    integer:: i
  continue

    call beginsub('value_to_index', 'var=%d dimord=%d value=%c', &
      & i=(/var%mapid, dimord/), c1=trim(value))

    call Open(axisvar, var, dimord, count_compact=.true.)
    nullify(axisval)
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
    call endsub('value_to_index', '(%c) = %r', &
      & c1=trim(value), r=(/result/))
    deallocate(axisval)
  end function value_to_index

end subroutine GTVarLimit
