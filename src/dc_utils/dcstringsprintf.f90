!== Formatted output conversion
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcstringsprintf.f90,v 1.2 2009-03-20 09:50:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== Overview
!
! C の sprintf(3) のように文字列をフォーマットして返します。
! ただし、実装は C の sprintf(3) とは大分違うのでご注意ください。
!
!== Formatter
!
! dc_string#CPrintf, dc_string#Printf のフォーマット引数に
! 用いられる指示子は <b><tt>%</tt></b> で始まります。種類は
! 以下の通りです。
!
! <b><tt>指示子</tt></b> ::
!    <tt>対応する引数</tt> :: データの種類と出力形式
!
! <b><tt>%d, %D</tt></b> ::
!    <tt>i(:)</tt>       :: 整数データ (10 進数) を表示. 
!              %2d や %04d のように'%' の後ろに数字を指定することで
!              出力する桁数を変更できます.
!              '%' の直後が 0 の場合は先頭に 0 を, そうでない場合は空白を埋めます.
!
! <b><tt>%o, %O</tt></b> ::
!    <tt>i(:)</tt>       :: 8 進数データを表示
!
! <b><tt>%x, %X</tt></b> :: 
!    <tt>i(:)</tt>       :: 16 進数データを表示
!
! <b><tt>%f, %F</tt></b> :: 
!    <tt>d(:)</tt>       :: 倍精度実数データを最大全桁数 80、小数部の桁数 40で表示
!
! <b><tt>%r, %R</tt></b> :: 
!    <tt>r(:)</tt>       :: 単精度実数データを最大全桁数 80、小数部の桁数 40で表示
!
! <b><tt>%b, %B</tt></b> :: 
!    <tt>L(:)</tt>       :: 論理データを 真：T、偽：F で表示
!
! <b><tt>%y, %Y</tt></b> :: 
!    <tt>L(:)</tt>       :: 論理データを 真：yes、偽：no で表示
!
! <b><tt>%c, %C</tt></b> :: 
!    <tt>c1、c2、c3</tt> :: 文字データ (変数)
!
! <b><tt>%a, %A</tt></b> :: 
!    <tt>ca</tt>         :: 文字データ (配列)
!
!
! 文字データ (変数) 以外は、1つの型のデータをいくつでも与えることが可能です。
! 文字データ (変数) は c1、c2、c3 にそれぞれ 1
! つづつの文字データしか与えることができません。
! +ca+ 引数を用いる場合は dc_string#StoA を併用すると便利です。
!
! また、フォーマット指定子として <b><tt>%*</tt></b> を与えることで、
! 複数のデータを一度に出力することも可能です。
! その場合、いくつのデータを一度に出力するかを <tt>n(:)</tt>
! に与える必要があります。
!
!== Example
!
!=== dc_string#CPrintf を用いた出力の例
!
!      use dc_types,  only: STRING
!      use dc_string, only: CPrintf
!      character(len = STRING) :: output, color="RED", size="Large"
!      integer, parameter      :: n1 = 2, n2 = 3
!      integer                 :: int = 10, arrayI1(n1), arrayI2(n2), i
!      real                    :: arrayR(n1)
!      logical                 :: eq
!   
!      do, i = 1, n1
!        arrayI1(i) = 123 * i ; arrayR(i)  = 1.23 * i
!      enddo
!      do, i = 1, n2
!        arrayI2(i) = 345 * i
!      enddo
!      eq = (maxval(arrayI1) == minval(arrayI2))
!      output = CPrintf(fmt="color=%c size=%c int=%03d I1=%*d I2=%*04d R=%*r equal=%y", &
!        &              c1=trim(color), c2=trim(size), i=(/int, arrayI1, arrayI2/), &
!        &              r=(/arrayR/), L=(/eq/), n=(/n1, n2, n1/))
!   
!      write(*,*) trim(output)
!
! 文字データ以外のものは基本的に1次元配列しか引数にとれないため、
! 多次元配列を出力したい場合には組込み関数である pack 関数を
! 用いると良いでしょう。以下にその例を記します。
!
!      use dc_types,  only: STRING
!      use dc_string, only: CPrintf
!      character(len = STRING) :: output
!      integer                 :: i,j,k
!      integer, parameter      :: n1 = 2, n2 = 3, n3 = 4
!      real                    :: array(n1,n2,n3)
!   
!      do, i = 1, n1
!        do, j = 1, n2 
!          do, k = 1, n3
!            array(i,j,k) = i * 0.1 + j * 1.0 + k * 10.0
!          enddo
!        enddo
!      enddo
!      output = CPrintf('array=<%*r>', &
!        &              r=(/pack(array(:,:,:), .true.)/), n=(/size(array(:,:,:))/))
!      write(*,*) trim(output)
!
!=== dc_string#Printf を用いた出力の例
!
!      use dc_types,  only: STRING
!      use dc_string, only: Printf
!      character(len = STRING) :: output, color="RED", size="Large"
!      integer, parameter      :: n1 = 2, n2 = 3
!      integer                 :: int = 10, arrayI1(n1), arrayI2(n2), i
!      real                    :: arrayR(n1)
!      logical                 :: eq
!
!      do, i = 1, n1
!        arrayI1(i) = 123 * i   ; arrayR(i)  = 1.23 * i
!      enddo
!      do, i = 1, n2
!        arrayI2(i) = 345 * i
!      enddo
!      eq = (maxval(arrayI1) == minval(arrayI2))
!
!      ! 装置番号 6 (標準出力) に直接出力する場合
!      call Printf(unit=6, &
!        &         fmt="color=%c size=%c int=%03d I1=%*d I2=%*04d R=%*r equal=%y", &
!        &         c1=trim(color), c2=trim(size), i=(/int, arrayI1, arrayI2/), &
!        &         r=(/arrayR/), L=(/eq/), n=(/n1, n2, n1/))
!
!      ! 文字列 output に渡す場合
!      call Printf(unit=output, &
!        &         fmt="color=%c size=%c int=%03d I1=%*d I2=%*04d R=%*r equal=%y", &
!        &         c1=trim(color), c2=trim(size), i=(/int, arrayI1, arrayI2/), &
!        &         r=(/arrayR/), L=(/eq/), n=(/n1, n2, n1/))
!      write(*,*) trim(output)


subroutine DCStringSPrintf(unit, fmt, i, r, d, L, n, c1, c2, c3, ca)
  !
  ! フォーマット文字列 fmt に従って変換された文字列を unit に返します。
  ! 第2引数 fmt には指示子を含む文字列を与えます。
  ! 指示子には「<tt>%</tt>」を用います。
  ! <tt>%</tt> を用いたい場合は 「<tt>%%</tt>」と記述します。
  ! 指示子および用例に関しての詳細は dc_utils/dcstringsprintf.f90 を参照ください。
  !
  use dc_types,      only: DP
  implicit none
  character(*),     intent(out)         :: unit
  character(*),     intent(in)          :: fmt
  integer,          intent(in), optional:: i(:), n(:)
  real,             intent(in), optional:: r(:)
  real(DP),         intent(in), optional:: d(:)
  logical,          intent(in), optional:: L(:)
  character(*),     intent(in), optional:: c1, c2, c3
  character(*),     intent(in), optional:: ca(:)

  ! 上記配列引数のカウンタ
  integer:: ni, nr, nd, nl, nc, na, nn
  integer:: ucur       ! unit に書かれた文字数
  integer:: endp       ! 既に処理された fmt の文字数
  integer:: cur        ! 現在着目中の文字は fmt(cur:cur) である
  integer:: ptr        ! fmt から検索をするときに使用
  integer:: exp_ptr    ! fmt から数値の指数部を検索をするときに使用
  integer:: minus_ptr  ! '-' を検索する時に使用
  integer:: repeat     ! %数字 または %* から決定された繰返し数
  integer:: m          ! 1:repeat の範囲で動くループ変数
  integer:: stat       ! エラー処理
  character(80):: cbuf ! read/write 文のバッファ
  character(80):: exp_buf ! real/write 文の指数部のバッファ (実数型用)
  character(80):: ibuf ! real/write 文のバッファ (整数型用)
  integer:: len_ibuf   ! ibuf の長さ
  integer:: figs_ibuf  ! ibuf の有効な桁数
  logical:: int_zero_fill ! 先頭を 0 で埋めるかどうかを判定するフラグ (整数型用)
  integer:: int_figs   ! 整数型を出力する際の桁数 (整数型用)
continue
  ni = 0;  nr = 0;  nd = 0;  nl = 0;  nc = 0;  na = 0;  nn = 0
  unit = ""
  ucur = 0
  endp = 0
  int_figs = 0
  int_zero_fill = .false.
  MainLoop: do
    cur = endp + 1
    if (cur > len(fmt)) exit MainLoop
    !
    ! リテラルに転写できる文字列 fmt(cur:endp-1) を発見処理
    !
    endp = cur - 1 + scan(fmt(cur: ), '%')
    if (endp > cur) then
      call append(unit, ucur, fmt(cur:endp-1), stat)
      if (stat /= 0) exit MainLoop
    else if (endp == cur - 1) then
      call append(unit, ucur, fmt(cur: ), stat)
      exit MainLoop
    endif
    !
    ! % から書式指定文字までを fmt(cur:endp) とする
    !
    cur = endp + 1
    endp = cur - 1 + scan(fmt(cur: ), 'DdOoXxFfRrBbYySsCcAa%')
    if (endp < cur) then
      call append(unit, ucur, fmt(cur-1: ), stat)
      exit MainLoop
    endif
    cbuf = fmt(cur:endp-1)
    !
    ! %* がある場合、n(:) に渡された数から繰り返し回数を取得
    !
    if (cbuf(1:1) == '*') then
      nn = nn + 1
      if (nn > size(n)) then
        repeat = 1
      else
        repeat = n(nn)
      endif
      ibuf = cbuf(2:)
    else
      repeat = 1
      ibuf = cbuf
!    else if (cbuf == '') then
!      repeat = 1
!    else
!      ptr = verify(cbuf, " 0123456789")
!      if (ptr > 0) cbuf(ptr: ) = " "
!      read(cbuf, "(I80)", iostat=ptr) repeat
    endif
    !
    ! %2d や %04d のように '%' の後ろに数字が指定され、
    ! かつ d (整数型変数の表示) の場合には先頭に空白
    ! または 0 を埋める.
    !
    if (scan(ibuf(1:1),'1234567890') > 0) then
      if (ibuf(1:1) == '0') then
        int_zero_fill = .true.
      else
        int_zero_fill = .false.
      end if
      read(unit=ibuf, fmt="(i80)") int_figs
    else
      int_figs = 0
      int_zero_fill = .false.
    endif
    PercentRepeat: do, m = 1, repeat
      if (m > 1) then
        call append(unit, ucur, ", ", stat)
        if (stat /= 0) exit MainLoop
      endif
      select case(fmt(endp:endp))
      case('d', 'D')
        if (.not. present(i)) cycle MainLoop
        ni = ni + 1;  if (ni > size(i)) cycle MainLoop
        write(ibuf, "(i20)") i(ni)
        len_ibuf = len(trim(adjustl(ibuf)))
        figs_ibuf = verify(ibuf, ' ')
        cbuf = ' '
        if (int_figs > len_ibuf) then
          minus_ptr = scan(ibuf, '-')
          if (int_zero_fill) then
            if (minus_ptr /= 0) then
              len_ibuf = len_ibuf - 1
              figs_ibuf = figs_ibuf + 1
              cbuf(1:int_figs-len_ibuf) = '-0000000000000000000'
            else
              cbuf(1:int_figs-len_ibuf) = '00000000000000000000'
            end if
          end if
          cbuf(int_figs-len_ibuf+1:) = ibuf(figs_ibuf:20)
        else
          cbuf = ibuf(figs_ibuf:20)
        end if
        call append(unit, ucur, trim(cbuf), stat)
        if (stat /= 0) exit MainLoop
      case('o', 'O')
        if (.not. present(i)) cycle MainLoop
        ni = ni + 1;  if (ni > size(i)) cycle MainLoop
        write(cbuf, "(o20)") i(ni)
        call append(unit, ucur, trim(adjustl(cbuf)), stat)
        if (stat /= 0) exit MainLoop
      case('x', 'X')
        if (.not. present(i)) cycle MainLoop
        ni = ni + 1;  if (ni > size(i)) cycle MainLoop
        write(cbuf, "(z20)") i(ni)
        call append(unit, ucur, trim(adjustl(cbuf)), stat)
        if (stat /= 0) exit MainLoop
      case('f', 'F')
        if (.not. present(d)) cycle MainLoop
        nd = nd + 1;  if (nd > size(d)) cycle MainLoop
        write(cbuf, "(g80.40)") d(nd)
        cbuf = adjustl(cbuf)
        exp_ptr = verify(cbuf, ' 1234567890-+.', back=.TRUE.)
        exp_buf = ' '
        if (exp_ptr > 0) then
          exp_buf = cbuf(exp_ptr: )
          cbuf(exp_ptr: ) = " "
        end if
        ptr = verify(cbuf, " 0", back=.TRUE.)
        if (ptr > 0) cbuf(ptr+1: ) = " "
        cbuf = trim(cbuf) // trim(exp_buf)
        call append(unit, ucur, trim(adjustl(cbuf)), stat)
        if (stat /= 0) exit MainLoop
      case('r', 'R')
        if (.not. present(r)) cycle MainLoop
        nr = nr + 1;  if (nr > size(r)) cycle MainLoop
        write(cbuf, "(g80.40)") r(nr)
        cbuf = adjustl(cbuf)
        exp_ptr = verify(cbuf, ' 1234567890-+.', back=.TRUE.)
        exp_buf = ' '
        if (exp_ptr > 0) then
          exp_buf = cbuf(exp_ptr: )
          cbuf(exp_ptr: ) = " "
        end if
        ptr = verify(cbuf, " 0", back=.TRUE.)
        if (ptr > 0) cbuf(ptr+1: ) = " "
        cbuf = trim(cbuf) // trim(exp_buf)
        call append(unit, ucur, trim(adjustl(cbuf)), stat)
        if (stat /= 0) exit MainLoop
      case('b', 'B')
        if (.not. present(L)) cycle MainLoop
        nl = nl + 1;  if (nl > size(L)) cycle MainLoop
        write(cbuf, "(L1)") L(nl)
        call append(unit, ucur, trim(adjustl(cbuf)), stat)
        if (stat /= 0) exit MainLoop
      case('y', 'Y')
        if (.not. present(L)) cycle MainLoop
        nl = nl + 1;  if (nl > size(L)) cycle MainLoop
        if (L(nl)) then
          call append(unit, ucur, "yes", stat)
          if (stat /= 0) exit MainLoop
        else
          call append(unit, ucur, "no", stat)
          if (stat /= 0) exit MainLoop
        endif
      case('c', 'C')
        nc = nc + 1
        if (nc == 1) then
          if (.not. present(c1)) cycle PercentRepeat
          call append(unit, ucur, c1, stat)
          if (stat /= 0) exit MainLoop
        else if (nc == 2) then
          if (.not. present(c2)) cycle PercentRepeat
          call append(unit, ucur, c2, stat)
          if (stat /= 0) exit MainLoop
        else if (nc == 3) then
          if (.not. present(c3)) cycle PercentRepeat
          call append(unit, ucur, c3, stat)
          if (stat /= 0) exit MainLoop
        endif
      case('a', 'A')
        if (.not. present(ca)) cycle MainLoop
        na = na + 1;  if (na > size(ca)) cycle MainLoop
        call append(unit, ucur, trim(adjustl(ca(na))), stat)
        if (stat /= 0) exit MainLoop
      case('%')
        call append(unit, ucur, '%', stat)
        if (stat /= 0) exit MainLoop
      end select
    enddo PercentRepeat
  enddo MainLoop
  return
contains

  subroutine append(unitx, ucur, val, stat)
    !
    ! unitx に val を付加。その際、unitx がその最大文字列長を越えた場合
    ! には stat = 2 を返す。
    !
    character(*), intent(inout):: unitx ! 最終的に返される文字列
    integer,      intent(inout):: ucur ! unitx の文字数
    character(*), intent(in)   :: val  ! unitx に付加される文字列
    integer,      intent(out)  :: stat ! ステータス
    integer                    :: wrsz ! val の文字列
    continue
    ! unitx の最大長を越えた場合には stat = 2 を返す。
    if (ucur >= len(unitx)) then
      stat = 2
      ! 正常時の処理
    else
      ! unitx の長さを越えた場合も考慮して unitx に val を付加する。
      wrsz = min(len(val), len(unitx) - ucur)
      unitx(1+ucur: wrsz+ucur) = val(1: wrsz)
      ucur = ucur + wrsz
      stat = 0
      if (wrsz < len(val)) stat = 1
    endif
  end subroutine append

end subroutine DCStringSPrintf
