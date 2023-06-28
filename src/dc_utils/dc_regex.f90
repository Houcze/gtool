!== dc_regex.f90 - 正規表現モジュール
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: dc_regex.f90,v 1.1 2009-03-23 21:48:50 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module dc_regex
  !
  !== 正規表現モジュール
  !
  ! サブルーチン match により正規表現を用いた文字列マッチを行うことが
  ! 可能です。
  !
  !--
  !== 開発者向けコメント
  !
  ! 正規表現からオートマトンへの変換は行わず、再帰手続によって表現します。
  ! Fortran の文字型として便利なように、パターン、テキストともに
  ! trailing spaces は必要ならば無限に補充されるもののように扱われます
  ! '$' 文字はパターンの終了をあらわします。
  !
  !++

  implicit none
  private
  public:: match

  character, save:: C_ESCAPE = '#'
  integer, parameter:: SYM_EOL = -128
  integer, parameter:: SYM_ANYCHAR = 500
  integer, parameter:: SYM_QUESTION = 501
  integer, parameter:: SYM_PLUS = 502
  integer, parameter:: SYM_STAR = 503
  integer, parameter:: SYM_NORMAL_SET = 520
  integer, parameter:: SYM_REVERSED_SET = 521
  integer, parameter:: SYM_HEADFIX = 540
  integer, parameter:: SYM_TAILFIX = 541
  integer, parameter:: SYM_ISDIGIT = 560
  integer, parameter:: SYM_ISALPHA = 561
  integer, parameter:: SYM_ISWORD = 562
  integer, parameter:: SYM_ISSPACE = 563
  integer, parameter:: SYM_ISXDIGIT = 564
  integer, parameter:: SYM_COUNT_BASE = 1000

contains

  subroutine preprocess_pattern(pattern, symbols)
    !
    ! メタキャラクタと普通の文字を分離
    !
    character(len = *), intent(in):: pattern
    integer, intent(out):: symbols(:)
    integer:: i, j, code, imax, j_last_set
    integer:: status, stat_return
    integer, parameter:: STAT_INIT = 1, STAT_ESCAPE = 2, &
      STAT_OPEN_SET = 3, STAT_IN_SET = 4, STAT_HEXADECIMAL = 5
    character:: c
  continue
    status = STAT_INIT
    stat_return = STAT_INIT
    symbols(:) = SYM_EOL
    j_last_set = 0
    imax = len_trim(pattern)
    j = 1
    do, i = 1, imax
      c = pattern(i:i)
      select case(status)
      case(STAT_INIT)
        if (c == C_ESCAPE) then
          status = STAT_ESCAPE
          cycle
        else if (c == "[") then
          symbols(j) = SYM_NORMAL_SET
          status = STAT_OPEN_SET
        else if (c == ".") then
          symbols(j) = SYM_ANYCHAR
        else if (c == "?") then
          symbols(j) = SYM_QUESTION
        else if (c == "+") then
          symbols(j) = SYM_PLUS
        else if (c == "*") then
          symbols(j) = SYM_STAR
        else if (c == "^" .and. i == 1) then
          symbols(j) = SYM_HEADFIX
        else if (c == "$" .and. i == imax) then
          symbols(j) = SYM_TAILFIX
        else
          symbols(j) = ichar(c)
        endif
      case(STAT_ESCAPE)
        if (c == 'd' .or. c == 'D') then
          symbols(j) = SYM_ISDIGIT
        else if (c == 'a' .or. c == 'A') then
          symbols(j) = SYM_ISALPHA
        else if (c == 'w' .or. c == 'W') then
          symbols(j) = SYM_ISWORD
        else if (c == 's' .or. c == 'S') then
          symbols(j) = SYM_ISSPACE
        else if (c == 'z' .or. c == 'Z') then
          symbols(j) = SYM_ISXDIGIT
        else if (c == 'x' .or. c == 'X') then
          symbols(j) = -1
          status = STAT_HEXADECIMAL
          cycle
        else
          symbols(j) = ichar(c)
        end if
        status = stat_return
      case(STAT_HEXADECIMAL)
        code = index("123456789ABCDEFabcdef", c)
        if (code >= 16) code = code - 6
        if (symbols(j) == -1) then
          symbols(j) = code
          cycle
        else
          symbols(j) = symbols(j) * 16 + code
          status = stat_return
        endif
      case(STAT_OPEN_SET)
        symbols(j) = SYM_COUNT_BASE
        j_last_set = j
        stat_return = STAT_IN_SET
        if (c == '^') then
          symbols(j - 1) = SYM_REVERSED_SET
          status = STAT_IN_SET
        else if (c == C_ESCAPE) then
          status = STAT_ESCAPE
        else
          j = j + 1
          symbols(j) = ichar(c)
          status = STAT_IN_SET
        endif
      case(STAT_IN_SET)
        if (c == ']') then
          symbols(j_last_set) = SYM_COUNT_BASE + j - j_last_set - 1
          stat_return = STAT_INIT
          status = STAT_INIT
          cycle
        else if (c == C_ESCAPE) then
          status = STAT_ESCAPE
          cycle
        else
          symbols(j) = ichar(c)
        endif
      end select
      j = j + 1
    enddo
    select case(status)
    case(STAT_ESCAPE)
      symbols(j) = ichar(' ')
    case(STAT_OPEN_SET)
      symbols(j) = SYM_COUNT_BASE
    case(STAT_IN_SET)
      symbols(j_last_set) = SYM_COUNT_BASE + j - j_last_set - 1
    end select
  end subroutine preprocess_pattern

  ! マッチすれば length は非負になる。
  ! マッチしなければ length == -1 となる。
  recursive subroutine match_here(ipat, text, length)
    integer, intent(in):: ipat(:)
    character(len = *), intent(in):: text
    integer, intent(out):: length
    integer:: s1, s2, remain, i, hitmax, hitcount, hit_at_least
    logical:: normal_hit
  continue
    ! パターンの終わり。空パターンには何でもマッチ
    if (size(ipat) == 0 .or. ipat(1) == SYM_EOL) then
      length = 0
      return
    endif
    ! パターンの文末固定指示
    if (ipat(1) == SYM_TAILFIX) then
      if (text == "") then
        length = 0
      else
        length = -1
      endif
      return
    endif
    if (len(text) == 0) then
      length = -1
      return
    endif
    ! 1字指定(範囲または1字リテラル)の抽出 ... ipat(s1:s2)
    if (ipat(1) == SYM_NORMAL_SET) then
      s1 = 3
      s2 = 2 + ipat(2) - SYM_COUNT_BASE
      normal_hit = .TRUE.
    else if (ipat(1) == SYM_REVERSED_SET) then
      s1 = 3
      s2 = 2 + ipat(2) - SYM_COUNT_BASE
      normal_hit = .FALSE.
    else
      s1 = 1
      s2 = 1
      normal_hit = .TRUE.
    endif
    ! その次の記号 ipat(s2+1) は量化子か次の1字指定である
    remain = s2 + 2
    select case (ipat(s2 + 1))
    case(SYM_STAR)
      hitmax = len(text)
      hit_at_least = 0
    case(SYM_PLUS)
      hitmax = len(text)
      hit_at_least = 1
    case(SYM_QUESTION)
      hitmax = 1
      hit_at_least = 0
    case default
      hitmax = 1
      hit_at_least = 1
      remain = s2 + 1
    end select
    ! 現位置以降の1字指定のヒット数を数える
    hitcount = 0
    do, i = 1, hitmax
      if (hit(ipat(s1:s2), text(i:i)) .neqv. normal_hit) then
        exit
      endif
      hitcount = i
    enddo
    ! 現位置で無ヒットの場合、ヒットを要するならマッチ失敗
    if (hitcount < hit_at_least) then
      length = -1
      return
    endif
    ! 最長原理: なるべく長くヒットしたものから、残りのマッチする
    ! ものを探す。いわゆる最左最長探索の最長である。
    do, i = 1 + hitcount, 1 + hit_at_least, -1
      call match_here(ipat(remain: ), text(i: ), length)
      if (length >= 0) then
        length = length + i - 1
        return
      endif
    enddo
    length = -1
  end subroutine match_here

  logical function hit(ipat, c) result(result)
    integer, intent(in):: ipat(:)
    character(len=*), intent(in):: c
    character(len=*), parameter:: &
      & DIGIT = "0123456789", &
      & XDIGIT = "ABCDEFabcdef", &
      & ALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    integer:: i
  continue
    do, i = 1, size(ipat)
      select case(ipat(i))
      case(SYM_ANYCHAR)
        result = .TRUE.
      case(SYM_ISALPHA)
        result = (index(ALPHA, c) > 0)
      case(SYM_ISDIGIT)
        result = (index(DIGIT, c) > 0)
      case(SYM_ISWORD)
        result = (index(DIGIT, c) > 0 .or. index(ALPHA, c) > 0 .or. &
          & c == '_')
      case(SYM_ISXDIGIT)
        result = (index(DIGIT, c) > 0 .or. index(XDIGIT, c) > 0)
      case(SYM_ISSPACE)
        result = (c == ' ' .or. (iachar(c) >= 8 .and. iachar(c) <= 13))
      case default
        result = (ipat(i) == ichar(c))
      end select
      if (result) return
    enddo
    result = .FALSE.
  end function hit

  subroutine match(pattern, text, start, length)
    !
    ! _pattern_ には正規表現を与えます。
    ! _text_ には正規表現によって探査したい文字列を与えます。
    !
    ! _pattern_ が _text_ にマッチした場合、
    ! _start_ には文字列の何文字目からマッチしたのかを示す数値 (正の整数)
    ! が返ります。
    ! _length_ には何文字分マッチしたのかを示す数値 (正の整数)
    ! が返ります。
    !
    ! マッチしない場合、 length == -1, start == 0 となります。
    !
    !
    !=== 例
    !
    !      program regex_test
    !        use dc_regex, only: match
    !        use dc_types, only: TOKEN
    !        implicit none
    !      
    !        integer:: start, length
    !        character(TOKEN) :: pattern, text
    !      continue
    !        pattern = "->"
    !        text    = "time->0.0,x->hoge"
    !        call match(trim(pattern), trim(text), start, length)
    !        call formatted_print
    !      
    !        pattern = "^##+"
    !        text    = "####### hoge"
    !        call match(trim(pattern), trim(text), start, length)
    !        call formatted_print
    !      
    !        pattern = "@+$"
    !        text    = "# hoge @@@"
    !        call match(trim(pattern), trim(text), start, length)
    !        call formatted_print
    !      
    !      contains
    !        subroutine formatted_print
    !          use dc_string, only: Printf
    !          call Printf(fmt='pattern= %c : text= %c : start= %d : length= %d', &
    !            & c1=trim(pattern), c2=trim(text), i=(/start, length/))
    !        end subroutine formatted_print
    !      
    !      end program regex_test
    !
    ! このプログラムを実行することで以下の出力が得られるはずです。
    !
    !      pattern= -> : text= time->0.0,x->hoge : start= 5 : length= 2
    !      pattern= ^##+ : text= ####### hoge : start= 1 : length= 7
    !      pattern= @+$ : text= # hoge @@@ : start= 8 : length= 3
    !
    implicit none
    character(len = *), intent(in):: pattern, text
    integer, intent(out):: start, length
    integer, allocatable:: ipattern(:)
    integer:: text_length
  continue
    ! 空 pattern は空文字列に適合
    if (len(pattern) <= 0) then
      length = 0
      start = 1
      return
    endif
    ! メタキャラクタの認識
    allocate(ipattern(len(pattern) + 2))
    call preprocess_pattern(pattern, ipattern)
    ! 頭寄せ指定のある場合
    if (ipattern(1) == SYM_HEADFIX) then
      start = 1
      call match_here(ipattern(2: ), text, length)
      if (length < 0) goto 995
      goto 999
    endif
    ! 最左原理
    text_length = len(text)
    do, start = 1, text_length + 1
      call match_here(ipattern, text(start:text_length), length)
      if (length >= 0) goto 999
    end do
    ! みつからない場合
995 continue
    start = 0
    length = -1
999 continue
    deallocate(ipattern)
  end subroutine match

end module dc_regex
