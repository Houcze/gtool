!== dcunits_com.f90 - 単位系処理用の下位モジュール
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: dcunits_com.f90,v 1.2 2009-03-23 22:01:42 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! This file provides dcunits_com
!

module dcunits_com !:nodoc:
  !
  !== Overview
  !
  ! dc_units モジュールで用いる下位の定数およびサブルーチンを提供します。
  !
  ! common private data for dc_units module
  !

  use dc_types, only: DP, STRING
  implicit none
  private
  public:: S_EOF, S_SHIFT, S_TEXT, S_MULTIPLY, S_DIVIDE, S_EXPONENT
  public:: S_OPENPAR, S_CLOSEPAR, S_REAL, S_INTEGER
  public:: DCUnitsSetLine, DCUnitsGetToken

  ! scannter symbols
  integer, parameter:: S_EOF = -128
  integer, parameter:: S_SHIFT = 300
  integer, parameter:: S_TEXT = 301
  integer, parameter:: S_MULTIPLY = 302
  integer, parameter:: S_DIVIDE = 303
  integer, parameter:: S_EXPONENT = 304
  integer, parameter:: S_OPENPAR = 305
  integer, parameter:: S_CLOSEPAR = 306
  integer, parameter:: S_REAL = 307
  integer, parameter:: S_INTEGER = 308

  ! scanner buffer
  character(STRING), private, save:: thisline = ""
  integer, private, save:: i = 1

contains

  subroutine DCUnitsSetLine(line)
    implicit none
    character(*), intent(in):: line
    thisline = line
    i = 1
  end subroutine DCUnitsSetLine

  subroutine DCUnitsGetToken(tokentype, ivalue, dvalue, cvalue)
    use dc_regex, only: match
    implicit none
    integer, intent(out):: tokentype
    integer, intent(out):: ivalue(5)
    real(DP), intent(out):: dvalue
    character(*), intent(out):: cvalue
    integer:: iend, istr, ilen, ios
    ivalue = 0
    dvalue = 0.0_DP
    cvalue = ""
    iend = len_trim(thisline)
    do
      if (i > iend) exit
      ! '#' 文字が現われれば EOF シンボルを返す
      call match("^##", thisline(i:), istr, ilen)
      if (istr > 0) then
        i = iend + 1
        tokentype = S_EOF
        return
      endif
      ! 空白を無視
      call match("^#s+", thisline(i:), istr, ilen)
      if (istr > 0) then
        i = i + ilen
        if (i > iend) exit
      endif
      ! シフト演算子チェック
      call match("^@", thisline(i:), istr, ilen)
      if (istr <= 0) call match("^from", thisline(i:), istr, ilen)
      if (istr <= 0) call match("^at", thisline(i:), istr, ilen)
      if (istr > 0) then
        i = i + ilen
        tokentype = S_SHIFT
        cvalue = thisline(i: i+ilen-1)
        return
      endif
      ! 名前チェック
      call match("^#a#w*#a", thisline(i:), istr, ilen)
      if (istr <= 0) call match("^[#a'""]", thisline(i:), istr, ilen)
      if (istr > 0) then
        tokentype = S_TEXT
        cvalue = thisline(i: i+ilen-1)
        i = i + ilen
        return
      endif
      ! '*' の前に '**' を認知せねば。
      call match("^#^", thisline(i:), istr, ilen)
      if (istr <= 0) call match("^#*#*", thisline(i:), istr, ilen)
      if (istr > 0) then
        tokentype = S_EXPONENT
        cvalue = thisline(i: i+ilen-1)
        i = i + ilen
        return
      endif
      ! 実数にならない小数点は S_MULTIPLY
      call match("^#.[^#d]", thisline(i:), istr, ilen)
      if (istr <= 0) call match("^#*", thisline(i:), istr, ilen)
      if (istr > 0) then
        tokentype = S_MULTIPLY
        cvalue = thisline(i: i+ilen-1)
        i = i + 1
        return
      endif
      ! 実数チェック. 小数点は語頭にあれば必ず数字が伴うので安心せよ
      call match("^[-+]?#d*#.#d*[EeDd][-+]?#d+", thisline(i:), istr, ilen)
      if (istr <= 0) call match("^[-+]?#d*#.#d*", thisline(i:), istr, ilen)
      if (istr > 0) then
        read(thisline(i: i+ilen-1), fmt=*, &
          & iostat=ios) dvalue
        if (ios /= 0) dvalue = HUGE(dvalue)
        cvalue = thisline(i: i+ilen-1)
        tokentype = S_REAL
        i = i + ilen
        return
      endif
      ! 整数チェック
      call match("^[-+]?#d+", thisline(i:), istr, ilen)
      if (istr > 0) then
        read(thisline(i: i+ilen-1), fmt=*, &
          & iostat=ios) ivalue(1)
        if (ios /= 0) ivalue(1) = HUGE(1)
        cvalue = thisline(i: i+ilen-1)
        tokentype = S_INTEGER
        i = i + ilen
        return
      endif
      ! ほかの１字トークンチェック
      if (thisline(i:i) == '/') then
        tokentype = S_DIVIDE
        cvalue = thisline(i:i)
        i = i + 1
        return
      endif
      if (thisline(i:i) == '(') then
        tokentype = S_OPENPAR
        cvalue = thisline(i:i)
        i = i + 1
        return
      endif
      if (thisline(i:i) == ')') then
        tokentype = S_CLOSEPAR
        cvalue = thisline(i:i)
        i = i + 1
        return
      endif
      ! だめだこりゃ。はい次いってみよう
      tokentype = ichar(thisline(i:i))
      cvalue = thisline(i:i)
      i = i + 1
      return
    enddo
    i = iend + 1
    tokentype = S_EOF
    cvalue = ""
  end subroutine DCUnitsGetToken

end module dcunits_com
