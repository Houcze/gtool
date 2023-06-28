!== dc_url.f90 - 変数 URL の文字列解析
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dc_url.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! This file provides dc_url
!

module dc_url
  !
  !== Overview
  !
  ! このモジュールは gtool4 変数 URL の文字列解析
  ! を行うための手続きを提供します。
  !
  ! gtool4 変数の書式に関しては,
  ! {gtool4 netCDF 規約}[link:../xref.htm#label-6]
  ! の「5. 各種の文字列書式」を参照ください。
  !
  !== Procedures Summary
  !
  ! 手続き群の要約
  !
  ! UrlSplit         :: 変数 URL を分解しファイル名、変数名、
  !                     属性名および入出力範囲指定を取り出す
  ! UrlMerge         :: ファイル名、変数名、属性名および入出力範囲指定
  !                     を連結して変数 URL を作成
  ! UrlResolve       :: 変数 URL の補完
  ! Url_Chop_IOrange :: 変数 URL から iorange を除去
  ! UrlSearchIORange :: 変数 URL 内の iorange うち, ある次元に関する
  !                     入出力範囲指定の値を取得
  ! dc_url#operator(.OnTheSameFile.) :: 2 つの変数 URL
  !                                     が同じファイルを指すかどうか判定
  !
  ! このモジュールは gtool4 変数において特別な役割を果たす
  ! 文字のニーモニックを提供します。gtool4 変数解析の際には、
  ! 直接文字を用いるのではなく、ここで提供する変数群
  ! (GT_ATMARK 等) を利用してください。
  !


  implicit none
  private

  public:: Url_Chop_IOrange
  public:: operator(.OnTheSameFile.)

  public:: UrlMerge
  interface UrlMerge
!    module procedure url_merge_v_vvv
    module procedure url_merge_cc
    module procedure url_merge_cccc
    module procedure url_merge_cccca
  end interface

  public:: UrlSplit
  interface UrlSplit
!    module procedure url_split_v
    module procedure url_split_c
  end interface

  public:: UrlResolve
  interface UrlResolve
    module procedure url_resolve_c
  end interface

  public:: UrlSearchIORange
  interface UrlSearchIORange
    module procedure url_search_iorange
  end interface

  interface operator(.OnTheSameFile.)
    module procedure UrlOnTheSameFile
  end interface

  character, public, parameter:: GT_ATMARK = "@"
                                 ! ファイル名と変数名の区切りに用いられます。
  character, public, parameter:: GT_QUESTION = "?"
                                 ! ファイル名と変数名の区切りに用いられます。
  character, public, parameter:: GT_COLON = ":"
                                 ! 変数の属性を示す時に用いられます。
  character, public, parameter:: GT_COMMA = ","
                                 ! 入出力範囲の限定に用いられます。
  character, public, parameter:: GT_EQUAL = "="
                                 ! 入出力範囲の限定に用いられます。
  character, public, parameter:: GT_CIRCUMFLEX = "^"
                                 ! 座標の位置を値ではなく、
                                 ! 格子点番号で指定する時に用いられます。
  character, public, parameter:: GT_PLUS = "+"
                                 ! 属性の行頭にこの文字がつく場合、大域属性を示します。

contains

  ! ANUrlMerge - 変数 URL の合成
  ! 空文字列の成分はないとみなされる。

!  type(VSTRING) function &
!    & url_merge_v_vvv(file, var, attr, iorange) result(result) !:nodoc:
!    use dcstring_base, only: VSTRING, operator(.cat.), operator(/=), &
!      & extract, operator(==) !:nodoc:
!    implicit none
!    type(VSTRING), intent(in):: file
!    type(VSTRING), intent(in), optional:: var
!    type(VSTRING), intent(in), optional:: attr
!    type(VSTRING), intent(in), optional:: iorange
!    result = file .cat. GT_ATMARK
!    if (present(var)) result = result .cat. var
!    if (present(attr)) then
!      if (attr /= "") result = result .cat. GT_COLON .cat. attr
!    endif
!    if (present(iorange)) then
!      if (extract(iorange, 1, 1) == GT_COMMA) then
!        result = result .cat. iorange
!      else if (iorange /= "") then
!        result = result .cat. GT_COMMA .cat. iorange
!      endif
!    endif
!  end function

  function url_merge_cc(file, var) result(result)
    !
    ! ファイル名 file、変数名 var を結合して relsult として返します。
    !
    use dc_types, only: STRING
    character(len = STRING):: result
    character(len = *), intent(in):: file
    character(len = *), intent(in):: var
  continue
    result = url_merge_cccc(file, var, "", "")
  end function url_merge_cc

  function url_merge_cccca(file, var, attr, iorange) result(result)
    !
    ! ファイル名 file、変数名 var、属性 attr、
    ! 入出力範囲 iorange を結合して relsult として返します。
    ! iorange には文字型配列を与えます。文字型配列のそれぞれの要素は
    ! GT_COMMA で連結されてから結合されます。
    !
    use dc_types, only: STRING
    character(len = STRING):: result
    character(len = *), intent(in):: file
    character(len = *), intent(in):: var
    character(len = *), intent(in):: attr
    character(len = *), intent(in):: iorange(:)
    integer:: i
  continue
    if (file /= "") then
      result = trim(file) // gt_atmark
    else
      result = gt_atmark
    endif
    if (var /= "") result = trim(result) // var
    if (attr /= "") then
      result = trim(result) // gt_colon // attr
    endif
    do i = 1, size(iorange)
      if (iorange(i) /= "") then
        if (iorange(i)(1:1) == gt_comma) then
          result = trim(result) // trim(iorange(i))
        else
          result = trim(result) // gt_comma // trim(iorange(i))
        endif
      endif
    end do
  end function url_merge_cccca

  function url_merge_cccc(file, var, attr, iorange) result(result)
    !
    ! ファイル名 file、変数名 var、属性 attr、
    ! 入出力範囲 iorange を結合して relsult として返します。
    !
    use dc_types, only: STRING
    character(len = STRING):: result
    character(len = *), intent(in):: file
    character(len = *), intent(in):: var
    character(len = *), intent(in):: attr
    character(len = *), intent(in):: iorange
  continue
    if (trim(file) /= "") then
      result = trim(file) // gt_atmark
    else
      result = gt_atmark
    endif
    if (trim(var) /= "") result = trim(result) // var
    if (trim(attr) /= "") then
      result = trim(result) // gt_colon // attr
    endif
    if (trim(iorange) /= "") then
      if (iorange(1:1) == gt_comma) then
        result = trim(result) // iorange
      else
        result = trim(result) // gt_comma // iorange
      endif
    endif
  end function url_merge_cccc

  subroutine Url_Chop_IOrange(fullname, iorange, remainder)
    !
    ! fullname で与えられる変数 URL の入出力範囲指定部分と
    ! 残りの部分とを分離し、それぞれ iorange と remainder に返します。
    !
    use dc_types, only: STRING
    character(len = *), intent(in):: fullname
    character(len = *), intent(out):: iorange   ! 入出力範囲指定部分
    character(len = *), intent(out):: remainder ! 残りの部分
    character(STRING):: file, var, attr
    call urlsplit(fullname, file=file, var=var, attr=attr, iorange=iorange)
    remainder = url_merge_cccc(file=file, var=var, attr=attr, iorange="")
  end subroutine url_chop_iorange

  function url_search_iorange(fullname, dimvar) result(result)
    !
    ! 変数 URL *fullname* 内の, 次元 *dimvar* に関する
    ! 入出力範囲指定の値を取得します。
    !
    ! fullname には gtool4 変数全体または入出力範囲指定部分の値を与えます。
    ! dimvar には入出力範囲指定部分に含まれる次元変数名を与えます。
    ! dimvar に対応する次元変数が存在する場合、その値を返します。
    ! dimvar に対応する次元変数が存在しない場合、空文字を返します。
    !
    use dc_types, only: STRING
    use dc_string, only: Split
    character(len = *), intent(in):: fullname
    character(len = *), intent(in):: dimvar
    character(len = STRING):: result
    character(STRING):: file, var, attr, iorange
    character(STRING), pointer :: ioranges_slice(:) => null()
    integer :: i, eqpos, atmark
  continue
    result = ""
    ! @ または ? が含まれているなら urlsplit で分離
    atmark = index(fullname, GT_QUESTION)
    if (atmark == 0) atmark = index(fullname, GT_ATMARK)
    if (atmark /= 0) then
      call urlsplit(fullname, file=file, var=var, attr=attr, iorange=iorange)
    else
      iorange = fullname
    end if
    call Split(iorange, ioranges_slice, GT_COMMA)
    do i = 1, size(ioranges_slice)
      eqpos = index(ioranges_slice(i), GT_EQUAL)
      if (ioranges_slice(i)(1:eqpos-1) == trim(dimvar)) then
        result = trim(ioranges_slice(i)(eqpos+1:))
        exit
      end if
    end do
    deallocate(ioranges_slice)
  end function url_search_iorange

  subroutine url_split_c(fullname, file, var, attr, iorange)
    !
    ! fullname で与えられる変数 URL を、ファイル名 file、 変数名 var、
    ! 属性名 attr、入出力範囲指定 iorange に分解して返します。
    ! 見つからない成分には空文字列が代入されます。
    !
    use dc_types, only: STRING
    character(len = *), intent(in):: fullname
    character(len = *), intent(out), optional:: file, var, attr, iorange
    character(len = STRING):: varpart
    integer:: atmark, colon, comma
    character(len = *), parameter:: VARNAME_SET &
      = "0123456789eEdD+-=^,.:_" &
      // "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
      // "abcdefghijklmnopqrstuvwxyz"
  continue
    ! まず URL と変数属性指定 (? または @ 以降) を分離する。
    ! URL は @ を含みうるため、最後の @ 以降に対して変数属性
    ! として許されない文字（典型的には '/'）が含まれていたら
    ! 当該 @ は URL の一部とみなす。
    atmark = index(fullname, GT_QUESTION)
    if (atmark == 0) then
      atmark = index(fullname, GT_ATMARK, back=.TRUE.)
      if (atmark /= 0) then
        if (verify(trim(fullname(atmark+1: )), VARNAME_SET) /= 0) then
          atmark = 0
        endif
      endif
    endif
    if (atmark == 0) then
      ! 変数属性指定はなかった。
      if (present(file)) file = fullname
      if (present(var)) var = ''
      if (present(attr)) attr = ''
      if (present(iorange)) iorange = ''
      return
    endif
    varpart = fullname(atmark+1: )
    ! 変数属性指定があった。
    if (present(file)) file = fullname(1: atmark - 1)
    ! 範囲指定を探索する。
    comma = index(varpart, GT_COMMA)
    if (comma /= 0) then
      ! 範囲指定がみつかった。
      if (present(var)) var = varpart(1: comma - 1)
      if (present(attr)) attr = ''
      if (present(iorange)) iorange = varpart(comma + 1: )
      return
    endif
    if (present(iorange)) iorange = ''
    ! 範囲指定がなかったので、属性名の検索をする。
    colon = index(varpart, GT_COLON)
    if (colon == 0) then
      if (present(var)) var = varpart
      if (present(attr)) attr = ''
      varpart = ''
      return
    endif
    if (present(var)) var = varpart(1: colon - 1)
    if (present(attr)) attr = varpart(colon + 1: )
    varpart = ''
  end subroutine url_split_c

!  subroutine url_split_v(fullname, file, var, attr, iorange) !:nodoc:
!    use dcstring_base, only: VSTRING, operator(.cat.), operator(/=), &
!      & extract, operator(==) !:nodoc:
!    use dc_string
!    implicit none
!    type(VSTRING), intent(in):: fullname
!    type(VSTRING), intent(out), optional::        file, var, attr, iorange
!    type(VSTRING):: varpart
!    integer:: atmark, colon, comma
!    character(len = *), parameter:: VARNAME_SET &
!      = "0123456789eEdD+-=^,.:_" &
!      // "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
!      // "abcdefghijklmnopqrstuvwxyz"
!  continue
!    ! まず URL と変数属性指定 (? または @ 以降) を分離する。
!    ! URL は @ を含みうるため、最後の @ 以降に対して変数属性
!    ! として許されない文字（典型的には '/'）が含まれていたら
!    ! 当該 @ は URL の一部とみなす。
!    atmark = vindex(fullname, GT_QUESTION)
!    if (atmark == 0) then
!      atmark = vindex(fullname, GT_ATMARK, .TRUE.)
!      if (atmark /= 0) then
!        varpart = extract(fullname, atmark + 1)
!        if (vverify(varpart, VARNAME_SET) /= 0) then
!          atmark = 0
!        endif
!      endif
!    endif
!    if (atmark == 0) then
!      ! 変数属性指定はなかった。
!      if (present(file)) file = fullname
!      if (present(var)) var = ''
!      if (present(attr)) attr = ''
!      if (present(iorange)) iorange = ''
!      return
!    endif
!    varpart = extract(fullname, atmark + 1)
!    ! 変数属性指定があった。
!    if (present(file)) file = extract(fullname, 1, atmark - 1)
!    ! 範囲指定を探索する。
!    comma = vindex(varpart, GT_COMMA)
!    if (comma /= 0) then
!      ! 範囲指定がみつかった。
!      if (present(var)) var = extract(varpart, 1, comma - 1)
!      if (present(attr)) attr = ''
!      if (present(iorange)) iorange = extract(varpart, comma + 1)
!      return
!    endif
!    if (present(iorange)) iorange = ''
!    ! 範囲指定がなかったので、属性名の検索をする。
!    colon = vindex(varpart, GT_COLON)
!    if (colon == 0) then
!      if (present(var)) var = varpart
!      if (present(attr)) attr = ''
!      varpart = ''
!      return
!    endif
!    if (present(var)) var = extract(varpart, 1, colon - 1)
!    if (present(attr)) attr = extract(varpart, colon + 1)
!    varpart = ''
!  end subroutine url_split_v

  !
  ! === 同じファイルに載っているかどうか判定 ===
  !

  logical function UrlOnTheSameFile(url_a, url_b) result(result)
    !
    ! 1 つ目の引数に与えられる変数 URL と 2 つ目の引数に与えられる
    ! 変数 URL とが同じファイルを指しているかどうか判定します。
    ! もしも同じファイルであれば <b><tt>.true.</tt></b> を、
    ! 異なるファイルであれば <b><tt>.false.</tt></b> を返します。
    !
    use dc_string
    use dc_types, only: STRING
    character(len = *), intent(in) :: url_a
    character(len = *), intent(in) :: url_b
    character(len = STRING)        :: filepart_a
    character(len = STRING)        :: filepart_b
    call UrlSplit(url_a, file=filepart_a)
    call UrlSplit(url_b, file=filepart_b)
    result = (filepart_a == filepart_b)
  end function UrlOnTheSameFile

  !
  ! === 相対リンクを解決 ===
  !

  function url_resolve_c(relative, base) result(result)
    !
    ! relative で与えられる変数 URL が完全でない (ファイル名、 変数名、
    ! 属性名、入出力範囲指定のどれかが無い) 場合に、 base
    ! から補完します。
    !
    use dc_string, only: StrHead
    use dc_types, only: STRING
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    character(len = *), intent(in):: relative
    character(len = *), intent(in):: base
    character(len = STRING):: result
    integer, parameter:: FILE = 1, VAR = 2, ATTR = 3, IOR = 4
    character(len = STRING):: rel(FILE:IOR), bas(FILE:IOR)
    character(3), parameter:: PATHDELIM = "/:" // achar(94)
    integer:: idir_r, idir_b
  continue
    call BeginSub('urlresolve', 'rel=<%c> base=<%c>', c1=relative, c2=base)
    call UrlSplit(trim(relative), file=rel(FILE), var=rel(VAR), &
      & attr=rel(ATTR), iorange=rel(IOR))
    call DbgMessage('rel -> file=<%c> var=<%c> attr=<%c>', &
      & c1=trim(rel(FILE)), c2=trim(rel(VAR)), &
      & c3=(trim(rel(ATTR)) // '> ior=<' // trim(rel(IOR))))
    call UrlSplit(base, file=bas(FILE), var=bas(VAR), &
      & attr=bas(ATTR), iorange=bas(IOR))
    call DbgMessage('base -> file=<%s> var=<%s> attr=<%s> ior=<%s>', &
      & c1=trim(bas(FILE)), c2=trim(bas(VAR)), &
      & c3=(trim(bas(ATTR)) // '> ior=<' // trim(bas(IOR))))
    ! --- ファイル名を欠くばあいは単に補う ---
    if (rel(FILE) == "") then
      rel(FILE) = bas(FILE)
      if (rel(VAR) == "") &
        & rel(VAR) = bas(VAR)
      result = UrlMerge(file=rel(FILE), var=rel(VAR), &
        & attr=rel(ATTR), iorange=rel(IOR))
      call EndSub('urlresolve', '1 result=%c', c1=trim(result))
      return
    endif
    ! --- 絶対パス (と見られる) ファイル名はそのまま使用 ---
    if (StrHead(rel(FILE), "file:") &
      & .OR. StrHead(rel(FILE), "http:") &
      & .OR. StrHead(rel(FILE), "ftp:") &
      & .OR. StrHead(rel(FILE), "news:") &
      & .OR. StrHead(rel(FILE), "www") &
      & .OR. StrHead(rel(FILE), "/") &
      & .OR. StrHead(rel(FILE), achar(94)) &
      & .OR. rel(FILE)(2:2) == ":" &
      ) then
      result = relative
      call EndSub('urlresolve', '2 result=%c', c1=trim(result))
      return
    endif
    ! ディレクトリ名の取り出し
    idir_b = scan(bas(FILE), PATHDELIM, back=.TRUE.) 
    if (idir_b == 0) then
      ! が、できなければ、（エラーとすべきかもしれぬが）
      ! 相対パスをそのまま使用
      result = relative
      call EndSub('urlresolve', '3 result=%c', c1=trim(result))
      return
    endif
    ! 相対パスのほうのディレクトリ名の取り出し
    idir_r = scan(rel(FILE), PATHDELIM, back=.TRUE.)
    if (idir_r == 0) then
      ! ができなければ全体を使用
      idir_r = 1
    endif
    result = base(1: idir_b) // relative(idir_r: )
    call EndSub('urlresolve', '4 result=%c', c1=trim(result))
  end function url_resolve_c

end module
