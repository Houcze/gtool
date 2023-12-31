! -*- coding: utf-8; mode: f90 -*-
!-------------------------------------------------------------------------------------
! Copyright (c) 2000-2016 Gtool Development Group. All rights reserved.
!-------------------------------------------------------------------------------------
! ** Important**
!
! This file is generated from gtdata_generic.erb by ERB included Ruby 2.3.1.
! Please do not edit this file directly. @see "gtdata_generic.erb"
!-------------------------------------------------------------------------------------

!--
! *** Caution!! ***
!
! This file is generated from "gtdata_generic.rb2f90" by Ruby 2.3.1.
! Please do not edit this file directly.
!
! [JAPANESE]
!
! ※※※ 注意!!! ※※※
!
! このファイルは "gtdata_generic.rb2f90" から Ruby 2.3.1
! によって自動生成されたファイルです.
! このファイルを直接編集しませんようお願い致します.
!
!
!++
!
!= gtdata_variable.f90 - gtool 変数の手続総称宣言
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_generic.rb2f90,v 1.5 2009-10-11 07:36:37 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtdata_generic
  !
  !== GT_VARIABLE 型変数の手続総称宣言
  !
  ! ファイルの形式に依存しない多次元データである GT_VARIABLE 型変数
  ! の操作に関する手続きの全てはここで総称宣言されて公開されます。
  !
  ! 以降に一覧される手続は、第 1 引数が主な操作対象になるようなスタイルで
  ! 設計されています。 第 1 引数から gtool4 の手続であることがわかるので、
  ! 名前にわざわざ GT とはつけていません。
  !
  !== 種類別手続き一覧
  !
  ! ASCII 順の手続き一覧は下記の "Methods" を参照してください。
  !
  !=== 変数の作成・初期化・終了
  !
  ! Create            ::    変数の作成
  ! Open              ::    変数の初期化
  ! Close             ::    変数の終了処理
  !
  !=== 問い合わせ
  !
  ! Inquire           ::    変数または属性に関する問い合わせ
  !
  !=== データの入出力
  !
  ! Get               ::    ファイル入力
  ! Put, PutLine     ::    ファイル出力
  !
  !=== 次元順序番号の問い合わせ・変更
  !
  ! Dimname_To_Dimord ::    次元相対名から次元順序番号の問い合わせ
  ! Exch_dim          ::    次元順序番号の交換
  !
  !=== 変数構成写像機能（リミット、次元の加除）
  !
  ! 変数 (GT_VARIABLE 型) は順序の決まった次元集合をもちます。それぞれ
  ! の次元には決まった長さがあり、1から長さまでの番号で識別される格子から
  ! なっています。しかしながら、必要に応じて変数がもつ次元の順序を入れ替え
  ! たり、次元が存在しないようにみせかけたり、各々の次元の長さを変えたり格
  ! 子番号を付け替えたりできます。
  !
  ! Limit             ::    入出力範囲を拘束
  ! Add_Dim           ::    変数に次元を追加
  ! Del_Dim           ::    変数から次元を隠蔽
  ! Transform         ::    2 つの変数の次元配置の共通化
  !
  !=== 入出力範囲限定機能（スライス）
  !
  ! Slice             ::    入出力範囲を限定
  ! Slice_Next        ::    入出力範囲を移動
  ! Get_Slice         ::    変数入出力範囲限定情報を問い合わせ
  !
  !=== 属性関係
  !
  ! 属性名は英字、数字、下線から構成されます。先頭の文字は英字または
  ! "<b><tt>+</tt></b>" でなければなりません。
  ! "<b><tt>+</tt></b>" から始まる属性名は内部的に使用されるもの
  ! で、netCDF 実装では大域変数に対応します。
  !
  ! Attr_Rewind       ::    変数属性列挙の初期化
  ! Attr_Next         ::    変数属性の列挙
  ! Attr_True         ::    変数属性を論理型として読み取り
  ! Get_Attr          ::    変数属性の読み取り
  ! Put_Attr          ::    変数属性の書き出し
  ! Copy_Attr         ::    属性のコピー
  ! Del_Attr          ::    変数属性の削除
  !
  !=== その他
  !
  ! GTVarSearch       :: ファイルの中の変数名の列挙
  ! GTVarSync         :: ファイル入出力の同期
  ! GTDataTmpNam      :: 変数名の自動作成
  ! GTVarDel          :: ファイルの中の変数の削除 (作成中)
  ! operator(.equivalent.) [link:#M000133] :: 同値判定

  implicit none
  !
  ! 基本開閉動作
  !

  interface Open
    subroutine GTVarOpenByDimOrd(var, source_var, dimord, &
      count_compact, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(out):: var
      type(GT_VARIABLE), intent(in):: source_var
      integer, intent(in):: dimord
      logical, intent(in), optional:: count_compact
      logical, intent(out), optional:: err
    end subroutine GTVarOpenByDimOrd
    subroutine GTVarOpen(var, url, writable, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(out):: var
      character(*), intent(in):: url
      logical, intent(in), optional:: writable
      logical, intent(out), optional:: err
    end subroutine GTVarOpen
  end interface

  interface Create
    subroutine GTVarCreate(var, url, dims, xtype, long_name, overwrite, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(out):: var
      character(len = *), intent(in):: url
      type(GT_VARIABLE), intent(in), optional:: dims(:)
      character(len = *), intent(in), optional:: xtype
      character(len = *), intent(in), optional:: long_name
      logical, intent(in), optional:: overwrite
      logical, intent(out), optional:: err
    end subroutine GTVarCreate
    subroutine GTVarCreateD(var, url, length, xtype, long_name, overwrite, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(out):: var
      character(len = *), intent(in):: url
      integer, intent(in):: length
      character(len = *), intent(in), optional:: xtype
      character(len = *), intent(in), optional:: long_name
      logical, intent(in), optional:: overwrite
      logical, intent(out), optional:: err
    end subroutine GTVarCreateD
  end interface Create

  interface Close
    subroutine GTVarClose(var, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      logical, intent(out), optional:: err
    end subroutine GTVarClose
  end interface Close

  !
  ! --- 属性関係 ---
  !

  interface Attr_Rewind
    subroutine GTVarAttrRewind(var)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var
    end subroutine GTVarAttrRewind
  end interface Attr_Rewind

  interface Attr_Next
    subroutine GTVarAttrNext(var, name, end)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var
      character(*), intent(out):: name
      logical, intent(out), optional:: end
    end subroutine GTVarAttrNext
  end interface Attr_Next

  ! 論理型読み取りは関数インターフェイスも提供
  interface Attr_True
    logical function GTVarAttrTrue(var, name, default)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      logical, intent(in), optional:: default
    end function GTVarAttrTrue
  end interface Attr_True

  interface Get_Attr
    ! スカラで受け取るのが一番簡単。解釈可能な値がとられ、残りは捨てられる。
    subroutine GTVarGetAttrI(var, name, value, default)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      integer, intent(out):: value
      integer, intent(in), optional:: default
    end subroutine GTVarGetAttrI
    subroutine GTVarGetAttrR(var, name, value, default)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real, intent(out):: value
      real, intent(in), optional:: default
    end subroutine GTVarGetAttrR

    subroutine GTVarGetAttrD(var, name, value, default)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: DP
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real(DP), intent(out):: value
      real(DP), intent(in), optional:: default
    end subroutine GTVarGetAttrD

    ! ポインタ配列を使って受け取る場合は解釈可能な数だけ実体が割り付けられる。
    subroutine GTVarGetAttrIP(var, name, value)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      integer, pointer:: value(:)
    end subroutine GTVarGetAttrIP

    subroutine GTVarGetAttrRP(var, name, value)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real, pointer:: value(:)
    end subroutine GTVarGetAttrRP

    subroutine GTVarGetAttrDP(var, name, value)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: DP
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real(DP), pointer:: value(:)
    end subroutine GTVarGetAttrDP

    ! integer 配列, real 配列として受け取る
    ! 場合は属性長があまっている場合には切り捨てられ、
    ! 属性長が足りない場合は default 値 (ポインタと違い必須) を埋める。

    subroutine GTVarGetAttrIA(var, name, value, default)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      integer, intent(out):: value(:)
      integer, intent(in):: default
    end subroutine GTVarGetAttrIA

    subroutine GTVarGetAttrRA(var, name, value, default)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real, intent(out):: value(:)
      real, intent(in):: default
    end subroutine GTVarGetAttrRA

    subroutine GTVarGetAttrDA(var, name, value, default)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: DP
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real(DP), intent(out):: value(:)
      real(DP), intent(in):: default
    end subroutine GTVarGetAttrDA

    ! character 型で受け取る場合は通常の文字型代入と同様、
    ! 受け側変数の長さに合わせて切り捨て・空白埋めを行う。
    ! 属性が存在しない場合 default 値を使う。

    subroutine GTVarGetAttrCC(var, name, value, default)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      character(len = *), intent(out):: value
      character(len = *), intent(in), optional:: default
    end subroutine GTVarGetAttrCC

    ! 文字列として読み取る場合は元の長さは正確に受け取られる。
    ! 属性が存在しない場合 default 値を使う。

!    subroutine GTVarGetAttrSC(var, name, value, default)
!      use dc_string, only: VSTRING
!      use gtdata_types, only: GT_VARIABLE
!      type(GT_VARIABLE), intent(in):: var
!      character(len = *), intent(in):: name
!      type(VSTRING), intent(out):: value
!      character(len = *), intent(in), optional:: default
!    end subroutine GTVarGetAttrSC

  end interface Get_Attr

  interface Put_Attr
    subroutine GTVarPutAttrDouble(var, name, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: DP
      real(DP),           intent(in)            :: value(:)
      real,               intent(out), optional :: err
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: name
    end subroutine GTVarPutAttrDouble
    subroutine GTVarPutAttrReal(var, name, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: SP
      real(SP),           intent(in)            :: value(:)
      real(SP),           intent(out), optional :: err
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: name
    end subroutine GTVarPutAttrReal
    subroutine GTVarPutAttrInt(var, name, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,            intent(in)            :: value(:)
      real,               intent(out), optional :: err
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: name
    end subroutine GTVarPutAttrInt
    subroutine GTVarPutAttrChar(var, name, value, xtype, err)
      use gtdata_types, only: GT_VARIABLE
      character(len = *), intent(in)            :: value
      logical,            intent(out), optional :: err
      character(len = *), intent(in),  optional :: xtype
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: name
    end subroutine GTVarPutAttrChar
    subroutine GTVarPutAttrLogical(var, name, value, err)
      use gtdata_types, only: GT_VARIABLE
      logical,            intent(in)            :: value
      real,               intent(out), optional :: err
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: name
    end subroutine GTVarPutAttrLogical
  end interface Put_Attr

  interface Del_Attr
    subroutine GTVarDelAttr(var, name, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: name
      logical, intent(out), optional:: err
    end subroutine GTVarDelAttr
  end interface Del_Attr

  interface Copy_Attr
    subroutine GTVarCopyAttr(to, attrname, from, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: to
      character(len = *), intent(in):: attrname
      type(GT_VARIABLE), intent(in):: from
      logical, intent(out), optional:: err
    end subroutine GTVarCopyAttr
    subroutine GTVarCopyAttrAll(to, from, err, global)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: to
      type(GT_VARIABLE), intent(inout):: from
      logical, intent(out), optional:: err
      logical, intent(in), optional:: global
    end subroutine GTVarCopyAttrAll
  end interface Copy_Attr

  !
  ! --- 次元関係 ---
  !

  ! 現在の入出力範囲の大きさは inquire(var, size=) で取得できる

  interface Slice
    ! おまかせ指示
    ! compatible を指定するとまったく同じに指定
    subroutine GTVarSliceAuto(var, compatible)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var
      type(GT_VARIABLE), intent(in), optional:: compatible
    end subroutine GTVarSliceAuto
    ! 次元を指定した指示
    subroutine GTVarSlice(var, dimord, start, count, stride)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: dimord
      integer, intent(in), optional:: start
      integer, intent(in), optional:: count
      integer, intent(in), optional:: stride
    end subroutine GTVarSlice
    ! 文字列による指示
    subroutine GTVarSliceC(var, string, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: string
      logical, intent(out):: err
    end subroutine GTVarSliceC
  end interface Slice

  interface Get_Slice
    ! 次元順番を指定して取得
    subroutine GTVarGetSlice(var, dimord, start, count, stride, count_compact)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: dimord
      integer, intent(out), optional:: start
      integer, intent(out), optional:: count
      integer, intent(out), optional:: stride
      logical, intent(in), optional:: count_compact
    end subroutine GTVarGetSlice
    ! 全次元について一括取得
    ! あらかじめ inquire(var, alldims) して配列を確保する。
    subroutine GTVarGetSliceAll(var, start, count, stride)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(out), optional:: start(:), count(:), stride(:)
    end subroutine GTVarGetSliceAll
  end interface Get_Slice

  interface Dimname_to_Dimord
    integer function GTVarDimName2Ord(var, name)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
    end function GTVarDimName2Ord
  end interface Dimname_to_Dimord

  ! limit: 空間変換器機能のセットアップ
  !
  interface Limit
    subroutine GTVarLimit(var, string, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout) :: var
      character(len = *), intent(in)   :: string
      logical, intent(out), optional   :: err
    end subroutine GTVarLimit
    subroutine GTVarLimit_iiii(var, dimord, start, count, stride, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var
      integer, intent(in)             :: dimord
      integer, intent(in) , optional  :: start, count, stride
      logical, intent(out), optional  :: err
    end subroutine GTVarLimit_iiii
  end interface Limit

  interface Transform
    subroutine GTVarXformBinary(var1, var2, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var1, var2
      logical, intent(out), optional:: err
    end subroutine GTVarXformBinary
  end interface Transform

  ! 明示的空間変換機能

  ! 変数ハンドル var の dimord 番目の位置に次元
  ! dimvar を追加する。dimord 番目以降の次元は一つ後ろにずれる。
  ! もし dimord が var の有効次元数より大きければ (有効次元数 + 1)
  ! が与えられたものとみなされる。
  interface Add_dim
    subroutine GTVarAddDim(var, dimord, dimvar, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      type(GT_VARIABLE), intent(in):: dimvar
      integer, intent(in):: dimord
      logical, intent(out):: err
    end subroutine gtvaradddim
  end interface Add_dim

  ! 変数ハンドルから次元を「除去」する。実際には、
  ! 次元対応表の順位を下げ有効次元数をデクリメントするだけなので、
  ! 入出力に支障はない。
  interface Del_dim
    subroutine GTVarDelDim(var, dimord, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: dimord
      logical, intent(out):: err
    end subroutine gtvardeldim
  end interface Del_dim

  ! 次元対応表の順位の交換
  interface Exch_dim
    subroutine GTVarExchDim(var, dimord1, dimord2, count_compact, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: dimord1, dimord2
      logical, intent(in), optional:: count_compact
      logical, intent(out):: err
    end subroutine gtvarexchdim
  end interface Exch_dim

  !
  ! --- 問い合わせ ---
  !

  interface Inquire
    subroutine GTVarInquire(var, growable, &
      & rank, alldims, allcount, size, &
      & xtype, name, url, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len=*), intent(out), optional:: xtype
      character(len=*), intent(out), optional:: name
      character(len=*), intent(out), optional:: url
      integer, intent(out), optional:: rank
      integer, intent(out), optional:: alldims
      integer, intent(out), optional:: allcount
      integer, intent(out), optional:: size
      logical, intent(out), optional:: growable
      logical, intent(out), optional:: err
    end subroutine GTVarInquire
    subroutine GTVarInquire2(var, allcount)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(out):: allcount(:)
    end subroutine GTVarInquire2
    subroutine GTVarInquireD(var, dimord, url, allcount, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: dimord
      character(len=*), intent(out), optional:: url
      integer, intent(out), optional:: allcount
      logical, intent(out), optional:: err
    end subroutine GTVarInquireD
    subroutine GTVarInquireA(var, attrname, xtype)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      character(len=*), intent(in):: attrname
      character(len=*), intent(out), optional:: xtype
    end subroutine GTVarInquireA
  end interface Inquire

  ! 総なめ用イテレータ
  interface Slice_Next
    subroutine GTVarSliceNext(var, dimord, err, stat)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in out):: var
      integer, intent(in), optional:: dimord
      logical, intent(out), optional:: err
      integer, intent(out), optional:: stat
    end subroutine GTVarSliceNext
  end interface Slice_Next

  ! 同値判定
  interface operator(.equivalent.)
    logical function gtvarequivalent(var1, var2)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var1, var2
    end function gtvarequivalent
  end interface operator(.equivalent.)

  !
  !== 入出力
  !
  !=== 入力
  !
  ! Get(var, value, nvalue, [err])
  ! 1次元配列に現在の入出力範囲を取得する。
  ! nvalue は利用者が配列長を格納しなければならない。
  ! 個別名 GTVarGetReal を用いると多次元配列に入力
  ! することもできるが、入出力範囲との関係に注意が必要。
  !
  ! Get(var, value, [err]) は 1〜 7 次元のポインタをとる。
  ! 入出力範囲をポインタ次元数に限定し、配列を確保し、
  ! 値を入れて返す。
  !

  interface Get
    subroutine GTVarGetDouble(var, value, nvalue, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: DP
      real(DP), intent(out):: value(*)
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: nvalue
      logical, intent(out), optional:: err
    end subroutine GTVarGetDouble
    subroutine GTVarGetPointerDouble1(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: DP
      real(DP),          pointer               :: value(:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerDouble1
    subroutine GTVarGetPointerDouble2(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: DP
      real(DP),          pointer               :: value(:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerDouble2
    subroutine GTVarGetPointerDouble3(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: DP
      real(DP),          pointer               :: value(:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerDouble3
    subroutine GTVarGetPointerDouble4(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: DP
      real(DP),          pointer               :: value(:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerDouble4
    subroutine GTVarGetPointerDouble5(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: DP
      real(DP),          pointer               :: value(:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerDouble5
    subroutine GTVarGetPointerDouble6(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: DP
      real(DP),          pointer               :: value(:,:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerDouble6
    subroutine GTVarGetPointerDouble7(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: DP
      real(DP),          pointer               :: value(:,:,:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerDouble7
    subroutine GTVarGetReal(var, value, nvalue, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: SP
      real(SP), intent(out):: value(*)
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: nvalue
      logical, intent(out), optional:: err
    end subroutine GTVarGetReal
    subroutine GTVarGetPointerReal1(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: SP
      real(SP),          pointer               :: value(:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerReal1
    subroutine GTVarGetPointerReal2(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: SP
      real(SP),          pointer               :: value(:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerReal2
    subroutine GTVarGetPointerReal3(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: SP
      real(SP),          pointer               :: value(:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerReal3
    subroutine GTVarGetPointerReal4(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: SP
      real(SP),          pointer               :: value(:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerReal4
    subroutine GTVarGetPointerReal5(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: SP
      real(SP),          pointer               :: value(:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerReal5
    subroutine GTVarGetPointerReal6(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: SP
      real(SP),          pointer               :: value(:,:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerReal6
    subroutine GTVarGetPointerReal7(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types,     only: SP
      real(SP),          pointer               :: value(:,:,:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerReal7
    subroutine GTVarGetInt(var, value, nvalue, err)
      use gtdata_types, only: GT_VARIABLE
      integer,  intent(out):: value(*)
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: nvalue
      logical, intent(out), optional:: err
    end subroutine GTVarGetInt
    subroutine GTVarGetPointerInt1(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,           pointer               :: value(:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerInt1
    subroutine GTVarGetPointerInt2(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,           pointer               :: value(:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerInt2
    subroutine GTVarGetPointerInt3(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,           pointer               :: value(:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerInt3
    subroutine GTVarGetPointerInt4(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,           pointer               :: value(:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerInt4
    subroutine GTVarGetPointerInt5(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,           pointer               :: value(:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerInt5
    subroutine GTVarGetPointerInt6(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,           pointer               :: value(:,:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerInt6
    subroutine GTVarGetPointerInt7(var, value, err)
      use gtdata_types, only: GT_VARIABLE
      integer,           pointer               :: value(:,:,:,:,:,:,:)
      type(GT_VARIABLE), intent(in)            :: var
      logical,           intent(out), optional :: err
    end subroutine GTVarGetPointerInt7
  end interface Get

  !
  ! --- 印字 ---
  !
  ! 印字というのは変数にしてみれば入力だしファイルにしてみれば出力だ。
  ! だから put と get を兼営しているようなものでまことにあやしいのだが、
  ! いちおう dc_string の手続名を継承して put_line にしておく。

  interface PutLine
    subroutine GTVarPutLine( var, unit, indent, err )
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in), optional:: unit
      character(*), intent(in), optional:: indent
      logical, intent(out), optional:: err
    end subroutine GTVarPutLine
  end interface

  !
  ! --- 出力 ---
  !
  interface Put
    subroutine GTVarPutDouble(var, value, nvalue, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: DP
      real(DP), intent(in):: value(nvalue)
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: nvalue
      logical, intent(out), optional:: err
    end subroutine GTVarPutDouble
    subroutine GTVarPutDouble1(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: DP
      real(DP), intent(in) :: value(:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutDouble1
    subroutine GTVarPutDouble2(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: DP
      real(DP), intent(in) :: value(:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutDouble2
    subroutine GTVarPutDouble3(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: DP
      real(DP), intent(in) :: value(:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutDouble3
    subroutine GTVarPutDouble4(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: DP
      real(DP), intent(in) :: value(:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutDouble4
    subroutine GTVarPutDouble5(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: DP
      real(DP), intent(in) :: value(:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutDouble5
    subroutine GTVarPutDouble6(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: DP
      real(DP), intent(in) :: value(:,:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutDouble6
    subroutine GTVarPutDouble7(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: DP
      real(DP), intent(in) :: value(:,:,:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutDouble7
    subroutine GTVarPutReal(var, value, nvalue, err)
      use gtdata_types, only: GT_VARIABLE
      use dc_types, only: SP
      real(SP), intent(in):: value(nvalue)
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: nvalue
      logical, intent(out), optional:: err
    end subroutine GTVarPutReal
    subroutine GTVarPutReal1(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: SP
      real(SP), intent(in) :: value(:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutReal1
    subroutine GTVarPutReal2(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: SP
      real(SP), intent(in) :: value(:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutReal2
    subroutine GTVarPutReal3(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: SP
      real(SP), intent(in) :: value(:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutReal3
    subroutine GTVarPutReal4(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: SP
      real(SP), intent(in) :: value(:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutReal4
    subroutine GTVarPutReal5(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: SP
      real(SP), intent(in) :: value(:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutReal5
    subroutine GTVarPutReal6(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: SP
      real(SP), intent(in) :: value(:,:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutReal6
    subroutine GTVarPutReal7(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      use dc_types,       only: SP
      real(SP), intent(in) :: value(:,:,:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutReal7
    subroutine GTVarPutInt(var, value, nvalue, err)
      use gtdata_types, only: GT_VARIABLE
      integer, intent(in):: value(nvalue)
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: nvalue
      logical, intent(out), optional:: err
    end subroutine GTVarPutInt
    subroutine GTVarPutInt1(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      integer,  intent(in) :: value(:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutInt1
    subroutine GTVarPutInt2(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      integer,  intent(in) :: value(:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutInt2
    subroutine GTVarPutInt3(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      integer,  intent(in) :: value(:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutInt3
    subroutine GTVarPutInt4(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      integer,  intent(in) :: value(:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutInt4
    subroutine GTVarPutInt5(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      integer,  intent(in) :: value(:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutInt5
    subroutine GTVarPutInt6(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      integer,  intent(in) :: value(:,:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutInt6
    subroutine GTVarPutInt7(var, value, err)
      use gtdata_types,   only: GT_VARIABLE
      integer,  intent(in) :: value(:,:,:,:,:,:,:)
      type(GT_VARIABLE),intent(in):: var
      logical  ,intent(out), optional:: err
    end subroutine GTVarPutInt7
    subroutine GTVarPutChar(var, value, nvalue, err)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(in):: var
      integer, intent(in):: nvalue
      character(*), intent(in):: value(nvalue)
      logical, intent(out), optional:: err
    end subroutine GTVarPutChar
  end interface Put

  !
  ! === 変数とファイルの関係 ===
  !

  interface GTVarSearch
    subroutine GTVarSearchNext(url, end)
      character(len = *), intent(out):: url
      logical, intent(out):: end
    end subroutine GTVarSearchNext
    subroutine GTVarSearchInit(urlBase)
      character(len = *), intent(in):: urlBase
    end subroutine GTVarSearchInit
  end interface GTVarSearch

  !
  ! === 安全のための脱出口 ===
  !
  ! SysDepAbort が呼び出すので、StoreError を呼んではならない
  !

  interface
    subroutine GTVarSync(var, stat)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout), optional:: var
      integer, intent(out), optional:: stat
    end subroutine GTVarSync
  end interface

  !
  ! === ユーティリティ ===
  !
  ! gtdata 層だけで書かれたライブラリ。

  interface Create
    subroutine GTVarCreateCopyC(var, url, copyfrom, copyvalue, &
      & overwrite, err)
      use gtdata_types, only: GT_VARIABLE
      implicit none
      type(GT_VARIABLE), intent(out):: var
      character(len = *), intent(in):: url
      type(GT_VARIABLE), intent(inout):: copyfrom
      logical, intent(in), optional:: copyvalue
      logical, intent(in), optional:: overwrite
      logical, intent(out), optional:: err
    end subroutine GTVarCreateCopyC
  end interface

  !
  ! 変数の削除 (作成中)
  !
  interface GTVarDel
    subroutine GTVarDel1(varname, err)
      character(len = *), intent(in):: varname
      logical, intent(out):: err
    end subroutine GTVarDel1
  end interface

  !
  ! --- 構造体関係 ---
  !

  interface Add_member
    subroutine GTVarAddMember(var, url, link_name)
      use gtdata_types, only: GT_VARIABLE
      type(GT_VARIABLE), intent(inout):: var
      character(len = *), intent(in):: url
      character(len = *), intent(in), optional:: link_name
    end subroutine GTVarAddMember
  end interface

  ! 一意な変数名の自動作成
  interface GTDataTmpNam
    subroutine GTVarTmpNam(file, base, result)
      character(len = *), intent(in):: file
      character(len = *), intent(in):: base
      character(len = *), intent(out):: result
    end subroutine GTVarTmpNam
  end interface

end module gtdata_generic
