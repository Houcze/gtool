!
!== 抽象 (abstruct) netCDF インターフェイスの総称宣言
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtdata_netcdf_generic.f90,v 1.2 2009-10-11 07:36:33 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtdata_netcdf_generic
  !
  !== 抽象 netCDF インターフェイスの総称宣言
  !

  implicit none
  !
  ! === 基本開閉動作 ===
  !

  !
  ! an ライブラリでは「ファイル」ではなく「変数」を開いたり閉じたりする。
  ! すべてのものは変数とその属性である。

  interface Open
    ! 
    ! open(var, url, [writable], [err]) は url で識別される
    ! 変数を開き var に格納する。ここで変数とは netCDF 変数または
    ! netCDF 次元である。次元と同名の netCDF 変数がある場合には両者は
    ! 同一視される。writable を真に指定すると書き込み可で開こうとする。
    ! デフォルトは writable=.FALSE. である。エラーが発生した場合は
    ! err が真となる。err を与えなければプログラムは停止する。
    !
    recursive subroutine GDNcVarOpen(var, url, writable, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(out):: var
      character(len = *), intent(in):: url
      logical, intent(in), optional:: writable
      logical, intent(out), optional:: err
    end subroutine GDNcVarOpen
    !
    ! gdnc ライブラリの変数は netCDF の変数と次元を統合したもの
    ! である。次元構造体は存在しない。そこで、gdnc の立場では
    ! 変数は任意個の次元と呼ばれる変数を持つということになる。
    ! 従来次元変数と呼ばれていたものは、変数が自分自身のみを
    ! 次元としてもつ場合を指す。
    !
    ! open(var, src_var, dimord, [count_compact], [err]) は
    ! 既に開かれた変数 src_var の ord 番目の次元にあたる変数を
    ! 開き var に格納する。順序 ord は現在の入出力範囲が
    ! 幅１になっている (コンパクト化している）を飛ばした
    ! 順序であるが、count_compact に真を指定すると
    ! すべての次元のなかの順序になる。
    !
    subroutine GDNcVarOpenByDimOrd(var, src_var, dimord, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(out):: var
      type(GD_NC_VARIABLE), intent(in):: src_var
      integer, intent(in):: dimord
      logical, intent(out), optional:: err
    end subroutine GDNcVarOpenByDimOrd
  end interface

  interface Search_dim
    integer function GDNcSearchDim(var, dimname)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: dimname
    end function GDNcSearchDim
  end interface

  interface Create
    !
    ! 従属変数 create
    !
    ! create(var, url, dims, [xtype], [overwrite], [err]) は
    ! 場所 url に次元 dims を持った変数を作成し、それを開いた
    ! ものを var に格納する。型 xtype を省略すると real と
    ! みなされる。既存変数があるとき失敗するが
    ! overwrite が真であれば続行する。
    ! ゼロ次元変数を作るには dims に長さゼロの配列を渡すこと。
    !
    subroutine GDNcVarCreate(var, url, xtype, dims, overwrite, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(out):: var
      character(len = *), intent(in):: url
      character(len = *), intent(in):: xtype
      type(GD_NC_VARIABLE), intent(in):: dims(:)
      logical, intent(in), optional:: overwrite
      logical, intent(out), optional:: err
    end subroutine GDNcVarCreate
    !
    ! 次元変数 create
    !
    ! create(var, url, xtype, length, [overwrite], [err]) は
    ! 長さ length の次元変数を作成する。
    !
    subroutine GDNcVarCreateD(var, url, xtype, length, overwrite, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(out):: var
      character(len = *), intent(in):: url
      character(len = *), intent(in):: xtype
      integer, intent(in):: length
      logical, intent(in), optional:: overwrite
      logical, intent(out), optional:: err
    end subroutine GDNcVarCreateD
  end interface

  interface
    subroutine GDNcVarDel(varname, hint, stat)
      character(len = *), intent(in):: varname
      character(len = *), intent(in):: hint
      integer, intent(out):: stat
    end subroutine GDNcVarDel
  end interface

  interface Close
    subroutine GDNcVarClose(var, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      logical, intent(out), optional:: err
    end subroutine GDNcVarClose
  end interface

  !
  ! === 変数に関する問い合わせ万般 ===
  !

  ! mainly for PRINT debugging
  interface toString
    function GDNcVarToString(var) result(result)
      use dc_types, only: STRING
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      character(string):: result
      type(GD_NC_VARIABLE), intent(in):: var
    end function GDNcVarToString
  end interface

  interface Inquire

    subroutine GDNcVarInquire(var, ndims, dimlen, growable, name, url, xtype)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      ! 変数の次元数
      integer, intent(out), optional:: ndims
      ! 変数が１次元である場合、次元長
      integer, intent(out), optional:: dimlen
      ! 変数が成長可能次元を持つか
      logical, intent(out), optional:: growable
      ! 文字型引数が短いと値の切り詰めが起こりうる
      ! '?' のあとの変数名
      character(*), intent(out), optional:: name
      ! 変数名、少なくともファイル名を含む、なるべく長い名前
      character(*), intent(out), optional:: url
      ! 変数の型名
      character(*), intent(out), optional:: xtype
    end subroutine GDNcVarInquire

    ! 引数は ndims 個でなければならない
    subroutine GDNcVarInquireIA(var, dimlen)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(out):: dimlen(:)
    end subroutine GDNcVarInquireIA

    subroutine GDNcAttrInquire(var, attrname, xtype)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len=*), intent(in):: attrname
      character(len=*), intent(out), optional:: xtype
    end subroutine GDNcAttrInquire

  end interface

  interface InquirePlus

    subroutine GDNcAttrInquirePlus(var, attrname, varid, nf_attrname)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len=*), intent(in):: attrname
      integer, intent(out):: varid
      character(len=*), intent(out):: nf_attrname
    end subroutine GDNcAttrInquirePlus

  end interface

  !
  ! === 属性関係 ===
  !

  ! すべては変数という an ライブラリの立場から、
  ! 大域属性は変数属性とみなされる。
  ! 属性読み取り時には変数属性の次に大域属性を検索する。
  ! 属性書き込み時には一部の例外を除き変数属性として書き込む。
  ! gtool4 規約で大域属性と規定された属性 Conventions,
  ! gt_version, title, gt_subtitle, comment, source,
  ! institution, production, history についてはまず
  ! 大域属性として書き込もうとする。既存の値があった
  ! 場合 Conventions, gt_version は gtool4 同士ならば
  ! 版数が大きくなるようにする。history は規定どおり
  ! 追加動作を行う。その他の属性名については既存と異なる
  ! 属性値があれば変数属性として書き込む。

  !
  ! --- 属性の列挙 ---
  !

  ! ある変数 var に付随した属性をすべて取得するにはまず
  ! attr_rewind(var) を呼び出してから無限ループの中で
  ! attr_next(var, name, [end]) を呼び出す。name がひとつ
  ! ひとつの属性名を与える。name が空文字列になったとき、
  ! すべての属性を探索し終えたことになる。このとき end を
  ! 与えていればそれが真になることでも判定できる。

  interface attr_rewind
    subroutine GDNcVarAttrRewind(var)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
    end subroutine GDNcVarAttrRewind
  end interface

  interface attr_next
    subroutine GDNcVarAttrNext(var, name, end)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(out):: name
      logical, intent(out), optional:: end
    end subroutine GDNcVarAttrNext
  end interface

  ! 変数 var の属性 name を取得して value に格納する。
  ! 属性が存在しないか value の長さが不足している場合
  ! default が補われる。属性の型はなんでもよく型変換をする。

  interface get_attr

    subroutine GDNcAttrGetChar(var, name, value, default, stat)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      character(len = *), intent(out):: value
      character(len = *), intent(in):: default
      integer, intent(out):: stat
    end subroutine GDNcAttrGetChar

    subroutine GDNcVarGetAttrLogical(var, name, value, default)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      logical, intent(out):: value
      logical, intent(in), optional:: default
    end subroutine GDNcVarGetAttrLogical

    ! お客様向きではないけれど、情報落ちのないインターフェイスということで....
    ! stat = -1:  その属性は存在しなかった
    ! stat = 0 ... size(value):  その属性を全部読み取った。サイズは stat 個
    ! stat > size(value):  配列長不足のため属性が全部読み取れなかった。
    !                      サイズは stat 個必要

    subroutine GDNcAttrGetReal(var, name, value, stat, default)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real, intent(out):: value(:)
      integer, intent(out):: stat
      real, intent(in), optional:: default
    end subroutine GDNcAttrGetReal

    subroutine GDNcAttrGetDouble(var, name, value, stat, default)
      use dc_types, only: DP
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real(DP), intent(out):: value(:)
      integer, intent(out):: stat
      real(DP), intent(in), optional:: default
    end subroutine GDNcAttrGetDouble

    subroutine GDNcAttrGetInt(var, name, value, stat, default)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      integer, intent(out):: value(:)
      integer, intent(out):: stat
      integer, intent(in), optional:: default
    end subroutine GDNcAttrGetInt

  end interface

  ! 変数 var の属性 name に value を格納する。
  ! 属性の型は value の型に適合するように設定される。
  ! 論理型は 1 文字の文字型として格納され、真は T,
  ! 偽は F となる。

  interface Put_Attr

    subroutine GDNcVarPutAttrReal(var, name, value, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real, intent(in):: value(:)
      logical, intent(out), optional:: err
    end subroutine GDNcVarPutAttrReal

    subroutine GDNcVarPutAttrDouble(var, name, value, err)
      use dc_types, only: DP
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      real(DP), intent(in):: value(:)
      logical, intent(out), optional:: err
    end subroutine GDNcVarPutAttrDouble

    subroutine GDNcVarPutAttrInt(var, name, value, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      integer, intent(in):: value(:)
      logical, intent(out), optional:: err
    end subroutine GDNcVarPutAttrInt

    subroutine GDNcVarPutAttrLogical(var, name, value, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      logical, intent(in):: value
      logical, intent(out), optional:: err
    end subroutine GDNcVarPutAttrLogical

    subroutine GDNcVarPutAttrChar(var, name, value, xtype, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      character(len = *), intent(in):: value
      character(len = *), intent(in), optional:: xtype
      logical, intent(out), optional:: err
    end subroutine GDNcVarPutAttrChar

  end interface

  interface del_attr
    subroutine GDNcVarDelAttr(var, name, err)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      logical, intent(out), optional:: err
    end subroutine GDNcVarDelAttr
  end interface

  interface copy_attr
    subroutine GDNcVarAttrCopy(to, attrname, from, stat)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: to
      character(len = *), intent(in):: attrname
      type(GD_NC_VARIABLE), intent(in):: from
      integer, intent(out):: stat
    end subroutine GDNcVarAttrCopy
  end interface

  !
  ! 入出力
  !

  interface Get

    subroutine GDNcVarGetReal(var, start, count, stride, imap, &
      & siz, value, iostat)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(in):: start(:)
      integer, intent(in):: count(:)
      integer, intent(in):: stride(:)
      integer, intent(in):: imap(:)
      integer, intent(in):: siz
      real, intent(out):: value(siz)
      integer, intent(out):: iostat
    end subroutine GDNcVarGetReal

    subroutine GDNcVarGetDouble(var, start, count, stride, imap, &
      & siz, value, iostat)
      use dc_types, only: DP
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(in):: start(:)
      integer, intent(in):: count(:)
      integer, intent(in):: stride(:)
      integer, intent(in):: imap(:)
      integer, intent(in):: siz
      real(DP), intent(out):: value(siz)
      integer, intent(out):: iostat
    end subroutine GDNcVarGetDouble

    subroutine GDNcVarGetInt(var, start, count, stride, imap, &
      & siz, value, iostat)
      use dc_types, only: DP
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(in):: start(:)
      integer, intent(in):: count(:)
      integer, intent(in):: stride(:)
      integer, intent(in):: imap(:)
      integer, intent(in):: siz
      integer, intent(out):: value(siz)
      integer, intent(out):: iostat
    end subroutine GDNcVarGetInt

  end interface

  interface Put

    subroutine GDNcVarPutReal(var, start, count, stride, imap, &
      & siz, value, iostat)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(in):: start(:)
      integer, intent(in):: count(:)
      integer, intent(in):: stride(:)
      integer, intent(in):: imap(:)
      integer, intent(in):: siz
      real, intent(in):: value(siz)
      integer, intent(out):: iostat
    end subroutine GDNcVarPutReal

    subroutine GDNcVarPutDouble(var, start, count, stride, imap, &
      & siz, value, iostat)
      use dc_types, only: DP
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(in):: start(:)
      integer, intent(in):: count(:)
      integer, intent(in):: stride(:)
      integer, intent(in):: imap(:)
      integer, intent(in):: siz
      real(DP), intent(in):: value(siz)
      integer, intent(out):: iostat
    end subroutine GDNcVarPutDouble

    subroutine GDNcVarPutInt(var, start, count, stride, imap, &
      & siz, value, iostat)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(in):: start(:)
      integer, intent(in):: count(:)
      integer, intent(in):: stride(:)
      integer, intent(in):: imap(:)
      integer, intent(in):: siz
      integer, intent(in):: value(siz)
      integer, intent(out):: iostat
    end subroutine GDNcVarPutInt

    subroutine GDNcVarPutChar(var, start, count, stride, imap, &
      & siz, value, iostat)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in):: var
      integer, intent(in):: start(:)
      integer, intent(in):: count(:)
      integer, intent(in):: stride(:)
      integer, intent(in):: imap(:)
      integer, intent(in):: siz
      character(*), intent(in):: value(siz)
      integer, intent(out):: iostat
    end subroutine GDNcVarPutChar

  end interface

  !
  ! === ファイル名から変数をさがす ===
  !

  interface var_search
        
    subroutine GDNcVarSearchInit(iter, urlBase)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE_SEARCH
      type(GD_NC_VARIABLE_SEARCH), intent(out):: iter
      character(len = *), intent(in):: urlBase
    end subroutine GDNcVarSearchInit

    subroutine GDNcVarSearchNext(iter, url, end)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE_SEARCH
      type(GD_NC_VARIABLE_SEARCH), intent(inout):: iter
      character(len = *), intent(out):: url
      logical, intent(out):: end
    end subroutine GDNcVarSearchNext

  end interface

  !
  ! 非常脱出用。このルーチンは SysDepAbort からも呼ばれるため、
  ! 自分で StoreError することはない。
  !

  interface
    subroutine GDNcVarSync(var, stat)
      use gtdata_netcdf_types, only: GD_NC_VARIABLE
      type(GD_NC_VARIABLE), intent(in), optional:: var
      integer, intent(out), optional:: stat
    end subroutine GDNcVarSync
  end interface

  !
  ! an 層の内部的使用のためのルーチン
  !
  interface
    subroutine GDNcXTypeName(ixtype, xtype)
      integer, intent(in):: ixtype
      character(*), intent(out):: xtype
    end subroutine GDNcXTypeName
  end interface

end module gtdata_netcdf_generic
