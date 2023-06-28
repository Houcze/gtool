=begin JA

= gtool5 ライブラリ概説

# * 森川 靖大 (morikawa), 豊田 英司 (toyoda)
#   * $Id: lib-overview.rd,v 1.5 2009-05-29 16:08:40 morikawa Exp $

=end JA

=begin JA

本文書は gtool5 ライブラリの構成と保守に必要な知識を概観します。

== 言語規格

((*Fortran 95 規格*)) に準拠するよう記述しています。

=== 例外

ただし、以下の点で例外もいくつかあります。
その場合にも、sysdep (詳細は下記参照) で処理系依存を吸収し、
たいていの処理系では動作するようにしています。

  * コマンドライン引数取得
    * 規格に含まれていない GETARG、IARG を、
      処理系がサポートしていれば使用します。
    * Fortran 2003 規格の command_argument_count、
      get_command_argument を、
      処理系がサポートしていれば使用します。

  * 環境変数取得
    * 規格に含まれていない GETENV を、
      処理系がサポートしていれば使用します。
    * Fortran 2003 規格の get_environment_variable を、
      処理系がサポートしていれば使用します。

  * プログラム異常終了
    * 規格に含まれていないトレースバック機能を有した
      プログラム異常終了サービスルーチンを、
      処理系がサポートしていれば使用します。
      * 今のところは、Fujitsu Fortran のみに対応しています。

== モジュール一覧

gtool5 ライブラリは多次元数値データの抽象的インターフェイスや、
数値モデル一般で使用するユーティリティを提供します。

=end JA

=begin HTMLJA
<p align="center">
  <img src="images/gtool5_overview.png" border="0" alt="gtool5 Overview" />
</p>

<p align="center">
  図1: gtool5 の概観。枠内に記述されるのはモジュールの名称です。
  矢印は参照関係を示します。実際には「汎用ツール」へは、
  他のすべてのモジュールから参照されていますが、
  図が煩雑になるためその矢印は省略しています。
  また、数が多いため、
  「汎用ツール」のうちのいくつかのモジュールはここに記されていません。
  なお、MPI Library への参照は、gtool5 を MPI 版でビルドする場合のみ行われます。
  <small> [
  <!-- <a href="images/gtool5_overview.pdf">PDF</a> | -->
  <a href="images/gtool5_overview.odg">OpenOffice Draw Src</a>
  ]
  </small>
</p>
=end HTMLJA

=begin JA

gtool5 は、Fortran 90/95 で数値モデルを作成するユーザに、
((<gtool4 netCDF 規約|URL:xref.htm#label-6>))
に基づく形式のデータを出力するための簡易インターフェースの提供、
および数値モデル一般で使用されることを想定した汎用的な関数やサブルーチンを提供します。
以下では、図1で示される各モジュールについて簡単な解説を行います。

* gtool_history

  ユーザへ簡易なインターフェースを提供します。 ユーザは、
  作成する数値モデル中でこのライブラリ内のサブルーチンを呼び出すことで、
  簡単にgtool4 netCDF 規約に基づく形式のデータを出力できます。

  Fortran 77 しか利用できない環境でも gtool_history モジュールを使用できるよう、
  いくつかの ((<Fortran 77 インターフェース|URL:develop_reference/files/hspack_rdoc.html>))
  も用意されています。

  * [ ((<コードリファレンス|URL:develop_reference/classes/gtool_history.html>))
    | ((<構成ファイル一覧|URL:../src/gtool/gtool_history>)) ]

* gtool_historyauto、gtool_history_nmlinfo

  gtool_history のアプリケーションで、
  複数の変数に対して、出力の間隔や出力の可否といった出力の設定を
  NAMELIST によって変更することが可能になっています。
  多数の変数を出力するようなモデルで便利です。

  ユーザインターフェースが gtool_historyauto、
  NAMELIST から得られた情報の管理を行うライブラリが gtool_history_nmlinfo
  です。

  * gtool_historyauto
    [ ((<コードリファレンス|URL:develop_reference/classes/gtool_historyauto.html>))
    | ((<構成ファイル一覧|URL:../src/gtool/gtool_historyauto>)) ]

  * gtool_history_nmlinfo
    [ ((<コードリファレンス|URL:develop_reference/classes/gtool_history_nmlinfo.html>))
    | ((<構成ファイル一覧|URL:../src/gtool/gtool_history_nmlinfo>)) ]

* gtdata

  データアクセスに関する手続きを提供します。
  各種のデータ形式を抽象化した多次元数値データアクセスライブラリです。
  いかなるデータ形式も、多次元数値配列である変数と、
  それに付随する属性の集合としてアクセスできます。

  構造データ型定義が gtdata_types、
  手続きの総称名の定義が gtdata_generic で行われています。
  また、ライブラリ内部で用いられる言語要素は
  gtdata_internal_map、gtdata_internal_vartable で定義されます。

  * gtdata_types
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_types.html>)) ]

  * gtdata_generic
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_generic.html>)) ]

  * gtdata_internal_map
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_internal_map.html>)) ]

  * gtdata_internal_vartable
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_internal_vartable.html>)) ]
  * [ ((<構成ファイル一覧|URL:../src/gtdata>)) ]
  

* gtdata_netcdf、gtdata_netcdf_file

  netCDF データ形式とのデータアクセスライブラリです。

  主にデータ操作を行う gtdata_netcdf と、
  主にデータファイルとのアクセスを行う gtdata_netcdf_file
  とに分けられています。

  gtdata_netcdf では、
  構造データ型定義が gtdata_netcdf_types、
  手続きの総称名の定義が gtdata_netcdf_generic で行われています。
  また、ライブラリ内部で用いられる言語要素は
  gtdata_netcdf_internal で定義されます。

  * gtdata_netcdf_types
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_netcdf_types.html>)) ]

  * gtdata_netcdf_generic
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_netcdf_generic.html>)) ]

  * gtdata_netcdf_internal
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_netcdf_internal.html>)) ]

  * [ ((<構成ファイル一覧|URL:../src/gtdata/gtdata_netcdf>)) ]

  gtdata_netcdf_file では、
  構造データ型定義が gtdata_netcdf_file_types、
  手続きの総称名の定義が gtdata_netcdf_file_generic で行われています。
  また、ライブラリ内部で用いられる言語要素は
  gtdata_netcdf_file_internal で定義されます。

  * gtdata_netcdf_file_types
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_netcdf_file_types.html>)) ]

  * gtdata_netcdf_file_generic
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_netcdf_file_generic.html>)) ]

  * gtdata_netcdf_file_internal
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_netcdf_file_internal.html>)) ]

  * [ ((<構成ファイル一覧|URL:../src/gtdata/gtdata_netcdf/gtdata_netcdf_file>)) ]


* gtdata_memory

  いわゆるメモリ変数をサポートします (いまのところ１次元だけ)。

  構造データ型定義が gtdata_memory_types、
  手続きの総称名の定義が gtdata_memory_generic で行われています。
  また、ライブラリ内部で用いられる言語要素は
  gtdata_memory_internal で定義されます。

  * gtdata_memory_types
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_memory_types.html>)) ]

  * gtdata_memory_generic
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_memory_generic.html>)) ]

  * gtdata_memory_internal
    [ ((<コードリファレンス|URL:develop_reference/classes/gtdata_memory_internal.html>)) ]

  * [ ((<構成ファイル一覧|URL:../src/gtdata/gtdata_memory>)) ]

* netcdf_f77

  NetCDF オリジナルのライブラリのインターフェースのためのモジュールです
  (netCDF オリジナルの netcdf.inc の代わりに
  利用すべきものです。netcdf.inc への参照はしていません)。

  ※ gtool5 の前身である gtool4 ツール／ライブラリから継承しているものですが、
  現在は NetCDF ライブラリの Fortran 90 インターフェースを直接利用するようにすべきかもしれません。

  * [ ((<コードリファレンス|URL:develop_reference/classes/netcdf_f77.html>))
    | ((<構成ファイル一覧|URL:../src/netcdf>)) ]

* dc_utils

  文字列処理やエラー処理など、Fortran 90/95 一般に使用可能な関数や手続きを用意しています。

  * ((<文字列操作 [dc_string]|URL:develop_reference/classes/dc_string.html>))
  * ((<変数 URL の文字列解析 [dc_url]|URL:develop_reference/classes/dc_url.html>))
  * ((<日付および時刻の操作 [dc_date]|URL:develop_reference/classes/dc_date.html>))
  * ((<メッセージの出力 [dc_message]|URL:develop_reference/classes/dc_message.html>))
  * ((<CPU 時間の計測 [dc_clock]|URL:develop_reference/classes/dc_clock.html>))
  * ((<デバッグ補助 [dc_trace]|URL:develop_reference/classes/dc_trace.html>))
  * ((<optional 引数の判定 [dc_present]|URL:develop_reference/classes/dc_present.html>))
  * ((<正規表現マッチ [dc_regex]|URL:develop_reference/classes/dc_regex.html>))
  * ((<コマンドライン引数の解析 [dc_args]|URL:develop_reference/classes/dc_args.html>))
  * ((<ハッシュ (連想配列) の提供 [dc_hash]|URL:develop_reference/classes/dc_hash.html>))
  * ((<テストプログラム作成支援 [dc_test]|URL:develop_reference/classes/dc_test.html>))
  * ((<ファイルオープン時の装置番号処理 [dc_iounit]|URL:develop_reference/classes/dc_iounit.html>))
  * [ ((<構成ファイル一覧|URL:../src/dc_utils>)) ]

  
* sysdep

  処理系依存事項のラッパーです。
  プログラムの異常終了、コマンドライン引数処理、環境変数取得など、
  処理系によって異なる事項についてこのモジュールが吸収し、
  上記のモジュールでは処理系に依らない共通の関数やサブルーチンが使用可能になります。

  * [ ((<コードリファレンス|URL:develop_reference/classes/sysdep.html>))
    | ((<構成ファイル一覧|URL:../src/sysdep>)) ]

* gtool5

  図1には記されていませんが、gtool5 ライブラリに含まれるほとんどの関数やサブルーチンのラッパーとなっており、
  このモジュールを USE 文によって参照することにより、
  全てのモジュールから提供される機能を使用可能です。

  * [ ((<コードリファレンス|URL:develop_reference/classes/gtool5.html>))
    | ((<構成ファイル一覧|URL:../src/gtool>)) ]


== 依存するライブラリ

gtool5 ライブラリは、netCDF 形式のファイルへのアクセスを行うため、
以下のライブラリに依存しています。

* ((<UNIDATA netCDF ライブラリ|URL:http://www.unidata.ucar.edu/software/netcdf/>))

== ディレクトリ構造

gtool5 ディレクトリ直下の ((*src*)) ディレクトリ以下に、
Fortran 90/95 ソースコードファイルが格納されます。
((*src*)) 以下は以下のような構造となっています。
":" の後ろが、そのディレクトリに格納されるモジュールです。
原則的には 1 モジュール 1 ディレクトリですが、
あまり数を多くすると煩雑なため、dc_utils では、
複数のモジュールを含んでいます。

* ((<"gtool5/src/"|URL:../src>))
  * |- ((<gtool|URL:../src/gtool>))/ : gtool5
  * |　|- ((<gtool_history|URL:../src/gtool/gtool_history>))/ : gtool_history
  * |　|- ((<gtool_historyauto|URL:../src/gtool/gtool_historyauto>))/ : gtool_historyauto
  * |　|- ((<gtool_history_nmlinfo|URL:../src/gtool/gtool_history_nmlinfo>))/ : gtool_history_nmlinfo
  * |- ((<gtdata|URL:../src/gtdata>))/      : gtdata_generic, gtdata_types
  * |　|- ((<gtdata_netcdf|URL:../src/gtdata/gtdata_netcdf>))/ : gtdata_netcdf
  * |　|　|- ((<gtdata_netcdf_file|URL:../src/gtdata/gtdata_netcdf/gtdata_netcdf_file>))/ : gtdata_netcdf_file
  * |　|- ((<gtdata_memory|URL:../src/gtdata/gtdata_memory>))/ : gtdata_memory
  * |- ((<dc_utils|URL:../src/dc_utils>))/    : dc_* (dc_types, dc_string, ...)
  * |- ((<netcdf|URL:../src/netcdf>))/      : netcdf_f77
  * `- ((<sysdep|URL:../src/sysdep>))/      : sysdep


== ソースコードファイルの種別

ソースコードファイルの拡張子は f90、F90、rb2f90 のいずれかです。

: 拡張子 f90 のファイル

  Fortran 95 規格に準拠して記述されたファイルです。

: 拡張子 F90 のファイル

  Fortran 95 規格に準拠して記述されたファイルです。
  ただし、コンパイルより前に C プリプロセッサによって処理されることを想定しています。
  gtool5 では主に、並列化版ライブラリの作成のために使用されており、
  並列化版ライブラリ作成に関連するいくつかのプログラムの拡張子が
  F90 となっています。

: 拡張子 rb2f90 のファイル

  Fortran によるプログラムファイルを作成するための
  Ruby プログラムファイルです。
  詳しくは、((<Ruby による Fortran コード自動生成システム|URL:lib-rb2f90.htm>))
  を参照してください。
  

=end JA

=begin HTMLJA
<hr />
<small>
  $Id: lib-overview.rd,v 1.5 2009-05-29 16:08:40 morikawa Exp $
</small>
=end HTMLJA
