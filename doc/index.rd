=begin JA

= gtool5 ドキュメント

# * 森川 靖大 (morikawa), 豊田 英司 (toyoda), 石渡 正樹 (momoko)
#   * $Id: index.rd,v 1.15 2009-10-19 11:56:11 morikawa Exp $

=end JA
=begin EN

= gtool5 Documents

# * Yasuhiro MORIKAWA (morikawa), Eizi TOYODA (toyoda), Masaki ISHIWATARI (momoko)
#   * $Id: index.rd,v 1.15 2009-10-19 11:56:11 morikawa Exp $

=end EN

=begin JA
== はじめに

  * ((<gtool5 とは？|URL:intro-gtool5.htm>))
  * ((<使用上の注意とライセンス規定|URL:../CREDITS.htm>))
  * ((<gtool5 インストールガイド|URL:../INSTALL.htm>))
    * ((<gt4f90ioからの移行について|URL:transition-gt4f90io.htm>))

=end JA

=begin EN
== Note

The gtool5 is a developing library. And documentations are mainly
written in Japanese, and translated into English. The work is always
in progress and incomplete. If you have found inconvenience, please
report to ((<"IMG:images/dcstaff_email.png">))

== Introduction

  * ((<What is gtool5 ? (JAPANESE only)|URL:intro-gtool5.htm>))
  * ((<Precautionary statement and Licence terms|URL:../CREDITS.htm.en>))
  * ((<Gtool5 Installation Guide? |URL:../INSTALL.htm.en>))
    * ((<Transition from gt4f90io|URL:transition-gt4f90io.htm.en>))

=end EN

=begin JA
== チュートリアル

  * ((<gtool5 オフィシャルチュートリアル|URL:tutorial>))
  * ((<利用者による gtool5 チュートリアル集|URL:http://www.gfd-dennou.org/library/gtool/gt5tutorial/>))

=end JA
=begin EN
== Tutorial

  * ((<Gtool5 Official Tutorial|URL:tutorial/index.htm.en>))
  * ((<Gtool5 Tutorials by Users (JAPANESE only)|URL:http://www.gfd-dennou.org/library/gtool/gt5tutorial/>))

=end EN

=begin JA
== コードリファレンス

=== データ入出力インターフェース

gtool4 netCDF 規約に基づくデータの入出力を行うためのインターフェースです. 

  * ((<"入出力インターフェース [gtool_history]"|URL:code_reference/classes/gtool_history.html>))

    * ((<"gt4f90io 後方互換 [gt4_history]"|URL:code_reference/classes/gt4_history.html>))

    * ((<"F77 用インターフェース [HSPACK]"|URL:code_reference/files/hspack_rdoc.html>))

  * ((<大規模モデル用出力インターフェース [gtool_historyauto]|URL:code_reference/classes/gtool_historyauto.html>))

=== Fortran 90/95 汎用モジュール

Fortran 90/95 によるプログラム一般で使用することを想定した
ユーティリティ群です. [ ] 内はモジュール名を表します. 

  * ((<文字列操作 [dc_string]|URL:code_reference/classes/dc_string.html>))
  * ((<変数 URL の文字列解析 [dc_url]|URL:code_reference/classes/dc_url.html>))
  * ((<暦および日時の操作 [dc_calendar]|URL:code_reference/classes/dc_calendar.html>))
  * ((<メッセージの出力 [dc_message]|URL:code_reference/classes/dc_message.html>))
  * ((<CPU 時間の計測 [dc_clock]|URL:code_reference/classes/dc_clock.html>))
  * ((<デバッグ補助 [dc_trace]|URL:code_reference/classes/dc_trace.html>))
  * ((<optional 引数の判定 [dc_present]|URL:code_reference/classes/dc_present.html>))
  * ((<正規表現マッチ [dc_regex]|URL:code_reference/classes/dc_regex.html>))
  * ((<コマンドライン引数の解析 [dc_args]|URL:code_reference/classes/dc_args.html>))
  * ((<ハッシュ (連想配列) の提供 [dc_hash]|URL:code_reference/classes/dc_hash.html>))
  * ((<テストプログラム作成支援 [dc_test]|URL:code_reference/classes/dc_test.html>))
  * ((<ファイルオープン時の装置番号処理 [dc_iounit]|URL:code_reference/classes/dc_iounit.html>))
  * ((<日付および時刻の操作 (廃止予定) [dc_date]|URL:code_reference/classes/dc_date.html>))

#=== gtool データ操作
#
#  * ((<gtdata_generic モジュール|URL:code_reference/classes/gtdata_generic.html>))
#
=end JA
=begin EN
== Code References

=== Data Input/Output Interfaces

Input/Output interfaces for data based on gtool4 netCDF Conventions. 

  * ((<"Input/Output intefaces (JAPANESE only) [gtool_history]"|URL:code_reference/classes/gtool_history.html>))

    * ((<"Backward compatibility for Gt4f90io (JAPANESE only) [gt4_history]"|URL:code_reference/classes/gt4_history.html>))

    * ((<"FORTRAN77 interfaces (JAPANESE only) [HSPACK]"|URL:code_reference/files/hspack_rdoc.html>))

  * ((<Output intefaces for large numerical models [gtool_historyauto]|URL:code_reference/classes/gtool_historyauto.html>))

    
=== Fortran 90/95 general-purpose modules

These are utilities for Fortran 90/95 programs. 
Characters in [ ] are module names.

  * ((<Support for development of test programs [dc_test]|URL:code_reference/classes/dc_test.html>))
  * ((<Unit number handling at file open [dc_iounit]|URL:code_reference/classes/dc_iounit.html>))
  * ((<Character-string handling (JAPANESE only) [dc_string]|URL:code_reference/classes/dc_string.html>))
  * ((<Analysis of URL (JAPANESE only) [dc_url]|URL:code_reference/classes/dc_url.html>))
  * ((<Calendar and Date Management (JAPANESE only) [dc_calendar]|URL:code_reference/classes/dc_calendar.html>))
  * ((<Output of messages (JAPANESE only) [dc_message]|URL:code_reference/classes/dc_message.html>))
  * ((<Monitor of CPU TIME (JAPANESE only) [dc_clock]|URL:code_reference/classes/dc_clock.html>))
  * ((<Debug tracer (JAPANESE only) [dc_trace]|URL:code_reference/classes/dc_trace.html>))
  * ((<Judge of optional arguments (JAPANESE only) [dc_present]|URL:code_reference/classes/dc_present.html>))
  * ((<Regular expression (JAPANESE only) [dc_regex]|URL:code_reference/classes/dc_regex.html>))

  * ((<Command line arguments parser (JAPANESE only) [dc_args]|URL:code_reference/classes/dc_args.html>))
  * ((<Hash handling (JAPANESE only) [dc_hash]|URL:code_reference/classes/dc_hash.html>))
  * ((<Date and Time Management (Obsolete; JAPANESE only) [dc_date]|URL:code_reference/classes/dc_date.html>))

=end EN

=begin JA
== 開発者向けドキュメント
* ((<ライブラリ概説|URL:lib-overview.htm>))
* ((<オブジェクト指向スタイル|URL:lib-oop.htm>))
* ((<「gtool 変数」という概念|URL:lib-gtoolvar1.htm>))
* ((<手続命名法|URL:lib-naming.htm>))
* ((<データ型|URL:lib-datatypes.htm>))
* ((<構造体一覧|URL:lib-derivedtype.htm>))
* ((<エラーの取り扱い|URL:lib-error.htm>))
* ((<ドキュメントの概説と保守管理の解説|URL:lib-overview-doc.htm>))
* ((<configure と Config.mk.in の保守管理|URL:lib-configure.htm>))
* ((<Ruby による Fortran コード自動生成システム|URL:lib-rb2f90.htm>))
* バージョン管理の方法 (作成中)
* モジュール間の依存関係自動チェックユーティリティ (作成中)
* 動作テスト (作成中)

* ((<開発者向けコードリファレンス|URL:develop_reference>))
  * 外部からの参照を許可していない (PRIVATE 属性を持つ)
    手続き等もドキュメント化されています.
  * Fortran 90/95 プログラムだけでなく, 開発支援用の Ruby スクリプトも
    ドキュメント化されています.

=end JA

=begin EN
== Documents for Developers

* ((<Overview of gtool5 (JAPANESE only)|URL:lib-overview.htm>))
* ((<Object-oriented programming style (JAPANESE only)|URL:lib-oop.htm>))
* ((<Concept of "gtool variable" (JAPANESE only)|URL:lib-gtoolvar1.htm>))
* ((<Naming convention of procedures (JAPANESE only)|URL:lib-naming.htm>))
* ((<About derived data types (JAPANESE only)|URL:lib-datatypes.htm>))
* ((<List of derived data types (JAPANESE only)|URL:lib-derivedtype.htm>))
* ((<Error handling|URL:lib-error.htm>))

=== Description for development and maintenance of gtool5

* ((<Documents for developers (Almost all documents are JAPANESE only)|URL:develop_reference>))
  * PRIVATE procedures that are not allowed to be referred from the
    outside are documented too.
  * Not only Fortran 90/95 programs but also Ruby scripts for
    development support are documented.

=end EN

=begin JA
== 開発履歴

* ((<gtool5 開発履歴|URL:../HISTORY.htm>))
* ((<ChangeLog|URL:../ChangeLog>))
=end JA
=begin EN
== History

* ((<Gtool5 Development History|URL:../HISTORY.htm.en>))
* ((<ChangeLog|URL:../ChangeLog>))
=end EN


=begin JA
== リンク

* ((<地球流体電脳倶楽部|URL:http://www.gfd-dennou.org/>))
  * ((<gtool プロジェクト|URL:http://www.gfd-dennou.org/library/gtool/>))
  * ((<dcmodel プロジェクト|URL:http://www.gfd-dennou.org/library/dcmodel/>))
  * ((<davis プロジェクト|URL:http://www.gfd-dennou.org/library/davis/>))

* ((<引用文献|URL:xref.htm>))
=end JA

=begin EN
== Link

* ((<GFD Dennou Club|URL:http://www.gfd-dennou.org/>))
  * ((<Gtool Project|URL:http://www.gfd-dennou.org/library/gtool/>))
  * ((<Dcmodel Project|URL:http://www.gfd-dennou.org/library/dcmodel/>))
  * ((<Davis Project|URL:http://www.gfd-dennou.org/library/davis/>))

* ((<References|URL:xref.htm.en>))

=end EN


=begin HTML
<hr />
<small>
  $Id: index.rd,v 1.15 2009-10-19 11:56:11 morikawa Exp $
</small>
=end HTML
