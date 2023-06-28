#
# gtool5 Development History in RD (Ruby Document) format 
#
#   For development history of gtool5, see "HISTORY.htm.en"
#   (written in English) or  "HISTORY.htm" (written in Japanese)
#   included in "gtool5" TGZ package available from
#   <http://www.gfd-dennou.org/library/gtool>.
#   Otherwise, see this file directly, or generate
#   above mentioned html files with "make guide" 
#   in current directory after installing  rdtool
#   <http://raa.ruby-lang.org/project/rdtool/>.

=begin JA

= gtool5 開発履歴

# * 森川 靖大 (morikawa)
#   * $Id: HISTORY,v 1.29 2010-12-28 09:53:36 morikawa Exp $

=end JA
=begin EN

= gtool5 Development History

# * Yasuhiro MORIKAWA (morikawa)
#   * $Id: HISTORY,v 1.29 2010-12-28 09:53:36 morikawa Exp $

=end EN

#=begin
#== 200?/??/?? (Tag: gtool5-????????)
#=end
#
#=begin JA
#=end JA
#=begin EN
#=end EN

=begin
== 2010/12/28 (Tag: gtool5-20101228-1)
=end
=begin JA
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>)) の改善
  * 変数の出力設定の確認手続 HistoryAutoChkOutput, HistoryAutoChkOutputTiming 作成. 
=end JA
=begin EN
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>)) is improved. 
  * Create output setting checkers "HistoryAutoChkOutput", "HistoryAutoChkOutputTiming". 
=end EN


=begin
== 2010/10/06 (Tag: gtool5-20101006)
=end

=begin JA
* ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>)) のバグフィックス
=end JA
=begin EN
* Bug fix of ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>))
=end EN

=begin
== 2010/09/24 (Tag: gtool5-20100924)
=end

=begin JA
* ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>)) の改良
  * 0 および負の年を許容するよう修正. 
  * DCCalDateInquire などの引数 elapsed_time に負の値を与えられるよう修正. 
* dc_calendar の ((<チュートリアル|URL:doc/tutorial/dc_calendar2.htm>)) および
  ((<リファレンスマニュアル|URL:doc/code_reference/classes/dc_calendar.html>)) の改訂. 
  * グレゴリオ暦使用時の DCCalInquire の "day_in_month", "day_in_month_ptr" に関するドキュメントを追加
* ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>)) のバグフィックス
  * "DCCalDateEvalSecOfYear" にオプショナル引数 cal を指定しない場合に
    エラーが発生.
* configure スクリプトの改良.
  * NetCDF ライブラリが C 版と Fortran 版に分かれている場合に対応.
=end JA
=begin EN
* Improvement of ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>))
  * Support zero and negative year. 
  * Support a negative value to an argument "elapsed_time" of "DCCalDateInquire" etc.
* Improvement of ((<tutorial|URL:doc/tutorial/dc_calendar2.htm>)) and 
  ((<reference manual|URL:doc/code_reference/classes/dc_calendar.html>))
  * Documents about "day_in_month", "day_in_month_ptr" are added. 
* Bug fix of ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>))
  * "DCCalDateEvalSecOfYear" occurs an error without an optional argument "cal". 
* "configure" script is improved.
  * Separated NetCDF library (C and Fortran) is supported. 
=end EN

=begin
== 2010/07/05 (Tag: gtool5-20100705)
=end

=begin JA
* ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>)) のバグフィックス
  * "DCCalDateEvalSecOfYear" にオプショナル引数 cal を指定しない場合に
    エラーが発生. 
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>)) の改善
  * 出力タイミングの調整
=end JA
=begin EN
* Bug fix of ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>))
  * "DCCalDateEvalSecOfYear" occurs an error without an optional argument "cal". 
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>)) is improved. 
  * Output timing is adjusted. 
=end EN


=begin
== 2010/06/21 (Tag: gtool5-20100621)
=end

=begin JA
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>)) の機能拡張
  * HistoryGetAttr で 1 次元配列情報を取得できるよう改善.
=end JA
=begin EN
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>)) is improved. 
  * "HistoryGetAttr" is improved for input of 1D array. 
=end EN


=begin
== 2010/04/13 (Tag: gtool5-20100413)
=end

=begin JA
* ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>)) の作成. 
  * 暦・日付管理については
    ((<dc_date|URL:doc/code_reference/classes/dc_date.html>))
    を廃止予定とし, このモジュールを使用する. 
  * チュートリアルも作成. 
    * ((<基本編|URL:doc/tutorial/dc_calendar1.htm>)), ((<上級編|URL:doc/tutorial/dc_calendar2.htm>))

* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>)) の機能拡張, 修正
  * HistoryPut によって文字型のデータ出力を可能とした.
  * ドキュメントの改善
    * 時間平均に関する文書を追加. 
    * 複数の HistoryPut のリファレンスマニュアル上での
      表示順序を調整.

* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>)),
  ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>)), 
  ((<gtool_history_nmlinfo|URL:doc/code_reference/classes/gtool_history_nmlinfo.html>))
  の内部で使用する経過時間の表現を dc_date で提供する DC_DIFFTIME 型ではなく, 
  倍精度実数型に変更した. 

* ((<dc_string|URL:doc/code_reference/classes/dc_string.html>)) の修正.
  * 端数の切り捨て関数 RoundNum の改善.

* ((<使用上の注意とライセンス規定|URL:CREDITS.htm>)) の更新

=end JA
=begin EN
* ((<dc_calendar|URL:doc/code_reference/classes/dc_calendar.html>)) is created. 
  * This module managements Calendar and Date instead of 
    ((<dc_date|URL:doc/code_reference/classes/dc_date.html>)) that is
    obsolete. 
  * Tutorials are created. 
    * ((<Basic|URL:doc/tutorial/dc_calendar1.htm>)), ((<Advanced|URL:doc/tutorial/dc_calendar2.htm>))

* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>)) is improved. 
  * Character type data can be output by "HistoryPut".
  * Documents are improved.
    * Documents about time average are added.
    * An order of documents of multiple "HistoryPut" on the reference manual
      is adjusted. 

* Elapsed time is implemented by double precision type instead of
  "DC_DIFFTIME" type (provided by dc_date module) in 
  ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>)),
  ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>)), 
  ((<gtool_history_nmlinfo|URL:doc/code_reference/classes/gtool_history_nmlinfo.html>))

* ((<dc_string|URL:doc/code_reference/classes/dc_string.html>)) is improved. 
  * Improve a function "RoundNum" that truncates fraction. 

* ((<Precautionary statement and Licence terms|URL:CREDITS.htm.en>)) is updated. 

=end EN


=begin
== 2009/08/09 (Tag: gtool5-20090809)
=end

=begin JA
* ((<dc_args|URL:doc/code_reference/classes/dc_args.html>)) のバグフィックス
  * 配列 opt_table の添え字番号が, 配列サイズの最大値を超えないよう修正. 
=end JA
=begin EN
* Fixed bug of ((<dc_args|URL:doc/code_reference/classes/dc_args.html>))
  * Subscript of an array "opt_table" is not over max of array size. 
=end EN


=begin
== 2009/07/29 (Tag: gtool5-20090729)
=end

=begin JA
* 変数のサイズがファイルフォーマットの制限を超える場合に,
  その旨を示すエラーメッセージを出力するよう修正. 
* バージョン 20090602 以降, Fortran90/SX で
  ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  が正常に動作していなかったバグを修正. 
=end JA
=begin EN
* When one or more variable sizes violate format constraints, 
  the message is output. 
* A bug that 
  ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  is not performed with Fortran90/SX is fixed. 
=end EN


=begin
== 2009/07/04 (Tag: gtool5-20090704)
=end

=begin JA
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  の修正.
  * HistoryGet における引数とデータの配列形状の整合性チェックの方法を修正.
=end JA
=begin EN
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  is modified. 
  * A way of consistency check of array shape between an argument and data
     in "HistoryGet" is modified. 
=end EN


=begin
== 2009/06/02 (Tag: gtool5-20090602)
=end

=begin JA
* ((<README|URL:README>)) ファイルと ((<VERSION|URL:VERSION>)) ファイルを作成. 
* src ディレクトリ以下のファイルの置き方を整理.
* 以下のモジュールを構成するソースコードファイルを整理. (個々の手続の本体を個別のファイルへ, 
  手続の引用仕様宣言を XXXXX_generic モジュールへ, 構造データ型の宣言を
  XXXXX_types モジュールへと分割).
  * ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  * ((<gtool_history_nmlinfo|URL:doc/code_reference/classes/gtool_history_nmlinfo.html>))
  * ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  * ((<dc_date|URL:doc/code_reference/classes/dc_date.html>))
=end JA
=begin EN
* ((<README|URL:README>)) file and ((<VERSION|URL:VERSION>)) file are created. 
* Location of files under "src" directory is changed. 
* Source code files that constitute following modules are arranged. 
  (Body of individual procedure is moved to individual file, 
  declarations of interfaces are moved to "XXXXX_generic" module, 
  declarations of user defined derived types are moved to 
  "XXXXX_types" module).
  * ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  * ((<gtool_history_nmlinfo|URL:doc/code_reference/classes/gtool_history_nmlinfo.html>))
  * ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  * ((<dc_date|URL:doc/code_reference/classes/dc_date.html>))
=end EN


=begin
== 2009/03/24 (Tag: gtool5-20090324)
=end

=begin JA
* src ディレクトリ以下のファイルの置き方を整理.
  * サブディレクトリに移動. 
* configure スクリプトの改良.
  * --enable-mpi オプションの追加.
  * mpi で始まるコマンドをコンパイラとして指定した場合に,
    自動的に並列化版ライブラリとしてビルド. 
* 開発者向けドキュメントの更新と追加.
  * 「ライブラリの概観」更新
  * 「ドキュメントの概観と保守管理の解説」作成
  * 「configure と Config.mk.in の保守管理」作成
  * 「Ruby による Fortran コード自動生成システム」作成
* ((<regex|URL:doc/code_reference/classes/regex.html>))
  モジュールを
  ((<dc_regex|URL:doc/code_reference/classes/dc_regex.html>))
  にリネーム.
=end JA
=begin EN
* A way of location of files under "src" directory is changed. 
  * Almost files are moved to subdirectories.
* "configure" script is improved. 
  * "--enable-mpi" option is added. 
  * If a command with "mpi" prefix is used, 
    parallel version library is built automatically. 
* Documents for developpers (JAPANESE only) are updated and added.
  * "Overview of the library" updated. 
  * "Overview of documents and explanations of maintenance" created. 
  * "Maintenance of configureand Config.mk.in " created. 
  * "Auto-generation system of Fortran codes by Ruby" created. 
* ((<regex|URL:doc/code_reference/classes/regex.html>))
  modules is renamed to 
  ((<dc_regex|URL:doc/code_reference/classes/dc_regex.html>)).
=end EN


=begin
== 2009/03/02 (Tag: gtool5-20090302)
=end

=begin JA
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  の改良.
  * 属性入力用インターフェース HistoryGetAttr を作成.
=end JA
=begin EN
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  is improved. 
  * An interface "HistoryGetAttr" that gets attributes is created. 
=end EN


=begin
== 2009/03/01 (Tag: gtool5-20090301)
=end

=begin JA
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  の改良.
  * HistoryGet, HistoryGetPointer に, データの時刻に関する情報を
    入手するためのオプショナル引数
    returned_time, flag_time_exist を追加.
=end JA
=begin EN
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  is improved. 
  * Optional arguments "returned_time", "flag_time_exist"
    for information about time of input data 
    are added to "HistoryGet", "HistoryGetPointer". 
=end EN


=begin
== 2009/02/28 (Tag: gtool5-20090228)
=end

=begin JA
* Cray XT + PGI Fortran での MPI を用いたデータ入出力に対応. 
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  の修正.
  * メモリの解放が不完全だった部分を修正.
* ((<gtool5 オフィシャルチュートリアル|URL:doc/tutorial>))
  に, gtool_historyauto, dc_date モジュールに関する文書を追加. 
=end JA
=begin EN
* Data I/O with MPI on Cray XT + PGI Fortran is supported. 
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  is modified. 
  * Bugs about memory deallocation are fixed. 
* Documents about "gtool_historyauto" and "dc_date" modules are added to 
  ((<Gtool5 offical tutorial|URL:doc/tutorial/index.htm.en>)). 
=end EN


=begin
== 2009/02/17 (Tag: gtool5-20090217)
=end

=begin JA
* ((<dc_scaledsec|URL:doc/code_reference/classes/dc_scaledsec.html>)) の修正. 
  * 関数 mod の修正.
=end JA
=begin EN
* ((<dc_scaledsec|URL:doc/code_reference/classes/dc_scaledsec.html>))
  is modified.
  * A function "mod" is modified. 
=end EN


=begin
== 2009/02/11 (Tag: gtool5-20090211)
=end

=begin JA
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  の修正.
  * HistoryPut で時刻データを与えた際に誤って警告が表示されるバグを修正.
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  の修正.
  * 出力間隔が N × Δt で割り切れない場合の処理の修正.
  * コンパイル時間の短縮のため, 出力変数の最大値を NetCDF 本来
    の最大数 2000 から 256 に変更. 
=end JA
=begin EN
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  is modified. 
  * A bug that erroneous warning is displayed when time is given by
    "HistoryPut" is fixed. 
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  is modified. 
  * Handling when output interval cannot be divided by N x Delta t is modified. 
  * For reduction of compilation, maximum value of output variables
    is changed from 2000 that is maximum value of netCDF to 256. 
=end EN


=begin
== 2009/01/16 (Tag: gtool5-20090116)
=end

=begin JA
* ((<dc_date|URL:doc/code_reference/classes/dc_date.html>))
  の修正.
  * DCDiffTimeCreate で無次元時間を単位とした場合のバグの修正.
=end JA
=begin EN
* ((<dc_date|URL:doc/code_reference/classes/dc_date.html>))
  is modified. 
  * A bug of "DCDiffTimeCreate" with non-dimensional time is fixed. 
=end EN


=begin
== 2009/01/15 (Tag: gtool5-20090115)
=end

=begin JA
* Intel Fortran version 11.0 に対応.
* ((<gtool5 ドキュメント|URL:doc/>)) ページの全体的な修正.
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  の修正.
  * HistoryGet を用いてデータ入力した際のメッセージ出力のバグを修正. 
=end JA
=begin EN
* Intel Fortran version 11.0 is supported.
* ((<gtool5 Documents|URL:doc/>)) page is overhauled. 
* ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  is modified. 
  * A bug about output messages of data input with "HistoryGet" is fixed. 
=end EN


=begin
== 2008/11/09 (Tag: gtool5-20081109)
=end

=begin JA
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  の修正.
  * 変数毎に時刻を前後に入れ替えての出力に関して修正.
=end JA
=begin EN
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  is modified.
  * Modification for output with switch of time backward and forward. 
=end EN


=begin
== 2008/10/21 (Tag: gtool5-20081021)
=end

=begin JA
* MPI に対応
  * 出力ファイルの自動分割・自動統合
    * ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
      の HistoryAutoCreate を参照. 
    * ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
      の HistoryCreate を参照. 
  * 複数ノードからのファイルの入力
    * ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
      の HistoryGet を参照. 
=end JA
=begin EN
* MPI is supported 
  * Auto split or auto integration of output files
    * See "HistoryAutoCreate" of 
      ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
    * See "HistoryCreate" of 
      ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
  * Input multiple files on multiple nodes. 
    * See "HistoryGet" of
      ((<gtool_history|URL:doc/code_reference/classes/gtool_history.html>))
=end EN


=begin
== 2008/10/07 (Tag: gtool5-20081007)
=end

=begin JA
* GNU Fortran に対応.
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  の改良. 
  * 速度の改善.
  * HistoryAutoCreate に origin_date 引数を追加. 
    この引数により, 出力開始日時が時刻次元の units 属性に付加される. 
* ((<dc_clock|URL:doc/code_reference/classes/dc_clock.html>)) の改良. 
  * 速度の改善. 
* ((<dc_scaledsec|URL:doc/code_reference/classes/dc_scaledsec.html>)) の改良. 
  * 速度の改善. 
* ((<dc_date|URL:doc/code_reference/classes/dc_date.html>)) の改良. 
  * 速度の改善. 
  * DCDateTimeCreate に整数型引数 zone_hour と zone_min を追加.
  * 負の DC_DIFFTIME 型変数の取り扱いについて修正.
* configure の修正.
  * FORTRAN90/SX に対応.
=end JA
=begin EN
* GNU Fortran is supported.
* ((<gtool_historyauto|URL:doc/code_reference/classes/gtool_historyauto.html>))
  is improved.
  * Speed is improved. 
  * An argument "origin_date" is added to "HistoryAutoCreate".
    By this argument, output start date is appended to "units" attribute
    of time dimension. 
* ((<dc_clock|URL:doc/code_reference/classes/dc_clock.html>)) is improved. 
  * Speed is improved. 
* ((<dc_scaledsec|URL:doc/code_reference/classes/dc_scaledsec.html>))
  is improved. 
  * Speed is improved. 
* ((<dc_date|URL:doc/code_reference/classes/dc_date.html>))
  is improved. 
  * Speed is improved. 
  * Integer arguments "zone_hour" and "zone_min" are added to
    "DCDateTimeCreate". 
  * Treatment of negative "DC_DIFFTIME" variables are modified.
* configure is improved.
  * FORTRAN90/SX is supported. 
=end EN


=begin
== 2008/09/23 (Tag: gtool5-20080923-2)
=end

=begin JA
* ((<gt4f90io|URL:http://www.gfd-dennou.org/library/gtool4/gt4f90io>))
  から gtool5 へライブラリの名称を変更. 
=end JA
=begin EN
* Library name is changed from
  "((<gt4f90io|URL:http://www.gfd-dennou.org/library/gtool4/gt4f90io>))"
  to "gtool5"
=end EN

=begin HTML
<hr />
<small>
$Id: HISTORY,v 1.29 2010-12-28 09:53:36 morikawa Exp $
</small>
=end HTML

#== Mode setting for Emacs
#Local Variables:
#mode: rd
#End:
#
