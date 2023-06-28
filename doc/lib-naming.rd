=begin JA

= 命名法

# * 豊田 英司 (toyoda), 森川 靖大 (morikawa)
#   * $Id: lib-naming.rd,v 1.1 2008-09-23 09:55:27 morikawa Exp $

=end JA

=begin JA

== パッケージ・モジュール命名法

モジュール名はインターフェイス・パッケージの名称をそのままつけるのを
本則としています。具体的には現状では下記のようになっています。

=end JA

=begin HTMLJA
<blockquote>
  <table border="1" cellspacing="1">
    <tr>
      <th>パッケージ名</th>
      <th>モジュール名</th>
      <th>機能</th>
    </tr>

    <tr>
      <td colspan="2">gtool_history</td>
      <td>gtool5
        の機能を用いて複数の変数からなるヒストリ機能を提供</td>
    </tr>

    <tr>
      <td colspan="2">gtool5</td>
      <td>gtdata および gtgraph の機能を提供</td>
    </tr>

    <tr>
      <td>gtdata</td>
      <td>gtdata_generic, gtdata_types</td>
      <td>ファイル形式に依存しない gtool
        変数に対する入出力を提供</td>
    </tr>

    <tr>
      <td>an (abstract netcdf)</td>
      <td>an_generic, an_types, an_file</td>
      <td>netCDF
        変数の入出力。ファイルの自動オープン、入出力範囲の保持、属性の自動文字列変換などの機能を付加する</td>
    </tr>

    <tr>
      <td>gr</td>
      <td>gr_generic, gr_types</td>
      <td>GrADS 格子点データのアクセスルーチン。未完成</td>
    </tr>

    <tr>
      <td colspan="2">netcdf_f77</td>
      <td>UNIDATA netCDF ライブラリの引用仕様宣言</td>
    </tr>

    <tr>
      <td colspan="2">sysdep</td>
      <td>Fortran
        コンパイラに依存するコードの共通インターフェイスを提供</td>
    </tr>

    <tr>
      <td colspan="2">dc_string</td>
      <td>文字列と数値の変換など</td>
    </tr>

    <tr>
      <td colspan="2">dc_error</td>
      <td>エラー処理関係</td>
    </tr>
  </table>

</blockquote>

=end HTMLJA

=begin JA

手続が多い層、たとえば gtgraph や gtdata ではコンパイル時間を短縮する
ために以下のような手段をとっています。まず、構造型や定数などは 
gtgraph_types のように _types で終わる名前のモジュールで定義します。
手続はこれらを用いて外部手続として作成します。そして、これらの外部手続の
引用仕様と総称名を与えます (gtgraph_generic のように) _generic で終わる
名前のモジュールを用意します。ユーザは外部手続名を使ってもよいですが、
ここで与えられた総称名を使って手続を呼び出すのを本則とします。

== 手続命名法

外部手続名は以下のように構成しています。

  * 操作の主たる対象となる構造体（第一引数）があって総称名が与えられて
    いるとき、これらの名前から下線を除去して連結したものを本則とします。
    このとき構造体名は1音節に省略しても構いません。たとえば
    GT_VARIABLE に対する Open サブルーチンは GtVarOpen と言います。

  * 同じ型の第一引数に対して多数の手続総称定義を与えている場合、手続名
    の末尾には残りの引数の型を適当に省略したものをつけて区別します。

  * 引数が組み込み型や VSTRING だけからなる場合は適宜所属する
    インターフェイスを決め、上に準じて命名します。
    たとえばシステム依存の (sysdep モジュール) プログラムを停止
    (abort) するサブルーチンは SysdepAbort と命名されます。

== ファイルの命名法

モジュール名または手続名を小文字にしたものをファイル名とします。
たとえば GtVarOpen サブルーチンは gtvaropen.f90 で提供されます。

ほとんど同時に使用されると考えられる複数のサブルーチンは面倒なので
ひとつのファイルにまとめていることがあります。
この場合ファイル名は共通部分をもちいますが、
誤解を生じるときには適当な接尾辞をつけることがあります。
たとえば GTCont... サブルーチン群はまとめて gtcontlib.f90
に書かれていますが、
これはかつて gtcont コマンドが存在したのでそのソースファイルと
誤認されないためでした。

=end JA

=begin HTMLJA
<hr />
<small>
  $Id: lib-naming.rd,v 1.1 2008-09-23 09:55:27 morikawa Exp $
</small>
=end HTMLJA
