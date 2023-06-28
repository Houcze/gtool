=begin JA

= データ型

# * 豊田 英司 (toyoda), 森川 靖大 (morikawa)
#   * $Id: lib-datatypes.rd,v 1.1 2008-09-23 09:55:27 morikawa Exp $

=end JA

=begin JA

== 内部型と外部型

データ型とはデータとビットパターンの対応付けです。gtool5 ラ
イブラリは入出力をつかさどりますので、プログラム内部（主記憶）のデータ
型とファイルのデータ型を区別しています。前者は内部型、後者は外部型と呼
ばれます。

== 内部型

内部型は Fortran の型です。現在のところ、以下のようなサポート状況になっ
ています。

=end JA

=begin HTMLJA
<div align="center">
  <center>
  <table border="1" cellspacing="1">
    <tr>
      <th>型名</th>
      <th>変数値</th>
      <th>属性値</th>
    </tr>
    <tr>
      <td>INTEGER</td>
      <td align="center">可</td>
      <td align="center">可</td>
    </tr>
    <tr>
      <td>REAL</td>
      <td align="center">可</td>
      <td align="center">可</td>
    </tr>
    <tr>
      <td>DOUBLE PRECISION</td>
      <td align="center">可</td>
      <td align="center">可</td>
    </tr>
    <tr>
      <td>COMPLEX</td>
      <td align="center"><span style="background-color: #FFFF00">計画中</span></td>
      <td align="center"><span style="background-color: #FFFF00">計画中</span></td>
    </tr>
    <tr>
      <td>LOGICAL</td>
      <td align="center">—</td>
      <td align="center">可</td>
    </tr>
    <tr>
      <td>CHARACTER(<i>n</i>)</td>
      <td align="center">—</td>
      <td align="center">可</td>
    </tr>
    <tr>
      <td>type(VSTRING)</td>
      <td align="center">—</td>
      <td align="center">可</td>
    </tr>
  </table>
  </center>
</div>

=end HTMLJA

=begin JA

すべての入出力手続はサブルーチンとして提供されます。ただし別途に、基本
論理型属性値を返す Attr_True 関数があります。

== 外部型

外部型は内部型とちがって Fortran 処理系によって制約されるわけではあり
ません。ファイル形式によってどのようなものでもあり得ます。

=== 変数の型の決定

変数の外部型は変数作成時に文字列で (Create サブルーチンの xtype= 引数) 
指定します。現在のところ、netCDF ファイル形式では下表のようになります。

=end JA

=begin HTMLJA
<div align="center">
  <center>
  <table border="1" cellspacing="1" width="607">
    <tr>
      <td width="99">受け付け可能な型名 (<b>太字</b>は正式型名)</td>
      <td width="225">netCDF<br />
        (type=an)</td>
    </tr>
    <tr>
      <td width="99"><b>short</b></td>
      <td width="225">[<span style="background-color: #FFFF00">計画中</span>]
        符号付2バイト整数</td>
    </tr>
    <tr>
      <td width="99">INTEGER<br />
        long<br />
        <b>int</b></td>
      <td width="225">符号付4バイト整数</td>
    </tr>
    <tr>
      <td width="99">REAL<br />
        <b>float</b></td>
      <td width="225">IEEE754 形式<br />
        32ビット浮動小数点数</td>
    </tr>
    <tr>
      <td width="99">DOUBLEPRECISION<br />
        <b>double</b></td>
      <td width="225">[<span style="background-color: #FFFF00">計画中</span>]
        IEEE754 形式<br />
        64ビット浮動小数点数</td>
    </tr>
  </table>
  </center>
</div>

=end HTMLJA

=begin JA

=== 属性の型の決定

属性の外部型はデフォルトでは put_attr サブルーチンの引数の型で決定され
ます。ただし、文字型については xtype= 引数が実装されており、外部型を 
int あるいは float 型に変換することができます。

内部の文字型から外部の数値型に変換する際には、コンマまたは空白で文字列
を区切って配列を生成します。

=== 変数の型と入力

現在のところ、外部型が文字型 (NF_TEXT) になっている変数は作成も入力も
できません。

=== 属性の型と入力

すべての外部型と内部型の組み合わせについて入力が可能です。

=end JA


=begin HTMLJA
<hr />
<small>
  $Id: lib-datatypes.rd,v 1.1 2008-09-23 09:55:27 morikawa Exp $
</small>
=end HTMLJA
