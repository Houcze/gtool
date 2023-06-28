=begin JA

= Fortran77 から使うには

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: gthist_f77.rd,v 1.2 2009-02-26 17:30:08 morikawa Exp $

=end JA

=begin JA

Fortran77 で書かれたプログラムからは, これまで示したようなやり方で 
gtool_history モジュールを使うことができません.

このために, Fortran77 用のインターフェースが別途用意してあります. ただ
し Fortan90 用のインターフェースと比較して次のような制限があります.

  * ((*複数のヒストリーファイルが使えない*))

  * ((*変数・属性の型・次元が自動判定されない.*))
    型と次元に応じたサブルーチンを選択したり,
    次元数や大きさを引数で与えなければならない.

使う際には対応する Fortran90 サブルーチンとは引数が異なっていることに
注意してください.

こちらの((<サンプルプログラム|URL:gthist_f77/diff77.f>)) を参考にしてください.
各サブルーチンの詳しい説明については
((<リファレンスマニュアル|URL:../index.htm>))
の「F77 用インターフェース [HSPACK]」
を参照してください.

* 参考資料
  * ((<上記サンプルプログラムの計算結果|URL:gthist_f77/diff77.nc>))
  * ((<上記サンプルプログラムの gtool_history を使わない版|URL:gthist_f77/diff77_orig.f>))

=end JA


=begin HTML
<hr /> <small>
  $Id: gthist_f77.rd,v 1.2 2009-02-26 17:30:08 morikawa Exp $
</small>
=end HTML
