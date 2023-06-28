=begin JA

= サンプルファイルリスト

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: examples.rd,v 1.2 2009-02-26 17:44:58 morikawa Exp $

=end JA

=begin JA

== データの入出力

* ((<1 次元熱伝導方程式. gtool_history を使わない例|URL:diff_orig.f90>))

* ((<1 次元熱伝導方程式. gtool_history を使った簡単な例|URL:diffusion_1.f90>)).
  [((<計算結果|URL:diffusion_1.nc>))]

* ((<1 次元熱伝導方程式. diffusion_1.f90 に属性の出力を加えた例|URL:diffusion_2.f90>)).
  [((<計算結果|URL:diffusion_2.nc>))]

* ((<2 次元ブシネスク方程式. 複数の変数を1つのヒストリーファイルに出力する場合の例|URL:benard_1.f90>)).
  [((<計算結果|URL:benard_1.nc>))]

  実行する場合には ((<ISPACK|URL:http://www.gfd-dennou.org/library/ispack>))
  の c2pack とその下位パッケージ, および 
  Fortran90 インターフェース ((<URL:c2pack.f90>)) が必要です.
  コンパイルの方法は((<複数のファイルに出力 |URL:gthist_multi.htm>))
  を参照して下さい.

* ((<2 次元ブシネスク方程式. 複数ヒストリーファイルに出力する場合の例|URL:benard_2.f90>)).
  計算結果 [((<流線関数|URL:psi.nc>)), ((<温度|URL:temp.nc>))]

  実行時に必要なライブラリとファイル, コンパイルの手順は上の例と同様です.

* ((<1 次元熱伝導方程式. Fortran77 版. gtool_history を使わない例|URL:diff77_orig.f>)).

* ((<1 次元熱伝導方程式. Fortran77 版. gtool_history を使った例|URL:diff77.f>)).
  [((<計算結果|URL:diff77.nc>))]


== Fortran 90/95 汎用モジュール

* ((<1 次元熱伝導方程式. diffusion_2.f90 に dc_types の種別型パラメタを導入した例|URL:diffusion_3.f90>)).

* ((<1 次元熱伝導方程式. diffusion_3.f90 にメッセージ出力を加えた例|URL:diffusion_4.f90>)).

* ((<1 次元熱伝導方程式. diffusion_3.f90 に CPU 時間の計測を加えた例|URL:diffusion_5.f90>)).

* ((<1 次元熱伝導方程式. diffusion_3.f90 にデバッグモードを加えた例|URL:diffusion_6.f90>)).

* ((<1 次元熱伝導方程式. diffusion_3.f90 にコマンドラインオプションを加えた例|URL:diffusion_7.f90>)).



=end JA


=begin HTML
<hr /> <small>
  $Id: examples.rd,v 1.2 2009-02-26 17:44:58 morikawa Exp $
</small>
=end HTML

