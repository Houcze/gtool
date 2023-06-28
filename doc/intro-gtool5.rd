=begin JA

= gtool5 とは？
# * 森川 靖大 (morikawa), 豊田 英司 (toyoda), 石渡 正樹(momoko), 
#   * $Id: intro-gtool5.rd,v 1.3 2009-03-10 12:55:18 morikawa Exp $
=end JA
=begin EN
= What is gtool5 ?
# * Yasuhiro MORIKAWA (morikawa), Eizi TOYODA (toyoda), Masaki ISHIWATARI (momoko)
#   * $Id: intro-gtool5.rd,v 1.3 2009-03-10 12:55:18 morikawa Exp $
=end EN

=begin JA
== gtool5 の概要

gtool5 Fortran 90/95 ライブラリは、
地球惑星流体科学の研究・教育のためのさまざまな数値モデル
で用いることを想定して開発されている Fortran 90/95 用のライブラリです。
これは
((<地球流体電脳倶楽部 gtool プロジェクト|URL:index.htm#label-11>))
の一環として開発・管理されています。

gtool5 は、データ入出力をはじめ、
日付および時刻の操作やメッセージ出力、CPU 時間の計測など、
Fortran 90/95 で書かれた数値モデルのための様々な機能を提供します。
特にデータ入出力については、複数の人間が、
数値モデルから得られた計算結果に対する理解・認識を共有することを容易とするために、
((<gtool4 netCDF 規約|URL:xref.htm#label-6>))
に基づくデータの入出力が可能になっています。

gtool5 は
((<地球流体電脳倶楽部 dcmodel プロジェクト|URL:http://www.gfd-dennou.org/library/dcmodel>))
などで導入され、データの共有を容易にするとともに、
ソースコードの可読性向上にも役立っています。


== gtool4 Tools/Library, gt4f90io との関係について

gtool5 は、
((<"gtool4 Tools/Library"|URL:http://www.gfd-dennou.org/library/gtool/obsolete.htm>)) および
((<gt4f90io|URL:http://www.gfd-dennou.org/library/gtool/gt4f90io>))
の後継となるライブラリです。

gtool4 Tools/Library から解析可視化機能を分離し、
データ入出力ライブラリとして特化させたものが gt4f90io であり、
そこに入出力以外の汎用ユーティリティを加えたものが gtool5 となります。

地球惑星流体科学の研究・教育のためのさまざまな数値モデルで共通して使用できるユーティリティを揃えてあり、
gtool5 ライブラリを用いることで複数の数値モデル間でのソースコードの比較や共有を容易に行うことが可能となります。


=end JA

=begin EN
Under Construction ...
=end EN

=begin HTML
<hr />
<small>
  $Id: intro-gtool5.rd,v 1.3 2009-03-10 12:55:18 morikawa Exp $
</small>
=end HTML
