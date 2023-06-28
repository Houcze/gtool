=begin JA

= gtool5 オフィシャルチュートリアル

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: index.rd,v 1.9 2009-10-19 11:56:43 morikawa Exp $

=end JA
=begin EN

= Gtool5 Official Tutorial

# * Yasuhiro Morikawa (morikawa), Masatsugu Odaka (odakker), Masaki Ishiwatari (momoko)
#   * $Id: index.rd,v 1.9 2009-10-19 11:56:43 morikawa Exp $

=end EN


=begin JA

== 概要

gtool5 をとりあえず使えるようになるためのチュートリアルです.
まずは ((<gtool5 インストールガイド|URL:../../INSTALL.htm>))
を参考に gtool5 のインストールを行ってください.

他にも,
((<こちら|URL:http://www.gfd-dennou.org/library/gtool/gt5tutorial/>))
に gtool5 に関するチュートリアルが集められる予定です.
また, 
((<gt4f90io に関するチュートリアル|URL:http://www.gfd-dennou.org/library/gtool/gt4tutorial/>))
も参考になるかもしれません. 

=end JA
=begin EN

== Introduction

This tutorial is a guidance to use fundamental subroutines and modules
of gtool5. Before using gtool5 following this tutorial, please install
gtool5 (see ((<gtool5 installation guide|URL:../../INSTALL.htm.en>))).

#Other tutorial of gtool5 will be uploaded on 
#((<Gtool5 Tutorials by Users|URL:http://www.gfd-dennou.org/library/gtool/gt5tutorial/index.htm.en>)).

=end EN
=begin JA

== データの入出力

=== 最低限データ入出力

(1) ((<データ出力のための最低限の設定|URL:gthist_first.htm>))
(2) ((<GAVE を用いたお手軽描画|URL:gthist_gave.htm>))
(3) ((<使われているサブルーチンの説明|URL:gthist_desc.htm>))
(4) ((<属性(attribute)をつける|URL:gthist_attr.htm>))
(5) ((<複数のファイルに出力|URL:gthist_multi.htm>))
(6) ((<ファイルから初期値を入力|URL:gthist_restart.htm>))
(7) ((<Fortran77 から使うには|URL:gthist_f77.htm>))

=== 多数のファイル出力を行うモデルでのデータ出力

(1) ((<多数のファイル出力を行うモデルでのデータ出力|URL:gtauto_first2.htm>))
(2) ((<使われているサブルーチンの説明|URL:gtauto_desc2.htm>))

#=== 並列化モデルでのデータ出力
#
#(1) ((<並列化モデルでのデータ出力|URL:gthist_mpi.htm>))
#(2) ((<使われているサブルーチンの説明|URL:gthist_mpidesc.htm>))

=end JA
=begin EN

== Date I/O interfaces

(1) ((<"Minimum setting for date I/O (JAPANESE only)"|URL:gthist_first.htm>))
(2) ((<"Date I/O interface subroutine (JAPANESE only)"|URL:gthist_desc.htm>))
(3) ((<"Add attribute (JAPANESE only)"|URL:gthist_attr.htm>))
(4) ((<"Store data to multi data files (JAPANESE only)"|URL:gthist_multi.htm>))
(5) ((<"Load initial date from data files (JAPANESE only)"|URL:gthist_restart.htm>))
(6) ((<"How to use gtool5 in Fortran77 programs? (JAPANESE only)"|URL:gthist_f77.htm>))

=end EN
=begin JA

== Fortran 90/95 汎用モジュール

(1) ((<種別型パラメタの提供|URL:dc_types.htm>))
(2) ((<メッセージの出力|URL:dc_message.htm>))
(3) ((<暦および日時の操作 (基本編)|URL:dc_calendar1.htm>))
(4) ((<暦および日時の操作 (上級編)|URL:dc_calendar2.htm>))
(5) ((<CPU 時間の計測|URL:dc_clock.htm>))
(6) ((<デバッグ補助|URL:dc_trace.htm>))
(7) ((<コマンドライン引数の解析|URL:dc_args.htm>))
(8) ((<日時および時刻の操作 (廃止予定)|URL:dc_date.htm>))
    
=end JA
=begin EN

== Fortran 90/95 general-purpose modules

(1) ((<"Type parameter specification"|URL:dc_types.htm.en>))
(2) ((<"Message dump"|URL:dc_message.htm.en>))
(3) ((<"Calendar and Date Management (Basic)"|URL:dc_calendar1.htm.en>))
(4) ((<"Calendar and Date Management (Advanced)"|URL:dc_calendar2.htm.en>))
(5) ((<"CPU time counting"|URL:dc_clock.htm.en>))                   
(6) ((<"Debug support"|URL:dc_trace.htm.en>))                       
(7) ((<"Command line arguments interpretation"|URL:dc_args.htm.en>))
(8) ((<"Date and Time Management (Obsolete)"|URL:dc_date.htm.en>))

=end EN

=begin HTML
<hr />
<small>
  $Id: index.rd,v 1.9 2009-10-19 11:56:43 morikawa Exp $
</small>
=end HTML
