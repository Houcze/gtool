=begin JA

= GAVE を用いたお手軽描画

# * 森川 靖大 (morikawa)
#   * $Id: gthist_gave.rd,v 1.2 2009-03-01 03:02:30 morikawa Exp $

=end JA

=begin JA

gtool_history モジュールを用いて出力した
gtool4 形式のデータは,
((<電脳 Ruby プロジェクト|URL:http://ruby.gfd-dennou.org/index-j.htm>))
製品の ((<GAVE|URL:http://ruby.gfd-dennou.org/products/gave/index-j.html>))
や ((<GPhys|URL:http://ruby.gfd-dennou.org/products/gphys/>))
で描画することが可能です.

ここでは
((<データ出力のための最低限の設定|URL:gthist_first.htm>))
で出力する
((<diffusion_1.nc|URL:gthist_first/diffusion_1.nc>))
を, GUI (グラフィカルユーザインターフェース)
ユーティリティである GAVE を用いて描画してみます. 
まず, 
((<電脳Ruby謹製品 インストールガイド|URL:http://www.gfd-dennou.org/arch/ruby/tutorial/install/index-j.html>))
を参考に, GAVE のインストールを行ってください. 

準備が出来たら, 以下のように gave を起動してください. 

  $ gave diffusion_1.nc

以下のようなウィンドウが表示されます. 
((:<center>:))
((<IMG|"IMG:gthist_gave/gave_main1.png">))
((:</center>:))

とりあえず描画ボタンを押してみましょう.
以下のような図が表示されます. 図は, 横軸が x, 縦軸が時間 t
となっており, 温度 temp がカラーで表示されています. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_draw1.png">))
((:</center>:))

次に等値線図を描いてみましょう. 
まず [動作] から [折れ線] を選んでください. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_setline.png">))
((:</center>:))

ウィンドウの表示が以下のようになるはずです. [X-軸] が x
になっていることを確認してください. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_main2.png">))
((:</center>:))

ここで描画ボタンを押すと, 以下のような図が表示されます.

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_draw2.png">))
((:</center>:))

最後に, この等値線図をアニメーション表示してみましょう. 
準備として, 最大値を固定するための設定を行います.
[ツール] から [軸] を選択してください. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_openaxes.png">))
((:</center>:))

新たに表示されたウィンドウで, [最小値] と [最大値]
にそれぞれ 0 と 1 を入力してください. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_setmax.png">))
((:</center>:))

[ツール] から [アニメーション] と [次元] を選択してください. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_openanim.png">))
((<IMG|"IMG:gthist_gave/gave_opendim.png">))
((:</center>:))

すると以下のウィンドウが表示されます. 
ここで, アニメーションウィンドウの再生マークを押してみましょう. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_windowanim.png">))
((<IMG|"IMG:gthist_gave/gave_windowdim.png">))
((:</center>:))

すると, 図がアニメーション表示されます. 同時に,
次元ウィンドウではアニメーションに合わせて時刻 t
の値が変化します. 止める際には,
アニメーションウィンドウのストップボタンを押してください. 

((:<center>:))
((<IMG|"IMG:gthist_gave/gave_draw3.png">))
((<IMG|"IMG:gthist_gave/gave_windowanimdim.png">))
((:</center>:))

ここでは, GUI ユーティリティである GAVE を用いた描画の簡単な例を紹介しました. 
GAVE についての詳細は, 
((<GAVE チュートリアル|URL:http://ruby.gfd-dennou.org/products/gave/tutorial/takemoto/>))
を参照ください
(「付録A 使用方法」でウィンドウ上の各設定値などについて詳しく解説しています). 

((<電脳 Ruby プロジェクト|URL:http://www.gfd-dennou.org/library/ruby>))
では他にも, 以下のようなユーティリティを提供しています. 
用途に応じてご使用ください.

  * ((<GPhys|URL:http://ruby.gfd-dennou.org/products/gphys/>))
    * オブジェクト指向スクリプト言語 Ruby で記述された,
      解析・可視化ソフトウェアです. GAVE も GPhys のアプリケーションの一つです.
      GAVE に比べ, 解析や可視化に際してより自在に設定を行うことが可能です. 
    * 実際に使用するにあたっては,
      ((<GPhys チュートリアル|URL:http://ruby.gfd-dennou.org/products/gphys/tutorial/>))
      を参照してください. なお, GAVE がインストールされていれば, 
      GPhys も既にインストールされていることとなるため,
      改めてのインストールは不要です.

  * ((<Gfdnavi|URL:http://www.gfd-dennou.org/library/davis/gfdnavi/>))
    * GPhys アプリケーションの一つで, 地球流体データのデータベース,
      解析・可視化のためのデスクトップツール兼サーバーです. 
    * データファイルが自動的にデータベースに登録され,
      容易に検索できるため多数のデータファイルを取り扱う場合に適しています.
    * 解析や可視化は Web ブラウザーを通して行うことが可能であり, 
      その結果得られた画像や数値データをデータベースに保存することも可能なことから,
      共同研究者とデータやそれに関する情報をやりとりするのにも適しています. 

=end JA


=begin HTML
<hr /> <small>
  $Id: gthist_gave.rd,v 1.2 2009-03-01 03:02:30 morikawa Exp $
</small>
=end HTML
