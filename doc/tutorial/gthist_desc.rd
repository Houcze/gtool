=begin JA

= 使われているサブルーチンの説明

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: gthist_desc.rd,v 1.3 2009-03-01 03:02:30 morikawa Exp $

=end JA

=begin JA

((<例題のプログラム|URL:gthist_first.htm>))
において gtool_history モジュールの各サブルーチンが行っていることを大まかに説明します.
より詳しい説明は
((<リファレンスマニュアル|URL:../index.htm>)) を参照してください. 

: use gtool_history

  モジュールの使用を宣言します. Fortran 90/95 メインプログラムの先頭にいれ
  ましょう.

: call HistoryCreate(file, title, source, institution, dims, dimsize, longnames, unit, origin, interval, [history])

  gtool4 データ出力の初期設定を行います. 各引数の意味は次のとおりです.

  * ((*file*)) は出力するファイルの名前を指定します.

  * ((*title*)), ((*source*)), ((*institution*))
    は生成する gtool4 データについての一般的な情報を書き込みます
    ((*title*)) はデータ全体の表題,
    ((*source*)) はデータを作成する際の手段を書いておき
    ます. モデル計算の出力ならばモデルの名前を書けばよいでしょう.
    ((*institution*)) はファイルを最終的に変更した人あるいは
    組織などを示す項目です.
  
  * ((*dims*)), ((*dimsizes*)), ((*longnames*)), ((*units*))
    は次元に関する情報を指定しています.
    サンプルプログラムでは複数の次元を設定していますので, それぞれの項目を
    配列として与えています.
     
    * ((*dims*)) は次元の名前を定義します.
      gtool_history ではこの名前を用いて次元を参照あるいは指定したりするこ
      とになります.
    * ((*dimsizes*)) はそれぞれの次元の大きさを指定しています.
      0 は延長可能な次元であることを意味します. 時間積分計算の場合には,
      時間の次元をこのような変数に指定すればよいでしょう.
    * ((*longnames*)) は長い名前を指定します.
      描画の際に軸のタイトルとして用いられることになります.
    * ((*units*)) は各次元の単位です.
      無次元の場合には '1' を単位として指定してください.

  * ((*origin*)), ((*interval*)) は出力するデータの時間設定に関連する項目です.
     
    * ((*origin*)) は時間の原点であり,
      HistoryPut により変数を最初に出力するときの時間を指定します.
    * ((*interval*)) は出力時間間隔を示しています. 同じ変数に対して
      HistoryPut を再度呼んだときに自動的に時間変数がこの値だけ増やさ
      れて出力されます.

  * ((*[history]*)) は出力ファイルの設定に関する項目です.
    出力ファイルを複数に分割する場合に指定します.
    ここに代入する変数は GT_HISTORY 型の構造体でなければなりません.


: call HistoryAddVariable(varname, dims, longname, units, [xtype])

  出力する変数を定義します. 各引数の意味は次のとおりです.

  * ((*varname*)) は変数の名前を指定します.
    以下, gtool_history ではこの名前を用いて変数を参照あるいは指定したりする
    ことになります.

  * ((*dims*)) は変数の次元を次元名の配列で指定します.
    ((*dims=('x','t')*)) では 2 次元配列であり,
    1 次元目が 'x', 2 次元目が 't' で
    あることを表しています.

  * ((*longname*)) は長い方の名前です.
    描画の際にタイトルとして用いられることになります.

  * ((*units*)) は変数の単位を表します.
    無次元の場合には '1' を指定しましょう.

  * ((*xtype*)) は変数の型を表します.
    指定可能な型については ((<リファレンスマニュアル|URL:../index.htm>))
    を参照してください. ((*xtype='double'*))
    は倍精度変数であることを表しています.

: call HistoryPut(varname, array, [history])

  定義した変数を出力します.

  * ((*varname*)) が出力する変数名です.
    あらかじめ HistoryAddVariable で定義されているか,
    HistoryCreate により次元変数として定義されている必要があります.

  * ((*array*)) は出力する値が格納されている配列です.

  * ((*history*)) はオプションで,
    出力するファイルを表します. あらかじめ
    HistoryCreate により定義されている必要があります.

: call HistoryClose([history])

  終了処理を行います. プログラムの最後にいれましょう.


=end JA


=begin HTML
<hr /> <small>
  $Id: gthist_desc.rd,v 1.3 2009-03-01 03:02:30 morikawa Exp $
</small>
=end HTML
