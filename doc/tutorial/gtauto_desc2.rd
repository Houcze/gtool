=begin JA

= 使われているサブルーチンの説明 (gtool_historyauto)

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: gtauto_desc2.rd,v 1.1 2009-10-19 11:56:10 morikawa Exp $

=end JA

=begin JA

((<例題のプログラム|URL:gtauto_first2.htm>))
において gtool_historyauto モジュールの各サブルーチンが行っていることを大まかに説明します. 
より詳しい説明は
((<リファレンスマニュアル: gtool_historyauto|URL:../code_reference/classes/gtool_historyauto.html>)) 
を参照してください. 

なお, バージョン 20090809 以前の gtool_historyauto では, 
dc_date モジュールを日付や時刻の扱いに使用しています. 
こちらの使い方に関しては,
((<使われているサブルーチンの説明 (gtool_historyauto; dc_date 使用版)|URL:gtauto_desc.htm>))
を参照ください. 


: use gtool_historyauto

  モジュールの使用を宣言します. Fortran 90/95 メインプログラムの先頭にいれましょう. 

: call HistoryAutoCreate(title, source, institution, dims, dimsize, longnames, unit, origin, interval, terminus)

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
      gtool_historyauto ではこの名前を用いて次元を参照あるいは指定したりするこ
      とになります.
    * ((*dimsizes*)) はそれぞれの次元の大きさを指定しています.
      0 は延長可能な次元であることを意味します. 時間積分計算の場合には,
      時間の次元をこのような変数に指定すればよいでしょう.
    * ((*longnames*)) は長い名前を指定します.
      描画の際に軸のタイトルとして用いられることになります.
    * ((*units*)) は各次元の単位です.
      無次元の場合には '1' を単位として指定してください.

  * ((*origin*)), ((*interval*)), ((*terminus*)) は出力するデータの時間設定に関連する項目です.

    * ((*origin*)) は時間の原点であり,
      HistoryAutoPut により変数を最初に出力するときの時間を指定します.
    * ((*interval*)) は出力時間間隔を示しています. 同じ変数に対して
      HistoryAutoPut を再度呼んだときに, この間隔に応じて
      自動的に内部で出力の可否を判定します. 
    * ((*terminus*)) は出力の終了時刻を設定します. 

    ただし, これらはデフォルト値として利用されるもので,
    NAMELIST から ((*IntValue*)), ((*OriginValue*)), ((*TerminusValue*))
    等の値を与えた場合にはそちらが優先されます. 

: call HistoryAutoPutAxis(dim, array)

  gtool4 データの座標軸データを設定します. ここに与えられた座標軸データは, 
  gtool_historyauto を用いて出力する全てのファイルに出力されます.

  * ((*dim*)) には座標の名称を指定します.
    これは ((*HistoryAutoCreate*)) の ((*dims*))
    に与えられたもののどれかでなければいけません.

  * ((*array*)) には座標軸データを与えます. 

: call HistoryAutoAddVariable(varname, dims, longname, units, [xtype])

  出力する変数を定義します. 各引数の意味は次のとおりです.

  * ((*varname*)) は変数の名前を指定します.
    以下, gtool_historyauto ではこの名前を用いて変数を参照あるいは指定したりする
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

: call HistoryAutoPut(time, varname, array)

  定義した変数を出力します. 

  * ((*time*)) にはモデル内の現在時刻を与えてください. 

  * ((*varname*)) は出力する変数名です.
    あらかじめ HistoryAutoAddVariable で定義されている必要があります. 

  * ((*array*)) は出力する値が格納されている配列です. 

: call HistoryAutoClose

  終了処理を行います. プログラムの最後にいれましょう.


=end JA


=begin HTML
<hr /> <small>
  $Id: gtauto_desc2.rd,v 1.1 2009-10-19 11:56:10 morikawa Exp $
</small>
=end HTML
