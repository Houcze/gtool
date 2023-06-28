=begin JA

= ファイルから初期値を入力

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: gthist_restart.rd,v 1.2 2009-02-26 17:01:35 morikawa Exp $

=end JA

=begin JA

次に gtool4 形式ファイルのデータをプログラムに入力する方法について解説
します. これは出力結果を初期値として計算を継続する場合, 初期値を変えた
計算を行うなどの場合に必要となります.


== HistoryGet

データを入力するには HistoryGet サブルーチンを用います.

      real(8) :: xyz_U(10,20,30)

      call HistoryGet('xhistget1.nc', 'U', xyz_U)

第1引数に入力するデータの格納されたファイル名を,
第2引数に入力するデータの変数名を,
第3引数に入力したデータを格納する配列を与えます. 

データが時間に依存する場合には, そのデータのうち最も新しい時刻
のデータが入力されます. 

第3引数として与える配列は入力するデータと同じ次元, 配列サイズで
なければなりません. 異なる場合には, エラーが発生します. 


=== 入力範囲指定

以下のように文字型引数 ((*range*)) を末尾に与えることで,
入力範囲を指定することも可能です.

      real(8) :: y_V(20)

      call HistoryGet('xhistget2.nc', 'V', y_V, range='x=180.0,y=-10.0:10.0,t=3.5')

((*range*)) の表記方法は
((<gtool4 netCDF 規約|URL:http://www.gfd-dennou.org/library/gtool4/>))
の「5.4 コンマ記法」です. 詳しくはそちらを参照してください.


== HistoryGetPointer

データの入力にポインタ型の引数を与えることで、入力するデータに
合わせて配列サイズを自動的に設定することも出来ます.
(次元数は一致している必要があります).

      real(8), pointer  :: xyz_W(:,:,:) => null()

      call HistoryGetPointer('xhistget3.nc', 'W', xyz_W)

メモリリークを避けるため,
データが入力されるポインタ型の引数は必ず空状態にして与えてください.

HistoryGetPointer でも上記と同様に ((*range*)) 引数を利用できます.


=end JA


=begin HTML
<hr /> <small>
  $Id: gthist_restart.rd,v 1.2 2009-02-26 17:01:35 morikawa Exp $
</small>
=end HTML
