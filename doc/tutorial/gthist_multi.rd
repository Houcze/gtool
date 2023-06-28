=begin JA

= 複数のファイルに出力

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: gthist_multi.rd,v 1.3 2009-02-27 08:37:10 morikawa Exp $

=end JA

=begin JA

ここまでに示した例では 1 つのファイルに全ての変数と属性の出力を行いま
した. しかしながら複数のファイルへ出力することもできます.

ここでは 2 次元の熱対流を計算するプログラムを例に解説します. 出力する
変数は 2 つ(流線関数と温度)です. 1 つのファイルに出力を行うプログラム
が ((<benard_1.f90|URL:gthist_multi/benard_1.f90>)),
2 つのファイルに分けて出力を行うプログラムが 
((<benard_2.f90|URL:gthist_multi/benard_2.f90>)) です.

((<benard_2.f90|URL:gthist_multi/benard_2.f90>)) では 34 行目で 

     type(GT_HISTORY) :: hst_psi, hst_temp
 

とし, それぞれ流線関数と温度を出力するファイルを示す GT_HISTORY
型構造体変数 hst_psi と hst_temp を宣言します.

次に 77 行目と 95 行目の HistoryCreate で出力ファイル
psi.nc と temp.nc
を作成します. このとき最後の引数には 34 行目で宣言した hst_psi 
と hst_temp を代入します.

    call HistoryCreate( &                             ! ヒストリー作成
      & file='psi.nc', title='convection in rotating annulus', &
                        : 
      & history=hst_psi ) 

                        :

    call HistoryCreate( &                             ! ヒストリー作成
      & file='temp.nc', title='convection in rotating annulus', &
                        : 
      & history=hst_temp )


117 行目から 118 行目までが出力です. HistoryPut の第 3 引数には 
HistoryCreate で指定したそれぞれの出力先ファイルに対応する GT_HISTORY 
型構造体変数を代入します.

    call HistoryPut('psi',  trans_g(psi),  hst_psi)
    call HistoryPut('temp', trans_g(temp), hst_temp)


では, 実際に nc ファイルを作成してみましょう.
((<benard_1.f90|URL:gthist_multi/benard_1.f90>)) と 
((<benard_2.f90|URL:gthist_multi/benard_2.f90>)) は
c2pack モジュールを利用するので, まず
((<c2pack.f90|URL:gthist_multi/c2pack.f90>)) を
コンパイルします. 


=== c2pack.f90 と ispack_snip.f のコンパイル

((<c2pack.f90|URL:gthist_multi/c2pack.f90>)) と
((<ispack_snip.f|URL:gthist_multi/ispack_snip.f>)) を
ダウンロードし, 以下のようにコンパイルしてください. 

    $ gt5frt -c c2pack.f90 ispack_snip.f

ispack_snip.f は ((<ISPACK|URL:http://www.gfd-dennou.org/library/ispack>))
の一部のパッケージを抜粋したもので, c2pack モジュールに必要な
サブルーチンが含まれています. 

コンパイルの結果, c2pack.o, c2pack.mod, ispack_snip.o が作成されれば OK です.


=== benard_1.f90 のコンパイル

では, ((<benard_1.f90|URL:gthist_multi/benard_1.f90>))
をコンパイルしてみましょう.

     $ gt5frt benard_1.f90 c2pack.o ispack_snip.o

すると, 実行ファイルを a.out が作成されます. そこで, 

     $ ./a.out

と実行させると, benard_1.nc が作成されます.


=== benard_2.f90 のコンパイル

では, 今度は ((<benard_2.f90|URL:gthist_multi/benard_2.f90>))
をコンパイルしてみましょう. コンパイルの仕方は benard_1.f90 と同様です.

     $ gt5frt benard_2.f90 c2pack.o ispack_snip.o

できた実行ファイル a.out を実行してみましょう. 

     $ ./a.out

すると今度は, 流線関数と温度のデータがそれぞれ,
((<psi.nc|URL:gthist_multi/psi.nc>)) と
((<temp.nc|URL:gthist_multi/temp.nc>))
に出力されます.

=end JA


=begin HTML
<hr /> <small>
  $Id: gthist_multi.rd,v 1.3 2009-02-27 08:37:10 morikawa Exp $
</small>
=end HTML
