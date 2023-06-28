=begin JA

= 属性(attribute)をつける

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: gthist_attr.rd,v 1.2 2009-02-26 16:22:21 morikawa Exp $

=end JA

=begin JA

gtool5 では変数に属性をつけて情報を記録することができます. 記録は

      call HistoryAddAttr(varname, attrname, value)

を呼ぶだけでできます. 第 1 引数が属性をつける変数名, 第 2 引数が属性名,
第 3 引数が属性の値です. 属性の値の型と次元は任意です. 文字型, 整数,
単精度実数, 倍精度実数, あるいはそれらの配列を属性の値として
記録することができます.


例えば経度座標が周期的座標であることを表す属性値を付加する際には,

     call HistoryAddAttr('lon','topology', 'circular')

とすれば良いです.

出力ファイル全体に対する属性 (NetCDF でいうところの大域属性, global
attribute) を指定するには, 属性名の先頭に ((*'+'*)) をつけて,
適当な変数に対して出力することになります.

     call HistoryAddAttr('temp','+calendar', 'noleap')


属性をつける変数として 'temp' としているにもかかわらず 属性名に
((*'+'*)) がついてますので,
ファイルには大域属性として出力されることになります.

実際に上記のサブルーチンを用いているプログラムの例を
((<こちら|URL:gthist_attr/diffusion_attr.f90>)) に示します.
このプログラムでは, 以下のように HistoryAddAttr を用いています. 

     call HistoryAddAttr('temp','gt_graph_tick_all',1)
     call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
     call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

では, このファイルを以下のようにコンパイルおよび実行してみましょう.

  $ gt5frt diffusion_attr.f90
  $ ./a.out

((<diffusion_attr.nc|URL:gthist_attr/diffusion_attr.nc>))
ファイルが作成されたはずです. このファイルの
ヘッダ部分を見てみます. (ncdump に -h オプションを指定すると,
データのヘッダ部分のみ表示します).

  $ ncdump -h diffusion_attr.nc

以下のように, 変数 temp には gt_graph_tick_all 等の属性が,
また大域属性として gt_user_davis_kappa が付加されているのが分かります. 

  netcdf diffusion_attr {
  dimensions:
          x = 30 ;
          t = UNLIMITED ; // (21 currently)
  variables:
                        :
                        :
          double temp(t, x) ;
                        :
                  temp:gt_graph_tick_all = 1 ;
                  temp:gt_graph_contour_spacing = 0.f, 1.f, 0.01f ;

  // global attributes:
                  :Conventions = "http://www.gfd-dennou.org/library/gtool4/conventions/" ;
                        :
                        :
                          "" ;
                  :gt_user_davis_kappa = 1. ;
  }


=end JA


=begin HTML
<hr /> <small>
  $Id: gthist_attr.rd,v 1.2 2009-02-26 16:22:21 morikawa Exp $
</small>
=end HTML
