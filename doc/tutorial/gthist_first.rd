=begin JA

= データ出力のための最低限の設定

# * 森川 靖大 (morikawa), 小高正嗣 (odakker), 石渡 正樹 (momoko)
#   * $Id: gthist_first.rd,v 1.2 2009-02-26 16:05:01 morikawa Exp $

=end JA

=begin JA

gtool_history モジュールは, 数値モデルの結果を gtool4 形式で出力するため
のインターフェースです. おもに時間積分の結果を等時間間隔で出力すること
を念頭においてます. このモジュールを用いれば, Fortran90 で書かれたプロ
グラムの 計算結果を gtool4 データ形式で出力することが簡単に実現できま
す.

以下では簡単なプログラムを例に, gtool_history モジュールの使い方を説明し
ます.


((<このプログラム|URL:gthist_first/diff_orig.f90>))
は 1 次元の熱伝導問題を解くものです. 出力結果は write 
文で行われているだけです. これに対して gtool_history を適用したのが以下
のプログラムです. 赤字(カラーがでない場合はボールド)が gtool_history に
関係している箇所です.

=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
!= Sample program for gtool_history/gtool5
!
! Solving diffusion equation
! \[
!     du/dt = \kappa d^2 u/dx^2
! \]
! for giving values of $u$ at $x=[0,1]$.
!
program diffusion
  <b><font color="red">
  use gtool_history                             ! モジュール指定
  </font></b>
  integer, parameter     :: nx=30                   ! グリッド数
  integer, parameter     :: nt=200                  ! 時間ステップ数
  integer, parameter     :: ndisp=10                ! 出力間隔
  real(8), parameter     :: dx=1.0/(nx-1)           ! グリッド間隔
  real(8), parameter     :: dt=0.0005               ! 時間間隔
  real(8), dimension(nx) :: x=(/(dx*(i-1),i=1,nx)/) ! 座標変数
  real(8), dimension(nx) :: temp                    ! 温度
  real(8), parameter     :: kappa=1.0               ! 熱拡散係数

  tinit = 0.0                                       ! 初期時刻設定

  temp = exp(-((x-0.5)/0.1)**2)                     ! 初期値設定
  <b><font color="red">
  call HistoryCreate( &amp;                        ! ヒストリー作成
    &amp; file='diffusion_1.nc', title='Diffusion equation', &amp;
    &amp; source='Sample program of gtool_history/gtool5',   &amp;
    &amp; institution='GFD_Dennou Club davis project',       &amp;
    &amp; dims=(/'x','t'/), dimsizes=(/nx,0/),               &amp;
    &amp; longnames=(/'X-coordinate','time        '/),       &amp;
    &amp; units=(/'m','s'/),                                 &amp;
    &amp; origin=real(tinit), interval=real(ndisp*dt) )

  call HistoryPut('x',x)                       ! 次元変数出力

  call HistoryAddVariable( &amp;                   ! 変数定義
    &amp; varname='temp', dims=(/'x','t'/), &amp;
    &amp; longname='temperature', units='K', xtype='double')

  call HistoryPut('temp',temp)                 ! 変数出力

  </font></b>
  do it=1,nt
    temp(2:nx-1) = temp(2:nx-1) &amp;               ! 時間積分
      &amp; + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(it,ndisp) == 0 ) then
      <b><font color="red">
      call HistoryPut('temp',temp)             ! 変数出力
      </font></b>
    endif
  enddo

  <b><font color="red">call HistoryClose</font></b>
  stop
end program diffusion
</pre>
=end HTML

=begin JA

このプログラムを ((<diffusion_1.f90|URL:gthist_first/diffusion_1.f90>))
という名前で保存し, 実際にコンパイルして実行してみましょう.

gtool5 が正しくインストールされている場合には

    $ gt5frt diffusion_1.f90

として実行ファイルを a.out を作成することができます. そこで, 

    $ ./a.out

と実行させると, ((<diffusion_1.nc|URL:gthist_first/diffusion_1.nc>))
という gtool4 データ形式のファイルが作成されます.
NetCDF のコマンド ncdump を用いて中身を見てみましょう.

    $ ncdump diffusion_1.nc | more

とすると, 変数の値だけでなく様々な情報が付加されている様子を見ることが
できます.

  netcdf diffusion_1 {
  dimensions:
          x = 30 ;
          t = UNLIMITED ; // (21 currently)
  variables:
          float x(x) ;
                  x:long_name = "X-coordinate" ;
                  x:units = "m" ;
          float t(t) ;
                  t:long_name = "time" ;
                  t:units = "s" ;
          double temp(t, x) ;
                  temp:long_name = "temperature" ;
                  temp:units = "K" ;

  // global attributes:
                  :Conventions = "http://www.gfd-dennou.org/library/gtool4/conventions/" ;
                  :gt_version = "4.3" ;
                  :title = "Diffusion equation" ;

                             : 

   x = 0, 0.03448276, 0.06896552, 0.1034483, 0.137931, 0.1724138, 0.2068965,

                             : 

   t = 0, 0.005, 0.01, 0.015, 0.02, 0.025, 0.03, 0.035, 0.04, 0.045, 0.05, 

   temp =
    1.38879542122922e-11, 3.87761921792298e-10, 8.53515772443671e-09, 
      1.48107636541387e-07, 2.02610925813017e-06, 2.18508219965393e-05, 
      0.000185777111465519, 0.00124519148341671, 0.00657960142498993, 


次にこのデータを描画してみましょう. 描画の方法は
((<GAVE を用いたお手軽描画|URL:gthist_gave.htm>)) を参照してください. 

=end JA


=begin HTML
<hr /> <small>
  $Id: gthist_first.rd,v 1.2 2009-02-26 16:05:01 morikawa Exp $
</small>
=end HTML
