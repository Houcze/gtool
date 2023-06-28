=begin JA

= 種別型パラメタの提供

# * 小高正嗣 (odakker), 森川靖大 (morikawa)
#   * $Id: dc_types.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $

=end JA
=begin EN

= Type parameter specification

# * Masatsugu ODAKA (odakker)
#   * $Id: dc_types.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $

=end EN

=begin JA

実数型変数の精度や文字型変数の文字数を指定する際に, Fortran90 
では種別型パラメタを用いることができます. 
gtool5 には標準的な種別型パラメタを提供するモジュール 
((<dc_types|URL:../code_reference/classes/dc_types.html>)) が用意
されています. これを用いると標準的な種別型パラメタを自分で定義する
ことなく利用することが可能となります. 

例として, ((<"データの入出力: 属性をつける"|URL:gthist_attr.htm>)) 
で用いた((<サンプルプログラム|URL:gthist_attr/diffusion_attr.f90>))において, 
実数型変数の精度を種別型パラメタを用いて指定したプログラムを示します
(ソースコードは((<こちら|URL:dc_types/diffusion_3.f90>))). 
赤字(カラーがでない場合はボールド)が dc_types に関係している箇所です.

=end JA
=begin EN

In Fortran 90 programs, the value of the kind type parameter for a
given data type can be specified by the KIND intrinsic
function. 
((<Dc_types|URL:../code_reference/classes/dc_types.html>))
module of gtool5 provides standard type parameters. 
By using the dc_types module, it is not necessary to
define type parameters in user's programs.

For example, a sample program using the dc_types module
(((<diffusion_3.f90|URL:dc_types/diffusion_3.f90>))) is shown here, which are
modified from ((<diffusion_attr.f90|URL:gthist_attr/diffusion_attr.f90>)) in ((<"Data
I/O interface: Add attribute"|URL:gthist_attr.htm>)).  Statements
with colored font (or bold font) are associated with the
dc_types module.

=end EN

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
!= Sample program for gtool_history/gtool5
!
! * 2007/06/25 M.Odaka
! * 2006/10/25 Y.Morikawa
! * 2003/08/21 M.Odaka
! * 2001/02/27 S.Takehiro
!
! Solving diffusion equation
! \[
!     du/dt = \kappa d^2 u/dx^2
! \]
! for giving values of $u$ at $x=[0,1]$.
!
program diffusion_3

  use gtool_history                                   ! Access module(モジュール指定)
  <b><font color="red">use dc_types, only : DP                           ! Access module (モジュール指定)</font></b>

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(<b><font color="red">DP</font></b>), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(<b><font color="red">DP</font></b>), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(<b><font color="red">DP</font></b>), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(<b><font color="red">DP</font></b>), dimension(nx):: temp                    ! Temperature (温度)
  real(<b><font color="red">DP</font></b>), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)

  tinit = 0.0                                       ! Set initial Time 
                                                    ! (初期時刻設定)
  
  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &amp;                             ! Create output file 
    &amp; file='diffusion_3.nc', &amp;                      ! (ヒストリー作成)
    &amp; title='Diffusion equation',                        &amp;
    &amp; source='Sample program of gtool_history/gtool5',   &amp;
    &amp; institution='GFD_Dennou Club davis project',       &amp;
    &amp; dims=(/'x','t'/), dimsizes=(/nx,0/),               &amp;
    &amp; longnames=(/'X-coordinate','time        '/),       &amp;
    &amp; units=(/'m','s'/),                                 &amp;
    &amp; origin=real(tinit), interval=real(ndisp*dt) )

  call HistoryPut('x',x)                            ! Output 'x' (次元変数出力)

  call HistoryAddVariable( &amp;                        ! Set output variable 
    &amp; varname='temp', dims=(/'x','t'/), &amp;           ! (変数定義)
    &amp; longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! Output 'temp' (変数出力)

  do it=1,nt
    temp(2:nx-1) = temp(2:nx-1) &amp;                   ! Time integration (時間積分)
      &amp; + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(it,ndisp) == 0 ) then
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif
  enddo

  call HistoryClose
  stop
end program diffusion_3
</pre>
=end HTML

=begin JA

ここでは実数型変数の精度を, dc_types モジュールが用意する
倍精度実数種別型パラメタ ((*DP*)) を用いて指定しました. 
dc_types モジュールにはこの他に整数型や文字型変数のための
種別型パラメタが用意されています. 詳しくは
((<リファレンスマニュアル|URL:../code_reference/classes/dc_types.html>)) 
を参照してください. 

=end JA

=begin EN

In this sample program, the parameter ((*DP*)) provided by dc_types
module is used as the kind type parameter for double precision real
literal constant. The dc_types module also provides 
kind type parameters for integer and character literal constants. 
In detail, please see 
((<gtool5 reference manual|URL:../code_reference/classes/dc_types.html>)). 

=end EN

=begin HTML
<hr /> <small>
  $Id: dc_types.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $
</small>
=end HTML
