=begin JA

= メッセージの出力

# * 小高正嗣 (odakker)
#   * $Id: dc_message.rd,v 1.3 2009-02-28 13:36:33 morikawa Exp $

=end JA
=begin EN

= Message dump

# * Masatsugu Odaka (odakker)
#   * $Id: dc_message.rd,v 1.3 2009-02-28 13:36:33 morikawa Exp $

=end EN


=begin JA

プログラムの実行中にさまざまなメッセージを標準出力に出力したい場合があ
ります. しかし, write 文を用いて読みやすい出力を行うためには, 書式指定
を工夫する必要があります. さらに変数値を含む文字列を出力したい場合には, 
文字列の連結操作や型変換を行うなどの処理が必要となり面倒です.

gtool5 にはメッセージ出力を行うサブルーチンを提供するモジュール 
((<dc_message|URL:../code_reference/classes/dc_message.html>)) が用意
されています. このモジュールに含まれる ((<MessageNotify|URL:../code_reference/classes/dc_message.html#M000416>)) サブルーチンを用いると, 
メッセージ出力を簡単に行うことができます.

例として, 
((<"Fortran 90/95 汎用モジュール: 種別型パラメタの提供"|URL:dc_types.htm>)) 
で用いた((<サンプルプログラム|URL:dc_types/diffusion_3.f90>))にメッセージ出力
を加えたプログラムを示します (ソースコードは((<こちら|URL:dc_message/diffusion_4.f90>))). 
ここでは変数 sigma の値に応じて三種類のメッセージを出力させます.
赤字(カラーがでない場合はボールド)が dc_messege に関係している箇所です.

=end JA
=begin EN

In many cases, it is necessary to dump message on standard output
during program execution. However, well sophisticated WRITE and FORMAT
statements are required for readable message. Moreover, 
when the message includes variables of not character literal constants, 
the conversion of type and concatenation are required, which are 
troublesome for programmers.

((<Dc_message|URL:../code_reference/classes/dc_message.html>)) module
of gtool5 provides subroutine for message dump. 
By using the dc_message module, readable message dump is realized
in user's programs with ease.

For example, a sample program using the dc_message module
(((<diffusion_4.f90|URL:dc_message/diffusion_4.f90>))) is shown here, which are
modified from ((<diffusion_3.f90|URL:dc_types/diffusion_3.f90>)) in ((<"
Fortran 90/95 general-purpose modules: Type parameter
specification"|URL:dc_types.htm.en>)).  Statements with colored font
(or bold font) are associated with the dc_message module.

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
program diffusion_4

  use gtool_history                                   ! Access module (モジュール指定)
  use dc_types, only : DP                           ! Access module (モジュール指定)
  <b><font color="red">use dc_message, only : MessageNotify              ! Access module (モジュール指定)</font></b>

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)
  real(DP)               :: sigma                   ! Parameter (計算安定条件パラメタ)

  tinit = 0.0                                       ! Set initial Time 
                                                    ! (初期時刻設定)

  sigma = kappa*dt/dx**2.0d0

  <b><font color="red">if ( sigma >= 0.5d0 ) then
    call MessageNotify( "E", &amp;                      ! Error message dump 
      &amp;                 "diffusion_4", &amp;            ! (エラーメッセージ出力)
      &amp;                 "dt is too large: k*dt/(dx)^2 = %f", &amp;
      &amp;                  d=(/sigma/) )
  else if ( sigma >= 0.4d0 ) then
    call MessageNotify( "W", &amp;                      ! Warning message dump 
      &amp;                 "diffusion_4", &amp;            ! (警告メッセージ出力)
      &amp;                 "dt is moderately large: k*dt/(dx)^2 = %f", &amp;
      &amp;                 d=(/sigma/) )
  else
    call MessageNotify( "M", &amp;                      ! Message dump 
      &amp;                 "diffusion_4", &amp;            ! (メッセージ出力)
      &amp;                 "dt is sufficiently small: k*dt/(dx)^2 = %f", &amp;
      &amp;                  d=(/sigma/) )
  end if</font></b>
    
  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value (初期値設定)

  call HistoryCreate( &amp;                             ! Create ouptpu file 
    &amp; file='diffusion_4.nc', &amp;                      ! (ヒストリー作成)
    &amp; title='Diffusion equation',                        &amp;
    &amp; source='Sample program of gtool_history/gtool5',   &amp;
    &amp; institution='GFD_Dennou Club davis project',       &amp;
    &amp; dims=(/'x','t'/), dimsizes=(/nx,0/),               &amp;
    &amp; longnames=(/'X-coordinate','time        '/),       &amp;
    &amp; units=(/'m','s'/),                                 &amp;
    &amp; origin=real(tinit), interval=real(ndisp*dt) )

  call HistoryPut('x',x)                            ! Output 'x' (次元変数出力)

  call HistoryAddVariable( &amp;                        ! Set output variable (変数定義)
    &amp; varname='temp', dims=(/'x','t'/), &amp;
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
end program diffusion_4
</pre>
=end HTML

=begin JA

このプログラムを実行すると, 以下のようなメッセージが追加されるようになります.

     \*** WARNING [diffusion_3] ***  dt is moderately large: k*dt/(dx)^2 = 0.4205000231056476

=end JA

=begin JA

上記のプログラムにおいて追加された dc_message モジュール サブルーチン
が行っていることを大まかに説明します. より詳しい説明は
((<リファレンスマニュアル|URL:../code_reference/classes/dc_message.html#M000416>)) を参照してください.

: use dc_message

  モジュールの使用を宣言します. Fortran90 メインプログラムの先頭にいれ
  ましょう. ここでは only 文を用いて
  ((<MessageNotify|URL:../code_reference/classes/dc_message.html#M000416>)) 
  サブルーチンだけを参照します. 


: call MessageNotify(level, where, message, [i], [r], [d], [L], [n], [c1], [c2], [c3])

  メッセージを出力します. 各引数の意味は以下の通りです.

  * ((*level*)) はメッセージの種類を指定する引数です. 
    * "M" (または "Message" など "M" で始まる文字列): 通常メッセージ
    * "W" (または "Warning" など "W" で始まる文字列): 警告メッセージ
    * "E" (または "Error" など "E" で始まる文字列): エラーメッセージ. 
      これを指定するとメッセージ出力後, プログラムを強制終了します. 

  * ((*where*)) は出力箇所を示すプログラムまたはサブルーチン名を指定する引数です.

  * ((*message*)) は出力したい文字列を指定する引数です. 
    文字列中に変数を含めることができます. ここでは倍精度実数型変数
    sigma の値を出力させるため, 変数指示詞 ((*%f*)) を用いています. 
    変数指示詞の詳細については ((<dcstringprintf.f90|URL:../code_reference/files/dcstringsprintf_f90.html>)) を参照してください.

    
  * ((*[i], [r], [d], [L], [n], [c1], [c2], [c3]*)) はオプション引数です.
    ((*message*)) に指定した文字列中の変数の値を指定します. 
    ここで倍精度実数型変数用のオプション引数 ((*[d]*)) に
    変数 sigma を代入しています. オプション引数の詳細については
    ((<dc_string|URL:../code_reference/classes/dc_string.html>)) 
    モジュール中の
    ((<Cprintf|URL:../code_reference/classes/dc_string.html#M000818>)) 
    関数を参照してください.


=end JA


=begin EN

Summary of dc_message module and its subroutines used in the sample
program are as follows. In detail, please see ((<gtool5 reference
manual|URL:../code_reference/classes/dc_message.html#M000416>)).

: use dc_message
 
  Access dc_message module. This statement is located at the beginning
  of main program. In this case, only
  ((<MessageNotify|URL:../code_reference/classes/dc_message.html#M000416>)) 
  subroutine is accessed by ONLY option.

: call MessageNotify(level, where, message, [i], [r], [d], [L], [n], [c1], [c2], [c3])

  Dump message. Descriptions of each argument are as follows.

  * ((*level*)) specifies the kind of message. 
    * "M" (or "Message"): standard message
    * "W" (or "Warning"): warning message
    * "E" (or "Error" ) : error message. 
      When this argument are specified, the program is terminated.

  * ((*where*)) specifies the program or subroutine name where 
    the MessageNotify subroutine is called.

  * ((*message*)) specifies the string of dump message,
    which can include variables. 
    In this case,
    double precision literal constant "sigma" 
    is included in the dump message by using 
    assignment argument ((*%f*)). 
    In details of assignment arguments, please see ((<dcstringprintf.f90|URL:../code_reference/files/dcstringsprintf_f90.html>)).

    
  * ((*[i], [r], [d], [L], [n], [c1], [c2], [c3]*)) are optional arguments
    which specify the variable included in the string assigned in
    ((*message*)).
    In this case, optional argument ((*[d]*)) for 
    double precision literal constant is used.
    In details of the optional arguments, 
    please see 
    ((<Cprintf|URL:../code_reference/classes/dc_string.html#M000818>)) 
    subroutine of 
    ((<dc_string|URL:../code_reference/classes/dc_string.html>)) module.



=end EN


=begin HTML
<hr /> <small>
  $Id: dc_message.rd,v 1.3 2009-02-28 13:36:33 morikawa Exp $
</small>
=end HTML
