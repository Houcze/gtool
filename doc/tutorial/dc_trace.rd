=begin JA

= デバッグ補助

# * 小高正嗣 (odakker)
#   * $Id: dc_trace.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $

=end JA
=begin EN

= Debug support

# * 小高正嗣 (odakker)
#   * $Id: dc_trace.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $

=end EN

=begin JA

gtool5 ライブラリには, 
デバッグ時の原因を追跡するサブルーチンを提供するモジュール 
((<dc_trace|URL:../code_reference/classes/dc_trace.html>)) が用意され
ています. これを用いるとデバッグ時のプログラムの追跡を比較的容易に
行うことが可能となります. 

例として, 
((<"Fortran 90/95 汎用モジュール: メッセージの出力"|URL:dc_message.htm>)) 
で用いた((<サンプルプログラム|URL:dc_message/diffusion_4.f90>))に
デバッグ出力を加えたプログラムを示します 
(ソースコードは((<こちら|URL:dc_trace/diffusion_6.f90>))). 
赤字(カラーがでない場合はボールド)が dc_trace に関係している箇所です.

=end JA
=begin EN

((<Dc_trace|URL:../code_reference/classes/dc_trace.html>)) module of 
gtool5 provides subroutines useful for debugging user's programs.
By using dc_trace module, it is easy to trace user's program in debugging.

For example, a sample program using the dc_clock module
(((<diffusion_6.f90|URL:dc_trace/diffusion_6.f90>))) is shown here, which are
modified from ((<diffusion_4.f90|URL:dc_message/diffusion_4.f90>)) in ((<"
Fortran 90/95 general-purpose modules: Message
dump"|URL:dc_message.htm.en>)).  Statements with colored font (or bold
font) are associated with the dc_clock module.

=end EN



=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
!= Sample program for gtool_history/gtool5
!
! * 2007/06/27 M.Odaka
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
program diffusion_6

  use gtool_history                                   ! Access module (モジュール指定)
  use dc_types, only : DP                           ! Access module (モジュール指定)
  use dc_message, only : MessageNotify              ! Access module (モジュール指定)
  <b><font color="red">use dc_trace, only : SetDebug, DbgMessage         ! Access module (モジュール指定)</font></b>

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)
  real(DP)               :: sigma                   ! Parameter (計算安定条件パラメタ)

  <b><font color="red">call SetDebug                                     ! Debug mode on 
                                                    ! (デバッグモードオン)

  call DbgMessage(fmt="*** Debug Message [diffusion_6] *** Debug Message On")
                                                    ! Debug message dump 
                                                    ! (デバッグ出力)  </font></b>

  tinit = 0.0                                       ! Set initial time 
                                                    ! (初期時刻設定)

  sigma = kappa*dt/dx**2.0d0

  if ( sigma >= 0.5d0 ) then
    call MessageNotify( "E", &amp;                      ! Error mesage dump 
      &amp;                 "diffusion_6", &amp;            !(エラーメッセージ出力 )
      &amp;                 "dt is too large: k*dt/(dx)^2 = %f", &amp;
      &amp;                  d=(/sigma/) )
  else if ( sigma >= 0.4d0 ) then
    call MessageNotify( "W", &amp;                      ! Warning message dump
      &amp;                 "diffusion_6", &amp;            ! (警告メッセージ出力)
      &amp;                 "dt is moderately large: k*dt/(dx)^2 = %f", &amp;
      &amp;                 d=(/sigma/) )
  else
    call MessageNotify( "M", &amp;                      ! Message dump 
      &amp;                 "diffusion_6", &amp;            ! (メッセージ出力) 
      &amp;                 "dt is sufficiently small: k*dt/(dx)^2 = %f", &amp;
      &amp;                  d=(/sigma/) )
  end if
    
  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &amp;                             ! Create output file 
    &amp; file='diffusion_6.nc', &amp;                      ! (ヒストリー作成)
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

  
    <b><font color="red">call DbgMessage(fmt="*** Debug Message [diffusion_6] *** Begin time integration")
                                                    ! Debug message dump
                                                    ! (デバッグ出力) </font></b>
  
    do it=1,nt
  
      temp(2:nx-1) = temp(2:nx-1) &amp;                 ! Time integration (時間積分)
        &amp; + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt
  
      if ( mod(it,ndisp) == 0 ) then
        call HistoryPut('temp',temp)                ! Output 'temp (変数出力)
      endif
  
      <b><font color="red">call DbgMessage(fmt="*** Debug Message [diffusion_6] *** it=%d, &amp;
        &amp;             i= (/it/) )               
                                                    ! Debug message dump
                                                    ! (デバッグ出力) </font></b>
  
    end do
  
    <b><font color="red">call DbgMessage(fmt="*** Debug Message [diffusion_6] *** End time integration")                           
                                                    ! Debug message dump
                                                    ! (デバッグ出力) </font></b>
  
  call HistoryClose
  
  stop
  end program diffusion_6
  
</pre>
=end HTML
  
=begin JA
  
このプログラムでは, 時間積分ループ部分の前後とループ内にメッセージを出力
しています. メッセージは標準エラー出力に出力されます. 出力例は以下です. 
gtool5 ライブラリの提供するサブルーチン内のデバッグ出力もオンになります.

=end JA
=begin EN

In this program, debug messages are dumped before and after the loop
of time integration. The messages are displayed on standard error out as
follows. Debug messages included subroutines of gtool5 library are
also dumped.

=endn EN

=begin  
  
    #SetDebug: dbg = 0
    #*** Debug Message [diffusion_5] *** Debug Message On
     *** WARNING [diffusion_6] ***  dt is moderately large: k*dt/(dx)^2 = 0.4205000231056476
    #call HistoryCreate1 version=<$Name:  $$Id: dc_trace.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $> : file=diffusion_6.nc ndims=2
    #|-dims(:)=x, t, dimsizes(:)=30, 0, longnames(:)=X-coordinate, time, units(:)=m, s
    #| call DCDateTimeCreate1 : current_time_used=<yes>
    #| end DCDateTimeCreate1 : time (caltype=4, day=733253, sec=5976., zone=+09:00, day_seconds=86400.)
    #| call DCDateTimeEval1
    #| end DCDateTimeEval1
    #| call GTVarCreateD version=<$Name:  $$Id: dc_trace.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $> : url=<diffusion_6.nc@x> length=30
    #| | call ANVarCreateD : url=<diffusion_6.nc@x>, xtype=<>, length=<30>
    #| | | call ANFileOpen : writable=yes overwrite=yes file=diffusion_6.nc
     *** MESSAGE [ANFileOpen] ***  diffusion_6.nc is overwritten.
    #| | | end ANFileOpen : id=4 stat=0
    #| | |-anfiledefinemode 4
    #| | |-an_vartable.add: added 1
    #| | end ANVarCreateD : stat=0
    #| |-[gt_variable 1: ndims=1, map.size=1]
    #| |-[dim1 dimno=1 ofs=0 step=1 all=30 start=1 count=30 stride=1 url=]
    #| |-[vartable 1: class=netcdf cid=1 ref=1]
    #| |-[AN_VARIABLE(file=4, var=1, dim=1)]
    #| end GTVarCreateD : class=netcdf mapid=1
  
=end 

  
=begin JA

以下では dc_trace モジュール サブルーチンが行っていることを大まかに説明します.
ここで扱っていないサブルーチンの機能を含むより詳しい説明は
((<リファレンスマニュアル|URL:../code_reference/classes/dc_trace.html>)) 
を参照してください.
  
: use dc_trace
 
  モジュールの使用を宣言します. Fortran90 メインプログラムの先頭にいれ
  ましょう. ここでは only 文を用いて必要なブルーチンだけを参照します. 
  
  
: SetDebug
  
  デバッグモードをオンにします. これにより, DbgMessage, BeginSub, EndSub
  などのサブルーチンが有効になります. 
  
  
: DbgMessage(fmt, [i], [r], [d], [L], [n], [c1], [c2], [c3], [ca])
  
  SetDebug が call された場合に, 引数 ((*fmt*)) に与えられた文字列を
  標準エラー出力に出力します. ((*fmt*)) 内に変数値を含めることもできます.
  その場合, オプション引数に対応する型の変数を指定します.
  
  各引数の意味は以下の通りです. 
    
  * ((*fmt*)) は書式を指定する引数です.
    ここでは整数型変数 it の値を出力させるため, 変数指示詞 ((*%i*)) 
    を用いています. 
    変数指示詞の詳細については ((<dcstringprintf.f90|URL:../code_reference/files/dcstringsprintf_f90.html>)) を参照してください.

  * ((*[i], [r], [d], [L], [n], [c1], [c2], [c3]*)) はオプション引数で, 
    ((*fmt*)) 内の変数値を指定します. 
    ここでは整数型変数用のオプション引数 ((*[d]*)) に
    変数 it を代入しています. オプション引数の詳細については
    ((<dc_string|URL:../code_reference/classes/dc_string.html>)) 
    モジュール中の
    ((<Cprintf|URL:../code_reference/classes/dc_string.html#M000818>)) 
    関数を参照してください.


=end JA
=begin EN

Summary of dc_drace module and its subroutines used in the sample
program are as follows. In detail, please see ((<gtool5 reference
manual|URL:../code_reference/classes/dc_trace.html>)).

  
: use dc_trace

  Access dc_trace module. This statement is located at the beginning
  of main program. In this case, ONLY option is used.
  
  
: SetDebug

  Turn on debug mode. By calling this subroutine, DbgMessage, BeginSub, and 
  EndSub subroutines of dc_trace module can be used.
  
  
: DbgMessage(fmt, [i], [r], [d], [L], [n], [c1], [c2], [c3], [ca])

  Dump string given as argument ((*fmt*)) on standard error out 
  when SetDebug subroutine is called.
  The argument ((*fmt*)) can include variables which are specified 
  by corresponding type optional arguments.

  Descriptions of each argument are as follows. 

  * ((*fmt*)) specifies format of dumped string. 
    In this case,
    integer literal constant "it" 
    is included in the dump message by using 
    assignment argument ((*%d*)). 
    In details of assignment arguments, please see ((<dcstringprintf.f90|URL:../code_reference/files/dcstringsprintf_f90.html>)).

  * ((*[i], [r], [d], [L], [n], [c1], [c2], [c3]*)) are optional arguments
    which specify the variable included in the string assigned in
    ((*fmt*)).
    In this case, optional argument ((*[i]*)) for 
    integer literal constant is used.
    In details of the optional arguments, 
    please see 
    ((<Cprintf|URL:../code_reference/classes/dc_string.html#M000818>)) 
    subroutine of 
    ((<dc_string|URL:../code_reference/classes/dc_string.html>)) module.


=end EN





=begin HTML
<hr /> <small>
  $Id: dc_trace.rd,v 1.3 2009-02-28 13:36:32 morikawa Exp $
</small>
=end HTML
