=begin JA

= CPU 時間の計測

# * 小高正嗣 (odakker2), 森川靖大 (morikawa)
#   * $Id: dc_clock.rd,v 1.4 2009-02-28 16:07:56 morikawa Exp $

=end JA

=begin EN

= CPU time counting

# * 小高正嗣 (odakker2), 森川靖大 (morikawa)
#   * $Id: dc_clock.rd,v 1.4 2009-02-28 16:07:56 morikawa Exp $

=end EN


=begin JA

#プログラムの処理に要した CPU 時間を計測するには, UNIX time コマンドを
#用いる, 専用のプロファイラを用いる等の方法があります. しかし, time コ
#マンドはプログラムを実行するプロセスの経過時間を計測するため, プログラ
#ム内の個別の処理に要した CPU 時間を計測することはできません. これに対
#しプロファイラはプログラム内で利用しているサブルーチン単位での CPU 時
#間計測が可能ですが, 最適化オプションの利用と排他的な場合や, コンパイラ
#によってはそもそもプロファイラが用意されていない場合があります.

gtool5 には CPU 時間を計測するサブルーチンを提供するモジュール 
((<dc_clock|URL:../code_reference/classes/dc_clock.html>)) が用意され
ています. これを用いるとプログラム中の任意の処理に要した CPU 時間を
計測することが可能となります.

例として, 
((<"Fortran 90/95 汎用モジュール: 種別型パラメタの提供"|URL:dc_types.htm>)) 
で用いた((<サンプルプログラム|URL:dc_types/diffusion_3.f90>))に
CPU 時間の計測を加えたプログラムを示します (ソースコードは((<こちら|URL:dc_clock/diffusion_5.f90>))). 
赤字(カラーがでない場合はボールド)が dc_clock に関係している箇所です.

=end JA
=begin EN

((<Dc_clock|URL:../code_reference/classes/dc_clock.html>)) module of 
gtool5 provides subroutines for CPU time counting. 
By using dc_clock module, CPU time it takes to execute arbitrary part
of user's program can be estimated.

For example, a sample program using the dc_clock module
(((<diffusion_5.f90|URL:dc_clock/diffusion_5.f90>))) is shown here, which are
modified from ((<diffusion_3.f90|URL:dc_types/diffusion_3.f90>)) in ((<"
Fortran 90/95 general-purpose modules: Type parameter
specification"|URL:dc_types.htm.en>)).  Statements with colored font
(or bold font) are associated with the dc_clock module.

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
program diffusion_5

  use gtool_history                                   ! Access module (モジュール指定)
  use dc_types, only : DP                           ! Access module (モジュール指定)
  <b><font color="red">use dc_clock, only : CLOCK, DCClockCreate, &amp;
    &amp; DCClockClose, DCClockStart, DCClockStop, &amp;
    &amp; DCClockResult, DCClockPredict, &amp;
    &amp; operator(+)                             ! Access module (モジュール指定)</font></b>

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)
  <b><font color="red">type(CLOCK)            :: clock_init, clock_loop  ! Variables for CPU time counting 
                                                    ! CPU 時間計測用変数</font></b>

  <b><font color="red">call DCClockCreate( &amp;           ! Initialize (初期化)
    &amp; clk = clock_init, &amp;         ! (out)
    &amp; name = 'initialization' )   ! (in)
  call DCClockCreate( &amp;           ! Initialize (初期化)
    &amp; clk = clock_loop, &amp;         ! (out)
    &amp; name = 'time-integration' ) ! (in)</font></b>

  <b><font color="red">call DCClockStart( clk = clock_init ) ! (inout)   ! Start CPU time counting 
                                                    ! (CPU 時間計測開始)</font></b>

  tinit = 0.0                                       ! Set initial time 
                                                    ! (初期時刻設定)

  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &amp;                             ! Create output file 
    &amp; file='diffusion_5.nc', &amp;                      ! (ヒストリー作成) 
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

  <b><font color="red">call DCClockStop( clk = clock_init ) ! (inout)    ! Stop CPU time counting 
                                                    ! (CPU 時間計測終了)</font></b>

  do it=1,nt
    <b><font color="red">call DCClockStart ( clk = clock_loop ) ! (inout) ! Start CPU time counting 
                                                     ! (CPU 時間計測開始)</font></b>

    temp(2:nx-1) = temp(2:nx-1) &amp;                   ! Time integration (時間積分)
      &amp; + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(it,ndisp) == 0 ) then
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif

    <b><font color="red">call DCClockStop( clk = clock_loop ) ! (inout)  ! Stop CPU time counting 
                                                    ! (CPU 時間計測終了)

    call DCClockPredict( &amp;               ! Estimate remaining time (残り時間の予測)
      &amp; clk = clock_init + clock_loop, &amp; ! (in)
      &amp; progress = real(it)/real(nt) )   ! (in) </font></b>
  end do

  call HistoryClose

  <b><font color="red">call DCClockResult( &amp;                    ! Display total CPU time (全 CPU 時間の表示)
    &amp; clks = (/clock_init, clock_loop/), &amp; ! (in)
    &amp; total_auto = .true. )                ! (in)
  call DCClockClose( clk = clock_init )    ! (inout)       ! Finalize (後処理)
  call DCClockClose( clk = clock_loop )    ! (inout)       ! Finalize (後処理)</font></b>

  stop
end program diffusion_5
</pre>
=end HTML

=begin JA

このプログラムでは, CPU 時間の計測を時間積分ループ前処理部分と
時間積分ループ部分とに分けて行い, 最後に合計値を求めています. 
時間積分ループ部分では以下のようなメッセージが出力されます.

=end JA
=begin EN

In this program, CPU time for initialization part and time integration
part are counted and summation of then is estimated at the end of
program.  At the time integration loop, following messages are dumped.

=end EN

=begin
  
  ########## PREDICTION OF CALCULATION ###########
  Start Date             2007-06-26T13:53:18+09:00
  Current Date           2007-06-26T13:53:19+09:00
  Progress     52.50%  [*************            ]
  Remaining CPU TIME      0.000000E+00
  Completion Date        2007-06-26T13:53:19+09:00

=end 

=begin JA

そしてプログラム終了時には以下のようなメッセージが出力されます.

=end JA
=begin EN

When the program is terminated, following messages are dumped.

=end EN

=begin
  
  ############## CPU TIME SUMMARY ################
  initialization         0.129980E-01
  time-integration       0.179959E-01
  ------------------------------------------------
         TOTAL TIME =    0.309939E-01
 
=end

=begin JA

以下では dc_clock モジュールの各サブルーチンが行っていることを大まかに説明します.
より詳しい説明は
((<リファレンスマニュアル|URL:../code_reference/classes/dc_clock.html>)) を参照してください.

: use dc_clock

  モジュールの使用を宣言します. Fortran90 メインプログラムの先頭にいれ
  ましょう. ここでは only 文を用いて
  必要なデータ型とサブルーチンだけを参照します. なお構造型 CLOCK 変数の
  加算を行う場合には, dc_clock で定義された加算演算子 ((*+*)) を use 
  する必要があることに注意してください.


: type(CLOCK)

  構造型 CLOCK 変数の宣言文です. 計測された CPU 時間は構造型 CLOCK 変数
  に格納されます. 


: DCClockCreate(clk, name)

  構造型 CLOCK 変数を初期化します. 各引数の意味は以下の通りです.
  
  * ((*clk*)) は初期化する構造型 CLOCK 変数を指定する引数です.

  * ((*name*)) は計測内容を指定する引数です. 


: DCClockStart(clk, [err])

  計測を開始します. 計測された CPU 時間は第 1 引数で与えた ((:clk:)) 
  に格納されます.


: DCClockStop(clk, [err])

  計測を終了します. 計測された CPU 時間は Close サブルーチンで
  その構造型 CLOCK 変数の使用を終了するまで save されます. 
  同じ構造型 CLOCK 変数を引数にして再度計測を行うと, もともと格納されて
  いた値に加算された値が格納されます.

: DCClockPredict ( clk, progress, [unit], [err] )
  
  経過 CPU 時間と処理を終了したプログラムの分量をもとに, 残り CPU 
  時間の予想を行います. 各引数の意味は以下の通りです.

  * ((*clk*)) は経過 CPU 時間を格納した構造型 CLOCK 変数を指定する引数です.

  * ((*progress*)) はこのサブルーチンが call された時点で
    処理を終了したプログラムの分量の割合を指定する引数です.
    開始時を 0, 終了時を 1 として, 0 から 1 までの実数値を与えます. 
    ここではループ変数と総時間ステップ数を利用しています.

  * ((*unit*)) は出力先装置番号を指定する引数です. 何も指定しないと
    出力装置として標準出力が選択されます.

  * ((*err*)) は第 1 引数 ((*clks*)) が初期化されていなくても
    処理を続行する場合に指定する引数です. 


: DCClockResults( clks, [unit] [total_auto], [clk_total], [total_name], [err] )

  CPU 時間の合計を表示します. 

  * ((*clks*)) は構造型 CLOCK 変数の配列を指定する引数です.

  * ((*unit*)) は出力先装置番号を指定する引数です. 何も指定しないと
    出力装置として標準出力が選択されます.

  * ((*total_auto*)) は合計値の出力の有無を指定する引数です. 
    .true. を与えると ((*clks*)) に指定した構造型 CLOCK 変数の合計値を出力します.
    次のオプション引数 ((*clk_total*)) が指定されている場合は ((*clk_total*))
    の値が優先されます.


  * ((*clk_total*)) は合計する構造型 CLOCK 変数を指定します.

  * ((*total_name*)) は合計値を出力する際にそのメッセージの冒頭に表示する文字列を指定します.

  * ((*err*)) は第 1 引数 ((*clks*)) が初期化されていなくても
    処理を続行する場合に指定する引数です. 


: DCClockClose( clk )

  引数に与えた構造型 CLOCK 変数 ((*clk*))の使用を終了します.

=end JA
=begin EN

Summary of dc_clock module and its subroutines used in the sample
program are as follows. In detail, please see ((<gtool5 reference
manual|URL:../code_reference/classes/dc_clock.html>)).


: use dc_clock

  Access dc_message module. This statement is located at the beginning
  of main program. In this case, ONLY option is used.
  If addition of CLOCK derived type variables, is necessary, 
  defined operator ((*+*)) must be accessed.


: tupe(CLOCK)

  Definition of CLOCK derived data type variable.
  Counted CPU time is stored this variables


: DCClockCreate(clk, name)

  Initialize CLOCK derived data type variable.
  Descriptions of each argument are as follows.
  
  * ((*clk*)) specifies initialized CLOCK derived data type variable.

  * ((*name*)) specifies label. 


: DCClockStart(clk, [err])

  Start CPU time counting. 
  Counted CPU time is stored 1'st argument ((:clk:)).


: DCClockStop(clk, [err])

  Stop CPU time counting. 
  Value of ((:clk:)) is saved unless subroutine ((*Close*)) is called.
  If CPU time counting is started again with same CLOCK derived data 
  type variable, new counted CPU time is added to saved value.


: DCClockPredict ( clk, progress, [unit], [err] )

  Estimate remaining CPU time by using elapsed CPU time and 
  ratio of already executed part of program to remaining part. 
  volume of program. Descriptions of each argument are as follows.

  * ((*clk*)) specifies CLOCK derived data type variable where elapsed
    CPU time is stored.

  * ((*progress*)) specifies ratio of already executed part of program to 
    remaining part where the subroutine is called.
    ((*progress*)) should be given as float value between 0 and 1,
    0 means at the start point of program and 1 means at the end of program.
    In this case, ((*progress*)) is given by using loop variable and
    total time step number. 

  * ((*unit*)) specifies unit number for output.
    Its default value is standard out.

  * ((*err*)) is argument to execute if the 1'st argument 
    ((*clks*)) is not initialized.


: DCClockResults( clks, [unit] [total_auto], [clk_total], [total_name], [err] )

  Display total CPU time. Descriptions of each argument are as follows.

  * ((*clks*)) specifies allay of CLOCK derived data type variable 
    for estimation total CPU time.

  * ((*unit*)) specifies unit number for output.
    Its default value is standard out.

  * ((*total_auto*)) is logical argument whether total CPU time is displayed 
    or not. 
    If ((*total_auto*)) is ".true.",  total value of 
    CLOCK derived data type variable specified as ((*clks*)) argument
    is dis-plied.
    If optional argument ((*clk_total*)) is specified, 
    the value of ((*clk_total*)) is given priority.

  * ((*clk_total*)) specifies CLOCK derived data type variable 
    for estimation total CPU time.

  * ((*total_name*)) specifies dump message when total CPU time is displayed.

  * ((*err*)) is argument to execute if the 1'st argument 
    ((*clks*)) is not initialized.



: DCClockClose( clk )

  Finalize CLOCK derived data type variable specified as 1'st argument.


=end EN



=begin HTML
<hr /> <small>
  $Id: dc_clock.rd,v 1.4 2009-02-28 16:07:56 morikawa Exp $
</small>
=end HTML
