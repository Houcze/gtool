=begin JA

= コマンドライン引数の解析

# * 小高正嗣 (odakker2), 森川靖大 (morikawa)
#   * $Id: dc_args.rd,v 1.3 2009-02-28 13:36:33 morikawa Exp $

=end JA
=begin EN

= Command line arguments interpretation

# * 小高正嗣 (odakker2), 森川靖大 (morkawa)
#   * $Id: dc_args.rd,v 1.3 2009-02-28 13:36:33 morikawa Exp $

=end EN


=begin JA

gtool5 には実行プログラムのコマンドライン引数を解析する
するサブルーチンを提供するモジュール 
((<dc_args|URL:../code_reference/classes/dc_args.html>)) が用意され
ています. これを用いるとプログラム実行時のオプション指定, 
ヘルプメッセージの表示などが可能になります. 

例として, 
((<"Fortran 90/95 汎用モジュール: 種別型パラメタの提供"|URL:dc_types.htm>)) 
で用いた((<サンプルプログラム|URL:dc_types/diffusion_3.f90>))に
コマンドライン引数を与えるように修正したプログラムを示します 
(ソースコードは((<こちら|URL:dc_args/diffusion_7.f90>))). 
赤字(カラーがでない場合はボールド)が dc_args に関係している箇所です.
関連して必要となる 
((<dc_types|URL:../code_reference/classes/dc_types.html>)),
((<dc_string|URL:../code_reference/classes/dc_string.html>)),
((<dc_message|URL:../code_reference/classes/dc_message.html>))
モジュールで提供されているデータ型とサブルーチンも use 文で参照しています. 

=end JA
=begin EN

((<Dc_args|URL:../code_reference/classes/dc_args.html>)) module of gtool5
provides subroutines for interpretation of command line arguments.
By using dc_args, run time options and help message can be specified 
to user's program. 

For example, a sample program using the dc_args module
(((<diffusion_7.f90|URL:dc_args/diffusion_7.f90>))) is shown here, which are
modified from ((<diffusion_3.f90|URL:dc_types/diffusion_3.f90>)) in ((<"
Fortran 90/95 general-purpose modules: Type parameter
specification"|URL:dc_types.htm.en>)).  Statements with colored font
(or bold font) are associated with the dc_message module.

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
program diffusion_7

  use gtool_history                                   ! Access module (モジュール指定)
  use dc_types, only : DP, STRING                   ! Access module (モジュール指定)
  use dc_message, only: MessageNotify               ! Access module (モジュール指定)
  <b><font color="red">use dc_string, only : StoA, StoI                  ! Access module (モジュール指定)
  use dc_args, only: ARGS, DCArgsOpen, DCArgsClose, DCArgsOption, &
    & DCArgsPutLine, DCArgsDebug, DCArgsHelp, DCArgsStrict
                                                    ! Access module (モジュール指定)</font></b>

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)
  <b><font color="red">type(ARGS)             :: arg                     ! Command line argument (引数)
  logical                :: OPT_step                ! logical parameter (引数用論理変数)
  character(STRING)      :: VAL_step                ! Value of command line argument (引数の値)</font></b>

  <b><font color="red">call DCArgsOpen(arg)                       ! Initialize variable 'arg'
                                             ! (引数の初期化) 
  call DCArgsOption(arg, StoA('-S', '--step'), OPT_step, VAL_step, &amp;
    &amp;         help="Specify time step (nt) [default value is 200]." )
                                             ! Set "-S/--step" option
                                             ! ("-S/--step" オプションの設定)

  call DCArgsDebug(arg)                      ! Set debug option
                                             ! (デバッグオプションの自動設定)
  call DCArgsHelp(arg)                       ! Set help option
                                             ! ヘルプオプションの自動設定
  call DCArgsStrict(arg)                     ! Set exception handling
                                             ! (無効なオプション指定時に警告表示)

  if (OPT_step) then
     nt = StoI(VAL_step)                     ! Set "nt" specified by command line argument 
                                             ! (引数の値を入力)

     call MessageNotify( "M", "diffusion_7", &amp;
       &amp;                 "Time step is %d", i=(/nt/) )
                                             ! Message dump
                                             ! (メッセージの出力)
  end if

  call DCArgsClose(arg)                      ! Finalize variable 'arg'
                                             ! (引数の使用終了)</font></b>


  tinit = 0.0                                       ! 初期時刻設定

  temp = exp(-((x-0.5)/0.1)**2)                     ! 初期値設定

  call HistoryCreate( &amp;                             ! ヒストリー作成
    &amp; file='diffusion_7.nc', title='Diffusion equation', &amp;
    &amp; source='Sample program of gtool_history/gtool5',   &amp;
    &amp; institution='GFD_Dennou Club davis project',       &amp;
    &amp; dims=(/'x','t'/), dimsizes=(/nx,0/),               &amp;
    &amp; longnames=(/'X-coordinate','time        '/),       &amp;
    &amp; units=(/'m','s'/),                                 &amp;
    &amp; origin=real(tinit), interval=real(ndisp*dt) )

  call HistoryPut('x',x)                            ! 次元変数出力

  call HistoryAddVariable( &amp;                        ! 変数定義
    &amp; varname='temp', dims=(/'x','t'/), &amp;
    &amp; longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! 変数出力

  do it=1,nt
    temp(2:nx-1) = temp(2:nx-1) &amp;                   ! 時間積分
      &amp; + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(it,ndisp) == 0 ) then
      call HistoryPut('temp',temp)                  ! 変数出力
    endif
  enddo

  call HistoryClose
  stop
end program diffusion_7
</pre>
=end HTML


=begin JA

このプログラムではコマンドラインオプションで計算ステップ数を指定できる
ようにしています. また, デバッグモードでの実行 
( ((<Fortran90 汎用ライブラリ: (4) デバッグ補助|URL:dc_trace.htm>)) 参照), 
ヘルプメッセージの出力もオプションで指定できます.

  * 計算ステップ数の指定はプログラム実行時に

      $ ./a.out -S=<計算ステップ数>

    または

      $ ./a.out --step=<計算ステップ数>

    とします.

  * デバッグモードで実行する場合

      &amp; ./a.out -D

    とします. デバッグメッセージはデフォルトでは標準出力に出力されます.

  * ヘルプメッセージの出力は

      $ a.out -H

    とします. このときプログラムは以下のようなヘルプメッセージを表示
    して終了します. 

      Options::
       -S=VAL, --step=VAL
           Specify time step (nt) [default value is 200].

       -D=VAL, --debug=VAL
           call dc_trace#SetDebug (display a lot of messages for debug).
           VAL is unit number (default is standard output)

       -h=VAL, -H=VAL, --help=VAL
           display this help and exit. VAL is unit number (default is
           standard output)

=end JA
=begin EN

In this program, the time step number 'no' can be specified by command
line argument.
Execution in debug mode ( ((<"Fortran 90/95 general-purpose modules: (4) Debud support"|URL:dc_trace.htm>)) ), and help message can be also specified.


  * The time step number is specified as follows

      $ ./a.out -S=<time step number>

    or, 

      $ ./a.out --step=<time step number>


  * Debug option is specified as follows.

      $ ./a.out -D

    Debug messages are dumped to standard out.

  * Help message can be displayed as follows.

      $ a.out -H

    If the '-H' is given, following messages are displayed.

      Options::
       -S=VAL, --step=VAL
           Specify time step (nt) [default value is 200].

       -D=VAL, --debug=VAL
           call dc_trace#SetDebug (display a lot of messages for debug).
           VAL is unit number (default is standard output)

       -h=VAL, -H=VAL, --help=VAL
           display this help and exit. VAL is unit number (default is
           standard output)

=end EN


=begin JA

以下では dc_args モジュール サブルーチンが行っていることを大まかに説明します.
ここで扱っていないサブルーチンの機能を含むより詳しい説明は
((<リファレンスマニュアル|URL:../code_reference/classes/dc_args.html>)) 
を参照してください.


: use dc_args

  モジュールの使用を宣言します. Fortran90 メインプログラムの先頭にいれ
  ましょう. ここでは only 文を用いて必要なデータ型とブルーチンだけを参照します. 

: types(ARGS)

  構造型 ARGS 変数の宣言文です. 引数情報は構造型 ARGS 変数に格納されます. 


: character(STRING)

  種別型パラメタ STRING を用いた文字変数の宣言文です. 引数の値を格納する
  文字型変数の種別型パラメタは, STRING にする必要があります. 
  種別型パラメタ STRING は
  ((<dc_types|URL:../code_reference/classes/dc_typs.html>)) モジュールで
  提供されているので,  メインプログラムの先頭部分で, use 文を用いて参照
  します. 


: DCArgsOpen(arg)

  構造型 ARGS 変数 arg の初期化を行います.


: DCArgsOption(arg, options, flag, [value], [help])

  オプション情報の登録と取得を行います. 引数の意味は以下の通りです.

  * ((*arg*)) は第二引数以下のオプション情報を格納する構造型 ARGS 
    変数を指定する引数です. 

  * ((*options*)) はオプション文字列を指定する引数です. 
    上記の例では '-S' と '--step' の二通りで与えられるようにしています 
    (((<StoA|URL:../code_reference/classes/dc_string.html#M000800>)) は
    ((<dc_string|URL:../code_reference/classes/dc_string.html>))
    モジュールで用意されている型変換関数です). 
    
  * ((*flag*)) はオプションが与えられているか否かを判定するための
    論理型変数を指定する引数です.
    ((*options*)) がコマンドライン引数に与えられると .true. を返し,
    そうでない場合は .false. を返します.

  * ((*[value]*)) はオプションに値が指定されていた場合にその値を格納する
    変数を指定する引数です. 
 
  * ((*[help]*)) はオプションに関するヘルプメッセージを指定する引数です.
    サブルーチン Help を用いた際にこのメッセージが出力されます.


: DCArgsDebug(arg)

  デバッグオプション -D および --debug を自動設定します.     


: DCArgsHelp(arg)

  ヘルプオプション -H および --help を自動設定します.
  このサブルーチンを使う場合は事前に Option, Debug サブルーチンを呼ぶ
  必要があります.


: DCArgsStrict(arg, [severe])

  オプションに与えられた文字列が Option サブルーチンで設定されていない
  場合に警告を返します. 引数 ((*[severe]*)) を .true. にしておくと
  エラーを返しプログラムを強制終了します. 
  このサブルーチンを使う場合は事前に Option, Debug, Help サブルーチンを
  呼ぶ必要があります.


: DCArgsClose(arg)

  構造型 ARGS 変数 arg の使用を終了します.
  

=end JA
=begin EN

Summary of dc_args module and its subroutines used in the sample
program are as follows. In detail, please see ((<gtool5 reference
manual|URL:../code_reference/classes/dc_args.html>)).


: use dc_args

  Access dc_args module. This statement is located at the beginning
  of main program. In this case, ONLY option is used.


: types(ARGS)

  Definition of ARGS derived data type variable.
  Information of command line arguments is stored this type variable.


: character(STRING)

  Definition of character literal constant with STRING type parameter.
  The type parameter of character literal constant which store
  the value of command line arguments must be STRING.  
  The type parameter STRING is provided by 
  ((<dc_types|URL:../code_reference/classes/dc_typs.html>)) module.
  This module is accessed at the beginning of main program


: DCArgsOpen(arg)

  Initialize ARGS derived data type variable 'arg'.


: DCArgsOption(arg, options, flag, [value], [help])

  Define command line options. Descriptions of each argument are as follows.  

  * ((*arg*)) specifies ARGS derived data type variable which store 
    information of command line arguments.

  * ((*options*)) specifies string of command line option.
    In this case, '-S' and  '--step' are specified,
    (((<StoA|URL:../code_reference/classes/dc_string.html#M000800>)) is 
    type translation function provided by 
    ((<dc_string|URL:../code_reference/classes/dc_string.html>))
    module. 
    
  * ((*flag*)) is logical argument for judgement whether command line option 
    is specified or not.
    If ((*options*)) is specified as command line option, 
    return value of ((*flag*)) is '.true.', If not, return value 
    ((*flag*)) is '.false.'

  * ((*[value]*)) is optional argument which store the value of
    command line option.
 
  * ((*[help]*)) is optional argument which specifies help message.
    This message is displayed when -H or --help options are specified.


: DCArgsDebug(arg)

  Set debug option (-D/--debug) automatically.


: DCArgsHelp(arg)

  Set help option (-H/--help) automatically.
  To call this subroutine, Option and Debug subroutine must be 
  called previously.


: DCArgsStrict(arg, [severe])

  Show warning message if specified command line argument is not defined
  by Option subroutine. 
  If the argument ((*[severe]*)) is '.true.' program is terminated.
  To call this subroutine, Option, Debug and Help subroutine must be 
  called previously.


: DCArgsClose(arg)

  Finalize ARGS derived data type variable 'arg'.

  

=end EN



=begin HTML
<hr /> <small>
  $Id: dc_args.rd,v 1.3 2009-02-28 13:36:33 morikawa Exp $
</small>
=end HTML

 LocalWords:  HTML hr dc args
