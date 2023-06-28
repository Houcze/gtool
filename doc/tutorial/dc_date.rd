=begin JA

= 日時および時刻の操作

# * 森川靖大 (morikawa)
#   * $Id: dc_date.rd,v 1.2 2009-03-01 03:02:30 morikawa Exp $

=end JA

=begin EN

= Date and Time Management

# * 森川靖大 (morikawa)
#   * $Id: dc_date.rd,v 1.2 2009-03-01 03:02:30 morikawa Exp $

=end EN


=begin JA

== 概要

gtool5 では日時や時刻の管理を容易にするためのモジュールとして
((*dc_date*)) を用意しています. 
このモジュールの使用により日時・時刻管理に際しての
年, 月, 日, 時, 分, 秒の変換を容易に行うことが可能となります.

以下では, 時間積分ループに関する時刻管理を題材に, dc_date
の使用方法を紹介します. 

== dc_date を用いたプログラム

例として, 
((<"Fortran 90/95 汎用モジュール: 種別型パラメタの提供"|URL:dc_types.htm>)) 
で用いた ((<サンプルプログラム|URL:dc_types/diffusion_3.f90>)) の
時刻管理に dc_date を用いたプログラムを示します
(拡散係数や時間ステップ等も多少変更しています). 
ソースコードは((<こちら|URL:dc_date/diffusion_date.f90>)). 
赤字(カラーがでない場合はボールド)が dc_date に関係している箇所です. 

=end JA
=begin EN

Under construction. 

=end EN


=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
!= Sample program for gtool_history/gtool5
!
! * 2009/02/28 Y.Morikawa
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
program diffusion_date

  use gtool_history                                 ! Access module (モジュール指定)
  <b><font color="red">use dc_date                                       ! Access module (モジュール指定)</font></b>
  use dc_types                                      ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=1.                   ! Time step [s] (時間間隔 [秒])
  <b><font color="red">type(DC_DIFFTIME)      :: deltime                 ! Time step (時間間隔)</font></b>
  <b><font color="red">type(DC_DIFFTIME)      :: endtime                 ! End time (終了時刻)</font></b>
  <b><font color="red">type(DC_DIFFTIME)      :: dsptime                 ! Output interval (出力間隔)</font></b>
  <b><font color="red">type(DC_DIFFTIME)      :: curtime                 ! Current time (現在時刻)</font></b>
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=0.0001            ! Diffusion coefficient (熱拡散係数)

  <b><font color="red">type(DC_DATETIME):: date                          ! Date (日時)</font></b>
  character(256):: date_str
<b><font color="red">
  call DCDateTimeCreate(date, year=2009, mon=2, day=28, hour=23, min=59)
                                                    ! Set date
                                                    ! (日時設定)

  call DCDiffTimeCreate(curtime, 0., 'sec')         ! Set initial time 
                                                    ! (初期時刻設定)
  call DCDiffTimeCreate(deltime, dt, 'sec')         ! Set time step
                                                    ! (時間間隔設定)
  call DCDiffTimeCreate(endtime, 10., 'min')        ! Set end time 
                                                    ! (終了時刻設定)
  call DCDiffTimeCreate(dsptime, 0.1, 'min')        ! Set output interval
                                                    ! (出力間隔設定)
</font></b>
  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &                             ! Create output file 
    & file='diffusion_date.nc', &                   ! (ヒストリー作成)
    & title='Diffusion equation',                        &
    & source='Sample program of gtool_history/gtool5',   &
    & institution='GFD_Dennou Club davis project',       &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),               &
    & longnames=(/'X-coordinate','time        '/),       &
    & units=(/'m  ','min'/),                             &
    & origin=<b><font color="red">curtime</font></b>, interval=<b><font color="red">dsptime</font></b> )

  call HistoryPut('x',x)                            ! Output 'x' (次元変数出力)

  call HistoryAddVariable( &                        ! Set output variable 
    & varname='temp', dims=(/'x','t'/), &           ! (変数定義) 
    & longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! Output 'temp' (変数出力)

  date_str = toChar(date)
  write(6,*) "Start: ", trim(date_str)              ! Display start date (開始日時出力) 

  do while ( <b><font color="red">curtime <= endtime</font></b> )                   <b><font color="red">! Check termination (終了判定)</font></b>

    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( <b><font color="red">mod( curtime, dsptime ) == 0</font></b> ) then        <b><font color="red">! Check output timing (出力タイミング判定)</font></b>
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif

    <b><font color="red">curtime = curtime + deltime                     ! Progress model time (モデル時刻進行)</font></b>
  enddo

  <b><font color="red">date = date + curtime                             ! Evaluate end date (終了日時算出) </font></b>
  date_str = <b><font color="red">toChar(date)</font></b>                           ! Convert date into characters (日時を文字型変数へ変換)
  write(6,*) "End:   ", trim(date_str)              ! Display end date (終了日時出力) 

  call HistoryClose
  stop
end program diffusion_date
</pre>
=end HTML

=begin JA

== プログラムのコンパイルと実行

((<上記のプログラム|URL:dc_date/diffusion_date.f90>)) をダウンロードし,
以下のようにコンパイルしてください. 

    $ gt5frt diffusion_date.f90

そして作成された a.out を以下のように実行してください. 

    $ ./a.out

実行すると以下のメッセージが表示されるとともに,
diffusion_date.nc ファイルが作成されます.

   　
   *** MESSAGE [HistoryCreate3] ***  "diffusion_date.nc" is created (origin=0. [min])
   Start: 2009-02-28T23:59:00+09:00
   End:   2009-03-01T00:09:01+09:00
   *** MESSAGE [HistoryClose] ***  "diffusion_date.nc" is closed

=end JA
=begin EN

=end EN

=begin JA

== ソースコードの解説

以下では dc_date モジュールの各サブルーチンが行っていることを大まかに説明します.
より詳しい説明は
((<リファレンスマニュアル: dc_date|URL:../code_reference/classes/dc_date.html>))
を参照してください.

: use dc_date

  モジュールの使用を宣言します. Fortran 90/95 メインプログラムの先頭にいれ
  ましょう. 

: type(DC_DIFFTIME)

  構造型 DC_DIFFTIME 変数の宣言文です.
  この型の変数には「日時差」に関する情報が格納されます.
  上記のプログラムではモデル時刻として使用されています. 

: type(DC_DATETIME)

  構造型 DC_DATETIME 変数の宣言文です. 
  この型の変数には「日時」に関する情報が格納されます. 

: DCDiffTimeCreate(diff, value, unit)

  DC_DIFFTIME 変数の設定を行います.

  * ((*diff*)) には DC_DIFFTIME 変数を指定します.

  * ((*value*)) には日時差 (時刻) を表す数値を指定します. 

  * ((*unit*)) には日時差 (時刻) を表す単位を指定します. 

: DCDateTimeCreate(date, [year], [mon], [day], [hour], [min], [sec])

  DC_DATETIME 変数の設定を行います.

  * ((*date*)) には DC_DATETIME 変数を指定します.

  * ((*year*)), ((*mon*)), ((*day*)), ((*hour*)), ((*min*)), ((*sec*)) には,
    それぞれ年, 月, 日, 時, 分, 秒を与えます.
    ((*sec*)) のみ倍精度の実数を与えます.
    その他は整数を与えます. 

: HistoryCreate( ..., origin, interval, ... )

  HistoryCreate の ((*origin*)) (出力開始時間) 
  と ((*interval*)) (出力間隔) には DC_DIFFTIME 
  変数を与えることが可能です. 

: curtime <= endtime

  DC_DIFFTIME 変数同士を比較演算子で大小の比較を行うことが可能です.
  ここでは, 「((*curtime*)) (現在時刻) が ((*endtime*)) (終了時刻)
  よりも過去 (小さい) かもしくは同時 (等しい) 場合に真を返す」ことを行っています.

  利用可能な比較演算子は, ((*==*)) ((*<=*)), ((*<*)), ((*>=*)), ((*>*))
  です. 

: mod( curtime, dsptime )

  DC_DIFFTIME 変数同士の演算も可能です.
  ここでは, ((*curtime*)) を ((*dsptime*)) で除算した際の余りを返しています.
  
  このように利用可能な関数として ((*mod*)), ((*max*)), ((*min*)) を用意しています. 

: curtime = curtime + deltime

  DC_DIFFTIME 変数同士を演算子を用いて演算することも可能です.
  ここでは, ((*curtime*)) に ((*deltime*)) を加えたものを ((*curtime*))
  に代入しています.

  このように利用可能な演算子として, ((*+*)), ((*-*)), ((* * *)), ((* / *))
  を用意しています. 


: date = date + curtime

  DC_DATETIME 変数に対して DC_DIFFTIME 変数を作用させることが可能です.
  ここでは ((*date*)) に ((*deltime*)) を加えたものを ((*date*))
  に代入しています.

  このように利用可能な演算子として, ((*+*)), ((*-*))
  を用意しています.

: toChar(date)

  DC_DATETIME 変数, DC_DIFFTIME 変数のどちらとも, ((*toChar*))
  関数で文字型変数に変換することが可能です.
  上記プログラムのように, 日時を表示する場合などに使用します. 


=end JA
=begin EN

=end EN



=begin HTML
<hr /> <small>
  $Id: dc_date.rd,v 1.2 2009-03-01 03:02:30 morikawa Exp $
</small>
=end HTML
