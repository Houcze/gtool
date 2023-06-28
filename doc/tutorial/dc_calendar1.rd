=begin JA

= 暦および日時の操作 (基本編)

# * 森川靖大 (morikawa)
#   * $Id: dc_calendar1.rd,v 1.2 2009-12-29 16:10:05 morikawa Exp $

=end JA

=begin EN

= Calendar and Date Management (Basic)

# * 森川靖大 (morikawa)
#   * $Id: dc_calendar1.rd,v 1.2 2009-12-29 16:10:05 morikawa Exp $

=end EN


=begin JA

== 概要

Gtool5 では暦や日時の管理を容易にするためのモジュールとして
((*dc_calendar*)) を用意しています. 
このモジュールの使用により暦・日時管理に際しての
年, 月, 日, 時, 分, 秒の変換を容易に行うことが可能となります. 

以下では, モデルの開始, 終了日時の管理を題材に, dc_calendar
の使用方法を紹介します. 

dc_calendar モジュールでは 1 ヶ月の日数, 1 日の秒数などを任意に指定した
暦を用いることも可能ですが, 以下ではグレゴリオ暦を用いて解説を行います. 
ユーザ定義の暦の使用法などについては
((<暦および日時の操作 (上級編)|URL:dc_calendar2.htm>))
を参照ください. 

== dc_calendar を用いたプログラム

例として, 
((<"Fortran 90/95 汎用モジュール: 種別型パラメタの提供"|URL:dc_types.htm>)) 
で用いた ((<サンプルプログラム|URL:dc_types/diffusion_3.f90>)) の
時刻管理に dc_calendar を用いたプログラムを示します
(拡散係数や時間ステップ等も多少変更しています). 
ソースコードは((<こちら|URL:dc_calendar/diffusion_cal1.f90>)). 
赤字(カラーがでない場合はボールド)が dc_calendar に関係している箇所です. 

=end JA
=begin EN

Under construction. 

=end EN


=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
!= Sample program for gtool_history/gtool5
!
! * 2009/10/18 Y.Morikawa
! * 2003/08/21 M.Odaka
! * 2001/02/27 S.Takehiro
!
! Solving diffusion equation
! \[
!     du/dt = \kappa d^2 u/dx^2
! \]
! for giving values of $u$ at $x=[0,1]$.
!
program diffusion_cal1

  use gtool_history                                 ! Access module (モジュール指定)
  <b><font color="red">use dc_calendar                               ! Access module (モジュール指定)</font></b>
  use dc_types                                      ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=1.                   ! Time step [s] (時間間隔 [秒])
  <b><font color="red">type(DC_CAL_DATE)    :: start_date          ! Start Date (モデル開始日時)</font></b>
  <b><font color="red">type(DC_CAL_DATE)    :: end_date            ! End Date (モデル終了日時)</font></b>
  <b><font color="red">real(DP)             :: curtime             ! Current time [sec] (経過時刻 [秒])</font></b>
  <b><font color="red">real(DP)             :: endtime             ! End time [sec] (終了時刻 [秒])</font></b>
  real(DP)             :: dsptime             ! Output interval [min] (出力間隔 [分])
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=0.0001            ! Diffusion coefficient (熱拡散係数)

  character(256):: date_str
<b><font color="red">
  call DCCalCreate( cal_type = 'Gregorian' )        ! Set calendar
                                                    ! (暦設定)
  call DCCalDateCreate( year = 2009, month =  2, day = 28, &
    &                   hour =   23, min   = 59, sec = 0.0_DP, &
    &                   date = start_date )
                                                    ! Set date
                                                    ! (日時設定)
  curtime = DCCalConvertToSec(  0.0_DP, 'sec' )
                                                    ! Set initial time 
                                                    ! (初期時刻設定)
  endtime = DCCalConvertToSec( 10.0_DP, 'min' )
                                                    ! Set end time 
                                                    ! (終了時刻設定)
</font></b>
  dsptime = 1.0_DP
                                                    ! Set output interval
                                                    ! (出力間隔設定)

  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &                             ! Create output file 
    & file='diffusion_cal1.nc', &                   ! (ヒストリー作成)
    & title='Diffusion equation',                        &
    & source='Sample program of gtool_history/gtool5',   &
    & institution='GFD_Dennou Club davis project',       &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),               &
    & longnames=(/'X-coordinate','time        '/),       &
    & units=(/'m  ','min'/),                             &
    & origind=curtime, intervald=dsptime )

  call HistoryPut('x',x)                            ! Output 'x' (次元変数出力)

  call HistoryAddVariable( &                        ! Set output variable 
    & varname='temp', dims=(/'x','t'/), &           ! (変数定義) 
    & longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! Output 'temp' (変数出力)
<b><font color="red">
  call DCCalDateInquire( date_str, date = start_date )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)
</font></b>
  call HistoryAddAttr( 't', 'units', 'min since ' // trim(date_str) )
                                                    ! Output start date to a file
                                                    ! (開始日時のファイルへの出力) 

  write(6,*) "Start: ", trim(date_str)              ! Display start date
                                                    ! (開始日時の表示) 

  do while ( curtime < endtime )                    ! Check termination (終了判定)

    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod( curtime, dsptime ) == 0.0_DP ) then   ! Check output timing (出力タイミング判定)
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif

    curtime = curtime + dt                          ! Progress model time (モデル時刻進行)
  enddo

<b><font color="red">
  call DCCalDateInquire( date_str, curtime, start_date )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)
</font></b>
  write(6,*) "End:   ", trim(date_str)              ! Display end date (終了日時出力) 

  call HistoryClose
  stop
end program diffusion_cal1
</pre>
=end HTML

=begin JA

== プログラムのコンパイルと実行

((<上記のプログラム|URL:dc_date/diffusion_cal1.f90>)) をダウンロードし,
以下のようにコンパイルしてください. 

    $ gt5frt diffusion_cal1.f90

そして作成された a.out を以下のように実行してください. 

    $ ./a.out

実行すると以下のメッセージが表示されるとともに,
diffusion_cal1.nc ファイルが作成されます.

   　
   *** MESSAGE [HistoryCreate1] ***  "diffusion_cal1.nc" is created (origin=0. [min])
   Start: 2009-02-28T23:59:00
   End:   2009-03-01T00:09:00
   *** MESSAGE [HistoryClose] ***  "diffusion_cal1.nc" is closed

=end JA
=begin EN

=end EN

=begin JA

== ソースコードの解説

以下では dc_calendar モジュールの各サブルーチンが行っていることを大まかに説明します.
より詳しい説明は
((<リファレンスマニュアル: dc_calendar|URL:../code_reference/classes/dc_calendar.html>))
を参照してください.

: use dc_calendar

  モジュールの使用を宣言します. Fortran 90/95 メインプログラムの先頭にいれ
  ましょう. 

: type(DC_CAL_DATE) :: start_date, end_date

  構造型 DC_CAL_DATE 変数の宣言文です.
  この型の変数には「日時」に関する情報が格納されます.
  上記のプログラムではモデルの開始日時と終了日時として使用されています. 

: real(DP) :: curtime, endtime 他

  モデルの現在時刻や終了の時刻は上記の DC_CAL_DATE 型の変数に格納された
  日時からの経過時間 (単位: 秒) として表現します. 

: DCCalCreate( cal_type, [cal], [err] )

  暦の設定を行います. cal_type に以下の文字列を与えることで暦の設定を
  行います. 上記の例のように省略可能引数 ((*cal*)) を与えない場合には
  デフォルトの暦として設定が行われます. 省略可能引数 ((*cal*)) は
  一度のプログラム実行に複数の暦を利用したい場合に使用します.

  : gregorian
    グレゴリオ暦
    
  : julian
    ユリウス暦
    
  : noleap
    閏年無しの暦
    
  : 360day
    1ヶ月が 30 日の暦
    
  : cyclic
    ある月の日数を 「30.6 × 月数 − 前月までの総日数」の小数点以下切捨とする暦 

  1 ヶ月の日数, 1 日の秒数などを任意に指定したい場合には
  ((<暦および日時の操作 (上級編)|URL:dc_calendar2.htm>))
  を参照ください. 

: DCCalDateCreate(year, month, day, hour, min, sec, [date], [zone], [err])

  日時を変数 ((*date*)) に設定します. 

: DCCalConvertToSec( in_time, in_unit, [cal], [err] )

  日時の単位変換を行います. 

  ((*in_time*)), ((*in_unit*)) に変換前の数値と単位を与えます. 
  DCCalConvertToSec はこの数値を((*秒*))に変換して返します.

  この他に, ((*DCCalConvertToMin*)) (分へ変換), 
  ((*DCCalConvertToHour*)) (時へ変換), ((*DCCalConvertToDay*)) (日へ変換)
  を利用可能です. 

: DCCalDateInquire( date_str, [elapse_sec], [date], [cal], [err] )

  ((*date*)) に与えられた情報を YYYY-MM-DDThh:mm:ss.sTZD
  (YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒, TZD はタイムゾーン)
  の形式の文字列として ((*date_str*)) に受け取ります.

  ((*elapse_sec*)) が指定される場合には ((*date*)) から ((*elapse_sec*)) の
  秒数経過した日時が返ります. 

=end JA
=begin EN

=end EN



=begin HTML
<hr /> <small>
  $Id: dc_calendar1.rd,v 1.2 2009-12-29 16:10:05 morikawa Exp $
</small>
=end HTML
