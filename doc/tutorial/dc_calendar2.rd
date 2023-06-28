=begin JA

= 暦および日時の操作 (上級編)

# * 森川靖大 (morikawa)
#   * $Id: dc_calendar2.rd,v 1.6 2010-08-26 10:50:09 morikawa Exp $

=end JA

=begin EN

= Calendar and Date Management (Advanced)

# * 森川靖大 (morikawa)
#   * $Id: dc_calendar2.rd,v 1.6 2010-08-26 10:50:09 morikawa Exp $

=end EN


=begin JA

== 概要

Gtool5 では暦や日時の管理を容易にするためのモジュールとして
((*dc_calendar*)) を用意しています. 
このモジュールの使用により暦・日時管理に際しての
年, 月, 日, 時, 分, 秒の変換を容易に行うことが可能となります. 

本チュートリアルでは, モデルの開始, 終了日時の管理を題材に, dc_calendar
の使用方法を紹介します. dc_calendar から提供される機能を用いて以下の
操作を行います. なお, 基本的な利用法については
((<暦および日時の操作 (基本編)|URL:dc_calendar1.htm>)) で
紹介していますので, まずはそちらを参照ください. 

* ユーザ定義暦の使用
* 複数の暦の使用
* 通日, 通秒の算出
* 閏年の判定
* gtool_historyauto モジュールを用いての暦や開始日時の指定
# * (終了日時から endtime を算出)

== dc_calendar を用いたプログラム

例として, 
((<"Fortran 90/95 汎用モジュール: 種別型パラメタの提供"|URL:dc_types.htm>)) 
で用いた ((<サンプルプログラム|URL:dc_types/diffusion_3.f90>)) の
時刻管理に dc_calendar を用いたプログラムを示します
(拡散係数や時間ステップ等を多少変更している他, 出力に
gtool_history ではなく gtool_historyauto を用いています.
gtool_historyauto については((<多数のファイル出力を行うモデルでのデータ出力|URL:gtauto_first.htm>))
を参照ください). 
ソースコードは((<こちら|URL:dc_calendar/diffusion_cal2.f90>)). 
赤字(カラーがでない場合はボールド)が dc_calendar に関係している箇所です. 

=end JA
=begin EN

Under construction. 

=end EN


=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
!= Sample program for gtool_historyauto/gtool5
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
program diffusion_cal2

  use gtool_historyauto                             ! Access module (モジュール指定)
  <b><font color="red">use dc_calendar                               ! Access module (モジュール指定)</font></b>
  use dc_types                                      ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number   (グリッド数)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  <b><font color="red">type(DC_CAL)         :: cal_gre             ! Gregorian Calendar (グレゴリオ暦)</font></b>
  <b><font color="red">type(DC_CAL)         :: cal_mars            ! Martian Calendar   (火星っぽい暦)</font></b>
  <b><font color="red">type(DC_CAL_DATE)    :: start_date          ! Start Date (モデル開始日時)</font></b>
  <b><font color="red">type(DC_CAL_DATE)    :: end_date            ! End Date   (モデル終了日時)</font></b>
  <b><font color="red">real(DP)             :: dt                  ! Time step    [sec] (時間間隔 [秒])</font></b>
  <b><font color="red">real(DP)             :: curtime             ! Current time [sec] (経過時刻 [秒])</font></b>
  <b><font color="red">real(DP)             :: endtime             ! End time     [sec] (終了時刻 [秒])</font></b>
  real(DP)             :: dsptime             ! Output interval [day] (出力間隔 [日])

  real(DP)             :: sec_of_year         ! Second of year (年始めからの通秒)
  real(DP)             :: day_of_year         ! Day of year    (年始めからの通日)
  real(DP)             :: sec_of_day          ! Second of day  (日始めからの通秒)
  logical              :: flag_leapyear       ! Leap year      (閏年)

  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0e-9            ! Diffusion coefficient (熱拡散係数)

  character(256):: date_str1, date_str2

  character(32):: cal_type
  integer:: month_in_year, day_in_month(1:12), hour_in_day, min_in_hour 
  integer, pointer:: day_in_month_ptr(:) =>null()
  real(DP):: sec_in_min

<b><font color="red">
  call DCCalCreate( cal_type = 'Gregorian', cal = cal_gre )
  call DCCalCreate( month_in_year = 1 , &
    &               day_in_month  = (/669/), &
    &               hour_in_day   = 24, &
    &               min_in_hour   = 1 , &
    &               sec_in_min    = 3694.0_DP, &
    &               cal           = cal_mars )
                                                    ! Set calendar
                                                    ! (暦設定)

  call DCCalDateCreate( year = 2000, month =  1, day = 1, &
    &                   hour =    0, min   =  0, sec = 0.0_DP, &
    &                   date = start_date )
  call DCCalDateCreate( year = 2001, month =  1, day = 1, &
    &                   hour =    0, min   =  0, sec = 0.0_DP, &
    &                   date = end_date )
                                                    ! Set date
                                                    ! (日時設定)

  curtime = DCCalConvertByUnit(  0.0_DP, 'sec', 'sec', cal = cal_mars )
                                                    ! Set initial time 
                                                    ! (初期時刻設定)
  endtime = DCCalDateDifference( start_date, end_date, cal = cal_mars )
                                                    ! Set end time 
                                                    ! (終了時刻設定)
  dt      = DCCalConvertByUnit(  1.0_DP, 'day', 'sec', cal = cal_mars )
                                                    ! Set time step
                                                    ! (時間ステップ設定)</font></b>
  dsptime = 10
                                                    ! Set output interval
                                                    ! (出力間隔設定)

  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryAutoCreate( &                         ! Create output file 
    & title='Diffusion equation',                &  ! (ヒストリー作成)
    & source='Sample program of gtool_historyauto/gtool5',   &
    & institution='GFD_Dennou Club davis project',           &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),                   &
    & longnames=(/'X-coordinate','time        '/),           &
    & units=(/'m  ','day'/),                                 &
    & origin=curtime, terminus=endtime, interval=dsptime,    &
    & <b><font color="red">cal=cal_mars, start_date=start_date </font></b>)

  call HistoryAutoPutAxis('x',x)                    ! Output 'x' (次元変数出力)

  call HistoryAutoAddVariable( &                    ! Set output variable 
    & varname='temp', dims=(/'x','t'/), &           ! (変数定義) 
    & longname='temperature', units='K', xtype='double', &
    & file='diffusion_cal2.nc' )

  call HistoryAutoPut(curtime,'temp',temp)          ! Output 'temp' (変数出力)
  
  write(6,*) "Calendars: "
<b><font color="red">
  call DCCalInquire( cal_type, &
    &                month_in_year    = month_in_year, &
    &                day_in_month_ptr = day_in_month_ptr, &
    &                hour_in_day      = hour_in_day, &
    &                min_in_hour      = min_in_hour, &
    &                sec_in_min       = sec_in_min, &
    &                cal              = cal_mars )
                                        ! Inquire calendar
                                        ! (暦に関して問い合わせ)
</font></b>
  write(6,*) "       Martian Cal:   month in year:  ", month_in_year
  write(6,*) "                      days in months: ", day_in_month_ptr
  write(6,*) "                      hour in day:    ", hour_in_day
  write(6,*) "                      min in hour:    ", min_in_hour
  write(6,*) "                      sec in min:     ", sec_in_min
  deallocate(day_in_month_ptr)
<b><font color="red">
  call DCCalInquire( cal_type, &
    &                month_in_year    = month_in_year, &
    &                day_in_month     = day_in_month, &
    &                hour_in_day      = hour_in_day, &
    &                min_in_hour      = min_in_hour, &
    &                sec_in_min       = sec_in_min, &
    &                cal              = cal_gre )
                                        ! Inquire calendar
                                        ! (暦に関して問い合わせ)
</font></b>
  write(6,*) "       Gregorian Cal: month in year:  ", month_in_year
  write(6,*) "                      days in months: ", day_in_month
  write(6,*) "                      hour in day:    ", hour_in_day
  write(6,*) "                      min in hour:    ", min_in_hour
  write(6,*) "                      sec in min:     ", sec_in_min

  write(6,*) ""
<b><font color="red">
  call DCCalDateInquire( date_str1, date = start_date )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)
</font></b>
  write(6,*) "Start: ", trim(date_str1)
                                                    ! Display start date
                                                    ! (開始日時の表示) 

  sec_of_year   = <b><font color="red">DCCalDateEvalSecOfYear( curtime, start_date, cal_mars )</font></b>
                                        ! Second of year (年始めからの通秒)
  day_of_year   = <b><font color="red">DCCalDateEvalDayOfYear( curtime, start_date, cal_mars )</font></b>
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = <b><font color="red">DCCalDateEvalSecOfDay( curtime, start_date, cal_mars )</font></b>
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = <b><font color="red">DCCalDateChkLeapYear( curtime, start_date, cal_mars )</font></b>
                                        ! Leap year      (閏年)

  write(6,*) "       Martian Cal:   second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear

  sec_of_year   = <b><font color="red">DCCalDateEvalSecOfYear( curtime, start_date, cal_gre )</font></b>
                                        ! Second of year (年始めからの通秒)
  day_of_year   = <b><font color="red">DCCalDateEvalDayOfYear( curtime, start_date, cal_gre )</font></b>
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = <b><font color="red">DCCalDateEvalSecOfDay( curtime, start_date, cal_gre )</font></b>
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = <b><font color="red">DCCalDateChkLeapYear( curtime, start_date, cal_gre )</font></b>
                                        ! Leap year      (閏年)

  write(6,*) "       Gregorian Cal: second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear

  do while ( curtime < endtime )                    ! Check termination (終了判定)

    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    call HistoryAutoPut(curtime,'temp',temp)        ! Output 'temp' (変数出力)

    curtime = curtime + dt                          ! Progress model time (モデル時刻進行)
  enddo

<b><font color="red">
  call DCCalDateInquire( date_str1, curtime, start_date, cal = cal_mars )
  call DCCalDateInquire( date_str2, curtime, start_date, cal = cal_gre )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)
</font></b>
  write(6,*) "End:   ", trim(date_str1), " (Mars), ", trim(date_str2), " (Gregorian)"
                                                    ! Display end date (終了日時出力) 

  sec_of_year   = <b><font color="red">DCCalDateEvalSecOfYear( curtime, start_date, cal_mars )</font></b>
                                        ! Second of year (年始めからの通秒)
  day_of_year   = <b><font color="red">DCCalDateEvalDayOfYear( curtime, start_date, cal_mars )</font></b>
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = <b><font color="red">DCCalDateEvalSecOfDay( curtime, start_date, cal_mars )</font></b>
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = <b><font color="red">DCCalDateChkLeapYear( curtime, start_date, cal_mars )</font></b>
                                        ! Leap year      (閏年)

  write(6,*) "       Martian Cal:   second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear

  sec_of_year   = <b><font color="red">DCCalDateEvalSecOfYear( curtime, start_date, cal_gre )</font></b>
                                        ! Second of year (年始めからの通秒)
  day_of_year   = <b><font color="red">DCCalDateEvalDayOfYear( curtime, start_date, cal_gre )</font></b>
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = <b><font color="red">DCCalDateEvalSecOfDay( curtime, start_date, cal_gre )</font></b>
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = <b><font color="red">DCCalDateChkLeapYear( curtime, start_date, cal_gre )</font></b>
                                        ! Leap year      (閏年)

  write(6,*) "       Gregorian Cal: second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear

  call HistoryAutoClose
  stop
end program diffusion_cal2
</pre>
=end HTML

=begin JA

== プログラムのコンパイルと実行

((<上記のプログラム|URL:dc_date/diffusion_cal2.f90>)) をダウンロードし,
以下のようにコンパイルしてください. 

    $ gt5frt diffusion_cal2.f90

そして作成された a.out を以下のように実行してください. 

    $ ./a.out

実行すると以下のメッセージが表示されるとともに,
diffusion_cal2.nc ファイルが作成されます.

   　
   *** MESSAGE [HistAuto] ***  ----- "gtool_historyauto_nml" is not loaded" -----
   *** MESSAGE [HistAuto] ***  Global Settings:
   *** MESSAGE [HistAuto] ***    AllOutput       = T
   *** MESSAGE [HistAuto] ***    FilePrefix      =
   *** MESSAGE [HistAuto] ***    Interval        = 10. [day]
   *** MESSAGE [HistAuto] ***    Precision       = float
   *** MESSAGE [HistAuto] ***    TimeAverage     = F
   *** MESSAGE [HistAuto] ***    Origin          = 0. [sec]
   *** MESSAGE [HistAuto] ***    Terminus        = 59310864. [day]
   *** MESSAGE [HistAuto] ***    SliceStart      = (/ 1 /)
   *** MESSAGE [HistAuto] ***    SliceEnd        = (/ -1 /)
   *** MESSAGE [HistAuto] ***    SliceStride     = (/ 1 /)
   *** MESSAGE [HistAuto] ***    SpaceAverage    = (/ F /)
   *** MESSAGE [HistAuto] ***    NewFileInterval = -1 [day]
   *** MESSAGE [HistAuto] ***
   *** MESSAGE [HistoryCreate1] ***  "diffusion_cal2.nc" is created (origin=0. [day])
   Calendars: 
          Martian Cal:   month in year:   1
                         days in months:  669
                         hour in day:     24
                         min in hour:     1
                         sec in min:      3694.000000000000
          Gregorian Cal: month in year:   12
                         days in months:  31 28 31 30 31 30 31 31 30 31 30 31
                         hour in day:     24
                         min in hour:     60
                         sec in min:      60.00000000000000
   
   Start: 2000-01-01T00:00:00
          Martian Cal:   second of year:  0.000000000000000E+00
                         day of year:     1.000000000000000
                         second of day:   0.000000000000000E+00
                         leap year:       F
          Gregorian Cal: second of year:  0.000000000000000E+00
                         day of year:     1.000000000000000
                         second of day:   0.000000000000000E+00
                         leap year:       T
   End:   2001-01-01T00:00:00 (Mars), 2001-11-17T11:14:24 (Gregorian)
          Martian Cal:   second of year:  0.000000000000000E+00
                         day of year:     1.000000000000000
                         second of day:   0.000000000000000E+00
                         leap year:       F
          Gregorian Cal: second of year:  27688464.00000000
                         day of year:     321.0000000000000
                         second of day:   40464.00000000000
                         leap year:       F
   *** MESSAGE [HistoryClose] ***  "diffusion_cal2.nc" is closed

=end JA
=begin EN

=end EN

=begin JA

== 2 種類の暦について

上記プログラムでは, 複数の暦を同一プログラムで使用する例を挙げるため, 
以下の 2 種類の暦を使用しています.

* グレゴリオ暦
* 火星を想定した暦 (以下, 火星の暦と呼称) 

仮に両暦の日時が 2000 年 1 月 1 日 0 時 0 分 0 秒で
一致するものとし, 火星の暦で 1 年間計算を行うものとしています. 
時間積分も基本的には火星の暦で行います. 
計算終了時に火星の暦のちょうど一年後がグレゴリオ暦でのどの日時に
あたるのかを算出・表示しています. 

== ソースコードの解説

以下では dc_calendar モジュールの各サブルーチンが行っていることを大まかに説明します.
より詳しい説明は
((<リファレンスマニュアル: dc_calendar|URL:../code_reference/classes/dc_calendar.html>))
を参照してください.

: use dc_calendar

  モジュールの使用を宣言します. Fortran 90/95 メインプログラムの先頭にいれ
  ましょう. 

: type(DC_CAL) :: cal_gre, cal_mars

  構造型 DC_CAL 変数の宣言文です.
  この型の変数には「暦」に関する情報が格納されます. 
  上記のプログラムではグレゴリオ暦とユーザ定義暦 (火星を想定した暦)
  として使用されています. 

: type(DC_CAL_DATE) :: start_date, end_date

  構造型 DC_CAL_DATE 変数の宣言文です.
  この型の変数には「日時」に関する情報が格納されます.
  上記のプログラムではモデルの開始日時と終了日時として使用されています. 

: real(DP) :: curtime, endtime

  モデルの現在時刻や終了の時刻は上記の DC_CAL_DATE 型の変数に格納された
  日時からの経過時間 (単位: 秒) として表現します. 

: DCCalCreate( cal_type, [cal], [err] )

  暦の設定を行います. ((*cal_type*)) に文字列を与えて暦の設定を
  行う方法については, ((<暦および日時の操作 (基本編)|URL:dc_calendar1.htm>))
  を参照ください. 

: DCCalCreate( month_in_year, day_in_month, hour_in_day, min_in_hour, min_in_hour, sec_in_min, [cal], [err] )

  暦の設定を行います. 引数にはそれぞれ以下の情報を与えます. 

  : month_in_year [整数型]
    1 年の月数.
    
  : day_in_month [整数型配列]
    各月の日数.
   
  : hour_in_day [整数型] 
    1 日の時間数.
    
  : min_in_hour [整数型] 
    1 日の時間数. 
    
  : min_in_hour  [整数型] 
    1 時間の分数.

  : sec_in_min [倍精度実数型]
    1 分の秒数.

: DCCalDateCreate(year, month, day, hour, min, sec, [date], [zone], [err])

  日時を変数 ((*date*)) に設定します. 

: DCCalConvertByUnit( in_time, in_unit, out_unit, [cal], [err] )

  日時の単位変換を行います. 

  ((*in_time*)), ((*in_unit*)) に変換前の数値と単位を,
  ((*out_unit*)) には変換後の単位を与えることで, 
  変換後の数値が返ります. 

: DCCalDateDifference( start_date, end_date, [cal] )

  ((*start_date*)) と ((*end_date*)) の差を秒数として取得します.
  ((*cal*)) に DC_CAL 型の変数を与えることで, 暦に応じた値を
  取得できます.

: HistoryAutoCreate( .., [cal], [start_date] )

  gtool_historyauto モジュールの初期設定サブルーチンである
  HistoryAutoCreate には暦と開始日時を設定可能です.
  これらの設定は, 出力ファイルの時刻変数の属性として出力されます.
  以下に, diffusion_cal2.nc 内の関連部分をテキスト形式で出力したものを
  引用します.

    variables:
            float t(t) ;
                    t:long_name = "time" ;
                    t:units = "day  since 2000-01-01T00:00:00" ;
                               //   ^^^^^^^^^^^^^^^^^^^^^^^^^
                    t:calendar = "user_defined" ; // 暦の種別
                    t:month_in_year = 1 ;
                    t:day_in_month = 669 ;
                    t:hour_in_day = 24 ;
                    t:min_in_hour = 1 ;
                    t:sec_in_min = 3694. ;

: DCCalInquire( [cal_type], [month_in_year], [day_in_month], [day_in_month_ptr], [hour_in_day], [min_in_hour], [sec_in_min], [cal] )

  ((*cal*)) (DC_CAL 型) に与えられた情報を以下の引数に返します.

  : cal_type
    暦の種類を示す文字列. 
  
  : month_in_year
    1 年の月数.
  
  : day_in_month(:)
    1 ヶ月の日数. ただし, 配列の大きさは 1 年の月数にしておく必要があります. 
    グレゴリオ暦の場合, 配列の 2 番目の要素 (2月) には必ず 28 が返ります. 
  
  : day_in_month_ptr(:)
    1 ヶ月の日数. ポインタであるため, month_in_year に応じて, 
    サブルーチン内で割り付けが行われます. day_in_month と同様に, 
    グレゴリオ暦の場合, 配列の 2 番目の要素 (2月) には必ず 28 が返ります. 
  
  : hour_in_day
    1 日の時間数.

  : min_in_hour
    1 時間の分数.
  
  : sec_in_min
    1 分の秒数.

: DCCalDateInquire( date_str, [elapse_sec], [date], [cal] )

  ((*date*)) に与えられた情報を YYYY-MM-DDThh:mm:ss.sTZD
  (YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒, TZD はタイムゾーン)
  の形式の文字列として ((*date_str*)) に受け取ります.

  ((*elapse_sec*)) が指定される場合には ((*date*)) から ((*elapse_sec*)) の
  秒数経過した日時が返ります. 

: DCCalDateEvalSecOfYear( elapse_sec, [date], [cal] )

  ((*date*)) を起点として経過時間 [秒] ((*elapse_sec*)) 後の
  年始めからの通秒を返します. 

: DCCalDateEvalDayOfYear( elapse_sec, [date], [cal] )

  ((*date*)) を起点として経過時間 [秒] ((*elapse_sec*)) 後の
  年始めからの通日を返します. 

: DCCalDateEvalSecOfDay( elapse_sec, [date], [cal] )

  ((*date*)) を起点として経過時間 [秒] ((*elapse_sec*)) 後の
  日始めからの通秒を返します. 

: DCCalDateChkLeapYear( elapse_sec, [date], [cal] )

  ((*date*)) を起点として経過時間 [秒] ((*elapse_sec*)) 後の
  年が閏年かどうかを返します.
  グレゴリオ暦かユリウス暦以外では全て偽が返ります. 

=end JA
=begin EN

=end EN



=begin HTML
<hr /> <small>
  $Id: dc_calendar2.rd,v 1.6 2010-08-26 10:50:09 morikawa Exp $
</small>
=end HTML
