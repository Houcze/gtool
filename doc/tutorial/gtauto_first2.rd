=begin JA

= 多数のファイル出力を行うモデルでのデータ出力

# * 森川 靖大 (morikawa)
#   * $Id: gtauto_first2.rd,v 1.1 2009-10-19 11:56:10 morikawa Exp $

=end JA

=begin JA

== 概要

出力ファイルの数が多いモデルでは, 
以下で紹介する ((*gtool_historyauto*)) モジュールを利用することで, 
複数ファイルの出力やその出力の設定変更を簡単に行うことが可能です. 

((*gtool_historyauto*))は
((<データ出力のための最低限の設定|URL:gthist_first.htm>))
で紹介した gtool_history モジュールの拡張版で, 
以下のような特徴を持ちます. 

* 複数のファイルへの出力を行う場合, gtool_history モジュールでは
  ((<ファイルごとに HistoryCreate を呼び出す|URL:gthist_multi.htm>))
  必要がありましたが, 
  gtool_historyauto では HistoryAutoCreate をモデル内で一度呼び出す
  だけで済みます. 
* 個別の変数について, 出力ファイル名や出力間隔を
  NAMELIST で手軽に変更可能です. 

以下では gtool_historyauto モジュールの使い方を
説明します. 

=== dc_date モジュールを使用する場合

バージョン 20090809 以前の gtool_historyauto では, 
dc_date モジュールを日付や時刻の扱いに使用しています. 
こちらの使い方に関しては,
((<多数のファイル出力を行うモデルでのデータ出力 (dc_date 版)|URL:gtauto_first.htm>))
を参照ください. 


== プログラム

プログラムとして, ((<階層的地球流体スペクトルモデル集 SPMODEL|URL:http://www.gfd-dennou.org/library/spmodel/>))
一つである
((<球面上の流れ|URL:http://www.gfd-dennou.org/library/spmodel/gallery/shallow-topo/>))
を計算するものを用います. 

gtool_history を用いたものが
((<sp_topo_gthist.f90|URL:gtauto_first/sp_topo_gthist.f90>))
になります (上記ページで公開されるオリジナルから少し改変されています). 
このプログラムをコンパイル・実行すると,
4 つの従属変数 (h, u, v, zeta) が単一のファイル sp_topo_gthist.nc に出力されます. 
gtool_history でこれを別々のファイルに出力するには, 
((<複数のファイルに出力|URL:gthist_multi.htm>)) に示すように, 
ファイル毎に初期化サブルーチン HistoryCreate を指定せねばなりません. 

gtool_historyauto を用いることで, 初期化サブルーチンの呼び出しが
一つで済むとともに, 変数ごとの出力設定の変更を容易に行うことができます. 


== gtool_historyauto を用いたサンプルプログラム

上記プログラムを gtool_historyauto を用いて書きなおしたものが
以下に示す ((<sp_topo_gtauto_v2.f90|URL:gtauto_first/sp_topo_gtauto_v2.f90>))
となります. 

赤字(カラーがでない場合はボールド)が gtool_historyauto に
関係している箇所です. (行数が多いため, 一部を抜粋しています).
左の緑字は行数を表します. 

このページでは各サブルーチンの詳細については述べません. 
以下でサンプルプログラムの実行や NAMELIST
による出力設定の変更を一通り試した後, 
((<使われているサブルーチンの説明|URL:gtauto_desc2.htm>))
を参照してください. 


=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
<font color="green">  1</font>     !----------------------------------------------------------------------
<font color="green">  2</font>     !  Copyright (C) 2001--2008 SPMODEL Development Group. All rights reserved.
<font color="green">  3</font>     !----------------------------------------------------------------------
<font color="green">  4</font>     ! Sample program for gtool_history/gtool5 and ISPACK   2002/08/21 S.Takehiro
<font color="green">  5</font>     !                                                      2004/01/26 M.Odaka
<font color="green">  6</font>     !                                                      2009/02/27 Y.Morikawa
<font color="green">  7</font>     !
<font color="green">  8</font>     ! Solving a linear 2-D shallow water system on a sphere 
<font color="green">  9</font>     !     with an isolated mountain
<font color="green"> 10</font>     !     du/dt + u0/a\cos\phi du/d\lambda + v/a du0/d\phi - u0v\tan\phi/a 
<font color="green"> 11</font>     !           - 2\Omega\sin\phi v = -g/a\cos\phi dh/d\lambda - \nu\lapla^4 u,
<font color="green"> 12</font>     !     dv/dt + u0/a\cos\phi dv/d\lambda + 2 u0 u \tan\phi/a 
<font color="green"> 13</font>     !           + 2\Omega\sin\phi u  =  -g/a  dh/d\phi - \nu\lapla^4 v,
<font color="green"> 14</font>     !     dh/dt + ( 1/\cos\phi d( (H+h0)u+(h-ht)u0 )/d\lambda 
<font color="green"> 15</font>     !               + 1/\cos\phi d( (H+h0)v\cos\phi)/d\phi ) = - \nu\lapla^4 h.
<font color="green"> 16</font>     !
<font color="green"> 17</font>     ! A setup is similar to the experiment of Grose and Hoskins (1979) 
<font color="green"> 18</font>     ! with a superrotating rigid-rotation zonal wind profile. 
<font color="green"> 19</font>     !
<font color="green"> 20</font>       program sp_topo_gtauto_v2
<font color="green"> 21</font>     
<font color="green"> 22</font>         use w_module
<font color="green"> 23</font>         use gtool5
<font color="green"> 24</font>         <b><font color="red">use gtool_historyauto                    ! モジュール指定</font></b>
<font color="green"> 25</font>         implicit none
<font color="green"> 26</font>     
<font color="green"> 27</font>       !---- 空間解像度設定 ----
<font color="green"> 28</font>         integer, parameter :: im=64, jm=32             ! 格子点の設定(X,Y)
<font color="green"> 29</font>         integer, parameter :: nm=21
<font color="green"> 30</font>  
<font color="green"> 31</font>       !---- 変数 ----
<font color="green"> 32</font>         real(8)            :: xy_U(0:im-1,jm)          ! 格子点データ(速度経度成分)
<font color="green"> 33</font>         real(8)            :: xy_V(0:im-1,jm)          ! 格子点データ(速度緯度成分)
<font color="green"> 34</font>         real(8)            :: xy_H(0:im-1,jm)          ! 格子点データ(変位)
                         :
                         : 
<font color="green"> 44</font>         real(8)            :: xy_Zeta(0:im-1,jm)       ! 格子点データ(渦度)
<font color="green"> 45</font>         real(8)            :: xy_Htopo(0:im-1,jm)      ! 格子点データ(地形)
<font color="green"> 46</font>  
<font color="green"> 47</font>       !---- 時間積分パラメター ----
<font color="green"> 48</font>         real(8):: dt                                   ! 時間間隔 [s]
<font color="green"> 49</font>         real(8):: dispint                              ! 出力時間間隔 [s]
<font color="green"> 50</font>         real(8):: endtime                              ! 計算終了時間 [s]
<font color="green"> 51</font>         real(8):: ct                                   ! 現在時刻 [s]
                         :
                         : 
<font color="green"> 68</font>       !------ NAMELIST ファイル名 ------
<font color="green"> 69</font>         character(*), parameter:: nmlfile = 'sp_topo_gtauto.nml'
                         :
                         :
<font color="green"> 75</font>       !---- 時間積分パラメターの設定 ----
<font color="green"> 76</font>         dt = 100.                                      ! 時間間隔 [s]
<font color="green"> 77</font>         dispint = dt * 500                             ! 出力時間間隔
<font color="green"> 78</font>         endtime = dt * 10000                           ! 計算終了時間
<font color="green"> 79</font>         ct = 0.                                        ! 現在時刻
<font color="green"> 80</font>
<font color="green"> 81</font>
<font color="green"> 82</font>       !---------------- 座標値の設定 ---------------------
<font color="green"> 83</font>         call w_Initial(nm,im,jm)                ! ISPACK初期化
<font color="green"> 84</font>     
<font color="green"> 85</font>       !------------------- 初期値設定 ----------------------
<font color="green"> 86</font>         xy_U0  = U0*cos(xy_Lat)
<font color="green"> 87</font>         xy_H0  = ( Omega*R0*U0/(2*Grav) + U0**2/(4*Grav) )*cos(2*xy_Lat)
<font color="green"> 88</font>     
<font color="green"> 89</font>         xy_U  = 0 ; xy_V  = 0 ; xy_H  = 0
<font color="green"> 90</font>     
<font color="green"> 91</font>         w_U0 = w_xy(xy_U0) !; w_H0 = w_xy(xy_H0)
<font color="green"> 92</font>         w_U = w_xy(xy_U) ; w_V = w_xy(xy_V) ; w_H = w_xy(xy_H)
                         :
                         : 
<font color="green">107</font>       !------------------- ヒストリー初期設定 ----------------------
<font color="green">108</font>         call output_gtool5_init                        ! ヒストリー初期化
<font color="green">109</font>         call output_gtool5                             ! 初期値出力
<font color="green">110</font>     
<font color="green">111</font>       !------------------- 時間積分 ----------------------
<font color="green">112</font>         do while ( ct <= endtime )
<font color="green">113</font>            if ( mod(ct, dispint) == 0.0d0 ) then
<font color="green">114</font>              write(6,*) 'it = ', int( ct / dt )
<font color="green">115</font>            end if
<font color="green">116</font>     
<font color="green">117</font>            ct = ct + dt  ! 時刻の進行
<font color="green">118</font>     
<font color="green">119</font>            w_U = ( w_U &amp;
<font color="green">120</font>                    + dt * w_xy( - xy_U0 * xy_GradLon_w(w_U) / R0   &amp;
<font color="green">121</font>                                 - xy_V  * xy_GradLat_w(w_U0) / R0  &amp;
<font color="green">122</font>                                 + xy_U0 * xy_V * tan(xy_Lat) / R0  &amp;
<font color="green">123</font>                                 + 2 * Omega * sin(xy_Lat) * xy_V   &amp;
<font color="green">124</font>                                - Grav * xy_GradLon_w(w_H)/ R0   ) &amp;
<font color="green">125</font>                  )/(1+Nu*(-rn(:,1)/R0**2)**(ndiff/2)*dt)
                         :
                         : 
<font color="green">144</font>            xy_H = xy_w(w_H)
<font color="green">145</font>     
<font color="green">146</font>            call output_gtool5  ! 出力
<font color="green">147</font>         enddo
<font color="green">148</font>     
<font color="green">149</font>         call output_gtool5_close  ! ファイルのクローズ 
<font color="green">150</font>         stop
<font color="green">151</font>     
<font color="green">152</font>       contains
<font color="green">153</font>         subroutine output_gtool5_init
<font color="green">154</font>           write(6,'(a)',advance='NO') '  Input NAMELIST file: '
<font color="green">155</font>           read (5,'(a)') nmlfile
<font color="green">156</font>
<font color="green">157</font>           <b><font color="red">call HistoryAutoCreate( &amp;                            ! ヒストリー作成</font></b>
<font color="green">158</font>           <b><font color="red">     title='Shallow water equation on a sphere',             &amp;</font></b>
<font color="green">159</font>           <b><font color="red">     source='Sample program of gtool_historyauto/gtool5',    &amp;</font></b>
<font color="green">160</font>           <b><font color="red">     institution='GFD_Dennou Club davis/spmodel project',    &amp;</font></b>
<font color="green">161</font>           <b><font color="red">     dims=(/'lon','lat','t  '/), dimsizes=(/im,jm,0/),       &amp;</font></b>
<font color="green">162</font>           <b><font color="red">     longnames=(/'longitude','latitude ','time     '/),      &amp;</font></b>
<font color="green">163</font>           <b><font color="red">     units=(/'degree_east ','degree_north','sec.        '/), &amp;</font></b>
<font color="green">164</font>           <b><font color="red">     origin=ct, interval=dispint, terminus=endtime,          &amp;</font></b>
<font color="green">165</font>           <b><font color="red">     namelist_filename=nmlfile )</font></b>
<font color="green">166</font>     
<font color="green">167</font>           <b><font color="red">call HistoryAutoPutAxis('lon',x_Lon*180/pi)              ! 変数出力</font></b>
<font color="green">168</font>           <b><font color="red">call HistoryAutoAddAttr('lon','topology','circular')     ! 周期属性</font></b>
<font color="green">169</font>           <b><font color="red">call HistoryAutoAddAttr('lon','modulo',360.0)            ! 周期属性</font></b>
<font color="green">170</font>           <b><font color="red">call HistoryAutoPutAxis('lat',y_Lat*180/pi)              ! 変数出力</font></b>
<font color="green">171</font>     
<font color="green">172</font>           <b><font color="red">call HistoryAutoAddVariable( &amp;                       ! 変数定義</font></b>
<font color="green">173</font>           <b><font color="red">     varname='h', dims=(/'lon','lat','t  '/), &amp; </font></b>
<font color="green">174</font>           <b><font color="red">     longname='surface displacement ', units='m')</font></b>
<font color="green">175</font>
<font color="green">176</font>           <b><font color="red">call HistoryAutoAddVariable( &amp;                       ! 変数定義</font></b>
<font color="green">177</font>           <b><font color="red">     varname='u', dims=(/'lon','lat','t  '/), &amp; </font></b>
<font color="green">178</font>           <b><font color="red">     longname='velocity(longitude) ', units='m/s')</font></b>
<font color="green">179</font>
<font color="green">180</font>           <b><font color="red">call HistoryAutoAddVariable( &amp;                       ! 変数定義</font></b>
<font color="green">181</font>           <b><font color="red">     varname='v', dims=(/'lon','lat','t  '/), &amp; </font></b>
<font color="green">182</font>           <b><font color="red">     longname='velocity(latitude) ', units='m/s')</font></b>
<font color="green">183</font>
<font color="green">184</font>           <b><font color="red">call HistoryAutoAddVariable( &amp;                       ! 変数定義</font></b>
<font color="green">185</font>           <b><font color="red">     varname='zeta', dims=(/'lon','lat','t  '/), &amp;</font></b>
<font color="green">186</font>           <b><font color="red">     longname='vorticity', units='1/s')</font></b>
<font color="green">187</font>
<font color="green">188</font>         end subroutine output_gtool5_init
<font color="green">189</font>         
<font color="green">190</font>         subroutine output_gtool5
<font color="green">191</font>           <b><font color="red">call HistoryAutoPut(ct, 'u', xy_U)</font></b>
<font color="green">192</font>           <b><font color="red">call HistoryAutoPut(ct, 'v', xy_V)</font></b>
<font color="green">193</font>           <b><font color="red">call HistoryAutoPut(ct, 'h', xy_H)</font></b>
<font color="green">194</font>           xy_Zeta = xy_w(w_Divlon_xy(xy_V) - w_Divlat_xy(xy_U))/r0
<font color="green">195</font>           <b><font color="red">call HistoryAutoPut(ct, 'zeta', xy_Zeta)</font></b>
<font color="green">196</font>         end subroutine output_gtool5
<font color="green">197</font>
<font color="green">198</font>         subroutine output_gtool5_close
<font color="green">199</font>           <b><font color="red">call HistoryAutoClose</font></b>
<font color="green">200</font>         end subroutine output_gtool5_close
<font color="green">201</font>
<font color="green">202</font>       end program sp_topo_gtauto_v2
</pre>
=end HTML

=begin JA

== コンパイルと実行

このプログラムを実際にコンパイルして実行してみましょう.
まず, 上記ファイルと, 計算実行に必要なサブルーチンや関数の定義ファイル, 
およびサンプルとして使用する NAMELIST ファイルを以下からダウンロードしてください. 

  * ((<sp_topo_gtauto_v2.f90|URL:gtauto_first/sp_topo_gtauto_v2.f90>))
  * ((<ispack_snip.f|URL:gthist_multi/ispack_snip.f>))
  * ((<w_module_snip.f90|URL:gtauto_first/w_module_snip.f90>))
  * ((<sp_topo_gtauto1.nml|URL:gtauto_first/sp_topo_gtauto1.nml>))
  * ((<sp_topo_gtauto2.nml|URL:gtauto_first/sp_topo_gtauto2.nml>))
  * ((<sp_topo_gtauto3.nml|URL:gtauto_first/sp_topo_gtauto3.nml>))

次に, 計算に必要なサブルーチンや関数が含まれるファイルをコンパイルします.
ispack_snip.f と w_module_snip.f90
を以下のようにコンパイルしてください.

    $ gt5frt -c w_module_snip.f90 ispack_snip.f

コンパイルの結果, 
5 つのファイル (w_module.mod, w_deriv_module.mod, w_base_module.mod,
w_module_snip.o, ispack_snip.o) が作成されれば OK です.
なお, ispack_snip.f と w_module_snip.f90 はそれぞれ, 
((<ISPACK|URL:http://www.gfd-dennou.org/library/ispack>)) ライブラリと
((<spml|URL:http://www.gfd-dennou.org/library/spmodel>))
ライブラリの一部を抜粋したものです. 

sp_topo_gtauto_v2.f90 を以下のようにコンパイルしてください. 

    $ gt5frt sp_topo_gtauto_v2.f90 w_module_snip.o ispack_snip.o

これで実行ファイル a.out が作成されます. 以下のように実行してみましょう. 

    $ ./a.out

実行すると, 以下のようなメッセージが表示され NAMELIST ファイル名の入力
が求められます. まずは何も入力せずに Enter キーを入力しましょう. 

    Input NAMELIST file: 

数秒から数分でプログラムが終了し,
4 つのファイル (u.nc, v.nc, h.nc, zeta.nc) が作成されます.
これらのファイルには, それぞれ変数 (u, v, h, zeta) が格納されています. 

NetCDF のコマンド ncdump を用いて中身を確認しましょう. 

    $ ncdump -v t u.nc | more

=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
    netcdf u {
    dimensions:
            lon = 32 ;
            lat = 16 ;
            t = UNLIMITED ; // (21 currently)
    variables:
            float lon(lon) ;
                    lon:long_name = "longitude" ;
                    lon:units = "degree_east" ;
                    lon:topology = "circular" ;
                    lon:modulo = 360.f ;
            float lat(lat) ;
                    lat:long_name = "latitude" ;
                    lat:units = "degree_north" ;
            float t(t) ;
                    t:long_name = "time" ;
                    t:<b><font color="red">units = "sec."</font></b> ;
            float u(t, lat, lon) ;
                    u:long_name = "velocity(longitude)" ;
                    u:units = "m/s" ;

    // global attributes:
                    :Conventions = "http://www.gfd-dennou.org/library/gtool4/conventions/" ;
                                     : 
    data:

     <b><font color="red">t = 0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 
        450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 
        900000, 950000, 1000000</font></b> ;
    }
</pre>
=end HTML

=begin JA

赤字(カラーがでない場合はボールド)を見ると, 
データが 50000 sec ごとに出力されていることがわかります.
これは ((<gtool_historyauto を用いたサンプルプログラム>)) の
以下の個所での出力間隔の設定を反映したものです.

=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
<font color="green"> 47</font>       !---- 時間積分パラメター ----
<font color="green"> 48</font>         real(8):: dt                                   ! 時間間隔 [s]
<font color="green"> 49</font>         real(8):: dispint                              ! 出力時間間隔
                         :
                         : 
<font color="green"> 75</font>       !---- 時間積分パラメターの設定 ----
<font color="green"> 76</font>         dt = 100.                                      ! 時間間隔 [s]
<font color="green"> 77</font>         dispint = dt * 500                             ! 出力時間間隔
<font color="green"> 78</font>         endtime = dt * 10000                           ! 計算終了時間
                         :
                         : 
<font color="green">157</font>           call HistoryAutoCreate( &amp;                            ! ヒストリー作成
<font color="green">158</font>                title='Shallow water equation on a sphere',             &amp;
<font color="green">159</font>                source='Sample program of gtool_historyauto/gtool5',    &amp;
<font color="green">160</font>                institution='GFD_Dennou Club davis/spmodel project',    &amp;
<font color="green">161</font>                dims=(/'lon','lat','t  '/), dimsizes=(/im,jm,0/),       &amp;
<font color="green">162</font>                longnames=(/'longitude','latitude ','time     '/),      &amp;
<font color="green">163</font>                units=(/'degree_east ','degree_north',<b><font color="red">'sec.        '</font></b>/), &amp;
<font color="green">164</font>                origin=ct, <b><font color="red">interval=dispint</font></b>, terminus=endtime,          &amp;
<font color="green">165</font>                namelist_filename=nmlfile )</font></b>
<font color="green">166</font>     
</pre>
=end HTML

=begin JA

== NAMELIST による出力間隔等の変更

NAMELIST ファイルを用いて出力間隔を変更してみます. 
再度 a.out を実行します. 

    $ ./a.out

今回は以下のように NAMELIST ファイル名を入力してください. 

    Input NAMELIST file: sp_topo_gtauto1.nml
                         ^^^^^^^^^^^^^^^^^^^ ← ここは手動で入力してください.  

すると以下のメッセージが出力された後, 計算が実行されます. 

   　
   *** MESSAGE [HistAuto] ***  ----- "gtool_historyauto_nml" is loaded from "sp_topo_gtauto1.nml" -----
   *** MESSAGE [HistAuto] ***  Global Settings:
   *** MESSAGE [HistAuto] ***    AllOutput       = F
   *** MESSAGE [HistAuto] ***    FilePrefix      =
   *** MESSAGE [HistAuto] ***    Interval        = 1. [day]
   　
   *** MESSAGE [HistAuto] ***  Individual Settings:
   *** MESSAGE [HistAuto] ***    Name            = u, v, h, zeta
   *** MESSAGE [HistAuto] ***    File            = <Name>.nc
   *** MESSAGE [HistAuto] ***    Interval        = 1. [day]

今回も 4 つのファイル (u.nc, v.nc, h.nc, zeta.nc) が作成されます.

NetCDF のコマンド ncdump を用いて中身を確認しましょう. 

    $ ncdump -v t u.nc | more


=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
    netcdf u {
    dimensions:
            lon = 32 ;
            lat = 16 ;
            t = UNLIMITED ; // (21 currently)
    variables:
                                     : 
            float t(t) ;
                    t:long_name = "time" ;
                    t:<b><font color="red">units = "day"</font></b> ;
            float u(t, lat, lon) ;
                    u:long_name = "velocity(longitude)" ;
                    u:units = "m/s" ;

    // global attributes:
                    :Conventions = "http://www.gfd-dennou.org/library/gtool4/conventions/" ;
                                     : 
    data:

     <b><font color="red">t = 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11</font></b> ;
    }
</pre>
=end HTML

=begin JA

赤字(カラーがでない場合はボールド)を見ると, 
データが 1 day (= 86400 sec) ごとに出力されていることがわかります. 
これは ((<sp_topo_gtauto1.nml|URL:gtauto_first/sp_topo_gtauto1.nml>))
の以下の個所での出力間隔の設定を反映したものです. 

=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
    &gtool_historyauto_nml
      IntValue = <b><font color="red">1.</font></b>,             ! 出力間隔の数値
      IntUnit = <b><font color="red">'day'</font></b>            ! 出力間隔の単位
    /
    &gtool_historyauto_nml
      Name = 'u, v, h, zeta'  ! 出力変数
    /
    </pre>
=end HTML

=begin JA

((*IntUnit*)) には, "sec", "min", "hour", "day", "month", "year", "nondim" (無次元時間)
などが使用可能です. 使用可能な単位については
((<dc_date_types|URL:../code_reference/classes/dc_date_types.html>))の
"Characters list for unit" を参照ください. 


== NAMELIST による変数ごとの個別出力設定

上記の例では, 全ての変数に対して一括の出力設定を行いましたが, 
変数ごとに個別設定を行うことも可能です. 

まず, 上記で作成したファイルを掃除しておきます.

    $ rm -f u.nc v.nc h.nc zeta.nc

そして再度 a.out を実行します. 

    $ ./a.out

今回は以下のように NAMELIST ファイル名を入力してください. 

    Input NAMELIST file: sp_topo_gtauto2.nml
                         ^^^^^^^^^^^^^^^^^^^ ← ここは手動で入力してください.  

すると以下のメッセージが出力された後, 計算が実行されます. 

   　
   *** MESSAGE [HistAuto] ***  ----- "gtool_historyauto_nml" is loaded from "sp_topo_gtauto2.nml" -----
   *** MESSAGE [HistAuto] ***  Global Settings:
   *** MESSAGE [HistAuto] ***    AllOutput       = F
   *** MESSAGE [HistAuto] ***    FilePrefix      =
   *** MESSAGE [HistAuto] ***    Interval        = 1. [day]
   　
   *** MESSAGE [HistAuto] ***  Individual Settings:
   *** MESSAGE [HistAuto] ***    Name            = u, v
   *** MESSAGE [HistAuto] ***    File            = <Name>.nc
   *** MESSAGE [HistAuto] ***    Interval        = 12. [hour]
   　
   *** MESSAGE [HistAuto] ***  Individual Settings:
   *** MESSAGE [HistAuto] ***    Name            = zeta
   *** MESSAGE [HistAuto] ***    File            = <Name>.nc
   *** MESSAGE [HistAuto] ***    Interval        = 1. [day]

今回は 3 つのファイル (u.nc, v.nc, zeta.nc) が作成されます.

NetCDF のコマンド ncdump を用いて中身を確認しましょう. 

    $ ncdump -v t u.nc | more

=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
                                     : 
            float t(t) ;
                    t:long_name = "time" ;
                    t:<b><font color="red">units = "hour"</font></b> ;
            float u(t, lat, lon) ;
                    u:long_name = "velocity(longitude)" ;
                    u:units = "m/s" ;
                                     : 
    data:

     <b><font color="red">t = 0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 
       192, 204, 216, 228, 240, 252, 264, 276</font></b> ;
    }
</pre>
=end HTML

=begin JA

    $ ncdump -v t zeta.nc | more

=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
                                     : 
            float t(t) ;
                    t:long_name = "time" ;
                    t:<b><font color="red">units = "day"</font></b> ;
            float zeta(t, lat, lon) ;
                    zeta:long_name = "vorticity" ;
                    zeta:units = "1/s" ;
                                     : 
    data:

     <b><font color="red">t = 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11</font></b> ;
    }
</pre>
=end HTML

=begin JA

赤字(カラーがでない場合はボールド)を見ると, 
変数 u は 12 hour おきに出力され, 一方で変数 zeta は
1 日おきに出力されていることがわかります.

これらは ((<sp_topo_gtauto2.nml|URL:gtauto_first/sp_topo_gtauto2.nml>))
の以下の個所での出力間隔の設定を反映したものです.

* 赤字はすべての変数のデフォルトの出力設定となります. 
  変数 "zeta" はその変数名のみを記述したため, 
  このデフォルトの出力設定に従って出力されました.
  (((*Name*)) を与えないか, もしくは空文字を与えた場合に, それが
  デフォルト設定となります). 
* 緑字は変数 "u" と "v" のみの出力設定となります. 
  これは上記のデフォルト設定に上書きされるため, "u" および
  "v" についてはこの出力設定に従って出力されました. 
* 変数 "h" は記述されていないため, 出力は行われませんでした. 

=end JA

=begin HTML
<pre style="background-color: #FFFFFF; color: blue; border-style: groove; border-width: 2">
    !
    ! データ出力の全体設定
    !
    &gtool_historyauto_nml
      IntValue = <b><font color="red">1.</font></b>,              ! 出力間隔の数値
      IntUnit = <b><font color="red">'day'</font></b>,            ! 出力間隔の単位
    /
    !
    ! データ出力の個別設定
    !
    &gtool_historyauto_nml
      Name = 'u, v'               ! 出力変数
      IntValue = <b><font color="green">12.</font></b>,             ! 出力間隔の数値
      IntUnit = <b><font color="green">'hour'</font></b>,           ! 出力間隔の単位
    /
    &gtool_historyauto_nml
      Name = 'zeta'               ! 出力変数
    /
    </pre>
=end HTML

=begin JA

== 設定可能な項目

上記では出力間隔の変更を例に挙げましたが, gtool_historyauto ではこれだけでなく,
以下の項目を変更可能です. 
一通りの設定が記述された NAMELIST ファイルを
((<sp_topo_gtauto3.nml|URL:gtauto_first/sp_topo_gtauto3.nml>))
として用意しましたので, こちらを用いて設定変更をお試しください.

=== 設定項目の一覧

: File
  (文字型) 出力ファイル名.
  未指定もしくは空文字を指定した場合には, "<((|変数名|))>.nc" がファイル名となります. 

: IntValue
  (実数型) 出力間隔の数値

: IntUnit
  (文字型) 出力間隔の単位
  
: Precision
  (文字型) データの精度. "float" (単精度実数型), "double" (倍精度実数型), "int" (整数型) を指定可能
  
: TimeAverage
  (論理型) データの時間平均フラグ. ".true." を与えると, 時間平均値が出力されます

: OriginValue
  (実数型) 出力開始時刻

: OriginUnit
  (文字型) 出力開始時刻の単位
  
: TerminusValue
  (実数型) 出力終了時刻
  
: TerminusUnit
  (文字型) 出力終了時刻の単位

: SliceStart
  (整数型配列) 空間方向の開始点. 
  空間方向に一部分のみ切り出して出力する場合に使用します. 以下の SliceEnd, SliceStride も同様です.
  配列の 1 番目, 2 番目... が HistoryAutoCreate の dims に指定された次元に対応します. 
  
: SliceEnd
  (整数型配列) 空間方向の終了点
  
: SliceStride
  (整数型配列) 空間方向の刻み幅

: SpaceAverage
  (論理型配列) 空間平均のフラグ. 
  配列の 1 番目, 2 番目... が HistoryAutoCreate の dims に指定された次元に対応します.
  
: NewFileIntValue
  (整数型) ファイル分割時間間隔.
  データを時間方向に分割する場合に使用します.
  分割時には, ファイルの末尾に開始時刻に相当する数値が自動的に付加されます. 
  
: NewFileIntUnit
  (文字型) ファイル分割時間間隔の単位


=== 全体設定でのみ有効な設定項目

((*Name*)) を指定しない, もしくは空文字を与えた場合,
それは全ての変数に対するデフォルト設定となります.

その場合にのみ有効な項目として以下のものがあります. 

: AllOutput
  (論理型) HistoryAutoAddVariable によってプログラム内で登録された変数を全て出力するためのフラグ.
  デフォルトでは((<NAMELIST による変数ごとの個別出力設定>))に示すように,
  変数は明示的に指定しない限り出力されませんが, この項目を ".true." とすることで, 
  全ての変数が出力されます. 
  
: FilePrefix
  (文字型) データのファイル名の接頭詞.
  例えば "exp1-" と指定すれば, 変数 "u" の出力ファイル名は (上記の項目 "File" で変数
  "u" に対して明示的にファイル名を指定しない限りは) "exp1-u.nc" となります.
  また, "data01/" のようにスラッシュを含む文字列を指定することで,
  カレントディレクトリ以外の場所に出力するよう設定することも可能です. 

== サブルーチンの解説

((<gtool_historyauto を用いたサンプルプログラム>)) で示した, 
gtool_historyauto の各サブルーチンおよびその引数については, 
((<使われているサブルーチンの説明|URL:gtauto_desc2.htm>))
を参照してください. 

=end JA



=begin HTML
<hr /> <small>
  $Id: gtauto_first2.rd,v 1.1 2009-10-19 11:56:10 morikawa Exp $
</small>
=end HTML
