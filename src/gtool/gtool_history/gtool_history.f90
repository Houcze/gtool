!
!= gtool4 netCDF データの入出力インターフェース
!= Interface of Input/Output of gtool4 netCDF data
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtool_history.f90,v 1.5 2009-06-01 15:17:22 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtool_history
  !
  != gtool4 netCDF データの入出力インターフェース
  != Interface of Input/Output of gtool4 netCDF data
  !
  ! gtool_history モジュールは, 数値モデルの結果を
  ! {gtool4 netCDF 規約}[link:../xref.htm#label-6] に基づくデータ形式
  ! (以降, gtool4 データと呼びます) で出力するためのインターフェースです.
  ! 主に時間積分の結果を等時間間隔で出力することを念頭においてます.
  ! このモジュールを用いれば, Fortran90 で書かれたプログラムの計算結果を
  ! gtool4 データで出力することが簡単に実現できます.
  !
  ! なお, Fortran77 用のインターフェースとして,
  ! HSPACK[link:files/hspack_rdoc.html]
  ! も用意しています.
  !
  !== Tutorial
  !
  ! * gtool5 オフィシャルチュートリアル: 
  !   * {データ出力のための最低限の設定}[link:../tutorial/gthist_first.htm]
  !   * {使われているサブルーチンの説明}[link:../tutorial/gthist_desc.htm]
  !   * {属性(attribute)をつける}[link:../tutorial/gthist_attr.htm]
  !   * {複数のファイルに出力}[link:../tutorial/gthist_multi.htm]
  !   * {ファイルから初期値を入力}[link:../tutorial/gthist_restart.htm]
  !
  !== Prepare
  !
  ! 以下の use 文を Fortran 90 プログラムの先頭に書き込んでください.
  ! 本 gtool_history モジュール内の手続きと構造型変数が
  ! 利用できるようになります.
  !
  !     use gtool_history
  !
  !== Procedures List
  !
  ! gtool_history_generic を参照ください. 
  !
  ! See "gtool_history_generic".
  !
  !== Derived types
  !
  ! gtool_history_types を参照ください. 
  !
  ! See "gtool_history_types".
  !
  !== {gtool4 netCDF 規約}[link:../xref.htm#label-6]との対応
  !
  ! バージョン gtool4_netCDF_version に対応しています。
  !
  !=== 生成系
  !
  ! 出力するデータには以下の大域属性を必ず与えます。
  !
  ! <b>netCDF属性</b>:: <b>与えられる値</b>
  ! Conventions   :: ユーザによる指定が無い限り gtool_history_internal#gtool4_netCDF_Conventions
  !                  が与えられます.
  ! gt_version    :: ユーザによる指定が無い限り gtool_history_internal#gtool4_netCDF_version
  !                  が与えられます.
  ! title         :: ユーザによって指定されます.
  ! source        :: ユーザによって指定されます.
  ! institution   :: ユーザによって指定されます.
  ! history       :: "unknown 2005-08-05T21:48:37+09:00> gtool_history: HistoryCreate\\n"
  !                  といった値が与えられます.
  !                  "unknown" の部分には, 環境変数 USER から取得される
  !                  ユーザ名が与えられます. その後ろにはファイルの生成を
  !                  開始した時刻が与えられます.
  !
  ! 出力するデータの変数には以下の属性を必ず与えます.
  !
  ! <b>netCDF属性</b>:: <b>与えられる値</b>
  ! long_name     :: ユーザによって指定されます.
  ! units         :: ユーザによって指定されます.
  !
  ! この他の属性に関して HistoryAddAttr などによって任意に与えることは
  ! 可能です. 禁止の属性に関しては警告を発するべきですが, 現在は
  ! チェックを行っていません.
  !
  !=== 解釈系
  !
  ! 原則的に, 現在の gtool_history は全ての属性の解釈を行ないません.
  ! 本来ならば, HistoryGet は scale_factor, add_offset,
  ! valid_range などの属性を解釈すべきかも知れません. ただし,
  ! HistoryCopyVariable は変数コピーの際, 変数に属する全ての属性と
  ! その値を引き継ぎます.
  !
  !--
  !
  ! This module is designed for output to gtool4 netCDF dataset
  ! sequentially along an axis (here after it will be called '+time+').
  ! The name indicates that the module is originally intended to serve as
  ! the '+history+' of atmospheric forecast models.
  !
  !== Dependency
  !
  !* module gtdata_types for internal data access
  !* module dc_types for constants dc_types#STRING and dc_types#TOKEN
  !* module dc_trace for error trace function
  !
  !++
  use gtool_history_types
  use gtool_history_generic
  implicit none

end module gtool_history
