!= gtool4 netCDF データの出力インターフェース (大規模モデル用)
!= Interface of Output of gtool4 netCDF data (For large models)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: gtool_historyauto.f90,v 1.5 2009-10-19 11:56:10 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2008. All rights reserved. 
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtool_historyauto
  !
  != gtool4 netCDF データの入出力インターフェース (大規模モデル用)
  != Interface of Input/Output of gtool4 netCDF data (For large models)
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! gtool_historyauto モジュールは gtool_history モジュールの応用版であり, 
  ! 多数の変数を出力する大規模な数値モデルを想定した, 
  ! データ出力のための簡便なインターフェースを提供します. 
  ! このモジュールは以下のような特徴を持ちます. 
  !
  ! * 複数のファイルへの出力を行う場合, 
  !   gtool_history モジュールではファイルごとに 
  !   gtool_history_generic#HistoryCreate を何度も
  !   呼び出す必要がありましたが, このモジュールでは
  !   gtool_historyauto_generic#HistoryAutoCreate をモデル内で一度呼び出すだけで済みます. 
  ! * 個別の変数について, 出力ファイルや出力間隔を手軽に変更可能です. 
  !   実際には, gtool_historyauto_generic#HistoryAutoAddVariable の引数もしくは, 
  !   NAMELIST#gtool_historyauto_nml によって変更することが可能です. 
  ! * gtool_history_types#GT_HISTORY 構造体を直接使用することなく
  !   出力を行うことが可能となっています. 
  !
  !
  ! "gtool_historyauto" module is an application of "gtool_history" module, 
  ! and provides data output easy-to-use interfaces
  ! for large numerical models that output many variables. 
  ! This module has following features. 
  !
  ! * In case that multiple files are output, 
  !   "gtool_history_generic#HistoryCreate" must be called many times at each file
  !   using "gtool_history" module, while 
  !   "gtool_historyauto_generic#HistoryAutoCreate" has to be called once in a numerical model
  !   using this module. 
  ! * Output filename or output interval, etc can be changed easily. 
  !   In practice, their settings are changed by arguments of
  !   "gtool_historyauto_generic#HistoryAutoAddVariable" or "NAMELIST#gtool_historyauto_nml". 
  ! * "gtool_history_types#GT_HISTORY" need not be used. 
  !
  !== Tutorial
  !
  ! * gtool5 オフィシャルチュートリアル: 
  !   * {多数のファイル出力を行うモデルでのデータ出力}[link:../tutorial/gtauto_first2.htm]
  !   * {使われているサブルーチンの説明}[link:../tutorial/gtauto_desc2.htm]
  ! 
  ! * Gtool5 official tutorial: 
  !   * {Data output for models that output many files (JAPANESE only)}[link:../tutorial/gtauto_first2.htm]
  !   * {Descriptions of used subroutines (JAPANESE only)}[link:../tutorial/gtauto_desc2.htm]
  ! 
  !== Procedures list
  !
  ! gtool_historyauto_generic を参照ください. 
  !
  ! See "gtool_historyauto_generic".
  !
  !== NAMELIST
  !
  ! NAMELIST#gtool_historyauto_nml
  !
  !== Acknowledgment
  !
  ! * このモジュールは, 堀之内氏による gt4_history の
  !   アプリケーション 
  !   gt4_historyauto[http://www.gfd-dennou.org/library/gtool4/gt4f90io/gt4f90io_current/doc/develop_reference/classes/gt4_historyauto_h.html]
  !   を参考にして作成しました. 
  !
  ! * gt4_historyauto[http://www.gfd-dennou.org/library/gtool4/gt4f90io/gt4f90io_current/doc/develop_reference/classes/gt4_historyauto_h.html]
  !   (document is written by Japanese)
  !   that is an application of "gt4_history"
  !   by Horinouchi is referred when this module is created. 
  !

  use gtool_historyauto_generic

end module gtool_historyauto
