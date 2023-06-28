!= NAMELIST の使用を想定したヒストリデータ出力情報管理用ユーティリティ
!
!= Utilities for history data output information management assuming use of NAMELIST
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: gtool_history_nmlinfo.f90,v 1.5 2009-06-01 15:17:18 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtool_history_nmlinfo
  !
  != NAMELIST の使用を想定したヒストリデータ出力情報管理用ユーティリティ
  !
  != Utilities for history data output information management assuming use of NAMELIST
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! 比較的大規模な数値モデルにおいて, データ出力の情報管理を
  ! 支援するためのモジュールです. 
  ! 個別のモジュールがそれぞれ独立にデータ出力を行うことと, 
  ! NAMELIST を用いて出力ファイルや出力間隔などを変更すること
  ! 想定して設計されています. 
  ! ただし, このモジュール自体の主目的は情報の管理であり, 
  ! 実際のデータ出力には gtool_history モジュールを
  ! 使用することに注意してください. 
  !
  ! This module supports information management of data output 
  ! in a comparatively large-scale numerical model. 
  ! This module is designed expecting that individual modules 
  ! perform data output independently, and output filename or 
  ! output interval is changed from NAMELIST. 
  ! Note that the purpose of this module is information management, 
  ! therefore actual data output is performed by "gtool_history" module.
  !
  !== Procedures List
  !
  ! gtool_history_nmlinfo_generic を参照ください. 
  !
  ! See "gtool_history_nmlinfo_generic".
  !
  !== Derived types
  !
  ! gtool_history_nmlinfo_types を参照ください. 
  !
  ! See "gtool_history_nmlinfo_types".
  !
  !== Usage
  !
  ! gtool_history_nmlinfo_generic を参照ください. 
  !
  ! See "gtool_history_nmlinfo_generic".
  !

  use gtool_history_nmlinfo_generic
  use gtool_history_nmlinfo_types

end module gtool_history_nmlinfo
