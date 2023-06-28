!= gtool5 のユーザインターフェイス
!= User interface of gtool5
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtool5.f90,v 1.4 2009-05-29 15:08:45 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module gtool5
  !
  != gtool5 のユーザインターフェイス
  != User interface of gtool5
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! このモジュールは gtool5 ライブラリの各モジュール群の公開要素を
  ! 全て一括で提供するためのインターフェースです.
  !
  ! 利用可能な手続き等については, 下記のモジュールリストを参照ください. 
  !
  ! This module provides almost all public entities of 
  ! modules in gtool5 library.
  !
  ! See following module list for available procedures, etc. 
  !
  use dc_utils
  use gtdata_types
  use gtdata_generic
  use gtool_history
  use gtool_history_nmlinfo
  use gtool_historyauto
  implicit none

end module gtool5
