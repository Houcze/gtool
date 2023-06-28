!
!== Interface of gt4f90io (for backward compatibility)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: gt4f90io.f90,v 1.4 2009-06-01 15:17:22 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2008. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module gt4f90io
  !
  !== gt4f90io のユーザインターフェイス
  !
  ! このモジュールは gt4f90io への後方互換のため、
  ! gtool5 で提供される要素と同じものを提供します。
  !
  use dc_utils
  use gtdata_types
  use gtdata_generic
  use gtool_history
  use gtool_history_nmlinfo
  use gtool_historyauto
  implicit none

end module gt4f90io
