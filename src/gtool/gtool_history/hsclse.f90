!
!= gtool_history Fortran 77 用インターフェース
!
! Authors::   Shin-ichi TAKEHIRO, Yasuhiro MORIKAWA
! Version::   $Id: hsclse.f90,v 1.2 2009-06-01 15:17:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== サブルーチン対応表
!
! gtool_history_generic#HistoryClose :: HSCLSE
!
subroutine hsclse
  !
  !== 機能
  !
  ! ヒストリーを閉じる.

  use gtool_history
    
  CALL HistoryClose

end subroutine hsclse
