!
!= gtool_history Fortran 77 用インターフェース
!
! Authors::   Shin-ichi TAKEHIRO, Yasuhiro MORIKAWA
! Version::   $Id: hsp3r.f90,v 1.2 2009-06-01 15:17:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== サブルーチン対応表
!
! gtool_history_generic#HistoryPut :: HSP1D, HSP2D, HSP3D, HSP1R, HSP2R, HSP3R
!
subroutine hsp3r(vname,var,i,j,k)
  !
  !== 機能
  !
  ! ヒストリーに変数を出力する
  !

  use gtool_history
  character(len=*) :: vname ! 変数名
  real             :: var(i,j,k) ! 変数
  integer          :: i,j,k ! 各次元の大きさ

  call HistoryPut(vname,var)

end subroutine hsp3r
