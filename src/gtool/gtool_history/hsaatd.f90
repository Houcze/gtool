!
!= gtool_history Fortran 77 用インターフェース
!
! Authors::   Shin-ichi TAKEHIRO, Yasuhiro MORIKAWA
! Version::   $Id: hsaatd.f90,v 1.2 2009-06-01 15:17:20 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== サブルーチン対応表
!
! gtool_history_generic#HistoryAddAttr :: HSAATC,HSAATR,HSAATD,HSAATI
!
subroutine hsaatd(vname, atname, value,i)
  !
  !== 機能
  !
  ! ヒストリー, 変数に属性をつける.
  !
  !== 備考
  !
  ! VNAME が空の場合にはヒストリーのグローバル属性として扱われる.
  !
  use gtool_history
  character(len=*), intent(in):: vname ! 変数名
  character(len=*), intent(in):: atname ! 属性名
  double precision, intent(in):: value(i) ! 変数
  integer          :: i ! 属性の次元数

  call HistoryAddAttr(vname, atname, value)

end subroutine hsaatd
