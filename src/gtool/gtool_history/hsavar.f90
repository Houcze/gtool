!
!= gtool_history Fortran 77 用インターフェース
!
! Authors::   Shin-ichi TAKEHIRO, Yasuhiro MORIKAWA
! Version::   $Id: hsavar.f90,v 1.2 2009-06-01 15:17:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== サブルーチン対応表
!
! gtool_history_generic#HistoryAddVariable :: HSAVAR
!
subroutine hsavar(vname,idim,dims,lname,unit,xtype)
  !
  !== 機能
  !
  ! 変数を定義する.
  !
  use gtool_history
  character(len=*),intent(in) :: vname            ! 変数名
  integer,intent(in)          :: idim             ! 定義する変数の次元の大きさ
  character(len=*),dimension(idim),intent(in) :: dims ! 次元
  character(len=*),intent(in) :: lname                ! 長い名前
  character(len=*),intent(in) :: unit                 ! 単位
  character(len=*),intent(in) :: xtype                ! 定義する変数のタイプ

  call HistoryAddVariable(varname=vname, dims=dims, &
    &                     longname=lname, units=unit, xtype=xtype)

end subroutine hsavar
