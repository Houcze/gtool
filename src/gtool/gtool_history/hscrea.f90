!
!= gtool_history Fortran 77 用インターフェース
!
! Authors::   Shin-ichi TAKEHIRO, Yasuhiro MORIKAWA
! Version::   $Id: hscrea.f90,v 1.2 2009-06-01 15:17:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== サブルーチン対応表
!
! gtool_history_generic#HistoryCreate :: HSCREA
!
subroutine hscrea(fname,title,source,institution, &
  &               idim,dims,dimsiz,xtypes,longnames,units, &
  &               origin,interval )
  !
  !== 機能
  !
  ! ヒストリーを定義する.
  !

  use gtool_history, only: HistoryCreate
  implicit none

  character(len=*)              :: fname           ! 出力ファイル
  character(len=*)              :: title           ! データ全体の表題
  character(len=*)              :: source          ! データを作成する際の手段
  character(len=*)              :: institution     ! ファイルを最終的に変更した人/組織
  integer                       :: idim            ! 定義する軸変数の数
  character(len=1),dimension(idim)   :: dims            ! 軸変数名
  integer,dimension(idim)       :: dimsiz          ! 軸変数の大きさ
  character(len=*),dimension(idim) :: xtypes          ! 軸変数のタイプ
  character(len=*),dimension(idim) :: longnames       ! 軸変数の長い名前
  character(len=*),dimension(idim) :: units           ! 単位
  real                          :: origin          ! 開始時間
  real                          :: interval        ! 出力時間間隔

  call HistoryCreate( file=fname, title=title, &
    & source=source, institution=institution, &
    & dims=dims, dimsizes=dimsiz, xtypes=xtypes, &
    & longnames=longnames, units=units, &
    & origin=origin, interval=interval )

end subroutine hscrea
