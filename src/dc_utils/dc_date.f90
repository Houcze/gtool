!== dc_date.f90 - 日付および時刻に関する手続きを提供するモジュール
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dc_date.f90,v 1.5 2009-06-01 15:17:23 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_date
  !== Overview
  !
  ! 日付および時刻を扱うための手続きを提供します
  !
  !== Tutorial
  !
  ! * gtool5 オフィシャルチュートリアル: 
  !   {日時および時刻の操作}[link:../tutorial/dc_date.htm]
  !
  !== Procedures List
  !
  ! dc_date_generic を参照ください. 
  !
  ! See "dc_date_generic".
  ! 
  !== Derived types
  !
  ! dc_date_types を参照ください. 
  !
  ! See "dc_date_types".
  !
  !== Usage
  !
  ! dc_date_generic を参照ください. 
  !
  ! See "dc_date_generic".
  ! 


  ! 日付時刻と時間間隔を区別する。
  ! 型宣言によって自明に定まるサブルーチンは dc_date_types に置く。

  use dc_date_types   ! 型宣言
  use dc_date_generic ! 手続の引用仕様

end module dc_date
