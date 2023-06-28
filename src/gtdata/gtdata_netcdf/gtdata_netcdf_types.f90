!= derived type interface for `abstract netcdf' variable interface
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_netcdf_types.f90,v 1.2 2009-05-25 09:51:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtdata_netcdf_types

  implicit none

  type GD_NC_VARIABLE
    ! == ユーザ (gtdata 層) が使うべきハンドル
    ! id は gtdata_netcdf_internal モジュールの gdnctab 表のインデックスとなる。
    integer:: id
  end type GD_NC_VARIABLE

  type GD_NC_VARIABLE_ENTRY
    !== gtdata_netcdf_internal モジュールの gdnctab 表の要素
    !
    !=== ID 情報
    !
    ! 変数 (gtdata_netcdf_variable 実体) は (/fileid, varid, dimid/) で
    ! 同定される。正当な変数の fileid は必ず正である。
    !
    !
    !=== 次元表
    !
    ! 次元変数については自次元が、非次元変数については
    ! 自分にとっての次元の dimid の一覧が保存される。
    !
    !=== 属性サーチ用イテレータ
    !
    integer:: fileid             ! ID 情報
    integer:: varid              ! ID 情報
    integer:: dimid              ! ID 情報
    integer, pointer:: dimids(:) ! 次元表
    integer:: attrid             ! 属性サーチ用イテレータ
  end type GD_NC_VARIABLE_ENTRY

  type GD_NC_VARIABLE_SEARCH
    !== 変数リスト検索イテレータ用
    integer:: fileid
    integer:: varid
    integer:: dimid
  end type GD_NC_VARIABLE_SEARCH

end module gtdata_netcdf_types
