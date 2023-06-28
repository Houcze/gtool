! -*- coding: utf-8; mode: f90 -*-
!-------------------------------------------------------------------------------------
! Copyright (c) 2000-2016 Gtool Development Group. All rights reserved.
!-------------------------------------------------------------------------------------
! ** Important**
!
! This file is generated from gtvargetnum.erb by ERB included Ruby 2.3.1.
! Please do not edit this file directly. @see "gtvargetnum.erb"
!-------------------------------------------------------------------------------------
!
!= 固定長配列への数値データの入力
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvargetnum.rb2f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Get
! として提供されます。
!

!== 固定長配列への数値データの入力
!
! 変数 *var* から *value* に数値データが入力されます。
! *nvalue* には配列長を代入する必要があります。
!
! 数値データ入力の際にエラーが生じた場合、メッセージを出力
! してプログラムは強制終了します。*err* を与えてある場合には
! の引数に .true. が返り、プログラムは終了しません。
!
! 入力しようとするデータの型が引数の型と異なる場合、データは引数の
! 型に変換されます。 この変換は netCDF の機能を用いています。
! 詳しくは {netCDF 日本語版マニュアル}[link:../xref.htm#label-10]
! の 3.3 型変換 を参照してください。
!
! *Get* は複数のサブルーチンの総称名であり、
! *value* にポインタ型の配列を与えることも可能です。上記の
! サブルーチンを参照してください。

subroutine GTVarGetDouble(var, value, nvalue, err)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: &
    & var_class, &
    & vtb_class_netcdf, &
    & map_to_internal_specs, &
    & gtvar_dump
  use gtdata_netcdf_generic, only: Get
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_types,   only: STRING, DP
  use dc_error,   only: StoreError, DC_NOERR, DC_ENEGATIVE, GT_EFAKE
  implicit none
  type(GT_VARIABLE), intent(in)            :: var
  real(DP),          intent(out)           :: value(*)
  integer,           intent(in)            :: nvalue
  logical,           intent(out), optional :: err
  integer                                  :: class, cid, stat
  integer                  , pointer       :: specs(:, :)
  character(STRING):: cause_c
  character(len = *), parameter:: subname = 'GTVarGetDouble'
  continue
  call var_class(var, class, cid)
  stat = DC_NOERR
  cause_c = ''
  if (nvalue < 1) then
    stat = DC_ENEGATIVE
    cause_c = 'nvalue'
    goto 999
  end if
  call gtvar_dump(var)
  call map_to_internal_specs(var, specs)
  if (class == vtb_class_netcdf) then
    call Get(GD_NC_VARIABLE(cid), start=specs(:, 1), count=specs(:, 2), &
      & stride=specs(:, 3), imap=specs(:, 4), siz=nvalue, value=value, &
      & iostat=stat)
  else
    stat = GT_EFAKE
  endif
  if (associated(specs)) deallocate(specs)
999 continue
  call StoreError(stat, subname, cause_c = cause_c, err = err)
end subroutine GTVarGetDouble

subroutine GTVarGetReal(var, value, nvalue, err)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: &
    & var_class, &
    & vtb_class_netcdf, &
    & map_to_internal_specs, &
    & gtvar_dump
  use gtdata_netcdf_generic, only: Get
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_types,   only: STRING ,SP
  use dc_error,   only: StoreError, DC_NOERR, DC_ENEGATIVE, GT_EFAKE
  implicit none
  type(GT_VARIABLE), intent(in)            :: var
  real(SP),          intent(out)           :: value(*)
  integer,           intent(in)            :: nvalue
  logical,           intent(out), optional :: err
  integer                                  :: class, cid, stat
  integer                  , pointer       :: specs(:, :)
  character(STRING):: cause_c
  character(len = *), parameter:: subname = 'GTVarGetDouble'
  continue
  call var_class(var, class, cid)
  stat = DC_NOERR
  cause_c = ''
  if (nvalue < 1) then
    stat = DC_ENEGATIVE
    cause_c = 'nvalue'
    goto 999
  end if
  call gtvar_dump(var)
  call map_to_internal_specs(var, specs)
  if (class == vtb_class_netcdf) then
    call Get(GD_NC_VARIABLE(cid), start=specs(:, 1), count=specs(:, 2), &
      & stride=specs(:, 3), imap=specs(:, 4), siz=nvalue, value=value, &
      & iostat=stat)
  else
    stat = GT_EFAKE
  endif
  if (associated(specs)) deallocate(specs)
999 continue
  call StoreError(stat, subname, cause_c = cause_c, err = err)
end subroutine GTVarGetReal

subroutine GTVarGetInt(var, value, nvalue, err)
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: &
    & var_class, &
    & vtb_class_netcdf, &
    & map_to_internal_specs, &
    & gtvar_dump
  use gtdata_netcdf_generic, only: Get
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_types,   only: STRING
  use dc_error,   only: StoreError, DC_NOERR, DC_ENEGATIVE, GT_EFAKE
  implicit none
  type(GT_VARIABLE), intent(in)            :: var
  integer,           intent(out)           :: value(*)
  integer,           intent(in)            :: nvalue
  logical,           intent(out), optional :: err
  integer                                  :: class, cid, stat
  integer                  , pointer       :: specs(:, :)
  character(STRING):: cause_c
  character(len = *), parameter:: subname = 'GTVarGetDouble'
  continue
  call var_class(var, class, cid)
  stat = DC_NOERR
  cause_c = ''
  if (nvalue < 1) then
    stat = DC_ENEGATIVE
    cause_c = 'nvalue'
    goto 999
  end if
  call gtvar_dump(var)
  call map_to_internal_specs(var, specs)
  if (class == vtb_class_netcdf) then
    call Get(GD_NC_VARIABLE(cid), start=specs(:, 1), count=specs(:, 2), &
      & stride=specs(:, 3), imap=specs(:, 4), siz=nvalue, value=value, &
      & iostat=stat)
  else
    stat = GT_EFAKE
  endif
  if (associated(specs)) deallocate(specs)
999 continue
  call StoreError(stat, subname, cause_c = cause_c, err = err)
end subroutine GTVarGetInt
