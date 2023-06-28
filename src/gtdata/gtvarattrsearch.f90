!
!= 変数の属性取得
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarattrsearch.f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!
!

subroutine GTVarAttrRewind(var)
  !
  !== 変数からの属性リスト取得 (初期化用)
  !
  ! *var* から属性名のリストを取得するために利用するサブルーチンです。
  ! このサブルーチンと Attr_Next によって属性リスト一覧を取得できます。
  !
  ! ある変数 *var* について全ての属性を列挙するためには、まず
  ! Attr_Rewind を呼んだ後、Attr_Next を呼びます。最初の呼び出しで
  ! 最初の属性が、次の呼び出しで次の属性の名前が得られます。最後の
  ! 属性のあとでは end == .true. となります。
  !
  ! 以下のサンプルソースコードを参照ください。
  !
  !
  !      ! 属性一覧の取得
  !      use gtool5
  !      type(GT_VARIABLE):: var
  !      character(len = STRING):: attrname
  !      logical:: end
  !      
  !      call Attr_Rewind(var)
  !      do
  !          call Attr_Next(var, attrname, end)
  !          if (end) exit
  !          write(*,*) trim(attrname)
  !      enddo
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: attr_rewind
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: attr_rewind
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  implicit none
  type(GT_VARIABLE), intent(inout), target:: var
  integer:: class, cid
continue
  call var_class(var, class, cid)
  select case(class)
  case(vtb_class_netcdf)
    call attr_rewind(GD_NC_VARIABLE(cid))
  case(vtb_class_memory)
    call attr_rewind(GD_MEM_VARIABLE(cid))
  end select
end subroutine GTVarAttrRewind

subroutine GTVarAttrNext(var, name, end)
  !
  !== 変数からの属性リスト取得
  !
  ! Attr_Rewind を参照してください。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: attr_next
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: attr_next
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  implicit none
  type(GT_VARIABLE), intent(inout), target:: var
  character(len = *), intent(out):: name
  logical, intent(out), optional:: end
  integer:: class, cid
continue
  call var_class(var, class, cid)
  select case(class)
  case(vtb_class_netcdf)
    call attr_next(GD_NC_VARIABLE(cid), name, end)
  case(vtb_class_memory)
    call attr_next(GD_MEM_VARIABLE(cid), name, end)
  end select
end subroutine GTVarAttrNext
