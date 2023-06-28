!
!= ファイル入出力の同期
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarsync.f90,v 1.4 2009-05-25 09:55:57 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#GTVarSync
! として提供されます。
!

subroutine GTVarSync(var, stat)
  !
  !== ファイル入出力の同期
  !
  ! 変数 *var* に関するメモリ内のバッファと netCDF ファイルのディスク上の
  ! コピーとを同期します。*var* が与えられない場合、プログラム内で
  ! これまでに入出力した全てのファイルに関して同期がおこなわれます。
  !
  ! *stat* にはステータスが返ります。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: GDNcVarSync
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  type(GT_VARIABLE), intent(inout), optional:: var
  integer, intent(out), optional:: stat
  integer:: class, cid
  if (.not. present(var)) then
    call GDNcVarSync(stat=stat)
  else
    call var_class(var, class, cid)
    select case (class)
    case(vtb_class_netcdf)
      call GDNcVarSync(GD_NC_VARIABLE(cid), stat=stat)
    end select
  endif
end subroutine GTVarSync
