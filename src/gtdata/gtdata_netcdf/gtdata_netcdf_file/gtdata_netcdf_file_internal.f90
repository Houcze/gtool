!= gtdata_netcdf_file 内で使用される内部向け定数, 変数, 手続き群
!= Internal constants, variables, procedures used in "gtdata_netcdf_file"
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_netcdf_file_internal.f90,v 1.1 2009-05-25 09:49:24 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtdata_netcdf_file_internal
  !
  != gtdata_netcdf_file 内で使用される内部向け定数, 変数, 手続き群
  != Internal constants, variables, procedures used in "gtdata_netcdf_file"
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  use gtdata_netcdf_file_types, only: GD_NC_FILE_ID_ENTRY
  implicit none

  type(GD_NC_FILE_ID_ENTRY), save, pointer:: id_head
                              ! netCDF ファイルのオープンクローズのための
                              ! ID テーブル. 
                              !
                              ! ID table for open/close of netCDF files. 

  logical, save:: id_used = .false.

end module gtdata_netcdf_file_internal
