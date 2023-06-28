!
!= netCDF ファイルのオープンクローズ用のオブジェクト
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_netcdf_file_types.f90,v 1.1 2009-04-28 11:05:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtdata_netcdf_file_types
  !
  !== netCDF ファイルのオープンクローズ用のオブジェクト
  !
  use dc_types, only: STRING
  use dc_trace, only: BeginSub, EndSub, DbgMessage

  implicit none
  private

  type GD_NC_FILE_ID_ENTRY
    integer:: id
    integer:: count
    logical:: writable
    character(len = STRING):: filename
    type(GD_NC_FILE_ID_ENTRY), pointer:: next
  end type GD_NC_FILE_ID_ENTRY

  public:: GD_NC_FILE_ID_ENTRY

end module gtdata_netcdf_file_types
