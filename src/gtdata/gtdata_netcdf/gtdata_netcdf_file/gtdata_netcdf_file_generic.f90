!
!= netCDF ファイルのオープンクローズに関する手続きの引用仕様
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_netcdf_file_generic.f90,v 1.1 2009-04-28 11:05:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module gtdata_netcdf_file_generic
  !
  != netCDF ファイルのオープンクローズに関する手続きの引用仕様
  !
  use dc_types, only: STRING
  use dc_trace, only: BeginSub, EndSub, DbgMessage

  implicit none
  private

  public:: GDNcFileOpen, GDNcFileClose, GDNcFileReopen
  public:: GDNcFileDataMode, GDNcFileDefineMode, GDNcFileSync
  public:: GDNcFileInquire

  interface
    subroutine GDNcFileOpen(fileid, filename, writable, overwrite, stat, err)
      integer, intent(out):: fileid
      character(len = *), intent(in):: filename
      logical, intent(in), optional:: writable
      logical, intent(in), optional:: overwrite
      logical, intent(out), optional:: err
      integer, intent(out), optional:: stat
    end subroutine GDNcFileOpen
  end interface

  interface
    subroutine GDNcFileClose(fileid, err)
      integer, intent(in):: fileid
      logical, intent(out), optional:: err
    end subroutine GDNcFileClose
  end interface

  interface
    subroutine GDNcFileReopen(fileid, err)
      integer, intent(in):: fileid
      logical, intent(out), optional:: err
    end subroutine GDNcFileReopen
  end interface

  interface
    integer function GDNcFileDataMode(fileid) result(result)
      integer, intent(in):: fileid
    end function GDNcFileDataMode
  end interface

  interface
    integer function GDNcFileDefineMode(fileid) result(result)
      integer, intent(in):: fileid
    end function GDNcFileDefineMode
  end interface

  interface
    subroutine GDNcFileSync(fileid, stat)
      integer, intent(in), optional:: fileid
      integer, intent(out), optional:: stat
    end subroutine GDNcFileSync
  end interface

  ! 非公開なので gtdata_netcdf_generic には置かない
  interface GDNcFileInquire
    subroutine GDNcFileInquireName(fileid, name)
      integer, intent(in):: fileid
      character(len = *), intent(out):: name
    end subroutine GDNcFileInquireName
  end interface

end module gtdata_netcdf_file_generic
