!== Generic procedures for memory variable support
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_memory_generic.f90,v 1.2 2009-04-29 05:25:06 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2001-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module gtdata_memory_generic
  ! いわゆるメモリ変数をサポートします (いまのところ１次元だけ)
  use dc_types, only: STRING, TOKEN, DP
  use gtdata_memory_types, only: GD_MEM_VARIABLE

  private

  public:: Create, Close
  public:: Attr_Rewind, Attr_Next, Attr_True, Del_Attr, Put_Attr, Get_Attr

  interface Create
    subroutine GDMemVarCreateD(var, url, length, xtype, long_name, overwrite, err)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(out):: var
      character(*), intent(in):: url
      integer, intent(in):: length
      character(*), intent(in), optional:: xtype, long_name
      logical, intent(in), optional:: overwrite
      logical, intent(out), optional:: err
    end subroutine GDMemVarCreateD

  end interface

  interface Close
    subroutine GDMemVarClose(var)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(in):: var
    end subroutine GDMemVarClose
  end interface

  interface Attr_Rewind
    subroutine GDMemVarAttrRewind(var)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(in):: var
    end subroutine GDMemVarAttrRewind
  end interface

  interface Attr_Next
    subroutine GDMemVarAttrNext(var, name, err)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(in):: var
      character(len = *), intent(out):: name
      logical, intent(out), optional:: err
    end subroutine GDMemVarAttrNext
  end interface

  interface Attr_True
    logical function GDMemVarAttrTrue(var, name, default) result(result)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      logical, intent(in), optional:: default
    end function GDMemVarAttrTrue
  end interface

  interface Del_Attr
    subroutine GDMemVarAttrDel(var, name, err)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      logical, intent(out), optional:: err
    end subroutine GDMemVarAttrDel
  end interface

  interface Put_Attr
    subroutine GDMemVarAttrAdd(var, attrname, attrval)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(in):: var
      character(*), intent(in):: attrname
      character(*), intent(in):: attrval
    end subroutine GDMemVarAttrAdd
  end interface

  interface Get_Attr
    subroutine GDMemVarAttrGet(var, name, value, err)
      use gtdata_memory_types, only: GD_MEM_VARIABLE
      type(GD_MEM_VARIABLE), intent(in):: var
      character(len = *), intent(in):: name
      character(len = *), intent(out):: value
      logical, intent(out), optional:: err
    end subroutine GDMemVarAttrGet
  end interface

end module gtdata_memory_generic
