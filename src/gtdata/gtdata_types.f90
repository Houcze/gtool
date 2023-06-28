!
!= gtool 変数の構造型 "GT_VARIABLE" 宣言
!= Declaration of derived type of gtool variable "GT_VARIABLE"
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_types.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2007. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module gtdata_types

  use dc_types, only: STRING
  implicit none
  private
  public:: GT_VARIABLE

  type GT_VARIABLE
    integer:: mapid = -1
  end type GT_VARIABLE

end module
