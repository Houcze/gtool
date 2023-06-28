!== Types for memory variable support
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtdata_memory_types.f90,v 1.2 2009-04-29 05:25:05 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2001-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module gtdata_memory_types
  ! いわゆるメモリ変数をサポートします (いまのところ１次元だけ)
  use dc_types, only: STRING, TOKEN, DP

  implicit none

  type GD_MEM_ATTR_CHAIN
    type(GD_MEM_ATTR_CHAIN), pointer:: next
    character(TOKEN):: name
    character, pointer:: cbuf(:)
  end type GD_MEM_ATTR_CHAIN

  type GD_MEM_VARIABLE_ENTRY
    character(TOKEN):: name
    character(TOKEN):: xtype
    real(DP), pointer:: dbuf(:)
    type(GD_MEM_ATTR_CHAIN), pointer:: attr
    type(GD_MEM_ATTR_CHAIN), pointer:: current
  end type GD_MEM_VARIABLE_ENTRY

  type GD_MEM_VARIABLE
    integer:: id
  end type GD_MEM_VARIABLE

end module gtdata_memory_types
