!== 暦法が dc_date_types で有効なものかどうかを調べる
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimevalidcaltype.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

function DCDateTimeValidCaltype(caltype) result(result)
  !
  ! 与えられる暦法が dc_date_types 内で有効であれば
  ! .true. を, それ以外の場合は .false. を返します.
  !
  use dc_date_types, only: PREPARED_CALTYPES
  implicit none
  integer, intent(in):: caltype
  logical:: result
  integer:: caltypes_size, i
continue
  result = .false.
  caltypes_size = size(PREPARED_CALTYPES) - 1
  do i = 0, caltypes_size
    if (caltype == PREPARED_CALTYPES(i)) then
      result = .true.
      exit
    end if
  end do
end function DCDateTimeValidCaltype
