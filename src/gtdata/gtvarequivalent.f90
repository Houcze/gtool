!
!= GT_VARIABLE 型変数の同値判定
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarequivalent.f90,v 1.3 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から
! gtdata_generic#operator(.equivalent.) として提供されます。

logical function GTVarEquivalent(var1, var2) result(result)
  !
  !== GT_VARIABLE 型変数の同値判定
  !
  ! 変数 <b>var1</b>, <b>var2</b> を比較し、同値である場合は .true. を、
  ! そうで無い場合は .false. を返します。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class
  type(GT_VARIABLE), intent(in):: var1, var2
  integer:: class1, class2, cid1, cid2
continue
  call var_class(var1, class1, cid1)
  call var_class(var2, class2, cid2)
  result = (class1 == class2) .and. (cid1 == cid2)
end function GTVarEquivalent
