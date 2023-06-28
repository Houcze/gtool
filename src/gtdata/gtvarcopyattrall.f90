!
!= 属性のコピー
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarcopyattrall.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Copy_Attr
! として提供されます。

subroutine GTVarCopyAttrAll(to, from, err, global)
  !
  !== 属性のコピー
  !
  ! 変数 *from* の全ての属性を変数 *to* へコピーします。
  !
  ! デフォルトでは大域属性もコピーしますが、
  ! *global* に .false. を与える場合、大域属性をコピーしません。
  !
  ! *Copy_Attr* は 2 つのサブルーチンの総称名であり、
  ! 他にも属性を指定してコピーする方法もあります。
  ! 上記のサブルーチンを参照ください。
  !
  use gtdata_types, only: gt_variable
  use gtdata_generic, only: attr_rewind, attr_next, gtvarcopyattr
  use dc_present,only:present_and_true, present_and_false
  use dc_url,   only: GT_PLUS
  use dc_error, only: dumperror
  use dc_trace, only: beginsub, endsub, DbgMessage
  use dc_types, only: string
  type(gt_variable), intent(inout):: to
  type(gt_variable), intent(inout):: from
  logical, intent(out), optional:: err
  logical, intent(in), optional:: global
  character(len = *), parameter:: subnam = "GTVarCopyAttrAll"
  character(len = STRING):: aname
  logical:: end
continue
  if (present(err)) err = .false.
  call beginsub(subnam)
  call attr_rewind(from)
  do
    call attr_next(from, aname, end)
    if (end) exit
    if ( (present_and_false(global)) .and. (aname(1:1) == GT_PLUS) ) then
      call DbgMessage("Ignored attr=%c", c1=aname)
      cycle
    end if
    call DbgMessage("Copied attr=%c", c1=aname)
    call GTVarCopyAttr(to=to, attrname=aname, from=from, err=err)
    if (present_and_true(err)) err = .false.
  enddo
  call endsub(subnam)
end subroutine GTVarCopyAttrAll
