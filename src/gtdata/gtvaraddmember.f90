!
!= 構造型変数の gt_structure_member 属性の追加
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvaraddmember.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Add_Member
! として提供されます。
!

subroutine GTVarAddMember(var, member_url, link_name)
  !
  !== 構造型変数の gt_structure_member 属性の追加
  !
  ! 変数 *var* の gt_structure_member 属性 
  ! ({gtool4 netCDF 規約}[link:../xref.htm#label-6] の 「4. 構造体変数」参照)
  ! に *member_url* の変数名部分を追加します。
  !
  use gtdata_types, only: GT_VARIABLE
  use dc_types, only: STRING
  use dc_url, only: UrlSplit
  use gtdata_generic, only: put_attr, get_attr
  use dc_error
  implicit none
  type(GT_VARIABLE), intent(inout):: var
  character(len = *), intent(in):: member_url
  character(len = *), intent(in), optional:: link_name
  character(len = string):: members, myshortname, conv
continue
  ! 短縮名称の決定
  ! 既存のメンバ名に重複しないものを選ぶ
  call get_attr(var, 'gt_structure_member', members)
  if (present(link_name)) then
    if (index(members, ' ' //link_name) == 0) then
      myshortname = link_name
      goto 1000
    endif
  endif
  conv = member_url
  call UrlSplit(conv, var=myshortname)
  ! fake 判定
  if (index(members, trim(myshortname)) /= 0) then
    call StoreError(GT_EFAKE, "GTVarAddMember(making unique name)")
  endif
1000 continue
  members = trim(members) // ' ' // trim(myshortname)
  call put_attr(var, 'gt_structure_member', trim(members))
  myshortname = 'gt_structure_link_' // trim(myshortname)
  call put_attr(var, myshortname, trim(member_url))

end subroutine GTVarAddMember
