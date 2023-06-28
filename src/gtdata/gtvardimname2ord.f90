!
!= 次元相対名から次元順序番号の問い合わせ
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvardimname2ord.f90,v 1.1 2009-05-29 14:40:25 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Dimname_to_Dimord
! として提供されます。

integer function GTVarDimName2Ord(var, name) result(result)
  !
  !== 次元相対名から次元順序番号の問い合わせ
  !
  ! 変数には複数の次元が所属します。次元は順序番号で識別されますが、
  ! 変数における次元の順序は入れ替えることもできるため、
  ! 変数に相対的短い名前で識別することが便利な場合もあります。
  ! たとえば変数 <tt>filename?var</tt> に対して
  ! <tt>filename?var,dim=1</tt> のような
  ! コンマ記法 ({gtool4 netCDF 規約}[link:../xref.htm#label-6] の
  ! 「5.4 コンマ記法」参照)
  ! で用いられるものです。(NetCDF 変数 <tt>filename?varname</tt>
  ! に対する次元名 <tt>dim</tt> は <tt>filename?dim</tt>
  ! を指示するでしょうが、必ずしもそのような関係が成り立つとは限りません)
  !
  ! Dimname_to_Dimord 手続はこのような相対次元名から次元順序番号を与えます。
  ! 正当な番号は1以上であり、0以下の番号はエラーを示します。
  ! 
  use gtdata_types, only: gt_variable
  use dc_string, only: stoi
  use gtdata_netcdf_generic, only: search_dim
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf
  implicit none
  type(gt_variable), intent(in):: var
  character(len = *), intent(in):: name
  integer:: class, cid
  result = -1
  if (name == ' ') return
  result = stoi(name)
  if (result /= 0) return
  ! 個別層に問い合わせて次元番号を得ようと試みる。
  call var_class(var, class, cid)
  if (class == vtb_class_netcdf) then
    result = search_dim(GD_NC_VARIABLE(cid), name)
  endif
  ! map 表から次元名を使って次元番号を得ようと試みる。
end function GTVarDimName2Ord
