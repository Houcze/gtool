!== Formatted output conversion
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcstringcprintf.f90,v 1.2 2009-03-20 09:50:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== Overview
!
! C の sprintf(3) のように文字列をフォーマットして返します。
! ただし、実装は C の sprintf(3) とは大分違うのでご注意ください。
!

function DCStringCPrintf(fmt, i, r, d, L, n, c1, c2, c3, ca) result(result)
  !
  ! フォーマット文字列 fmt に従って変換された文字列を返します。
  ! 第1引数 fmt には指示子を含む文字列を与えます。
  ! 指示子には「<tt>%</tt>」を用います。
  ! <tt>%</tt> を用いたい場合は 「<tt>%%</tt>」と記述します。
  ! 指示子に関しての詳細や用例に関しては dc_utils/dcstringsprintf.f90 を参照ください。
  !
  use dc_types, only: STRING, DP
  use dc_string, only: Printf
  implicit none
  character(len = STRING):: result
  character(*), intent(in):: fmt
  integer, intent(in), optional:: i(:), n(:)
  real, intent(in), optional:: r(:)
  real(DP), intent(in), optional:: d(:)
  logical, intent(in), optional:: L(:)
  character(*), intent(in), optional:: c1, c2, c3
  character(*), intent(in), optional:: ca(:)
continue
  call printf(result, fmt, i=i, r=r, d=d, L=L, n=n, &
    &         c1=c1, c2=c2, c3=c3, ca=ca)
end function DCStringCPrintf
