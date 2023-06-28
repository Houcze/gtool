!== Formatted output conversion
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcstringfprintf.f90,v 1.2 2009-03-20 09:50:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!== Overview
!
! C の sprintf(3) のように文字列をフォーマットして返します。
! ただし、実装は C の sprintf(3) とは大分違うのでご注意ください。
!

subroutine DCStringFPrintf(unit, fmt, i, r, d, L, n, c1, c2, c3, ca)
  !
  ! フォーマット文字列 fmt に従って変換された文字列を
  ! 装置番号 unit に返します。unit を省略する場合には標準出力に返します。
  ! 第2引数 fmt には指示子を含む文字列を与えます。
  ! 指示子には「<tt>%</tt>」を用います。
  ! <tt>%</tt> を用いたい場合は 「<tt>%%</tt>」と記述します。
  ! 指示子および用例に関しての詳細は dc_utils/dcstringsprintf.f90 を参照ください。
  !
  use dc_types, only: STRING, DP
  use dc_string, only: Printf
  implicit none
  integer, intent(in), optional:: unit
  character(*), intent(in):: fmt
  integer, intent(in), optional:: i(:), n(:)
  real, intent(in), optional:: r(:)
  real(DP), intent(in), optional:: d(:)
  logical, intent(in), optional:: L(:)
  character(*), intent(in), optional:: c1, c2, c3
  character(*), intent(in), optional:: ca(:)
  character(STRING):: buf
continue
  call printf(buf, fmt, i=i, r=r, d=d, L=L, n=n, &
    & c1=c1, c2=c2, c3=c3, ca=ca)
  if (present(unit)) then
    write(unit, '(A)') trim(buf)
  else
    write(*, '(A)') trim(buf)
  endif
end subroutine
