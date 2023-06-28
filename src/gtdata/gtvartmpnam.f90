!
!= 変数名の自動作成
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvartmpnam.f90,v 1.1 2009-05-29 14:40:25 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#GTDataTmpNam
! として提供されます。
!

subroutine GTVarTmpNam(file, base, result)
  !
  !== 変数名の自動作成
  !
  ! ファイル名 *file* と変数名 *base* から <b>file@base</b> を
  ! 作成して *result* に返す。
  ! *file* が空文字の場合は <b>gtool.nc</b> がファイル名として代用され、
  ! *base* が空文字の場合は <b>tmp</b> が変数名として代用される。
  !
  use dc_types, only: STRING
  use dc_url, only: UrlMerge
  use gtdata_generic, only: Open, Close
  use gtdata_types, only: GT_VARIABLE
  implicit none
  character(len = *), intent(in):: file
  character(len = *), intent(in):: base
  character(len = *), intent(out):: result
  type(GT_VARIABLE):: var
  integer:: n
  logical:: failed
  character(len = STRING):: fnam, vnam, vnambase
continue
  if (file == "") then
    fnam = "gtool.nc"
  else
    fnam = file
  endif
  if (base == "") then
    vnambase = "tmp"
  else
    vnambase = base
  endif
  n = 0
  do
    n = n + 1
    ! compatibility note (2001-12-02, TOYODA Eizi)
    ! dc_string の toChar を使いたいのだが FQS Fortran が失敗する
    write(unit=vnam, fmt="(i32)") n
    vnam = trim(vnambase) // trim(adjustl(vnam))
    result = UrlMerge(file=fnam, var=vnam)
    call Open(var, result, writable=.FALSE., err=failed)
    if (failed) return
    call Close(var)
  enddo
end subroutine GTVarTmpNam
