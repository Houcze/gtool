!
!= 入出力範囲の指定
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarsliceauto.f90,v 1.1 2009-03-20 09:09:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Slice
! として提供されます。


subroutine GTVarSliceAuto(var, compatible)
  !
  !== 入出力範囲をおまかせ指示
  !
  ! 変数 *var* の入出力範囲を「適当に」小さくします。
  ! *compatible* を指定すると、その変数と全く同じ入出力範囲に
  ! に指定します。
  !
  ! *Slice* は複数のサブルーチンの総称名であり、
  ! 他にも文字列や番号で指定する方法があります。
  ! 以下のサブルーチンを参照してください。
  !
  ! Slice に関連する手続きとして、Get_Slice と Slice_Next が
  ! あります。Get_Slice は設定された入出力範囲を取得します。 
  ! Slice_Next は入出力範囲を移動することによって、変数全体を読み取る
  ! ために利用するサブルーチンです。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: Inquire, Get_Slice, GTVarSlice
  type(GT_VARIABLE), intent(inout):: var
  type(GT_VARIABLE), intent(in), optional:: compatible
  integer:: nd, i
  integer, allocatable:: start(:), count(:), stride(:)
  call Inquire(var, alldims=nd)
  if (nd <= 0) return
  allocate(start(nd), count(nd), stride(nd))
  if (present(compatible)) then
    call Get_Slice(compatible, start, count, stride)
    do, i = 1, nd
      call GTVarSlice(var, i, start(i), count(i), stride(i))
    enddo
  else
    call Get_Slice(var, start, count, stride)
    do, i = 1, nd
      if (count(i) < 1) count(i) = 1
      call GTVarSlice(var, i, start(i), count(i), stride(i))
    enddo
  endif
  deallocate(start, count, stride)
end subroutine GTVarSliceAuto
