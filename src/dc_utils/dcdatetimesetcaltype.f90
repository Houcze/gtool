!== 暦法の設定
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimesetcaltype.f90,v 1.2 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

subroutine DCDateTimeSetCaltype(caltype)
  !
  ! 暦法のデフォルトを設定します.
  ! dc_date_types#CAL_CYCLIC, dc_date_types#CAL_NOLEAP, 
  ! dc_date_types#CAL_JULIAN, dc_date_types#CAL_GREGORIAN
  ! のいづれかを引数 *caltype* に与えてください.
  !
  ! なお, この手続きを呼ばない場合, デフォルトの暦法は
  ! dc_date_types#CAL_GREGORIAN に設定されています.
  !
  use dc_date_types, only: caltype_default => caltype
  use dc_trace, only: BeginSub, EndSub
  use dc_message, only: MessageNotify
  use dc_date_generic, only: ValidCaltype
  implicit none
  integer, intent(in):: caltype
  character(*), parameter :: subname = 'DCDateTimeSetCaltype'
continue
  call BeginSub(subname, 'caltype=<%d>', i=(/caltype/))
  if (ValidCaltype(caltype)) then
    caltype_default = caltype
  else
    call MessageNotify('W', subname, &
      & 'caltype=<%d> is invalid calender type.', &
      & i=(/caltype/))
  end if
  call EndSub(subname, 'dc_date_types#caltype_default=<%d>', i=(/caltype_default/))
end subroutine DCDateTimeSetCaltype
