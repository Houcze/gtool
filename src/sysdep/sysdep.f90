!== sysdep.f90 - module providing interface for system dependent procedures
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: sysdep.f90,v 1.1 2009-03-20 09:09:49 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! This file provides sysdep
!

module sysdep
  !
  != module providing interface for system dependent procedures
  !
  ! このモジュールはシステムに依存する手続きに関する
  ! インタフェースを提供します。
  ! 言い換えると、このモジュールではシステムに依存するサブルーチンと関数の
  ! インタフェース宣言がなされています。
  ! これらの手続きは対応する名前がついたファイル内において実装されています
  ! (すなわち <tt>sysdep</tt> で始まる名前のファイル群において、です)。
  !
  ! 従って、ある名前の手続きがあったとして、その手続きの実装は実際には
  ! 複数のファイルにおいて行われている可能性があるので気をつけてください。
  !
  ! この sysdep モジュールは他のモジュールに依存しません。
  !
  !
  ! It provides interface for system dependent procedures.
  ! In other words, there is interface declaration of a function and
  ! subroutines whose feature is regarded as system dependent.
  ! Implementation of these procedures are given in files with 
  ! corresponding name (i.e. that begins with <tt>sysdep</tt>-).
  !
  ! Note that a procedure with one name may have several implementations.
  !
  ! The sysdep has no dependence to other modules.
  !
  private
  public:: AbortProgram, SysdepArgGet, SysdepArgCount, SysdepEnvGet

  interface AbortProgram
    subroutine SysdepAbort(string)
      character(len = *):: string
    end subroutine SysdepAbort
  end interface

  interface

    integer function SysdepArgCount()
    end function SysdepArgCount

    subroutine SysdepArgGet(index, value)
      integer, intent(in):: index
      character(len = *):: value
    end subroutine SysdepArgGet

    subroutine SysdepEnvGet(env, str)
      character(len = *), intent(in)  :: env
      character(len = *), intent(out) :: str
    end subroutine SysdepEnvGet

  end interface

end module sysdep
