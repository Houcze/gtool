!= dc_utils のユーザインターフェース
!= User interface of dc_utils
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_utils.f90,v 1.6 2010-04-11 14:13:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
module dc_utils
  !
  != dc_utils のユーザインターフェイス
  != User interface of dc_utils
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! このモジュールは dc_utils ライブラリの各モジュール群の公開要素を
  ! 全て一括で提供するためのインターフェースです.
  !
  ! 利用可能な手続き等については, 下記のモジュールリストを参照ください. 
  !
  ! This module provides almost all public entities of 
  ! modules in dc_utils library.
  !
  ! See following module list for available procedures, etc. 
  !

  ! * "dc_trace#debug" -> dc_debug, for gfortran bug.
  use dc_trace, only: BeginSub, EndSub, SetDebug, DbgMessage, &
                      Dbg_Scratch, SubLevel, DataDump, DcDebug=>Debug
  use dc_types
  use dc_string
  use dc_error
  use dc_url
  use dc_message
  use dc_present
  use dc_date
  use dc_calendar
  use dc_test
  use dc_clock
  use dc_args
  use dc_hash
  use dc_iounit
  use dc_regex
  implicit none

!!$  interface debug
!!$    module procedure dcdebug
!!$  end interface

end module dc_utils
