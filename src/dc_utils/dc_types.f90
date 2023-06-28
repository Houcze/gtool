!== Kind type parameter value
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dc_types.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! This file provides dc_types
!

module dc_types
  !
  !== Overview
  !
  ! 種別型パラメタを提供します。
  !
  use, intrinsic :: ISO_C_BINDING, only: C_FLOAT, C_DOUBLE
  use, intrinsic :: ISO_FORTRAN_ENV, only: INPUT_UNIT, OUTPUT_UNIT, ERROR_UNIT
  implicit none
  private
  public :: SP, SP_EPS
  public :: DP, DP_EPS
  public :: TOKEN, STRING
  public :: STDIN, STDOUT, STDERR

  integer, parameter:: SP  = C_FLOAT     ! Single Precision.
  real(SP), parameter :: SP_EPS = EPSILON(0.0)
  integer, parameter:: DP  = C_DOUBLE    ! Double Precision.
                                         ! 倍精度実数型変数の種別型パラメタ
                                         ! として用います。
  real(DP), parameter :: DP_EPS = EPSILON(0.0D0)

  integer, parameter:: TOKEN  = 32       ! Token.
                                         ! 単語やキーワードを保持する
                                         ! 文字型変数の種別型パラメタ
                                         ! として用います。

  integer, parameter:: STRING = 256      ! String.
                                         ! 文字列を保持する
                                         ! 文字型変数の種別型パラメタ
                                         ! として用います。
                                         !
                                         !--
                                         !開発者向け情報
                                         !
                                         ! 256 という値に深い理由はありません.
                                         ! 必要ならばより大きな値を設定
                                         ! しても構いません.
                                         ! ただし 8 バイト境界となるよう,
                                         ! 8 の倍数となっていることを
                                         ! 推奨します.
                                         !
                                         ! SR11000 の最適化
                                         ! FORTRAN90 を使用する場合に
                                         ! はだいたい 255 以下に
                                         ! 指定する必要があります.
                                         !
                                         !++

  integer, parameter:: STDIN  = INPUT_UNIT     ! 標準入力の装置番号
  integer, parameter:: STDOUT = OUTPUT_UNIT    ! 標準出力の装置番号
  integer, parameter:: STDERR = ERROR_UNIT     ! 標準エラー出力の装置番号

end module
