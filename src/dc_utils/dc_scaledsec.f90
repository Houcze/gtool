!= 小数点以下の「秒」や整数型では表現できない大きい数を正確に演算するためのモジュール 
!= A module for correct operations of "seconds" after the decimal point, and large number more than integer type
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_scaledsec.f90,v 1.1 2009-03-20 09:09:53 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2008. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_scaledsec
  ! 
  ! #assignment(=)   :: 代入
  ! #operator(+)     :: 加算
  ! #operator(-)     :: 減算
  ! #operator(*)     :: 乗算
  ! #operator(/)     :: 除算
  ! mod              :: 余り
  ! modulo           :: 剰余
  ! #operator(==)    :: 比較
  ! #operator(>)     :: 比較
  ! #operator(<)     :: 比較
  ! abs              :: 絶対値の算出
  ! int              :: 整数の算出 (小数点以下切捨て)
  ! sign             :: 符号の設定
  ! floor            :: 整数の算出 (対象の数値以下で最大の整数)
  ! ceiling          :: 整数の算出 (対象の数値以上で最小の整数)
  !
  use dc_types, only: DP
  implicit none
  private

  public:: DC_SCALED_SEC
  public:: assignment(=), DCScaledSecPutLine
  public:: operator(==), operator(>), operator(<), operator(>=), operator(<=)
  public:: operator(+), operator(-), operator(*), operator(/), mod, modulo
  public:: abs, int, sign, floor, ceiling

!!$  integer, parameter:: imin = -1  ! 最小値の指数 = imin*6
!!$  integer, parameter:: imax =  4  ! 最大値の指数 = imax*6
!!$  real(DP), parameter:: scale_factor = 1.0e+6_DP
!!$  integer, parameter:: scale_factor_int = 1000000
  integer, parameter:: imin = -2  ! 最小値の指数 = imin*3
  integer, parameter:: imax =  8  ! 最大値の指数 = imax*3
  real(DP), parameter:: scale_factor = 1.0e+3_DP
  real(DP), parameter:: scale_factor_xx (-(imax+1):imax+1) = &
    & (/ 1.0e-27_DP, &
    &    1.0e-24_DP, 1.0e-21_DP, 1.0e-18_DP, 1.0e-15_DP, &
    &    1.0e-12_DP, 1.0e-9_DP,  1.0e-6_DP,  1.0e-3_DP,  &
    &    1.0_DP, &
    &    1.0e+3_DP,  1.0e+6_DP,  1.0e+9_DP,  1.0e+12_DP, &
    &    1.0e+15_DP, 1.0e+18_DP, 1.0e+21_DP, 1.0e+24_DP, &
    &    1.0e+27_DP /)

  integer, parameter:: scale_factor_int = 1000
  integer, parameter:: scale_factor_int_xx (0:3) = &
    & (/ 1, 1000, 1000000, 1000000000 /)

  type DC_SCALED_SEC
    !
    ! 小数点以下の「秒」や, 整数型では表現できないほど大きい数を
    ! 正確に演算するための型. 
    !
    ! Derived type for precise operations of "seconds" after 
    ! the decimal point, and large number more than integer type. 
    !
    sequence
    integer:: sec_ary(imin:imax) = 0
    logical:: flag_negative = .false.
    logical:: dummy = .false.
  end type DC_SCALED_SEC

  interface assignment(=)
    module procedure DCScaledSecCreateR !:doc-priority 20:
    module procedure DCScaledSecCreateD !:doc-priority 30:
    module procedure DCScaledSecCreateI !:doc-priority 40:

    module procedure DCScaledSecToNumR !:doc-priority 60:
    module procedure DCScaledSecToNumD !:doc-priority 70:
    module procedure DCScaledSecToNumI !:doc-priority 80:
  end interface

  interface PutLine
    module procedure DCScaledSecPutLine
  end interface

  interface operator(==)
    module procedure dcscaledsec_eq_ss !:doc-priority 20:
    module procedure dcscaledsec_eq_si !:doc-priority 61:
    module procedure dcscaledsec_eq_is !:doc-priority 62:
    module procedure dcscaledsec_eq_sr !:doc-priority 63:
    module procedure dcscaledsec_eq_rs !:doc-priority 64:
    module procedure dcscaledsec_eq_sd !:doc-priority 65:
    module procedure dcscaledsec_eq_ds !:doc-priority 66:
  end interface

  interface operator(>)
    module procedure dcscaledsec_gt_ss
    module procedure dcscaledsec_gt_si
    module procedure dcscaledsec_gt_is
  end interface

  interface operator(<)
    module procedure dcscaledsec_lt_ss
    module procedure dcscaledsec_lt_si
    module procedure dcscaledsec_lt_is
  end interface

  interface operator(>=)
    module procedure dcscaledsec_ge_ss
    module procedure dcscaledsec_ge_si
    module procedure dcscaledsec_ge_is
  end interface

  interface operator(<=)
    module procedure dcscaledsec_le_ss
    module procedure dcscaledsec_le_si
    module procedure dcscaledsec_le_is
  end interface

  interface operator(+)
    module procedure dcscaledsec_add_ss
    module procedure dcscaledsec_add_si
    module procedure dcscaledsec_add_is
    module procedure dcscaledsec_add_sr
    module procedure dcscaledsec_add_rs
    module procedure dcscaledsec_add_sd
    module procedure dcscaledsec_add_ds
  end interface

  interface operator(-)
    module procedure dcscaledsec_sub_s
    module procedure dcscaledsec_sub_ss
    module procedure dcscaledsec_sub_si
    module procedure dcscaledsec_sub_is
    module procedure dcscaledsec_sub_sr
    module procedure dcscaledsec_sub_rs
    module procedure dcscaledsec_sub_sd
    module procedure dcscaledsec_sub_ds
  end interface

  interface operator(*)
    module procedure dcscaledsec_mul_ss
    module procedure dcscaledsec_mul_si
    module procedure dcscaledsec_mul_is
    module procedure dcscaledsec_mul_sd
    module procedure dcscaledsec_mul_ds
    module procedure dcscaledsec_mul_sr
    module procedure dcscaledsec_mul_rs
  end interface

  interface operator(/)
    module procedure dcscaledsec_div_si
    module procedure dcscaledsec_div_sr
    module procedure dcscaledsec_div_sd
    module procedure dcscaledsec_div_ss
  end interface

  interface mod
    module procedure dcscaledsec_mod_si
    module procedure dcscaledsec_mod_sr
    module procedure dcscaledsec_mod_sd
    module procedure dcscaledsec_mod_ss
  end interface

  interface modulo
    module procedure dcscaledsec_modulo_si
    module procedure dcscaledsec_modulo_sr
    module procedure dcscaledsec_modulo_sd
    module procedure dcscaledsec_modulo_ss
  end interface

  interface abs
    module procedure dcscaledsec_abs_s
  end interface

  interface int
    module procedure dcscaledsec_int_s
  end interface

  interface sign
    module procedure dcscaledsec_sign_si
    module procedure dcscaledsec_sign_sr
    module procedure dcscaledsec_sign_sd
    module procedure dcscaledsec_sign_ss
  end interface

  interface floor
    module procedure dcscaledsec_floor_s
  end interface

  interface ceiling
    module procedure dcscaledsec_ceiling_s
  end interface

contains

  !-------------------------------------------------------------------

  subroutine DCScaledSecCreateI(sclsec, sec)
    implicit none
    type(DC_SCALED_SEC), intent(out):: sclsec
    integer, intent(in):: sec
  continue
    call DCScaledSecCreateD(sclsec, real( sec, DP ))
  end subroutine DCScaledSecCreateI

  !-------------------------------------------------------------------

  subroutine DCScaledSecCreateR(sclsec, sec)
    implicit none
    type(DC_SCALED_SEC), intent(out):: sclsec
    real, intent(in):: sec
  continue
    call DCScaledSecCreateD(sclsec, real( sec, DP ))
  end subroutine DCScaledSecCreateR

  !-------------------------------------------------------------------

  subroutine DCScaledSecCreateD(sclsec, sec)
    use dc_message, only: MessageNotify
    use dc_error, only: StoreError, DC_NOERR, DC_ETOOLARGETIME
    use dc_trace, only: BeginSub, EndSub
    use dc_types, only: DP, STRING
    implicit none
    type(DC_SCALED_SEC), intent(out):: sclsec
    real(DP), intent(in):: sec

    real(DP):: work_sec, print_sec
    integer:: i, cd, move_up, work_sec_scl_nint

    integer :: stat
    character(STRING) :: cause_c
    character(*), parameter:: subname = 'dc_scaledsec'
  continue
    !call BeginSub(subname, 'sec=<%f>', d = (/ sec /) )
    stat = DC_NOERR
    cause_c = ''

    cd = 0
    if ( sec < 0.0_DP ) then
      sclsec % flag_negative = .true.
      work_sec = - sec
    else
      sclsec % flag_negative = .false.
      work_sec = sec
    end if

    if ( work_sec > scale_factor_xx (imax + 1) ) then
      call MessageNotify( 'W', subname, &
        & 'input number (%f) is too large.', &
        & d = (/ sec /) )
      stat = DC_ETOOLARGETIME
      goto 999
    end if

    sclsec % sec_ary = 0
    do i = imax, imin, -1

      work_sec_scl_nint = nint( work_sec * scale_factor_xx(-i) )
      if ( .not. work_sec < scale_factor_xx(i) &
        &  .or.  ( i == imin .and. work_sec_scl_nint >= 1 )  ) then

        if ( i < 0 ) then
          sclsec % sec_ary(i) = work_sec_scl_nint
        else
          sclsec % sec_ary(i) = int( work_sec / scale_factor_xx(i) )
        end if
        work_sec = work_sec - sclsec % sec_ary(i) * scale_factor_xx(i)
        cd = cd + count_digit( sclsec % sec_ary(i) )
      end if
      if ( cd > 5 ) then
        if ( .not. abs( work_sec ) < scale_factor_xx(i-1) ) then
          print_sec = sclsec
!!$          call MessageNotify( 'W', subname, &
!!$            & 'input number (%f) is truncated to (%f).', &
!!$            & d = (/ sec, print_sec /) )
        end if
        exit
      end if
    end do

    move_up = 0
    do i = imin, imax
      sclsec % sec_ary(i) = sclsec % sec_ary(i) + move_up
      move_up = 0
      do while ( sclsec % sec_ary(i) >= scale_factor_int )
        move_up = move_up + 1
        sclsec % sec_ary(i) = sclsec % sec_ary(i) - scale_factor_int
      end do
    end do

999 continue
    call StoreError(stat, subname, cause_c=cause_c)
    !call EndSub(subname)
  end subroutine DCScaledSecCreateD

  !-------------------------------------------------------------------

  subroutine DCScaledSecToNumI(sec, sclsec)
    use dc_types, only: DP
    implicit none
    integer, intent(out):: sec
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP):: secd
  continue
    call DCScaledSecToNumD(secd, sclsec)
    sec = nint( secd )
  end subroutine DCScaledSecToNumI

  !-------------------------------------------------------------------

  subroutine DCScaledSecToNumR(sec, sclsec)
    use dc_types, only: DP
    implicit none
    real, intent(out):: sec
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP):: secd
  continue
    call DCScaledSecToNumD(secd, sclsec)
    sec = real( secd )
  end subroutine DCScaledSecToNumR

  !-------------------------------------------------------------------

  subroutine DCScaledSecToNumD(sec, sclsec)
    use dc_types, only: DP
    implicit none
    real(DP), intent(out):: sec
    type(DC_SCALED_SEC), intent(in):: sclsec

    integer:: i
  continue
    sec = 0.0_DP
    do i = imax, imin, -1
      sec = sec + ( sclsec % sec_ary(i) * scale_factor_xx(i) )
    end do
    if ( sclsec % flag_negative ) sec = - sec
  end subroutine DCScaledSecToNumD

  !-------------------------------------------------------------------

  subroutine DCScaledSecPutLine( sclsec, unit, indent )
    !
    ! 引数 *sclsec* に設定されている情報を印字します. 
    ! デフォルトではメッセージは標準出力に出力されます. 
    ! *unit* に装置番号を指定することで, 出力先を変更することが可能です. 
    !
    ! Print information of *sclsec*. 
    ! By default messages are output to standard output. 
    ! Unit number for output can be changed by *unit* argument. 
    !
    use dc_string, only: Printf, toChar
    use dc_trace, only: BeginSub, EndSub
    use dc_types, only: STDOUT, STRING
    implicit none
    type(DC_SCALED_SEC), intent(in) :: sclsec
    integer, intent(in), optional :: unit
                              ! 出力先の装置番号. 
                              ! デフォルトの出力先は標準出力. 
                              !
                              ! Unit number for output. 
                              ! Default value is standard output. 
    character(*), intent(in), optional:: indent
                              ! 表示されるメッセージの字下げ. 
                              !
                              ! Indent of displayed messages. 

    integer :: out_unit, sec_ary_rev(imin:imax)
    integer:: indent_len
    character(STRING):: indent_str
    character(1):: sign
    character(*), parameter:: subname = 'DCScaledSecPutLine'
  continue
    !call BeginSub(subname)

    if (present(unit)) then
      out_unit = unit
    else
      out_unit = STDOUT
    end if

    indent_len = 0
    indent_str = ''
    if ( present(indent) ) then
      if ( len(indent) /= 0 ) then
        indent_len = len(indent)
        indent_str(1:indent_len) = indent
      end if
    end if

    sec_ary_rev(imin:imax) = sclsec % sec_ary(imax:imin:-1)
    if ( sclsec % flag_negative ) then
      sign = '-'
    else
      sign = '+'
    end if
    if ( imax - imin + 1 == 6 ) then
      call Printf(out_unit, &
        & indent_str(1:indent_len) // &
        & '#<DC_SCALED_SEC:: @sign=%c @yotta=%d @exa=%d @tera=%d @mega=%d @base=%d @micro=%d>', &
        & i = sec_ary_rev, c1 = sign )
    elseif ( imax - imin + 1 == 11 ) then
      call Printf(out_unit, &
        & indent_str(1:indent_len) // &
        & '#<DC_SCALED_SEC:: @sign=%c @yotta=%d @zetta=%d @exa=%d @peta=%d @tera=%d', &
        & i = sec_ary_rev(imin:imin+4), c1 = sign )
      call Printf(out_unit, &
        & indent_str(1:indent_len) // &
        & '                          @giga=%d @mega=%d @kilo=%d @base=%d @milli=%d @micro=%d>', &
        & i = sec_ary_rev(imax-5:imax) )
    else
      call Printf(out_unit, &
        & indent_str(1:indent_len) // &
        & '#<DC_SCALED_SEC:: @sign=%c @sec_ary=%*d>', &
        & i = sec_ary_rev, n = (/ imax - imin + 1 /), c1 = sign )
    end if
  999 continue
    !call EndSub(subname)
  end subroutine DCScaledSecPutLine

  !-------------------------------------------------------------------

  logical function dcscaledsec_eq_ss(sclsec1, sclsec2) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2

    integer:: i
  continue
    if ( sclsec1 % flag_negative .and. .not. sclsec2 % flag_negative ) then
      result = .false.

      return
    elseif ( .not. sclsec1 % flag_negative .and. sclsec2 % flag_negative ) then
      result = .false.
      return
    end if

    do i = imax, imin, -1
      if ( .not. sclsec1 % sec_ary(i) == sclsec2 % sec_ary(i) ) then
        result = .false.
        return
      end if
    end do

    result = .true.
  end function dcscaledsec_eq_ss

  !-------------------------------------------------------------------

  logical function dcscaledsec_eq_si(sclsec, sec) result(result)
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: sec
    type(DC_SCALED_SEC):: sclsec2
    integer:: i, sec1
  continue
    if ( sclsec % flag_negative .and. .not. sec < 0 ) then
      result = .false.
      return
    elseif ( .not. sclsec % flag_negative .and. sec < 0 ) then
      result = .false.
      return
    end if

    if ( abs(sec) > scale_factor_int_xx(3) ) then
      sclsec2 = sec
      result = sclsec == sclsec2
    else
      if (      .not. all( sclsec % sec_ary(imin:-1) == (/0, 0/) ) &
        &  .or. .not. all( sclsec % sec_ary(3:imax) == (/0, 0, 0, 0, 0, 0/) ) ) then
        result = .false.
        return
      end if
      sec1 = sclsec % sec_ary(0)
      do i = 1, 2
        sec1 = sec1 + sclsec % sec_ary(i) * scale_factor_int_xx(i)
      end do
      result = sec1 == sec
    end if
  end function dcscaledsec_eq_si

  !-------------------------------------------------------------------

  logical function dcscaledsec_eq_is(sec, sclsec) result(result)
    implicit none
    integer, intent(in):: sec
    type(DC_SCALED_SEC), intent(in):: sclsec
  continue
    result = sclsec == sec
  end function dcscaledsec_eq_is

  !-------------------------------------------------------------------

  logical function dcscaledsec_eq_sr(sclsec, sec) result(result)
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: sec
    type(DC_SCALED_SEC):: sclsec2
  continue
    sclsec2 = sec
    result = sclsec == sclsec2
  end function dcscaledsec_eq_sr

  !-------------------------------------------------------------------

  logical function dcscaledsec_eq_rs(sec, sclsec) result(result)
    implicit none
    real, intent(in):: sec
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: sclsec2
  continue
    sclsec2 = sec
    result = sclsec == sclsec2
  end function dcscaledsec_eq_rs

  !-------------------------------------------------------------------

  logical function dcscaledsec_eq_sd(sclsec, sec) result(result)
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: sec
    type(DC_SCALED_SEC):: sclsec2
  continue
    sclsec2 = sec
    result = sclsec == sclsec2
  end function dcscaledsec_eq_sd

  !-------------------------------------------------------------------

  logical function dcscaledsec_eq_ds(sec, sclsec) result(result)
    implicit none
    real(DP), intent(in):: sec
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: sclsec2
  continue
    sclsec2 = sec
    result = sclsec == sclsec2
  end function dcscaledsec_eq_ds

  !-------------------------------------------------------------------

  logical function dcscaledsec_gt_ss(sclsec1, sclsec2) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2

    integer:: i
    logical:: both_negative, flag_equal
  continue
    result = .false.
    flag_equal = .true.

    if ( sclsec1 % flag_negative .and. .not. sclsec2 % flag_negative ) then
      result = .false.
      return
    elseif ( .not. sclsec1 % flag_negative .and. sclsec2 % flag_negative ) then
      result = .true.
      return
    elseif ( sclsec1 % flag_negative .and. sclsec2 % flag_negative ) then
      both_negative = .true.
    else
      both_negative = .false.
    end if

    do i = imax, imin, -1
      if ( sclsec1 % sec_ary(i) > sclsec2 % sec_ary(i) ) then
        result = .true.
        flag_equal = .false.
        exit
      elseif ( sclsec1 % sec_ary(i) < sclsec2 % sec_ary(i) ) then
        result = .false.
        flag_equal = .false.
        exit
      end if
    end do

    if ( .not. flag_equal .and. both_negative ) result = .not. result

  end function dcscaledsec_gt_ss

  !-------------------------------------------------------------------

  logical function dcscaledsec_gt_si(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
    integer:: i, sec1, factor_abs
    logical:: both_negative
  continue
    if ( sclsec % flag_negative .and. .not. factor < 0 ) then
      result = .false.
      return
    elseif ( .not. sclsec % flag_negative .and. factor < 0 ) then
      result = .true.
      return
    elseif ( sclsec % flag_negative .and. factor < 0 ) then
      both_negative = .true.
    else
      both_negative = .false.
    end if

    factor_abs = abs(factor)

    if ( factor_abs > scale_factor_int_xx(3) ) then
      factor_scl = factor
      result = sclsec > factor_scl
      return
    else
      if ( .not. all( sclsec % sec_ary(3:imax) == (/0, 0, 0, 0, 0, 0/) ) ) then
        result = .true.
      else
        sec1 = sclsec % sec_ary(0)
        do i = 1, 2
          sec1 = sec1 + sclsec % sec_ary(i) * scale_factor_int_xx(i)
        end do
        if ( sec1 == factor_abs ) then
          result = .not. all( sclsec % sec_ary(imin:-1) == (/0, 0/) )
        else
          result = sec1 > factor_abs
        end if
      end if

      if ( both_negative ) result = .not. result
    end if

  end function dcscaledsec_gt_si

  !-------------------------------------------------------------------

  logical function dcscaledsec_gt_is(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    integer, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
    integer:: i, sec1, factor_abs
    logical:: both_negative
  continue
    if ( sclsec % flag_negative .and. .not. factor < 0 ) then
      result = .true.
      return
    elseif ( .not. sclsec % flag_negative .and. factor < 0 ) then
      result = .false.
      return
    elseif ( sclsec % flag_negative .and. factor < 0 ) then
      both_negative = .true.
    else
      both_negative = .false.
    end if

    factor_abs = abs(factor)

    if ( factor_abs > scale_factor_int_xx(3) ) then
      factor_scl = factor
      result = factor_scl > sclsec
      return
    else
      if ( .not. all( sclsec % sec_ary(3:imax) == (/0, 0, 0, 0, 0, 0/) ) ) then
        result = .false.
      else
        sec1 = sclsec % sec_ary(0)
        do i = 1, 2
          sec1 = sec1 + sclsec % sec_ary(i) * scale_factor_int_xx(i)
        end do
        if ( sec1 == factor_abs ) then
          result = .false.
        else
          result = factor_abs > sec1
        end if
      end if

      if ( both_negative ) result = .not. result
    end if
  end function dcscaledsec_gt_is

  !-------------------------------------------------------------------

  logical function dcscaledsec_lt_ss(sclsec1, sclsec2) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2
    integer:: i
    logical:: both_negative, flag_equal
  continue
    result = .false.
    flag_equal = .true.

    if ( sclsec1 % flag_negative .and. .not. sclsec2 % flag_negative ) then
      result = .true.
      return
    elseif ( .not. sclsec1 % flag_negative .and. sclsec2 % flag_negative ) then
      result = .false.
      return
    elseif ( sclsec1 % flag_negative .and. sclsec2 % flag_negative ) then
      both_negative = .true.
    else
      both_negative = .false.
    end if

    do i = imax, imin, -1
      if ( sclsec1 % sec_ary(i) > sclsec2 % sec_ary(i) ) then
        result = .false.
        flag_equal = .false.
        exit
      elseif ( sclsec1 % sec_ary(i) < sclsec2 % sec_ary(i) ) then
        result = .true.
        flag_equal = .false.
        exit
      end if
    end do

    if ( .not. flag_equal .and. both_negative ) result = .not. result

  end function dcscaledsec_lt_ss

  !-------------------------------------------------------------------

  logical function dcscaledsec_lt_si(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
    integer:: i, sec1, factor_abs
    logical:: both_negative
  continue
    if ( sclsec % flag_negative .and. .not. factor < 0 ) then
      result = .true.
      return
    elseif ( .not. sclsec % flag_negative .and. factor < 0 ) then
      result = .false.
      return
    elseif ( sclsec % flag_negative .and. factor < 0 ) then
      both_negative = .true.
    else
      both_negative = .false.
    end if

    factor_abs = abs(factor)

    if ( factor_abs > scale_factor_int_xx(3) ) then
      factor_scl = factor
      result =  sclsec < factor_scl
      return
    else
      if ( .not. all( sclsec % sec_ary(3:imax) == (/0, 0, 0, 0, 0, 0/) ) ) then
        result = .false.
      else
        sec1 = sclsec % sec_ary(0)
        do i = 1, 2
          sec1 = sec1 + sclsec % sec_ary(i) * scale_factor_int_xx(i)
        end do
        if ( sec1 == factor_abs ) then
          result = .false.
        else
          result = sec1 < factor_abs
        end if
      end if

      if ( both_negative ) result = .not. result
    end if
  end function dcscaledsec_lt_si

  !-------------------------------------------------------------------

  logical function dcscaledsec_lt_is(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    integer, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
    integer:: i, sec1, factor_abs
    logical:: both_negative
  continue
    if ( sclsec % flag_negative .and. .not. factor < 0 ) then
      result = .false.
      return
    elseif ( .not. sclsec % flag_negative .and. factor < 0 ) then
      result = .true.
      return
    elseif ( sclsec % flag_negative .and. factor < 0 ) then
      both_negative = .true.
    else
      both_negative = .false.
    end if

    factor_abs = abs(factor)

    if ( factor_abs > scale_factor_int_xx(3) ) then
      factor_scl = factor
      result = factor_scl < sclsec
      return
    else
      if ( .not. all( sclsec % sec_ary(3:imax) == (/0, 0, 0, 0, 0, 0/) ) ) then
        result = .true.
      else
        sec1 = sclsec % sec_ary(0)
        do i = 1, 2
          sec1 = sec1 + sclsec % sec_ary(i) * scale_factor_int_xx(i)
        end do
        if ( sec1 == factor_abs ) then
          result = .not. all( sclsec % sec_ary(imin:-1) == (/0, 0/) )
        else
          result = factor_abs < sec1
        end if
      end if

      if ( both_negative ) result = .not. result
    end if

  end function dcscaledsec_lt_is

  !-------------------------------------------------------------------

  logical function dcscaledsec_ge_ss(sclsec1, sclsec2) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2
  continue
    result = .not. sclsec1 < sclsec2
  end function dcscaledsec_ge_ss

  !-------------------------------------------------------------------

  logical function dcscaledsec_ge_si(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
  continue
    result = .not. sclsec < factor
  end function dcscaledsec_ge_si

  !-------------------------------------------------------------------

  logical function dcscaledsec_ge_is(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    integer, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
  continue
    result = .not. factor < sclsec
  end function dcscaledsec_ge_is

  !-------------------------------------------------------------------

  logical function dcscaledsec_le_ss(sclsec1, sclsec2) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2
  continue
    result = .not. sclsec1 > sclsec2
  end function dcscaledsec_le_ss

  !-------------------------------------------------------------------

  logical function dcscaledsec_le_si(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
  continue
    result = .not. sclsec > factor
  end function dcscaledsec_le_si

  !-------------------------------------------------------------------

  logical function dcscaledsec_le_is(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の比較
    ! 
    ! Comparison of two "DC_SCALED_SEC" variables
    !
    implicit none
    integer, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
  continue
    result = .not. factor > sclsec
  end function dcscaledsec_le_is

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_add_ss(sclsec1, sclsec2) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の加算. 
    ! 
    ! Addition of two "DC_SCALED_SEC" variables
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2

    integer:: i, move_up
    logical:: both_negative, sclsec2_flag_negative
    type(DC_SCALED_SEC):: sclsec1_opsign, sclsec2_opsign
  continue
    move_up = 0
    both_negative = .false.

    ! 負の値の処理
    ! Handle negative value
    !
    sclsec2_flag_negative = sclsec2 % flag_negative
    if ( sclsec1 % flag_negative ) then
      both_negative = .true.
      sclsec2_flag_negative = .not. sclsec2_flag_negative
    end if
    if ( sclsec2_flag_negative ) then
      sclsec1_opsign = sclsec1
      sclsec1_opsign % flag_negative = .false.
      sclsec2_opsign = sclsec2
      sclsec2_opsign % flag_negative = .false.
      result = sclsec1_opsign - sclsec2_opsign
      if ( both_negative ) then
        result % flag_negative = .not. result % flag_negative
      end if
      return
    end if

    ! 加算
    ! Addition
    !
    do i = imin, imax
      result % sec_ary(i) = sclsec1 % sec_ary(i) + sclsec2 % sec_ary(i) + move_up
      if ( .not. result % sec_ary(i) < scale_factor_int ) then
        if ( i == imax ) then
          call MessageNotify( 'E', 'dc_scaledsec#operator(*)', &
            & 'DC_SCALED_SEC must be smaller than 10^24' )
        end if
        move_up = result % sec_ary(i) / scale_factor_int
        result % sec_ary(i) = mod( result % sec_ary(i), scale_factor_int )
      else
        move_up = 0
      end if
    end do

    if ( both_negative ) then
      result % flag_negative = .true.
    else
      result % flag_negative = .false.
    end if

  end function dcscaledsec_add_ss

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_add_si(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の加算. 
    ! 
    ! Addition of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec + factor_scl
  end function dcscaledsec_add_si

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_add_is(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の加算. 
    ! 
    ! Addition of two "DC_SCALED_SEC" variables
    !
    implicit none
    integer, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = factor_scl + sclsec
  end function dcscaledsec_add_is

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_add_sr(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の加算. 
    ! 
    ! Addition of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec + factor_scl
  end function dcscaledsec_add_sr

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_add_rs(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の加算. 
    ! 
    ! Addition of two "DC_SCALED_SEC" variables
    !
    implicit none
    real, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec + factor_scl
  end function dcscaledsec_add_rs

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_add_sd(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の加算. 
    ! 
    ! Addition of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec + factor_scl
  end function dcscaledsec_add_sd

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_add_ds(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の加算. 
    ! 
    ! Addition of two "DC_SCALED_SEC" variables
    !
    implicit none
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec + factor_scl
  end function dcscaledsec_add_ds

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_s(sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の符号を逆にする. 
    ! 
    ! Inverse sign of a "DC_SCALED_SEC" variable
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
  continue
    result % flag_negative = .not. sclsec % flag_negative
    result % sec_ary = sclsec % sec_ary
  end function dcscaledsec_sub_s

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_ss(sclsec1, sclsec2) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の減算. 
    ! 
    ! Subtraction of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2

    integer:: i, move_down
    logical:: both_negative, sclsec2_flag_negative
    type(DC_SCALED_SEC):: sclsec1_opsign, sclsec2_opsign
    type(DC_SCALED_SEC):: sclsec1_nosign, sclsec2_nosign
    type(DC_SCALED_SEC):: large, small
  continue
    both_negative = .false.

    ! 負の値の処理
    ! Handle negative value
    !
    sclsec2_flag_negative = sclsec2 % flag_negative
    if ( sclsec1 % flag_negative ) then
      both_negative = .true.
      sclsec2_flag_negative = .not. sclsec2_flag_negative
    end if
    if ( sclsec2_flag_negative ) then
      sclsec1_opsign = sclsec1
      sclsec1_opsign % flag_negative = .false.
      sclsec2_opsign = sclsec2
      sclsec2_opsign % flag_negative = .false.

      result = sclsec1_opsign + sclsec2_opsign
      if ( both_negative ) then
        result % flag_negative = .not. result % flag_negative
      end if
      return
    end if

    ! 絶対値の比較
    ! Compare absolute values
    !
    sclsec1_nosign = sclsec1
    sclsec1_nosign % flag_negative = .false.
    sclsec2_nosign = sclsec2
    sclsec2_nosign % flag_negative = .false.

    if ( sclsec1_nosign > sclsec2_nosign ) then
      result % flag_negative = .false.
      large = sclsec1_nosign
      small = sclsec2_nosign
    elseif ( sclsec1_nosign < sclsec2_nosign ) then
      result % flag_negative = .true.
      large = sclsec2_nosign
      small = sclsec1_nosign
    else
      result = 0
      return
    end if

    move_down = 0
    do i = imin, imax
      result % sec_ary(i) = large % sec_ary(i) - small % sec_ary(i) + move_down
      if ( result % sec_ary(i) < 0 ) then
        move_down = ( result % sec_ary(i) / scale_factor_int ) - 1
        result % sec_ary(i) = &
          & mod( result % sec_ary(i), scale_factor_int ) + scale_factor_int
      else
        move_down = 0
      end if
    end do

    if ( both_negative ) then
      result % flag_negative = .not. result % flag_negative
    end if

  end function dcscaledsec_sub_ss

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_si(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の減算. 
    ! 
    ! Subtraction of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec - factor_scl
  end function dcscaledsec_sub_si

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_is(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の減算. 
    ! 
    ! Subtraction of two "DC_SCALED_SEC" variables
    !
    implicit none
    integer, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = factor_scl - sclsec
  end function dcscaledsec_sub_is

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_sr(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の減算. 
    ! 
    ! Subtraction of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec - factor_scl
  end function dcscaledsec_sub_sr

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_rs(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の減算. 
    ! 
    ! Subtraction of two "DC_SCALED_SEC" variables
    !
    implicit none
    real, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = factor_scl - sclsec
  end function dcscaledsec_sub_rs

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_sd(sclsec, factor) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の減算. 
    ! 
    ! Subtraction of two "DC_SCALED_SEC" variables
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec - factor_scl
  end function dcscaledsec_sub_sd

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sub_ds(factor, sclsec) result(result)
    !
    ! 2 つの DC_SCALED_SEC 型変数の減算. 
    ! 
    ! Subtraction of two "DC_SCALED_SEC" variables
    !
    implicit none
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = factor_scl - sclsec
  end function dcscaledsec_sub_ds

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mul_ss(sclsec1, sclsec2) result(result)
    !
    ! DC_SCALED_SEC 型変数の乗算. 
    ! 
    ! Multiplication of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in), target:: sclsec1, sclsec2
    integer:: sec_ary_int(imin:imax,imin:imax)
!    real(DP):: sec_ary_int(imin:imax,imin:imax)
    integer:: i, j, move_up
    type(DC_SCALED_SEC):: zero_sec
  continue
    if ( sclsec1 == zero_sec .or. sclsec2 == zero_sec ) then
      result = zero_sec
      return
    end if

    if ( sclsec1 % flag_negative ) then
      result % flag_negative = .not. sclsec2 % flag_negative
    else
      result % flag_negative = sclsec2 % flag_negative
    end if

    move_up = 0
    sec_ary_int(:,:) = 0
    do i = imin, imax
      do j = imin, imax
        sec_ary_int(i,j) = &
          & sclsec1 % sec_ary(j) * sclsec2 % sec_ary(i) + move_up
        if ( i + j > imax .and. sec_ary_int(i,j) /= 0 ) then
          call MessageNotify( 'E', 'dc_scaledsec#operator(*)', &
            & 'DC_SCALED_SEC must be smaller than 10^24' )
        end if
        if ( .not. sec_ary_int(i,j) < scale_factor ) then
          move_up = int( sec_ary_int(i,j) / scale_factor_int )
          sec_ary_int(i,j) = sec_ary_int(i,j) - move_up * scale_factor_int
        else
          move_up = 0
        end if
      end do
    end do

    result % sec_ary = 0 
    do i = imin, imax
      do j = imin, imax
        if ( i + j < imin ) cycle
        if ( i + j > imax ) cycle
        result % sec_ary(i+j) = result % sec_ary(i+j) + sec_ary_int(i,j)
      end do
    end do

    move_up = 0
    do i = imin, imax
      result % sec_ary(i) = result % sec_ary(i) + move_up
      move_up = 0
      do while ( .not. result % sec_ary(i) < scale_factor_int )
        if ( i == imax ) then
          call MessageNotify( 'E', 'dc_scaledsec#operator(*)', &
            & 'DC_SCALED_SEC must be smaller than 10^24' )
        end if
        result % sec_ary(i) = result % sec_ary(i) - scale_factor_int
        move_up = move_up + 1
      end do
    end do

  end function dcscaledsec_mul_ss

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mul_si(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数の乗算. 
    ! 
    ! Multiplication of a "DC_SCALED_SEC" variable
    !
    !--
    ! 高速化のため, mul_ss を使用しない. 
    !++
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    integer:: factor_abs
    type(DC_SCALED_SEC):: zero_sec
    real(DP):: sec_ary_dp(imin:imax)
    integer:: i, move_up
  continue
    if ( sclsec == zero_sec .or. factor == 0 ) then
      result = zero_sec
      return
    end if

    if ( sclsec % flag_negative ) then
      result % flag_negative = .not. factor < 0
    else
      result % flag_negative = factor < 0
    end if
    factor_abs = abs(factor)

    move_up = 0
    sec_ary_dp(:) = 0.0_DP
    do i = imin, imax
      sec_ary_dp(i) = sclsec % sec_ary(i) * factor_abs + move_up

      if ( .not. sec_ary_dp(i) < scale_factor ) then
        move_up = int( sec_ary_dp(i) / scale_factor )
        sec_ary_dp(i) = sec_ary_dp(i) - move_up * scale_factor
      else
        move_up = 0
      end if
    end do

    if ( move_up /= 0 ) then
      call MessageNotify( 'E', 'dc_scaledsec#operator(*)', &
        & 'DC_SCALED_SEC must be smaller than 10^24' )
    end if

    result % sec_ary(imin:imax) = sec_ary_dp(imin:imax)

  end function dcscaledsec_mul_si

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mul_is(factor, sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の乗算. 
    ! 
    ! Multiplication of a "DC_SCALED_SEC" variable
    !
    implicit none
    integer, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
  continue
    result = sclsec * factor
  end function dcscaledsec_mul_is

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mul_sd(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数の乗算. 
    ! 
    ! Multiplication of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec * factor_scl
  end function dcscaledsec_mul_sd

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mul_ds(factor, sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の乗算. 
    ! 
    ! Multiplication of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
  continue
    result = sclsec * factor
  end function dcscaledsec_mul_ds

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mul_sr(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数の乗算. 
    ! 
    ! Multiplication of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl
  continue
    factor_scl = factor
    result = sclsec * factor_scl
  end function dcscaledsec_mul_sr

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mul_rs(factor, sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の乗算. 
    ! 
    ! Multiplication of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    real, intent(in):: factor
    type(DC_SCALED_SEC), intent(in):: sclsec
  continue
    result = sclsec * factor
  end function dcscaledsec_mul_rs

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_div_ss(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数の除算. 
    ! 
    ! Division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec, factor
    real(DP):: factor_abs
  continue

    ! frt, ifort などでは, 1.0e+23 などの実数とすると, 
    ! 9.9999e+22 などとなってしまうため, 
    ! factor として指定するものは 10e+12 までとする. (うーむ, 汚い対応だな....)
    ! (morikawa 2008/09/01) 
    !
    if ( .not. all( factor % sec_ary (imax-4:imax) == (/ 0, 0, 0, 0, 0 /) ) ) then
      call MessageNotify( 'E', 'dc_scaledsec#mod', &
        & 'factor must be smaller than 10^12' )
    end if

    factor_abs = factor
    result = sclsec / factor_abs

  end function dcscaledsec_div_ss

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_div_si(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数の除算. 
    ! 
    ! Division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
  continue
    result = sclsec / real( factor, DP )
  end function dcscaledsec_div_si

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_div_sd(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数の除算. 
    ! 
    ! Division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: factor
    integer:: i
    real(DP):: factor_abs, move_down, sec_ary_mod(imin+imin:imax)
    !logical:: flag_approximate
  continue
    if ( sclsec % flag_negative ) then
      result % flag_negative = .not. factor < 0.0_DP
    else
      result % flag_negative = factor < 0.0_DP
    end if
    factor_abs = abs(factor) * scale_factor_xx(2)

!    flag_approximate = .false.
    move_down = 0.0_DP
    do i = imax, imin + imin, -1
      if ( i > imax + imin ) then
        sec_ary_mod(i) = sclsec % sec_ary(i)
      elseif ( i > imin - 1 ) then
        result % sec_ary(i-imin) = int( ( sclsec % sec_ary(i) + move_down ) / factor_abs )
        sec_ary_mod(i) = &
          & mod( ( sclsec % sec_ary(i) + move_down ), factor_abs )
      else
        result % sec_ary(i-imin) = int( move_down / factor_abs )
        sec_ary_mod(i) = mod( move_down, factor_abs )
      end if

      if ( sec_ary_mod(i) /= 0.0_DP ) then
        !if ( i < imin ) flag_approximate = .true.
        move_down = sec_ary_mod(i) * scale_factor
      else
        move_down = 0.0_DP
      end if
    end do

!!$    if ( flag_approximate ) then
!!$      call MessageNotify( 'W', 'dc_scaledsec#operator(/)', &
!!$        & 'result may be calculated approximately' )
!!$    end if

  end function dcscaledsec_div_sd

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_div_sr(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数の除算. 
    ! 
    ! Division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: factor
  continue
    result = sclsec / real( factor, DP )
  end function dcscaledsec_div_sr

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mod_ss(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の余りを計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec, factor

    type(DC_SCALED_SEC):: factor_scl
    real(DP):: sec_ary_mod(imin+imin:imax)
    integer:: i, move_down_index
    real(DP):: move_down
    real(DP):: factor_dp
    type(DC_SCALED_SEC):: zero_sec
  continue

    ! frt, ifort などでは, 1.0e+23 などの実数とすると, 
    ! 9.9999e+22 などとなってしまうため, 
    ! factor として指定するものは 10e+12 までとする. (うーむ, 汚い対応だな....)
    ! (morikawa 2008/09/01) 
    !
    if ( .not. all( factor % sec_ary (imax-4:imax) == (/ 0, 0, 0, 0, 0 /) ) ) then
      call MessageNotify( 'E', 'dc_scaledsec#mod', &
        & 'factor must be smaller than 10^12' )
    end if

    if ( sclsec == factor ) then
      result = zero_sec
      return
    end if

    factor_scl % sec_ary(imin:-1) = 0
    factor_scl % sec_ary(imin-imin:imax) = factor % sec_ary(imin:imax+imin)
    factor_scl % flag_negative = factor % flag_negative

    factor_dp = factor_scl

    move_down = 0.0_DP
    do i = imax, imin + imin, -1
      move_down_index = i
      if ( move_down /= 0.0_DP ) then
        if ( abs(factor_dp) > ( move_down + scale_factor ) * scale_factor_xx( i - imin ) ) exit
      end if

      if ( i > imin - 1 ) then
        sec_ary_mod(i) = &
          & mod( ( sclsec % sec_ary(i) + move_down ), factor_dp )
      else
        sec_ary_mod(i) = mod( move_down, factor_dp )
      end if

      if ( sec_ary_mod(i) /= 0.0_DP ) then
        move_down = sec_ary_mod(i) * scale_factor
      else
        move_down = 0.0_DP
      end if

    end do

    result = move_down * scale_factor_xx(move_down_index)
    if ( move_down_index > imin - 1 ) then
      result % sec_ary(imin:move_down_index) = sclsec % sec_ary(imin:move_down_index)
    end if

    result % flag_negative = sclsec % flag_negative

  end function dcscaledsec_mod_ss

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mod_si(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の余りを計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl

  continue
    factor_scl = factor
    result = mod( sclsec, factor_scl )
  end function dcscaledsec_mod_si

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mod_sr(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の余りを計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl

  continue
    factor_scl = factor
    result = mod( sclsec, factor_scl )
  end function dcscaledsec_mod_sr

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_mod_sd(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の余りを計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl

  continue
    factor_scl = factor
    result = mod( sclsec, factor_scl )
  end function dcscaledsec_mod_sd

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_modulo_ss(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の剰余を計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec, factor

    type(DC_SCALED_SEC):: factor_scl
    real(DP):: sec_ary_mod(imin+imin:imax)
    integer:: i, move_down_index
    real(DP):: move_down
    real(DP):: factor_dp
    type(DC_SCALED_SEC):: zero_sec
  continue
  
    ! frt, ifort などでは, 1.0e+23 などの実数とすると, 
    ! 9.9999e+22 などとなってしまうため, 
    ! factor として指定するものは 10e+12 までとする. (うーむ, 汚い対応だな....)
    ! (morikawa 2008/09/01) 
    !
    if ( .not. all( factor % sec_ary (imax-4:imax) == (/ 0, 0, 0, 0, 0 /) ) ) then
      call MessageNotify( 'E', 'dc_scaledsec#modulo', &
        & 'factor must be smaller than 10^12' )
    end if

    if ( sclsec == factor ) then
      result = zero_sec
      return
    end if

    factor_scl % sec_ary(imin:-1) = 0
    factor_scl % sec_ary(imin-imin:imax) = factor % sec_ary(imin:imax+imin)
    factor_scl % flag_negative = factor % flag_negative

    factor_dp = factor_scl

    move_down = 0.0_DP
    do i = imax, imin + imin, -1
      move_down_index = i
      if ( move_down /= 0.0_DP ) then
        if ( abs(factor_dp) > ( move_down + scale_factor ) * scale_factor_xx( i - imin ) ) exit
      end if

      if ( i > imin - 1 ) then
        sec_ary_mod(i) = &
          & mod( ( sclsec % sec_ary(i) + move_down ), factor_dp )
      else
        sec_ary_mod(i) = mod( move_down, factor_dp )
      end if

      if ( sec_ary_mod(i) /= 0.0_DP ) then
        move_down = sec_ary_mod(i) * scale_factor
      else
        move_down = 0.0_DP
      end if

    end do

    result = move_down * scale_factor_xx(move_down_index)
    if ( move_down_index > imin - 1 ) then
      result % sec_ary(imin:move_down_index) = sclsec % sec_ary(imin:move_down_index)
    end if
    
    result % flag_negative = .false.

    if ( .not. result == zero_sec ) then
      if ( .not. sclsec % flag_negative .and. factor % flag_negative ) then
        result = - factor - result
        result % flag_negative = .not. sclsec % flag_negative
        
      elseif ( sclsec % flag_negative .and. .not. factor % flag_negative ) then
        result = factor - result
        result % flag_negative = .not. sclsec % flag_negative

      else
        result % flag_negative = sclsec % flag_negative
        
      end if
    end if

  end function dcscaledsec_modulo_ss

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_modulo_si(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の剰余を計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl

  continue
    factor_scl = factor
    result = modulo( sclsec, factor_scl )
  end function dcscaledsec_modulo_si

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_modulo_sr(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の剰余を計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl

  continue
    factor_scl = factor
    result = modulo( sclsec, factor_scl )
  end function dcscaledsec_modulo_sr

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_modulo_sd(sclsec, factor) result(result)
    !
    ! DC_SCALED_SEC 型変数を割った際の剰余を計算. 
    ! 
    ! Calculate of remainder by division of a "DC_SCALED_SEC" variable
    !
    use dc_message, only: MessageNotify
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC):: factor_scl

  continue
    factor_scl = factor
    result = modulo( sclsec, factor_scl )
  end function dcscaledsec_modulo_sd

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_abs_s(sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の絶対値を返す. 
    ! 
    ! Return an absolute value of a "DC_SCALED_SEC" variable
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec

  continue
    result = sclsec
    if ( result % flag_negative ) result % flag_negative = .false.
  end function dcscaledsec_abs_s

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_int_s(sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の整数値 (小数点以下切捨て) を返す. 
    ! 
    ! Return an integer value (fractional parts are truncated) of a "DC_SCALED_SEC" variable
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer:: i
  continue
    result = sclsec
    do i = -1, imin, -1
      result % sec_ary(i) = 0
    end do
  end function dcscaledsec_int_s

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sign_ss(sclsec1, sclsec2) result(result)
    !
    ! sclsec1 の絶対値に sclsec2 の符号をつけたものを返す. 
    ! 
    ! Return an absolute value of "sclsec1" with sign of "sclsec2".
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec1, sclsec2
  continue
    result = sclsec1
    result % flag_negative = sclsec2 % flag_negative
  end function dcscaledsec_sign_ss

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sign_si(sclsec, factor) result(result)
    !
    ! sclsec の絶対値に factor の符号をつけたものを返す. 
    ! 
    ! Return an absolute value of "sclsec" with sign of "factor".
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer, intent(in):: factor
    type(DC_SCALED_SEC):: sclsec_work
  continue
    sclsec_work = factor
    result = sign( sclsec, sclsec_work )
  end function dcscaledsec_sign_si

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sign_sr(sclsec, factor) result(result)
    !
    ! sclsec の絶対値に factor の符号をつけたものを返す. 
    ! 
    ! Return an absolute value of "sclsec" with sign of "factor".
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real, intent(in):: factor
    type(DC_SCALED_SEC):: sclsec_work
  continue
    sclsec_work = factor
    result = sign( sclsec, sclsec_work )
  end function dcscaledsec_sign_sr

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_sign_sd(sclsec, factor) result(result)
    !
    ! sclsec の絶対値に factor の符号をつけたものを返す. 
    ! 
    ! Return an absolute value of "sclsec" with sign of "factor".
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    real(DP), intent(in):: factor
    type(DC_SCALED_SEC):: sclsec_work
  continue
    sclsec_work = factor
    result = sign( sclsec, sclsec_work )
  end function dcscaledsec_sign_sd

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_floor_s(sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の整数値 (対象の数値以下で最大の整数) を返す. 
    ! 
    ! Return an integer value (maximum integer under the given value) 
    ! of a "DC_SCALED_SEC" variable
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer:: i
    logical:: flag_after_decimal
  continue
    result = sclsec
    flag_after_decimal = .false.
    do i = -1, imin, -1
      if ( result % sec_ary(i) /= 0 ) flag_after_decimal = .true.
      result % sec_ary(i) = 0
    end do
    if ( flag_after_decimal .and. result % flag_negative ) then
      result = result - 1
    end if

  end function dcscaledsec_floor_s

  !-------------------------------------------------------------------

  type(DC_SCALED_SEC) function dcscaledsec_ceiling_s(sclsec) result(result)
    !
    ! DC_SCALED_SEC 型変数の整数値 (対象の数値以上で最小の整数) を返す. 
    ! 
    ! Return an integer value (minimum integer over the given value) 
    ! of a "DC_SCALED_SEC" variable
    !
    implicit none
    type(DC_SCALED_SEC), intent(in):: sclsec
    integer:: i
    logical:: flag_after_decimal
  continue
    result = sclsec
    flag_after_decimal = .false.
    do i = -1, imin, -1
      if ( result % sec_ary(i) /= 0 ) flag_after_decimal = .true.
      result % sec_ary(i) = 0
    end do
    if ( flag_after_decimal .and. .not. result % flag_negative ) then
      result = result + 1
    end if

  end function dcscaledsec_ceiling_s

  !-------------------------------------------------------------------
  !-----------------  内部サブルーチン  ------------------------------
  !-------------------------------------------------------------------

  function count_digit(sec) result(result)
    implicit none
    integer, intent(in):: sec
    integer:: result

    integer:: i
  continue

    do i = 5, 0, -1
      if ( .not. sec < 10**i ) then
        result = i+1
        return
      end if
    end do
    result = 0

  end function count_digit


end module dc_scaledsec
