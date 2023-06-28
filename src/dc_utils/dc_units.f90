!== dc_units.f90 - 単位系処理用モジュール
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: dc_units.f90,v 1.1 2009-03-20 09:09:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! This file provides dc_units
!

module dc_units !:nodoc:
  !
  !== Overview
  !
  ! このモジュールは単位系処理のためのプログラム群を提供します。
  !

  use dc_types, only: DP, TOKEN, STRING
  implicit none

  private
  public:: UNITS
  public:: clear, deallocate, add_okay
  public:: assignment(=), operator(*), operator(/), operator(+)
  private:: units_simplify

  type UNITS
    real(DP):: factor
    integer:: nelems
    character(TOKEN), pointer:: name(:)
    character(TOKEN):: offset
    real(DP), pointer:: power(:)
  end type UNITS

  interface clear
    module procedure dcunitsclear
  end interface

  interface deallocate
    module procedure dcunitsdeallocate
  end interface

  interface assignment(=)
    module procedure dcunitsbuild, dcunitstostring
  end interface

  interface operator(*)
    module procedure dcunitsmul
  end interface

  interface operator(/)
    module procedure dcunitsdiv
  end interface

  interface operator(+)
    module procedure dcunitsadd
  end interface

contains

  subroutine units_simplify(u, name, power)
    type(UNITS), intent(inout):: u
    character(*), intent(in):: name(u%nelems)
    real(DP), intent(in):: power(u%nelems)
    integer:: i, n, j, onazi
    integer:: table(u%nelems)

    if (u%nelems < 1) return
    table(:) = 0
    n = 0
    do, i = 1, u%nelems
      if (name(i) == '') cycle
      onazi = 0
      do, j = 1, i - 1
        if (name(j) == name(i)) then
          onazi = j
        endif
      enddo
      if (onazi > 0) then
        table(i) = table(onazi)
      else
        n = n + 1
        table(i) = n
      endif
    enddo
    allocate(u%name(n), u%power(n))
    u%power = 0.0_DP
    do, i = 1, u%nelems
      if (table(i) == 0) cycle
      u%name(table(i)) = name(i)
      u%power(table(i)) = u%power(table(i)) + power(i)
    enddo
    u%nelems = n
  end subroutine units_simplify

  type(UNITS) function dcUnitsMul(u1, u2) result(result)
    type(UNITS), intent(in):: u1, u2
    integer:: n
    character(TOKEN), allocatable:: name(:)
    real(DP), allocatable:: power(:)
    result%factor = u1%factor * u2%factor
    result%nelems = u1%nelems + u2%nelems
    result%offset = ""
    n = result%nelems
    if (n == 0) then
      nullify(result%name, result%power)
      return
    endif
    allocate(name(n), power(n))
    name = (/u1%name, u2%name/)
    power = (/u1%power, u2%power/)
    call units_simplify(result, name, power)
    deallocate(name, power)
  end function dcUnitsMul

  type(UNITS) function dcUnitsDiv(u1, u2) result(result)
    type(UNITS), intent(in):: u1, u2
    integer:: n, n1
    character(TOKEN), allocatable:: name(:)
    real(DP), allocatable:: power(:)
    if (abs(u2%factor) < tiny(u2%factor)) then
      result%factor = sign(u1%factor, 1.0_DP) * &
        & sign(u2%factor, 1.0_DP) * &
        & huge(1.0_DP)
    else
      result%factor = u1%factor / u2%factor
    endif
    result%nelems = u1%nelems + u2%nelems
    result%offset = ""
    n = result%nelems
    if (n == 0) then
      nullify(result%name, result%power)
      return
    endif
    allocate(name(n), power(n))
    n1 = u1%nelems
    if (n1 >= 1) then
      name(1:n1) = u1%name(1:n1)
      power(1:n1) = u1%power(1:n1)
    endif
    n1 = n1 + 1
    if (n >= n1) then
      name(n1:n) = u2%name(1:u2%nelems)
      power(n1:n) = -u2%power(1:u2%nelems)
    endif
    call units_simplify(result, name, power)
    deallocate(name, power)
  end function dcUnitsDiv

  type(UNITS) function dcUnitsAdd(u1, u2) result(result)
    type(UNITS), intent(in):: u1, u2
    type(UNITS):: x
    result%offset = u1%offset
    result%nelems = u1%nelems
    result%factor = u1%factor + u2%factor
    x = u1 / u2
    if (x%nelems == 0) then
      nullify(result%name, result%power)   
      return
    endif
    if (all(abs(x%power(1:result%nelems)) < tiny(0.0_DP))) then
      allocate(result%name(result%nelems), result%power(result%nelems))
      result%name = u1%name
      result%power = u1%power
      return
    endif
    result%factor = 0.0
    result%nelems = -1
    result%offset = "MISMATCH"
    nullify(result%name, result%power)   
  end function dcUnitsAdd

  logical function add_okay(u1, u2) result(result)
    type(UNITS), intent(in):: u1, u2
    type(UNITS):: x
    character(STRING):: debug
    call clear(x)
    x = u1 / u2
    debug = u1
    debug = u2
    debug = x
    if (x%nelems == 0) then
      result = .true.
    else if (all(abs(x%power(1:x%nelems)) < tiny(0.0_DP))) then
      result = .true.
    else
      result = .false.
    endif
    call deallocate(x)
  end function add_okay

  subroutine dcunitsclear(u)
    type(UNITS), intent(inout):: u
    nullify(u%name)
    nullify(u%power)
    u%factor = 1.0_DP
    u%offset = ""
    u%nelems = 0
  end subroutine dcunitsclear

  subroutine dcunitsdeallocate(u)
    type(UNITS), intent(inout):: u
    if (associated(u%name)) deallocate(u%name)
    if (associated(u%power)) deallocate(u%power)
    u%factor = 1.0_DP
    u%offset = ""
    u%nelems = 0
  end subroutine dcunitsdeallocate

  subroutine dcunitstostring(string, u)
    !
    ! UNITS 型変数から文字型変数を生成します。
    !
    character(*), intent(out):: string
    type(UNITS), intent(in):: u
    integer:: i, ip, npower
    character(TOKEN):: buffer
    character:: mul = '.'
    real(DP), parameter:: allowed = epsilon(1.0_DP) * 16.0

    if (u%nelems < 0) then
      string = 'error from ' // u%offset
      return 
    endif

    write(buffer, "(1pg20.12)") u%factor
    string = buffer
    if (u%nelems < 1) return

    if (abs(u%factor - 1.0) < allowed) then
      string = ""
    else if (abs(u%factor + 1.0) < allowed) then
      string = "-"
    endif

    ip = len_trim(string) + 1
    do, i = 1, u%nelems
      npower = nint(u%power(i))
      if (abs(1.0 - u%power(i)) < allowed) then
        buffer = " "
      else if (abs(npower - u%power(i)) < allowed) then
        write(buffer, "(i10)") npower
        buffer = adjustl(buffer)
      else
        write(buffer, "(1pg10.3)") u%power(i)
        buffer = adjustl(buffer)
      endif
      if (buffer == '0') cycle
      string = trim(string) // mul // trim(u%name(i)) // trim(buffer)
    enddo
    if (ip <= len(string)) string(ip:ip) = ' '
    if (string(1:1) == " ") string = adjustl(string)
    if (u%offset /= "") then
      string = trim(string) // '@' // trim(u%offset)
    endif
  end subroutine dcunitstostring

  subroutine dcunitsbuild(u, cunits)
    !
    ! 文字型変数から UNITS 型変数を生成します (constructor)。
    !
    use dcunits_com
    type(UNITS), intent(out):: u
    character(*), intent(in):: cunits

    ! 構築中の情報、乗算対象の列として保持する。
    ! これは shift オペレータ付き単位を乗算できないことを示す。
    type elem_units
      character(TOKEN):: name
      real(DP):: power, factor
    end type elem_units
    type(elem_units), target:: ustack(100)
    integer:: ui = 1

    ! 構文単位が占める乗算対象の stack における最初の添字
    type paren_t
      real(DP):: factor
      integer:: factor_exp
      logical:: factor_inv
      integer:: power_exp
      integer:: paren_exp
    end type paren_t
    type(paren_t):: pstack(50)
    integer:: pi = 1

    ! パーサの状態遷移
    integer, parameter:: Y_INIT = 1, Y_NUMBER = 2, Y_NAME = 3, &
      & Y_NX = 4, Y_NI = 5, Y_MUL = 6, Y_SHIFT = 7
    integer:: yparse_status = Y_INIT

    ! 字句
    integer:: ltype
    integer:: ivalue(5)
    real(DP):: dvalue
    character(TOKEN):: cvalue
    ! その他
    integer:: i
        
    ! --- 実行部 ---
    ! 初期化
    if (associated(u%name)) deallocate(u%name)
    if (associated(u%power)) deallocate(u%power)
    u%nelems = 0
    u%offset = ""
    u%factor = 1.0_DP
    if (cunits == "") return
    call dcunitssetline(cunits)
    call ustack_clear
    call pstack_clear
    yparse_status = Y_INIT

    do
      call dcunitsgettoken(ltype, ivalue, dvalue, cvalue)
      select case(ltype)
      case (S_INTEGER)
        select case(yparse_status)
        case (Y_INIT, Y_MUL)
          pstack(pi)%factor = pstack(pi)%factor * ivalue(1)
          yparse_status = Y_NUMBER
        case (Y_NAME, Y_NX)
          i = pstack(pi)%power_exp
          ustack(i:ui)%power = ustack(i:ui)%power * ivalue(1) 
          call power_next
          yparse_status = Y_NI
        case (Y_SHIFT)
          u%offset = cvalue
        case default
          call error
        end select
      case (S_REAL)
        select case(yparse_status)
        case (Y_INIT, Y_MUL)
          pstack(pi)%factor = pstack(pi)%factor * dvalue
          yparse_status = Y_NUMBER
        case (Y_NAME, Y_NX)
          i = pstack(pi)%power_exp
          ustack(i:ui)%power = ustack(i:ui)%power * dvalue
          call power_next
          yparse_status = Y_NI
        case (Y_SHIFT)
          u%offset = cvalue
        case default
          call error
        end select
      case (S_TEXT)
        select case(yparse_status)
        case (Y_INIT, Y_NUMBER, Y_MUL)
          ustack(ui)%name = cvalue
          yparse_status = Y_NAME
        case (Y_NAME, Y_NI)
          call ustack_grow
          call power_next
          ustack(ui)%name = cvalue
          yparse_status = Y_NAME
        case (Y_SHIFT)
          u%offset = cvalue
        case default
          call error
        end select
      case (S_EXPONENT)
        select case(yparse_status)
        case (Y_NAME)
          yparse_status = Y_NX
        case default
          call error
        end select
      case (S_MULTIPLY)
        select case(yparse_status)
        case (Y_NUMBER, Y_NAME)
          call factor_next
          yparse_status = Y_MUL
        case default
          call error
        end select
      case (S_DIVIDE)
        select case(yparse_status)
        case (Y_NUMBER, Y_NAME)
          call factor_next
          pstack(pi)%factor_inv = .TRUE.
          yparse_status = Y_MUL
        case default
          call error
        end select
      case (S_SHIFT)
        if (yparse_status == Y_NX) call cancel_exp
        call units_finalize
        yparse_status = Y_SHIFT
      case (S_OPENPAR)
        if (yparse_status == Y_NX) call cancel_exp
        call pstack_push
      case (S_CLOSEPAR)
        call units_finalize
        call pstack_pop
      case (S_EOF)
        exit
      case default
        call error
      end select
    enddo
    if (yparse_status == Y_NX) call cancel_exp
    call units_finalize

    u%nelems = ui
    u%factor = product(ustack(1:ui)%factor)
    call units_simplify(u, ustack(1:ui)%name, ustack(1:ui)%power)

  contains

    subroutine cancel_exp
      print *, "DCUnitsBuild: syntax error, operator(**) ignored"
    end subroutine cancel_exp

    subroutine error
      print *, "DCUnitsBuild: unexpected token <", &
        & trim(cvalue), "> ignored"
    end subroutine error

    subroutine power_next
      ! power_exp の終了処理
      call ustack_grow
      pstack(pi)%power_exp = ui
    end subroutine power_next

    subroutine factor_next
      ! factor_exp の終了処理
      real(DP):: factor
      i = pstack(pi)%factor_exp
      factor = product(ustack(i:ui)%factor) * pstack(pi)%factor
      if (pstack(pi)%factor_inv) then
        ustack(i:ui)%power = -ustack(i:ui)%power
        factor = 1.0_DP / factor
      endif
      ustack(i)%factor = factor
      ustack(i+1:ui)%factor = 1.0_DP
      call power_next
      pstack(pi)%factor = 1.0_DP
      pstack(pi)%factor_exp = ui
    end subroutine factor_next

    subroutine units_finalize
      call factor_next
    end subroutine units_finalize

    subroutine ustack_clear
      ui = 0
      call ustack_grow
    end subroutine ustack_clear

    subroutine ustack_grow
      if (ui >= size(ustack)) stop 'DCUnitsBuild: too many elements'
      ui = ui + 1
      ustack(ui)%name = ""
      ustack(ui)%factor = 1.0_DP
      ustack(ui)%power = 1.0_DP
    end subroutine ustack_grow

    subroutine pstack_clear
      pi = 0
      call pstack_push
    end subroutine pstack_clear

    subroutine pstack_push
      if (pi >= size(pstack)) stop 'DCUnitsBuild: too many parens'
      pi = pi + 1
      call ustack_grow
      pstack(pi)%factor_exp = ui
      pstack(pi)%factor = 1.0_DP
      pstack(pi)%factor_inv = .FALSE.
      pstack(pi)%power_exp = ui
      pstack(pi)%paren_exp = ui
    end subroutine pstack_push

    subroutine pstack_pop
      ! factor_exp の終了処理
      call power_next
      pi = pi - 1
    end subroutine pstack_pop

  end subroutine dcunitsbuild

end module dc_units
