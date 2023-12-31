!--
! *** Caution!! ***
! 
! This file is generated from "dc_present.rb2f90" by Ruby 1.8.5.
! Please do not edit this file directly.
!
! [JAPANESE]
!
! ※※※ 注意!!! ※※※
!
! このファイルは "dc_present.rb2f90" から Ruby 1.8.5
! によって自動生成されたファイルです.
! このファイルを直接編集しませんようお願い致します.
!
!
!++
!== Judge optional control parameters
!
! Authors::   Takeshi HORINOUCHI, Yasuhiro MORIKAWA
! Version::   $Id: dc_present.f90,v 1.1 2009-03-20 09:09:53 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!

module dc_present
  !
  !== Judge optional control parameters
  !
  ! Fortran90/95 の optional 引数の判定用の関数群を提供しています。
  !
  ! These functions judge optional control parameters.
  !
  !

  use dc_types, only: DP, TOKEN, STRING
  use dc_trace, only: BeginSub, EndSub
  private
  public :: present_and_true
  public :: present_and_false
  public :: present_and_zero
  public :: present_and_nonzero
  public :: present_and_eq
  public :: present_and_ne
  public :: present_and_not_empty
  public :: present_select

  interface present_and_eq
    module procedure present_and_eq_integer
    module procedure present_and_eq_real
    module procedure present_and_eq_double
!!$#ifndef NO_DOUBLE
!!$     module procedure present_and_eq_double
!!$#endif
  end interface

  interface present_and_ne
    module procedure present_and_ne_integer
    module procedure present_and_ne_real
    module procedure present_and_ne_double
!!$#ifndef NO_DOUBLE
!!$     module procedure present_and_ne_double
!!$#endif
  end interface

  interface present_select
    module procedure present_select_Char
    module procedure present_select_Char_auto
    module procedure present_select_Int
    module procedure present_select_Int_auto
    module procedure present_select_Real
    module procedure present_select_Real_auto
    module procedure present_select_Double
    module procedure present_select_Double_auto
  end interface

contains

  function present_and_true(arg) result(result)
    !
    ! arg が省略されておらず、且つ <tt>.true.</tt> の場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    logical,intent(in),optional :: arg
  continue
    if(present(arg)) then
      if(arg) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_true

  function present_and_false(arg) result(result)
    !
    ! arg が省略されておらず、且つ <tt>.false.</tt> の場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    logical,intent(in),optional :: arg
  continue
    if(present(arg)) then
      if(arg) then
        result=.false.
      else
        result=.true.
      endif
    else
      result=.false.
    endif
  end function present_and_false

  function present_and_zero(arg) result(result)
    !
    ! arg が省略されておらず、且つ 0 の場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    integer,intent(in),optional :: arg
  continue
    if(present(arg)) then
      if(arg==0) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_zero

  function present_and_nonzero(arg) result(result)
    !
    ! arg が省略されておらず、且つ 0 ではない場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    integer,intent(in),optional :: arg
  continue
    if(present(arg)) then
      if(arg==0) then
        result=.false.
      else
        result=.true.
      endif
    else
      result=.false.
    endif
  end function present_and_nonzero

  function present_and_eq_integer(arg,val) result(result)
    !
    ! arg が省略されておらず、且つ val と等しい場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    integer,intent(in),optional :: arg
    integer,intent(in) :: val
  continue
    if(present(arg)) then
      if(arg==val) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_eq_integer

  function present_and_eq_real(arg,val) result(result)
    !
    ! arg が省略されておらず、且つ val と等しい場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    real,intent(in),optional :: arg
    real,intent(in) :: val
  continue
    if(present(arg)) then
      if(arg==val) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_eq_real

  function present_and_eq_double(arg,val) result(result)
    !
    ! arg が省略されておらず、且つ val と等しい場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    real(DP),intent(in),optional :: arg
    real(DP),intent(in) :: val
  continue
    if(present(arg)) then
      if(arg==val) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_eq_double

  function present_and_ne_integer(arg,val) result(result)
    !
    ! arg が省略されておらず、且つ val と等しくない場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    integer,intent(in),optional :: arg
    integer,intent(in) :: val
  continue
    if(present(arg)) then
      if(arg/=val) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_ne_integer

  function present_and_ne_real(arg,val) result(result)
    !
    ! arg が省略されておらず、且つ val と等しくない場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    real,intent(in),optional :: arg
    real,intent(in) :: val
  continue
    if(present(arg)) then
      if(arg/=val) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_ne_real

  function present_and_ne_double(arg,val) result(result)
    !
    ! arg が省略されておらず、且つ val と等しくない場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    real(DP),intent(in),optional :: arg
    real(DP),intent(in) :: val
  continue
    if(present(arg)) then
      if(arg/=val) then
        result=.true.
      else
        result=.false.
      endif
    else
      result=.false.
    endif
  end function present_and_ne_double

  function present_and_not_empty(arg) result(result)
    !
    ! arg が省略されておらず、且つ空文字ではない場合、
    ! <tt>.true.</tt> が返ります。
    !
    logical :: result
    character(len=*),intent(in),optional :: arg
  continue
    if(present(arg)) then
      if(arg=="") then
        result=.false.
      else
        result=.true.
      endif
    else
      result=.false.
    endif
  end function present_and_not_empty


  function present_select_Char(  &
    &  invalid, default,      &
    &  c0, &
    &  c1, &
    &  c2, &
    &  c3, &
    &  c4, &
    &  c5, &
    &  c6, &
    &  c7, &
    &  c8, &
    &  c9       &
    &                            ) result(result)
    !
    ! 省略可能な引数 c0 〜 c9 のうち、
    ! 省略されておらず、且つ invalid と等しくないものを 1 つ返します。
    ! 優先順位が最も高いものは c0 で、
    ! 最も低いのは c9 です。
    ! c0 〜 c9 の全てが省略されているか
    ! もしくは invalid と同様な場合は default が返ります。
    !
    implicit none
    character(*)     ,intent(in)          :: invalid
    character(*)     ,intent(in)          :: default
    character(*)     ,intent(in),optional :: c0
    character(*)     ,intent(in),optional :: c1
    character(*)     ,intent(in),optional :: c2
    character(*)     ,intent(in),optional :: c3
    character(*)     ,intent(in),optional :: c4
    character(*)     ,intent(in),optional :: c5
    character(*)     ,intent(in),optional :: c6
    character(*)     ,intent(in),optional :: c7
    character(*)     ,intent(in),optional :: c8
    character(*)     ,intent(in),optional :: c9
    character(STRING)                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = 'present_select_Char'
  continue

!!$    call BeginSub(subname, 'invalid=%c default=%c',  &
!!$      &        c1=trim(invalid), c2=trim(default)   )
    specified = .false.

    if ( present(c0) ) then
      if ( len(trim(c0)) > len(trim(invalid)) ) then
        result = c0
        specified = .true.
      else
        if ( trim(c0) /= invalid(:len(trim(c0))) ) then
          result = c0
          specified = .true.
        endif
      end if
    end if

    if ( present(c1) .and. .not. specified) then
      if ( len(trim(c1)) > len(trim(invalid)) ) then
        result = c1
        specified = .true.
      else
        if ( trim(c1) /= invalid(:len(trim(c1))) ) then
          result = c1
          specified = .true.
        endif
      end if
    end if
    if ( present(c2) .and. .not. specified) then
      if ( len(trim(c2)) > len(trim(invalid)) ) then
        result = c2
        specified = .true.
      else
        if ( trim(c2) /= invalid(:len(trim(c2))) ) then
          result = c2
          specified = .true.
        endif
      end if
    end if
    if ( present(c3) .and. .not. specified) then
      if ( len(trim(c3)) > len(trim(invalid)) ) then
        result = c3
        specified = .true.
      else
        if ( trim(c3) /= invalid(:len(trim(c3))) ) then
          result = c3
          specified = .true.
        endif
      end if
    end if
    if ( present(c4) .and. .not. specified) then
      if ( len(trim(c4)) > len(trim(invalid)) ) then
        result = c4
        specified = .true.
      else
        if ( trim(c4) /= invalid(:len(trim(c4))) ) then
          result = c4
          specified = .true.
        endif
      end if
    end if
    if ( present(c5) .and. .not. specified) then
      if ( len(trim(c5)) > len(trim(invalid)) ) then
        result = c5
        specified = .true.
      else
        if ( trim(c5) /= invalid(:len(trim(c5))) ) then
          result = c5
          specified = .true.
        endif
      end if
    end if
    if ( present(c6) .and. .not. specified) then
      if ( len(trim(c6)) > len(trim(invalid)) ) then
        result = c6
        specified = .true.
      else
        if ( trim(c6) /= invalid(:len(trim(c6))) ) then
          result = c6
          specified = .true.
        endif
      end if
    end if
    if ( present(c7) .and. .not. specified) then
      if ( len(trim(c7)) > len(trim(invalid)) ) then
        result = c7
        specified = .true.
      else
        if ( trim(c7) /= invalid(:len(trim(c7))) ) then
          result = c7
          specified = .true.
        endif
      end if
    end if
    if ( present(c8) .and. .not. specified) then
      if ( len(trim(c8)) > len(trim(invalid)) ) then
        result = c8
        specified = .true.
      else
        if ( trim(c8) /= invalid(:len(trim(c8))) ) then
          result = c8
          specified = .true.
        endif
      end if
    end if
    if ( present(c9) .and. .not. specified) then
      if ( len(trim(c9)) > len(trim(invalid)) ) then
        result = c9
        specified = .true.
      else
        if ( trim(c9) /= invalid(:len(trim(c9))) ) then
          result = c9
          specified = .true.
        endif
      end if
    end if

    if (.not. specified) then
      result = default
    end if

!!$    call EndSub(subname, "result=%c", c1=trim(result))

  end function present_select_Char


  function present_select_Int(  &
    &  invalid, default,      &
    &  d0, &
    &  d1, &
    &  d2, &
    &  d3, &
    &  d4, &
    &  d5, &
    &  d6, &
    &  d7, &
    &  d8, &
    &  d9       &
    &                            ) result(result)
    !
    ! 省略可能な引数 d0 〜 d9 のうち、
    ! 省略されておらず、且つ invalid と等しくないものを 1 つ返します。
    ! 優先順位が最も高いものは d0 で、
    ! 最も低いのは d9 です。
    ! d0 〜 d9 の全てが省略されているか
    ! もしくは invalid と同様な場合は default が返ります。
    !
    implicit none
    integer     ,intent(in)          :: invalid
    integer     ,intent(in)          :: default
    integer     ,intent(in),optional :: d0
    integer     ,intent(in),optional :: d1
    integer     ,intent(in),optional :: d2
    integer     ,intent(in),optional :: d3
    integer     ,intent(in),optional :: d4
    integer     ,intent(in),optional :: d5
    integer     ,intent(in),optional :: d6
    integer     ,intent(in),optional :: d7
    integer     ,intent(in),optional :: d8
    integer     ,intent(in),optional :: d9
    integer                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = 'present_select_Int'
  continue

!!$    call BeginSub(subname, &
!!$      &        'invalid=%d default=%d',  &
!!$      &        i=(/invalid, default/))
    specified = .false.

    if ( present(d0) ) then
      if ( d0 /= invalid ) then
        result = d0
        specified = .true.
      endif
    end if

    if ( present(d1) .and. .not. specified ) then
      if ( d1 /= invalid ) then
        result = d1
        specified = .true.
      endif
    end if
    if ( present(d2) .and. .not. specified ) then
      if ( d2 /= invalid ) then
        result = d2
        specified = .true.
      endif
    end if
    if ( present(d3) .and. .not. specified ) then
      if ( d3 /= invalid ) then
        result = d3
        specified = .true.
      endif
    end if
    if ( present(d4) .and. .not. specified ) then
      if ( d4 /= invalid ) then
        result = d4
        specified = .true.
      endif
    end if
    if ( present(d5) .and. .not. specified ) then
      if ( d5 /= invalid ) then
        result = d5
        specified = .true.
      endif
    end if
    if ( present(d6) .and. .not. specified ) then
      if ( d6 /= invalid ) then
        result = d6
        specified = .true.
      endif
    end if
    if ( present(d7) .and. .not. specified ) then
      if ( d7 /= invalid ) then
        result = d7
        specified = .true.
      endif
    end if
    if ( present(d8) .and. .not. specified ) then
      if ( d8 /= invalid ) then
        result = d8
        specified = .true.
      endif
    end if
    if ( present(d9) .and. .not. specified ) then
      if ( d9 /= invalid ) then
        result = d9
        specified = .true.
      endif
    end if

    if (.not. specified) then
      result = default
    end if

!!$    call EndSub(subname, "result=%d", &
!!$      &      i=(/result/))

  end function present_select_Int


  function present_select_Real(  &
    &  invalid, default,      &
    &  r0, &
    &  r1, &
    &  r2, &
    &  r3, &
    &  r4, &
    &  r5, &
    &  r6, &
    &  r7, &
    &  r8, &
    &  r9       &
    &                            ) result(result)
    !
    ! 省略可能な引数 r0 〜 r9 のうち、
    ! 省略されておらず、且つ invalid と等しくないものを 1 つ返します。
    ! 優先順位が最も高いものは r0 で、
    ! 最も低いのは r9 です。
    ! r0 〜 r9 の全てが省略されているか
    ! もしくは invalid と同様な場合は default が返ります。
    !
    implicit none
    real     ,intent(in)          :: invalid
    real     ,intent(in)          :: default
    real     ,intent(in),optional :: r0
    real     ,intent(in),optional :: r1
    real     ,intent(in),optional :: r2
    real     ,intent(in),optional :: r3
    real     ,intent(in),optional :: r4
    real     ,intent(in),optional :: r5
    real     ,intent(in),optional :: r6
    real     ,intent(in),optional :: r7
    real     ,intent(in),optional :: r8
    real     ,intent(in),optional :: r9
    real                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = 'present_select_Real'
  continue

!!$    call BeginSub(subname, &
!!$      &        'invalid=%r default=%r',  &
!!$      &        r=(/invalid, default/))
    specified = .false.

    if ( present(r0) ) then
      if ( r0 /= invalid ) then
        result = r0
        specified = .true.
      endif
    end if

    if ( present(r1) .and. .not. specified ) then
      if ( r1 /= invalid ) then
        result = r1
        specified = .true.
      endif
    end if
    if ( present(r2) .and. .not. specified ) then
      if ( r2 /= invalid ) then
        result = r2
        specified = .true.
      endif
    end if
    if ( present(r3) .and. .not. specified ) then
      if ( r3 /= invalid ) then
        result = r3
        specified = .true.
      endif
    end if
    if ( present(r4) .and. .not. specified ) then
      if ( r4 /= invalid ) then
        result = r4
        specified = .true.
      endif
    end if
    if ( present(r5) .and. .not. specified ) then
      if ( r5 /= invalid ) then
        result = r5
        specified = .true.
      endif
    end if
    if ( present(r6) .and. .not. specified ) then
      if ( r6 /= invalid ) then
        result = r6
        specified = .true.
      endif
    end if
    if ( present(r7) .and. .not. specified ) then
      if ( r7 /= invalid ) then
        result = r7
        specified = .true.
      endif
    end if
    if ( present(r8) .and. .not. specified ) then
      if ( r8 /= invalid ) then
        result = r8
        specified = .true.
      endif
    end if
    if ( present(r9) .and. .not. specified ) then
      if ( r9 /= invalid ) then
        result = r9
        specified = .true.
      endif
    end if

    if (.not. specified) then
      result = default
    end if

!!$    call EndSub(subname, "result=%r", &
!!$      &      r=(/result/))

  end function present_select_Real


  function present_select_Double(  &
    &  invalid, default,      &
    &  f0, &
    &  f1, &
    &  f2, &
    &  f3, &
    &  f4, &
    &  f5, &
    &  f6, &
    &  f7, &
    &  f8, &
    &  f9       &
    &                            ) result(result)
    !
    ! 省略可能な引数 f0 〜 f9 のうち、
    ! 省略されておらず、且つ invalid と等しくないものを 1 つ返します。
    ! 優先順位が最も高いものは f0 で、
    ! 最も低いのは f9 です。
    ! f0 〜 f9 の全てが省略されているか
    ! もしくは invalid と同様な場合は default が返ります。
    !
    implicit none
    real(DP)     ,intent(in)          :: invalid
    real(DP)     ,intent(in)          :: default
    real(DP)     ,intent(in),optional :: f0
    real(DP)     ,intent(in),optional :: f1
    real(DP)     ,intent(in),optional :: f2
    real(DP)     ,intent(in),optional :: f3
    real(DP)     ,intent(in),optional :: f4
    real(DP)     ,intent(in),optional :: f5
    real(DP)     ,intent(in),optional :: f6
    real(DP)     ,intent(in),optional :: f7
    real(DP)     ,intent(in),optional :: f8
    real(DP)     ,intent(in),optional :: f9
    real(DP)                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = 'present_select_Double'
  continue

!!$    call BeginSub(subname, &
!!$      &        'invalid=%f default=%f',  &
!!$      &        d=(/invalid, default/))
    specified = .false.

    if ( present(f0) ) then
      if ( f0 /= invalid ) then
        result = f0
        specified = .true.
      endif
    end if

    if ( present(f1) .and. .not. specified ) then
      if ( f1 /= invalid ) then
        result = f1
        specified = .true.
      endif
    end if
    if ( present(f2) .and. .not. specified ) then
      if ( f2 /= invalid ) then
        result = f2
        specified = .true.
      endif
    end if
    if ( present(f3) .and. .not. specified ) then
      if ( f3 /= invalid ) then
        result = f3
        specified = .true.
      endif
    end if
    if ( present(f4) .and. .not. specified ) then
      if ( f4 /= invalid ) then
        result = f4
        specified = .true.
      endif
    end if
    if ( present(f5) .and. .not. specified ) then
      if ( f5 /= invalid ) then
        result = f5
        specified = .true.
      endif
    end if
    if ( present(f6) .and. .not. specified ) then
      if ( f6 /= invalid ) then
        result = f6
        specified = .true.
      endif
    end if
    if ( present(f7) .and. .not. specified ) then
      if ( f7 /= invalid ) then
        result = f7
        specified = .true.
      endif
    end if
    if ( present(f8) .and. .not. specified ) then
      if ( f8 /= invalid ) then
        result = f8
        specified = .true.
      endif
    end if
    if ( present(f9) .and. .not. specified ) then
      if ( f9 /= invalid ) then
        result = f9
        specified = .true.
      endif
    end if

    if (.not. specified) then
      result = default
    end if

!!$    call EndSub(subname, "result=%f", &
!!$      &      d=(/result/))

  end function present_select_Double


  function present_select_Char_auto(  &
    &  invalid, default,      &
    &  c0, &
    &  c1, &
    &  c2, &
    &  c3, &
    &  c4, &
    &  c5, &
    &  c6, &
    &  c7, &
    &  c8, &
    &  c9                     &
    &                            ) result(result)
    !
    ! invalid に <tt>.false.</tt> を与えた場合、省略可能な引数
    ! c0 〜 c9 のうち、
    ! 省略されておらず且つ優先順位が最も高いものを
    ! 1 つ返します。優先順位が最も高いのは c0 で、
    ! 最も低いのは c9 です。
    !
    ! invarlid が .true. の場合は、
    ! 空文字 (空白のみの場合も空文字と扱う) は省略されている
    ! のと同様に扱われ、優先順位に関わらず無視されます。
    ! 与えられた引数の全てが空文字の場合は default が返ります。
    !
    implicit none
    logical          ,intent(in)          :: invalid
    character(*)     ,intent(in)          :: default
    character(*)     ,intent(in),optional :: c0
    character(*)     ,intent(in),optional :: c1
    character(*)     ,intent(in),optional :: c2
    character(*)     ,intent(in),optional :: c3
    character(*)     ,intent(in),optional :: c4
    character(*)     ,intent(in),optional :: c5
    character(*)     ,intent(in),optional :: c6
    character(*)     ,intent(in),optional :: c7
    character(*)     ,intent(in),optional :: c8
    character(*)     ,intent(in),optional :: c9
    character(STRING)                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = "present_select_Char_auto"
  continue

!!$    call BeginSub(subname, 'invalid=%y default=%c',  &
!!$      &        l=(/invalid/), c1=trim(default)   )
    specified = .false.

    if ( present(c0) ) then
       if ( trim(c0) /= '' ) then
          result = c0
          specified = .true.
       endif
    end if

    if ( present(c1) .and. .not. specified ) then
       if ( trim(c1) /= '' ) then
          result = c1
          specified = .true.
       endif
    end if
    if ( present(c2) .and. .not. specified ) then
       if ( trim(c2) /= '' ) then
          result = c2
          specified = .true.
       endif
    end if
    if ( present(c3) .and. .not. specified ) then
       if ( trim(c3) /= '' ) then
          result = c3
          specified = .true.
       endif
    end if
    if ( present(c4) .and. .not. specified ) then
       if ( trim(c4) /= '' ) then
          result = c4
          specified = .true.
       endif
    end if
    if ( present(c5) .and. .not. specified ) then
       if ( trim(c5) /= '' ) then
          result = c5
          specified = .true.
       endif
    end if
    if ( present(c6) .and. .not. specified ) then
       if ( trim(c6) /= '' ) then
          result = c6
          specified = .true.
       endif
    end if
    if ( present(c7) .and. .not. specified ) then
       if ( trim(c7) /= '' ) then
          result = c7
          specified = .true.
       endif
    end if
    if ( present(c8) .and. .not. specified ) then
       if ( trim(c8) /= '' ) then
          result = c8
          specified = .true.
       endif
    end if
    if ( present(c9) .and. .not. specified ) then
       if ( trim(c9) /= '' ) then
          result = c9
          specified = .true.
       endif
    end if

    if (.not. specified) then
       result = default
    end if

!!$    call EndSub(subname, "result=%c", c1=trim(result))

  end function present_select_Char_auto


  function present_select_Int_auto(  &
    &  invalid, default,      &
    &  d0, &
    &  d1, &
    &  d2, &
    &  d3, &
    &  d4, &
    &  d5, &
    &  d6, &
    &  d7, &
    &  d8, &
    &  d9                     &
    &                            ) result(result)
    !
    ! invalid に <tt>.false.</tt> を与えた場合、省略可能な引数
    ! d0 〜 d9 のうち、
    ! 省略されておらず且つ優先順位が最も高いものを
    ! 1 つ返します。優先順位が最も高いのは d0 で、
    ! 最も低いのは d9 です。
    !
    ! invarlid が .true. の場合は、
    ! 0 は省略されている
    ! のと同様に扱われ、優先順位に関わらず無視されます。
    ! 与えられた引数の全てが 0 の場合は default が返ります。
    !
    implicit none
    logical          ,intent(in)          :: invalid
    integer     ,intent(in)          :: default
    integer     ,intent(in),optional :: d0
    integer     ,intent(in),optional :: d1
    integer     ,intent(in),optional :: d2
    integer     ,intent(in),optional :: d3
    integer     ,intent(in),optional :: d4
    integer     ,intent(in),optional :: d5
    integer     ,intent(in),optional :: d6
    integer     ,intent(in),optional :: d7
    integer     ,intent(in),optional :: d8
    integer     ,intent(in),optional :: d9
    integer                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = "present_select_Int_auto"
  continue

!!$    call BeginSub(subname, &
!!$      &        'invalid=%y default=%d',  &
!!$      &        l=(/invalid/), i=(/default/))
    specified = .false.

    if ( present(d0) ) then
       if ( .not. invalid ) then
          result = d0
          specified = .true.
       elseif ( d0 /= 0 ) then
          result = d0
          specified = .true.
       endif
    end if

    if ( present(d1) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d1
          specified = .true.
       elseif ( d1 /= 0 ) then
          result = d1
          specified = .true.
       endif
    end if
    if ( present(d2) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d2
          specified = .true.
       elseif ( d2 /= 0 ) then
          result = d2
          specified = .true.
       endif
    end if
    if ( present(d3) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d3
          specified = .true.
       elseif ( d3 /= 0 ) then
          result = d3
          specified = .true.
       endif
    end if
    if ( present(d4) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d4
          specified = .true.
       elseif ( d4 /= 0 ) then
          result = d4
          specified = .true.
       endif
    end if
    if ( present(d5) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d5
          specified = .true.
       elseif ( d5 /= 0 ) then
          result = d5
          specified = .true.
       endif
    end if
    if ( present(d6) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d6
          specified = .true.
       elseif ( d6 /= 0 ) then
          result = d6
          specified = .true.
       endif
    end if
    if ( present(d7) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d7
          specified = .true.
       elseif ( d7 /= 0 ) then
          result = d7
          specified = .true.
       endif
    end if
    if ( present(d8) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d8
          specified = .true.
       elseif ( d8 /= 0 ) then
          result = d8
          specified = .true.
       endif
    end if
    if ( present(d9) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = d9
          specified = .true.
       elseif ( d9 /= 0 ) then
          result = d9
          specified = .true.
       endif
    end if

    if (.not. specified) then
       result = default
    end if

!!$    call EndSub(subname, "result=%d", &
!!$      &      i=(/result/))

  end function present_select_Int_auto


  function present_select_Real_auto(  &
    &  invalid, default,      &
    &  r0, &
    &  r1, &
    &  r2, &
    &  r3, &
    &  r4, &
    &  r5, &
    &  r6, &
    &  r7, &
    &  r8, &
    &  r9                     &
    &                            ) result(result)
    !
    ! invalid に <tt>.false.</tt> を与えた場合、省略可能な引数
    ! r0 〜 r9 のうち、
    ! 省略されておらず且つ優先順位が最も高いものを
    ! 1 つ返します。優先順位が最も高いのは r0 で、
    ! 最も低いのは r9 です。
    !
    ! invarlid が .true. の場合は、
    ! 0 は省略されている
    ! のと同様に扱われ、優先順位に関わらず無視されます。
    ! 与えられた引数の全てが 0 の場合は default が返ります。
    !
    implicit none
    logical          ,intent(in)          :: invalid
    real     ,intent(in)          :: default
    real     ,intent(in),optional :: r0
    real     ,intent(in),optional :: r1
    real     ,intent(in),optional :: r2
    real     ,intent(in),optional :: r3
    real     ,intent(in),optional :: r4
    real     ,intent(in),optional :: r5
    real     ,intent(in),optional :: r6
    real     ,intent(in),optional :: r7
    real     ,intent(in),optional :: r8
    real     ,intent(in),optional :: r9
    real                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = "present_select_Real_auto"
  continue

!!$    call BeginSub(subname, &
!!$      &        'invalid=%y default=%r',  &
!!$      &        l=(/invalid/), r=(/default/))
    specified = .false.

    if ( present(r0) ) then
       if ( .not. invalid ) then
          result = r0
          specified = .true.
       elseif ( r0 /= 0.0 ) then
          result = r0
          specified = .true.
       endif
    end if

    if ( present(r1) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r1
          specified = .true.
       elseif ( r1 /= 0.0 ) then
          result = r1
          specified = .true.
       endif
    end if
    if ( present(r2) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r2
          specified = .true.
       elseif ( r2 /= 0.0 ) then
          result = r2
          specified = .true.
       endif
    end if
    if ( present(r3) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r3
          specified = .true.
       elseif ( r3 /= 0.0 ) then
          result = r3
          specified = .true.
       endif
    end if
    if ( present(r4) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r4
          specified = .true.
       elseif ( r4 /= 0.0 ) then
          result = r4
          specified = .true.
       endif
    end if
    if ( present(r5) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r5
          specified = .true.
       elseif ( r5 /= 0.0 ) then
          result = r5
          specified = .true.
       endif
    end if
    if ( present(r6) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r6
          specified = .true.
       elseif ( r6 /= 0.0 ) then
          result = r6
          specified = .true.
       endif
    end if
    if ( present(r7) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r7
          specified = .true.
       elseif ( r7 /= 0.0 ) then
          result = r7
          specified = .true.
       endif
    end if
    if ( present(r8) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r8
          specified = .true.
       elseif ( r8 /= 0.0 ) then
          result = r8
          specified = .true.
       endif
    end if
    if ( present(r9) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = r9
          specified = .true.
       elseif ( r9 /= 0.0 ) then
          result = r9
          specified = .true.
       endif
    end if

    if (.not. specified) then
       result = default
    end if

!!$    call EndSub(subname, "result=%r", &
!!$      &      r=(/result/))

  end function present_select_Real_auto


  function present_select_Double_auto(  &
    &  invalid, default,      &
    &  f0, &
    &  f1, &
    &  f2, &
    &  f3, &
    &  f4, &
    &  f5, &
    &  f6, &
    &  f7, &
    &  f8, &
    &  f9                     &
    &                            ) result(result)
    !
    ! invalid に <tt>.false.</tt> を与えた場合、省略可能な引数
    ! f0 〜 f9 のうち、
    ! 省略されておらず且つ優先順位が最も高いものを
    ! 1 つ返します。優先順位が最も高いのは f0 で、
    ! 最も低いのは f9 です。
    !
    ! invarlid が .true. の場合は、
    ! 0 は省略されている
    ! のと同様に扱われ、優先順位に関わらず無視されます。
    ! 与えられた引数の全てが 0 の場合は default が返ります。
    !
    implicit none
    logical          ,intent(in)          :: invalid
    real(DP)     ,intent(in)          :: default
    real(DP)     ,intent(in),optional :: f0
    real(DP)     ,intent(in),optional :: f1
    real(DP)     ,intent(in),optional :: f2
    real(DP)     ,intent(in),optional :: f3
    real(DP)     ,intent(in),optional :: f4
    real(DP)     ,intent(in),optional :: f5
    real(DP)     ,intent(in),optional :: f6
    real(DP)     ,intent(in),optional :: f7
    real(DP)     ,intent(in),optional :: f8
    real(DP)     ,intent(in),optional :: f9
    real(DP)                         :: result

    !=== Variables for internal work
    logical                 :: specified
    character(*),  parameter:: subname = "present_select_Double_auto"
  continue

!!$    call BeginSub(subname, &
!!$      &        'invalid=%y default=%f',  &
!!$      &        l=(/invalid/), d=(/default/))
    specified = .false.

    if ( present(f0) ) then
       if ( .not. invalid ) then
          result = f0
          specified = .true.
       elseif ( f0 /= 0.0_DP ) then
          result = f0
          specified = .true.
       endif
    end if

    if ( present(f1) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f1
          specified = .true.
       elseif ( f1 /= 0.0_DP ) then
          result = f1
          specified = .true.
       endif
    end if
    if ( present(f2) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f2
          specified = .true.
       elseif ( f2 /= 0.0_DP ) then
          result = f2
          specified = .true.
       endif
    end if
    if ( present(f3) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f3
          specified = .true.
       elseif ( f3 /= 0.0_DP ) then
          result = f3
          specified = .true.
       endif
    end if
    if ( present(f4) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f4
          specified = .true.
       elseif ( f4 /= 0.0_DP ) then
          result = f4
          specified = .true.
       endif
    end if
    if ( present(f5) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f5
          specified = .true.
       elseif ( f5 /= 0.0_DP ) then
          result = f5
          specified = .true.
       endif
    end if
    if ( present(f6) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f6
          specified = .true.
       elseif ( f6 /= 0.0_DP ) then
          result = f6
          specified = .true.
       endif
    end if
    if ( present(f7) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f7
          specified = .true.
       elseif ( f7 /= 0.0_DP ) then
          result = f7
          specified = .true.
       endif
    end if
    if ( present(f8) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f8
          specified = .true.
       elseif ( f8 /= 0.0_DP ) then
          result = f8
          specified = .true.
       endif
    end if
    if ( present(f9) .and. .not. specified ) then
       if ( .not. invalid ) then
          result = f9
          specified = .true.
       elseif ( f9 /= 0.0_DP ) then
          result = f9
          specified = .true.
       endif
    end if

    if (.not. specified) then
       result = default
    end if

!!$    call EndSub(subname, "result=%f", &
!!$      &      d=(/result/))

  end function present_select_Double_auto

end module dc_present
!--
! vi:set readonly sw=4 ts=8:
!
!Local Variables:
!mode: f90
!buffer-read-only: t
!End:
!
!++
