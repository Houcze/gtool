!= dc_date 内で使用される内部向け定数, 変数, 手続き群
!= Internal constants, variables, procedures used in "dc_date"
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dc_date_internal.f90,v 1.1 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_date_internal
  != dc_date 内で使用される内部向け定数, 変数, 手続き群
  != Internal constants, variables, procedures used in "dc_date"

  use dc_date_types, only: DC_DATETIME, DC_DIFFTIME
  use dc_types, only: DP, STRING, TOKEN
  use dc_present, only: present_and_not_empty

  implicit none

  private
  public:: dcdate_normalize, dcdate_parse_unit, dcdate_set_day_seconds_scl
  public:: dcdate_nondimcheck

contains

  subroutine dcdate_normalize(day, sec, day_seconds, nondim_flag)
    !
    !=== 日と秒の正規化
    !
    ! このサブルーチンは内部向けなので dc_date モジュール外では
    ! 極力使用しないでください.
    !
    ! 日付 *day* と秒数 *sec* の正規化を行います. *sec* が *day_seconds*
    ! (省略される場合は dc_date_types#day_seconds) を超える場合, *day*
    ! に繰上げを行います.
    ! また, *sec* と *day* の符号が逆の場合, 同符号になるよう
    ! 設定します.
    !
    use dc_date_types, only: &
      & flag_set_day_seconds_scl, day_seconds_scl
    use dc_scaledsec, only: DC_SCALED_SEC, &
      & operator(<), operator(>), operator(<=), operator(>=), &
      & operator(+), operator(-), operator(*), operator(/), &
      & modulo, int, abs, sign
    implicit none
    type(DC_SCALED_SEC), intent(inout):: day
    type(DC_SCALED_SEC), intent(inout):: sec
    type(DC_SCALED_SEC), intent(in), optional:: day_seconds
    logical, intent(in):: nondim_flag
    type(DC_SCALED_SEC):: sgn, day_sec, zero_sec
  continue
    if ( nondim_flag ) return
    if (present(day_seconds)) then
      day_sec = day_seconds
    else
      if ( .not. flag_set_day_seconds_scl ) call dcdate_set_day_seconds_scl
      day_sec = day_seconds_scl
    end if
    if (abs(sec) >= day_sec) then
      day = day + int(sec / day_sec)
      sec = modulo(sec, day_sec)
    end if
!!    zero_sec = 0  (デフォルト値 = 0 を使用する). 
    if (      ( sec > zero_sec .and. day < zero_sec ) &
      &  .or. ( sec < zero_sec .and. day > zero_sec )   ) then
      sgn = sign(day, 1)
      day = day - sgn
      sec = sec + sgn * day_sec
    endif
  end subroutine dcdate_normalize

  subroutine dcdate_set_day_seconds_scl
    use dc_scaledsec, only: DC_SCALED_SEC, assignment(=)
    use dc_date_types, only: day_seconds,  &
      & flag_set_day_seconds_scl, day_seconds_scl
  continue
    if ( .not. flag_set_day_seconds_scl ) then
      flag_set_day_seconds_scl = .true.
      day_seconds_scl = day_seconds
    end if
  end subroutine dcdate_set_day_seconds_scl

  subroutine dcdate_nondimcheck(opr, diff1, diff2, rslt)
    !
    ! このサブルーチンは内部向けなので dc_date モジュール外では
    ! 極力使用しないでください.
    !
    ! diff1 と diff2 が両方とも有次元もしくは無次元かをチェックし, 
    ! 両方が同じであれば, その結果を rslt に適用します. 
    ! 2つの引数で片方が有次元, もう片方が無次元の場合には
    ! エラーを発生させます. 
    !
    use dc_error, only: StoreError, DC_EDIMTIME
    implicit none
    character(*), intent(in):: opr  ! 演算子の名称
    type(DC_DIFFTIME), intent(in):: diff1, diff2
    type(DC_DIFFTIME), intent(inout):: rslt
  continue
    if (      (       diff1 % nondim_flag .and. .not. diff2 % nondim_flag ) &
      &  .or. ( .not. diff1 % nondim_flag .and.       diff2 % nondim_flag ) ) then
      call StoreError(DC_EDIMTIME, opr)
    end if
    rslt % nondim_flag = diff1 % nondim_flag
  end subroutine dcdate_nondimcheck

  function ParseTimeUnits(str) result(symbol)
    !
    ! 引数 *str* に与えられた文字列を解釈し, 日時の単位を示す
    ! シンボルを返します. それぞれ以下の文字列が日時の単位として解釈されます.
    ! 大文字と小文字は区別されません.
    !
    ! 年         :: dc_date_types#UNIT_YEAR
    ! 月         :: dc_date_types#UNIT_MONTH
    ! 日         :: dc_date_types#UNIT_DAY
    ! 時         :: dc_date_types#UNIT_HOUR
    ! 分         :: dc_date_types#UNIT_MIN
    ! 秒         :: dc_date_types#UNIT_SEC
    ! 無次元時間 :: dc_date_types#UNIT_NONDIM
    !
    ! 返るシンボル (整数型) は以下の通りです. 
    !
    ! 年         :: dc_date_types#UNIT_SYMBOL_YEAR
    ! 月         :: dc_date_types#UNIT_SYMBOL_MONTH
    ! 日         :: dc_date_types#UNIT_SYMBOL_DAY
    ! 時         :: dc_date_types#UNIT_SYMBOL_HOUR
    ! 分         :: dc_date_types#UNIT_SYMBOL_MIN
    ! 秒         :: dc_date_types#UNIT_SYMBOL_SEC
    ! 無次元時間 :: dc_date_types#UNIT_SYMBOL_NONDIM
    !
    ! これらに該当しない文字列を *str* に与えた場合, 
    ! dc_date_types#UNIT_SYMBOL_ERR が返ります. 
    !
    use dc_types, only: TOKEN
    use dc_date_types, only: UNIT_YEAR, UNIT_MONTH, UNIT_DAY, &
      & UNIT_HOUR, UNIT_MIN, UNIT_SEC, UNIT_NONDIM, &
      & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
      & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC, &
      & UNIT_SYMBOL_NONDIM, UNIT_SYMBOL_ERR
    use dc_string, only: StriEq
    implicit none
    character(*), intent(in):: str
    integer:: symbol
    integer:: unit_str_size, i
    character(TOKEN):: unit
  continue
    unit = adjustl(str)
    unit_str_size = size(UNIT_NONDIM)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_NONDIM(i)))) then
        symbol = UNIT_SYMBOL_NONDIM
        return
      end if
    end do

    unit_str_size = size(UNIT_SEC)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_SEC(i)))) then
        symbol = UNIT_SYMBOL_SEC
        return
      end if
    end do

    unit_str_size = size(UNIT_MIN)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_MIN(i)))) then
        symbol = UNIT_SYMBOL_MIN
        return
      end if
    end do

    unit_str_size = size(UNIT_HOUR)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_HOUR(i)))) then
        symbol = UNIT_SYMBOL_HOUR
        return
      end if
    end do

    unit_str_size = size(UNIT_DAY)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_DAY(i)))) then
        symbol = UNIT_SYMBOL_DAY
        return
      end if
    end do

    unit_str_size = size(UNIT_MONTH)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_MONTH(i)))) then
        symbol = UNIT_SYMBOL_MONTH
        return
      end if
    end do

    unit_str_size = size(UNIT_YEAR)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_YEAR(i)))) then
        symbol = UNIT_SYMBOL_YEAR
        return
      end if
    end do

    symbol = UNIT_SYMBOL_ERR

  end function ParseTimeUnits

  character(TOKEN) function dcdate_parse_unit(str) result(unit)
    !
    ! このサブルーチンは内部向けなので dc_date モジュール外では
    ! 極力使用しないでください.
    !
    ! 引数 *str* に与えられた文字列を解釈し, 日時の単位を
    ! 返します. それぞれ以下の文字列が日時の単位として解釈されます.
    ! 大文字と小文字は区別されません.
    ! 返る文字列は以下の文字型の配列の先頭の文字列です.
    ! (例: *str* に 'hrs.' が与えられる場合, dc_date_types#UNIT_HOUR
    ! 配列の先頭の文字列 UNIT_HOUR(1) が返ります.)
    !
    ! 年         :: dc_date_types#UNIT_YEAR
    ! 月         :: dc_date_types#UNIT_MONTH
    ! 日         :: dc_date_types#UNIT_DAY
    ! 時         :: dc_date_types#UNIT_HOUR
    ! 分         :: dc_date_types#UNIT_MIN
    ! 秒         :: dc_date_types#UNIT_SEC
    ! 無次元時間 :: dc_date_types#UNIT_NONDIM
    !
    ! これらに該当しない文字列を *str* に与えた場合, 空文字が返ります.
    !
    use dc_types, only: TOKEN
    use dc_date_types, only: UNIT_YEAR, UNIT_MONTH, UNIT_DAY, &
      & UNIT_HOUR, UNIT_MIN, UNIT_SEC, UNIT_NONDIM
    use dc_string, only: StriEq
    implicit none
    character(*), intent(in):: str
    integer :: unit_str_size, i
  continue
    unit = adjustl(str)
    unit_str_size = size(UNIT_NONDIM)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_NONDIM(i)))) then
        unit = UNIT_NONDIM(1)
        return
      end if
    end do

    unit_str_size = size(UNIT_SEC)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_SEC(i)))) then
        unit = UNIT_SEC(1)
        return
      end if
    end do

    unit_str_size = size(UNIT_MIN)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_MIN(i)))) then
        unit = UNIT_MIN(1)
        return
      end if
    end do

    unit_str_size = size(UNIT_HOUR)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_HOUR(i)))) then
        unit = UNIT_HOUR(1)
        return
      end if
    end do

    unit_str_size = size(UNIT_DAY)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_DAY(i)))) then
        unit = UNIT_DAY(1)
        return
      end if
    end do

    unit_str_size = size(UNIT_MONTH)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_MONTH(i)))) then
        unit = UNIT_MONTH(1)
        return
      end if
    end do

    unit_str_size = size(UNIT_YEAR)
    do i = 1, unit_str_size
      if (StriEq(trim(unit), trim(UNIT_YEAR(i)))) then
        unit = UNIT_YEAR(1)
        return
      end if
    end do

    unit = ''

  end function dcdate_parse_unit

end module dc_date_internal
