!= 単位を表す文字列の解析
!= Parse strings that mean units
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimeparseunits.f90,v 1.1 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
  function DCDateTimeParseUnits(str) result(symbol)
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

  end function DCDateTimeParseUnits
