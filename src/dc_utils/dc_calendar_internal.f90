!= dc_calendar 内で使用される内部向け定数, 変数, 手続き群
!= Internal parameters, variables, procedures used in "dc_calendar"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_calendar_internal.f90,v 1.7 2010-10-06 01:48:12 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_calendar_internal
  !
  != dc_calendar 内で使用される内部向け定数, 変数, 手続き群
  != Internal parameters, variables, procedures used in "dc_calendar"
  !

  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
  use dc_types, only: DP
  implicit none
  private
  public:: default_cal_set, dccaldate_normalize, &
    &      dccaltype_str, dccaldate_str2ustr, &
    &      dccaldate_str2usym, dccaldate_ym2d

  type(DC_CAL), save, target, public:: default_cal
                               ! デフォルトの暦. 
                               ! 
                               ! DCCal で始まる手続のうち, 
                               ! "dc_calendar_types#DC_CAL" 型の
                               ! 省略可能引数が与えられない場合にはこの暦
                               ! が設定もしくは利用される. 
                               ! 

  type(DC_CAL_DATE), save, target, public:: default_date
                               ! デフォルトの日時. 
                               ! 
                               ! DCCalDate で始まる手続のうち, 
                               ! "dc_calendar_types#DC_CAL_DATE" 型の
                               ! 省略可能引数が与えられない場合にはこの日時
                               ! が設定もしくは利用される. 
                               ! 

contains
  subroutine default_cal_set
    ! 
    ! DCCal で始まる手続が呼び出され, "dc_calendar_types#DC_CAL" 型の
    ! 引数に暦の設定が行われていない場合には, まずこの手続を呼び出して, 
    ! デフォルトの暦 "default_cal" をグレゴリオ暦として設定する. 
    ! 
    ! 既に "default_cal" に暦が設定されている場合にはこの手続
    ! 内でそれを判定して何もせずに手続を終了するため, 
    ! この手続は何度呼び出しても良く, 呼び出す側で状態の有無を
    ! 確認する必要はない. 
    ! 
    use dc_calendar_types, only: CAL_GREGORIAN
    implicit none
    type(DC_CAL), pointer:: calp =>null()
  continue
    calp => default_cal

    if ( calp % initialized ) return

    calp % cal_type = CAL_GREGORIAN
    calp % month_in_year = 12
    calp % hour_in_day   = 24
    calp % min_in_hour   = 60
    calp % sec_in_min    = 60.0_DP
    allocate( calp % day_in_month(1:12) )
    calp % day_in_month(1:12) = &
      & (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)

    calp % initialized = .true.
    nullify( calp )
  end subroutine default_cal_set


  function dccaldate_normalize( year, month, day, hour, min, sec, cal ) result(stat)
    !
    ! 暦情報 cal に従い, 日時情報の正規化を行う. 
    ! dc_calender モジュール内部で使用されることを前提とする. 
    ! 具体的には以下の場合に正規化を実施する. 
    !
    ! * sec が cal で設定される「1 分の秒数」を超えている. 
    !   -> sec を「1 分の秒数」以内に納めるよう，年月日時分に繰り上げる．
    ! * min が cal で設定される「1 時間の分数」を超えている. 
    !   -> min を「1 時間の分数」以内に納めるよう，年月日時に繰り上げる．
    ! * hour が cal で設定される「1 日の時間数」を超えている. 
    !   -> hour を「1 日の時間数」以内に納めるよう，年月日に繰り上げる．
    ! * day が cal で設定される「1 月の日数」を超えている. 
    !   -> day を「1 月の日数」以内に納めるよう，年月に繰り上げる．
    ! 
    ! * sec, min, hour, day が負の場合. 
    !   -> それぞれを正にするよう負の分を上の位に繰り上げる．
    !
    ! * day が 0 の場合. 
    !   -> 正にするよう上の位に繰り上げる．
    !
    ! 正常に正規化が行われれば stat には DC_NOERR (=0) が返るが, 
    ! 日時情報が暦と整合的でない場合にはエラーコード DC_EINCONSISTCALDATE 
    ! が返る. 整合的であるかどうかは以下で判定する. 
    ! 
    ! * 月が, 暦情報に含まれる「1年の月数」を
    !   既に超えてしまっている場合
    !
    use dc_error, only: DC_NOERR, DC_EINCONSISTCALDATE
    use dc_calendar_types, only: &
      & CAL_USER_DEFINED, &
      & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN, CAL_360DAY
    implicit none
    integer:: stat                      ! ステータス. Status.
    integer, intent(inout):: year       ! 年. Year.  
    integer, intent(inout):: month      ! 月. Month. 
    integer, intent(inout):: day        ! 日. Day. 
    integer, intent(inout):: hour       ! 時. Hour. 
    integer, intent(inout):: min        ! 分. Minute. 
    real(DP), intent(inout):: sec       ! 秒. Sec. 
    type(DC_CAL), intent(in):: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 

    ! 既定の暦での 1 ヶ月の日数
    ! Days in months of previously-defined calendars
    !
    integer:: day_in_month_jg
    integer, pointer:: day_in_month(:) =>null()

    ! 既定の暦での 1 日・時間・分の時間・分・秒数
    ! Hours, minutes, seconds in a day, a hour, a minute of previously-defined calendars
    !
    integer:: month_in_year   ! 1 日の時間数. 
                              ! Hours in a day. 
    integer:: hour_in_day     ! 1 日の時間数. 
                              ! Hours in a day. 
    integer:: min_in_hour     ! 1 時間の分数. 
                              ! Minutes in a hour. 
    real(DP):: sec_in_min     ! 1 分の秒数. 
                              ! Seconds in a minute. 

    ! 作業変数
    ! Work variables
    !
    real(DP):: wyear, wday, whour, wmin
    real(DP):: wdb, ychunk_e6, ychunk_e3, chunk_scale_e6, chunk_scale_e3
  continue
    stat = DC_NOERR

    ! 日時と暦の整合性チェック
    ! Check consistency calendar and date
    !
!!$    if ( min  > cal % min_in_hour ) stat = DC_EINCONSISTCALDATE
!!$    if ( hour > cal % hour_in_day ) stat = DC_EINCONSISTCALDATE
!!$    if ( all( day  > cal % day_in_month) ) stat = DC_EINCONSISTCALDATE
    select case( cal % cal_type )
    case( CAL_USER_DEFINED )
      if ( month > cal % month_in_year ) stat = DC_EINCONSISTCALDATE
    case default
      if ( month > 12 ) stat = DC_EINCONSISTCALDATE
    end select

    if ( stat /= DC_NOERR ) return

    ! 秒〜日の変換パラメタ他の設定
    ! Set parameter for conversion of sec -- day, etc
    ! 
    month_in_year = cal % month_in_year
    hour_in_day   = cal % hour_in_day
    min_in_hour   = cal % min_in_hour
    sec_in_min    = cal % sec_in_min
    day_in_month => cal % day_in_month

    select case( cal % cal_type )
    case( CAL_JULIAN )
      chunk_scale_e6 = 4.0e+5
      ychunk_e6 = 146100000.0_DP

      chunk_scale_e3 = 4.0e+2
      ychunk_e3 = 146100.0_DP
    case( CAL_GREGORIAN )
      chunk_scale_e6 = 4.0e+5
      ychunk_e6 = 146097000.0_DP

      chunk_scale_e3 = 4.0e+2
      ychunk_e3 = 146097.0_DP
    case default
      chunk_scale_e6 = 1.0e+6
      ychunk_e6 = chunk_scale_e6 * sum( day_in_month(:) )

      chunk_scale_e3 = 1.0e+3
      ychunk_e3 = chunk_scale_e3 * sum( day_in_month(:) )
    end select

    ! 倍精度実数に一時的に格納
    ! Store in double precision variable temporally
    !
    wyear  = real( year, DP )
    wday   = real( day, DP )
    whour  = real( hour, DP )
    wmin   = real( min, DP )


    ! 秒 -> 分の繰り上げ
    ! Moving up sec -> min
    !
    if ( .not. sec < sec_in_min ) then
      wmin = wmin + aint( sec / sec_in_min )
      sec = mod( sec, sec_in_min )
    elseif ( sec < 0.0_DP ) then
      wdb = ceiling( abs(sec) / sec_in_min )
      wmin = wmin - wdb
      sec = sec + wdb * sec_in_min
    end if

    ! 分 -> 時の繰り上げ
    ! Moving up min -> hour
    !
    if ( .not. wmin < min_in_hour ) then
      whour = whour + aint( wmin / min_in_hour )
      wmin = mod( wmin, real( min_in_hour, DP ) )
    elseif ( wmin < 0 ) then
      wdb = ceiling( abs(wmin) / real(min_in_hour) )
      whour = whour - wdb
      wmin = wmin + wdb * min_in_hour
    end if

    ! 時 -> 日の繰り上げ
    ! Moving up hour -> day
    !
    if ( .not. whour < hour_in_day ) then
      wday = wday + aint( whour / hour_in_day )
      whour = mod( whour, real( hour_in_day, DP ) )
    elseif ( whour < 0 ) then
      wdb = ceiling( abs(whour) / real(hour_in_day) )
      wday = wday - wdb
      whour = whour + wdb * hour_in_day
    end if

    ! 日が負もしくは 0 の場合，負の部分を年に繰り上げ，日を正に変換
    ! Negative or 0 part of day is moved up to year
    !
    if ( wday < 1.0_DP ) then
      select case( cal % cal_type )
      case( CAL_JULIAN )

        do while ( wday < 1.0_DP )

          if ( wday < - ychunk_e6 ) then
            wyear = wyear + chunk_scale_e6 * ( aint( wday / ychunk_e6 ) - 1.0_DP )
            wday = mod( wday, ychunk_e6 ) + ychunk_e6
          end if

          if ( wday < 1.0_DP ) then
            wyear = wyear + chunk_scale_e3 * ( aint( wday / ychunk_e3 ) - 1.0_DP )
            wday = mod( wday, ychunk_e3 ) + ychunk_e3
          end if

        end do

      case( CAL_GREGORIAN )

        do while ( wday < 1.0_DP )

          if ( wday < - ychunk_e6 ) then
            wyear = wyear + chunk_scale_e6 * ( aint( wday / ychunk_e6 ) - 1.0_DP )
            wday = mod( wday, ychunk_e6 ) + ychunk_e6
          end if

          if ( wday < 1.0_DP ) then
            wyear = wyear + chunk_scale_e3 * ( aint( wday / ychunk_e3 ) - 1.0_DP )
            wday = mod( wday, ychunk_e3 ) + ychunk_e3
          end if

        end do

      case default

        do while ( wday < 1.0_DP )

          if ( wday < - ychunk_e6 ) then
            wyear = wyear + chunk_scale_e6 * ( aint( wday / ychunk_e6 ) - 1.0_DP )
            wday = mod( wday, ychunk_e6 ) + ychunk_e6
          end if

          if ( wday < 1.0_DP ) then
            wyear = wyear + chunk_scale_e3 * ( aint( wday / ychunk_e3 ) - 1.0_DP )
            wday = mod( wday, ychunk_e3 ) + ychunk_e3
          end if

        end do

      end select

    end if

    ! 日 -> 年月の繰り上げ
    ! Moving up day -> year and month
    !
    select case( cal % cal_type )
    case( CAL_JULIAN )

      if ( wday > ychunk_e6 ) then
        wyear = wyear + chunk_scale_e6 * aint( wday / ychunk_e6 )
        wday = mod( wday, ychunk_e6 )
      end if

      if ( wday > ychunk_e3 ) then
        wyear = wyear + chunk_scale_e3 * aint( wday / ychunk_e3 )
        wday = mod( wday, ychunk_e3 )
      end if

      do
        if ( month == 2 ) then
          if ( mod( wyear, 4.0_DP ) == 0 ) then
            day_in_month_jg = 29
          else
            day_in_month_jg = 28
          end if
        else
          day_in_month_jg = day_in_month(month)
        end if

        if ( .not. wday > day_in_month_jg ) exit

        wday = wday - day_in_month_jg
        month = month + 1
        if ( month > month_in_year ) then
          month = 1
          wyear = wyear + 1
        end if
      end do

    case( CAL_GREGORIAN )

      if ( wday > ychunk_e6 ) then
        wyear = wyear + chunk_scale_e6 * aint( wday / ychunk_e6 )
        wday = mod( wday, ychunk_e6 )
      end if

      if ( wday > ychunk_e3 ) then
        wyear = wyear + chunk_scale_e3 * aint( wday / ychunk_e3 )
        wday = mod( wday, ychunk_e3 )
      end if

      do
        if ( month == 2 ) then
          if ( mod( wyear, 400.0_DP ) == 0 ) then
            day_in_month_jg = 29
          elseif ( mod( wyear, 100.0_DP ) == 0 ) then
            day_in_month_jg = 28
          elseif ( mod( wyear, 4.0_DP ) == 0 ) then
            day_in_month_jg = 29
          else
            day_in_month_jg = 28
          end if
        else
          day_in_month_jg = day_in_month(month)
        end if

        if ( .not. wday > day_in_month_jg ) exit

        wday = wday - day_in_month_jg
        month = month + 1
        if ( month > month_in_year ) then
          month = 1
          wyear = wyear + 1
        end if
      end do

    case default

      if ( wday > ychunk_e6 ) then
        wyear = wyear + chunk_scale_e6 * aint( wday / ychunk_e6 )
        wday = mod( wday, ychunk_e6 )
      end if

      if ( wday > ychunk_e3 ) then
        wyear = wyear + chunk_scale_e3 * aint( wday / ychunk_e3 )
        wday = mod( wday, ychunk_e3 )
      end if

      do while ( wday > day_in_month(month) )
        wday = wday - day_in_month(month)
        month = month + 1
        if ( month > month_in_year ) then
          month = 1
          wyear = wyear + 1
        end if
      end do

    end select

    ! 整数に戻す
    ! Return to integer
    !
    year  = wyear 
    day   = wday  
    hour  = whour 
    min   = wmin  

  end function dccaldate_normalize

  function dccaldate_ym2d( year, month, day, cal, day_of_year ) result(stat)
    !
    ! 暦情報 cal に従い, 月日をその年が始まった時からの通日に変換する. 
    ! 結果は倍精度実数として *day_of_year* に返る. 
    ! dccaldate_normalize によって正規化した後に呼び出すこと. 
    !
    use dc_error, only: DC_NOERR, DC_EINCONSISTCALDATE
    use dc_calendar_types, only: &
      & CAL_USER_DEFINED, &
      & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN, CAL_360DAY
    implicit none
    integer:: stat                      ! ステータス. Status.
    integer, intent(in):: year          ! 年. Year.  
    integer, intent(in):: month         ! 月. Month. 
    integer, intent(in):: day           ! 日. Day. 
    real(DP), intent(out):: day_of_year ! 年始からの通日. Day of year
    type(DC_CAL), intent(in):: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 

    ! 作業変数
    ! Work variables
    !
    integer:: i

  continue
    stat = DC_NOERR

    ! 日時と暦の整合性チェック
    ! Check consistency calendar and date
    !
!!$    if ( min  > cal % min_in_hour ) stat = DC_EINCONSISTCALDATE
!!$    if ( hour > cal % hour_in_day ) stat = DC_EINCONSISTCALDATE
!!$    if ( all( day  > cal % day_in_month) ) stat = DC_EINCONSISTCALDATE
    select case( cal % cal_type )
    case( CAL_USER_DEFINED )
      if ( month > cal % month_in_year ) stat = DC_EINCONSISTCALDATE
    case default
      if ( month > 12 ) stat = DC_EINCONSISTCALDATE
    end select

    if ( stat /= DC_NOERR ) return

    ! 倍精度実数に一時的に格納
    ! Store in double precision variable temporally
    !
    day_of_year  = real( day, DP )

    ! 年月 -> 日 の繰り下げ
    ! Moving doun year and month -> day
    !
    select case( cal % cal_type )
    case( CAL_JULIAN )

      do i = 1, month - 1
        if ( i == 2 ) then
          if ( mod( year, 4 ) == 0 ) then
            day_of_year = day_of_year + 29
          else
            day_of_year = day_of_year + 28
          end if
        else
          day_of_year = day_of_year + cal % day_in_month(i)
        end if
      end do

    case( CAL_GREGORIAN )

      do i = 1, month - 1
        if ( i == 2 ) then
          if ( mod( year, 400 ) == 0 ) then
            day_of_year = day_of_year + 29
          elseif ( mod( year, 100 ) == 0 ) then
            day_of_year = day_of_year + 28
          elseif ( mod( year, 4 ) == 0 ) then
            day_of_year = day_of_year + 29
          else
            day_of_year = day_of_year + 28
          end if
        else
          day_of_year = day_of_year + cal % day_in_month(i)
        end if

      end do

    case default

      do i = 1, month - 1
        day_of_year = day_of_year + cal % day_in_month(i)
      end do

    end select

  end function dccaldate_ym2d


  function dccaltype_str( cal_type ) result(str)
    !
    ! 整数型の暦タイプ *cal_type* を文字列 *str* に変換する. 
    ! 不正な *cal_type* の場合は空文字が返る. 
    ! 
    use dc_calendar_types, only: &
      & CAL_USER_DEFINED, &
      & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN, CAL_360DAY
    use dc_types, only: TOKEN
    implicit none
    character(TOKEN):: str
    integer, intent(in):: cal_type

    ! 作業変数
    ! Work variables
    !
  continue
    select case( cal_type )
    case(CAL_USER_DEFINED) ; str = 'user_defined'
    case(CAL_CYCLIC)       ; str = 'cyclic      '
    case(CAL_NOLEAP)       ; str = 'noleap      '
    case(CAL_JULIAN)       ; str = 'julian      '
    case(CAL_GREGORIAN)    ; str = 'gregorian   '
    case(CAL_360DAY)       ; str = '360day      '
    case default           ; str = '            '
    end select
  end function dccaltype_str


  function dccaldate_str2ustr(str) result(unit)
    !
    ! 引数 *str* に与えられた文字列を解釈し, 日時の単位を *unit* に返す. 
    ! それぞれ以下の文字列が日時の単位として解釈される.
    ! 大文字と小文字は区別されない. 
    ! 返る文字列は以下の文字型の配列の先頭の文字列となる. 
    ! (例: *str* に 'hrs.' が与えられる場合, dc_calendar_types#UNIT_HOUR
    ! 配列の先頭の文字列 UNIT_HOUR(1) が返る.)
    !
    ! 年         :: dc_calendar_types#UNIT_YEAR
    ! 月         :: dc_calendar_types#UNIT_MONTH
    ! 日         :: dc_calendar_types#UNIT_DAY
    ! 時         :: dc_calendar_types#UNIT_HOUR
    ! 分         :: dc_calendar_types#UNIT_MIN
    ! 秒         :: dc_calendar_types#UNIT_SEC
    !
    ! これらに該当しない文字列を *str* に与えた場合, 空文字が返る. 
    !
    use dc_types, only: TOKEN
    use dc_calendar_types, only: UNIT_YEAR, UNIT_MONTH, UNIT_DAY, &
      & UNIT_HOUR, UNIT_MIN, UNIT_SEC
    use dc_string, only: StriEq
    implicit none
    character(*), intent(in):: str
    character(TOKEN):: unit
    integer :: unit_str_size, i
  continue
    unit = adjustl(str)

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

  end function dccaldate_str2ustr

  function dccaldate_str2usym(str) result(symbol)
    !
    ! 引数 *str* に与えられた文字列を解釈し, 日時の単位を示す
    ! 整数 *symbol* を返す. それぞれ以下の文字列が日時の単位として解釈する.
    ! 大文字と小文字は区別しない.
    !
    ! 年         :: dc_calendar_types#UNIT_YEAR
    ! 月         :: dc_calendar_types#UNIT_MONTH
    ! 日         :: dc_calendar_types#UNIT_DAY
    ! 時         :: dc_calendar_types#UNIT_HOUR
    ! 分         :: dc_calendar_types#UNIT_MIN
    ! 秒         :: dc_calendar_types#UNIT_SEC
    !
    ! 返るシンボル (整数型) は以下の通り. 
    !
    ! 年         :: dc_calendar_types#UNIT_SYMBOL_YEAR
    ! 月         :: dc_calendar_types#UNIT_SYMBOL_MONTH
    ! 日         :: dc_calendar_types#UNIT_SYMBOL_DAY
    ! 時         :: dc_calendar_types#UNIT_SYMBOL_HOUR
    ! 分         :: dc_calendar_types#UNIT_SYMBOL_MIN
    ! 秒         :: dc_calendar_types#UNIT_SYMBOL_SEC
    !
    ! これらに該当しない文字列を *str* に与えた場合, 
    ! dc_calendar_types#UNIT_SYMBOL_ERR が返る. 
    !
    use dc_types, only: TOKEN
    use dc_calendar_types, only: UNIT_YEAR, UNIT_MONTH, UNIT_DAY, &
      & UNIT_HOUR, UNIT_MIN, UNIT_SEC, &
      & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
      & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC, &
      & UNIT_SYMBOL_ERR
    use dc_string, only: StriEq
    implicit none
    character(*), intent(in):: str
    integer:: symbol
    integer:: unit_str_size, i
    character(TOKEN):: unit
  continue
    unit = adjustl(str)

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

  end function dccaldate_str2usym

end module dc_calendar_internal
