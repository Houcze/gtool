!== dc_date_types#DC_DATETIME, dc_date_types#DC_DIFFTIME 型変数の生成
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcdatetimecreate.f90,v 1.3 2010-04-11 14:13:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

subroutine DCDateTimeCreate1(time, &
  & year, mon, day, hour, min, sec, &
  & zone, zone_hour, zone_min, caltype, caltype_str, day_seconds, &
  & sclyear, sclmon, sclday, sclsec, err)
  !
  ! dc_date_types#DC_DATETIME 型変数の生成を行います.
  ! 引数 *year*, *mon*, *day*, *hour*, *min*, *sec* の全てを与えない場合,
  ! このサブルーチンが呼ばれた際の時刻が使用されます.
  !
  ! 引数 *caltype* には暦法を設定します.
  ! dc_date_types#CAL_CYCLIC, dc_date_types#CAL_NOLEAP, 
  ! dc_date_types#CAL_JULIAN, dc_date_types#CAL_GREGORIAN
  ! のいづれかを与えてください. 引数 *caltype* を指定しない場合, 暦法は
  ! dc_date_types#CAL_GREGORIAN に設定されます.
  ! 暦法は *caltype_str* に文字列を与えることでも指定可能です. 
  ! 上記の暦法に対応する文字列は以下の通りです. (大文字小文字は区別しません)
  !
  ! dc_date_types#CAL_CYCLIC    :: cyclic
  ! dc_date_types#CAL_NOLEAP    :: noleap
  ! dc_date_types#CAL_JULIAN    :: julian
  ! dc_date_types#CAL_GREGORIAN :: gregorian
  !
  ! 引数 *zone* には UTC からの時差を設定します.
  ! '+09:00' や '-13:00' のように時差を 6 文字で指定してください.
  ! 引数 *zone* を指定しない場合, date_and_time 組み込みサブルーチン
  ! によって得られる時差を設定します. 
  ! 時差は *zone_hour* または *zone_min* に整数型を与えることでも
  ! 指定可能です. 
  !
  ! 引数 *day_seconds* には 1 日何秒かを設定します. この引数を
  ! 指定しない場合, dc_date_types#day_seconds の値が用いられます.
  ! dc_date_types#day_seconds は SetSecOfDay で変更可能です.
  !
  ! 引数 *caltype* および, *zone* に不適切な値が与えられた場合,
  ! エラーを発生させます.
  ! 引数 *err* を与えている場合には *err* に .true. が返り,
  ! プログラムは続行します.
  !

  use dc_date_generic, only: DCDiffTimeCreate, &
    & ValidCaltype, ValidZone, toChar
  use dc_date_internal, only: dcdate_set_day_seconds_scl
  use dc_date_types, only: DC_DATETIME, DC_DIFFTIME, &
    & caltype_default => caltype, &
    & flag_set_day_seconds_scl, day_seconds_scl, &
    & CYCLIC_MDAYS, CAL_NOLEAP, CAL_JULIAN, CAL_CYCLIC, CAL_GREGORIAN, &
    & MIN_SECONDS, HOUR_SECONDS, FOUR_YEARS, YEAR_MONTHS
  use dc_error, only: StoreError, DC_EBADCALTYPE, DC_EBADTIMEZONE, DC_NOERR
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_present, only: present_select
  use dc_string, only: LChar, CPrintf
  use dc_scaledsec, only: DC_SCALED_SEC, &
    & assignment(=), DCScaledSecPutLine, &
    & operator(==), operator(>), operator(<), operator(>=), operator(<=), &
    & operator(+), operator(-), operator(*), operator(/), mod, modulo, &
    & abs, int, sign, floor, DCScaledSecPutLine
  use dc_types,      only: DP, STRING, TOKEN
  implicit none
  type(DC_DATETIME), intent(out) :: time
  integer, intent(in), optional:: year ! 年
  integer, intent(in), optional:: mon  ! 月
  integer, intent(in), optional:: day  ! 日
  integer, intent(in), optional:: hour ! 時
  integer, intent(in), optional:: min  ! 分
  real(DP),intent(in), optional:: sec  ! 秒
  character(*), intent(in), optional :: zone ! UTC からの時差
  integer, intent(in), optional :: zone_hour ! UTC からの時差 (時)
  integer, intent(in), optional :: zone_min  ! UTC からの時差 (分)
  integer, intent(in), optional:: caltype ! 暦法
  character(*), intent(in), optional:: caltype_str ! 暦法 (文字型による指定)
  real(DP),intent(in), optional:: day_seconds  ! 1 日の秒数
  type(DC_SCALED_SEC), intent(in), optional:: sclyear ! 年 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(in), optional:: sclmon  ! 月 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(in), optional:: sclday  ! 日 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(in), optional:: sclsec  ! 秒 (DC_SCALED_SEC 型)
  logical, intent(out), optional:: err

  real(DP):: gcsec
  integer :: gcday, gcmon, gcyear
  real(DP):: essec, esds
  integer :: esday
  type(DC_SCALED_SEC):: iday, imon, month, iyear, century, isec
  character(6) :: izone
  integer, parameter:: year_default = 0, mon_default = 1
  integer, parameter:: day_default = 1
  integer, parameter:: sec_default = 0
  logical :: current_time_used
  type(DC_DIFFTIME):: zonediff
  character(TOKEN):: zone_str
  integer :: stat, cause_i
  character(STRING) :: cause_c
  character(*), parameter :: subname = 'DCDateTimeCreate1'
continue
  current_time_used = .not. present(year) &
    & .and. .not. present(mon) &
    & .and. .not. present(day) &
    & .and. .not. present(hour) &
    & .and. .not. present(min) &
    & .and. .not. present(sec) &
    & .and. .not. present(sclyear) &
    & .and. .not. present(sclmon) &
    & .and. .not. present(sclday) &
    & .and. .not. present(sclsec)
  call BeginSub(subname, 'current_time_used=<%y>', l=(/current_time_used/))
  stat = DC_NOERR
  cause_i = DC_NOERR
  cause_c = ''

  if ( present(day_seconds) ) then
    time % day_seconds = day_seconds
  else
    if ( .not. flag_set_day_seconds_scl ) call dcdate_set_day_seconds_scl
    time % day_seconds = day_seconds_scl
  end if

  call get_current_time(gcyear, gcmon, gcday, gcsec, izone) ! (out)
  iyear = gcyear
  imon = gcmon
  iday = gcday
  isec = gcsec

  if (.not. current_time_used) then
    if ( present(zone_hour) .or. present(zone_min) ) then
      call DCDiffTimeCreate( zonediff, &     ! (out)
        & hour = zone_hour, min = zone_min ) ! (in)
      zone_str = toChar(zonediff)
      if ( zone_str(1:1) == '-' ) then
        izone(1:1) = '-'
      else
        izone(1:1) = '+'
      end if
      izone(2:6) = zone_str(13:17)
    end if
    if (present(zone)) then
      izone = zone
    end if
    if ( .not. ValidZone(izone)) then
      stat = DC_EBADTIMEZONE
      cause_c = izone
      if (present(err)) then
        call MessageNotify('W', subname, &
          & 'zone=<%c> is invalid.', &
          & c1=trim(izone))
      else
        goto 999
      end if
    end if

    if ( present(sclsec) ) then
      isec = sclsec
    elseif( present(sec) ) then
      isec = sec
    else
      isec = sec_default
    end if
    if (present(min)) then
      isec = isec + min * MIN_SECONDS
    end if
    if (present(hour)) then
      isec = isec + hour * HOUR_SECONDS
    end if

    if ( present(sclday) ) then
      iday = sclday
    elseif( present(day) ) then
      iday = day
    else
      iday = day_default
    end if

    if ( present(sclday) ) then
      iday = sclday
    elseif( present(day) ) then
      iday = day
    else
      iday = day_default
    end if
    iday = iday + floor(isec / time % day_seconds)

    if ( present(sclmon) ) then
      imon = sclmon
    elseif( present(mon) ) then
      imon = mon
    else
      imon = mon_default
    end if

    if ( present(sclyear) ) then
      iyear = sclyear
    elseif( present(year) ) then
      iyear = year
    else
      iyear = year_default
    end if
  end if

  time % zone = izone
  time % sec = modulo(isec, time % day_seconds)
  time % caltype = caltype_default
  if (present(caltype_str)) then
    select case( LChar(trim(caltype_str)) )
    case('cyclic')
      time % caltype = CAL_CYCLIC
    case('noleap')
      time % caltype = CAL_NOLEAP
    case('julian')
      time % caltype = CAL_JULIAN
    case('gregorian')
      time % caltype = CAL_GREGORIAN
    case('')
      time % caltype = CAL_GREGORIAN
    case default
      stat = DC_EBADCALTYPE
      cause_i = 0
      call MessageNotify('W', subname, &
        & 'caltype=<%c> is invalid calender type.', &
        & c1 = trim(caltype_str) )
      if ( .not. present(err) ) then
        goto 999
      end if
    end select
  end if

  if (present(caltype)) then
    if (ValidCaltype(caltype)) then
      time % caltype = caltype
    else
      stat = DC_EBADCALTYPE
      cause_i = caltype
      if (present(err)) then
        call MessageNotify('W', subname, &
          & 'caltype=<%d> is invalid calender type.', &
          & i=(/caltype/))
      else
        goto 999
      end if
    end if
  end if
  if (time % caltype == CAL_CYCLIC) then
    time % day = int( iday + imon * CYCLIC_MDAYS )
    goto 999
  endif
  month = modulo(imon - 3, YEAR_MONTHS) + 3
  iyear = iyear + int( (imon - month) / YEAR_MONTHS )
  iday = iday + int( (month * 306 - 914) / 10 )
  if (time % caltype == CAL_NOLEAP) then
    time % day = iday + iyear * 365 + 90
  else
    iday = iday + int( (iyear * FOUR_YEARS - modulo(iyear * FOUR_YEARS, 4)) / 4 )
    if (time % caltype == CAL_JULIAN .or. iday < 640116) then
      time % day = iday + 91
    else
      century = (iyear - modulo(iyear, 100)) / 100 + 1
      time % day = iday - int( (century * 3 - modulo(century * 3, 4)) / 4 ) + 93
    endif
  endif


999 continue
  call StoreError(stat, subname, err, cause_c, cause_i)
  esday = time % day ; essec = time % sec ; esds = time % day_seconds
  call EndSub(subname, 'time (caltype=%d, day=%d, sec=%f, zone=%c, day_seconds=%f)', &
    & i=(/time % caltype, esday/), d=(/essec, esds/), &
    & c1=trim(time % zone))

  contains
    subroutine get_current_time(jyear, jmon, jday, jsec, jzone)
      !
      ! date_and_time 組み込みサブルーチンを用いて, 現在
      ! 時刻と UTC からの時差を返します.
      !
      use dc_types, only: DP
      use dc_string, only: StoD
      implicit none
      integer, intent(out) :: jyear, jmon, jday
      real(DP), intent(out) :: jsec
      character(*), intent(out) :: jzone

      integer :: date_time_values(1:8)
      character(5)  :: zone_raw
    continue

      call date_and_time(zone=zone_raw, values=date_time_values)

      jzone = zone_raw(1:3) // ":" // zone_raw(4:5)

      jyear = date_time_values(1)
      jmon  = date_time_values(2)
      jday  = date_time_values(3)
      jsec  = real(date_time_values(5), DP) * HOUR_SECONDS &
        &     + real(date_time_values(6), DP) * MIN_SECONDS &
        &     + real(date_time_values(7), DP)

    end subroutine get_current_time

end subroutine DCDateTimeCreate1


subroutine DCDiffTimeCreate1(diff, &
  & year, mon, day, hour, min, sec, day_seconds, nondim, &
  & sclyear, sclmon, sclday, sclsec )
  !
  ! dc_date_types#DC_DIFFTIME 型変数の生成を行います.
  ! 引数 year, mon, day, hour, min, sec, nondim を与えない場合,
  ! 0 が与えられたことになります.
  !
  ! 引数 *day_seconds* には 1 日何秒かを設定します. この引数を
  ! 指定しない場合, dc_date_types#day_seconds の値が用いられます.
  ! dc_date_types#day_seconds は SetSecOfDay で変更可能です.
  !
  use dc_date_internal, only: dcdate_normalize
  use dc_date_generic, only: ValidCaltype
  use dc_date_types, only: DC_DIFFTIME, &
    & day_seconds_default => day_seconds, &
    & MIN_SECONDS, HOUR_SECONDS, YEAR_MONTHS
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub, Debug
  use dc_present, only: present_select
  use dc_scaledsec, only: DC_SCALED_SEC, &
    & assignment(=), DCScaledSecPutLine, &
    & operator(==), operator(>), operator(<), operator(>=), operator(<=), &
    & operator(+), operator(-), operator(*), operator(/), mod, modulo, &
    & abs, int, sign, floor
  use dc_string, only: CPrintf
  use dc_types,      only: DP, STRING
  implicit none
  type(DC_DIFFTIME), intent(out) :: diff
  integer,  intent(in), optional:: year ! 年. Year
  integer,  intent(in), optional:: mon  ! 月. Month
  integer,  intent(in), optional:: day  ! 日. Day
  integer,  intent(in), optional:: hour ! 時. Hour
  integer,  intent(in), optional:: min  ! 分. Minute
  real(DP), intent(in), optional:: sec  ! 秒. Second
  real(DP), intent(in), optional:: day_seconds  ! 1 日の秒数
  real(DP), intent(in), optional:: nondim  ! 無次元時間. Nondimensional time
  type(DC_SCALED_SEC), intent(in), optional:: sclyear ! 年 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(in), optional:: sclmon  ! 月 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(in), optional:: sclday  ! 日 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(in), optional:: sclsec  ! 秒 (DC_SCALED_SEC 型)

  type(DC_SCALED_SEC):: iyear, imon, iday, ihour, imin, isec
  integer, parameter:: year_default = 0, mon_default = 0
  integer, parameter:: day_default = 0, hour_default = 0, min_default = 0
  integer, parameter:: sec_default = 0
  real(DP):: essec, esds
  integer :: esmon, esday
  character(STRING):: endsub_msb
  logical:: dbg_mode
  character(*), parameter :: subname = 'DCDiffTimeCreate1'
continue
  call BeginSub(subname)

  if ( present(nondim) ) then
    diff % nondim_flag = .true.
    diff % mon = 0
    diff % day = 0
    diff % sec = nondim
    goto 999
  else
    diff % nondim_flag = .false.
  end if

  if ( present(sclyear) ) then
    iyear = sclyear
  elseif( present(year) ) then
    iyear = year
  else
    iyear = year_default
  end if

  if ( present(sclmon) ) then
    imon = sclmon
  elseif( present(mon) ) then
    imon = mon
  else
    imon = mon_default
  end if
  
  if ( present(sclday) ) then
    iday = sclday
  elseif( present(day) ) then
    iday = day
  else
    iday = day_default
  end if

  ihour = present_select(.false., hour_default, hour)
  imin  = present_select(.false., min_default, min)

  if ( present(sclsec) ) then
    isec = sclsec
  elseif( present(sec) ) then
    isec = sec
  else
    isec = sec_default
  end if

  diff % mon = iyear * YEAR_MONTHS + imon
  diff % day = iday
  diff % sec =   ihour * HOUR_SECONDS &
    &          + imin  * MIN_SECONDS &
    &          + isec

  if( present(day_seconds) ) then
    diff % day_seconds = day_seconds
  else
    diff % day_seconds = day_seconds_default
  end if

  call dcdate_normalize(diff % day, diff % sec, diff % day_seconds, diff % nondim_flag)

999 continue
  call Debug( dbg_mode )
  if ( dbg_mode ) then
    esmon = diff % mon ; esday = diff % day
    essec = diff % sec ; esds = diff % day_seconds
    endsub_msb = &
      & CPrintf( 'mon=%d, day=%d, sec=%f, day_seconds=%f, nondim_flag=%b', &
      &          i = (/ esmon, esday /), d = (/ essec, esds /), &
      &          l = (/ diff % nondim_flag /) )
  else
    endsub_msb = ''
  end if
  call EndSub(subname, 'diff (%c)', c1 = trim(endsub_msb) )
end subroutine DCDiffTimeCreate1


subroutine DCDiffTimeCreate2D(diff, value, unit, unit_symbol, err)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の生成を行います.
  ! 引数 *value* に数値を, *unit* に単位を表す文字列を, 
  ! または *unit_symbol* に単位を表すシンボルを与えてください.
  ! 
  ! unit に指定できるのは以下の文字列です. (大文字小文字は区別しません).
  !
  ! 年         :: dc_date_types#UNIT_YEAR
  ! 月         :: dc_date_types#UNIT_MONTH
  ! 日         :: dc_date_types#UNIT_DAY
  ! 時         :: dc_date_types#UNIT_HOUR
  ! 分         :: dc_date_types#UNIT_MIN
  ! 秒         :: dc_date_types#UNIT_SEC
  ! 無次元時間 :: dc_date_types#UNIT_NONDIM
  !
  ! これらに該当しない文字列を *unit* に与えた場合, エラーを発生させます.
  !
  ! unit_symbol に指定できるのは以下のシンボルです.
  !
  ! 年         :: dc_date_types#UNIT_SYMBOL_YEAR
  ! 月         :: dc_date_types#UNIT_SYMBOL_MONTH
  ! 日         :: dc_date_types#UNIT_SYMBOL_DAY
  ! 時         :: dc_date_types#UNIT_SYMBOL_HOUR
  ! 分         :: dc_date_types#UNIT_SYMBOL_MIN
  ! 秒         :: dc_date_types#UNIT_SYMBOL_SEC
  ! 無次元時間 :: dc_date_types#UNIT_SYMBOL_NONDIM
  !
  ! これらに該当しないシンボルを *unit_symbol* に与えた場合, 
  ! エラーを発生させます. 
  !
  ! 引数 *err* を与えている場合には *err* に .true. が返り,
  ! プログラムは続行します.
  !
  use dc_types, only: DP, STRING
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_EBADUNIT, DC_NOERR
  use dc_string, only: StriEq
  use dc_date_generic, only: DCDiffTimeCreate, ParseTimeUnits
  use dc_date_types, only: DC_DIFFTIME, &
    & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
    & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC, UNIT_SYMBOL_NONDIM, &
    & UNIT_SYMBOL_ERR, &
    & MIN_SECONDS, HOUR_SECONDS, day_seconds, CYCLIC_MDAYS, YEAR_MONTHS
  use dc_scaledsec, only: DC_SCALED_SEC, &
    & assignment(=), DCScaledSecPutLine, &
    & operator(==), operator(>), operator(<), operator(>=), operator(<=), &
    & operator(+), operator(-), operator(*), operator(/), mod, modulo, &
    & abs, int, sign, floor
  implicit none
  type(DC_DIFFTIME), intent(out) :: diff
  real(DP), intent(in) :: value
  character(*), intent(in) :: unit
  integer, intent(in), optional :: unit_symbol
  logical, intent(out), optional :: err

  real(DP):: essec
  integer :: esmon, esday
  integer :: stat, val_int
  type(DC_SCALED_SEC):: val_scl, val_dec
  character(STRING) :: cause_c
  integer:: symbol

  character(*), parameter :: subname = 'DCDiffTimeCreate2'
continue
  call BeginSub(subname, 'value=%f', d=(/value/))
  stat = DC_NOERR
  cause_c = ''
  symbol = UNIT_SYMBOL_ERR
  if ( present(unit_symbol) ) then
    symbol = unit_symbol
  else
    symbol = ParseTimeUnits(unit)
  end if

  if ( symbol == UNIT_SYMBOL_SEC ) then
    call DCDiffTimeCreate(diff, sec=value)
    goto 999
  elseif ( symbol == UNIT_SYMBOL_NONDIM ) then
    call DCDiffTimeCreate(diff, nondim=value)
    goto 999
  end if

  val_int = int(value)
  val_scl = int(value)
  val_dec = value - int(value)

  if ( symbol == UNIT_SYMBOL_MIN ) then
    call DCDiffTimeCreate(diff, min = val_int, sclsec = val_dec * MIN_SECONDS)
  elseif ( symbol == UNIT_SYMBOL_HOUR ) then
    call DCDiffTimeCreate(diff, hour = val_int, sclsec = val_dec * HOUR_SECONDS)
  elseif ( symbol == UNIT_SYMBOL_DAY ) then
    call DCDiffTimeCreate(diff, sclday = val_scl, sclsec = val_dec * day_seconds)
  elseif ( symbol == UNIT_SYMBOL_MONTH ) then
    call DCDiffTimeCreate(diff, sclmon = val_scl, &
      & sclsec = int(val_dec * CYCLIC_MDAYS) * day_seconds)
  elseif ( symbol == UNIT_SYMBOL_YEAR ) then
    call DCDiffTimeCreate(diff, sclyear = val_scl, &
      & sclsec = int(val_dec * CYCLIC_MDAYS * YEAR_MONTHS) * day_seconds)
  else
    stat = DC_EBADUNIT
    cause_c = unit
  end if

999 continue
  call StoreError(stat, subname, err, cause_c)
  esmon = diff % mon ; esday = diff % day ; essec = diff % sec
  call EndSub(subname, 'diff (mon=%d, day=%d, sec=%f)', &
    & i=(/esmon, esday/), d=(/essec/))
end subroutine DCDiffTimeCreate2D

subroutine DCDiffTimeCreate2R(diff, value, unit, unit_symbol, err)
  use dc_types, only: DP
  use dc_date_generic, only: DCDiffTimeCreate
  use dc_date_types, only: DC_DIFFTIME
  implicit none
  type(DC_DIFFTIME), intent(out) :: diff
  real, intent(in) :: value
  character(*), intent(in) :: unit
  integer, intent(in), optional :: unit_symbol
  logical, intent(out), optional :: err
continue
  call DCDiffTimeCreate(diff, real( value, DP ), unit, unit_symbol, err)
end subroutine DCDiffTimeCreate2R

subroutine DCDiffTimeCreate2I(diff, value, unit, unit_symbol, err)
  use dc_types, only: DP
  use dc_date_generic, only: DCDiffTimeCreate
  use dc_date_types, only: DC_DIFFTIME
  implicit none
  type(DC_DIFFTIME), intent(out) :: diff
  integer, intent(in) :: value
  character(*), intent(in) :: unit
  integer, intent(in), optional :: unit_symbol
  logical, intent(out), optional :: err
continue
  call DCDiffTimeCreate(diff, real( value, DP ), unit, unit_symbol, err)
end subroutine DCDiffTimeCreate2I

subroutine DCDateTimeCreateI(time, sec)
  !
  ! dc_date_types#DC_DATETIME 型変数の生成を行います.
  ! 引数 sec には秒数を与えてください. 年月日, 時分を使って
  ! 指定を行いたい場合は Create を利用してください.
  !
  use dc_types, only: DP
  use dc_date_types, only: DC_DATETIME
  use dc_date_generic, only: DCDateTimeCreate
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=)
  implicit none
  type(DC_DATETIME), intent(out):: time
  integer, intent(in):: sec
continue
  call DCDateTimeCreate(time, sec = real(sec, DP) )
end subroutine DCDateTimeCreateI

subroutine DCDateTimeCreateR(time, sec)
  !
  ! dc_date_types#DC_DATETIME 型変数の生成を行います.
  ! 引数 sec には秒数を与えてください. 年月日, 時分を使って
  ! 指定を行いたい場合は Create を利用してください.
  !
  use dc_types, only: DP
  use dc_date_types, only: DC_DATETIME
  use dc_date_generic, only: DCDateTimeCreate
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=)
  implicit none
  type(DC_DATETIME), intent(out):: time
  real, intent(in):: sec
continue
  call DCDateTimeCreate(time, sec = real(sec, DP) )
end subroutine DCDateTimeCreateR

subroutine DCDateTimeCreateD(time, sec)
  use dc_types, only: DP
  use dc_date_types, only: DC_DATETIME
  use dc_date_generic, only: DCDateTimeCreate
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=)
  implicit none
  type(DC_DATETIME), intent(out):: time
  real(DP), intent(in):: sec
continue
  call DCDateTimeCreate(time, sec = sec)
end subroutine DCDateTimeCreateD

subroutine DCDiffTimeCreateI(diff, sec)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の生成を行います.
  ! 引数 sec には秒数を与えてください. 年月日, 時分を使って
  ! 指定を行いたい場合は Create を利用してください.
  !
  use dc_types, only: DP
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: DCDiffTimeCreate
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=)
  implicit none
  type(DC_DIFFTIME), intent(out):: diff
  integer, intent(in):: sec
continue
  call DCDiffTimeCreate(diff, sec = real(sec, DP) )
end subroutine DCDiffTimeCreateI

subroutine DCDiffTimeCreateR(diff, sec)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の生成を行います.
  ! 引数 sec には秒数を与えてください. 年月日, 時分を使って
  ! 指定を行いたい場合は Create を利用してください.
  !
  use dc_types, only: DP
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: DCDiffTimeCreate
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=)
  implicit none
  type(DC_DIFFTIME), intent(out):: diff
  real, intent(in):: sec
continue
  call DCDiffTimeCreate(diff, sec = real(sec, DP) )
end subroutine DCDiffTimeCreateR

subroutine DCDiffTimeCreateD(diff, sec)
  use dc_types, only: DP
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: DCDiffTimeCreate
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=)
  implicit none
  type(DC_DIFFTIME), intent(out):: diff
  real(DP), intent(in):: sec
continue
  call DCDiffTimeCreate(diff, sec = sec)
end subroutine DCDiffTimeCreateD


!-----------------------------------------------
! 後方互換用
! For backward compatibility
subroutine DCDateTimeCreate1_bc(time, &
  & year, mon, day, hour, min, sec, &
  & zone, caltype, day_seconds, err)
  use dc_types, only: DP
  use dc_date_types, only: DC_DATETIME
  use dc_date_generic, only: DCDateTimeCreate
  type(DC_DATETIME), intent(out):: time
  integer, intent(in), optional:: year, mon, day, hour, min
  real(DP),intent(in), optional:: sec, day_seconds
  character(*), intent(in), optional :: zone
  integer, intent(in), optional:: caltype
  logical, intent(out), optional:: err
continue
  call DCDateTimeCreate( time, &
    & year, mon, day, hour, min, sec, &
    & zone, caltype, day_seconds = day_seconds, err = err )
end subroutine DCDateTimeCreate1_bc

subroutine DCDiffTimeCreate1_bc(diff, &
  & year, mon, day, hour, min, sec, day_seconds)
  use dc_types, only: DP
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: DCDiffTimeCreate
  type(DC_DIFFTIME), intent(out) :: diff
  integer, intent(in), optional:: year, mon, day, hour, min
  real(DP),intent(in), optional:: sec, day_seconds
continue
  call DCDiffTimeCreate( diff, &
    & year, mon, day, hour, min, sec, day_seconds )
end subroutine DCDiffTimeCreate1_bc

subroutine DCDiffTimeCreate2_bc(diff, value, unit, err)
  use dc_types, only: DP
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: DCDiffTimeCreate
  type(DC_DIFFTIME), intent(out) :: diff
  real(DP), intent(in) :: value
  character(*), intent(in) :: unit
  logical, intent(out), optional :: err
continue
  call DCDiffTimeCreate( diff, value, unit, err = err )
end subroutine DCDiffTimeCreate2_bc
