!== dc_date_types#DC_DATETIME, dc_date_types#DC_DIFFTIME 型変数から月日秒への変換
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dcdatetimeeval.f90,v 1.2 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2006. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

subroutine DCDateTimeEval1(time, year, mon, day, hour, min, &
  & sec, caltype, zone, sclyear, sclmon, sclday, sclsec)
  !
  ! dc_date_types#DC_DATETIME 型変数 *time* を
  ! 年 *year*, 月 *mon*, 日 *day*, 時間 *hour*, 分 *min*, 秒 *sec*,
  ! 暦法 *caltype*, タイムゾーン *zone* に変換して返します.
  !
  use dc_types, only: DP
  use dc_date_types, only: DC_DATETIME, &
    & CYCLIC_MDAYS, CAL_NOLEAP, CAL_JULIAN, CAL_CYCLIC, &
    & YEAR_MONTHS, YEAR_DAYS, FOUR_YEARS, FOUR_CENTURY, &
    & HOUR_SECONDS, MIN_SECONDS
  use dc_scaledsec, only:  DC_SCALED_SEC, &
    & assignment(=), DCScaledSecPutLine, &
    & operator(==), operator(>), operator(<), operator(>=), operator(<=), &
    & operator(+), operator(-), operator(*), operator(/), mod, modulo, &
    & abs, int, floor, ceiling
  use dc_trace, only: BeginSub, EndSub
  implicit none
  type(DC_DATETIME), intent(in):: time
  integer, intent(out), optional:: year ! 年
  integer, intent(out), optional:: mon  ! 月
  integer, intent(out), optional:: day  ! 日
  integer, intent(out), optional:: hour ! 時
  integer, intent(out), optional:: min  ! 分
  real(DP),intent(out), optional:: sec  ! 秒
  integer, intent(out), optional:: caltype ! 暦法
  character(*), intent(out), optional:: zone ! タイムゾーン (UTC からの時差)
  type(DC_SCALED_SEC), intent(out), optional:: sclyear ! 年 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(out), optional:: sclmon  ! 月 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(out), optional:: sclday  ! 日 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(out), optional:: sclsec  ! 秒 (DC_SCALED_SEC 型)

  type(DC_SCALED_SEC):: iyear, month, iday, imon, isec
  !character(*), parameter :: subname = 'DCDateTimeEval1'
continue
  !call BeginSub(subname)
  if (present(zone)) then
    zone = time % zone
  end if
  if (present(caltype)) then
    caltype = time % caltype
  end if
  isec = time % sec
  if (present(hour)) then
    hour = floor(isec / HOUR_SECONDS)
    isec = modulo(isec, HOUR_SECONDS)
  end if
  if (present(min)) then
    min = floor(isec / MIN_SECONDS)
    isec = modulo(isec, MIN_SECONDS)
  end if
  if (present(sec)) then
    sec = isec
  end if
  if (present(sclsec)) then
    sclsec = isec
  end if

  if (time % caltype == CAL_CYCLIC) then
    iday = time % day
    if (present(year)) year = 0
    if (present(sclyear)) sclyear = 0
    if (present(sclmon)) then
      sclmon = floor(iday / CYCLIC_MDAYS)
      iday = ceiling( modulo(iday, CYCLIC_MDAYS) )
    elseif (present(mon)) then
      mon = floor(iday / CYCLIC_MDAYS)
      iday = ceiling( modulo(iday, CYCLIC_MDAYS) )
    end if
    if (present(day)) day = iday
    if (present(sclday)) sclday = iday
    goto 999
  endif
  if (time % caltype == CAL_NOLEAP) then
    iday = int( modulo(time%day - 91, YEAR_DAYS) )
    iyear = int( (time%day - 91 - iday) / YEAR_DAYS )
  else
    if (time % caltype == CAL_JULIAN .or. time%day < 640196) then
      iday = int( modulo(time%day - 92, FOUR_YEARS) )
      iyear = int( (time%day - 92 - iday) / FOUR_YEARS ) * 4
    else
      iday = int( modulo(time%day - 94, FOUR_CENTURY) )
      iyear = int( (time%day - 94 - iday) / FOUR_CENTURY ) * 400
      if (iday == FOUR_CENTURY - 1) then
        iyear = iyear + 300
        iday = 36525
      else
        iyear = iyear + int( iday / 36524 ) * 100
        iday = int( modulo(iday, 36524) )
      endif
      iyear = iyear + int( iday / FOUR_YEARS ) * 4
      iday = int( modulo(iday, FOUR_YEARS) )
    endif
    if (iday == FOUR_YEARS - 1) then
      iyear = iyear + 3
      iday = YEAR_DAYS
    else
      iyear = iyear + int( iday / YEAR_DAYS )
      iday = int( modulo(iday, YEAR_DAYS) )
    endif
  endif

  iday = iday * 10 + 922
  month = int( iday / 306 )

  if (present(sclyear)) then
    imon = mod(month - 1, YEAR_MONTHS) + 1
    sclyear = iyear + int( (month - imon) / YEAR_MONTHS )
  elseif (present(year)) then
    imon = mod(month - 1, YEAR_MONTHS) + 1
    year = iyear + int( (month - imon) / YEAR_MONTHS )
  else
    imon = month
  end if
  if (present(sclmon)) then
    iday = int( mod(iday, 306) / 10 ) + 1
    sclmon = imon
  elseif (present(mon)) then
    iday = int( mod(iday, 306) / 10 ) + 1
    mon = imon
  else
    iday = int( iday / 10 )  + 1
  end if

  if (present(day)) day = iday
  if (present(sclday)) sclday = iday

999 continue
  !call EndSub(subname)
end subroutine DCDateTimeEval1


subroutine DCDiffTimeEval1(diff, &
  & year, mon, day, hour, min, sec, nondim, &
  & sclyear, sclmon, sclday, sclsec, sclnondim, err)
  !
  ! dc_date_types#DC_DIFFTIME 型変数 *diff* を
  ! 年 *year*, 月 *mon*, 日 *day*, 時間 *hour*, 分 *min*, 秒 *sec*,
  ! 無次元時間 *nondim* に変換して返します.
  !
  use dc_types, only: DP
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_ENODIMTIME, DC_EDIMTIME
  use dc_date_types, only: DC_DIFFTIME, &
    & MIN_SECONDS, HOUR_SECONDS, YEAR_MONTHS
  use dc_scaledsec, only:  DC_SCALED_SEC, &
    & assignment(=), DCScaledSecPutLine, &
    & operator(==), operator(>), operator(<), operator(>=), operator(<=), &
    & operator(+), operator(-), operator(*), operator(/), mod, modulo, &
    & abs, int, floor, ceiling
  implicit none
  type(DC_DIFFTIME), intent(in):: diff
  integer, intent(out), optional:: year ! 年
  integer, intent(out), optional:: mon  ! 月
  integer, intent(out), optional:: day  ! 日
  integer, intent(out), optional:: hour ! 時
  integer, intent(out), optional:: min  ! 分
  real(DP),intent(out), optional:: sec  ! 秒
  real(DP),intent(out), optional:: nondim  ! 無次元時間. Nondimensional time
  type(DC_SCALED_SEC), intent(out), optional:: sclyear ! 年 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(out), optional:: sclmon  ! 月 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(out), optional:: sclday  ! 日 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(out), optional:: sclsec  ! 秒 (DC_SCALED_SEC 型)
  type(DC_SCALED_SEC), intent(out), optional:: sclnondim  ! 無次元時間 (DC_SCALED_SEC 型)
  logical, intent(out), optional :: err
  type(DC_SCALED_SEC):: imon, isec
  integer:: stat
  character(*), parameter :: subname = 'DCDiffTimeEval1'
continue
  !call BeginSub(subname)
  stat = DC_NOERR
  if ( present(sclnondim) ) then
    if ( .not. diff % nondim_flag ) then
      stat = DC_EDIMTIME
      goto 999
    end if
    sclnondim = diff % sec
  elseif ( present(nondim) ) then
    if ( .not. diff % nondim_flag ) then
      stat = DC_EDIMTIME
      goto 999
    end if
    nondim = diff % sec
  else
    if ( diff % nondim_flag ) then
      stat = DC_ENODIMTIME
      goto 999
    end if
  end if

  imon = diff % mon
  isec = diff % sec
  if (present(sclyear)) then
    sclyear = int( imon / YEAR_MONTHS )
    imon = mod(imon, YEAR_MONTHS)
  elseif (present(year)) then
    year = int( imon / YEAR_MONTHS )
    imon = mod(imon, YEAR_MONTHS)
  endif

  if (present(sclmon)) then
    sclmon = imon
  elseif (present(mon)) then
    mon = imon
  endif

  if (present(sclday)) then
    sclday = diff % day
  elseif (present(day)) then
    day = diff % day
  else
    isec = isec + diff % day * diff % day_seconds
  endif

  if (present(hour)) then
    hour = int(isec / HOUR_SECONDS)
    isec = mod(isec, HOUR_SECONDS)
  endif
  if (present(min)) then
    min = int(isec / MIN_SECONDS)
    isec = mod(isec, MIN_SECONDS)
  endif

  if (present(sec)) then
    sec = isec
  endif
  if (present(sclsec)) then
    sclsec = isec
  endif
999 continue
  call StoreError(stat, subname, err)
  !call EndSub(subname)
end subroutine DCDiffTimeEval1


function DCDateTimeEvalDay(time) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数の日時を日数に換算して
  ! 倍精度実数型変数で返します. (例えば 12 時間は 0.5 日と換算されます).
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DATETIME
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=), operator(/), operator(+)
  implicit none
  real(DP):: result
  type(DC_DATETIME), intent(in):: time
  type(DC_SCALED_SEC):: day, sec
continue
  call Eval(time, sclday = day, sclsec = sec)
  result = day + sec / time % day_seconds
end function DCDateTimeEvalDay

function DCDiffTimeEvalDay(diff) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の日時を日数に換算して
  ! 倍精度実数型変数で返します. (例えば 12 時間は 0.5 日と換算されます).
  !
  ! 1 ヶ月は dc_date_types#CYCLIC_MDAYS と換算します.
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DIFFTIME, CYCLIC_MDAYS
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=), operator(/), &
    & operator(+), operator(*), int
  implicit none
  real(DP):: result
  type(DC_DIFFTIME), intent(in):: diff
  type(DC_SCALED_SEC):: day, mon, sec
continue
  call Eval(diff, sclmon = mon, sclday = day, sclsec = sec)
  result = int(mon * CYCLIC_MDAYS) + day + sec / diff % day_seconds
end function DCDiffTimeEvalDay


function DCDateTimeEvalHour(time) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数の日時を時間に換算して
  ! 倍精度実数型変数で返します.
  ! (例えば 2 日は 48 時間に, 30 分 は 0.5 時間と換算されます).
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DATETIME, HOUR_SECONDS
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=), operator(/), &
    & operator(+), operator(*), int
  implicit none
  real(DP):: result
  type(DC_DATETIME), intent(in):: time
  type(DC_SCALED_SEC):: day, sec
continue
  call Eval(time, sclday = day, sclsec = sec)
  result = (day * time % day_seconds + sec) / HOUR_SECONDS
end function DCDateTimeEvalHour

function DCDiffTimeEvalHour(diff) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の日時を時間に換算して
  ! 倍精度実数型変数で返します. 
  ! (例えば 2 日は 48 時間に, 30 分 は 0.5 時間と換算されます).
  !
  ! 1 ヶ月は dc_date_types#CYCLIC_MDAYS と換算します.
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DIFFTIME, HOUR_SECONDS, CYCLIC_MDAYS
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=), operator(/), &
    & operator(+), operator(*), int
  implicit none
  real(DP):: result
  type(DC_DIFFTIME), intent(in):: diff
  type(DC_SCALED_SEC):: mon, day, sec
continue
  call Eval(diff, sclmon = mon, sclday = day, sclsec = sec)
  result = ( int(mon * CYCLIC_MDAYS) + day &
    &         * diff % day_seconds + sec) / HOUR_SECONDS
end function DCDiffTimeEvalHour


function DCDateTimeEvalMin(time) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数の日時を分に換算して
  ! 倍精度実数型変数で返します.
  ! (例えば 1 日は 3600 分に, 30 秒 は 0.5 分と換算されます).
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DATETIME, MIN_SECONDS
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=), operator(/), &
    & operator(+), operator(*), int
  implicit none
  real(DP):: result
  type(DC_DATETIME), intent(in):: time
  type(DC_SCALED_SEC):: day, sec
continue
  call Eval(time, sclday = day, sclsec = sec)
  result = (day * time % day_seconds + sec) / MIN_SECONDS
end function DCDateTimeEvalMin

function DCDiffTimeEvalMin(diff) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の日時を分に換算して
  ! 倍精度実数型変数で返します. 
  ! (例えば 1 日は 3600 分に, 30 秒 は 0.5 分と換算されます).
  !
  ! 1 ヶ月は dc_date_types#CYCLIC_MDAYS と換算します.
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DIFFTIME, MIN_SECONDS, CYCLIC_MDAYS
  use dc_scaledsec, only: DC_SCALED_SEC, assignment(=), operator(/), &
    & operator(+), operator(*), int
  implicit none
  real(DP):: result
  type(DC_DIFFTIME), intent(in):: diff
  type(DC_SCALED_SEC):: mon, day, sec
continue
  call Eval(diff, sclmon = mon, sclday = day, sclsec = sec)
  result = ( int(mon * CYCLIC_MDAYS) + day &
    &         * diff % day_seconds + sec) / MIN_SECONDS
end function DCDiffTimeEvalMin


function DCDateTimeEvalSec(time) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数の日時を秒に換算して
  ! 倍精度実数型変数で返します.
  !
  ! 年の要素は無視されます. すなわち, 1999-01-01 が格納された time と
  ! 2007-01-01 が格納された time からは同じ値が返ります.
  ! (これはもしかすると望ましく無い動作かもしれません).
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DATETIME
  use dc_scaledsec, only: assignment(=)
  implicit none
  real(DP):: result
  type(DC_DATETIME), intent(in):: time
  integer:: day
  real(DP):: sec, day_seconds
continue
  call Eval(time, day = day, sec = sec)
  day_seconds = time % day_seconds
  result = day * day_seconds + sec
end function DCDateTimeEvalSec

function DCDiffTimeEvalSec(diff) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の日時を秒に換算して
  ! 倍精度実数型変数で返します.
  !
  ! 1 ヶ月は dc_date_types#CYCLIC_MDAYS と換算します.
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DIFFTIME, CYCLIC_MDAYS
  use dc_scaledsec, only: assignment(=)
  implicit none
  real(DP):: result
  type(DC_DIFFTIME), intent(in):: diff
  integer:: mon, day
  real(DP):: sec, day_seconds
continue
  if ( .not. diff % nondim_flag ) then
    call Eval(diff, mon = mon, day = day, sec = sec)
    day_seconds = diff % day_seconds
    result = int(mon * CYCLIC_MDAYS) + day * day_seconds + sec
  else
    call Eval(diff, nondim = result)
  end if
end function DCDiffTimeEvalSec

function DCDiffTimeEvalNondim(diff) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の日時を無時限時間に換算して
  ! 倍精度実数型変数で返します.
  !
  ! 1 ヶ月は dc_date_types#CYCLIC_MDAYS と換算します.
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DIFFTIME, CYCLIC_MDAYS
  implicit none
  real(DP):: result
  type(DC_DIFFTIME), intent(in):: diff
  real(DP):: nondim
continue
  call Eval(diff, nondim=nondim)
  result = nondim
end function DCDiffTimeEvalNondim

function DCDateTimeEvalSclSec(time) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数の日時を秒に換算して
  ! DC_SCALED_SEC 型で返します.
  !
  ! 年の要素は無視されます. すなわち, 1999-01-01 が格納された time と
  ! 2007-01-01 が格納された time からは同じ値が返ります.
  ! (これはもしかすると望ましく無い動作かもしれません).
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DATETIME
  use dc_scaledsec, only: DC_SCALED_SEC, operator(/), &
    & operator(+), operator(*), int
  implicit none
  type(DC_SCALED_SEC):: result
  type(DC_DATETIME), intent(in):: time
  type(DC_SCALED_SEC):: day, sec
continue
  call Eval(time, sclday = day, sclsec = sec)
  result = day * time % day_seconds + sec
end function DCDateTimeEvalSclSec

function DCDiffTimeEvalSclSec(diff) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の日時を秒に換算して
  ! DC_SCALED_SEC 型で返します.
  !
  ! 1 ヶ月は dc_date_types#CYCLIC_MDAYS と換算します.
  !
  use dc_types, only: DP
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DIFFTIME, CYCLIC_MDAYS
  use dc_scaledsec, only: DC_SCALED_SEC, operator(/), &
    & operator(==), operator(+), operator(*), int
  implicit none
  type(DC_SCALED_SEC):: result
  type(DC_DIFFTIME), intent(in):: diff
  type(DC_SCALED_SEC):: mon, day, sec
  type(DC_SCALED_SEC):: zero_sec
continue
  if ( .not. diff % nondim_flag ) then
    call Eval(diff, sclmon = mon, sclday = day, sclsec = sec)
    if ( mon == zero_sec ) then
      result = day * diff % day_seconds + sec
    else
      result = ( int(mon * CYCLIC_MDAYS) + day ) * diff % day_seconds + sec
    end if
  else
    call Eval(diff, sclnondim = sec)
    result = sec
  end if
end function DCDiffTimeEvalSclSec

function DCDateTimeEvalByUnit(time, unit, unit_symbol) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数の日時を *unit* または
  ! *unit_symbol* の単位
  ! に換算して倍精度実数型変数で返します. 
  ! 
  ! *unit* には
  ! 日 dc_date_types#UNIT_DAY, 時 dc_date_types#UNIT_HOUR,
  ! 分 dc_date_types#UNIT_MIN, 秒 dc_date_types#UNIT_SEC 
  ! を与えることが可能です. 
  !
  ! *unit_symbol* には
  ! 日 dc_date_types#UNIT_SYMBOL_DAY, 時 dc_date_types#UNIT_SYMBOL_HOUR,
  ! 分 dc_date_types#UNIT_SYMBOL_MIN, 秒 dc_date_types#UNIT_SYMBOL_SEC 
  ! を与えることが可能です. 
  !
  ! これらに該当しないものを *unit* または *unit_symbol* 
  ! に与えた場合, もしくは引数を両方とも与えない場合, 0.0 が返ります.
  !
  use dc_types, only: DP, TOKEN
  use dc_date_generic, only: EvalSec, EvalMin, EvalHour, EvalDay, ParseTimeUnits
  use dc_date_types, only: DC_DATETIME, &
    & UNIT_SYMBOL_DAY, UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC, &
    & UNIT_SYMBOL_ERR
  implicit none
  real(DP):: result
  type(DC_DATETIME), intent(in):: time
  character(*), intent(in):: unit
  integer, intent(in), optional:: unit_symbol
  integer:: symbol
continue
  symbol = UNIT_SYMBOL_ERR
  if ( present(unit_symbol) ) then
    symbol = unit_symbol
  else
    symbol = ParseTimeUnits(unit)
  end if

  if ( symbol == UNIT_SYMBOL_SEC ) then
    result = EvalSec(time)
  elseif ( symbol == UNIT_SYMBOL_MIN ) then
    result = EvalMin(time)
  elseif ( symbol == UNIT_SYMBOL_HOUR ) then
    result = EvalHour(time)
  elseif ( symbol == UNIT_SYMBOL_DAY ) then
    result = EvalDay(time)
  else
    result = 0.0_DP
  end if
end function DCDateTimeEvalByUnit


function DCDiffTimeEvalByUnit(diff, unit, unit_symbol) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数の日時を *unit* の単位
  ! に換算して倍精度実数型変数で返します. 
  ! 
  ! *unit* には
  ! 日 dc_date_types#UNIT_DAY, 時 dc_date_types#UNIT_HOUR,
  ! 分 dc_date_types#UNIT_MIN, 秒 dc_date_types#UNIT_SEC, 
  ! 無次元時間 dc_date_types#UNIT_NONDIM
  ! を与えることが可能です. 
  ! 
  ! *unit_symbol* には
  ! 日 dc_date_types#UNIT_SYMBOL_DAY, 時 dc_date_types#UNIT_SYMBOL_HOUR,
  ! 分 dc_date_types#UNIT_SYMBOL_MIN, 秒 dc_date_types#UNIT_SYMBOL_SEC 
  ! 無次元時間 dc_date_types#UNIT_SYMBOL_NONDIM
  ! を与えることが可能です. 
  !
  ! これらに該当しないものを *unit* または *unit_symbol* 
  ! に与えた場合, もしくは引数を両方とも与えない場合, 0.0 が返ります.
  !
  use dc_types, only: DP, TOKEN
  use dc_date_generic, only: EvalNondim, EvalSec, EvalMin, EvalHour, EvalDay, &
    & ParseTimeUnits
  use dc_date_types, only: DC_DIFFTIME, &
    & UNIT_SYMBOL_DAY, UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, &
    & UNIT_SYMBOL_SEC, UNIT_SYMBOL_NONDIM, UNIT_SYMBOL_ERR
  implicit none
  real(DP):: result
  type(DC_DIFFTIME), intent(in):: diff
  character(*), intent(in):: unit
  integer, intent(in), optional:: unit_symbol
  integer:: symbol
continue
  symbol = UNIT_SYMBOL_ERR
  if ( present(unit_symbol) ) then
    symbol = unit_symbol
  else
    symbol = ParseTimeUnits(unit)
  end if

  if ( symbol == UNIT_SYMBOL_NONDIM ) then
    result = EvalNondim(diff)
  elseif ( symbol == UNIT_SYMBOL_SEC ) then
    result = EvalSec(diff)
  elseif ( symbol == UNIT_SYMBOL_MIN ) then
    result = EvalMin(diff)
  elseif ( symbol == UNIT_SYMBOL_HOUR ) then
    result = EvalHour(diff)
  elseif ( symbol == UNIT_SYMBOL_DAY ) then
    result = EvalDay(diff)
  else
    result = 0.0_DP
  end if
end function DCDiffTimeEvalByUnit



!!$subroutine DCDateTimeEval0(time, mon, day, sec)
!!$  !
!!$  ! dc_date_types#DC_DATETIME 型変数の *time* を
!!$  ! 月 *mon*, 日 *day*, 秒 *sec* に変換して返す.
!!$  !
!!$  use dc_types, only: DP
!!$  use dc_date_types, only: DC_DATETIME, &
!!$    & CYCLIC_MDAYS, CAL_NOLEAP, CAL_JULIAN, CAL_CYCLIC, &
!!$    & FOUR_YEARS, FOUR_CENTURY
!!$  use dc_trace, only: BeginSub, EndSub
!!$  implicit none
!!$  type(DC_DATETIME), intent(in):: time
!!$  integer, intent(out):: mon, day
!!$  real(DP), intent(out):: sec
!!$  integer:: year, month
!!$  character(*), parameter :: subname = 'DCDateTimeEval0'
!!$continue
!!$  call BeginSub(subname)
!!$  sec = time%sec
!!$  if (time % caltype == CAL_CYCLIC) then
!!$    day = modulo(dble(time%day - 1), CYCLIC_MDAYS) + 1
!!$    mon = (time%day - 1) / CYCLIC_MDAYS
!!$    goto 999
!!$  endif
!!$  if (time % caltype == CAL_NOLEAP) then
!!$    day = modulo(time%day - 91, 365)
!!$    year = (time%day - 91 - day) / 365
!!$  else
!!$    if (time % caltype == CAL_JULIAN .or. time%day < 640196) then
!!$      day = modulo(time%day - 92, FOUR_YEARS)
!!$      year = (time%day - 92 - day) / FOUR_YEARS * 4
!!$    else
!!$      day = modulo(time%day - 94, FOUR_CENTURY)
!!$      year = (time%day - 94 - day) / FOUR_CENTURY * 400
!!$      if (day == FOUR_CENTURY - 1) then
!!$        year = year + 300
!!$        day = 36525
!!$      else
!!$        year = year + day / 36524 * 100
!!$        day = modulo(day, 36524)
!!$      endif
!!$      year = year + day / FOUR_YEARS * 4
!!$      day = modulo(day, FOUR_YEARS)
!!$    endif
!!$    if (day == FOUR_YEARS - 1) then
!!$      year = year + 3
!!$      day = 365
!!$    else
!!$      year = year + day / 365
!!$      day = modulo(day, 365)
!!$    endif
!!$  endif
!!$  day = day * 10 + 922
!!$  month = day / 306
!!$  mon = mod(month - 1, 12) + 1
!!$  year = year + (month - mon) / 12
!!$  day = mod(day, 306) / 10  + 1
!!$999 continue
!!$  call EndSub(subname, 'mon=<%d>, day=<%d>, sec=<%f>',&
!!$    & i=(/mon, day/), d=(/sec/))
!!$end subroutine DCDateTimeEval0
