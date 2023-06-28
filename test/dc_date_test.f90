program dc_date_test
  use dc_types, only: STRING, DP, STDOUT
  use dc_test, only: AssertEqual
  use dc_string, only: StoA, Printf
  use dc_date, only: DC_DATETIME, DC_DIFFTIME, &
    & DCDateTimeCreate, DCDiffTimeCreate, Eval, &
    & DCDateTimePutLine, DCDiffTimePutLine, assignment(=), &
    & operator(+), operator(-), operator(*), operator(/), mod, &
    & operator(<), operator(>), operator(<=), operator(>=), &
    & operator(==), min, max, &
    & EvalDay, EvalHour, EvalMin, EvalSec, EvalNondim, EvalByUnit, &
    & ValidZone, ZoneToDiff, SetZone, SetSecOfDay, ParseTimeUnits, &
    & toChar, toCharCal
  use dc_date_types, only: &
    & CAL_NOLEAP, CAL_CYCLIC, CAL_JULIAN, day_seconds, &
    & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
    & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC, &
    & UNIT_SYMBOL_NONDIM, UNIT_SYMBOL_ERR
  use dc_trace, only: SetDebug

  implicit none
  type(DC_DATETIME):: time1, time2, time3, time4, time5, time6
  type(DC_DATETIME):: time11, time12, time13, time14
  type(DC_DIFFTIME):: diff1, diff2, diff3, diff4, diff5, diff6, diff7
  type(DC_DIFFTIME):: diff8, diff9
  type(DC_DIFFTIME):: diff11, diff12, diff13, diff14, diff15
  integer:: year = 0, mon = 0, day = 0, hour = 0, minu = 0
  integer:: time_symbol
  real(DP):: sec = 0.0_DP, nondim = 0.0_DP
  real(DP):: day_seconds_default
  character(STRING) :: test_answer
  logical:: err
continue

  call SetDebug

  !-------------------------------------------------------------------
  ! デフォルトの一日の秒数を保存
  ! Save default seconds of a day 
  !-------------------------------------------------------------------
  day_seconds_default = day_seconds


  ! 暦チェック
  ! Check calender
  !
  call DCDateTimeCreate(time1)
  call Printf(STDOUT, 'Current time is %c', c1=trim(toChar(time1)))
  call DCDateTimePutLine(time1)

  call DCDateTimeCreate(time2, year=2008, mon=2, day=29, hour=20, sec=1300.5_DP, &
    & zone='+09:00', caltype=CAL_NOLEAP)
  call DCDateTimePutLine(time2)
  call AssertEqual('DateTime (CAL_NOLEAP), Create/toChar test 1', &
    & '2008-03-01T20:21:40.5+09:00', toChar(time2))
  call DCDateTimeCreate(time2, year=2008, mon=2, day=29, hour=20, sec=1300.5_DP, &
    & zone='+09:00', caltype_str='NOLEAP')
  call AssertEqual('DateTime (CAL_NOLEAP), Create/toChar test 2', &
    & '2008-03-01T20:21:40.5+09:00', toChar(time2))

  call DCDateTimeCreate(time3, year=1999, mon=11, day=13, hour=20, min=3, sec=1300.5_DP)
  call DCDateTimePutLine(time3)
  call Eval(time3, year=year, mon=mon, day=day, hour=hour, min=minu, sec=sec)
  call AssertEqual('DateTime (CAL_GREGORIAN), Create/Eval test 1', &
    & (/1999, 11, 13, 20, 24/), (/year, mon, day, hour, minu/))
  call AssertEqual('DateTime (CAL_GREGORIAN), Create/Eval test 2', &
    & (/40.5_DP/), (/sec/))
  year = 0; mon = 0; day = 0; hour = 0; minu = 0; sec = 0.0_DP

  call DCDateTimeCreate(time4, mon=0, day=1, sec=12096.0_DP, caltype=CAL_CYCLIC)
  call DCDateTimePutLine(time4)
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalDay test 1', &
    & 114, nint(EvalDay(time4) * 100))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalHour test 1', &
    & 2736, nint(EvalHour(time4) * 100))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalMin test 1', &
    & 16416, nint(EvalMin(time4) * 10))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalSec test 1', &
    & 98496, nint(EvalSec(time4)))

  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 1', &
    & 114, nint(EvalByUnit(time4, '   day') * 100))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 2', &
    & 2736, nint(EvalByUnit(time4, 'HOUR') * 100))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 3', &
    & 16416, nint(EvalByUnit(time4, 'minutes') * 10))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 4', &
    & 98496, nint(EvalByUnit(time4, 'sec.')))

  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 5', &
    & 114, nint(EvalByUnit(time4, unit_symbol = UNIT_SYMBOL_DAY) * 100))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 6', &
    & 2736, nint(EvalByUnit(time4, unit_symbol = UNIT_SYMBOL_HOUR) * 100))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 7', &
    & 16416, nint(EvalByUnit(time4, unit_symbol = UNIT_SYMBOL_MIN) * 10))
  call AssertEqual('DateTime (CAL_CYCLIC), Create/EvalByUnit test 8', &
    & 98496, nint(EvalByUnit(time4, unit_symbol = UNIT_SYMBOL_SEC)))

  call DCDateTimeCreate(time4, mon=0, caltype_str='')
  call AssertEqual('DateTime, Create/toCharCal test 1', &
    & 'gregorian', toCharCal(time4) )
  call DCDateTimeCreate(time4, mon=0, caltype_str='julian')
  call AssertEqual('DateTime, Create/toCharCal test 2', &
    & 'JULIAN', toCharCal(time4, .true.) )

  ! DC_DIFFTIME の作成と Eval のチェック
  ! Check creation and "Eval" of "DC_DIFFTIME"
  !
  call DCDiffTimeCreate(diff1, mon=2, day=12, sec=100345.0_DP)
  call Eval(diff1, mon=mon, day=day, sec=sec)
  call AssertEqual('DiffTime, Create/Eval test 1', &
    & (/2, 13/), (/mon, day/))
  call AssertEqual('DiffTime, Create/Eval test 2', &
    & (/13945.0_DP/), (/sec/))
  call Eval(diff1, nondim=nondim, err=err)
  call AssertEqual('DiffTime, Create/Eval test 3', &
    & .true., err )
  call DCDiffTimeCreate(diff1, nondim=345.0_DP)
  call Eval(diff1, sec=sec, err=err)
  call AssertEqual('DiffTime, Create/Eval test 4', &
    & .true., err )
  call Eval(diff1, nondim=nondim, err=err)
  call AssertEqual('DiffTime, Create/Eval test 5', &
    & (/345.0_DP/), (/nondim/))


  ! toChar チェック
  ! Check "toChar"
  !
  call DCDiffTimeCreate(diff1, mon=2, day=12, sec=100345.0_DP)
  call AssertEqual('DiffTime, Create/toChar test 1', &
    & '+0000-02-13T03:52:25', toChar(diff1))
  call DCDiffTimeCreate(diff1, value=25.0_DP, unit='  day')
  call AssertEqual('DiffTime, Create/toChar test 2', &
    & '+0000-00-25T00:00:00', toChar(diff1))
  call DCDiffTimeCreate(diff1, value=-25.0_DP, unit='min')
  call AssertEqual('DiffTime, Create/toChar test 3', &
    & '-0000-00-00T00:25:00', toChar(diff1))
  call DCDiffTimeCreate(diff1, value=25.0_DP, unit='s')
  call AssertEqual('DiffTime, Create/toChar test 4', &
    & '+0000-00-00T00:00:25', toChar(diff1))
  call DCDiffTimeCreate(diff1, value=25.5, unit='1')
  call AssertEqual('DiffTime, Create/toChar test 5', &
    & '25.5', toChar(diff1))

  call DCDiffTimeCreate(diff1, hour=2, min=12)
  call AssertEqual('DiffTime, Create/toChar test 6', &
    & '+0000-00-00T02:12:00', toChar(diff1))
  call DCDiffTimeCreate(diff1, hour=2, min=125)
  call AssertEqual('DiffTime, Create/toChar test 7', &
    & '+0000-00-00T04:05:00', toChar(diff1))
  call DCDiffTimeCreate(diff1, hour=-2, min=25)
  call AssertEqual('DiffTime, Create/toChar test 8', &
    & '-0000-00-00T01:35:00', toChar(diff1))
  call DCDiffTimeCreate(diff1, day=1, hour=-2, min=25)
  call AssertEqual('DiffTime, Create/toChar test 9', &
    & '+0000-00-00T22:25:00', toChar(diff1))
  call DCDiffTimeCreate(diff1, hour=-9, min=-10)
  call AssertEqual('DiffTime, Create/toChar test 10', &
    & '-0000-00-00T09:10:00', toChar(diff1))


  ! EvalXXXX チェック
  ! Check "EvalXXXX"
  !
  call DCDiffTimeCreate(diff2, mon=0, day=1, sec=12096.0_DP)
  call AssertEqual('DiffTime, Create/EvalDay test 1', &
    & 114, nint(EvalDay(diff2) * 100))
  call AssertEqual('DiffTime, Create/EvalHour test 1', &
    & 2736, nint(EvalHour(diff2) * 100))
  call AssertEqual('DiffTime, Create/EvalMin test 1', &
    & 16416, nint(EvalMin(diff2) * 10))
  call AssertEqual('DiffTime, Create/EvalSec test 1', &
    & 98496, nint(EvalSec(diff2)))
  call DCDiffTimeCreate(diff2, nondim=12096.0_DP)
  call AssertEqual('DiffTime, Create/EvalNondim test 1', &
    & 12096, nint(EvalNondim(diff2)))

  call DCDiffTimeCreate(diff2, mon=0, day=1, sec=12096.0_DP)
  call AssertEqual('DiffTime, Create/EvalByUnit test 1', &
    & 114, nint(EvalByUnit(diff2, '  d  ') * 100))
  call AssertEqual('DiffTime, Create/EvalByUnit test 2', &
    & 2736, nint(EvalByUnit(diff2, 'hrs.') * 100))
  call AssertEqual('DiffTime, Create/EvalByUnit test 3', &
    & 16416, nint(EvalByUnit(diff2, 'min') * 10))
  call AssertEqual('DiffTime, Create/EvalByUnit test 4', &
    & 98496, nint(EvalByUnit(diff2, 's')))
  call DCDiffTimeCreate(diff2, value=12345.0, unit='1')
  call AssertEqual('DiffTime, Create/EvalByUnit test 5', &
    & 12345, nint(EvalByUnit(diff2, '1')))

  call DCDiffTimeCreate(diff2, mon=0, day=1, sec=12096.0_DP)
  call AssertEqual('DiffTime, Create/EvalByUnit test 6', &
    & 114, nint(EvalByUnit(diff2, unit_symbol = UNIT_SYMBOL_DAY) * 100))
  call AssertEqual('DiffTime, Create/EvalByUnit test 7', &
    & 2736, nint(EvalByUnit(diff2, unit_symbol = UNIT_SYMBOL_HOUR) * 100))
  call AssertEqual('DiffTime, Create/EvalByUnit test 8', &
    & 16416, nint(EvalByUnit(diff2, unit_symbol = UNIT_SYMBOL_MIN) * 10))
  call AssertEqual('DiffTime, Create/EvalByUnit test 9', &
    & 98496, nint(EvalByUnit(diff2, unit_symbol = UNIT_SYMBOL_SEC)))
  call DCDiffTimeCreate(diff2, value=12345.0, unit='1')
  call AssertEqual('DiffTime, Create/EvalByUnit test 10', &
    & 12345, nint(EvalByUnit(diff2, unit_symbol = UNIT_SYMBOL_NONDIM)))

  ! 四則演算チェック
  ! Check four arithmetic operations
  !
  call DCDiffTimeCreate(diff3, mon=2, day=10, sec=1000.0_DP)
  call DCDiffTimeCreate(diff4, mon=1, day=5,  sec=500.0_DP)
  call DCDateTimeCreate(time5, &
    & year=2006, mon=9, day=20, hour=1, min=3, sec=30.5_DP, &
    & zone='+09:00')
  call DCDateTimeCreate(time6, &
    & year=2006, mon=11, day=13, hour=23, min=3, sec=20.5_DP, &
    & zone='-10:00')

  call AssertEqual('Operator(+) test 1', &
    & '2006-11-30T01:20:10.5+09:00', toChar(diff3 + time5))
  call AssertEqual('Operator(+) test 2', &
    & '2006-11-30T01:20:10.5+09:00', toChar(time5 + diff3))
  call AssertEqual('Operator(+) test 3', &
    & '+0000-03-15T00:25:00', toChar(diff3 + diff4))

  call AssertEqual('Operator(-) test 1', &
    & '+0000-00-55T16:59:50', toChar(time6 - time5))
  call AssertEqual('Operator(-) test 2', &
    & '2006-09-03T22:46:40.5-10:00', toChar(time6 - diff3))
  call AssertEqual('Operator(-) test 3', &
    & '+0000-01-05T00:08:20', toChar(diff3 - diff4))
  call AssertEqual('Operator(-) test 4', &
    & '+0000-02-10T00:16:30', toChar(diff3 - 10))
  call AssertEqual('Operator(-) test 5', &
    & '+0000-02-10T00:15:00', toChar(diff3 - 100.0))
  call AssertEqual('Operator(-) test 6', &
    & '+0000-02-10T00:00:00', toChar(diff3 - 1000.0_DP))

  test_answer = '+0000-02-10T00:16:40'
  call AssertEqual('Operator(*) test 1', &
    & trim(test_answer), toChar(diff4 * 2))
  call AssertEqual('Operator(*) test 2', &
    & trim(test_answer), toChar(diff4 * 2.0))
  call AssertEqual('Operator(*) test 3', &
    & trim(test_answer), toChar(diff4 * 2.0_DP))
  call AssertEqual('Operator(*) test 4', &
    & trim(test_answer), toChar(2 * diff4))
  call AssertEqual('Operator(*) test 5', &
    & trim(test_answer), toChar(2.0 * diff4))
  call AssertEqual('Operator(*) test 6', &
    & trim(test_answer), toChar(2.0_DP * diff4))

  test_answer = '+0000-01-05T00:08:20'
  call AssertEqual('Operator(/) test 1', &
    & trim(test_answer), toChar(diff3 / 2))
  call AssertEqual('Operator(/) test 2', &
    & trim(test_answer), toChar(diff3 / 2.0))
  call AssertEqual('Operator(/) test 3', &
    & trim(test_answer), toChar(diff3 / 2.0_DP))
  call AssertEqual('Operator(/) test 4', &
    & 2.0_DP, diff3 / diff4)

  call AssertEqual('mod test 1', &
    & '+0000-00-00T00:00:00', toChar(mod(diff3, diff4)))

  ! 比較演算子チェック
  ! Check comparative operators
  !
  call DCDateTimeCreate(time12, year=2008, mon=2, day=29, hour=20, sec=1300.5_DP, &
    & zone='+09:00')
  call DCDateTimeCreate(time13, year=2008, mon=2, day=29, hour=19, sec=1300.5_DP, &
    & zone='-13:00')
  call DCDateTimeCreate(time14, year=2008, mon=2, day=28, hour=22, sec=1300.5_DP, &
    & zone='-13:00')

  !call DCDateTimePutLine(time12)
  !call DCDateTimePutLine(time14)

  call DCDiffTimeCreate(diff5, day=10, hour=869, sec=1300.5_DP)
  !call DCDiffTimePutLine(diff5)
  call DCDiffTimeCreate(diff6, day=29, hour=20, sec=10.0_DP)
  diff7 = diff5 - diff6

  call AssertEqual('Operator(>) test 1', .true., time1 > time3)
  call AssertEqual('Operator(>) test 2', .false., time12 > time13)
  call AssertEqual('Operator(>) test 3', .true., diff5 > diff6)
  call AssertEqual('Operator(>) test 4', .true., 2577611 > diff6)
  call AssertEqual('Operator(>) test 5', .false., diff6 > 2577610)
  call AssertEqual('Operator(>=) test 1', .true., time1 >= time3)
  call AssertEqual('Operator(>=) test 2', .true., time12 >= time14)
  call AssertEqual('Operator(>=) test 3', .true., diff5 >= diff6)
  call AssertEqual('Operator(>=) test 4', .true., diff7 >= diff5 - diff6)
  call AssertEqual('Operator(>=) test 5', .true., 2577611 >= diff6)
  call AssertEqual('Operator(>=) test 6', .true., diff6 >= 2577610)
  call AssertEqual('Operator(<) test 1', .false., time1 < time3)
  call AssertEqual('Operator(<) test 2', .true., time12 < time13)
  call AssertEqual('Operator(<) test 3', .false., diff5 < diff6)
  call AssertEqual('Operator(<) test 4', .true., 2577609 < diff6)
  call AssertEqual('Operator(<) test 5', .false., diff6 < 2577610)
  call AssertEqual('Operator(<=) test 1', .false., time1 <= time3)
  call AssertEqual('Operator(<=) test 2', .true., time12 <= time13)
  call AssertEqual('Operator(<=) test 3', .false., diff5 <= diff6)
  call AssertEqual('Operator(<=) test 4', .true., diff7 <= diff5 - diff6)
  call AssertEqual('Operator(<=) test 5', .true., 2577609 <= diff6)
  call AssertEqual('Operator(<=) test 6', .true., diff6 <= 2577610)
  call AssertEqual('Operator(==) test 1', .false., time13 == time12)
  call AssertEqual('Operator(==) test 2', .true., time12 == time14)

  call DCDateTimeCreate(time13, year=2008, &
    & mon=2, day=29, hour=20, sec=1300.5_DP, &
    & zone='+09:00' )
  call AssertEqual('Operator(==) test 3', .true., time13 == time12)
  call AssertEqual('Operator(<) test 6', .false., time13 < time12)
  call AssertEqual('Operator(>) test 6', .false., time13 > time12)
  call AssertEqual('Operator(==) test 3', .false., diff5 == diff6)
  call AssertEqual('Operator(==) test 4', .true., diff5 == diff6 + diff7)
  call AssertEqual('Operator(==) test 5', .false., diff5 == 0.0_DP)
  diff8 = 3993700.0 ; diff9 = 0.5 ; diff8 = diff8 + diff9
  call AssertEqual('Operator(==) test 6', .true., diff8 == diff5)
  call AssertEqual('Operator(==) test 7', .true., diff5 - diff6 - diff7 == 0.0)
  call AssertEqual('Operator(==) test 8', .true., 0 == diff5 - diff6 - diff7)
  call AssertEqual('Operator(<) test 7', .false., diff5 - diff6  < diff7)
  call AssertEqual('Operator(>) test 7', .false., diff5 - diff6  > diff7)

  ! タイムゾーンチェック
  ! Check time zone
  !
  call AssertEqual('ValidZone test 1', .true., ValidZone('+09:00'))
  call AssertEqual('ValidZone test 2', .false., ValidZone('+0900'))
  call AssertEqual('ValidZone test 3', .false., ValidZone(' 13:00'))
  call AssertEqual('ValidZone test 4', .false., ValidZone('1'))

  call AssertEqual('ZoneToDiff test 1', '+0000-00-00T12:00:00', &
    & toChar(ZoneToDiff('-12:00')))
  call AssertEqual('ZoneToDiff test 2', '+0000-00-00T00:00:00', &
    & toChar(ZoneToDiff('aaaa')))
  call AssertEqual('ZoneToDiff test 3', '-0000-00-00T09:30:00', &
    & toChar(ZoneToDiff('+09:30')))

  call DCDateTimeCreate(time11, &
    & year=2006, mon=11, day=13, hour=20, min=3, sec=20.0_DP, &
    & zone='+09:00')
  call SetZone(time11, '-13:00')
  call AssertEqual('SetZone test 1', &
    & '2006-11-12T22:03:20-13:00', toChar(time11))

  call DCDateTimeCreate(time11, &
    & year=2006, mon=11, day=13, hour=20, min=3, sec=20.0_DP, &
    & zone_hour=-9)
  call AssertEqual('Create with zone_hour/zone_min test 1', &
    & '2006-11-13T20:03:20-09:00', toChar(time11))

  call DCDateTimeCreate(time11, &
    & year=2006, mon=11, day=13, hour=20, min=3, sec=20.0_DP, &
    & zone_hour=10, zone_min=-10)
  call AssertEqual('Create with zone_hour/zone_min test 2', &
    & '2006-11-13T20:03:20+09:50', toChar(time11))

  call DCDateTimeCreate(time11, &
    & year=2006, mon=11, day=13, hour=20, min=3, sec=20.0_DP, &
    & zone_hour=-10, zone_min=-10)
  call AssertEqual('Create with zone_hour/zone_min test 2', &
    & '2006-11-13T20:03:20-10:10', toChar(time11))

  ! day_seconds 変更チェック
  ! Check change of "day_seconds"
  !
  call DCDiffTimeCreate(diff11, mon=1, day=2, sec=80100.0_DP, day_seconds=40000.0_DP)
  call DCDiffTimeCreate(diff12, mon=1, day=2, sec=172900.0_DP)
  call SetSecOfDay(172800.0_DP)
  call DCDiffTimeCreate(diff13, mon=1, day=2, sec=345700.0_DP)
  call SetSecOfDay(day_seconds_default)

  call AssertEqual('DiffTime, Create with day_seconds test 1', &
    & '+0000-01-04T00:01:40', toChar(diff11))
  call AssertEqual('DiffTime, Create with day_seconds test 2', &
    & '+0000-01-04T00:01:40', toChar(diff12))
  call AssertEqual('DiffTime, Create/SetSecOfDay test 1', &
    & '+0000-01-04T00:01:40', toChar(diff13))

  ! 正規化チェック
  ! Check normalization
  !
  call DCDiffTimeCreate(diff14, mon=0, day=1, sec=0.0_DP)
  call DCDiffTimeCreate(diff15, mon=0, day=0, sec=86400.0_DP)

  call AssertEqual('Normalization test 1', &
    & .false., diff14 > diff15 )

  ! max, minチェック
  ! Check "max", "min"
  !
  call DCDateTimeCreate( time11, year=2008, mon=2, day=29, &
    & hour=19, sec=1300.5_DP, zone='+09:00' )
  call DCDateTimeCreate( time12, year=2007, mon=2, day=29, &
    & hour=20, sec=1300.5_DP, zone='+09:00' )

  time13 = max(time11, time12)
  call AssertEqual('DateTime Max test 1', .true., time13 == time11 )
  call AssertEqual('DateTime Max test 2', .true., time13 > time12 )

  time13 = min(time11, time12)
  call AssertEqual('DateTime Min test 1', .true., time13 == time12 )
  call AssertEqual('DateTime Min test 2', .true., time13 < time11 )

  call DCDiffTimeCreate( diff11, day=100, hour=20,  sec=10.0_DP )
  call DCDiffTimeCreate( diff12, day=10,  hour=869, sec=1300.5_DP )

  diff13 = max(diff11, diff12)
  call AssertEqual('DiffTime Max test 1', .true., diff13 == diff11 )
  call AssertEqual('DiffTime Max test 2', .true., diff13 > diff12 )

  diff13 = min(diff11, diff12)
  call AssertEqual('DiffTime Min test 1', .true., diff13 == diff12 )
  call AssertEqual('DiffTime Min test 2', .true., diff13 < diff11 )

  ! ParseTimeUnits チェック
  ! Check "ParseTimeUnits"
  !
  time_symbol = ParseTimeUnits('1')
  call AssertEqual('ParseTimeUnits test 1', UNIT_SYMBOL_NONDIM, time_symbol )

  time_symbol = ParseTimeUnits('sec')
  call AssertEqual('ParseTimeUnits test 2', UNIT_SYMBOL_SEC, time_symbol )

  time_symbol = ParseTimeUnits('min')
  call AssertEqual('ParseTimeUnits test 3', UNIT_SYMBOL_MIN, time_symbol )

  time_symbol = ParseTimeUnits('hr')
  call AssertEqual('ParseTimeUnits test 4', UNIT_SYMBOL_HOUR, time_symbol )

  time_symbol = ParseTimeUnits('day')
  call AssertEqual('ParseTimeUnits test 5', UNIT_SYMBOL_DAY, time_symbol )

  time_symbol = ParseTimeUnits('month')
  call AssertEqual('ParseTimeUnits test 6', UNIT_SYMBOL_MONTH, time_symbol )

  time_symbol = ParseTimeUnits('yr')
  call AssertEqual('ParseTimeUnits test 7', UNIT_SYMBOL_YEAR, time_symbol )

  time_symbol = ParseTimeUnits('hoge')
  call AssertEqual('ParseTimeUnits test 8', UNIT_SYMBOL_ERR, time_symbol )

  time_symbol = ParseTimeUnits('')
  call AssertEqual('ParseTimeUnits test 9', UNIT_SYMBOL_ERR, time_symbol )


  ! DCDiffTimeToChar チェック
  ! Check DCDiffTimeToCheck
  !
  call DCDiffTimeCreate(diff1, min=54)
  call DCDiffTimeCreate(diff2, min=9)
  call AssertEqual('DCDiffTimeToChar test 1', &
    & '+0000-00-00T01:03:00', trim(toChar(diff1 + diff2)) )

end program dc_date_test
