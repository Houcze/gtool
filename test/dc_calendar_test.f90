program dc_calendar_test
  use dc_types, only: STRING, DP, STDOUT
  use dc_test, only: AssertEqual
  use dc_string, only: StoA, Printf
  use dc_trace, only: SetDebug
  use dc_message, only: MessageNotify
  use dc_calendar, only: DC_CAL, DC_CAL_DATE, &
    & DCCalCreate, DCCalInquire, DCCalDefault, &
    & DCCalDateCreate, DCCalDateInquire, &
    & DCCalDateEval, &
    & DCCalConvertByUnit, DCCalConvertToSec, DCCalConvertToMin, &
    & DCCalConvertToHour, DCCalConvertToDay, &
    & DCCalDateDifference, &
    & DCCalDateEvalSecOfYear, DCCalDateEvalDayOfYear, &
    & DCCalDateEvalSecOfDay, DCCalDateChkLeapYear, &
    & DCCalDateCurrent, DCCalParseUnit, &
    & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
    & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC
  implicit none
  type(DC_CAL):: cal1, cal2, cal3, cal4, cal5
  type(DC_CAL_DATE):: date1, date2, date3, date4, date5, date6

  logical:: err

  character(STRING):: cal_type, date_str, zone
  integer:: month_in_year
  integer:: day_in_month12(1:12), day_in_month1(1:1)
  integer, pointer:: day_in_month_ptr(:) =>null()
  integer:: hour_in_day, min_in_hour
  real(DP):: sec_in_min

  integer:: year, month, day, hour, min
  real(DP):: sec, dday, dhour, dmin
  real(DP):: sec_of_year, day_of_year, sec_of_day
  logical:: leap_year

  integer:: i, unit_sym

  real(DP):: etime

continue

  call SetDebug

  ! 初期設定と問合せのテスト
  ! Check initialization and inquiry
  !
  call DCCalInquire
  call DCCalInquire( err = err )
  call AssertEqual('DCCalInquire test 0-1', .false.,  err )

  call DCCalCreate( 'gregorian', err = err )
  call AssertEqual('DCCalCreate test 1-1', .false., err )
  call DCCalCreate( 'hogehoge', cal = cal1, err = err )
  call AssertEqual('DCCalCreate test 1-2', .true., err )
  call DCCalCreate( '', err = err )
  call AssertEqual('DCCalCreate test 1-3', .true., err )

  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP, &
    & cal = cal1, err = err )
  call AssertEqual('DCCalCreate test 2-1', .false., err )
  call DCCalCreate( 3, (/ 30, 30, 30, 40/), 24, 60, 60.0_DP, &
    & cal = cal2, err = err )
  call AssertEqual('DCCalCreate test 2-2', .true., err )
  call DCCalCreate( 3, (/ 30, 30, 30/), 24, - 60, 60.0_DP, &
    & err = err )
  call AssertEqual('DCCalCreate test 2-3', .true., err )

  call DCCalInquire( err = err )
  call AssertEqual('DCCalInquire test 1-1', .false., err )
  call DCCalInquire( &
    & cal_type, month_in_year, day_in_month12, day_in_month_ptr, &
    & hour_in_day, min_in_hour, sec_in_min )
  call AssertEqual('DCCalInquire test 2-1', 'gregorian', cal_type )
  call AssertEqual('DCCalInquire test 2-2', 12, month_in_year )
  call AssertEqual('DCCalInquire test 2-3', &
    & (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /), day_in_month12 )
  call AssertEqual('DCCalInquire test 2-4', &
    & (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /), day_in_month_ptr )
  call AssertEqual('DCCalInquire test 2-5', 24, hour_in_day )
  call AssertEqual('DCCalInquire test 2-6', 60, min_in_hour )
  call AssertEqual('DCCalInquire test 2-7', 60.0_DP, sec_in_min )

  call DCCalInquire( &
    & cal_type, month_in_year, day_in_month1, day_in_month_ptr, &
    & hour_in_day, min_in_hour, sec_in_min, cal = cal1 )
  call AssertEqual('DCCalInquire test 3-1', 'user_defined', cal_type )
  call AssertEqual('DCCalInquire test 3-2', 1, month_in_year )
  call AssertEqual('DCCalInquire test 3-3', (/ 669 /), day_in_month1 )
  call AssertEqual('DCCalInquire test 3-4', (/ 669 /), day_in_month_ptr )
  call AssertEqual('DCCalInquire test 3-5', 24, hour_in_day )
  call AssertEqual('DCCalInquire test 3-6', 1, min_in_hour )
  call AssertEqual('DCCalInquire test 3-7', 3694.0_DP, sec_in_min )

  call DCCalInquire( cal = cal2, err = err )
  call AssertEqual('DCCalInquire test 4-1', .true., err )


  call DCCalDateCreate( 2009, 10, 25, 2, 45, 30.0_DP, err = err )
  call AssertEqual('DCCalDateCreate test 1-1', .false., err )

  call DCCalDateCreate( 2009, 10, 25, 2, 45, 30.0_DP, zone = '+09:00', date = date1, err = err )
  call AssertEqual('DCCalDateCreate test 1-2', .false., err )
  call DCCalDateInquire( date = date1, zone = zone )
  call AssertEqual('DCCalDateCreate/Inquire test 1-3', '+09:00', zone )

  call DCCalDateCreate( "s since 2009-06-17T11:23:45+09:00", date1, err = err )
  call AssertEqual('DCCalDateCreate test 2-1', .false., err )
  call DCCalDateInquire( year, month, day, hour, min, sec, &
    &                    date = date1 )
  call AssertEqual('DCCalDateCreate/Inquire test 2-2', 2009, year )
  call AssertEqual('DCCalDateCreate/Inquire test 2-3', 6, month )
  call AssertEqual('DCCalDateCreate/Inquire test 2-4', 17, day )
  call AssertEqual('DCCalDateCreate/Inquire test 2-5', 11, hour )
  call AssertEqual('DCCalDateCreate/Inquire test 2-6', 23, min )
  call AssertEqual('DCCalDateCreate/Inquire test 2-7', 45.0_DP, sec )

  call DCCalDateCreate( "122010-01-308T12:00:1155-00:00", date = date1 )
  call AssertEqual('DCCalDateCreate test 2-8', .false., err )
  call DCCalDateInquire( date_str, date = date1, cal = cal1 )
  call AssertEqual('DCCalDateCreate/Inquire test 2-9', '122010-01-308T12:00:1155-00:00', date_str )

  call DCCalDateCreate( "2009-12-02T03:01:00", err = err )
  call AssertEqual('DCCalDateCreate test 3-1', .false., err )

  call DCCalDateCreate( "seconds since 22-10-8 15:16:42.5432 -6:00", err = err )
  call AssertEqual('DCCalDateCreate test 4-1', .false., err )
  call DCCalDateInquire( year, month, day, hour, min, sec, zone )
  call AssertEqual('DCCalDateCreate/Inquire test 4-2', 22, year )
  call AssertEqual('DCCalDateCreate/Inquire test 4-3', 10, month )
  call AssertEqual('DCCalDateCreate/Inquire test 4-4', 8, day )
  call AssertEqual('DCCalDateCreate/Inquire test 4-5', 15, hour )
  call AssertEqual('DCCalDateCreate/Inquire test 4-6', 16, min )
  call AssertEqual('DCCalDateCreate/Inquire test 4-6', 42.5432_DP, sec )
  call AssertEqual('DCCalDateCreate/Inquire test 4-7', '-6:00', zone )

  call DCCalDateCreate( "days since 8-1-1 0:1:2.2", err = err )
  call AssertEqual('DCCalDateCreate test 5-1', .false., err )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateCreate/Inquire test 5-2', '0008-01-01T00:01:02.2', date_str )

  call DCCalDateCreate( "days since 2008-11 00:0", err = err )
  call AssertEqual('DCCalDateCreate test 6-1', .true., err )


  call DCCalCreate( 'noleap' )
  call DCCalDateCreate( "2009-12-02T13:50:50.5-13:00", date1, err = err )
  call DCCalDateInquire( year, month, day, hour, min, sec, &
    &                    elapse_sec = 86436010.5_DP, date = date1 )
  call AssertEqual('DCCalDateInquire test 7-2', 2012, year )
  call AssertEqual('DCCalDateInquire test 7-3', 8, month )
  call AssertEqual('DCCalDateInquire test 7-4', 29, day )
  call AssertEqual('DCCalDateInquire test 7-5', 23, hour )
  call AssertEqual('DCCalDateInquire test 7-6', 51, min )
  call AssertEqual('DCCalDateInquire test 7-7', 1.0_DP, sec )

  call DCCalDateInquire( date_str, 86436010.5_DP, date1 )
  call AssertEqual('DCCalDateInquire test 8-1', '2012-08-29T23:51:01-13:00', date_str )

  call DCCalDateInquire( date_str, 86436010.5_DP, date1, cal = cal1, err = err )
  call AssertEqual('DCCalDateInquire test 8-2', .true., err )

  call DCCalDateCreate( "2009-1-02T13:0:50.5-13:00", date1 )
  call DCCalDateInquire( date_str, 86436010.5_DP, date1, cal = cal1 )
  call AssertEqual('DCCalDateInquire test 8-3', '2010-01-308T12:00:155-13:00', date_str )

  ! DCCalDefault のテスト
  ! Check "DCCalDefault"
  !
  call DCCalCreate( 'gregorian' )
  call DCCalDefault( cal1 )
  call DCCalInquire( cal_type, cal = cal1 )
  call AssertEqual('DCCalDefault test 1-1', 'gregorian', cal_type )

  call DCCalCreate( 'julian' )
  call DCCalDefault( cal1 )
  call DCCalInquire( cal_type, cal = cal1 )
  call AssertEqual('DCCalDefault test 1-2', 'julian', cal_type )

  call DCCalCreate( 1, (/ 660 /), 23, 1, 3594.0_DP )
  call DCCalDefault( cal1 )
  call DCCalInquire( &
    & cal_type, &
    & month_in_year = month_in_year, &
    & day_in_month_ptr = day_in_month_ptr, &
    & hour_in_day = hour_in_day, &
    & min_in_hour = min_in_hour, &
    & sec_in_min = sec_in_min, &
    & cal = cal1 )
  call AssertEqual('DCCalDefault test 2-1', 'user_defined', cal_type )
  call AssertEqual('DCCalDefault test 2-2', 1, month_in_year )
  call AssertEqual('DCCalDefault test 2-4', (/ 660 /), day_in_month_ptr )
  call AssertEqual('DCCalDefault test 2-5', 23, hour_in_day )
  call AssertEqual('DCCalDefault test 2-6', 1, min_in_hour )
  call AssertEqual('DCCalDefault test 2-7', 3594.0_DP, sec_in_min )


  ! Eval (経過時間と日時情報から経過時間後の日時情報の算出) のテスト
  ! Check "Eval" (evaluate date from initial date and elapse time)
  !
  call DCCalCreate( 'noleap' )
  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 360000.0_DP )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 1-1', '2009-10-29T06:45:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, -360000.0_DP, err = err )
  call AssertEqual('DCCalDateEval test 1-2', .false., err )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 864000000.0_DP, 'sec' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 2-1', '2037-03-19T02:45:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 14400000.0_DP, 'min' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 2-2', '2037-03-19T02:45:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 240000.0_DP, 'hrs' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 2-3', '2037-03-19T02:45:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 10000.0_DP, 'day' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 2-4', '2037-03-19T02:45:30', date_str )


  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 2700.0_DP, 'sec' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 3-1', '2009-10-25T03:30:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 45.0_DP, 'min' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 3-2', '2009-10-25T03:30:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP, 0.75_DP, 'hrs' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 3-3', '2009-10-25T03:30:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP,  0.03125_DP, 'day' )
  call DCCalDateInquire( date_str )
  call AssertEqual('DCCalDateEval test 3-4', '2009-10-25T03:30:30', date_str )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP,  0.03125_DP, 'month', err = err )
  call AssertEqual('DCCalDateEval test 4-1', .true., err )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP,  0.03125_DP, 'year', err = err )
  call AssertEqual('DCCalDateEval test 4-2', .true., err )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP,  0.03125_DP, 'hoge', err = err )
  call AssertEqual('DCCalDateEval test 4-3', .true., err )

  call DCCalDateEval( 2009, 10, 25, 2, 45, 30.0_DP,  -0.03125_DP, '', err = err )
  call AssertEqual('DCCalDateEval test 4-4', .true., err )



  call DCCalDateCreate( "2000-1-02T13:0:50.5+22:00", date1 )
  call DCCalDateEval( date1, 864000000.0_DP, date = date2 )
  call DCCalDateInquire( date_str, date = date2 )
  call AssertEqual('DCCalDateEval test 5-1', '2027-05-27T13:00:50.5+22:00', date_str )

  call DCCalDateCreate( "2000-1-02T13:0:50.5+22:00", date1 )
  call DCCalDateEval( date1, 10000.0_DP, 'days', date = date2 )
  call DCCalDateInquire( date_str, date = date2 )
  call AssertEqual('DCCalDateEval test 5-2', '2027-05-27T13:00:50.5+22:00', date_str )


  call DCCalDateEval( 2000, 1, 2, 13, 0, 50.5_DP,  &
    &                 864000000.0_DP, &
    &                 year, month, day, hour, min, sec )
  call AssertEqual('DCCalDateEval test 6-1', 2027, year )
  call AssertEqual('DCCalDateEval test 6-2', 5, month )
  call AssertEqual('DCCalDateEval test 6-3', 27, day )
  call AssertEqual('DCCalDateEval test 6-4', 13, hour )
  call AssertEqual('DCCalDateEval test 6-5', 0, min )
  call AssertEqual('DCCalDateEval test 6-6', 50.5_DP, sec )


  call DCCalDateEval( 2000, 1, 2, 13, 0, 50.5_DP,  &
    &                 240000.0_DP, 'hrs', &
    &                 year, month, day, hour, min, sec )
  call AssertEqual('DCCalDateEval test 7-1', 2027, year )
  call AssertEqual('DCCalDateEval test 7-2', 5, month )
  call AssertEqual('DCCalDateEval test 7-3', 27, day )
  call AssertEqual('DCCalDateEval test 7-4', 13, hour )
  call AssertEqual('DCCalDateEval test 7-5', 0, min )
  call AssertEqual('DCCalDateEval test 7-6', 50.5_DP, sec )


  ! 10 億年 (4 byte 整数のほぼ限界) まで積算できるかチェック
  ! Check integration to one billion year (almost limit of 4 byte integer)
  !
  call DCCalDateCreate( 1, 1, 2, 12, 2, 0.0_DP, date = date1 )
  call DCCalDateEval( date1, 3.1536e+16_DP, date = date2 )
  call DCCalDateInquire( date_str, date = date2 )
  call AssertEqual('Integral to one billion year test 1-1', '1000000001-01-02T12:02:00', date_str )

  call DCCalDateEval( date1, 3.1536e+11_DP, date = date2 )
  call DCCalDateInquire( date_str, date = date2 )
  call AssertEqual('Integral to one billion year test 1-2', '10001-01-02T12:02:00', date_str )

  call DCCalDateEval( date1, 3.1536e+9_DP, date = date2 )
  call DCCalDateInquire( date_str, date = date2 )
  call AssertEqual('Integral to one billion year test 1-3', '0101-01-02T12:02:00', date_str )

  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP, cal = cal1 )
  call DCCalDateEval( date1, 3.1536e+16_DP, date = date2, cal = cal1 )
  call DCCalDateInquire( year, month, day, hour, min, sec, &
    &                    date = date2, cal = cal1, &
    &                    err = err )
  call AssertEqual('Integral to one billion year test 2-1', 531706974, year )
  call AssertEqual('Integral to one billion year test 2-2', 1, month )
  call AssertEqual('Integral to one billion year test 2-3', 414, day )
  call AssertEqual('Integral to one billion year test 2-4', 19, hour )


  ! 単位変換のチェック 
  ! Check conversion of units
  !
  sec = DCCalConvertByUnit( 1.0_DP, 'day', 'sec' )
  call AssertEqual('DCCalConvertByUnit test 1-1', 86400.0_DP, sec )

  dhour = DCCalConvertByUnit( 86400.0_DP, 'sec', 'hrs' )
  call AssertEqual('DCCalConvertByUnit test 1-2', 24.0_DP, dhour )

  sec = DCCalConvertByUnit( 1.0_DP, UNIT_SYMBOL_DAY, UNIT_SYMBOL_SEC )
  call AssertEqual('DCCalConvertByUnit test 1-3', 86400.0_DP, sec )

  dhour = DCCalConvertByUnit( 86400.0_DP, UNIT_SYMBOL_SEC, UNIT_SYMBOL_HOUR )
  call AssertEqual('DCCalConvertByUnit test 1-4', 24.0_DP, dhour )

  sec = DCCalConvertByUnit( 1.0_DP, 'day', 'sec', cal = cal1 )
  call AssertEqual('DCCalConvertByUnit test 2-1', 88656.0_DP, sec )

  dhour = DCCalConvertByUnit( 88656.0_DP, 'sec', 'hours', cal = cal1 )
  call AssertEqual('DCCalConvertByUnit test 2-2', 24.0_DP, dhour )

  dmin = DCCalConvertByUnit( 88656.0_DP, 'sec', 'min', cal = cal1 )
  call AssertEqual('DCCalConvertByUnit test 2-3', 24.0_DP, dmin )

  dhour = DCCalConvertByUnit( 1.0_DP, 'min', 'hrs', cal = cal1 )
  call AssertEqual('DCCalConvertByUnit test 2-4', 1.0_DP, dhour )

  dday = DCCalConvertByUnit( 24.0_DP, 'min', 'day', cal = cal1 )
  call AssertEqual('DCCalConvertByUnit test 2-5', 1.0_DP, dday )

  call DCCalParseUnit( 'hoge', unit_sym, err = err )
  call AssertEqual('DCCalParseUnit test 1-1', .true., err )

  call DCCalParseUnit( 'day', unit_sym )
  call AssertEqual('DCCalParseUnit test 2-1', UNIT_SYMBOL_DAY, unit_sym )

  call DCCalParseUnit( 'sec', unit_sym )
  call AssertEqual('DCCalParseUnit test 2-2', UNIT_SYMBOL_SEC, unit_sym )

  call DCCalParseUnit( 'hrs', unit_sym )
  call AssertEqual('DCCalParseUnit test 2-3', UNIT_SYMBOL_HOUR, unit_sym )

  sec = DCCalConvertToSec( 1.0_DP, 'day' )
  call AssertEqual('DCCalConvertToSec test 1-1', 86400.0_DP, sec )

  dmin = DCCalConvertToMin( 24.0_DP, 'hrs' )
  call AssertEqual('DCCalConvertToMin test 1-1', 1440.0_DP, dmin )

  dhour = DCCalConvertToHour( 86400.0_DP, 'sec' )
  call AssertEqual('DCCalConvertToHour test 1-1', 24.0_DP, dhour )

  dday = DCCalConvertToDay( 14400.0_DP, 'min' )
  call AssertEqual('DCCalConvertToDay test 1-1', 10.0_DP, dday )

  ! 差分計算のチェック
  ! Check calculate difference
  !
  call DCCalDateCreate( "1999-06-17T11:23:45+09:00", date1 )
  call DCCalDateCreate( "2009-06-18T12:24:46+09:00", date2 )
  sec = DCCalDateDifference( date1, date2 )
  call AssertEqual('DCCalDateDifference test 1-1', 315450061.0_DP, sec )

  call DCCalDateCreate( "2009-06-17T11:23:45+09:00", date1 )
  call DCCalDateCreate( "2009-07-17T11:23:45+09:00", date2 )
  sec = DCCalDateDifference( date1, date2 )
  call AssertEqual('DCCalDateDifference test 1-2', 2592000.0_DP, sec )

  call DCCalDateCreate( "2009-01-17T11:0:450", date1 )
  call DCCalDateCreate( "2019-01-17T11:0:450", date2 )
  sec = DCCalDateDifference( date1, date2, cal1 )
  call AssertEqual('DCCalDateDifference test 2-1', 593108640.0_DP, sec )

  call DCCalCreate( 'gregorian', cal = cal2 )
  call DCCalDateCreate( "2001-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2002-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 3-1', 365.0_DP * 86400, sec )

  call DCCalCreate( 'gregorian', cal = cal2 )
  call DCCalDateCreate( "1999-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2000-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 3-2', 365.0_DP * 86400, sec )

  call DCCalCreate( 'gregorian', cal = cal2 )
  call DCCalDateCreate( "2000-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2001-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 3-3', 366.0_DP * 86400, sec )

  call DCCalCreate( 'gregorian', cal = cal2 )
  call DCCalDateCreate( "2100-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2101-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 3-4', 365.0_DP * 86400, sec )

  call DCCalCreate( 'julian', cal = cal2 )
  call DCCalDateCreate( "2001-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2002-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 4-1', 365.0_DP * 86400, sec )

  call DCCalCreate( 'julian', cal = cal2 )
  call DCCalDateCreate( "1999-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2000-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 4-2', 365.0_DP * 86400, sec )

  call DCCalCreate( 'julian', cal = cal2 )
  call DCCalDateCreate( "2000-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2001-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 4-3', 366.0_DP * 86400, sec )

  call DCCalCreate( 'julian', cal = cal2 )
  call DCCalDateCreate( "2100-01-1T0:0:0", date1 )
  call DCCalDateCreate( "2101-01-1T0:0:0", date2 )
  sec = DCCalDateDifference( date1, date2, cal = cal2 )
  call AssertEqual('DCCalDateDifference test 4-4', 366.0_DP * 86400, sec )

  ! 既定の暦の動作チェック
  ! Check operations of previously-defined calendars
  !
  call DCCalCreate( 'cyclic', cal = cal3 )
  call DCCalDateCreate( "1-1-1T0:0:0", date3 )
  call DCCalDateEval( date3, 367.0_DP * 400.0_DP * 86400.0_DP, cal3, date4 )
  call DCCalDateInquire( date_str, date = date4 )
  call AssertEqual('Cyclic calendar test 1-1', '0401-01-01T00:00:00', date_str )
  call DCCalDateCreate( "2009-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2009-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Cyclic calendar test 2-1', 86400.0_DP * 3, sec )

  call DCCalDateCreate( "2012-02-1T0:0:0", date3 )
  call DCCalDateCreate( "2012-08-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Cyclic calendar test 2-2', 86400.0_DP * 183, sec )

  call DCCalDateCreate( "1-01-1T10:30:50", date3 )
  call DCCalDateCreate( "40001-01-01T10:30:50", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Cyclic calendar test 2-5', 367 * 40000 * 86400.0_DP, sec )


  call DCCalCreate( 'noleap', cal = cal3 )
  call DCCalDateCreate( "1-1-1T0:0:0", date3 )
  call DCCalDateEval( date3, 365.0_DP * 400.0_DP * 86400.0_DP, cal3, date4 )
  call DCCalDateInquire( date_str, date = date4 )
  call AssertEqual('Noleap calendar test 1-1', '0401-01-01T00:00:00', date_str )

  call DCCalDateCreate( "2009-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2009-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Noleap calendar test 2-1', 86400.0_DP, sec )

  call DCCalDateCreate( "2012-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2012-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Noleap calendar test 2-2', 86400.0_DP, sec )

  call DCCalDateCreate( "2100-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2100-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Noleap calendar test 2-3', 86400.0_DP, sec )

  call DCCalDateCreate( "2400-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2400-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Noleap calendar test 2-4', 86400.0_DP, sec )

  call DCCalDateCreate( "1-01-1T10:30:50", date3 )
  call DCCalDateCreate( "40001-01-01T10:30:50", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Noleap calendar test 2-5', 146000 * 100 * 86400.0_DP, sec )


  call DCCalCreate( 'julian', cal = cal3 )
  call DCCalDateCreate( "1-1-1T0:0:0", date3 )
  call DCCalDateEval( date3, 146100.0_DP * 86400.0_DP, cal3, date4 )
  call DCCalDateInquire( date_str, date = date4 )
  call AssertEqual('Julian calendar test 1-1', '0401-01-01T00:00:00', date_str )

  call DCCalDateCreate( "2009-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2009-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Julian calendar test 2-1', 86400.0_DP, sec )

  call DCCalDateCreate( "2012-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2012-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Julian calendar test 2-2', 86400.0_DP * 2, sec )

  call DCCalDateCreate( "2100-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2100-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Julian calendar test 2-3', 86400.0_DP * 2, sec )

  call DCCalDateCreate( "2400-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2400-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Julian calendar test 2-4', 86400.0_DP * 2, sec )

  call DCCalDateCreate( "1-01-1T10:30:50", date3 )
  call DCCalDateCreate( "40001-01-01T10:30:50", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Julian calendar test 2-5', 146100 * 100 * 86400.0_DP, sec )


  call DCCalCreate( 'gregorian', cal = cal3 )
  call DCCalDateCreate( "1-1-1T0:0:0", date3 )
  call DCCalDateEval( date3, 146097.0_DP * 86400.0_DP, cal3, date4 )
  call DCCalDateInquire( date_str, date = date4, cal = cal3 )
  call AssertEqual('Gregorian calendar test 1-1', '0401-01-01T00:00:00', date_str )

  call DCCalDateCreate( "2009-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2009-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Gregorian calendar test 2-1', 86400.0_DP, sec )

  call DCCalDateCreate( "2012-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2012-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Gregorian calendar test 2-2', 86400.0_DP * 2, sec )

  call DCCalDateCreate( "2100-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2100-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Gregorian calendar test 2-3', 86400.0_DP, sec )

  call DCCalDateCreate( "2400-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2400-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Gregorian calendar test 2-4', 86400.0_DP * 2, sec )

  call DCCalDateCreate( "1-01-1T10:30:50", date3 )
  call DCCalDateCreate( "40001-01-01T10:30:50", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('Gregorian calendar test 2-5', 146097 * 100 * 86400.0_DP, sec )

  call DCCalCreate( '360day', cal = cal3 )
  call DCCalDateCreate( "1-1-1T0:0:0", date3 )
  call DCCalDateEval( date3, 360.0_DP * 400.0_DP * 86400.0_DP, cal3, date4 )
  call DCCalDateInquire( date_str, date = date4 )
  call AssertEqual('360day calendar test 1-1', '0401-01-01T00:00:00', date_str )

  call DCCalDateCreate( "2009-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2009-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('360day calendar test 2-1', 86400.0_DP * 3, sec )

  call DCCalDateCreate( "2012-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2012-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('360day calendar test 2-2', 86400.0_DP * 3, sec )

  call DCCalDateCreate( "2100-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2100-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('360day calendar test 2-3', 86400.0_DP * 3, sec )

  call DCCalDateCreate( "2400-02-28T0:0:0", date3 )
  call DCCalDateCreate( "2400-03-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('360day calendar test 2-4', 86400.0_DP * 3, sec )

  call DCCalDateCreate( "1-01-1T10:30:50", date3 )
  call DCCalDateCreate( "40001-01-01T10:30:50", date4 )
  sec = DCCalDateDifference( date3, date4, cal3 )
  call AssertEqual('360day calendar test 2-5', 144000 * 100 * 86400.0_DP, sec )


  ! 通日 (ある年の何日目か) の算出チェック
  ! Check evaluation of day of year
  !
  call DCCalCreate( 'gregorian', cal = cal3 )
  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP, cal = cal4 )
  call DCCalCreate( 'julian', cal = cal5 )
  call DCCalDateCreate( "2000-01-01T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('DayOfYear (Gregorian) test 1-1', 1.0_DP, day_of_year )

  call DCCalDateCreate( "2000-02-01T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('DayOfYear (Gregorian) test 1-2', 32.0_DP, day_of_year )

  call DCCalDateCreate( "2000-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('DayOfYear (Gregorian) test 1-3', 366.0_DP, day_of_year )

  call DCCalDateCreate( "1999-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('DayOfYear (Gregorian) test 1-4', 365.0_DP, day_of_year )

  call DCCalDateCreate( "1996-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('DayOfYear (Gregorian) test 1-5', 366.0_DP, day_of_year )

  call DCCalDateCreate( "2100-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('DayOfYear (Gregorian) test 1-6', 365.0_DP, day_of_year )

  call DCCalDateCreate( "2000-12-31T12:00:00", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 43200.0_DP, &    ! (in)
    &                          date3, cal3 )    ! (in) optional
  call AssertEqual('DayOfYear (Gregorian) test 1-7', 1.0_DP, day_of_year )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('DayOfYear (Martian Cal) test 1-1', 1.0_DP, day_of_year )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 3000.0_DP * 10, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('DayOfYear (Martian Cal) test 1-2', 1.0_DP, day_of_year )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 3000.0_DP * 20, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('DayOfYear (Martian Cal) test 1-3', 2.0_DP, day_of_year )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 3694.0_DP * 24 * 669, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('DayOfYear (Martian Cal) test 1-4', 1.0_DP, day_of_year )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 3694.0_DP * 24 * 668, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('DayOfYear (Martian Cal) test 1-5', 669.0_DP, day_of_year )

  call DCCalDateCreate( "2000-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('DayOfYear (Julian) test 1-1', 366.0_DP, day_of_year )

  call DCCalDateCreate( "2100-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('DayOfYear (Julian) test 1-2', 366.0_DP, day_of_year )

  call DCCalDateCreate( "1996-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('DayOfYear (Julian) test 1-3', 366.0_DP, day_of_year )

  call DCCalDateCreate( "1994-12-31T10:30:50", date3 )
  day_of_year = DCCalDateEvalDayOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('DayOfYear (Julian) test 1-4', 365.0_DP, day_of_year )


  ! 通秒 (ある年が始まって何秒経過したか) の算出チェック
  ! Check evaluation of sec of year
  !
  call DCCalCreate( 'gregorian', cal = cal3 )
  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP, cal = cal4 )
  call DCCalCreate( 'julian', cal = cal5 )
  call DCCalDateCreate( "2000-01-01T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('SecOfYear (Gregorian) test 1-1', 10 * 3600 + 30 * 60 + 50.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-02-01T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('SecOfYear (Gregorian) test 1-2', 31.0_DP * 86400 + 10 * 3600 + 30 * 60 + 50.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-12-31T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('SecOfYear (Gregorian) test 1-3', 365.0_DP * 86400 + 10 * 3600 + 30 * 60 + 50.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-12-31T12:00:00", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 43200.0_DP, &    ! (in)
    &                          date3, cal3 )    ! (in) optional
  call AssertEqual('SecOfYear (Gregorian) test 1-4', 0.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfYear (Martian Cal) test 1-1', 10.0_DP * 3694 + 50, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 3000.0_DP * 10, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfYear (Martian Cal) test 1-2', 10.0_DP * 3694 + 50 + 3000 * 10, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 3000.0_DP * 20, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfYear (Martian Cal) test 1-3', 10.0_DP * 3694 + 50 + 3000 * 20, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 3694.0_DP * 24 * 669, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfYear (Martian Cal) test 1-4', 10.0_DP * 3694 + 50, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 3694.0_DP * 24 * 668, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfYear (Martian Cal) test 1-5', 3694 * 24 * 668.0_DP + 10.0_DP * 3694 + 50, sec_of_day )

  call DCCalDateCreate( "2000-12-31T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('SecOfYear (Julian) test 1-1', 365.0_DP * 86400 + 37850, sec_of_day )

  call DCCalDateCreate( "2100-12-31T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('SecOfYear (Julian) test 1-2', 365.0_DP * 86400 + 37850, sec_of_day )

  call DCCalDateCreate( "1996-12-31T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('SecOfYear (Julian) test 1-3', 365.0_DP * 86400 + 37850, sec_of_day )

  call DCCalDateCreate( "1994-12-31T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfYear( 0.0_DP, &      ! (in)
    &                          date3, cal5 ) ! (in) optional
  call AssertEqual('SecOfYear (Julian) test 1-4', 364.0_DP * 86400 + 37850, sec_of_day )


  ! 通秒 (ある日が始まって何秒経過したか) の算出チェック
  ! Check evaluation of sec of day 
  !
  call DCCalCreate( 'gregorian', cal = cal3 )
  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP, cal = cal4 )
  call DCCalDateCreate( "2000-01-10T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 0.0_DP, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('SecOfDay (Gregorian) test 1-1', 10 * 3600 + 30 * 60 + 50.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-02-01T10:30:50", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 3600.0_DP * 14, &      ! (in)
    &                          date3, cal3 ) ! (in) optional
  call AssertEqual('SecOfDay (Gregorian) test 1-2', 30 * 60 + 50.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-12-31T11:59:59", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 3600.0_DP * 12 + 1, &      ! (in)
    &                        date3, cal3 ) ! (in) optional
  call AssertEqual('SecOfDay (Gregorian) test 1-3', 0.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-12-31T11:59:59", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 3600.0_DP * 12, &      ! (in)
    &                        date3, cal3 ) ! (in) optional
  call AssertEqual('SecOfDay (Gregorian) test 1-4', 86399.0_DP, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 0.0_DP, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfDay (Martian Cal) test 1-1', 10.0_DP * 3694 + 50, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 3000.0_DP * 10, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfDay (Martian Cal) test 1-2', 10.0_DP * 3694 + 50 + 3000 * 10, sec_of_day )

  call DCCalDateCreate( "2000-01-01T10:0:50", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 3000.0_DP * 20, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfDay (Martian Cal) test 1-3', 10.0_DP * 3694 + 50 + 3000 * 20 - 3694 * 24, sec_of_day )

  call DCCalDateCreate( "2000-01-01T0:0:00", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 3694.0_DP * 24 - 1, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfDay (Martian Cal) test 1-4', 3694.0_DP * 24 - 1, sec_of_day )

  call DCCalDateCreate( "2000-01-01T12:0:00", date3 )
  sec_of_day = DCCalDateEvalSecOfDay( 3694.0_DP * 12, &      ! (in)
    &                          date3, cal4 ) ! (in) optional
  call AssertEqual('SecOfDay (Martian Cal) test 1-5', 0.0_DP, sec_of_day )


  ! 閏年かどうかの判定のチェック
  ! Check judgement of leap year
  !
  call DCCalCreate( 'gregorian', cal = cal3 )
  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP, cal = cal4 )
  call DCCalCreate( 'julian', cal = cal5 )

  call DCCalDateCreate( "2000-01-01T12:0:00", date3 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, &   ! (in)
    &                   date3, cal3 )       ! (in) optional
  call AssertEqual('DCCalDateChkLeapYear (Gregorian) test 1-1', .true., leap_year )

  call DCCalDateCreate( "2000-01-01T12:0:00", date3 )
  leap_year = DCCalDateChkLeapYear( 86400.0_DP * 366, &   ! (in)
    &                   date3, cal3 )       ! (in) optional
  call AssertEqual('DCCalDateChkLeapYear (Gregorian) test 1-2', .false., leap_year )

  call DCCalDateCreate( "2100-01-01T12:0:00", date3 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, &   ! (in)
    &                   date3, cal3 )       ! (in) optional
  call AssertEqual('DCCalDateChkLeapYear (Gregorian) test 1-3', .false., leap_year )

  call DCCalDateCreate( "2000-01-01T12:0:00", date3 )
  leap_year = DCCalDateChkLeapYear( 3694.0_DP * 12, &   ! (in)
    &                   date3, cal4 )       ! (in) optional
  call AssertEqual('DCCalDateChkLeapYear (Martian Cal) test 1-1', .false., leap_year )

  call DCCalDateCreate( "2000-01-01T12:0:00", date3 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, &   ! (in)
    &                   date3, cal5 )       ! (in) optional
  call AssertEqual('DCCalDateChkLeapYear (Julian) test 1-1', .true., leap_year )

  call DCCalDateCreate( "2000-01-01T12:0:00", date3 )
  leap_year = DCCalDateChkLeapYear( 86400.0_DP * 366, &   ! (in)
    &                   date3, cal5 )       ! (in) optional
  call AssertEqual('DCCalDateChkLeapYear (Julian) test 1-2', .false., leap_year )

  call DCCalDateCreate( "2100-01-01T12:0:00", date3 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, &   ! (in)
    &                   date3, cal5 )       ! (in) optional
  call AssertEqual('DCCalDateChkLeapYear (Julian) test 1-3', .true., leap_year )


  ! 現在時刻の取得
  ! Get current time
  !
  call DCCalDateCurrent( date1 ) 
  call DCCalDateInquire( date_str, date = date1 )
  call MessageNotify('M', 'dc_calender_test', 'Current time is <%c>', c1 = trim(date_str) )

  ! グレゴリオ暦 (閏年有り) の計算のチェック
  ! Check calculation about Gregorian calendar (with leap year)
  !
  call DCCalCreate( cal_type = 'Gregorian' )
  call DCCalDateCreate( year = 1900, month = 1, day = 1, &
    &                   hour =   0,  min   = 0, sec = 0.0_DP, &
    &                   date = date1 )

  call DCCalDateCreate( year = 1912, month = 1, day = 1, &
    &                   hour =   0,  min   = 0, sec = 0.0_DP, &
    &                   date = date2 )

  call DCCalDateCreate( year = 2000, month = 1, day = 1, &
    &                   hour =   0,  min   = 0, sec = 0.0_DP, &
    &                   date = date3 )

  call DCCalDateCreate( year = 2004, month = 1, day = 1, &
    &                   hour =   0,  min   = 0, sec = 0.0_DP, &
    &                   date = date4 )

  call DCCalDateCreate( year = 2009, month = 1, day = 1, &
    &                   hour =   0,  min   = 0, sec = 0.0_DP, &
    &                   date = date5 )

  call DCCalDateCreate( year = 2012, month = 1, day = 1, &
    &                   hour =   0,  min   = 0, sec = 0.0_DP, &
    &                   date = date6 )

  etime = DCCalConvertByUnit(  365.0_DP, 'day', 'sec' )

  call DCCalDateInquire( date_str, etime, date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Calculation about Gregorian test 1-1', .false., leap_year )
  call AssertEqual('Calculation about Gregorian test 1-2', &
    & '1901-01-01T00:00:00', date_str )

  call DCCalDateInquire( date_str, etime, date2 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date2 )
  call AssertEqual('Calculation about Gregorian test 2-1', .true., leap_year )
  call AssertEqual('Calculation about Gregorian test 2-2', &
    & '1912-12-31T00:00:00', date_str )

  call DCCalDateInquire( date_str, etime, date3 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date3 )
  call AssertEqual('Calculation about Gregorian test 3-1', .true., leap_year )
  call AssertEqual('Calculation about Gregorian test 3-2', &
    & '2000-12-31T00:00:00', date_str )

  call DCCalDateInquire( date_str, etime, date4 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date4 )
  call AssertEqual('Calculation about Gregorian test 4-1', .true., leap_year )
  call AssertEqual('Calculation about Gregorian test 4-2', &
    & '2004-12-31T00:00:00', date_str )

  call DCCalDateInquire( date_str, etime, date5 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date5 )
  call AssertEqual('Calculation about Gregorian test 5-1', .false., leap_year )
  call AssertEqual('Calculation about Gregorian test 5-2', &
    & '2010-01-01T00:00:00', date_str )

  call DCCalDateInquire( date_str, etime, date6 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date6 )
  call AssertEqual('Calculation about Gregorian test 6-1', .true., leap_year )
  call AssertEqual('Calculation about Gregorian test 6-2', &
    & '2012-12-31T00:00:00', date_str )


  ! 負の elapsed_time の扱いのチェック
  ! Check handling of negative "elapse_time"
  !
  call DCCalCreate( 'gregorian' )
  call DCCalDateCreate( "2009-06-17T11:23:45+09:00", date1 )
  call DCCalDateInquire( date_str, -10.0_DP, date1 )
  call AssertEqual('Handling of negative elapse_time test 1-1', &
    & '2009-06-17T11:23:35+09:00', date_str )

  call DCCalDateInquire( year, month, day, hour, min, sec, &
    &                    elapse_sec = - 86400.0_DP - 3600.0_DP - 60.0_DP, &
    &                    date = date1 )
  call AssertEqual('Handling of negative elapse_time test 2-1', 2009, year )
  call AssertEqual('Handling of negative elapse_time test 2-2', 6, month )
  call AssertEqual('Handling of negative elapse_time test 2-3', 16, day )
  call AssertEqual('Handling of negative elapse_time test 2-4', 10, hour )
  call AssertEqual('Handling of negative elapse_time test 2-5', 22, min )
  call AssertEqual('Handling of negative elapse_time test 2-6', 45.0_DP, sec )

  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP, cal = cal1 )
  call DCCalDateCreate( "2009-01-17T11:0:45", date1 )
  call DCCalDateInquire( date_str, -10.0_DP, date1, cal = cal1 )
  call AssertEqual('Handling of negative elapse_time test 3-1', &
    & '2009-01-17T11:00:35', date_str )

  call DCCalDateInquire( year, month, day, hour, min, sec, &
    &                    elapse_sec = - 3694.0_DP * 49 - 100.0_DP, &
    &                    date = date1, cal = cal1 )
  call AssertEqual('Handling of negative elapse_time test 4-1', 2009, year )
  call AssertEqual('Handling of negative elapse_time test 4-2', 1, month )
  call AssertEqual('Handling of negative elapse_time test 4-3', 15, day )
  call AssertEqual('Handling of negative elapse_time test 4-4', 9, hour )
  call AssertEqual('Handling of negative elapse_time test 4-5', 0, min )
  call AssertEqual('Handling of negative elapse_time test 4-6', 3639.0_DP, sec )

  call DCCalCreate( 'gregorian' )
  year = 2009; month = 6; day = 17; hour = 11; min = 23; sec = 45
  call DCCalDateEval( &
    & 2009, 6, 17, 11, 23, 45.0_DP, &
    & elapse_sec = - 86400.0_DP * 365, date = date1 )
  call DCCalDateInquire( date_str, date = date1 )
  call AssertEqual('Handling of negative elapse_time test 5-1', &
    & '2008-06-17T11:23:45', date_str )

  call DCCalDateEval( &
    & 2008, 6, 17, 11, 23, 45.0_DP, &
    & elapse_sec = - 86400.0_DP * 365, date = date1 )
  call DCCalDateInquire( date_str, date = date1 )
  call AssertEqual('Handling of negative elapse_time test 5-2', &
    & '2007-06-18T11:23:45', date_str )

  call DCCalDateEval( &
    & 2000, 1, 1, 1, 23, 45.0_DP, &
    & elapse_time = - 365.0_DP * 300, units = 'day', date = date1 )
  call DCCalDateInquire( date_str, date = date1 )
  call AssertEqual('Handling of negative elapse_time test 5-3', &
    & '1700-03-14T01:23:45', date_str )

  call DCCalCreate( 'noleap' )
  call DCCalDateEval( &
    & 2000, 1, 1, 1, 23, 45.0_DP, &
    & elapse_time = - 365.0_DP * 300, units = 'day', date = date1 )
  call DCCalDateInquire( date_str, date = date1 )
  call AssertEqual('Handling of negative elapse_time test 6-1', &
    & '1700-01-01T01:23:45', date_str )

  call DCCalCreate( 'julian' )
  call DCCalDateCreate( "2001-01-01T10:30:50", date3 )
  call DCCalDateEval( date3, - 86400.0_DP * 365, date = date4 )
  call DCCalDateInquire( date_str, date = date4 )
  call AssertEqual('Handling of negative elapse_time test 7-1', &
    & '2000-01-02T10:30:50', date_str )

  call DCCalCreate( '360day' )
  call DCCalDateCreate( "2001-01-01T10:30:50", date3 )
  call DCCalDateEval( date3, - 360.0_DP * 300, 'day', date = date4 )
  call DCCalDateInquire( date_str, date = date4 )
  call AssertEqual('Handling of negative elapse_time test 8-1', &
    & '1701-01-01T10:30:50', date_str )

  call DCCalDateEval( &
    & 2000, 1, 100, 12, 0, 2000.0_DP, &
    & - 100 * 3694.0_DP * 24 * 669, &
    & year, month, day, hour, min, sec, &
    & cal = cal1 )
  call AssertEqual('Handling of negative elapse_time test 9-1', 1900, year )
  call AssertEqual('Handling of negative elapse_time test 9-2', 1, month )
  call AssertEqual('Handling of negative elapse_time test 9-3', 100, day )
  call AssertEqual('Handling of negative elapse_time test 9-4', 12, hour )
  call AssertEqual('Handling of negative elapse_time test 9-5', 0, min )
  call AssertEqual('Handling of negative elapse_time test 9-6', 2000.0_DP, sec )

  call DCCalDateEval( &
    & 2000, 1, 100, 12, 0, 2000.0_DP, &
    & - 200.0_DP * 669, 'day', &
    & year, month, day, hour, min, sec, &
    & cal = cal1 )
  call AssertEqual('Handling of negative elapse_time test 10-1', 1800, year )
  call AssertEqual('Handling of negative elapse_time test 10-2', 1, month )
  call AssertEqual('Handling of negative elapse_time test 10-3', 100, day )
  call AssertEqual('Handling of negative elapse_time test 10-4', 12, hour )
  call AssertEqual('Handling of negative elapse_time test 10-5', 0, min )
  call AssertEqual('Handling of negative elapse_time test 10-6', 2000.0_DP, sec )

  call DCCalCreate( 'gregorian' )
  call DCCalDateCreate( "1996-12-31T10:30:50", date1 )
  sec_of_year = DCCalDateEvalSecOfYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 11-1', &
    & 364 * 86400 + 10 * 3600 + 30 * 60 + 50.0_DP, sec_of_year )

  call DCCalDateCreate( "2000-12-31T10:30:50", date1 )
  sec_of_year = DCCalDateEvalSecOfYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 11-2', &
    & 364 * 86400 + 10 * 3600 + 30 * 60 + 50.0_DP, sec_of_year )

  call DCCalDateCreate( "2100-12-31T10:30:50", date1 )
  sec_of_year = DCCalDateEvalSecOfYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 11-3', &
    & 363 * 86400 + 10 * 3600 + 30 * 60 + 50.0_DP, sec_of_year )

  call DCCalCreate( 'julian' )
  call DCCalDateCreate( "1996-12-31T10:30:50", date1 )
  day_of_year = DCCalDateEvalDayOfYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 12-1', &
    & 365.0_DP, day_of_year )

  call DCCalDateCreate( "2000-12-31T10:30:50", date1 )
  day_of_year = DCCalDateEvalDayOfYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 12-2', &
    & 365.0_DP, day_of_year )

  call DCCalDateCreate( "2100-12-31T10:30:50", date1 )
  day_of_year = DCCalDateEvalDayOfYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 12-3', &
    & 365.0_DP, day_of_year )

  call DCCalDateCreate( "2099-12-31T10:30:50", date1 )
  day_of_year = DCCalDateEvalDayOfYear( - 86400.0_DP * 365, date1 )
  call AssertEqual('Handling of negative elapse_time test 12-4', &
    & 365.0_DP, day_of_year )


  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP )
  call DCCalDateCreate( "2000-1-669T12:0:2000", date1 )
  sec_of_day = DCCalDateEvalSecOfDay( - 3694.0_DP * 13, date1 )
  call AssertEqual('Handling of negative elapse_time test 13-1', &
    & 23 * 3694.0_DP + 2000 , sec_of_day )

  call DCCalCreate( 'gregorian' )
  call DCCalDateCreate( "1996-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 14-1', .false., leap_year )

  call DCCalDateCreate( "1997-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative elapse_time test 14-2', .true., leap_year )


  ! 負の年のチェック
  ! Check handling of negative year
  !
  call DCCalCreate( 'gregorian' )
  call DCCalDateCreate( - 1230, 10, 25, 2, 45, 30.0_DP, date1 )

  call DCCalDateInquire( date_str, date = date1 )
  call AssertEqual('Handling of negative year test 1-1', &
    & "-1230-10-25T02:45:30", date_str )

  call DCCalDateCreate( - 0, 10, 25, 2, 45, 30.0_DP, date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 2-1', .true., leap_year )
  call DCCalDateCreate( "-1-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 2-2', .false., leap_year )
  call DCCalDateCreate( "-4-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 2-3', .true., leap_year )
  call DCCalDateCreate( "-100-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 2-4', .false., leap_year )
  call DCCalDateCreate( "-400-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 2-5', .true., leap_year )

  call DCCalDateCreate( "-399-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 365, date1 )
  call AssertEqual('Handling of negative year test 2-6', .true., leap_year )

  call DCCalDateCreate( "-400-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 365, date1 )
  call AssertEqual('Handling of negative year test 2-7', .true., leap_year )

  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative year test 2-8', .false., leap_year )
  
  call DCCalDateInquire( date_str, elapse_sec = - 86400.0_DP * 365, date = date1 )

  call DCCalCreate( 'julian' )
  call DCCalDateCreate( - 0, 10, 25, 2, 45, 30.0_DP, date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 3-1', .true., leap_year )
  call DCCalDateCreate( "-1-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 3-2', .false., leap_year )
  call DCCalDateCreate( "-4-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 3-3', .true., leap_year )
  call DCCalDateCreate( "-100-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 3-4', .true., leap_year )
  call DCCalDateCreate( "-400-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( 0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 3-5', .true., leap_year )

  call DCCalDateCreate( "-399-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 365, date1 )
  call AssertEqual('Handling of negative year test 3-6', .true., leap_year )

  call DCCalDateCreate( "-400-12-31T10:30:50", date1 )
  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 365, date1 )
  call AssertEqual('Handling of negative year test 3-7', .true., leap_year )

  leap_year = DCCalDateChkLeapYear( - 86400.0_DP * 366, date1 )
  call AssertEqual('Handling of negative year test 3-8', .false., leap_year )
  
  call DCCalDateInquire( date_str, elapse_sec = - 86400.0_DP * 365, date = date1 )



  call DCCalCreate( 'gregorian' )
  call DCCalDateCreate( "0-12-31T0:0:0", date3 )
  call DCCalDateCreate( "1-01-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-1', 86400.0_DP, sec )

  call DCCalDateCreate( "0-2-28T0:0:0", date3 )
  call DCCalDateCreate( "1-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-2', 86400.0_DP * 366, sec )

  call DCCalDateCreate( "000-2-28T0:0:0", date3 )
  call DCCalDateCreate( "000-3-1T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-3', 86400.0_DP * 2, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-1-3-1T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-4', 86400.0_DP, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "0-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-5', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-400-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-399-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-6', 86400.0_DP * 366, sec )

  sec = DCCalDateDifference( date4, date3 )
  call AssertEqual('Handling of negative year test 4-7', - 86400.0_DP * 366, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "1-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-8', 86400.0_DP * ( 365 * 2 + 1), sec )
  call DCCalDateCreate( "-10400-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-10399-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-9', 86400.0_DP * 366, sec )

  call DCCalDateCreate( "100-2-28T0:0:0", date3 )
  call DCCalDateCreate( "101-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-10', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-500-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-499-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-11', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-500-2-27T22:45:27", date3 )
  call DCCalDateCreate( "-500-2-28T6:7:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-12', &
    & 3600 + 14 * 60 + 33.0_DP +  6 * 3600 + 7 * 60 + 50.0_DP , sec )

  call DCCalDateCreate( "-500-2-27T22:45:27", date3 )
  call DCCalDateCreate( "-499-2-28T6:7:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 4-13', &
    & 86400.0_DP * 365 + 3600 + 14 * 60 + 33.0_DP +  6 * 3600 + 7 * 60 + 50.0_DP , sec )

  call DCCalDateCreate( -4, 1, 1, 0, 0, 0.0_DP, date1 )
  call DCCalDateInquire( date_str, -1.0_DP, date = date1 )
  call AssertEqual('Handling of negative year test 5-1', &
    & "-005-12-31T23:59:59", date_str )

  leap_year = DCCalDateChkLeapYear( -10.0_DP, date1 )
  call AssertEqual('Handling of negative year test 5-2', .false., leap_year )
  leap_year = DCCalDateChkLeapYear( -1.0_DP, date1 )
  call AssertEqual('Handling of negative year test 5-3', .false., leap_year )
  leap_year = DCCalDateChkLeapYear(  0.0_DP, date1 )
  call AssertEqual('Handling of negative year test 5-4', .true., leap_year )
  leap_year = DCCalDateChkLeapYear(  1.0_DP, date1 )
  call AssertEqual('Handling of negative year test 5-5', .true., leap_year )




  call DCCalCreate( 'julian' )
  call DCCalDateCreate( "0-12-31T0:0:0", date3 )
  call DCCalDateCreate( "1-01-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-1', 86400.0_DP, sec )

  call DCCalDateCreate( "0-2-28T0:0:0", date3 )
  call DCCalDateCreate( "1-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-2', 86400.0_DP * 366, sec )

  call DCCalDateCreate( "000-2-28T0:0:0", date3 )
  call DCCalDateCreate( "000-3-1T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-3', 86400.0_DP * 2, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-1-3-1T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-4', 86400.0_DP, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "0-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-5', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-400-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-399-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-6', 86400.0_DP * 366, sec )

  sec = DCCalDateDifference( date4, date3 )
  call AssertEqual('Handling of negative year test 6-7', - 86400.0_DP * 366, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "1-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-8', 86400.0_DP * ( 365 * 2 + 1), sec )

  call DCCalDateCreate( "-10400-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-10399-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-9', 86400.0_DP * 366, sec )

  call DCCalDateCreate( "100-2-28T0:0:0", date3 )
  call DCCalDateCreate( "101-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-10', 86400.0_DP * 366, sec )

  call DCCalDateCreate( "-500-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-499-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-11', 86400.0_DP * 366, sec )

  call DCCalDateCreate( "-500-2-27T22:45:27", date3 )
  call DCCalDateCreate( "-500-2-28T6:7:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-12', &
    & 3600 + 14 * 60 + 33.0_DP +  6 * 3600 + 7 * 60 + 50.0_DP , sec )

  call DCCalDateCreate( "-500-2-27T22:45:27", date3 )
  call DCCalDateCreate( "-499-2-28T6:7:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 6-13', &
    & 86400.0_DP * 366 + 3600 + 14 * 60 + 33.0_DP +  6 * 3600 + 7 * 60 + 50.0_DP , sec )


  call DCCalCreate( 'noleap' )
  call DCCalDateCreate( "0-12-31T0:0:0", date3 )
  call DCCalDateCreate( "1-01-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-1', 86400.0_DP, sec )

  call DCCalDateCreate( "0-2-28T0:0:0", date3 )
  call DCCalDateCreate( "1-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-2', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "000-2-28T0:0:0", date3 )
  call DCCalDateCreate( "000-3-1T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-3', 86400.0_DP, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-1-3-1T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-4', 86400.0_DP, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "0-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-5', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-400-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-399-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-6', 86400.0_DP * 365, sec )

  sec = DCCalDateDifference( date4, date3 )
  call AssertEqual('Handling of negative year test 7-7', - 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-1-2-28T0:0:0", date3 )
  call DCCalDateCreate( "1-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-8', 86400.0_DP * ( 365 * 2 ), sec )

  call DCCalDateCreate( "-10400-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-10399-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-9', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "100-2-28T0:0:0", date3 )
  call DCCalDateCreate( "101-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-10', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-500-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-499-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-11', 86400.0_DP * 365, sec )

  call DCCalDateCreate( "-500-2-27T22:45:27", date3 )
  call DCCalDateCreate( "-500-2-28T6:7:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-12', &
    & 3600 + 14 * 60 + 33.0_DP +  6 * 3600 + 7 * 60 + 50.0_DP , sec )

  call DCCalDateCreate( "-500-2-27T22:45:27", date3 )
  call DCCalDateCreate( "-499-2-28T6:7:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 7-13', &
    & 86400.0_DP * 365 + 3600 + 14 * 60 + 33.0_DP +  6 * 3600 + 7 * 60 + 50.0_DP , sec )



  call DCCalCreate( 1, (/ 669 /), 24, 1, 3694.0_DP )
  call DCCalDateCreate( "0-1-669T0:0:0", date3 )
  call DCCalDateCreate( "1-1-01T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-1', 24 * 3694.0_DP, sec )

  call DCCalDateCreate( "0-1-669T0:0:0", date3 )
  call DCCalDateCreate( "1-1-669T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-2', 669 * 24 * 3694.0_DP, sec )

  call DCCalDateCreate( "-1-1-300T0:0:0", date3 )
  call DCCalDateCreate( "-1-1-400T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-3', 100 * 24 * 3694.0_DP, sec )

  call DCCalDateCreate( "-1-1-28T0:0:0", date3 )
  call DCCalDateCreate( "0-1-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-4', 669 * 24 * 3694.0_DP, sec )

  call DCCalDateCreate( "-400-2-28T0:0:0", date3 )
  call DCCalDateCreate( "-399-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-5', 669 * 24 * 3694.0_DP, sec )

  sec = DCCalDateDifference( date4, date3 )
  call AssertEqual('Handling of negative year test 8-6', - 669 * 24 * 3694.0_DP, sec )

  call DCCalDateCreate( "-1-1-28T0:0:0", date3 )
  call DCCalDateCreate( "1-1-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-7', 669 * 24 * 3694.0_DP * 2, sec )

  call DCCalDateCreate( "-10400-1-28T0:0:0", date3 )
  call DCCalDateCreate( "-10399-1-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-8', 669 * 24 * 3694.0_DP, sec )

  call DCCalDateCreate( "100-2-28T0:0:0", date3 )
  call DCCalDateCreate( "101-2-28T0:0:0", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-9', 669 * 24 * 3694.0_DP, sec )

  call DCCalDateCreate( "-500-1-627T22:0:27", date3 )
  call DCCalDateCreate( "-500-1-628T06:0:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-10', &
    & 3694 + (3694 - 27) + 6 * 3694 + 50.0_DP, sec )

  call DCCalDateCreate( "-500-1-427T22:0:27", date3 )
  call DCCalDateCreate( "-499-1-428T06:0:50", date4 )
  sec = DCCalDateDifference( date3, date4 )
  call AssertEqual('Handling of negative year test 8-11', &
    & 669 * 24 * 3694.0_DP + 3694 + (3694 - 27) + 6 * 3694 + 50.0_DP, sec )

end program dc_calendar_test
