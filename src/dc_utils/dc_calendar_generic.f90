!= dc_calendar より提供される手続の引用仕様宣言
!= Interface of procedures provided from dc_calendar
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_calendar_generic.f90,v 1.9 2009-12-29 16:10:02 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_calendar_generic
  != dc_calendar より提供される手続の引用仕様宣言
  != Interface of procedures provided from dc_calendar
  !
  ! モジュールの概要や使用法については, dc_calendar 
  ! を参照ください. 
  !
  ! See "dc_calendar" for brief of this module and usage. 
  !
  !== Procedures List
  !
  ! 暦に関する手続
  !
  ! Procedures for calendar
  !
  ! DCCalCreate        :: 暦の設定
  ! DCCalInquire       :: 暦情報の問い合わせ
  ! DCCalConvertByUnit :: 単位の変換
  ! DCCalConvertToSec  :: 時間の秒への変換
  ! DCCalConvertToMin  :: 時間の分への変換
  ! DCCalConvertToHour :: 時間の時への変換
  ! DCCalConvertToDay  :: 時間の日への変換
  ! DCCalDefault       :: デフォルトの暦情報の取得
  ! DCCalToChar        :: 暦情報の文字列への変換
  ! DCCalParseUnit     :: 単位の解釈
  ! ------------       ::
  ! DCCalCreate        :: Set calendar
  ! DCCalInquire       :: Inquire information of calendar 
  ! DCCalConvertByUnit :: Convert of unit
  ! DCCalConvertToSec  :: Convert time into second
  ! DCCalConvertToMin  :: Convert time into minute
  ! DCCalConvertToHour :: Convert time into hour
  ! DCCalConvertToDay  :: Convert time into day
  ! DCCalDefault       :: Get information of default calendar
  ! DCCalToChar        :: Convert calender information into strings
  ! DCCalParseUnit     :: Parse units
  !
  !
  ! 日時に関する手続
  !
  ! Procedures for date
  !
  ! DCCalDateCreate        :: 日時の設定
  ! DCCalDateInquire       :: 日時の問い合わせ
  ! DCCalDateEval          :: 日時の算出
  ! DCCalDateDifference    :: 日時差の算出
  ! DCCalDateEvalSecOfYear :: 年始めからの通秒の算出
  ! DCCalDateEvalDayOfYear :: 年始めからの通日の算出
  ! DCCalDateEvalSecOfDay  :: 日始めからの通秒の算出
  ! DCCalDateChkLeapYear   :: 閏年かどうかの判定
  ! DCCalDateCurrent       :: 実時間の取得
  ! DCCalDateParseStr      :: 日時を示す文字列 (YYYY-MM-DDThh:mm:ss.sTZD) の解釈
  ! DCCalDateToChar        :: 日時の文字列 (YYYY-MM-DDThh:mm:ss.sTZD) への変換
  ! ------------           ::
  ! DCCalDateCreate        :: Set date
  ! DCCalDateInquire       :: Inquire date
  ! DCCalDateEval          :: Evaluate date
  ! DCCalDateDifference    :: Evaluate difference of date
  ! DCCalDateEvalSecOfYear :: Evaluate second of year
  ! DCCalDateEvalDayOfYear :: Evaluate day of year
  ! DCCalDateEvalSecOfDay  :: Evaluate second of day
  ! DCCalDateChkLeapYear   :: Judge whether it is a leap year
  ! DCCalDateCurrent       :: Get actual time
  ! DCCalDateParseStr      :: Parse a string of date (YYYY-MM-DDThh:mm:ss.sTZD) 
  ! DCCalDateToChar        :: Convert date into a string (YYYY-MM-DDThh:mm:ss.sTZD) 
  !

  implicit none

  interface DCCalCreate
    subroutine DCCalCreate1( cal_type, cal, err )
      use dc_calendar_types, only: DC_CAL
      character(*), intent(in):: cal_type
      type(DC_CAL), intent(out), optional, target:: cal
      logical, intent(out), optional:: err
    end subroutine DCCalCreate1

    subroutine DCCalCreate2( month_in_year, day_in_month, &
      &                      hour_in_day, min_in_hour, sec_in_min, &
      &                      cal, err )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      integer, intent(in):: month_in_year
      integer, intent(in):: day_in_month(:)
      integer, intent(in):: hour_in_day, min_in_hour
      real(DP), intent(in):: sec_in_min
      type(DC_CAL), intent(out), optional, target:: cal
      logical, intent(out), optional:: err
    end subroutine DCCalCreate2
  end interface

  interface DCCalDefault
    subroutine DCCalDefault1( cal )
      use dc_calendar_types, only: DC_CAL
      type(DC_CAL), intent(out):: cal
    end subroutine DCCalDefault1
  end interface

  interface DCCalInquire
    subroutine DCCalInquire1( cal_type, &
      &                       month_in_year, &
      &                       day_in_month, day_in_month_ptr, &
      &                       hour_in_day, min_in_hour, sec_in_min, &
      &                       cal, err )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      character(*), intent(out), optional:: cal_type
      integer, intent(out), optional:: month_in_year
      integer, intent(out), optional:: day_in_month(:)
      integer, pointer, optional:: day_in_month_ptr(:)
      integer, intent(out), optional:: hour_in_day, min_in_hour
      real(DP), intent(out), optional:: sec_in_min
      type(DC_CAL), intent(in), optional, target:: cal
      logical, intent(out), optional:: err
    end subroutine DCCalInquire1
  end interface

  interface DCCalToChar
    function DCCalToChar1( cal ) result(result)
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: STRING
      character(STRING):: result
      type(DC_CAL), intent(in):: cal
    end function DCCalToChar1
  end interface

  interface DCCalConvertByUnit
    function DCCalConvertByUnit1( in_time, in_unit, out_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      character(*), intent(in):: in_unit
      character(*), intent(in):: out_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertByUnit1

    function DCCalConvertByUnit2( in_time, in_unit, out_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      integer, intent(in):: in_unit
      integer, intent(in):: out_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertByUnit2
  end interface

  interface DCCalConvertToSec

    function DCCalConvertToSec1( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      character(*), intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToSec1

    function DCCalConvertToSec2( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      integer, intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToSec2

  end interface

  interface DCCalConvertToMin

    function DCCalConvertToMin1( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      character(*), intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToMin1

    function DCCalConvertToMin2( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      integer, intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToMin2

  end interface

  interface DCCalConvertToHour

    function DCCalConvertToHour1( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      character(*), intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToHour1

    function DCCalConvertToHour2( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      integer, intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToHour2

  end interface

  interface DCCalConvertToDay

    function DCCalConvertToDay1( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      character(*), intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToDay1

    function DCCalConvertToDay2( in_time, in_unit, cal ) result( out_time )
      use dc_calendar_types, only: DC_CAL
      use dc_types, only: DP
      real(DP):: out_time
      real(DP), intent(in):: in_time
      integer, intent(in):: in_unit
      type(DC_CAL), intent(in), optional, target:: cal
    end function DCCalConvertToDay2

  end interface

  interface DCCalParseUnit
    subroutine DCCalParseUnit1( unit_str, unit_sym, err )
      character(*), intent(in):: unit_str
      integer, intent(out):: unit_sym
      logical, intent(out), optional:: err
    end subroutine DCCalParseUnit1
  end interface

  interface DCCalDateCreate
    subroutine DCCalDateCreate1( year, month, day, hour, min, sec, date, zone, err )
      use dc_calendar_types, only: DC_CAL_DATE
      use dc_types, only: DP
      integer, intent(in):: year, month, day, hour, min
      real(DP), intent(in):: sec
      type(DC_CAL_DATE), intent(out), optional, target:: date
      character(*), intent(in), optional:: zone
      logical, intent(out), optional:: err
    end subroutine DCCalDateCreate1

    subroutine DCCalDateCreate2( date_str, date, err )
      use dc_calendar_types, only: DC_CAL_DATE
      character(*), intent(in):: date_str
      type(DC_CAL_DATE), intent(out), optional, target:: date
      logical, intent(out), optional:: err
    end subroutine DCCalDateCreate2
  end interface

  interface DCCalDateCurrent
    subroutine DCCalDateCurrent1( date, err )
      use dc_calendar_types, only: DC_CAL_DATE
      type(DC_CAL_DATE), intent(out):: date
      logical, intent(out), optional:: err
    end subroutine DCCalDateCurrent1
  end interface

  interface DCCalDateParseStr
    subroutine DCCalDateParseStr1( date_str, &
      & year, month, day, hour, min, sec, zone, &
      & err )
      use dc_types, only: DP
      character(*), intent(in):: date_str
      integer, intent(out):: year, month, day, hour, min
      real(DP), intent(out):: sec
      character(*), intent(out):: zone
      logical, intent(out), optional:: err
    end subroutine DCCalDateParseStr1
  end interface

  interface DCCalDateInquire
    subroutine DCCalDateInquire1( year, month, day, hour, min, sec, zone, &
      & elapse_sec, date, cal, err )
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      integer, intent(out), optional:: year, month, day, hour, min
      real(DP), intent(out), optional:: sec
      character(*), intent(out), optional:: zone
      real(DP), intent(in), optional:: elapse_sec
      type(DC_CAL_DATE), intent(in), optional, target:: date
      type(DC_CAL), intent(in), optional, target:: cal
      logical, intent(out), optional:: err
    end subroutine DCCalDateInquire1

    subroutine DCCalDateInquire2( date_str, elapse_sec, date, cal, err )
      use dc_types, only: DP
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      character(*), intent(out):: date_str
      real(DP), intent(in), optional:: elapse_sec
      type(DC_CAL_DATE), intent(in), optional, target:: date
      type(DC_CAL), intent(in), optional, target:: cal
      logical, intent(out), optional:: err
    end subroutine DCCalDateInquire2
  end interface

  interface DCCalDateToChar
    function DCCalDateToChar1( year, month, day, hour, min, sec, zone ) &
      & result(result)
      use dc_types, only: STRING, DP, TOKEN
      character(STRING):: result
      integer, intent(in):: year, month, day, hour, min
      real(DP), intent(in):: sec
      character(*), intent(in), optional:: zone
    end function DCCalDateToChar1
  end interface

  interface DCCalDateEval
    subroutine DCCalDateEvalYMDHMS1( year, month, day, hour, min, sec, elapse_sec, cal, date, err )
      use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
      use dc_types, only: DP
      integer, intent(in):: year, month, day, hour, min
      real(DP), intent(in):: sec, elapse_sec
      type(DC_CAL), intent(in), optional, target:: cal
      type(DC_CAL_DATE), intent(out), optional, target:: date
      logical, intent(out), optional:: err
    end subroutine DCCalDateEvalYMDHMS1

    subroutine DCCalDateEvalYMDHMS2( year, month, day, hour, min, sec, elapse_time, units, cal, date, err )
      use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
      use dc_types, only: DP
      integer, intent(in):: year, month, day, hour, min
      real(DP), intent(in):: sec, elapse_time
      character(*), intent(in):: units
      type(DC_CAL), intent(in), optional, target:: cal
      type(DC_CAL_DATE), intent(out), optional, target:: date
      logical, intent(out), optional:: err
    end subroutine DCCalDateEvalYMDHMS2

    subroutine DCCalDateEvalID1( init_date, elapse_sec, cal, date, err )
      use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
      use dc_types, only: DP
      type(DC_CAL_DATE), intent(in):: init_date
      real(DP), intent(in):: elapse_sec
      type(DC_CAL), intent(in), optional, target:: cal
      type(DC_CAL_DATE), intent(out), optional, target:: date
      logical, intent(out), optional:: err
    end subroutine DCCalDateEvalID1

    subroutine DCCalDateEvalID2( init_date, elapse_time, units, cal, date, err )
      use dc_calendar_types, only: DC_CAL, DC_CAL_DATE
      use dc_types, only: DP
      type(DC_CAL_DATE), intent(in):: init_date
      real(DP), intent(in):: elapse_time
      character(*), intent(in):: units
      type(DC_CAL), intent(in), optional, target:: cal
      type(DC_CAL_DATE), intent(out), optional, target:: date
      logical, intent(out), optional:: err
    end subroutine DCCalDateEvalID2

    subroutine DCCalDateEvalYM2YM1( &
      & year1, month1, day1, hour1, min1, sec1, &
      & elapse_sec, &
      & year2, month2, day2, hour2, min2, sec2, &
      & cal, err )
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      integer, intent(in):: year1, month1, day1, hour1, min1
      real(DP), intent(in):: sec1, elapse_sec
      integer, intent(out):: year2, month2, day2, hour2, min2
      real(DP), intent(out):: sec2
      type(DC_CAL), intent(in), optional, target:: cal
      logical, intent(out), optional:: err
    end subroutine DCCalDateEvalYM2YM1

    subroutine DCCalDateEvalYM2YM2( &
      & year1, month1, day1, hour1, min1, sec1, &
      & elapse_time, units, &
      & year2, month2, day2, hour2, min2, sec2, &
      & cal, err )
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      integer, intent(in):: year1, month1, day1, hour1, min1
      real(DP), intent(in):: sec1, elapse_time
      character(*), intent(in):: units
      integer, intent(out):: year2, month2, day2, hour2, min2
      real(DP), intent(out):: sec2
      type(DC_CAL), intent(in), optional, target:: cal
      logical, intent(out), optional:: err
    end subroutine DCCalDateEvalYM2YM2

  end interface

  interface DCCalDateDifference
    function DCCalDateDifference1( start_date, end_date, cal ) result(sec)
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      type(DC_CAL_DATE), intent(in):: start_date, end_date
      type(DC_CAL), intent(in), optional, target:: cal
      real(DP):: sec
    end function DCCalDateDifference1
  end interface

  interface DCCalDateEvalSecOfYear
    function DCCalDateEvalSecOfYear1( elapse_sec, date, cal ) result(result)
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      real(DP), intent(in):: elapse_sec
      type(DC_CAL_DATE), intent(in), optional, target:: date
      type(DC_CAL), intent(in), optional, target:: cal
      real(DP):: result
    end function DCCalDateEvalSecOfYear1
  end interface

  interface DCCalDateEvalDayOfYear
    function DCCalDateEvalDayOfYear1( elapse_sec, date, cal ) result(result)
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      real(DP), intent(in):: elapse_sec
      type(DC_CAL_DATE), intent(in), optional, target:: date
      type(DC_CAL), intent(in), optional, target:: cal
      real(DP):: result
    end function DCCalDateEvalDayOfYear1
  end interface

  interface DCCalDateEvalSecOfDay
    function DCCalDateEvalSecOfDay1( elapse_sec, date, cal ) result(result)
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      real(DP), intent(in):: elapse_sec
      type(DC_CAL_DATE), intent(in), optional, target:: date
      type(DC_CAL), intent(in), optional, target:: cal
      real(DP):: result
    end function DCCalDateEvalSecOfDay1
  end interface

  interface DCCalDateChkLeapYear
    function DCCalDateChkLeapYear1( elapse_sec, date, cal ) result(result)
      use dc_calendar_types, only: DC_CAL_DATE, DC_CAL
      use dc_types, only: DP
      real(DP), intent(in):: elapse_sec
      type(DC_CAL_DATE), intent(in), optional, target:: date
      type(DC_CAL), intent(in), optional, target:: cal
      logical:: result
    end function DCCalDateChkLeapYear1
  end interface

end module dc_calendar_generic
