!= 暦と日時に関する構造データ型と定数
!= Derived types and parameters of calendar and date 
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dc_calendar_types.f90,v 1.2 2009-10-17 14:08:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_calendar_types
  != 暦と日時に関する構造データ型と定数
  != Derived types and parameters of calendar and date 
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! dc_calendar に関連する構造体や定数を提供します. 
  !
  ! Derived types and parameters for "dc_calendar" are provided. 
  !
  !== Derived types List
  !
  ! DC_CAL       :: 暦情報
  ! DC_CAL_DATE  :: 日時情報
  ! ------------ ::
  ! DC_CAL       :: Calendar
  ! DC_CAL_DATE  :: Date
  !
  !== Parameters for calender
  !
  ! CAL_USER_DEFINED  :: 1 年の月数, 1 月の日数, 1 日の時間数, 1 時間の分数,
  !                      1 分の秒数が任意である暦. 
  ! CAL_GREGORIAN     :: グレゴリオ暦
  ! CAL_JULIAN        :: ユリウス暦
  ! CAL_NOLEAP        :: 閏年無しの暦
  ! CAL_360DAY        :: 1ヶ月が 30 日の暦
  ! CAL_CYCLIC        :: ある月の日数を
  !                      「30.6 × 月数 − 前月までの総日数」
  !                      の小数点以下切捨とする暦
  ! ------------      ::
  ! CAL_USER_DEFINED  :: A calendar in which number of months of a year, 
  !                      days of a month, hours of a day, minutes of a hour, 
  !                      seconds of a minute are arbitrary. 
  ! CAL_GREGORIAN     :: Gregorian calendar. 
  ! CAL_JULIAN        :: Julian calendar. 
  ! CAL_NOLEAP        :: A calendar without leap year. 
  ! CAL_360DAY        :: A calendar in which number of days of a month is 30. 
  ! CAL_CYCLIC        :: A calendar in which number of days of a year is
  !                      "30.6 x (number of months) - (total days until last month)"
  !                      (truncate fractional part). 
  !
  !== Characters list for unit
  ! 
  ! 日時の単位として認識される文字列のリストです. 
  !
  ! List of strings recognized as units of date. 
  ! 
  ! UNIT_SEC          :: 秒の単位
  ! UNIT_MIN          :: 分の単位
  ! UNIT_HOUR         :: 時間の単位
  ! UNIT_DAY          :: 日の単位
  ! UNIT_MONTH        :: 月の単位
  ! UNIT_YEAR         :: 年の単位
  ! ------------      ::
  ! UNIT_SEC          :: Units of second
  ! UNIT_MIN          :: Units of minute
  ! UNIT_HOUR         :: Units of hour
  ! UNIT_DAY          :: Units of day
  ! UNIT_MONTH        :: Units of month
  ! UNIT_YEAR         :: Units of year  
  !
  !== Symbols for unit
  !
  ! dc_calendar から提供される手続で使用される, 
  ! 日時の単位を示す整数です. 
  !
  ! Integers that indicate units of date. 
  ! They are used in procedures provied from "dc_calendar". 
  ! 
  ! UNIT_SYMBOL_SEC    :: 秒の単位
  ! UNIT_SYMBOL_MIN    :: 分の単位
  ! UNIT_SYMBOL_HOUR   :: 時間の単位
  ! UNIT_SYMBOL_DAY    :: 日の単位
  ! UNIT_SYMBOL_MONTH  :: 月の単位
  ! UNIT_SYMBOL_YEAR   :: 年の単位
  ! UNIT_SYMBOL_ERR    :: 無効な単位
  ! ------------       ::
  ! UNIT_SYMBOL_SEC    :: Units of second
  ! UNIT_SYMBOL_MIN    :: Units of minute
  ! UNIT_SYMBOL_HOUR   :: Units of hour
  ! UNIT_SYMBOL_DAY    :: Units of day
  ! UNIT_SYMBOL_MONTH  :: Units of month
  ! UNIT_SYMBOL_YEAR   :: Units of year
  ! UNIT_SYMBOL_ERR    :: Invalid unit
  !

  use dc_types, only: DP, TOKEN
  implicit none

  private
  public:: DC_CAL, DC_CAL_DATE
  public:: CAL_USER_DEFINED, CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, &
    &      CAL_GREGORIAN, CAL_360DAY
  public:: UNIT_SEC, UNIT_MIN, UNIT_HOUR, &
    &      UNIT_DAY, UNIT_MONTH, UNIT_YEAR
  public:: UNIT_SYMBOL_SEC, UNIT_SYMBOL_MIN, &
    &      UNIT_SYMBOL_HOUR, UNIT_SYMBOL_DAY, UNIT_SYMBOL_MONTH, &
    &      UNIT_SYMBOL_YEAR, UNIT_SYMBOL_ERR

  integer, parameter:: CAL_USER_DEFINED = 1
                                         ! 利用者が任意で指定する暦. 
                                         ! User defined calendar. 
                                         !
  integer, parameter:: CAL_CYCLIC = 2    ! 1 ヵ月を 30.6 日 (CYCLIC_MDAYS) 
                                         ! とする暦. 
                                         !
                                         ! All months are 30.6 day. 
                                         !
  integer, parameter:: CAL_NOLEAP = 3    ! 1 年 365 日 (閏年無し) の暦. 
                                         !
                                         ! Gregorian calendar without
                                         ! leap years, i.e., all years
                                         ! are 365 days long.
                                         !
  integer, parameter:: CAL_JULIAN = 4    ! ユリウス暦. 
                                         ! Julian calendar. 
                                         !
  integer, parameter:: CAL_GREGORIAN = 5 ! グレゴリオ暦. 
                                         ! Gregorian calendar. 
                                         !
  integer, parameter:: CAL_360DAY = 6    ! 1 年 360 日 (1 月 30 日) の暦. 
                                         !
                                         ! All years are 360 days 
                                         ! divided into 30 day months.

  type DC_CAL
    !
    ! 暦を表現する構造体. 
    !
    ! この構造体の要素を直接を参照および変更してはならない. 
    ! この構造体を扱う上では必ず dc_calendar_generic より提供される
    ! 手続きを用いること. 
    !
    ! A derived type that expresses calendar.  
    ! 
    ! Do not refer and modify elements of this derived type. 
    ! Use procedures provied by "dc_calendar_generic" necessarily 
    ! for handling this derived type. 
    !
    logical:: initialized = .false. 
                              ! 初期設定フラグ. 
                              ! Initialization flag
    integer:: month_in_year
                              ! 1 年の月数. 
                              ! Months in a year. 
    integer, pointer:: day_in_month(:) => null()
                              ! 1 ヶ月の日数. 
                              ! Days in months. 
    integer:: hour_in_day
                              ! 1 日の時間数. 
                              ! Hours in a day. 
    integer:: min_in_hour
                              ! 1 時間の分数. 
                              ! Minutes in a hour. 
    real(DP):: sec_in_min
                              ! 1 分の秒数. 
                              ! Seconds in a minute. 
    integer:: cal_type
                              ! 暦の種別. 
                              ! Kind of calendar. 
  end type DC_CAL

  type DC_CAL_DATE
    !
    ! 日時を表現する構造体
    !
    ! この構造体の要素を直接を参照および変更してはならない. 
    ! この構造体を扱う上では必ず dc_calendar_generic より提供される
    ! 手続きを用いること. 
    !
    ! A derived type that expresses date.  
    ! 
    ! Do not refer and modify elements of this derived type. 
    ! Use procedures provied by "dc_calendar_generic" necessarily 
    ! for handling this derived type. 
    !
    logical:: initialized = .false.
                              ! 初期設定フラグ. 
                              ! Initialization flag
    integer:: year            ! 年. Year.  
    integer:: month           ! 月. Month. 
    integer:: day             ! 日. Day. 
    integer:: hour            ! 時. Hour. 
    integer:: min             ! 分. Minute. 
    real(DP):: sec            ! 秒. Sec. 
    character(TOKEN):: zone   ! UTC からの時差. Time-zone. 
  end type DC_CAL_DATE

  ! 日時の単位として認識される文字列
  ! Strings recognized as units of date and time 
  !
  character(*), parameter, dimension(8) :: UNIT_SEC = (/ &
    & 'seconds', 'second ', 'secs.  ', 'secs   ', &
    & 'sec.   ', 'sec    ', 's.     ', 's      '/) ! 秒の単位を示す文字列

  character(*), parameter, dimension(4) :: UNIT_MIN = (/ &
    & 'minutes', 'minute ', 'min.   ', 'min    '/) ! 分の単位を示す文字列
  character(*), parameter, dimension(8) :: UNIT_HOUR = (/ &
    & 'hours', 'hour ', 'hrs. ', 'hrs  ', &
    & 'hr.  ', 'hr   ', 'h.   ', 'h    '/)         ! 時の単位を示す文字列
  character(*), parameter, dimension(4) :: UNIT_DAY = (/ &
    & 'days', 'day ', 'd.  ', 'd   '/)             ! 日の単位を示す文字列
  character(*), parameter, dimension(6) :: UNIT_MONTH = (/ &
    & 'months', 'month ', 'mon.  ', &
    & 'mon   ', 'mo.   ', 'mo    '/)               ! 月の単位を示す文字列
  character(*), parameter, dimension(4) :: UNIT_YEAR = (/ &
    & 'years', 'year ', 'yr.  ', 'yr   '/)         ! 年の単位を示す文字列


  ! 日時の単位のシンボル
  ! Synbols of units of date and time
  !
  integer, parameter:: UNIT_SYMBOL_ERR    = -1 ! 無効な単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_SEC    = 2 ! 秒の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_MIN    = 3 ! 分の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_HOUR   = 4 ! 時間の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_DAY    = 5 ! 日の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_MONTH  = 6 ! 月の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_YEAR   = 7 ! 年の単位を示すシンボル

end module dc_calendar_types
