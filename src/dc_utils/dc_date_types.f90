!== 日付・時刻に関する構造データ型
!== Data types for date and time
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: dc_date_types.f90,v 1.1 2009-03-20 09:09:53 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]

module dc_date_types
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! dc_date で用いられる構造体や変数, 定数を定義しているモジュールです. 
  !
  ! また暦法に関する情報も管理されています. 
  !
  ! Derived types, variables, parameters for "dc_date" are defined 
  ! in this module. 
  ! 
  ! Information of calender are managed too.
  !
  !== Derived types List
  !
  ! DC_DATETIME :: 通日と通秒の対で日付時刻を表現します. 
  ! DC_DIFFTIME :: X ヶ月後, X 日前, などを表現します. 
  !
  !== Characters list for unit
  ! 
  ! UNIT_NONDIM       :: .
  ! UNIT_SEC          :: .
  ! UNIT_MIN          :: .
  ! UNIT_HOUR         :: .
  ! UNIT_DAY          :: .
  ! UNIT_MONTH        :: .
  ! UNIT_YEAR         :: .
  !
  !== Symbols for unit
  ! 
  ! UNIT_SYMBOL_NONDIM :: .
  ! UNIT_SYMBOL_SEC    :: .
  ! UNIT_SYMBOL_MIN    :: .
  ! UNIT_SYMBOL_HOUR   :: .
  ! UNIT_SYMBOL_DAY    :: .
  ! UNIT_SYMBOL_MONTH  :: .
  ! UNIT_SYMBOL_YEAR   :: .
  ! UNIT_SYMBOL_ERR    :: .
  !
  !== Parameters for calender
  !
  ! CAL_CYCLIC        :: .
  ! CAL_NOLEAP        :: .
  ! CAL_JULIAN        :: .
  ! CAL_GREGORIAN     :: .
  !
  !== Parameters for conversion of year-month-day-hour-min-sec
  ! 
  ! CYCLIC_MDAYS      :: .
  ! DAY_SECONDS_EARTH :: .
  ! MIN_SECONDS       :: .
  ! HOUR_SECONDS      :: .
  ! YEAR_MONTHS       :: .
  ! YEAR_DAYS         :: .
  ! FOUR_YEARS        :: .
  ! FOUR_CENTURY      :: .
  ! PREPARED_CALTYPES :: .
  !
  !
  use dc_types, only: DP, STRING
  use dc_scaledsec, only: DC_SCALED_SEC

  implicit none

  private
  public:: DC_DATETIME, DC_DIFFTIME
  public:: UNIT_NONDIM, UNIT_SEC, UNIT_MIN, UNIT_HOUR, UNIT_DAY, UNIT_MONTH, UNIT_YEAR
  public:: UNIT_SYMBOL_NONDIM, UNIT_SYMBOL_SEC, UNIT_SYMBOL_MIN
  public:: UNIT_SYMBOL_HOUR, UNIT_SYMBOL_DAY, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_YEAR
  public:: UNIT_SYMBOL_ERR
  public:: CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN
  public:: CYCLIC_MDAYS, DAY_SECONDS_EARTH, MIN_SECONDS, HOUR_SECONDS
  public:: YEAR_MONTHS, YEAR_DAYS, FOUR_YEARS, FOUR_CENTURY
  public:: PREPARED_CALTYPES
  public:: caltype, day_seconds, day_seconds_scl, flag_set_day_seconds_scl

  !-------
  ! 暦法

  integer, parameter:: CAL_CYCLIC = 1   ! 1 ヵ月を 30.6 日 (CYCLIC_MDAYS)
                                        ! とする暦.
                                        ! (例: 0 ヶ月目は 1 〜 30 (30.6),
                                        ! 1 ヶ月目は 1 〜 31 (61.2),
                                        ! 2 ヶ月目は 1 〜 30 (91.8),
                                        ! 3 ヶ月目は 1 〜 31 (122.4),
                                        ! 4 ヶ月目は 1 〜 31 (153.0) ...)
                                        !
                                        ! 仮想的な時間で実験を行う
                                        ! 場合に使用することを想定してい
                                        ! ます.
                                        !
  integer, parameter:: CAL_NOLEAP = 2   ! 1 年 365 日の暦
                                        !
  integer, parameter:: CAL_JULIAN = 3   ! ユリウス暦
                                        !
  integer, parameter:: CAL_GREGORIAN = 4! グレゴリオ暦
                                        !
  integer, parameter:: PREPARED_CALTYPES(0:3) = &
    & (/CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN/)

  integer, save:: caltype = CAL_GREGORIAN ! デフォルトの暦

  !-------
  ! 日時の単位間の関係

  real(DP), parameter:: CYCLIC_MDAYS = 30.6_DP    ! CAL_CYCLIC で使用される
                                                  ! 1 ヶ月の日数.
                                                  ! また DC_DIFFTIME の
                                                  ! 1 ヶ月の日数にも
                                                  ! 使用します.

  integer, parameter:: MIN_SECONDS  = 60          ! 1 分の秒数
  integer, parameter:: HOUR_SECONDS = 3600        ! 1 時間の秒数
  real(DP), parameter:: DAY_SECONDS_EARTH  = 86400.0_DP
                                                  ! 地球の 1 日の秒数
  real(DP), save :: day_seconds = DAY_SECONDS_EARTH
                                                  ! 1 日の秒数
  type(DC_SCALED_SEC), save :: day_seconds_scl
                                                  ! 1 日の秒数
  logical, save :: flag_set_day_seconds_scl = .false.
                                                  ! 1 日の秒数
  integer, parameter:: YEAR_DAYS = 365            ! 1 年 (非閏年) の日数
  integer, parameter:: YEAR_MONTHS = 12           ! 1 年の月数
  integer, parameter:: FOUR_YEARS = YEAR_DAYS * 4 + 1
                                                  ! 4 年の日数
  integer, parameter:: FOUR_CENTURY = YEAR_DAYS * 400 + 97
                                                  ! 1 世紀の日数

  !-------
  ! 日時の単位として認識される文字列

  character(*), parameter, dimension(1) :: UNIT_NONDIM = (/ &
    & '1' /)                                       ! 無次元時間の単位を示す文字列

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

  !-------
  ! 日時の単位のシンボル

  integer, parameter:: UNIT_SYMBOL_ERR    = -1 ! 無効な単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_NONDIM = 1 ! 無時限時間の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_SEC    = 2 ! 秒の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_MIN    = 3 ! 分の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_HOUR   = 4 ! 時間の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_DAY    = 5 ! 日の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_MONTH  = 6 ! 月の単位を示すシンボル
  integer, parameter:: UNIT_SYMBOL_YEAR   = 7 ! 年の単位を示すシンボル

  type DC_DATETIME
    ! 通日と通秒の対で日付時刻を表現します.
    !
    ! この構造データ型の変数を使用する際は必ず変数を
    ! dc_date#Create または dc_date#assignment(=)
    ! によって初期化してください. また, *day*, *sec* などの内部変数は
    ! 直接変更しないでください.
    !
    ! 利用法は dc_date の "List" および "Usage" を参照してください.
    !
    sequence
    integer :: caltype = CAL_GREGORIAN ! 暦法
    type(DC_SCALED_SEC):: day          ! 日
    type(DC_SCALED_SEC):: sec          ! 秒
    logical:: dummy = .false.
                              ! 境界を埋めるためのダミー変数. 
                              ! Dummy variable for boundary alignment
    type(DC_SCALED_SEC):: day_seconds  ! 1 日の秒数
    character(STRING) :: zone = '+00:00' ! UTC からの時差
  end type DC_DATETIME

  type DC_DIFFTIME
    ! X ヶ月後, X 日前, などを表現するためのデータ型です.
    !
    ! この構造データ型の変数を使用する際は必ず変数を
    ! dc_date#Create または dc_date#assignment(=)
    ! によって初期化してください. また, *day*, *sec* などの内部変数は
    ! 直接変更しないでください.
    !
    ! 利用法は dc_date の "List" および "Usage" を参照してください.
    !
    ! なお, 1 ヶ月は dc_date_types#CYCLIC_MDAYS と換算します.
    !
    !--
    !== 開発者向け情報
    !
    ! 「1ヵ月後」という概念に対応するため、month 欄を持ちます。
    !
    ! 注意: 日付と違って月を normalize することはできません。
    !++
    sequence
    type(DC_SCALED_SEC):: mon ! 月. Month
    type(DC_SCALED_SEC):: day ! 日. Day
    type(DC_SCALED_SEC):: sec ! 秒 または無次元時間. Seconds or nondimensional time
    logical:: dummy0 = .false.
                              ! 境界を埋めるためのダミー変数. 
                              ! Dummy variable for boundary alignment
    type(DC_SCALED_SEC):: day_seconds  ! 1 日の秒数
                              ! 1 日の秒数. Seconds of day
    logical:: nondim_flag = .false.
                              ! 無次元数を示すフラグ. 
                              ! Flag for nondimensional number
    logical:: dummy1 = .false.
                              ! 境界を埋めるためのダミー変数. 
                              ! Dummy variable for boundary alignment

!!$    real(DP):: sec_scale_factor
!!$                              ! 1.0 以下の sec が与えられた際に, 
!!$                              ! 丸め誤差によって
!!$                              ! sec がずれるのを防ぐためのファクター
!!$                              ! (実験的機能)
  end type DC_DIFFTIME

end module dc_date_types
