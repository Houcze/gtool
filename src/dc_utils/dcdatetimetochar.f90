!== dc_date_types#DC_DATETIME, dc_date_types#DC_DIFFTIME 型変数の文字変換
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimetochar.f90,v 1.2 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2006-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

function DCDateTimeToChar(time) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数を文字型変数へ変換して返します.
  ! 書式は下記のように JIS X 0301 の完全表記です.
  !
  !   YYYY-MM-DDThh:mm:ss.sTZD
  !
  ! YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒,
  ! TZD はタイムゾーンを表します.
  !
  use dc_types, only: STRING, TOKEN, DP
  use dc_string, only: toChar, CPrintf, StoA
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DATETIME, &
    & MIN_SECONDS, HOUR_SECONDS, YEAR_MONTHS
  use dc_message, only: MessageNotify
  implicit none
  character(STRING):: result
  type(DC_DATETIME), intent(in):: time

  integer :: year, mon, day, hour, min, csec_len
  real(DP):: sec
  character(TOKEN) :: zone, csec
continue

  call Eval(time, &
    & year=year, mon=mon, day=day, hour=hour, min=min, sec=sec, zone=zone)

  csec = toChar(sec)
  if ( trim(csec) == '-0.' ) csec = '0.'
  do while ( index('123456789.', csec(len_trim(csec):len_trim(csec)) ) == 0 )
    if ( len_trim(csec) < 2 ) exit
    csec = csec(1:len_trim(csec)-1)
  end do
  if (int(sec) > -1 .and. int(sec) < 10)  csec = '0' // csec
  csec_len = len(trim(adjustl(csec)))
  if (csec(csec_len:csec_len) == '.') csec = csec(1:csec_len-1)

  result = CPrintf('%04d-%02d-%02dT%02d:%02d:%c%c', &
    & i=(/year, mon, day, hour, min/), &
    & c1=trim(csec), c2=trim(zone))

end function DCDateTimeToChar


function DCDiffTimeToChar(diff) result(result)
  !
  ! dc_date_types#DC_DIFFTIME 型変数を文字型変数へ変換して返します.
  ! 書式は以下のようになります.
  !
  !   +YYYY-MM-DDThh:mm:ss.s
  !   -YYYY-MM-DDThh:mm:ss.s
  !
  ! YYYY は年, MM は月, DD は日, hh は時, mm は分, ss.s は秒を表します.
  ! ただし, DD は 2 桁を超える場合があります.
  ! (dc_date_types#DC_DIFFTIME は X ヶ月後, X 日前, などを表現するため
  ! のデータ型なので, 日を月に繰り上げたり, 月を日に繰り下げることを
  ! しません. また「年」の情報も持ちません. 1 年の日数や 1 月の日数は
  ! dc_date_types#DC_DATETIME 側で決まります).
  !
  ! なお, DCDiffTimeCreate において, 単位を '1' とした場合は無時限時間と
  ! 扱うため, 以下のような書式となります. 
  ! 
  !   ss.s
  !
  use dc_types, only: STRING, TOKEN, DP
  use dc_string, only: toChar, CPrintf, StoA
  use dc_date_generic, only: Eval
  use dc_date_types, only: DC_DIFFTIME
  use dc_scaledsec, only: assignment(=)
  implicit none
  character(STRING):: result
  type(DC_DIFFTIME), intent(in):: diff

  integer :: year, mon, day, hour, min, csec_len
  real(DP):: sec
  character(TOKEN) :: csec
  character(1) :: pm
continue

  if ( .not. diff % nondim_flag ) then
    call Eval(diff, year=year, mon=mon, day=day, hour=hour, min=min, sec=sec)

    if ( year < 0 .or. mon < 0 .or. day < 0 .or.  &
      &  hour < 0 .or. min < 0 .or. sec < 0         ) then
      year=abs(year) ; mon=abs(mon) ; day=abs(day)
      hour=abs(hour) ; min=abs(min) ; sec=abs(sec)
      pm = '-'
    else
      pm = '+'
    end if

    csec = toChar(sec)
    if ( trim(csec) == '-0.' ) csec = '0.'
    do while ( index('123456789.', csec(len_trim(csec):len_trim(csec)) ) == 0 )
      if ( len_trim(csec) < 2 ) exit
      csec = csec(1:len_trim(csec)-1)
    end do
    if (int(sec) > -1 .and. int(sec) < 10)  csec = '0' // csec
    csec_len = len(trim(adjustl(csec)))
    if (csec(csec_len:csec_len) == '.') csec = csec(1:csec_len-1)

    result = CPrintf('%c%04d-%02d-%02dT%02d:%02d:%c', &
      & i=(/year, mon, day, hour, min/), &
      & c1=pm, c2=trim(csec))
  else
    sec = diff % sec
    result = toChar( sec )
  end if

end function DCDiffTimeToChar

function DCDateTimeToCharCal(time, upcase) result(result)
  !
  ! dc_date_types#DC_DATETIME 型変数の暦を文字型にして返します. 
  ! 現在サポートされている暦は以下の通りです. 
  ! 左が暦を示す整数型変数, 右が返る文字列です. 
  ! *upcase* に .true. を与えた場合には, 大文字となって返ります. 
  !
  ! dc_date_types#CAL_CYCLIC    :: cyclic
  ! dc_date_types#CAL_NOLEAP    :: noleap
  ! dc_date_types#CAL_JULIAN    :: julian
  ! dc_date_types#CAL_GREGORIAN :: gregorian
  !
  !
  use dc_types, only: TOKEN
  use dc_present, only: present_and_true
  use dc_date_types, only: DC_DATETIME, &
    & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN
  use dc_string, only: toUpper
  character(TOKEN) :: result
  type(DC_DATETIME), intent(in):: time
  logical, intent(in), optional:: upcase
continue
  select case( time % caltype )
  case(CAL_CYCLIC)
    result = 'cyclic'
  case(CAL_NOLEAP)
    result = 'noleap'
  case(CAL_JULIAN)
    result = 'julian'
  case(CAL_GREGORIAN)
    result = 'gregorian'
  case default
    result = 'none'
  end select

  if ( present_and_true(upcase) ) then
    call toUpper(result) ! (inout)
  end if

end function DCDateTimeToCharCal
