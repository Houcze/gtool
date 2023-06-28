!= 日時の文字列の解釈
!= Parse strings of date
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldateparsestr.f90,v 1.3 2010-09-24 07:07:31 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!
subroutine DCCalDateParseStr1( date_str, &
  & year, month, day, hour, min, sec, zone, &
  & err )
  ! *date_str* で与えられる日時形式 
  ! (gtool4 netCDF 規約「5.5 日時形式」に準拠) を解釈し, 
  ! *year* 〜 *zone* に返します. 
  !
  ! Parse strings of date (conformed to gtool4 netCDF Convention
  ! "5.5 Expression of date and time") specified as *date_str*, 
  ! and return *year* -- *zone*. 
  !

  use dc_regex, only: match
  use dc_message, only: MessageNotify
  use dc_string, only: LChar, StoI, StoD
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADDATE
  use dc_types, only: STRING, DP, TOKEN
  implicit none
  character(*), intent(in):: date_str
                              ! 日時情報を表す文字列. 
                              ! 表示形式については gtool4 netCDF 規約
                              ! 5.5 日時形式を参照のこと. 
                              ! 
                              ! Strings that express date and time. 
                              ! See gtool4 netCDF Convention 
                              ! 5.5 Expression of date and time for details. 
  integer, intent(out):: year  ! 年. Year.
  integer, intent(out):: month ! 月. Month.
  integer, intent(out):: day   ! 日. Day.
  integer, intent(out):: hour  ! 時. Hour.
  integer, intent(out):: min   ! 分. Minute.
  real(DP), intent(out):: sec  ! 秒. Second.
  character(*), intent(out):: zone
                              ! UTC からの時差. Time-zone. 
  logical, intent(out), optional:: err
                              ! 例外処理用フラグ. 
                              ! デフォルトでは, この手続き内でエラーが
                              ! 生じた場合, プログラムは強制終了します. 
                              ! 引数 *err* が与えられる場合, 
                              ! プログラムは強制終了せず, 代わりに
                              ! *err* に .true. が代入されます. 
                              !
                              ! Exception handling flag. 
                              ! By default, when error occur in 
                              ! this procedure, the program aborts. 
                              ! If this *err* argument is given, 
                              ! .true. is substituted to *err* and 
                              ! the program does not abort. 


  ! 作業変数
  ! Work variables
  !
  integer:: start, length
  character(STRING):: str1, str2
  character(TOKEN):: zone_pm, zone_hrs, zone_min
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateParseStr1'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! 与えられた文字列が日時表現として有効かどうかをチェック
  ! Check validation of strings as an expression of date
  !
  call match( '[-]*#d+-#d+-#d+[#w#s]+#d+:#d+:#d+', date_str, & ! (in)
    &         start, length ) ! (out)

  if ( length > 0 ) then
    str1 = date_str(start:)
  else
    stat = DC_EBADDATE
    call MessageNotify('W', subname, &
      & 'date_str=<%c> is invalid expression as date.', &
      & c1 = trim(date_str) )
    goto 999
  end if

  ! 年の解釈
  ! Parse year
  !
  call match( '^[-]*#d+-', str1, & ! (in)
    &         start, length ) ! (out)
  str2 = str1(start:start+length-2)
  str1 = str1(start+length:)
  year = StoI(str2)

  ! 月の解釈
  ! Parse month
  !
  call match( '^#d+-', str1, & ! (in)
    &         start, length ) ! (out)
  str2 = str1(start:start+length-2)
  str1 = str1(start+length:)
  month = StoI(str2)

  ! 日の解釈
  ! Parse day
  !
  call match( '^#d+[#w#s]', str1, & ! (in)
    &         start, length ) ! (out)
  str2 = str1(start:start+length-2)
  str1 = str1(start+length:)
  day = StoI(str2)

  ! 時の解釈
  ! Parse hour
  !
  call match( '#d+:', str1, & ! (in)
    &         start, length ) ! (out)
  str2 = str1(start:start+length-2)
  str1 = str1(start+length:)
  hour = StoI(str2)

  ! 分の解釈
  ! Parse minute
  !
  call match( '#d+:', str1, & ! (in)
    &         start, length ) ! (out)
  str2 = str1(start:start+length-2)
  str1 = str1(start+length:)
  min = StoI(str2)

  ! 秒の解釈
  ! Parse min
  !
  call match( '#d+', str1, & ! (in)
    &         start, length ) ! (out)
  str2 = str1(start:start+length-1)
  str1 = str1(start+length:)

  call match( '^#.#d+', str1, & ! (in)
    &         start, length ) ! (out)

  if ( length > 0 ) then
    str2 = trim(str2) // str1(start:start+length-1)
    str1 = str1(start+length:)
  end if
  sec = StoD(str2)

  ! UTC からの時差の解釈
  ! Parse time-zone difference
  !
  call match( '[#+-]#d+:#d+', str1, & ! (in)
    &         start, length ) ! (out)
  if ( length > 0 ) then
    zone_pm = str1(start:start)
    str1 = str1(start+1:start+length-1)

    call match( '^#d+:', str1, & ! (in)
      &         start, length ) ! (out)
    zone_hrs = str1(start:start+length-2)
    zone_min = str1(start+length:)
    zone = trim(zone_pm) // trim(zone_hrs) // ':' // trim(zone_min)
  else
    zone = ''
  end if

  call DbgMessage('year=<%d> month=<%d> day=<%d> hour=<%d> min=<%d> sec=<%f>' // &
    & ' zone=<%c>', &
    & i = (/year, month, day, hour, min/), d = (/sec/), &
    & c1 = trim(zone) )

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateParseStr1

