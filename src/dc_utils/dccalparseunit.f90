!= 単位の解釈
!= Parse units
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccalparseunit.f90,v 1.2 2009-10-17 14:08:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!

subroutine DCCalParseUnit1( unit_str, unit_sym, err )
  !
  ! 文字列変数の日時単位 *unit_str* を整数型変数の日時単位
  ! *unit_sym* に変換します. 
  ! *unit_str* として有効な文字列は以下の通りです. 
  !
  ! dc_calendar_types#UNIT_SEC          :: 秒の単位
  ! dc_calendar_types#UNIT_MIN          :: 分の単位
  ! dc_calendar_types#UNIT_HOUR         :: 時間の単位
  ! dc_calendar_types#UNIT_DAY          :: 日の単位
  ! dc_calendar_types#UNIT_MONTH        :: 月の単位
  ! dc_calendar_types#UNIT_YEAR         :: 年の単位
  ! 
  ! 有効な文字列が与えられた場合, *uni_sym* に以下の変数に相当する
  ! 整数が返ります. 
  !
  ! dc_calendar_types#UNIT_SYMBOL_SEC          :: 秒の単位
  ! dc_calendar_types#UNIT_SYMBOL_MIN          :: 分の単位
  ! dc_calendar_types#UNIT_SYMBOL_HOUR         :: 時間の単位
  ! dc_calendar_types#UNIT_SYMBOL_DAY          :: 日の単位
  ! dc_calendar_types#UNIT_SYMBOL_MONTH        :: 月の単位
  ! dc_calendar_types#UNIT_SYMBOL_YEAR         :: 年の単位
  !
  ! 無効な値が与えられた場合, エラーを生じます. 
  !
  ! Parse a character variable of units of date *unit_str*, and 
  ! return an integer variable of units of date *unit_sym*. 
  ! Valid strings as *unit_str* are as follows. 
  !
  ! dc_calendar_types#UNIT_SEC          :: Units of second
  ! dc_calendar_types#UNIT_MIN          :: Units of minute
  ! dc_calendar_types#UNIT_HOUR         :: Units of hour
  ! dc_calendar_types#UNIT_DAY          :: Units of day
  ! dc_calendar_types#UNIT_MONTH        :: Units of month
  ! dc_calendar_types#UNIT_YEAR         :: Units of year
  !
  ! When a valid string is specified, an integer corresponding 
  ! one of following variables is returned to *unit_sym*. 
  !
  ! dc_calendar_types#UNIT_SYMBOL_SEC          :: Units of second
  ! dc_calendar_types#UNIT_SYMBOL_MIN          :: Units of minute
  ! dc_calendar_types#UNIT_SYMBOL_HOUR         :: Units of hour
  ! dc_calendar_types#UNIT_SYMBOL_DAY          :: Units of day
  ! dc_calendar_types#UNIT_SYMBOL_MONTH        :: Units of month
  ! dc_calendar_types#UNIT_SYMBOL_YEAR         :: Units of year
  !
  ! If an invalid string is specified an error is caused. 
  !

  use dc_calendar_types, only: UNIT_SYMBOL_ERR
  use dc_calendar_internal, only: dccaldate_str2usym
  use dc_message, only: MessageNotify
  use dc_string, only: LChar
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADCALTYPE, DC_EBADUNIT
  use dc_types, only: STRING, DP
  implicit none
  character(*), intent(in):: unit_str
  integer, intent(out):: unit_sym
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
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalParseUnit1'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

  ! 単位の文字列をシンボル (整数型変数) に変換
  ! Convert strings of units into symbols (integer variables)
  !
  unit_sym = dccaldate_str2usym( unit_str )

  ! エラー処理
  ! Error Handling
  !
  if ( unit_sym == UNIT_SYMBOL_ERR ) then
    call MessageNotify('W', subname, 'unit_str=<%c> is invalid. (ONLY day,hour,min,sec are valid)', &
      & c1 = trim(unit_str) )
    stat = DC_EBADUNIT
    goto 999
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalParseUnit1
