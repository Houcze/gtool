!= 日時差の算出. 
!= Evaluate difference of date. 
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldatedifference.f90,v 1.7 2010-09-24 07:07:31 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!
function DCCalDateDifference1( start_date, end_date, cal ) result(sec)
  !
  ! 日時差を算出します. 
  !
  ! 省略可能引数 *cal* が省略された場合には, 日時差の算出に
  ! dc_calendar 内部で保持される暦が用いられます. 
  ! *cal* が省略されない場合にはその変数に設定された暦が用いられます. 
  !
  ! Evaluate difference of date. 
  !
  ! If an optional argument *cal* is omitted, 
  ! information of calendar that is stored in the "dc_calendar" 
  ! is used for evaluation of difference of date. 
  ! If *cal* is not omitted, information of the variable is used. 
  !

  use dc_calendar_internal, only: default_cal, default_cal_set, &
    & dccaltype_str, dccaldate_str2usym
  use dc_calendar_types, only: DC_CAL, DC_CAL_DATE, &
    & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
    & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC, &
    & CAL_USER_DEFINED, &
    & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN, CAL_360DAY
  use dc_error, only: StoreError, DC_NOERR, DC_EBADUNIT, DC_ENOTINIT
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_types, only: DP, TOKEN, STRING
  implicit none
  real(DP):: sec
                              ! *start_date* と *end_date* との差 (秒数). 
                              ! Difference (seconds) between *start_date* and *end_date*.
  type(DC_CAL_DATE), intent(in):: start_date
                              ! 起点となる日時. 
                              ! Date of origin. 
  type(DC_CAL_DATE), intent(in):: end_date
                              ! 終点となる日時. 
                              ! Date of terminus. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 

  ! 作業変数
  ! Work variables
  !
  type(DC_CAL), pointer:: calp =>null()
  real(DP):: start_year, start_day, start_sec, start_neg_offset_day
  real(DP)::   end_year,   end_day,   end_sec,   end_neg_offset_day
  integer:: day_in_4years, day_in_400years
  integer:: start_year_int, end_year_int
  integer:: i, j
  character(*), parameter:: subname = 'DCCalDateDifference1'
continue
  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  if ( present( cal ) ) then
    calp => cal
  else
    calp => default_cal
    if ( .not. calp % initialized ) call default_cal_set
  end if

  ! 初期設定のチェック
  ! Check initialization
  !
  if ( .not. calp % initialized ) then
    sec = 0.0_DP
    return
  end if

  if ( .not. start_date % initialized ) then
    sec = 0.0_DP
    return
  end if

  if ( .not. end_date % initialized ) then
    sec = 0.0_DP
    return
  end if

  start_neg_offset_day = 0
  end_neg_offset_day   = 0

  start_year_int = start_date % year
  end_year_int   = end_date % year

  ! 日への変換
  ! Convert into days
  ! 
  select case( calp % cal_type )
  case( CAL_JULIAN )

    day_in_4years = 1461

    ! 年が負の場合，400 年単位で引き算し下駄を履かせる
    !
    do while ( start_year_int < 1 )
      start_neg_offset_day =   start_neg_offset_day &
        &                    + day_in_4years * 100
      start_year_int       =   start_year_int &
        &                    + 400
    end do

    ! start_date の日への変換
    ! Convert start_date into days
    ! 
    if ( ( start_year_int - 1 ) > 4 ) then
      start_day = int( ( start_year_int - 1 ) / 4 ) * day_in_4years
      start_year = mod( start_year_int - 1, 4 ) + 1
    else
      start_day  = 0
      start_year = start_year_int
    end if

    start_day = start_day + ( start_year - 1 ) * sum ( calp % day_in_month(:) )
    do i = 1, start_date % month - 1
      if ( start_year == 4 .and. i == 2 ) then
        start_day = start_day + 29
      else
        start_day = start_day + calp % day_in_month(i)
      end if
    end do
    start_day = start_day + start_date % day

    ! 年が負の場合，400 年単位で引き算し下駄を履かせる
    !
    do while ( end_year_int < 1 )
      end_neg_offset_day =   end_neg_offset_day &
        &                  + day_in_4years * 100
      end_year_int       =   end_year_int &
        &                  + 400
    end do

    ! end_date の日への変換
    ! Convert end_date into days
    ! 
    if ( ( end_year_int - 1 ) > 4 ) then
      end_day = int( ( end_year_int - 1 ) / 4 ) * day_in_4years
      end_year = mod( end_year_int - 1, 4 ) + 1
    else
      end_day  = 0
      end_year = end_year_int
    end if

    end_day = end_day + ( end_year - 1 ) * sum ( calp % day_in_month(:) )
    do i = 1, end_date % month - 1
      if ( end_year == 4 .and. i == 2 ) then
        end_day = end_day + 29
      else
        end_day = end_day + calp % day_in_month(i)
      end if
    end do
    end_day = end_day + end_date % day

  case( CAL_GREGORIAN )

    day_in_400years = 146097

    ! 年が負の場合，400 年単位で引き算し下駄を履かせる
    !
    do while ( start_year_int < 1 )
      start_neg_offset_day =   start_neg_offset_day &
        &                    + day_in_400years
      start_year_int       =   start_year_int &
        &                    + 400
    end do

    ! start_date の日への変換
    ! Convert start_date into days
    ! 
    if ( ( start_year_int - 1 ) > 400 ) then
      start_day = int( ( start_year_int - 1 ) / 400 ) * day_in_400years
      start_year = mod( start_year_int - 1, 400 ) + 1
    else
      start_day  = 0
      start_year = start_year_int
    end if

    do j = 1, int( start_year - 1 )
      do i = 1, calp % month_in_year
        if ( i == 2 ) then
          if     ( mod( j, 400 ) == 0 ) then
            start_day = start_day + 29
          elseif ( mod( j, 100 ) == 0 ) then
            start_day = start_day + 28
          elseif ( mod( j, 4   ) == 0 ) then
            start_day = start_day + 29
          else
            start_day = start_day + 28
          end if
        else
          start_day = start_day + calp % day_in_month(i)
        end if
      end do
    end do

    do i = 1, start_date % month - 1
      if ( i == 2 ) then
        if     ( mod( start_year, 400.0_DP ) == 0 ) then
          start_day = start_day + 29
        elseif ( mod( start_year, 100.0_DP ) == 0 ) then
          start_day = start_day + 28
        elseif ( mod( start_year, 4.0_DP   ) == 0 ) then
          start_day = start_day + 29
        else
          start_day = start_day + 28
        end if
      else
        start_day = start_day + calp % day_in_month(i)
      end if
    end do

    start_day = start_day + start_date % day

    ! 年が負の場合，400 年単位で引き算し下駄を履かせる
    !
    do while ( end_year_int < 1 )
      end_neg_offset_day =   end_neg_offset_day &
        &                  + day_in_400years
      end_year_int       =   end_year_int &
        &                  + 400
    end do

    ! end_date の日への変換
    ! Convert end_date into days
    ! 
    if ( ( end_year_int - 1 ) > 400 ) then
      end_day = int( ( end_year_int - 1 ) / 400 ) * day_in_400years
      end_year = mod( end_year_int - 1, 400 ) + 1
    else
      end_day  = 0
      end_year = end_year_int
    end if

    do j = 1, int( end_year - 1 )
      do i = 1, calp % month_in_year
        if ( i == 2 ) then
          if     ( mod( j, 400 ) == 0 ) then
            end_day = end_day + 29
          elseif ( mod( j, 100 ) == 0 ) then
            end_day = end_day + 28
          elseif ( mod( j, 4   ) == 0 ) then
            end_day = end_day + 29
          else
            end_day = end_day + 28
          end if
        else
          end_day = end_day + calp % day_in_month(i)
        end if
      end do
    end do

    do i = 1, end_date % month - 1
      if ( i == 2 ) then
        if     ( mod( end_year, 400.0_DP ) == 0 ) then
          end_day = end_day + 29
        elseif ( mod( end_year, 100.0_DP ) == 0 ) then
          end_day = end_day + 28
        elseif ( mod( end_year, 4.0_DP   ) == 0 ) then
          end_day = end_day + 29
        else
          end_day = end_day + 28
        end if
      else
        end_day = end_day + calp % day_in_month(i)
      end if
    end do

    end_day = end_day + end_date % day

  case default
    ! start_date の日への変換
    ! Convert start_date into days
    ! 
    start_day = ( start_year_int - 1 ) * sum ( calp % day_in_month(:) )
    do i = 1, start_date % month - 1
      start_day = start_day + calp % day_in_month(i)
    end do
    start_day = start_day + start_date % day

    ! end_date の日への変換
    ! Convert end_date into days
    ! 
    end_day = ( end_year_int - 1 ) * sum ( calp % day_in_month(:) )
    do i = 1, end_date % month - 1
      end_day = end_day + calp % day_in_month(i)
    end do
    end_day = end_day + end_date % day
  end select

  ! start_date の秒への変換
  ! Convert start_date into seconds
  ! 
  start_sec =   ( start_day - 1 - start_neg_offset_day ) &
      &                           * calp % hour_in_day &
      &                           * calp % min_in_hour &
      &                           * calp % sec_in_min  &
      &       + start_date % hour * calp % min_in_hour &
      &                           * calp % sec_in_min  &
      &       + start_date % min  * calp % sec_in_min  &
      &       + start_date % sec

  ! end_date の秒への変換
  ! Convert end_date into seconds
  ! 
  end_sec =   ( end_day - 1 - end_neg_offset_day ) &
      &                       * calp % hour_in_day &
      &                       * calp % min_in_hour &
      &                       * calp % sec_in_min  &
      &     + end_date % hour * calp % min_in_hour &
      &                       * calp % sec_in_min  &
      &     + end_date % min  * calp % sec_in_min  &
      &     + end_date % sec

  ! 差分の計算
  ! Calculate difference
  !
  sec = end_sec - start_sec

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp )
end function DCCalDateDifference1


!!$
!!$
!!$subroutine DCCalConvertByUnit1( in_time, in_unit, out_unit, out_time, cal, err )
!!$  use dc_calendar_internal, only: default_cal, default_cal_set, &
!!$    & dccaltype_str, dccaldate_str2usym
!!$  use dc_calendar_types, only: DC_CAL, &
!!$    & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
!!$    & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC
!!$  use dc_error, only: StoreError, DC_NOERR, DC_EBADUNIT, DC_ENOTINIT
!!$  use dc_message, only: MessageNotify
!!$  use dc_trace, only: BeginSub, EndSub
!!$  use dc_types, only: DP, TOKEN, STRING
!!$  implicit none
!!$  real(DP), intent(in):: in_time
!!$  character(*), intent(in):: in_unit
!!$  character(*), intent(in):: out_unit
!!$  real(DP), intent(out):: out_time
!!$  type(DC_CAL), intent(in), optional, target:: cal
!!$  logical, intent(out), optional:: err
!!$                              ! 例外処理用フラグ. 
!!$                              ! デフォルトでは, この手続き内でエラーが
!!$                              ! 生じた場合, プログラムは強制終了します. 
!!$                              ! 引数 *err* が与えられる場合, 
!!$                              ! プログラムは強制終了せず, 代わりに
!!$                              ! *err* に .true. が代入されます. 
!!$                              !
!!$                              ! Exception handling flag. 
!!$                              ! By default, when error occur in 
!!$                              ! this procedure, the program aborts. 
!!$                              ! If this *err* argument is given, 
!!$                              ! .true. is substituted to *err* and 
!!$                              ! the program does not abort. 
!!$
!!$  ! 作業変数
!!$  ! Work variables
!!$  !
!!$  type(DC_CAL), pointer:: calp =>null()
!!$  real(DP):: in_timew
!!$  integer:: in_unit_sym, out_unit_sym
!!$  integer:: stat
!!$  character(STRING):: cause_c
!!$  character(*), parameter:: subname = 'DCCalConvertByUnit1'
!!$continue
!!$  call BeginSub( subname )
!!$  stat = DC_NOERR
!!$  cause_c = ''
!!$
!!$  ! オブジェクトのポインタ割付
!!$  ! Associate pointer of an object
!!$  !
!!$  if ( present( cal ) ) then
!!$    calp => cal
!!$  else
!!$    calp => default_cal
!!$    if ( .not. calp % initialized ) call default_cal_set
!!$  end if
!!$
!!$  ! 初期設定のチェック
!!$  ! Check initialization
!!$  !
!!$  if ( .not. calp % initialized ) then
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL'
!!$    goto 999
!!$  end if
!!$
!!$  ! 単位の解釈
!!$  ! Parse units
!!$  !
!!$  in_unit_sym  = dccaldate_str2usym( in_unit )
!!$  out_unit_sym = dccaldate_str2usym( out_unit )
!!$
!!$  ! 数値の変換
!!$  ! Convert a value
!!$  !
!!$  select case(in_unit_sym)
!!$  case(UNIT_SYMBOL_DAY)
!!$    in_timew = in_time * calp % hour_in_day &
!!$      &                * calp % min_in_hour &
!!$      &                * calp % sec_in_min
!!$  case(UNIT_SYMBOL_HOUR)
!!$    in_timew = in_time * calp % min_in_hour &
!!$      &                * calp % sec_in_min
!!$  case(UNIT_SYMBOL_MIN)
!!$    in_timew = in_time * calp % sec_in_min
!!$  case(UNIT_SYMBOL_SEC)
!!$    in_timew = in_time
!!$  case default
!!$    cause_c = in_unit
!!$    call MessageNotify('W', subname, 'in_unit=<%c> is invalid. (ONLY day,hour,min,sec are valid)', &
!!$      & c1 = trim(in_unit) )
!!$    stat = DC_EBADUNIT
!!$    goto 999
!!$  end select
!!$
!!$  select case(out_unit_sym)
!!$  case(UNIT_SYMBOL_DAY)
!!$    out_time = in_timew / calp % hour_in_day &
!!$      &                 / calp % min_in_hour &
!!$      &                 / calp % sec_in_min
!!$  case(UNIT_SYMBOL_HOUR)
!!$    out_time = in_timew / calp % min_in_hour &
!!$      &                 / calp % sec_in_min
!!$  case(UNIT_SYMBOL_MIN)
!!$    out_time = in_timew / calp % sec_in_min
!!$  case(UNIT_SYMBOL_SEC)
!!$    out_time = in_timew
!!$  case default
!!$    cause_c = out_unit
!!$    call MessageNotify('W', subname, 'out_unit=<%c> is invalid. (ONLY day,hour,min,sec are valid)', &
!!$      & c1 = trim(out_unit) )
!!$    stat = DC_EBADUNIT
!!$    goto 999
!!$  end select
!!$
!!$  ! 終了処理, 例外処理
!!$  ! Termination and Exception handling
!!$  !
!!$999 continue
!!$  nullify( calp )
!!$  call StoreError( stat, subname, err, cause_c )
!!$  call EndSub( subname )
!!$end subroutine DCCalConvertByUnit1
