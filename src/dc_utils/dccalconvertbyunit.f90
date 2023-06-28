function DCCalConvertByUnit1( in_time, in_unit, out_unit, cal ) result( out_time )
  !
  ! 単位の変換を行います. 
  !
  ! 時間の単位として有効な文字列については以下を参照下さい.
  !
  ! dc_calendar_types#UNIT_SEC          :: 秒の単位
  ! dc_calendar_types#UNIT_MIN          :: 分の単位
  ! dc_calendar_types#UNIT_HOUR         :: 時間の単位
  ! dc_calendar_types#UNIT_DAY          :: 日の単位
  ! 
  ! 省略可能引数 *cal* が省略された場合には, dc_calendar 内部で
  ! 保持される暦に関する情報を用いた単位の変換が行われます. 
  ! *cal* が省略されない場合にはその変数に設定された暦の情報を
  ! 用いて単位の変換が行われます. 
  !
  ! Convert of unit. 
  !
  ! Valid strings as units of time are follows. 
  ! 
  ! dc_calendar_types#UNIT_SEC          :: Units of second
  ! dc_calendar_types#UNIT_MIN          :: Units of minute
  ! dc_calendar_types#UNIT_HOUR         :: Units of hour
  ! dc_calendar_types#UNIT_DAY          :: Units of day
  !
  ! If an optional argument *cal* is omitted, 
  ! unit is converted with information of a calendar 
  ! that is stored in the "dc_calendar". 
  ! If *cal* is not omitted, unit is converted with information of the variable.
  !
  use dc_calendar_internal, only: default_cal, default_cal_set, &
    & dccaltype_str, dccaldate_str2usym
  use dc_calendar_types, only: DC_CAL, &
    & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
    & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC
  use dc_calendar_generic, only: DCCalConvertByUnit
  use dc_error, only: StoreError, DC_NOERR, DC_EBADUNIT, DC_ENOTINIT
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_types, only: DP, TOKEN, STRING
  implicit none
  real(DP):: out_time
                              ! 変換後の時間の数値. 
                              ! 
                              ! Numerical value of time after conversion. 

  real(DP), intent(in):: in_time
                              ! 変換前の時間の数値. 
                              ! 
                              ! Numerical value of time before conversion. 
  character(*), intent(in):: in_unit
                              ! 変換前の時間の単位. 
                              ! 
                              ! Units of time before conversion. 
  character(*), intent(in):: out_unit
                              ! 変換後の時間の単位. 
                              ! 
                              ! Units of time after conversion. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 

  ! 作業変数
  ! Work variables
  !
  type(DC_CAL), pointer:: calp =>null()
  real(DP):: in_timew
  integer:: in_unit_sym, out_unit_sym
!!$  integer:: stat
!!$  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalConvertByUnit1'
continue
!!$  call BeginSub( subname )
!!$  stat = DC_NOERR
!!$  cause_c = ''

  out_time = -1.0

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
    call MessageNotify('W', subname, '"cal" is not initialized. <-1> is returned.' )
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 単位の解釈
  ! Parse units
  !
  in_unit_sym  = dccaldate_str2usym( in_unit )
  out_unit_sym = dccaldate_str2usym( out_unit )

  ! 数値の変換
  ! Convert a value
  !
  out_time = DCCalConvertByUnit( in_time, in_unit_sym, out_unit_sym, cal )

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp )
!!$  call StoreError( stat, subname, err, cause_c )
!!$  call EndSub( subname )
end function DCCalConvertByUnit1

!---------------------------------------------------------------------

function DCCalConvertByUnit2( in_time, in_unit, out_unit, cal ) result( out_time )
  !
  ! 単位の変換を行います. 
  !
  ! 時間の単位として有効な整数型変数については以下を参照下さい. 
  ! 単位として整数値を直接与えることはせず, 以下の変数を
  ! 与えてください. 
  !
  ! dc_calendar_types#UNIT_SYMBOL_SEC          :: 秒の単位
  ! dc_calendar_types#UNIT_SYMBOL_MIN          :: 分の単位
  ! dc_calendar_types#UNIT_SYMBOL_HOUR         :: 時間の単位
  ! dc_calendar_types#UNIT_SYMBOL_DAY          :: 日の単位
  ! 
  ! 省略可能引数 *cal* が省略された場合には, dc_calendar 内部で
  ! 保持される暦に関する情報を用いた単位の変換が行われます. 
  ! *cal* が省略されない場合にはその変数に設定された暦の情報を
  ! 用いて単位の変換が行われます. 
  !
  ! Convert of unit. 
  !
  ! Valid integer variables as units of time are follows. 
  ! Do not specify integer directly, but specify following variables. 
  ! 
  ! dc_calendar_types#UNIT_SYMBOL_SEC          :: Units of second
  ! dc_calendar_types#UNIT_SYMBOL_MIN          :: Units of minute
  ! dc_calendar_types#UNIT_SYMBOL_HOUR         :: Units of hour
  ! dc_calendar_types#UNIT_SYMBOL_DAY          :: Units of day
  !
  ! If an optional argument *cal* is omitted, 
  ! unit is converted with information of a calendar 
  ! that is stored in the "dc_calendar". 
  ! If *cal* is not omitted, unit is converted with information of the variable.
  !
  use dc_calendar_internal, only: default_cal, default_cal_set, &
    & dccaltype_str, dccaldate_str2usym
  use dc_calendar_types, only: DC_CAL, &
    & UNIT_SYMBOL_YEAR, UNIT_SYMBOL_MONTH, UNIT_SYMBOL_DAY, &
    & UNIT_SYMBOL_HOUR, UNIT_SYMBOL_MIN, UNIT_SYMBOL_SEC
  use dc_error, only: StoreError, DC_NOERR, DC_EBADUNIT, DC_ENOTINIT
  use dc_message, only: MessageNotify
  use dc_trace, only: BeginSub, EndSub
  use dc_types, only: DP, TOKEN, STRING
  implicit none
  real(DP):: out_time
                              ! 変換後の時間の数値. 
                              ! 
                              ! Numerical value of time after conversion. 
  real(DP), intent(in):: in_time
                              ! 変換前の時間の数値. 
                              ! 
                              ! Numerical value of time before conversion. 
  integer, intent(in):: in_unit
                              ! 変換前の時間の単位. 
                              ! 
                              ! Units of time before conversion. 
  integer, intent(in):: out_unit
                              ! 変換後の時間の単位. 
                              ! 
                              ! Units of time after conversion. 
  type(DC_CAL), intent(in), optional, target:: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 

  ! 作業変数
  ! Work variables
  !
  type(DC_CAL), pointer:: calp =>null()
  real(DP):: in_timew
!!$  integer:: stat
!!$  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalConvertByUnit2'
continue
!!$  call BeginSub( subname )
!!$  stat = DC_NOERR
!!$  cause_c = ''

  out_time = -1.0

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
    call MessageNotify('W', subname, '"cal" is not initialized. <-1> is returned.' )
!!$    stat = DC_ENOTINIT
!!$    cause_c = 'DC_CAL'
    goto 999
  end if

  ! 数値の変換
  ! Convert a value
  !
  select case(in_unit)
  case(UNIT_SYMBOL_DAY)
    in_timew = in_time * calp % hour_in_day &
      &                * calp % min_in_hour &
      &                * calp % sec_in_min
  case(UNIT_SYMBOL_HOUR)
    in_timew = in_time * calp % min_in_hour &
      &                * calp % sec_in_min
  case(UNIT_SYMBOL_MIN)
    in_timew = in_time * calp % sec_in_min
  case(UNIT_SYMBOL_SEC)
    in_timew = in_time
  case default
!    cause_c = in_unit
    call MessageNotify('W', subname, 'in_unit=<%d> is invalid. (ONLY day,hour,min,sec are valid).'  // &
      & ' <-1> is returned.', &
      & i = (/ in_unit /) )
!!$    stat = DC_EBADUNIT
    goto 999
  end select

  select case(out_unit)
  case(UNIT_SYMBOL_DAY)
    out_time = in_timew / calp % hour_in_day &
      &                 / calp % min_in_hour &
      &                 / calp % sec_in_min
  case(UNIT_SYMBOL_HOUR)
    out_time = in_timew / calp % min_in_hour &
      &                 / calp % sec_in_min
  case(UNIT_SYMBOL_MIN)
    out_time = in_timew / calp % sec_in_min
  case(UNIT_SYMBOL_SEC)
    out_time = in_timew
  case default
!    cause_c = out_unit
    call MessageNotify('W', subname, 'out_unit=<%d> is invalid. (ONLY day,hour,min,sec are valid).'  // &
      & ' <-1> is returned.', &
      & i = (/ out_unit /) )
!!$    stat = DC_EBADUNIT
    goto 999
  end select

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  nullify( calp )
!!$  call StoreError( stat, subname, err, cause_c )
!!$  call EndSub( subname )
end function DCCalConvertByUnit2
