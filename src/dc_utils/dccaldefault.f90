!= デフォルトの暦情報の取得
!= Get information of default calendar
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dccaldefault.f90,v 1.2 2009-10-17 14:08:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2009-. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルに記載される手続き群は dc_calendar モジュールから提供されます. 
!
! Procedures described in this file are provided from "dc_calendar" module. 
!

subroutine DCCalDefault1( cal )
  !
  ! dc_calendar においてデフォルト設定となっている暦を返します. 
  ! このデフォルトの暦は, dc_calendar_generic#DCCalCreate 
  ! において省略可能引数 *cal* を省略して指定された暦が該当します. 
  ! ただし DCCalCreate が呼び出されていない場合にはグレゴリオ暦となります. 
  !
  ! Default calender in "dc_calendar" is returned. 
  ! The default calender is set by dc_calendar_generic#DCCalCreate 
  ! without optional argument *cal*. 
  ! If the DCCalCreate is called, the calendar becomes Gregorian calendar. 
  !
  use dc_calendar_types, only: DC_CAL, &
    & CAL_CYCLIC, CAL_NOLEAP, CAL_JULIAN, CAL_GREGORIAN, CAL_360DAY
  use dc_calendar_internal, only: default_cal, default_cal_set
  use dc_message, only: MessageNotify
  use dc_string, only: LChar
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADCALTYPE
  use dc_types, only: STRING, DP
  implicit none
  type(DC_CAL), intent(out):: cal
                              ! 暦情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! calendar. 

  ! 作業変数
  ! Work variables
  !
  type(DC_CAL), pointer:: calp =>null()
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDefault1'
continue
  call BeginSub( subname )

  ! オブジェクトのポインタ割付
  ! Associate pointer of an object
  !
  calp => default_cal
  if ( .not. calp % initialized ) call default_cal_set

!!$  ! 初期設定のチェック
!!$  ! Check initialization
!!$  !
!!$  if ( calp % initialized ) then
!!$    stat = DC_EALREADYINIT
!!$    cause_c = 'DC_CAL'
!!$    goto 999
!!$  end if

  ! 各要素への値の設定
  ! Configure elements
  !
  cal % cal_type = calp % cal_type

  allocate( cal % day_in_month( calp % month_in_year ) )
  cal % month_in_year = calp % month_in_year
  cal % day_in_month  = calp % day_in_month 
  cal % hour_in_day   = calp % hour_in_day  
  cal % min_in_hour   = calp % min_in_hour  
  cal % sec_in_min    = calp % sec_in_min   

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
  cal % initialized = .true.
999 continue
  nullify( calp )
  call EndSub( subname )
end subroutine DCCalDefault1

