subroutine DCCalDateCurrent1( date, err )
  !
  ! 実時間を dc_calendar_types#DC_CAL_DATE 型の
  ! date に返します. 
  ! 実時間は Fortran 90 以降の組み込みサブルーチンである 
  ! date_and_time から得られます. 
  !
  ! Return actual time +date+ (type "dc_calendar_types#DC_CAL_DATE"). 
  ! The actual time is acquired by "date_and_time" that is 
  ! a built-in subroutine of Fortran 90 or more. 
  !
  use dc_calendar_generic, only: DCCalDateParseStr, DCCalDateCreate
  use dc_calendar_types, only: DC_CAL_DATE
  use dc_calendar_internal, only: default_date
  use dc_message, only: MessageNotify
  use dc_types, only: DP, TOKEN
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, DC_EBADDATE
  use dc_types, only: STRING
  implicit none
  type(DC_CAL_DATE), intent(out):: date
                              ! 実時間の日時情報を収めたオブジェクト. 
                              ! 
                              ! An object that stores information of 
                              ! date and time of actual time. 
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
  integer :: date_time_values(1:8)
  character(5)  :: zone_raw

  integer:: year  ! 年. Year.
  integer:: month ! 月. Month.
  integer:: day   ! 日. Day.
  integer:: hour  ! 時. Hour.
  integer:: min   ! 分. Minute.
  real(DP):: sec  ! 秒. Second.
  character(TOKEN):: zone
                  ! UTC からの時差. Time-zone.
  integer:: stat
  character(STRING):: cause_c
  character(*), parameter:: subname = 'DCCalDateCurrent1'
continue
  call BeginSub( subname )
  stat = DC_NOERR
  cause_c = ''

!!$  ! 初期設定のチェック
!!$  ! Check initialization
!!$  !
!!$  if ( datep % initialized ) then
!!$    stat = DC_EALREADYINIT
!!$    cause_c = 'DC_CAL_DATE'
!!$    goto 999
!!$  end if


  ! date_and_time 組み込みサブルーチンを用いて, 現在
  ! 時刻と UTC からの時差を取得. 
  !
  call date_and_time(zone=zone_raw, values=date_time_values)
  zone = zone_raw(1:3) // ":" // zone_raw(4:5)


  ! オブジェクトの作成
  ! Create an object
  !
  call DCCalDateCreate( &
    & date_time_values(1), date_time_values(2), date_time_values(3), & ! (in)
    & date_time_values(5), date_time_values(6), &                      ! (in)
    & real( date_time_values(7), DP ), & ! (in)
    & date, zone, err = err )            ! (out) optional
  if ( present(err) ) then
    if ( err ) then
      stat = DC_EBADDATE
      goto 999
    end if
  end if

  ! 終了処理, 例外処理
  ! Termination and Exception handling
  !
999 continue
  call StoreError( stat, subname, err, cause_c )
  call EndSub( subname )
end subroutine DCCalDateCurrent1
