!= Sample program for gtool_historyauto/gtool5
!
! * 2010/08/26 Y.Morikawa
! * 2009/10/18 Y.Morikawa
! * 2003/08/21 M.Odaka
! * 2001/02/27 S.Takehiro
!
! Solving diffusion equation
! \[
!     du/dt = \kappa d^2 u/dx^2
! \]
! for giving values of $u$ at $x=[0,1]$.
!
program diffusion_cal2

  use gtool_historyauto                       ! Access module (モジュール指定)
  use dc_calendar                             ! Access module (モジュール指定)
  use dc_types                                ! Access module (モジュール指定)

  integer, parameter     :: nx=30             ! Grid number   (グリッド数)
  real(DP), parameter    :: dx=1.0/(nx-1)     ! Grid interval (グリッド間隔)
  type(DC_CAL)         :: cal_gre             ! Gregorian Calendar (グレゴリオ暦)
  type(DC_CAL)         :: cal_mars            ! Martian Calendar   (火星っぽい暦)
  type(DC_CAL_DATE)    :: start_date          ! Start Date (モデル開始日時)
  type(DC_CAL_DATE)    :: end_date            ! End Date   (モデル終了日時)
  real(DP)             :: dt                  ! Time step    [sec] (時間間隔 [秒])
  real(DP)             :: curtime             ! Current time [sec] (経過時刻 [秒])
  real(DP)             :: endtime             ! End time     [sec] (終了時刻 [秒])
  real(DP)             :: dsptime             ! Output interval [day] (出力間隔 [日])

  real(DP)             :: sec_of_year         ! Second of year (年始めからの通秒)
  real(DP)             :: day_of_year         ! Day of year    (年始めからの通日)
  real(DP)             :: sec_of_day          ! Second of day  (日始めからの通秒)
  logical              :: flag_leapyear       ! Leap year      (閏年)

  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0e-9            ! Diffusion coefficient (熱拡散係数)

  character(256):: date_str1, date_str2

  character(32):: cal_type
  integer:: month_in_year, day_in_month(1:12), hour_in_day, min_in_hour 
  integer, pointer:: day_in_month_ptr(:) =>null()
  real(DP):: sec_in_min

  call DCCalCreate( cal_type = 'Gregorian', cal = cal_gre )
  call DCCalCreate( month_in_year = 1 , &
    &               day_in_month  = (/669/), &
    &               hour_in_day   = 24, &
    &               min_in_hour   = 1 , &
    &               sec_in_min    = 3694.0_DP, &
    &               cal           = cal_mars )
                                                    ! Set calendar
                                                    ! (暦設定)

  call DCCalDateCreate( year = 2000, month =  1, day = 1, &
    &                   hour =    0, min   =  0, sec = 0.0_DP, &
    &                   date = start_date )
  call DCCalDateCreate( year = 2001, month =  1, day = 1, &
    &                   hour =    0, min   =  0, sec = 0.0_DP, &
    &                   date = end_date )
                                                    ! Set date
                                                    ! (日時設定)

  curtime = DCCalConvertByUnit(  0.0_DP, 'sec', 'sec', cal = cal_mars )
                                                    ! Set initial time 
                                                    ! (初期時刻設定)
  endtime = DCCalDateDifference( start_date, end_date, cal = cal_mars )
                                                    ! Set end time 
                                                    ! (終了時刻設定)
  dt      = DCCalConvertByUnit(  1.0_DP, 'day', 'sec', cal = cal_mars )
                                                    ! Set time step
                                                    ! (時間ステップ設定)
  dsptime = 10
                                                    ! Set output interval
                                                    ! (出力間隔設定)

  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryAutoCreate( &                         ! Create output file 
    & title='Diffusion equation',                &  ! (ヒストリー作成)
    & source='Sample program of gtool_historyauto/gtool5',   &
    & institution='GFD_Dennou Club davis project',           &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),                   &
    & longnames=(/'X-coordinate','time        '/),           &
    & units=(/'m  ','day'/),                                 &
    & origin=curtime, terminus=endtime, interval=dsptime,    &
    & cal=cal_mars, start_date=start_date )

  call HistoryAutoPutAxis('x',x)                    ! Output 'x' (次元変数出力)

  call HistoryAutoAddVariable( &                    ! Set output variable 
    & varname='temp', dims=(/'x','t'/), &           ! (変数定義) 
    & longname='temperature', units='K', xtype='double', &
    & file='diffusion_cal2.nc' )

  call HistoryAutoPut(curtime,'temp',temp)          ! Output 'temp' (変数出力)

  write(6,*) "Calendars: "

  call DCCalInquire( cal_type, &
    &                month_in_year    = month_in_year, &
    &                day_in_month_ptr = day_in_month_ptr, &
    &                hour_in_day      = hour_in_day, &
    &                min_in_hour      = min_in_hour, &
    &                sec_in_min       = sec_in_min, &
    &                cal              = cal_mars )
                                        ! Inquire calendar
                                        ! (暦に関して問い合わせ)

  write(6,*) "       Martian Cal:   month in year:  ", month_in_year
  write(6,*) "                      days in months: ", day_in_month_ptr
  write(6,*) "                      hour in day:    ", hour_in_day
  write(6,*) "                      min in hour:    ", min_in_hour
  write(6,*) "                      sec in min:     ", sec_in_min
  deallocate(day_in_month_ptr)

  call DCCalInquire( cal_type, &
    &                month_in_year    = month_in_year, &
    &                day_in_month     = day_in_month, &
    &                hour_in_day      = hour_in_day, &
    &                min_in_hour      = min_in_hour, &
    &                sec_in_min       = sec_in_min, &
    &                cal              = cal_gre )
                                        ! Inquire calendar
                                        ! (暦に関して問い合わせ)

  write(6,*) "       Gregorian Cal: month in year:  ", month_in_year
  write(6,*) "                      days in months: ", day_in_month
  write(6,*) "                      hour in day:    ", hour_in_day
  write(6,*) "                      min in hour:    ", min_in_hour
  write(6,*) "                      sec in min:     ", sec_in_min

  write(6,*) ""

  call DCCalDateInquire( date_str1, date = start_date )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)

  write(6,*) "Start: ", trim(date_str1)
                                                    ! Display start date
                                                    ! (開始日時の表示) 

  sec_of_year   = DCCalDateEvalSecOfYear( curtime, start_date, cal_mars )
                                        ! Second of year (年始めからの通秒)
  day_of_year   = DCCalDateEvalDayOfYear( curtime, start_date, cal_mars )
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = DCCalDateEvalSecOfDay( curtime, start_date, cal_mars )
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = DCCalDateChkLeapYear( curtime, start_date, cal_mars )
                                        ! Leap year      (閏年)

  write(6,*) "       Martian Cal:   second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear

  sec_of_year   = DCCalDateEvalSecOfYear( curtime, start_date, cal_gre )
                                        ! Second of year (年始めからの通秒)
  day_of_year   = DCCalDateEvalDayOfYear( curtime, start_date, cal_gre )
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = DCCalDateEvalSecOfDay( curtime, start_date, cal_gre )
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = DCCalDateChkLeapYear( curtime, start_date, cal_gre )
                                        ! Leap year      (閏年)

  write(6,*) "       Gregorian Cal: second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear


  do while ( curtime < endtime )                    ! Check termination (終了判定)

    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    call HistoryAutoPut(curtime,'temp',temp)             ! Output 'temp' (変数出力)

    curtime = curtime + dt                          ! Progress model time (モデル時刻進行)
  enddo


  call DCCalDateInquire( date_str1, curtime, start_date, cal = cal_mars )
  call DCCalDateInquire( date_str2, curtime, start_date, cal = cal_gre )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)

  write(6,*) "End:   ", trim(date_str1), " (Mars), ", trim(date_str2), " (Gregorian)"
                                                    ! Display end date (終了日時出力) 

  sec_of_year   = DCCalDateEvalSecOfYear( curtime, start_date, cal_mars )
                                        ! Second of year (年始めからの通秒)
  day_of_year   = DCCalDateEvalDayOfYear( curtime, start_date, cal_mars )
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = DCCalDateEvalSecOfDay( curtime, start_date, cal_mars )
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = DCCalDateChkLeapYear( curtime, start_date, cal_mars )
                                        ! Leap year      (閏年)

  write(6,*) "       Martian Cal:   second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear

  sec_of_year   = DCCalDateEvalSecOfYear( curtime, start_date, cal_gre )
                                        ! Second of year (年始めからの通秒)
  day_of_year   = DCCalDateEvalDayOfYear( curtime, start_date, cal_gre )
                                        ! Day of year    (年始めからの通日)
  sec_of_day    = DCCalDateEvalSecOfDay( curtime, start_date, cal_gre )
                                        ! Second of day  (日始めからの通秒)
  flag_leapyear = DCCalDateChkLeapYear( curtime, start_date, cal_gre )
                                        ! Leap year      (閏年)

  write(6,*) "       Gregorian Cal: second of year: ", sec_of_year
  write(6,*) "                      day of year:    ", day_of_year
  write(6,*) "                      second of day:  ", sec_of_day
  write(6,*) "                      leap year:      ", flag_leapyear

  call HistoryAutoClose
  stop
end program diffusion_cal2
