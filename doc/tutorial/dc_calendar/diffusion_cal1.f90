!= Sample program for gtool_history/gtool5
!
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
program diffusion_cal1

  use gtool_history                                 ! Access module (モジュール指定)
  use dc_calendar                               ! Access module (モジュール指定)
  use dc_types                                      ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=1.                   ! Time step [s] (時間間隔 [秒])
  type(DC_CAL_DATE)    :: start_date          ! Start Date (モデル開始日時)
  type(DC_CAL_DATE)    :: end_date            ! End Date (モデル終了日時)
  real(DP)             :: curtime             ! Current time [sec] (現在時刻 [秒])
  real(DP)             :: endtime             ! End time [sec] (終了時刻 [秒])
  real(DP)             :: dsptime             ! Output interval [min] (出力間隔 [分])
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=0.0001            ! Diffusion coefficient (熱拡散係数)


  character(256):: date_str

  call DCCalCreate( cal_type = 'Gregorian' )        ! Set calendar
                                                    ! (暦設定)
  call DCCalDateCreate( year = 2009, month =  2, day = 28, &
    &                   hour =   23, min   = 59, sec = 0.0_DP, &
    &                   date = start_date )
                                                    ! Set date
                                                    ! (日時設定)
  curtime = DCCalConvertToSec(  0.0_DP, 'sec' )
                                                    ! Set initial time 
                                                    ! (初期時刻設定)
  endtime = DCCalConvertToSec( 10.0_DP, 'min' )
                                                    ! Set end time 
                                                    ! (終了時刻設定)
  dsptime = 1.0_DP
                                                    ! Set output interval
                                                    ! (出力間隔設定)

  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &                             ! Create output file 
    & file='diffusion_cal1.nc', &                   ! (ヒストリー作成)
    & title='Diffusion equation',                        &
    & source='Sample program of gtool_history/gtool5',   &
    & institution='GFD_Dennou Club davis project',       &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),               &
    & longnames=(/'X-coordinate','time        '/),       &
    & units=(/'m  ','min'/),                             &
    & origind=curtime, intervald=dsptime )

  call HistoryPut('x',x)                            ! Output 'x' (次元変数出力)

  call HistoryAddVariable( &                        ! Set output variable 
    & varname='temp', dims=(/'x','t'/), &           ! (変数定義) 
    & longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! Output 'temp' (変数出力)

  call DCCalDateInquire( date_str, date = start_date )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)

  call HistoryAddAttr( 't', 'units', 'min since ' // trim(date_str) )
                                                    ! Output start date to a file
                                                    ! (開始日時のファイルへの出力) 

  write(6,*) "Start: ", trim(date_str)              ! Display start date
                                                    ! (開始日時の表示) 

  do while ( curtime < endtime )                    ! Check termination (終了判定)

    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod( curtime, dsptime ) == 0.0_DP ) then   ! Check output timing (出力タイミング判定)
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif

    curtime = curtime + dt                          ! Progress model time (モデル時刻進行)
  enddo


  call DCCalDateInquire( date_str, curtime, start_date )
                                        ! Inquire date and set a character variable
                                        ! (日時を問い合わせて文字型変数へ代入)

  write(6,*) "End:   ", trim(date_str)              ! Display end date (終了日時出力) 

  call HistoryClose
  stop
end program diffusion_cal1
