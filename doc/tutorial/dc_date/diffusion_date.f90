!= Sample program for gtool_history/gtool5
!
! * 2009/02/28 Y.Morikawa
! * 2007/06/25 M.Odaka
! * 2006/10/25 Y.Morikawa
! * 2003/08/21 M.Odaka
! * 2001/02/27 S.Takehiro
!
! Solving diffusion equation
! \[
!     du/dt = \kappa d^2 u/dx^2
! \]
! for giving values of $u$ at $x=[0,1]$.
!
program diffusion_date

  use gtool_history                                 ! Access module (モジュール指定)
  use dc_date                                       ! Access module (モジュール指定)
  use dc_types                                      ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=1.                   ! Time step [s] (時間間隔 [秒])
  type(DC_DIFFTIME)      :: deltime                 ! Time step (時間間隔)
  type(DC_DIFFTIME)      :: endtime                 ! End time (終了時刻)
  type(DC_DIFFTIME)      :: dsptime                 ! Output interval (出力間隔)
  type(DC_DIFFTIME)      :: curtime                 ! Current time (現在時刻)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=0.0001            ! Diffusion coefficient (熱拡散係数)

  type(DC_DATETIME):: date                          ! Date (日時)
  character(256):: date_str

  call DCDateTimeCreate(date, year=2009, mon=2, day=28, hour=23, min=59)
                                                    ! Set date
                                                    ! (日時設定)

  call DCDiffTimeCreate(curtime, 0., 'sec')         ! Set initial time 
                                                    ! (初期時刻設定)
  call DCDiffTimeCreate(deltime, dt, 'sec')         ! Set time step
                                                    ! (時間間隔設定)
  call DCDiffTimeCreate(endtime, 10., 'min')        ! Set end time 
                                                    ! (終了時刻設定)
  call DCDiffTimeCreate(dsptime, 0.1, 'min')        ! Set output interval
                                                    ! (出力間隔設定)

  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &                             ! Create output file 
    & file='diffusion_date.nc', &                   ! (ヒストリー作成)
    & title='Diffusion equation',                        &
    & source='Sample program of gtool_history/gtool5',   &
    & institution='GFD_Dennou Club davis project',       &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),               &
    & longnames=(/'X-coordinate','time        '/),       &
    & units=(/'m  ','min'/),                             &
    & origin=curtime, interval=dsptime )

  call HistoryPut('x',x)                            ! Output 'x' (次元変数出力)

  call HistoryAddVariable( &                        ! Set output variable 
    & varname='temp', dims=(/'x','t'/), &           ! (変数定義) 
    & longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! Output 'temp' (変数出力)

  date_str = toChar(date)
  write(6,*) "Start: ", trim(date_str)              ! Display start date (開始日時出力) 

  do while ( curtime <= endtime )                   ! Check termination (終了判定)

    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod( curtime, dsptime ) == 0 ) then        ! Check output timing (出力タイミング判定)
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif

    curtime = curtime + deltime                     ! Progress model time (モデル時刻進行)
  enddo

  date = date + curtime                             ! Evaluate end date (終了日時算出) 
  date_str = toChar(date)                           ! Convert date into characters (日時を文字型変数へ変換)
  write(6,*) "End:   ", trim(date_str)              ! Display end date (終了日時出力) 

  call HistoryClose
  stop
end program diffusion_date
