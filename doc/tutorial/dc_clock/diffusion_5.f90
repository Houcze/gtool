!= Sample program for gtool_history/gtool5
!
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
program diffusion_5

  use gtool_history                                   ! Access module (モジュール指定)
  use dc_types, only : DP                           ! Access module (モジュール指定)
  use dc_clock, only : CLOCK, Create, Close, Start, &
    &            Stop, Result, Predict, operator(+) ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)
  type(CLOCK)            :: clock_init, clock_loop  ! Variable for CPU time counting 
                                                    ! CPU 時間計測用変数

  call Create( clk = clock_init, &                  ! Initialize (初期化)
    &          name = 'initialization' )
  call Create( clk = clock_loop, &                  ! Initialize (初期化)
    &          name = 'time-integration' )

  call Start( clk = clock_init )                    ! Start CPU time counting
                                                    ! (CPU 時間計測開始)

  tinit = 0.0                                       ! Set initial time 
                                                    ! (初期時刻設定)
    
  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &                             ! Create output file 
    & file='diffusion_5.nc', &                      ! (ヒストリー作成)
    & title='Diffusion equation',                        &
    & source='Sample program of gtool_history/gtool5',   &
    & institution='GFD_Dennou Club davis project',       &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),               &
    & longnames=(/'X-coordinate','time        '/),       &
    & units=(/'m','s'/),                                 &
    & origin=real(tinit), interval=real(ndisp*dt) )

  call HistoryPut('x',x)                            ! Output 'x' (次元変数出力)

  call HistoryAddVariable( &                        ! Set output variable
    & varname='temp', dims=(/'x','t'/), &           ! (変数定義)
    & longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! Output 'temp (変数出力)

  call Stop( clk = clock_init )                     ! Stop CPU time counting
                                                    ! (CPU 時間計測終了)

  do it=1,nt
    call Start ( clk = clock_loop )                 ! Start CPU time counting
                                                    ! (CPU 時間計測開始)

    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(it,ndisp) == 0 ) then
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif

    call Stop( clk = clock_loop )                   ! Stop CPU Time counting
                                                    ! CPU 時間計測終了

    call Predict( clk = clock_init + clock_loop, &  ! Estimate remaining time
      &            progress = real(it)/real(nt) )   ! (残り時間の予測)

  end do

  call HistoryClose

  call Result( clks = (/clock_init, clock_loop/), & ! Display total CPU time
    &          total_auto = .true. )                ! (全 CPU 時間の表示) 

  call Close( clk = clock_init )                    ! Finalize (後処理)
  call Close( clk = clock_loop )                    ! Finalize (後処理)

  stop
end program diffusion_5

