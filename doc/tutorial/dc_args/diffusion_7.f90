!= Sample program for gtool_history/gtool5
!
! * 2007/06/27 M.Odaka
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
program diffusion_7

  use gtool_history                                   ! Access module (モジュール指定)
  use dc_types, only : DP, STRING                   ! Access module (モジュール指定)
  use dc_message, only: MessageNotify               ! Access module (モジュール指定)
  use dc_string, only : StoA, StoI                  ! Access module (モジュール指定)
  use dc_args,  only : ARGS, Open, Debug, Help, Strict, Close, Option
                                                    ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)
  type(ARGS)             :: arg                     ! Command line argument (引数)
  logical                :: OPT_step                ! logical parameter (引数用論理変数)
  character(STRING)      :: VAL_step                ! Value of command line argument (引数の値)

  call Open(arg)                             ! Initialize variable 'arg' 
                                             ! (引数の初期化) 
  call Option(arg, StoA('-S', '--step'), OPT_step, VAL_step, &
    &         help="Specify time step (nt) [default value is 200]." )
                                             ! Set "-S/--step" option
                                             ! ("-S/--step" オプションの設定)

  call Debug(arg)                            ! Set debug option
                                             ! (デバッグオプションの自動設定)
  call Help(arg)                             ! Set help option
                                             ! ヘルプオプションの自動設定
  call Strict(arg)                           ! Set exception handling
                                             ! (無効なオプション指定時に警告表示)

  if (OPT_step) then
     nt = StoI(VAL_step)                     ! Set "nt" specified by command line argument 
                                             ! (引数の値を入力)

     call MessageNotify( "M", "diffusion_7", &
       &                 "Time step is %d", i=(/nt/) )
                                             ! Message dump
                                             ! (メッセージの出力)
  end if

  call Close(arg)                            ! Finalize variable 'arg'
                                             ! (引数の使用終了)

  tinit = 0.0                                       ! Set initial Time 
                                                    ! (初期時刻設定)

  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &                             ! Create output file 
    & file='diffusion_7.nc', &                      ! (ヒストリー作成)
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

  call HistoryPut('temp',temp)                      ! Output 'temp' (変数出力)

  do it=1,nt
    temp(2:nx-1) = temp(2:nx-1) &                   ! Time integration (時間積分)
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(it,ndisp) == 0 ) then
      call HistoryPut('temp',temp)                  ! Output 'temp' (変数出力)
    endif
  enddo

  call HistoryClose
  stop
end program diffusion_7

