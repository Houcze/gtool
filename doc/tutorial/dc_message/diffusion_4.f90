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
program diffusion_4

  use gtool_history                                   ! Access module (モジュール指定)
  use dc_types, only : DP                           ! Access module (モジュール指定)
  use dc_message, only : MessageNotify              ! Access module (モジュール指定)

  integer, parameter     :: nx=30                   ! Grid number (グリッド数)
  integer, parameter     :: nt=200                  ! Time step number (時間ステップ数)
  integer, parameter     :: ndisp=10                ! Output interval (出力間隔)
  real(DP), parameter    :: dx=1.0/(nx-1)           ! Grid interval (グリッド間隔)
  real(DP), parameter    :: dt=0.0005               ! Time step (時間間隔)
  real(DP), dimension(nx):: x=(/(dx*(i-1),i=1,nx)/) ! X coordinate (座標変数)
  real(DP), dimension(nx):: temp                    ! Temperature (温度)
  real(DP), parameter    :: kappa=1.0               ! Diffusion coefficient (熱拡散係数)
  real(DP)               :: sigma                   ! Parameter (計算安定条件パラメタ)

  tinit = 0.0                                       ! Set initial Time 
                                                    ! (初期時刻設定)

  sigma = kappa*dt/dx**2.0d0

  if ( sigma >= 0.5d0 ) then
    call MessageNotify( "E", &                      ! Error mesage dump 
      &                 "diffusion_4", &            !(エラーメッセージ出力 )
      &                 "dt is too large: k*dt/(dx)^2 = %f", &
      &                  d=(/sigma/) )
  else if ( sigma >= 0.4d0 ) then
    call MessageNotify( "W", &                      ! Warning message dump
      &                 "diffusion_4", &            ! (警告メッセージ出力)
      &                 "dt is moderately large: k*dt/(dx)^2 = %f", &
      &                 d=(/sigma/) )
  else
    call MessageNotify( "M", &                      ! Message dump 
      &                 "diffusion_4", &            ! (メッセージ出力) 
      &                 "dt is sufficiently small: k*dt/(dx)^2 = %f", &
      &                  d=(/sigma/) )
  end if
    
  temp = exp(-((x-0.5)/0.1)**2)                     ! Set initial value 
                                                    ! (初期値設定)

  call HistoryCreate( &                             ! Create output file 
    & file='diffusion_4.nc', &                      ! (ヒストリー作成)
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
end program diffusion_4

