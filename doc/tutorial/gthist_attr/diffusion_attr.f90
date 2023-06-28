!= Sample program for gtool_history/gtool5
!
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
program diffusion

  use gtool_history                                   ! モジュール指定

  integer, parameter     :: nx=30                   ! グリッド数
  integer, parameter     :: nt=200                  ! 時間ステップ数
  integer, parameter     :: ndisp=10                ! 出力間隔
  real(8), parameter     :: dx=1.0/(nx-1)           ! グリッド間隔
  real(8), parameter     :: dt=0.0005               ! 時間間隔
  real(8), dimension(nx) :: x=(/(dx*(i-1),i=1,nx)/) ! 座標変数
  real(8), dimension(nx) :: temp                    ! 温度
  real(8), parameter     :: kappa=1.0               ! 熱拡散係数

  tinit = 0.0                                       ! 初期時刻設定

  temp = exp(-((x-0.5)/0.1)**2)                     ! 初期値設定

  call HistoryCreate( &                             ! ヒストリー作成
    & file='diffusion_attr.nc', title='Diffusion equation', &
    & source='Sample program of gtool_history/gtool5',   &
    & institution='GFD_Dennou Club davis project',       &
    & dims=(/'x','t'/), dimsizes=(/nx,0/),               &
    & longnames=(/'X-coordinate','time        '/),       &
    & units=(/'m','s'/),                                 &
    & origin=real(tinit), interval=real(ndisp*dt) )

  call HistoryPut('x',x)                            ! 次元変数出力

  call HistoryAddVariable( &                        ! 変数定義
    & varname='temp', dims=(/'x','t'/), &
    & longname='temperature', units='K', xtype='double')

  call HistoryAddAttr('temp','gt_graph_tick_all',1)
  call HistoryAddAttr('temp','gt_graph_contour_spacing',(/0.0,1.0,0.01/))
  call HistoryAddAttr('temp','+gt_user_davis_kappa',kappa)

  call HistoryPut('temp',temp)                      ! 変数出力

  do it=1,nt
    temp(2:nx-1) = temp(2:nx-1) &                   ! 時間積分
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(it,ndisp) == 0 ) then
      call HistoryPut('temp',temp)                  ! 変数出力
    endif
  enddo

  call HistoryClose
  stop
end program diffusion
