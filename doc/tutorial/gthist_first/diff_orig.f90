! Sample program for gtool_history/gtool5              2001/02/27 S.Takehiro
! 
! Solving diffusion equation 
!     du/dt = kappa d^2 u/dx^2
! for giving values of u at x=0 and 1. 
!
program diffusion

  integer, parameter     :: nx=30                   ! グリッド数
  integer, parameter     :: nt=200                  ! 時間ステップ数
  integer, parameter     :: ndisp=10                ! 出力間隔
  real(8), parameter     :: dx=1.0/(nx-1)           ! グリッド間隔
  real(8), parameter     :: dt=0.0005               ! 時間間隔
  real(8), dimension(nx) :: x=(/(dx*(i-1),i=1,nx)/) ! 座標変数
  real(8), dimension(nx) :: temp                    ! 温度
  real(8), parameter     :: kappa=1.0               ! 熱拡散係数

  temp = exp(-((x-0.5)/0.1)**2)                     ! 初期値設定

  do it=1,nt
    temp(2:nx-1) = temp(2:nx-1) &                   ! 時間積分
      & + kappa*(temp(3:nx)-2*temp(2:nx-1)+temp(1:nx-2))/dx**2*dt

    if ( mod(nt,ndisp) .eq. 0 ) then
      write(6,*)it*dt                               ! 変数出力
      write(6,*)temp                                ! 変数出力
    endif
  enddo

  stop
end program diffusion
