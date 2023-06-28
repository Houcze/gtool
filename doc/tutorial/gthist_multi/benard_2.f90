!= Sample program for gtool_history/gtool5 and ISPACK
!
! * 2009/02/27 Y.Morikawa
! * 2003/08/21 M.Odaka
! * 2001/02/27 S.Takehiro
!
! Solving 2-D Boussinesq fluid system
! \[
!     d\zeta/dt + J(\psi,\zeta) = Ra \; dT/dx + \nabla\zeta, \] \[
!     dT/dt + J(\psi,T) - d\psi/dx = \nabla T,               \] \[
!     \nabla\psi = \zeta,                                    \] \[
!     \psi = \zeta = T = 0 \quad at \; y=0,1.
! \]
!
program benard2

  use c2pack
  use gtool_history

  integer, parameter :: km=16, lm=8   ! 切断波数の設定(X,Y)
  integer, parameter :: im=64, jm=16  ! 格子点の設定(X,Y)

  real(8) :: psi(0:jm,0:im-1), temp(0:jm,0:im-1)
  real(8) :: wpsi(-km:km,lm), wtemp(-km:km,lm), wzeta(-km:km,lm)

  real(8) :: x(0:jm,0:im-1), y(0:jm,0:im-1)
  real(8), parameter :: xmin=0.0, xmax=4.0
  real(8), parameter :: ymin=0.0, ymax=1.0
  real(8), parameter :: Ra=1.0e4

  real(8), parameter :: dt=1e-4
  integer, parameter :: nt=5000, ndisp=500

  type(GT_HISTORY) :: hst_psi, hst_temp

  integer :: i,j

  tinit = 0.0                         ! 初期時刻設定

  do i=0,im-1
    do j=0,jm
      x(j,i)= (xmax-xmin)/im*i
      y(j,i)= (ymax-ymin)/jm*j
    enddo
  enddo

  temp = 0.0
  temp(jm/2,im/2) = 0.01

  call c2initial(im,jm,km,lm,xmax,ymax)

  wtemp = wsin_g(temp)                ! 初期値設定
  wpsi = 0.0 ; wzeta = 0.0            ! 初期値設定

  temp = temp + 1-y                   ! データ出力用
  call output_gtool4_init             ! ヒストリー初期化

  do it=1,nt
    wtemp = wtemp + dt*( -jac_ws(wpsi,wtemp) + dx_ws(wpsi) + nabla_ws(wtemp) )
    wzeta = wzeta + &
      & dt*( - jac_ws(wpsi,wzeta) &
      & + Ra*dx_ws(wtemp) + nabla_ws(wzeta)    )
    wpsi = nabla_inv_ws(wzeta)

    if(mod(it,ndisp) .eq. 0)then
      temp = grid_ws(wtemp)+1-y
      psi = grid_ws(wpsi)
      call output_gtool4
    endif
  enddo

  call output_gtool4_close
  stop

contains
  subroutine output_gtool4_init
    call HistoryCreate( &                             ! ヒストリー作成
      & file='psi.nc', title='convection in rotating annulus', &
      & source='Sample program of gtool_history/gtool5', &
      & institution='GFD_Dennou Club davis project', &
      & dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/), &
      & longnames=(/'X-coordinate','Y-coordinate','time        '/),&
      & units=(/'1','1','1'/), &
      & origin=real(tinit), interval=real(ndisp*dt), &
      & history=hst_psi )

    call HistoryPut('x',x(1,0:im-1),hst_psi)                  ! 変数出力
    call HistoryPut('y',y(0:jm,1),hst_psi)                    ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
      & varname='psi', dims=(/'x','y','t'/), &
      & longname='stream function', units='1', xtype='double',&
      & history=hst_psi )

    call HistoryCreate( &                             ! ヒストリー作成
      & file='temp.nc', title='convection in rotating annulus', &
      & source='Sample program of gtool_history/gtool5', &
      & institution='GFD_Dennou Club davis project', &
      & dims=(/'x','y','t'/), dimsizes=(/im,jm+1,0/), &
      & longnames=(/'X-coordinate','Y-coordinate','time        '/),&
      & units=(/'1','1','1'/), &
      & origin=real(tinit), interval=real(ndisp*dt), &
      & history=hst_temp )

    call HistoryPut('x',x(1,0:im-1),hst_temp)                 ! 変数出力
    call HistoryPut('y',y(0:jm,1),hst_temp)                   ! 変数出力

    call HistoryAddVariable( &                                ! 変数定義
      & varname='temp', dims=(/'x','y','t'/), &
      & longname='temperature', units='1', xtype='double',&
      & history=hst_temp)

  end subroutine output_gtool4_init

  subroutine output_gtool4
    write(6,*) 'it = ',it
    call HistoryPut('psi',  trans_g(psi),  hst_psi)
    call HistoryPut('temp', trans_g(temp), hst_temp)
  end subroutine output_gtool4

  subroutine output_gtool4_close
    call HistoryClose(hst_psi)
    call HistoryClose(hst_temp)
  end subroutine output_gtool4_close

end program benard2
