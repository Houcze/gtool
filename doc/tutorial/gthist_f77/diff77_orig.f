* Sample program for gtool_history/gtool5        2001/02/27 S.Takehiro
* 
* Solving diffusion equation 
*     du/dt = kappa d^2 u/dx^2
* for giving values of u at x=0 and 1. 
*
      program diffeq

      integer nx, nt, ndisp
        parameter( nx=30 )                       ! グリッド数
        parameter( nt=200 )                      ! 時間ステップ数
        parameter( ndisp=10 )                    ! 出力間隔
      double precision  dx, dt, kappa
        parameter( dx=1.0/(nx-1))                ! グリッド間隔
        parameter( dt=0.0005 )                   ! 時間間隔
        parameter( kappa=1.0D0 )                 ! 熱拡散係数
      double precision x(nx)                     ! 座標変数
      double precision temp(nx), temp1(nx)       ! 温度

      integer i,it                               ! 作業変数

      do i=1,nx
        x(i)=dx*(i-1)                            ! 座標値設定
      enddo

      do i=1,nx
        temp(i) = exp(-((x(i)-0.5)/0.1)**2)      ! 初期値設定
      enddo

      do it=1,nt                                 ! 時間積分
         do i=2,nx-1
            temp1(i) = temp(i) 
     &               + kappa*(temp(i+1)-2*temp(i)+temp(i-1))
     &                   /dx**2*dt
         enddo
         do i=1,nx
            temp(i) = temp1(i)
         enddo

         if ( mod(nt,ndisp) .eq. 0 ) then
            write(6,*)it*dt                      ! 変数出力
            write(6,*)temp                       ! 変数出力
         endif
      enddo

      stop
      end
