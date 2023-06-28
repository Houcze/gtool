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

      character*1  dims(2), units(2)
      character*6  xtypes(2)
      character*12 lnames(2)
      integer      dimsiz(2)
      data      dims    /'x','t'/
      data      dimsiz  /nx,0/
      data      xtypes  /'double','double'/
      data      lnames  /'X-coordinate','time        '/
      data      units   /'m','s'/

      do i=1,nx
        x(i)=dx*(i-1)                            ! 座標値設定
      enddo

      do i=1,nx
        temp(i) = exp(-((x(i)-0.5)/0.1)**2)      ! 初期値設定
      enddo

      call hscrea( 'diff77.nc', 'Diffusion equation',      ! ヒストリー作成 
     &             'Sample Fortran77 progam of gtool_history/gtool5', 
     &             'GFD_Dennou Club davis project', 
     &             2, dims, dimsiz, xtypes, lnames, units, 
     &             0.0, real(ndisp*dt)          )

      call hsp1d('x',x,nx)                                     ! 次元変数出力
      call hsavar('temp',2,dims,'temperature','K','double')    ! 変数定義

      call hsp1d('temp',temp,nx)                               ! 変数出力
      call hsaatd('temp','+gt_user_davis_kappa',kappa,1)       ! 属性出力

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
            call hsp1d('temp',temp,nx)                      ! 変数出力
         endif
      enddo

      call hsclse

      stop
      end
