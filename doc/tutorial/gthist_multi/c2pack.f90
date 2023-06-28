! Sample Fortran90 interface for c2pack/ISPACK         2001/02/27 S.Takehiro
!
module c2pack
  implicit none

  integer            :: im=8,  jm=8      ! 格子点の設定(X,Y)
  integer            :: km=32, lm=16     ! 切断波数の設定(X,Y)
  double precision   :: xl=2.0, yl=1.0   ! 領域の大きさ

  integer,dimension(5)                       :: itj
  double precision,dimension(:),allocatable  :: tj
  integer,dimension(5)                       :: iti
  double precision,dimension(:),allocatable  :: ti

  double precision, dimension(:),allocatable    :: wg, ws, wgj
  double precision, dimension(:,:),allocatable  :: wgg,wss,wsc
  double precision, parameter  :: pi=3.141592653589793

  private
  public c2initial
  public grid_ws, grid_wc, wsin_g, wcos_g, trans_g
  public nabla_ws, nabla_inv_ws, dx_ws, dy_ws, jac_ws
  public nabla_wc, dx_wc, dy_wc
!  public nabla_wc, dx_wc, dy_wc, jac_wc

  save im, jm, km, lm, itj, tj, iti, ti, xl, yl

  contains
  !--------------- 初期化 -----------------
    subroutine c2initial(i,j,k,l,xlength,ylength)

      integer,intent(in) :: i, j           ! 格子点の設定(X,Y)
      integer,intent(in) :: k, l           ! 切断波数の設定(X,Y)

      double precision,intent(in)   :: xlength, ylength   ! 領域の大きさ

      im = i       ; jm = j
      km = k       ; lm = l
      xl = xlength ; yl = ylength

      allocate(tj(jm*6),ti(im*2))
      allocate(wg((jm+1)*im),wgg(0:jm,0:im-1))
      allocate(ws((2*km+1)*(lm+1)),wgj((jm+1)*im*3))
      allocate(wss(-km:km,lm),wsc(-km:km,0:lm))

      call c2init(jm,im,itj,tj,iti,ti)
    end subroutine c2initial

  !--------------- 基本変換 -----------------
    function grid_ws(s) ! スペクトル SIN(Y) -> 台形格子
      double precision, dimension(0:jm,0:im-1)              :: grid_ws
      double precision, dimension(-km:km,lm), intent(in)    :: s

      call c2s2ga(lm,km,jm,im,s,grid_ws,wg,itj,tj,iti,ti,1)
    end function grid_ws

    function grid_wc(s) ! スペクトル COS(Y) -> 台形格子
      double precision, dimension(0:jm,0:im-1)              :: grid_wc
      double precision, dimension(-km:km,0:lm), intent(in)  :: s

      call c2s2ga(lm,km,jm,im,s,grid_wc,wg,itj,tj,iti,ti,2)
    end function grid_wc

    function wsin_g(g)  ! 台形格子 -> スペクトル SIN(Y)
      double precision, dimension(-km:km,lm)                :: wsin_g
      double precision, dimension(0:jm,0:im-1), intent(in)  :: g

      wgg = g
      call c2g2sa(lm,km,jm,im,wgg,wsin_g,wg,itj,tj,iti,ti,1)
    end function wsin_g

    function wcos_g(g)  ! 台形格子 -> スペクトル COS(Y)
      double precision, dimension(-km:km,0:lm)              :: wcos_g
      double precision, dimension(0:jm,0:im-1), intent(in)  :: g

      wgg = g
      call c2g2sa(lm,km,jm,im,wgg,wcos_g,wg,itj,tj,iti,ti,2)
    end function wcos_g

    function trans_g(g)  ! 台形格子の転置
      double precision, dimension(0:jm,0:im-1)              :: trans_g
      double precision, dimension(0:jm,0:im-1), intent(in)  :: g

      call c2s2gt(jm,im,g,trans_g)
    end function trans_g

  !--------------- 微分計算 -----------------
    function nabla_ws(s)   ! スペクトル SINY に作用する \nabla 演算子
      double precision, dimension(-km:km,lm)                :: nabla_ws
      double precision, dimension(-km:km,lm), intent(in)    :: s
      integer k,l

      do l=1,lm
         do k=-km,km
            nabla_ws(k,l) = -((2*pi*k/xl)**2+(pi*l/yl)**2)*s(k,l)
         enddo
      enddo
    end function nabla_ws

    function nabla_wc(s)   ! スペクトル COSY に作用する \nabla 演算子
      double precision, dimension(-km:km,0:lm)                :: nabla_wc
      double precision, dimension(-km:km,0:lm), intent(in)    :: s
      integer k,l

      do l=0,lm
         do k=-km,km
            nabla_wc(k,l) = -((2*pi*k/xl)**2+(pi*l/yl)**2)*s(k,l)
         enddo
      enddo
    end function nabla_wc

    function nabla_inv_ws(s)   ! スペクトル SINY に作用する逆 \nabla 演算子
      double precision, dimension(-km:km,lm)                :: nabla_inv_ws
      double precision, dimension(-km:km,lm), intent(in)    :: s
      integer k,l

      do l=1,lm
         do k=-km,km
            nabla_inv_ws(k,l) = -s(k,l)/((2*pi*k/xl)**2+(pi*l/yl)**2)
         enddo
      enddo
    end function nabla_inv_ws

    function dx_ws(s)   ! スペクトル SINY に作用する x 微分演算子
      double precision, dimension(-km:km,lm)                :: dx_ws
      double precision, dimension(-km:km,lm), intent(in)    :: s
      integer k,l

      do l=1,lm
         do k=-km,km
            dx_ws(k,l)  =  (-2*pi*k/xl)*s(-k,l)
         enddo
      enddo
    end function dx_ws

    function dx_wc(s)   ! スペクトル COS(Y) に作用する x 微分演算子
      double precision, dimension(-km:km,0:lm)                :: dx_wc
      double precision, dimension(-km:km,0:lm), intent(in)    :: s
      integer k,l

      do l=0,lm
         do k=-km,km
            dx_wc(k,l)  =  (-2*pi*k/xl)*s(-k,l)
         enddo
      enddo
    end function dx_wc

    function dy_ws(s)   ! スペクトル SINY に作用する y 微分演算子
      double precision, dimension(-km:km,0:lm)              :: dy_ws
      double precision, dimension(-km:km,lm), intent(in)    :: s
      integer k,l

      do l=0,lm
         do k=-km,km
            dy_ws(k,l)  =  (pi*l/yl)*s(k,l)
         enddo
      enddo
    end function dy_ws

    function dy_wc(s)   ! スペクトル COSY に作用する y 微分演算子
      double precision, dimension(-km:km,lm)              :: dy_wc
      double precision, dimension(-km:km,0:lm), intent(in)    :: s
      integer k,l

      do l=1,lm
         do k=-km,km
            dy_wc(k,l)  =  -(pi*l/yl)*s(k,l)
         enddo
      enddo
    end function dy_wc

    function jac_ws(sa,sb)   ! スペクトル SINY に作用するヤコビアン
      double precision, dimension(-km:km,lm)                :: jac_ws
      double precision, dimension(-km:km,lm), intent(in)    :: sa,sb

      integer k,l

      call c2ajcb(lm,km,jm,im,sa,sb,wss,ws,wgj,itj,tj,iti,ti)

      do l=1,lm
         do k=-km,km
            jac_ws(k,l) = (2*pi/xl)*(pi/yl)*wss(k,l)
         enddo
      enddo
    end function jac_ws

!    function jac_wc(sa,sb)   ! スペクトル COS(Y) に作用するヤコビアン
!      double precision, dimension(-km:km,0:lm)              :: jac_wc
!      double precision, dimension(-km:km,lm), intent(in)    :: sa
!      double precision, dimension(-km:km,0:lm), intent(in)  :: sb
!      integer k,l

!      call c2ajcc(lm,km,jm,im,sa,sb,wsc,ws,wgj,itj,tj,iti,ti)

!      do l=0,lm
!         do k=-km,km
!            !jac_wc(k,l) = (2*pi/xl)*(pi/yl)*wc(k,l)
!            jac_wc(k,l) = wsc(k,l)
!         enddo
!      enddo
!    end function jac_wc

  end module c2pack
