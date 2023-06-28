!------------------------------------------------------------------------
!   このファイルに記述されるモジュール群は,
!   gtool5 ライブラリのチュートリアルで使用するため,
!   spml Fortran90 ライブラリ
!   <http://www.gfd-dennou.org/library/spmodel>
!   のモジュールの一部を抜粋したものです.
!   ライセンスはオリジナル spml に準拠するため,
!   改変や再配布に際しては spml のライセンスを参照ください.
!------------------------------------------------------------------------

!--
!----------------------------------------------------------------------
!     Copyright (c) 2002--2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  w_base_module
!
!  spml/w_base_module モジュールは球面上での 2 次元流体運動を球面調和函
!  数を用いたスペクトル法によって数値計算するためのモジュール w_module
!  の下部モジュールであり, スペクトル計算の基本的な Fortran90 関数を提
!  供する.
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んで
!  いる. スペクトルデータおよび格子点データの格納方法や変換の詳しい計算
!  法については ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!== 履歴
!
!      2001/12/08  竹広真一
!      2001/12/26  竹広真一  関数,変数名前変更
!      2002/02/07  竹広真一  関数,変数名前再変更
!      2002/03/30  竹広真一  関数,変数名前再再変更
!      2002/05/25  竹広真一  格子点座標を区別すべく命名法変更
!      2005/03/13  竹広真一  l_nm, nm_l を配列で引数を渡せるように拡張
!      2005/07/04  竹広真一  OpenMP 版変換ルーチンに対応
!                            バンク競合を避けるための作業配列追加
!      2005/07/10  竹広真一  OpenMP セットアップのメッセージ出力
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2007/11/21  竹広真一  初期化サブルーチンメッセージ出力
!      2008/02/23  佐々木洋平 格子点データの配列を(im,jm) から (0:im-1, 0:jm-1)
!                             に変更.
!      2008/06/25  佐々木洋平 格子点データの配列を(0:im-1,1:jm) に変更
!      2008/07/04  佐々木洋平 コメントを RDoc 用に微修正
!      2008/12/28  竹広真一   xy_w, w_xy のコメントを追加
!      2009/01/09  竹広真一   w_base_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!
!      制限
!         ・変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module w_base_module
  !
  != w_base_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_module_snip.f90,v 1.2 2009-02-28 12:24:52 morikawa Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
  !
  !== 概要.
  !
  ! spml/w_base_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための
  ! モジュール w_module の下部モジュールであり, スペクトル法の
  ! 基本的な Fortran90 関数を提供する.
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチン
  ! を呼んでいる. スペクトルデータおよび格子点データの格納方法
  ! や変換の詳しい計算法については ISPACK/SNPACK,SPPACK のマニ
  ! ュアルを参照されたい.
  !
  use dc_message
  implicit none

  integer               :: im=64            ! 格子点の設定(東西)
  integer               :: jm=32            ! 格子点の設定(南北)
  integer               :: nm=21            ! 切断波数の設定
  integer               :: np=1             ! OPENMP 最大スレッド数

  logical               :: openmp=.false.   ! OPENMP スイッチ

  integer               :: it(6)            ! 変換用配列
  real(8), allocatable  :: t(:)             ! 変換用配列
  integer, allocatable  :: ip(:)            ! 変換用配列
  real(8), allocatable  :: p(:), r(:)       ! 変換用配列
  integer, allocatable  :: ia(:)            ! 変換用配列
  real(8), allocatable  :: a(:)             ! 変換用配列
  real(8), allocatable  :: y(:,:)           ! 変換用配列

  real(8), allocatable  :: q(:)             ! 作業配列
  real(8), allocatable  :: ww(:), ws(:)     ! 作業配列
  real(8), allocatable  :: wv(:)            ! 作業配列(OPENMP用)

  real(8), allocatable  :: x_Lon(:), y_Lat(:)                ! 緯度経度
  real(8), allocatable  :: x_Lon_Weight(:), y_Lat_Weight(:)  ! 座標重み
  real(8), allocatable  :: xy_Lon(:,:), xy_Lat(:,:)

  real(8), allocatable  :: xy_work(:,:)     ! w_xy,xy_w 変換用配列
  integer               :: id=65, jd=33     ! xy_work の大きさ

  real(8), parameter    :: pi=3.1415926535897932385D0

  private

  public im, jm, nm                           ! 格子点数, 切断波数, 半径
  public it, t, y, ip, p, r, ia, a            ! 変換用作業配列
  public openmp, np                           ! OPENMP 用変数

  public w_base_Initial                       ! 初期化サブルーチン
  public x_Lon, y_Lat                         ! 格子座標
  public x_Lon_Weight, y_Lat_Weight           ! 格子座標重み
  public xy_Lon, xy_Lat                       ! 格子座標(im,jm)
  public l_nm, nm_l                           ! 波数格納位置
  public xy_w, w_xy                           ! 変換関数

  interface l_nm
     module procedure l_nm_array00
     module procedure l_nm_array01
     module procedure l_nm_array10
     module procedure l_nm_array11
  end interface

  interface nm_l
     module procedure nm_l_int
     module procedure nm_l_array
  end interface

  save im, jm, nm                             ! 格子点数, 切断波数, 半径を記憶
  save it, t, y, ip, p, r, ia, a              ! 変換用配列を記憶
  save id, jd                                 ! 変換用配列の大きさ

  contains
  !--------------- 初期化 -----------------
    subroutine w_base_Initial(n_in,i_in,j_in,np_in)
      !
      ! スペクトル変換の格子点数, 波数および OPENMP 使用時の
      ! 最大スレッド数を設定する.
      !
      ! 実際の使用には上位サブルーチン w_Initial を用いること.
      !
      integer,intent(in) :: i_in              !(in) 格子点数(東西)
      integer,intent(in) :: j_in              !(in) 格子点数(南北)
      integer,intent(in) :: n_in              !(in) 切断全波数
      integer,intent(in), optional :: np_in   !(in) OPENMP での最大スレッド数

      integer :: iw, i, j

      im = i_in  ; jm = j_in  ; nm = n_in

      if ( present(np_in) )then
         np = np_in

         if ( np .gt. 1 ) then
            openmp = .true. 
            allocate(wv((nm+4)*(nm+3)*np))
            call MessageNotify('M','w_base_Initial', &
                 'OpenMP computation was set up.')
         else
            openmp = .false. 
         endif

      else
         openmp = .false. 
      endif

      if ( im/2*2 .eq. im ) then
         id = im+1 
      else
         id = im
      endif
      if ( openmp ) then
         jd = jm
      else if ( jm/2*2 .eq. jm ) then
         jd = jm+1
      else
         jd = jm
      endif
      allocate(xy_work(id,jd))                ! 変換用配列
      xy_work = 0.

      allocate(t(im*2))                       ! 変換用配列
      allocate(ip(((nm+1)/2+nm+1)*2))         ! 変換用配列
      allocate(p(((nm+1)/2+nm+1)*jm))         ! 変換用配列
      allocate(r(((nm+1)/2*2+3)*(nm/2+1)))    ! 変換用配列
      allocate(ia((nm+1)*(nm+1)*4))           ! 変換用配列
      allocate(a((nm+1)*(nm+1)*6))            ! 変換用配列
      allocate(y(jm/2,4))                     ! 変換用配列

      allocate(q(((nm+1)/2+nm+1)*jm))         ! 作業配列
      if ( openmp ) then
         iw=(im+nm+1)*3*jm/2
      else
         iw=max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)
      endif
      allocate(ws(iw),ww(iw))                 ! 作業用配列

      allocate(x_Lon(0:im-1))                ! 格子点座標格納配列(経度)
      allocate(x_Lon_Weight(0:im-1))
      allocate(xy_Lon(0:im-1,1:jm))
      allocate(y_Lat(1:jm))
      allocate(y_Lat_Weight(1:jm))             ! 格子点座標格納配列
      allocate(xy_Lat(0:im-1,1:jm))        ! 格子点座標格納配列

      call sninit(nm,im,jm,it,t,y,ip,p,r,ia,a)

      do i=0,im-1
         x_Lon(i)  = 2*pi/im*i               ! 経度座標
         x_Lon_Weight(i) = 2*pi/im           ! 経度座標重み
      enddo


      do j=1,jm/2
         y_Lat(jm/2+j)   =  asin(y(j,1))        ! 緯度座標
         y_Lat(jm/2-j+1) = -asin(y(j,1))        ! 緯度座標
         y_Lat_Weight(jm/2+j)   = 2*y(j,2)      ! 緯度重み(Gauss grid)
         y_Lat_Weight(jm/2-j+1) = 2*y(j,2)      ! 緯度重み(Gauss grid)
      enddo

      do j=1,jm
         xy_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xy_Lat(i,:) = y_Lat
      enddo

      call MessageNotify('M','w_base_initial',&
           'w_base_module (2009/01/09) is initialized')

    end subroutine w_base_Initial

  !--------------- 基本変換 -----------------

    function l_nm_array00(n,m)
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 引数 n,m がともに整数値の場合, 整数値を返す. 
      !
      integer               :: l_nm_array00   
      !(out) スペクトルデータの格納位置 

      integer, intent(in)   :: n     !(in) 全波数
      integer, intent(in)   :: m     !(in) 帯状波数           

      call snnm2l(n,m,l_nm_array00)
    end function l_nm_array00

    function l_nm_array01(n,marray)           ! スペクトルデータの格納位置 
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1 引数 n が整数, 第 2 引数 marray が整数 1 次元配列の場合, 
      ! marray と同じ大きさの 1 次元整数配列を返す. 
      !
      integer, intent(in)  :: n               !(in) 全波数
      integer, intent(in)  :: marray(:)       !(in) 帯状波数
      integer              :: l_nm_array01(size(marray))
      !(out) スペクトルデータ位置

      integer              :: i 

      do i=1, size(marray)
         l_nm_array01(i) = l_nm_array00(n,marray(i))
      enddo
    end function l_nm_array01

    function l_nm_array10(narray,m)
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1 引数 narray が整数 1 次元配列, 第 2 引数  m が整数の場合, 
      ! narray と同じ大きさの 1 次元整数配列を返す. 
      !
      integer, intent(in)  :: narray(:)           !(in) 全波数  
      integer, intent(in)  :: m                   !(in) 帯状波数
      integer              :: l_nm_array10(size(narray))
      !(out) スペクトルデータ位置

      integer              :: i 

      do i=1, size(narray)
         l_nm_array10(i) = l_nm_array00(narray(i),m)
      enddo
    end function l_nm_array10

    function l_nm_array11(narray,marray)
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1,2 引数 narray, marray がともに整数 1 次元配列の場合, 
      ! narray, marray と同じ大きさの 1 次元整数配列を返す. 
      ! narray, marray は同じ大きさでなければならない. 
      !
      integer, intent(in)  :: narray(:)          !(in) 全波数  
      integer, intent(in)  :: marray(:)          !(in) 帯状波数
      integer              :: l_nm_array11(size(narray))
      !(out) スペクトルデータ位置

      integer              :: i 

      if ( size(narray) .ne. size(marray) ) then
         call MessageNotify('E','l_nm_array11',&
              'dimensions of input arrays  n and m are different.')
      endif

      do i=1, size(narray)
         l_nm_array11(i) = l_nm_array00(narray(i),marray(i))
      enddo
    end function l_nm_array11

    function nm_l_int(l)
      ! 
      ! スペクトルデータの格納位置(l)から全波数(n)と東西波数(m)を返す.
      !
      ! 引数 l が整数値の場合, 対応する全波数と帯状波数を
      ! 長さ 2 の 1 次元整数値を返す. 
      ! nm_l(1) が全波数, nm_l(2) が帯状波数である. 
      !
      integer               :: nm_l_int(2)  !(out) 全波数, 帯状波数
      integer, intent(in)   :: l            !(in) スペクトルデータの格納位置
      
      call snl2nm(l,nm_l_int(1),nm_l_int(2))
    end function nm_l_int

    function nm_l_array(larray)
      ! 
      ! スペクトルデータの格納位置(l)から全波数(n)と東西波数(m)を返す.
      !
      ! 引数 larray が整数 1 次元配列の場合, 
      ! larray に対応する n, m を格納した 2 次元整数配列を返す. 
      ! nm_l_array(:,1) が全波数, nm_l_array(:,2) が帯状波数である. 
      !
      integer, intent(in)  :: larray(:)
      !(out) 全波数, 帯状波数

      integer              :: nm_l_array(size(larray),2)
      !(in) スペクトルデータの格納位置

      integer              :: i

      do i=1, size(larray)
         nm_l_array(i,:) = nm_l_int(larray(i))
      enddo
    end function nm_l_array

    function xy_w(w_data,ipow,iflag)
      !
      ! スペクトルデータから格子データへ変換する(1 層用).
      !
      real(8)               :: xy_w(0:im-1,1:jm)
      !(out) 格子点データ

      real(8), intent(in)   :: w_data((nm+1)*(nm+1))
      !(in) スペクトルデータ

      integer, intent(in), optional  :: ipow      
      !(in) 作用させる 1/cosφ の次数. 省略時は 0. 

      integer, intent(in), optional  :: iflag
      !(in) 変換の種類
      !    0 : 通常の正変換
      !   -1 : 経度微分を作用させた逆変換
      !    1 : 緯度微分 cosφ・∂/∂φ を作用させた逆変換
      !    2 : sinφを作用させた逆変換
      !    省略時は 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval, i, j

      logical :: first=.true.                    ! 初回判定スイッチ
      save first

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','xy_w', &
                 'OpenMP routine SNTSOG/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntsog(nm,im,id,jm,1,w_data,xy_work,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call snts2g(nm,im,id,jm,jd,1,w_data,xy_work,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)
      endif
      do i=0,im-1
        do j=1,jm
          xy_w(i,j) = xy_work(i+1,j)
        enddo
      enddo
      first = .false.

    end function xy_w

    function w_xy(xy_data,ipow,iflag)
      !
      ! 格子データからスペクトルデータへ(正)変換する(1 層用).
      !
      real(8)               :: w_xy((nm+1)*(nm+1))
      !(out) スペクトルデータ

      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) 格子点データ

      integer, intent(in), optional  :: ipow
      !(in) 変換時に同時に作用させる 1/cosφ の次数. 省略時は 0.

      integer, intent(in), optional  :: iflag
      ! 変換の種類
      !    0 : 通常の正変換
      !   -1 : 経度微分を作用させた正変換 
      !    1 : 緯度微分 1/cosφ・∂(f cos^2φ)/∂φ を作用させた正変換
      !    2 : sinφを作用させた正変換
      !  省略時は 0.


      integer, parameter  :: ipow_default  = 0    ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0    ! スイッチデフォルト値

      integer ipval, ifval, i, j

      logical :: first=.true.                     ! 初回判定スイッチ
      save first

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif
      
      do i=0,im-1
        do j=1,jm
          xy_work(i+1,j)=xy_data(i,j)
        enddo
      enddo

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','w_xy', &
                 'OpenMP routine SNTGOS/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntgos(nm,im,id,jm,1,xy_work,w_xy,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call sntg2s(nm,im,id,jm,jd,1,xy_work,w_xy,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)
      endif
      first = .false.

    end function w_xy

  end module w_base_module
!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  w_deriv_module
!
!  spml/w_deriv_module モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール w_module の下部モジュールであり, スペクトル法の
!  微分計算のための Fortran90 関数を提供する. 
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!  このモジュールを使うためには前もって w_base_initial を呼んで
!  切断波数, 格子点数の設定をしておく必要がある. 
!
!
!履歴  2001/12/08  竹広真一
!      2001/12/26  竹広真一  関数,変数名前変更
!      2002/02/07  竹広真一  関数,変数名前再変更
!      2002/03/30  竹広真一  関数,変数名前再再変更
!      2002/05/25  竹広真一  格子点座標を区別すべく命名法変更
!      2005/07/04  竹広真一  OPENMP 版変換ルーチンに対応
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2008/05/31  竹広真一  初期化ルーチンを分離
!      2008/06/22  佐々木洋平 格子点データの配列開始点を 1 から 0 へ.
!      2008/06/23  佐々木洋平 格子点データの格納は (0:im-1, 1:jm) へ.
!      2008/07/01  佐々木洋平 コメントを RDoc 用に微修正
!      2009/01/09  竹広真一  w_deriv_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!
!      制限
!         ・変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module w_deriv_module
  !
  != w_deriv_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_module_snip.f90,v 1.2 2009-02-28 12:24:52 morikawa Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/w_deriv_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール w_module の下部モジュールであり, スペクトル法の
  ! 微分計算のための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  ! このモジュールを使うためには前もって w_base_initial を呼んで
  ! 切断波数, 格子点数の設定をしておく必要がある. 
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : im, jm, nm, it, t, y, ip, p, r, ia, a, &
                            w_base_Initial, xy_w, w_xy
  implicit none

  real(8), allocatable  :: rn(:,:)            
  ! ラプラシアン演算用配列
  !
  ! スペクトルデータのラプラシアンを計算するための係数
  ! 配列のサイズは((nm+1)*(nm+1), 2)
  !
  ! r(L,1) には L 番目の格納位置のスペクトルに対するラプラシアン計算の
  ! 係数 -n(n+1) の値が格納されている.
  !
  integer, allocatable  :: irm(:,:)           
  ! 経度微分演算用配列
  !
  ! スペクトルデータの経度微分を計算するための係数.
  ! 配列サイズは ( (nm+1)*(nm+1),2 ) である.
  !
  ! L番目の格納位置のスペクトルが実部なら, irm(L,1)には対応する虚部の格納位置が,
  ! irm(L,2) には東西波数 m が格納されている. また, L番目の格納位置のスペクトル
  ! が虚部なら, irm(L,1)には対応する実部の格納位置が, irm(L,2)には -m が格納され
  ! ている.
  !
  integer, allocatable  :: ip2(:), ip3(:)     ! ヤコビアン計算用配列
  real(8), allocatable  :: p2(:), p3(:)       ! ヤコビアン計算用配列
  real(8), allocatable  :: r2(:), r3(:)       ! ヤコビアン計算用配列

  real(8), allocatable  :: q(:)               ! 作業配列
  real(8), allocatable  :: ww(:),ws(:)        ! 作業配列

  private

  public w_deriv_Initial                      ! 初期化
  public w_Lapla_w, w_LaplaInv_w              ! ラプラシアンと逆演算
  public w_DLon_w                             ! 経度微分
  public xy_GradLon_w, xy_GradLat_w           ! 勾配型微分
  public w_DivLon_xy, w_DivLat_xy             ! 発散型微分
  public w_Div_xy_xy                          ! 発散型微分
  public w_Jacobian_w_w                       ! ヤコビアン
  public xy_GradLambda_w, xy_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xy, w_DivMu_xy           ! 発散型微分(λ,μ座標)

  public rn, irm                              ! ラプラシアン/経度微分演算用配列

  save rn, irm, ip2, ip3, p2, p3, r2, r3

  contains

  !--------------- 初期化 -----------------
    subroutine w_deriv_initial
      !
      ! スペクトル微分計算に必要となる作業領域を設定する. 
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない. 
      !
      ! このサブルーチンを単独で用いるのでなく, 
      ! 上位サブルーチン w_Initial を使用すること.
      !
      integer iw

      allocate(rn((nm+1)*(nm+1),2))           ! ラプラシアン演算用配列
      allocate(irm((nm+1)*(nm+1),2))          ! 経度微分演算用配列
      call spnini(nm,rn)
      call spmini(nm,irm)

      allocate(ip2(2*((nm+1)/2+nm+1)*2))      ! ヤコビアン計算用配列
      allocate(p2(2*((nm+1)/2+nm+1)*jm))      ! ヤコビアン計算用配列
      allocate(r2(2*((nm+1)/2*2+3)*(nm/2+1))) ! ヤコビアン計算用配列
      allocate(ip3(3*((nm+1)/2+nm+1)*2))      ! ヤコビアン計算用配列
      allocate(p3(3*((nm+1)/2+nm+1)*jm))      ! ヤコビアン計算用配列
      allocate(r3(3*((nm+1)/2*2+3)*(nm/2+1))) ! ヤコビアン計算用配列
      call snkini(nm,jm,2,ip,p,r,ip2,p2,r2)
      call snkini(nm,jm,3,ip,p,r,ip3,p3,r3)

      allocate(q(3*((nm+1)/2+nm+1)*jm))       ! 作業用配列
      iw=3*max( ((nm+1)/2*2+3)*(nm/2+2)*2, &
                jm*((nm+1)/2+nm+1)*2, jm*jm )
      allocate(ws(iw),ww(iw))                 ! 作業用配列

      call MessageNotify('M','w_deriv_initial',&
           'w_deriv_module (2009/01/09) is initialized')

    end subroutine w_deriv_initial

  !--------------- 微分計算 -----------------
    function w_Lapla_w(w_data)
      !
      ! 入力スペクトルデータにラプラシアン
      !
      !    ▽^2 = 1/cos^2φ・∂^2/∂λ^2 + 1/cosφ・∂/∂φ(cosφ∂/∂φ)
      !
      ! を作用する(1 層用).
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8)              :: w_Lapla_w((nm+1)*(nm+1))
      !(out) 入力スペクトルデータのラプラシアン

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      call spclap(nm,w_data,w_Lapla_w,rn(1,1))
    end function w_Lapla_w

    function w_LaplaInv_w(w_data)
      !
      ! 入力スペクトルデータに逆ラプラシアン
      !
      !    ▽^{-2}
      !      =[1/cos^2φ・∂^2/∂λ^2 + 1/cosφ・∂/∂φ(cosφ∂/∂φ)]^{-1}
      !
      ! を作用する(1 層用).
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8)              :: w_LaplaInv_w((nm+1)*(nm+1))
      !(out) スペクトルデータの逆ラプラシアン

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      call spclap(nm,w_data,w_LaplaInv_w,rn(1,2))
    end function w_LaplaInv_w

    function w_DLon_w(w_data)
      !
      ! スペクトルデータに経度微分 ∂/∂λ を作用させる(1 層用).
      !
      ! スペクトルデータの経度微分とは, 対応する格子点データに
      ! 経度微分∂/∂λを作用させたデータのスペクトル変換のことである.
      ! 
      real(8)              :: w_DLon_w((nm+1)*(nm+1))
      !(out) スペクトルデータの経度微分

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      call spclam(nm,w_data,w_DLon_w,irm)

    end function w_DLon_w

    function xy_GradLon_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を
      ! 作用させた格子点データを返す(1 層用).
      !
      real(8)              :: xy_GradLon_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xy_GradLon_w = xy_w(w_data,ipow=1,iflag=-1)

    end function xy_GradLon_w

    function xy_GradLat_w(w_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(1 層用).
      !
      real(8)              :: xy_GradLat_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xy_GradLat_w = xy_w(w_data,ipow=1,iflag=1)

    end function xy_GradLat_w

    function w_DivLon_xy(xy_data)
      !
      ! 格子点データに発散型経度微分 1/cosφ・∂/∂λ を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLon_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型経度微分したスペクトルデータ
      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivLon_xy = w_xy(xy_data,ipow=1,iflag=-1)

    end function w_DivLon_xy

    function w_DivLat_xy(xy_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLat_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivLat_xy = w_xy(xy_data,ipow=1,iflag=1)

    end function w_DivLat_xy

    function w_Div_xy_xy(xy_u,xy_v)
      !
      ! 2 つの入力格子点データをベクトル成分とする発散を計算し, 
      ! スペクトルデータとして返す(1 層用).
      !
      real(8)              :: w_Div_xy_xy((nm+1)*(nm+1))
      !(out) 2 つの入力格子点データをベクトル成分とする発散のスペクトルデータ

      real(8), intent(in)  :: xy_u(0:im-1,1:jm)
      !(in) ベクトル経度成分の格子点データ

      real(8), intent(in)  :: xy_v(0:im-1,1:jm)
      !(in) ベクトル緯度成分の格子点データ

      w_Div_xy_xy = w_Divlon_xy(xy_u) + w_Divlat_xy(xy_v)

    end function w_Div_xy_xy

    function w_Jacobian_w_w(w_a,w_b)
      ! 2 つのスペクトルデータにヤコビアン
      !
      !   J(f,g) = ∂f/∂λ・∂g/∂μ - ∂g/∂λ・∂f/∂μ
      !          = ∂f/∂λ・1/cosφ・∂g/∂φ
      !             - ∂g/∂λ・1/cosφ・∂f/∂φ
      !
      ! を作用させる(1 層用).

      real(8)             :: w_Jacobian_w_w((nm+1)*(nm+1))
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), intent(in) :: w_a((nm+1)*(nm+1))
      !(in) 1つ目の入力スペクトルデータ
      
      real(8), intent(in) :: w_b((nm+1)*(nm+1))
      !(in) 2つ目の入力スペクトルデータ

      call spnjcb(nm,im,im,jm,jm,w_a,w_b,w_Jacobian_w_w,&
           it,t,y,ip2,p2,r2,ip3,p3,r3,ia,a,q,ws,ww)

    end function w_Jacobian_w_w

  !--------------- 微分計算 (λ,μ座標系用) -----------------
    function xy_GradLambda_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 ∂/∂λ を作用する(1 層用).
      !
      real(8)              :: xy_GradLambda_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ
      
      xy_GradLambda_w = xy_w(w_data,ipow=0,iflag=-1)

    end function xy_GradLambda_w

    function xy_GradMu_w(w_data)
      !
      ! スペクトルデータに勾配型緯度微分 (1-μ^2)∂/∂μ  (μ=sinφ)
      ! を作用させて格子点データに変換して返す(1 層用).
      !
      real(8)              :: xy_GradMu_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xy_GradMu_w = xy_w(w_data,ipow=0,iflag=1)

    end function xy_GradMu_w

    function w_DivLambda_xy(xy_data)
      !
      ! 格子点データに発散型経度微分 1/(1-μ^2)・∂/∂λ (μ=sinφ) 
      ! を作用させてスペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLambda_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型経度微分したスペクトルデータ

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivLambda_xy = w_xy(xy_data,ipow=2,iflag=-1)

    end function w_DivLambda_xy

    function w_DivMu_xy(xy_data)
      !
      ! 格子点データに発散型緯度微分 ∂/∂μ (μ=sinφ)を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivMu_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivMu_xy = w_xy(xy_data,ipow=2,iflag=1)

    end function w_DivMu_xy

  end module w_deriv_module
!--
!----------------------------------------------------------------------
! Copyright (c) 2002-2008 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  w_module
!
!   spml/w_module モジュールは球面上での 2 次元流体運動を
!   球面調和函数を用いたスペクトル法によって数値計算するための
!   Fortran90 関数を提供する. 
!
!   w_module は実際には基本変換, 微分計算, 積分・平均計算, スペクトル解析
!   をそれぞれ担っている下部モジュール w_base_module, w_deriv_module, 
!   w_integral_module, w_spectrum_module からなっている.
!
!   内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる.
!   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!   ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2002/02/02  竹広真一  1 層用
!      2002/02/07  竹広真一  関数,変数名前再変更
!      2002/03/25  竹広真一  モジュール名変更
!      2002/05/25  竹広真一  格子点座標を区別すべく命名法変更
!      2002/10/07  竹広真一  λ, μ座標系用微分追加
!      2005/04/23  竹広真一  スペクトル解析モジュール追加
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2006/03/19  竹広真一  変数・手続き群の要約をコメントに追加
!      2007/10/28  竹広真一  補間関数モジュール追加
!      2007/11/21  竹広真一  初期化サブルーチンメッセージ出力
!      2008/05/31  竹広真一  初期化サブルーチン新たに作成
!      2008/07/01  佐々木洋平 コメントを RDoc 用に微修正
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!
!++
module w_module
  !
  != w_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_module_snip.f90,v 1.2 2009-02-28 12:24:52 morikawa Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/w_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための
  ! Fortran90 関数を提供する. 
  !
  ! w_module は実際には基本変換, 微分計算, 積分・平均計算, スペクトル解析
  ! をそれぞれ担っている下部モジュール w_base_module, w_deriv_module, 
  ! w_integral_module, w_spectrum_module からなっている.
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる.
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (w_, nm_, n_, xy_, x_, y_) は, 返す値の形を示している.
  !   w_  :: スペクトルデータ
  !   xy_ :: 2 次元格子点データ
  !   nm_ :: スペクトルデータの並んだ 3 次元配列(スペクトルデータの並びは
  !          全波数 n, 帯状波数 m で指定される 2 次元配列)
  !   n_  :: スペクトルデータの並んだ 2 次元配列 (スペクトルデータの並びは
  !          全波数 n で指定される 1 次元配列)
  !   x_  :: 経度方向 1 次元格子点データ
  !   y_  :: 緯度方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !   LaplaInv, Jacobian)は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_w_w, _w, _xy, _x, _y) は, 入力変数の形スペクトルデータ
  !   および格子点データであることを示している.
  !   _w   :: スペクトルデータ
  !   _w_w :: 2 つのスペクトルデータ
  !   _xy  :: 2 次元格子点データ
  !   _x   :: 経度方向 1 次元格子点データ
  !   _y   :: 緯度方向 1 次元格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * xy : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm). 
  !   * im, jm はそれぞれ経度, 緯度座標の格子点数であり, サブルーチン
  !     w_Initial にてあらかじめ設定しておく.
  !
  ! * w : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1)). 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン w_Initial にて
  !     あらかじめ設定しておく. 
  !   * スペクトルデータの格納のされ方は関数 l_nm, nm_l によって
  !     調べることができる.
  !
  ! * nm : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm). 
  !     第 1 次元が水平全波数,  第 2 次元が帯状波数を表す. 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン w_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * n : スペクトルデータの並んだ 1 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm). 
  !   * 第 1 次元が水平全波数を表す. nm は球面調和函数の最大全波数であり, 
  !     サブルーチン w_Initial にてあらかじめ設定しておく.
  !
  ! * x, y : 経度, 緯度方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1) 
  !     および real(8), dimension(1:jm).
  !
  ! * w_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * xy_ で始まる関数が返す値は 2 次元格子点データに同じ.
  !
  ! * x_, y_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! w_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_Lon, y_Lat     ::  格子点座標(緯度, 経度座標)を格納した 1 次元配列
  ! x_Lon_Weight, y_Lat_Weight ::  重み座標を格納した 1 次元配列
  ! xy_Lon, xy_Lat   :: 格子点データの経度・緯度座標(X,Y)
  !                     (格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! xy_w :: スペクトルデータから格子データへの変換
  ! w_xy :: 格子データからスペクトルデータへの変換
  ! l_nm, nm_l :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! w_Lapla_w       :: スペクトルデータにラプラシアンを作用させる
  ! rn              :: スペクトルデータのラプラシアンを計算するための係数. 
  ! irm             :: 経度微分演算用配列
  ! w_LaplaInv_w    :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! w_DLon_w        :: スペクトルデータに経度微分∂/∂λを作用させる
  ! xy_GradLon_w    :: スペクトルデータに
  !                    勾配型経度微分 1/cosφ・∂/∂λを作用させる
  ! xy_GradLat_w    :: スペクトルデータに勾配型緯度微分∂/∂φを作用させる
  ! w_DivLon_xy     :: 格子データに発散型経度微分 1/cosφ・∂/∂λを作用させる
  ! w_DivLat_xy     :: 格子データに
  !                    発散型緯度微分 1/cosφ・∂(g cosφ)/∂φを作用させる
  ! w_Div_xy_xy     :: ベクトル成分である 2 つの格子データに発散を作用させる
  ! w_Jacobian_w_w  :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  !
  !==== 微分(λ,μ=sinφ 座標)
  !
  ! xy_GradLambda_w :: スペクトルデータに勾配型経度微分∂/∂λを作用させる
  ! xy_GradMu_w     :: スペクトルデータに
  !                    勾配型緯度微分 (1-μ^2)∂/∂μを作用させる
  ! w_DivLambda_xy  :: 格子データに
  !                    発散型経度微分 1/(1-μ^2)・∂/∂λを作用させる
  ! w_DivMu_xy      :: 格子データに発散型緯度微分∂/∂μを作用させる
  !
  !==== 補間
  !
  ! Interpolate_w :: スペクトルデータから任意の点での値を求める. 
  !
  !==== 積分・平均
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 次元格子点データの全領域積分および平均
  ! y_IntLon_xy, y_AvrLon_xy   :: 2 次元格子点データの経度方向積分および平均
  ! IntLon_x, AvrLon_x         :: 1 次元(X)格子点データの経度方向積分および平均
  ! x_IntLat_xy, x_AvrLat_xy   :: 2 次元格子点データの緯度方向積分および平均
  ! IntLat_y, AvrLat_y         :: 1 次元(Y)格子点データの緯度方向積分および平均
  !
  !==== スペクトル解析
  !
  ! nm_EnergyFromStreamfunc_w  :: 流線関数からエネルギースペクトルを計算する
  !                               (水平全波数 n, 帯状波数 m 空間)
  ! n_EnergyFromStreamfunc_w   :: 流線関数からエネルギースペクトルを計算する
  !                               (水平全波数 n 空間) 
  ! nm_EnstrophyFromStreamfunc_w  :: 流線関数からエンストロフィースペクトルを
  !                                  計算する (水平全波数 n, 帯状波数 m 空間)
  ! n_EnstrophyFromStreamfunc_w   :: 流線関数からエンストロフィースペクトルを
  !                                  計算する (水平全波数 n 空間)
  ! w_spectrum_VMiss              ::  欠損値
  !
  !
  use w_base_module
  use w_deriv_module

  private

  public w_Initial                            ! 初期化

  public x_Lon, y_Lat                         ! 格子座標
  public x_Lon_weight, y_Lat_Weight           ! 格子座標重み
  public xy_Lon, xy_Lat                       ! 格子座標(im,jm)
  public xy_w, w_xy, l_nm, nm_l               ! 変換関数

  public rn, irm                              ! ラプラシアン/経度微分演算用配列
  public w_Lapla_w, w_LaplaInv_w              ! ラプラシアンと逆演算
  public w_DLon_w                             ! 経度微分
  public xy_GradLon_w, xy_GradLat_w           ! 勾配型微分
  public w_DivLon_xy, w_DivLat_xy             ! 発散型微分
  public w_Div_xy_xy                          ! 発散型微分
  public w_Jacobian_w_w                       ! ヤコビアン
  public xy_GradLambda_w, xy_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xy, w_DivMu_xy           ! 発散型微分(λ,μ座標)

  public Interpolate_w                        ! 補間関数

  public IntLonLat_xy                         ! 緯度経度積分
  public y_IntLon_xy, IntLon_x                ! 経度積分    
  public x_IntLat_xy, IntLat_y                ! 緯度積分    
  public AvrLonLat_xy                         ! 緯度経度平均
  public y_AvrLon_xy, AvrLon_x                ! 経度平均    
  public x_AvrLat_xy, AvrLat_y                ! 緯度平均    

  public nm_EnergyFromStreamfunc_w            ! エネルギースペクトル           
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnergyFromStreamfunc_w             ! エネルギースペクトル
                                              ! (水平全波数 n 空間) 
  public nm_EnstrophyFromStreamfunc_w         ! エンストロフィースペクトル     
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnstrophyFromStreamfunc_w          ! エンストロフィースペクトル  
                                              !  (水平全波数 n 空間)
  public w_spectrum_VMiss                     ! 欠損値

contains

  !--------------- 初期化 -----------------
    subroutine w_initial(n_in,i_in,j_in,np_in)
      !
      ! スペクトル変換の格子点数, 波数および OPENMP 使用時の
      ! 最大スレッド数を設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
      ! しなければならない. 
      !
      ! np_in に 1 より大きな値を指定すれば ISPACK の球面調和函数変換 
      ! OPENMP 並列計算ルーチンが用いられる. 並列計算を実行するには, 
      ! 実行時に環境変数 OMP_NUM_THREADS を np_in 以下の数字に設定する等の
      ! システムに応じた準備が必要となる. 
      !
      ! np_in に 1 より大きな値を指定しなければ並列計算ルーチンは呼ばれない.
      !
      integer,intent(in) :: i_in              !(in) 格子点数(東西)
      integer,intent(in) :: j_in              !(in) 格子点数(南北)
      integer,intent(in) :: n_in              !(in) 切断波数の設定
      integer,intent(in), optional :: np_in   !(in) OPENMP での最大スレッド数

!      integer iw

      if ( present (np_in) )then
         call w_base_initial(n_in,i_in,j_in,np_in)
      else
         call w_base_initial(n_in,i_in,j_in)
      endif

      call w_deriv_initial

    end subroutine w_initial

end module w_module
