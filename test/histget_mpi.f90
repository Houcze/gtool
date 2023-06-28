!== Sample program for gtool_history/gtool5 (MPI version)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histget_mpi.f90,v 1.4 2009-03-02 05:55:16 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!

program histget
  ! histget_mpi.f90 - Sample program for gtool_history/gtool5 (MPI).
  !                   Test Program for "HistoryGet".
  !

  use dc_trace,   only: SetDebug, DataDump
  use dc_message, only: MessageNotify
  use dc_types,   only: STRING, DP
  use dc_string,  only: StoA, toChar
  use dc_test, only: AssertEqual
  use gtool_history
  use mpi
  implicit none
  integer   :: err_mpi, myrank_mpi, nprocs_mpi
  integer   :: i,j,k,l                        ! 作業変数
  integer   :: nx, ny, nz                     ! グリッド数
  integer   :: time_period=3                  ! 時間
  real,     allocatable :: x(:)               ! x座標変数
  real(DP), allocatable :: y(:)               ! y座標変数
  real(DP), allocatable :: z(:)               ! z座標変数

  real                  :: u           ! 出力兼入力用無次元配列
  real(DP), allocatable :: v(:)        ! 出力兼入力用 1 次元配列
  real(DP), allocatable :: v2(:)       ! 出力兼入力用 1 次元配列その2
  integer , allocatable :: w(:,:)      ! 出力兼入力用 2 次元配列
  real(DP), allocatable :: q(:,:,:)    ! 出力兼入力用 3 次元配列

  real(DP), allocatable:: qa(:,:,:)  ! 入力用 3 次元配列

  real,     pointer   :: up       =>null() ! 入力用無次元配列ポインタ
  real(DP), pointer   :: vp(:)    =>null() ! 入力用 1 次元配列ポインタ
  integer,  pointer   :: wp(:,:)  =>null() ! 入力用 2 次元配列ポインタ
  real(DP), pointer   :: qp(:,:,:)=>null() ! 入力用 3 次元配列ポインタ

  real,     pointer   :: xp(:) =>null()    ! 入力用 x座標元配列ポインタ
  real(DP), pointer   :: yp(:) =>null()    ! 入力用 y座標元配列ポインタ
  real(DP), pointer   :: zp(:) =>null()    ! 入力用 z座標元配列ポインタ

  real(DP), pointer   :: vp_range     =>null() ! range での入力テスト用
  real(DP), pointer   :: qp_range(:,:)=>null() ! range での入力テスト用

  real(DP):: rettime
  logical:: texist
  logical:: err
  character(STRING), parameter  :: subname = 'histget_mpi'
continue

  !-----------------------------------------------------------------
  !  MPI 初期設定
  !-----------------------------------------------------------------
  call MPI_Init(err_mpi)
  call MPI_Comm_Rank(MPI_COMM_WORLD, myrank_mpi, err_mpi)
  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs_mpi, err_mpi)

  if ( nprocs_mpi /= 4 ) then
    call MessageNotify( 'E', 'histget_mpi', 'Test must be done with 4 node')
  end if

  call SetDebug

  !-------------------------------------------------------------------
  !  割付と初期値設定
  !-------------------------------------------------------------------
  nx = 3 ; ny = 2 ; nz = 2
  allocate( x(nx), y(ny), z(nz), v(nx), v2(nz), w(nx,ny), q(nx,ny,nz) )

  x(:) = (/( 1.0*(i-1), i=1, nx )/)
  select case( myrank_mpi )
  case(0)
    y(:) = (/  0.0, 10.0 /) ; z(:) = (/   0.0, 100.0 /)
  case(1)
    y(:) = (/  0.0, 10.0 /) ; z(:) = (/ 200.0, 300.0 /)
  case(2)
    y(:) = (/ 20.0, 30.0 /) ; z(:) = (/   0.0, 100.0 /)
  case(3)
    y(:) = (/ 20.0, 30.0 /) ; z(:) = (/ 200.0, 300.0 /)
  case default
    call MessageNotify( 'E', 'histget_mpi', 'Test must be done with 4 node')
  end select

  !-------------------------------------------------------------------
  !  エラーフラグのチェック
  !-------------------------------------------------------------------
  err = .false.
  call HistoryGet('xhistget_mpi/xhistget_non.nc', 'v', v, err = err)
  call AssertEqual('err flag test 1', answer = .true., check = err)

  !-------------------------------------------------------------------
  !  まずは入力用のファイルを作成
  !-------------------------------------------------------------------

  call HistoryCreate(file='xhistget_mpi/xhistget1.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=StoA('x','y','z','t'), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=StoA('X-coordinate','Y-coordinate', 'Z-coordinate','time'), &
    & units=StoA('m','m','m','s'), &
    & origin=0.0, interval=0.2, &
    & xtypes=StoA('real','double','double','real'), &
    & flag_mpi_split=.true. )

  call HistoryPut('x',x)   ! 次元変数出力
  call HistoryPut('y',y)   ! 次元変数出力
  call HistoryPut('z',z)   ! 次元変数出力

  call HistoryAddVariable('scalar', dims=(/''/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('u', dims=(/'t'/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('v', dims=(/'x','t'/), &
    & longname='1 Dimensional any quantity', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('v2', dims=(/'z','t'/), &
    & longname='1 Dimensional any quantity II', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('w', dims=(/'x','y','t'/), &
    & longname='2 Dimensional any quantity', &
    & units='2-dimensional any unit', xtype='int')
  call HistoryAddVariable('q', dims=(/'x','y','z','t'/), &
    & longname='3 Dimensional any quantity', &
    & units='3-dimensional any unit', xtype='double')

  call HistoryPut('scalar', 123.0)
  do, l = 1, time_period
    u = real(l)*0.100000001
    do i = 1, nx
      v(i) = dble(l)*0.100000001d0+dble(i)*10.0000001d0
      do j = 1, ny
        w(i,j) = l + i * 10 + j * 1000
        do k = 1, nz
          q(i,j,k) = dble(l)*0.100000001d0     &
            & + dble(i)*10.0000001d0  &
            & + dble(j)*1000.00001d0  &
            & + dble(k)*100000.001d0

          v2(k) = dble(k)*100000.001d0 + dble(l)
        enddo
      enddo
    enddo
    call HistoryPut('t', real(l)*0.2)
    call HistoryPut('u', u)
    call HistoryPut('v', v)
    call HistoryPut('v2', v2)
    call HistoryPut('w', w)
    call HistoryPut('q', q)

  enddo

  call DataDump('v', v, strlen=10)
  call DataDump('w', dble(w), strlen=60)
  call DataDump('q(1,1,', q(1,1,:))

  call HistoryClose

  !-------------------------------------------------------------------
  !  固定長配列による入力テスト (色々な作法で入力)
  !-------------------------------------------------------------------

  call MessageNotify('M', subname, 'Getting by Fixed-Length Array.')

  call HistoryCreate(file='xhistget_mpi/xhistget2.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/), &
    & flag_mpi_split=.true. )

  call HistoryPut('x',x)   ! 次元変数出力
  call HistoryPut('y',y)   ! 次元変数出力
  call HistoryPut('z',z)   ! 次元変数出力

  call HistoryAddVariable('scalar', dims=(/''/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('u', dims=(/'t'/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('v', dims=(/'x','t'/), &
    & longname='1 Dimensional any quantity', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('v2', dims=(/'z','t'/), &
    & longname='1 Dimensional any quantity II', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('w', dims=(/'x','y','t'/), &
    & longname='2 Dimensional any quantity', &
    & units='2-dimensional any unit', xtype='int')
  call HistoryAddVariable('q', dims=(/'x','y','z','t'/), &
    & longname='3 Dimensional any quantity', &
    & units='3-dimensional any unit', xtype='double')

  call HistoryGet(file='xhistget_mpi/xhistget1.nc', varname='scalar', &
    & array=u, flag_mpi_split=.true. )
  call HistoryPut('scalar', u)
  do, l = 1, time_period - 1
    call HistoryPut('t', real(l)*0.2)
    call HistoryGet(file='xhistget_mpi/xhistget1.nc', varname='u', & 
      &          array=u, time=real(l)*0.2, flag_mpi_split=.true.)
    call HistoryPut('u', u)
    call HistoryGet('xhistget_mpi/xhistget1.nc', 'v', v, dble(l)*0.2_DP, flag_mpi_split=.true.)
    call HistoryPut('v', v)
    call HistoryGet('xhistget_mpi/xhistget1.nc', 'v2', v2, range='t=^'//toChar(l), flag_mpi_split=.true.)
    call HistoryPut('v2', v2)
    call HistoryGet('xhistget_mpi/xhistget1.nc', 'w', w, time=real(l)*0.2, flag_mpi_split=.true.)
    call HistoryPut('w', w)
    call HistoryGet('xhistget_mpi/xhistget1.nc', 'q', q, 't=^'//toChar(l), flag_mpi_split=.true.)
    call HistoryPut('q', q)
  enddo

  !---------------------------
  !  時間の自動判別のテスト
  l = time_period
  call HistoryPut('t', real(l)*0.2)
  call HistoryGet(file='xhistget_mpi/xhistget1.nc', varname='u', array=u, flag_mpi_split=.true.)
  call HistoryPut('u', u)
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'v', v, range='', flag_mpi_split=.true.)
  call HistoryPut('v', v)
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'v2', v2, time=real(l)*0.2, flag_mpi_split=.true.)
  call HistoryPut('v2', v2)
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'w', w, flag_mpi_split=.true.)
  call HistoryPut('w', w)
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'q', q, 't=^'//toChar(l), flag_mpi_split=.true.)
  call HistoryPut('q', q)

  call HistoryClose

  !---------------------------
  !  配列サイズが異なる場合の処理
  allocate( qa (nx+1,ny+1,nz+1) )

  err = .false.
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'v', qa, &
    & quiet = .true., err = err, flag_mpi_split=.true. )
  call AssertEqual('rank consistency test 1', answer = .true., check = err)

  err = .false.
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'q', qa, &
    & quiet = .true., err = err, flag_mpi_split=.true. )
  call AssertEqual('shape consistency test 1', answer = .true., check = err)

  deallocate ( qa )
  allocate( qa (nx,ny,nz) )

  err = .true.
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'q', qa, err = err, flag_mpi_split=.true. )
  call AssertEqual('shape consistency test 2', answer = .false., check = err)

  !-------------------------------------------------------------------
  !  ポインタ配列による入力テスト (色々な作法で入力)
  !-------------------------------------------------------------------


  call HistoryCreate(file='xhistget_mpi/xhistget3.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/), &
    & flag_mpi_split=.true. )

  call HistoryPut('x',x)   ! 次元変数出力
  call HistoryPut('y',y)   ! 次元変数出力
  call HistoryPut('z',z)   ! 次元変数出力

  call HistoryAddVariable('scalar', dims=(/''/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('u', dims=(/'t'/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('v', dims=(/'x','t'/), &
    & longname='1 Dimensional any quantity', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('v2', dims=(/'z','t'/), &
    & longname='1 Dimensional any quantity II', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('w', dims=(/'x','y','t'/), &
    & longname='2 Dimensional any quantity', &
    & units='2-dimensional any unit', xtype='int')
  call HistoryAddVariable('q', dims=(/'x','y','z','t'/), &
    & longname='3 Dimensional any quantity', &
    & units='3-dimensional any unit', xtype='double')

  call HistoryGetPointer(file='xhistget_mpi/xhistget1.nc', varname='scalar', &
    & array=up, flag_mpi_split=.true.)
  call HistoryPut('scalar', up)
  deallocate(up)
  do, l = 1, time_period - 1
    call HistoryPut('t', real(l)*0.2)
    call HistoryGetPointer(file='xhistget_mpi/xhistget1.nc', varname='u', & 
      &          array=up, time=real(l, DP) * 0.2_DP, flag_mpi_split=.true. )
    call HistoryPut('u', up)
    deallocate(up)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'v', vp, dble(l)*0.2_DP, flag_mpi_split=.true.)
    call HistoryPut('v', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'v2', vp, range='t=^'//toChar(l), flag_mpi_split=.true.)
    call HistoryPut('v2', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'w', wp, time=real(l)*0.2, flag_mpi_split=.true.)
    call HistoryPut('w', wp)
    deallocate(wp)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'q', qp, 't=^'//toChar(l), flag_mpi_split=.true.)
    call HistoryPut('q', qp)
    deallocate(qp)
  enddo

  !---------------------------
  !  時間の自動判別のテスト
  l = time_period
  call HistoryPut('t', real(l)*0.2)
  call HistoryGetPointer(file='xhistget_mpi/xhistget1.nc', varname='u', array=up, flag_mpi_split=.true.)
  call HistoryPut('u', up)
  deallocate(up)
  call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'v', vp, range='', flag_mpi_split=.true.)
  call HistoryPut('v', vp)
  deallocate(vp)
  call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'v2', vp, dble(l)*0.2_DP, flag_mpi_split=.true.)
  call HistoryPut('v2', vp)
  deallocate(vp)
  call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'w', wp, flag_mpi_split=.true.)
  call HistoryPut('w', wp)
  deallocate(wp)
  call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'q', qp, 't=^'//toChar(l), flag_mpi_split=.true.)
  call HistoryPut('q', qp)
  deallocate(qp)

  call HistoryClose

  !-------------------------------------------------------------------
  !  次元データの入力テスト (色々な作法で入力)
  !-------------------------------------------------------------------
  call MessageNotify('M', subname, 'Getting by Various Forms.')

  call HistoryCreate(file='xhistget_mpi/xhistget4.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/), &
    & flag_mpi_split=.true.)

  ! 次元データを入力する
  call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'x' ,xp, flag_mpi_split=.true.)   ! x 次元入力
  call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'y' ,yp, flag_mpi_split=.true.)   ! y 次元入力
  call HistoryGetPointer(file='xhistget_mpi/xhistget1.nc', varname='z' ,array=zp, flag_mpi_split=.true.)   ! z 次元入力

  call HistoryPut('x',xp)   ! 次元変数出力
  call HistoryPut('y',yp)   ! 次元変数出力
  call HistoryPut('z',zp)   ! 次元変数出力

  call HistoryAddVariable('scalar', dims=(/''/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('u', dims=(/'t'/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('v', dims=(/'x','t'/), &
    & longname='1 Dimensional any quantity', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('v2', dims=(/'z','t'/), &
    & longname='1 Dimensional any quantity II', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('w', dims=(/'x','y','t'/), &
    & longname='2 Dimensional any quantity', &
    & units='2-dimensional any unit', xtype='int')
  call HistoryAddVariable('q', dims=(/'x','y','z','t'/), &
    & longname='3 Dimensional any quantity', &
    & units='3-dimensional any unit', xtype='double')


  call HistoryGetPointer(file='xhistget_mpi/xhistget1.nc', varname='scalar', &
    & array=up, flag_mpi_split=.true.)
  call HistoryPut('scalar', up)
  deallocate(up)
  do, l = 1, time_period
    call HistoryPut('t', real(l)*0.2)
    call HistoryGetPointer(file='xhistget_mpi/xhistget1.nc', varname='u', & 
      &          array=up, time=real(l) * 0.2, flag_mpi_split=.true. )
    call HistoryPut('u', up)
    deallocate(up)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'v', vp, dble(l)*0.2_DP, flag_mpi_split=.true.)
    call HistoryPut('v', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'v2', vp, range='t=^'//toChar(l), flag_mpi_split=.true.)
    call HistoryPut('v2', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'w', wp, time=real(l)*0.2, flag_mpi_split=.true.)
    call HistoryPut('w', wp)
    deallocate(wp)
    call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'q', qp, 't=^'//toChar(l), flag_mpi_split=.true.)
    call HistoryPut('q', qp)
    deallocate(qp)
  enddo

  call HistoryClose


  !-------------------------------------------------------------------
  !  returned_value と flag_time_exist のテスト
  !-------------------------------------------------------------------
  call HistoryGet('xhistget_mpi/xhistget1.nc', 'scalar', u, &
    & returned_time = rettime, flag_time_exist = texist, flag_mpi_split=.true. )
  call AssertEqual('returned_time test 1', &
    & answer = int(rettime * 10000), check = 0 )
  call AssertEqual('flag_time_exist test 1', &
    & answer = texist, check = .false. )

  call HistoryGet('xhistget_mpi/xhistget1.nc', 'u', u, &
    & returned_time = rettime, flag_time_exist = texist, flag_mpi_split=.true. )
  call AssertEqual('returned_time test 2', &
    & answer = int(rettime * 10000), check = 6000 )
  call AssertEqual('flag_time_exist test 2', &
    & answer = texist, check = .true. )

  call HistoryGet('xhistget_mpi/xhistget1.nc', 'u', u, time=0., &
    & returned_time = rettime, flag_mpi_split=.true. )
  call AssertEqual('returned_time test 3', &
    & answer = int(rettime * 10000), check = 2000 )

  if (associated(vp)) deallocate(vp)
  call HistoryGetPointer('xhistget_mpi/xhistget1.nc', 'v', vp, range='t=0.35', &
    & returned_time = rettime, flag_mpi_split=.true. )
  if (associated(vp)) deallocate(vp)
  call AssertEqual('returned_time test 4', &
    & answer = int(rettime * 10000), check = 4000 )


  if ( associated(up) ) deallocate(up)
  if ( associated(vp) ) deallocate(vp)
  if ( associated(wp) ) deallocate(wp)
  if ( associated(qp) ) deallocate(qp)
  if ( associated(xp) ) deallocate(xp)
  if ( associated(yp) ) deallocate(yp)
  if ( associated(zp) ) deallocate(zp)
  if ( associated(vp_range) ) deallocate(vp_range)
  if ( associated(qp_range) ) deallocate(qp_range)

  !-----------------------------------------------------------------
  !  MPI 終了処理
  !-----------------------------------------------------------------
  call MPI_Finalize(err_mpi)

end program histget
