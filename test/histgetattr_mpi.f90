!== Sample program for gtool_history/gtool5
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histgetattr_mpi.f90,v 1.1 2009-03-02 05:55:16 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!

program histgetattr
  ! histgetattr.f90 - Sample program for gtool_history/gtool5.
  !                   Test Program for "HistoryGetAttr".
  !

  use dc_trace,   only: SetDebug, DataDump
  use dc_message, only: MessageNotify
  use dc_types,   only: STRING, DP
  use dc_string,  only: StoA, toChar
  use dc_test, only: AssertEqual
  use gtool_history
  use netcdf
  use mpi
  implicit none
  integer   :: err_mpi, myrank_mpi, nprocs_mpi
  integer             :: i,j,k,l                        ! 作業変数
  integer   :: nx, ny, nz                     ! グリッド数
  integer   :: time_period=3                  ! 時間
  real,     allocatable :: x(:)               ! x座標変数
  real(DP), allocatable :: y(:)               ! y座標変数
  real(DP), allocatable :: z(:)               ! z座標変数

  real                :: u           ! 出力兼入力用無次元配列

  character(STRING):: input_file, cval
  integer:: ival
  real:: rval
  real(DP):: dval
  logical:: err
  character(STRING), parameter  :: subname = 'histgetattr_mpi'
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
  allocate( x(nx), y(ny), z(nz) )

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
  call HistoryGetAttr('xhistgetattr_mpi/xhistgetattr_non.nc', 'v', &
    & 'units', cval, err = err, flag_mpi_split=.true.)
  call AssertEqual('err flag test 1', answer = .true., check = err)

  !-------------------------------------------------------------------
  !  まずは入力用のファイルを作成
  !-------------------------------------------------------------------
  input_file = 'xhistgetattr_mpi/xhistgetattr1.nc'

  call HistoryCreate(file=input_file, &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=StoA('x','y','z','t'), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=StoA('X-coordinate','Y-coordinate', 'Z-coordinate','time'), &
    & units=StoA('m','m','m','sec'), &
    & origin=0.0, interval=0.2, &
    & xtypes=StoA('real','double','double','real'), &
    & flag_mpi_split=.true.)

  call HistoryPut('x',x)   ! 次元変数出力
  call HistoryPut('y',y)   ! 次元変数出力
  call HistoryPut('z',z)   ! 次元変数出力

  call HistoryAddVariable('scalar', dims=(/''/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('u', dims=(/'t'/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')

  call HistoryAddAttr('x','topology','circular' )
  call HistoryAddAttr('x','modulo', 360.0)

  call HistoryAddAttr('u','missing_value', -9999.0_DP)

  call HistoryPut('scalar', 123.0)
  do, l = 1, time_period
    u = real(l)*0.100000001
    call HistoryPut('t', real(l)*0.2)
    call HistoryPut('u', u)
  enddo

  call HistoryClose

  call MessageNotify('M', subname, 'Input file <%c> is generated.', &
    & c1=trim(input_file))

  !-------------------------------------------------------------------
  !  属性入力テスト
  !-------------------------------------------------------------------
  call HistoryGetAttr(input_file, 't', &
    & 'units', cval, flag_mpi_split=.true. )
  call AssertEqual('HistoryGetAttr Char test 1', &
    & answer = cval, check = 'sec')

  call HistoryGetAttr(input_file, 't', &
    & 'missing_value', rval, flag_mpi_split=.true.  )
  call AssertEqual('HistoryGetAttr Real test 1', &
    & answer = rval, check = NF90_FILL_FLOAT)

  call HistoryGetAttr(input_file, 't', &
    & 'missing_value', dval, flag_mpi_split=.true.  )
  call AssertEqual('HistoryGetAttr Dble test 1', &
    & answer = dval, check = NF90_FILL_DOUBLE)

  call HistoryGetAttr(input_file, 't', &
    & 'missing_value', ival, flag_mpi_split=.true.  )
  call AssertEqual('HistoryGetAttr Int test 1', &
    & answer = ival, check = NF90_FILL_INT)

  call HistoryGetAttr(input_file, 'x', &
    & 'topology', cval, flag_mpi_split=.true.  )
  call AssertEqual('HistoryGetAttr Char test 2', &
    & answer = cval, check = 'circular')

  call HistoryGetAttr(input_file, 'x', &
    & '+title', cval, flag_mpi_split=.true.  )
  call AssertEqual('HistoryGetAttr Char test 3', &
    & answer = cval, check = 'gtool_history HistoryGet test 1')

  call HistoryGetAttr(input_file, 'x', &
    & 'modulo', rval, flag_mpi_split=.true.  )
  call AssertEqual('HistoryGetAttr Real test 2', &
    & answer = rval, check = 360.0)

  call HistoryGetAttr(input_file, 'u', &
    & 'missing_value', dval, flag_mpi_split=.true.  )
  call AssertEqual('HistoryGetAttr Dble test 2', &
    & answer = dval, check = -9999.0_DP)


  !-----------------------------------------------------------------
  !  MPI 終了処理
  !-----------------------------------------------------------------
  call MPI_Finalize(err_mpi)

end program histgetattr
