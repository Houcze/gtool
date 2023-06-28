!== Sample program for gtool_history/gtool5 with MPI
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histtest_mpi.f90,v 1.10 2009-10-12 04:49:40 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2008. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!
program histtest_mpi
  !
  ! Test Program for "HistoryCreate", "HistoryAddVariable",
  ! "HistoryPut", "HistorySetTime", "HistoryClose".
  !
  use dc_string, only: toChar
  use dc_types, only: STRING, DP, TOKEN
  use dc_trace, only: SetDebug
  use dc_test, only: AssertEqual, AssertGreaterThan, AssertLessThan
  use dc_date_types, only: DC_DIFFTIME
  use dc_date, only: DCDiffTimeCreate, operator(+), operator(*), mod, &
    & DCDiffTimePutLine, EvalNonDim, EvalSec
  use gtool_history, only: GT_HISTORY, HistoryCreate, HistoryAddVariable, &
    &                    HistoryClose, HistoryCopy, HistoryPut, &
    &                    HistorySetTime, HistoryAddAttr, HistoryPutLine, &
    &                    HistoryPutAxisMPI
  use dc_string, only: StoA, CPrintf
  use dc_message, only: MessageNotify
  use mpi
  implicit none
  integer:: err_mpi, myrank_mpi, nprocs_mpi
  integer, allocatable:: dimsizes1(:)
  real(DP), allocatable:: axis1(:), axis2(:), axis3(:)
  real(DP), allocatable:: axis1_all(:), axis2_all(:), axis3_all(:)
  real(DP), allocatable:: xy_U(:,:), x_V(:), xy_W(:,:), y_P(:)
  real(DP), allocatable:: x_Lon_Weight(:)
  real(DP):: Total
  real, allocatable:: time_set(:)
  integer:: i, j
  character(TOKEN):: char
continue

  !-----------------------------------------------------------------
  !  MPI 初期設定
  !-----------------------------------------------------------------
  call MPI_Init(err_mpi)
  call MPI_Comm_Rank(MPI_COMM_WORLD, myrank_mpi, err_mpi)
  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs_mpi, err_mpi)

  if ( nprocs_mpi /= 4 ) then
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end if

  call SetDebug

!goto 1100

100 continue

  !-----------------------------------------------------------------
  !  分散出力テスト (lon 方向に等間隔に並列化)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3), axis1(1:2), axis2(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(1) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(2) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/40.0, 50.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(3) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/60.0, 70.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi1.nc',       &
    & title = 'gtool_history MPI test 1',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_split = .true.  )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2  )

!!  call HistoryAddAttr('lon', 'topology', 'circular')
!!  call HistoryAddAttr('lon', 'modulo',   360.0)

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  allocate( xy_U(2,3) )
  allocate( x_V(2) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  do i = 1, 2
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( xy_U, x_V )

200 continue

  !-----------------------------------------------------------------
  !  統合出力テスト (lon 方向に等間隔に並列化)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3), axis1(1:2), axis2(1:3) )
  allocate( axis1_all(1:8), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(1) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(2) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/40.0, 50.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(3) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/60.0, 70.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi2.nc',       &
    & title = 'gtool_history MPI test 2',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:8) = (/(real(i) * 10.0, i = 0, 7)/)
  axis2_all(1:3) = (/-10.0, 0.0, 10.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

 call HistoryAddAttr('lon', 'topology', 'circular')
 call HistoryAddAttr('lon', 'modulo',   360.0)

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )
  call HistoryAddAttr('U', 'comment', 'test')

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  allocate( xy_U(2,3) )
  allocate( x_V(2) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  do i = 1, 2
    call HistoryPut('t', real(i-1) )
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V )


300 continue

  !-----------------------------------------------------------------
  !  統合出力テスト (lat, lon 方向に等間隔に並列化)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3), axis1(1:2), axis2(1:3) )
  allocate( axis1_all(1:4), axis2_all(1:6) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/-0.05_DP, -0.03_DP, -0.01_DP/)
  case(1) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/ 0.01_DP,  0.03_DP,  0.05_DP/)
  case(2) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/-0.05_DP, -0.03_DP, -0.01_DP/)
  case(3) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/ 0.01_DP,  0.03_DP,  0.05_DP/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi3.nc', &
    & title = 'gtool_history MPI test 3',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:4) = (/(real(i) * 10.0, i = 0, 3)/)
  axis2_all(1:6) = (/(real(i) * 0.02_DP - 0.07_DP, i = 1, 6)/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )
  call HistoryAddAttr('U', 'comment', 'test')

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  allocate( xy_U(2,3) )
  allocate( x_V(2) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 1000.0_DP + axis2(j) * 1000.0_DP
!      xy_U(i,j) = axis2(j) * 100.0_DP
      x_V(i) = axis1(i) * 1000.0
!write(*,*) '# r', myrank_mpi, '##### main ## i=', i, ', j=', j, ', U=', xy_U(i,j)
    end do
  end do

  do i = 1, 2
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V )

400 continue

  !-----------------------------------------------------------------
  !  分散出力テスト (lon 方向に不等間隔に並列化)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3) )
  allocate( axis1_all(1:12), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/3,3,0/)
    allocate( axis1(1:3), axis2(1:3) )
    axis1(:) = (/ 0.0, 10.0, 20.0/) ;             axis2(:) = (/-10.0, 0.0, 10.0/)
  case(1) ; dimsizes1(:) = (/1,3,0/)
    allocate( axis1(1:1), axis2(1:3) )
    axis1(:) = (/30.0/) ;                         axis2(:) = (/-10.0, 0.0, 10.0/)
  case(2) ; dimsizes1(:) = (/5,3,0/)
    allocate( axis1(1:5), axis2(1:3) )
    axis1(:) = (/40.0, 50.0, 60.0, 70.0, 80.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(3) ; dimsizes1(:) = (/4,3,0/)
    allocate( axis1(1:4), axis2(1:3) )
    axis1(:) = (/90.0, 100.0, 110.0, 120.0/) ;    axis2(:) = (/-10.0, 0.0, 10.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi4.nc',       &
    & title = 'gtool_history MPI test 4',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_split = .true. , &
    & flag_mpi_gather = .false. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:12) = (/(real(i) * 10.0, i = 0, 11)/)
  axis2_all(1:3) = (/-10.0, 0.0, 10.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  allocate( xy_U( dimsizes1(1), dimsizes1(2) ) )
  allocate( x_V( dimsizes1(1) ) )
  do j = 1, dimsizes1(2)
    do i = 1, dimsizes1(1)
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  do i = 1, 2
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V )


500 continue

  !-----------------------------------------------------------------
  !  統合出力テスト (lon 方向に不等間隔に並列化, 時間発展しない値の出力)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3) )
  allocate( axis1_all(1:13), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/3,3,0/)
    allocate( axis1(1:3), axis2(1:3), x_Lon_Weight(1:3) )
    axis1(:) = (/ 0.0, 10.0, 20.0/) ;             axis2(:) = (/-10.0, 0.0, 10.0/)
    x_Lon_Weight(:) = (/14.0, 12.0, 10.0/)
  case(1) ; dimsizes1(:) = (/1,3,0/)
    allocate( axis1(1:1), axis2(1:3), x_Lon_Weight(1:1)  )
    axis1(:) = (/30.0/) ;                         axis2(:) = (/-10.0, 0.0, 10.0/)
    x_Lon_Weight(:) = (/8.0/)
  case(2) ; dimsizes1(:) = (/5,3,0/)
    allocate( axis1(1:5), axis2(1:3), x_Lon_Weight(1:5)  )
    axis1(:) = (/40.0, 50.0, 60.0, 70.0, 80.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
    x_Lon_Weight(:) = (/6.0, 4.0, 2.0, 4.0, 6.0/)
  case(3) ; dimsizes1(:) = (/4,3,0/)
    allocate( axis1(1:4), axis2(1:3), x_Lon_Weight(1:4)  )
    axis1(:) = (/90.0, 100.0, 110.0, 120.0/) ;    axis2(:) = (/-10.0, 0.0, 10.0/)
    x_Lon_Weight(:) = (/8.0, 10.0, 12.0, 14.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi5.nc',       &
    & title = 'gtool_history MPI test 5',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_split = .false., flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:13) = (/(real(i) * 10.0, i = 0, 12)/)
  axis2_all(1:3) = (/-10.0, 0.0, 10.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='lon_weight', dims=(/'lon'/)         , &
    & longname='weight for integration or average in lon', units='deg.' )

  call HistoryAddAttr( &                          ! 変数定義
    & varname='lon', attrname='gt_calc_weight', &
    & value='lon_weight' )

  call HistoryPut('lon_weight', x_Lon_Weight)

  allocate( xy_U( dimsizes1(1), dimsizes1(2) ) )
  allocate( x_V( dimsizes1(1) ) )
  do j = 1, dimsizes1(2)
    do i = 1, dimsizes1(1)
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  do i = 1, 2
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V, x_Lon_Weight )


600 continue

  !-----------------------------------------------------------------
  !  統合出力テスト (lon 方向に飛び飛びで並列化)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3) )
  allocate( axis1_all(1:13), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/3,3,0/)
    allocate( axis1(1:3), axis2(1:3) )
    axis1(:) = (/ 0.0, 110.0, 120.0/) ;           axis2(:) = (/-10.0, 0.0, 10.0/)
  case(1) ; dimsizes1(:) = (/1,3,0/)
    allocate( axis1(1:1), axis2(1:3) )
    axis1(:) = (/10.0/) ;                         axis2(:) = (/-10.0, 0.0, 10.0/)
  case(2) ; dimsizes1(:) = (/5,3,0/)
    allocate( axis1(1:5), axis2(1:3) )
    axis1(:) = (/20.0, 30.0, 60.0, 70.0, 90.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(3) ; dimsizes1(:) = (/4,3,0/)
    allocate( axis1(1:4), axis2(1:3) )
    axis1(:) = (/40.0, 50.0, 80.0, 100.0/) ;      axis2(:) = (/-10.0, 0.0, 10.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi6.nc',       &
    & title = 'gtool_history MPI test 6',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_split = .false., flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:13) = (/(real(i) * 10.0, i = 0, 12)/)
  axis2_all(1:3) = (/-10.0, 0.0, 10.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  allocate( xy_U( dimsizes1(1), dimsizes1(2) ) )
  allocate( x_V( dimsizes1(1) ) )
  do j = 1, dimsizes1(2)
    do i = 1, dimsizes1(1)
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  do i = 1, 2
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V )


700 continue

  !-----------------------------------------------------------------
  !  統合出力テスト (時間平均出力)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3), axis1(1:2), axis2(1:3) )
  allocate( axis1_all(1:8), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case(1) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case(2) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/40.0, 50.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case(3) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/60.0, 70.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi7.nc',       &
    & title = 'gtool_history MPI test 7',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & origin = 0.0, interval=10.0, &
    & flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:8) = (/(real(i) * 10.0, i = 0, 7)/)
  axis2_all(1:3) = (/0.0, 10.0, 20.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double', &
    & time_average=.true. )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='W', dims=(/'lon', 'lat','t  '/)         , &
    & longname='vertical wind', units='m s-1', xtype='double' )

  allocate( xy_U(2,3) )
  allocate( xy_W(2,3) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      xy_W(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
    end do
  end do

  do i = 1, 30
    call HistoryPut('U', xy_U * i, time=real(i) )
    call HistoryPut('W', xy_W * i, time=real(i) )
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, xy_W )

800 continue

  !-----------------------------------------------------------------
  !  統合出力テスト (空間平均出力, スカラー値の出力)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3), axis1(1:2), axis2(1:3) )
  allocate( axis1_all(1:5), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case(1) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/10.0, 20.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case(2) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case(3) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/30.0, 40.0/) ; axis2(:) = (/0.0, 10.0, 20.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi8.nc',       &
    & title = 'gtool_history MPI test 8',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & origin = 0.0, interval=10.0, &
    & flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:5) = (/(real(i) * 10.0, i = 0, 4)/)
  axis2_all(1:3) = (/0.0, 10.0, 20.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='P', dims=(/'lat','t  '/)         , &
    & longname='unknown variable', units='?', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='Total', dims=(/'t  '/)         , &
    & longname='unknown total value', units='?', xtype='double' )


  allocate( xy_U(2,3) )
  allocate( y_P(3) )
  Total = 10000 * ( myrank_mpi + 1 )
  do j = 1, 3
    y_P(j) = axis2(j) * 1.0 + 10000 * ( myrank_mpi + 1 )
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0 + 10000 * ( myrank_mpi + 1 )
    end do
  end do

  do i = 1, 2
    call HistoryPut('U', xy_U * i )
    call HistoryPut('P', y_P * i )
    call HistoryPut('Total', Total * i )
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, y_P )

900 continue

  !-----------------------------------------------------------------
  !  統合出力テスト (SetTime 使用, 時間発展しない値の出力)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3), axis1(1:2), axis2(1:3) )
  allocate( axis1_all(1:8), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(1) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(2) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/40.0, 50.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(3) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/60.0, 70.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi9.nc',       &
    & title = 'gtool_history MPI test 9',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )

  axis1_all(1:8) = (/(real(i) * 10.0, i = 0, 7)/)
  axis2_all(1:3) = (/-10.0, 0.0, 10.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double', &
    & time_average=.true. )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  allocate( xy_U(2,3) )
  allocate( x_V(2) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  allocate( time_set(1:6) )
  time_set(1:6) = (/10.0, 40.0, 50.0, 100.0, 200.0, 220.0/)
  do i = 1, 6
    call HistorySetTime( time_set(i) )
    call HistoryPut('U', xy_U * i, time = time_set(i))
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V )
  deallocate( time_set )


1000 continue

  !-----------------------------------------------------------------
  !  統合出力時の例外処理
  !    * HistoryPut に座標サイズより大きいデータが与えられた場合
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:3), axis1(1:2), axis2(1:4) )
  allocate( axis1_all(1:8), axis2_all(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/-10.0, 0.0, 10.0, 20.0/)
  case(1) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/-10.0, 0.0, 10.0, 20.0/)
  case(2) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/40.0, 50.0/) ; axis2(:) = (/-10.0, 0.0, 10.0, 20.0/)
  case(3) ; dimsizes1(:) = (/2,3,0/)
    axis1(:) = (/60.0, 70.0/) ; axis2(:) = (/-10.0, 0.0, 10.0, 20.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi10.nc',       &
    & title = 'gtool_history MPI test 10',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','t  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','time     '/), &
    & units = (/'deg.','deg.','sec.'/), &
    & flag_mpi_gather = .true.  )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2  )

  axis1_all(1:8) = (/(real(i) * 10.0, i = 0, 7)/)
  axis2_all(1:3) = (/-10.0, 0.0, 10.0/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )

!!  call HistoryAddAttr('lon', 'topology', 'circular')
!!  call HistoryAddAttr('lon', 'modulo',   360.0)

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  allocate( xy_U(2,3) )
  allocate( x_V(2) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  do i = 1, 2
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V )



1100 continue

  !-----------------------------------------------------------------
  !  文字型データ出力 (文字型データ自体は統合しないが,
  !  他データを統合出力時に問題なく出力できるか確認のため)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:4), axis1(1:2), axis2(1:3) )
  allocate( axis1_all(1:4), axis2_all(1:6) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/-0.05_DP, -0.03_DP, -0.01_DP/)
  case(1) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/ 0.01_DP,  0.03_DP,  0.05_DP/)
  case(2) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/-0.05_DP, -0.03_DP, -0.01_DP/)
  case(3) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/ 0.01_DP,  0.03_DP,  0.05_DP/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi11.nc', &
    & title = 'gtool_history MPI test 11',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','stl', 't  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','str len  ','time     '/), &
    & units = (/'deg.','deg.','none','sec.'/), &
    & flag_mpi_gather = .true. )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2 )
  call HistoryPut( 'stl', (/(real(i), i = 1, 32)/) )

  axis1_all(1:4) = (/(real(i) * 10.0, i = 0, 3)/)
  axis2_all(1:6) = (/(real(i) * 0.02_DP - 0.07_DP, i = 1, 6)/)

  call HistoryPutAxisMPI( 'lon', axis1_all )
  call HistoryPutAxisMPI( 'lat', axis2_all )
  call HistoryPutAxisMPI( 'stl', (/(real(i), i = 1, 32)/) )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )
  call HistoryAddAttr('U', 'comment', 'test')

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='StrR', dims=(/'stl','t  '/)         , &
    & longname='real', units='none', xtype='float' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='Str', dims=(/'stl','t  '/)         , &
    & longname='string', units='none', xtype='char' )

  allocate( xy_U(2,3) )
  allocate( x_V(2) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 1000.0_DP + axis2(j) * 1000.0_DP
!      xy_U(i,j) = axis2(j) * 100.0_DP
      x_V(i) = axis1(i) * 1000.0
!write(*,*) '# r', myrank_mpi, '##### main ## i=', i, ', j=', j, ', U=', xy_U(i,j)
    end do
  end do

  do i = 1, 3
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
    call HistoryPut('StrR', (/(real(j) * i, j = 1, 32)/))
    char = 'loop=' // toChar(i)
    call HistoryPut('Str', char)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( axis1_all, axis2_all )
  deallocate( xy_U, x_V )


1200 continue

  !-----------------------------------------------------------------
  !  文字型データ出力 (文字型データ自体は分散しないが,
  !  他データを分散出力時に問題なく出力できるか確認のため)
  !-----------------------------------------------------------------
  allocate( dimsizes1(1:4), axis1(1:2), axis2(1:3) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/ 0.0, 10.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(1) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/20.0, 30.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(2) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/40.0, 50.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case(3) ; dimsizes1(:) = (/2,3,32,0/)
    axis1(:) = (/60.0, 70.0/) ; axis2(:) = (/-10.0, 0.0, 10.0/)
  case default
    call MessageNotify( 'E', 'histtest_mpi', 'Test must be done with 4 node')
  end select

  call HistoryCreate(file='xhisttest_mpi/xhisttest_mpi12.nc',       &
    & title = 'gtool_history MPI test 12',                        &
    & source = 'gtool5/Fortran library test kit',        &
    & institution = 'GFD Dennou Club',                     &
    & dims = (/'lon','lat','stl', 't  '/),                        &
    & dimsizes = dimsizes1,                                &
    & longnames = (/'longitude','latitude ','str len  ','time     '/), &
    & units = (/'deg.','deg.','none','sec.'/), &
    & flag_mpi_split = .true.  )

  call HistoryPut( 'lon', axis1 )
  call HistoryPut( 'lat', axis2  )
  call HistoryPut( 'stl', (/(real(i), i = 1, 32)/) )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='U', dims=(/'lon','lat','t  '/)         , &
    & longname='eastward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='V', dims=(/'lon','t  '/)         , &
    & longname='northward wind', units='m s-1', xtype='double' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='StrR', dims=(/'stl','t  '/)         , &
    & longname='real', units='none', xtype='float' )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='Str', dims=(/'stl','t  '/)         , &
    & longname='string', units='none', xtype='char' )

  allocate( xy_U(2,3) )
  allocate( x_V(2) )
  do j = 1, 3
    do i = 1, 2
      xy_U(i,j) = axis1(i) * 100.0 + axis2(j) * 1.0
      x_V(i) = axis1(i) * 100.0
    end do
  end do

  do i = 1, 3
    call HistoryPut('U', xy_U * i)
    call HistoryPut('V', x_V * i)
    call HistoryPut('StrR', (/(real(j) * i, j = 1, 32)/))
    char = 'loop=' // toChar(i)
    call HistoryPut('Str', char)
  enddo

  call HistoryClose
  deallocate( dimsizes1, axis1, axis2 )
  deallocate( xy_U, x_V )


  !-----------------------------------------------------------------
  !  MPI 終了処理
  !-----------------------------------------------------------------
  call MPI_Finalize(err_mpi)

end program histtest_mpi
