!= gtool_historyauto  (旧版: dc_date 使用) のテストプログラム (MPI 版)
!= Test program of "gtool_historyauto" with MPI (old version: using dc_date)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histauto_mpi.f90,v 1.6 2009-10-12 05:11:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2008. All rights reserved. 
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!
! このプログラムは gtool_historyauto が時刻と暦の管理に dc_date を
! 用いていたころの版の後方互換のために維持されています. 
!
! 最新版の維持については histauto2_mpi.f90 を参照ください. 
!

program histauto_mpi
  use dc_trace, only: SetDebug
  use dc_types, only: DP, STDERR
  use dc_test, only: AssertEqual
  use dc_date_types, only: DC_DIFFTIME, DC_DATETIME
  use dc_date, only: DCDateTimeCreate, DCDiffTimeCreate, operator(+), EvalSec
  use dc_message, only: MessageNotify
  use gtool_historyauto, only: HistoryAutoCreate, &
    & HistoryAutoAddAttr, HistoryAutoAddWeight, HistoryAutoPutAxis, &
    & HistoryAutoClose, HistoryAutoPut, HistoryAutoAllVarFix, &
    & HistoryAutoAddVariable, HistoryAutoPutAxisMPI
  use mpi
  implicit none
  integer:: err_mpi, myrank_mpi, nprocs_mpi

  integer:: i, j, k
  logical:: err
!  real, allocatable:: u(:,:,:)
  real(DP):: secd
  integer, allocatable:: dimsizes1(:)
  real(DP), allocatable:: axis1(:), axis2(:), axis3(:)
  real(DP), allocatable:: axis1_all(:), axis2_all(:), axis3_all(:)
  real(DP), allocatable:: xyz_U(:,:,:)
  character(*), parameter:: source = 'histauto.f90'
  character(*), parameter:: institution = 'gtool project'
  type(DC_DIFFTIME):: time1, time2, time3
  type(DC_DIFFTIME):: deltime1
  type(DC_DIFFTIME):: endtime1, inttime1
  type(DC_DIFFTIME), allocatable:: deltime2(:)
  type(DC_DATETIME):: date1

continue

  ! MPI 初期設定
  ! Initialize MPI
  !
  call MPI_Init(err_mpi)
  call MPI_Comm_Rank(MPI_COMM_WORLD, myrank_mpi, err_mpi)
  call MPI_Comm_Size(MPI_COMM_WORLD, nprocs_mpi, err_mpi)

  if ( nprocs_mpi /= 4 ) then
    call MessageNotify( 'E', 'histauto_mpi', 'Test must be done with 4 node')
  end if

  call SetDebug

!  goto 1500

100 continue
  ! 統合出力テスト
  ! Integrated output test
  !
  allocate( dimsizes1(1:4), axis1(1:2), axis2(1:2), axis3(1:2) )
  allocate( axis1_all(1:2), axis2_all(1:4), axis3_all(1:4) )
  allocate( xyz_U(1:2,1:2,1:2) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/ 45.0,  90.0/)
    axis3(:) = (/  0.0, 100.0/)
  case(1) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/ 45.0,  90.0/)
    axis3(:) = (/200.0, 300.0/)
  case(2) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/135.0, 180.0/)
    axis3(:) = (/  0.0, 100.0/)
  case(3) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/135.0, 180.0/)
    axis3(:) = (/200.0, 300.0/)
  case default
    call MessageNotify( 'E', 'histauto_mpi', 'Test must be done with 4 node')
  end select
  axis1_all(1:2) = (/-10.0,  10.0/)
  axis2_all(1:4) = (/ 45.0,  90.0, 135.0, 180.0/)
  axis3_all(1:4) = (/  0.0, 100.0, 200.0, 300.0/)

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 0 )                       ! (in)
  call DCDiffTimeCreate( inttime1, &  ! (out)
    & hour = 1 )                      ! (in)
  call DCDiffTimeCreate( endtime1, &  ! (out)
    & hour = 3 )                      ! (in)

  call HistoryAutoCreate( &
    &       title = 'Test01 for gtool_historyauto MPI', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = dimsizes1, &                            ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm   ', 'm   ', 'deg.', 'min ' /), & ! (in)
    &    xtypes = (/ 'double' /), &                       ! (in) optional
    &     interval = inttime1, &                          ! (in) optional
    &       origin = time1, &                             ! (in) optional
    &     terminus = endtime1, &                          ! (in) optional
    &  file_prefix = 'xhistauto_mpi/test01-', &           ! (in) optional
    & flag_mpi_gather = .true. )                          ! (in) optional

  call DCDiffTimeCreate( deltime1, &  ! (out)
    & min = 10 )                      ! (in)

  call HistoryAutoPutAxis( 'x', axis1 )  ! (in)
  call HistoryAutoPutAxis( 'y', axis2 )  ! (in)
  call HistoryAutoPutAxis( 'z', axis3 )  ! (in)

  call HistoryAutoPutAxisMPI( 'x', axis1_all ) ! (in)
  call HistoryAutoPutAxisMPI( 'y', axis2_all ) ! (in)
  call HistoryAutoPutAxisMPI( 'z', axis3_all ) ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 1.5, 2.5 /) * myrank_mpi )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'z', &                            ! (in)
    & weight = (/ 10.0, 25.0 /) * myrank_mpi, & ! (in)
    & units = 'deg.*2' , xtype = 'double' )   ! (in) optional

  call HistoryAutoAddAttr( &
    & varname = 'x', attrname = 'test_attr', &   ! (in)
    & value = 'test_attr_value' )                ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'y   ', 'z   ', 'time'/), 'u', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'y   ', 'z   ', 'time'/), 'v', 'm s-1', & ! (in)
    & time_average = .true. )                                  ! (in) optional

  call HistoryAutoAllVarFix

  do k = 1, 2
    do j = 1, 2
      do i = 1, 2
        xyz_U(i,j,k) = axis1(i) * 1.0 + axis2(j) * 100.0 + axis3(k) * 1000.0
      end do
    end do
  end do

  do i = 0, 30
    call HistoryAutoPut( time1, 'u', xyz_U + i * 2 )
    call HistoryAutoPut( time1, 'v', xyz_U + i * 2 )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose

  deallocate( dimsizes1, axis1, axis2, axis3 )
  deallocate( axis1_all, axis2_all, axis3_all )
  deallocate( xyz_U )


200 continue
  ! 分散出力テスト
  ! Discrete output test
  !
  allocate( dimsizes1(1:4), axis1(1:2), axis2(1:2), axis3(1:2) )
  allocate( axis1_all(1:2), axis2_all(1:4), axis3_all(1:4) )
  allocate( xyz_U(1:2,1:2,1:2) )
  select case( myrank_mpi )
  case(0) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/ 45.0,  90.0/)
    axis3(:) = (/  0.0, 100.0/)
  case(1) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/ 45.0,  90.0/)
    axis3(:) = (/200.0, 300.0/)
  case(2) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/135.0, 180.0/)
    axis3(:) = (/  0.0, 100.0/)
  case(3) ; dimsizes1(:) = (/2,2,2,0/)
    axis1(:) = (/-10.0,  10.0/)
    axis2(:) = (/135.0, 180.0/)
    axis3(:) = (/200.0, 300.0/)
  case default
    call MessageNotify( 'E', 'histauto_mpi', 'Test must be done with 4 node')
  end select
  axis1_all(1:2) = (/-10.0,  10.0/)
  axis2_all(1:4) = (/ 45.0,  90.0, 135.0, 180.0/)
  axis3_all(1:4) = (/  0.0, 100.0, 200.0, 300.0/)

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 0 )                       ! (in)
  call DCDiffTimeCreate( inttime1, &  ! (out)
    & hour = 1 )                      ! (in)
  call DCDiffTimeCreate( endtime1, &  ! (out)
    & hour = 3 )                      ! (in)

  call HistoryAutoCreate( &
    &       title = 'Test02 for gtool_historyauto MPI', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = dimsizes1, &                            ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm   ', 'm   ', 'deg.', 'min ' /), & ! (in)
    &    xtypes = (/ 'double' /), &                       ! (in) optional
    &     interval = inttime1, &                          ! (in) optional
    &       origin = time1, &                             ! (in) optional
    &     terminus = endtime1, &                          ! (in) optional
    &  file_prefix = 'xhistauto_mpi/test02-', &           ! (in) optional
    & flag_mpi_split = .true. )                           ! (in) optional

  call DCDiffTimeCreate( deltime1, &  ! (out)
    & min = 10 )                      ! (in)

  call HistoryAutoPutAxis( 'x', axis1 )  ! (in)
  call HistoryAutoPutAxis( 'y', axis2 )  ! (in)
  call HistoryAutoPutAxis( 'z', axis3 )  ! (in)

  call HistoryAutoPutAxisMPI( 'x', axis1_all ) ! (in)
  call HistoryAutoPutAxisMPI( 'y', axis2_all ) ! (in)
  call HistoryAutoPutAxisMPI( 'z', axis3_all ) ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 1.5, 2.5 /) * myrank_mpi )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'z', &                            ! (in)
    & weight = (/ 10.0, 25.0 /) * myrank_mpi, & ! (in)
    & units = 'deg.*2' , xtype = 'double' )   ! (in) optional

  call HistoryAutoAddAttr( &
    & varname = 'x', attrname = 'test_attr', &   ! (in)
    & value = 'test_attr_value' )                ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'y   ', 'z   ', 'time'/), 'u', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'y   ', 'z   ', 'time'/), 'v', 'm s-1', & ! (in)
    & time_average = .true. )                                  ! (in) optional

  call HistoryAutoAllVarFix

  do k = 1, 2
    do j = 1, 2
      do i = 1, 2
        xyz_U(i,j,k) = axis1(i) * 1.0 + axis2(j) * 100.0 + axis3(k) * 1000.0
      end do
    end do
  end do

  do i = 0, 30
    call HistoryAutoPut( time1, 'u', xyz_U + i * 2 )
    call HistoryAutoPut( time1, 'v', xyz_U + i * 2 )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


!!$400 continue
!!$  ! NAMELIST を用いた, 出力設定テスト 1
!!$  ! Output settings test 1 with NAMELIST
!!$  !
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 1 )                       ! (in)
!!$  call DCDiffTimeCreate( endtime1, &  ! (out)
!!$    & hour = -1 )                     ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 30.0_DP )                 ! (in)
!!$  call DCDiffTimeCreate( inttime1, &  ! (out)
!!$    & hour = 1 )                      ! (in)
!!$
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test04 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'hrs'  /), & ! (in)
!!$    & namelist_filename = 'histauto_nml04.nml', &         ! (in) optional
!!$    &     interval = inttime1, &                          ! (in) optional
!!$    &       origin = time1, &                             ! (in) optional
!!$    &     terminus = endtime1 )                           ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u1', (/'x   ', 'time'/), 'u1', 'm s-1' ) ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u2', (/'y   ', 'time'/), 'u2', 'm s-1' ) ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u3', (/'z   ', 'time'/), 'u3', 'm s-1' ) ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'y   ', 'time'/), 'v', 'm s-1', & ! (in)
!!$    & time_average = .true. )                  ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'z   ', 'time'/), 'z', 'm s-1', & ! (in)
!!$    & xtype = 'double' )                       ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 41
!!$    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 2 ) /) * i )
!!$    call HistoryAutoPut( time1, 'u2', (/ ( real(j), j = 1, 3 ) /) * i )
!!$    call HistoryAutoPut( time1, 'u3', (/ ( real(j), j = 1, 4 ) /) * i )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) * i )
!!$    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$500 continue
!!$  ! 空間切り出しテスト 1 (NAMELIST 使用せず)
!!$  ! Slices of spaces test 1 (without NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test05 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                         ! (in)
!!$    & institution = institution, &                    ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test05-', &               ! (in) optional
!!$    & slice_start  = (/  1,  1, 3 /), &                   ! (in) optional
!!$    & slice_end    = (/ -1, -1, 6 /), &                   ! (in) optional
!!$    & slice_stride = (/  1,  2, 1 /) )                    ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & sec = 0.0_DP )                  ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 1.0_DP )                  ! (in)
!!$
!!$  call HistoryAutoPutAxis( &
!!$    & dim = 'y', array = (/ -10.0, -2.0, 0.0, 3.0, 6.0, 10.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddWeight( &
!!$    & dim = 'y', weight = (/ 4.0, 5.0, 2.5, 3.0, 3.5, 2.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'y   ', 'time'/), 'v', 'm s-1', & ! (in)
!!$    & time_average = .true. )                          ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'y   ', 'z   ', 'time'/), 'z', 'm s-1', & ! (in)
!!$    & xtype = 'double' )                               ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'ns', (/'x   ', 'y   ', 'z   ', 'time'/), & ! (in)
!!$    & 'no slice', 'm s-1', &                      ! (in)
!!$    & slice_start  = (/  1,  1,  1 /), &          ! (in) optional
!!$    & slice_end    = (/ -1, -1, -1 /), &          ! (in) optional
!!$    & slice_stride = (/  1,  1,  1 /) )           ! (in) optional
!!$
!!$! エラーチェック用
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'u1', (/'x   ', 'time'/), 'u1', 'm s-1', &  ! (in)
!!$!    & file = 'xhistauto_mpi/test05-u.nc', &           ! (in) optional
!!$!    & slice_start  = (/  1,  2,  1 /) )           ! (in) optional
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'u2', (/'x   ', 'time'/), 'u1', 'm s-1', &  ! (in)
!!$!    & file = 'xhistauto_mpi/test05-u.nc', &           ! (in) optional
!!$!    & slice_start  = (/  1,  1,  2 /) )           ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 3
!!$    call HistoryAutoPut( time1, 'v', &
!!$      &    reshape( (/ ( real(j), j = 1, 24 ) /) + 100 * i, (/ 4, 6 /) ) )
!!$    call HistoryAutoPut( time1, 'w', &
!!$      &    reshape( (/ ( real(j), j = 1, 48 ) /) + 100 * i, (/ 6, 8 /) ) )
!!$    call HistoryAutoPut( time1, 'ns', &
!!$      &    reshape( (/ ( real(j), j = 1, 192 ) /) + 1000 * i, (/ 4, 6, 8 /) ) )
!!$
!!$! エラーチェック用
!!$!
!!$!    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 4 ) /) * i )
!!$!    call HistoryAutoPut( time1, 'u2', (/ ( real(j), j = 1, 6 ) /) * i )
!!$
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$600 continue
!!$  ! 空間切り出しテスト 2 (NAMELIST 使用)
!!$  ! Slices of spaces test 2 (with NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test06 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test06-', &               ! (in) optional
!!$    & namelist_filename = 'histauto_nml06.nml' )          ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & sec = 0.0_DP )                  ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 1.0_DP )                  ! (in)
!!$
!!$  call HistoryAutoPutAxis( &
!!$    & dim = 'y', array = (/ -10.0, -2.0, 0.0, 3.0, 6.0, 10.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddWeight( &
!!$    & dim = 'y', weight = (/ 4.0, 5.0, 2.5, 3.0, 3.5, 2.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'y   ', 'time'/), 'v', 'm s-1' )  ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'y   ', 'z   ', 'time'/), 'z', 'm s-1' )  ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'ns', (/'x   ', 'y   ', 'z   ', 'time'/), & ! (in)
!!$    & 'no slice', 'm s-1' )                       ! (in)
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 3
!!$    call HistoryAutoPut( time1, 'v', &
!!$      &    reshape( (/ ( real(j), j = 1, 24 ) /) + 100 * i, (/ 4, 6, 1 /) ) )
!!$    call HistoryAutoPut( time1, 'w', &
!!$      &    reshape( (/ ( real(j), j = 1, 48 ) /) + 100 * i, (/ 6, 8 /) ) )
!!$    call HistoryAutoPut( time1, 'ns', &
!!$      &    reshape( (/ ( real(j), j = 1, 192 ) /) + 1000 * i, (/ 4, 6, 8 /) ) )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$700 continue
!!$  ! 空間平均テスト 1 (NAMELIST なし)
!!$  ! Average of spaces test 1 (without NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test07 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test07-', &               ! (in) optional
!!$    & space_average = (/ .false., .false., .false. /) )   ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & sec = 0.0_DP )                  ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 1.0_DP )                  ! (in)
!!$
!!$  call HistoryAutoPutAxis( &
!!$    & dim = 'y', array = (/ -8.0, -4.0, 0.0, 4.0, 6.0, 8.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddWeight( &
!!$    & dim = 'y', weight = (/ 5.0,  4.0, 4.0, 3.0, 2.0, 2.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u1', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u1', 'm s-1', &                  ! (in)
!!$    & file = 'xhistauto_mpi/test07-u.nc', & ! (in) optional
!!$    & space_average = (/ .true. /) )    ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u2', (/'y   ', 'time'/), &              ! (in)
!!$    & 'u2', 'm s-1', &                         ! (in)
!!$    & file = 'xhistauto_mpi/test07-u.nc', &        ! (in) optional
!!$    & space_average = (/ .false., .true. /) )  ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'y   ', 'z   ', 'time'/), &     ! (in)
!!$    & 'v', 'm s-1', &                                ! (in)
!!$    & space_average = (/ .true., .true., .true. /) ) ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w1', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
!!$    & 'w1', 'm s-1', &                               ! (in)
!!$    & file = 'xhistauto_mpi/test07-w.nc', &              ! (in) optional
!!$    & slice_start  = (/  1,  1, 3 /), &              ! (in) optional
!!$    & slice_end    = (/ -1, -1, 6 /), &              ! (in) optional
!!$    & slice_stride = (/  2,  1, 1 /), &              ! (in) optional
!!$    & space_average = (/ .true., .true., .true. /) ) ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w2', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
!!$    & 'w2', 'm s-1', &                               ! (in)
!!$    & file = 'xhistauto_mpi/test07-w.nc', &              ! (in) optional
!!$    & slice_start  = (/  1,  1, 3 /), &              ! (in) optional
!!$    & slice_end    = (/ -1, -1, 6 /), &              ! (in) optional
!!$    & slice_stride = (/  2,  1, 1 /) )               ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 3
!!$    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 4 ) /) * 2 + 100 * i )
!!$    call HistoryAutoPut( time1, 'u2', (/ 2.0, 3.0, 4.0, 6.0, 7.0, 15.0 /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'v', &
!!$      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
!!$    call HistoryAutoPut( time1, 'w1', &
!!$      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
!!$    call HistoryAutoPut( time1, 'w2', &
!!$      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
!!$
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$800 continue
!!$  ! 空間平均テスト 2 (NAMELIST 使用)
!!$  ! Average of spaces test 2 (with NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test08 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test08-', &               ! (in) optional
!!$    & namelist_filename = 'histauto_nml08.nml' )          ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & sec = 0.0_DP )                  ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 1.0_DP )                  ! (in)
!!$
!!$  call HistoryAutoPutAxis( &
!!$    & dim = 'y', array = (/ -8.0, -4.0, 0.0, 4.0, 6.0, 8.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddWeight( &
!!$    & dim = 'y', weight = (/ 5.0,  4.0, 4.0, 3.0, 2.0, 2.0 /) )  ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u1', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u1', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u2', (/'y   ', 'time'/), &       ! (in)
!!$    & 'u2', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'y   ', 'z   ', 'time'/), &     ! (in)
!!$    & 'v', 'm s-1' )                                 ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w1', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
!!$    & 'w1', 'm s-1' )                                ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w2', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
!!$    & 'w2', 'm s-1' )                                ! (in)
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 3
!!$    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 4 ) /) * 2 + 100 * i )
!!$    call HistoryAutoPut( time1, 'u2', (/ 2.0, 3.0, 4.0, 6.0, 7.0, 15.0 /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'v', &
!!$      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
!!$    call HistoryAutoPut( time1, 'w1', &
!!$      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
!!$    call HistoryAutoPut( time1, 'w2', &
!!$      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$900 continue
!!$  ! 出力開始, 終了時刻設定テスト 1 (NAMELIST なし)
!!$  ! Start and stop of output timing setting test 1 (without NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test09 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'min'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test09-', &               ! (in) optional
!!$    &     interval =  2.0, &                              ! (in) optional
!!$    &       origin =  5.0,   terminus = 10.0 )            ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 1 )                       ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 30.0_DP )                 ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'y   ', 'time'/), &       ! (in)
!!$    & 'v', 'm s-1', &                  ! (in)
!!$    & time_average = .true., &         ! (in) optional
!!$    & terminus = 15.0 )                ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'z   ', 'time'/), &       ! (in)
!!$    & 'w', 'm s-1', &                  ! (in)
!!$    & origin = -1.0, terminus = -1.0 ) ! (in) optional
!!$
!!$! エラーチェック用
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'err1', (/'x   ', 'time'/), 'err1', 'm s-1', &  ! (in)
!!$!    & file = 'xhistauto_mpi/test09-err.nc', &             ! (in) optional
!!$!    & origin = 2.0, terminus = 10.0 )                 ! (in) optional
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'err2', (/'x   ', 'time'/), 'err2', 'm s-1', &  ! (in)
!!$!    & file = 'xhistauto_mpi/test09-err.nc', &             ! (in) optional
!!$!    & origin = 2.0, terminus = 9.0 )                  ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 30
!!$    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 50 * ( i + 1 ) )
!!$    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 5 * ( i + 1 ) )
!!$
!!$! エラーチェック用
!!$!
!!$!    call HistoryAutoPut( time1, 'err1', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
!!$!    call HistoryAutoPut( time1, 'err2', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
!!$
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$1000 continue
!!$  ! 出力開始, 終了時刻設定テスト 2 (NAMELIST 使用)
!!$  ! Start and stop of output timing setting test 2 (with NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test10 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'hrs'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test10-', &               ! (in) optional
!!$    & namelist_filename = 'histauto_nml10.nml' )          ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 1 )                       ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 30.0_DP )                 ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'y   ', 'time'/), &       ! (in)
!!$    & 'v', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'z   ', 'time'/), &       ! (in)
!!$    & 'w', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 30
!!$    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 50 * ( i + 1 ) )
!!$    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 5 * ( i + 1 ) )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$1100 continue
!!$  ! ファイル分割出力テスト 1 (NAMELIST なし)
!!$  ! Separately output file test 1 (without NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test11 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                         ! (in)
!!$    & institution = institution, &                    ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'day'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test11-', &               ! (in) optional
!!$    &     interval =  0.25, &                             ! (in) optional
!!$    & newfile_interval = 1 )                              ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & sec = 0.0_DP )                  ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & hour = 1 )                      ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                   ! (in)
!!$
!!$  !
!!$  ! 分割されたファイルの最初のデータは, 平均値にならない. 
!!$  ! (バグと言えばバグだが, とりあえず放置 2008/07/26 morikawa)
!!$  !
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'y   ', 'time'/), &       ! (in)
!!$    & 'v', 'm s-1', &                  ! (in)
!!$    & time_average = .true., &         ! (in) optional
!!$    & interval = 0.5, &                ! (in) optional
!!$    & origin = 1.5, terminus = 3.5 )   ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'z   ', 'time'/), &       ! (in)
!!$    & 'w', 'm s-1', &                  ! (in)
!!$    & interval = 0.5, &                ! (in) optional
!!$    & origin = 1.5, terminus = 3.5 )   ! (in) optional
!!$
!!$!
!!$! エラーチェック用
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'err', (/'z   ', 'time'/), &       ! (in)
!!$!    & 'err', 'm s-1', &                  ! (in)
!!$!    & newfile_interval = 1, &            ! (in) optional
!!$!    & time_units = 'min' )               ! (in) optional
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'err2', (/'z   ', 'time'/), &       ! (in)
!!$!    & 'err2', 'm s-1', &                  ! (in)
!!$!    & file = 'xhistauto_mpi/test11-err.nc' )  ! (in) optional
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'err3', (/'z   ', 'time'/), &       ! (in)
!!$!    & 'err3', 'm s-1', &                  ! (in)
!!$!    & file = 'xhistauto_mpi/test11-err.nc' ) ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 0, 100
!!$    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 100 * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$
!!$1200 continue
!!$  ! ファイル分割出力テスト 2 (NAMELIST 使用)
!!$  ! Separately output file test 2 (with NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test12 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'day'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test12-', &               ! (in) optional
!!$    & namelist_filename = 'histauto_nml12.nml' )          ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & day = 0 )                       ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & day = 1 )                       ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'y   ', 'time'/), &       ! (in)
!!$    & 'v', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'z   ', 'time'/), &       ! (in)
!!$    & 'w', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 0, 100
!!$    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 100 * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$1300 continue
!!$  ! ランクの名称をファイル名に反映するテスト1 (NAMELIST なし)
!!$  ! Reflect of rank name to file name test 1 (with NAMELIST)
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test13 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'm  ',  'm  ',  'min'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test13-', &               ! (in) optional
!!$    &     interval = 10.0, &                              ! (in) optional
!!$    &         rank = '01' )                               ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 0 )                       ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & min = 10 )                      ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'y   ', 'time'/), &       ! (in)
!!$    & 'v', 'm s-1', &                  ! (in)
!!$    & file = 'xhistauto_mpi/test13-v.nc' ) ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'z   ', 'time'/), &       ! (in)
!!$    & 'w', 'm s-1', &                  ! (in)
!!$    & newfile_interval = 100 )         ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 0, 30
!!$    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 100 * i )
!!$    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 100 * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$1400 continue
!!$  ! 7 次元に依存する変数の出力テスト
!!$  ! Output of variables depended on 7 dimensions test
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test14 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'd1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!$    &                'd6  ', 'd7  ', 'd8  ', 'd9  ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 3, 4, 3, 2, 3, 4, 3, 2, 0 /), &   ! (in)
!!$    & longnames = (/ 'd1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!$    &                'd6  ', 'd7  ', 'd8  ', 'd9  ', 'time' /), & ! (in)
!!$    &     units = (/ '1   ', '1   ', '1   ', '1   ', '1   ', &
!!$    &                '1   ', '1   ', '1   ', '1   ', 'sec ' /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test14-' )                ! (in) optional
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & sec = 0.0_DP )                  ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & sec = 1.0_DP )                  ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'d1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!$    &        'd6  ', 'd7  ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                                   ! (in)
!!$
!!$!
!!$! エラーチェック用
!!$!
!!$!  call HistoryAutoAddVariable( &
!!$!    & 'v', (/'d1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!$!    &        'd6  ', 'd7  ', 'd8  ', 'time'/), &       ! (in)
!!$!    & 'v', 'm s-1' )                                   ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'d3  ', 'd4  ', 'd5  ', 'd6  ', 'd7  ', &
!!$    &        'd8  ', 'd9  ', 'time'/), &             ! (in)
!!$    & 'w', 'm s-1', &                                ! (in)
!!$    & space_average = &
!!$    &   (/ .false., .false., .true., .true., .false., .true., .true., .true. /) ) ! (in) optional
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 0, 2
!!$    call HistoryAutoPut( time1, 'u', &
!!$      & reshape( (/ ( real(j), j = 1, 2*3*4*3*2*3*4 ) /), &
!!$      &                            (/ 2,3,4,3,2,3,4 /)     ) + 10000 * i )
!!$    call HistoryAutoPut( time1, 'w', &
!!$      & reshape( (/ ( real(j)*10, j = 1, 4*3*2*3*4*3*2 ) /), &
!!$      &                               (/ 4,3,2,3,4,3,2 /)     ) + 100000 * i )
!!$
!!$!
!!$! エラーチェック用
!!$!
!!$!    call HistoryAutoPut( time1, 'v', &
!!$!      & reshape( (/ ( real(j), j = 1, 2*3*4*3*2*3*4 ) /), &
!!$!      &                            (/ 2,3,4,3,2,3,4 /)     ) + 10000 * i )
!!$
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$1500 continue
!!$  ! 出力開始, 終了時刻設定テスト 2 (引数 + NAMELIST 使用)
!!$  ! Start and stop of output timing setting test 2 (with arguments and NAMELIST)
!!$  !
!!$  call DCDiffTimeCreate( endtime1, &  ! (out)
!!$    & hour = 1 )                      ! (in)
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 10 )                      ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & min = 5 )                       ! (in)
!!$  call DCDiffTimeCreate( inttime1, &  ! (out)
!!$    & min = 5 )                       ! (in)
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test15 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &           ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'min'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test15-01-', &            ! (in) optional
!!$    &       origin =  time1, terminus = endtime1, &       ! (in) optional
!!$    &     interval =  inttime1, &                         ! (in) optional
!!$    &   all_output = .true., &                            ! (in) optional
!!$    & namelist_filename = 'histauto_nml15-01.nml' )       ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                   ! (in)
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 0 )                       ! (in)
!!$  call DCDiffTimeCreate( endtime1, &  ! (out)
!!$    & hour = -1 )                     ! (in)
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'time'/), &       ! (in)
!!$    & 'v', 'm s-1', &                  ! (in)
!!$    & origin = time1, terminus = endtime1 ) ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 0 )                       ! (in)
!!$  do i = 1, 21
!!$    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 2 ) /) + 50 * ( i + 1 ) )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$  call DCDiffTimeCreate( endtime1, &  ! (out)
!!$    & day = 3 )                       ! (in)
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & hour = 24 )                     ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & hour = 6 )                      ! (in)
!!$  call DCDiffTimeCreate( inttime1, &  ! (out)
!!$    & hour = 12 )                     ! (in)
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test15 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), & ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &           ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), & ! (in)
!!$    &     units = (/ 'm  ',  'day'  /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test15-02-', &            ! (in) optional
!!$    &       origin =  time1, terminus = endtime1, &       ! (in) optional
!!$    &     interval =  inttime1, &                         ! (in) optional
!!$    &   all_output = .true., &                            ! (in) optional
!!$    & namelist_filename = 'histauto_nml15-02.nml' )       ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), &       ! (in)
!!$    & 'u', 'm s-1' )                   ! (in)
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & hour = 0 )                      ! (in)
!!$  call DCDiffTimeCreate( endtime1, &  ! (out)
!!$    & hour = -1 )                     ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'time'/), &       ! (in)
!!$    & 'v', 'm s-1', &                  ! (in)
!!$    & origin = time1, terminus = endtime1 ) ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & min = 0 )                       ! (in)
!!$  do i = 1, 25
!!$    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 10 * ( i - 1 ) )
!!$    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 2 ) /) + 10 * ( i - 1 ) )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$1600 continue
!!$  ! 時刻を入れ替えての出力テスト
!!$  ! Output test with switch of time
!!$  !
!!$  call DCDiffTimeCreate( time1, & ! (out)
!!$    & min = 0 )                   ! (in)
!!$  call DCDiffTimeCreate( time2, & ! (out)
!!$    & min = 1 )                   ! (in)
!!$  call DCDiffTimeCreate( time3, & ! (out)
!!$    & min = 2 )                   ! (in)
!!$  call DCDiffTimeCreate( deltime1, & ! (out)
!!$    & min = 1 )                      ! (in)
!!$  call DCDiffTimeCreate( inttime1, & ! (out)
!!$    & min = 10 )                     ! (in)
!!$  call DCDiffTimeCreate( endtime1, & ! (out)
!!$    & min = 35 )                     ! (in)
!!$
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test16 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), &  ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), &  ! (in)
!!$    &     units = (/ 'm  ', 'min'  /), & ! (in)
!!$    &       origin =  time3, terminus = endtime1, &       ! (in) optional
!!$    &     interval =  inttime1, &                         ! (in) optional
!!$    &  file_prefix = 'xhistauto_mpi/test16-' )                ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'time'/), 'v', 'm s-1', & ! (in)
!!$    & time_average = .true., xtype = 'double' )        ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'x   ', 'time'/), 'w', 'm s-1' )  ! (in)
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 1, 31
!!$    call HistoryAutoPut( time1, 'u', (/ 10.0, 20.0 /) * i )
!!$    call HistoryAutoPut( time2, 'v', (/ 10.0, 20.0 /) * i  )
!!$    call HistoryAutoPut( time3, 'w', (/ 10.0, 20.0 /) * i  )
!!$    time1 = time1 + deltime1
!!$    time2 = time2 + deltime1
!!$    time3 = time3 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$
!!$1700 continue
!!$  ! 時間の次元に "since 20XX-XX-XX .." といった情報を与えるテスト
!!$  ! Test for check that "since 20XX-XX-XX .." is given to unit of time
!!$  !
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test17 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), &  ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), &  ! (in)
!!$    &     units = (/ 'm                                  ', &
!!$    &                'day since 2008-10-05T00:00:00+00:00' /), & ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test17-' )                ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  call DCDiffTimeCreate( time1, &     ! (out)
!!$    & day = 0 )                       ! (in)
!!$  call DCDiffTimeCreate( deltime1, &  ! (out)
!!$    & day = 1 )                       ! (in)
!!$
!!$  do i = 1, 3
!!$    call HistoryAutoPut( time1, 'u', (/ 10.0, 20.0 /) * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$1800 continue
!!$  ! Δt と出力間隔が一致しない場合のテスト
!!$  ! Test for output interval is not equal to delta t
!!$  !
!!$  call DCDiffTimeCreate( time1, & ! (out)
!!$    & min = 0 )                   ! (in)
!!$
!!$  allocate( deltime2(1:3) )
!!$  call DCDiffTimeCreate( deltime2(1), & ! (out)
!!$    & sec = 20.0_DP )                   ! (in)
!!$  call DCDiffTimeCreate( deltime2(2), & ! (out)
!!$    & sec = 30.0_DP )                   ! (in)
!!$  call DCDiffTimeCreate( deltime2(3), & ! (out)
!!$    & sec = 40.0_DP )                   ! (in)
!!$
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test18 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), &  ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), &  ! (in)
!!$    &     units = (/ 'm  ', 'min'  /), & ! (in)
!!$    &     interval = 1.0, &                         ! (in) optional
!!$    &  file_prefix = 'xhistauto_mpi/test18-' )          ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), 'u', 'm s-1', &  ! (in)
!!$    & interval = 60.0, time_units = 'sec' )     ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'v', (/'x   ', 'time'/), 'v', 'm s-1', &  ! (in)
!!$    & interval = 100.0, time_units = 'sec' )    ! (in)
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'w', (/'x   ', 'time'/), 'w', 'm s-1',&   ! (in)
!!$    & interval = 30.0, time_units = 'sec', &    ! (in)
!!$    & time_average = .true., xtype = 'double' ) ! (in) optional
!!$
!!$  call HistoryAutoAllVarFix
!!$
!!$  do i = 0, 10
!!$    call HistoryAutoPut( time1, 'u', (/ 10.0, 20.0 /) * i )
!!$    call HistoryAutoPut( time1, 'v', (/ 10.0, 20.0 /) * i )
!!$    call HistoryAutoPut( time1, 'w', (/ 10.0, 20.0 /) * i )
!!$    time1 = time1 + deltime2( mod(i,3) + 1 )
!!$  end do
!!$
!!$  call HistoryAutoClose
!!$
!!$  deallocate( deltime2 )
!!$
!!$
!!$1900 continue
!!$  ! 日時設定テスト
!!$  ! Date configuration test
!!$  !
!!$  call DCDiffTimeCreate( time1, & ! (out)
!!$    & sec = 0.0_DP )              ! (in)
!!$  call DCDiffTimeCreate( deltime1, & ! (out)
!!$    & sec = 1.0_DP )                 ! (in)
!!$
!!$  call DCDateTimeCreate( date1, &                  ! (out)
!!$    & year = 2008, mon = 10, day = 10, hour = 12 ) ! (in)
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test19 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), &           ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), &           ! (in)
!!$    &     units = (/ 'm  ', 'sec'  /), &            ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test19-none-', &    ! (in) optional
!!$    &  origin_date = date1, &                       ! (in) optional
!!$    &  origin_date_invalid = .true. )               ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
!!$  call HistoryAutoAllVarFix
!!$  do i = 1, 2
!!$    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$  call HistoryAutoClose
!!$
!!$
!!$  call DCDateTimeCreate( date1, &                  ! (out)
!!$    & year = 2008, mon = 10, day = 10, hour = 12 ) ! (in)
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test19 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), &           ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), &           ! (in)
!!$    &     units = (/ 'm  ', 'sec'  /), &            ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test19-gregorian-', & ! (in) optional
!!$    &  origin_date = date1 )                          ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
!!$  call HistoryAutoAllVarFix
!!$  do i = 1, 2
!!$    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$  call HistoryAutoClose
!!$
!!$
!!$  call DCDateTimeCreate( date1, &                   ! (out)
!!$    & year = 2008, mon = 10, day = 10, hour = 12, & ! (in)
!!$    & caltype_str = 'noleap' )                      ! (in)
!!$  call HistoryAutoCreate( &
!!$    &       title = 'Test19 for gtool_historyauto', & ! (in)
!!$    &      source = source, &                       ! (in)
!!$    & institution = institution, &                  ! (in)
!!$    &      dims = (/ 'x   ', 'time' /), &           ! (in)
!!$    &  dimsizes = (/ 2, 0 /), &                     ! (in)
!!$    & longnames = (/ 'x   ', 'time' /), &           ! (in)
!!$    &     units = (/ 'm  ', 'sec'  /), &            ! (in)
!!$    &  file_prefix = 'xhistauto_mpi/test19-noleap-', &  ! (in) optional
!!$    &  origin_date = date1, &                       ! (in) optional
!!$    &  origin_date_invalid = .false. )              ! (in) optional
!!$
!!$  call HistoryAutoAddVariable( &
!!$    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
!!$  call HistoryAutoAllVarFix
!!$  do i = 1, 2
!!$    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
!!$    time1 = time1 + deltime1
!!$  end do
!!$  call HistoryAutoClose



  ! MPI 終了処理
  ! Finalize MPI
  !
  call MPI_Finalize(err_mpi)

end program histauto_mpi
