!= gtool_historyauto (旧版: dc_date 使用) のテストプログラム
!= Test program of "gtool_historyauto" (old version: using dc_date)
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histauto.f90,v 1.10 2009-10-11 03:55:56 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2008. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
!
! このプログラムは gtool_historyauto が時刻と暦の管理に dc_date を
! 用いていたころの版の後方互換のために維持されています.
!
! 最新版の維持については histauto2.f90 を参照ください.
!

program histauto
  use dc_trace, only: SetDebug
  use dc_types, only: DP, STDERR
  use dc_test, only: AssertEqual
  use dc_date_types, only: DC_DIFFTIME, DC_DATETIME
  use dc_date, only: DCDateTimeCreate, DCDiffTimeCreate, operator(+), EvalSec
  use dc_calendar, only: DC_CAL, DC_CAL_DATE, DCCalCreate, DCCalDateCreate
  use gtool_historyauto, only: HistoryAutoCreate, &
    & HistoryAutoAddAttr, HistoryAutoAddWeight, HistoryAutoPutAxis, &
    & HistoryAutoClose, HistoryAutoPut, HistoryAutoAllVarFix, &
    & HistoryAutoAddVariable
  implicit none

  integer:: i, j
  logical:: err
!  real, allocatable:: u(:,:,:)
  real(DP):: secd
  character(*), parameter:: source = 'histauto.f90'
  character(*), parameter:: institution = 'gtool4 project'
  type(DC_DIFFTIME):: time1, time2, time3
  type(DC_DIFFTIME):: deltime1
  type(DC_DIFFTIME):: endtime1, inttime1
  type(DC_DIFFTIME), allocatable:: deltime2(:)
  type(DC_DATETIME):: date1
  type(DC_CAL):: cal1
  type(DC_CAL_DATE):: caldate1
  real(DP):: esec1
  real(DP):: timed1, timed2, timed3, deltimed1

continue

  call SetDebug

!  goto 2200

100 continue
  ! 基本テスト
  ! Basic test
  !
  call HistoryAutoCreate( &
    &       title = 'Test01 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /) )  ! (in)

  call DCDiffTimeCreate( time1, & ! (out)
    & sec = 0.0_DP )              ! (in)
  call DCDiffTimeCreate( deltime1, & ! (out)
    & sec = 1.0_DP )                 ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1', & ! (in)
    & file = 'xhistauto/test01-u.nc' )         ! (in) optional

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'z   ', 'time'/), 'v', 'm s-1', & ! (in)
    & file = 'xhistauto/test01-v.nc', &                ! (in) optional
    & time_average = .true., xtype = 'double' )        ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w1', (/'y   ', 'z   '/), 'w1', 'm s-1', &         ! (in)
    & file = 'xhistauto/test01-w.nc' )                   ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w2', (/'time', 'y   ', 'z   '/), 'w2', 'm s-1', & ! (in)
    & file = 'xhistauto/test01-w.nc' )                   ! (in) optional

  call HistoryAutoAllVarFix

  do i = 1, 10
    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
    call HistoryAutoPut( time1, 'v', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    call HistoryAutoPut( time1, 'w1', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    call HistoryAutoPut( time1, 'w2', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    time1 = time1 + deltime1
  end do

  call HistoryAutoPut( time1, 'w', (/ 1.0, 2.0 /), err = err )
  call AssertEqual( 'HistoryAutoPut error handling test 1-1', &
    & answer = .true., check = err )

  call HistoryAutoClose

200 continue
  ! 出力タイミング自動チェックテスト
  ! Output timing automatically checking test
  !
  call HistoryAutoCreate( &
    &       title = 'Test02 for gtool_historyauto', &     ! (in)
    &      source = source, &                             ! (in)
    & institution = institution, &                        ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'min'  /), & ! (in)
    &  file_prefix = 'xhistauto/test02-', &               ! (in) optional
    &       origin = 0.5, &                               ! (in) optional
    &     interval = 1.0 )                                ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & sec = 30.0_DP )                 ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 10.0_DP )                 ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'z   ', 'time'/), 'v', 'm s-1', & ! (in)
    & time_average = .true., xtype = 'double' )        ! (in) optional

  call HistoryAutoAllVarFix

  do i = 1, 19
!    write(*,*) i, (i - 1) * 10 + 30, ((i - 1) * 10 + 30) / 60.0
    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
    call HistoryAutoPut( time1, 'v', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
!    write(*,*) '---------------'
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose

300 continue
  ! 座標データ, 座標重み, 属性設定テスト
  ! Axes data, axes weights, attributes settings test
  !
  call HistoryAutoCreate( &
    &       title = 'Test03 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm   ', 'm   ', 'deg.', 'sec ' /), & ! (in)
    &    xtypes = (/ 'double' /), &                       ! (in) optional
    &  file_prefix = 'xhistauto/test03-' )                ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & sec = 0.0_DP )                  ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 1.0_DP )                  ! (in)

  call HistoryAutoPutAxis( &
    & dim = 'y', array = (/ -2.0, 0.0, 3.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 1.5, 2.5, 3.5 /) )  ! (in)

  call HistoryAutoPutAxis( &
    & dim = 'z', array = (/ 0.0, 20.0, 50.0, 90.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'z', &                                   ! (in)
    & weight = (/ 10.0, 25.0, 45.0, 20.0 /) * 2.0, & ! (in)
    & units = 'deg.*2' , xtype = 'double' )          ! (in) optional

  call HistoryAutoAddAttr( &
    & varname = 'x', attrname = 'test_attr', &   ! (in)
    & value = 'test_attr_value' )                ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'y   ', 'z   ', 'time'/), 'u', 'm s-1' ) ! (in)

  call HistoryAutoAllVarFix

  do i = 1, 3
    call HistoryAutoPut( time1, 'u', &
      & reshape( (/ ( real(j), j = 1, 24 ) /) * i, (/ 2, 3, 4 /) ) )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose

400 continue
  ! NAMELIST を用いた, 出力設定テスト 1
  ! Output settings test 1 with NAMELIST
  !
  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 1 )                       ! (in)
  call DCDiffTimeCreate( endtime1, &  ! (out)
    & hour = -1 )                     ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 30.0_DP )                 ! (in)
  call DCDiffTimeCreate( inttime1, &  ! (out)
    & hour = 1 )                      ! (in)

  call HistoryAutoCreate( &
    &       title = 'Test04 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'hrs'  /), & ! (in)
    & namelist_filename = 'histauto_nml04.nml', &         ! (in) optional
    &     interval = inttime1, &                          ! (in) optional
    &       origin = time1, &                             ! (in) optional
    &     terminus = endtime1 )                           ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u1', (/'x   ', 'time'/), 'u1', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'u2', (/'y   ', 'time'/), 'u2', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'u3', (/'z   ', 'time'/), 'u3', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'time'/), 'v', 'm s-1', & ! (in)
    & time_average = .true. )                  ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w', (/'z   ', 'time'/), 'z', 'm s-1', & ! (in)
    & xtype = 'double' )                       ! (in) optional

  call HistoryAutoAllVarFix

  do i = 1, 41
    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 2 ) /) * i )
    call HistoryAutoPut( time1, 'u2', (/ ( real(j), j = 1, 3 ) /) * i )
    call HistoryAutoPut( time1, 'u3', (/ ( real(j), j = 1, 4 ) /) * i )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) * i )
    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) * i )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


500 continue
  ! 空間切り出しテスト 1 (NAMELIST 使用せず)
  ! Slices of spaces test 1 (without NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test05 for gtool_historyauto', & ! (in)
    &      source = source, &                         ! (in)
    & institution = institution, &                    ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
    &  file_prefix = 'xhistauto/test05-', &               ! (in) optional
    & slice_start  = (/  1,  1, 3 /), &                   ! (in) optional
    & slice_end    = (/ -1, -1, 6 /), &                   ! (in) optional
    & slice_stride = (/  1,  2, 1 /) )                    ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & sec = 0.0_DP )                  ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 1.0_DP )                  ! (in)

  call HistoryAutoPutAxis( &
    & dim = 'y', array = (/ -10.0, -2.0, 0.0, 3.0, 6.0, 10.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 4.0, 5.0, 2.5, 3.0, 3.5, 2.0 /) )  ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'y   ', 'time'/), 'v', 'm s-1', & ! (in)
    & time_average = .true. )                          ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w', (/'y   ', 'z   ', 'time'/), 'z', 'm s-1', & ! (in)
    & xtype = 'double' )                               ! (in) optional

  call HistoryAutoAddVariable( &
    & 'ns', (/'x   ', 'y   ', 'z   ', 'time'/), & ! (in)
    & 'no slice', 'm s-1', &                      ! (in)
    & slice_start  = (/  1,  1,  1 /), &          ! (in) optional
    & slice_end    = (/ -1, -1, -1 /), &          ! (in) optional
    & slice_stride = (/  1,  1,  1 /) )           ! (in) optional

! エラーチェック用
!
!  call HistoryAutoAddVariable( &
!    & 'u1', (/'x   ', 'time'/), 'u1', 'm s-1', &  ! (in)
!    & file = 'xhistauto/test05-u.nc', &           ! (in) optional
!    & slice_start  = (/  1,  2,  1 /) )           ! (in) optional
!
!  call HistoryAutoAddVariable( &
!    & 'u2', (/'x   ', 'time'/), 'u1', 'm s-1', &  ! (in)
!    & file = 'xhistauto/test05-u.nc', &           ! (in) optional
!    & slice_start  = (/  1,  1,  2 /) )           ! (in) optional

  call HistoryAutoAllVarFix

  do i = 1, 3
    call HistoryAutoPut( time1, 'v', &
      &    reshape( (/ ( real(j), j = 1, 24 ) /) + 100 * i, (/ 4, 6 /) ) )
    call HistoryAutoPut( time1, 'w', &
      &    reshape( (/ ( real(j), j = 1, 48 ) /) + 100 * i, (/ 6, 8 /) ) )
    call HistoryAutoPut( time1, 'ns', &
      &    reshape( (/ ( real(j), j = 1, 192 ) /) + 1000 * i, (/ 4, 6, 8 /) ) )

! エラーチェック用
!
!    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 4 ) /) * i )
!    call HistoryAutoPut( time1, 'u2', (/ ( real(j), j = 1, 6 ) /) * i )

    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


600 continue
  ! 空間切り出しテスト 2 (NAMELIST 使用)
  ! Slices of spaces test 2 (with NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test06 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
    &  file_prefix = 'xhistauto/test06-', &               ! (in) optional
    & namelist_filename = 'histauto_nml06.nml' )          ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & sec = 0.0_DP )                  ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 1.0_DP )                  ! (in)

  call HistoryAutoPutAxis( &
    & dim = 'y', array = (/ -10.0, -2.0, 0.0, 3.0, 6.0, 10.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 4.0, 5.0, 2.5, 3.0, 3.5, 2.0 /) )  ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'y   ', 'time'/), 'v', 'm s-1' )  ! (in)

  call HistoryAutoAddVariable( &
    & 'w', (/'y   ', 'z   ', 'time'/), 'z', 'm s-1' )  ! (in)

  call HistoryAutoAddVariable( &
    & 'ns', (/'x   ', 'y   ', 'z   ', 'time'/), & ! (in)
    & 'no slice', 'm s-1' )                       ! (in)

  call HistoryAutoAllVarFix

  do i = 1, 3
    call HistoryAutoPut( time1, 'v', &
      &    reshape( (/ ( real(j), j = 1, 24 ) /) + 100 * i, (/ 4, 6, 1 /) ) )
    call HistoryAutoPut( time1, 'w', &
      &    reshape( (/ ( real(j), j = 1, 48 ) /) + 100 * i, (/ 6, 8 /) ) )
    call HistoryAutoPut( time1, 'ns', &
      &    reshape( (/ ( real(j), j = 1, 192 ) /) + 1000 * i, (/ 4, 6, 8 /) ) )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


700 continue
  ! 空間平均テスト 1 (NAMELIST なし)
  ! Average of spaces test 1 (without NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test07 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
    &  file_prefix = 'xhistauto/test07-', &               ! (in) optional
    & space_average = (/ .false., .false., .false. /) )   ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & sec = 0.0_DP )                  ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 1.0_DP )                  ! (in)

  call HistoryAutoPutAxis( &
    & dim = 'y', array = (/ -8.0, -4.0, 0.0, 4.0, 6.0, 8.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 5.0,  4.0, 4.0, 3.0, 2.0, 2.0 /) )  ! (in)

  call HistoryAutoAddVariable( &
    & 'u1', (/'x   ', 'time'/), &       ! (in)
    & 'u1', 'm s-1', &                  ! (in)
    & file = 'xhistauto/test07-u.nc', & ! (in) optional
    & space_average = (/ .true. /) )    ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u2', (/'y   ', 'time'/), &              ! (in)
    & 'u2', 'm s-1', &                         ! (in)
    & file = 'xhistauto/test07-u.nc', &        ! (in) optional
    & space_average = (/ .false., .true. /) )  ! (in) optional

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'y   ', 'z   ', 'time'/), &     ! (in)
    & 'v', 'm s-1', &                                ! (in)
    & space_average = (/ .true., .true., .true. /) ) ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w1', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
    & 'w1', 'm s-1', &                               ! (in)
    & file = 'xhistauto/test07-w.nc', &              ! (in) optional
    & slice_start  = (/  1,  1, 3 /), &              ! (in) optional
    & slice_end    = (/ -1, -1, 6 /), &              ! (in) optional
    & slice_stride = (/  2,  1, 1 /), &              ! (in) optional
    & space_average = (/ .true., .true., .true. /) ) ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w2', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
    & 'w2', 'm s-1', &                               ! (in)
    & file = 'xhistauto/test07-w.nc', &              ! (in) optional
    & slice_start  = (/  1,  1, 3 /), &              ! (in) optional
    & slice_end    = (/ -1, -1, 6 /), &              ! (in) optional
    & slice_stride = (/  2,  1, 1 /) )               ! (in) optional

  call HistoryAutoAllVarFix

  do i = 1, 3
    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 4 ) /) * 2 + 100 * i )
    call HistoryAutoPut( time1, 'u2', (/ 2.0, 3.0, 4.0, 6.0, 7.0, 15.0 /) + 100 * i )
    call HistoryAutoPut( time1, 'v', &
      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
    call HistoryAutoPut( time1, 'w1', &
      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
    call HistoryAutoPut( time1, 'w2', &
      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )

    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


800 continue
  ! 空間平均テスト 2 (NAMELIST 使用)
  ! Average of spaces test 2 (with NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test08 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 4, 6, 8, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
    &  file_prefix = 'xhistauto/test08-', &               ! (in) optional
    & namelist_filename = 'histauto_nml08.nml' )          ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & sec = 0.0_DP )                  ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 1.0_DP )                  ! (in)

  call HistoryAutoPutAxis( &
    & dim = 'y', array = (/ -8.0, -4.0, 0.0, 4.0, 6.0, 8.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 5.0,  4.0, 4.0, 3.0, 2.0, 2.0 /) )  ! (in)

  call HistoryAutoAddVariable( &
    & 'u1', (/'x   ', 'time'/), &       ! (in)
    & 'u1', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'u2', (/'y   ', 'time'/), &       ! (in)
    & 'u2', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'y   ', 'z   ', 'time'/), &     ! (in)
    & 'v', 'm s-1' )                                 ! (in)

  call HistoryAutoAddVariable( &
    & 'w1', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
    & 'w1', 'm s-1' )                                ! (in)

  call HistoryAutoAddVariable( &
    & 'w2', (/'x   ', 'y   ', 'z   ', 'time'/), &    ! (in)
    & 'w2', 'm s-1' )                                ! (in)

  call HistoryAutoAllVarFix

  do i = 1, 3
    call HistoryAutoPut( time1, 'u1', (/ ( real(j), j = 1, 4 ) /) * 2 + 100 * i )
    call HistoryAutoPut( time1, 'u2', (/ 2.0, 3.0, 4.0, 6.0, 7.0, 15.0 /) + 100 * i )
    call HistoryAutoPut( time1, 'v', &
      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
    call HistoryAutoPut( time1, 'w1', &
      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
    call HistoryAutoPut( time1, 'w2', &
      &    reshape( (/ ( real(j * 10), j = 1, 192 ) /) + 10000 * i, (/ 4, 6, 8 /) ) )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


900 continue
  ! 出力開始, 終了時刻設定テスト 1 (NAMELIST なし)
  ! Start and stop of output timing setting test 1 (without NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test09 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'min'  /), & ! (in)
    &  file_prefix = 'xhistauto/test09-', &               ! (in) optional
    &     interval =  2.0, &                              ! (in) optional
    &       origin =  5.0,   terminus = 10.0 )            ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 1 )                       ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 30.0_DP )                 ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), &       ! (in)
    & 'u', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'time'/), &       ! (in)
    & 'v', 'm s-1', &                  ! (in)
    & time_average = .true., &         ! (in) optional
    & terminus = 15.0_DP )                ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w', (/'z   ', 'time'/), &             ! (in)
    & 'w', 'm s-1', &                        ! (in)
    & origin = -1.0_DP, terminus = -1.0_DP ) ! (in) optional

! エラーチェック用
!
!  call HistoryAutoAddVariable( &
!    & 'err1', (/'x   ', 'time'/), 'err1', 'm s-1', &  ! (in)
!    & file = 'xhistauto/test09-err.nc', &             ! (in) optional
!    & origin = 2.0, terminus = 10.0 )                 ! (in) optional
!
!  call HistoryAutoAddVariable( &
!    & 'err2', (/'x   ', 'time'/), 'err2', 'm s-1', &  ! (in)
!    & file = 'xhistauto/test09-err.nc', &             ! (in) optional
!    & origin = 2.0, terminus = 9.0 )                  ! (in) optional

  call HistoryAutoAllVarFix

  do i = 1, 30
    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 50 * ( i + 1 ) )
    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 5 * ( i + 1 ) )

! エラーチェック用
!
!    call HistoryAutoPut( time1, 'err1', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
!    call HistoryAutoPut( time1, 'err2', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )

    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


1000 continue
  ! 出力開始, 終了時刻設定テスト 2 (NAMELIST 使用)
  ! Start and stop of output timing setting test 2 (with NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test10 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'hrs'  /), & ! (in)
    &  file_prefix = 'xhistauto/test10-', &               ! (in) optional
    & namelist_filename = 'histauto_nml10.nml' )          ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 1 )                       ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & sec = 30.0_DP )                 ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), &       ! (in)
    & 'u', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'time'/), &       ! (in)
    & 'v', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'w', (/'z   ', 'time'/), &       ! (in)
    & 'w', 'm s-1' )                   ! (in)

  call HistoryAutoAllVarFix

  do i = 1, 30
    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 50 * ( i + 1 ) )
    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 5 * ( i + 1 ) )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


1100 continue
  ! ファイル分割出力テスト 1 (NAMELIST なし)
  ! Separately output file test 1 (without NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test11 for gtool_historyauto', & ! (in)
    &      source = source, &                         ! (in)
    & institution = institution, &                    ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'day'  /), & ! (in)
    &  file_prefix = 'xhistauto/test11-', &               ! (in) optional
    &     interval =  0.25, &                             ! (in) optional
    & newfile_interval = 1 )                              ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & sec = 0.0_DP )                  ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & hour = 1 )                      ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), &       ! (in)
    & 'u', 'm s-1' )                   ! (in)

  !
  ! 分割されたファイルの最初のデータは, 平均値にならない.
  ! (バグと言えばバグだが, とりあえず放置 2008/07/26 morikawa)
  !
  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'time'/), &           ! (in)
    & 'v', 'm s-1', &                      ! (in)
    & time_average = .true., &             ! (in) optional
    & interval = 0.5_DP, &                 ! (in) optional
    & origin = 1.5_DP, terminus = 3.5_DP ) ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w', (/'z   ', 'time'/), &           ! (in)
    & 'w', 'm s-1', &                      ! (in)
    & interval = 0.5_DP, &                 ! (in) optional
    & origin = 1.5_DP, terminus = 3.5_DP ) ! (in) optional

!
! エラーチェック用
!
!  call HistoryAutoAddVariable( &
!    & 'err', (/'z   ', 'time'/), &       ! (in)
!    & 'err', 'm s-1', &                  ! (in)
!    & newfile_interval = 1, &            ! (in) optional
!    & time_units = 'min' )               ! (in) optional
!
!  call HistoryAutoAddVariable( &
!    & 'err2', (/'z   ', 'time'/), &       ! (in)
!    & 'err2', 'm s-1', &                  ! (in)
!    & file = 'xhistauto/test11-err.nc' )  ! (in) optional
!
!  call HistoryAutoAddVariable( &
!    & 'err3', (/'z   ', 'time'/), &       ! (in)
!    & 'err3', 'm s-1', &                  ! (in)
!    & file = 'xhistauto/test11-err.nc' ) ! (in) optional

  call HistoryAutoAllVarFix

  do i = 0, 100
    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 100 * i )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 100 * i )
    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 100 * i )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose



1200 continue
  ! ファイル分割出力テスト 2 (NAMELIST 使用)
  ! Separately output file test 2 (with NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test12 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'day'  /), & ! (in)
    &  file_prefix = 'xhistauto/test12-', &               ! (in) optional
    & namelist_filename = 'histauto_nml12.nml' )          ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & day = 0 )                       ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & day = 1 )                       ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), &       ! (in)
    & 'u', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'time'/), &       ! (in)
    & 'v', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'w', (/'z   ', 'time'/), &       ! (in)
    & 'w', 'm s-1' )                   ! (in)

  call HistoryAutoAllVarFix

  do i = 0, 100
    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 100 * i )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 100 * i )
    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 100 * i )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


1300 continue
  ! ランクの名称をファイル名に反映するテスト1 (NAMELIST なし)
  ! Reflect of rank name to file name test 1 (with NAMELIST)
  !
  call HistoryAutoCreate( &
    &       title = 'Test13 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'min'  /), & ! (in)
    &  file_prefix = 'xhistauto/test13-', &               ! (in) optional
    &     interval = 10.0, &                              ! (in) optional
    &         rank = '01' )                               ! (in) optional

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 0 )                       ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & min = 10 )                      ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), &       ! (in)
    & 'u', 'm s-1' )                   ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'time'/), &       ! (in)
    & 'v', 'm s-1', &                  ! (in)
    & file = 'xhistauto/test13-v.nc' ) ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w', (/'z   ', 'time'/), &       ! (in)
    & 'w', 'm s-1', &                  ! (in)
    & newfile_interval = 100 )         ! (in) optional

  call HistoryAutoAllVarFix

  do i = 0, 30
    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 100 * i )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 3 ) /) + 100 * i )
    call HistoryAutoPut( time1, 'w', (/ ( real(j), j = 1, 4 ) /) + 100 * i )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


1400 continue
  ! 7 次元に依存する変数の出力テスト
  ! Output of variables depended on 7 dimensions test
  !
!!  call HistoryAutoCreate( &
!!    &       title = 'Test14 for gtool_historyauto', & ! (in)
!!    &      source = source, &                       ! (in)
!!    & institution = institution, &                  ! (in)
!!    &      dims = (/ 'd1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!    &                'd6  ', 'd7  ', 'd8  ', 'd9  ', 'time' /), & ! (in)
!!    &  dimsizes = (/ 2, 3, 4, 3, 2, 3, 4, 3, 2, 0 /), &   ! (in)
!!    & longnames = (/ 'd1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!    &                'd6  ', 'd7  ', 'd8  ', 'd9  ', 'time' /), & ! (in)
!!    &     units = (/ '1   ', '1   ', '1   ', '1   ', '1   ', &
!!    &                '1   ', '1   ', '1   ', '1   ', 'sec ' /), & ! (in)
!!    &  file_prefix = 'xhistauto/test14-' )                ! (in) optional
!!
!!  call DCDiffTimeCreate( time1, &     ! (out)
!!    & sec = 0.0_DP )                  ! (in)
!!  call DCDiffTimeCreate( deltime1, &  ! (out)
!!    & sec = 1.0_DP )                  ! (in)
!!
!!  call HistoryAutoAddVariable( &
!!    & 'u', (/'d1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!    &        'd6  ', 'd7  ', 'time'/), &       ! (in)
!!    & 'u', 'm s-1' )                           ! (in)
!!

  ! エラーチェック用

!!  call HistoryAutoAddVariable( &
!!    & 'v', (/'d1  ', 'd2  ', 'd3  ', 'd4  ', 'd5  ', &
!!    &        'd6  ', 'd7  ', 'd8  ', 'time'/), &       ! (in)
!!    & 'v', 'm s-1' )                                   ! (in)
!!
!!  call HistoryAutoAddVariable( &
!!    & 'w', (/'d3  ', 'd4  ', 'd5  ', 'd6  ', 'd7  ', &
!!    &        'd8  ', 'd9  ', 'time'/), &             ! (in)
!!    & 'w', 'm s-1', &                                ! (in)
!!    & space_average = &
!!    &   (/ .false., .false., .true., .true., .false., .true., .true., .true. /) ) ! (in) optional
!!  call HistoryAutoAllVarFix
!!
!!  do i = 0, 2
!!    call HistoryAutoPut( time1, 'u', &
!!      & reshape( (/ ( real(j), j = 1, 2*3*4*3*2*3*4 ) /), &
!!      &                            (/ 2,3,4,3,2,3,4 /)     ) + 10000 * i )
!!    call HistoryAutoPut( time1, 'w', &
!!      & reshape( (/ ( real(j)*10, j = 1, 4*3*2*3*4*3*2 ) /), &
!!      &                               (/ 4,3,2,3,4,3,2 /)     ) + 100000 * i )
!!    ! エラーチェック用
!!    call HistoryAutoPut( time1, 'v', &
!!      & reshape( (/ ( real(j), j = 1, 2*3*4*3*2*3*4 ) /), &
!!      &                            (/ 2,3,4,3,2,3,4 /)     ) + 10000 * i )
!!
!!    time1 = time1 + deltime1
!!  end do
!!
!! call HistoryAutoClose


1500 continue
  ! 出力開始, 終了時刻設定テスト 2 (引数 + NAMELIST 使用)
  ! Start and stop of output timing setting test 2 (with arguments and NAMELIST)
  !
  call DCDiffTimeCreate( endtime1, &  ! (out)
    & hour = 1 )                      ! (in)
  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 10 )                      ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & min = 5 )                       ! (in)
  call DCDiffTimeCreate( inttime1, &  ! (out)
    & min = 5 )                       ! (in)
  call HistoryAutoCreate( &
    &       title = 'Test15 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 0 /), &           ! (in)
    & longnames = (/ 'x   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'min'  /), & ! (in)
    &  file_prefix = 'xhistauto/test15-01-', &            ! (in) optional
    &       origin =  time1, terminus = endtime1, &       ! (in) optional
    &     interval =  inttime1, &                         ! (in) optional
    &   all_output = .true., &                            ! (in) optional
    & namelist_filename = 'histauto_nml15-01.nml' )       ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), &       ! (in)
    & 'u', 'm s-1' )                   ! (in)

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 0 )                       ! (in)
  call DCDiffTimeCreate( endtime1, &  ! (out)
    & hour = -1 )                     ! (in)
  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'time'/), &       ! (in)
    & 'v', 'm s-1', &                  ! (in)
    & origin = time1, terminus = endtime1 ) ! (in) optional

  call HistoryAutoAllVarFix

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 0 )                       ! (in)
  do i = 1, 21
    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 5 * ( i + 1 ) )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 2 ) /) + 50 * ( i + 1 ) )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


  call DCDiffTimeCreate( endtime1, &  ! (out)
    & day = 3 )                       ! (in)
  call DCDiffTimeCreate( time1, &     ! (out)
    & hour = 24 )                     ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & hour = 6 )                      ! (in)
  call DCDiffTimeCreate( inttime1, &  ! (out)
    & hour = 12 )                     ! (in)
  call HistoryAutoCreate( &
    &       title = 'Test15 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 0 /), &           ! (in)
    & longnames = (/ 'x   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'day'  /), & ! (in)
    &  file_prefix = 'xhistauto/test15-02-', &            ! (in) optional
    &       origin =  time1, terminus = endtime1, &       ! (in) optional
    &     interval =  inttime1, &                         ! (in) optional
    &   all_output = .true., &                            ! (in) optional
    & namelist_filename = 'histauto_nml15-02.nml' )       ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), &       ! (in)
    & 'u', 'm s-1' )                   ! (in)

  call DCDiffTimeCreate( time1, &     ! (out)
    & hour = 0 )                      ! (in)
  call DCDiffTimeCreate( endtime1, &  ! (out)
    & hour = -1 )                     ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'time'/), &       ! (in)
    & 'v', 'm s-1', &                  ! (in)
    & origin = time1, terminus = endtime1 ) ! (in) optional

  call HistoryAutoAllVarFix

  call DCDiffTimeCreate( time1, &     ! (out)
    & min = 0 )                       ! (in)
  do i = 1, 25
    call HistoryAutoPut( time1, 'u', (/ ( real(j), j = 1, 2 ) /) + 10 * ( i - 1 ) )
    call HistoryAutoPut( time1, 'v', (/ ( real(j), j = 1, 2 ) /) + 10 * ( i - 1 ) )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


1600 continue
  ! 時刻を入れ替えての出力テスト
  ! Output test with switch of time
  !
  call DCDiffTimeCreate( time1, & ! (out)
    & min = 0 )                   ! (in)
  call DCDiffTimeCreate( time2, & ! (out)
    & min = 1 )                   ! (in)
  call DCDiffTimeCreate( time3, & ! (out)
    & min = 2 )                   ! (in)
  call DCDiffTimeCreate( deltime1, & ! (out)
    & min = 1 )                      ! (in)
  call DCDiffTimeCreate( inttime1, & ! (out)
    & min = 10 )                     ! (in)
  call DCDiffTimeCreate( endtime1, & ! (out)
    & min = 35 )                     ! (in)

  call HistoryAutoCreate( &
    &       title = 'Test16 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), &  ! (in)
    &  dimsizes = (/ 2, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'time' /), &  ! (in)
    &     units = (/ 'm  ', 'min'  /), & ! (in)
    &       origin =  time3, terminus = endtime1, &       ! (in) optional
    &     interval =  inttime1, &                         ! (in) optional
    &  file_prefix = 'xhistauto/test16-' )                ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'time'/), 'v', 'm s-1', &  ! (in)
    & time_average = .true., xtype = 'double' ) ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w', (/'x   ', 'time'/), 'w', 'm s-1' )  ! (in)

  call HistoryAutoAllVarFix

  do i = 1, 31
    call HistoryAutoPut( time1, 'u', (/ 10.0, 20.0 /) * i )
    call HistoryAutoPut( time3, 'w', (/ 10.0, 20.0 /) * i )
    call HistoryAutoPut( time2, 'v', (/ 10.0, 20.0 /) * i )
    time1 = time1 + deltime1
    time2 = time2 + deltime1
    time3 = time3 + deltime1
  end do

  call HistoryAutoClose


1700 continue
  ! 時間の次元に "since 20XX-XX-XX .." といった情報を与えるテスト
  ! Test for check that "since 20XX-XX-XX .." is given to unit of time
  !
  call HistoryAutoCreate( &
    &       title = 'Test17 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), &  ! (in)
    &  dimsizes = (/ 2, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'time' /), &  ! (in)
    &     units = (/ 'm                                  ', &
    &                'day since 2008-10-05T00:00:00+00:00' /), & ! (in)
    &  file_prefix = 'xhistauto/test17-' )                ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)

  call HistoryAutoAllVarFix

  call DCDiffTimeCreate( time1, &     ! (out)
    & day = 0 )                       ! (in)
  call DCDiffTimeCreate( deltime1, &  ! (out)
    & day = 1 )                       ! (in)

  do i = 1, 3
    call HistoryAutoPut( time1, 'u', (/ 10.0, 20.0 /) * i )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose

1800 continue
  ! Δt と出力間隔が一致しない場合のテスト1
  ! Test 1 for output interval is not equal to delta t
  !
  call DCDiffTimeCreate( time1, & ! (out)
    & min = 0 )                   ! (in)

  allocate( deltime2(1:3) )
  call DCDiffTimeCreate( deltime2(1), & ! (out)
    & sec = 20.0_DP )                   ! (in)
  call DCDiffTimeCreate( deltime2(2), & ! (out)
    & sec = 30.0_DP )                   ! (in)
  call DCDiffTimeCreate( deltime2(3), & ! (out)
    & sec = 40.0_DP )                   ! (in)

  call HistoryAutoCreate( &
    &       title = 'Test18 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), &  ! (in)
    &  dimsizes = (/ 2, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'time' /), &  ! (in)
    &     units = (/ 'm  ', 'min'  /), & ! (in)
    &     interval = 1.0, &                         ! (in) optional
    &  file_prefix = 'xhistauto/test18-' )          ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1', &  ! (in)
    & interval = 60.0_DP, time_units = 'sec' )  ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'x   ', 'time'/), 'v', 'm s-1', &  ! (in)
    & interval = 100.0_DP, time_units = 'sec' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'w', (/'x   ', 'time'/), 'w', 'm s-1',&   ! (in)
    & interval = 30.0_DP, time_units = 'sec', & ! (in)
    & time_average = .true., xtype = 'double' ) ! (in) optional

  call HistoryAutoAllVarFix

  do i = 0, 10
    call HistoryAutoPut( time1, 'u', (/ 10.0, 20.0 /) * i )
    call HistoryAutoPut( time1, 'v', (/ 10.0, 20.0 /) * i )
    call HistoryAutoPut( time1, 'w', (/ 10.0, 20.0 /) * i )
    time1 = time1 + deltime2( mod(i,3) + 1 )
  end do

  call HistoryAutoClose

  deallocate( deltime2 )

1900 continue
  ! 日時設定テスト
  ! Date configuration test
  !
  call DCDiffTimeCreate( time1, & ! (out)
    & sec = 0.0_DP )              ! (in)
  call DCDiffTimeCreate( deltime1, & ! (out)
    & sec = 1.0_DP )                 ! (in)

  call DCDateTimeCreate( date1, &                  ! (out)
    & year = 2008, mon = 10, day = 10, hour = 12 ) ! (in)
  call HistoryAutoCreate( &
    &       title = 'Test19 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), &           ! (in)
    &  dimsizes = (/ 2, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'time' /), &           ! (in)
    &     units = (/ 'm  ', 'sec'  /), &            ! (in)
    &  file_prefix = 'xhistauto/test19-none-', &    ! (in) optional
    &  origin_date = date1, &                       ! (in) optional
    &  origin_date_invalid = .true. )               ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
  call HistoryAutoAllVarFix
  do i = 1, 2
    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
    time1 = time1 + deltime1
  end do
  call HistoryAutoClose


  call DCDateTimeCreate( date1, &                   ! (out)
    & year = 2008, mon = 10, day = 10, hour = 12, & ! (in)
    & zone = '+09:00' )                             ! (in)
  call HistoryAutoCreate( &
    &       title = 'Test19 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), &           ! (in)
    &  dimsizes = (/ 2, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'time' /), &           ! (in)
    &     units = (/ 'm  ', 'sec'  /), &            ! (in)
    &  file_prefix = 'xhistauto/test19-gregorian-', & ! (in) optional
    &  origin_date = date1 )                          ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
  call HistoryAutoAllVarFix
  do i = 1, 2
    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
    time1 = time1 + deltime1
  end do
  call HistoryAutoClose


  call DCDateTimeCreate( date1, &                   ! (out)
    & year = 2008, mon = 10, day = 10, hour = 12, & ! (in)
    & zone = '+09:00', caltype_str = 'noleap' )     ! (in)
  call HistoryAutoCreate( &
    &       title = 'Test19 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), &           ! (in)
    &  dimsizes = (/ 2, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'time' /), &           ! (in)
    &     units = (/ 'm  ', 'sec'  /), &            ! (in)
    &  file_prefix = 'xhistauto/test19-noleap-', &  ! (in) optional
    &  origin_date = date1, &                       ! (in) optional
    &  origin_date_invalid = .false. )              ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)
  call HistoryAutoAllVarFix
  do i = 1, 2
    call HistoryAutoPut( time1, 'u', (/ 1.0, 2.0 /) * i )
    time1 = time1 + deltime1
  end do
  call HistoryAutoClose


2000 continue
  ! Δt と出力間隔が一致しない場合のテスト2
  ! Test2 for output interval is not equal to delta t
  !
  call DCDiffTimeCreate( time1, & ! (out)
    & min = 0 )                   ! (in)

  call DCDiffTimeCreate( time1, & ! (out)
    & sec = 0.0_DP )              ! (in)
  call DCDiffTimeCreate( deltime1, & ! (out)
    & min = 9 )                      ! (in)

  call HistoryAutoCreate( &
    &       title = 'Test20 for gtool_historyauto', & ! (in)
    &      source = source, &                       ! (in)
    & institution = institution, &                  ! (in)
    &      dims = (/ 'x   ', 'time' /), &  ! (in)
    &  dimsizes = (/ 2, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'time' /), &  ! (in)
    &     units = (/ 'm  ', 'min'  /), &   ! (in)
    &     interval = 1.0, &                         ! (in) optional
    &  file_prefix = 'xhistauto/test20-' )          ! (in) optional

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1', &  ! (in)
    & interval = 60.0_DP, time_units = 'min' )  ! (in)

  call HistoryAutoAllVarFix

  do i = 0, 60
    call HistoryAutoPut( time1, 'u', (/ 10.0, 20.0 /) * i )
    time1 = time1 + deltime1
  end do

  call HistoryAutoClose


2100 continue
  ! 基本テスト (dc_calendar 使用)
  ! Basic test (Use dc_calendar)
  !
  call DCCalCreate('gregorian')
  call DCCalDateCreate('2009-10-09 11:20:30.25', caldate1 )

  call HistoryAutoCreate( &
    &       title = 'Test21 for gtool_historyauto', & ! (in)
    &      source = source, &                         ! (in)
    & institution = institution, &                    ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
    &  start_date = caldate1 )                            ! (in) optional

  timed1 = 0.0_DP
  deltimed1 = 1.0_DP

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1', & ! (in)
    & file = 'xhistauto/test21-u.nc' )         ! (in) optional

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'z   ', 'time'/), 'v', 'm s-1', & ! (in)
    & file = 'xhistauto/test21-v.nc', &                ! (in) optional
    & time_average = .true., xtype = 'double' )        ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w1', (/'y   ', 'z   '/), 'w1', 'm s-1', &         ! (in)
    & file = 'xhistauto/test21-w.nc' )                   ! (in) optional

  call HistoryAutoAddVariable( &
    & 'w2', (/'time', 'y   ', 'z   '/), 'w2', 'm s-1', & ! (in)
    & file = 'xhistauto/test21-w.nc' )                   ! (in) optional

  call HistoryAutoAllVarFix

  do i = 1, 10
    call HistoryAutoPut( timed1, 'u', (/ 1.0, 2.0 /) * i )
    call HistoryAutoPut( timed1, 'v', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    call HistoryAutoPut( timed1, 'w1', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    call HistoryAutoPut( timed1, 'w2', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    timed1 = timed1 + deltimed1
  end do

  call HistoryAutoClose


2200 continue
  ! ユーザ定義暦のテスト
  ! Test for user defined calendar
  !
  call DCCalCreate( 1, (/ 700 /), 12, 1, 1000.0_DP, cal = cal1 )
  call DCCalDateCreate('2000-1-123T23:0:2000', caldate1 )

  timed1 = 9000.0_DP
  deltimed1 = 900.0_DP

  call HistoryAutoCreate( &
    &       title = 'Test22 for gtool_historyauto', & ! (in)
    &      source = source, &                         ! (in)
    & institution = institution, &                    ! (in)
    &      dims = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &  dimsizes = (/ 2, 3, 4, 0 /), &                     ! (in)
    & longnames = (/ 'x   ', 'y   ', 'z   ', 'time' /), & ! (in)
    &     units = (/ 'm  ',  'm  ',  'm  ',  'sec'  /), & ! (in)
    &    xtypes = (/ 'double', 'double', 'double', 'double' /), & ! (in)
    &    origin = timed1, &                               ! (in) optional
    &  terminus = timed1 + deltimed1 * 1000, &            ! (in) optional
    & namelist_filename = 'histauto_nml22.nml', &         ! (in) optional
    &  start_date = caldate1, &                           ! (in) optional
    &         cal = cal1 )                                ! (in) optional

  call HistoryAutoPutAxis( &
    & dim = 'y', array = (/ -2.0, 0.0, 3.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'y', weight = (/ 1.5, 2.5, 3.5 /) )  ! (in)

  call HistoryAutoPutAxis( &
    & dim = 'z', array = (/ 0.0, 20.0, 50.0, 90.0 /) )  ! (in)

  call HistoryAutoAddWeight( &
    & dim = 'z', &                                   ! (in)
    & weight = (/ 10.0, 25.0, 45.0, 20.0 /) * 2.0, & ! (in)
    & units = 'deg.*2' , xtype = 'double' )          ! (in) optional

  call HistoryAutoAddAttr( &
    & varname = 'x', attrname = 'test_attr', &   ! (in)
    & value = 'test_attr_value' )                ! (in)

  call HistoryAutoAddVariable( &
    & 'u', (/'x   ', 'time'/), 'u', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'v', (/'y   ', 'z   ', 'time'/), 'v', 'm s-1' )  ! (in)

  call HistoryAutoAddVariable( &
    & 'w1', (/'y   ', 'z   '/), 'w1', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'w2', (/'time', 'y   ', 'z   '/), 'w2', 'm s-1' ) ! (in)

  call HistoryAutoAddVariable( &
    & 'w3', (/'z   ', 'time'/), 'w3', 'm s-1' ) ! (in)


  call HistoryAutoAllVarFix

  do i = 1, 200
    call HistoryAutoPut( timed1, 'u', (/ 1.0, 2.0 /) * i )
    call HistoryAutoPut( timed1, 'v', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    call HistoryAutoPut( timed1, 'w1', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    call HistoryAutoPut( timed1, 'w2', &
      &    reshape( (/ ( real(j), j = 1, 12 ) /) * i, (/ 3,4 /) ) )
    call HistoryAutoPut( timed1, 'w3', (/ 1.0, 2.0, 3.0, 4.0 /) * i )
    timed1 = timed1 + deltimed1
  end do

  call HistoryAutoClose

end program histauto
