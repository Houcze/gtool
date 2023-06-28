!== Sample program for gtool_history/gtool5
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: histtest.f90,v 1.6 2009-10-12 04:07:16 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2008. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!
program histtest
  !
  ! Test Program for "HistoryCreate", "HistoryAddVariable",
  ! "HistoryPut", "HistorySetTime", "HistoryClose".
  !
  use dc_types, only: STRING, DP
  use dc_trace, only: SetDebug
  use dc_string, only: toChar
  use dc_test, only: AssertEqual, AssertGreaterThan, AssertLessThan
  use dc_date_types, only: DC_DIFFTIME
  use dc_date, only: DCDiffTimeCreate, operator(+), operator(*), mod, &
    & DCDiffTimePutLine, EvalNonDim, EvalSec
  use gtool_history, only: GT_HISTORY, HistoryCreate, HistoryAddVariable, &
    &                    HistoryClose, HistoryCopy, HistoryPut, &
    &                    HistorySetTime, HistoryAddAttr, HistoryPutLine
  use dc_string, only: StoA, CPrintf
  implicit none
  integer:: i, dtary(0:2)
  integer, allocatable:: time_ary(:)
  character(STRING):: char
  type(GT_HISTORY):: gt4hist_test_5, gt4hist_test_7, gt4hist_test_8
  real(DP):: Zeta(4,8)
  real(DP), allocatable:: data2d(:,:), data2d1(:,:)
  type(DC_DIFFTIME):: origin1, interval1, curtime1, deltime1
  real(DP):: timed1
  logical:: err, time_average_store
continue

  !----- デバッグモードへ -----
  call SetDebug

!goto 1400

  !-----------------------------------------------------------------
  !  HistoryCreate1 を用いた基本出力テスト
  !    (Conventions と gt_version は自動出力)
  !-----------------------------------------------------------------
  call HistoryPutLine( indent = '  ** PutLine test ** ' )

  call HistoryCreate(file='xhisttest/xhisttest1.nc', &
    & title='gtool_history test 1',                 &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club',              &
    & dims=(/'time'/),                            &
    & dimsizes=(/0/),                             &
    & longnames=(/'time'/),                       &
    & units=(/'s @ 2001-12-16T10:00'/),           &
    & origin=0.0, interval=0.0,                   &
    & xtypes=(/'real'/))

  call HistoryPutLine( indent = '  ** PutLine test ** ' )

  call HistoryAddVariable('u', dims=(/'time'/), &
    & longname='any quantity', units='non-dimensional')
  call HistoryAddVariable('scalar', dims=(/''/), &
    & longname='scalar quantity', units='1')
  call HistoryPut('scalar', 999.9)
  do, i = 1, 24
    call HistoryPut('time', real(i))
    call HistoryPut('u', real(i) * 10.0)
  enddo
  call HistoryPutLine( indent = '  ** PutLine test ** ' )
  call HistoryClose

200 continue

  !-----------------------------------------------------------------
  !  HistoryCreate1 を用いた出力テスト
  !    時間は gtool_history 内部の TimeGoAhead にて進める。
  !    ここでは時間を気にせずただ HistoryPut を呼んでみる。
  !    この場合、HistoryCreate に渡す origin と interval が使われる。
  !    (gt_version のみを手動で出力)
  !-----------------------------------------------------------------
  call HistoryCreate(file='xhisttest/xhisttest2.nc', &
    & title='gtool_history test 2',                 &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club',              &
    & gt_version="4.0",                           &
    & dims=(/'x   ', 'time'/),                    &
    & dimsizes=(/3, 0/),                          &
    & longnames=(/'eastward length', 'time           '/), &
    & units=(/'m','s'/),                          &
    & origin=100.0, interval=2.5,                 &
    & xtypes=(/'real', 'real'/))

  call HistoryPut('x', (/100.0, 200.0, 300.0/))
  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='any quantity', units='m/s')
  do, i = 1, 3
    call HistoryPut('u', (/1.0*i, 2.0*i, 3.0*i/))
  enddo
  call HistoryClose

!goto 1100

  !-----------------------------------------------------------------
  !  HistoryCreate1 を用いた出力テスト
  !    時間は HistoryPut にて与える。
  !    (Conventions と gt_version は手動で出力)
  !    (origin と interval は自動設定)
  !-----------------------------------------------------------------
  call HistoryCreate(file='xhisttest/xhisttest3.nc', &
    & title='gtool_history test 3', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD DennouClub', &
    & conventions="http://www.gfd-dennou.org/library/gtool4/conventions/", &
    & gt_version="4.1",                           &
    & dims=(/'x   ', 'time'/),                    &
    & dimsizes=(/3, 0/),                          &
    & longnames=(/'eastward length', 'time           '/), &
    & units=(/'m','s'/),                          &
    & xtypes=(/'float', 'float'/)         )

  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='foo quantity', units='m/s')
  call HistoryAddVariable('v', dims=(/'x   ', 'time'/), &
    & longname='foo quantity', units='m/s', xtype='int')
  call HistoryPut('x', (/0.0, 1.0, 2.0/))
  call HistoryPut('time', 0.0)
  call HistoryPut('u', (/0.0, 0.1, 0.2/))
  call HistoryPut('v', (/10, 11, 12/))
  call HistoryPut('time', 1.0)
  call HistoryPut('u', (/2.0, 2.1, 2.2/))
  call HistoryPut('time', 2.0)
  call HistoryPut('u', (/3.0, 3.1, 3.2/))
  call HistoryPut('v', (/40, 41, 42/))
  call HistoryClose

  !-----------------------------------------------------------------
  !  HistoryCreate1 を用いた出力テスト
  !    時間は HistorySetTime にて与える。
  !    (Conventions のみを手動で出力)
  !-----------------------------------------------------------------
  call HistoryCreate(file='xhisttest/xhisttest4.nc', &
    & title='gtool_history test 4', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD DennouClub', &
    & conventions="Another certain Conventions", &
    & dims=(/'x   ', 'time'/),                    &
    & dimsizes=(/3, 0/),                          &
    & longnames=(/'eastward length', 'time           '/), &
    & units=(/'m','s'/),                          &
    & origin=0.0, interval=0.0,                 &
    & xtypes=(/'float', 'float'/)         )
  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='foo quantity', units='m/s')
  call HistoryAddVariable('v', dims=(/'x   ', 'time'/), &
    & longname='foo quantity', units='m/s')
  call HistoryPut('x', (/0.0, 1.0, 2.0/))
  call HistorySetTime(0.0)
  call HistoryPut('u', (/0.0, 0.1, 0.2/))
  call HistoryPut('v', (/1.0, 1.1, 1.2/))
  call HistorySetTime(1.0)
  call HistoryPut('u', (/2.0, 2.1, 2.2/))
  call HistorySetTime(2.0)
  call HistoryPut('u', (/3.0, 3.1, 3.2/))
  call HistorySetTime(1.0)
  call HistoryPut('v', (/4.0, 4.1, 4.2/))
  call HistorySetTime(2.0)
  call HistoryPut('v', (/5.0, 5.1, 5.2/))
  call HistoryClose

  !-----------------------------------------------------------------
  ! * HistoryCreate1 を用いた出力テスト
  !   * 明示的に引数キーワードを与えずに出力
  !     GT_HISTORY 変数を利用
  !
  ! * HistoryCopy によるコピーのテスト (GT_HISTORY 変数を利用した場合)
  !-----------------------------------------------------------------
  call HistoryCreate('xhisttest/xhisttest5.nc',    &
    & 'gtool_history test 5',                     &
    & 'gtool5/Fortran library test kit',      &
    & 'GFD Dennou Club',                        &
    & (/'x   ', 'time'/),                       &
    & (/3, 0/),                                 &
    & (/'eastward length', 'time           '/), &
    & (/'m','s'/),                              &
    & 100.0, 2.5,                               &
    & (/'real', 'real'/),                       &
    & gt4hist_test_5,                           &
    & conventions = 'http://www.gfd-dennou.org/library/gtool4/conventions/', &
    & gt_version = '4.1')

  call HistoryPut('x', (/100.0, 200.0, 300.0/), gt4hist_test_5)
  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='any quantity', units='m/s', history=gt4hist_test_5)
  do, i = 1, 3
    call HistoryPut('u', (/1.0*i, 2.0*i, 3.0*i/), gt4hist_test_5)
  enddo

  call HistoryCopy(gt4hist_test_7, 'xhisttest/xhisttest7.nc', &
    & gt4hist_test_5, &
    & title='gtool_history test 7', origin=0.0, interval=0.0)
  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='any quantity', units='m/s', history=gt4hist_test_7)
  do, i = 1, 3
    call HistoryPut('u', (/1.0*i, 2.0*i, 3.0*i/), gt4hist_test_7)
  enddo
  call HistoryClose(gt4hist_test_7)

  call HistoryClose(gt4hist_test_5)


  !-----------------------------------------------------------------
  ! * HistoryAddAttr を用いた出力テスト
  ! * HistoryCopy によるコピーのテスト (GT_HISTORY 変数を利用しない場合)
  !-----------------------------------------------------------------
  call HistoryCreate(file='xhisttest/xhisttest6.nc',       &
    & title='gtool_history test 6',                        &
    & source='gtool5/Fortran library test kit',        &
    & institution='GFD Dennou Club',                     &
    & dims=(/'lon','lat','t  '/),                        &
    & dimsizes=(/4,8,0/),                                &
    & longnames=(/'longitude','latitude ','time     '/), &
    & units=(/'deg.','deg.','sec.'/),                    &
    & origin=111.0, interval=12.3                       )

  call HistoryAddAttr('lon', 'topology', 'circular')
  call HistoryAddAttr('lon', 'modulo',   360.0)

  call HistoryPut( 'lon', (/0., 90., 180., 270./) )
  call HistoryPut( 'lat', &
    & (/-70.0, -50.0, -30.0, -10.0, 10.0, 30.0, 50.0, 70.0/) )

  call HistoryAddVariable( &                          ! 変数定義
    & varname='zeta', dims=(/'lon','lat','t  '/)         , &
    & longname='vorticity', units='1/s', xtype='double'      )

  Zeta(:,:) = 1.0
  call HistoryPut('zeta', Zeta)
  Zeta(:,:) = 2.0
  call HistoryPut('zeta', Zeta)


  call HistoryCopy(gt4hist_test_8, 'xhisttest/xhisttest8.nc', &
    & title='gtool_history test 8', origin=13.13, interval=12.12)
  call HistoryAddVariable('zeta', dims=(/'lon ','lat ', 't   '/), &
    & longname='any quantity', units='m/s', history=gt4hist_test_8)
  do, i = 1, 3
    call HistoryPut('zeta', Zeta * i, gt4hist_test_8)
  enddo
  call HistoryClose(gt4hist_test_8)

  call HistoryClose


  !-----------------------------------------------------------------
  !  HistoryPut の range オプションの動作確認
  !-----------------------------------------------------------------
!!  call HistoryCreate(file='xhisttest/xhisttest9.nc', &
!!    & title='gtool_history test 9', &
!!    & source='gtool5/Fortran library test kit', &
!!    & institution='GFD DennouClub', &
!!    & dims=StoA('x', 'l', 'time'), &
!!    & dimsizes=(/3, 5, 0/), &
!!    & longnames=StoA('eastward length', 'any parameter', 'time'), &
!!    & units=StoA('m','1','s'), &
!!    & origin=0.0, interval=0.0 ,&
!!    & xtypes=StoA('float', 'int', 'float'))
!!  call HistoryAddVariable('u', dims=StoA('x', 'l', 'time'), &
!!    & longname='foo quantity', units='m/s')
!!  call HistoryPut('x', (/10.0, 20.0, 30.0/))
!!  call HistoryPut('l', (/1.0, 2.0, 3.0, 4.0, 5.0/))
!!  call HistorySetTime(0.0)
!!  do i = 1, 5
!!    char = CPrintf("l=%d", i=(/i/))
!!    call HistoryPut('u', &
!!      & (/1.0 * real(i), 1.1 * real(i), 1.2 * real(i)/), range=char)
!!  end do
!!  call HistorySetTime(1.0)
!!  do i = 1, 5
!!    char = CPrintf("l=%d", i=(/i/))
!!    call HistoryPut('u', &
!!      & (/2.0 * real(i), 2.1 * real(i), 2.2 * real(i)/), range=char)
!!  end do
!!  call HistorySetTime( timed = 2.0_DP )
!!  do i = 1, 5
!!    char = CPrintf("l=%d", i=(/i/))
!!    call HistoryPut('u', &
!!      & (/3.0 * real(i), 3.1 * real(i), 3.2 * real(i)/), range=char)
!!  end do
!!  call HistoryPutLine( indent = '  ** PutLine test ** ' )
!!  call HistoryClose
!!
!!1000 continue
 !-----------------------------------------------------------------
 !  HistoryPut の平均値出力の動作確認
 !-----------------------------------------------------------------
 call HistoryCreate(file='xhisttest/xhisttest10.nc', &
   & title='gtool_history test 10', &
   & source='gtool5/Fortran library test kit', &
   & institution='GFD DennouClub', &
   & dims=StoA('x', 'y', 'time'), &
   & dimsizes=(/3, 2, 0/), &
   & longnames=StoA('eastward length', 'northward length', 'time'), &
   & units=StoA('m', 'm', 's'), &
   & origin=0.0, interval=10.0  )
 call HistoryAddVariable('Scalar', dims=(/''/), &
   & longname='any quantity', units='m')
 call HistoryAddVariable('Scalar2', dims=(/''/), &
   & longname='any quantity', units='m', time_average=.true., &
   & err = err )
 call AssertEqual( 'average output error handling test 1-1', &
   & answer = .true., check = err )
 call HistoryAddVariable('Data', dims=StoA('x', 'y', 'time'), &
   & longname='any quantity', units='m/s')
 call HistoryAddVariable('DataAvr', dims=StoA('x', 'y', 'time'), &
   & longname='average of any quantity', units='m/s', time_average=.true.)
 call HistoryAddVariable('DataAvr2', dims=StoA('x', 'time'), &
   & longname='average of any quantity', units='m/s', average=.true.)
 call HistoryPut('x', (/1.0, 2.0, 3.0/))
 call HistoryPut('y', (/1.0, 2.0, 3.0/))
 call HistoryPut('Scalar', (/1.0/))

 allocate( data2d(1:3,1:2) )
 data2d = reshape( (/1.0, 2.0, 3.0, 4.0, 5.0, 6.0/), (/3,2/) )
 call HistoryPut( varname = 'DataAvr', & ! (in)
   & array = data2d, &                   ! (in)
   & err = err )                         ! (out)
 call AssertEqual( 'average output error handling test 2-1', &
   & answer = .true., check = err )

 call HistoryPut( varname = 'Data', &    ! (in)
   & array = (/1.0, 2.0, 3.0/), &        ! (in)
   & range = 'y=1', &                    ! (in)
   & time = 2.0, &                       ! (in)
   & err = err )                         ! (out)
 call AssertEqual( 'average output error handling test 2-2', &
   & answer = .true., check = err )

 call HistoryPut( varname = 'DataAvr', & ! (in)
   & array = (/1.0, 2.0, 3.0/), &        ! (in)
   & time = 2.0, &                       ! (in)
   & err = err )                         ! (out)
 call AssertEqual( 'average output error handling test 2-3', &
   & answer = .true., check = err )

 do i = 1, 35
   data2d = data2d + 1.0
   call HistoryPut( varname = 'Data', &
     & array = data2d, time = real(i), quiet = .false. )
   call HistoryPut( varname = 'DataAvr', &
     & array = data2d, time = real(i), quiet = .false. )
   call HistoryPut( varname = 'DataAvr2', &
     & array = data2d(:,1), time = real(i), quiet = .false. )
 end do
 call HistoryPutLine( indent = '  ** PutLine test ** ' )
 call HistoryClose

 deallocate( data2d )


! goto 9999

1100 continue

  !-----------------------------------------------------------------
  !  HistoryCreate3 を用いた基本出力テスト
  !-----------------------------------------------------------------
  call DCDiffTimeCreate( origin1,    1.0, 'hr'  )
  call DCDiffTimeCreate( interval1,  1.0, 'hr'  )
  call DCDiffTimeCreate( deltime1,  10.0, 'min' )
  curtime1 = origin1
  call HistoryCreate(file='xhisttest/xhisttest11.nc', &
    & title='gtool_history test 11',              &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club',              &
    & dims=(/'x   ', 'time'/),                    &
    & dimsizes=(/2,0/),                           &
    & longnames=(/'x   ', 'time'/),               &
    & units=(/'m                     ', &
    &         'min @ 2008-09-11T13:30'/),         &
    & origin=origin1, interval=interval1 )

  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='any quantity', units='non-dimensional')
  call HistoryAddVariable('scalar', dims=(/''/), &
    & longname='scalar quantity', units='1')
  call HistoryPut('scalar', 999.9)
  do, i = 0, 24
    call HistoryPut('u', (/real(i) * 10.0, real(i) * 20.0 /), &
      & difftime = curtime1 )
    curtime1 = curtime1 + deltime1
  enddo
  call HistoryClose

1200 continue

  !-----------------------------------------------------------------
  !  HistoryCreate3 を用いた基本出力テスト2
  !    HistorySetTime を使用する.
  !-----------------------------------------------------------------
  call DCDiffTimeCreate( origin1,    0.0001_DP, '1' )
  call DCDiffTimeCreate( interval1,  0.0100_DP, '1' )
  call DCDiffTimeCreate( deltime1,   0.0001_DP, '1' )
  curtime1 = origin1
  dtary(0:2) = (/ 5, 2, 3 /)
  call HistoryCreate(file='xhisttest/xhisttest12.nc', &
    & title='gtool_history test 12',              &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club',              &
    & dims=(/'x   ', 'time'/),                    &
    & dimsizes=(/2,0/),                           &
    & longnames=(/'x   ', 'time'/),               &
    & units=(/'m', '1'/),                         &
    & origin=origin1, interval=interval1 )

  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='any quantity', units='non-dimensional')
  call HistoryPut('u', (/0.0, 0.0/) )
  curtime1 = curtime1 + deltime1 * 9

  do, i = 1, 27
    curtime1 = curtime1 + deltime1 * dtary( mod( i, 3 ) )
    call HistorySetTime( difftime = curtime1 )
    call HistoryPut('u', (/real(i) * 1.0, real(i) * 2.0 /) )
  enddo
  call HistoryClose

1300 continue

  !-----------------------------------------------------------------
  !  HistoryCreate3 を用いた基本出力テスト3
  !    時間が不等な間隔の場合の平均化処理
  !-----------------------------------------------------------------
  call DCDiffTimeCreate( origin1, min=0 )
  allocate( time_ary(1:10) )
  time_ary = (/ 0, 1, 2, 10, 11, 12, 20, 21, 22, 30 /)

  call HistoryCreate(file='xhisttest/xhisttest13.nc', &
    & title='gtool_history test 13', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD DennouClub', &
    & dims=StoA('x', 'y', 'time'), &
    & dimsizes=(/3, 2, 0/), &
    & longnames=StoA('eastward length', 'northward length', 'time'), &
    & units=StoA('m', 'm', 'min'), &
    & origin=origin1 )

  call HistoryAddVariable('Data', dims=StoA('x', 'y', 'time'), &
    & longname='any quantity', units='m/s')
  call HistoryAddVariable('DataAvr', dims=StoA('x', 'y', 'time'), &
    & longname='average of any quantity', units='m/s', time_average=.true.)
  call HistoryAddVariable('DataAvr2', dims=StoA('x', 'time'), &
    & longname='average of any quantity', units='m/s', average=.true.)

  call HistoryPut('x', (/1.0, 2.0, 3.0/))
  call HistoryPut('y', (/1.0, 2.0, 3.0/))

  allocate( data2d(1:3,1:2), data2d1(1:3,1:2) )
  data2d = reshape( (/10.0, 20.0, 30.0, 40.0, 50.0, 60.0/), (/3,2/) )

  do i = 1, 10
    call DCDiffTimeCreate( curtime1, min = time_ary(i) )
    timed1 = time_ary(i)

    data2d1 = data2d * i
    time_average_store = .not. mod( time_ary(i), 10 ) == 0
    if ( .not. time_average_store ) then
      call HistorySetTime( difftime = curtime1 )
    end if

    call HistoryPut( varname = 'Data', &
      & array = (/ data2d1 /), timed = timed1, &
      & time_average_store = time_average_store )
    call HistoryPut( varname = 'DataAvr', &
      & array = data2d1, timed = timed1, &
      & time_average_store = time_average_store )
    call HistoryPut( varname = 'DataAvr2', &
      & array = data2d1(:,1), difftime = curtime1, &
      & time_average_store = time_average_store )
  end do
  call HistoryPutLine( indent = '  ** PutLine test ** ' )
  call HistoryClose

  deallocate( data2d )

1400 continue

  !-----------------------------------------------------------------
  !  文字型データの出力
  !-----------------------------------------------------------------
  call HistoryCreate(file='xhisttest/xhisttest14.nc', &
    & title='gtool_history test 14',                 &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club',              &
    & dims=(/'strlen_time', 'time       '/),      &
    & dimsizes=(/10, 0/),                         &
    & longnames=(/'strlen_time', 'time       '/), &
    & units=(/'none                ', &
    &         's @ 2001-12-16T10:00'/),           &
    & origin=0.0, interval=0.0,                   &
    & xtypes=(/'int ', 'real'/))

  call HistoryAddVariable('str', dims=(/'strlen_time', 'time       '/), &
    & longname='string', units='none', &
    & xtype = 'char' )
  do, i = 1, 24
    call HistoryPut('time', real(i))
    char = toChar(real(i) * 10.0)
    call HistoryPut('str', 'str=' // trim(char) // '          ' )
  enddo
  call HistoryClose

9999 continue

end program histtest
