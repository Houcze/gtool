!== Sample program for gtool_history/gtool5
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: histaxis.f90,v 1.2 2009-10-10 11:02:21 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!
program histaxis
  !
  ! Test Program for "HistoryCreate2",
  ! "HistoryAddVariable2", "HistoryPut",
  ! "HistorySetTime", "HistoryClose".
  !
  use gtool_history, only: Copy, Create, &
    & HistoryAddVariable, HistoryAxisAddAttr, HistoryAxisCreate, &
    & HistoryClose, HistoryCreate, HistoryPut, HistoryVarinfoAddAttr, &
    & Inquire, Put_Attr, HistoryVarinfoInitialized, HistoryVarinfoClear
  use dc_test, only: AssertEqual
  use dc_trace, only: SetDebug, DbgMessage
  use dc_types, only:  TOKEN, STRING
  use gtool_history
  implicit none
  integer:: i
  type(GT_HISTORY_AXIS)           :: axes1(1), axes2(3), axes3(2), axes4(3)
  type(GT_HISTORY_VARINFO)        :: var1, var2, var4
  character(TOKEN), pointer       :: dims1(:) =>null()
  character(STRING)               :: name
  type(GT_HISTORY)                :: gt4hist_axis_3
  real(8)          :: Zeta(4,8)

continue

  !----- デバッグモードへ -----
  call SetDebug

  !-----------------------------------------------------------------
  !  HistoryCreate2 と HistoryAddVariable2 を用いた基本出力テスト
  !-----------------------------------------------------------------
  !----- 軸の指定 -----
  call Create(axes1(1), 'time', 0, 'time', 's @ 2001-12-16T10:00', 'float')

  !----- 変数の指定 -----
  call AssertEqual('HistoryVarinfoInitialized Test 1', &
    & answer=.false., check = HistoryVarinfoInitialized( var1 ) )
  call Create(var1, 'u', (/'time'/), 'any quantity', 'non-dimensional', 'float')
  call AssertEqual('HistoryVarinfoInitialized Test 2', &
    & answer=.true., check = HistoryVarinfoInitialized( var1 ) )

  call HistoryCreate(file='xhistaxis/xhistaxis1.nc', &
    & title='gtool_history HistoryCreate2 test 1',   &
    & source='gtool5/Fortran library test kit',  &
    & institution='GFD Dennou Club',               &
    & axes=axes1(:),                               &
    & origin=0.0, interval=0.0,                    &
    & conventions="http://www.gfd-dennou.org/library/gtool4/conventions/", &
    & gt_version="4.1"  )

  call HistoryAddVariable(var1)
  do, i = 1, 24
    call Inquire(axes1(1), name=name)
    call HistoryPut( trim(name) , real(i) )
    call Inquire(var1, name=name)
    call HistoryPut( trim(name), real(i) * 10.0)
  enddo
  call HistoryClose


  !-----------------------------------------------------------------
  !  HistoryCreate2 と HistoryAddVariable2 を用いた出力テスト
  !    構造体の直接指定
  !-----------------------------------------------------------------
  allocate(dims1(2))
  dims1(:) = (/'x   ', 'time'/)
  call Create(axes2(1), 'x', 3, 'eastward length', 'm', 'float')
  call Put_Attr(axes2(1), 'missing_value', -999.0d20 )
  call HistoryAxisCreate(axes2(2), 'time', 0, 'time', 's', 'float')
  call Copy(axes2(3), axes2(1), name='x_dummy')

  call HistoryCreate(file='xhistaxis/xhistaxis2.nc', & 
    & title='gtool_history HistoryCreate2 test 2', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD DennouClub', &
    & conventions="http://www.gfd-dennou.org/library/gtool4/conventions/", &
    & gt_version="4.1", &
    & axes=axes2,  &
    & origin=0.0, interval=0.0) 
  call Create(var2, 'u', dims1(:), 'foo quantity', 'm/s', 'float')
  call HistoryAddVariable(varinfo=var2)
  call HistoryVarinfoClear(varinfo=var2)
  call Create(var2, 'v', dims1(:), 'foo quantity', 'm/s', 'float')
  call HistoryAddVariable(varinfo=var2)
  call HistoryPut('x', (/0.0, 1.0, 2.0/))
  call HistoryPut('time', 0.0)
  call HistoryPut('u', (/0.0, 0.1, 0.2/))
  call HistoryPut('v', (/1.0, 1.1, 1.2/))
  call HistoryPut('time', 1.0)
  call HistoryPut('u', (/2.0, 2.1, 2.2/))
  call HistoryPut('time', 2.0)
  call HistoryPut('u', (/3.0, 3.1, 3.2/))
  call HistoryPut('v', (/4.0, 4.1, 4.2/))
  call HistoryClose

  !-----------------------------------------------------------------
  !  HistoryCreate2 を用いた出力テスト
  !    明示的に引数キーワードを与えずに出力
  !-----------------------------------------------------------------
  !----- 軸の指定 -----
  call Create(axes3(1), 'x', 3, 'eastward length', 'm', 'float')
  call Create(axes3(2), 'time', 0, 'time', 's', 'float')

  call HistoryCreate('xhistaxis/xhistaxis3.nc',   &
    & 'gtool_history HistoryCreate2 test 3',      &
    & 'gtool5/Fortran library test kit',      &
    & 'GFD Dennou Club',                        &
    & axes3,                                    &
    & 100.0, 2.5,                               &
    & gt4hist_axis_3,                           &
    & conventions = 'http://www.gfd-dennou.org/library/gtool4/conventions/', &
    & gt_version = '4.1')

  call HistoryPut('x', (/100.0, 200.0, 300.0/), gt4hist_axis_3)
  call HistoryAddVariable('u', dims=(/'x   ', 'time'/), &
    & longname='any quantity', units='m/s', history=gt4hist_axis_3)
  do, i = 1, 3
    call HistoryPut('u', (/1.0*i, 2.0*i, 3.0*i/), gt4hist_axis_3)
  enddo
  call HistoryClose(gt4hist_axis_3)


  !-----------------------------------------------------------------
  !  HistoryAddAttr2 を用いた出力テスト
  !-----------------------------------------------------------------
  !----- 軸の指定 -----
  call Create(axes4(1), 'lon', 4, 'longitude', 'deg.', 'float')
  call Create(axes4(2), 'lat', 8, 'latitude', 'deg.', 'float')
  call Create(axes4(3), 'time', 0, 'time', 'sec.', 'float')

  !----- 変数の指定 -----
  call Create(var4, 'zeta', (/'lon ', 'lat ', 'time'/), 'vorticity', '1/s', 'double')


  !----- 軸座標の属性の指定 -----
  call Put_Attr(axes4(1), 'topology', 'circular')
  call HistoryAxisAddAttr(axis = axes4(1), attrname='modulo', value=360.0)

  !----- gtool_history モジュールの各サブルーチンを呼ぶ
  call HistoryCreate(file='xhistaxis/xhistaxis4.nc',       &
    & title='gtool_history HistoryCreate2 test 4',         &
    & source='gtool5/Fortran library test kit',        &
    & institution='GFD Dennou Club',                     &
    & axes=axes4(:),                                     &
    & origin=111.0, interval=12.3                       )

  !----- 変数の属性の指定 -----
  call Put_Attr(var4, 'gt_graph_contours_levels', &
    & (/996.0, 1000.0, 1004.0, 1008.0, 1012.0/))
  call HistoryVarinfoAddAttr(varinfo = var4, &
    & attrname='missing_value', value=-2.0d20)
  call Put_Attr(var4, 'ignore_missing', .true.)
  call Put_Attr(var4, 'zero_point', 1000)

  !----- 変数の設定 -----
  call HistoryAddVariable(var4)

  call HistoryPut( 'lon', (/0., 90., 180., 270./) )
  call HistoryPut( 'lat', &
    & (/-70.0, -50.0, -30.0, -10.0, 10.0, 30.0, 50.0, 70.0/) )

  Zeta(:,:) = 3.0
  call HistoryPut('zeta', Zeta)
  Zeta(:,:) = 4.0
  call HistoryPut('zeta', Zeta)
  call HistoryClose

end program histaxis
