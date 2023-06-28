!== Sample program for gtool_history/gtool5
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histget.f90,v 1.6 2009-07-04 05:05:05 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!

program histget
  ! histget.f90 - Sample program for gtool_history/gtool5.
  !               Test Program for "HistoryGet".
  !

  use dc_trace,   only: SetDebug, DataDump
  use dc_message, only: MessageNotify
  use dc_types,   only: STRING, DP
  use dc_string,  only: StoA, toChar
  use dc_test, only: AssertEqual
  use gtool_history
  implicit none
  integer             :: i,j,k,l                        ! 作業変数
  integer,  parameter :: nx=3, ny=4, nz=5               ! グリッド数
  integer,  parameter :: time_period=3                  ! 時間
  real,     parameter :: x(nx)=(/(1.0*(i-1),i=1,nx)/)   ! x座標変数
  real(DP), parameter :: y(ny)=(/(10.0*(i-1),i=1,ny)/)  ! y座標変数
  real(DP), parameter :: z(nz)=(/(100.0*(i-1),i=1,nz)/) ! z座標変数

  real                :: u           ! 出力兼入力用無次元配列
  real(DP)            :: v(nx)       ! 出力兼入力用 1 次元配列
  real(DP)            :: v2(nz)      ! 出力兼入力用 1 次元配列その2
  integer             :: w(nx,ny)    ! 出力兼入力用 2 次元配列
  real(DP)            :: q(nx,ny,nz) ! 出力兼入力用 3 次元配列

  real(DP), allocatable:: qa(:,:,:)  ! 入力用 3 次元配列

  real,     pointer   :: up       =>null() ! 入力用無次元配列ポインタ
  real(DP), pointer   :: vp(:)    =>null() ! 入力用 1 次元配列ポインタ
  integer,  pointer   :: wp(:,:)  =>null() ! 入力用 2 次元配列ポインタ
  real(DP), pointer   :: qp(:,:,:)=>null() ! 入力用 3 次元配列ポインタ

  real,     pointer   :: xp(:) =>null()    ! 入力用 x座標元配列ポインタ
  real(DP), pointer   :: yp(:) =>null()    ! 入力用 y座標元配列ポインタ
  real(DP), pointer   :: zp(:) =>null()    ! 入力用 z座標元配列ポインタ

  real,     pointer   :: tp(:) =>null()    ! 入力用 時間次元配列ポインタ

  real(DP), pointer   :: vp_range     =>null() ! range での入力テスト用
  real(DP), pointer   :: qp_range(:,:)=>null() ! range での入力テスト用

  real(DP), allocatable:: ma(:,:,:,:,:,:,:)  ! 縮退次元のあるデータ入力テスト用

  real(DP):: rettime
  logical:: texist
  logical:: err
  character(STRING), parameter  :: subname = 'histget'
continue

  call SetDebug

  !-------------------------------------------------------------------
  !  エラーフラグのチェック
  !-------------------------------------------------------------------
  err = .false.
  call HistoryGet('xhistget/xhistget_non.nc', 'v', v, err = err)
  call AssertEqual('err flag test 1', answer = .true., check = err)

  !-------------------------------------------------------------------
  !  まずは入力用のファイルを作成
  !-------------------------------------------------------------------

  call HistoryCreate(file='xhistget/xhistget1.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=StoA('x','y','z','t'), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=StoA('X-coordinate','Y-coordinate', 'Z-coordinate','time'), &
    & units=StoA('m','m','m','s'), &
    & origin=0.0, interval=0.2, &
    & xtypes=StoA('real','double','double','real'))

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

  call MessageNotify('M', subname, 'Input file <%c> is generated.', &
    & c1='xhistget/xhistget1.nc')

  !-------------------------------------------------------------------
  !  固定長配列による入力テスト (色々な作法で入力)
  !-------------------------------------------------------------------

  call MessageNotify('M', subname, 'Getting by Fixed-Length Array.')

  call HistoryCreate(file='xhistget/xhistget2.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/))

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

  call HistoryGet(file='xhistget/xhistget1.nc', varname='scalar', &
    & array=u)
  call HistoryPut('scalar', u)
  do, l = 1, time_period - 1
    call HistoryPut('t', real(l)*0.2)
    call HistoryGet(file='xhistget/xhistget1.nc', varname='u', & 
      &          array=u, time=real(l)*0.2)
    call HistoryPut('u', u)
    call HistoryGet('xhistget/xhistget1.nc', 'v', v, dble(l)*0.2_DP)
    call HistoryPut('v', v)
    call HistoryGet('xhistget/xhistget1.nc', 'v2', v2, range='t=^'//toChar(l))
    call HistoryPut('v2', v2)
    call HistoryGet('xhistget/xhistget1.nc', 'w', w, time=real(l)*0.2)
    call HistoryPut('w', w)
    call HistoryGet('xhistget/xhistget1.nc', 'q', q, 't=^'//toChar(l))
    call HistoryPut('q', q)
  enddo

  !---------------------------
  !  時間の自動判別のテスト
  l = time_period
  call HistoryPut('t', real(l)*0.2)
  call HistoryGet(file='xhistget/xhistget1.nc', varname='u', array=u)
  call HistoryPut('u', u)
  call HistoryGet('xhistget/xhistget1.nc', 'v', v, range='')
  call HistoryPut('v', v)
  call HistoryGet('xhistget/xhistget1.nc', 'v2', v2, time=real(l)*0.2)
  call HistoryPut('v2', v2)
  call HistoryGet('xhistget/xhistget1.nc', 'w', w)
  call HistoryPut('w', w)
  call HistoryGet('xhistget/xhistget1.nc', 'q', q, 't=^'//toChar(l))
  call HistoryPut('q', q)

  call HistoryClose

  !---------------------------
  !  配列サイズが異なる場合の処理
  allocate( qa (nx+1,ny+1,nz+1) )

  err = .false.
  call HistoryGet('xhistget/xhistget1.nc', 'v', qa, &
    & quiet = .true., err = err )
  call AssertEqual('rank consistency test 1', answer = .true., check = err)

  err = .false.
  call HistoryGet('xhistget/xhistget1.nc', 'q', qa, &
    & quiet = .true., err = err )
  call AssertEqual('shape consistency test 1', answer = .true., check = err)

  deallocate ( qa )
  allocate( qa (nx,ny,nz) )

  err = .true.
  call HistoryGet('xhistget/xhistget1.nc', 'q', qa, err = err )
  call AssertEqual('shape consistency test 2', answer = .false., check = err)

  !-------------------------------------------------------------------
  !  ポインタ配列による入力テスト (色々な作法で入力)
  !-------------------------------------------------------------------


  call HistoryCreate(file='xhistget/xhistget3.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/))

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

  call HistoryGetPointer(file='xhistget/xhistget1.nc', varname='scalar', &
    & array=up)
  call HistoryPut('scalar', up)
  deallocate(up)
  do, l = 1, time_period - 1
    call HistoryPut('t', real(l)*0.2)
    call HistoryGetPointer(file='xhistget/xhistget1.nc', varname='u', & 
      &          array=up, time=real(l, DP) * 0.2_DP )
    call HistoryPut('u', up)
    deallocate(up)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'v', vp, dble(l)*0.2_DP)
    call HistoryPut('v', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'v2', vp, range='t=^'//toChar(l))
    call HistoryPut('v2', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'w', wp, time=real(l)*0.2)
    call HistoryPut('w', wp)
    deallocate(wp)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'q', qp, 't=^'//toChar(l))
    call HistoryPut('q', qp)
    deallocate(qp)
  enddo

  !---------------------------
  !  時間の自動判別のテスト
  l = time_period
  call HistoryPut('t', real(l)*0.2)
  call HistoryGetPointer(file='xhistget/xhistget1.nc', varname='u', array=up)
  call HistoryPut('u', up)
  deallocate(up)
  call HistoryGetPointer('xhistget/xhistget1.nc', 'v', vp, range='')
  call HistoryPut('v', vp)
  deallocate(vp)
  call HistoryGetPointer('xhistget/xhistget1.nc', 'v2', vp, dble(l)*0.2_DP)
  call HistoryPut('v2', vp)
  deallocate(vp)
  call HistoryGetPointer('xhistget/xhistget1.nc', 'w', wp)
  call HistoryPut('w', wp)
  deallocate(wp)
  call HistoryGetPointer('xhistget/xhistget1.nc', 'q', qp, 't=^'//toChar(l))
  call HistoryPut('q', qp)
  deallocate(qp)

  call HistoryClose

  !-------------------------------------------------------------------
  !  次元データの入力テスト (色々な作法で入力)
  !-------------------------------------------------------------------
  call MessageNotify('M', subname, 'Getting by Various Forms.')

  call HistoryCreate(file='xhistget/xhistget4.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/))

  ! 次元データを入力する
  call HistoryGetPointer('xhistget/xhistget1.nc', 'x' ,xp)   ! x 次元入力
  call HistoryGetPointer('xhistget/xhistget1.nc', 'y' ,yp)   ! y 次元入力
  call HistoryGetPointer(file='xhistget/xhistget1.nc', varname='z' ,array=zp)   ! z 次元入力

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


  call HistoryGetPointer(file='xhistget/xhistget1.nc', varname='scalar', &
    & array=up)
  call HistoryPut('scalar', up)
  deallocate(up)
  do, l = 1, time_period
    call HistoryPut('t', real(l)*0.2)
    call HistoryGetPointer(file='xhistget/xhistget1.nc', varname='u', & 
      &          array=up, time=real(l) * 0.2 )
    call HistoryPut('u', up)
    deallocate(up)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'v', vp, dble(l)*0.2_DP)
    call HistoryPut('v', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'v2', vp, range='t=^'//toChar(l))
    call HistoryPut('v2', vp)
    deallocate(vp)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'w', wp, time=real(l)*0.2)
    call HistoryPut('w', wp)
    deallocate(wp)
    call HistoryGetPointer('xhistget/xhistget1.nc', 'q', qp, 't=^'//toChar(l))
    call HistoryPut('q', qp)
    deallocate(qp)
  enddo

  call HistoryClose

  call HistoryGetPointer(file='xhistget/xhistget1.nc', varname='t', &
    & array=tp)
  call AssertEqual('time get test 1', &
    & answer = (/2, 4, 6/), check = int(tp * 10) )
  deallocate(tp)

  !-------------------------------------------------------------------
  !  range オプションのテスト
  !-------------------------------------------------------------------
  call MessageNotify('M', subname, 'Getting with range option. (take a long time ...)')

  call HistoryCreate(file='xhistget/xhistget5.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/))

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

  call HistoryGet(file='xhistget/xhistget1.nc', varname='scalar', &
    & array=u)
  call HistoryPut('scalar', u)
  do, l = 1, time_period - 1
    call HistoryPut('t', real(l)*0.2)
    call HistoryGet(file='xhistget/xhistget1.nc', varname='u', & 
      &          array=u, range='t=' // toChar(real(l)*0.2 + 0.05))
    call HistoryPut('u', u)

    do i = 1, size(xp)
      call HistoryGet('xhistget/xhistget1.nc', 'v', v(i), &
        & range='t=' // trim(toChar(real(l)*0.2 - 0.05)) // &
        &       ',x=' // trim(toChar(xp(i))) )
    end do
    call HistoryPut('v', v)

    do k = 1, size(zp)
      if (associated(vp_range)) deallocate(vp_range)
      call HistoryGetPointer('xhistget/xhistget1.nc', &
        & 'v2', vp_range, &
        & range='t=' // trim(toChar(real(l)*0.2)) // &
        &       ',z=' // trim(toChar(zp(k))) )
      v2(k) = vp_range
    end do
    call HistoryPut('v2', v2)

    do j = 1, size(yp)
      call HistoryGet('xhistget/xhistget1.nc', 'w', w(:,j), &
        & range='t=^' // trim(toChar(l)) // &
        &       ',y=' // trim(toChar(yp(j))) )
    end do
    call HistoryPut('w', w)

    do i = 1, size(xp)
      if (associated(qp_range)) deallocate(qp_range)
      call HistoryGetPointer('xhistget/xhistget1.nc', 'q', &
        & qp_range, &
        & range='t=^' // trim(toChar(l)) // &
        &       ',x=' // trim(toChar(xp(i))) )
      q(i,:,:) = qp_range(:,:)
    end do
    call HistoryPut('q', q)
  enddo

  !---------------------------
  !  時間の自動判別のテスト
  l = time_period
  call HistoryPut('t', real(l)*0.2)
  call HistoryGet(file='xhistget/xhistget1.nc', varname='u', array=u)
  call HistoryPut('u', u)
  do i = 1, size(xp)
    call HistoryGet('xhistget/xhistget1.nc', 'v', v(i), &
      & range='x=' // trim(toChar(xp(i))) )
  end do
  call HistoryPut('v', v)
  call HistoryGet('xhistget/xhistget1.nc', 'v2', v2, time=real(l)*0.2)
  call HistoryPut('v2', v2)
  do j = 1, size(yp)
    call HistoryGet('xhistget/xhistget1.nc', 'w', w(:,j), &
      & range='y=' // trim(toChar(yp(j))) )
  end do
  call HistoryPut('w', w)
  call HistoryGet('xhistget/xhistget1.nc', 'q', q, 't=^'//toChar(l))
  call HistoryPut('q', q)

  call HistoryClose


  !-------------------------------------------------------------------
  !  returned_value と flag_time_exist のテスト
  !-------------------------------------------------------------------
  call HistoryGet('xhistget/xhistget1.nc', 'scalar', u, &
    & returned_time = rettime, flag_time_exist = texist )
  call AssertEqual('returned_time test 1', &
    & answer = int(rettime * 10000), check = 0 )
  call AssertEqual('flag_time_exist test 1', &
    & answer = texist, check = .false. )

  call HistoryGet('xhistget/xhistget1.nc', 'u', u, &
    & returned_time = rettime, flag_time_exist = texist )
  call AssertEqual('returned_time test 2', &
    & answer = int(rettime * 10000), check = 6000 )
  call AssertEqual('flag_time_exist test 2', &
    & answer = texist, check = .true. )

  call HistoryGet('xhistget/xhistget1.nc', 'u', u, time=0., &
    & returned_time = rettime )
  call AssertEqual('returned_time test 3', &
    & answer = int(rettime * 10000), check = 2000 )

  if (associated(vp)) deallocate(vp)
  call HistoryGetPointer('xhistget/xhistget1.nc', 'v', vp, range='t=0.35', &
    & returned_time = rettime )
  if (associated(vp)) deallocate(vp)
  call AssertEqual('returned_time test 4', &
    & answer = int(rettime * 10000), check = 4000 )



  !-------------------------------------------------------------------
  !  X 次元データを X 次元配列ではない変数で読み込むテスト
  !-------------------------------------------------------------------
  call MessageNotify('M', subname, 'Getting array with different dimsize array ')

  call HistoryCreate(file='xhistget/xhistget6.nc', &
    & title='gtool_history HistoryGet test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &  'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.2, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/))

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

  call HistoryGet(file='xhistget/xhistget1.nc', varname='scalar', &
    & array=u)
  call HistoryPut('scalar', u)

  if ( allocated( ma ) ) deallocate( ma )
  do, l = 1, time_period
    allocate( ma(1, 1, 1, 1, 1, 1, 1) )
    call HistoryGet(file='xhistget/xhistget1.nc', varname='u', &
      &          array=ma, time=real(l)*0.2)
    call HistoryPut('u', ma )
    deallocate( ma )

    allocate( ma(1, 1, 1, nx, 1, 1, 1) )
    call HistoryGet(file='xhistget/xhistget1.nc', varname='v', &
      &          array=ma, time=real(l)*0.2)
    call HistoryPut('v', ma )
    deallocate( ma )

    allocate( ma(nz, 1, 1, 1, 1, 1, 1) )
    call HistoryGet(file='xhistget/xhistget1.nc', varname='v2', &
      &          array=ma, time=real(l)*0.2)
    call HistoryPut('v2', ma )
    deallocate( ma )

    allocate( ma(1, 1, nx, 1, ny, 1, 1) )
    call HistoryGet(file='xhistget/xhistget1.nc', varname='w', &
      &          array=ma, time=real(l)*0.2)
    call HistoryPut('w', ma )
    deallocate( ma )

    allocate( ma(1, nx, 1, ny, 1, 1, nz) )
    call HistoryGet(file='xhistget/xhistget1.nc', varname='q', &
      &          array=ma, time=real(l)*0.2)
    call HistoryPut('q', ma )
    deallocate( ma )

  enddo

  call HistoryClose



  !-------------------------------------------------------------------
  !  割付解除
  !-------------------------------------------------------------------

  if ( associated(up) ) deallocate(up)
  if ( associated(vp) ) deallocate(vp)
  if ( associated(wp) ) deallocate(wp)
  if ( associated(qp) ) deallocate(qp)
  if ( associated(xp) ) deallocate(xp)
  if ( associated(yp) ) deallocate(yp)
  if ( associated(zp) ) deallocate(zp)
  if ( associated(tp) ) deallocate(tp)
  if ( associated(vp_range) ) deallocate(vp_range)
  if ( associated(qp_range) ) deallocate(qp_range)

end program histget
