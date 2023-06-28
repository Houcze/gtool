!== Sample program for gtool_history/gtool5
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histcopy.f90,v 1.1 2008-09-23 09:56:38 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2006-. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!
program histcopy
  ! histcopy.f90 - Sample program for gtool_history/gtool5.
  !                Test Program for "HistoryCopyVariable".
  !
  use dc_trace,   only: SetDebug
  use dc_message, only: MessageNotify
  use dc_types,   only: DP
  use gtool_history
  implicit none
  integer            :: i,j,k,l                        ! 作業変数
  integer, parameter :: nx=3, ny=4, nz=5               ! グリッド数
  integer, parameter :: time_step=6                    ! 時間ステップ数
  real,    parameter :: x(nx)=(/(1.0*(i-1),i=1,nx)/)   ! x座標変数
  real(DP),parameter :: y(ny)=(/(10.0*(i-1),i=1,ny)/)  ! y座標変数
  real(DP),parameter :: z(nz)=(/(100.0*(i-1),i=1,nz)/) ! z座標変数

  real               :: u           ! 出力用無次元変数
  real(DP)           :: v(nx)       ! 出力用 1 次元変数
  real               :: w(nx,ny)    ! 出力用 2 次元変数
  real(DP)           :: q(nx,ny,nz) ! 出力用 3 次元変数

continue

  call SetDebug

  !-----------------------------------------------------------------
  ! まずはコピー元となるファイルを作成
  !-----------------------------------------------------------------
  call HistoryCreate(file='xhistcopy/xhistcopy1.nc', &
    & title='gtool_history HistoryCopyVariable test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x','y','z','t'/), &
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &             'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/))

  call HistoryPut('x',x)   ! 次元変数出力
  call HistoryPut('y',y)   ! 次元変数出力
  call HistoryPut('z',z)   ! 次元変数出力

  !----- 変数定義 -----
  call HistoryAddVariable('u', dims=(/'t'/), &
    & longname='Non Dimensional any quantity', &
    & units='non-dimensional')
  call HistoryAddVariable('v', dims=(/'x','t'/), &
    & longname='1 Dimensional any quantity', &
    & units='1-dimensional any unit', xtype='double')
  call HistoryAddVariable('w', dims=(/'x','y','t'/), &
    & longname='2 Dimensional any quantity', &
    & units='2-dimensional any unit')
  call HistoryAddVariable('q', dims=(/'x','y','z','t'/), &
    & longname='3 Dimensional any quantity', &
    & units='3-dimensional any unit', xtype='double')

  !----- 変数属性付加 -----
  ! 各変数への属性付加
  call HistoryAddAttr('u', 'scale_factor',  100.0)
  call HistoryAddAttr('v', 'add_offset',    1000.0)
  call HistoryAddAttr('w', 'missing_value', -2.0e20)
  call HistoryAddAttr('q', 'valid_range', (/-1.0e30, 1.0e30/))

  !----- 数値代入 -----
  do, l = 1, time_step
    u = real(l)*0.01
    do i = 1, nx
      v(i) = dble(l)*0.01d0+dble(i)*1.0d0
      do j = 1, ny
        w(i,j) = real(l)*0.01+real(i)*1.0+real(j)*100.0
        do k = 1, nz
          q(i,j,k) = dble(l)*0.01d0 + dble(i)*1.0d0 &
            & + dble(j)*100.0d0+dble(k)*10000.0d0
        enddo
      enddo
    enddo
    ! 時間を明示的に与える場合
    call HistoryPut('t', real(l)*0.2)
    call HistoryPut('u', u)
    call HistoryPut('v', v)
    call HistoryPut('w', w)
    call HistoryPut('q', q)
  enddo
  call HistoryClose

  !-----------------------------------------------------------------
  !  次元の認識テスト
  !    1) HistoryCreate は基本的に上と同様
  !    2) 変更点は以下の2つ
  !       a) 時間の次元変数 "t" を "time" に
  !       b) 鉛直座標の次元変数 "z" を "Z" に
  !    これらの差異は、HistoryCopyVariable では無視される仕様に
  !    なっている。
  !-----------------------------------------------------------------

  call HistoryCreate(file='xhistcopy/xhistcopy2.nc', &
    & title='gtool_history HistoryCopyVariable test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'x   ','y   ','Z   ','time'/), & ! わざと t を time に変更
    & dimsizes=(/nx,ny,nz,0/), &
    & longnames=(/'X-coordinate','Y-coordinate', &
    &             'Z-coordinate','time        '/), &
    & units=(/'m','m','m','s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real  ','double','double','real  '/))

  call HistoryPut('x',x)   ! 次元変数出力
  call HistoryPut('y',y)   ! 次元変数出力
  call HistoryPut('Z',z)   ! 次元変数出力

  !----- 変数コピー -----
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'u')
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'v')
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'w')
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'q')

  !----- 元ファイルの時と同様な計算ループ -----
  do, l = 1, time_step
    u = real(l)*0.01
    do i = 1, nx
      v(i) = dble(l)*0.01d0+dble(i)*1.0d0
      do j = 1, ny
        w(i,j) = real(l)*0.01+real(i)*1.0+real(j)*100.0
        do k = 1, nz
          q(i,j,k) = dble(l)*0.01d0 + dble(i)*1.0d0 &
            & + dble(j)*100.0d0+dble(k)*10000.0d0
        enddo
      enddo
    enddo
    ! 時間を明示的に与える場合
    call HistoryPut('time', real(l)*0.2)
    call HistoryPut('u', u)
    call HistoryPut('v', v)
    call HistoryPut('w', w)
    call HistoryPut('q', q)
  enddo
  
  call HistoryClose

  !-----------------------------------------------------------------
  !  次元の自動生成テスト
  !    1) HistoryCreate で指定する次元は時間のみ。
  !       空間次元の定義と値の代入は HistoryCopyVariable で定義
  !       される変数に応じて行なわれる。
  !    2) 空間次元のための HistoryPut は行なわない。
  !       HistoryCopyVariable で自動的にコピーされる。
  !    3) 時間次元のための HistoryPut は行なわない。
  !       変数の HistoryPut に応じて自動的に作成される。
  !-----------------------------------------------------------------

  call HistoryCreate(file='xhistcopy/xhistcopy3.nc', &
    & title='gtool_history HistoryCopyVariable test 1', &
    & source='gtool5/Fortran library test kit', &
    & institution='GFD Dennou Club', &
    & dims=(/'t'/), &
    & dimsizes=(/0/), &
    & longnames=(/'time'/), &
    & units=(/'s'/), &
    & origin=0.0, interval=0.2, &
    & xtypes=(/'real'/))

  !----- 変数コピー -----
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'u')
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'v')
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'w')
  call HistoryCopyVariable('xhistcopy/xhistcopy1.nc', 'q')

  !----- 元ファイルの時と同様な計算ループ -----
  do, l = 1, time_step
    u = real(l)*0.01
    do i = 1, nx
      v(i) = dble(l)*0.01d0+dble(i)*1.0d0
      do j = 1, ny
        w(i,j) = real(l)*0.01+real(i)*1.0+real(j)*100.0
        do k = 1, nz
          q(i,j,k) = dble(l)*0.01d0 + dble(i)*1.0d0 &
            & + dble(j)*100.0d0+dble(k)*10000.0d0
        enddo
      enddo
    enddo
    ! 時間を明示的には与えない
    !   HistoryCreate の origin と interval に依存
    call HistoryPut('u', u)
    call HistoryPut('v', v)
    call HistoryPut('w', w)
    call HistoryPut('q', q)
  enddo

  call HistoryClose

end program histcopy
