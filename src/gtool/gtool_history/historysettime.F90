!= 時刻指定
!= Set time
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: historysettime.F90,v 1.6 2010-04-11 14:13:50 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistorySetTime(time, history, difftime, timed)
    !
    !== 時刻指定
    !
    ! 明示的に時刻指定を行なうためのサブルーチンです。
    ! このサブルーチンを用いる前に、HistoryCreate による初期設定が必要です。
    ! このサブルーチンを使用する事で HistoryCreate の *interval* が無効
    ! になるので注意してください。
    !
    !--
    ! 時刻を明示設定している状態で、巻き戻しを含めた時間設定。
    ! 前進している間は検索をしないようになっている。
    !++
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    use gtdata_generic, only: Slice, Put, Get
    use gtdata_types, only: GT_VARIABLE
    use dc_date_generic, only: DCDiffTimeCreate, operator(<), operator(>), &
      & EvalByUnit, min, max, DCDiffTimePutLine
    use dc_date_types, only: DC_DIFFTIME
    use dc_trace, only: BeginSub, EndSub, DbgMessage, Debug
    use dc_types, only: STRING, TOKEN, DP
#ifdef LIB_MPI
    use mpi
#endif
    implicit none
    real, intent(in), optional:: time
                              ! 時刻
                              ! 
                              ! ここで言う "時刻" とは、
                              ! HistoryCreate の *dims* で "0"
                              ! と指定されたものです。
                              ! もしも時刻が定義されていな
                              ! い場合は、 このサブルーチン
                              ! は何の効果も及ぼしません。
                              ! 
    type(GT_HISTORY), intent(inout), optional, target:: history
                              ! 出力ファイルの設定に関する情報を
                              ! 格納した構造体
                              ! 
                              ! ここに指定するものは、
                              ! HistoryCreate によって初期設定
                              ! されていなければなりません。
                              ! 
    type(DC_DIFFTIME), intent(in), optional:: difftime
                              ! 時刻 (dc_date_types#DC_DIFFTIME 型)
                              ! 
                              ! ここで言う "時刻" とは、
                              ! HistoryCreate の *dims* で "0"
                              ! と指定されたものです。
                              ! もしも時刻が定義されていな
                              ! い場合は、 このサブルーチン
                              ! は何の効果も及ぼしません。
                              ! 
    real(DP), intent(in), optional:: timed
                              ! 時刻 (倍精度実数型)
                              ! 
                              ! ここで言う "時刻" とは、
                              ! HistoryCreate の *dims* で "0"
                              ! と指定されたものです。
                              ! もしも時刻が定義されていな
                              ! い場合は、 このサブルーチン
                              ! は何の効果も及ぼしません。
                              ! 

    type(GT_HISTORY), pointer:: hst =>null()
    type(GT_VARIABLE):: var
    real, pointer:: buffer(:) =>null()
    real(DP):: dt
!    type(DC_DIFFTIME):: dt
    real(DP):: timew
    logical:: err, dbg_mode
#ifdef LIB_MPI
    integer:: err_mpi
#endif
    character(*), parameter:: subname = "HistorySetTime"
  continue
    call BeginSub(subname)

    if (present(history)) then
      hst => history
    else
      hst => default
    endif

    call Debug( dbg_mode ) 
    if ( dbg_mode ) then
      if ( present(difftime) ) then
        timew = EvalByUnit( difftime, '', hst % unlimited_units_symbol )
        call DbgMessage('time=%f', d = (/timew/) )
      elseif ( present(timed) ) then
        call DbgMessage('time=%f', d = (/timed/) )
      elseif ( present(time) ) then
        call DbgMessage('time=%r', r = (/time/) )
      end if
    end if

    if (hst % unlimited_index == 0) then
      goto 999
    endif
    var = hst % dimvars(hst % unlimited_index)
    hst % dim_value_written(hst % unlimited_index) = .true.

    if ( present(difftime) ) then
      dt = EvalByUnit( difftime, '', hst % unlimited_units_symbol )
      timew = dt 
    elseif ( present(timed) ) then
      dt = timed
!!$      call DCDiffTimeCreate( dt, &                          ! (out)
!!$        & real( timed ), '', hst % unlimited_units_symbol ) ! (in)
      timew = timed
    elseif ( present(time) ) then
      dt = time
!!$      call DCDiffTimeCreate( dt, &                 ! (out)
!!$        & time, '', hst % unlimited_units_symbol ) ! (in)
      timew = time
    end if

    if (      dt < hst % oldest &
      &  .or. dt > hst % newest &
      &  .or. hst % count(2) == 0 ) then

      hst % count(:) = maxval(hst % count(:)) + 1
      hst % newest = max(hst % newest, dt)
      hst % oldest = min(hst % oldest, dt)

#ifdef LIB_MPI
      if ( .not. hst % mpi_gather &
        &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

      call Slice(var, 1, start=hst % count(1), count=1)
      timew = dt
!      timew = EvalByUnit( dt, '', hst % unlimited_units_symbol )
      call Put(var, (/timew/), 1, err)
      if (err) call DumpError()

#ifdef LIB_MPI
      end if
#endif

      goto 999
    endif

#ifdef LIB_MPI
    if ( .not. hst % mpi_gather &
      &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

    call Slice(var, 1, start=1, count=hst % count(2))
    call Get(var, buffer, err)
    hst % count(1:1) = minloc(abs(buffer - timew))
    deallocate(buffer)

#ifdef LIB_MPI

      if ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) then
        call MPI_Bcast( hst % count(1:1), 1, MPI_INTEGER, 0, MPI_COMM_WORLD, err_mpi )
      end if

    elseif ( hst % mpi_gather .and. hst % mpi_myrank /= 0 ) then
      call MPI_Bcast( hst % count(1:1), 1, MPI_INTEGER, 0, MPI_COMM_WORLD, err_mpi )
    end if
#endif

999 continue
    call EndSub(subname)
  end subroutine HistorySetTime
