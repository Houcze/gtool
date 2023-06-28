!= 出力設定のコピー
!= Copy configurations of output 
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historycopy.F90,v 1.3 2009-10-10 08:01:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryCopy1(hist_dest, file, hist_src, &
    & title, source, institution, &
    & origin, interval, &
    & conventions, gt_version)
    !
    ! 引数 *hist_src* の内容にコピーし, *hist_dest* へ返します. *hist_src*
    ! が与えられない場合は, 引数 *history* を与えずに呼び出した
    ! HistoryCreate の設定内容が参照されます.
    ! HistoryCreate と同様に, 出力の初期設定を行います. *file*
    ! は必ず与えなければならず, *hist_src* と同じファイルへ出力
    ! しようとする場合はエラーを生じます.
    ! HistoryAddVariable で設定される内容に関してはコピーされません.
    !
    ! それ以降の引数を与えることで, hist_src の設定を
    ! 上書きすることが可能です.
    !
    use gtool_history_internal, only: default
    use gtool_history_generic, only: HistoryInquire, HistoryCreate, HistoryPut
    use gtdata_generic, only: Inquire, Get_Attr, Copy_Attr, Get, Put
    use dc_error,   only: StoreError, DC_NOERR, GT_EARGSIZEMISMATCH
    use dc_present, only: present_select
    use dc_types,   only: STRING, TOKEN, DP
    use dc_date, only: EvalByUnit
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    implicit none
    type(GT_HISTORY), intent(out), target:: hist_dest
    character(*), intent(in):: file
    type(GT_HISTORY), intent(in), optional, target:: hist_src
    character(*), intent(in), optional:: title, source, institution
    real, intent(in), optional:: origin, interval
    character(*), intent(in), optional:: conventions, gt_version

    ! Internal Work
    type(GT_HISTORY), pointer:: src =>null()
    character(STRING) :: title_src, source_src, institution_src
    character(STRING) :: conventions_src, gt_version_src
    character(STRING), pointer:: dims(:) => null()
    integer          , pointer:: dimsizes(:) => null()
    character(STRING), pointer:: longnames(:) => null()
    character(STRING), pointer:: units(:)  => null()
    character(STRING), pointer:: xtypes(:) => null()
    real:: originw, intervalw
    integer  :: i, numdims
    logical        :: err
    real(DP),pointer :: dimvalue(:) => null()
    character(len = *),parameter:: subname = "HistoryCopy1"
  continue
    call BeginSub(subname, 'file=<%c>', c1=trim(file))

    if (present(hist_src)) then
      src => hist_src
    else
      src => default
    endif

#ifdef LIB_MPI
    if ( .not. src % mpi_gather &
      &  .or. ( src % mpi_gather .and. src % mpi_myrank == 0 ) ) then
#endif

    numdims = size(src % dimvars)

    call HistoryInquire(history=src, title=title_src, &
      & source=source_src, institution=institution_src, &
      & dims=dims, dimsizes=dimsizes, longnames=longnames, &
      & units=units, xtypes=xtypes, &
      & conventions=conventions_src, gt_version=gt_version_src)

    if ( present(origin) ) then
      originw = origin
    else
      originw = src % origin
!      originw = EvalByUnit( src % origin, '', src % unlimited_units_symbol )
    end if

    intervalw = src % interval
!    intervalw = EvalByUnit( src % interval, '', src % unlimited_units_symbol )
    if ( present(interval) ) then
      if ( interval /= 0.0 ) then
        intervalw = interval
      end if
    end if

    call HistoryCreate(file=trim(file), &
      & title=trim(present_select('', title_src, title)), &
      & source=trim(present_select('', source_src, source)), &
      & institution=trim(present_select('', institution_src, institution)), &
      & dims=dims, dimsizes=dimsizes, longnames=longnames, units=units, &
      & origin=originw, interval=intervalw, &
      & xtypes=xtypes, &
      & history=hist_dest, &
      & conventions=trim(present_select('', conventions_src, conventions)), &
      & gt_version=trim(present_select('', gt_version_src, gt_version)) )

    !
    ! 次元変数が属性を持っている場合のことも考え, 最後に直接
    ! hist_dst % dimvars へ copy_attr (gtvarcopyattrall) をかける.
    !
    do i = 1, numdims
      call Copy_Attr(hist_dest % dimvars(i), src % dimvars (i), global=.false.)
    end do

    ! dimvars を Get してみて, 値を持っているようならデータを与えてしまう.
    do i = 1, numdims
      if (dimsizes(i) == 0) cycle
      call Get(src % dimvars(i), dimvalue, err)
      if (err) cycle
      call HistoryPut(dims(i), dimvalue, hist_dest)
      deallocate(dimvalue)
    end do

    deallocate(dims, dimsizes, longnames, units, xtypes)

#ifdef LIB_MPI
      end if
#endif

    call EndSub(subname)
  end subroutine HistoryCopy1

  !-------------------------------------------------------------------

  subroutine HistoryCopy2(hist_dest, file, hist_src, &
    & title, source, institution, &
    & origin, interval, &
    & conventions, gt_version)
    !
    ! 使用方法は HistoryCopy と同様です. 
    !
    ! Usage is same as "HistoryCopy".
    !
    !--
    ! 総称名 Copy として提供するための関数です. 
    ! 機能は HistoryCopy1 と同じです. 
    !++
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_generic, only: HistoryCopy
    implicit none
    type(GT_HISTORY), intent(out), target:: hist_dest
    character(*), intent(in):: file
    type(GT_HISTORY), intent(in), optional, target:: hist_src
    character(*), intent(in), optional:: title, source, institution
    real, intent(in), optional:: origin, interval
    character(*), intent(in), optional:: conventions, gt_version

    ! Internal Work
    character(len = *),parameter:: subname = "HistoryCopy2"
  continue
    call BeginSub(subname, 'file=<%c>', c1=trim(file))
    call HistoryCopy(hist_dest, file, hist_src, &
      & title, source, institution, &
      & origin, interval, &
      & conventions, gt_version)
    call EndSub(subname)
  end subroutine HistoryCopy2
