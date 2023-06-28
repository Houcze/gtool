!= GT_HISTORY_ATTR 型変数への問い合わせ
!= Inquire for a "GT_HISTORY" variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyinquire.f90,v 1.3 2009-10-10 08:01:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryInquire1(history, err, file, title, source, &
    & dims, dimsizes, longnames, units, xtypes, &
    & institution, origin, interval, newest, oldest, &
    & conventions, gt_version, &
    & axes, varinfo )
    !
    !== GT_HISTORY 型変数への問い合わせ
    !
    ! HistoryCreate や HistoryAddVariable などで設定した値の
    ! 参照を行います。
    !
    ! file, title, source, institution, origin, interval,
    ! conventions, gt_version, dims, dimsizes, longnames, units,
    ! xtypes に関しては HistoryCreate を参照してください。
    !
    ! title, source, institution, origin, interval, conventions, gt_version
    ! に関しては、値が得られなかった場合は "unknown" が返ります。
    !
    ! dims, dimsizes, longnames, units, xtypes に関してはポインタに
    ! 値を返すため、必ずポインタを空状態にしてから与えてください。
    !
    ! axes と varinfo にはそれぞれ座標軸情報と変数情報を返します。
    ! 将来的には全ての属性の値も一緒に返す予定ですが、現在は
    ! long_name, units, xtype のみが属性の値として返ります。
    !
    ! *HistoryInquire* は 2 つのサブルーチンの総称名です。
    ! HistoryCreate で *history* を与えなかった場合の問い合わせに関しては
    ! 上記のサブルーチンを参照してください。
    !
    !=== エラー
    !
    ! 以下の場合に、このサブルーチンはエラーを生じプログラムを終了させます。
    ! ただし、*err* 引数を与える場合、この引数に <tt>.true.</tt> を
    ! 返し、プログラムは続行します。
    !
    ! - *history* が HistoryCreate によって初期設定されていない場合
    ! - HistoryAddVariable や HistoryCopyVariable 等による変数定義が
    !   一度も行われていない GT_HISTORY 変数に対して引数 varinfo
    !   を渡した場合
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    use gtdata_generic, only: Inquire, Get_Attr, Open, Close
    use gtdata_types, only: GT_VARIABLE
    use dc_url, only: UrlSplit
    use dc_error, only: StoreError, DC_NOERR, GT_EBADHISTORY, NF90_ENOTVAR
    use dc_date, only: EvalByUnit
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_types, only: STRING, TOKEN, DP
    implicit none
    type(GT_HISTORY), intent(in):: history
    logical, intent(out), optional :: err
    character(*), intent(out), optional:: file, title, source, institution
    real,intent(out), optional:: origin, interval
    real,intent(out), optional:: newest ! 最新の時刻
    real,intent(out), optional:: oldest ! 最初の時刻
    character(*), intent(out), optional:: conventions, gt_version
    character(*), pointer, optional:: dims(:) ! (out)
    integer,pointer, optional:: dimsizes(:) ! (out)
    character(*), pointer, optional:: longnames(:) ! (out)
    character(*), pointer, optional:: units(:) ! (out)
    character(*), pointer, optional:: xtypes(:) ! (out)
    type(GT_HISTORY_AXIS), pointer, optional :: axes(:) ! (out)
    type(GT_HISTORY_VARINFO), pointer, optional :: varinfo(:) ! (out)

    ! Internal Work
    character(STRING)      :: url, cause_c
    character(TOKEN)       :: unknown_mes = 'unknown'
    integer          :: i, j, numdims, numvars, alldims, stat
    logical                :: growable
    type(GT_VARIABLE)      :: dimvar
    character(*), parameter:: subname = "HistoryInquire1"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c =  ''
    if (.not. associated(history % dimvars) .or. &
      & size(history % dimvars) < 1) then
      stat = GT_EBADHISTORY
      goto 999
    end if

    if (present(file)) then
      call Inquire(history % dimvars(1), url=url)
      call UrlSplit(fullname=url, file=file)
    end if
    if (present(title)) then
      call Get_Attr(history % dimvars(1), '+title', title, trim(unknown_mes))
    end if
    if (present(source)) then
      call Get_Attr(history % dimvars(1), '+source', source, trim(unknown_mes))
    end if
    if (present(institution)) then
      call Get_Attr(history % dimvars(1), '+institution', institution, trim(unknown_mes))
    end if

    if (present(origin)) then
      origin = history % origin
!      origin = EvalByUnit( history % origin, '', history % unlimited_units_symbol )
    end if
    if (present(interval)) then
      interval = history % interval
!      interval = EvalByUnit( history % interval, '', history % unlimited_units_symbol )
    end if
    if (present(newest)) then
      newest = history % newest
!      newest = EvalByUnit( history % newest, '', history % unlimited_units_symbol )
    end if
    if (present(oldest)) then
      oldest = history % oldest
!      oldest = EvalByUnit( history % oldest, '', history % unlimited_units_symbol )
    end if
    if (present(conventions)) then
      call Get_Attr(history % dimvars(1), '+Conventions', conventions, trim(unknown_mes))
    end if
    if (present(gt_version)) then
      call Get_Attr(history % dimvars(1), '+gt_version', gt_version, trim(unknown_mes))
    end if
    if (present(dims)) then
      numdims = size(history % dimvars)
      allocate(dims(numdims))
      do i = 1, numdims
        call Inquire(history % dimvars(i), name=dims(i))
      end do
    end if
    if (present(dimsizes)) then
      numdims = size(history % dimvars)
      allocate(dimsizes(numdims))
      do i = 1, numdims
        call Inquire(history % dimvars(i), size=dimsizes(i), growable=growable)
        if (growable) dimsizes(i) = 0
      end do
    end if
    if (present(longnames)) then
      numdims = size(history % dimvars)
      allocate(longnames(numdims))
      do i = 1, numdims
        call Get_attr(history % dimvars(i), 'long_name', &
          & longnames(i), 'unknown')
      end do
    end if
    if (present(units)) then
      numdims = size(history % dimvars)
      allocate(units(numdims))
      do i = 1, numdims
        call Get_attr(history % dimvars(i), 'units', &
          & units(i), 'unknown')
      end do
    end if
    if (present(xtypes)) then
      numdims = size(history % dimvars)
      allocate(xtypes(numdims))
      do i = 1, numdims
        call Inquire(history % dimvars(i), xtype=xtypes(i))
      end do
    end if
    if (present(axes)) then
      numvars = size(history % dimvars)
      allocate(axes(numvars))
      do i = 1, numvars
        call Inquire(history % dimvars(i), &
          & allcount=axes(i) % length, &
          & xtype=axes(i) % xtype, name=axes(i) % name)
        call Get_Attr(history % dimvars(i), 'long_name', &
          & axes(i) % longname, 'unknown')
        call Get_Attr(history % dimvars(i), 'units', &
          & axes(i) % units, 'unknown')

        ! 属性 GT_HISTORY_ATTR はまだ取得できない
        !
        ! するためには, 属性名に対して様々な型が存在しうると
        ! 考えられるため, get_attr (gtdata_generic および gtdata_netcdf_generic)
        ! に err 属性を装備させ, 取得できない際にエラーを
        ! 返してもらわなければならないだろう.

      end do
    end if

    if (present(varinfo)) then
      if (.not. associated(history % vars) .or. &
        & size(history % vars) < 1) then
        stat = NF90_ENOTVAR
        goto 999
      end if

      numvars = size(history % vars)
      allocate(varinfo(numvars))
      do i = 1, numvars
        call Inquire(history % vars(i), alldims=alldims, &
          & xtype=varinfo(i) % xtype, name=varinfo(i) % name)
        call Get_Attr(history % vars(i), 'long_name', &
          & varinfo(i) % longname, 'unknown')
        call Get_Attr(history % vars(i), 'units', &
          & varinfo(i) % units, 'unknown')

        ! 属性 GT_HISTORY_ATTR はまだ取得できない
        !
        ! するためには, 属性名に対して様々な型が存在しうると
        ! 考えられるため, get_attr (gtdata_generic および gtdata_netcdf_generic)
        ! に err 属性を装備させ, 取得できない際にエラーを
        ! 返してもらわなければならないだろう.

        allocate(varinfo(i) % dims(alldims))
        do j = 1, alldims
          call Open(var=dimvar, source_var=history % vars(i), &
            & dimord=j, count_compact=.true.)
          call Inquire(dimvar, name=varinfo(i) % dims(j))
          call Close(dimvar)
        end do

        varinfo(i) % initialized = .true.

      end do
    end if
999 continue
    call StoreError(stat, subname, err, cause_c=cause_c)
    call EndSub(subname)
  end subroutine HistoryInquire1

  !-------------------------------------------------------------------

  subroutine HistoryInquire2(history, err, file, title, source, &
    & dims, dimsizes, longnames, units, xtypes, &
    & institution, origin, interval, newest, oldest, &
    & conventions, gt_version, &
    & axes, varinfo )
    !
    !== GT_HISTORY 型変数への問い合わせ
    !
    ! HistoryCreate で *history* を指定しなかった場合はこちらの
    ! サブルーチンで問い合わせを行います。
    ! *history* には必ず "<tt>default</tt>" という文字列を与えてください。
    !
    ! *HistoryInquire* は 2 つのサブルーチンの総称名です。
    ! 各引数の情報に関しては下記のサブルーチンを参照してください。
    !
    !--
    ! HistoryInquire1 と同機能だが, こちらは
    ! history に "default" という文字列を代入することで,
    ! デフォルトで出力されるファイル名 (HistoryCreate で
    ! history 引数を与えない場合のファイル名) が返る.
    !++
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    use gtool_history_generic, only: HistoryInquire
    use dc_error, only: StoreError, DC_NOERR, NF90_EINVAL
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_types, only: STRING, TOKEN, DP
    implicit none
    character(*), intent(in):: history
    logical, intent(out), optional :: err
    character(*), intent(out), optional:: file, title, source, institution
    real,intent(out), optional:: origin, interval, newest, oldest
    character(*), intent(out), optional:: conventions, gt_version
    character(*), pointer, optional:: dims(:) ! (out)
    integer,pointer, optional:: dimsizes(:) ! (out)
    character(*), pointer, optional:: longnames(:) ! (out)
    character(*), pointer, optional:: units(:) ! (out)
    character(*), pointer, optional:: xtypes(:) ! (out)
    type(GT_HISTORY_AXIS), pointer, optional :: axes(:) ! (out)
    type(GT_HISTORY_VARINFO), pointer, optional :: varinfo(:) ! (out)
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = "HistoryInquire2"
  continue
    call BeginSub(subname, "history=%c", c1=trim(history))
    stat = DC_NOERR
    cause_c = ''
    if (trim(history) /= 'default') then
      stat = NF90_EINVAL
      cause_c = 'history="' // trim(history) // '"'
      goto 999
    end if
    call HistoryInquire(default, err, file, title, source, &
      & dims, dimsizes, longnames, units, xtypes, &
      & institution, origin, interval, newest, oldest, &
      & conventions, gt_version, &
      & axes, varinfo )
999 continue
    call StoreError(stat, subname, cause_c=cause_c)
    call EndSub(subname)
  end subroutine HistoryInquire2

  !-------------------------------------------------------------------

  subroutine HistoryInquire3(history, err, file, title, source, &
    & dims, dimsizes, longnames, units, xtypes, &
    & institution, origin, interval, newest, oldest, &
    & conventions, gt_version, &
    & axes, varinfo )
    !
    ! 使用方法は HistoryInquire と同様です.
    !
    ! Usage is same as "HistoryInquire".
    !
    !--
    ! 総称名 Inquire として提供するためのサブルーチンです.
    ! 機能は HistoryInquire1 と同じです.
    !++
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_generic, only: HistoryInquire
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    type(GT_HISTORY), intent(in):: history
    logical, intent(out), optional :: err
    character(*), intent(out), optional:: file, title, source, institution
    real,intent(out), optional:: origin, interval
    real,intent(out), optional:: newest ! 最新の時刻
    real,intent(out), optional:: oldest ! 最初の時刻
    character(*), intent(out), optional:: conventions, gt_version
    character(*), pointer, optional:: dims(:) ! (out)
    integer,pointer, optional:: dimsizes(:) ! (out)
    character(*), pointer, optional:: longnames(:) ! (out)
    character(*), pointer, optional:: units(:) ! (out)
    character(*), pointer, optional:: xtypes(:) ! (out)
    type(GT_HISTORY_AXIS), pointer, optional :: axes(:) ! (out)
    type(GT_HISTORY_VARINFO), pointer, optional :: varinfo(:) ! (out)

    character(*), parameter:: subname = "HistoryInquire3"
  continue
    call BeginSub(subname)
    call HistoryInquire(history, err, file, title, source, &
      & dims, dimsizes, longnames, units, xtypes, &
      & institution, origin, interval, newest, oldest, &
      & conventions, gt_version, &
      & axes, varinfo )
    call EndSub(subname)
  end subroutine HistoryInquire3

  !-------------------------------------------------------------------

  subroutine HistoryInquire4(history, err, file, title, source, &
    & dims, dimsizes, longnames, units, xtypes, &
    & institution, origin, interval, newest, oldest, &
    & conventions, gt_version, &
    & axes, varinfo )
    !
    ! 使用方法は HistoryInquire と同様です.
    !
    ! Usage is same as "HistoryInquire".
    !
    !--
    ! 総称名 Inquire として提供するためのサブルーチンです.
    ! 機能は HistoryInquire2 と同じです.
    !++
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_generic, only: HistoryInquire
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    character(*), intent(in):: history
    logical, intent(out), optional :: err
    character(*), intent(out), optional:: file, title, source, institution
    real,intent(out), optional:: origin, interval, newest, oldest
    character(*), intent(out), optional:: conventions, gt_version
    character(*), pointer, optional:: dims(:) ! (out)
    integer,pointer, optional:: dimsizes(:) ! (out)
    character(*), pointer, optional:: longnames(:) ! (out)
    character(*), pointer, optional:: units(:) ! (out)
    character(*), pointer, optional:: xtypes(:) ! (out)
    type(GT_HISTORY_AXIS), pointer, optional :: axes(:) ! (out)
    type(GT_HISTORY_VARINFO), pointer, optional :: varinfo(:) ! (out)
    character(*), parameter:: subname = "HistoryInquire4"
  continue
    call BeginSub(subname)
    call HistoryInquire(history, err, file, title, source, &
      & dims, dimsizes, longnames, units, xtypes, &
      & institution, origin, interval, newest, oldest, &
      & conventions, gt_version, &
      & axes, varinfo )
    call EndSub(subname)
  end subroutine HistoryInquire4
