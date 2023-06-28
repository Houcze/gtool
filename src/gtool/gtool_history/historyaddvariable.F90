!= 変数定義
!= Definition of a variable
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: historyaddvariable.F90,v 1.5 2009-10-28 10:59:22 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  recursive subroutine HistoryAddVariable1( &
    & varname, dims, longname, units, &
    & xtype, time_average, average, history, err )
    !
    !== 変数定義
    !
    ! gtool4 データ内の変数の定義を行います。このサブルーチンを
    ! 用いる前に、 HistoryCreate による初期設定が必要です。
    !
    ! 既に gtool4 データが存在し、そのデータ内の変数と全く同じ
    ! 構造の変数を定義したい場合は HistoryCopyVariable を利用すると便利です。
    !
    ! *HistoryAddVariable* というサブルーチン名は 2 つの別々の
    ! サブルーチンの総称名です。上記のサブルーチンも参照ください。
    !
    !=== 時間平均について
    !
    ! gtool_history を用いた出力では、以下の条件を満たす場合に出力データを
    ! 時間方向に平均化して出力します。
    !
    ! * HistoryAddVariable の引数 *time_average* に .true. を与えている。
    ! * HistoryPut に時刻を示す引数 *time* (単精度実数型)、
    !   *timed* (倍精度実数型)、*difftime* (dc_date_types#DC_DIFFTIME)
    !   のいづれかを与えている。
    !
    ! HistoryPut に与えられたデータは gtool_history モジュール内部
    ! にいったん保管され、HistoryCreate の引数 *interval* の間隔を
    ! おいて出力が行われます。平均化は出力時から次の出力時の間の
    ! データを用いて行われます。時間間隔が一定でない場合、
    ! 重み付けをして出力されます。重み付けは以下のように行われます。
    !
    !   sum( <data> * <weight> ) / sum ( <weight> )
    !
    ! <data>   :: *array* または *value* に与えられたデータ
    ! <weight> :: 前回 HistoryPut が呼ばれた際の時刻と今回の時刻との差
    ! sum      :: 前回出力が行われた時刻から *interval* 経過するまでの値の和
    !
    use gtool_history_types, only: GT_HISTORY, &
      & GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR, &
      & GT_HISTORY_AVRDATA, GT_HISTORY_MPIVARINDEX
    use gtool_history_internal, only: default, lookup_dimension
    use gtool_history_generic, only: HistoryAddVariable, HistoryVarinfoCreate
    use gtdata_generic, only: Inquire, Create, Slice, Put_Attr, &
      & Put, PutLine
    use gtdata_types, only: GT_VARIABLE
    use netcdf, only: NF90_EBADDIM
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, HST_ENODEPENDTIME, &
      & HST_EMPINOAXISDATA
    use dc_string, only: CPrintf, JoinChar, StoA
    use dc_url, only: GT_ATMARK, UrlResolve
    use dc_present, only: present_and_true
    use dc_types, only: STRING
    use dc_date, only: DCDiffTimeCreate
    use dc_date_types, only: DC_DIFFTIME
    use dc_message, only: MessageNotify
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_types, only: STRING, TOKEN, DP
#ifdef LIB_MPI
    use mpi
#endif
    implicit none
    character(len = *), intent(in):: varname
                              ! 定義する変数の名前
                              !
                              ! 最大文字数は dc_type#TOKEN
                              !
    character(len = *), intent(in):: dims(:)
                              ! 変数が依存する次元の名前
                              !
                              ! 時間の次元は配列の最後に指定
                              ! しなければならない。
                              ! ここで指定するものは、
                              ! HistoryCreate にて dims で指定
                              ! されていなければならない。
                              !
                              ! もしもスカラー変数を作成
                              ! する場合には, サイズが 1 で
                              ! 中身が空の文字型配列,
                              ! すなわち <tt> (/''/) </tt>
                              ! を与えること.
                              !
    character(len = *), intent(in):: longname
                              ! 変数の記述的名称
                              !
                              ! 最大文字数は dc_types#STRING
                              !
    character(len = *), intent(in):: units
                              ! 変数の単位
                              !
                              ! 最大文字数は dc_types#STRING
                              !
    character(len = *), intent(in), optional:: xtype
                              ! 変数のデータ型
                              !
                              ! デフォルトはfloat (単精度実数型)
                              ! である。 有効なのは、
                              ! double (倍精度実数型)、 int
                              ! (整数型)、char (文字型)である。
                              ! 指定しない場合や、無効な型を指定した
                              ! 場合には、 float (単精度実数型)
                              ! となる。
                              !
    logical, intent(in), optional:: time_average
                              ! 出力データを時間平均する場合には
                              ! .true. を与えます。
                              ! デフォルトは .false. です。
                              !
                              ! If output data is averaged in time direction,
                              ! specify ".true.".
                              ! Default is ".false.".
                              !
    logical, intent(in), optional:: average
                              ! time_average の旧版.
                              ! Old version of "time_average"
    type(GT_HISTORY), intent(inout), optional, target:: history
                              ! 出力ファイルの設定に関する情報を
                              ! 格納した構造体
                              !
                              ! ここに指定するものは、
                              ! HistoryCreate によって初期設定
                              ! されていなければなりません。
                              !
    logical, intent(out), optional:: err
                              ! 例外処理用フラグ.
                              ! デフォルトでは, この手続き内でエラーが
                              ! 生じた場合, プログラムは強制終了します.
                              ! 引数 *err* が与えられる場合,
                              ! プログラムは強制終了せず, 代わりに
                              ! *err* に .true. が代入されます.
                              !
                              ! Exception handling flag.
                              ! By default, when error occur in
                              ! this procedure, the program aborts.
                              ! If this *err* argument is given,
                              ! .true. is substituted to *err* and
                              ! the program does not abort.
    type(GT_HISTORY), pointer:: hst =>null()
    type(GT_VARIABLE), pointer:: vwork(:) =>null(), dimvars(:) =>null()
    character(STRING):: fullname, url, cause_c
    integer, pointer:: count_work(:) =>null()
    integer, pointer:: var_avr_count_work(:) =>null()
    integer:: var_avr_length
    type(GT_HISTORY_AVRDATA), pointer:: var_avr_data_work(:) =>null()
    logical, pointer:: var_avr_firstput_work(:) =>null()
    real(DP), pointer:: var_avr_coefsum_work(:) =>null()
    real(DP), pointer:: var_avr_baseint_work(:) =>null()
    real(DP), pointer:: var_avr_prevtime_work(:) =>null()
!!$    type(DC_DIFFTIME), pointer:: var_avr_baseint_work(:) =>null()
!!$    type(DC_DIFFTIME), pointer:: var_avr_prevtime_work(:) =>null()
    character(STRING):: time_name, time_xtype, time_url
    type(GT_VARIABLE), pointer:: dimvars_work(:) =>null()
    logical, pointer:: dim_value_written_work(:) =>null()
    integer:: dimvars_size
    logical:: nv_exist, bnds_exist
    character(STRING):: nv_name_check, bnds_name_check
    character(*), parameter:: nv_suffix = '_nv'
    character(*), parameter:: bnds_suffix = '_bnds'
    type(GT_VARIABLE), pointer:: timevar
    integer, pointer:: dimord(:) =>null()
    integer:: nvars, numdims, i, stat
#ifdef LIB_MPI
    integer:: err_mpi
    type(GT_HISTORY_VARINFO), pointer:: work_mpi_varinfo(:) =>null()
    type(GT_HISTORY_MPIVARINDEX), pointer:: work_mpi_vars_index(:) =>null()
#endif
    character(*), parameter:: subname = "HistoryAddVariable1"
  continue
    call BeginSub(subname, 'name=<%a>, dims=<%a>, longname=<%a>, units=<%a>', &
      & ca=StoA(varname, JoinChar(dims), longname, units))
    stat = DC_NOERR
    cause_c = ''

    ! 操作対象決定
    !
    if (present(history)) then
      hst => history
    else
      hst => default
    endif

    ! 初期設定のチェック
    ! Check initialization
    !
    if ( .not. hst % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GT_HISTORY'
      goto 999
    end if

#ifdef LIB_MPI
    ! HistoryPutAxisMPI が既に呼ばれていることをチェック
    ! Check that "HistoryPutAxisMPI" is already called
    !
    if ( hst % mpi_gather .and. hst % mpi_myrank == 0 .and. &
      &  .not. hst % mpi_fileinfo % already_output ) then
      call MessageNotify('W', subname, &
        & 'Specify data of axes in whole area by "HistoryPutAxisMPI" explicitly ' // &
        & 'before "call HistoryAddVariable".' )
      stat = HST_EMPINOAXISDATA
      goto 999
    end if

    ! 変数表拡張 (MPI, ファイル統合版)
    !
    if ( hst % mpi_gather ) then
      if ( associated(hst % mpi_varinfo) ) then
        nvars = size(hst % mpi_varinfo(:))
        work_mpi_varinfo => hst % mpi_varinfo
        nullify( hst % mpi_varinfo )
        allocate( hst % mpi_varinfo(nvars + 1) )
        hst % mpi_varinfo(1:nvars) = work_mpi_varinfo
        deallocate( work_mpi_varinfo )

        nvars = size(hst % mpi_vars_index(:))
        work_mpi_vars_index => hst % mpi_vars_index
        nullify( hst % mpi_vars_index )
        allocate( hst % mpi_vars_index(nvars + 1) )
        hst % mpi_vars_index(1:nvars) = work_mpi_vars_index
        deallocate( work_mpi_vars_index )
      else
        nvars = 0
        allocate( hst % mpi_varinfo(nvars + 1) )
        allocate( hst % mpi_vars_index(nvars + 1) )
      end if
      call HistoryVarinfoCreate( &
        & hst % mpi_varinfo(nvars + 1), &          ! (out)
        & varname, dims, longname, units, xtype, & ! (in)
        & time_average, average )                  ! (in) optional
    end if
#endif

    ! 変数表拡張
    !
    if (associated(hst % vars)) then
      nvars = size(hst % vars(:))
      vwork => hst % vars
      count_work => hst % count
      nullify(hst % vars, hst % count)
      allocate(hst % vars(nvars + 1), hst % count(nvars + 1))
      hst % vars(1:nvars) = vwork(1:nvars)
      hst % count(1:nvars) = count_work(1:nvars)
      deallocate(vwork, count_work)
      count_work => hst % growable_indices
      nullify(hst % growable_indices)
      allocate(hst % growable_indices(nvars + 1))
      hst % growable_indices(1:nvars) = count_work(1:nvars)
      deallocate(count_work)

      ! 平均値出力のための変数表コピー
      ! Copy table of variables for average value output
      !
      var_avr_count_work => hst % var_avr_count
      nullify( hst % var_avr_count )
      allocate( hst % var_avr_count(nvars + 1) )
      hst % var_avr_count(1:nvars) = var_avr_count_work(1:nvars)
      deallocate( var_avr_count_work )

      var_avr_data_work => hst % var_avr_data
      nullify(hst % var_avr_data)
      allocate(hst % var_avr_data(nvars + 1))
      do i = 1, nvars
        hst % var_avr_data(i) % length = var_avr_data_work(i) % length
        allocate(hst % var_avr_data(i) % &
          & a_DataAvr(var_avr_data_work(i) % length))
        hst % var_avr_data(i) % a_DataAvr = var_avr_data_work(i) % a_DataAvr
        deallocate( var_avr_data_work(i) % a_DataAvr )
      end do
      deallocate( var_avr_data_work )

      var_avr_firstput_work => hst % var_avr_firstput
      nullify( hst % var_avr_firstput )
      allocate( hst % var_avr_firstput(nvars + 1) )
      hst % var_avr_firstput(1:nvars) = var_avr_firstput_work(1:nvars)
      deallocate( var_avr_firstput_work )

      var_avr_coefsum_work => hst % var_avr_coefsum
      nullify( hst % var_avr_coefsum )
      allocate( hst % var_avr_coefsum(nvars + 1) )
      hst % var_avr_coefsum(1:nvars) = var_avr_coefsum_work(1:nvars)
      deallocate( var_avr_coefsum_work )

      var_avr_baseint_work => hst % var_avr_baseint
      nullify( hst % var_avr_baseint )
      allocate( hst % var_avr_baseint(nvars + 1) )
      hst % var_avr_baseint(1:nvars) = var_avr_baseint_work(1:nvars)
      deallocate( var_avr_baseint_work )

      var_avr_prevtime_work => hst % var_avr_prevtime
      nullify( hst % var_avr_prevtime )
      allocate( hst % var_avr_prevtime(nvars + 1) )
      hst % var_avr_prevtime(1:nvars) = var_avr_prevtime_work(1:nvars)
      deallocate( var_avr_prevtime_work )
    else

      ! トリッキーだが, ここで count だけ 2 要素確保するのは,
      ! HistorySetTime による巻き戻しに備えるため.
      !
      allocate(hst % vars(1), hst % count(2), hst % growable_indices(1))
      hst % count(2) = 0
      allocate(hst % var_avr_count(1), hst % var_avr_data(1))
      allocate(hst % var_avr_firstput(1), hst % var_avr_coefsum(1))
      allocate(hst % var_avr_baseint(1), hst % var_avr_prevtime(1))
    endif
    nvars = size(hst % vars(:))
    hst % growable_indices(nvars) = 0
    if ( nvars < 2 ) then
      hst % count(nvars) = 0
    else
      hst % count(nvars) = hst % count(1)
    end if

    ! スカラー変数作成への対応
    !
    if (size(dims) == 1 .and. trim(dims(1)) == '') then
      numdims = 0
    else
      numdims = size(dims)
    end if

    allocate( dimvars(numdims) )
    allocate( dimord(numdims) )

#ifdef LIB_MPI
    if ( .not. hst % mpi_gather &
      &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

    ! 次元の ID の取得
    !
    do, i = 1, numdims
      ! hst 内で, 次元変数名 dim(i) に当たる次元変数の ID である
      ! hst % dimvar(i) を dimvars(i) に, 添字を dimord に
      !
      dimvars(i) = lookup_dimension( hst, dims(i), &   ! (in)
        &                            ord = dimord(i) ) ! (out)
      if (dimord(i) == 0) then
        stat = NF90_EBADDIM
        cause_c = CPrintf('"%c" dimension is not found.', c1=trim(dims(i)))
        goto 999
      end if
    end do

#ifdef LIB_MPI
      if ( hst % mpi_gather ) then
        call MPI_Bcast( dimord, numdims, MPI_INTEGER, 0, MPI_COMM_WORLD, err_mpi )
      end if

    elseif ( hst % mpi_gather .and. hst % mpi_myrank /= 0 ) then
      call MPI_Bcast( dimord, numdims, MPI_INTEGER, 0, MPI_COMM_WORLD, err_mpi )
    end if
#endif

    ! 変数添字次元を決定
    !
    do, i = 1, numdims
      ! 無制限次元の添字と一致する場合に,
      ! その添字を hst % growable_indices(nvars) に
      !
      if (dimord(i) == hst % unlimited_index) then
        hst % growable_indices(nvars) = i
      endif
    enddo

#ifdef LIB_MPI
    if ( .not. hst % mpi_gather &
      &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

    ! 変数作成
    !
    call Inquire(hst % dimvars(1), url=url)
    fullname = UrlResolve((GT_ATMARK // trim(varname)), trim(url))
    call Create(hst % vars(nvars), trim(fullname), dimvars, xtype=xtype)
    if ( associated(dimvars) ) deallocate( dimvars )

    ! 拡張可能次元があったらそれをサイズ 1 に拡張しておく
    !
    if (hst % growable_indices(nvars) /= 0) then
      call Slice(hst % vars(nvars), hst % growable_indices(nvars), &
        & start=1, count=1, stride=1)
    endif
    call Put_Attr(hst % vars(nvars), 'long_name', longname)
    call Put_Attr(hst % vars(nvars), 'units', units)

#ifdef LIB_MPI
    end if
#endif

    ! 平均処理に関する情報管理
    !
    if (      present_and_true( time_average ) &
      &  .or. present_and_true( average ) ) then

      hst % var_avr_count(nvars) = 0

#ifdef LIB_MPI
      if ( .not. hst % mpi_gather &
        &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

      ! 情報の取得
      ! Get Information
      !
      timevar => hst % dimvars( hst % unlimited_index )
      call Inquire( &
        & var = timevar, &                    ! (in)
        & name = time_name, url = time_url, & ! (out)
        & xtype = time_xtype )                ! (out)

#ifdef LIB_MPI
        if ( hst % mpi_gather ) then
          call MPI_Bcast( time_name,  STRING, MPI_CHARACTER, 0, MPI_COMM_WORLD, err_mpi )
          call MPI_Bcast( time_url,   STRING, MPI_CHARACTER, 0, MPI_COMM_WORLD, err_mpi )
          call MPI_Bcast( time_xtype, STRING, MPI_CHARACTER, 0, MPI_COMM_WORLD, err_mpi )
        end if

      elseif ( hst % mpi_gather .and. hst % mpi_myrank /= 0 ) then

        call MPI_Bcast( time_name,  STRING, MPI_CHARACTER, 0, MPI_COMM_WORLD, err_mpi )
        call MPI_Bcast( time_url,   STRING, MPI_CHARACTER, 0, MPI_COMM_WORLD, err_mpi )
        call MPI_Bcast( time_xtype, STRING, MPI_CHARACTER, 0, MPI_COMM_WORLD, err_mpi )
      end if
#endif


      ! 変数のデータ数の取得
      !
#ifdef LIB_MPI
      if ( .not. hst % mpi_gather ) then
#endif
      call Inquire(hst % vars(nvars), size = var_avr_length )

#ifdef LIB_MPI
      else
        var_avr_length = 1
        do i = 1, numdims
          if ( hst % unlimited_index == dimord(i) ) cycle
          var_avr_length = &
            & var_avr_length * hst % mpi_dimdata_each( dimord(i) ) % length
        end do
      end if
#endif

      ! 割り付け
      !
      hst % var_avr_data(nvars) % length = var_avr_length
      allocate(hst % var_avr_data(nvars) % a_DataAvr(var_avr_length))
      hst % var_avr_data(nvars) % a_DataAvr = 0.0_DP

      ! デフォルト値設定
      !
      hst % var_avr_firstput = .true.
      hst % var_avr_coefsum(nvars) = 0.0_DP
      hst % var_avr_baseint(nvars) = 0.0_DP
!!$      call DCDiffTimeCreate( &
!!$        & hst % var_avr_baseint(nvars), & ! (out)
!!$        & sec = 0.0_DP )                   ! (in)
      hst % var_avr_prevtime(nvars) = hst % var_avr_baseint(nvars)

      ! 時間次元情報の取得
      !
      if ( hst % growable_indices(nvars) < 1 ) then
        stat = HST_ENODEPENDTIME
        cause_c = trim(varname)
        goto 999
      end if

#ifdef LIB_MPI
      if ( .not. hst % mpi_gather &
        &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

      ! 時間次元への属性 "bounds" の追加
      !
      call Put_Attr( var = timevar, &              ! (inout)
        & name = 'bounds', &                       ! (in)
        & value = trim(time_name) // bnds_suffix ) ! (in)

      ! 変数 "varname" への属性 "cell_methods" の追加
      !
      call Put_Attr( var = hst % vars(nvars), & ! (inout)
        & name = 'cell_methods', &              ! (in)
        & value = trim(time_name) // ': mean' ) ! (in)

      ! "time_nv" 次元の作成 (既に作成されていたら何もしない)
      !
      dimvars_size = size( hst % dimvars )
      nv_exist = .false.
      do i = 1, dimvars_size
        call Inquire( &
          & var = hst % dimvars(i), & ! (in)
          & name = nv_name_check )    ! (out)
        if ( trim(time_name) // trim(nv_suffix) == trim(nv_name_check) ) then
          nv_exist = .true.
          exit
        end if
      end do

      if ( .not. nv_exist ) then
        dimvars_work => hst % dimvars
        dim_value_written_work => hst % dim_value_written
        nullify(hst % dimvars, hst % dim_value_written)
        allocate(hst % dimvars(dimvars_size + 1))
        allocate(hst % dim_value_written(dimvars_size + 1))
        hst % dimvars(1:dimvars_size) = dimvars_work(1:dimvars_size)
        hst % dim_value_written(1:dimvars_size) = dim_value_written_work(1:dimvars_size)
        deallocate(dimvars_work)
        deallocate(dim_value_written_work)

        call Create( &
          & var = hst % dimvars(dimvars_size + 1), &   ! (out)
          & url = trim(time_url) // trim(nv_suffix), & ! (in)
          & length = 2, xtype = 'integer' )            ! (in)

        hst % time_nv_index = dimvars_size + 1

        call Put_Attr( var = hst % dimvars(dimvars_size + 1), & ! (inout)
          & name = 'long_name', &                               ! (in)
          & value = 'number of vertices of time')               ! (in)
        call Put_Attr( var = hst % dimvars(dimvars_size + 1), & ! (inout)
          & name = 'units', value = '1' )                       ! (in)

        call Put( var = hst % dimvars(dimvars_size + 1), & ! (inout)
          & value = (/1, 2/) )                             ! (in)

        hst % dim_value_written(dimvars_size + 1) = .true.
      end if

      ! "time_bnds" 変数の作成 (既に作成されていたら何もしない)
      !
      bnds_exist = .false.
      do i = 1, nvars
        call Inquire( &
          & var = hst % vars(i), &   ! (in)
          & name = bnds_name_check ) ! (out)
        if ( trim(time_name) // trim(bnds_suffix) == trim(bnds_name_check) ) then
          bnds_exist = .true.
          exit
        end if
      end do

#ifdef LIB_MPI
        if ( hst % mpi_gather ) then
          call MPI_Bcast( hst % time_nv_index, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, err_mpi )
          call MPI_Bcast( bnds_exist, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, err_mpi )
        end if

      elseif ( hst % mpi_gather .and. hst % mpi_myrank /= 0 ) then

        call MPI_Bcast( hst % time_nv_index, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, err_mpi )
        call MPI_Bcast( bnds_exist, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, err_mpi )
      end if
#endif

      if ( associated(dimord) )  deallocate( dimord )

      if ( .not. bnds_exist ) then
        call HistoryAddVariable( &
          &  history = hst, &                                 ! (inout)
          &  varname = trim(time_name) // trim(bnds_suffix), &
          &     dims = StoA( trim(time_name) // trim(nv_suffix), &
          &                  trim(time_name) ), &             ! (in)
          & longname = 'bounds of time', &                    ! (in)
          & units = hst % unlimited_units, &                  ! (in)
          & xtype = time_xtype )                              ! (in)
      end if

    ! 平均処理が不要な場合
    !
    else
      hst % var_avr_count(nvars) = -1

      ! 割り付け
      !
      var_avr_length = 1
      hst % var_avr_data(nvars) % length = var_avr_length
      allocate(hst % var_avr_data(nvars) % a_DataAvr(var_avr_length))
      hst % var_avr_data(nvars) % a_DataAvr = 0.0_DP

      ! デフォルト値設定
      !
      hst % var_avr_firstput = .true.
      hst % var_avr_coefsum(nvars) = 0.0_DP
      hst % var_avr_baseint(nvars) = 0.0_DP
!!$      call DCDiffTimeCreate( &
!!$        & hst % var_avr_baseint(nvars), & ! (out)
!!$        & sec = 0.0_DP )                   ! (in)
      hst % var_avr_prevtime(nvars) = hst % var_avr_baseint(nvars)

    end if

    ! 終了処理, 例外処理
    ! Termination and Exception handling
    !
999 continue
    if ( associated(dimvars) ) deallocate( dimvars )
    if ( associated(dimord) )  deallocate( dimord )
    call StoreError(stat, subname, err, cause_c)
    call EndSub(subname)
  end subroutine HistoryAddVariable1

  subroutine HistoryAddVariable2( &
    & varinfo, history, err )
    !
    !== 変数定義
    !
    ! gtool4 データ内の変数の定義を行います。このサブルーチンを
    ! 用いる前に、 HistoryCreate による初期設定が必要です。
    !
    ! 既に gtool4 データが存在し、そのデータ内の変数と全く同じ
    ! 構造の変数を定義したい場合は HistoryCopyVariable を利用すると
    ! 便利です。
    !
    ! *HistoryAddVariable* というサブルーチン名は 2 つの別々の
    ! サブルーチンの総称名です。下記のサブルーチンも参照ください。
    !
    use dc_string, only: JoinChar
    use gtool_history_generic, only: HistoryAddVariable
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default, append_attrs
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_types, only: STRING, TOKEN, DP
    implicit none
    type(GT_HISTORY_VARINFO), intent(in)             :: varinfo
                              ! 変数情報を格納した構造体
                              !
                              ! ここに指定するものは、
                              ! HistoryVarinfoCreate によって
                              ! 初期設定されていなければなりません。
                              !
    type(GT_HISTORY),         intent(inout), optional:: history
                              ! 出力ファイルの設定に関する情報を
                              ! 格納した構造体
                              !
                              ! ここに指定するものは、
                              ! HistoryCreate によって初期設定
                              ! されていなければなりません。
                              !
    logical, intent(out), optional:: err
                              ! 例外処理用フラグ.
                              ! デフォルトでは, この手続き内でエラーが
                              ! 生じた場合, プログラムは強制終了します.
                              ! 引数 *err* が与えられる場合,
                              ! プログラムは強制終了せず, 代わりに
                              ! *err* に .true. が代入されます.
                              !
                              ! Exception handling flag.
                              ! By default, when error occur in
                              ! this procedure, the program aborts.
                              ! If this *err* argument is given,
                              ! .true. is substituted to *err* and
                              ! the program does not abort.

    character(len = *), parameter:: subname = "HistoryAddVariable2"
  continue
    call BeginSub(subname, 'varname=<%c>, dims=<%c>, longname=<%c>', &
      & c1=trim(varinfo % name), c2=trim(JoinChar(varinfo % dims)),   &
      & c3=trim(varinfo % longname)                               )
    call HistoryAddVariable( &
      & history = history, &              ! (inout)
      & varname = varinfo % name, &       ! (in)
      & dims = varinfo % dims, &          ! (in)
      & longname = varinfo % longname, &  ! (in)
      & units = varinfo % units, &        ! (in)
      & xtype = varinfo % xtype, &        ! (in)
      & time_average = varinfo % time_average, & ! (in) optional
      & err = err )                              ! (out) optional
    if (associated( varinfo % attrs )) then
      call append_attrs( varinfo % name, varinfo % attrs, history )
    end if
    call EndSub(subname)
  end subroutine HistoryAddVariable2
