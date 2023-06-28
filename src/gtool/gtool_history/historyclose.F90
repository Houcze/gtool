!= 出力設定のコピー
!= Copy configurations of output 
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: historyclose.F90,v 1.2 2009-05-25 09:45:20 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryClose( history, quiet, err )
    !
    !== gtool4 データの終了処理
    !
    ! HistoryCreate で始まったデータ出力の終了処理をおこなうものです.
    ! プログラム内で HistoryCreate を用いた場合, プログラムを終了する
    ! 前に必ずこのサブルーチンを呼んで下さい.
    !
    use gtool_history_types, only: GT_HISTORY
    use gtool_history_internal, only: default, set_fake_dim_value
    use gtool_history_generic, only: HistoryVarinfoClear
    use gtdata_generic, only: Close, Inquire
    use gtdata_types, only: GT_VARIABLE
    use dc_message, only: MessageNotify
    use dc_url, only: UrlSplit
    use dc_present, only: present_and_true
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT
    use dc_types, only: STRING, DP
    use dc_date_types, only: UNIT_SYMBOL_ERR
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    type(GT_HISTORY), intent(inout), optional, target:: history
                              ! 出力ファイルの設定に関する情報を
                              ! 格納した構造体
                              ! 
                              ! ここに指定するものは,
                              ! HistoryCreate によって初期設定
                              ! されていなければなりません.
                              ! 
    logical, intent(in), optional:: quiet
                              ! .true. を与えた場合, 
                              ! メッセージ出力が抑制されます. 
                              !
                              ! If ".true." is given, 
                              ! messages are suppressed. 
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
    character(STRING):: url, file
    integer:: i, v_size
    integer:: stat
    character(STRING):: cause_c
    character(len = *), parameter:: subname = "HistoryClose"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = ""

    if (present(history)) then
      hst => history
    else
      hst => default
    endif

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
    !-----------------------------------------------------------------
    if ( .not. hst % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GT_HISTORY'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  メッセージ出力用にファイル名取得
    !  Get filename for output messages
    !-----------------------------------------------------------------
#ifdef LIB_MPI
    if ( .not. hst % mpi_gather &
      &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

    if ( .not. present_and_true(quiet) ) then
      call Inquire( hst % dimvars(1), & ! (in)
        & url = url )                   ! (out)
      call UrlSplit( fullname = url, &  ! (in)
        & file = file )                 ! (out)
    end if

#ifdef LIB_MPI
    endif
#endif

    !-----------------------------------------------------------------
    !  変数のクローズ
    !  Close variables
    !-----------------------------------------------------------------
#ifdef LIB_MPI
    if ( .not. hst % mpi_gather &
      &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

    v_size = size(hst % dimvars)
    do, i = 1, v_size
      if (.not. hst % dim_value_written(i)) &
        call set_fake_dim_value(hst, i)
      call Close(hst % dimvars(i))
    enddo
    v_size = size(hst % vars)
    do, i = 1, v_size
      call Close(hst % vars(i))
    enddo

#ifdef LIB_MPI
    endif
#endif

    deallocate(hst % dimvars)
    v_size = size(hst % vars)

    !-----------------------------------------------------------------
    !  設定のクリア
    !  Clear configurations
    !-----------------------------------------------------------------
    hst % unlimited_index = 0
    hst % unlimited_units = ''
    hst % unlimited_units_symbol = UNIT_SYMBOL_ERR
    if (associated(hst % dim_value_written)) deallocate(hst % dim_value_written)
    if (associated(hst % vars)) deallocate(hst % vars)
    if (associated(hst % growable_indices)) deallocate(hst % growable_indices)
    if (associated(hst % count)) deallocate(hst % count)
    if (associated(hst % var_avr_count)) deallocate(hst % var_avr_count)
    do, i = 1, v_size
      if (associated(hst % var_avr_data(i) % a_DataAvr)) deallocate(hst % var_avr_data(i) % a_DataAvr)
    enddo
    if (associated(hst % var_avr_data)) deallocate(hst % var_avr_data)
    if (associated(hst % var_avr_firstput)) deallocate(hst % var_avr_firstput)
    if (associated(hst % var_avr_coefsum)) deallocate(hst % var_avr_coefsum)
    if (associated(hst % var_avr_baseint)) deallocate(hst % var_avr_baseint)
    if (associated(hst % var_avr_prevtime)) deallocate(hst % var_avr_prevtime)
    hst % time_bnds = 0.0_DP
    hst % time_bnds_output_count = 0
    hst % time_nv_index = 0
    hst % origin_setting = .false.

#ifdef LIB_MPI

    if ( associated( hst % mpi_fileinfo % axes ) ) deallocate( hst % mpi_fileinfo % axes )
    if ( associated( hst % mpi_fileinfo ) ) deallocate( hst % mpi_fileinfo )

    v_size = size(hst % mpi_dimdata_all)
    do, i = 1, v_size
      if ( associated( hst % mpi_dimdata_all(i) % a_Axis ) ) deallocate( hst % mpi_dimdata_all(i) % a_Axis )
      if ( associated( hst % mpi_dimdata_all(i) % attrs ) ) deallocate( hst % mpi_dimdata_all(i) % attrs )
    enddo
    if ( associated( hst % mpi_dimdata_all ) ) deallocate( hst % mpi_dimdata_all )

    v_size = size(hst % mpi_dimdata_each)
    do, i = 1, v_size
      if ( associated( hst % mpi_dimdata_each(i) % a_Axis ) ) deallocate( hst % mpi_dimdata_each(i) % a_Axis )
    enddo
    if ( associated( hst % mpi_dimdata_each ) ) deallocate( hst % mpi_dimdata_each )

    if ( associated( hst % mpi_gthr_info ) ) then
      v_size = size(hst % mpi_gthr_info)
      do, i = 1, v_size
        if ( associated( hst % mpi_gthr_info(i) % index_all ) ) deallocate( hst % mpi_gthr_info(i) % index_all )
        if ( associated( hst % mpi_gthr_info(i) % length ) )    deallocate( hst % mpi_gthr_info(i) % length )
      end do
      deallocate( hst % mpi_gthr_info )
    end if

    if ( associated(hst % mpi_varinfo) ) then
      v_size = size(hst % mpi_varinfo)
      do, i = 1, v_size
        call HistoryVarinfoClear( hst % mpi_varinfo(i), err )
      end do
      deallocate( hst % mpi_varinfo )
    end if

    if ( associated(hst % mpi_vars_index) ) then
      v_size = size(hst % mpi_vars_index)
      do, i = 1, v_size
        if ( associated( hst % mpi_vars_index(i) % each2all ) ) deallocate( hst % mpi_vars_index(i) % each2all )
        if ( associated( hst % mpi_vars_index(i) % allcount ) ) deallocate( hst % mpi_vars_index(i) % allcount )
      end do
      deallocate( hst % mpi_vars_index )
    end if

#endif

    !-----------------------------------------------------------------
    !  メッセージ出力
    !  Output messages
    !-----------------------------------------------------------------
#ifdef LIB_MPI
    if ( .not. hst % mpi_gather &
      &  .or. ( hst % mpi_gather .and. hst % mpi_myrank == 0 ) ) then
#endif

    if ( .not. present_and_true(quiet) ) then
      call MessageNotify('M', subname, &
        & '"%c" is closed', &
        & c1 = trim( file ), rank_mpi = -1 )
    end if

#ifdef LIB_MPI
    endif

    hst % mpi_myrank     = -1
    hst % mpi_nprocs = -1
    hst % mpi_gather   = .false.
    hst % mpi_split    = .false.
#endif


    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
    hst % initialized = .false.
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HistoryClose
