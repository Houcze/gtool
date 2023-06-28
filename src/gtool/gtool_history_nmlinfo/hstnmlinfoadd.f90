!= 変数の出力情報の追加
!= Add output information of a variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfoadd.f90,v 1.2 2009-10-10 10:59:01 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  recursive subroutine HstNmlInfoAdd( gthstnml, &
    & name, file, &
    & interval_value, interval_unit, &
    & precision, &
    & time_average, average, &
    & fileprefix, &
    & origin_value, origin_unit, &
    & terminus_value, terminus_unit, &
    & slice_start, slice_end, slice_stride, &
    & space_average, &
    & newfile_intvalue, newfile_intunit, &
    & err )
    !
    ! 変数の出力情報を加えます.
    !
    ! デフォルト値を設定するには, *name* を与えないか, または
    ! *name* に空白を与えてください.
    ! デフォルト値を与える場合, *file* に与えられる情報は無視されます.
    ! *fileprefix* はデフォルト値に与える場合のみ有効です.
    !
    ! *name* に変数名が指定され, その際に *file* が与えられない,
    ! または空白が与えられる場合, *file* には
    ! "<i><*name* に与えられた文字></i>.nc" が指定されます.
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます.
    !
    ! Add output information of a variable.
    !
    ! In order to set default values, specify blank to *name* or
    ! do not specify *name*.
    ! When default values are specified, *file* is ignored.
    ! *fileprefix* is valid only when default values are specified.
    !
    ! When a variable identifier is specified to *name* and
    ! *file* is not specified or blanks are specified to *file*,
    ! "<i><string given to *name*></i>.nc" is specified to *file*.
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet,
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_generic, only: HstNmlInfoDelete
    use gtool_history_nmlinfo_internal, only: ListSearch, ListLast
    use gtool_history_nmlinfo_internal, only: name_delimiter
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar, CPrintf
    use dc_present, only: present_and_not_empty, present_and_true, present_select
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_date_types, only: DC_DIFFTIME
    use dc_date, only: DCDiffTimeCreate, operator(>), operator(<)
    use dc_message, only: MessageNotify
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, DC_EARGLACK, &
      & USR_ERRNO, HST_ENOTINDEFINE, HST_EBADNEWFILEINT
    use netcdf, only: NF90_MAX_DIMS
    implicit none
    type(GTHST_NMLINFO), intent(inout):: gthstnml
    character(*), intent(in), optional:: name
                              ! 変数名.
                              !
                              ! 先頭の空白は無視されます.
                              !
                              ! "Data1,Data2" のようにカンマで区切って複数
                              ! の変数を指定することも可能です.
                              !--
                              ! ただし,
                              ! その際には, *file* 引数で与えられる情報は
                              ! 無視されます. その他の情報はそれぞれの
                              ! 変数の情報として設定されます.
                              !++
                              !
                              ! Variable identifier.
                              !
                              ! Blanks at the head of the name are ignored.
                              !
                              ! Multiple variables can be specified
                              ! as "Data1,Data2" too. Delimiter is comma.
                              !--
                              ! In this case, *file* is ignored, and
                              ! other information is set to each variable.
                              !++
                              !
    character(*), intent(in), optional:: file
                              ! ヒストリデータのファイル名.
                              ! History data filenames
    real(DP), intent(in), optional:: interval_value
                              ! ヒストリデータの出力間隔の数値.
                              ! 負の値を与えると, 出力を抑止します.
                              !
                              ! Numerical value for interval of history data output.
                              ! Negative values suppresses output.
    character(*), intent(in), optional:: interval_unit
                              ! ヒストリデータの出力間隔の単位.
                              ! Unit for interval of history data output
    character(*), intent(in), optional:: precision
                              ! ヒストリデータの精度.
                              ! Precision of history data
    logical, intent(in), optional:: time_average
                              ! 出力データの時間平均化フラグ.
                              ! Flag for time average of output data.
    logical, intent(in), optional:: average
                              ! time_average の旧版.
                              ! Old version of "time_average"
    character(*), intent(in), optional:: fileprefix
                              ! ヒストリデータのファイル名の接頭詞.
                              ! Prefixes of history data filenames
    real(DP), intent(in), optional:: origin_value
                              ! 出力開始時刻.
                              ! Start time of output.
    character(*), intent(in), optional:: origin_unit
                              ! 出力開始時刻の単位.
                              ! Unit of start time of output.
    real(DP), intent(in), optional:: terminus_value
                              ! 出力終了時刻.
                              ! End time of output.
    character(*), intent(in), optional:: terminus_unit
                              ! 出力終了時刻の単位.
                              ! Unit of end time of output.
    integer, intent(in), optional:: slice_start(:)
                              ! 空間方向の開始点.
                              ! Start points of spaces.
    integer, intent(in), optional:: slice_end(:)
                              ! 空間方向の終了点.
                              ! End points of spaces.
    integer, intent(in), optional:: slice_stride(:)
                              ! 空間方向の刻み幅.
                              ! Strides of spaces.
    logical, intent(in), optional:: space_average(:)
                              ! 平均化のフラグ.
                              ! Flag of average.
    integer, intent(in), optional:: newfile_intvalue
                              ! ファイル分割時間間隔.
                              ! Interval of time of separation of a file.
    character(*), intent(in), optional:: newfile_intunit
                              ! ファイル分割時間間隔の単位.
                              ! Unit of interval of time of separation of a file.
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

    !-----------------------------------
    !  作業変数
    !  Work variables
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr_last =>null()
    type(DC_DIFFTIME):: interval_time, newfileint_time
    character(TOKEN), pointer:: varnames_array(:) =>null()
    integer:: i, vnmax, ary_size
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoAdd'
  continue
    call BeginSub( subname, &
      & fmt = '@name=%a @file=%a @interval_value=%r @interval_unit=%a @precision=%a @time_average=%y @fileprefix=%a', &
      & d  = (/ present_select(.true., -1.0_DP, interval_value) /), &
      & l  = (/ present_and_true(time_average) /), &
      & ca = StoA( present_select(.true., '<no>', name), &
      &            present_select(.true., '<no>', file), &
      &            present_select(.true., '<no>', interval_unit), &
      &            present_select(.true., '<no>', precision), &
      &            present_select(.true., '<no>', fileprefix) ) &
      & )

    stat = DC_NOERR
    cause_c = ''

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
    !-----------------------------------------------------------------
    if ( .not. gthstnml % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GTHST_NMLINFO'
      goto 999
    end if

    if ( .not. gthstnml % define_mode ) then
      stat = HST_ENOTINDEFINE
      cause_c = 'Add'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  複数の変数を設定する場合
    !  Configure multiple variables
    !-----------------------------------------------------------------
    if ( present_and_not_empty(name) ) then
      if ( index(name, name_delimiter) > 0 ) then
        call DbgMessage( 'multiple entries (%c) will be created', c1 = trim(name) )
!!$        if ( present(file) ) call DbgMessage( 'argument @file=%c is ignored', c1 = trim(file) )

        call Split( str = name, sep = name_delimiter, & ! (in)
          & carray = varnames_array )                   ! (out)
        vnmax = size( varnames_array )

        do i = 1, vnmax
          call HstNmlInfoAdd( &
            & gthstnml = gthstnml, &             ! (inout)
            & name = varnames_array(i), &        ! (in)
            & file = file, &                     ! (in)
            & interval_value = interval_value, & ! (in)
            & interval_unit = interval_unit, &   ! (in)
            & precision = precision, &           ! (in)
            & time_average = time_average, &     ! (in)
            & average = average, &               ! (in)
            & origin_value = origin_value, &         ! (in)
            & origin_unit = origin_unit, &           ! (in)
            & terminus_value = terminus_value, &     ! (in)
            & terminus_unit = terminus_unit, &       ! (in)
            & slice_start = slice_start, &           ! (in)
            & slice_end = slice_end, &               ! (in)
            & slice_stride = slice_stride, &         ! (in)
            & space_average = space_average, &       ! (in)
            & newfile_intvalue = newfile_intvalue, & ! (in)
            & newfile_intunit = newfile_intunit, &   ! (in)
            & err = err )                        ! (out)
          if ( present_and_true( err ) ) then
            deallocate( varnames_array )
            stat = USR_ERRNO
            goto 999
          end if
        end do
        deallocate( varnames_array )
        goto 999
      end if
    end if

    !-----------------------------------------------------------------
    !  *gthstnml* へ情報を追加.
    !  Add information to *gthstnml*
    !-----------------------------------------------------------------
    if ( .not. present_and_not_empty(name) ) then
      if ( present(interval_value) ) gthstnml % gthstnml_list % interval_value = interval_value
      if ( present(interval_unit)  ) gthstnml % gthstnml_list % interval_unit  = interval_unit
      if ( present(precision)      ) gthstnml % gthstnml_list % precision      = precision
      if ( present(average)        ) gthstnml % gthstnml_list % time_average   = average
      if ( present(time_average)   ) gthstnml % gthstnml_list % time_average   = time_average
      if ( present(fileprefix)     ) gthstnml % gthstnml_list % fileprefix     = fileprefix

      if ( present(origin_value    ) ) gthstnml % gthstnml_list % origin_value     = origin_value
      if ( present(origin_unit     ) ) gthstnml % gthstnml_list % origin_unit      = origin_unit
      if ( present(terminus_value  ) ) gthstnml % gthstnml_list % terminus_value   = terminus_value
      if ( present(terminus_unit   ) ) gthstnml % gthstnml_list % terminus_unit    = terminus_unit
      if ( present(slice_start     ) ) then
        ary_size = size(slice_start)
        gthstnml % gthstnml_list % slice_start(1:ary_size)  = slice_start
      end if
      if ( present(slice_end      ) ) then
        ary_size = size(slice_end)
        gthstnml % gthstnml_list % slice_end(1:ary_size)    = slice_end
      end if
      if ( present(slice_stride   ) ) then
        ary_size = size(slice_stride)
        gthstnml % gthstnml_list % slice_stride(1:ary_size) = slice_stride
      end if
      if ( present(space_average   ) ) then
        ary_size = size(space_average)
        gthstnml % gthstnml_list % space_average(1:ary_size) = space_average
      end if
      if ( present(newfile_intvalue) ) gthstnml % gthstnml_list % newfile_intvalue = newfile_intvalue
      if ( present(newfile_intunit ) ) gthstnml % gthstnml_list % newfile_intunit  = newfile_intunit


      hptr => gthstnml % gthstnml_list

    else
      hptr => gthstnml % gthstnml_list
      call ListSearch( gthstnml_list = hptr, & ! (inout)
        &              name = name )           ! (in)
      if ( .not. associated(hptr) ) then
        call DbgMessage( 'new entry (%c) is created', c1 = trim( adjustl( name ) ) )

        hptr_last => gthstnml % gthstnml_list
        call ListLast( gthstnml_list = hptr_last ) ! (inout)
        allocate( hptr )

        nullify( hptr % next )

        hptr % interval_value => gthstnml % gthstnml_list % interval_value
        hptr % interval_unit  => gthstnml % gthstnml_list % interval_unit
        hptr % precision      => gthstnml % gthstnml_list % precision
        hptr % time_average   => gthstnml % gthstnml_list % time_average
        hptr % fileprefix     => gthstnml % gthstnml_list % fileprefix

        hptr % origin_value     => gthstnml % gthstnml_list % origin_value
        hptr % origin_unit      => gthstnml % gthstnml_list % origin_unit
        hptr % terminus_value   => gthstnml % gthstnml_list % terminus_value
        hptr % terminus_unit    => gthstnml % gthstnml_list % terminus_unit
        hptr % slice_start      => gthstnml % gthstnml_list % slice_start
        hptr % slice_end        => gthstnml % gthstnml_list % slice_end
        hptr % slice_stride     => gthstnml % gthstnml_list % slice_stride
        hptr % space_average    => gthstnml % gthstnml_list % space_average
        hptr % newfile_intvalue => gthstnml % gthstnml_list % newfile_intvalue
        hptr % newfile_intunit  => gthstnml % gthstnml_list % newfile_intunit

        hptr_last % next => hptr
      else
        call DbgMessage( 'entry (%c) is overwritten', c1 = trim( adjustl( name ) ) )
      end if

      hptr % name  = adjustl( name )
      if ( present_and_not_empty(file) ) then
        hptr % file = file
        nullify(  hptr % fileprefix )
        allocate( hptr % fileprefix )
        hptr % fileprefix = ''
      else
        hptr % file = trim( adjustl(name) ) // '.nc'
      end if

      if ( present(interval_value) ) then
        nullify(  hptr % interval_value )
        allocate( hptr % interval_value )
        hptr % interval_value = interval_value
      end if
      if ( present(interval_unit)  ) then
        nullify(  hptr % interval_unit  )
        allocate( hptr % interval_unit  )
        hptr % interval_unit  = interval_unit
      end if
      if ( present(precision)      ) then
        nullify(  hptr % precision      )
        allocate( hptr % precision      )
        hptr % precision      = precision
      end if
      if ( present(average)        ) then
        nullify(  hptr % time_average        )
        allocate( hptr % time_average        )
        hptr % time_average   = average
      end if
      if ( present(time_average)   ) then
        nullify(  hptr % time_average   )
        allocate( hptr % time_average   )
        hptr % time_average   = time_average
      end if

      if ( present(origin_value)   ) then
        nullify(  hptr % origin_value   )
        allocate( hptr % origin_value   )
        hptr % origin_value   = origin_value
      end if
      if ( present(origin_unit)   ) then
        nullify(  hptr % origin_unit   )
        allocate( hptr % origin_unit   )
        hptr % origin_unit   = origin_unit
      end if
      if ( present(terminus_value)   ) then
        nullify(  hptr % terminus_value   )
        allocate( hptr % terminus_value   )
        hptr % terminus_value   = terminus_value
      end if
      if ( present(terminus_unit)   ) then
        nullify(  hptr % terminus_unit   )
        allocate( hptr % terminus_unit   )
        hptr % terminus_unit   = terminus_unit
      end if
      if ( present(slice_start)   ) then
        ary_size = size( slice_start )
        nullify(  hptr % slice_start   )
        allocate( hptr % slice_start(1:NF90_MAX_DIMS)   )
        hptr % slice_start = 1
        hptr % slice_start(1:ary_size) = slice_start
      end if
      if ( present(slice_end)   ) then
        ary_size = size( slice_end )
        nullify(  hptr % slice_end   )
        allocate( hptr % slice_end(1:NF90_MAX_DIMS)   )
        hptr % slice_end = -1
        hptr % slice_end(1:ary_size) = slice_end
      end if
      if ( present(slice_stride)   ) then
        ary_size = size( slice_stride )
        nullify(  hptr % slice_stride   )
        allocate( hptr % slice_stride(1:NF90_MAX_DIMS)   )
        hptr % slice_stride = 1
        hptr % slice_stride(1:ary_size) = slice_stride
      end if
      if ( present(space_average)   ) then
        ary_size = size( space_average )
        nullify(  hptr % space_average   )
        allocate( hptr % space_average(1:NF90_MAX_DIMS)   )
        hptr % space_average = .false.
        hptr % space_average(1:ary_size) = space_average
      end if
      if ( present(newfile_intvalue)   ) then
        nullify(  hptr % newfile_intvalue   )
        allocate( hptr % newfile_intvalue   )
        hptr % newfile_intvalue   = newfile_intvalue
      end if
      if ( present(newfile_intunit)   ) then
        nullify(  hptr % newfile_intunit   )
        allocate( hptr % newfile_intunit   )
        hptr % newfile_intunit   = newfile_intunit
      end if

    end if

    !---------------------------------------------------------------
    !  時間の単位のチェック
    !  Check unit of time
    !---------------------------------------------------------------
    call DCDiffTimeCreate( &
      & diff = interval_time, &          ! (out)
      & value = hptr % interval_value, & ! (in)
      & unit = hptr % interval_unit, &   ! (in)
      & err = err )                      ! (out)
    if ( present_and_true( err ) ) then
      call HstNmlInfoDelete( &
        & gthstnml = gthstnml, & ! (inout)
        & name = name )          ! (in)
      stat = USR_ERRNO
      goto 999
    end if

    !---------------------------------------------------------------
    !  ファイル分割時間間隔のチェック
    !  Check interval of time of separation of a file
    !---------------------------------------------------------------
    call DCDiffTimeCreate( &
      & diff = newfileint_time, &                  ! (out)
      & value = real( hptr % newfile_intvalue ), & ! (in)
      & unit = hptr % newfile_intunit, &           ! (in)
      & err = err )                                ! (out)
    if ( present_and_true( err ) ) then
      call HstNmlInfoDelete( &
        & gthstnml = gthstnml, & ! (inout)
        & name = name )          ! (in)
      stat = USR_ERRNO
      goto 999
    end if

    if (             ( hptr % newfile_intvalue > 0     )   &
      &  .and. .not. ( newfileint_time > interval_time ) ) then
      call MessageNotify( 'W', subname, &
        & 'newfile_int=%d [%c] must be greater than interval=%r [%c]', &
        & i = (/ hptr % newfile_intvalue /), &
        & r = (/ hptr % interval_value /), &
        & c1 = trim( hptr % newfile_intunit ), &
        & c2 = trim( hptr % interval_unit ) )

      call HstNmlInfoDelete( &
        & gthstnml = gthstnml, & ! (inout)
        & name = name )          ! (in)
      stat = HST_EBADNEWFILEINT
      cause_c = CPrintf( '%d [%c]', &
        & i = (/ hptr % newfile_intvalue /), c1 = trim( hptr % newfile_intunit ) )
      goto 999
    end if

    nullify( hptr )

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoAdd
