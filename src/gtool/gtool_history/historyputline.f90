!= GT_HISTORY に格納される情報の印字
!= Print information stored in a "GT_HISTORY" variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyputline.f90,v 1.3 2009-10-10 08:01:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryPutLine( history, unit, indent, err )
    !
    ! 引数 *history* に設定されている情報を印字します. 
    ! デフォルトではメッセージは標準出力に出力されます. 
    ! *unit* に装置番号を指定することで, 出力先を変更することが可能です. 
    !
    ! Print information of *history*. 
    ! By default messages are output to standard output. 
    ! Unit number for output can be changed by *unit* argument. 
    !
    use gtool_history_types, only: GT_HISTORY
    use gtool_history_internal, only: default
    use gtool_history_generic, only: HistoryInquire
    use gtdata_generic, only: PutLine, Get_Attr
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT
    use dc_date, only: EvalByUnit
    implicit none
    type(GT_HISTORY), intent(in), target, optional:: history
    integer, intent(in), optional:: unit
                              ! 出力先の装置番号. 
                              ! デフォルトの出力先は標準出力. 
                              !
                              ! Unit number for output. 
                              ! Default value is standard output. 
    character(*), intent(in), optional:: indent
                              ! 表示されるメッセージの字下げ. 
                              !
                              ! Indent of displayed messages. 
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
    type(GT_HISTORY), pointer:: hst =>null()
    integer:: i, max
    integer:: stat
    character(STRING):: cause_c
    integer:: out_unit
    integer:: indent_len
    character(STRING):: indent_str

    character(STRING):: file, title, source, institution
    character(STRING):: conventions, gt_version
    character(TOKEN), pointer:: dims(:) =>null()
    integer, pointer:: dimsizes(:) =>null()
    character(STRING), pointer:: longnames(:) =>null()
    character(TOKEN), pointer:: units(:) =>null()
    character(TOKEN), pointer:: xtypes(:) =>null()

    real:: origin, interval, newest, oldest
    character(*), parameter:: subname = 'HistoryPutLine'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    !-----------------------------------------------------------------
    !  出力先装置番号と字下げの設定
    !  Configure output unit number and indents
    !-----------------------------------------------------------------
    if ( present(unit) ) then
      out_unit = unit
    else
      out_unit = STDOUT
    end if

    indent_len = 0
    indent_str = ''
    if ( present(indent) ) then
      if ( len(indent) /= 0 ) then
        indent_len = len(indent)
        indent_str(1:indent_len) = indent
      end if
    end if

    if (present(history)) then
      hst => history
    else
      hst => default
    endif

    !-----------------------------------------------------------------
    !  "GT_HISTORY" の設定の印字
    !  Print the settings for "GT_HISTORY"
    !-----------------------------------------------------------------
    if ( hst % initialized ) then
      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '#<GT_HISTORY:: @initialized=%y', &
        & l = (/hst % initialized/) )

      call HistoryInquire( history = hst, &             ! (in)
        & err = err, &                                  ! (out)
        & file = file, title = title, &                 ! (out)
        & source = source, institution = institution, & ! (out)
        & dims = dims, dimsizes = dimsizes, &           ! (out)
        & longnames = longnames, &                      ! (out)
        & units = units, xtypes = xtypes, &             ! (out)
        & conventions = conventions, &                  ! (out)
        & gt_version = gt_version )                     ! (out)

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @file=%c @title=%c', &
        & c1 = trim(file), c2 = trim(title) )

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @source=%c @institution=%c', &
        & c1 = trim(source), c2 = trim(institution) )

      max = size( dims )
      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @dims=%c @dimsizes=%*d', &
        & c1 = trim( JoinChar(dims, ',') ), &
        & i = dimsizes, n = (/max/) )
      deallocate( dims, dimsizes )

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @longnames=%c', &
        & c1 = trim( JoinChar(longnames, ',') ) )
      deallocate( longnames )

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @units=%c @xtypes=%c', &
        & c1 = trim( JoinChar(units, ',') ), &
        & c2 = trim( JoinChar(xtypes, ',') ) )
      deallocate( units, xtypes )

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @conventions=%c @gt_version=%c', &
        & c1 = trim(conventions), c2 = trim(gt_version) )

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @unlimited_index=%d', &
        & i = (/hst % unlimited_index/) )

      max = size( hst % dim_value_written )
      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @dim_value_written=%*y', &
        & l = hst % dim_value_written, n = (/max/) )

      origin   = hst % origin
      interval = hst % interval
      newest   = hst % newest
      oldest   = hst % oldest

!!$      origin   = EvalByUnit( hst % origin, '', hst % unlimited_units_symbol )
!!$      interval = EvalByUnit( hst % interval, '', hst % unlimited_units_symbol )
!!$      newest   = EvalByUnit( hst % newest, '', hst % unlimited_units_symbol )
!!$      oldest   = EvalByUnit( hst % oldest, '', hst % unlimited_units_symbol )

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' @origin=%r @interval=%r @newest=%r @oldest=%r', &
        & r = (/origin, interval, newest, oldest/) )

      if ( associated( hst % growable_indices ) ) then
        max = size( hst % growable_indices )
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @growable_indices=%*d', &
          & i = hst % growable_indices, n = (/max/) )
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @growable_indices=<null>' )
      end if

      if ( associated( hst % count ) ) then
        max = size( hst % count )
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @count=%*d', &
          & i = hst % count, n = (/max/) )
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @count=<null>' )
      end if

      if ( associated( hst % dimvars ) ) then
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @dimvars=' )
        max = size( hst % dimvars )
        do i = 1, max
          call PutLine( hst % dimvars(i), out_unit, &
            & indent_str(1:indent_len) // '  ', err )
        end do
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @dimvars=<null>' )
      end if

      if ( associated( hst % vars ) ) then
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @vars=' )
        max = size( hst % vars )
        do i = 1, max
          call PutLine( hst % vars(i), out_unit, &
            & indent_str(1:indent_len) // '  ', err )
        end do
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @vars=<null>' )
      end if

      if ( associated( hst % var_avr_count ) ) then
        max = size( hst % var_avr_count )
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_count=%*d', &
          & i = hst % var_avr_count, n = (/max/) )
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_count=<null>' )
      end if

      if ( associated( hst % var_avr_firstput ) ) then
        max = size( hst % var_avr_firstput )
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_firstput=%*b', &
          & l = hst % var_avr_firstput, n = (/max/) )
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_firstput=<null>' )
      end if

      if ( associated( hst % var_avr_coefsum ) ) then
        max = size( hst % var_avr_coefsum )
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_coefsum=%*f', &
          & d = hst % var_avr_coefsum, n = (/max/) )
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_coefsum=<null>' )
      end if

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '  @time_bnds=%*f, @time_bnds_output_count=%d', &
        & i = (/hst % time_bnds_output_count/), &
        & d = hst % time_bnds, &
        & n = (/ size(hst % time_bnds) /) )

      if ( associated( hst % var_avr_data ) ) then
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_data=' )
        max = size( hst % var_avr_data )
        do i = 1, max
          call Printf( out_unit, &
            & indent_str(1:indent_len) // &
            & '  #<GT_HISTORY_AVRDATA:: @length=%d', &
            & i = (/hst % var_avr_data(i) % length/) )
          call PutLine( hst % var_avr_data(i) % a_DataAvr, unit = out_unit, &
            & lbounds = lbound(hst % var_avr_data(i) % a_DataAvr), &
            & ubounds = ubound(hst % var_avr_data(i) % a_DataAvr), &
            & indent = indent_str(1:indent_len) // &
            & '    @a_DataAvr=' )
        end do
      else
        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' @var_avr_data=<null>' )
      end if

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '>' )
    else
      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '#<GT_HISTORY:: @initialized=%y>', &
        & l = (/hst % initialized/) )
    end if

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HistoryPutLine
