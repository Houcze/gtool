!= GTHST_NMLINFO 型の変数に設定される情報の印字
!= Print information of "GTHST_NMLINFO"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfoputline.f90,v 1.2 2009-05-31 12:08:02 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoPutLine( gthstnml, unit, indent, err )
    !
    ! 引数 *gthstnml* に設定されている情報を印字します. 
    ! デフォルトではメッセージは標準出力に出力されます. 
    ! *unit* に装置番号を指定することで, 出力先を変更することが可能です. 
    !
    ! Print information of *gthstnml*. 
    ! By default messages are output to standard output. 
    ! Unit number for output can be changed by *unit* argument. 
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListNext
    use gtool_history_generic, only: HistoryPutLine
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
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
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
    integer:: stat
    character(STRING):: cause_c
    integer:: out_unit
    integer:: indent_len
    character(STRING):: indent_str
    character(*), parameter:: subname = 'HstNmlInfoPutLine'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
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

    !-----------------------------------------------------------------
    !  "GTHST_NMLINFO" の設定の印字
    !  Print the settings for "GTHST_NMLINFO"
    !-----------------------------------------------------------------
    if ( gthstnml % initialized ) then
      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '#<GTHST_NMLINFO:: @initialized=%y define_mode=%y', &
        & l = (/gthstnml % initialized, gthstnml % define_mode/) )

      hptr => gthstnml % gthstnml_list

      do while ( associated( hptr ) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & ' #<GTHST_NMLINFO_ENTRY:: @name=%c @file=%c', &
          & c1 = trim(hptr % name), &
          & c2 = trim(hptr % file) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @interval_value=%r @interval_unit=%c', &
          & r = (/hptr % interval_value/), &
          & c1 = trim(hptr % interval_unit) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @output_step_disable=%y', &
          & l = (/hptr % output_step_disable/) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @precision=%c @time_average=%y', &
          & c1 = trim(hptr % precision), &
          & l = (/ hptr % time_average /) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @fileprefix=%c', &
          & c1 = trim(hptr % fileprefix) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @origin_value=%r @origin_unit=%c', &
          & r = (/hptr % origin_value/), &
          & c1 = trim(hptr % origin_unit) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @terminus_value=%r @terminus_unit=%c', &
          & r = (/hptr % terminus_value/), &
          & c1 = trim(hptr % terminus_unit) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @slice_start=%*d ...', &
          & i = (/hptr % slice_start(1:10)/), n = (/ 10 /) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @slice_end=%*d ...', &
          & i = (/hptr % slice_end(1:10)/), n = (/ 10 /) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @slice_stride=%*d ...', &
          & i = (/hptr % slice_stride(1:10)/), n = (/ 10 /) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @space_average=%*b ...', &
          & l = (/hptr % space_average(1:10)/), n =(/ 10 /) )

        call Printf( out_unit, &
          & indent_str(1:indent_len) // &
          & '  @newfile_intvalue=%d @newfile_intunit=%c', &
          & i = (/hptr % newfile_intvalue/), &
          & c1 = trim(hptr % newfile_intunit) )

        if ( .not. gthstnml % define_mode ) then
          call Printf( out_unit, &
            & indent_str(1:indent_len) // &
            & '  @history=' )

          call HistoryPutLine( hptr % history, &
            & unit = out_unit, &
            & indent = indent_str(1:indent_len) // &
            & '   ' )
        end if

        call ListNext( gthstnml_list = hptr ) ! (inout)
      end do

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & ' >' )

      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '>' )
    else
      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '#<GTHST_NMLINFO:: @initialized=%y>', &
        & l = (/gthstnml % initialized/) )
    end if

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoPutLine
