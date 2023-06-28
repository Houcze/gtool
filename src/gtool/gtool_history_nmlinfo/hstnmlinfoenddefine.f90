!= 定義モードから出力モードに移行
!= Transit from define mode to output mode
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfoenddefine.f90,v 1.3 2009-07-28 14:27:54 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoEndDefine( gthstnml, err )
    !
    ! 定義モードから出力モードに移行し, 
    ! *gthstnml* に設定した情報を確定します. 
    ! HstNmlInfoAssocGTHist サブルーチンを呼び出す前に, 
    ! 必ずこのサブルーチンを呼び出してください. 
    ! このサブルーチンを呼んだ後に 
    ! HstNmlInfoAdd, HstNmlInfoDelete, HstNmlInfoResetDefault 
    ! を呼ぶとプログラムはエラーを発生させます. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合にも, プログラムはエラーを発生させます. 
    !
    ! Transit from define mode to output mode, 
    ! and determine information configured in *gthstnml*. 
    ! Use this subroutine before "HstNmlInfoAssocGTHist" is used. 
    ! If "HstNmlInfoAdd", "HstNmlInfoDelete", "HstNmlInfoResetDefault" 
    ! are used after 
    ! this subroutine is used, error is occurred. 
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListNext, ListSearch
    use gtool_history_nmlinfo_internal, only: opened_files
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_hash, only: HASH, DCHashPut, DCHashGet, DCHashRewind, DCHashNext, DCHashNumber
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, &
      & HST_ENOTINDEFINE, HST_EINTFILE, HST_EBADORIGIN, HST_EBADTERMINUS, &
      & HST_EBADSLICE, HST_EBADNEWFILEINT
    use dc_message, only: MessageNotify
    implicit none
    type(GTHST_NMLINFO), intent(inout):: gthstnml
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
    !  複数の変数を一つのファイルへ出力するためのチェック用変数
    !  Variables for checking for output multiple variables to one file
    character(STRING):: opname, opfile
    logical:: end

    !-----------------------------------
    !  作業変数
    !  Work variables
    character(STRING):: fullfilename
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr_prev =>null()
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoEndDefine'
  continue
    call BeginSub( subname )
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
      cause_c = 'EndDefine'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  gtool_history_types#GT_HISTORY 変数の割付
    !  Allocate "gtool_history_types#GT_HISTORY" variables
    !-----------------------------------------------------------------
    hptr => gthstnml % gthstnml_list
    if ( .not. associated( hptr % history ) ) then
      allocate( hptr % history )
    end if
    WholeLoop : do while ( associated( hptr % next ) )
      call ListNext( gthstnml_list = hptr ) ! (inout)
      if ( trim(hptr % name) == '' .or. trim(hptr % file) == '' ) &
        & cycle WholeLoop

      fullfilename = trim( hptr % fileprefix ) // hptr % file

      !---------------------------------------------------------------
      !  以前に同一ファイル名の gtool_history_types#GT_HISTORY 変数がある場合, そちらに結合
      !  If "gtool_history_types#GT_HISTORY" that has same filename exist already, associate to it
      !---------------------------------------------------------------
      nullify( hptr_prev )
      call DCHashRewind(opened_files) ! (inout)
      SearchLoop : do
        call DCHashNext( opened_files, & ! (inout)
          & opname, opfile, end )  ! (out)
        if ( end ) exit SearchLoop
        if ( trim(opfile) /= trim(fullfilename) ) cycle SearchLoop
        hptr_prev => gthstnml % gthstnml_list

        call ListSearch( gthstnml_list = hptr_prev, & ! (inout)
          &              name = opname )              ! (in)
        if ( .not. associated( hptr_prev ) ) cycle SearchLoop
        if ( trim(hptr % name) == trim(hptr_prev % name) ) cycle SearchLoop

        ! interval_value, interval_unit の同一性をチェック
        ! Check consistency of "interval_value", "interval_unit"
        !
        if ( hptr % interval_value /= hptr_prev % interval_value ) then
          call MessageNotify( 'W', subname, &
            & '@interval_value=%r (var=%a) and @interval_value=%r (var=%a) are applied to a file "%a"', &
            & r = (/hptr % interval_value, hptr_prev % interval_value/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EINTFILE
          cause_c = fullfilename
          goto 999
        elseif ( hptr % interval_unit /= hptr_prev % interval_unit ) then
          call MessageNotify( 'W', subname, &
            & '@interval_unit=%a (var=%a) and @interval_unit=%a (var=%a) are applied to a file "%a"', &
            & ca = StoA(hptr % interval_unit, hptr % name, &
            &           hptr_prev % interval_unit, hptr_prev % name, &
            &           fullfilename) )
          stat = HST_EINTFILE
          cause_c = fullfilename
          goto 999
        end if

        ! origin_value, origin_unit の同一性をチェック
        ! Check consistency of "origin_value", "origin_unit"
        !
        if ( hptr % origin_value /= hptr_prev % origin_value ) then
          call MessageNotify( 'W', subname, &
            & '@origin_value=%r (var=%a) and @origin_value=%r (var=%a) are applied to a file "%a"', &
            & r = (/hptr % origin_value, hptr_prev % origin_value/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EBADORIGIN
          cause_c = fullfilename
          goto 999
        elseif ( hptr % origin_unit /= hptr_prev % origin_unit ) then
          call MessageNotify( 'W', subname, &
            & '@origin_unit=%a (var=%a) and @origin_unit=%a (var=%a) are applied to a file "%a"', &
            & ca = StoA(hptr % origin_unit, hptr % name, &
            &           hptr_prev % origin_unit, hptr_prev % name, &
            &           fullfilename) )
          stat = HST_EBADORIGIN
          cause_c = fullfilename
          goto 999
        end if

        ! terminus_value, terminus_unit の同一性をチェック
        ! Check consistency of "terminus_value", "terminus_unit"
        !
        if ( hptr % terminus_value /= hptr_prev % terminus_value ) then
          call MessageNotify( 'W', subname, &
            & '@terminus_value=%r (var=%a) and @terminus_value=%r (var=%a) are applied to a file "%a"', &
            & r = (/hptr % terminus_value, hptr_prev % terminus_value/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EBADTERMINUS
          cause_c = fullfilename
          goto 999
        elseif ( hptr % terminus_unit /= hptr_prev % terminus_unit ) then
          call MessageNotify( 'W', subname, &
            & '@terminus_unit=%a (var=%a) and @terminus_unit=%a (var=%a) are applied to a file "%a"', &
            & ca = StoA(hptr % terminus_unit, hptr % name, &
            &           hptr_prev % terminus_unit, hptr_prev % name, &
            &           fullfilename) )
          stat = HST_EBADTERMINUS
          cause_c = fullfilename
          goto 999
        end if

        ! newfile_intvalue が有効な場合はエラーを返す. 
        ! Error is occurred when "newfile_intvalue" is valid
        !
        if (      ( hptr % newfile_intvalue > 0.0 ) &
          &  .or. ( hptr_prev % newfile_intvalue > 0.0 ) ) then
          call MessageNotify( 'W', subname, &
            & 'when @newfile_intvalue=%d (var=%a) > 0 or' // &
            & ' @newfile_intvalue=%d (var=%a) > 0, multiple variables can not be output to one file "%a"', &
            & i = (/hptr % newfile_intvalue, hptr_prev % newfile_intvalue/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EBADNEWFILEINT
          cause_c = fullfilename
          goto 999
        end if

        ! newfile_intvalue, newfile_intunit の同一性をチェック
        ! Check consistency of "newfile_intvalue", "newfile_intunit"
        !
        if ( hptr % newfile_intvalue /= hptr_prev % newfile_intvalue ) then
          call MessageNotify( 'W', subname, &
            & '@newfile_intvalue=%d (var=%a) and @newfile_intvalue=%d (var=%a) are applied to a file "%a"', &
            & i = (/hptr % newfile_intvalue, hptr_prev % newfile_intvalue/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EBADNEWFILEINT
          cause_c = fullfilename
          goto 999
        elseif ( hptr % newfile_intunit /= hptr_prev % newfile_intunit ) then
          call MessageNotify( 'W', subname, &
            & '@newfile_intunit=%a (var=%a) and @newfile_intunit=%a (var=%a) are applied to a file "%a"', &
            & ca = StoA(hptr % newfile_intunit, hptr % name, &
            &           hptr_prev % newfile_intunit, hptr_prev % name, &
            &           fullfilename) )
          stat = HST_EBADNEWFILEINT
          cause_c = fullfilename
          goto 999
        end if


        ! slice_start, slice_end, slice_stride, space_average の同一性チェック
        ! Check consistency of "slice_start", "slice_end", "slice_stride", "space_average"
        !
        if ( any( hptr % slice_start /= hptr_prev % slice_start ) ) then
          call MessageNotify( 'W', subname, &
            & '@slice_start=%*d (var=%a) and @slice_start=%*d (var=%a) are applied to a file "%a"', &
            & i = (/hptr % slice_start(1:10), hptr_prev % slice_start(1:10)/), &
            & n = (/10, 10/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EBADSLICE
          cause_c = fullfilename
          goto 999
        elseif ( any( hptr % slice_end /= hptr_prev % slice_end ) ) then
          call MessageNotify( 'W', subname, &
            & '@slice_end=%*d (var=%a) and @slice_end=%*d (var=%a) are applied to a file "%a"', &
            & i = (/hptr % slice_end(1:10), hptr_prev % slice_end(1:10)/), &
            & n = (/10, 10/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EBADSLICE
          cause_c = fullfilename
          goto 999
        elseif ( any( hptr % slice_stride /= hptr_prev % slice_stride ) ) then
          call MessageNotify( 'W', subname, &
            & '@slice_stride=%*d (var=%a) and @slice_stride=%*d (var=%a) are applied to a file "%a"', &
            & i = (/hptr % slice_stride(1:10), hptr_prev % slice_stride(1:10)/), &
            & n = (/10, 10/), &
            & ca = StoA(hptr % name, hptr_prev % name, fullfilename) )
          stat = HST_EBADSLICE
          cause_c = fullfilename
          goto 999
        end if

        !
        ! GT_HISTORY 変数の結合
        ! Associate "GT_HISTORY" variable
        !
        hptr % history => hptr_prev % history
        exit SearchLoop
      end do SearchLoop

      !---------------------------------------------------------------
      !  新規に割付
      !  Allocate newly
      !---------------------------------------------------------------
      if ( .not. associated( hptr % history ) ) then
        allocate( hptr % history )
        hptr % history % initialized = .false.
      end if

      !---------------------------------------------------------------
      !  割り付けられた名前とファイル名を登録
      !  Regist allocated name and filename
      !---------------------------------------------------------------
      call DCHashPut( opened_files, &       ! (inout)
        & hptr % name, fullfilename ) ! (in)

    end do WholeLoop

    nullify( hptr )
    nullify( hptr_prev )

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
    gthstnml % define_mode = .false.
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoEndDefine
