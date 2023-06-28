!= 初期設定されていない変数名のチェック
!= Check uninitialized variable names
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfoallvarinicheck.f90,v 1.1 2009-05-11 15:15:15 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoAllVarIniCheck( gthstnml, &
    & invalid, &
    & names, &
    & err )
    !
    ! 初期設定されていない変数名のチェック
    !
    ! 初期設定されていない変数名がある場合, 
    ! *invalid* に .true. を返し, *names* には初期設定されていない
    ! 変数名をカンマで区切った文字列を返します. 
    ! 
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! Check uninitialized variable names
    ! 
    ! If uninitialized variable names are exist, 
    ! .true. is set to *invalid*, and uninitialized variable names
    ! are joined with comma, and set to *names*. 
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListSearch, ListNext
    use gtool_history_nmlinfo_internal, only: name_delimiter
    use gtool_history, only: HistoryInitialized
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_present, only: present_and_not_empty, present_and_true
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, DC_EARGLACK, DC_ENOENTRY
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
    logical, intent(out):: invalid
                              ! 初期設定されていない変数名が存在する
                              ! 場合には .true. を返す. 
                              ! 
                              ! If uninitialized variable names are exist, 
                              ! .true. is returned. 
                              ! 
    character(*), intent(out):: names
                              ! 初期設定されていない変数名のリスト. 
                              ! 
                              ! List of uninitialized variable names. 
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
    character(*), parameter:: subname = 'HstNmlInfoAllVarIniCheck'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    names = ''
    invalid = .false.

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
    !-----------------------------------------------------------------
    if ( .not. gthstnml % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GTHST_NMLINFO'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  *gthstnml* 内から, *name* に関する情報を探査.
    !  Search information correspond to *name* in *gthstnml*
    !-----------------------------------------------------------------
    hptr => gthstnml % gthstnml_list
    do while ( associated( hptr % next ) )
      call ListNext( gthstnml_list = hptr ) ! (inout)

      if ( .not. HistoryInitialized( hptr % history ) ) then
        invalid = .true.
        if ( trim(names) /= '' ) names = trim(names) // name_delimiter
        names = trim(names) // adjustl( hptr % name )
      end if
    end do

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname, fmt = '@invalid=%y @names=%c', &
      & l = (/ invalid /), c1 = trim(names) )
  end subroutine HstNmlInfoAllVarIniCheck
