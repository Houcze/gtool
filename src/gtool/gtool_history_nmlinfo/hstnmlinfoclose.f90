!= GTHST_NMLINFO 型の変数の終了処理
!= Deconstructor of "GTHST_NMLINFO"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfoclose.f90,v 1.2 2009-06-01 15:17:18 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoClose( gthstnml, err )
    !
    ! GTHST_NMLINFO 型の変数の終了処理を行います. 
    !
    ! このサブルーチンを使用する前に, *gthstnml* に格納されている
    ! gtool_history_types#GT_HISTORY 型の全ての変数に対して, 
    ! gtool_history_generic#HistoryClose を用いて終了処理を行ってください. 
    ! 終了処理されていないものがある場合, 
    ! プログラムはエラーを発生させます. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! Deconstructor of "GTHST_NMLINFO". 
    !
    ! Terminate all "gtool_history_types#GT_HISTORY" variables in *gthstnml*
    ! by "gtool_history_generic#HistoryClose" before this subroutine is used. 
    ! If unterminated variables remain, 
    ! error is occurred. 
    !
    ! Note that if *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred. 
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListLast
    use gtool_history, only: HistoryInitialized
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, HST_ENOTTERMGTHIST
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
    !  作業変数
    !  Work variables
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr_prev =>null()
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoClose'
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

    !-----------------------------------------------------------------
    !  "GTHST_NMLINFO" の設定の消去
    !  Clear the settings for "GTHST_NMLINFO"
    !-----------------------------------------------------------------
    do 
      hptr => gthstnml % gthstnml_list
      call ListLast( gthstnml_list = hptr, & ! (inout)
        & previous = hptr_prev )             ! (out)
      call DbgMessage( 'remove entry (%c)', c1 = trim(hptr % name) )
      if ( trim( hptr % name ) == '' ) exit
      if ( .not. gthstnml % define_mode ) then
        if ( HistoryInitialized( hptr % history ) ) then
          stat = HST_ENOTTERMGTHIST
          cause_c = hptr % name
          goto 999
        end if
      end if
      deallocate( hptr )
      nullify( hptr_prev % next )
    end do
    deallocate( gthstnml % gthstnml_list )

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
    gthstnml % initialized = .false.
    gthstnml % define_mode = .true.
999 continue
    nullify( hptr )
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoClose
