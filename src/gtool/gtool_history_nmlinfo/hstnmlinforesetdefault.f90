!= デフォルト値を残し, 登録したデータを削除
!= Stored data is deleted without default settings
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinforesetdefault.f90,v 1.1 2009-05-11 15:15:14 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoResetDefault( gthstnml, err )
    !
    ! デフォルト値を残し, 登録したデータを削除します.
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! Stored data is deleted without default settings. 
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
    use gtool_history_nmlinfo_generic, only: HstNmlInfoDelete, HstNmlInfoGetNames
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, HST_ENOTINDEFINE
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
    character(TOKEN), pointer:: varnames_array(:) =>null()
    integer:: i, vnmax
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoResetDefault'
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
      cause_c = 'ResetDefault'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  変数名リストの取得
    !  Get varnames list
    !-----------------------------------------------------------------
    call HstNmlInfoGetNames( gthstnml, & ! (in)
      & varnames_array )                 ! (out)
    vnmax = size( varnames_array )

    do i = 1, vnmax
      call HstNmlInfoDelete( &
        & gthstnml = gthstnml, &     ! (inout)
        & name = varnames_array(i) ) ! (in)
    end do

    deallocate( varnames_array )

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoResetDefault
