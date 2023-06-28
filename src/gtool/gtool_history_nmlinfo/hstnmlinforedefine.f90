!= 出力モードから定義モードに移行
!= Transit from output mode to define mode
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinforedefine.f90,v 1.1 2009-05-11 15:15:14 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoReDefine( gthstnml, err )
    !
    ! 出力モードから定義モードに戻り, 
    ! 再び情報を設定可能にします. 
    ! HstNmlInfoAssocGTHist サブルーチンを呼び出す前には, 
    ! 再度 HstNmlInfoEndDefine を呼び出して定義モードへと移行してください. 
    ! このサブルーチンを呼んだ後でなら, 再度
    ! HstNmlInfoAdd, HstNmlInfoDelete, HstNmlInfoResetDefault 
    ! を呼ぶことが可能です. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合にも, プログラムはエラーを発生させます. 
    !
    ! Return from output mode to define mode, 
    ! information can be configured again. 
    ! Use "HstNmlInfoEndDefine" again and 
    ! transit from define mode to output mode, 
    ! before "HstNmlInfoAssocGTHist" is used. 
    ! "HstNmlInfoAdd", "HstNmlInfoDelete", "HstNmlInfoResetDefault" 
    ! can be are used again after 
    ! this subroutine is used. 
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_hash, only: HASH, DCHashPut, DCHashGet, DCHashRewind, DCHashNext, DCHashNumber
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, HST_ENOTINDEFINE, HST_EINTFILE, HST_EINDEFINE
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
    !  作業変数
    !  Work variables
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoReDefine'
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

    if ( gthstnml % define_mode ) then
      stat = HST_EINDEFINE
      cause_c = 'ReDefine'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  定義モードへと戻る
    !  Return to define mode
    !-----------------------------------------------------------------
    gthstnml % define_mode = .true.

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoReDefine
