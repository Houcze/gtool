!= HstNmlInfoOutputStep の無効化
!= Invalidate "HstNmlInfoOutputStep"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfooutputstepdisable.f90,v 1.1 2009-05-11 15:15:14 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoOutputStepDisable( gthstnml, &
    & name, err )
    !
    ! このサブルーチンを使用すると, *name* に関して, 
    ! 以降は HstNmlInfoOutputStep が常に .false. を返すようになります. 
    !
    ! データ出力間隔を出力の初期設定から変更し, 
    ! データを出力するたびに時刻を明示的に指定する場合に利用することを
    ! 想定しています. 
    !
    ! HstNmlInfoEndDefine で定義モードから出力モードに
    ! 移行した後に呼び出してください. 
    ! HstNmlInfoEndDefine を呼ぶ前にこのサブルーチンを使用すると, 
    ! プログラムはエラーを発生させます. 
    !
    ! *name* に関する情報が見当たらない場合, 
    ! プログラムはエラーを発生させます. 
    ! *name* が空文字の場合にも, 
    ! プログラムはエラーを発生させます. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合にも, プログラムはエラーを発生させます. 
    !
    ! After this subroutine is used, 
    ! "HstNmlInfoOutputStep" returns .false. already 
    ! corresponding to the *name*. 
    !
    ! This subroutine expected to use when 
    ! interval of data output is changed from initialization of output, 
    ! and time is specified explicitly whenever data is output. 
    !
    ! Use after state is changed from define mode to
    ! output mode by "HstNmlInfoEndDefine". 
    ! If this subroutine is used before 
    ! "HstNmlInfoEndDefine" is used, error is occurred. 
    !
    ! When data correspond to *name* is not found, error is occurred.
    ! When *name* is blank, error is occurred too.
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListSearch
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, DC_ENOENTRY, HST_EBADNAME, HST_EINDEFINE
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
    character(*), intent(in):: name
                              ! 変数名. 
                              ! 先頭の空白は無視されます. 
                              ! 
                              ! Variable identifier. 
                              ! Blanks at the head of the name are ignored. 
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
    character(*), parameter:: subname = 'HstNmlInfoOutputStepDisable'
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

    if ( trim( name ) == '' ) then
      stat = HST_EBADNAME
      cause_c = ''
      goto 999
    end if

    if ( gthstnml % define_mode ) then
      stat = HST_EINDEFINE
      cause_c = 'OutputStepDisable'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  *gthstnml* 内から, *name* に関する history を探査.
    !  Search "history" correspond to *name* in *gthstnml*
    !-----------------------------------------------------------------
    hptr => gthstnml % gthstnml_list
    call ListSearch( gthstnml_list = hptr, & ! (inout)
      &              name = name )           ! (in)

    if ( .not. associated( hptr ) ) then
      stat = DC_ENOENTRY
      cause_c = adjustl( name )
      goto 999
    end if

    hptr % output_step_disable = .true.

    nullify( hptr )

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoOutputStepDisable
