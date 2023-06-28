!= GT_HISTORY_VARINFO 変数のクリア
!= Destructor of GT_HISTORY_VARINFO
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyvarinfoclear.f90,v 1.2 2009-05-25 09:45:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryVarinfoClear0(varinfo, err)
    !
    !== GT_HISTORY_VARINFO 型変数初期化
    !
    ! *varinfo* で与えられた変数を HistoryVarinfoCreate による初期設定よりも
    ! さらに前の状態に初期化します。
    !
    ! Destructor of GT_HISTORY_VARINFO
    !
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_types, only: STRING, TOKEN, DP
    implicit none
    type(GT_HISTORY_VARINFO),intent(inout) :: varinfo
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

    integer:: stat
    character(STRING):: cause_c
    character(len = *), parameter:: subname = "HistoryVarinfoClear1"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = ''

    if ( .not. varinfo % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GT_HISTORY_VARINFO'
      goto 999
    end if

    varinfo % name     = ""
    varinfo % longname = ""
    varinfo % units    = ""
    varinfo % xtype    = ""
    if (associated(varinfo % attrs)) then
      deallocate(varinfo % attrs)
    end if

    varinfo % initialized = .false.
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub(subname)
  end subroutine HistoryVarinfoClear0
