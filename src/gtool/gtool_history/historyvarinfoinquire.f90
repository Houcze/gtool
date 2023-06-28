!= GT_HISTORY_VARINFO 変数への問い合わせ
!= Inquire for a GT_HISTORY_VARINFO variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyvarinfoinquire.f90,v 1.2 2009-05-25 09:45:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryVarinfoInquire1( varinfo, & ! (in)
    & name, dims, longname, units, xtype, &     ! (out) optional
    & time_average, average, err &              ! (out) optional
    & )
    !
    !== GT_HISTORY_VARINFO 型変数への問い合わせ
    !
    ! GT_HISTORY_VARINFO 型の変数内の各情報を参照します。
    !
    ! dims はポインタ配列です。空状態にして与えてください。
    !
    use dc_types, only: STRING, TOKEN, DP
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default
    implicit none
    type(GT_HISTORY_VARINFO),intent(in) :: varinfo
    character(*), intent(out), optional:: name     ! 変数名
    character(*), pointer,     optional:: dims(:)  !(out) 依存する次元
    character(*), intent(out), optional:: longname ! 変数の記述的名称
    character(*), intent(out), optional:: units    ! 変数の単位
    character(*), intent(out), optional:: xtype    ! 変数の型
    logical, intent(out), optional:: time_average  ! 時間平均
    logical, intent(out), optional:: average       ! 時間平均 (後方互換用)
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

    ! Internal Work
    integer:: i, numdims, stat
    character(STRING):: cause_c
    character(*), parameter:: subname = "HistoryVarinfoInquire1"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = ''

    if ( .not. varinfo % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GT_HISTORY_VARINFO'
      goto 999
    end if
    if (present(name)) name = varinfo % name
    if (present(dims)) then
      numdims = size(varinfo % dims)
      allocate(dims(numdims))
      do i = 1, numdims
        dims(i) = varinfo % dims(i)
      end do
    end if
    if ( present(longname) ) longname = varinfo % longname
    if ( present(units)    ) units    = varinfo % units
    if ( present(xtype)    ) xtype    = varinfo % xtype
    if ( present(time_average)  ) time_average  = varinfo % time_average
    if ( present(average)  )           average  = varinfo % time_average

999 continue
    call StoreError(stat, subname, err, cause_c=cause_c)
    call EndSub(subname)
  end subroutine HistoryVarinfoInquire1

  subroutine HistoryVarinfoInquire2( varinfo, & ! (in)
    & name, dims, longname, units, xtype, &     ! (out) optional
    & time_average, average, err &              ! (out) optional
    & )
    !
    ! 使用方法は HistoryVarinfoInquire と同様です. 
    !
    ! Usage is same as "HistoryVarinfoInquire".
    !
    !--
    ! 総称名 Inquire として提供するためのサブルーチンです. 
    ! 機能は HistoryVarinfoInquire1 と同じです. 
    !++
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_generic, only: HistoryVarinfoInquire
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    type(GT_HISTORY_VARINFO),intent(in) :: varinfo
    character(*), intent(out), optional:: name     ! 変数名
    character(*), pointer,     optional:: dims(:)  !(out) 依存する次元
    character(*), intent(out), optional:: longname ! 変数の記述的名称
    character(*), intent(out), optional:: units    ! 変数の単位
    character(*), intent(out), optional:: xtype    ! 変数の型
    logical, intent(out), optional:: time_average  ! 時間平均
    logical, intent(out), optional:: average       ! 時間平均 (後方互換用)
    logical, intent(out), optional:: err
    character(*), parameter:: subname = "HistoryVarinfoInquire2"
  continue
    call BeginSub(subname)
    call HistoryVarinfoInquire( varinfo, &
      & name, dims, longname, units, xtype, &
      & time_average, average, err )
    call EndSub(subname)
  end subroutine HistoryVarinfoInquire2
