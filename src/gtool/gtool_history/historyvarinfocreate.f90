!= GT_HISTORY_VARINFO 変数の作成
!= Constructor of GT_HISTORY_VARINFO
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyvarinfocreate.f90,v 1.1 2009-05-06 14:23:12 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryVarinfoCreate1( varinfo, & ! (out)
    & name, dims, longname, units, xtype, &    ! (in)
    & time_average, average, err &            ! (in) optional
    & ) 
    !
    !== GT_HISTORY_VARINFO 型変数作成
    !
    ! GT_HISTORY_VARINFO 型変数を作成します。
    ! このサブルーチンによる設定の後、
    ! HistoryAddVariable の *varinfo* に与えます。
    ! さらに属性を付加する場合には HistoryVarinfoAddAttr
    ! を用いてください。
    !
    ! Constructor of GT_HISTORY_VARINFO
    !
    use gtool_history_types, only: GT_HISTORY_VARINFO
    use dc_types, only: STRING, TOKEN, DP
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_message, only: MessageNotify
    use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT
    implicit none
    type(GT_HISTORY_VARINFO),intent(inout) :: varinfo
    character(*), intent(in):: name     ! 変数名
    character(*), intent(in):: dims(:)  ! 依存する次元
    character(*), intent(in):: longname ! 変数の記述的名称
    character(*), intent(in):: units    ! 変数の単位
    character(*), intent(in), optional:: xtype
                                         ! 変数の型
    logical, intent(in), optional:: time_average
                                         ! 時間平均
    logical, intent(in), optional:: average
                                         ! 時間平均 (後方互換用)
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
    character(*), parameter:: subname = "HistoryVarinfoCreate1"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = ''

    if ( varinfo % initialized ) then
      stat = DC_EALREADYINIT
      cause_c = 'GT_HISTORY_VARINFO'
      goto 999
    end if

    varinfo % name = name
    varinfo % longname = longname
    varinfo % units = units
    if ( present(xtype) )        varinfo % xtype = xtype
    if ( present(time_average) ) varinfo % time_average = time_average
    if ( present(average) )      varinfo % time_average = average
    numdims = size(dims)
    allocate(varinfo % dims(numdims))
    do i = 1, numdims
      varinfo % dims(i) = dims(i)
      if (len(trim(dims(i))) > TOKEN) then
        call MessageNotify('W', subname, &
          & 'dimension name <%c> is trancated to <%c>', &
          & c1=trim(dims(i)), c2=trim(varinfo % dims(i)))
      end if
    end do
    varinfo % initialized = .true.

999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub(subname)
  end subroutine HistoryVarinfoCreate1

  !-------------------------------------------------------------------

  subroutine HistoryVarinfoCreate2( varinfo, & ! (out)
    & name, dims, longname, units, xtype, &    ! (in)
    & time_average, average, err &            ! (in) optional
    & ) 
    !
    ! 使用方法は HistoryVarinfoCreate と同様です. 
    !
    ! Usage is same as "HistoryVarinfoCreate".
    !
    !--
    ! 総称名 Create として提供するためのサブルーチンです. 
    ! 機能は HistoryVarinfoCreate1 と同じです. 
    !++
    use gtool_history_types, only: GT_HISTORY_VARINFO
    use gtool_history_generic, only: HistoryVarinfoCreate
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    type(GT_HISTORY_VARINFO),intent(inout) :: varinfo
    character(*), intent(in):: name     ! 変数名
    character(*), intent(in):: dims(:)  ! 依存する次元
    character(*), intent(in):: longname ! 変数の記述的名称
    character(*), intent(in):: units    ! 変数の単位
    character(*), intent(in), optional:: xtype
                                         ! 変数の型
    logical, intent(in), optional:: time_average
                                         ! 時間平均
    logical, intent(in), optional:: average
                                         ! 時間平均 (後方互換用)
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
    character(*), parameter:: subname = "HistoryVarinfoCreate2"
  continue
    call BeginSub(subname)
    call HistoryVarinfoCreate( varinfo, & ! (out)
      & name, dims, longname, units, xtype, &    ! (in)
      & time_average, average, err &            ! (in) optional
      & ) 
    call EndSub(subname)
  end subroutine HistoryVarinfoCreate2
