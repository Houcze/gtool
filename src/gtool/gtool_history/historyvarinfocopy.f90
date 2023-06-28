!= GT_HISTORY_VARINFO のコピー
!= Copy GT_HISTORY_VARINFO
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyvarinfocopy.f90,v 1.2 2009-05-25 09:45:19 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryVarinfoCopy1(varinfo_dest, varinfo_src, err, &
    & name, dims, longname, units, xtype )
    !
    !== GT_HISTORY_VARINFO 型変数コピー
    !
    ! GT_HISTORY_VARINFO 型の変数 *varinfo_src* を
    ! *varinfo_dest* にコピーします。
    ! *varinfo_src* は HistoryVarinfoCreate によって初期設定されている必要が
    ! あります。
    ! さらに属性を付加する場合には HistoryVarinfoAddAttr
    ! を用いてください。
    !
    ! *err* を与えておくと、コピーの際何らかの不具合が生じても
    ! 終了せずに err が真になって返ります。
    !
    ! *err* 以降の引数は、コピーの際に上書きする値です。
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_internal, only: default, copy_attrs
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_present,only: present_select
    use dc_string, only: JoinChar
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, DC_EALREADYINIT
    use dc_types, only: STRING, TOKEN
    implicit none
    type(GT_HISTORY_VARINFO),intent(out):: varinfo_dest
    type(GT_HISTORY_VARINFO),intent(in):: varinfo_src
    logical, intent(out), optional:: err
    character(*) , intent(in), optional:: name     ! 次元変数名
    character(*) , intent(in), optional, target:: dims(:)  ! 依存する次元
    character(*) , intent(in), optional:: longname ! 次元変数の記述的名称
    character(*) , intent(in), optional:: units    ! 次元変数の単位
    character(*) , intent(in), optional:: xtype    ! 次元変数の型

    integer:: i, stat
    character(STRING):: cause_c
    character(TOKEN), pointer :: srcdims(:) =>null() ! 依存する次元
    character(*), parameter:: subname = "HistoryVarinfoCopy1"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = ''

    if ( .not. varinfo_src % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GT_HISTORY_VARINFO'
      goto 999
    end if

    if ( varinfo_dest % initialized ) then
      stat = DC_EALREADYINIT
      cause_c = 'GT_HISTORY_VARINFO'
      goto 999
    end if

    varinfo_dest % name     = present_select('', varinfo_src % name, name)
    varinfo_dest % longname = present_select('', varinfo_src % longname, longname)
    varinfo_dest % units    = present_select('', varinfo_src % units, units)
    varinfo_dest % xtype    = present_select('', varinfo_src % xtype, xtype)

    if (present(dims)) then
      srcdims => dims
    else
      srcdims => varinfo_src % dims
    endif

    call DbgMessage('srcdims=<%c>', &
      & c1=trim(JoinChar(srcdims)))

    allocate(  varinfo_dest % dims( size( srcdims ) )  )
    do i = 1, size(srcdims)
      varinfo_dest % dims(i) = srcdims(i)
    end do

    call DbgMessage('varinfo_dest %% dims=<%c>', &
      & c1=trim(JoinChar(varinfo_dest % dims)))

    if (associated( varinfo_src % attrs ) ) then
      allocate(  varinfo_dest % attrs( size( varinfo_src % attrs) )  )
      call copy_attrs( from = varinfo_src % attrs, &
        &                to = varinfo_dest % attrs, err = err)
    end if

    varinfo_dest % initialized = .true.
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub(subname)
  end subroutine HistoryVarinfoCopy1

  subroutine HistoryVarinfoCopy2(varinfo_dest, varinfo_src, err, &
    & name, dims, longname, units, xtype )
    !
    ! 使用方法は HistoryVarinfoCopy と同様です. 
    !
    ! Usage is same as "HistoryVarinfoCopy".
    !
    !--
    ! 総称名 Copy として提供するための関数です. 
    ! 機能は HistoryVarinfoCopy1 と同じです. 
    !++
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR
    use gtool_history_generic, only: HistoryVarinfoCopy
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    type(GT_HISTORY_VARINFO),intent(out):: varinfo_dest
    type(GT_HISTORY_VARINFO),intent(in):: varinfo_src
    logical, intent(out), optional:: err
    character(*) , intent(in), optional:: name     ! 次元変数名
    character(*) , intent(in), optional, target:: dims(:)  ! 依存する次元
    character(*) , intent(in), optional:: longname ! 次元変数の記述的名称
    character(*) , intent(in), optional:: units    ! 次元変数の単位
    character(*) , intent(in), optional:: xtype    ! 次元変数の型

    character(*), parameter:: subname = "HistoryVarinfoCopy2"
  continue
    call BeginSub(subname)
    call HistoryVarinfoCopy(varinfo_dest, varinfo_src, err, &
      & name, dims, longname, units, xtype )
    call EndSub(subname)
  end subroutine HistoryVarinfoCopy2
