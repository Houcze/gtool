!= 変数名の有効性を設定
!= Set validation to variable names
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfosetvalidname.f90,v 1.1 2009-05-11 15:15:14 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoSetValidName( gthstnml, &
    & name, &
    & err )
    !
    ! 変数名の有効性を設定. 
    !
    ! 無効な変数名を検知するため, このサブルーチンで
    ! 有効な変数に対しては明示的に設定を行います. 
    ! 
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! Set validation to variable names. 
    ! 
    ! For detection of invalid variable names, 
    ! Set validation to variable names explicitly by this 
    ! subroutine. 
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
    character(*), intent(in):: name
                              ! 有効であることを設定する変数名. 
                              ! 
                              ! "Data1,Data2" のようにカンマで区切って複数
                              ! の変数を指定することも可能です. 
                              ! 
                              ! A variable name that is set validation. 
                              ! 
                              ! Multiple variables can be specified 
                              ! as "Data1,Data2" too. Delimiter is comma. 
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
    character(TOKEN), pointer:: varnames_array(:) =>null()
    integer:: i, vnmax
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoSetValidName'
  continue
    call BeginSub( subname, fmt = '@name=%c', c1 = trim(name) )
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
    !  複数の変数名の取り扱い
    !  Handle multiple variables
    !-----------------------------------------------------------------
    call Split( str = name, sep = name_delimiter, & ! (in)
      & carray = varnames_array )                   ! (out)
    vnmax = size( varnames_array )

    !-----------------------------------------------------------------
    !  *gthstnml* 内から, *name* に関する情報を探査.
    !  Search information correspond to *name* in *gthstnml*
    !-----------------------------------------------------------------
    do i = 1, vnmax
      hptr => gthstnml % gthstnml_list
      call ListSearch( gthstnml_list = hptr, &    ! (inout)
        &              name = varnames_array(i) ) ! (in)
      if ( associated( hptr ) ) then
        hptr % name_invalid = .false.
      end if
    end do

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoSetValidName
