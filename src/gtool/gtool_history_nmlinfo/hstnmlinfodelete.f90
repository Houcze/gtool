!= 変数の出力情報の削除
!= Delete output information of a variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfodelete.f90,v 1.1 2009-05-11 15:15:15 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  recursive subroutine HstNmlInfoDelete( gthstnml, &
    & name, &
    & err )
    !
    ! 変数の出力情報を削除します. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! Delete output information of a variable.
    ! 
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListSearch
    use gtool_history_nmlinfo_internal, only: name_delimiter
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_present, only: present_and_not_empty, present_and_true
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, DC_EARGLACK, USR_ERRNO, HST_ENOTINDEFINE
    implicit none
    type(GTHST_NMLINFO), intent(inout):: gthstnml
    character(*), intent(in):: name
                              ! 変数名. 
                              ! 
                              ! 先頭の空白は無視されます. 
                              ! 
                              ! "Data1,Data2" のようにカンマで区切って複数
                              ! の変数を指定することが可能です. 
                              ! 
                              ! Variable identifier. 
                              ! 
                              ! Blanks at the head of the name are ignored. 
                              ! 
                              ! Multiple variables can be specified 
                              ! as "Data1,Data2". Delimiter is comma. 
                              ! 
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
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr_next =>null()
    character(TOKEN), pointer:: varnames_array(:) =>null()
    integer:: i, vnmax
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoDelete'
  continue
    call BeginSub( subname, &
      & fmt = '@name=%c', &
      & c1 = trim( name ) )
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
      cause_c = 'Delete'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  複数の変数を削除する場合
    !  Delete multiple variables
    !-----------------------------------------------------------------
    if ( present_and_not_empty(name) ) then
      if ( index(name, name_delimiter) > 0 ) then
        call DbgMessage( 'multiple entries (%c) will be deleted', c1 = trim(name) )
        call Split( str = name, sep = name_delimiter, & ! (in)
          & carray = varnames_array )                   ! (out)
        vnmax = size( varnames_array )

        do i = 1, vnmax
          call HstNmlInfoDelete( &
            & gthstnml = gthstnml, &      ! (inout)
            & name = varnames_array(i), & ! (in)
            & err = err )                 ! (out)
          if ( present_and_true( err ) ) then
            deallocate( varnames_array )
            stat = USR_ERRNO
            goto 999
          end if
        end do
        deallocate( varnames_array )
        goto 999
      end if
    end if

    !-----------------------------------------------------------------
    !  *gthstnml* の情報を削除.
    !  Delete information in *gthstnml*
    !-----------------------------------------------------------------
    hptr => gthstnml % gthstnml_list
    call ListSearch( gthstnml_list = hptr, & ! (inout)
      &              name = name, &          ! (in)
      &              previous = hptr_prev, & ! (out)
      &              next = hptr_next )      ! (out)

    if ( .not. associated( hptr ) ) goto 999
    if ( ( trim(hptr % name) /= '' ) .and. associated( hptr_prev ) ) then
      call DbgMessage( 'entry (%c) is deleted', c1 = trim( adjustl( name ) ) )
      hptr_prev % next => hptr_next
      deallocate( hptr )
    end if

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoDelete
