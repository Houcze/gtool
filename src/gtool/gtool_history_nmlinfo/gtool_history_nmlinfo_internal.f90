!= gtool_history_nmlinfo 内で使用される内部向け定数, 変数, 手続き群
!= Internal constants, variables, procedures used in "gtool_history_nmlinfo"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: gtool_history_nmlinfo_internal.f90,v 1.1 2009-05-11 15:15:15 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtool_history_nmlinfo_internal
  !
  != gtool_history_nmlinfo 内で使用される内部向け定数, 変数, 手続き群
  !
  != Internal constants, variables, procedures used in "gtool_history_nmlinfo"
  !

  use dc_hash, only: HASH
  implicit none
  private
  public:: ListNext, ListLast, ListSearch

  character(1), parameter, public:: name_delimiter = ','
                              ! 複数の変数名の区切り文字
                              ! Delimiter for multiple variable names

  type(HASH), save, public:: opened_files
                              ! 複数の変数を一つのファイルへ
                              ! 出力するためのチェック用変数. 
                              ! 
                              ! Variables for checking for 
                              ! output multiple variables to one file. 

  character(*), parameter, public:: version = &
    & '$Name:  $' // &
    & '$Id: gtool_history_nmlinfo_internal.f90,v 1.1 2009-05-11 15:15:15 morikawa Exp $'

  !-----------------------------------------------------------------
  !  非公開手続
  !  Private procedures
  !-----------------------------------------------------------------

  interface ListNext
    module procedure HstNmlInfoListNext
  end interface

  interface ListLast
    module procedure HstNmlInfoListLast
  end interface

  interface ListSearch
    module procedure HstNmlInfoListSearch
  end interface

contains

  subroutine HstNmlInfoListNext( &
    & gthstnml_list, err )
    !
    ! リスト構造である *gthstnml_list* (GTHST_NMLINFO_ENTRY 型) を受け取り, 
    ! 次のエントリを *gthstnml_list* に再結合して返します. 
    ! 次のエントリが無い場合, *gthstnml_list* の最後のエントリの 
    ! *next* (空状態) に接続して返します. 
    ! *gthstnml_list* が始めから空の場合には空状態を返します. 
    !
    ! *gthstnml_list* (type "GTHST_NMLINFO_ENTRY") that is a list structure 
    ! is recieved, and *gthstnml_list* is reassociated to next entry, and
    ! is returned. 
    ! If next entry is not found, *gthstnml_list* is associated to 
    ! *next* in last entry (null), and returned. 
    ! If *gthstnml_list* is null from the beginning, null is returned.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO_ENTRY
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR
    use dc_types, only: TOKEN, STRING
    implicit none
    type(GTHST_NMLINFO_ENTRY), pointer:: gthstnml_list
                              ! (inout)
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
    character(*), parameter:: subname = 'HstNmlInfoListNext'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    !-----------------------------------------------------------------
    !  空状態の場合は何もしないで返す
    !  If null, return without change
    !-----------------------------------------------------------------
    if ( .not. associated( gthstnml_list ) ) goto 999

    !-----------------------------------------------------------------
    !  次のエントリに結合して返す
    !  Next entry is associated, and returned
    !-----------------------------------------------------------------
    gthstnml_list => gthstnml_list % next

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoListNext

  subroutine HstNmlInfoListLast( &
    & gthstnml_list, previous, err )
    !
    ! リスト構造である *gthstnml_list* (GTHST_NMLINFO_ENTRY 型) を受け取り, 
    ! 最後のエントリに再結合して返します. 
    ! *gthstnml_list* が始めから空の場合には空状態を返します. 
    !
    ! *previous* が与えられる場合, 当該エントリの一つ前の
    ! エントリに結合します. 
    !
    ! *gthstnml_list* (type "GTHST_NMLINFO_ENTRY") that is a list structure 
    ! is recieved, and *gthstnml_list* is reassociated to 
    ! last entry, and returned. 
    ! If *gthstnml_list* is null from the beginning, null is returned.
    !
    ! If *previous* is given, an entry previous to the above entry
    ! is associated. 
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO_ENTRY
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR
    use dc_types, only: TOKEN, STRING
    implicit none
    type(GTHST_NMLINFO_ENTRY), pointer:: gthstnml_list
                              ! (inout)
    type(GTHST_NMLINFO_ENTRY), pointer, optional:: previous
                              ! (out)
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
    character(*), parameter:: subname = 'HstNmlInfoListLast'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    if ( present( previous ) ) nullify( previous )

    !-----------------------------------------------------------------
    !  空状態の場合は何もしないで返す
    !  If null, return without change
    !-----------------------------------------------------------------
    if ( .not. associated( gthstnml_list ) ) goto 999

    !-----------------------------------------------------------------
    !  最後のエントリの *next* に結合して返す
    !  "*next*" in last entry is associated, and returned
    !-----------------------------------------------------------------
    do while ( associated( gthstnml_list % next ) )
      if ( present( previous ) ) previous => gthstnml_list
      call ListNext( gthstnml_list = gthstnml_list ) ! (inout)
    end do

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoListLast

  subroutine HstNmlInfoListSearch( &
    & gthstnml_list, name, &
    & previous, next, err )
    !
    ! リスト構造である *gthstnml_list* (GTHST_NMLINFO_ENTRY 型) を受け取り, 
    ! 引数 *name* と同じ値を持つエントリに再結合して返します. 
    ! 見つからない場合は空状態を返します.
    ! *gthstnml_list* が始めから空の場合には空状態を返します. 
    ! 
    ! *previous* が与えられる場合, 当該エントリの一つ前の
    ! エントリに結合します. 前のエントリが無い場合には
    ! 空状態を返します. 
    !
    ! *next* が与えられる場合, 当該エントリの一つ後ろの
    ! エントリに結合します. 後ろのエントリが無い場合には
    ! 空状態を返します. 
    ! 
    ! *gthstnml_list* (type "GTHST_NMLINFO_ENTRY") that is a list structure 
    ! is recieved, and *gthstnml_list* is reassociated to 
    ! the entry that has a value that is same as argument *name*, 
    ! and returned. 
    ! If the entry is not found, null is returned. 
    ! If *gthstnml_list* is null from the beginning, null is returned. 
    !
    ! If *previous* is given, an entry previous to the above entry
    ! is associated. If previous entries are not found, 
    ! null is returned.
    !
    ! If *next* is given, an entry next to the above entry
    ! is associated. If next entries are not found, 
    ! null is returned.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO_ENTRY
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR
    use dc_types, only: TOKEN, STRING
    implicit none
    type(GTHST_NMLINFO_ENTRY), pointer:: gthstnml_list
                              ! (inout)
    character(*), intent(in):: name
                              ! 変数名. 
                              ! 先頭の空白は無視されます. 
                              ! 
                              ! Variable identifier. 
                              ! Blanks at the head of the name are ignored. 
    type(GTHST_NMLINFO_ENTRY), pointer, optional:: previous
                              ! (out)
    type(GTHST_NMLINFO_ENTRY), pointer, optional:: next
                              ! (out)
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
    character(*), parameter:: subname = 'HstNmlInfoListSearch'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    !-----------------------------------------------------------------
    !  空状態の場合は何もしないで返す
    !  If null, return without change
    !-----------------------------------------------------------------
    if ( .not. associated( gthstnml_list ) ) goto 999

    !-----------------------------------------------------------------
    !  引数 *name* と同じ *name* を持つエントリを探査
    !  The entry that has *name* that is same as argument *name* is searched
    !-----------------------------------------------------------------
    if ( present( previous ) ) nullify( previous )
    if ( present( next ) ) nullify( next )
    if ( trim( adjustl( gthstnml_list % name ) ) == trim( adjustl( name ) ) ) then
      if ( present( next ) ) then
        next => gthstnml_list % next
      end if
      goto 999
    end if

    do while ( associated( gthstnml_list ) )
      if ( present( previous ) ) previous => gthstnml_list
      call ListNext( gthstnml_list = gthstnml_list ) ! (inout)
      if ( .not. associated( gthstnml_list ) ) goto 999
      if ( trim( adjustl( gthstnml_list % name ) ) == trim( adjustl( name ) ) ) then
        if ( present( next ) ) then
          next => gthstnml_list % next
        end if
        goto 999
      end if
    end do

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoListSearch

end module gtool_history_nmlinfo_internal
