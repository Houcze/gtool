!= 変数リストを文字列型配列ポインタとして取得
!= Return list of variables as character array pointer
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfogetnames.f90,v 1.1 2009-05-11 15:15:15 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HstNmlInfoGetNames( gthstnml, varnames_ary, err )
    !
    ! *gthstnml* が設定されている変数リストを文字型配列ポインタに
    ! 返します. varnames_ary は空状態にして与えてください.
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます.
    !
    ! List of variables registered in *gthstnml* is returned to
    ! character array pointer.
    ! Nullify "varnames_ary" before it is given to this subroutine.
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet,
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListSearch, ListNext
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT
    use netcdf, only: NF90_MAX_VARS
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
    character(TOKEN), pointer:: varnames_ary(:) ! (out)
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
    integer:: varnums, ary_size
    character(TOKEN), allocatable:: varnames_ary_tmp1(:), varnames_ary_tmp2(:)
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoNames'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    varnums = 0

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
    !  割り付け
    !  Allocate
    !-----------------------------------------------------------------
    if ( associated(varnames_ary) ) deallocate(varnames_ary)
    allocate( varnames_ary_tmp1(1:NF90_MAX_VARS) )

    !-----------------------------------------------------------------
    !  情報の取り出し
    !  Fetch information
    !-----------------------------------------------------------------
    hptr => gthstnml % gthstnml_list
    do while ( associated( hptr % next ) )
      call ListNext( gthstnml_list = hptr ) ! (inout)
      varnums = varnums + 1
      ary_size = size( varnames_ary_tmp1 )
      if ( varnums > ary_size ) then
        allocate( varnames_ary_tmp2(1:ary_size) )
        varnames_ary_tmp2(1:ary_size) = varnames_ary_tmp1(1:ary_size)
        deallocate( varnames_ary_tmp1 )
        allocate( varnames_ary_tmp1(1:varnums*2) )
        varnames_ary_tmp1(1:ary_size) = varnames_ary_tmp2(1:ary_size)
        deallocate( varnames_ary_tmp2 )
      end if

      varnames_ary_tmp1(varnums) = adjustl( hptr % name )
    end do

    if ( varnums > 0 ) then
      allocate( varnames_ary(1:varnums) )
      varnames_ary(1:varnums) = varnames_ary_tmp1(1:varnums)
    else
      allocate( varnames_ary(1:1) )
      varnames_ary = ''
    end if

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    nullify( hptr )
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoGetNames
