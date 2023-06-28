!= 出力の有効性のチェック
!= Checker of output validation
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfooutputvalid.f90,v 1.1 2009-05-11 15:15:14 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  logical function HstNmlInfoOutputValid( gthstnml, &
    & name ) result(result)
    !
    ! 変数 *name* の出力が有効であれば, 
    ! .true. を, そうでなければ .false. を返します. 
    ! 出力が有効であるかどうかは, 出力間隔 *interval_value* の
    ! 正負によって判定されます. 正の場合が有効, 負の場合が無効です. 
    ! *gthstnml* が初期設定されていない場合にも .false. が返ります. 
    ! *name* に関するデータが存在しない場合にも .false. が返ります. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! If output of a variable *name* is valid, 
    ! .true. is returned, otherwise .false. is returned. 
    ! Whether output is valid or not is judged with positive or negative 
    ! of *interval_value*. Positive is valid, and negative is invalid. 
    ! When *gthstnml* is not initialized, .false. is returned too. 
    ! When data correspond to *name* is not found, .false. is returned too. 
    ! 
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListSearch
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_present, only: present_and_not_empty, present_and_true
    use dc_date_types, only: DC_DIFFTIME
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
    character(*), intent(in):: name
                              ! 変数名. 
                              ! 先頭の空白は無視されます. 
                              ! 
                              ! Variable identifier. 
                              ! Blanks at the head of the name are ignored. 

    !-----------------------------------
    !  作業変数
    !  Work variables
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
!!$    character(*), parameter:: subname = 'HstNmlInfoOutputValid'
  continue

    result = .false.

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
    !-----------------------------------------------------------------
    if ( .not. gthstnml % initialized ) goto 999

    !-----------------------------------------------------------------
    !  情報格納変数への結合
    !  Associate a variable storing information
    !-----------------------------------------------------------------
    hptr => gthstnml % gthstnml_list
    call ListSearch( gthstnml_list = hptr, & ! (inout)
      &              name = name )           ! (in)

    if ( .not. associated( hptr ) ) goto 999

    !-----------------------------------------------------------------
    !  出力の有効性のチェック
    !  Check validity of output
    !-----------------------------------------------------------------
    if ( hptr % interval_value > 0.0 ) then
      result = .true.
      goto 999
    end if

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    nullify( hptr )
  end function HstNmlInfoOutputValid
