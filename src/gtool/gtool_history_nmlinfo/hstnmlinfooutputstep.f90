!= 出力タイミングのチェック
!= Checker of output timing
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfooutputstep.f90,v 1.2 2009-05-31 12:08:02 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  logical function HstNmlInfoOutputStep( gthstnml, &
    & name, time ) result(result)
    !
    ! *time* が変数 *name* の出力されるタイミングであれば
    ! .true. を, そうでなければ .false. を返します. 
    ! *gthstnml* が初期設定されていない場合にも .false. が返ります. 
    ! *name* に関するデータが存在しない場合にも .false. が返ります. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! If *time* is the time that a variable *name* is output,
    ! .true. is returned, otherwise .false. is returned
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
    use dc_date_generic, only: DCDiffTimeCreate, mod, operator(==), toChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
    character(*), intent(in):: name
                              ! 変数名. 
                              ! 先頭の空白は無視されます. 
                              ! 
                              ! Variable identifier. 
                              ! Blanks at the head of the name are ignored. 
    type(DC_DIFFTIME), intent(in):: time
                              ! 現在時刻. Current time

    !-----------------------------------
    !  作業変数
    !  Work variables
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
    type(DC_DIFFTIME):: interval_time
!!$    character(*), parameter:: subname = 'HstNmlInfoOutputStep'
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
    if ( hptr % output_step_disable ) goto 999

    !-----------------------------------------------------------------
    !  時刻のチェック
    !  Check time
    !-----------------------------------------------------------------
    if ( .not. hptr % interval_value > 0.0 ) goto 999

    call DCDiffTimeCreate( &
      & diff = interval_time, &                      ! (out)
      & value = real( hptr % interval_value, DP ), & ! (in)
      & unit = hptr % interval_unit )                ! (in)

    if ( mod( time, interval_time ) == 0 ) then
      result = .true.
    end if

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    nullify( hptr )
  end function HstNmlInfoOutputStep
