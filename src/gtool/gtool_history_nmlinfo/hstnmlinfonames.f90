!= 変数リストの取得
!= Return list of variables
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfonames.f90,v 1.2 2009-05-31 12:08:02 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  function HstNmlInfoNames( gthstnml ) result(result)
    !
    ! *gthstnml* が設定されている変数リストをカンマでつなげて
    ! 返します. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, 空文字が返ります. 
    !
    ! List of variables registered in *gthstnml* is join with camma, 
    ! and returned. 
    ! 
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! blank is returned. 
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListSearch, ListNext
    use gtool_history_nmlinfo_internal, only: name_delimiter
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    implicit none
    character(STRING):: result
    type(GTHST_NMLINFO), intent(in):: gthstnml

    !-----------------------------------
    !  作業変数
    !  Work variables
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
    logical:: first
!!$    character(*), parameter:: subname = 'HstNmlInfoNames'
  continue

    result = ''
    first = .true.

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
    !-----------------------------------------------------------------
    if ( .not. gthstnml % initialized ) goto 999

    !-----------------------------------------------------------------
    !  情報の取り出し
    !  Fetch information
    !-----------------------------------------------------------------
    hptr => gthstnml % gthstnml_list
    do while ( associated( hptr % next ) )
      call ListNext( gthstnml_list = hptr ) ! (inout)
      if ( first ) then
        result = adjustl( hptr % name )
        first = .false.
      else
        result = trim( result ) // name_delimiter // adjustl( hptr % name )
      end if
    end do

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    nullify( hptr )
  end function HstNmlInfoNames
