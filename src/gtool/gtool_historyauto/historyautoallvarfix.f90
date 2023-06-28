!= NAMELIST から読み込んだ変数名の有効性チェック
!= Checker of validation of variable names that are loaded from NAMELIST
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyautoallvarfix.f90,v 1.2 2009-05-31 14:36:33 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2008-2009. All rights reserved. 
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryAutoAllVarFix
    !
    ! このサブルーチンは以下の動作を行います. 
    ! 
    ! * NAMELIST から読み込んだ変数名に無効なものが存在したかどうかをチェック. 
    ! * HistoryAutoAddVariable で登録した変数名を印字. 
    !
    ! このサブルーチンを呼んだ後に HistoryAutoAddVariable を呼ぶと
    ! エラーを生じます. 
    !
    ! This subroutine performs following acts. 
    ! 
    ! * Check that invalid variable names are loaded from NAMELIST or not. 
    ! * Print registered variable names by "HistoryAutoAddVariable". 
    !
    ! If "HistoryAutoAddVariable" is called after this subroutine is called, 
    ! an error is occurred. 
    !
    use gtool_historyauto_internal, only: initialized, numdims, numvars, &
      & flag_allvarfixed, gthst_vars, gthstnml, save_mpi_gather, sub_sname
    use gtool_history_nmlinfo_generic, only: HstNmlInfoAllNameValid
    use gtool_history, only: HistoryVarinfoInquire
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR, HST_EBADVARNAME, DC_ENOTINIT
    use dc_message, only: MessageNotify
    use dc_date, only: operator(*), operator(+)
    use dc_string, only: JoinChar
    use dc_types, only: DP, STRING, TOKEN

    implicit none
    logical:: allvar_invalid
                              ! 無効な変数名のチェックフラグ. 
                              ! Check flag of invalid variable names. 

    integer, parameter:: names_limit = 100
    character(names_limit):: names_invalid
                              ! 無効な変数名. 
                              ! Invalid variable names. 

    character(STRING):: name, units, longname, var_info_str
    character(TOKEN), pointer:: dims(:) =>null()
    integer:: msnot_rank
    integer:: stat, i
    character(STRING):: cause_c
    character(*), parameter:: subname = "HistoryAutoAllVarFix"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = ""

    ! 初期設定チェック
    ! Check initialization
    !
    if ( .not. initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'gtool_historyauto'
      goto 999
    end if

    ! 既に確定後であれば何もせずに終了. 
    ! Nothing is done after fixed
    !
    if ( flag_allvarfixed ) goto 999


    ! 無効な変数名のチェック (初回のみ)
    ! Check invalid variable names (at only first time)
    !
    call HstNmlInfoAllNameValid( &
      & gthstnml = gthstnml, &               ! (inout)
      & invalid = allvar_invalid, names = names_invalid ) ! (out)

    if ( len_trim(names_invalid) > (names_limit - 5)  ) then
      names_invalid = names_invalid(1:names_limit - 5) // ' ....'
    end if

    if ( allvar_invalid ) then
      stat = HST_EBADVARNAME
      cause_c = names_invalid
      call MessageNotify( 'W', subname, &
        & 'names "%c" from NAMELIST "gtool_historyauto_nml" are invalid.', &
        & c1 = trim(names_invalid) )
      goto 999
    end if

    ! 登録された変数の印字 (初回のみ)
    ! Print registered variables (at only first time)
    !
    msnot_rank = -1
    if ( save_mpi_gather ) msnot_rank = 0
    call MessageNotify( 'M', sub_sname, '-------------------------------------------', rank_mpi = msnot_rank )
    call MessageNotify( 'M', sub_sname, '----- Registered variables for output -----', rank_mpi = msnot_rank )
    call MessageNotify( 'M', sub_sname, '-------------------------------------------', rank_mpi = msnot_rank )

    do i = 1, numvars
      call HistoryVarinfoInquire( &
        & varinfo = gthst_vars(i), &    ! (in)
        & name = name, &                ! (out) optional
        & dims = dims, &                ! (out) optional
        & longname = longname, &        ! (out) optional
        & units = units )               ! (out) optional

      var_info_str = trim( longname ) // ' [' // &
        &            trim( units ) // '] {' // &
        &            trim( JoinChar( dims, ',' ) ) // '}'
      deallocate( dims )

      call MessageNotify( 'M', sub_sname, '  %c  (%c)', &
        & c1 = trim(name), c2 = trim(var_info_str), rank_mpi = msnot_rank )

    end do
    call MessageNotify( 'M', sub_sname, '-----', rank_mpi = msnot_rank )

    ! フラグの設定
    ! Set a flag
    !
    if ( .not. flag_allvarfixed ) flag_allvarfixed = .true.

999 continue
    call StoreError(stat, subname, cause_c = cause_c)
    call EndSub(subname, 'stat=%d', i = (/stat/) )
  end subroutine HistoryAutoAllVarFix
