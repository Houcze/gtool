!= 変数の出力設定の確認手続
!= Check output setting of a variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyautochkoutput.f90,v 1.2 2010-12-28 09:53:09 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2010-. All rights reserved. 
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  function HistoryAutoChkOutput( varname ) result(result)
    !
    ! 変数の出力設定の確認を行います. 
    !
    ! *varname* に指定された変数名が出力されるよう設定されている
    ! 場合には *true* が, 出力されないよう設定されている場合は
    ! *false* が返ります. 
    ! 
    ! *HistoryAutoCreate* による初期設定がなされていない場合や, 
    ! *varname* に指定された変数名が HistoryAutoAddVariable によって
    ! 登録されていない場合, 返り値に *false* が返ります. 
    !
    ! Check output setting of a variable. 
    !
    ! If output setting of *varname* is valid, *true* is returned. 
    ! If output setting of *varname* is invalid, *false* is returned. 
    !
    ! If initialization with "HistoryAutoCreate" is not done yet, *false* is returned. 
    ! If *varname* is invalid, *false* is returned. 
    !
    use gtool_historyauto_internal, only: initialized, numdims, numvars, &
      & varname_vars, output_valid_vars
    use dc_types, only: DP, STRING, TOKEN

    implicit none
    logical:: result
    character(*), intent(in):: varname
                              ! 変数の名前. 
                              ! Variable name

    integer:: i, vnum
    integer, save:: svnum = 1
    character(*), parameter:: subname = "HistoryAutoChkOutput"
  continue
    ! 初期設定チェック
    ! Check initialization
    !
    if ( .not. initialized ) then
      result = .false.
      goto 999
    end if

    ! 変数 ID のサーチ
    ! Search variable ID
    !
    VarSearch: do
      do i = svnum, numvars
        if ( trim( varname_vars(i) ) == trim(varname) ) then
          vnum = i
          exit VarSearch
        end if
      end do
      do i = 1, svnum - 1
        if ( trim( varname_vars(i) ) == trim(varname) ) then
          vnum = i
          exit VarSearch
        end if
      end do

      result = .false.
      goto 999
    end do VarSearch

    svnum = vnum

    ! 出力設定の確認
    ! Check output setting
    !
    result = output_valid_vars( vnum )

999 continue
  end function HistoryAutoChkOutput



  function HistoryAutoChkOutputTiming( time, varname ) result(result)
    !
    ! 変数の出力設定の確認を行います. 
    !
    ! *varname* に指定された変数名が、*time* のタイミングで出力されるよう
    ! 設定されている場合には *true* が, 出力されないよう設定されている場合は
    ! *false* が返ります. 
    !
    ! なお, gtool_historyauto は時間ステップ可変に対応しているため, 
    ! *time* のタイミングで出力されるかどうかについては, 
    ! その前回に出力された時間に依存します. 従って, とある時間の
    ! 出力設定の確認は下記のように HistoryAutoPut の前で使用して下さい. 
    !
    !   if ( HistoryAutoChkOutputTiming( time, var ) ) then
    !     <some operation ...>
    !   end if 
    !
    !   call HistoryAutoPut( time, var, data )
    !
    ! 以下のように使用した場合には, 期待するような返り値が
    ! 得られないことにご留意下さい. 
    ! 
    !   do i = 1, 10 
    !     write(*,*) HistoryAutoChkOutputTiming( i * timestep, var )
    !   end do
    !
    ! *HistoryAutoCreate* による初期設定がなされていない場合や, 
    ! *varname* に指定された変数名が HistoryAutoAddVariable によって
    ! 登録されていない場合, 返り値に *false* が返ります. 
    !
    ! Check output setting of a variable. 
    !
    ! If *varname* is output on *time*, *true* is returned. 
    !
    ! Please use this function as follows. 
    ! 
    !   if ( HistoryAutoChkOutputTiming( time, var ) ) then
    !     <some operation ...>
    !   end if 
    !
    !   call HistoryAutoPut( time, var, data )
    !
    ! Following usage does not return correct values.
    ! 
    !   do i = 1, 10 
    !     write(*,*) HistoryAutoChkOutputTiming( i * timestep, var )
    !   end do
    !
    ! If initialization with "HistoryAutoCreate" is not done yet, *false* is returned. 
    ! If *varname* is invalid, *false* is returned. 
    !
    use gtool_historyauto_internal, only: initialized, numdims, numvars, &
      & varname_vars, output_valid_vars, origin_time_vars, histaddvar_vars, &
      & newfile_inttime_vars, newfile_createtime_vars, prev_outtime_vars, &
      & interval_time_vars, terminus_time_vars
    use dc_types, only: DP, STRING, TOKEN

    implicit none
    logical:: result
    real(DP), intent(in):: time
                              ! データの時刻. 
                              ! Time of data
    character(*), intent(in):: varname
                              ! 変数の名前. 
                              ! Variable name

    real(DP), parameter:: zero_time = 0.0_DP
    integer:: i, vnum
    integer, save:: svnum = 1
    character(*), parameter:: subname = "HistoryAutoChkOutputTiming"
  continue

    ! 初期設定チェック
    ! Check initialization
    !
    if ( .not. initialized ) then
      result = .false.
      goto 999
    end if

    ! 変数 ID のサーチ
    ! Search variable ID
    !
    VarSearch: do
      do i = svnum, numvars
        if ( trim( varname_vars(i) ) == trim(varname) ) then
          vnum = i
          exit VarSearch
        end if
      end do
      do i = 1, svnum - 1
        if ( trim( varname_vars(i) ) == trim(varname) ) then
          vnum = i
          exit VarSearch
        end if
      end do

      result = .false.
      goto 999
    end do VarSearch

    svnum = vnum

    ! 出力設定の確認
    ! Check output setting
    !
    result = output_valid_vars( vnum )

    if ( .not. result ) goto 999

    if ( origin_time_vars(vnum) > time ) then
      result = .false. 
      goto 999
    end if

    if (             origin_time_vars(vnum) <= time &
      &  .and.       (      terminus_time_vars(vnum) < zero_time &
      &                .or. terminus_time_vars(vnum) >= time      ) &
      &  .and. .not. histaddvar_vars(vnum)            ) then

      result = .true.
      goto 999
    end if

    if ( terminus_time_vars(vnum) > zero_time .and. terminus_time_vars(vnum) < time ) then
      result = .false.
      goto 999
    end if

    if ( newfile_inttime_vars(vnum) > zero_time ) then
      if ( time - newfile_createtime_vars(vnum) >= newfile_inttime_vars(vnum) ) then
        result = .true.
        goto 999
      end if
    end if

    if ( time - prev_outtime_vars(vnum) >= interval_time_vars(vnum) ) then
      result = .true.
      goto 999
    end if

    result = .false.

999 continue
  end function HistoryAutoChkOutputTiming
