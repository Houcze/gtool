!= gtool_history_nmlinfo モジュールのテストプログラム
!
!= Test program for "gtool_history_nmlinfo" 
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histnmlinfo.f90,v 1.2 2009-10-10 10:58:59 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! <b>Note that Japanese and English are described in parallel.</b>
!
! gtool_history_nmlinfo モジュールの動作テストを行うためのプログラムです.
! このプログラムがコンパイルできること, および実行時に
! プログラムが正常終了することを確認してください.
!
! This program checks the operation of "gtool_history_nmlinfo" module.
! Confirm compilation and execution of this program.
!

program histnmlinfo
  use gtool_history_nmlinfo, only: GTHST_NMLINFO, &
    & HstNmlInfoCreate, HstNmlInfoClose, HstNmlInfoPutLine, &
    & HstNmlInfoInitialized, HstNmlInfoDefineMode, &
    & HstNmlInfoEndDefine, HstNmlInfoReDefine, &
    & HstNmlInfoAdd, HstNmlInfoDelete, HstNmlInfoResetDefault, &
    & HstNmlInfoInquire, HstNmlInfoAssocGtHist, &
    & HstNmlInfoOutputStepDisable, HstNmlInfoOutputStep, &
    & HstNmlInfoOutputValid, HstNmlInfoNames, HstNmlInfoGetNames, &
    & HstNmlInfoAllVarIniCheck, &
    & HstNmlInfoSetValidName, HstNmlInfoAllNameValid
  use gtool_history, only: GT_HISTORY, HistoryInitialized, HistoryCreate, &
    & HistoryAddVariable, HistoryPut, HistoryClose
  use dc_test, only: AssertEqual, AssertGreaterThan, AssertLessThan
  use dc_types, only: DP, STRING, TOKEN
  use dc_string, only: StoA, PutLine
  use dc_args, only: ARGS, DCArgsOpen, DCArgsHelpMsg, DCArgsOption, &
    & DCArgsDebug, DCArgsHelp, DCArgsStrict, DCArgsClose
  use dc_date_types, only: DC_DIFFTIME
  use dc_date, only: DCDiffTimeCreate, operator(*)
  implicit none

  !-------------------------------------------------------------------
  !  実験の表題, モデルの名称, 所属機関名
  !  Title of a experiment, name of model, sub-organ
  !-------------------------------------------------------------------
  character(*), parameter:: title = &
    & 'gtool_history_nmlinfo_test $Name:  $ :: ' // &
    & 'Test program of "gtool_history_nmlinfo" module'
  character(*), parameter:: source = &
    & 'gtool4 project ' // &
    & '(See http://www.gfd-dennou.org/library/gtool4)'
  character(*), parameter:: institution = &
    & 'GFD Dennou Club (See http://www.gfd-dennou.org)'


  !-------------------------------------------------------------------
  !  作業変数
  !  Work variables
  !-------------------------------------------------------------------
  type(ARGS):: arg            ! コマンドライン引数. 
                              ! Command line arguments
  logical:: OPT_namelist      ! -N, --namelist オプションの有無. 
                              ! Existence of '-N', '--namelist' option
  character(STRING):: VAL_namelist
                              ! -N, --namelist オプションの値. 
                              ! Value of '-N', '--namelist' option

  type(GTHST_NMLINFO):: gthstnml00, gthstnml01, gthstnml02, gthstnml03, gthstnml04
  type(GTHST_NMLINFO):: gthstnml05, gthstnml06, gthstnml07, gthstnml08, gthstnml09
  type(GTHST_NMLINFO):: gthstnml10, gthstnml11
  logical:: err
  integer:: i
  type(DC_DIFFTIME):: interval_time
  character(STRING):: file
                              ! ヒストリデータのファイル名. 
                              ! History data filenames
  real(DP):: interval_value
                              ! ヒストリデータの出力間隔の数値. 
                              ! Numerical value for interval of history data output
  character(TOKEN):: interval_unit
                              ! ヒストリデータの出力間隔の単位. 
                              ! Unit for interval of history data output
  character(TOKEN):: precision
                              ! ヒストリデータの精度. 
                              ! Precision of history data
  logical:: time_average
                              ! 出力データの平均化フラグ. 
                              ! Flag for time_average of output data.
  character(STRING):: fileprefix
                              ! ヒストリデータのファイル名の接頭詞. 
                              ! Prefixes of history data filenames
  character(STRING):: varnames
                              ! 変数名リスト. 
                              ! List of variables
  character(TOKEN), pointer:: varnames_array(:) =>null()
                              ! 変数名リスト. 
                              ! List of variables

  type(GT_HISTORY), pointer:: history00 =>null()
                              ! gtool_history モジュール用構造体. 
                              ! Derived type for "gtool_history" module
  character(TOKEN):: name
                              ! 変数名. Variable identifier
  logical:: invalid

!!$  character(*), parameter:: subname = 'gtool_history_nmlinfo_test'
continue

  !-------------------------------------------------------------------
  !  コマンドライン引数の処理
  !  Command line arguments handling
  !-------------------------------------------------------------------
  call DCArgsOpen( arg )
  call DCArgsHelpMsg( arg, 'Title', title )
  call DCArgsHelpMsg( arg, 'Usage', &
    & './gtool_history_nmlinfo_test [Options]' )
  call DCArgsHelpMsg( arg, 'Source', source )
  call DCArgsHelpMsg( arg, 'Institution', institution )
  call DCArgsOption( arg, StoA('-N', '--namelist'), &
    & OPT_namelist, VAL_namelist, help = 'NAMELIST filename' )
  call DCArgsDebug( arg ) ; call DCArgsHelp( arg ) ; call DCArgsStrict( arg, severe = .true. )
  call DCArgsClose( arg )

  !-------------------------------------------------------------------
  !  基本の初期設定, 終了処理テスト
  !  Basic initialization and termination test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( gthstnml = gthstnml00 ) ! (out)
  call AssertEqual( 'basic initialization test 1', &
    & answer = .true., check = HstNmlInfoInitialized(gthstnml00) )
  call HstNmlInfoPutLine( gthstnml = gthstnml00 ) ! (in)

  call HstNmlInfoClose( gthstnml = gthstnml00 ) ! (inout)
  call AssertEqual( 'basic termination test 1', &
    & answer = .false., check = HstNmlInfoInitialized(gthstnml00) )
  call HstNmlInfoPutLine( gthstnml = gthstnml00 ) ! (in)

  call HstNmlInfoCreate( &
    & gthstnml = gthstnml00, & ! (inout)
    & interval_value = 3.0_DP, &  ! (in)
    & interval_unit = 'min', & ! (in)
    & precision = 'double', &  ! (in)
    & time_average = .false. ) ! (in)
  call AssertEqual( 'basic initialization test 2', &
    & answer = .true., check = HstNmlInfoInitialized(gthstnml00) )
  call HstNmlInfoPutLine( gthstnml = gthstnml00 ) ! (in)

  call HstNmlInfoClose( gthstnml = gthstnml00 ) ! (inout)
  call AssertEqual( 'basic termination test 2', &
    & answer = .false., check = HstNmlInfoInitialized(gthstnml00) )
  call HstNmlInfoPutLine( gthstnml = gthstnml00 ) ! (in)


  !-------------------------------------------------------------------
  !  重複初期設定に関するエラー処理のテスト
  !  Error handling related to duplicated initialization test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( gthstnml = gthstnml00 )  ! (inout)
  call HstNmlInfoCreate( gthstnml = gthstnml00, & ! (inout)
    & err = err )                       ! (out)
  call AssertEqual( 'error handling related to duplicated initialization test 1', &
    & answer = .true., check = err )
  call HstNmlInfoPutLine( gthstnml = gthstnml00 ) ! (in)
  call HstNmlInfoClose( gthstnml = gthstnml00 )   ! (inout)


  !-------------------------------------------------------------------
  !  終了処理に関するエラー処理のテスト
  !  Error handling related to termination test
  !-------------------------------------------------------------------
  call HstNmlInfoClose( gthstnml = gthstnml00, & ! (inout)
    & err = err )
  call AssertEqual( 'error handling related to termination test 1', &
    & answer = .true., check = err )

  !-------------------------------------------------------------------
  !  終了処理のテスト
  !  Termination test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate(&
    & gthstnml = gthstnml01, & ! (out)
    & interval_value = 3.0_DP, &  ! (in)
    & interval_unit = 'min' )  ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml01, & ! (inout)
    & name = 'Data1' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml01, & ! (inout)
    & name = 'Data2' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml01, & ! (inout)
    & name = 'Data3' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml01, & ! (inout)
    & name = 'Data4' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml01, & ! (inout)
    & name = 'Data5' )         ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml01 ) ! (in)

  call HstNmlInfoClose( gthstnml = gthstnml01 ) ! (inout)

  !-------------------------------------------------------------------
  !  無効な値に関するエラー処理のテスト
  !  Error handling related to invalid values test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml02, &      ! (out)
    & interval_value = 3.0_DP, &       ! (in)
    & interval_unit = 'hogehoge', & ! (in)
    & precision = 'double', &       ! (in)
    & time_average = .false., &     ! (in)
    & err = err )                   ! (out)
  call HstNmlInfoPutLine( gthstnml = gthstnml02 ) ! (in)
  call AssertEqual( 'error handling related to invalid values test 1', &
    & answer = .true., check = err )

  call HstNmlInfoCreate( &
    & gthstnml = gthstnml02, &      ! (inout)
    & interval_value = 3.0_DP, &       ! (in)
    & interval_unit = 'hour', &     ! (in)
    & precision = 'double', &       ! (in)
    & time_average = .false. )      ! (in)
  call HstNmlInfoPutLine( gthstnml = gthstnml02 ) ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml02, &      ! (inout)
    & name = 'Data1', &             ! (in)
    & interval_value = 3.0_DP, &       ! (in)
    & interval_unit = 'foo', &      ! (in)
    & err = err )                   ! (out)
  call HstNmlInfoPutLine( gthstnml = gthstnml02 ) ! (in)
  call AssertEqual( 'error handling related to invalid values test 2', &
    & answer = .true., check = err )

  call HstNmlInfoClose( gthstnml = gthstnml02 ) ! (inout)

  !-------------------------------------------------------------------
  !  データ操作のテスト
  !  Data handling test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml03, & ! (out)
    & interval_value = 30.0_DP, & ! (in)
    & interval_unit = 'min' )  ! (in)

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml03, &           ! (in)
    & name = '', &                       ! (in)
    & file = file, &                     ! (out)
    & interval_value = interval_value, & ! (out)
    & interval_unit = interval_unit, &   ! (out)
    & precision = precision, &           ! (out)
    & time_average = time_average )      ! (out)

  call AssertEqual( 'data handling test 1-1', &
    & answer = '', check = file )
  call AssertEqual( 'data handling test 1-2', &
    & answer = 30.0_DP, check = interval_value, &
    & significant_digits = 7, ignore_digits = -6 )
  call AssertEqual( 'data handling test 1-3', &
    & answer = 'min', check = interval_unit )
  call AssertEqual( 'data handling test 1-4', &
    & answer = 'float', check = precision )
  call AssertEqual( 'data handling test 1-5', &
    & answer = .false., check = time_average )

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml03, & ! (inout)
    & interval_value = 3.0_DP, &  ! (in)
    & interval_unit = 'min', & ! (in)
    & precision = 'double' )   ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml03 ) ! (in)

  call HstNmlInfoAdd( gthstnml = gthstnml03, &  ! (inout)
    & name = 'Data1', &               ! (in)
    & precision = 'double' )          ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml03 ) ! (in)

  call HstNmlInfoAdd( gthstnml = gthstnml03, & ! (inout)
    & name = 'Data2', &              ! (in)
    & file = 'Data2.nc', &           ! (in)
    & interval_value = 5.0_DP, &        ! (in)
    & interval_unit = 'hour' )       ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml03 ) ! (in)

  call HstNmlInfoAdd( gthstnml = gthstnml03, & ! (inout)
    & name = 'Data3', &              ! (in)
    & interval_value = -1.0_DP )        ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml03 ) ! (in)

  call HstNmlInfoInquire( gthstnml = gthstnml03, & ! (in)
    & name = 'Data2', &                  ! (in)
    & file = file, &                     ! (out)
    & interval_value = interval_value, & ! (out)
    & interval_unit = interval_unit, &   ! (out)
    & precision = precision, &           ! (out)
    & time_average = time_average )      ! (out)

  call AssertEqual( 'data handling test 2-1', &
    & answer = 'Data2.nc', check = file )
  call AssertEqual( 'data handling test 2-2', &
    & answer = 5.0_DP, check = interval_value, &
    & significant_digits = 7, ignore_digits = -6 )
  call AssertEqual( 'data handling test 2-3', &
    & answer = 'hour', check = interval_unit )
  call AssertEqual( 'data handling test 2-4', &
    & answer = 'double', check = precision )
  call AssertEqual( 'data handling test 2-5', &
    & answer = .false., check = time_average )

  call HstNmlInfoDelete( gthstnml = gthstnml03, & ! (inout)
    & name = 'Data2' )                  ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml03 ) ! (in)

  call HstNmlInfoInquire( gthstnml = gthstnml03, & ! (in)
    & name = 'Data2', &                  ! (in)
    & file = file, &                     ! (out)
    & interval_value = interval_value, & ! (out)
    & interval_unit = interval_unit, &   ! (out)
    & precision = precision, &           ! (out)
    & time_average = time_average, &     ! (out)
    & err = err )                        ! (out)

  call AssertEqual( 'data handling test 3-1', &
    & answer = .true., check = err )


  call HstNmlInfoInquire( gthstnml = gthstnml03, & ! (in)
    & name = 'Data1', &                  ! (in)
    & interval_value = interval_value, & ! (out)
    & interval_unit = interval_unit, &   ! (out)
    & precision = precision, &           ! (out)
    & time_average = time_average )      ! (out)

  call AssertEqual( 'data handling test 2-2', &
    & answer = 3.0_DP, check = interval_value, &
    & significant_digits = 7, ignore_digits = -6 )
  call AssertEqual( 'data handling test 2-3', &
    & answer = 'min', check = interval_unit )
  call AssertEqual( 'data handling test 2-4', &
    & answer = 'double', check = precision )
  call AssertEqual( 'data handling test 2-5', &
    & answer = .false., check = time_average )

  call HstNmlInfoAdd( gthstnml = gthstnml03, & ! (inout)
    & interval_value = 1.0_DP, &        ! (in)
    & interval_unit = 'day', &       ! (in)
    & precision = 'float', &         ! (in)
    & average = .true. )             ! (in)

  call HstNmlInfoInquire( gthstnml = gthstnml03, & ! (in)
    & name = 'Data1', &                  ! (in)
    & interval_value = interval_value, & ! (out)
    & interval_unit = interval_unit, &   ! (out)
    & precision = precision, &           ! (out)
    & average = time_average )           ! (out)

  call AssertEqual( 'data handling test 2-2', &
    & answer = 1.0_DP, check = interval_value, &
    & significant_digits = 7, ignore_digits = -6 )
  call AssertEqual( 'data handling test 2-3', &
    & answer = 'day', check = interval_unit )
  call AssertEqual( 'data handling test 2-4', &
    & answer = 'double', check = precision )
  call AssertEqual( 'data handling test 2-5', &
    & answer = .true., check = time_average )


  call AssertEqual( 'data handling test 4-1', &
    & answer = .true., &
    & check = HstNmlInfoOutputValid( gthstnml = gthstnml03, name = '' ) )

  call AssertEqual( 'data handling test 4-2', &
    & answer = .true., &
    & check = HstNmlInfoOutputValid( gthstnml = gthstnml03, name = 'Data1' ) )

  call AssertEqual( 'data handling test 4-3', &
    & answer = .false., &
    & check = HstNmlInfoOutputValid( gthstnml = gthstnml03, name = 'Data2' ) )

  call AssertEqual( 'data handling test 4-4', &
    & answer = .false., &
    & check = HstNmlInfoOutputValid( gthstnml = gthstnml03, name = 'Data3' ) )

  call HstNmlInfoClose( gthstnml = gthstnml03 ) ! (inout)

  !-------------------------------------------------------------------
  !  出力ステップ問い合わせのテスト
  !  Inquire output step test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml04, &    ! (out)
    & interval_value = 30.0_DP, & ! (in)
    & interval_unit = 'min' )     ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml04, &  ! (inout)
    & name = 'Data1', &         ! (in)
    & file = 'Data1.nc', &      ! (in)
    & interval_value = 1.0_DP, &   ! (in)
    & interval_unit = 'hour' )  ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml04, & ! (inout)
    & name = 'Data2', &        ! (in)
    & file = 'Data2.nc', &     ! (in)
    & interval_value = -1.0_DP )  ! (in)

  call DCDiffTimeCreate( &
    & diff = interval_time, & ! (out)
    & value = 6.0_DP, &       ! (in)
    & unit = 'sec' )          ! (in)

  call AssertEqual( 'inquire output step test 1-1', &
    & answer = .false., &
    & check = HstNmlInfoOutputStep( gthstnml = gthstnml04, name = 'Data2', time = interval_time ) )

  call AssertEqual( 'inquire output step test 1-2', &
    & answer = .false., &
    & check = HstNmlInfoOutputStep( gthstnml = gthstnml04, name = '', time = interval_time ) )

  call AssertEqual( 'inquire output step test 1-3', &
    & answer = .true., &
    & check = HstNmlInfoOutputStep( gthstnml = gthstnml04, name = '', time = interval_time * 300 ) )

  call AssertEqual( 'inquire output step test 1-4', &
    & answer = .false., &
    & check = HstNmlInfoOutputStep( gthstnml = gthstnml04, name = 'Data1', time = interval_time * 300 ) )

  call AssertEqual( 'inquire output step test 1-5', &
    & answer = .true., &
    & check = HstNmlInfoOutputStep( gthstnml = gthstnml04, name = 'Data1', time = interval_time * 600 ) )

  call HstNmlInfoOutputStepDisable( &
    & gthstnml = gthstnml04, & ! (inout)
    & name = 'Data1', &        ! (in)
    & err = err )              ! (out)
  call AssertEqual( 'inquire output step test 2-1', &
    & answer = .true., check = err )

  call HstNmlInfoEndDefine( gthstnml = gthstnml04 ) ! (inout)

  call HstNmlInfoOutputStepDisable( gthstnml = gthstnml04, & ! (inout)
    & name = 'Data1' )                             ! (in)
  call AssertEqual( 'inquire output step test 2-2', &
    & answer = .false., &
    & check = HstNmlInfoOutputStep( gthstnml = gthstnml04, name = 'Data1', time = interval_time * 600 ) )

  call HstNmlInfoClose( gthstnml = gthstnml04 ) ! (inout)

  !-------------------------------------------------------------------
  !  fileprefix テスト
  !  fileprefix test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml05, & ! (out)
    & interval_value = 30.0_DP, & ! (in)
    & interval_unit = 'min', & ! (in)
    & fileprefix = 'hoge/' )   ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml05, & ! (inout)
    & name = 'Data1' )         ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml05 ) ! (in)

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml05, &  ! (in)
    & name = '', &              ! (in)
    & file = file, &            ! (out)
    & fileprefix = fileprefix ) ! (out)

  call AssertEqual( 'fileprefix test 1-1', &
    & answer = '', check = file )
  call AssertEqual( 'fileprefix test 1-2', &
    & answer = 'hoge/', check = fileprefix )

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml05, &  ! (in)
    & name = '  Data1', &       ! (in)
    & file = file, &            ! (out)
    & fileprefix = fileprefix ) ! (out)

  call AssertEqual( 'fileprefix test 2-1', &
    & answer = 'hoge/Data1.nc', check = file )
  call AssertEqual( 'fileprefix test 2-2', &
    & answer = 'hoge/', check = fileprefix )

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml05, &  ! (inout)
    & name = '', &              ! (in)
    & fileprefix = 'foo/bar_' ) ! (in)

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml05, &  ! (in)
    & name = '', &              ! (in)
    & file = file, &            ! (out)
    & fileprefix = fileprefix ) ! (out)

  call AssertEqual( 'fileprefix test 3-1', &
    & answer = '', check = file )
  call AssertEqual( 'fileprefix test 3-2', &
    & answer = 'foo/bar_', check = fileprefix )

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml05, &  ! (in)
    & name = 'Data1', &         ! (in)
    & file = file, &            ! (out)
    & fileprefix = fileprefix ) ! (out)

  call AssertEqual( 'fileprefix test 4-1', &
    & answer = 'foo/bar_Data1.nc', check = file )
  call AssertEqual( 'fileprefix test 4-2', &
    & answer = 'foo/bar_', check = fileprefix )

  !-------------------------------------------------------------------
  !  names テスト
  !  "names" test
  !-------------------------------------------------------------------
  call HstNmlInfoGetNames( gthstnml06, & ! (in)
    & varnames_array, err )              ! (out)

  call AssertEqual( '"GetNames" test 1-1', &
    & answer = .true., check = err )

  varnames = HstNmlInfoNames( gthstnml06 )
  call AssertEqual( '"Names" test 1-1', &
    & answer = '', check = varnames )

  call HstNmlInfoCreate( &
    & gthstnml = gthstnml06, & ! (out)
    & interval_value = 3.0_DP, &  ! (in)
    & interval_unit = 'min' )  ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml06, & ! (inout)
    & name = 'Data1' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml06, & ! (inout)
    & name = 'Data2' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml06, & ! (inout)
    & name = 'Data3' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml06, & ! (inout)
    & name = 'Data4' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml06, & ! (inout)
    & name = 'Data5' )         ! (in)

  call HstNmlInfoDelete( &
    & gthstnml = gthstnml06, & ! (inout)
    & name = 'Data3' )         ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml06 ) ! (in)

  call HstNmlInfoGetNames( gthstnml06, & ! (in)
    & varnames_array )                ! (out)

  call AssertEqual( '"GetNames" test 1-2', &
    & answer = (/ 'Data1', 'Data2', 'Data4', 'Data5' /), check = varnames_array )
  deallocate( varnames_array )

  varnames = HstNmlInfoNames( gthstnml06 )
  call AssertEqual( '"Names" test 1-2', &
    & answer = 'Data1,Data2,Data4,Data5', check = varnames )

  call HstNmlInfoClose( gthstnml = gthstnml06 ) ! (inout)

  !-------------------------------------------------------------------
  !  ResetDefault テスト
  !  "ResetDefault" test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml07, & ! (out)
    & interval_value = 3.0_DP, &  ! (in)
    & interval_unit = 'min' )  ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml07, & ! (inout)
    & name = 'Data1' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml07, & ! (inout)
    & name = 'Data2' )         ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml07 ) ! (in)

  call HstNmlInfoResetDefault( gthstnml = gthstnml07 ) ! (inout)

  call HstNmlInfoPutLine( gthstnml = gthstnml07 ) ! (in)

  call HstNmlInfoGetNames( gthstnml07, & ! (in)
    & varnames_array )                ! (out)

  call AssertEqual( '"ResetDefault" test 1-1', &
    & answer = (/ '' /), check = varnames_array )
  deallocate( varnames_array )

  call AssertEqual( '"ResetDefault" test 1-2', &
    & answer = .true., check = HstNmlInfoInitialized( gthstnml07 ) )

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml07, &           ! (in)
    & name = '', &                       ! (in)
    & interval_value = interval_value, & ! (in)
    & interval_unit = interval_unit )    ! (in)

  call AssertEqual( '"ResetDefault" test 1-3', &
    & answer = 3.0_DP, check = interval_value )
  call AssertEqual( '"ResetDefault" test 1-4', &
    & answer = 'min', check = interval_unit )

  call HstNmlInfoClose( gthstnml = gthstnml07 ) ! (inout)

  !-------------------------------------------------------------------
  !  複数変数操作テスト
  !  Multiple variables handling test
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml08, & ! (out)
    & interval_value = 3.0_DP, &  ! (in)
    & interval_unit = 'min' )  ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml08, &               ! (inout)
    & name = 'Data1, Data2, Data3, Data4', & ! (in)
    & file = 'multi.nc', &                   ! (in)
    & interval_value = 2.0_DP, &                ! (in)
    & interval_unit = 'hour' )               ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml08, & ! (inout)
    & precision = 'double' )   ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml08 ) ! (in)

  call HstNmlInfoGetNames( gthstnml08, & ! (in)
    & varnames_array )                ! (out)

  call AssertEqual( 'multiple variables handling test 1-1', &
    & answer = (/ 'Data1', 'Data2', 'Data3', 'Data4' /), check = varnames_array )
  deallocate( varnames_array )

  call HstNmlInfoDelete( &
    & gthstnml = gthstnml08, & ! (inout)
    & name = 'Data2, Data3' )  ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml08 ) ! (in)

  call HstNmlInfoGetNames( gthstnml08, & ! (in)
    & varnames_array )                ! (out)

  call AssertEqual( 'multiple variables handling test 1-1', &
    & answer = (/ 'Data1', 'Data4' /), check = varnames_array )
  deallocate( varnames_array )

  call HstNmlInfoClose( gthstnml = gthstnml08 ) ! (inout)


  !-------------------------------------------------------------------
  !  出力モードテストその1 (基本的な出力)
  !  Output mode test part 1 (Basic outout)
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml09, &                    ! (out)
    & interval_value = 3.0_DP, &                     ! (in)
    & interval_unit = 'min', &                    ! (in)
    & precision = 'double', &                     ! (in)
    & fileprefix = 'xhistnmlinfo/' ) ! (in)
  call AssertEqual( 'define mode check test 1-1', &
    & answer = .true., check = HstNmlInfoDefineMode( gthstnml09 ) )

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml09, & ! (inout)
    & name = 'Data1' )         ! (in)
  call AssertEqual( 'define mode check test 1-2', &
    & answer = .true., check = HstNmlInfoDefineMode( gthstnml09 ) )

  call HstNmlInfoPutLine( gthstnml = gthstnml09 ) ! (in)

  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml09, & ! (in)
    & name = 'Data1', &        ! (in)
    & history = history00, &   ! (out)
    & err = err )              ! (out)
  call AssertEqual( 'define mode check test 1-3', &
    & answer = .true., check = err )

  call HstNmlInfoEndDefine( gthstnml = gthstnml09 ) ! (inout)

  call AssertEqual( 'define mode check test 1-4', &
    & answer = .false., check = HstNmlInfoDefineMode( gthstnml09 ) )

  call HstNmlInfoPutLine( gthstnml = gthstnml09 ) ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml09, & ! (inout)
    & name = 'Data2', &        ! (in)
    & err = err )              ! (out)
  call AssertEqual( 'output mode error handling test 1-1', &
    & answer = .true., check = err )

  call HstNmlInfoDelete( &
    & gthstnml = gthstnml09, & ! (inout)
    & name = 'Data1', &        ! (in)
    & err = err )              ! (out)
  call AssertEqual( 'output mode error handling test 1-2', &
    & answer = .true., check = err )

  call HstNmlInfoResetDefault( &
    & gthstnml = gthstnml09, & ! (inout)
    & err = err )              ! (out)
  call AssertEqual( 'output mode error handling test 1-3', &
    & answer = .true., check = err )

  call HstNmlInfoPutLine( gthstnml = gthstnml09 ) ! (in)

  call HstNmlInfoReDefine( gthstnml = gthstnml09 ) ! (inout)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml09, & ! (inout)
    & name = 'Data2', &        ! (in)
    & err = err )              ! (out)
  call AssertEqual( 'ReDefine test 1-1', &
    & answer = .false., check = err )

  call HstNmlInfoDelete( &
    & gthstnml = gthstnml09, & ! (inout)
    & name = 'Data2', &        ! (in)
    & err = err )              ! (out)
  call AssertEqual( 'ReDefine test 1-2', &
    & answer = .false., check = err )

  call HstNmlInfoEndDefine( gthstnml = gthstnml09 ) ! (inout)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml09, & ! (in)
    & name = 'DataX', &        ! (in)
    & history = history00, &   ! (out)
    & err = err )
  call AssertEqual( 'output mode AssocGtHist test 1-1', &
    & answer = .true., check = err )
  call AssertEqual( 'output mode AssocGtHist test 1-2', &
    & answer = .false., check = associated( history00 ) )

  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml09, & ! (in)
    & name = 'Data1', &        ! (in)
    & history = history00 )    ! (out)
  call AssertEqual( 'output mode AssocGtHist test 1-3', &
    & answer = .true., check = associated( history00 ) )
  call AssertEqual( 'output mode AssocGtHist test 1-4', &
    & answer = .false., check = HistoryInitialized( history00 ) )

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml09, &           ! (in)
    & name = 'Data1', &                  ! (in)
    & file = file, &                     ! (out)
    & interval_value = interval_value, & ! (out)
    & interval_unit = interval_unit )    ! (out)

  call HistoryCreate( &
    & history = history00, &                                ! (out)
    & file = file, title = 'Data1', &                       ! (in)
    & source = source, institution = institution, &         ! (in)
    & dims = StoA('lon', 'lat', 'time'), &                  ! (in)
    & dimsizes = (/4, 2, 0/), &                             ! (in)
    & longnames = StoA('longitude', 'latitude', 'time'), &  ! (in)
    & units = StoA('degree_east', 'degree_north', &
    &              interval_unit), &                        ! (in)
    & origin = 0.0, &                                       ! (in)
    & interval = real( interval_value ) )                   ! (in)

  call HistoryPut( &
    & history = history00, &                                 ! (inout)
    & varname = 'lon', array = (/0.0, 90.0, 180.0, 270.0/) ) ! (in)
  call HistoryPut( &
    & history = history00, &                     ! (inout)
    & varname = 'lat', array = (/-40.0, 40.0/) ) ! (in)
  call HstNmlInfoPutLine( gthstnml = gthstnml09 ) ! (in)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml09, & ! (in)
    & name = 'Data1', &        ! (in)
    & history = history00 )    ! (out)
  call HstNmlInfoInquire( &
    & gthstnml = gthstnml09, & ! (in)
    & name = 'Data1', &        ! (in)
    & precision = precision )  ! (out)
  call HistoryAddVariable( &
    & history = history00, &                ! (inout)
    & varname = 'Data1', &                  ! (in)
    & dims = StoA('lon', 'lat', 'time'), &  ! (in)
    & longname = 'sample data 1', &         ! (in)
    & units = 'm s-1', xtype = precision )  ! (in)
  call HstNmlInfoPutLine( gthstnml = gthstnml09 ) ! (in)

  call HstNmlInfoAllVarIniCheck( gthstnml = gthstnml09, & ! (in)
    & invalid = invalid, names = varnames )          ! (out)
  call AssertEqual( 'HstNmlInfoAllVarIniCheck test 1-1', &
    & answer = .false., check = invalid )
  call AssertEqual( 'HstNmlInfoAllVarIniCheck test 1-2', &
    & answer = '', check = trim(varnames) )

  call HstNmlInfoAllNameValid( gthstnml = gthstnml09, & ! (in)
    & invalid = invalid, names = varnames )          ! (out)
  call AssertEqual( 'HstNmlInfoAllNameValid test 1-1', &
    & answer = .true., check = invalid )
  call AssertEqual( 'HstNmlInfoAllNameValid test 1-2', &
    & answer = 'Data1', check = trim(varnames) )

  do i = 1, 3
    nullify( history00 )
    call HstNmlInfoAssocGtHist( &
      & gthstnml = gthstnml09, & ! (in)
      & name = 'Data1', &        ! (in)
      & history = history00 )    ! (out)
    call HistoryPut( &
      & history = history00, &                ! (inout)
      & varname = 'Data1', &                  ! (in)
      & array = reshape( (/1, 2, 3, 4, 5, 6, 7, 8/) * i, &
      &                  (/4, 2/) ) )         ! (in)
  end do
  call HstNmlInfoPutLine( gthstnml = gthstnml09 ) ! (in)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml09, & ! (in)
    & name = 'Data1', &        ! (in)
    & history = history00 )    ! (out)
  call HistoryClose( &
    & history = history00 ) ! (inout)

  call HstNmlInfoClose( gthstnml = gthstnml09 ) ! (inout)


  !-------------------------------------------------------------------
  !  出力モードテストその2 (複数の変数を1つのファイルへ)
  !  Output mode test part 2 (Multi variables are output to one file)
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml10, &                    ! (out)
    & interval_value = 3.0_DP, &                     ! (in)
    & interval_unit = 'min', &                    ! (in)
    & precision = 'double', &                     ! (in)
    & fileprefix = 'xhistnmlinfo/' ) ! (in)

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml10, &  ! (in)
    & name = '', &              ! (in)
    & fileprefix = fileprefix ) ! (out)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml10, &                      ! (inout)
    & name = 'Data2,Data3', &                       ! (in)
    & file = trim( fileprefix ) // 'Data2_3_4.nc' ) ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml10, &                       ! (inout)
    & name = 'Data4', &                              ! (in)
    & file = trim( fileprefix ) // 'Data2_3_4.nc', & ! (in)
    & interval_value = 3.0_DP, &                        ! (in)
    & interval_unit = 'min', &                       ! (in)
    & precision = 'float' )                          ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml10, &                       ! (inout)
    & name = 'DataX', &                              ! (in)
    & file = trim( fileprefix ) // 'Data2_3_4.nc', & ! (in)
    & interval_value = 6.0_DP )                         ! (in)
  call HstNmlInfoEndDefine( &
    & gthstnml = gthstnml10, & ! (inout)
    & err = err )              ! (out)
  call AssertEqual( 'multi files output error handling test 1-1', &
    & answer = .true., check = err )

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml10, &                       ! (inout)
    & name = 'DataX', &                              ! (in)
    & file = trim( fileprefix ) // 'Data2_3_4.nc', & ! (in)
    & interval_unit = 'hour' )                       ! (in)
  call HstNmlInfoEndDefine( &
    & gthstnml = gthstnml10, & ! (inout)
    & err = err )              ! (out)
  call AssertEqual( 'multi files output error handling test 1-2', &
    & answer = .true., check = err )

  call HstNmlInfoDelete( &
    & gthstnml = gthstnml10, & ! (inout)
    & name = 'DataX' )         ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml10, & ! (inout)
    & name = '  Data9' )         ! (in)
  call HstNmlInfoAdd( &
    & gthstnml = gthstnml10, & ! (inout)
    & name = '  Data8' )         ! (in)

  call HstNmlInfoEndDefine( gthstnml = gthstnml10 ) ! (inout)
  call AssertEqual( 'multi files output error handling test 1-3', &
    & answer = .false., check = HstNmlInfoDefineMode( gthstnml10 ) )
  call HstNmlInfoPutLine( gthstnml = gthstnml10 ) ! (in)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data2', &        ! (in)
    & history = history00 )    ! (out)

  call HstNmlInfoInquire( &
    & gthstnml = gthstnml10, &           ! (in)
    & name = 'Data2', &                  ! (in)
    & file = file, &                     ! (out)
    & interval_value = interval_value, & ! (out)
    & interval_unit = interval_unit )    ! (out)

  call HistoryCreate( &
    & history = history00, &                                ! (out)
    & file = file, &                                        ! (in)
    & title = 'Data2_3_4 multi variable output', &          ! (in)
    & source = source, institution = institution, &         ! (in)
    & dims = StoA('lon', 'lat', 'time'), &                  ! (in)
    & dimsizes = (/4, 2, 0/), &                             ! (in)
    & longnames = StoA('longitude', 'latitude', 'time'), &  ! (in)
    & units = StoA('degree_east', 'degree_north', &
    &              interval_unit), &                        ! (in)
    & origin = 0.0, &                                       ! (in)
    & interval = real( interval_value ) )                   ! (in)

  call HistoryPut( &
    & history = history00, &                                 ! (inout)
    & varname = 'lon', array = (/0.0, 90.0, 180.0, 270.0/) ) ! (in)
  call HistoryPut( &
    & history = history00, &                     ! (inout)
    & varname = 'lat', array = (/-40.0, 40.0/) ) ! (in)
  call HstNmlInfoPutLine( gthstnml = gthstnml10 ) ! (in)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data2', &        ! (in)
    & history = history00 )    ! (out)
  call HstNmlInfoInquire( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data2', &        ! (in)
    & precision = precision )  ! (out)
  call HistoryAddVariable( &
    & history = history00, &                ! (inout)
    & varname = 'Data2', &                  ! (in)
    & dims = StoA('lon', 'lat', 'time'), &  ! (in)
    & longname = 'sample data 2', &         ! (in)
    & units = 'm s-1', xtype = precision )  ! (in)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data3', &        ! (in)
    & history = history00 )    ! (out)
  call AssertEqual( 'multi files output test 1-1', &
    & answer = .true., check = HistoryInitialized( history00 ) )
  call HstNmlInfoInquire( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data3', &        ! (in)
    & precision = precision )  ! (out)
  call HistoryAddVariable( &
    & history = history00, &                ! (inout)
    & varname = 'Data3', &                  ! (in)
    & dims = StoA('lon', 'time'), &         ! (in)
    & longname = 'sample data 3', &         ! (in)
    & units = 'm s-1', xtype = precision )  ! (in)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data4', &        ! (in)
    & history = history00 )    ! (out)
  call AssertEqual( 'multi files output test 1-2', &
    & answer = .true., check = HistoryInitialized( history00 ) )
  call HstNmlInfoInquire( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data4', &        ! (in)
    & precision = precision )  ! (out)
  call HistoryAddVariable( &
    & history = history00, &                ! (inout)
    & varname = 'Data4', &                  ! (in)
    & dims = StoA('lat', 'time'), &         ! (in)
    & longname = 'sample data 4', &         ! (in)
    & units = 'm s-1', xtype = precision )  ! (in)

  call HstNmlInfoPutLine( gthstnml = gthstnml10 ) ! (in)

  call HstNmlInfoAllVarIniCheck( gthstnml = gthstnml10, & ! (in)
    & invalid = invalid, names = varnames )          ! (out)
  call AssertEqual( 'HstNmlInfoAllVarIniCheck test 2-1', &
    & answer = .true., check = invalid )
  call AssertEqual( 'HstNmlInfoAllVarIniCheck test 2-2', &
    & answer = 'Data9,Data8', check = trim(varnames) )

  call HstNmlInfoAllNameValid( gthstnml = gthstnml10, & ! (in)
    & invalid = invalid, names = varnames )          ! (out)
  call AssertEqual( 'HstNmlInfoAllNameValid test 2-1', &
    & answer = .true., check = invalid )
  call AssertEqual( 'HstNmlInfoAllNameValid test 2-2', &
    & answer = 'Data2,Data3,Data4,Data9,Data8', check = trim(varnames) )

  call HstNmlInfoSetValidName( gthstnml = gthstnml10, & ! (in)
    & name = 'Data2' )                                  ! (in)
  call HstNmlInfoSetValidName( gthstnml = gthstnml10, & ! (in)
    & name = 'Data3' )                                  ! (in)

  call HstNmlInfoAllNameValid( gthstnml = gthstnml10, & ! (in)
    & invalid = invalid, names = varnames )          ! (out)
  call AssertEqual( 'HstNmlInfoAllNameValid test 3-1', &
    & answer = .true., check = invalid )
  call AssertEqual( 'HstNmlInfoAllNameValid test 3-2', &
    & answer = 'Data4,Data9,Data8', check = trim(varnames) )

  call HstNmlInfoSetValidName( gthstnml = gthstnml10, & ! (in)
    & name = 'Data4,Data9,Data8' )                      ! (in)

  call HstNmlInfoAllNameValid( gthstnml = gthstnml10, & ! (in)
    & invalid = invalid, names = varnames )          ! (out)
  call AssertEqual( 'HstNmlInfoAllNameValid test 4-1', &
    & answer = .false., check = invalid )
  call AssertEqual( 'HstNmlInfoAllNameValid test 4-2', &
    & answer = '', check = trim(varnames) )

  do i = 1, 3
    nullify( history00 )
    call HstNmlInfoAssocGtHist( &
      & gthstnml = gthstnml10, & ! (in)
      & name = 'Data2', &        ! (in)
      & history = history00 )    ! (out)
    call HistoryPut( &
      & history = history00, &                ! (inout)
      & varname = 'Data2', &                  ! (in)
      & array = reshape( (/1, 2, 3, 4, 5, 6, 7, 8/) * i, &
      &                  (/4, 2/) ) )         ! (in)

    call HstNmlInfoAssocGtHist( &
      & gthstnml = gthstnml10, & ! (in)
      & name = 'Data3', &        ! (in)
      & history = history00 )    ! (out)
    call HistoryPut( &
      & history = history00, &                ! (inout)
      & varname = 'Data3', &                  ! (in)
      & array = (/10, 20, 30, 40/) * i )      ! (in)

    call HstNmlInfoAssocGtHist( &
      & gthstnml = gthstnml10, & ! (in)
      & name = 'Data4', &        ! (in)
      & history = history00 )    ! (out)
    call HistoryPut( &
      & history = history00, &     ! (inout)
      & varname = 'Data4', &       ! (in)
      & array = (/100, 200 /) * i) ! (in)

  end do

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data2', &        ! (in)
    & history = history00 )    ! (out)
  call HistoryClose( &
    & history = history00 ) ! (inout)

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data3', &        ! (in)
    & history = history00 )    ! (out)
  call AssertEqual( 'multi files output test 2-1', &
    & answer = .false., check = HistoryInitialized( history00 ) )

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml10, & ! (in)
    & name = 'Data4', &        ! (in)
    & history = history00 )    ! (out)
  call AssertEqual( 'multi files output test 2-2', &
    & answer = .false., check = HistoryInitialized( history00 ) )

  call HstNmlInfoClose( gthstnml = gthstnml10 ) ! (inout)


  !-------------------------------------------------------------------
  !  出力モードテストその3 (Close のエラー処理)
  !  Output mode test part 3 (error handling in Close)
  !-------------------------------------------------------------------
  call HstNmlInfoCreate( &
    & gthstnml = gthstnml11, &                    ! (out)
    & interval_value = 3.0_DP, &                     ! (in)
    & interval_unit = 'min', &                    ! (in)
    & precision = 'double', &                     ! (in)
    & fileprefix = 'xhistnmlinfo/' ) ! (in)

  call HstNmlInfoAdd( &
    & gthstnml = gthstnml11, &  ! (inout)
    & name = 'Data5,Data6' )    ! (in)

  call HstNmlInfoEndDefine( gthstnml = gthstnml11 ) ! (inout)

  do i = 1, 2
    select case (i)
    case (1) ; name = 'Data5'
    case (2) ; name = 'Data6'
    case default ; name = ''
    end select

    nullify( history00 )
    call HstNmlInfoAssocGtHist( &
      & gthstnml = gthstnml11, & ! (in)
      & name = name, &           ! (in)
      & history = history00 )    ! (out)
    call HstNmlInfoInquire( &
      & gthstnml = gthstnml11, &           ! (in)
      & name = name, &                     ! (in)
      & file = file, &                     ! (out)
      & interval_value = interval_value, &       ! (out)
      & interval_unit = interval_unit )          ! (out)
    call HistoryCreate( &
      & history = history00, &                                ! (out)
      & file = file, &                                        ! (in)
      & title = name, &                                       ! (in)
      & source = source, institution = institution, &         ! (in)
      & dims = StoA('lon', 'lat', 'time'), &                  ! (in)
      & dimsizes = (/4, 2, 0/), &                             ! (in)
      & longnames = StoA('longitude', 'latitude', 'time'), &  ! (in)
      & units = StoA('degree_east', 'degree_north', &
      &              interval_unit), &                        ! (in)
      & origin = 0.0, &                                       ! (in)
      & interval = real( interval_value ) )                   ! (in)

    call HistoryPut( &
      & history = history00, &                                 ! (inout)
      & varname = 'lon', array = (/0.0, 90.0, 180.0, 270.0/) ) ! (in)
    call HistoryPut( &
      & history = history00, &                     ! (inout)
      & varname = 'lat', array = (/-40.0, 40.0/) ) ! (in)

    call HistoryAddVariable( &
      & history = history00, &                ! (inout)
      & varname = name, &                     ! (in)
      & dims = StoA('lon', 'lat', 'time'), &  ! (in)
      & longname = 'sample ' // name, &       ! (in)
      & units = 'm s-1', xtype = precision )  ! (in)

    call HistoryPut( &
      & history = history00, &                ! (inout)
      & varname = name, &                     ! (in)
      & array = reshape( (/1, 2, 3, 4, 5, 6, 7, 8/) * i, &
      &                  (/4, 2/) ) )         ! (in)
  end do

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml11, & ! (in)
    & name = 'Data5', &        ! (in)
    & history = history00 )    ! (out)
  call HistoryClose( &
    & history = history00 ) ! (inout)

  call HstNmlInfoClose( &
    & gthstnml = gthstnml11, & ! (inout)
    & err = err )              ! (out)
  call AssertEqual( 'error handling in "Close" test 1-1', &
    & answer = .true., check = err )

  nullify( history00 )
  call HstNmlInfoAssocGtHist( &
    & gthstnml = gthstnml11, & ! (in)
    & name = 'Data6', &        ! (in)
    & history = history00 )    ! (out)
  call HistoryClose( &
    & history = history00 ) ! (inout)

  call HstNmlInfoClose( gthstnml = gthstnml11 ) ! (inout)

end program histnmlinfo
