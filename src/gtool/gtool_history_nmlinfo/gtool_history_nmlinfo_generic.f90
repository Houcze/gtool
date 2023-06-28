!= gtool_history_nmlinfo より提供される手続の引用仕様宣言
!= Interface of procedures provided from gtool_history_nmlinfo
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: gtool_history_nmlinfo_generic.f90,v 1.4 2009-10-10 10:59:01 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtool_history_nmlinfo_generic
  !
  != gtool_history_nmlinfo より提供される手続の引用仕様宣言
  != Interface of procedures provided from gtool_history_nmlinfo
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! モジュールの概要については, gtool_history_nmlinfo 
  ! を参照ください. 
  !
  ! See "gtool_history_nmlinfo" for brief of this module. 
  !
  !== Procedures List
  !
  ! HstNmlInfoCreate            :: gtool_history_nmlinfo_types#GTHST_NMLINFO 型変数の初期設定
  ! HstNmlInfoClose             :: gtool_history_nmlinfo_types#GTHST_NMLINFO 型変数の終了処理
  ! HstNmlInfoPutLine           :: gtool_history_nmlinfo_types#GTHST_NMLINFO 型変数に格納されている情報の印字
  ! HstNmlInfoInitialized       :: gtool_history_nmlinfo_types#GTHST_NMLINFO 型変数が初期設定されているか否か
  ! HstNmlInfoDefineMode        :: 定義モードの場合に真を返す
  ! HstNmlInfoEndDefine         :: 変数情報定義モードから出力モードへ変更
  ! HstNmlInfoReDefine          :: 出力モードから変数情報定義モードへ変更
  ! HstNmlInfoAdd               :: 変数情報の追加
  ! HstNmlInfoDelete            :: 変数情報の削除
  ! HstNmlInfoResetDefault      :: デフォルト設定のみに戻す
  ! HstNmlInfoInquire           :: 変数情報の問い合わせ
  ! HstNmlInfoAssocGtHist       :: 変数に応じた gtool_history_types#GT_HISTORY 型変数を返す
  ! HstNmlInfoOutputStepDisable :: output_step が常に .false. を返すよう設定する
  ! HstNmlInfoOutputStep        :: 出力の設定が有効である場合に真を返す
  ! HstNmlInfoOutputValid       :: 現在の時刻が出力のタイミングの場合に真を返す
  ! HstNmlInfoNames             :: 登録されている変数名リストを返す (関数) 
  ! HstNmlInfoGetNames          :: 登録されている変数名リストを返す (サブルーチン) 
  ! HstNmlInfoAllVarIniCheck    :: 初期設定されていない変数名のチェック
  ! HstNmlInfoSetValidName      :: 変数名の有効性を設定
  ! HstNmlInfoAllNameValid      :: 無効な変数名のチェック
  ! ------------      :: ------------
  ! HstNmlInfoCreate            :: Constructor of "gtool_history_nmlinfo_types#GTHST_NMLINFO"
  ! HstNmlInfoClose             :: Deconstructor of "gtool_history_nmlinfo_types#GTHST_NMLINFO"
  ! HstNmlInfoPutLine           :: Print information of "gtool_history_nmlinfo_types#GTHST_NMLINFO"
  ! HstNmlInfoInitialized       :: Check initialization of "gtool_history_nmlinfo_types#GTHST_NMLINFO"
  ! HstNmlInfoDefineMode        :: True is returned if current state is define mode
  ! HstNmlInfoEndDefine         :: Transit define mode about information of variables to output mode
  ! HstNmlInfoReDefine          :: Transit output mode to define mode about information of variables
  ! HstNmlInfoAdd               :: Add information of variables
  ! HstNmlInfoDelete            :: Delete information of variables
  ! HstNmlInfoResetDefault      :: Reset to default settings
  ! HstNmlInfoInquire           :: Inquire information of variables
  ! HstNmlInfoAssocGtHist       :: "gtool_history_types#GT_HISTORY" correspond to variable is returned
  ! HstNmlInfoOutputStepDisable :: Configure that "output_step" returns .false. already
  ! HstNmlInfoOutputStep        :: True is returned when a configuration of output is valid
  ! HstNmlInfoOutputValid       :: True is returned when current time is output timing
  ! HstNmlInfoNames             :: Return list of registerd variable identifiers (function)
  ! HstNmlInfoGetNames          :: Return list of registerd variable identifiers (subroutine)
  ! HstNmlInfoAllVarIniCheck    :: Check uninitialized variable names
  ! HstNmlInfoSetValidName      :: Set validation to variable names
  ! HstNmlInfoAllNameValid      :: Check invalid variable names
  !
  !== Usage
  !
  ! このモジュールは以下のような手順で用いてください. 
  !
  ! このモジュールを使用したサンプル Fortran プログラム
  ! 作成スクリプトが
  ! http://www.gfd-dennou.org/library/dcpam/dcpam4/dcpam4_current/script/f90/dcmodel_f90sample_maker.rb
  ! から入手できます. Ruby で記述されており, 実行することで
  ! サンプルとなる Fortran プログラムが作成されます. 
  ! 下記の解説のみでは実際の利用法やご利益が分かりにくいため, 
  ! サンプル Fortran プログラムを実際に見てみることをオススメします. 
  ! 
  ! 0. モジュール内で, gtool_history_nmlinfo_types#GTHST_NMLINFO 型の変数を定義しておきます. 
  !
  ! 1. HstNmlInfoCreate を用いて, gtool_history_nmlinfo_types#GTHST_NMLINFO 型の変数の初期設定を行います. 
  !    この際にデフォルトの出力間隔 *interval_value*, 
  !    *interval_unit*, 精度 *precision*, 時間の平均化 *time_average*, 
  !    出力ファイル名接頭詞 *fileprefix* を設定します. 
  !
  ! 2. プログラムがデフォルトで出力する変数がある場合, Add を
  !    使用して登録してください. 
  !    *name* には変数名を与えます. *name* は変数を識別するためのキーと
  !    して利用するため, 異なる変数に対して同じ *name* を指定しないで
  !    ください. 
  !    *file* には出力ファイル名を与えます. 与えない場合, 
  !    適当なファイル名が設定されます. 
  !    その他の情報は上記と同様です. 
  !
  ! 3. NAMELIST から得られた出力変数の情報を, 
  !    HstNmlInfoAdd を使用して登録してください. 
  !    HstNmlInfoAdd で既に登録済みの *name* を再度登録することで, 
  !    設定が上書きされます. 
  !
  ! 4. 登録が完了したら, HstNmlInfoEndDefine を用いて, 
  !    定義モードから出力モードへ移行してください. 
  !
  ! 5. gtool_history_generic#HistoryCreate, gtool_history_generic#HistoryAddVariable
  !    gtool_history_generic#HistoryPut 等で出力設定およびデータ出力を行う際には, 
  !    HstNmlInfoAssocGtHist に対し, 変数名 *name* と
  !    gtool_history_types#GT_HISTORY 型のポインタ *history* を渡してください. 
  !    gtool_history_nmlinfo_types#GTHST_NMLINFO 型の変数に登録されている
  !    *name* に関する gtool_history_types#GT_HISTORY 型変数に結合された
  !    *history* が返ります. この *history* 
  !    を上記 gtool_history のサブルーチン群の引数 *history* に渡して
  !    出力設定およびデータ出力を行ってください. 
  !    gtool_history_generic#HistoryCreate に必要な
  !    出力間隔や精度は HstNmlInfoInquire を用いて得ることができます. 
  !    使用後は, NULLIFY によって *history* を空状態にしてください. 
  !    (DEALLOCATE を使用すると出力に関する情報が失われるため, 
  !    使用しないでください). 
  !
  !    それぞれの変数に関して, 出力設定が有効かどうかについては, 
  !    HstNmlInfoOutputValid で知ることが可能です. 
  !
  !    また, 時間積分中に gtool_history_generic#HistoryPut を使用する際
  !    に, 現在時刻が出力タイミングかどうかについては, 
  !    HstNmlInfoOutputStep で知ることが可能です. 
  !
  ! 6. ファイルの出力が終了したら, 上記の手順と同様に
  !    gtool_history_types#GT_HISTORY 型の *history* を取得し, 
  !    gtool_history_generic#HistoryClose によって終了処理を行ってください. 
  !
  ! 7. 最後に, HstNmlInfoClose によって, gtool_history_nmlinfo_types#GTHST_NMLINFO 型の変数の
  !    終了処理を行います.
  !
  !
  ! Use this module as follows. 
  !
  ! Sample Fortran programs generator (Ruby script) is available from 
  ! http://www.gfd-dennou.org/library/dcpam/dcpam4/dcpam4_current/script/f90/dcmodel_f90sample_maker.rb .
  ! Sample Fortran programs are created by executing this script. 
  ! Because neither actual usage nor the profit are understood 
  ! easily only from the following explanations, 
  ! It is recommended to see sample Fortran programs actually.
  ! 
  ! 0. Declare "gtool_history_nmlinfo_types#GTHST_NMLINFO" variable in the module. 
  !
  ! 1. Initialize "gtool_history_nmlinfo_types#GTHST_NMLINFO" variable by "HstNmlInfoCreate". 
  !    On this occasion, configure default 
  !    *interval_value*, *interval_unit*, 
  !    *precision*, *time_average*, *fileprefix* (prefix of output file). 
  !
  ! 2. Register by using "HstNmlInfoAdd" when there are variables that 
  !    the program outputs by default. 
  !    variable identifier is given to *name*. Do not specify same *name* 
  !    for different variables because *name* is used as a key to 
  !    identify the variable. The output file name is given to *file*. 
  !    A suitable file name is set when not giving it. 
  !    The extra information is similar to the above-mentioned. 
  !
  ! 3. Register information of output variables obtained from NAMELIST
  !    by using "HstNmlInfoAdd". When registered *name* is registered again, 
  !    the setting concerning the *name* has been overwritten. 
  !
  ! 4. Shift from the define mode to output mode by using "HstNmlInfoEndDefine"
  !    when registration is completed. 
  !
  ! 5. Pass "HstNmlInfoAssocGtHist" variable identifier *name* and 
  !    *history* of "gtool_history_types#GT_HISTORY" pointer when 
  !    setting output and data output is performed with 
  !    "gtool_history_generic#HistoryCreate", "gtool_history_generic#HistoryAddVariable" 
  !    "gtool_history_generic#HistoryPut" etc. 
  !    *history* is associated to "gtool_history_types#GT_HISTORY" correspond 
  !    to *name* stored in "gtool_history_nmlinfo_types#GTHST_NMLINFO" variable. 
  !    Pass the *history* to subroutines in "gtool_history" above-mentioned, 
  !    and configure output setting and output data. 
  !    Necessary output interval and precision for 
  !    "gtool_history_generic#HistoryCreate" can be obtained by using 
  !    "HstNmlInfoInquire". 
  !    Please put *history* into a null state by NULLIFY after use. 
  !    (Information of output is lost when DEALLOCATE is used, 
  !    so do not use it). 
  !
  !    It can know whether the output setting is effective 
  !    for each variable with "output_valid". 
  !
  !    Moreover, it can know time now to be whether output timing 
  !    when "gtool_history_generic#HistoryPut" is used 
  !    while integrating time with "output_step". 
  !
  ! 6. Acquire *history* of "gtool_history_types#GT_HISTORY" type as well as
  !    the above-mentioned procedure, and terminate that by 
  !    "gtool_history_generic#HistoryClose" when the output of the file ends. 
  !
  ! 7. Finally, the termination of the variable of "gtool_history_nmlinfo_types#GTHST_NMLINFO" 
  !    type is done by "Close". 
  !

  implicit none
  private
  public:: HstNmlInfoCreate, HstNmlInfoClose
  public:: HstNmlInfoPutLine, HstNmlInfoInitialized
  public:: HstNmlInfoDefineMode, HstNmlInfoEndDefine, HstNmlInfoReDefine
  public:: HstNmlInfoAdd, HstNmlInfoDelete
  public:: HstNmlInfoResetDefault
  public:: HstNmlInfoInquire
  public:: HstNmlInfoAssocGtHist
  public:: HstNmlInfoOutputStepDisable
  public:: HstNmlInfoOutputStep
  public:: HstNmlInfoOutputValid
  public:: HstNmlInfoNames, HstNmlInfoGetNames
  public:: HstNmlInfoAllVarIniCheck
  public:: HstNmlInfoSetValidName
  public:: HstNmlInfoAllNameValid

  !-----------------------------------------------------------------
  !  公開手続
  !  Public procedures
  !-----------------------------------------------------------------

  interface HstNmlInfoCreate
    subroutine HstNmlInfoCreate( gthstnml, &
      & interval_value, &
      & interval_unit, &
      & precision, &
      & time_average, average, &
      & fileprefix, &
      & origin_value, origin_unit, &
      & terminus_value, terminus_unit, &
      & slice_start, slice_end, slice_stride, &
      & space_average, &
      & newfile_intvalue, newfile_intunit, &
      & err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      use dc_types, only: DP
      type(GTHST_NMLINFO), intent(inout):: gthstnml
      real(DP), intent(in), optional:: interval_value
      character(*), intent(in), optional:: interval_unit
      character(*), intent(in), optional:: precision
      logical, intent(in), optional:: time_average
      logical, intent(in), optional:: average
      character(*), intent(in), optional:: fileprefix
      real(DP), intent(in), optional:: origin_value
      character(*), intent(in), optional:: origin_unit
      real(DP), intent(in), optional:: terminus_value
      character(*), intent(in), optional:: terminus_unit
      integer, intent(in), optional:: slice_start(:)
      integer, intent(in), optional:: slice_end(:)
      integer, intent(in), optional:: slice_stride(:)
      logical, intent(in), optional:: space_average(:)
      integer, intent(in), optional:: newfile_intvalue
      character(*), intent(in), optional:: newfile_intunit
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoCreate
  end interface

  interface HstNmlInfoClose
    subroutine HstNmlInfoClose( gthstnml, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(inout):: gthstnml
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoClose
  end interface

  interface HstNmlInfoPutLine
    subroutine HstNmlInfoPutLine( gthstnml, unit, indent, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
      integer, intent(in), optional:: unit
      character(*), intent(in), optional:: indent
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoPutLine
  end interface

  interface HstNmlInfoInitialized
    logical function HstNmlInfoInitialized( gthstnml ) result(result)
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
    end function HstNmlInfoInitialized
  end interface

  interface HstNmlInfoDefineMode
    logical function HstNmlInfoDefineMode( gthstnml ) result(result)
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
    end function HstNmlInfoDefineMode
  end interface

  interface HstNmlInfoEndDefine
    subroutine HstNmlInfoEndDefine( gthstnml, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(inout):: gthstnml
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoEndDefine
  end interface

  interface HstNmlInfoReDefine
    subroutine HstNmlInfoReDefine( gthstnml, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(inout):: gthstnml
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoReDefine
  end interface

  interface HstNmlInfoAdd
    recursive subroutine HstNmlInfoAdd( gthstnml, &
      & name, file, &
      & interval_value, interval_unit, &
      & precision, &
      & time_average, average, &
      & fileprefix, &
      & origin_value, origin_unit, &
      & terminus_value, terminus_unit, &
      & slice_start, slice_end, slice_stride, &
      & space_average, &
      & newfile_intvalue, newfile_intunit, &
      & err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      use dc_types, only: DP
      type(GTHST_NMLINFO), intent(inout):: gthstnml
      character(*), intent(in), optional:: name
      character(*), intent(in), optional:: file
      real(DP), intent(in), optional:: interval_value
      character(*), intent(in), optional:: interval_unit
      character(*), intent(in), optional:: precision
      logical, intent(in), optional:: time_average
      logical, intent(in), optional:: average
      character(*), intent(in), optional:: fileprefix
      real(DP), intent(in), optional:: origin_value
      character(*), intent(in), optional:: origin_unit
      real(DP), intent(in), optional:: terminus_value
      character(*), intent(in), optional:: terminus_unit
      integer, intent(in), optional:: slice_start(:)
      integer, intent(in), optional:: slice_end(:)
      integer, intent(in), optional:: slice_stride(:)
      logical, intent(in), optional:: space_average(:)
      integer, intent(in), optional:: newfile_intvalue
      character(*), intent(in), optional:: newfile_intunit
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoAdd
  end interface

  interface HstNmlInfoDelete
    recursive subroutine HstNmlInfoDelete( gthstnml, name, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(inout):: gthstnml
      character(*), intent(in):: name
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoDelete
  end interface

  interface HstNmlInfoResetDefault
    subroutine HstNmlInfoResetDefault( gthstnml, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(inout):: gthstnml
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoResetDefault
  end interface

  interface HstNmlInfoInquire
    subroutine HstNmlInfoInquire( gthstnml, &
      & name, &
      & file, &
      & interval_value, &
      & interval_unit, &
      & precision, &
      & time_average, average, &
      & fileprefix, &
      & origin_value, origin_unit, &
      & terminus_value, terminus_unit, &
      & slice_start, slice_end, slice_stride, &
      & space_average, &
      & newfile_intvalue, newfile_intunit, &
      & err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      use dc_types, only: DP
      type(GTHST_NMLINFO), intent(in):: gthstnml
      character(*), intent(in), optional:: name
      character(*), intent(out), optional:: file
      real(DP), intent(out), optional:: interval_value
      character(*), intent(out), optional:: interval_unit
      character(*), intent(out), optional:: precision
      logical, intent(out), optional:: time_average
      logical, intent(out), optional:: average
      character(*), intent(out), optional:: fileprefix
      real(DP), intent(out), optional:: origin_value
      character(*), intent(out), optional:: origin_unit
      real(DP), intent(out), optional:: terminus_value
      character(*), intent(out), optional:: terminus_unit
      integer, intent(out), optional:: slice_start(:)
      integer, intent(out), optional:: slice_end(:)
      integer, intent(out), optional:: slice_stride(:)
      logical, intent(out), optional:: space_average(:)
      integer, intent(out), optional:: newfile_intvalue
      character(*), intent(out), optional:: newfile_intunit
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoInquire
  end interface

  interface HstNmlInfoAssocGtHist
    subroutine HstNmlInfoAssocGtHist( gthstnml, &
      & name, history, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      use gtool_history, only: GT_HISTORY
      type(GTHST_NMLINFO), intent(in):: gthstnml
      character(*), intent(in):: name
      type(GT_HISTORY), pointer:: history
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoAssocGtHist
  end interface

  interface HstNmlInfoOutputStepDisable
    subroutine HstNmlInfoOutputStepDisable( gthstnml, &
      & name, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
      character(*), intent(in):: name
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoOutputStepDisable
  end interface

  interface HstNmlInfoOutputStep
    logical function HstNmlInfoOutputStep( gthstnml, &
      & name, time ) result(result)
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      use gtool_history, only: GT_HISTORY
      use dc_date_types, only: DC_DIFFTIME
      type(GTHST_NMLINFO), intent(in):: gthstnml
      character(*), intent(in):: name
      type(DC_DIFFTIME), intent(in):: time
    end function HstNmlInfoOutputStep
  end interface

  interface HstNmlInfoOutputValid
    logical function HstNmlInfoOutputValid( gthstnml, &
      & name ) result(result)
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
      character(*), intent(in):: name
    end function HstNmlInfoOutputValid
  end interface

  interface HstNmlInfoNames
    function HstNmlInfoNames( gthstnml ) result(result)
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      use dc_types, only: STRING
      character(STRING):: result
      type(GTHST_NMLINFO), intent(in):: gthstnml
    end function HstNmlInfoNames
  end interface

  interface HstNmlInfoGetNames
    subroutine HstNmlInfoGetNames( gthstnml, varnames_ary, err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      use dc_types, only: TOKEN
      type(GTHST_NMLINFO), intent(in):: gthstnml
      character(TOKEN), pointer:: varnames_ary(:)
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoGetNames
  end interface

  interface HstNmlInfoAllVarIniCheck
    subroutine HstNmlInfoAllVarIniCheck( gthstnml, &
      & invalid, &
      & names, &
      & err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
      logical, intent(out):: invalid
      character(*), intent(out):: names
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoAllVarIniCheck
  end interface

  interface HstNmlInfoSetValidName
    subroutine HstNmlInfoSetValidName( gthstnml, &
      & name, &
      & err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
      character(*), intent(in):: name
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoSetValidName
  end interface

  interface HstNmlInfoAllNameValid
    subroutine HstNmlInfoAllNameValid( gthstnml, &
      & invalid, &
      & names, &
      & err )
      use gtool_history_nmlinfo_types, only: GTHST_NMLINFO
      type(GTHST_NMLINFO), intent(in):: gthstnml
      logical, intent(out):: invalid
      character(*), intent(out):: names
      logical, intent(out), optional:: err
    end subroutine HstNmlInfoAllNameValid
  end interface

end module gtool_history_nmlinfo_generic
