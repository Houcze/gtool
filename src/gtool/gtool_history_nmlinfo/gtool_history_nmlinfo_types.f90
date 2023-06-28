!= gtool_history_nmlinfo から提供される構造データ型
!= Derived data type of "gtool_history_nmlinfo"
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: gtool_history_nmlinfo_types.f90,v 1.3 2009-06-01 15:17:18 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtool_history_nmlinfo_types
  !
  != gtool_history_nmlinfo から提供される構造データ型
  != Derived data type of "gtool_history_nmlinfo"
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! モジュールの概要については, gtool_history_nmlinfo 
  ! を参照ください. 
  !
  ! See "gtool_history_nmlinfo" for brief of this module. 
  !
  !== Derived types List
  !
  ! GTHST_NMLINFO        :: NAMELIST から入力した情報を収めた構造データ型. 
  !                         利用者への提供用. 
  ! GTHST_NMLINFO_ENTRY  :: GTHST_NMLINFO 内での個々の変数のためのエントリ. 
  !                         内部用 (利用者は直接参照しない). 
  ! ------------         :: ------------
  ! GTHST_NMLINFO        :: Derived type that stores information input from NAMELIST. 
  !                         This derived type is provided for users. 
  ! GTHST_NMLINFO_ENTRY  :: Entry for individual variable in "GTHST_NMLINFO". 
  !                         This is used internally. 
  !                         (Users do not refer this derived type). 
  !

  use gtool_history, only: GT_HISTORY
  use dc_types, only: TOKEN, STRING
  implicit none
  public:: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY

  type GTHST_NMLINFO
    !
    ! NAMELIST から取得したヒストリデータの出力情報
    ! を格納するための構造データ型です. 
    ! まず, gtool_history_nmlinfo_generic#HstNmlInfoCreate 
    ! で "GTHST_NMLINFO" 型の変数を初期設定して下さい.
    ! 初期設定された "GTHST_NMLINFO" 型の変数を再度利用する際には,
    ! gtool_history_nmlinfo_generic#HstNmlInfoClose 
    ! によって終了処理を行ってください.
    !
    ! This derived type is worked in order to store information about 
    ! data output from NAMELIST. 
    ! Initialize "GTHST_NMLINFO" variable by 
    ! "gtool_history_nmlinfo_generic#HstNmlInfoCreate" before usage.
    ! If you reuse "GTHST_NMLINFO" variable again for another application, 
    ! terminate by "gtool_history_nmlinfo_generic#HstNmlInfoClose".
    !
    logical:: initialized = .false.
                              ! 初期設定フラグ. 
                              ! Initialization flag
    logical:: define_mode = .true.
                              ! 定義状態を表すフラグ. 
                              ! Flag that represents define mode
    type(GTHST_NMLINFO_ENTRY), pointer:: gthstnml_list =>null()
                              ! 変数ごとの情報リスト. 
                              ! 格納される情報については
                              ! GTHST_NMLINFO_ENTRY を参照のこと. 
                              !
                              ! Information list about individual variable 
                              ! See "GTHST_NMLINFO_ENTRY" 
                              ! about stored information. 
  end type GTHST_NMLINFO

  type GTHST_NMLINFO_ENTRY
    !
    ! 出力変数ごとの情報を格納するための構造体です. 
    ! この構造体はモジュール内で使用されることを想定しているため, 
    ! モジュール外部からは使用しないでください. 
    !
    ! Information about individual output variable is stored in
    ! this derived type.
    ! It is expected that this derived type is used
    ! internally, so do not refer from the outside.
    !
    character(TOKEN):: name
                              ! 変数名. Variable identifier
    character(STRING):: file
                              ! ヒストリデータのファイル名. 
                              ! History data filenames
    real, pointer:: interval_value =>null()
                              ! ヒストリデータの出力間隔の数値. 
                              ! 負の値を与えると, 出力を抑止します. 
                              ! 
                              ! Numerical value for interval of history data output. 
                              ! Negative values suppresses output.
    character(TOKEN), pointer:: interval_unit =>null()
                              ! ヒストリデータの出力間隔の単位. 
                              ! Unit for interval of history data output
    character(TOKEN), pointer:: precision =>null()
                              ! ヒストリデータの精度. 
                              ! Precision of history data
    logical, pointer:: time_average =>null()
                              ! 出力データの時間平均化フラグ. 
                              ! Flag for time average of output data.
    character(STRING), pointer:: fileprefix =>null()
                              ! ヒストリデータのファイル名の接頭詞. 
                              ! Prefixes of history data filenames

    real, pointer:: origin_value =>null()
                              ! 出力開始時刻. 
                              ! Start time of output. 
    character(TOKEN), pointer:: origin_unit =>null()
                              ! 出力開始時刻の単位. 
                              ! Unit of start time of output. 
    real, pointer:: terminus_value =>null()
                              ! 出力終了時刻. 
                              ! End time of output. 
    character(TOKEN), pointer:: terminus_unit =>null()
                              ! 出力終了時刻の単位. 
                              ! Unit of end time of output. 
    integer, pointer:: slice_start(:) =>null()
                              ! 空間方向の開始点. 
                              ! Start points of spaces. 
    integer, pointer:: slice_end(:) =>null()
                              ! 空間方向の終了点. 
                              ! End points of spaces. 
    integer, pointer:: slice_stride(:) =>null()
                              ! 空間方向の刻み幅. 
                              ! Strides of spaces. 
    logical, pointer:: space_average(:) =>null()
                              ! 平均化のフラグ. 
                              ! Flag of average. 
    integer, pointer:: newfile_intvalue =>null()
                              ! ファイル分割時間間隔. 
                              ! Interval of time of separation of a file. 
    character(TOKEN), pointer:: newfile_intunit =>null()
                              ! ファイル分割時間間隔の単位. 
                              ! Unit of interval of time of separation of a file. 

    logical:: output_step_disable = .false.
                              ! output_step 無効化フラグ. 
                              ! "output_step" disable flag
    logical:: name_invalid = .true.
                              ! 無効な変数名を検知するためのフラグ.
                              ! A flag for detection of invalid variable names
    type(GT_HISTORY), pointer:: history =>null()
                              ! gtool_history モジュール用構造体. 
                              ! Derived type for "gtool_history" module
    type(GTHST_NMLINFO_ENTRY), pointer:: next =>null()
                              ! リスト構造のための変数. 
                              ! A variable for a list structure
  end type GTHST_NMLINFO_ENTRY

end module gtool_history_nmlinfo_types
