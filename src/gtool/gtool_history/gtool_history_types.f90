!
!= gtool_history より提供される構造データ型
!= Derived types provided from "gtool_history"
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtool_history_types.f90,v 1.5 2009-10-10 08:01:52 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!

module gtool_history_types
  !
  != gtool_history より提供される構造データ型
  != Derived types provided from "gtool_history"
  !
  ! <b>Note that Japanese and English are described in parallel.</b>
  !
  ! モジュールの概要や使用方法については, gtool_history
  ! を参照ください.
  !
  ! See "gtool_history" for brief and usage of this module.
  !
  !== Derived types
  !
  ! GT_HISTORY          :: gtool4 データ出力用
  ! GT_HISTORY_AXIS     :: gtool4 データ座標軸情報
  ! GT_HISTORY_VARINFO  :: gtool4 データ変数情報
  !

  use dc_types, only: STRING, TOKEN, STDERR, DP, SP
  use dc_date_types, only: DC_DIFFTIME, UNIT_SYMBOL_ERR
  use gtdata_types, only: GT_VARIABLE
  implicit none
  private

  public:: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO
  public:: GT_HISTORY_ATTR, GT_HISTORY_AVRDATA
  public:: GT_HISTORY_MPIFILEINFO, GT_HISTORY_MPIAXISDATA
  public:: GT_HISTORY_MPIAXISINFO, GT_HISTORY_MPIVARINDEX

  type GT_HISTORY
    !
    !== gtool4 netCDF データの出力用構造体
    !
    ! この型の変数は HistoryCreate によって初期設定される必要があります。
    ! 初期設定後、データ出力用の複数のサブルーチンによって利用されます。
    ! 最終的には HistoryClose によって終了処理してください。
    !
    ! この構造体の内部の要素は非公開になっています。
    ! 問い合わせの際には HistoryInquire を利用してください。
    !
    !
    ! Data entity of this type represents a netCDF dataset
    ! controlled by gtool5 library.
    ! It must be initialized by HistoryCreate ,
    ! then used in many subroutines, and must be finalized by
    ! HistoryClose .
    ! Note that the resultant file is undefined if you forget to
    ! finalize it.
    !
    ! Users are recommended to retain the object of this type
    ! returned by HistoryCreate,
    ! to use it as the last argument called *history* for
    ! all following subroutine calls.
    ! However, it is not mandatory.
    ! When you are going to write *ONLY* one dataset,
    ! argument *history* of all subroutine calls can be omitted, and
    ! the history entity will be internally managed within this module.

    logical:: initialized = .false.
    ! 初期設定フラグ.
    ! Initialization flag
    integer:: unlimited_index = 0
    ! 無制限次元の ID.
    ! ID of unlimited dimension.
    character(TOKEN):: unlimited_units = ''
    ! 無制限次元の単位.
    ! Units of unlimited dimension.
    integer:: unlimited_units_symbol = UNIT_SYMBOL_ERR
    ! 無制限次元の単位.
    ! Units of unlimited dimension.
    type(GT_VARIABLE), pointer:: dimvars(:) =>null()
    ! 次元変数 ID配列.
    ! it is index of dimvars(:),
    ! not that of vars(:).
    logical, pointer:: dim_value_written(:) =>null()
    ! 各次元が記述済みかどうか
    real(DP):: origin, interval, newest, oldest
    ! 単位は HistoryCreate の units 中で無制限次元
    ! に対応するものになる.
    logical:: origin_setting = .false.
    ! 時間の原点が明示的に設定されたか.
    type(GT_VARIABLE), pointer:: vars(:) =>null()
    ! 変数 ID 配列
    integer, pointer:: growable_indices(:) =>null()
    ! 無制限次元の添字
    ! (無制限次元が無い時は 0)
    integer, pointer:: count(:) =>null()
    ! 各配列の無制限次元の配列長
    integer, pointer:: var_avr_count(:) =>null()
    ! 各変数の時間平均値出力の際の積算回数.
    ! -1 の場合は出力データを平均化しない.
    !
    ! Number of times of integral
    ! for time-averaged value output of each variable.
    ! -1 disables average value output
    type(GT_HISTORY_AVRDATA), pointer:: var_avr_data(:) =>null()
    ! 時間平均値を出力するためのデータ一時保管用配列.
    !
    ! Array for temporary keeping data for
    ! time-averaged value output
    logical, pointer:: var_avr_firstput(:) =>null()
    ! 一度目の出力を示すフラグ.
    !
    ! Flag for first output
    real(DP), pointer:: var_avr_coefsum(:) =>null()
    ! 各変数の時間平均値蓄積の際の係数の合計値.
    !
    ! Summation of coefficients for integral
    ! of time-averaged value of each variable
    real(DP), pointer:: var_avr_baseint(:) =>null()
    ! 各変数の時間平均値出力のための基本時間間隔.
    !
    ! Basic interval of time for output
    ! of time-averaged value of each variable
    real(DP), pointer:: var_avr_prevtime(:) =>null()
    ! 各変数の時間平均値出力のための前回の時刻保管.
    !
    ! Store keeping of previous time for output
    ! of time-averaged value of each variable
    real(DP):: time_bnds(1:2) = 0.0_DP
    ! "time_bnds" 変数に出力されるデータ.
    !
    ! Data that is to be output in "time_bnds"
    ! variable
    integer:: time_bnds_output_count = 0
    ! "time_bnds" 変数に出力された回数.
    !
    ! Number of output in "time_bnds" variable
    integer:: time_nv_index = 0
    ! time_nv 次元の ID.
    ! ID of dimension "time_nv"

    ! MPI 関連の変数
    ! Variables for MPI
    !
    integer:: mpi_myrank = -1
    ! MPI におけるノード番号. Node number of MPI
    integer:: mpi_nprocs = -1
    ! MPI における全ノード数. Number of all nodes of MPI
    logical:: mpi_gather = .false.
    ! ファイルを統合して出力するフラグ.
    ! Flag for integration of files
    logical:: mpi_split = .false.
    ! ファイルを分割して出力するフラグ.
    ! Flag for split of files
    type(GT_HISTORY_MPIFILEINFO), pointer:: mpi_fileinfo =>null()
    ! ファイルに関する情報
    ! Information about file
    type(GT_HISTORY_MPIAXISDATA), pointer:: mpi_dimdata_all(:) =>null()
    ! 全体の軸データを保管するための配列.
    ! Array for keeping data of axes in whole area.
    type(GT_HISTORY_MPIAXISDATA), pointer:: mpi_dimdata_each(:) =>null()
    ! 個々のノードでの軸データを保管するための配列.
    ! Array for keeping data of axes on each node.
    type(GT_HISTORY_MPIAXISINFO), pointer:: mpi_gthr_info(:) =>null()
    ! データを一箇所に集約する際に必要な情報.
    ! Information for integration of data.
    type(GT_HISTORY_VARINFO), pointer:: mpi_varinfo(:) =>null()
    ! 変数情報.
    ! Information of variables
    type(GT_HISTORY_MPIVARINDEX), pointer:: mpi_vars_index(:) =>null()
    ! 各変数の配列添字情報.
    ! Indexes of array of each variable
  end type GT_HISTORY

  type GT_HISTORY_AVRDATA
    !
    ! 時間方向の平均値を出力するためのデータ一時保管用配列.
    !
    ! Array for temporary keeping data for time average value output.
    !
    real(DP), pointer:: a_DataAvr(:) =>null()
    integer:: length
  end type GT_HISTORY_AVRDATA

  type(GT_HISTORY), save, target:: default  ! history が未指定の場合に使用

  type GT_HISTORY_AXIS
    !
    !== 座標軸情報を格納する構造体
    !
    ! この型の変数は HistoryAxisCreate, HistoryAxisCopy, HistoryInquire
    ! によって初期設定される必要があります。
    ! 初期設定後、HistoryCreate の *axes* に与えます。
    !
    ! 問い合わせは HistoryAxisInquire によって行います。
    ! 属性の付加は HistoryAxisAddAttr によって行います。
    ! 初期化は HistoryAxisClear によって行います。
    !
    ! This type may be used as a argument *axes* of HistoryCreate
    ! to define features of axes of a history dataset.
    ! Typically, a constant array of this type will be used for
    ! fixed specification.
    !
    character(TOKEN) :: name     = "" ! 次元変数名
    integer          :: length   = 0  ! 次元長 (配列サイズ)
    character(STRING):: longname = "" ! 次元変数の記述的名称
    character(STRING):: units    = "" ! 次元変数の単位
    character(TOKEN) :: xtype    = "" ! 次元変数の型
    type(GT_HISTORY_ATTR), pointer:: attrs(:) =>null() ! 属性情報群
  end type GT_HISTORY_AXIS


  type GT_HISTORY_VARINFO
    !
    !== 変数情報を格納する構造体
    !
    ! この型の変数は HistoryVarinfoCreate, HistoryVarinfoCopy,
    ! HistoryInquire
    ! によって初期設定される必要があります。
    ! 初期設定後、HistoryAddVariable の *varinfo* に与えます。
    !
    ! 問い合わせは HistoryVarinfoInquire によって行います。
    ! 属性の付加は HistoryVarinfoAddAttr によって行います。
    ! 初期化は HistoryVarinfoClear によって行います。
    !
    ! This type may be used as a argument *varinfo* of
    ! HistoryAddVariable
    ! to define features of variable of a history dataset.
    !
    character(TOKEN)          :: name     = ""     ! 変数名
    character(TOKEN), pointer :: dims(:)  =>null() ! 依存する次元
    character(STRING)         :: longname = ""     ! 変数の記述的名称
    character(STRING)         :: units    = ""     ! 変数の単位
    character(TOKEN)          :: xtype    = ""     ! 変数の型
    type(GT_HISTORY_ATTR), pointer:: attrs(:) =>null() ! 属性情報群
    logical:: time_average = .false.               ! 時間平均
    logical:: initialized = .false.
    ! 初期設定フラグ.
    ! Initialization flag
  end type GT_HISTORY_VARINFO

  type GT_HISTORY_ATTR
    !
    ! 変数の属性情報の構造体. 外部参照はさせず, GT_HISTORY_VARINFO
    ! および GT_HISTORY_AXIS に内包されて利用されることを
    ! 想定している. 直接的にこの構造体を変数にとる
    ! サブルーチンは gtool_history_internal モジュール内の
    ! gtool_history_internal#append_attrs および
    ! gtool_history_internal#copy_attrs .
    !
    character(TOKEN)    :: attrname                ! 属性の名前
    character(TOKEN)    :: attrtype                ! 属性の値の型
    logical             :: array = .false.         ! 属性の値が配列かどうか
    character(STRING)   :: Charvalue               ! 属性の値 (文字型変数)
    integer             :: IntValue                ! 属性の値 (整数型変数)
    real(SP)            :: RealValue               ! 属性の値 (単精度実数型変数)
    real(DP)            :: DoubleValue             ! 属性の値 (倍精度実数型変数)
    logical             :: Logicalvalue            ! 属性の値 (論理型変数)
    integer,  pointer   :: IntArray(:) =>null()    ! 属性の値 (整数型配列)
    real(SP), pointer   :: RealArray(:) =>null()   ! 属性の値 (単精度実数型配列)
    real(DP), pointer   :: DoubleArray(:) =>null() ! 属性の値 (倍精度実数型配列)
  end type GT_HISTORY_ATTR

  type GT_HISTORY_MPIFILEINFO
    !
    ! MPI 使用時に, ファイルに書き出す基本情報.
    ! (各ノード上のデータを統合して一つのファイルに出力する際に使用)
    !
    ! Basic information output to a file  when MPI is used.
    ! (This is used when data on each node is integrated and output to one file )
    !
    logical:: already_output = .false.
    type(GT_HISTORY_AXIS), pointer:: axes(:) =>null()
    logical:: overwrite
    character(STRING):: file
    character(STRING):: title
    character(STRING):: source
    character(STRING):: institution
    character(STRING):: conventions
    character(TOKEN):: gt_version
    logical:: gtver_add
    logical:: quiet
    character(STRING):: nc_history
  end type GT_HISTORY_MPIFILEINFO

  type GT_HISTORY_MPIAXISDATA
    !
    ! MPI 使用時に, 軸データを保管するための配列.
    !
    ! Array for keeping data of axes when MPI is used.
    !
    real(DP), pointer:: a_Axis(:) =>null()
    integer:: length = -1
    type(GT_HISTORY_ATTR), pointer:: attrs(:) =>null()
    ! 座標の属性 (HistoryCreate2 が呼ばれた場合に使用)
    ! Attributes of axes (this is used when HistoryCreate2 is used)
  end type GT_HISTORY_MPIAXISDATA

  type GT_HISTORY_MPIAXISINFO
    !
    ! MPI 使用時に, データを一箇所に集約する際に必要な情報.
    !
    ! Information for integration of data when MPI is used.
    !
    integer, pointer:: index_all(:,:) =>null()
    integer, pointer:: length(:) =>null()
  end type GT_HISTORY_MPIAXISINFO

  type GT_HISTORY_MPIVARINDEX
    !
    ! MPI 使用時に, データを一箇所に集約する際に必要な情報.
    !
    ! Information for integration of data when MPI is used.
    !
    integer, pointer:: each2all(:,:) =>null()
    integer, pointer:: allcount(:) =>null()
    integer:: allcount_all = 0
  end type GT_HISTORY_MPIVARINDEX

end module gtool_history_types
