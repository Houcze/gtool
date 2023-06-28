!= gtool_historyauto の初期設定
!= Initialization of gtool_historyauto
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historyautocreate.f90,v 1.7 2010-01-16 14:14:16 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2008-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryAutoCreate3( &
    & title, source, institution, &           ! (in)
    & dims, dimsizes, longnames, units, &     ! (in)
    & origin, terminus, &                     ! (in)
    & xtypes, conventions, gt_version, &      ! (in) optional
    & all_output, &                           ! (in) optional
    & file_prefix, &                          ! (in) optional
    & namelist_filename, &                    ! (in) optional
    & interval, &                             ! (in) optional
    & slice_start, slice_end, slice_stride, & ! (in) optional
    & space_average, &                        ! (in) optional
    & time_average, &                         ! (in) optional
    & newfile_interval, &                     ! (in) optional
    & rank, &                                 ! (in) optional
    & origin_date, origin_date_invalid, &     ! (in) optional
    & start_date, cal, &                      ! (in) optional
    & flag_mpi_gather, flag_mpi_split &       ! (in) optional
    & )
    !
    ! 複数のデータ出力を行うための初期化を行います.
    !
    ! この HistoryAutoCreate には, モデル内で出力する
    ! 変数が依存する座標や座標重みなどを全てを設定してください.
    !
    ! all_output に .true. を与えた場合や,
    ! namelist_filename を与えない (空文字を与える) 場合には,
    ! HistoryAutoAddVariable で登録される全ての変数が出力されます.
    ! 一方で namelist_filename に NAMELIST ファイル名を与える場合には,
    ! その NAMELIST ファイルから出力のオンオフや,
    ! 出力ファイル名, 出力間隔などを変更可能です.
    ! 変更可能な項目に関しては NAMELIST#gtool_historyauto_nml
    ! を参照して下さい.
    !
    ! interval, origin, terminus, slice_start, slice_end, slice_stride,
    ! space_average, time_average, newfile_interval
    ! などの設定はデフォルト値として使用されます.
    ! これらの設定値は HistoryAutoAddVariable および
    ! NAMELIST#gtool_historyauto_nml で上書きされます.
    ! (優先度が高いのは NAMELIST#gtool_historyauto_nml ,
    ! HistoryAutoAddVariable の引数,
    ! HistoryAutoCreate の引数 の順です).
    !
    !
    ! Initialization for multiple history data output
    !
    ! Set all axes and their weights depended by variables
    ! output from numerical models to this "HistoryAutoCreate".
    !
    ! All variables registered by "HistoryAutoAddVariable"
    ! are output if .true. is given to "all_output" or
    ! "namelist_filename" is not given (or blanks are given)
    ! On the other hand, if a filename of NAMELIST file is
    ! given to "namelist_filename", on/off of output,
    ! output filename and output interval, etc. can be changed
    ! from the NAMELIST file.
    ! For available items, see "NAMELIST#gtool_historyauto_nml".
    !
    ! Settings about
    ! "interval", "origin", "terminus", "slice_start", "slice_end", "slice_stride",
    ! "space_average", "time_average", "newfile_interval"
    ! etc. are used as default values.
    ! Their set values are overwritten by
    ! "HistoryAutoAddVariable" or
    ! "NAMELIST#gtool_historyauto_nml".
    ! ("NAMELIST#gtool_historyauto_nml" is high priority,
    ! arguments of "HistoryAutoAddVariable" are medium,
    ! arguments of "HistoryAutoCreate" are low).
    !
    use gtool_historyauto_internal, only: initialized, version, sub_sname, &
      & zero_time, numdims, &
      & title_save, source_save, institution_save, conventions_save, &
      & gt_version_save, rank_save, save_mpi_split, save_mpi_gather, &
      & time_unit_bycreate, time_unit_suffix, gthst_axes, data_axes, &
      & all_output_save, gthstnml, cal_save
    use gtool_history, only: HistoryAxisCreate, HistoryAxisAddAttr
    use gtool_history_nmlinfo_generic, only: HstNmlInfoCreate, HstNmlInfoAdd, &
      & HstNmlInfoEndDefine, HstNmlInfoPutLine, HstNmlInfoAllNameValid, &
      & HstNmlInfoInquire
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, &
      & GT_EARGSIZEMISMATCH, HST_ENOTIMEDIM, DC_ENEGATIVE
    use netcdf, only: NF90_EMAXDIMS, NF90_MAX_DIMS
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_present, only: present_and_not_empty, present_and_true, &
      & present_select
    use dc_calendar, only: DC_CAL, DC_CAL_DATE, &
      & DCCalDateInquire, DCCalInquire, DCCalDefault
    use dc_date, only: DCDiffTimeCreate, EvalbyUnit, toChar, toCharCal, Eval
    use dc_date_types, only: DC_DIFFTIME, DC_DATETIME
    use dc_message, only: MessageNotify
    use dc_iounit, only: FileOpen
    use dc_types, only: DP, STRING, TOKEN
    implicit none
    character(*), intent(in):: title
                              ! データ全体の表題.
                              ! Title of entire data
    character(*), intent(in):: source
                              ! データを作成する際の手段.
                              ! Source of data file
    character(*), intent(in):: institution
                              ! ファイルを最終的に変更した組織/個人.
                              ! Institution or person that changes files for the last time
    character(*), intent(in):: dims(:)
                              ! 次元の名前.
                              !
                              ! 配列の大きさに制限はありません.
                              ! 個々の次元の文字数は dc_types#TOKEN まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で
                              ! 補ってください.
                              !
                              ! Names of dimensions.
                              !
                              ! Length of array is unlimited.
                              ! Limits of numbers of characters of each
                              ! dimensions are "dc_types#TOKEN".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    integer, intent(in):: dimsizes (:)
                              ! dims で指定したそれぞれの次元大きさ.
                              !
                              ! 配列の大きさは dims の大きさと等しい
                              ! 必要があります.  '0' (数字のゼロ) を指定
                              ! するとその次元は 無制限次元 (unlimited
                              ! dimension) となります. (gtool_history
                              ! では時間の次元に対して無制限次元を
                              ! 用いることを想定しています). ただし,
                              ! 1 つの NetCDF ファイル (バージョン 3)
                              ! は最大で 1 つの無制限次元しか持てないので,
                              ! 2 ヶ所以上に '0' を指定しないでください.
                              ! その場合, 正しく gtool4 データが出力されません.
                              !
                              ! Lengths of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".  If '0' (zero) is
                              ! specified, the dimension is treated as
                              ! unlimited dimension.
                              ! (In "gtool_history", unlimited dimension is
                              ! expected to be used as time).
                              ! Note that one NetCDF file (version 3)
                              ! can not have two or more unlimited
                              ! dimensions, so that do not specify '0'
                              ! to two or more places. In that case,
                              ! gtoo4 data is not output currently
                              !
    character(*), intent(in):: longnames (:)
                              ! dims で指定したそれぞれの次元の名前.
                              !
                              ! 配列の大きさは dims の大きさ
                              ! と等しい必要があります. 文字数
                              ! は dc_types#STRING まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Names of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Limits of numbers of characters are
                              ! "dc_types#STRING".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    character(*), intent(in):: units(:)
                              ! dims で指定したそれぞれの次元の単位.
                              !
                              ! 配列の大きさは dims の大きさ
                              ! と等しい必要があります. 文字数
                              ! は dc_types#STRING まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Units of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Limits of numbers of characters are
                              ! "dc_types#STRING".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    type(DC_DIFFTIME), intent(in):: origin
                              ! 出力開始時刻.
                              !
                              ! Start time of output.
                              !
    type(DC_DIFFTIME), intent(in):: terminus
                              ! 出力終了時刻.
                              !
                              ! End time of output.
                              !
    character(*), intent(in),  optional:: xtypes(:)
                              ! dims で指定したそれぞれの
                              ! 次元のデータ型.
                              !
                              ! デフォルトは float (単精度実数型)
                              ! です. 有効なのは,
                              ! double (倍精度実数型),
                              ! int (整数型) です. 指定しない
                              ! 場合や, 無効な型を指定した場合には,
                              ! float となります. なお, 配列の大きさ
                              ! は *dims* の大きさと等しい必要が
                              ! あります. 配列内の文字数は全て
                              ! 同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Data types of dimensions specified
                              ! with "dims".
                              !
                              ! Default value is "float" (single precision).
                              ! Other valid values are
                              ! "double" (double precision),
                              ! "int" (integer).
                              ! If no value or invalid value is specified,
                              ! "float" is applied.
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    character(*), intent(in), optional:: conventions
                              ! 出力するファイルの netCDF
                              ! 規約
                              !
                              ! 省略した場合,
                              ! もしくは空文字を与えた場合,
                              ! 出力する netCDF 規約の
                              ! Conventions 属性に値
                              ! gtool4_netCDF_Conventions
                              ! が自動的に与えられます.
                              !
                              ! NetCDF conventions of output file.
                              !
                              ! If this argument is omitted or,
                              ! blanks are given,
                              ! gtool4_netCDF_Conventions is given to
                              ! attribute "Conventions" of an output file
                              ! automatically.
                              !
    character(*), intent(in), optional:: gt_version
                              ! gtool4 netCDF 規約のバージョン
                              !
                              ! 省略した場合, gt_version 属性に
                              ! 規約の最新版のバージョンナンバー
                              ! gtool4_netCDF_version
                              ! が与えられます.
                              ! (ただし, 引数 conventions に
                              ! gtool4_netCDF_Conventions
                              ! 以外が与えられる場合は
                              ! gt_version 属性を作成しません).
                              !
                              ! Version of gtool4 netCDF Conventions.
                              !
                              ! If this argument is omitted,
                              ! latest version number of gtool4 netCDF
                              ! Conventions is given to attribute
                              ! "gt_version" of an output file
                              ! (However, gtool4_netCDF_Conventions is
                              ! not given to an argument "conventions",
                              ! attribute "gt_version" is not created).
                              !
    logical, intent(in), optional:: all_output
                              ! 登録変数を全て出力するためのフラグ.
                              !
                              ! .true. を指定すると,
                              ! HistoryAutoAddVariable で登録された
                              ! 変数が全て出力されるようになります.
                              !
                              ! *namelist_filename* が指定される場合
                              ! には, デフォルトは .false. となります.
                              ! この場合には,
                              ! *namelist_filename* に指定された
                              ! NAMELIST ファイルから読み込まれる
                              ! NAMELIST#gtool_historyauto_nml
                              ! で指定された変数のみ出力されます.
                              !
                              ! *namelist_filename* が指定されない場合
                              ! には, .true. となります.
                              !
                              !
                              ! Flag for output all registered variables.
                              !
                              ! When .true. is specified,
                              ! all variables registered by
                              ! "HistoryAutoAddVariable" are output.
                              !
                              ! If *namelist_filename* is specified,
                              ! default value becomes .false. .
                              ! In this case,
                              ! only variables specified in
                              ! "NAMELIST#gtool_historyauto_nml"
                              ! loaded from a NAMELIST file
                              ! *namelist_filename*.
                              !
                              ! If *namelist_filename* is not specified,
                              ! this value becomes .true. .
                              !
    character(*), intent(in), optional:: file_prefix
                              ! データのファイル名の接頭詞.
                              ! Prefixes of history data filenames
    character(*), intent(in), optional:: namelist_filename
                              ! NAMELIST ファイルの名称.
                              !
                              ! 省略した場合, もしくは空白文字を与えた場合,
                              ! NAMELIST ファイルは読み込みません.
                              !
                              ! Name of NAMELIST file.
                              !
                              ! If this argument is omitted,
                              ! or blanks are specified,
                              ! no NAMELIST file is loaded.
                              !
    type(DC_DIFFTIME), intent(in), optional:: interval
                              ! 出力時間間隔.
                              !
                              ! 省略した場合,
                              ! 自動的に 1.0 [sec] が設定されます.
                              !
                              ! Interval of output time.
                              !
                              ! If this argument is omitted,
                              ! 1.0 [sec] is specified
                              ! automatically.
                              !
    integer, intent(in), optional:: slice_start(:)
                              ! 空間方向の開始点.
                              !
                              ! 省略した場合, 座標データの開始点が設定されます.
                              !
                              ! Start points of spaces.
                              !
                              ! If this argument is omitted,
                              ! start points of dimensions are set.
                              !
    integer, intent(in), optional:: slice_end(:)
                              ! 空間方向の終了点.
                              !
                              ! 省略した場合, もしくは負の値が与えら得た場合,
                              ! 座標データの終了点が設定されます.
                              !
                              ! End points of spaces.
                              !
                              ! If this argument is omitted or
                              ! negative value is specified,
                              ! end points of dimensions are set.
                              !
    integer, intent(in), optional:: slice_stride(:)
                              ! 空間方向の刻み幅.
                              !
                              ! 省略した場合, 1 が設定されます.
                              !
                              ! Strides of spaces
                              !
                              ! If this argument is omitted,
                              ! 1 is set.
                              !
    logical, intent(in), optional:: space_average(:)
                              ! 平均化のフラグ.
                              !
                              ! .true. が指定される座標に対して平均化を
                              ! 行います.
                              ! 省略した場合, .false. が設定されます.
                              !
                              ! Flag of average.
                              !
                              ! Axes specified .true. are averaged.
                              ! If this argument is omitted,
                              ! .false. is set.
                              !
    logical, intent(in), optional:: time_average
                              ! 出力データの時間平均フラグ.
                              ! デフォルトは .false.
                              ! Flag for time average of output data
                              ! Default value is .false.
    integer, intent(in), optional:: newfile_interval
                              ! ファイル分割時間間隔.
                              !
                              ! 省略した場合,
                              ! 時間方向へのファイル分割を行いません.
                              !
                              ! Interval of time of separation of a file.
                              !
                              ! If this argument is omitted,
                              ! a files is not separated in time direction.
                              !
    character(*), intent(in), optional:: rank
                              ! ランクの名称.
                              !
                              ! Name of a rank.
                              !
    type(DC_DATETIME), intent(in), optional:: origin_date
                              ! 出力開始日時.
                              ! この引数は廃止予定のため, start_date を使用して下さい.
                              !
                              ! Start date of output.
                              ! Use "start_date" because this argument is obsolete.
                              !
    logical, intent(in), optional:: origin_date_invalid
                              ! .true. を与えると, origin_date を無効にします.
                              !
                              ! If ".true." is given, "origin_date" is ignored.
    type(DC_CAL_DATE), intent(in), optional:: start_date
                              ! 出力開始日時.
                              !
                              ! Start date of output.
                              !
    type(DC_CAL), intent(in), optional:: cal
                              ! 暦情報.
                              ! これを指定しない場合, dc_calendar モジュールの
                              ! デフォルトの暦が使用されます.
                              !
                              ! Calendar.
                              ! If this argument is specified, default calendar in
                              ! "dc_calendar" module is used.
                              !
    logical, intent(in), optional:: flag_mpi_gather
                              ! MPI 使用時に, 各ノードで HistoryPut
                              ! に与えたデータを一つのファイルに統合して出力
                              ! する場合には .true. を与えてください.
                              ! デフォルトは .false. です.
                              !
                              ! .true. を与えた場合, HistoryPutAxisMPI
                              ! に全体の軸データを与えてください.
                              !
                              ! When MPI is used, if ".true." is given,
                              ! data given to "HistoryPut" on each node
                              ! is integrated and output to one file.
                              ! Default value is ".false.".
                              !
                              ! If .true. is given, give data of axes in
                              ! whole area to "HistoryPutAxisMPI"
                              !
    logical, intent(in), optional:: flag_mpi_split
                              ! MPI 使用時に, 各ノードで HistoryPut
                              ! に与えたデータをそれぞれ別名のファイルに
                              ! 出力する場合には .true. を与えてください.
                              ! デフォルトは .false. です.
                              !
                              ! When MPI is used, if ".true." is given,
                              ! data given to "HistoryPut" on each node
                              ! is split into discrete files.
                              ! Default value is ".false.".
                              !


    ! NAMELIST 変数群 ; NAMELIST group of variables
    character(STRING):: Name
                              ! 変数名.
                              ! 空白の場合には, この他の設定値は
                              ! gtool_historyauto モジュールにおいて
                              ! 出力されるデータ全ての
                              ! デフォルト値となります.
                              !
                              ! "Data1,Data2" のようにカンマで区切って複数
                              ! の変数を指定することも可能です.
                              !
                              ! Variable identifier.
                              ! If blank is given, other values are
                              ! used as default values of output data
                              ! in "gtool_historyauto".
                              !
                              ! Multiple variables can be specified
                              ! as "Data1,Data2" too. Delimiter is comma.
    character(STRING):: File
                              ! 出力ファイル名.
                              ! これはデフォルト値としては使用されません.
                              ! *Name* に値が設定されている時のみ有効です.
                              !
                              ! Output file name.
                              ! This is not used as default value.
                              ! This value is valid only when *Name* is
                              ! specified.

    real(DP):: IntValue
                              ! データの出力間隔の数値.
                              ! 負の値を与えると, 出力を抑止します.
                              ! Numerical value for interval of history data output
                              ! Negative values suppresses output.
    character(TOKEN):: IntUnit
                              ! データの出力間隔の単位.
                              ! Unit for interval of history data output
    character(TOKEN):: Precision
                              ! データの精度.
                              ! デフォルトは float (単精度実数型)
                              ! です. 有効なのは,
                              ! double (倍精度実数型),
                              ! int (整数型) です. 指定しない
                              ! 場合や, 無効な型を指定した場合には,
                              ! float となります.
                              !
                              ! Precision of history data
                              ! Default value is "float" (single precision).
                              ! Other valid values are
                              ! "double" (double precision),
                              ! "int" (integer).
                              ! If no value or invalid value is specified,
                              ! "float" is applied.
    character(STRING):: FilePrefix
                              ! データのファイル名の接頭詞.
                              ! Prefixes of history data filenames
    logical:: TimeAverage
                              ! 出力データの時間平均フラグ.
                              !
                              ! ".true." を与えると, 時間平均値が出力されます.
                              !
                              ! Flag for time average of output data
                              !
                              ! If ".ture." is specified,
                              ! time average values are output.
                              !
    logical:: AllOutput
                              ! 登録変数を全て出力するためのフラグ.
                              ! Flag for output all registered variables.
    real(DP):: OriginValue
                              ! 出力開始時刻.
                              ! Start time of output.
    character(TOKEN):: OriginUnit
                              ! 出力開始時刻の単位.
                              ! Unit of start time of output.
    real(DP):: TerminusValue
                              ! 出力終了時刻.
                              ! End time of output.
    character(TOKEN):: TerminusUnit
                              ! 出力終了時刻の単位.
                              ! Unit of end time of output.
    integer:: SliceStart(1:NF90_MAX_DIMS)
                              ! 空間方向の開始点.
                              ! Start points of spaces.
    integer:: SliceEnd(1:NF90_MAX_DIMS)
                              ! 空間方向の終了点.
                              !
                              ! 省略した場合, もしくは負の値が与えら得た場合,
                              ! 座標データの終了点が設定されます.
                              !
                              ! End points of spaces.
                              !
                              ! If this argument is omitted or
                              ! negative value is specified,
                              ! end points of dimensions are set.
                              !
    integer:: SliceStride(1:NF90_MAX_DIMS)
                              ! 空間方向の刻み幅.
                              ! Strides of spaces.
    logical:: SpaceAverage(1:NF90_MAX_DIMS)
                              ! 空間平均のフラグ.
                              ! Flag of spatial average.
    integer:: NewFileIntValue
                              ! ファイル分割時間間隔の数値.
                              ! Numerical value for interval of time of separation of a file.
    character(TOKEN):: NewFileIntUnit
                              ! ファイル分割時間間隔の単位.
                              ! Unit of interval of time of separation of a file.

    namelist /gtool_historyauto_nml/ &
      & Name, File, &
      & IntValue, IntUnit, &
      & Precision, &
      & FilePrefix, &
      & TimeAverage, AllOutput, &
      & OriginValue, OriginUnit, &
      & TerminusValue, TerminusUnit, &
      & SliceStart, SliceEnd, SliceStride, SpaceAverage, &
      & NewFileIntValue, NewFileIntUnit
                              ! gtool_historyauto モジュールのデータ用
                              ! NAMELIST 変数群名.
                              !
                              ! gtool_historyauto_generic#HistoryAutoCreate
                              ! を使用する際に, オプショナル引数 *namelist_filename*
                              ! へ NAMELIST ファイル名を指定することで,
                              ! そのファイルからこの NAMELIST 変数群を
                              ! 読み込みます.
                              !
                              ! NAMELIST group name for
                              ! history data of "gtool_historyauto" module.
                              !
                              ! If a NAMELIST filename is specified to
                              ! an optional argument *namelist_filename* when
                              ! "gtool_historyauto_generic#HistoryAutoCreate"
                              ! is used, this NAMELIST group is
                              ! loaded from the file.


    ! 作業変数 ; Work variables
    integer:: blank_index
    integer:: stat
    character(STRING):: cause_c
    integer:: unit_nml        ! NAMELIST ファイルオープン用装置番号.
                              ! Unit number for NAMELIST file open
    integer:: iostat_nml      ! NAMELIST 読み込み時の IOSTAT.
                              ! IOSTAT of NAMELIST read
    character(TOKEN):: pos_nml
                              ! NAMELIST 読み込み時のファイル位置.
                              ! File position of NAMELIST read
    integer:: i, j
    character(TOKEN):: my_xtype

    real(DP):: interval_work, origin_work, terminus_work
    integer:: date_day
    real(DP):: date_sec
    integer:: msnot_rank
    character(STRING):: date_str
    character(TOKEN):: cal_str, cal_type
    integer:: origin_year, origin_month, origin_day, origin_hour, origin_min
    real(DP):: origin_sec
    integer:: month_in_year, hour_in_day, min_in_hour
    integer, pointer:: day_in_month(:) =>null()
    real(DP):: sec_in_min
    character(*), parameter:: subname = "HistoryAutoCreate3"
  continue
    call BeginSub(subname, version = version)
    stat = DC_NOERR
    cause_c = ""

    ! このサブルーチンが 2 度呼ばれたらエラー
    ! Error is occurred when this subroutine is called twice
    !
    if ( initialized ) then
      stat = DC_EALREADYINIT
      cause_c = 'gtool_historyauto'
      goto 999
    end if

    ! ゼロ秒の作成.
    ! Create zero seconds
    !
    zero_time = 0.0_DP
!!$    call DCDiffTimeCreate( &
!!$      & zero_time, &        ! (out)
!!$      & sec = 0.0_DP  )     ! (in)

    ! 次元の数に関するエラー処理
    ! Error handling for number of dimensions
    !
    numdims = size(dims)

    if ( size(dimsizes) /= numdims ) then
      cause_c = 'dimsizes, dims'
    elseif ( size(longnames) /= numdims ) then
      cause_c = 'longnames, dims'
    elseif ( size(units) /= numdims ) then
      cause_c = 'units, dims'
    endif
    if ( trim(cause_c) /= "" ) then
      stat = GT_EARGSIZEMISMATCH
      goto 999
    end if

    if ( numdims > NF90_MAX_DIMS ) then
      stat = NF90_EMAXDIMS
      goto 999
    end if

    ! 時刻次元に関するエラー処理
    ! Error handling for time dimension
    !
    if ( dimsizes(numdims) /= 0 ) then
      call MessageNotify( 'W', subname, &
        & 'time dimension must be specified to the last of "dims"' )
      stat = HST_ENOTIMEDIM
      goto 999
    end if

    ! 出力ファイルの基本メタデータの保管
    ! Save basic meta data for output file
    !
    title_save       = title
    source_save      = source
    institution_save = institution

    conventions_save = ''
    if ( present(conventions) ) conventions_save = conventions

    gt_version_save = ''
    if ( present(gt_version) ) gt_version_save = gt_version

    rank_save = ''
    if ( present(rank) ) rank_save = rank

    ! MPI に関する情報の保管
    ! Save information about MPI
    !
    save_mpi_split  = present_and_true( flag_mpi_split )
    save_mpi_gather = present_and_true( flag_mpi_gather )

    msnot_rank = -1
    if ( save_mpi_gather ) msnot_rank = 0

    ! 時刻の単位のチェック
    ! Check units of time
    !
    time_unit_bycreate = units(numdims)
    time_unit_suffix = ''
    blank_index = index( trim( adjustl(time_unit_bycreate) ), ' ' )
    if ( blank_index > 1  ) then
      time_unit_suffix = time_unit_bycreate(blank_index+1:)
      time_unit_bycreate = time_unit_bycreate(1:blank_index-1)
    end if

    ! 座標軸データの保管
    ! Save axes data
    !
    do i = 1, numdims
      my_xtype = ''
      if ( present(xtypes) ) then
        if ( size(xtypes) >= i ) then
          my_xtype = xtypes(i)
        end if
      end if

      call HistoryAxisCreate( &
        &     axis = gthst_axes(i), &                     ! (out)
        &     name = dims(i),       size = dimsizes(i), & ! (in)
        & longname = longnames(i), units = units(i), &    ! (in)
        &    xtype = my_xtype )                           ! (in)

      allocate( data_axes(i) % a_axis( dimsizes(i) ) )
      data_axes(i) % a_axis = (/ ( real( j, DP ), j = 1, dimsizes(i) ) /)

    end do

    ! 暦の登録
    ! Register calendar
    !
    if ( present(cal) ) then
      cal_save = cal
    else
      call DCCalDefault( cal_save )
    end if

    ! 日時の指定
    ! Specify date
    !
    if ( present(start_date) ) then

      call DCCalDateInquire( &
        & date_str = date_str, &   ! (out)
        & date     = start_date, & ! (in) optional
        & cal      = cal &         ! (in) optional
        !    & , zone = "+09:00" &
        & )        ! (in) optional

      call DCCalDateInquire( &
        & origin_year, origin_month, origin_day, & ! (out) optional
        & origin_hour, origin_min,   origin_sec, & ! (out) optional
        & date     = start_date, & ! (in) optional
        & cal      = cal &         ! (in) optional
        & )

      call DCCalInquire( &
        & cal_str, &                          ! (out) optional
        & month_in_year    = month_in_year, & ! (out) optional
        & day_in_month_ptr = day_in_month , & ! (out) optional
        & hour_in_day      = hour_in_day  , & ! (out) optional
        & min_in_hour      = min_in_hour  , & ! (out) optional
        & sec_in_min       = sec_in_min   , & ! (out) optional
        & cal = cal_save )                    ! (in) optional

      ! 地球暦の場合のみ units 属性に "since ..." を付加
      !
      select case ( trim(cal_str) )
      case ( 'gregorian' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( 'julian' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( 'noleap' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( '360day' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( 'cyclic' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      end select

      ! 開始日時情報の付与
      !
      call HistoryAxisAddAttr( &
        & axis = gthst_axes(numdims), &    ! (inout)
        & attrname = 'origin', &           ! (in)
        & value = 'origin_year origin_month origin_day ' // &
        &         'origin_hour origin_min origin_sec' )    ! (in)

      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_year',  origin_year )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_month', origin_month )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_day',   origin_day )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_hour',  origin_hour )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_min',   origin_min )

      ! 暦情報の付与
      !
      call HistoryAxisAddAttr( &
        & axis = gthst_axes(numdims), &    ! (inout)
        & attrname = 'calendar', &         ! (in)
        & value = cal_str )                ! (in)

      if ( trim(cal_str) == 'user_defined' ) then
        call HistoryAxisAddAttr( gthst_axes(numdims), 'month_in_year', month_in_year )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'day_in_month',  day_in_month )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'hour_in_day',   hour_in_day )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'min_in_hour',   min_in_hour )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'sec_in_min',    sec_in_min )
      end if

      deallocate( day_in_month )

    elseif ( present(origin_date) &
      &  .and. .not. present_and_true(origin_date_invalid) ) then
      call Eval( origin_date, &            ! (in)
        & day = date_day, sec = date_sec ) ! (out)
      if ( date_day /= 0 .or. date_sec /= 0.0 ) then
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // toChar(origin_date)

        call HistoryAxisAddAttr( &
          & axis = gthst_axes(numdims), &    ! (inout)
          & attrname = 'calendar', &         ! (in)
          & value = toCharCal(origin_date) ) ! (in)

      end if
    end if

    ! 登録変数を全て出力するためのフラグの保管
    ! Save flag for output all registered variables
    !
    if ( present(all_output) ) all_output_save = all_output
    if ( .not. present_and_not_empty(namelist_filename) ) all_output_save = .true.
    AllOutput = all_output_save

    ! 出力時間間隔のデフォルト値設定
    ! Configure default interval of output time
    !
    if ( all_output_save ) then
      if ( present(interval) ) then
        interval_work = EvalbyUnit( interval, time_unit_bycreate )
      else
        interval_work = 1.0
      end if
    else
      interval_work = - 1.0
    end if

    ! 出力開始・終了時刻のデフォルト値設定
    ! Configure default origin/terminus time of output
    !
    origin_work = EvalbyUnit( origin, 'sec' )
    terminus_work = EvalbyUnit( terminus, time_unit_bycreate )

    ! gtool_historyauto_nml へデフォルト値の設定
    ! Configure default values for "gtool_historyauto_nml"
    !
    call HstNmlInfoCreate( gthstnml ) ! (out)

    call HstNmlInfoAdd( &
      & gthstnml = gthstnml, &                  ! (inout)
      & name = '', &                            ! (in) optional
      & precision = 'float', &                  ! (in) optional
      & fileprefix = file_prefix, &             ! (in) optional
      & interval_value = interval_work, &       ! (in) optional
      & interval_unit  = time_unit_bycreate, &  ! (in) optional
      & origin_value   = origin_work, &         ! (in) optional
      & origin_unit    = 'sec', &               ! (in) optional
!!$      & origin_unit    = time_unit_bycreate, &  ! (in) optional
      & terminus_value = terminus_work, &       ! (in) optional
      & terminus_unit  = time_unit_bycreate, &  ! (in) optional
      & time_average = time_average, &          ! (in) optional
      & slice_start  = slice_start, &           ! (in) optional
      & slice_end    = slice_end, &             ! (in) optional
      & slice_stride = slice_stride, &          ! (in) optional
      & space_average = space_average, &        ! (in) optional
      & newfile_intvalue = newfile_interval, &  ! (in) optional
      & newfile_intunit = time_unit_bycreate )  ! (in) optional

    ! NAMELIST ファイルの読み込み
    ! Load NAMELIST file
    !
    if ( present_and_not_empty(namelist_filename) ) then
      call FileOpen( unit_nml, &          ! (out)
        & namelist_filename, mode = 'r' ) ! (in)

      iostat_nml = 0
      pos_nml = ''

      call MessageNotify( 'M', sub_sname, '----- "gtool_historyauto_nml" is loaded from "%c" -----', &
        & c1 = trim(namelist_filename), rank_mpi = msnot_rank )

      do while ( trim(pos_nml) /= 'APPEND' .and. iostat_nml == 0 )

        Name = ''
        File = ''
        call HstNmlInfoInquire( &
          & gthstnml = gthstnml, &             ! (in)
          & interval_value = IntValue, &       ! (out) optional
          & interval_unit = IntUnit, &         ! (out) optional
          & precision = Precision, &           ! (out) optional
          & time_average = TimeAverage, &      ! (out) optional
          & origin_value   = OriginValue, &    ! (out) optional
          & origin_unit    = OriginUnit, &     ! (out) optional
          & terminus_value = TerminusValue, &  ! (out) optional
          & terminus_unit  = TerminusUnit, &   ! (out) optional
          & slice_start  = SliceStart, &       ! (out) optional
          & slice_end    = SliceEnd, &         ! (out) optional
          & slice_stride = SliceStride, &      ! (out) optional
          & space_average = SpaceAverage, &    ! (out) optional
          & newfile_intvalue = NewFileIntValue, & ! (out) optional
          & newfile_intunit  = NewFileIntUnit, &  ! (out) optional
          & fileprefix = FilePrefix )          ! (out) optional

        read( unit = unit_nml, &            ! (in)
          &    nml = gtool_historyauto_nml, & ! (out)
          & iostat = iostat_nml )           ! (out)
        inquire( unit = unit_nml, & ! (in)
          &  position = pos_nml )   ! (out)

        if ( iostat_nml == 0 ) then

          ! NAMELIST から与えられた値が無効な場合, デフォルト値を使用
          ! Default values are used when values from NAMELIST are invalid
          !
          if ( .not. IntValue > 0.0 ) then
            IntValue = interval_work
            IntUnit  = time_unit_bycreate
          end if
          if ( .not. OriginValue > 0.0 ) then
            OriginValue = origin_work
            OriginUnit  = 'sec'
          end if
          if ( .not. TerminusValue > 0.0 ) then
            TerminusValue = terminus_work
            TerminusUnit  = time_unit_bycreate
          end if

          ! 情報の登録
          ! Register information
          !
          call HstNmlInfoAdd( &
            & gthstnml = gthstnml, &             ! (inout)
            & name = Name, &                     ! (in) optional
            & file = File, &                     ! (in) optional
            & interval_value = IntValue, &       ! (in) optional
            & interval_unit = IntUnit, &         ! (in) optional
            & precision = Precision, &           ! (in) optional
            & time_average = TimeAverage, &      ! (in) optional
            & origin_value   = OriginValue, &    ! (in) optional
            & origin_unit    = OriginUnit, &     ! (in) optional
            & terminus_value = TerminusValue, &  ! (in) optional
            & terminus_unit  = TerminusUnit, &   ! (in) optional
            & slice_start  = SliceStart, &       ! (in) optional
            & slice_end    = SliceEnd, &         ! (in) optional
            & slice_stride = SliceStride, &      ! (in) optional
            & space_average = SpaceAverage, &    ! (in) optional
            & newfile_intvalue = NewFileIntValue, & ! (in) optional
            & newfile_intunit  = NewFileIntUnit, &  ! (in) optional
            & fileprefix = FilePrefix )          ! (in) optional

          ! 登録変数を全て出力するためのフラグの保管
          ! Save flag for output all registered variables
          !
          if ( trim(Name) == '' ) then
            all_output_save = AllOutput
          end if

          ! 印字 ; Print
          !
          if ( trim(File) == '' ) File = trim(FilePrefix) // '<Name>.nc'

          if ( trim(Name) == '' ) then
            call MessageNotify( 'M', sub_sname, 'Global Settings:', rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  AllOutput       = %b', l  = (/ AllOutput   /), rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  FilePrefix      = %c', c1 = trim(FilePrefix   ), rank_mpi = msnot_rank )
          else
            call MessageNotify( 'M', sub_sname, 'Individual Settings:', rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  Name            = %c', c1 = trim(Name           ), rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  File            = %c', c1 = trim(File           ), rank_mpi = msnot_rank )
          end if
          call MessageNotify( 'M', sub_sname, '  Interval        = %f [%c]', &
            & d = (/ IntValue /), c1 = trim( IntUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  Precision       = %c', c1 = trim(Precision    ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  TimeAverage     = %b', l  = (/ TimeAverage   /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  Origin          = %f [%c]', &
            & d = (/ OriginValue /), c1 = trim( OriginUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  Terminus        = %f [%c]', &
            & d = (/ TerminusValue /), c1 = trim( TerminusUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SliceStart      = (/ %*d /)', &
            &                                i = SliceStart(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SliceEnd        = (/ %*d /)', &
            &                                i = SliceEnd(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SliceStride     = (/ %*d /)', &
            &                                i = SliceStride(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SpaceAverage    = (/ %*b /)', &
            &                                l = SpaceAverage(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  NewFileInterval = %d [%c]', &
            & i = (/ NewFileIntValue /), c1 = trim( NewFileIntUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '', rank_mpi = msnot_rank )

        else
          call MessageNotify( 'M', sub_sname, '----- loading is finished (iostat=%d) -----', &
            & i = (/iostat_nml/), rank_mpi = msnot_rank )
        end if
      end do

      close( unit_nml )


    ! NAMELIST ファイルを読み込まない場合
    ! NAMELIST file is not loaded
    !
    else
      call MessageNotify( 'M', sub_sname, '----- "gtool_historyauto_nml" is not loaded" -----', rank_mpi = msnot_rank )
      Name = ''
      File = ''
      call HstNmlInfoInquire( &
        & gthstnml = gthstnml, &             ! (in)
        & interval_value = IntValue, &       ! (out) optional
        & interval_unit = IntUnit, &         ! (out) optional
        & precision = Precision, &           ! (out) optional
        & time_average = TimeAverage, &      ! (out) optional
        & origin_value   = OriginValue, &    ! (out) optional
        & origin_unit    = OriginUnit, &     ! (out) optional
        & terminus_value = TerminusValue, &  ! (out) optional
        & terminus_unit  = TerminusUnit, &   ! (out) optional
        & slice_start  = SliceStart, &       ! (out) optional
        & slice_end    = SliceEnd, &         ! (out) optional
        & slice_stride = SliceStride, &      ! (out) optional
        & space_average = SpaceAverage, &    ! (out) optional
        & newfile_intvalue = NewFileIntValue, & ! (out) optional
        & newfile_intunit  = NewFileIntUnit, &  ! (out) optional
        & fileprefix = FilePrefix )          ! (out) optional

      ! 印字 ; Print
      !
      call MessageNotify( 'M', sub_sname, 'Global Settings:', rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  AllOutput       = %b', l  = (/ AllOutput   /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  FilePrefix      = %c', c1 = trim(FilePrefix   ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Interval        = %f [%c]', &
        & d = (/ IntValue /), c1 = trim( IntUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Precision       = %c', c1 = trim(Precision    ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  TimeAverage     = %b', l  = (/ TimeAverage   /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Origin          = %f [%c]', &
        & d = (/ OriginValue /), c1 = trim( OriginUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Terminus        = %f [%c]', &
        & d = (/ TerminusValue /), c1 = trim( TerminusUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SliceStart      = (/ %*d /)', &
        &                                i = SliceStart(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SliceEnd        = (/ %*d /)', &
        &                                i = SliceEnd(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SliceStride     = (/ %*d /)', &
            &                                i = SliceStride(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SpaceAverage    = (/ %*b /)', &
        &                                l = SpaceAverage(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  NewFileInterval = %d [%c]', &
        & i = (/ NewFileIntValue /), c1 = trim( NewFileIntUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '' , rank_mpi = msnot_rank)

    end if

    ! 終了処理, 例外処理
    ! Termination and Exception handling
    !
    initialized = .true.

999 continue
    call StoreError(stat, subname, cause_c = cause_c)
    call EndSub(subname, 'stat=%d', i = (/stat/) )
  end subroutine HistoryAutoCreate3

  !-------------------------------------------------------------------

  subroutine HistoryAutoCreate2( &
    & title, source, institution, &           ! (in)
    & dims, dimsizes, longnames, units, &     ! (in)
    & xtypes, conventions, gt_version,&       ! (in) optional
    & all_output, &                           ! (in) optional
    & file_prefix, &                          ! (in) optional
    & namelist_filename, &                    ! (in) optional
    & interval, origin, terminus, &           ! (in) optional
    & slice_start, slice_end, slice_stride, & ! (in) optional
    & space_average, &                        ! (in) optional
    & time_average, &                         ! (in) optional
    & newfile_interval, &                     ! (in) optional
    & rank, &                                 ! (in) optional
    & origin_date, origin_date_invalid, &     ! (in) optional
    & start_date, cal, &                      ! (in) optional
    & flag_mpi_gather, flag_mpi_split &       ! (in) optional
    & )
    !
    ! 複数のヒストリデータ出力を行うための初期化を行います.
    !
    ! この HistoryAutoCreate には, モデル内で出力する
    ! 変数が依存する座標や座標重みなどを全てを設定してください.
    !
    ! all_output に .true. を与えた場合や,
    ! namelist_filename を与えない (空文字を与える) 場合には,
    ! HistoryAutoAddVariable で登録される全ての変数が出力されます.
    ! 一方で namelist_filename に NAMELIST ファイル名を与える場合には,
    ! その NAMELIST ファイルから出力のオンオフや,
    ! 出力ファイル名, 出力間隔などを変更可能です.
    ! 変更可能な項目に関しては NAMELIST#gtool_historyauto_nml
    ! を参照して下さい.
    !
    ! interval, origin, terminus, slice_start, slice_end, slice_stride,
    ! space_average, time_average, newfile_interval
    ! などの設定はデフォルト値として使用されます.
    ! これらの設定値は HistoryAutoAddVariable および
    ! NAMELIST#gtool_historyauto_nml で上書きされます.
    ! (優先度が高いのは NAMELIST#gtool_historyauto_nml ,
    ! HistoryAutoAddVariable の引数,
    ! HistoryAutoCreate の引数 の順です).
    !
    !
    ! Initialization for multiple history data output
    !
    ! Set all axes and their weights depended by variables
    ! output from numerical models to this "HistoryAutoCreate".
    !
    ! All variables registered by "HistoryAutoAddVariable"
    ! are output if .true. is given to "all_output" or
    ! "namelist_filename" is not given (or blanks are given)
    ! On the other hand, if a filename of NAMELIST file is
    ! given to "namelist_filename", on/off of output,
    ! output filename and output interval, etc. can be changed
    ! from the NAMELIST file.
    ! For available items, see "NAMELIST#gtool_historyauto_nml".
    !
    ! Settings about
    ! "interval", "origin", "terminus", "slice_start", "slice_end", "slice_stride",
    ! "space_average", "time_average", "newfile_interval"
    ! etc. are used as default values.
    ! Their set values are overwritten by
    ! "HistoryAutoAddVariable" or
    ! "NAMELIST#gtool_historyauto_nml".
    ! ("NAMELIST#gtool_historyauto_nml" is high priority,
    ! arguments of "HistoryAutoAddVariable" are medium,
    ! arguments of "HistoryAutoCreate" are low).
    !
    use gtool_historyauto_generic, only: HistoryAutoCreate
    use gtool_historyauto_internal, only: initialized, numdims, time_unit_bycreate, time_unit_suffix
    use gtool_history, only: HistoryAxisCreate
    use gtool_history_nmlinfo_generic, only: HstNmlInfoCreate, HstNmlInfoAdd, &
      & HstNmlInfoEndDefine, HstNmlInfoPutLine, HstNmlInfoAllNameValid, &
      & HstNmlInfoInquire
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, &
      & GT_EARGSIZEMISMATCH, HST_ENOTIMEDIM
    use netcdf, only: NF90_EMAXDIMS, NF90_MAX_DIMS
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_present, only: present_and_not_empty, present_and_true, &
      & present_select
    use dc_calendar, only: DC_CAL, DC_CAL_DATE
    use dc_date, only: DCDiffTimeCreate, EvalbyUnit
    use dc_date_types, only: DC_DIFFTIME, DC_DATETIME
    use dc_message, only: MessageNotify
    use dc_iounit, only: FileOpen
    use dc_types, only: DP, STRING, TOKEN
    implicit none
    character(*), intent(in):: title
                              ! データ全体の表題.
                              ! Title of entire data
    character(*), intent(in):: source
                              ! データを作成する際の手段.
                              ! Source of data file
    character(*), intent(in):: institution
                              ! ファイルを最終的に変更した組織/個人.
                              ! Institution or person that changes files for the last time
    character(*), intent(in):: dims(:)
                              ! 次元の名前.
                              !
                              ! 配列の大きさに制限はありません.
                              ! 個々の次元の文字数は dc_types#TOKEN まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で
                              ! 補ってください.
                              !
                              ! Names of dimensions.
                              !
                              ! Length of array is unlimited.
                              ! Limits of numbers of characters of each
                              ! dimensions are "dc_types#TOKEN".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    integer, intent(in):: dimsizes (:)
                              ! dims で指定したそれぞれの次元大きさ.
                              !
                              ! 配列の大きさは dims の大きさと等しい
                              ! 必要があります.  '0' (数字のゼロ) を指定
                              ! するとその次元は 無制限次元 (unlimited
                              ! dimension) となります. (gtool_history
                              ! では時間の次元に対して無制限次元を
                              ! 用いることを想定しています). ただし,
                              ! 1 つの NetCDF ファイル (バージョン 3)
                              ! は最大で 1 つの無制限次元しか持てないので,
                              ! 2 ヶ所以上に '0' を指定しないでください.
                              ! その場合, 正しく gtool4 データが出力されません.
                              !
                              ! Lengths of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".  If '0' (zero) is
                              ! specified, the dimension is treated as
                              ! unlimited dimension.
                              ! (In "gtool_history", unlimited dimension is
                              ! expected to be used as time).
                              ! Note that one NetCDF file (version 3)
                              ! can not have two or more unlimited
                              ! dimensions, so that do not specify '0'
                              ! to two or more places. In that case,
                              ! gtoo4 data is not output currently
                              !
    character(*), intent(in):: longnames (:)
                              ! dims で指定したそれぞれの次元の名前.
                              !
                              ! 配列の大きさは dims の大きさ
                              ! と等しい必要があります. 文字数
                              ! は dc_types#STRING まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Names of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Limits of numbers of characters are
                              ! "dc_types#STRING".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    character(*), intent(in):: units(:)
                              ! dims で指定したそれぞれの次元の単位.
                              !
                              ! 配列の大きさは dims の大きさ
                              ! と等しい必要があります. 文字数
                              ! は dc_types#STRING まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Units of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Limits of numbers of characters are
                              ! "dc_types#STRING".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    character(*), intent(in),  optional:: xtypes(:)
                              ! dims で指定したそれぞれの
                              ! 次元のデータ型.
                              !
                              ! デフォルトは float (単精度実数型)
                              ! です. 有効なのは,
                              ! double (倍精度実数型),
                              ! int (整数型) です. 指定しない
                              ! 場合や, 無効な型を指定した場合には,
                              ! float となります. なお, 配列の大きさ
                              ! は *dims* の大きさと等しい必要が
                              ! あります. 配列内の文字数は全て
                              ! 同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Data types of dimensions specified
                              ! with "dims".
                              !
                              ! Default value is "float" (single precision).
                              ! Other valid values are
                              ! "double" (double precision),
                              ! "int" (integer).
                              ! If no value or invalid value is specified,
                              ! "float" is applied.
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    character(*), intent(in), optional:: conventions
                              ! 出力するファイルの netCDF
                              ! 規約
                              !
                              ! 省略した場合,
                              ! もしくは空文字を与えた場合,
                              ! 出力する netCDF 規約の
                              ! Conventions 属性に値
                              ! gtool4_netCDF_Conventions
                              ! が自動的に与えられます.
                              !
                              ! NetCDF conventions of output file.
                              !
                              ! If this argument is omitted or,
                              ! blanks are given,
                              ! gtool4_netCDF_Conventions is given to
                              ! attribute "Conventions" of an output file
                              ! automatically.
                              !
    character(*), intent(in), optional:: gt_version
                              ! gtool4 netCDF 規約のバージョン
                              !
                              ! 省略した場合, gt_version 属性に
                              ! 規約の最新版のバージョンナンバー
                              ! gtool4_netCDF_version
                              ! が与えられます.
                              ! (ただし, 引数 conventions に
                              ! gtool4_netCDF_Conventions
                              ! 以外が与えられる場合は
                              ! gt_version 属性を作成しません).
                              !
                              ! Version of gtool4 netCDF Conventions.
                              !
                              ! If this argument is omitted,
                              ! latest version number of gtool4 netCDF
                              ! Conventions is given to attribute
                              ! "gt_version" of an output file
                              ! (However, gtool4_netCDF_Conventions is
                              ! not given to an argument "conventions",
                              ! attribute "gt_version" is not created).
                              !
    logical, intent(in), optional:: all_output
                              ! 登録変数を全て出力するためのフラグ.
                              !
                              ! .true. を指定すると,
                              ! HistoryAutoAddVariable で登録された
                              ! 変数が全て出力されるようになります.
                              !
                              ! *namelist_filename* が指定される場合
                              ! には, デフォルトは .false. となります.
                              ! この場合には,
                              ! *namelist_filename* に指定された
                              ! NAMELIST ファイルから読み込まれる
                              ! NAMELIST#gtool_historyauto_nml
                              ! で指定された変数のみ出力されます.
                              !
                              ! *namelist_filename* が指定されない場合
                              ! には, .true. となります.
                              !
                              !
                              ! Flag for output all registered variables.
                              !
                              ! When .true. is specified,
                              ! all variables registered by
                              ! "HistoryAutoAddVariable" are output.
                              !
                              ! If *namelist_filename* is specified,
                              ! default value becomes .false. .
                              ! In this case,
                              ! only variables specified in
                              ! "NAMELIST#gtool_historyauto_nml"
                              ! loaded from a NAMELIST file
                              ! *namelist_filename*.
                              !
                              ! If *namelist_filename* is not specified,
                              ! this value becomes .true. .
                              !
    character(*), intent(in), optional:: file_prefix
                              ! ヒストリデータのファイル名の接頭詞.
                              ! Prefixes of history data filenames
    character(*), intent(in), optional:: namelist_filename
                              ! NAMELIST ファイルの名称.
                              !
                              ! 省略した場合, もしくは空白文字を与えた場合,
                              ! NAMELIST ファイルは読み込みません.
                              !
                              ! Name of NAMELIST file.
                              !
                              ! If this argument is omitted,
                              ! or blanks are specified,
                              ! no NAMELIST file is loaded.
                              !
    real, intent(in), optional:: interval
                              ! 出力時間間隔.
                              !
                              ! 省略した場合,
                              ! 自動的に 1.0 [sec] が設定されます.
                              !
                              ! Interval of output time.
                              !
                              ! If this argument is omitted,
                              ! 1.0 [sec] is specified
                              ! automatically.
                              !
    real, intent(in), optional:: origin
                              ! 出力開始時刻.
                              !
                              ! 省略した場合, 自動的に 0.0 [sec] が
                              ! 設定されます.
                              !
                              ! Start time of output.
                              !
                              ! If this argument is omitted,
                              ! 0.0 [sec] is specified
                              ! automatically.
                              !
    real, intent(in), optional:: terminus
                              ! 出力終了時刻.
                              !
                              ! 省略した場合, 数値モデルの実行が終了するまで
                              ! 出力を行います.
                              !
                              ! End time of output.
                              !
                              ! If this argument is omitted,
                              ! output is continued until a numerical model
                              ! is finished.
                              !
    integer, intent(in), optional:: slice_start(:)
                              ! 空間方向の開始点.
                              !
                              ! 省略した場合, 座標データの開始点が設定されます.
                              !
                              ! Start points of spaces.
                              !
                              ! If this argument is omitted,
                              ! start points of dimensions are set.
                              !
    integer, intent(in), optional:: slice_end(:)
                              ! 空間方向の終了点.
                              !
                              ! 省略した場合, 座標データの終了点が設定されます.
                              !
                              ! End points of spaces.
                              !
                              ! If this argument is omitted,
                              ! End points of dimensions are set.
                              !
    integer, intent(in), optional:: slice_stride(:)
                              ! 空間方向の刻み幅.
                              !
                              ! 省略した場合, 1 が設定されます.
                              !
                              ! Strides of spaces
                              !
                              ! If this argument is omitted,
                              ! 1 is set.
                              !
    logical, intent(in), optional:: space_average(:)
                              ! 平均化のフラグ.
                              !
                              ! .true. が指定される座標に対して平均化を
                              ! 行います.
                              ! 省略した場合, .false. が設定されます.
                              !
                              ! Flag of average.
                              !
                              ! Axes specified .true. are averaged.
                              ! If this argument is omitted,
                              ! .false. is set.
                              !
    logical, intent(in), optional:: time_average
                              ! 出力データの時間平均フラグ.
                              ! デフォルトは .false.
                              ! Flag for time average of output data
                              ! Default value is .false.
    integer, intent(in), optional:: newfile_interval
                              ! ファイル分割時間間隔.
                              !
                              ! 省略した場合,
                              ! 時間方向へのファイル分割を行いません.
                              !
                              ! Interval of time of separation of a file.
                              !
                              ! If this argument is omitted,
                              ! a files is not separated in time direction.
                              !
    character(*), intent(in), optional:: rank
                              ! ランクの名称.
                              ! 空文字を与えた場合には無視されます.
                              !
                              ! Name of a rank.
                              ! If blank is given, this argument is ignored.
                              !
    type(DC_DATETIME), intent(in), optional:: origin_date
                              ! 出力開始日時.
                              ! この引数は廃止予定のため, start_date を使用して下さい.
                              !
                              ! Start date of output.
                              ! Use "start_date" because this argument is obsolete.
                              !
    logical, intent(in), optional:: origin_date_invalid
                              ! .true. を与えると, origin_date を無効にします.
                              !
                              ! If ".true." is given, "origin_date" is ignored.
                              !
    type(DC_CAL_DATE), intent(in), optional:: start_date
                              ! 出力開始日時.
                              !
                              ! Start date of output.
                              !
    type(DC_CAL), intent(in), optional:: cal
                              ! 暦情報.
                              ! これを指定しない場合, dc_calendar モジュールの
                              ! デフォルトの暦が使用されます.
                              !
                              ! Calendar.
                              ! If this argument is specified, default calendar in
                              ! "dc_calendar" module is used.
                              !
    logical, intent(in), optional:: flag_mpi_gather
                              ! MPI 使用時に, 各ノードで HistoryPut
                              ! に与えたデータを一つのファイルに統合して出力
                              ! する場合には .true. を与えてください.
                              ! デフォルトは .false. です.
                              !
                              ! .true. を与えた場合, HistoryPutAxisMPI
                              ! に全体の軸データを与えてください.
                              !
                              ! When MPI is used, if ".true." is given,
                              ! data given to "HistoryPut" on each node
                              ! is integrated and output to one file.
                              ! Default value is ".false.".
                              !
                              ! If .true. is given, give data of axes in
                              ! whole area to "HistoryPutAxisMPI"
                              !
    logical, intent(in), optional:: flag_mpi_split
                              ! MPI 使用時に, 各ノードで HistoryPut
                              ! に与えたデータをそれぞれ別名のファイルに
                              ! 出力する場合には .true. を与えてください.
                              ! デフォルトは .false. です.
                              !
                              ! When MPI is used, if ".true." is given,
                              ! data given to "HistoryPut" on each node
                              ! is split into discrete files.
                              ! Default value is ".false.".
                              !
    integer:: blank_index
    type(DC_DIFFTIME):: interval_difftime, origin_difftime, terminus_difftime
    integer:: stat
    character(STRING):: cause_c
    character(*), parameter:: subname = "HistoryAutoCreate2"
  continue
    call BeginSub(subname)
    stat = DC_NOERR
    cause_c = ""

    ! このサブルーチンが 2 度呼ばれたらエラー
    ! Error is occurred when this subroutine is called twice
    !
    if ( initialized ) then
      stat = DC_EALREADYINIT
      cause_c = 'gtool_historyauto'
      goto 999
    end if

    ! 次元の数に関するエラー処理
    ! Error handling for number of dimensions
    !
    numdims = size(dims)

    if ( size(dimsizes) /= numdims ) then
      cause_c = 'dimsizes, dims'
    elseif ( size(longnames) /= numdims ) then
      cause_c = 'longnames, dims'
    elseif ( size(units) /= numdims ) then
      cause_c = 'units, dims'
    endif
    if ( trim(cause_c) /= "" ) then
      stat = GT_EARGSIZEMISMATCH
      goto 999
    end if

    if ( numdims > NF90_MAX_DIMS ) then
      stat = NF90_EMAXDIMS
      goto 999
    end if

    ! 時刻次元に関するエラー処理
    ! Error handling for time dimension
    !
    if ( dimsizes(numdims) /= 0 ) then
      call MessageNotify( 'W', subname, &
        & 'time dimension must be specified to the last of "dims"' )
      stat = HST_ENOTIMEDIM
      goto 999
    end if

    ! 時刻の単位のチェック
    ! Check units of time
    !
    time_unit_bycreate = units(numdims)
    time_unit_suffix = ''
    blank_index = index( trim( adjustl(time_unit_bycreate) ), ' ' )
    if ( blank_index > 1  ) then
      time_unit_suffix = time_unit_bycreate(blank_index+1:)
      time_unit_bycreate = time_unit_bycreate(1:blank_index-1)
    end if

    ! 出力時間間隔のデフォルト値設定
    ! Configure default interval of output time
    !
    if ( present(interval) ) then
      call DCDiffTimeCreate( &
        & interval_difftime, &           ! (out)
        & interval, time_unit_bycreate ) ! (in)
    else
      call DCDiffTimeCreate( &
        & interval_difftime, &      ! (out)
        & 1.0, time_unit_bycreate ) ! (in)
    end if

    ! 出力開始・終了時刻のデフォルト値設定
    ! Configure default origin/terminus time of output
    !
    if ( present(origin) ) then
      call DCDiffTimeCreate( &
        & origin_difftime, &           ! (out)
        & origin, time_unit_bycreate ) ! (in)
    else
      call DCDiffTimeCreate( &
        & origin_difftime, &           ! (out)
        & 0.0, time_unit_bycreate )    ! (in)
    end if

    if ( present(terminus) ) then
      call DCDiffTimeCreate( &
        & terminus_difftime, &           ! (out)
        & terminus, time_unit_bycreate ) ! (in)
    else
      call DCDiffTimeCreate( &
        & terminus_difftime, &       ! (out)
        & -1.0, time_unit_bycreate )  ! (in)
    end if

    ! HistoryAutoCreate1 の呼び出し
    ! Call "HistoryAutoCreate1"
    !
    call HistoryAutoCreate( &
      & title = title, source = source, &        ! (in)
      & institution = institution, &             ! (in)
      & dims = dims, dimsizes = dimsizes, &      ! (in)
      & longnames = longnames, units = units, &  ! (in)
      &      origin = origin_difftime, &         ! (in)
      &    terminus = terminus_difftime, &       ! (in)
      &      xtypes = xtypes, &                  ! (in) optional
      & conventions = conventions, &             ! (in) optional
      &  gt_version = gt_version, &              ! (in) optional
      &  all_output = all_output, &              ! (in) optional
      & file_prefix = file_prefix, &             ! (in) optional
      & namelist_filename = namelist_filename, & ! (in) optional
      &      interval = interval_difftime, &     ! (in) optional
      &   slice_start = slice_start, &           ! (in) optional
      &     slice_end = slice_end, &             ! (in) optional
      &  slice_stride = slice_stride, &          ! (in) optional
      & space_average = space_average, &         ! (in) optional
      &  time_average = time_average, &          ! (in) optional
      & newfile_interval = newfile_interval, &   ! (in) optional
      & rank = rank, &                           ! (in) optional
      & origin_date = origin_date, &                 ! (in) optional
      & origin_date_invalid = origin_date_invalid, & ! (in) optional
      & start_date = start_date, &                   ! (in) optional
      & cal = cal, &                                 ! (in) optional
      & flag_mpi_gather = flag_mpi_gather, &         ! (in) optional
      & flag_mpi_split = flag_mpi_split )            ! (in) optional

999 continue
    call StoreError(stat, subname, cause_c = cause_c)
    call EndSub(subname)
  end subroutine HistoryAutoCreate2

  !-------------------------------------------------------------------

  subroutine HistoryAutoCreate1( &
    & title, source, institution, &           ! (in)
    & dims, dimsizes, longnames, units, &     ! (in)
    & origin, terminus, &                     ! (in)
    & xtypes, conventions, gt_version, &      ! (in) optional
    & all_output, &                           ! (in) optional
    & file_prefix, &                          ! (in) optional
    & namelist_filename, &                    ! (in) optional
    & interval, &                             ! (in) optional
    & slice_start, slice_end, slice_stride, & ! (in) optional
    & space_average, &                        ! (in) optional
    & time_average, &                         ! (in) optional
    & newfile_interval, &                     ! (in) optional
    & rank, &                                 ! (in) optional
    & origin_date, origin_date_invalid, &     ! (in) optional
    & start_date, cal, &                      ! (in) optional
    & flag_mpi_gather, flag_mpi_split &       ! (in) optional
    & )
    !
    ! 複数のデータ出力を行うための初期化を行います.
    !
    ! この HistoryAutoCreate には, モデル内で出力する
    ! 変数が依存する座標や座標重みなどを全てを設定してください.
    !
    ! all_output に .true. を与えた場合や,
    ! namelist_filename を与えない (空文字を与える) 場合には,
    ! HistoryAutoAddVariable で登録される全ての変数が出力されます.
    ! 一方で namelist_filename に NAMELIST ファイル名を与える場合には,
    ! その NAMELIST ファイルから出力のオンオフや,
    ! 出力ファイル名, 出力間隔などを変更可能です.
    ! 変更可能な項目に関しては NAMELIST#gtool_historyauto_nml
    ! を参照して下さい.
    !
    ! interval, origin, terminus, slice_start, slice_end, slice_stride,
    ! space_average, time_average, newfile_interval
    ! などの設定はデフォルト値として使用されます.
    ! これらの設定値は HistoryAutoAddVariable および
    ! NAMELIST#gtool_historyauto_nml で上書きされます.
    ! (優先度が高いのは NAMELIST#gtool_historyauto_nml ,
    ! HistoryAutoAddVariable の引数,
    ! HistoryAutoCreate の引数 の順です).
    !
    !
    ! Initialization for multiple history data output
    !
    ! Set all axes and their weights depended by variables
    ! output from numerical models to this "HistoryAutoCreate".
    !
    ! All variables registered by "HistoryAutoAddVariable"
    ! are output if .true. is given to "all_output" or
    ! "namelist_filename" is not given (or blanks are given)
    ! On the other hand, if a filename of NAMELIST file is
    ! given to "namelist_filename", on/off of output,
    ! output filename and output interval, etc. can be changed
    ! from the NAMELIST file.
    ! For available items, see "NAMELIST#gtool_historyauto_nml".
    !
    ! Settings about
    ! "interval", "origin", "terminus", "slice_start", "slice_end", "slice_stride",
    ! "space_average", "time_average", "newfile_interval"
    ! etc. are used as default values.
    ! Their set values are overwritten by
    ! "HistoryAutoAddVariable" or
    ! "NAMELIST#gtool_historyauto_nml".
    ! ("NAMELIST#gtool_historyauto_nml" is high priority,
    ! arguments of "HistoryAutoAddVariable" are medium,
    ! arguments of "HistoryAutoCreate" are low).
    !
    use gtool_historyauto_internal, only: initialized, version, sub_sname, &
      & zero_time, numdims, &
      & title_save, source_save, institution_save, conventions_save, &
      & gt_version_save, rank_save, save_mpi_split, save_mpi_gather, &
      & time_unit_bycreate, time_unit_suffix, gthst_axes, data_axes, &
      & all_output_save, gthstnml, cal_save
    use gtool_history, only: HistoryAxisCreate, HistoryAxisAddAttr
    use gtool_history_nmlinfo_generic, only: HstNmlInfoCreate, HstNmlInfoAdd, &
      & HstNmlInfoEndDefine, HstNmlInfoPutLine, HstNmlInfoAllNameValid, &
      & HstNmlInfoInquire
    use dc_trace, only: BeginSub, EndSub
    use dc_error, only: StoreError, DC_NOERR, DC_EALREADYINIT, &
      & GT_EARGSIZEMISMATCH, HST_ENOTIMEDIM, DC_ENEGATIVE
    use netcdf, only: NF90_EMAXDIMS, NF90_MAX_DIMS
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_present, only: present_and_not_empty, present_and_true, &
      & present_select
    use dc_calendar, only: DC_CAL, DC_CAL_DATE, &
      & DCCalDateInquire, DCCalInquire, DCCalDefault, DCCalConvertByUnit
    use dc_date, only: DCDiffTimeCreate, EvalbyUnit, toChar, toCharCal, Eval
    use dc_date_types, only: DC_DIFFTIME, DC_DATETIME
    use dc_message, only: MessageNotify
    use dc_iounit, only: FileOpen
    use dc_types, only: DP, STRING, TOKEN
    implicit none
    character(*), intent(in):: title
                              ! データ全体の表題.
                              ! Title of entire data
    character(*), intent(in):: source
                              ! データを作成する際の手段.
                              ! Source of data file
    character(*), intent(in):: institution
                              ! ファイルを最終的に変更した組織/個人.
                              ! Institution or person that changes files for the last time
    character(*), intent(in):: dims(:)
                              ! 次元の名前.
                              !
                              ! 配列の大きさに制限はありません.
                              ! 個々の次元の文字数は dc_types#TOKEN まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で
                              ! 補ってください.
                              !
                              ! Names of dimensions.
                              !
                              ! Length of array is unlimited.
                              ! Limits of numbers of characters of each
                              ! dimensions are "dc_types#TOKEN".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    integer, intent(in):: dimsizes (:)
                              ! dims で指定したそれぞれの次元大きさ.
                              !
                              ! 配列の大きさは dims の大きさと等しい
                              ! 必要があります.  '0' (数字のゼロ) を指定
                              ! するとその次元は 無制限次元 (unlimited
                              ! dimension) となります. (gtool_history
                              ! では時間の次元に対して無制限次元を
                              ! 用いることを想定しています). ただし,
                              ! 1 つの NetCDF ファイル (バージョン 3)
                              ! は最大で 1 つの無制限次元しか持てないので,
                              ! 2 ヶ所以上に '0' を指定しないでください.
                              ! その場合, 正しく gtool4 データが出力されません.
                              !
                              ! Lengths of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".  If '0' (zero) is
                              ! specified, the dimension is treated as
                              ! unlimited dimension.
                              ! (In "gtool_history", unlimited dimension is
                              ! expected to be used as time).
                              ! Note that one NetCDF file (version 3)
                              ! can not have two or more unlimited
                              ! dimensions, so that do not specify '0'
                              ! to two or more places. In that case,
                              ! gtoo4 data is not output currently
                              !
    character(*), intent(in):: longnames (:)
                              ! dims で指定したそれぞれの次元の名前.
                              !
                              ! 配列の大きさは dims の大きさ
                              ! と等しい必要があります. 文字数
                              ! は dc_types#STRING まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Names of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Limits of numbers of characters are
                              ! "dc_types#STRING".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    character(*), intent(in):: units(:)
                              ! dims で指定したそれぞれの次元の単位.
                              !
                              ! 配列の大きさは dims の大きさ
                              ! と等しい必要があります. 文字数
                              ! は dc_types#STRING まで.
                              ! 配列内の文字数は
                              ! 全て同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Units of dimensions specified with "dims".
                              !
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Limits of numbers of characters are
                              ! "dc_types#STRING".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    real(DP), intent(in):: origin
                              ! 出力開始時刻.
                              !
                              ! Start time of output.
                              !
    real(DP), intent(in):: terminus
                              ! 出力終了時刻.
                              !
                              ! End time of output.
                              !
    character(*), intent(in),  optional:: xtypes(:)
                              ! dims で指定したそれぞれの
                              ! 次元のデータ型.
                              !
                              ! デフォルトは float (単精度実数型)
                              ! です. 有効なのは,
                              ! double (倍精度実数型),
                              ! int (整数型) です. 指定しない
                              ! 場合や, 無効な型を指定した場合には,
                              ! float となります. なお, 配列の大きさ
                              ! は *dims* の大きさと等しい必要が
                              ! あります. 配列内の文字数は全て
                              ! 同じでなければなりません.
                              ! 足りない文字分は空白で補います.
                              !
                              ! Data types of dimensions specified
                              ! with "dims".
                              !
                              ! Default value is "float" (single precision).
                              ! Other valid values are
                              ! "double" (double precision),
                              ! "int" (integer).
                              ! If no value or invalid value is specified,
                              ! "float" is applied.
                              ! Length of this array must be same as
                              ! length of "dim".
                              ! Numbers of characters in this array
                              ! must be same.
                              ! Make up a deficit with blanks.
                              !
    character(*), intent(in), optional:: conventions
                              ! 出力するファイルの netCDF
                              ! 規約
                              !
                              ! 省略した場合,
                              ! もしくは空文字を与えた場合,
                              ! 出力する netCDF 規約の
                              ! Conventions 属性に値
                              ! gtool4_netCDF_Conventions
                              ! が自動的に与えられます.
                              !
                              ! NetCDF conventions of output file.
                              !
                              ! If this argument is omitted or,
                              ! blanks are given,
                              ! gtool4_netCDF_Conventions is given to
                              ! attribute "Conventions" of an output file
                              ! automatically.
                              !
    character(*), intent(in), optional:: gt_version
                              ! gtool4 netCDF 規約のバージョン
                              !
                              ! 省略した場合, gt_version 属性に
                              ! 規約の最新版のバージョンナンバー
                              ! gtool4_netCDF_version
                              ! が与えられます.
                              ! (ただし, 引数 conventions に
                              ! gtool4_netCDF_Conventions
                              ! 以外が与えられる場合は
                              ! gt_version 属性を作成しません).
                              !
                              ! Version of gtool4 netCDF Conventions.
                              !
                              ! If this argument is omitted,
                              ! latest version number of gtool4 netCDF
                              ! Conventions is given to attribute
                              ! "gt_version" of an output file
                              ! (However, gtool4_netCDF_Conventions is
                              ! not given to an argument "conventions",
                              ! attribute "gt_version" is not created).
                              !
    logical, intent(in), optional:: all_output
                              ! 登録変数を全て出力するためのフラグ.
                              !
                              ! .true. を指定すると,
                              ! HistoryAutoAddVariable で登録された
                              ! 変数が全て出力されるようになります.
                              !
                              ! *namelist_filename* が指定される場合
                              ! には, デフォルトは .false. となります.
                              ! この場合には,
                              ! *namelist_filename* に指定された
                              ! NAMELIST ファイルから読み込まれる
                              ! NAMELIST#gtool_historyauto_nml
                              ! で指定された変数のみ出力されます.
                              !
                              ! *namelist_filename* が指定されない場合
                              ! には, .true. となります.
                              !
                              !
                              ! Flag for output all registered variables.
                              !
                              ! When .true. is specified,
                              ! all variables registered by
                              ! "HistoryAutoAddVariable" are output.
                              !
                              ! If *namelist_filename* is specified,
                              ! default value becomes .false. .
                              ! In this case,
                              ! only variables specified in
                              ! "NAMELIST#gtool_historyauto_nml"
                              ! loaded from a NAMELIST file
                              ! *namelist_filename*.
                              !
                              ! If *namelist_filename* is not specified,
                              ! this value becomes .true. .
                              !
    character(*), intent(in), optional:: file_prefix
                              ! データのファイル名の接頭詞.
                              ! Prefixes of history data filenames
    character(*), intent(in), optional:: namelist_filename
                              ! NAMELIST ファイルの名称.
                              !
                              ! 省略した場合, もしくは空白文字を与えた場合,
                              ! NAMELIST ファイルは読み込みません.
                              !
                              ! Name of NAMELIST file.
                              !
                              ! If this argument is omitted,
                              ! or blanks are specified,
                              ! no NAMELIST file is loaded.
                              !
    real(DP), intent(in), optional:: interval
                              ! 出力時間間隔.
                              !
                              ! 省略した場合,
                              ! 自動的に 1.0 [sec] が設定されます.
                              !
                              ! Interval of output time.
                              !
                              ! If this argument is omitted,
                              ! 1.0 [sec] is specified
                              ! automatically.
                              !
    integer, intent(in), optional:: slice_start(:)
                              ! 空間方向の開始点.
                              !
                              ! 省略した場合, 座標データの開始点が設定されます.
                              !
                              ! Start points of spaces.
                              !
                              ! If this argument is omitted,
                              ! start points of dimensions are set.
                              !
    integer, intent(in), optional:: slice_end(:)
                              ! 空間方向の終了点.
                              !
                              ! 省略した場合, もしくは負の値が与えら得た場合,
                              ! 座標データの終了点が設定されます.
                              !
                              ! End points of spaces.
                              !
                              ! If this argument is omitted or
                              ! negative value is specified,
                              ! end points of dimensions are set.
                              !
    integer, intent(in), optional:: slice_stride(:)
                              ! 空間方向の刻み幅.
                              !
                              ! 省略した場合, 1 が設定されます.
                              !
                              ! Strides of spaces
                              !
                              ! If this argument is omitted,
                              ! 1 is set.
                              !
    logical, intent(in), optional:: space_average(:)
                              ! 平均化のフラグ.
                              !
                              ! .true. が指定される座標に対して平均化を
                              ! 行います.
                              ! 省略した場合, .false. が設定されます.
                              !
                              ! Flag of average.
                              !
                              ! Axes specified .true. are averaged.
                              ! If this argument is omitted,
                              ! .false. is set.
                              !
    logical, intent(in), optional:: time_average
                              ! 出力データの時間平均フラグ.
                              ! デフォルトは .false.
                              ! Flag for time average of output data
                              ! Default value is .false.
    integer, intent(in), optional:: newfile_interval
                              ! ファイル分割時間間隔.
                              !
                              ! 省略した場合,
                              ! 時間方向へのファイル分割を行いません.
                              !
                              ! Interval of time of separation of a file.
                              !
                              ! If this argument is omitted,
                              ! a files is not separated in time direction.
                              !
    character(*), intent(in), optional:: rank
                              ! ランクの名称.
                              !
                              ! Name of a rank.
                              !
    type(DC_DATETIME), intent(in), optional:: origin_date
                              ! 出力開始日時.
                              ! この引数は廃止予定のため, start_date を使用して下さい.
                              !
                              ! Start date of output.
                              ! Use "start_date" because this argument is obsolete.
                              !
    logical, intent(in), optional:: origin_date_invalid
                              ! .true. を与えると, origin_date を無効にします.
                              !
                              ! If ".true." is given, "origin_date" is ignored.
    type(DC_CAL_DATE), intent(in), optional:: start_date
                              ! 出力開始日時.
                              !
                              ! Start date of output.
                              !
    type(DC_CAL), intent(in), optional:: cal
                              ! 暦情報.
                              ! これを指定しない場合, dc_calendar モジュールの
                              ! デフォルトの暦が使用されます.
                              !
                              ! Calendar.
                              ! If this argument is specified, default calendar in
                              ! "dc_calendar" module is used.
                              !
    logical, intent(in), optional:: flag_mpi_gather
                              ! MPI 使用時に, 各ノードで HistoryPut
                              ! に与えたデータを一つのファイルに統合して出力
                              ! する場合には .true. を与えてください.
                              ! デフォルトは .false. です.
                              !
                              ! .true. を与えた場合, HistoryPutAxisMPI
                              ! に全体の軸データを与えてください.
                              !
                              ! When MPI is used, if ".true." is given,
                              ! data given to "HistoryPut" on each node
                              ! is integrated and output to one file.
                              ! Default value is ".false.".
                              !
                              ! If .true. is given, give data of axes in
                              ! whole area to "HistoryPutAxisMPI"
                              !
    logical, intent(in), optional:: flag_mpi_split
                              ! MPI 使用時に, 各ノードで HistoryPut
                              ! に与えたデータをそれぞれ別名のファイルに
                              ! 出力する場合には .true. を与えてください.
                              ! デフォルトは .false. です.
                              !
                              ! When MPI is used, if ".true." is given,
                              ! data given to "HistoryPut" on each node
                              ! is split into discrete files.
                              ! Default value is ".false.".
                              !


    ! NAMELIST 変数群 ; NAMELIST group of variables
    character(STRING):: Name
                              ! 変数名.
                              ! 空白の場合には, この他の設定値は
                              ! gtool_historyauto モジュールにおいて
                              ! 出力されるデータ全ての
                              ! デフォルト値となります.
                              !
                              ! "Data1,Data2" のようにカンマで区切って複数
                              ! の変数を指定することも可能です.
                              !
                              ! Variable identifier.
                              ! If blank is given, other values are
                              ! used as default values of output data
                              ! in "gtool_historyauto".
                              !
                              ! Multiple variables can be specified
                              ! as "Data1,Data2" too. Delimiter is comma.
    character(STRING):: File
                              ! 出力ファイル名.
                              ! これはデフォルト値としては使用されません.
                              ! *Name* に値が設定されている時のみ有効です.
                              !
                              ! Output file name.
                              ! This is not used as default value.
                              ! This value is valid only when *Name* is
                              ! specified.

    real(DP):: IntValue
                              ! データの出力間隔の数値.
                              ! 負の値を与えると, 出力を抑止します.
                              ! Numerical value for interval of history data output
                              ! Negative values suppresses output.
    character(TOKEN):: IntUnit
                              ! データの出力間隔の単位.
                              ! Unit for interval of history data output
    character(TOKEN):: Precision
                              ! データの精度.
                              ! デフォルトは float (単精度実数型)
                              ! です. 有効なのは,
                              ! double (倍精度実数型),
                              ! int (整数型) です. 指定しない
                              ! 場合や, 無効な型を指定した場合には,
                              ! float となります.
                              !
                              ! Precision of history data
                              ! Default value is "float" (single precision).
                              ! Other valid values are
                              ! "double" (double precision),
                              ! "int" (integer).
                              ! If no value or invalid value is specified,
                              ! "float" is applied.
    character(STRING):: FilePrefix
                              ! データのファイル名の接頭詞.
                              ! Prefixes of history data filenames
    logical:: TimeAverage
                              ! 出力データの時間平均フラグ.
                              !
                              ! ".true." を与えると, 時間平均値が出力されます.
                              !
                              ! Flag for time average of output data
                              !
                              ! If ".ture." is specified,
                              ! time average values are output.
                              !
    logical:: AllOutput
                              ! 登録変数を全て出力するためのフラグ.
                              ! Flag for output all registered variables.
    real(DP):: OriginValue
                              ! 出力開始時刻.
                              ! Start time of output.
    character(TOKEN):: OriginUnit
                              ! 出力開始時刻の単位.
                              ! Unit of start time of output.
    real(DP):: TerminusValue
                              ! 出力終了時刻.
                              ! End time of output.
    character(TOKEN):: TerminusUnit
                              ! 出力終了時刻の単位.
                              ! Unit of end time of output.
    integer:: SliceStart(1:NF90_MAX_DIMS)
                              ! 空間方向の開始点.
                              ! Start points of spaces.
    integer:: SliceEnd(1:NF90_MAX_DIMS)
                              ! 空間方向の終了点.
                              !
                              ! 省略した場合, もしくは負の値が与えら得た場合,
                              ! 座標データの終了点が設定されます.
                              !
                              ! End points of spaces.
                              !
                              ! If this argument is omitted or
                              ! negative value is specified,
                              ! end points of dimensions are set.
                              !
    integer:: SliceStride(1:NF90_MAX_DIMS)
                              ! 空間方向の刻み幅.
                              ! Strides of spaces.
    logical:: SpaceAverage(1:NF90_MAX_DIMS)
                              ! 空間平均のフラグ.
                              ! Flag of spatial average.
    integer:: NewFileIntValue
                              ! ファイル分割時間間隔の数値.
                              ! Numerical value for interval of time of separation of a file.
    character(TOKEN):: NewFileIntUnit
                              ! ファイル分割時間間隔の単位.
                              ! Unit of interval of time of separation of a file.

    namelist /gtool_historyauto_nml/ &
      & Name, File, &
      & IntValue, IntUnit, &
      & Precision, &
      & FilePrefix, &
      & TimeAverage, AllOutput, &
      & OriginValue, OriginUnit, &
      & TerminusValue, TerminusUnit, &
      & SliceStart, SliceEnd, SliceStride, SpaceAverage, &
      & NewFileIntValue, NewFileIntUnit
                              ! gtool_historyauto モジュールのデータ用
                              ! NAMELIST 変数群名.
                              !
                              ! gtool_historyauto_generic#HistoryAutoCreate
                              ! を使用する際に, オプショナル引数 *namelist_filename*
                              ! へ NAMELIST ファイル名を指定することで,
                              ! そのファイルからこの NAMELIST 変数群を
                              ! 読み込みます.
                              !
                              ! NAMELIST group name for
                              ! history data of "gtool_historyauto" module.
                              !
                              ! If a NAMELIST filename is specified to
                              ! an optional argument *namelist_filename* when
                              ! "gtool_historyauto_generic#HistoryAutoCreate"
                              ! is used, this NAMELIST group is
                              ! loaded from the file.


    ! 作業変数 ; Work variables
    integer:: blank_index
    integer:: stat
    character(STRING):: cause_c
    integer:: unit_nml        ! NAMELIST ファイルオープン用装置番号.
                              ! Unit number for NAMELIST file open
    integer:: iostat_nml      ! NAMELIST 読み込み時の IOSTAT.
                              ! IOSTAT of NAMELIST read
    character(TOKEN):: pos_nml
                              ! NAMELIST 読み込み時のファイル位置.
                              ! File position of NAMELIST read
    integer:: i, j
    character(TOKEN):: my_xtype

    real(DP):: interval_work, origin_work, terminus_work
    integer:: date_day
    real(DP):: date_sec
    integer:: msnot_rank
    character(STRING):: date_str
    character(TOKEN):: cal_str, cal_type
    integer:: origin_year, origin_month, origin_day, origin_hour, origin_min
    real(DP):: origin_sec
    integer:: month_in_year, hour_in_day, min_in_hour
    integer, pointer:: day_in_month(:) =>null()
    real(DP):: sec_in_min
    character(*), parameter:: subname = "HistoryAutoCreate1"
  continue
    call BeginSub(subname, version = version)
    stat = DC_NOERR
    cause_c = ""

    ! このサブルーチンが 2 度呼ばれたらエラー
    ! Error is occurred when this subroutine is called twice
    !
    if ( initialized ) then
      stat = DC_EALREADYINIT
      cause_c = 'gtool_historyauto'
      goto 999
    end if

    ! ゼロ秒の作成.
    ! Create zero seconds
    !
    zero_time = 0.0_DP
!!$    call DCDiffTimeCreate( &
!!$      & zero_time, &        ! (out)
!!$      & sec = 0.0_DP  )     ! (in)

    ! 次元の数に関するエラー処理
    ! Error handling for number of dimensions
    !
    numdims = size(dims)

    if ( size(dimsizes) /= numdims ) then
      cause_c = 'dimsizes, dims'
    elseif ( size(longnames) /= numdims ) then
      cause_c = 'longnames, dims'
    elseif ( size(units) /= numdims ) then
      cause_c = 'units, dims'
    endif
    if ( trim(cause_c) /= "" ) then
      stat = GT_EARGSIZEMISMATCH
      goto 999
    end if

    if ( numdims > NF90_MAX_DIMS ) then
      stat = NF90_EMAXDIMS
      goto 999
    end if

    ! 時刻次元に関するエラー処理
    ! Error handling for time dimension
    !
    if ( dimsizes(numdims) /= 0 ) then
      call MessageNotify( 'W', subname, &
        & 'time dimension must be specified to the last of "dims"' )
      stat = HST_ENOTIMEDIM
      goto 999
    end if

    ! 出力ファイルの基本メタデータの保管
    ! Save basic meta data for output file
    !
    title_save       = title
    source_save      = source
    institution_save = institution

    conventions_save = ''
    if ( present(conventions) ) conventions_save = conventions

    gt_version_save = ''
    if ( present(gt_version) ) gt_version_save = gt_version

    rank_save = ''
    if ( present(rank) ) rank_save = rank

    ! MPI に関する情報の保管
    ! Save information about MPI
    !
    save_mpi_split  = present_and_true( flag_mpi_split )
    save_mpi_gather = present_and_true( flag_mpi_gather )

    msnot_rank = -1
    if ( save_mpi_gather ) msnot_rank = 0

    ! 時刻の単位のチェック
    ! Check units of time
    !
    time_unit_bycreate = units(numdims)
    time_unit_suffix = ''
    blank_index = index( trim( adjustl(time_unit_bycreate) ), ' ' )
    if ( blank_index > 1  ) then
      time_unit_suffix = time_unit_bycreate(blank_index+1:)
      time_unit_bycreate = time_unit_bycreate(1:blank_index-1)
    end if

    ! 座標軸データの保管
    ! Save axes data
    !
    do i = 1, numdims
      my_xtype = ''
      if ( present(xtypes) ) then
        if ( size(xtypes) >= i ) then
          my_xtype = xtypes(i)
        end if
      end if

      call HistoryAxisCreate( &
        &     axis = gthst_axes(i), &                     ! (out)
        &     name = dims(i),       size = dimsizes(i), & ! (in)
        & longname = longnames(i), units = units(i), &    ! (in)
        &    xtype = my_xtype )                           ! (in)

      allocate( data_axes(i) % a_axis( dimsizes(i) ) )
      data_axes(i) % a_axis = (/ ( real( j, DP ), j = 1, dimsizes(i) ) /)

    end do

    ! 暦の登録
    ! Register calendar
    !
    if ( present(cal) ) then
      cal_save = cal
    else
      call DCCalDefault( cal_save )
    end if

    ! 日時の指定
    ! Specify date
    !
    if ( present(start_date) ) then

      call DCCalDateInquire( &
        & date_str = date_str, &   ! (out)
        & date     = start_date, & ! (in) optional
        & cal      = cal &         ! (in) optional
        !    & , zone = "+09:00" &
        & )

      call DCCalDateInquire( &
        & origin_year, origin_month, origin_day, & ! (out) optional
        & origin_hour, origin_min,   origin_sec, & ! (out) optional
        & date     = start_date, & ! (in) optional
        & cal      = cal &         ! (in) optional
        & )

      call DCCalInquire( &
        & cal_str, &                          ! (out) optional
        & month_in_year    = month_in_year, & ! (out) optional
        & day_in_month_ptr = day_in_month , & ! (out) optional
        & hour_in_day      = hour_in_day  , & ! (out) optional
        & min_in_hour      = min_in_hour  , & ! (out) optional
        & sec_in_min       = sec_in_min   , & ! (out) optional
        & cal = cal_save )                    ! (in) optional

      ! 地球暦の場合のみ units 属性に "since ..." を付加
      !
      select case ( trim(cal_str) )
      case ( 'gregorian' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( 'julian' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( 'noleap' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( '360day' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      case ( 'cyclic' )
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // trim(date_str)
      end select

      ! 開始日時情報の付与
      !
      call HistoryAxisAddAttr( &
        & axis = gthst_axes(numdims), &    ! (inout)
        & attrname = 'origin', &           ! (in)
        & value = 'origin_year origin_month origin_day ' // &
        &         'origin_hour origin_min origin_sec' )    ! (in)

      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_year',  origin_year )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_month', origin_month )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_day',   origin_day )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_hour',  origin_hour )
      call HistoryAxisAddAttr( gthst_axes(numdims), 'origin_min',   origin_min )

      ! 暦情報の付与
      !
      call HistoryAxisAddAttr( &
        & axis = gthst_axes(numdims), &    ! (inout)
        & attrname = 'calendar', &         ! (in)
        & value = cal_str )                ! (in)

      if ( trim(cal_str) == 'user_defined' ) then
        call HistoryAxisAddAttr( gthst_axes(numdims), 'month_in_year', month_in_year )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'day_in_month',  day_in_month )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'hour_in_day',   hour_in_day )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'min_in_hour',   min_in_hour )
        call HistoryAxisAddAttr( gthst_axes(numdims), 'sec_in_min',    sec_in_min )
      end if

      deallocate( day_in_month )

    elseif ( present(origin_date) &
      &  .and. .not. present_and_true(origin_date_invalid) ) then
      call Eval( origin_date, &            ! (in)
        & day = date_day, sec = date_sec ) ! (out)
      if ( date_day /= 0 .or. date_sec /= 0.0 ) then
        time_unit_suffix = trim(time_unit_suffix) // &
          & ' since ' // toChar(origin_date)

        call HistoryAxisAddAttr( &
          & axis = gthst_axes(numdims), &    ! (inout)
          & attrname = 'calendar', &         ! (in)
          & value = toCharCal(origin_date) ) ! (in)

      end if
    end if

    ! 登録変数を全て出力するためのフラグの保管
    ! Save flag for output all registered variables
    !
    if ( present(all_output) ) all_output_save = all_output
    if ( .not. present_and_not_empty(namelist_filename) ) all_output_save = .true.
    AllOutput = all_output_save

    ! 出力時間間隔のデフォルト値設定
    ! Configure default interval of output time
    !
    if ( all_output_save ) then
      if ( present(interval) ) then
        interval_work = interval
!        interval_work = EvalbyUnit( interval, time_unit_bycreate )
      else
        interval_work = 1.0
      end if
    else
      interval_work = - 1.0
    end if

    ! 出力開始・終了時刻のデフォルト値設定
    ! Configure default origin/terminus time of output
    !
    origin_work = &
      & DCCalConvertByUnit( origin, time_unit_bycreate, 'sec', cal_save )
    terminus_work = terminus

!    origin_work = EvalbyUnit( origin, 'sec' )
!    terminus_work = EvalbyUnit( terminus, time_unit_bycreate )

    ! gtool_historyauto_nml へデフォルト値の設定
    ! Configure default values for "gtool_historyauto_nml"
    !
    call HstNmlInfoCreate( gthstnml ) ! (out)

    call HstNmlInfoAdd( &
      & gthstnml = gthstnml, &                  ! (inout)
      & name = '', &                            ! (in) optional
      & precision = 'float', &                  ! (in) optional
      & fileprefix = file_prefix, &             ! (in) optional
      & interval_value = interval_work, &       ! (in) optional
      & interval_unit  = time_unit_bycreate, &  ! (in) optional
      & origin_value   = origin_work, &         ! (in) optional
      & origin_unit    = 'sec', &               ! (in) optional
!!$      & origin_unit    = time_unit_bycreate, &  ! (in) optional
      & terminus_value = terminus_work, &       ! (in) optional
      & terminus_unit  = time_unit_bycreate, &  ! (in) optional
      & time_average = time_average, &          ! (in) optional
      & slice_start  = slice_start, &           ! (in) optional
      & slice_end    = slice_end, &             ! (in) optional
      & slice_stride = slice_stride, &          ! (in) optional
      & space_average = space_average, &        ! (in) optional
      & newfile_intvalue = newfile_interval, &  ! (in) optional
      & newfile_intunit = time_unit_bycreate )  ! (in) optional

    ! NAMELIST ファイルの読み込み
    ! Load NAMELIST file
    !
    if ( present_and_not_empty(namelist_filename) ) then
      call FileOpen( unit_nml, &          ! (out)
        & namelist_filename, mode = 'r' ) ! (in)

      iostat_nml = 0
      pos_nml = ''

      call MessageNotify( 'M', sub_sname, '----- "gtool_historyauto_nml" is loaded from "%c" -----', &
        & c1 = trim(namelist_filename), rank_mpi = msnot_rank )

      do while ( trim(pos_nml) /= 'APPEND' .and. iostat_nml == 0 )

        Name = ''
        File = ''
        call HstNmlInfoInquire( &
          & gthstnml = gthstnml, &             ! (in)
          & interval_value = IntValue, &       ! (out) optional
          & interval_unit = IntUnit, &         ! (out) optional
          & precision = Precision, &           ! (out) optional
          & time_average = TimeAverage, &      ! (out) optional
          & origin_value   = OriginValue, &    ! (out) optional
          & origin_unit    = OriginUnit, &     ! (out) optional
          & terminus_value = TerminusValue, &  ! (out) optional
          & terminus_unit  = TerminusUnit, &   ! (out) optional
          & slice_start  = SliceStart, &       ! (out) optional
          & slice_end    = SliceEnd, &         ! (out) optional
          & slice_stride = SliceStride, &      ! (out) optional
          & space_average = SpaceAverage, &    ! (out) optional
          & newfile_intvalue = NewFileIntValue, & ! (out) optional
          & newfile_intunit  = NewFileIntUnit, &  ! (out) optional
          & fileprefix = FilePrefix )          ! (out) optional

        read( unit = unit_nml, &            ! (in)
          &    nml = gtool_historyauto_nml, & ! (out)
          & iostat = iostat_nml )           ! (out)
        inquire( unit = unit_nml, & ! (in)
          &  position = pos_nml )   ! (out)

        if ( iostat_nml == 0 ) then

          ! NAMELIST から与えられた値が無効な場合, デフォルト値を使用
          ! Default values are used when values from NAMELIST are invalid
          !
          if ( .not. IntValue > 0.0 ) then
            IntValue = interval_work
            IntUnit  = time_unit_bycreate
          end if
          if ( .not. OriginValue > 0.0 ) then
            OriginValue = origin_work
            OriginUnit  = 'sec'
          end if
          if ( .not. TerminusValue > 0.0 ) then
            TerminusValue = terminus_work
            TerminusUnit  = time_unit_bycreate
          end if

          ! 情報の登録
          ! Register information
          !
          call HstNmlInfoAdd( &
            & gthstnml = gthstnml, &             ! (inout)
            & name = Name, &                     ! (in) optional
            & file = File, &                     ! (in) optional
            & interval_value = IntValue, &       ! (in) optional
            & interval_unit = IntUnit, &         ! (in) optional
            & precision = Precision, &           ! (in) optional
            & time_average = TimeAverage, &      ! (in) optional
            & origin_value   = OriginValue, &    ! (in) optional
            & origin_unit    = OriginUnit, &     ! (in) optional
            & terminus_value = TerminusValue, &  ! (in) optional
            & terminus_unit  = TerminusUnit, &   ! (in) optional
            & slice_start  = SliceStart, &       ! (in) optional
            & slice_end    = SliceEnd, &         ! (in) optional
            & slice_stride = SliceStride, &      ! (in) optional
            & space_average = SpaceAverage, &    ! (in) optional
            & newfile_intvalue = NewFileIntValue, & ! (in) optional
            & newfile_intunit  = NewFileIntUnit, &  ! (in) optional
            & fileprefix = FilePrefix )          ! (in) optional

          ! 登録変数を全て出力するためのフラグの保管
          ! Save flag for output all registered variables
          !
          if ( trim(Name) == '' ) then
            all_output_save = AllOutput
          end if

          ! 印字 ; Print
          !
          if ( trim(File) == '' ) File = trim(FilePrefix) // '<Name>.nc'

          if ( trim(Name) == '' ) then
            call MessageNotify( 'M', sub_sname, 'Global Settings:', rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  AllOutput       = %b', l  = (/ AllOutput   /), rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  FilePrefix      = %c', c1 = trim(FilePrefix   ), rank_mpi = msnot_rank )
          else
            call MessageNotify( 'M', sub_sname, 'Individual Settings:', rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  Name            = %c', c1 = trim(Name           ), rank_mpi = msnot_rank )
            call MessageNotify( 'M', sub_sname, '  File            = %c', c1 = trim(File           ), rank_mpi = msnot_rank )
          end if
          call MessageNotify( 'M', sub_sname, '  Interval        = %f [%c]', &
            & d = (/ IntValue /), c1 = trim( IntUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  Precision       = %c', c1 = trim(Precision    ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  TimeAverage     = %b', l  = (/ TimeAverage   /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  Origin          = %f [%c]', &
            & d = (/ OriginValue /), c1 = trim( OriginUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  Terminus        = %f [%c]', &
            & d = (/ TerminusValue /), c1 = trim( TerminusUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SliceStart      = (/ %*d /)', &
            &                                i = SliceStart(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SliceEnd        = (/ %*d /)', &
            &                                i = SliceEnd(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SliceStride     = (/ %*d /)', &
            &                                i = SliceStride(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  SpaceAverage    = (/ %*b /)', &
            &                                l = SpaceAverage(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '  NewFileInterval = %d [%c]', &
            & i = (/ NewFileIntValue /), c1 = trim( NewFileIntUnit ), rank_mpi = msnot_rank )
          call MessageNotify( 'M', sub_sname, '', rank_mpi = msnot_rank )

        else
          call MessageNotify( 'M', sub_sname, '----- loading is finished (iostat=%d) -----', &
            & i = (/iostat_nml/), rank_mpi = msnot_rank )
        end if
      end do

      close( unit_nml )


    ! NAMELIST ファイルを読み込まない場合
    ! NAMELIST file is not loaded
    !
    else
      call MessageNotify( 'M', sub_sname, '----- "gtool_historyauto_nml" is not loaded" -----', rank_mpi = msnot_rank )
      Name = ''
      File = ''
      call HstNmlInfoInquire( &
        & gthstnml = gthstnml, &             ! (in)
        & interval_value = IntValue, &       ! (out) optional
        & interval_unit = IntUnit, &         ! (out) optional
        & precision = Precision, &           ! (out) optional
        & time_average = TimeAverage, &      ! (out) optional
        & origin_value   = OriginValue, &    ! (out) optional
        & origin_unit    = OriginUnit, &     ! (out) optional
        & terminus_value = TerminusValue, &  ! (out) optional
        & terminus_unit  = TerminusUnit, &   ! (out) optional
        & slice_start  = SliceStart, &       ! (out) optional
        & slice_end    = SliceEnd, &         ! (out) optional
        & slice_stride = SliceStride, &      ! (out) optional
        & space_average = SpaceAverage, &    ! (out) optional
        & newfile_intvalue = NewFileIntValue, & ! (out) optional
        & newfile_intunit  = NewFileIntUnit, &  ! (out) optional
        & fileprefix = FilePrefix )          ! (out) optional

      ! 印字 ; Print
      !
      call MessageNotify( 'M', sub_sname, 'Global Settings:', rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  AllOutput       = %b', l  = (/ AllOutput   /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  FilePrefix      = %c', c1 = trim(FilePrefix   ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Interval        = %f [%c]', &
        & d = (/ IntValue /), c1 = trim( IntUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Precision       = %c', c1 = trim(Precision    ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  TimeAverage     = %b', l  = (/ TimeAverage   /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Origin          = %f [%c]', &
        & d = (/ OriginValue /), c1 = trim( OriginUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  Terminus        = %f [%c]', &
        & d = (/ TerminusValue /), c1 = trim( TerminusUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SliceStart      = (/ %*d /)', &
        &                                i = SliceStart(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SliceEnd        = (/ %*d /)', &
        &                                i = SliceEnd(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SliceStride     = (/ %*d /)', &
            &                                i = SliceStride(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  SpaceAverage    = (/ %*b /)', &
        &                                l = SpaceAverage(1:numdims-1), n = (/ numdims-1 /), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '  NewFileInterval = %d [%c]', &
        & i = (/ NewFileIntValue /), c1 = trim( NewFileIntUnit ), rank_mpi = msnot_rank )
      call MessageNotify( 'M', sub_sname, '' , rank_mpi = msnot_rank)

    end if

    ! 終了処理, 例外処理
    ! Termination and Exception handling
    !
    initialized = .true.

999 continue
    call StoreError(stat, subname, cause_c = cause_c)
    call EndSub(subname, 'stat=%d', i = (/stat/) )
  end subroutine HistoryAutoCreate1
