!= "GTHST_NMLINFO" 変数の出力情報を問い合わせ
!= Inquire output information of a "GTHST_NMLINFO" variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: hstnmlinfoinquire.f90,v 1.2 2009-10-10 10:59:00 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2007-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
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
    !
    ! 変数の出力情報を問い合わせます. 
    !
    ! デフォルト値を取得するには, *name* を与えないか, または
    ! *name* に空白を与えてください. 
    !
    ! *name* に関するデータが存在しない場合, エラーを発生させます. 
    !
    ! なお, 与えられた *gthstnml* が HstNmlInfoCreate によって初期設定
    ! されていない場合, プログラムはエラーを発生させます. 
    !
    ! Inquire output information of a variable.
    ! 
    ! If data correspond to *name* is not found, 
    ! error is occurred. 
    !
    ! If *gthstnml* is not initialized by "HstNmlInfoCreate" yet, 
    ! error is occurred.
    !
    use gtool_history_nmlinfo_types, only: GTHST_NMLINFO, GTHST_NMLINFO_ENTRY
    use gtool_history_nmlinfo_internal, only: ListSearch
    use dc_trace, only: BeginSub, EndSub
    use dc_string, only: PutLine, Printf, Split, StrInclude, StoA, JoinChar
    use dc_present, only: present_and_not_empty, present_and_true
    use dc_types, only: DP, STRING, TOKEN, STDOUT
    use dc_error, only: StoreError, DC_NOERR, DC_ENOTINIT, DC_EARGLACK, DC_ENOENTRY
    implicit none
    type(GTHST_NMLINFO), intent(in):: gthstnml
    character(*), intent(in), optional:: name
                              ! 変数名. 
                              ! 先頭の空白は無視されます. 
                              ! 
                              ! Variable identifier. 
                              ! Blanks at the head of the name are ignored. 
    character(*), intent(out), optional:: file
                              ! ヒストリデータのファイル名. 
                              ! History data filenames
    real(DP), intent(out), optional:: interval_value
                              ! ヒストリデータの出力間隔の数値. 
                              ! 負の値を与えると, 出力を抑止します. 
                              ! 
                              ! Numerical value for interval of history data output. 
                              ! Negative values suppresses output.
    character(*), intent(out), optional:: interval_unit
                              ! ヒストリデータの出力間隔の単位. 
                              ! Unit for interval of history data output
    character(*), intent(out), optional:: precision
                              ! ヒストリデータの精度. 
                              ! Precision of history data
    logical, intent(out), optional:: time_average
                              ! 出力データの時間平均化フラグ. 
                              ! Flag for time average of output data.
    logical, intent(out), optional:: average
                              ! time_average の旧版. 
                              ! Old version of "time_average"
    character(*), intent(out), optional:: fileprefix
                              ! ヒストリデータのファイル名の接頭詞. 
                              ! Prefixes of history data filenames
    real(DP), intent(out), optional:: origin_value
                              ! 出力開始時刻. 
                              ! Start time of output. 
    character(*), intent(out), optional:: origin_unit
                              ! 出力開始時刻の単位. 
                              ! Unit of start time of output. 
    real(DP), intent(out), optional:: terminus_value
                              ! 出力終了時刻. 
                              ! End time of output. 
    character(*), intent(out), optional:: terminus_unit
                              ! 出力終了時刻の単位. 
                              ! Unit of end time of output. 
    integer, intent(out), optional:: slice_start(:)
                              ! 空間方向の開始点. 
                              ! Start points of spaces. 
    integer, intent(out), optional:: slice_end(:)
                              ! 空間方向の終了点. 
                              ! End points of spaces. 
    integer, intent(out), optional:: slice_stride(:)
                              ! 空間方向の刻み幅. 
                              ! Strides of spaces. 
    logical, intent(out), optional:: space_average(:)
                              ! 平均化のフラグ. 
                              ! Flag of average. 
    integer, intent(out), optional:: newfile_intvalue
                              ! ファイル分割時間間隔. 
                              ! Interval of time of separation of a file. 
    character(*), intent(out), optional:: newfile_intunit
                              ! ファイル分割時間間隔の単位. 
                              ! Unit of interval of time of separation of a file. 

    logical, intent(out), optional:: err
                              ! 例外処理用フラグ. 
                              ! デフォルトでは, この手続き内でエラーが
                              ! 生じた場合, プログラムは強制終了します. 
                              ! 引数 *err* が与えられる場合, 
                              ! プログラムは強制終了せず, 代わりに
                              ! *err* に .true. が代入されます. 
                              !
                              ! Exception handling flag. 
                              ! By default, when error occur in 
                              ! this procedure, the program aborts. 
                              ! If this *err* argument is given, 
                              ! .true. is substituted to *err* and 
                              ! the program does not abort. 

    !-----------------------------------
    !  作業変数
    !  Work variables
    type(GTHST_NMLINFO_ENTRY), pointer:: hptr =>null()
    character(STRING):: name_work
    integer:: stat, ary_size
    character(STRING):: cause_c
    character(*), parameter:: subname = 'HstNmlInfoInquire'
  continue
    call BeginSub( subname )
    stat = DC_NOERR
    cause_c = ''

    !-----------------------------------------------------------------
    !  初期設定のチェック
    !  Check initialization
    !-----------------------------------------------------------------
    if ( .not. gthstnml % initialized ) then
      stat = DC_ENOTINIT
      cause_c = 'GTHST_NMLINFO'
      goto 999
    end if

    !-----------------------------------------------------------------
    !  *gthstnml* 内から, *name* に関する情報を探査.
    !  Search information correspond to *name* in *gthstnml*
    !-----------------------------------------------------------------
    if ( present(name) ) then
      name_work = name
    else
      name_work = ''
    end if
    hptr => gthstnml % gthstnml_list
    call ListSearch( gthstnml_list = hptr, & ! (inout)
      &              name = name_work )      ! (in)

    if ( .not. associated( hptr ) ) then
      stat = DC_ENOENTRY
      cause_c = adjustl( name_work )
      goto 999
    end if

    if ( hptr % name == '' ) then
      if ( present(file)           ) file           = ''
    else
      if ( present(file)           ) file           = trim( hptr % fileprefix ) // hptr % file
    end if
    if ( present(interval_value) ) interval_value = hptr % interval_value
    if ( present(interval_unit)  ) interval_unit  = hptr % interval_unit 
    if ( present(precision)      ) precision      = hptr % precision     
    if ( present(average)        ) average        = hptr % time_average  
    if ( present(time_average)   ) time_average   = hptr % time_average  
    if ( present(fileprefix)     ) fileprefix     = hptr % fileprefix    

    if ( present(origin_value    ) ) origin_value     = hptr % origin_value    
    if ( present(origin_unit     ) ) origin_unit      = hptr % origin_unit     
    if ( present(terminus_value  ) ) terminus_value   = hptr % terminus_value  
    if ( present(terminus_unit   ) ) terminus_unit    = hptr % terminus_unit   
    if ( present(slice_start     ) ) then
      ary_size = size(slice_start)
      slice_start  = hptr % slice_start(1:ary_size)
    end if
    if ( present(slice_end     ) ) then
      ary_size = size(slice_end)
      slice_end  = hptr % slice_end(1:ary_size)
    end if
    if ( present(slice_stride     ) ) then
      ary_size = size(slice_stride)
      slice_stride  = hptr % slice_stride(1:ary_size)
    end if
    if ( present(space_average     ) ) then
      ary_size = size(space_average)
      space_average  = hptr % space_average(1:ary_size)
    end if
    if ( present(newfile_intvalue) ) newfile_intvalue = hptr % newfile_intvalue
    if ( present(newfile_intunit ) ) newfile_intunit  = hptr % newfile_intunit 

    nullify( hptr )

    !-----------------------------------------------------------------
    !  終了処理, 例外処理
    !  Termination and Exception handling
    !-----------------------------------------------------------------
999 continue
    call StoreError( stat, subname, err, cause_c )
    call EndSub( subname )
  end subroutine HstNmlInfoInquire
