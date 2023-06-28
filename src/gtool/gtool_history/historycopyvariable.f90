!= 変数定義のコピー
!= Copy definition of a variable
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: historycopyvariable.f90,v 1.3 2009-10-10 08:01:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2004-2009. All rights reserved.
! License::   See COPYRIGHT[link:../../../COPYRIGHT]
!
  subroutine HistoryCopyVariable1(file, varname, history, overwrite)
    !
    !== 変数定義 (別ファイルの変数コピー)
    !
    ! gtool4 データ内の変数の定義を行います。 他の gtool4 データの
    ! ファイル名とその中の変数名を指定することで、 自動的のその変数の
    ! 構造や属性をコピーして変数定義します。このサブルーチンを
    ! 用いる前に、 HistoryCreate による初期設定が必要です。
    !
    ! 構造や属性を手動で設定する場合には HistoryAddVariable
    ! を用いて下さい。
    !
    use gtool_history_types, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, GT_HISTORY_ATTR, GT_HISTORY_AVRDATA
    use gtool_history_internal, only: default
    use gtdata_generic, only: Create, Inquire, Open, Slice, Close
    use gtdata_types, only: GT_VARIABLE
    use dc_present, only: present_and_false
    use dc_url, only: UrlMerge, GT_ATMARK, UrlResolve
    use dc_date, only: DCDiffTimeCreate
    use dc_date_types, only: DC_DIFFTIME
    use dc_types, only: STRING, TOKEN, DP
    use dc_trace, only: BeginSub, EndSub, DbgMessage
    implicit none
    character(len = *), intent(in):: file
                              ! コピーしようとする変数が格納された
                              ! netCDF ファイル名
                              ! 
    character(len = *), intent(in):: varname
                              ! コピー元となる変数の名前
                              !
                              ! 定義される変数名もこれと
                              ! 同じになります。
                              ! 最大文字数は dc_types#TOKEN 。
                              !
                              ! 依存する次元が存在しない
                              ! 場合は自動的にその次元に関する
                              ! 変数情報も元のファイルから
                              ! コピーします。
                              ! この場合に「同じ次元」と見
                              ! なされるのは、(1) 無制限次
                              ! 元 (自動的に「時間」と認識
                              ! される)、
                              ! (2) サイズと単位が同じ次元、
                              ! です。
                              ! 
    type(GT_HISTORY), intent(inout), optional, target:: history
                              ! 出力ファイルの設定に関する情報を
                              ! 格納した構造体
                              ! 
                              ! ここに指定するものは、
                              ! HistoryCreate によって初期設定
                              ! されていなければなりません。
                              !
    logical, intent(in), optional:: overwrite
                              ! 上書きの可否の設定
                              !
                              ! この引数に .false. を渡すと、
                              ! 既存のファイルを上書きしません。
                              ! デフォルトは上書きします。
                              ! 

    type(GT_HISTORY),  pointer:: hst =>null()
    type(GT_VARIABLE), pointer:: vwork(:) =>null(), dimvars(:) =>null()
    type(GT_VARIABLE):: copyfrom
    character(STRING):: fullname, url, copyurl
    integer, pointer:: count_work(:) =>null()
    integer, pointer:: var_avr_count_work(:) =>null()
    integer:: var_avr_length
    logical, pointer:: var_avr_firstput_work(:) =>null()
    real(DP), pointer:: var_avr_coefsum_work(:) =>null()
    real(DP), pointer:: var_avr_baseint_work(:) =>null()
    real(DP), pointer:: var_avr_prevtime_work(:) =>null()
!!$    type(DC_DIFFTIME), pointer:: var_avr_baseint_work(:) =>null()
!!$    type(DC_DIFFTIME), pointer:: var_avr_prevtime_work(:) =>null()
    type(GT_HISTORY_AVRDATA), pointer:: var_avr_data_work(:) =>null()
    integer:: nvars, numdims, i
    logical:: growable, overwrite_required
    character(*), parameter:: subname = "HistoryCopyVariable1"
  continue
    call BeginSub(subname, 'file=%c varname=%c', &
      & c1=trim(file), c2=trim(varname))
    !----- 操作対象決定 -----
    if (present(history)) then
      hst => history
    else
      hst => default
    endif

    !----- 変数表拡張 -----
    if (associated(hst % vars)) then
      nvars = size(hst % vars(:))
      vwork => hst % vars
      count_work => hst % count
      nullify(hst % vars, hst % count)
      allocate(hst % vars(nvars + 1), hst % count(nvars + 1))
      hst % vars(1:nvars) = vwork(1:nvars)
      hst % count(1:nvars) = count_work(1:nvars)
      deallocate(vwork, count_work)
      count_work => hst % growable_indices
      nullify(hst % growable_indices)
      allocate(hst % growable_indices(nvars + 1))
      hst % growable_indices(1:nvars) = count_work(1:nvars)
      deallocate(count_work)

      !
      ! 平均値出力のための変数表コピー
      ! Copy table of variables for average value output
      !
      var_avr_count_work => hst % var_avr_count
      nullify( hst % var_avr_count )
      allocate( hst % var_avr_count(nvars + 1) )
      hst % var_avr_count(1:nvars) = var_avr_count_work(1:nvars)
      deallocate( var_avr_count_work )

      var_avr_data_work => hst % var_avr_data
      nullify(hst % var_avr_data)
      allocate(hst % var_avr_data(nvars + 1))
      do i = 1, nvars
        hst % var_avr_data(i) % length = var_avr_data_work(i) % length
        allocate(hst % var_avr_data(i) % &
          & a_DataAvr(var_avr_data_work(i) % length))
        hst % var_avr_data(i) % a_DataAvr = var_avr_data_work(i) % a_DataAvr
      end do
      deallocate( var_avr_data_work )

      var_avr_firstput_work => hst % var_avr_firstput
      nullify( hst % var_avr_firstput )
      allocate( hst % var_avr_firstput(nvars + 1) )
      hst % var_avr_firstput(1:nvars) = var_avr_firstput_work(1:nvars)
      deallocate( var_avr_firstput_work )

      var_avr_coefsum_work => hst % var_avr_coefsum
      nullify( hst % var_avr_coefsum )
      allocate( hst % var_avr_coefsum(nvars + 1) )
      hst % var_avr_coefsum(1:nvars) = var_avr_coefsum_work(1:nvars)
      deallocate( var_avr_coefsum_work )

      var_avr_baseint_work => hst % var_avr_baseint
      nullify( hst % var_avr_baseint )
      allocate( hst % var_avr_baseint(nvars + 1) )
      hst % var_avr_baseint(1:nvars) = var_avr_baseint_work(1:nvars)
      deallocate( var_avr_baseint_work )

      var_avr_prevtime_work => hst % var_avr_prevtime
      nullify( hst % var_avr_prevtime )
      allocate( hst % var_avr_prevtime(nvars + 1) )
      hst % var_avr_prevtime(1:nvars) = var_avr_prevtime_work(1:nvars)
      deallocate( var_avr_prevtime_work )
    else
      ! トリッキーだが、ここで count だけ 2 要素確保するのは、
      ! HistorySetTime による巻き戻しに備えるため。
      allocate(hst % vars(1), hst % count(2), hst % growable_indices(1))
      hst % count(2) = 0
      allocate(hst % var_avr_count(1), hst % var_avr_data(1))
      allocate(hst % var_avr_firstput(1), hst % var_avr_coefsum(1))
      allocate(hst % var_avr_baseint(1), hst % var_avr_prevtime(1))
    endif
    nvars = size(hst % vars(:))
    hst % growable_indices(nvars) = 0
    hst % count(nvars) = 0
    hst % var_avr_count(nvars) = -1
    hst % var_avr_firstput = .true.
    hst % var_avr_coefsum(nvars) = 0.0_DP
    hst % var_avr_baseint(nvars) = 0.0_DP
!!$    call DCDiffTimeCreate( &
!!$      & hst % var_avr_baseint(nvars), & ! (out)
!!$      & sec = 0.0_DP )                   ! (in)
    hst % var_avr_prevtime(nvars) = hst % var_avr_baseint(nvars)

    !----- コピー元ファイルの変数 ID 取得 -----
    copyurl = UrlMerge(file, varname)
    call Open(copyfrom, copyurl)

    !----- 変数コピー -----
    call Inquire(hst % dimvars(1), url=url)
    fullname = UrlResolve((GT_ATMARK // trim(varname)), trim(url))
    overwrite_required = .true.
    if (present_and_false(overwrite)) overwrite_required = .false.
    call Create(hst % vars(nvars), trim(fullname), copyfrom, &
      &      copyvalue=.FALSE., overwrite=overwrite_required)

    !----- 無制限次元の添字を探査 -----
    call Inquire(hst % vars(nvars), alldims=numdims)
    allocate(dimvars(numdims))
    ! 各次元毎に情報を取得し, growable == .TRUE. のもの (つまりは時間)
    ! の添字番号を取得する
    do, i = 1, numdims
      call Open(var=dimvars(i), source_var=hst % vars(nvars), &
        &    dimord=i, count_compact=.TRUE.)
      ! 各次元変数の growable を調べる
      call Inquire(var=dimvars(i), growable=growable)
      if (growable) then
        hst % growable_indices(nvars) = i
      endif
    enddo

    !----- 拡張可能次元があったらそれをサイズ 1 に拡張しておく -----
    if (hst % growable_indices(nvars) /= 0) then
      call Slice(hst % vars(nvars), hst % growable_indices(nvars), &
        & start=1, count=1, stride=1)
    endif

    deallocate(dimvars)

    call Inquire( hst % vars(nvars), size = var_avr_length )
    allocate( hst % var_avr_data(nvars) % a_DataAvr(var_avr_length) )
    hst % var_avr_data(nvars) % length = var_avr_length
    hst % var_avr_data(nvars) % a_DataAvr = 0.0_DP

    call Close(copyfrom)
    call EndSub(subname)
  end subroutine HistoryCopyVariable1
