subroutine GDNcFileOpen(fileid, filename, writable, overwrite, stat, err)
  use gtdata_netcdf_file_types, only: GD_NC_FILE_ID_ENTRY
  use gtdata_netcdf_file_internal, only: id_head, id_used
  use netcdf, only: &
    & NF90_WRITE, &
    & NF90_NOWRITE, &
    & NF90_ENOTNC4, &
    & NF90_NOERR, &
    & NF90_NOCLOBBER, &
    & NF90_CLOBBER, &
    & NF90_OPEN, &
    & NF90_CREATE
  use dc_message, only: MessageNotify
  use dc_error, only: StoreError
  use dc_types, only: STRING
  use dc_trace, only: BeginSub, EndSub
  implicit none
  integer, intent(out):: fileid
  character(len = *), intent(in):: filename
  logical, intent(in), optional:: writable
                                        ! .TRUE. は書き込みモード、
                                        ! .FALSE. は読込モード。
                                        ! 読込モードの際にファイルが
                                        ! ファイルが存在しないと
                                        ! エラーになる。
                                        ! デフォルトは読み込みモード
  logical, intent(in), optional:: overwrite
                                        ! writable が .TRUE. の
                                        ! 場合のみ有効。
                                        ! .TRUE. ならば上書きモード
                                        ! .FALSE. の場合、既存の
                                        ! ファイルが存在すると
                                        ! エラーになる
  logical, intent(out), optional:: err
  integer, intent(out), optional:: stat
  logical:: writable_required
  logical:: overwrite_required
  type(GD_NC_FILE_ID_ENTRY), pointer:: identptr, prev
  integer:: mystat, mode
  character(len = 256):: real_filename
  character(len = STRING):: cause_c
  character(*), parameter:: subname = "GDNcFileOpen"
continue
  fileid = -1
  !
  ! オプションの解釈
  !
  writable_required = .FALSE.
  overwrite_required = .FALSE.
  if (present(writable)) writable_required = writable
  if (present(overwrite)) overwrite_required = overwrite
  call BeginSub(subname, 'writable=%y overwrite=%y file=%c', &
    & L=(/writable_required, overwrite_required/), c1=trim(filename))
  !
  ! 同じ名前で書込み可能性も適合していれば nf90_open しないで済ませる
  !
  if (id_used) then
    identptr => id_head
    nullify(prev)
    do
      if ((identptr % filename == filename) &
        & .and. (identptr % writable .or. .not. writable_required)) then
        fileid = identptr % id
        identptr % count = identptr % count + 1
        if (present(err)) err = .FALSE.
        if (present(stat)) stat = NF90_NOERR
        mystat = NF90_NOERR
        goto 999
      endif
      prev => identptr
      identptr => identptr % next
      if (.not. associated(identptr)) exit
    enddo
    allocate(identptr)
    prev%next => identptr
  else
    nullify(prev)
    allocate(id_head)
    identptr => id_head
    id_used = .TRUE.
  endif
  nullify(identptr % next)
  identptr % filename = filename
  identptr % writable = writable_required
  identptr % count = 1
  !
  ! URL の部分的サポート
  !
  real_filename = filename
  if (real_filename(1:8) == 'file:///') then
    real_filename = real_filename(8: )
  else if (real_filename(1:5) == 'file:' .AND. real_filename(6:6) /= '/') then
    real_filename = real_filename(6: )
  endif
  !
  ! いざ nf90_open
  !
  mode = NF90_NOWRITE
  if (writable_required) mode = ior(mode, NF90_WRITE)
  ! 既に nc ファイルがあると思って開けてみる
  mystat = NF90_OPEN(real_filename, mode, identptr % id)
  !
  ! ファイルが既に存在する場合
  !
  if (mystat == NF90_NOERR) then
    ! 書き込みモードの場合
    if (writable_required) then
      if (overwrite_required) then
        ! 上書きモードの場合
        mode = NF90_CLOBBER
        call MessageNotify('M', subname, &
          & '"%c" is overwritten.', c1=trim(filename), rank_mpi = -1)
      else
        ! 上書き禁止モードの場合
        mode = NF90_NOCLOBBER
        call MessageNotify('W', subname, &
          & '"%c" is opened in write-protect mode.', c1=trim(filename), rank_mpi = -1)
      end if
      mystat = NF90_CREATE(real_filename, mode, identptr % id)
      if (mystat /= NF90_NOERR) then
        cause_c=filename
        if (present(stat)) stat = mystat
        goto 999
      end if
    endif
    ! 読み込みモードの場合は何もしない
  else
    !
    ! ファイルが無かった場合
    !
    if (.not. writable_required) then
      ! 読み込みモードの場合
      !
      ! 「無いよ」とエラーを吐いて終了
      if (mystat /= NF90_NOERR) then
        cause_c=filename
        if (present(stat)) stat = mystat
        goto 999
      end if
    else
      ! 書き込みモードの場合
      mode = NF90_CLOBBER
      ! ファイルを作成する
      mystat = NF90_CREATE(real_filename, mode, identptr % id)
      if (mystat /= NF90_NOERR) then
        cause_c=filename
        if (present(stat)) stat = mystat
        goto 999
      end if
    endif
  endif

  fileid = identptr % id

  ! 失敗したら消しておく
  if (mystat /= NF90_NOERR) then
    if (associated(prev)) then
      prev%next => identptr % next
    else
      id_head => identptr % next
      if (.not. associated(id_head)) id_used = .FALSE.
    endif
    deallocate(identptr)
    fileid = -1
  endif

  if (present(stat)) then
    stat = mystat
    if (present(err)) err = (stat /= NF90_NOERR)
  else
    cause_c=filename
    goto 999
  endif
999 continue
  call StoreError(mystat, subname, err, cause_c)
  call EndSub(subname, 'id=%d stat=%d', i=(/fileid, mystat/))
end subroutine GDNcFileOpen
