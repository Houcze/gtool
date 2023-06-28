!
!= 変数のコピー
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvarcreatecopy.f90,v 1.1 2009-03-20 09:09:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

subroutine GTVarCreateCopyC(var, url, copyfrom, copyvalue, &
  & overwrite, err)
  !
  !== 変数のコピー
  !
  ! 変数 *copyfrom* と同じ次元、属性を持った変数を *url* に作成します。
  ! 必要ならば次元変数も複製されます。
  ! *copyvalue* を <tt>.true.</tt> に指定すると値も複製されます。
  ! 作成された変数の ID は var に返されます。
  !
  ! 既存変数があるとき失敗しますが、
  ! overwrite == .true. であれば上書きして続行します。
  ! (まだ *overwrite* の動作は保障されていません)。
  !
  ! 作成の際にエラーが生じた場合、メッセージを出力してプログラムは
  ! 強制終了します。*err* を与えてある場合にはこの引数に .true.
  ! が返り、プログラムは終了しません。
  !
  !--
  ! なお、次元変数の複製は copyfrom と url が異なるファイルに
  ! 載っている場合に行なわれる。これは netCDF/an を想定したものだが
  ! ほかのファイル形式が追加されたときには変更を要するかもしれない。
  !++
  !
  use gtdata_types, only: GT_VARIABLE
  use dc_types, only: STRING, TOKEN
  use gtdata_generic, only: Open, Inquire, Close, Create, Copy_Attr
  use dc_url, only: UrlSplit, GT_ATMARK
  use dc_trace, only: BeginSub, EndSub
  use dc_error, only: StoreError, GT_ENOMEM
  implicit none
  intrinsic trim
  type(GT_VARIABLE),    intent(out)   :: var
  character(len = *),   intent(in)    :: url
  type(GT_VARIABLE),    intent(inout) :: copyfrom
  logical, intent(in),  optional      :: copyvalue
  logical, intent(in),  optional      :: overwrite
  logical, intent(out), optional      :: err
  type(GT_VARIABLE),    allocatable   :: vDimSource(:)
  type(GT_VARIABLE),    allocatable   :: vDimDest(:)
  integer                             :: i, nd, stat
  logical                             :: myerr
  character(STRING)                   :: vpart, upart, desturl
  character(TOKEN)                    :: xtype
  character(len = *),      parameter:: version = &
    & '$Name:  $' // &
    & '$Id: gtvarcreatecopy.f90,v 1.1 2009-03-20 09:09:51 morikawa Exp $'
continue
  call BeginSub('gtvarcreatecopy', 'url=%c copyfrom=%d', &
    & c1=trim(url), i=(/copyfrom%mapid/), version=version)
  stat = 0
  myerr = .FALSE.
  !-----------------------------------------------------------------
  !  コピーする変数の次元をコピー先のファイルに作成
  !-----------------------------------------------------------------
  !----- コピー元 copyfrom の次元変数の取得 -----
  call Inquire(copyfrom, alldims=nd)
  allocate(vDimSource(nd), vDimDest(nd), stat=stat)
  if (stat /= 0) goto 999
  desturl = url
  !----- コピー元 copyfrom の各次元情報を vDimSource に取り出し, -----
  !----- それをコピー先 desturl へコピーしてその次元 ID を       -----
  !----- vDimDest に返してもらう.                                -----
  do, i = 1, nd
    call Open(vDimSource(i), copyfrom, dimord=i, &
      & count_compact=.TRUE., err=myerr)
    call GTVarCopyDim(to=vDimDest(i), from=vDimSource(i), &
      & target=desturl)
  end do
  !-----------------------------------------------------------------
  !  変数作成
  !-----------------------------------------------------------------
  !----- url に変数名が無い場合, コピー元の変数名を使用 -----
  call UrlSplit(url, var=vpart)
  if (vpart == "") then
    call Inquire(copyfrom, url=upart)
    call UrlSplit(upart, var=vpart)
    desturl = trim(desturl) // GT_ATMARK // trim(vpart)
  end if
  !----- 実際に変数作成 -----
  call Inquire(copyfrom, xtype=xtype)
  call Create(var, trim(desturl), dims=vDimDest, xtype=xtype, &
    &      overwrite=overwrite, err=myerr)
  if (myerr) goto 990
  call copy_attr(to=var, from=copyfrom, err=myerr)
  if (myerr) goto 990
  if (present(copyvalue)) then
    if (copyvalue) then
      call GTVarCopyValue(to=var, from=copyfrom)
    endif
  endif
  do, i = 1, nd
    call Close(vDimSource(i))
    call Close(vDimDest(i))
  end do
990 continue
  deallocate(vDimSource, vDimDest, stat=stat)
999 continue
  if (stat /= 0) then
    call StoreError(GT_ENOMEM, "GTVarCreateCopy", err)
  else if (present(err)) then
    err = myerr
  else if (myerr) then
    call DumpError
  end if
  call EndSub('gtvarcreatecopy', 'result=%d', i=(/var%mapid/))
contains

  ! from と同じ内容の次元変数を URL target で示される変数の作成時に
  ! 次元として使えるように to に複写。
  ! なるべく再オープンで済まそうとする。
  ! 複写する場合もなるべく次元名を合わせようとする。
  !
  subroutine GTVarCopyDim(to, from, target)
    use gtdata_types
    use dc_types, only: token, string
    use dc_url, only: UrlSplit, UrlMerge, operator(.onthesamefile.)
    use gtdata_generic, only: Open, Inquire, Create, copy_attr
    type(GT_VARIABLE), intent(out):: to
    type(GT_VARIABLE), intent(inout):: from
    character(len = *), intent(in):: target
    character(len = string):: url, file, dimname
    character(len = token):: xtype
    logical:: growable, myerr
    integer:: length
  continue
    call BeginSub('gtvarcopydim', 'from=%d target=<%c>', &
      & i=(/from%mapid/), c1=trim(target))
    !----- 同じファイル上にコピーする場合は参照カウンタを1つ回すだけ -----
    call Inquire(var=from, url=url)
    if (trim(url) .onthesamefile. trim(target)) then
      call Open(to, from, dimord=0)
      call EndSub('gtvarcopydim', 'dup-handle')
      return
    endif
    !----- 異なるファイル上にコピーする場合, 既に次元変数 from が -----
    !----- target の次元変数として含まれるかチェック              -----
    call UrlSplit(target, file=file)
    if (LookupEquivalent(to, from, file)) then
      !----- 含まれる場合はそれで終了 -----
      call EndSub('gtvarcopydim', 'equivalent-exists')
      return
    else
      !----- 含まれない場合次元変数 from を target 上に作成 -----
      ! 次元変数 from が無制限次元である場合には長さを 0 に
      call Inquire(var=from, growable=growable, allcount=length)
      if (growable) length = 0
      call Inquire(var=from, xtype=xtype, name=dimname)
      !
      url = urlmerge(file, dimname)
      call Create(to, trim(url), length, xtype, err=myerr)
      if (myerr) then
        ! 指定名称でうまくいかない場合は自動生成名にする
        call Create(to, trim(file), length, xtype)
      endif
      call copy_attr(to, from, myerr)
      call GTVarCopyValue(to, from)
      call EndSub('gtvarcopydim', 'created')
      return
    endif
  end subroutine GTVarCopyDim

  !-----------------------------------------------------------------
  !  ・ 次元変数 from が既に file にあるのかを判定
  !       次元変数 from がコピー先の nc ファイル file に既に
  !       存在するなら .TRUE. しないなら .FALSE. を result に返す.
  !       result = .TRUE. が返る場合にはそれに該当する次元の ID を
  !       to に返す.
  !       - 判定条件は 1) from が無制限次元で, file も無制限次元を
  !         持つこと, または 2) 次元変数 from のサイズと一致する次元が
  !         file 内にあり, 且つその次元の単位名が from の単位名と一致
  !         すること.
  !         ※ もしかすると条件が足りないかも知れない.
  !-----------------------------------------------------------------
  logical function LookupEquivalent(to, from, file) result(result)
    use dc_types, only: string
    use dc_string, only: toChar
    use gtdata_generic, only: Inquire, GTVarSearch, Open, get_attr
    type(GT_VARIABLE), intent(out):: to
    type(GT_VARIABLE), intent(in):: from
    character(len = *), intent(in):: file
    character(len = string):: url, units1, units2, reason
    logical:: end, growable1, growable2
    integer:: len1, len2
    character(len = *), parameter:: subnam = "lookupequivalent"
    call BeginSub(subnam, 'from=%d file=<%c>', &
      & i=(/from%mapid/), c1=trim(file))
    result = .FALSE.
    !----- 次元変数 from のサイズと単位, 無制限次元かどうかを探査 -----
    call Inquire(from, allcount=len1, growable=growable1)
    call get_attr(from, 'units', units1, default='')
    !----- コピー先 file の変数情報を探査 -----
    ! とりあえずは次元だけでなく全ての変数について開く
    call GTVarSearch(file)
    do
      call GTVarSearch(url, end)
      if (end) exit
      call Open(to, url, writable=.TRUE., err=end)
      if (end) exit
      ! 次元変数のサイズと, 無制限次元かどうかを取得
      !   (次元変数でないもののサイズは, 依存する次元変数のサイズを
      !    掛け合わせたものとなるので, もしかすると誤動作するかも).
      call Inquire(to, allcount=len2, growable=growable2)
      ! 次元変数 from が無制限次元で, 且つ file 内の次元変数も
      ! 無制限次元の場合は, 同じ次元変数と考える.
      if (.not. growable1 .or. .not. growable2) then
        ! 次元変数 from のサイズと file 内の次元変数のサイズが
        ! 異なる場合はスキップ
        if (len1 /= len2) then
          call Close(to)
          cycle
        endif
        call get_attr(to, 'units', units2, default='')
        ! 本当は dc_units で比較すべきだがとりあえず文字列比較
        if (units1 /= units2) then
          call Close(to)
          cycle
        else
          reason = 'length of from is ' // trim(toChar(len1)) // &
            &   '. units of from is ' // "[" //               &
            &   trim(units1) // "]" //                        &
            &   '. And file has same length and units.'
        endif
      else
        reason = 'from is UNLIMITED dimension, and file has it'
      endif
      result = .TRUE.
      call EndSub(subnam, 'found (%c)', c1=trim(reason))
      return
    enddo
    call EndSub(subnam, 'not found')
  end function LookupEquivalent

  ! すでに存在する変数について、値をコピーする。
  !
  subroutine GTVarCopyValue(to, from)
    use gtdata_types, only: GT_VARIABLE
    use gtdata_generic, only: GTVarGetReal, GTVarPutReal, Inquire, Slice, Slice_Next
    use dc_error, only: DumpError
    use dc_string
    type(GT_VARIABLE), intent(inout):: to
    type(GT_VARIABLE), intent(inout):: from
    real, allocatable:: rbuffer(:)
    logical:: err
    integer:: siz, stat
    !
    call BeginSub('gtvarcopyvalue')
    ! 値のコピー
    call Slice(from)
    call Slice(to, compatible=from)
    call Inquire(from, size=siz)
    allocate (rbuffer(siz))
    do
      call GTVarGetReal(from, rbuffer, siz, err)
      if (err) call DumpError()
      call GtVarPutReal(to, rbuffer, siz, err)
      if (err) call DumpError()
      call Slice_Next(from, stat=stat)
      if (stat /= 0) exit
      call Slice_Next(to, stat=stat)
    enddo
    deallocate (rbuffer)
    call EndSub('gtvarcopyvalue')
  end subroutine GTVarCopyValue

end subroutine GTVarCreateCopyC
