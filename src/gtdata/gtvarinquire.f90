!
!= 変数または属性に関する問い合わせ
!
! Authors::   Eizi TOYODA, Yasuhiro MORIKAWA
! Version::   $Id: gtvarinquire.f90,v 1.5 2009-07-04 04:58:06 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#Inquire
! として提供されます。

subroutine GTVarInquire(var, growable, rank, alldims, allcount, &
  & size, xtype, name, url, err )
  !
  !== 変数に関する問い合わせ
  !
  ! 変数 *var* に関する問い合わせを行います。
  !
  ! 返り値となる引数の文字型の実引数の長さが足りないと、
  ! 結果が損なわれます。引数の文字列の長さとして dc_types#STRING
  ! を用いることを推奨します。
  !
  ! *Inquire* は複数のサブルーチンの総称名であり、
  ! 問い合わせ方法は複数用意されています。
  ! 下記のサブルーチンも参照してください。
  !
  ! 他にも変数に関する問い合わせのための手続きとして
  ! Get_Slice, Dimname_to_Dimord があります。
  !
  !--
  ! このサブルーチンは INQUIRE 文を模して作られたもので、
  ! オブジェクト・変数・属性に関する問い合わせを行います。
  !++
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use gtdata_netcdf_generic, only: inquire
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_trace, only: beginsub, endsub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(in):: var
  character(len=*), intent(out), optional:: xtype
                                        ! 外部型の名前
  character(len=*), intent(out), optional:: name
                                        ! name は変数名の最小の単位を返します。
                                        ! ファイル名を含まないため
                                        ! プログラム内での一意性は
                                        ! 保証されません。
                                        ! 
  character(len=*), intent(out), optional:: url
                                        ! url はファイル名のついた変数名
                                        ! を返します。
                                        ! プログラム内で一意です。
                                        ! 
  integer, intent(out), optional:: rank
                                        ! コンパクト(縮退)次元を数えない、
                                        ! 次元の数
                                        ! 
  integer, intent(out), optional:: alldims
                                        ! 縮退次元を含む全次元数。
                                        ! dimord には基本的にこちらを
                                        ! 使います。
                                        ! 
  integer, intent(out), optional:: allcount
                                        ! 変数が次元変数である場合、
                                        ! 総数を返します。
                                        ! エラーの場合はゼロを返します。
                                        ! 
  integer, intent(out), optional:: size
                                        ! 変数の入出力領域の大きさ。
                                        ! (変数が依存する各次元の長
                                        ! [格子点数]の積)
                                        ! 
  logical, intent(out), optional:: growable
                                        ! 変数が次元変数である場合、
                                        ! 自動拡張可能か否かを返します。
                                        ! 次元変数でない場合は不定となります。
                                        ! 
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
            integer:: class, cid
continue
  call beginsub('gtvarinquire', 'var.mapid=%d', i=(/var%mapid/))
  call var_class(var, class, cid)
  select case(class)
  case(vtb_class_netcdf)
    if (present(xtype) .or. present(name) .or. present(url)) then
      call inquire(GD_NC_VARIABLE(cid), xtype=xtype, name=name, url=url)
      if (present(xtype)) call DbgMessage('xtype=%c', c1=trim(xtype))
      if (present(name)) call DbgMessage('name=%c', c1=trim(name))
      if (present(url)) call DbgMessage('url=%c', c1=trim(url))
    endif
    if (present(growable)) then
      call inquire(GD_NC_VARIABLE(cid), growable=growable)
      call DbgMessage('growable=%y', L=(/growable/))
    endif
  case(vtb_class_memory)
    call DbgMessage('vtb_class_memory not implemented: skipped')
  end select
  if (present(alldims)) alldims = internal_get_alldims(var)
  if (present(allcount)) allcount = internal_get_allcount(var)
  if (present(size)) size = internal_get_size(var)
  if (present(rank)) rank = internal_get_rank(var)
  call endsub('gtvarinquire')
  return
contains

  integer function internal_get_alldims(var) result(result)
    use gtdata_internal_map, only: map_lookup
    implicit none
    type(GT_VARIABLE), intent(in):: var
    call map_lookup(var, ndims=result)
    call DbgMessage('alldims=%d', i=(/result/))
  end function internal_get_alldims

  integer function internal_get_allcount(var) result(result)
    use gtdata_internal_map, only: gt_dimmap, map_lookup
    implicit none
    type(GT_VARIABLE), intent(in):: var
    type(gt_dimmap), allocatable:: map(:)
    integer:: nd
    call map_lookup(var, ndims=nd)
    if (nd <= 0) then
      call DbgMessage('internal_get_allcount: no map')
      result = 1
      return
    endif
    allocate(map(nd))
    call map_lookup(var, map=map)
    result = product(map(1:nd)%allcount)
    call DbgMessage('internal_get_allcount: %d map.size=%d', &
      & i=(/result, nd/))
    deallocate(map)
  end function internal_get_allcount

  integer function internal_get_size(var) result(result)
    use gtdata_internal_map, only: gt_dimmap, map_lookup
    implicit none
    type(GT_VARIABLE), intent(in):: var
    type(gt_dimmap), allocatable:: map(:)
    integer:: nd
    call map_lookup(var, ndims=nd)
    if (nd <= 0) then
      call DbgMessage('internal_get_size: no map')
      result = 1
      return
    endif
    allocate(map(nd))
    call map_lookup(var, map=map)
    result = product(map(1:nd)%count)
    call DbgMessage('internal_get_size: %d map.size=%d', &
      & i=(/result, nd/))
    deallocate(map)
  end function internal_get_size

  integer function internal_get_rank(var) result(result)
    use gtdata_internal_map, only: gt_dimmap, map_lookup
    implicit none
    type(GT_VARIABLE), intent(in):: var
    type(gt_dimmap), allocatable:: map(:)
    integer:: nd

    call map_lookup(var, ndims=nd)
    if (nd <= 0) then
      call DbgMessage('internal_get_rank: no map')
      result = 0
      return
    endif
    allocate(map(nd))
    call map_lookup(var, map=map)
    result = count(map(1:nd)%count > 1)
    call DbgMessage('internal_get_rank: %d', i=(/result/))
    deallocate(map)
  end function internal_get_rank

end subroutine GTVarInquire

subroutine GTVarInquire2(var, allcount)
  !
  !== 変数の依存する次元 (複数) の総数の問い合わせ
  !
  ! 変数 *var* が依存する各次元の総数を返します。
  ! *allcount* の配列のサイズは依存する次元の数だけ必要です。
  ! 依存する次元の数は上記の *Inquire* の *alldims* で調べることが
  ! できます。
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: inquire, open, close
  use dc_trace, only: beginsub, endsub
  type(GT_VARIABLE), intent(in):: var
  integer, intent(out):: allcount(:) ! alldims 個必要
  integer:: i, n
  type(GT_VARIABLE):: v
  call beginsub('gtvarinquire2')
  call inquire(var, alldims=n)
  do, i = 1, n
    call Open(v, var, i, count_compact=.true.)
    call inquire(var, allcount=allcount(i))
    call Close(v)
  enddo
  call endsub('gtvarinquire2')
end subroutine

subroutine GTVarInquireA(var, attrname, xtype)
  !
  !== 変数の属性の型の問い合わせ
  !
  ! 変数 *var* の属性 *attrname* の値の型を *xtype* に返します。
  !
  !--
  ! 文字数が合わなければ当然変なことが起こるが、気にしない。
  !++
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory
  use dc_trace, only: beginsub, endsub
  use gtdata_netcdf_generic, only: inquire
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  type(GT_VARIABLE), intent(in):: var
  character(len=*), intent(in):: attrname
  character(len=*), intent(out), optional:: xtype
  integer:: class, cid
  character(len = *), parameter:: subnam = "gtvarinquireA"
continue
  call beginsub(subnam, "%c", c1=trim(attrname))
  call var_class(var, class, cid)
  select case(class)
  case(vtb_class_netcdf)
    call inquire(GD_NC_VARIABLE(cid), attrname=attrname, xtype=xtype)
  end select
  call endsub(subnam)
end subroutine GTVarInquireA

subroutine GTVarInquireD(var, dimord, url, allcount, err)
  !
  !== 変数の次元に関する問い合わせ
  !
  ! 変数 *var* の次元順序番号 *dimord* に対応する次元の
  ! URL *url* と総数 *allcout* を返します。
  ! 
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: open, close, inquire
  use dc_trace, only: beginsub, endsub
  implicit none
  type(GT_VARIABLE), intent(in):: var
  integer, intent(in):: dimord
  character(len=*), intent(out), optional:: url
  integer, intent(out), optional:: allcount
  logical, intent(out), optional:: err
  type(GT_VARIABLE):: dimvar
  character(len = *), parameter:: subnam = "gtvarinquireD"
continue
  call beginsub(subnam, "%d", i=(/dimord/))
  call open(dimvar, source_var=var, dimord=dimord, err=err)
  if (present(url)) call inquire(dimvar, url=url)
  if (present(allcount)) call inquire(dimvar, allcount=allcount)
  call close(dimvar)
  call endsub(subnam)
end subroutine GTVarInquireD
