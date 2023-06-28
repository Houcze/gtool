!
!= 独立変数 (次元) の作成
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvarcreated.f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

subroutine GTVarCreateD(var, url, length, xtype, long_name, overwrite, err)
  !
  !== 独立変数 (次元) の作成
  !
  ! 場所 *url* に長さ *length* の自分自身を次元とする変数つまり GT_VARIABLE 型
  ! の実体を作成し、それを第 1 引数 *var* にセットします。
  ! Open されたものと同様、第1引数 *var* は後で必ず
  ! Close されなければなりません。
  !
  ! 長さ length == 0 を指定するとその変数は可変長次元となります。
  ! 型 *xtype* を省略すると "+float+" と
  ! みなされます。既存変数があるとき失敗しますが、
  ! overwrite == .true. であれば上書きして続行します。
  ! (まだ *overwrite* の動作は保障されていません)。
  ! dims の省略は 0 次元変数の設定を意味します。
  !
  ! 次元変数は自動生成されることが多いため、変数名部を欠く指定に対しては
  ! 名前を自動生成します。
  !
  ! 作成の際にエラーが生じた場合、メッセージを出力してプログラムは
  ! 強制終了します。*err* を与えてある場合にはこの引数に .true.
  ! が返り、プログラムは終了しません。
  !
  use dc_string, only: StrHead
  use gtdata_types, only: GT_VARIABLE
  use gtdata_generic, only: GtDataTmpNam
  use gtdata_netcdf_generic, only: Create, Put_Attr
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use gtdata_memory_generic, only: Create
  use gtdata_memory_types, only: GD_MEM_VARIABLE
  use gtdata_internal_map, only: map_create, vtb_class_memory, vtb_class_netcdf, gtvar_dump
  use dc_url, only: UrlSplit, UrlMerge
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  use dc_error, only: StoreError, DC_NOERR
  use dc_types, only: STRING
  implicit none
  type(GT_VARIABLE), intent(out):: var
  character(len = *), intent(in):: url
  integer, intent(in):: length
  character(len = *), intent(in), optional:: xtype
  character(len = *), intent(in), optional:: long_name
  logical, intent(in), optional:: overwrite
  logical, intent(out), optional:: err
  character(len = STRING):: fnam, vnam, new_url, data_class
  type(GD_NC_VARIABLE):: gdnc
  type(GD_MEM_VARIABLE):: mem
  integer :: stat, cause_i
  character(len = *),      parameter:: subname = "GTVarCreateD"
  character(len = *),      parameter:: version = &
    & '$Name:  $' // &
    & '$Id: gtvarcreated.f90,v 1.5 2009-05-25 09:55:58 morikawa Exp $'
continue
  call BeginSub(subname, 'url=<%c> length=%d', &
    & c1=trim(url), i=(/length/), version=version)
  stat = DC_NOERR
  cause_i = 0
  data_class = ''
  if (strHead(url, "memory:")) then
    call Create(mem, url, length, xtype, long_name, overwrite, err)
    call map_create(var, vtb_class_memory, mem%id, 1, (/length/), stat)
    if (stat /= DC_NOERR) then
      cause_i = 1
      goto 999
    end if
    call gtvar_dump(var)
    data_class = 'memory'
    goto 999
  endif
  ! URL の検査
  call UrlSplit(url, file=fnam, var=vnam)
  if (vnam == "") then
    call GtDataTmpNam(file=fnam, base="dim", result=new_url)
  else
    new_url = url
  endif
  ! gdnc 形式が選択される場合は
  call Create(var=gdnc, url=new_url, length=length, xtype=xtype, &
    & overwrite=overwrite, err=err)
  if (present(long_name)) then
    call put_attr(gdnc, 'long_name', long_name, err=err)
  endif
  call map_create(var, vtb_class_netcdf, gdnc%id, 1, (/length/), stat)
    if (stat /= DC_NOERR) then
      cause_i = 1
      goto 999
    end if
  call gtvar_dump(var)
  data_class = 'netcdf'
999 continue
  call StoreError(stat, subname, err, cause_i=cause_i)
  call EndSub(subname, 'class=%c mapid=%d', &
    & c1=trim(data_class), i=(/var%mapid/) )
end subroutine GTVarCreateD
