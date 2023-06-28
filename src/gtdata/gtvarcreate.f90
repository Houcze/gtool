!
!= 従属変数の作成
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvarcreate.f90,v 1.4 2009-05-25 09:55:58 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン, 関数は gtdata_generic から提供されます。
!

subroutine GTVarCreate(var, url, dims, xtype, long_name, overwrite, err)
  !
  !== 従属変数の作成
  !
  ! 場所 *url* に次元 *dims* を持った変数つまり GT_VARIABLE 型
  ! の実体を作成し、それを第 1 引数 *var* にセットします。
  ! Open されたものと同様、第1引数 *var* は後で必ず
  ! Close されなければなりません。
  !
  ! 型 *xtype* を省略すると "+float+" と
  ! みなされます。既存変数があるとき失敗しますが、
  ! overwrite == .true. であれば上書きして続行します。
  ! (まだ *overwrite* の動作は保障されていません)。
  ! dims の省略は 0 次元変数の設定を意味します。
  !
  ! 作成の際にエラーが生じた場合、メッセージを出力してプログラムは
  ! 強制終了します。*err* を与えてある場合にはこの引数に .true.
  ! が返り、プログラムは終了しません。
  !
  !
  use gtdata_types, only: GT_VARIABLE
  use gtdata_internal_map, only: var_class, vtb_class_netcdf, vtb_class_memory, &
    & map_create, gtvar_dump
  use gtdata_netcdf_generic, only: create, put_attr, inquire
  use gtdata_netcdf_types, only: GD_NC_VARIABLE
  use dc_string, only: strhead
  use dc_error, only: StoreError, DC_NOERR, GT_EFAKE
  use dc_types, only: TOKEN
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(out):: var
  character(len = *), intent(in):: url
  type(GT_VARIABLE), intent(in), optional:: dims(:)
  character(len = *), intent(in), optional:: xtype
  character(len = *), intent(in), optional:: long_name
  logical, intent(in), optional:: overwrite
  logical, intent(out), optional:: err
  type(GD_NC_VARIABLE), allocatable:: gdnc_dims(:)
  type(GD_NC_VARIABLE):: gdnc
  integer, allocatable:: allcount(:)
  integer:: i, ndims, stat, cause_i
  character(len = TOKEN):: myxtype
  character(len = *),      parameter:: subname = "GTVarCreate"
  character(len = *),      parameter:: version = &
    & '$Name:  $' // &
    & '$Id: gtvarcreate.f90,v 1.4 2009-05-25 09:55:58 morikawa Exp $'
continue
  stat = DC_NOERR
  ndims = 0
  cause_i = 0
  if (present(dims)) ndims = size(dims)
  call BeginSub(subname, 'url=%c ndims=%d', c1=trim(url), i=(/ndims/), &
    & version=version)
  if (strhead(url, "memory:")) then
    ! メモリ変数の作成
    stat = GT_EFAKE
    goto 999
  else
    ! gdnc 変数の作成
    if (present(err)) err = .false.
    if (present(xtype)) then
      myxtype = xtype
    else
      myxtype = "float"
    endif
    if (present(dims)) then
      allocate(gdnc_dims(ndims), allcount(ndims))
      do, i = 1, ndims
        call var_class(dims(i), cid=gdnc_dims(i)%id)
        call DbgMessage('dim=%d mapid=%d -> cid=%d', i=(/i, dims(i)%mapid, gdnc_dims(i)%id/))
        call inquire(gdnc_dims(i), dimlen=allcount(i))
      enddo
      call create(var=gdnc, url=url, dims=gdnc_dims, xtype=myxtype, &
        & overwrite=overwrite, err=err)
    else
      ndims = 0
      allocate(gdnc_dims(1), allcount(1)) ! dummy
      call create(var=gdnc, url=url, dims=gdnc_dims(1:0), &
        & xtype=myxtype, overwrite=overwrite, err=err)
    endif
    call map_create(var, vtb_class_netcdf, gdnc%id, ndims, allcount, stat)
    if (stat /= DC_NOERR) then
      cause_i = ndims
      goto 999
    end if
    deallocate(gdnc_dims, allcount)
    if (present(long_name)) then
      call put_attr(gdnc, 'long_name', long_name, err=err)
    endif
  endif
  call gtvar_dump(var)
  call DbgMessage('var%%mapid=%d', i=(/var % mapid/))
999 continue
  call StoreError(stat, subname, err, cause_i=cause_i)
  call EndSub(subname)
end subroutine GTVarCreate
