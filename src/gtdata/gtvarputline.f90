!
!= 変数の印字
!
! Authors::   Yasuhiro MORIKAWA, Eizi TOYODA
! Version::   $Id: gtvarputline.f90,v 1.1 2009-03-20 09:09:51 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! 以下のサブルーチン、関数は gtdata_generic から gtdata_generic#PutLine
! として提供されます。

subroutine GTVarPutLine( var, unit, indent, err )
  !
  ! 引数 *var* に設定されている情報を印字します. 
  ! デフォルトではメッセージは標準出力に出力されます. 
  ! *unit* に装置番号を指定することで, 出力先を変更することが可能です. 
  !
  ! Print information of *var*. 
  ! By default messages are output to standard output. 
  ! Unit number for output can be changed by *unit* argument. 
  !
  use dc_types, only: STRING, STDOUT
  use gtdata_types, only: GT_VARIABLE
  use dc_error, only: ErrorCode, StoreError, DC_NOERR, GT_ENOMEM
  use dc_string, only: toChar, Printf, PutLine
  use gtdata_generic, only: Get, Inquire
  use dc_trace, only: BeginSub, EndSub, DbgMessage
  implicit none
  type(GT_VARIABLE), intent(in):: var
  integer, intent(in), optional:: unit
                              ! 出力先の装置番号. 
                              ! デフォルトの出力先は標準出力. 
                              !
                              ! Unit number for output. 
                              ! Default value is standard output. 
  character(*), intent(in), optional:: indent
                              ! 表示されるメッセージの字下げ. 
                              !
                              ! Indent of displayed messages. 
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
  real, allocatable:: rvalue(:)
  integer:: siz, stat
!!$  integer:: i
  logical:: myerr
  integer:: out_unit
  integer:: indent_len
  character(STRING):: indent_str
  character(*), parameter:: subname = 'GTVarPutLine'
continue
  call BeginSub(subname, '%d', i=(/var % mapid/))
  stat = DC_NOERR
  !-----------------------------------------------------------------
  !  出力先装置番号と字下げの設定
  !  Configure output unit number and indents
  !-----------------------------------------------------------------
  if ( present(unit) ) then
    out_unit = unit
  else
    out_unit = STDOUT
  end if

  indent_len = 0
  indent_str = ''
  if ( present(indent) ) then
    if ( len(indent) /= 0 ) then
      indent_len = len(indent)
      indent_str(1:indent_len) = indent
    end if
  end if

  !-----------------------------------------------------------------
  !  初期設定されていない変数の印字
  !  Print uninitialized variables
  !-----------------------------------------------------------------
  if ( var % mapid < 0 ) then
    call Printf( out_unit, &
      & indent_str(1:indent_len) // &
      & '#<GT_VARIABLE:: @initialized=%y>', &
      & l = (/.false./) )
    goto 999
  end if

  !-----------------------------------------------------------------
  !  初期設定されている変数の印字
  !  Print initialized variables
  !-----------------------------------------------------------------
  call Inquire(var, size=siz)
  call DbgMessage('size = %d', i=(/siz/))
  stat = DC_NOERR
  allocate(rvalue(siz), stat=stat)
  if (stat /= DC_NOERR) then
    stat = GT_ENOMEM
    goto 999
  endif
  call Get(var, rvalue, size(rvalue), err=myerr)
  if (myerr) then
    stat = ErrorCode()
    if (stat /= DC_NOERR) then
      call Printf( out_unit, &
        & indent_str(1:indent_len) // &
        & '#<GT_VARIABLE:: @initialized=%y>', &
        & l = (/.false./) )
      stat = DC_NOERR
    end if
    goto 999
  endif
  call Printf( out_unit, &
    & indent_str(1:indent_len) // &
    & '#<GT_VARIABLE:: @initialized=%y', &
    & l = (/.true./) )

  call PutLine( rvalue, unit = out_unit, &
    & lbounds = lbound(rvalue), &
    & ubounds = ubound(rvalue), &
    & indent = indent_str(1:indent_len) // &
    & ' @value=' )

!!$  do, i = 1, size(rvalue)
!!$    call Printf(fmt='%r', r=(/rvalue(i)/))
!!$  end do

  call Printf( out_unit, &
    & indent_str(1:indent_len) // &
    & '>' )

  deallocate(rvalue, stat=stat)
  if (stat /= DC_NOERR) stat = GT_ENOMEM

999 continue
  call StoreError(stat, subname, err)
  call EndSub(subname, '%d stat=%d', i=(/var % mapid, stat/))
end subroutine GTVarPutLine
