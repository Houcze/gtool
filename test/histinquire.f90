!== Sample program for gtool_history/gtool5.
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: histinquire.f90,v 1.1 2008-09-23 09:56:38 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../COPYRIGHT]
!
! Test Program for "Inquire", in gtool_history

program histinquire
  use dc_types, only: STRING
  use dc_trace, only: SetDebug
  use gtool_history, only: GT_HISTORY, GT_HISTORY_AXIS, GT_HISTORY_VARINFO, &
    &                    HistoryCreate, Inquire, HistoryAddVariable, &
    &                    HistoryClose, HistoryInquire
  use dc_string,only: StoA, Printf, JoinChar, toChar
  use dc_message, only: MessageNotify
  use dc_test, only: AssertEqual
  implicit none
  integer:: length
  integer, pointer:: dimsizes(:)
  character(STRING), pointer:: dim_names(:), dim_units(:)
  character(STRING) :: xtype, units, longname, name
  character(STRING), pointer :: dims(:) => null()
  logical :: err
  type(GT_HISTORY) :: gthist
  type(GT_HISTORY_AXIS), pointer :: axes(:) => null()
  type(GT_HISTORY_VARINFO), pointer :: varinfo(:) => null()
continue

  !-------------------------------------------------------------------
  !  デバッグモード設定
  !  Debug mode setting
  !-------------------------------------------------------------------
  call SetDebug

  !-------------------------------------------------------------------
  !  ファイル出力
  !  Output file
  !-------------------------------------------------------------------
  call HistoryCreate(file = 'xhistinquire1.nc', &
    & title = 'Inquire in gtool_history test 1',      &
    & source = 'gtool5/Fortran library test kit', &
    & institution = 'GFD Dennou Club',              &
    & dims = StoA('x', 'time'),                  &
    & dimsizes = (/3, 0/),                          &
    & longnames = StoA('eastward length', 'time'), &
    & units = StoA('m', 'sec.'),                   &
    & origin = 0.0, interval = 2.5,                   &
    & xtypes = StoA('double', 'real'), &
    & history = gthist)

  !-------------------------------------------------------------------
  !  HistoryInquire Test
  !-------------------------------------------------------------------
  call HistoryInquire( &
    & history = gthist, &
    & dims = dim_names, dimsizes = dimsizes, units = dim_units )

  call AssertEqual('HistoryInquire Test 1 (dims)', &
    & answer = StoA('x', 'time'), check = dim_names )
  call AssertEqual('HistoryInquire Test 1 (dimsizes)', &
    & answer = (/3, 0/), check = dimsizes )
  call AssertEqual('HistoryInquire Test 1 (units)', &
    & answer = StoA('m', 'sec.'), check = dim_units )

  !-------------------------------------------------------------------
  !  HistoryAxisInquire Test
  !-------------------------------------------------------------------
  call Inquire(gthist, axes=axes)

  call Inquire(axes(1), name, length, longname, units, xtype)
  call AssertEqual('HistoryAxisInquire Test 1 (name)', &
    & answer='x', check=name)
  call AssertEqual('HistoryAxisInquire Test 1 (length)', &
    & answer=3, check=length)
  call AssertEqual('HistoryAxisInquire Test 1 (longname)', &
    & answer='eastward length', check=longname)
  call AssertEqual('HistoryAxisInquire Test 1 (units)', &
    & answer='m', check=units)
  call AssertEqual('HistoryAxisInquire Test 1 (xtype)', &
    & answer='double', check=xtype)

  call Inquire(axes(2), name, length, longname, units, xtype)
  call AssertEqual('HistoryAxisInquire Test 2 (name)', &
    & answer='time', check=name)
  call AssertEqual('HistoryAxisInquire Test 2 (length)', &
    & answer=0, check=length)
  call AssertEqual('HistoryAxisInquire Test 2 (longname)', &
    & answer='time', check=longname)
  call AssertEqual('HistoryAxisInquire Test 2 (units)', &
    & answer='sec.', check=units)
  call AssertEqual('HistoryAxisInquire Test 2 (xtype)', &
    & answer='float', check=xtype)

  !-------------------------------------------------------------------
  !  HistoryVarinfoInquire Test
  !-------------------------------------------------------------------
  call Inquire(gthist, err=err, varinfo=varinfo)
  call AssertEqual('HistoryVarinfoInquire Test 1', answer=.true., check=err)
  if (associated(varinfo)) deallocate(varinfo)

  call HistoryAddVariable('u', dims=StoA('x', 'time'), &
    & longname='any quantity', units='m/s', history=gthist)
  call HistoryAddVariable('u_ave', dims=StoA('time'), &
    & longname='any average', units='m/s', history=gthist)

  call Inquire(gthist, err=err, varinfo=varinfo)
  call AssertEqual('HistoryVarinfoInquire Test 2', answer=.false., check=err)

  call Inquire(varinfo(1), name, dims, longname, units, xtype)
  call AssertEqual('HistoryVarinfoInquire Test 3 (name)', &
    & answer='u', check=name)
  call AssertEqual('HistoryVarinfoInquire Test 3 (dims)', &
    & answer=StoA('x', 'time'), check=dims)
  call AssertEqual('HistoryVarinfoInquire Test 3 (longname)', &
    & answer='any quantity', check=longname)
  call AssertEqual('HistoryVarinfoInquire Test 3 (units)', &
    & answer='m/s', check=units)
  call AssertEqual('HistoryVarinfoInquire Test 3 (xtype)', &
    & answer='float', check=xtype)

  call Inquire(varinfo(2), name, dims, longname, units, xtype)
  call AssertEqual('HistoryVarinfoInquire Test 4 (name)', &
    & answer='u_ave', check=name)
  call AssertEqual('HistoryVarinfoInquire Test 4 (dims)', &
    & answer=StoA('time'), check=dims)
  call AssertEqual('HistoryVarinfoInquire Test 4 (longname)', &
    & answer='any average', check=longname)
  call AssertEqual('HistoryVarinfoInquire Test 4 (units)', &
    & answer='m/s', check=units)
  call AssertEqual('HistoryVarinfoInquire Test 4 (xtype)', &
    & answer='float', check=xtype)

  call HistoryClose(gthist)

  deallocate(dims)
  deallocate(axes)
  deallocate(varinfo)

end program histinquire
