!== dc_date_types#DC_DATETIME, dc_date_types#DC_DIFFTIME の印字
!
! Authors::   Yasuhiro MORIKAWA
! Version::   $Id: dcdatetimeputline.f90,v 1.2 2009-05-25 10:01:34 morikawa Exp $
! Tag Name::  $Name:  $
! Copyright:: Copyright (C) GFD Dennou Club, 2000-2005. All rights reserved.
! License::   See COPYRIGHT[link:../../COPYRIGHT]
!
! このファイルで提供される手続き群は dc_date モジュールにて提供されます。
!

subroutine DCDateTimePutLine( time, unit, indent )
  !
  ! 引数 *time* に設定されている情報を印字します. 
  ! デフォルトではメッセージは標準出力に出力されます. 
  ! *unit* に装置番号を指定することで, 出力先を変更することが可能です. 
  !
  ! Print information of *time*. 
  ! By default messages are output to standard output. 
  ! Unit number for output can be changed by *unit* argument. 
  !
  use dc_date_types, only: DC_DATETIME
  use dc_date_generic, only: toChar
  use dc_string, only: Printf
  use dc_trace, only: BeginSub, EndSub
  use dc_types, only: STDOUT, STRING
  use dc_scaledsec, only: DCScaledSecPutLine
  implicit none
  type(DC_DATETIME), intent(in) :: time
  integer, intent(in), optional :: unit
                              ! 出力先の装置番号. 
                              ! デフォルトの出力先は標準出力. 
                              !
                              ! Unit number for output. 
                              ! Default value is standard output. 
  character(*), intent(in), optional:: indent
                              ! 表示されるメッセージの字下げ. 
                              !
                              ! Indent of displayed messages. 

  integer :: out_unit
  integer:: indent_len
  character(STRING):: indent_str
  character(*), parameter :: subname = 'DCDateTimePutLine'
continue
  call BeginSub(subname)
  if (present(unit)) then
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

  call Printf(out_unit, &
    & indent_str(1:indent_len) // &
    & '#<DC_DATETIME:: @date=%c @caltype=%d @zone=%c', &
    & i=(/time % caltype/), c1=trim(toChar(time)), c2=trim(time % zone) )

  call Printf(out_unit, &
    & indent_str(1:indent_len) // &
    & '                @day=' )
  call DCScaledSecPutLine( time % day, unit = unit, &
    & indent = indent_str(1:indent_len) // &
    & '                     ' )

  call Printf(out_unit, &
    & indent_str(1:indent_len) // &
    & '                @sec=' )
  call DCScaledSecPutLine( time % sec, unit = unit, &
    & indent = indent_str(1:indent_len) // &
    & '                     ' )

  call Printf(out_unit, &
    & indent_str(1:indent_len) // '>' )

999 continue
  call EndSub(subname)
end subroutine DCDateTimePutLine

subroutine DCDiffTimePutLine( diff, unit, indent )
  !
  ! 引数 *diff* に設定されている情報を印字します. 
  ! デフォルトではメッセージは標準出力に出力されます. 
  ! *unit* に装置番号を指定することで, 出力先を変更することが可能です. 
  !
  ! Print information of *diff*. 
  ! By default messages are output to standard output. 
  ! Unit number for output can be changed by *unit* argument. 
  !
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: toChar
  use dc_string, only: Printf
  use dc_trace, only: BeginSub, EndSub
  use dc_types, only: STDOUT, STRING
  use dc_scaledsec, only: DCScaledSecPutLine
  implicit none
  type(DC_DIFFTIME), intent(in) :: diff
  integer, intent(in), optional :: unit
                              ! 出力先の装置番号. 
                              ! デフォルトの出力先は標準出力. 
                              !
                              ! Unit number for output. 
                              ! Default value is standard output. 
  character(*), intent(in), optional:: indent
                              ! 表示されるメッセージの字下げ. 
                              !
                              ! Indent of displayed messages. 

  integer :: out_unit
  integer:: indent_len
  character(STRING):: indent_str
  character(*), parameter :: subname = 'DCDiffTimePutLine'
continue
  call BeginSub(subname)
  if (present(unit)) then
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

  call Printf(out_unit, &
    & indent_str(1:indent_len) // &
    & '#<DC_DIFFTIME:: @diff=%c @nondim=%b', &
    & c1 = trim(toChar(diff)), l = (/ diff % nondim_flag /) )

  call Printf(out_unit, &
    & indent_str(1:indent_len) // &
    & '                @mon=' )
  call DCScaledSecPutLine( diff % mon, unit = unit, &
    & indent = indent_str(1:indent_len) // &
    & '                     ' )

  call Printf(out_unit, &
    & indent_str(1:indent_len) // &
    & '                @day=' )
  call DCScaledSecPutLine( diff % day, unit = unit, &
    & indent = indent_str(1:indent_len) // &
    & '                     ' )

  call Printf(out_unit, &
    & indent_str(1:indent_len) // &
    & '                @sec=' )
  call DCScaledSecPutLine( diff % sec, unit = unit, &
    & indent = indent_str(1:indent_len) // &
    & '                     ' )

  call Printf(out_unit, &
    & indent_str(1:indent_len) // '>' )

999 continue
  call EndSub(subname)
end subroutine DCDiffTimePutLine


!-----------------------------------------------
! 後方互換用
! For backward compatibility
subroutine DCDateTimePutLine_bc(time, unit)
  use dc_date_types, only: DC_DATETIME
  use dc_date_generic, only: DCDateTimePutLine
  type(DC_DATETIME), intent(in) :: time
  integer, intent(in), optional :: unit
continue
  call DCDateTimePutLine( time, unit )
end subroutine DCDateTimePutLine_bc

subroutine DCDiffTimePutLine_bc(diff, unit)
  use dc_date_types, only: DC_DIFFTIME
  use dc_date_generic, only: DCDiffTimePutLine
  type(DC_DIFFTIME), intent(in) :: diff
  integer, intent(in), optional :: unit
continue
  call DCDiffTimePutLine( diff, unit )
end subroutine DCDiffTimePutLine_bc
