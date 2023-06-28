program dc_args_test
  use dc_types
  use dc_string, only: StoA, JoinChar
  use dc_args, only: ARGS, DCArgsOpen, DCArgsClose, DCArgsOption, &
    & DCArgsPutLine, DCArgsDebug, DCArgsHelp, DCArgsStrict, &
    & DCArgsGet, DCArgsHelpMsg, DCArgsNumber
  use dc_test, only: AssertEqual
  implicit none
  type(ARGS) :: arg
  logical :: OPT_size
  logical :: OPT_namelist
  character(STRING) :: VAL_namelist, char
  character(TOKEN), pointer :: argv(:) => null()
continue

  call DCArgsOpen( arg = arg )   ! (out)
  call DCArgsHelpMsg( arg = arg, &              ! (inout)
    & category = 'Title', &                     ! (in)
    & msg = 'dcargs $Revision: 1.1 $ ' // &
    &       ':: Test program of dc_args' )      ! (in)
  call DCArgsHelpMsg( arg = arg, &              ! (inout)
    & category = 'Usage', &                     ! (in)
    & msg = 'dcargs [Options] arg1, arg2, ...') ! (in)
  call DCArgsOption( arg = arg, &           ! (inout)
    & options = StoA('-s', 'size'), &       ! (in)
    & flag = OPT_size, &                    ! (out)
    & help = "Return number of arguments")  ! (in)
  call DCArgsOption( arg = arg, &           ! (inout)
    & options = StoA('N', '--namelist'), &  ! (in)
    & flag = OPT_namelist, &                ! (out)
    & value = VAL_namelist, &               ! (out)
    & help = "Namelist filename")           ! (in)
  call DCArgsHelpMsg( arg = arg, &          ! (inout)
    & category = 'DESCRIPTION', &           ! (in)
    & msg = '(1) Define type "HASH". ' // &
    &       '(2) Open the variable. ' // &
    &       '(3) set HelpMsg. ' // &
    &       '(4) set Options. ' // &
    &       '(5) call Debug. ' // &
    &       '(6) call Help. ' // &
    &       '(7) call Strict.')             ! (in)
  call DCArgsHelpMsg( arg = arg, &                         ! (inout)
    & category = 'Copyright', &                            ! (in)
    & msg = 'Copyright (C) ' // &
    &       'GFD Dennou Club, 2008. All rights reserved.') ! (in)

  call DCArgsDebug( arg = arg )  ! (inout)
  call DCArgsHelp( arg = arg )   ! (inout)
  call DCArgsStrict( arg = arg ) ! (inout)
  call DCArgsGet( arg = arg, &   ! (inout)
    & argv = argv )              ! (out)

  call AssertEqual('Number of arguments test', 3, DCArgsNumber(arg))

  call AssertEqual('Short option test 1', .true., OPT_size)

  call AssertEqual('Long option test 1', .true., OPT_namelist)
  call AssertEqual('Long option test 2', 'test.nml', VAL_namelist)

  char = trim(JoinChar(argv))

  call AssertEqual('Strings of arguments test', 'arg1, arg2, arg3', char)

  deallocate(argv)
  call DCArgsClose( arg ) ! (inout)

end program dc_args_test
