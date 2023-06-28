program dc_clock_test
  use dc_types, only: STRING, DP, STDOUT
  use dc_clock, only : CLOCK, DCClockCreate, DCClockClose, DCClockStart, &
    & DCClockStop, DCClockPutLine, DCClockResult, &
    & operator(+), operator(-), DCClockSetName, DCClockGet, DCClockEvalSec, &
    & DCClockToChar, DCClockPredict
  use dc_args, only: ARGS, DCArgsOpen, DCArgsDebug, DCArgsHelp, &
    & DCArgsStrict, DCArgsClose
  use dc_string, only: Printf
  implicit none
  type(CLOCK) :: clock1, clock2, clock3, clock_total
  type(ARGS) :: arg

  integer :: i, j
  real(DP) :: a, b, sec
  integer, parameter :: tmp_file_unit = 20
  integer, parameter :: loop_num = 3
  character(STRING) :: char

continue

  call DCArgsOpen(arg)
  call DCArgsDebug(arg) ; call DCArgsHelp(arg) ; call DCArgsStrict(arg)
  call DCArgsClose(arg)

  call DCClockCreate(clock1, 'Exp.')
  call DCClockCreate(clock2, '4op.')
  open(tmp_file_unit, file='dc_clock_test.tmp', status='replace')
  do i = 1, loop_num
    call DCClockStart(clock1)
    a = 2.0d0
    do j = 1, 1000000
      a = (a**2)**0.3 + 1.0d0
    enddo
    call DCClockStop(clock1)
    call DCClockStart(clock2)
    b = 1.0d0
    do j = 1, 1000000
      b = b / 3.0d0 * 2.0d0 + 1.0d0 - 1.0d-1
    enddo
    call DCClockStop(clock2)
    call DCClockPredict(clock1 + clock2, real(i)/real(loop_num))
  enddo
  call DCClockPutLine(clock1)
  call DCClockPutLine(clock2, indent=' ')

  call DCClockGet(clock1, sec)
  call Printf(STDOUT, 'clock1 sec=%f', d=(/sec/))
  sec = DCClockEvalSec(clock2)
  call Printf(STDOUT, 'clock2 sec=%f', d=(/sec/))

  char = DCClockToChar(clock1 + clock2)
  call Printf(STDOUT, 'clock1 + clock2 = %c', c1=trim(char))

  write(tmp_file_unit,*) a
  write(tmp_file_unit,*) b
  call DCClockResult((/clock1, clock2/), total_name='Total name test 1')
  call DCClockResult((/clock1/), total_auto=.true.)
  clock_total = clock1 + clock2
  clock3 = clock1 - clock2
  call DCClockResult((/clock1, clock2, clock3/), clk_total=clock_total)

  call DCClockClose(clock1)
  call DCClockClose(clock2)
  close(tmp_file_unit)

end program dc_clock_test
