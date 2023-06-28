program dc_iounit_test
  use dc_types, only: STRING, TOKEN
  use dc_test, only: AssertEqual
  use dc_iounit, only: FileOpen
  use dc_args, only: ARGS, DCArgsOpen, DCArgsDebug, DCArgsHelp, &
    & DCArgsStrict, DCArgsClose
  implicit none
  type(ARGS):: arg
  character(*), parameter:: test_dir = 'tmp_dc_iounit_test/'
  integer:: unit00
  logical:: err
  character(STRING):: char00, char01, char02, char03, char04, char05
  integer:: iostat
  character(TOKEN):: pos
continue

  call DCArgsOpen(arg)
  call DCArgsDebug(arg) ; call DCArgsHelp(arg) ; call DCArgsStrict(arg)
  call DCArgsClose(arg)

  !---------------------------------------------------------
  !  Open for reading (r) test
  !---------------------------------------------------------
  call FileOpen(unit00, file = test_dir // 'test00.txt', mode = 'r', err = err)
  call AssertEqual('Open for reading (r) test 1', .true., err)


  !---------------------------------------------------------
  !  Open for writing (w) test
  !---------------------------------------------------------
  call FileOpen(unit00, file = test_dir // 'test00.txt', mode = 'w' )
  write(unit00, *) '01: This is a file for input/output test.'
  write(unit00, *) '02: End of file.                         '
  close(unit00)

  call FileOpen(unit00, file = test_dir // 'test00.txt', mode = 'r' )
  read(unit00, fmt='(A)', iostat=iostat) char01
  read(unit00, fmt='(A)', iostat=iostat) char02
  read(unit00, fmt='(A)', iostat=iostat) char03
  inquire(unit00, position=pos)
  close(unit00)

  call AssertEqual('Open for writing (w) test 1', &
    & .true., trim(pos) == 'APPEND' .or. iostat == -1)

  call AssertEqual('Open for writing (w) test 2', &
    & '01: This is a file for input/output test.', trim(adjustl(char01)))
  call AssertEqual('Open for writing (w) test 3', &
    & '02: End of file.                         ', trim(adjustl(char02)))

  char00 = ''
  char01 = ''
  char02 = ''
  char03 = ''
  char04 = ''
  char05 = ''

  !---------------------------------------------------------
  !  Open for overwriting (rw) test
  !---------------------------------------------------------
  call FileOpen(unit00, file = test_dir // 'test00.txt', mode = 'rw' )
  rewind(unit00)
  read(unit00, fmt='(A)', iostat=iostat) char01
  inquire(unit00, position=pos)
  close(unit00)

  call AssertEqual('Open for overwriting (rw) test 1', &
    & .true., trim(pos) == 'APPEND' .or. iostat == -1)

  !---------------------------------------------------------
  !  Open for append (a, ra) test
  !---------------------------------------------------------
  call FileOpen(unit00, file = test_dir // 'test00.txt', mode = 'w' )
  write(unit00, *) '01: This is a file for input/output test.'
  write(unit00, *) '02: End of file.                         '
  close(unit00)

  call FileOpen(unit00, file = test_dir // 'test00.txt', mode = 'a' )
  write(unit00, *) '03: Append and write test 1.             '
  rewind(unit00)
  read(unit00, fmt='(A)', iostat=iostat) char00
  call AssertEqual('Open for append (a) test 1', &
    & .false., iostat == 0)
  close(unit00)

  call FileOpen(unit00, file = test_dir // 'test00.txt', mode = 'ra' )
  write(unit00, *) '04: Append and write test 2.             '
  rewind(unit00)
  read(unit00, fmt='(A)', iostat=iostat) char01
  read(unit00, fmt='(A)', iostat=iostat) char02
  read(unit00, fmt='(A)', iostat=iostat) char03
  read(unit00, fmt='(A)', iostat=iostat) char04
  read(unit00, fmt='(A)', iostat=iostat) char05
  inquire(unit00, position=pos)
  close(unit00)

  call AssertEqual('Open for append (ra) test 1', &
    & .true., trim(pos) == 'APPEND' .or. iostat == -1)

  call AssertEqual('Open for append (ra) test 2', &
    & '01: This is a file for input/output test.', trim(adjustl(char01)))
  call AssertEqual('Open for append (ra) test 3', &
    & '02: End of file.                         ', trim(adjustl(char02)))
  call AssertEqual('Open for append (ra) test 4', &
    & '03: Append and write test 1.             ', trim(adjustl(char03)))
  call AssertEqual('Open for append (ra) test 5', &
    & '04: Append and write test 2.             ', trim(adjustl(char04)))

  char00 = ''
  char01 = ''
  char02 = ''
  char03 = ''
  char04 = ''
  char05 = ''

end program dc_iounit_test
