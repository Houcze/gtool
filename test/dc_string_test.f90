program dc_string_test
  use dc_types, only: STRING, DP
  use dc_string, only: Split, StoA, CPrintf, StriEq, StrInclude, PutLine, &
    & Replace, RoundNum, toChar
  use dc_trace, only: SetDebug
  use dc_test, only: AssertEqual
  implicit none

  character(STRING), pointer:: carray(:) =>null()
  character(STRING):: char
  logical:: strieq_check
  real(DP):: dpreal(-5:-3, 3:5, 0:2)
  integer:: i, j, k
continue

  call SetDebug

  !
  ! Test for "Split"
  !
  call Split('time=0.0', carray, '=')
  call AssertEqual('Split Test 1', (/'time', '0.0 '/), carray)
  deallocate(carray)

  call Split('  time = 0.0  ', carray, '')
  call AssertEqual('Split Test 2', (/'    ', 'time', '=   ', '0.0 '/), carray)
  deallocate(carray)

  call Split('time=0.0 ||$ x=1.0 ||$ y=hogehoge ', carray, '||$')
  call AssertEqual('Split Test 3', &
    & (/'time=0.0   ', &
    &   ' x=1.0     ', &
    &   ' y=hogehoge'/), &
    & carray)
  deallocate(carray)

  call Split('  time = 0.0  ', carray, '', 2)
  call AssertEqual('Split Test 4', &
    & (/'          ', &
    &   'time = 0.0'/), &
    & carray)
  deallocate(carray)

  call Split('time=0.0,x=1.0,y=0.1,z=10,m=8,l=9 ', carray, ',', 3)
  call AssertEqual('Split Test 5', &
    & (/'time=0.0          ', &
    &   'x=1.0             ', &
    &   'y=0.1,z=10,m=8,l=9'/), &
    & carray)
  deallocate(carray)



  !
  ! Test for "StoA"
  !

  allocate(carray(3))
  carray = StoA('hogehoge', 'foo', 'hero')
  call AssertEqual('StoA Test', &
    & (/'hogehoge', &
    &   'foo     ', &
    &   'hero    '/), &
    & carray)
  deallocate(carray)

  char = CPrintf('%a, %a, %a, %a, %a, %a', &
    & ca=StoA("hogehoge", "foo", "hero", "uni"))
  call AssertEqual('CPrintf Test 1', 'hogehoge, foo, hero, uni, ,', char)

  char = CPrintf('%*a', &
    & ca=StoA("hogehoge", "foo", "hero", "uni"), n=(/4/))
  call AssertEqual('CPrintf Test 2', 'hogehoge, foo, hero, uni', char)

  char = CPrintf('%*d', i=(/1, 2, 4/), n=(/2/))
  call AssertEqual('CPrintf Test 3', '1, 2', char)

  char = CPrintf('%*r', r=(/(/1.0, 2.0, 4.0/)/), n=(/5/))
  call AssertEqual('CPrintf Test 4', '1., 2., 4.,', char)

  char = CPrintf('%*r %*a', r=(/1.0, 2.0, 4.0/), &
    & ca=StoA('hogehoge', 'foo', 'hero', 'uni'), n=(/2,3/))
  call AssertEqual('CPrintf Test 5', '1., 2. hogehoge, foo, hero', char)

  char = CPrintf('%d %5d %05d %4d %*02d', &
    & i=(/123, -987000, -360, 91, 1, 9/), &
    & n=(/2/))
  call AssertEqual('CPrintf Test 6', '123 -987000 -0360   91 01, 09', char)

  !
  ! Test for "StriEq"
  !
  strieq_check = StriEq('hogehoge', 'HogeHoge')
  call AssertEqual('StriEq Test 1', .true., strieq_check)

  strieq_check = StriEq('foo', 'HogeHoge')
  call AssertEqual('StriEq Test 2', .false., strieq_check)

  !
  ! Test for "StrInclude"
  !
  strieq_check = StrInclude( StoA('aaa', ' BBB', 'ccc'), 'bbb' )
  call AssertEqual('StrInclude Test 1', .false., strieq_check)

  strieq_check = StrInclude( StoA('aaa', ' BBB', 'ccc'), 'bbb', &
    &                        ignore_case = .true. )
  call AssertEqual('StrInclude Test 2', .true., strieq_check)

  strieq_check = StrInclude( StoA('aaa', 'BBB', 'ccc'), 'DDD' )
  call AssertEqual('StrInclude Test 3', .false., strieq_check)

  strieq_check = StrInclude( StoA('aaa', ' BBB', 'ccc'), 'bbb', &
    &                        ignore_space = .false. )
  call AssertEqual('StrInclude Test 4', .false., strieq_check)

  !
  ! Test for "PutLine"
  !
  call PutLine ( (/0.0/) )
  call PutLine ( (/1, 2, 3/) )
  call PutLine ( reshape((/-999.0, -45.874, -1.0e5, &
    &                     -2.7e3, -9.2e-1, -5.0e2/), (/2,3/)), &
    &            indent = '  ')

  do i = lbound(dpreal, 1), ubound(dpreal, 1)
    do j = lbound(dpreal, 2), ubound(dpreal, 2)
      do k = lbound(dpreal, 3), ubound(dpreal, 3)
        dpreal (i,j,k) = i * (-1.0) + j * 3.0 + k * (-2.5)
      end do
    end do
  end do

  call PutLine ( dpreal, indent = '  NOBND  :: ' )
  call PutLine ( dpreal, indent = '  SPECBND:: ', &
    &            lbounds = lbound(dpreal), ubounds = ubound(dpreal) )


  !
  ! Test for "Replace"
  !
  char = Replace( string = 'hogehoge,herohero', from = 'hoge', to = 'foo' )
  call AssertEqual('Replace Test 1', 'foohoge,herohero', char)

  char = Replace( string = 'h geh ge,her her ', from = ' ', to = '', &
    &             recursive = .true. )
  call AssertEqual('Replace Test 2', 'hgehge,herher', char)

  char = Replace( string = 'Data1,Data2,Data3', from = ',', to = ',dir/', &
    &             recursive = .true. )
  call AssertEqual('Replace Test 3', 'Data1,dir/Data2,dir/Data3', char)

  char = Replace( string = ':::::', from = ':', to = ':,:', &
    &             recursive = .true. )
  call AssertEqual('Replace Test 4', ':,::,::,::,::,:', char)

  char = Replace( string = 'Data', from = '', to = 'PR_' )
  call AssertEqual('Replace Test 5', 'PR_Data', char)

  char = Replace( string = 'Data', from = ' ', to = '_SUF' )
  call AssertEqual('Replace Test 6', 'Data_SUF', char)


  !
  ! Test for "RoundNum"
  !
  char = RoundNum('0.30000001')
  call AssertEqual('RoundNum Test 1', '0.3', char)

  char = RoundNum('0.69999999')
  call AssertEqual('RoundNum Test 2', '0.7', char)

  char = RoundNum('0.89999998')
  call AssertEqual('RoundNum Test 3', '0.9', char)

  char = RoundNum('0.99999998')
  call AssertEqual('RoundNum Test 4', '1.', char)

  char = RoundNum('13.99999998')
  call AssertEqual('RoundNum Test 5', '14.', char)

  char = RoundNum('0.')
  call AssertEqual('RoundNum Test 6', '0.', char)

  char = RoundNum('1.9')
  call AssertEqual('RoundNum Test 7', '1.9', char)

  char = RoundNum('100000002')
  call AssertEqual('RoundNum Test 8', '100000002', char)

  char = RoundNum('1999999998')
  call AssertEqual('RoundNum Test 9', '1999999998', char)

  char = RoundNum('0.1E+20')
  call AssertEqual('RoundNum Test 10', '0.1E+20', char)

  char = RoundNum('13.99999998E-20')
  call AssertEqual('RoundNum Test 11', '14.E-20', char)

  char = RoundNum('2.20000000000000017763568')
  call AssertEqual('RoundNum Test 12', '2.2', char)

  !
  ! Test for "toChar"
  !
  ! 環境依存になってまともにテストできないため止める. 
  !
!  char = toChar(1.E+19)
!  call AssertEqual('toChar Test 1', '0.1E+20', char)
!
!  char = toChar(1.D-30)
!  call AssertEqual('toChar Test 2', '0.1E-29', char)

end program dc_string_test
