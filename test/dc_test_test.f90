program dc_test_test
  use dc_types, only: STRING, DP
  use dc_test, only: AssertEqual, AssertGreaterThan, AssertLessThan
  implicit none
  character(32):: str1
  character(STRING):: str2
  real:: r1(2)
  integer:: int1
  real:: numr1(2)
  real(DP):: numd1(2,3)
  real(DP):: numd2(2,3)
  logical:: y_n
continue

  str1 = 'foo'
  r1 = (/ 1.0_DP, 2.0_DP /)
  call AssertEqual( message = 'String test', answer = 'foo', check = str1 )
  call AssertEqual( message = 'Float test', &
    & answer = (/1.0e0, 2.0e0/), check = r1 )


  str2 = "foo"
  call AssertEqual( 'Character test', answer = 'foo', check = str2 )
  int1 = 1
  call AssertEqual( 'Integer test', answer = 1, check = int1 )
  numr1(:) = (/ 0.001235423, 0.248271 /)
  call AssertGreaterThan( 'Float test 1', &
    & answer = (/ 0.00061771142, 0.1241354 /), check = numr1 / 2.0 )
  call AssertLessThan( 'Float test 2', &
    & answer = (/ 0.00061771158, 0.1241358 /), check = numr1 / 2.0 )
  y_n = .true.
  call AssertEqual( 'Logical test', answer = .true., check = y_n )

  numd1 = reshape( (/ -19.432_DP, 75.3_DP, 3.183_DP, &
    &                  0.023_DP,  -0.9_DP, 328.2_DP /), &
    &              (/ 2,3 /) )
  call AssertGreaterThan( 'Double precision test 1', &
    & answer = reshape( (/ -38.8639_DP, 150.5999_DP, 6.365999_DP, &
    &                     0.0459999_DP,  -1.7999_DP, 656.3999_DP /), &
    &                   (/ 2,3 /) ), &
    & check = numd1*2.0_DP )
  call AssertLessThan( 'Double precision test 2', &
    & answer = reshape( (/ -38.86401_DP, 150.60001_DP,  6.3660001_DP, &
    &                     0.04600001_DP, -1.8000001_DP,     656.6_DP /), &
    &                   (/ 2,3 /) ), &
    & check = numd1*2.0_DP, negative_support=.true. )

  call AssertEqual( 'Double precision test 3', &
    & answer = numd1, &
    & check = ( numd1 / 3.0_DP ) * 3.0_DP, &
    & significant_digits = 10, ignore_digits = -10 )

  call AssertEqual( 'Double precision test 4', &
    & answer = 0.9_DP, &
    & check = ( 0.9_DP / 3.0_DP ) * 3.0_DP, &
    & significant_digits = 10, ignore_digits = -15 )

  call AssertEqual( 'Double precision test 5', &
    & answer = 19.432_DP, &
    & check = ( 19.432_DP / 3.0_DP ) * 3.0_DP, &
    & significant_digits = 10, ignore_digits = -10 )

  call AssertEqual( 'Double precision test 6', &
    & answer = - 19.432_DP, &
    & check = ( - 19.432_DP / 3.0_DP ) * 3.0_DP, &
    & significant_digits = 10, ignore_digits = -10 )

  numd2 = reshape( (/  19.4e+7_DP,     75.3_DP, 3.18e-7_DP, &
    &                   0.023e-7_DP, 0.9e+7_DP,   328.2_DP /), &
    &              (/ 2,3 /) )

  call AssertEqual( 'Double precision test 7', &
    & answer = numd2, &
    & check = ( ( ( numd2 + 0.008_DP - 0.008_DP ) / 1.5_DP ) * 3.0_DP ) / 2.0_DP, &
    & significant_digits = 10, ignore_digits = -15 )

end program dc_test_test
