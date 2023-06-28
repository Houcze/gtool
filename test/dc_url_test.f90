program dc_url_test
  use dc_types, only: STRING
  use dc_url, only: UrlSearchIORange
  use dc_trace, only: SetDebug
  use dc_test, only: AssertEqual
  implicit none
  character(STRING) :: char
continue

  call SetDebug

  !
  ! Test for "UrlSearchIORange"
  !

  char = UrlSearchIORange("time=1.0,x=345,lat=0.11,sigma=0", "lat")
  call AssertEqual('UrlSearchIORange Test 1', '0.11', char)

  char = UrlSearchIORange("time=1.0,x=345,lat=0.11,sigma=0", "lata")
  call AssertEqual('UrlSearchIORange Test 2', '', char)

  char = UrlSearchIORange("file@v,x=345,lat=0.11,sigma=0", "x")
  call AssertEqual('UrlSearchIORange Test 3', '345', char)

  char = UrlSearchIORange("file?v,z=220,lat=0.11,sigma=0", "z")
  call AssertEqual('UrlSearchIORange Test 4', '220', char)

end program dc_url_test
