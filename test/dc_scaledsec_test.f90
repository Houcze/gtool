program dc_scaledsec_test
  use dc_message, only: MessageNotify
  use dc_types, only: STRING, DP, STDOUT
  use dc_test, only: AssertEqual
  use dc_string, only: StoA, Printf
  use dc_scaledsec, only: DC_SCALED_SEC, DCScaledSecPutLine, assignment(=), &
    & operator(==), operator(>), operator(<), operator(>=), operator(<=), &
    & operator(+), operator(-), operator(*), operator(/), mod, modulo, &
    & abs, int, sign, floor, ceiling
  use dc_trace, only: SetDebug

  implicit none
  type(DC_SCALED_SEC):: sclsec1, sclsec2, sclsec3, sclsec4, sclsec5
  real:: secr
  real(DP):: secd
  integer:: i, j, isec
  integer:: iary(1:100)
continue

  call SetDebug

  ! DC_SCALED_SEC 変数の設定
  ! Assignment of "DC_SCALED_SEC" variable
  !
  sclsec1 = 1.23456789012344e+3_DP
  !call DCScaledSecPutLine(sclsec1)
  call AssertEqual('assignment(=) test 1', &
    & (/0, 568, 234, 1,0,0,0,0,0,0,0/), sclsec1 % sec_ary )
!    & (/567890, 1234, 0,0,0,0/), sclsec1 % sec_ary )

  sclsec1 = 12e-5
  !call DCScaledSecPutLine(sclsec1)
  call AssertEqual('assignment(=) test 2', &
!    & (/120,0,0,0,0,0/), sclsec1 % sec_ary )
    & (/120,0,0,0,0,0,0,0,0,0,0/), sclsec1 % sec_ary )

  sclsec1 = 1.20e+6
  !call DCScaledSecPutLine(sclsec1)
  call AssertEqual('assignment(=) test 3', &
!    & (/0,200000,1,0,0,0/), sclsec1 % sec_ary )
    & (/0,0,0,200,1,0,0,0,0,0,0/), sclsec1 % sec_ary )

  sclsec1 = 5.25678991d+26
  !call DCScaledSecPutLine(sclsec1)
  call AssertEqual('assignment(=) test 4', &
!    & (/0,0,0,0,0,525678/), sclsec1 % sec_ary )
    & (/0,0,0,0,0,0,0,0,0,678,525/), sclsec1 % sec_ary )

  sclsec1 = - 0.0
  call AssertEqual('assignment(=) test 5-1', &
!    & (/0,0,0,0,0,0/), sclsec1 % sec_ary )
    & (/0,0,0,0,0,0,0,0,0,0,0/), sclsec1 % sec_ary )
  call AssertEqual('assignment(=) test 5-2', &
    & .false., sclsec1 % flag_negative )

  sclsec1 = - 0.05
  call AssertEqual('assignment(=) test 6-1', &
!    & (/50000,0,0,0,0,0/), sclsec1 % sec_ary )
    & (/0,50,0,0,0,0,0,0,0,0,0/), sclsec1 % sec_ary )
  call AssertEqual('assignment(=) test 6-2', &
    & .true., sclsec1 % flag_negative )

  sclsec1 = 1001.001_DP
  call AssertEqual('assignment(=) test 7-1', &
    & (/0,1,1,1,0,0,0,0,0,0,0/), sclsec1 % sec_ary )

  ! エラーチェック
  ! Check Error 
  !
!  sclsec1 = 5.25678991d+27

  ! 比較演算子チェック
  ! Check operators for comparison
  !
  sclsec1 = 0
  sclsec2 = 0.005
  sclsec3 = 0.005
  call AssertEqual('operator(==) test 1', &
    & .true.,  sclsec1 == 0 )
  call AssertEqual('operator(==) test 1', &
    & .false., sclsec1 == sclsec2 )
  call AssertEqual('operator(>) test 1', &
    & .false., sclsec1 > sclsec2 )
  call AssertEqual('operator(>) test 2', &
    & .false., sclsec1 > 1 )
  call AssertEqual('operator(>) test 3', &
    & .false., -3 > sclsec2 )
  call AssertEqual('operator(<) test 1', &
    & .true.,  sclsec1 < sclsec2 )
  call AssertEqual('operator(<) test 2', &
    & .true.,  sclsec1 < 10 )
  call AssertEqual('operator(<) test 3', &
    & .false.,  10 < sclsec2 )
  call AssertEqual('operator(>=) test 1', &
    & .true.,  sclsec2 >= sclsec3 )
  call AssertEqual('operator(>=) test 2', &
    & .false., sclsec2 >= sclsec3 * 2 )
  call AssertEqual('operator(>=) test 3', &
    & .true., sclsec2 * 2 >= sclsec3 )
  call AssertEqual('operator(>=) test 4', &
    & .true., 0 >= sclsec1 )
  call AssertEqual('operator(>=) test 5', &
    & .false., sclsec2 >= 1 )
  call AssertEqual('operator(<=) test 1', &
    & .true.,  sclsec2 <= sclsec3 )
  call AssertEqual('operator(<=) test 2', &
    & .true.,  sclsec2 <= sclsec3 * 2 )
  call AssertEqual('operator(<=) test 3', &
    & .false., sclsec2 * 2 <= sclsec3 )
  call AssertEqual('operator(<=) test 4', &
    & .true.,  0 <= sclsec1 )
  call AssertEqual('operator(<=) test 5', &
    & .false.,  sclsec2 <= -1 )

  ! 加算チェック
  ! Check addition
  !
  sclsec2 =      101
  sclsec3 = 12345000
  sclsec4 = sclsec2 + sclsec3
  isec = sclsec4
  call AssertEqual('operator(+) test 1-1', &
    & 12345101, isec )
  call AssertEqual('operator(+) test 1-2', &
    & .false., sclsec4 % flag_negative )

  sclsec4 = sclsec2 + 12345000
  isec = sclsec4
  call AssertEqual('operator(+) test 1-3', &
    & 12345101, isec )
  call AssertEqual('operator(+) test 1-4', &
    & .false., sclsec4 % flag_negative )

  sclsec4 = 101 + sclsec3
  isec = sclsec4
  call AssertEqual('operator(+) test 1-5', &
    & 12345101, isec )
  call AssertEqual('operator(+) test 1-6', &
    & .false., sclsec4 % flag_negative )

  sclsec2 = 0
  sclsec3 = 0.0003
  do i = 1, 10000
    sclsec2 = sclsec2 + sclsec3
!    if ( mod(i, 100) == 0 ) call DCScaledSecPutLine(sclsec2)
  enddo
  secr = sclsec2
  call AssertEqual('operator(+) test 2-2', &
!    & (/0,3,0,0,0,0/), sclsec2 % sec_ary )
    & (/0,0,3,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('operator(+) test 2-2', &
    & .false., sclsec2 % flag_negative )

  ! 減算チェック
  ! Check subtraction
  !
  sclsec1 = - 0.05
  sclsec1 = - sclsec1
  call AssertEqual('operator(-) test 1-1', &
!    & (/50000,0,0,0,0,0/), sclsec1 % sec_ary )
    & (/0,50,0,0,0,0,0,0,0,0,0/), sclsec1 % sec_ary )
  call AssertEqual('operator(-) test 1-2', &
    & .false., sclsec1 % flag_negative )

  sclsec2 =   345101
  sclsec3 = 12345000
  sclsec4 = sclsec3 - sclsec2
  isec = sclsec4
  call AssertEqual('operator(-) test 2-1', &
    & 11999899, isec )
  call AssertEqual('operator(-) test 2-2', &
    & .false., sclsec4 % flag_negative )

  sclsec4 = sclsec3 - 345101
  isec = sclsec4
  call AssertEqual('operator(-) test 2-3', &
    & 11999899, isec )
  call AssertEqual('operator(-) test 2-4', &
    & .false., sclsec4 % flag_negative )

  sclsec4 = 12345000 - sclsec2
  isec = sclsec4
  call AssertEqual('operator(-) test 2-5', &
    & 11999899, isec )
  call AssertEqual('operator(-) test 2-6', &
    & .false., sclsec4 % flag_negative )

  sclsec2 = 345.101_DP
  sclsec3 = 123.45000_DP
  sclsec4 = sclsec2 - sclsec3
  call AssertEqual('operator(-) test 3-1', &
!    & (/651000,221,0,0,0,0/), sclsec4 % sec_ary )
    & (/0,651,221,0,0,0,0,0,0,0,0/), sclsec4 % sec_ary )
  call AssertEqual('operator(-) test 3-2', &
    & .false., sclsec4 % flag_negative )

  sclsec4 = - sclsec2 + sclsec3
  call AssertEqual('operator(-) test 4-1', &
!    & (/651000,221,0,0,0,0/), sclsec4 % sec_ary )
    & (/0,651,221,0,0,0,0,0,0,0,0/), sclsec4 % sec_ary )
  call AssertEqual('operator(-) test 4-2', &
    & .true., sclsec4 % flag_negative )

  sclsec3 = - sclsec3
  sclsec4 = - sclsec2 - sclsec3
  call AssertEqual('operator(-) test 5-1', &
!    & (/651000,221,0,0,0,0/), sclsec4 % sec_ary )
    & (/0,651,221,0,0,0,0,0,0,0,0/), sclsec4 % sec_ary )
  call AssertEqual('operator(-) test 5-2', &
    & .true., sclsec4 % flag_negative )

  sclsec2 = - 345.101_DP
  sclsec3 = - 123.45000_DP
  sclsec4 = sclsec2 + sclsec3
  call AssertEqual('operator(-) test 6-1', &
!    & (/551000,468,0,0,0,0/), sclsec4 % sec_ary )
    & (/0,551,468,0,0,0,0,0,0,0,0/), sclsec4 % sec_ary )
  call AssertEqual('operator(-) test 6-2', &
    & .true., sclsec4 % flag_negative )

  ! 乗算チェック
  ! Check multiplication
  !
  sclsec1 = 0.00003
  sclsec2 = sclsec1 * 0
  call AssertEqual('operator(*) test 1-1', &
!    & (/0,0,0,0,0,0/), sclsec2 % sec_ary )
    & (/0,0,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('operator(*) test 1-2', &
    & .false., sclsec2 % flag_negative )

  sclsec2 = sclsec1 * 40000
  call AssertEqual('operator(*) test 2-1', &
!    & (/200000,1,0,0,0,0/), sclsec2 % sec_ary )
    & (/0,200,1,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('operator(*) test 2-2', &
    & .false., sclsec2 % flag_negative )

  sclsec2 = sclsec1
  do i = 1, 6
    sclsec2 = sclsec2 * 100000
  end do
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(*) test 3-1', &
!    & (/0,0,0,0,0,30/), sclsec2 % sec_ary )
    & (/0,0,0,0,0,0,0,0,0,0,30/), sclsec2 % sec_ary )
  call AssertEqual('operator(*) test 3-2', &
    & .false., sclsec2 % flag_negative )

  sclsec2 = sclsec1 * ( - 1234567 )
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(*) test 4-1', &
!    & (/37010,37,0,0,0,0/), sclsec2 % sec_ary )
    & (/10,37,37,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('operator(*) test 4-2', &
    & .true., sclsec2 % flag_negative )

  sclsec1 = 864000000
  sclsec2 = 1.0e+15_DP
  sclsec3 = sclsec1 * sclsec2
  !call DCScaledSecPutLine(sclsec3)
  call AssertEqual('operator(*) test 5-1', &
    & (/0,0,0,0,0,0,0,0,0,864,0/), sclsec3 % sec_ary )

  sclsec3 = sclsec2 * sclsec1
  call AssertEqual('operator(*) test 5-2', &
    & (/0,0,0,0,0,0,0,0,0,864,0/), sclsec3 % sec_ary )

  sclsec1 = 1.0e+15_DP
  sclsec2 = 123.0_DP * sclsec1
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(*) test 6-1', &
    & (/0,0,0,0,0,0,0,123,0,0,0/), sclsec2 % sec_ary )

  sclsec2 = sclsec1 * 123.0
  call AssertEqual('operator(*) test 6-2', &
    & (/0,0,0,0,0,0,0,123,0,0,0/), sclsec2 % sec_ary )

  sclsec1 = 71.2
  sclsec2 = 86400
  sclsec3 = sclsec1 * sclsec2
  !call DCScaledSecPutLine(sclsec3)
  isec = sclsec3
  call AssertEqual('operator(*) test 7-1', &
    & 6151680, isec )
  call AssertEqual('operator(*) test 7-2', &
    & (/0,0,680,151,6,0,0,0,0,0,0/), sclsec3 % sec_ary )

  sclsec1 = 0.25_DP
  sclsec2 = sclsec1 * 86400.0_DP
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('operator(*) test 8-1', &
    & 21600, isec )
  call AssertEqual('operator(*) test 8-2', &
    & (/0,0,600,21,0,0,0,0,0,0,0/), sclsec2 % sec_ary )

  sclsec1 = 1001.001_DP
  sclsec2 = sclsec1 * 1001.001_DP
  !call DCScaledSecPutLine(sclsec1)
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(*) test 9-1', &
    & (/1,2,3,2,1,0,0,0,0,0,0/), sclsec2 % sec_ary )

  sclsec1 = 1001.001_DP
  sclsec1 = sclsec1 + 1.0e+6_DP
  sclsec1 = sclsec1 + 1.0e+9_DP
  !call DCScaledSecPutLine(sclsec1)
  sclsec2 = sclsec1 * sclsec1
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(*) test 10-1', &
    & (/1,2,3,4,5,4,3,2,1,0,0/), sclsec2 % sec_ary )

  sclsec1 = 1001.001_DP
  sclsec1 = sclsec1 + 1.0e-6_DP
  sclsec1 = sclsec1 + 1.0e+6_DP
  sclsec1 = sclsec1 + 1.0e+9_DP
  !call DCScaledSecPutLine(sclsec1)
  sclsec2 = sclsec1 * sclsec1
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(*) test 11-1', &
    & (/3,4,5,6,5,4,3,2,1,0,0/), sclsec2 % sec_ary )


  ! 除算チェック
  ! Check division
  !
  sclsec1 = 0.00003
  sclsec2 = sclsec1 / 3.0
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(/) test 1-1', &
!    & (/10,0,0,0,0,0/), sclsec2 % sec_ary )
    & (/10,0,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('operator(/) test 1-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = 1.0e+13_DP
  sclsec2 = sclsec1 / 4.0_DP
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(/) test 2-1', &
!    & (/0,0,500000,2,0,0/), sclsec2 % sec_ary )
    & (/0,0,0,0,0,500,2,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('operator(/) test 2-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = 685689e+10_DP
  !call DCScaledSecPutLine(sclsec1)
  sclsec2 = sclsec1 / 101
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(/) test 3-1', &
!    & (/0,0,890000,67,0,0/), sclsec2 % sec_ary )
    & (/0,0,0,0,0,890,67,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('operator(/) test 3-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = 1005003002
  !call DCScaledSecPutLine(sclsec1)
  sclsec2 = sclsec1 / 10107
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('operator(/) test 4-1', &
!    & (/99436,0,0,0,0/), sclsec2 % sec_ary(0:4) )
    & (/436,99,0,0,0,0,0,0,0/), sclsec2 % sec_ary(0:8) )
  call AssertEqual('operator(/) test 4-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = 123e+13_DP
  do i = 1, 3
    !call DCScaledSecPutLine(sclsec1)
    sclsec1 = sclsec1 * 90
  end do
  !call DCScaledSecPutLine(sclsec2)
  sclsec2 = sclsec1 / (- 1.0e+11_DP)
  call AssertEqual('operator(/) test 5-1', &
!    & (/700000,8966,0,0,0/), sclsec2 % sec_ary(0:4) )
    & (/0,700,966,8,0,0,0,0,0/), sclsec2 % sec_ary(0:8) )
  call AssertEqual('operator(/) test 5-2', &
    & .true., sclsec2 % flag_negative )


  ! mod による余り算出チェック
  ! Check calculation of remainder by "mod"
  !
  sclsec1 = 12.345_DP
  sclsec2 = mod( sclsec1, 2.001_DP )
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('mod test 1-1', &
!    & (/339000,0,0,0,0,0/), sclsec2 % sec_ary )
    & (/0,339,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('mod test 1-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = 0.00017_DP
  sclsec2 = mod( sclsec1, 0.00003_DP )
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('mod test 2-1', &
!    & (/20,0,0,0,0,0/), sclsec2 % sec_ary )
    & (/20,0,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('mod test 2-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = 123e+15_DP
  do i = 1, 3
    !call DCScaledSecPutLine(sclsec1)
    sclsec1 = sclsec1 * 90
  end do
  !call DCScaledSecPutLine(sclsec1)
  sclsec2 = mod( sclsec1, - 1.0e+11_DP )
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('mod test 3-1', &
    & (/0,0,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('mod test 3-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = -23
  sclsec2 = mod( sclsec1, 4 )
  isec = sclsec2
  call AssertEqual('mod test 4-1', -3, isec )

  sclsec1 = 23
  sclsec2 = mod( sclsec1, -4 )
  isec = sclsec2
  call AssertEqual('mod test 4-2', 3, isec )

  sclsec1 = -23
  sclsec2 = mod( sclsec1, -4 )
  isec = sclsec2
  call AssertEqual('mod test 4-3', -3, isec )

  sclsec1 = 13
  sclsec2 = mod( sclsec1, 12 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('mod test 5-1', 1, isec )

  sclsec1 = 9
  sclsec1 = sclsec1 - 3
  call AssertEqual('mod test 6-1', &
    & (/0,0,6,0,0,0,0,0,0,0,0/), sclsec1 % sec_ary )
  sclsec2 = mod( sclsec1, 12 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('mod test 6-2', 6, isec )

  sclsec1 = 9
  sclsec2 = mod( sclsec1 - 3, 12 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('mod test 6-3', 6, isec )

  sclsec1 = 66591
  sclsec2 = mod( sclsec1, 86400 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('mod test 7-1', 66591, isec )

  sclsec1 = 86400.0
  sclsec2 = mod( sclsec1, 86400 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('mod test 8-1', 0, isec )

  sclsec1 = 3780.0
  sclsec2 = mod( sclsec1, 3600 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('mod test 9-1', 180, isec )

  ! modulo による剰余算出チェック
  ! Check calculation of remainder by "modulo"
  !
  sclsec1 = 12.345_DP
  sclsec2 = modulo( sclsec1, 2.001_DP )
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('modulo test 1-1', &
!    & (/339000,0,0,0,0,0/), sclsec2 % sec_ary )
    & (/0,339,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('modulo test 1-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = 0.00017_DP
  sclsec2 = modulo( sclsec1, -0.00003_DP )
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('modulo test 2-1', &
!    & (/20,0,0,0,0,0/), sclsec2 % sec_ary )
    & (/10,0,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('modulo test 2-2', &
    & .true., sclsec2 % flag_negative )

  sclsec1 = 123e+15_DP
  do i = 1, 3
    !call DCScaledSecPutLine(sclsec1)
    sclsec1 = sclsec1 * 90
  end do
  !call DCScaledSecPutLine(sclsec1)
  sclsec2 = modulo( sclsec1, - 1.0e+11_DP )
  !call DCScaledSecPutLine(sclsec2)
  call AssertEqual('modulo test 3-1', &
    & (/0,0,0,0,0,0,0,0,0,0,0/), sclsec2 % sec_ary )
  call AssertEqual('modulo test 3-2', &
    & .false., sclsec2 % flag_negative )

  sclsec1 = -23
  sclsec2 = modulo( sclsec1, 4 )
  isec = sclsec2
  call AssertEqual('modulo test 4-1', 1, isec )

  sclsec1 = 23
  sclsec2 = modulo( sclsec1, -4 )
  isec = sclsec2
  call AssertEqual('modulo test 4-2', -1, isec )

  sclsec1 = -23
  sclsec2 = modulo( sclsec1, -4 )
  isec = sclsec2
  call AssertEqual('modulo test 4-3', -3, isec )

  sclsec1 = 13
  sclsec2 = modulo( sclsec1, 12 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('modulo test 5-1', 1, isec )

  sclsec1 = 9
  sclsec1 = sclsec1 - 3
  call AssertEqual('modulo test 6-1', &
    & (/0,0,6,0,0,0,0,0,0,0,0/), sclsec1 % sec_ary )
  sclsec2 = modulo( sclsec1, 12 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('modulo test 6-2', 6, isec )

  sclsec1 = 9
  sclsec2 = modulo( sclsec1 - 3, 12 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('modulo test 6-3', 6, isec )

  sclsec1 = 66591
  sclsec2 = modulo( sclsec1, 86400 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('modulo test 7-1', 66591, isec )

  sclsec1 = 86400.0
  sclsec2 = modulo( sclsec1, 86400 )
  !call DCScaledSecPutLine(sclsec2)
  isec = sclsec2
  call AssertEqual('modulo test 8-1', 0, isec )


  ! 絶対値の算出チェック
  ! Check calculation of absolute values
  !
  sclsec1 = -123
  isec = abs( sclsec1 )
  call AssertEqual('abs test 1-1', 123, isec )

  sclsec1 = 123
  isec = abs( sclsec1 )
  call AssertEqual('abs test 1-2', 123, isec )


  ! int による整数値の算出チェック
  ! Check calculation of integer by "int"
  !
  sclsec1 = 0.001002_DP
  isec = int( sclsec1 )
  call AssertEqual('int test 1-1', 0, isec )

  sclsec1 = 100.23_DP
  isec = int( sclsec1 )
  call AssertEqual('int test 1-2', 100, isec )

  sclsec1 = - 120034
  isec = int( sclsec1 )
  call AssertEqual('int test 1-3', - 120034, isec )


  ! sign 関数チェック
  ! Check calculation of "sign" function
  !
  sclsec1 = 100
  isec = sign( sclsec1, -1 )
  call AssertEqual('sign test 1-1', -100, isec )

  sclsec1 = - 2100
  isec = sign( sclsec1, 1.0 )
  call AssertEqual('sign test 1-2', 2100, isec )

  sclsec1 = - 120034
  isec = sign( sclsec1, 0 )
  call AssertEqual('sign test 1-3', 120034, isec )


  ! floor による整数値の算出チェック
  ! Check calculation of integer by "floor"
  !
  sclsec1 = 0.001002_DP
  isec = floor( sclsec1 )
  call AssertEqual('floor test 1-1', 0, isec )

  sclsec1 = - 0.001002_DP
  isec = floor( sclsec1 )
  call AssertEqual('floor test 1-2', -1, isec )

  sclsec1 = 100.23_DP
  isec = floor( sclsec1 )
  call AssertEqual('floor test 2-1', 100, isec )

  sclsec1 = - 100.23_DP
  isec = floor( sclsec1 )
  call AssertEqual('floor test 2-2', -101, isec )

  sclsec1 = 120034
  isec = floor( sclsec1 )
  call AssertEqual('floor test 3-1', 120034, isec )

  sclsec1 = - 120034
  isec = floor( sclsec1 )
  call AssertEqual('floor test 3-2', - 120034, isec )


  ! ceiling による整数値の算出チェック
  ! Check calculation of integer by "ceiling"
  !
  sclsec1 = 0.001002_DP
  isec = ceiling( sclsec1 )
  call AssertEqual('ceiling test 1-1', 1, isec )

  sclsec1 = - 0.001002_DP
  isec = ceiling( sclsec1 )
  call AssertEqual('ceiling test 1-2', 0, isec )

  sclsec1 = 100.23_DP
  isec = ceiling( sclsec1 )
  call AssertEqual('ceiling test 2-1', 101, isec )

  sclsec1 = - 100.23_DP
  isec = ceiling( sclsec1 )
  call AssertEqual('ceiling test 2-2', -100, isec )

  sclsec1 = 120034
  isec = ceiling( sclsec1 )
  call AssertEqual('ceiling test 3-1', 120034, isec )

  sclsec1 = - 120034
  isec = ceiling( sclsec1 )
  call AssertEqual('ceiling test 3-2', - 120034, isec )


  ! 総合チェック
  ! Check various usage
  !
  sclsec1 = 0
  sclsec2 = 0.005
  sclsec3 = 0.105
  sclsec4 = 1.155
  i = 0
  j = 0
  do while ( sclsec1 <= sclsec4 )
    i = i + 1
    sclsec1 = sclsec1 + sclsec2
    !write(*,*) i
    !call DCScaledSecPutLine(sclsec1)
    if ( mod( sclsec1, sclsec3 ) == 0 ) then
      j = j + 1
      iary(j) = i
    end if
    if ( i > 1000 ) then
      call MessageNotify( 'E', '', 'integrative test 1 FAILURE' )
    end if
  end do
  call AssertEqual('integrative test 1-1', &
    & (/21, 42, 63, 84, 105, 126, 147, 168, 189, 210, 231/), &
    & iary(1:j) )
  call AssertEqual('integrative test 1-2', &
    & 232, i )


  sclsec1 = 86400.0e+5
  do i = 1, 3
    sclsec1 = sclsec1 * 100000
  end do
  sclsec2 = 0.03
  sclsec3 = 6
  sclsec4 = 30
  sclsec5 = sclsec1 + sclsec4
  i = 0
  j = 0
  !call DCScaledSecPutLine(sclsec1)
  !call DCScaledSecPutLine(sclsec3)
  !call DCScaledSecPutLine(sclsec5)
  do while ( sclsec1 <= sclsec5 )
    i = i + 1
    sclsec1 = sclsec1 + sclsec2
    !write(*,*) i
    !call DCScaledSecPutLine(sclsec1)
    if ( mod( sclsec1, sclsec3 ) == 0 ) then
      j = j + 1
      iary(j) = i
    end if
    if ( i > 5000 ) then
      call MessageNotify( 'E', '', 'integrative test 2 FAILURE' )
    end if
  end do
  !write(*,*) iary(1:j)
  !write(*,*) i

  call AssertEqual('integrative test 2-1', &
    & (/200, 400, 600, 800, 1000/), &
    & iary(1:j) )
  call AssertEqual('integrative test 2-2', &
    & 1001, i )

end program dc_scaledsec_test
