program main
  use dc_types, only: DP
  use polymorphism

  integer:: aryi0
  real:: aryr5(2,2,2,2,2)
  real(DP):: aryd7(2,2,2,2,2,2,2)

  aryi0 = 10
  aryr5 = 100
  aryd7 = 1000

  call Polymor(aryi0)
  call Polymor(aryr5)
  call Polymor(aryd7)

end program main
