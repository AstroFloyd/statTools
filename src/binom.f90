!> \file  binom.f90  Calculate binominal statistics:  P = n! / [k!(n-k)!] * p^k * (1-p)^(n-k) 
!!                   for k succesful trials out of n trials with probability p

!***********************************************************************************************************************************
!> \brief  Calculate binominal statistics:  P = n! / [k!(n-k)!] * p^k * (1-p)^(n-k)
!!         for k succesful trials out of n trials with probability p
!!
!! - AF, (<=?) 2011-04-27
!! - 2012-04-29: added case for k=0
!!

program binom
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_statistics, only: binom_prob, binom_cumul_prob
  use ST_general, only: statTools_init
  
  implicit none
  real(double) :: p, bin, binCumul,binminCumul,  mean,var,stdev
  integer :: n,k
  
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  if(command_argument_count().ne.3) call syntax_quit('n k p', 0, 'Compute the probability of k succesful trials out of n '// &
       'trials with probability p:  n! / [k!(n-k)!] * p^k * (1-p)^(n-k)')
  
  call get_command_argument_i(1,n)
  call get_command_argument_i(2,k)
  call get_command_argument_d(3,p)
  
  bin = binom_prob(n,k,p)               ! Binomial probability: EXACTLY k succesful trials
  binCumul = binom_cumul_prob(n,k,p)    ! Cumulative probability: k OR FEWER succesful trials
  binminCumul = 1.d0 - binCumul + bin   ! Cumulative probability: k OR MORE succesful trials
  
  mean = n*p
  var = n*p*(1.d0-p)
  stdev = sqrt(var)
  
  write(*,*)
  write(*,'(A,I15)')                            '  n:               ',n
  write(*,'(A,I15)')                            '  k:               ',k
  write(*,'(A,F15.6,ES15.6)')                   '  p:               ',p,p
  
  write(*,*)
  write(*,'(A,F15.6,ES15.6, A,ES13.6)')         '  binom:           ',bin,bin, ',   1 :',1.d0/bin
  write(*,'(A,F15.6,ES15.6, A,ES13.6, A,I0,A)') '  binom cumul:     ',binCumul,binCumul, ',   1 :',1.d0/binCumul, &
       '  (',k,' or fewer)'
  write(*,'(A,F15.6,ES15.6, A,ES13.6, A,I0,A)') '  1 - binom cumul: ',binminCumul,binminCumul, ',   1 :',1.d0/binminCumul, &
       '  (',k,' or more)'
  
  !write(*,'(A,F15.6,ES15.6)')'  1 - binom_Cumul: ',1.d0-binCumul,1.d0-binCumul  ! Not accurate because of round off
  
  write(*,*)
  write(*,'(A,F15.6,ES15.6)')                   '  mean:            ',mean,mean
  write(*,'(A,F15.6,ES15.6)')                   '  variance:        ',var,var
  write(*,'(A,F15.6,ES15.6)')                   '  stdev:           ',stdev,stdev
  write(*,*)
  
end program binom
!***********************************************************************************************************************************

