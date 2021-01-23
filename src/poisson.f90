!> \file  poisson.f90  Calculate Poisson statistics:  P = λ^k e^-λ / k!

!***********************************************************************************************************************************
!> \brief  Calculate Poisson statistics:  P = λ^k e^-λ / k!
!!
!! -AF, 2014-10-26

program poisson
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_statistics, only: poisson_prob, poisson_prob_cumul
  use ST_general, only: statTools_init
  
  implicit none
  real(double) :: lambda, pois, poisCumul,poisminCumul,  mean,var,stdev
  integer :: k
  
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  if(command_argument_count().ne.2) call syntax_quit('k λ', 0, 'Compute the probability of k events occurring in a fixed '// &
       'interval for a known average rate of λ and independently of the time since the last event: P = λ^k e^-λ / k!')
  
  call get_command_argument_i(1, k)
  call get_command_argument_d(2, lambda)
  
  pois = poisson_prob(k, lambda)
  poisCumul = poisson_prob_cumul(k, lambda)
  poisminCumul = 1.d0 - poisCumul + pois
  
  mean  = lambda
  var   = lambda
  stdev = sqrt(var)
  
  write(*,*)
  write(*,'(A,I15)')                            '  k:                ',k
  write(*,'(A,F15.6,ES15.6)')                   '  λ:                ',lambda,lambda
  write(*,*)
  write(*,'(A,F15.6,ES15.6, A,ES13.6, A,I0,A)')         '  poison:           ', pois,       pois,       ',   1 :', 1.d0/pois,     &
       '  (exactly ',k,')'
  write(*,'(A,F15.6,ES15.6, A,ES13.6, A,I0,A)') '  poison cumul:     ', poisCumul,    poisCumul,    ',   1 :', 1.d0/poisCumul,    &
       '  (',k,' or fewer)'
  write(*,'(A,F15.6,ES15.6, A,ES13.6, A,I0,A)') '  1 - poison cumul: ', poisminCumul, poisminCumul, ',   1 :', 1.d0/poisminCumul, &
       '  (',k,' or more)'
  
  write(*,*)
  write(*,'(A,F15.6,ES15.6)')                   '  mean:             ',mean,  mean
  write(*,'(A,F15.6,ES15.6)')                   '  variance:         ',var,   var
  write(*,'(A,F15.6,ES15.6)')                   '  stdev:            ',stdev, stdev
  write(*,*)
  
end program poisson
!***********************************************************************************************************************************

