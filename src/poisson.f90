!> \file  poisson.f90  Calculate Poisson statistics:  P = λ^k e^-λ / k!

!***********************************************************************************************************************************
!> \brief  Calculate Poisson statistics:  P = λ^k e^-λ / k!
!!
!! -AF, 2014-10-26

program poisson
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_statistics, only: poisson_prob
  use ST_general, only: statTools_init
  
  implicit none
  real(double) :: lambda, pois, poissum,poisminsum,  mean,var,stdev, pp
  integer :: k,i
  
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  if(command_argument_count().ne.2) call syntax_quit('k λ', 0, 'Compute the probability of k events occurring in a fixed '// &
       'interval for a known average rate of λ and independently of the time since the last event: P = λ^k e^-λ / k!')
  
  call get_command_argument_i(1, k)
  call get_command_argument_d(2, lambda)
  
  poissum = 0.d0
  poisminsum = 0.d0
  pois = poisson_prob(k, lambda)
  if(k.eq.0) then
     poissum = pois
     poisminsum = 1.d0 - pois
  else
     do i=1,k
        poissum = poissum + poisson_prob(i, lambda)
     end do
     do i=k,huge(k)
        pp = poisson_prob(i, lambda)
        poisminsum = poisminsum + pp
        if(pp.lt.1.d-7) exit  ! Quoted accuracy: 10^-6
     end do
  end if
  
  mean  = lambda
  var   = lambda
  stdev = sqrt(var)
  
  write(*,*)
  write(*,'(A,I15)')                    '  k:              ',k
  write(*,'(A,F15.6,ES15.6)')           '  λ:              ',lambda,lambda
  write(*,*)
  write(*,'(A,F15.6,ES15.6, A,ES13.6)') '  poison:         ', pois,       pois,       ',   1 :', 1.d0/pois
  write(*,'(A,F15.6,ES15.6, A,ES13.6)') '  poison_sum:     ', poissum,    poissum,    ',   1 :', 1.d0/poissum
  write(*,'(A,F15.6,ES15.6, A,ES13.6)') '  1 - poison_sum: ', poisminsum, poisminsum, ',   1 :', 1.d0/poisminsum
  
  write(*,*)
  write(*,'(A,F15.6,ES15.6)')           '  mean:           ',mean,  mean
  write(*,'(A,F15.6,ES15.6)')           '  variance:       ',var,   var
  write(*,'(A,F15.6,ES15.6)')           '  stdev:          ',stdev, stdev
  write(*,*)
  
end program poisson
!***********************************************************************************************************************************

