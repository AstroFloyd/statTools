!> \file  gauss.f90  Calculate Gaussian statistics:  P = exp(-x^2/2)/sqrt(2*pi) dx

!***********************************************************************************************************************************
!> \brief  Calculate Poisson statistics:  P = exp(-x^2/2)/sqrt(2*pi) dx
!!
!! - AF, 2016-12-21

program gauss
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi2
  use SUFR_system, only: syntax_quit, swapdbl
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_statistics, only: poisson_prob
  use ST_general, only: statTools_init
  
  implicit none
  integer, parameter :: nIter=1000000  ! 1e6
  real(double) :: mu,sig, x1,x2, xx,dx, integral
  integer :: iter
  
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  ! Default mean and st.dev:
  mu = 0.d0
  sig = 1.d0
  
  ! Get command-line arguments:
  select case(command_argument_count())
  case(2)
     write(*,'(A)') '  Two arguments:  assuming µ=0 and σ=1'
     call get_command_argument_d(1, x1)
     call get_command_argument_d(2, x2)
  case(4)
     call get_command_argument_d(1, mu)
     call get_command_argument_d(2, sig)
     call get_command_argument_d(3, x1)
     call get_command_argument_d(4, x2)
  case default
     call syntax_quit('<x1> <x2> (µ=0, σ=1)  or  <µ> <σ> <x1> <x2>', 0, 'Compute the probability of a value between '// &
          'x1 and x2 for a Gaussian distribution with mean µ and standard deviation σ (or µ=0, σ=1 if not specified): '// &
          'P = Int_x1^x2 exp(-(x-µ)^2/(2σ^2)) / (sqrt(2 pi)σ) dx ')
  end select
  
  if(x1.gt.x2) call swapdbl(x1,x2)
  
  
  integral = 0.d0
  dx = (x2-x1)/dble(nIter)
  do iter=1,nIter
     xx = x1 + (dble(iter)-0.5d0)/dble(nIter) * (x2-x1)
     integral = integral + exp(-(xx-mu)**2/(2*sig**2)) / (sqrt(pi2)*sig) * dx  ! precomputing constants speeds things up by ~2%
  end do
  
  !print*,'  P = ', integral, integral-0.68268949213708585d0  ! Check accuracy: ~1e-14 for 1^6 iterations
  
  write(*,*)
  write(*,'(A,F25.16,ES25.16)')                   '  µ:   ', mu, mu
  write(*,'(A,F25.16,ES25.16)')                   '  σ:   ', sig, sig
  write(*,'(A,F25.16,ES25.16)')                   '  x1:  ', x1, x1
  write(*,'(A,F25.16,ES25.16)')                   '  x2:  ', x2, x2
  write(*,'(A,F25.16,ES25.16)')                   '  dx:  ', dx, dx
  write(*,*)
  write(*,'(A,F25.16,ES25.16, A,ES13.6)')         '  P:   ', integral,       integral,       ',   1 :', 1.d0/integral
  write(*,*)
  
end program gauss
!***********************************************************************************************************************************

