!> \file die.f90  Throw an n-sided die

!***********************************************************************************************************************************
!> \brief  Throw an n-sided die

program die
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_command_line, only: get_command_argument_i
  use SUFR_system, only: syntax_quit
  use SUFR_random_numbers, only: get_ran_seed, ran_unif
  implicit none
  integer :: Narg, Nsides, Nthrows, seed, it
  
  call set_SUFR_constants()  ! Define constants for libSUFR
  
  Narg = command_argument_count()
  if(Narg.lt.1 .or. Narg.gt.2)  call syntax_quit('<# sides> [<# throws>]', 0, 'Throw an n-sided die.  Default is one throw.')
  
  Nthrows = 1
  call get_command_argument_i(1,Nsides)
  if(Narg.ge.2) call get_command_argument_i(2,Nthrows)
  
  seed = get_ran_seed(0)  ! 0 - completely random
  write(*,*)
  do it = 1,Nthrows
     write(*,'(A,I0,A,I0)') ' Throw ',it,': ', ceiling(ran_unif(seed)*Nsides)
  end do
  write(*,*)
  
end program die
!***********************************************************************************************************************************

