!> \file dice.f90  Throw n-sided dice

!***********************************************************************************************************************************
!> \brief  Throw n-sided dice (several times)
!!
!! - AF, 2015-12-20, adapted from die
!!

program dice
  use SUFR_command_line, only: get_command_argument_i
  use SUFR_system, only: syntax_quit
  use SUFR_random_numbers, only: get_ran_seed, ran_unif
  use ST_general, only: statTools_init
  
  implicit none
  integer :: Narg, Ndice, Nsides, Nthrows, seed, it,id, throw,tot
  
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  Narg = command_argument_count()
  if(Narg.lt.1 .or. Narg.gt.3)  call syntax_quit('<# dice> [<# sides> [<# throws>]]', 0, 'Throw k n-sided dice m times.  '// &
       'Default is one throw with six-sided dice.')
  
  Nthrows = 1
  Nsides = 6
  call get_command_argument_i(1, Ndice)
  if(Narg.ge.2) call get_command_argument_i(2, Nsides)
  if(Narg.ge.3) call get_command_argument_i(3, Nthrows)
  
  seed = get_ran_seed(0)  ! 0 - completely random
  write(*,*)
  do it = 1,Nthrows
     write(*,'(A,I0,A)', advance='no') '  Throw ',it,': '
     tot = 0
     do id = 1,Ndice
        throw = ceiling(ran_unif(seed)*Nsides)
        write(*,'(1x,I0)', advance='no') throw
        tot = tot + throw
     end do
     write(*,'(A,I0)') ' = ', tot
  end do
  write(*,*)
  
end program dice
!***********************************************************************************************************************************

