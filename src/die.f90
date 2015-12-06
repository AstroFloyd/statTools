!> \file  die.f90:  Throw a die

!***********************************************************************************************************************************
!> \brief  Throw a die

program die
  use SUFR_command_line
  use SUFR_random_numbers, only: get_ran_seed, ran_unif
  implicit none
  integer :: nsides, nthrows, seed, it
  
  nthrows = 1
  
  select case(command_argument_count())
  case(1)
     call get_command_argument_i(1,nsides)
  case(2)
     call get_command_argument_i(1,nsides)
     call get_command_argument_i(2,nthrows)
  case default
     write(0,'(/,A,/)')'  syntax:  die <# sides> [<# throws>]   Default is to trow the die once'
     stop
  end select
  
  seed = get_ran_seed(0)  ! 0 - completely random
  write(*,*)
  do it = 1,nthrows
     write(6,'(A,I0,A,I0)') ' Throw ',it,': ', ceiling(ran_unif(seed)*nsides)
  end do
  write(*,*)
  
end program die
!***********************************************************************************************************************************

