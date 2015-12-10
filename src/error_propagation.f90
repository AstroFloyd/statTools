!> \file  error_propagation.f90  Compute the error when adding, subtracting, multiplying or dividing two quantities with symmetric 
!!                               errors

!***********************************************************************************************************************************
!> \brief  Compute the error when adding, subtracting, multiplying or dividing two quantities with symmetric errors
!!
!! - AF, 2014-11-04

program error_propagation
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_statistics, only: poisson_prob
  use ST_general, only: statTools_init
  
  implicit none
  real(double) :: var1,dvar1,var2,dvar2, var,dvar
  character :: fmt*(64)
  
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  if(command_argument_count().ne.4)  call syntax_quit('x dx  y dy', 0, 'Compute the error when adding, subtracting, multiplying'// &
       ' or dividing two quantities with symmetric errors')
  
  call get_command_argument_d(1, var1)
  call get_command_argument_d(2, dvar1)
  call get_command_argument_d(3, var2)
  call get_command_argument_d(4, dvar2)
  
  write(*,'(/,2x,A)') 'Error propagation for four operators:'
  
  fmt = '(/,2x,A15,2x,2(A,F15.6),2(A,ES15.6),A)'  ! Format string for the output lines
  
  ! Addition:
  var = var1 + var2
  dvar = sqrt(dvar1**2 + dvar2**2)
  write(*,trim(fmt)) 'Addition:','x +- dx  +  y +- dy  =  ',var,'   +-',dvar,'      =   ',var,'   +-',dvar
  
  ! Subtraction:
  var = var1 - var2
  write(*,trim(fmt)) 'Subtraction:','x +- dx  -  y +- dy  =  ',var,'   +-',dvar,'      =   ',var,'   +-',dvar
  
  ! Multiplication:
  var = var1 * var2
  dvar = sqrt((dvar1/var1)**2 + (dvar2/var2)**2) * var
  write(*,trim(fmt)) 'Multiplication:','x +- dx  *  y +- dy  =  ',var,'   +-',dvar,'      =   ',var,'   +-',dvar
  
  ! Division:
  var = var1 / var2
  dvar = sqrt((dvar1/var1)**2 + (dvar2/var2)**2) * var
  write(*,trim(fmt)) 'Division:','x +- dx  /  y +- dy  =  ',var,'   +-',dvar,'      =   ',var,'   +-',dvar
  
  write(*,*)
end program error_propagation
!***********************************************************************************************************************************

