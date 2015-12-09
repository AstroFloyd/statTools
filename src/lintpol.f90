!> \file lintpol.f90  Do linear inter- or extrapolation using two datapoints
!!
!! - AF, <= 2012-02-24
!! - 2012-03-07: merged with lintpol2
!!

!***********************************************************************************************************************************
program lintpol
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use ST_general, only: statTools_init
  
  implicit none
  real(double) :: x1,x2,x,y1,y2,y,a,b
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  if(command_argument_count().ne.5)  call syntax_quit('<x1> <x2>  <y1> <y2>  <x>', 0, &
       'Inter- or extrapolate using two data points (x1,y1) and (x2,y2) to find the value y that corresponds to x')
  
  call get_command_argument_d(1,x1)
  call get_command_argument_d(2,x2)
  call get_command_argument_d(3,y1)
  call get_command_argument_d(4,y2)
  call get_command_argument_d(5,x)
  
  a = (y2-y1)/(x2-x1)
  b = y1 - a*x1
  y = a*x + b
  
  write(*,'(1p, /,2x,2(A,G15.7))')   'y = a*x + b,  a =',a,'  b =',b
  write(*,'(1p, /,2x,2(A,G15.7),/)') 'x =',x,'    ->    y =',y
  
end program lintpol
!***********************************************************************************************************************************
