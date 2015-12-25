!> \file  asymmetric_errors.f90  Propagation of asymmetric errors;  see example in Sect.2/Fig.1 of arXiv:physics/0403086
!!
!! AF, 2014-11-22
!!
!! \see http://arxiv.org/abs/physics/0403086

!***********************************************************************************************************************************
program asymmetric_errors
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_numerics, only: deq0
  use SUFR_text, only: d2s
  use SUFR_interpolate, only: linear_interpolation
  use SUFR_command_line, only: get_command_argument_d
  use ST_general, only: statTools_init
  
  implicit none
  integer, parameter :: pln=2*500  ! MUST be even!!! 200: 0.2s (min: 0.08s), error: sigma: 3.2e-4;  500: 1.6s, error: sigma: 5e-5
  integer :: iy, jx,jy, verbosity
  real(double) :: xx1(pln),Px1(pln), xx2(pln),Px2(pln),  xmin,xmax,dx,xmin1,xmax1, yy(pln),fy(pln), myFunction
  real(double) :: yCumul(pln), cumul1,cumul2, bnd1,bnd2, median,mode,dfyMin,dfyMax, x1,dx1Pl,dx1Mn, x2,dx2Pl,dx2Mn, accur
  character :: oper
  
  call statTools_init()  ! Initialise statTools and libSUFR
  verbosity = 0  ! 1: add some extra output
  
  select case(command_argument_count())
  case(5)  ! 2 x 2 + operator: symmetric
     call get_command_argument_d(1,xmin)  ! x1 - dx1Mn
     call get_command_argument_d(2,xmax)  ! x1 + dx1Pl
     x1 = (xmin+xmax)/2.d0
     dx1Pl = abs(xmax-xmin)/2.d0
     dx1Mn = dx1Pl
     
     call get_command_argument_d(3,xmin)  ! x2 - dx2Mn
     call get_command_argument_d(4,xmax)  ! x2 + dx2Pl
     x2 = (xmin+xmax)/2.d0
     dx2Pl = abs(xmax-xmin)/2.d0
     dx2Mn = dx2Pl
     
     call get_command_argument(5,oper)    ! operation: '+', '-'
     
  case(7)  ! 2 x 3 + operator: asymmetric
     call get_command_argument_d(1,x1)    ! x1
     call get_command_argument_d(2,dx1Pl)  ! + dx1Pl
     call get_command_argument_d(3,dx1Mn)  ! - dx1Mn
     
     call get_command_argument_d(4,x2)    ! x2
     call get_command_argument_d(5,dx2Pl)  ! + dx2Pl
     call get_command_argument_d(6,dx2Mn)  ! - dx2Mn
     
     call get_command_argument(7,oper)    ! operation: '+', '-'
     
  case default
     call syntax_quit('<x1> <+dx1> <-dx1>  <x2> <+dx2> <-dx2> <operator (+ or -)>', 0, &
          'Approximate error propagation for asymmetric errors, e.g. (x1 + dx1 - dx1) + (x2 + dx2 - dx2)')
  end select
  
  
  median=0.d0; dfyMin=0.d0; dfyMax=0.d0
  
  
  write(*,'(/,9(A,A))') '  Computing:   ', d2s(x1,5), '  +  ', d2s(dx1Pl,5), '  -  ', d2s(dx1Mn,5), '    '//oper//'    ', &
       d2s(x2,5), '  +  ', d2s(dx2Pl,5), '  -  ', d2s(dx2Mn,5)

  
  ! Define functions:
  xmax = maxval([x1,x2, myFunction(x1,x2, oper)]) + sqrt(dx1Pl**2 + dx2Pl**2) * 5  ! 5 sigma ~ 0.999999426697
  xmin = minval([x1,x2, myFunction(x1,x2, oper)]) - sqrt(dx1Mn**2 + dx2Mn**2) * 5  ! 5 sigma ~ 0.999999426697
  dx = abs(xmax-xmin)/dble(pln-1)
  if(verbosity.ge.1) write(*,'(/,A,2(2x,2F8.2))') '  Xmin, Xmax, dx: ', xmin,xmax, dx
  
  ! Create a proper buffer by using 5 time the maximum of the four sigmas: -  check do this separately per operator?
  xmax = maxval([x1,x2, myFunction(x1,x2, oper)]) + maxval(abs([dx1Pl, dx1Mn, dx2Pl, dx2Mn]))*5
  xmin = minval([x1,x2, myFunction(x1,x2, oper)]) - maxval(abs([dx1Pl, dx1Mn, dx2Pl, dx2Mn]))*5
  dx = abs(xmax-xmin)/dble(pln-1)
  
  
  if(verbosity.ge.1) write(*,'(/,A,2(2x,2F8.2))') '  Xmin, Xmax, dx: ', xmin,xmax, dx
  
  ! Add an extra buffer of 20% to the range:
  xmin1 = ((xmin+xmax) - dx*(pln-1))/2.d0
  xmax1 = ((xmin+xmax) + dx*(pln-1))/2.d0
  
  if(verbosity.ge.1) write(*,'(/,A,2(2x,2F8.2))') '  Xmin, Xmax, dx: ', xmin,xmax, dx
  
  
  ! Create triangle from Fig.1, for X1:
  !call funcExamplePaper(pln, xmin,xmax, xx1,Px1)
  
  ! And another one, for X2 - identical for now:
  !call funcExamplePaper(pln, xmin,xmax, xx2,Px2)
  
  ! Try my own test function:
  !call myFunc01(pln, xmin,xmax, xx2,Px2)
  
  call asymGaussian(pln, xmin,xmax,  x1, abs(dx1Pl), abs(dx1Mn),  xx1,Px1)
  call asymGaussian(pln, xmin,xmax,  x2, abs(dx2Pl), abs(dx2Mn),  xx2,Px2)
  
  
  do iy=1,pln
     yy(iy) = xmin + dx*(iy-1)
     
     fy(iy) = 0.d0
     do jx=1,pln
        do jy=1,pln
           if( abs( myFunction(xx1(jx), xx2(jy), oper)  - yy(iy)) .lt. dx )  fy(iy) = fy(iy) + Px1(jx) * Px2(jy)
        end do
     end do
     
     if(iy.eq.1) then
        yCumul(iy) = fy(iy)
     else        
        yCumul(iy) = yCumul(iy-1) + fy(iy)
     end if
  end do
  
  accur = 1.d0 - sqrt((2.d0-yCumul(pln))/2.d0)      ! Estimate the accuracy
  write(*,'(/,A,F9.5,A)') '  Accuracy:  ', accur, '  (1.0 is perfect)'
  if(abs(yCumul(pln)-2.d0).gt.0.001d0) then
     write(*,'(////,A)') '  ***  WARNING:  the accuracy is very poor: your results may not be correct!  ***'
     write(*,'(A,//)') '       Press  ENTER  to continue...'
     read(*,*)
  end if
  fy = fy / yCumul(pln)
  
  
  
  ! Determine 1-sigma range of f(y):
  bnd1 = (1.d0-0.682689492137d0)/2.d0
  bnd2 = 1.d0 - bnd1
  mode = -huge(mode)
  
  do iy=2,pln
     cumul1 = yCumul(iy-1)/yCumul(pln)
     cumul2 = yCumul(iy)/yCumul(pln)
     
     if(mode.lt.-huge(mode)/2.d0 .and. fy(iy).lt.fy(iy-1)) mode = (yy(iy-1)+yy(iy))/2.d0
     
     if(cumul1.lt.0.5d0 .and. cumul2.ge.0.5d0) then
        median = linear_interpolation(cumul1,cumul2, yy(iy-1)+dx/2.d0,yy(iy)+dx/2.d0, 0.5d0)  ! Median; npl must be even
     end if
     
     if(cumul1.lt.bnd2 .and. cumul2.gt.bnd2) then
        dfyMin = linear_interpolation(cumul1,cumul2, yy(iy-1)+dx/2.d0, yy(iy)+dx/2.d0, bnd2)  ! 'Upper sigma;' npl must be even!
     end if
     if(cumul1.lt.bnd1 .and. cumul2.gt.bnd1) then
        dfyMax = linear_interpolation(cumul1,cumul2, yy(iy-1)+dx/2.d0, yy(iy)+dx/2.d0, bnd1)  ! 'Lower sigma;' npl must be even!
     end if
  end do
  
  
  ! Case where (at least) one set of errors is zero:
  if(deq0(dx1Pl).and.deq0(dx1Mn)) then  ! Errors of x1 are zero; final errors are equal to those of x2
     median = myFunction(x1,x2, oper)
     mode = median
     dfyMin = median + abs(dx2Pl)
     dfyMax = median - abs(dx2Mn)
     yCumul(pln) = 2.d0
  end if
  if(deq0(dx2Pl).and.deq0(dx2Mn)) then  ! Errors of x2 are zero; final errors are equal to those of x1
     median = myFunction(x1,x2, oper)
     mode = median
     dfyMin = median + abs(dx1Pl)
     dfyMax = median - abs(dx1Mn)
     yCumul(pln) = 2.d0
  end if


  ! Print output:
  write(*,*)
  write(*,'(A)')          '  Results: '
  write(*,'(A,F14.5)')    '  Median:  ', median
  write(*,'(A,F14.5)')    '  Mode:    ', mode
  write(*,*)
  write(*,'(3(A,A))') '  Asymmetric:  ', d2s(median,5), '   +  ', d2s(abs(median-dfyMin),5), '   -  ', d2s(abs(median-dfyMax),5)
  write(*,'(2(A,A))') '  Symmetric:   ', d2s((dfyMin+dfyMax)/2.d0,5), '  +-  ', d2s(abs(dfyMin-dfyMax)/2.d0,5)
  write(*,'(2(A,A))') '  Range:       ', d2s(dfyMax,5), '  --  ', d2s(dfyMin,5)
  write(*,*)
  
end program asymmetric_errors
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Create the triangle function from the example in Fig.1:
!!
!! \param  pln   Number of data points in this function
!!
!! \param  xmin  Minimum value for x
!! \param  xmax  Maximum value for x
!!
!! \retval xx    x: quantity of which the probability must be described
!! \retval Px    P(x): probability of x

subroutine funcExamplePaper(pln, xmin,xmax, xx,Px)
  use SUFR_kinds, only: double
  
  implicit none
  integer, intent(in) :: pln
  real(double), intent(in) :: xmin,xmax
  real(double), intent(out) :: xx(pln),Px(pln)
  integer :: pli
  real(double) :: dx
  
  dx = abs(xmax-xmin)/dble(pln-1)
  do pli=1,pln
     xx(pli) = xmin + dx*(pli-1)
     if(xx(pli).lt.-1.d0 .or. xx(pli).gt.1.d0) then
        Px(pli) = 0.d0
     else
        if(xx(pli).lt.0.5d0) then
           Px(pli) = 2.d0/3.d0*xx(pli) + 2.d0/3.d0
        else
           Px(pli) = -2.d0*xx(pli) + 2.d0
        end if
     end if
  end do
  
end subroutine funcExamplePaper
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Create my own example function
!!
!! \param  pln   Number of data points in this function
!!
!! \param  xmin  Minimum value for x
!! \param  xmax  Maximum value for x
!!
!! \retval xx    x: quantity of which the probability must be described
!! \retval Px    P(x): probability of x

subroutine myFunc01(pln, xmin,xmax, xx,Px)
  use SUFR_kinds, only: double
  
  implicit none
  integer, intent(in) :: pln
  real(double), intent(in) :: xmin,xmax
  real(double), intent(out) :: xx(pln),Px(pln)
  integer :: pli
  real(double) :: dx
  
  dx = abs(xmax-xmin)/dble(pln-1)
  do pli=1,pln
     xx(pli) = xmin + dx*(pli-1)
     if(xx(pli).lt.-2.d0 .or. xx(pli).gt.0.d0) then
        Px(pli) = 0.d0
     else
        if(xx(pli).lt.0.5d0) then
           Px(pli) = 1.d0/2.d0*(xx(pli)+1.d0) + 1.d0/2.d0
        else
           Px(pli) = -2.d0*(xx(pli)+1.d0) + 2.d0
        end if
     end if
  end do
  
end subroutine myFunc01
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Create an 'asymmetric Gaussian' distribution
!!
!! \param  pln   Number of data points in this function
!!
!! \param  xmin  Minimum value for x
!! \param  xmax  Maximum value for x
!!
!! \param  mu    Mean of the Gaussian
!! \param  sig1  Standard deviation for the upper half of the Gaussian (x>mu)
!! \param  sig2  Standard deviation for the lower half of the Gaussian (x<mu)
!!
!! \retval xx    x: quantity of which the probability must be described
!! \retval Px    P(x): probability of x

subroutine asymGaussian(pln, xmin,xmax, mu,sig1,sig2, xx,Px)
  use SUFR_kinds, only: double
  !use SUFR_constants, only: pi
  
  implicit none
  integer, intent(in) :: pln
  real(double), intent(in) :: xmin,xmax, mu,sig1,sig2
  real(double), intent(out) :: xx(pln),Px(pln)
  integer :: pli
  real(double) :: dx, cumul
  
  dx = abs(xmax-xmin)/dble(pln-1)
  cumul = 0.d0
  
  do pli=1,pln
     xx(pli) = xmin + dx*(pli-1)
     
     if(xx(pli).ge.mu) then
        !Px(pli) = 1.d0/(sig1*sqrt(2*pi)) * exp( -(xx(pli)-mu)**2 / (2*sig1**2) )
        Px(pli) = exp( -(xx(pli)-mu)**2 / (2*sig1**2) )
     else
        !Px(pli) = 1.d0/(sig2*sqrt(2*pi)) * exp( -(xx(pli)-mu)**2 / (2*sig2**2) )
        Px(pli) = exp( -(xx(pli)-mu)**2 / (2*sig2**2) )
     end if
     
     cumul = cumul + Px(pli)
     
  end do
  
  Px = Px / cumul !  Normalise
  
end subroutine asymGaussian
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Discrete Dirac delta:  return 1 if |x-x0| < dx, 0 otherwise.  Very expensive, since you'll multiply 0 with other
!!                                numbers very often in some integral.  Instead, use the if statement in your code directly.

function DiracDelta(x, x0, dx)
  use SUFR_kinds, only: double
  
  implicit none
  real(double), intent(in) :: x, x0, dx
  real(double) :: DiracDelta
  
  DiracDelta = 0.d0
  if(abs(x-x0).le.abs(dx)) DiracDelta = 1.d0
  
end function DiracDelta
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Function used to compute desired variable
!!
!! \param  x1    Variable 1
!! \param  x2    Variable 2
!! \param  oper  Operation used ('+', '-', '*', 'x', '/')

function myFunction(x1,x2, oper)
  use SUFR_kinds, only: double
  implicit none
  real(double), intent(in) :: x1,x2
  character, intent(in) :: oper
  real(double) :: myFunction
  
  select case(oper)
  case('+')
     myFunction = x1 + x2
  case('-')
     myFunction = x1 - x2
  case('*','x')
     myFunction = x1 * x2
  case('/')
     myFunction = x1 / x2
  case default
     write(*,'(A)') '  Using a custom operation'
     write(*,'(A)') '  No custom operation defined'
     myFunction = 0.d0
  end select
  
end function myFunction
!***********************************************************************************************************************************

