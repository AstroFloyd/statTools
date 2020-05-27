!> \file statistics.f90  Calculate basic statistics for a number of data points

!***********************************************************************************************************************************
!> \brief  Calculate basic statistics for a number of data points
!!
!! \todo
!! - Fancy +- printing at bottom doesn't work for negative mean_om, stdev_om
!!
!! - AF, <= 2009-01-27
!! - 2012-05-11: use libSUFR; print median and variance
!! - 2012-06-21: print mean +- stdev in correct accuracy
!! - 2015-03-03: print min-max range and standard error of the mean
!!

program statistics
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_statistics, only: median, stdev
  use SUFR_numerics, only: dne0
  use SUFR_text, only: d2s
  use ST_general, only: statTools_init
  
  implicit none
  real(double), allocatable :: dat(:)
  
  integer :: n, iArg,nArg, mean_om,stdev_om, extra_digits, nexp
  real(double) :: tot,mean,median1,prod,geomean, var,myStDev, minx,maxx,rangex, mean_fmt,stdev_fmt, nd
  character :: arg*(99), fmt*(99)
  
  
  call statTools_init()  ! Initialise statTools and libSUFR
  
  nArg = command_argument_count()
  if(Narg.lt.2)  call syntax_quit('<x_1> <x_2> [<x_3> ... <x_n>]', 0, 'Calculate basic statistics for a number of data points')
  
  n = nArg
  allocate(dat(n))
  
  do iArg=1,nArg
     call get_command_argument(iArg,arg)
     read(arg,*) dat(iArg)
  end do
  
  
  nd      = dble(n)
  tot     = sum(dat(1:n))
  mean    = tot/nd
  median1 = median(dat(1:n))
  prod    = product(dat(1:n))
  geomean = prod**(1.d0/nd)
  
  myStDev = stdev(dat(1:n), dMean=mean, var=var)  ! Compute standard deviation and variance
  
  
  minx = minval(dat(1:n))
  maxx = maxval(dat(1:n))
  rangex = maxx - minx
  
  
  
  extra_digits = 0   ! Print more than the default number of significant digits: default 0, try 1
  mean_om = 1
  if(dne0(mean)) mean_om = floor(log10(abs(mean)))
  stdev_om = 1
  if(dne0(myStDev)) stdev_om = floor(log10(abs(myStDev))) - extra_digits
  
  ! Extra room for exponent if <0, <-9, >9, >99, ...
  nexp = 0
  if(stdev_om.gt.0.d0) nexp = floor(log10(abs(dble(stdev_om))))
  if(stdev_om.lt.0.d0) nexp = ceiling(log10(abs(dble(stdev_om))))
  
  
  
  write(*,*)
  write(*,'(A,I15)')         '  n:             ',n
  write(*,'(A,1p,G15.6)')    '  sum:           ',tot
  ! Standard error of the mean - https://en.wikipedia.org/wiki/Standard_error#Standard_error_of_the_mean:
  write(*,'(2(A,1p,G15.6))') '  mean + err:    ',mean, ' +- ',myStDev/sqrt(dble(n))
  write(*,'(A,1p,G15.6)')    '  median:        ',median1
  write(*,'(A,1p,G15.6)')    '  variance:      ',var
  write(*,'(A,1p,G15.6)')    '  stdev:         ',myStDev
  write(*,'(A,1p,G15.6)')    '  stdev/mean:    ',myStDev/mean
  write(*,'(A,1p,G15.6)')    '  geom. mean:    ',geomean
  
  write(*,*)
  write(*,'(A,1p,G15.6)')    '  min:           ',minx
  write(*,'(A,1p,G15.6)')    '  max:           ',maxx
  write(*,'(A,1p,G15.6)')    '  max/min:       ',maxx/minx
  write(*,'(A,1p,G15.6)')    '  range:         ',rangex
  write(*,'(A,1p,G15.6)')    '  range/mean:    ',rangex/mean
  
  write(*,*)
  if(abs(mean_om).le.3 .and. abs(stdev_om).le.3) then
     write(*,'(A)')          '  mean +- stdev:          '//d2s(mean,3)//' +- '//d2s(myStDev,3)
     write(*,'(A)')          '  mean +- stdev range:    '//d2s(mean-myStDev,3)//' - '//d2s(mean+myStDev,3)
  else
     mean_fmt = mean/10.d0**stdev_om
     stdev_fmt = myStDev/10.d0**stdev_om
     !print*,mean_om, stdev_om, mean_om-stdev_om
     write(fmt,'(A,I3.3,A,I3.3,A,I3.3,A)') '(A,I',abs(mean_om-stdev_om)+1,', A,I',extra_digits+1,',A,I',nexp+1,')'
     
     write(*,trim(fmt))      '  mean +- stdev:          ', nint(mean_fmt), ' +- ', nint(stdev_fmt), ' x 10^',stdev_om
     write(*,trim(fmt))      '  mean +- stdev range:    ', nint(mean_fmt-stdev_fmt), ' - ', nint(mean_fmt+stdev_fmt), &
          ' x 10^',stdev_om
  end if
  
  write(*,'(2(A,1p,G12.3))') '  min - max range:        ', minx, ' - ', maxx
  
  write(*,*)
  
  
end program statistics
!***********************************************************************************************************************************
