c************************************************************************
c History:
c  rjs  13feb90 Adapted from the velocalc routines, which these routines
c		replace.
c  rjs  10aug93 Used maxdim.h. How did this do so long without being found?
c  mjs  12mar94 rename file -> uvfitsubs.for (so name is unique in miriad).
c************************************************************************
c*uvfit1 -- Fit a constant to an "object" returned by uvinfo.
c:uv-data
c+
	subroutine uvfit1(tno,object,n,a,epsi)
c
	implicit none
	integer tno,n
	character object*(*)
	double precision a,epsi
c
c  UvFit1 fits a constant to an "object" return by uvinfo. It
c  returns the estimate of the constant and the rms error of the
c  fit.
c
c  Input:
c    tno	Handle of the uv data file.
c    object	Some string, passed down to uvinfo.
c    n		The number of channels ("nread" returned by uvread).
c  Output:
c    a		The estimate of the constant value.
c    epsi	Rms error of the estimate.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i
	double precision Syy,Sy
	double precision buf(maxchan)
c
c  Get the parameter for channel, determine the average, and determine
c  the rms error.
c
	if(n.gt.maxchan) call bug('f','Too many channels for me')
	call uvinfo(tno,object,buf)
	Syy = 0
	Sy = 0
	do i=1,n
	  Sy = Sy + buf(i)
	  Syy = Syy + buf(i)*buf(i)
	enddo
	a = Sy/n
	epsi = (Syy + a*a*n - 2*a*Sy)/n
	epsi = sqrt(max(0.d0,epsi))
	end
c************************************************************************
c*uvfit2 -- Fit a line to an "object" returned by uvinfo.
c:uv-data
c+
	subroutine uvfit2(tno,object,n,a,b,epsi)
c
	implicit none
	integer tno,n
	character object*(*)
	double precision a,b,epsi
c
c  UvFit2 fits a line to an "object" return by uvinfo. It
c  returns the estimate of the poarameters of the line, along with
c  the rms error of the fit.
c
c  Input:
c    tno	Handle of the uv data file.
c    object	Some string, passed down to uvinfo.
c    n		The number of channels ("nread" returned by uvread).
c  Output:
c    a,b	The equation of the line is
c		 a*(i-1) + b
c    epsi	Rms error of the estimate.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i
	double precision Syy,Sy,Sxy,Sx,Sxx
	double precision buf(maxchan)
c
c  Get the parameter for channel, determine the average, and determine
c  the rms error.
c
	if(n.gt.maxchan) call bug('f','Too many channels for me')
	call uvinfo(tno,object,buf)
c
	if(n.eq.1)then
	  a = buf(1)
	  b = a
	  epsi = 0
	else
c
c  Sum statistics about the velocities.
c
	  Syy = 0
	  Sy = 0
	  Sxy = 0
	  do i=1,n
	    Sy = Sy + buf(i)
	    Syy = Syy + buf(i)*buf(i)
	    Sxy = Sxy + (i-1)*buf(i)
	  enddo
c
c  Use standard formulae to calculate the sum of (i-1) and the sum of
c  (i-1)**2.
c
	  Sx  = n*(n-1)/2
	  Sxx = ( 2*(n-1)**3 + 3*(n-1)**2 + (n-1) ) / 6
c
c  Calculate b and a, using standard linear least squares formulae.
c
	  b = (Sxx*Sy - Sxy*Sx)/(n*Sxx - Sx*Sx)
	  a = (n*Sxy - Sx*Sy)/(n*Sxx - Sx*Sx)
c
c  Calculate the rms error of the fit.
c
	  epsi = Syy - 2*a*Sxy - 2*b*Sy + a*a*Sxx
     *		+ n*b*b + 2*a*b*Sx
	  epsi = sqrt(max(epsi/n,0.0d0))
	endif
c
	end
