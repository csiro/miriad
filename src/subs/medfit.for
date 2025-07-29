#ifdef TEST
c************************************************************************
	program test
c
	integer N
	parameter(N=300)
	integer i
	real a,b,x(N),y(N)
c
	call gaus(y,N)
	do i=1,N
	  x(i) = i
	  y(i) = y(i) + 2*i
	enddo
c
	call medfit(x,y,n,a,b,.false.)
	write(*,*)'Fit without offset:',a,b
	call medfit(x,y,n,a,b,.true.)
	write(*,*)'Fit with offset:   ',a,b
	end
#endif
c************************************************************************
	subroutine medfit(x,y,n,a,b,dooff)
c
	implicit none
	integer n
	real X(n),Y(n),a,b
	logical dooff
c
c  Determine "a" and (possibly) "b" such that
c    y \approx a * x + b
c  in a L1 sense.
c
c  Input:
c    X,Y	Data.
c    n		Number of data points.
c    dooff	Solve for "b" as well as "a"
c  Output:
c    a		Scale factor.
c    b		Offset. This is set to zero if "dooff" is false.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	double precision SumXX,SumYY,SumXY,SumX,SumY
	real rms,a1,a2,f1,f2,f,delta
	integer i
c
	integer tmp
c
c  Externals.
c
	real medfunc
c
c  Determine the least squares fit first.
c
	SumX  = 0
	SumY  = 0
	SumXX = 0
	SumYY = 0
	SumXY = 0
	do i=1,n
	  SumX  = SumX  + X(i)
	  SumY  = SumY  + Y(i)
	  SumXX = SumXX + X(i)*X(i)
	  SumYY = SumYY + Y(i)*Y(i)
	  SumXY = SumXY + X(i)*Y(i)
	enddo
	if(dooff)then
	  delta = (n*SumXX - SumX*SumX)
	  a = (n*SumXY - SumX*SumY)/delta
	  b = (SumXX*SumY - SumX*SumXY)/delta
	  rms = SumYY + a*a*SumXX + b*b*n
     *		- 2*a*SumXY - 2*b*SumY - 2*a*b*SumX
	  rms = rms / delta
	else
	  delta = n*SumXX
	  a = n*SumXY/delta
	  b = 0
	  rms = (SumYY + a*a*SumXX - 2*a*SumXY)
	  rms = rms / delta
	endif
c
	rms = max(0.01*a,sqrt(max(0.,rms)))
c
	if(dooff)then
	  call memAlloc(tmp,n,'r')
	else
	  tmp = 1
	endif
c
	a1 = a
	f1 = medfunc(a1,b,X,Y,memr(tmp),n,dooff)
	a2 = a1 + sign(3*rms,f1)
	f2 = medfunc(a2,b,X,Y,memr(tmp),n,dooff)
	dowhile(f1*f2.gt.0)
	  a = 2*a2 - a1
	  a1 = a2
	  f1 = f2
	  a2 = a
	  f2 = medfunc(a2,b,X,Y,memr(tmp),n,dooff)
	enddo
c
	dowhile(abs(a1-a2).gt.0.001*rms)
	  a = 0.5*(a1+a2)
	  if(a.eq.a1.or.a.eq.a2)return
	  f = medfunc(a,b,X,Y,memr(tmp),n,dooff)
	  if(f*f1.ge.0)then
	    f1 = f
	    a1 = a
	  else
	    f2 = f
	    a2 = a
	  endif
	enddo
c
	if(dooff)call memFree(tmp,n,'r')
c
	end
c************************************************************************
	real function medfunc(a,b,X,Y,tmp,n,dooff)
c
	implicit none
	integer n
	real a,b,X(n),Y(n),tmp(n)
	logical dooff
c
c  Determine the function needed to solve for the median.
c
c------------------------------------------------------------------------
	integer i
c
	if(dooff)then
	  do i=1,n
	    tmp(i) = y(i) - a*x(i)
	  enddo
	  call median(tmp,n,b)
	else
	  b = 0
	endif
c
	medfunc = 0
	do i=1,n
	  medfunc = medfunc + x(i)*sign(1.0,y(i)-a*x(i)-b)
	enddo
c
	end
