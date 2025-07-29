c************************************************************************
c*Snrm2 -- Euclidean norm of a real vector.
c:BLAS
c+
	real function snrm2( n, cx, incx)
c
	integer n,incx
	real cx(*)
c
c  Find the Euclidean norm of a vector of reals. Do it in a way
c  to help avoid overflow and underflow problems.
c
c  Input:
c    n		Number of elements.
c    incx	Increment between elements.
c    cx		Real vector.
c
c  Output:
c    snrm2	The Euclidean norm.
c--
c------------------------------------------------------------------------
	integer i
	real scal,maxv,sum
c
c  Externals.
c
	integer isamax
c
	if(n.le.0)then
	  snrm2 = 0
	else
	  i = isamax(n,cx,incx)
	  i = (i-1)*incx + 1 
	  maxv = abs( cx(i) )
	  scal = 1./maxv
	  sum = 0
	  do 10 i=1,n*incx,incx
	    sum = sum + ( scal*cx(i) )**2
   10	  continue
	  snrm2 = maxv * sqrt( sum )
	endif
	end
