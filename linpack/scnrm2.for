c************************************************************************
c*Scnrm2 - Euclidean norm of a complex vector.
c:BLAS
c*
	real function scnrm2(n,cx,incx)
c
	integer n,incx
	complex cx(*)
c
c  Find the Euclidean norm of a complex vector.
c
c  Input:
c    n		Length of the vector.
c    cx		The complex vector.
c    incx	The increment.
c  Output:
c    scnrm2	The Euclidean norm.
c--
c------------------------------------------------------------------------
	integer i
	real sum,maxv,scal
	complex temp
c
c  Externals.
c
	integer icamax
c
	if(n.le.0)then
	  scnrm2 = 0
	else
	  i = icamax(n, cx, incx)
	  i = (i-1)*incx + 1
	  maxv = abs(cx(i))
	  scal = 1/maxv
	  sum = 0
	  do 10 i=1,n*incx,incx
	    temp = scal * cx(i)
	    sum = sum + real(temp)**2 + aimag(temp)**2
   10	  continue
	  scnrm2 = maxv * sqrt(sum)
	endif
	end
