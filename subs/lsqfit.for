c************************************************************************
c* lsqfit -- Non-linear least squares fitter.
c& rjs
c: least-squares,fitting
c+
	subroutine lsqfit(FCN,m,n,x,covar,rms,ifail1,ifail2)
c
	implicit none
	external FCN
	integer m,n,ifail1,ifail2
	real x(n),covar(n,n),rms
c
c  Lsqfit minimises the sum of squares of M non-linear equations in N
c  unknown variables "x". The caller provides a subroutine which calculats
c  the non-linear equations.
c
c  Input:
C   FCN      This is the name of the user-supplied subroutine which
c            calculates the functions. fcn must be declared
c            in an external statement in the user calling
c            routine, and should be written as follows.
c
c              subroutine fcn(m,n,x,fvec,iflag)
c              integer m,n,iflag
c              real x(n),fvec(m)
c                ---------- 
c                calculate the functions at x and
c                return this vector in fvec.
c                ----------
c             return  
c             end   
c     
c           the value of iflag should not be changed by fcn unless
c           the user wants to terminate execution.
c           In this case set iflag to a negative integer.
c
c    n      Number of unknowns being solved for.
c    m      Number of equations.
c  Input/Output:
c    x	    On input, this should give an initial estimate of the solution
c	    vector. On output, this gives the final solution vector estimate.
c  Output:
c    covar  The covariance matrix.
c    rms    The rms error at the end of the fitting process.
c    ifail1 This gives the status of the fitting process. 0 indicates
c	    success.
c    ifail2 This gives the status of the determination of the covariance
c	    matrix. 0 indicates success.
c--
c  History:
c    ??????? rjs  Original version.
c    05dec95 rjs  Documented.
c------------------------------------------------------------------------
	real epsfcn,tol
	parameter(epsfcn=1e-3,tol=1e-6)
	include 'maxdim.h'
	include 'mem.h'
	integer lwa,wa,iwa
c
	lwa = m*n + 5*n + 2*m
	call memalloc(wa,lwa,'r')
	call memalloc(iwa,n,'i')
	call lmdiff(FCN,m,n,x,memr(wa),epsfcn,tol,
     *	  ifail1,memi(iwa),memr(wa+m),lwa-m)
c
c  Fiddle the IFAIL status.
c
	if(ifail1.ge.1.and.ifail1.le.3)then
	  ifail1 = 0
	else if(ifail1.eq.0)then
	  ifail1 = 1
	endif
c
c  Get the covariance and rms back.
c
	ifail2 = ifail1
	if(ifail2.eq.0)call getcovar(FCN,m,n,x,memr(wa),memr(wa+m),
     *	    epsfcn,covar,rms,ifail2)
c
c  Free up memory.
c
	call memfree(wa,lwa,'r')
	call memfree(iwa,n,'i')
	end
c************************************************************************
	subroutine getcovar(FCN,m,n,x,fvec,wrk,epsfcn,covar,rms,ifail)
c
	implicit none
	integer m,n,ifail
	real fvec(m),x(n),wrk(m,n+1),epsfcn,rms
	external FCN
	real covar(n,n)
c
c  Determine the covariance matrix after calling LMDIF or LMDIFF.
c
c------------------------------------------------------------------------
	double precision sumsq,temp
	integer i,j,k
	real rtemp
c
c  Determine the sum of squares.
c
	sumsq = 0
	do i=1,m
	  sumsq = sumsq + fvec(i)*fvec(i)
	enddo
	rms = sqrt( sumsq / m )
c
c  Determine the jacobian matrix.
c
	ifail = 0
	call fdjac2(FCN,m,n,x,fvec,wrk,m,ifail,epsfcn,wrk(1,n))
	if(ifail.ne.0)return
c
c  Determine the J^T * J.
c
	do j=1,n
	  do i=j,n
	    temp = 0
	    do k = 1,m
	      temp = temp + wrk(k,j)*wrk(k,i)
	    enddo
	    covar(i,j) = temp * (m-n) / sumsq
	    covar(j,i) = temp * (m-n) / sumsq
	  enddo
	enddo
c
c
c  Invert it.
c
	call krout(0,n,0,covar,n,rtemp,0,ifail,wrk(1,1),wrk(1,2))
c
	end
