c************************************************************************
c  Least squares fitting routines.
c
c  History:
c    rjs Dark-ages Original version.
c    rjs  8sep89   Improved documentation.
c    rjs 12dec89   Improved documentation and error reporting some more.
c    rjs 25jan90   Minor documentation improvement.
c    bpw 11sep90   Add linlsq
c    bpw 12jul91   Corrected backwards relation in documentation of linlsq
c    mjs 25jul91   Re-included "programmer" in in-code docs
c
c************************************************************************
c*Llsqu - Linear least squares fitting
c&bpw
c:least-squares,fitting
c+
	subroutine llsqu(f,A,n,m,c,ifail,B,pivot)
c
	implicit none
	integer m,n,ifail,pivot(n)
	real f(m),A(n,m),c(n),B(n,n)
c
c  Solve a linear least squares problem in "n" unknowns, and "m" equations.
c  As usual, m must be greater or equal to n.
c
c  The problem is solved by finding:
c					t
c				  y' = A y
c
c  and					t
c				  B  = A A
c
c  then solving this system of linear equations.
c
c  The LINPACK routines SGEFA and SGESL are called to solve a system of
c  linear equations.
c
c  Inputs:
c    n		Number of unknowns.
c    m		Number of equations.
c    f		Function values for the m equations.
c    A		The matrix giving the weights for each equation.
c
c  Scratch:
c    B
c    pivot
c
c  Output:
c    c		The solution coefficients.
c    ifail	Success status:
c		  0	Finished OK.
c		  1	Singular matrix encountered.
c--
c------------------------------------------------------------------------
	double precision sum
	integer i,j,k
c
c  If m and n are equal, then it is a simple solution of linear equations.
c
	if(m.lt.n)then
	  ifail = 1
	else if(m.eq.n)then
	  do i=1,n
	    c(i) = f(i)
	  enddo
	  do j=1,n
	    do i=1,n
	      B(i,j) = A(i,j)
	    enddo
	  enddo
	  call sgefa(B,n,n,pivot,ifail)
	  if(ifail.eq.0)call sgesl(B,n,n,pivot,c,1)
c
c  If m and n are not equal, then we have to generate the 
c  Accumulate in double precision to avoid some rounding errors.
c
	else
	  do i=1,n
	    sum = 0
	    do k=1,M
	      sum = sum + A(i,k)*f(k)
	    enddo
	    c(i) = sum
c
	    do j=i,n
	      sum = 0
	      do k=1,m
	        sum = sum + A(i,k)*A(j,k)
	      enddo
	      B(i,j) = sum
	      B(j,i) = sum
	    enddo
	  enddo
	  call sgefa(B,n,n,pivot,ifail)
	  if(ifail.ne.0)then
	    ifail = 1
	  else
	    call sgesl(B,n,n,pivot,c,1)
	  endif
	endif
	end
c************************************************************************
c*Nllsqu -- Nonlinear least squares fitting
c&bpw
c: least-squares,fitting
c+
	subroutine nllsqu(n,m,x,h,itmax,eps1,eps2,der,ifail,
     *	  FUNCTION,DERIVE,f,fp,dx,dfdx,aa)
c
	implicit none
	integer n,m,itmax,ifail
	real eps1,eps2,x(n),h(n)
	logical der
	external FUNCTION,DERIVE
c
	real f(m),fp(m),dx(n),dfdx(n,m),aa(n,n)
c
c  NLLSQU minimizes the sum of squares, and solves a set of nonlinear
c  equations. This is derived from H. Spath, "The damped Taylors series
c  method for minimizing a sum of squares and solving systems of nonlinear
c  equations." Comm. ACM v10, n11, p726-728.
c
c  There have been some modifications to the algorithm as presented in CACM.
c  In particular the call sequence is different, and the algorithm has been
c  mildly improved in a few places.
c
c  Inputs:
c    n		Number of unknowns.
c    m		Number of nonlinear equations.
c    itmax	Max no of iterations.
c    eps1	Iteration stops if (sum f**2) < eps1
c    eps2	Iteration stops if eps2 * sum (abs(x)) < sum( abs(dx) )
c    der	Logical. If true, then the derivative routine is called. If
c		false, the derivative is estimated by many calls to FUNCTION.
c    h		This is used ONLY if der=.false. It gives the step sizes
c		to use in estimating partial derivatives.
c  Input/Output:
c    x		Input: Initial estimate of solution.
c		Output:	The best solution so far.
c
c  Scratch:
c    f
c    fp
c    dx
c    dfdx
c    aa
c
c  Outputs:
c    ifail	ifail = 0 All OK.
c			1 Singular matrix encountered.
c			2 Max number of iterations exceeded.
c			3 Failure to find better solution.
c
c  Externals:
c	The external FUNCTION must be implemented. But DERIVE is not
c	needed if "der" is set false.
c
cc	subroutine DERIVE(x,dfdx,n,m)
cc	real x(n),dfdx(n,m)
cc Inputs:
cc      x	Prospective solution.
cc Outputs:
cc	dfdx	Derivatives of the nonlinear equation for this particular
cc		value of x.
cc
cc	subroutine FUNCTION(x,f,n,m)
cc	real x(n),f(m)
cc Inputs:
cc	x	Prospective solution.
cc Outputs:
cc	f	Value of the m nonlinear equations, given x.
c--
c------------------------------------------------------------------------
	real lambda
	parameter(lambda=0.2)
	integer i,k,z,l
	real hf,hl,hs,hz,hh
	logical first
c
	first = .true.
	hs = 0
	z = 0
c
c ITERATION:
  10	z = z + 1
	ifail = 2
	if(z.gt.itmax)goto 30
	l = 0
	hl = 1.
c
c DAMP:
  20	l = l + 1
	ifail = 3
	if(l.gt.16)goto 30
	call FUNCTION(x,f,n,m)
	hf = 0
	do i=1,m
	  hf = hf + f(i)*f(i)
	enddo
	if(.not.first.and.hf.gt.hs)then
	  hl = 0.5*hl
	  do k=1,n
	    x(k) = x(k) + hl*dx(k)
	  enddo
	  goto 20
	endif
	first = .false.
	hs = hf
	ifail = 0
	if(hs.lt.eps1)goto 30
c
c  Determine the Jacobian matrix.
c
	if(der)then
	  call DERIVE(x,dfdx,n,m)
	else
	  do i=1,n
	    hh = x(i)
	    x(i) = hh + h(i)
	    call FUNCTION(x,fp,n,m)
	    x(i) = hh
	    hz = 1.0/h(i)
	    do k=1,m
	      dfdx(i,k) = hz*(fp(k)-f(k))
	    enddo
	  enddo
	endif
c
c  Perform the linear least squares solution.
c
	call llsqu(f,dfdx,n,m,dx,ifail,aa,fp)
	if(ifail.ne.0)goto 30
c
c  Add the estimated step change to x and check for convergence.
c
	hz = 0
	hf = 0
	do i=1,n
	  x(i) = x(i) - dx(i)
	  hz = hz + abs(x(i))
	  hf = hf + abs(dx(i))
	enddo
	if(hf.ge.eps2*hz)goto 10
c
c ENDE:
  30	continue
	end

c-----------------------------------------------------------------------

c* linlsq - return parameters of a straight line fit
c: least-squares
c& bpw
c+
      subroutine linlsq( xarr,yarr,npnt, a1,b1,a2,b2, sigx,sigy,corr )

      real           xarr(*)
      real           yarr(*)
      integer        npnt
      real           a1, a2, b1, b2
      real           sigx, sigy, corr

c This routine returns the parameters of a linear least squares fit to the
c relation defined by xarr and yarr.
c 
c
c Input:
c   xarr:         the x values
c   yarr:         the y values
c   npnt:         number of elements of xarr and yarr
c
c Output:
c   a1, b1:       coefficients of the relation y=a1*x+b1
c   a2, b2:       coefficients of the relation x=a2*y+b2
c   sigx, sigy:   rms values of x and y
c   corr:         correlation coefficient
c--

      real           sumx, sumy, sumsqx, sumsqy, sumxy
      real           x, y

      integer        i

      sumx   = 0.
      sumy   = 0.
      sumsqx = 0.
      sumsqy = 0.
      sumxy  = 0.
      do i = 1, npnt
        x      = xarr( i )
        y      = yarr( i )
        sumx   = sumx   + x
        sumy   = sumy   + y
        sumsqx = sumsqx + x**2
        sumsqy = sumsqy + y**2
        sumxy  = sumxy  + x*y
      enddo

      if( sumy.eq.0. .and. sumsqy.eq.0. ) then
        a1   = 0.
        a2   = 0.
        b1   = 0.
        b2   = 0.
        sigx = 0.
        sigy = 0.
        corr = 0.
      else
        a1   = ( npnt*sumxy - sumx*sumy ) / ( npnt*sumsqx - sumx**2 )
        a2   = ( npnt*sumxy - sumx*sumy ) / ( npnt*sumsqy - sumy**2 )
        b1   = ( sumy - a1*sumx ) / npnt
        b2   = ( sumx - a2*sumy ) / npnt
        sigx = sqrt(  sumsqx/npnt - sumx*sumx/npnt/npnt )
        sigy = sqrt(  sumsqy/npnt - sumy*sumy/npnt/npnt )
        corr = ( sumxy/npnt  - sumx*sumy/npnt/npnt ) / (sigx*sigy)
      endif
      
      return
      end

