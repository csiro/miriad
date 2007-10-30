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
c    rjs 05mar98   Broken out freom lsqu.for, to work around LINUX problem.
c
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
