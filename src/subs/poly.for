c************************************************************************
c  History:
c    rjs          Picked a long time ago.
c    rjs  28oct93 Include double precision versions.
c*************************************************************************
c*Rpolyzr -- Roots of a real polynomial.
c:polynomials
c+
      subroutine rpolyzr(a,nn,roots,ifail)
c
      implicit none
      integer nn,ifail
      complex roots(nn)
      real a(0:nn)
c
c  This procedure attempts to solve a real polynomial equation in degree nn
c  using the technique suggested by Grant and Hitchins (1971) to limiting
c  machine precision. On entry, the coefficients of the polynomial are
c  held in the array a(0:nn), with a(0) holding the coefficient of the
c  highest power. On normal entry, the parameter ifail has value zero,
c  and will remain zero on successful exit, with the calculated estimates
c  of the roots held in roots(1:nn), in approximately decreasing order
c  of magnitude. The parameter "tol" givers the precision of the floating
c  binary arithmetic used, normally 2**(-t), where "t" is the number
c  of bits in the mantissa.
c
c  Abnormal exits will be indicated by ifail having a value 1 or 2. The
c  former implieds that either a(0) = 0 or nn < 1. For ifail=2, a
c  possible saddle point has been detected. The degree of the reduced poly
c  is stored in nn and its coefficients are held in a(0:nn), the roots
c  obtained thus far are held in roots(nn+1) onwards. An immediate
c  re-entry is possible with ind unchanged and with a new starting
c  point for the search held in roots(1).
c
c  The algorithm has been copies from:
c    Grant J.A., Hitchins G.D., (1973 or 1974) "Two algorithms for the
c    solution of polynomial equations to limiting machine precision"
c    The Computer Journal, vol 18, n3, pp 258-264
c
c  Apart from cosmetic changes, it is an FORTRAN equivalent of the
c  ALGOL program they present.
c
c  Input/Output:
c    nn		The degree of the poly. 
c    a		The poly coefficients.  Destroyed by the algorithm.
c    ifail	Should be zero on normal entry and exit, but see above.
c
c  Output:
c    roots	The roots of the polynomial.
c--
c-----------------------------------------------------------------
	integer maxdeg
	real tol
	parameter(maxdeg=100)
	integer i, k, n
	real r, j, rx, jx, sig, t, scale, g, tol2, s1, s2, s, x, y
	real fun, nfun, fac, imagtmp
	real b(0:maxdeg), c(0:maxdeg)
	logical sat, flag
c
c  Initialize.
c
	tol = 2.**(-24)
	fac = 1.00
	flag = ifail.ne.0
	ifail = 0
	tol2 = tol * sqrt(tol)
	n = nn
	if(a(0).eq.0.0 .or. n.lt.1 .or. n.gt.maxdeg)then
	  ifail = 1
c  goto ifail
	  goto 8000
	endif
c
c  zerotest:
  	do while( a(n).eq.0 )
	  roots(n) = (0.,0.)
	  n = n - 1
	enddo
c
c  normalisation:
  30	continue
	scale = 0.0
	do i=0,n
	  if( abs(a(i)) .ge. 1e-5) scale = scale + log( abs( a(i) ) )
	enddo
	k = scale/((n+1) * log(2.))
	scale = 2.**(-k)
	do i=0,n
	  a(i) = a(i) * scale
	  b(i) = a(i)
	enddo
c
c  Test for low order polynomial for explicit solution.
c
c  goto (finish,linear,quadratic)
	if(n.le.2)goto (8000,7980,7990),n+1
c
c  schur test:
  65	continue
	do i=n,1,-1
	  do k=1,i
	    c(k-1) = b(i) * b(k) - b(0) * b(i-k)
	  enddo
c if (??) goto search
	  if(c(i-1) .lt. -tol) goto 100
	  if(c(i-1).lt.1.0)then
	    t = 1.
	  else
	    t = 1./c(i-1)
	  endif
	  do k=i-1,0,-1
	    b(k) = c(k) * t
	  enddo
	enddo
c End of schur test for zero in unit circle.
c
c  Transformation.
c
	fac = 2. * fac
	scale = 1.
	do i=n-1,0,-1
	  scale = 2* scale
	  a(i) = a(i) * scale
	  b(i) = a(i)
	enddo
c goto schur test
	goto 65
c
c search:
 100	continue		
	if(flag)then
	  x = real(roots(1))
	  y = aimag(roots(1)) + tol
	  flag = .false.
	else
	  x = 1e-3
	  y = 0.1
	endif
c
	call rpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
	fun = r*r + j*j
c
c again:
 130	continue
	g = rx*rx + jx*jx
	if(g.lt.fun*tol2)then
	  ifail = 2
	  scale = 1.0
	  do i=n-1,0,-1
	    scale = scale * fac
	    a(i) = a(i)/scale
	  enddo
c
c goto fail with possible saddle point detected;
c
	  goto 8010
	endif			
	s1 = -(r*rx + j*jx)/g
	s2 =  (r*jx - j*rx)/g
	sig = 2e-4
	s = sqrt(s1*s1 + s2*s2)
	if(s.gt.1)then
	  s1 = s1/s
	  s2 = s2/s
	  sig = sig/s
	endif
c
c  Valid direction of search has been determined, now proceed to determine
c  suitable step.
c
	x = x + s1
	y = y + s2
c
c
c Loop:
 170	continue
	call  rpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
	if(.not.sat)then
	  nfun = r*r + j*j
	  if( fun-nfun.lt.sig*fun)then
	    s1 = 0.5 * s1
	    s2 = 0.5 * s2
	    s  = 0.5 * s
	    sig= 0.5 * sig
	    x = x -s1
	    y = y - s2
c goto loop
	    goto 170
	  endif
	  fun = nfun
c goto again
	  goto 130
	endif
c
c  New root
c
	fun = 1./tol2
	k = 0
	imagtmp = y * fac
c if(??) goto complex
	if( abs(y).gt.0.1)goto 200
c
c  Check possibility of real root.
c
	s1 = y
	y = 0
	call  rpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
	y = s1
c if(??) goto complex
	if(.not.sat)goto 200
c
c  Real root accepted and both backward and forward deflations
c  are performed with linear factor.
c
	roots(n) = cmplx( x * fac, 0.)
	n = n - 1
	b(0) = a(0)
	c(n) = -a(n+1)/x
	do i=1,n
	  b(i) = a(i) + x * b(i-1)
	  c(n-i) = (c(n-i+1) - a(n-i+1))/x
	enddo
c goto join
	goto 220
c
c  Complex root accepted and both backward and forward deflations are
c  performed with quadratic factor.
c
c complex:
 200	continue
	roots(n-1) = cmplx( x * fac, imagtmp )
	roots(n)   = conjg( roots(n-1) )
	n = n - 2
	r = 2 * x
	j = -(x*x + y*y)
	b(0) = a(0)
	b(1) = a(1) + r*b(0)
	c(n) = -a(n+2)/j
	c(n-1) = -(a(n+1) + r*c(n))/j
	do i=2,n
	  b(i) = a(i) + r*b(i-1) + j*b(i-2)
	  c(n-i) = -(a(n-i+2) - c(n-i+2) + r*c(n-i+1))/j
	enddo
c
c  Matching point for composite deflation.
c
c join:
 220	continue
	do i=0,n
	  nfun = abs(b(i)) + abs(c(i))
	  if(nfun.gt.tol)then
	    nfun = abs(b(i)-c(i))/nfun
	    if(nfun.lt.fun)then
	      fun = nfun
	      k = i
	    endif
	  endif
	enddo
c
	do i=k-1,0,-1
	  a(i) = b(i)
	enddo
	a(k) = 0.5*( b(k) + c(k) )
	do i=k+1,n
	  a(i) = c(i)
	enddo
	goto 30
c
c  Linear
c
c Linear:
 7980	continue
	roots(1) = cmplx( -a(1)/a(0)*fac, 0.)
c goto finish
	goto 8000
c
c  Quadratic.
c
c quadratic:
 7990	continue
	r = a(1) * a(1) - 4.0*a(0)*a(2)
	if(r.le.0)then 
	  roots(1) = 
     *		cmplx(-0.5 * a(1)/a(0) * fac, 0.5*sqrt(-r)/a(0) * fac )
	  roots(2) = conjg(roots(1))
	else
	  roots(1) = 
     *	    cmplx(0.5*(-a(1) - sign(sqrt(r),a(1))) / a(0) * fac, 0.)
	  roots(2) =
     *	    cmplx( a(2)/(real(roots(1)) * a(0)) * fac * fac, 0.)
	endif
c
c  Finished.
c
c finish:
 8000	continue
	n = 0
c
c  Failed.
c
 8010	continue
	nn = n
	end
c*******************************************************************
	subroutine rpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
c
	implicit none
	integer n
	real x,y,r,rx,j,jx,a(0:n),tol
	logical sat
c
c Evaluates r, rx, j, jx at the point x+jy and applies the Adams test.
c The boolean variabele SAT is given the value .true. if the test is
c satisfied.
c
c--------------------------------------------------------------------
	real p,q,a1,a2,a3,b1,b2,b3,c,t
	integer k,m
	p = -2.*x
	q = x*x + y*y
	t = sqrt(q)
	b2 = 0.
	a2 = 0.
	b1 = a(0)
	a1 = b1
	c = abs(a1) * 0.8
	m = n - 2
	do k=1,m
	  a3 = a2
	  a2 = a1
	  a1 = a(k) - p*a2 - q*a3
	  c = t * c + abs(a1)
	  b3 = b2
	  b2 = b1
	  b1 = a1 - p * b2 - q * b3
	enddo
	a3 = a2
	a2 = a1
	a1 = a(n-1) - p * a2 - q * a3
	r = a(n) + x * a1 - q * a2
	j = a1*y
	rx = a1 - 2. * b2 * y * y
	jx = 2. * y * (b1 - x * b2)
	c = t * (t * c + abs(a1)) + abs(r)
	sat = sqrt(r*r+j*j).lt.(2.*abs(x*a1)-8.*(abs(r)+abs(a1) * t)
     *				     + 10.*c) * tol * (1+tol)**(4*n+3)
	end
c*************************************************************************
c*Dpolyzr -- Roots of a real polynomial.
c:polynomials
c+
      subroutine dpolyzr(a,nn,roots,ifail)
c
      implicit none
      integer nn,ifail
      complex roots(nn)
      double precision a(0:nn)
c
c  This procedure attempts to solve a real polynomial equation in degree nn
c  using the technique suggested by Grant and Hitchins (1971) to limiting
c  machine precision. On entry, the coefficients of the polynomial are
c  held in the array a(0:nn), with a(0) holding the coefficient of the
c  highest power. On normal entry, the parameter ifail has value zero,
c  and will remain zero on successful exit, with the calculated estimates
c  of the roots held in roots(1:nn), in approximately decreasing order
c  of magnitude. The parameter "tol" givers the precision of the floating
c  binary arithmetic used, normally 2**(-t), where "t" is the number
c  of bits in the mantissa.
c
c  Abnormal exits will be indicated by ifail having a value 1 or 2. The
c  former implieds that either a(0) = 0 or nn < 1. For ifail=2, a
c  possible saddle point has been detected. The degree of the reduced poly
c  is stored in nn and its coefficients are held in a(0:nn), the roots
c  obtained thus far are held in roots(nn+1) onwards. An immediate
c  re-entry is possible with ind unchanged and with a new starting
c  point for the search held in roots(1).
c
c  The algorithm has been copies from:
c    Grant J.A., Hitchins G.D., (1973 or 1974) "Two algorithms for the
c    solution of polynomial equations to limiting machine precision"
c    The Computer Journal, vol 18, n3, pp 258-264
c
c  Apart from cosmetic changes, it is an FORTRAN equivalent of the
c  ALGOL program they present.
c
c  Input/Output:
c    nn		The degree of the poly. 
c    a		The poly coefficients.  Destroyed by the algorithm.
c    ifail	Should be zero on normal entry and exit, but see above.
c
c  Output:
c    roots	The roots of the polynomial.
c--
c-----------------------------------------------------------------
	integer maxdeg
	double precision tol
	parameter(maxdeg=100)
	integer i, k, n
	double precision r,j,rx,jx,sig,t,scale,g,tol2,s1,s2,s,x,y
	double precision fun, nfun, fac, imagtmp
	double precision b(0:maxdeg), c(0:maxdeg)
	logical sat, flag
c
c  Initialize.
c
	tol = 2.**(-48)
	fac = 1.00
	flag = ifail.ne.0
	ifail = 0
	tol2 = tol * sqrt(tol)
	n = nn
	if(a(0).eq.0.0 .or. n.lt.1 .or. n.gt.maxdeg)then
	  ifail = 1
c  goto ifail
	  goto 8000
	endif
c
c  zerotest:
  	do while( a(n).eq.0 )
	  roots(n) = (0.,0.)
	  n = n - 1
	enddo
c
c  normalisation:
  30	continue
	scale = 0.0
	do i=0,n
	  if( abs(a(i)) .ge. 1e-5) scale = scale + log( abs( a(i) ) )
	enddo
	k = scale/((n+1) * log(2.))
	scale = 2.**(-k)
	do i=0,n
	  a(i) = a(i) * scale
	  b(i) = a(i)
	enddo
c
c  Test for low order polynomial for explicit solution.
c
c  goto (finish,linear,quadratic)
	if(n.le.2)goto (8000,7980,7990),n+1
c
c  schur test:
  65	continue
	do i=n,1,-1
	  do k=1,i
	    c(k-1) = b(i) * b(k) - b(0) * b(i-k)
	  enddo
c if (??) goto search
	  if(c(i-1) .lt. -tol) goto 100
	  if(c(i-1).lt.1.0)then
	    t = 1.
	  else
	    t = 1./c(i-1)
	  endif
	  do k=i-1,0,-1
	    b(k) = c(k) * t
	  enddo
	enddo
c End of schur test for zero in unit circle.
c
c  Transformation.
c
	fac = 2. * fac
	scale = 1.
	do i=n-1,0,-1
	  scale = 2* scale
	  a(i) = a(i) * scale
	  b(i) = a(i)
	enddo
c goto schur test
	goto 65
c
c search:
 100	continue		
	if(flag)then
	  x = real(roots(1))
	  y = aimag(roots(1)) + tol
	  flag = .false.
	else
	  x = 1e-3
	  y = 0.1
	endif
c
	call dpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
	fun = r*r + j*j
c
c again:
 130	continue
	g = rx*rx + jx*jx
	if(g.lt.fun*tol2)then
	  ifail = 2
	  scale = 1.0
	  do i=n-1,0,-1
	    scale = scale * fac
	    a(i) = a(i)/scale
	  enddo
c
c goto fail with possible saddle point detected;
c
	  goto 8010
	endif			
	s1 = -(r*rx + j*jx)/g
	s2 =  (r*jx - j*rx)/g
	sig = 2e-4
	s = sqrt(s1*s1 + s2*s2)
	if(s.gt.1)then
	  s1 = s1/s
	  s2 = s2/s
	  sig = sig/s
	endif
c
c  Valid direction of search has been determined, now proceed to determine
c  suitable step.
c
	x = x + s1
	y = y + s2
c
c
c Loop:
 170	continue
	call dpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
	if(.not.sat)then
	  nfun = r*r + j*j
	  if( fun-nfun.lt.sig*fun)then
	    s1 = 0.5 * s1
	    s2 = 0.5 * s2
	    s  = 0.5 * s
	    sig= 0.5 * sig
	    x = x -s1
	    y = y - s2
c goto loop
	    goto 170
	  endif
	  fun = nfun
c goto again
	  goto 130
	endif
c
c  New root
c
	fun = 1./tol2
	k = 0
	imagtmp = y * fac
c if(??) goto complex
	if( abs(y).gt.0.1)goto 200
c
c  Check possibility of real root.
c
	s1 = y
	y = 0
	call  dpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
	y = s1
c if(??) goto complex
	if(.not.sat)goto 200
c
c  Real root accepted and both backward and forward deflations
c  are performed with linear factor.
c
	roots(n) = cmplx( real(x * fac), 0.)
	n = n - 1
	b(0) = a(0)
	c(n) = -a(n+1)/x
	do i=1,n
	  b(i) = a(i) + x * b(i-1)
	  c(n-i) = (c(n-i+1) - a(n-i+1))/x
	enddo
c goto join
	goto 220
c
c  Complex root accepted and both backward and forward deflations are
c  performed with quadratic factor.
c
c complex:
 200	continue
	roots(n-1) = cmplx( x * fac, imagtmp )
	roots(n)   = conjg( roots(n-1) )
	n = n - 2
	r = 2 * x
	j = -(x*x + y*y)
	b(0) = a(0)
	b(1) = a(1) + r*b(0)
	c(n) = -a(n+2)/j
	c(n-1) = -(a(n+1) + r*c(n))/j
	do i=2,n
	  b(i) = a(i) + r*b(i-1) + j*b(i-2)
	  c(n-i) = -(a(n-i+2) - c(n-i+2) + r*c(n-i+1))/j
	enddo
c
c  Matching point for composite deflation.
c
c join:
 220	continue
	do i=0,n
	  nfun = abs(b(i)) + abs(c(i))
	  if(nfun.gt.tol)then
	    nfun = abs(b(i)-c(i))/nfun
	    if(nfun.lt.fun)then
	      fun = nfun
	      k = i
	    endif
	  endif
	enddo
c
	do i=k-1,0,-1
	  a(i) = b(i)
	enddo
	a(k) = 0.5*( b(k) + c(k) )
	do i=k+1,n
	  a(i) = c(i)
	enddo
	goto 30
c
c  Linear
c
c Linear:
 7980	continue
	roots(1) = -a(1)/a(0)*fac
c goto finish
	goto 8000
c
c  Quadratic.
c
c quadratic:
 7990	continue
	r = a(1) * a(1) - 4.0*a(0)*a(2)
	if(r.le.0)then 
	  roots(1) = 
     *		cmplx(-0.5 * a(1)/a(0) * fac, 0.5*sqrt(-r)/a(0) * fac )
	  roots(2) = conjg(roots(1))
	else
	  roots(1) = 0.5*(-a(1) - sign(sqrt(r),a(1))) / a(0) * fac
	  roots(2) = a(2)/(real(roots(1)) * a(0)) * fac * fac
	endif
c
c  Finished.
c
c finish:
 8000	continue
	n = 0
c
c  Failed.
c
 8010	continue
	nn = n
	end
c*******************************************************************
	subroutine dpolsolr(tol,x,y,r,rx,j,jx,a,n,sat)
c
	implicit none
	integer n
	double precision x,y,r,rx,j,jx,a(0:n),tol
	logical sat
c
c Evaluates r, rx, j, jx at the point x+jy and applies the Adams test.
c The boolean variabele SAT is given the value .true. if the test is
c satisfied.
c
c--------------------------------------------------------------------
	double precision p,q,a1,a2,a3,b1,b2,b3,c,t
	integer k,m
	p = -2.*x
	q = x*x + y*y
	t = sqrt(q)
	b2 = 0.
	a2 = 0.
	b1 = a(0)
	a1 = b1
	c = abs(a1) * 0.8
	m = n - 2
	do k=1,m
	  a3 = a2
	  a2 = a1
	  a1 = a(k) - p*a2 - q*a3
	  c = t * c + abs(a1)
	  b3 = b2
	  b2 = b1
	  b1 = a1 - p * b2 - q * b3
	enddo
	a3 = a2
	a2 = a1
	a1 = a(n-1) - p * a2 - q * a3
	r = a(n) + x * a1 - q * a2
	j = a1*y
	rx = a1 - 2. * b2 * y * y
	jx = 2. * y * (b1 - x * b2)
	c = t * (t * c + abs(a1)) + abs(r)
	sat = sqrt(r*r+j*j).lt.(2.*abs(x*a1)-8.*(abs(r)+abs(a1) * t)
     *				     + 10.*c) * tol * (1+tol)**(4*n+3)
	end

