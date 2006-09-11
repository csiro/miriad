c  lspoly, legdr, matinv, valpol
c-----------------------------------------------------------------------
c* lspoly -- least squares polynomial fit 
c& mchw
c: polynomials
c+
      subroutine lspoly(nn,l,x,y,w,z,coeff)
c
      implicit none
      integer nn,l
      real x(l),y(l),z(l),w(l),coeff(9)
c
c	Least-squares polynomial fit of degree NN, up to 8, for L data
c	points [X,Y] w. weights W.
c
c	Fit is with Legendre polynomials of argument XL=C*(X-XM),
c	where XM is midpoint of extreme range of X, and C is a
c	factor that converts ends of range to +1 and -1.
c	Coefficients returned are those of power series, however.
c	COEFF(J) is coefficient of (X-x(1))**(J-1). 
c	Also returned are Z(I),	values of the polynomial at X(I).
c	Calling routine must dimension COEFF at least NN+1.
c
c	To evaluate polynomials, see also VALPOL.
c
c--
c	Requires subr. LEGDR subr. MATINV and function VALPOL,
c	all attached. Originally from the Berkeley (VMS) UTL.OLB 
c	library
c
c  History:
c	(By I.R. King and S. Djorgovski,  Univ. of Calif.,Berkeley)
c	fixed bugs in the following:	 19 Jan 1986 - MCHW
c
c		1) zero order fit
c		2) large x with small range
c		3) coeff not initialized
c
c    mchw  --apr93 Extracted from a task and installed as a sub.
c    mjs   22apr93 Added in-code docs.
c    pjt   23apr93 Eliminate flint complaints.
c    pjt   14dec93 Documented X-X(1) offsets !!!
c
c-----------------------------------------------------------------------
      real a(10,10),d(9),xmp(9),bc(45),t(25),p(10),free(10)
      integer i,j,k,n,nm,j1,ie,ib
      real xfirst,xlo,xhi,xmid,xfac,xl,prod,csum,fpow,xmpv,sum
      real valpol,determ
c
c	'T' are coefficients of powers in Lagrange poly's arranged
c	in order in which they will be needed.
c
      DATA T/1.,-.5,.375,-.3125,.2734375,1.,-1.5,1.875,-2.1875,1.5,
     1  -3.75,6.5625,-9.84375,2.5,-8.75,19.6875,4.375,-19.6875,
     2  54.140625,7.875,-43.3125,14.4365,-93.84375,26.8125,50.273437/
c
c	'BC' are binomial coefficients, arranged in order needed.
c
      DATA BC/1.,-1.,1.,-1.,1.,-1.,1.,-1.,1.,1.,-2.,3.,-4.,5.,-6.,7.,
     1       -8.,1.,-3.,6.,-10.,15.,-21.,28.,1.,-4.,10.,-20.,35.,-56.,
     2       1.,-5.,15.,-35.,70.,1.,-6.,21.,-56.,1.,-7.,28.,1.,-8.,1./
c
c  Initialize.
c
	n = min0( nn + 1, 9)
	do i=1,9
	  coeff(i)=0.
	enddo
	do i=1,l
	  z(i)=0.
	enddo
c
c  Zero-order fit.
c
      if(n.le.0) then	
	coeff(1)=0.0
	coeff(2)=0.0
	do i = 1,l
	  coeff(1) = coeff(1) + y(i)*w(i)
	  coeff(2) = coeff(2) + w(i)
	enddo
	if(coeff(2).ne.0.) coeff(1) = coeff(1)/coeff(2)
	do i = 1,l
	  z(i) = coeff(1)
	enddo
	return
c
c  Undetermined system.
c
      else if (l.lt.n) then
	call bug('w',' lspoly> undetermined system !')
	return
      else
        nm = n+1
      endif
c
c  Find range which will be mapped on (-1,1).
c
      xhi = 0.
      xlo = 0.
      xfirst = x(1)
      do i=1,l
	  x(i) = x(i) - xfirst		
	  if(x(i).gt.xhi) xhi=x(i)
	  if(x(i).lt.xlo) xlo=x(i)
      enddo
      xmid=0.5*(xhi+xlo)
      xfac=2./(xhi-xlo)
c
c  Form the normal equations.
c	a(i,j) is augmented normal matrix, n by nm.
c
      do j=1,nm
        do i=1,n
          a(i,j)=0.
        enddo
      enddo
      do k=1,l
	xl = xfac*(x(k)-xmid)
	call legdr(xl,p,n)
	do i=1,n
	  prod = w(k)*p(i)
	  do j=1,i
	    a(i,j)=a(i,j)+prod*p(j)
          enddo
	  a(i,nm)=a(i,nm)+prod*y(k)
        enddo
      enddo
      do j=1,nn
	j1 = j+1
	do i=j1,n
	a(j,i) = a(i,j)
        enddo
      enddo
c
c  Form the free param. vector for eqs. solving.
c
      do i=1,n
	free(i) = a(i,nm)
      enddo
c
c  Now solve normal eqs.
c
      call matinv(a,free,n,10,determ)
c
c	FREE(K) now contains the coefficients of Legendre poly's,
c	which are now converted to coeff's of powers of XL=XFAC*(X-XMID).
c
      ib=1
      do j=1,n
	  ie=ib+(n-j)/2
	  csum=0.
	  k=j-2
	  do i=ib,ie
	    k=k+2
	    csum=csum+free(k)*t(i)
          enddo
      d(j)=csum
      ib=ib+(11-j)/2
      enddo
c
c  The d(i) get multiplied by xfac**(i-1), xmp(i) are xmid**(i-1)
c
      fpow=xfac
      xmpv=xmid
      xmp(1)=1.
      do i=2,n
	  xmp(i)=xmpv
	  xmpv=xmpv*xmid
	  d(i)=d(i)*fpow
	  fpow=fpow*xfac
      enddo
      j=1
      do i=1,n
         sum=0.
	 do k=i,n
	    sum=sum+d(k)*bc(j)*xmp(k-i+1)
	    j=j+1
         enddo
         coeff(i)=sum
         j=j+9-n
      enddo
c
c  Evaluate polynomial at the data points:
c
      do i=1,l
	  z(i) = valpol(x(i),coeff,n)
	  x(i) = x(i) + xfirst
      enddo
      end
c-----------------------------------------------------------------------
c* legdr -- legendre polynomial utility
c& mchw
c: math,polynomials
c+
      subroutine legdr(x,p,np)
c
      implicit none
      integer np
      real x,p(np)
c
c  Will put values of first NP Legendre polynomials of X in array P.
c  Max NP is 9 (i.e., up to 8th order). P(N) is (N-1)th order
c  BUG:  p(np) dangerous if compiled with run time array checking (-C)
c
c--
c    History:
c
c    mchw  --apr93 Extracted from a task and installed as a sub.
c    mjs   22apr93 Added in-code docs.
c    pjt   23apr93 Eliminated flint complaints.
c----------------------------------------------------------------------c
c
      integer n
      real c(9),r(9)
c
c	P(1),P(2),P(3),... are 0th, 1st, 2nd, ... order Legendre
c	polynomials.
c
      DATA c/1.5,1.66666667,1.75,1.8,1.83333333,1.85714286,1.875,
     *       1.88888889,1.9/
      DATA r/.5,.666667,.75,.8,.833333,.85714286,.875,.8888889,.9/
c
      p(1)=1
      if(np.eq.1) return
      p(2)=x
      if(np.eq.2) return
      if(np.gt.10) np=10
      do n=3,np
	p(n)=(c(n-2)*x*p(n-1)-r(n-2)*p(n-2))
      enddo
      end
c-----------------------------------------------------------------------
c*matinv -- single precision matrix inversion
c& mchw
c: math
c+
      subroutine matinv(a,b,n,d,determ)   
c
      implicit none
      integer n,d
      real a(d,n),b(n),determ
c       
c  This is a single precision version of matrix inversion with accom-
c   panying solution of linear equations.     
c   a(n,n) is the matrix to be inverted. the inverse is returned into 
c   the same array. b(n) is the constant vector, i.e. a(n,n)*x(n)=b(n)
c   the solution x(n) is returned into array b(n). the determinant  of
c   a as determ. Arrays a(d,d) and b(n) must be dimensioned in the    
c   main program.      **** note: dimension a(d,d) in main. ****     
c--
c       changed 12-aug-83 to omit those error messages.
c    History:
c
c    mchw  --apr93 Extracted from a task and installed as a sub.
c    mjs   22apr93 Added in-code docs.
c    pjt   23apr93 Eliminated flint complaints.
c-----------------------------------------------------------------------
      real    pivot(100),d1,amax,swap,t
      integer ipivot(100),idx(100,2),i,j,k,l,irow,icolum        
      integer jrow,jcolum,l1
c
c  Initialization    
c
      determ=1. 
      do j=1,n       
        ipivot(j)=0       
      enddo
c
c  Search for pivotal element
c
      do i=1,n       
        amax=0.   
        do j=1,n
          if (ipivot(j).ne.1) then
            do k=1,n       
              d1 = a(j,k) 
              if (ipivot(k).gt.1) return
              if (ipivot(k).lt.1) then
                if ( abs(amax) .lt. abs(d1)) then
                  irow = j    
                  icolum = k  
                  amax = a(j,k)       
                endif
              endif
            enddo
          endif
        enddo

        if (amax.eq.0.) then
          determ = 0.0
          return
        endif
        ipivot(icolum)=ipivot(icolum)+1   
c
c  Interchange rows to put pivot element on diagonal 
c
        if (irow.ne.icolum) then
          determ = -determ    
          do l=1,n       
            swap = a(irow,l)    
            a(irow,l) = a(icolum,l)     
            a(icolum,l) = swap  
          enddo
          swap=b(irow)      
          b(irow) = b(icolum) 
          b(icolum) = swap    
        endif
        idx(i,1) = irow   
        idx(i,2) = icolum 
        pivot(i) = a(icolum,icolum) 
        determ=determ*pivot(i)    
c
c  Divide pivot row by pivot element 
c
        a(icolum,icolum)=1.       
        do  l=1,n       
          a(icolum,l)=a(icolum,l)/pivot(i)  
        enddo
        b(icolum) = b(icolum)/pivot(i)      
c
c  Reduce non-pivotal rows   
c
        do l1=1,n      
          if (l1.ne.icolum) then
            t=a(l1,icolum)    
            a(l1,icolum)=0.   
            do l=1,n       
              a(l1,l)=a(l1,l)-a(icolum,l)*t     
            enddo
            b(l1)=b(l1)-b(icolum)*t   
          endif
        enddo
      enddo
c
c  Interchange columns       
c
      do i=1,n       
        l=n+1-i   
        if (idx(l,1).ne.idx(l,2)) then
          jrow=idx(l,1)   
          jcolum=idx(l,2) 
          do k=1,n       
            swap=a(k,jrow)    
            a(k,jrow)=a(k,jcolum)     
            a(k,jcolum)=swap  
          enddo
        endif
      enddo
      end       
c***********************************************************************
c* valpol -- Evaluate powers-of-x polynomial at the point x
c& mchw
c: polynomials
c+
      real function valpol(x,coeff,n)
c
      implicit none
      integer n
      real x, coeff(n)
c
c  Evaluate powers-of-x polynomial at the point x.
c  polynomial order is n-1.
c       Input:
c        x       point to evaluate polynomial at
c        n       length of coeff array
c        coeff   polynomial coefficients for x**(0..n-1)
c       Output:
c        valpol  value of polynomial
c
c
c       Warning: If COEFF are the coefficients as returned by LSPOLY,
c       the input value X needs to be relative to it's first point X(1).
c       See LSPOLY.
c
c--
c    History:
c
c    mchw  --apr93 Extracted from a task and installed as a sub.
c    mjs   22apr93 Added in-code doc marks.
c    pjt   14dec93 documented LSPOLY link and it's dangers
c********1*********2*********3*********4*********5*********6*********7*c
      integer i
      valpol = coeff(n)
      do i=1,n-1
         valpol = valpol*x+coeff(n-i)
      enddo
      end
c********1*********2*********3*********4*********5*********6*********7*c
