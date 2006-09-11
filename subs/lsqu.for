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
c    rjs 05mar98   Removed nllsqu to a separate file, to work around a 
c		   LINUX problem.
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
c* linlsq - return parameters of a straight line fit
c: least-squares
c& bpw
c+
      subroutine linlsq( xarr,yarr,npnt, a1,b1,a2,b2, sigx,sigy,corr )
c
      real           xarr(*)
      real           yarr(*)
      integer        npnt
      real           a1, a2, b1, b2
      real           sigx, sigy, corr
c
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
c------------------------------------------------------------------------
      double precision sumx, sumy, sumsqx, sumsqy, sumxy
      real x, y
      integer i
c
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
c
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
c
      end
