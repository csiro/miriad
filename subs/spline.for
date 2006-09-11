c***********************************************************************
c     From Forsyth et al. (sources can be obtained from netlib)
c     coding style changed a bit into miriad format
c***********************************************************************
c* spline -- setup for cubic spline interpolation
c& pjt
c: interpolation
c+
      subroutine spline (n, x, y, b, c, d)
      implicit none
      integer n
      double precision x(n), y(n), b(n), c(n), d(n)
c
c
c  The coefficients b(i), c(i), and d(i), i=1,2,...,n are computed
c  for a cubic interpolating spline
c
c    s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
c
c    for  x(i) .le. x .le. x(i+1)
c
c  input..
c
c    n = the number of data points or knots (n.ge.2)
c    x = the abscissas of the knots in strictly increasing order
c    y = the ordinates of the knots
c
c  output..
c
c    b, c, d  = arrays of spline coefficients as defined above.
c
c  using  p  to denote differentiation,
c
c    y(i) = s(x(i))
c    b(i) = sp(x(i))
c    c(i) = spp(x(i))/2
c    d(i) = sppp(x(i))/6  (derivative from the right)
c
c  the accompanying function subprogram  ``seval''  can be used
c  to evaluate the spline.
c
c  Origin: spline routines from Forsyth et al. (see netlib - 1992)
c
c-----------------------------------------------------------------------
      integer nm1, ib, i
      double precision t
c-----------------------------------------------------------------------
      nm1 = n-1
      if ( n .lt. 2 ) return
      if ( n .lt. 3 ) then
         b(1) = (y(2)-y(1))/(x(2)-x(1))
         c(1) = 0.
         d(1) = 0.
         b(2) = b(1)
         c(2) = 0.
         d(2) = 0.
         return
      end if
c
c  set up tridiagonal system
c
c  b = diagonal, d = offdiagonal, c = right hand side.
c
      d(1) = x(2) - x(1)
      c(2) = (y(2) - y(1))/d(1)
      do i = 2, nm1
         d(i) = x(i+1) - x(i)
         b(i) = 2.*(d(i-1) + d(i))
         c(i+1) = (y(i+1) - y(i))/d(i)
         c(i) = c(i+1) - c(i)
      end do
c
c  end conditions.  third derivatives at  x(1)  and  x(n)
c  obtained from divided differences
c
      b(1) = -d(1)
      b(n) = -d(n-1)
      c(1) = 0.
      c(n) = 0.

      if (n.ne.3) then
         c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
         c(n) = c(n-1)/(x(n)-x(n-2)) - c(n-2)/(x(n-1)-x(n-3))
         c(1) = c(1)*d(1)**2/(x(4)-x(1))
         c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
      end if
c
c  forward elimination
c
      do i = 2, n
         t = d(i-1)/b(i-1)
         b(i) = b(i) - t*d(i-1)
         c(i) = c(i) - t*c(i-1)
      end do
c
c  back substitution
c
      c(n) = c(n)/b(n)
      do ib = 1, nm1
         i = n-ib
         c(i) = (c(i) - d(i)*c(i+1))/b(i)
      end do
c
c  c(i) is now the sigma(i) of the text
c
c  compute polynomial coefficients
c
      b(n) = (y(n) - y(nm1))/d(nm1) + d(nm1)*(c(nm1) + 2.*c(n))
      do i = 1, nm1
         b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2.*c(i))
         d(i) = (c(i+1) - c(i))/d(i)
         c(i) = 3.*c(i)
      end do
      c(n) = 3.*c(n)
      d(n) = d(n-1)
c
      end
c***********************************************************************
c* seval -- Evaluate a cubic spline
c& pjt
c: interpolation
c+
      double precision function seval (n, u, x, y, b, c, d)
      implicit none
      integer n
      double precision  u, x(n), y(n), b(n), c(n), d(n)
c
c  this subroutine evaluates the cubic spline function
c
c    seval = y(i) + b(i)*(u-x(i)) + c(i)*(u-x(i))**2 + d(i)*(u-x(i))**3
c
c    where  x(i) .lt. u .lt. x(i+1), using horner's rule
c
c  if  u .lt. x(1) then  i = 1  is used.
c  if  u .ge. x(n) then  i = n  is used.
c
c  input..
c
c    n = the number of data points
c    u = the abscissa at which the spline is to be evaluated
c    x,y = the arrays of data abscissas and ordinates
c    b,c,d = arrays of spline coefficients computed by spline
c
c  if  u  is not in the same interval as the previous call, then a
c  binary search is performed to determine the proper interval.
c
c-----------------------------------------------------------------------
      integer i, j, k
      double precision dx
      save i
      data i/1/
c-----------------------------------------------------------------------
      if ( i .ge. n ) i = 1
      if ( u .lt. x(i) ) goto 10
      if ( u .le. x(i+1) ) goto 30
c
c  binary search
c
   10 continue
      i = 1
      j = n+1
   20 continue
      k = (i+j)/2
      if ( u .lt. x(k) ) j = k
      if ( u .ge. x(k) ) i = k
      if ( j .gt. i+1 ) goto 20
c
c  evaluate spline
c
   30 continue
      dx = u - x(i)
      seval = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
c
      end
c

