c  History:
c    nebk 05nov92 Original version.
c
c************************************************************************
c* lsf -- Least squares fit to y = mx + b 
c& nebk
c: least-squares
c+
      subroutine lsf (noerr, n, x, y, w, m, b, sigm, sigb, chisq, q)
c-----------------------------------------------------------------------
c     Least squares fit, y = mx + b  
c
c     Algorithm from numerical recipes P 508
c
c  Input
c    noerr True if there are no errors associated with the data
c    n     Number of data points
c    x,y   Data 
c    w     Weights; should be 1/sig_i**2 where sig_i is the error in y_i
c  Output
c    m     Fitted slope 
c    b     Fitted Y intercept
c    sigm  Error in m
c    sigb  Error in b
c    chisq Chi squared
c    q     Goodness of fit. If there were no input errors then it
c          is assumed that Q=1.0, and then SIGM and SIGB are
c          worked out.  You can't have an independent estimate of
c	   q and sigm,sigb unless you input the errors.  Even with
c	   errors, we can't work out q unless N > 2. Again q is assumed 
c	   to be 1.0 if N <= 2.
c--
c
c-----------------------------------------------------------------------
      implicit none
c
      integer n
      real x(n), y(n), w(n), m, b, sigm, sigb, chisq, q
      logical noerr
cc
      double precision sx, sy, sw, sxoss, st2, t, dm, db, dsigm,
     +dsigb, sigdat, dchisq, dq
      integer i
c
      real gammq
c-----------------------------------------------------------------------
c
c Find sums
c
      sx = 0.0
      sy = 0.0       
      sw = 0.0
      do i = 1, n
        sx = sx + w(i)*x(i)
        sy = sy + w(i)*y(i)
        sw = sw + w(i)
      end do
c
      sxoss = sx / sw
      dm = 0.0
      st2 = 0.0
      do i = 1, n
        t = (x(i) - sxoss) * sqrt(w(i))
        st2 = st2 + t*t
        dm = dm + t*y(i)*sqrt(w(i))
      end do
c
c Compute results
c
      dm = dm / st2
      db = (sy - sx*dm) / sw
      dsigm = sqrt(1.0/st2)
      dsigb = sqrt((1.0 + sx*sx/(sw*st2)) / sw)
c
c Find chi squared
c
      dchisq = 0.0
      do i = 1, n
        dchisq = dchisq + ((y(i) - db - dm*x(i))*sqrt(w(i)))**2
      end do
c
c Find goodness of fit
c
      if (noerr) then
c
c Assume perfect if no input errors
c
        dq = 1.0
        if (n.gt.2) then
c
c Adjust errors if we can
c
          sigdat = sqrt(dchisq/(n-2))
          dsigm = dsigm * sigdat
          dsigb = dsigb * sigdat
        end if
      else
c
c Independent goodness of fit can be calculated here
c
        if (n.gt.2) then
          dq = dble(gammq(0.5*(n-2),0.5*real(dchisq)))
        else
c
c Couldn't work it out so assume its perfect
c
          dq = 1.0
        end if
      end if
c
c Assign single precision results
c
      m = dm
      b = db
      sigm = dsigm
      sigb = dsigb
      q = dq
      chisq = dchisq
c
      end
