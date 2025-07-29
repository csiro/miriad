c
c  Some subroutines to do Hanning smoothing of an array
c
c  History:
c    nebk 25may89 Original version.
c    rjs   8sep89 Improved documentation.
c    nebk 11apr91 Changed HANNSM to overwrite input with output
c                 Compact algorithm in RFACT and add test for overflow
c                 Changed HCOEFFS to remove maximum number of
c                 coefficients restriction.  It will fail in RFACT 
c                 if asked for too big a number
c    bpw  26sep91 Add boxcar smoothing
c    nebk 28oct92 Rewrite binomial coefficients with gamma function
c                 removing RFACT along the way.
c    nebk 03nov92 Break GAMMLN into GAMMA.FOR with some other gamma
c                 function subroutines
c************************************************************************
c* Hcoeffs -- Calculated coefficients for Hanning smoothing.
c& nebk
c: smoothing,convolution
c+
      subroutine hcoeffs (nsmth, coeffs)
c
      implicit none
      integer nsmth
      real coeffs(nsmth)
c
c  Work out binomial weights or coefficients for Hanning smooth
c
c  Input:
c    nsmth   i    Smoothing length
c  Output:
c    coeffs  r    Weights for data points
c--
c---------------------------------------------------------------------------
      integer n, k, i
      real sum
c--------------------------------------------------------------------------
c
c Make smoothing length odd
c
      if (nsmth.le.2) then
        nsmth = 1
      else if (mod(nsmth,2).eq.0) then
        nsmth = nsmth + 1
        call bug ('w',
     *            'Increasing smoothing length by 1 to an odd number')
      end if
c
      if (nsmth.ge.3) then
c
c Get coefficients
c
        n = nsmth - 1
        sum = 0.0
        do i = 1, nsmth
          k = i - 1
          call bico (n, k, coeffs(i))
          sum = coeffs(i) + sum
        end do
c
c Normalize
c
        do i = 1, nsmth
          coeffs(i) = coeffs(i) / sum
        end do
      end if
c
      end
c
c
c************************************************************************
c* BiCo -- Find binomial coefficients
c& nebk
c: binomial,utilities
c+
      subroutine bico (n, k, c)
c
      implicit none
      integer n, k
      real c
c
c  Compute binomial coefficent. 
c  Algorithm from Numerical Recipes, p 158
c
c         /  \
c         | n |       n!
c         |   |  = --------
c         |   |     k!(n-k)!
c         | k |   
c         \  /
c
c  Input:
c    n,k     i     Binomial arguments
c  Output:
c    c       r     Result
c--
c-------------------------------------------------------------------------
      real factln
c------------------------------------------------------------------------
      c = nint(exp(factln(n)-factln(k)-factln(n-k)))
c
      end
c
c***********************************************************************
c* FactLn -- Find ln(n!)
c& nebk
c: factorial,utilities
c+
      real function factln (n)
c
      implicit none
      integer n
c
c  Find ln(n!) with Gamma function.   
c  Algorithm from Numerical Rbecipes, p 159
c
c  Input:
c    n      i     Input
c--
c-------------------------------------------------------------------------
      real a(100), gammln
c
      save a
      data a /100*-1.0/
c------------------------------------------------------------------------
      if (n.lt.0) call bug ('f', 'Negative argument in FACTLN')
c
      if (n.le.99) then
        if (a(n+1).lt.0) a(n+1) = gammln(n+1.0)
        factln = a(n+1)
      else
        factln = gammln(n+1.0)
      end if
c
      end
c
c
c************************************************************************
c* Hannsm -- Hanning smooth a data array..
c& nebk
c: smoothing,convolution
c+
      subroutine hannsm (nsmth, coeffs, npts, arr, work)
c
      implicit none
      integer npts, nsmth
      real arr(npts), work(nsmth), coeffs(nsmth)
c
c  Hanning smooth an array.
c
c  Input:
c    nsmth    i    Smoothing length.  SHould be odd integer.
c    coeffs   r    Weights.
c    npts     i    Number of data points.
c  Input/output:
c    arr      r    Data array.  Contains smoothed result on output
c    work     r    Work array. Must be at least of size NSMTH
c--
c-------------------------------------------------------------------------
      real sum
      integer half, i, j, k, j1, j2
c--------------------------------------------------------------------------
      if (nsmth.lt.3) return
c
      half = (nsmth - 1) / 2
      j1 = 1
      j2 = 1
c
      do i = 1, npts
c
c Compute smoothed output point and do something with ends
c
        k = 1
        sum = 0.0
        do j = i-half, i+half
          if (j.lt.1) then
            sum = sum + coeffs(k)*arr(1)
          else if (j.gt.npts) then
            sum = sum + coeffs(k)*arr(npts)
          else
            sum = sum + coeffs(k)*arr(j)
          end if
          k = k + 1
        end do
c
c Assign smoothed point to temporary cyclic buffer
c
        work(j1) = sum
c
c Copy smoothed point to input array when no longer needed as input
c
        if (i.ge.half+1) then
           arr(i-half) = work(j2)
c
           j2 = j2 + 1
           if (j2.gt.nsmth) j2 = 1
        end if
c
        j1 = j1 + 1
        if (j1.gt.nsmth) j1 = 1
      end do
c
c Copy remaining smoothed points into input
c
      do j = npts-half+1, npts
        arr(j) = work(j2)
c
        j2 = j2 + 1
        if (j2.gt.nsmth) j2 = 1
      end do
c
      end
c
c
c************************************************************************
c* Bcoeffs -- Calculate coefficients for boxcar smoothing.
c& bpw
c: smoothing,convolution
c+
      subroutine bcoeffs( nsmth, coeffs )
c
      implicit none
      integer nsmth
      real coeffs(*)
c
c  Work out coefficients for boxcar smooth
c
c  Input:
c    nsmth   i    Smoothing length
c  Output:
c    coeffs  r    Weights for data points
c--
c------------------------------------------------------------------------
      integer i, j
      real weight
c------------------------------------------------------------------------
      weight = nsmth
      if (mod(nsmth,2).eq.1) then
        j = 1
      else
        nsmth = nsmth + 1
        coeffs(1) = 0.0
        j = 2
      end if
      do i = j, nsmth
        coeffs(i) = 1.0 / weight
      end do
c
      end
c************************************************************************
c* Boxcarsm -- Boxcar smooth a data array..
c& bpw
c: smoothing,convolution
c+
      subroutine boxcarsm (nsmth, coeffs, npts, arr, work)
c
      implicit none
      integer npts, nsmth
      real arr(npts), work(nsmth), coeffs(nsmth)
c
c  Boxcar smooth an array 
c
c  Input:
c    nsmth    i    Smoothing length.
c    coeffs   r    Weights set by BCOEFFS
c    npts     i    Number of data points.
c  Input/output:
c    arr      r    Data array.  Contains smoothed result on output
c    work     r    Work array. Must be at least of size NSMTH
c--
c------------------------------------------------------------------------
      call hannsm (nsmth, coeffs, npts, arr, work)
c
      end

