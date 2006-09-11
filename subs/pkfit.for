c************************************************************************
c* pkfit -- Fit to the location of the peak pixel.
c:
c& rjs
c+
      subroutine pkfit(z, width, zmax, pix, c)
      implicit none
c
      integer width
      real z(width,width), c(6), zmax
      double precision pix(2)
c
c  Fit a parabola to the function in Z.
c
c  Input:
c    Width      Width of the support of Z.
c    Z          Function values.
c
c  Output:
c    C          Coefficients of the fit.
c    zmax       Estimated max value of the function.
c    pix        Location, relative to the central pixel, of the maximum
c--
c   History
c    23jul99 rjs  Extract this subroutine from maxfit.
c
c-----------------------------------------------------------------------
      integer maxwidth
      parameter(maxwidth = 5)
c
      real array(6,maxwidth*maxwidth), hw, rtemp(36), denom, x, y
      integer i, j, k, itemp(6), ifail
c
c  Fill in the coefficients of the array.
c
      k = 0
      hw = 0.5 * (width + 1)
      do j = 1, width
        y = j - hw
c
        do i=1,width
          k = k + 1
          x = i - hw
          array(1,k) = 1
          array(2,k) = x
          array(3,k) = y
          array(4,k) = x*y
          array(5,k) = x*x
          array(6,k) = y*y
        end do
      end do
c
c  This is a linear problem. Call a routine to solve it.
c
      call llsqu (z, array, 6, width*width, c, ifail, rtemp, itemp)
      if (ifail.ne.0) call bug ('f','Least squares solution failed')
c
c  Work out the location of the maxima, and the value of the function there.
c
      denom  = c(4)*c(4) - 4*c(5)*c(6)
      if (denom.ne.0.0) then
        pix(1) = (2*c(2)*c(6)-c(3)*c(4)) / denom
        pix(2) = (2*c(3)*c(5)-c(2)*c(4)) / denom
      else
        call bug ('f', 
     +    'Least squares solution failed -- all pixels identical ?')
      end if
      zmax = c(1) + c(2)*pix(1) + c(3)*pix(2) + 
     +       c(4)*pix(1)*pix(2) + c(5)*pix(1)*pix(1) +
     +       c(6)*pix(2)*pix(2)
c
      end
