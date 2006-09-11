c
c  Some functions/subroutines to do with gamma functions. AN asterisk
c  means it is user callable, else its an internal subroutine.
c  These are coded from the Numerical Recipes algorithms.
c
c *  Errfun -  Find error function Erf(x)
c *  GammLn -  Find natural log of Gamma function gamma(x)
c *  GammP  -  Find incomplete Gamma function P(a,x)
c *  GammQ  -  Find incomplete Gamma function Q(a,x) = 1 - P(a,x)
c    GSer   -  Find P with series approximation
c    GCF    -  Find Q with continued fraction approximation
c  
c
c  History:
c    nebk 03nov92 Original version.
c    nebk 27apr93 Rename erf to errfun
c    nebk 12nov93 Prevent some underflow messages on exponentials
c    nebk 06jan94 Call new ExpUN for underflow messages
c
c************************************************************************
c* GammP -- Find incomplete Gamma function P(a,x)
c& nebk
c: gamma function, utilities
c+
      real function gammp (a, x)
c
      implicit none
      real a, x
c
c Find the incomplete Gamma function P(a,x)
c Algorithm from Numerical Recipes p 161
c
c  Input
c    a,x     r     Arguments
c--
c
c-----------------------------------------------------------------------
      real gamser, gln, gammcf
c-----------------------------------------------------------------------
      if (x.lt.0.0 .or. a.le.0.0) call bug ('f',
     +  'Argument out of range in GammP')
c
      if (x.lt.a+1.0) then
c
c Use series representation
c
        call gser (a, x, gamser, gln)
        gammp = gamser
      else
c
c Use continued fraction representation
c
        call gcf (a, x, gammcf, gln)
        gammp = 1.0 - gammcf
      end if
c
      end
c
c************************************************************************
c* GammQ -- Find incomplete Gamma function Q(a,x) = 1 - P(a,x)
c& nebk
c: gamma function, utilities
c+
      real function gammq (a, x)
c
      implicit none
      real a, x
c
c Find the incomplete Gamma function Q(a,x) = 1 - P(a,x)
c Algorithm from Numerical Recipes p 162
c
c  Input
c    a,x     r     Arguments
c--
c
c-----------------------------------------------------------------------
      real gamser, gln, gammcf
c-----------------------------------------------------------------------
      if (x.lt.0.0 .or. a.le.0.0) call bug ('f',
     +  'Argument out of range in GammP')
c
      if (x.lt.a+1.0) then
c
c Use series representation
c
        call gser (a, x, gamser, gln)
        gammq = 1.0 - gamser
      else
c
c Use continued fraction representation
c
        call gcf (a, x, gammcf, gln)
        gammq = gammcf
      end if
c
      end
c
c************************************************************************
c
      subroutine gser (a, x, gamser, gln)
c
      implicit none
      real a, x, gamser, gln
c
c Find the incomplete Gamma function P(a,x) with its series
c represenation. Algorithm from Numerical Recipes p 162
c
c  Input
c    a,x     r     Arguments
c  Output
c    gamser  r     P
c    gln     r     ln(Gamma(a))
c
c-----------------------------------------------------------------------
      integer itmax
      real eps
      parameter (itmax = 100, eps = 3.0e-7)
c
      real ap, sum, del
      integer i
c
      real gammln, arg, expun
c-----------------------------------------------------------------------
      gln = gammln(a)
c
      if (x.lt.0.0) then
        call bug ('f', 'Argument out of range in GSer')
      else if (x.eq.0.0) then
        gamser = 0.0
      else
        ap = a
        sum = 1.0 / a
        del = sum
        do i = 1, itmax
          ap = ap + 1.0
          del = del * x / ap
          sum = sum + del
          if (abs(del).lt.abs(sum)*eps) goto 100
        end do
        call bug ('f', 'No convergence in GSer')
c
100     arg = -x + a*log(x) - gln
        gamser = sum*expun(arg)
      end if
c
      end
c
c************************************************************************
c
      subroutine gcf (a, x, gammcf, gln)
c
      implicit none
      real a, x, gammcf, gln
c
c Find the incomplete Gamma function Q(a,x) = 1 - P(a,x) with its 
c continued fraction represenation. Algorithm from Numerical 
c Recipes p 162
c
c  Input
c    a,x     r     Arguments
c  Output
c    gammcf  r     Q(a,x)
c    gln     r     ln(Gamma(a))
c
c-----------------------------------------------------------------------
      integer itmax
      real eps
      parameter (itmax = 100, eps = 3.0e-7)
c
      real gold, a0, a1, b0, b1, fac, an, ana, anf, g
      integer i
c
      real gammln, arg, expun
c-----------------------------------------------------------------------
      gln = gammln(a)
c
      gold = 0.0
      a0 = 1.0
      a1 = x
      b0 = 0.0
      b1 = 1.0
      fac = 1.0
c
      do i = 1, itmax
        an = real(i)
        ana = an - a
        a0 = (a1 + a0*ana) * fac
        b0 = (b1 + b0*ana) * fac
        anf = an * fac
        a1 = x*a0 + anf*a1
        b1 = x*b0 + anf*b1
        if (a1.ne.0.0) then
          fac = 1.0 / a1
          g = b1 * fac
          if (abs((g-gold)/g).lt.eps) goto 100
          gold = g
        end if
      end do
      call bug ('f', 'No convergence in GCF')
c
100   arg = -x + a*log(x) - gln
      gammcf = expun(arg) * g
c
      end
c
c************************************************************************
c* GammLn -- Find ln(Gamma(x))
c& nebk
c: gamma function, utilities
c+
      real function gammln(xx)
c
      implicit none
      real xx
c
c Find ln(Gamma(x)) where Gamma is the Gamma function
c Algorithm from Numerical Recipes p 157
c
c  Input
c    xx     r     Argument
c--
c
c-----------------------------------------------------------------------
      double precision cof(6), stp, fpf, x, tmp, ser
      integer j
c
      save cof, stp
      data cof /76.18009173d0, -86.50532033d0,   24.01409822d0,
     +          -1.231739516d0,  0.120858003d-2, -0.536382d-5/
      data stp /2.50662827465d0/
      data fpf / 5.5d0/
c-----------------------------------------------------------------------
      x = xx - 1.0
      tmp = x + fpf
      tmp = (x + 0.5)*log(tmp) - tmp
      ser = 1.0
      do j = 1, 6
        x = x + 1.0
        ser = ser + cof(j)/x
      end do
      gammln = tmp + log(stp*ser)
c
      end
c
c
c************************************************************************
c* Errfun -- Find Erf(x)
c& nebk
c: error function, gamma function, utilities
c+
      real function errfun (x)
c
      implicit none
      real x
c
c Find Erf(x) from Gamma function
c Algorithm from Numerical Recipes p 164
c
c  Input
c    x     r     Argument
c--
c
c-----------------------------------------------------------------------
      real gammp
c-----------------------------------------------------------------------
      if (x.lt.0.0) then
        errfun = -gammp(0.5,x**2)
      else
        errfun =  gammp(0.5,x**2)
      end if
c
      end



