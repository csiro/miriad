c* ExpUn -- Find exponential and don't underflow
c& nebk
c: utilities
c+
      real function ExpUn (x)
c
      implicit none
      real x
c
c Find the exponential of a number but try to avoid underflow messages
c
c  Input
c    x     r     Argument
c--
c
c-----------------------------------------------------------------------
      real under
      parameter (under = -70.0)
c-----------------------------------------------------------------------
      if (x.le.under) then
        expun = 0.0
      else
        expun = exp(x)
      end if
c
      end
c
c
c* DExpUn -- Find exponential and don't underflow
c& nebk
c: utilities
c+
      double precision function DExpUn (x)
c
      implicit none
      double precision x
c
c Find the exponential of a number but try to avoid underflow messages
c
c  Input
c    x     d     Argument
c--
c
c-----------------------------------------------------------------------
      double precision under
      parameter (under = -70.0)
c-----------------------------------------------------------------------
      if (x.le.under) then
        dexpun = 0.0
      else
        dexpun = exp(x)
      end if
c
      end

