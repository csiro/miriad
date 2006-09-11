c************************************************************************
c  History:
c     nov75      Written by D.E. Amos and S.L. Daniel.
c                Modified by A.H. Morris
c    9sep93 rjs  Stolen from NSWC library and further modified.
c   16dec94 rjs  Included cin.
c   11apr97 rjs  Added dsinc function.
c
c  Bugs:
c    si and cin should really be call dsi and dcin.
c************************************************************************
c* si - Sine integral
c& rjs
c: utilities
c+
      double precision function si(x)
c
      implicit none
      double precision x
c
c  si computes the integral of sin(t)/t on (0,x) by means
c  of Chebyshev expansions on (0,5) and (5,infinity).
c
c  Inputs:
c    x		Limit of integration.
c  Output:
c    si		Value of the integral.
c
c  Accuracy:
c    2 units of the 14th significant figure.
c
c  References
c    sand76-006c
c    The special functions and their approximations, vol. ii, by
c    Y.L. Luke. Academic Press, New York, 1969.
c--
c------------------------------------------------------------------------
      include 'mirconst.h'
      double precision pio2
      parameter(pio2=0.5d0*dpi)
      integer n1,n2,m1,m2
      parameter(n1=16,n2=46,m1=14,m2=21)
c
      double precision bb(n1),cc(n2)
c
      double precision ax,bx,tx,b1,b2,temp,aic,rc,amax
      integer j,i
c     -------------------
      data bb(1) / 6.84101190850653d-01/, bb(2) /-3.74538448460062d-01/,
     1     bb(3) /-2.82656062651542d-02/, bb(4) / 3.06078454012071d-02/,
     2     bb(5) /-8.99242948380352d-04/, bb(6) /-1.09884251456048d-03/,
     3     bb(7) / 5.81151604367358d-05/, bb(8) / 2.28802638122969d-05/,
     4     bb(9) /-1.35078982929539d-06/, bb(10)/-3.13213946132892d-07/,
     5     bb(11)/ 1.86619586786257d-08/, bb(12)/ 3.03991719607226d-09/,
     6     bb(13)/-1.76437788946489d-10/, bb(14)/-2.20236421792690d-11/,
     7     bb(15)/ 1.22710107703240d-12/, bb(16)/ 1.23680681116783d-13/
c     -------------------
      data cc(1) / 9.76155271128712d-01/, cc(2) / 8.96845854916423d-02/,
     1     cc(3) /-3.04656658030696d-02/, cc(4) / 8.50892472922945d-02/,
     2     cc(5) /-5.78073683148386d-03/, cc(6) /-5.07182677775691d-03/,
     3     cc(7) / 8.38643256650893d-04/, cc(8) /-3.34223415981738d-04/,
     4     cc(9) /-2.15746207281216d-05/, cc(10)/ 1.28560650086065d-04/,
     5     cc(11)/-1.56456413510232d-05/, cc(12)/-1.52025513597262d-05/,
     6     cc(13)/ 4.04001013843204d-06/, cc(14)/-5.95896122752160d-07/,
     7     cc(15)/-4.34985305974340d-07/, cc(16)/ 7.13472533530840d-07/,
     8     cc(17)/-5.34302186061100d-08/, cc(18)/-1.76003581156610d-07/,
     9     cc(19)/ 3.85028855125900d-08/, cc(20)/ 1.92576544441700d-08/,
     1     cc(21)/-1.00735358217200d-08/, cc(22)/ 3.36359194377000d-09/,
     2     cc(23)/ 1.28049619406000d-09/, cc(24)/-2.42546870827000d-09/,
     3     cc(25)/ 1.86917288950000d-10/, cc(26)/ 7.13431298340000d-10/,
     4     cc(27)/-1.70673483710000d-10/, cc(28)/-1.14604070350000d-10/,
     5     cc(29)/ 5.88004411500000d-11/, cc(30)/-6.78417843000000d-12/,
     6     cc(31)/-1.21572380900000d-11/, cc(32)/ 1.26561248700000d-11/,
     7     cc(33)/ 4.74814180000000d-13/, cc(34)/-5.32309477000000d-12/,
     8     cc(35)/ 9.05903810000000d-13/, cc(36)/ 1.40046450000000d-12/,
     9     cc(37)/-5.00968320000000d-13/, cc(38)/-1.80458040000000d-13/
      data cc(39)/ 1.66162910000000d-13/, cc(40)/-5.02616400000000d-14/,
     1     cc(41)/-3.48453600000000d-14/, cc(42)/ 4.60056600000000d-14/,
     2     cc(43)/ 5.74000000000000d-16/, cc(44)/-1.95310700000000d-14/,
     3     cc(45)/ 3.68837000000000d-15/, cc(46)/ 5.62862000000000d-15/
c     -------------------
c     ****** amax is a machine dependent constant. it is assumed that
c            sin(x) and cos(x) are defined for  abs(x) .le. amax, and
c            that pio2 - (1 + 1/x)/x = pio2 for x .gt. amax.
c
      amax = 1.0e5
c
      ax=abs(x)
      if (ax.gt.5.0) go to 20
      j=n1
      bx=0.40*ax-1.0
      tx=bx+bx
      b1=bb(j)
      b2=0.
      do 10 i=1,m1
      j=j-1
      temp=b1
      b1=tx*b1-b2+bb(j)
   10 b2=temp
      si=(bx*b1-b2+bb(1))*x
      return
c
   20 if (ax.gt.amax) go to 50
      bx=10./ax-1.
      tx=bx+bx
      j=n2
      b1=cc(j)
      b2=0.0
      do 30 i=1,m2
      j=j-2
      temp=b1
      b1=tx*b1-b2+cc(j)
   30 b2=temp
      aic=bx*b1-b2+cc(2)
c
      j=n2-1
      b1=cc(j)
      b2=0.0
      do 40 i=1,m2
      j=j-2
      temp=b1
      b1=tx*b1-b2+cc(j)
   40 b2=temp
      rc=bx*b1-b2+cc(1)
c
      si=(rc*cos(ax)+aic*sin(ax))/ax
      si=pio2-si
      if (x.lt.0.0) si=-si
      return
c
   50 si=sign(pio2,x)
c
      end
c************************************************************************
c* cin - Cosine integral
c& rjs
c: utilities
c+
	double precision function cin(x)
c
	implicit none
	double precision x
c
c     written by d.e. amos and s.l. daniel, november, 1975.
c     modified by a.h. morris
c
c     references
c         sand76-0062
c         the special functions and their approximations, vol. ii, by
c         y.l. luke. academic press, new york, 1969.
c
c     Abstract
c         cin computes the integral of (1-cos(t))/t on (0,x) by means
c         of chebyshev expansions on (0,5) and (5,infinity).
c
c     Description of arguments
c
c         input
c           x      - limit of integration, unrestricted
c
c         output
c           cin    - value of the integral
c--
c------------------------------------------------------------------------
      double precision econ
      integer n1,n2,m1,m2
      parameter(n1=16,n2=46,m1=14,m2=21)
      parameter(econ= 5.77215664901533e-01)
c
      double precision ax,bx,tx,temp,b1,b2,rc,aic,amax
      integer i,j
      double precision bb(n1),cc(n2)
c
c     -------------------
      data bb(1) / 1.82820351064538e-01/, bb(2) /-8.23768704567135e-02/,
     1     bb(3) /-1.03468764544958e-02/, bb(4) / 5.05085201960312e-03/,
     2     bb(5) / 5.73772812356328e-05/, bb(6) /-1.42717916181096e-04/,
     3     bb(7) / 2.89263664732599e-06/, bb(8) / 2.43068098304909e-06/,
     4     bb(9) /-7.90337487433443e-08/, bb(10)/-2.80205535437371e-08/,
     5     bb(11)/ 1.05488738052065e-09/, bb(12)/ 2.34186901801115e-10/,
     6     bb(13)/-9.27762554764014e-12/, bb(14)/-1.48682586858284e-12/,
     7     bb(15)/ 5.95210263082868e-14/, bb(16)/ 7.42057835287916e-15/
c     -------------------
      data cc(1) / 9.76155271128712e-01/, cc(2) / 8.96845854916423e-02/,
     1     cc(3) /-3.04656658030696e-02/, cc(4) / 8.50892472922945e-02/,
     2     cc(5) /-5.78073683148386e-03/, cc(6) /-5.07182677775691e-03/,
     3     cc(7) / 8.38643256650893e-04/, cc(8) /-3.34223415981738e-04/,
     4     cc(9) /-2.15746207281216e-05/, cc(10)/ 1.28560650086065e-04/,
     5     cc(11)/-1.56456413510232e-05/, cc(12)/-1.52025513597262e-05/,
     6     cc(13)/ 4.04001013843204e-06/, cc(14)/-5.95896122752160e-07/,
     7     cc(15)/-4.34985305974340e-07/, cc(16)/ 7.13472533530840e-07/,
     8     cc(17)/-5.34302186061100e-08/, cc(18)/-1.76003581156610e-07/,
     9     cc(19)/ 3.85028855125900e-08/, cc(20)/ 1.92576544441700e-08/,
     1     cc(21)/-1.00735358217200e-08/, cc(22)/ 3.36359194377000e-09/,
     2     cc(23)/ 1.28049619406000e-09/, cc(24)/-2.42546870827000e-09/,
     3     cc(25)/ 1.86917288950000e-10/, cc(26)/ 7.13431298340000e-10/,
     4     cc(27)/-1.70673483710000e-10/, cc(28)/-1.14604070350000e-10/,
     5     cc(29)/ 5.88004411500000e-11/, cc(30)/-6.78417843000000e-12/,
     6     cc(31)/-1.21572380900000e-11/, cc(32)/ 1.26561248700000e-11/,
     7     cc(33)/ 4.74814180000000e-13/, cc(34)/-5.32309477000000e-12/,
     8     cc(35)/ 9.05903810000000e-13/, cc(36)/ 1.40046450000000e-12/,
     9     cc(37)/-5.00968320000000e-13/, cc(38)/-1.80458040000000e-13/
      data cc(39)/ 1.66162910000000e-13/, cc(40)/-5.02616400000000e-14/,
     1     cc(41)/-3.48453600000000e-14/, cc(42)/ 4.60056600000000e-14/,
     2     cc(43)/ 5.74000000000000e-16/, cc(44)/-1.95310700000000e-14/,
     3     cc(45)/ 3.68837000000000e-15/, cc(46)/ 5.62862000000000e-15/
c     -------------------
c     ****** amax is a machine dependent constant. it is assumed that
c            sin(x) and cos(x) are defined for  abs(x) .le. amax, and
c            that econ + ln(x) - (1 + 1/x)/x = econ + ln(x)
c            for x .gt. amax.
c
      amax = 1.0e5
c
c
      ax = abs(x)
      if (ax.gt.5.0) go to 20
      j=n1
      bx=0.40*ax-1.0
      tx=bx+bx
      b1=bb(j)
      b2=0.
      do 10 i=1,m1
      j=j-1
      temp=b1
      b1=tx*b1-b2+bb(j)
   10 b2=temp
      cin=x*x*(bx*b1-b2+bb(1))
      return
c
   20 if (ax.gt.amax) go to 50
      bx=10./ax-1.0
      tx=bx+bx
      j=n2
      b1=cc(j)
      b2=0.0
      do 30 i=1,m2
      j=j-2
      temp=b1
      b1=tx*b1-b2+cc(j)
   30 b2=temp
      aic=bx*b1-b2+cc(2)
c
      j=n2-1
      b1=cc(j)
      b2=0.0
      do 40 i=1,m2
      j=j-2
      temp=b1
      b1=tx*b1-b2+cc(j)
   40 b2=temp
      rc=bx*b1-b2+cc(1)
c
      cin=(rc*sin(ax)-aic*cos(ax))/ax
      cin=(econ-cin)+log(ax)
      return
c
   50 cin=econ+log(ax)
      return
      end
c************************************************************************
	double precision function dsinc(x)
c
	implicit none
	double precision x
c
c  Return the value of Bracewell's sinc function.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision t
c
c  For small x, use a simple Taylor series expansion.
c
	if(abs(x).lt.0.05)then
	  t = x * x
	  dsinc = 1 - (1.644934067 - 0.811742426*t)*t
c
c  Otherwise use the sine function.
c
	else
	  t = DPI*x
	  dsinc = sin(t)/t
	endif
c
	end

