c
c  Work out some Bessel functions. 
c  Currently I0 and I1 only.
c
c  History:
c    nebk 24aug92 Original version.
c    nebk 27aug92 Use symmetries to expand argument range
c    bmg/rjs 29may96 Added BessJ0.
c************************************************************************
c* BessI0 -- Compute modified Bessel function I0
c& nebk
c: utilities,numbers
c+
      double precision function bessi0 (x)
c
      implicit none
      double precision x
c
c     Compute modified Bessel function I0(x) with polynomial
c     approximation.   Valid for all arguments.
c     Error is < 1.6e-7 for |x|< 3.75, else < 1.9e-7
c
c     Algorithm from Handbook of Mathematical Functions, P 378
c
c  Input
c    x       Argument
c  Output
c    bessi0  Result
c--
c---------------------------------------------------------------
      double precision t, a1(7), a2(9), tsq, tm1, xx
c
      save a1, a2
      data a1 /1.0, 3.5156229, 3.0899424, 1.2067492,
     +         0.2659732,  0.0360768, 0.0045813/
      data a2 /0.39894228,  0.01328592,  0.00225319,
     +        -0.00157565,  0.00916281, -0.02057706,
     +         0.02635537, -0.01647633,  0.00392377/
c---------------------------------------------------------------
      xx = abs(x)
      t = xx / 3.75
c
      if (xx.le.3.75) then
        tsq = t * t
        bessi0 = a1(1)+tsq*(a1(2)+tsq*(a1(3)+tsq*(a1(4)+tsq*(a1(5)+
     +           tsq*(a1(6)+tsq*a1(7))))))
      else
        tm1 = 1.0 / t
        bessi0 = a2(1)+tm1*(a2(2)+tm1*(a2(3)+tm1*(a2(4)+tm1*(a2(5)+
     +           tm1*(a2(6)+tm1*(a2(7)+tm1*(a2(8)+tm1*a2(9))))))))
        bessi0 = bessi0 * exp(xx) / sqrt(xx)
      end if
c
      end
c************************************************************************
c* BessI1 -- Compute modified Bessel function I1
c& nebk
c: utilities,numbers
c+
      double precision function bessi1 (x)
c
      implicit none
      double precision x
c
c     Compute modified Bessel function I1(x) with polynomial
c     approximation.   Valid for all arguments.
c     Error is < 8e-9 for |x| < 3.75, else < 2.2e-7
c
c     Algorithm from Handbook of Mathematical Functions, P 378
c
c  Input
c    x       Argument
c  Output
c    bessi1  Result
c--
c---------------------------------------------------------------
      double precision t, a1(7), a2(9), tsq, tm1, xx
      integer s
c
      save a1, a2
      data a1 /0.5, 0.87890594, 0.51498869, 0.15084934,
     +         0.02658733, 0.00301532, 0.00032411/
      data a2 /0.39894228,-0.03988024,-0.00362018,
     +         0.00163801,-0.01031555, 0.02282967,
     +        -0.02895312, 0.01787654,-0.00420059/
c---------------------------------------------------------------
      s = 1
      if (x.lt.0.0) s = -1
      xx = abs(x)
      t = xx / 3.75 
c
      if (xx.le.3.75) then
        tsq = t * t
        bessi1 = a1(1)+tsq*(a1(2)+tsq*(a1(3)+tsq*(a1(4)+tsq*(a1(5)+
     +           tsq*(a1(6)+tsq*a1(7))))))
        bessi1 = s * bessi1 * xx
      else
        tm1 = 1.0 / t
        bessi1 = a2(1)+tm1*(a2(2)+tm1*(a2(3)+tm1*(a2(4)+tm1*(a2(5)+
     +           tm1*(a2(6)+tm1*(a2(7)+tm1*(a2(8)+tm1*a2(9))))))))
        bessi1 = s * bessi1 * exp(xx) / sqrt(xx)
      end if
c
      end
c************************************************************************
c*BessJ0 -- Calculate j0(x)
c:special-functions,bessel-functions
c*
	double precision function bessj0(arg)
c
	implicit none
	double precision arg
c
c   BESSJ0 calculates the bessel function,  j0(x) from the
c   polynomial approximations. See Abramowitz and Stegun, Handbook of
c   Mathematical Functions, sections 9.4.1 and 9.4.3, pages 369-370.
c
c  Input:
c    arg	The value of x.
c  Output:
c    bessj0	The calculated value of j0(x)
c--
c  rjs   ???89 
c  nebk  10sep89  Install missing ENDIF. 
c  bmg   22may96  Modified j1xbyx to work for j0x
c------------------------------------------------------------------c
      double precision f,x,t
c
c  For the range -3.le.arg.le.+3, the maximum error is 5 e-8.
c
      if(abs(arg).lt.3)then
	X = (ARG/3.)*(ARG/3.)
	BESSJ0 = 1.0
     *		  + X*(	- 2.2499997
     *	          + X*(	+ 1.2656208
     *	          + X*(	- 0.3163866
     *	          + X*(	+ 0.0444479
     *	          + X*(	- 0.0039444
     *	          + X*(	+ 0.0002100))))))
c
c   for the range  abs(arg).gt.3.  , max. error is 7 e-8
c
      else
	X = 3./ABS(ARG)
	F = .79788456	+ X*( -.00000077
     *			+ X*( -.00552740
     *			+ X*( -.00009512
     *			+ X*( +.00137237
     *			+ X*( -.00072805
     *			+ X*( +.00014476 ))))))
	T = ARG - 0.78539816
     *			+ X*(-.04166397
     *			+ X*(-.00003954
     *			+ X*(+.00262573
     *			+ X*(-.00054125
     *			+ X*(-.00029333
     *			+ X*(+.00013558 ))))))
	BESSJ0 = F*COS(T)/SQRT(ARG)
      end if
c
      END
	
