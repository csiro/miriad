c************************************************************************
c*J1xByx -- Calculate j1(x)/x
c:special-functions,bessel-functions
c*
	real function j1xbyx(arg)
c
	implicit none
	real arg
c
c   J1xByx calculates the bessel function,  j1(x)/x  from the
c   polynomial approximations. See Abramowitz and Stegun, Handbook of
c   Mathematical Functions, sections 9.4.4 and 9.4.6, page 370.
c
c  Input:
c    arg	The value of x.
c  Output:
c    j1xbyx	The calculated value of j1(x)/x
c--
c  rjs   ???89 
c  nebk  10sep89  Install missing ENDIF. 
c------------------------------------------------------------------c
      real f,x,t
c
c  For the range -3.le.arg.le.+3, the maximum error is 1.3 e-8.
c
      if(abs(arg).lt.3)then
	X = (ARG/3.)*(ARG/3.)
	J1XBYX = 0.5 + X*(	- 0.56249985
     *		     + X*(	+ 0.21093573
     *		     + X*(	- 0.03954289
     *		     + X*(	+ 0.00443319
     *		     + X*(	- 0.00031761
     *		     + X*(	+ 0.00001109))))))
c
c   for the range  abs(arg).gt.3.  , max. error is 1. e-7
c
      else
	X = 3./ABS(ARG)
	F = .79788456	+ X*( +.00000156
     *			+ X*( +.01659667
     *			+ X*( +.00017105
     *			+ X*( -.00249511
     *			+ X*( +.00113653
     *			+ X*( -.00020033 ))))))
	T = ARG - 2.35619449
     *			+ X*( .12499612
     *			+ X*( .00005650
     *			+ X*(-.00637879
     *			+ X*( .00074348
     *			+ X*( .00079824
     *			+ X*(-.00029166 ))))))
	J1XBYX = F*COS(T)/ARG/SQRT(ARG)
      end if
c
      END
