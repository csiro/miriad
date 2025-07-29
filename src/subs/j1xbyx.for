c************************************************************************
c  History:
c    rjs   ???89 
c    nebk  10sep89  Install missing ENDIF. 
c    rjs   24aug01  Added jinc and chat functions.
c    dpr   27aug01  Declare your variables rjs!
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
c************************************************************************
	real function jinc(x)
c
	implicit none
	real x
c
c  Bracewell's jinc function = J1(pi*x)/(2*x)
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	real j1xbyx
c
	jinc = 0.5*PI*j1xbyx(PI*x)
	end
c************************************************************************
	real function chat(x)
c
	implicit none
	real x
c
c  Polynomial approximation to Bracewell's Chinese hat - chat - function.
c
c  chat(x) = 0.5*(acos(x)-abs(x)*sqrt(1-x*x))
c
c  This is the autocorrelation of uniform disks
c
c------------------------------------------------------------------------
	real t
	t = abs(x)
	if(t.gt.1)then
	  chat = 0
	else
	  chat = 0.78639 + t * (-1.02669 
     *			 + t * ( 0.15909
     *			 + t * (-0.16691 
     *			 + t *   0.24491 )))
	endif
	end
