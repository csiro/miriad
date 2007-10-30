c************************************************************************
c
c  These are a collection of routines used for Zeeman analysis. In
c  particular, it finds maximum likelihood and least squares solutions
c  to the estimate of alpha, beta and the sigmas of these parameters.
c
c  Some LinPack routines are used to solve various systems of linear
c  equations. ZedScale calls various Miriad header handling routines.
c  Many subroutines call the infamous "bug" routine to indicate a
c  fatal error, or warning.
c
c  History:
c    rjs   jun89 Original version.
c    nebk  jun89 Fine subroutine ZedScale.
c    rl	 15jun89 Replaced abs(cdelt) with cdelt, in ZedScale.
c    rjs 23jun89 Included ZedScale in this file. Improved Zed's algorithm.
c		 Eliminated I0 parameter from all calls.
c    rjs 29jun89 Made some of the summations double precision, to avoid
c		 rounding problems in cases of good signal to noise.
c    rjs 30jun89 Fixed sigma estimate for 2 sided derivative, in Zed.
c    rjs  5jul89 Added routine ZedIHat, deleted routine ZedSig.
c    rjs  9jul89 Added ZedFudge and ZedRho to this file. Corrected sigma
c		 estimates in ZedLsq.
c    rjs 12jul89 Fixed bug in ZedWind to do with ZedFudge handling too
c		 many channels.
c    rjs 14jul89 Fixed bug in ZedDi to do with ZedFudge handling spatial
c		 correlations in two dimensions.
c    rl  17jul89 Fixed sign problems (again!).  This time added minus in
c		 velocity to frequency conversion so increasing velocity goes
c		 to decreasing frequency, as actually happens in nature.
c    rjs 28jul89 Added debiased least squares to Zed. 
c    nebk15sep89 Added ZedVHat
c    rjs 25jan90 Minor documentation improvement.
c    rjs 10mar93 Use maxdim.h and MemAlloc/MemFree.
c    rjs 25jun93 Fudges to ZedFudge to make it more robust (but still
c		 give poor fudge factors).
c    nebk01jul94 Improve zedscale to correctly work out the frequency 
c                increment from optical as well as radio velocity axes
c    nebk20aug94 Use new cocvt routines in zedscale in favour of spaxsw
c  Routines:
c    ZedScale Returns the conversion factor between channel increment and
c	      magnetic field.
c    Zed      This finds the channel shift which minimises the misfit.
c    ZedFunc  This returns the misfit for given alpha and beta.
c    ZedIHat  This returns the estimate of IHat, given alpha and beta.
c    ZedVHat  This returns the estimate of VHat, given alpha, beta and IHat
c    ZedFudge Determine sigma fudge factor, when handling correlated noise.
c    ZedRho   This does a correct max likelihood treatment in the case of
c	      correlation between adjacent channels.
c
c************************************************************************
c* ZedScale -- Determine conversion factor from channels to magnetic field.
c& nebk
c: Zeeman
c+
      subroutine ZedScale (lunI, freq, scale, noline)
c
      implicit none
      integer lunI
      real scale, freq
      logical noline
c
c     Depending on the first axis type, set the scale to convert
c     from a channel increment to a frequency increment (Hz) and then
c     if possible, to a magnetic field strength
c
c     Input:
c        lunI    i  Handle for I spectrum cube
c        freq    r  Frequency (GHz) of line for matching with
c                   Zeeman parameters stored internally   
c     Output:
c        scale   r  If noline=.false. scale converts one channel
c                   to a magnetic field strength (G), else scale 
c                   converts one channel to Hz
c        noline  l  If .false. the user specified FREQ was matched
c--
c----------------------------------------------------------------------
      real light
      parameter (light = 2.9979e5)
      integer nfreq
      parameter (nfreq = 4)
c
      double precision cdelt, crval, crpix
      integer j, ifreq, imch, ifax
      character ctype*9
c
      integer cfreq(nfreq)
      real zsplit(nfreq)
c
c Splitting in Hz/G for various lines
c
      data cfreq  /1420,   1665,     1667,     1720  /
      data zsplit /2.80E6, 3.2787E6, 1.9608E6, 0.6536E6/
c-----------------------------------------------------------------------
c
c Get frequency increment in Hz
c
      call coinit (lunI)
      call covelset (lunI, 'frequency')
      call cofindax (lunI, 'spectral', ifax)
      call coaxdesc (lunI, ifax, ctype, crpix, crval, cdelt)
      call cofin (lunI)
      cdelt = cdelt * 1.0e9
c
c Integer frequency in MHz; try and match it
c
      ifreq = nint(1000*freq)
      noline = .true.
      do j = 1, nfreq
        if(ifreq.eq.cfreq(j)) then
          imch = j
          noline = .false.
          goto 10
        end if
      end do
c
10    if (noline) then 
c
c No match, scale just converts channels to Hz
c
        scale = cdelt
      else
c
c Convert channels to Gauss
c
        scale = cdelt / zsplit(imch)
      end if
c
      end
c************************************************************************
c* Zed -- Zeeman fit of I spectrum to V spectrum.
c& nebk
c: Zeeman
c+
	subroutine Zed(mode,ispect,vspect,m,n,a,b,siga,sigb,sigi,convrg)
c
	implicit none
	character mode*(*)
	integer m,n
	logical convrg
	real ispect(m,n),vspect(m,n),a,b,siga,sigb,sigi
c
c  This finds values of a and b, which are optimal in either a maximum
c  likelihood or least squares sense, such that
c
c    V = a*D*I + b*I
c
c  One solution for a and b is found for the n given spectra.
c
c  Here D is a matrix approximating a derivative operator. The derivative
c  formula used here is either:
c    f(i) - f(i-1)
c  or
c    0.5*(f(i+1) - f(i-1))
c
c  Inputs:
c    ispect,vspect Measured I and V spectra.
c    m		Number of data points in the I and V spectra.
c    n		Number of spectra.
c    mode	This consists of flags, which determine
c		the algorithm to be used. Possible characters are:
c		  '2'	Use two sided derivative estimate. Otherwise a
c			one sided derivative is used.
c		  'm'	Use the maximum likelihood technique, otherwise a 
c			least squares algorithm is used.
c		  'l'	Fit for the leakage parameter as well.
c		  'x'	Perform extra checks that we have converged to the
c			global minimum. This slows down the algorithm by
c			about a factor of 3. This should be used in cases
c			of poor signal to noise.
c		   'd'  Debiased least squares. This debiases alpha and sigma
c			alpha only.
c		For example, mode='mlx2' means use max. likelihood, including
c		leakage, use two sided derivative, and perform extra checks
c		that we have converged to the global minimum.
c  Output:
c    a,b	Estimated a and b parameters.
c    siga,sigb	Estimates of the error in a and b.
c    sigi	Estimate of the rms noise.
c    convrg	True if the procedure converged. It is generally safe to
c		ignore this. Even when it indicates the algorithm has
c		failed to converge, it actually means that it is essentially
c		at the minimum, but not all conditions for convergence have
c		yet to be satisfied.
c--
c------------------------------------------------------------------------
	integer nsig
	parameter(nsig=4)
	logical leak,ML,extra,convrg0,more,DeBias
	integer delta,i
	real a0,b0,siga0,sigb0,sigi0,sigim,sigip,sigs,limit,t
c
	DeBias = index(mode,'d').ne.0
	ML = index(mode,'m').ne.0
	leak = index(mode,'l').ne.0
	extra = index(mode,'x').ne.0.and.ML
	delta = 0
	if(index(mode,'2').ne.0) delta = 1
c
c  Get the least squares solution for alpha and beta. If we are going to
c  find the max. likelihood solution, do not initially find the leakage.
c
	call ZedLSQ(ispect,vspect,m,n,a,b,siga,sigb,sigi,
     *		leak.and..not.ML,delta)
	convrg = .true.
c
c  Debias least squares if desirable.
c
	if(DeBias.or.ML)
     *	  call ZedDeLsq(ispect,m,n,a,siga,sigi,delta,convrg)
c
c
c  Get the max likelihood solution if desired.
c
	if(ML)then
	  call ZedML(ispect,vspect,m,n,a,b,siga,sigb,sigi,
     *					leak,delta,convrg)
	  if(extra)then
	    do i=-1,1,2
	      a0 = a + i*15*siga
	      b0 = 0
	      call ZedML(ispect,vspect,m,n,a0,b0,siga0,sigb0,sigi0,
     *					leak,delta,convrg0)
	      if(sigi0.lt.sigi)then
		a = a0
		b = b0
		sigi = sigi0
		siga = siga0
		sigb = sigb0
		convrg = convrg0
	      endif
	    enddo
	  endif
c
c  We have the max. likelihood solution. We want to get a better estimate
c  of siga than that provided by ZedML. ZedML uses derivative information
c  at the minima, but as the likelihood function is not gaussian for poor
c  signal to noise ratios, it is better to fit a parabola to three
c  points at a, a+sigs*siga, a-sigs*siga.
c
	  limit = sigi * sqrt( 1. + real(nsig*nsig)/real((m-1)*n) )
	  sigs = 4
	  more = .true.
	  dowhile(more)
	    sigs = 1.5*sigs
	    call ZedFunc(mode,ispect,vspect,m,n,a-sigs*siga,b,sigim)
	    call ZedFunc(mode,ispect,vspect,m,n,a+sigs*siga,b,sigip)
	    more = min(sigim,sigip).lt.limit
	  enddo
	  t = 0.5 * (m-1)*n*(sigim*sigim + sigip*sigip - 2*sigi*sigi)
	  if(t.ne.0)siga = siga * sigi * sigs / sqrt(t)
	endif
c
	end
c************************************************************************
	subroutine ZedDeLsq(ispect,m,n,a,siga,sigi,delta,convrg)
c
	implicit none
	integer m,n,delta
	logical convrg
	real ispect(m,n),a,siga,sigi
c
c  Determine the least squares debias factor.
c
c  Inputs:
c    m,n
c    ispect
c    delta
c  Input/Output:
c    a
c    siga
c  Output:
c    convrg
c------------------------------------------------------------------------
	real etahat2,eta2
c
	call ZedEta(ispect,m,n,sigi,delta,eta2,etahat2)
c
	convrg = etahat2.gt.0
	if(convrg)then
	  if(delta.eq.0)then
	    a = a * (1+2/etahat2)
	    siga = siga * (1+2/etahat2)
	  else
	    a = a * (1+0.5/etahat2)
	    siga = siga * (1+0.5/etahat2)
	  endif
	endif
c	  
	end
c************************************************************************
	subroutine ZedEta(ispect,m,n,sigi,delta,eta2,etahat2)
c
	implicit none
	integer m,n,delta
	real sigi,eta2,etahat2,ispect(m,n)
c
c------------------------------------------------------------------------
	real SumDD,t
	integer i,j
c
	SumDD = 0
	do j=1,n
	  do i=2,m-delta
	    t = ispect(i+delta,j)-ispect(i-1,j)
	    SumDD = SumDD + t*t
	  enddo
	enddo
	eta2 = SumDD / ( (delta+1)**2 * (n*(m-delta-1)) * (sigi*sigi) )
	if(delta.eq.1)then
	  etahat2 = eta2 - 0.5
	else
	  etahat2 = eta2 - 2
	endif
c
	end
c************************************************************************
c* ZedIhat -- Calculate Zeeman estimate of a true I spectrum
c& nebk
c: Zeeman
c+
	subroutine ZedIHat(mode,ispect,vspect,m,n,a,b,ihat)
c
	implicit none
	character mode*(*)
	integer m,n
	real ispect(m,n),vspect(m,n),ihat(m,n)
	real a,b
c
c  This returns an estimate of the true I spectrum, given a and b.
c
c  Inputs:
c    mode	Flags to the algorithm. The only recognized flag is '2',
c		for two sided derivative.
c    ispect,vspect Observed I and V spectra.
c    m		Number of channels.
c    n		Number of spectra.
c    a,b	Alpha and beta parameters of interest.
c
c  Output:
c    ihat	An estimate of the true value of I.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer j,delta
c
	delta = 0
	if(index(mode,'2').ne.0) delta = 1
c
	if(m.gt.maxchan) call bug('f','Too many channels')
c
	do j=1,n
	  if(delta.eq.0)then
	    call zed1(Ispect(1,j),Vspect(1,j),m,a,b,ihat(1,j))
	  else
	    call zed2(Ispect(1,j),Vspect(1,j),m,a,b,ihat(1,j))
	  endif
	enddo
	end
c************************************************************************
c* ZedVhat -- Calculate Zeeman estimate of a true V spectrum
c& nebk
c: Zeeman
c+
	subroutine ZedVHat(mode,IHat,m,n,a,b,VHat)
c
	implicit none
	character mode*(*)
	integer m,n
	real IHat(m,n), VHat(M,n)
	real a,b
c
c  This returns an estimate of the true V spectrum, given a, b and IHat
c
c  Inputs:
c    mode	Flags to the algorithm. The only recognized flag is '2',
c		for two sided derivative.
c    IHat       Estimate of true noiseless spectrum
c    m		Number of channels.
c    n		Number of spectra.
c    a,b	Alpha and beta parameters of interest.
c
c  Output:
c    VHat	An estimate of the true value of V.  Note that the first
c               and possibly last (depending on derivative type) is NOT 
c               filled.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,delta
        real s,der
c
	delta = 0
	if(index(mode,'2').ne.0) delta = 1
c
	if(m.gt.maxchan) call bug('f','Too many channels')

        if (delta.eq.1) then
          s = 0.5
        else
          s = 1.0
        end if
c
        do j = 1, n
          do i = 2, m-delta
            der = IHat(i+delta,j) - IHat(i-1,j)
            VHat(i,j) =  (a * s * der) + (b * IHat(i,j))
          end do
        end do
c
        end
c************************************************************************
c* ZedFunc -- Return Zeeman maximum likelihood chi**2 value.
c& nebk
c: Zeeman
c+
	subroutine ZedFunc(mode,ispect,vspect,m,n,a,b,sigi)
c
	implicit none
	integer m,n
	real ispect(m,n),vspect(m,n),a,b,sigi
	character mode*(*)
c
c  This returns a normalized value of \chi^2, given a and b.
c
c  Inputs:
c    mode	Flags to the algorithm. The only recognized flag is '2',
c		for two sided derivative.
c    ispect,vspect I and V spectra.
c    m		Number of channels.
c    n		Number of spectra.
c    a,b	Alpha and beta parameters of interest.
c
c  Output:
c    sigi	Normalized version of \chi^2. In particular
c		sigi = \sqrt{\chi^2/((m-1)*n)}
c		The expected value of this at the minimum is \sigma_I.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,delta
	real i0(maxchan),ad,t
c
	delta = 0
	if(index(mode,'2').ne.0) delta = 1
c
	if(m.gt.maxchan) call bug('f','Too many channels')
c
	ad = a/(delta+1)
	sigi = 0
	do j=1,n
	  if(delta.eq.0)then
	    call zed1(Ispect(1,j),Vspect(1,j),m,a,b,i0)
	  else
	    call zed2(Ispect(1,j),Vspect(1,j),m,a,b,i0)
	  endif
	  do i=2,m-delta
	    t = ad * (I0(i+delta) - I0(i-1))
	    sigi = sigi + (Ispect(i,j) - I0(i))**2 +
     *		   (Vspect(i,j) - t - b*I0(i))**2
	  enddo
	enddo
	sigi = sqrt(sigi/((m-1-delta)*n))
	end
c************************************************************************
	subroutine ZedLSQ(ispect,vspect,m,n,a,b,siga,sigb,sigi,
     *							leak,delta)
c
	implicit none
	integer m,n,delta
	real ispect(m,n),vspect(m,n),a,b,siga,sigb,sigi
	logical leak
c
c  Calculate the least squares solution.
c
c  Input:
c    ispect
c    vspect
c    m,n
c    leak
c    delta
c  Outputs:
c    a,b
c    siga,sigb,sigi
c
c------------------------------------------------------------------------
	double precision SumDD,SumVD,SumBB,SumBD,SumVB,SumVV
	real s,t
	integer i,j
c
	s = delta + 1
	SumVV = 0
	SumDD = 0
	SumVD = 0
	SumBB = 0
	SumVB = 0
	SumBD = 0
	do j=1,n
	  do i=2,m-delta
	    t = ispect(i+delta,j) - ispect(i-1,j)
	    SumVV = SumVV + dble(Vspect(i,j))**2
	    SumDD = SumDD + dble(t)**2
	    SumVD = SumVD + dble(Vspect(i,j)) * t
	    SumBB = SumBB + dble(Ispect(i,j)) * Ispect(i,j)
	    SumVB = SumVB + dble(Vspect(i,j)) * Ispect(i,j)
	    SumBD = SumBD + dble(Ispect(i,j)) * t
	  enddo
	enddo
c
c  Determine the least squares estimate of a and b.
c
	if(leak)then
	  t = 1 / (SumBB*SumDD - SumBD*SumBD)
	  a = s * t * ( SumBB*SumVD - SumBD*SumVB )
	  b =	  t * ( SumDD*SumVB - SumBD*SumVD )
	else
	  a = s * SumVD / SumDD
	  b = 0
	endif
c
c  Calculate the error estimates.
c
	sigi = abs(SumVV + a*a*SumDD/(s*s) + b*b*SumBB
     *		- 2*a*SumVD/s - 2*b*SumVB + 2*a*b*SumBD/s)
	if(delta.eq.0)then
	  sigi = sqrt( sigi / ((1+a*a+(a+b)*(a+b))*(m-1)*n) )
	  siga = sigi / sqrt(SumDD) * (1 + 2*a*a)
	else
	  sigi = sqrt(sigi/((1+a*a/2+b*b)*(m-2)*n))
	  siga = 2 * sigi / sqrt(SumDD) * (1 + 0.5*a*a)
	endif
	sigb = sigi/sqrt(SumBB)
c
	end
c************************************************************************
	subroutine ZedML(ispect,vspect,m,n,a,b,siga,sigb,sigi,
     *						leak,delta,convrg)
c
	implicit none
	integer m,n
	real ispect(m,n),vspect(m,n),a,b,siga,sigb,sigi
	logical convrg,leak
	integer delta
c
c  This finds the maximum likelihood solution of the problem. This works
c  by iteratively finding the optimum I0 solution, then the optimum
c  solution for a and b. The first part (done by Zed1 or Zed2) involves
c  solving a tridiagonal or pentadiagonal system. The second part involves
c  a simple linear least squares problem in one or two variables.
c
c  Inputs:
c    ispect,vspect I and V spectra.
c    m		Number of data points in the I and V spectra.
c    n		Number of spectra.
c    leak	Logical. True if a leakage term is to be fitted.
c    delta	This can be either 0 or 1, which determines whether a
c		one or two sided derivative is used.
c  Output:
c    a,b	Estimated a and b parameters.
c    siga,sigb	Formal error of the estimate.
c    sigi	Estimate of the rms noise in I and V.
c    convrg	True if the procedure converged.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MaxNiter
	real epsi
	parameter(MaxNiter=200,epsi=1e-4)
c
	integer i,j,niter
	logical more
	double precision SumDD,SumVD,SumBB,SumBD,SumVB,SumII,SumVV
	real s,t,i0(maxchan),OldA
c
	if(m.gt.maxchan) call bug('f','Too many channels for me')
c
	more = .true.
	niter = 0
	s = delta + 1
c
c  The main interation loop.
c
	dowhile(more.and.niter.lt.MaxNiter)
c
c  Initialise.
c
	  SumII = 0
	  SumDD = 0
	  SumVD = 0
	  SumBB = 0
	  SumVB = 0
	  SumBD = 0
	  OldA = a
c
c  Get the new estimate of I0.
c
	  do j=1,n
	    if(delta.eq.0)then
	      call zed1(Ispect(1,j),Vspect(1,j),m,a,b,I0)
	    else
	      call zed2(Ispect(1,j),Vspect(1,j),m,a,b,I0)
	    endif
c
c  Accumulate statistics.
c
	    do i=2,m-delta
	      t = i0(i+delta) - i0(i-1)
	      SumII = SumII + dble((Ispect(i,j) - I0(i)))**2
	      SumDD = SumDD + dble(t)**2
	      SumVD = SumVD + dble(Vspect(i,j)) * t
	      SumBB = SumBB + dble(I0(i))**2
	      SumVB = SumVB + dble(Vspect(i,j)) * I0(i)
	      SumBD = SumBD + dble(I0(i)) * t
	    enddo
	  enddo
c
c  Determine the least squares estimate of a and b.
c
	  if(leak)then
	    t = 1 / (SumBB*SumDD - SumBD*SumBD)
	    a = s * t * ( SumBB*SumVD - SumBD*SumVB )
	    b =     t * ( SumDD*SumVB - SumBD*SumVD )
	  else
	    a = s * SumVD / SumDD
	    b = 0 
	  endif
	  more = (abs(a-OldA) .gt. epsi*abs(a))
c
c  Loop the loop.
c
	  niter = niter + 1
	enddo
	convrg = .not.more
c
c  Return the estimates of siga, sigb and sigi, based on the most
c  recently calculated statistics.
c
	SumVV = 0
	do j=1,n
	  do i=2,m-delta
	    SumVV = SumVV + dble(Vspect(i,j))**2
	  enddo
	enddo
c
	sigi = abs(SumII + SumVV + a*a*SumDD/(s*s) + b*b*SumBB
     *		- 2*a*SumVD/s - 2*b*SumVB + 2*a*b*SumBD/s)
	sigi = sqrt(sigi/((m-delta-1)*n))
	siga = s * sigi / sqrt(SumDD)
	sigb = sigi/sqrt(SumBB)
c
	end
c************************************************************************
	subroutine zed1(Ispect,Vspect,n,a,b,I0)
c
	implicit none
	integer n
	real a,b
	real Ispect(n),Vspect(n),I0(n)
c
c  This finds the maximum likelihood solution of I0, when a one sided
c  derivative is used. It involves solving a tridiagonal system of
c  equations.
c
c  Outputs:
c    I0		The estimate of I0.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real aa,ab,aab,aaab,beta
	real gamma(maxchan)
	integer i
c
c  Calculate a new estimate for I0. Forward substitution loop.
c
	aa = a*a
	ab = a + b
	aab = a*(a+b)
	aaab = aa + (a+b)*(a+b)
c
	beta = 1 + aa
	i0(1) = ( aa*ispect(1) - aab*ispect(2)  + a*vspect(2) )/beta
	do i=2,n-1
	  gamma(i) = -aab/beta
	  beta = 1 + aaab + aab*gamma(i)
c
	  i0(i) = ( aaab*ispect(i) - aab*(ispect(i-1) + ispect(i+1))
     *		 - ab*vspect(i) + a*vspect(i+1) + aab*i0(i-1) ) / beta
	enddo
	gamma(n) = -aab/beta
	beta = 1 + aa + aab*gamma(n)
	i0(N) = ( aa*ispect(n) - aab*ispect(n-1)
     *			- ab*vspect(n) + aab*i0(n-1) ) / beta
c
c  Back substitution loop.
c
	beta = i0(n)
	i0(n) = ispect(n) - beta
	do i=n-1,1,-1
	  beta =  i0(i) - beta*gamma(i+1)
	  i0(i)  = ispect(i) - beta
	enddo
	end
c************************************************************************
	subroutine zed2(Ispect,Vspect,n,a,b,I0)
c
	implicit none
	integer n
	real a,b
	real Ispect(n),Vspect(n),I0(n)
c
c  This finds the maximum likelihood solution of I0, when a two sided
c  derivative is used. It involves solving a pentadiagonal system of
c  equations.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real abd(3,maxchan),ad,bd
	integer i,ifail
c
	ad = 0.5 * a
	bd = b
c
c  Initalise the matrix system that we need to solve.
c
	abd(3,1) = 1 + ad*ad
	abd(2,2) = -ad*bd
	abd(3,2) = 1 + ad*ad + bd*bd
	i0(1) =               ad*Vspect(2)
	i0(1) = i0(1) + ad*ad*Ispect(1) - ad*bd*Ispect(2)
     *		      - ad*ad*Ispect(3)
	i0(2) = -bd*Vspect(2) + ad*Vspect(3)
	i0(2) = i0(2) - ad*bd*Ispect(1) + (ad*ad+bd*bd)*Ispect(2) 
     *			- ad*ad*Ispect(4)
	do i=3,n-2
	  abd(1,i) = -ad*ad
	  abd(2,i) = 0
	  abd(3,i) = 1 + 2*ad*ad + bd*bd
	  i0(i) = -ad*Vspect(i-1) - bd*Vspect(i) + ad*Vspect(i+1)
	  i0(i) = i0(i) - ad*ad*Ispect(i-2) + (2*ad*ad+bd*bd)*Ispect(i)
     *			  - ad*ad*Ispect(i+2)
	enddo
c
	abd(1,n-1) = -ad*ad
	abd(2,n-1) = 0
	abd(3,n-1) = 1 + ad*ad + bd*bd
	abd(1,n) = -ad*ad
	abd(2,n) = -ad*bd
	abd(3,n) = 1 + ad*ad
c
	i0(n-1) = -ad*Vspect(n-2) - bd*Vspect(n-1)
	i0(n-1) = i0(n-1) - ad*bd*Ispect(n) + (ad*ad+bd*bd)*Ispect(n-1) 
     *			    - ad*ad*Ispect(n-3)
	i0(n) =   -ad*Vspect(n-1)
	i0(n) = i0(n) + ad*ad*Ispect(n) - ad*bd*Ispect(n-1)
     *			    - ad*ad*Ispect(n-2)
c
c  Solve the system of equations.
c
	call spbfa(abd,3,n,2,ifail)
	if(ifail.ne.0) call bug('f','Failed to factor matrix!!!')
	call spbsl(abd,3,n,2,i0)
c
c  Estimate the true I from this.
c
	do i=1,n
	  i0(i) = Ispect(i) - i0(i)
	enddo
c
	end
c************************************************************************
c* ZedFudge -- Calculate sigma fudge factor, for Zeeman experiments.
c& nebk
c: Zeeman
c+
	subroutine ZedFudge(mode,ispect,vspect,m,n1,n2,a,b,fudge,
     *							rho,beam,nx,ny)
c
	implicit none
	character mode*(*)
	integer m,n1,n2,nx,ny
	real ispect(m,n1*n2),vspect(m,n1*n2)
	real a,b,fudge,rho,beam(nx*ny)
c
c  This determines the fudge factor that the \sigma_{\alpha} estimate
c  should be multiplied by to give a correct value. This is used when
c  the noise is correlated.
c
c  Inputs:
c    mode	'2' indicates that we should use a two sided derivative.
c    m		Number of channels.
c    n1,n2	Number of pixels in x and y.
c    ispect	I spectra.
c    vspect	V spectra.
c    a,b	Values of \alpha and \beta to use.
c    rho	Correlation factor in the spectral dimension.
c    beam	Dirty beam patch.
c    nx,ny	Size of the dirty beam patch.
c
c  Output:
c    fudge	Sigma fudge factor. Sigma estimates derived by ignoring the
c		noise correlation should be multiplied by this fudge factor
c		to get better error estimates.
c--
c
c  NOTE: We set MAXXY statically. Allowing it to become arbitrarily big
c  runs us into singular maxtrix problems. All this code is a fudge anyway
c  and of limited use. The fudge factor is not reliable, and Monte Carlo
c  simulations should be used in preference.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXXY
	parameter(MAXXY=100)
c
	integer pAP,pDI,pRDI
	real gamma(MAXCHAN)
c
	logical spatial,spectral
	integer delta,md,n1d,n2d,off
	real SumDD,SumDRD
c
	real ref(MAXBUF)
	common ref
c
c  Externals.
c
	real sdot
c
c  Initialise.
c
	spectral = abs(rho).gt.1e-2
	spatial = n1*n2.gt.1.and.nx*ny.gt.1
	fudge = 1
	if(.not.spectral.and..not.spatial) return
	delta = 0
	if(index(mode,'2').ne.0) delta = 1
	md = m - 1 - delta
c
c  Check that the problem is not too big.
c
	if(m.gt.maxchan)
     *	  call bug('f','Spectral dimension is too big')
c
c  If the spatial dimension is too big, just handle a subwindow.
c  ZedWind determines a good subwindow.
c
	if(n1*n2.gt.maxxy)then
	  call bug('w','Spatial dimension is too big in ZedFudge')
	  call bug('w','ZedFudge is processing a subwindow only')
	  call ZedWind(ispect,m,n1,n2,maxxy,off,n1d,n2d)
	else
	  off = 1
	  n1d = n1
	  n2d = n2
	endif
c
c  Allocate memory.
c
	call MemAlloc(pDI,(m-1)*n1d*n2d,'r')
	call MemAlloc(pRDI,(m-1)*n1d*n2d,'r')
	call MemAlloc(pAP,n1d*n2d*(n1d*n2d+1)/2,'r')
c
c  Determine DI, and make a copy of it.
c
	call ZedDi(ispect(1,off),vspect(1,off),a,b,ref(pDI),
     *					m,md,n1d,n2d,n1,delta)
	call scopy(md*n1d*n2d,ref(pDI),1,ref(pRDI),1)
c
c  Do RDi for the spectral portion.
c
	if(spectral)then
	  call ZedFCov(gamma,md,rho)
	  call ZedFApp(ref(pRDI),md,n1d*n2d,gamma,rho)
	endif
c
c  Do RDi for the spatial portion.
c
	if(spatial)then
	  call ZedXYCov(ref(pAP),n1d,n2d,beam,nx,ny)
	  call ZedXYApp(ref(pRDI),md,n1d*n2d,ref(pAP))
	endif
c
c  Determine the relevant inner product.
c
	SumDD = sdot(md*n1d*n2d,ref(pDI),1,ref(pDI),1)
	SumDRD = sdot(md*n1d*n2d,ref(pDI),1,ref(pRDI),1)
c
	Fudge = sqrt(SumDD / SumDRD)
c
c  Free up memory.
c
	call MemFree(pDI,(m-1)*n1d*n2d,'r')
	call MemFree(pRDI,(m-1)*n1d*n2d,'r')
	call MemFree(pAP,n1d*n2d*(n1d*n2d+1)/2,'r')
c
	end
c************************************************************************
	subroutine ZedWind(ispect,m,n1,n2,maxxy,off,n1d,n2d)
c
	implicit none
	integer m,n1,n2,off,n1d,n2d,maxxy
	real ispect(m,n1,n2)
c
c  This determines the subwindow of the input spectra that we are to
c  process. It works by finding the spectrum with the most power, and
c  then setting a maximum size window around this spectrum. The aspect
c  ratio of this window are approximately the same as the aspect ratio
c  of the input region.
c
c------------------------------------------------------------------------
	integer i,j,k,j0,k0
	real temp,maxpower
c
	maxpower = 0
	do k=1,n2
	  do j=1,n1
	    temp = 0
	    do i=1,m
	      temp = temp + ispect(i,j,k)*ispect(i,j,k)
	    enddo
	    if(temp.gt.maxpower)then
	      maxpower = temp
	      j0 = j
	      k0 = k
	    endif
	  enddo
	enddo
	n1d = sqrt(real((maxxy*n1)/n2))
	n1d = min(n1d,n1)
	n2d = min(maxxy/n1d, n2)
	j0 = max(1,min(j0 - n1d/2,n1-n1d+1))
	k0 = max(1,min(k0 - n2d/2,n2-n2d+1))
c
	off = j0 + (k0-1)*n1
	end
c************************************************************************
	subroutine ZedDi(ispect,vspect,a,b,di,m,md,n1,n2,n,delta)
c
	implicit none
	integer m,md,n,n1,n2,delta
	real ispect(m,n,n2),vspect(m,n,n2),di(md,n1*n2)
	real a,b
c
c  This determine the maximum likelihood derivative I spectra.
c
c  Inputs:
c    ispect,vspect I and V spectra.
c    a,b	Max. likelihood \alpha and \beta parameters.
c    m		The number of channels.
c    md		M less 1 or 2, depending if 1 or 2 sided derivative is used.
c    delta	If 0, use one sided derivative. If 1, use two sided.
c
c  Output:
c    di		Derivative spectra.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	real ihat(maxchan),s
	integer i,j,k,j0
c
	s = 1./real(delta+1)
c
	j0 = 0
	do k=1,n2
	  do j=1,n1
	    if(delta.eq.0)then
	      call zed1(Ispect(1,j,k),Vspect(1,j,k),m,a,b,ihat)
	    else
	      call zed2(Ispect(1,j,k),Vspect(1,j,k),m,a,b,ihat)
	    endif
	    j0 = j0 + 1
	    do i=1,md
	      di(i,j0) = s * (ihat(i+1+delta) - ihat(i))
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine ZedXYApp(DI,md,n,a)
c
	implicit none
	integer md,n
	real DI(md,n),a(n*(n+1)/2)
c
c  Multiply the estimate of the derivative of I, by the inverse of the
c  spatial covariance matrix.
c
c  Input:
c    md		The length of the rhs vectors.
c    n		Number of right hand sides.
c    a		"Factorisation" of the covariance matrix, determined
c		by ZedXYCov.
c
c  Input/Output:
c    DI		On input, this is the right-hand-side vectors. On output,
c		it is the rhs vectors multiplied by the inverse of the
c		covariance matrix.
c
c------------------------------------------------------------------------
	integer maxxy
	parameter(maxxy=100)
	real b(maxxy)
	integer i,j
c
	do j=1,md
	  do i=1,n
	    b(i) = di(j,i)
	  enddo
	  call sppsl(a,n,b)
	  do i=1,n
	    di(j,i) = b(i)
	  enddo
	enddo
	end
c************************************************************************
	subroutine ZedFApp(DI,md,n,gamma,rho)
c
	implicit none
	integer md,n
	real DI(md,n),rho,gamma(md)
c
c  Multiply the estimate of the derivative of I, by the inverse of the
c  spectral covariance matrix.
c
c  Input:
c    md		The length of the rhs vectors.
c    n		Number of right hand sides.
c    rho	The correlation factor.
c    gamma	"Factorisation" of the covariance matrix, determined
c		by ZedFCov.
c
c  Input/Output:
c    DI		On input, this is the right-hand-side vectors. On output,
c		it is the rhs vectors multiplied by the inverse of the
c		covariance matrix.
c
c------------------------------------------------------------------------
	integer i,j
c
	do j=1,n
	  do i=2,md
	    di(i,j) = gamma(i) * (di(i,j) - rho*di(i-1,j))
	  enddo
	  do i=md-1,1,-1
	    di(i,j) = di(i,j) - rho*gamma(i)*di(i+1,j)
	  enddo
	enddo
	end
c************************************************************************
	subroutine ZedFCov(gamma,md,rho)
c
	implicit none
	integer md
	real gamma(md),rho
c
c  Form a factorisation of the spectral covariance matrix.
c
c------------------------------------------------------------------------
	integer i
c
	if(abs(rho).ge.0.5)
     *	  call bug('w','Spectral covariance matrix may be singular')
c
	gamma(1) = 1
	do i=2,md
	  gamma(i) = 1 / (1 - rho*rho*gamma(i-1))
	enddo
	end
c************************************************************************
	subroutine ZedXYCov(a,n1,n2,beam,nx,ny)
c
	implicit none
	integer n1,n2,nx,ny
	real a(n1*n2*(n1*n2+1)/2),beam(nx,ny)
c
c  This computes the factorisation of the spatial covariance matrix.
c  The factorisation is done by the LinPack routine SPPFA, so resulting array
c  is ready for use with the LinPack routine SPPSL.
c
c  Input:
c    n1,n2	Size of the spatial region of interest.
c    nx,ny	Size of the beam patch.
c    beam	The beam patch.
c
c  Output:
c    a		Factorisation of the covariance matrix.
c
c------------------------------------------------------------------------
	integer maxxy
	parameter(maxxy=100)
	integer i,j,k,i0,i1,j1,ifail
	real temp,rcond
	integer indx(maxxy)
	real acf(2*maxxy)
c
c  Find the autocorrelation of the beam in the region (-(n-1):n1-1,0:n2-1)
c
	k = 0
	do j = 0,n2-1
	  do i = -(n1-1),n1-1
	    temp = 0
	    do j1 = 1,ny-j
	      do i1 = max(1,1-i),min(nx,nx-i)
		temp = temp + beam(i1,j1) * beam(i1+i,j1+j)
	      enddo
	    enddo
	    k = k + 1
	    acf(k) = temp
	  enddo
	enddo
c
c  Normalize the acf to have a peak value of 1.
c
	acf(n1) = 1.01 * acf(n1)
	temp = 1/acf(n1)
	do k=1,(2*n1-1)*n2
	  acf(k) = temp * acf(k)
	enddo
c
c  Form the covariance matrix. The best canned routine I can find to solve
c  this system of equations is a Linpack routine to solve a real symmetric
c  positive definite matrix. So we have to pack the autocorrelation function
c  into the form expected by this routine. The way we do this is a bit
c  contorted, but at least it vectorises.
c
	do j=1,n1*n2
	  indx(j) = mod(j-1,n1) + (2*n1-1)*((j-1)/n1)
	enddo
c
	k = 0
	do j=1,n1*n2
	  i0 = n1 + indx(j)
	  do i=1,j
	    k = k + 1
	    a(k) = acf(i0-indx(i))
	  enddo
	enddo
c
c  Factor the covariance matrix and check that it is well conditioned.
c
	call sppco(a,n1*n2,rcond,acf,ifail)
	if(ifail.ne.0)
     *	  call bug('f','Spatial covariance matrix is singular')
	if(rcond.lt.1e-5)
     *	  call bug('w','Spatial covariance matrix is ill conditioned')
	end
c************************************************************************
c* ZedRho -- Estimate Zeeman splitting, for spectrally correlated noise.
c& nebk
c: Zeeman
c+
	subroutine ZedRho(mode,ispect,vspect,m,n,rho,a,b,siga,sigb,sigi,
     *						convrg)
c
	implicit none
	character mode*(*)
	integer m,n
	real ispect(m),vspect(m),rho,a,b,siga,sigb,sigi
	logical convrg
c
c  This finds the maximum likelihood solution, when there is correlation
c  between adjacent channels. This performs a proper treatment. There are
c  no approximations.
c
c
c  Inputs:
c    ispect,vspect Measured I and V spectra.
c    m		Number of data points in the I and V spectra.
c    n		Number of spectra. Currently this must be 1.
c    mode	Currently ignored.
c    rho	Channel-to-channel correlation coefficient.
c  Output:
c    a,b	Estimated a and b parameters.
c    siga,sigb	Estimates of the error in a and b.
c    sigi	Estimate of the rms noise.
c    convrg	True if the procedure converged. It is generally safe to
c		ignore this. Even when it indicates the algorithm has
c		failed to converge, it actually means that it is essentially
c		at the minimum, but not all conditions for convergence have
c		yet to be satisfied.
c  Limitations:
c    Unlike Zed, this does not handle multiple spectra, leakage terms,
c    2-sided derivative nor does it do extra searching to avoid spurious
c    minima. Its estimate of siga is not as good as Zed's. Generally
c    it is faster and just as accurate (more accurate in siga) to call
c    the combination of Zed and ZedFudge.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer maxniter
	real epsi
	parameter(maxniter=200,epsi=1.e-4)
	real RDRV(maxchan),Ihat(maxchan),DI(maxchan),RDI(maxchan)
	integer pivot(maxchan)
	integer i,j,niter,i0,ifail,delta
	real t,SumVD,SumDD,OldA
	logical more,leak
	integer pA0,pRDRD
c
	real ref(MAXBUF)
	common ref
c
c  Allocate memory.
c
	call MemAlloc(pRDRD,m*m,'r')
	call MemAlloc(pA0,m*m,'r')
c
c  Check input parameters.
c
	leak = index(mode,'l').ne.0
	delta = 0
	if(index(mode,'2').ne.0) delta = 1
c
	if(m.gt.maxchan) call bug('f','Too many channels')
	if(n.ne.1) call bug('f','Can only handle one spectra')
	if(leak) call bug('w','Cannot handle leakage')
	if(delta.ne.0) call bug('w','One-sided derivative only')
c
c  Calculate the least squares solution for a.
c
	SumVD = 0
	SumDD = 0
	do j=2,m
	  t = ispect(j) - ispect(j-1)
	  SumVD = SumVD + t*vspect(j)
	  SumDD = SumDD + t*t
	enddo
	a = SumVD / SumDD
c
c  Calculate the matrix RD^TR^{-1}D.
c
	do j=1,m
	  i0 = (j-1)*m + pRDRD - 1
	  do i=i0+1,i0+m-1
	    ref(i) = 0
	  enddo
	  if(j.ne.m)ref(i0+j) = -1
	  if(j.ne.1)ref(i0+j-1) = 1
	  call ZedRDR(ref(i0+1),m,rho)
	enddo
c
c  Calculate the vector RD^TR^{-1}V.
c
	do i=1,m-1
	  RDRV(i) = vspect(i+1)
	enddo
	call ZedRDR(RDRV,m,rho)
c
c  Determine the initial estimate of alpha.
c
	more = .true.
	niter = 0
	dowhile(more.and.niter.lt.MaxNiter)
c
c  Determine a new Ihat.
c
	  call scopy(m*m,ref(pRDRD),1,ref(pA0),1)
	  t = 1/(a*a)
	  do j=1,m*m,(m+1)
	    ref(pA0+j-1) = ref(pA0+j-1) + t
	  enddo
	  call sgefa(ref(pA0),m,m,pivot,ifail)
	  if(ifail.ne.0) call bug('f','Failed to invert a matrix')
c
c  Form the right hand side, and solve.
c
	  do j=1,m
	    Ihat(j) = Ispect(j)/(a*a) + RDRV(j)/a
	  enddo
	  call sgesl(ref(pA0),m,m,pivot,ihat,1)
c
c  Calculate DI and R^{-1}DI.
c
	  do i=1,m-1
	    DI(i) = Ihat(i+1) - ihat(i)
	    RDI(i) = DI(i)
	  enddo
	  call ZedR(RDI,m-1,rho)
c
c  Calculate the inner products that will give use a new alpha.
c
	  SumVD = 0
	  SumDD = 0
	  do i=1,m-1
	    SumVD = SumVD + Vspect(i+1)*RDI(i)
	    SumDD = SumDD + DI(i)*RDI(i)
	  enddo
	  OldA = a
	  a = SumVD / SumDD
	  more = (abs(a-OldA) .gt. epsi*abs(a))
	  niter = niter + 1
	enddo
	convrg = .not.more
c
c  Calculate the error estimates.
c
	sigi = 0
	do j=2,m
	  t = ihat(j) - ihat(j-1)
	  sigi = sigi + (ispect(j)-ihat(j))**2 + (vspect(j) - a*t)**2
	enddo
	sigi = sqrt(sigi/(m-1))
	siga = sigi / sqrt(SumDD)
	b = 0
	sigb = 0
c
c  Free memory.
c
	call MemFree(pRDRD,m*m,'r')
	call MemFree(pA0,m*m,'r')
c
	end
c************************************************************************
	subroutine ZedRDR(x,n,rho)
c
	implicit none
	integer n
	real x(n),rho
c
c  This calculates the vector x = RD^TR^{-1}x, where R is the covariance
c  matrix and D is the derivative matrix
c
c  Input:
c    n		Number of data elements.
c    rho	The correlation.
c  Input/Output:
c    x		On input, there are N-1 elements in "data". On output
c		there are the N elements.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i
	real y(maxchan)
c
c  Find x = R^{-1}x.
c
	call ZedR(x,n-1,rho)
c
c  Find x = D^Tx
c
	y(1) = -x(1)
	do i=2,n-1
	  y(i) = x(i-1) - x(i)
	enddo
	y(n) = x(n-1)
c
c  Find x = Rx.
c
	x(1) = y(1) + rho*y(2)
	do i=2,n-1
	  x(i) = y(i) + rho*(y(i-1)+y(i+1))
	enddo
	x(n) = y(n) + rho*y(n-1)
	end
c************************************************************************
	subroutine ZedR(x,n,rho)
c
	implicit none
	integer n
	real x(n),rho
c
c  Solve the tridiagonal system.
c
c    y = Rx
c  Where R is a tridiagonal matrix, with main diagonal element being 1, and
c  super and sub main diagonals being rho.
c
c  Input:
c    n		Array size.
c    rho	Matrix coefficient.
c
c  In/Out:
c    x
c------------------------------------------------------------------------
	include 'maxdim.h'
	real beta,gamma(maxchan)
	integer i
c
	beta = 1
	do i=2,n
	  gamma(i) = rho / beta
	  beta = 1 - rho*rho/beta
	  x(i) = (x(i) - rho*x(i-1))/beta
	enddo
	do i=n-1,1,-1
	  x(i) = x(i) - gamma(i+1)*x(i+1)
	enddo
	end
