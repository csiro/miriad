c************************************************************************
c* opacget -- Compute sky brightness and opacity of a model atmosphere.
c& rjs
c: utilities
c+
	subroutine opacGet(nfreq,freq,el,t0,p0,h0,fac,Tb)
c
	implicit none
	integer nfreq
	real freq(nfreq),el,t0,p0,h0,fac(nfreq),Tb(nfreq)
c
c  Returns the transmissivity of the atmosphere given frequency, elevation
c  angle and meteorological data. This uses a simple model of the
c  atmosphere and Liebe's model (1985) of the complex refractive index of
c  air.
c
c  The model of the atmosphere is one with an exponential fall-off in
c  the water vapour content (scale height of 1540 m) and a temperature lapse
c  rate of 6.5 mK/m. Otherwise the atmosphere obeys the ideal gas equation
c  and hydrostatic equilibrium.
c
c  Input:
c    freq	Frequency (Hz).
c    el		Elevation angle (radians).
c    t0,p0,h0	Met data - Observatory temperature, pressure and humidity
c		(Kelvins, Pascals and fraction).
c
c  Output:
c    fac	Transmissivity.
c    Tb		Sky brightness temperature.
c------------------------------------------------------------------------
	integer N
	parameter(N=50)
	real tau,Ldry,Lvap,T(N),Pdry(N),Pvap(N),z(N),P,zd
	integer i
c
c  Atmospheric parameters.
c
        real M,R,Mv,g,rho0
        parameter(M=28.96e-3,R=8.314,Mv=18e-3,rho0=1e3,g=9.81)
c
c  d - Temperature lapse rate                0.0065 K/m.
c  z0 - Water vapour scale height,           1540 m.
c  zmax - Max altitude of model atmosphere,  10000 m.
c
	real d,z0,zmax
	parameter(d=0.0065,z0=1540.0,zmax=10000.0)
c
c  Externals.
c
	real pvapsat
c
c  Generate a model of the atmosphere -- T is temperature,
c  Pdry is partial pressure of "dry" consistuents, Pvap is the
c  partial pressure of the water vapour.
c
	do i=1,N
	  zd = (i-1)*zmax/real(N)
	  z(i) = zd
	  T(i) = T0/(1+d/T0*zd)
	  P = P0*exp(-M*g/(R*T0)*(zd+0.5*d*zd*zd/T0))
	  Pvap(i) = min(h0*exp(-zd/z0)*pvapsat(T(1)),pvapsat(T(i)))
	  Pdry(i) = P - Pvap(i)
	enddo
c
c  Determine the transmissivity and sky brightness.
c
	do i=1,nfreq
	  call refract(T,Pdry,Pvap,z,N,freq(i),2.7,el,Tb(i),tau,
     *							    Ldry,Lvap)
	  fac(i) = exp(-tau)
	enddo
c
	end
c************************************************************************
c* refract -- Compute refractive index for an atmosphere.
c& rjs
c: utilities
c+
	subroutine refract(t,pdry,pvap,z,n,nu,T0,el,Tb,tau,Ldry,Lvap)
c
	implicit none
	integer n
	real T(n),Pdry(n),Pvap(n),z(n),nu,T0,el
	real Tb,Ldry,Lvap,tau
c
c  Determine the sky brightness and excess path lengths for a
c  parallel slab atmosphere. Liebe's model (1985) is used to determine
c  the complex refractive index of air.
c
c  Input:
c    n		Number of atmospheric layers.
c    T		Temperature of the layers. T(1) is the temperature at the
c		lowest layer (Kelvin).
c    Pdry	Partial pressure of the dry components (Pascals).
c    Pvap	Partial pressure of the water vapour component (Pascals).
c    z		Height of the layer.
c    nu		Frequency of interest (Hz).
c    T0		Astronomical brightness temperature (Kelvin). e.g. 2.7 for
c		the cosmic background.
c    el		Elevation angle of the source above the atmosphere (radians).
c  Output:
c    Tb		Brightness temperature (Kelvin).
c    tau	Opacity (nepers).
c    Ldry	Excess path -- dry component (meters).
c    Lvap	Excess path -- water vapour component (meters).
c
c  Reference:
c    Liebe, An updated model for millimeter wave propogation in moist air,
c    Radio Science, 20, 1069-1089 (1985).
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	integer i
	real snell,dtau,nr,ni,l,dz
	complex ndry,nvap
c
c  Externals.
c
	complex refdry,refvap
c
	tau = 0
	Tb = T0
	Ldry = 0
	Lvap = 0
c
	snell = sin(el)
	do i=n,1,-1
	  if(i.eq.1)then
	    dz = 0.5*(z(2) - z(1))
	  else if(i.eq.n)then
	    dz = 0.5*(z(n) - z(n-1))
	  else
	    dz = 0.5*(z(i+1) - z(i-1))
	  endif
	  Ndry = refdry(nu,T(i),Pdry(i),Pvap(i))
	  Nvap = refvap(nu,T(i),Pdry(i),Pvap(i))
	  nr = 1+real (Ndry+Nvap)*1e-6
	  ni =   aimag(Ndry+Nvap)*1e-6
	  l = dz*nr / sqrt(nr*nr+(snell*snell)-1)
	  dtau = l*4*PI*nu/CMKS*ni
	  Tb = (Tb-T(i))*exp(-dtau) + T(i)
	  tau = tau + dtau
	  Ldry = Ldry + l*real(Ndry)*1e-6
	  Lvap = Lvap + l*real(Nvap)*1e-6
	enddo
c
	end
c************************************************************************
c* pvapsat -- Compute sky brightness and opacity of a model atmosphere.
c& rjs
c: utilities
c+
	real function pvapsat(T)
c
	implicit none
	real T
c
c  Determine the saturation pressure of water vapour.
c
c  Inputs:
c    T		Temperature (Kelvin).
c  Output:
c    pvatsat	Vapour stauration pressure (Pascals).
c
c  Reference:
c    Waters, Refraction effects in the neutral atmosphere. Methods of
c    Experimental Physics, vol 12B, p 186-200 (1976).
c------------------------------------------------------------------------
	real es,theta
c	theta = 273.0/T
c	es = 6.105*exp(25.22*(1-theta) + 5.31*log(theta)) * 100
c
	if(T.gt.215)then
	  theta = 300.0/T
	  es = 1e5/(41.51*theta**(-5)*10**(9.834*theta-10.0))
	else
	  es = 0
	endif
	pvapsat = es
	end
c************************************************************************
	complex function refdry(nu,T,Pdry,Pvap)
c
	implicit none
	real nu,T,Pdry,Pvap
c
c  Determine the complex refractivity of the dry components of the
c  atmosphere.
c
c  Inputs:
c    nu		Observing frequency (Hz).
c    T		Temperature (Kelvin).
c    Pdry	Partial pressure of dry components (Pascals).
c    Pvap	Partial pressure of water vapour (Pascals).
c
c  Reference:
c    Liebe, An updated model for millimeter wave propogation in moist air,
c    Radio Science, 20, 1069-1089 (1985).
c------------------------------------------------------------------------
	integer i
	real theta,p,e,f
	double precision nr,ni
	double precision x,y,z,delta,gamma,S
	real ap,gamma0
c
c  Table of microwave oxygen lines and their parameters.
c
	integer NL
	parameter(NL=48)
	double precision nu0(NL)
	real a1(NL),a2(NL),a3(NL),a4(NL),a5(NL),a6(NL)
c
	data (nu0(i),a1(i),a2(i),a3(i),a4(i),a5(i),a6(i),i=1,18)/	
     *  49.452379D00,    0.12E-6, 11.830,  8.40E-3, 0.0,  5.60E-3,  1.7,
     *  49.962257D00,    0.34E-6, 10.720,  8.50E-3, 0.0,  5.60E-3,  1.7,
     *  50.474238D00,    0.94E-6,  9.690,  8.60E-3, 0.0,  5.60E-3,  1.7,
     *  50.987748D00,    2.46E-6,  8.690,  8.70E-3, 0.0,  5.50E-3,  1.7,
     *  51.503350D00,    6.08E-6,  7.740,  8.90E-3, 0.0,  5.60E-3,  1.8,
     *  52.021409D00,   14.14E-6,  6.840,  9.20E-3, 0.0,  5.50E-3,  1.8,
     *  52.542393D00,   31.02E-6,  6.000,  9.40E-3, 0.0,  5.70E-3,  1.8,
     *  53.066906D00,   64.10E-6,  5.220,  9.70E-3, 0.0,  5.30E-3,  1.9,
     *  53.595748D00,  124.70E-6,  4.480, 10.00E-3, 0.0,  5.40E-3,  1.8,
     *  54.129999D00,  228.00E-6,  3.810, 10.20E-3, 0.0,  4.80E-3,  2.0,
     *  54.671157D00,  391.80E-6,  3.190, 10.50E-3, 0.0,  4.80E-3,  1.9,
     *  55.221365D00,  631.60E-6,  2.620, 10.79E-3, 0.0,  4.17E-3,  2.1,
     *  55.783800D00,  953.50E-6,  2.115, 11.10E-3, 0.0,  3.75E-3,  2.1,
     *  56.264777D00,  548.90E-6,  0.010, 16.46E-3, 0.0,  7.74E-3,  0.9,
     *  56.363387D00, 1344.00E-6,  1.655, 11.44E-3, 0.0,  2.97E-3,  2.3,
     *  56.968180D00, 1763.00E-6,  1.255, 11.81E-3, 0.0,  2.12E-3,  2.5,
     *  57.612481D00, 2141.00E-6,  0.910, 12.21E-3, 0.0,  0.94E-3,  3.7,
     *  58.323874D00, 2386.00E-6,  0.621, 12.66E-3, 0.0, -0.55E-3, -3.1/
c
	data (nu0(i),a1(i),a2(i),a3(i),a4(i),a5(i),a6(i),i=19,36)/	
     *  58.446589D00, 1457.00E-6,  0.079, 14.49E-3, 0.0,  5.97E-3,  0.8,
     *  59.164204D00, 2404.00E-6,  0.386, 13.19E-3, 0.0, -2.44E-3,  0.1,
     *  59.590982D00, 2112.00E-6,  0.207, 13.60E-3, 0.0,  3.44E-3,  0.5,
     *  60.306057D00, 2124.00E-6,  0.207, 13.82E-3, 0.0, -4.13E-3,  0.7,
     *  60.434775D00, 2461.00E-6,  0.386, 12.97E-3, 0.0,  1.32E-3, -1.0,
     *  61.150558D00, 2504.00E-6,  0.621, 12.48E-3, 0.0, -0.36E-3,  5.8,
     *  61.800152D00, 2298.00E-6,  0.910, 12.07E-3, 0.0, -1.59E-3,  2.9,
     *  62.411212D00, 1933.00E-6,  1.255, 11.71E-3, 0.0, -2.66E-3,  2.3,
     *  62.486253D00, 1517.00E-6,  0.078, 14.68E-3, 0.0, -4.77E-3,  0.9,
     *  62.997974D00, 1503.00E-6,  1.660, 11.39E-3, 0.0, -3.34E-3,  2.2,
     *  63.568515D00, 1087.00E-6,  2.110, 11.08E-3, 0.0, -4.17E-3,  2.0,
     *  64.127764D00,  733.50E-6,  2.620, 10.78E-3, 0.0, -4.48E-3,  2.0,
     *  64.678900D00,  463.50E-6,  3.190, 10.50E-3, 0.0, -5.10E-3,  1.8,
     *  65.224067D00,  274.80E-6,  3.810, 10.20E-3, 0.0, -5.10E-3,  1.9,
     *  65.764769D00,  153.00E-6,  4.480, 10.00E-3, 0.0, -5.70E-3,  1.8,
     *  66.302088D00,   80.09E-6,  5.220,  9.70E-3, 0.0, -5.50E-3,  1.8,
     *  66.836827D00,   39.46E-6,  6.000,  9.40E-3, 0.0, -5.90E-3,  1.7,
     *  67.369595D00,   18.32E-6,  6.840,  9.20E-3, 0.0, -5.60E-3,  1.8/
c
	data (nu0(i),a1(i),a2(i),a3(i),a4(i),a5(i),a6(i),i=37,48)/	
     *  67.900862D00,    8.01E-6,  7.740,  8.90E-3, 0.0, -5.80E-3,  1.7,
     *  68.431001D00,    3.30E-6,  8.690,  8.70E-3, 0.0, -5.70E-3,  1.7,
     *  68.960306D00,    1.28E-6,  9.690,  8.60E-3, 0.0, -5.60E-3,  1.7,
     *  69.489021D00,    0.47E-6, 10.720,  8.50E-3, 0.0, -5.60E-3,  1.7,
     *  70.017342D00,    0.16E-6, 11.830,  8.40E-3, 0.0, -5.60E-3,  1.7,
     * 118.750341D00,  945.00E-6,  0.000, 15.92E-3, 0.0, -0.44E-3,  0.9,
     * 368.498350D00,   67.90E-6,  0.020, 19.20E-3, 0.6,  0.00E00,  1.0,
     * 424.763120D00,  638.00E-6,  0.011, 19.16E-3, 0.6,  0.00E00,  1.0,
     * 487.249370D00,  235.00E-6,  0.011, 19.20E-3, 0.6,  0.00E00,  1.0,
     * 715.393150D00,   99.60E-6,  0.089, 18.10E-3, 0.6,  0.00E00,  1.0,
     * 773.838730D00,  671.00E-6,  0.079, 18.10E-3, 0.6,  0.00E00,  1.0,
     * 834.145330D00,  180.00E-6,  0.079, 18.10E-3, 0.6,  0.00E00,  1.0/
c
c  Convert to the units of Liebe.
c
	theta = 300/T
	e = 0.001*Pvap
	p = 0.001*Pdry
	f = nu * 1e-9
c
	ap =1.4e-10*(1-1.2e-5*f**1.5)
	gamma0 = 5.6e-3*(p+1.1*e)*theta**0.8
	nr = 2.588*p*theta +
     *	     3.07e-4*(1.0/(1.0+(f/gamma0)**2)-1)*p*theta*theta
	ni = (2*3.07e-4/(gamma0*(1+(f/gamma0)**2)*(1+(f/60)**2)) + 
     *	     ap*p*theta**2.5)*f*p*theta*theta
c
c  Sum the contributions of the lines.
c
	do i=1,NL
	  S = a1(i)*p*theta**3*exp(a2(i)*(1-theta))
	  gamma = a3(i)*(p*theta**(0.8-a4(i)) + 1.1*e*theta)
	  delta = a5(i)*p*theta**a6(i)
	  x = (nu0(i)-f)*(nu0(i)-f) + gamma*gamma
	  y = (nu0(i)+f)*(nu0(i)+f) + gamma*gamma
	  z = (nu0(i)+gamma*gamma/nu0(i))
	  nr = nr + S*( (z-f)/x + (z+f)/y - 2/nu0(i) + 
     *		delta*(1/X-1/Y)*gamma*f/nu0(i) )
	  ni = ni + S*( (1/X+1/Y)*gamma*f/nu0(i) -
     *		delta*((nu0(i)-f)/X + (nu0(i)+f)/Y)*f/nu0(i) )
	enddo
c
c  Return the result.
c
	refdry = cmplx(real(nr),real(ni))
c
	end
c************************************************************************
	complex function refvap(nu,T,Pdry,Pvap)
c
	implicit none
	real nu,T,Pdry,Pvap
c
c  Determine the complex refractivity of the water vapour monomers.
c
c  Inputs:
c    nu		Observing frequency (Hz).
c    T		Temperature (Kelvin).
c    Pdry	Partial pressure of dry components (Pascals).
c    Pvap	Partial pressure of water vapour (Pascals).
c
c  Reference:
c    Liebe, An updated model for millimeter wave propogation in moist air,
c    Radio Science, 20, 1069-1089 (1985).
c------------------------------------------------------------------------
	integer i
	real theta,p,e,f
	double precision nr,ni
	double precision x,y,z,gamma,S
c
c  Table of the microwave water lines.
c
	integer NL
	parameter(NL=30)
	double precision nu0(NL)
	real b1(NL),b2(NL),b3(NL)
c
	data (nu0(i),b1(i),b2(i),b3(i),i=1,18)/
     *  22.235080D00,  0.1090, 2.143, 27.84E-3,
     *  67.813960D00,  0.0011, 8.730, 27.60E-3,
     * 119.995940D00,  0.0007, 8.347, 27.00E-3,
     * 183.310117D00,  2.3000, 0.653, 28.35E-3,
     * 321.225644D00,  0.0464, 6.156, 21.40E-3,
     * 325.152919D00,  1.5400, 1.515, 27.00E-3,
     * 336.187000D00,  0.0010, 9.802, 26.50E-3,
     * 380.197372D00, 11.9000, 1.018, 27.60E-3,
     * 390.134508D00,  0.0044, 7.318, 19.00E-3,
     * 437.346667D00,  0.0637, 5.015, 13.70E-3,
     * 439.150812D00,  0.9210, 3.561, 16.40E-3,
     * 443.018295D00,  0.1940, 5.015, 14.40E-3,
     * 448.001075D00, 10.6000, 1.370, 23.80E-3,
     * 470.888947D00,  0.3300, 3.561, 18.20E-3,
     * 474.689127D00,  1.2800, 2.342, 19.80E-3,
     * 488.491133D00,  0.2530, 2.814, 24.90E-3,
     * 503.568532D00,  0.0374, 6.693, 11.50E-3,
     * 504.482692D00,  0.0125, 6.693, 11.90E-3/
c
	data (nu0(i),b1(i),b2(i),b3(i),i=19,30)/
     * 556.936002D00, 510.000, 0.114, 30.00E-3,
     * 620.700807D00,  5.0900, 2.150, 22.30E-3,
     * 658.006500D00,  0.2740, 7.767, 30.00E-3,
     * 752.033227D00, 250.000, 0.336, 28.60E-3,
     * 841.073593D00,  0.0130, 8.113, 14.10E-3,
     * 859.865000D00,  0.1330, 7.989, 28.60E-3,
     * 899.407000D00,  0.0550, 7.845, 28.60E-3,
     * 902.555000D00,  0.0380, 8.360, 26.40E-3,
     * 906.205524D00,  0.1830, 5.039, 23.40E-3,
     * 916.171582D00,  8.5600, 1.369, 25.30E-3,
     * 970.315022D00,  9.1600, 1.842, 24.00E-3,
     * 987.926764D00, 138.000, 0.178, 28.60E-3/
c
c  Convert to the units of Liebe.
c
	theta = 300/T
	e = 0.001*Pvap
	p = 0.001*Pdry
	f = nu * 1e-9
c
	nr = 2.39*e*theta + 41.6*e*theta*theta +
     *		6.47e-6*f**2.05*e*theta**2.4
	ni = (0.915*1.40e-6*p + 5.41e-5*e*theta*theta*theta)*
     *							f*e*theta**2.5
c
c  Sum the contributions of the lines.
c
	do i=1,NL
	  S = b1(i)*e*theta**3.5*exp(b2(i)*(1-theta))
	  gamma = b3(i)*(p*theta**0.8 + 4.80*e*theta)
	  x = (nu0(i)-f)*(nu0(i)-f) + gamma*gamma
	  y = (nu0(i)+f)*(nu0(i)+f) + gamma*gamma
	  z = (nu0(i)+gamma*gamma/nu0(i))
	  nr = nr + S*((z-f)/x + (z+f)/y - 2/nu0(i))
	  ni = ni + S*((1/X+1/Y)*gamma*f/nu0(i))
	enddo
c
c  Return the result.
c
	refvap = cmplx(real(nr),real(ni))
c
	end

