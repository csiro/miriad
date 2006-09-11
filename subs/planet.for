c************************************************************************
c
c  Ephemerides of the planets.
c
c  History:
c    rjs  xxdec95 Original version.
c    rjs  18dec95 Make sub-Earth point a right handed coordinate system.
c    rjs  17jun96 Added some parameters about the Earth.
c    rjs  04feb01 Added subroutine pltbs.
c    rjs  03mar01 Improved brightness temps for the planets from Ulrich etc.
c
c  Bugs:
c    Need to understand the brightness temperature fluctuations of the
c    planets better (e.g. phase, distance from Sun, inclination of
c    Saturn's rings, weather).
c************************************************************************
	real function pltbs(iplanet,freq)
c
	implicit none
	integer iplanet
	real freq
c
c  Return the brightness temperature of a planet.
c
c  Inputs:
c    iplanet  Planet number in range 1-9.
c    freq     Observing frequency, in GHz.
c  Output:
c    pltbs    Brightness temperature, in Kelvin.
c
c------------------------------------------------------------------------
	integer NVEN
	parameter(NVEN=3)
	real ven(2,NVEN)
c
	integer NMAR
	parameter(NMAR=3)
	real mar(2,NMAR)
c
	integer NJUP
	parameter(NJUP=40)
	real jup(2,NJUP)
c
	integer NSAT
	parameter(NSAT=4)
	real sat(2,NSAT)
c
	integer NPLANET
	parameter(NPLANET=9)
	real pltb(NPLANET)
c
	real pltbintp
c
	save pltb,jup,ven,mar,sat
c------------------------------------------------------------------------
c  Misc planets
c     Ref: Ulich, AJ, 86, 1619 (1981).
c
c	          Mer   Ven   Ear   Mar   Jup   Sat   Ura   Nep   Plu
	data pltb/440.0,  0.0,288.0,  0.0,  0.0,  0.0,134.1,126.9,68.0/
c------------------------------------------------------------------------
c  Venus
c     Ref: Ulich, AJ, 86, 1619 (1981).
c
c                      Freq(GHz)        Temp (Kelvin)
c                      -----            ------
	data ven(1,1) / 31.4/, ven(2,1) /466.0/
	data ven(1,2) / 90.0/, ven(2,2) /367.1/
	data ven(1,3) /150.0/, ven(2,3) /294.0/
c------------------------------------------------------------------------
c  Mars
c     Ref: Ulich, AJ, 86, 1619 (1981).
c
c                     Freq(GHz)         Temp (Kelvin)
c                      -----             -----
	data mar(1,1) / 31.4/, mar(2,1) /194.3/
	data mar(1,2) / 90.0/, mar(2,2) /206.8/
	data mar(1,3) /150.0/, mar(2,3) /206.0/
c------------------------------------------------------------------------
c  Jupiter
c    Ref: Digitised from de Pater & Massie, Icarus, 62, 143 (1985)
c
c                      Freq (GHz)                Temp (Kelvin)
c                      -------------             ------------- 
	data jup(1,1) /9.8380085e-01/, jup(2,1) /4.4896755e+02/
	data jup(1,2) /1.0439904e+00/, jup(2,2) /4.2728614e+02/
	data jup(1,3) /1.1412491e+00/, jup(2,3) /4.0663717e+02/
	data jup(1,4) /1.2725089e+00/, jup(2,4) /3.7669617e+02/
	data jup(1,5) /1.4908442e+00/, jup(2,5) /3.4469027e+02/
	data jup(1,6) /1.6459416e+00/, jup(2,6) /3.2920354e+02/
	data jup(1,7) /1.9093594e+00/, jup(2,7) /3.0442478e+02/
	data jup(1,8) /2.3272980e+00/, jup(2,8) /2.8377581e+02/
	data jup(1,9) /2.8649334e+00/, jup(2,9) /2.6415929e+02/
	data jup(1,10)/3.4920372e+00/, jup(2,10)/2.4970501e+02/
	data jup(1,11)/4.1318879e+00/, jup(2,11)/2.3834808e+02/
	data jup(1,12)/5.5602589e+00/, jup(2,12)/2.2182891e+02/
	data jup(1,13)/6.7773410e+00/, jup(2,13)/2.1253687e+02/
	data jup(1,14)/8.2608296e+00/, jup(2,14)/2.0221239e+02/
	data jup(1,15)/1.0069038e+01/, jup(2,15)/1.9188791e+02/
	data jup(1,16)/1.1680494e+01/, jup(2,16)/1.8362832e+02/
	data jup(1,17)/1.3958190e+01/, jup(2,17)/1.7227139e+02/
	data jup(1,18)/1.6515769e+01/, jup(2,18)/1.5678466e+02/
	data jup(1,19)/1.8970283e+01/, jup(2,19)/1.4646018e+02/
	data jup(1,20)/2.0533338e+01/, jup(2,20)/1.3923304e+02/
	data jup(1,21)/2.2446234e+01/, jup(2,21)/1.3407080e+02/
	data jup(1,22)/2.4056421e+01/, jup(2,22)/1.3407080e+02/
	data jup(1,23)/2.5782114e+01/, jup(2,23)/1.3510324e+02/
	data jup(1,24)/2.6823249e+01/, jup(2,24)/1.3716814e+02/
	data jup(1,25)/3.0205775e+01/, jup(2,25)/1.4439528e+02/
	data jup(1,26)/3.5740420e+01/, jup(2,26)/1.5265487e+02/
	data jup(1,27)/4.0647746e+01/, jup(2,27)/1.5781711e+02/
	data jup(1,28)/4.8574054e+01/, jup(2,28)/1.6401180e+02/
	data jup(1,29)/6.0990659e+01/, jup(2,29)/1.6917404e+02/
	data jup(1,30)/7.2883835e+01/, jup(2,30)/1.7227139e+02/
	data jup(1,31)/9.1514558e+01/, jup(2,31)/1.7330383e+02/
	data jup(1,32)/1.1490771e+02/, jup(2,32)/1.7227139e+02/
	data jup(1,33)/1.6409107e+02/, jup(2,33)/1.6814159e+02/
	data jup(1,34)/1.9803915e+02/, jup(2,34)/1.6814159e+02/
	data jup(1,35)/2.3665677e+02/, jup(2,35)/1.6504425e+02/
	data jup(1,36)/2.8001972e+02/, jup(2,36)/1.6297935e+02/
	data jup(1,37)/3.3462355e+02/, jup(2,37)/1.5884956e+02/
	data jup(1,38)/4.1192584e+02/, jup(2,38)/1.5265487e+02/
	data jup(1,39)/4.8740358e+02/, jup(2,39)/1.4542773e+02/
	data jup(1,40)/5.7671121e+02/, jup(2,40)/1.3303835e+02/
c------------------------------------------------------------------------
c  Saturn
c     Ref: 31.4,90,150 GHz -- Ulich, AJ, 86, 1619 (1981).
c          1.4 GHz         -- ATCA measurement by Sault (1999?)
c
c                      Freq(GHz)         Temp (Kelvin)
c                      -----             -----
	data sat(1,1) /  1.4/, sat(2,1) /194.0/
	data sat(1,2) / 31.4/, sat(2,2) /133.0/
	data sat(1,3) / 90.0/, sat(2,3) /149.3/
	data sat(1,4) /150.0/, sat(2,4) /137.0/
c
c  Process the appropriate planet.
c
	goto(100,20,100,40,50,60,100,100,100),iplanet
c
c Venus
c
  20	pltbs = pltbintp(freq,ven,NVEN)
	return
c
c Mars
c
  40	pltbs = pltbintp(freq,mar,NMAR)
	return
c
c Jupiter
c
  50	pltbs = pltbintp(freq,jup,NJUP)
	return
c
c Saturn
c
  60	pltbs = pltbintp(freq,sat,NSAT)
	return
c
c  All the rest.
c	
 100	pltbs = pltb(iplanet)
c
	end
c************************************************************************
	real function pltbintp(freq,planet,np)
c
	implicit none
	integer np
	real freq,planet(2,np)
c
c  Interpolate the brightness temperature of Jupiter.
c
c------------------------------------------------------------------------
	integer i,j,k
	real fi,fj
c
	if(freq.le.planet(1,1))then
	  pltbintp = planet(2,1)
	else if(freq.ge.planet(1,np))then
	  pltbintp = planet(2,np)
	else
	  i = 1
	  j = np
	  dowhile(i+1.lt.j)
	    k = (i+j)/2
	    if(freq.gt.planet(1,k))then
	      i = k
	    else
	      j = k
	    endif
	  enddo
	  fj = (freq-planet(1,i))/(planet(1,j)-planet(1,i))
	  fi = 1-fj
	  pltbintp = fj*planet(2,j) + fi*planet(2,i)
	endif
c
	end
c************************************************************************
	integer function plLook(source)
c
	implicit none
	character source*(*)
c
c  Identify a planet.
c
c------------------------------------------------------------------------
	character source1*32
	integer ip
c
	integer NPLANETS
	parameter(NPLANETS=9)
	character planets(NPLANETS)*8
	integer np(NPLANETS)
c
c  Externals.
c
	integer binsrcha
c
        data planets/'earth   ','jupiter ','mars    ','mercury ',
     *    'neptune ','pluto   ','saturn  ','uranus  ','venus   '/
	data np     / 3,         5,        4,          1,
     *	   8,	      9,         6,	   7,	       2/
c
	source1 = source
	call lcase(source1)
	ip = binsrcha(source1,planets,NPLANETS)
	if(ip.gt.0)ip = np(ip)
	plLook = ip
	end
c************************************************************************
	subroutine plradec(jday,np,ra,dec)
c
	implicit none
	integer np
	double precision jday,ra,dec
c
c  Determine the J2000 RA/DEC (corrected for light travel time) of
c  a planet.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer EARTH
	parameter(EARTH=3)
c
	double precision pv(6),pve(6),dt,dist
	integer jstat1,jstat2
c
	call plobseph(jday,np,pv,jstat1)
	call plobseph(jday,EARTH,pve,jstat2)
	if(jstat1.ne.0.or.jstat2.ne.0)
     *	  call bug('f','Unable to obtain planet parameters, in plradec')
c
c  Compute distance, and correct for light travel time.
c
	pv(1) = pv(1) - pve(1)
	pv(2) = pv(2) - pve(2)
	pv(3) = pv(3) - pve(3)
	dist = sqrt( pv(1)*pv(1) + pv(2)*pv(2) + pv(3)*pv(3) )
	dt = 1e3 * dist / cmks
	pv(1) = pv(1) - pv(4)*dt
	pv(2) = pv(2) - pv(5)*dt
	pv(3) = pv(3) - pv(6)*dt
	dist = sqrt( pv(1)*pv(1) + pv(2)*pv(2) + pv(3)*pv(3) )
	pv(1) = pv(1) / dist
	pv(2) = pv(2) / dist
	pv(3) = pv(3) / dist
c
c  Convert to ra/dec.
c
	call lmn2sph(pv,ra,dec)
	end
c************************************************************************
	subroutine plpar(jday,np,sub,dist,bmaj,bmin,bpa)
c
	implicit none
	double precision jday,sub(3),dist
	real bmaj,bmin,bpa
	integer np
c
c  Return information about the apparent properties of a planet
c  at a given time.
c
c  NOTE: This uses the Earth/Moon barycentre, rather than the geocentre!
c
c  Input:
c    jday	Julian day, in the TDB timescale.
c    np		Planet number.
c  Output:
c    sub	The unit vector of the sub-Earth point, in a
c		planetocentric system.
c    dist	Distance of the planet centre from the geocenter, in km.
c    bmaj,bmin,bpa The apparent major and minor planet axes, in radians, and
c		its position angle. The position angle is in an equatorial
c		J2000 system, measured from north through east.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer EARTH
	parameter(EARTH=3)
	integer jstat1,jstat2,i
	double precision pv(6),pve(6),dt,alpha,delta,W,r,f
	double precision x(3),y(3),z(3),xd(3),yd(3),zd(3)
	double precision cosW,sinW,north(3),nuy,nuz,ydd,zdd
c
	save north
	data north/0.d0,0.d0,1.d0/
	
c
c  Get the planet and Earth positions and velocities.
c
	call plobseph(jday,np,pv,jstat1)
	call plobseph(jday,EARTH,pve,jstat2)
	if(jstat1.ne.0.or.jstat2.ne.0)
     *	  call bug('f','Failed to get planet info')
c
c  Compute distance, and correct for light travel time.
c
	pv(1) = pv(1) - pve(1)
	pv(2) = pv(2) - pve(2)
	pv(3) = pv(3) - pve(3)
	dist = sqrt( pv(1)*pv(1) + pv(2)*pv(2) + pv(3)*pv(3) )
	dt = 1e3 * dist / cmks
	pv(1) = pv(1) - pv(4)*dt
	pv(2) = pv(2) - pv(5)*dt
	pv(3) = pv(3) - pv(6)*dt
	dist = sqrt( pv(1)*pv(1) + pv(2)*pv(2) + pv(3)*pv(3) )
	pv(1) = pv(1) / dist
	pv(2) = pv(2) / dist
	pv(3) = pv(3) / dist
c
c  Get physical ephemerides of the planet corrected for light travel
c  time.
c
	call plphyeph(jday-dt/86400.d0,np,alpha,delta,w,r,f)
c
c  Compute unit vectors in the equatorial system of the planetcentric
c  X,Y,Z axes for W=0.
c
	call sph2lmn(alpha,delta,z)
	call sph2lmn(alpha+0.5*dpi,0.d0,x)
	call veccross(z,x,y)
c
c  Rotate the x and y axes by W.
c
	cosW = cos(W)
	sinW = sin(W)
	do i=1,3
	  xd(i) =  cosW*X(i) + sinW*Y(i)
	  yd(i) = -sinW*X(i) + cosW*Y(i)
	enddo
c
c  Set the sub-Earth vector.
c
	sub(1) = - ( pv(1)*xd(1) + pv(2)*xd(2) + pv(3)*xd(3) )
	sub(2) = - ( pv(1)*yd(1) + pv(2)*yd(2) + pv(3)*yd(3) )
	sub(3) = - ( pv(1)*z(1)  + pv(2)*z(2)  + pv(3)*z(3) )
c
c  Set the major, minor and position angle of the planetary equator.
c
	bmaj = 2*r/dist
	bmin = bmaj*(1 - f + f*sub(3)*sub(3))
c
c  Compute the position angle.
c
	call veccross(north,pv,yd)
	call veccross(pv,yd,zd)
	ydd = sqrt(yd(1)*yd(1) + yd(2)*yd(2) + yd(3)*yd(3))
	zdd = sqrt(zd(1)*zd(1) + zd(2)*zd(2) + zd(3)*zd(3))
	nuy = ( yd(1)*z(1) + yd(2)*z(2) + yd(3)*z(3) ) / ydd
	nuz = ( zd(1)*z(1) + zd(2)*z(2) + zd(3)*z(3) ) / zdd
	bpa = atan2(nuy,nuz)
	end
c************************************************************************
	subroutine plphyeph(jday,np,alpha,delta,w,r,f)
c
	implicit none
	double precision jday,alpha,delta,w,r,f
	integer np
c
c
c  Physical ephemerides of the planets. This is derived from
c  Explanatory Supplement to the Astronomical Almanac, Section 7.4
c  See this reference for a more comprehensive description.
c
c  Input:
c    jday	Time, as a Julian day, in the TDB timescale.
c    np		Planet number.
c		(1=Mercury,2=Venus,4=Mars,5=Jupiter,6=Saturn,7=Uranus,
c		 8=Neptune,9=Pluto)
c   Output:
c    alpha,delta The right ascension and declination of date (J2000 system)
c		of the objects north pole.  The units are radians.
c    w		Position of the prime meridian of the object with respect
c		to an inertial frame. The units are radians.
c    r		Equatorial radius of the object (in the radio!) in km.
c    f		Flattening of the polar radius.
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision J2000
	parameter(J2000=2451545.0d0)
c
	double precision t,TT,N
c
	integer NOBJ
	integer MERCURY,VENUS,MARS,JUPITER,SATURN,URANUS,NEPTUNE,PLUTO
	integer EARTH
	parameter(NOBJ=9)
	parameter(MERCURY=1,VENUS=2,EARTH=3,MARS=4,JUPITER=5,SATURN=6,
     *		  URANUS=7,NEPTUNE=8,PLUTO=9)
	double precision alpha0(NOBJ),dalpha(NOBJ)
	double precision delta0(NOBJ),ddelta(NOBJ)
	double precision W0(NOBJ),dW(NOBJ)
	double precision radius(NOBJ),flat(NOBJ)
c
	data alpha0(MERCURY),dalpha(MERCURY),
     *       delta0(MERCURY),ddelta(MERCURY),
     *	     W0(MERCURY),dW(MERCURY),radius(MERCURY),flat(MERCURY)/
     *       281.01d0,  -0.033d0,      61.45d0,  -0.005d0,
     *	     329.71d0,   6.1385025d0,  2439.7d0,  0.d0/
c
	data alpha0(VENUS),  dalpha(VENUS)  ,
     *       delta0(VENUS),  ddelta(VENUS)  ,
     *	     W0(VENUS),  dW(VENUS),  radius(VENUS),  flat(VENUS)  /
     *       272.72d0,  -0d0,          67.15,     0.d0,
     *	     160.26d0,   -1.4813596d0, 6501.9d0,  0.d0/
c
	data alpha0(EARTH),  dalpha(EARTH)  ,
     *       delta0(EARTH),  ddelta(EARTH)  ,
     *	     W0(EARTH),  dW(EARTH),  radius(EARTH),  flat(EARTH)  /
     *       0d0,	0d0,	0d0,	0d0,
     *	     0d0,   	0d0, 	6378.140d0,  	3.3528134d-3/
c
	data alpha0(MARS)   ,dalpha(MARS)   ,
     *       delta0(MARS)   ,ddelta(MARS)   ,
     *	     W0(MARS)   ,dW(MARS)   ,radius(MARS)   ,flat(MARS)   /
     *       317.681d0,  -0.108d0,      52.886d0,  -0.061d0,
     *	     176.868d0, 350.8919830d0,  3397.0d0,  0.0065d0/
c
	data alpha0(JUPITER),dalpha(JUPITER),
     *       delta0(JUPITER),ddelta(JUPITER),
     *	     W0(JUPITER),dW(JUPITER),radius(JUPITER),flat(JUPITER)/
     *       268.05d0,  -0.009d0,      64.49d0,   0.003d0,
     *	     284.95d0, 870.5360000d0, 71492.0d0,  0.06487d0/
c
	data alpha0(SATURN) ,dalpha(SATURN) ,
     *       delta0(SATURN) ,ddelta(SATURN) ,
     *	     W0(SATURN) ,dW(SATURN) ,radius(SATURN) ,flat(SATURN) /
     *        40.58d0,  -0.036d0,      83.54d0,  -0.004d0,
     *	      38.90d0, 810.7939024d0, 60268.0d0,  0.09796d0/
c
	data alpha0(URANUS) ,dalpha(URANUS) ,
     *       delta0(URANUS) ,ddelta(URANUS) ,
     *	     W0(URANUS) ,dW(URANUS) ,radius(URANUS) ,flat(URANUS) /
     *       257.43d0,   0.d0,        -15.10d0,   0.d0,
     *	     203.81d0,-501.1600928d0, 25559.0d0,  0.02293d0/
c
	data alpha0(NEPTUNE),dalpha(NEPTUNE),
     *       delta0(NEPTUNE),ddelta(NEPTUNE),
     *	     W0(NEPTUNE),dW(NEPTUNE),radius(NEPTUNE),flat(NEPTUNE)/
     *       299.36d0,   0.07d0,       43.46d0,  -0.51d0,
     *	     253.18d0, 536.3128492d0, 24764.0d0,  0.0171d0/
c
	data alpha0(PLUTO)  ,dalpha(PLUTO)  ,
     *       delta0(PLUTO)  ,ddelta(PLUTO)  ,
     *	     W0(PLUTO)  ,dW(PLUTO)  ,radius(PLUTO)  ,flat(PLUTO)  /
     *       313.02d0,   0.d0,          9.09d0,   0.d0,
     *	     236.77d0, -56.3623195d0,  1151.0d0,  0.d0/
c
	if(np.lt.1.or.np.gt.NOBJ)call bug('f',
     *	  'Invalid planet number in PLPHYEPH')
c
	t  = jday - J2000
	TT = t/36535.0d0
c
	if(np.eq.NEPTUNE)then
	  N = dpi/180.d0 * (357.85 + 52.316*TT)
	  alpha = dpi/180.d0 * ( alpha0(np) + dalpha(np)*sin(N) )
	  delta = dpi/180.d0 * ( delta0(np) + ddelta(np)*cos(N) )
	  W =     dpi/180.d0 * ( W0(np) + dW(np)*t - 0.48*sin(N) )
	else
	  alpha = dpi/180.d0 * ( alpha0(np) + dalpha(np)*TT )
	  delta = dpi/180.d0 * ( delta0(np) + ddelta(np)*TT )
	  W =     dpi/180.d0 * ( W0(np) + dW(np)*t )
	endif
	r = radius(np)
	f = flat(np)
	end
c************************************************************************
      SUBROUTINE PLOBSEPH (DATE, NP, PV, JSTAT)
*+
*     - - - - - - - - -
*      P L O B S E P H
*     - - - - - - - - -
*
*  Approximate heliocentric position and velocity of a specified
*  major planet (Mercury, Venus, EMB, Mars, Jupiter, Saturn, Uranus
*  or Neptune).
*
*  Given:
*     DATE      d      Julian Date
*     NP        i      planet (1=Mercury, 2=Venus, 3=EMB ... 8=Neptune)
*
*  Returned:
*     PV        d(6)   heliocentric x,y,z,xdot,ydot,zdot, J2000
*                                           equatorial triad (km,km/s)
*     JSTAT     i      status: -1 = illegal NP (outside 1-8)
*                               0 = OK
*                              +1 = warning: date outside 1000-3000
*                              +2 = warning: solution didn't converge
*
*  NOTE the following differences to convert it to follow the Miriad
*  conventions:
*    * DATE is now Julian date (not modified Julian date).
*    * Positions and velocities are km and km/s, no AU and AU/s
*
*  Notes
*
*  1  The epoch, DATE, is in the TDB timescale and is a
*     Julian Date.
*
*  2  The reference frame is equatorial and is with respect to the
*     mean equinox and ecliptic of epoch J2000.
*
*  3  If an NP value outside the range 1-8 is supplied, an error
*     status (JSTAT = -1) is returned and the PV vector set to zeroes.
*
*  4  The algorithm is due to J.L. Simon, P. Bretagnon, J. Chapront,
*     M. Chapront-Touze, G. Francou and J. Laskar (Bureau des
*     Longitudes, Paris, France).
*
*  5  Comparisons of the present routine with the JPL DE200 ephemeris
*     give the following RMS errors over the interval 1960-2025:
*
*                      position (km)     speed (metre/sec)
*
*        Mercury            334               0.437
*        Venus             1060               0.855
*        EMB               2010               0.815
*        Mars              7690               1.98
*        Jupiter          71700               7.70
*        Saturn          199000              19.4
*        Uranus          564000              16.4
*        Neptune         158000              14.4
*
*     From comparisons with DE102, Simon et al quote the following
*     longitude accuracies over the interval 1800-2200:
*
*        Mercury                 4"
*        Venus                   5"
*        EMB                     6"
*        Mars                   17"
*        Jupiter                71"
*        Saturn                 81"
*        Uranus                 86"
*        Neptune                11"
*
*     Over the interval 1000-3000, the accuracy is better than 1.5
*     times that over 1800-2200.  Outside the interval 1000-3000 the
*     accuracy declines.
*
*  6  The present SLALIB implementation differs from the original
*     Simon et al Fortran code in the following respects.  None of
*     the changes affects the result significantly.
*
*       *  The result is returned only in equatorial Cartesian form;
*          the ecliptic longitude, latitude and radius vector are not
*          returned.
*
*       *  The result is in the J2000 equatorial frame, not ecliptic.
*
*       *  The velocity is in km per second, not AU per day.
*
*       *  Everything is done in-line:  there are no calls to other
*          routines.
*
*       *  Different error/warning status values are used.
*
*       *  A different Kepler's-equation-solver is used (avoiding
*          use of COMPLEX*16).
*
*       *  Polynomials in T are nested to minimize rounding errors.
*
*       *  Explicit double-precision constants are used to avoid
*          mixed-mode expressions.
*
*       *  There are other, cosmetic, changes to comply with
*          Starlink/SLALIB style guidelines.
*
*  7  For NP=3 the result is for the Earth-Moon Barycentre.  To
*     obtain the heliocentric position and velocity of the Earth,
*     either use the SLALIB routine sla_EVP or call sla_DMOON and
*     subtract 0.012150581 times the geocentric Moon vector from
*     the EMB vector produced by the present routine.  (The Moon
*     vector should be precessed to J2000 first, but this can
*     be omitted for modern epochs without introducing significant
*     inaccuracy.)
*
*  8  The status, JSTAT, indicates the most serious condition
*     encountered, where illegal NP is considered the most serious,
*     followed by failure to converge, then remote epoch.
*
*  Reference:  Astron. Astrophys. 282, 663 (1994).
*
*  P.T.Wallace   Starlink   24 November 1994
*-
      IMPLICIT NONE

      DOUBLE PRECISION DATE
      INTEGER NP
      DOUBLE PRECISION PV(6)
      INTEGER JSTAT

*  Maximum number of iterations allowed to solve Kepler's equation
      INTEGER KMAX
      PARAMETER (KMAX=10)

*  2Pi, arcsec to radians
      DOUBLE PRECISION D2PI,AS2R
      PARAMETER (D2PI=6.283185307179586476925286766559D0,
     :           AS2R=4.848136811095359935899141023579D-6)

*  Sin and cos of J2000 mean obliquity (IAU 1976)
      DOUBLE PRECISION SINEPS,COSEPS
      PARAMETER (SINEPS=0.3977771559319137D0,
     :           COSEPS=0.9174820620691818D0)

*  AU to km
      DOUBLE PRECISION AUKM
      PARAMETER (AUKM=149.597870D6)

*  Gaussian constant / seconds per day
      DOUBLE PRECISION GKS
      PARAMETER (GKS=0.017202098950D0/86400D0)

      INTEGER I,J,K
      DOUBLE PRECISION AMAS(8),A(3,8),DLM(3,8),E(3,8),
     :                 PI(3,8),DINC(3,8),OMEGA(3,8),
     :                 KP(9,8),CA(9,8),SA(9,8),
     :                 KQ(10,8),CL(10,8),SL(10,8),
     :                 T,DA,DL,DE,DP,DI,DO,DMU,ARGA,ARGL,AM,AE,
     :                 DAE,AE2,AT,R,V,SI2,XQ,XP,TL,XSW,XCW,XM2,
     :                 XF,CI2,XMS,XMC,XPXQ2,X,Y,Z

*  Planetary inverse masses
      DATA AMAS / 6023600D0,408523.5D0,328900.5D0,3098710D0,
     :            1047.355D0,3498.5D0,22869D0,19314D0 /

*
*  Tables giving the mean Keplerian elements, limited to T**2 terms:
*
*         A       semi-major axis (AU)
*         DLM     mean longitude (degree and arcsecond)
*         E       eccentricity
*         PI      longitude of the perihelion (degree and arcsecond)
*         DINC    inclination (degree and arcsecond)
*         OMEGA   longitude of the ascending node (degree and arcsecond)
*
      DATA A /
     :  0.3870983098D0,             0D0,      0D0,
     :  0.7233298200D0,             0D0,      0D0,
     :  1.0000010178D0,             0D0,      0D0,
     :  1.5236793419D0,           3D-10,      0D0,
     :  5.2026032092D0,       19132D-10,  -39D-10,
     :  9.5549091915D0, -0.0000213896D0,  444D-10,
     : 19.2184460618D0,       -3716D-10,  979D-10,
     : 30.1103868694D0,      -16635D-10,  686D-10 /
*
      DATA DLM /
     : 252.25090552D0, 5381016286.88982D0,  -1.92789D0,
     : 181.97980085D0, 2106641364.33548D0,   0.59381D0,
     : 100.46645683D0, 1295977422.83429D0,  -2.04411D0,
     : 355.43299958D0,  689050774.93988D0,   0.94264D0,
     :  34.35151874D0,  109256603.77991D0, -30.60378D0,
     :  50.07744430D0,   43996098.55732D0,  75.61614D0,
     : 314.05500511D0,   15424811.93933D0,  -1.75083D0,
     : 304.34866548D0,    7865503.20744D0,   0.21103D0/
*
      DATA E /
     : 0.2056317526D0,  0.0002040653D0,      -28349D-10,
     : 0.0067719164D0, -0.0004776521D0,       98127D-10,
     : 0.0167086342D0, -0.0004203654D0, -0.0000126734D0,
     : 0.0934006477D0,  0.0009048438D0,      -80641D-10,
     : 0.0484979255D0,  0.0016322542D0, -0.0000471366D0,
     : 0.0555481426D0, -0.0034664062D0, -0.0000643639D0,
     : 0.0463812221D0, -0.0002729293D0,  0.0000078913D0,
     : 0.0094557470D0,  0.0000603263D0,            0D0 /
*
      DATA PI /
     :  77.45611904D0,  5719.11590D0,   -4.83016D0,
     : 131.56370300D0,   175.48640D0, -498.48184D0,
     : 102.93734808D0, 11612.35290D0,   53.27577D0,
     : 336.06023395D0, 15980.45908D0,  -62.32800D0,
     :  14.33120687D0,  7758.75163D0,  259.95938D0,
     :  93.05723748D0, 20395.49439D0,  190.25952D0,
     : 173.00529106D0,  3215.56238D0,  -34.09288D0,
     :  48.12027554D0,  1050.71912D0,   27.39717D0 /
*
      DATA DINC /
     : 7.00498625D0, -214.25629D0,   0.28977D0,
     : 3.39466189D0,  -30.84437D0, -11.67836D0,
     :          0D0,  469.97289D0,  -3.35053D0,
     : 1.84972648D0, -293.31722D0,  -8.11830D0,
     : 1.30326698D0,  -71.55890D0,  11.95297D0,
     : 2.48887878D0,   91.85195D0, -17.66225D0,
     : 0.77319689D0,  -60.72723D0,   1.25759D0,
     : 1.76995259D0,    8.12333D0,   0.08135D0 /
*
      DATA OMEGA /
     :  48.33089304D0,  -4515.21727D0,  -31.79892D0,
     :  76.67992019D0, -10008.48154D0,  -51.32614D0,
     : 174.87317577D0,  -8679.27034D0,   15.34191D0,
     :  49.55809321D0, -10620.90088D0, -230.57416D0,
     : 100.46440702D0,   6362.03561D0,  326.52178D0,
     : 113.66550252D0,  -9240.19942D0,  -66.23743D0,
     :  74.00595701D0,   2669.15033D0,  145.93964D0,
     : 131.78405702D0,   -221.94322D0,   -0.78728D0 /
*
*  Tables for trigonometric terms to be added to the mean elements
*  of the semi-major axes.
*
      DATA KP /
     : 69613, 75645, 88306, 59899, 15746, 71087, 142173,  3086,    0,
     : 21863, 32794, 26934, 10931, 26250, 43725,  53867, 28939,    0,
     : 16002, 21863, 32004, 10931, 14529, 16368,  15318, 32794,    0,
     : 6345,   7818, 15636,  7077,  8184, 14163,   1107,  4872,    0,
     : 1760,   1454,  1167,   880,   287,  2640,     19,  2047, 1454,
     :  574,      0,   880,   287,    19,  1760,   1167,   306,  574,
     :  204,      0,   177,  1265,     4,   385,    200,   208,  204,
     :    0,    102,   106,     4,    98,  1367,    487,   204,    0 /
*
      DATA CA /
     :      4,    -13,    11,    -9,    -9,    -3,    -1,     4,    0,
     :   -156,     59,   -42,     6,    19,   -20,   -10,   -12,    0,
     :     64,   -152,    62,    -8,    32,   -41,    19,   -11,    0,
     :    124,    621,  -145,   208,    54,   -57,    30,    15,    0,
     : -23437,  -2634,  6601,  6259, -1507, -1821,  2620, -2115,-1489,
     :  62911,-119919, 79336, 17814,-24241, 12068,  8306, -4893, 8902,
     : 389061,-262125,-44088,  8387,-22976, -2093,  -615, -9720, 6633,
     :-412235,-157046,-31430, 37817, -9740,   -13, -7449,  9644,    0 /
*
      DATA SA /
     :     -29,    -1,     9,     6,    -6,     5,     4,     0,    0,
     :     -48,  -125,   -26,   -37,    18,   -13,   -20,    -2,    0,
     :    -150,   -46,    68,    54,    14,    24,   -28,    22,    0,
     :    -621,   532,  -694,   -20,   192,   -94,    71,   -73,    0,
     :  -14614,-19828, -5869,  1881, -4372, -2255,   782,   930,  913,
     :  139737,     0, 24667, 51123, -5102,  7429, -4095, -1976,-9566,
     : -138081,     0, 37205,-49039,-41901,-33872,-27037,-12474,18797,
     :       0, 28492,133236, 69654, 52322,-49577,-26430, -3593,    0 /
*
*  Tables giving the trigonometric terms to be added to the mean
*  elements of the mean longitudes.
*
      DATA KQ /
     :  3086, 15746, 69613, 59899, 75645, 88306, 12661, 2658,  0,   0,
     : 21863, 32794, 10931,    73,  4387, 26934,  1473, 2157,  0,   0,
     :    10, 16002, 21863, 10931,  1473, 32004,  4387,   73,  0,   0,
     :    10,  6345,  7818,  1107, 15636,  7077,  8184,  532, 10,   0,
     :    19,  1760,  1454,   287,  1167,   880,   574, 2640, 19,1454,
     :    19,   574,   287,   306,  1760,    12,    31,   38, 19, 574,
     :     4,   204,   177,     8,    31,   200,  1265,  102,  4, 204,
     :     4,   102,   106,     8,    98,  1367,   487,  204,  4, 102 /
*
      DATA CL /
     :     21,   -95, -157,   41,   -5,   42,   23,   30,     0,    0,
     :   -160,  -313, -235,   60,  -74,  -76,  -27,   34,     0,    0,
     :   -325,  -322,  -79,  232,  -52,   97,   55,  -41,     0,    0,
     :   2268,  -979,  802,  602, -668,  -33,  345,  201,   -55,    0,
     :   7610, -4997,-7689,-5841,-2617, 1115, -748, -607,  6074,  354,
     : -18549, 30125,20012, -730,  824,   23, 1289, -352,-14767,-2062,
     :-135245,-14594, 4197,-4030,-5630,-2898, 2540, -306,  2939, 1986,
     :  89948,  2103, 8963, 2695, 3682, 1648,  866, -154, -1963, -283 /
*
      DATA SL /
     :   -342,   136,  -23,   62,   66,  -52,  -33,   17,     0,    0,
     :    524,  -149,  -35,  117,  151,  122,  -71,  -62,     0,    0,
     :   -105,  -137,  258,   35, -116,  -88, -112,  -80,     0,    0,
     :    854,  -205, -936, -240,  140, -341,  -97, -232,   536,    0,
     : -56980,  8016, 1012, 1448,-3024,-3710,  318,  503,  3767,  577,
     : 138606,-13478,-4964, 1441,-1319,-1482,  427, 1236, -9167,-1918,
     :  71234,-41116, 5334,-4935,-1848,   66,  434,-1748,  3780, -701,
     : -47645, 11647, 2166, 3194,  679,    0, -244, -419, -2531,   48 /



*  Validate the planet number
      IF (NP.LT.1.OR.NP.GT.8) THEN
         JSTAT=-1
         DO I=1,6
            PV(I)=0D0
         END DO
      ELSE

*     Time: Julian millennia since J2000
         T=(DATE-2451545.0D0)/365250D0

*     OK status unless remote epoch
         IF (ABS(T).LE.1D0) THEN
            JSTAT=0
         ELSE
            JSTAT=1
         END IF

*     Compute the mean elements
         DA=A(1,NP)+(A(2,NP)+A(3,NP)*T)*T
         DL=(3600D0*DLM(1,NP)+(DLM(2,NP)+DLM(3,NP)*T)*T)*AS2R
         DE=E(1,NP)+(E(2,NP)+E(3,NP)*T)*T
         DP=MOD((3600D0*PI(1,NP)+(PI(2,NP)+PI(3,NP)*T)*T)*AS2R,D2PI)
         DI=(3600D0*DINC(1,NP)+(DINC(2,NP)+DINC(3,NP)*T)*T)*AS2R
         DO=MOD((3600D0*OMEGA(1,NP)+(OMEGA(2,NP)+OMEGA(3,NP)*T)*T)*AS2R,
     :          D2PI)

*     Apply the trigonometric terms
         DMU=0.35953620D0*T
         DO J=1,8
            ARGA=KP(J,NP)*DMU
            ARGL=KQ(J,NP)*DMU
            DA=DA+(CA(J,NP)*COS(ARGA)+SA(J,NP)*SIN(ARGA))*1D-7
            DL=DL+(CL(J,NP)*COS(ARGL)+SL(J,NP)*SIN(ARGL))*1D-7
         END DO
         ARGA=KP(9,NP)*DMU
         DA=DA+T*(CA(9,NP)*COS(ARGA)+SA(9,NP)*SIN(ARGA))*1D-7
         DO J=9,10
            ARGL=KQ(J,NP)*DMU
            DL=DL+T*(CL(J,NP)*COS(ARGL)+SL(J,NP)*SIN(ARGL))*1D-7
         END DO
         DL=MOD(DL,D2PI)

*     Iterative solution of Kepler's equation to get eccentric anomaly
         AM=DL-DP
         AE=AM+DE*SIN(AM)
         DAE=1D0
         K=0
         DO WHILE (K.LT.KMAX.AND.ABS(DAE).GT.1D-12)
            DAE=(AM-AE+DE*SIN(AE))/(1D0-DE*COS(AE))
            AE=AE+DAE
            K=K+1
            IF (K.GE.KMAX) JSTAT=2
         END DO

*     True anomaly
         AE2=AE/2D0
         AT=2D0*ATAN2(SQRT((1D0+DE)/(1D0-DE))*SIN(AE2),COS(AE2))

*     Distance (AU) and speed (radians per second)
         R=DA*(1D0-DE*COS(AE))
         V=GKS*SQRT((1D0+1D0/AMAS(NP))/(DA*DA*DA))

         SI2=SIN(DI/2D0)
         XQ=SI2*COS(DO)
         XP=SI2*SIN(DO)
         TL=AT+DP
         XSW=SIN(TL)
         XCW=COS(TL)
         XM2=2D0*(XP*XCW-XQ*XSW)
         XF=DA/SQRT(1D0-DE*DE)
         CI2=COS(DI/2D0)
         XMS=(DE*SIN(DP)+XSW)*XF
         XMC=(DE*COS(DP)+XCW)*XF
         XPXQ2=2D0*XP*XQ

*     Position (J2000 ecliptic x,y,z in AU)
         X=R*(XCW-XM2*XP)
         Y=R*(XSW+XM2*XQ)
         Z=R*(-XM2*CI2)

*     Rotate to equatorial
         PV(1)=AUKM * X
         PV(2)=AUKM * (Y*COSEPS-Z*SINEPS)
         PV(3)=AUKM * (Y*SINEPS+Z*COSEPS)

*     Velocity (J2000 ecliptic xdot,ydot,zdot in AU/s)
         X=V*((-1D0+2D0*XP*XP)*XMS+XPXQ2*XMC)
         Y=V*((1D0-2D0*XQ*XQ)*XMC-XPXQ2*XMS)
         Z=V*(2D0*CI2*(XP*XMS+XQ*XMC))

*     Rotate to equatorial
         PV(4)=AUKM * X
         PV(5)=AUKM * (Y*COSEPS-Z*SINEPS)
         PV(6)=AUKM * (Y*SINEPS+Z*COSEPS)

      END IF

      END
