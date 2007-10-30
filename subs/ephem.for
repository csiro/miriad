c************************************************************************
c  A set of simple routines to convert between coordinate systems
c  typically found in astronomy.
c
c  History:
c      feb91 rjs  Original version.
c    13jun91 rjs  Added Jul2UT.
c     3jul91 rjs  Some comments in precess, as suggested by pjt.
c     5jul93 rjs  Added xyz2llh, sph2lmn, epo2jul. Use mirconst.h
c    20aug93 rjs  Equation of the equinox, nutation, aberration,
c		  a routine for TAI-UTC and TDT-UTC. Improved precession
c		  and LST routines.
c     4mar94 rjs  Added sunradec.
c     5nov94 rjs  Update leap second table.
c     8dec95 rjs  Add lmn2sph
c
c  General Reference:
c    Explanatory Supplement to the Astronomical Almanac. 1993.
c************************************************************************
c* Jul2UT -- Convert Julian day to UT.
c& rjs
c: utilities
c+
	subroutine Jul2UT(jday,ut)
c
	implicit none
	double precision jday,ut
c
c  A simple conversion from Julian day to UT.
c
c  Input:
c    jday	Julian day.
c  Output:
c    ut		UT, in radians.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	ut = 2*dpi*(jday - int(jday - 0.5) - 0.5)
	end
c************************************************************************
c* PreRotat -- Determine rotation contribution of precession.
c& rjs
c: utilities
c+
	subroutine prerotat(jday,ra,dec,jout,theta)
c
	implicit none
	double precision jday,ra,dec,jout,theta
c
c  A simple routine to determine the rotation of the coordinate
c  system between the mean equatorial coordinates (ra,dec) at "jday",
c  and the apparent coordinates as "jout".
c
c  Input:
c    jday	The Julian day of the reference time.
c    ra,dec	Mean RA and DEC (radians) at the reference time.
c    jout	The Julian day of the new date.
c  Output:
c    theta	The rotation of the coordinate system at time "jout"
c		with respect to that at time "jday" (at (ra,dec)).
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision r0,d0,r1,d1,r2,d2
c
	r0 = ra
	d0 = dec
	call precess(jday,r0,d0,jout,r1,d1)
	call nutate(jout,r1,d1,r0,d0)
	call aberrate(jout,r0,d0,r1,d1)
c
c  Add an arcminute to the declination and precess again.
c
	r0 = ra
	d0 = dec + dpi/(180*60)
	call precess(jday,r0,d0,jout,r2,d2)
	call nutate(jout,r2,d2,r0,d0)
	call aberrate(jout,r0,d0,r2,d2)
c
c  Determine the rotation that resulted.
c
	d0 = (d2 - d1)
	r0 = (r2 - r1) * cos(dec)
	theta = -atan2(r0,d0)
c	
	end
c************************************************************************
c* Precess -- Precess from one mean RA,DEC to another.
c& rjs
c: utilities
c+
	subroutine precess(jday1,ra1,dec1,jday2,ra2,dec2)
c
	implicit none
	double precision jday1,ra1,dec1,jday2,ra2,dec2
c
c  A simple precession routine, to precess from one set of mean
c  equatorial coordinates (RA,DEC), to another at a different epoch.
c  This is accurate to order 0.3 arcsec over 50 years.
c
c  Reference:
c    Explanatory Supplement to the Astronomical Almanac, 1993. p 105-106.
c
c  NOTE: This does not take account of atmospheric refraction,
c  nutation, aberration nor gravitational deflection.
c
c  Input:
c    jday1	Julian day of the known epoch.
c    ra1,dec1	RA,DEC at the jday1 epoch (radians).
c    jday2	Julian day of the new epoch.
c  Output:
c    ra2,dec2	Precessed coordinates (radians).
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	double precision r0,d0,rm,dm,T,M,N
c
	T = (jday1 - 2451545.d0)/36525
	M = dpi/180 * (1.2812323 + (0.0003879 + 0.0000101*T)*T)*T
	N = dpi/180 * (0.5567530 - (0.0001185 + 0.0000116*T)*T)*T
	rm = ra1 - 0.5*(M + N*sin(ra1)*tan(dec1))
	dm = dec1 - 0.5*N*cos(rm)
c
c  J2000 coordinates.
c
	r0 = ra1 - M - N*sin(rm)*tan(dm)
	d0 = dec1 - N*cos(rm)
c
c  Coordinates of the other epoch.
c
	T = (jday2 - 2451545.d0)/36525
	M = dpi/180 * (1.2812323 + (0.0003879 + 0.0000101*T)*T)*T
	N = dpi/180 * (0.5567530 - (0.0001185 + 0.0000116*T)*T)*T
	rm = r0 + 0.5*(M + N*sin(r0)*tan(d0))
	dm = d0 - 0.5*N*cos(rm)
c
	ra2 = r0 + M + N*sin(rm)*tan(dm)
	dec2 = d0 + N*cos(rm)
c
	end
c************************************************************************
c* Parang -- Calculate parallactic angle.
c& rjs
c: utilities
c+
	subroutine parang(obsra,obsdec,lst,latitude,chi)
c
	implicit none
	double precision obsra,obsdec,lst,latitude
	real chi
c
c  This computes the parallactic angle of an alt-az telescope. Accuracy
c  is about 0.0003 radians.
c
c  Input:
c    obsra )	Apparent RA and DEC of the source of interest (radians).
c    obsdec)
c    lst	Local sidereal time (radians).
c    latitude	Observatory geodetic latitude (radians).
c
c  Output:
c    chi	Parallactic angle (radians).
c--
c------------------------------------------------------------------------
	double precision sinq,cosq,ha
c
	ha = lst - obsra
	sinq = cos(latitude)*sin(ha)
	cosq = sin(latitude)*cos(obsdec) - cos(latitude)*sin(obsdec)*
     *					   cos(ha)
c
	chi = atan2(sinq,cosq)
	end
c************************************************************************
c* JulLst -- Convert Julian date to local mean sidereal time.
c& rjs
c: utilities
c+
	subroutine Jullst(jday,long,lst)
c
	implicit none
	double precision jday,lst,long
c
c  Convert from Julian day to local mean sidereal time.
c
c  Reference: Explanatory Supplement to the Astronomical Almanac, p50-52.
c  Accuracy appears to be 0.01 sec of time.
c
c  Input:
c    jday	Julian day of interest.
c    long	Observatory longitude (radians). East of Greenwich is
c		positive.
c  Output:
c    lst	Local mean sidereal time (radians), in the range [0,2*pi].
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision T,UT,GMST
c
	T = nint(jday - 1.0d0) + 0.5d0
	UT = jday - T
	T = (T - 2451545d0) / 36525d0
c
	GMST = 24110.54841 +(8640184.812866 + (0.093104 - 6.2e-6*T)*T)*T
	GMST = GMST / (3600*24) + UT * 
     *	 (1.002737909350795d0 + (5.9006d-11 - 5.9d-15*T)*T)
	lst = 2*dpi*mod(GMST+long/(2*dpi),1.d0)
	if(lst.lt.0)lst = lst + 2*dpi
c
	end
c************************************************************************
c* xyz2llh -- Convert from CIO (x,y,z) to latitude/longitude/height.
c& rjs
c: utilities
c+
	subroutine xyz2llh(x,y,z,lat,long,height)
c
	implicit none
	double precision x,y,z,lat,long,height
c
c  Convert betweena location defined in terms of CIO (x,y,z)
c  coordinates to one in geodetic latitude, longitude, and height
c  above the reference geoid.
c
c  Reference:
c    Kenneth R. Lang, "Astrophysical Formulae", pages 493-497.
c    Values for flattening and equatorial radius from John Reynolds,
c    who says they are the IAU 1976 values.
c
c  Input:
c   x,y,z	CIO coordinates, in meters.
c  Output:
c   lat,long	Geodetic latitude and longitude, in radians.
c   height	Height above the reference geoid (i.e. sea level), in meters.
c--
c------------------------------------------------------------------------
c f  -- Earth's flattening factor
c ae -- Earth's equatorial radius, in meters.
c
	double precision f,ae,fm12
	parameter(f=1/298.257,ae=6.378140d6,fm12=(1-f*(2-f)))
c
	long = atan2(y,x)
	lat =  atan(z/sqrt(x*x+y*y)/fm12)
	height = z/sin(lat) - ae*fm12/sqrt(1-f*(2-f)*sin(lat)**2)
c 
	end
c************************************************************************
c* Sph2lmn -- Convert from spherical coordinates to direction cosines.
c& rjs
c: utilities
c+
	subroutine sph2lmn(ra,dec,lmn)
c
	implicit none
	double precision ra,dec,lmn(3)
c
c  Convert spherical coordinates (e.g. ra,dec or long,lat) into 
c  direction cosines.
c
c  Input:
c    ra,dec	Angles in radians.
c  Output:
c    lmn	Direction cosines.
c--
c------------------------------------------------------------------------
	lmn(1) = cos(ra)*cos(dec)
	lmn(2) = sin(ra)*cos(dec)
	lmn(3) = sin(dec)
	end
c************************************************************************
c* lmn2sph -- Convert from direction cosines to spherical coordinates.
c& rjs
c: utilities
c+
	subroutine lmn2sph(lmn,ra,dec)
c
	implicit none
	double precision ra,dec,lmn(3)
c
c  Convert from direction cosines into spherical coordinates
c  (e.g. ra,dec or long,lat) into 
c
c  Input:
c    lmn	Direction cosines.
c  Output:
c    ra,dec	Angles in radians.
c--
c------------------------------------------------------------------------
	dec = asin(lmn(3))
	ra  = atan2(lmn(2),lmn(1))
	end
c************************************************************************
	double precision function epo2jul(epoch,code)
c
	implicit none
	double precision epoch
	character code*1
c
c  Convert an epoch (in years) to a Julian day.
c
c  Input:
c    epoch	The epoch.
c    code	Either 'B' (Besselian epoch) or 'J' (Julian) or ' '
c		If its blank, Julian epoch is assumed for values
c		greater than 1984.
c  Output:
c    epo2jul	The Julian day.
c------------------------------------------------------------------------
	logical julian
c
	if(code.eq.' ')then
	  julian = epoch.gt.1984
	else
	  julian = code.eq.'J'.or.code.eq.'j'
	  if(code.ne.'J'.and.code.ne.'j'.and.
     *	     code.ne.'B'.and.code.ne.'b')
     *	    call bug('f','Unrecognized epoch type, in epo2jul')
	endif
c
	if(julian)then
	  epo2jul = 365.25       *(epoch-2000) + 2451545d0
	else
	  epo2jul = 365.242198781*(epoch-1900) + 2415020.31352d0
	endif
	end
c************************************************************************
c* DelTime -- Difference between UTC and some other time systems.
c& rjs
c: utilities
c+
	double precision function deltime(jday,sys)
c
	double precision jday
	character sys*(*)
c
c  Determine the difference between UTC and some other time system.
c  For example
c
c	double precision TAI,UTC
c	TAI = UTC + deltime(UTC,'tai')
c
c  Input:
c    jday	UTC time when the difference is required.
c    sys	The time system. Possible values are:
c		'tai'	International atomic time.
c		'tdt'	Terrestial dynamical time. )
c		'tdb'	Barycentric dynamical time.) Taken as identical.
c		'et'	Ephemeris time.            )
c		'utc'	Coordinated universal time.
c  Output:
c    deltime	The time difference, T - UTC, in days.
c--
c------------------------------------------------------------------------
	integer i
c
	logical init
	integer NLEAP
	parameter(NLEAP=19)
	character leap(NLEAP)*7
	double precision dtime(NLEAP)
	save init,leap
	data init/.false./
c
c  The following table gives dates when a leap second was introduced
c  into UTC. KEEP THIS TABLE UP TO DATE.
c
	data leap /'72JUL01','73JAN01','74JAN01','75JAN01','76JAN01',
     *		   '77JAN01','78JAN01','79JAN01','80JAN01','81JUL01',
     *		   '82JUL01','83JUL01','85JUL01','88JAN01','90JAN01',
     *		   '91JAN01','92JUL01','93JUL01','94JUL01'/
c
c  Initialise the table of leap seconds.
c
	if(.not.init)then
	  do i=1,NLEAP
	    call dayjul(leap(i),dtime(i))
	  enddo
	  init = .true.
	endif
c
c  Determine the difference between TAI and UTC. For times before the first
c  leap second, assume it changes at 1 sec/year.
c
	deltime = 10
	if(jday.lt.dtime(1))then
	  deltime = deltime - nint((dtime(1)-jday)/365.25)
	else if(jday.ge.dtime(NLEAP))then
	  deltime = deltime + NLEAP
	else
	  i = 1
	  dowhile(jday.gt.dtime(i+1))
	    i = i + 1
	  enddo
	  deltime = deltime + i
	endif
c
c  Determine what time system the user really wanted.
c
	if(sys.eq.'tai')then
	  continue
	else if(sys.eq.'tdt'.or.sys.eq.'tdb'.or.sys.eq.'et')then
	  deltime = deltime + 32.184
	else if(sys.eq.'utc')then
	  deltime = 0
	else
	  call bug('f','Unrecognised time system, in DELTIME')
	endif
c
c  Deltime now contains the time offset in seconds. Convert this to
c  days.
c
	deltime = deltime / (24*3600)
	end
c************************************************************************
c* Aberrate -- Convert RA,DEC from true to geocentric apparent coords.
c& rjs
c: utilities
c+
	subroutine aberrate(jday,ra,dec,rapp,dapp)
c
	implicit none
	double precision jday,ra,dec,rapp,dapp
c
c  Account for the effect of annual aberration, to convert
c  from a true (RA,DEC) to a geocentric apparent (RA,DEC).
c
c  Input:
c    jday	Julian date.
c    ra,dec	True (RA,DEC).
c  Output:
c    rapp,dapp	Geocentric apparent (RA,DEC).
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision pos(3),vel(3),sinra,sindec,cosra,cosdec
c
	call vearth(jday,pos,vel)
c
	sinra = sin(ra)
	cosra = cos(ra)
	sindec = sin(dec)
	cosdec = cos(dec)
	rapp = ra +  (-vel(1)*sinra + vel(2)*cosra)/
     *				     (0.001*cmks*cosdec)
	dapp = dec + (-vel(1)*cosra*sindec - vel(2)*sinra*sindec 
     *		    + vel(3)*cosdec)/(0.001*cmks)
c
	end
c************************************************************************
c* Eqeq -- Equation of the equinox
c& rjs
c: utilities
c+
	double precision function eqeq(jday)
c
	implicit none
	double precision jday
c
c  Return the equation of the equinox.
c  To convert a mean sidereal time to an apparent sidereal time,
c  add the equation of the equinox.
c  e.g.
c    LAST = LMST + eqeq(jday)
c
c  Input:
c    jday	Julian date.
c  Output:
c    eqeq	The equation of the equinox, in radians.
c
c--
c------------------------------------------------------------------------
	double precision dpsi,deps
	double precision mobliq
c
c  Get nutation parameters.
c
	call nuts(jday,dpsi,deps)
c
c  Equation of the equinox.
c
	eqeq = dpsi * cos(mobliq(jday)+deps)
	end
c************************************************************************
c* Mobliqu -- Mean obliquity of the ecliptic
c& rjs
c: utilities
c+
	double precision function mobliq(jday)
c
	implicit none
	double precision jday
c
c  Return the mean obliquity of the ecliptic.
c
c  Input:
c    jday	Julian day.
c  Output:
c    mobliq	Mean obliquity of the ecliptic, in radians.
c
c  Reference:
c    Explanatory Supplement ... page 114.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision T
c
c  Centuries from J2000
c
	T = (jday - 2451545d0) / 36525d0
c
c  Mean obliquity.
c
	mobliq = 84381.448d0 - (46.8150+(0.00059-0.001813*T)*T)*T
	mobliq = dpi/(180d0*3600d0) * mobliq
	end
c************************************************************************
c* Nutate -- Convert from mean to true equatorial coordinates.
c& rjs
c: utilities
c+
	subroutine Nutate(jday,rmean,dmean,rtrue,dtrue)
c
	implicit none
	double precision jday,rmean,dmean,rtrue,dtrue
c
c  Convert between mean and true equatorial coordinates, by
c  accounting for nutation.
c
c  Input:
c    jday	Julian day.
c    rmean,dmean Mean (RA,DEC) at jday.
c  Output:
c    rtrue,dtrue True (RA,DEC) at jday.
c--
c------------------------------------------------------------------------
	double precision deps,dpsi,eps
	double precision coseps,sineps,sinra,cosra,tandec
c
c  Externals.
c
	double precision mobliq
c
c  Nutation parameters.
c
	call nuts(jday,dpsi,deps)
c
c  True obliquity.
	eps = mobliq(jday) + deps
c
c  Various parameters.
	sineps = sin(eps)
	coseps = cos(eps)
	sinra  = sin(rmean)
	cosra  = cos(rmean)
	tandec = tan(dmean)
c
	rtrue = rmean + (coseps + sineps*sinra*tandec)*dpsi 
     *		      - cosra*tandec*deps
	dtrue = dmean + sineps*cosra*dpsi + sinra*deps
	end
c************************************************************************
c* Nuts -- Return nutation parameters.
c& rjs
c: utilities
c+
	subroutine nuts(jday,dpsi,deps)
c
	implicit none
	double precision jday,dpsi,deps
c
c  Return nutation parameters. The claimed accuracy is 1 arcsec.
c
c  Input:
c    jday	Julian date.
c  Output:
c    dpsi,deps	Difference between mean and true ecliptic latitude and
c		longitude due to nutation, in radians.
c
c  Reference:
c    Explanatory Supplmenet, page 120.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision d,t1,t2
c
	d = jday - 2451545d0
	t1 = dpi/180*(125.0d0 - 0.05295d0 * d)
	t2 = dpi/180*(200.9d0 + 1.97129d0 * d)
	dpsi = dpi/180 * (-0.0048*sin(t1) - 0.0004*sin(t2))
	deps = dpi/180 * ( 0.0026*cos(t1) + 0.0002*cos(t2))
	end
c************************************************************************
c* Sunradec -- The Sun's apparent RA and DEC.
c& rjs
c: utilities
c+
	subroutine sunradec(jday,ra,dec)
c
	implicit none
	double precision jday,ra,dec
c
c  Determine the apparent RA and DEC of the Sun at a particular
c  time. Precision is 0.01 degrees for times between 1950 and 2050.
c
c  Reference:
c    Astronomical Ephemeris, 1994, page C24.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision jd2000
	parameter(jd2000=2451545.0)
	double precision n,L,g,lambda,epsi
c
c  Externals.
c
	double precision mobliq
c
c  Days from J2000.
	n = jday - jd2000
c
c  Mean longitude of Sun, corrected for aberration.
	L = dpi/180 * (280.466 + 0.9856474d0 * n)
c
c  Mean anomaly.
	g = dpi/180 * (357.528 + 0.9856003d0 * n)
c
c  Ecliptic longitude and latitude.
	lambda = L + dpi/180 * ( 1.915*sin(g) + 0.020*sin(2*g) )
	lambda = mod(lambda,2d0*dpi)
	if(lambda.lt.0) lambda = lambda + 2d0*dpi
c
c  Obliquity of the ecliptic.
	epsi = mobliq(jday)
c
c RA and DEC.
c
	ra = atan(cos(epsi)*tan(lambda))
	ra = ra + dpi * nint((lambda-ra)/dpi)
	ra = mod(ra,2*dpi)
	if(ra.lt.0)ra = ra + dpi
	dec = asin(sin(epsi)*sin(lambda))
 	end

