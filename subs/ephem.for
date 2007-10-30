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
c    12dec95 rjs  Added veccross,lstjul.
c    11mar96 rjs  Update leap second table.
c    03oct96 rjs  Corrected leap second table error.
c    01mar97 rjs  Added new leap second.
c    21mar97 rjs  Added llh2xyz.
c    07jul97 rjs  Included fk45z and fk54z (from slalib), as well as
c		  making lmn2sph return RA in range 0 to 2*PI.
c    16jul97 rjs  Added azel.
c    15jan99 rjs  Added new leap second.
c    13sep99 rjs  Make jullst more robust.
c    12may04 rjs  Make jullst more robust - again.
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
c* azel -- Calculate azimuth and elevation from ra/dec.
c& rjs
c: utilities
c+
	subroutine azel(obsra,obsdec,lst,latitude,az,el)
c
	implicit none
	double precision obsra,obsdec,lst,latitude
	double precision az,el
c
c  This computes the azimuth and elevation.
c
c  Input:
c    obsra )	Apparent RA and DEC of the source of interest (radians).
c    obsdec)
c    lst	Local sidereal time (radians).
c    latitude	Observatory geodetic latitude (radians).
c
c  Output:
c    az,el	Azimuth and elevation angle (radians).
c--
c------------------------------------------------------------------------
	double precision ha
c
	ha = lst - obsra
        el = asin(sin(latitude)*sin(obsdec) +
     *	    cos(latitude)*cos(obsdec)*cos(ha))
        az = atan2(-cos(obsdec)*sin(ha),
     *      cos(latitude)*sin(obsdec)-sin(latitude)*cos(obsdec)*cos(ha))
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
c* llh2xyz -- Convert from latitude/longitude/height to CIO (x,y,z).
c& rjs
c: utilities
c+
	subroutine llh2xyz(lat,long,height,x,y,z)
c
	implicit none
	double precision x,y,z,lat,long,height
c
c  Convert betweena location defined in terms
c  geodetic latitude, longitude, and height above the reference geoid to
c  CIO coordinates.
c
c  Reference:
c    Kenneth R. Lang, "Astrophysical Formulae", pages 493-497.
c    Values for flattening and equatorial radius from John Reynolds,
c    who says they are the IAU 1976 values.
c
c  Input:
c   lat,long	Geodetic latitude and longitude, in radians.
c   height	Height above the reference geoid (i.e. sea level), in meters.
c  Output:
c   x,y,z	CIO coordinates, in meters.
c--
c------------------------------------------------------------------------
c f  -- Earth's flattening factor
c ae -- Earth's equatorial radius, in meters.
c
	double precision f,ae,fm12
	parameter(f=1/298.257,ae=6.378140d6,fm12=(1-f*(2-f)))
c
	double precision Nphi
c
	Nphi = ae/sqrt(1-f*(2-f)*sin(lat)**2)
	x = (Nphi+height)*cos(long)*cos(lat)
	y = (Nphi+height)*sin(long)*cos(lat)
	z = (fm12*Nphi+height)*sin(lat)
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
	include 'mirconst.h'
c
	dec = asin(lmn(3)/sqrt(lmn(1)**2+lmn(2)**2+lmn(3)**2))
	ra  = atan2(lmn(2),lmn(1))
	if(ra.lt.0)ra = ra + 2*DPI
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
	double precision function jul2epo(jday,code)
c
	implicit none
	double precision jday
	character code*1
c
c  Convert a Julian day into an epoch (in years).
c
c  Input:
c    jday	The epoch.
c    code	Either 'B' (Besselian epoch) or 'J' (Julian) or ' '
c		If its blank, Julian epoch is assumed for values
c		greater than 1984.
c  Output:
c    jul2epo	The Julian day.
c------------------------------------------------------------------------
	logical julian
c
	if(code.eq.' ')then
	  julian = jday.gt.2445701d0
	else
	  julian = code.eq.'J'.or.code.eq.'j'
	  if(code.ne.'J'.and.code.ne.'j'.and.
     *	     code.ne.'B'.and.code.ne.'b')
     *	    call bug('f','Unrecognized epoch type, in jul2epo')
	endif
c
	if(julian)then
	  jul2epo = (jday-2451545d0)/365.25 + 2000
	else
	  jul2epo = (jday-2415020.31352d0)/365.242198781 + 1900
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
c		'tt'    Terrestial time		   )
c		'utc'	Coordinated universal time.
c  Output:
c    deltime	The time difference, T - UTC, in days.
c--
c------------------------------------------------------------------------
	integer i
c
	logical init
	integer NLEAP
	parameter(NLEAP=22)
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
     *		   '91JAN01','92JUL01','93JUL01','94JUL01','96JAN01',
     *		   '97JUL01','99JAN01'/
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
	else if(sys.eq.'tdt'.or.sys.eq.'tdb'.or.sys.eq.'et'.or.
     *	  sys.eq.'tt')then
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
c************************************************************************
	subroutine veccross(x,y,z)
c
	double precision x(3),y(3),z(3)
c
	z(1) = x(2)*y(3) - x(3)*y(2)
	z(2) = x(3)*y(1) - x(1)*y(3)
	z(3) = x(1)*y(2) - x(2)*y(1)
	end
c************************************************************************
	double precision function LstJul(lst,jday,long)
c
	implicit none
	double precision lst,jday,long
c
c  Convert an LST to a Julian day.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision myday,mylst,delta,lst1
	logical more
c
	double precision eqeq
c
	lst1 = mod(lst,2*DPI)
	myday = jday
	more = .true.
	dowhile(more)
	  call jullst(myday,long,mylst)
	  mylst = mylst + eqeq(myday)
	  delta = - (mylst - lst1) / (2*pi)
	  if(delta.gt.0.5)delta = delta - 1
	  if(delta.lt.-0.5) delta = delta + 1
	  myday = myday + delta * 365.25d0/366.25d0
	  more = abs(delta).gt.1/(24.*3600.)
	enddo
	LstJul = myday
	end
************************************************************************
      SUBROUTINE FK45Z (R1950,D1950,JDAY,R2000,D2000)
*+
*     - - - - - -
*      F K 4 5 Z
*     - - - - - -
*
*  Convert B1950.0 FK4 star data to J2000.0 FK5 assuming zero
*  proper motion in an inertial frame (double precision)
*
*  This routine converts stars from the old, Bessel-Newcomb, FK4
*  system to the new, IAU 1976, FK5, Fricke system, in such a
*  way that the FK5 proper motion is zero.  Because such a star
*  has, in general, a non-zero proper motion in the FK4 system,
*  the routine requires the epoch at which the position in the
*  FK4 system was determined.
*
*  The method is from Appendix 2 of Ref 1, but using the constants
*  of Ref 4.
*
*  Given:
*     R1950,D1950     dp    B1950.0 FK4 RA,Dec at epoch (rad)
*     JDAY            dp    Julian date of epoch
*
*  Returned:
*     R2000,D2000     dp    J2000.0 FK5 RA,Dec (rad)
*
*  Notes:
*
*  2)  Conversion from Besselian epoch 1950.0 to Julian epoch
*      2000.0 only is provided for.  Conversions involving other
*      epochs will require use of the appropriate precession,
*      proper motion, and E-terms routines before and/or
*      after FK425 is called.
*
*  3)  In the FK4 catalogue the proper motions of stars within
*      10 degrees of the poles do not embody the differential
*      E-term effect and should, strictly speaking, be handled
*      in a different manner from stars outside these regions.
*      However, given the general lack of homogeneity of the star
*      data available for routine astrometry, the difficulties of
*      handling positions that may have been determined from
*      astrometric fields spanning the polar and non-polar regions,
*      the likelihood that the differential E-terms effect was not
*      taken into account when allowing for proper motion in past
*      astrometry, and the undesirability of a discontinuity in
*      the algorithm, the decision has been made in this routine to
*      include the effect of differential E-terms on the proper
*      motions for all stars, whether polar or not.  At epoch 2000,
*      and measuring on the sky rather than in terms of dRA, the
*      errors resulting from this simplification are less than
*      1 milliarcsecond in position and 1 milliarcsecond per
*      century in proper motion.
*
*  References:
*
*     1  Aoki,S., et al, 1983.  Astron.Astrophys., 128, 263.
*
*     2  Smith, C.A. et al, 1989.  "The transformation of astrometric
*        catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
*
*     3  Yallop, B.D. et al, 1989.  "Transformation of mean star places
*        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
*        Astron.J. 97, 274.
*
*     4  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
*        the Astronomical Almanac", ISBN 0-935702-68-7.
*
*
*  Original:                      P.T.Wallace  24 December 1992
*  Modified to use within Miriad: Bob Sault    30 April 1997
*-

      IMPLICIT NONE

      DOUBLE PRECISION R1950,D1950,JDAY,R2000,D2000

      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)

      DOUBLE PRECISION W
      INTEGER I,J

*  Position and position+velocity vectors
      DOUBLE PRECISION R0(3),A1(3),V1(3),V2(6)

*  Radians per year to arcsec per century
      DOUBLE PRECISION PMF
      PARAMETER (PMF=100D0*60D0*60D0*360D0/D2PI)

*
*  JULIAN DATE of B1950 and J2000.
      DOUBLE PRECISION B1950,J2000
      PARAMETER(B1950=2433282.423D0,J2000=2451545.000D0)

*
*  CANONICAL CONSTANTS  (see references)
*

*  Vectors A and Adot, and matrix M (only half of which is needed here)
      DOUBLE PRECISION A(3),AD(3),EM(6,3)
      DATA A,AD/ -1.62557D-6,  -0.31919D-6, -0.13843D-6,
     :           +1.245D-3,    -1.580D-3,   -0.659D-3/

      DATA (EM(I,1),I=1,6) / +0.9999256782D0,
     :                       +0.0111820610D0,
     :                       +0.0048579479D0,
     :                       -0.000551D0,
     :                       +0.238514D0,
     :                       -0.435623D0 /

      DATA (EM(I,2),I=1,6) / -0.0111820611D0,
     :                       +0.9999374784D0,
     :                       -0.0000271474D0,
     :                       -0.238565D0,
     :                       -0.002667D0,
     :                       +0.012254D0 /

      DATA (EM(I,3),I=1,6) / -0.0048579477D0,
     :                       -0.0000271765D0,
     :                       +0.9999881997D0,
     :                       +0.435739D0,
     :                       -0.008541D0,
     :                       +0.002117D0 /



*  Spherical to Cartesian
      CALL SPH2LMN(R1950,D1950,R0)

*  Adjust vector A to give zero proper motion in FK5
      W=(JDAY-B1950)/365.242198781d0/PMF
      DO I=1,3
         A1(I)=A(I)+W*AD(I)
      END DO

*  Remove e-terms
      W=R0(1)*A1(1)+R0(2)*A1(2)+R0(3)*A1(3)
      DO I=1,3
         V1(I)=R0(I)-A1(I)+W*R0(I)
      END DO

*  Convert position vector to Fricke system
      DO I=1,6
         W=0D0
         DO J=1,3
            W=W+EM(I,J)*V1(J)
         END DO
         V2(I)=W
      END DO

*  Allow for fictitious proper motion in FK4
      W=(JDAY-J2000)/365.25d0/PMF
      DO I=1,3
         V2(I)=V2(I)+W*V2(I+3)
      END DO

*  Revert to spherical coordinates
      CALL LMN2SPH(V2,R2000,D2000)

      END
************************************************************************
      SUBROUTINE FK54Z (R2000,D2000,JDAY,
     :                      R1950,D1950,DR1950,DD1950)
*+
*     - - - - - -
*      F K 5 4 Z
*     - - - - - -
*
*  Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming
*  zero proper motion and parallax (double precision)
*
*  This routine converts star positions from the new, IAU 1976,
*  FK5, Fricke system to the old, Bessel-Newcomb, FK4 system.
*
*  Given:
*     R2000,D2000     dp    J2000.0 FK5 RA,Dec (rad)
*     JDAY            dp    Julian date of epoch.
*
*  Returned:
*     R1950,D1950     dp    B1950.0 FK4 RA,Dec (rad) at epoch JDAY
*     DR1950,DD1950   dp    B1950.0 FK4 proper motions (rad/trop.yr)
*
*  Notes:
*
*  1)  The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
*
*  2)  Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0
*      only is provided for.  Conversions involving other epochs will
*      require use of the appropriate precession routines before and
*      after this routine is called.
*
*  3)  Unlike in the FK524 routine, the FK5 proper motions, the
*      parallax and the radial velocity are presumed zero.
*
*  4)  It is the intention that FK5 should be a close approximation
*      to an inertial frame, so that distant objects have zero proper
*      motion;  such objects have (in general) non-zero proper motion
*      in FK4, and this routine returns those fictitious proper
*      motions.
*
*  5)  The position returned by this routine is in the B1950
*      reference frame but at epoch JDAY.  For
*      comparison with catalogues the JDAY argument will
*      frequently correspond to B1950.
*
*  Called:  FK524, PM
*
*  Author:   P.T.Wallace   Starlink   10 April 1990
*  Modified: Bob Sault		       7 July  1997
*  
*-

      IMPLICIT NONE

      DOUBLE PRECISION R2000,D2000,JDAY,
     :                 R1950,D1950,DR1950,DD1950

      DOUBLE PRECISION R,D,PX,RV

      DOUBLE PRECISION B1950
      PARAMETER(B1950=2433282.423D0)


*  FK5 equinox J2000 (any epoch) to FK4 equinox B1950 epoch B1950
      CALL FK524(R2000,D2000,0D0,0D0,0D0,0D0,
     :               R,D,DR1950,DD1950,PX,RV)

*  Fictitious proper motion to epoch BEPOCH
      CALL PM(R,D,DR1950,DD1950,0D0,0D0,B1950,JDAY,
     :            R1950,D1950)

      END
************************************************************************
      SUBROUTINE FK524 (R2000,D2000,DR2000,DD2000,P2000,V2000,
     :                      R1950,D1950,DR1950,DD1950,P1950,V1950)
*+
*     - - - - - -
*      F K 5 2 4
*     - - - - - -
*
*  Convert J2000.0 FK5 star data to B1950.0 FK4 (double precision)
*
*  This routine converts stars from the new, IAU 1976, FK5, Fricke
*  system, to the old, Bessel-Newcomb, FK4 system.  The precepts
*  of Smith et al (Ref 1) are followed, using the implementation
*  by Yallop et al (Ref 2) of a matrix method due to Standish.
*  Kinoshita's development of Andoyer's post-Newcomb precession is
*  used.  The numerical constants from Seidelmann et al (Ref 3) are
*  used canonically.
*
*  Given:  (all J2000.0,FK5)
*     R2000,D2000     dp    J2000.0 RA,Dec (rad)
*     DR2000,DD2000   dp    J2000.0 proper motions (rad/Jul.yr)
*     P2000           dp    parallax (arcsec)
*     V2000           dp    radial velocity (km/s, +ve = moving away)
*
*  Returned:  (all B1950.0,FK4)
*     R1950,D1950     dp    B1950.0 RA,Dec (rad)
*     DR1950,DD1950   dp    B1950.0 proper motions (rad/trop.yr)
*     P1950           dp    parallax (arcsec)
*     V1950           dp    radial velocity (km/s, +ve = moving away)
*
*  Notes:
*
*  1)  The proper motions in RA are dRA/dt rather than
*      cos(Dec)*dRA/dt, and are per year rather than per century.
*
*  2)  Note that conversion from Julian epoch 2000.0 to Besselian
*      epoch 1950.0 only is provided for.  Conversions involving
*      other epochs will require use of the appropriate precession,
*      proper motion, and E-terms routines before and/or after
*      FK524 is called.
*
*  3)  In the FK4 catalogue the proper motions of stars within
*      10 degrees of the poles do not embody the differential
*      E-term effect and should, strictly speaking, be handled
*      in a different manner from stars outside these regions.
*      However, given the general lack of homogeneity of the star
*      data available for routine astrometry, the difficulties of
*      handling positions that may have been determined from
*      astrometric fields spanning the polar and non-polar regions,
*      the likelihood that the differential E-terms effect was not
*      taken into account when allowing for proper motion in past
*      astrometry, and the undesirability of a discontinuity in
*      the algorithm, the decision has been made in this routine to
*      include the effect of differential E-terms on the proper
*      motions for all stars, whether polar or not.  At epoch 2000,
*      and measuring on the sky rather than in terms of dRA, the
*      errors resulting from this simplification are less than
*      1 milliarcsecond in position and 1 milliarcsecond per
*      century in proper motion.
*
*  References:
*
*     1  Smith, C.A. et al, 1989.  "The transformation of astrometric
*        catalog systems to the equinox J2000.0".  Astron.J. 97, 265.
*
*     2  Yallop, B.D. et al, 1989.  "Transformation of mean star places
*        from FK4 B1950.0 to FK5 J2000.0 using matrices in 6-space".
*        Astron.J. 97, 274.
*
*     3  Seidelmann, P.K. (ed), 1992.  "Explanatory Supplement to
*        the Astronomical Almanac", ISBN 0-935702-68-7.
*
*  P.T.Wallace   Starlink   19 December 1993
*-

      IMPLICIT NONE

      DOUBLE PRECISION R2000,D2000,DR2000,DD2000,P2000,V2000,
     :                 R1950,D1950,DR1950,DD1950,P1950,V1950


*  Miscellaneous
      DOUBLE PRECISION R,D,UR,UD,PX,RV
      DOUBLE PRECISION SR,CR,SD,CD,X,Y,Z,W
      DOUBLE PRECISION V1(6),V2(6)
      DOUBLE PRECISION XD,YD,ZD
      DOUBLE PRECISION RXYZ,WD,RXYSQ,RXY
      INTEGER I,J

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925287D0)

*  Radians per year to arcsec per century
      DOUBLE PRECISION PMF
      PARAMETER (PMF=100D0*60D0*60D0*360D0/D2PI)

*  Small number to avoid arithmetic problems
      DOUBLE PRECISION TINY
      PARAMETER (TINY=1D-30)

*
*  CANONICAL CONSTANTS  (see references)
*

*  Km per sec to AU per tropical century
*  = 86400 * 36524.2198782 / 149597870
      DOUBLE PRECISION VF
      PARAMETER (VF=21.095D0)

*  Constant vector and matrix (by columns)
      DOUBLE PRECISION A(6),EMI(6,6)
      DATA A/ -1.62557D-6,  -0.31919D-6, -0.13843D-6,
     :        +1.245D-3,    -1.580D-3,   -0.659D-3/

      DATA (EMI(I,1),I=1,6) / +0.9999256795D0,
     :                        -0.0111814828D0,
     :                        -0.0048590040D0,
     :                        -0.000551D0,
     :                        -0.238560D0,
     :                        +0.435730D0 /

      DATA (EMI(I,2),I=1,6) / +0.0111814828D0,
     :                        +0.9999374849D0,
     :                        -0.0000271557D0,
     :                        +0.238509D0,
     :                        -0.002667D0,
     :                        -0.008541D0 /

      DATA (EMI(I,3),I=1,6) / +0.0048590039D0,
     :                        -0.0000271771D0,
     :                        +0.9999881946D0,
     :                        -0.435614D0,
     :                        +0.012254D0,
     :                        +0.002117D0 /

      DATA (EMI(I,4),I=1,6) / -0.00000242389840D0,
     :                        +0.00000002710544D0,
     :                        +0.00000001177742D0,
     :                        +0.99990432D0,
     :                        -0.01118145D0,
     :                        -0.00485852D0 /

      DATA (EMI(I,5),I=1,6) / -0.00000002710544D0,
     :                        -0.00000242392702D0,
     :                        +0.00000000006585D0,
     :                        +0.01118145D0,
     :                        +0.99991613D0,
     :                        -0.00002716D0 /

      DATA (EMI(I,6),I=1,6) / -0.00000001177742D0,
     :                        +0.00000000006585D0,
     :                        -0.00000242404995D0,
     :                        +0.00485852D0,
     :                        -0.00002717D0,
     :                        +0.99996684D0 /



*  Pick up J2000 data (units radians and arcsec/JC)
      R=R2000
      D=D2000
      UR=DR2000*PMF
      UD=DD2000*PMF
      PX=P2000
      RV=V2000

*  Spherical to Cartesian
      SR=SIN(R)
      CR=COS(R)
      SD=SIN(D)
      CD=COS(D)

      X=CR*CD
      Y=SR*CD
      Z=   SD

      W=VF*RV*PX

      V1(1)=X
      V1(2)=Y
      V1(3)=Z

      V1(4)=-UR*Y-CR*SD*UD+W*X
      V1(5)= UR*X-SR*SD*UD+W*Y
      V1(6)=         CD*UD+W*Z

*  Convert position+velocity vector to BN system
      DO I=1,6
         W=0D0
         DO J=1,6
            W=W+EMI(I,J)*V1(J)
         END DO
         V2(I)=W
      END DO

*  Position vector components and magnitude
      X=V2(1)
      Y=V2(2)
      Z=V2(3)
      RXYZ=SQRT(X*X+Y*Y+Z*Z)

*  Apply E-terms to position
      W=X*A(1)+Y*A(2)+Z*A(3)
      X=X+A(1)*RXYZ-W*X
      Y=Y+A(2)*RXYZ-W*Y
      Z=Z+A(3)*RXYZ-W*Z

*  Recompute magnitude
      RXYZ=SQRT(X*X+Y*Y+Z*Z)

*  Apply E-terms to both position and velocity
      X=V2(1)
      Y=V2(2)
      Z=V2(3)
      W=X*A(1)+Y*A(2)+Z*A(3)
      WD=X*A(4)+Y*A(5)+Z*A(6)
      X=X+A(1)*RXYZ-W*X
      Y=Y+A(2)*RXYZ-W*Y
      Z=Z+A(3)*RXYZ-W*Z
      XD=V2(4)+A(4)*RXYZ-WD*X
      YD=V2(5)+A(5)*RXYZ-WD*Y
      ZD=V2(6)+A(6)*RXYZ-WD*Z

*  Convert to spherical
      RXYSQ=X*X+Y*Y
      RXY=SQRT(RXYSQ)

      IF (X.EQ.0D0.AND.Y.EQ.0D0) THEN
         R=0D0
      ELSE
         R=ATAN2(Y,X)
         IF (R.LT.0.0D0) R=R+D2PI
      END IF
      D=ATAN2(Z,RXY)

      IF (RXY.GT.TINY) THEN
         UR=(X*YD-Y*XD)/RXYSQ
         UD=(ZD*RXYSQ-Z*(X*XD+Y*YD))/((RXYSQ+Z*Z)*RXY)
      END IF

*  Radial velocity and parallax
      IF (PX.GT.TINY) THEN
         RV=(X*XD+Y*YD+Z*ZD)/(PX*VF*RXYZ)
         PX=PX/RXYZ
      END IF

*  Return results
      R1950=R
      D1950=D
      DR1950=UR/PMF
      DD1950=UD/PMF
      P1950=PX
      V1950=RV

      END
************************************************************************
      SUBROUTINE PM (R0, D0, PR, PD, PX, RV, JEP0, JEP1, R1, D1)
*+
*     - - -
*      P M
*     - - -
*
*  Apply corrections for proper motion to a star RA,Dec
*  (double precision)
*
*  References:
*     1984 Astronomical Almanac, pp B39-B41.
*     (also Lederle & Schwan, Astron. Astrophys. 134,
*      1-6, 1984)
*
*  Given:
*     R0,D0    dp     RA,Dec at epoch JEP0 (rad)
*     PR,PD    dp     proper motions:  RA,Dec changes per year of epoch
*     PX       dp     parallax (arcsec)
*     RV       dp     radial velocity (km/sec, +ve if receding)
*     JEP0     dp     start epoch in years (e.g. Julian epoch)
*     JEP1     dp     end epoch in years (same system as EP0)
*
*  Returned:
*     R1,D1    dp     RA,Dec at epoch EP1 (rad)
*
*  Called:
*     sph2lmn       spherical to Cartesian
*     lmn2sph       Cartesian to spherical
*
*  Note:
*     The proper motions in RA are dRA/dt rather than
*     cos(Dec)*dRA/dt, and are in the same coordinate
*     system as R0,D0.
*
*  P.T.Wallace   Starlink   12 April 1990
*-

      IMPLICIT NONE

      DOUBLE PRECISION R0,D0,PR,PD,PX,RV,JEP0,JEP1,R1,D1

*  Km/s to AU/year multiplied by arc seconds to radians
      DOUBLE PRECISION VFR
      PARAMETER (VFR=0.21094502D0*0.4848136811095359949D-05)

      INTEGER I
      DOUBLE PRECISION W,EM(3),T,P(3)



*  Spherical to Cartesian
      CALL SPH2LMN(R0,D0,P)

*  Space motion (radians per year)
      W=VFR*RV*PX
      EM(1)=-PR*P(2)-PD*COS(R0)*SIN(D0)+W*P(1)
      EM(2)= PR*P(1)-PD*SIN(R0)*SIN(D0)+W*P(2)
      EM(3)=         PD*COS(D0)        +W*P(3)

*  Apply the motion
      T=(JEP1-JEP0)/365.242198781d0
      DO I=1,3
         P(I)=P(I)+T*EM(I)
      END DO

*  Cartesian to spherical
      CALL LMN2SPH(P,R1,D1)

      END
