c***********************************************************************
	program pops
	implicit none
c
c= pops -- Convert position to another epoch.
c& rjs
c: utility
c+
c	POPS is a Miriad task which converts equatorial positions
c	from one epoch to another. Accuracy is about 1 arcsec.
c	If an observatory is also given, POPS
c	gives other source information, such as radial velocity, source
c	rise and set time and source elevation and azimuth. 
c@ radec
c	Right ascension and declination, given as hh:mm:ss,dd:mm:ss.
c	These coordinates are assumed to be mean coordinates of the given
c	epoch. The mean, true and apparent coordinates are printed on output.
c@ epochs
c	The input and output epochs. The epochs can be given as either
c	Bxxxx or Jxxxx (where xxxx is a year, e.g. B1950 or J2000), or as
c	yymmmdd:hh:mm:ss, a time in normal Miriad format 
c	(e.g. 93JAN05:10:25:00).
c@ observ
c	Observatory name. Default is the geocenter.
c--
c History:
c   rjs  23aug93  Original version.
c   rjs  20sep93  Add observatory and source rise and set times.
c   rjs  27oct93  Use keyt routines.
c   rjs  19nov93  Velocity calculation was incorrectly getting
c		  geocentric velocities. Better formatting.
c   rjs   9feb93  Give source elevation and azimuth as well.
c   rjs  12dec95  Extract lstjul to ephem.for
c Bugs:
c   * The precession, nutation and aberration are pretty simple. No
c     correction for the FK4 zero-point or elliptic terms of aberrations.
c-----------------------------------------------------------------------
	character version*(*)
	parameter(version='Pops: version 1.0 9-Feb-94')
c
	character string*64,observ*32
	double precision r0,d0,rm,dm,rt,dt,ra,da,jday1,jday2
	double precision lat,long
	logical ok
	integer lobs
	real vhel,vlsr
c
c  Externals.
c
	character hangle*16,rangle*16
	integer len1
c
	call output(version)
	call keyini
	call Keyt('radec',r0,'hms',0.d0)
	call Keyt('radec',d0,'dms',0.d0)
	call Keyt('epochs',jday1,'atime',0.d0)
	call Keyt('epochs',jday2,'atime',0.d0)
	call keya('observ',observ,'geocenter')
	call keyfin
c
c  Check the times.
c
	if(jday1.lt.1.or.jday2.lt.1)
     *	  call bug('f','Input epochs must be given')
c
c  Determine observatory lat and long.
c
	if(observ.ne.'geocenter')then
	  call obspar(observ,'latitude',lat,ok)
	  if(ok)call obspar(observ,'longitude',long,ok)
	  if(.not.ok)then
	    call bug('w','Could not determine observatory lat/long')
	    observ = 'geocenter'
	  endif
	else
	  lat = 0
	  long = 0
	endif
	lobs = len1(observ)
c
c  Precess, nutate and aberrate.
c
	call precess(jday1,r0,d0,jday2,rm,dm)
	call nutate(jday2,rm,dm,rt,dt)
	call aberrate(jday2,rt,dt,ra,da)
c
c  Determine the geocentric velocity (both LSR and barycentric).
c
	call VelRad(jday2,ra,da,rm,dm,vhel,vlsr,
     *			lat,long,observ.ne.'geocenter')
c
c  Report the results.
c
	call julday(jday1,'H',string)
	call output('Inputs Coordinates for time '//string)
	string = '  Mean:     '//hangle(r0)//' '//rangle(d0)
	call output(string)
	call output(' ')
	call julday(jday2,'H',string)
	call output('Output Coordinates for time '//string)
	string = '  Mean:     '//hangle(rm)//' '//rangle(dm)
	call output(string)
	string = '  True:     '//hangle(rt)//' '//rangle(dt)
	call output(string)
	string = '  Apparent: '//hangle(ra)//' '//rangle(da)
	call output(string)
	call output(' ')
c
	call output(' ')
	write(string,'(a,f8.3)')
     *	  'Barycentric Radial Velocity of '//observ(1:lobs)//' (km/s):',
     *	  vhel
	call output(string)
	write(string,'(a,f8.3)')
     *	  'LSR         Radial Velocity of '//observ(1:lobs)//' (km/s):',
     *	  vlsr
	call output(string)
c
c  Determine source rise and set times.
c
	if(observ.ne.'geocenter')then
	  call azel(jday2,lat,long,ra,da)
	  call riseset(jday2,lat,long,ra,da)
	endif
c 
	end
c************************************************************************
	subroutine azel(jday,lat,long,ra,da)
c
	implicit none
	double precision jday,lat,long,ra,da
c
c  Give the azimuth and elevation of the source.
c
c  Input:
c    jday	Julian day.
c    lat,long	Observatory geodetic latitude and longitude (radians).
c    ra,da	Apparent RA and DEC (radians).
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision lst,ha,az,el
	character line*80
c
c  Externals.
c
	character hangle*16
c
	call jullst(jday,long,lst)
	ha = lst - ra
	el = asin( sin(lat)*sin(da) + cos(lat)*cos(da)*cos(ha))
	az = atan2(-cos(da)*sin(ha),
     *		   cos(lat)*sin(da) - sin(lat)*cos(da)*cos(ha))
c
	call output(' ')
	line = 'Local Sidereal Time: '//hangle(lst)
	call output(line)
	write(line,'(a,f6.1,a)')
     *		'Source Azimuth:   ',180/pi*az,' deg'
	call output(line)
	write(line,'(a,f6.1,a)')
     *		'Source Elevation: ',180/pi*el,' deg'
	call output(line)
	end
c************************************************************************
	subroutine riseset(jday,lat,long,ra,dec)
c
	implicit none
	double precision jday,lat,long,ra,dec
c
c  Echo out the source rise and set times (assume 13 degrees minimum
c  elevation).
c
c  Input:
c    jday	The date of interest.
c    lat,long	Source latitude and longitude.
c    ra,dec	Source apparent RA and DEC.
c------------------------------------------------------------------------
	include 'mirconst.h'
	real sinel,sinl,cosl,sind,cosd,ha
	double precision rise,set,temp
	character string*32
c
	double precision LstJul
c
	sinl = sin(lat)
	cosl = cos(lat)
	sind = sin(dec)
	cosd = cos(dec)
	sinel = sin(12.*pi/180.)
c
	call output(' ')
	temp = (sinel - sinl*sind ) / (cosl*cosd)
	if(abs(temp).gt.1)then
	  if(dec*lat.lt.0)then
	    call bug('w','Source never rises to 12 deg elevation')
	  else
	    call output('Source is always above the horizon')
	  endif
	else
c
c  HA is the hour angle when the source rises/sets, while "rise" and "set"
c  are the LST times of rise and set.
c
	  ha = acos(temp)
	  rise = ra - ha
	  set  = ra + ha
c
c  Convert from LST to UT.
c
	  call output('For a minimum elevation of 12 degrees ...')
	  rise = LstJul(rise,jday,long)
	  call julday(rise,'H',string)
	  call output('Source rises at UT '//string)
	  rise = rise + ha/pi
c
	  set  = LstJul(set,rise,long)
	  call julday(set,'H',string)
	  call output('Source sets  at UT '//string)
	endif
c
	end
c************************************************************************
	subroutine VelRad(jday,ra,da,rm,dm,vhel,vlsr,lat,long,doobs)
c
	implicit none
	double precision jday,ra,da,rm,dm,lat,long
	logical doobs
	real vhel,vlsr
c
c  Compute the radial velocity in the direction of the source.
c
c  Input:
c    jday	Date of interest.
c    rm,dm	Mean coordinates.
c    ra,da	Apparent coordinates.
c    lat,long	Observatory latitude and longitude.
c    doobs	True if the observatory lat/long has been initialised.
c  Output:
c    vhel	Barycentric radial velocity component of the geocenter
c		in the direction of the source.
c    vlsr	LSR radial velocity component of the geocenter in the
c		direction of the source.
c------------------------------------------------------------------------
	integer i
	double precision posearth(3),velearth(3),velsun(3)
	double precision velsite(3),last
	double precision lmna(3),lmn2000(3),r2000,d2000
c
c  Externals.
c
	double precision epo2jul,eqeq
c
c  Compute observatory parameters -- LST and velocity.
c
	if(doobs)then
	  call jullst(jday,long,last)
	  last = last + eqeq(jday)
	  call vsite(lat,last,velsite)
	else
	  velsite(1) = 0
	  velsite(2) = 0
	  velsite(3) = 0
	endif
c
	call sph2lmn(ra,da,lmna)
	call vearth(jday,posearth,velearth)
c
	vhel = 0
	do i=1,3
	  vhel = vhel - (velsite(i) + velearth(i))*lmna(i)
	enddo
c
	call precess(jday,rm,dm,epo2jul(2000.d0,'J'),r2000,d2000)
	call sph2lmn(r2000,d2000,lmn2000)
	call vsun(velsun)
	vlsr = vhel
	do i=1,3
	  vlsr = vlsr + lmn2000(i)*velsun(i)
	enddo
c
	end
