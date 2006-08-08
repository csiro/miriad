c************************************************************************
c
c  A set of routines giving information about observatories.
c
c  History:
c    rjs  20jun91 Original version.
c    rjs   2jul91 Corrected Westerbork parameters, as suggested by pjt.
c    rjs   2jun93 Near complete rewrite, to give it greater flexibility.
c	          Add also more parameters.
c    mchw 15jul93 Add some more parameters.
c    rjs  16sep93 Rename bsrch to binsrch.
c    rjs   7mar94 Assume WSRT evector is -90 degrees (assume fixed dipoles).
c    rjs  29jul94 Tolerate double-barrelled names (e.g. 'OVRO MMA').
c    rjs   9aug94 Add "ew" parameter for ATCA and WSRT.
c    rjs   5jul95 Added some penticon parameters.
c    rjs  27sep95 Added Parkes and nants.
c    rjs  11oct95 Added subreflector diameter for atca.
c    rjs  11mar96 Changed "nobeyama" to "nro10m", and added more info
c		  on it.
c    rjs  14mar96 Fixed bug I introduced on 11mar96.
c    rjs  11aug96 Added mopra, plus miscellaneous other parameters.
c    mchw 18sep96 Added iram15m array on PdB.
c    rjs  17dec96 More accurate positions for ATNF telescopes.
c    mchw 20may97 Added CSO and JCMT. Updated HATCREEK.
c    mchw 09jun97 Added evector to hatcreek
c    rjs  24jun97 Correct height of ATCA.
c    rjs/jm 11jul97 Correct longitude sign for IRAM15M, JCMT and CSO.
c    mchw 05aug98 Added mount and nants to ovro.
c    mchw 05feb99 Added CARMA for combined hatcreek and ovro arrays.
c    rjs  31mar00 Added SEST.
c    dpr  22may01 Added HOBART26M, CEDUNA30M, XYEW
c    mchw 24may01 Added RPA
c    mchw 03jan02 Added SZA
c    mchw 09jul02 Added ALMA
c    mchw 26aug03 Added SMA
c    gxm  27jan04 Added generic systemp for WSRT (to please wsrtfits).
c    mchw 07jul04 Added SMA10 and SZA6 cross correlations for CARMA.
c    pjt  24sep04 Final location of CARMA at Cedar Flats
c************************************************************************
c* ObsPrint -- Print list of known observatories.
c: utility
c& rjs
c+
	subroutine obsPrint
c
	implicit none
c
c  This prints a list of the known observatories.
c--
c------------------------------------------------------------------------
	include 'obspar.h'
	character tel*32
	integer l,i
c
	call obsInit
c
	tel = ' '
	call output('Known observatories are:')
	do i=1,nparms
	  l = index(parname(i),'/')
	  if(parname(i)(1:l-1).ne.tel)then
	    tel = parname(i)(1:l-1)
	    call output('   '//tel)
	  endif
	enddo
c
	end
c************************************************************************
c* ObsPar - Get characteristics of a particular observatory.
c: utility
c& rjs
c+
	subroutine obsPar(observ,object,value,ok)
c
	implicit none
	character observ*(*),object*(*)
	double precision value
	logical ok
c
c  This returns some known characteristics of various observervatories.
c
c  Input:
c    observ	Name of the observatory. Current list is :
c                 'ALMA', 'ATCA', 'CARMA', CEDUNA30M', 'CSO', 'GMRT',
c                 'HATCREEK', 'HOBART26M', 'IRAM15M', 'JCMT',
c                 'KITTPEAK', 'NOBEYAMA', 'NOBEYAMA45', 'ONSALA', 'OVRO',
c		  'PARKES', 'PENTICTON', 'QUABBIN', 'RPA', 'SZA', 'SZA10', 'SZA6', 'VLA', 
c		  'WSRT'
c                 
c    object	The parameter of the observatory of interest. Possible
c		values are:
c		 'latitude '	Observatory latitude, in radians.
c		 'longitude'	Observatory longitude, in radians.
c		 'jyperk'	Typical system gain, in Jy/K.
c		 'systemp'	Typical system temperature, in K.
c		 'evector'	Offset angle of the feed to the local
c				vertical.
c		 'mount'	Telescope mount: 0 = alt-az
c						 1   equitorial
c                                                3   xy-ew
c                                                4   nasmyth
c		 'antdiam'	Antenna diameter, in meters.
c		 'subdiam'	Subreflector diameter.
c		 'height'	Height above sea level, in meters
c		 'ew'		Positive if the telescope is an E-W array.
c	         'nants'        Number of antennas normally in the array.
c		 'ellimit'	Elevation limit.
c  Output:
c    value	The value of the parameter.
c    ok		True if the value was successfully found.
c--
c------------------------------------------------------------------------
	include 'obspar.h'
	character name*24
	integer l
c
c  Externals.
c
	integer len1,binsrcha
c
c
c  Initialise the list of known parameters, if necessary.
c
	call obsInit
c
c  Determine the name of the parameter that we want, and locate it.
c
	l = index(observ,' ') - 1
	if(l.le.0)l = len1(observ)
	name = observ(1:l)//'/'//object
	call lcase(name)
	l = binsrcha(name,parname,nparms)
	ok = l.gt.0
	if(ok) value = parvalue(l)
	end
c************************************************************************
	subroutine obsinit
c
	implicit none
c
c  Initialise the list of known parameters.
c
c  NOTE: The following list MUST be in alphabetic order, and MUST be
c  lower case!!! Note that the '/' character is before all alphabetic]
c  characters in the ASCII sequence.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'obspar.h'
c
	double precision ALTAZ,EQUATOR,NASMYTH,XYEW
	parameter(ALTAZ=0.d0,EQUATOR=1.d0,NASMYTH=4.d0,XYEW=3.d0)
c
c  Externals.
c
	double precision obsdms
c
	logical first
	save first
	data first/.true./
c
	if(.not.first)return
	first = .false.
	nparms = 0
c
c  ALMA - will need to be updated. From Simon Radford:
c At the Grenoble meeting, Chajnantor South was chosen as the nominal 
c array center. In 1999 November, we measured this with a differential GPS:
c 
c                             N            E          alt
c Chajnantor South        7453228.830  627807.166  5016.792
c 
c where the coordinates are UTM [m] on the Prov. S. Am. 1956 datum, which 
c we use for mapping and civil construction. This position is 544 m south 
c and 35 m east of the NRAO container. By long integration with a 
c navigation GPS, I measured the position of the NRAO container (Memos 
c 261, 312):
c 
c NRAO  container         7453772      627772      5060
c 
c on the same datum. Again using this datum, the latitude and longitude of 
c the NRAO container are:
c 
c    23deg 1'  9.42''  South    67deg 45' 11.44''  West
c 
c Note these coordinates are based on the PSAm56 datum and are not the 
c "astronomical" latitude and longitude. The native datum for GPS, the 
c WGS84 datum, is very close to the astronomical coordinate system 
c determined from VLBI. In this datum, the NRAO container is at:
c 
c    23deg 1' 22.42''  South    67deg 45' 17.74''  West
c
	call obsad('alma/antdiam',	12.d0)
	call obsad('alma/ellimit',	12.0*dpi/180.d0)
	call obsad('alma/evector',	0.25*dpi)
	call obsad('alma/height',	5017.d0)
	call obsad('alma/jyperk',	40.7d0)
	call obsad('alma/latitude',	obsdms(-1, 23,01,22.42))
	call obsad('alma/longitude',	obsdms( 1,67,47, 17.74))
	call obsad('alma/mount',	ALTAZ)
	call obsad('alma/nants',	64.d0)
	call obsad('alma/systemp',	40.d0)
c
c
c  The Australia Telescope Compact Array (ATNF).
c  Latitude, longitude and height refer to station 35. Info from
c  John Reynolds. Geodetic coordinates on WGS84
c
	call obsad('atca/antdiam',	22.d0)
	call obsad('atca/ellimit',	12.0*dpi/180.d0)
	call obsad('atca/evector',	0.25*dpi)
	call obsad('atca/ew',		1.d0)
	call obsad('atca/height',	236.87d0)
	call obsad('atca/jyperk',	13.d0)
	call obsad('atca/latitude',	obsdms(-1, 30,18,46.3849))
	call obsad('atca/longitude',	obsdms( 1,149,33, 0.4997))
	call obsad('atca/mount',	ALTAZ)
	call obsad('atca/nants',	6.d0)
	call obsad('atca/subdiam',	2.8d0)
	call obsad('atca/systemp',	50.d0)
c
c  CARMA (Combined California Array  - geometric mean of 10.4 and 6.1m)
c
	call obsad('carma/antdiam',	8.0d0)
	call obsad('carma/ellimit',	5.0*dpi/180.d0)
	call obsad('carma/evector',	0.5*dpi)
	call obsad('carma/height',	2200.0d0)
	call obsad('carma/jyperk',	80.d0)
        call obsad('carma/latitude',     obsdms( 1, 37,16, 43.00))
        call obsad('carma/longitude',    obsdms(-1,118, 8, 32.00))
	call obsad('carma/mount',	ALTAZ)
	call obsad('carma/nants',	15.d0)
	call obsad('carma/systemp',	200.d0)
c
c  The Ceduna 30m dish.
c  Ref: Richard Dodson 22/05/01
c
	call obsad('ceduna30m/antdiam',	30.d0)
	call obsad('ceduna30m/height', 161.d0)
	call obsad('ceduna30m/latitude',  obsdms(-1, 31,52,05.04))
	call obsad('ceduna30m/longitude', obsdms( 1,133,48,36.565))
	call obsad('ceduna30m/mount',	ALTAZ)
	call obsad('ceduna30m/nants',	1.d0)
c
c  CSO (from Oliver Lay -> MCHW 20may1997 - some values need confirmation)
c
	call obsad('cso/antdiam',	10.4d0)
	call obsad('cso/ellimit',	5.0*dpi/180.d0)
	call obsad('cso/evector',	0.5*dpi)
	call obsad('cso/height',	4080.0d0)
	call obsad('cso/jyperk',	60.d0)
	call obsad('cso/latitude',	obsdms( 1, 19,49,33.8))
	call obsad('cso/longitude',	obsdms(-1,155,28,46.4))
	call obsad('cso/mount',	ALTAZ)
	call obsad('cso/nants',	2.d0)
	call obsad('cso/systemp',	500.d0)
c
c  GMRT.
c
	call obsad('gmrt/antdiam',	45.d0)
	call obsad('gmrt/height',	650.0d0)
	call obsad('gmrt/latitude',	obsdms( 1,19,06,0.0))
	call obsad('gmrt/longitude',	obsdms(-1,74,03,0.0))
c
c  The HATCREEK mm array (BIMA).
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('hatcreek/antdiam',	6.1d0)
	call obsad('hatcreek/evector',	0.0d0)
	call obsad('hatcreek/height',	1043.d0)
	call obsad('hatcreek/jyperk',	120.d0)
	call obsad('hatcreek/latitude', obsdms( 1, 40,49, 2.50))
	call obsad('hatcreek/longitude',obsdms(-1,121,28,18.49))
	call obsad('hatcreek/mount',	ALTAZ)
	call obsad('hatcreek/nants',   10.d0)
	call obsad('hatcreek/subdiam',	0.61d0)
	call obsad('hatcreek/systemp',	300.d0)
c
c  The Hobart 26m dish.
c  Ref: misc NGS skd file from hobart
	call obsad('hobart26m/antdiam',	26.d0)
	call obsad('hobart26m/height',	66.12d0)
	call obsad('hobart26m/latitude',  obsdms(-1, 42,48,12.9))
	call obsad('hobart26m/longitude', obsdms( 1,147,26,25.9))
	call obsad('hobart26m/mount',	XYEW)
	call obsad('hobart26m/nants',	1.d0)
c
c  The IRAM mm array at PdB.
c  Ref: S.Guillaoteau etal., 1992, A&A 262, 624.
c
        call obsad('iram15m/antdiam',   15.d0)
        call obsad('iram15m/height',    2650.d0)
        call obsad('iram15m/jyperk',    24.d0)
        call obsad('iram15m/latitude',  obsdms( 1, 44,38,02.00))
        call obsad('iram15m/longitude', obsdms( 1,5,54,28.40))
        call obsad('iram15m/mount',     ALTAZ)
        call obsad('iram15m/nants',     6.d0)
        call obsad('iram15m/systemp',   300.d0)
c
c  JCMT (from Oliver Lay -> MCHW 20may1997)
c
	call obsad('jcmt/antdiam',	15.0d0)
	call obsad('jcmt/ellimit',	5.0*dpi/180.d0)
	call obsad('jcmt/evector',	0.5*dpi)
	call obsad('jcmt/height',	4092.0d0)
	call obsad('jcmt/jyperk',	40.d0)
	call obsad('jcmt/latitude',	obsdms( 1, 19,49,33.8))
	call obsad('jcmt/longitude',	obsdms(-1,155,28,46.4))
	call obsad('jcmt/mount',	ALTAZ)
	call obsad('jcmt/nants',	2.d0)
	call obsad('jcmt/subdiam',	0.75d0)
	call obsad('jcmt/systemp',	500.d0)
c
c  The Kitt Peak mm single dish (NRAO).
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('kittpeak/antdiam',	12.d0)
	call obsad('kittpeak/height',	1938.d0)
	call obsad('kittpeak/jyperk',	55.d0)
	call obsad('kittpeak/latitude',	obsdms( 1, 31,57,12.10))
	call obsad('kittpeak/longitude',obsdms(-1,111,36,51.12))
        call obsad('kittpeak/mount',     ALTAZ)
	call obsad('kittpeak/nants',	1.d0)
	call obsad('kittpeak/systemp',	200.d0)
c
c  The Mopra dish.
c
	call obsad('mopra/antdiam',	22.d0)
	call obsad('mopra/height',	866.44d0)
	call obsad('mopra/latitude',	obsdms(-1, 31,16,04.127))
	call obsad('mopra/longitude',	obsdms( 1,149,05,58.706))
	call obsad('mopra/mount',	ALTAZ)
	call obsad('mopra/nants',	1.d0)
c
c  Nobeyama 45 m single dish.
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('nobeyama45/antdiam',45.d0)
	call obsad('nobeyama45/jyperk',	6.d0)
	call obsad('nobeyama45/nants',	1.d0)
	call obsad('nobeyama45/systemp',500.d0)
c
c  The Nobeyama mm array.
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('nro10m/antdiam',	10.d0)
	call obsad('nro10m/height',	1350.d0)
	call obsad('nro10m/jyperk',	74.d0)
	call obsad('nro10m/latitude',	obsdms( 1, 35,56, 0.0))
	call obsad('nro10m/longitude',	obsdms( 1,138,29, 0.0))
	call obsad('nro10m/nants',	6.0d0)
	call obsad('nro10m/systemp',	300.d0)
c
c  Onsala Dish.
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('onsala/antdiam',	20.d0)
	call obsad('onsala/height',	10.d0)
	call obsad('onsala/jyperk',	28.d0)
	call obsad('onsala/latitude',	obsdms( 1, 57,23,46.60))
	call obsad('onsala/longitude',	obsdms( 1, 11,55,45.40))
	call obsad('onsala/nants',	1.d0)
	call obsad('onsala/systemp',	250.d0)
c
c  Owens Valley Radio Observatory (mm array).
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('ovro/antdiam',	10.4d0)
	call obsad('ovro/height',	1222.d0)
	call obsad('ovro/jyperk',	74.d0)
	call obsad('ovro/latitude',	obsdms( 1, 37,14, 0.00))
	call obsad('ovro/longitude',	obsdms(-1,118,17, 0.00))
	call obsad('ovro/mount',	ALTAZ)
	call obsad('ovro/nants',	6.0d0)
	call obsad('ovro/systemp',	300.d0)
c
c  Parkes.
c
	call obsad('parkes/antdiam',	64.d0)
	call obsad('parkes/ellimit',	30.5d0*dpi/180.d0)
	call obsad('parkes/height',	414.80d0)
	call obsad('parkes/latitude',	obsdms(-1, 32,59,54.263)) 
	call obsad('parkes/longitude',	obsdms( 1,148,15,48.636))
	call obsad('parkes/mount',	ALTAZ)
	call obsad('parkes/nants',	1.d0)
	call obsad('parkes/subdiam',	3.0d0)
c
c  Some Penticton parameters.
c
	call obsad('penticton/antdiam',	9.0d0)
	call obsad('penticton/height',	156.d0)
	call obsad('penticton/latitude', obsdms( 1, 49,19,24.0))
	call obsad('penticton/longitude',obsdms(-1,119,37,12.0))
c
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('quabbin/antdiam',	15.d0)
	call obsad('quabbin/jyperk',	45.d0)
	call obsad('quabbin/systemp',	240.d0)
c
c  RPA - seti array near Leuchner
c
        call obsad('rpa/antdiam',       3.6d0)
        call obsad('rpa/ellimit',       8.0d0*dpi/180.d0)
        call obsad('rpa/height',        238.d0)
        call obsad('rpa/jyperk',        418.d0)
        call obsad('rpa/latitude',      obsdms( 1, 37,55, 3.6))
        call obsad('rpa/longitude',     obsdms(-1,122, 9,21.0))
        call obsad('rpa/mount',         ALTAZ)
        call obsad('rpa/nants',         6.d0)
        call obsad('rpa/systemp',       60.d0)
c
c  SEST.
c
	call obsad('sest/antdiam',	15.d0)
	call obsad('sest/height',	2400.d0)
	call obsad('sest/latitude',	obsdms(-1,29,15,34.0))
	call obsad('sest/longitude',	obsdms( 1,70,44,04.0))
c
c  Submillimeter Array (SMA).
c  Supplied by Ramprasad Rao
c
	call obsad('sma/antdiam',	6.0d0)
	call obsad('sma/height',	4080.0d0)
	call obsad('sma/jyperk',	130.d0)
	call obsad('sma/latitude',	obsdms( 1, 19,49,33.8))
	call obsad('sma/longitude',	obsdms(-1,155,28,46.4))
	call obsad('sma/mount',	NASMYTH)
	call obsad('sma/nants',	8.0d0)
c
c  SZA - Sunyaev-Zel'dovich Array of eight 3.5m antennas - part of CARMA.
c
        call obsad('sza/antdiam',     3.5d0)
        call obsad('sza/ellimit',     5.0*dpi/180.d0)
        call obsad('sza/evector',     0.5*dpi)
        call obsad('sza/height',      2400.0d0)
        call obsad('sza/jyperk',      383.d0)
        call obsad('sza/latitude',     obsdms( 1, 37,14, 0.00))
        call obsad('sza/longitude',    obsdms(-1,118,17, 0.00))
        call obsad('sza/mount',       ALTAZ)
        call obsad('sza/nants',       8.d0)
        call obsad('sza/systemp',     200.d0)
c
c  SZA10 - SZA cross correlalations of 3.5m and 10.4m antennas - part of CARMA.
c
        call obsad('sza10/antdiam',     6.0d0)
        call obsad('sza10/ellimit',     5.0*dpi/180.d0)
        call obsad('sza10/evector',     0.5*dpi)
        call obsad('sza10/height',      2400.0d0)
        call obsad('sza10/jyperk',      128.d0)
        call obsad('sza10/latitude',     obsdms( 1, 37,14, 0.00))
        call obsad('sza10/longitude',    obsdms(-1,118,17, 0.00))
        call obsad('sza10/mount',       ALTAZ)
        call obsad('sza10/nants',       8.d0)
        call obsad('sza10/systemp',     200.d0)
c
c  SZA6 - SZA cross correlalations of 3.5m and 6.1m antennas - part of CARMA.
c
        call obsad('sza6/antdiam',     4.6d0)
        call obsad('sza6/ellimit',     5.0*dpi/180.d0)
        call obsad('sza6/evector',     0.5*dpi)
        call obsad('sza6/height',      2400.0d0)
        call obsad('sza6/jyperk',      220.d0)
        call obsad('sza6/latitude',     obsdms( 1, 37,14, 0.00))
        call obsad('sza6/longitude',    obsdms(-1,118,17, 0.00))
        call obsad('sza6/mount',       ALTAZ)
        call obsad('sza6/nants',       8.d0)
        call obsad('sza6/systemp',     200.d0)
c
c  The Very Large Array (NRAO).
c  Values taken from the Green Book (pages 1-10, 1-16, 6-17).
c
	call obsad('vla/antdiam',	25.d0)
	call obsad('vla/ellimit',	8.0d0*dpi/180.d0)
	call obsad('vla/height',	2124.d0)
	call obsad('vla/jyperk',	8.d0)
	call obsad('vla/latitude',	obsdms( 1, 34, 4,43.497))
	call obsad('vla/longitude',	obsdms(-1,107,37, 3.819))
	call obsad('vla/mount',		ALTAZ)
	call obsad('vla/nants',		27.d0)
	call obsad('vla/systemp',	60.d0)
c
c  Westerbork Synthesis Radio Telescope (NFRA).
c  Latitude and longitude given by Noordam, which differ from the
c  the values in the ephemeris (lat=52:55:00.90, long=6:35:15.00)
c
	call obsad('wsrt/antdiam',	25.d0)
	call obsad('wsrt/evector',	-0.5*dpi)
	call obsad('wsrt/ew',		1.d0)
	call obsad('wsrt/height',	5.d0)
	call obsad('wsrt/jyperk',	8.d0)
	call obsad('wsrt/latitude', 	obsdms( 1, 52,43,53.84))
	call obsad('wsrt/longitude',	obsdms( 1,  6,36,15.01))
	call obsad('wsrt/mount',	EQUATOR)
	call obsad('wsrt/nants',	14.d0)
c       gmx (Jan 27, 2004): Added system temperature
c       see http://www.astron.nl/wsrt/wsrtGuide/node6.html
	call obsad('wsrt/systemp',	30.0d0)
c
c
	end
c************************************************************************
	subroutine obsad(name,value)
c
	implicit none
	character name*(*)
	double precision value
c
c  Add a new parameter to the list of parameters.
c------------------------------------------------------------------------
	include 'obspar.h'
	character line*24
c
c  Check for sensible values.
c
	line = name
	if(nparms.eq.MAXPARMS)
     *		call bug('f','Buffer overflow in ObsAdd:'//line)
	if(nparms.gt.0)then
	  if(name.le.parname(nparms))
     *		call bug('f','ObsInit list not ordered:'//line)
	endif
	nparms = nparms + 1
	if(len(name).gt.len(parname(nparms)))
     *		call bug('f','Name too long in ObsInit list:'//line)
c
	parname(nparms) = name
	parvalue(nparms)  = value
	end
c************************************************************************
	double precision function obsdms(s,deg,m,sec)
c
	implicit none
	integer deg,m,s
	real sec
c
c  Convert degrees, minutes, seconds to radians. The angle is
c  s*(deg + m/60 + sec/3600)
c
c  Inputs:
c    s		The sign of the angle.
c    deg	Degrees -- must be positive!
c    m		Minutes.
c    sec	Seconds.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	if(min(deg,m).lt.0.or.sec.lt.0)
     *	  call bug('f','Negative value in obsdms')
	if(abs(s).ne.1)call bug('f','Bad sign in obsdms')
	obsdms = dble(deg) + m/60.d0 + sec/3600.d0
	obsdms = s*dpi/180.d0 * obsdms
	end
