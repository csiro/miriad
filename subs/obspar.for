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
c                 'ATCA', 'CSO', 'GMRT', 'HATCREEK', 'IRAM15M', 'JCMT',
c                 'KITTPEAK', 'NOBEYAMA', 'NOBEYAMA45', 'ONSALA', 'OVRO',
c		  'PARKES', 'PENTICTON', 'QUABBIN', 'VLA', 'WSRT'
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
	double precision ALTAZ,EQUATOR
	parameter(ALTAZ=0.d0,EQUATOR=1.d0)
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
c  The Australia Telescope Compact Array (ATNF).
c  Latitude, longitude and height refer to station 35. Info from
c  John Reynolds (IAU 1976 spheroid).
c
	call obsad('atca/antdiam',	22.d0)
	call obsad('atca/ellimit',	12.0*dpi/180.d0)
	call obsad('atca/evector',	0.25*dpi)
	call obsad('atca/ew',		1.d0)
	call obsad('atca/height',	213.869d0)
	call obsad('atca/jyperk',	13.d0)
	call obsad('atca/latitude',	obsdms(-1, 30,18,46.3849))
	call obsad('atca/longitude',	obsdms( 1,149,34, 0.4997))
	call obsad('atca/mount',	ALTAZ)
	call obsad('atca/nants',	6.d0)
	call obsad('atca/subdiam',	2.8d0)
	call obsad('atca/systemp',	50.d0)
c
c  CSO (from Oliver Lay -> MCHW 20may1997 - some values need confirmation)
c
	call obsad('cso/antdiam',	10.4d0)
	call obsad('cso/ellimit',	5.0*dpi/180.d0)
	call obsad('cso/evector',	0.5*dpi)
	call obsad('cso/height',	4080.0d0)
	call obsad('cso/jyperk',	60.d0)
	call obsad('cso/latitude',	obsdms( 1, 19,49,33.8))
	call obsad('cso/longitude',	obsdms( 1,155,28,46.4))
	call obsad('cso/mount',	ALTAZ)
	call obsad('cso/nants',	2.d0)
	call obsad('cso/systemp',	500.d0)
c
c  GMRT.
c
	call obsad('gmrt/antdiam',	45.d0)
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
c  The IRAM mm array at PdB.
c  Ref: S.Guillaoteau etal., 1992, A&A 262, 624.
c
        call obsad('iram15m/antdiam',   15.d0)
        call obsad('iram15m/height',    2650.d0)
        call obsad('iram15m/jyperk',    24.d0)
        call obsad('iram15m/latitude',  obsdms( 1, 44,38,02.00))
        call obsad('iram15m/longitude',obsdms(-1,5,54,28.40))
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
	call obsad('jcmt/longitude',	obsdms( 1,155,28,46.4))
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
	call obsad('kittpeak/nants',	1.d0)
	call obsad('kittpeak/systemp',	200.d0)
c
c  The Mopra dish.
c
	call obsad('mopra/antdiam',	22.d0)
	call obsad('mopra/height',	866.047d0)
	call obsad('mopra/latitude',	obsdms(-1, 31,16,04.3195))
	call obsad('mopra/longitude',	obsdms( 1,149,05,58.7740))
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
	call obsad('ovro/systemp',	300.d0)
c
c  Parkes.
c
	call obsad('parkes/antdiam',	64.d0)
	call obsad('parkes/ellimit',	30.5d0*dpi/180.d0)
	call obsad('parkes/height',	411.756d0)
	call obsad('parkes/latitude',	obsdms(-1, 32,59,54.2552)) 
	call obsad('parkes/longitude',	obsdms( 1,148,15,48.6393))
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
c  The Very Large Array (NRAO).
c  Values taken from the Green Book (pages 1-10, 1-16, 6-17).
c
	call obsad('vla/antdiam',	25.d0)
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
