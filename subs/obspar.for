c************************************************************************
c* ObsPar - Get characteristics of a particular observatory.
c: utility
c& rjs
c+
	subroutine obspar(observ,object,value,ok)
c
	implicit none
	character observ*(*),object*(*)
	double precision value
	logical ok
c
c  THis returns some known characteristics of various observervatories.
c
c  Input:
c    observ	Name of the observatory. For example:
c		  'HATCREEK', 'VLA', 'ATCA' (Australia Telescope)
c		  'WSRT' (Westerbork).
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
c		 'height'	Height above sea level, in meters
c		 'ew'		Positive if the telescope is an E-W array.
c  Output:
c    value	The value of the parameter.
c    ok		True if the value was successfully found.
c--
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
c------------------------------------------------------------------------
	include 'obspar.h'
	character name*24
	integer l
c
	logical first
	save first
c
c  Externals.
c
	integer len1,binsrcha
c
	data first/.true./
c
c  Initialise the list of known parameters, if necessary.
c
	if(first)call obsinit
	first = .false.
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
c  Externals.
c
	double precision obsdms
c
	nparms = 0
c
c  The Australia Telescope Compact Array (ATNF).
c
	call obsad('atca/antdiam',	22.d0)
	call obsad('atca/evector',	0.25*dpi)
	call obsad('atca/ew',		1.d0)
	call obsad('atca/height',	217.d0)
	call obsad('atca/jyperk',	13.d0)
	call obsad('atca/latitude',	obsdms(-1, 30,18,52.02))
	call obsad('atca/longitude',	obsdms( 1,149,34, 0.94))
	call obsad('atca/mount',	0.d0)
	call obsad('atca/systemp',	50.d0)
c
c  GMRT.
c
	call obsad('gmrt/antdiam',	45.d0)
c
c  The Hat Ck mm array (BIMA).
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('hatcreek/antdiam',	6.1d0)
	call obsad('hatcreek/height',	1043.d0)
	call obsad('hatcreek/jyperk',	160.d0)
	call obsad('hatcreek/latitude', obsdms( 1, 40,49, 2.50))
	call obsad('hatcreek/longitude',obsdms(-1,121,28,18.49))
	call obsad('hatcreek/mount',	0.d0)
	call obsad('hatcreek/subdiam',	0.61d0)
	call obsad('hatcreek/systemp',	300.d0)
c
c  The Kitt Peak mm single dish (NRAO).
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('kittpeak/antdiam',	12.d0)
	call obsad('kittpeak/height',	1938.d0)
	call obsad('kittpeak/jyperk',	55.d0)
	call obsad('kittpeak/latitude',	obsdms( 1, 31,57,12.10))
	call obsad('kittpeak/longitude',obsdms(-1,111,36,51.12))
	call obsad('kittpeak/systemp',	200.d0)
c
c  The Nobeyama mm array.
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('nobeyama/antdiam',	10.d0)
	call obsad('nobeyama/jyperk',	74.d0)
	call obsad('nobeyama/systemp',	300.d0)
c
c  Nobeyama 45 m single dish.
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('nobeyama45/antdiam',45.d0)
	call obsad('nobeyama45/jyperk',	6.d0)
	call obsad('nobeyama45/systemp',500.d0)
c
c  Onsala Dish.
c  Jyperk and systemp given by Wright, from 3mm vlbi.
c
	call obsad('onsala/antdiam',	20.d0)
	call obsad('onsala/height',	10.d0)
	call obsad('onsala/jyperk',	28.d0)
	call obsad('onsala/latitude',	obsdms( 1, 57,23,46.60))
	call obsad('onsala/longitude',	obsdms( 1, 11,55,45.40))
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
	call obsad('vla/mount',		0.d0)
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
	call obsad('wsrt/mount',	1.d0)
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
