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
c    observ	Name of the observatory. Possible values are:
c		  'HATCREEK', 'VLA', 'ATCA' (Australia Telescope)
c		  'WSRT' (Westerbork).
c    object	The parameter of the observatory of interest. Possible
c		values are:
c		 'latitude '	Observatory latitude, in radians.
c		 'longitude'	Observatory longitude, in radians.
c		 'jyperk'	Typical system gain, in Jy/K.
c		 'evector'	Offset angle of the feed to the local
c				vertical.
c		 'mount'	Telescope mount: 0 = alt-az
c						 1   equitorial
c		 'antdiam'	Antenna diameter, in meters.
c  Output:
c    value	The value of the parameter.
c    ok		True if the value was successfully found.
c--
c  History:
c    rjs  20jun91 Original version.
c    rjs   2jul91 Corrected Westerbork parameters, as suggested by pjt.
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision dpi4
	parameter(dpi4=0.25d0*dpi)
	integer nobs,nobjects
	parameter(nobjects=6,nobs=4)
	integer hatcreek,vla,atca,wsrt
	parameter(atca=1,hatcreek=2,vla=3,wsrt=4)
c
	integer i,j,k
	character observs(nobs)*8,objects(nobjects)*12
	double precision values(nobjects,nobs)
c
c  The list of objects and observatories.
c
	data observs/'ATCA    ','HATCREEK','VLA     ','WSRT    '/
	data objects/
     *	  'evector     ','jyperk      ','latitude    ','longitude   ',
     *	  'mount       ','antdiam     '/
c
c  Tables of telescope characteristics.
c------------------------------------------------------------------------
c  Australia Telescope.
c    Feed offset=45 deg, Jy/K = 10, lat=-30:18:52.02,long=149:34:00.94
c    Mount=alt-az, diameter=22 m.
c
	data (values(i,atca),	 i=1,nobjects)/
     *	  dpi4,        13.d0,-0.52908695858305821, 2.61043534181478787,
     *	  0.d0,	       22.d0/
c
c  Hat Creek.
c    Feed offset=??, Jy/K ~ 200, lat=40:49:02.50,long=-121:28:18.49
c    Mount=alt-az, diameter=6.1 m.
c
	data (values(i,hatcreek),i=1,nobjects)/
     *	  0.d0,	      200.d0, 0.71239734336437988,-2.12008290680541611,
     *	  0.d0,	       6.1d0/
c
c  VLA.
c    Feed offset=??, Jy/K ~ 8, lat=34:04:43.50,long=-107:37:03.82
c    Mount=alt-az, diameter=25 m.
c
	data (values(i,vla),	 i=1,nobjects)/
     *	  0.d0,         8.d0, 0.59478637791960720,-1.87828367838904553,
     *	  0.d0,	       25.d0/
c
c  Westerbork.
c    Feed offset=moves!, Jy/K ~ 8?, lat=52:43:53.84,long=6:36:15.01
c    Mount=equitorial, diameter=25 m.
c  The lat,long given above are those given by Jan Noordam, which differ
c  from the ephemeris values of lat=52:55:00.90, long=6:35:15.00
c
	data (values(i,wsrt),	 i=1,nobjects)/
     *	  0.d0,	        8.d0, 0.92034042769558678, 0.11526450116516021,
c     *	  0.d0,	        8.d0, 0.92357442583679605, 0.11526445268379217,
     *	  1.d0,	       25.d0/
c------------------------------------------------------------------------
c  Determine the observatory and the parameter.
c
	ok = .false.
	j = 0
	do i=1,nobs
	  if(observs(i).eq.observ) j = i
	enddo
	if(j.eq.0)return
	k = 0
	do i=1,nobjects
	  if(objects(i).eq.object) k = i
	enddo
	if(k.eq.0)return
c
c  Return the parameter.
c
	value = values(k,j)
	ok = .true.
	end
