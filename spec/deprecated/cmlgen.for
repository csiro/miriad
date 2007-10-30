c************************************************************************
	program cmlgen
	implicit none
c= cmlgen -- Generate "select" files for given CML range of Jovian rotation.
c& rjs
c: uv analysis
c+
c@ times
c	The start and end observing times, in normal Miriad format.
c	No default.
c@ cml
c	The start and end CML values, in degrees. No default.
c@ out
c	The output text file. No default.
c@ telescop
c	The observatory of interest. The default is the ATCA.
c--
c------------------------------------------------------------------------
	character version*(*)
	double precision MINUTE
	parameter(version='CMLgen: version 1.0 19-Jun-97')
	parameter(MINUTE=1.d0/24.d0/60.d0)
	double precision t1,t2,lat,long,tstart,tprev
	logical ok
	real cml1,cml2
	integer lu,iostat
	character out*64,observ*32
c
	logical within
c
	call output(version)
	call keyini
	call keyt('times',t1,'atime',0.d0)
	call keyt('times',t2,'atime',0.d0)
	call keyr('cml',cml1,0.)
	call keyr('cml',cml2,0.)
	call keya('out',out,' ')
	call keya('telescop',observ,'atca')
	call keyfin
c
c  Get the latitude and longitude of the observatory.
c
	call obspar(observ,'latitude',lat,ok)
	if(ok)call obspar(observ,'longitude',long,ok)
	if(.not.ok)call bug('f',
     *		'Failed to determine observatory lat/long')
c
c  Open the output.
c
	call txtopen(lu,out,'new',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Do the real work.
c
	ok = .false.
	dowhile(t1.lt.t2)
	  if(within(t1,lat,long,cml1,cml2))then
	    if(.not.ok)tstart = t1
	    tprev = t1
	    ok = .true.
	  else if(ok)then
	    if(tstart.lt.tprev)call Seltime(lu,tstart,tprev)
	    ok = .false.
	  endif
	  t1 = t1 + MINUTE
	enddo
c
	if(ok.and.tstart.lt.t2)call Seltime(lu,tstart,t2)
	call txtclose(lu)
	end
c************************************************************************
	subroutine Seltime(lu,tstart,tend)
c
	implicit none
	integer lu
	double precision tstart,tend
c------------------------------------------------------------------------
	character line*80,t1*32,t2*32
	integer l1,l2,iostat
c
	integer len1
	call julday(tstart,'H',t1)
	l1 = len1(t1)
	call julday(tend,'H',t2)
	l2 = len1(t2)
	line = 'time('//t1(1:l1)//','//t2(1:l2)//')'
	call txtwrite(lu,line,len1(line),iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	end
c************************************************************************
	logical function Within(time,lat,long,cml1,cml2)
c
	implicit none	
	double precision time,lat,long
	real cml1,cml2
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer JUPITER,EARTH
	parameter(JUPITER=5,EARTH=3)
	double precision sub(3),dist,tdb,ra,dec,w,r,f,jlong,jlat
	real bmaj,bmin,bpa
c
	double precision deltime
	logical JupUp
c
	tdb = time + deltime(time,'tdb')
	call plphyeph(tdb,JUPITER,ra,dec,w,r,f)
	call plpar(tdb,JUPITER,sub,dist,bmaj,bmin,bpa)
	call plradec(tdb,JUPITER,ra,dec)
c
c  Is Jupiter up?
c
	if(JupUp(time,lat,long,ra,dec))then
	  sub(2) = -sub(2)
	  call lmn2sph(sub,jlong,jlat)
	  jlong = mod(180./DPI * jlong,360.d0)
	  if(jlong.lt.0)jlong = jlong + 360.d0
	  if(cml2.lt.cml1)then
	    Within = jlong.ge.cml1.or.jlong.le.cml2
	  else
	    Within = jlong.ge.cml1.and.jlong.le.cml2
	  endif
	else
	  Within = .false.
	endif
c
	end
c************************************************************************
	logical function JupUp(time,lat,long,ra,dec)
c
	implicit none
	double precision time,lat,long,ra,dec
c------------------------------------------------------------------------
	include 'mirconst.h'
	real sinl,cosl,sind,cosd,sinel,temp,ha,sha
	double precision lst
c
	sinl = sin(lat)
	cosl = cos(lat)
	sind = sin(dec)
	cosd = cos(dec)
	sinel = sin(10.*PI/180.)
c
	call jullst(time,long,lst)
c
	temp = (sinel - sinl*sind) / (cosl*cosd)
c
	if(abs(temp).gt.1)then
	  if(dec*lat.lt.0)then
	    JupUp = .false.
	  else
	    JupUp = .true.
	  endif
	else
	  sha = acos(temp)
	  ha = mod(lst - ra,2*DPI)
	  if(ha.gt.PI) ha = ha - 2*PI
	  if(ha.lt.-PI) ha = ha + 2*PI
	  JupUp = abs(ha).le.sha
	endif
c
	end
