c************************************************************************
	program paraplot
	implicit none
c
c= paraplot -- Plot parallactic and elevation angle for a source.
c& rjs
c: utility
c+
c	PARAPLOT plots parallactic angle and elevation as a function of
c	hour angle.
c@ lat
c	Observatory latitude, in degrees. The default is the AT's latitude.
c@ elev
c	Elevation limit, in degrees. Default is 15 degrees.
c@ dec
c	Source declination, in degrees. Default is the same declination
c	as the observatory.
c@ device
c	PGPLOT device. Default is no plot.
c@ log
c	Log file. Default is no log file.
c--
c  History:
c    rjs  22may91 Original version.
c    nebk 22may91 Twiddled about with the plot a bit
c    rjs  13jun91 Eliminated chi offset fiddle.
c    rjs  24apr91 PGPLOT standardisation.
c    mjs  13mar93 pgplot subr names have less than 7 chars.
c    nebk 29jun93 Add 'O' to PGTBOX options (omit leading zeros)
c    rjs  14oct98 Plot sin(2*chi) as well as cos(2*chi).
c
c  Bugs:
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	integer npts
	parameter(version='ParaPlot: version 1.0 14-Oct-98')
	parameter(npts=256)
c
	double precision lat,elev,dec,rise,set,temp,delta,a,b
	double precision theta
	integer i
	logical more
	character device*32,logfile*32,line*64
	real t,xlo,xhi,ylo1,yhi1,ylo2,yhi2
	real time(npts),chi(npts),el(npts),c2chi(npts),s2chi(npts)
c
c  Externals.
c
	character hangle*32,rangle*32
	integer pgbeg
c
	call output(version)
	call keyini
	call keyd('lat',lat,-30d0 - 18d0/60 - 52.019d0/3600)
	call keyd('elev',elev,15.d0)
	call keyd('dec',dec,lat)	
	call keya('device',device,' ')
	call keya('log',logfile,' ')
	call keyfin
c
c  Check the inputs, and convert to radians.
c
	if(device.eq.' '.and.logfile.eq.' ')
     *	  call bug('f','Either the plot device or log file needed')
	if(elev.lt.0.or.elev.gt.90)
     *	  call bug('f','Bad minimum elevation')
	if(lat.lt.-90.or.lat.gt.90)
     *	  call bug('f','Bad observatory latitude')
	if(dec.lt.-90.or.dec.gt.90)
     *	  call bug('f','Bad source declination')
c
	elev = pi/180 * elev
	lat  = pi/180 * lat
	dec  = pi/180 * dec
c
c  Determine the rise and set times.
c
	temp = (sin(elev) - sin(lat)*sin(dec) ) / ( cos(lat)*cos(dec) )
	if(abs(temp).gt.1)then
	  if(dec*lat.lt.0)then
	    call bug('f','Source never rises to minimum elevation')
	  else
	    call output('Source is always above the horizon!')
	    rise = -pi
	    set =   pi
	  endif
	else
	  rise = -acos(temp)
	  set  = -rise
	  call output('Set time is '//hangle(set))
	endif
c
c  Calculate the parallactic angle and elevation, as a function of
c  hour angle.
c
	delta = (set - rise) / (npts-1)
	a = sin(lat)*sin(dec)
	b = cos(lat)*cos(dec)
	theta = rise
	do i=1,npts
	  time(i)  = 24* 3600 * theta / (2*pi)
	  call parang(0.d0,dec,theta,lat,t)
	  chi(i)   = 180/pi * t
	  c2chi(i) = cos(2*t)
	  s2chi(i) = sin(2*t)
	  el(i)    = 180/pi * asin(a + b*cos(theta))
	  if(i.eq.1)then
	    xlo = time(1)
	    xhi = xlo
	    ylo1 = min(chi(1),el(1))
	    yhi1 = max(chi(1),el(1))
	    ylo2 = min(c2chi(1),s2chi(1))
	    yhi2 = ylo1
	  else
	    xlo = min(xlo,time(i))
	    xhi = max(xhi,time(i))
	    ylo1 = min(ylo1,chi(i),el(i))
	    yhi1 = max(yhi1,chi(i),el(i))
	    ylo2 = min(ylo2,c2chi(i),s2chi(i))
	    yhi2 = max(yhi2,c2chi(i),s2chi(i))
	  endif
	  theta = theta + delta
	enddo
c
	line = 'Declination: '//rangle(dec)
c
c  Do the plotting.
c
	if(device.ne.' ')then
	  if(pgbeg(0,device,1,2).ne.1)then
	    call pgldev
	    call bug('f','Error opening graphics device')
	  endif
	  call pgpage
	  call pgsch(1.6)
          call pgvstd
	  call pgswin(xlo,xhi,ylo1,yhi1)
	  call pgtbox('BCSTNHZO',0.,0,'BCNST',0.,0)
	  call pgsls(1)
	  call pgline(npts,time,chi)
	  call pgsls(2)
	  call pgline(npts,time,el)
	  call pglab('Hour Angle','\gx and Elevation (degrees)',line)
	  call pgpage
c
	  call pgsls(1)
	  call pgswin(xlo,xhi,-1.0,1.0)
	  call pgtbox('BCNSTHZO',0.,0,'BCNST',0.,0)
	  call pgline(npts,time,c2chi)
	  call pglab('Hour Angle','Cos(2\gx) and Sin(2\gx)',' ')
	  call pgsls(2)
	  call pgline(npts,time,s2chi)
	  call pgend
	endif
c
c  Write out the log file.
c
	if(logfile.ne.' ')then
	  call LogOpen(logfile,' ')
	  call LogWrite(line,more)
	  call LogWrite('   Time        Chi        Elev      cos(2chi)',
     *								   more)
	  do i=1,npts
	    write(line,'(4(1pg12.4))')time(i)/3600,chi(i),el(i),c2chi(i)
	    call LogWrite(line,more)
	  enddo
	  call LogClose
	endif
	end
