c************************************************************************
	program pbplot
	implicit none
c
c= pbplot -- Plot primary beam shapes.
c& rjs
c: utility
c+
c	PBPLOT plots the primary beam function.
c@ pbtype
c	This is used to determine the type of the primary beam. Several
c	values can be given. Normally this will simply be a telescope
c	name.
c@ freq
c	The frequency, in GHz, at which to determine the primary beam. The
c	default is 1.4 GHz.
c@ options
c	Extra processing options. There is only one possibility at the moment.
c	  derivative  Plot the derivative with frequency of the primary
c	              beam (rather than the normal primary beam).
c@ device
c	PGPLOT device. Default is no plot.
c--
c  History:
c    rjs  20oct94 Original version.
c    rjs  21nov95 Added title.
c  Bugs:
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	integer npts,MAXTEL
	parameter(version='PbPlot: version 1.0 21-Nov-95')
	parameter(npts=256,MAXTEL=8)
c
	character device*64,telescop(MAXTEL)*16,line*64,title*32
	real freq,x(npts),y(npts,MAXTEL),maxrad,pbfwhm,cutoff
	real xmin,xmax,ymin,ymax,xlo,xhi,ylo,yhi
	integer i,j,ntel,pbObj(MAXTEL),coObj,length
	logical doder
c
c  Externals.
c
	real pbder,pbget
	integer pgbeg,len1
c
c  Get input parameters.
c
	call output(version)
	call keyini
	call mkeya('pbtype',telescop,MAXTEL,ntel)
	call keyr('freq',freq,1.4)
	if(freq.le.0)call bug('f','Invalid frequency')
	call keya('device',device,' ')
	call GetOpt(doder)
	call keyfin
c
c  If no telescopes were given, just list the possibilities.
c
	if(ntel.eq.0)then
	  call pbList
	else
c
c  Create a simple coorindate object.
c
	call coRaDec(coObj,'SIN',0.d0,0.d0)
	call coAxSet(coObj,3,'FREQ',0.d0,dble(freq),0.1d0*dble(freq))
	call coReinit(coObj)
c
c  Create the primary beam objects, give some messgages, and determine
c  the max value of X to plot.
c
	xmax = 0
	do j=1,ntel
	  call pbInit(pbObj(j),telescop(j),coObj)
	  call pbInfo(pbObj(j),pbfwhm,cutoff,maxrad)
	  call output('Primary beam: '//telescop(j))
	  write(line,10)180*60/pi*pbfwhm
  10	  format('  FWHM (arcmin):',f7.2)
	  call output(line)
	  write(line,15)180*60/pi*maxrad
  15	  format('  Cutoff Radius:',f7.2)
	  call output(line)
	  write(line,20)cutoff
  20	  format('  Cutoff Value: ',f6.3)
	  call output(line)
	  xmax = max(xmax, maxrad)
	  if(j.eq.1)then
	    title = 'Telescopes: '//telescop(1)
	  else
	    title(length+1:) = ','//telescop(j)
	  endif
	  length = len1(title)
	enddo
c
c  Evaluate the primary beams at npt points.
c
	do i=1,npts
	  x(i) = xmax/real(npts-1) * (i-1)
	enddo
c
	do j=1,ntel
	  do i=1,npts
	    if(doder)then
	      y(i,j) = pbDer(pbObj(j),x(i),0.)
	    else
	      y(i,j) = pbGet(pbObj(j),x(i),0.)
	    endif
	  enddo
	  call pbFin(pbObj(j))
	enddo
	call coFin(coObj)
c
c  Determine min anx max values.
c
	xmin = 0
	xmax = 180*60/pi * xmax
	do i=1,npts
	  x(i) = 180*60/pi * x(i)
	enddo
c
	ymin = y(1,1)
	ymax = ymin
	do j=1,ntel
	  do i=1,npts
	    ymax = max(ymax,y(i,j))
	    ymin = min(ymin,y(i,j))
	  enddo
	enddo
c
	call pgrnge(xmin,xmax,xlo,xhi)
	call pgrnge(ymin,ymax,ylo,yhi)
c
c  Do the plotting.
c
	if(device.ne.' ')then
	  if(pgbeg(0,device,1,1).ne.1)then
	    call pgldev
	    call bug('f','Error opening graphics device')
	  endif
	  call pgpage
          call pgvstd
	  call pgswin(xlo,xhi,ylo,yhi)
	  call pgtbox('BCNST',0.,0,'BCNST',0.,0)
	  call pgsls(1)
	  do j=1,ntel
	    call pgline(npts,x,y(1,j))
	  enddo
	  if(doder)then
	    call pglab(	'Radial Distance (arcmin)',
     *			'Primary Beam Derivative',title(1:length))
	  else
	    call pglab(	'Radial Distance (arcmin)',
     *			'Primary Beam Response',title(1:length))
	  endif
	  call pgend
	endif
	endif
c
	end
c************************************************************************
	subroutine GetOpt(doder)
c
	implicit none
	logical doder
c
c  Get processing options.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*10
	logical present(NOPTS)
c
	data opts/'derivative'/
c
	call options('options',opts,present,NOPTS)
	doder = present(1)
	end
