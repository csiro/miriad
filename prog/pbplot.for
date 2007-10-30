c************************************************************************
	program pbplot
	implicit none
c
c= pbplot -- Plot primary beam shapes.
c& rjs
c: utility
c+
c	PBPLOOT plots the primary beam function.
c@ telescop
c	This is used to determine the type of the primary beam. Several
c	values can be given.
c@ freq
c	The frequency, in GHz, at which to determine the primary beam. The
c	default is 1.4 GHz.
c@ options
c	Extra processing options. There is only one possibility at the moment.
c@ device
c	PGPLOT device. Default is no plot.
c--
c  History:
c    rjs  20oct94 Original version.
c  Bugs:
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	integer npts,MAXTEL
	parameter(version='PbPlot: version 1.0 20-Oct-94')
	parameter(npts=256,MAXTEL=8)
c
	character device*64,telescop(MAXTEL)*16,line*64
	real freq,x(npts),y(npts,MAXTEL),xmin,xmax,ymin,ymax
	real maxrad,pbfwhm,cutoff,delta
	integer i,j,ntel,pbObj(MAXTEL),coObj
	logical doder
c
c  Externals.
c
	real pb1der,pb1get
	integer pgbeg
c
c  Get input parameters.
c
	call output(version)
	call keyini
	call mkeya('telescop',telescop,MAXTEL,ntel)
	if(ntel.eq.0)call bug('f','A telescope name must be given')
	call keyr('freq',freq,1.4)
	if(freq.le.0)call bug('f','Invalid frequency')
	call keya('device',device,' ')
	call GetOpt(doder)
	call keyfin
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
	  call pb1Init(pbObj(j),telescop(j),coObj)
	  call pb1Info(pbObj(j),pbfwhm,cutoff,maxrad)
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
	      y(i,j) = pb1Der(pbObj(j),x(i),0.)
	    else
	      y(i,j) = pb1Get(pbObj(j),x(i),0.)
	    endif
	  enddo
	  call pb1Fin(pbObj(j))
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
	delta = 0.05 * (xmax - xmin)
	xmax = xmax + delta
	xmin = xmin - delta
	delta = 0.05 * (ymax - ymin)
	ymax = ymax + delta
	ymin = ymin - delta
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
	  call pgswin(xmin,xmax,ymin,ymax)
	  call pgtbox('BCNST',0.,0,'BCNST',0.,0)
	  call pgsls(1)
	  do j=1,ntel
	    call pgline(npts,x,y(1,j))
	  enddo
	  if(doder)then
	    call pglab(	'Radial Distance (arcmin)',
     *			'Primary Beam Derivative',' ')
	  else
	    call pglab(	'Radial Distance (arcmin)',
     *			'Primary Beam Response',' ')
	  endif
	  call pgend
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
