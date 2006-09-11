c************************************************************************
	program calplot
	implicit none
c
c= calplot -- Plot flux of primary calibrators
c& rjs
c: utility
c+
c	CALPLOT plots the flux of primary calibrators as a function
c	of frequency.
c@ source
c	The source to be plotted. No default.
c@ xrange
c	This gives two numbers, being the frequency range to plot, in GHz.
c	The default is 4.7,4.9.
c@ stokes
c	The Stokes parameters to be plotted. The default is to plot
c	only I.
c@ device
c	PGPLOT device. No default.
c--
c  History:
c    rjs  19mar93 Derived from PARAPLOT.
c    nebk 01mar95 Allow smaller ranges
c
c  Bugs:
c   * Perfect?
c------------------------------------------------------------------------
	character version*(*)
	integer NPTS,MAXPOL,NCOL
	parameter(version='CalPlot: version 1.0 01-Mar-95')
	parameter(NPTS=256,MAXPOL=4,NCOL=12)
c
	character source*32,device*32,line*80,stokes*4
	integer npol,pol(MAXPOL),length,ierr,cols(NCOL),i,j
	real x(NPTS),y(NPTS,MAXPOL),xrange(2)
	real xlo,xhi,ylo,yhi,delta,maxv,sdf
	double precision freq(NPTS)
c
c  Externals.
c
	integer pgbeg,PolsP2C,len1
	logical keyprsnt
	character PolsC2P*2
c
	data cols/1,7,2,5,3,4,6,8,9,10,11,12/
c
	call output(version)
	call keyini
	call keya('source',source,' ')
	if(source.eq.' ')call bug('f','A source must be given')
	call keya('stokes',stokes,'i')
	call keyr('xrange',xrange(1),4.7)
	call keyr('xrange',xrange(2),4.9)
	if(xrange(2).le.xrange(1).or.xrange(1).le.0)
     *	  call bug('f','Bad frequency range')
c
	npol = 0
	dowhile(stokes.ne.' '.and.npol.lt.MAXPOL)
	  npol = npol + 1
	  pol(npol) = PolsP2C(stokes)
	  call keya('stokes',stokes,' ')
	enddo
	if(keyprsnt('stokes'))
     *	  call bug('f','Too many Stokes parameters given')
	call keya('device',device,' ')
	call keyfin
c
c  Get information about the source.
c
	sdf = (xrange(2) - xrange(1)) / (NPTS-1)
	do i=1,NPTS
	  freq(i) = xrange(1) + sdf*(i-1)
	  x(i) = freq(i)
	enddo
c
	line = 'Stokes='
	length = len1(line)
	do i=1,npol
	  stokes = PolsC2P(pol(i))
	  line(length+1:) = stokes
	  length = len1(line) + 1
	  line(length:length) = ','
	  call lcase(stokes)
	  call CalStoke(source,stokes,freq,y(1,i),NPTS,ierr)
	  if(ierr.eq.2)call bug('f','Unknown source')
	  if(ierr.eq.1)call bug('w','Extrapolation being used')
	enddo
	line(length+2:) = 'Source='//source
	length = len1(line)
c
c  Determine the plot ranges.
c
	delta = 0.05*(xrange(2)-xrange(1))
	if(delta.le.1e-5*xrange(2)) delta = 0.1*xrange(2)
	xlo = xrange(1) - delta
	xhi = xrange(2) + delta
c
	ylo = y(1,1)
	yhi = ylo
	do j=1,npol
	  do i=1,NPTS
	    ylo = min(ylo,y(i,j))
	    yhi = max(yhi,y(i,j))
	  enddo
	enddo
	delta = 0.05*(yhi - ylo)
	maxv = max(abs(yhi),abs(ylo))
	if(delta.le.1e-5*maxv)delta = 0.01*maxv
	if(delta.eq.0) delta = 1
	ylo = ylo - delta
	yhi = yhi + delta
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
	  do i=1,npol
	    call pgsci(cols(mod(i-1,NCOL)+1))
	    call pgline(npts,x,y(1,i))
	  enddo
	  call pgsci(1)
	  call pglab('Frequency (GHz)','Flux (Jy)',line(1:length))
	  call pgend
	endif
	end
