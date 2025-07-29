c************************************************************************
	program contsen
	implicit none
c
c= contsen - Continuum subtraction noise sensitivity.
c& rjs
c: uv analysis
c+
c	CONTSEN makes plots of noise amplification factor of tasks such
c	as UVLIN, AVMATHS and CONTSUB.
c@ chans
c	This specifices the channel ranges that contain only continuum
c	(line free). It consists of a number of paris, each pair giving
c	a start and end channel. The default is that there are 512
c	continuum channels.
c@ xrange
c	The range of channels to plot the noise sensitivity. The default
c	is the min anx max channels given by the chans keyword.
c@ order
c	The order of the polynomial fit in UVLIN. Either 1 or 2 numbers
c	can be given. If two numbers are given, then CONTSEN determines
c	the noise sensitivity for order(1) to order(2) inclusive. The
c	default is to determine the noise sensitivity for first order
c	only.
c@ device
c	PGPLOT device. No default.
c@ nxy
c	The plot grid. The default depends on the number of plots.
c--
c  History:
c    rjs 22oct93 Original version.
c    rjs 10mar94 Added xrange keyword.
c    rjs 04dec95 Increase max order.
c    rjs 18sep05 Change to eliminate inconsistent type flaw.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='ContSen: version 1.0 10-Mar-94')
	include 'maxdim.h'
	integer MAXORDER,MAXCH
	parameter(MAXORDER=12,MAXCH=32)
	integer symbol
	parameter(symbol=1)
c
	double precision sum(0:2*MAXORDER),a(0:MAXORDER,0:MAXORDER)
	double precision x(MAXCHAN),b(0:MAXORDER,0:MAXORDER)
	double precision f(0:MAXORDER),r(MAXCHAN),t
	logical cont(MAXCHAN)
	integer chans(2,MAXCH),mnchan,mxchan
	integer i,j,order(2),ifail,nch,imin,imax,nx,ny,nplots
	integer iorder,npts,pivot(MAXORDER+1)
c
	character device*64
	real xv(MAXCHAN),yv(MAXCHAN),xmin,xmax,ymin,ymax,delta,maxv
c
c  Externals.
c
	integer pgbeg
	character itoaf*3
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call keya('device',device,' ')
	call mkeyi('chans',chans,2*MAXCHAN,nch)
	call keyi('nxy',nx,0)
	call keyi('nxy',ny,nx)
	call keyi('order',order(1),1)
	call keyi('order',order(2),order(1))
	call keyi('xrange',mnchan,1)
	call keyi('xrange',mxchan,0)
	call keyfin
c
c  Check the inputs.
c
	if(device.eq.' ')call bug('f','PGPLOT device must be given')
	if(order(1).lt.0.or.order(2).lt.order(1).or.
     *	   order(2).gt.MAXORDER)call bug('f','Invalid order parameter')
	if(mod(nch,2).ne.0)
     *	  call bug('f','Incomplete channel ranges given')
	nch = nch / 2
c
c  Determine the array giving continuum only channels.
c
	do i=1,MAXCHAN
	  cont(i) = .false.
	enddo
	if(nch.gt.0)then
	  imin = chans(1,1)
	  imax = chans(2,1)
	  do j=1,nch
	    if(chans(1,j).lt.1.or.chans(2,j).gt.MAXCHAN.or.
     *	       chans(1,j).gt.chans(2,j))
     *		call bug('f','Invalid channel range')
	    do i=chans(1,j),chans(2,j)
	      cont(i) = .true.
	    enddo
	    imin = min(imin,chans(1,j))
	    imax = max(imax,chans(2,j))
	  enddo
	else
	  if(mxchan.gt.mnchan)then
	    imin = mnchan
	    imax = mxchan
	  else
	    imin = 1
	    imax = min(512,MAXCHAN)
	  endif
	  do i=imin,imax
	    cont(i) = .true.
	  enddo
	endif
c
	if(mnchan.ge.mxchan)then
	  mxchan = imax
	  mnchan = imin
	endif
c
c  Determine the plot grid.
c
	nplots = order(2) - order(1) + 1
	if(nx.eq.0.or.ny.eq.0)then
	  if(nplots.eq.1)then
	    nx = 1
	    ny = 1
	  else if(nplots.le.4)then
	    nx = 2
	    ny = 2
	  else if(nplots.le.6)then
	    nx = 3
	    ny = 2
	  else
	    nx = 3
	    ny = 3
	  endif
	endif
c
c  Initialise the PGPLOT device.
c
	if(pgbeg(0,device,nx,ny).ne.1)then
	  call pgldev
	  call bug('f','Error opening PGPLOT device')
	endif
	call pgsch(real(max(nx,ny))**0.4)
c
c  Generate the noise sensitivity plots.
c
	do iorder=order(1),order(2)
	  npts = 0
	  do i=imin,imax
	    if(cont(i))then
	      npts = npts + 1
	      x(npts) = i
	      r(npts) = i
	    endif
	  enddo
	  if(npts.eq.0)call bug('f','No channels selected')
c
	  sum(0) = npts
	  do j=1,2*iorder
	    t = 0
	    do i=1,npts
	      t = t + r(i)
	      r(i) = r(i) * x(i)
	    enddo
	    sum(j) = t
	  enddo
c
	  do j=0,iorder
	    do i=0,iorder
	      a(i,j) = sum(i+j)
	      b(i,j) = 0
	    enddo
	    b(j,j) = 1
	  enddo
c
	  call dgefa(a,MAXORDER+1,iorder+1,pivot,ifail)
	  if(ifail.ne.0)call bug('f','Inversion failed')
	  do i=0,iorder
	    call dgesl(a,MAXORDER+1,iorder+1,pivot,b(0,i),1)
	  enddo
c
	  do i=mnchan,mxchan
	    xv(i) = i
	    call GetNoise(i,b,iorder,MAXORDER,f,cont(i),yv(i))
	  enddo
c
c  Determine scaling factors.
c
	  xmin = mnchan
	  xmax = mxchan
	  ymin = yv(mnchan)
	  ymax = ymin
	  do i=mnchan,mxchan
	    ymin = min(ymin,yv(i))
	    ymax = max(ymax,yv(i))
	  enddo
c
	  delta = 0.05*(xmax-xmin)
	  if(delta.le.0)delta = 1
	  xmax = xmax + delta
	  xmin = xmin - delta
c
	  delta = 0.05*(ymax-ymin)
	  maxv = max(abs(ymax),abs(ymin))
	  if(delta.le.1e-4*maxv) delta = 0.01*maxv
	  if(delta.eq.0)delta = 1
	  ymax = ymax + delta
	  ymin = ymin - delta
c
	  call pgpage
	  call pgvstd
	  call pgswin(xmin,xmax,ymin,ymax)
	  call pgtbox('BCNST',0.,0,'BCNST',0.,0)
c
	  call pgpt(mxchan-mnchan+1,xv(mnchan),yv(mnchan),symbol)
	  call pglab('Channel Number','Noise Amplification',
     *	    'Order='//itoaf(iorder))
	enddo
c
	call pgend
c
	end
c************************************************************************
	subroutine GetNoise(n,b,order,MAXORDER,f,cont,x)
c
	implicit none
	integer n,order,MAXORDER
	double precision b(0:MAXORDER,0:MAXORDER),f(0:MAXORDER)
	logical cont
	real x
c
c------------------------------------------------------------------------
	integer i,j
c
	do i=0,order
	  f(i) = b(i,order)
	  do j=order-1,0,-1
	    f(i) = f(i)*n + b(i,j)
	  enddo
	enddo
c
	x = f(order)
	do j=order-1,0,-1
	  x = x*n + f(j)
	enddo
c
	if(cont)then
	  x = sqrt(1-x)
	else
	  x = sqrt(1+x)
	endif
c
	end
