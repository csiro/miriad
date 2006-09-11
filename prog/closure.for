c************************************************************************
	program closure
	implicit none
c
c= closure
c& rjs
c: uv analysis, plotting
c+
c	CLOSURE is a Miriad task which plots some closure quantities.
c	The these include the closure phase (the "triple phase"), the
c	closure amplitude (the "quad amplitude"), and two less well known
c	quantities -- the triple amplitude and quad phase.
c
c	These are gefined as
c
c	  Closure phase     (triple phase):   arg( V12*V23*conjg(V13) )
c	  Closure amplitude (quad amplitude): abs( (V12*V34)/(V14*conjg(V34)) )
c	  Triple amplitude:                   abs( V12*V23*conjg(V13) )**0.3333
c	  Quad phase:                         arg( (V12*V34)/(V14*conjg(V34)) )
c
c	The closure phase, quad phase and closure amplitude should be
c	independent of antenna-based errors, and for a point source should
c	have values of 	zero phase or unit amplitude. The triple amplitude is
c	independent of antenna-based phase errors (but not amplitude errors),
c	and for a point source is a measure of the flux density.
c	
c	The task works by averaging the quantites that are the argument
c	of the abs or arg function.
c	These are always averaged over the selected frequency channels and
c	over the parallel-hand polarizations. Optionally the averaging
c	can also be done over time and over the different closure paths.
c
c	CLOSURE also prints (and optionally plots) the theoretical error
c	(as a result of thermal noise) of the quantities.
c	Note this assumes a point source model, and that the
c	signal-to-noise ratio is appreciable (at least 10) in each averaged
c	product.
c
c@ vis
c	The input visibility datasets. Several datasets can be given.
c@ select
c	Standard visibility selection. See help on "select" for more
c	information.
c@ line
c	Standard visibility linetype. See the help "line" for more information.
c@ stokes
c	Normal Stokes/polaization selection. The default is to process all
c	parallel-hand polarisations.
c@ device
c	PGPLOT plotting device. The default is no plotting device (the
c	program merely prints out some statistics).
c@ nxy
c	The number of plots per page in x and y. The default depends on
c	the number of plots.
c@ yrange
c	The y range of the plots. The default is to autoscale.
c@ interval
c	Time averaging interval. The default is no time averaging of the
c	triple correlations.
c@ options
c	Task enrichment parameters. Several parameters can be given, separated
c	by commas. Minimum match is supported. Possible options are:
c	  amplitude Plot the amplitude quantity (the default is to plot phase).
c	  quad      Plot the quad quantity (the default is to plot the
c	            triple quantity).
c	  avall     Average all quantities from different triangles
c	            together. Note the theoretical error estimates are
c	            incorrect when using options=avall.
c	  notriple  Plot data from all quantities on a single plot.
c	  rms       Plot theoretical error bars on the points. The error
c	            bars are +/- sigma.
c	The following give control over calibration to be applied to the
c	visibility data before the triple correlations are formed. Note
c	that applying phase calibrations does not affect the closure phase!
c	  nocal     Do not perform gain calibration.
c	  nopol     Do not perform polarisation calibration on the data.
c	  nopass    Do not perform bandpass calibration on the data.
c--
c  History:
c    rjs   7sep94 Original version.
c    rjs  14sep95 Bring it up to scratch.
c    rjs  19sep95 Reset the valid flag on a baseline after an integration.
c    rjs   9nov95 Time axis was mislabelled by 1 integration.
c    pjt  20jun96 Larger MAXPLOTS for BIMA (20 -> 90)
c    rjs  29jul97 Added quad quantities.
c    rjs  11aug97 Minor fiddles to make it more robust.
c    mchw 20may98 Larger MAXPLOTS for 10-antennas (90 -> 120)
c    rjs  20oct00 Print out number of points when giving stats.
c    rjs  31jan01 Support other stokes types.
c    rjs  08apr02 Allow negative values when taking cube roots.
c    rjs  18jul04 Check for division by zero in quad quantities.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer MAXPNTS,MAXPLOTS,MAXTRIP
	integer PolMin,PolMax,MAXPOL
	character version*(*)
	parameter(version='version 18-Jul-04')
	parameter(MAXPNTS=5000,MAXPLOTS=120)
	parameter(MAXTRIP=(MAXANT*(MAXANT-1)*(MAXANT-2))/6)
	parameter(PolMin=-8,PolMax=4,MAXPOL=2)
c
	logical avall,notrip,doamp,doerr,quad
	character uvflags*16,device*64
	real interval,yrange(2)
	integer nx,ny,nread,i,j,mpnts,mplots,tno,pnt1,pnt2
	integer npol,polcvt(PolMin:PolMax),p,ant1,ant2,nants,bl
	double precision preamble(4),time0,t,tmin,tmax,tprev
	logical first,more,doii
	complex data(MAXCHAN)
	logical flag(MAXCHAN)
c
c  Plot buffers.
c
	character title(MAXPLOTS)*16
	real x(MAXPNTS*MAXPLOTS),y(MAXPNTS*MAXPLOTS)
	real yerr(MAXPNTS*MAXPLOTS)
	integer npnts(MAXPLOTS),nplots
c
c  Accumulation buffers.
c
	complex trip(MAXTRIP)
	integer tripplot(MAXTRIP),ntrip(MAXTRIP)
	double precision triptime(MAXTRIP)
	real tripsig2(MAXTRIP)
c
c  Integration buffers.
c
	integer corrpnt(MAXBASE,MAXPOL),flagpnt(MAXBASE,MAXPOL)
	integer nchan(MAXBASE,MAXPOL)
	logical init(MAXBASE,MAXPOL)
	real sigma2(MAXBASE,MAXPOL)
c
c  Externals.
c
	logical uvDatOpn
c
c Lets go! Get user inputs.
c
	call output('Closure: '//version)
	call keyini
	call GetOpt(avall,notrip,doamp,doerr,quad,uvflags)
	notrip = notrip.or.avall
c
	call uvDatInp('vis',uvflags)
	call uvDatGti('npol',npol)
	doii = npol.eq.0
	call keya('device',device,' ')
	call keyi('nxy',nx,0)
	call keyi('nxy',ny,nx)
	call keyr('interval',interval,0.)
	call keyr('yrange',yrange(1),0.)
	call keyr('yrange',yrange(2),0.)
	call keyfin
c
c  Initialise the integration buffers.
c
	do j=1,MAXPOL
	  do i=1,MAXBASE
	    nchan(i,j) = 0
	    init(i,j) = .false.
	  enddo
	enddo
c
c  Initialise the accumulation buffers.
c
	do i=1,MAXTRIP
	  tripplot(i) = 0
	  ntrip(i) = 0
	enddo
c
c  Initialise the plot buffer.
c
	if(notrip)then
	  mpnts = MAXPNTS*MAXPLOTS
	  mplots = 1
	else
	  mpnts = MAXPNTS
	  mplots = MAXPLOTS
	endif
	nplots = 0
	do i=1,mplots
	  npnts(i) = 0
	enddo
c
c Miscellaneous initialisation.
c
	interval = max(interval,0.01)
	interval = interval / (24.*60.)
	first = .true.
c
	nants = 0
	npol = 0
	do i=PolMin,PolMax
	  polcvt(i) = 0
	enddo
c
	more = uvDatOpn(tno)
	dowhile(more)
	  call uvDatRd(preamble,data,flag,MAXCHAN,nread)
	  if(nread.eq.0)then
	    call uvDatCls
	    more = uvDatOpn(tno)
	  else
c
c  Get baseline and polarisation information.
c
	    call basant(preamble(4),ant1,ant2)
	    bl = (ant2-1)*(ant2-2)/2 + ant1
	    p = 0
	    if(min(ant1,ant2).ge.1.and.max(ant1,ant2).le.MAXANT.and.
     *	        ant1.ne.ant2)
     *		call PolIdx(p,npol,polcvt,PolMin,PolMax,MAXPOL,doii)
c
c  Handle time information, and do fiddles for the first time through.
c
	    if(p.gt.0)then	
	      if(first)then
	        time0 = int(preamble(3) - 0.5d0) + 0.5d0
	        tmin = preamble(3) - time0
	        tmax = tmin
	        tprev = tmin
		first = .false.
	      endif
	      t = preamble(3) - time0
c
c  Flush the integration buffers if needed.
c
	      if(abs(t-tprev).gt.1./(3600.*24.))then
		call IntFlush(nants,npol,tprev,quad,avall,init,
     *		  memc,corrpnt,
     *		  meml,flagpnt,nchan,sigma2,MAXBASE,MAXPOL,
     *		  ntrip,trip,triptime,tripsig2,MAXTRIP)
		tprev = t
	      endif
c
c  Flush the accumulation buffers if needed.
c
	      if(t-tmin.gt.interval.or.tmax-t.gt.interval)then
		call AccFlush(nants,notrip,quad,avall,doamp,
     *		  ntrip,trip,triptime,tripsig2,tripplot,maxtrip,
     *		  x,y,yerr,npnts,nplots,title,mpnts,mplots)
		tmin = t
		tmax = tmin
		nants = 0
	      endif
c
c  Save this baseline.
c
	      tmin = min(tmin,t)
	      tmax = max(tmax,t)
	      nants = max(nants,ant1,ant2)
c
	      if(nchan(bl,p).ne.nread)then
		if(nchan(bl,p).gt.0)then
		  call MemFree(corrpnt(bl,p),nchan(bl,p),'c')
		  call MemFree(flagpnt(bl,p),nchan(bl,p),'l')
		endif
		nchan(bl,p) = nread
		call MemAlloc(corrpnt(bl,p),nchan(bl,p),'c')
		call MemAlloc(flagpnt(bl,p),nchan(bl,p),'l')
	      endif
c
	      pnt1 = corrpnt(bl,p) - 1
	      pnt2 = flagpnt(bl,p) - 1
	      do i=1,nchan(bl,p)
		memc(pnt1+i) = data(i)
		meml(pnt2+i) = flag(i)
	      enddo
	      call uvDatGtr('variance',sigma2(bl,p))
	      init(bl,p) = .true.
c
	    endif
	  endif
	enddo
c
c  Flush the integration and accumulation buffers.
c
	if(nants.gt.0)then
	  call IntFlush(nants,npol,t,quad,avall,init,
     *	    memc,corrpnt,
     *	    meml,flagpnt,nchan,sigma2,MAXBASE,MAXPOL,
     *	    ntrip,trip,triptime,tripsig2,MAXTRIP)
	  call AccFlush(nants,notrip,quad,avall,doamp,
     *	    ntrip,trip,triptime,tripsig2,tripplot,maxtrip,
     *	    x,y,yerr,npnts,nplots,title,mpnts,mplots)
	endif
c
c  Do what we are really here for.
c
	if(nplots.eq.0)call bug('f','No data was found to plot')
	if(avall)then
	  call bug('w',
     *	   'Theoretical error estimates are incorrect (too optimistic)')
	  call bug('w',
     *	   '    when using options=avall')
	endif
	call Plotit(doamp,doerr,device,nx,ny,yrange,
     *			x,y,yerr,npnts,nplots,title,mpnts)
c
	end
c************************************************************************
	subroutine AccFlush(nants,notrip,quad,avall,doamp,
     *	  ntrip,trip,triptime,tripsig2,tripplot,maxtrip,
     *	  x,y,yerr,npnts,nplots,title,maxpnts,maxplots)
c
	implicit none
	integer nants,maxpnts,maxplots,maxtrip
	logical notrip,avall,doamp,quad
	integer ntrip(maxtrip),tripplot(maxtrip)
	complex trip(maxtrip)
	double precision triptime(maxtrip)
	real tripsig2(maxtrip)
	character title(maxplots)*(*)
	real x(maxpnts,maxplots),y(maxpnts,maxplots)
	real yerr(maxpnts,maxplots)
	integer npnts(maxplots),nplots
c
c  Flush the accumulated triple products to the plot buffers.
c------------------------------------------------------------------------
	integer i1,i2,i3,i4,i4lo,i4hi,nantsd,p,k
c
	nantsd = nants
	if(avall)         nantsd = 3
	if(avall.and.quad)nantsd = 4
	if(quad)then
	  i4lo = 4
	  i4hi = nantsd
	else
	  i4lo = nantsd+1
	  i4hi = nantsd+1
	endif
c
	k = 0
	do i4=i4lo,i4hi
	do i3=3,i4-1
	  do i2=2,i3-1
	    do i1=1,i2-1
	      k = k + 1
	      if(ntrip(k).gt.0)then
c
c  Associate this triple with a plot, if needed.
c
		if(tripplot(k).eq.0)then
		  if(.not.notrip.or.nplots.eq.0)then
		    nplots = nplots + 1
		    if(nplots.gt.maxplots)call bug('f','Too many plots')
		    call triplab(notrip,quad,i1,i2,i3,i4,title(nplots))
		  endif
		  tripplot(k) = nplots
		endif
c
c  Add this to the plot buffer.
c
		p = tripplot(k)
		npnts(p) = npnts(p) + 1
		if(npnts(p).gt.maxpnts)call bug('f','Too many points')
		call Tripcalc(doamp,quad,
     *			 ntrip(k),triptime(k),trip(k),tripsig2(k),
     *			 x(npnts(p),p),y(npnts(p),p),yerr(npnts(p),p))
	        ntrip(k) = 0
	      endif
	    enddo
	  enddo
	enddo
	enddo
c
	end
c************************************************************************
	subroutine triplab(notrip,quad,i1,i2,i3,i4,title)
c
	implicit none
	logical notrip,quad
	integer i1,i2,i3,i4
	character title*(*)
c------------------------------------------------------------------------
	integer l1,l2,l3,l4
	character n1*8,n2*8,n3*8,n4*8
c
	character itoaf*8
	integer len1
	if(notrip)then
	  title = 'All antennas'
	else
	  n1 = itoaf(i1)
	  l1 = len1(n1)
	  n2 = itoaf(i2)
	  l2 = len1(n2)
	  n3 = itoaf(i3)
	  l3 = len1(n3)
	  n4 = itoaf(i4)
	  l4 = len1(n4)
	  if(quad)then
	    title = 'Antennas '//n1(1:l1)//'-'//n2(1:l2)//
     *			    '-'//n3(1:l3)//'-'//n4(1:l4)
	  else
	    title = 'Antennas '//n1(1:l1)//'-'//n2(1:l2)//'-'//n3(1:l3)
	  endif
	endif
	end
c************************************************************************
	subroutine Tripcalc(doamp,quad,n,time,trip,sigma2,x,y,yerr)
c
	implicit none
	logical doamp,quad
	integer n
	double precision time
	complex trip
	real sigma2,x,y,yerr
c------------------------------------------------------------------------
	include 'mirconst.h'
	real amp
c
	x = 24*3600*time/n
c
	amp = abs(trip/n)
	if(quad)then
	  if(doamp)then
	    y = amp
	    yerr = sqrt(sigma2)/n
	  else if(amp.eq.0)then
	    y = 0
	    yerr = 0
	  else
	    y = 180/pi * atan2(aimag(trip),real(trip))
	    yerr = 180/pi * sqrt(sigma2)/n
	  endif
	else
	  amp = abs(trip/n)
	  if(doamp)then
	    y = real(trip/n)
	    y = sign(abs(y)**0.333333,y)
c	    y = amp ** 0.333333
	    yerr = sqrt(sigma2)/(3*n)
	  else if(amp.eq.0)then
	    y = 0
	    yerr = 0
	  else
	    y = 180/pi * atan2(aimag(trip),real(trip))
	    yerr = 180/pi * sqrt(sigma2)/n/(amp ** 0.333333)
	  endif
	endif
c
	end
c************************************************************************
	subroutine IntFlush(nants,npol,time,quad,avall,
     *	  init,Corrs,CorrPnt,Flags,FlagPnt,nchan,sigma2,maxbase,maxpol,
     *	  ntrip,trip,triptime,tripsig2,maxtrip)
c
	implicit none
	integer nants,npol,maxbase,maxpol,maxtrip
	double precision time,triptime(maxtrip)
	integer CorrPnt(maxbase,maxpol),FlagPnt(maxbase,maxpol)
	integer ntrip(maxtrip),nchan(maxbase,maxpol)
	logical quad,avall,init(maxbase,maxpol),Flags(*)
	real sigma2(maxbase,maxpol),tripsig2(maxtrip)
	complex Corrs(*),trip(maxtrip)
c------------------------------------------------------------------------
	real flux
	integer p,i4,i3,i2,i1,bl12,bl13,bl23,bl14,bl34,k,i,nread
	integer pflag12,pflag13,pflag23,pdata12,pdata23,pdata13
	integer pflag14,pflag34,        pdata14,pdata34
	complex denom
c
	do p=1,npol
	  if(avall)then
	    k = 1
	  else
	    k = 0
	  endif
c
c  Quad quantity.
c
	  if(quad)then
	    do i4=4,nants
	    do i3=3,i4-1
	      do i2=2,i3-1
	        do i1=1,i2-1
	 	  if(.not.avall)k = k + 1
	          bl12 = ((i2-1)*(i2-2))/2 + i1
	          bl34 = ((i4-1)*(i4-2))/2 + i3
	          bl14 = ((i4-1)*(i4-2))/2 + i1
	          bl23 = ((i3-1)*(i3-2))/2 + i2
		  if(init(bl12,p).and.init(bl34,p).and.init(bl14,p).and.
     *		     init(bl23,p))then
		    if(ntrip(k).eq.0)then
		      triptime(k) = 0
		      tripsig2(k) = 0
	 	      trip(k) = 0
		    endif
		    nread = min(nchan(bl12,p),nchan(bl34,p),
     *				nchan(bl14,p),nchan(bl23,p))
		    pdata12 = corrpnt(bl12,p) - 1
		    pdata34 = corrpnt(bl34,p) - 1
		    pdata14 = corrpnt(bl14,p) - 1
		    pdata23 = corrpnt(bl23,p) - 1
		    pflag12 = flagpnt(bl12,p) - 1
		    pflag34 = flagpnt(bl34,p) - 1
		    pflag14 = flagpnt(bl14,p) - 1
		    pflag23 = flagpnt(bl23,p) - 1
		    do i=1,nread
		      if(Flags(pflag12+i).and.Flags(pflag34+i).and.
     *		         Flags(pflag14+i).and.Flags(pflag23+i))then
			flux = 0.25*(abs(Corrs(pdata12+i)) + 
     *				     abs(Corrs(pdata34+i)) +
     *				     abs(Corrs(pdata14+i)) +
     *				     abs(Corrs(pdata23+i)))
			denom = Corrs(pdata14+i)*conjg(Corrs(pdata23+i))
			if(abs(real(denom))+abs(aimag(denom)).eq.0.or.
     *			   abs(flux).eq.0)call bug('f',
     *		  'Flux quantity identically zero when doing division')
		        trip(k) = trip(k) +
     *			  (Corrs(pdata12+i) *       Corrs(pdata34+i))/
     *			  denom
		        tripsig2(k) = tripsig2(k) + 
     *			  (sigma2(bl12,p) + sigma2(bl34,p) +
     *			   sigma2(bl14,p) + sigma2(bl23,p))/(flux*flux)
		        triptime(k) = triptime(k) + time
		        ntrip(k) = ntrip(k) + 1
		      endif
		    enddo
		  endif
	        enddo
	      enddo
	      enddo
	    enddo
c
c  Triple quantity.
c
	  else
	    do i3=3,nants
	      do i2=2,i3-1
	        do i1=1,i2-1
	 	  if(.not.avall)k = k + 1
	          bl12 = ((i2-1)*(i2-2))/2 + i1
	          bl13 = ((i3-1)*(i3-2))/2 + i1
	          bl23 = ((i3-1)*(i3-2))/2 + i2
		  if(init(bl12,p).and.init(bl13,p).and.init(bl23,p))then
		    if(ntrip(k).eq.0)then
		      triptime(k) = 0
		      tripsig2(k) = 0
	 	      trip(k) = 0
		    endif
		    nread = min(nchan(bl12,p),nchan(bl13,p),
     *				nchan(bl23,p))
		    pdata12 = corrpnt(bl12,p) - 1
		    pdata13 = corrpnt(bl13,p) - 1
		    pdata23 = corrpnt(bl23,p) - 1
		    pflag12 = flagpnt(bl12,p) - 1
		    pflag13 = flagpnt(bl13,p) - 1
		    pflag23 = flagpnt(bl23,p) - 1
		    do i=1,nread
		      if(Flags(pflag12+i).and.Flags(pflag13+i).and.
     *		         Flags(pflag23+i))then
		        trip(k) = trip(k) + Corrs(pdata12+i)
     *					* Corrs(pdata23+i)
     *				  * conjg(Corrs(pdata13+i))
		        tripsig2(k) = tripsig2(k) + sigma2(bl12,p)
     *						+ sigma2(bl23,p)
     *						+ sigma2(bl13,p)
		        triptime(k) = triptime(k) + time
		        ntrip(k) = ntrip(k) + 1
		      endif
		    enddo
		  endif
	        enddo
	      enddo
	    enddo
	  endif
c
c  Reset the baseline.
c
	  do i=1,(nants*(nants-1))/2
	    init(i,p) = .false.
	  enddo
	enddo
	end
c************************************************************************
	subroutine GetOpt(avall,notrip,doamp,doerr,quad,uvflags)
c
	implicit none
	logical avall,notrip,doamp,doerr,quad
	character uvflags*(*)
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=8)
	character opts(NOPTS)*9
	logical present(NOPTS)
	data opts/'avall    ','notriple ','amplitude','rms      ',
     *		  'nocal    ','nopol    ','nopass   ','quad     '/
c
	call options('options',opts,present,NOPTS)
	avall = present(1)
	notrip = present(2)
	doamp  = present(3)
	doerr  = present(4)
	quad   = present(8)
c
c c -- docal
c f -- dopass
c e -- dopol
c
	uvflags = 'dslx'
	if(.not.present(5))uvflags(5:5) = 'c'
	if(.not.present(6))uvflags(6:6) = 'e'
	if(.not.present(7))uvflags(7:7) = 'f'
	end
c************************************************************************
	subroutine PolIdx(p,npol,polcvt,PolMin,PolMax,MAXPOL,doii)
c
	implicit none
	integer p,PolMin,PolMax,npol,MAXPOL
	integer polcvt(PolMin:PolMax)
	logical doii
c------------------------------------------------------------------------
	integer pol
c
c  Externals.
c
	logical polspara
c
	p = 0
	call uvDatGti('pol',pol)
	if(pol.lt.PolMin.or.pol.gt.PolMax)return
	if(polcvt(pol).eq.0)then
	  if(.not.doii.or.PolsPara(pol))then
	    npol = npol + 1
	    if(npol.gt.MAXPOL)call bug('f','Too many polarisations')
	    polcvt(pol) = npol
	  endif
	endif
	p = polcvt(pol)
	end
c************************************************************************
	subroutine Plotit(doamp,doerr,device,nx,ny,yrange,
     *			x,y,yerr,npnts,nplots,title,maxpnts)
c
	implicit none
	logical doamp,doerr
	character device*(*)
	real yrange(2)
	integer nplots,maxpnts,nx,ny
	integer npnts(nplots)
	real x(maxpnts,nplots),y(maxpnts,nplots),yerr(maxpnts,nplots)
	character title(nplots)*(*)
c
c  Make the closure phase or triple amplitude plots.
c------------------------------------------------------------------------
	real xmin,xmax,ymin,ymax,t
	double precision yrms,yerrav,yav
	integer i,j,jd,n,nxd,nyd,symbol
	character line*80
c
c  Externals.
c
	integer pgbeg
	character itoaf*8
c
c  Determine some statistics.
c
	xmin = x(1,1)
	xmax = xmin
	ymin = y(1,1)
	ymax = ymin
	yrms = 0
	yav = 0
	yerrav = 0
c
	n = 0
	do j=1,nplots
	  do i=1,npnts(j)
	    xmin = min(xmin,x(i,j))
	    xmax = max(xmax,x(i,j))
	    ymin = min(ymin,y(i,j))
	    ymax = max(ymax,y(i,j))
	    yrms = yrms + y(i,j)**2
	    yav  = yav  + y(i,j)
	    yerrav = yerrav + yerr(i,j)
	    n = n + 1
	  enddo
	enddo
	yav    = yav/n
	yrms   = sqrt(yrms/n)
	yerrav = yerrav/n
c
	if(doamp)then
	  write(line,'(a,f9.4)')
     *	    'Mean amplitude:',yav
	  call output(line)
	  yrms = sqrt(abs(yrms**2 - yav**2))
	  write(line,'(a,f8.4)')
     *	    'Actual amplitude rms scatter:     ',yrms
	  call output(line)
	  write(line,'(a,f8.4)')
     *	    'Theoretical amplitude rms scatter:',yerrav
	  call output(line)
	else
	  write(line,'(a,f8.3)')
     *	    'Actual closure phase rms value (degrees):       ',yrms
	  call output(line)
	  write(line,'(a,f8.3)')
     *	    'Theoretical closure phase rms scatter (degrees):',yerrav
	  call output(line)
	endif
	call output(
     *	'Number of points:                                  '//itoaf(n))
c
	if(device.eq.' ')return
c
	call RelAxis(xmin,xmax)
	call RelAxis(ymin,ymax)
	if(nx.ne.0.and.ny.ne.0)then
	  nxd = nx
	  nyd = ny
	else if(nplots.eq.1)then
	  nxd = 1
	  nyd = 1
	else if(nplots.le.4)then
	  nxd = 2
	  nyd = 2
	else
	  nxd = 3
	  nyd = 2
	endif
c
	if(yrange(2).gt.yrange(1))then
	  ymin = yrange(1)
	  ymax = yrange(2)
	endif
c
	if(pgbeg(0,device,nxd,nyd).ne.1)then
	  call pgldev
	  call bug('f','Failed to open plot device')
	endif
	call pgscf(2)
	call pgsch(real(max(nxd,nyd))**0.4)
	call pgvstd
	symbol = 1
	if(n.lt.80*nplots)symbol = 17
c
	jd = 0
	do j=1,nplots
	  call sorter(jd,title,nplots)
	  call pgpage
	  call pgswin(xmin,xmax,ymin,ymax)
	  call pgtbox('BCNSTHZO',0.,0.,'BCNST',0.,0.)
	  if(doamp)then
	    call pglab('Time','Amplitude',title(jd))
	  else
	    call pglab('Time','Closure Phase (degrees)',title(jd))
	  endif
	  if(.not.doerr.or.symbol.ne.1)
     *		call pgpt(npnts(jd),x(1,jd),y(1,jd),symbol)
	  if(doerr)then
	    do i=1,npnts(jd)
	      t = y(i,jd)
	      y(i,jd)    = t + yerr(i,jd)
	      yerr(i,jd) = t - yerr(i,jd)
	    enddo
	    call pgerry(npnts(jd),x(1,jd),y(1,jd),yerr(1,jd),1.)
	  endif
	enddo
	call pgend
c
	end
c************************************************************************
	subroutine sorter(jd,title,nplots)
c
	implicit none
	integer jd,nplots
	character title(nplots)*(*)
c------------------------------------------------------------------------
	integer k,jdd
c
	if(jd.eq.0)then
	  jd = 1
	  do k=2,nplots
	    if(title(k).lt.title(jd))jd = k
	  enddo
	else
	  jdd = 0
	  do k=1,nplots
	    if(title(k).gt.title(jd))then
	      if(jdd.eq.0)then
		jdd = k
	      else if(title(k).lt.title(jdd))then
		jdd = k
	      endif
	    endif
	  enddo
	  jd = jdd
	endif
	end
c************************************************************************
	subroutine RelAxis(lo,hi)
c
	implicit none
	real lo,hi
c------------------------------------------------------------------------
	real delta,maxv
	delta = 0.05*(hi-lo)
	maxv  = max(abs(hi),abs(lo))
	if(delta.le.1e-4*maxv) delta = 0.01*maxv
	if(delta.eq.0)delta = 1
	lo = lo - delta
	hi = hi + delta
	end
