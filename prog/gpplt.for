c************************************************************************
	program gpplt
	implicit none
c
c= GpPlt -- Plot and list gain and polarization correction terms.
c& rjs
c: calibration
c+
c	GpPlt is a MIRIAD task which plots and lists antenna gain and
c	polarization terms. The plot for gains is against time. The
c	plot for polarization characteristics is against antenna number.
c@ vis
c	The name of the input data-set. This will normally be a visibility
c	data-set. No default.
c@ device
c	The PGPLOT plotting device to use. The default is no plot.
c@ log
c	The log to give a listing of the gains and polarization terms. The
c	default is no log.
c@ yaxis
c	This specifies what is to be plotted/listed. Several values can be
c	given, separated by commas. Minimum match is used:
c	  amp	   Plot/list amplitudes. This is the default if the gains
c	           are being plotted/listed, and nothing else is requested.
c	  phase    Plot/list phases.
c	  real	   Plot/list the real parts. This is the default if the
c	           polarization terms are being plotted/listed, and nothing
c	           else is requested.
c	  imag     Plot/list the imaginary parts.
c	If nothing is given, "amp" is assumed.
c@ options
c	Thus gives some extra processing options. Several values can be
c	given, separated by commas. Minimum match us used.
c	  gains	       Plot/list the gains vs time. This is the default if
c		       nothing else is requested.
c	  xygains      Plot/list the ratio of X gain to Y gain.
c	  xbyygain     Plot/list the product of X gain by Y gain.
c	  polarization Plot/list the polarizations vs antenna number.
c	  delays       Plot/list the delays vs time.
c	  speccor      Plot/list the spectral correction vs time.
c	  bandpass     Plot/list the bandpass shape vs frequency.
c	  dots         Plot things as dots (rather than chunky circles).
c	  dtime	       Give time in days and fractions of a day. This is more
c	               useful for listing files which are to be passed into
c	               some other analysis or plotting tool.
c	  wrap         Don't unwrap phase plots
c@ nxy
c	Number of plots in the x and y directions. The default is 2,2.
c@ select
c	A subset of the normal uv-selection parameters. They are not
c	entirely consisently used. Antenna and time selection is supported
c	for gains. Time selection is supported for delay and spectral
c	correction. Antenna and frequency selection is supported for
c	bandpasses. The default is to select everything.
c@ yrange
c	The min and max range along the y axis of the plots. By default
c	gpplt autoscales. If the ``yrange'' parameter is given, then this
c	range is used on ALL plots. So it may not make much sense to
c	plot different sorts of quantities (e.g. amplitude and phases)
c	when explicitly giving the plot range.
c--
c  History:
c    rjs  16jul91 Original version.
c    rjs  24jul91 Changed autoscaling algorithm to avoid rounding
c		  problems.
c    nebk 13aug91 Ensure each feed starts plots on new page
c    rjs  19aug91 Added xbyy gains.
c    nebk 29aug91 Changed range to yrange
c    nebk 03sep91 Correct use of pgbegin to include error status.
c    rjs  05sep91 Do not plot flagged gains.
c    rjs  11sep91 Do not plot plots which have no points.
c    rjs  16sep91 Fixes for the above fix.
c    rjs  29nov91 Minor bug when listing polarisation leakages.
c    rjs  24apr92 PGPLOT standardisations.
c    rjs  29apr92 Appease nebk (and maintain my sanity) by adding options=dots
c    rjs   1may92 Added select.
c    rjs   9jul92 Added plotting of delays.
c    rjs  14jul92 Bastille day. Plot/list bandpass shapes.
c    rjs  23jul92 Changed way delays were handled.
c    nebk 22sep92 Add options=WRAP
c    rjs   5nov92 Plot the reciprocal of the bandpass correction, as this
c		  is what people are used to looking at.
c    mjs  13mar93 pgplot subr names have less than 7 chars.
c    rjs  23mar93 Corrected bug listing bandpass tables.
c    nebk 29jun93 Add 'O' to PGTBOX options string (omit leading zeros)
c    mchw 08jul93 changed maxGains and maxTimes to 2*MAXCHAN*MAXANT
c    rjs  09jul93 An extra message.
c    rjs  04aug93 Rename attenutation to spectral correction.
c    rjs  06aug93 Partial support for antenna selection.
c    rjs  18oct93 Include file name in titles.
c    nebk 24oct93 CHange nxy defaults to 3,2 with complicated algorithm
c    rjs/nebk 29mar94 Fix bugs in listing of bandpass table and improve
c		  formatting.
c  Bugs:
c------------------------------------------------------------------------
	integer MAXSELS
	character version*(*)
	parameter(MAXSELS=256)
	parameter(version='GpPlt: version 29-Mar-94')
	include 'gpplt.h'
	integer iostat,tIn,nx,ny,nfeeds,nants,nsols,ierr,symbol,nchan
	integer ntau
	character vis*64,device*64,logfile*64,BaseTime*20
	double precision T0
	logical doamp,dophase,doreal,doimag,dogains,dopol,dodtime,doxy
	logical doxbyy,doplot,dolog,more,ltemp,dodots,dodelay,dopass
	logical dospec,dowrap
	complex G1(maxGains),G2(maxGains)
	real alpha(maxGains)
	real times(maxTimes),range(2)
	character Feeds(3)*1
	real sels(MAXSELS)
        common /grrrrr/ dowrap
c
c  Externals.
c
	logical hdprsnt
	integer pgbeg
c
	data Feeds/'I','X','Y'/
c
c  Get the user parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input data-set must be given')
	call keya('device',device,' ')
	doplot = device.ne.' '
	call keya('log',logfile,' ')
	dolog = logfile.ne.' '
	if(.not.(dolog.or.doplot))
     *	  call bug('f','One of the device and log must be given')
	call GetAxis(doamp,dophase,doreal,doimag)
	call GetOpt(dogains,doxy,doxbyy,dopol,dodtime,dodots,
     *	  dodelay,dospec,dopass,dowrap)
	call keyi('nxy',nx,0)
	call keyi('nxy',ny,0)
        if(nx.ne.0.and.ny.eq.0)then
          ny = nx
        else if(nx.eq.0.and.ny.eq.0)then
          nx = 3
          ny = 2
        endif
	if(nx.le.0.or.ny.le.0)
     *	  call bug('f','Bad value for nxy')
	call SelInput('select',sels,MAXSELS)
	call keyr('yrange',range(1),0.)
	call keyr('yrange',range(2),range(1)-1.)
	call keyfin
c
c  Determine the plotting symbol.
c
	symbol = 17
	if(dodots) symbol = 1
c
c  Fill in the defaults.
c
	if(.not.(dogains.or.doxy.or.doxbyy.or.dopol.or.
     *		dodelay.or.dopass.or.dospec))dogains = .true.
	if(.not.(doamp.or.dophase.or.doreal.or.doimag))doamp = .true.
c
c  Open up all the inputs.
c
	call hopen(tIn,vis,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening input '//vis)
	  call bugno('f',iostat)
	endif
c
c  Check for the needed tables.
c
	if(dopass)then
	  dopass = hdprsnt(tIn,'bandpass')
	  if(.not.dopass)call bug('w','Bandpass function not present')
	endif
c
	if(dopol)then
	  dopol = hdprsnt(tIn,'leakage')
	  if(.not.dopol)call bug('w',
     *		'Polarization leakage table not present')
	endif
c
	if(dogains.or.doxy.or.doxbyy.or.dodelay.or.dospec)then
	  ltemp = hdprsnt(tIn,'gains')
	  if(ltemp) ltemp = hdprsnt(tIn,'ngains')
	  if(.not.ltemp)then
	    call bug('w',
     *		'Antenna gains information not present')
	    dogains = .false.
	    doxy = .false.
	    doxbyy = .false.
	    dodelay = .false.
	    dospec = .false.
	  else if(doxy.or.doxbyy)then
	    call rdhdi(tIn,'nfeeds',nfeeds,1)
	    doxy = nfeeds.eq.2.and.doxy
	    doxbyy = nfeeds.eq.2.and.doxbyy
	    if(nfeeds.ne.2)call bug('w',
     *		'Cannot compute XY gains from a single set of gains')
	  else if(dodelay.or.dospec)then
	    call rdhdi(tIn,'ntau',ntau,0)
	    if(ntau.eq.0)then
	      call bug('w',
     *		'Delays/spectral correction not present in gains')
	      dodelay = .false.
	      dospec = .false.
	    endif
	  endif
	endif
	if(.not.(dodelay.or.dospec.or.dopol.or.doxy.or.doxbyy.or.
     *	  dogains.or.dopass))
     *	  call bug('f','Requested options cannot be performed')
c
c  Open up the other devices now that we think everything looks OK.
c
	if(doplot)then
          ierr = pgbeg(0,device,nx,ny)
          if (ierr.ne.1)then
	    call pgldev
	    call bug ('f', 'Error in PGPLOT device')
	  endif
	  call pgsch(real(max(nx,ny))**0.4)
	endif
	if(dolog)then
	  call LogOpen(logfile,' ')
	  call LogWrite('# Gain/Polarization listing for '//vis,more)
	endif
c
c  Do the gain plots.
c
	if(dogains.or.doxy.or.doxbyy.or.dodelay.or.dospec)then
	  call GLoad(tIn,T0,times,G1,nfeeds,ntau,nants,nsols,sels,
     *	    maxGains,maxTimes)
	  if(.not.dodtime)call TScale(times,nsols)
	  call JulDay(T0,'H',BaseTime)
	  call output('The base time is '//BaseTime)
	  if(doLog)
     *	    call LogWrite('# The base time is '//BaseTime,more)
	  if(dogains)then
	    call GnCvt(G1,G2,nfeeds,ntau,nants*nsols)
	    call GainPlt(vis,times,G2,nfeeds,nants,nsols,range,
     *		Feeds(nfeeds),doamp,dophase,doreal,doimag,
     *		doplot,dolog,dodtime,symbol,nx*ny)
	  endif
	  if(doxy)then
	    call XYCvt(G1,G2,nfeeds,ntau,nants*nsols,.true.)
	    call GainPlt(vis,times,G2,1,nants,nsols,range,
     *		'XY',doamp,dophase,doreal,doimag,
     *		doplot,dolog,dodtime,symbol,nx*ny)
	  endif
	  if(doxbyy)then
	    call XYCvt(G1,G2,nfeeds,ntau,nants*nsols,.false.)
	    call GainPlt(vis,times,G2,1,nants,nsols,range,
     *		'X*Y',doamp,dophase,doreal,doimag,
     *		doplot,dolog,dodtime,symbol,nx*ny)
	  endif
	  if(dodelay)then
	    call AlphaCvt(G1,alpha,nfeeds,ntau,nants*nsols,.true.)
	    call AlphaPlt(vis,times,alpha,nants,nsols,range,
     *		'Delay (nsec)',
     *		doplot,dolog,dodtime,symbol,nx*ny)
	  endif
	  if(dospec)then
	    call AlphaCvt(G1,alpha,nfeeds,ntau,nants*nsols,.false.)
	    call AlphaPlt(vis,times,alpha,nants,nsols,range,
     *		'Spectral Correction',
     *		doplot,dolog,dodtime,symbol,nx*ny)
	  endif
	endif
c
c  Do the bandpass plots.
c
	if(dopass)then
	  call BLoad(tIn,times,G1,nfeeds,nants,nchan,sels,
     *		maxGains,maxTimes)
	  call BPPlt(times,G1,nfeeds,nants,nchan,range,
     *		Feeds(nfeeds),doamp,dophase,doreal,doimag,
     *		doplot,dolog,symbol,nx*ny)
	endif
c
c  Do the polarization leakage term plots.
c
	if(dopol)then
	  if(doLog)call LogWrite('# Polarization Information',more)
	  call PLoad(tIn,G1,nfeeds,nants,maxGains)
	  call PolPlt(G1,nfeeds,nants,range,Feeds(nfeeds),
     *		doamp,dophase,doreal,doimag,doplot,dolog,symbol)
	endif
c
c  Close up now.
c
	if(doplot)call pgend
	if(dolog) call logclose
	call hclose(tIn)
	end
c************************************************************************
	subroutine GLoad(tIn,T0,time,G,nfeeds,ntau,nants,nsols,sels,
     *	  maxGains,maxTimes)
c
	implicit none
	integer tIn,nfeeds,nants,ntau,nsols,maxGains,maxTimes
	complex G(maxGains)
	real time(maxTimes),sels(*)
	double precision T0
c
c  Load the antenna gains.
c
c  Input:
c    tIn
c    maxGains
c    maxTimes
c  Output:
c    T0		Base time, as a Julian date.
c    time	Offset Julian date.
c    G		The antenna gains.
c    nfeeds	Number of feeds (1 or 2).
c    ntau	Number of delay/spec corr terms (0 or 1).
c    nants	Number of antennae.
c    nsols	Number of solution intervals.
c-----------------------------------------------------------------------_
	integer item,iostat,offset,i,k,ngains
	double precision T
	logical doselect,select
c
c  Externals.
c
	integer hsize
	logical SelProbe
c
c  Determine the various parameters, and check their validity. We have pretty
c  well checked that all is OK before, so nothing should go wrong.
c
	doselect = SelProbe(sels,'time?',0.d0)
	call rdhdi(tIn,'nfeeds',nfeeds,1)
	call rdhdi(tIn,'ntau',ntau,0)
	call rdhdi(tIn,'ngains',ngains,1)
	call rdhdi(tIn,'nsols',nsols,1)
	if(nfeeds.le.0.or.ntau.lt.0.or.ngains.le.0.or.nsols.le.0)
     *	  call bug('f','Bad gain table size information')
	nants = ngains / (nfeeds + ntau)
	if(nants*(nfeeds+ntau).ne.ngains)
     *	  call bug('f','Number of gains does equal nants*(nfeeds+ntau)')
	if((nfeeds+ntau)*nants*nsols.gt.maxGains)call bug('f',
     *	  'Too many gains for me')
	if(nsols.gt.maxTimes)call bug('f',
     *	  'Too many solution intervals for me')
c
	call haccess(tIn,item,'gains','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error accessing the gains table')
	  call bugno('f',iostat)
	endif
c
c  Determine what we thing the number of solutions should be from the
c  size of the file.
c
	if(hsize(item).ne.8+(ngains+1)*8*nsols)
     *	  call bug('f','Gain table does not look the right size')
c
c  All is OK. Lets go for it.
c
	k = 0
	offset = 8
	do i=1,nsols
	  call hreadd(item,T,offset,8,iostat)
	  if(iostat.ne.0)call bugno('f',iostat)
	  offset = offset + 8
	  if(doselect)then
	    select = SelProbe(sels,'time',T)
	  else
	    select = .true.
	  endif
	  if(select)then
	    k = k + 1
	    if(k.eq.1) T0 = nint(T - 1.d0) + 0.5d0
	    time(k) = T - T0
	    call hreadr(item,G((k-1)*ngains+1),offset,8*ngains,iostat)
	    if(iostat.ne.0)call bugno('f',iostat)
	  endif
	  offset = offset + 8*ngains
	enddo
	if(k.eq.0)call bug('f','No gains selected')
	nsols = k
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Blank out the antenna gains that were not selected.
c
	if(SelProbe(sels,'antennae?',0.d0))
     *	  call AntGSel(sels,G,nfeeds,ntau,nants,nsols)
	end
c************************************************************************
	subroutine AntGSel(sels,G,nfeeds,ntau,nants,nsols)
c
	implicit none
	integer nfeeds,ntau,nants,nsols
	real sels(*)
	complex G(nfeeds+ntau,nants,nsols)
c
c  Blank out any antennas that were not selected.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k
	logical ant(MAXANT)
c
c  Externals.
c
	logical SelProbe
c
c  Determine which antennas were selected.
c
	do i=1,nants
	  ant(i) = SelProbe(sels,'antennae',257.d0*i)
	enddo
c
c  Now blank out the unwanted antennas.
c
	do k=1,nsols
	  do j=1,nants
	    if(.not.ant(j))then
	      do i=1,nfeeds
		G(i,j,k) = 0
	      enddo
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine BLoad(tIn,freq,Gains,nfeeds,nants,nchan,sels,
     *	  maxPass,maxfreq)
c
	implicit none
	integer tIn,nants,nchan,maxPass,maxfreq,nfeeds
	real freq(maxfreq),sels(*)
	complex Gains(maxPass)
c
c  Load the bandpass shapes.
c
c  Input:
c    tIn
c    maxPass	Max number of gains that can be handled.
c    maxfreq	Max number of frequencies that can be handled.
c  Output:
c    nants
c    nfeeds
c    nchan
c    freq
c    Gains
c------------------------------------------------------------------------
	include 'gpplt.h'
	integer ngains,nspect,item,iostat,n,off,nschan,i,j,k,offi,offo
	integer ntau
	double precision freqs(2)
	logical doselect,select(maxtimes)
c
c  Externals.
c
	logical selprobe
c
	call rdhdi(tIn,'nfeeds',nfeeds,1)
	call rdhdi(tIn,'ngains',ngains,1)
	call rdhdi(tIn,'ntau',ntau,0)
	call rdhdi(tIn,'nchan0',nchan,0)
	call rdhdi(tIn,'nspect0',nspect,0)
	if(nfeeds.le.0.or.ngains.le.0)
     *	  call bug('f','Bad gain table size information')
	nants = ngains / (nfeeds+ntau)
	if(nants*(nfeeds+ntau).ne.ngains)
     *	  call bug('f','Number of gains does equal nants*nfeeds')
	if(nchan.gt.min(maxfreq,maxtimes).or.nchan.le.0)call bug('f',
     *	  'Bad number of frequencies')
	if(nspect.le.0.or.nspect.gt.nchan)call bug('f',
     *	  'Bad number of frequency spectral windows')
	if(nfeeds*nants*nchan.gt.maxPass)call bug('f',
     *	  'Too many gains for me')
c
	doselect = SelProbe(sels,'frequency?',0.d0)
c
c  Read the frequency table.
c
	call haccess(tIn,item,'freqs','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error accessing the bandpass frequency table')
	  call bugno('f',iostat)
	endif
c
	n = 0
	off = 8
	do i=1,nspect
	  call hreadi(item,nschan,off,4,iostat)
	  off = off + 8
	  if(iostat.eq.0)call hreadd(item,freqs,off,2*8,iostat)
	  off = off + 2*8
	  if(iostat.ne.0)then
	    call bug('w','Error reading bandpass frequency table')
	    call bugno('f',iostat)
	  endif
	  do j=1,nschan
	    n = n + 1
	    freq(n) = freqs(1) + (j-1)*freqs(2)
	    select(n) = .not.doselect.or.
     *			SelProbe(sels,'frequency',dble(freq(n)))
	  enddo
	enddo
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Read the bandpass table now.
c
	call haccess(tIn,item,'bandpass','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error accessing the bandpass table')
	  call bugno('f',iostat)
	endif
c
	off = 8
	call hreadr(item,Gains,off,8*nants*nfeeds*nchan,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error reading the bandpass table')
	  call bugno('f',iostat)
	endif
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Perform frequency selection, if needed.
c
	if(doselect)then
	  offo = 0
	  offi = 0
	  do k=1,nants
	    do j=1,nfeeds
	      do i=1,nchan
		offi = offi + 1
	        if(select(i))then
		  offo = offo + 1
		  Gains(offo) = Gains(offi)
	        endif
	      enddo
	    enddo
	  enddo
c
	  offo = 0
	  do j=1,nchan
	    if(select(j))then
	      offo = offo + 1
	      freq(offo) = freq(j)
	    endif
	  enddo
	  nchan = offo
	  if(nchan.eq.0)call bug('f','No channels selected')
	endif
c
c  Blank out the unwanted antennas.
c
	if(SelProbe(sels,'antennae?',0.d0))
     *	  call AntBSel(sels,Gains,nchan*nfeeds,nants)
c	  
	end
c************************************************************************
	subroutine AntBSel(sels,G,n,nants)
c
	implicit none
	integer n,nants
	real sels(*)
	complex G(n,nants)
c
c  Blank out any antennas that were not selected.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	logical ant(MAXANT)
c
c  Externals.
c
	logical SelProbe
c
c  Determine which antennas were selected.
c
	do i=1,nants
	  ant(i) = SelProbe(sels,'antennae',257.d0*i)
	enddo
c
c  Now blank out the unwanted antennas.
c
	do j=1,nants
	  if(.not.ant(j))then
	    do i=1,n
	      G(i,j) = 0
	    enddo
	  endif
	enddo
c
	end
c************************************************************************
	subroutine PLoad(tIn,Leaks,nfeeds,nants,maxLeaks)
c
	implicit none
	integer tIn,nfeeds,nants,maxLeaks
	complex Leaks(2,maxLeaks)
c
c  Load the polarisation leakage table.
c
c------------------------------------------------------------------------	
	integer item,iostat
c
c  Externals.
c
	integer hsize
c
	call haccess(tIn,item,'leakage','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error accessing the leakage table')
	  call bugno('f',iostat)
	endif
c
c  Determine the number of antennae.
c
	nfeeds = 2
	nants = (hsize(item)-8)/16
	if(nants.le.0.or.nants.gt.maxLeaks)
     *	  call bug('f','Illegal number of leakage parameters')
c
c  Now read them in.
c
	call hreadr(item,Leaks,8,8*nants*nfeeds,iostat)
	if(iostat.ne.0)then
	  call bug('w','I/O error while reading the leakage tabele')
	  call bugno('f',iostat)
	endif
c
c  And close up.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	end
c************************************************************************
	subroutine AlphaPlt(vis,time,alpha,nants,nsols,range,
     *			ylabel,
     *			doplot,dolog,dodtime,symbol,ppp)
c
	implicit none
	integer nants,nsols,symbol,ppp
	real time(nsols),range(2)
	real alpha(nants*nsols)
	character ylabel*(*),vis*(*)
	logical doplot,dolog,dodtime
c
c  Do the plot or the listing.
c
c  Inputs:
c------------------------------------------------------------------------
	include 'gpplt.h'
	character line*80,Title*48
	logical more
	real y(maxTimes)
	integer iday,ihr,imin,isec,i,j,j1,offset,k,length
c
c  Externals.
c
	character itoaf*3
	integer len1
c
c  Do the plots.
c
	if(doplot)then
	  do j=1,nants
	    call AlPick(alpha(j),nants,nsols,y)
	    call SetPG(time(1),time(nsols),y,nsols,range,dodtime)
	    call pgpt(nsols,time,y,symbol)
	    Title = 'Antenna '//itoaf(j)//'File='//vis
	    length = len1(title)
	    call pglab('Time',ylabel,Title(1:length))
	  enddo
	  call subfill(nants,ppp)
	endif
c
c  Write the needed data to the listing file.
c
	if(dolog)then
	  offset = 0
	  do k=1,nsols
	    if(k.eq.1)then
	      line = '# Listing of '//ylabel
	      call LogWrite(line,more)
	      line = '# Number of antennae: '//itoaf(nants)
	      call LogWrite(line,more)
	    endif
	    do j=1,nants,6
	      j1 = min(j+5,nants)
	      if(j.eq.1)then
		if(dodtime)then
		  write(line(1:12),'(f11.6)')time(k)
		else
		  isec = nint(time(k))
		  iday = isec/(24*3600)
		  isec = isec - 24*3600*iday
		  ihr  = isec/3600
		  isec = isec - 3600*ihr
		  imin = isec / 60
		  isec = isec - 60*imin
		  write(line(1:12),'(i2,a,i2.2,a,i2.2,a,i2.2)')
     *			iday,' ',ihr,':',imin,':',isec
		endif
	      else
		line(1:12) = ' '
	      endif
	      write(line(13:80),'(6g11.3)')
     *			(alpha(i),i=offset+j,offset+j1)
	      call LogWrite(line,more)
	    enddo
	    offset = offset + nants
	  enddo
	endif
	end
c************************************************************************
	subroutine AlPick(alpha,nants,nsols,y)
c
	implicit none
	integer nants,nsols
	real alpha(nants,nsols),y(nsols)
c------------------------------------------------------------------------
	integer i
	do i=1,nsols
	  y(i) = alpha(1,i)
	enddo
	end
c************************************************************************
	subroutine PolPlt(Leaks,nfeeds,nants,range,Feeds,
     *		doamp,dophase,doreal,doimag,doplot,dolog,symbol)
c
	implicit none
	integer nants,nfeeds,symbol
	logical doamp,dophase,doreal,doimag,doplot,dolog
	complex Leaks(nfeeds*nants)
	character Feeds(nfeeds)*(*)
	real range(2)
c
c------------------------------------------------------------------------
c

c  Externals.
c
	external GetAmp,GetPhase,GetReal,GetImag
c
	if(doamp)  call PolPlt2(Leaks,nfeeds,nants,range,Feeds,
     *	  doplot,dolog,symbol,GetAmp)
	if(dophase)call PolPlt2(Leaks,nfeeds,nants,range,Feeds,
     *	  doplot,dolog,symbol,GetPhase)
	if(doreal) call PolPlt2(Leaks,nfeeds,nants,range,Feeds,
     *	  doplot,dolog,symbol,GetReal)
	if(doimag) call PolPlt2(Leaks,nfeeds,nants,range,Feeds,
     *	  doplot,dolog,symbol,GetImag)
	end
c************************************************************************
	subroutine PolPlt2(Leaks,nfeeds,nants,range,Feeds,
     *	  doplot,dolog,symbol,GetVal)
c
	implicit none
	integer nfeeds,nants,symbol
	complex Leaks(nfeeds*nants)
	logical doplot,dolog
	character Feeds(nfeeds)*(*)
	real range(2)
	external GetVal
c
c------------------------------------------------------------------------
	include 'gpplt.h'
	real x(2*maxant),y(2*maxant)
	integer i,j,j1,k,l
	logical more
	character line*80,type*8,Label*16
c
c  Externals.
c
	character itoaf*3
	integer len1
c
	if(doplot)then
	  do i=1,nants
	    x(i) = i
	  enddo
	  do k=1,nfeeds
	    call GetVal(Leaks(k),nfeeds,nants,y,type)
	    call SetPG(1.,real(nants),y,nants,range,.true.)
	    call pgpt(nants,x,y,symbol)
	    Label = Feeds(k)//'-Leakage-'//type
	    call pglab('Antenna Number',Label,' ')
	  enddo
	endif
c
	if(dolog)then
	  call GetVal(Leaks,1,nfeeds*nants,y,type)
	  l = len1(type)
	  write(line,10)type(1:l),(Feeds(j),j=1,nfeeds)
   10	  format('# Listing of the ',a,
     *		' of the leakages for feeds ',4(a,:,','))
	  call LogWrite(line,more)
	  line = '# Number of antennae: '//itoaf(nants)
	  call LogWrite(line,more)
	  do j=1,nfeeds*nants,6
	    j1 = min(j+5,nfeeds*nants)
	    write(line,'(7g11.3)')(y(i),i=j,j1)
	    call LogWrite(line,more)
	  enddo
	endif
	end
c************************************************************************
	subroutine TScale(time,nsols)
c
	implicit none
	integer nsols
	real time(nsols)
c
c  Scale the times to seconds.
c
c  Input:
c    nsols	Number of times.
c  Input/Output:
c    time	The times. On input, these are in fractions of a day.
c		On output these are the seconds in a day.
c------------------------------------------------------------------------
	integer i
	real scale
	parameter(scale=24.0*3600.0)
c
	do i=1,nsols
	  time(i) = scale * time(i)
	enddo
	end
c************************************************************************
	subroutine GainPlt(vis,time,G,nfeeds,nants,nsols,range,
     *	  Feeds,doamp,dophase,doreal,doimag,doplot,dolog,
     *    dodtime,symbol,ppp)
c
	implicit none
	integer nfeeds,nants,nsols,ppp,symbol
	complex G(nfeeds*nants*nsols)
	real time(nsols),range(2)
	logical doamp,dophase,doreal,doimag,doplot,dolog,dodtime
	character Feeds(nfeeds)*(*),vis*(*)
c
c  Plot/list the antenna gains.
c
c  Input:
c    nfeeds	Number of polarization feeds.
c    nants	Number of antennae.
c    nsols	Number of solutions.
c    time	The offset time of each solution.
c    G		The gains
c    range	Range along Y axis for plots.
c    Feeds	Used to form labels and descriptions.
c    doamp,dophase,doreal,doimag If true, the do the corresponding
c		plot/listing.
c    doplot,dolog If true, do a plot or write the table.
c    symbol	Plotting symbol.
c    dodtime	Give time in fractions of a day.
c    ppp	Plots per page.
c    vis	File name.
c------------------------------------------------------------------------
c
c  Externals.
c
	external GetAmp,GetPhase,GetReal,GetImag
c
	if(doamp)  call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  Feeds,doplot,dolog,dodtime,symbol,GetAmp,ppp)
	if(dophase)call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  Feeds,doplot,dolog,dodtime,symbol,GetPhase,ppp)
	if(doreal) call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  Feeds,doplot,dolog,dodtime,symbol,GetReal,ppp)
	if(doimag) call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  Feeds,doplot,dolog,dodtime,symbol,GetImag,ppp)
	end
c************************************************************************
	subroutine BpPlt(freq,G,nfeeds,nants,nchan,range,
     *	  Feeds,doamp,dophase,doreal,doimag,doplot,dolog,
     *    symbol,ppp)
c
	implicit none
	integer nfeeds,nants,nchan,ppp,symbol
	complex G(nchan*nfeeds*nants)
	real freq(nchan),range(2)
	logical doamp,dophase,doreal,doimag,doplot,dolog
	character Feeds(nfeeds)*(*)
c
c  Plot/list the bandpass shape.
c
c  Input:
c    nfeeds	Number of polarization feeds.
c    nants	Number of antennae.
c    nchan	Number of channels.
c    freq	The offset time of each solution.
c    G		The gains
c    range	Range along Y axis for plots.
c    Feeds	Used to form labels and descriptions.
c    doamp,dophase,doreal,doimag If true, the do the corresponding
c		plot/listing.
c    doplot,dolog If true, do a plot or write the table.
c    symbol	Plotting symbol.
c    ppp	Plots per page.
c------------------------------------------------------------------------
c
c  Externals.
c
	external GetAmp,GetPhase,GetReal,GetImag
c
	if(doamp)  call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  Feeds,doplot,dolog,symbol,GetAmp,ppp)
	if(dophase)call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  Feeds,doplot,dolog,symbol,GetPhase,ppp)
	if(doreal) call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  Feeds,doplot,dolog,symbol,GetReal,ppp)
	if(doimag) call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  Feeds,doplot,dolog,symbol,GetImag,ppp)
	end
c************************************************************************
	subroutine GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  Feeds,doplot,dolog,dodtime,symbol,GetVal,ppp)
c
	implicit none
	integer nfeeds,nants,nsols,ppp,symbol
	real time(nsols),range(2)
	complex G(nfeeds*nants*nsols)
	logical doplot,dolog,dodtime
	character Feeds(nfeeds)*(*),vis*(*)
	external GetVal
c
c  Do the plot or the listing.
c
c  Inputs:
c	Similar to GainPlt, except ...
c	GetVal	Routine used to convert to the desired quantity.
c------------------------------------------------------------------------
	include 'gpplt.h'
	character line*80,type*8,Label*20,Title*48
	logical more
	real x(maxTimes),y(maxTimes)
	complex gains(maxTimes)
	integer iday,ihr,imin,isec,i,j,j1,k,l,offset,nres,ng,length
c
c  Externals.
c
	character itoaf*3
	integer len1
c
c  Do the plots.
c
	if(doplot)then
	  do k=1,nfeeds
	    nres = 0
	    do j=1,nants
	      offset = k + (j-1)*nfeeds
	      call GnPick(time,G(offset),nfeeds*nants,nsols,x,Gains,ng)
	      if(ng.gt.0)then
	        call GetVal(Gains,1,ng,y,type)
		call SetPG(time(1),time(nsols),y,ng,range,dodtime)
		call pgpt(ng,x,y,symbol)
	        Label = Feeds(k)//'-Gain-'//type
	        Title = 'Antenna '//itoaf(j)//'File='//vis
		length = len1(title)
	        call pglab('Time',Label,Title(1:length))
		nres = nres + 1
	      endif
	    enddo
	    call subfill(nres,ppp)
	  enddo
	endif
c
c  Write the needed data to the listing file.
c
	if(dolog)then
	  offset = 1
	  do k=1,nsols
	    call GetVal(G(offset),1,nfeeds*nants,y,type)
	    if(k.eq.1)then
	      l = len1(type)
	      write(line,10)type(1:l),(Feeds(j),j=1,nfeeds)
   10	      format('# Listing of the ',a,
     *		' of the gains for ',4(a,:,','))
	      call LogWrite(line,more)
	      line = '# Number of antennae: '//itoaf(nants)
	      call LogWrite(line,more)
	    endif
	    do j=1,nfeeds*nants,6
	      j1 = min(j+5,nfeeds*nants)
	      if(j.eq.1)then
		if(dodtime)then
		  write(line(1:12),'(f11.6)')time(k)
		else
		  isec = nint(time(k))
		  iday = isec/(24*3600)
		  isec = isec - 24*3600*iday
		  ihr  = isec/3600
		  isec = isec - 3600*ihr
		  imin = isec / 60
		  isec = isec - 60*imin
		  write(line(1:12),'(i2,a,i2.2,a,i2.2,a,i2.2)')
     *			iday,' ',ihr,':',imin,':',isec
		endif
	      else
		line(1:12) = ' '
	      endif
	      write(line(13:80),'(6g11.3)')(y(i),i=j,j1)
	      call LogWrite(line,more)
	    enddo
	    offset = offset + nfeeds*nants
	  enddo
	endif
	end
c************************************************************************
	subroutine BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  Feeds,doplot,dolog,symbol,GetVal,ppp)
c
	implicit none
	integer nfeeds,nants,nchan,ppp,symbol
	real freq(nchan),range(2)
	complex G(nchan*nfeeds*nants)
	logical doplot,dolog
	character Feeds(nfeeds)*(*)
	external GetVal
c
c  Do the plot or the listing.
c
c  Inputs:
c	Similar to BpPlt, except ...
c	GetVal	Routine used to convert to the desired quantity.
c------------------------------------------------------------------------
	include 'gpplt.h'
	character line*80,type*8,Label*20,Title*12
	logical more
	real x(maxTimes),y(maxTimes),y1(maxTimes),freqmin,freqmax
	complex gains(maxTimes)
	integer i,j,j1,k,offset,nres,ng
c
c  Externals.
c
	character itoaf*3
c
c  Do the plots.
c
	if(doplot)then
c
c  Determine the min and max frequencies.
c
	  freqmin = freq(1)
	  freqmax = freqmin
	  do j=1,nchan
	    freqmin = min(freqmin,freq(j))
	    freqmax = max(freqmax,freq(j))
	  enddo
c
	  do k=1,nfeeds
	    nres = 0
	    do j=1,nants
	      offset = nchan*( (k-1) + (j-1)*nfeeds) + 1
	      call GnPick(freq,G(offset),1,nchan,x,Gains,ng)
	      if(ng.gt.0)then
	        call GnRec(Gains,ng)
	        call GetVal(Gains,1,ng,y,type)
		call SetPG(freqmin,freqmax,y,ng,range,.true.)
		call pgpt(ng,x,y,symbol)
	        Label = Feeds(k)//'-BandPass-'//type
	        Title = 'Antenna '//itoaf(j)
	        call pglab('Frequency (GHz)',Label,Title)
		nres = nres + 1
	      endif
	    enddo
	    call subfill(nres,ppp)
	  enddo
	endif
c
c  Write the needed data to the listing file.
c
	if(dolog)then
	  offset = 1
	  do k=1,nchan
	    call GetVal(G(offset),nfeeds*nchan,nants,y,type)
	    if(nfeeds.gt.1)call GetVal(G(offset+nchan),nfeeds*nchan,
     *						nants,y1,type)
	    if(k.eq.1)call BpTitle(type,Feeds,nfeeds,nants)
	    do j=1,nfeeds*nants,6
	      j1 = min(j+5,nfeeds*nants)
	      if(j.eq.1)then
		write(line(1:10),'(f10.5)')freq(k)
	      else
		line(1:10) = ' '
	      endif
	      if(nfeeds.eq.1)then
	        write(line(11:80),'(6f10.5))')(y(i),i=j,j1)
	      else
		write(line(11:80),'(6f10.5)')
     *			(y(i),y1(i),i=(j-1)/2+1,(j1-1)/2+1)
	      endif
	      call LogWrite(line,more)
	    enddo
	    offset = offset + 1
	  enddo
	endif
	end
c************************************************************************
	subroutine BpTitle(type,Feeds,nfeeds,nants)
c
	implicit none
	integer nfeeds,nants
	character type*(*),Feeds(nfeeds)*(*)
c
c  Generate the title for the Bandpass listing file.
c------------------------------------------------------------------------
	integer length,j,j1,j2,ant,feed
	logical more
	character line*80
c
c  Externals.
c
	character itoaf*2
	integer len1
c
	length = len1(type)
	write(line,10)type(1:length),(Feeds(j),j=1,nfeeds)
   10	format('# Listing of the ',a,
     *		' of the bandpass for ',4(a,:,','))
	call LogWrite(line,more)
	line = '# Number of antennae: '//itoaf(nants)
	call LogWrite(line,more)
	do j1=1,nants*nfeeds,6
	  if(j1.eq.1)then
	    line = '# Freq(GHz)'
	  else
	    line = '#'
	  endif
	  length = 16
	  j2 = min(j1+5,nants*nfeeds)
	  do j=j1,j2
	    ant = (j-1)/nfeeds + 1
	    feed = j - (ant-1)*nfeeds
	    line(length+1:) = Feeds(feed)//itoaf(ant)
	    length = length + 10
	  enddo
	  call logwrite(line(1:length),more)
	enddo
	end
c************************************************************************
	subroutine GnRec(Gains,n)
c
	implicit none
	integer n
	complex Gains(n)
c
c  Take the reciprocal of some gains. All the gains will be non-zero
c
c  Input:
c    n		Number of gains
c  Input/Output:
c    Gains	Antenna gains.
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  if(abs(real(Gains(i)))+abs(aimag(Gains(i))).gt.0)
     *					 Gains(i) = 1/Gains(i)
	enddo
c
	end
c************************************************************************
	subroutine GnPick(tin,Gin,n1,n2,tout,Gout,nout)
c
	implicit none
	integer n1,n2,nout
	real tin(n2),tout(n2)
	complex Gin(n1,n2),Gout(n2)
c
c  This removes the bad gains from the table of gains to plot.
c
c  Input:
c    n1		Number of gains per time step.
c    n2		Number of input time steps.
c    tin	Input times.
c    Gin	Input gains.
c  Output:
c    tout	Output times.
c    Gout	Output gains.
c    nout	Number of output time steps.
c------------------------------------------------------------------------
	integer i
	complex g
c
	nout = 0
	do i=1,n2
	  g = Gin(1,i)
	  if(abs(real(g))+abs(aimag(g)).gt.0)then
	    nout = nout + 1
	    tout(nout) = tin(i)
	    Gout(nout) = g
	  endif
	enddo
c
	end
c************************************************************************
	subroutine SetPG(xmin,xmax,y,n,range,dodtime)
c
	implicit none
	integer n
	real xmin,xmax,y(*),range(2)
	logical dodtime
c
c  Determine the range of the plots, and call the appropriate PGPLOT
c  routines to set this up.

c  Inputs:
c    xmin,xmax	Range of the X data to be plotted.
c    y		The Y data to be plotted.
c    n		Number of points in Y.
c------------------------------------------------------------------------
	real xlo,xhi,ylo,yhi,delta,maxv
	integer i
c
	delta = 0.05*(xmax-xmin)
	if(delta.le.0)delta = 1
	xlo = xmin - delta
	xhi = xmax + delta
c
	if(range(2).gt.range(1))then
	  yhi = range(2)
	  ylo = range(1)
	else if(n.eq.0)then
	  ylo = -1
	  yhi =  1
	else
	  yhi = y(1)
	  ylo = yhi
	  do i=2,n
	    yhi = max(yhi,y(i))
	    ylo = min(ylo,y(i))
	  enddo
c
	  delta = 0.05*(yhi-ylo)
	  maxv = max(abs(ylo),abs(yhi))
	  if(delta.le.1e-4*maxv) delta = 0.01*maxv
	  if(delta.eq.0) delta = 1
	  ylo = ylo - delta
	  yhi = yhi + delta
	endif
c
	call pgpage
	call pgvstd
	call pgswin(xlo,xhi,ylo,yhi)
	if(dodtime)then
	  call pgtbox('BCNST',0.,0,'BCNST',0.,0)
	else
	  call pgtbox('BCNSTHZO',0.,0,'BCNST',0.,0)
	endif
	end
c************************************************************************
	subroutine GetAmp(G,n1,n2,Out,type)
c
	implicit none
	integer n1,n2
	complex G(n1,n2)
	real Out(n2)
	character type*(*)
c
c  Get the amplitude of a complex value.
c------------------------------------------------------------------------
	integer i
c
	type = 'Amp'
c
	do i=1,n2
	  Out(i) = abs(G(1,i))
	enddo
	end
c************************************************************************
	subroutine GetPhase(G,n1,n2,Out,type)
c
	implicit none
        logical dowrap
	integer n1,n2
	complex G(n1,n2)
	real Out(n2)
	character type*(*)
        common /grrrrr/ dowrap
c
c  Get the phase of a complex value.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
	real theta0,theta
	complex ctemp
	logical first
c
	type = 'Phase'
c
	first = .true.
	do i=1,n2
	  ctemp = G(1,i)
	  if(abs(real(ctemp))+abs(aimag(ctemp)).eq.0)then
	    theta = 0
c
c  Get the phase, and do some simple phase unwrapping.
c
	  else
	    theta = 180/pi * atan2(aimag(ctemp),real(ctemp))
            if (.not.dowrap) then
  	      if(first)then
	        theta0 = theta
	        first = .false.
	      else
	        theta = theta - 360*nint((theta-theta0)/360.)
	        theta0 = 0.5*(theta + theta0)
              endif
	    endif
	  endif
c
	  out(i) = theta
	enddo
	end
c************************************************************************
	subroutine GetReal(G,n1,n2,Out,type)
c
	implicit none
	integer n1,n2
	complex G(n1,n2)
	real Out(n2)
	character type*(*)
c
c  Get the real part of a complex value.
c------------------------------------------------------------------------
	integer i
c
	type = 'Real'
c
	do i=1,n2
	  Out(i) = real(G(1,i))
	enddo
	end
c************************************************************************
	subroutine GetImag(G,n1,n2,Out,type)
c
	implicit none
	integer n1,n2
	complex G(n1,n2)
	real Out(n2)
	character type*(*)
c
c  Get the imaginary of a complex value.
c------------------------------------------------------------------------
	integer i
c
	type = 'Imag'
c
	do i=1,n2
	  Out(i) = aimag(G(1,i))
	enddo
	end
c************************************************************************
	subroutine GnCvt(In,Out,nfeeds,ntau,ngains)
c
	implicit none
	integer nfeeds,ntau,ngains
	complex In(nfeeds+ntau,ngains),Out(nfeeds,ngains)
c
c  Pick out the true gains.
c
c------------------------------------------------------------------------
	integer i,j
c
	do j=1,ngains
	  do i=1,nfeeds
	    Out(i,j) = In(i,j)
	  enddo
	enddo
	end
c************************************************************************
	subroutine AlphaCvt(In,Out,nfeeds,ntau,ngains,doimag)
c
	implicit none
	integer nfeeds,ntau,ngains
	logical doimag
	complex In((nfeeds+ntau)*ngains)
	real Out(ngains)
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,j
c
	i = 1 + nfeeds
	do j=1,ngains
	  if(doimag)then
	    Out(j) = aimag(In(i)) / (2*pi)
	  else
	    Out(j) = real(In(i))
	  endif
	  i = i + nfeeds + ntau
	enddo
	end
c************************************************************************
	subroutine XYCvt(In,Out,nfeeds,ntau,ngains,doxy)
c
	implicit none
	integer nfeeds,ntau,ngains
	logical doxy
	complex In((nfeeds+ntau)*ngains),Out(ngains)
c
c  This divides or multiplies the X gains by the Y gains, to get the XY gains.
c
c------------------------------------------------------------------------
	integer i,j
	real temp
c
	i = 1
	do j=1,ngains
	  temp = abs(real(In(i+1)))+abs(aimag(In(i+1)))
	  if(temp.le.0)then
	    Out(j) = 0
	  else if(doxy)then
	    Out(j) = In(i)/In(i+1)
	  else
	    Out(j) = In(i)*conjg(In(i+1))
	  endif
	  i = i + nfeeds + ntau
	enddo
	end
c************************************************************************
	subroutine GetOpt(dogains,doxy,doxbyy,dopol,dodtime,dodots,
     *		          dodelay,dospec,dopass,dowrap)
c
	implicit none
	logical dogains,dopol,dodtime,doxy,doxbyy,dodots,dodelay
	logical dospec,dopass,dowrap
c
c  Get extra processing options.
c
c  Output:
c    dogains	If true, process the gains.
c    doxy	If true, process the ratio of X/Y gains.
c    doxbyy	If true, process the product of X*Y gains
c    dopol	If true, process the polarizations.
c    dodtime	If true, give time as day fractions.
c    dodots	If true, plot small dots (rather than big circles).
c    dodelay	If true, process the delays table.
c    dopass	If true, process the bandpass table.
c    dowrap     If true, don't unwrap phases
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=10)
	logical present(nopt)
	character opts(nopt)*12
c
	data opts/'gains       ','polarization','dtime       ',
     *		  'xygains     ','xbyygains   ','dots        ',
     *		  'delays      ','bandpass    ','speccor     ',
     *            'wrap        '/
c
	call options('options',opts,present,nopt)
	dogains = present(1)
	dopol   = present(2)
	dodtime = present(3)
	doxy    = present(4)
	doxbyy  = present(5)
	dodots  = present(6)
	dodelay = present(7)
	dopass  = present(8)
	dospec  = present(9)
        dowrap  = present(10)
	end
c************************************************************************
	subroutine GetAxis(doamp,dophase,doreal,doimag)
c
	implicit none
	logical doamp,dophase,doreal,doimag
c
c  Determine the things to plot.
c
c  Output:
c    doamp	Plot the amplitude.
c    dophase	Plot the phase.
c    doreal	Plot the real part.
c    doimag	Plot the imaginary part.
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=4)
	logical present(nopt)
	character opts(nopt)*9
c
	data opts/'ampltiude','phase    ','real     ','imaginary'/
c
	call options('yaxis',opts,present,nopt)
	doamp   = present(1)
	dophase = present(2)
	doreal  = present(3)
	doimag  = present(4)
	end
c************************************************************************
      subroutine subfill(nres,ppp)
c
      implicit none
      integer nres,ppp
c
c     Skip through some blank sub-plots
c------------------------------------------------------------------------
      integer i,n
c
      n = mod( ppp-mod(nres,ppp), ppp)
      do i = 1, n
        call pgpage
      end do
c
      end
