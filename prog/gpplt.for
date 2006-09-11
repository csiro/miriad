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
c    rjs  25apr95 Significant rework. Unwrap phases consistently.
c    rjs  17aug95 Failed to take reciprocal of bandpass.
c    rjs  19sep95 Fix write statement in printing polarisation sol'ns.
c    rjs  12dec96 Fix bug in GainPlt2 related to getting times stamps and
c		  flagging wrong.
c  Bugs:
c------------------------------------------------------------------------
	integer MAXSELS
	character version*(*)
	parameter(MAXSELS=256)
	parameter(version='GpPlt: version 12-Dec-96')
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
     *		Feeds(nfeeds),doamp,dophase,dowrap,doreal,doimag,
     *		doplot,dolog,dodtime,symbol,nx*ny)
	  endif
	  if(doxy)then
	    call XYCvt(G1,G2,nfeeds,ntau,nants*nsols,.true.)
	    call GainPlt(vis,times,G2,1,nants,nsols,range,
     *		'XY',doamp,dophase,dowrap,doreal,doimag,
     *		doplot,dolog,dodtime,symbol,nx*ny)
	  endif
	  if(doxbyy)then
	    call XYCvt(G1,G2,nfeeds,ntau,nants*nsols,.false.)
	    call GainPlt(vis,times,G2,1,nants,nsols,range,
     *		'X*Y',doamp,dophase,dowrap,doreal,doimag,
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
     *		Feeds(nfeeds),doamp,dophase,dowrap,doreal,doimag,
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
c    nants	Number of antennas.
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
c  Take the reciprocal of the gains.
c
	offi = 0
	do k=1,nants
	  do j=1,nfeeds
	    do i=1,nchan
	      offi = offi + 1
	      if(abs(real(Gains(offi)))+abs(aimag(Gains(offi))).gt.0)
     *		Gains(offi) = 1/Gains(offi)
	    enddo
	  enddo
	enddo
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
c  Determine the number of antennas.
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
	      line = '# Number of antennas: '//itoaf(nants)
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
	real GetAmp,GetPhasW,GetReal,GetImag
	external GetAmp,GetPhasW,GetReal,GetImag
c
	if(doamp)  call PolPlt2(Leaks,nfeeds,nants,range,'Amp',Feeds,
     *	  doplot,dolog,symbol,GetAmp)
	if(dophase)call PolPlt2(Leaks,nfeeds,nants,range,'Phase',Feeds,
     *	  doplot,dolog,symbol,GetPhasW)
	if(doreal) call PolPlt2(Leaks,nfeeds,nants,range,'Real',Feeds,
     *	  doplot,dolog,symbol,GetReal)
	if(doimag) call PolPlt2(Leaks,nfeeds,nants,range,'Imag',Feeds,
     *	  doplot,dolog,symbol,GetImag)
	end
c************************************************************************
	subroutine GainPlt(vis,time,G,nfeeds,nants,nsols,range,
     *	  Feeds,doamp,dophase,dowrap,doreal,doimag,doplot,dolog,
     *    dodtime,symbol,ppp)
c
	implicit none
	integer nfeeds,nants,nsols,ppp,symbol
	complex G(nfeeds*nants*nsols)
	real time(nsols),range(2)
	logical doamp,dophase,dowrap,doreal,doimag,doplot,dolog,dodtime
	character Feeds(nfeeds)*(*),vis*(*)
c
c  Plot/list the antenna gains.
c
c  Input:
c    nfeeds	Number of polarization feeds.
c    nants	Number of antennas.
c    nsols	Number of solutions.
c    time	The offset time of each solution.
c    G		The gains
c    range	Range along Y axis for plots.
c    Feeds	Used to form labels and descriptions.
c    doamp,dophase,doreal,doimag If true, the do the corresponding
c		plot/listing.
c    dowrap	Do not attempt to unwrap the phases.
c    doplot,dolog If true, do a plot or write the table.
c    symbol	Plotting symbol.
c    dodtime	Give time in fractions of a day.
c    ppp	Plots per page.
c    vis	File name.
c------------------------------------------------------------------------
c
c  Externals.
c
	real GetAmp,GetPhase,GetPhasW,GetReal,GetImag
	external GetAmp,GetPhasW,GetPhase,GetReal,GetImag
c
	if(doamp)  call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  'Amp',Feeds,doplot,dolog,dodtime,symbol,GetAmp,ppp)
	if(dophase)then
	  if(dowrap)then
	    call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	      'Phase',Feeds,doplot,dolog,dodtime,symbol,GetPhasW,ppp)
	  else
	    call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	      'Phase',Feeds,doplot,dolog,dodtime,symbol,GetPhase,ppp)
	  endif
	endif
	if(doreal) call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  'Real',Feeds,doplot,dolog,dodtime,symbol,GetReal,ppp)
	if(doimag) call GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  'Imag',Feeds,doplot,dolog,dodtime,symbol,GetImag,ppp)
	end
c************************************************************************
	subroutine BpPlt(freq,G,nfeeds,nants,nchan,range,
     *	  Feeds,doamp,dophase,dowrap,doreal,doimag,doplot,dolog,
     *    symbol,ppp)
c
	implicit none
	integer nfeeds,nants,nchan,ppp,symbol
	complex G(nchan*nfeeds*nants)
	real freq(nchan),range(2)
	logical doamp,dophase,dowrap,doreal,doimag,doplot,dolog
	character Feeds(nfeeds)*(*)
c
c  Plot/list the bandpass shape.
c
c  Input:
c    nfeeds	Number of polarization feeds.
c    nants	Number of antennas.
c    nchan	Number of channels.
c    freq	The offset time of each solution.
c    G		The gains
c    range	Range along Y axis for plots.
c    Feeds	Used to form labels and descriptions.
c    doamp,dophase,doreal,doimag If true, the do the corresponding
c		plot/listing.
c    dowrap	True if we are not to attempt to unwrap.
c    doplot,dolog If true, do a plot or write the table.
c    symbol	Plotting symbol.
c    ppp	Plots per page.
c------------------------------------------------------------------------
c
c  Externals.
c
	real     GetAmp,GetPhasW,GetPhase,GetReal,GetImag
	external GetAmp,GetPhasW,GetPhase,GetReal,GetImag
c
	if(doamp)  call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  'Amp',Feeds,doplot,dolog,symbol,GetAmp,ppp)
	if(dophase)then
	  if(dowrap)then
	    call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	      'Phase',Feeds,doplot,dolog,symbol,GetPhasW,ppp)
	  else
	    call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	      'Phase',Feeds,doplot,dolog,symbol,GetPhase,ppp)
	  endif
	endif
	if(doreal) call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  'Real',Feeds,doplot,dolog,symbol,GetReal,ppp)
	if(doimag) call BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  'Imag',Feeds,doplot,dolog,symbol,GetImag,ppp)
	end
c************************************************************************
	subroutine PolPlt2(Leaks,nfeeds,nants,range,type,Feeds,
     *	  doplot,dolog,symbol,GetVal)
c
	implicit none
	integer nfeeds,nants,symbol
	complex Leaks(nfeeds*nants)
	logical doplot,dolog
	character Feeds(nfeeds)*(*),type*(*)
	real range(2)
	real GetVal
	external GetVal
c
c------------------------------------------------------------------------
	include 'gpplt.h'
	real x(2*MAXANT),y(2*MAXANT),Value
	integer ifeed,iant,j,j1,j2
	logical more
	character line*80,Label*16
c
c  Externals.
c
	character itoaf*3
c
	if(doplot)then
	  do ifeed=1,nfeeds
	    Value = 0
	    do iant=1,nants
	      x(iant) = iant
	      y(iant) = GetVal(Leaks(ifeed+nfeeds*(iant-1)),Value)
	    enddo
	    call SetPG(1.,real(nants),y,nants,range,.true.)
	    call pgpt(nants,x,y,symbol)
	    Label = Feeds(ifeed)//'-Leakage-'//type
	    call pglab('Antenna Number',Label,' ')
	  enddo
	endif
c
	if(dolog)then
	  Value = 0
	  do j=1,nfeeds*nants
	    y(j) = GetVal(Leaks(j),Value)
	  enddo
	  write(line,10)type,(Feeds(ifeed),ifeed=1,nfeeds)
   10	  format('# Listing of the ',a,
     *		' of the leakages for feeds ',4(a,:,','))
	  call LogWrite(line,more)
	  line = '# Number of antennas: '//itoaf(nants)
	  call LogWrite(line,more)
	  do j1=1,nfeeds*nants,6
	    j2 = min(j1+5,nfeeds*nants)
	    write(line,'(7f11.3)')(y(j),j=j1,j2)
	    call LogWrite(line,more)
	  enddo
	endif
	end
c************************************************************************
	subroutine GainPlt2(vis,time,G,nfeeds,nants,nsols,range,
     *	  Type,Feeds,doplot,dolog,dodtime,symbol,GetVal,ppp)
c
	implicit none
	integer nfeeds,nants,nsols,ppp,symbol
	real time(nsols),range(2)
	complex G(nfeeds*nants*nsols)
	logical doplot,dolog,dodtime
	character Feeds(nfeeds)*(*),vis*(*),Type*(*)
	real GetVal
	external GetVal
c
c  Do the plot or the listing.
c
c  Inputs:
c	Similar to GainPlt, except ...
c	GetVal	Routine used to convert to the desired quantity.
c------------------------------------------------------------------------
	include 'gpplt.h'
	character line*80,Title*48,Label*20
	logical more
	real x(maxTimes),y(maxTimes),Value(2*MAXANT)
	complex Gain
	integer iday,ihr,imin,isec,j,j1,j2,isol,ifeed,iant
	integer offset,nres,ng,length
c
c  Externals.
c
	character itoaf*3
	integer len1
c
c  Do the plots.
c
	if(doplot)then
	  do ifeed=1,nfeeds
	    nres = 0
	    do iant=1,nants
	      offset = ifeed + (iant-1)*nfeeds
	      ng = 0
	      Value(offset) = 0
	      do isol=1,nsols
		Gain = G(offset+(isol-1)*nfeeds*nants)
		if(abs(real(Gain))+abs(aimag(Gain)).gt.0)then
		  ng = ng + 1
		  x(ng) = time(isol)
		  y(ng) = GetVal(Gain,Value(offset))
		endif
	      enddo
	      if(ng.gt.0)then
		call SetPG(time(1),time(nsols),y,ng,range,dodtime)
		call pgpt(ng,x,y,symbol)
	        Label = Feeds(ifeed)//'-Gain-'//type
	        Title = 'Antenna '//itoaf(iant)//'File='//vis
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
	  do j=1,nfeeds*nants
	    Value(j) = 0
	  enddo
	  write(line,10)type,(Feeds(j),j=1,nfeeds)
   10	  format('# Listing of the ',a,' of the gains for ',4(a,:,','))
	  call LogWrite(line,more)
	  line = '# Number of antennas: '//itoaf(nants)
	  call LogWrite(line,more)
c
	  do isol=1,nsols
	    offset = (isol-1)*nants*nfeeds
	    do j=1,nfeeds*nants
	      y(j) = GetVal(G(offset+j),Value(j))
	    enddo
	    do j1=1,nfeeds*nants,6
	      j2 = min(j1+5,nfeeds*nants)
	      if(j1.eq.1)then
		if(dodtime)then
		  write(line(1:12),'(f11.6)')time(isol)
		else
		  isec = nint(time(isol))
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
	      write(line(13:80),'(6g11.3)')(y(j),j=j1,j2)
	      call LogWrite(line,more)
	    enddo
	  enddo
	endif
	end
c************************************************************************
	subroutine BpPlt2(freq,G,nfeeds,nants,nchan,range,
     *	  type,Feeds,doplot,dolog,symbol,GetVal,ppp)
c
	implicit none
	integer nfeeds,nants,nchan,ppp,symbol
	real freq(nchan),range(2)
	complex G(nchan*nfeeds*nants)
	logical doplot,dolog
	character Feeds(nfeeds)*(*),type*(*)
	real GetVal
	external GetVal
c
c  Do the plot or the listing.
c
c  Inputs:
c	Similar to BpPlt, except ...
c	GetVal	Routine used to convert to the desired quantity.
c------------------------------------------------------------------------
	include 'gpplt.h'
	character line*80,Label*20,Title*12
	logical more
	real x(maxTimes),y(maxTimes),freqmin,freqmax
	real Value(2*MAXANT)
	complex Gain
	integer ichan,ifeed,iant,offset,j,j1,j2,nres,ng
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
	  do ichan=1,nchan
	    freqmin = min(freqmin,freq(ichan))
	    freqmax = max(freqmax,freq(ichan))
	  enddo
c
	  do ifeed=1,nfeeds
	    nres = 0
	    do iant=1,nants
	      offset = (ifeed-1) + (iant-1)*nfeeds
	      ng = 0
	      Value(offset+1) = 0
	      do ichan=1,nchan
		Gain = G(ichan + nchan*offset)
		if(abs(real(Gain))+abs(aimag(Gain)).gt.0)then
		  ng = ng + 1
		  x(ng) = freq(ichan)
		  y(ng) = GetVal(Gain,Value(offset+1))
		endif
	      enddo
	      if(ng.gt.0)then
		call SetPG(freqmin,freqmax,y,ng,range,.true.)
		call pgpt(ng,x,y,symbol)
	        Label = Feeds(ifeed)//'-BandPass-'//type
	        Title = 'Antenna '//itoaf(iant)
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
	  call BpTitle(type,Feeds,nfeeds,nants)
	  do offset=1,nants*nfeeds
	    Value(offset) = 0
	  enddo
c
	  do ichan=1,nchan
	    offset = 0
	    do iant=1,nants
	      do ifeed=1,nfeeds
		Gain = G(ichan+nchan*offset)
	        y(offset+1) = GetVal(Gain,Value(offset+1))
		offset = offset + 1
	      enddo
	    enddo
	    do j1=1,nfeeds*nants,6
	      j2 = min(j1+5,nfeeds*nants)
	      if(j1.eq.1)then
		write(line,'(f10.5,6f10.5)')freq(ichan),(y(j),j=j1,j2)
	      else
		write(line,'(10x,  6f10.5)')            (y(j),j=j1,j2)
	      endif
	      call LogWrite(line,more)
	    enddo
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
	line = '# Number of antennas: '//itoaf(nants)
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
	real function GetAmp(G,pAmp)
c
	implicit none
	complex G
	real pAmp
c
c  Get the amplitude of a complex value.
c------------------------------------------------------------------------
	GetAmp = abs(G)
	end
c************************************************************************
	real function GetPhase(G,pPhase)
c
	implicit none
	complex G
	real pPhase
c
c  Get the unwrapped phase of a complex value.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	if(abs(real(G))+abs(aimag(G)).eq.0)then
	  GetPhase = 0
	else
	  GetPhase = 180/pi * atan2(aimag(G),real(G))
	  GetPhase = GetPhase - 360*nint((GetPhase-pPhase)/360.)
	  pPhase = 0.5*(GetPhase + pPhase)
	endif
	end
c************************************************************************
	real function GetPhasW(G,pPhase)
c
	implicit none
	complex G
	real pPhase
c
c  Get the wrapped phase of a complex value.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	if(abs(real(G))+abs(aimag(G)).eq.0)then
	  GetPhasW = 0
	else
	  GetPhasW = 180/pi * atan2(aimag(G),real(G))
	endif
	end
c************************************************************************
	real function GetReal(G,pReal)
c
	implicit none
	complex G
	real pReal
c
c  Get the real part of a complex value.
c------------------------------------------------------------------------
	GetReal = real(G)
	end
c************************************************************************
	real function GetImag(G,pImag)
c
	implicit none
	complex G
	real pImag
c
c  Get the imaginary of a complex value.
c------------------------------------------------------------------------
	GetImag = aimag(G)	
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
