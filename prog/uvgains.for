c************************************************************************
	program uvgains
	implicit none
c
c= uvgains - Create baseline-based channel gains from averaged uv-data
c& mchw
c: uv analysis
c+
c	UVGAINS averages uv-data, and creates channel gains for each
c	baseline for either spectral line or wideband channels. The
c	channel gains are written into the output averaged uv-data and
c	are used to correct for bandpass or closure errors in the data.
c	UVGAINS can also be used to make autocorrelation ON/OFF scans,
c	and fit polynomials to spectral baselines.
c	The channel gains are applied when copying or imaging the uv-data.
c	To apply the channel gains to another uv-data file, use COPYHD to
c	copy the channel gains items (cgains, ncgains, ncbase) or wideband
c	gains items (wgains, nwgains, nwbase) into the other uv-data file.
c	The channel or wideband gains should only be transfered if the
c	uv-data files use the same correlator configuration.
c	In general the gains are applied automatically by Miriad tasks when
c	plotting (uvplt, uvspec), copying (uvcat, uvcal, uvaver, uvgains),
c	or imaging (invert). The channel gains and wideband gains are not
c	interpolated and can only be applied if all the channels are used,
c	i.e. in plotting, or imaging all the channels. To plot or process
c	other line types the channel gains must first be applied by copying
c	the channel gains into the Miriad dataset to be corrected (copyhd),
c	and writing all the channels into a new Miriad dataset (uvcat, uvcal). 
c
c	Examples:
c	
c	To derive the channel gains relative to the 1st wideband channel:
c	    uvgains vis=mars.12apr out=pass interval=1000 ref=wide,1
c
c	To apply the gains to another file:
c	    copyhd in=pass out=omc1.12apr items=ncgains,cgains,ncbase 
c	(gpcopy can also be used to copy the channel gains)
c	
c	To plot the passband corrected spectra (must plot all channels):
c	    uvspec vis=omc1.12apr device=/xw
c
c	To plot the spectra without the passband correction:
c	    uvspec vis=omc1.12apr device=/xw options=nopass
c
c	To process other line types, first write a corrected dataset:
c	    uvcal vis=omc1.12apr out=omc1.12apr.pass options=nowide
c	(items nwgains,nwbase,wgains are needed to correct the wideband)
c
c	To make ON/OFF scans for autocorrelation data:
c	    uvgains vis=off out=passband
c	    gpcopy vis=passband out=on
c
c	The entire ON/OFF scan can be plotted:
c	    uvspec vis=on device=/xw
c
c	The passband must be applied before further processing:
c	    uvcat vis=on out=on-off
c
c	To fit the spectral baselines in autocorrelation data:
c	    uvgains vis=on-off  options=window,polyfit device=/xw
c		badchan=(where the spectral lines are) out=
c@ vis
c	The name of the input uv-data file.
c@ select
c	The normal uv selection commands. The default is use everything.
c	See the help on "select" for more details.
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	See the help on "line" for more details.
c	The default is all channels (or all wide channels if there are no
c	spectral channels). The output will consist of only spectral or
c	wideband data (but not both).
c	The data to be corrected must use the same line type.
c	Only un-averaged channel data can be used with options=polyfit.
c@ ref
c	The normal reference linetype, in the form:
c	  line,start,width
c	The channel or wideband gains can be determined relative to the
c	reference line before averaging the data. This may be the best method
c	in the case of high SNR or rapid antenna gain fluctuations. In this
c	case no gain corrections should be applied: options=nocal,nopol
c	Using a reference line and applying gains does not make sense.
c	The default is no reference line.
c@ stokes
c	If a value is given, uvgains converts the input into the required
c	polarizations before writing to the output. Default is to copy
c	across the polarizations present in the input files.
c@ interval
c	Time averaging interval, in minutes. The default is 0. (i.e. no
c	averaging). The channel gains are derived from the first average.
c	The uv-data can be averaged into several intervals to see how
c	cgains or wgains change with time. A large interval averages each
c	occurance of each 'source'.
c@ out
c	The name of the output uv data set. No default.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreviated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   'nocal'      Do not apply the gains file. By default, UVGAINS
c	                applies the gains file in copying the data.
c	   'nopol'      Do not apply polarization corrections. By default
c	                UVGAINS corrects for polarization cross-talk.
c	   'dopass'     Apply bandpass corrections. By default UVGAINS
c	                does not correct for the shape of the bandpass.
c	   'polyfit'    Use polynomial fits to the averaged data to derive
c	                the channel gains.
c	   'window'	Fit polynomial to each spectral window.
c			Default is to fit all channels.
c	   'dsb'	Fit polynomial to each sideband for double sideband
c			spectra. 'dsb' and 'window' are mutually exclusive.
c	   'scalar'	Fit polynomial to amplitude and phase. Default is
c			to fit (real,imag). 
c	   'unwrap'	Attempt to extend phases beyond -180 to 180 degrees
c			for 'scalar' polynomial fit.
c	   'noamp'	Re-normalize the gains to the average amplitude on
c			each baseline. i.e. do not force amplitude closure.
c			The default normalizes the gains to obtain unit
c			amplitude on all baselines. i.e. amplitude closure.
c@ device
c	PGPLOT device for polynomial fits. The default is to ask user.
c@ nxy
c	Two values. Number of plot windows in x and y, Default nx=1, ny=nx.
c@ npoly
c	Two values to specify the order of the Lagrange polynomial fit to
c	the real & imaginary, or amplitude & phase of the averaged uv-data.
c	Different values can be given for each window.
c       The maximum is 8th order polynomial. Default 0,0,0,0,0,0,0,0,0,0,0,0
c       If just two values are given, these values are used for each window.
c@ badchan
c	For options=polyfit
c	Number of ranges of bad channels followed by list of up to 20 pairs
c	of numbers to specify range of channels to exclude in polyfit.
c	e.g. to exclude 2 center channels in eight 64-channel windows, use:
c	badchan=8,32,33,96,97,160,161,224,225,288,289,352,353,416,417,480,481
c@ endchan
c	For options=polyfit
c	Number of channels to drop from window edges in polyfit. Default=4.
c--
c  History:
c    mchw 30nov90 Tentative version.
c    rjs  19jun91 Original version for uvaver task.
c    rjs   5jul91 Fixed bug in the uvset(...,'wide',...) call.
c    rjs  18jul91 Fixed numerous bugs dealing with time averaging.
c    rjs  29aug91 Fiddled copying of systemp in WindUpd.
c    mchw 12sep91 Option to write wgains and cgains items.
c    mchw 14jul92 Changed name to uvgains.
c    djw  14jul92 Polynomial fits and PGPLOT interface to plot gains.
c    mchw 04aug92 Cleanup code and doc. Make polyfit an option and
c		  provide keyword input for polyfit & plot parameters.
c    rjs  17aug92 Added var routines. Various other cleaning up.
c    mjs  10mar93 Elim. compiler warns on convex (label defined but not
c                 referenced).
c    mjs  13mar93 pgplot subr names have less than 7 chars.
c    djw  30mar93 Added badchan and endchan to exclude in poly fits.
c    mchw 14apr93 Handle missing baselines. Fix old style loop in matinv.
c    mchw 15apr93 Added polynomial fits to each spectral window,
c			and unwrap options.
c     Temporarily Suppress new average because of duplicate dra ddec in data.
c    mchw 06aug93 Better documentation.
c    mchw 15sep93 Set dotaver=.true. to get bandpass for no averaging.
c    mchw 25dec93 Fix bug with excluded endchans in polyfit with window.
c		  scalar and dsb options. Fit to (real,imag) is default.
c    mchw 03jan94 Exclude flagged data. Re-normalize passband to unit gain.
c    mchw 10apr94 Put MAXWIN into maxdim.h
c    djw  16jul94 List of poly orders; dimension npoly(2*MAXWIN) 
c    mchw 08sep94 Suppress plots for constant (autocorrelation) data.
c    rjs  29mar96 Handle autocorrelation data.
c    mchw 29dec96 Options=noamp.
c		  The default now makes unit amplitude on all baselines.
c  Bugs:
c    * This can write out either massaged channels, or massaged wides,
c      but not both simultaneously.
c    * Too much of this code worries about polarisations.
c-----------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='uvgains: version 1.0 08-Sep-94')
	character uvflags*8,ltype*16,out*64,device*40
	real inttime
	integer npol,Snpol,pol,tIn,tOut,vupd,nread
	integer ninter,nxy(2),npoly(2*MAXWIN),inpoly
	logical dotaver
	logical doflush,buffered,PolVary
	logical dopass,dowgains,dopoly,dowindow,dowrap,scalar,dsb,noamp
	double precision preamble(4),T0,T1,Tprev,interval
	complex data(MAXCHAN)
	logical flags(MAXCHAN)

	integer endchan,maxbad
	parameter(maxbad=20)
	integer nbad,badchan(maxbad)
	integer i
c
c  Externals.
c
	logical uvDatPrb,uvDatOpn,uvVarUpd
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags,dopoly,dowindow,dowrap,scalar,dsb,noamp)
	call uvDatInp('vis',uvflags)
	call keyd('interval',interval,0.d0)
	call keya('out',out,' ')
	call keya('device',device,'?')
	call keyi('nxy',nxy(1),1)
	call keyi('nxy',nxy(2),nxy(1))
c  initialize npoly=0 
	do i=1,2*MAXWIN
	  npoly(i)=0
	enddo
	call mkeyi('npoly',npoly,2*MAXWIN,inpoly)
	if(inpoly.eq.2)then
	  do i=3,2*MAXWIN,2
	    npoly(i)=npoly(1)
	    npoly(i+1)=npoly(2)
	  enddo
	endif
	call keyi('endchan',endchan,4)
	call keyi('badchan',nbad,0)
	if(nbad.gt.0)then
	  do i=1,2*nbad
	    call keyi('badchan',badchan(i),0)
	  enddo
	endif

	call keyfin
c
c  Check the input parameters.
c
	if(out.eq.' ')call bug('f','Output file must be specified')
	if(interval.lt.0)call bug('f','Illegal value for interval')
	interval = interval/(24.*60.)
c
c  Open the input and the output files.
c
	call uvDatGti('npol',Snpol)
	dotaver = interval.gt.0.or.uvDatPrb('polarization?',0.d0)
	dotaver = .true.
	if(.not.uvDatOpn(tIn))call bug('f','Error opening input')
	call uvopen(tOut,out,'new')
c
	call uvDatGta('ltype',ltype)
	call VarInit(tIn,ltype)
	call VarOnit(tIn,tOut,ltype)
c
	call uvVarIni(tIn,vupd)
c	call uvVarSet(vupd,'dra')
c	call uvVarSet(vupd,'ddec')
	call uvVarSet(vupd,'source')
c
c  Determine which channel gains are to to written.
c
	dowgains = ltype.eq.'wide'
c
c  More initialisation.
c
	Snpol = 0
	dopass = .true.
	PolVary = .false.
	doflush = .false.
	buffered = .false.
	call BufIni
c
c  Loop over the data.
c
	call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	Tprev = preamble(3)
	T1 = Tprev + interval
	T0 = Tprev
	dowhile(nread.gt.0)
c
c  Determine if we need to flush out the averaged data. Also determine
c  if we need to update the variables that we calculate the hard way.
c
	  if(dotaver)then
	    doflush = uvVarUpd(vupd)
	    doflush = (doflush.or.preamble(3).gt.T1.or.
     *				  preamble(3).lt.T0).and.buffered
	  endif
c
c  Flush out the accumulated data & Write the channel gains item.
c  for the first average.
c
	  if(doflush)then
	  if(dopass) call Passwrite(tin,tout,dowgains,dopoly,dowindow,
     *		dowrap,scalar,dsb,device,nxy,npoly,noamp,
     *		endchan,nbad,maxbad,badchan)
	    dopass=.false.
	    call BufFlush(tOut,npol,ninter)
	    PolVary = PolVary.or.npol.eq.0.or.
     *		(Snpol.ne.npol.and.Snpol.gt.0)
	    call uvrdvrr(tIn,'inttime',inttime,1.)
	    call uvputvrr(tOut,'inttime',ninter*inttime,1)
	    Snpol = npol
	    T0 = preamble(3)
	    T1 = T0 + interval
	    buffered = .false.
	  endif	
c
c  Either accumulate the new data (if time averaging), else write out
c  the data.
c
	  if(dotaver)then
	    call BufAcc(preamble,data,flags,nread)
	    call VarCopy(tIn,tOut)
	    buffered = .true.
	  else
	    call uvDatGti('pol',pol)
	    call uvputvri(tOut,'pol',pol,1)
	    if(npol.le.0)then
	      call uvDatGti('npol',npol)
	      if(npol.ne.Snpol)then
		call uvputvri(tOut,'npol',npol,1)
		PolVary = Snpol.gt.0
	      endif
	      Snpol = npol
	    endif
	    call VarCopy(tIn,tOut)
	    call uvwrite(tOut,preamble,data,flags,nread)
	    npol = npol - 1
	  endif
c
c  Keep on going. Read in another record.
c
	  Tprev = preamble(3)
	  call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	enddo
c
c  Flush out anything remaining.
c
	if(buffered)then
	  if(dopass) call Passwrite(tin,tout,dowgains,dopoly,dowindow,
     *		dowrap,scalar,dsb,device,nxy,npoly,noamp,
     *		endchan,nbad,maxbad,badchan)
	  dopass=.false.
	  call VarCopy(tIn,tOut)
	  call BufFlush(tOut,npol,ninter)
	  call uvrdvrr(tIn,'inttime',inttime,1.)
	  call uvputvrr(tOut,'inttime',ninter*inttime,1)
	  PolVary = PolVary.or.npol.le.0.or.
     *	    (Snpol.ne.npol.and.Snpol.gt.0)
	  Snpol = npol
	endif
c
c  Write things to the header which describe the data as a whole.
c
	if(.not.PolVary.and.Snpol.gt.0)call wrhdi(tOut,'npol',Snpol)
c
c  Update the history and close up files.
c
	call hdcopy(tIn,tOut,'history')
	call hisopen(tOut,'append')
	call hiswrite(tOut,'UVGAINS Miriad '//version)
	call hisinput(tOut,'UVGAINS')
	call hisclose(tOut)
	call uvclose(tOut)
	call uvDatCls
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine GetOpt
     *		(uvflags,dopoly,dowindow,dowrap,scalar,dsb,noamp)
c
	implicit none
	character uvflags*(*)
	logical dopoly,dowindow,dowrap,scalar,dsb,noamp
c
c  Determine the flags to pass to the uvdat routines, and other options.
c
c  Output:
c    uvflags	Flags to pass to the uvdat routines.
c    dopoly	Do polynomial fits if true.
c    dowindow	Fit polynomial to each window.
c    dsb	Fit polynomial to each sideband.
c    scalar	Fit polynomial to amplitude and phase.
c    dowrap	Attempt to extend phase beyond -180 to 180 degrees.
c    noamp      Re-normalize the gains to the average amplitude for
c                       each baseline. i.e. no amplitude closure.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=9)
	character opts(nopts)*8
	integer l
	logical present(nopts),docal,dopol,dopass
	data opts/'nocal   ','nopol   ','dopass  ','polyfit ',
     *		  'window  ','unwrap  ','scalar  ','dsb     ',
     *		  'noamp   '/
c
	call options('options',opts,present,nopts)
	docal = .not.present(1)
	dopol = .not.present(2)
	dopass=      present(3)
	dopoly = present(4)
	dowindow = present(5)
	dowrap = present(6)
	scalar = present(7)
	dsb =    present(8)
	noamp =  present(9)
	uvflags = 'dslr'
	l = 4
	if(docal)then
	  l = l + 1
	  uvflags(l:l) = 'c'
	endif
	if(dopol)then
	  l = l + 1
	  uvflags(l:l) = 'e'
	endif
	if(dopass)then
	  l = l + 1
	  uvflags(l:l) = 'f'
	endif
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine BufIni
	implicit none
c
c  Initialise the routines which do the buffering and averaging of
c  the visibility data.
c  All the buffering/averaging is performed in arrays stored in a
c  common block.
c-----------------------------------------------------------------------
	include 'uvgains.h'
	free = 1
	mbase = 0
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine BufFlush(tOut,npol,ninter)
c
	implicit none
	integer tOut,npol,ninter
c
c  This writes out the averaged data. The accumulated data is in common.
c  This starts by dividing the accumulated data by "N", and then writes
c  it out.
c
c  Inputs:
c    tOut	The handle of the output file.
c  Output:
c    npol	The number of polarisations in the output. If this
c		varies, a zero is returned.
c    ninter	Average number of intervals per baseline per polarisation.
c------------------------------------------------------------------------
	include 'uvgains.h'
	complex data(MAXCHAN)
	double precision preambl(4)
	logical flags(MAXCHAN)
	integer i,j,k,nbp,p
	logical PolVary
c
	nbp = 0
	ninter = 0
	npol = 0
	do j=1,mbase
	  if(cnt(j).gt.0)then
	    if(npols(j).ne.npol)then
	      call uvputvri(tOut,'npol',npols(j),1)
	      PolVary = npol.gt.0
	      npol = npols(j)
	    endif
	    preambl(1) = preamble(1,j) / cnt(j)
	    preambl(2) = preamble(2,j) / cnt(j)
	    preambl(3) = preamble(3,j) / cnt(j)
	    preambl(4) = preamble(4,j) / cnt(j)
c
c  Average the data in each polarisation. If there is only one scan in the
c  average, not bother to average it.
c
	    do i=1,npol
	      p = pnt(i,j) - 1
	      call uvputvri(tOut,'pol',pols(i,j),1)
	      ninter = ninter + cntp(i,j)
	      nbp = nbp + 1
	      if(cntp(i,j).gt.1)then
		do k=1,nchan(i,j)
		  if(count(k+p).gt.0)then
		    flags(k) = .true.
		    data(k) = buf(k+p) / count(k+p)
		  else
		    flags(k) = .false.
		    data(k) = 0
		  endif
		enddo
 		call uvwrite(tOut,preambl,data,flags,nchan(i,j))
	      else
	        do k=1,nchan(i,j)
	          flags(k) = count(k+p).gt.0
	        enddo
		call uvwrite(tOut,preambl,buf(p+1),flags,nchan(i,j))
	      endif		
	    enddo
	  endif
	enddo
c
c  Reset the counters.
c
	free = 1
	mbase = 0
c
c  If the number of polarisations varied, zero npol.
c
	if(PolVary) npol = 0
	ninter = ninter / nbp
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine BufAcc(preambl,data,flags,nread)
c
	implicit none
	integer nread
	double precision preambl(4)
	complex data(nread)
	logical flags(nread)
c
c  This accumulates the visibility data. The accumulated data is left
c  in common.
c
c  Input/Output:
c    preambl	Preamble. Destroyed on output.
c    data	The correlation data to be averaged. Destroyed on output.
c    flags	The data flags.
c    nread	The number of channels.
c------------------------------------------------------------------------
	include 'uvgains.h'
	integer i,p,bl,pol,i1,i2
c
c  Determine the baseline number, and conjugate the data if necessary.
c
	call BasAnt(preambl(4),i1,i2)
	bl = ((i2-1)*i2)/2 + i1
c
c  Zero up to, and including, this baseline.
c
	do i=mbase+1,bl
	  cnt(i) = 0
	enddo
	mbase = max(mbase,bl)
c
c  Add in this visibility.
c
	if(cnt(bl).eq.0)then
	  cnt(bl) = 1
	  npols(bl) = 0
	  preamble(1,bl) = preambl(1)
	  preamble(2,bl) = preambl(2)
	  preamble(3,bl) = preambl(3)
	  preamble(4,bl) = preambl(4)
	else
	  cnt(bl) = cnt(bl) + 1
	  preamble(1,bl) = preamble(1,bl) + preambl(1)
	  preamble(2,bl) = preamble(2,bl) + preambl(2)
	  preamble(3,bl) = preamble(3,bl) + preambl(3)
	  preamble(4,bl) = preamble(4,bl) + preambl(4)
	endif
c
c  Determine the polarisation.
c
	call uvDatGti('pol',pol)
	p = 0
	do i=1,npols(bl)
	  if(pols(i,bl).eq.pol) p = i
	enddo
c
c  A new baseline. Set up the description of it.
c
	if(p.eq.0)then
	  npols(bl) = npols(bl) + 1
	  p = npols(bl)
	  if(p.gt.MAXPOL) call bug('f',
     *	    'Too many polarizations, in BufAcc')
	  pols(p,bl) = pol
	  cntp(p,bl) = 1
	  nchan(p,bl) = nread
	  pnt(p,bl) = free
	  free = free + nread
	  if(free.gt.MAXAVER)call bug('f',
     *	    'Too much data to accumulate, in BufAcc')
c
c  Copy across the new data.
c
	  p = pnt(p,bl) - 1
	  do i=1,nread
	    if(flags(i))then
	      buf(i+p) = data(i)
	      count(i+p) = 1
	    else
	      buf(i+p) = (0.,0.)
	      count(i+p) = 0
	    endif
	  enddo
c
c  Else accumulate old data.
c
	else
	  cntp(p,bl) = cntp(p,bl) + 1
	  nread = min(nread,nchan(p,bl))
	  nchan(p,bl) = nread
	  p = pnt(p,bl) - 1
	  do i=1,nread
	    if(flags(i))then
	      buf(i+p) = buf(i+p) + data(i)
	      count(i+p) = count(i+p) + 1
	    endif
	  enddo
	endif
c
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine Passwrite(tin,tout,dowgains,dopoly,dowindow,
     *		dowrap,scalar,dsb,device,nxy,npoly,noamp,
     *		endchan,nbad,maxbad,badchan)
c
	implicit none
	include 'uvgains.h'
	integer tin,tout,nxy(2),npoly(2*MAXWIN)
	logical dowgains,dopoly,dowrap,dowindow,scalar,dsb,noamp
	character*(*) device
c
c  Write out the channel gains item.
c
c  Input:
c    tin	Handle of the input file.
c    tout	Handle of the output file.
c    dowgains	Write wgains item if true, else write cgains item.
c    dopoly	Do polynomial fits to derive channel gains.
c    dowindow	Do polynomial fits to each spectral window.
c    dsb	Fit polynomial to each sideband.
c    scalar	Fit polynomial to amplitude and phase.
c    dowrap	Attempt to extend phase beyond -180 to 180 degrees.
c    device	PGLOT device
c    nxy	number of plot windows in x & y
c    npoly	Order of polynomial fits for amplitude and phase.
c    endchan 	number of edge channels to exclude from poly fit
c    nbad	number of bad channel ranges
c    maxbad	max number of bad channel ranges
c    badchan	pairs of channels corresponding to bad channel ranges
c    noamp      Re-normalize the gains to the average amplitude for
c                       each baseline. i.e. no amplitude closure.
c------------------------------------------------------------------------
	integer i,j,k,p,iostat,item,offset,ngains,b1,b2
	complex gains(MAXCHAN*MAXBASE),temp
        real x(MAXCHAN),yamp(MAXCHAN),yphase(MAXCHAN)
	integer endchan,maxbad
	integer nbad,badchan(maxbad)
	logical flags(MAXCHAN)
	real sum,wt
	character label*12
c
c  Create a gains item.
c
	if(dowgains) then
	  call haccess(tout,item,'wgains','write',iostat)
	else
	  call haccess(tout,item,'cgains','write',iostat)
	endif
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
c
c  Now calculate the channel gains, and write then out
c
      if(dopoly)then
c
c  Use polynomial fits to real and imag or amplitude and phase
c  to derive channel gains. Exclude flagged data from fits.
c
	call pgbeg(0,device,nxy(1),nxy(2))
	do j=1,mbase
	  if(cnt(j).gt.0)then
	    call pgpage
	    do i=1,npols(j)
	      p = pnt(i,j) - 1
	      ngains = nchan(i,j)
	      offset = (j-1)*ngains
	      b2 = nint(preamble(4,j)/cnt(j))
	      b1 = b2/256
	      b2 =b2 -256*b1
	      write(label,'(a,i3,a,i3)') 'ant: ',b1,'-',b2
	      do k=1,ngains
                x(k)=k
	        flags(k) = count(k+p).gt.0.
		if(flags(k)) then
	          temp = buf(k+p) / count(k+p)
		else
		  temp = (0.,0.)
		endif
		if(scalar)then
                  yamp(k) = sqrt( real(temp)**2 + aimag(temp)**2 )
                  yphase(k) = atan2(aimag(temp),real(temp))
		else
                  yamp(k) = real(temp)
                  yphase(k) = aimag(temp)
		endif
	      enddo
              call pfit(ngains,x,yamp,yphase,flags,gains,offset,npoly,
     *          tin,dowrap,scalar,dsb,dowindow,noamp,
     *		endchan,nbad,maxbad,badchan,label)
	    enddo
	  endif
	enddo
	call pgend
c
c  Use averaged uv-data to derive channel gains.
c
      else
	do j=1,mbase
	  if(cnt(j).gt.0)then
	    do i=1,npols(j)
	      p = pnt(i,j) - 1
	      ngains = nchan(i,j)
	      offset = (j-1)*ngains
	      do k=1,ngains
		if(abs(buf(k+p)).gt.0)then
		  gains(k+offset) = count(k+p)/buf(k+p)
	        else
	          gains(k+offset) = 1.
		endif
	      enddo
c
c  Re-normalize the gains to the average amplitude for each baseline.
c
	      if(noamp)then
	        sum = 0.
	        wt = 0.
		do k=1,ngains
		 if(count(k+p).gt.0)then
		  sum = sum + abs(buf(k+p)/count(k+p))
		  wt = wt + 1.
		 endif
		enddo
		if((sum/wt).gt.0.)then
		  do k=1,ngains
	            gains(k+offset) = sum/wt * gains(k+offset)
		  enddo
	        endif
	      endif
c
c  Process next baseline and polarization.
c
	    enddo
	  endif
	enddo
c
      endif
c
c  Write out the channel gains item.
c
	offset = 0
	call hwriter(item,gains,offset,8*ngains*mbase,iostat)
	if(iostat.ne.0)then
	  call bug('w','I/O error while writing to gains item')
	  call bugno('f',iostat)
	endif
c
c  Close up the gains item and write some header information.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing output gains item')
	  call bugno('f',iostat)
	endif
	if(dowgains) then
	  call wrhdi(tout,'nwgains',ngains)
	  call wrhdi(tout,'nwbase',mbase)
	else
	  call wrhdi(tout,'ncgains',ngains)
	  call wrhdi(tout,'ncbase',mbase)
	endif
c
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine pfit(ngains,x,yamp,yphase,flags,gains,offset,npoly,
     *          tin,dowrap,scalar,dsb,dowindow,noamp,
     *		endchan,nbad,maxbad,badchan,label)
c
	implicit none
	include 'uvgains.h'
	integer tin,ngains,offset,npoly(2*MAXWIN)
	integer endchan,maxbad,nbad,badchan(maxbad)
	logical dowrap,dowindow,scalar,dsb,noamp
	character label*(*)
c
c Do polynomial fit to averaged data
c
c  Input:
c    ngains	Number of channels.
c    x   	Channel number 1,...ngains.
c    yamp	Amp or real array.
c    yphase	Phase or imaginary array.
c    offset	Offset in gain table.
c    npoly	Order of polynomial fits to amplitude & phase.
c    tin	Handle of the input file.
c    dowrap	Attempt to extend phase beyond -180 to 180 degrees.
c    scalar	Fit polynomial to amplitude and phase.
c    dsb	Fit polynomial to each sideband.
c    dowindow	Do polynomial fits to each spectral window.
c    device	PGLOT device
c    endchan 	number of edge channels to exclude from fit
c    nbad	number of bad channel ranges
c    maxbad	max number of bad channel ranges
c    badchan	pairs of channels corresponding to bad channel ranges
c    label	Plot label.
c    noamp      Renormalize the gains to the average amplitude for
c                       each baseline. i.e. no amplitude closure.
c  Output
c    gains = 1. / polynomial fit evaluated at channel
c------------------------------------------------------------------------
	real pi
	parameter (pi=3.1415926)
	complex gains(MAXCHAN*MAXBASE)
        real x(MAXCHAN),yamp(MAXCHAN),yphase(MAXCHAN)
        real w(MAXCHAN),zamp(MAXCHAN),zphase(MAXCHAN)
	logical flags(MAXCHAN)
        real coeff(9),xmin,xmax
	real yamin,yamax,ypmin,ypmax,sum
	complex expi
	integer i,j,j1,k,nspect
	integer ischan(MAXWIN),nschan(MAXWIN)
	double precision sdf(MAXWIN),sfreq(MAXWIN)
c
c  Initialize arrays.
c
        do k=1,ngains
	  if(flags(k))then
	    w(k)=1.
	  else
	    w(k)=0.
	  endif
	  zamp(k) = 1.
	  zphase(k) = 0.
        enddo
c
c  Check for 2pi discontinuities
c
	if(scalar.and.dowrap) then
	  do k=2,ngains
	    if (yphase(k)-yphase(k-1) .gt. pi) then
	      yphase(k) = yphase(k) - 2.*pi
	    else if (yphase(k)-yphase(k-1) .lt. -pi) then
	      yphase(k) = yphase(k) + 2.*pi
 	    endif
	  enddo
	endif
c
c  Set up weight array (w=0.) for bad channels.
c
	do k=1,2*nbad,2
	  do i=badchan(k),badchan(k+1)
	    w(i)=0.
	  enddo
	enddo
c
c  Determine plot limits
c
        xmin=x(1)
        xmax=x(ngains)
        yamin=yamp(1)
        ypmin=yphase(1)
        yamax=yamp(1)
        ypmax=yphase(1)
c
        do k=1,ngains
          yamin=min(yamin,yamp(k))
          ypmin=min(ypmin,yphase(k))
          yamax=max(yamax,yamp(k))
          ypmax=max(ypmax,yphase(k))
        enddo
c
	if(scalar)then
	  ypmin=-1.1*pi
	  ypmax=+1.1*pi
	endif
c
c  Plot imag or phase.
c
	if(ypmin.ne.ypmax)then
          call pgsvp(0.1,0.9,0.1,0.4)
          call pgswin(xmin,xmax,ypmin,ypmax)
	  call pgbox('bcnst',0.,0,'bcnst',0.,0)
          call pgpt(ngains,x,yphase,1)
	  if(scalar)then
	    call pglab('channel','phase',label)
	  else
	    call pglab('channel','imag',label)
	  endif
	endif
c
c  Plot real or amplitude.
c
	if(yamin.ne.yamax)then
          call pgsvp(0.1,0.9,0.5,0.8)
          call pgswin(xmin,xmax,yamin,yamax)
	  call pgbox('bcnst',0.,0,'bcnst',0.,0)
          call pgpt(ngains,x,yamp,1)
	  if(scalar)then
	    call pglab('channel','amp',label)
	  else
	    call pglab('channel','real',label)
	  endif
	endif
c
c  Do polynomial fits
c
	if(dowindow)then
	  call uvgetvri(tin,'nspect',nspect,1)
	  if(nspect.le.0)
     *	    call bug('f','Bad value for uv variable nspect')
	  call uvgetvri(tin,'ischan',ischan,nspect)
	  call uvgetvri(tin,'nschan',nschan,nspect)
	  call uvgetvrd(tin,'sdf',sdf,nspect)
	  call uvgetvrd(tin,'sfreq',sfreq,nspect)
c
	  do i=1,nspect
	    j = ischan(i)
	    k = nschan(i)
	    do j1=1,endchan
	      w(j+j1-1) = 0.
	      w(j+k-j1) = 0.
	    enddo
            call lspoly(npoly(2*i-1),k,
     *                  x(j),yamp(j),w(j),zamp(j),coeff)
            call lspoly(npoly(2*i),k,
     *                  x(j),yphase(j),w(j),zphase(j),coeff)
	  enddo
c	
	else if(dsb)then
	  k = ngains/2
	  do j=1,k+1,k
	    do j1=1,endchan
	      w(j+j1-1) = 0.
	      w(j+k-j1) = 0.
	    enddo
            call lspoly(npoly(1),k,x(j),yamp(j),w(j),zamp(j),coeff)
            call lspoly(npoly(2),k,x(j),yphase(j),w(j),zphase(j),coeff)
	  enddo
c
	else
	  do j1=1,endchan
	    w(j1) = 0.
	    w(ngains+1-j1) = 0.
	  enddo
          call lspoly(npoly(1),ngains,x,yamp,w,zamp,coeff)
          call lspoly(npoly(2),ngains,x,yphase,w,zphase,coeff)
	endif
c
c  Plot imag/phase fit
c
	if(ypmin.ne.ypmax)then
          call pgsvp(0.1,0.9,0.1,0.4)
	  call pgsci (7)
          call pgswin(xmin,xmax,ypmin,ypmax)
          call pgline(ngains,x,zphase,1)
	endif
c
c  Plot real/amp fit
c
	if(yamin.ne.yamax)then
          call pgsvp(0.1,0.9,0.5,0.8)
          call pgswin(xmin,xmax,yamin,yamax)
          call pgline(ngains,x,zamp,1)
	endif
	call pgsci (1)
c
c  Compute gains; use fitted (real,imag) or (amp,phase).
c
	sum = 0.
	if(scalar)then
	  do k=1,ngains
	    gains(k+offset) = 1. / ( zamp(k) *expi(zphase(k)) )
	    sum = sum + zamp(k)
	  enddo
	else
	  do k=1,ngains
	    gains(k+offset) = 1. / cmplx( zamp(k),zphase(k) )
	    sum = sum + sqrt(zamp(k)*zamp(k)+zphase(k)*zphase(k))
	  enddo
	endif
c
c  Re-normalize the gains to the average amplitude for each baseline.
c
	if(noamp.and.(sum/ngains).ne.0.)then
	  do k = 1,ngains
	    gains(k+offset) = gains(k+offset) * sum/ngains
	  enddo
	endif
c
        end
