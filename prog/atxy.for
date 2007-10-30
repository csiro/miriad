c************************************************************************
	program atxy
c
	implicit none
c
c= atxy - Apply on-line XY phase measurements to Miriad data.
c& rjs
c: uv analysis
c+
c	ATXY corrects data for XY phase errors. It determines the XY phase
c	measurements, either from the xyphase visibility variable or from
c	the text file written by AIPS ATLOD, averages them, and then
c	applies them to the data. The averaging step consists of finding
c	the median XY phase in a given interval.
c@ vis
c	The names of the input uv data sets. No default.
c@ xyphase
c	If the data were loaded with Miriad ATLOD, then they should contain
c	the on-line XY phase measurements in the xyphase variable. Howver
c	if the data were loaded with AIPS (and then transferred to Miriad
c	via FITS files), then you must give the name of the XY phase
c	text file written by AIPS. Generally it will be XYPHS_xx, where
c	xx is an AIPS user number.
c@ refant
c	Antennas to apply XY phase corrections to. Generally it is best to
c	apply corrections to only a single antenna -- the antenna which you
c	have the greatest confidence in the XY phase measurements. Often
c	this antenna will be the reference antenna during the observation.
c	If you apply the XY phase to a subset of antennas, one of these
c	antennas should be used as the ``refant'' antenna to GPCAL.
c	No default.
c@ interval
c	This gives the length of an averaging interval, in minutes. The
c	default is 30 minutes, but larger values may be useful.
c@ break
c       The times, in the normal Miriad format, where there are 
c       break points. A ``break point'' gives a time where some form of
c       instrumental glitch took place, and the xyphases jumped. By
c       giving a break point, you prevent a solution interval from spanning
c       this time.
c@ sideband
c	Sideband indicators. If the input was loaded with AIPS ATLOD, and
c	if you averaged it into a channel-0 data-set, you must give the
c	sideband indicator (+1 or -1) for each IF. This information is needed
c	to convert between the AIPS and Miriad XY phase convention.
c	If the input is still multi-channel data, or you used Miriad ATLOD,
c	it is wiser to let this parameter default. Note that ATXY cannot
c	cope with channel-0 data-sets when the sideband indicators
c	change with time.
c@ out
c	The name of the output uv data set. No default.
c@ options
c	Extra processing options. Several options can be given,
c	separated by commas. Minimum match is supported. Possible values
c	are:
c	  srcbreak  Generate break points every time there is a change
c	            in source.
c	  undo      Apply the negative of the XY phase correction. There
c	            should be no sensible reason to ever use this option.
c	  reweight  Reweight the data (in the lag domain) to minimise the
c	            so-called Gibbs phenomena and to help localise
c	            interference.
c--
c  History:
c    15oct93 rjs  Original version.
c     4nov93 rjs  Apply 0 to everything but the reference antenna.
c     9nov93 rjs  Flush out the last XY phase solution.
c    20nov93 rjs  Read from the xyphase variable if available.
c    25nov93 rjs  Doc change only.
c     1sep94 rjs  First day of spring. Autobreak option.
c     1sep94 rjs  w-axis changes.
c    21sep94 rjs  Vote on whether to use -1 or +1 for the default
c		  correction.
c     7sep94 rjs  Correct bug in deteriming time solution.
c    22mar95 rjs  Added the undo (madness) and reweight options.
c
c  Bugs:
c    * Probably a more sophiticated fitting process (rather than just
c      taking the medians) could be used.
c------------------------------------------------------------------------
	integer PolXX,PolYY,PolXY,PolYX
	parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
	include 'maxdim.h'
	integer MAXBREAK,ATANT,ATIF,NWTS
	character version*(*)
	parameter(MAXBREAK=128,ATANT=6,ATIF=2,NWTS=64)
	parameter(version='AtXY: version 1.0 22-Mar-95')
c
	integer lVis,lOut,vCopy,vFreq
	character vis*64,out*64,txt*64,dtype*1
	integer ants(MAXANT),nbreak,nants,npol,nschan(ATIF),nspect
	integer dside(ATIF),uside(ATIF),nside
	integer ngood,nbad,i,length,nchan,i1,i2,pol
	integer nrewt,nnowt,nsus
	real Ratio
	character line*64
	logical antmask(ATANT),updated,flags(MAXCHAN),doxy1,doxy2
	logical dovar,doauto,undo,dowt
	real interval,gap,lags(NWTS),wts(NWTS)
	double precision break(MAXBREAK),freq(ATIF)
	double precision preamble(5)
	complex data(MAXCHAN)
c
c  Externals.
c
	character itoaf*8
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call keya('xyphase',txt,' ')
	call keyr('interval',interval,30.)
	call keyr('interval',gap,interval)
        call mkeyt('break',break,MAXBREAK,nbreak,'time')
	call mkeyi('refant',ants,MAXANT,nants)
	call mkeyi('sideband',uside,ATIF,nside)
	call GetOpt(doauto,undo,dowt)
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
	if(min(interval,gap).le.0)call bug('f','Bad interval value')
	interval = interval / (24.*60.)
	gap = gap / (24.*60.)
c
c  Check that the sideband indicators are +/- 1.
c
	do i=1,nside
	  if(abs(uside(i)).ne.1)call bug('f','Bad sideband indicator')
	enddo
c
c  Generate an antenna mask.
c
	if(nants.eq.0)call bug('f','A reference antenna must be given')
	do i=1,ATANT
	  antmask(i) = .false.
	enddo
c
	do i=1,nants
	  if(ants(i).lt.1.or.ants(i).gt.ATANT)
     *	    call bug('f','Illegal antenna number')
	  antmask(ants(i)) = .true.
	enddo
c
c  Determine the reweighting coefficients, if necessary.
c
	if(dowt)call LagWt(wts,NWTS,0.04)
c
c  Get ready to copy the data.
c
	call uvopen(lVis,vis,'old')
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call uvprobvr(lVis,'xyphase',dtype,length,updated)
	dovar = dtype.eq.'r'
	if(.not.dovar.and.txt.eq.' ')then
	  call bug('w',
     *	  'Input does not contain xyphase variable, and ...')
	  call bug('f','Xyphase text file not given')
	endif
	call uvprobvr(lVis,'corr',dtype,length,updated)
	call uvopen(lOut,out,'new')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	if(dtype.eq.' ')call bug('f','Did not find corr data in input')
	call uvset(lOut,'corr',dtype,0,0.,0.,0.)
	call InitCpy(lVis,vCopy)
	call InitFreq(lVis,vFreq)
c
c  Make the output history.
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'ATXY: Miriad '//version)
	call hisinput(lOut,'ATXY')
	call hisclose(lOut)
c
c  Handle the case of a constant number of polarisations -- the normal case.
c
	call rdhdi(lVis,'npol',npol,0)
	if(npol.ne.0)call wrhdi(lOut,'npol',npol)
c
c  Generate break points, if required.
c
	if(doauto)call AutoBrk(lVis,break,nbreak,MAXBREAK)
c
c  Get the first time, determine the XY phases and generate the gains.
c
	call ReadXY(lVis,vFreq,dovar,txt,interval,gap,break,nbreak)
c
c  Determine whether we have to correct this visibility. If so, correct it.
c  First, though, check that our frequency information is up to date.
c
	call uvrewind(lVis)
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	call FreqUpd(lVis,vFreq,freq,dside,nschan,nspect,ATIF)
c
	ngood = 0
	nbad  = 0
	nrewt = 0
	nnowt = 0
	nsus  = 0
	ratio = 0
	dowhile(nchan.gt.0)
	  call BasAnt(preamble(5),i1,i2)
	  doxy1 = i1.gt.0.and.i1.le.ATANT
	  if(doxy1)doxy1 = antmask(i1)
	  doxy2 = i2.gt.0.and.i2.le.ATANT
	  if(doxy2)doxy2 = antmask(i2)
	  call uvrdvri(lVis,'pol',pol,PolXX)
c
	  if(dowt)then
	    call FreqUpd(lVis,vFreq,freq,dside,nschan,nspect,ATIF)
	    call Reweight(data,flags,nchan,nschan,nspect,wts,lags,NWTS,
     *	      nrewt,nnowt,nsus,ratio)
	  endif
c
	  if(pol.eq.PolXX.or.(pol.eq.PolYY.and..not.(doxy1.or.doxy2)))
     *								   then
	    ngood = ngood + nchan
	  else
	    if((doxy1.or.doxy2).and..not.dowt)
     *	      call FreqUpd(lVis,vFreq,freq,dside,nschan,nspect,ATIF)
	    if(nside.ge.nspect)then
	      call XYCorr(data,flags,nchan,freq,uside,nschan,nspect,
     *		  dovar,undo,preamble(4),i1,i2,doxy1,doxy2,pol,
     *		  ngood,nbad)
	    else
	      call XYCorr(data,flags,nchan,freq,dside,nschan,nspect,
     *		  dovar,undo,preamble(4),i1,i2,doxy1,doxy2,pol,
     *		  ngood,nbad)
	    endif
	  endif
c
c  Copy modified variables, and the data.
c
	  call uvVarCpy(vCopy,lOut)
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call output('Number of correlations XY phase corrected: '//
     *						      itoaf(ngood))
	if(nbad.gt.0)call bug('w',
     *	  'Correlations flagged for lack of xy phase: '//itoaf(nbad))
	if(nrewt.gt.0)then
	  call output('Number of spectra reweighted: '//itoaf(nrewt))
	  ratio = sqrt(ratio/nrewt)
	  write(line,'(a,1pe7.1)')'RMS Ratio of Lag(32)/Lag(0): ',ratio
	  call output(line)
	  if(ratio.gt.1e-4)
     *	    call bug('w','This lag ratio looks bad -- seek advice')
	endif
	if(nsus.gt.0)call bug('w',
     *	  'Number of suspect spectra flagged: '//itoaf(nsus))
	if(nnowt.gt.0)call bug('w',
     *	  'Spectra not reweighted (incorrect no. channels): '//
     *	  itoaf(nnowt))
c
c  Close up and die.
c
	call uvclose(lVis)
	call uvclose(lOut)
c
	end
c************************************************************************
	subroutine Reweight(data,flags,nchan,nschan,nspect,
     *	  wts,lags,nwts,nrewt,nnowt,nsus,ratio)
c
	implicit none
	integer nchan,nspect,nschan(nspect),nwts,nrewt,nnowt,nsus
	complex data(nchan)
	real wts(nwts),lags(nwts),ratio
	logical flags(nchan)
c
c  Fourier transform the data, multiply the data by the weight,
c  and Fourier transform back.
c------------------------------------------------------------------------
	integer i,j,n,nfreq
c
	nfreq = nwts/2 + 1
c
	n = 0
	do j=1,nspect
	  if(nschan(j).eq.nfreq)then
	    call fftcr(data(n+1),lags,-1,nwts)
	    if(abs(lags(1)).le.1000*abs(lags(nfreq)))then
	      nsus = nsus + 1
	      do i=1,nfreq
		flags(i+n) = .false.
	      enddo
	    else
	      ratio = ratio + (lags(nfreq)/lags(1))**2
	      do i=1,nwts
	        lags(i) = lags(i)*wts(i)/real(nwts)
	      enddo
	      call fftrc(lags,data(n+1),1,nwts)
	      nrewt = nrewt + 1
	    endif
	  else
	    nnowt = nnowt + 1
	  endif
	  n = n + nschan(j)
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(doauto,undo,dowt)
c
	implicit none
	logical doauto,undo,dowt
c
c  Get extra processing options.
c
c  Output:
c    doauto	True if srcbreak has been given.
c    undo	True if we are to apply the negative of the phase
c		correction.
c    dowt	True if we are to reweight the data.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	logical present(NOPTS)
	character opts(NOPTS)*8
	data opts/'srcbreak','undo    ','reweight'/
c
	call options('options',opts,present,NOPTS)
c
	doauto = present(1)
	undo   = present(2)
	dowt   = present(3)
	end
c************************************************************************
	subroutine InitCpy(lVis,vCopy)
c
	implicit none
	integer lVis,vCopy
c
c  Get the handle of the variables to be copied.
c
c------------------------------------------------------------------------
	integer NCOPY
	parameter(NCOPY=71)
	integer i
	character copy(NCOPY)*8
        data copy/    'airtemp ','antdiam ','antpos  ','atten   ',
     *     'axisrms ','chi     ','corbit  ','corbw   ','corfin  ',
     *     'cormode ','coropt  ','cortaper','ddec    ','dec     ',
     *     'dewpoint','dra     ','epoch   ','evector ','focus   ',
     *     'freq    ','freqif  ','inttime ','ivalued ','jyperk  ',
     *     'latitud ','longitu ','lo1     ','lo2     ','lst     ',
     *     'mount   ','nants   ','ntemp   ','ntpower ','obsdec  ',
     *     'observer','obsra   ','on      ','operator','pbfwhm  ',
     *     'phaselo1','phaselo2','phasem1 ','plangle ','plmaj   ',
     *     'plmin   ','pltb    ','precipmm','ra      ','relhumid',
     *     'source  ','telescop','temp    ','tpower  ','ut      ',
     *     'veldop  ','veltype ','version ','vsource ','winddir ',
     *     'windmph ','xyphase ','delay0  ','npol    ','pol     ',
     *	   'nspect  ','restfreq','ischan  ','nschan  ','sfreq   ',
     *	   'sdf     ','systemp '/
c
c  Determine the things to be checked and the things to be copied.
c
	call uvVarIni(lVis,vCopy)
	do i=1,NCOPY
	  call uvVarSet(vCopy,Copy(i))
	enddo
c
	end
c************************************************************************
	subroutine InitFreq(lVis,vFreq)
c
	implicit none
	integer lVis,vFreq
c
c  Get the handle of the variables to alert us to a change in the freq
c  setup.
c
c------------------------------------------------------------------------
	integer NFREQ
	parameter(NFREQ=4)
	integer i
	character freq(NFREQ)*8
        data freq/'nspect  ','nschan  ','sfreq   ','sdf     '/
c
c  Determine the things to be checked.
c
	call uvVarIni(lVis,vFreq)
	do i=1,NFREQ
	  call uvVarSet(vFreq,Freq(i))
	enddo
c
	end
c************************************************************************
	subroutine FreqUpd(lVis,vFreq,freq,side,nschan,nspect,mspect)
c
	implicit none
	integer lVis,vFreq
	integer nspect,mspect,side(mspect),nschan(mspect)
	double precision freq(mspect)
c
c  Determine the frequency parameters.
c
c------------------------------------------------------------------------
	integer ATIF
	parameter(ATIF=2)
	integer i
	double precision sdf(ATIF)
c
c  Externals.
c
	logical uvVarUpd
c
	if(.not.uvVarUpd(vFreq))return
c
c  Get the frequency description.
c
	call uvrdvri(lVis,'nspect',nspect,1)
	if(nspect.gt.min(mspect,ATIF))
     *		call bug('f','Too many spectral windows')
	call uvgetvrd(lVis,'sfreq',freq,nspect)
	call uvgetvrd(lVis,'sdf',sdf,nspect)
	call uvgetvri(lVis,'nschan',nschan,nspect)
c
c  Determine sideband indicator.
c
	do i=1,nspect
	  freq(i) = freq(i) + sdf(i)*(nschan(i)/2)
	  side(i) = nint(sign(1.d0,sdf(i)))
	enddo
c
	end
c************************************************************************
	subroutine XYCorr(data,flags,nchan,freq,side,
     *		  nschan,nspect,domir,undo,time,i1,i2,
     *		  doxy1,doxy2,pol,ngood,nbad)
c
	implicit none
	integer nchan,nspect,nschan(nspect),i1,i2,pol,ngood,nbad
	integer side(nspect)
	double precision freq(nspect),time
	complex data(nchan)
	logical flags(nchan),doxy1,doxy2,domir,undo
c
c  Correct for the XY phase.
c
c  Input:
c    nchan	 Number of channels.
c    side	 Sideband indicator.
c    freq        Frequency (GHz) of central channel.
c    nschan	 Number of channels in each IF.
c    nspect	 Number of IFs.
c    domir	 XY phases are in Miriad convention.
c    time	 Observation time
c    i1,i2	 Antenna numbers.
c    pol	 Polarisation type.
c    doxy1,doxy2 Flags to indicate which xy phases to correct for.
c  Input/Output:
c    ngood,nbad	 Count of the number of good and bad cases.
c    data	 The data. Uncorrected on input, corrected on output.
c    flags	 The data flags.
c------------------------------------------------------------------------
	integer PolXX,PolYY,PolXY,PolYX
	parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
	integer i,j,n
	complex fac,xy1,xy2,xy0
	logical flag1,flag2,flag
c
	call XYdef(domir,xy0)
c
	n = 0
	do j=1,nspect
c
c  Get the XY phase. The default XY phase is -1.
c
	  flag1 = .true.
	  xy1 = xy0
	  if(doxy1.and.(pol.eq.PolYX.or.pol.eq.PolYY))
     *	      call XYGet(domir,time,freq(j),side(j),i1,xy1,flag1)
c
	  flag2 = .true.
	  xy2 = xy0
	  if(doxy2.and.(pol.eq.PolXY.or.pol.eq.PolYY))
     *	      call XYGet(domir,time,freq(j),side(j),i2,xy2,flag2)
c
	  flag = flag1.and.flag2
	  if(pol.eq.PolXX)then
	    fac = 1
	  else if(pol.eq.PolYY)then
	    fac = 1/(xy1 * conjg(xy2))
	  else if(pol.eq.PolXY)then
	    fac = 1/conjg(xy2)
	  else if(pol.eq.PolYX)then
	    fac = 1/xy1
	  endif
	  if(undo) fac = conjg(fac)
c
c  Correct the data.
c
	  do i=n+1,n+nschan(j)
	    data(i) = fac * data(i)
	    flags(i) = flag.and.flags(i)
	  enddo
	  n = n + nschan(j)
	  if(flag)then
	    ngood = ngood + nschan(j)
	  else
	    nbad  = nbad + nschan(j)
	  endif
	enddo
	end
c************************************************************************
	subroutine AutoBrk(lVis,break,nbreak,MAXBREAK)
c
	implicit none
	integer lVis,nbreak,MAXBREAK
	double precision break(MAXBREAK)
c
c  Generate a break point every time the source changes.
c
c  Input:
c    lVis	Handle of the input vis dataset.
c    MAXBREAK	Maximum number of break points.
c  Input/Output:
c    break	On input, the user specified break points.
c		On output, the automatically generated ones are
c		added.
c    nbreak	Number of break points.
c------------------------------------------------------------------------
	logical ok
	character src1*32,src2*32
	double precision time
c
c  Externals.
c
	integer uvscan
c
	call uvrewind(lVis)	
c
	if(uvscan(lVis,'source').ne.0)then
	  call bug('w','No source name found ... srcbreak ignored')
	else
	  call uvrdvra(lVis,'source',src1,' ')
	  ok = .true.
	  dowhile(uvscan(lVis,'source').eq.0.and.ok)
	    call uvrdvra(lVis,'source',src2,' ')
	    if(src1.ne.src2)then
	      if(nbreak.eq.MAXBREAK)then
		ok = .false.
	      else
		src1 = src2
		call uvrdvrd(lVis,'time',time,0.d0)
		nbreak = nbreak + 1
		break(nbreak) = time - 1./86400.
	      endif
	    endif
	  enddo
	  if(.not.ok)call bug('w',
     *		'Too many break points ... some discarded')
	endif
c
	end
c************************************************************************
	subroutine ReadXY(lVis,vFreq,dovar,txt,
     *				interval,gap,break,nbreak)
c
	implicit none
	integer nbreak,vFreq,lVis
	real interval,gap
	logical dovar
	double precision break(*)
	character txt*(*)
c
c  Load the XY phase data from either the text file or the XYPHASE variable.
c  Depending where they come from, they are either in AIPS or MIRIAD
c  convention.
c
c  Inputs:
c    dovar	Get the XY phases from the dataset.
c------------------------------------------------------------------------
	integer MAXPNTS,MAXFREQ,ATANT,ATIF
	parameter(MAXFREQ=32,MAXPNTS=10000,ATANT=6,ATIF=2)
	real phi(MAXPNTS),buf(MAXPNTS)
	double precision freqs(MAXFREQ)
	double precision Tfirst,Tend,Tlast,T0,f,T,SumT
	integer antfreq(MAXPNTS),nfreq,npnts,indx(MAXPNTS)
	integer indx2(MAXFREQ),lu,freqid,i,iostat
	logical dobreak
c
	integer ispect,nspect
	logical flag(ATANT,ATIF)
	real     xyp(ATANT,ATIF)
	double precision freq(ATIF)
c
c  Externals.
c
	integer uvscan
c
	call XYinit
	if(.not.dovar)then
	  call txtopen(lu,txt,'old',iostat)
	  if(iostat.ne.0)then
	    call bug('f','Error opening xyphase text file')
	    call bugno('f',iostat)
	  endif
	else
	  lu = lVis
	endif
c
	call uvrewind(lVis)
	T0 = 0
	call GetXYP(dovar,lu,vFreq,T,freq,xyp,flag,ATANT,ATIF,nspect)
	if(nspect.eq.0)call bug('f','Failed to get any XY phase data')
c
	if(.not.dovar)then
	  if(uvscan(lVis,'time').ne.0)call bug('f','Screwy error')
	  call uvgetvrd(lVis,'time',T0,1)
	  T0 = nint(T0 - T - 0.5d0) + 0.5d0
	endif
	T = T + T0
	Tend = T - 2*interval
	Tfirst = Tend
	Tlast = Tend
	dobreak = .false.
	npnts = 0
c
	dowhile(nspect.gt.0)
	  if(T.lt.Tfirst)call bug('f','XY phases out of time order')
c
c  Check for the end of a solution interval. If so, determine the
c  solution, and determine when the next solution interval ends.
c
	  if(T.gt.Tend.or.T.gt.Tlast+gap.or.
     *	     npnts+nspect*ATANT.gt.MAXPNTS)then
	    if(npnts.gt.0)
     *	      call XySol(antfreq,phi,buf,indx,indx2,npnts,
     *					SumT/npnts,freqs,nfreq)
	    if(dobreak.and.T.gt.Tend)call XyBreak(Tend)
	    npnts = 0
	    nfreq = 0
	    SumT = 0
	    Tfirst = T
	    call NextTend(Tend,dobreak,T,interval,break,nbreak)
	  endif
c
c  Determine the appropriate frequency id.
c
	  do ispect=1,nspect
	    f = freq(ispect)
	    freqid = 0
	    do i=1,nfreq
	      if(f.eq.freqs(i))freqid = i
	    enddo
	    if(freqid.eq.0)then
	      if(nfreq.eq.MAXFREQ)call bug('f','Freq table overflow')
	      nfreq = nfreq + 1
	      freqs(nfreq) = f
	      freqid = nfreq
	    endif
c
c  Add these XY phase points to the buffer.
c
	    do i=1,ATANT
	      if(flag(i,ispect))then
	        npnts = npnts + 1
	        antfreq(npnts) = ATANT*freqid + i - 1
	        phi(npnts) = xyp(i,ispect)
	        SumT = SumT + T
	      endif
	    enddo
	  enddo
c
c  Loop the loop.
c
	  Tlast = T
	  call GetXYP(dovar,lu,vFreq,T,freq,xyp,flag,ATANT,ATIF,nspect)
	  T = T + T0
	enddo
c
c  Flush out the last solution.
c
	if(npnts.gt.0)
     *	      call XySol(antfreq,phi,buf,indx,indx2,npnts,
     *					SumT/npnts,freqs,nfreq)

	if(.not.dovar)call txtclose(lu,iostat)
c
	call XYDone
c
	end
c************************************************************************
	subroutine GetXYP(dovar,lu,vFreq,T,freq,xyp,flag,nants,mspect,
     *								nspect)
c
	implicit none
	logical dovar
	integer lu,vFreq,nants,mspect,nspect
	double precision T,freq(mspect)
	real xyp(nants,mspect)
	logical flag(nants,mspect)
c
c  Read multiple records of XY phase data.
c
c  Output:
c    xyp	XY phase, in radians (either Miriad or AIPS convention).
c    flag	Flags for the XY phase variable.
c------------------------------------------------------------------------
	integer ATIF,ATANT
	parameter(ATIF=2,ATANT=6)
	include 'mirconst.h'
	real buf(ATIF*ATANT+2)
	integer side(ATIF),nschan(ATIF)
	integer i,j,iostat
c
c  Externals.
c
	integer uvscan
c
	if(nants.ne.ATANT)call bug('f','Inconsistent number antennas')
c
c  Get the XY phases from the XY phase variable.
c
	if(dovar)then
	  if(uvscan(lu,'xyphase').eq.0)then
	    call uvgetvrd(lu,'time',T,1)
	    call FreqUpd(lu,vFreq,freq,side,nschan,nspect,ATIF)
	    if(nspect.gt.min(ATIF,mspect))
     *	      call bug('f','Buffer overflow you know where')
	    call uvgetvrr(lu,'xyphase',xyp,nants*nspect)
	    do j=1,nspect
	      do i=1,nants
		flag(i,j) = .true.
	      enddo
	    enddo
	  else
	    nspect = 0
	  endif
c
c  Get the XY phases from the XY phase text file.
c
	else
	  call getrec(lu,buf,2+2*nants,iostat)
	  if(iostat.eq.0)then
	    nspect = 1
	    T = buf(1)
	    freq(1) = 0.001 * buf(2)
	    j = 3
	    do i=1,nants
	      xyp(i,1) = pi/180. * buf(j)
	      flag(i,1) = buf(j+1).eq.0
	      j = j + 2
	    enddo
	  else if(iostat.ne.-1)then
	    call bug('w','Error reading xyphase text file')
	    call bugno('f',iostat)
	  else
	    nspect = 0
	  endif
	endif
c
	end
c************************************************************************
	subroutine getrec(lTxt,vals,nvals,iostat)
c
	implicit none
	integer nvals,lTxt,iostat
	real vals(nvals)
c
c  Decode a string of doubles.
c------------------------------------------------------------------------
	character line*132,token*32
	double precision t
	integer k1,k2,length,i
	logical ok
c
	k1 = 1
	call txtread(lTxt,line,k2,iostat)
	if(iostat.ne.0)return
c
	do i=1,nvals
	  call getfield(line,k1,k2,token,length)
	  if(length.le.0)call bug('f','Line too short')
	  call atodf(token(1:length),t,ok)
	  if(.not.ok)call bug('f','Error decoding line')
	  vals(i) = t
	enddo
c
	end
c************************************************************************
	subroutine NextTend(Tend,dobreak,Tfirst,interval,break,nbreak)
c
	implicit none
	integer nbreak
	double precision Tend,Tfirst,break(*)
	real interval
	logical dobreak
c
c  Determine the end of the next solution interval.
c
c  Input:
c    Tfirst	The first time in the solution interval.
c    interval	Maximum length of a solution interval.
c    break	Break points.
c    nbreak	Number of break points.
c  Output:
c    Tend	The end of the solution interval.
c    dobreak	True if the end is the result of a break point.
c
c------------------------------------------------------------------------
	double precision tf0,te0
	integer i
c
	Tend = Tfirst + interval
c
        tf0 = mod(tfirst - 0.5d0,1.d0)
        te0 = mod(tend   - 0.5d0,1.d0)
        if(te0.lt.tf0)te0 = te0 + 1   
        dobreak = .false.  
        do i=1,nbreak
          if(tf0.lt.break(i).and.break(i).le.te0)then
            tend = tend - (te0 - break(i))
            dobreak = .true.
          else if(tfirst.lt.break(i).and.break(i).le.tend)then
            tend = break(i)   
            dobreak = .true.
          endif
        enddo
	end
c************************************************************************
	subroutine XYinit
c
	implicit none
c
c------------------------------------------------------------------------
	include 'atxy.h'
	nsol = 0
	ntime = 0
	end
c************************************************************************
	subroutine XYBreak(T)
c
	implicit none
	double precision T
c
c  Insert a break point into the XY phase table.
c
c------------------------------------------------------------------------
	include 'atxy.h'
	if(ntime.ge.MAXTIME)call bug('f','Too many times')
	if(ntime.gt.0)then
	  if(T.le.time(ntime))call bug('f','Times out of order')
	endif
c
	ntime = ntime + 1
	time(ntime) = T
	idx(ntime) = 0
	nidx(ntime) = 0
c
	end
c************************************************************************
	subroutine XYDone
c
	implicit none
c
c  Determine an average XY phase solution.
c
c------------------------------------------------------------------------
	include 'atxy.h'
	include 'mirconst.h'
c
	integer i,j,pos,neg
c
	pos = 0
	neg = 0
	do j=1,nsol
	  do i=1,ATANT
	    if(real(xyphase(i,j)).gt.0)then
	      pos = pos + 1
	    else if(real(xyphase(i,j)).lt.0)then
	      neg = neg + 1
	    endif
	  enddo
	enddo
c
	if(pos.gt.neg)then
	  axyphase = 1
	else
	  axyphase = -1
	endif
c
	end
c************************************************************************
	subroutine XySol(antfreq,phi,buf,indx1,indx2,
     *				npnts,T,freqs,nfreqs)
c
	implicit none
	integer npnts,nfreqs
	integer antfreq(npnts),indx1(npnts),indx2(nfreqs)
	real phi(npnts),buf(npnts)
	double precision T,freqs(nfreqs)
c
c  Determine the median XY phase.
c
c  Input:
c    npnts	Number of xy phase points.
c    antfreq	Antenna/frequency tags.
c    phi	XY phase measurement (in degrees, in AIPS convention).
c    T		Mean time of the interval.
c    freqs	Frequency table.
c    nfreqs	Number of frequencies.
c  Scratch:
c    buf
c    indx1
c    indx2
c------------------------------------------------------------------------
	include 'atxy.h'
	integer i,j,freqid,ant,off,n
	real rp,ip
	complex ctemp
c
	if(nfreqs.gt.npnts.or.nfreqs.eq.0.or.npnts.eq.0)
     *	  call bug('f','Illegal solution parameters')
c
c  Add another time.
c
	if(ntime+1.gt.MAXTIME)call bug('f','Too many times')
	if(nsol+nfreqs.gt.MAXSOL)call bug('f','Too many solutions')
	ntime = ntime + 1
	time(ntime) = T
	idx(ntime) = nsol + 1
	nidx(ntime) = nfreqs
c
c  Make space for nfreqs more solutions.
c
	do j=1,nfreqs
	  do i=1,ATANT
	    xyphase(i,j+nsol) = 0
	  enddo
	enddo
c
c  Determine the mapping from freqid number to the gain slot.
c  Write the frequency for each gain slot.
c
	call hsortd(nfreqs,freqs,indx1)
	do i=1,nfreqs
	  indx2(indx1(i)) = i
	enddo
	do i=1,nfreqs
	  freq(nsol+i) = freqs(indx2(i))
	enddo
c
c  Sort according to the antfreq array.
c
	call hsorti(npnts,antfreq,indx1)
c
	off = 0
	dowhile(off.lt.npnts)
	  ant = antfreq(indx1(off+1))
	  freqid = ant/ATANT
	  ant = ant - ATANT*freqid + 1
c
c  Determine the number of points for this freqid/antenna pair.
c
	  call search(indx1(off+1),npnts-off,antfreq,npnts,n)
c
c  Find the median of the real and imaginary parts.
c
	  do i=1,n
	    buf(i) = cos(phi(indx1(i+off)))
	  enddo
	  call median(buf,n,rp)
	  do i=1,n
	    buf(i) = sin(phi(indx1(i+off)))
	  enddo
	  call median(buf,n,ip)
c
c  We have the xy phase!!
c
	  ctemp = cmplx(rp,ip)
	  ctemp = ctemp / abs(ctemp)
	  xyphase(ant,indx2(freqid)+nsol) = ctemp
c
	  off = off + n
	enddo
c
	nsol = nsol + nfreqs
	end
c************************************************************************
	subroutine search(indx,n,antfreq,npnts,nout)
c
	implicit none
	integer n,npnts,indx(n),antfreq(npnts),nout
c
c------------------------------------------------------------------------
	integer itemp
	logical more
c
	itemp = antfreq(indx(1))
c
	more = .true.
	nout = 1
	dowhile(nout.lt.n.and.more)
	  more = antfreq(indx(nout+1)).eq.itemp
	  if(more)nout = nout + 1
	enddo
c
	end
c************************************************************************
	subroutine XYDef(domir,xyp)
c
	implicit none
	logical domir
	complex xyp
c
c  Return the default XY phase for a particular sideband.
c------------------------------------------------------------------------
	include 'atxy.h'
c
c  Convert from AIPS convention to Miriad convention.
c
	if(domir)then
	  xyp = axyphase
	else
	  xyp = -axyphase
	endif
c
	end
c************************************************************************
	subroutine XYGet(domir,t,f,side,ant,xyp,flag)
c
	implicit none
	double precision t,f
	integer ant,side
	complex xyp
	logical flag,domir
c
c  Determine the XY phase of an antenna at a particular time.
c
c  Input:
c    t		Time (Julian day).
c    f		Frequency (GHz).
c    side	Sideband indicator.
c    ant	Antenna number.
c    domir	XY phase is in Miriad convention.
c  Output:
c    xyp	The xyphase factor (Miriad convention).
c    flag	True if xyp is good.
c------------------------------------------------------------------------
	include 'atxy.h'
	integer t1,t2,f1,f2
c
c  Get the solution times bracketting the time we are interested in.
c
	call GetT(t,time,ntime,nidx,t1,t2)
c
c  Which is the most appropriate time?
c
	if(t1.eq.0)then
	  f1 = 0
	else if(t2.eq.0)then
	  call GetF(f,freq(idx(t1)),nidx(t1),f1)
	  if(f1.gt.0)f1 = f1 + idx(t1) - 1
	else
	  call GetF(f,freq(idx(t1)),nidx(t1),f1)
	  if(f1.gt.0)f1 = f1 + idx(t1) - 1
	  call GetF(f,freq(idx(t2)),nidx(t2),f2)
	  if(f2.gt.0)f2 = f2 + idx(t2) - 1
	  if(f1.eq.0)then
	    f1 = f2
	  else if(f2.ne.0)then
	    if((abs(f-freq(f1)).gt.abs(f-freq(f2))).or.
     *	       (abs(f-freq(f1)).eq.abs(f-freq(f2)).and.
     *		abs(t-time(t1)).gt.abs(t-time(t2))))f1 = f2
	  endif
	endif
c
c  Convert from AIPS convention to Miriad convention.
c
	xyp = 0
	if(f1.gt.0)xyp = xyphase(ant,f1)
	if(domir)then
	  continue
	else if(side.lt.0)then
	  xyp = -conjg(xyp)
	else
	  xyp = -xyp
	endif
	flag = abs(real(xyp))+abs(aimag(xyp)).gt.0
	if(.not.flag)xyp = 1
c
	end
c************************************************************************
	subroutine GetT(t,time,ntime,nidx,t1,t2)
c
	implicit none
	integer ntime,nidx(ntime),t1,t2
	double precision t,time(ntime)
c
c  Determine the solution intervals bracketting the time we are
c  interested in.
c
c  Input:
c    ntime
c    nidx
c    t
c    time
c  Output:
c    t1,t2
c------------------------------------------------------------------------
	integer j
c
c  Check for the case of a time at the beginning of the end.
c
	if(T.lt.time(1).or.ntime.eq.1)then
	  t1 = 1
	  t2 = 0
	else if(T.ge.time(ntime))then
	  t1 = ntime
	  t2 = 0
c
c  Check for a time in the middle of the observation.
c
	else
	  t1 = 1
	  t2 = ntime
	  dowhile(t1+1.lt.t2)
	    j = (t1+t2)/2
	    if(t.ge.time(j))then
	      t1 = j
	    else
	      t2 = j
	    endif
	  enddo
	  if(nidx(t2).eq.0)t2 = 0
	endif
c
c  Check for break points.
c
	if(nidx(t1).eq.0)then
	  t1 = t2
	  t2 = 0
	endif
c
	end
c************************************************************************
	subroutine GetF(f,freq,nfreq,indx)
c
	implicit none
	integer nfreq,indx
	double precision f,freq(nfreq)
c
c  Determine the frequency which is closest to the observing frequency.
c
c  Input:
c    f		Observing frequency.
c    freq	List of solution frequencies.
c    nfreq	Number of solution frequencies.
c  Output:
c    indx	Index of the best match.
c------------------------------------------------------------------------
	integer i
c
	indx = 1
	do i=2,nfreq
	  if(abs(f-freq(i)).lt.abs(f-freq(indx)))indx = i
	enddo
c
	end
