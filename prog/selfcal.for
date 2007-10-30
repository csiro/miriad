c************************************************************************
	program selfcal
	implicit none
c
c= selfcal - Determine self-calibration of calibration gains.
c& mchw
c: calibration, map making
c+
c	SELFCAL is a MIRIAD task to perform self-calibration of visibility data.
c	Either phase only or amplitude and phase calibration can be performed.
c	The input to SELCAL are a visibility data file, and model images.
c	This program then calculates the visibilities corresponding to the
c	model, accumulates the statistics needed to determine the antennae
c	solutions, and then calculates the self-cal solutions.
c
c	The output is a calibration file, ready to be applied to the
c	visibility data.
c
c@ vis
c	Name of input visibility data file. No default.
c@ select
c	Standard uv data selection criteria. Generally this should not include
c	a "dra" and "ddec" selection, as SELFCAL automatically matches data
c	with the appropriate observing center.
c@ model
c	Name of the input models. Several models can be given, which can
c	cover different channel ranges, different pointing and sources, and
c	different polarizations of the input visibility data. Generally
c	the model should be derived (by mapping and deconvolution) from the
c	input visibility file, so that the channels in the model correspond
c	to channels in the visibility file. Though the maps can be made using
c	any linetype, generally "channel" linetype will give best results (??).
c	The units of the model MUST be JY/PIXEL, rather than JY/BEAM, and
c	should be weighted by the primary beam. The task DEMOS can be used
c	to extract primary beam weighted models from a mosaiced image. If
c	no models are given, a point source model is assumed.
c
c	NOTE: When you give SELFCAL a model, it will, by default, select the
c	data associated with this model from the visibility data-set. This
c	includes selecting the appropriate range of channels, the appropriate
c	polarisation type and the appropriate pointing (if options=selradec
c	is used). If you use a point source model, if it YOUR responsibility
c	to select the appropriate data. In particular, you may well want
c	to select appropriate polarisations and sourcings or sources.
c@ clip
c	Clip level. For models of intensity, any pixels below the clip level
c	are set to zero. For models of Stokes Q,U,V, or MFS I*alpha models,
c	any pixels whose absolute value is below the clip level are set
c	to zero. Default is 0.
c@ interval
c	The length of time, in minutes, of a gain solution. Default is 5,
c	but use a larger value in cases of poor signal to noise, or
c	if the atmosphere and instrument is fairly stable.
c@ options
c       This gives several processing options. Possible values are:
c	  amplitude  Perform amplitude and phase self-cal.
c	  phase      Perform phase only self-cal.
c	  smooth     Determine the solutions in such a way that they are
c	             smooth with time.
c	  polarized  The source is polarised. By default the source is
c	             assumed to be unpolarised. For a polarized source,
c	             SELFCAL cannot perform polarization conversion. That
c	             is, if the model is of a particular polarization, then
c	             the visibility file should contain that sort of
c	             polarization. For example, if the model is Stokes-Q,
c	             then the visibility file should contain Stokes-Q.
c	  mfs        This is used if there is a single plane in the input
c	             model, which is assumed to represent the image at all
c	             frequencies. This should also be used if the model has
c	             been derived from MFCLEAN.
c	  relax      Relax the convergence criteria. This is useful when
c	             selfcal'ing with a very poor model.
c	  apriori    This is used if there is no input model, and the
c	             source in the visibility data is either a planet,
c	             or a standard calibrator. This causes the model data
c	             to be scaled by the known flux of the source. For a
c	             planet, this flux will be a function of baseline. If
c	             the source is a point source, the ``apriori'' option
c	             is only useful if the ``amplitude'' and ``noscale''
c	             option are being used. For a planet, this option
c	             should also be used for a phase selfcal, to get the
c	             correct weighting of the different baselines in the
c	             solution.
c	  noscale    Do not scale the gains. By default the gains are scaled
c	             so that the rms gain amplitude is 1. Generally this
c	             option should be used with the apriori option.
c	             It must be used if selfcal is being used to determine
c	             Jy/K, and should also be used if the model is believed
c	             to have the correct scale.
c	  selradec   This causes SELFCAL to select only those visibilities
c	             whose observing center is within plus or minus three
c	             pixels of the model reference pixel. This is needed
c	             if there are multiple pointings or multiple sources in
c	             the input uv file. By default no observing center
c	             selection is performed.
c	Note that "amplitude" and "phase" are mutually exclusive.
c	The default is options=phase.
c@ minants
c	Data at a given solution interval is deleted  if there are fewer than
c	MinAnts antennae operative during the solution interval. The default
c	is 3 for options=phase and 4 for options=amplitude.
c@ refant
c	This sets the reference antenna, which is given a phase angle of zero.
c	The default, for a given solution interval, is the antennae with the
c	greatest weight.
c@ flux
c	If MODEL is blank, then the flux (Jy) of a point source model can
c	be specified here. Also used as the default flux for the apriori
c	option. The default is 1 (assuming the model parameter is not given)
c@ offset
c	This gives the offset in arcseconds of a point source model (the
c	offset is positive to the north and to the east). This parameter is
c	used if the MODEL parameter is blank. The default is 0,0. The
c	amplitude of the point source is chosen so that flux in the model
c	is the same as the visibility flux.
c@ line
c	The visibility linetype to use, in the standard form, viz:
c	  type,nchan,start,width,step
c	Generally if there is an input model, this parameter defaults to the
c	linetype parameters used to construct the map. If you wish to override
c	this, or if the info is not in the header, or if you are using
c	a point source model, this parameter can be useful.
c--
c
c  History:
c    rjs  13mar90 Original version.
c    rjs  23mar90 Mods to gains file. Eliminated write statements. Check
c		  that the minimum number of antennae are there.
c    rjs  28mar90 Fixed apriori and noscale options. Get it to recalculate
c		  channel bandwidth after every model.
c    rjs  29mar90 Fixed bug in the scaling of SumVM.
c    rjs  31mar90 Increased maxchan.
c    rjs  24apr90 An added error check.
c    rjs  27apr90 Corrected bug in checking for convergence in amp selfcal.
c		  Mildly improved amp selfcal.
c    pjt   2may90 maxchan in selfacc1 now through maxdim.h
c    rjs  16oct90 Check that the vis file is cross-correlation data.
c   mchw  24nov90 Corrected comment and protected sqrt in solve1.
c    rjs  26nov90 Minor changes to the phase-solution routine, to make it
c		  more persistent.
c    mjs  25feb91 Changed references of itoa to itoaf.
c    rjs  25mar91 Added `relax' option. Minor changes to appease flint.
c    rjs   5apr91 Trivial change to get ModelIni to do some polarisation
c		  handling.
c    mjs  04aug91 Replaced local maxants/maxbl to use maxdim.h values
c    rjs  15oct91 Increased hash table size. Extra messages.
c    rjs   1nov91 Polarized and mfs options. Removed out.
c    rjs  29jan92 New call sequence to "Model".
c    rjs   3apr92 Use memalloc routines.
c    rjs  26apr92 Handle clip level better.
c    rjs   1may92 Added nfeeds keyword to output gains header.
c    rjs  17may92 More fiddles to the way clipping is handled.
c    rjs  23jun92 Changes to doc and messages (centre=>center, etc).
c    rjs  22nov92 Added ntau keyword to output gains header.
c    mchw 20apr93 Added flux keyword and option selradec.
c    rjs  29mar93 Fiddle noise calculation. BASANT changes.
c    rjs  18may93 If rms=0, assume rms=1.
c    rjs  19may93 Merge mchw and rjs versions of selfcal.
c    rjs  28jun93 Iterate a bit longer. Better memory allocation.
c    rjs  31aug93 Better amplitude calibration with low S/N data.
c    rjs  24sep93 Doc changes only.
c    rjs   9nov93 Better recording of time of a particular solution interval.
c  Bugs/Shortcomings:
c   * Selfcal should check that the user is not mixing different
c     polarisations and pointings.
c   * It would be desirable to apply bandpasses, and merge gain tables,
c     apply polarisation calibration, etc.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Selfcal: version 1.0 9-Nov-93')
	integer MaxMod,maxsels,nhead
	parameter(MaxMod=32,maxsels=256,nhead=3)
c
	character Models(MaxMod)*64,vis*64,ltype*32
	character flag1*8,flag2*8,obstype*32
	integer tvis,tmod,tscr
	integer nModel,minants,refant,nsize(3),nchan,nvis,i
	real sels(maxsels),clip,interval,offset(2),lstart,lwidth,lstep
	logical phase,amp,smooth,doline,apriori,noscale,relax,doPol,mfs
	real flux
	logical selradec
c
c  Externals.
c
	logical keyprsnt
	external header,calget
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keyf('vis',vis,' ')
	call SelInput('select',sels,maxsels)
	call mkeyf('model',Models,MaxMod,nModel)
	call keyr('clip',clip,0.)
	call keyr('interval',interval,5.)
	call keyi('minants',minants,0)
 	call keyi('refant',refant,0)
	call keyr('flux',flux,1.)
	call keyr('offset',offset(1),0.)
	call keyr('offset',offset(2),0.)
	doline = keyprsnt('line')
	if(doline)then
	  call keya('line',ltype,'channel')
	  call keyi('line',nchan,1)
	  call keyr('line',lstart,1.)
	  call keyr('line',lwidth,1.)
	  call keyr('line',lstep,lwidth)
	endif
	call GetOpt(phase,amp,smooth,apriori,noscale,relax,doPol,mfs,
     *		selradec)
	call keyfin
c
c  Check that the inputs make sense.
c
	if(vis.eq.' ') call bug('f','Input visibility file is missing')
	if(interval.le.0) call bug('f','Bad calibration interval')
	interval = interval / (24.*60.)
c
	if(MinAnts.eq.0)then
	  if(phase)then
	    Minants = 3
	  else if(amp)then
	    Minants = 4
	  endif
	endif
	if(MinAnts.lt.2)then
	  call bug('f','Bad value for the minants parameter.')
	else if(phase.and.MinAnts.lt.3)then
	  call bug('w','Phase selfcal with minants < 3 is unusual')
	else if(amp.and.MinAnts.lt.4)then
	  call bug('w','Amplitude selfcal with minants < 4 is unusual')
	endif
c
	if(nModel.eq.0)then
	  if(abs(offset(1))+abs(offset(2)).eq.0)then
	    call output(
     *		'Model is a point source at the observing center')
	  else
	    call output('Using a point source model')
	  endif
	endif
c
c  Open the visibility file, check that it is interferometer data, and set
c  the line type if necessary.
c
	call uvopen(tvis,vis,'old')
	call rdhda(tvis,'obstype',obstype,'crosscorrelation')
	if(obstype(1:5).ne.'cross')
     *	  call bug('f','The vis file is not cross correlation data')
	if(doline)call uvset(tvis,'data',ltype,nchan,lstart,lwidth,
     *								lstep)
c
c  Determine the flags to the MODELINI routine.
c  p -- Perform pointing selection.
c  s -- Perform polarisation selection.
c  l -- Set up line type.
c  t -- Source is polarised.
c
	flag1 = 's'
	if(selradec) flag1(2:2) = 'p'
	if(.not.doline.and..not.mfs)flag1(3:3) = 'l'
	if(doPol)		    flag1(4:4) = 't'
c
c  Determine the flags to the MODEL routine.
c  l - Perform clipping.
c  a - Perform auto-scaling.
c  m - Model is a mfs one.
c  c - Use calibration file to determine model characteristics.
c
	flag2 = 'l'
c	if(.not.noscale) flag2(2:2) = 'a'
	if(mfs)          flag2(3:3) = 'm'
	if(apriori)      flag2(4:4) = 'c'
c
c  Loop over all the models.
c
	if(nModel.eq.0)then
	  call output('Reading the visibility file ...')
	  call SelfSet(.true.,MinAnts) 
	  call SelApply(tvis,sels,.true.)
	  call Model(flag2,tvis,0,offset,flux,tscr,
     *				nhead,header,calget,nchan,nvis)
	  call SelfIni
	  call output('Accumulating statistics ...')
	  call SelfAcc(tscr,nchan,nvis,interval)
	  call scrclose(tscr)
	else
	  do i=1,nModel
	    call output('Calculating the model for '//Models(i))
	    call SelfSet(i.eq.1,MinAnts)
	    call xyopen(tmod,Models(i),'old',3,nsize)
	    call ModelIni(tmod,tvis,sels,flag1)
	    call Model(flag2,tvis,tmod,offset,Clip,tscr,
     *				nhead,header,calget,nchan,nvis)
	    call xyclose(tmod)
	    call output('Accumulating statistics ...')
	    if(i.eq.1) call SelfIni
	    call SelfAcc(tscr,nchan,nvis,interval)
	    call scrclose(tscr)
	  enddo
	endif
c
c  Open the output file to contain the gain solutions.
c
	call HisOpen(tvis,'append')
	call HisWrite(tvis,'SELFCAL: Miriad '//version)
	call HisInput(tvis,'SELFCAL')
c
c  Calculate the self-cal gains.
c
	call output('Finding the selfcal solutions ...')
	call Solve(tvis,phase,smooth,relax,noscale,refant,interval)
c
c  Close up.
c
	call SelfFin
	call HisClose(tvis)
	call uvclose(tvis)
	end
c************************************************************************
	subroutine GetOpt(phase,amp,smooth,apriori,noscale,relax,
     *						doPol,mfs,selradec)
c
	implicit none
	logical phase,amp,smooth,apriori,noscale,relax,doPol,mfs,
     *							    selradec
c
c  Determine extra processing options.
c
c  Output:
c    phase	If true, do phase self-cal.
c    amp	If true, do amplitude/phase self-cal.
c    smooth	If true, do some extra time averaging.
c    apriori	If true, model routine checks calibrator flux table
c		for an estimate of the calibrator flux.
c    noscale	Do not scale the model to conserve flux.
c    relax	Relax convergence criteria.
c    doPol	Source is polarized.
c    mfs	Model is frequency independent, or has been derived
c		from MFCLEAN.
c    selradec	Input uv file contains multiple pointings or multiple
c		sources.
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=9)
	character opts(nopt)*9
	logical present(nopt)
	data opts/'amplitude','phase    ','smooth   ',
     *		  'apriori  ','noscale  ','relax    ',
     *		  'polarized','mfs      ','selradec '/
	call options('options',opts,present,nopt)
	amp = present(1)
	phase = present(2)
	smooth = present(3)
	apriori = present(4)
	noscale = present(5)
	relax = present(6)
	doPol = present(7)
	mfs = present(8)
	selradec = present(9)
	if(amp.and.phase)
     *	  call bug('f','Cannot do both amp and phase self-cal')
	if(.not.(amp.or.phase)) phase = .true.
	end
c************************************************************************
	subroutine header(tvis,preamble,data,flags,nchan,
     *						accept,Out,nhead)
	implicit none
	integer tvis,nchan,nhead
	complex data(nchan)
	logical flags(nchan),accept
	real Out(nhead)
	double precision preamble(4)
c
c  This is a service routine called by the model subroutines. It is
c  called every time a visibility is read from the data file.
c
c  Input:
c    tvis	Handle of the visibility file.
c    nhead	The value of nhead
c    nchan	The number of channels.
c    preamble	Preamble returned by uvread.
c    data	A complex array of nchan elements, giving the correlation data.
c		Not used.
c    flags	The data flags. Not used.
c  Output:
c   out		The nhead values to save with the data. Three values are
c		returned:
c		  out(1) -- baseline number.
c		  out(2) -- time (days) relative to time0.
c		  out(3) -- estimated sigma**2.
c   accept	This determines whether the data is accepted or discarded.
c		It is always accepted unless the baseline number looks bad.
c------------------------------------------------------------------------
	include 'selfcal.h'
	integer i1,i2
	double precision rms
c
	if(first)then
	  call uvrdvri(tvis,'nants',nants,0)
	  if(nants.le.0)call bug('f',
     *	    'The data file does not contain the number of antennae')
	  if(nants.lt.MinAnts)call bug('f',
     *	    'Fewer than the minimum number of antennae are present')
	  time0 = int(preamble(3)) + 0.5
	  nbad = 0
	  first = .false.
	endif
c
c  Determine antenna numbers, to make sure they are OK.
c
	call basant(preamble(4),i1,i2)
	accept = i1.le.nants.and.i2.le.nants
c
c  If all looks OK, then calculate the theoretical rms, and store away
c  the information that we need.
c
	if(accept)then
	  out(1) = preamble(4)
	  out(2) = preamble(3) - time0
	  call uvinfo(tvis,'variance',rms)
	  if(rms.le.0)rms=1
	  out(3) = rms
	else
	  nbad = nbad + 1
	endif
	end
c************************************************************************
	subroutine SelfSet(firstd,MinAntsd)
c
	implicit none
	logical firstd
	integer MinAntsd
c------------------------------------------------------------------------
	include 'selfcal.h'
	first = firstd
	MinAnts = MinAntsd
	end
c************************************************************************
	subroutine SelfIni
	implicit none
c------------------------------------------------------------------------
	include 'selfcal.h'
	integer i,SolSize
c
c  Externals.
c
	integer prime,MemBuf
c
	nHash = prime(maxHash-1)
	do i=1,nHash+1
	  Hash(i) = 0
	enddo
c
c  Determine the indices into the buffer. These are offsets into the
c  one scratch buffer. This scatch buffer is used as 5 arrays, namely
c    complex SumVM(nBl,maxSol),Gains(nants,maxSol)
c    real SumVV(nBl,maxSol),SumMM(maxSol),Weight(nBl,maxSol)
c    real Count(maxSol)
c
	nBl = (nants*(nants-1))/2
	SolSize = 3 + 4*nBl + 2*nants
	maxSol = min(nHash,max(minSol,(MemBuf()-10)/SolSize))
	nSols = 0
	TotVis = 0
	call MemAlloc(pSumVM,maxSol*nBl,'c')
	call MemAlloc(pSumVV,maxSol*nBl,'r')
	call MemAlloc(pSumMM,maxSol,'r')
	call MemAlloc(pWeight,maxSol*nBl,'r')
	call MemAlloc(pCount,maxSol,'r')
	call MemAlloc(pGains,maxSol*nants,'c')
	call MemAlloc(prTime,maxSol,'r')
c
	end
c************************************************************************
	subroutine SelfFin
c
	implicit none
c
c  Release allocated memory.
c
c------------------------------------------------------------------------
	include 'selfcal.h'
	call MemFree(pSumVM,maxSol*nBl,'c')
	call MemFree(pSumVV,maxSol*nBl,'r')
	call MemFree(pSumMM,maxSol,'r')
	call MemFree(pWeight,maxSol*nBl,'r')
	call MemFree(pCount,maxSol,'r')
	call MemFree(pGains,maxSol*nants,'c')
	call MemFree(prTime,maxSol,'r')
	end
c************************************************************************
	subroutine SelfAcc(tscr,nchan,nvis,interval)
c
	implicit none
	integer tscr,nchan,nvis
	real interval
c
c  This calls the routine which does the real work in accumulating
c  the statistics about a model.
c
c  Input:
c    tscr	The handle of the scratch file, which contains the visibility
c		and model information.
c    nchan	The number of channels in the scratch file.
c    nvis	The number of visbilities in the scratch file.
c    Interval	The self-cal gain interval.
c------------------------------------------------------------------------
	include 'selfcal.h'
c
	TotVis = TotVis + nVis
	call SelfAcc1(tscr,nchan,nvis,nBl,maxSol,nSols,
     *	  nhash,Hash,Indx,interval,Time,
     *	  Memc(pSumVM),Memr(pSumVV),Memr(pSumMM),
     *	  Memr(pWeight),Memr(pCount),Memr(prTime))
	end
c************************************************************************
	subroutine SelfAcc1(tscr,nchan,nvis,nBl,maxSol,nSols,
     *	  nHash,Hash,Indx,interval,
     *	  Time,SumVM,SumVV,SumMM,Weight,Count,rTime)
c
	implicit none
	integer tscr,nchan,nvis,nBl,maxSol,nSols
	integer nHash,Hash(nHash+1),Indx(nHash)
	integer Time(maxSol)
	real interval
	complex SumVM(nBl,maxSol)
	real SumVV(nBl,maxSol),SumMM(maxSol),Weight(nBl,maxSol)
	real Count(maxSol),rTime(maxSol)

	include 'maxdim.h'
c
c  This reads through the scratch file which contains the visibility
c  and the model. It finds (via a hash table) the index of the slot
c  which is being used to store info for this time interval. It then
c  accumulates various statistics into the appropriate arrays. These
c  statistics are eventually used to determine the self-cal solutions.
c
c  Input:
c    tscr	Handle of the scratch file.
c    nchan	Number of channels.
c    nvis	Number of visibilities.
c    nants	Number of antennae.
c    nBl	Number of baselines = nants*(nants-1)/2.
c    maxSol	Max number of solution slots.
c    nSols	Actual number of solution slots being used.
c    nHash	Hash table size.
c    interval	Self-cal interval.
c  Input/Output:
c  In the following, t=time (within a solution interval), f=channels,
c		b=baseline number, a = antenna number.
c    Time	The integer time, nint((preamble(3)-time0)/interval).
c    Hash	Hash table, used to locate a solution interval.
c    Indx	Index from hash table to solution interval.
c    SumVM	Sum(over t,f)conjg(Model)*Vis/sigma**2. Varies with b.
c    SumVV	Sum(over t,f)|Model|**2/sigma**2. Varies with b.
c    SumMM	Sum(over t,f,b) |Vis|**2/sigma**2.
c    Weight	Sum(over t,f) 1/sigma**2. Varies with b.
c    Count	Sum(over t,f,b) 1.
c    rTime	Sum(over t,f,b) t.
c
c  The Visibility Records:
c    The scratch file consists of "nvis" records, each of size nhead+5*nchan
c    The first nhead=3 values are
c      baseline number
c      time
c      estimated sigma**2
c
c  The Hash Table:
c    The hash table is a cyclic buffer. Each element in the buffer potentially
c    holds info pertaining to one time interval. A value of 0 in Hash indicates
c    an unused slot, whereas a non-zero number indicates a used slot. This
c    non-zero number is 2*itime+1, which is always odd, and is unique to a
c    particular time.
c
c    The last entry in the hash table (element nHash+1) is always zero, so the
c    check for the end of the buffer can be placed outside the main loop.
c------------------------------------------------------------------------
	integer maxlen,nhead
	parameter(nhead=3,maxlen=5*maxchan+nhead)
	integer i,j,k,ihash,itime,bl,i1,i2,length
	real Out(maxlen),Wt
c
	if(nchan.gt.maxchan) call bug('f','Too many channels')
	length = nhead + 5*nchan
c
	do j=1,nvis
	  call scrread(tscr,Out,(j-1)*length,length)
	  itime = nint(Out(2)/interval)
	  ihash = 2*itime + 1
	  i = mod(itime,nHash)
	  if(i.le.0) i = i + nHash
c
c  Find this entry in the hash table.
c
	  dowhile(Hash(i).ne.0.and.Hash(i).ne.ihash)
	    i = i + 1
	  enddo
	  if(i.gt.nHash)then
	    i = 1
	    dowhile(Hash(i).ne.0.and.Hash(i).ne.ihash)
	      i = i + 1
	    enddo
	  endif
c
c  It was not in the hash table. Add a new entry.
c
	  if(Hash(i).eq.0)then
	    nSols = nSols + 1
	    if(nSols.ge.maxSol) call bug('f','Hash table overflow')
	    Hash(i) = ihash
	    Indx(i) = nSols
	    Time(nSols) = itime
	    do k=1,nBl
	      SumVM(k,nSols) = (0.,0.)
	      SumVV(k,nSols) = 0.
	      Weight(k,nSols) = 0.
	    enddo
	    SumMM(nSols) = 0
	    Count(nSols) = 0
	    rTime(nSols) = 0
	  endif
c
c  We have found the slot containing the info. Accumulate in the
c  info about this visibility record.
c
	  i = Indx(i)
	  wt = 0.5/Out(3)
	  call basant(dble(out(1)),i1,i2)
	  bl = (i2-1)*(i2-2)/2 + i1
	  do k=nhead+1,nhead+5*nchan,5
	    if(out(k+4).gt.0)then
	      SumVM(bl,i) = SumVM(bl,i) +
     *		Wt*cmplx(Out(k),Out(k+1))*cmplx(Out(k+2),-Out(k+3))
	      SumVV(bl,i) = SumVV(bl,i) + Wt*(Out(k+2)**2 + Out(k+3)**2)
	      SumMM(i) = SumMM(i) + Wt * (Out(k)**2 + Out(k+1)**2)
	      Weight(bl,i) = Weight(bl,i) + Wt
	      Count(i) = Count(i) + 1
	      rTime(i) = rTime(i) + Out(2)
	    endif
	  enddo
	enddo
	end
c************************************************************************
	subroutine Solve(tgains,phase,smooth,relax,noscale,refant,
     *							    interval)
c
	implicit none
	integer tgains
	logical phase,smooth,relax,noscale
	integer refant
	real interval
c
c  We have previously accumulated all the statistics that we nee. Now we
c  are ready to determine the self-cal solutions.
c
c------------------------------------------------------------------------
	include 'selfcal.h'
	character line*64
c
c  Externals.
c
	character itoaf*8
c
	if(nbad.ne.0) call bug('w',
     *	  'No. visibilities with bad baseline numbers = '//itoaf(nbad))
	line = 'Total number of visibilities processed: '//itoaf(TotVis)
	call HisWrite(tgains,'SELFCAL: '//line)
	call output(line)
	line = 'Total number of solution intervals: '//itoaf(nSols)
	call HisWrite(tgains,'SELFCAL: '//line)
	call output(line)
c
c  Determine all the gain solutions.
c
	call Solve1(tgains,nSols,nBl,nants,phase,smooth,relax,noscale,
     *	  minants,refant, Time0,interval,Time,Indx,
     *	  Memc(pSumVM),Memr(pSumVV),Memr(pSumMM),Memc(pGains),
     *	  Memr(pWeight),Memr(pCount),memr(prTime))
	end
c************************************************************************
	subroutine Solve1(tgains,nSols,nBl,nants,phase,smooth,relax,
     *	  noscale,minants,refant,Time0,interval,Time,TIndx,
     *	  SumVM,SumVV,SumMM,Gains,Weight,Count,rTime)
c
	implicit none
	integer tgains
	logical phase,smooth,relax,noscale
	integer nSols,nBl,nants,minants,refant
	integer Time(nSols),TIndx(nSols)
	complex SumVM(nBl,nSols),Gains(nants,nSols)
	real SumVV(nBl,nSols),SumMM(nSols),Weight(nBl,nSols)
	real Count(nSols),rTime(nSols),interval
	double precision Time0
c
c  This runs through all the accumulated data, and calculates the
c  selfcal solutions.
c
c  Input:
c    phase
c    smooth
c    relax
c    nSols
c    nBl
c    nants
c    minants
c    refant
c    time
c    Weight
c    Count
c    rTime
c    SumMM
c    SumVM
c    SumVV
c  Scratch:
c    TIndx
c    Gains
c------------------------------------------------------------------------
	include 'maxdim.h'
	logical Convrg
	integer k,k0,nbad,iostat,item,offset,header(2)
	double precision dtime
c
c  Externals.
c
	character itoaf*8
c
c  Sort the self-cal solutions into order of increasing time.
c
	call sortidxi(nSols,Time,TIndx)
c
c  Partially combine adjacent time slots, if desired.
c
	if(smooth) call SmthData(nSols,nBl,Time,TIndx,
     *	  SumVM,SumVV,SumMM,Weight,Count,rTime)
c
c  Now calculate the solutions.
c
	nbad = 0
	do k=1,nSols
	  call Solve2(nbl,nants,SumVM(1,k),SumVV(1,k),Weight(1,k),
     *	    phase,relax,minants,refant,Gains(1,k),Convrg)
	  if(.not.Convrg)then
	    nbad = nbad + 1
	    Count(k) = 0
	  endif
	enddo
c
c  Write out some info to wake the user up from his/her slumber.
c
	if(nbad.ne.0) call bug('w','Intervals with no solution: '//
     *							itoaf(nbad))
	if(nbad.eq.nsols) call bug('f','No solutions were found')
c
c  Scale the gains if needed.
c
	if(.not.(noscale.or.phase))call GainScal(Gains,nants,nSols)
c
c  Calculate statistics.
c
	call CalcStat(tgains,nsols,nbl,nants,SumVM,SumMM,SumVV,
     *	  Weight,Count,Gains)
c
c  Write the gains out to a gains table.
c
	call haccess(tgains,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	header(1) = 0
	header(2) = 0
	offset = 0
	call hwritei(item,header,offset,8,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	offset = 8
	do k=1,nSols
	  k0 = TIndx(k)
	  if(Count(k0).gt.0)then
	    dtime = rTime(k0) / Count(k0) + time0
	    call hwrited(item,dtime,offset,8,iostat)
	    offset = offset + 8
	    call GFudge(gains(1,k0),nants)
	    if(iostat.eq.0)call hwriter(item,gains(1,k0),offset,8*nants,
     *								iostat)
	    offset = offset + 8*nants
	    if(iostat.ne.0)then
	      call bug('w','I/O error while writing to gains item')
	      call bugno('f',iostat)
	    endif
	  endif
	enddo
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing output gains item')
	  call bugno('f',iostat)
	endif
c
c  Write some extra information for the gains table.
c
	call wrhdd(tgains,'interval',dble(interval))
	call wrhdi(tgains,'ngains',nants)
	call wrhdi(tgains,'nsols',nsols-nbad)
	call wrhdi(tgains,'nfeeds',1)
	call wrhdi(tgains,'ntau',0)
c
	end
c************************************************************************
	subroutine GainScal(Gains,nants,nSols)
c
	implicit none
	integer nants,nSols
	complex Gains(nants,nSols)
c
c  Scale the gains to have an rms value of 1.
c
c------------------------------------------------------------------------
	integer i,j,N
	real Sum,fac
	complex g
c
	Sum = 0
	N = 0
	do j=1,nsols
	  do i=1,nants
	    g = Gains(i,j)
	    if(abs(real(g))+abs(aimag(g)).gt.0)then
	      N = N + 1
	      Sum = Sum + real(g)**2 + aimag(g)**2
	    endif
	  enddo
	enddo
c
	if(N.eq.0)return
	fac = sqrt(N / Sum)
c
	do j=1,nSols
	  do i=1,nants
	    Gains(i,j) = fac * Gains(i,j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GFudge(gains,nants)
c
	implicit none
	integer nants
	complex gains(nants)
c
c  Get the reciprocal of the gains.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nants
	  if(abs(real(gains(i)))+abs(aimag(gains(i))).gt.0)
     *	    gains(i) = 1/gains(i)
	enddo
c
	end
c************************************************************************
	subroutine SmthData(nSols,nBl,Time,TIndx,SumVM,SumVV,
     *	  SumMM,Weight,Count,rTime)
c
	implicit none
	integer nSols,nBl,Time(nSols),TIndx(nSols)
	complex SumVM(nBl,nSols)
	real SumVV(nBl,nSols),SumMM(nSols),Weight(nBl,nSols)
	real Count(nSols),rTime(nSols)
c
c  This adds in a contribution, to the statistics (needed for determining
c  self-cal solutions), from adjacent time intervals.
c
c  Input:
c    nSols
c    nBl
c    Time
c    TIndx
c  Input/Output:
c    SumVM
c    SumVV
c    SumMM
c    Weight
c    Count
c    rTime
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex SaveVM(MAXBASE),ctemp
	real SaveVV(MAXBASE),SaveMM,SaveWt(MAXBASE),SaveCnt,SaveTim,temp
	logical saved,dosave
	integer i,k,k0,k1
c
	saved = .false.
	do k=1,nSols
	  k0 = TIndx(k)
	  dosave = k.lt.nSols
	  if(dosave) dosave = Time(TIndx(k+1)).eq.Time(TIndx(k))+1
	  if(dosave)then
	    k1 = TIndx(k+1)
c
c  Do the case where we have to add in half the previous slot.
c
	    if(.not.saved)then
	      do i=1,nBl
		SaveVM(i) = SumVM(i,k0)
		SaveVV(i) = SumVV(i,k0)
		SaveWt(i) = Weight(i,k0)
		SumVM(i,k0) = SumVM(i,k0) + 0.5*SumVM(i,k1)
	        SumVV(i,k0) = SumVV(i,k0) + 0.5*SumVV(i,k1)
		Weight(i,k0) = Weight(i,k0) + 0.5*Weight(i,k1)
	      enddo
	      SaveMM = SumMM(k0)
	      SaveCnt = Count(k0)
	      SaveTim = rTime(k0)
	      SumMM(k0) = SumMM(k0) + 0.5*SumMM(k1)
	      Count(k0) = Count(k0) + 0.5*Count(k1)
	      rTime(k0) = rTime(k0) + 0.5*rTime(k1)
	    else
	      do i=1,nBl
		ctemp = SumVM(i,k0)
		SumVM(i,k0) = SumVM(i,k0) +
     *				0.5 * ( SaveVM(i) + SumVM(i,k1) )
		SaveVM(i) = ctemp
		temp = SumVV(i,k0)
	        SumVV(i,k0) = SumVV(i,k0) +
     *				0.5 * ( SaveVV(i) + SumVV(i,k1) )
		SaveVV(i) = temp
		temp = Weight(i,k0)
		Weight(i,k0) = Weight(i,k0) +
     *				0.5 * ( SaveWt(i) + Weight(i,k1) )
		SaveWt(i) = temp
	      enddo
	      temp = SumMM(k0)
	      SumMM(k0) = SumMM(k0) + 0.5 * ( SaveMM + SumMM(k1) )
	      SaveMM = temp
	      temp = Count(k0)
	      Count(k0) = Count(k0) + 0.5 * ( SaveCnt + Count(k1) )
	      SaveCnt = temp
	      temp = rTime(k0)
	      rTime(k0) = rTime(k0) + 0.5 * ( SaveTim + rTime(k1) )
	      SaveTim = temp
	    endif
	  else
	    if(saved)then
	      do i=1,nBl
		SumVM(i,k0) = SumVM(i,k0) + 0.5 * SaveVM(i)
	        SumVV(i,k0) = SumVV(i,k0) + 0.5 * SaveVV(i)
		Weight(i,k0) = Weight(i,k0) + 0.5*SaveWt(i)
	      enddo
	      SumMM(k0) = SumMM(k0) + 0.5*SaveMM
	      Count(k0) = Count(k0) + 0.5*SaveCnt
	      rTime(k0) = rTime(k0) + 0.5*SaveTim
	    endif
	  endif
	  saved = dosave
	enddo
c
	end
c************************************************************************
	subroutine Solve2(nbl,nants,SumVM,SumVV,Weight,
     *	    phase,relax,MinAnts,Refant,GainOut,Convrg)
c
	implicit none
	integer nbl,nants,MinAnts,RefAnt
	real SumVV(nbl),Weight(nBl)
	complex SumVM(nbl),GainOut(nants)
	logical phase,relax,Convrg
c
c  Routine to do various fooling around before a routine is called to
c  determine the gain solution. The fooling around makes the inner loop
c  of the gain solution pretty clean.
c
c  Inputs:
c    nants	Number of antennae.
c    nbl	Number of baselines, must equal nants*(nants-1)/2
c    MinAnts	The minimum number of antenna that must be present.
c    RefAnt	The antenna to refer solutions to.
c    SumVM	The weighted sum of the visibilities.
c    SumVV	The weighted sum of the visibilities moduli squared.
c
c  Outputs:
c    GainOut	The complex gains to apply to correct the data.
c    Convrg	Whether it converged or not.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer b1(MAXBASE),b2(MAXBASE)
	integer i,j,k,Nblines,nantenna,nref
	complex Sum(MAXANT),Gain(MAXANT),Temp,SVM(MAXBASE)
	real Sum2(MAXANT),Wts(MAXANT)
	real SVV(MAXBASE)
	integer Indx(MAXANT)
c
c  Externals.
c
	integer ismax
c
c  Check.
c
	if(nbl.ne.nants*(nants-1)/2)
     *	  call bug('f','Number of antennae and baselines do not agree')
	if(nants.gt.MAXANT)call bug('f','Too many antennas')
c
c  Some intialising.
c
	do k=1,nants
	  Indx(k) = 0
	  Wts(k) = 0
	enddo
c
c  Now find which baselines are there, and map the antenna numbers,
c
c  Squeeze out unnecessary baselines and antenna from this time slice.
c  This also works out the map from baseline number to "notional antenna
c  number" pairs, contained in B1 and B2. INDX is set up to contain the
c  map from notional antenna number to the FITS-file antenna number.
c  Thus baseline i, corresponds to notional antenna nos. B1(i) and B2(i).
c  If INDX(k).eq.B1(i), then the real antenna no. is K.
c
c  This fancy compression is needed to give good clean inner loops
c  (no IF statements) in the gain solution routines.
c
	nantenna = 0
	NBlines = 0
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    if(SumVV(k).gt.0)then
	      Wts(i) = Wts(i) + Weight(k)
	      Wts(j) = Wts(j) + Weight(k)
c
	      NBlines = NBlines + 1
	      SVM(NBlines) = SumVM(k)
	      SVV(NBlines) = SumVV(k)
c
	      if(Indx(i).eq.0)then
		nantenna = nantenna + 1
		Indx(i) = nantenna
	      endif
	      B1(NBlines) = Indx(i)
c
	      if(Indx(j).eq.0)then
		nantenna = nantenna + 1
		Indx(j) = nantenna
	      endif
	      B2(NBlines) = Indx(j)
	    endif
	  enddo
	enddo
c
c  Check that we have enough antennae. More initialisation.
c  Call the routine to get the solution.
c
	if(MinAnts.gt.nantenna)then
	  Convrg = .false.
	else if(phase)then
	  call phasol  (NBlines,nantenna,Sum,SVM,b1,b2,gain,convrg)
	  convrg = convrg.or.relax
	else
	  call amphasol(NBlines,nantenna,Sum,Sum2,SVM,SVV,
     *						   b1,b2,gain,convrg)
	  convrg = convrg.or.relax
	endif
c
c  If it converged, unsqueeze the gains, and refer them to the reference
c  antenna.
c
	if(.not.Convrg)then
	  do i=1,nants
	    GainOut(i) = 0
	  enddo
	else
	  do i=1,nants
	    if(Indx(i).eq.0)then
	      GainOut(i) = (0.,0.)
	    else
	      GainOut(i) = Gain(Indx(i))
	    endif
	  enddo
c
	  nref = Refant
	  if(nref.ne.0)then
	    if(Wts(nref).le.0) nref = 0
	  endif
	  if(nref.eq.0) nref = ismax(nants,Wts,1)
	  Temp = conjg(GainOut(nRef))/abs(GainOut(nRef))
	  do i=1,nants
	    GainOut(i) = Temp * GainOut(i)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine CalcStat(tgains,nsols,nbl,nants,SumVM,SumMM,SumVV,
     *	  Weight,Count,Gains)
c
	implicit none
	integer tgains
	integer nsols,nbl,nants
	complex SumVM(nbl,nsols),Gains(nants,nsols)
	real SumMM(nsols),SumVV(nbl,nsols),Weight(nbl,nsols)
	real Count(nsols)
c
c  Accumulate the various statistics.
c
c------------------------------------------------------------------------
	real Resid,SumChi2,SumPhi,SumExp,SumWts,SumAmp,Phi,Amp,Sigma
	real m1,m2,wt
	complex g1,g2
	integer i,j,k,sol
	character line*64
c
	SumChi2 = 0
	SumPhi = 0
	SumExp = 0
	SumWts = 0
	SumAmp = 0
c
c  Calculate residual. Note that if the residuals is so small that rounding
c  error may be a problem (this will only happen with dummy data), then take
c  its absolute value.
c
	do sol=1,nSols
	  if(Count(sol).gt.0)then
	    k = 0
	    Resid = SumMM(sol)
	    do j=2,nants
	      do i=1,j-1
		k = k + 1
		Wt = Weight(k,sol)
		g1 = conjg(Gains(i,sol))
		g2 = Gains(j,sol)
		m1 = real(g1)**2 + aimag(g1)**2
		m2 = real(g2)**2 + aimag(g2)**2
		if(Wt.gt.0.and.m1.gt.0.and.m2.gt.0)then
		  Resid = Resid - 2*real(g1*g2*SumVM(k,sol)) +
     *			m1*m2*SumVV(k,sol)
		  SumWts = SumWts + Wt
		  call amphase(g1*g2,amp,phi)
		  SumPhi = SumPhi + Wt*phi**2
		  SumAmp = SumAmp + Wt*(1-amp)**2
	        endif
	      enddo
	    enddo
	    SumChi2 = SumCHi2 + abs(Resid)
	    SumExp = SumExp + Count(sol)
	  endif
	enddo
c
	Phi = sqrt(abs(SumPhi/(2*SumWts)))
	Amp = sqrt(abs(SumAmp/(2*SumWts)))
	Sigma = sqrt(abs(SumChi2/SumExp))
	write(line,'(a,f6.1)')'Rms of the gain phases (degrees):',phi
	call output(line)
	call HisWrite(tgains,'SELFCAL: '//line)
	write(line,'(a,1pg10.3)')'Rms deviation of gain from 1:',amp
	call output(line)
	call HisWrite(tgains,'SELFCAL: '//line)
	write(line,'(a,1pg10.3)')
     *	  'Ratio of Actual to Theoretical noise:',Sigma
	call output(line)
	call HisWrite(tgains,'SELFCAL: '//line)
c
	end
c************************************************************************
	subroutine phasol(Nblines,NAnts,Sum,SumVM,b1,b2,Gain,Convrg)
c
	implicit none
	logical Convrg
	integer Nblines,Nants
	integer b1(NBlines),b2(NBlines)
	complex SumVM(Nblines),Gain(NAnts),Sum(NAnts)
c
c  Solve for the phase corrections which minimise the error. This uses
c  a nonlinear Jacobi iteration, as suggested by Fred Schwab in "Adaptive
c  calibration of radio interferomter data", SPIE Vol. 231, 1980
c  International Optical Computing Conference (pp 18-24). The damping
c  heuristics are copied from AIPS ASCAL.
c
c  Input:
c    NBlines	Number of baselines.
c    Nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of Model*conjg(Vis)
c  Scratch:
c    Sum
c  Output:
c    Convrg	If .true., then the algorithm converged.
c    Gain	The antenna gain solution.
c------------------------------------------------------------------------
	integer MaxIter
	real Epsi,Epsi2
	parameter(MaxIter=100,Epsi=1.e-8,Epsi2=1.e-4)
c
	real Factor,Change
	complex Temp
	integer Niter,i
c
c  Initialise.
c
	do i=1,NAnts
	  Gain(i) = (1.,0.)
	  Sum(i) = (0.,0.)
	enddo
c
	Factor = 0.8
	if(Nants.le.6)Factor = 0.5
c
c  Iterate.
c
	Convrg = .false.
	Niter = 0
	do while(.not.Convrg.and.Niter.lt.MaxIter)
	  Niter = Niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
	  do i=1,nblines
	    Sum(b1(i)) = Sum(b1(i)) + Gain(b2(i)) *       SumVM(i)
	    Sum(b2(i)) = Sum(b2(i)) + Gain(b1(i)) * conjg(SumVM(i))
	  enddo
c
c  Update the gains. The following will be flagged as a short loop
c  on the Cray, if we assume we have fewer than 32 antennae. Hopefully
c  this will vectorise. For "typical" cases, the absolute value function
c  in the next loop takes up about 30-40% of the run time of this routine
c  on a VAX.
c
	  Change = 0
c#maxloop 32
	  do i=1,nants
	    Temp = ( Sum(i)/abs(Sum(i)) )
	    Temp = Gain(i) + Factor * ( Temp - Gain(i) )
	    Temp = Temp/abs(Temp)
	    Change = Change + real(Gain(i)-Temp)**2
     *			    + aimag(Gain(i)-Temp)**2
	    Gain(i) = Temp
	    Sum(i) = (0.,0.)
	  enddo
	  Convrg = Change/nants .lt. Epsi
	enddo
	Convrg = Change/nants .lt. Epsi2
	end
c************************************************************************
	subroutine amphasol(NBlines,NAnts,Sum,Sum2,SumVM,SumVV,
     *						b1,b2,gain,convrg)
c
	implicit none
	logical Convrg
	integer NBlines,NAnts
	integer B1(NBlines),B2(NBlines)
	complex SumVM(NBlines),Gain(NAnts),Sum(NAnts)
	real SumVV(NBlines),Sum2(NAnts)
c
c  Solve for the amplitudes and phase corrections which minimise
c  error. Algorithm is again Schwab's Jacobi iteration. The damping
c  heuristics are copied from AIPS ASCAL or dreamed up by me (as was the
c  initial gain estimate).
c
c  Input:
c    NBlines	Number of baselines.
c    Nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of Model*conjg(Vis), for each baseline.
c    SumVV	Sum of Vis*conjg(Vis), for each baseline.
c  Scratch:
c    Sum
c  Output:
c    Convrg	If .true., then the algorithm converged.
c    Gain	The antenna gain solution.
c
c------------------------------------------------------------------------
	integer maxiter
	real Epsi,Epsi2
	parameter(maxiter=100,Epsi=1.e-8,Epsi2=1e-4)
	integer i,Niter
	real Factor,Change,SumRVV,SumWt,SumRVM
	complex Temp
	real Factors(11)
	data Factors/0.5,0.75,8*0.9,0.5/
c
c  Calculate initial phase gain estimates.
c
	call phasol(NBlines,Nants,Sum,SumVM,b1,b2,gain,convrg)
	if(.not.convrg)return
c
c  Get an initial approximation of the gain solution. This finds a single
c  real gain which minimises the error. This helps stablise the algorithm
c  when the gain solution is very different from 1 (e.g. when we are
c  calculating a priori calibration factors).
c
	SumRVM = 0
	SumRVV = 0
	do i=1,NBlines
	  SumRVM = SumRVM + conjg(gain(b1(i)))*gain(b2(i))*SumVM(i)
	  SumRVV = SumRVV + SumVV(i)
	enddo
	Factor = sqrt(abs(SumRVM / SumRVV))
c
c  Ready for the amplitude/phase solution.
c
	do i=1,NAnts
	  Gain(i) = Factor * Gain(i)
	  Sum(i) = 0
	  Sum2(i) = 0.
	enddo
c
c  Iterate.
c
	Convrg = .false.
	Niter = 0
	do while(.not.Convrg.and.Niter.lt.MaxIter)
	  Niter = Niter + 1
c
c  Set the damping constant. I do not think this is really necessary.
c  AIPS does it.
c
	  if(Nants.le.6)then
	    Factor = 0.5
	  else
	    Factor = Factors(min(11,Niter))
	  endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
	  do i=1,nblines
	    Sum(b1(i)) = Sum(b1(i)) + Gain(b2(i)) *       SumVM(i)
	    Sum(b2(i)) = Sum(b2(i)) + Gain(b1(i)) * conjg(SumVM(i))
	    Sum2(b1(i)) = Sum2(b1(i)) +
     *	     (real(Gain(b2(i)))**2 + aimag(Gain(b2(i)))**2) * SumVV(i)
	    Sum2(b2(i)) = Sum2(b2(i)) +
     *	     (real(Gain(b1(i)))**2 + aimag(Gain(b1(i)))**2) * SumVV(i)
	  enddo
c
c  Update the gains. The following should be flagged as a short loop
c  on the Cray, if we assume we have fewer than 32 antennae. Hopefully
c  this will vectorise.
c
	  Change = 0
	  SumWt = 0
c#maxloop 32
	  do i=1,nants
	    Temp = Sum(i)/Sum2(i) - Gain(i)
	    Gain(i) = Gain(i) + Factor * Temp
	    Change = Change + (real(Temp)**2 + aimag(Temp)**2)
	    SumWt  = SumWt  + (real(Gain(i))**2 + aimag(Gain(i))**2)
	    Sum(i) = (0.,0.)
	    Sum2(i) = 0.
	  enddo
	  Convrg = Change/SumWt .lt. Epsi
	enddo
	Convrg = Change/SumWt .lt. Epsi2
	end
