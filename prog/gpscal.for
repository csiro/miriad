c************************************************************************
	program gpscal
	implicit none
c
c= gpscal - Self-calibration for arrays with linear feeds
c& rjs
c: calibration, map making
c+
c	GPSCAL is a MIRIAD task to perform self-calibration of visibility data,
c	where the visibility data is produced by linear feeds, and the source
c	is moderately or strongly polarised. Either phase only or amplitude
c	and phase calibration can be performed. The inputs to GPSCAL include a
c	visibility data file, and model images. The models should be of
c	differing polarisations of the same pointing and frequencies.
c	GPSCAL then calculates the visibilities corresponding to the model,
c	accumulates the statistics needed to determine the antennae solutions,
c	and then calculates the self-cal solutions.
c
c	Note that while GPSCAL can be used for self-calibrating with a point
c	source model, task GPCAL is more flexible (and efficient) at doing
c	this. For a point source model, GPSCAL is preferable only if the
c	data-set is not in time order (GPCAL insists that it is) or the point
c	source is not at the observing center.
c@ vis
c	Name of input visibility data file. No default.
c@ select
c	Standard uv data selection criteria. The default is all data.
c@ model
c	Name of the input models. Several models can be given, which differ
c	in polarisation type. Polarisation types can be I,Q,U or V. Any
c	polarisation types that are not given are assumed to be zero.
c	The different models should correspond to the same pointing
c	and range of channels. Generally the model should be derived (by
c	mapping and deconvolution) from the input visibility file, so that
c	the channels in the model correspond to channels in the visibility
c	file. Though the maps can be made using any linetype, generally
c	"channel" linetype will give best results (??). The units of the
c	model MUST be JY/PIXEL, rather than JY/BEAM. It should be weighted
c	by the primary beam. The task DEMOS can be used to extract primary
c	beam weighted models from a mosaiced image. If no models are given,
c	a point source model is assumed (see FLUX keyword).
c@ clip
c	Clip level. For models of intensity, any pixels below the clip level
c	are set to zero. For models of Stokes Q,U,V, or MFS I*alpha models,
c	any pixels whose absolute value is below the clip level are set
c	to zero. The default is not to do any clipping.
c@ flux
c	If not model is given, then a point source model is assumed. This
c	keyword gives the flux of the point source model. Four values can be
c	given, corresponding to I,Q,U and V respectively. NOTE: The flux
c	of the model is not adjusted to match the flux of the data, so if
c	amplitude selfcalibration is being performed, the fluxes given should
c	accurately portray the fluxes of the source. The default is 1,0,0,0.
c@ offset
c	This gives the offset in arcseconds of a point source model (the
c	offset is positive to the north and to the east). This parameter is
c	used if the MODEL parameter is blank. The default is 0,0. The
c	amplitude of the point source is chosen so that flux in the model
c	is the same as the visibility flux.
c@ interval
c	The length of time, in minutes, of a gain solution. Default is 5,
c	but use a larger value in cases of poor signal to noise, or
c	if the atmosphere and instrument is fairly stable.
c@ options
c       This gives several processing options. Possible values are:
c	  amplitude  Perform amplitude and phase self-cal.
c	  phase      Perform phase only self-cal.
c	  noxy       Do not attempt to solve for the XY phase difference.
c	             Normally GPSCAL attempts to solve for the XY phase
c	             on all antennae except for the reference antenna.
c	             The XY phase difference is assumed to be constant
c	             throughout the observation.
c	  xyvary     Allow the XY phase difference to vary from integration
c	             to integration. If not specified, the XY phase is
c	             constrained to be constant throughout the data-set.
c	  xyref      Solve for the XY phase of the reference antenna. To
c	             do this, the source should be strongly polarized (at
c	             least 5%) and Q or U models should be provided. This
c	             option cannot be used with ``noxy''. 
c	  mfs        This is used if there is a single plane in the input
c	             model, which is assumed to represent the image at all
c	             frequencies. This should also be used if the model has
c	             been derived from MFCLEAN. You should specify the
c		     LINE keyword if you use the mfs option.
c	  noscale    Do not scale the gains. By default the gains are scaled
c	             so that the rms gain amplitude is 1. Generally this
c	             should be used if the model is believed to have the
c	             correct flux density scale.
c	Note that "amplitude" and "phase" are mutually exclusive.
c	The default is options=phase.
c@ minants
c	Data at a given solution interval is deleted  if there are fewer than
c	MinAnts antennae operative during the solution interval. The default
c	is 3 for options=phase and 4 for options=amplitude.
c@ refant
c	This sets the reference antenna, which is given a phase angle of zero.
c	The default is antenna 3.
c@ line
c	The visibility linetype to use, in the standard form, viz:
c	  type,nchan,start,width,step
c	Generally if there is an input model, this parameter defaults to the
c	linetype parameters used to construct the map. If you wish to override
c	this, or if the info is not in the header, or if you are using
c	a point source model, this parameter can be useful.
c--
c  History:
c    rjs  26apr92 Adapted from SELFCAL and GPCAL.
c    rjs   4may92 Bug fixes and minor enhancements.
c    rjs  17may92 Fiddles with clippling.
c    rjs  23jun92 Doc and message changes. Handle XY phase when refering
c		  antennae.
c    rjs  04aug92 Eliminate the xyphases item.
c    rjs  22nov92 Added ntau to output gains table.
c    rjs  29mar93 Fiddle noise calculation.
c    rjs  26oct93 Make counting of nants consistent with other progs.
c    rjs   4nov93 options=noscale.
c    rjs  13dec93 Sign convention of V change.
c    rjs  23dec93 Minimum match for linetypes.
c    rjs  13sep94 Improve an error message. FELO changes.
c    rjs  31jan95 Accomodate model.for changes.
c    rjs   1oct96 Default is not to clip.
c------------------------------------------------------------------------
	include 'gpscal.h'
	character version*(*)
	parameter(version='GpsCal: version 1.0 1-Oct-96')
	integer MAXSELS,nhead
	parameter(MAXSELS=256,nhead=0)
c
	character Models(4)*64,vis*64,ltype*32
	character flag1*8,flag2*8
	integer tvis,tmod,tscr(4),tno,Indx
	integer nModel,minants,refant,nants,nsize(3),nchan,nvis,i
	real sels(MAXSELS),clip,interval,offset(2),lstart,lwidth,lstep
	real flux(4)
	double precision Saved(16)
	logical phase,amp,doline,mfs,doxy,xyvary,doref,noscale,doclip
c
c  Externals.
c
	character PolsC2P*2
	external Header,calget
	logical keyprsnt
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keyf('vis',vis,' ')
	call SelInput('select',sels,MAXSELS)
	call mkeyf('model',Models,4,nModel)
	doclip = keyprsnt('clip')
	call keyr('clip',clip,0.)
	call keyr('interval',interval,5.)
	call keyi('minants',minants,0)
 	call keyi('refant',refant,3)
	call keyr('offset',offset(1),0.)
	call keyr('offset',offset(2),0.)
	call keyr('flux',flux(PolI),1.0)
	call keyr('flux',flux(PolQ),0.0)
	call keyr('flux',flux(PolU),0.0)
	call keyr('flux',flux(PolV),0.0)
	call keyline(ltype,nchan,lstart,lwidth,lstep)
	doline = ltype.ne.' '
	call GetOpt(phase,amp,doxy,xyvary,doref,mfs,noscale)
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
	  if(flux(PoLI).le.0)call bug('f','Bad flux given')
	  if(flux(PolQ)*flux(PolQ)+flux(PolU)*flux(PolU)+
     *	     flux(PolV)*flux(PolV).gt.flux(PolI)*flux(PolI))
     *	    call bug('f','Flux is more than 100% polarised!')
	endif
c
c  Open the visibility file, check that it is interferometer data, and set
c  the line type if necessary.
c
	call uvopen(tvis,vis,'old')
	if(doline)call uvset(tvis,'data',ltype,nchan,lstart,lwidth,
     *								lstep)
c
c  Determine the flags to the MODELINI and MODEL routines.
c
	flag1 = 'p'
	if(.not.doline.and..not.mfs)flag1(2:2) = 'l'
c
	flag2 = ' '
	if(doclip) flag2(1:1) = 'l'
	if(mfs)    flag2(2:2) = 'm'
c
c  Loop over all the models.
c
	do i=1,4
	  tscr(i) = 0
	enddo
c
	call HeadIni
	if(nModel.eq.0)then
	  call SelApply(tvis,sels,.true.)
	  do i=1,4
	    if(flux(i).ne.0)then
	      call output('Doing model computation for '//PolsC2P(i))
	      call uvrewind(tvis)
	      call HeadIni
	      call Model(flag2,tvis,0,offset,flux(i),tscr(i),
     *				nhead,Header,calget,nchan,nvis)
	    endif
	  enddo
	else
	  do i=1,nModel
	    call output('Calculating the model for '//Models(i))
	    call xyopen(tmod,Models(i),'old',3,nsize)
	    call HeadIni
	    call ModelIni(tmod,tvis,sels,flag1)
	    call Model(flag2,tvis,tmod,offset,clip,tno,
     *				nhead,Header,calget,nchan,nvis)
	    call GetPolTy(i.eq.1,tmod,Indx,nchan,nvis,Saved)
	    tscr(Indx) = tno
	    call xyclose(tmod)
	  enddo
	endif
c
c  Check we have enough antennas.
c
	call HeadAnts(nants)
	if(nants.lt.MinAnts)call bug('f','Too few antennas')
	if(refant.gt.nants)
     *	  call bug('f','Reference antenna does not exist')
c
c  Add history to the input file.
c
	call HisOpen(tvis,'append')
	call HisWrite(tvis,'GPSCAL: Miriad '//version)
	call HisInput(tvis,'GPSCAL')
c
c  What stage are we at? We have computed the model data for all the
c  polarisations given.
c
	call uvrewind(tvis)
	call uvset(tvis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call AccSolve(tvis,tscr,minants,refant,phase,doxy,xyvary,
     *	  doref,noscale,nants,interval,nchan)
c
c  Close up.
c
	do i=1,4
	  if(tscr(i).ne.0)call scrclose(tscr(i))
	enddo
	call HisClose(tvis)
	call uvclose(tvis)
	end
c************************************************************************
	subroutine GetOpt(phase,amp,doxy,xyvary,doref,mfs,noscale)
c
	implicit none
	logical phase,amp,doxy,doref,mfs,xyvary,noscale
c
c  Determine extra processing options.
c
c  Output:
c    phase	If true, do phase self-cal.
c    amp	If true, do amplitude/phase self-cal.
c    doxy	Solve for XY phase difference.
c    xyvary	Allow the XY phase to vary from integration to integration.
c    doref	Solve for XY phase difference on the reference antenna.
c    mfs	Model is frequency independent, or has been derived
c		from MFCLEAN.
c    noscale	Do not scale the gains.
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=7)
	character opts(nopt)*9
	logical present(nopt)
	data opts/'amplitude','phase    ','xyvary   ',
     *		  'noxy     ','xyref    ','mfs      ',
     *		  'noscale  '/
	call options('options',opts,present,nopt)
	amp = present(1)
	phase = present(2)
	xyvary = present(3)
	doxy = .not.present(4)
	doref = present(5)
	mfs = present(6)
	noscale = present(7)
	if(amp.and.phase)
     *	  call bug('f','Cannot do both amp and phase self-cal')
	if(.not.(amp.or.phase)) phase = .true.
	if(xyvary.and.phase)
     *	  call bug('f','You must select options=amplitude with xyvary')
	if(xyvary.and..not.doxy)
     *	  call bug('f','You cannot specify option noxy with xyvary')
	if(doref.and..not.doxy)
     *	  call bug('f','You cannot specify option noxy with xyref')
	end
c************************************************************************
	subroutine GetPolTy(first,tmod,PolType,nchan,nvis,Saved)
c
	implicit none
	logical first
	integer tmod,PolType,nchan,nvis
	double precision Saved(8)
c
c  This checks that the models are compatible, and that the number of
c  channels and visibilities resulting from them are the same. It also
c  gets the poarisation type of the model.
c
c  Input:
c    tmod	Handle of the input file.
c    nchan	Number of channels in the data.
c    nvis	Number of visibilities in the data.
c  Input/Output:
c    Saved	Used to hold info about the model, used by this routine
c		to check that all the models point to the same place.
c  Output:
c    PolType	The polarisation code for the file.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer NCheck
	parameter(NCheck=4)
	integer i,naxis
	double precision New(NCheck)
	character num*2,ctype*32
c
c  Externals.
c
	character itoaf*2
c
c  Set up the new vector.
c
	New(1) = nchan
	New(2) = nvis
	call rdhdd(tmod,'crval1',New(3),0.d0)
	call rdhdd(tmod,'crval2',New(4),0.d0)
c
c  Check that the old and the new are the same.
c
	if(first)then
	  do i=1,NCheck
	    Saved(i) = New(i)
	  enddo
	else
	  do i=1,NCheck
	    if(Saved(i).ne.New(i))
     *	      call bug('f','Models do not appear to agree')
	  enddo
	endif
c
c  Get the polarisation type of the model.
c
	call rdhdi(tmod,'naxis',naxis,0)
	PolType = 0
	do i=3,naxis
	  num = itoaf(i)
	  call rdhda(tmod,'ctype'//num,ctype,' ')
	  if(ctype.eq.'STOKES')call rdhdi(tmod,'crval'//num,PolType,0)
	enddo
	if(PolType.lt.PolI.or.PolType.gt.PolV)
     *	  call bug('f','Illegal polarisation type')
c
	end
c************************************************************************
	subroutine Header(tvis,preamble,data,flags,nchan,
     *						accept,Out,nhead)
	implicit none
	integer tvis,nchan,nhead
	complex data(nchan)
	logical flags(nchan),accept
	real Out
	double precision preamble(5)
c
c  This is a service routine called by the model subroutines. It is
c  called every time a visibility is read from the data file.
c
c  Input:
c    tvis	Handle of the visibility file.
c    nhead	The value of nhead. Zero for GPSCAL.
c    nchan	The number of channels.
c    preamble	Preamble returned by uvread.
c    data	A complex array of nchan elements, giving the correlation data.
c		Not used.
c    flags	The data flags. Not used.
c  Output:
c   out		The nhead values to save with the data. No values are
c		returned.
c   accept	This determines whether the data is accepted or discarded.
c		It is always accepted unless the baseline number looks bad.
c------------------------------------------------------------------------
	integer i,i1,i2
c
	logical first
	integer nants,baseline
	double precision time
	common/GPSCALC/time,baseline,nants,first
c
	i2 = nint(Preamble(5))
	if(first)then
	  call uvrdvri(tvis,'nants',nants,0)
	  accept = .true.
	else
	  accept = i2.ne.baseline.or.
     *			abs(time-Preamble(4)).gt.0.25/(24.*3600.)
	endif
c
	if(accept)then
	  call basant(preamble(5),i1,i2)
	  accept = i1.ne.i2
	endif
c
	if(accept)then
	  baseline = nint(Preamble(5))
	  time = Preamble(4)
c
	  do i=1,nchan
	    flags(i) = .true.
	  enddo
	endif
	first = .false.
c
	end
c************************************************************************
	subroutine HeadIni
c
	implicit none
c
c  Initialise the header routines.
c------------------------------------------------------------------------
	logical first
	integer nants,baseline
	double precision time
	common/GPSCALC/time,baseline,nants,first
c
	first = .true.
	end
c************************************************************************
	subroutine HeadAnts(nantsd)
c
	implicit none
	integer nantsd
c
c  Return the number of antennae.
c------------------------------------------------------------------------
	logical first
	integer nants,baseline
	double precision time
	common/GPSCALC/time,baseline,nants,first
c
	nantsd = nants
	end
c************************************************************************
	subroutine AccSolve(tvis,tscr,minants,refant,phase,doxy,xyvary,
     *	  doref,noscale,nants,interval,nchan)
c
	implicit none
	integer tvis,tscr(4),refant,minants,nants
	integer nchan
	real interval
	logical phase,doxy,xyvary,doref,noscale
c
c  This accumulates statistics and finds the selfcal solutions.
c
c  Input:
c    tvis	Handle of the input dataset.
c    tscr	Scratch file containing the input models.
c    refant	Reference antenna.
c    minants	Minimum number of antennas for a solution.
c    nants	Number of antennae.
c    nchan	Number of channels in the data.
c    interval	Selfcal solution interval.
c    phase	Do phase solution only?
c    doxy	Attempt to solve for the XY phase.
c    xyvary	Allow the XY phase to vary from integration to integration.
c    doref	Solve for XY phase of the reference antenna.
c    noscale    Do not scale the gains.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'gpscal.h'
	integer maxHash,minSol
	parameter(maxHash=20000,minSol=200)
	integer SolSize,maxSol,nbl,nSols
	double precision time0
	complex xyp(MAXANT),D(2,MAXANT),xyref
	integer pSumVM,pSumMM,pGains,pTime,pCount
	integer Hash(maxHash),Indx(maxHash)
c
c  Dynamic memory declarations.
c
	real ref(MAXBUF)
	integer iref(MAXBUF)
	logical lref(MAXBUF)
	complex cref(MAXBUF/2)
	common ref
	equivalence(ref,iref,lref,cref)
c
c  Externals.
c
	integer MemBuf
	character itoaf*4
c
c  Allocate memory.
c
	nbl = (nants*(nants-1))/2
	SolSize = (4*2*nbl+4*nbl+2*nants+1+1)
	maxSol = min(maxHash,max(minSol,3*MemBuf()/SolSize))
	call MemAlloc(pSumVM,4*nbl*maxSol,'c')
	call MemAlloc(pSumMM,4*nbl*maxSol,'r')
	call MemAlloc(pTime,maxSol,'r')
	call MemAlloc(pCount,maxSol,'i')
c
c  Initialise the leakage table and the xyphase table and the routine
c  to perform polarisation conversion.
c
	call PolIni(tvis,D,xyp,nants)
	call CompIni(tscr,D,nants)
c
c  Accumulate the data.
c
	call output('Accumulating statistics ...')
	call Accum(tvis,nchan,nbl,maxSol,nSols,Hash,Indx,maxHash,
     *	 interval,ref(pTime),Time0,iref(pCount),
     *	 cref(pSumVM),ref(pSumMM))
	call output('Number of solution intervals: '//itoaf(nSols))
	if(nSols.eq.0)call bug('f','No data found')
c
c  Initialise the gain table.
c
	call MemAlloc(pGains,2*nants*nSols,'c')
	call GainIni(nants,nSols,cref(pGains),xyp)
	xyref = xyp(refant)
c
c  Find the gain solution.
c
	call output('Finding gain solutions ...')
	call Solve(nbl,nants,nSols,cref(pSumVM),ref(pSumMM),
     *	  phase,minants,doxy,xyvary,cref(pGains),lref(pCount),xyp)
c
c  Fiddle the gains to put them into a standard form.
c
	call output('Fiddling and saving the gains ...')
	call GainFidd(cref(pGains),nants,nSols,refant,xyref,doref,
     *	  phase.or.noscale)
	call GainSave(tvis,cref(pGains),nants,nSols,lref(pCount),
     *	  ref(pTime),Time0,interval,Indx)
c
c  Free the memory.
c
	call MemFree(pSumVM,4*nbl*maxSol,'c')
	call MemFree(pSumMM,4*nbl*maxSol,'r')
	call MemFree(pTime,maxSol,'r')
	call MemFree(pCount,maxSol,'i')
	call MemFree(pGains,2*nants*nSols,'c')
	end
c************************************************************************
	subroutine writeo(tvis,line)
c
	implicit none
	integer tvis
	character line*(*)
c
c  Write out a line to the history file and the output.
c------------------------------------------------------------------------
	character string*80
c
	string = 'GPSCAL: '//line
	call HisWrite(tvis,string)
	call output(line)
	end
c************************************************************************
	subroutine Solve(nbl,nants,nSols,SumVM,SumMM,
     *	  phase,minants,doxy,xyvary,Gains,Convrg,xyp)
c
	implicit none
	integer nbl,nants,nSols,minants
	logical phase,doxy,xyvary
	logical Convrg(nSols)
	real SumMM(4,nbl,nSols)
	complex SumVM(4,nbl,nSols),Gains(2,nants,nSols),xyp(nants)
c
c  Determine the selfcalibration solution.
c
c  Input:
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    nSols	Number of solution intervals.
c    SumVM	Sum of the data times the conjugate of the model.
c    SumMM	Sum of the modulus of the model squared.
c    phase	If true, do phase-only selfcal.
c    doxy	If true, attempt to solve for the xy phase.
c    xyvary	Allow XY phase to vary from integration to integration.
c    minants	The minimum number of antennas that must be present in
c		a solution interval.
c  Input/Output:
c    xyp	The xy phases. This is output only if doxy is true.
c    Gains	The antenna gains.
c  Output:
c    Conv	Flag to indicate whether the solution is good.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer MAXITER
	real tol
	parameter(tol=1.0E-3)
	parameter(MAXITER=30)
	integer i,niter,nConvrg
	logical more
	character line*64
	real ttol,epsi1,epsi2,epsi
c
c  Iterate until we converge!!
c
	ttol = 0.01
	if(.not.doxy.or.xyvary)ttol = tol
	more = .true.
	niter = 0
	dowhile(more.and.niter.lt.MAXITER)
	  niter = niter + 1
	  epsi = 0
	  ttol = max(0.5*ttol,0.3*tol)
c
c  Do the gain solution without XY phase.
c
	  nConvrg = 0
	  epsi1 = 0
	  do i=1,nSols
	    call GainSol(nants,nbl,minants,phase,xyvary,niter.eq.1,
     *	      SumVM(1,1,i),SumMM(1,1,i),xyp,Gains(1,1,i),ttol*ttol,
     *	      epsi1,Convrg(i))
	    if(Convrg(i)) nConvrg = nConvrg + 1
	  enddo
c
	  if(nConvrg.ne.nSols)then
	    write(line,'(a,i2,a,i4)')'Iter=',niter,
     *	    ', Intervals without solutions:',nSols-nConvrg
	    call output(line)
	  endif
c
	  epsi1 = sqrt(epsi1)
	  write(line,'(a,i2,a,f8.3)')'Iter=',niter,
     *	    ', Amplit/Phase Solution Error:',epsi1
	  call output(line)
c
c  Calculate the XY phases, if required.
c
	  epsi2 = 0
	  if(doxy.and..not.xyvary)then
	    call XYSol(nants,nbl,nSols,minants,
     *		SumVM,SumMM,xyp,Gains,Convrg,ttol*ttol,epsi2)
	    epsi2 = sqrt(epsi2)
	    write(line,'(a,i2,a,f8.3)')'Iter=',niter,
     *	      ', XY Phase Solution Error:    ',epsi2
	    call output(line)
	  endif
c
c  Check for convergence, and get ready for another iteration.
c
	  epsi = max(epsi1,epsi2)
	  more = doxy.and..not.xyvary.and.epsi.gt.tol
	  more = epsi.gt.tol
	enddo
c
	if(more)
     *	  call bug('w','Failed to converge ... saving results anyway')
	end
c************************************************************************
	subroutine GainFidd(Gains,nants,nSols,refant,xyref,doref,
     *							    noscale)
c
	implicit none
	integer nants,nSols,refant
	logical doref,noscale
	complex Gains(2,nants,nSols),xyref
c
c  Fiddle the gains into the standard form. In particular, refer to
c  the reference antenna, and possibly make the xyphase on the reference
c  antenna zero.
c
c  Input:
c    nants	Number of antennae.
c    nSols	Number of solution intervals.
c    refant	Reference antenna.
c    xyref	The required xyphase on the reference antenna. This is
c		not meaningful if doref is true.
c    doref	Do not adjust the phase of the Y feed of the reference
c		antenna to zero.
c    noscale	Do not scale the gains to have an rms value of 1.
c  Input/Output:
c    Gains	The antenna gains.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer i,j,p,n
	real t,fac
	complex temp,Ref(2)
c
c  Determine the appropriate scale factor if needed.
c
	n = 0
	fac = 0
	if(.not.noscale)then
	  do j=1,nSols
	    do p=1,2
	      do i=1,nants
		if(abs(real (Gains(p,i,j)))+
     *	         abs(aimag(Gains(p,i,j))).gt.0)then
		  n = n + 1
		  fac = fac +  real(Gains(p,i,j))**2
     *			    + aimag(Gains(p,i,j))**2
		endif
	      enddo
	    enddo
	  enddo
	endif
c
	if(n.gt.0)then
	  fac = sqrt(fac/n)
	else
	  fac = 1
	endif
c
c  Invert and reference all the gains. The X gains are referred to the
c  X feed of the reference antenna. The Y gains are referred to to
c  either the X or Y feed of the reference antenna (depending on doref).
c
	do j=1,nSols
c
c  Determine the phase to subtract off.
c
	  Ref(X) = cmplx(fac,0.)
	  t = abs(Gains(X,refant,j))
	  if(t.gt.0) Ref(X) = fac * Gains(X,refant,j) / t
	  Ref(Y) = Ref(X)
	  if(.not.doref)then
	    t = abs(Gains(Y,refant,j))
	    if(t.gt.0)Ref(Y) = fac * Gains(Y,refant,j) *
     *						conjg(xyref) / t
	  endif
c
	  do p=1,2
	    temp = Ref(p)
	    do i=1,nants
	      if(abs(real (Gains(p,i,j)))+
     *	         abs(aimag(Gains(p,i,j))).gt.0)then
	        Gains(p,i,j) = temp / Gains(p,i,j)
	      else
	        Gains(p,i,j) = (0.,0.)
	      endif
	    enddo
	  enddo
c
	enddo
c
	end
c************************************************************************
	subroutine GainSave(tvis,Gains,nants,nSols,Convrg,
     *	  time,time0,interval,Indx)
c
	implicit none
	integer tvis,nants,nSols,Indx(nSols)
	complex Gains(2*nants,nSols)
	logical Convrg(nSols)
	real time(nSols),interval
	double precision time0
c
c  Save the gains in the output file. This consists of:
c    * Sorting the gains into time order.
c    * Refering them to the reference antenna.
c    * Getting the inverse of the gains.
c
c  Input:
c    tvis	Handle of the output vis file.
c    Gains	The gain solutions.
c    Convrg	True if the gains are valid.
c    nants,nSols Number of antennae and number of solution intervals.
c    time	Offset time of the solution interval.
c    time0	Base time of the solution interval.
c  Scratch:
c    Indx	Used in the sorting process.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer j,j0,n,offset,item,iostat
	double precision jday
c
c  Determine the time ordering of the gain table.
c
	call sortidxr(nSols,time,Indx)
c
c  Open the gains table.
c
	call haccess(tvis,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output amp/phase table')
	  call bugno('f',iostat)
	endif
	call hwritei(item,0,0,4,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error writing header of amp/phase table')
	  call bugno('f',iostat)
	endif
c
c  Invert the gains, and refer them to the reference antenna.
c
	n = 0
	offset = 8
	do j=1,nSols
	  j0 = Indx(j)
	  if(Convrg(j0))then
	    n = n + 1
	    jday = time(j0) + time0
	    call hwrited(item,jday,offset,8,iostat)
	    offset = offset + 8
	    if(iostat.ne.0)then
	      call bug('w','Error writing time to amp/phase table')
	      call bugno('f',iostat)
	    endif
	    call hwriter(item,Gains(1,j0),offset,8*2*nants,iostat)
	    offset = offset + 8*2*nants
	    if(iostat.ne.0)then
	      call bug('w','Error writing gains to amp/phase table')
	      call bugno('f',iostat)
	    endif
	  endif
	enddo
c
c  Finished writing the gains table.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Now write out the other parameters that need to go along with this.
c
	call wrhdi(tvis,'nfeeds',2)
	call wrhdi(tvis,'ntau',0)
	call wrhdi(tvis,'ngains',2*nants)
	call wrhdi(tvis,'nsols',n)
	call wrhdd(tvis,'interval',dble(interval))
	end
c************************************************************************
	subroutine XYSol(nants,nbl,nSols,minants,
     *				SumVM,SumMM,xyp,Gains,Convrg,tol,epsi)
c
	implicit none
	integer nants,nbl,nSols,minants
	complex SumVM(4,nbl,nSols),xyp(nants),Gains(2,nants,nSols)
	real tol,epsi,SumMM(4,nbl,nSols)
	logical Convrg(nSols)
c
c  Correct the XY phases.
c
c  Input:
c    nants	Number of antennae.
c    nbl	Number of baselines.
c    nSols	Number of solution intervals.
c    minants	Minimum number of antennae.
c    SumVM,SumMM Accumulated rubbish.
c    Convrg	True if this solution is good.
c    tol	Tolerance of the solution.
c  Input/Output:
c    xyp	The current estimate of the xy phases.
c    Gains	The current estimate of the gains.
c    epsi	The worst error for this iteration.
c------------------------------------------------------------------------
	include 'gpscal.h'
	complex SVM(4,MAXBASE),G(MAXANT),phase(MAXANT)
	real Wts(MAXBASE)
	integer i,j,k,l,b1(MAXBASE),b2(MAXBASE),Indx(MAXANT)
	integer nantsd,nbld
c
c  Collapse all the solution intervals into a single one.
c
	do i=1,nbl
	  SVM(XX,i) = (0.,0.)
	  SVM(YY,i) = (0.,0.)
	  SVM(XY,i) = (0.,0.)
	  SVM(YX,i) = (0.,0.)
	  Wts(i) = 0.
	enddo
c
	do l=1,nSols
	  if(Convrg(l))then
	    k = 0
	    do j=2,nants
	      do i=1,j-1
		k = k + 1
		SVM(XX,k) = SVM(XX,k) +
     *			conjg(Gains(X,i,l))*Gains(X,j,l)*SumVM(XX,k,l)
		SVM(YY,k) = SVM(YY,k) +
     *			conjg(Gains(Y,i,l))*Gains(Y,j,l)*SumVM(YY,k,l)
		SVM(XY,k) = SVM(XY,k) +
     *			conjg(Gains(X,i,l))*Gains(Y,j,l)*SumVM(XY,k,l)
		SVM(YX,k) = SVM(YX,k) +
     *			conjg(Gains(Y,i,l))*Gains(X,j,l)*SumVM(YX,k,l)
		Wts(k) = Wts(k) + SumMM(XX,k,l) + SumMM(YY,k,l)
     *				+ SumMM(XY,k,l) + SumMM(YX,k,l)
	      enddo
	    enddo
	  endif
	enddo
c
c  Initialise some things.
c
	do i=1,nants
	  Indx(i) = 0
	enddo
c
c  Squeeze out missing baselines.
c
	nantsd = 0
	nbld = 0
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    if(Wts(k).gt.0)then
	      nbld = nbld + 1
	      if(Indx(i).eq.0)then
	        nantsd = nantsd + 1
	        Indx(i) = nantsd
	      endif
	      if(Indx(j).eq.0)then
	        nantsd = nantsd + 1
	        Indx(j) = nantsd
	      endif
	      b1(nbld) = Indx(i)
	      b2(nbld) = Indx(j)
	      SVM(XX,nbld) = SVM(XX,k)
	      SVM(YY,nbld) = SVM(YY,k)
	      SVM(XY,nbld) = SVM(XY,k)
	      SVM(YX,nbld) = SVM(YX,k)
	    endif
	  enddo
	enddo
c
c  Check!
c
	if(nantsd.lt.minants)
     *	  call bug('f','Too few antennas to solve for XY phase')
c
c  Determine the incremental XY gains.
c
	call XYSol2(nbld,nantsd,SVM,b1,b2,phase,tol,epsi)
c
c  Unpack the solutions.
c
	do i=1,nants
	  if(Indx(i).eq.0)then
	    G(i) = (1.,0.)
	  else
	    G(i) = phase(Indx(i))
	  endif
	enddo
c
c  Apply the solutions to the gains and xyphase tables.
c
	do j=1,nSols
	  do i=1,nants
	    Gains(X,i,j) = conjg(G(i)) * Gains(X,i,j)
	    Gains(Y,i,j) =       G(i)  * Gains(Y,i,j)
	  enddo
	enddo
c
	do i=1,nants
	  xyp(i) = G(i) * G(i) * xyp(i)
	enddo
c
	end
c************************************************************************
	subroutine XYSol2(nbl,nants,SumVM,b1,b2,G,tol,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	real tol,epsi
	complex SumVM(4,nbl),G(nants)
c
c  Solve for the XY phase.
c
c  Input:
c    nbl,nants	Number of baselines and number of antennae.
c    SumVM	Sum of some rubbish.
c    b1,b2	SumVM(i) corresponds to antennas b1(i),b2(i).
c    tol	Solution tolerance.
c  Input/Output:
c    epsi	Max change encountered dur8ing this iteration.
c  Output:
c    G		The xy phase gains.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer MAXITER
	parameter(MAXITER=100)
	complex Sum(MAXANT),Temp
	real Factor,Change
	integer i,niter
	logical Convrg
c
c  Initialise.
c
	do i=1,nants
	  G(i) = (1.,0.)
	  Sum(i) = (0.,0.)
	enddo
c
	Factor = 0.8
	if(nants.le.6)Factor = 0.5
c
c  Iterate.
c
	Convrg = .false.
	niter = 0
	dowhile(.not.Convrg.and.niter.lt.MAXITER)
	  niter = niter + 1
c
c  Sum the contributions.
c
	  do i=1,nbl
	    Sum(b1(i)) = Sum(b1(i))
     *	      +       G(b2(i))  * ( conjg(SumVM(XX,i)) + SumVM(YY,i) )
     *	      + conjg(G(b2(i))) * ( conjg(SumVM(XY,i)) + SumVM(YX,i) )
c
	    Sum(b2(i)) = Sum(b2(i))
     *	      +       G(b1(i))  * ( SumVM(XX,i) + conjg(SumVM(YY,i)) )
     *	      + conjg(G(b1(i))) * ( SumVM(XY,i) + conjg(SumVM(YX,i)) )
	  enddo
c
c  Update the gains.
c
	  Change = 0
c#maxloop 32
	  do i=1,nants
	    Temp = Sum(i) / abs(Sum(i))
	    Temp = G(i) + Factor * ( Temp - G(i) )
	    Temp = Temp / abs(Temp)
	    Change = Change + real( G(i) - Temp)**2
     *			    + aimag(G(i) - Temp)**2
	    G(i) = Temp
	    Sum(i) = (0.,0.)
	  enddo
	  epsi = max(epsi,Change/nants)
	  Convrg = Change/nants .lt. tol
	enddo
c	
	end
c************************************************************************
	subroutine GainSol(nants,nbl,minants,phase,xyvary,first,
     *	  SumVM,SumMM,xyp,Gains,tol,epsi,convrg)
c
	implicit none
	integer nants,nbl,minants
	logical phase,xyvary,first,convrg
	complex SumVM(4,nbl),Gains(2,nants),xyp(nants)
	real SumMM(4,nbl),tol,epsi
c
c  Determine the gain solution.
c
c  Input:
c    nants
c    nbl
c    minants
c    phase	Do phase-only solution.
c    xyvary	Allow the xy phases to vary.
c    first	Get initial estimates of the solutions.
c    SumVM
c    SumMM
c    xyp	The XY phases.
c    tol
c  Input/Output:
c    Gains
c    epsi
c  Output:
c    convrg
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer b1(MAXBASE),b2(MAXBASE),Indx(MAXANT)
	integer i,j,k,nbld,nantsd
	complex SVM(4,MAXBASE),Gx(MAXANT),Gy(MAXANT)
	real SMM(4,MAXBASE),axy(MAXANT)
c
c  Some initialising.
c
	do k=1,nants
	  Indx(k) = 0
	enddo
c
c  Squeeze out missing baselines.
c
	nantsd = 0
	nbld = 0
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    if(SumMM(XX,k).gt.0.and.SumMM(YY,k).gt.0)then
	      nbld = nbld + 1
	      if(Indx(i).eq.0)then
	        nantsd = nantsd + 1
	        Indx(i) = nantsd
	      endif
	      if(Indx(j).eq.0)then
	        nantsd = nantsd + 1
	        Indx(j) = nantsd
	      endif
	      b1(nbld) = Indx(i)
	      b2(nbld) = Indx(j)
	      SVM(XX,nbld) = SumVM(XX,k)
	      SMM(XX,nbld) = SumMM(XX,k)
	      SVM(YY,nbld) = conjg(xyp(i))*xyp(j)*SumVM(YY,k)
	      SMM(YY,nbld) = SumMM(YY,k)
	      SVM(XY,nbld) = xyp(j)*SumVM(XY,k)
	      SMM(XY,nbld) = SumMM(XY,k)
	      SVM(YX,nbld) = conjg(xyp(i))*SumVM(YX,k)
	      SMM(YX,nbld) = SumMM(YX,k)
	    endif
	  enddo
	enddo
c
c  Fill in the current estimates of everything.
c
	do i=1,nants
	  if(Indx(i).gt.0)then
	    Gx(Indx(i)) = Gains(X,i)
	    Gy(Indx(i)) = Gains(Y,i) * conjg(xyp(i))
	    axy(Indx(i)) = abs(Gains(Y,i) / Gains(X,i))
	  endif
	enddo
c
c  Call the solving routine.
c
	convrg = .true.
	if(minants.gt.nantsd)then
	  convrg = .false.
	else if(xyvary)then
	  if(first)then
	    call AmpInSol(nbld,nantsd,SVM,SMM,b1,b2,Gx,axy,epsi)
	    do i=1,nantsd
	      Gy(i) = axy(i)*Gx(i)
	    enddo
	  endif
	  call AmpSolXY(nbld,nantsd,SVM,SMM,b1,b2,Gx,Gy,tol,epsi)
	else if(phase)then
	  call PhaseSol(nbld,nantsd,SVM,b1,b2,Gx,tol,epsi)
	else
	  if(first)call AmpInSol(nbld,nantsd,SVM,SMM,b1,b2,Gx,axy,epsi)
	  call AmpSol(nbld,nantsd,SVM,SMM,b1,b2,Gx,axy,tol,epsi)
	endif
c
c  If it converged, unpack the gain solutions.
c
	if(convrg)then
	  do i=1,nants
	    if(Indx(i).eq.0)then
	      Gains(X,i) = (0.,0.)
	      Gains(Y,i) = (0.,0.)
	    else
	      Gains(X,i) = Gx(Indx(i))
	      if(xyvary)then
		Gains(Y,i) = xyp(i) * Gy(Indx(i))
	      else
	        Gains(Y,i) = axy(Indx(i))*xyp(i)*Gx(Indx(i))
	      endif
	    endif
	  enddo
	endif
c
c  All dead and buried.
c
	end
c************************************************************************
	subroutine Accum(tvis,nchan,nbl,maxSol,nSols,Hash,Indx,maxHash,
     *	  interval,Time,Time0,Count,SumVM,SumMM)
c
	implicit none
c
	integer tvis,nchan,nbl,maxSol,nSols
	integer maxHash,Hash(maxHash),Indx(maxHash)
	real interval,Time(maxSol),SumMM(4,nbl,maxSol)
	integer Count(maxSol)
	complex SumVM(4,nbl,maxSol)
	double precision Time0
c
c  Accumulate the statistics needed to solve the polarisation selfcal
c  problem.
c
c  Input:
c    tvis
c    nchan
c    nbl
c    maxSol
c    maxHash	Size of the scratch hash tables.
c    interval	Solution interval size.
c  Scratch:
c    Hash
c    Indx
c  Output:
c    nSols	Number of solution intervals.
c    Tim0	Base time.
c    Time	The average time of the solution interval, offset
c		by time0.
c    Count	The number of correlations added to this solution
c		interval.
c    SumVM	The sum of the data times the conjugate of the model.
c		This is summed for a baseline for a solution interval, with
c		1/sigma**2 as the weight.
c    SumMM	The sum of the modulus of the model.
c------------------------------------------------------------------------
	include 'gpscal.h'
	complex Data(MAXCHAN),Model(MAXCHAN)
	logical flags(MAXCHAN)
	integer i,k,bl,pol,nHash,itime,ihash,nread
	real sigma2,wt
	double precision t
c
c  Externals.
c
	integer prime
c
c  Initialise the hash table, and the number of solutions.
c
	nHash = prime(maxHash-1)
	do i=1,nHash+1
	  Hash(i) = 0
	enddo
	nSols = 0
c
c  Read all the data, and accumulate the needed statistics.
c
	call Compute(tvis,t,bl,pol,sigma2,Data,Model,flags,
     *							nchan,nread)
	time0 = int(t) + 0.5
	dowhile(nread.gt.0)
	  itime = nint((t-time0)/interval)
	  ihash = 2*itime + 1
	  i = mod(itime,nHash)
	  if(i.le.0)i = i + nHash
c
c  Dind this entry in the hash table.
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
c  If it was not in the hash table, add a new entry.
c
	  if(Hash(i).eq.0)then
	    nSols = nSols + 1
	    if(nSols.ge.maxSol)call bug('f','Solution table overflow')
	    Hash(i) = ihash
	    Indx(i) = nSols
	    Time(nSols) = 0
	    Count(nSols) = 0
	    do k=1,nbl
	      SumVM(XX,k,nSols) = (0.,0.)
	      SumVM(YY,k,nSols) = (0.,0.)
	      SumVM(XY,k,nSols) = (0.,0.)
	      SumVM(YX,k,nSols) = (0.,0.)
	      SumMM(XX,k,nSols) = 0.
	      SumMM(YY,k,nSols) = 0.
	      SumMM(XY,k,nSols) = 0.
	      SumMM(YX,k,nSols) = 0.
	    enddo
	  endif
c
c  We have found the slot containing the info. Accumulate info into
c  it about this visibility record.
c
	  i = Indx(i)
	  wt = 0.5/sigma2
	  do k=1,nchan
	    if(flags(k))then
	      SumVM(pol,bl,i) = SumVM(pol,bl,i) +
     *		wt * Data(k) * conjg(Model(k))
	      SumMM(pol,bl,i) = SumMM(pol,bl,i) +
     *		wt * (real(Model(k))**2 + aimag(Model(k))**2)
	      Count(i) = Count(i) + 1
	      Time(i) = Time(i) + real(t-Time0)
	    endif
	  enddo
	  call Compute(tvis,t,bl,pol,sigma2,Data,Model,flags,
     *							nchan,nread)
	enddo
c
c  All the data has been accumulated. Calculate the average time.
c
	do i=1,nSols
	  if(Count(i).gt.0) Time(i) = Time(i) / Count(i)
	enddo
c
	end
c************************************************************************
	subroutine CompIni(tscr,D,nants)
c
	integer tscr(4),nants
	complex D(2,nants)
c
c  Initialise, ready for the model computation.
c
c  Input:
c    tscr	Handles of the scratch files.
c------------------------------------------------------------------------
	include 'gpscal.h'
c
	integer i,j,k
c
	complex Models(MAXCHAN,4)
	integer visno,scr(4)
	complex a(6,4,MAXBASE)
	common/CompC/Models,a,visno,scr
c
c  Initialise things to zero.
c
	do j=1,4
	  scr(j) = tscr(j)
	enddo
c
c  Determine the coefficients that convert between Stokes and raw
c  linears. These are a function of baseline, to deal with the leakages.
c  The coefficients are
c    V = a(1)*I + a(2)*V + Q*(a(3)*cos(phi)+a(4)*sin(phi))
c			 + U*(a(5)*cos(phi)+a(6)*sin(phi))
c
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    a(1,XX,k) = 1
	    a(2,XX,k) = (0., 1.)*(D(X,i) - conjg(D(X,j)))
	    a(3,XX,k) = 1
	    a(4,XX,k) = -(D(X,i) + conjg(D(X,j)))
	    a(5,XX,k) =  (D(X,i) + conjg(D(X,j)))
	    a(6,XX,k) = 1
c
	    a(1,YY,k) =  1
	    a(2,YY,k) = (0.,-1.)*(D(Y,i) - conjg(D(Y,j)))
	    a(3,YY,k) = -1
	    a(4,YY,k) = -(D(Y,i) + conjg(D(Y,j)))
	    a(5,YY,k) =  (D(Y,i) + conjg(D(Y,j)))
	    a(6,YY,k) = -1
c
	    a(1,XY,k) =  (D(X,i) + conjg(D(Y,j)))
	    a(2,XY,k) =  (0.,-1.)
	    a(3,XY,k) = -(D(X,i) - conjg(D(Y,j)))
	    a(4,XY,k) = -1
	    a(5,XY,k) =  1
	    a(6,XY,k) = -(D(X,i) + conjg(D(Y,j)))
c
	    a(1,YX,k) =  (D(Y,i) + conjg(D(X,j)))
	    a(2,YX,k) =  (0., 1.)
	    a(3,YX,k) =  (D(Y,i) - conjg(D(X,j)))
	    a(4,YX,k) = -1
	    a(5,YX,k) =  1
	    a(6,YX,k) =  (D(Y,i) - conjg(D(X,j)))
	  enddo
	enddo
c
c  Remember that we have not read anything yet.
c
	visno = 0
	end
c************************************************************************
	subroutine Compute(tvis,time,bl,pol,wt,Data,Model,flags,
     *							nchan,nread)
c
	implicit none
	integer tvis,pol,bl,nchan,nread
	double precision time
	complex Data(nchan),Model(nchan)
	real wt
	logical flags(nchan)
c
c  Compute the model visibilities.
c
c  Input:
c    tvis	Handle of the input visibility file.
c    nchan	Number of channels.
c  Output:
c    time	Integration time for this visibility.
c    bl		Baseline number for the visibility. This is equal to
c		  (i2-1)*(i2-2)/2 + i1,
c		where i1 is less than i2.
c    pol	Data polarisation type. This takes values 1 to 4,
c		corresponding to XX,YY,XY and YX respectively.
c    wt		Variance of the data.
c    Data	The raw visibilities.
c    Model	The model visibilities.
c    flags	Flags.
c    nread	Number of channels read.
c------------------------------------------------------------------------`
	include 'gpscal.h'
c
	complex Models(MAXCHAN,4)
	integer visno,scr(4)
	complex a(6,4,MAXBASE)
	common/CompC/Models,a,visno,scr
c
	integer baseline
	logical Modflags(MAXCHAN)
	double precision time0,sigma2
	real Cos2Chi,Sin2Chi
	save Modflags,baseline,time0,Cos2Chi,Sin2Chi,sigma2
c
	double precision preamble(5)
	complex aI,aQ,aU,aV
	logical more
	integer i,j,i0,i1,i2
	real Out(5*MAXCHAN),chi
c
c  Read some good data.
c
	more = .true.
	dowhile(more)
	  call uvread(tvis,preamble,Data,flags,nchan,nread)
	  if(nread.le.0)return
	  call basant(preamble(5),i1,i2)
	  more = i1.eq.i2
	enddo
	time = preamble(4)
	bl = nint(preamble(5))
c
	if(nread.ne.nchan)
     *	  call bug('f','Software inconsistency, in Compute')
c
	if(visno.eq.0)then
	  baseline = 0
	  time0 = 0
	  do j=1,4
	    if(scr(j).eq.0)then
	      do i=1,MAXCHAN
	        Models(i,j) = 0
	      enddo
	    endif
	  enddo
	endif
c
c  See if we have the required data. Assume we have the required data if
c  the baseline numbers are the same, and if the times are within a
c  quarter of a second.
c
	if(visno.eq.0.or.bl.ne.baseline.or.
     *		     abs(time-time0).gt.0.25/(24.*3600.))then
	  do i=1,nchan
	    Modflags(i) = .true.
	  enddo
c
	  do j=1,4
	    if(scr(j).ne.0)then
	      call scrread(scr(j),Out,5*visno*nchan,5*nchan)
	      i0 = 1
	      do i=1,nchan
	        Models(i,j) = cmplx(Out(i0+2),Out(i0+3))
	        Modflags(i) = Modflags(i).and.(Out(i0+4).gt.0)
	        i0 = i0 + 5
	      enddo
	    endif
	  enddo
c
c  Get the parallactic angle and variance.
c
	  call uvrdvrr(tvis,'chi',chi,0.)
	  Cos2Chi = cos(2*chi)
	  Sin2Chi = sin(2*chi)
	  call uvinfo(tvis,'variance',sigma2)
	  if(sigma2.le.0)sigma2 = 1
c
c  Update the description of these records.
c
	  visno = visno + 1
	  time0 = time
	  baseline = nint(preamble(5))
	endif
c
c  Return the variance and the polarisation type.
c
	wt = sigma2
	call uvrdvri(tvis,'pol',pol,0)
	pol = -pol - 4
	if(pol.lt.XX.or.pol.gt.YX)then
	  call bug('w',
     *		'Data contains an unsupported polarisation type')
	  call bug('f','Data must be raw linear polarisations')
	endif
	bl = (i2-1)*(i2-2)/2 + i1
c
c  All appears to be well. Now compute the model.
c
	aI = a(1,pol,bl)
	aQ = a(3,pol,bl)*Cos2Chi + a(4,pol,bl)*Sin2Chi
	aU = a(5,pol,bl)*Cos2Chi + a(6,pol,bl)*Sin2Chi
	aV = a(2,pol,bl)
	do i=1,nchan
	  Model(i) = aI*Models(i,PolI) + aQ*Models(i,PolQ) +
     *		     aU*Models(i,PolU) + aV*Models(i,PolV)
	  flags(i) = flags(i).and.Modflags(i)
	enddo
c
	end
c************************************************************************
	subroutine PolIni(tIn,D,xyp,nants)
c
	implicit none
	integer tIn,nants
	complex D(2,nants),xyp(nants)
c
c  Set the initial estimates of the gains and leakage terms.
c
c  Inputs:
c    tIn	Handle of the input file.
c    nants	Number of antennae.
c  Output:
c    D		The initial leakage terms.
c    xyp	The initial xyphases.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer i,n,item,iostat,count(MAXANT)
	real theta,phase(MAXANT),sd(MAXANT)
c
c  Externals.
c
	integer hsize
c
c  See if there is an XY phase table associated with this data file.
c  If so use this.
c
	call GetXY(tIn,phase,sd,count,MAXANT,n)
	n = min(n,nants)
	do i=1,n
	  if(count(i).gt.0)then
	    theta = phase(i)
	    xyp(i) = cmplx(cos(theta),sin(theta))
	  else
	    xyp(i) = (1.,0.)
	  endif
	enddo
c
	do i=n+1,nants
	  xyp(i) = (1.,0.)
	enddo
c
c  Initialise the leakage terms. See if there is already a leakage
c  table in the input file. If so use this as the initial estimate.
c  Otherwise just set things to zero.
c
	call haccess(tIn,item,'leakage','read',iostat)
	if(iostat.eq.0)then
	  call output(
     *	    'Using leakage parameters from vis file')
	  n = min((hsize(item)-8)/16,nants)
	  call hreadr(item,D,8,16*n,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error reading leakage table from input')
	    call bugno('f',iostat)
	  endif
	  call hdaccess(item,iostat)
	else
	  n = 0
	endif
	do i=n+1,nants
	  D(X,i) = (0.,0.)
	  D(Y,i) = (0.,0.)
	enddo
c
	end
c************************************************************************
	subroutine GainIni(nants,nSols,Gains,xyp)
c
	implicit none
	integer nants,nSols
	complex Gains(2,nants,nSols),xyp(nants)
c
c  Initialise the gains.
c
c  Input:
c    nants
c    nSols
c    xyp
c  Output:
c    Gains
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer i,j
	do j=1,nSols
	  do i=1,nants
	    Gains(X,i,j) = (1.,0.)
	    Gains(Y,i,j) = xyp(i)
	  enddo
	enddo
	end
c************************************************************************
	subroutine PhaseSol(nbl,nants,SumVM,b1,b2,G,tol,epsi)
c
	implicit none
	integer nbl,nants
	integer b1(nbl),b2(nbl)
	real tol,epsi
	complex SumVM(4,nbl),G(nants)
c
c  Solve for the phase corrections which minimise the error. This uses
c  a nonlinear Jacobi iteration, as suggested by Fred Schwab in "Adaptive
c  calibration of radio interferomter data", SPIE Vol. 231, 1980
c  International Optical Computing Conference (pp 18-24). The damping
c  heuristics are copied from AIPS ASCAL.
c
c  Input:
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of conjg(Vis)*Model
c    tol	Convergence tolerance.
c  Input/Output:
c    Gain	The antenna gain solution.
c    epsi	Chanle in solution during the iterations.
c------------------------------------------------------------------------
	include 'gpscal.h'
	complex SVM(MAXBASE),Sum(MAXANT)
	logical convrg
	real Factor,Change
	complex Temp
	integer Niter,i
c
	integer MAXITER
	parameter(MAXITER=100)
c
c  Initialise. Remember the ratio of the Y to X gains, and then
c  sum the Y and X parts together.
c
	do i=1,nants
	  Sum(i) = 0
	enddo
c
	do i=1,nbl
	  SVM(i) = SumVM(XX,i) + SumVM(YY,i)
	enddo
c
	Factor = 0.8
	if(Nants.le.6)Factor = 0.5
c
c  Iterate.
c
	Convrg = .false.
	Niter = 0
	do while(.not.Convrg.and.Niter.lt.MAXITER)
	  Niter = Niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
	  do i=1,nbl
	    Sum(b1(i)) = Sum(b1(i)) + G(b2(i)) *       SVM(i)
	    Sum(b2(i)) = Sum(b2(i)) + G(b1(i)) * conjg(SVM(i))
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
	    Temp = G(i) + Factor * ( Temp - G(i) )
	    Temp = Temp/abs(Temp)
	    Change = Change + real(G(i)-Temp)**2
     *			    + aimag(G(i)-Temp)**2
	    G(i) = Temp
	    Sum(i) = (0.,0.)
	  enddo
	  epsi = max(epsi,Change/nants)
	  Convrg = Change/nants .lt. tol
	enddo
c
	end
c************************************************************************
	subroutine AmpInSol(nbl,nants,SumVM,SumMM,b1,b2,G,axy,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	complex SumVM(4,nbl),G(nants)
	real SumMM(4,nbl),axy(nants),epsi
c
c  Get the first estimate at the amplitude gain. Do this by getting the
c  phase-only solution, and then scaling to the average amplitude.
c
c  Input:
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    SumVM,SumMM Accumulated rubbish used in the solution step.
c    b1,b2	Antenna numbers for each sum.
c  Input/Output:
c    G		The gains.
c    epsi	Solution change.
c------------------------------------------------------------------------
	include 'gpscal.h'
	real tol
	parameter(tol=1e-5)
	integer i
	real SumRMM,SumRVM,alpha
c
c  Get the phase solution.
c
	call PhaseSol(nbl,nants,SumVM,b1,b2,G,tol,epsi)
c
c  Determine a scale factor to multiply the gains by, to make them about
c  the correct amplitude.
c
	SumRVM = 0
	SumRMM = 0
	do i=1,nbl
	  SumRVM = SumRVM + conjg(G(b1(i)))*G(b2(i))*
     *	     ( SumVM(XX,i) + axy(b1(i))*axy(b2(i))*SumVM(YY,i) )
	  SumRMM = SumRMM +   G(b1(i))*G(b2(i))  *
     *			conjg(G(b1(i))*G(b2(i))) *
     *	      (SumMM(XX,i) + (axy(b1(i))*axy(b2(i)))**2 * SumMM(YY,i))
	enddo
	alpha = sqrt( abs(SumRVM / SumRMM) )
c
	do i=1,nants
	  G(i) = alpha * G(i)
	enddo
c
	end
c************************************************************************
	subroutine AmpSolXY(nbl,nants,SumVM,SumMM,b1,b2,Gx,Gy,tol,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	complex SumVM(4,nbl),Gx(nants),Gy(nants)
	real SumMM(4,nbl),epsi,tol
c
c  This determines the amplitude/phase solution for a given time interval.
c  The complex gain of the X and Y feeds are determined. The XY phase or
c  amplitude ratio is not constrained.
c
c  Inputs:
c    nbl,nants	Number of baselines and number of antennae.
c    SumMM,SumVM Accumulated rubbish used in the solution process.
c    b1,b2	Antenna numbers of the given baseline.
c    tol	Tolerance in determining the solutions.
c  Input/Output:
c    Gx		Current estimate of the X gains.
c    Gy		Current estimate of the Y gains.
c  Output:
c    epsi	Fractional change in gains.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer MAXITER
	parameter(MAXITER=100)
c
	integer i,niter
	logical convrg
	real t,Factor,ChangeX,ChangeY,SumWtX,SumWtY
	real Sum2(2,MAXANT)
	complex Sum(2,MAXANT),Temp
c
	real Factors(11)
	data Factors/0.5,0.75,8*0.9,0.5/
c
c  Initialise.
c
	do i=1,nants
	  Sum(X,i) = (0.,0.)
	  Sum(Y,i) = (0.,0.)
	  Sum2(X,i) = 0.
	  Sum2(Y,i) = 0.
	enddo
c
	convrg = .false.
	niter = 0
	dowhile(.not.convrg.and.niter.lt.maxiter)
	  niter = niter + 1
c
c  Get the same damping factor as AIPS.
c
	  if(nants.le.6)then
	    factor = 0.5
	  else
	    factor = factors(min(11,niter))
	  endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
	  do i=1,nbl
	    Sum(X,b1(i))  = Sum(X,b1(i))
     *				+ Gx(b2(i)) *	    SumVM(XX,i)
     *				+ Gy(b2(i)) *       SumVM(XY,i)
	    Sum(X,b2(i))  = Sum(X,b2(i))
     *				+ Gx(b1(i)) * conjg(SumVM(XX,i))
     *				+ Gy(b1(i)) * conjg(SumVM(YX,i))
c
	    Sum2(X,b1(i)) = Sum2(X,b1(i))
     *	      + (real(Gx(b2(i)))**2 + aimag(Gx(b2(i)))**2)*SumMM(XX,i)
     *	      + (real(Gy(b2(i)))**2 + aimag(Gy(b2(i)))**2)*SumMM(XY,i)
	    Sum2(X,b2(i)) = Sum2(X,b2(i)) +
     *	      + (real(Gx(b1(i)))**2 + aimag(Gx(b1(i)))**2)*SumMM(XX,i)
     *	      + (real(Gy(b1(i)))**2 + aimag(Gy(b1(i)))**2)*SumMM(YX,i)
c
	    Sum(Y,b1(i))  = Sum(Y,b1(i))
     *				+ Gy(b2(i)) *       SumVM(YY,i)
     *				+ Gx(b2(i)) *       SumVM(YX,i)
	    Sum(Y,b2(i))  = Sum(Y,b2(i))
     *				+ Gy(b1(i)) * conjg(SumVM(YY,i))
     *				+ Gx(b1(i)) * conjg(SumVM(XY,i))
c
	    Sum2(Y,b1(i)) = Sum2(Y,b1(i))
     *	      + (real(Gy(b2(i)))**2 + aimag(Gy(b2(i)))**2)*SumMM(YY,i)
     *	      + (real(Gx(b2(i)))**2 + aimag(Gx(b2(i)))**2)*SumMM(YX,i)
	    Sum2(Y,b2(i)) = Sum2(Y,b2(i)) +
     *	      + (real(Gy(b1(i)))**2 + aimag(Gy(b1(i)))**2)*SumMM(YY,i)
     *	      + (real(Gx(b1(i)))**2 + aimag(Gx(b1(i)))**2)*SumMM(XY,i)
	  enddo
c
c  Update the gains.
c
	  ChangeX = 0
	  SumWtX = 0
	  ChangeY = 0
	  SumWtY = 0
c#maxloop 32
	  do i=1,nants
c
c  Evaluate X and Y gains.
c
	    Temp = Sum(X,i) / Sum2(X,i) - Gx(i)
	    Gx(i) = Gx(i) + Factor * Temp
	    ChangeX = ChangeX + real(Temp)**2 + aimag(Temp)**2
	    SumWtX = SumWtX + real(Gx(i))**2  + aimag(Gx(i))**2
c
	    Temp = Sum(Y,i) / Sum2(Y,i) - Gy(i)
	    Gy(i) = Gy(i) + Factor * Temp
	    ChangeY = ChangeY + real(Temp)**2 + aimag(Temp)**2
	    SumWtY = SumWtY + real(Gy(i))**2  + aimag(Gy(i))**2
c
c  Zero the accumulators.
c
	    Sum(X,i) = 0
	    Sum(Y,i) = 0
	    Sum2(X,i) = 0
	    Sum2(Y,i) = 0
	  enddo
	  t = max(ChangeX/SumWtX,ChangeY/SumWtY)
	  epsi = max(epsi,t)
	  convrg = t.lt.tol
	enddo
c
	end
c************************************************************************
	subroutine AmpSol  (nbl,nants,SumVM,SumMM,b1,b2,G,axy,tol,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	complex SumVM(4,nbl),G(nants)
	real SumMM(4,nbl),axy(nants),epsi,tol
c
c  This determines the amplitude/phase solution for a given time interval.
c  The complex gain of the X feed, and the X/Y amplitude gain ratio is
c  determined. It it assumed that the XY phases have already been applied
c  to the YY data.
c
c  Inputs:
c    nbl,nants	Number of baselines and number of antennae.
c    SumMM,SumVM Accumulated rubbish used in the solution process.
c    b1,b2	Antenna numbers of the given baseline.
c    tol	Tolerance in determining the solutions.
c  Input/Output:
c    G		Current estimate of the X gains.
c    Axy	Current estimate of the X/Y amplitude ratio.
c  Output:
c    epsi	Fractional change in gains.
c------------------------------------------------------------------------
	include 'gpscal.h'
	integer MAXITER
	parameter(MAXITER=100)
c
	integer i,niter
	logical convrg
	real t,Factor,ChangeX,ChangeY,SumWtX,SumWtY
	real Sum2(2,MAXANT)
	complex Sum(2,MAXANT),Temp
c
	real Factors(11)
	data Factors/0.5,0.75,8*0.9,0.5/
c
c  Initialise.
c
	do i=1,nants
	  Sum(X,i) = (0.,0.)
	  Sum(Y,i) = (0.,0.)
	  Sum2(X,i) = 0.
	  Sum2(Y,i) = 0.
	enddo
c
	convrg = .false.
	niter = 0
	dowhile(.not.convrg.and.niter.lt.maxiter)
	  niter = niter + 1
c
c  Get the same damping factor as AIPS.
c
	  if(nants.le.6)then
	    factor = 0.5
	  else
	    factor = factors(min(11,niter))
	  endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
	  do i=1,nbl
	    Sum(X,b1(i))  = Sum(X,b1(i)) +
     *			   G(b2(i)) *       SumVM(XX,i)
	    Sum(X,b2(i))  = Sum(X,b2(i)) +
     *			   G(b1(i)) * conjg(SumVM(XX,i))
c
	    Sum2(X,b1(i)) = Sum2(X,b1(i)) +
     *	      (real(G(b2(i)))**2 + aimag(G(b2(i)))**2)*SumMM(XX,i)
	    Sum2(X,b2(i)) = Sum2(X,b2(i)) +
     *	      (real(G(b1(i)))**2 + aimag(G(b1(i)))**2)*SumMM(XX,i)
c
	    Sum(Y,b1(i))  = Sum(Y,b1(i)) +
     *	      Axy(b2(i)) * G(b2(i)) *       SumVM(YY,i)
	    Sum(Y,b2(i))  = Sum(Y,b2(i)) +
     *	      Axy(b1(i)) * G(b1(i)) * conjg(SumVM(YY,i))
c
	    Sum2(Y,b1(i)) = Sum2(Y,b1(i)) + Axy(b2(i))**2 *
     *	      (real(G(b2(i)))**2 + aimag(G(b2(i)))**2)*SumMM(YY,i)
	    Sum2(Y,b2(i)) = Sum2(Y,b2(i)) + Axy(b1(i))**2 *
     *	      (real(G(b1(i)))**2 + aimag(G(b1(i)))**2)*SumMM(YY,i)
	  enddo
c
c  Update the gains.
c
	  ChangeX = 0
	  SumWtX = 0
	  ChangeY = 0
	  SumWtY = 0
c#maxloop 32
	  do i=1,nants
c
c  Evaluate X gain
c
	    t = 1./(Sum2(X,i) + Axy(i)*Axy(i)*Sum2(Y,i))
	    Temp = t * ( Sum(X,i) + Axy(i)*Sum(Y,i) ) - G(i)
	    G(i) = G(i) + Factor * Temp
	    ChangeX = ChangeX + real(Temp)**2 + aimag(Temp)**2
	    SumWtX = SumWtX + real(G(i))**2  + aimag(G(i))**2
c
c  Evaluate Y amplitude.
c
	    t = real(conjg(G(i))*Sum(Y,i)) /
     *	        ((real(G(i))**2+aimag(G(i))**2) * Sum2(Y,i)) - Axy(i)
	    t = max(t,-0.5*Axy(i))
	    Axy(i) = Axy(i) + Factor * t
	    ChangeY = ChangeY + t*t
	    SumWtY = SumWtY + Axy(i)*Axy(i)
c
c  Zero the accumulators.
c
	    Sum(X,i) = 0
	    Sum(Y,i) = 0
	    Sum2(X,i) = 0
	    Sum2(Y,i) = 0
	  enddo
	  t = max(ChangeX/SumWtX,ChangeY/SumWtY)
	  epsi = max(epsi,t)
	  convrg = t.lt.tol
	enddo
c
	end

