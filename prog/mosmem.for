c************************************************************************
	program mosmem
	implicit none
c
c= mosmem - Maximum Entropy deconvolution for a mosaiced image
c& rjs
c: deconvolution
c+
c	MOSMEM is a MIRIAD task which performs a maximum entropy
c	deconvolution of a mosaiced image. Optionally it can also perform
c	a joint deconvolution of a mosaic and single dish image.
c@ map
c	One or perhaps two input dirty images (or cubes). These should have
c	units of Jy/beam. The first should be produced by INVERTs mosaic mode.
c	The optional second dirty map can be a single-dish image. It must
c	be on exactly the same pixel grid as the first image. If necessary,
c	use REGRID to make this so. If two inputs are given, then a joint
c	deconvolution of the two is performed.
c@ beam
c	One or perhaps two input dirty beams. The first, corresponding to the
c	first input dirty map, will be produced by INVERTs mosaic mode. There
c	is no default. The second dirty beam (which must be given if there
c	are two dirty map inputs) gives the point-spread function of the
c	single dish dirty map. This second dirty beam need not be the same
c	image size as the input dirty maps, and may be appreciably smaller.
c	This single-dish beam is assumed to be position-independent, but it
c	need not be symmetric.
c@ model
c	An initial estimate of the deconvolved image. For point sources,
c	giving a good initial model may help convergence. In principle,
c	this only helps convergence, but should not affect the final
c	solution. The model could be the output from a previous run of
c	MOSMEM or any other deconvolution task. It must have flux units of
c	Jy/pixel. The default is a flat estimate, with the correct flux.
c@ default
c	The default image. This is the image that the final solution will
c	tend towards. The final result will be influenced by this default
c	if the constrains that the data put on the solution are weak.
c	The default is a flat estimate, with the correct flux.
c@ out
c	The name of the output map. The units of the output will be Jy/pixel.
c	It can be input to RESTOR to produce a restored image, or alternatively
c	to MOSMEM, as a model, to continue the deconvolution process.
c@ niters
c	The maximum number of iterations. The default is 30.
c@ region
c	This specifies the region to be deconvolved. See the Users Manual
c	for instructions on how to specify this. The default is the entire
c	image.
c@ measure
c	The entropy measure to be used, either "gull" (-p*log(p/e)) or
c	"cornwell" (-log(cosh(p)) -- also called the maximum emptiness
c	criteria). Using the maximum emptiness criteria is not recommended.
c@ tol
c	Tolerance of solution. There is no need to change this from the
c	default of 0.01.
c@ q
c	One or two values (corresponding to the mosaic and single dish
c	observations). These give estimates of the number of points per beam.
c	MOSMEM can usually come up with a good, image-dependent estimate.
c@ rmsfac
c	MOSMEM must be able to the theoretical rms noise of the input dirty
c	map(s), and will, by default, attempt to reduce the residuals to have
c	the same rms as this. If the true rms noise is different from the 
c	theoretical, you may give the factor to multiply by to convert from
c	theoretical to true rms noise.
c
c	The theoretical rms will usually be an optimistic estimate of the
c	true noise level. The true noise will be increased by calibration
c	errors, confusion, poorly understood distant sidelobes, etc.
c	The rmsfac factor gives some `fudge factor' (usually greater than 1)
c	to scale the theoretical noise estimate by. Either one or two values
c	can be given, with the second value corresponding to the single dish
c	input.
c
c	For a mosaic, the theoretical rms is position dependent, and is
c	determined from information save by INVERT (the mostable table).
c	For a single dish image, the rms is assumed to be constant across
c	the field, and given by the "rms" item in the image. If the single
c	dish input does not contain this item, then this must be added
c	before using MOSMEM. This is easily done: for image xxxx, use
c	  puthd in=xxxx/rms value=????
c	where "????" is the rms noise in Jy/beam.
c@ factor
c	The flux calibration factor. This is only relevant when doing a
c	joint deconvolution of a mosaic and a single-dish image. It gives the
c	factor which the single-dish data should be multiplied by to convert
c	it to the same flux units as the mosaic. The default is 1. If the
c	``dofactor'' options is used (see below), MOSMEM solves for this
c	parameter.
c@ flux
c	An estimate of the integrated flux of the source. This parameter is
c	generally not useful if there is an input single dish image. 
c	Giving MOSMEM a good value for the integrated flux
c	will help it find a good solution. On the other hand, giving
c	a poor value may do harm. Normally MOSMEM will NOT constrain the
c	integrated flux to be this value, but see the ``doflux'' option below.
c	The default is image-dependent for measure=gull, and zero for
c	measure=cornwell. A value can be given for each plane being
c	deconvolved.
c@ options
c	Task enrichment parameters. Several can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  doflux     Constrain the solution to have the correct integrated flux
c	             (normally the integrated flux is not constrained). The
c	             integrated flux is determined from the "flux" parameter or
c	             (if no flux parameter is given) from the default image.
c	             This option cannot be used if a single dish input map is
c	             also given.
c	  dofactor   Solve for the flux calibration factor.
c	  verbose    Give lots of messages during the iterations. The default
c	             is to give a one line message at each iteration.
c--
c  History:
c    rjs  23nov94  Adapted from MAXEN.
c    rjs   3dec94  Doc only.
c    rjs   6feb95  Copy mosaic table to output component file.
c    rjs  10aug95  New routine to modify alpha and beta.
c    rjs  12oct95  Support "default" and "model" being different sizes from
c		   the deconvolved region.
c    rjs  27oct95  Increased max length of filenames.
c    rjs  24nov95  Default default image is now proportional to the gain.
c    rjs  29Feb96  Call xyflush after each plane.
c    mwp  27May97  Allow flux estimates for each plane.
c    rjs  21jun97  Tidies up above change.
c    rjs  24jun97  Correct call to alignini
c    rjs  02jul97  cellscal change.
c    rjs  23jul97  Add pbtype.
c    rjs  01aug97  Fiddle default to make it always reasonably valued.
c    rjs  28nov97  Increase max number of boxes.
c    rjs  23feb98  Added extra protection against underflow.
c    rjs  17mar98  Added single dish support, "factor" parameter and
c		   options=dofactor.
c    rjs  19jan99  It was failing to access the correct single dish plane!!
c		   Also some changes in some checks and guessing TFlux to
c		   make it more robust.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='MosMem: version 1.0 19-Jan-99')
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	integer MaxRun,MaxBoxes
	parameter(MaxRun=3*maxdim,MaxBoxes=2048)
c
	integer gull,cornwell
	parameter(gull=1,cornwell=2)
c
	character MapNam*64,BeamNam*64,ModelNam*64,OutNam*64,DefNam*64
	character MapSin*64,BeamSin*64
	character entropy*8,line*80
	integer lBeama,lMapa,lBeamb,lMapb,lModel,lOut,lDef
	integer nMap(3),nMapb(3),nModel(3),nOut(MAXNAX),nBeam(3),nDef(3)
	integer xmin,ymin,xmax,ymax,n1,n2,i
	integer imin,imax,jmin,jmax,kmin,kmax,blc(3),trc(3),naxis,k
	integer icentre,jcentre
	integer maxniter,niter
	integer measure
	real Tol,rmsfaca,rmsfacb,TFlux,Qest,Qa,Qb,TRms,Trms2
	real ffacSD,ffacDef,Alpha,Beta,De,Df,temp
	real StLim,StLen1,StLen2,OStLen1,OStLen2,J0,J1
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb
	real fluxlist(maxdim),fac
	integer nfret
	logical converge,positive,verbose,doflux,dosingle,dofac
	integer Run(3,MaxRun),nRun,Boxes(maxBoxes),nPoint,maxPoint
	integer xmoff,ymoff,zmoff,xdoff,ydoff,zdoff
c
	integer pMapa,pMapb,pEst,pDef,pResa,pResb
	integer pNewEst,pNewResa,pNewResb,pWta,pWtb,Cnvl
c
c  Externals.
c
	character itoaf*4
	logical hdprsnt
	integer len1
c
c  Get and check the input parameters.
c
	call output(version)
	call keyini
	call keya('map',MapNam,' ')
	call keya('map',MapSin,' ')
	call keya('beam',BeamNam,' ')
	call keya('beam',BeamSin,' ')
	call keya('out',OutNam,' ')
	if(MapNam.eq.' '.or.BeamNam.eq.' '.or.OutNam.eq.' ')
     *	  call bug('f','A file name was missing from the parameters')
	if(MapSin.eq.' '.neqv.BeamSin.eq.' ')call bug('f',
     *	  'Input single dish map and beam names should be given')
	dosingle = MapSin.ne.' '
	call keya('model',ModelNam,' ')
	call keya('default',DefNam,' ')
	call keyr('tol',Tol,0.01)
	call keyi('niters',maxniter,30)
	call keyr('q',Qa,0.)
	qb = 0
	if(dosingle)call keyr('q',Qb,0.)
	call keyr('rmsfac',rmsfaca,1.)
	rmsfacb = 1
	if(dosingle)call keyr('rmsfac',rmsfacb,1.)
	nfret = 0
	call mkeyr('flux',fluxlist,maxdim,nfret)
	if(dosingle.and.nfret.gt.0)then
	  call bug('w','Setting the "flux" parameter when using '//
     *			'single-dish data may be unwise')
	endif
	call BoxInput('region',MapNam,Boxes,MaxBoxes)
	call GetOpt(verbose,doflux,dofac,entropy)
	if(dofac.and..not.dosingle)call bug('f',
     *	  'options=dofactor is valid only with joint deconvolutions')
	if(doflux.and.dosingle)call bug('f',
     *	  'option=doflux cannot be used when a single dish is given')
	if(min(rmsfaca,rmsfacb).le.0.0)
     *	  call bug('f','RMSFAC is not positive')
	if(min(rmsfaca,rmsfacb).lt.0.9)
     *	  call bug('w','RMSFAC seems small')
	if(maxniter.lt.0)call bug('f','NITERS was given a bad value')
	if(Tol.le.0.)
     *	  call bug('f','The TOL parameter must be positive valued')
	call keyr('factor',fac,1.0)
	call keyfin
c
c  Get ready.
c
	if(entropy.eq.'gull')then
	  measure = gull
	  positive = .true.
	else if(entropy.eq.'cornwell')then
	  measure = cornwell
	  positive = .false.
	endif
c
c  Open the input maps.
c
	call xyopen(lMapa,MapNam,'old',3,nMap)
	if(max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
	call rdhdi(lMapa,'naxis',naxis,3)
	naxis = min(naxis,MAXNAX)
	call coInit(lMapa)
c
c  Open the single dish image.
c
	if(dosingle)then
	  call xyopen(lMapb,MapSin,'old',3,nMapb)
	  if(hdprsnt(lMapb,'mostable'))call bug('w',
     *	    'Is the second input map really a single dish observation?')
	  if(nMap(1).ne.nMapb(1).or.nMap(2).ne.nMapb(2).or.
     *	     nMap(3).ne.nMapb(3))call bug('f',
     *	    'Input maps differ in size; they must have identical grids')
	  call AlignIni(lMapa,lMapb,nMap(1),nMap(2),nMap(3),
     *	    xmoff,ymoff,zmoff)
	  if(xmoff.ne.0.or.ymoff.ne.0.or.zmoff.ne.0)call bug('f',
     *	    'Input maps are misaligned; they must have identical grids')
	  call rdhdr(lMapb,'rms',Trms,0.0)
	  if(Trms.le.0)call bug('f',
     *	    'The single dish map must have an rms item')
	else
	  TRms = 0.
	endif
	TRms2 = TRms*TRms
c
	call BoxMask(lMapa,boxes,maxboxes)
	if(dosingle)call BoxMask(lMapb,boxes,maxboxes)
	call BoxSet(Boxes,3,nMap,' ')
	call BoxInfo(Boxes,3,blc,trc)
	imin = blc(1)
	imax = trc(1)
	jmin = blc(2)
	jmax = trc(2)
	kmin = blc(3)
	kmax = trc(3)
	nOut(1) = imax - imin + 1
	nOut(2) = jmax - jmin + 1
	nOut(3) = kmax - kmin + 1

	if(nfret.lt.nOut(3)) then
	  TFlux = 0
	  if(nfret.ge.1)TFlux = fluxlist(nfret)
	  do i=nfret+1,nOut(3)
	    fluxlist(i) = TFlux
	  enddo
	else if(nfret.gt.nOut(3)) then
	  call bug('w','More flux estimates than planes...')
	  call bug('w','ignoring extras.')
	endif
c
c  Open the mosaic beam, and get some info about it.
c
	call xyopen(lBeama,BeamNam,'old',3,nBeam)
	n1 = nBeam(1)
	n2 = nBeam(2)
	if(max(n1,n2).gt.maxdim) call bug('f','Beam too big')
	call BeamChar(lBeama,n1,n2,Qest,icentre,jcentre,ffacSD)
	write(line,'(a,f6.1)')'For '//BeamNam(1:len1(BeamNam))//
     *		', an estimate of Q is',Qest
	call output(line)
	if(Qa.gt.0.)then
	  write(line,'(a,1pg8.1)')
     *			'Using user given pixels per beam of',Qa
	  call output(line)
	else
	  Qa = Qest
	endif
	call mcInitF(lBeama)
c
c  Open the single dish beam, if needed.
c
	if(dosingle)then
	  call xyopen(lBeamb,BeamSin,'old',2,nBeam)
	  n1 = nBeam(1)
	  n2 = nBeam(2)
	  if(max(n1,n2).gt.maxdim) call bug('f','Beam too big')
	  call BeamChar(lBeamb,n1,n2,Qest,icentre,jcentre,ffacSD)
	  write(line,'(a,f6.1)')'For '//BeamSin(1:len1(BeamSin))//
     *		', an estimate of Q is',Qest
	  call output(line)
	  if(Qb.gt.0.)then
	    write(line,'(a,1pg8.1)')
     *			'Using user given pixels per beam of',Qb
	    call output(line)
	  else
	    Qb = Qest
	  endif
	  call SDConIni(Cnvl,lBeamb,n1,n2,icentre,jcentre,
     *	    nout(1),nout(2))
	  call xyclose(lBeamb)
	else
	  Qb = 0.
	endif
c
c  Open the model if needed, and check that is is the same size as the
c  output.
c
	if(ModelNam.ne.' ')then
	  call xyopen(lModel,ModelNam,'old',3,nModel)
	  call AlignIni(lModel,lMapa,nMap(1),nMap(2),nMap(3),
     *						xmoff,ymoff,zmoff)
	endif
c
c  Initial values for alpha and beta.
c
	Alpha = 0
	Beta = 0
c
c  Open the default image if needed, and check that is is the same size as the
c  output.
c
	ffacDef = 0
	if(DefNam.ne.' ')then
	  call xyopen(lDef,DefNam,'old',3,nDef)
	  call AlignIni(lDef,lMapa,nMap(1),nMap(2),nMap(3),
     *						xdoff,ydoff,zdoff)
	endif
c
c  Open up the output.
c
	do i=4,naxis
	  nOut(i) = 1
	enddo
	call xyopen(lOut,OutNam,'new',naxis,nOut)
	call Header(lMapa,lOut,blc,trc,version)
	call xyflush(lOut)
c
c  Loop.
c
	maxPoint = 0
	do k=kmin,kmax
	  if(kmin.ne.kmax)call output('Plane: '//itoaf(k))
c
	  call BoxRuns(1,k,' ',boxes,Run,MaxRun,nRun,
     *					xmin,xmax,ymin,ymax)
	  call BoxCount(Run,nRun,nPoint)
	  if(nPoint.gt.0)then
c
c  Allocate arrays to hold everything.
c
	  if(nPoint.gt.maxPoint)then
	    if(maxPoint.gt.0)then
	      call memFree(pEst,maxPoint,'r')
	      call memFree(pDef,maxPoint,'r')
	      call memFree(pNewEst,maxPoint,'r')
	      call memFree(pMapa,maxPoint,'r')
	      call memFree(pWta,maxPoint,'r')
	      call memFree(pResa,maxPoint,'r')
	      call memFree(pNewResa,maxPoint,'r')
	      if(dosingle)then
		call memFree(pMapb,maxPoint,'r')
		call memFree(pWtb,maxPoint,'r')
		call memFree(pResb,maxPoint,'r')
		call memFree(pNewResb,maxPoint,'r')
	      endif
	    endif
	    maxPoint = nPoint
	    call memAlloc(pEst,maxPoint,'r')
	    call memAlloc(pDef,maxPoint,'r')
	    call memAlloc(pNewEst,maxPoint,'r')
	    call memAlloc(pMapa,maxPoint,'r')
	    call memAlloc(pWta,maxPoint,'r')
	    call memAlloc(pResa,maxPoint,'r')
	    call memAlloc(pNewResa,maxPoint,'r')
	    if(dosingle)then
	      call memAlloc(pMapb,maxPoint,'r')
	      call memAlloc(pWtb,maxPoint,'r')
	      call memAlloc(pResb,maxPoint,'r')
	      call memAlloc(pNewResb,maxPoint,'r')
	    else
	      pMapb = pMapa
	      pWtb = pWta
	      pResb = pResa
	      pNewResb = pNewResa
	    endif
	  endif
c
c  Initialise the mosaic routines, get 1/sigma**2, and get the dirty image.
c
	  call mcPlaneR(lMapa,k,Run,nRun,nPoint)
	  call mcSigma2(memr(pWta),nPoint,.false.)
	  call mcGain(memr(pEst),nPoint)
	  call xysetpl(lMapa,1,k)
	  call GetPlane(lMapa,Run,nRun,0,0,nMap(1),nMap(2),
     *				memr(pMapa),maxPoint,nPoint)
	  if(dosingle)then
	    call xysetpl(lMapb,1,k)
	    call GetPlane(lMapb,Run,nRun,0,0,nMap(1),nMap(2),
     *				memr(pMapb),maxPoint,nPoint)
	    call SDConWt(memr(pEst),memr(pWtb),nPoint)
	  endif
c
c  Get the default image.
c
	  if(DefNam.eq.' ')then
	    call Copy(nPoint,memr(pEst),memr(pDef))
	  else
	    call AlignGet(lDef,Run,nRun,k,xdoff,ydoff,zdoff,
     *		nDef(1),nDef(2),nDef(3),memr(pDef),maxPoint,nPoint)
	    call DefGain(memr(pEst),memr(pDef),nPoint)
	  endif
c
c  Get the Default map.
c
	  Tflux = fluxlist(k-kmin+1)
	  if(TFlux.le.0.and.dosingle)then
	    call GetFxSD(memr(pEst),memr(pMapb),nPoint,TFlux)
	    TFlux = fac * TFlux / ffacSD
	  endif
	  if(Tflux.le.0.and.DefNam.ne.' ')then
	    if(ffacDef.le.0)call GetFFDef(lDef,ffacDef)
	    call GetFxDef(memr(pDef),nPoint,TFlux)
	    TFlux = TFlux / ffacDef
	  endif
	  if(Tflux.le.0)then
	    call GetRms(memr(pWta),nPoint,TFlux)
	    TFlux = RmsFaca*TFlux*nPoint/Qa
	  endif
	  call DefFudge(nPoint,memr(pDef),TFlux)
	  write(line,'(a,1pe10.3)')'Using an integrated flux of',TFlux
	  call output(line)
c
c  Get the Estimate and Residual.
c
	  if(ModelNam.eq.' ')then
	    call Copy(nPoint,memr(pDef),memr(pEst))
	  else
	    call AlignGet(lModel,Run,nRun,k,xmoff,ymoff,zmoff,
     *		nModel(1),nModel(2),nModel(3),memr(pEst),
     *		maxPoint,nPoint)
	    if(positive) call ClipIt(memr(pDef),memr(pEst),nPoint)
	  endif
c
	  call Diff(memr(pEst),memr(pMapa),memr(pResa),nPoint,Run,nRun)
	  if(dosingle)call SDConDif(cnvl,memr(pEst),memr(pMapb),fac,
     *	   memr(pResb),memr(pWtb),nPoint,Run,nRun,xmax,ymax)
c
c  Get all the information.
c
	  call GetInfo(nPoint,
     *	      memr(pEst),memr(pResa),memr(pWta),memr(pResb),memr(pWtb),
     *	      TRms2*fac*fac,measure,memr(pDef),dosingle,Alpha,Beta,
     *	      Qa,Qb,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb)
c------------------------------------------------------------------------
c  Now start to iterate at long last.
c
	OStLen1 = 0
	OStLen2 = 0
	Converge = .false.
	Niter = 0
	dowhile(.not.converge.and.Niter.lt.MaxNiter)
	  Niter = Niter + 1
c
c  Update Alpha and Beta.
c
	  De = nPoint*(Rmsa*Rmsa - RmsFaca*RmsFaca)
	  if(dosingle)then
	    Df = nPoint*(Rmsb*Rmsb - RmsFacb*RmsFacb)
	  else
	    Df = Flux - TFlux
	  endif
	  call NewAlpB(Alpha,Beta,De,Df,doflux.or.dosingle,GradEE,
     *	    GradEF,GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
c  Calculate the next step to take.
c
	  call CalStep(nPoint,dosingle,
     *	      memr(pResa),memr(pWta),memr(pResb),memr(pWtb),
     *	      TRms2*fac*fac,memr(pEst),memr(pNewEst),memr(pDef),
     *	      measure,Alpha,Beta,Qa,Qb,J0)
c
c  Determine the max step length, and the initial step length.
c
	  StLim = 1
	  if(GradJJ.gt.0)StLim = min(1.4,0.15*Grad11/GradJJ)
	  StLen1 = min(0.5*(1+OStLen1),StLim)
	  OStLen1 = StLen1
	  J0 = J0 * StLen1
c
c  Take the plunge.
c
	  call TakeStep(nPoint,memr(pEst),memr(pNewEst),
     *					StLen1,positive,StLim)
c
c  Convolve the estimate with the beam and subtract the map.
c
	  call Diff(memr(pNewEst),memr(pMapa),memr(pNewResa),
     *						nPoint,Run,nRun)
	  if(dosingle)call SDConDif(cnvl,memr(pNewEst),memr(pMapb),fac,
     *	    memr(pNewResb),memr(pWtb),nPoint,Run,nRun,xmax,ymax)
c
c  Work out what was really the best step length.
c
	  call ChekStep(nPoint,memr(pEst),memr(pNewEst),memr(pDef),
     *		memr(pNewResa),memr(pWta),memr(pNewResb),memr(pWtb),
     *		TRms2*fac*fac,dosingle,measure,Alpha,Beta,Qa,Qb,J1)
	  if(J0-J1.ne.0)then
	    StLen2 = J0/(J0-J1)
	  else
	    StLen2 = 1
	  endif
	  StLen2 = 0.5*(StLen2 + OStLen2)
	  StLen2 = min(StLen2,StLim/StLen1)
	  OStLen2 = StLen2
c
c  Now interpolate between the actual step and the one we should
c  have taken. Only interpolate if its absolutely necessary. That
c  is if the second step length is not near 1. In practise it will
c  be near 1 on the first few iterations.
c
	  if(abs(StLen2-1.).gt.0.05)then
	    call IntStep(nPoint,memr(pEst),memr(pNewEst),StLen2)
	    call IntStep(nPoint,memr(pResa),memr(pNewResa),StLen2)
	    if(dosingle)call IntStep(nPoint,memr(pResb),
     *					    memr(pNewResb),StLen2)
	  else
	    StLen2 = 1
	    call Swap(pEst,pNewEst)
	    call Swap(pResa,pNewResa)
	    if(dosingle)call Swap(pResb,pNewResb)
	  endif
c
c  Calculate a new estimate for Q using a magic formula.
c
	if(abs(StLen1-1.).lt.0.05)then
	  temp =  sqrt((1./max(0.5,min(2.,StLen1*StLen2))+3.)/4.)
	  Qa = Qa * temp
	  Qb = Qb * temp
	endif
c
c  Get all the information.
c
	  if(dofac.and.abs(StLen1-1.).lt.0.05)
     *	    call NewFac(nPoint,memr(pResb),memr(pMapb),fac,TRms2)
	  call GetInfo(nPoint,
     *	      memr(pEst),memr(pResa),memr(pWta),memr(pResb),memr(pWtb),
     *	      TRms2*fac*fac,measure,memr(pDef),dosingle,Alpha,Beta,
     *	      Qa,Qb,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb)
c
c  Reawaken the user with more crap to let him/her ponder over
c  what could possibly be going wrong. Give him/her as much as
c  possible to ponder over.
c
	  if(verbose)then
	    call output('Iteration '//itoaf(niter))
	    write(line,20)Alpha,Beta,Qa,Qb
	    call output(line)
	    write(line,22)Flux,GradJJ/Grad11,Rmsa,Rmsb
	    call output(line)
	    write(line,21)Immin,Immax,fac
	    call output(line)
	    write(line,23)StLim,StLen1,StLen2
	    call output(line)
	  else
	    if(dosingle)then
	      write(line,25)Niter,Rmsa,Rmsb,fac,Flux,GradJJ/Grad11
	    else
	      write(line,24)Niter,Rmsa,Flux,GradJJ/Grad11
	    endif
	    call output(line)
	  endif
c
  20	  format('  Alpha =',1pe12.3,' Beta   =',1pe12.3,
     *		' Q       =',1p2e12.3)
  21	  format('  Immin =',1pe12.3,' Immax  =',1pe12.3,
     *		' Factor  =',1pe12.3)
  22	  format('  Flux  =',1pe12.3,' NormGrd=',1pe12.3,
     *		' Rms     =',1p2e12.3)
  23	  format('  StLim =',1pe12.3,' StLen1 =',1pe12.3,
     *		' StLen2  =',1pe12.3)
  24	  format(' Iter =',i3,' RmsFac =',1pe10.3,' Flux =',1pe10.3,
     *		' NormGrd =',0pf6.3)
  25	  format(' Iter=',i3,' RmsFac=',f6.2,1x,f6.2,' Factor=',1pe9.3,
     *		' Flux=',1pe9.3,' NormGrd=',0pf5.3)
c
c  Check for convergence.
c
	  converge = (Rmsa-RmsFaca.lt.0.05*Rmsfaca)		.and.
     *		     ((Flux-TFlux).lt.0.05*TFlux.or..not.doflux).and.
     *		      (GradJJ/Grad11.lt.Tol)
	  if(dosingle.and.converge)
     *		converge = Rmsb-RmsFacb.lt.0.05*RmsFacb
	enddo
c------------------------------------------------------------------------
c
c  We have finished processing this plane. More info to the user!
c
	    if(converge)then
	      call output('MOSMEM seems to have converged')
	    else
	      call output('Failed to converge in NITERS iterations')
	    endif
	  endif
c
c  Write out this plane.
c
	  call xysetpl(lOut,1,k-kmin+1)
	  call PutPlane(lOut,Run,nRun,1-imin,1-jmin,
     *				nOut(1),nOut(2),memr(pEst),nPoint)
	  call xyflush(lOut)
	enddo
c
c  Close up the files. Ready to go home.
c
	call coFin(lMapa)
	call xyclose(lMapa)
	call xyclose(lBeama)
	if(dosingle)call xyclose(lMapb)
	if(ModelNam.ne.' ')call xyclose(lModel)
	call xyclose(lOut)
c
c  Release memory.
c
	if(maxPoint.le.0)call bug('f','No data deconvolved')
	call memFree(pEst,maxPoint,'r')
	call memFree(pDef,maxPoint,'r')
	call memFree(pNewEst,maxPoint,'r')
	call memFree(pMapa,maxPoint,'r')
	call memFree(pWta,maxPoint,'r')
	call memFree(pResa,maxPoint,'r')
	call memFree(pNewResa,maxPoint,'r')
	if(dosingle)then
	  call memFree(pMapb,maxPoint,'r')
	  call memFree(pWtb,maxPoint,'r')
	  call memFree(pResb,maxPoint,'r')
	  call memFree(pNewResb,maxPoint,'r')
	endif
c
c  Thats all folks.
c
	end
c************************************************************************
	subroutine GetOpt(verbose,doflux,dofac,entropy)
c
	implicit none
	logical verbose,doflux,dofac
	character entropy*(*)
c
c  Get extra processing options and the entropy measure.
c
c  Output:
c    verbose	Give lots of messages.
c    doflux	Constrain the flux.
c    entropy	The entropy measure.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=3)
	logical present(NOPT)
	character opts(NOPT)*8
c
	integer NMEASURE
	parameter(NMEASURE=2)
	integer nout
	character measure(NMEASURE)*8
c
	data measure/'gull    ','cornwell'/
	data opts/'verbose ','doflux  ','dofactor'/
c
	call options('options',opts,present,NOPT)
	verbose = present(1)
	doflux  = present(2)
	dofac   = present(3)
c
c
	call keymatch('measure',NMEASURE,measure,1,entropy,nout)
	if(nout.eq.0) entropy = measure(1)
	end
c************************************************************************
	subroutine Swap(a,b)
c
	implicit none
	integer a,b
c------------------------------------------------------------------------
	integer t
c
	t = a
	a = b
	b = t
	end
c************************************************************************
	subroutine Copy(n,From,To)
c
	implicit none
	integer n
	real From(n),To(n)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  To(i) = From(i)
	enddo
	end
c************************************************************************
	subroutine GetFFDef(lDef,ffacDef)
c
	implicit none
	integer lDef
	real ffacDef
c
c  Get the factor needed to convert the sum of the pixel values into
c  an integrated flux.
c
c------------------------------------------------------------------------
	character bunit*32
	real bmaj,bmin,cdelt1,cdelt2
c
        call rdhda(lDef,'bunit',bunit,' ')
	call lcase(bunit)
c
	if(bunit.eq.'jy/pixel')then
	  ffacDef = 1
	else if(bunit.eq.'jy/beam')then
          call rdhdr(lDef,'bmaj',bmaj,0.0)
          call rdhdr(lDef,'bmin',bmin,0.0)
          call rdhdr(lDef,'cdelt1',cdelt1,0.0)
          call rdhdr(lDef,'cdelt2',cdelt2,0.0)
          if (abs(cdelt1*cdelt2*bmaj*bmin).le.0.0)call bug('f',
     *	    'Could not determine the beam parameters of default image')
	  ffacDef = 1.1331 * abs( bmaj * bmin / (cdelt1*cdelt2) )
	else
	  call bug('w','Unrecognised flux units for the default image')
	  call bug('w','Pretending default image units are JY/PIXEL')
	  ffacDef = 1
	endif
c
	end
c************************************************************************
	subroutine DefGain(Gain,Def,nPoint)
c
	implicit none
	integer nPoint
	real Gain(nPoint),Def(nPoint)
c
c  Apply the gains to the default image.
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Def(i) = Gain(i) * Def(i)
	enddo
c
	end
c************************************************************************
	subroutine DefFudge(npoint,Def,TFlux)
c
	implicit none
	integer npoint
	real Def(npoint),TFlux
c
c  This clips the default so that its well defined, and scales it so that
c  it has the correct integrated flux.
c
c------------------------------------------------------------------------
	real alpha,temp
	double precision sum
	integer i
c
	sum = 0
	temp = 1e-4 * TFlux/nPoint
	do i=1,npoint
	  sum = sum + max(Def(i),temp)
	enddo
c
	if(Sum.le.0)call bug('f','Cannot scale default image')
c
	alpha = TFlux/Sum
	do i=1,npoint
	  Def(i) = alpha*max(Def(i),temp)
	enddo
c
	end
c***********************************************************************
	subroutine ClipIt(Def,Est,nPoint)
c
	implicit none
	integer nPoint
	real Def(nPoint),Est(nPoint)
c
c  Set up the minimum of the default image.
c
c  Input:
c    clip
c    nPoint
c    Def	The default image.
c  Input/Output:
c    Est	The estimate image, whixh is being clipped.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Est(i) = max(Est(i),0.1*Def(i))
	enddo
	end
c************************************************************************
	subroutine BeamChar(lBeam,n1,n2,Qest,icentre,jcentre,fluxfac)
c
	implicit none
	integer lBeam,n1,n2,icentre,jcentre
	real Qest,fluxfac
c
c  Determine the location of the centre of the beam, and get an estimate
c  of the number of points per beam.
c
c  Inputs:
c    lBeam	Handle of the beam file.
c    n1,n2	Size of the beam.
c
c  Outputs:
c    icentre,jcentre Coordinates of the centre of the beam. This is
c		assumed to be near the image centre.
c    Qest	An estimate of the number of points per beam.
c    fluxfac    Integration of the beam.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real tol
	parameter(tol=0.1)
	integer i,j
	real bmax,Data(maxdim)
	double precision Sum,Sum2
c
	Sum = 0
	Sum2 = 0
	bmax = 0
	icentre = 0
	jcentre = 0
	do j=1,n2
	  call xyread(lBeam,j,Data)
	  do i=1,n1
	    Sum = Sum + Data(i)
	    if(data(i).gt.tol)Sum2 = Sum2 + Data(i)*Data(i)
	    if(Data(i).gt.bmax)then
	      icentre = i
	      jcentre = j
	      bmax = Data(i)
	    endif
	  enddo
	enddo
c
	Qest = sqrt(2.7*Sum2)
	fluxfac = Sum
	if(abs(1-bmax).gt.0.01) call bug('f','Beam peak is not 1')
	end
c************************************************************************
	subroutine GetFxSD(Gain,Model,nPoint,flux)
c
	implicit none
	integer nPoint
	real Gain(nPoint),Model(nPoint),flux
c
c  Determine the flux of an image.
c------------------------------------------------------------------------
	integer i
c
	flux = 0
	do i=1,nPoint
	  if(Gain(i)*Model(i).gt.0)flux = flux + Gain(i)*Model(i)
	enddo
c
	end
c************************************************************************
	subroutine GetFxDef(Model,nPoint,flux)
c
	implicit none
	integer nPoint
	real Model(nPoint),flux
c
c  Determine the flux of an image.
c------------------------------------------------------------------------
	integer i
c
	flux = 0
	do i=1,nPoint
	  flux = flux + Model(i)
	enddo
c
	end
c************************************************************************
	subroutine GetRms(Wt,nPoint,Rms)
c
	implicit none
	integer nPoint
	real Wt(nPoint),Rms
c
c  Determine the RMS value.
c------------------------------------------------------------------------
	integer i,Count
c
	Rms = 0
	Count = 0
c
	do i=1,nPoint
	  if(Wt(i).gt.0)then
	    Rms = Rms + 1/Wt(i)
	    Count = Count + 1
	  endif
	enddo
c
	Rms = sqrt(Rms/Count)
c
	end
c************************************************************************
	subroutine IntStep(nPoint,Old,New,FracNew)
c
	implicit none
	integer nPoint
	real FracNew
	real Old(nPoint),New(nPoint)
c
c  Update the current image by interpolating between two previous ones.
c------------------------------------------------------------------------
	real FracOld
	integer i
c
	FracOld = 1. - FracNew
	do i=1,nPoint
	  Old(i) = FracOld*Old(i) + FracNew*New(i)
	enddo
c
	end
c************************************************************************
	subroutine CalStep(nPoint,dosingle,Resa,Wta,Resb,Wtb,TRms2,
     *	  Est,Step,Def,measure,Alpha,Beta,Qa,Qb,J0)
c
	implicit none
	integer nPoint,measure
	real Alpha,Beta,Qa,Qb,J0,TRms2
	real Def(nPoint),Est(nPoint),Step(nPoint)
	real Resa(nPoint),Resb(nPoint),Wta(nPoint),Wtb(nPoint)
	logical dosingle
c
c  Calculate the step to take next.
c
c  Inputs:
c    nPoint	Number of points.
c    Est	Current estimate of the MEM solution.
c    Def	The default image.
c    Resa	Current residuals of the mosaic.
c    Wta	1/Sigma**2 for the mosaic image.
c    Resb	Current residuals for the single dish.
c    Wtb	1/Gain of the mosaic.
c    TRms2	sigma2 for the single dish.
c    dosingle	True if joint deconvolution with single dish data.
c    measure	Determines the entropy measure used.
c
c  Output:
c    Step	The step to take towards a better estimate.
c    Length	A measure of the length of the step.
c
c------------------------------------------------------------------------
	integer run
	parameter(run=1024)
	integer n,l,ltot
	real Diag, GradJ, Stepd
	real dH(run),d2H(run)
c
	J0 = 0
	n = 0
	dowhile(n.lt.nPoint)
	  ltot = min(nPoint-n,run)
	  call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
	  do l=1,ltot
	    if(dosingle)then
	      GradJ = dH(l) - 2*Qa*Alpha*Resa(n+l)*Wta(n+l)
     *			    - 2*Qb*Beta *Resb(n+l)*Wtb(n+l)/TRms2
	      Diag = 1./( 2.*Alpha*Qa*Qa*Wta(n+l) + 
     *			  2.*Beta* Qb*Qb*Wtb(n+l)*Wtb(n+l)/TRms2-d2H(l))
	    else
	      GradJ = dH(l) - 2.*Qa*Alpha*Resa(n+l)*Wta(n+l) - Beta
	      Diag = 1 / (2*Alpha*Qa*Qa*Wta(n+l) - d2H(l))
	    endif
	    Stepd = Diag*GradJ
	    J0 = J0 + GradJ*Stepd
	    Step(n+l) = Stepd
	  enddo
	  n = n + ltot
	enddo
c
	end
c************************************************************************
	subroutine TakeStep(nPoint,Est,NewEst,StLen,doClip,StLim)
c
	implicit none
	integer nPoint
	real Est(nPoint),NewEst(nPoint)
	real StLen,StLim
	logical doClip
c
c  Take the final step!
c
c------------------------------------------------------------------------
	integer i
	real Stepd
c
	if(doClip)then
	  do i=1,nPoint
	    Stepd = StLen*max(NewEst(i),-0.9*Est(i)/StLim)
	    NewEst(i) = max(Est(i) + Stepd,1e-30)
	  enddo
	else
	  do i=1,nPoint
	    NewEst(i) = Est(i) + StLen*NewEst(i)
	  enddo
	endif
	end
c************************************************************************
	subroutine ChekStep(nPoint,OldEst,Est,Def,Resa,Wta,Resb,Wtb,
     *		TRms2,dosingle,measure,Alpha,Beta,Qa,Qb,J0)
c
	implicit none
	integer nPoint,Measure
	real OldEst(nPoint),Est(nPoint),Def(nPoint)
	real Resa(nPoint),Wta(nPoint),Resb(nPoint),Wtb(nPoint)
	real Alpha,Beta,Qa,Qb,J0,TRms2
	logical dosingle
c
c  Determine some things about this place we are thinking of moving
c  to. Is it a good neighbourhood? Will my kids be safe here?
c
c  Inputs:
c    nPoint	Size of the region being deconvolved.
c    Alpha,Beta	Lagrangian multipliers.
c    Def	Default image.
c    Wt		
c    Q		Pixels/beam.
c    Res	The residual.
c    Est	The estimate.
c    OldEst	The old estimate.
c    measure	Determines the entropy measure.
c
c  Output:
c    J0		Some useful (??) statistic.
c
c------------------------------------------------------------------------
	integer run
	parameter(run=1024)
	integer n,l,ltot
	real GradJ,Step
	real dH(run),d2H(run)
c
	J0 = 0.
	n = 0
	do while(n.lt.nPoint)
	  ltot = min(nPoint-n,run)
	  call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
	  do l=1,ltot
	    if(dosingle)then
	      GradJ = dH(l) - 2*Qa*Alpha*Resa(n+l)*Wta(n+l)
     *			    - 2*Qb*Beta *Resb(n+l)*Wtb(n+l)/TRms2
	    else
	      GradJ = dH(l) - 2.*Alpha*Qa*Resa(n+l)*Wta(n+l) - Beta
	    endif
	    Step = Est(n+l) - OldEst(n+l)
	    J0 = J0 + GradJ*Step
	  enddo
	  n = n + ltot
	enddo
c
	end
c************************************************************************
	subroutine GetInfo(nPoint,Est,Resa,Wta,Resb,Wtb,TRms2,Measure,
     *	  Def,dosingle,Alpha,Beta,Qa,Qb,
     *	  GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ,
     *    GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb)
c
	implicit none
	integer nPoint
	real Resa(nPoint),Wta(nPoint),Resb(nPoint),Wtb(nPoint)
	real Est(nPoint),Def(nPoint)
	integer Measure
	real Alpha,Beta,Qa,Qb,TRms2
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb
	logical dosingle
c
c  Get information on the current state of play.
c
c  Inputs:
c    nPoint	Number of points in the input.
c    Res,Est	The Residuals and Estimate respectively.
c    measure	Determines the entropy measure used.
c    Def	The default image.
c    Wt		1/Sigma**2 for the mosaic.
c    Alpha
c    Beta
c    Q
c
c  Outputs:
c    GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
c    GradHH,GradJJ,NomGrd,Immax,Immin,Flux,Rms
c------------------------------------------------------------------------
	integer Run
	parameter(Run=1024)
	integer n,l,ltot
	real Diag,GradE,GradH,GradF,temp
	real dH(Run),d2H(Run)
c
	GradEE = 0.
	GradEF = 0.
	GradEH = 0.
	GradFF = 0.
	GradFH = 0.
	GradHH = 0.
	Rmsa   = 0.
	Rmsb   = 0.
	Flux   = 0.
	Immin = Est(1)
	Immax = Immin
	temp = 0
c
	n = 0
	do while(n.lt.nPoint)
	  ltot = min(Run,nPoint-n)
	  call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
	  do l=1,ltot
	    GradE = 2. * Qa * Resa(n+l) * Wta(n+l)
	    Rmsa = Rmsa + Wta(n+l) * Resa(n+l)**2
	    if(dosingle)then
	      Rmsb = Rmsb + Resb(n+l)**2/TRms2
	      GradF = 2. * Qb * Resb(n+l) * Wtb(n+l)/TRms2
	      Diag = 1./( 2.*Alpha*Qa*Qa*Wta(n+l) + 
     *			  2.*Beta* Qb*Qb*Wtb(n+l)*Wtb(n+l)/TRms2-d2H(l))
	    else
	      GradF = 1
	      Diag = 1./( 2.*Alpha*Qa*Qa*Wta(n+l) - d2H(l) )
	    endif
	    GradH = dH(l)
	    GradEE = GradEE + GradE*Diag*GradE
	    GradEF = GradEF + GradE*Diag*GradF
	    GradEH = GradEH + GradE*Diag*GradH
	    GradFF = GradFF + GradF*Diag*GradF
	    GradFH = GradFH + GradF*Diag*GradH
	    GradHH = GradHH + GradH*Diag*GradH
	    temp = temp + Diag
	    Flux = Flux + Est(n+l)
	    Immin = min(Immin,Est(n+l))
	    Immax = max(Immax,Est(n+l))
	  enddo
	  n = n + ltot
	enddo
c
c  Finish up various variables.
c
	Rmsa = sqrt(Rmsa/real(nPoint))
	Rmsb = sqrt(Rmsb/real(nPoint))
	GradEJ = GradEH - Alpha*GradEE - Beta*GradEF
	GradFJ = GradFH - Alpha*GradEF - Beta*GradFF
	GradJJ = GradHH + Alpha*Alpha*GradEE + Beta*Beta*GradFF
     *		- 2.*Alpha*GradEH - 2.*Beta*GradFH
     *		+ 2.*Alpha*Beta*GradEF
	Grad11 = GradHH + alpha**2*GradEE + beta**2*GradFF
	if(Grad11.le.0)Grad11 = temp
c
	end	
c************************************************************************
	subroutine EntFunc(measure,n,Est,Default,dH,d2H)
c
	implicit none
	integer n,measure
	real Default(n),Est(n),dH(n),d2H(n)
c
c  Routine to find the first and second derivatives of the desired
c  entropy measure. These are:
c    Gull, Daniel and Skilling entropy function:
c      H   = - SUM b*log(b/em)
c      dH  = -log(b/m)
c      d2H = -1/b
c
c    Cornwell's "UTESS" measure:
c      H   = - SUM log(cosh(b/m))
c      dH  = -tanh(b/m)/m
c      d2H = -(sech(b/m)/m)**2
c          = dH**2 - 1/m**2
c
c  Inputs:
c    measure	The entropy measure desired, either gull or cornwell.
c    n		Number of elements to find derivative info for.
c    Est	Brightness estimate, b.
c    Default	Default image.
c
c  Outputs:
c    dH		First derivative of the entropy function.
c    d2H	Second derivative.
c
c------------------------------------------------------------------------
	integer gull,cornwell
	parameter(gull=1,cornwell=2)
	integer i
	real def
c
c  The Gull, Daniel and Skilling measure.
c
	if(measure.eq.gull)then
	  do i=1,n
	    dH(i) = -log(Est(i)/Default(i))
	    d2H(i) = -1.0/Est(i)
	  enddo
c
c  Cornwells UTESS measure.
c
	else
	  do i=1,n
	    def = 1/Default(i)
	    dH(i) = -def * tanh(Est(i)*def)
	    d2H(i) = dH(i)*dH(i) - def*def
	  enddo
	endif
c
	end
c************************************************************************
	subroutine Diff(Est,Map,Res,nPoint,Run,nRun)
c
	implicit none
	integer nPoint,nRun,Run(3,nRun)
	real Est(nPoint),Map(nPoint),Res(nPoint)
c
c------------------------------------------------------------------------
	integer i
c
	call mcCnvlR(Est,Run,nRun,Res)
c
	do i=1,nPoint
	  Res(i) = Res(i) - Map(i)
	enddo
c
	end
c************************************************************************
	subroutine Header(lMap,lOut,blc,trc,version)
c
	integer lMap,lOut
	integer blc(3),trc(3)
	character version*(*)
c
c  Write a header for the output file.
c
c  Input:
c    version	Program version ID.
c    lMap	The handle of the input map.
c    lOut	The handle of the output estimate.
c    blc	Blc of the bounding region.
c    trc	Trc of the bounding region.
c
c------------------------------------------------------------------------
	include 'maxnax.h'
	integer i,lblc,ltrc
	real crpix
	character line*72,txtblc*32,txttrc*32,num*2
	integer nkeys
	parameter(nkeys=17)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'obstime ','epoch   ','history ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','object  ',
     *	  'observer','telescop','restfreq','vobs    ','btype   ',
     *	  'mostable','cellscal','pbtype  ','pbfwhm  '/
c
c  Fill in some parameters that will have changed between the input
c  and output.
c
	call wrhda(lOut,'bunit','JY/PIXEL')
c
	do i=1,MAXNAX
	  num = itoaf(i)
	  if(i.le.3)then
	    call rdhdr(lMap,'crpix'//num,crpix,1.)
	    crpix = crpix - blc(i) + 1
	    call wrhdr(lOut,'crpix'//num,crpix)
	  else
	    call hdcopy(lMap,lOut,'crpix'//num)
	  endif
	  call hdcopy(lMap,lOut,'cdelt'//num)
	  call hdcopy(lMap,lOut,'crval'//num)
	  call hdcopy(lMap,lOut,'ctype'//num)
	enddo
c
c  Copy all the other keywords across, which have not changed and add history
c
	do i=1,nkeys
	  call hdcopy(lMap, lOut, keyw(i))
	enddo
c
c  Write crap to the history file, to attempt (ha!) to appease Neil.
c  Neil is not easily appeased you know.  Just a little t.l.c. is all he needs.
c  
c
	call hisopen(lOut,'append')
        line = 'MOSMEM: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'MOSMEM')
c
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	line = 'MOSMEM: Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'
	call hiswrite(lOut,line)
c
	call hisclose(lOut)
c
	end
c************************************************************************
	subroutine NewAlpB(Alpha,Beta,De,Df,dotwo,GradEE,GradEF,
     *		GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
	implicit none
	real Alpha,Beta,De,Df,GradEE,GradEF
	real GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH
	logical dotwo
c
c  Determine new values for alpha and beta.
c------------------------------------------------------------------------
	real tol1,tol2
	parameter(tol1=0.1,tol2=0.05)
c
	real Denom,Dalp,Dbet,l,Alpha1,Alpha2,Beta1,Beta2,b2m4ac
c
c  Check if things are doing poorly. If so, just aim at reducing the
c  gradient.
c
	l = abs(GradJJ/Grad11)
	if(Alpha.le.0)l = 0
c
	if(dotwo)then
	  Denom = 1./(GradEE*GradFF - GradEF*GradEF)
	  Alpha1 = (GradFF*GradEH - GradEF*GradFH) * Denom
	  Beta1  = (GradEE*GradFH - GradEF*GradEH) * Denom
	else
	  Alpha1 = GradEH / GradEE
	  Beta1  = 0
	endif
c
	if(dotwo)then
	  Denom = 1./(GradEE*GradFF - GradEF*GradEF)
	  Dalp = ( GradFF*(De+GradEJ) - GradEF*(Df+GradFJ) ) * Denom
	  Dbet =-( GradEF*(De+GradEJ) - GradEE*(Df+GradFJ) ) * Denom
	else
	  Denom = 1./GradEE
	  Dalp = (De+GradEJ) * Denom
	  Dbet = 0.
	endif
c
	b2m4ac = GradEJ*GradEJ - (GradJJ-tol1*Grad11)*GradEE
        if(b2m4ac.gt.0)then
          b2m4ac = sqrt(b2m4ac)
	  Dalp = max((GradEJ - b2m4ac)/GradEE,
     *		 min((GradEJ + b2m4ac)/GradEE,Dalp))
	else
	  Dalp = 0
        endif
c
        b2m4ac = GradFJ*GradFJ - (GradJJ-tol1*Grad11)*GradFF
        if(b2m4ac.gt.0)then
          b2m4ac = sqrt(b2m4ac)
	  Dbet = max((GradFJ - b2m4ac)/GradFF,
     *		 min((GradFJ + b2m4ac)/GradFF,Dbet))
	else
	  Dbet = 0
        endif
c
	Alpha2 = Alpha+ Dalp
	Beta2  = Beta + Dbet
c
	if(l.ge.tol2.or.Alpha2.le.0)then
	  Alpha = max(Alpha1,0.)
	else
	  Alpha = max(Alpha2,0.)
	endif
c
	if(l.ge.tol2.or.Beta2.le.0)then
	  Beta = max(Beta1,0.)
	else
	  Beta = max(Beta2,0.)
	endif
c
	end
c************************************************************************
	subroutine SDConIni(Cnvl,lBeam,n1,n2,ic,jc,nx,ny)
c
	implicit none
	integer Cnvl,lBeam,n1,n2,ic,jc,nx,ny
c
c  Initialise the routines that convolve with the dirty beam.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer na,nb,pData
c
c  Determine the size of the beam that we need to feed to the convolution
c  routines.
c
	na = max(n1,nx+min(n1,nx)-1)
	nb = max(n2,ny+min(n2,ny)-1)
c
	if(na.eq.n1.and.nb.eq.n2)then
	  call CnvlIniF(Cnvl,lBeam,n1,n2,ic,jc,0.0,' ')
	else if(na.le.2*n1.and.nb.le.2*n2)then
	  call CnvlIniF(Cnvl,lBeam,n1,n2,ic,jc,0.0,'e')
	else
	  call memAlloc(pData,na*nb,'r')
	  call SDConLod(lBeam,n1,n2,memr(pData),na,nb)
	  call CnvlIniA(Cnvl,memr(pData),na,nb,ic,jc,0.0,' ')
	  call memFree(pData,na*nb,'r')
	endif
c
	end
c************************************************************************
	subroutine SDConLod(lBeam,n1,n2,Data,nx,ny)
c
	implicit none
	integer lBeam,n1,n2,nx,ny
	real Data(nx,ny)
c
c  Load a single dish beam, zero padding where necessary.
c------------------------------------------------------------------------
	integer i,j
c
	do j=1,n2
	  call xyread(lBeam,j,Data(1,j))
	  do i=n1+1,nx
	    Data(i,j) = 0
	  enddo
	enddo
c
	do j=n2+1,ny
	  do i=1,nx
	    Data(i,j) = 0
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine SDConDif(cnvl,Est,Map,fac,Res,Wt,nPoint,Run,nRun,
     *						nx,ny)
c
	implicit none
	integer cnvl,nPoint,nRun,Run(3,nRun),nx,ny
	real Est(nPoint),Map(nPoint),Res(nPoint),Wt(nPoint),fac
c
c  Determine the convolution of the estimate with the single dish beam.
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Res(i) = Est(i) * Wt(i)
	enddo
c
	call CnvlR(cnvl,res,nx,ny,Run,nRun,res,'c')
c
	do i=1,nPoint
	  Res(i) = Res(i) - fac*Map(i)
	enddo
c
	end
c************************************************************************
	subroutine SDConWt(Gain,Wt,nPoint)
c
	implicit none
	integer nPoint
	real Gain(nPoint),Wt(nPoint)
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  if(Gain(i).gt.0)then
	    Wt(i) = 1/Gain(i)
	  else
	    Wt(i) = 0
	  endif
	enddo
c
	end
c************************************************************************
	subroutine NewFac(nPoint,Res,Map,fac,TRms2)
c
	implicit none
	integer nPoint
	real TRms2,fac,Res(nPoint),Map(nPoint)
c
c------------------------------------------------------------------------
	real dirty,nFac
	double precision SumMM,SumDM
	integer i
c
	SumMM = 0
	SumDM = 0
	do i=1,nPoint
	  dirty = Res(i) + fac*Map(i)
	  SumMM = SumMM + Map(i)*Map(i)
	  SumDM = SumDM + dirty*Map(i)
	enddo
c
	nfac = SumDM/(SumMM - nPoint*TRms2)
c
	do i=1,nPoint
	  Res(i) = Res(i) + (fac - nfac)*Map(i)
	enddo
c
	fac = nfac
c
	end
