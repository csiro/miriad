c************************************************************************
	program pmosmem
	implicit none
c
c= pmosmem - Maximum Entropy deconvolution for polarization mosaics
c& rjs
c: deconvolution
c+
c	PMOSMEM is a MIRIAD task which performs a joint maximum entropy
c	deconvolution of polarized mosaiced images. Optionally it can 
c	perform a joint deconvolution with a single dish image as well.
c@ map
c	Two to five input images. The first must be a Stokes-I mosaic,
c	then one or more polarized mosaics (Stokes Q, U and V images), and then
c	optionally a c	Stokes-I single dish image. These should have
c	units of Jy/beam. The mosaics should be produced by INVERTs mosaic mode.
c	All the images must be on exactly the same pixel grid. If necessary,
c	use REGRID to make this so.
c@ beam
c	One or perhaps two input dirty beams. The first, corresponding to the
c	mosaics, will be produced by INVERTs mosaic mode. There
c	is no default. The second dirty beam (which must be given if there
c	are two dirty map inputs) gives the point-spread function of the
c	single dish dirty map. This second dirty beam need not be the same
c	image size as the input dirty maps, and may be appreciably smaller.
c	This single-dish beam is assumed to be position-independent, but it
c	need not be symmetric.
c@ default
c	The default image. This is the Stokes-I image that the final solution
c	will tend towards. The final result will be influenced by this default
c	if the constrains that the data put on the solution are weak.
c	The default is a flat estimate, with the correct flux.
c@ out
c	The name of the output map. The units of the output will be Jy/pixel.
c	It can be input to RESTOR to produce a restored image, or alternatively
c	to PMOSMEM, as a model, to continue the deconvolution process.
c@ niters
c	The maximum number of iterations. The default is 30.
c@ region
c	This specifies the region to be deconvolved. See the Users Manual
c	for instructions on how to specify this. The default is the entire
c	image.
c@ tol
c	Tolerance of solution. There is no need to change this from the
c	default of 0.01.
c@ q
c	One or two values (corresponding to the mosaic and single dish
c	observations). These give estimates of the number of points per beam.
c	PMOSMEM can usually come up with a good, image-dependent estimate.
c@ rmsfac
c	PMOSMEM must be able to the theoretical rms noise of the input dirty
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
c	before using PMOSMEM. This is easily done: for image xxxx, use
c	  puthd in=xxxx/rms value=????
c	where "????" is the rms noise in Jy/beam.
c@ factor
c	The flux calibration factor. This is only relevant when doing a
c	joint deconvolution of a mosaic and a single-dish image. It gives the
c	factor which the single-dish data should be multiplied by to convert
c	it to the same flux units as the mosaic. The default is 1. 
c@ flux
c	An estimate of the integrated flux of the source. This parameter cannot
c	be used if there is an input single dish image. 
c	Giving PMOSMEM a good value for the integrated flux
c	will help it find a good solution. On the other hand, giving
c	a poor value may do harm. Normally PMOSMEM will NOT constrain the
c	integrated flux to be this value, but see the ``doflux'' option below.
c	The default is image-dependent.
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
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='PMosMem: version 1.0 17-Mar-98')
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	integer MaxRun,MaxBoxes,MaxPol
	parameter(MaxRun=3*maxdim,MaxBoxes=2048,MaxPol=4)
c
	character MapNam(MaxPol+1)*64,BeamNam(2)*64,OutNam(MaxPol)*64
	character DefNam*64,line*80
	integer lBeam(2),lMap(MaxPol+1),lOut(MaxPol),lDef
	integer nMap(3),nMapb(3),nOut(MAXNAX),nBeam(3),nDef(3)
	integer npol,nplan,iax,nbm,nt
	integer xmin,ymin,xmax,ymax,n1,n2,i
	integer imin,imax,jmin,jmax,kmin,kmax,blc(3),trc(3),naxis,k
	integer icentre,jcentre
	integer maxniter,niter
	real Tol,rmsfaca,rmsfacb,rmsfacc,TFlux,Qest,Qa,Qb,TRms,Trms2
	real ffacSD,ffacDef,Alpha,Beta,Gamma,De,Df,Dg,temp
	real StLim,StLen1,StLen2,OStLen1,OStLen2,J0,J1
	real GradEE,GradEF,GradEG,GradEH,GradEJ
	real        GradFF,GradFG,GradFH,GradFJ
	real	           GradGG,GradGH,GradGJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb,Rmsc
	real fluxlist(maxdim),fac
	double precision pol
	integer nfret
	logical converge,verbose,doflux
	integer Run(3,MaxRun),nRun,Boxes(maxBoxes),nPoint,maxPoint
	integer xmoff,ymoff,zmoff,xdoff,ydoff,zdoff
c
	integer pMap(MaxPol+1),pEst(MaxPol),pDef,pRes(MaxPol+1)
	integer pNewEst(MaxPol),pNewRes(MaxPol+1),pWta,pWtb,Cnvl
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
	call mkeyf('map',MapNam,5,nplan)
	call mkeyf('beam',BeamNam,2,nbm)
	if(nbm.lt.1)call bug('f','Beam datasets must be given')
	npol = nplan - nbm + 1
	if(npol.lt.2)call bug('f','Incorrect number of maps or beams')
	call mkeya('out',OutNam,npol,nt)
	if(nt.ne.npol)
     *	  call bug('f','Incorrect number of output names given')
	call keya('default',DefNam,' ')
	call keyr('tol',Tol,0.01)
	call keyi('niters',maxniter,30)
	call keyr('q',Qa,0.)
	qb = 0
	if(npol.ne.nplan)call keyr('q',Qb,0.)
	call keyr('rmsfac',rmsfaca,1.)
	call keyr('rmsfac',rmsfacb,rmsfaca)
	rmsfacc = rmsfacb
	if(npol.ne.nplan)call keyr('rmsfac',rmsfacc,rmsfacb)
c
	nfret = 0
	call mkeyr('flux',fluxlist,maxdim,nfret)
	if(npol.ne.nplan.and.nfret.gt.0)call bug('w',
     *	    'Using "flux" parameter with single dish input is unusual')
	call BoxInput('region',MapNam,Boxes,MaxBoxes)
	call GetOpt(verbose,doflux)
	if(doflux.and.npol.ne.nplan)call bug('f',
     *	  'option=doflux cannot be used when a single dish is given')
	if(min(rmsfaca,rmsfacb,rmsfacc).le.0.0)
     *	  call bug('f','RMSFAC is not positive')
	if(min(rmsfaca,rmsfacb,rmsfacc).lt.0.9)
     *	  call bug('w','RMSFAC seems small')
	if(maxniter.lt.0)call bug('f','NITERS was given a bad value')
	if(Tol.le.0.)
     *	  call bug('f','The TOL parameter must be positive valued')
	call keyr('factor',fac,1.0)
	call keyfin
c
c  Open the input images.
c
	call xyopen(lMap(1),MapNam(1),'old',3,nMap)
	if(max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
	call rdhdi(lMap(1),'naxis',naxis,3)
	naxis = min(naxis,MAXNAX)
	call coInit(lMap(1))
	call coFindAx(lMap(1),'stokes',iax)
	pol = 0
	if(iax.ne.0)call coCvt1(lMap(1),iax,'ap',1.d0,'aw',pol)
	if(abs(pol-1.d0).gt.1e-3)
     *	  call bug('f','First input map must be Stokes-I mosaic')
c
c  Get the polarised input images.
c
	do i=2,npol
	  call xyopen(lMap(i),MapNam(i),'old',3,nMapb)
	  call coInit(lMap(i))
	  if(nMap(1).ne.nMapb(1).or.nMap(2).ne.nMapb(2).or.
     *	     nMap(3).ne.nMapb(3))call bug('f',
     *	    'Input maps differ in size; they must have identical grids')
	  call AlignIni(lMap(1),lMap(i),nMap(1),nMap(2),nMap(3),
     *	    xmoff,ymoff,zmoff)
	  if(xmoff.ne.0.or.ymoff.ne.0.or.zmoff.ne.0)call bug('f',
     *	    'Input maps are misaligned; they must have identical grids')
	  pol = 0
	  if(iax.ne.0)call coCvt1(lMap(i),iax,'ap',1.d0,'aw',pol)
	  if(pol.lt.1.999.or.pol.gt.4.001)
     *	    call bug('f','Not a polarized mosaic: '//MapNam(i))
	enddo
c  
c
c  Open the single dish image.
c
	if(npol.ne.nplan)then
	  call xyopen(lMap(nplan),MapNam(nplan),'old',3,nMapb)
	  if(hdprsnt(lMap(nplan),'mostable'))call bug('w','Is the'//
     *	    ' last input dirty map really a single dish observation?')
	  if(nMap(1).ne.nMapb(1).or.nMap(2).ne.nMapb(2).or.
     *	     nMap(3).ne.nMapb(3))call bug('f',
     *	    'Input maps differ in size; they must have identical grids')
	  call AlignIni(lMap(1),lMap(nplan),nMap(1),nMap(2),nMap(3),
     *	    xmoff,ymoff,zmoff)
	  if(xmoff.ne.0.or.ymoff.ne.0.or.zmoff.ne.0)call bug('f',
     *	    'Input maps are misaligned; they must have identical grids')
	  call rdhdr(lMap(nplan),'rms',Trms,0.0)
	  if(Trms.le.0)call bug('f',
     *	    'The single dish map must have an rms item')
	else
	  TRms = 0.
	endif
	TRms2 = TRms*TRms
c
	do i=1,nplan
	  call BoxMask(lMap(i),boxes,maxboxes)
	enddo
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
	call xyopen(lBeam(1),BeamNam(1),'old',3,nBeam)
	n1 = nBeam(1)
	n2 = nBeam(2)
	if(max(n1,n2).gt.maxdim) call bug('f','Beam too big')
	call BeamChar(lBeam(1),n1,n2,Qest,icentre,jcentre,ffacSD)
	write(line,'(a,f6.1)')'For '//BeamNam(1)(1:len1(BeamNam(1)))//
     *		', an estimate of Q is',Qest
	call output(line)
	if(Qa.gt.0.)then
	  write(line,'(a,1pg8.1)')
     *			'Using user given pixels per beam of',Qa
	  call output(line)
	else
	  Qa = Qest
	endif
	call mcInitF(lBeam(1))
c
c  Open the single dish beam, if needed.
c
	if(npol.ne.nplan)then
	  call xyopen(lBeam(2),BeamNam(2),'old',2,nBeam)
	  n1 = nBeam(1)
	  n2 = nBeam(2)
	  if(max(n1,n2).gt.maxdim) call bug('f','Beam too big')
	  call BeamChar(lBeam(2),n1,n2,Qest,icentre,jcentre,ffacSD)
	  write(line,'(a,f6.1)')'For '//BeamNam(2)(1:len1(BeamNam(2)))//
     *		', an estimate of Q is',Qest
	  call output(line)
	  if(Qb.gt.0.)then
	    write(line,'(a,1pg8.1)')
     *			'Using user given pixels per beam of',Qb
	    call output(line)
	  else
	    Qb = Qest
	  endif
	  call SDConIni(Cnvl,lBeam(2),n1,n2,icentre,jcentre,
     *	    nout(1),nout(2))
	  call xyclose(lBeam(2))
	else
	  Qb = 0.
	endif
c
c  Initial values for alpha and beta.
c
	Alpha = 0
	Beta = 0
	Gamma = 0
c
c  Open the default image if needed, and check that is is the same size as the
c  output.
c
	ffacDef = 0
	if(DefNam.ne.' ')then
	  call xyopen(lDef,DefNam,'old',3,nDef)
	  call AlignIni(lDef,lMap(1),nMap(1),nMap(2),nMap(3),
     *						xdoff,ydoff,zdoff)
	endif
c
c  Open up the output.
c
	do i=4,naxis
	  nOut(i) = 1
	enddo
	do i=1,npol
	  call xyopen(lOut(i),OutNam(i),'new',naxis,nOut)
	  call Header(lMap(i),lOut(i),blc,trc,version)
	  call xyflush(lOut(i))
	enddo
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
	      call memFree(pEst,maxPoint*npol,'r')
	      call memFree(pNewEst,maxPoint*npol,'r')
	      call memFree(pMap,maxPoint*nplan,'r')
	      call memFree(pRes,maxPoint*nplan,'r')
	      call memFree(pNewRes,maxPoint*nplan,'r')
	      call memFree(pDef,maxPoint,'r')
	      call memFree(pWta,maxPoint,'r')
	      if(npol.ne.nplan)call memFree(pWtb,maxPoint,'r')
	    endif
	    maxPoint = nPoint
	    call memAlloc(pEst,maxPoint*npol,'r')
	    call memAlloc(pNewEst,maxPoint*npol,'r')
	    call memAlloc(pMap,maxPoint*nplan,'r')
	    call memAlloc(pRes,maxPoint*nplan,'r')
	    call memAlloc(pNewRes,maxPoint*nplan,'r')
	    do i=2,npol
	      pEst(i) = pEst(i-1) + maxPoint
	      pNewEst(i) = pNewEst(i-1) + maxPoint
	    enddo
	    do i=2,nplan
	      pMap(i) = pMap(i-1) + maxPoint
	      pRes(i) = pRes(i-1) + maxPoint
	      pNewRes(i) = pNewRes(i-1) + maxPoint
	    enddo
	    call memAlloc(pDef,maxPoint,'r')
	    call memAlloc(pWta,maxPoint,'r')
	    if(npol.ne.nplan)then
	      call memAlloc(pWtb,maxPoint,'r')
	    else
	      pWtb = pWta
	    endif
	  endif
c
c  Initialise the mosaic routines, get 1/sigma**2, and get the dirty image.
c
	  call mcPlaneR(lMap(1),k,Run,nRun,nPoint)
c
c  FUDGE. The beam of single pointing observations does not
c  contain the appropriate information to compute the rms
c  noise -- only the dirty image does. So, if its a single
c  pointing observation, then get the rms from the dirty
c  image, and scale the Wta arrary appropriately.
c
	  if(.not.hdprsnt(lBeam(1),'mostable'))then
	    call rdhdr(lMap(1),'rms',temp,1.)
	    if(temp.le.0)temp = 1
	    call SPntFid(memr(pWta),nPoint,temp)
	  else
	    call mcSigma2(memr(pWta),nPoint,.false.)
	  endif

	  call mcGain(memr(pEst(1)),nPoint)
	  do i=2,npol
	    call zero(memr(pEst(i)),nPoint)
	  enddo
	  do i=1,nplan
	    call xysetpl(lMap(i),1,k)
	    call GetPlane(lMap(i),Run,nRun,0,0,nMap(1),nMap(2),
     *				memr(pMap(i)),maxPoint,nPoint)
	  enddo
	  if(npol.ne.nplan)
     *		call SDConWt(memr(pEst(1)),memr(pWtb),nPoint)
c
c  Get the default image.
c
	  if(DefNam.eq.' ')then
	    call Copy(nPoint,memr(pEst(1)),memr(pDef))
	  else
	    call AlignGet(lDef,Run,nRun,k,xdoff,ydoff,zdoff,
     *		nDef(1),nDef(2),nDef(3),memr(pDef),maxPoint,nPoint)
	    call DefGain(memr(pEst(1)),memr(pDef),nPoint)
	  endif
c
c  Get the Default map.
c
	  Tflux = fluxlist(k-kmin+1)
	  if(TFlux.le.0.and.npol.ne.nplan)then
	    call GetFxSD(memr(pEst(1)),memr(pMap(nplan)),nPoint,TFlux)
	    TFlux = fac * TFlux / ffacSD
	  endif
	  if(TFlux.le.0.and.DefNam.ne.' ')then
	    if(ffacDef.le.0)call GetFFDef(lDef,ffacDef)
	    call GetFxDef(memr(pDef),nPoint,TFlux)
	    TFlux = TFlux / ffacDef
	  endif
	  if(TFlux.le.0)then
	    call GetRms(memr(pWta),nPoint,TFlux)
	    TFlux = RmsFaca*TFlux*nPoint/Qa
	  endif
	  call DefFudge(nPoint,memr(pDef),TFlux)
	  write(line,'(a,1pe10.3)')'Using an integrated flux of',TFlux
	  call output(line)
c
c  Get the Estimate and Residual.
c
	  call Copy(nPoint,memr(pDef),memr(pEst(1)))
c
	  call Diff(memr(pEst(1)),memr(pMap(1)),memr(pRes(1)),nPoint,
     *							       Run,nRun)
	  do i=2,npol
	    call Negate(memr(pMap(i)),memr(pRes(i)),nPoint)
	  enddo
	  if(npol.ne.nplan)call SDConDif(cnvl,memr(pEst(1)),
     *	   memr(pMap(nplan)),fac,memr(pRes(nplan)),memr(pWtb),nPoint,
     *	   Run,nRun,xmax,ymax)
c
c  Get all the information.
c
	  call GetInfo(nPoint,npol,nplan,
     *	      memr(pEst(1)),memr(pRes(1)),memr(pWta),memr(pWtb),
     *	      TRms2*fac*fac,memr(pDef),Alpha,Beta,Gamma,Qa,Qb,
     *		GradEE,GradEF,GradEG,GradEH,GradEJ,
     *		       GradFF,GradFG,GradFH,GradFJ,
     *			      GradGG,GradGH,GradGJ,
     *			             GradHH,GradJJ,
     *		     Grad11,Immax,Immin,Flux,Rmsa,Rmsb,Rmsc)
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
c  Update alpha, beta and gamma.
c
	  De = nPoint*(Rmsa*Rmsa - RmsFaca*RmsFaca)
	  Df = (npol-1)*nPoint*(Rmsb*Rmsb-RmsFacb*RmsFacb)
	  if(npol.ne.nplan)then
	    Dg = nPoint*(Rmsc*Rmsc - RmsFacc*RmsFacc)
	  else
	    Dg = Flux - TFlux
	  endif
	  call NewABG(Alpha,Beta,Gamma,De,Df,Dg,doflux.or.npol.ne.nplan,
     *	    GradEE,GradEF,GradEG,GradFF,GradFG,GradGG,
     *	    GradEH,GradFH,GradGH,GradEJ,GradFJ,GradGJ,Grad11,GradJJ)
c
c  Calculate the next step to take.
c
	  call CalStep(nPoint,npol,nplan,
     *	      memr(pRes(1)),memr(pWta),memr(pWtb),
     *	      TRms2*fac*fac,memr(pEst(1)),memr(pNewEst(1)),memr(pDef),
     *	      Alpha,Beta,Gamma,Qa,Qb,J0)
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
	  call TakeStep(nPoint,npol,memr(pEst(1)),memr(pNewEst(1)),
     *					StLen1,StLim)
c
c  Convolve the estimate with the beam and subtract the map.
c
	  do i=1,npol
	    call Diff(memr(pNewEst(i)),memr(pMap(i)),memr(pNewRes(i)),
     *						nPoint,Run,nRun)
	  enddo
	  if(npol.ne.nplan)call SDConDif(cnvl,memr(pNewEst(1)),
     *	    memr(pMap(nplan)),fac,memr(pNewRes(nplan)),memr(pWtb),
     *	    nPoint,Run,nRun,xmax,ymax)
c
c  Work out what was really the best step length.
c
	  call ChekStep(nPoint,npol,nplan,memr(pEst(1)),
     *		memr(pNewEst(1)),
     *		memr(pDef),memr(pNewRes(1)),memr(pWta),memr(pWtb),
     *		TRms2*fac*fac,Alpha,Beta,Gamma,Qa,Qb,J1)
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
	  if(abs(StLen2-1).gt.0.05)then
	    call PIntStep(nPoint,npol,
     *		memr(pEst(1)),memr(pNewEst(1)),StLen2)
	    do i=1,nplan
	      call IntStep(nPoint,memr(pRes(i)),memr(pNewRes(i)),StLen2)
	    enddo
	  else
	    StLen2 = 1
	    do i=1,npol
	      call Swap(pEst(i),pNewEst(i))
	    enddo
	    do i=1,nplan
	      call Swap(pRes(i),pNewRes(i))
	    enddo
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
	  call GetInfo(nPoint,npol,nplan,
     *	      memr(pEst(1)),memr(pRes(1)),memr(pWta),memr(pWtb),
     *	      TRms2*fac*fac,memr(pDef),Alpha,Beta,Gamma,Qa,Qb,
     *		GradEE,GradEF,GradEG,GradEH,GradEJ,
     *		       GradFF,GradFG,GradFH,GradFJ,
     *			      GradGG,GradGH,GradGJ,
     *			             GradHH,GradJJ,
     *		     Grad11,Immax,Immin,Flux,Rmsa,Rmsb,Rmsc)
c
c  Reawaken the user with more crap to let him/her ponder over
c  what could possibly be going wrong. Give him/her as much as
c  possible to ponder over.
c
	  if(verbose)then
	    call output('Iteration '//itoaf(niter))
	    write(line,22)Flux,GradJJ/Grad11
	    call output(line)
	    write(line,20)Alpha,Beta,Gamma
	    call output(line)
	    write(line,27)Rmsa,Rmsb,Rmsc
	    call output(line)
	    write(line,26)Qa,Qb
	    call output(line)
	    write(line,21)Immin,Immax,fac
	    call output(line)
	    write(line,23)StLim,StLen1,StLen2
	    call output(line)
	  else
	    if(npol.ne.nplan)then
	      write(line,25)Niter,Rmsa,Rmsb,Rmsc,Flux,GradJJ/Grad11
	    else
	      write(line,24)Niter,Rmsa,Rmsb,Flux,GradJJ/Grad11
	    endif
	    call output(line)
	  endif
c
  20	  format('  Alpha =',1pe12.3,' Beta   =',1pe12.3,
     *		' Gamma   =',1pe12.3)
  21	  format('  Immin =',1pe12.3,' Immax  =',1pe12.3,
     *		' Factor  =',1pe12.3)
  22	  format('  Flux  =',1pe12.3,' NormGrd=',1pe12.3)
  23	  format('  StLim =',1pe12.3,' StLen1 =',1pe12.3,
     *		' StLen2  =',1pe12.3)
  24	  format(' Iter =',i3,' RmsFac =',1p2e10.3,' Flux =',1pe10.3,
     *		' NormGrd =',0pf6.3)
  25	  format(' Iter=',i3,' RmsFac=',f6.2,2f7.2,
     *		' Flux=',1pe9.3,' NormGrd=',0pf5.3)
  26	  format('  Q     =  ',1p2e10.3)
  27	  format('  RMS   =  ',1p3e10.3)
c
c  Check for convergence.
c
	  converge = (Rmsa-RmsFaca.lt.0.05*Rmsfaca)		.and.
     *		     (Rmsb-RmsFacb.lt.0.05*Rmsfacb)		.and.
     *		     ((Flux-TFlux).lt.0.05*TFlux.or..not.doflux).and.
     *		      (GradJJ/Grad11.lt.Tol)
	  if(npol.ne.nplan.and.converge)
     *		converge = Rmsc-RmsFacc.lt.0.05*RmsFacc
	enddo
c------------------------------------------------------------------------
c
c  We have finished processing this plane. More info to the user!
c
	    if(converge)then
	      call output('PMOSMEM seems to have converged')
	    else
	      call output('Failed to converge in NITERS iterations')
	    endif
	  endif
c
c  Write out this plane.
c
	  do i=1,npol
	    if(k-kmin+1.gt.1)call xysetpl(lOut(i),1,k-kmin+1)
	    call PutPlane(lOut(i),Run,nRun,1-imin,1-jmin,
     *				nOut(1),nOut(2),memr(pEst(i)),nPoint)
	    call xyflush(lOut(i))
	  enddo
	enddo
c
c  Close up the files. Ready to go home.
c
	call coFin(lMap(1))
	do i=1,nplan
	  call xyclose(lMap(i))
	enddo
	call xyclose(lBeam(1))
	do i=1,npol
	  call xyclose(lOut(i))
	enddo
c
c  Release memory.
c
	if(maxPoint.le.0)call bug('f','No data deconvolved')
	call memFree(pEst,maxPoint*npol,'r')
	call memFree(pNewEst,maxPoint*npol,'r')
	call memFree(pMap,maxPoint*nplan,'r')
	call memFree(pRes,maxPoint*nplan,'r')
	call memFree(pNewRes,maxPoint*nplan,'r')
	call memFree(pDef,maxPoint,'r')
	call memFree(pWta,maxPoint,'r')
	if(npol.ne.nplan)call memFree(pWtb,maxPoint,'r')
c
c  Thats all folks.
c
	end
c************************************************************************
	subroutine SPntFid(Wta,nPoint,rms)
c
	implicit none
	integer nPoint
	real Wta(nPoint),rms
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Wta(i) = 1.0/(rms*rms)
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(verbose,doflux)
c
	implicit none
	logical verbose,doflux
c
c  Get extra processing options and the entropy measure.
c
c  Output:
c    verbose	Give lots of messages.
c    doflux	Constrain the flux.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=2)
	logical present(NOPT)
	character opts(NOPT)*8
	data opts/'verbose ','doflux  '/
c
	call options('options',opts,present,NOPT)
	verbose = present(1)
	doflux  = present(2)
c
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
	  Def(i) = alpha*Def(i)
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
	  flux = flux + Gain(i)*Model(i)
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
	subroutine PIntStep(nPoint,npol,Old,New,FracNew)
c
	implicit none
	integer nPoint,npol
	real FracNew
	real Old(nPoint,npol),New(nPoint,npol)
c
c  Update the current image by interpolating between two previous ones.
c------------------------------------------------------------------------
	real FracOld,mold,mnew,iold,inew,fac
	integer i,j
c
	FracOld = 1. - FracNew
	do i=1,nPoint
	  iold = Old(i,1)
	  Old(i,1) = FracOld*Old(i,1) + FracNew*New(i,1)
	  inew = Old(i,1)
	  mold = 0
	  mnew = 0
	  do j=2,npol
	    mold = mold + Old(i,j)*Old(i,j)
	    Old(i,j) = FracOld*Old(i,j) + FracNew*New(i,j)
	    mnew = mnew + Old(i,j)*Old(i,j)
	  enddo
	  mold = sqrt(mold)
	  mnew = sqrt(mnew)
	  if(mnew.lt.1e-4*inew)then
	    fac = 0
	  else
	    fac = (0.4 + 0.4*mold/iold)*(inew/mnew)
	  endif
	  if(fac.lt.1)then
	    do j=2,npol
	      Old(i,j) = fac*Old(i,j)
	    enddo
	  endif
	enddo
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
	subroutine CalStep(nPoint,npol,nplan,Res,Wta,Wtb,TRms2,
     *	  Est,Step,Def,Alpha,Beta,Gamma,Qa,Qb,J0)
c
	implicit none
	integer nPoint,npol,nplan
	real Alpha,Beta,Gamma,Qa,Qb,J0,TRms2
	real Def(nPoint),Est(nPoint,npol),Step(nPoint,npol)
	real Res(nPoint,nplan),Wta(nPoint),Wtb(nPoint)
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
c
c  Output:
c    Step	The step to take towards a better estimate.
c    Length	A measure of the length of the step.
c
c------------------------------------------------------------------------
	integer i,j
	real Diag, GradJ, Stepd,m,mlog,mexp,x
	real dH,dH2
c
	J0 = 0
	do i=1,nPoint
	  m = 0
	  do j=2,npol
	    m = m + est(i,j)*est(i,j)
	  enddo
	  m = sqrt(m)
	  if(m.gt.0.01*est(i,1))then
	    mlog = log((est(i,1)+m)/(est(i,1)-m))/m
	    mexp = (est(i,1)/(est(i,1)*est(i,1)-m*m) - 0.5*mlog)/(m*m)
	  else
	    x = m/est(i,1)
	    mlog = 2.0*(1.0+ x*x*(1.0/3.0 + 0.2*x*x))/est(i,1)
	    mexp = 2.0*(1.0/3.0 + 0.4*x*x)/(est(i,1)*est(i,1)*est(i,1))
	  endif
	  do j=1,npol
	    if(j.eq.1)then
	      dH  = -0.5*log((est(i,1)*est(i,1)-m*m)/(def(i)*def(i)))
	      dH2 = -est(i,1)/(est(i,1)*est(i,1) - m*m)
	      if(nplan.eq.npol)then
		gradj = dH - 2*Qa*alpha*Res(i,1)*wta(i) - gamma
		diag = 1./(2.*alpha*Qa*Qa*Wta(i) - dH2)
	      else
		gradj = dH - 2*Qa*alpha*Res(i,1)*wta(i)
     *			   - 2*Qb*gamma*Res(i,nplan)*wtb(i)/TRms2
		diag = 1./( 2.*alpha*Qa*Qa*Wta(i) + 
     *			    2.*gamma*Qb*Qb*Wtb(i)*Wtb(i)/TRms2 - dH2)
	      endif
	    else
	      dH  = -0.5*est(i,j)*mlog
	      dH2 = -0.5*(mlog + est(i,j)*est(i,j)*mexp)
	      gradj = dH -2*Qa*beta*Res(i,j)*wta(i)
	      diag = 1./(2.*beta*Qa*Qa*Wta(i) - dH2)
	    endif
	    Stepd = Diag*GradJ
	    J0 = J0 + GradJ*Stepd
	    Step(i,j) = Stepd
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine TakeStep(nPoint,npol,Est,NewEst,StLen,StLim)
c
	implicit none
	integer nPoint,npol
	real Est(nPoint,npol),NewEst(nPoint,npol)
	real StLen,StLim
c
c  Take the final step!
c
c------------------------------------------------------------------------
	integer i,j
	real Stepd,mold,mnew,iold,inew,fac
c
	do i=1,nPoint
	  mold = 0
	  mnew = 0
	  do j=2,npol
	    mold = mold + Est(i,j)*Est(i,j)
	    mnew = mnew + (Est(i,j)+StLen*NewEst(i,j))**2
	  enddo
	  mold = sqrt(mold)
	  mnew = sqrt(mnew)
c
	  iold = Est(i,1)
	  Stepd = StLen*max(NewEst(i,1),-0.9*Est(i,1)/StLim)
	  NewEst(i,1) = max(Est(i,1) + Stepd,1e-12)
	  inew = NewEst(i,1)
	  if(mnew.lt.1e-4*inew)then
	    fac = 0
	  else
	    fac = min(1.0,(0.4 + 0.4*mold/iold)*(inew/mnew))
	  endif
	  do j=2,npol
	    NewEst(i,j) = fac*(Est(i,j) + StLen*NewEst(i,j))
	  enddo
	enddo
	end
c************************************************************************
	subroutine ChekStep(nPoint,npol,nplan,OldEst,Est,Def,Res,
     *		Wta,Wtb,TRms2,Alpha,Beta,Gamma,Qa,Qb,J0)
c
	implicit none
	integer nPoint,npol,nplan
	real OldEst(nPoint,npol),Est(nPoint,npol),Def(nPoint)
	real Res(nPoint,nplan),Wta(nPoint),Wtb(nPoint)
	real Alpha,Beta,Gamma,Qa,Qb,J0,TRms2
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
c
c  Output:
c    J0		Some useful (??) statistic.
c
c------------------------------------------------------------------------
	integer i,j
	real GradJ,Step,dH,mlog,m,x
c
	J0 = 0.
	do i=1,nPoint
	  m = 0
	  do j=2,npol
	    m = m + est(i,j)*est(i,j)
	  enddo
	  m = sqrt(m)
	  if(m.gt.0.01*est(i,1))then
	    mlog = log((est(i,1)+m)/(est(i,1)-m))/m
	  else
	    x = m/est(i,1)
	    mlog = 2.0*(1.0+ x*x*(1.0/3.0 + 0.2*x*x))/est(i,1)
	  endif
	  do j=1,npol
	    if(j.eq.1)then
	      dH  = -0.5*log((est(i,1)*est(i,1)-m*m)/(def(i)*def(i)))
	      if(nplan.eq.npol)then
		gradj = dH - 2*Qa*alpha*Res(i,1)*wta(i) - gamma
	      else
		gradj = dH - 2*Qa*alpha*Res(i,1)*wta(i)
     *			   - 2*Qb*gamma*Res(i,nplan)*wtb(i)/TRms2
	      endif
	    else
	      dH  = -0.5*est(i,j)*mlog
	      gradj = dH -2*Qa*beta*Res(i,j)*wta(i)
	    endif
	    Step = Est(i,j) - OldEst(i,j)
	    J0 = J0 + GradJ*Step
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GetInfo(nPoint,npol,nplan,Est,Res,Wta,Wtb,TRms2,
     *	  Def,Alpha,Beta,Gamma,Qa,Qb,
     *		GradEE,GradEF,GradEG,GradEH,GradEJ,
     *		       GradFF,GradFG,GradFH,GradFJ,
     *			      GradGG,GradGH,GradGJ,
     *			             GradHH,GradJJ,
     *	  Grad11,Immax,Immin,Flux,Rmsa,Rmsb,Rmsc)
c
	implicit none
	integer nPoint,npol,nplan
	real Res(nPoint,nplan),Wta(nPoint),Wtb(nPoint)
	real Est(nPoint,npol),Def(nPoint)
	real Alpha,Beta,Gamma,Qa,Qb,TRms2
	real	GradEE,GradEF,GradEG,GradEH,GradEJ,
     *		       GradFF,GradFG,GradFH,GradFJ,
     *			      GradGG,GradGH,GradGJ,
     *			             GradHH,GradJJ
	real Grad11,Immax,Immin,Flux,Rmsa,Rmsb,Rmsc
c
c  Get information on the current state of play.
c
c  Inputs:
c    nPoint	Number of points in the input.
c    Res,Est	The Residuals and Estimate respectively.
c    Def	The default image.
c    Wta	1/Sigma**2 for the mosaic.
c    Alpha
c    Beta
c    Gamma
c    Qa		Mosaic value for Q.
c    Qb		Single dish value for Q.
c
c  Outputs:
c    GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
c    GradHH,GradJJ,NomGrd,Immax,Immin,Flux,Rms
c------------------------------------------------------------------------
	integer i,j
	real Diag,dH,dH2,dE,dF,dG,temp,m,mexp,mlog,x
c
	GradEE = 0.
	GradEF = 0.
	GradEG = 0.
	GradEH = 0.
	GradFF = 0.
	GradFG = 0.
	GradFH = 0.
	GradGG = 0.
	GradGH = 0
	GradHH = 0.
	Rmsa   = 0.
	Rmsb   = 0.
	Rmsc   = 0.
	Flux   = 0.
	Immin = Est(1,1)
	Immax = Immin
	temp = 0
c
	do i=1,nPoint
	  m = 0
	  do j=2,npol
	    m = m + est(i,j)*est(i,j)
	  enddo
	  m = sqrt(m)
	  if(m.gt.0.01*est(i,1))then
	    mlog = log((est(i,1)+m)/(est(i,1)-m))/m
	    mexp = (est(i,1)/(est(i,1)*est(i,1)-m*m) - 0.5*mlog)/(m*m)
	  else
	    x = m/est(i,1)
	    mlog = 2.0*(1.0+ x*x*(1.0/3.0 + 0.2*x*x))/est(i,1)
	    mexp = 2.0*(1.0/3.0 + 0.4*x*x)/(est(i,1)*est(i,1)*est(i,1))
	  endif
	  do j=1,npol
	    if(j.eq.1)then
	      Flux = Flux + Est(i,1)
	      Rmsa = Rmsa + Wta(i) * Res(i,1)**2
	      dH  = -0.5*log((est(i,1)*est(i,1)-m*m)/(def(i)*def(i)))
	      dH2 = -est(i,1)/(est(i,1)*est(i,1) - m*m)
	      dE  = 2*Qa*Res(i,1)*Wta(i)
	      dF  = 0
	      if(nplan.eq.npol)then
		dG = 1
		diag = 1./(2.*alpha*Qa*Qa*Wta(i) - dH2)
	      else
	        Rmsc = Rmsc + Res(i,nplan)**2/TRms2
		dG = 2*Qb*Res(i,nplan)*Wtb(i)/TRms2
		diag = 1./( 2.*alpha*Qa*Qa*Wta(i) + 
     *			    2.*gamma*Qb*Qb*Wtb(i)*Wtb(i)/TRms2 - dH2)
	      endif
	    else
	      Rmsb = Rmsb + Wta(i) * Res(i,j)**2
	      dH  = -0.5*est(i,j)*mlog
	      dH2 = -0.5*(mlog + est(i,j)*est(i,j)*mexp)
	      dE  = 0
	      dF  = 2*Qa*Res(i,j)*Wta(i)
	      dG  = 0
	      diag = 1./(2.*beta*Qa*Qa*Wta(i) - dH2)
	    endif
	    GradEE = GradEE + dE*diag*dE
	    GradEG = GradEG + dE*diag*dG
	    GradEH = GradEH + dE*diag*dH
	    GradFF = GradFF + dF*diag*dF
	    GradFH = GradFH + dF*diag*dH
	    GradGG = GradGG + dG*diag*dG
	    GradGH = GradGH + dG*diag*dH
	    GradHH = GradHH + dH*diag*dH
	    temp = temp + diag
	    Immin = min(Immin,Est(i,j))
	    Immax = max(Immax,Est(i,j))
	  enddo
	enddo
c
c  Finish up various variables.
c
	Rmsa = sqrt(Rmsa/real(nPoint))
	Rmsb = sqrt(Rmsb/real((npol-1)*nPoint))
	Rmsc = sqrt(Rmsc/real(nPoint))
	GradEJ = GradEH - alpha*GradEE - beta*GradEF - gamma*GradEG
	GradFJ = GradFH - alpha*GradEF - beta*GradFF - gamma*GradFG
	GradGJ = GradGH - alpha*GradEG - beta*GradFG - gamma*GradGG
	GradJJ = GradHH + alpha*alpha*GradEE + beta*beta*GradFF
     *			+ gamma*gamma*GradGG
     *		- 2.*alpha*GradEH - 2.*Beta*GradFH - 2*gamma*GradGH
     *		+ 2.*alpha*beta*GradEF + 2.*alpha*gamma*GradEG
     *		+ 2.*beta*gamma*GradFG
	Grad11 = GradHH + alpha**2*GradEE + beta**2*GradFF
     *					  + gamma**2*GradGG
	if(Grad11.le.0)Grad11 = temp
c
	end
c************************************************************************
	subroutine Zero(Est,nPoint)
c
	implicit none
	integer nPoint
	real Est(nPoint)
c
c------------------------------------------------------------------------
	integer i
	do i=1,nPoint
	  Est(i) = 0
	enddo
c
	end
c************************************************************************
	subroutine Negate(In,Out,nPoint)
c
	implicit none
	integer nPoint
	real In(nPoint),Out(nPoint)
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Out(i) = -In(i)
	enddo
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
        line = 'PMOSMEM: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'PMOSMEM')
c
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	line = 'PMOSMEM: Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'
	call hiswrite(lOut,line)
c
	call hisclose(lOut)
c
	end
c************************************************************************
	subroutine NewAbg(alpha,beta,gamma,De,Df,Dg,dothree,
     *	  GradEE,GradEF,GradEG,GradFF,GradFG,GradGG,
     *	  GradEH,GradFH,GradGH,GradEJ,GradFJ,GradGJ,Grad11,GradJJ)
c
	implicit none
	real Alpha,Beta,Gamma,De,Df,Dg
	real GradEE,GradEF,GradEG,GradFF,GradFG,GradGG
	real GradEH,GradFH,GradGH,GradEJ,GradFJ,GradGJ
	real Grad11,GradJJ
	logical dothree
c
c  Determine new values for alpha and beta.
c------------------------------------------------------------------------
	real tol1,tol2
	parameter(tol1=0.1,tol2=0.05)
c
	real Dalp,Dbet,Dgam,Alpha1,Alpha2,Beta1,Beta2,Gamma1,Gamma2
	real Denom,l,b2m4ac
	real a(3,3),b(3)
	integer pivot(3),ifail
c
c  Check if things are doing poorly. If so, just aim at reducing the
c  gradient.
c
	l = abs(GradJJ/Grad11)
	if(Alpha.le.0)l = 0
c
	if(dothree)then
	  a(1,1) = GradEE
	  a(1,2) = GradEF
	  a(1,3) = GradEG
	  a(2,1) = GradEF
	  a(2,2) = GradFF
	  a(2,3) = GradFG
	  a(3,1) = GradEG
	  a(3,2) = GradFG
	  a(3,3) = GradGG
	  call sgefa(a,3,3,pivot,ifail)
	  if(ifail.ne.0)
     *	    call bug('f','Matrix inversion failed -- singular matrix?')
c
	  b(1) = GradEH
	  b(2) = GradFH
	  b(3) = GradGH
	  call sgesl(a,3,3,pivot,b,1)
	  Alpha1 = b(1)
	  Beta1  = b(2)
	  Gamma1 = b(3)
c
	  b(1) = De + GradEJ
	  b(2) = Df + GradFJ
	  b(3) = Dg + gradGJ
	  call sgesl(a,3,3,pivot,b,1)
	  Dalp = b(1)
	  Dbet = b(2)
	  Dgam = b(3)
	else
	  Denom = 1./(GradEE*GradFF - GradEF*GradEF)
	  Alpha1 = (GradFF*GradEH - GradEF*GradFH) * Denom
	  Beta1  = (GradEE*GradFH - GradEF*GradEH) * Denom
	  Gamma1 = 0
c
	  Denom = 1./(GradEE*GradFF - GradEF*GradEF)
	  Dalp = ( GradFF*(De+GradEJ) - GradEF*(Df+GradFJ) ) * Denom
	  Dbet =-( GradEF*(De+GradEJ) - GradEE*(Df+GradFJ) ) * Denom
	  Dgam = 0
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
	b2m4ac = GradGJ*GradGJ - (GradJJ-tol1*Grad11)*GradGG
	if(b2m4ac.gt.0)then
	  b2m4ac = sqrt(b2m4ac)
	  Dgam = max((GradGJ - b2m4ac)/GradGG,
     *		 min((GradGJ + b2m4ac)/GradGG,Dgam))
	else
	  Dgam = 0
	endif
c
	Alpha2 = Alpha+ Dalp
	Beta2  = Beta + Dbet
	Gamma2 = Gamma + Dgam
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
	if(l.ge.tol2.or.Gamma2.le.0)then
	  Gamma = max(Gamma1,0.)
	else
	  Gamma = max(Gamma2,0.)
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
