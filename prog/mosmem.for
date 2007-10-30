c************************************************************************
	program mosmem
	implicit none
c
c= mosmem - Maximum Entropy deconvolution for a mosaiced image
c& rjs
c: deconvolution
c+
c	MOSMEM is a MIRIAD task which performs a (joint) maximum entropy
c	deconvolution of a mosaiced image.
c@ map
c	The input dirty map, which should have units of Jy/beam, which was
c	produced by INVERTs mosaic mode.
c@ beam
c	The input dirty beam, also produced by INVERTs mosaic mode. No
c	default
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
c	It can be input to RESTOR or MOSMEM (as a model, to continue the
c	deconvolution process).
c@ niters
c	The maximum number of iterations. The default is 30.
c@ region
c	This specifies the region to be deconvolved. See the Users Manual
c	for instructions on how to specify this. The default is the entire
c	image.
c@ measure
c	The entropy measure to be used, either "gull" (-p*log(p/e)) or
c	"cornwell" (-log(cosh(p)) -- also called the maximum emptiness
c	criteria).
c@ tol
c	Tolerance of solution. There is no need to change this from the
c	default of 0.01.
c@ q	
c	An estimate of the number of points per beam. MOSMEM can usually
c	come up with a good, image-dependent estimate.
c@ rmsfac
c	MOSMEM knows the theoretical rms noise of the input dirty map, and 
c	will, by default, attempt to reduce the residuals to have an rms of
c	this amount. If the true rms noise is different from the theoretical,
c	you may give the factor to multiply by to convert from theoretical
c	to true rms noise.
c
c	The theoretical rms will usually be an optimistic estimate of the
c	true noise level. The true noise will be increased by calibration
c	errors, confusion, poorly understood distant sidelobes, etc, so
c	rmsfac will usually give some `fudge factor' greater than 1.
c@ flux
c	An estimate of the total flux of the source. Giving a good total flux
c	will help MOSMEM find a good solution. On the other hand, giving
c	a poor value may do harm. Normally MOSMEM will NOT constrain the
c	total flux to be this value, but see the ``doflux'' option below.
c	The default is image-dependent for measure=gull, and zero for
c	measure=cornwell. A value can be given for each plane being
c	deconvolved.
c@ options
c	Task enrichment parameters. Several can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  doflux     Constraint the flux to be that given by the "flux"
c	             parameter. Normally the "flux" parameter value is only
c	             used to determine the default image level.
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
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='MosMem: version 1.0 24-Jun-97')
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	integer MaxRun,MaxBoxes
	parameter(MaxRun=3*maxdim,MaxBoxes=1024)
c
	integer gull,cornwell
	parameter(gull=1,cornwell=2)
c
	character MapNam*64,BeamNam*64,ModelNam*64,OutNam*64,DefNam*64
	character entropy*8,line*72
	integer lBeam,lMap,lModel,lOut,lDef
	integer nMap(3),nModel(3),nOut(MAXNAX),nBeam(3),nDef(3)
	integer xmin,ymin,xmax,ymax,n1,n2,i
	integer imin,imax,jmin,jmax,kmin,kmax,blc(3),trc(3),naxis,k
	integer icentre,jcentre
	integer maxniter,niter
	integer measure
	real Tol,rmsfac,TFlux,Qest,Q
	real Alpha,Beta,De,Df
	real StLim,StLen1,StLen2,OStLen1,OStLen2,J0,J1
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms
	real fluxlist(maxdim)
	integer nfret
	logical converge,positive,verbose,doflux
	integer Run(3,MaxRun),nRun,Boxes(maxBoxes),nPoint,maxPoint
	integer xmoff,ymoff,zmoff,xdoff,ydoff,zdoff
c
	integer pMap,pEst,pDef,pRes,pNewEst,pNewRes,pWt
c
c  Externals.
c
	character itoaf*4
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('map',MapNam,' ')
	call keya('beam',BeamNam,' ')
	call keya('model',ModelNam,' ')
	call keya('default',DefNam,' ')
	call keya('out',OutNam,' ')
	call keyr('tol',Tol,0.01)
	call keyi('niters',maxniter,30)
	call keyr('q',Q,0.)
	call keyr('rmsfac',rmsfac,1.)
	call mkeyr('flux',fluxlist,maxdim,nfret)
	call BoxInput('region',MapNam,Boxes,MaxBoxes)
	call GetOpt(verbose,doflux,entropy)
	call keyfin
c
c  Check everything makes sense.
c
	if(rmsfac.le.0.0)call bug('f','RMSFAC is not positive')
	if(rmsfac.lt.0.9)call bug('w','RMSFAC seems small')
	if(maxniter.lt.0)call bug('f','NITERS was given a bad value')
	if(MapNam.eq.' '.or.BeamNam.eq.' '.or.OutNam.eq.' ')
     *	  call bug('f','A file name was missing from the parameters')
	if(Tol.le.0.)
     *	  call bug('f','The TOL parameter must be positive valued')
c
	if(entropy.eq.'gull')then
	  measure = gull
	  positive = .true.
	else if(entropy.eq.'cornwell')then
	  measure = cornwell
	  positive = .false.
	endif
c
c  Open the beam, and get some info about it.
c
	call xyopen(lBeam,BeamNam,'old',3,nBeam)
	n1 = nBeam(1)
	n2 = nBeam(2)
	if(max(n1,n2).gt.maxdim) call bug('f','Beam too big')
	call BeamChar(lBeam,n1,n2,Qest,icentre,jcentre)
	write(line,'(a,1pg8.1)')'An estimate of Q is',Qest
	call output(line)
	if(Q.gt.0.)then
	  write(line,'(a,1pg8.1)')
     *			'Using user given pixels per beam of',Q
	  call output(line)
	else
	  Q = Qest
	endif
c
	call mcInitF(lBeam)
c
c  Open the input map.
c
	call xyopen(lMap,MapNam,'old',3,nMap)
	if(max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
	call rdhdi(lMap,'naxis',naxis,3)
	naxis = min(naxis,MAXNAX)
	call coInit(lMap)
	call BoxMask(lMap,boxes,maxboxes)
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
c  Open the model if needed, and check that is is the same size as the
c  output.
c
	if(ModelNam.ne.' ')then
	  call xyopen(lModel,ModelNam,'old',3,nModel)
	  call AlignIni(lModel,lMap,nMap(1),nMap(2),nMap(3),
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
	if(DefNam.ne.' ')then
	  call xyopen(lDef,DefNam,'old',3,nDef)
	  call AlignIni(lDef,lMap,nMap(1),nMap(2),nMap(3),
     *						xdoff,ydoff,zdoff)
	endif
c
c  Open up the output.
c
	do i=4,naxis
	  nOut(i) = 1
	enddo
	call xyopen(lOut,OutNam,'new',naxis,nOut)
	call Header(lMap,lOut,blc,trc,version)
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
	      call memFree(pMap,maxPoint,'r')
	      call memFree(pWt,maxPoint,'r')
	      call memFree(pEst,maxPoint,'r')
	      call memFree(pDef,maxPoint,'r')
	      call memFree(pRes,maxPoint,'r')
	      call memFree(pNewEst,maxPoint,'r')
	      call memFree(pNewRes,maxPoint,'r')
	    endif
	    maxPoint = nPoint
	    call memAlloc(pMap,maxPoint,'r')
	    call memAlloc(pWt,maxPoint,'r')
	    call memAlloc(pEst,maxPoint,'r')
	    call memAlloc(pDef,maxPoint,'r')
	    call memAlloc(pRes,maxPoint,'r')
	    call memAlloc(pNewEst,maxPoint,'r')
	    call memAlloc(pNewRes,maxPoint,'r')
	  endif
c
c  Initialise the mosaic routines, get 1/sigma**2, and get the dirty image.
c
	  call mcPlaneR(lMap,k,Run,nRun,nPoint)
	  call mcSigma2(memr(pWt),nPoint,.false.)
	  call xysetpl(lMap,1,k)
	  call GetPlane(lMap,Run,nRun,0,0,nMap(1),nMap(2),
     *				memr(pMap),maxPoint,nPoint)
c
c  Get the Default map.
c
	  TFlux=fluxlist(k-kmin+1)
	  if(TFlux.le.0.and.positive)then
	    call GetRms(memr(pWt),nPoint,TFlux)
	    TFlux = RmsFac*TFlux*nPoint/Q
	  endif
c
	  if(DefNam.eq.' ')then
	    call mcGain(memr(pDef),nPoint)
	    call DefScal(memr(pDef),npoint,TFlux)
	  else
	    call AlignGet(lDef,Run,nRun,k,xdoff,ydoff,zdoff,
     *		nDef(1),nDef(2),nDef(3),memr(pDef),maxPoint,nPoint)
	  endif
c
c  Get the Estimate and Residual. Also get information about the
c  current situation.
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
	  call Diff(memr(pEst),memr(pMap),memr(pRes),nPoint,Run,nRun)
c
c  Get all the information.
c
	  call GetInfo(nPoint,
     *	      memr(pEst),memr(pRes),measure,memr(pDef),memr(pWt),
     *	      Alpha,Beta,Q,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)
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
	  De = nPoint*(Rms*Rms - RmsFac*RmsFac)
	  Df = Flux - TFlux
	  call NewAlpB(Alpha,Beta,De,Df,doflux,GradEE,GradEF,
     *		GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
c  Calculate the next step to take.
c
	  call CalStep(nPoint,
     *	      memr(pEst),memr(pRes),memr(pNewEst),memr(pWt),memr(pDef),
     *	      measure,Alpha,Beta,Q,J0)
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
	  call Diff(memr(pNewEst),memr(pMap),memr(pNewRes),
     *						nPoint,Run,nRun)
c
c  Work out what was really the best step length.
c
	  call ChekStep(nPoint,memr(pEst),memr(pNewEst),memr(pNewRes),
     *		memr(pDef),memr(pWt),measure,Alpha,Beta,Q,J1)
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
	    call IntStep(nPoint,memr(pRes),memr(pNewRes),StLen2)
	  else
	    StLen2 = 1
	    call Swap(pEst,pNewEst)
	    call Swap(pRes,pNewRes)
	  endif
c
c  Calculate a new estimate for Q using a magic formula.
c
	if(abs(StLen1-1.).lt.0.05)
     *	  Q = Q * sqrt((1./max(0.5,min(2.,StLen1*StLen2))+3.)/4.)
c
c  Get all the information.
c
	  call GetInfo(nPoint,
     *	      memr(pEst),memr(pRes),measure,memr(pDef),memr(pWt),
     *	      Alpha,Beta,Q,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)
c
c  Reawaken the user with more crap to let him/her ponder over
c  what could possibly be going wrong. Give him/her as much as
c  possible to ponder over.
c
	  if(verbose)then
	    call output('Iteration '//itoaf(niter))
	    write(line,20)Alpha,Beta,Q
	    call output(line)
	    write(line,21)Immin,Immax
	    call output(line)
	    write(line,22)Rms,Flux,GradJJ/Grad11
	    call output(line)
	    write(line,23)StLim,StLen1,StLen2
	    call output(line)
	  else
	    write(line,24)Niter,Rms,Flux,GradJJ/Grad11
	    call output(line)
	  endif
c
  20	  format('  Alpha =',1pe12.3,' Beta  =',1pe12.3,
     *		' Q       =',1pe12.3)
  21	  format('  Immin =',1pe12.3,' Immax =',1pe12.3)
  22	  format('  Rms   =',1pe12.3,' Flux  =',1pe12.3,
     *		' NormGrd =',1pe12.3)
  23	  format('  StLim =',1pe12.3,' StLen1=',1pe12.3,
     *		' StLen2  =',1pe12.3)
  24	  format(' Iter =',i3,' RmsFac =',1pe10.3,' Flux =',1pe10.3,
     *		' NormGrd =',1pe10.3)
c
c  Check for convergence.
c
	  converge = (Rms-RmsFac.lt.0.05*Rmsfac)		.and.
     *		     ((Flux-TFlux).lt.0.05*TFlux.or..not.doflux).and.
     *		      (GradJJ/Grad11.lt.Tol)
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
	call xyclose(lMap)
	call xyclose(lBeam)
	if(ModelNam.ne.' ')call xyclose(lModel)
	call xyclose(lOut)
c
c  Thats all folks.
c
	end
c************************************************************************
	subroutine GetOpt(verbose,doflux,entropy)
c
	implicit none
	logical verbose,doflux
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
	parameter(NOPT=2)
	logical present(NOPT)
	character opts(NOPT)*8
c
	integer NMEASURE
	parameter(NMEASURE=2)
	integer nout
	character measure(NOPT)*8
c
	data measure/'gull    ','cornwell'/
	data opts/'verbose ','doflux  '/
c
	call options('options',opts,present,NOPT)
	verbose = present(1)
	doflux  = present(2)
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
	subroutine DefScal(Def,npoint,TFlux)
c
	implicit none
	integer npoint
	real Def(npoint),TFlux
c
c  Scale the default so that it has the right total flux.
c
c------------------------------------------------------------------------
	real sum,alpha,minlev
	integer i
c
	sum = 0
	do i=1,npoint
	  sum = sum + Def(i)
	enddo
c
	if(Sum.le.0)call bug('f','Cannot scale default image')
c
	alpha = TFlux/Sum
	minlev = 1e-6*TFlux/npoint
	do i=1,npoint
	  Def(i) = max(alpha*Def(i),minlev)
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
	subroutine BeamChar(lBeam,n1,n2,Qest,icentre,jcentre)
c
	implicit none
	integer lBeam,n1,n2,icentre,jcentre
	real Qest
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
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nP
	parameter(nP=8)
	integer imin,imax,jmin,jmax,i,j
	real Sum,bmax,Data(maxdim)
c
	integer ismax
c
	imin = max(n1/2+1-nP,1)
	imax = min(n1/2+1+nP,n1)
	jmin = max(n2/2+1-nP,1)
	jmax = min(n2/2+1+nP,n2)
c
	sum = 0
	bmax = 0
	icentre = 0
	jcentre = 0
	do j=jmin,jmax
	  call xyread(lBeam,j,Data)
	  do i=imin,imax
	    Sum = Sum + Data(i)*Data(i)
	  enddo
	  i = ismax(n1,Data,1)
	  if(Data(i).gt.bmax)then
	    icentre = i
	    jcentre = j
	    bmax = Data(i)
	  endif
	enddo
c
	Qest = sqrt(8*Sum)
	if(abs(1-bmax).gt.0.01) call bug('f','Beam peak is not 1')
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
	subroutine CalStep(nPoint,Est,Res,Step,Wt,Def,
     *		measure,Alpha,Beta,Q,J0)
c
	implicit none
	integer nPoint,measure
	real Alpha,Beta,Q,J0
	real Def(nPoint),Est(nPoint),Res(nPoint),Step(nPoint),Wt(nPoint)
c
c  Calculate the step to take next.
c
c  Inputs:
c    nPoint	Number of points.
c    Est	Current estimate of the MEM solution.
c    Res	Current residuals.
c    Def	The default image.
c    Wt		1/Sigma**2 image.
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
	    Diag = 1 / (2*Alpha*Q*Q*Wt(n+l) - d2H(l))
	    GradJ = dH(l) - 2.*Q*Alpha*Res(n+l)*Wt(n+l) - Beta
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
	    NewEst(i) = Est(i) + Stepd
	  enddo
	else
	  do i=1,nPoint
	    NewEst(i) = Est(i) + StLen*NewEst(i)
	  enddo
	endif
	end
c************************************************************************
	subroutine ChekStep(nPoint,OldEst,Est,Res,Def,Wt,
     *			measure,Alpha,Beta,Q,J0)
c
	implicit none
	integer nPoint,Measure
	real OldEst(nPoint),Est(nPoint),Res(nPoint),Def(nPoint)
	real Wt(nPoint)
	real Alpha,Beta,Q,J0
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
	    GradJ = dH(l) - 2.*Alpha*Q*Res(n+l)*Wt(n+l) - Beta
	    Step = Est(n+l) - OldEst(n+l)
	    J0 = J0 + GradJ*Step
	  enddo
	  n = n + ltot
	enddo
c
	end
c************************************************************************
	subroutine GetInfo(nPoint,Est,Res,Measure,Def,Wt,Alpha,Beta,Q,
     *	  GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ,
     *    GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)
c
	implicit none
	integer nPoint
	real Res(nPoint),Est(nPoint),Wt(nPoint),Def(nPoint)
	integer Measure
	real Alpha,Beta,Q
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms
c
c  Get information on the current state of play.
c
c  Inputs:
c    nPoint	Number of points in the input.
c    Res,Est	The Residuals and Estimate respectively.
c    measure	Determines the entropy measure used.
c    Def	The default image.
c    Wt		1/Sigma**2
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
	real Diag,GradE,GradH
	real dH(Run),d2H(Run)
c
	GradEE = 0.
	GradEF = 0.
	GradEH = 0.
	GradFF = 0.
	GradFH = 0.
	GradHH = 0.
	Rms    = 0.
	Flux   = 0.
	Immin = Est(1)
	Immax = Immin
c
	n = 0
	do while(n.lt.nPoint)
	  ltot = min(Run,nPoint-n)
	  call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
	  do l=1,ltot
	    GradE = 2. * Q * Res(n+l) * Wt(n+l)
	    GradH = dH(l)
	    Diag = 1./( 2.*Alpha*Q*Q*Wt(n+l) - d2H(l) )
	    GradEE = GradEE + GradE*Diag*GradE
	    GradEF = GradEF + GradE*Diag
	    GradEH = GradEH + GradE*Diag*GradH
	    GradFF = GradFF +       Diag
	    GradFH = GradFH +       Diag*GradH
	    GradHH = GradHH + GradH*Diag*Gradh
	    Flux = Flux + Est(n+l)
	    Rms  = Rms + Wt(n+l) * Res(n+l)**2
	    Immin = min(Immin,Est(n+l))
	    Immax = max(Immax,Est(n+l))
	  enddo
	  n = n + ltot
	enddo
c
c  Finish up various variables.
c
	Rms = sqrt(Rms/real(nPoint))
	GradEJ = GradEH - Alpha*GradEE - Beta*GradEF
	GradFJ = GradFH - Alpha*GradEF - Beta*GradFF
	GradJJ = GradHH + Alpha*Alpha*GradEE + Beta*Beta*GradFF
     *		- 2.*Alpha*GradEH - 2.*Beta*GradFH
     *		+ 2.*Alpha*Beta*GradEF
	Grad11 = GradHH + alpha**2*GradEE + beta**2*GradFF
	if(Grad11.le.0)Grad11 = GradFF
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
	parameter(nkeys=16)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'obstime ','epoch   ','history ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','object  ','pbfwhm  ',
     *	  'observer','telescop','restfreq','vobs    ','btype   ',
     *	  'mostable','cellscal'/
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
	subroutine NewAlpB(Alpha,Beta,De,Df,doflux,GradEE,GradEF,
     *		GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
	implicit none
	real Alpha,Beta,De,Df,GradEE,GradEF
	real GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH
	logical doflux
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
	if(doflux)then
	  Denom = 1./(GradEE*GradFF - GradEF*GradEF)
	  Alpha1 = (GradFF*GradEH - GradEF*GradFH) * Denom
	  Beta1  = (GradEE*GradFH - GradEF*GradEH) * Denom
	else
	  Alpha1 = GradEH / GradEE
	  Beta1  = 0
	endif
c
	if(doflux)then
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
