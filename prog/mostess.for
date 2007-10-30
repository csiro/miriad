c************************************************************************
	program mostess
	implicit none
c
c= mostess - Maximum Entropy deconvolution for a mosaiced observation
c& rjs
c: deconvolution
c+
c	MOSMANY performs a joint maximum entropy deconvolution of a
c	collection of dirty images from a mosaiced experiment.
c@ map
c	The input dirty maps. No default.
c@ beam
c	The input dirty beams. No default.
c@ default
c	Default image name. The default is a flat image.
c@ model
c	Name of the initial estimate. Default is a flat initial estimate.
c@ out
c	The name of the output map.
c@ niters
c	The maximum number of iterations. The default is 30.
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
c	MOSTESS knows the theoretical rms noise of the input dirty maps, and 
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
c	measure=cornwell.
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
c    rjs   7aug95  Adapted from MOSMEM.
c    rjs  02jul97  cellscal change.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='MosTess: version 1.0 7-Aug-95')
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
c
	integer gull,cornwell
	parameter(gull=1,cornwell=2)
c
	integer MAXPNT
	parameter(MAXPNT=350)
	integer lBeam,lDef,lMap,lOut,lMod
	integer n,npnt,nOut(MAXNAX),nBeam(2),nDef(3),nMod(3),nMap(2)
	integer mnx,mny,nx,ny,naxis,n1,n2
	character MapNam(MAXPNT)*64,BeamNam(MAXPNT)*64
	character DefNam*64,ModNam*64,OutNam*64,entropy*16
	character line*80
	real sigma(MAXPNT)
	integer x0(MAXPNT),y0(MAXPNT)
	integer Cnvl(MAXPNT),pbObj(MAXPNT)
	integer i,icentre,jcentre,offset(3)
	integer maxniter,niter
	integer measure
	real Tol,rmsfac,TFlux,Qest,Q,Sigt
	real Alpha,Beta,De,Df
	real StLim,StLen1,StLen2,OStLen1,OStLen2,J0,J1
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms,ClipLev
	logical converge,positive,verbose,doflux
c
	integer pMap,pEst,pDef,pRes,pNewEst,pNewRes,pWt,pDChi,pNewDChi
	integer pTmp
c
c  Externals.
c
	character itoaf*4
	integer ismax
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call mkeyf('map',MapNam,MAXPNT,npnt)
	call mkeyf('beam',BeamNam,MAXPNT,n)
	call keya('default',DefNam,' ')
	call keya('model',ModNam,' ')
	call keya('out',OutNam,' ')
	call keyr('tol',Tol,0.01)
	call keyi('niters',maxniter,30)
	call keyr('q',Q,0.)
	call keyr('rmsfac',rmsfac,1.)
	call keyr('flux',TFlux,0.)
	call GetOpt(verbose,doflux,entropy)
	call keyfin
c
c  Check everything makes sense.
c
	if(npnt.eq.0)call bug('f','Input maps must be given')
	if(n.ne.npnt)call bug('f','Number of maps and beams differ')
	if(rmsfac.le.0.0)call bug('f','RMSFAC is not positive')
	if(rmsfac.lt.0.9)call bug('w','RMSFAC seems small')
	if(maxniter.lt.0)call bug('f','NITERS was given a bad value')
	if(OutNam.eq.' ')
     *	  call bug('f','An output file name must be given')
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
	call output('Loading beams and dirty maps ...')
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
	call xyclose(lBeam)
c
c  Load the beams and images.
c
	call BmInit(BeamNam,npnt,icentre,jcentre,Cnvl)
	call ImInit(MapNam,npnt,pMap,nx,ny,mnx,mny,x0,y0,sigma,pbObj)
c
c  Open the model and default image if needed.
c
	if(ModNam.ne.' ')then
	  call xyopen(lMod,ModNam,'old',2,nMod)
	  if(nMod(1).ne.mnx.or.nMod(2).ne.mny)
     *	    call bug('f','Bad model image size')
	endif
c
	if(DefNam.ne.' ')then
	  call xyopen(lDef,DefNam,'old',2,nDef)
	  if(nDef(1).ne.mnx.or.nDef(2).ne.mny)
     *	    call bug('f','Bad default Image size')
	endif
c
c  Open the output.
c
	nOut(1) = mnx
	nOut(2) = mny
	nOut(3) = 1
	call xyopen(lMap,MapNam,'old',2,nMap)
	call rdhdi(lMap,'naxis',naxis,0)
	naxis = min(naxis,MAXNAX)
	do i=4,naxis
	  nOut(i) = 1
	enddo
	call xyopen(lOut,OutNam,'new',naxis,nOut)
c
c  Initial values for alpha and beta.
c
	Alpha = 0
	Beta = 0
c
c  Loop.
c
	call memAlloc(pWt,mnx*mny,'r')
	call memAlloc(pEst,mnx*mny,'r')
	call memAlloc(pDef,mnx*mny,'r')
	call memAlloc(pRes,npnt*nx*ny,'r')
	call memAlloc(pNewEst,mnx*mny,'r')
	call memAlloc(pNewRes,npnt*nx*ny,'r')
	call memAlloc(pDChi,mnx*mny,'r')
	call memAlloc(pNewDChi,mnx*mny,'r')
	call memAlloc(pTmp,nx*ny,'r')
c
c  Get the sum A**2/sigma**2
c
	call GetWt(npnt,x0,y0,nx,ny,sigma,pbObj,memr(pWt),mnx,mny)
c
c  Get the Default map and Clip level.
c
	  if(TFlux.eq.0.and.positive)then
	    TFlux = 0
	    do i=1,npnt
	      TFlux = TFlux + sigma(i)
	    enddo
	    TFlux = RmsFac*TFlux*mnx*mny/(Q*npnt)
	  endif
c
	  if(DefNam.eq.' ')then
	    ClipLev = 0.01 * TFlux/(mnx*mny)
	    call Assign(TFlux/(mnx*mny),memr(pDef),mnx*mny)
	  else
	    call PlLoad(lDef,memr(pDef),mnx,mny)
	    i = Ismax(mnx*mny,memr(pDef),1)
	    ClipLev = 0.01 * abs(memr(pDef+i-1))
	    if(positive) call ClipIt(0.1*ClipLev,memr(pDef),mnx*mny)
	  endif
c
c  Get the Estimate and Residual. Also get information about the
c  current situation.
c
	  if(ModNam.eq.' ')then
	    call Copy(mnx*mny,memr(pDef),memr(pEst))
	  else
	    call PlLoad(lMod,memr(pEst),mnx,mny)
	    if(positive) call ClipIt(ClipLev,memr(pEst),mnx*mny)
	  endif
c
	  call Diff(memr(pEst),memr(pMap),memr(pRes),memr(pDChi),
     *	    memr(pTmp),npnt,nx,ny,mnx,mny,pbObj,Cnvl,sigma,x0,y0)
c
c  Get all the information.
c
	  call GetInfo(mnx*mny,
     *	      memr(pEst),memr(pDChi),measure,memr(pDef),memr(pWt),
     *	      Alpha,Beta,Q,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux)
	  call GetRms(npnt,nx*ny,memr(pRes),sigma,rms)
c------------------------------------------------------------------------
c  Now start to iterate at long last.
c
	call output('Start iterating')
	OStLen1 = 0
	OStLen2 = 0
	Converge = .false.
	Niter = 0
	dowhile(.not.converge.and.Niter.lt.MaxNiter)
	  Niter = Niter + 1
c
c  Update Alpha and Beta.
c
	  De = npnt*nx*ny*(Rms*Rms - RmsFac*RmsFac)
	  Df = Flux - TFlux
	  call NewAlpB(Alpha,Beta,De,Df,doflux,GradEE,GradEF,
     *		GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
c  Calculate the next step to take.
c
	  call CalStep(mnx*mny,
     *	      memr(pEst),memr(pDChi),memr(pNewEst),memr(pWt),memr(pDef),
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
c  Determine the correct Clip Level (to prevent the estimate going
c  negative, if this is not allowed).
c
	  if(positive)ClipLev = min(ClipLev,max(0.1*Immin,1e-6*Immax))
c
c  Take the plunge.
c
	  call TakeStep(mnx*mny,memr(pEst),memr(pNewEst),
     *					StLen1,ClipLev,StLim)
c
c  Convolve the estimate with the beam and subtract the map.
c
	  call Diff(memr(pNewEst),memr(pMap),memr(pNewRes),
     *	    memr(pNewDChi),
     *	    memr(pTmp),npnt,nx,ny,mnx,mny,pbObj,Cnvl,sigma,x0,y0)
c
c  Work out what was really the best step length.
c
	  call ChekStep(mnx*mny,memr(pEst),memr(pNewEst),memr(pNewDChi),
     *		memr(pDef),measure,Alpha,Beta,Q,J1)
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
	    call IntStep(mnx*mny,memr(pEst),memr(pNewEst),StLen2)
	    call IntStep(mnx*mny,memr(pDChi),memr(pNewDChi),StLen2)
	    call IntStep(npnt*nx*ny,memr(pRes),memr(pNewRes),StLen2)
	  else
	    StLen2 = 1
	    call Swap(pEst,pNewEst)
	    call Swap(pDChi,pNewDChi)
	    call Swap(pRes,pNewRes)
	  endif
c
c  Calculate a new estimate for Q using a magic formula.
c
	  if(abs(StLen1-1.).lt.0.05)
     *	    Q = Q * sqrt((1./max(0.5,min(2.,StLen1*StLen2))+3.)/4.)
c
c  Get all the information.
c
	    call GetInfo(mnx*mny,
     *	      memr(pEst),memr(pDChi),measure,memr(pDef),memr(pWt),
     *	      Alpha,Beta,Q,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux)
	    call GetRms(npnt,nx*ny,memr(pRes),sigma,rms)
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
     *		' NormGrd =',0pf5.3)
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
	      call output('MOSTESS seems to have converged')
	    else
	      call output('Failed to converge in NITERS iterations')
	    endif
c
c  Write out this plane.
c
	  Sigt = Sigma(1)
	  do i=2,npnt
	    Sigt = max(Sigt,Sigma(i))
	  enddo
	  call PlTaper(Sigt,memr(pWt),memr(pEst),mnx,mny)
	  call PlSave(lOut,memr(pEst),mnx,mny)
c
c  Construct a header for the output file, and give some history
c  information.
c
	offset(1) = x0(1)
	offset(2) = y0(1)
	offset(3) = 0
	call Header(lMap,lOut,offset,version,niter)
c
c  Close up the files. Ready to go home.
c
	call xyclose(lMap)
	if(ModNam.ne.' ')call xyclose(lMod)
	call xyclose(lOut)
c
c  Release any allocated memory.
c
	call memFree(pMap,npnt*nx*ny,'r')
	call memFree(pWt,mnx*mny,'r')
	call memFree(pEst,mnx*mny,'r')
	call memFree(pDef,mnx*mny,'r')
	call memFree(pRes,npnt*nx*ny,'r')
	call memFree(pNewEst,mnx*mny,'r')
	call memFree(pNewRes,npnt*nx*ny,'r')
	call memFree(pDChi,mnx*mny,'r')
	call memFree(pNewDChi,mnx*mny,'r')
	call memFree(pTmp,nx*ny,'r')
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
c***********************************************************************
	subroutine Assign(def,Default,nPoint)
c
	implicit none
	integer nPoint
	real def,Default(nPoint)
c
c  Set up the default image.
c
c  Input:
c    def
c    nPoint
c  Output:
c    Default	The default image.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Default(i) = def
	enddo
	end
c***********************************************************************
	subroutine ClipIt(clip,Default,nPoint)
c
	implicit none
	integer nPoint
	real clip,Default(nPoint)
c
c  Set up the minimum of the default image.
c
c  Input:
c    clip
c    nPoint
c  Input/Output:
c    Default	The default image.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Default(i) = max(clip,Default(i))
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
	subroutine CalStep(nPoint,Est,DChi,Step,Wt,Def,
     *		measure,Alpha,Beta,Q,J0)
c
	implicit none
	integer nPoint,measure
	real Alpha,Beta,Q,J0
	real Def(nPoint),Est(nPoint),DChi(nPoint)
	real Step(nPoint),Wt(nPoint)
c
c  Calculate the step to take next.
c
c  Inputs:
c    nPoint	Number of points.
c    Est	Current estimate of the MEM solution.
c    DChi	Derivative of chi**2.
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
	    GradJ = dH(l) - 2.*Q*Alpha*DChi(n+l) - Beta
	    Stepd = Diag*GradJ
	    J0 = J0 + GradJ*Stepd
	    Step(n+l) = Stepd
	  enddo
	  n = n + ltot
	enddo
c
	end
c************************************************************************
	subroutine TakeStep(nPoint,Est,NewEst,StLen,Clip,StLim)
c
	implicit none
	integer nPoint
	real Est(nPoint),NewEst(nPoint)
	real StLen,Clip,StLim
c
c  Take the final step!
c
c------------------------------------------------------------------------
	integer i
	real Stepd
c
	if(Clip.gt.0)then
	  do i=1,nPoint
	    Stepd = StLen*max(NewEst(i),(Clip-Est(i))/StLim)
	    NewEst(i) = Est(i) + Stepd
	  enddo
	else
	  do i=1,nPoint
	    NewEst(i) = Est(i) + StLen*NewEst(i)
	  enddo
	endif
	end
c************************************************************************
	subroutine ChekStep(nPoint,OldEst,Est,DChi,Def,
     *			measure,Alpha,Beta,Q,J0)
c
	implicit none
	integer nPoint,Measure
	real OldEst(nPoint),Est(nPoint),DChi(nPoint),Def(nPoint)
	real Alpha,Beta,Q,J0
c
c  Determine some things about this place we are thinking of moving
c  to. Is it a good neighbourhood? Will my kids be safe here?
c
c  Inputs:
c    nPoint	Size of the region being deconvolved.
c    Alpha,Beta	Lagrangian multipliers.
c    Def	Default image.
c    Q		Pixels/beam.
c    DChi	Derivative of chi**2.
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
	    GradJ = dH(l) - 2.*Alpha*Q*DChi(n+l) - Beta
	    Step = Est(n+l) - OldEst(n+l)
	    J0 = J0 + GradJ*Step
	  enddo
	  n = n + ltot
	enddo
c
	end
c************************************************************************
	subroutine GetInfo(nPoint,Est,DChi,Measure,Def,Wt,Alpha,Beta,Q,
     *	  GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ,
     *    GradHH,GradJJ,Grad11,Immax,Immin,Flux)
c
	implicit none
	integer nPoint
	real DChi(nPoint),Est(nPoint),Wt(nPoint),Def(nPoint)
	integer Measure
	real Alpha,Beta,Q
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux
c
c  Get information on the current state of play.
c
c  Inputs:
c    nPoint	Number of points in the input.
c    DChi,Est	The derivative of chi**2 and Estimate respectively.
c    measure	Determines the entropy measure used.
c    Def	The default image.
c    Wt		Sum A**2/Sigma**2
c    Alpha
c    Beta
c    Q
c
c  Outputs:
c    GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
c    GradHH,GradJJ,NomGrd,Immax,Immin,Flux
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
	Flux   = 0.
	Immin = Est(1)
	Immax = Immin
c
	n = 0
	do while(n.lt.nPoint)
	  ltot = min(Run,nPoint-n)
	  call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
	  do l=1,ltot
	    GradE = 2. * Q * DChi(n+l)
	    GradH = dH(l)
	    Diag = 1./( 2.*Alpha*Q*Q*Wt(n+l) - d2H(l) )
	    GradEE = GradEE + GradE*Diag*GradE
	    GradEF = GradEF + GradE*Diag
	    GradEH = GradEH + GradE*Diag*GradH
	    GradFF = GradFF +       Diag
	    GradFH = GradFH +       Diag*GradH
	    GradHH = GradHH + GradH*Diag*Gradh
	    Flux = Flux + Est(n+l)
	    Immin = min(Immin,Est(n+l))
	    Immax = max(Immax,Est(n+l))
	  enddo
	  n = n + ltot
	enddo
c
c  Finish up various variables.
c
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
	subroutine Diff(Est,Map,Res,DChi,Tmp,npnt,nx,ny,mnx,mny,
     *	  pbObj,Cnvl,sigma,x0,y0)
c
	implicit none
	integer npnt,nx,ny,mnx,mny
	integer pbObj(npnt),Cnvl(npnt),x0(npnt),y0(npnt)
	real sigma(npnt)
	real Est(mnx,mny),Res(nx,ny,npnt),Map(nx,ny,npnt),Tmp(nx,ny)
	real DChi(mnx,mny)
c
c  Input:
c    Est
c    Map
c    pbObj
c    Cnvl
c    x0,y0
c    nx,ny
c    mnx,mny
c    npnt
c    sigma
c  Output:
c    Res
c    DChi
c  Scratch:
c    Tmp
c------------------------------------------------------------------------
	integer i,j,xoff,yoff,pnt
	real fac
c
c  Externals.
c
	real pbGet
c
c  Set the derivative of chi**2 to 0.
c
	do j=1,mny
	  do i=1,mnx
	    DChi(i,j) = 0
	  enddo
	enddo
c
c  Loop through the pointings.
c    Multiply by the primary beam. Convolve. Accumulate the residual.
c
	do pnt=1,npnt
	  xoff = x0(pnt)
	  yoff = y0(pnt)
	  do j=1,ny
	    do i=1,nx
	      Tmp(i,j) = pbGet(pbObj(pnt),real(i),real(j))
	      Res(i,j,pnt) = Tmp(i,j)*Est(i+xoff,j+yoff)
	    enddo
	  enddo
c
c  Convolve with the dirty beam.
c
	  call CnvlA(Cnvl(pnt),Res(1,1,pnt),nx,ny,Res(1,1,pnt),'c')
c
c  Accumulate the derivative of chi**2.
c
	  fac = 1/(sigma(pnt)*sigma(pnt))
	  do j=1,ny
	    do i=1,nx
	      Res(i,j,pnt) = Res(i,j,pnt) - Map(i,j,pnt)
	      DChi(i+xoff,j+yoff) = DChi(i+xoff,j+yoff) + 
     *		fac * Tmp(i,j) * Res(i,j,pnt)
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine Header(lMap,lOut,offset,version,niter)
c
	integer lMap,lOut
	character version*(*)
	integer niter,offset(3)
c
c  Write a header for the output file.
c
c  Input:
c    version	Program version ID.
c    lMap	The handle of the input map.
c    lOut	The handle of the output estimate.
c    niter	The maximum number of iterations performed.
c    offset	Offsets between the input map pixel units and the
c		mosaiced output pixel units.
c------------------------------------------------------------------------
	include 'maxnax.h'
	integer i
	real crpix
	character line*72,num*2
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
	call wrhdi(lOut,'niters',Niter)
c
	do i=1,MAXNAX
	  num = itoaf(i)
	  if(i.le.3)then
	    call rdhdr(lMap,'crpix'//num,crpix,1.)
	    crpix = crpix + offset(i)
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
c
	call hisopen(lOut,'append')
	line = 'MOSTESS: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'MOSTESS')
c
	call hiswrite(lOut,'MOSTESS: Total Iterations = '//itoaf(Niter))
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
c************************************************************************
	subroutine BmInit(BeamNam,npnt,ic,jc,Cnvl)
c
	implicit none
	integer npnt,ic,jc
	character BeamNam(npnt)*(*)
	integer Cnvl(npnt)
c
c  Create all the convolvers.
c------------------------------------------------------------------------
	integer i,lBeam,nsize(2)
c
	do i=1,npnt
	  call xyopen(lBeam,BeamNam(i),'old',2,nsize)
	  call cnvlIniF(Cnvl(i),lBeam,nsize(1),nsize(2),ic,jc,0.,'s')
	  call xyclose(lBeam)
	enddo
c
	end
c************************************************************************
	subroutine ImInit(MapNam,npnt,pMap,nx,ny,mnx,mny,x0,y0,
     *							rms,pbObj)
c
	implicit none
	integer npnt,nx,ny,mnx,mny,x0(npnt),y0(npnt),pbObj(npnt)
	character MapNam(npnt)*(*)
	integer pMap
	real rms(npnt)
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer i,nsize(2),lMap,xmin,xmax,ymin,ymax
c
c  Load the first one.
c
	call xyopen(lMap,MapNam(1),'old',2,nsize)
	nx = nsize(1)
	ny = nsize(2)
	call memAlloc(pMap,nx*ny*npnt,'r')
c
c  Process the first image.
c
	call ImProc(lMap,memr(pMap),nx,ny,pbObj(1),rms(1),x0(1),y0(1))
	xmin = x0(1)
	xmax = xmin + nx - 1
	ymin = y0(1)
	ymax = ymin + ny - 1
	call xyclose(lMap)
c
	do i=2,npnt
	  call xyopen(lMap,MapNam(i),'old',2,nsize)
	  if(nsize(1).ne.nx.or.nsize(2).ne.ny)
     *	    call bug('f','Inconsistency in map sizes')
	  call ImProc(lMap,memr(pMap+(i-1)*nx*ny),nx,ny,
     *			pbObj(i),rms(i),x0(i),y0(i))
	  xmin = min(xmin,x0(i))
	  xmax = max(xmax,x0(i) + nx - 1)
	  ymin = min(ymin,y0(i))
	  ymax = max(ymax,y0(i) + ny - 1)
	  call xyclose(lMap)
	enddo
c
	mnx = xmax - xmin + 1
	mny = ymax - ymin + 1
c
	do i=1,npnt
	  x0(i) = x0(i) - xmin 
	  y0(i) = y0(i) - ymin
	enddo
c
	end
c************************************************************************
	subroutine ImProc(lMap,Data,nx,ny,pbObj,rms,xc,yc)
c
	implicit none
	integer lMap,nx,ny,pbObj,xc,yc
	real Data(nx,ny),rms
c
c------------------------------------------------------------------------
	integer j,n
	double precision radec(3)
	character pbtype*32
c
	call mosLoad(lMap,n)
	if(n.ne.1)call bug('f','Can only handle single pointing files')
	call mosGet(1,radec(1),radec(2),rms,pbtype)
	radec(3) = 1
	call rdhdi(lMap,'crpix1',xc,0)
	call rdhdi(lMap,'crpix2',yc,0)
	xc = 1 - xc
	yc = 1 - yc
	call coInit(lMap)
	call pbInitc(pbObj,pbtype,lMap,'aw/aw/ap',radec)
	call coFin(lMap)
c
	do j=1,ny
	  call xyread(lMap,j,Data(1,j))
	enddo
c
	end
c************************************************************************
	subroutine GetWt(npnt,x0,y0,nx,ny,sigma,pbObj,Wts,mnx,mny)
c
	implicit none
	integer mnx,mny,nx,ny,npnt
	integer x0(npnt),y0(npnt),pbObj(npnt)
	real Wts(mnx,mny),sigma(npnt)
c
c  Determine the sum of the weights.
c
c------------------------------------------------------------------------
	integer i,j,pnt,xoff,yoff
	real fac,pb
c
c  Externals.
c
	real pbGet
c
	do j=1,mny
	  do i=1,mnx
	    Wts(i,j) = 0
	  enddo
	enddo
c
	do pnt=1,npnt
	  xoff = x0(pnt)
	  yoff = y0(pnt)
	  fac = sigma(pnt)
	  fac = 1/(fac*fac)
	  do j=1,ny
	    do i=1,nx
	      pb = pbGet(pbObj,real(i),real(j))
	      Wts(i+xoff,j+yoff) = Wts(i+xoff,j+yoff) + fac*pb*pb
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GetRms(npnt,npix,Res,sigma,Rms)
c
	implicit none
	integer npnt,npix
	real Res(npix,npnt),sigma(npnt),Rms
c
c  Input:
c    Res
c    npix
c    npnt
c    sigma
c  Output:
c    rms
c------------------------------------------------------------------------
	integer i,pnt
	real fac
c
	Rms = 0
	do pnt=1,npnt
	  fac = 1/(sigma(pnt)*sigma(pnt))
	  do i=1,npix
	    Rms = Rms + fac*Res(i,pnt)*Res(i,pnt)
	  enddo
	enddo
c
	Rms = sqrt(Rms/(npnt*npix))
c
	end
c************************************************************************
	subroutine PlLoad(lu,Data,nx,ny)
c
	implicit none
	integer lu,nx,ny
	real Data(nx,ny)
c
c  Load in a plane.
c------------------------------------------------------------------------
	integer j
c
	do j=1,ny
	  call xyread(lu,j,Data(1,j))
	enddo
c
	end
c************************************************************************
	subroutine PlTaper(Sigt,Wt,Est,mnx,mny)
c
	implicit none
	integer mnx,mny
	real Sigt,Wt(mnx,mny),Est(mnx,mny)
c
c  Taper the edge of the image.
c------------------------------------------------------------------------
	integer i,j
	real Sigt2
c
	Sigt2 = Sigt*Sigt
	do j=1,mny
	  do i=1,mnx
	    if(Sigt2*Wt(i,j).lt.1)
     *		Est(i,j) = Est(i,j) * sqrt(Sigt2*Wt(i,j))
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine PlSave(lu,Data,nx,ny)
c
	implicit none
	integer lu,nx,ny
	real Data(nx,ny)
c
c  Write out a plane.
c------------------------------------------------------------------------
	integer j
c
	do j=1,ny
	  call xywrite(lu,j,Data(1,j))
	enddo
c
	end

