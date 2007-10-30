c************************************************************************
	program maxen
	implicit none
c
c= maxen - Maximum Entropy deconvolution
c& rjs mchw
c: deconvolution
c+
c	MAXEN is a MIRIAD task which performs a maximum entropy deconvolution
c	algorithm on a cube.
c@ map
c	The input dirty map, which should have units of Jy/beam. No default.
c@ beam
c	The input dirty beam. No default
c@ model
c	An initial model of the deconvolved image. This could be the output
c	from a previous run of MAXEN or any other deconvolution
c	task. It must have flux units of Jy/pixel. The default is a flat
c	estimate, with the correct flux.
c@ default
c	The default image. This could be the output
c	from a previous run of MAXEN or any other deconvolution
c	task. It must have flux units of Jy/pixel. The default is a flat
c	estimate, with the correct flux.
c@ out
c	The name of the output map. The units of the output will be Jy/pixel.
c	It can be input to RESTOR, MAXEN (as a model, to continue the
c	deconvolution process),	or to SELFCAL (for self-calibrating
c	visibility data).
c@ niters
c	The maximum number of iterations. The default is 20.
c@ region
c	This specifies the region to be deconvolved. See the Users Manual
c	for instructions on how to specify this. The default is the inner
c	quarter of all planes.
c@ measure
c	The entropy measure to be used, either "gull" (-p*log(p/e)) or
c	"cornwell" (-log(cosh(p)) -- also called the maximum emptyness
c	criteria).
c@ tol
c	Tolerance of solution. There is no need to change this from the
c	default of 0.01.
c@ q	
c	An estimate of the number of points per beam. MAXEN can usually
c	come up with a pretty good, image dependent estimate.
c@ rms
c	The rms noise, in Jy/beam, in the dirty map. No default. The
c	convergence and behavior of MAXEN depends strongly on the value
c	of this parameter.
c@ flux
c	The flux of the source. If FLUX is given as a positive
c	number, then the resultant image is constrained to have this
c	flux. If FLUX is negative, then abs(FLUX) is used as an estimate
c	of the source flux only and the actual flux is allowed to vary.
c	If FLUX=0, then MAXEN makes a guess at the source flux. Default is
c	FLUX=0, though you should give MAXEN more information if at all
c	possible. On the other hand, a bad value here can cause MAXEN to
c	blow up.
c@ options
c	Task enrichment parameters. Several can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  quiet      Do not give any messages during iterations. The default
c	             is to give a one line message at each iteration.
c	  verbose    Give lots of messages during the iterations. The default
c	             is to give a one line message at each iteration.
c	  asym       The beam is asymmetric. By default MAXEN assumes the
c	             beam has 180 degree rotation symmetry, which is the
c	             norm for beams in radio-astronomy.
c	  pad        Double the beam size by padding it with zeros. This
c	             will give better stability if you are daring enough
c	             to deconvolve more than the inner quarter of the dirty
c	             image.
c--
c  History:
c    rjs   nov88 - Original version.
c    rjs  9jun89 - Included Diff in source. Changed call sequences to
c		   GetPlane and PutPlane. Improved history. Removed sign
c		   change fudge.
c    rjs 13mar90 - Copied linetype header values to the output file.
c		   Added version thingo.
c    rjs 22mar90 - Cosmetic changes to the format statements.
c    rjs 12apr90 - Fixed bug which caused the old history to be overwritten.
c    rjs 30apr90 - Changed call sequence to BoxInput.
c   mchw  7may90 - Fixed call sequence to BoxInput.
c   mchw 25oct90 - Added default image and message level.
c		   Fixed bug in output map size in PutPlane.
c   mchw 14nov90 - Set minimum default level. Added pbfwhm to header.
c   mchw 03jan91 - Increased Maxboxes to handle more complex region.
c   rjs  08apr91 - Used Mem routines, to avoid the need to declare large
c		   static arrays.
c   rjs  19apr91 - Fixed bug in EntFunc in determining d2H (apparently
c		   introduced on 25oct90). Tidied up messages somewhat.
c   rjs  19apr91 - More minor optimisations.
c   mjs  08aug91 - correct BeamChar typo:  jmax=max(etc) to jmax=min(etc)
c   rjs  14oct91 - Bug in dealing with non-square beams.
c   rjs  24may92 - Fiddles to steps in alpha and stlen to make convergence
c		   more robust (but sometimes slower).
c   nebk 25nov92 - Copy btype to output
c   mchw 07jan93 - Copy across crpix3.
c   mjs  17feb93 - minor doc mod only (RESTORE -> RESTOR).
c   rjs   5mar93 - History standardisation. Use cnvl routines. Add extra
c	           options.
c   rjs  27nov94 - Significant changes and bugfixes to make it more robust.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Maxen: version 1.0 05-Mar-93')
	include 'maxdim.h'
	integer MaxRun,MaxBoxes
	parameter(MaxRun=3*maxdim,MaxBoxes=1024)
c
	integer quiet,normal,verbose
	parameter(quiet=0,normal=1,verbose=2)
c
	integer gull,cornwell
	parameter(gull=1,cornwell=2)
c
	character MapNam*32,BeamNam*32,ModelNam*32,OutNam*32,DefNam*32
	character entropy*8,messlev*8,flags*8,line*72
	integer lBeam,lMap,lModel,lOut,lDef
	integer nMap(3),nModel(3),nOut(3),nBeam(2),nDef(3)
	integer xmin,ymin,xmax,ymax,n1,n2,nx,ny,MaxMap
	integer imin,imax,jmin,jmax,kmin,kmax,blc(3),trc(3),naxis,k
	integer icentre,jcentre
	integer maxniter,niter
	integer measure,message
	real Tol,TRms,TFlux,Qest,De,Df,OStLen2,OStLen1
	real J0,J1,StLen1,StLen2,StLim,Alpha,Beta,Q
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms,ClipLev
	logical converge,positive,asym,pad,doflux
	integer Run(3,MaxRun),nRun,Boxes(maxBoxes),nPoint
c
	integer pBem,pMap,pEst,pDef,pRes,pNewEst,pNewRes
	real Data(MaxBuf)
	common Data
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
	call keya('map',MapNam,' ')
	call keya('beam',BeamNam,' ')
	call keya('model',ModelNam,' ')
	call keya('default',DefNam,' ')
	call keya('out',OutNam,' ')
	call keyr('tol',Tol,0.01)
	call keyi('niters',maxniter,20)
	call keyr('q',Q,0.)
	call keyr('rms',TRms,0.)
	call keyr('flux',TFlux,0.)
	call BoxInput('region',MapNam,Boxes,MaxBoxes)
	call GetMeas(entropy)
	call GetOpt(messlev,asym,pad)
	call keyfin
c
c  Check everything makes sense.
c
	doflux = TFlux.gt.0
	TFlux = abs(TFlux)
	if(Trms.le.0.)call bug('f','RMS is not positive')
	if(maxniter.lt.0)call bug('f','NITERS has bad value')
	if(MapNam.eq.' '.or.BeamNam.eq.' '.or.OutNam.eq.' ')
     *	  call bug('f','A file name was missing from the parameters')
	if(Tol.le.0.)
     *	  call bug('f','The TOL parameter must be positive valued')
c
	if(messlev.eq.'quiet')then
	  message = quiet
	else if(messlev.eq.'normal')then
	  message = normal
	else if(messlev.eq.'verbose')then
	  message = verbose
	endif
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
	call xyopen(lBeam,BeamNam,'old',2,nBeam)
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
	flags = ' '
	if(.not.asym) flags(1:1) = 's'
	if(pad)       flags(2:2) = 'e'
	call CnvlIniF(pBem,lBeam,n1,n2,icentre,jcentre,0.,flags)
c
c  Open the input map.
c
	call xyopen(lMap,MapNam,'old',3,nMap)
	if(max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
	call rdhdi(lMap,'naxis',naxis,3)
	naxis = min(naxis,3)
	call BoxMask(lMap,boxes,maxboxes)
	call BoxSet(Boxes,3,nMap,'q')
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
	if(nOut(1).gt.n1.or.nOut(2).gt.n2)
     *	  call bug('f','Region of map to deconvolve is too big')
	if(2*nOut(1)-1.gt.n1.or.2*nOut(2)-1.gt.n2)
     *	  call bug('w','Size of region of map to deconvolve is unsafe')
c
c  Allocate arrays to hold everything.
c
	MaxMap = nOut(1)*nOut(2)
	call MemAlloc(pMap,MaxMap,'r')
	call MemAlloc(pEst,MaxMap,'r')
	call MemAlloc(pDef,MaxMap,'r')
	call MemAlloc(pRes,MaxMap,'r')
	call MemAlloc(pNewEst,MaxMap,'r')
	call MemAlloc(pNewRes,MaxMap,'r')
c
c  Open the model if needed, and check that is is the same size as the
c  output.
c
	if(ModelNam.ne.' ')then
	  call xyopen(lModel,ModelNam,'old',3,nModel)
	  if(nModel(1).ne.nOut(1).or.nModel(2).ne.nOut(2)
     *	    .or.nModel(3).ne.nOut(3)) call bug('f','Model size')
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
	  if(nDef(1).ne.nOut(1).or.nDef(2).ne.nOut(2)
     *	    .or.nDef(3).ne.nOut(3)) call bug('f','Default Image size')
	endif
c
c  Open up the output.
c
	call xyopen(lOut,OutNam,'new',naxis,nOut)
c
c  Loop.
c
	do k=kmin,kmax
	  if(kmin.ne.kmax)call output('Plane: '//itoaf(k))
c
	  call BoxRuns(1,k,'r',boxes,Run,MaxRun,nRun,
     *					xmin,xmax,ymin,ymax)
	  nx = xmax - xmin + 1
	  ny = ymax - ymin + 1
c
c  Get the Map.
c
	  call xysetpl(lMap,1,k)
	  call GetPlane(lMap,Run,nRun,xmin-1,ymin-1,nMap(1),nMap(2),
     *				Data(pMap),MaxMap,nPoint)
c
c  Get the Default map and Clip level.
c
	  if(TFlux.eq.0)TFlux = TRms*nPoint/Q
c
	  if(DefNam.eq.' ')then
	    ClipLev = 0.01 * TFlux/nPoint
	    call Assign(TFlux/nPoint,Data(pDef),nPoint)
	  else
	    call xysetpl(lDef,1,k-kmin+1)
	    call GetPlane(lDef,Run,nRun,xmin-imin,ymin-jmin,
     *			nDef(1),nDef(2),Data(pDef),MaxMap,nPoint)
	    Imax = Ismax(npoint,Data(pDef),1)
	    ClipLev = 0.01 * abs(Data(pDef+Imax-1))
	    call ClipIt(0.1*ClipLev,Data(pDef),nPoint)
	  endif
c
c  Get the Estimate and Residual. Also get information about the
c  current situation.
c
	  if(ModelNam.eq.' ')then
	    call Copy(nPoint,Data(pDef),Data(pEst))
	  else
	    call xysetpl(lModel,1,k-kmin+1)
	    call GetPlane(lModel,Run,nRun,xmin-imin,ymin-jmin,
     *			nModel(1),nModel(2),Data(pEst),MaxMap,nPoint)
	    if(ClipLev.gt.0) call ClipIt(ClipLev,Data(pEst),nPoint)
	  endif
c
	  call Diff(pBem,Data(pEst),Data(pMap),Data(pRes),
     *	    nPoint,nx,ny,Run,nRun)
c
	  call GetInfo(nPoint,Data(pEst),Data(pRes),measure,Data(pDef),
     *	    Alpha,Beta,Q,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	    GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)
c
c  Put the user to sleep with lots of meaningful messages.
c
	  if(message.eq.verbose)then
	    call output('Initialising ...')
	    write(line,10)Alpha,Beta,Q
  10	    format('  Alpha =',1pe12.3,' Beta  =',1pe12.3,
     *		' Q       =',1pe12.3)
	    call output(line)
	    write(line,11)Immin,Immax
  11	    format('  Immin =',1pe12.3,' Immax =',1pe12.3)
	    call output(line)
	    write(line,12)Rms,Flux,GradJJ/Grad11
  12	    format('  Rms   =',1pe12.3,' Flux  =',1pe12.3,
     *		' NormGrd =',1pe12.3)
	    call output(line)
	  endif
c
c------------------------------------------------------------------------
c  Now start to iterate at long last.
c
	OStLen2 = 0
	OStLen1 = 0
	Converge = .false.
	Niter = 0
	dowhile(.not.converge.and.Niter.lt.MaxNiter)
	  Niter = Niter + 1
c
c  Update Alpha and Beta.
c
	  De = nPoint*(Rms*Rms - TRms*TRms)
	  Df = Flux - TFlux
	  call NewAlpB(Alpha,Beta,De,Df,doflux,GradEE,GradEF,
     *		GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
c  Calculate the next step to take.
c
	  call CalStep(nPoint,Data(pEst),Data(pRes),Data(pNewEst),
     *		measure,Data(pDef),Alpha,Beta,Q,J0)
c
c  Determine the max step length, and the initial step length.
c  Also correct the value of GradJ.Step for the shorter length
c  step.
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
	  call TakeStep(nPoint,Data(pEst),Data(pNewEst),
     *					StLen1,ClipLev,StLim)
c
c  Convolve the estimate with the beam and subtract the map.
c
	  call Diff(pBem,Data(pNewEst),Data(pMap),Data(pNewRes),
     *	    nPoint,nx,ny,Run,nRun)
c
c  Now work out what was really the best step length. Make sure that
c  its no longer than the step length we previously determined was the
c  max length overall.
c
	  call ChekStep(nPoint,Data(pEst),Data(pNewEst),Data(pNewRes),
     *			measure,Data(pDef),Alpha,Beta,Q,J1)
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
c  be near 1 on the first few iterations, where StLim, rather than
c  anything else, determines the step length. If StLen2 is near 1,
c  just swap the pointers around rather than do any real work.
c
	  if(abs(StLen2-1.).gt.0.05)then
	    call IntStep(nPoint,Data(pEst),Data(pNewEst),StLen2)
	    call IntStep(nPoint,Data(pRes),Data(pNewRes),StLen2)
	  else
	    StLen2 = 1
	    call Swap(pEst,pNewEst)
	    call Swap(pRes,pNewRes)
	  endif
c
c  Calculate a new estimate for Q using the magic formula which seems
c  to work. Only recalculate when we did not clip back the step length
c  excessively. That is recalculate if StLen1 is close to 1.
c
	if(abs(StLen1-1.).lt.0.05)
     *	  Q = Q * sqrt((1./max(0.5,min(2.,StLen1*StLen2))+3.)/4.)
c
c  Get new info on the current state of play.
c
	  call GetInfo(nPoint,Data(pEst),Data(pRes),measure,Data(pDef),
     *	    Alpha,Beta,Q,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *	    GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)
c
c  Reawaken the user with more crap to let him/her ponder over
c  what could possibly be going wrong. Give him/her as much as
c  possible to ponder over.
c
	  if(message.eq.verbose)then
	    call output('Iteration '//itoaf(niter))
	    write(line,20)Alpha,Beta,Q
	    call output(line)
	    write(line,21)Immin,Immax
	    call output(line)
	    write(line,22)Rms,Flux,GradJJ/Grad11
	    call output(line)
	    write(line,23)StLim,StLen1,StLen2
	    call output(line)
	  else if(message.eq.normal)then
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
  24	  format(' Iter =',i3,' Rms =',1pe10.3,' Flux =',1pe10.3,
     *		' NormGrd =',1pe10.3)
c
c  Check for convergence.
c
	 converge = (Rms-TRms       .lt.0.05*TRms		  .and.
     *		    (abs(Flux-TFlux).lt.0.05*TFlux.or..not.doflux).and.
     *		     GradJJ/Grad11  .lt.Tol			  )
	enddo
c------------------------------------------------------------------------
c
c  We have finished processing this plane. More info to the user!
c
	  if(converge)then
	    call output('MAXEN has converged ... finishing up now')
	  else
	    call output('Failed to converge in NITERS iterations')
	  endif
c
c  Write out this plane.
c
	  call xysetpl(lOut,1,k-kmin+1)
	  call PutPlane(lOut,Run,nRun,xmin-imin,ymin-jmin,
     *				nOut(1),nOut(2),Data(pEst),nPoint)
	enddo
c
c  Construct a header for the output file, and give some history
c  information.
c
	call Header(lMap,lOut,blc,trc,version,niter)
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
	subroutine GetOpt(messlev,asym,pad)
c
	implicit none
	character messlev*(*)
	logical asym,pad
c
c  Get extra processing options.
c
c  Output:
c    messlev	Possibles values: 'quiet', 'normal', 'verbose'.
c    asym	Beam is asymmetric.
c    pad	Double beam size.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=4)
	logical present(NOPT)
	character opts(NOPT)*8
	data opts/'quiet   ','verbose ','asym    ','pad     '/
c
	call options('options',opts,present,NOPT)
	messlev = 'normal'
	if(present(1)) messlev = 'quiet'
	if(present(2)) messlev = 'verbose'
	if(present(1).and.present(2))
     *	  call bug('f','Cannot mix quiet and verbose options')
	asym = present(3)
	pad = present(4)
c
	end
c************************************************************************
	subroutine GetMeas(entropy)
c
	implicit none
	character entropy*(*)
c
c  Determine the entropy measure to be used.
c
c  Output:
c    entropy	The entropy measure.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=2)
	integer nout
	character opts(NOPT)*8
	data opts/'gull    ','cornwell'/
c
	call keymatch('measure',NOPT,opts,1,entropy,nout)
	if(nout.eq.0) entropy = opts(1)
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
	subroutine ChekStep(nPoint,OldEst,Est,Res,
     *			measure,Default,Alpha,Beta,Q,J0)
c
	implicit none
	integer nPoint,Measure
	real OldEst(nPoint),Est(nPoint),Res(nPoint)
	real Default(nPoint),Alpha,Beta,Q,J0
c
c  Determine some things about this place we are thinking of moving
c  to. Is it a good neighbourhood? Will my kids be safe here?
c
c  Inputs:
c    nPoint	Size of the region being deconvolved.
c    Alpha,Beta	Lagrangian multipliers.
c    Default	Default image.
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
	  call EntFunc(measure,ltot,Est(n+1),Default(n+1),dH,d2H)
	  do l=1,ltot
	    GradJ = dH(l) - 2.*Alpha*Q*Res(n+l) - Beta
	    Step = Est(n+l) - OldEst(n+l)
	    J0 = J0 + GradJ*Step
	  enddo
	  n = n + ltot
	enddo
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
	real Denom,Dalp,Dbet,l,Alpha1,Alpha2,Beta1,Beta2,b2m4ac
c
c  Check if things are doing poorly. If so, just aim at reducing the
c  gradient.
c
	l = 10*abs(GradJJ/Grad11)
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
	b2m4ac = GradEJ*GradEJ - (GradJJ-0.3*Grad11)*GradEE
        if(b2m4ac.gt.0.and.Alpha.eq.0)then
          b2m4ac = sqrt(b2m4ac)
	  Dalp = max((GradEJ - b2m4ac)/GradEE,
     *		 min((GradEJ + b2m4ac)/GradEE,Dalp))
        endif
c
        b2m4ac = GradFJ*GradFJ - (GradJJ-0.3*Grad11)*GradFF
        if(b2m4ac.gt.0.and.Beta.eq.0)then
          b2m4ac = sqrt(b2m4ac)
	  Dbet = max((GradFJ - b2m4ac)/GradFF,
     *		 min((GradFJ + b2m4ac)/GradFF,Dbet))
        endif
c
	Alpha2 = Alpha+ Dalp
	Beta2  = Beta + Dbet
c
	if(l.ge.1.or.Alpha2.le.0)then
	  Alpha = max(Alpha1,0.)
	else if(l.le.0.or.Alpha1.le.0)then
	  Alpha = max(Alpha2,0.)
	else
	  Alpha = exp(l*log(Alpha1) + (1-l)*log(Alpha2))
	endif
c
	if(l.ge.1.or.Beta2.le.0)then
	  Beta = max(Beta1,0.)
	else if(l.le.0.or.Beta1.le.0)then
	  Beta = max(Beta2,0.)
	else
	  Beta = exp(l*log(Beta1) + (1-l)*log(Beta2))
	endif
c
	end
c************************************************************************
	subroutine CalStep(nPoint,Estimate,Residual,Step,
     *		measure,Default,Alpha,Beta,Q,J0)
c
	implicit none
	integer nPoint,measure
	real Default(nPoint),Alpha,Beta,Q,J0
	real Estimate(nPoint),Residual(nPoint),Step(nPoint)
c
c  Calculate the step to take next.
c
c  Inputs:
c    nPoint	Number of points.
c    Estimate	Current estimate of the MEM solution.
c    Residual	Current residuals.
c    Default	The default image.
c    measure	Determines the entropy measure used.
c
c  Output:
c    Step	The step to take towards a better estimate.
c    J0		The current value for GradJJ.
c
c------------------------------------------------------------------------
	integer run
	parameter(run=1024)
	integer n,l,ltot
	real Temp, Diag, GradJ, Stepd
	real dH(run),d2H(run)
c
	Temp = 2.*Alpha*Q*Q
	J0 = 0
c
	n = 0
	dowhile(n.lt.nPoint)
	  ltot = min(nPoint-n,run)
	  call EntFunc(measure,ltot,Estimate(n+1),Default(n+1),dH,d2H)
	  do l=1,ltot
	    Diag = 1 / (Temp - d2H(l))
	    GradJ = dH(l) - 2.*Q*Alpha*Residual(n+l) - Beta
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
	subroutine GetInfo(nPoint,Est,Res,Measure,Default,Alpha,Beta,Q,
     *	  GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ,
     *    GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)
c
	implicit none
	integer nPoint
	real Res(nPoint),Est(nPoint)
	integer Measure
	real Default(nPoint),Alpha,Beta,Q
	real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
	real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms
c
c  Get information on the current state of play.
c
c  Inputs:
c    nPoint	Number of points in the input.
c    Res,Est	The Residuals and Estimate respectively.
c    measure	Determines the entropy measure used.
c    Default	The default image.
c    Alpha
c    Beta
c    Q
c
c  Outputs:
c    GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
c    GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms
c------------------------------------------------------------------------
	integer Run
	parameter(Run=1024)
	integer n,l,ltot,Imax,Imin
	real Diag,GradE,GradH
	real dH(Run),d2H(Run)
c
c  Externals to find min and max indices.
c
	integer Ismin,Ismax
c
	n = 0
c
	GradEE = 0.
	GradEF = 0.
	GradEH = 0.
	GradFF = 0.
	GradFH = 0.
	GradHH = 0.
	Rms    = 0.
	Flux   = 0.
	do while(n.lt.nPoint)
	  ltot = min(Run,nPoint-n)
	  call EntFunc(measure,ltot,Est(n+1),Default(n+1),dH,d2H)
	  do l=1,ltot
	    GradE = 2. * Q * Res(n+l)
	    GradH = dH(l)
	    Diag = 1./( 2.*Alpha*Q*Q - d2H(l) )
	    GradEE = GradEE + GradE*Diag*GradE
	    GradEF = GradEF + GradE*Diag
	    GradEH = GradEH + GradE*Diag*GradH
	    GradFF = GradFF +       Diag
	    GradFH = GradFH +       Diag*GradH
	    GradHH = GradHH + GradH*Diag*Gradh
	    Flux = Flux + Est(n+l)
	    Rms  = Rms + Res(n+l)**2
	  enddo
	  n = n + ltot
	enddo
c
c  Find the min and max values. Call the library routines to do this,
c  so that we use efficient code!
c
	Imax = Ismax(nPoint,Est,1)
	Imin = Ismin(nPoint,Est,1)
	Immax = Est(Imax)
	Immin = Est(Imin)
c
c  Finish up various variables.
c
	Rms = sqrt(Rms/real(nPoint))
	GradEJ = GradEH - Alpha*GradEE - Beta*GradEF
	GradFJ = GradFH - Alpha*GradEF - Beta*GradFF
	GradJJ = GradHH + Alpha*Alpha*GradEE + Beta*Beta*GradFF
     *		- 2.*Alpha*GradEH - 2.*Beta*GradFH
     *		+ 2.*Alpha*Beta*GradEF
	Grad11 = GradHH + Alpha*Alpha*GradEE + Beta*Beta*GradFF
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
	subroutine Diff(pBem,Estimate,Map,Residual,nPoint,nx,ny,
     *	  Run,nRun)
c
	implicit none
	integer nPoint,nx,ny,nRun,Run(3,nRun),pBem
	real Estimate(nPoint),Map(nPoint),Residual(nPoint)
c
c------------------------------------------------------------------------
	integer i
c
	call CnvlR(pBem,Estimate,nx,ny,Run,nRun,Residual,'c')
c
	do i=1,nPoint
	  Residual(i) = Residual(i) - Map(i)
	enddo
c
	end
c************************************************************************
	subroutine Header(lMap,lOut,blc,trc,version,niter)
c
	integer lMap,lOut
	integer blc(3),trc(3)
	character version*(*)
	integer niter
c
c  Write a header for the output file.
c
c  Input:
c    version	Program version ID.
c    lMap	The handle of the input map.
c    lOut	The handle of the output estimate.
c    blc	Blc of the bounding region.
c    trc	Trc of the bounding region.
c    niter	The maximum number of iterations performed.
c
c------------------------------------------------------------------------
	include 'maxnax.h'
	integer i,lblc,ltrc
	real crpix
	character line*72,txtblc*32,txttrc*32,num*2
	integer nkeys
	parameter(nkeys=14)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'obstime ','epoch   ','history ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','object  ','pbfwhm  ',
     *	  'observer','telescop','restfreq','vobs    ','btype   '/
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
        line = 'MAXEN: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'MAXEN')
c
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	line = 'MAXEN: Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'
	call hiswrite(lOut,line)
c
	call hiswrite(lOut,'MAXEN: Total Iterations = '//itoaf(Niter))
	call hisclose(lOut)
c
	end
