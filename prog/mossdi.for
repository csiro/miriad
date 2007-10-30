c************************************************************************
	program mossdi
	implicit none
c
c= mossdi - Mosaic Steer CLEAN algorithm
c& rjs
c: deconvolution
c+
c	MOSSDI is a MIRIAD task which performs a Steer CLEAN on a mosaiced
c	image or cube.
c@ map
c	The input dirty map, which should have units of Jy/beam. No default.
c@ beam
c	The input dirty beam. No default
c@ model
c	An initial model of the deconvolved image. The default is 0.
c@ out
c	The name of the output map. The units of the output will be Jy/pixel.
c@ gain
c	CLEAN loop gain. The default is 0.1.
c@ niters
c	The maximum number of iterations. The default is 100.
c@ cutoff
c	Iterating stops if the maximum falls below this level. The
c	default is 0.
c@ clip
c	This sets the relative clip level. Values are typically 0.75 to 0.9.
c	The default is conservative and image dependent.
c@ region
c	The standard region of interest keyword. See `help region' for more
c	information. The default is the entire image.
c@ options
c	Task enrichment parameters. Several can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  asym       The beam for at least one pointing is asymmetric. By
c	             default MOSSDI assumes the beam has 180 degree rotation
c	             symmetry, which is the norm for beams in radio-astronomy.
c	  pad        Double the beam size by padding it with zeros. This
c	             will give better stability if you did not use the
c	             `double' option in INVERT.
c--
c  History:
c    rjs 31oct94 - Original version.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='MosSDI: version 1.0 31-Oct-94')
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	integer MAXRUN,MAXBOXES
	parameter(MAXRUN=3*maxdim,MAXBOXES=1024)
c
	character MapNam*64,BeamNam*64,ModelNam*64,OutNam*64,line*64
	character flags*4
	integer Boxes(MAXBOXES),Run(3,MAXRUN),nRun,blc(3),trc(3),maxMap
	integer nPoint
	integer lMap,lBeam,lModel,lOut
	integer i,k,imin,imax,jmin,jmax,kmin,kmax,xmin,xmax,ymin,ymax
	integer naxis,nMap(3),nbeam(3),nout(MAXNAX),nModel(3)
	integer pStep,pStepR,pRes,pEst
	integer maxniter,niter,ncomp
	logical more
	real dmin,dmax,drms,cutoff,clip,gain
	logical asym,pad
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
	call keya('out',OutNam,' ')
	call keyi('niters',maxniter,100)
	call keyr('cutoff',cutoff,0.0)
	call keyr('gain',gain,0.1)
	call keyr('clip',clip,0.0)
	call BoxInput('region',MapNam,Boxes,MaxBoxes)
	call GetOpt(asym,pad)
	call keyfin
c
c  Check everything makes sense.
c
	if(maxniter.lt.0)call bug('f','NITERS has bad value')
	if(MapNam.eq.' '.or.BeamNam.eq.' '.or.OutNam.eq.' ')
     *	  call bug('f','A file name was missing from the parameters')
c
c  Open the input map.
c
	call xyopen(lMap,MapNam,'old',3,nMap)
	if(max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
	call coInit(lMap)
	call rdhdi(lMap,'naxis',naxis,3)
	naxis = min(naxis,MAXNAX)
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
	do i=4,naxis
	  nout(i) = 1
	enddo
c
c  Allocate arrays to hold everything.
c
	MaxMap = nOut(1)*nOut(2)
	call MemAlloc(pStep, MaxMap,'r')
	call MemAlloc(pStepR,MaxMap,'r')
	call MemAlloc(pEst,  MaxMap,'r')
	call MemAlloc(pRes,  MaxMap,'r')
c
c  Open the model if needed, and check that is is the same size as the
c  output.
c
	if(ModelNam.ne.' ')then
	  call xyopen(lModel,ModelNam,'old',3,nModel)
	  if(nModel(1).ne.nOut(1).or.nModel(2).ne.nOut(2)
     *	    .or.nModel(3).ne.nOut(3)) call bug('f','Model size is bad')
	endif
c
c  Open up the output.
c
	call xyopen(lOut,OutNam,'new',naxis,nOut)
c
c  Initialise the convolution routines.
c
	call output('Initialising the convolutioner ...')
	call xyopen(lBeam,BeamNam,'old',3,nBeam)
	flags = ' '
	if(.not.asym) flags(1:1) = 's'
	if(pad)       flags(2:2) = 'e'
	call mosCnvlI(lBeam,flags)
c
c  Loop.
c
	do k=kmin,kmax
	  if(kmin.ne.kmax)then
	    call output('Beginning plane: '//itoaf(k))
	  else
	    call output('Beginning to CLEAN ...')
	  endif
c
	  call BoxRuns(1,k,' ',boxes,Run,MaxRun,nRun,
     *					xmin,xmax,ymin,ymax)
	  call mosCnvlS(lMap,k,Run,nRun)
c
c  Get the Map.
c
	  call xysetpl(lMap,1,k)
	  call GetPlane(lMap,Run,nRun,0,0,nMap(1),nMap(2),
     *				memr(pRes),MaxMap,nPoint)
c
c  Get the Estimate and Residual. Also get information about the
c  current situation.
c
	  if(ModelNam.eq.' ')then
	    call Zero(nPoint,memr(pEst))
	  else
	    call xysetpl(lModel,1,k-kmin+1)
	    call GetPlane(lModel,Run,nRun,1-imin,1-jmin,
     *			nModel(1),nModel(2),memr(pEst),MaxMap,nPoint)
	    call Diff(memr(pEst),memr(pRes),memr(pStep),nPoint,Run,nRun)
	    call swap(pRes,pStep)
	  endif
c
c  Do the real work.
c
	  niter = 0
	  more = .true.
	  dowhile(more)
	    call Steer(memr(pEst),memr(pRes),memr(pStep),memr(pStepR),
     *	      nPoint,Run,nRun,gain,clip,dmin,dmax,drms,ncomp)
	    line = ' Components subtracted: '//itoaf(ncomp)
	    call output(line)
	    write(line,'(a,1p3e12.3)')' Residual min,max,rms: ',
     *					dmin,dmax,drms
	    call output(line)
	    niter = niter + ncomp
	    more = niter.lt.maxniter.and.
     *		   max(abs(dmax),abs(dmin)).gt.cutoff
	  enddo
c
c  Write out this plane.
c
	  call xysetpl(lOut,1,k-kmin+1)
	  call PutPlane(lOut,Run,nRun,1-imin,1-jmin,
     *				nOut(1),nOut(2),memr(pEst),nPoint)
	enddo
c
c  Make the header of the output.
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
	subroutine GetOpt(asym,pad)
c
	implicit none
	logical asym,pad
c
c  Get extra processing options.
c
c  Output:
c    asym	Beam is asymmetric.
c    pad	Double beam size.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=2)
	logical present(NOPT)
	character opts(NOPT)*8
	data opts/'asym    ','pad     '/
c
	call options('options',opts,present,NOPT)
	asym = present(1)
	pad  = present(2)
c
	end
c************************************************************************
	subroutine Steer(Est,Res,Step,StepR,nPoint,Run,nRun,
     *	  gain,clip,dmin,dmax,drms,ncomp)
c
	implicit none
	integer nPoint,nRun,Run(3,nRun),ncomp
	real gain,clip,dmin,dmax,drms
	real Est(nPoint),Res(nPoint),Step(nPoint),StepR(nPoint)
c
c  Perform a Steer iteration.
c
c  Input:
c    nPoint	Number of points in the input.
c    Run,nRun	This describes the region-of-interest.
c    gain	CLEAN loop gain.
c    clip	Steer clip level.
c  Input/Output:
c    Est	The current deconvolved image estimate.
c    Res	The current residuals.
c  Scratch:
c    Step,StepR	Used to contain the proposed change and its convolution
c		with the beam pattern.
c  Output:
c    dmin,dmax,drms Min, max and rms residuals after this iteration.
c    ncomp	Number of components subtracted off this time.
c------------------------------------------------------------------------
	integer i
	real thresh,g
	double precision RR,RS
c
c  Externals.
c
	integer isamax
c
c  Determine the threshold.
c
	i = isamax(nPoint,Res,1)
	thresh = clip * abs(Res(i))
c
c  Get the step to try.
c
	ncomp = 0
	do i=1,nPoint
	  if(abs(Res(i)).gt.thresh)then
	    Step(i) = Res(i)
	    ncomp = ncomp + 1
	  else
	    Step(i) = 0
	  endif
	enddo
c
c  Convolve this step.
c
	call mosCnvlR(Step,Run,nRun,StepR)
c
c  Now determine the amount of this step to subtract which would
c  minimise the residuals.
c
	RR = 0
	RS = 0
	thresh = 1e-4*thresh
	do i=1,nPoint
	  if(abs(StepR(i)).gt.thresh)then
	    RR = RR + Res(i)*Res(i)
	    RS = RS + Res(i)*StepR(i)
	  endif
	enddo
	g = RR / RS
c
c  Subtract off a fraction of this, and work out the new statistics.
c
	g = gain * g
	dmin = Res(1) - g*StepR(1)
	dmax = dmin
	RR = 0
	do i=1,nPoint
	  Res(i) = Res(i) - g * StepR(i)
	  dmin = min(dmin,Res(i))
	  dmax = max(dmax,Res(i))
	  RR = RR + Res(i)*Res(i)
	  Est(i) = Est(i) + g * Step(i)
	enddo
c
	drms = sqrt(RR/nPoint)
c
	end
c************************************************************************
	subroutine Diff(Est,Map,Res,nPoint,Run,nRun)
c
	implicit none
	integer nPoint,nRun,Run(3,nRun)
	real Est(nPoint),Map(nPoint),Res(nPoint)
c
c  Determine the residuals for this model.
c------------------------------------------------------------------------
	integer i
c
	call mosCnvlR(Est,Run,nRun,Res)
c
	do i=1,nPoint
	  Res(i) = Map(i) - Res(i)
	enddo
c
	end
c************************************************************************
	subroutine Swap(a,b)
c
	implicit none
	integer a,b
c
c  Swap two integers about.
c------------------------------------------------------------------------
	integer t
c
	t = a
	a = b
	b = t
	end
c************************************************************************
	subroutine Zero(n,Out)
c
	implicit none
	integer n
	real Out(n)
c
c  Zero an array.
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  Out(i) = 0
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
	integer i,lblc,ltrc
	real crpix1,crpix2,crpix3
	character line*72,txtblc*32,txttrc*32
	integer nkeys
	parameter(nkeys=23)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ','crval1  ',
     *	  'crval2  ','crval3  ','ctype1  ','ctype2  ','ctype3  ',
     *    'obstime ','epoch   ','history ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','object  ','pbfwhm  ',
     *	  'observer','telescop','restfreq','vobs    ','btype   '/
c
c  Fill in some parameters that will have changed between the input
c  and output.
c
	call wrhda(lOut,'bunit','JY/PIXEL')
	call rdhdr(lMap,'crpix1',crpix1,1.)
	call rdhdr(lMap,'crpix2',crpix2,1.)
	call rdhdr(lMap,'crpix3',crpix3,1.)
	crpix1 = crpix1 - blc(1) + 1
	crpix2 = crpix2 - blc(2) + 1
	crpix3 = crpix3 - blc(3) + 1
	call wrhdr(lOut,'crpix1',crpix1)
	call wrhdr(lOut,'crpix2',crpix2)
	call wrhdr(lOut,'crpix3',crpix3)
	call wrhdi(lOut,'niters',Niter)
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
        line = 'MOSSDI: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'MOSSDI')
c
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	line = 'MOSSDI: Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'
	call hiswrite(lOut,line)
c
	call hiswrite(lOut,'MOSSDI: Total Iterations = '//itoaf(Niter))
	call hisclose(lOut)
c
	end
