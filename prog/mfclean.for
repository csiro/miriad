c************************************************************************
	program mfclean
	implicit none
c
c= mfclean - Multi-frequency synthesis CLEAN.
c& rjs
c: deconvolution
c+
c	MFCLEAN is a MIRIAD task to deconvolve a multi-frequency synthesis
c	image. It can perform either Clark or Hogbom iterations.
c	To achieve good results, the width of the beam should be at
c	least 3 times that of the region being cleaned. The
c	region being cleaned should be reasonably centred in the map, and
c	should have an appreciable guard band around it to the map edge (of
c	size comparable to the width of the region being cleaned).
c
c	To form a multi-frequency synthesis image and beam, use INVERTs
c	``mfs'' and ``sdb'' options. This will create
c	a map with one plane, and a beam with two planes (the normal dirty
c	beam, and the spectral dirty beam).
c
c	The result of MFCLEAN is a component image, consisting of two planes.
c	The first plane is the normal flux components. The second plane are
c	components of ``flux times spectral index'' (that is, I*alpha).
c
c	Use task MFSPIN to get a crude spectral index image from the output
c	of MFCLEAN.
c
c	The sign convention used for the spectral index, alpha, is that:
c	  I(f) = I(f0) * (f/f0) ** alpha
c
c	MFCLEAN differs from CLEAN in a number of ways.
c	* Task CLEAN only requires that the beam is twice the size of the
c	  region being cleaned whereas, for MFCLEAN, it is recommended that
c	  the dirty beam be three times the size of the region being cleaned.
c	* MFCLEAN requires a guard band around the edge of the region being
c	  cleaned.
c	* MFCLEAN does not have a Steer cleaning option, nor prussian hats.
c@ map
c	The input dirty map, which should have units of Jy/beam. No
c	default. 
c@ beam
c	The input dirty beam. This should be formed using INVERT with
c	options=sdb. No default.
c@ model
c	An initial model of the deconvolved image. This could be the
c	output from a previous run of MFCLEAN. It must have flux units of
c	Jy/pixel. The default is no model (i.e. a zero map).
c@ out
c	The name of the output map. The units of the output will be
c	Jy/pixel. This file will contain the contribution of the input model.
c	It will consist of two planes, giving the flux density image and the
c	"flux times spectral index" image (also called the scaled flux
c	derivative image). No default.
c@ gain
c	The minor iteration loop gain. Two values can be given, the second
c	being the gain for the spectral components. If only one value is
c	given, the flux and spectral components use the same gain. The
c	default is 0.1.
c@ cutoff
c	MFCLEAN finishes when the absolute maximum residual falls below
c	CUTOFF. Default is 0. 
c@ niters
c	The maximum number of minor iterations. MFCLEAN finishes when
c	abs(NITERS) minor iterations have been performed. Clean may finish
c	before this point, however, if NITERS is negative and the absolute
c	maximum residual becomes negative valued, or if the cutoff level
c	(as described above) is reached. 
c@ region
c	This specifies the region to be Cleaned. See the Users Manual for
c	instructions on how to specify this. The default is the inner
c	quarter. This default is generally inadequate, and a smaller
c	region should be explicitly specified.
c@ minpatch
c	The minimum patch size when performing minor iterations. Default
c	is 51, but make this larger if you are having problems with
c	corrugations. You can make it smaller when cleaning images which
c	consist of a pretty good dirty beam. 
c@ speed
c	This is the same as the speed-up factor in the AIPS APCLN.
c	Negative values makes the rule used to end a major iteration more
c	conservative. This causes less components to be found during a
c	major iteration, and so should improve the quality of the Clean
c	algorithm. Usually this will not be needed unless you are having
c	problems with corrugations. A positive value can be useful when
c	cleaning simple point-like sources. Default is 0. 
c@ mode
c	This can be either "hogbom", "clark" or "any", and
c	determines the Clean algorithm used. If the mode is "any", then
c	MFCLEAN determines which is the best algorithm to use. The default
c	is "any". 
c--
c  History:
c    rjs   Nov89 - Original version.
c    rjs  9jun89 - Call sequences to GETPLANE and PUTPLANE has changed.
c		   Subroutine DIFF included in this file. Headers improved.
c    rjs 13mar90 - Copy across linetype parameters from input to output file.
c		   Added version thingo.
c    rjs 12mar90 - Fixed bug which caused the old history to be overwritten.
c    rjs 30apr90 - Changed call sequence to BoxInput.
c   mchw 11jul90 - Added header keywords and increased length of filenames.
c   mchw 09nov90 - Added pbfwhm to map header.
c   mjs  25feb91 - Changed references of itoa to itoaf, mitoa to mitoaf.
c   rjs  20mar91 - Trivial change to tolerate multi-plane beams (for mfs).
c		   Rearranged declarations and data statement, in header,
c		   to standard order. Renamed "index" to "indx" in GetComp
c		   to appease flint.
c   rjs  20mar91 - Reincarnation as MFCLEAN.
c   rjs   8apr91 - Use Mem routines. Handle images with naxis4=1 better.
c		   Some cosmetic changes.
c   rjs  19sep91 - Significant rework.
c   rjs  14oct91 - Bug in NewEst, like the one in clean.for
c   rjs  10mar92 - Improvements to the documentation and error messages only!
c   rjs  12mar92 - Made tests for convergence consistent in main routine
c		   and SubComp -- to avoid potential infinite loops!
c   rjs  17may92 - Minor mods in the messages.
c   rjs  27may92 - Doc changes only.
c   rjs  12nov92 - Doc changes only.
c   nebk 25nov92 - COpy btype to output
c   rjs  30mar93 - Limit the size of the spectral component.
c   rjs  27nov93 - Another algorithm to try to limit the size of the spectral
c		   component.
c   rjs   4may95 - Doc change only.
c   rjs  29nov95 - Better treatment of model.
c   rjs  13sep96 - Friday 13th! Improve check for negative components.
c
c  Bugs and Shortcomings:
c     * The way it does convolutions is rather inefficent, partially
c	because all the FFTing is hidden away.
c
c  Important Constants:
c    MaxDim	The max linear dimension of an input (or output) image.
c
c  Runs:
c    Run(3,nrun) We may want to clean several boxes (which may overlap).
c		Run is an 3 x N array, with each entry being of the form:
c		  j, imin, imax
c		This gives a range of i in line j to process. There could
c		be several entries for a given line, describing disconnected
c		ranges of i to process (There is no overlap between runs).
c		Specifying boxes in this way is reasonably concise for the
c		programmer, and yet makes vectorisable code straightforward
c		to write.
c
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='MfClean: version 1.0 27-Nov-93')
	include 'maxdim.h'
	integer maxBeam,maxCmp1,maxCmp2,maxBox,maxRun,maxP
	parameter(maxCmp1=66000,maxCmp2=32000,maxP=257)
	parameter(maxBeam=maxP*maxP,maxBox=3*MAXDIM,maxRun=3*maxDim)
c
	integer Boxes(maxBox),Run(3,maxRun),nPoint,nRun
	integer Map0,Map1,Res0,Res1,Est0,Est1,Tmp
	integer FFT0,FFT1,FFT01,FFT10,FFT00,FFT11
	real Rcmp0(maxCmp2),Rcmp1(maxCmp2),Ccmp0(maxCmp2),Ccmp1(maxCmp2)
	real Histo(maxP/2+1)
	real Patch00(maxBeam),Patch11(maxBeam)
	real Patch01(maxBeam),Patch10(maxBeam)
	integer Icmp(maxCmp1),JCmp(maxCmp1)
c
	character Mode*8,Text*7
	real Cutoff,Gain0,Gain1,Speed,Limit,Scale
	logical NegStop,NegFound,More
	integer maxNiter,Niter,totNiter,minPatch,maxPatch
	integer naxis,n1,n2,n1d,n2d,ic,jc,nx,ny,ntmp
	integer xmin,xmax,ymin,ymax,xoff,yoff,zoff
	character MapNam*64,BeamNam*64,ModelNam*64,OutNam*64,line*72
	integer lMap,lBeam,lModel,lOut
	integer nMap(3),nBeam(3),nModel(3),nOut(4)
	real EstASum
	real ResMin,ResMax,ResAMax,ResRms
c
	real dat(maxBuf)
	common dat
c
c  Externals.
c
	character itoaf*8
c
c  Get the input parameters.
c
	call output(version)
	call inputs(MapNam,BeamNam,ModelNam,OutNam,maxNiter,NegStop,
     *	  Cutoff,Boxes,maxBox,MinPatch,Gain0,Gain1,Speed,mode)
c
c  Open the beam, get some characteristics about it, then read in the
c  beam patch. The beam patch is not required if we are performing Steer
c  iterations only. However, some of the statistics returned by BeamChar
c  are required.
c
	call xyopen(lBeam,BeamNam,'old',3,nBeam)
	n1 = nBeam(1)
	n2 = nBeam(2)
	if(nBeam(3).lt.2) call bug('f',
     *	  'MFCLEAN requires a spectral dirty beam')
c
c  Fiddle the min and max patch sizes.
c
	maxPatch = min(maxP,2*((min(n1,n2)-1)/2) + 1)
	if(maxPatch.le.0) call bug('f','Bad patch size')
	call rdhdi(lBeam,'crpix1',ic,n1/2+1)
	call rdhdi(lBeam,'crpix2',jc,n2/2+1)
	maxPatch = min(maxPatch,
     *		    2*min(ic-1,n1-ic,jc-1,n2-jc)+1)
	if(minPatch.gt.maxPatch)then
	  call bug('w','Setting min patch size to '//itoaf(maxPatch))
	  minPatch = maxPatch
	endif
c
c  Open the map, and determine the area being cleaned.
c
	call xyopen(lMap,MapNam,'old',3,nMap)
	if(nMap(3).ne.1)
     *	  call bug('f','Input map is not a multi-freq synthesis map')
	call rdhdi(lMap,'naxis',naxis,3)
	naxis = max(min(naxis,4),3)
	call BoxMask(lMap,boxes,maxBox)
	call BoxSet(boxes,3,nMap,'q')
	call BoxRuns(1,1,'r',boxes,Run,maxRun,nRun,
     *					xmin,xmax,ymin,ymax)
	nx = xmax - xmin + 1
	ny = ymax - ymin + 1
	call CntRuns(Run,nRun,nPoint)
	if(nPoint.le.0)call bug('f','No pixels selected!')
	if(nx.gt.n1.or.ny.gt.n2)
     *	  call bug('f','Region of map to deconvolve is too big')
	if(2*nx-1.gt.n1.or.2*ny-1.gt.n2)
     *	  call bug('w','Size of region of map to deconvolve is unsafe')
c
c  Determine the CLEAN algorithm that is to be used.
c
	if((mode.eq.'any'.or.mode.eq.'hogbom').and.
     *	    nPoint.le.maxCmp1.and.
     *	    (2*nx-1).le.maxPatch.and.(2*ny-1).le.maxPatch)then
	  mode = 'hogbom'
	else
	  if(mode.eq.'hogbom')
     *	    call bug('w','Cannot use Hogbom algorithm -- using Clark')
	  mode = 'clark'
	endif
c
c  Determine the size of the sub-beam.
c
	n1d = min(ic-1,n1-ic) - nx + 1
	n1d = 2*min(n1d,xmin-1,nMap(1)-xmax,nx-1) + 1
	n2d = min(jc-1,n2-jc) - ny + 1
	n2d = 2*min(n2d,ymin-1,nMap(2)-ymax,ny-1) + 1
	write(line,'(a,i4,a,i4)')'Sub-beam size is',n1d,' by',n2d
	call output(line)
	if(n1d.lt.5.or.n2d.lt.5)then
	  call bug('w','Sub-beam size is too small')
	  call bug('f','Specify a smaller region to be cleaned')
	endif
	if(n1d.lt.nx/2.or.n2d.lt.ny/2)
     *	  call bug('w','Sub-beam size is dangerously small')
c
c  Open the output.
c
	nOut(1) = nx
	nOut(2) = ny
	nOut(3) = 2
	nOut(4) = 1
	call xyopen(lOut,OutNam,'new',naxis,nOut)
c
c  Get the FFT of the beam.
c
	call output('FFTing the beams ...')
	call MemAlloc(Tmp,n1*n2,'r')
	call xysetpl(lBeam,1,1)
	call GetBeam(FFT0,lBeam,dat(Tmp),n1,n2,n1d,n2d,ic,jc)
	call xysetpl(lBeam,1,2)
	call GetBeam(FFT1,lBeam,dat(Tmp),n1,n2,n1d,n2d,ic,jc)
c
c  Get the convolution of the map and the two beams.
c
	call output('Calculating the map*beam ...')
	call MemAlloc(Map0,nPoint,'r')
	call MemAlloc(Map1,nPoint,'r')
	call GetMap(FFT0,lMap,Run,nRun,xmin-1,ymin-1,dat(Map0),
     *					dat(Tmp),nMap(1),nMap(2))
	call GetMap(FFT1,lMap,Run,nRun,xmin-1,ymin-1,dat(Map1),
     *					dat(Tmp),nMap(1),nMap(2))
c
c  Extract the patches, and get some statistics about the result.
c
	call output('Calculating patches ...')
	call xysetpl(lBeam,1,2)
	call GetPatch(FFT1,lBeam,n1,n2,
     *	  Patch11,maxPatch,ic,jc,dat(Tmp))
	call GetPatch(FFT0,lBeam,n1,n2,
     *	  Patch10,maxPatch,ic,jc,dat(Tmp))
	call xysetpl(lBeam,1,1)
	call GetPatch(FFT1,lBeam,n1,n2,
     *	  Patch01,maxPatch,ic,jc,dat(Tmp))
	call GetPatch(FFT0,lBeam,n1,n2,
     *	  Patch00,maxPatch,ic,jc,dat(Tmp))
	if(mode.eq.'clark')
     *	  call GetHisto(dat(Tmp),n1,n2,ic,jc,Histo,maxPatch/2+1)
c
c  Get the flux scale factor, and scale the cutoff.
c
	Scale = Patch00(maxPatch/2+1+(maxPatch/2)*maxPatch)
	Cutoff = Scale * Cutoff
c
c  If the mode is Clark, or if we have an input model, we need to form
c  the beam0*beam0, beam1*beam1 and beam0*beam1 beam FFTs.
c
	if(mode.eq.'clark'.or.ModelNam.ne.' ')then
	  call xysetpl(lBeam,1,1)
	  call CnvlIniF(FFT00,lBeam,n1,n2,ic,jc,0.,'s')
	  call CnvlCopy(FFT01,FFT00,'x')
	  call CnvlCo(FFT01,FFT1,'x')
	  call CnvlCo(FFT00,FFT0,'x')
	  call xysetpl(lBeam,1,2)
	  call CnvlIniF(FFT11,lBeam,n1,n2,ic,jc,0.,'s')
	  call CnvlCopy(FFT10,FFT11,'x')
	  call CnvlCo(FFT10,FFT0,'x')
	  call CnvlCo(FFT11,FFT1,'x')
	endif
c
c  Free up the unneeded beams, and then allocate some more memory.
c
	call CnvlFin(FFT1)
	call CnvlFin(FFT0)
	call MemAlloc(Res0,nPoint,'r')
	call MemAlloc(Res1,nPoint,'r')
	call MemAlloc(Est0,nPoint,'r')
	call MemAlloc(Est1,nPoint,'r')
c
c  Initialise the estimate, and determine the residuals if the the user
c  gave an estimate. Determine statistics about the estimate and the
c  residuals.
c
	if(ModelNam.eq.' ')then
	  EstASum = 0
	  call NoModel(dat(Map0),dat(Map1),dat(Est0),dat(Est1),
     *					dat(Res0),dat(Res1),nPoint)
	  totNiter = 0
	else
	  call output('Loading the model and getting residuals ...')
	  call xyopen(lModel,ModelNam,'old',3,nModel)
	  call AlignIni(lModel,lMap,nModel(1),nModel(2),nModel(3),
     *						xoff,yoff,zoff)
	  zoff = 0
	  call AlignGet(lModel,Run,nRun,1,xmin+xoff-1,ymin+yoff-1,zoff,
     *		nModel(1),nModel(2),nModel(3),dat(Est0),nPoint,ntmp)
	  call AlignGet(lModel,Run,nRun,2,xmin+xoff-1,ymin+yoff-1,zoff,
     *		nModel(1),nModel(2),nModel(3),dat(Est1),nPoint,ntmp)
	  call Diff(dat(Est0),dat(Est1),dat(Map0),dat(Map1),
     *	    dat(Res0),dat(Res1),dat(Tmp),nPoint,nx,ny,Run,nRun,
     *	    FFT00,FFT11,FFT01,FFT10)
	  call SumAbs(EstASum,dat(Est0),nPoint)
	  call rdhdi(lModel,'niters',totNiter,0)
	  call xyclose(lModel)
	endif
c
c  Free up the beams if we no longer need them.
c
	if(mode.ne.'clark'.and.ModelNam.ne.' ')then
	  call CnvlFin(FFT00)
	  call CnvlFin(FFT11)
	  call CnvlFin(FFT01)
	  call CnvlFin(FFT10)
	endif
c
c  Get statistics about the residuals.
c
	call output('Starting to iterate ...')
	call Stats(dat(Res0),nPoint,ResMin,ResMax,ResAMax,ResRms)
	
c
c  Perform the appropriate iteration until no more.
c
	Niter = 0
	negFound = .false.
	More = nPoint.gt.0
	Limit = 0
	dowhile(More)
	  if(mode.eq.'hogbom')then
	    call Hogbom(maxPatch,Patch00,Patch11,Patch01,Patch10,nx,ny,
     *	      dat(Res0),dat(Res1),dat(Est0),dat(Est1),Icmp,Jcmp,
     *	      dat(Tmp),nPoint,Run,nRun,EstASum,Cutoff,Gain0,Gain1,
     *	      negStop,negFound,maxNiter,Niter)
	      text = ' Hogbom'
	  else
	    call Clark(nx,ny,dat(Res0),dat(Res1),dat(Est0),dat(Est1),
     *	      nPoint,Run,nRun,Histo,Patch00,Patch11,Patch01,Patch10,
     *	      minPatch,maxPatch,Cutoff,negStop,maxNiter,Gain0,Gain1,
     *	      Speed,ResAMax,EstASum,Niter,Limit,negFound,
     *	      Rcmp0,Rcmp1,Ccmp0,Ccmp1,Icmp,Jcmp,dat(Tmp),maxCmp2)
	    call Diff(dat(Est0),dat(Est1),dat(Map0),dat(Map1),
     *	      dat(Res0),dat(Res1),dat(Tmp),nPoint,nx,ny,Run,nRun,
     *	      FFT00,FFT11,FFT01,FFT10)
	    text = ' Clark'
	  endif
c
c  Output some messages to assure the user that the machine has not crashed.
c
	  call output(Text//' Iterations: '//itoaf(Niter))
	  call Stats(dat(Res0),nPoint,ResMin,ResMax,ResAMax,ResRms)
	  write(line,'(a,1p3e12.3)')' Residual min,max,rms: ',
     *			ResMin/Scale,ResMax/Scale,ResRms/Scale
	  call output(line)
c
c  Check for convergence.
c
	  more = .not.((negFound.and.negStop)
     *			.or.ResAMax.le.Cutoff.or.Niter.ge.MaxNiter)
c
	enddo
c
c  Give a message about what terminated the iterations.
c
	if(ResAMax.le.Cutoff)then
	  call output(' Stopping -- Clean cutoff limit reached')
	else if(Niter.ge.maxNiter)then
	  call output(' Stopping -- Maximum iterations performed')
	else if(NegStop.and.NegFound)then
	  call output(' Stopping -- Negative components encountered')
	endif
c
c  Write out the result.
c
	call xysetpl(lOut,1,1)
	call PutPlane(lOut,Run,nRun,0,0,nx,ny,dat(Est0),nPoint)
	call xysetpl(lOut,1,2)
	call PutPlane(lOut,Run,nRun,0,0,nx,ny,dat(Est1),nPoint)
c
c  Construct a header for the output file, and give some history
c  information.
c
	totNiter = totNiter + Niter
	call Header(lMap,lOut,xmin,ymin,totNiter,version)
c
c  Close up the files. Ready to go home.
c
	call xyclose(lMap)
	call xyclose(lBeam)
	call xyclose(lOut)
c
c  Thats all folks.
c
	end
c************************************************************************
	subroutine GetBeam(FFT,lBeam,Data,n1,n2,n1d,n2d,ic,jc)
c
	implicit none
	integer FFT,lBeam,n1,n2,n1d,n2d,ic,jc
	real Data(n1,n2)
c
c  This reads in a subsection of the beam, zeros part of it, and
c  passes it to the FFT initialisation routines.
c
c  Input:
c    lBeam	Handle of the beam file.
c    n1,n2	Size of the beam file.
c    n1d,n2d	Size of the non-zero segment to be passed to the CnvlIni routine.
c    ic,jc	Centre of the beam.
c  Output:
c    FFT	Handle of the FFT of the beam.
c  Scratch:
c    Data
c------------------------------------------------------------------------
	integer i,j,imin,imax,jmin,jmax
c
	imin = ic - n1d/2 - 1
	imax = ic + n1d/2 + 1
	jmin = jc - n2d/2 - 1
	jmax = jc + n2d/2 + 1
c
	do j=1,n2
	  if(j.le.jmin.or.j.ge.jmax)then
	    do i=1,n1
	      Data(i,j) = 0
	    enddo
	  else
	    call xyread(lBeam,j,Data(1,j))
	    do i=1,imin
	      Data(i,j) = 0
	    enddo
	    do i=imax,n1
	      Data(i,j) = 0
	    enddo
	  endif
	enddo
c
	call CnvlIniA(FFT,Data,n1,n2,ic,jc,0.,'s')
	end
c************************************************************************
	subroutine GetPatch(FFT,lBeam,n1,n2,Patch,maxPatch,ic,jc,Tmp)
c
	implicit none
	integer FFT,lBeam,n1,n2,ic,jc,maxPatch
	real Patch(maxPatch,maxPatch),Tmp(n1,n2)
c
c  Get the beam patch, and determine things about it.
c
c------------------------------------------------------------------------
	integer i,j,ioff,joff
c
c  Get the correlation of the two beams.
c
	call CnvlF(FFT,lBeam,n1,n2,Tmp,'cx')
c
c  Extract out the portion we are interested in.
c
	joff = jc - maxPatch/2 - 1
	ioff = ic - maxPatch/2 - 1
c
	do j=1,maxPatch
	  do i=1,maxPatch
	    Patch(i,j) = Tmp(i+ioff,j+joff)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GetHisto(Data,n1,n2,ic,jc,Histo,nHisto)
c
	implicit none
	integer n1,n2,ic,jc,nHisto
	real Histo(nHisto),Data(n1,n2)
c
c  Get the histogram needed by the Clark iterations.
c
c  Input:
c    n1,n2	Size of the beam.
c    ic,jc	Centre of the beam.
c    nHisto	Size of the output histogram.
c    Data	The beam.
c  Output:
c    Histo	The required histogram info.
c------------------------------------------------------------------------
	integer i,j,k,imin,imax,jmin,jmax
c
c  Externals.
c
	integer isamax
c
c  Initialise the histogram array.
c
	do k=1,nHisto
	  Histo(k) = 0
	enddo
c
c  Determine some limits.
c
	jmin = max(1, jc - (nHisto-2))
	jmax = min(n2,jc + (nHisto-2))
	imin = max(1, ic - (nHisto-2))
	imax = min(n1,ic + (nHisto-2))
c
c  Gather the statistics.
c
	do j=1,n2
	  if(j.lt.jmin.or.j.gt.jmax)then
	    i = isamax(n1,Data(1,j),1)
	    Histo(nHisto) = max(abs(Data(i,j)),Histo(nHisto))
	  else
	    if(imin.gt.1)then
	      i = isamax(imin-1,Data(1,j),1)
	      Histo(nHisto) = max(abs(Data(i,j)),Histo(nHisto))
	    endif
	    if(imax.lt.n1)then
	      i = isamax(n1-imax,Data(imax+1,j),1) + imax
	      Histo(nHisto) = max(abs(Data(i,j)),Histo(nHisto))
	    endif
c
	    do i=imin,imax
	      k = max(abs(i-ic),abs(j-jc)) + 1
	      Histo(k) = max(Histo(k),abs(Data(i,j)))
	    enddo
	  endif
	enddo
c
c  Now Histo(k) contains the max abs value occurring at distance k.
c  Collapse this do so that Histo(k) contains the max abs value
c  occurring at a distance greater of equal to k.
c
	do k=nHisto-1,1,-1
	  Histo(k) = max(Histo(k),Histo(k+1))
	enddo
c
	end
c************************************************************************
	subroutine GetMap(FFT,lMap,Run,nRun,xoff,yoff,Map,Tmp,nx,ny)
c
	implicit none
	integer FFT,lMap,nRun,Run(3,nRun),nx,ny,xoff,yoff
	real Map(*),Tmp(nx,ny)
c
c  Correlate the map with the beam, and return just that part of the
c  result that is within our region of interest. The region of interest
c  is given by the runs array.
c
c  Input:
c    FFT	Handle of the FFT of the beam.
c    lMap	Handle of the map file.
c    Run	The runs specification.
c    nRun	Number of runs.
c    xoff,yoff	Offsets to add to the runs specifications.
c    nx,ny	Size of the map file, and also the size of the scratch
c		array.
c  Output:
c    Map	The region of interest.
c  Scratch:
c    Tmp
c------------------------------------------------------------------------
	integer i,j,k,pnt
c
c  Correlate the map with the beam, and put the result into Tmp.
c
	call CnvlF(FFT,lMap,nx,ny,Tmp,'cx')
c
c  Extract out the runs of interest.
c
	pnt = 0
	do k=1,nRun
	  j = Run(1,k) + yoff
	  do i=Run(2,k)+xoff,Run(3,k)+xoff
	    pnt = pnt + 1
	    Map(pnt) = Tmp(i,j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine CntRuns(Run,nRun,MaxMap)
c
	implicit none
	integer nRun,Run(3,nRun),MaxMap
c
c  Count the number of pixels in the region of interest.
c
c  Input:
c    Run	Describes the runs in the region of interest.
c    nRun	The number of runs.
c  Output:
c    MaxMap	Number of pixels in the region of interest.
c------------------------------------------------------------------------
	integer i
c
	MaxMap = 0
	do i=1,nRun
	  MaxMap = MaxMap + Run(3,i) - Run(2,i) + 1
	enddo
c
	end
c************************************************************************
	subroutine Stats(Data,n,Dmin,Dmax,DAmax,Drms)
c
	implicit none
	integer n
	real Data(n)
	real Dmin,Dmax,DAmax,Drms
c
c  Calculate every conceivably wanted statistic.
c
c  Input:
c    n		Number of points.
c    Data	Input data array.
c
c  Output:
c    Dmin	Data minima.
c    Dmax	Data maxima.
c    DAmax	Data absolute maxima.
c    Drms	Rms value of the data.
c
c------------------------------------------------------------------------
	integer i
c
c  Externals.
c
	integer ismax,ismin
c
c  Calculate the minima and maxima.
c
	i = ismax(n,Data,1)
	Dmax = Data(i)
	i = ismin(n,Data,1)
	Dmin = Data(i)
	DAmax = max(abs(Dmax),abs(Dmin))
c
c  Calculate the sums.
c
	Drms = 0
	do i=1,n
	  Drms = Drms + Data(i)*Data(i)
	enddo
	Drms = sqrt(Drms/n)
c
	end
c************************************************************************
	subroutine inputs(map,beam,estimate,out,Niter,negStop,
     *	  cutoff,box,maxbox,minpatch,gain0,gain1,speed,mode)
c
	implicit none
	integer Niter, minpatch, maxbox
	integer box(maxbox)
	real cutoff,gain0,gain1,speed
	logical negStop
	character map*(*),beam*(*),estimate*(*),out*(*),mode*(*)
c
c       Get user supplied inputs
c
c    Input:
c      maxbox	   The maximum number of boxes.
c    Output:
c      Map         Name of the input map. No default.
c      Beam        Name of the input beam. No default.
c      Estimate    Name of the input estimate of the deconvolved image.
c	           Default is a estimate which is zero.
c      Out         Name of the output deconvolved map. No default.
c      Gain0,Gain1 Clean loop gain. Default 0.5 (a bit high).
c      Cutoff)     The iterations stop when either the absolute max residual
c      Niter )    remaining is less than Cutoff, of Niter minor iterations
c                  have been performed (whichever comes first). The default
c		   Cutoff is 0 (i.e. iterate forever), and the default number
c	           of minor iterations is 250.  This is the total
c                  number of iterations to do.
c      negStop	   Stop on first negative component.
c      minpatch    The minimum beam patch width.
c      speed       Speedup factor. Default is 0.
c      box	   The boxes specification.
c      mode	   Either "hogbom","clark" or "any"(default).
c
c-------------------------------------------------------------------------
	include 'maxdim.h'
c
	call keyini
	call keya ('map', map, ' ')
	call keya ('beam', beam, ' ')
	call keya ('model', estimate,' ')
	call keya ('out', out, ' ')
	if (map.eq.' ' .or. beam.eq.' ' .or. out.eq.' ')
     *	  call bug ('f', 'A file name was missing from the parameters')
	call keyi ('niters', Niter, 250)
	negStop = Niter.lt.0
	Niter = abs(Niter)
	if (Niter.eq.0) call bug ('f', 'NITERS must be nonzero')
	call keyr ('cutoff', cutoff,0.)
c
	call BoxInput('region',map,box,maxbox)
c
	call keyi ('minpatch', minpatch, 51)
	call keyr ('gain', gain0, 0.1)
	if (gain0.le.0 .or. gain0.gt.1)
     *    call bug('f','Bad gain value, it must be in the range (0,1]')
	call keyr ('gain', gain1, gain0)
	call keyr ('speed', speed, 0.0)
	call keya('mode',mode,'any')
	if(mode.ne.'clark'.and.mode.ne.'any'.and.mode.ne.'hogbom')
     *	  call bug('f','Bad value for mode')
	call keyfin
c
        end
c************************************************************************
	subroutine Header(lIn,lOut,xmin,ymin,Niter,version)
c
	implicit none
	integer lIn,lOut,Niter,xmin,ymin
	character version*(*)
c
c Copy across the header to the model.
c
c  Inputs:
c    lIn
c    lOut
c    xmin,ymin
c    Niter
c    version
c------------------------------------------------------------------------
	integer i
	real crpix1,crpix2
	character line*72
	integer nkeys
	parameter(nkeys=33)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *	  'crpix3  ','crpix4  ','crval1  ','crval2  ','crval3  ',
     *	  'crval4  ','ctype1  ','ctype2  ','ctype3  ','ctype4  ',
     *    'date-obs','epoch   ','history ','instrume','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','object  ',
     *	  'observer','telescop','xshift  ','yshift  ','obsra   ',
     *	  'obsdec  ','restfreq','vobs    ','pbfwhm  ','btype   '/
c
c  Fill in some parameters that will have changed between the input
c  and output.
c
	call wrhda(lOut,'bunit','JY/PIXEL')
	call rdhdr(lIn,'crpix1',crpix1,1.)
	call rdhdr(lIn,'crpix2',crpix2,1.)
	crpix1 = crpix1 - xmin + 1
	crpix2 = crpix2 - ymin + 1
	call wrhdr(lOut,'crpix1',crpix1)
	call wrhdr(lOut,'crpix2',crpix2)
	call wrhdi(lOut,'niters',Niter)
c
c  Copy all the other keywords across, which have not changed and add history
c
	do i=1,nkeys
	  call hdcopy(lIn, lOut, keyw(i))
	enddo
c
c  Write crap to the history file, to attempt (ha!) to appease Neil.
c  I will never be happy Robbie.
c
	call hisopen(lOut,'append')
	line = 'MFCLEAN: Miriad '//version
        call hiswrite(lOut,line)
	call hisinput(lOut,'MFCLEAN')
	call hiswrite(lOut,'MFCLEAN: Total Iterations = '//itoaf(Niter))
	call hisclose(lOut)
c
	end
c************************************************************************
	subroutine SumAbs(EstASum,Est0,nPoint)
c
	implicit none
	integer nPoint
	real Est0(nPoint),EstASum
c
c  Find the sum of the absolute value of the estimate.
c
c  Input:
c    nPoint
c    Est0
c  Output:
c    EstASum
c------------------------------------------------------------------------
	integer i
c
	EstASum = 0
	do i=1,nPoint
	  EstASum = EstASum + abs(Est0(i))
	enddo
	end
c************************************************************************
	subroutine NoModel(Map0,Map1,Est0,Est1,Res0,Res1,nPoint)
c
	implicit none
	integer nPoint
	real Map0(nPoint),Map1(nPoint),Est0(nPoint),Est1(nPoint)
	real Res0(nPoint),Res1(nPoint)
c
c  This initialises the estimate and the residuals, for the case where
c  this is no model.
c
c  Input:
c    nPoint	Number of points.
c    Map0	The original dirty map correlated with Beam0.
c    Map1	The original dirty map correlated with Beam1.
c  Output:
c    Res0	The beam0 residuals.
c    Res1	The beam1 residuals.
c    Est0	The estimate, which are initially zero.
c    Est1	The spectral estimate, which are initially zero.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Est0(i) = 0.
	  Est1(i) = 0.
	  Res0(i) = Map0(i)
	  Res1(i) = Map1(i)
	enddo
	end
c************************************************************************
	subroutine Hogbom(n,Patch00,Patch11,Patch01,Patch10,nx,ny,
     *	  Rcmp0,Rcmp1,Ccmp0,Ccmp1,Icmp,Jcmp,Tmp,Ncmp,Run,nRun,
     *	  EstASum,Cutoff,gain0,gain1,negStop,negFound,MaxNiter,Niter)
c
	implicit none
	integer nx,ny,nCmp,nRun,n
	integer ICmp(nCmp),JCmp(nCmp),Run(3,nRun)
	real Rcmp0(nCmp),Rcmp1(nCmp),Ccmp0(nCmp),Ccmp1(nCmp),Tmp(nCmp)
	real Patch00(n,n),Patch11(n,n),Patch01(n,n),Patch10(n,n)
	real Cutoff,gain0,gain1,EstASum
	logical negStop,negFound
	integer MaxNiter,Niter
c
c  Perform a Hogbom Clean.
c
c  Inputs:
c    Patch00	The autocorrelation of the Beam0 beam.
c    Patch11	The autocorrelation of the Beam1 beam.
c    Patch01	The cross correlation of the Beam0 and Beam1 beams.
c    Patch10	The cross correlation of the Beam1 and Beam0 beams.
c    n		Size of the beam.
c    nx,ny	Size of the input image.
c    nCmp	Number of pixels.
c    maxNiter	Maximum number of iterations to be performed.
c    negStop	Stop when a negative component is encountered.
c    Gain0,Gain1 Loop gain.
c    Cutoff	Stop when the residuals fall below the cutoff.
c    Run(3,nRun) This specifices the runs of the input image that are to
c		be processed.
c
c  Input/Output:
c    RCmp0,RCmp1 Residuals.
c    CCmp0	The image estimate.
c    CCmp1	Image * Spectral index estimate.
c    Niter	Number of iterations to be performed.
c    EstASum	Sum of the absolute value of the estimate.
c    negFound	Set true if a negative component was encountered.
c
c  Scratch:
c    Icmp	Coordinate in x of a pixel.
c    Jcmp	Coordinate in y of a pixel.
c
c  
c  Internal Crap:
c    Ymap	See SubComp for an explanation.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,Ncmpd,x0,y0,n0,itemp
	integer YMap(maxdim+1)
c
c  Clear out YMap.
c
	do j=1,ny+1
	  YMap(j) = 0
	enddo
c
c  Fill in the array giving the coordinates of the residuals, and
c  accumulate the number of residuals in each row into YMap.
c
	Ncmpd = 0
	do j=1,nRun
	  y0 = Run(1,j)
	  x0 = Run(2,j)-1
	  n0 = Run(3,j) - x0
	  do i=1,n0
	    Ncmpd = Ncmpd + 1
	    Icmp(Ncmpd) = x0 + i
	    Jcmp(Ncmpd) = y0
	  enddo
	  Ymap(y0) = Ymap(y0) + n0
	enddo
	if(Ncmpd.ne.Ncmp) call bug('f','Internal bug in Hogbom - 1')
c
c  YMap currently contains the number of residuals found in a particular row.
c  Convert this so that YMap(j) gives the total number of peak residuals in
c  rows 1 to j-1.
c
	Ncmpd = 0
	do j=1,ny+1
	  itemp = Ncmpd
	  Ncmpd = Ncmpd + Ymap(j)
	  YMap(j) = itemp
	enddo
	if(Ncmpd.ne.Ncmp) call bug('f','Internal bug in Hogbom - 2')
c
c  Ready to perform the subtraction step. Lets go.
c
	call SubComp(nx,ny,Ymap,Patch00,Patch11,Patch01,Patch10,n,n/2,
     *	  Gain0,Gain1,MaxNiter,NegStop,0.,0.,Cutoff,EstASum,Icmp,Jcmp,
     *	  Rcmp0,Rcmp1,Ccmp0,Ccmp1,Tmp,Ncmp,Niter,negFound)
	end
c************************************************************************
	subroutine Clark(nx,ny,Res0,Res1,Est0,Est1,
     *	  nPoint,Run,nRun, Histo,Patch00,Patch11,Patch01,Patch10,
     *	  minPatch,maxPatch,Cutoff,negStop,maxNiter,
     *	  Gain0,Gain1,Speed,ResAMax,EstASum,Niter,Limit,negFound,
     *	  Rcmp0,Rcmp1,Ccmp0,Ccmp1,Icmp,Jcmp,Tmp,maxCmp)
c
	implicit none
	integer nx,ny,minPatch,maxPatch,maxNiter,Niter,nRun,nPoint
	integer Run(3,nrun)
	real Res0(nPoint),Res1(nPoint),Est0(nPoint),Est1(nPoint)
	logical negStop,negFound
	real Cutoff,Gain0,Gain1,Speed,Limit,ResAMax,EstASum
	real Patch00(maxPatch,maxPatch),Patch11(maxPatch,maxPatch)
	real Patch01(maxPatch,maxPatch),Patch10(maxPatch,maxPatch)
	real Histo(maxPatch/2+1)
	integer maxCmp,Icmp(maxCmp),Jcmp(maxCmp)
	real Ccmp0(maxCmp),Ccmp1(maxCmp),Rcmp0(maxCmp),Rcmp1(maxCmp)
	real Tmp(maxCmp)
c
c  Perform the component gathering step of a major Clark Clean iteration.
c  Determine the limiting residual, and store the components
c  greater than in the residual list. Perform the subtraction portion of
c  a minor iteration, by Cleaning this list. Then add the newly found
c  components to the estimate.
c
c  Inputs:
c    nx,ny	Image size.
c    Res0,Res1	The residuals.
c    Est0,Est1	The estimate.
c    nPoint	The number of points in the residual and estimate.
c    Histo	
c    Patch00,Patch11,Patch01,Patch10 Normal and spectral dirty beams.
c    minPatch)	The min and max sizes that the beam patch can take.
c    maxPatch)
c    maxNiter	The maximum total number of minor iterations permitted.
c    negStop	Stop iterating on the first negative component.
c    Cutoff
c    Gain0,Gain1 Loop gain.
c    Speed	Clean speed-up factor.
c    Run	The Run, defining the area to be cleaned.
c    nrun	The number of runs.
c    ResAMax	Absolute maximum of the residuals.
c    EstASum	Absolute sum of the estimates.
c
c  Input/Output:
c    Niter	The actual number of minor iterations performed. Updated.
c    negFound	True if a negative component was found (if negStop is true,
c		then this will stop before the negative component is added
c		to the component list).
c
c  Output:
c    Limit	Max residual that went into the residual list.
c
c  Important or Odd Thingos:
c    Ymap	When cleaning the table of residuals, we need to determine where
c		residuals corresponding to a given range of j exist.
c		Ymap(j) gives the index of the table entry before that
c		contains, or would have contained, line j residuals.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer Ymap(maxdim+1)
	integer nPatch,nCmp
c
c  Find the limiting residual that we can fit into the residual list, then
c  go and fill the residual list.
c
	call GetLimit(Res0,nPoint,ResAMax,maxCmp,Histo,maxPatch,
     *				nPatch,Limit)
	Limit = max(Limit, 0.5 * Cutoff)
	call GetComp(Res0,Res1,Est0,Est1,nPoint,ny,Ymap,Limit,
     *		Icmp,Jcmp,Rcmp0,Rcmp1,Ccmp0,Ccmp1,maxCmp,nCmp,Run,nRun)
c
c  Determine the patch size to use, perform the minor iterations, then
c  add the new components to the new estimate.
c
	nPatch = max(MinPatch/2,nPatch)
	call SubComp(nx,ny,Ymap,Patch00,Patch11,Patch01,Patch10,
     *	  maxPatch,nPatch,Gain0,Gain1,MaxNiter,negStop,1.,Speed,Limit,
     *	  EstASum,Icmp,Jcmp,RCmp0,Rcmp1,
     *	  Ccmp0,Ccmp1,Tmp,Ncmp,Niter,negFound)
c
	call NewEst(Ccmp0,Ccmp1,Icmp,Jcmp,nCmp,Est0,Est1,
     *	  nPoint,Run,nRun)
	end
c************************************************************************
	subroutine SubComp(nx,ny,Ymap,Patch00,Patch11,Patch01,Patch10,
     *	  n,PWidth,Gain0,Gain1,maxNiter,NegStop,g,Speed,Limit,
     *	  EstASum,Icmp,Jcmp,Rcmp0,Rcmp1,
     *	  Ccmp0,Ccmp1,Tmp,Ncmp,Niter,negFound)
c
	implicit none
	integer nx,ny,n,Ncmp,Niter,MaxNiter,PWidth
	real Limit,Gain0,Gain1,g,Speed,EstASum
	integer Icmp(Ncmp),Jcmp(Ncmp),Ymap(ny+1)
	real Patch00(n,n),Patch11(n,n),Patch01(n,n),Patch10(n,n)
	real Rcmp0(Ncmp),Rcmp1(Ncmp),Ccmp0(Ncmp),Ccmp1(Ncmp),Tmp(Ncmp)
	logical negStop, negFound
c
c  Perform minor iterations. This quits performing minor iterations when
c
c   ResMax < Limit * (1+ sum( |component|/(EstAMax+sum(|component|)) )
c
c  This is different to that suggested by Clark, but has the advantage
c  that it does not depend on iteration number.
c
c  Inputs:
c    Icmp,Jcmp	Coordinates of each residual peak.
c    Ncmp	Number of residual peaks.
c    Gain	Loop gain.
c    Speed	Speed up factor. A factor of zero reverts to a Clark Clean.
c    g		Yet another parameter to control the end of the minor
c		cycles.
c    negStop	Stop on the first negative peak.
c    Limit	All residuals above LIMIT are included in the residual
c		peaks.
c    MaxNiter	Maximum number of minor iterations to be performed.
c    Patch0	Autocorrelation of Beam patch.
c    Patch1	Autocorrelation of spectral beam patch.
c    Patch01	Crosscorrelation of beam and spectral beam patch.
c    Patch10	Crosscorrelation of spectral beam and beam patch.
c    n		Dimension of beam patch. This is an odd number. The patch
c		is square. The peak is at n/2+1
c    PWidth	Patch half width to be used.
c    ny		Number of rows in the residuals.
c    Ymap	When cleaning the table of residuals, we need to determine where
c		residuals corresponding to a given range of j exist.
c		Ymap(j) gives the index of the table entry before that
c		contains, or would have contained, line j residuals.
c  Scratch:
c    Tmp
c  Input/Outputs:
c    Rcmp0,Rcmp1 Flux at each residual peak.
c    Ccmp0	Component due to the Patch0 beam.
c    Ccmp1	Component due to the Patch1 beam.
c    Niter	Number of minor iterations completed.
c    EstASum	Absolute sum of the current model.
c
c  Outputs:
c    negFound	True if a negative component was found.
c
c------------------------------------------------------------------------
	integer maxrun
	parameter(maxrun=4096)
	integer c,i,i0,j0,k,ktot,ltot,NIndx
	integer Pk,p,ipk,jpk,ipkd,jpkd
	real TermRes,ResMax,Wt0,Wt1,beta,P00,P11,P01
	integer Temp(maxrun),Indx(maxrun)
	logical more
c
c  Initialise.
c
	c = n/2 + 1
	P00 = Patch00(c,c)
	P11 = Patch11(c,c)
	P01 = Patch01(c,c)
	call GetPk(Ncmp,Rcmp0,Rcmp1,P00,P11,P01,Tmp,Pk,Wt0,Wt1,ResMax)
	negFound = negFound .or. ResMax.lt.0 .or. Wt0+Ccmp0(Pk).lt.0
	TermRes = Limit
	beta = g * Limit**(Speed+1)
c
c  Loop until no more. Start with some house keeping.
c
	more = abs(ResMax).gt.TermRes .and. Niter.lt.MaxNiter .and.
     *		.not.(negStop.and.negFound)
	dowhile(more)
	  ipk = Icmp(Pk)
	  jpk = Jcmp(Pk)
	  ipkd = ipk - c
	  jpkd = jpk - c
c
c  Determine the breakup between the Patch0 and Patch1 beams.
c
	  Wt0 = gain0 * Wt0
	  Wt1 = gain1 * Wt1
c
c  Find the residuals which have suitable y values.
c
	  i = max(1, jpk-PWidth)
	  k = Ymap(i)
	  i = min(ny,jpk+PWidth)
	  ktot = Ymap(i+1)
c
c  Some more housekeeping.
c
	  EstASum = EstASum - abs(Ccmp0(pk))
	  Ccmp0(Pk) = Ccmp0(Pk) + Wt0
	  EstASum = EstASum + abs(Ccmp0(pk))
	  Ccmp1(Pk) = Ccmp1(Pk) + Wt1
c
c  Find the residuals which have suitable x values, then subtract. If the
c  beam does not cover the extent of the subimage, we have to go through
c  the process of determining which pixels are covered. This is done in
c  clunky vectorised fashion.
c
	  if(max(nx-ipk,ipk-1).gt.PWidth)then
	    do while(k.lt.ktot)
	      ltot = min(ktot-k,MaxRun)
	      do i=k+1,k+ltot
	        Temp(i-k) = abs(Icmp(i)-ipk)
	      enddo
c
	      call whenile(ltot,Temp,1,PWidth,Indx,Nindx)
c
c#ivdep
	      do i=1,Nindx
	        p = k + Indx(i)
	        i0 = Icmp(p) - ipkd
	        j0 = Jcmp(p) - jpkd
	        Rcmp0(p) = Rcmp0(p) - Wt0 * Patch00(i0,j0)
     *				    - Wt1 * Patch10(i0,j0)
		Rcmp1(p) = Rcmp1(p) - Wt0 * Patch01(i0,j0)
     *				    - Wt1 * Patch11(i0,j0)
	      enddo
	      k = k + ltot
	    enddo
c
	  else
	    do i=k+1,ktot
	      i0 = Icmp(i) - ipkd
	      j0 = Jcmp(i) - jpkd
	      Rcmp0(i) = Rcmp0(i) - Wt0 * Patch00(i0,j0)
     *				  - Wt1 * Patch10(i0,j0)
	      Rcmp1(i) = Rcmp1(i) - Wt0 * Patch01(i0,j0)
     *				  - Wt1 * Patch11(i0,j0)
	    enddo
	  endif
c
c  Ready for the next loop.
c
	  Niter = Niter + 1
	  TermRes = TermRes + 
     *	   beta * abs(Wt0) / ( EstASum * abs(ResMax)**Speed )
	  call GetPk(Ncmp,Rcmp0,Rcmp1,P00,P11,P01,Tmp,Pk,Wt0,Wt1,ResMax)
	  negFound = negFound.or.ResMax.lt.0  .or. Wt0+Ccmp0(Pk).lt.0
	  more = abs(ResMax).gt.TermRes .and. Niter.lt.MaxNiter .and.
     *		.not.(negStop.and.negFound)


	enddo
	end
c************************************************************************
	subroutine GetPk(n,R0,R1,P00,P11,P01,Tmp,Pk,Wt0,Wt1,ResMax)
c
	implicit none
	integer n,Pk
	real P00,P11,P01,R0(n),R1(n),Tmp(n),ResMax,Wt0,Wt1
c
c  Determine the location of the peak residual.
c  Input:
c    R0,R1	Residuals.
c    n		Number of residuals.
c    P00,P11,P01 Peaks in the beam.
c  Scratch:
c    Tmp	Holds the statistic used to determine the location.
c  Output:
c    Pk		Location to subtract from.
c    Wt0,WT1	Values of beams to subtract off.
c    ResMax	Current residual maximum.
c------------------------------------------------------------------------
	integer i,j
	real delta
c
c  Externals.
c
	integer ismax,ismin
c
c  Determine the optimum place to subtract flux from.
c
	do i=1,n
	  Tmp(i) = R0(i)*R0(i)*P11 + R1(i)*R1(i)*P00 - 2*R0(i)*R1(i)*P01
	enddo
c
	pk = ismax(n,Tmp,1)
c
c  Determine the maximum residual.
c
	j = ismax(n,R0,1)
	i = ismin(n,R0,1)
	if(abs(R0(i)).lt.abs(R0(j))) i = j
	ResMax = abs(R0(i))
c
	delta  = 1./(P00*P11 - P01*P01)
	Wt0 = (P11*R0(pk) - P01*R1(pk))*delta
	Wt1 = (P00*R1(pk) - P01*R0(pk))*delta
c
c  If the spectral component is more than 5 times the flux component,
c  trim back the spectral component, and see whether it would be
c  better to subtract from the residual peak instead.
c
c	if(abs(Wt1).gt.100*abs(Wt0))then
c	  Wt1 = sign(5*Wt0,Wt1)
c	  twt0 = (P11*R0(i) - P01*R1(i))*delta
c	  twt1 = (P00*R1(i) - P01*R0(i))*delta
c	  twt1 = sign(5*twt0,twt1)
c	  if(twt0*R0(i)+twt1*R1(i).gt.Wt0*R0(pk)+Wt1*R1(pk))then
c	    Wt0 = twt0
c	    Wt1 = twt1
c	    pk = i
c	  endif
c	endif
c
	end
c************************************************************************
	subroutine GetLimit(Res0,nPoint,ResAMax,maxCmp,
     *		Histo,MaxPatch,nPatch,Limit)
c
	implicit none
	integer nPoint,maxPatch,nPatch,maxCmp
	real Res0(nPoint),ResAMax,Histo(maxPatch/2+1),Limit
c
c  Determine the limiting threshold and the patch size. The algorithm
c  used to determine the Limit and patch size are probably very
c  important to the run time of the program. Presently, however, the
c  algorithm is fairly simple.
c
c  Inputs:
c    Res0	The input residuals.
c    nPoint	Number of residuals.
c    ResAMax	The absolute maximum residual.
c    maxCmp	The maximum number of peak residuals that can be stored.
c    Histo	Histogram of the beam patch.
c    maxPatch	Width of beam patch.
c
c  Outputs:
c    Limit	Threshold above which residual points are to be chosen.
c    nPatch	Half width of beam patch.
c
c------------------------------------------------------------------------
	integer HistSize
	parameter(HistSize=512)
	integer i,m,Acc
	real ResAMin,a,b,x
	integer ResHis(HistSize)
c
c  Initialise the histogram array, as well as other stuff.
c
	ResAMin = ResAMax * Histo(maxPatch/2+1) / Histo(1)
	a = (HistSize-2)/(ResAMax-ResAMin)
	b = 2 - a * ResAMin
	do i=1,HistSize
	  ResHis(i) = 0
	enddo
c
c  Now get the histogram while taking account of the boxes.
c
	do i=1,nPoint
	  m = max(int(a * abs(Res0(i)) + b),1)
	  ResHis(m) = ResHis(m) + 1
	enddo
c
c  Now work out where to set the limit.
c
	Acc = 0
	m = HistSize + 1
	do while(Acc.le.maxCmp)
	  m = m - 1
	  if(m.eq.0)then
	    Acc = maxCmp + 1
	  else
	    Acc = Acc + ResHis(m)
	  endif
	enddo
	m = m + 1
	Limit = (m-b)/a
c
c  Now work out what the corresponding beam patch size is.
c
	nPatch = 1
	x = Limit / ResAMax
	do while(nPatch.lt.maxPatch/2.and.x.lt.Histo(nPatch))
	  nPatch = nPatch + 1
	enddo
c
	end
c************************************************************************
	subroutine GetComp(Res0,Res1,Est0,Est1,nPoint,ny,Ymap,Limit,
     *		           Icmp,Jcmp,Rcmp0,Rcmp1,Ccmp0,Ccmp1,
     *			   maxCmp,nCmp,Run,nRun)
c
	implicit none
	integer nPoint,ny,maxCmp,nCmp,nRun,Run(3,nrun)
	real Limit
	real Res0(nPoint), Res1(nPoint), Est0(nPoint), Est1(nPoint)
	real Rcmp0(maxCmp),Rcmp1(maxCmp),Ccmp0(maxCmp),Ccmp1(maxCmp)
	integer Ymap(ny+1),Icmp(maxCmp),Jcmp(maxCmp)
c
c  Get the residuals that are greater than a certain cutoff.
c
c  Inputs:
c    nx,ny	Size of the residual map.
c    maxCmp	Max number of components that are possible.
c    Limit	Threshold above which components are to be taken.
c    Run	Runs specifications.
c    nrun	Number of runs.
c    Res0,Res1	Contains all the residuals.
c
c  Outputs:
c    Ncmp	Number of residual peaks returned.
c    Ymap	Ymap(j) gives the index, in Icmp,Jcmp,Residual of the last
c		residual peak such that Jcmp .lt. j. See SubComp.
c    Icmp,Jcmp	Array of the indices of the residual peaks.
c    Rcmp0,Rcmp1 Array if the residual peaks.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k,l,Ncmpd,x0,y0,n0,itemp
	real Temp(maxdim)
	integer Indx(maxdim)
c
c  Clear the mapping array.
c
	do j=1,ny+1
	  Ymap(j) = 0
	enddo
c
c  Loop around finding the residuals greater, in absolute value, than
c  LIMIT. Copy these to the residuals table.
c
	Ncmp = 0
	l = 0
	do k=1,nrun
	  y0 = Run(1,k)
	  x0 = Run(2,k) - 1
	  n0 = Run(3,k) - x0
c
	  do i=1,n0
	    Temp(i) = abs(Res0(l+i))
	  enddo
	  call whenfgt (n0, Temp, 1, Limit, Indx,Ncmpd)
	  if(Ncmp+Ncmpd.gt.maxCmp)
     *		call bug('f','Internal bug in GetComp')
c
          do i = 1, Ncmpd
            Rcmp0(i+Ncmp) = Res0(l+Indx(i))
            Rcmp1(i+Ncmp) = Res1(l+Indx(i))
	    Ccmp0(i+Ncmp) = Est0(l+Indx(i))
	    Ccmp1(i+Ncmp) = Est1(l+Indx(i))
	    Icmp(i+Ncmp) = x0 + Indx(i)
	    Jcmp(i+Ncmp) = y0
          enddo
	  l = l + n0
          Ncmp = Ncmp + Ncmpd
	  Ymap(y0) = Ymap(y0) + Ncmpd
        enddo
c
c  Ymap currently contains the number of residuals found in a particular
c  row. Convert this so that Ymap(j) gives the total number of peak residuals
c  in rows 1 to j-1. This loop will probably not vectorise.
c
	Ncmp = 0
	do j=1,ny+1
	  itemp = Ncmp
	  Ncmp = Ncmp + Ymap(j)
	  Ymap(j) = itemp
	enddo
c
c  If no components were found, stop; this means that user has
c  probably specified CLEAN boxes outside the bulk of the emission
c
	if(Ncmp.eq.0)call bug('w','No peak residuals found in GETCOMP')
c
	end
c************************************************************************
	subroutine NewEst(Ccmp0,Ccmp1,Icmp,Jcmp,Ncmp,Est0,Est1,
     *	  nPoint,Run,nRun)
c
	implicit none
	integer nCmp,nPoint,nRun
	integer ICmp(nCmp),JCmp(nCmp),Run(3,nRun)
	real CCmp0(nCmp),CCmp1(nCmp),Est0(nPoint),Est1(nPoint)
c
c  This adds the components in CCmp0,CCmp1 to the components in Est0
c  and Est1. The components in the two arrays are, unfortunately, stored in
c  very different ways, CCmp having associated arrays containing indices, while
c  Est0/Est1 is described by run specifications.
c
c  Inputs:
c    nCmp	Number of components.
c    ICmp,JCmp	Coordinates of components in CCmp.
c    CCmp0,CCmp1 Values of the component.
c    nPoint	Number of points in the estimate.
c    Run	Run specifications describing components in Estimate.
c    nRun	Number of run specifications.
c
c  Input/Output:
c    Est0	All the components.
c    Est1
c------------------------------------------------------------------------
	integer i,j,k,l
c
c  Vectorise this if you can!
c
	j = 1
	k = 1
	do l=1,nCmp
	  dowhile(JCmp(l).gt.Run(1,k).or.ICmp(l).gt.Run(3,k))
	    j = j + Run(3,k) - Run(2,k) + 1
	    k = k + 1
	  enddo
	  i = ICmp(l) - Run(2,k) + j
	  Est0(i) = CCmp0(l)
	  Est1(i) = CCmp1(l)
	enddo
	end
c************************************************************************
	subroutine Diff(Est0,Est1,Map0,Map1,Res0,Res1,Tmp,
     *	  nPoint,nx,ny,Run,nRun,FFT00,FFT11,FFT01,FFT10)
c
	implicit none
	integer nPoint,nx,ny,nRun,Run(3,nRun)
	real Est0(nPoint),Est1(nPoint),Res0(nPoint),Res1(nPoint)
	real Map0(nPoint),Map1(nPoint),Tmp(nPoint)
	integer FFT00,FFT11,FFT01,FFT10
c
c  Determine the residuals left.
c
c  Input:
c    Est0,Est1	Clean component estimates.
c    Map0,Map1	Maps.
c    nx,ny	Bounding box size.
c    Run	The runs specification.
c    nRun	Number of runs.
c    nPoint	Number of pixels in the runs.
c    FFT00,FFT11,FFT01,FFT10 FFTs of the beams of interest.
c  Output:
c    Res0,Res1	Residuals.
c  Scratch:
c    Tmp
c------------------------------------------------------------------------
	integer i
c
	call CnvlR(FFT00,Est0,nx,ny,Run,nRun,Tmp,'c')
	call CnvlR(FFT10,Est1,nx,ny,Run,nRun,Res0,'c')
c
	do i=1,nPoint
	  Res0(i) = Map0(i) - Tmp(i) - Res0(i)
	enddo
c
	call CnvlR(FFT01,Est0,nx,ny,Run,nRun,Tmp,'c')
	call CnvlR(FFT11,Est1,nx,ny,Run,nRun,Res1,'c')
c
	do i=1,nPoint
	  Res1(i) = Map1(i) - Tmp(i) - Res1(i)
	enddo
c
	end
