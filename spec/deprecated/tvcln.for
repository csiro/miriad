c************************************************************************
	program tvcln
	implicit none
c
c= tvcln - Clark/Steer CLEAN with display of intermediates
c& rjs mchw
c: deconvolution
c+
c	TVCLN is a MIRIAD task, which performs a hybrid Clark/Steer Clean
c	algorithm, which takes a dirty map and beam, and produces an output
c	map which consists of the Clean components. This output can be
c	input to SELFCAL to self-calibrate visibilities, or input to RESTOR
c	to produce a "clean" image. Optionally CLEAN can take as one of
c	its inputs a model of the deconvolved image. This model could be
c	from a previous CLEAN run, or from any of the other deconvolution
c	tasks (e.g. MAXEN). 
c@ map
c	The input dirty map, which should have units of Jy/beam. No
c	default. 
c@ beam
c	The input dirty beam. No default
c@ model
c	An initial model of the deconvolved image. This could be the
c	output from a previous run of CLEAN, or the output of any of the
c	deconvolution tasks (e.g. MAXEN). It must have flux units of
c	Jy/pixel. The default is no model (i.e. a zero map). 
c@ out
c	The name of the output map. The units of the output will be
c	Jy/pixel. It can be input to RESTOR, CLEAN (as a model, to do
c	more cleaning), or to SELFCAL (for self-calibrating visibility
c	data). 
c@ gain
c	The minor iteration loop gain. Default is 0.1.
c@ cutoff
c	CLEAN finishes when the absolute maximum residual falls below
c	CUTOFF. Default is 0. 
c@ niters
c	The maximum number of minor iterations. Clean finishes when
c	abs(NITERS) minor iterations have been performed. Clean may finish
c	before this point, however, if NITERS is negative and the absolute
c	maximum residual becomes negative valued, or if the cutoff level
c	(as described above) is reached. 
c@ region
c	This specifies the region to be Cleaned. The default is the largest,
c	centered region that it is safe to deconvolve.
c@ phat
c	Cornwells prussian hat parameter. When cleaning extended sources,
c	CLEAN may produce a badly corrugated image. This can be suppressed
c	to some extent by cleaning with a dirty beam which has had a spike
c	added at its center (i.e. a beam that looks like a prussian hat).
c	PHAT gives the value of this spike, with 0 to 0.5 being good
c	values. Default is 0 (but use a non-zero value for extended
c	sources). 
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
c	algorithm Usually this will not be needed unless you are having
c	problems with corrugations. A positive value can be useful when
c	cleaning simple point-like sources. Default is 0. 
c@ mode
c	This can be either "hogbom", "clark", "steer" or "any", and
c	determines the Clean algorithm used. If the mode is "any", then
c	CLEAN determines which is the best algorithm to use. The default
c	is "any". 
c@ clip
c	This sets the relative clip level in a Steer clean, values
c	typically being 0.75 to 0.9. The default is image dependent. 
c@ server
c	If this is set to the name of a TV device, then at the end of
c	each major cycle, the restored image is displayed on the TV.
c--
c
c  History:
c    rjs   Nov89 - Original version.
c    rjs  9jun89 - Call sequences to GETPLANE and PUTPLANE has changed.
c		   Subroutine DIFF included in this file. Headers improved.
c    rjs 27jun89 - Reincarnated as TVCLN, which included additions for
c		   displaying on the TV and control panel interaction.
c    rjs 16jul89 - Fudges for the SIGGRAPH demo.
c    rjs 27jul89 - More fudges.
c    rjs 29jul89 - Goes into interactive mode only if a Clark iteration
c		   was performed.
c    rjs 27oct89 - Undid some SIGGRAPH fudges.
c    rjs 30apr90 - Changed call sequence to BoxInput.
c    pjt  2may90 - maxchan -> mxchan because of new maxdim.h
c   mchw 11jul90 - Added header keywords and increased length of filenames.
c   mchw 17jul90 - Added code to find beamsize if needed.
c     jm 17jul90 - Changes for pan routines and call TvFlush.
c   mchw 09nov90 - Added pbfwhm to map header.
c   mjs  25feb91 - Changed references of itoa to itoaf, mitoa to mitoaf.
c   mjs  15mar92 - Changed include file name restore.h to restor.h
c    jm  04jul92 - Added call to tvscale in routine Display.
c   rjs  24dec92 - Doc change only.
c   rjs  05jan93 - Doc changes only.
c   rjs  10feb93 - Get rid of maxdim**2 arrays (use memalloc). Copy restor.h
c		   to tvcln.h.
c   rjs  10sep96 - Get rid fo tvclncom common block.
c   rjs  29jan97 - Change default region of interest.
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
	parameter(version='TvCln: version 1.0 29-Jan-97')
	include 'maxdim.h'
	integer MaxBeam,maxCmp1,maxCmp2,MaxBox,MaxRun,MaxP
	parameter(maxCmp1=66000,MaxCmp2=32000,MaxP=257)
	parameter(MaxBeam=MaxP*MaxP,MaxBox=1024,MaxRun=3*maxdim)
c
	integer Boxes(MaxBox),Run(3,MaxRun)
	real RCmp(MaxCmp2),CCmp(maxCmp2)
	real Histo(MaxP/2+1),BemPatch(MaxBeam)
	integer ICmp(maxCmp1),JCmp(maxCmp1)
c
	integer MaxMap,MaxFFT
	integer Map,Estimate,Residual,Restore,BemFFT,DMCFFT
c
	character Mode*8,Moded*8,Text*7,server*64
	real Cutoff,Gain,Phat,Speed,Clip,defClip,Limit,bmin,bmax
	logical NegStop,NegFound,More,FFTIni,doInter,doCtrl,doClark
	integer MaxNiter,Niter,minPatch,maxPatch
	integer naxis,n1,n2,icentre,jcentre,nx,ny
	integer blc(3),trc(3),xmin,xmax,ymin,ymax
	integer k,nRun,nPoint
	character MapNam*64,BeamNam*64,ModelNam*64,OutNam*64,line*72
	integer lMap,lBeam,lModel,lOut
	integer nMap(3),nBeam(3),nModel(3),nOut(3)
	real EstASum
	real ResMin,ResMax,ResAMax,ResRms
	real fwhm1,fwhm2,bpa
c
c  Dynamic memory array.
c
	real ref(MAXBUF)
	common ref
c
c  Externals.
c
	character itoaf*8
c
c  Get the input parameters.
c
	call output(version)
	call inputs(MapNam,BeamNam,ModelNam,OutNam,MaxNiter,NegStop,
     *	  Cutoff,Boxes,MaxBox,MinPatch,Gain,PHat,Speed,Clip,mode,server)
c
c  Determine if we have a control panel and a TV device.
c
	call InterIni(server,doCtrl,doInter)
c
c  Open the beam, get some characteristics about it, then read in the
c  beam patch. The beam patch is not required if we are performing Steer
c  iterations only. However, some of the statistics returned by BeamChar
c  are required.
c
	call xyopen(lBeam,BeamNam,'old',2,nBeam)
	n1 = nBeam(1)
	n2 = nBeam(2)
	if(max(n1,n2).gt.maxdim)call bug('f','Beam is too big')
	FFTIni = .false.
c
	maxPatch = min(MaxP,2*((min(n1,n2)-1)/2) + 1)
	if(maxPatch.le.0) call bug('f','Bad patch size')
	call BeamChar(lBeam,n1,n2,icentre,jcentre,Histo,maxPatch)
	maxPatch = min(maxPatch,
     *		    2*min(icentre-1,n1-icentre,jcentre-1,n2-jcentre)+1)
	if(abs(Histo(1)-1).gt.0.001)
     *		    call bug('f','Beam maxima is not 1')
	if(mode.eq.'steer'.or.mode.eq.'any')then
	  defClip = 0.2*Histo(1) + 0.8*Histo(2)
	  if(Clip.eq.0)then
	    Clip = defClip
	  else if(Clip.lt.defClip)then
	    call bug('w','Clip level seems small')
	  endif
	  write(line,'(a,f6.3)')'Steer Clip Level:',Clip
	  call output(line)
	endif
	if(mode.ne.'steer'.and.mode.ne.'hogbom'.and.doInter)
     *	    call BeamSize(lBeam,n1,n2,icentre,jcentre,fwhm1,fwhm2,bpa)
	if(mode.ne.'steer')
     *	    call GetPatch(lBeam,BemPatch,maxPatch,PHat,icentre,jcentre)
c
c  Open the map, and determine the area being cleaned.
c
	call xyopen(lMap,MapNam,'old',3,nMap)
	call rdhdi(lMap,'naxis',naxis,3)
	naxis = min(naxis,3)
	call defregio(boxes,nMap,nBeam,icentre,jcentre)
	call BoxMask(lMap,boxes,maxbox)
	call BoxSet(boxes,3,nMap,' ')
	call BoxInfo(boxes,3,blc,trc)
	nOut(1) = trc(1) - blc(1) + 1
	nOut(2) = trc(2) - blc(2) + 1
	nOut(3) = trc(3) - blc(3) + 1
	if(nOut(1).gt.n1.or.nOut(2).gt.n2)
     *	  call bug('f','Region of map to deconvolve is too big')
	if(2*nOut(1)-1.gt.n1.or.2*nOut(2)-1.gt.n2)
     *	  call bug('w','Size of region of map to deconvolve is unsafe')
	call ImMinMax(lMap, naxis, nMap, bmin, bmax)
c
c  Open the model if there is one. Note that currently the model must agree
c  exactly in size with the output map (an unfortunate restriction.
c
	if(ModelNam.ne.' ')then
	  call xyopen(lModel,ModelNam,'old',3,nModel)
	  if(nOut(1).ne.nModel(1).or.nOut(2).ne.nModel(2).or.
     *	     nout(3).ne.nModel(3))
     *	     call bug('f','Model and output size do not agree')
	endif
c
c  Open the output.
c
	call xyopen(lOut,OutNam,'new',naxis,nOut)
c
c  Allocate memory.
c
	MaxMap = nOut(1)*nOut(2)
	MaxFFT = (n1/2+1)*n2
	call memalloc(Map,MaxMap,'r')
	call memalloc(Estimate,MaxMap,'r')
	call memalloc(Residual,MaxMap,'r')
	call memalloc(Restore,MaxMap,'r')
	call memalloc(BemFFT,MaxFFT,'r')
	call memalloc(DMCFFT,MaxFFT,'r')
c
c  Loop over all the planes of interest.
c
	do k=blc(3),trc(3)
	  if(blc(3).ne.trc(3))call output('Plane: '//itoaf(k))
c
c  Get the Map, Estimate and Residual.
c
	  call BoxRuns(1,k,'r',boxes,Run,MaxRun,nRun,
     *					xmin,xmax,ymin,ymax)
	  nx = xmax - xmin + 1
	  ny = ymax - ymin + 1
c
	  call xysetpl(lMap,1,k)
	  call GetPlane(lMap,Run,nRun,xmin-1,ymin-1,nMap(1),nMap(2),
     *				ref(Map),MaxMap,nPoint)
c
c  Determine the CLEAN algorithm that is to be used.
c
	  moded = mode
	  if((mode.eq.'any'.or.mode.eq.'hogbom').and.
     *	    nPoint.le.maxCmp1.and.
     *	    (2*nx-1).le.maxPatch.and.(2*ny-1).le.maxPatch)then
	    moded = 'hogbom'
	  else if(mode.eq.'hogbom')then
	    call bug('w','Cannot use Hogbom algorithm -- using Clark')
	    moded = 'clark'
	  else
	    moded = mode
	  endif
c
c  Initialise the FFT of the beam if needed.
c
	  if((moded.ne.'hogbom'.or.ModelNam.ne.' ').and..not.FFTIni)then
	    FFTIni = .true.
	    call ConvlIni(lBeam,ref(BemFFT),n1,n2,PHat,icentre,jcentre)
	    if(moded.ne.'steer'.and.moded.ne.'hogbom'.and.doInter)
     *		call GetDMC(lBeam,ref(BemFFT),ref(DMCFFT),n1,n2)
	  endif
c
c  Initialise the estimate, and determine the residuals if the the user
c  gave an estimate. Determine statistics about the estimate and the
c  residuals.
c
	  if(ModelNam.eq.' ')then
	    EstASum = 0
	    call CopyMap(ref(Map),ref(Residual),ref(Estimate),nPoint)
	  else
	    call xysetpl(lModel,1,k-blc(3)+1)
	    call GetPlane(lModel,Run,nRun,xmin-blc(1),ymin-blc(2),
     *			nModel(1),nModel(2),ref(Estimate),MaxMap,nPoint)
	    call Diff(ref(Estimate),ref(Map),ref(Residual),nPoint,nx,ny,
     *		Run,nRun,ref(BemFFT),n1,n2)
	    call GetAbsEs(ref(Estimate),nPoint,EstASum)
	  endif
	  call Stats(ref(Residual),nPoint,ResMin,ResMax,ResAMax,ResRms)
c
c  Perform the appropriate iteration until no more.
c
	  Niter = 0
	  negFound = .false.
	  More = nPoint.gt.0
	  Limit = 0
	  dowhile(More)
	    if(moded.eq.'hogbom')then
	      call Hogbom(MaxPatch,BemPatch,nx,ny,ref(Residual),
     *		ref(Estimate),ICmp,JCmp,nPoint,Run,nRun,EstASum,
     *		Cutoff,Gain,negStop,negFound,MaxNiter,Niter)
	      text = ' Hogbom'
	      doClark = .false.
	    else if(moded.eq.'steer')then
	      call Steer(ref(Residual),ref(Estimate),ref(Map),
     *		ref(BemFFT),nPoint,nx,ny,Clip*ResAMax,Gain,n1,n2,
     *		Niter,Run,nRun)
	      text = ' Steer'
	      doClark = .false.
	    else
	      call Clark(nx,ny,ref(Residual),ref(Estimate),nPoint,
     *		Run,nRun,Histo,BemPatch,minPatch,maxPatch,Cutoff,
     *		negStop,MaxNiter,Gain,Speed,ResAMax,EstASum,Niter,Limit,
     *		negFound,RCmp,CCmp,ICmp,JCmp,maxCmp2)
	      call Diff(ref(Estimate),ref(Map),ref(Residual),nPoint,
     *		nx,ny,Run,nRun,ref(BemFFT),n1,n2)
	      text = ' Clark'
	      doClark = .true.
	      if(moded.eq.'any'.and.(Limit/ResAMax).gt.Clip)
     *						moded = 'steer'
	    endif
c
c  Output some messages to assure the user that the Cray has not crashed.
c
	    call output(Text//' Iterations: '//itoaf(Niter))
	    call Stats(ref(Residual),nPoint,ResMin,ResMax,
     *	      ResAMax,ResRms)
	    write(line,'(a,1p3e12.3)')' Residual min,max,rms: ',
     *				      ResMin,ResMax,ResRms
	    call output(line)
c
c  If we are going Steer iterations, see if the next iteration would run
c  into negative components.
c
	    if(moded.eq.'steer')
     *	      negFound = negFound.or.(-Clip*ResAMax.gt.ResMin)
c
c  Check for convergence.
c
	    more = .not.((negFound.and.negStop)
     *			.or.ResAMax.le.Cutoff.or.Niter.ge.MaxNiter)
c
	    if(nx*ny.eq.nPoint.and.doInter.and.doClark)then
	      call Diff(ref(Estimate),ref(Map),ref(Restore),
     *		nPoint,nx,ny,Run,nRun,ref(DMCFFT),n1,n2)
	      call Interact(ref(Estimate),ref(Residual),ref(Restore),
     *		nx,ny,bmin,bmax,more,doCtrl)
	    endif
	  enddo
c
c  Give a message about what terminated the iterations.
c
	  if(nPoint.eq.0)then
	    call output(' No region selected in this plane')
	  else if(ResAMax.le.Cutoff)then
	    call output(' Stopping -- Clean cutoff limit reached')
	  else if(Niter.ge.MaxNiter)then
	    call output(' Stopping -- Maximum iterations performed')
	  else if(NegStop.and.NegFound)then
	    call output(' Stopping -- Negative components encountered')
	  else
	    call output(' Stopped at users request')
	  endif
c
c  Write out this plane.
c
	  call xysetpl(lOut,1,k-blc(3)+1)
	  call PutPlane(lOut,Run,nRun,xmin-blc(1),ymin-blc(2),
     *	    nOut(1),nOut(2),ref(Estimate),nPoint)
	enddo
c
c  Deallocate memory.
c
	call memfree(Map,MaxMap,'r')
	call memfree(Estimate,MaxMap,'r')
	call memfree(Residual,MaxMap,'r')
	call memfree(Restore,MaxMap,'r')
	call memfree(BemFFT,MaxFFT,'r')
	call memfree(DMCFFT,MaxFFT,'r')
c
c  Construct a header for the output file, and give some history
c  information.
c
	call Header(lMap,lOut,blc,trc,Niter,clip,mode,minpatch,
     *							version)
c
c  Close up the files. Ready to go home.
c
	call xyclose(lMap)
	call xyclose(lBeam)
	if(doInter) call tvclose
	if(ModelNam.ne.' ')call xyclose(lModel)
	call xyclose(lOut)
c
c  Thats all folks.
c
	end
c************************************************************************
	subroutine Header(lIn,lOut,blc,trc,Niter,
     *	  clip,mode,minpatch,version)
c
	implicit none
	integer lIn,lOut,Niter,minpatch,blc(3),trc(3)
	real clip
	character mode*(*),version*(*)
c
c Copy across the header to the model.
c
c  Inputs:
c    lIn
c    lOut
c    blc
c    trc
c    Niter
c    clip
c    mode
c    minPatch
c    version
c------------------------------------------------------------------------
	integer i,lblc,ltrc
	real crpix1,crpix2,crpix3
	character line*72,txtblc*32,txttrc*32
	integer nkeys
	parameter(nkeys=32)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *	  'crpix4  ','crval1  ','crval2  ','crval3  ','crval4  ',
     *		     'ctype1  ','ctype2  ','ctype3  ','ctype4  ',
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
	call rdhdr(lIn,'crpix3',crpix3,1.)
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
	  call hdcopy(lIn, lOut, keyw(i))
	enddo
c
c  Write crap to the history file, to attempt (ha!) to appease Neil.
c
	call hisopen(lOut,'append')
	line = 'CLEAN: Miriad '//version
        call hiswrite(lOut,line)
	call hisinput(lOut,'CLEAN')
c
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	line = 'CLEAN: Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'
	call hiswrite(lOut,line)
c
	if(mode.eq.'steer'.or.mode.eq.'any')then
	  write(line,'(''CLEAN: Steer Clip Level ='',f6.3)')Clip
	  call hiswrite(lOut,line)
	endif	    
	call hiswrite(lOut,'CLEAN: Minpatch = '//itoaf(minpatch))
	call hiswrite(lOut,'CLEAN: Total Iterations = '//itoaf(Niter))
	call hisclose(lOut)
c
	end
c************************************************************************
	subroutine GetAbsEs(Estimate,nPoint,EstASum)
c
	implicit none
	integer nPoint
	real Estimate(nPoint),EstASum
c
c------------------------------------------------------------------------
	integer i
c
	EstASum = 0
	do i=1,nPoint
	  EstASum = EstASum + abs(Estimate(i))
	enddo
c
	end
c************************************************************************
	subroutine CopyMap(Map,Residual,Estimate,nPoint)
c
	implicit none
	integer nPoint
	real Map(nPoint),Residual(nPoint),Estimate(nPoint)
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,nPoint
	  Estimate(i) = 0.
	  Residual(i) = Map(i)
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
	subroutine GetPatch(lBeam,Patch,maxPatch,PHat,ic,jc)
c
	implicit none
	integer lBeam,maxPatch,ic,jc
	real Patch(maxPatch,maxPatch),PHat
c
c  Read in the central portion of the beam.
c
c  Inputs:
c    lBeam	Handle of the file containing the beam.
c    maxPatch	The full width of the patch to be read in.
c    ic,jc	Location of the centre pixel of the beam.
c    PHat	Prussian hat.
c 
c  Output:
c    Patch	The read in central portion of the beam.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer imin,imax,jmin,jmax,i,j
	real Data(maxdim)
c
	imin = ic - maxPatch/2
	imax = imin + maxPatch - 1
	jmin = jc - maxPatch/2
	jmax = jmin + maxPatch - 1
c
	do j=jmin,jmax
	  call xyread(lBeam,j,Data)
	  do i=imin,imax
	    Patch(i-imin+1,j-jmin+1) = Data(i)
	  enddo
	enddo
c
	Patch(ic-imin+1,jc-jmin+1) = Patch(ic-imin+1,jc-jmin+1) + PHat
c
	end
c************************************************************************
	subroutine inputs(map,beam,estimate,out,Niter,negStop,
     *	  cutoff,box,maxbox,minpatch,gain,phat,speed,clip,mode,server)
c
	implicit none
	integer Niter, minpatch, maxbox
	integer box(maxbox)
	real cutoff,gain,phat,speed,clip
	logical negStop
	character map*(*),beam*(*),estimate*(*),out*(*),mode*(*)
	character server*(*)
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
c      Gain        Clean loop gain. Default 0.5 (a bit high).
c      Cutoff)     The iterations stop when either the absolute max residual
c      Niter )    remaining is less than Cutoff, of Niter minor iterations
c                  have been performed (whichever comes first). The default
c		   Cutoff is 0 (i.e. iterate forever), and the default number
c	           of minor iterations is 250.  This is the total
c                  number of iterations to do.
c      negStop	   Stop on first negative component.
c      minpatch    The minimum beam patch width.
c      phat	   Prussian hat. Default is 0.
c      speed       Speedup factor. Default is 0.
c      box	   The boxes specification.
c      clip	   The Steer clip level.
c      mode	   Either "clark" (default), "steer" or "any".
c      server	   The name of the display server.
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
	call keyr ('gain', gain, 0.1)
	if(gain.le.0.or.gain.ge.1)
     *	  call bug('f','Gain must be in the range (0,1)')
	call keyr ('phat', phat, 0.0)
	call keyr ('speed', speed, 0.0)
	call keyr ('clip', clip, 0.)
	if(clip.lt.0.or.clip.gt.1)
     *	  call bug('f','Bad clip value, it must be in the range [0,1]')
	call keya('mode',mode,'any')
	if(mode.ne.'clark'.and.mode.ne.'steer'.and.mode.ne.'any'.and.
     *	  mode.ne.'hogbom') call bug('f','Bad value for mode')
	call keya('server',server,' ')
	call keyfin
c
        end
c************************************************************************
	subroutine BeamChar(lBeam,n1,n2,ic,jc,Histo,maxPatch)
c
	implicit none
	integer lBeam,n1,n2,ic,jc,maxPatch
	real Histo(maxPatch/2+1)
c
c  Determine the location of the max value in the beam, and an array
c  giving the max abs value of the beam outside a particular area.
c
c  Input:
c    lBeam	Handle of the file containing the beam.
c    n1,n2	Beam size.
c    maxPatch	Maximum patch size.
c
c  Output:
c    ic,jc	Pixel coordinate of the max pixel.
c    Histo	Histo(i) is the absolute maximum outside the patch
c		with width 2*i-1 around the beam maximum.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k,imin,imax,jmin,jmax,nHisto
	real Data(maxdim),bmax
c
c  External.
c
	integer isamax,ismax
c
c  Initialise.
c
	nHisto = maxPatch/2 + 1
c
c  Determine the value and location of the beam maximum.
c
	bmax = 0
	do j=1,n2
	  call xyread(lBeam,j,Data)
	  i = ismax(n1,Data,1)
	  if(j.eq.1.or.Data(i).gt.bmax)then
	     bmax = Data(i)
	     ic = i
	     jc = j
	  endif
	enddo
c
c  Initialise the "histo" array.
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
	do j=1,n2
	  call xyread(lBeam,j,Data)
	  if(j.lt.jmin.or.j.gt.jmax)then
	    i = isamax(n1,Data,1)
	    Histo(nHisto) = max(abs(Data(i)),Histo(nHisto))
	  else
	    if(imin.gt.1)then
	      i = isamax(imin-1,Data,1)
	      Histo(nHisto) = max(abs(Data(i)),Histo(nHisto))
	    endif
	    if(imax.lt.n1)then
	      i = isamax(n1-imax,Data(imax+1),1) + imax
	      Histo(nHisto) = max(abs(Data(i)),Histo(nHisto))
	    endif
c
	    do i=imin,imax
	      k = max(abs(i-ic),abs(j-jc)) + 1
	      Histo(k) = max(Histo(k),abs(Data(i)))
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
	subroutine Steer(Residual,Estimate,Temp,Beam,nPoint,nx,ny,
     *		Limit,Gain,n1,n2,Niter,Run,nrun)
c
	implicit none
	integer Niter,nrun,Run(3,nrun),nPoint,nx,ny
	real Gain,Limit
	real Residual(nPoint),Temp(nPoint),Estimate(nPoint),Beam(*)
	integer n1,n2
c
c  Perform an iteration of the Steer "Clean".
c
c  Input:
c    Run	The Run specifying the area of interest.
c    nrun	The number of runs. A null run follows run number nrun.
c    limit	The limit to cut at.
c    clip	Steer clip level.
c
c  Input/Output:
c    Niter	Number of Niter iterations.
c
c------------------------------------------------------------------------
	integer i
	real SumRE,SumEE,g
c
c  Form the new Steer estimate.
c
	do i=1,nPoint
	  if(abs(Residual(i)).gt.Limit)then
	    Temp(i) = Residual(i)
	    Niter = Niter + 1
	  else
	    Temp(i) = 0.
	  endif
	enddo
c
c  Convolve it with the dirty beam.
c
	call Convl(Temp,Temp,nPoint,nx,ny,Run,nRun,Beam,n1,n2)
c
c  "Temp" now contains the new estimate convolved with the dirty beam.
c  Determine the optimum gain to apply to minimise the residuals.
c  However apply the users damping factor to this gain.
c
	SumRE = 0
	SumEE = 0
	do i=1,nPoint
	  SumRE = SumRE + Residual(i)*Temp(i)
	  SumEE = SumEE + Temp(i)*Temp(i)
	enddo
	g = Gain*SumRE/SumEE
c
c  Now go through and update the estimate, using this gain. Also
c  determine the new residuals.
c
	do i=1,nPoint
	  if(abs(Residual(i)).gt.Limit)
     *		Estimate(i) = Estimate(i) + g * Residual(i)
	  Residual(i) = Residual(i) - g * Temp(i)
	enddo
c
	end
c************************************************************************
	subroutine Hogbom(n,Patch,nx,ny,RCmp,CCmp,ICmp,JCmp,nCmp,
     *	  Run,nRun,EstASum,Cutoff,Gain,negStop,negFound,MaxNiter,Niter)
c
	implicit none
	integer nx,ny,nCmp,nRun,n
	integer ICmp(nCmp),JCmp(nCmp),Run(3,nRun)
	real RCmp(nCmp),CCmp(nCmp),Patch(n,n)
	real Cutoff,Gain,EstASum
	logical negStop,negFound
	integer MaxNiter,Niter
c
c  Perform a Hogbom Clean.
c
c  Inputs:
c    Patch	The beam.
c    n		Size of the beam.
c    nx,ny	Size of the input image.
c    nCmp	Number of pixels.
c    maxNiter	Maximum number of iterations to be performed.
c    negStop	Stop when a negative component is encountered.
c    Gain	Loop gain.
c    Cutoff	Stop when the residuals fall below the cutoff.
c    Run(3,nRun) This specifices the runs of the input image that are to
c		be processed.
c
c  Input/Output:
c    RCmp	Residuals.
c    CCmp	The image estimate.
c    Niter	Number of iterations to be performed.
c    EstASum	Sum of the absolute value of the estimate.
c    negFound	Set true if a negative component was encountered.
c
c  Scratch:
c    ICmp	Coordinate in x of a pixel.
c    JCmp	Coordinate in y of a pixel.
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
	call SubComp(nx,ny,Ymap,Patch,n,n/2,Gain,MaxNiter,NegStop,
     *	  0.,0.,Cutoff,EstASum,Icmp,Jcmp,Rcmp,Ccmp,Ncmp,Niter,negFound)
	end
c************************************************************************
	subroutine Clark(nx,ny,Residual,Estimate,nPoint,Run,nRun,
     *		Histo,Patch,minPatch,maxPatch,Cutoff,negStop,MaxNiter,
     *		Gain,Speed,ResAMax,EstASum,Niter,Limit,negFound,
     *		RCmp,CCmp,ICmp,JCmp,maxCmp)
c
	implicit none
	integer nx,ny,minPatch,maxPatch,maxNiter,Niter,nRun,nPoint
	integer Run(3,nrun)
	real Residual(nPoint),Estimate(nPoint)
	logical negStop,negFound
	real Cutoff,Gain,Speed,Limit,ResAMax,EstASum
	real Histo(maxPatch/2+1),Patch(maxPatch,maxPatch)
	integer maxCmp,ICmp(maxCmp),JCmp(maxCmp)
	real CCmp(maxCmp),RCmp(maxCmp)
c
c  Perform the component gathering step of a major Clark Clean iteration.
c  Determine the limiting residual, and store the components
c  greater than in the residual list. Perform the subtraction portion of
c  a minor iteration, by Cleaning this list. Then add the newly found
c  components to the estimate.
c
c  Inputs:
c    nx,ny	Image size.
c    Residual	The residuals.
c    Estimate	The estimate.
c    nPoint	The number of points in the residual and estimate.
c    Histo	
c    Patch	The beam patch.
c    minPatch)	The min and max sizes that the beam patch can take.
c    maxPatch)
c    maxNiter	The maximum total number of minor iterations permitted.
c    negStop	Stop iterating on the first negative component.
c    Cutoff
c    Gain	Loop gain.
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
	integer nPatch,nCmp,i
c
c  Find the limiting residual that we can fit into the residual list, then
c  go and fill the residual list.
c
	call GetLimit(Residual,nPoint,ResAMax,maxCmp,Histo,MaxPatch,
     *				nPatch,Limit)
	Limit = max(Limit, 0.5 * Cutoff)
	call GetComp(Residual,nPoint,ny,Ymap,Limit,
     *		Icmp,Jcmp,Rcmp,maxCmp,nCmp,Run,nRun)
c
c  Initialise the current components to zero.
c
	do i=1,nCmp
	  CCmp(i) = 0
	enddo
c
c  Determine the patch size to use, perform the minor iterations, then
c  add the new components to the new estimate.
c
	nPatch = max(MinPatch/2,nPatch)
	call SubComp(nx,ny,Ymap,Patch,MaxPatch,nPatch,Gain,MaxNiter,
     *	      negStop,1.,Speed,Limit,EstASum,ICmp,JCmp,RCmp,CCmp,
     *	      nCmp,Niter,negFound)
c
	call NewEst(CCmp,ICmp,JCmp,nCmp,Estimate,nPoint,Run,nRun)
	end
c************************************************************************
	subroutine SubComp(nx,ny,Ymap,Patch,n,PWidth,Gain,MaxNiter,
     *		NegStop,g,Speed,Limit,EstASum,Icmp,Jcmp,Rcmp,
     *		Ccmp,Ncmp,Niter,negFound)
c
	implicit none
	integer nx,ny,n,Ncmp,Niter,MaxNiter,PWidth
	real Limit,Gain,g,Speed,EstASum
	integer Icmp(Ncmp),Jcmp(Ncmp),Ymap(ny+1)
	real Patch(n,n),Rcmp(Ncmp),Ccmp(Ncmp)
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
c    Patch	Beam patch.
c    n		Dimension of beam patch. This is an odd number. The patch
c		is square. The peak is at n/2+1
c    PWidth	Patch half width to be used.
c    ny		Number of rows in the residuals.
c    Ymap	When cleaning the table of residuals, we need to determine where
c		residuals corresponding to a given range of j exist.
c		Ymap(j) gives the index of the table entry before that
c		contains, or would have contained, line j residuals.
c
c  Input/Outputs:
c    Rcmp	Flux at each residual peak.
c    Niter	Number of minor iterations completed.
c    EstASum	Absolute sum of the current model.
c
c  Outputs:
c    negFound	True if a negative component was found.
c
c------------------------------------------------------------------------
	integer maxrun
	parameter(maxrun=4096)
	integer i,i0,j0,k,ktot,ltot,NIndx
	integer Pk,p,ipk,jpk,ipkd,jpkd
	real TermRes,ResMax,Wts,alpha
	integer Temp(maxrun),Indx(maxrun)
	logical more
c
c  Externals.
c
	integer isamax
c
c  Initialise.
c
	Pk = isamax(Ncmp,Rcmp,1)
	ResMax = Rcmp(Pk)
	negFound = negFound .or. ResMax.lt.0
	TermRes = Limit
	alpha = g * Limit**(Speed+1)
c
c  Loop until no more. Start with some house keeping.
c
	more = abs(ResMax).gt.TermRes .and. Niter.lt.MaxNiter .and.
     *		.not.(negStop.and.negFound)
	dowhile(more)
c
c  Add the peaks to be subtracted into the components list, and
c  do some added house keeping.
c
	  Wts = Gain * Rcmp(Pk)
	  EstASum = EstASum - abs(CCmp(pk))
	  Ccmp(Pk) = Ccmp(Pk) + Wts
	  EstASum = EstASum + abs(CCmp(pk))
	  ipk = Icmp(Pk)
	  jpk = Jcmp(Pk)
	  ipkd = ipk - (n/2 + 1)
	  jpkd = jpk - (n/2 + 1)
c
c  Find the residuals which have suitable y values.
c
	  i = max(1, jpk-PWidth)
	  k = Ymap(i)
	  i = min(ny,jpk+PWidth)
	  ktot = Ymap(i+1)
c
c  Find the residuals which have suitable x values, then subtract. If the
c  beam does not cover the extent of the subimage, we have to go through
c  the process of determining which pixels are covered. This is done in a
c  clunchy vectorised fashion.
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
	        Rcmp(p) = Rcmp(p) - Wts * Patch(i0,j0)
	      enddo
	      k = k + ltot
	    enddo
c
	  else
	    do i=k+1,ktot
	      RCmp(i) = RCmp(i) - Wts * Patch(ICmp(i)-ipkd,JCmp(i)-jpkd)
	    enddo
	  endif
c
c  Ready for the next loop.
c
	  Niter = Niter + 1
	  TermRes = TermRes + 
     *	   alpha * abs(Wts) / ( EstASum * abs(ResMax)**Speed )
	  Pk = isamax(Ncmp,Rcmp,1)
	  ResMax = Rcmp(Pk)
	  negFound = negFound.or.ResMax.lt.0
	  more = abs(ResMax).gt.TermRes .and. Niter.lt.MaxNiter .and.
     *		.not.(negStop.and.negFound)
	enddo
	end
c************************************************************************
	subroutine GetLimit(Residual,nPoint,ResAMax,maxCmp,
     *		Histo,MaxPatch,nPatch,Limit)
c
	implicit none
	integer nPoint,maxPatch,nPatch,maxCmp
	real Residual(nPoint),ResAMax,Histo(maxPatch/2+1),Limit
c
c  Determine the limiting threshold and the patch size. The algorithm
c  used to determine the Limit and patch size are probably very
c  important to the run time of the program. Presently, however, the
c  algorithm is fairly simple.
c
c  Inputs:
c    Residual	The input residuals.
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
	ResAMin = ResAMax * Histo(maxPatch/2+1)
	a = (HistSize-2)/(ResAMax-ResAMin)
	b = 2 - a * ResAMin
	do i=1,HistSize
	  ResHis(i) = 0
	enddo
c
c  Now get the histogram while taking accound of the boxes.
c
	do i=1,nPoint
	  m = max(int(a * abs(Residual(i)) + b),1)
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
	subroutine GetComp(Residual,nPoint,ny,Ymap,Limit,
     *		           Icmp,Jcmp,RCmp,maxCmp,nCmp,Run,nRun)
c
	implicit none
	integer nPoint,ny,maxCmp,nCmp,nRun,Run(3,nrun)
	real Limit,Residual(nPoint),RCmp(maxCmp)
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
c    Residual	Contains all the residuals.
c
c  Outputs:
c    Ncmp	Number of residual peaks returned.
c    Ymap	Ymap(j) gives the index, in Icmp,Jcmp,Residual of the last
c		residual peak such that Jcmp .lt. j. See SubComp.
c    Icmp,Jcmp	Array of the indices of the residual peaks.
c    RCmp	Array if the residual peaks.
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
	    Temp(i) = abs(Residual(l+i))
	  enddo
	  call whenfgt (n0, Temp, 1, Limit, Indx,Ncmpd)
	  if(Ncmp+Ncmpd.gt.maxCmp)
     *		call bug('f','Internal bug in GetComp')
c
          do i = 1, Ncmpd
            RCmp(i+Ncmp) = Residual(l+Indx(i))
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
	subroutine NewEst(CCmp,ICmp,JCmp,nCmp,Estimate,nPoint,Run,nRun)
c
	implicit none
	integer nCmp,nPoint,nRun
	integer ICmp(nCmp),JCmp(nCmp),Run(3,nRun)
	real CCmp(nCmp),Estimate(nPoint)
c
c  This adds the components in CCmp to the components in Estimate.
c  The components in the two arrays are, unfortunately, stored in very
c  different ways, CCmp having associated arrays containing indices, while
c  Estimate is described by run specifications.
c
c  Inputs:
c    nCmp	Number of components.
c    ICmp,JCmp	Coordinates of components in CCmp.
c    CCmp	Value of the component.
c    nPoint	Number of points in the estimate.
c    Run	Run specifications describing components in Estimate.
c    nRun	Number of run specifications.
c
c  Input/Output:
c    Estimate	All the components.
c
c------------------------------------------------------------------------
	integer i,j,k,l
c
c  Vectorise this if you can!
c
	j = 1
	k = 1
	do l=1,nCmp
	  dowhile(JCmp(l).gt.Run(1,k).or.ICmp(l).gt.Run(3,k))
	    k = k + 1
	    j = j + Run(3,k) - Run(2,k) + 1
	  enddo
	  i = ICmp(l) - Run(2,k) + j
	  Estimate(i) = Estimate(i) + CCmp(l)
	enddo
	end
c************************************************************************
	subroutine Diff(Estimate,Map,Residual,nPoint,nx,ny,Run,nRun,
     *				BemFFT,n1,n2)
c
	implicit none
	integer nPoint,nx,ny,nRun,Run(3,nRun),n1,n2
	real Estimate(nPoint),Map(nPoint),Residual(nPoint)
	real BemFFT(*)
c
c------------------------------------------------------------------------
	integer i
c
	call Convl(Estimate,Residual,nPoint,nx,ny,Run,nRun,
     *				BemFFT,n1,n2)
	do i=1,nPoint
	  Residual(i) = Map(i) - Residual(i)
	enddo
c
	end
c************************************************************************
	subroutine GetDMC(lBeam,BemFFT,DMCFFT,mBeam,nBeam)
c
	implicit none
	integer lBeam,mBeam,nBeam
	real BemFFT((mBeam/2+1)*nBeam),DMCFFT((mBeam/2+1)*nBeam)
c
c  This determines the FFT of the dirty beam minus the Clean beam.
c
c  Inputs:
c    lBeam
c    mBeam,nBeam
c    BemFFT
c  Output:
c    DMCFFT
c
c------------------------------------------------------------------------
	integer i
	real cdelt1,cdelt2,a,sxx,syy,sxy,fwhm1,fwhm2,pa
c
c  Set DMC as the FFT of the beam.
c
	do i=1,(mBeam/2+1)*nBeam
	  DMCFFT(i) = BemFFT(i)
	enddo
c
c  Get parameters needed to determine the Clean beam size.
c
	call rdhdr(lBeam,'bmaj',fwhm1,0.)
	call rdhdr(lBeam,'bmin',fwhm2,0.)
	call rdhdr(lBeam,'bpa',pa,0.)
	if(fwhm1*fwhm2.eq.0)
     *	  call bug('f','Beam size parameters missing from beam header')
	call rdhdr(lBeam,'cdelt1',cdelt1,0.)
	call rdhdr(lBeam,'cdelt2',cdelt2,0.)
	if(cdelt1*cdelt2.eq.0)
     *		call bug('f','Beam pixel increment missing')
c
c  Determine the parameters of the Clean beam.
c
	call GauTran(cdelt1,cdelt2,mBeam,nBeam,fwhm1,fwhm2,pa,
     *							a,sxx,syy,sxy)
c
c  Subtract the FFT of the Clean beam from the FFT of the dirty beam.
c
	call GausAdd(DMCFFT,mBeam,nBeam,a,sxx,syy,sxy,.true.)
c
	end
c************************************************************************
	subroutine InterIni(server,doCtrl,doInter)
c
	implicit none
	character server*(*)
	logical doCtrl,doInter
c
c------------------------------------------------------------------------
 	integer length,i1,i2,status
	character values(2)*5,lutval(3)*5,image(3)*5,fidpan(2)*5
	character panel*24,tv*24
c
	integer len1
c
	data values/'PAUSE',' RUN '/
	data lutval/'COLOR',' RGB ',' B&W '/
	data image/'CLEAN','DIRTY','MODEL'/
	data fidpan/' MOD ','ROAM '/
c
	doctrl = .false.
	dointer = .false.
	length = len1(server)
	if(length.eq.0)return
	i1 = index(server,'@')
	if(i1.le.1.or.i1.ge.length)
     *	  call bug('f','TV device names must be of the form type@name')
	i2 = index(server(i1+1:length),'/') + i1
	if(i2.ne.i1.and.min(length-i2,i2-(i1+1)).le.0)
     *	  call bug('f','Bad name for TV/panel server')
	if(server(i2:i2).eq.'/')then
	  tv = server(1:i2-1)
	  panel = server(i2+1:length)
	else
	  tv = server(1:length)
	  panel = server(i1+1:length)
	endif
	call tvopen(tv)
	doInter = .true.
	status = -1
	if(panel.ne.'-')call CtrlInit(panel,status)
	if(status.ne.0)then
	  call bug('w','Failed to connect to panel server')
	  doCtrl = .false.
	else
	  call CtrlDef('pause','button',values,2)
	  call CtrlDef('exit','button','EXIT ',1)
	  call CtrlDef('image','button',image,3)
	  call CtrlDef('lut','button',lutval,3)
	  call CtrlDef('zoom','button','ZOOM+',1)
	  call CtrlDef('unzoom','button','ZOOM-',1)
	  call CtrlDef('fidpan','button',fidpan,2)
	  call CtrlDef('reset','button','RESET',1)
	  call CtrlDef('cursor','cursor','Pan',1)
	  call CtrlView	  
	  doCtrl = .true.
	endif
	end
c************************************************************************
	subroutine Interact(Estimate,Residual,Restore,nx,ny,
     *					bmin,bmax,more,doCtrl)
c
	implicit none
	integer nx,ny
	real bmin,bmax
	logical more,doCtrl
	real Restore(nx,ny),Estimate(nx,ny),Residual(nx,ny)
c
c------------------------------------------------------------------------
	integer IMEST,IMRESID,IMREST,FIDDLE,PAN
	parameter(IMEST=2,IMRESID=1,IMREST=0,FIDDLE=0,PAN=1)
	integer mxchan,changes,val1,val2
	character object*16
	logical domore,dodisp,doview
c
	integer zoom,xc,yc,jx0,jy0,levels,image,fidpan,nxd,nyd,i0,j0
	integer xpix,ypix
	real bscale,bzero
	logical first
	character table(3)*8
c
c  Externals.
c
	integer pow2
c
c  Save statements.
c
	save bscale,bzero,zoom,xc,yc,jx0,jy0,levels,first,image,fidpan
	save nxd,nyd,i0,j0,xpix,ypix
c
	data first/.true./
	data table/'colour  ','rainbow ','B&W     '/
c
	if(first)then
	  call tvchar(xpix,ypix,mxchan,levels)
	  bscale = (levels - 1) / (bmax - bmin)
	  bzero  = bmin
	  nxd = min(xpix,nx)
	  nyd = min(ypix,ny)
	  jx0 = (xpix - nxd)/2
	  jy0 = (ypix - nyd)/2
	  i0 = (nx-nxd)/2 + 1
	  j0 = (ny-nyd)/2 + 1
	  Zoom = min(pow2(min(xpix/nxd,ypix/nyd)),8)
	  Xc = xpix/2
	  Yc = ypix/2
	  image = IMREST
	  fidpan = FIDDLE
	endif
c
c  Do some fiddling of the TV the first time thru.
c
	if(first.and.doCtrl)then
	  object = ' '
	  val1 = 0
	  val2 = 0
	  call output(' Waiting to run ...')
	  dowhile(object.ne.'pause'.and.val1.ne.1)
	    call CtrlWait(object,changes,val1,val2)
	  enddo
	  call TvReset
	  call TvChan (1)
	  call ViewIt(xpix,ypix,xc,yc,zoom)
	endif
	first = .false.
c
c  Display the image.
c
	if(image.eq.IMREST)then
	  call Display(Restore(i0,j0),nxd,nyd,nx,jx0,jy0,
     *					bscale,bzero,levels)
	else if(image.eq.IMRESID)then
	  call Display(Residual(i0,j0),nxd,nyd,nx,jx0,jy0,
     *					bscale,bzero,levels)
	else if(image.eq.IMEST)then
	  call Display(Estimate(i0,j0),nxd,nyd,nx,jx0,jy0,
     *					bscale,bzero,levels)
	endif
c
c  Check whether we are to go into interactive mode.
c
	if(doCtrl)then
	  call CtrlChck('pause',changes,val1,val2)
	  if(val1.eq.0)then
	    call output(' Pausing ...')
	    call CtrlWait(object,changes,val1,val2)
	    domore = .true.
	    dowhile(domore)
	      dodisp = .false.
	      doview = .false.
	      if(object.eq.'lut')then
	        call tvlut(table(val1+1))
		dodisp = .true.
	      else if(object.eq.'image')then
		image = val1
		dodisp = .true.
	      else if(object.eq.'fidpan')then
		fidpan = val1
	      else if(object.eq.'pause')then
		domore = val1.eq.0
	      else if(object.eq.'exit')then
		domore = .false.
		more = .false.
	      else if(object.eq.'zoom')then
	        zoom = min(8,Zoom + Zoom)
		doview = .true.
	      else if(object.eq.'unzoom')then
	        zoom = max(1,Zoom/2)
		doview = .true.
	      else if(object.eq.'reset')then
		bscale = (levels - 1) / (bmax - bmin)
		bzero  = bmin
		Zoom = min(pow2(min(xpix/nxd,ypix/nyd)),8)
		Xc = xpix/2
		Yc = ypix/2
		doview = .true.
		dodisp = .true.
	      else if(object.eq.'cursor')then
		if(fidpan.eq.FIDDLE)then
		  bscale = (levels-1)/(0.04*(val2+1)*(bmax-bmin))
		  bzero = 0.02*((val1-val2)*bmax + (val1+val2)*bmin)
		  dodisp = .true.
		else
		  xc = xpix*val1/100
		  yc = ypix - ypix*val2/100
		  doview = .true.
		endif
	      endif
	      if(doview) call ViewIt(xpix,ypix,xc,yc,zoom)
	      if(dodisp)then
		if(image.eq.IMREST)then
		  call Display(Restore(i0,j0),nxd,nyd,nx,jx0,jy0,
     *					bscale,bzero,levels)
	        else if(image.eq.IMRESID)then
		  call Display(Residual(i0,j0),nxd,nyd,nx,jx0,jy0,
     *					bscale,bzero,levels)
		else if(image.eq.IMEST)then
		  call Display(Estimate(i0,j0),nxd,nyd,nx,jx0,jy0,
     *					bscale,bzero,levels)
		endif
	      endif
	      if(doview.or.dodisp) call TvFlush
	      if(domore)call CtrlWait(object,changes,val1,val2)
	    enddo
	    call output(' Continuing ...')
	  endif
	  if(more)then
	    call CtrlChck('exit',changes,val1,val2)
	    more = more.and.changes.eq.0
	  endif
	endif
c
	end
c************************************************************************
	subroutine ViewIt(xpix,ypix,xc,yc,zoom)
c
	implicit none
	integer xpix,ypix,xc,yc,zoom
c
c------------------------------------------------------------------------
	integer blcx,blcy,trcx,trcy
	blcx = max(0,min(xc - xpix/(2*Zoom),xpix - xpix/Zoom))
	blcy = max(0,min(yc - ypix/(2*Zoom),ypix-ypix/Zoom))
	trcx = blcx + xpix/Zoom - 1
	trcy = blcy + ypix/Zoom - 1
	call TvView(blcx,blcy,trcx,trcy)
	end
c************************************************************************
	integer function pow2(n)
c
	implicit none
	integer n
c
c  Return a power of two less than or equal to n.
c
c------------------------------------------------------------------------
	integer k
	k = 1
	dowhile(k+k.le.n)
	  k = k + k
	enddo
	pow2 = k
	end
c************************************************************************
	subroutine Display(Restore,nx,ny,n1,jx0,jy0,bscale,bzero,levels)
c
	implicit none
	integer nx,ny,n1,jx0,jy0,levels
	real bscale,bzero,Restore(n1,ny)
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer data(maxdim)
	integer i,j,jx,jy
c	
	jx = jx0
	jy = jy0
	call tvscale(bzero, bscale)
	do j=1,ny
	  do i = 1, nx
	    data(i) = bscale * (Restore(i,j) - bzero)
	    data(i) = min(max(0,data(i)), levels-1)
	  enddo
	  call tvline(jx,jy,1,data,nx)
	  jy = jy + 1
	enddo
	call TvFlush
	end
c************************************************************************
	subroutine GausAdd(BemFFT,mBeam,nBeam,a,sxx,syy,sxy,doBeam)
c
	implicit none
	integer mBeam,nBeam
	real a,sxx,syy,sxy
	logical doBeam
	real BemFFT(nBeam,mBeam/2+1)
c
c  Inputs:
c    mBeam,nBeam Give the dimensions of the (untransformed) beam. The
c		dimensions of the transformed beam are derived from these.
c    a,sxx,syy,sxy Coefficients which describe the gaussian.
c    doBeam	If true, then BemFFT already contains a transform of the
c		beam, so calculate transform of (gaussian-beam).
c
c  Input/Output:
c    BemFFT	If doBeam is true, this contains the transform of the beam
c		on input. On output it contains either the transform of the
c		gaussian, or the transform of (gaussian-beam).
c
c------------------------------------------------------------------------
	integer i,j,i0,j0
	real temp
c
c  Gaussian - beam.
c
	do i=1,mBeam/2+1
	  if(doBeam)then
	    i0 = 1
	    j0 = 1
	    do j=1,nBeam/2 
	      temp=a*exp(sxx*(i-i0)**2+syy*(j-j0)**2+sxy*(i-i0)*(j-j0))
	      BemFFT(j,i) = BemFFT(j,i) - temp
	    enddo
	    i0 = 1
	    j0 = nBeam + 1
	    do j=nBeam/2+1,nBeam
	      temp=a*exp(sxx*(i-i0)**2+syy*(j-j0)**2+sxy*(i-i0)*(j-j0))
	      BemFFT(j,i) = BemFFT(j,i) - temp
	    enddo
	  else
c
c  Just a gaussian.
c
	    i0 = 1
	    j0 = 1
	    do j=1,nBeam/2 
	      temp=a*exp(sxx*(i-i0)**2+syy*(j-j0)**2+sxy*(i-i0)*(j-j0))
	      BemFFT(j,i) = temp
	    enddo
	    i0 = 1
	    j0 = nBeam + 1
	    do j=nBeam/2+1,nBeam
	      temp=a*exp(sxx*(i-i0)**2+syy*(j-j0)**2+sxy*(i-i0)*(j-j0))
	      BemFFT(j,i) = temp
	    enddo
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GauTran(cdelt1,cdelt2,n1,n2,fwhm1,fwhm2,pa,
     *							a,sxx,syy,sxy)
c
	implicit none
	integer n1,n2
	real cdelt1,cdelt2,fwhm1,fwhm2,pa,a,sxx,syy,sxy
c
c  Get the parameters which describe the transform of the gaussian.
c  The transform gaussian will be given by:
c
c    a*exp( sxx*(i-i0)**2+syy*(j-j0)**2 + sxy*(i-i0)*(j-j0) )
c
c  The values for sxx, syy and sxy come from continuious Fourier theory.
c  However continuious theory only approximates the discrete case. So
c  we calculate "a" by summing the gaussian to work out what the scale
c  factor should be to give an inverse whoes peak is 1.
c
c  Inputs:
c    cdelt1 )	The increments in the two image axes (radians).
c    cdelt2 )
c    n1,n2	Dimension of the image under consideration.
c    fwhm1 )	Full width at half max of the gaussian (radians).
c    fwhm2 )
c    pa		Position angle (degrees).
c
c  Outputs:
c    a		Scale factor.
c    sxx )
c    syy )	See above.
c    sxy )
c
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	integer i,j,i0,j0
	real du,dv,adash,bdash,theta,c2,s2,sum,temp
c
	adash = pi**2/(4*log(2.)) * fwhm1**2
	bdash = pi**2/(4*log(2.)) * fwhm2**2
	theta = pi/180. * pa
	c2 = -cos(2*theta)
	s2 = -sin(2*theta)
c
	du = 1./(n1*cdelt1)
	dv = 1./(n2*cdelt2) 
c
	sxx = -0.5 * (adash*(1+c2) + bdash*(1-c2)) * du * du
	syy = -0.5 * (adash*(1-c2) + bdash*(1+c2)) * dv * dv
	sxy =  (adash-bdash) * s2 * du * dv
c
c  Now work out what the scale factor "a" should be.
c
	i0 = 1
	j0 = n2/2+1
	do i=1,n1/2
	  temp = 0
	  do j=1,n2
	    temp = temp + exp( sxx*(i-i0)*(i-i0) +
     *			       syy*(j-j0)*(j-j0) +
     *			       sxy*(i-i0)*(j-j0) )
	  enddo
c
	  if(i.eq.1)then
	    sum = temp
	  else
	    sum = sum + 2*temp
	  endif
	enddo
	a = 1./sum
	end
c************************************************************************
	subroutine BeamSize(lBeam,n1,n2,xbeam,ybeam,bmaj,bmin,bpa)
c
	implicit none
	integer lBeam,n1,n2,xbeam,ybeam
	real bmaj,bmin,bpa
c
c  Find beam size and location.
c  Put bmaj, bmin, bpa into the map header if not present.
c
c  Inputs:
c    lBeam	Handle of the beam.
c    n1,n2	Dimensions of the beam
c  Outputs:
c    xBeam,yBeam Location of beam peak.
c    bmaj,bmin	fwhm beam size in radians
c    bpa	position angle of beam in degrees.
c
c  History:
c    mchw 17 july 1990
c-----------------------------------------------------------------------c
	real pi
	parameter(pi=3.141592653589793)
	integer nP
	parameter(nP=11)
	real Patch(nP*nP),cdelt1,cdelt2
	character line*72
c
	call GetBeam(lBeam,n1,n2,
     *		 	Patch,nP,xBeam,yBeam)

c
c  Determine the fwhm (in radians)
c
	call rdhdr(lBeam,'bmaj',bmaj,0.)
	call rdhdr(lBeam,'bmin',bmin,0.)
	call rdhdr(lBeam,'bpa',bpa,0.)
	if(bmaj*bmin.eq.0) then
	  call rdhdr(lBeam,'cdelt1',cdelt1,0.)
	  call rdhdr(lBeam,'cdelt2',cdelt2,0.)
c
c  Put bmaj, bmin, bpa into the map header if not present.
c
	  if(cdelt1*cdelt2.eq.0)
     *		call bug('f','Beam pixel increment missing')
	  call GetFwhm(Patch,nP,xBeam-n1/2+nP/2,yBeam-n2/2+nP/2,
     *				cdelt1,cdelt2,bmaj,bmin,bpa)
	  call wrhdr(lBeam,'bmaj',bmaj)
	  call wrhdr(lBeam,'bmin',bmin)
	  call wrhdr(lBeam,'bpa',bpa)
	endif
c
c  Advise the user of beam size.
c
	write(line,'(a,f6.2,a,f6.2,a)')
     *	   'Fitted gaussian beam fwhm is',(3600*180/pi)*bmaj,' by',
     *	   (3600*180/pi)*bmin,' arcsec.'
	call output(line)
	write(line,'(a,f6.1,a)')'Position angle: ',bpa,' degrees.'
	call output(line)
c
	end
c************************************************************************
	subroutine GetBeam(lBeam,n1,n2,Patch,nP,xBeam,yBeam)
c
	implicit none
	integer n1,n2,lBeam,nP,xBeam,yBeam
	real Patch(nP,nP)
c
c  This gets the central portion of the beam, and determines the location
c  of the beam maxima (which is assumed to be near the centre of the beam).
c
c  Inputs:
c    lBeam	Handle of the beam.
c    n1,n2	Dimensions of the beam
c    nP		Size of central patch to return.
c
c  Outputs:
c    xBeam,yBeam Location of beam peak.
c    Patch	The central portion of the beam, centered around
c		(n1/2+1,n2/2+1)
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,imin,imax,jmin,jmax
	real Data(maxdim)
c
c  Externals.
c
	integer Ismax
c
c  Open the beam file and check its size.
c
	if(max(n1,n2).gt.maxdim)call bug('f','Beam is too big')
	imin = n1/2 - nP/2 + 1
	imax = imin + nP - 1
	jmin = n2/2 - nP/2 + 1
	jmax = jmin + nP - 1
	if(imin.lt.1.or.imax.gt.n1.or.jmin.lt.1.or.jmax.gt.n2)
     *	  call bug('f','Beam is too small')
c
c  Read in the central patch of the beam.
c
	do j=jmin,jmax
	  call xyread(lBeam,j,Data)
	  do i=imin,imax
	    Patch(i-imin+1,j-jmin+1) = Data(i)
	  enddo
	enddo
c
c  Find the maximum, and hopefully it is 1.
c
	i = ismax(nP*nP,Patch,1)
	xBeam = mod(i-1,nP) + 1
	yBeam = (i-1)/nP + 1
	if(abs(1-Patch(xBeam,yBeam)).gt.0.01)
     *		call bug('w','Beam peak is not 1')
	xBeam = xBeam + imin - 1
	yBeam = yBeam + jmin - 1
c
	end
c************************************************************************
	subroutine GetFwhm(Beam,nP,xBeam,yBeam,cdelt1,cdelt2,
     *				Fwhm1,Fwhm2,Pa)
c
	implicit none
	integer xBeam,yBeam,nP
	real Beam(nP*nP)
	real cdelt1,cdelt2,Fwhm1,Fwhm2,Pa
c
c  Get the full width half max parameters. This calls a routine which
c  finds the least squares fit of the beam patch to a guassian. The
c  result is then converted into more useful units.
c
c  Inputs:
c    Beam	The central portion of the beam.
c    np		Dimension of the beam patch.
c    xBeam,yBeam Location of the center of the beam.
c    cdelt1,cdelt2 Grid increments
c
c  Outputs:
c    Fwhm1	Fwhm, in units of cdelt1, cdelt2 along the major axis.
c    Fwhm2	Fwhm, in units of cdelt1, cdelt2 along the minor axis.
c    Pa		Position angle, in degrees, measured east of north.
c
c------------------------------------------------------------------------
	integer MaxIter
	real pi
	parameter(MaxIter=100,pi=3.141592653589793)
	include 'tvcln.h'
	real X(3),dx(3),aa(3*3),t1,t2
	real f(nPM*nPM),fp(nPM*nPM),dfdx(3*nPM*nPM)
	integer ifail,k,i,j
	external FUNCTION,DERIVE
c
c  Initialise the arrays ready for the optimisation routine.
c
	if(nP.gt.nPM)call bug('f','Beam patch too big to handle')
	k = 0
	do j=1,nP
	  do i=1,nP
	    k = k + 1
	    sxxc(k) = (i-xBeam)**2
	    syyc(k) = (j-yBeam)**2
	    sxyc(k) = (i-xBeam)*(j-yBeam)
	    Patch(k) = Beam(k)
	  enddo
	enddo
c
c  Form the initial estimate of the gaussian beam, by using the least
c  squares solution of a "linearised" version of the problem. This should
c  be robust, though somewhat inaccurate.
c
	call LinEst(Beam,nP,xBeam,yBeam,x)
c
c  Now perform the fit using a proper least squares routine.
c
	call nllsqu(3,nP*nP,x,dx,MaxIter,0.,0.005/3,.true.,ifail,
     *	  FUNCTION,DERIVE,f,fp,dx,dfdx,aa)
	if(ifail.ne.0)call bug('f','Beam fit failed')
c
c  Convert the results to meaningful units. The fwhm are in grid units
c  and the pa is in degrees.
c
	x(1) = -x(1) / (cdelt1*cdelt1)
	x(2) = -x(2) / (cdelt2*cdelt2)
	x(3) = -x(3) / (cdelt1*cdelt2)
c
	t1 = x(1)+x(2)
	t2 = sqrt((x(1)-x(2))**2 + x(3)**2)
	fwhm1 = 0.5 * ( t1 - t2 )
	fwhm2 = 0.5 * ( t1 + t2 )
	fwhm1 = sqrt(4*log(2.)/fwhm1)
	fwhm2 = sqrt(4*log(2.)/fwhm2)
	if(x(3).ne.0.)then
	  pa = 90. / pi * atan2(-x(3),x(1)-x(2))
	else
	  pa = 0.
	endif
c
	end
c************************************************************************
	subroutine LinEst(Beam,nP,xBeam,yBeam,b)
c
	implicit none
	integer nP,xBeam,yBeam
	real b(3),Beam(nP,nP)
c
c  Estimate the parameters for the gaussian fit using an approximate
c  but linear technique. This finds values of b which
c  minimises:
c
c    SUM ( log(Beam(x,y)) - b(1)*x*x - b(2)*y*y - b(3)*x*y )**2
c
c  where the sum is taken over the "main lobe" of the beam only (the
c  "main lobe" is the central part of the beam which is greater than
c  a threshold). Because this is a linear least squares problem, it
c  should always produce a solution (i.e. no worries about convergence
c  of an iterative fitting process).
c
c  Inputs:
c    nP		Dimension of the beam patch.
c    xBeam)	Center pixel of the beam patch.
c    yBeam)
c    Beam	The beam patch.
c
c  Output:
c    b		The estimates of the parameters.
c
c------------------------------------------------------------------------
	real thresh
	parameter(thresh=0.1)
	integer i,j,ilo,ihi,ilod,ihid,ipvt(3),ifail
	real a(3,3),x,y,z,f
	logical more
c
c  Check that center pixel is within the patch.
c
	if(xBeam.lt.1.or.xBeam.gt.nP.or.yBeam.lt.1.or.yBeam.gt.nP)
     *	  call bug('f','Centre pixel of beam is not in beam patch')
c
c  Determine the pixel range that spans across the main lobe at x=0.
c
	more = .true.
	ihi = xBeam
	dowhile(ihi.lt.nP.and.more)
	  more = Beam(ihi+1,yBeam).gt.thresh
	  if(more)ihi = ihi + 1
	enddo
	ilo = xBeam - (ihi-xBeam)
c
c  Accumulate the info we want over the pixels of the main lobe. For each row,
c  this also keeps track of the range in x which bridges the central lobe.
c
	do j=1,3
	  b(j) = 0
	  do i=1,3
	    a(i,j) = 0
	  enddo
	enddo
c
	j = yBeam
	dowhile(ilo.le.ihi.and.j.le.nP)
	  ilod = nP + 1
	  ihid = 0
	  do i=max(ilo-1,1),min(ihi+1,nP)
	    if(Beam(i,j).gt.thresh)then
	      ilod = min(ilod,i)
	      ihid = max(ihid,i)
	      x = (i-xBeam)**2
	      y = (j-yBeam)**2
	      z = (i-xBeam)*(j-yBeam)
	      f = log(Beam(i,j))
	      a(1,1) = a(1,1) + x*x
	      a(2,1) = a(2,1) + x*y
	      a(3,1) = a(3,1) + x*z
	      a(2,2) = a(2,2) + y*y
	      a(3,2) = a(3,2) + y*z
	      a(3,3) = a(3,3) + z*z
	      b(1) = b(1) + f*x
	      b(2) = b(2) + f*y
	      b(3) = b(3) + f*z
	    endif
	  enddo
	  ilo = ilod
	  ihi = ihid
	  j = j + 1
	enddo
c
	a(1,2) = a(2,1)
	a(1,3) = a(3,1)
	a(2,3) = a(3,2)
c
c  Solve the 3x3 system of equations, to find the numbers that we really want.
c  If the matrix proves singular, return the estimate as two grid units.
c
	call sgefa(a,3,3,ipvt,ifail)
	if(ifail.eq.0)then
	  call sgesl(a,3,3,ipvt,b,0)
	else
	  b(1) = -log(2.)
	  b(2) = -log(2.)
	  b(3) = 0
	endif
	end
c************************************************************************
	subroutine DERIVE(x,dfdx,n,m)
c
	implicit none
	integer n,m
	real x(n),dfdx(n,m)
c
c------------------------------------------------------------------------
	include 'tvcln.h'
	integer i
	real temp
c
	do i=1,m
	  temp = exp(sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3))
	  dfdx(1,i) = - sxxc(i) * temp
	  dfdx(2,i) = - syyc(i) * temp
	  dfdx(3,i) = - sxyc(i) * temp
	enddo
c
	end
c************************************************************************
	subroutine FUNCTION(x,f,n,m)
c
	implicit none
	integer n,m
	real x(n),f(m)
c
c  Calculate the mismatch function.
c
c------------------------------------------------------------------------
	include 'tvcln.h'
	integer i
	real temp
c
	do i=1,m
	  temp = exp(sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3))
	  f(i) = Patch(i) - temp
	enddo
	end
c************************************************************************
	subroutine defregio(boxes,nMap,nBeam,icentre,jcentre)
c
	implicit none
	integer boxes(*),nMap(3),nBeam(2),icentre,jcentre
c
c  Set the region of interest to the lastest area that can be safely
c  deconvolved.
c------------------------------------------------------------------------
	integer blc(3),trc(3),width
c
	width = min(icentre-1,nBeam(1)-icentre) + 1
	blc(1) = max(1,(nMap(1)-width)/2)
	trc(1) = min(nMap(1),blc(1)+width-1)
c
	width = min(jcentre-1,nBeam(2)-jcentre) + 1
	blc(2) = max(1,(nMap(2)-width)/2)
	trc(2) = min(nMap(2),blc(2)+width-1)
c
	blc(3) = 1
	trc(3) = 1
c
	call BoxDef(boxes,3,blc,trc)
c
	end
