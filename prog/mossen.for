c************************************************************************
	program mossen
	implicit none
c= mossen -- Determine the rms and gain in a mosaiced image.
c& rjs
c: map combination
c+
c	MOSSEN determines the rms and gain of a mosaiced image. Both
c	of these parameters are a function of position (and also mildly
c	frequency dependent).
c@ in
c	The input map of interest. No default.
c@ region
c	The standard region of interest parameter, giving the region
c	where the sensitivity function is to be determined. The default
c	is the entire input image.
c@ sen
c	The output image giving the rms as a function of position.
c	The default is that this image is not created.
c@ gain
c	The output image giving the gain to a unit point source as a
c	function of position. To avoid noise amplification at the
c	edge of mosaiced regions, Miriad does not normally totally
c	correct for the primary beam beyond a certain point. The default
c	is that no gain image is formed.
c--
c  History:
c    rjs   6nov94 Original version.
c    rjs  13mar95 Add call to mosMFin
c    rjs  23jul97 Add pbtype.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	character version*(*)
	integer MAXBOXES,MAXRUNS
	parameter(version='MosSen: version 1.0 6-Nov-94')
	parameter(MAXBOXES=1024,MAXRUNS=3*MAXDIM)
c
	integer tIn,tSen,tGain,pSen,pGain,i,k,nBuff
	integer xmin,xmax,ymin,ymax
	logical dosen,dogain
	character in*64,sen*64,gain*64
	integer boxes(MAXBOXES),npix1,npix2,nin(3),nout(MAXNAX)
	integer Runs1(3,MAXRUNS),Runs2(3,MAXRUNS),nRuns1,nRuns2
	integer naxis,blc(MAXNAX),trc(MAXNAX)
	integer npnt
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('in',in,' ')
	if(in.eq.' ')call bug('f','An input must be given')
	call keya('sen',sen,' ')
	call keya('gain',gain,' ')
	dosen = sen.ne.' '
	dogain = gain.ne.' '
	if(.not.dosen.and..not.dogain)call bug('f',
     *	  'Either an output rms or gain image must be requested')
	call BoxInput('region',in,boxes,MAXBOXES)
	call keyfin
c
c  Open the input.
c
	call xyopen(tIn,in,'old',3,nin)
	call coInit(tIn)
	call rdhdi(tIn,'naxis',naxis,3)
	naxis = min(naxis,MAXNAX)
c
c  Initialise the mosaic routines.
c
	call mosLoad(tIn,npnt)
c
c  Set up the region of interest and determine the output image size.
c
	call boxMask(tIn,Boxes,MAXBOXES)
	call boxSet(boxes,3,nin,' ')
	call boxInfo(boxes,naxis,blc,trc)
	nout(1) = trc(1) - blc(1) + 1
	nout(2) = trc(2) - blc(2) + 1
	nout(3) = trc(3) - blc(3) + 1
	do i=4,naxis
	  nout(i) = 1
	enddo
c
c  Do the real work.
c
	if(dosen) call MakeIm(tIn,tSen,sen,naxis,nout,blc,version)
	if(dogain)call MakeIm(tIn,tGain,gain,naxis,nout,blc,version)
c
	nBuff = 0
	do k=blc(3),trc(3)
	  call boxRuns(1,k,' ',boxes,Runs1,MAXRUNS,nRuns1,
     *					xmin,xmax,ymin,ymax)
	  call Count(Runs1,nRuns1,npix1)
c
c  Allocate memory if needed.
c
	  if(npix1.gt.nBuff)then
	    if(nBuff.gt.0)then
	      call memFree(pSen,nBuff,'r')
	      call memFree(pGain,nBuff,'r')
	    endif
	    nBuff = npix1
	    call memAlloc(pSen,nBuff,'r')
	    call memAlloc(pGain,nBuff,'r')
	  endif
c
c  Do the real work now.
c
	  call mosMini(tIn,real(k))
	  call mosWtsR(Runs1,nRuns1,memr(pGain),memr(pSen),npix1)
	  call Compress(memr(pSen),memr(pGain),npix1,npix2,
     *	    Runs1,nRuns1,Runs2,nRuns2,MAXRUNS)
c
c  Store the result. What we actually compute is the receprocal
c  of what the user wants.
c
	  if(dosen)then
	    if(naxis.gt.2) call xysetpl(tSen,1,k-blc(3)+1)
	    call PutPlane(tSen,Runs2,nRuns2,1-blc(1),1-blc(2),
     *				nOut(1),nOut(2),memr(pSen),npix2)
	    call PutRuns( tSen,Runs2,nRuns2,1-blc(1),1-blc(2),
     *				nOut(1),nOut(2))
	  endif
	  if(dogain)then
	    if(naxis.gt.2) call xysetpl(tGain,1,k-blc(3)+1)
	    call PutPlane(tGain,Runs2,nRuns2,1-blc(1),1-blc(2),
     *				nOut(1),nOut(2),memr(pGain),npix2)
	    call PutRuns( tGain,Runs2,nRuns2,1-blc(1),1-blc(2),
     *				nOut(1),nOut(2))
	  endif
	  call mosMFin
	enddo
c
c  Close up shop.
c
	if(dogain)call xyclose(tGain)
	if(dosen) call xyclose(tSen)
	call memFree(pSen,nBuff,'r')
	call memFree(pGain,nBuff,'r')
	call xyclose(tIn)
	end
c************************************************************************
	subroutine Count(Runs,nRuns,npix)
c
	implicit none
	integer nRuns,Runs(3,nRuns+1),npix
c
c  Count the number of pixels of interest in this plane.
c------------------------------------------------------------------------
	integer i
c
	npix = 0
	do i=1,nRuns
	  npix = npix + Runs(3,i) - Runs(2,i) + 1
	enddo
	end
c************************************************************************
	subroutine Compress(Sen,Gain,npix1,npix2,
     *	    Runs1,nRuns1,Runs2,nRuns2,MAXRUNS)
c
	implicit none
	integer npix1,npix2,nRuns1,nRuns2,MAXRUNS
	integer Runs1(3,nRuns1),Runs2(3,MAXRUNS)
	real Sen(npix1),Gain(npix1)
c
c  Eliminate those pixels which have a gain of zero.
c------------------------------------------------------------------------
	integer i,ipt,opt,iRuns,ngood
c
	ipt = 0
	opt = 0
	nRuns2 = 0
	do iRuns=1,nRuns1
	  ngood = 0
	  do i=Runs1(2,iRuns),Runs1(3,iRuns)
	    ipt = ipt + 1
	    if(Gain(ipt).gt.0)then
	      ngood = ngood + 1
	      opt = opt + 1
	      Gain(opt) = 1/Gain(ipt)
	      Sen(opt)  = sqrt(Sen(ipt)*Gain(opt))
	    else if(ngood.gt.0)then
	      nRuns2 = nRuns2 + 1
	      if(nRuns2.gt.MAXRUNS)call bug('f','Runs array overflow')
	      Runs2(1,nRuns2) = Runs1(1,iRuns)
	      Runs2(2,nRuns2) = i - ngood
	      Runs2(3,nRuns2) = i - 1
	      ngood = 0
	    endif
	  enddo
	  if(ngood.gt.0)then
	    nRuns2 = nRuns2 + 1
	    if(nRuns2.ge.MAXRUNS)call bug('f','Runs array overflow')
	    Runs2(1,nRuns2) = Runs1(1,iRuns)
	    Runs2(2,nRuns2) = Runs1(3,iRuns) - ngood + 1
	    Runs2(3,nRuns2) = Runs1(3,iRuns)
	  endif
	enddo
c
	Runs2(1,nRuns2+1) = 0
	Runs2(2,nRuns2+1) = 0
	Runs2(3,nRuns2+1) = 0
	npix2 = opt
c
	end
c************************************************************************
	subroutine MakeIm(tIn,tOut,Out,naxis,nout,blc,version)
c
	implicit none
	integer tIn,tOut
	integer naxis,nout(naxis),blc(naxis)
	character Out*(*),version*(*)
c
c  Create an output dataset.
c------------------------------------------------------------------------
	integer i
	double precision crpix
	character line*72,num*3
	integer nkeys
	parameter(nkeys=30)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *	  'cdelt5  ','crval1  ','crval2  ','crval3  ','crval4  ',
     *	  'crval5  ','ctype1  ','ctype2  ','ctype3  ','ctype4  ',
     *	  'ctype5  ','obstime ','epoch   ','history ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','object  ','pbfwhm  ',
     *	  'observer','telescop','restfreq','vobs    ','btype   ',
     *	  'pbtype  '/
c
c  Open the output.
c
	call xyopen(tOut,Out,'new',naxis,nout)
c
c  Copy all the other keywords across, which have not changed and add history
c
	do i=1,nkeys
	  call hdcopy(tIn, tOut, keyw(i))
	enddo
c
c  Adjust the reference pixel.
c
	do i=1,naxis
	  num = itoaf(i)
	  call rdhdd(tIn,'crpix'//num,crpix,1.d0)
	  call wrhdd(tOut,'crpix'//num,crpix-blc(i)+1)
	enddo
c
c  Write crap to the history file.
c
	call hisopen(tOut,'append')
	line = 'MOSSEN: Miriad '//version
	call hiswrite(tOut,line)
	call hisinput(tOut,'MOSSEN')
	call hisclose(tOut)
c
	end
	
