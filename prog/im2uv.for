c************************************************************************
	program im2uv
	implicit none
c
c= im2uv -- Convert an image to visibilities.
c& rjs
c: uv analysis
c+
c	This Fourier transforms an image and writes the result as a
c	visibility file. Thus the uv plane sampling pattern is a
c	cartesian grid. Many of the uv variables are given reasonably
c	arbitrary values to make the output dataset appear mildly
c	sensible.
c@ model
c	Input image model. No default. The model should have units of 
c       Jy/pixel. eg. a clean component model. Only a two-dimensional 
c       model is handled.
c@ out
c	Output visibility dataset. No default.
c@ region
c	The region of interest in the model. The model is treated as
c	zero outside the region of interest. See the help on "region"
c	for more information.
c@ select
c	Normal visibility data selection. See the help on "select" for
c	more information. Currently only 'uvrange' selection is supported.
c--
c  History:
c    11-Feb-97 rjs  Preliminary version.
c    14-Feb-97 rjs  Correct horrendous bug.
c    07-jul-97 rjs  Change coaxdesc to coaxget, and arg of covelset.
c    10-may-01 dpr  Doc change only.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
c
	character version*(*)
	integer MAXSELS,MAXBOXES,MAXRUNS
	parameter(version='Im2uv: version 1.0 11-Feb-97')
	parameter(MAXSELS=1000,MAXBOXES=2048,MAXRUNS=3*MAXDIM)
	character model*64,vis*64
	real sels(MAXSELS),xpix,ypix,dx,dy
	double precision time,freq
	integer boxes(MAXBOXES),runs(3,MAXRUNS),xmin,xmax,ymin,ymax
	integer nsize(2),tIn,tOut,pnt,nRuns,n1,n2
	logical doSel
c
c  Externals.
c
	integer nextpow2
	logical keyprsnt
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('model',model,' ')
	if(model.eq.' ')call bug('f','Input model must be given')
	call keya('out',vis,' ')
	if(vis.eq.' ')call bug('f','Output vis dataset must be given')
	doSel = keyprsnt('select')
	call selInput('select',sels,MAXSELS)
	call boxInput('region',model,boxes,MAXBOXES)
	call keyfin
c
c  Open the input model and output vis datasets.
c
	call xyopen(tIn,model,'old',2,nsize)
	call uvopen(tOut,vis,'new')
	call SetUp(tIn,tOut,version,xpix,ypix,dx,dy,time,freq)
c
c  Set the region of interest.
c
	call boxMask(tIn,boxes,MAXBOXES)
	call boxSet(boxes,2,nsize,' ')
	call boxRuns(1,1,' ',boxes,Runs,MAXRUNS,nRuns,
     *					xmin,xmax,ymin,ymax)
c
c  Determine the size of the Fourier transform.
c
	n1 = max(nextpow2(nsize(1)),16)
	n2 = max(nextpow2(nsize(2)),16)
	if(max(n1,n2).gt.MAXDIM)
     *	  call bug('f','Image too big for me to handle')
c
c  Get some memory.
c
	call memAlloc(pnt,(n1/2+1)*n2,'c')
c
c  Do the real work.
c
	call DoFFT(tIn,memc(pnt),nsize(1),nsize(2),n1,n2,Runs,nRuns,
     *							xpix,ypix)
	call WriteOut(tOut,memc(pnt),n1,n2,dx,dy,time,doSel,Sels,freq)
c
c  Tidy up and close down.
c
	call memFree(pnt,(n1/2+1)*n2,'c')
	call xyclose(tIn)
	call uvclose(tOut)
c
	end
c************************************************************************
	subroutine DoFFT(tIn,Buf,nx,ny,n1,n2,Runs,nRuns,xpix,ypix)
c
	implicit none
	integer tIn,nx,ny,n1,n2,nRuns,Runs(3,nRuns+1)
	real xpix,ypix
	complex Buf(n2,(n1/2+1))
c
c  Read the image, blank out uninteresting regions, and FFT it.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	real data(MAXDIM),shift(MAXDIM)
	complex cdata(MAXDIM)
	integer ic,jc,i,j,iprev,r,i0,j0,offset
	real dx,dy,theta,s
	complex w
c
c  Determine the reference pixel to use.
c
	call output('Doing the transform ...')
	ic = nint(xpix)
	if(ic.lt.1.or.ic.gt.nx)ic = nx/2 + 1
	jc = nint(ypix)
	if(jc.lt.1.or.jc.gt.ny)jc = ny/2 + 1
c
c  Zero the shifting array.
c
	do i=nx-ic+2,n1-ic+1
	   data(i) = 0
	enddo
c
c  Read the file and do the first pass of the FFT.
c
	r = 1
	do j=1,ny
	  iprev = 0
c
c  Get the data for this row, and zero out anything that we do not want.
c
	  dowhile(Runs(1,r).eq.j)
	    if(iprev.eq.0)call xyread(tIn,j,Data)
	    do i=iprev+1,Runs(2,r)
	      Data(i) = 0
	    enddo
	    iprev = Runs(3,r)
	    r = r + 1
	  enddo
c
c  Now shift and FFT the array, and copy it to the buffer.
c
	  if(iprev.ne.0)then
	    do i=iprev+1,nx
	      Data(i) = 0
	    enddo
	    offset = ic - 1
	    do i=1,nx-ic+1
	      Shift(i) = Data(i+offset)
	    enddo
	    offset = -(n1-ic+1)
	    do i=n1-ic+2,n1
	      Shift(i) = Data(i+offset)
	    enddo
	    call fftrc(shift,cdata,1,n1)
	    do i=1,n1/2+1
	      Buf(j,i) = cdata(i)
	    enddo
c
c  If there was not data, we just zero the buffer.
c
	  else
	    do i=1,n1/2+1
	      Buf(j,i) = 0
	    enddo
	  endif
	enddo
c
c  Now the second pass of the Fourier transform.
c
	do j=ny-jc+2,n2-jc+1
	  cdata(j) = 0
	enddo
	do i=1,n1/2+1
	  offset = jc-1
	  s = 1
	  do j=1,ny-jc+1
	    cdata(j) = s*Buf(j+offset,i)
	    s = -s
	  enddo
	  offset = -(n2-jc+1)
	  if(2*(jc/2).eq.jc)then
	    s = -1
	  else
	    s =  1
	  endif
	  do j=n2-jc+2,n2
	    cdata(j) = s*Buf(j+offset,i)
	    s = -s
	  enddo
	  call fftcc(cdata,Buf(1,i),1,n2)
	enddo
c
c  Apply shift if needed.
c
	i0 = 1
	j0 = n2/2 + 1
	dx = xpix - ic
	dy = ypix - jc
	if(abs(dx)+abs(dy).gt.1e-4)then
	  call output('Performing fractional pixel shift ...')
	  do i=1,n1/2+1
	    do j=1,n2
	      theta = -2*PI*(dx*real(i-i0)/real(n1) +
     *			     dy*real(j-j0)/real(n2))
	      w = cmplx(cos(theta),sin(theta))
	      buf(j,i) = w*buf(j,i)
	    enddo
	  enddo
	endif
c
	end
c************************************************************************
	subroutine WriteOut(tOut,Buf,n1,n2,dx,dy,time,doSel,Sels,freq)
c
	implicit none
	integer n1,n2,tOut
	complex Buf(n2,n1/2+1)
	logical doSel
	real Sels(*),dx,dy
	double precision time,freq
c
c  Write out the visibility data.
c
c------------------------------------------------------------------------
	integer i,j
	double precision preamble(5)
	logical ok
c
c  Externals.
c
	logical selProbe
c
	call output('Writing the output ...')
	do i=1,n1/2+1
	  do j=1,n2
	    preamble(1) = real(i-1)/real(n1)*dx
	    preamble(2) = real(j-(n2/2+1))/real(n2)*dy
	    preamble(3) = 0
	    preamble(4) = time
	    preamble(5) = 258
	    if(doSel)then
	      ok = selProbe(sels,'uvrange',
     *		freq*sqrt(real(preamble(1)**2+preamble(2)**2)))
	    else
	      ok = .true.
	    endif
	    if(ok)call uvwrite(tOut,preamble,Buf(j,i),.true.,1)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine SetUp(tIn,tOut,version,xpix,ypix,dx,dy,obstime,freq)
c
	implicit none
	integer tIn,tOut
	real xpix,ypix,dx,dy
	character version*(*)
	double precision obstime,freq
c
c  Fill in all the details about the output dataset.
c
c  Things currently not written, which perhaps could be:
c    lst,chi,latitude,longitud,wsystemp
c------------------------------------------------------------------------
	integer iax,pol
	double precision t,crpix,cdelt,restfreq,ra,dec,obsra,obsdec
	double precision sfreq,sdf,f0
	character ctype*32,object*32,observer*32,telescop*32,line*80
	real vobs,epoch
c
c  Set up the output dataset.
c
	call uvset(tOut,'data','wide',0,1.,1.,1.)
	call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call uvputvri(tOut,'nants',2,1)
	call uvputvrr(tOut,'inttime',10.0,1)
c
	call coInit(tIn)
c
c  Frequency info.
c
	call coFindAx(tIn,'frequency',iax)
	if(iax.eq.0)then
	  ctype = 'FREQ-OBS'
	  sfreq = 1.4
	  sdf = 0.1
	  f0 = sfreq
	else
	  call coVelSet(tIn,'freq')
	  call coAxGet(tIn,iax,ctype,crpix,f0,sdf)
	  call coCvt1(tIn,iax,'ap',1.d0,'aw',sfreq)
	endif
	call uvputvrr(tOut,'wfreq',real(sfreq),1)
	call uvputvrr(tOut,'wwidth',real(sdf),1)
	freq = sfreq
c
c  Rest frequency and velocity info.
c
	ctype(1:4) = 'VELO'
	call uvputvra(tOut,'veltype',ctype)
	call rdhdd(tIn,'restfreq',restfreq,0.d0)
	call uvputvrd(tOut,'restfreq',restfreq,1)
	call rdhdr(tIn,'vobs',vobs,0.)
	call uvputvrr(tOut,'vsource',0.0,1)
	call uvputvrr(tOut,'veldop',vobs,1)
c
c  RA and DEC info.
c
	call coFindAx(tIn,'ra',iax)
	if(iax.ne.1)call bug('f','First axis is not an RA axis')
	call coAxGet(tIn,iax,ctype,crpix,ra,cdelt)
	call rdhdd(tIn,'obsra',obsra,ra)
	dx = 1/(cdelt*f0)
	xpix = crpix
	call uvputvrd(tOut,'ra',ra,1)
	call uvputvrd(tOut,'obsra',obsra,1)
c
	call coFindAx(tIn,'dec',iax)
	if(iax.ne.2)call bug('f','Second axis is not a DEC axis')
	call coAxGet(tIn,iax,ctype,crpix,dec,cdelt)
	call rdhdd(tIn,'obsdec',obsdec,dec)
	dy = 1/(cdelt*f0)
	ypix = crpix
	call uvputvrd(tOut,'dec',dec,1)
	call uvputvrd(tOut,'obsdec',obsdec,1)
c
c  Equinox info.
c
	call rdhdr(tIn,'epoch',epoch,2000.0)
	call uvputvrr(tOut,'epoch',epoch,1)
c
c  Polarisation info.
c
	call coFindAx(tIn,'stokes',iax)
	if(iax.eq.0)then
	  pol = 1
	else
	  call coCvt1(tIn,iax,'ap',1.d0,'aw',t)
	  pol = nint(t)
	  if(pol.eq.0)pol = 1
	endif
	call uvputvri(tOut,'npol',1,1)
	call uvputvri(tOut,'pol',pol,1)
c
c  Observation time.
c
	call rdhdd(tIn,'obstime',obstime,0.d0)
	if(obstime.eq.0.d0)call todayjul(obstime)
c
c  Object, observer and telescope.
c
	call rdhda(tIn,'object',object,'im2uv-model')
	call uvputvra(tOut,'source',object)
	call rdhda(tIn,'observer',observer,'im2uv-observer')
	call uvputvra(tOut,'observer',observer)
	call rdhda(tIn,'telescop',telescop,'im2uv-telescop')
	call uvputvra(tOut,'telescop',telescop)
c
c  History file.
c
	call hdcopy(tIn,tOut,'history')
	call hisopen(tOut,'append')
	line = 'IM2UV: Miriad Im2uv '//version
	call hisWrite(tOut,line)
	call hisInput(tOut,'IM2UV')
	call hisClose(tOut)
c
	call coFin(tIn)
c
	end
