c************************************************************************
c  A set of subroutines to form an image from visibility data.
c
c  User callable routines are:
c    MapIni(mode,tscr,nvis,npnt,umax,vmax,offcorr,totchan)
c    MapFin
c    MapScale(ichan)
c    Mapper(ichan,pMap)
c
c  History:
c    rjs  25oct94 Original version.
c    rjs  22nov94 Array bound vioolation in mapfft2, which affected images
c		  that were larger than maxdim/2.
c    rjs   8dec94 Check MAXDIM is big enough. Better messages.
c    rjs  12jan95 Fixed bug dealing with the amount of memory to allocate.
c    rjs  13jan95 Second try at the above.
c    rjs  16jan93 A third try at the above.
c    rjs   4aug95 Check that the beam is non-zero. Bug out if not.
c    rjs  13dec95 Set minimum transform size to be 16 (FFT limitation).
c    rjs  07jan97 Fiddle memory conservation alogirthm yet again.
c    rjs  03apr09 Change use of scratch file to help large file access.
c************************************************************************
	subroutine MapFin
c
	implicit none
c
c  Finish and tidy up.
c------------------------------------------------------------------------
	include 'mapper.h'
	if(nBuff.gt.0)call memFrep(pBuff,nBuff,'r')
	nBuff = 0
	end
c************************************************************************
	subroutine MapIni(mode1,tscr1,nvis1,npnt1,umax1,vmax1,
     *						offcorr1,totchan1)
c
	implicit none
	character mode1*(*)
	integer tscr1,npnt1,offcorr1,totchan1,nvis1
	real umax1,vmax1
c
c  Set the basic mapping parameters.
c------------------------------------------------------------------------
	include 'mapper.h'
c
	tscr = tscr1
	nvis = nvis1
	npnt = npnt1
	umax = umax1
	vmax = vmax1
	offcorr = 2*offcorr1 - 1
	totchan = totchan1
	mode = mode1
	if(npnt.gt.1.and.mode.ne.'fft')call bug('f',
     *	  'Only FFT mode supported when gridding multiple pointings')
c
	chan1 = 0
	chan2 = 0
	nBuff = 0
	nt = 0
	ginit = .false.
c
	end
c************************************************************************
	subroutine MapDef(nchan1,nx1,ny1)
c
	implicit none
	integer nchan1,nx1,ny1
c
c  Define the size of a particular channel.
c------------------------------------------------------------------------
	include 'mapper.h'
	logical merge
c
c  Externals.
c
	character itoaf*6
c
	if(max(nx1,ny1).gt.MAXDIM)
     *	  call bug('f','MAXDIM value is too small, in mapper.for.'//
     *	   ' Current max image size is '//itoaf(MAXDIM))
c
	merge = nt.gt.0
	if(merge) merge = nx(nt).eq.nx1.and.ny(nt).eq.ny1
	if(merge)then
	  nchan(nt) = nchan(nt) + nchan1
	else
	  nt = nt + 1
	  if(nt.gt.MAXT)call bug('f','Too many different types of maps')
	  nx(nt) = nx1
	  ny(nt) = ny1
	  nchan(nt) = nchan1
	endif
c
	end
c************************************************************************
	subroutine MapScale(ichan)
c
	implicit none
	integer ichan
c
c  Set the factors to scale the maps by.
c    Input:
c      ichan	Channel to use in setting the scale.
c------------------------------------------------------------------------
	include 'mapper.h'
	include 'mem.h'
c
	integer i
	ptrdiff pMap
	real Sum
c
c  Do a gridding pass if necessary, and determine the offset of the
c  plane of interest.
c
	if(mode.eq.'fft')then
	  if(ichan.lt.chan1.or.ichan.gt.chan2)call MapGrid(ichan)
	  pMap = pBuff + 2*nu*nv*npnt*(ichan-chan1) + nextra
	  do i=1,npnt
	    call MapVSum(memr(pMap+(i-1)*2*nu*nv),nu*nv,Sum)
	    if(Sum.eq.0)call bug('f','No data found for pointing')
	    Scale(i) = 0.5/Sum
	  enddo
	else
	  if(npnt.gt.1)call bug('f',
     *	    'Cannot handle multiple pointings for DFT or MEDIAN mode')
	  call MapSlowS(tscr,nvis,offcorr+2*(ichan-1),
     *					offcorr+2*totchan-1,Sum)
	  if(Sum.eq.0)call bug('f','No data found for pointing')
	  if(mode.eq.'median')then
	    Scale(1) = nvis/Sum
	  else
	    Scale(1) = 1/Sum
	  endif
	endif
c
	end
c************************************************************************
	subroutine Mapper(ichan,pMap)
c
	implicit none
	integer ichan
	ptrdiff pMap
c
c  Get an image.
c
c  Input:
c    ichan	Channel to use in setting the scale.
c------------------------------------------------------------------------
	include 'mapper.h'
	include 'mem.h'
c
	integer ioff,ooff,i,pcent
	character line*64
c
c  Do a gridding pass if necessary.
c
	if(mode.eq.'fft')then
	  if(ichan.lt.chan1.or.ichan.gt.chan2)call MapGrid(ichan)
c
c  Now do the Fourier transform and grid correction of this plane.
c
	  ioff = 2*nu*nv*npnt*(ichan-chan1) + nextra
	  ooff = 0
	  do i=1,npnt
	    call MapFFT1(memr(pBuff+ioff),nu,nv,u0,v0,n2)
	    call MapFFT2(memr(pBuff),ioff,ooff,nu,nv,nxc,nyc,n1,u0,v0,
     *	      scale(i),xCorr(n1/2-nxc/2+1),yCorr(n2/2-nyc/2+1))
	    ioff = ioff + 2*nu*nv
	    ooff = ooff + nxc*nyc
	  enddo
c
c  Do the DFT and Median processing modes (pretty slow ...).
c
	else
	  if(npnt.gt.1)call bug('f',
     *	    'Cannot handle multiple pointings in DFT or MEDIAN mode')
	  if(ichan.lt.chan1.or.ichan.gt.chan2)call MapBufS(ichan)
	  call MapSlow(tscr,mode,nvis,offcorr+2*(ichan-1),
     *		offcorr+2*totchan-1,
     *		memr(pBuff+nxc*nyc),
     *		memr(pBuff+nxc*nyc+4*nvis),
     *		memr(pBuff),nxc,nyc,scale(1))
	endif
	pMap = pBuff
c
c  Keep the user awake.
c
	if(ichan.eq.chan2.and.ichan.lt.totchan)then
	  pcent = nint(100.0 * real(ichan) / real(totchan))
	  write(line,'(a,i3,a)')'Finished gridding ',pcent,'% ...'
	  call output(line)
	endif
c
	end
c************************************************************************
	subroutine MapBufS(ichan)
c
	implicit none
	integer ichan
c
c  Determine the size of the buffer, etc.
c------------------------------------------------------------------------
	include 'mapper.h'
	integer it
c
	chan2 = 0
	chan1 = ichan
	it = 0
	dowhile(chan2.lt.chan1)
	  it = it + 1
	  chan2 = chan2 + nchan(it)
	enddo
	nxc = nx(it)
	nyc = ny(it)
c
	if(nBuff.lt.nxc*nyc+5*nvis)then
	  if(nBuff.gt.0)call MemFrep(pBuff,nBuff,'r')
	  nBuff = nxc*nyc + 5*nvis
	  call MemAllop(pBuff,nBuff,'r')
	endif
c
	end
c************************************************************************
	subroutine MapVSum(Dat,n,Sum)
c
	implicit none
	integer n
	complex Dat(n)
	real Sum
c
c  Determine the sum of the complex data, and set this as the pointing
c  scale factor.
c------------------------------------------------------------------------
	integer i
	double precision temp
c
	temp = 0
	do i=1,n
	  temp = temp + real(Dat(i))
	enddo
c
	Sum = temp
	end
c************************************************************************
	subroutine MapGrid(ichan)
c
	implicit none
	integer ichan
c
c  Perform a gridding pass ...
c------------------------------------------------------------------------
	include 'mapper.h'
	include 'mem.h'
c
c  Initialise the gridder, if needed.
c
	if(.not.ginit)call MapGinit
	ginit = .true.
c
c  Determine things about grid sizes, buffers, centres, etc.
c
	call mapBuf(ichan)
c
c  Now actually do the work.
c
	call MapVis(tscr,cgf,ncgf,width,nvis,offcorr+2*(chan1-1),
     *	  chan2-chan1+1,offcorr+2*totchan-1,memr(pBuff+nextra),
     *	  nu,nv,npnt,u0,v0,n1,n2)
c
	end
c************************************************************************
	subroutine MapGinit
c
	implicit none
c
c  Initialise ready for gridding.
c------------------------------------------------------------------------
	include 'mapper.h'
c
	integer WIDE
	character func*(*)
	real alpha
	parameter(WIDE=6,alpha=1.0,func='spheroidal')
c
	integer nxm,nym,i
c
c  Externals.
c
	integer nextpow2
c
c  Determine the maximum image size.
c
	nxm = nx(1)
	nym = ny(1)
	do i=2,nt
	  nxm = max(nxm,nx(i))
	  nym = max(nym,nx(i))
	enddo
c
c  Determine the transform size.
c
	n1 = max(nextpow2(nxm),16)
	n2 = max(nextpow2(nym),16)
c
c  Get the convolutional gridding function.
c
	width = WIDE
	ncgf = width * ( (MAXCGF-1)/width ) + 1
	call gcffun(func,cgf,ncgf,width,alpha)
	call corrfun(func,xcorr,n1,width,alpha)
	call corrfun(func,ycorr,n2,width,alpha)
c
	end
c************************************************************************
	subroutine mapBuf(ichan)
c
	implicit none
	integer ichan
c
c  Determine the gridding/mapping buffer size to be used, etc.

c  Input:
c    ichan	Channel to grid.
c  Output (in common):
c    chan1,chan2 Range of channels to grid.
c    nu,nv	Grid size (in complex elements).
c    u0,v0	Pixel coordinate of the origin of the uv plane.
c    nextra	Number of "extra" things to allocate (to hold a final
c		image).
c    pBuff	Pointer to the buffer.
c    nBuff	The number of elements allocated.
c------------------------------------------------------------------------
	include 'mapper.h'
	integer plsize,npass,it,maxplane,nplanes
c
c  Externals.
c
	integer memBuf
c
c  Determine the range of things to possibly grid.
c
	chan2 = 0
	chan1 = ichan
	it = 0
	dowhile(chan2.lt.chan1)
	  it = it + 1
	  chan2 = chan2 + nchan(it)
	enddo
	maxplane = chan2 - chan1 + 1
	nxc = nx(it)
	nyc = ny(it)
c
	nu = 2*int(umax*n1 + 0.5*width) + 1
	nv = 2*int(vmax*n2 + 0.5*width) + 1
	nu = min(nu,n1-1)
	nv = min(nv,n2-1)
	nu = nu/2 + width/2 + 1
c
	nv = max(nv,nyc)
c	nu = max(nu,(nxc-1)/2+1)
c
	u0 = width/2 + 1
	v0 = nv/2 + 1
c
c  Determine some things ...
c    plsize -- the size, in REALs of the grid needed for a plane.
c    nextra -- the number of extra REALs needed so that we can hold a plane
c	       in place after FFTing.
c    nplanes -- Number of planes we can fit in a reasonable amount of
c		memory.
c    npass   -- Number of i/o passes needed to grid all these images.
c
c  Check for integer overflow 
c
        if (log(2.0*nu)+log(1.0*nv)+log(1.0*npnt).gt.31*log(2.0)) 
     *   call bug('f','Too many pointings for this image size') 
	plsize= 2*nu*nv*npnt
	nextra = max(0, npnt*nxc*nyc - 2*nu*((npnt-1)*nv+(v0+nyc/2-1)),
     *		        nxc*nyc-2*nu*nyc-2*((u0-1)+nu*(v0-(nyc/2+1))) )
	nextra = 2*((nextra+1)/2)
c
	nplanes = max(nBuff-nextra,memBuf()-nextra,plsize)/plsize
	nplanes = min(nplanes,maxplane)
	npass = (maxplane-1)/nplanes + 1
	nplanes = (maxplane-1)/npass + 1
c
c  Is the current buffer big enough? If not, make it big enough.
c
	if(nplanes*plsize+nextra.gt.nBuff)then
	  if(nBuff.gt.0)call memFrep(pBuff,nBuff,'r')
	  nBuff = nplanes * plsize + nextra
	  call memAllop(pBuff,nBuff,'r')
	endif
c
c  Set the channel range that we will grid.
c
	chan1 = ichan
	chan2 = chan1 + nplanes - 1
c
	end
c************************************************************************
	subroutine MapVis(tvis,cgf,ncgf,width,nvis,nstart,ncount,
     *	  VisSize,Grd,nu,nv,npnt,u0,v0,n1,n2)
c
	implicit none
	integer tvis,ncgf,width,nu,nv,u0,v0,n1,n2
	integer nstart,ncount,npnt,nvis,VisSize
	real cgf(ncgf)
	complex Grd(nu,nv,npnt,ncount)
c
c  The start of the gridding process.
c
c  Inputs:
c    tvis	Handle of the visibility scratch file.
c    width	Width of gridding convolution function.
c    cgf	Tabulated values of gridding convolutiuon function.
c    ncgf	Number of tabulated values of gridding convolution function.
c    nu,nv	Size of grid array.
c    u0,v0	Index of grid point corresponding to (u,v) = (0,0).
c    n1,n2	Transform size.
c    nvis	Number of visibilities.
c    VisSize	Size of each visibility record.
c    nstart	First frequency channel to map.
c    ncount	Number of frequency channels to map.
c    npnt	Number of pointings to map.
c
c  Output:
c    Grd	Gridded visibiliites.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer maxrun,maxwidth
	parameter(maxrun=8*MAXCHAN+20,maxwidth=8)
	integer i,j,k,ktot,ltot,VispBuf,pnt
	integer poff(maxwidth**2),qoff(maxwidth**2),goff(maxwidth**2)
	real Visibs(maxrun)
c
c  Some constants.
c
	VispBuf = Maxrun/VisSize
	if(VispBuf.eq.0)
     *	  call bug('f','Too many channels for buffer in GridVis')
c
c  Initialise the index arrays used to make the gridding process
c  vectorise.
c
	if(width.gt.maxwidth)
     *	  call bug('f','Convolving function too large in MapVis')
	call MapIndx(ncgf,width,nu,poff,qoff,goff)
c
c  Zero the grid array.
c
	do k=1,ncount
	  do pnt=1,npnt
	    do j=1,nv
	      do i=1,nu
	        Grd(i,j,pnt,k) = 0
	      enddo
	    enddo
	  enddo
	enddo
c
c  Loop through the visibilities, gridding the appropriate ones.
c
	call scrrecsz(tvis,VisSize)
	k = 0
	ktot = nvis
	dowhile(k.lt.ktot)
	  ltot = min(VispBuf,ktot-k)
	  call scrread(tvis,Visibs,k,ltot)
	  call Mapit(Visibs,ltot,nstart,ncount,npnt,VisSize,
     *	    Grd,nu,nv,u0,v0,n1,n2,Cgf,ncgf,width,poff,qoff,goff)
	  k = k + ltot
	enddo
c
	end
c***********************************************************************
	subroutine Mapit(Vis,nvis,offset,ncount,npnt,size,
     *		Grd,nu,nv,u0,v0,n1,n2,
     *		cgf,ncgf,width,poff,qoff,goff)
c
	implicit none
	integer nVis,size,offset,ncount,npnt
	integer nu,nv,u0,v0,n1,n2
	complex Grd(nv*nu,npnt,ncount)
	real Vis(size,nvis)
	integer ncgf,width
	integer poff(width*width),qoff(width*width),goff(width*width)
	real cgf(ncgf)
c
c  Grid a buffer of visibilities. This is the version for a vector
c  machine.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer InU,InV,InPnt
	parameter(InU=1,InV=2,InPnt=4)
	integer i,j,k,l,uu,vv,p0,q0,g0,gg,pp,qq,Step,chan,pnt
	complex Dat,Dat1
	real Weight,u,v,hwd,du,dv
	logical ok
c
c  Initialise.
c
	Step = (Ncgf-1)/Width
	hwd = 0.5 * (width - 1)
c
c  Loop thru this buffer of visibilities. The early part is the
c  initialisation for the most important loop in the code.
c
c  Convert u and v to grid units and work out the limits of the convolving
c  region.
c
	do l=1,nvis
	  u = Vis(InU,l) * n1
	  v = Vis(InV,l) * n2
c
c  Conjugate the data, if needed.
c
	  if(u.lt.0)then
	    u = -u
	    v = -v
	    k = offset
	    do chan=1,ncount
	      Vis(k+1,l) = -Vis(k+1,l)
	      k = k + 2
	    enddo
	  endif
c
c  Determine where to grid it.
c
	  pnt = nint(Vis(InPnt,l))
c
	  uu = nint(u - hwd)
	  du = u - uu
	  uu = uu + u0 - 1
	  vv = nint(v - hwd)
	  dv = v - vv
	  vv = vv + v0 - 1
c
c  Check if the data fits on the grid. If not, discard it. Otherwise
c  grid it.
c
	  ok = uu.ge.0.and.uu+width.le.nu.and.vv.ge.0.and.vv+width.le.nv
	  if(ok)then
	    g0 = uu + vv*nu
c
	    p0 = ncgf/2 - nint( Step * du )
	    q0 = ncgf/2 - nint( Step * dv )
c
	    if(ncount.lt.width)then
	      k = offset
	      do chan=1,ncount
	        Dat = cmplx(Vis(k,l),Vis(k+1,l))
#ifdef vector
c#ivdep
c#maxloop 64
	        do i=1,width*width
	          Weight = cgf(p0+poff(i)) * cgf(q0+qoff(i))
	          Grd(g0+  goff(i),pnt,chan) = 
     *		  Grd(g0+  goff(i),pnt,chan) + Weight * Dat
	        enddo
#else
	        qq = q0 + 1
	        gg = g0
	        do j=1,width
		  Dat1 = cgf(qq) * Dat
		  pp = p0 + 1
		  do i=1,width
		    Grd(gg+i,pnt,chan) =
     *		    Grd(gg+i,pnt,chan) + cgf(pp) * Dat1
		    pp = pp + Step
		  enddo
		  qq = qq + Step
		  gg = gg + nu
	        enddo
#endif
	        k = k + 2
	      enddo
c
	    else
	      do i=1,width*width
	        Weight = cgf(p0+poff(i)) * cgf(q0+qoff(i))
	        gg = g0 + goff(i)
	        k = offset
	        do chan=1,ncount
	          Dat = cmplx(Vis(k,l),Vis(k+1,l))
		  Grd(gg, pnt,chan) =
     *		  Grd(gg, pnt,chan) + Weight * Dat
		  k = k + 2
	        enddo
	      enddo
	    endif
	  endif
c
	enddo
c
	end
c************************************************************************
	subroutine MapIndx(ncgf,width,nu,poff,qoff,goff)
c
	implicit none
	integer ncgf,width,nu
	integer poff(width*width),qoff(width*width),goff(width*width)
c
c  Initialise arrays used to help make the inner gridding loop
c  vectorise.
c
c  Input:
c    ncgf	Number of tabulated values of the convolution function.
c    width	Width of the convolution function.
c    nu		Width of grid array.
c
c  Output:
c    poff	Convolution array index, in x.
c    qoff	Convolution array index, in y.
c    goff	Grid array index.
c
c------------------------------------------------------------------------
	integer i,j,k,p0,q0,g0,g0d,Step
c
c  Initialise the index arrays used by the gridding routine. This will
c  not vectorise, but who cares.
c
	Step = (Ncgf-1)/Width
	if(Step*Width+1.ne.Ncgf)
     *	  call bug('f','Ncgf not mult. of Step in IndxIni')
	k = 0
	q0 = 1
	g0d = 1
	do j=1,width
	  p0 = 1
	  g0 = g0d
	  do i=1,width
	    k = k + 1
	    poff(k) = p0
	    qoff(k) = q0
	    goff(k) = g0
	    p0 = p0 + step
	    g0 = g0 + 1
	  enddo
	  q0 = q0 + step
	  g0d = g0d + nu
	enddo
	end
c************************************************************************
	subroutine MapFFT2(Grd,inoff,outoff,nu,nv,nx,ny,n1,u0,v0,Scale,
     *							xCorr,yCorr)
c
	implicit none
	integer nv,nu,nx,ny,u0,v0,n1,inoff,outoff
	real Scale,xCorr(nx),yCorr(ny)
	real Grd(*)
c
c  Perform second pass of FFT, apply grid corrections and scaling, and
c  write out the result. The operations are done in place.
c
c  Input:
c    nu,nv	Size of the grid array.
c    u0,v0	Index of the centre of the grid array.
c    nx,ny	Output map size.
c    n1		Transform size.
c    Scale	Scale factor to apply.
c    xCorr	)  Gridding corrections.
c    yCorr	)
c  Input/Output:
c    Grd	Gridded visibility, which has been through the first
c		pass FFT. On output, this is overwritten with the
c		output map.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	real cdata(maxdim+2),rdata(maxdim),scale1
	integer i,j,offi,offo,nud,ioff
c
	offi = 2*( (u0-1) + nu*(v0-(ny/2 + 1)) ) + inoff
	offo = outoff

c
	nud = nu - u0 + 1
c
	do i=2*nud+1,n1+2
	  cdata(i) = 0
	enddo
c
	do j=1,ny
	  if(offo.gt.offi)call bug('f',
     *		'Memory conservation algorithm failed, in Mapper')
	  do i=1,2*nud
	    cdata(i) = Grd(i+offi)
	  enddo
	  call fftcr(cdata,rdata,-1,n1)
	  scale1 = scale * ycorr(ny/2+1) * xcorr(nx/2+1) / ycorr(j)
	  ioff = n1/2 - nx/2
	  do i=1,nx
	    Grd(i+offo) = rdata(i+ioff) * scale1 / xcorr(i)
	  enddo
c
	  offi = offi + 2*nu
	  offo = offo + nx
	  if(offo.gt.offi)call bug('f',
     *		'Memory conservation algorithm failed, in Mapper')
	enddo
c
	end
c************************************************************************
	subroutine MapFFT1(Grd,nu,nv,u0,v0,n2)
c
	implicit none
	integer nv,nu,u0,v0,n2
	complex Grd(nu,nv)
c
c  This takes the gridded visibility, performs fudges on it to put it into
c  a state appreciated by the FFT routines, and then the first pass FFT.
c  The fudges it performs are,
c   1.	To reflect the small number of grid cells, with negative values of
c	u into positive values of u (by conjugating it and adding it to the
c	appropriate positive cell).
c   2.  Switch the ordering of the data (so that the pixel corresponding
c	to (u,v)=(0,0) gets shifted to pixel (1,1), and multiply by
c	(-1)**(i+j), so that the output map has the centre at pixel
c	(nx/2+1,ny/2+1).
c
c  Inputs:
c    u0,v0	Index of the pixel in the grid array corresponding to
c		(u,v) = (0,0).
c    nu,nv	Input gridded visibility data size.
c    n2		Size of FFT to use.
c
c  Input/Output:
c    Grd	Input gridded visibility. Destroyed in the FFT process.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	integer i,j,id,jd,nd,nl,n,joff
	complex cdat1(maxdim),cdat2(maxdim),temp
c
	nd = nv / 2
	if(v0.ne.nd+1)call bug('f','Assumption failed in MapFFT1')
	nl = (nv-1)/2
c
c  Zero the middle section of the array which we never use.
c
	do j=nl+2,n2-nl
	  cdat1(j) = 0
	enddo
c
c  Add negative part of u if necessary. The point at nd+1 is done manually
c  to avoid a vector dependency when jd = j.
c
	do i=u0,nu
	  if(i.lt.2*u0)then
	    id = 2*u0 - i
	    jd = nv
	    do j=2*v0-nv,nd
	      temp = Grd(i,j)  + conjg(Grd(id,jd))
	      Grd(i,jd) = Grd(i,jd) + conjg(Grd(id,j))
	      Grd(i,j)  = temp
	      jd = jd - 1
	    enddo
	    Grd(i,nd+1) = Grd(i,nd+1) + conjg(Grd(id,nd+1))
	  endif
c
c  Copy the data, performing the shift and multiplication by (-1)**(i+j).
c  It is not the most elegant code, but it vectorises without any help.
c
	  if(2*((i-u0)/2).eq.(i-u0))then
	    n = 2*((nl+1)/2)
	    do j=1,n,2
	      cdat1(j)   =   Grd(i,j+v0-1)
	      cdat1(j+1) = - Grd(i,j+v0)
	    enddo
	    if(n.ne.nl+1)then
	      cdat1(nl+1)    =   Grd(i,v0+nl)
	    else
	      cdat1(n2-nl+1) = - Grd(i,v0-nl)
	    endif
	    n = 2*(nl/2)
	    do j=1,n,2
	      cdat1(j+n2-n)   =   Grd(i,j+v0-n-1)
	      cdat1(j+n2-n+1) = - Grd(i,j+v0-n)
	    enddo
	  else
	    n = 2*((nl+1)/2)
	    do j=1,n,2
	      cdat1(j)   = - Grd(i,j+v0-1)
	      cdat1(j+1) =   Grd(i,j+v0)
	    enddo
	    if(n.ne.nl+1)then
	      cdat1(nl+1)    = - Grd(i,v0+nl)
	    else
	      cdat1(n2-nl+1) =   Grd(i,v0-nl)
	    endif
	    n = 2*(nl/2)
	    do j=1,n,2
	      cdat1(j+n2-n)   = - Grd(i,j+v0-n-1)
	      cdat1(j+n2-n+1) =   Grd(i,j+v0-n)
	    enddo
	  endif
c
c  Perform the FFT on this column.
c
	  call fftcc(cdat1,cdat2,-1,n2)
c
c  Copy the column back.
c
	  joff = (n2/2 + 1) - v0
	  do j=1,nv
	    Grd(i,j) = cdat2(j+joff)
	  enddo
c
	enddo
	end
c************************************************************************
	subroutine MapSlowS(tscr,nvis,offcorr,VisSize,Sum)
c
	implicit none
	integer tscr,nvis,offcorr,VisSize
	real Sum
c
c  Determine the sum of the visibilities.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXRUN
	parameter(MAXRUN=1024)
	real Vis(MAXRUN)
	double precision temp
	integer k,ktot,l,ltot,l0,VispBuf
c
c  Determine the number of visibilities we can fit into a buffer.
c
	VispBuf = MAXRUN/VisSize
	if(VispBuf.lt.1)
     *		call bug('f','Buffer size too small in MapSlowS')
	call scrrecsz(tscr,VisSize)
c
	temp = 0
	k = 0
	ktot = nvis
	dowhile(k.lt.ktot)
	  ltot = min(VispBuf,ktot-k)
	  call scrread(tscr,Vis,k,ltot)
	  l0 = offcorr
	  do l=1,ltot
	    temp = temp + Vis(l0)
	    l0 = l0 + VisSize
	  enddo
	  k = k + ltot
	enddo
c
	Sum = temp
c
	end	
c************************************************************************
	subroutine MapSlow(tscr,mode,nvis,offcorr,VisSize,
     *				Dat,Wrk,Map,nx,ny,scale)
c
	implicit none
	integer tscr,nvis,offcorr,VisSize,nx,ny
	real Dat(4,nvis),Wrk(nvis),Map(nx,ny),scale
	character mode*(*)
c
c  Make an image the slow way -- either a DFT, or slower still
c  a median transform.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer MAXRUN
	parameter(MAXRUN=1024)
	real Vis(MAXRUN),theta
	integer VispBuf,k,ktot,l,ltot,l0,i0,j0,i,j
	double precision dtemp
c
c  Determine the number of visibilities that we can fit in per
c  record.
c
	VispBuf = MAXRUN/VisSize
	if(VispBuf.lt.1)call bug('f','Too many channels for me')
c
c  Read in all the data.
c
	k = 0
	ktot = nvis
	dowhile(k.lt.ktot)
	  ltot = min(VispBuf,ktot-k)
	  call scrread(tscr,Vis,k,ltot)
	  l0 = 0
	  do l=1,ltot
	    Dat(1,k+l) = Vis(l0+1)
	    Dat(2,k+l) = Vis(l0+2)
	    Dat(3,k+l) = Scale*Vis(l0+offcorr)
	    Dat(4,k+l) = Scale*Vis(l0+offcorr+1)
	    l0 = l0 + VisSize
	  enddo
	  k = k + ltot
	enddo
c
c  Now do the real work.
c
	i0 = nx/2 + 1
	j0 = ny/2 + 1
c
	do j=1,ny
	  do i=1,nx
c
c  Phase up the data.
c
	    do k=1,nvis
	      theta = -2*pi*( (i-i0)*Dat(1,k) + (j-j0)*Dat(2,k) )
	      Wrk(k) = cos(theta)*Dat(3,k) - sin(theta)*Dat(4,k)
	    enddo
c
c  Either take the median of the sum.
c
	    if(mode.eq.'median')then
	      call median(Wrk,nvis,Map(i,j))
	    else
	      dtemp = 0
	      do k=1,nvis
		dtemp = dtemp + Wrk(k)
	      enddo
	      Map(i,j) = dtemp
	    endif
	  enddo
	enddo
c
	end
