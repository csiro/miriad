c************************************************************************
	program imcomb
	implicit none
c
c= imcomb - Combine images
c& rjs
c: map combination
c+
c	IMCOMB is a MIRIAD task which combines several images into
c	one. The images are weighted in the region of overlap to minimise
c	the rms noise.
c@ in
c	Name of the input image datasets. Several can be given.
c	Wildcards are supported. At least one must be given.
c@ out
c	The name of the output dataset.
c@ rms
c	The rms noise levels of each of the input images. This is used in
c	weighting the images when combining them (and thus minimising the
c	noise level in any overlap region). Only the relative magnitude
c	of the noise values are important.
c
c	The images are weighted as 1/rms**2 -- so if you wish the images
c	to have speific weights, compute them accordingly. For example
c	if you wish to weight two images by the ratios 4 to 1, use
c	  rms = 0.5,1
c	where 0.5 is 1/sqrt(4)
c
c	The default is to use the theoretical rms of the input images found
c	in the image header. If this is missing, then the last valid rms
c	noise level found is used. If no values are given, and the first
c	dataset does not contain the rms value in the header, equal weights
c	are used for all images.
c@ options
c	Extra processing options. Several values can be given, separated
c	by a comma. Minimum match of names is used. Possible values are:
c	  nonormalise  Do not renormalise the output. Normally the output
c	               is normalised to account for overlap regions.
c--
c  History:
c    rjs  29nov94 Original version.
c    rjs  12jan95 COrrect alignment code on axis 3. Write mask file.
c    rjs   3aug95 Mask file was not correctly set for options=nonorm.
c    rjs  15mar97 Handle processing multiple more than MAXOPEN files.
c    rjs  02jul97 cellscal change.
c    rjs  21jul97 Handle image alignment better. Fix bug related to
c		  incorrect flagging checks.
c    rjs  23jul97 Added pbtype.
c    rjs  13nov98 Increase MAXIN.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	character version*(*)
	parameter(version='ImComb: version 1.0 21-Jul-97')
	integer MAXIN,MAXOPEN
	parameter(MAXIN=500,MAXOPEN=6)
c
	character in(MAXIN)*64,tin*64,out*64
	integer nrms,nin,tno(MAXIN),tOut,nsize(3,MAXIN),nOpen
	integer nOut(MAXNAX),minpix,maxpix,k,i,naxis,off(3)
	integer pData,pWts
	logical mosaic,nonorm,interp,equal
	real rms(MAXIN),rms0,blctrc(6,MAXIN)
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call mkeyf('in',in,MAXIN,nin)
	call keya('tin',tin,' ')
	call keya('out',out,' ')
	call mkeyr('rms',rms,MAXIN,nrms)
	call GetOpt(mosaic,nonorm)
	call keyfin
c
c  Check the inputs.
c
	if(nin.le.0)call bug('f','Input images must be given')
	if(out.eq.' ')call bug('f','An output image must be given')
c
c  Open the files, determine the size of the output. Determine the grid
c  system from the first map.
c
	if(nIn.le.maxOpen)then
	  nOpen = nIn
	else
	  nOpen = maxOpen - 1
	endif
c
	equal  = .false.
	do i=1,nIn
	  call xyopen(tno(i),In(i),'old',3,nsize(1,i))
	  call coInit(tno(i))
	  if(max(nsize(1,i),nsize(2,i)).gt.maxdim)
     *	    call bug('f','Input map is too big')

	  if(i.eq.1)then
	    call ThingIni(nsize(1,i),blctrc(1,i))
	    call rdhdi(tno(i),'naxis',naxis,3)
	    naxis = min(naxis,MAXNAX)
	  else
	    call ThingChk(tno(1),tno(i),nsize(1,i),interp,blctrc(1,i))
	    if(interp)call bug('f','Cannot interpolate')
	  endif
c
c  Check the rms value.
c
	  if(equal)then
	    rms(i) = 1
	  else if(nrms.lt.i)then
	    call rdhdr(tno(i),'rms',rms0,0.0)
	    if(rms0.gt.0)then
	      rms(i) = rms0
	    else if(i.eq.1)then
	      rms(i) = 1
	      equal = .true.
	    else
	      rms(i) = rms(i-1)
	    endif
	  endif
	  if(rms(i).le.0)call bug('f','Invalid rms value')
c
	  if(i.gt.nOpen)then
	    call coFin(tno(i))
	    call xyclose(tno(i))
	  endif
	enddo
c
c  Determine the size of the output.
c
	do k=1,3
	  minpix = nint(blctrc(k,1))
	  maxpix = nint(blctrc(k+3,1))
	  do i=2,nIn
	    minpix = min(minpix,nint(blctrc(k,  i)))
	    maxpix = max(maxpix,nint(blctrc(k+3,i)))
	  enddo
	  nOut(k) = maxpix - minpix + 1
	  off(k) = 1 - minpix
	  do i=1,nIn
	    blctrc(k,i) = blctrc(k,i) + off(k)
	    blctrc(k+3,i) = blctrc(k+3,i) + off(k)
	  enddo
	enddo
c
	do k=4,naxis
	  nout(k) = 1
	enddo
c
c  Create the output.
c
	call xyopen(tOut,out,'new',naxis,nOut)
	call hdout(tno(1),tOut,off,version)
c
c  Allocate arrays.
c
	call memAlloc(pData,nOut(1)*nOut(2),'r')
	call memAlloc(pWts,nout(1)*nOut(2),'r')
c
c  Process it.
c
	do k=1,nOut(3)
	  call CombIni(memr(pData),memr(pWts),nOut(1),nOut(2))
	  do i=1,nIn
	    if(i.gt.nOpen)call xyopen(tno(i),In(i),'old',3,nsize(1,i))
	    call Combo(k,tno(i),blctrc(1,i),nsize(1,i),1/rms(i)**2,
     *		       memr(pData),memr(pWts),nOut(1),nOut(2))
	    if(i.gt.nOpen)call xyclose(tno(i))
	  enddo
	  call CombFin(k,tOut,nonorm,
     *		       memr(pData),memr(pWts),nOut(1),nOut(2))
	enddo
c
c  Free arrays.
c
	call memFree(pData,nOut(1)*nOut(2),'r')
	call memFree(pWts, nout(1)*nOut(2),'r')
c
c  Close up.
c
	do i=1,nOpen
	  call xyclose(tno(i))
	enddo
	call xyclose(tOut)
	end
c************************************************************************
	subroutine GetOpt(mosaic,nonorm)
c
	implicit none
	logical mosaic,nonorm
c
c  Determine processing options.
c
c  Output:
c    mosaic
c    nonorm
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=2)
	logical present(NOPTS)
	character opts(NOPTS)*12
	data opts/'mosaic      ','nonormalise '/
c
	call options('options',opts,present,NOPTS)
c
	mosaic = present(1)
	nonorm = present(2)
c
	end
c************************************************************************
	subroutine hdout(tin,tout,off,version)
c
	implicit none
	integer tin,tout
	integer off(3)
	character version*(*)
c
c  Make up the header of the output file.
c
c  Input:
c    tin	The handle of the input file, which is to be used as a template.
c    tout	The handle of the output file.
c    off
c    version
c------------------------------------------------------------------------
	double precision crpix
	integer i
	character line*80,num*2
c
	integer nkeys
	parameter(nkeys=39)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*2
c
	data keyw/   'bunit   ','crval1  ','crval2  ','crval3  ',
     *	  'crval4  ','crval5  ','cdelt1  ','cdelt2  ','cdelt3  ',
     *	  'cdelt4  ','cdelt5  ','crpix4  ','crpix5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ',
     *	  'ctype5  ','obstime ','epoch   ','bmaj    ','bmin    ',
     *	  'bpa     ','niters  ','object  ','telescop','observer',
     *	  'restfreq','vobs    ','obsra   ','obsdec  ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','btype   ','pbfwhm  ',
     *	  'cellscal','pbtype  '/
c
c  Write out coordinate information.
c
	do i=1,3
	  num = itoaf(i)
	  call rdhdd(tIn,'crpix'//num,crpix,0.d0)
	  crpix = crpix + off(i)
	  call wrhdd(tOut,'crpix'//num,crpix)
	enddo
c
c  Copy other parameters.
c
	do i=1,nkeys
	  call hdcopy(tIn,tOut,keyw(i))
	enddo
c
c  Create the output history.
c
	call hdcopy(tin,tout,'history')
	call hisopen(tout,'append')
c
	line = 'IMCOMB: Miriad '//version
	call hiswrite(tout,line)
	call hisinput(tout,'IMCOMB')
	call hisclose(tout)
	end
************************************************************************
	subroutine ThingIni(nsize,blctrc)
c
	implicit none
	integer nsize(3)
	real blctrc(6)
c
c------------------------------------------------------------------------
	blctrc(1) = 1
	blctrc(2) = 1
	blctrc(3) = 1
	blctrc(4) = nsize(1)
	blctrc(5) = nsize(2)
	blctrc(6) = nsize(3)
c
	end
c************************************************************************
	subroutine ThingChk(tIn,tOut,nsize,interp,blctrc)
c
	implicit none
	integer tIn,tOut,nsize(3)
	logical interp
	real blctrc(6)
c
c------------------------------------------------------------------------
	double precision In(3),Out(3)
	integer i
c
	call pcvtinit(tOut,tIn)
	In(1) = 1
	In(2) = 1
	In(3) = 1
	call pcvt(In,Out,3)
	blctrc(1) = Out(1)
	blctrc(2) = Out(2)
	blctrc(3) = Out(3)
c
	In(1) = nsize(1)
	In(2) = nsize(2)
	In(3) = nsize(3)
	call pcvt(In,Out,3)
	blctrc(4) = Out(1)
	blctrc(5) = Out(2)
	blctrc(6) = Out(3)
c
	interp = .false.
	do i=1,3
	  interp = interp.or.
     *		   nint(blctrc(i+3))-nint(blctrc(i))+1.ne.nsize(i).or.
     *		   abs(nint(blctrc(i))-blctrc(i)).gt.0.05.or.
     *		   abs(nint(blctrc(i+3))-blctrc(i+3)).gt.0.05
	enddo
c
	end
c************************************************************************
	subroutine CombIni(Data,Wts,nx,ny)
c
	implicit none
	integer nx,ny
	real Data(nx*ny),Wts(nx*ny)
c
c  Zero the arrays.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nx*ny
	  Data(i) = 0
	  Wts(i) = 0
	enddo
c
	end
c************************************************************************
	subroutine Combo(k,tIn,blctrc,nsize,Wt,Data,Wts,nx,ny)
c
	implicit none
	integer k,tIn,nsize(3),nx,ny
	real Data(nx,ny),Wts(nx,ny),blctrc(6),Wt
c
c  Add the contribution of this image.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real Line(MAXDIM)
	logical flags(MAXDIM)
	integer ioff,joff,koff,jlo,jhi,ilo,ihi,i,j
c
	if(nsize(1).gt.MAXDIM)call bug('f','Image too big for me')
c
	ioff = 1 - nint(blctrc(1))
	joff = 1 - nint(blctrc(2))
	koff = 1 - nint(blctrc(3))
c
	if(k+koff.lt.1.or.k+koff.gt.nsize(3))return
	jlo = max(1,1-joff)
	jhi = min(ny,nsize(2)-joff)
	ilo = max(1,1-ioff)
	ihi = min(nx,nsize(1)-ioff)
c
	if(k+koff.gt.1)call xysetpl(tIn,1,k+koff)
	do j=jlo,jhi
	  call xyread(tIn,j+joff,line)
	  call xyflgrd(tIn,j+joff,flags)
	  do i=ilo,ihi
	    if(flags(i+ioff))then
	      Data(i,j) = Data(i,j) + Wt*Line(i+ioff)
	      Wts(i,j)  = Wts(i,j)  + Wt
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine CombFin(k,tOut,nonorm,Data,Wts,nx,ny)
c
	implicit none
	integer k,tOut,nx,ny
	logical nonorm
	real Data(nx,ny),Wts(nx,ny)
c
c  Normalise and write out the images.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	logical flags(MAXDIM)
c
	if(k.gt.1)call xysetpl(tOut,1,k)
c
	do j=1,ny
	  if(.not.nonorm)then
	    do i=1,nx
	      if(Wts(i,j).gt.0)Data(i,j) = Data(i,j) / Wts(i,j)
	      flags(i) = Wts(i,j).gt.0
	    enddo
	  else
	    do i=1,nx
	      flags(i) = Wts(i,j).gt.0
	    enddo
	  endif
	  call xywrite(tOut,j,Data(1,j))
	  call xyflgwr(tOut,j,flags)
	enddo
c
	end
