c************************************************************************
	program immerge
	implicit none
c
c= immerge -- Linear merging of images.
c& rjs
c: map combination
c+
c	IMMERGE is a Miriad task to linearly merge together two images with
c	different resolutions. The two images must be of the same field
c	and use the same coordinate system.
c
c	In combining the data, it is assumed that the low resolution
c	image better represents the short spacing data, whereas the
c	high resolution best represents the fine structure. Commonly,
c	the low resolution image will be a single-dish observation,
c	and the high resolution will be a mosaiced interferometric
c	observation.
c@ in
c	This gives the two images to be merged. The first input must be
c	the high resolution image, and the second the low. There is 
c	no default. The two images must be on the same coordinate grid.
c	If necessary, use REGRID to achieve this.
c@ out
c	The output image. No default.
c@ factor
c	The flux calibration factor. Ideally the two inputs will have
c	correctly calibrated flux scales, in units like Jy/beam. In this
c	case, the flux calibration factor would be 1. In practise, the
c	calibration may not be perfect, and an extra calibration factor
c	will be required to align the flux scales. "factor" gives the
c	factor to scale the low resolution image to put it on the same
c	flux scale as the high resolution image. Note that this factor is
c	in addition to the scaling needed to convert Jy/beam at a low
c	resolution to Jy/beam at the high resolution.
c
c	If no value is given for "factor", IMMERGE determines this by
c	comparing the values in the Fourier plane, of data within an
c	particular annulus (after accounting for the differing resolutions
c	of the two inputs). IMMERGE finds the scale factor which
c	minimises differences between the data of the two (in a robust/L1
c	sense) in the annulus.
c@ uvrange
c	This specifies an annulus, in the Fourier domain, where the
c	high and low resolution images should agree (after allowing
c	for resolution differences). This is the annulus of data used to
c	deduce the flux calibration factor, and with options=feather.
c
c	Two or three values can be given. The first two give the inner
c	and outer radius of the annulus in the Fourier domain. The third
c	argument gives the units. Possible units are "klambda"
c	(kilo-wavelengths), "meters", "feet" and "nanoseconds". The default
c	is "klambda".
c
c	Values for "uvrange" must be given either if "options=feather"
c	is used or if the flux calibration factor is being deduced.
c@ region
c	Region-of-interest parameter. See the help on ``region''
c	for more information. NOTE: This parameter is ONLY used for
c	determining the flux calibration factor. Only plane selection
c	(e.g. via the ``image'' command) is allowed. Typically you would
c	want to select a range of planes which contains significant signal
c	in the overlap region.
c@ device
c	PGPLOT device for a plot. When determining the flux calibration
c	factor, IMMERGE can produce a plot showing the correspondence
c	between the high and low resolution data points in the annulus
c	(after correcting for resolution effects and the deduced flux
c	calibration factor). Ideally it will show a line with "y=x".
c	The default is not to produce a plot. 
c@ guard
c	Before Fourier transforming, the images are padded with a guard
c	band. "guard" gives one or two values, being the minimum width of
c	this guard band in pixels, in the x and y directions. The actual
c	guard band used is such that the size of the image plus guard band
c	is a power of 2.
c@ options
c	Task enrichment parameters. Several can be given, separated by commas.
c	Minimum match is used.
c	  normalize  Rather than the output being the merged images, the
c	             output is the low resolution image corrected by
c	             the flux calibration factor.
c	  zero       By default, IMMERGE pads the guard band with data which
c	             minimizes FFT edge effects. If the input images are
c	             really zero beyond the edges of the two input images,
c	             then padding with zeros might be preferable. This is
c	             particularly so if IMMERGE is deducing the flux
c	             calibration scale factor.
c	  feather    This merges the two images together in a fashion similar
c	             to AIPS IMERG. This method is generally less desirable
c	             than the default scheme used by IMMERGE.
c	  shift      Determine the optimum shift to apply to the low
c	             resolution image to make it align with the high
c	             resolution one.
c	  notaper    Normally the low-resolution image is tapered to match any
c	             residual primary beam response in the high-resolution image.
c	             This option causes this step to be skipped.
c--
c
c  History:
c    rjs  12jul97 Original version.
c    rjs  16mar98 Added region parameter.
c    rjs  19aug98 Fix bug introduced on 16 March where the first plane
c		  in the output was the last plane in the selected region
c		  for factor determination.
c    rjs  02sep98 Subroutine name change only to avoid a LINUX conflict.
c    rjs  23jul99 Taper down the low resolution image to make it compatible
c	          with the high resolution one. Added shift option.
c    rjs  29mar00 Added options=notaper.
c  Bugs:
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Immerge: version 1.0 29-Mar-00')
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mirconst.h'
	include 'mem.h'
	integer MAXBOX
	parameter(MAXBOX=2048)
c
	integer pIn1,pIn2,pWt1,pWt2
	logical domerge,dofac,doout,dozero,dofeath,dotaper
	logical doshift,notaper
	integer n,ngx,ngy,lIn1,lIn2,lOut,iax,i,k,xoff,yoff,zoff
	integer nin(3),nout(MAXNAX),ntemp(3),naxis,ifail,npnt
	integer Box(MAXBOX)
	character In1*80,In2*80,out*80,device*64,line*80
	character mess1*64,mess2*64
	double precision freq1,freq2,cdelt1,cdelt2,x1(2),x2(2)
	real fac,uvlo,uvhi,freq,lambda,temp,du,dv,pfac
	real bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2
	real bmaj,bmin,bpa,bmajt,bmint,bpat,norm
	real sfac,sxx,sxy,syy,xs,ys
c
	integer NUNITS
	parameter(NUNITS=4)
	character units(NUNITS)*12,unit*12
c
c  Externals.
c
	logical keyprsnt
	integer nextpow2
c
c  Data statements.
c
	data units/'klambda     ','meters      ','nanoseconds ',
     *		   'feet        '/
c
	call output(version)
	call keyini
	call GetOpt(domerge,dozero,dofeath,doshift,notaper)
	call keyf('in',in1,' ')
	call keyf('in',in2,' ')
	if(in1.eq.' '.or.in2.eq.' ')
     *	  call bug('f','Two input files must be given')
	call keyi('guard',ngx,0)
	call keyi('guard',ngy,ngx)
	if(ngx.lt.0.or.ngy.lt.0)call bug('f','Invalid values for guard')
	dofac = .not.keyprsnt('factor')
	call keyr('factor',fac,1.0)
	if(dofac)then
	  call keya('device',device,' ')
	  call BoxInput('region',in1,box,MAXBOX)
	endif
	if(dofac.or.dofeath)then
	  call keyr('uvrange',uvlo,0.)
	  call keyr('uvrange',uvhi,-1.)
	  call keymatch('uvrange',NUNITS,units,1,unit,n)
	  if(n.eq.0)unit = units(1)
	  if(uvlo.ge.uvhi)
     *	    call bug('f','Invalid uvrange value')
	else
	  uvlo = 0
	  uvhi = -1
	endif
	call keya('out',out,' ')
	doOut = out.ne.' '
	if(.not.dofac.and..not.doout)
     *	  call bug('f','Inputs do not require any work')
	call keyfin
	dotaper = (domerge.or.dofac).and..not.notaper
c
c  Open the input datasets, and get their beam parameters.
c
	call xyopen(lIn1,in1,'old',3,nIn)
	call coInit(lIn1)
	call GetBeam(lIn1,in1,bmaj1,bmin1,bpa1,mess1)
	call coGetd(lIn1,'cdelt1',cdelt1)
	call coGetd(lIn1,'cdelt2',cdelt2)
c
	call xyopen(lIn2,in2,'old',3,ntemp)
	call coInit(lIn2)
	call GetBeam(lIn2,in2,bmaj2,bmin2,bpa2,mess2)
c
c  Check that the two images are compatible.
c
	do i=1,3
	  if(nIn(i).ne.nTemp(i))call bug('f','Incompatible image sizes')
	enddo
	call alignIni(lIn1,lIn2,nIn(1),nIn(2),nIn(3),xoff,yoff,zoff)
	if(xoff.ne.0.or.yoff.ne.0.or.zoff.ne.0)
     *	  call bug('f','Inputs are not aligned')
c
	call rdhdi(lIn1,'naxis',naxis,1)
	naxis = min(naxis,MAXNAX)
c
c  Round up the total sizes to the next power of two.
c
	ngx = nextpow2(nIn(1)+ngx)
	ngy = nextpow2(nIn(2)+ngy)
	if(max(ngx,ngy).gt.MAXDIM)
     *	  call bug('f','Image+guard size too big for me')
c
c  Determine which of the two images has the coarser resolution, and
c  determine the gaussian to convolve with to bring one to the same
c  resolution as the other.
c
	if(bmaj1*bmin1.gt.bmaj2*bmin2)call bug('f',
     *	  'The first input image must be the high resolution')
	call GauDFac(bmaj2,bmin2,bpa2,bmaj1,bmin1,bpa1,
     *				norm,bmaj,bmin,bpa,ifail)
	if(ifail.eq.0)call Gaufac(bmaj1,bmin1,bpa1,bmaj,bmin,bpa,
     *				norm,bmajt,bmint,bpat,ifail)
	if(ifail.ne.0)
     *	  call bug('f','Could not determine convolving parameters')
c	write(line,'(a,1pe10.3)')'Flux unit conversion factor:',
c     *					abs(cdelt1*cdelt2/norm)
c	call trimout(line)
c
c  Convert the gaussian parameters to the Fourier domain.
c
	call CvtParam(norm,bmaj,bmin,bpa,cdelt1,cdelt2,ngx,ngy,
     *						sfac,sxx,sxy,syy)
c
c  Determine the scaling factor to convert uvlo and uvhi to lambda.
c
	if(dofeath.or.dofac)then
	  if(unit.eq.'nanoseconds')then
	    temp = 1e-9*CMKS
	  else if(unit.eq.'feet')then
	    temp = 12*0.0254
	  else if(unit.eq.'klambda')then
	    temp = 1000
	  else if(unit.eq.'meters')then
	    temp = 1
	  else
	    call bug('f','Unrecognised units')
	  endif
	  if(unit.ne.'klambda')then
	    call coVelSet(lIn1,'freq')
	    call coFindAx(lIn1,'freq',iax)
	    call coCvt1(lIn1,iax,'op',0.d0,'aw',freq1)
	    call coVelSet(lIn2,'freq')
	    call coFindAx(lIn2,'freq',iax)
	    call coCvt1(lIn2,iax,'op',0.d0,'aw',freq2)
	    if(min(freq1,freq2).le.0.)
     *	      call bug('f','Could not determine observing frequency')
	    freq = sqrt(freq1*freq2)
	    write(line,'(a,f8.3,a)')'Using a frequency of ',freq,
     *		' GHz, to convert annulus to wavelengths'
	    call trimout(line)
	    lambda = CMKS/(freq*1e9)
	    temp = temp/lambda
	  endif
	  uvlo = uvlo * temp
	  uvhi = uvhi * temp
	endif
	du = 1/(ngx*cdelt1)
	dv = 1/(ngy*cdelt2)
c
c  Allocate memory, if needed.
c
	if(dofac.or.domerge)then
	  call memAlloc(pIn1,(ngx+2)*ngy,'r')
	  call memAlloc(pIn2,(ngx+2)*ngy,'r')
	  if(dotaper)then
	    call mosLoad(lIn1,npnt)
	    call memAlloc(pWt1,nIn(1)*nIn(2),'r')
	    call memAlloc(pWt2,nIn(1)*nIn(2),'r')
	  else
	    pWt1 = pIn1
	    pWt2 = pIn2
	  endif
	endif
c
c  Determine the scale factor.
c
	if(dofac)then
	  call boxSet(box,3,nIn,' ')
	  pfac = (4.0*log(2.0))/PI*abs(cdelt1*cdelt2)/(bmaj2*bmin2)
	  call GetFac(lIn1,lIn2,box,memr(pIn1),memr(pIn2),
     *	    nIn(1),nIn(2),ngx,ngy,dozero,device,
     *	      sfac,sxx,sxy,syy,uvlo,uvhi,du,dv,fac,pfac,
     *	      dotaper,memr(pWt1),memr(pWt2),xs,ys,doshift)
	  write(line,'(a,1pe11.3)')'Flux calibration factor:',fac
	  call trimout(line)
	  if(doshift)then
	    x1(1) = xs
	    x1(2) = ys
	    call coCvt(lIn1,'op/op',x1,'ow/ow',x2)
	    write(line,'(a,f10.2,a,f10.2,a)')
     *	      'Applying a shift of',
     *	      180*3600/PI*x2(1),', ',180*3600/PI*x2(2),
     *	      ' arcseconds to low the resolution data'
	    call trimout(line)
	  endif
	endif
c
c  Open the output.
c
	if(doOut)then
	  nOut(1) = nIn(1)
	  nOut(2) = nIn(2)
	  nOut(3) = nIn(3)
	  do i=4,naxis
	    nOut(i) = 1
	  enddo
	  call xyopen(lOut,out,'new',naxis,nOut)
	  if(domerge)then
	    call MkHead(lIn1,lOut,version,fac,mess1,mess2)
	  else
	    call hdcopy(lIn2,lOut,'mask')
	    call MkHead(lIn2,lOut,version,fac,mess1,mess2)
	  endif
c
	  do k=1,nOut(3)
	    if(k.ne.1)then
	      call xySetpl(lIn1,1,k)
	      call xySetpl(lIn2,1,k)
	      call xySetpl(lOut,1,k)
	    endif
c
	    if(domerge)then
	      call GetDat(lIn1,memr(pIn1),nIn(1),nIn(2),
     *		  ngx,ngy,dozero,.false.,memr(pWt1),memr(pWt2))
	      if(dotaper)call mosMIni(lIn1,real(k))
	      call GetDat(lIn2,memr(pIn2),nIn(1),nIn(2),
     *		  ngx,ngy,dozero,dotaper,memr(pWt1),memr(pWt2))
	      if(abs(xs)+abs(ys).gt.0)
     *		call shiftit1(memr(pIn2),ngx,ngy,xs,ys)
	      if(dotaper)call mosMFin
	      call Mergeit(dofeath,memr(pIn1),memr(pIn2),ngx,ngy,
     *		uvlo,uvhi,du,dv,fac/sfac,sxx,sxy,syy)
	      call WriteOut(lOut,memr(pIn1),ngx,nOut(2))
	    else
	      call Normalis(lIn2,lOut,nOut(1),nOut(2),fac)
	    endif
	  enddo
	  call xyClose(lOut)
	endif
c
	call xyClose(lIn1)
	call xyClose(lIn2)
	if(dofac.or.domerge)then
	  call memFree(pIn1,(ngx+2)*ngy,'r')
	  call memFree(pIn2,(ngx+2)*ngy,'r')
	  if(dotaper)then
	    call memFree(pWt1,nIn(1)*nIn(2),'r')
	    call memFree(pWt2,nIn(1)*nIn(2),'r')
	  endif
	endif
c
	end
c************************************************************************
	subroutine CvtParam(norm,bmaj,bmin,bpa,cdelt1,cdelt2,ngx,ngy,
     *						sfac,sxx,sxy,syy)
c
	implicit none
	real norm,bmaj,bmin,bpa
	double precision cdelt1,cdelt2
	integer ngx,ngy
	real sfac,sxx,syy,sxy
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	real theta,bmajf,bminf,c2,s2,a,b,dx,dy
c
	sfac = PI/(4*log(2.0)) * bmaj * bmin / norm
	bmajf = 4*log(2.0)/PI/bmaj
	bminf = 4*log(2.0)/PI/bmin
	dx = 1/(cdelt1*ngx)
	dy = 1/(cdelt2*ngy)
c       
	theta = pi/180. * bpa
	s2 = -sin(2*theta)
	c2 = -cos(2*theta)
	a = 4*log(2.) / (bmajf*bmajf)
	b = 4*log(2.) / (bminf*bminf)
	sxx = -0.5*( a*(c2+1) + b*(1-c2) ) * dx*dx
	syy = -0.5*( b*(c2+1) + a*(1-c2) ) * dy*dy
	sxy = -(b-a)*s2 * dx*dy
c
	end
c************************************************************************
	subroutine Normalis(lIn,lOut,nx,ny,fac)
c
	implicit none
	integer lIn,lOut,nx,ny
	real fac
c------------------------------------------------------------------------
	include 'maxdim.h'
	real Data(MAXDIM)
	integer i,j
c
	do j=1,ny
	  call xyread(lIn,j,Data)
	  do i=1,nx
	    Data(i) = fac * Data(i)
	  enddo
	  call xywrite(lOut,j,Data)
	enddo
c
	end
c************************************************************************
	subroutine WriteOut(lOut,Data,ngx,ny)
c
	implicit none
	integer ngx,ny,lOut
	real Data(ngx+2,ny)
c
c------------------------------------------------------------------------
	integer j
c
	do j=1,ny
	  call xywrite(lOut,j,Data(1,j))
	enddo
	end
c************************************************************************
	subroutine GetOpt(domerge,dozero,dofeath,doshift,notaper)
c
	implicit none
	logical domerge,dozero,dofeath,doshift,notaper
c
c  Get extra processin parameters.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=5)
	character opts(NOPTS)*12
	logical   present(NOPTS)
c
	data opts/'normalize   ','zero        ','feather     ',
     *		  'shift       ','notaper     '/
c
	call options('options',opts,present,NOPTS)
	domerge = .not.present(1)
	dozero  =      present(2)
	dofeath =      present(3)
	if(dofeath.and..not.domerge)call bug('f',
     *	  'The feather and normalize options make no sense together')
	doshift =      present(4)
	notaper =      present(5)
	end
c************************************************************************
	subroutine MkHead(lIn,lOut,version,fac,mess1,mess2)
c
	implicit none
	integer lIn,lOut
	character version*(*),mess1*(*),mess2*(*)
	real fac
c
c  Make the output dataset header.
c------------------------------------------------------------------------
	character line*64
	integer i
c
	integer nkeys
	parameter(nkeys=41)
	character keyw(nkeys)*8
	data keyw/   'bunit   ','obstime ','epoch   ','cellscal',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *	  'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *	  'crpix1  ','crpix2  ','crpix3  ','crpix4  ','crpix5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *	  'niters  ','object  ','telescop','observer','btype   ',
     *	  'restfreq','vobs    ','obsra   ','obsdec  ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','history ',
     *	  'bmaj    ','bmin    ','bpa     '/
c
c  Copy other parameters.
c
	do i=1,nkeys
	  call hdcopy(lIn,lOut,keyw(i))
	enddo
c
c  Create the output history.
c
	call hisopen(lOut,'append')
	line = 'IMMERGE: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'IMMERGE')
	call hiswrite(lOut,
     *	  'IMMERGE: The inputs are assumed to have gaussian beams')
	line = 'IMMERGE: '//mess1
	call hiswrite(lOut,line)
	line = 'IMMERGE: '//mess2
	call hiswrite(lOut,line)
	write(line,'(a,1pe13.5)')
     *	  'IMMERGE: Using a flux calibration factor of',fac
	call hiswrite(lOut,line)
	call hisclose(lOut)
	end
c************************************************************************
	subroutine GetDat(lIn,In,nx,ny,ngx,ngy,dozero,
     *	  dotaper,Wt1,Wt2)
c
	implicit none
	integer lIn,nx,ny,ngx,ngy
	logical dozero,dotaper
	real In(ngx+2,ngy),Wt1(nx,ny),Wt2(nx,ny)
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,length,ntaper
	real scale,x,fac,staper
	logical flags(MAXDIM)
	character ctaper*4
c
c  Get the weight image
c
	ntaper = 0
	staper = 0
	if(dotaper)call mosWts(Wt1,Wt2,nx,ny,0,0)
c
	do j=1,ny
c
c  Get pixels and flags. Set bad pixels to zero.
c
	  call xyRead(lIn,j,In(1,j))
	  call xyflgrd(lIn,j,flags)
	  do i=1,nx
	    if(.not.flags(i))In(i,j) = 0
	  enddo
c
	  if(dotaper)then
	    do i=1,nx
	      ntaper = ntaper + 1
	      if(Wt1(i,j).gt.0)then
	        In(i,j) = In(i,j)/Wt1(i,j)
		staper = staper + 1/Wt1(i,j)
	      else
		In(i,j) = 0
	      endif
	    enddo
	  endif
	    
c
c  Zero extend the image.
c
	  do i=nx+1,ngx
	    In(i,j) = 0
	  enddo
c
c  Reflect the image in a bizzar fashion, if needed.
c
	  if(.not.dozero)then
	    if(nx+1.eq.ngx)then
	      In(nx+1,j) = 0.5*(In(nx,j) + In(1,j))
	    else
	      length = min(ngx-nx-1,nx-1)
	      scale = 2.0/real(length)
	      do i=1,length
	        x = scale*(i-1) - 1
	        fac = 0.5 - x*(0.75 - 0.25*x*x)
	        In(nx+i,j) = In(nx+i,j) + fac*In(nx-i,j)
	        In(ngx-i+1,j) = In(ngx-i+1,j) + fac*In(i+1,j)
	      enddo
	    endif
	  endif
	enddo
c
c  Zero the rest of the guard band.
c
	do j=ny+1,ngy
	  do i=1,ngx
	    In(i,j) = 0
	  enddo
	enddo
c
c  Reflect the image into the guard band if needed.
c
	if(.not.dozero)then
	  if(ny+1.eq.ngy)then
	    do i=1,ngx
	      In(i,ny+1) = 0.5*(In(i,ny) + In(i,1))
	    enddo
	  else
	    length = min(ngy-ny-1,ny-1)
	    scale = 2.0/real(length)
	    do j=1,length
	      x = scale*(j-1) - 1
	      fac = 0.5 - x*(0.75 - 0.25*x*x)
	      do i=1,ngx
	        In(i,ny+j) = In(i,ny+j) + fac*In(i,ny-j)
	        In(i,ngy-j+1) = In(i,ngy-j+1) + fac*In(i,j+1)
	      enddo
	    enddo
	  endif
	endif
c
c  Report on the tapering.
c
	if(ntaper.gt.0)then
	  staper = staper/ntaper
	  write(ctaper,'(f4.2)')staper
	  if(staper.lt.0.5)call bug('w','Taper correction for low '//
     *	    'resolution image is small (mean='//ctaper//')')
	  if(staper.lt.0.1)call bug('f','This is too small')
	endif
c
c  Fourier transform the image.
c
	call FFFT(In,ngx,ngy)
c
	end
c************************************************************************
	subroutine GetBeam(lIn,in,bmaj,bmin,bpa,message)
c
	implicit none
	integer lIn
	character in*(*),message*(*)
	real bmaj,bmin,bpa
c
c  Get the gaussian beam parameters corresponding to this observation.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	character line*80,pbtype*16,label*18
	double precision ra,dec
	real rms,pbfwhm,cutoff,maxrad
	integer npnt,pbObj
c
c  Externals.
c
	character itoaf*6
c
	call rdhdr(lIn,'bmaj',bmaj,0.)
	call rdhdr(lIn,'bmin',bmin,0.)
	call rdhdr(lIn,'bpa',bpa,0.)
c
c  If the gaussian parameters were not in the header, assume its
c  a single dish, and try to get the primary beam size.
c
	if(bmaj*bmin.le.0)then
	  call mosLoad(lIn,npnt)
	  line = 'Cannot determine gaussian beam '//
     *		 'parameters for mosaic in '//in
	  if(npnt.gt.1)call bug('f',line)
	  call mosGet(1,ra,dec,rms,pbtype)
	  call pbInit(pbObj,pbtype,lIn)
	  call pbInfo(pbObj,pbfwhm,cutoff,maxrad)
	  call pbFin(pbObj)
	  bmaj = pbfwhm
	  bmin = pbfwhm
	  bpa = 0
	  line = 'Assuming single-dish observation in '//in
	else
	  line = 'Found gaussian beam parameters for '//in
	endif
c
	call trimout(line)
	label = ' arcsec; pa='//itoaf(nint(bpa))
	write(line,'(a,f8.2,a,f8.2,a)')' ... with beam fwhm:',
     *	  (3600*180/PI)*bmaj,' by',(3600*180/PI)*bmin,
     *	  label
	call trimout(line)
	message = line
c
	end
c************************************************************************
	subroutine GetFac(lIn1,lIn2,box,In1,In2,nx,ny,ngx,ngy,
     *	  dozero,device,sfac,sxx,sxy,syy,uvlo,uvhi,du,dv,fac,pfac,
     *	  dotaper,Wt1,Wt2,xs,ys,doshift)
c
	implicit none
	integer nx,ny,ngx,ngy,lIn1,lIn2
	integer box(*)
	logical dozero,doshift,dotaper
	complex In1(ngx/2+1,ngy),In2(ngx/2+1,ngy)
	real sfac,sxx,sxy,syy,uvlo,uvhi,fac,du,dv,pfac,xs,ys
	character device*(*)
	real Wt1(nx,ny),Wt2(nx,ny)
c
c  Determine the scale factor to normalise everything.
c
c  Inputs:
c    In1	Fine-resolution input data.
c    In2	Coarse-resolution input data.
c    ngx,ngy	Fourier transform size.
c    sfac,sxx,sxy,syy Gaussian parameters.
c    uvlo,uvhi	Annulus of interest, in lambda.
c    du,dv	Cell increments in u and v.
c    device	PGPLOT device to plot the fit on.
c    pfac	Scale factor used in plotting.
c  Output:
c    fac	Scale factor to apply to the coarser resolution data.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	include 'mem.h'
	integer MAXRUNS
	parameter(MAXRUNS=MAXDIM+1)
	integer nul,nuh,nvl,nvh,n,pX,pY,pUU,pVV,np,npd
	integer imin,imax,jmin,jmax,kmin,kmax,k
	integer nruns,runs(3,MAXRUNS)
	integer blc(3),trc(3)
c
c  Externals.
c
	character itoaf*8
c
c  Determine the range of planes to process.
c
	call boxInfo(box,3,blc,trc)
	kmin = blc(3)
	kmax = trc(3)
c
c  Determine the number of pixels which are in the overlap annulus.
c
	nul = nint(abs(uvlo/du)-0.5)
	nuh = nint(abs(uvhi/du)+0.5)
	nvl = nint(abs(uvlo/dv)-0.5)
	nvh = nint(abs(uvhi/dv)+0.5)
	n = (kmax-kmin+1)*
     *		nint(0.5*PI*(nuh*nvh - nul*nvl) + nuh - nul + 1.5)
	call memAlloc(pX,n,'c')
	call memAlloc(pY,n,'c')
	call memAlloc(pUU,n,'i')
	call memAlloc(pVV,n,'i')
c
c  Get all the data in the annuli.
c
	np = 0
	do k=kmin,kmax
	  call boxRuns(1,k,' ',box,runs,MAXRUNS,nruns,
     *						imin,imax,jmin,jmax)
	  if(imin.ne.1.or.imax.ne.nx.or.jmin.ne.1.or.jmax.ne.ny.or.
     *	    (nruns.ne.ny.and.nruns.ne.0))call bug('f',
     *	      'Only plane selection supported in region keyword')
	  if(nruns.gt.0)then
	    if(k.ne.1)then
	      call xysetpl(lIn1,1,k)
	      call xysetpl(lIn2,1,k)
	    endif
	    call GetDat(lIn1,In1,nx,ny,ngx,ngy,dozero,
     *						.false.,Wt1,Wt2)
	    if(dotaper)call mosMIni(lIn1,real(k))
	    call GetDat(lIn2,In2,nx,ny,ngx,ngy,dozero,
     *						dotaper,Wt1,Wt2)
	    if(dotaper)call mosMFin
	    call AnnExt(In1,In2,ngx,ngy,memc(pX+np),memc(pY+np),
     *		memi(pUU+np),memi(pVV+np),
     *		n-np,npd,sfac,sxx,sxy,syy,uvlo,uvhi,du,dv)
	    np = np + npd
	  endif
	enddo
c
	call trimout('Number of data points in the annulus: '
     *						//itoaf(np))
	if(np.eq.0)call bug('f','No Data points found in annulus')
c
c  Determine the offset.
c
	if(doshift)then
	  call GetShift(np,memi(pUU),memi(pVV),memc(pX),memc(pY),
     *						In1,ngx,ngy,xs,ys)
	  call Shiftit2(np,memi(pUU),memi(pVV),memc(pY),xs,ys,ngx,ngy)
	else
	  xs = 0
	  ys = 0
	endif
c
c  Fit for the scale factor.
c
	call DetFac(memc(pX),memc(pY),2*np,fac)
c
c  Plot the scale factor, if the user wanted this.
c
	if(device.ne.' ')call PlotFac(device,memc(pX),memc(pY),
     *					2*np,fac,pfac,doshift)
c
c  Free the allocated memory.
c
	call memFree(pX,n,'c')
	call memFree(pY,n,'c')
	call memFree(pUU,n,'i')
	call memFree(pVV,n,'i')
c
c  Reset to select the first plane.
c
	if(kmax.ne.1)then
	  call xysetpl(lIn1,1,1)
	  call xysetpl(lIn2,1,1)
	endif
c
	end
c************************************************************************
	subroutine GetShift(np,uu,vv,x,y,data,ngx,ngy,xs,ys)
c
	implicit none
	integer np,ngx,ngy
	integer uu(np),vv(np)
	complex x(np),y(np),data(ngx/2+1,ngy)
	real xs,ys
c
c  Determine the required shift to optimally align the X and Y data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k,l
c
	do j=1,ngy
	  do i=1,ngx/2+1
	    data(i,j) = (0.,0.)
	  enddo
	enddo
c
c  Accumulate the cross correlation between X and Y.
c
	do k=1,np
	  l = uu(k) + vv(k)
	  if(2*(l/2).eq.l)then
	    data(uu(k),vv(k)) = data(uu(k),vv(k)) + x(k)*conjg(y(k))
	  else
	    data(uu(k),vv(k)) = data(uu(k),vv(k)) - x(k)*conjg(y(k))
	  endif
	enddo
c
c  Fourier transform it now.
c
	call IFFT(data,ngx,ngy)
c
c  Find the peak of the data.
c
	call findpk(data,ngx,ngy,xs,ys)
c
	end
c************************************************************************
	subroutine Shiftit2(np,uu,vv,y,xs,ys,ngx,ngy)
c
	implicit none
	integer np,ngx,ngy
	integer uu(np),vv(np)
	real xs,ys
	complex y(np)
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
	complex W
	real theta
c
	do i=1,np
	  theta = 2*PI*(xs*(uu(i)-1)/real(ngx)+ys*(vv(i)-1)/real(ngy))
	  W = cmplx(cos(theta),sin(theta))
	  y(i) = W*y(i)
	enddo
c
	end
c************************************************************************
	subroutine shiftit1(data,ngx,ngy,xs,ys)
c
	implicit none
	integer ngx,ngy
	real xs,ys
	complex data(ngx/2+1,ngy)
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,j
	complex W
	real theta
c
	do j=1,ngy
	  do i=1,ngx/2+1
	    theta = 2*PI*(xs*(i-1)/real(ngx)+ys*(j-1)/real(ngy))
	    W = cmplx(cos(theta),sin(theta))
	    data(i,j) = W*data(i,j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine findpk(data,ngx,ngy,xs,ys)
c
	implicit none
	integer ngx,ngy
	real data(ngx+2,ngy),xs,ys
c
c  Find the position, to a fraction of a pixel of the maximum pixel.
c------------------------------------------------------------------------
	integer i,j,k,imax,jmax
	real pkval,fit(9),coeffs(6),fmax
	double precision pixmax(2)
c
	imax = 1
	jmax = 1
	pkval = data(1,1)
	do j=1,ngy
	  do i=1,ngx
	    if(data(i,j).gt.pkval)then
	      pkval = data(i,j)
	      imax = i
	      jmax = j
	    endif
	  enddo
	enddo
c
	if(imax.eq.1.or.imax.eq.ngx.or.jmax.eq.1.or.jmax.eq.ngy)
     *	  call bug('f','Cannot fit to peak next to image edge')
	k = 0
	do j=jmax-1,jmax+1
	  do i=imax-1,imax+1
	    k = k + 1
	    fit(k) = data(i,j)
	  enddo
	enddo
c
c  Locate the peak.
c
	call pkfit(fit,3,fmax,pixmax,coeffs)
	xs = imax + pixmax(1) - (ngx/2+1)
	ys = jmax + pixmax(2) - (ngy/2+1)
c
	end
c************************************************************************
	subroutine PlotFac(device,X,Y,n,a,pfac,doshift)
c
	implicit none
	integer n
	character device*(*)
	real X(n),Y(n),a,pfac
	logical doshift
c
c------------------------------------------------------------------------
	real xmin,xmax,xlo,xhi,xp(2)
	integer i
c
c  Externals.
c
	integer pgbeg
c
c  Find the min and max.
c
	xmin = pfac*x(1)
	xmax = xmin
	do i=1,n
	  x(i) = pfac*x(i)
	  y(i) = pfac*a*y(i)
	  xmin = min(xmin,x(i),y(i))
	  xmax = max(xmax,x(i),y(i))
	enddo
c
c  Create the plot of the normal data.
c
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Error opening graphics device')
	endif
	call pgscf(2)
	call pgpage
	call pgvstd
	call pgrnge(xmin,xmax,xlo,xhi)
	call pgwnad(xlo,xhi,xlo,xhi)
	call pgbox('BCNST',0.,0,'BCNST',0.,0)
	if(n.lt.100)then
	  call pgpt(n,x,y,17)
	else
	  call pgpt(n,x,y,1)
	endif
c
c  Plot the y = x line.
c
	call pgsci(2)
	xp(1) = xlo
	xp(2) = xhi
	call pgline(2,xp,xp)
c
c  Label it
c
	call pgsci(1)
	if(doshift)then
	  call pglab('High Resolution Data (Jy)',
     *		     'Scaled/Shifted Low Resolution Data (Jy)',
     *		     'Plot of Data in Fourier Annulus')
	else
	  call pglab('High Resolution Data (Jy)',
     *		     'Scaled Low Resolution Data (Jy)',
     *		     'Plot of Data in Fourier Annulus')
	endif
	call pgend
c
	end
c************************************************************************
	subroutine DetFac(X,Y,n,a)
c
	implicit none
	integer n
	real X(n),Y(n),a
c
c  Determine "a" such that
c    X \approx a * Y
c
c  Input:
c    X,Y	Data.
c    n		Number of data points.
c  Output:
c    a		Scale factor.
c------------------------------------------------------------------------
	double precision SumXX,SumYY,SumXY
	real rms,a1,a2,f1,f2,f
	integer i
c
c  Externals.
c
	real medfunc
c
c  Determine the least squares fit first.
c
	SumXX = 0
	SumYY = 0
	SumXY = 0
	do i=1,n
	  SumXX = SumXX + X(i)*X(i)
	  SumYY = SumYY + Y(i)*Y(i)
	  SumXY = SumXY + X(i)*Y(i)
	enddo
c
	if(SumYY.eq.0)call bug('f','Low resolution image is zero')
	if(SumXX.eq.0)call bug('f','High resolution image is zero')
	a = SumXY/SumYY
	rms = (SumXX + a*a*SumYY - 2*a*SumXY)/(n*SumYY)
	rms = max(0.01*a,sqrt(max(0.,rms)))
c
	a1 = a
	f1 = medfunc(a1,X,Y,n)
	a2 = a1 + sign(3*rms,f1)
	f2 = medfunc(a2,X,Y,n)
	dowhile(f1*f2.gt.0)
	  a = 2*a2 - a1
	  a1 = a2
	  f1 = f2
	  a2 = a
	  f2 = medfunc(a2,X,Y,n)
	enddo
c
	dowhile(abs(a1-a2).gt.0.001*rms)
	  a = 0.5*(a1+a2)
	  if(a.eq.a1.or.a.eq.a2)return
	  f = medfunc(a,X,Y,n)
	  if(f*f1.ge.0)then
	    f1 = f
	    a1 = a
	  else
	    f2 = f
	    a2 = a
	  endif
	enddo
c
	end
c************************************************************************
	real function medfunc(a,X,Y,n)
c
	implicit none
	integer n
	real a,X(n),Y(n)
c
c  Determine the function needed to solve for the median.
c
c------------------------------------------------------------------------
	integer i
c
	medfunc = 0
	do i=1,n
	  medfunc = medfunc + y(i)*sign(1.0,x(i)-a*y(i))
     *			    - x(i)/a/a*sign(1.0,y(i)-x(i)/a)
	enddo
c
	end
c************************************************************************
	subroutine AnnExt(In1,In2,ngx,ngy,X,Y,UU,VV,nmax,np,
     *	  sfac,sxx,sxy,syy,uvlo,uvhi,du,dv)
c
	implicit none
	integer ngx,ngy,nmax,np
	complex X(nmax),Y(nmax)
	integer UU(nmax),VV(nmax)
	complex In1(ngx/2+1,ngy),In2(ngx/2+1,ngy)
	real sfac,sxx,sxy,syy,uvlo,uvhi,du,dv
c
c  Extract the pixels from the annulus of overlap of the coarse and
c  fine resolution images.
c
c  Output:
c    X,Y	Pixels from the low and high resolution data, that we are
c		scaling to equalize.
c    R		Radius in wavelengths
c    np		Number of pixels extracted.
c
c------------------------------------------------------------------------
	integer i,j,ic,jc
	real uv2,t
c
	np = 0
c
c  Do the top half,
c
	ic = 1
	jc = 1
	do j=1,ngy/2+1
	  do i=1,ngx/2+1
	    uv2 = ((i-ic)*du)**2 + ((j-jc)*dv)**2
	    if(uv2.ge.uvlo*uvlo.and.uv2.le.uvhi*uvhi)then
	      t = sxx*(i-ic)*(i-ic) + sxy*(i-ic)*(j-jc) + 
     *		  syy*(j-jc)*(j-jc)
	      t = sfac*exp(t)
	      np = np + 1
	      if(np.gt.nmax)call bug('f','Too many points in Annext')
	      X(np) = t*In1(i,j)
	      Y(np) =   In2(i,j)
	      UU(np) = i
	      VV(np) = j
	    endif
	  enddo
	enddo
c
c  Do the bottom half.
c
	ic = 1
	jc = ngy+1
	do j=ngy/2+2,ngy
	  do i=1,ngx/2+1
	    uv2 = ((i-ic)*du)**2 + ((j-jc)*dv)**2
	    if(uv2.ge.uvlo*uvlo.and.uv2.le.uvhi*uvhi)then
	      t = sxx*(i-ic)*(i-ic) + sxy*(i-ic)*(j-jc) + 
     *		  syy*(j-jc)*(j-jc)
	      t = sfac*exp(t)
	      np = np + 1
	      if(np.gt.nmax)call bug('f','Too many points in Annext')
	      X(np) = t*In1(i,j)
	      Y(np) =   In2(i,j)
	      UU(np) = i
	      VV(np) = j
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine Mergeit(dofeath,In1,In2,ngx,ngy,uvlo,uvhi,du,dv,
     *						fac,sxx,sxy,syy)
c
	implicit none
	integer ngx,ngy
	complex In1(ngx/2+1,ngy),In2(ngx/2+1,ngy)
	real fac,sxx,sxy,syy,uvlo,uvhi,du,dv
	logical dofeath
c
c  Merge together two Fourier transforms.
c
c  Input/Output:
c    In1	On input, this is the transform of the finer-resolution data.
c		On output, this is the merged data in the image domain.
c  Input:
c    In2	This is the transform of the coarser-resolution data.
c    ngx,ngy
c    uvlo,uvhi
c    fac,sxx,sxy,syy Gaussian parameters.
c    du,dv	Cell increments in u and v.
c
c------------------------------------------------------------------------
	integer i,j,ic,jc
	real uv2,t,x,y
c
c  Do the top half,
c
	ic = 1
	jc = 1
	do j=1,ngy/2+1
	  do i=1,ngx/2+1
	    uv2 = ((i-ic)*du)**2 + ((j-jc)*dv)**2
	    t = sxx*(i-ic)*(i-ic) + sxy*(i-ic)*(j-jc) + 
     *		syy*(j-jc)*(j-jc)
	    if(dofeath)then
	      if(uv2.lt.uvlo*uvlo)then
	        t = fac*exp(-t)
	        In1(i,j) = t*In2(i,j)
	      else if(uv2.le.uvhi*uvhi)then
	        t = fac*exp(-t)
	        x = 2*(sqrt(uv2) - uvlo)/(uvhi-uvlo) - 1
	        y = 0.5 - x*(0.75 - 0.25*x*x)
	        In1(i,j) = (1-y)*In1(i,j) + y*t*In2(i,j)
	      endif
	    else if(t.gt.-20)then
	      In1(i,j) = fac*In2(i,j) + (1-exp(t))*In1(i,j)
	    endif
	  enddo
	enddo
c
c  Do the bottom half.
c
	ic = 1
	jc = ngy+1
	do j=ngy/2+2,ngy
	  do i=1,ngx/2+1
	    uv2 = ((i-ic)*du)**2 + ((j-jc)*dv)**2
	    t = sxx*(i-ic)*(i-ic) + sxy*(i-ic)*(j-jc) + 
     *		syy*(j-jc)*(j-jc)
	    if(dofeath)then
	      if(uv2.lt.uvlo*uvlo)then
	        t = fac*exp(-t)
	        In1(i,j) = t*In2(i,j)
	      else if(uv2.le.uvhi*uvhi)then
	        t = fac*exp(-t)
	        x = 2*(sqrt(uv2) - uvlo)/(uvhi-uvlo) - 1
	        y = 0.5 - x*(0.75 - 0.25*x*x)
	        In1(i,j) = (1-y)*In1(i,j) + y*t*In2(i,j)
	      endif
	    else if(t.gt.-20)then
	      In1(i,j) = fac*In2(i,j) + (1-exp(t))*In1(i,j)
	    endif
	  enddo
	enddo
c
c  Fourier transform back to the image domain.
c
	call IFFT(In1,ngx,ngy)
c
	end
c************************************************************************
	subroutine IFFT(In,ngx,ngy)
c
	implicit none
	integer ngx,ngy
	complex In(ngx/2+1,ngy)
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	real scale
	complex Temp1(MAXDIM),temp2(MAXDIM)
c
	scale = 1/real(ngx*ngy)
	do i=1,ngx/2+1
	  do j=1,ngy
	    Temp1(j) = scale*In(i,j)
	  enddo
	  call fftcc(Temp1,Temp2,-1,ngy)
	  do j=1,ngy
	    In(i,j) = Temp2(j)
	  enddo
	enddo
c
	do j=1,ngy
	  do i=1,ngx/2+1
	    Temp1(i) = In(i,j)
	  enddo
	  call fftcr(Temp1,In(1,j),-1,ngx)
	enddo
c
	end
c************************************************************************
	subroutine FFFT(In,ngx,ngy)
c
	implicit none
	integer ngx,ngy
	complex In(ngx/2+1,ngy)	
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	complex Temp1(MAXDIM),Temp2(MAXDIM)
c
c
c  Do the first pass of the FFT.
c
	do j=1,ngy
	  call fftrc(In(1,j),Temp1,1,ngx)
	  do i=1,ngx/2+1
	    In(i,j) = Temp1(i)
	  enddo
	enddo
c
c  Do the second pass of the FFT.
c
	do i=1,ngx/2+1
	  do j=1,ngy
	    Temp1(j) = In(i,j)
	  enddo
	  call fftcc(Temp1,Temp2,1,ngy)
	  do j=1,ngy
	    In(i,j) = Temp2(j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine trimout(line)
c
	implicit none
	character line*(*)
c
c------------------------------------------------------------------------
	integer i,l
	logical blank,add
	character message*128
c
	blank = .false.
	l = 0
	do i=1,len(line)
	  add = line(i:i).ne.' '.or..not.blank
	  if(add)then
	    l = l + 1
	    message(l:l) = line(i:i)
	  endif
	  blank = line(i:i).eq.' '
	enddo
	if(l.gt.0)call output(message(1:l))
c
	end
