c************************************************************************
	program mfspin
	implicit none
c
c= mfspin - Compute spectral index from a MFCLEAN model.
c& rjs
c: deconvolution
c+
c	MFSPIN is a MIRIAD task which computes spectral index and possibly
c	rotation measure images, given models produced by MFCLEAN.  It
c	does this by convolving both the flux and "flux time spectral index"
c	planes of the models by a Gaussian, and then performing a division.
c
c	It has two modes of operation.  One is to give a single input model
c	(Stokes I or V even) which consists of two planes.  MFSPIN will then
c	compute the spectral index image from that.   The second mode 
c	requires both Stokes Q and U models.  MFSPIN will then compute
c	a linearly polarized intensity spectral index and optionally a 
c	rotation measure image.
c@ model
c	The model produced by MFCLEAN. No default. Normally this will be
c	a single image such as a model for Stokes I or V. However, you
c	can also give it two models, one for Stokes Q and one for Stokes U
c	(see the discussion above).
c@ beam
c	The input dirty beam. This is only required to determine the beam
c	size. If the fwhm parameter is given, the beam will not be used.
c@ clip
c	The division clip level. One or two numbers can be given. If two
c	values are given, then flux pixel values in this range are flagged
c	as bad before the division (but after the convolution!). If a single
c	value is given, then for Stokes I images (or images for which the
c	Stokes parameter is unknown),  all pixel values less than the
c	specified clip are flagged. If one value is given and the image is
c	of Stokes Q,U or V, then pixels are flagged when their absolute value
c	is less than the clip level. The default is 0.
c@ fwhm
c	The size, in arcsec, of the gaussian beam to convolve with.
c	This will normally be two numbers, giving the
c	full-width at half-maximum of the major and minor axes of the
c	gaussian. If only one number is given, the gaussian will have
c	equal major and minor axes. If no values are given, they are
c	computed by fitting a gaussian to the given dirty beam. Note
c	that the fitting routine will probably give different values to
c	the AIPS MX and APCLN tasks. Generally the value computed by
c	MFSPIN is to be preferred to the APCLN and MX values.
c@ pa
c	The position angle, in degrees, of the gaussian,
c	measured east from north. The default is determined from the dirty
c	beam fit (The value for PA is ignored, if no value is given for
c	FWHM).
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match is used. Possible options are:
c	  pbcorr   Correct the spectral index for primary beam effects.
c	           By default, this is not performed.
c@ out
c	The output image names. No default. Normally this will be a single
c	output, being the spectral index. If the input consists of a Q and a
c	U model, the output can be two names. The first is the linearly
c	polarized intensity spectral index image, and the second will receive
c	an estimate of the rotation measure (units of rad/m**2).
c--
c
c  History:
c    rjs   4may92  Cloned from RESTOR.
c    rjs   6may92  Outputs mask file. Computes rotation measure. Does
c		   primary beam correction.
c    rjs  26may92  Write btype keyword. Change of sign convention for
c		   spectral index.
c    nebk 28jan93  Adapt to new primary beam routines.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	integer nP,PolQ,PolU,PolV
	integer MAXRUNS
	parameter(MAXRUNS=5*MAXDIM,nP=11)
	parameter(version='MfSpin: version 28-Jan-93')
	parameter(PolQ=2,PolU=3,PolV=4)
c
	real dat(MAXBUF)
	common dat
c
	character beam*64,modl1*64,modl2*64,out1*64,out2*64
	logical NeedFwhm,pbcorr,twoclip,doQU
	integer nsize(3),pol1,pol2
	integer mBeam,nBeam,mModel,nModel,nPd
	integer Runs(3,MAXRUNS),nruns,npix
	integer lBeam,lMod1,lMod2,xBeam,yBeam
	integer Flux1,Flux2,Alpha1,Alpha2,Gaus,handle
	character line*72,pbtype*6
	real Patch(nP*nP)
	real fwhm1,fwhm2,pa
	real clip(2),cdelt1,cdelt2,crpix1,crpix2,nu0
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
	call keya('model',modl1,' ')
	call keya('model',modl2,' ')
	call keya('beam',Beam,' ')
	call keya('out',Out1,' ')
	call keya('out',Out2,' ')
	call keyr('clip',clip(1),0.)
	twoclip = keyprsnt('clip')
	if(twoclip)then
	  call keyr('clip',clip(2),0.)
	else
	  clip(2) =  clip(1)
	  clip(1) = -clip(1)
	endif
	call keyr('fwhm',fwhm1,0.)
	call keyr('fwhm',fwhm2,fwhm1)
	call keyr('pa',pa,0.)
	call GetOpt(pbcorr)
	call keyfin
c
c  Check the reasonableness of the inputs.
c
	if(clip(2).lt.clip(1))call bug('f','Bad clip range')
	if(Modl1.eq.' ')call bug('f','Input model name missing')
	if(Out1.eq.' ')  call bug('f','Output map name missing')
	doQU = Modl2.ne.' '
c
	fwhm1 = fwhm1 * pi / 180 / 3600
	fwhm2 = fwhm2 * pi / 180 / 3600
	NeedFwhm = fwhm1*fwhm2.eq.0
	if(Beam.eq.' '.and.NeedFwhm)
     *	  call bug('f','Either beam or fwhm must be given')
c
c  Determine the gaussian that we are going to convolve with. If its is
c  in the beams header, just use that. Otherwise fit it.
c
	if(NeedFwhm)then
	  call xyopen(lBeam,Beam,'old',3,nsize)
	  mBeam = nsize(1)
	  nBeam = nsize(2)
	  call rdhdr(lBeam,'bmaj',fwhm1,0.)
	  call rdhdr(lBeam,'bmin',fwhm2,0.)
	  call rdhdr(lBeam,'bpa',pa,0.)
	  if(fwhm1*fwhm2.le.0)then
	    if(max(mBeam,nBeam).gt.maxdim)
     *	    call bug('f','Beam is too big for me to handle')
	    nPd = min(nP,mBeam,nBeam)
	    nPd = nPd - mod(nPd+1,2)
	    call rdhdr(lBeam,'cdelt1',cdelt1,0.)
	    call rdhdr(lBeam,'cdelt2',cdelt2,0.)
	    if(cdelt1*cdelt2.eq.0)
     *	      call bug('f','Pixel increments missing from the beam')
	    call GetPatch(lBeam,mBeam,nBeam,Patch,nPd,xBeam,yBeam)
	    call GetFwhm(Patch,nPd,xBeam-mBeam/2+nPd/2,
     *	      yBeam-nBeam/2+nPd/2,cdelt1,cdelt2,Fwhm1,Fwhm2,Pa)
	    call wrhdr(lBeam,'bmaj',fwhm1)
	    call wrhdr(lBeam,'bmin',fwhm2)
	    call wrhdr(lBeam,'bpa',pa)
	  endif
	  call xyclose(lBeam)
	endif
c
c  Keep the user awake with spurious information.
c
	write(line,'(a,f6.2,a,f6.2,a)')
     *	   'Using gaussian beam fwhm of',(3600*180/pi)*fwhm1,' by',
     *	   (3600*180/pi)*fwhm2,' arcsec.'
	call output(line)
	write(line,'(a,f6.1,a)')'Position angle: ',pa,' degrees.'
	call output(line)
c
c  Open the model, and get various info about it.
c
	call xyopen(lMod1,Modl1,'old',3,nsize)
	mModel = nsize(1)
	nModel = nsize(2)
	if(nsize(3).ne.2)call bug('f',
     *		'Model does not appear to be of the correct type')
c
	call GetInfo(lMod1,mModel,nModel,
     *	  pol1,nu0,cdelt1,cdelt2,crpix1,crpix2)
	twoclip = twoclip .or. 
     *		  (pol1.eq.PolQ.or.pol1.eq.PolU.or.Pol1.eq.PolV)
c
c  If there is a second model, open it, etc.
c
	if(doQU)then
	  call xyopen(lMod2,Modl2,'old',3,nsize)
	  if(nsize(1).ne.mModel.or.nsize(2).ne.nModel)
     *	    call bug('f','Model sizes do not agree')
	  if(nsize(3).ne.2)call bug('f',
     *		'Model does not appear to be of the correct type')
	  call GetInfo(lMod2,mModel,nModel,
     *		pol2,nu0,cdelt1,cdelt2,crpix1,crpix2)
c
c  Make the Q model the first.
c
	  if(pol1.gt.pol2)call QUswap(lMod1,pol1,Modl1,lMod2,pol2,Modl2)
c
c  Final checks for sensibleness.
c
	  if(pol1.ne.PolQ.and.pol2.ne.PolU)call bug('f',
     *	    'The two models do not seem to be Q and U')
	  if(Out2.ne.' '.and.nu0.le.0)call bug('f',
     *	    'Unable to determine reference frequency')
	else if(pol1.eq.PolQ.or.pol1.eq.PolU)then
	  call bug('w',
     *	    'Spectral index of a linearly polarised map is meaningless')
	endif
c
c  Get the characteristics of the gaussian.
c
	mBeam = nextpow2(mModel)
	nBeam = nextpow2(nModel)
	xBeam = mBeam/2 + 1
	yBeam = nBeam/2 + 1
c
c  Get the Fourier transform of the gaussian.
c
	call MemAlloc(Gaus,mBeam*nBeam,'r')
	call GetBeam(mBeam,nBeam,xBeam,yBeam,dat(Gaus),
     *	  cdelt1,cdelt2,fwhm1,fwhm2,pa)
	call CnvlIniA(handle,dat(Gaus),mBeam,nBeam,xBeam,yBeam,0.,'s')
	call MemFree(Gaus,mBeam*nBeam,'r')
c
c  Allocate all the memory we could possible want.
c
	call MemAlloc(Flux1,mModel*nModel,'r')
	call MemAlloc(Alpha1,mModel*nModel,'r')
	if(doQU)then
	  call MemAlloc(Flux2,mModel*nModel,'r')
	  call MemAlloc(Alpha2,mModel*nModel,'r')
	else
	  Flux2 = Flux1
	endif
c
c  Convolve the flux planes and determine the runs.
c
	call CnvlF(handle,lMod1,mModel,nModel,dat(Flux1),'c')
	if(doQU)call CnvlF(handle,lMod2,mModel,nModel,dat(Flux2),'c')
c
	call Clipper(twoclip,doQU,clip,
     *	    dat(Flux1),dat(Flux2),mModel,nModel,Runs,maxruns,nruns)
	if(nruns.eq.0)call bug('f','No good pixels selected')
c
c  Compress them.
c
	call Compact(dat(Flux1),mModel,nModel,Runs,nRuns,npix)
	if(doQU)call Compact(dat(Flux2),mModel,nModel,Runs,nRuns,npix)
c
c  Now convolve and compact the "alpha" plane.
c
	call xysetpl(lMod1,1,2)
        call CnvlF(handle,lMod1,mModel,nModel,dat(Alpha1),'c')
	call Compact(dat(Alpha1),mModel,nModel,Runs,nRuns,npix)
	if(doQU)then
	  call xysetpl(lMod2,1,2)
	  call CnvlF(handle,lMod2,mModel,nModel,dat(Alpha2),'c')
	  call Compact(dat(Alpha2),mModel,nModel,Runs,nRuns,npix)
	endif
c
c  Do the division and primary beam correction (if needed).
c
	if(doQU)then
	  call Divide2(npix,dat(Alpha1),dat(Alpha2),
     *			    dat(Flux1),dat(Flux2))
	else
	  call Divide1(npix,dat(Alpha1),dat(Flux1))
	endif
	if(pbcorr)call CorrSI(lMod1,npix,dat(Alpha1),Runs,nRuns,
     *	  crpix1,cdelt1,crpix2,cdelt2,pbtype)
c
c  Save the spectral index map.
c
	call WriteOut(Out1,dat(Alpha1),npix,mModel,nModel,
     *		Runs,nruns,lMod1,version,fwhm1,fwhm2,pa,
     *		pbcorr,pbtype,.true.)
c
c  Save the rotation measure map, if required.
c
	if(Out2.ne.' ')then
	  if(doQU)then
	    call CorrRM(npix,dat(Alpha2),nu0)
	    call WriteOut(Out2,dat(Alpha2),npix,mModel,nModel,
     *		Runs,nruns,lMod1,version,fwhm1,fwhm2,pa,
     *		pbcorr,pbtype,.false.)
	  else
	    call bug('w','Second output data-set name ignored')
	  endif
	endif
c
c  All said and done. Close up the files, and leave.
c
	call MemFree(Flux1,mModel*nModel,'r')
	call MemFree(Alpha1,mModel*nModel,'r')
	call xyclose(lMod1)
	if(doQU)then
	  call MemFree(Flux2,mModel*nModel,'r')
	  call MemFree(Alpha2,mModel*nModel,'r')
	  call xyclose(lMod2)
	endif
c
	end
c************************************************************************
	subroutine QUswap(lMod1,pol1,Modl1,lMod2,pol2,Modl2)
c
	implicit none
	integer pol1,pol2,lMod1,lMod2
	character Modl1,Modl2
c
c  Swap around things.
c------------------------------------------------------------------------
	integer t
	character line*64
c
	t = lMod1
	lMod1 = lMod2
	lMod2 = t
c
	t = pol1
	pol1 = pol2
	pol2 = t
c
	line = Modl1
	Modl1 = Modl2
	Modl2 = line
c
	end
c************************************************************************
	subroutine GetOpt(pbcorr)
c
	implicit none
	logical pbcorr
c
c  Get processing options.
c
c  Output:
c    pbcorr	If true, correct spectral index for primary beam effects.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=1)
	character opts(nopts)*8
	logical present(nopts)
	data opts/'pbcorr  '/
c
	call options('options',opts,present,nopts)
	pbcorr = present(1)
	end
c************************************************************************
	subroutine GetInfo(tmod,nx,ny,
     *	  		pol,nu0,cdelt1,cdelt2,crpix1,crpix2)
c
	implicit none
	integer tmod,nx,ny,pol
	real nu0,cdelt1,cdelt2,crpix1,crpix2
c
c  Determine various info about the model.
c
c  Input:
c    tmod	The handle of the input model.
c    nx,ny	Model size.
c  Output:
c    pol	The polarisation type of the model. If this info is missing
c		then Stokes-I is assumed.
c    nu0	Reference frequency of the model. Default is 0.
c    crpix1,crpix2 Reference pixel -- assumed to be the pointing centre.
c		Default is the model centre.
c    cdelt1,cdelt2 Pixel increments -- No default.
c------------------------------------------------------------------------
	integer StokesI
	parameter(StokesI=1)
	integer naxis,i,n
	character ctype*16,num*2
	real crval,crpix,cdelt,value
c
c  Externals.
c
	character itoaf*2
c
c  The first two axes are assumed to be RA and DEC.
c
	call rdhdr(tmod,'crpix1',crpix1,real(nx/2+1))
	call rdhdr(tmod,'crpix2',crpix2,real(ny/2+1))
	call rdhdr(tmod,'cdelt1',cdelt1,0.)
	call rdhdr(tmod,'cdelt2',cdelt2,0.)
	if(cdelt1*cdelt2.eq.0)
     *		call bug('f','Model pixel increment missing')
c
c  Determine the polarisation and reference frequency of the model.
c
	call rdhdi(tmod,'naxis',naxis,0)
	pol = StokesI
	nu0 = 0
	do i=3,naxis
	  num = itoaf(i)
	  call rdhda(tmod,'ctype'//num,ctype,' ')
	  n = 0
	  if(ctype(1:4).eq.'FREQ'.or.ctype(1:6).eq.'STOKES')then
	    call rdhdi(tmod,'naxis'//num,n,0)
	    if(n.gt.1.and.ctype(1:6).eq.'STOKES')call bug('f',
     *			'Cannot handle polarisation cubes')
	    call rdhdr(tmod,'crval'//num,crval,1.)
	    call rdhdr(tmod,'crpix'//num,crpix,1.)
	    call rdhdr(tmod,'cdelt'//num,cdelt,1.)
	    value = nint( crval + (1-crpix)*cdelt )
	    if(ctype(1:4).eq.'FREQ')then
	      nu0 = value
	    else
	      pol = nint(value)
	    endif
	  endif
	enddo
c
	end
c************************************************************************
	subroutine CorrRM(npix,Data,nu0)
c
	implicit none
	integer npix
	real Data(npix),nu0
c
c  Scale the rotation measure, to get it into units of rad/m**2
c
c  Input:
c    npix	Number of pixels.
c    nu0	Reference frequency, in GHz.
c  Input/Output:
c    Data	Rotation measure.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
	real factor
c
	factor = -nu0*nu0*1e18 / (4*CMKS*CMKS)
c
	do i=1,npix
	  Data(i) = factor * Data(i)
	enddo
	end
c************************************************************************
	subroutine CorrSI(tin,npix,Data,Runs,nRuns,
     *	  crpix1,cdelt1,crpix2,cdelt2,pbtype)
c
	implicit none
	integer nRuns,npix,Runs(3,nRuns),tin
	real Data(npix),crpix1,crpix2,cdelt1,cdelt2
        character pbtype*(*)
c
c  Correct the spectral index, in Data, for the effect of the primary beam.
c
c  Input:
c    tin        Handle of first model image
c    npix	Total number of pixels.
c    nRuns,Runs	Runs specification.
c    crpix1,crpix2 The reference pixel, which is assumed to be the
c		pointing centre.
c    cdelt1,cdelt2 Pixel increment, in radians.
c  Output:
c    pbtype     Type of primary beam correction; 'GAUSS', 'POLY','SINGLE'
c  Input/Output:
c    Data	The spectral index, before and after correction.
c------------------------------------------------------------------------
	integer i,j,k,pix,ierr,ifax
	real theta2,pbcut,pbfwhm
        double precision freq,pbc(5),finc
        real pbget,pbder,pbfac
c
c Find frequency of model image and set primary beam
c
        ifax = 0
        call getfreq(tin,1.0,ifax,freq,finc,ierr)
        if (ierr.gt.0) call bug ('f', 'Could not find model frequency')
        call pbinit(0.0,freq,tin,.true.)
        call pbinfo(pbfwhm,pbc,pbcut,pbtype)
c
        pix = 0
        do k=1,nRuns
          j = Runs(1,k)
          do i=Runs(2,k),Runs(3,k)
	    pix = pix + 1
	    theta2 = cdelt1*cdelt1*(i-crpix1)*(i-crpix1) +
     *	             cdelt2*cdelt2*(j-crpix2)*(j-crpix2)
            pbfac = pbget(theta2)
            if (pbfac.gt.0.0) then
  	      Data(pix) = Data(pix) - freq*pbder(theta2)/pbfac
            else
              Data(pix) = 0.0
            end if
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine Divide1(npix,Num,Denom)
c
	implicit none
	integer npix
	real Num(*),Denom(*)
c------------------------------------------------------------------------
	integer i
c
	do i=1,npix
	  Num(i) = Num(i) / Denom(i)
	enddo
	end
c************************************************************************
	subroutine Divide2(npix,NumR,NumI,DenomR,DenomI)
c
	implicit none
	integer npix
	real NumR(npix),NumI(npix),DenomR(npix),DenomI(npix)
c
c  This divides the complex number, (NumR,NumI) by the complex
c  number (DenomR,DenomI).
c------------------------------------------------------------------------
	integer i
	real tempR,tempI,temp
c
	do i=1,npix
	  temp = DenomR(i)*DenomR(i) + DenomI(i)*DenomI(i)
	  tempR = ( NumR(i)*DenomR(i) + NumI(i)*DenomI(i) ) / temp
	  tempI = ( NumI(i)*DenomR(i) - NumR(i)*DenomI(i) ) / temp
	  NumR(i) = tempR
	  NumI(i) = tempI
	enddo
	end
c************************************************************************
	subroutine Compact(Data,nx,ny,Runs,nRuns,npix)
c
	implicit none
	integer nx,ny,nRuns,Runs(3,nRuns),npix
	real Data(nx*ny)
c
c  Compress out the unneeded data.
c  Input:
c    nx,ny	Array size.
c    Runs,nRuns	Runs specification.
c  Input/Output:
c    data	On input, it contains all the data. On output, just the
c		good data.
c  Output:
c    npix	Total number of good pixels.
c------------------------------------------------------------------------
	integer i,j,k
c
	npix = 0
	do k=1,nRuns
	  j = nx*(Runs(1,k)-1) + Runs(2,k) - 1
	  do i=Runs(2,k),Runs(3,k)
	    npix = npix + 1
	    j = j + 1
	    Data(npix) = Data(j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine Clipper(twoclip,twodat,clip,Dat1,Dat2,nx,ny,
     *						Runs,maxruns,nruns)
c
	implicit none
	logical twoclip,twodat
	integer nx,ny,maxruns,Runs(3,maxruns),nruns
	real clip(2),Dat1(nx,ny),Dat2(nx,ny)
c
c  Determine the good data. Discard data that is below the clip levels.
c
c  Input:
c    twoclip
c    clip	The two clip levels. If twoclip, data between clip(1) and
c		clip(2) is bad. If twoclip is false, data below clip(2)
c		is bad.
c    nx,ny	Image size.
c    Dat1,Dat2	The pixel data.
c    maxruns	Max number of runs.
c  Output:
c    runs	The runs of the good data.
c    nruns	The number of runs.
c------------------------------------------------------------------------
	include 'maxdim.h'
	logical good(MAXDIM)
	integer i,j,ngood
	real clip1,clip2
c
	clip1 = clip(1)
	clip2 = clip(2)
c
	nruns = 0
	do j=1,ny
	  if(twoclip)then
	    if(twodat)then
	      do i=1,nx
		good(i) = ( Dat1(i,j).lt.clip1.or.Dat1(i,j).gt.clip2 )
     *		  .and.   ( Dat2(i,j).lt.clip1.or.Dat2(i,j).gt.clip2 )
	      enddo
	    else
	      do i=1,nx
	        good(i) = Dat1(i,j).lt.clip1.or.Dat1(i,j).gt.clip2
	      enddo
	    endif
	  else
	    if(twodat)then
	      do i=1,nx
		good(i) = Dat1(i,j).gt.clip2.and.Dat2(i,j).gt.clip2
	      enddo
	    else
	      do i=1,nx
	        good(i) = Dat1(i,j).gt.clip2
	      enddo
	    endif
	  endif
c
	  ngood = 0
	  do i=1,nx
	    if(good(i))then
	      ngood = ngood + 1
	    else if(ngood.gt.0)then
	      nruns = nruns + 1
	      if(nruns.ge.MAXRUNS)call bug('f','Runs buffer overflow')
	      Runs(1,nruns) = j
	      Runs(2,nruns) = i-ngood
	      Runs(3,nruns) = i-1
	      ngood = 0
	    endif
	  enddo
	enddo
	end
c************************************************************************
	subroutine WriteOut(Out,Data,npix,nx,ny,Runs,nRuns,
     *	  lModel,version,fwhm1,fwhm2,pa,pbcorr,pbtype,si)
c
	implicit none
	integer lModel,nx,ny,nRuns,npix
	integer Runs(3,nRuns+1)
	real Data(npix)
	character version*(*),out*(*),pbtype*(*)
	real fwhm1,fwhm2,pa
	logical pbcorr,si
c
c  Write out the convolved model to the output file.
c
c  Input:
c    Out	Output image name.
c    Data	The model pixel values.
c    nx,ny	The size of the model.
c    lModel	The handle of the input file -- used to construct output
c		header.
c    version	Program version.
c    fwhm1,fwhm2,pa Gaussian parameters -- for output history.
c    pbcorr	Primary beam corrected?
c    si		Spectral index map?
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,nsize(4),naxis,lOut
	character line*72
	integer nkeys
	parameter(nkeys=35)
	character keyw(nkeys)*8
	data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *		     'crval1  ','crval2  ','crval3  ','crval4  ',
     *		     'crpix1  ','crpix2  ','crpix3  ','crpix4  ',
     *		     'ctype1  ','ctype2  ','ctype3  ','ctype4  ',
     *	  'date-obs','epoch   ','instrume','niters  ','object  ',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ','pbfwhm  ',
     *	  'telescop','xshift  ','yshift  ','history ','restfreq',
     *	  'vobs    ','observer','obsra   ','obsdec  '/
c
c  Open the output.
c
	nsize(1) = nx
	nsize(2) = ny
	nsize(3) = 1
	nsize(4) = 1
	call rdhdi(lModel,'naxis',naxis,2)
	naxis = min(naxis,4)
	call xyopen(lOut,Out,'new',naxis,nsize)
c
c  Write out the data.
c
	call putplane(lOut,Runs,nRuns,0,0,nx,ny,Data,npix)
	call putruns(lOut,Runs,nRuns,0,0,nx,ny)
c
c  Copy keywords across, which have not changed.
c
	do i=1,nkeys
	  call hdcopy(lModel,lOut,keyw(i))
	enddo
	if(si)then
	  call wrbtype(lOut,'spectral_index')
	else
	  call wrbtype(lOut,'rotation_measure')
	  call wrhda(lOut,'bunit','RAD/M/M')
	endif
c
c  Write the history file.
c
	call hisopen(lOut,'append')
	line = 'MFSPIN: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'MFSPIN')
c
	call wrhdr(lOut,'bmaj', fwhm1)
	call wrhdr(lOut,'bmin', fwhm2)
	call wrhdr(lOut,'bpa',  pa)
	write (line, 100) fwhm1*3600*180/pi,fwhm2*3600*180/pi,pa
100	format ('MFSPIN: Beam = ', 1pe10.3, ' x ', 1pe10.3,
     *          ' arcsec, pa = ', 1pe10.3, ' degrees')
	call hiswrite(lOut,line)
	if(pbcorr.and.si)then
          if(pbtype.eq.'GAUSS') then
            call hiswrite(lOut,
     *        'MFSPIN: Gaussian primary beam correction done')
          else if(pbtype.eq.'POLY') then
            call hiswrite(lOut,
     *        'MFSPIN: Polynomial primary beam correction done')
          endif
	endif
	call hisclose(lOut)
c
	call xyclose(lOut)
	end
c************************************************************************
	subroutine GetBeam(mBeam,nBeam,xBeam,yBeam,Beam,
     *			cdelt1,cdelt2,fwhm1,fwhm2,pa)
c
	implicit none
	integer mBeam,nBeam,xBeam,yBeam
	real Beam(mBeam,nBeam),cdelt1,cdelt2,fwhm1,fwhm2,pa
c
c  Get the effective beam, less any gaussian.
c
c  Input:
c    mBeam,nBeam  Size of the beam.
c    cdelt1,cdelt2,fwhm1,fwhm2,pa Gaussian parameters.
c  Output:
c    Beam	  The output beam.
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	integer i,j
	real theta,s2,c2,sxx,syy,sxy,a,b,t
c
c  Determine the beam.
c
	theta = pi/180. * pa
	s2 = -sin(2*theta)
	c2 = -cos(2*theta)
	a = 4*log(2.)/(fwhm1*fwhm1)
	b = 4*log(2.)/(fwhm2*fwhm2)
	sxx = -0.5*( a*(c2+1) + b*(1-c2) ) * cdelt1*cdelt1
	syy = -0.5*( b*(c2+1) + a*(1-c2) ) * cdelt2*cdelt2
	sxy = -(b-a)*s2 * cdelt1*cdelt2
	do j=1,nBeam
	  do i=1,mBeam
	    t = sxx*(i-xBeam)*(i-xBeam) + sxy*(i-xBeam)*(j-yBeam) +
     *	      syy*(j-yBeam)*(j-yBeam)
	    if(t.gt.-20)then
	      Beam(i,j) = exp(t)
	    else
	      Beam(i,j) = 0
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GetPatch(lBeam,n1,n2,Patch,nP,xBeam,yBeam)
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
c    Patch	The central portion of the beam, centered aound the pixel
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
c    cdelt1,cdelt2 Grid increments, in degrees.
c
c  Outputs:
c    Fwhm1	Fwhm, in degrees along the major axis.
c    Fwhm2	Fwhm, in degrees along the minor axis.
c    Pa		Position angle, in degrees, measured east of north.
c
c------------------------------------------------------------------------
	integer MaxIter
	real pi
	parameter(MaxIter=100,pi=3.141592653589793)
	include 'mfspin.h'
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
	include 'mfspin.h'
	integer i
	real temp
c
	do i=1,m
	  temp = sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3)
	  if(temp.gt.-20)then
	    temp = exp(temp)
	  else
	    temp = 0
	  endif
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
	include 'mfspin.h'
	integer i
	real temp
c
	do i=1,m
	  temp = sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3)
	  if(temp.gt.-20)then
	    f(i) = Patch(i) - exp(temp)
	  else
	    f(i) = Patch(i)
	  endif
	enddo
        end
