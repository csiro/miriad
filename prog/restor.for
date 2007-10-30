c************************************************************************
	program restor
	implicit none
c
c= restor - Restore clean components to make the CLEAN map
c& rjs mchw
c: deconvolution
c+
c	RESTOR is a MIRIAD task which performs a number of functions
c	typically performed after the deconvolution step. These include
c	generating a "CLEAN" map, calculating residuals and convolving
c	a model by a gaussian beam.
c
c	RESTOR can also handle multi-frequency synthesis data. In this
c	case the input dirty map should consist of just one plane. The input
c	beam may contain several planes, and the input model may have no
c	more planes (but possibly fewer) than the beam. To get the residuals,
c	each plane in the model is convolved with the corresponding plane in
c	the beam, and subtracted from the dirty map.
c@ model
c	The model of the deconvolved cube. Usually this will be produced
c	by CLEAN or MAXEN. The units of this image should be Jy/pixel. No
c	default.
c@ beam
c	The input dirty beam. No default.
c@ map
c	The input dirty cube, which should have units of Jy/beam. This
c	can be omitted when mode=convolve. Otherwise no default.
c@ mode
c	This can be one of the values:
c	  "clean"     This is the normal use, and the default, where the
c	              output is the map, less the model convolved by the
c	              dirty beam, plus the model convolved by the gaussian
c	  "residual"  The output is the map, less the model convolved by the
c	              dirty beam.
c	  "convolve"  The output is the model convolved by the gaussian. The
c	              map parameter is ignored, and the beam is needed only if
c	              the user does not give the gaussian fwhm and pa.
c	  "dirty"     The output is the map convolved with the beam.
c@ fwhm
c	The size, in arcsec, of the gaussian beam to use in the
c	restoration. This will normally be two numbers, giving the
c	full-width at half-maximum of the major and minor axes of the
c	gaussian. If only one number is given, the gaussian will have
c	equal major and minor axes. If no values are given, they are
c	either retrieved from the beam header, or computed by fitting a
c	gaussian to the given dirty beam. Note that the fitting routine
c	will probably give different values to the AIPS MX and APCLN tasks.
c	Generally the value computed by RESTOR is to be preferred to the
c	APCLN and MX values.
c@ pa
c	The position angle, in degrees, of the gaussian restoring beam,
c	measured east from north. The default is determined from the dirty
c	beam fit (The value for PA is ignored, if no value is given for
c	FWHM).
c@ out
c	The output restored image. No default.
c--
c
c  History:
c    rjs Dark_ages Original version.
c    rjs 16aug89   Some minor formatting enhancements.
c    rjs 22sep89   Protected against the case when cdelt==0.
c    rjs  1mar90   Changed call sequence of nextpow2.
c    mchw 20jun90  Added linetype keywords to output image header.
c    mchw 17jul90  Increased filename lengths. Added version.
c    mchw 05oct90  Option to add sub-image into map.
c    rjs  20mar91  Tolerate multi-plane beams (for mfs).
c    rjs   8apr91  Properly handle multi-freq synthesis. Also use Mem
c		   routines. Various minor improvements.
c    mchw 24apr91  Restored the doc for mode=add.
c    rjs  11jun91  Changed doc slightly.
c    rjs  16sep91  Improved doc slightly. Uses the new cnvl routines.
c    rjs   9oct91  Fixed bug which happens when there is no beam.
c    mjs  13mar92  RESTORE -> RESTOR (name change)
c    mchw 19mar92  Restored pbfwhm to header parameters copied.
c    rjs  22apr92  Doc and output message changes (for Lauren).
c    rjs   4may92  Avoid floating underflows.
c    rjs  26may92  Add btype and rms to list of variables to copy.
c    rjs  25jun92  Use keymatch routine.
c    rjs  17jan94  Make sure beam is writable before writing out
c		   beam parameters.
c    rjs  16nov94  Substantial rework. Use new "restore" routines.
c    rjs   3dec94  Copy mosaic table across, if it exists.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Restor: version 1.2 3-Dec-94')
c
	include 'maxdim.h'
	include 'mem.h'
	include 'mirconst.h'
c
	character map*64,beam*64,modl*64,out*64,mode*16,line*72,iomode*8
	real fwhm1,fwhm2,pa,rms
	logical dogaus,domap,dofit
	integer naxis,nsize(4),xoff,yoff,zoff,i,x0,y0
	integer mMap,nMap,oMap,mBeam,nBeam,mModel,nModel,oModel
	integer mCnvl,nCnvl
	integer lMap,lBeam,lModel,lOut
	integer pOut
c
c  Externals.
c
	character itoaf*4
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('map',Map,' ')
	call keya('beam',Beam,' ')
	call keya('model',modl,' ')
	call keya('out',Out,' ')
	call keyr('fwhm',fwhm1,0.)
	call keyr('fwhm',fwhm2,fwhm1)
	call keyr('pa',pa,0.)
	call GetMode(mode)
	call keyfin
c
c  Check the reasonableness of the inputs.
c
	domap  = mode.eq.'clean'.or.mode.eq.'residual'
	dogaus = mode.eq.'convolve'.or.mode.eq.'clean'
	dofit  = dogaus.and.fwhm1*fwhm2.eq.0
c
	if(Modl.eq.' ')call bug('f','Input model name missing')
	if(Out.eq.' ') call bug('f','Output map name missing')
	if(Beam.eq.' ')call bug('f','Input beam name missing')
	if(Map.eq.' '.and.domap)
     *	  call bug('f','Input map name missing')
c
c  Convert beam units.
c
	fwhm1 = pi/180/3600 * fwhm1
	fwhm2 = pi/180/3600 * fwhm2
c
c  Open the model and the beam.
c
	call xyopen(lModel,Modl,'old',3,nsize)
	mModel = nsize(1)
	nModel = nsize(2)
	oModel = nsize(3)
	call rdhdi(lModel,'naxis',naxis,3)
	naxis = min(naxis,4)
	call coInit(lModel)
c
	call xyopen(lBeam,Beam,'old',3,nsize)
	mBeam = nsize(1)
	nBeam = nsize(2)
	if(dofit)then
	  call rdhdr(lBeam,'bmaj',fwhm1,0.)
	  call rdhdr(lBeam,'bmin',fwhm2,0.)
	  call rdhdr(lBeam,'bpa',pa,0.)
	  dofit = fwhm1*fwhm2.le.0
	endif
c
c  Fit the beam, if necessary.
c
	if(dofit)then
	  call BeamFit(lBeam,mBeam,nBeam,fwhm1,fwhm2,pa)
	  call hmode(lBeam,iomode)
	  if(index(iomode,'w').ne.0)then
	    call wrhdr(lBeam,'bmaj',fwhm1)
	    call wrhdr(lBeam,'bmin',fwhm2)
	    call wrhdr(lBeam,'bpa',pa)
	  endif
	endif
c
c  Keep the user awake with spurious information.
c
	if(dogaus)then
	  write(line,'(a,f6.2,a,f6.2,a)')
     *	   'Using gaussian beam fwhm of',(3600*180/pi)*fwhm1,' by',
     *	   (3600*180/pi)*fwhm2,' arcsec.'
	  call output(line)
	  write(line,'(a,f6.1,a)')'Position angle: ',pa,' degrees.'
	  call output(line)
	endif
c
c  Reset the beam to a dummy size, if we are just convolving
c  with a gaussian.
c
	if(mode.eq.'convolve')then
	  mBeam = 1
	  nBeam = 1
	endif
c
c  Determine the alignment between the map and the model.
c  The conversion from Model grid units to Map grid units is to
c  add (xoff,yoff,zoff) to the coordinate.
c
	if(domap)then
	  call xyopen(lMap,Map,'old',3,nsize)
	  call rdhdr(lMap,'rms',rms,0.0)
	  mMap = nsize(1)
	  nMap = nsize(2)
	  oMap = nsize(3)
	  call align(lMap,lModel,mMap,nMap,oMap,xoff,yoff,zoff)
	else
	  rms = 0
	  mMap = mModel + mBeam - 1
	  nMap = nModel + nBeam - 1
	  oMap = oModel
	  xoff = mBeam/2
	  yoff = nBeam/2
	  zoff = 0
	endif
	mCnvl = mBeam
	nCnvl = nBeam
	if(mMap.gt.mBeam)mCnvl = min(mMap,mBeam+mModel-1)
	if(nMap.gt.nBeam)nCnvl = min(nMap,nBeam+nModel-1)
c
	x0 = xoff - (mCnvl/2 - mModel/2)
	y0 = yoff - (nCnvl/2 - nModel/2)
c
c  Initialise the convolution routines.
c
	if(mode.eq.'residual')mode = 'dirty'
	call RestIni(lBeam,mCnvl,nCnvl,fwhm1,fwhm2,pi/180*pa,mode)
c
c  Open the output, and create its header.
c
	nsize(1) = mMap
	nsize(2) = nMap
	nsize(3) = oMap
	nsize(4) = 1
	call xyopen(lOut,Out,'new',naxis,nsize)
c
	call header(lModel,mModel,nModel,lOut,
     *	  version,dogaus,fwhm1,fwhm2,pa,rms,xoff,yoff,zoff)
	if(doMap) call hdcopy(lMap,lOut,'mostable')
c
c  Loop over the third dimension.
c
	call memAlloc(pOut,mCnvl*nCnvl,'r')
	do i=1,oMap
          if (mod(i,10).eq.0 .or. (i.eq.1.and.oMap.ge.10))
     *	    call output ('Beginning plane '//itoaf(i))
  	  call xysetpl(lOut,1,i)
	  if(domap)call xysetpl(lMap,1,i)
	  if(i-zoff.ge.1.and.i-zoff.le.oModel)then
	    call Restore(lModel,i-zoff,memr(pOut))
	    if(domap)then
	      call SubModel(lMap,mMap,nMap,
     *		  memr(pOut),mCnvl,nCnvl,lOut,x0,y0)
	    else
	      if(mMap.ne.mCnvl.or.nMap.ne.nCnvl)
     *			call bug('f','Algorithmic failure')
	      call WriteOut(lOut,memr(pOut),mMap,nMap)
	    endif
	  else
	    call CopyOut(lMap,lOut,mMap,nMap)
	  endif
	enddo
c
c  All said and done. Close up the files, and leave.
c
	call memFree(pOut,mCnvl*nCnvl,'r')
	call RestFin
	call xyclose(lModel)
	call xyclose(lOut)
	if(doMap) call xyclose(lMap)
	call xyclose(lBeam)
c
	end
c************************************************************************
	subroutine GetMode(mode)
c
	implicit none
	character mode*(*)
c
c  Determine the operation to perform.
c  Output:
c    mode	The operation to be performed.
c------------------------------------------------------------------------
	integer nout
	integer nopt
	parameter(nopt=4)
	character opts(nopt)*8
	data opts/'clean   ','residual','convolve','dirty   '/
c
	call keymatch('mode',nopt,opts,1,mode,nout)
	if(nout.eq.0) mode = 'clean'
	end
c************************************************************************
	subroutine align(lMap,lModel,mMap,nMap,oMap,xoff,yoff,zoff)
c
	implicit none
	integer lMap,lModel
	integer mMap,nMap,oMap,xoff,yoff,zoff
c
c  Determine the alignment parameters between the map and the model.
c  This insists that they line up on pixels.
c
c  Input:
c    lMap	Handle of the map file.
c    lModel	Handle of the model file.
c    mMap,nMap,oMap Map dimensions.
c
c  Output:
c    xoff,yoff,zoff These values have to be added to the grid units of
c		the model to convert them to grid units of the map.
c
c------------------------------------------------------------------------
	integer i,offset(3),nsize(3)
	character num*1
	real vM,dM,rM,vE,dE,rE,temp
c
c  Externals.
c
	character itoaf*2
c
	nsize(1) = mMap
	nsize(2) = nMap
	nsize(3) = oMap
c
	do i=1,3
	  num = itoaf(i)
	  call rdhdr(lMap,'crval'//num,vM,0.)
	  call rdhdr(lMap,'cdelt'//num,dM,1.)
	  call rdhdr(lMap,'crpix'//num,rM,1.)
	  call rdhdr(lModel,'crval'//num,vE,0.)
	  call rdhdr(lModel,'cdelt'//num,dE,1.)
	  call rdhdr(lModel,'crpix'//num,rE,1.)
	  if(dE.eq.0)call bug('f','Increment on axis '//num//' is zero')
	  temp = (vM-vE)/dE + (rM-rE)
	  offset(i) = nint(temp)
	  if(abs(offset(i)-temp).gt.0.05)
     *	    call bug('f','Map and model do not align on pixels')
	  if(abs(dM-dE).ge.0.001*abs(dM))
     *	    call bug('f','Map and model increments are different')
	  if(offset(i).lt.0.or.offset(i).ge.nsize(i))
     *	    call bug('f','Map and model do not overlap well')
	enddo
c
	xoff = offset(1)
	yoff = offset(2)
	zoff = offset(3)
c
	end
c************************************************************************
	subroutine Header(lModel,mModel,nModel,lOut,
     *	  version,dogaus,fwhm1,fwhm2,pa,rms,xoff,yoff,zoff)
c
	implicit none
	character version*(*)
	integer mModel,nModel,lModel,lOut,xoff,yoff,zoff
	real fwhm1,fwhm2,pa,rms
	logical dogaus
c
c  Output the map.
c
c  Inputs:
c    fwhm1	Beam major axis width (radians).
c    fwhm2	Beam minor axis width (radians).
c    pa		Position angle of the gaussian beam (degrees).
c    rms	Theoretical RMS noise in the image.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
	real crpix
	character line*72
	integer nkeys
	parameter(nkeys=30)
	character keyw(nkeys)*8
	data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *	  'crpix4  ','crval1  ','crval2  ','crval3  ','crval4  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','btype   ',
     *	  'obstime ','epoch   ','niters  ','object  ',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ','pbfwhm  ',
     *	  'telescop','history ','restfreq',
     *	  'vobs    ','observer','obsra   ','obsdec  '/
c
c  Copy keywords across, which have not changed.
c
	do i=1,nkeys
	  call hdcopy(lModel,lOut,keyw(i))
	enddo
c
	call rdhdr(lModel,'crpix1',crpix,real(mModel/2+1))
	call wrhdr(lOut,'crpix1',crpix+xoff)
c
	call rdhdr(lModel,'crpix2',crpix,real(nModel/2+1))
	call wrhdr(lOut,'crpix2',crpix+yoff)
c
	call rdhdr(lModel,'crpix3',crpix,1.)
	call wrhdr(lOut,'crpix3',crpix+zoff)
c
	call rdhda(lModel,'bunit',line,' ')
	i = index(line,'/PIXEL')
	if(i.eq.0.and.line.ne.' ')then
	  call bug('w','The model units are not /PIXEL')
	else if(i.ne.0)then
	  line(i:len(line)) = '/BEAM'
	  call wrhda(lOut,'bunit',line)
	endif
c
c  Write the history file.
c
	call hisopen(lOut,'append')
	line = 'RESTOR: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'RESTOR')
c
	if(rms.gt.0)call wrhdr(lOut,'rms',rms)
c
	if(dogaus)then
	  call wrhdr(lOut,'bmaj', fwhm1)
	  call wrhdr(lOut,'bmin', fwhm2)
	  call wrhdr(lOut,'bpa',  pa)
	  write (line, 100) fwhm1*3600*180/pi,fwhm2*3600*180/pi,pa
100	  format ('RESTOR: Beam = ', 1pe10.3, ' x ', 1pe10.3,
     *          ' arcsec, pa = ', 1pe10.3, ' degrees')
	  call hiswrite(lOut,line)
	else
	  call hiswrite(lOut,'RESTOR: No gaussian')
	endif
c
	call hisclose(lOut)
c
	end
c************************************************************************
	subroutine WriteOut(lOut,Out,mOut,nOut)
c
	implicit none
	integer lOut,mOut,nOut
	real Out(mOut,nOut)
c
c  Write out the convolved model to the output file.
c
c  Input:
c    Model	The model pixel values.
c    mModel,nModel The size of the model.
c    lOut	The handle of the output file.
c------------------------------------------------------------------------
	integer j
c
	do j=1,nOut
	  call xywrite(lOut,j,Out(1,j))
	enddo
	end
c************************************************************************
	subroutine CopyOut(lIn,lOut,mOut,nOut)
c
	implicit none
	integer lIn,lOut,mOut,nOut
c
c  Copy a map from the input to the output.
c
c  Inputs:
c    lIn	Handle of the input map file.
c    lOut	Handle of the output map file.
c    mOut,nOut	Size of the map.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer j
	real Data(MAXDIM)
c
	if(mOut.gt.MAXDIM)call bug('f','Output too big')
c
	do j=1,nOut
	  call xyread(lIn,j,Data)
	  call xywrite(lOut,j,Data)
	enddo
c
	end
c************************************************************************
	subroutine SubModel(lMap,mMap,nMap,Model,mModel,nModel,
     *						lOut,xoff,yoff)
c
	implicit none
	integer lMap,mMap,nMap,mModel,nModel,lOut,xoff,yoff
	real Model(mModel,nModel)
c
c  This subtracts the convolved model from the map, and writes the
c  result out.
c
c  Input:
c    lMap	Handle of the input map.
c    mMap,nMap	Map size.
c    mModel,nModel Model size.
c    Model	Model pixel values.
c    xoff,yoff	Offsets to add to a model pixel coordinate to make it
c		a map pixel coordinate.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,ilo,ihi,jlo,jhi
	real Map(maxdim)
c
c  Read the map in and add the model to it.
c
	jlo = max(1,yoff+1)
	jhi = min(nMap,yoff+nModel)
	ilo = max(1,xoff+1)
	ihi = min(mMap,xoff+mModel)
c
	do j=1,nMap
	  call xyread(lMap,j,Map)
	  if(j.ge.jlo.and.j.le.jhi)then
	    do i=ilo,ihi
	      Map(i) = Map(i) - Model(i-xoff,j-yoff)
	    enddo
	  endif
	  call xywrite(lOut,j,Map)
	enddo
c
	end
c************************************************************************
	subroutine BeamFit(lBeam,mBeam,nBeam,fwhm1,fwhm2,pa)
c
	implicit none
	integer lBeam,mBeam,nBeam
	real fwhm1,fwhm2,pa
c
c  Fit the beam parameters.
c
c  Input:
c    lBeam	 Handle of the beam dataset.
c    mBeam,nBeam Size of the beam.
c  Output:
c    fwhm1,fwhm2 Beam major and minor axes (radians).
c    bpa	 Beam position angle (degrees).
c------------------------------------------------------------------------
	integer nP
	parameter(nP=11)
	real Patch(nP*nP),cdelt1,cdelt2
	integer nPd,xBeam,yBeam
c
	nPd = min(nP,mBeam,nBeam)
	nPd = nPd - mod(nPd+1,2)
	call GetPatch(lBeam,mBeam,nBeam,Patch,nPd,xBeam,yBeam)
	call rdhdr(lBeam,'cdelt1',cdelt1,0.)
	call rdhdr(lBeam,'cdelt2',cdelt2,0.)
	if(cdelt1*cdelt2.eq.0)
     *		call bug('f','Model pixel increment missing')
	call GetFwhm(Patch,nPd,xBeam-mBeam/2+nPd/2,
     *	    yBeam-nBeam/2+nPd/2,cdelt1,cdelt2,Fwhm1,Fwhm2,Pa)
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
	include 'mirconst.h'
	integer MaxIter
	parameter(MaxIter=100)
	include 'restor.h'
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
	include 'restor.h'
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
	include 'restor.h'
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
