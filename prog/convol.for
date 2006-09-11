c************************************************************************
	program convol
	implicit none
c= convol - Convolve a cube with a "beam function"
c& rjs mchw
c: map manipulation, map analysis
c	CONVOL is a MIRIAD task which convolves a map or model with a "beam".
c@ map
c	The input image. This can be two or three dimensional. No default.
c@ beam
c	The input beam. This cannot be 3-dimensional. The beam is generally
c	assumed to be symmetrical about its reference pixel, but see the
c	"asymmetric" option if this is not so. If this is not given, then
c	a gaussian beam must be specified by the fwhm and pa parameters.
c@ fwhm
c	The size, in arcsec, of the gaussian beam to be used in place
c	of the beam parameter. Either beam or fwhm MUST be set.
c	This parameter will normally be two numbers, giving the
c	full-width at half-maximum of the major and minor axes of the
c	gaussian. If only one number is given, the gaussian will have
c	equal major and minor axes.
c@ pa
c	The position angle, in degrees, of the gaussian beam,
c	measured east from north. The default is determined from the dirty
c	beam fit (The value for PA is ignored, if no value is given for
c	FWHM).
c@ region
c	The region of the input map to convolve. See the Users Manual for
c	instructions on how to specify this. The default is the entire
c	input image.
c@ out
c	The output image. No default.
c@ options
c	Some extra processing options. Several can be given, separated
c	by commas. Minimum match is used.
c	  "divide"   Divide, rather than mulitple, the transform of the map
c	             by the transform of the beam. That is, perform
c	             deconvolution, rather than convolution.
c	  "asymmetric" Normally the beam is assumed to be symmetric (as it
c	             normally is in radio astronomy). This options causes
c	             CONVOL to go through the extra steps to handle a possibly
c	             asymmetric beam. If the beam is asymmetric, and you go
c	             not give this option, CONVOL symmetrises the beam (i.e.
c	             discards the anti-symmetric component).
c	  "correlate" Correlate rather than convolve with the beam. The
c	             difference between correlation and convolution are only
c	             apparent for asymmetric beams.
c@ scale
c	The scale factor to multiply the output by. The default is for
c	CONVOL to determine the appropriate scale factor to make the
c	output in JY/BEAM.
c	NOTE: If the input image is in units of JY/BEAM, then to determine
c	the appropriate scale factor, CONVOL must know the beam parameters
c	(bmaj, bmin and bpa items) of both the input map and the beam. If
c	these items are not present, CONVOL issues a warning and uses a scale
c	factor of 1.
c@ sigma
c	When doing devonvolution (options=divide), this gives a noise
c	parameter. Default is 0.
c--
c  History:
c    rjs,mchw 18aug89 Converted from RESTORE.
c    rjs      30apr90 Changed call sequence to Boxinput.
c    mchw     04dec90 Added pbfwhm to header.
c    mchw     03apr91 Minor mods to doc and checking inputs.
c    rjs      03apr91 Use the Mem routines. Fixed bug relating to beam and
c		      image of different sizes.
c    rjs,mchw 30may91 Make it handle asymmetric beams, by using the
c		      ConvlC routines. Better handling of units. Divide
c		      option.
c    rjs      13sep91 Friday the 13th change. Use new convolution routines.
c		      Writes mask. Options=asymmetric. Corrections to the doc.
c    rjs      10mar92 Increased size of maxruns.
c    nebk     25nov92 Copy btype to output
c    rjs      22nov93 Handle gaussian beam. Get rid of "scale" option.
c    rjs      11jan93 Honour explicit scale factors.
c    mchw  06sep94 Set default bmin to be bmaj, and fix log line in doc.
c
c  Bugs:
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer maxbox,maxruns
	character version*(*)
	parameter(version='Convol: version 1.0 06-Sep-94' )
	parameter(maxruns=3*maxdim)
	parameter(maxbox=1024)
	character map*32,beam*32,out*32
	integer nsize(3),naxis
	integer lMap,lBeam,lOut,iref,jref,blc(3),trc(3)
	integer xmin,xmax,ymin,ymax,nx,ny,n1,n2,xoff,yoff
	integer nPoint,nRuns,k,l,Box(maxbox),Runs(3,maxRuns)
	double precision cdelt1,cdelt2
	real crpix1,crpix2,bmaj,bmin,bpa,bmaj1,bmin1,bpa1,factor,sigma
	character bunit*32,flags*4
	logical divide,selfscal,rect,asym,corr,doscale,dogaus
c
	integer handle,pDat
	include 'mem.h'
c
c  Externals.
c
	logical BoxRect,keyprsnt
	character itoaf*8
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('map',Map,' ')
	call keya('beam',Beam,' ')
	doGaus = Beam.eq.' '
	if(doGaus)then
	  call keyr('fwhm',bmaj1,0.)
	  call keyr('fwhm',bmin1,bmaj1)
	  call keyr('pa',bpa1,0.)
	endif
	call keya('out',Out,' ')
	call BoxInput('region',map,box,maxbox)
	call GetOpt(divide,asym,corr)
	selfscal = .not.(divide.or.keyprsnt('scale'))
	call keyr('scale',factor,1.)
	call keyr('sigma',sigma,0.)
	call keyfin
c
c  Check the reasonableness of the inputs.
c
	if(map.eq.' ') call bug('f','Input map missing')
	if(out.eq.' ') call bug('f','Output map missing')
	if(.not.divide) sigma = 0
	if(divide.and.sigma.eq.0)
     *	  call bug('f','Sigma must be set, when using options=divide')

c  Open the map and handle the boxes to be processed.
c
	call xyopen(lMap,map,'old',3,nsize)
	nx = nsize(1)
	ny = nsize(2)
	call rdhdi(lMap,'naxis',naxis,3)
	call rdhdd(lMap,'cdelt1',cdelt1,1.d0)
	call rdhdd(lMap,'cdelt2',cdelt2,1.d0)
	naxis = min(naxis,3)
c
	call BoxMask(lMap,box,maxbox)
	call BoxSet(box,3,nsize,' ')
	call BoxInfo(box,3,blc,trc)
	rect = BoxRect(box)
c
c  Fiddle the gaussian parameters.
c
	doGaus = beam.eq.' '
	if(doGaus)then
	  n1 = nx
	  n2 = ny
	  iref = nx/2 + 1
	  jref = ny/2 + 1
	  bmaj1 = pi/180./3600. * bmaj1
	  bmin1 = pi/180./3600. * bmin1
	  if(bmaj1*bmin1.le.0)call bug('f',
     *	    'Either a beam or gaussian must be given')
c
c  Open the beam.
c
	else
	  call xyopen(lBeam,beam,'old',2,nsize)
	  n1 = nsize(1)
	  n2 = nsize(2)
	  call rdhdr(lBeam,'crpix1',crpix1,real(n1/2+1))
	  call rdhdr(lBeam,'crpix2',crpix2,real(n2/2+1))
	  iref = nint(crpix1)
	  jref = nint(crpix2)
	endif
c
c  Check that the map and beam sizes and deltas are the same.
c
	if(nx.gt.n1.or.ny.gt.n2)
     *	  call bug('f','Map must be smaller than the beam')
c
c  Determine the units,etc, of the output, along with any scale
c  factors.
c
	if(selfscal)then
	  if(doGaus)then
	    call GauPar1(lMap,bmaj1,bmin1,bpa1,
     *				   bunit,bmaj,bmin,bpa,factor)
	  else
	    call GauPar2(lMap,lBeam,bunit,bmaj,bmin,bpa,factor)
	  endif
	else
	  call rdhda(lMap,'bunit',bunit,' ')
	  bmaj = 0
	  bmin = 0
	  bpa = 0
	endif
	doscale = abs(factor-1).gt.1e-3
c
c  Calculate the transform of the beam.
c
	l = 0
	flags = ' '
	if(.not.asym)then
	  l = l + 1
	  flags(l:l) = 's'
	endif
	if(divide)then
	  l = l + 1
	  flags(l:l) = 'd'
	endif
	if(corr)then
	  l = l + 1
	  flags(l:l) = 'x'
	endif
	if(doGaus)then
	  call memAlloc(pDat,n1*n2,'r')
	  call GauGen(memR(pDat),n1,n2,iref,jref,
     *			bmaj1,bmin1,bpa1,cdelt1,cdelt2)
	  call CnvlIniA(handle,memR(pDat),n1,n2,iref,jref,
     *						     sigma,flags)
	  call memFree(pDat,n1*n2,'r')
	else
	  call CnvlIniF(handle,lBeam,n1,n2,iref,jref,sigma,flags)
	  call xyclose(lBeam)
	endif
c
c  Open the output, and create its header.
c
	nsize(1) = trc(1) - blc(1) + 1
	nsize(2) = trc(2) - blc(2) + 1
	nsize(3) = trc(3) - blc(3) + 1
	call xyopen(lOut,Out,'new',naxis,nsize)
	call header(lMap,lOut,naxis,blc,
     *	  bunit,bmaj,bmin,bpa,version)
	call MemAlloc(pDat,nsize(1)*nsize(2),'r')
c
c  Loop over the third dimension.
c
	do k=blc(3),trc(3)
          if (mod(k-blc(3),10).eq.0 .and. blc(3).ne.trc(3))
     *	    call output('Beginning plane '//itoaf(k))
c
c  Get the run spec. and read in the data.
c
	  call BoxRuns(1,k,'r',box,Runs,MaxRuns,nRuns,
     *					xmin,xmax,ymin,ymax)
	  nx = xmax - xmin + 1
	  ny = ymax - ymin + 1
	  xoff = xmin - blc(1)
	  yoff = ymin - blc(2)
c
	  call xysetpl(lMap,1,k)
	  call GetPlane(lMap,Runs,nRuns,xmin-1,ymin-1,nx,ny,
     *				memR(pDat),nsize(1)*nsize(2),nPoint)
c
c  Do the real work.
c
	  call CnvlR(handle,memR(pDat),nx,ny,Runs,nRuns,memR(pDat),'c')
c
c  Apply a scale factor, if needed.
c
	  if(doscale)call Scale(memR(pDat),nPoint,factor)
c
c  Write out this plane.
c
	  call xysetpl(lOut,1,k-blc(3)+1)
	  call PutPlane(lOut,Runs,nRuns,xoff,yoff,
     *				nsize(1),nsize(2),memR(pDat),nPoint)
c
c  Write out a blanking mask, if needed.
c
	  if(.not.rect)
     *	    call PutRuns(lOut,Runs,nRuns,xoff,yoff,nsize(1),nsize(2))
	enddo
c
c  All said and done. Close up the files, and leave.
c
	call xyclose(lOut)
	call xyclose(lMap)
c
	end
c************************************************************************
	subroutine GauGen(Data,n1,n2,iref,jref,
     *			bmaj,bmin,bpa,cdelt1,cdelt2)
c
	implicit none
	integer n1,n2,iref,jref
	real Data(n1,n2),bmaj,bmin,bpa
	double precision cdelt1,cdelt2
c
c  Generate a gaussian.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	real theta,s2,c2,a,b,sxx,syy,sxy,t
	integer i,j
c
	theta = pi/180. * bpa
	s2 = -sin(2*theta)
	c2 = -cos(2*theta)
	a = 4*log(2.) / (bmaj*bmaj)
	b = 4*log(2.) / (bmin*bmin)
	sxx = -0.5*( a*(c2+1) + b*(1-c2) ) * cdelt1*cdelt1
	syy = -0.5*( b*(c2+1) + a*(1-c2) ) * cdelt2*cdelt2
	sxy = -(b-a)*s2 * cdelt1*cdelt2
c
	do j=1,n2
	  do i=1,n1
	    t = sxx*(i-iref)*(i-iref) + sxy*(i-iref)*(j-jref) + 
     *		syy*(j-jref)*(j-jref)
	    if(t.gt.-20)then
	      Data(i,j) = exp(t)
	    else
	      Data(i,j) = 0
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(divide,asym,corr)
c
	implicit none
	logical divide,asym,corr
c
c  Get extra processing options.
c
c  Output:
c    divide	True if we are really deconvolving.
c    asym	Is the beam asymmetric?
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=3)
	logical present(nopts)
	character opts(nopts)*10
	data opts/'divide    ','asymmetric','correlate '/
c
	call options('options',opts,present,nopts)
	divide = present(1)
	asym    = present(2)
	corr = present(3)
	if(.not.asym.and.corr) call bug('w',
     *  'Correlation and convolution do not differ for symmetric beams')
	end
c************************************************************************
	subroutine Scale(Data,n,factor)
c
	integer n
	real Data(n),factor
c
c  Multiply by a scale factor.
c
c  Input:
c    n		Number of points.
c    factor	Scale factor.
c  In/Out:
c    Data	The data to scale.
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  Data(i) = factor * Data(i)
	enddo
	end
c************************************************************************
	subroutine Header(lMap,lOut,naxis,blc,
     *	  bunit,bmaj,bmin,bpa,version)
c
	implicit none
	character version*(*),bunit*(*)
	integer lMap,lOut,naxis,blc(naxis)
	real bmaj,bmin,bpa
c
c  Output the map header.
c
c  Inputs:
c    lMap	Handle of the input map.
c    lOut	Handle of the output map.
c    naxis	Number of dimensions in the input.
c    blc	Bottom left corner of the input which is going to (1,1,1)
c		ofthe output.
c    bunit	Units of the output.
c    bmaj,bmin,bpa Effective beam parameters of the output.
c    version	Version of this program.
c------------------------------------------------------------------------
	integer i
	character line*72,num*1
	real crpix
	integer nkeys
	parameter(nkeys=25)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*2
c
	data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ','crval1  ',
     *	  'crval2  ','crval3  ','ctype1  ','ctype2  ','ctype3  ',
     *	  'date-obs','epoch   ','instrume','niters  ','object  ',
     *	  'telescop','xshift  ','yshift  ','history ','restfreq',
     *	  'vobs    ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *    'btype   '/
c
c  Copy keywords across, which have not changed.
c
	do i=1,nkeys
	  call hdcopy(lMap,lOut,keyw(i))
	enddo
c
c  Handle the reference pixels.
c
	do i=1,naxis
	  num = itoaf(i)
	  call rdhdr(lMap,'crpix'//num,crpix,1.)
	  call wrhdr(lOut,'crpix'//num,crpix-blc(i)+1)
	enddo
c
c  Set the parameters to determine the units and effective beam.
c
	if(bunit.ne.' ')call wrhda(lOut,'bunit',bunit)
	if(bmaj*bmin.ne.0)then
	  call wrhdr(lOut,'bmaj',bmaj)
	  call wrhdr(lOut,'bmin',bmin)
	  call wrhdr(lOut,'bpa',bpa)
	endif
c
c  Write the history file.
c
	call hisopen(lOut,'append')
	line = 'CONVOL: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'CONVOL')
	call hisclose(lOut)
c
	end
