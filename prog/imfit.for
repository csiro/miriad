c************************************************************************
	program imfit
	implicit none
c
c= imfit -- Fit models to a given image dataset
c& rjs
c: image analysis
c	IMFIT is a Miriad task which fits model components to a
c	image data-set. If several image planes are given, each plane
c	is fitted separately. Optionally the model or the residuals can be
c	written out.
c
c	To get a good fit, it is important that you keep irrelevant
c	pixels out of the fitting process, by using the region
c	and/or the clip keywords. Also for multi-component fits,
c	it is important to give reasonable estimates of the source
c	parameters of the components (spar keyword).
c@ in
c	Name of the input image dataset. No default.
c@ region
c	Normal region of interest. The fit is performed only in this
c	region. This region should be modest in size. The default is
c	the whole input image.
c@ clip
c	Clip level. For input images of intensity, any pixels below the
c	clip level are excluded from the fitting process. For other sorts
c	of images (e.g. Stokes Q, U or V) pixels whose absolute values are
c	below the clip level are excluded from the fit. The default is 0.
c@ object
c	This gives the component types that IMFIT fits for. Several components
c	can be given (the components can be of the same type, or different),
c	and minimum match is supported. Possible objects are
c	  level       An offset (DC) level
c	  gaussian    An elliptical or circular gaussian.
c	  disk        An elliptical or circular disk.
c	  point       This is a short-hand for a gaussian with the
c	              width of the point-spread-function.
c	  beam        This is a short-hand for a gaussian with a
c	              peak value of 1 and located at the image centre.
c	              This would typically be used when fitting a beam
c	              pattern.
c	For example, to fit for two gaussians, use:
c	`object=gaussian,gaussian'. There is no default.
c@ spar
c	This gives initial estimates of source parameters.  For
c	each object given by the `object' keyword, either 1 (for
c	the level) or 6 (for disks and gaussians) values should be
c	given. The values are as follows:
c	  Object Type             SPAR values
c	  -----------             -----------
c	   level                   offset
c	   gaussian                amp,x,y,bmaj,bmin,pa
c	   disk                    amp,x,y,bmaj,bmin,pa
c	   point                   amp,x,y
c	   beam                    bmaj,bmin,pa
c	Here "offset" is the offset level, "amp" is the peak value of
c	the object, "x" and "y" are the offset positions (in arcsec) of
c	the object relative to the reference pixel, "bmaj" and "bmin" are
c	the major and minor axes FWHM (in arcsec), and "pa" is the position
c	angle of an ellitpical component (in degrees). The position angle is
c	measured from north through east.
c	You should give initial estimates for all parameters for each object
c	(this includes parameters that might seem redundant or meaningless,
c	such as "bmin" and "pa" for components that are constrained to be
c	circular). However if (and only if) you are fitting for a single
c	object, IMFIT can derive an initial estimate for itself.
c@ fix
c	This gives a set a flag parameters, one parameter per source.
c	Each parameter consists of a set of letters, which indicate
c	which source parameters of a component are to be held fixed.
c	These source parameters are fixed by the initial estimates
c	given by the `spar' parameter.
c	The letters corresponding to each source parameter are:
c	  f   The amplitude (Flux) is fixed.
c	  x   The offset in RA is fixed.
c	  y   The offset in DEC is fixed.
c	  a   The major axis parameter is fixed.
c	  b   The minor axis parameter is fixed.
c	  p   The position angle parameter is fixed.
c	  c   The gaussian or disk is circular (not elliptical).
c	For a source where all source parameters vary, a dash (-)
c	can be used for this parameter.
c
c	For example "fix=fx,fc" indicates that the amplitude and RA offset
c	is to be fixed for the first source, whereas the second source,
c	(which is presumably a gaussian or disk) has a fixed flux, and
c	is circular.
c	The default is to assume that everything can vary.
c@ out
c	The optional output data-set. The default is not to create an
c	output data-set. If an output dataset name is given, then
c	either the model or residual image can be saved.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Minimum match is used. Possible values are:
c	  residual The output data-set is the residual visibilities.
c	           If an output is being created, the default is to make
c	           this the fitted model.
c--
c  History:
c    mchw 22apr94 new task.
c    mchw 09may94 removed redundant declarations and labels
c    rjs  24aug94 Rewrite.
c    rjs  12sep94 Added error estimates, deconvolution by beam and
c		  total flux estimates.
c    rjs  15sep94 Support object=point and object=beam.
c    rjs  26jan95 Eliminate non-standard string concatenation.
c    rjs  06apr95 Get solver to work with relative coordinates, to
c		  eliminate divergence problem.
c    rjs  01nov95 Better fiddles in gaufid.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
c
	character version*(*)
	parameter(version='version 1.0 18-Jan-96')
	integer MAXBOX,MAXVAR
	parameter(MAXBOX=1024,MAXVAR=30)
c
	character in*64,out*64
	real clip,x(MAXVAR),covar(MAXVAR*MAXVAR),rms,trms
	real bmaj,bmin,bpa,bvol,bvolp
	logical dores,inten,defsrc,doOut,dofit
	integer ifail1,ifail2,lIn,lOut
	integer Boxes(MAXBOX)
	integer blc(3),trc(3),nin(3),nout(MAXNAX),naxis,k,m,nvar,iax
	double precision dpol
c
c  Externals.
c
	logical PolsPara
	character itoaf*2
	external FUNCTION
c
c  Get the input parameters.
c
	call output('ImFit: '//version)
	call keyini
	call keya('in',in,' ')
	call BoxInput('region',in,boxes,MAXBOX)
	call LoadSrc(defsrc)
	call keyr('clip',clip,0.)
	call keya('out',out,' ')
	doOut = out.ne.' '
	call GetOpt(dores)
	call keyfin
c
c  Open the input dataset, and initialise various things.
c
	call xyopen(lIn,in,'old',3,nin)
	if(nin(1).gt.maxdim)
     *	  call bug('f','Image too big for me to handle')
	call rdhdi(lIn,'naxis',naxis,0)
	naxis = min(naxis,MAXNAX)
	call BoxSet(boxes,3,nin,' ')
	call BoxMask(lIn,boxes,MAXBOX)
	call BoxInfo(boxes,3,blc,trc)
c
c  Is this an intensity-type image?
c
	call coInit(lIn)
	call coFindAx(lIn,'stokes',iax)
	if(iax.ne.0)then
	  call coCvt1(lIn,iax,'ap',1.d0,'aw',dpol)
	  inten = PolsPara(nint(dpol))
	else
	  inten = .true.
	endif
c
c  Make the output image, if required.
c
	if(doOut)then
	  nout(1) = nin(1)
	  nout(2) = nin(2)
	  nout(3) = trc(3) - blc(3) + 1
	  do k=4,MAXNAX
	    nout(k) = 1
	  enddo
	  call xyopen(lOut,out,'new',naxis,nout)
	  call MkHead(lIn,lOut,blc(3)-1,version)
	endif
c
c  Get the theoretical rms noise.
c
	call rdhdr(lIn,'rms',trms,0.)
c
c  Loop the loop.
c
	do k=blc(3),trc(3)
c
c  Load the data.
c
	  call xysetpl(lIn,1,k)
	  if(doOut)call xysetpl(lOut,1,k-blc(3)+1)
	  call LoadDat(lIn,Boxes,k,nin(1),nin(2),clip,inten,m)
	  dofit = m.gt.0
c
c  Get the beam parameters, fill in "the appropriate values for POINT and
c  BEAM types, convert the coordinates to pixels, and fill in defaults
c  if necessary.
c
	  if(dofit)then
	    call BeamPar(lIn,k,bvol,bvolp,bmaj,bmin,bpa)
	    call ParFill(bmaj,bmin,bpa)
	    call CoordFid(lIn,k,.true.)
	    if(defsrc)call GetEst
	    defsrc = .false.
c
c  Pack the variables into an array.
c
	    call PackVar(x,nvar,MAXVAR)
	    dofit = nvar.gt.0
	    if(.not.doOut.and..not.doFit)
     *	      call bug('f','Nothing to be done -- check inputs!')
	    if(nvar.ge.m)call bug('f','Too few pixels to fit to')
	  endif
c
c  Do the fitting process.
c
	  if(dofit)then
	    call lsqfit(FUNCTION,m,nvar,x,covar,rms,ifail1,ifail2)
	    call UPackVar(x,nvar)
	    if(ifail2.eq.0)call UpackCov(covar,nvar)
	    if(ifail1.ne.0)
     *	      call bug('w','Failed to converge: ifail='//itoaf(ifail1))
	    if(ifail2.ne.ifail1)
     *	      call bug('w','Failed to determine covariance matrix')
	  else
	    call bug('w','Nothing to fit!')
	    rms = 0
	  endif
c
c  If there is an output, write it out.
c
	  if(doOut)call GenOut(lIn,lOut,nin(1),nin(2),dores)
c
c  Convert to astronomical units, and report.
c
	  call CoordFid(lIn,k,.false.)
	  if(dofit)then
	    call Report(rms,trms,bvol,bvolp,bmaj,bmin,bpa)
	  endif
	enddo
c
c  Write out the beam parameters, if needed.
c
	if(blc(3).eq.1.and.trc(3).eq.1.and.ifail1.eq.0)call BeamSet(lIn)
c
c  All said and done.
c
	call xyclose(lIn)
	if(doOut)call xyclose(lOut)
c
	end
c************************************************************************
	subroutine BeamSet(lIn)
c
	implicit none
	integer lIn
c
c  Write the beam parameters if it looks appropriate. This is if only a
c  BEAM object was fitted for, and if the dataset is writable.
c------------------------------------------------------------------------
	include 'imfit.h'
	include 'mirconst.h'
	real f1,f2,sf1,sf2,p,sp
	character mode*8
c
	if(nsrc.eq.1.and.srctype(1).eq.BEAM)then
	  call hmode(lIn,mode)
	  if(index(mode,'w').ne.0)then
	    call GauFid(fwhm1(1),fwhm2(1),0.,0.,pa(1),0.,
     *					f1,f2,sf1,sf2,p,sp)
	    call wrhdr(lIn,'bmaj',pi/180/3600*f1)
	    call wrhdr(lIn,'bmin',pi/180/3600*f2)
	    call wrhdr(lIn,'bpa',p)
	  endif
	endif
c
	end
c************************************************************************
	subroutine ParFill(bmaj,bmin,bpa)
c
	implicit none
	real bmaj,bmin,bpa
c
c  Fill in parameters for POINT and BEAM types.
c
c  Input:
c    bmaj,bmin,bpa  Beam parameters, in radians.
c------------------------------------------------------------------------
	include 'imfit.h'
	integer i
c
	do i=1,nsrc
	  if(srctype(i).eq.POINT)then
	    if(bmaj*bmin.le.0)call bug('f',
     *	'Beam parameters were not found -- object=point cannot be used')
	    fwhm1(i) = bmaj
	    fwhm2(i) = bmin
	    pa(i) = bpa
	  else if(srctype(i).eq.BEAM)then
	    flux(i) = 1
	    l0(i) = 0
	    m0(i) = 0
	  endif
	enddo
c
	end
c************************************************************************
	subroutine CoordFid(lIn,k,topix)
c
	implicit none
	integer lIn,k
	logical topix
c
c  Convert coordinates between world and pixel coordinates.
c
c  Input:
c    lIn	Handle of the coordinate system.
c    k
c    topix
c------------------------------------------------------------------------
	include 'imfit.h'
	double precision in(3),out(3)
	double precision crpix(2),crval(2),cdelt(2)
	character ctype(2)*16
	real bmaj,bmin,bpa,dx,dy
	integer i
c
	do i=1,nsrc
	  if(srctype(i).ne.LEVEL)then
c
c  Convert the position.
c
	    in(1) = l0(i)
	    in(2) = m0(i)
	    in(3) = k
	    if(topix)then
	      call coCvt(lIn,'ow/ow/ap',in,'ap/ap/ap',out)
	    else
	      call coCvt(lIn,'ap/ap/ap',in,'ow/ow/ap',out)
	    endif
	    l0(i) = out(1)
	    m0(i) = out(2)
c
c  Convert the gaussian parameters.
c
	    if(topix)then
	      call coGauCvt(lIn,'ow/ow/ap',in,
     *		  'w',fwhm1(i),fwhm2(i),pa(i),'p',bmaj,bmin,bpa)
	    else
	      call coGauCvt(lIn,'ap/ap/ap',in,
     *		  'p',fwhm1(i),fwhm2(i),pa(i),'w',bmaj,bmin,bpa)
	    endif
c
c  Convert the uncertainties.
c
	    sfwhm1(i) = bmaj / fwhm1(i) * sfwhm1(i)
	    sfwhm2(i) = bmin / fwhm2(i) * sfwhm2(i)
	    if(spa(i)+sl0(i)+sm0(i).gt.0)then
	      if(topix)then
		call coLin(lIn,'ow/ow/ap',in,2,ctype,crpix,crval,cdelt)
		dx = 1/abs(cdelt(1))
		dy = 1/abs(cdelt(2))
	      else
		call coLin(lIn,'ap/ap/ap',in,2,ctype,crpix,crval,cdelt)
		dx = abs(cdelt(1))
		dy = abs(cdelt(2))
	      endif
	      sl0(i) = sl0(i) * dx
	      sm0(i) = sm0(i) * dy
	      spa(i) = spa(i) / ( dy/dx*cos(pa(i))**2 + 
     *				  dx/dy*sin(pa(i))**2 )
	    endif
c
	    fwhm1(i) = bmaj
	    fwhm2(i) = bmin
	    pa(i)    = bpa
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GenOut(lIn,lOut,n1,n2,dores)
c
	implicit none
	integer lIn,lOut,n1,n2
	logical dores
c
c  Generate the output model or residuals.
c
c  Input:
c    lIn	Handle of the input dataset.
c    lOut	Handle of the output dataset.
c    n1,n2	Size of the input and output images.
c    dores	True if we are to write the residuals.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	real data(MAXDIM),model(MAXDIM)
	integer x(MAXDIM),y(MAXDIM)
	logical domask,mask(MAXDIM)
c
c  Externals.
c
	logical hdprsnt
c
	domask = hdprsnt(lIn,'mask')
c
	do i=1,n1
	  x(i) = i
	enddo
c
	do j=1,n2
	  do i=1,n1
	    y(i) = j
	  enddo
	  call Eval(x,y,model,n1)
	  if(dores)then
	    call xyread(lIn,j,data)
	    do i=1,n1
	      data(i) = data(i) - model(i)
	    enddo
	    call xywrite(lOut,j,data)
	    if(domask)then
	      call xyflgrd(lIn,j,mask)
	      call xyflgwr(lOut,j,mask)
	    endif
	  else
	    call xywrite(lOut,j,model)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GetEst
	implicit none
c
c  Generate an initial estimate for a single component model.
c
c------------------------------------------------------------------------
	include 'imfit.h'
	include 'mirconst.h'
c
	integer i
	double precision P,XP,YP,XYP,XXP,YYP,SP
	real t,fac
c
	SP = 0
	P = 0
	XP = 0
	YP = 0
	XYP = 0
	XXP = 0
	YYP = 0
c
	do i=1,ndata
	  SP  = SP + data(i)
	  t = abs(data(i))
	  P   = P   + t
	  XP  = XP  + t*x(i)
	  YP  = YP  + t*y(i)
	  XYP = XYP + t*x(i)*y(i)
	  XXP = XXP + t*x(i)*x(i)
	  YYP = YYP + t*y(i)*y(i)
	enddo
c
	if(srctype(1).eq.LEVEL)then
	  flux(1) = P / ndata
	else
	  fac = 4*log(2.)
	  XP  = XP / P
	  YP  = YP / P
	  XYP = XYP / P - XP*YP
	  XXP = XXP / P - XP*XP
	  YYP = YYP / P - YP*YP
	  if(srctype(1).ne.BEAM)then
	    l0(1) = XP
	    m0(1) = YP
	  endif
	  if(srctype(1).ne.POINT)then
	    fwhm1(1) = sqrt(fac*(XXP + YYP +
     *		sqrt( (XXP-YYP)**2 + 4*(XYP)**2 )))
	    fwhm2(1) = sqrt(fac*(XXP + YYP -
     *		sqrt( (XXP-YYP)**2 + 4*(XYP)**2 )))
	    if(circ(1))then
	      fwhm1(1) = sqrt(fwhm1(1)*fwhm2(1))
	      fwhm2(1) = fwhm1(1)
	      pa(1) = 0
	    else
	      pa(1) = 0.5*atan2(2*XYP,YYP-XXP)
	    endif
	  endif
	  if(srctype(1).ne.BEAM)then
	    flux(1) = sign(fac * P / ( pi * fwhm1(1) * fwhm2(1) ),SP)
	  endif
	endif
c
	end
c************************************************************************
	subroutine MkHead(lIn,lOut,off,version)
c
	implicit none
	integer lIn,lOut,off
	character version*(*)
c
c  Make the header of the output image.
c------------------------------------------------------------------------
	integer i
	double precision crpix3
	character umsg*64
	integer nkeys
	parameter(nkeys=43)
	character keyw(nkeys)*8
c
c  Externals.
c
	logical hdprsnt
c
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *	  'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *	  'crpix1  ','crpix2  ',           'crpix4  ','crpix5  ',
     *	  'date-obs','epoch   ','history ','instrume','niters  ',
     *	  'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *	  'restfreq','telescop','vobs    ','xshift  ','yshift  ',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ','btype   '/
c
	do i=1,nkeys
	  call hdcopy(lIn,lOut,keyw(i))
	enddo
c
	if(hdprsnt(lIn,'crpix3'))then
	  call rdhdd(lIn,'crpix3',crpix3,0.d0)
	  crpix3 = crpix3 - off
	  call wrhdd(lOut,'crpix3',crpix3)
	endif
	call hisopen(lOut,'append')
	umsg = 'IMFIT: Miriad '//version
        call hiswrite (lOut, umsg)
	call hisinput(lOut,'IMFIT')
	call hisclose(lOut)
	end
c************************************************************************
	subroutine LoadDat(lIn,Boxes,k,n1,n2,clip,inten,m)
c
	implicit none
	integer lIn,Boxes(*),m,k,n1,n2
	real clip
	logical inten
c
c  Load the relevant data for this plane.
c
c  Input:
c    lIn
c    Boxes
c    inten	Is "clip" an upper or absolute threshold.
c    clip	Clip level.
c  Output:
c    m		Number of points loaded.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXRUNS
	parameter(MAXRUNS=3*MAXDIM)
	include 'imfit.h'
	integer Runs(3,MAXRUNS),nRuns,xmin,xmax,ymin,ymax
	integer iRun,n,n0,ipt,i,xt,yt
c
	call BoxRuns(1,k,' ',boxes,Runs,MAXRUNS,nRuns,
     *					xmin,xmax,ymin,ymax)
	call GetPlane(lIn,Runs,nRuns,0,0,n1,n2,Data,MAXDATA,m)
	if(m.eq.0)return
c
c  We have the data. Clip, if required, and fill out the x,y coordinate.
c
	iRun = 0
	n0 = 0
	n = 0
	ipt = 0
	xt = 0
	yt = 0
	do i=1,m
	  if(n.eq.n0)then
	    iRun = iRun + 1
	    n0 = Runs(3,iRun)-Runs(2,iRun)+1
	    n = 0
	  endif
c
	  if((Data(i).gt.clip).or.(.not.inten.and.Data(i).lt.-clip))then
	    ipt = ipt + 1
	    Data(ipt) = Data(i)
	    x(ipt) = Runs(2,iRun) + n
	    y(ipt) = Runs(1,iRun)
	    xt = xt + x(ipt)
	    yt = yt + y(ipt)
	  endif
c
	  n = n + 1
	enddo
c
	m = ipt
	ndata = ipt
	xoff = xt / real(ndata)
	yoff = yt / real(ndata)
c
	end
c************************************************************************
	subroutine PackVar(var,nvar,MAXVAR)
c
	implicit none
	integer nvar,MAXVAR
	real var(MAXVAR)
c
c  Store all the things that we need to vary.
c
c------------------------------------------------------------------------
	include 'imfit.h'
	integer i,j,ncurr
	real tmp(6)
c
	nvar = 0
	do i=1,nsrc
	  ncurr = 0
	  if(vflux(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = flux(i)
	  endif
	  if(vl0(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = l0(i) - xoff
	  endif
	  if(vm0(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = m0(i) - yoff
	  endif
	  if(vfwhm1(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = fwhm1(i)
	  endif
	  if(vfwhm2(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = fwhm2(i)
	  endif
	  if(vpa(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = pa(i)
	  endif
c
c  Copy the estimates to the variables.
c
	  if(nvar+ncurr.gt.MAXVAR)
     *	  call bug('f','Too many free parameters')
	  do j=1,ncurr
	    nvar = nvar + 1
	    var(nvar) = tmp(j)
	  enddo
c
	enddo
c
	end
c************************************************************************
	subroutine UpackCov(covar,nvar)
c
	implicit none
	integer nvar
	real covar(nvar,nvar)
c
c  Unpack the covariance matrix.
c------------------------------------------------------------------------
	include 'imfit.h'
	integer i,n
c
	n = 0
c
	do i=1,nsrc
	  if(vflux(i))then
	    n = n + 1
	    sflux(i) = covar(n,n)
	  endif
	  if(vl0(i))then
	    n = n + 1
	    sl0(i) = covar(n,n)
	  endif
	  if(vm0(i))then
	    n = n + 1
	    sm0(i) = covar(n,n)
	  endif
	  if(vfwhm1(i))then
	    n = n + 1
	    sfwhm1(i) = covar(n,n)
	    if(circ(i))sfwhm2(i) = sfwhm1(i)
	  endif
	  if(vfwhm2(i))then
	    n = n + 1
	    sfwhm2(i) = covar(n,n)
	  endif
	  if(vpa(i))then
	    n = n + 1
	    spa(i) = covar(n,n)
	  endif
	enddo
c
	if(n.ne.nvar)
     *	  call bug('f','Inconsistency in UPackCov')
c
	end
c************************************************************************
	subroutine UPackVar(var,nvar)
c
	implicit none
	integer nvar
	real var(nvar)
c
c  Unpack all the things that we need to vary.
c
c------------------------------------------------------------------------
	include 'imfit.h'
	integer i,n
c
	n = 0
c
	do i=1,nsrc
	  if(vflux(i))then
	    n = n + 1
	    flux(i) = var(n)
	  endif
	  if(vl0(i))then
	    n = n + 1
	    l0(i) = var(n) + xoff
	  endif
	  if(vm0(i))then
	    n = n + 1
	    m0(i) = var(n) + yoff
	  endif
	  if(vfwhm1(i))then
	    n = n + 1
	    fwhm1(i) = var(n)
	    if(circ(i))fwhm2(i) = fwhm1(i)
	  endif
	  if(vfwhm2(i))then
	    n = n + 1
	    fwhm2(i) = var(n)
	  endif
	  if(vpa(i))then
	    n = n + 1
	    pa(i) = var(n)
	  endif
	enddo
c
	if(n.ne.nvar)
     *	  call bug('f','Inconsistency in UPackVar')
c
	end
c************************************************************************
	subroutine GetOpt(dores)
c
	implicit none
	logical dores
c
c  Get extra processing options.
c
c  Output:
c    dores
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=1)
	character opts(nopts)*8
	logical present(nopts)
	data opts/'residual '/
c
	call options('options',opts,present,nopts)
c
	dores = present(1)
	end
c************************************************************************
	subroutine FUNCTION(m,nvar,var,fvec,iflag)
c
	implicit none
	integer m,nvar,iflag
	real var(nvar)
	real fvec(m)
c
c------------------------------------------------------------------------
	include 'imfit.h'
	integer i
c
	if(m.ne.ndata)call bug('f','Inconsistency in FUNCTION')
c
c  Unpack the things that we are solving for.
c
	call UpackVar(var,nvar)
c
c  Evaluate the model.
c
	call Eval(x,y,fvec,m)
c
c  Compute the residuals now.
c
	do i=1,m
	  fvec(i) = Data(i) - fvec(i)
	enddo
c
	end	
c************************************************************************
	subroutine Eval(x0,y0,Model,n)
c
	implicit none
	integer n,x0(n),y0(n)
	real Model(n)
c
c  Evaluate the current model at some pixels.
c
c  Input:
c    n		Number of points.
c    x0,y0	Pixel coordinates at which to evaluate the model.
c  Output:
c    model	The evaluated model.
c------------------------------------------------------------------------
	include 'imfit.h'
	integer i,j
	real cospa,sinpa,t,xx,yy,xp,yp,xscal,yscal
c
c  Set the model to zero initially.
c
	do i=1,n
	  model(i) = 0
	enddo
c
c  Loop over the various model types.
c
	do j=1,nsrc
c
c  Level component.
c
	  if(srctype(j).eq.LEVEL)then
	    do i=1,n
	      model(i) = model(i) + flux(j)
	    enddo
c
c  Gaussian component.
c
	  else if(srctype(j).eq.GAUSSIAN.or.srctype(j).eq.POINT.or.
     *		  srctype(j).eq.BEAM)then
	    cospa = cos(pa(j))
	    sinpa = sin(pa(j))
	    xscal = 4*log(2.)/(fwhm2(j)*fwhm2(j))
	    yscal = 4*log(2.)/(fwhm1(j)*fwhm1(j))
	    do i=1,n
	      xx = x0(i) - l0(j)
	      yy = y0(i) - m0(j)
	      yp =  yy*cospa + xx*sinpa
	      xp = -yy*sinpa + xx*cospa
	      t = xscal*(xp*xp) + yscal*(yp*yp)
	      if(t.lt.70)model(i) = model(i) + flux(j) * exp(-t)
	    enddo
c
c  Disk component.
c
	  else if(srctype(j).eq.DISK)then
	    cospa = cos(pa(j))
	    sinpa = sin(pa(j))
	    xscal = 1./(fwhm2(j)*fwhm2(j))
	    yscal = 1./(fwhm1(j)*fwhm1(j))
	    do i=1,n
	      xx = x0(i) - l0(j)
	      yy = y0(i) - m0(j)
	      yp =  yy*cospa + xx*sinpa
	      xp = -yy*sinpa + xx*cospa
	      t = xscal*(xp*xp) + yscal*(yp*yp)
	      if(t.lt.0.25)model(i) = model(i) + flux(j)
	    enddo
	  endif
	enddo
c
	end
c************************************************************************
	subroutine Report(rms,trms,bvol,bvolp,bmaj,bmin,bpa)
c
	implicit none
	real rms,trms,bvol,bvolp,bmaj,bmin,bpa
c
c  Report on the source component solution.
c
c  Input:
c    rms	RMS residual error after the fit.
c    trms	Theoretical rms noise in the image.
c    bvol	Beam volume, in radians**2.
c    bvol	Beam volume, in pixels.
c    bmaj	Beam major axis (radians).
c    bmin	Beam minor axis (radians).
c    bpa	Beam position angle (radians).
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'imfit.h'
c
	integer i,ifail
	real f1,f2,p,sf1,sf2,sp,tflux,sfac,fac
	character line*80
c
	integer NOBJS
	parameter(NOBJS=5)
	character objects(NOBJS)*8
c
	data objects(DISK)    /'disk    '/
	data objects(GAUSSIAN)/'gaussian'/
	data objects(LEVEL)   /'level   '/
	data objects(POINT)   /'point   '/
	data objects(BEAM)    /'beam    '/
c
	call output('-------------------------------------------------')
c
	if(trms.gt.0)then
	  write(line,5)rms,trms
	else
	  write(line,5)rms
	endif
   5	  format('RMS residual is',1pe9.2,:,
     *		' (theoretical image noise is',1pe9.2,')')
	call output(line)
c
c  Tell the user what gaussian is being used.
c
	if(bmaj*bmin.gt.0)then
	  call output(' ')
	  call GauFid(bmaj,bmin,0.,0.,bpa,0.,f1,f2,sf1,sf2,p,sp)
	  call output('Using the following beam parameters when')
	  call output('deconvolving and converting to integrated flux')
	  write(line,7)f1,f2
	  call output(line)
	  write(line,8)p
	  call output(line)
   7	  format('  Beam Major, minor axes (arcsec):',2f9.2)
   8	  format('  Beam Position angle (degrees):',   f10.1)
	endif
c
c  Tell the user about the noise normalisation factor.
c
	call output(' ')
	if(bvolp.eq.1)then
	  sfac = 1
	  call output(
     *		'Theoretical noise is uncorrelated between pixels')
	else if(bvolp.gt.0)then
	  sfac = sqrt(bvolp)
	  write(line,9)sfac
   9	  format('Scaling error estimates by',f5.1,' to account for')
	  call output(line)
	  call output('noise correlation between pixels')
	else
	  call output('Beam volume is not known. The error estimates')
	  call output(
     *	    'assume that the noise is uncorrelated between pixels.')
	  sfac = 1
	endif
c
	do i=1,nsrc
	  call output(' ')
	  write(line,10)i,objects(srctype(i))
  10	  format('Source',i3,', Object type: ',a)
	  call output(line)
	  if(srctype(i).eq.LEVEL)then
	    if(sflux(i).gt.0)then
	      write(line,20)flux(i),sfac*sflux(i)
	    else
	      write(line,20)flux(i)
	    endif
  20	    format('  Offset Level:',f26.4,:,' +/-',1pg9.2)
	    call output(line)
	  else
	    if(srctype(i).ne.BEAM)then
	      if(sflux(i).gt.0)then
	        write(line,30)flux(i),sfac*sflux(i)
	      else
	        write(line,30)flux(i)
	      endif
  30	      format('  Peak value:',f27.4,:,' +/-',f8.4)
	      call output(line)
	      if(bvol.gt.0.and.srctype(i).ne.POINT)then
	        tflux = flux(i) * pi/4 * fwhm1(i) * fwhm2(i)
	        if(srctype(i).eq.GAUSSIAN) tflux = tflux / log(2.0)
	        tflux = tflux / bvol
	        write(line,35)tflux
  35	        format('  Total integrated flux:',f16.4)
	        call output(line)
	      endif
	      write(line,40)3600*180/pi*l0(i),3600*180/pi*m0(i)
  40	      format('  Offset Position (arcsec):  ',2f9.2)
	      call output(line)
	      if(sl0(i)+sm0(i).gt.0)then
	        write(line,45)3600*180/pi*sfac*sl0(i),
     *			    3600*180/pi*sfac*sm0(i)
  45	        format('  Positional errors (arcsec):',2f9.2)
	        call output(line)
	      endif
	    endif
	    if(srctype(i).ne.POINT)then
	      call GauFid(fwhm1(i),fwhm2(i),sfac*sfwhm1(i),
     *		sfac*sfwhm2(i),pa(i),sfac*spa(i),f1,f2,sf1,sf2,p,sp)
	      if(sf1.gt.0)then
	        write(line,50)'Major',f1,sf1
	      else
	        write(line,50)'Major',f1
	      endif
	      call output(line)
	      if(sf1.gt.0)then
	        write(line,50)'Minor',f2,sf2
	      else
	        write(line,50)'Minor',f2
	      endif
	      call output(line)
  50	      format('  ',a,' axis (arcsec):',f16.2,:,' +/-',f7.2)
	      if(sp.gt.0)then
	        write(line,60)p,sp
	      else
	      write(line,60)p
	      endif
  60	      format('  Position angle (degrees):',f11.2,:,' +/-',f11.2)
	      call output(line)
c
c  Deconvolve the beam, if possible.
c
	      if(bmaj*bmin.gt.0)then
	        call GauDFac(fwhm1(i),fwhm2(i),180/pi*pa(i),
     *		  bmaj,bmin,180/pi*bpa,fac,f1,f2,p,ifail)
	        if(ifail.eq.0)then
		  p = pi/180. * p
		  call GauFid(f1,f2,0.,0.,p,0.,f1,f2,sf1,sf2,p,sp)
	          write(line,70)f1,f2
  70	          format('  Deconvolved Major, minor axes (arcsec):',
     *								 2f9.2)
	          call output(line)
	          write(line,80)p
  80	          format('  Deconvolved Position angle (degrees):',
     *								 f10.1)
	          call output(line)
	        else if(ifail.eq.1)then
		  call output(
     *		   '  Deconvolution appears to produce a point source')
	        else
		  call bug('w','Failed in attempting to deconvolve')
	        endif
	      endif
	    endif
	  endif
	enddo
c
	call output('-------------------------------------------------')
c
	end
c************************************************************************
	subroutine GauFid(fwhm1,fwhm2,sfwhm1,sfwhm2,pa,spa,f1,f2,
     *							sf1,sf2,p,sp)
c
	implicit none
	real fwhm1,fwhm2,pa,f1,f2,p,sfwhm1,sfwhm2,spa,sf1,sf2,sp
c
c  Convert the gaussian parameters to arcsec.
c------------------------------------------------------------------------
	include 'mirconst.h'
	real t
c
	f1 = 3600*180/pi * abs(fwhm1)
	f2 = 3600*180/pi * abs(fwhm2)
	sf1 = 3600*180/pi * sfwhm1
	sf2 = 3600*180/pi * sfwhm2
	p = 180./pi * pa
	sp = 180./pi * spa
	if(f1.lt.f2)then
	  t = f1
	  f1 = f2
	  f2 = t
	  t = sf1
	  sf1 = sf2
	  sf2 = t
	  p = p + 90
	endif
	p = mod(p,180.)
	if(p.lt.-90) p = p + 180
	if(p.gt. 90) p = p - 180
	end
c************************************************************************
	subroutine LoadSrc(defsrc)
c
	implicit none
	logical defsrc
c
c  Load the source components and their initial estimates.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'imfit.h'
	integer nout,i
	character object*16,fix*16
c
	integer NOBJS
	parameter(NOBJS=5)
	character objects(NOBJS)*8
	integer objtype(NOBJS)
c
c  Externals.
c
	logical keyprsnt
	integer binsrcha
c
c  NOTE: The following list of object names MUST be in alphabetic
c  order.
c
	data objects/'beam    ','disk    ','gaussian',
     *				'level   ','point   '/
	data objtype/ BEAM,      DISK,      GAUSSIAN,
     *				 LEVEL,     POINT/
c
	nsrc = 0
	dowhile(keyprsnt('object'))
	  nsrc = nsrc + 1
	  if(nsrc.gt.MAXSRC)call bug('f','Too manu sources')
	  call keymatch('object',NOBJS,objects,1,object,nout)
	  i = binsrcha(object,objects,NOBJS)
	  srctype(nsrc) = objtype(i)
c
c  Get the source parameters.
c
	  vflux(nsrc) = .true.
	  vl0(nsrc) = .true.
	  vm0(nsrc) = .true.
	  vfwhm1(nsrc) = .true.
	  vfwhm2(nsrc) = .true.
	  vpa(nsrc) = .true.
c
	  defsrc = .not.keyprsnt('spar')
	  if(srctype(nsrc).eq.LEVEL)then
	    call keyr('spar',flux(nsrc),0.)
	    vl0(nsrc) = .false.
	    vm0(nsrc) = .false.
	    vfwhm1(nsrc) = .false.
	    vfwhm2(nsrc) = .false.
	    vpa(nsrc) = .false.
	  else if(srctype(nsrc).eq.POINT)then
	    call keyr('spar',flux(nsrc),0.)
	    call keyr('spar',l0(nsrc),0.)
	    call keyr('spar',m0(nsrc),0.)
	    l0(nsrc)    = l0(nsrc) * pi/180./3600.
	    m0(nsrc)    = m0(nsrc) * pi/180./3600.
	    vfwhm1(nsrc) = .false.
	    vfwhm2(nsrc) = .false.
	    vpa(nsrc) = .false.
	  else if(srctype(nsrc).eq.BEAM)then
	    call keyr('spar',fwhm1(nsrc),1.)
	    call keyr('spar',fwhm2(nsrc),1.)
	    call keyr('spar',pa(nsrc),0.)
	    if(min(fwhm1(nsrc),fwhm2(nsrc)).le.0)
     *	      call bug('f','Invalid FWHM parameters given')
	    fwhm1(nsrc) = fwhm1(nsrc) * pi/180./3600.
	    fwhm2(nsrc) = fwhm2(nsrc) * pi/180./3600.
	    pa(nsrc)    = pa(nsrc) * pi/180.
	    vflux(nsrc) = .false.
	    vl0(nsrc) = .false.
	    vm0(nsrc) = .false.
	  else if(srctype(nsrc).eq.DISK.or.
     *		  srctype(nsrc).eq.GAUSSIAN)then
	    call keyr('spar',flux(nsrc),0.)
	    call keyr('spar',l0(nsrc),0.)
	    call keyr('spar',m0(nsrc),0.)
	    call keyr('spar',fwhm1(nsrc),1.)
	    call keyr('spar',fwhm2(nsrc),1.)
	    if(min(fwhm1(nsrc),fwhm2(nsrc)).le.0)
     *	      call bug('f','Invalid FWHM parameters given')
	    call keyr('spar',pa(nsrc),0.)
c
	    l0(nsrc)    = l0(nsrc) * pi/180./3600.
	    m0(nsrc)    = m0(nsrc) * pi/180./3600.
	    fwhm1(nsrc) = fwhm1(nsrc) * pi/180./3600.
	    fwhm2(nsrc) = fwhm2(nsrc) * pi/180./3600.
	    pa(nsrc)    = pa(nsrc) * pi/180.
	  endif
c
c  Determine what is fixed and what is variable.
c
	  call keya('fix',fix,'-')
	  call lcase(fix)
	  vflux(nsrc) = index(fix,'f').eq.0.and.vflux(nsrc)
	  vl0(nsrc)   = index(fix,'x').eq.0.and.vl0(nsrc)
	  vm0(nsrc)   = index(fix,'y').eq.0.and.vm0(nsrc)
	  vfwhm1(nsrc)= index(fix,'a').eq.0.and.vfwhm1(nsrc)
	  circ(nsrc)  = index(fix,'c').ne.0
	  if(circ(nsrc))then
	    vfwhm2(nsrc) = .false.
	    vpa(nsrc)    = .false.
	    fwhm2(nsrc)  = fwhm1(nsrc)
	    pa(nsrc)     = 0
	  else
	    vfwhm2(nsrc) = index(fix,'b').eq.0.and.vfwhm1(nsrc)
	    vpa(nsrc)    = index(fix,'p').eq.0.and.vpa(nsrc)
	  endif
	enddo
c
	if(nsrc.eq.0)call bug('f','The object keyword must be set')
	if(defsrc.and.nsrc.gt.1)call bug('f',
     *	  'Can only estimate initial source for 1 component only')
c
	end
c************************************************************************
	subroutine BeamPar(lIn,k,bvol,bvolp,bmaj,bmin,bpa)
c
	implicit none
	integer lIn,k
	real bvol,bvolp,bmaj,bmin,bpa
c
c  Get things dealing with units.
c
c  Input:
c    lIn	Handle of the input dataset.
c    k		Plane of interest.
c  Output:
c    bvol	Beam volume, in radians**2. Set to zero if this cannot
c		be determined.
c    bvolp	Beam volume in pixels.
c    bmaj,bmin,bpa Beam major, minor axes and position angle.
c------------------------------------------------------------------------
	include 'mirconst.h'
	character bunit*16,ctype(2)*16
	real bmajp,bminp,bpap
	double precision crpix(2),crval(2),cdelt(2),x(3)
c
c  Convert the beam to radians for the current plane.
c
	call rdhdr(lIn,'bmaj',bmaj,0.)
	call rdhdr(lIn,'bmin',bmin,0.)
	call rdhdr(lIn,'bpa',bpa,0.)
	bpa = pi/180 * bpa
c
	if(bmaj*bmin.gt.0)then
	  x(1) = 0
	  x(2) = 0
	  x(3) = 0
	  call coGauCvt(lIn,'op/op/op',x,'w',bmaj,bmin,bpa,
     *					 'p',bmajp,bminp,bpap)
	  x(3) = k
	  call coGauCvt(lIn,'op/op/ap',x,'p',bmajp,bminp,bpap,
     *					 'w',bmaj,bmin,bpa)
	endif
c
c  Determine the beam value, in radians**2
c
	call rdhda(lIn,'bunit',bunit,' ')
	call ucase(bunit)
	if(index(bunit,'/PIXEL').ne.0)then
	  x(1) = 0
	  x(2) = 0
	  x(3) = k
	  call coLin(lIn,'op/op/ap',x,2,ctype,crpix,crval,cdelt)
	  bvol = abs(cdelt(1)*cdelt(2))
	  bvolp = 1
	else if(index(bunit,'/BEAM').ne.0.and.bmaj*bmin.gt.0)then
	  bvol = pi/4/log(2.0)*bmaj*bmin
	  bvolp = pi/4/log(2.0)*bmajp*bminp
	else
	  bvol = 0
	  bvolp = 0
	endif
c
	end
