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
c	Here "offset" is the offset level, "amp" os the peak value of
c	the object, "x" and "y: are the offset positions (in arcsec) of
c	the object relative to the reference pixel, "bmaj" and "bmin" are
c	the major and minor axes FWHM (in arcsec), and "pa" is the position
c	angle of an ellitpical component. The position angle is measured
c	from north through east.
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
c	either the model or residual visibilities can be saved.
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
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
c
	character version*(*)
	parameter(version='version 1.0 24-Aug-94')
	integer MAXBOX,MAXVAR
	parameter(MAXBOX=1024,MAXVAR=20)
	real tol,epsfcn
	parameter(tol=1e-6,epsfcn=1e-3)
c
	character in*64,out*64
	real clip,x(MAXVAR),rms
	logical dores,inten,defsrc,doOut,dofit
	integer iwa(MAXVAR),fvec,wa,lwa,ifail,lIn,lOut
	integer Boxes(MAXBOX)
	integer blc(3),trc(3),nin(3),nout(MAXNAX),naxis,k,m,nvar,iax
	double precision dpol
c
c  Externals.
c
	logical PolsPara
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
c  Generate the initial estimate.
c
	  if(dofit)then
	    if(defsrc)then
	      call GetEst
	      defsrc = .false.
	    else
	      call CoordFid(lIn,k,.true.)
	    endif
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
	    lwa = m*nvar + 5*nvar + m
	    call memalloc(wa,lwa,'r')
	    call memalloc(fvec,m,'r')
	    call lmdiff(FUNCTION,m,nvar,x,memr(fvec),epsfcn,tol,
     *	      ifail,iwa,memr(wa),lwa)
	    if(ifail.eq.0)then
	      ifail = 1
	    else if(ifail.ge.1.and.ifail.le.3)then
	      ifail = 0
	    endif
c
	    call UPackVar(x,nvar)
	    call GetRMS(m,memr(fvec),rms)
	    call memfree(fvec,m,'r')
	    call memfree(wa,lwa,'r')
	  endif
c
c  If there is an output, write it out.
c
	  if(doOut)call GenOut(lIn,lOut,nin(1),nin(2),dores)
c
c  Convert to astronomical units, and report.
c
	  call CoordFid(lIn,k,.false.)
	  if(dofit)call Report(rms,ifail)
	enddo
c
c  All said and done.
c
	call xyclose(lIn)
	if(doOut)call xyclose(lOut)
c
	end
c************************************************************************
	subroutine GetRMS(m,fvec,rms)
c
	implicit none
	integer m
	real fvec(m),rms
c
c  Determine the rms residual.
c------------------------------------------------------------------------
	include 'imfit.h'
	double precision dtemp
	integer i
c
	if(m.ne.ndata)call bug('f','Inconsistency in GetRMS')
c
c  Evaluate the model at the appropriate pixels.
c
	call Eval(x,y,fvec,m)
c
c  Accumulate the rms.
c
	dtemp = 0
	do i=1,m
	  dtemp = dtemp + (data(i)-fvec(i))**2
	enddo
c
	rms = sqrt(dtemp / m)
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
	real bmaj,bmin,bpa
	integer i
c
	do i=1,nsrc
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN)then
	    in(1) = l0(i)
	    in(2) = m0(i)
	    in(3) = k
	    if(topix)then
	      call coCvt(lIn,'ow/ow/ap',in,'ap/ap/ap',out)
	      call coGauCvt(lIn,'ow/ow/ap',in,
     *		'w',fwhm1(i),fwhm2(i),pa(i),'p',bmaj,bmin,bpa)
	    else
	      call coCvt(lIn,'ap/ap/ap',in,'ow/ow/ap',out)
	      call coGauCvt(lIn,'ap/ap/ap',in,
     *		'p',fwhm1(i),fwhm2(i),pa(i),'w',bmaj,bmin,bpa)
	    endif
	    l0(i) = out(1)
	    m0(i) = out(2)
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
	  XP  = XP / P
	  YP  = YP / P
	  XYP = XYP / P - XP*YP
	  XXP = XXP / P - XP*XP
	  YYP = YYP / P - YP*YP
	  l0(1) = XP
	  m0(1) = YP
	  fac = 4*log(2.)
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
	  flux(1) = sign(fac * P / ( pi * fwhm1(1) * fwhm2(1) ),SP)
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
        call hiswrite (lOut, 'IMFIT: Miriad '//version)
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
	integer iRun,n,n0,ipt,i
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
	  endif
c
	  n = n + 1
	enddo
c
	m = ipt
	ndata = ipt
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
	    tmp(ncurr) = l0(i)
	  endif
	  if(vm0(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = m0(i)
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
	    l0(i) = var(n)
	  endif
	  if(vm0(i))then
	    n = n + 1
	    m0(i) = var(n)
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
	  else if(srctype(j).eq.GAUSSIAN)then
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
	subroutine Report(rms,ifail)
c
	implicit none
	real rms
	integer ifail
c
c  Report on the source component solution.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'imfit.h'
c
	integer i
	real f1,f2,p,t
	character line*80
c
	integer NOBJS
	parameter(NOBJS=3)
	character objects(NOBJS)*8
c
c  Externals.
c
	character itoaf*3
c
	data objects(DISK)    /'disk    '/
	data objects(GAUSSIAN)/'gaussian'/
	data objects(LEVEL)   /'level   '/
c
	call output('-------------------------------------------------')
c
	if(ifail.ne.0)
     *	  call bug('w','Failed to converge: ifail='//itoaf(ifail))
	write(line,5)rms
   5	format('RMS residual is',1pe10.3)
	call output(line)
c
	do i=1,nsrc
	  call output(' ')
	  write(line,10)i,objects(srctype(i))
  10	  format('Source',i3,', Object type: ',a)
	  call output(line)
	  if(srctype(i).eq.LEVEL)then
	    write(line,20)flux(i)
  20	    format('  Offset Level:',1pg26.4)
	    call output(line)
	  else
	    write(line,30)flux(i)
  30	    format('  Peak value:  ',f25.4)
	    call output(line)
	    write(line,40)3600*180/pi*l0(i),3600*180/pi*m0(i)
  40	    format('  Offset Position (arcsec):  ',2f9.2)
	    call output(line)
	    f1 = 3600*180/pi * fwhm1(i)
	    f2 = 3600*180/pi * fwhm2(i)
	    p = 180./pi * pa(i)
	    if(f1.lt.f2)then
	      t = f1
	      f1 = f2
	      f2 = t
	      p = p + 90
	    endif
	    p = mod(p,180.)
	    if(p.lt.-90) p = p + 180
	    if(p.gt. 90) p = p - 180
c
	    write(line,50)f1,f2
  50	    format('  Major, minor axes (arcsec):',2f9.2)
	    call output(line)
	    write(line,60)p
  60	    format('  Position angle (degrees):',   f10.1)
	    call output(line)
	  endif
	enddo
c
	call output('-------------------------------------------------')
c
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
	parameter(NOBJS=3)
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
	data objects/'disk    ','gaussian','level   '/
	data objtype/ DISK,	 GAUSSIAN,  LEVEL/
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
	  defsrc = .not.keyprsnt('spar')
	  call keyr('spar',flux(nsrc),0.)
	  if(srctype(nsrc).eq.DISK.or.srctype(nsrc).eq.GAUSSIAN)then
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
	  vflux(nsrc) = index(fix,'f').eq.0
	  if(srctype(nsrc).eq.GAUSSIAN.or.srctype(nsrc).eq.DISK)then
	    vl0(nsrc)   = index(fix,'x').eq.0
	    vm0(nsrc)   = index(fix,'y').eq.0
	    vfwhm1(nsrc)= index(fix,'a').eq.0
	    circ(nsrc)  = index(fix,'c').ne.0
	    if(circ(nsrc))then
	      vfwhm2(nsrc) = .false.
	      vpa(nsrc)    = .false.
	      fwhm2(nsrc)  = fwhm1(nsrc)
	      pa(nsrc)     = 0
	    else
	      vfwhm2(nsrc) = index(fix,'b').eq.0
	      vpa(nsrc)    = index(fix,'p').eq.0
	    endif
	  else
	    vl0(nsrc) = .false.
	    vm0(nsrc) = .false.
	    vfwhm1(nsrc) = .false.
	    vfwhm2(nsrc) = .false.
	    vpa(nsrc) = .false.
	    circ(nsrc) = .false.
	  endif
	enddo
c
	if(nsrc.eq.0)call bug('f','The object keyword must be set')
	if(defsrc.and.nsrc.gt.1)call bug('f',
     *	  'Can only estimate initial source for 1 component only')
c
	end
