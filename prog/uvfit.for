c************************************************************************
	program uvfit
c
c= uvfit - Fit point sources to a given vis file.
c& rjs
c: uv analysis
c	UVFIT is a Miriad task which fits model components to a visibility
c	dataset. Optionally the model or residual visibilities can
c	be written out.
c@ vis
c	Name of the input visibility file. No default.
c@ stokes
c	Normal Stokes/polarisation parameter (e.g. i,q,u,v,ii etc).
c	Only a single polarisation can be requested. The default is
c	`ii' (i.e. Stokes-I for an unpolarised source).
c@ line
c	Normal line-type processing with normal defaults. However, you
c	must select only a single channel!!
c@ select
c	Normal data selection. Default is all cross-correlation data.
c@ object
c	This gives the object type that uvfit fits for. Several objects
c	can be given (the objects can be of the same type, or different),
c	and minimum match is supported. Possible objects are
c	  point       A point source
c	  disk        An elliptical or circular disk.
c	  gaussian    An elliptical or circular gaussian.
c	For example, to fit for a point source and gaussian, use:
c	`object=point,gaussian'.
c@ spar
c	This gives initial estimates of source parameters.  For
c	each object given by the `object' keyword, either 3 (for
c	point sources) or 6 (for disks and gaussians) values should be
c	given.
c
c	The 3 parameters for point sources are:
c	  flux,offset_ra,offset_dec
c
c	The 6 parameters for gaussians and disks are: 
c	  flux,offset_ra,offset_dec,bmaj,bmin,pa
c
c	Here `flux' is the total flux of the component,
c	offset_ra and offset_dec are the offset (in arcsec) of the
c	component, bmaj and bmin are the disk or gaussian FWHM (major
c	and minor axes), in arcsec, and pa is the position angle
c	(measured from North towards East) of an elliptical gaussian
c	or disk.
c
c	For circular gaussians and disks, the bmin and bpa arguments
c	must still be present.
c
c	The more complex the set of objects being fitted for, the more
c	important it is to give a good estimate of the source parameters.
c	Generally the estimates of the source position should be accurate
c	to the fundamental resolution (for point sources) or the size of
c	the component (for extended sources).
c@ fix
c	This gives a set a flag parameters, one parameter per source.
c	Each parameter consists of a set of letters, which indicate
c	which source parameters of a component are to be held fixed.
c	These source parameters are fixed by the initial estimates
c	given by the `spar' parameter.
c	The letters corresponding to each source parameter are:
c	  f   The flux is fixed.
c	  x   The offset in RA is fixed.
c	  y   The offset in DEC is fixed.
c	  a   The major axis parameter is fixed.
c	  b   The minor axis parameter is fixed.
c	  p   The position angle parameter is fixed.
c	  c   The gaussian or disk is circular (not elliptical).
c	For a source where all source parameters vary, a dash (-)
c	can be used for this parameter.
c
c	For example "fix=fx,fc" indicates that the flux and RA offset
c	is to be fixed for the first source, whereas the second source,
c	(which is presumably a gaussian or disk) has a fixed flux, and
c	is circular.
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
c    rjs  13dec90  Original version.
c    rjs  15jan91  Retyped it in, because I deleted the old one!!
c    mjs  06dec91  Changed "itoa" to "itoaf"
c    rjs  24apr92  PGPLOT standardisation.
c    nag  24nov92  Generalization for N point sources
c    nag  10dec92  Add variance
c    nag  16feb93  Various tidying, correcting of errors, etc.
c    rjs  21jul93  Use uvDatGtr('variance'..). Extra error checks.
c    rjs  10nov93  Work on multi-channel data.
c    rjs  24jan94  Change ownership
c    rjs  23jul94  Major rewrite. No error estimates for the time
c		   being.
c------------------------------------------------------------------------
	integer MAXVAR
	parameter(MAXVAR=20)
	include 'maxdim.h'
	character version*(*)
        parameter(version='version 1.0 23-Jul-94')
	include 'uvfit.h'
c
	real tol,epsfcn
	parameter(tol=1e-3,epsfcn=1e-2)
c
	character out*64,ltype*16
	integer lIn,lOut
	integer nread,ifail,i,nvar,npol,pol
	real x(MAXVAR)
	double precision preamble(4),sfreq(MAXCHAN)
	complex data(MAXCHAN),Model(MAXCHAN)
	logical flags(MAXCHAN),dores
	integer fvec,wa,iwa(MAXVAR),lwa
c
c  Externals.
c
        character itoaf*8
	logical uvDatOpn
        external FUNCTION
c
c  Dynamic memory commons.
c
	include 'mem.h'
c
c  Get the inputs.
c
	call output('Uvfit: '//version)
	call keyini
	call uvDatInp('vis','sdlpxbcef')
	call LoadSrc
	call keya('out',out,' ')
	call GetOpt(dores)
        call keyfin
c
c  Set the polarisations to ii if nothing was selected.
c
	call uvDatGti('npol',npol)
	if(npol.gt.1)
     *	  call bug('f','Only a single polarisation can be selected')
	if(npol.eq.0)call uvDatSet('stokes',0)
c
c  Open the visibility file, and read all the data.
c
	call output('Reading the data ...')
        if(.not.uvDatOpn(lIn))
     *	  call bug('f','Error opening input file')
        nvis = 0
	call uvDatRd(preamble,data,flags,MAXCHAN,nread)
        dowhile(nread.ge.1)
	  call uvinfo(lIn,'sfreq',sfreq)
	  do i=1,nread
	    if(flags(i))then
	      nvis = nvis + 1
	      if(nvis.gt.MAXVIS)call bug('f','Buffer overflow')
	      u(nvis) = preamble(1)*sfreq(i)
	      v(nvis) = preamble(2)*sfreq(i)
	      vis(nvis) = data(i)
	    endif
	  enddo
	  call uvDatRd(preamble,data,flags,MAXCHAN,nread)
        enddo
        if(nvis.le.0)call bug('f','No valid data')
        call output('Number of visibilities: '//itoaf(nvis))
        call uvDatCls
c
c  Pack the things that we are going to solve for.
c
	call PackPar(x,nvar,MAXVAR)
c
c  Call the nllsqu solver.
c
	if(nvar.gt.0)then
	  lwa = 2*nvis*nvar + 5*nvar + 2*nvis
	  call memalloc(wa,lwa,'r')
	  call memalloc(fvec,nvis,'c')
	  call output('Performing the fitting process ...')
	  call lmdiff(FUNCTION,2*nvis,nvar,x,memc(fvec),epsfcn,tol,
     *	    ifail,iwa,memr(wa),lwa)
	  call memfree(fvec,nvis,'c')
	  call memfree(wa,lwa,'r')
	  call Upackpar(x,nvar)
	else
	  call bug('w','There are no free parameters')
	  ifail = 1
	endif
c
c  Do the least squares fit.
c
	if(ifail.eq.0)then
	  ifail = 1
	else if(ifail.ge.1.and.ifail.le.3)then
	  ifail = 0
	endif
        if(ifail.ne.0)call bug('w','Failed to converge: ifail='
     *      //itoaf(ifail))
c
c  Report on the results.
c
	call Report
c
c  Write out the results.
c
	if(out.ne.' ')then
	  call output('Generating output file ...')
	  call uvDatRew()
	  call uvDatGta('ltype',ltype)
	  if(.not.uvDatOpn(lIn))
     *	    call bug('f','Error opening input file')
	  call VarInit(lIn,ltype)
c	  
	  call uvopen(lOut,out,'new')
	  call hdcopy(lIn,lOut,'history')
	  call hisopen(lOut,'append')
	  call hiswrite(lOut,'UVFIT: Miriad '//version)
	  call hisinput(lOut,'UVFIT')
	  call hisclose(lOut)
	  call VarOnit(lIn,lOut,ltype)
c
c  Get the first record, and write the polarisation type.
c
	  call uvDatRd(preamble,data,flags,MAXCHAN,nread)
c
	  call uvputvri(lOut,'npol',1,1)
	  call uvDatGti('pol',pol)
	  call uvputvri(lOut,'pol',pol,1)
c
c  Process all the records.
c
	  dowhile(nread.ge.1)
	    call uvinfo(lIn,'sfreq',sfreq)
	    do i=1,nread
	      u(i) = preamble(1) * sfreq(i)
	      v(i) = preamble(2) * sfreq(i)
	    enddo
	    if(dores)then
	      call Eval(u,v,Model,nread)
	      do i=1,nread
	        data(i) = data(i) - model(i)
	      enddo
	    else
	      call Eval(u,v,data,nread)
	    endif
	    call VarCopy(lIn,lOut)
	    call uvwrite(lOut,preamble,data,flags,nread)
	    call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	  enddo
	  call uvclose(lOut)
	  call uvDatCls
	endif
c
        end
c************************************************************************
	subroutine PackPar(x,nvar,MAXVAR)
c
	implicit none
	integer nvar,MAXVAR
	real x(MAXVAR)
c
c  Store all the things that we need to vary.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
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
	    tmp(ncurr) = 180*3600/pi * l0(i)
	  endif
	  if(vm0(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = 180*3600/pi * m0(i)
	  endif
c
c  Gaussian and disk sources.
c
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN)then
	    if(vfwhm1(i))then
	      ncurr = ncurr + 1
	      tmp(ncurr) = 180*3600/pi * fwhm1(i)
	    endif
	    if(vfwhm2(i))then
	      ncurr = ncurr + 1
	      tmp(ncurr) = 180*3600/pi * fwhm2(i)
	    endif
	    if(vpa(i))then
	      ncurr = ncurr + 1
	      tmp(ncurr) = 180/pi * pa(i)
	    endif
	  endif
c
c  Copy the estimates of x to the variables.
c
	  if(nvar+ncurr.gt.MAXVAR)
     *	    call bug('f','Too many free parameters')
	  do j=1,ncurr
	    nvar = nvar + 1
	    x(nvar) = tmp(j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine UPackPar(x,nvar)
c
	implicit none
	integer nvar
	real x(nvar)
c
c  Store all the things that we need to vary.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
	integer i,n
c
	n = 0
	do i=1,nsrc
	  if(vflux(i))then
	    n = n + 1
	    flux(i) = x(n)
	  endif
	  if(vl0(i))then
	    n = n + 1
	    l0(i) = pi/180/3600 * x(n)
	  endif
	  if(vm0(i))then
	    n = n + 1
	    m0(i) = pi/180/3600 * x(n)
	  endif
c
c  Gaussian and disk sources.
c
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN)then
	    if(vfwhm1(i))then
	      n = n + 1
	      fwhm1(i) = pi/180/3600 * abs(x(n))
	      if(circ(i))fwhm2(i) = fwhm1(i)
	    endif
	    if(vfwhm2(i))then
	      n = n + 1
	      fwhm2(i) = pi/180/3600 * abs(x(n))
	    endif
	    if(vpa(i))then
	      n = n + 1
	      pa(i) = pi/180 * x(n)
	    endif
	  endif
	enddo
c
	if(n.ne.nvar)
     *	  call bug('f','Inconsistent number of free parameters')
c
	end
c************************************************************************
	subroutine FUNCTION(m,nvar,x,fvec,iflag)
c
	implicit none
	integer m,nvar,iflag
	real x(nvar)
	complex fvec(m/2)
c
c------------------------------------------------------------------------
	include 'uvfit.h'
	integer i
c
c  Check and unpack the things that we are solving for.
c
	if(m.ne.2*nvis)call bug('f','Inconsistency in FUNCTION')
	call Upackpar(x,nvar)
c
c  Evaluate the model.
c
	call Eval(u,v,fvec,nvis)
c
c  Return the residual.
c
	do i=1,nvis
	  fvec(i) = vis(i) - fvec(i)
	enddo
c
	end
c************************************************************************
	subroutine Eval(uu,vv,model,n)
c
	implicit none
	integer n
	real uu(n),vv(n)
	complex model(n)
c
c  Evaluate the source model.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
	integer i,j
	real amp,theta,beta,cosi,sini,fac
	complex w
c
c  Externals.
c
	real j1xbyx
c
c  Initialise the model to 0.
c
	fac = pi**2/4.0/log(2.0)
c
	do i=1,n
	  model(i) = 0
	enddo
c
c  Loop over the various model types.
c
	do j=1,nsrc
c
c  Point source component.
c
	  if(srctype(j).eq.POINT)then
	    do i=1,n
	      theta = 2*pi*(uu(i)*l0(j)+vv(i)*m0(j))
	      w = flux(j)*cmplx(cos(theta),sin(theta))
	      model(i) = model(i) + w
	    enddo
c
c  Gaussian component.
c
	  else if(srctype(j).eq.GAUSSIAN)then
	    cosi = cos(pa(j))
	    sini = sin(pa(j))
	    do i=1,n
	      theta = 2*pi*(uu(i)*l0(j)+vv(i)*m0(j))
	      w = flux(j)*cmplx(cos(theta),sin(theta))
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +		     (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      if(fac*beta.lt.70)then
	        amp = exp(-fac*beta)
	        model(i) = model(i) + amp*w
	      endif
	    enddo
c
c  Disk component.
c
	  else if(srctype(j).eq.DISK)then
	    cosi = cos(pa(j))
	    sini = sin(pa(j))
	    do i=1,n
	      theta = 2*pi*(uu(i)*l0(j)+vv(i)*m0(j))
	      w = flux(j)*cmplx(cos(theta),sin(theta))
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +		     (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      amp = 2*j1xbyx(pi*sqrt(beta))
	      model(i) = model(i) + amp*w
	    enddo
	  else
	    call bug('f','Software bug: unrecognised srctype')
	  endif
	enddo
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
	subroutine Report
	implicit none
c
c  Report on the source component solution.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
	real f1,f2,p,t
	integer i
	character line*80
c
	integer NOBJS
	parameter(NOBJS=3)
	character objects(NOBJS)*8
c
	data objects(DISK)    /'disk    '/
	data objects(GAUSSIAN)/'gaussian'/
	data objects(POINT)   /'point   '/
c
	call output('------------------------------------------------')
c
	do i=1,nsrc
	  write(line,10)i,objects(srctype(i))
   10	  format('Source',i3,', Object type: ',a)
	  call output(line)
	  write(line,20)flux(i)
   20	  format('  Flux:',1pg34.4)
	  call output(line)
	  write(line,25)3600*180/pi*l0(i),3600*180/pi*m0(i)
   25	  format('  Offset Position (arcsec): ',2f9.2)
	  call output(line)
c
	  if(srctype(i).eq.GAUSSIAN.or.srctype(i).eq.DISK)then
	    f1 = 3600*180/pi * fwhm1(i)
	    f2 = 3600*180/pi * fwhm2(i)
	    p = 180/pi * pa(i)
	    if(f1.lt.f2)then
	      t = f1
	      f1 = f2
	      f2 = t
	      p = p + 90
	    endif
	    p = mod(p,180.)
	    if(p.lt.-90)p = p + 180
	    if(p.gt. 90)p = p - 180
c
	    write(line,30)f1,f2
   30	    format('  Major,minor axes (arcsec):',2f9.2)
	    call output(line)
	    write(line,40)p
   40	    format('  Position angle (degrees):',f9.1)
	    call output(line)
	  endif
	enddo
	call output('------------------------------------------------')
c	  
	end
c************************************************************************
	subroutine LoadSrc
	implicit none
c
c Load the source components and their initial estimates.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
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
	data objects/'disk    ','gaussian','point   '/
	data objtype/DISK,       GAUSSIAN,  POINT/
c
	nsrc = 0
	dowhile(keyprsnt('object'))
c
c  Get the source type.
c
	  nsrc = nsrc + 1
	  if(nsrc.gt.MAXSRC)call bug('f','Too many source')
	  call keymatch('object',NOBJS,objects,1,object,nout)
	  i = binsrcha(object,objects,NOBJS)
	  srctype(nsrc) = objtype(i)
c
c  Get the source parameters.
c
	  call keyr('spar',flux(nsrc),0.)
	  call keyr('spar',l0(nsrc),0.)
	  call keyr('spar',m0(nsrc),0.)
	  l0(nsrc) = pi/180/3600 * l0(nsrc)
	  m0(nsrc) = pi/180/3600 * m0(nsrc)
c
	  if(srctype(nsrc).eq.DISK.or.srctype(nsrc).eq.GAUSSIAN)then
	    call keyr('spar',fwhm1(nsrc),1.)
	    call keyr('spar',fwhm2(nsrc),1.)
	    if(min(fwhm1(nsrc),fwhm2(nsrc)).le.0)
     *	      call bug('f','Invalid FWHM parameters given')
	    call keyr('spar',pa(nsrc),0.)
	    fwhm1(nsrc) = pi/180/3600 * fwhm1(nsrc)
	    fwhm2(nsrc) = pi/180/3600 * fwhm2(nsrc)
	    pa(nsrc) = pi/180 * pa(nsrc)
	  endif
c
c  Determine what is fixed, and what is variable.
c
	  call keya('fix',fix,'-')
	  call lcase(fix)
	  vflux(nsrc) = index(fix,'f').eq.0
	  vl0(nsrc)   = index(fix,'x').eq.0
	  vm0(nsrc)   = index(fix,'y').eq.0
	  if(srctype(nsrc).eq.GAUSSIAN.or.srctype(nsrc).eq.DISK)then
	    vfwhm1(nsrc)= index(fix,'a').eq.0
	    circ(nsrc) = index(fix,'c').ne.0
	    if(circ(nsrc))then
	      vfwhm2(nsrc) = .false.
	      vpa(nsrc)    = .false.
	      fwhm2(nsrc) = fwhm1(nsrc)
	      pa(nsrc) = 0
	    else
	      vfwhm2(nsrc) = index(fix,'b').eq.0
	      vpa(nsrc)    = index(fix,'p').eq.0
	    endif
	  endif
	enddo
c
	end
