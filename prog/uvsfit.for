	program uvsfit
c
c= uvsfit - Fit point sources to a given vis file.
c& dmcc
c: uv analysis
c+
c	UVSFIT is a Miriad task which fits model components to a visibility
c	dataset. UVSFIT differs from UVFIT by fitting explicitly for the
c       frequency dependence of source flux. Optionally the model or
c       residual visibilities can be written out.
c       UVSFIT can only handle a limited number of visibilities in fitting
c       mode. You can reduce the data volume by averaging in frequency
c       using the line parameter (apply bandpass first) or selecting a 
c       subset of the data. If you want to produce model or residual 
c       visibilities at full resolution for all the data, you can
c       run UVSFIT again with all source parameters specified and fixed.
c	
c@ vis
c	Name of the input visibility file or files. No default.
c@ stokes
c	Normal Stokes/polarisation parameter (e.g. i,q,u,v,ii etc).
c	Only a single polarisation can be requested. The default is
c	`ii' (i.e. Stokes-I for an unpolarised source).
c@ line
c	Normal line-type processing with normal defaults.
c@ select
c	Normal data selection. Default is all cross-correlation data.
c@ object
c	This gives the object type that uvsfit fits for. Several objects
c	can be given (the objects can be of the same type, or different),
c	and minimum match is supported. Possible objects are
c	  point       A point source
c	  disk        An elliptical or circular disk.
c	  gaussian    An elliptical or circular gaussian.
c	  shell       The 2D projection of a thin, spherical shell.
c	  ring        A face-on, thin, elliptical or circular ring
c	For example, to fit for a point source and gaussian, use:
c	`object=point,gaussian'.
c@ spar
c        This gives initial estimates of source parameters.  For
c        each object given by the `object' keyword, either 3 (for
c        point sources) or 6 (for disks and gaussians) values should be
c        given. The values are as follows:
c          Object Type             SPAR values
c          -----------             -----------
c           point                   flux,x,y
c           gaussian                flux,x,y,bmaj,bmin,pa
c           disk                    flux,x,y,bmaj,bmin,pa
c           shell                   flux,x,y,bmaj
c           ring                    flux,x,y,bmaj,bmin,pa
c 
c        Here "flux" is the total flux density of the component,
c        "x" and "y" are the offset positions (in arcsec) of the object
c        relative to the observing center, "bmaj" and "bmin" are the major
c        and minor axes FWHM (in arcsec), and "pa" is the position angle
c        of an elliptical component (in degrees). The position angle is
c        measured from north through east.
c        You must give initial estimates for all parameters for each object
c        (this includes parameters that are redundant or meaningless,
c        such as "bmin" and "pa" for components that are constrained to be
c        circular).
c 
c        The more complex the set of objects being fitted for, the more
c        important it is to give a good estimate of the source parameters.
c        Generally the estimates of the source position should be accurate
c        to the fundamental resolution (for point sources) or the size of
c        the component (for extended sources).
c@ fix
c        This gives a set a flag parameters, one parameter per source.
c        Each parameter consists of a set of letters, which indicate
c        which source parameters of a component are to be held fixed.
c        These source parameters are fixed by the initial estimates
c        given by the `spar' parameter.
c        The letters corresponding to each source parameter are:
c          f   The flux is fixed.
c          x   The offset in RA is fixed.
c          y   The offset in DEC is fixed.
c          a   The major axis parameter is fixed.
c          b   The minor axis parameter is fixed.
c          p   The position angle parameter is fixed.
c          c   The gaussian, disk or ring is circular (not elliptical).
c        For a source where all source parameters vary, a dash (-)
c        can be used for this parameter.
c 
c        For example "fix=fx,fc" indicates that the flux and RA offset
c        is to be fixed for the first source, whereas the second source,
c        (which is presumably a gaussian, disk or ring) has a fixed flux, and
c        is circular.
c
c@ sspar
c        This gives initial estimates of source spectral parameters.  For
c        each object given by the `object' keyword, 3  values should be
c        given. The values, for all object types, are as follows:
c                     SSPAR values
c                     -----------
c                    alpha0,alpha1,alpha2
c 
c        Here "alpha0", "alpha1" and "alpha2" are defined in:
c
c                  lfr = ln(frq/frq0)
c		  alpha(frq) = alpha0 + lfr*(alpha1 + lfr*alpha2)
c
c        giving the spectral index as a function of frequency so that
c 
c                  flux(frq) = flux*(frq/frq0)**alpha(frq)
c
c        gives the component flux at frequency frq. Here frq0 is the reference
c        frequency. If alpha0, alpha1, alpha2 are not given, values of zero
c	are used.
c	If any sspar values are given, all (three per source) must be given.
c@ sfix
c        This gives a set a flag parameters for the spectral parameters,
c        one parameter per source.  Each parameter consists of a set of
c        digits, indicating which of the spectral terms are to be held fixed.
c        These source parameters are fixed by the initial estimates
c        given by the `sspar' parameter.
c        The digits corresponding to each source parameter are:
c          0   The spectral term alpha0 is fixed.
c          1   The spectral term alpha1 is fixed.
c          2   The spectral term alpha2 is fixed.
c
c        For a source where all source parameters vary, a dash (-)
c        can be used for this parameter.
c@ freqref
c       A frequency in GHz used as the reference frequency for the flux
c       and spectral index results.
c@ out
c       Optionally, an output file will be written for each input file.
c       If given, the number of output data-sets should equal the number
c	of input visibility files, and either the model or residual
c	visibilities can be saved.
c@ options
c        Extra processing options. Several can be given, separated by commas.
c        Minimum match is used. Possible values are:
c          residual The residual visibilities are written to the output.
c                   If an output is being created, the default is to make
c                   this the fitted model.
c@ log
c       Optionally, a log file will be written with the model parameters
c	and their errors.  For each fitted source, four lines are written
c	to the log: line 1 has source number, object type and three or six
c	parameter values, depending on the object type; line 2 has the
c	formal errors in each (0.0 if not a free parameter); line 3 has
c	the values of all three spectral terms; line 4 has the formal
c	errors in those terms (again 0.0 if not included in the fit).
c
c$Id: uvsfit.for,v 1.6 2013/12/05 01:59:50 wie017 Exp $
c--
c  History:
c    dmcc 12jan12  Original version, adapted from uvfit version 14jan05.
c    dmcc 10oct12  Adapted for spectral curvature and for general use.
c    dmcc 22oct12  Include ref freq in output log; fix errors in logging.
c    mhw  05dec13  Don't read/accumulate data if not fitting
c-----------------------------------------------------------------------
	include 'maxdim.h'
	include 'uvsfit.h'
	integer maxOut
	parameter (maxOut=4)
c
	character out(maxOut)*64, ltype*16, version*72, line*80
	character logfile*64
	integer lIn,lOut,nout
	integer nread,ifail1,ifail2,i,nvar,npol,pol,iOut
	real x(MAXVAR),rms,covar(MAXVAR*MAXVAR)
	double precision preamble(4),sfreq(MAXCHAN)
	complex data(MAXCHAN),Model(MAXCHAN)
	logical flags(MAXCHAN),dores, dolog
c
c  Externals.
c
        character itoaf*8, versan*72
	logical uvDatOpn
        external FUNCTION
c-----------------------------------------------------------------------
      version = versan ('uvsfit',
     :                  '$Revision: 1.6 $',
     :                  '$Date: 2013/12/05 01:59:50 $')
c
c  Get the inputs.
c  Relative to original uvfit, insist on cross-corrlelations with flag 'x'.
c
	call keyini
	call uvDatInp('vis','sdlpxcef')
	call LoadSrc
        call keyr('freqref',freqref, 0.0)
	call mkeya('out',out,maxOut,nout)
	call GetOpt(dores)
	call keya('log',logfile,' ')
        call keyfin

	dolog = logfile.ne.' '
	if (dolog) call logopen(logfile,' ')

c
c  Set the polarisations to ii if nothing was selected.
c
	call uvDatGti('npol',npol)
	if(npol.gt.1)
     *	  call bug('f','Only a single polarisation can be selected')
	if(npol.eq.0)call uvDatSet('stokes',0)
c
c  Pack the things that we are going to solve for.
c
	call PackPar(x,nvar)
	if(nout.eq.0.and.nvar.eq.0)
     *	  call bug('f','Nothing to be done -- check inputs!')
c
c  Open the visibility file, and read all the data.
c
        if (nvar.gt.0) then
	  call output('Reading the data ...')
          nvis = 0
	  dowhile(uvDatOpn(lIn))
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
		  freq(nvis) = sfreq(i)
	        endif
	      enddo
	      call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	    enddo
	    call uvDatCls
	  enddo
          if(nvis.le.0)call bug('f','No valid data found')
          call output('Total number of correlations: '//itoaf(nvis))

c
c  Set the reference frequency
c
	  if (freqref .eq. 0.0) then
	     freqref = freq(1)
	  endif
	  if(nvar.ge.2*nvis)call bug('f','Too few correlations to fit')
c
c  Call the least squares solver.
c
	  write(line,24) nvar
 24	  format('Performing the fitting process: ',i2,' params...')
	  call output(line)
	  call lsqfit(FUNCTION,2*nvis,nvar,x,covar,rms,ifail1,ifail2)
	  call Upackpar(x,nvar)
	  if(ifail2.eq.0)call UpackCov(covar,nvar)
	  if(ifail1.ne.0)then
	    call bug('w','Failed to converge: ifail='//itoaf(ifail1))
	  else if(ifail2.ne.ifail1)then
	    call bug('w','Failed to determine covariance matrix')
	  endif
	else
	  call bug('w','There are no free parameters')
	endif
c
c  Report on the results.
c
	if(nvar.gt.0)call Report(rms, dolog)
c
c  Write out the results.
c
	iOut = 0
	if (nout.gt.0) then
	   call output('Generating output file ...')
	   call uvDatRew()  
	   dowhile(uvDatOpn(lIn))
	     iOut = iOut+1
	     if (iOut.gt.nOut) then
		call bug('f','Too few output files given')
	     end if
             if (iOut.eq.1) call uvDatGta('ltype',ltype) 
	     call VarInit(lIn,ltype)
c
	     call uvopen(lOut,out(iOut),'new')
	     call hdcopy(lIn,lOut,'history')
	     call hisopen(lOut,'append')
	     call hiswrite(lOut,'UVSFIT: Miriad '//version)
	     call hisinput(lOut,'UVSFIT')
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
		  freq(i) = sfreq(i)
		  u(i) = preamble(1) * sfreq(i)
		  v(i) = preamble(2) * sfreq(i)
	       enddo
               if (freqref.eq.0) freqref=sfreq(1)
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
	  enddo
	endif
c
        end
c***********************************************************************
	subroutine PackPar(x,nvar)
c
	integer nvar
	real x(nvar)
c
c  Store all the things that we need to vary.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvsfit.h'
	integer i,j,ncurr
	real tmp(7)
c
	nvar = 0
	do i=1,nsrc
	  ncurr = 0
	  if(vflux(i))then
	    ncurr = ncurr + 1
	    tmp(ncurr) = flux(i)
	  endif
          if(valph0(i))then
             ncurr = ncurr + 1
             tmp(ncurr) = alph0(i)
          endif
          if(valph1(i))then
             ncurr = ncurr + 1
             tmp(ncurr) = alph1(i)
          endif
          if(valph2(i))then
             ncurr = ncurr + 1
             tmp(ncurr) = alph2(i)
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
c  Gaussian, disk, shell and ring sources.
c
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN.or.
     *        srctype(i).eq.SHELL.or.srctype(i).eq.RING)then
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
c***********************************************************************
	subroutine UPackPar(x,nvar)
c
	integer nvar
	real x(nvar)
c
c  Store all the things that we need to vary.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvsfit.h'
	integer i,n
c
	n = 0
	do i=1,nsrc
	  if(vflux(i))then
	    n = n + 1
	    flux(i) = x(n)
	  endif
          if(valph0(i))then
             n = n + 1
             alph0(i) = x(n)
          end if
          if(valph1(i))then
             n = n + 1
             alph1(i) = x(n)
          end if
          if(valph2(i))then
             n = n + 1
             alph2(i) = x(n)
          end if
	  if(vl0(i))then
	    n = n + 1
	    l0(i) = pi/180/3600 * x(n)
	  endif
	  if(vm0(i))then
	    n = n + 1
	    m0(i) = pi/180/3600 * x(n)
	  endif
c
c  Gaussian, disk, shell and ring sources.
c
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN.or.
     *        srctype(i).eq.SHELL.or.srctype(i).eq.RING)then
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
c***********************************************************************
	subroutine UPackCov(covar,nvar)
c
	integer nvar
	real covar(nvar,nvar)
c
c  Unpack the covariance matrix.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvsfit.h'
	integer i,n
c
	n = 0
	do i=1,nsrc
	  if(vflux(i))then
	    n = n + 1
	    sflux(i) = sqrt(abs(covar(n,n)))
	  else
	    sflux(i) = 0
	  endif
          if(valph0(i))then
             n = n + 1
             salph0(i) = sqrt(abs(covar(n,n)))
          else
             salph0(i) = 0
          endif
          if(valph1(i))then
             n = n + 1
             salph1(i) = sqrt(abs(covar(n,n)))
          else
             salph1(i) = 0
          endif
          if(valph2(i))then
             n = n + 1
             salph2(i) = sqrt(abs(covar(n,n)))
          else
             salph2(i) = 0
          endif
	  if(vl0(i))then
	    n = n + 1
	    sl0(i) = pi/180/3600 * sqrt(abs(covar(n,n)))
	  else
	    sl0(i) = 0
	  endif
	  if(vm0(i))then
	    n = n + 1
	    sm0(i) = pi/180/3600 * sqrt(abs(covar(n,n)))
	  else
	    sm0(i) = 0
	  endif
c
c  Gaussian, disk, shell and ring sources.
c
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN.or.
     *        srctype(i).eq.SHELL.or.srctype(i).eq.RING)then
	    if(vfwhm1(i))then
	      n = n + 1
	      sfwhm1(i) = pi/180/3600 * sqrt(abs(covar(n,n)))
	      if(circ(i))sfwhm2(i) = sfwhm1(i)
	    endif
	    if(vfwhm2(i))then
	      n = n + 1
	      sfwhm2(i) = pi/180/3600 * sqrt(abs(covar(n,n)))
	    endif
	    if(vpa(i))then
	      n = n + 1
	      spa(i) = pi/180 * sqrt(abs(covar(n,n)))
	    endif
	  endif
	enddo
c
	if(n.ne.nvar)
     *	  call bug('f','Inconsistent number of free parameters')
c
	end
c***********************************************************************
	subroutine FUNCTION(m,nvar,x,fvec,iflag)
c
	integer m,nvar,iflag
	real x(nvar)
	complex fvec(m/2)
c
c-----------------------------------------------------------------------
	include 'uvsfit.h'
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
c***********************************************************************
	subroutine Eval(uu,vv,model,n)
c
	integer n
	real uu(n),vv(n)
	complex model(n)
c
c  Evaluate the source model.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvsfit.h'
	integer i,j
	real amp,theta,beta,cosi,sini,fac,flx,fr,alph
	complex w

c
c  Externals.
c
	real j1xbyx
	double precision bessj0
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
		 fr = freq(i)/freqref
		 alph = alph0(j) + log(fr)*(alph1(j)+log(fr)*alph2(j))
		 flx = flux(j)*fr**alph
		 w = flx*cmplx(cos(theta),sin(theta))
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
              fr = freq(i)/freqref
              alph = alph0(j) + log(fr)*(alph1(j)+log(fr)*alph2(j))
              flx = flux(j)*fr**alph
	      w = flx*cmplx(cos(theta),sin(theta))
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
              fr = freq(i)/freqref
              alph = alph0(j) + log(fr)*(alph1(j)+log(fr)*alph2(j))
              flx = flux(j)*fr**alph
	      w = flx*cmplx(cos(theta),sin(theta))
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +		     (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      amp = 2*j1xbyx(pi*sqrt(beta))
	      model(i) = model(i) + amp*w
	    enddo

c
c  Thin shell component.
c
          else if(srctype(j).eq.SHELL)then
	    cosi = cos(pa(j))
	    sini = sin(pa(j))
	    do i=1,n
	      theta = 2*pi*(uu(i)*l0(j)+vv(i)*m0(j))
              fr = freq(i)/freqref
              alph = alph0(j) + log(fr)*(alph1(j)+log(fr)*alph2(j))
              flx = flux(j)*fr**alph
	      w = flx*cmplx(cos(theta),sin(theta))
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +		     (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      amp = sin(pi*sqrt(beta))/pi/sqrt(beta)
	      model(i) = model(i) + amp*w
	    enddo
c
c  Thin ring component.
c
          else if(srctype(j).eq.RING)then
	    cosi = cos(pa(j))
	    sini = sin(pa(j))
	    do i=1,n
	      theta = 2*pi*(uu(i)*l0(j)+vv(i)*m0(j))
              fr = freq(i)/freqref
              alph = alph0(j) + log(fr)*(alph1(j)+log(fr)*alph2(j))
              flx = flux(j)*fr**alph
	      w = flx*cmplx(cos(theta),sin(theta))
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +		     (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      amp = bessj0(dble(pi*sqrt(beta)))
	      model(i) = model(i) + amp*w
	    enddo
	  else
	    call bug('f','Software bug: unrecognised srctype')
	  endif
	enddo
c
	end
c***********************************************************************
	subroutine GetOpt(dores)
c
	logical dores
c
c  Get extra processing options.
c
c  Output:
c    dores
c-----------------------------------------------------------------------
	integer nopts
	parameter(nopts=1)
	character opts(nopts)*8
	logical present(nopts)
	data opts/'residual'/
c
	call options('options',opts,present,nopts)
c
	dores = present(1)
	end
c***********************************************************************
	subroutine Report(rms, dolog)
c
	real rms
	logical dolog
c
c  Report on the source component solution.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvsfit.h'
	real f1,f2,p,t,sf1,sf2,sp
	real l0a,m0a,sl0a,sm0a
	integer i
	logical more
	character line*80
c
	integer NOBJS
	parameter(NOBJS=5)
	character objects(NOBJS)*8
c
	data objects(DISK)    /'disk    '/
	data objects(GAUSSIAN)/'gaussian'/
	data objects(POINT)   /'point   '/
	data objects(RING)    /'ring    '/
	data objects(SHELL)   /'shell   '/

c
	call output('------------------------------------------------')
	if (dolog) call loginput('uvsfit')
c
	write(line,3)freqref
    3	format('Reference frequency (GHz): ',f7.3)
	call output(line)
	write(line,5)rms
    5	format('RMS residual is',1pe10.3)
	call output(line)
	call output(' ')
	if (dolog) then
	   write(line,3)freqref
	   call logwrite(line,more)
	   write(line,5) rms
	   call logwrite(line, more)
	endif
c
	do i=1,nsrc
	  write(line,10)i,objects(srctype(i))
   10	  format('Source',i3,', Object type: ',a)
	  call output(line)
	  if(sflux(i).gt.0)then
	    write(line,20)flux(i),sflux(i)
	  else
	    write(line,20)flux(i)
	  endif
   20	  format('  Flux: ',1pg34.4,:,' +/- ',1pe8.2)
	  call output(line)
	  l0a = 3600*180/pi*l0(i)
	  m0a = 3600*180/pi*m0(i)
	  sl0a = 3600*180/pi*sl0(i)
	  sm0a = 3600*180/pi*sm0(i)
	  write(line,25)l0a,m0a
   25	  format('  Offset Position (arcsec):  ',2f11.4)
	  call output(line)
	  if(sl0(i)+sm0(i).gt.0)then
	    write(line,26)sl0a,sm0a
   26	    format('  Positional errors (arcsec):    ',1p2e9.2)
	    call output(line)
	  endif
c
c
c  Gaussian, disk, shell and ring sources.
c
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN.or.
     *        srctype(i).eq.SHELL.or.srctype(i).eq.RING)then
	    f1 = 3600*180/pi * fwhm1(i)
	    f2 = 3600*180/pi * fwhm2(i)
	    sf1 = 3600*180/pi * sfwhm1(i)
	    sf2 = 3600*180/pi * sfwhm2(i)
	    p = 180/pi * pa(i)
	    sp = 180/pi * spa(i)
	    if(f1.lt.f2)then
	      t = f1
	      f1 = f2
	      f2 = t
	      t = sf1
	      sf1 = sf2
	      t = sf2
	      p = p + 90
	    endif
	    p = mod(p,180.)
	    if(p.lt.-90)p = p + 180
	    if(p.gt. 90)p = p - 180
c
            write(line,30)f1,f2
   30       format('  Major,minor axes (arcsec):',2f11.4)

	    call output(line)
	    if(sf1+sf2.gt.0)then
	      write(line,31)sf1,sf2
   31	      format('  Axes errors (arcsec):     ',1p2e9.2)
	      call output(line)
	    endif
	    write(line,40)p
   40	    format('  Position angle (degrees):  ',f9.1)
	    call output(line)
	    if(sp.gt.0)then
	      write(line,41)sp
   41	      format('  Pos  angle error (degrees):',1pe9.2)
	      call output(line)
	    endif
	  endif
c
c       write spectral parameters
c
          if(salph0(i).gt.0)then
             write(line,51)alph0(i),salph0(i)
          else
             write(line,51)alph0(i)
          endif
   51	  format('  Alpha0:',1pg34.4,:,' +/- ',1pe8.2)
          call output(line)
          if(salph1(i).gt.0)then
             write(line,53)alph1(i),salph1(i)
          else
             write(line,53)alph1(i)
          endif
   53	  format('  Alpha1:',1pg34.4,:,' +/- ',1pe8.2)
          call output(line)
          if(salph2(i).gt.0)then
             write(line,54)alph2(i),salph2(i)
          else
             write(line,54)alph2(i)
          endif
   54	  format('  Alpha2:',1pg34.4,:,' +/- ',1pe8.2)
          call output(line)

c
c       Write log records
c
	  if (dolog) then
	     if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN.or.
     *          srctype(i).eq.SHELL.or.srctype(i).eq.RING)then
	        write(line,62) i,objects(srctype(i)),flux(i),l0a,m0a,
     *                         f1,f2,p
 62		format(i3,1x,a,1p,g14.3,0p,f10.3,f10.3,2f10.4,f9.1)
		call logwrite(line,more)
		write(line,63) sflux(i),sl0a,sm0a,sf1,sf2,sp
 63		format(12x,1p,e14.3,0p,2f10.3,2f10.4,f9.1)
	     else
	        write(line,65) i,objects(srctype(i)),flux(i),l0a,m0a
 65		format(i3,1x,a,1p,g14.3,0p,f10.3,f10.3)
		call logwrite(line,more)
		write(line,66) sflux(i),sl0a,sm0a
 66		format(12x,1p,e14.3,0p,2f10.3)
	     endif
	     call logwrite(line,more)
	     write(line,68) alph0(i),alph1(i),alph2(i)
 68	     format(12x,1p,3g14.4)
	     call logwrite(line,more)
	     write(line,70) salph0(i),salph1(i),salph2(i)
 70	     format(12x,1p,3g14.4)
	     call logwrite(line,more)
	  endif
	enddo
	call output('------------------------------------------------')
	if (dolog) call logclose()
c
	end
c***********************************************************************
	subroutine LoadSrc
c
c Load the source components and their initial estimates.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvsfit.h'
	integer nout,i
	character object*16,fix*16,sfix*16
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

 	data objects/'disk    ','gaussian','point   ','ring   ',
     *               'shell   '/
	data objtype/ DISK,      GAUSSIAN,  POINT, RING,  SHELL/

c
	nsrc = 0
	dowhile(keyprsnt('object'))
c
c  Get the source type.
c
	  nsrc = nsrc + 1
	  if(nsrc.gt.MAXSRC)call bug('f','Too many sources')
          call keymatch('object',NOBJS,objects,1,object,nout)
          i = binsrcha(object,objects,NOBJS)
	  srctype(nsrc) = objtype(i)

c
c  Set all the parameters to the default.
c
	  sflux(nsrc) = 0
	  sl0(nsrc) = 0
	  sm0(nsrc) = 0
	  sfwhm1(nsrc) = 0
	  sfwhm2(nsrc) = 0
	  spa(nsrc) = 0
	  salph0(nsrc) = 0
	  salph1(nsrc) = 0
	  salph2(nsrc) = 0
c
c  Get the source parameters.
c
	  call keyr('spar',flux(nsrc),0.)
	  call keyr('spar',l0(nsrc),0.)
	  call keyr('spar',m0(nsrc),0.)
	  l0(nsrc) = pi/180/3600 * l0(nsrc)
	  m0(nsrc) = pi/180/3600 * m0(nsrc)
c
          if(srctype(nsrc).eq.DISK.or.srctype(nsrc).eq.GAUSSIAN.or.
     +        srctype(nsrc).eq.RING)then
	    call keyr('spar',fwhm1(nsrc),1.)
	    call keyr('spar',fwhm2(nsrc),1.)
	    if(min(fwhm1(nsrc),fwhm2(nsrc)).le.0)
     *	      call bug('f','Invalid FWHM parameters given')
	    call keyr('spar',pa(nsrc),0.)
	    pa(nsrc) = pi/180 * pa(nsrc)
	    fwhm1(nsrc) = pi/180/3600 * fwhm1(nsrc)
	    fwhm2(nsrc) = pi/180/3600 * fwhm2(nsrc)
	  endif
c
          If(srctype(nsrc).eq.SHELL)then
	    call keyr('spar',fwhm1(nsrc),1.)
	    if(fwhm1(nsrc).le.0)
     *	      call bug('f','Invalid FWHM parameters given')
	    fwhm1(nsrc) = pi/180/3600 * fwhm1(nsrc)
	  endif
c
c Fetch the spectal parameters
c
          call keyr('sspar',alph0(nsrc),0.)
          call keyr('sspar',alph1(nsrc),0.)
          call keyr('sspar',alph2(nsrc),0.)
c
c  Determine what is fixed, and what is variable.
c
	  call keya('fix',fix,'-')
	  call lcase(fix)
	  vflux(nsrc) = index(fix,'f').eq.0
	  vl0(nsrc)   = index(fix,'x').eq.0
	  vm0(nsrc)   = index(fix,'y').eq.0


         if(srctype(nsrc).eq.DISK.or.srctype(nsrc).eq.GAUSSIAN.or.
     *        srctype(nsrc).eq.SHELL.or.srctype(nsrc).eq.RING)then
            vfwhm1(nsrc)= index(fix,'a').eq.0
            if(srctype(nsrc).eq.SHELL)then
               circ(nsrc) = .true.
 	    else
		circ(nsrc) = index(fix,'c').ne.0
            endif
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
c
c  Determine what spectral parameter is fixed, and what is variable.
c
	  call keya('sfix',sfix,'-')
	  call lcase(sfix)
          valph0(nsrc) = index(sfix,'0').eq.0
          valph1(nsrc) = index(sfix,'1').eq.0
          valph2(nsrc) = index(sfix,'2').eq.0

	enddo
c
	end
