	program uvfit
c
c= uvfit - Fit point sources to a given vis file.
c& rjs
c: uv analysis
c+
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
c	Normal line-type processing with normal defaults. 
c@ select
c	Normal data selection. Default is all cross-correlation data.
c@ object
c	This gives the object type that uvfit fits for. Several objects
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
c	This gives initial estimates of source parameters.  For
c	each object given by the `object' keyword, either 3 (for
c	point sources) or 6 (for disks and gaussians) values should be
c	given. The values are as follows:
c	  Object Type             SPAR values
c	  -----------             -----------
c	   point                   flux,x,y
c	   gaussian                flux,x,y,bmaj,bmin,pa
c	   disk                    flux,x,y,bmaj,bmin,pa
c	   shell                   flux,x,y,bmaj
c	   ring                    flux,x,y,bmaj,bmin,pa
c
c	Here "flux" is the total flux density of the component,
c	"x" and "y" are the offset positions (in arcsec) of the object
c	relative to the observing center, "bmaj" and "bmin" are the major
c	and minor axes FWHM (in arcsec), and "pa" is the position angle
c	of an elliptical component (in degrees). The position angle is
c	measured from north through east.
c	You must give initial estimates for all parameters for each object
c	(this includes parameters that are redundant or meaningless,
c	such as "bmin" and "pa" for components that are constrained to be
c	circular).
c
c	The more complex the set of objects being fitted for, the more
c	important it is to give a good estimate of the source parameters.
c	Generally the estimates of the source position should be accurate
c	to the fundamental resolution (for point sources) or the size of
c	the component (for extended sources).
c@ bin
c       Specify frequency and time binning of the data.
c       The first value specifies the size of the frequency bins in GHz
c       to divide the data into. 
c       The second value specifies the size of the time bins in hours.
c       An initial fit with all the data will be done to improve the 
c       starting values of the fit.
c       The output will contain the interpolated model (or residuals). 
c       A spectral index estimate will be given for frequency binning.
c       Defaults to 0,0 (no binning).
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
c	  c   The gaussian, disk or ring is circular (not elliptical).
c	For a source where all source parameters vary, a dash (-)
c	can be used for this parameter.
c
c	For example "fix=fx,fc" indicates that the flux and RA offset
c	is to be fixed for the first source, whereas the second source,
c	(which is presumably a gaussian, disk or ring) has a fixed flux, and
c	is circular
c@ fix2
c       Like fix, but applied after the initial fit using all data.
c       Use this with the bin parameter to fix e.g., position while 
c       fitting for flux variation with time or frequency.
c       Defaults to the value of fix.
c@ out
c       Optionally, you can specify one or more output files for model
c       or residual visibilities. If you specify one output file, all data
c       will be written to that file. If you specify more than one output
c       file, the number of output files should equal the number of input 
c	visibility files.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Minimum match is used. Possible values are:
c	  residual The output data-set is the residual visibilities.
c	           If an output is being created, the default is to make
c	           this the fitted model.
c         
c
c$Id: uvfit.for,v 1.10 2013/08/30 01:49:21 wie017 Exp $
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
c    rjs   1sep94  Error estimates!
c    bmg  08may96  Added fitting for a thin, spherical shell
c    bmg  29may96  Added fitting for a thin face-on ring. I have coded
c                  spheres and thick shells, but they don't work yet!
c    rjs  18mar97  Handle multiple files, autocorr data, better message.
c    bmg  14jan05  Added 2 more significant figures to offset position
c    mhw  07nov11  Add frequency and time binning
c    mhw  19oct12  Write multiple output files (copied from uvsfit)
c-----------------------------------------------------------------------
	include 'maxdim.h'
	include 'uvfit.h'
        integer maxOut
        parameter (maxOut=10)
c
	character out(maxOut)*64, ltype*16, version*72
        character calday1*20, calday2*20
        character*16 fix2(MAXSRC)
	integer lIn,lOut,nout
	integer nread,ifail1,ifail2,i,j,k,nvar,npol,pol,iOut
	real x(MAXVAR),x1(MAXVAR),rms,covar(MAXVAR*MAXVAR)
        real cflux(MAXSRC),csflux(MAXSRC),cl(MAXSRC),csl(MAXSRC)
        real cm(MAXSRC),csm(MAXSRC),cw1(MAXSRC),csw1(MAXSRC)
        real cw2(MAXSRC),csw2(MAXSRC),cpa(MAXSRC),cspa(MAXSRC)
	double precision preamble(4),sfreq(MAXCHAN)
	complex data(MAXCHAN),Model(MAXCHAN)
	logical flags(MAXCHAN),dores
c
c  Externals.
c
        character itoaf*8, dtoaf*8, versan*72
	logical uvDatOpn
        external FUNCTION,FUNCTION1
c-----------------------------------------------------------------------
      version = versan ('uvfit',
     :                  '$Revision: 1.10 $',
     :                  '$Date: 2013/08/30 01:49:21 $')
c
c  Get the inputs.
c
	call keyini
	call uvDatInp('vis','sdlpcef')
	call LoadSrc(fix2,MAXSRC)
        call keyd('bin',df,0.d0)
        call keyd('bin',dt,0.d0)
        dt = dt / 24.d0
	call mkeya('out',out,maxOut,nout)
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
                freq(nvis) = sfreq(i)
                time(nvis) = preamble(3)
	        vis(nvis) = data(i)
	      endif
	    enddo
	    call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	  enddo
	  call uvDatCls
	enddo
        if(nvis.le.0)call bug('f','No valid data found')
        call output('Total number of correlations: '//itoaf(nvis))
c
c  Pack the things that we are going to solve for.
c
	call PackPar(x,nvar,MAXVAR)
	if(nOut.eq.0.and.nvar.eq.0)
     *	  call bug('f','Nothing to be done -- check inputs!')
	if(nvar.ge.2*nvis)call bug('f','Too few correlations to fit')
c
c  Call the least squares solver.
c
	if(nvar.gt.0)then
	  call output('Performing the fitting process ...')
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
	if(nvar.gt.0)call Report(rms,.false.)
c
c  Now do the binned solutions if requested        
c
        ntime=1
        nfreq=1
        if(nvar.gt.0.and.(dt.gt.0.or.df.gt.0)) then
c
c         Determine time, freq, ranges to solve for
c        
          call tslots
          call fslots
c
c        Save the continuum parameters for later
c          
          do i=1,nsrc
            cflux(i)=flux(i)
            csflux(i)=sflux(i)
            cl(i)=l0(i)
            csl(i)=sl0(i)
            cm(i)=m0(i)
            csm(i)=sm0(i)
            cw1(i)=fwhm1(i)
            csw1(i)=sfwhm1(i)
            cw2(i)=fwhm2(i)
            csw2(i)=sfwhm2(i)
            cpa(i)=pa(i)
            cspa(i)=spa(i)
          enddo
c
c        Set the parameters to be free/fixed, repack params and save copy
c
          call Fixed(fix2,nsrc)
          call PackPar(x,nvar,MAXVAR)
          do i=1,nvar
            x1(i)=x(i)
          enddo
c          
          do j=1,nfreq
            mfreq(j)=0
            nf(j)=0
          enddo
c
c         Solve for each time/freq bin
c              
          do k=1,ntime
            call julday(times(k),'H',calday1)
            call julday(times(k)+dt,'H',calday2)
            call output('Doing time range '//calday1//' - '//calday2)
            do j=1,nfreq
              call output('Doing freq range '//dtoaf(freqs(j),1,3)//
     *           ' to '//dtoaf(freqs(j)+df,1,3)//' GHz')
              nvis1=0
              do i=1,nvis
                if (time(i).ge.times(k)
     *            .and.time(i).le.times(k)+dt) then   
                  if (freq(i).ge.freqs(j)
     *            .and.freq(i).le.freqs(j)+df) then
                    nvis1=nvis1+1
                    u1(nvis1)=u(i)
                    v1(nvis1)=v(i)
                    vis1(nvis1)=vis(i)
                    mfreq(j)=mfreq(j)+log(freq(i))
                    nf(j)=nf(j)+1
                  endif
                endif
              enddo
              call output('Number of correlations: '//itoaf(nvis1))
c
c             Set 'global' solution as first guess
c
              do i=1,nvar
                x(i)=x1(i)
              enddo
              if (nvar.lt.2*nvis1) then
                call lsqfit(FUNCTION1,2*nvis1,nvar,x,covar,rms,ifail1,
     *           ifail2)   
	        call Upackpar(x,nvar)
                do i=1,nvar
                  par(j,k,i,1)=x(i)
                enddo
	        if(ifail2.eq.0)call UpackCov(covar,nvar)
	        if(ifail1.ne.0)then
	          call bug('w','Failed to converge: ifail='
     *             //itoaf(ifail1))
	        else if(ifail2.ne.ifail1)then
	          call bug('w','Failed to determine covariance matrix')
	        endif
                call Report(rms,.false.)
              else
                call bug('w','Too few correlations to fit')
              endif
            enddo
          enddo
c
c  Report spectral index
c
          if (nfreq.gt.1) then
c
c           Restore the continuum parameters
c          
            do i=1,nsrc
              flux(i)=cflux(i)
              sflux(i)=csflux(i)
              l0(i)=cl(i)
              sl0(i)=csl(i)
              m0(i)=cm(i)
              sm0(i)=csm(i)
              fwhm1(i)=cw1(i)
              sfwhm1(i)=csw1(i)
              fwhm2(i)=cw2(i)
              sfwhm2(i)=csw2(i)
              pa(i)=cpa(i)
              spa(i)=cspa(i)
            enddo
            call Report(rms,.true.)
          endif
        endif
c
c  Write out the results.
c
        iOut = 0
	if(nOut.gt.0)then
	  call output('Generating output file(s) ...')
	  call uvDatRew()
	  call uvDatGta('ltype',ltype)
          dowhile(uvDatOpn(lIn))
            iOut = iOut+1
            if (nOut.gt.1.and.iOut.gt.nOut) then
              call bug('f','Too few output files given')
            endif
	    call VarInit(lIn,ltype)
c
            if (iOut.le.nOut) then
	      call uvopen(lOut,out(iOut),'new')
	      call hdcopy(lIn,lOut,'history')
	      call hisopen(lOut,'append')
	      call hiswrite(lOut,'UVFIT: Miriad '//version)
	      call hisinput(lOut,'UVFIT')
	      call hisclose(lOut)
            endif
	    call VarOnit(lIn,lOut,ltype)
c
            if (ntime.gt.1.or.nfreq.gt.1) call int2dIni(nvar)
c
c  Get the first record, and write the polarisation type.
c
	    call uvDatRd(preamble,data,flags,MAXCHAN,nread)
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
                if (ntime.eq.1.and.nfreq.eq.1) then
                  call Eval(u,v,Model,nread)
                else
  	          call Eval2(nvar,u,v,Model,sfreq,preamble(3),nread)
                endif
	        do i=1,nread
	          data(i) = data(i) - model(i)
	        enddo
	      else
                if (ntime.eq.1.and.nfreq.eq.1) then
                  call Eval(u,v,data,nread)
                else
	          call Eval2(nvar,u,v,data,sfreq,preamble(3),nread)
                endif
	      endif
	      call VarCopy(lIn,lOut)
	      call uvwrite(lOut,preamble,data,flags,nread)
	      call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	    enddo
            if (nOut.gt.1) call uvclose(lOut)
	    call uvDatCls
          enddo
          if (nOut.eq.1) call uvclose(lOut)
	endif
c
        end
c***********************************************************************
	subroutine tslots
c       Determine the time slots
c-----------------------------------------------------------------------
	include 'uvfit.h'
	integer i,j
        double precision tmin,tmax
c
        tmin=time(1)
        tmax=time(1)
        do i=2,nvis
          if (time(i).lt.tmin) tmin=time(i)
          if (time(i).gt.tmax) tmax=time(i)
        enddo
        times(1)=tmin
        if (dt.eq.0.d0) then
          ntime=1
          dt=tmax-tmin
          return
        endif
        
        do j=2,MAXBIN
          times(j)=tmax
          do i=1,nvis
            if (time(i).gt.times(j-1)+dt.and.time(i).lt.times(j))
     *        times(j)=time(i)
          enddo
        enddo
        ntime=1
        do j=2,MAXBIN
          if (times(j).lt.tmax) ntime=ntime+1
        enddo
        if (ntime.eq.MAXBIN.and.times(MAXBIN).lt.tmax-dt)
     *    call bug('w','Exceeded max number of time bins, increase'//
     *    ' time bin size')   
        end
c***********************************************************************
	subroutine fslots
c       Determine the frequency slots
c-----------------------------------------------------------------------
	include 'uvfit.h'
	integer i,j
        double precision fmin,fmax
c
        fmin=freq(1)
        fmax=freq(1)
        do i=2,nvis
          if (freq(i).lt.fmin) fmin=freq(i)
          if (freq(i).gt.fmax) fmax=freq(i)
        enddo
        freqs(1)=fmin
        if (df.eq.0.d0) then
          nfreq=1
          df=fmax-fmin
          return
        endif
        
        do j=2,MAXBIN
          freqs(j)=fmax
          if (freqs(j-1).lt.fmax) then
            do i=1,nvis
              if (freq(i).gt.freqs(j-1)+df.and.freq(i).lt.freqs(j))
     *          freqs(j)=freq(i)
            enddo
          endif
        enddo
        nfreq=1
        do j=2,MAXBIN
          if (freqs(j).lt.fmax) nfreq=nfreq+1
        enddo
        if (nfreq.eq.MAXBIN.and.freqs(MAXBIN).lt.fmax-df)
     *    call bug('w','Exceeded max number of freq bins, increase'//
     *    ' freq bin size')   
        end
c***********************************************************************
	subroutine PackPar(x,nvar,nmax)
c
	integer nvar,nmax
	real x(nmax)
c
c  Store all the things that we need to vary.
c-----------------------------------------------------------------------
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
c  Gaussian, disk, shell and ring sources.
c
	  if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN.or.
     *        srctype(i).eq.SHELL.or.srctype(i).eq.RING)then
	    if(vfwhm1(i))then
	      n = n + 1
	      fwhm1(i) = pi/180/3600 * abs(x(n))
	      if(circ(i)) then
                fwhm2(i) = fwhm1(i)
              endif
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
	include 'uvfit.h'
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
	      if(circ(i)) then
                sfwhm2(i) = sfwhm1(i)
              endif
            else
              sfwhm1(i)=0
              sfwhm2(i)=0
	    endif
	    if(vfwhm2(i))then
	      n = n + 1
	      sfwhm2(i) = pi/180/3600 * sqrt(abs(covar(n,n)))
            else
              sfwhm2(i)=0
	    endif
	    if(vpa(i))then
	      n = n + 1
	      spa(i) = pi/180 * sqrt(abs(covar(n,n)))
            else
              spa(i) = 0
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
c***********************************************************************
	subroutine FUNCTION1(m,nvar,x,fvec,iflag)
c
	integer m,nvar,iflag
	real x(nvar)
	complex fvec(m/2)
c
c-----------------------------------------------------------------------
	include 'uvfit.h'
	integer i
c
c  Check and unpack the things that we are solving for.
c
	if(m.ne.2*nvis1)call bug('f','Inconsistency in FUNCTION')
	call Upackpar(x,nvar)
c
c  Evaluate the model.
c
	call Eval(u1,v1,fvec,nvis1)
c
c  Return the residual.
c
	do i=1,nvis1
	  fvec(i) = vis1(i) - fvec(i)
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
	include 'uvfit.h'
	integer i,j
	real amp,theta,beta,cosi,sini,fac
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

c
c  Thin shell component.
c
          else if(srctype(j).eq.SHELL)then
	    cosi = cos(pa(j))
	    sini = sin(pa(j))
	    do i=1,n
	      theta = 2*pi*(uu(i)*l0(j)+vv(i)*m0(j))
	      w = flux(j)*cmplx(cos(theta),sin(theta))
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
	      w = flux(j)*cmplx(cos(theta),sin(theta))
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
	subroutine int2dIni(nvar)
        integer nvar
c
c  
c  Calculate Interpolation parameters.
c-----------------------------------------------------------------------
	include 'uvfit.h'
	integer ivar,ifreq,itime
        double precision x(MAXBIN),y(MAXBIN),b(MAXBIN),c(MAXBIN),
     *    d(MAXBIN)
        do ivar=1,nvar
          do ifreq=1,nfreq
            do itime=1,ntime
              x(itime)=times(itime)+dt/2
              y(itime)=par(ifreq,itime,ivar,1)
            enddo
            call spline(ntime,x,y,b,c,d)
            do itime=1,ntime
              par(ifreq,itime,ivar,2) = b(itime)
              par(ifreq,itime,ivar,3) = c(itime)
              par(ifreq,itime,ivar,4) = d(itime)
            enddo
          enddo
        enddo
        end
c***********************************************************************
	subroutine int2d(nvar,nmax,n,t,f,val)
        integer nvar,nmax,n
        double precision val(nmax,n),t,f(n)
c
c  
c  Interpolate the parameters.
c-----------------------------------------------------------------------
	include 'uvfit.h'
	integer ivar,ifreq,itime,i
        double precision x(MAXBIN),y(MAXBIN),b(MAXBIN),c(MAXBIN),
     *    d(MAXBIN),r(MAXBIN)
c
c  Externals.
c
	double precision seval
c
c  Interpolate in time for each frequency
c  Then interpolate in frequency
c     
        do ivar=1,nvar
          do ifreq=1,nfreq
            do itime=1,ntime
              y(itime)=par(ifreq,itime,ivar,1)
              b(itime)=par(ifreq,itime,ivar,2)
              c(itime)=par(ifreq,itime,ivar,3)
              d(itime)=par(ifreq,itime,ivar,4)
            enddo
            x(ifreq)=freqs(ifreq)+df/2
            r(ifreq)=seval(ntime,t,times,y,b,c,d)
          enddo
          call spline(nfreq,x,r,b,c,d)
          do i=1,n
            val(ivar,i)=seval(nfreq,f(i),x,r,b,c,d)
          enddo
        enddo
        end
c***********************************************************************
	subroutine Eval2(nvar,uu,vv,model,sfreq,stime,n)
c
	integer nvar,n
	real uu(n),vv(n)
        double precision sfreq(n),stime 
	complex model(n)
c
c  Evaluate the source model.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
        include 'maxdim.h'
	integer i,j,k
	real amp,theta,beta,cosi,sini,fac,x(MAXVAR)
        double precision val(MAXVAR,MAXCHAN)
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
c  Interpolation
c       
        call int2d(nvar,MAXVAR,n,stime,sfreq,val)
c
c  Loop over the spectrum and the various model types.
c
        do i=1,n
          do k=1,nvar
            x(k) = val(k,i)
          enddo       
          call UPackPar(x,nvar)
          do j=1,nsrc
            if (srctype(j).ne.POINT) then
              cosi = cos(pa(j))
              sini = sin(pa(j))
            endif
	    theta = 2*pi*(uu(i)*l0(j)+vv(i)*m0(j))
	    w = flux(j)*cmplx(cos(theta),sin(theta))
c
c  Point source component.
c
	    if(srctype(j).eq.POINT)then
	      model(i) = model(i) + w
c
c  Gaussian component.
c
	    else if(srctype(j).eq.GAUSSIAN)then
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     *               (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      if(fac*beta.lt.70)then
	        amp = exp(-fac*beta)
	        model(i) = model(i) + amp*w
	      endif
c
c  Disk component.
c
	    else if(srctype(j).eq.DISK)then
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +               (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      amp = 2*j1xbyx(pi*sqrt(beta))
	      model(i) = model(i) + amp*w
c
c  Thin shell component.
c
            else if(srctype(j).eq.SHELL)then
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +		     (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      amp = sin(pi*sqrt(beta))/pi/sqrt(beta)
	      model(i) = model(i) + amp*w
c
c  Thin ring component.
c
            else if(srctype(j).eq.RING)then
	      beta = (fwhm2(j)*(uu(i)*cosi-vv(i)*sini))**2 +
     +		     (fwhm1(j)*(uu(i)*sini+vv(i)*cosi))**2
	      amp = bessj0(dble(pi*sqrt(beta)))
	      model(i) = model(i) + amp*w
	    else
	      call bug('f','Software bug: unrecognised srctype')
	    endif
          enddo
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
	subroutine Report(rms,doalpha)
c
	real rms
        logical doalpha
c
c  Report on the source component solution.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
	real f1,f2,p,t,sf1,sf2,sp,flx(MAXSRC,MAXBIN)
        real Slow,Shigh,Flow,Fhigh,alpha,salpha
	integer i,j,k,n,nlow,nhigh
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
c
        if (rms.gt.0) then
	  write(line,5)rms
    5	  format('RMS residual is',1pe10.3)
	  call output(line)
	  call output(' ')
        endif
        
        if (doalpha) then
          call 
     *       output('---- Continuum Solution with spectral index ----')
          n=0
          do i=1,nsrc
            if (vflux(i)) then
              n=n+1
              do j=1,nfreq
                flx(i,j)=0
                do k=1,ntime
                  flx(i,j) = flx(i,j)+par(j,k,n,1)
                enddo
              enddo
            endif
            if (vl0(i)) n=n+1
            if (vm0(i)) n=n+1
            if (srctype(i).ne.POINT) then
              if (vfwhm1(i)) n=n+1
              if (vfwhm2(i)) n=n+1
              if (vpa(i)) n=n+1
            endif
          enddo
          Flow=0
          Fhigh=0
          nlow=0
          nhigh=0
          do j=1,nfreq/2
            Flow=Flow+mfreq(j)
            nlow=nlow+nf(j)
            Fhigh=Fhigh+mfreq(nfreq+1-j)
            nhigh=nhigh+nf(nfreq+1-j)
          enddo
          Flow=Flow/nlow
          Fhigh=Fhigh/nhigh
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
          if (doalpha) then
            Slow=0
            Shigh=0
            do j=1,nfreq/2
              Slow=Slow+flx(i,j)
              Shigh=Shigh+flx(i,nfreq+1-j)
            enddo
            alpha = log(Shigh/Slow)/(Fhigh-Flow)
            salpha = sqrt(2.0)*sflux(i)/flux(i)/(Fhigh-Flow)
            write(line,21) alpha,salpha
            call output(line)
   21       format('  Spectral index:               ',f6.3,' +/- ',f6.3)
          endif
	  write(line,25)3600*180/pi*l0(i),3600*180/pi*m0(i)
   25	  format('  Offset Position (arcsec):  ',2f11.4)
	  call output(line)
	  if(sl0(i)+sm0(i).gt.0)then
	    write(line,26)3600*180/pi*sl0(i),3600*180/pi*sm0(i)
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
   41	      format('  Pos. angle error (degrees):',1pe9.2)
	      call output(line)
	    endif
	  endif
	enddo
	call output('------------------------------------------------')
c
	end
c***********************************************************************
	subroutine LoadSrc(fix2,n)
        integer n
        character*16 fix2(n)
c
c Load the source components and their initial estimates.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
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
          call keya('fix2',fix2(nsrc),'-')
	enddo        
c
	end
c***********************************************************************
	subroutine Fixed(fix2,n)
        integer n
	character*16 fix2(n)
c
c Decode the fix2 parameter.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvfit.h'
	integer i
        character*16 fix
c
c  Determine what is fixed, and what is variable.
c
	do i=1,n
          fix=fix2(i)
	  call lcase(fix)
	  vflux(i) = index(fix,'f').eq.0
	  vl0(i)   = index(fix,'x').eq.0
	  vm0(i)   = index(fix,'y').eq.0

         if(srctype(i).eq.DISK.or.srctype(i).eq.GAUSSIAN.or.
     *        srctype(i).eq.SHELL.or.srctype(i).eq.RING)then
            vfwhm1(i)= index(fix,'a').eq.0
            if(srctype(i).eq.SHELL)then
               circ(i) = .true.
 	    else
		circ(i) = index(fix,'c').ne.0
            endif
	    if(circ(i))then
	      vfwhm2(i) = .false.
	      vpa(i)    = .false.
	      fwhm2(i) = fwhm1(i)
	      pa(i) = 0
	    else
	      vfwhm2(i) = index(fix,'b').eq.0
	      vpa(i)    = index(fix,'p').eq.0
	    endif
	  endif
	enddo
c
	end
