c************************************************************************
	program uvpit
c
c= uvpit - Fit polarisation of a time-varying point source.
c& rjs
c: uv analysis
c	UVPIT is a Miriad task which fits visibility data with a time
c	varying, polarised point source. Optionally the model or
c	residual visibilities can be written out.
c
c	This will work for datasets that contain either all four
c	linear polarisations, or just the parallel hands.
c@ vis
c	Name of the input visibility file. No default.
c@ line
c	Normal line-type processing with normal defaults. However, you
c	must select only a single channel!!
c@ select
c	Normal data selection. Default is all cross-correlation data.
c@ flux
c	Initial estimates of the I,Q,U,V flux densities. The default is
c	1,0,0,0.
c@ offset
c	Initial estimates of the source offset, in arcsec. The default is
c	0,0.
c@ out
c	The optional output data-set. The default is not to create an
c	output data-set. If an output dataset name is given, then
c	either the model or residual visibilities can be saved.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Minimum match is used. Possible values are:
c	  noqu     Do not attempt to solve for Stokes-Q and U.
c	  nov      Do not attempt to solve for Stokes-V. The default is
c	           to attempt to solve for V if there are XY and YX
c	           correlations present.
c	  noshift  Fix the source position.
c	  constant The source does not vary with time. The default is
c	           to assume the source varies with time.
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
c    rjs  31aug94  Reborn as uvpit.
c------------------------------------------------------------------------
	integer PolXX,PolYY,PolXY,PolYX
	parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
	integer MAXVAR
	parameter(MAXVAR=20)
	include 'maxdim.h'
	character version*(*)
        parameter(version='version 1.0 23-Jul-94')
	include 'uvpit.h'
c
	real tol,epsfcn
	parameter(tol=1e-6,epsfcn=1e-3)
c
	character out*64,ltype*16
	integer lIn,lOut
	integer nread,ifail,i,nvar,npol
	real x(MAXVAR),rms
	double precision preamble(4),sfreq(MAXCHAN),time0
	complex data(MAXCHAN),Model(MAXCHAN)
	logical flags(MAXCHAN),dores,cross
	integer fvec,wa,iwa(MAXVAR),lwa
	integer ipol
	real chi0
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
	call output('Uvpit: '//version)
	call keyini
	call uvDatInp('vis','dlpxbcef')
	call keya('out',out,' ')
	call GetOpt(dores,noqu,nov,const,noshift)
	call LoadSrc
        call keyfin
c
c  Open the visibility file, and read all the data.
c
	call output('Reading the data ...')
        if(.not.uvDatOpn(lIn))
     *	  call bug('f','Error opening input file')
	call coInit(lIn)
c
	cross = .false.
        nvis = 0
	call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	time0 = preamble(3)
        dowhile(nread.ge.1)
	  call uvinfo(lIn,'sfreq',sfreq)
	  call uvDatGti('pol',ipol)
	  call uvrdvrr(lIn,'chi',chi0,0.)
	  if(ipol.ne.PolXX.and.ipol.ne.PolYY.and.
     *	     ipol.ne.PolXY.and.ipol.ne.PolYX)
     *	    call bug('f','UVPIT works for linear pol. data only')
	  do i=1,nread
	    if(flags(i))then
	      cross = cross.or.ipol.eq.PolXY.or.ipol.eq.PolYX
	      nvis = nvis + 1
	      if(nvis.gt.MAXVIS)call bug('f','Buffer overflow')
	      u(nvis) = preamble(1)*sfreq(i)
	      v(nvis) = preamble(2)*sfreq(i)
	      chi(nvis) = chi0
	      pol(nvis) = ipol
	      t(nvis) = preamble(3) - time0
	      vis(nvis) = data(i)
	    endif
	  enddo
	  call uvDatRd(preamble,data,flags,MAXCHAN,nread)
        enddo
        if(nvis.le.0)call bug('f','No valid data')
        call output('Number of visibilities: '//itoaf(nvis))
        call uvDatCls
c
	if(.not.cross.and..not.nov)then
	  call bug('w','No cross-hand polarisations present')
	  call bug('w','Stokes-V not being solved for')
	  nov = .true.
	endif
c
c  Pack the things that we are going to solve for.
c
	call CoordFid(lIn,.true.)
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
	  call Upackpar(x,nvar)
	  call GetRMS(nvis,memc(fvec),rms)
	  call memfree(fvec,nvis,'c')
	  call memfree(wa,lwa,'r')
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
	call CoordFid(lIn,.false.)
	call Report(time0,rms)
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
	  call hiswrite(lOut,'UVPIT: Miriad '//version)
	  call hisinput(lOut,'UVPIT')
	  call hisclose(lOut)
	  call VarOnit(lIn,lOut,ltype)
c
c  Get the first record, and write the polarisation type.
c
	  call uvDatRd(preamble,data,flags,MAXCHAN,nread)
c
c  Process all the records.
c
	  dowhile(nread.ge.1)
	    call uvDatGti('npol',npol)
	    call uvputvri(lOut,'npol',npol,1)
	    call uvDatGti('pol',ipol)
	    call uvputvri(lOut,'pol',ipol,1)
	    call uvrdvrr(lIn,'chi',chi0,0.)
	    call uvinfo(lIn,'sfreq',sfreq)
	    do i=1,nread
	      u(i) = preamble(1) * sfreq(i)
	      v(i) = preamble(2) * sfreq(i)
	      chi(i) = chi0
	      pol(i) = ipol
	      t(i)   = preamble(3) - time0
	    enddo
	    if(dores)then
	      call Eval(u,v,chi,t,pol,Model,nread)
	      do i=1,nread
	        data(i) = data(i) - model(i)
	      enddo
	    else
	      call Eval(u,v,chi,t,pol,data,nread)
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
	subroutine CoordFid(lIn,topix)
c
	implicit none
	integer lIn
	logical topix
c
c  Convert between true offset coordinates and grid coordinates.
c------------------------------------------------------------------------
	include 'uvpit.h'
	double precision x1(2),x2(2)
c
	x1(1) = offset(1)
	x1(2) = offset(2)
c
	if(topix)then
	  call coCvt(lIn,'ow/ow',x1,'op/op',x2)
	else
	  call coCvt(lIn,'op/op',x1,'ow/ow',x2)
	endif
c
	offset(1) = x2(1)
	offset(2) = x2(2)
c
	end
c************************************************************************
	subroutine GetRMS(m,fvec,rms)
c
	implicit none
	integer m
	real rms
	complex fvec(m)
c
c  Determine the rms residual.
c------------------------------------------------------------------------
	integer i
	double precision dtemp
c
	dtemp = 0
	do i=1,m
	  dtemp = dtemp + real(fvec(i))**2 + aimag(fvec(i))**2
	enddo
c
	rms = sqrt(dtemp / (2*m))
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
	include 'uvpit.h'
c
	x(1) = iflux
	nvar = 1
	if(.not.noqu)then
	  x(nvar+1) = qflux
	  x(nvar+2) = uflux
	  nvar = nvar + 2
	endif
	if(.not.nov)then
	  x(nvar+1) = vflux
	  nvar = nvar + 1
	endif
	if(.not.const)then
	  x(nvar+1) = dfdt
	  nvar = nvar + 1
	endif
	if(.not.noshift)then
	  x(nvar+1) = 3600*180/pi * offset(1)
	  x(nvar+2) = 3600*180/pi * offset(2)
	  nvar = nvar + 2
	endif
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
	include 'uvpit.h'
	integer n
c
	iflux = x(1)
	n = 1
	if(.not.noqu)then
	  qflux = x(n+1)
	  uflux = x(n+2)
	  n = n + 2
	endif
	if(.not.nov)then
	  vflux = x(n+1)
	  n = n + 1
	endif
	if(.not.const)then
	  dfdt = x(n+1)
	  n = n + 1
	endif
	if(.not.noshift)then
	  offset(1) = pi/180/3600 * x(n+1)
	  offset(2) = pi/180/3600 * x(n+2)
	  n = n + 2
	endif
c
	if(n.ne.nvar)call bug('f','Inconsistency in UnPackPar')
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
	include 'uvpit.h'
	integer i
c
c  Check and unpack the things that we are solving for.
c
	if(m.ne.2*nvis)call bug('f','Inconsistency in FUNCTION')
	call Upackpar(x,nvar)
c
c  Evaluate the model.
c
	call Eval(u,v,chi,t,pol,fvec,nvis)
c
c  Return the residual.
c
	do i=1,nvis
	  fvec(i) = vis(i) - fvec(i)
	enddo
c
	end
c************************************************************************
	subroutine Eval(uu,vv,chi0,t0,pol0,model,n)
c
	implicit none
	integer n
	real uu(n),vv(n),chi0(n),t0(n)
	integer pol0(n)
	complex model(n)
c
c  Evaluate the source model.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer PolXX,PolYY,PolXY,PolYX
	parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
	include 'uvpit.h'
	integer i
	real theta
	complex w
c
	do i=1,n
	  if(pol0(i).eq.PolXX)then
	    model(i) = iflux
     *		+ qflux*cos(2*chi0(i)) + uflux*sin(2*chi0(i))
	  else if(pol0(i).eq.PolYY)then
	    model(i) = iflux
     *		- qflux*cos(2*chi0(i)) - uflux*sin(2*chi0(i))
	  else if(pol0(i).eq.PolXY)then
	    model(i) = -qflux*sin(2*chi0(i)) + uflux*cos(2*chi0(i))
     *		      - cmplx(0.,vflux)
	  else if(pol0(i).eq.PolYX)then
	    model(i) = -qflux*sin(2*chi0(i)) + uflux*cos(2*chi0(i))
     *		      + cmplx(0.,vflux)
	  else
	    call bug('f','Unsupported polarisation')
	  endif
	  theta = 2*pi*(uu(i)*offset(1)+vv(i)*offset(2))
	  w = cmplx(cos(theta),sin(theta))
	  model(i) = model(i) * (1 + dfdt*t0(i)) * w
	enddo	    
c
	end
c************************************************************************
	subroutine GetOpt(dores,noqu,nov,const,noshift)
c
	implicit none
	logical dores,noqu,nov,const,noshift
c
c  Get extra processing options.
c
c  Output:
c    dores
c    noqu
c    nov
c    const
c    noshift
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=5)
	character opts(nopts)*8
	logical present(nopts)
	data opts/'residual ','noqu    ','nov     ','constant',
     *		  'noshift  '/
c
	call options('options',opts,present,nopts)
c
	dores   = present(1)
	noqu    = present(2)
	nov     = present(3)
	const   = present(4)
	noshift = present(5)
	end
c************************************************************************
	subroutine Report(time0,rms)
c
	implicit none
	double precision time0
	real rms
c
c  Report on the source component solution.
c------------------------------------------------------------------------
	include 'uvpit.h'
	include 'mirconst.h'
	real per,pa
	character line*64
c
	write(line,5)rms
	call output(line)
c
	if(.not.const)then
	  call julday(time0,'H',line(1:20))
	  call output('Fluxes referenced to time: '//line(1:20))
	endif
c
	call output('Flux Densities:')
	write(line,10)'I',iflux
	call output(line)
	if(.not.noqu)then
	  write(line,10)'Q',qflux
	  call output(line)
	  write(line,10)'U',uflux
	  call output(line)
	  per = 100 * sqrt(qflux*qflux+uflux*uflux) / iflux
	  if(abs(uflux)+abs(qflux).gt.0)then
	    pa  = 90/pi * atan2(uflux,qflux)
	  else
	    pa = 0
	  endif
	endif
c
	if(.not.nov)then
	  write(line,10)'V',vflux
	  call output(line)
	endif
c
	if(.not.noqu)then
	  write(line,20)per
	  call output(line)
	  write(line,30)pa
	  call output(line)
	endif
c
	if(.not.const)then
	  write(line,40)100*dfdt
	  call output(line)
	endif
c
	if(.not.noshift)then
	  write(line,50)3600*180/pi*offset(1),3600*180/pi*offset(2)
	  call output(line)
	endif
c
   5	format('RMS residual is',1pe10.3)
  10	format('  Stokes ',a,': ',f10.4,' Jy')
  20	format('Fraction linear polarisation:',f7.2,' %')
  30    format('Linear pol. position angle:  ',f7.2,' deg.')
  40	format('Flux time derivative:        ',f7.2,' %/day')
  50    format('Offset position:           ',2f9.2,' arcsec')
c
	end
c************************************************************************
	subroutine LoadSrc
c
	implicit none
c
c  Load initial estimates of the source parameters.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'uvpit.h'
c
	call keyr('flux',iflux,1.)
	call keyr('flux',qflux,0.)
	call keyr('flux',uflux,0.)
	call keyr('flux',vflux,0.)
	dfdt = 0
	call keyr('offset',offset(1),0.)
	call keyr('offset',offset(2),0.)
	offset(1) = pi/180/3600 * offset(1)
	offset(2) = pi/180/3600 * offset(2)
c
	end
