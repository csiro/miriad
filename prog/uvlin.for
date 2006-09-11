c************************************************************************
	program uvlin
	implicit none
c
c= uvlin - Separate continuum and lines in a spectral data-set.
c& rjs
c: uv analysis
c+
c	UVLIN separates line and continuum in a spectral visibility
c	data-set. It does this by fitting a low order polynomial to the
c	real and imaginary parts of the line free channels of each spectrum.
c	This polynomial function is taken to represent the continuum, and
c	any deviation from this is taken to represent the line data.
c
c	Optionally a constraint can be added that the line shape is constant,
c	and its strength is proportional to the continuum level. This is a good
c	approximation in many recombination line experiments. In this case,
c	an iterative algorithm is used to estimate the line shape and to
c	optimally disentangle the line and the continuum.
c
c	See the related tasks CONTSEN and CONTERR, which give estimates
c	of the noise amplification and the residual continuum.
c@ vis
c	The name of the input uv data set. No default.
c@ select
c	The normal uv selection commands. The default is to select everything.
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	The default is all channels. Note that if there are multiple
c	spectral windows, the fitting process is performed on each window
c	separately.
c@ chans
c	This specifies the channel ranges that contain only continuum
c	(line free). It consists of a number of pairs, each pair giving
c	a start and end channel. These are relative to the channels
c	selected with the "line" parameter. The default is that all channels
c	are line-free, which is quite a good approximation if the line is
c	weak compared to the continuum.
c@ out
c	The name of the output uv data-set. There is no default name.
c	The output can be either the line, continuum, or fitted line shape,
c	as specified by the options keyword. The default is to write the
c	line data.
c@ order
c	Order of polynomial used to fit the continuum. The default is 1
c	(i.e. a linear fit). Possible values are 0 to 5. Also see the
c	`relax' option.
c@ offset
c	An offset (in arcsec) that would shift the data to the center of the
c	dominant emission. Giving this shift allows UVLIN to reduce the
c	error involved in approximating the continuum as a polynomial.
c	The shift information is only used in the fitting process -- the
c	output data is not shifted. The default is 0,0 (no shift).
c	The following options determine what is written to the output
c	data-set.
c@ mode
c	This determines what is written out. The default is to write out
c	the line data. The following are possible values:
c	   'line'        The output data-set is the line data. This is
c	                 the default.
c	   'fit'         The output data-set is the fitted line shape data.
c	                 This is valid only when using the 'lpropc' option.
c	   'chan0'       The output data-set is an average of all the
c	                 continuum data in a visibility record.
c	   'continuum'   The output data-set is the continuum data. This
c	                 has the same number of channels as the input
c	                 data-set.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreivated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   'sun'         With this options, the OFFSET keyword is ignored,
c	                 and a shift appropriate for the Sun is determined
c	                 This can be useful to eliminate solar interference.
c	   'twofit'      If either the OFFSET parameter is set, or the SUN
c	                 option invoked, this options instructs UVLIN to
c	                 perform a simultaneous polynomial fit for a emission
c	                 both at the phase centre and at the appropriate
c	                 offset.
c	   'lpropc'      The line shape is constant throughout the data-set,
c			 and the intensity is proportional to the continuum
c	                 strength. This cannot be used together with
c	                 the 'twofit' option.
c	   'relax'       Normally UVLIN attempts to avoid overfitting the
c	                 data by reducing the order of the fit. It does this
c	                 if a significant number of channels are flagged
c	                 (more than 40% bad) or if it believes there is
c	                 inadequate data to perform the fit (less than
c	                 5*(order+1) channels). You can overwrite this
c	                 conservatism, if you know what you are doing, with
c	                 the `relax' option.
c	The following options can be used to turn off calibration corrections.
c	The default is to apply any calibration present.
c	   'nocal'       Do not apply the gains table.
c	   'nopass'      Do not apply bandpass corrections. It is unwise
c	                 to turn off bandpass correction, as the continuum
c	                 estimation process will be confused by a bandpass
c	                 which is not flat.
c	   'nopol'       Do not apply polarization corrections.
c--
c  History:
c    rjs  11aug93 Original version
c    rjs  24aug93 Added shift option.
c    rjs   5nov93 Better poly fitter? Increase MAXORDER.
c    nebk 16nov93 Remove Stokes keyword.
c    rjs  23dec93 Poly fitter does not handle zeroth order!! Make this a
c		  special case.
c   rjs    8mar94 Shift option was not shifting data back to the original
c		  position.
c   rjs   14mar94 Sun and twofit options.
c   rjs   16aug94 Zeroth order fits were failing again!
c   rjs   17aug94 Slightly better handling of offset value.
c   rjs    9sep94 Handle felocity linetype.
c   rjs   19sep04 Handle varying jyperk.
c   rjs   18sep05 More rigorously avoid mixed use of arrays.
c  Bugs:
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer MAXCH,MAXORDER
	character version*(*)
	parameter(MAXCH=32,MAXORDER=11)
	parameter(version='UvLin: version 1.0 28-Sep-05')
c
	logical sun,twofit,relax,lpropc,cflags(MAXCHAN)
	character uvflags*16,out*64,ltype*32,mode*12
	integer chans(2,MAXCH),nch,order,i,j
	integer lIn,lOut
	double precision shift(2)
c
c  Externals.
c
	logical uvDatOpn
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(sun,twofit,lpropc,relax,uvflags)
	call GetMode(mode)
	call uvDatInp('vis',uvflags)
	call keya('out',out,' ')
	call mkeyi('chans',chans,2*MAXCH,nch)
	call keyi('order',order,1)
	call keyd('offset',shift(1),0.d0)
	call keyd('offset',shift(2),0.d0)
	call keyfin
c
c  Check the inputs.
c
	if(out.eq.' ')call bug('f','An output must be given')
	if(mod(nch,2).ne.0)
     *	  call bug('f','Incomplete channel ranges given')
	nch = nch / 2
	if(order.lt.0.or.order.gt.MAXORDER)
     *	  call bug('f','The order must be in the range [0,11]')
	if(mode.eq.'fit'.and..not.lpropc)
     *	  call bug('f','Cannot write a fit without options=lpropc')
c
c  Convert the shift to radians.
c
	shift(1) = pi/180/3600 * shift(1)
	shift(2) = pi/180/3600 * shift(2)
	if(abs(shift(1))+abs(shift(2)).gt.0.and.sun)call bug('w',
     *    'The OFFSET parameter is ignored for OPTIONS=SUN')
	if(abs(shift(1))+abs(shift(2)).eq.0.and..not.sun)then
	  if(twofit)call bug('w',
     *	    'OPTIONS=TWOFIT ignored when no offset given')
	  twofit = .false.
	endif
	if(twofit.and.lpropc)call bug('f',
     *	  'OPTIONS=TWOFIT,LPROPC cannot be used together')
c
c  Determine the array giving continuum-only channels.
c
	if(nch.gt.0)then
	  do i=1,MAXCHAN
	    cflags(i) = .false.
	  enddo
	  do j=1,nch
	    do i=max(chans(1,j),1),min(chans(2,j),MAXCHAN)
	      cflags(i) = .true.
	    enddo
	  enddo
	else
	  do i=1,MAXCHAN
	    cflags(i) = .true.
	  enddo
	endif
c
c  Open the inputs and the outputs.
c
	if(.not.uvDatOpn(lIn))
     *	  call bug('f','Failed to open the input data-set')
	call uvDatGta('ltype',ltype)
	call VarInit(lIn,ltype)
c
	call uvopen(lOut,out,'new')
	call hdcopy(lIn,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'UVLIN: Miriad '//version)
	call hisinput(lOut,'UVLIN')
	call hisclose(lOut)
	call VarOnit(lIn,lOut,ltype)
	if(mode.eq.'chan0')call VarAvAll(lOut,.true.)
c
c  Do the work.
c
	if(lpropc)then
	  call lineprop(lIn,lOut,mode,order,relax,sun,shift,
     *						cflags,MAXCHAN)
	else
	  call uvlsf(lIn,lOut,mode,order,relax,sun,shift,twofit,
     *						cflags,MAXCHAN)
	endif
c
c  All said and done. Close up shop.
c
	call uvDatCls
	call uvclose(lOut)
	end
c************************************************************************
	subroutine GetMode(mode)
c
	implicit none
	character mode*(*)
c
c  Determine what we want to write out.
c------------------------------------------------------------------------
	integer nout
	integer nopt
	parameter(nopt=4)
	character opts(nopt)*9
	data opts/'line     ','fit      ','chan0    ','continuum'/
c
	call keymatch('mode',nopt,opts,1,mode,nout)
	if(nout.eq.0) mode = 'line'
	end
c************************************************************************
	subroutine GetOpt(sun,twofit,lpropc,relax,uvflags)
c
	implicit none
	logical lpropc,relax,sun,twofit
	character uvflags*(*)
c
c  Determine extra processing options.
c
c  Output:
c    sun
c    twofit
c    lpropc
c    relax
c    uvflags
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=7)
	logical present(NOPTS)
	character opts(NOPTS)*9
c
	data opts/'lpropc   ','relax     ','nocal    ','nopol    ',
     *	          'nopass   ','sun      ','twofit   '/
c
	call options('options',opts,present,NOPTS)
	lpropc = present(1)
	relax  = present(2)
	sun = present(6)
	twofit = present(7)
c
c  Determine the flags to pass to the uvDat routines.
c    d - Data selection.
c    l - Linetype processing.
c    s - Stokes processing.
c    b - Input must be a single file.
c    c - Apply gain table.
c    e - Apply leakage correction.
c    f - Apply bandpass correction.
c
	uvflags = 'dlb'
	if(.not.present(3))uvflags(5:5) = 'c'
	if(.not.present(4))uvflags(6:6) = 'e'
	if(.not.present(5))uvflags(7:7) = 'f'
c
	end
c************************************************************************
	subroutine lineprop(lIn,lOut,mode,order,relax,sun,shift,
     *							cflags,mchan)
c
	implicit none
	integer lIn,lOut
	integer order,mchan
	logical cflags(mchan),relax,sun
	character mode*(*)
	double precision shift(2)
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXPOL,MAXSPECT
	parameter(MAXPOL=4,MAXSPECT=16)
	integer npol,poltype(MAXPOL),nvis,nchan,nspect,nschan(MAXSPECT)
	integer lScr
	complex line(MAXCHAN*MAXPOL)
	logical lflags(MAXCHAN*MAXPOL)
	real inttime
c
c  Externals.
c
	character itoaf*8
c
c  Open a scratch file.
c
	call scropen(lScr)
c
c  Read the data.
c
	call SpecRd(lIn,lScr,nvis,nchan,nspect,nschan,MAXSPECT,
     *			sun,shift,npol,poltype,MAXPOL,inttime)
	call output('Number of visibilities:     '//itoaf(nvis))
	call output('Number of spectral windows: '//itoaf(nspect))
	call output('Number of channels:         '//itoaf(nchan))
	call output('Number of polarizations:    '//itoaf(npol))
	if(nchan.gt.mchan)call bug('f','Too many channels')
c
c  Estimate the spectra.
c
	call SpecEst(lScr,nvis,nchan,npol,nspect,nschan,order,relax,
     *	  line,lflags,cflags)
	call ScrClose(lScr)
c
c  Now write out the result.
c
	if(mode.eq.'fit')then
	  call SpecWr1(lIn,lOut,line,lflags,nchan,npol,poltype,inttime)
	else
	  call SpecWr2(lIn,lOut,line,lflags,nchan,npol,poltype,mode,
     *				sun,shift,order,relax,nspect,nschan)
	endif
	end
c************************************************************************
	subroutine SpecWr2(lIn,lOut,line,lflags,nchan,npol,poltype,
     *			mode,sun,shift,order,relax,nspect,nschan)
c
	implicit none
	integer lIn,lOut,nchan,npol,poltype(npol),nspect,order
	integer nschan(nspect)
	complex line(nchan,npol)
	logical lflags(nchan,npol),relax,sun
	character mode*(*)
	double precision shift(2)
c
c  Write out the continuum.
c
c------------------------------------------------------------------------
	integer PolMin,PolMax
	parameter(PolMin=-8,PolMax=4)
	include 'maxdim.h'
	integer i,nread,pol,j,nout
	double precision preamble(4),shft(2)
	complex data(MAXCHAN)
	logical dflags(MAXCHAN)
	integer pindx(PolMin:PolMax)
c
	call uvrewind(lIn)
c
c  Initialise polarisation index.
c
	do i=PolMin,PolMax
	  pindx(i) = 0
	enddo
	do i=1,npol
	  pindx(poltype(i)) = i
	enddo
c
c  Create the output stuff.
c
	call ContIni
	call uvDatRd(preamble,data,dflags,MAXCHAN,nread)
	call coInit(lIn)
	call coCvt(lIn,'ow/ow',shift,'op/op',shft)
	call coFin(lIn)
	dowhile(nread.eq.nchan)
	  call uvDatGti('pol',pol)
	  j = 0
	  if(pol.ge.PolMin.and.pol.le.PolMax) j = pindx(pol)
	  if(j.gt.0)then
	    call VarCopy(lIn,lOut)
	    call PolCpy(lOut)
	    call ProcAll(lIn,preamble,data,dflags,line(1,j),lflags(1,j),
     *		nchan,nspect,nschan,order,relax,sun,shft,.false.,
     *		mode,nout)
	    call uvwrite(lOut,preamble,data,dflags,nout)
	  endif
	  call uvDatRd(preamble,data,dflags,MAXCHAN,nread)
	enddo
	call ContRep
c
	end
c************************************************************************
	subroutine SpecWr1(lIn,lOut,line,lflags,nchan,npol,poltype,
     *							    inttime)
c
	implicit none
	integer lIn,lOut,nchan,npol,poltype(npol)
	complex line(nchan,npol)
	logical lflags(nchan,npol)
	real inttime
c
c  Write the fit to the spectrum.
c------------------------------------------------------------------------
	include 'maxdim.h'
	double precision preamble(4)
	complex data(MAXCHAN)
	logical dflags(MAXCHAN)
	integer i,j,nread
c
c  Read a record just so we position the input at the right place.
c
	call uvrewind(lIn)
	call uvDatRd(preamble,data,dflags,MAXCHAN,nread)
c
c  Create the output stuff.
c
	call VarCopy(lIn,lOut)
	if(inttime.gt.0)call uvputvrr(lOut,'inttime',inttime/npol,1)
	call uvputvri(lOut,'npol',npol,1)
	preamble(1) = 0
	preamble(2) = 0
	preamble(4) = 257
c
c  Write the spectra.
c
	do j=1,npol
	  call uvputvri(lOut,'pol',poltype(j),1)
	  do i=1,nchan
	    data(i) = line(i,j) - 1
	  enddo
	  call uvwrite(lOut,preamble,data,lflags(1,j),nchan)
	enddo
c
	end
c************************************************************************
	subroutine SpecEst(lScr,nvis,nchan,npol,nspect,nschan,order,
     *	  relax,line,lflags,cflags)
c
	implicit none
	integer lScr,nvis,nchan,npol,nspect,nschan(nspect),order
	complex line(nchan,npol)
	logical lflags(nchan,npol),cflags(nchan),relax
c
c  Assuming that the spectrum is proportional to the continuum,
c  determine the spectrum.
c
c  Input:
c    cflags	Flags indicating which channels are purely continuum.
c    sun	Whether to shift to the Sun's position.
c  Output:
c    line
c    lflags
c------------------------------------------------------------------------
	include 'maxdim.h'
	real tol
	integer MAXPOL,MAXITER,NOOFF
	parameter(MAXPOL=4,tol=1e-4,MAXITER=30,NOOFF=1)
c
	integer i,j,k,iter,offset,npt
	real epsi
	logical first
	character string*64
	complex tc
	complex da(MAXCHAN,MAXPOL),data(MAXCHAN),cont(MAXCHAN)
	logical dflags(MAXCHAN)
	real    aa(MAXCHAN,MAXPOL)
	integer n
	complex fac
c  Initialise the spectra.
c
	do j=1,npol
	  do i=1,nchan
	    line(i,j) = 1
	    lflags(i,j) = .true.
	  enddo
	enddo
c
c  Loop.
c
	first = .true.
	epsi = 1
	iter = 0
	dowhile(epsi.gt.tol.and.iter.lt.MAXITER)
	  iter = iter + 1
c
c  Initialise the accumulators.
c
	  do j=1,npol
	    do i=1,nchan
	      da(i,j) = 0
	      aa(i,j) = 0
	      if(cflags(i)) line(i,j) = 1
	    enddo
	  enddo
c
c  Loop through all the data.
c  Get the data.
c
	  offset = 0
	  do k=1,nvis
	    call SpecGet(lScr,offset,nchan,j,data,dflags)
c
c  Given the spectrum, fit the continuum. On the first time through, use
c  only the continuum channels.
c
	    if(first)then
	      call ContFit(data,dflags,cont,line(1,j),cflags,
     *			nchan,nspect,nschan,order,relax,.false.)
	    else
	      call ContFit(data,dflags,cont,line(1,j),lflags(1,j),
     *			nchan,nspect,nschan,order,relax,.false.)
	    endif
c
c  Accumulate the spectrum.
c
	     do i=1,nchan
	      if(dflags(i))then
		da(i,j) = da(i,j) + data(i)*conjg(cont(i))
		aa(i,j) = aa(i,j) + real(cont(i))**2 + aimag(cont(i))**2
	      endif
	    enddo
	  enddo
c
c  We have finished reading through the data. Now we have to determine the
c  spectra. Loop over the polarisations. Determine the flags of the line.
c  If its OK, determine the new estimate of the line.
c
	  epsi = 0
	  npt = 0
	  do j=1,npol
	    n = 0
	    fac = 0
	    do i=1,nchan
	      lflags(i,j) = aa(i,j).gt.0
	      if(lflags(i,j))then
		da(i,j) = line(i,j) * da(i,j) / aa(i,j)
		if(cflags(i))then
		  fac = fac + da(i,j)
		  n = n + 1
		endif
	      endif
	    enddo
c
	    if(n.le.0)call bug('f','No continuum channels')
	    fac = n / fac
	    do i=1,nchan
	      if(lflags(i,j))then
		da(i,j) = fac * da(i,j)
		tc = da(i,j) - line(i,j)
		line(i,j) = line(i,j) + tc
		if(.not.cflags(i))then
		  epsi = epsi + real(tc)**2 + aimag(tc)**2
		  npt = npt + 1
		endif
	      endif
	    enddo
	  enddo
	  if(npt.gt.0) epsi = sqrt(epsi/npt)
c
c  Keep the user amused and loop.
c
	  write(string,'(a,i2,a,f6.4)')'Iteration ',iter,', Error ',epsi
	  call output(string)
	  first = .false.
	enddo
c
	if(epsi.gt.tol)
     *	  call bug('w','Failed to converge ... finishing anyway')
	end
c************************************************************************
	subroutine SpecGet(lScr,offset,nchan,j,data,dflags)
c
	implicit none
	integer lScr,offset,nchan,j
	complex data(nchan)
	logical dflags(nchan)
c
c  Read in a record from the scratch file.
c
c  Input:
c    lScr
c    nchan
c  Input/Output:
c    offset
c  Output:
c    j		Polarisation index.
c    data	The data.
c    dflags	The data flags.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real d(3*MAXCHAN+1)
	integer i,i0
c
	call ScrRead(lScr,d,offset,3*nchan+1)
	j = nint(d(1))
	offset = offset + 3*nchan + 1
c
	i0 = 2
	do i=1,nchan
	  data(i) = cmplx(d(i0),d(i0+1))
	  dflags(i) = d(i0+2).gt.0
	  i0 = i0 + 3
	enddo
c
	end
c************************************************************************
	subroutine SpecRd(lIn,lScr,nvis,nchan,nspect,nschan,maxspect,
     *			sun,shift,npol,poltype,maxpol,inttime)
c
	implicit none
	integer lIn,lScr,nvis,nchan,nspect,maxspect,npol,maxpol
	integer nschan(maxspect),poltype(maxpol)
	logical sun
	real inttime
	double precision shift(2)
c
c  Read the data, and write it to a scratch file.
c
c  Input:
c    lScr
c    lIn
c    shift	Shift to apply to the data.
c    sun
c    maxspect	Maximum number of spectral windows allowed.
c    maxpol	Maximum number of polarisations allowed.
c  Output:
c    nvis
c    nchan
c    nspect
c    nschan
c    npol
c    poltype
c    inttime	Total integration time.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer PolMin,PolMax
	parameter(PolMin=-8,PolMax=4)
	integer pindx(PolMin:PolMax),i,j,offset,vupd,nread,pol
	complex data(MAXCHAN),w(MAXCHAN),unit(MAXCHAN)
	logical dflags(MAXCHAN),doshift
	real d(3*MAXCHAN+1),t
	double precision preamble(4),shft(2)
c
c  Is a shift to be performed?
c
	doshift = sun.or.abs(shift(1))+abs(shift(2)).gt.0
c
c  Initialise the polarisation index.
c
	npol = 0
	do i=PolMin,PolMax
	  pindx(i) = 0
	enddo
c
c  Read the first record. Determine the spectral window setup. Ignore
c  subsequent changes in the spectral window setup.
c
	call dspecini(lIn,vupd)
	call uvDatRd(preamble,data,dflags,MAXCHAN,nchan)
	call dspect(lIn,vupd,nchan,maxspect,nspect,nschan)
	nread = nchan
	inttime = 0
c
	do i=1,nchan
	  unit(i) = 1
	enddo
c
	call coInit(lIn)
	call coCvt(lIn,'ow/ow',shift,'op/op',shft)
	call coFin(lIn)
c
c  Keep track of the polarisations, and write the data to a scratch file.
c
	nvis = 0
	offset = 0
	dowhile(nread.eq.nchan)
	  call uvrdvrr(lIn,'inttime',t,0.)
	  inttime = inttime + t
	  call uvDatGti('pol',pol)
	  if(pol.ge.PolMin.and.pol.le.PolMax)then
	    if(pindx(pol).eq.0)then
	      npol = npol + 1
	      if(npol.gt.maxpol)
     *		call bug('f','Too many polarisations')
	      poltype(npol) = pol
	      pindx(pol) = npol
	    endif
	    if(doshift)then
	      call ShiftMod(sun,lIn,preamble,unit,nchan,shft,w)
	      do j=1,nchan
		Data(j) = conjg(w(j)) * Data(j)
	      enddo
	    endif
	    d(1) = pindx(pol)
	    i = 2
	    do j=1,nchan
	      d(i)   = real(Data(j))
	      d(i+1) = aimag(Data(j))
	      d(i+2) = 1
	      if(.not.dflags(j))d(i+2) = -1
	      i = i + 3
	    enddo
	    call scrwrite(lScr,d,offset,1+3*nchan)
	    offset = offset + 1 + 3*nchan
	    nvis = nvis + 1
	  endif
	  call uvDatRd(preamble,data,dflags,MAXCHAN,nread)
	enddo
c
	if(nread.ne.0)call bug('f','Number of channels changed')
c
	end
c************************************************************************
	subroutine uvlsf(lIn,lOut,mode,order,relax,sun,shift,twofit,
     *							cflags,mchan)
c
	implicit none
	integer lIn,lOut
	integer order,mchan
	logical cflags(mchan),relax,sun,twofit
	double precision shift(2)
	character mode*(*)
c
c  Perform visibility-based continuum subtraction.
c
c  Inputs:
c
c------------------------------------------------------------------------
	integer MAXSPECT
	include 'maxdim.h'
	parameter(MAXSPECT=16)
	integer vupd,nchan,nspect,nschan(MAXSPECT),i,nout
	double precision preamble(4),shft(2)
	complex data(MAXCHAN),line(MAXCHAN)
	logical dflags(MAXCHAN)
c
c  Initialise the spectral shape -- which is 1 in the continuum channels.
c
	do i=1,MAXCHAN
	  line(i) = 1
	enddo
c
c  Determine the line type.
c
	call dspecini(lIn,vupd)
c
c  Read through the data.
c
	call ContIni
	call uvDatRd(preamble,data,dflags,MAXCHAN,nchan)
	call coInit(lIn)
	call coCvt(lIn,'ow/ow',shift,'op/op',shft)
	call coFin(lIn)
c
	dowhile(nchan.gt.0)
	  if(nchan.gt.mchan)call bug('f','Too many channels')
	  call dspect(lIn,vupd,nchan,MAXSPECT,nspect,nschan)
	  call VarCopy(lIn,lOut)
	  call PolCpy(lOut)
	  call ProcAll(lIn,preamble,data,dflags,line,cflags,
     *	    nchan,nspect,nschan,order,relax,sun,shft,twofit,mode,nout)
	  call uvwrite(lOut,preamble,data,dflags,nout)
	  call uvDatRd(preamble,data,dflags,MAXCHAN,nchan)
	enddo
	call ContRep
c
	end
c************************************************************************
	subroutine ProcAll(lIn,preamble,data,dflags,line,lflags,
     *	  nchan,nspect,nschan,order,relax,sun,shift,twofit,mode,nread)
c
	implicit none
	integer lIn,nchan,nspect,nschan(nspect),nread,order
	complex data(nchan),line(nchan)
	logical dflags(nchan),lflags(nchan),relax,sun,twofit
	double precision shift(2)
	double precision preamble(4)
	character mode*(*)
c
c  Do the real work.
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex cont(MAXCHAN),w(MAXCHAN),ctemp
	logical doshift
	integer i,ntemp
c
c  Generate the model of the spectrum. This is the initial model
c  possibly multiplied by a phase factor.
c
	doshift = abs(shift(1))+abs(shift(2)).gt.0.or.sun
	if(doshift)then
	  call ShiftMod(sun,lIn,preamble,line,nchan,shift,w)
	else
	  do i=1,nchan
	    w(i) = line(i)
	  enddo
	endif
c
c  Fit the continuum.
c
	call ContFit(data,dflags,cont,w,lflags,
     *		nchan,nspect,nschan,order,relax,twofit)
c
c  Generate the line output.
c
	if(mode.eq.'line')then
	  do i=1,nchan
	    data(i) = data(i) - cont(i)
	  enddo
	  nread = nchan
c
c  Generate the output channel-0 file.
c
	else if(mode.eq.'chan0')then
	  ctemp = 0
	  ntemp = 0
	  do i=1,nchan
	    if(dflags(i))then
	      ctemp = ctemp + cont(i)
	      ntemp = ntemp + 1
	    endif
	  enddo
	  dflags(1) = ntemp.gt.0
	  if(dflags(1))then
	    data(1) = ctemp / ntemp
	  else
	    data(1) = 0
	  endif
	  nread = 1
c
c  Generate the output continuum.
c
	else if(mode.eq.'continuum')then
	  do i=1,nchan
	    data(i) = cont(i)
	  enddo
	  nread = nchan
	else
	  call bug('f','Unrecognised mode in ProcAll')
	endif
c
	end
c************************************************************************
	subroutine ShiftMod(sun,lIn,preamble,line,nchan,shift,w)
c
	implicit none
	integer lIn,nchan
	double precision preamble(4)
	logical sun
	double precision shift(2)
	complex w(nchan),line(nchan)
c
c  Apply a shift to the model of the spectrum.
c
c  Input:
c    sun	Shift to the location of the Sun.
c    shift	The (l,m) of the new phase centre.
c    line	The unshifted model spectrum.
c    nchan	Number of channels.
c    preamble	u,v,time,bl of the visibility.
c  Output:
c    w		The shifted model.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer i
	real theta,theta0
	double precision sfreq(MAXCHAN)
c
c  Determine the fringe rate.
c
	if(sun)then
	  call ShiftSun(lIn,preamble,theta0)
	else
	  theta0 = 2*pi * (shift(1)*preamble(1) + shift(2)*preamble(2))
	endif
c
c  Get the sky frequency and set the rotation factors.
c
	call uvinfo(lIn,'sfreq',sfreq)
	do i=1,nchan
	  theta = theta0 * sfreq(i)
	  w(i) = cmplx(cos(theta),sin(theta)) * line(i)
	enddo
c
	end
c************************************************************************
	subroutine ShiftSun(tno,preamble,theta0)
c
	implicit none
	integer tno
	double precision preamble(4)
	real theta0
c
c  Determine the fringe rate of the Sun.
c
c  Input:
c    tno	Handle of the visibility dataset.
c    preamble	Normal preamble (u,v,t,bl)
c  Output:
c    theta0	Fringe rate.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	double precision ras,decs,antpos(3*MAXANT)
	double precision obsra,obsdec,lst,ha
	double precision sinHA,cosHA,sinDEC,cosDEC
	double precision u,v,w,l,m,n,bx,by,bz
	integer nants,i1,i2
c
c  Get the antenna positions, the LST, apparent RA and DEC.
c
	call basant(preamble(4),i1,i2)
	call uvgetvrd(tno,'obsra',obsra,1)
	call uvgetvrd(tno,'obsdec',obsdec,1)
	call uvgetvrd(tno,'lst',lst,1)
	ha = lst - obsra
	call uvgetvri(tno,'nants',nants,1)
	if(nants.gt.MAXANT.or.min(i1,i2).lt.1
     *			  .or.max(i1,i2).gt.nants)
     *	  call bug('f','Invalid antenna number in ShiftSun')
	call uvgetvrd(tno,'antpos',antpos,3*nants)
c
	sinHA = sin(ha)
	cosHA = cos(ha)
	sinDEC = sin(obsdec)
	cosDEC = cos(obsdec)
c
c  Determine the u-v-w coordinates in light-nanosecs.
c
	bx = antpos(i2) - antpos(i1)
	by = antpos(i2+nants) - antpos(i1+nants)
	bz = antpos(i2+2*nants) - antpos(i1+2*nants)
c
	u = bx*sinHA + by*cosHA
	v = (-bx*cosHA + by*sinHA)*sinDEC + bz*cosDEC
	w = ( bx*cosHA - by*sinHA)*cosDEC + bz*sinDEC
c
c  Get the apparent position of the Sun.
c
	call sunradec(preamble(3),ras,decs)
c
c  Determine the direction cosines.
c
	l = sin(obsra-ras)*cos(decs)
	m = cos(obsra-ras)*cos(decs)*sinDEC - sin(decs)*cosDEC
	n = cos(obsra-ras)*cos(decs)*cosDEC + sin(decs)*sinDEC
c
c  The following four statements are written in this fashion to work
c  around a bug in the Sun compiler.
c
	theta0 = u*l
	theta0 = theta0 + v*m
	theta0 = theta0 + w*(n-1)
	theta0 = -2*pi*theta0
c
	end
c************************************************************************
	subroutine PolCpy(lOut)
c
	implicit none
	integer lOut
c------------------------------------------------------------------------
	integer npol,pol
	real jyperk
c
	call uvDatGti('npol',npol)
	if(npol.eq.0)
     *	    call bug('f','Could not determine number of polarisations')
	call uvDatGti('pol',pol)
	call uvDatGtr('jyperk',jyperk)
	call uvputvri(lOut,'npol',npol,1)
	call uvputvri(lOut,'pol',pol,1)
	call uvputvrr(lOut,'jyperk',jyperk,1)
	end
c************************************************************************
	subroutine dspecini(lIn,vupd)
c
	implicit none
	integer lIn,vupd
c------------------------------------------------------------------------
	character ltype*16
c
	call uvDatGta('ltype',ltype)
	if(ltype.eq.'channel')then
	  call uvVarini(lIn,vupd)
	  call uvVarSet(vupd,'nschan')
	else if(ltype.eq.'velocity'.or.ltype.eq.'felocity')then
	  vupd = 0
	else
	  call bug('f','Unrecognised or unsupported linetype '//ltype)
	endif
	end
c************************************************************************
	subroutine dspect(lIn,vupd,nchan,maxspect,nspect,nschan)
c
	implicit none
	integer lIn,vupd,nchan,maxspect,nspect,nschan(MAXSPECT)
c
c  Determine the number of channels in each spectral window.
c
c  Input:
c    lIn
c    vupd
c    nchan
c    maxspect
c  Output:
c    nspect
c    nschan
c------------------------------------------------------------------------
	integer MSPECT
	parameter(MSPECT=16)
	double precision line(6)
	integer start,step,n,nschand(MSPECT),ispect
c
c  Externals.
c
	logical uvVarUpd
c
	if(vupd.eq.0)then
	  nspect = 1
	  nschan(1) = nchan
	else if(uvVarupd(vupd))then
	  call uvinfo(lIn,'line',line)
	  n = nint(line(2))
	  start = nint(line(3))
	  step  = nint(line(5))
	  call uvrdvri(lIn,'nspect',nspect,1)
	  if(nspect.eq.1)then
	    nschan(1) = n
	  else
	    if(nspect.gt.MSPECT)
     *		call bug('f','Too many spectral windows')
	    call uvgetvri(lIn,'nschan',nschand,nspect)
	    ispect = 1
	    nspect = 0
	    dowhile(n.gt.0)
	      dowhile(start.gt.nschand(ispect))
	        start = start - nschand(ispect)
	        ispect = ispect + 1
	      enddo
	      nspect = nspect + 1
	      if(nspect.gt.maxspect)
     *		call bug('f','Too many sepctral windows')
	      nschan(nspect) = min((nschand(ispect)-start)/step+1,n)
	      n = n - nschan(nspect)
	      start = start + step*nschan(nspect)
	    enddo
	  endif
	endif
	end
c************************************************************************
	subroutine ContIni
c
	implicit none
c
c  Initialise counters.
c
c------------------------------------------------------------------------
	integer MAXORDER
	parameter(MAXORDER=11)
	integer Count(MAXORDER+1),Fail
	common/ContCom/Count,Fail
c
	integer i
c
	Fail = 0
	do i=1,MAXORDER+1
	  Count(i) = 0
	enddo
c
	end
c************************************************************************
	subroutine ContRep
c
	implicit none
c
c  Report the outcome of the fitting.
c
c------------------------------------------------------------------------
	integer MAXORDER
	parameter(MAXORDER=11)
	integer Count(MAXORDER+1),Fail
	common/ContCom/Count,Fail
c
	integer i,id
	character order*4,orders(0:3)*3
c
	character itoaf*8
c
	data orders/'0th','1st','2nd','3rd'/
c
	if(Fail.gt.0)call output(
     *	  'Visibilities where continuum fit failed: '//itoaf(Fail))
c
	do i=1,MAXORDER+1
	  if(Count(i).gt.0)then
	    id = i - 1
	    if(id.le.3)then
	      order = orders(id)
	    else
	      write(order,'(i2,a)')id,'th'
	    endif
	    call output('Visibilities where '//order//
     *		' order fit used: '//itoaf(Count(i)))
	  endif
	enddo
c
	end
c************************************************************************
	subroutine ContFit(data,dflags,cont,line,lflags,
     *				nchan,nspect,nschan,order,relax,twofit)
c
	implicit none
	integer nchan,nspect,nschan(nspect),order
	logical dflags(nchan),lflags(nchan),relax,twofit
	complex data(nchan),cont(nchan),line(nchan)
c
c  Given data and a model of the spectrum, determine a new model which is
c  a poly times the old model.
c
c  Input:
c    data	The raw data.
c    line	The model of the spectrum.
c    lflags	Flags associated with the spectral model.
c    nchan	Total number of channels.
c    nspect	Number of spectral windows.
c    nschan	Number of channels in each spectral window.
c    order	Polynomial order to fit.
c    relax	Give the user what she/he really asked for.
c  Input/Output:
c    dflags	On input, these are the flags associated with the data.
c		On output, more flags could be set to bad to indicate that
c		the spectrum could not be determined at this channel.
c  Output:
c    cont	The spectral estimate.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXORDER
	parameter(MAXORDER=11)
c
	integer Count(MAXORDER+1),Fail
	common/ContCom/Count,Fail
c
	integer i0,i,j,npt,npos,npar
	logical ok
	complex coeff1(0:MAXORDER),coeff2(0:MAXORDER)
	complex y(MAXCHAN),l(MAXCHAN)
	integer x(MAXCHAN)
	real f
c
	i0 = 1
	do j=1,nspect
c
c  Extract all the good data.
c
	  call ContExt(data(i0),dflags(i0),line(i0),lflags(i0),
     *					nschan(j),npt,npos,x,y,l)
c
c  Determine the number of free parameters that we are going to solve for.
c
	  npar = order + 1
	  if(twofit)npar = 2*npar
	  if(npt.gt.0.and..not.relax)then
	    f = real(npt)/real(npos)
	    dowhile(npar.gt.0.and.f.lt.0.6)
	      npar = npar - 1
	      f = f/0.6
	    enddo
	    npar = min(npar,npt,MAXORDER+1)
	    if(twofit)npar = 2*(npar/2)
	  endif
	  if(npar.gt.MAXORDER+1)call bug('f','Order too large')
c
c  Fit the spectrum if we have enough data.
c
	  if(npt.le.npar.or.npar.le.0)then
	    ok = .false.
	  else if(twofit)then
	    npar = npar/2
	    call ContPol2(nschan(j),npt,x,y,l,npar-1,coeff1,coeff2,ok)
	  else
	    call ContPol1(nschan(j),npt,x,y,l,npar-1,coeff1,ok)
	    do i=0,npar-1
	      coeff2(i) = 0
	    enddo
	  endif
c
c  If the fit was ok, generate the continuum, otherwise blank the output.
c
	  if(ok)then
	    Count(npar) = Count(npar) + 1
	    call ContGen(cont(i0),nschan(j),
     *					npar-1,coeff1,coeff2,line(i0))
	  else
	    Fail = Fail + 1
	    do i=i0,i0+nschan(nspect)-1
	      dflags(i) = .false.
	      cont(i) = 0
	    enddo
	  endif
c 
c  Go back for another spectral window.
c
	  i0 = i0 + nschan(j)
	enddo
c
	end
c************************************************************************
	subroutine ContExt(data,dflags,line,lflags,nchan,npt,npos,x,y,l)
c
	implicit none
	integer nchan,npt,npos,x(nchan)
	complex data(nchan),line(nchan),y(nchan),l(nchan)
	logical dflags(nchan),lflags(nchan)
c
c  Extract the good channels that we have a line for.
c------------------------------------------------------------------------
	integer i
c
	npt = 0
	npos = 0
	do i=1,nchan
	  if(lflags(i))npos = npos + 1
	  if(dflags(i).and.lflags(i))then
	    npt = npt + 1
	    x(npt) = i
	    y(npt) = data(i)
	    l(npt) = line(i)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine ContGen(d,nchan,order,coeff1,coeff2,line)
c
	implicit none
	integer nchan,order
	complex d(nchan),line(nchan),coeff1(0:order),coeff2(0:order)
c
c  Generate the continuum.
c------------------------------------------------------------------------
	integer i,j
	real a,b
c
	do i=1,nchan
	  d(i) = coeff2(order) + coeff1(order)*line(i)
	enddo
c
	a = 2.0/real(nchan-1)
	b = 0.5*(nchan+1)
c
	do j=order-1,0,-1
	  do i=1,nchan
	    d(i) = d(i)*a*(i-b) + coeff2(j) + coeff1(j)*line(i)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine ContPol2(nchan,npt,x,y,l,order,coeff1,coeff2,ok)
c
	implicit none
	integer npt,x(npt),order,nchan
	complex y(npt),l(npt),coeff1(0:order),coeff2(0:order)
	logical ok
c
c  Determine the coefficients of the fit to the spectrum. We fit for
c  the sum of a constant and a model.
c
c  Input:
c    npt	Number of points.
c    x		The index of the data.
c    order	Order of the polynomial fit.
c
c    l		The model spectrum.
c    y		On input, it contains the data.
c  Output:
c    coeff	Polynominal coefficients of the polynomial.
c    ok		True if the fit was successful. Otherwise false.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXORDER,MAXSIZE
	parameter(MAXORDER=11,MAXSIZE=2*(MAXORDER+1))
	integer ifail,i,j,n,i0
	real beta(MAXSIZE*MAXSIZE),a,b,p
	real alpha(MAXSIZE),eqr(MAXSIZE),eqi(MAXSIZE)
	integer pivot(MAXSIZE)
c
	a = 2.0/real(nchan-1)
	b = 0.5*(nchan+1)
	n = 4*(order+1)
	if(n.gt.MAXSIZE)call bug('f','Cannot work it out')
c
c  Initialise the solver matrix.
c
	call LlsquIni(alpha,Beta,n)
c
c  Generate the equations.
c
	do j=1,npt
	  i0 = 1
	  p = 1
	  do i=0,order
	    eqr(i0)   =   real(l(j))*p
	    eqr(i0+1) = -aimag(l(j))*p
	    eqr(i0+2) = p
	    eqr(i0+3) = 0
	    eqi(i0)   =  aimag(l(j))*p
	    eqi(i0+1) =   real(l(j))*p
	    eqi(i0+2) = 0
	    eqi(i0+3) = p
	    i0 = i0 + 4
	    p = p * a * (x(j) - b)
	  enddo
	  call LlsquAcc( real(y(j)),eqr,alpha,Beta,1,n)
	  call LlsquAcc(aimag(y(j)),eqi,alpha,Beta,1,n)
	enddo
c
c  Solve.
c
	call LlsquSol(alpha,Beta,n,ifail,pivot)
	ok = ifail.eq.0
c
c  Return the solutions.
c
	if(ok)then
	  do i=0,order
	    coeff1(i) = cmplx(alpha(4*i+1),alpha(4*i+2))
	    coeff2(i) = cmplx(alpha(4*i+3),alpha(4*i+4))
	  enddo
	endif
c
	end
c************************************************************************
	subroutine ContPol1(nchan,npt,x,y,l,order,coeff,ok)
c
	implicit none
	integer npt,x(npt),order,nchan
	complex y(npt),l(npt),coeff(0:order)
	logical ok
c
c  Determine the coefficients of the fit to the spectrum. We are given
c  a model spectrum, "l", and we find a poly to multiply the model
c  by to give a fit of the spectrum.
c
c  Input:
c    npt	Number of points.
c    x		The index of the data.
c    order	Order of the polynomial fit.
c
c    l		The model spectrum.
c    y		On input, it contains the data.
c  Output:
c    coeff	Polynominal coefficients of the polynomial.
c    ok		True if the fit was successful. Otherwise false.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXORDER
	parameter(MAXORDER=11)
	integer ifail,i
	real rnorm,a,b
	real wv(MAXCHAN),xv(MAXCHAN),yv(MAXCHAN)
	real wrk1(2*(MAXORDER+1)),wrk2(4*MAXCHAN)
	real rcoeff(0:MAXORDER),icoeff(0:MAXORDER)
	complex sum
	real sumwt
c
c  Handle zeroth order as a special case (wpfit cannot handle zeroth order!).
c
	if(order.eq.0)then
	  sum = 0
	  sumwt = 0
	  do i=1,npt
	    sum = sum + conjg(l(i))*y(i)
	    sumwt = sumwt + real(l(i))**2 + aimag(l(i))**2
	  enddo
	  coeff(0) = sum / sumwt
	  ok = .true.
	  return
	endif
c
c  HAve we enough space?
c
	if(npt.gt.MAXCHAN.or.order.gt.MAXORDER)
     *	  call bug('f','Problem too big, in ContPoly')
c
c  Determine the weights.
c
	a = 2.0/real(nchan-1)
	b = 0.5*(nchan+1)
	do i=1,npt
	  xv(i) = a*(x(i)-b)
	  wv(i) = real(l(i))**2 + aimag(l(i))**2
	enddo
c
c  The real part.
c
	do i=1,npt
	  yv(i) = real(y(i)/l(i))
	enddo
	call wpfit(order,npt,xv,yv,wv,rcoeff,rnorm,wrk1,wrk2,ifail)
	ok = ifail.eq.0
	if(.not.ok)return
c
c  The imaginary part.
c
	do i=1,npt
	  yv(i) = aimag(y(i)/l(i))
	enddo
	call wpfit(order,npt,xv,yv,wv,icoeff,rnorm,wrk1,wrk2,ifail)
	ok = ifail.eq.0
	if(.not.ok)return
c
c  Return the complex coefficients.
c
	do i=0,order
	  coeff(i) = cmplx(rcoeff(i),icoeff(i))
	enddo
c
	end
