c************************************************************************
	program uvflux
c
	implicit none
c
c= UvFlux -- Determine some statistics about visibilities.
c& rjs
c: uv-analysis
c+
c	UvFlux is a MIRIAD task which gives a mean visibility value.
c	The visibilities are averaged together regardless of time, (u,v)
c	coordinate, frequency or baseline. Distinct polarisations
c	and sources, however, are averaged separately. This task will be
c	most useful when examining the fluxes of point sources.
c
c	Both vector and scalar averages are formed. It also prints out
c	the rms scatter around the vector mean, and the RMS variation
c	in the amplitude.
c
c	Definitions in Detail: UvFlux prints out six quantities.
c
c	Theoretical RMS -- This is the rms error, resulting from
c	receiver thermal noise, that would be expected in the real or
c	imaginary part of each visibility.
c
c	Vector Average -- This is simply the normal average (mean)
c	of the real and imaginary parts of the visibility data. For a
c	point source, the real part should give the point source flux
c	density, and the imaginary part should be noise.
c
c	RMS Scatter -- This is the RMS scatter of real and imaginary
c	parts of the actual visibilities around their mean value. If the
c	data do represent a point source, and it is well calibrated, this
c	should be the same number as the ``Theoretical RMS''.
c
c	Average Amplitude -- This gives the mean value of the visibilty
c	amplitude (the so-call amplitude scalar average). If the S/N
c	ratio is much less than 1, this average amplitude will be dominated
c	by thermal noise bias, and should be 1.25 times larger than the
c	``Theoretical RMS''. If the S/N ratio is much greater than 1, then
c	this should be a good estimate of the point-source's flux density.
c	This will be unaffected by phase calibration errors (unlike the
c	``Vector Average'', which decorrelates).
c
c	RMS Amp Scatter -- This gives the RMS scsatter of the visibility
c	amplitudes around the mean visibility amplitude. If the S/N ratio
c	is much less than 1, then this should be 0.65 times the ``Theoretical
c	RMS'. If the S/N ratio is much greater than 1, this should be the
c	same as ``Theoretical RMS''.
c
c	Number Corrs -- This gives the number of correlations, N, used in
c	forming each mean. NOTE that the three RMS values printed are RMS
c	difference between the correlations and some mean -- it is not
c	an error-of-the-mean. To convert an RMS to an error-in-the-mean,
c	divide by sqrt(N).
c
c@ vis
c	The name of the input visibility data-set. Several files can be
c	given, wildcarding is supported. No default.
c@ select
c	Standard uv selection. The default is all data.
c@ line
c	Standard line-type specification. When there are multiple channels
c	selected, uvflux averages them all together.
c@ stokes
c	Normal Stokes processing. You can select several Stokes or
c	polarisation parameters, which will be be averaged independently.
c@ offset
c	An offset (arcsec) to shift the uv data. The sign convention is the
c	same as INVERT, MAXFIT, UVFIT, etc. The default is 0,0 (no shift).
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  nocal    Do not apply any antenna gain calibration corrections.
c	           By default these are applied if they exist.
c	  nopol    Do not apply polarisation leakage corrections. By default
c	           these are applied if they exist.
c	  nopass   Do not apply bandpass corrections. By default these
c	           are applied if they exist.
c--
c  History:
c    rjs   5mar93 Original version.
c    rjs  21jul93 Add printing of the theoretical noise.
c    rjs  24aug93 Added the "shift" parameter.
c    rjs   5nov93 Avoid divide-by-zero problem when data are identically 0.
c    rjs  16nov93 Do not do planet processing.
c    rjs   4may94 Use double precision, better doc, print number visibs.
c    rjs  17aug94 Handle offsets somewhat better.
c    rjs  09mar97 CHange label "visibs" to "corrs" and change doc file.
c    rjs  12oct98 Changed printing format.
c  Bugs:
c    ?? Perfect?
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer MAXPOL,MAXSRC,PolMin,PolMax
	character version*(*)
	parameter(MAXPOL=4,MAXSRC=256,PolMin=-9,PolMax=4)
	parameter(version='UvFlux: version 1.0 12-Oct-98')
c
	character uvflags*16,polcode*2,line*132
	logical docal,dopol,dopass,found,doshift
	character sources(MAXSRC)*12,source*12
	double precision fluxr(MAXPOL,MAXSRC),fluxi(MAXPOL,MAXSRC)
	double precision amp(MAXPOL,MAXSRC),amp2(MAXPOL,MAXSRC)
	double precision rms2(MAXPOL,MAXSRC)
	double precision shift(2),shft(2)
	complex vecaver
	real vecscat,scalscat,temp,vecamp,vecpha,scalamp,sig2
	integer i,j,t,nlines
	integer ncnt(MAXPOL,MAXSRC)
	integer PolIndx(PolMin:PolMax),p(MAXPOL),pp(MAXPOL)
	integer nsrc,npol,isrc,ipol,vsource,tno
c
	integer nchan
	double precision preamble(4)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
c
c  Externals.
c
	logical uvDatOpn,uvVarUpd
	character PolsC2P*2
c
c  Get the user parameters.
c
	call output(version)
	call keyini
	call GetOpt(docal,dopol,dopass)
c
c  Determine the shift.
c
	call keyd('offset',shift(1),0.d0)
	call keyd('offset',shift(2),0.d0)
	doshift = abs(shift(1))+abs(shift(2)).gt.0
c
c Determine uvDat parameters flags.
c s: stokes processing, d: data selection, l: linetype
c c: gain calibration,  e: pol calib,      f: bandpass calibration.
c
	uvflags = 'sdl'
	if(docal)  uvflags(5:5) = 'c'
	if(dopol)  uvflags(6:6) = 'e'
	if(dopass) uvflags(7:7) = 'f'
	call uvDatInp('vis',uvflags)
c
	call keyfin
c
c  Initialise.
c
	isrc = 0
	nsrc = 0
	npol = 0
	do i=PolMin,PolMax
	  PolIndx(i) = 0
	enddo
c
c  Convert the shift to radians.
c
	shift(1) = pi/180/3600 * shift(1)
	shift(2) = pi/180/3600 * shift(2)
c
c  Loop the loop until we have no more files.
c
	dowhile(uvDatOpn(tno))
	  call uvVarIni(tno,vsource)
	  call uvVarSet(vsource,'source')
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
c
c  Determine the polarisation.
c
	    call uvDatGti('pol',ipol)
	    if(PolIndx(ipol).eq.0)then
	      npol = npol + 1
	      if(npol.gt.MAXPOL)
     *		call bug('f','Too many polarisations')
	      PolIndx(ipol) = npol
	    endif
	    ipol = PolIndx(ipol)
c
c  Determine the source number. Has the source variable changed.
c  If so, check whether this was a real change. If it was, search
c  out list of known sources for it. If it was not found, initialise
c  a slot for a new source.
c
	    if(uvVarUpd(vsource))then
	      if(doshift)then
		call coInit(tno)
		call coCvt(tno,'ow/ow',shift,'op/op',shft)
		call coFin(tno)
	      endif
	      call uvrdvra(tno,'source',source,' ')
	      found = .false.
	      if(isrc.gt.0)found = source.eq.sources(isrc)
	      if(.not.found)then
		isrc = 0
		dowhile(.not.found.and.isrc.lt.nsrc)
		  isrc = isrc + 1
		  found = sources(isrc).eq.source
		enddo
	      endif
	      if(.not.found)then
		nsrc = nsrc + 1
		if(nsrc.gt.MAXSRC)
     *		  call bug('f','Too many sources')
		sources(nsrc) = source
		do i=1,MAXPOL
		  fluxr(i,nsrc) = 0
		  fluxi(i,nsrc) = 0
		  amp(i,nsrc)  = 0
		  amp2(i,nsrc) = 0
		  rms2(i,nsrc) = 0
		  ncnt(i,nsrc) = 0
		enddo
		isrc = nsrc
	      endif
	    endif
c
c  Shift the data if necessary.
c
	    if(doshift)call Shiftit(tno,preamble,data,nchan,shft)
c
c  Get the rms noise.
c
	    call uvDatGtr('variance',sig2)	    
c
c  Accumulate the data.
c
	    do i=1,nchan
	      if(flags(i))then
		fluxr(ipol,isrc) = fluxr(ipol,isrc) + real(data(i))
		fluxi(ipol,isrc) = fluxi(ipol,isrc) + aimag(data(i))
		rms2(ipol,isrc) = rms2(ipol,isrc) + sig2
		temp = abs(data(i))
		amp(ipol,isrc)  = amp(ipol,isrc) + temp
		amp2(ipol,isrc) = amp2(ipol,isrc) + temp*temp
		ncnt(ipol,isrc) = ncnt(ipol,isrc) + 1
	      endif
	    enddo
c
c  Loop the loop.
c
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvDatCls
	enddo
c
c  Determine the order that we will print the polarisations out in.
c
	npol = 0
	do j=PolMin,PolMax
	  if(PolIndx(j).gt.0)then
	    npol = npol + 1
	    p(npol) = j
	    pp(npol) = PolIndx(j)
	    do i=npol,2,-1
	      if(abs(p(i)).lt.abs(p(i-1)))then
		t = p(i)
		p(i) = p(i-1)
		p(i-1) = t
		t = pp(i)
		pp(i) = pp(i-1)
		pp(i-1) = t
	      endif
	    enddo
	  endif
	enddo
c
c  Print out the results.
c
	nlines = 0
	call output('---------------------------------------------'//
     *		'-----------------------------------')
	call output('Source     Pol Theoretic   Vector Average'//
     *		'      RMS      Average  RMS Amp  Number')
	call output('                  RMS        (real,imag) '//
     *		'    Scatter      Amp    Scatter  Corrs')
	call output('------     --- -------- -----------------'//
     *		'--- -------  --------- --------  ------')

c
	do isrc=1,nsrc
	  source = sources(isrc)
	  do i=1,npol
	    ipol = pp(i)
	    if(ncnt(ipol,isrc).gt.0)then
	      PolCode = PolsC2P(p(i))
	      fluxr(ipol,isrc) = fluxr(ipol,isrc) / ncnt(ipol,isrc)
	      fluxi(ipol,isrc) = fluxi(ipol,isrc) / ncnt(ipol,isrc)
	      vecaver  = cmplx(real(fluxr(ipol,isrc)),
     *			       real(fluxi(ipol,isrc)))
	      vecscat  = amp2(ipol,isrc) / (2*ncnt(ipol,isrc))
     *			- 0.5*(fluxr(ipol,isrc)**2+fluxi(ipol,isrc)**2)
	      vecscat = sqrt(abs(vecscat))
	      call amphase(vecaver,vecamp,vecpha)
	      scalamp = amp(ipol,isrc) / ncnt(ipol,isrc)
	      scalscat = amp2(ipol,isrc) / ncnt(ipol,isrc)
     *			- (amp(ipol,isrc) / ncnt(ipol,isrc))**2
	      scalscat = sqrt(abs(scalscat))
	      sig2 = sqrt(rms2(ipol,isrc)/ncnt(ipol,isrc))
	      write(line,
     *		'(a,a,1pe8.1,1pe11.3,1pe11.3,1pe8.1,1pe11.3,1pe9.2,i8)')
     *		source,polcode,sig2,fluxr(ipol,isrc),
     *		fluxi(ipol,isrc),vecscat,
     *		scalamp,scalscat,ncnt(ipol,isrc)
	      call output(line)
	      source = ' '
	      nlines = nlines + 1
	    endif
	  enddo
	enddo
c
	if(nlines.eq.0)call bug('f','No valid data found')
	call output('---------------------------------------------'//
     *		'-----------------------------------')
c
	end
c************************************************************************
	subroutine Shiftit(tno,uv,data,nchan,shift)
c
	implicit none
	integer nchan,tno
	double precision uv(2)
	double precision shift(2)
	complex data(nchan)
c
c  Perform a shift on the data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer i
	real theta
	complex w
	double precision sfreq(MAXCHAN)
c
c  Get the sky frequency.
c
	call uvinfo(tno,'sfreq',sfreq)
c
c  Perform the shift.
c
	do i=1,nchan
	  theta = -2*pi * (shift(1)*uv(1) + shift(2)*uv(2)) * sfreq(i)
	  w = cmplx(cos(theta),sin(theta))
	  data(i) = w * data(i)
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(docal,dopol,dopass)
c
	implicit none
	logical docal,dopol,dopass
c
c  Outputs:
c    docal	Apply calibration corrections.
c    dopol	Apply polarisation leakage corrections.
c    dopass	Apply bandpass corrections.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=3)
	character opts(NOPT)*8
	logical present(NOPT)
	data opts/'nocal   ','nopol   ','nopass  '/
c
	call options('options',opts,present,NOPT)
	docal = .not.present(1)
	dopol = .not.present(2)
	dopass= .not.present(3)
	end
