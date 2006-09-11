c************************************************************************
	program invert
	implicit none
c
c  A Miriad map making program. This uses a grid and FFT approach. The
c  gridding function is Schwabs spheroidal function, with a
c  support of 6x6 cells.
c
c  History:
c    rjs        89  Initial version
c    nebk  29apr89  Added option to shift map centre from phase centre.
c    nebk  31may89  Change IMSIZE to two dimensions and
c                   put third dimension of image into LINE.
c    rjs   27jun89  Protected the case of cdelt3 being left as zero.
c    nebk  27jul89  Fixed bug which confused fwhmx and fwhmy.
c    rjs   16aug89  Improved some formatting.
c    rjs   18oct89  Support of uv selection. Added needed changes to support
c		    the new calling sequence for planet rotation/scaling.
c    rjs    7nov89  Increased line buffer size in HISTORY.
c    rjs   13nov89  Multiple input files, systemp weighting, better history
c		    comments.
c    rjs   14feb90  Handle multi-pointing files. More checking that source,
c		    channel velocity, ra and dec remain constant. Corrected pi!
c		    Replaced velocalc with uvfit. More statistics.
c    rjs   22feb90  Corrected bug which did not discard out of range
c		    visibilities. Cosmetic changes.
c    rjs   23mar90  Changes to support applying calibration on the fly.
c		    Version number.
c    rjs   28mar90  Changed the default of JyperK.
c    rjs   29mar90  Corrected spelling mistake "sytemp" in the options list.
c    pjt    2may90  included maxdim.h in getvis for maxchan
c    mchw  21may90  Better error messages.
c    mchw  10Jul90  Worked on documentation.
c    rjs   16oct90  Checks that the data is cross-correlation data.
c    mchw  09Nov90  Added pbfwhm to map header.
c    mchw  20dec90  Added theoretical rms noise to history.
c			Minor docs and dots in AppWts.
c    rjs   10jan91  An extra check for zero visibilities to map.
c    rjs    5feb91  New call sequence to the uvdat routines. Able to
c		    map one stokes parameter. "sloppy" option.
c    rjs   15mar91  Grid beam the same time as the maps. Multiple
c		    pols/stokes at a time. Bandwidth synthesis.
c    rjs   18apr91  Fixed documentation comment. Changed a common block.
c		    Corrected call sequence of hclose.
c    rjs   23apr91  Reverted default line-type to 1 channel only.
c    rjs    1may91  Doc improvements (??) and an extra user message.
c    rjs   11jun91  More doc improvements (??) and extra user messages.
c    rjs   28jun91  Flag to perform polarisation leakage correction.
c    rjs    3jul91  Changes to checking for data in files, to appease
c		    Lauren. Changed crval1,crval2 to double to appease pjt.
c    rjs   28aug91  An extra check of the input parameters.
c    nebk  30aug91  Improve documentation for options=mfs
c    rjs   12sep91  Slight change for systemp weighting.
c    rjs   19sep91  Check if the output files already exit.
c    rjs   18feb92  Documentation enhancement to appease lgm.
c    rjs   18mar92  Better memory allocation.
c    rjs    3apr92  Calls to MemBuf.
c    nebk  05may92  Tell user when finished
c    rjs   26may92  Write btype keyword. Change spectral index sign convention
c		    for mfs beams.
c    mchw  09jun92  Check RA & DEC change lt 1% of cdelt's. Improve doc shift.
c    rjs   11jun92  More doc changes.
c    rjs   25jun92  Single channel gets labelled with frequency.
c    rjs    1jul92  Doc changes only.
c    rjs   27jul92  Fiddles with the velocity/frequency labelling, and on
c		    where the linetype is retrieved from.
c    mchw  14aug92  Changed systemp weighting to include JyperK.
c    rjs   26aug92  Add nopass option.
c    rjs   29aug92  Add "slow" and "vslow" options.
c    rjs   25sep92  Recalculate the bandwidth often. Better description
c		    of systemp weighting.
c    mchw  11feb93  Read uvvariables ra, dec as double precision.
c    rjs   29mar93  Use uvinfo(...,'variance'...) to get rms. Fix erroneous
c		    calls to uvrdvrd.
c    rjs   29jun93  Tell user whats going wrong when uvinfo fails to
c		    determine variance, when systemp weighting used.
c    mchw  39jun93  Option to make imaginary image for non Hermitian data.
c    rjs    1jul93  Doc changes and merge of mchw/rjs versions.
c    rjs   21jul93  Get rid of calls to uvinfo(..,'frequency'...). Better
c		    error messages (suggested by Lauren Likkel). Noise
c		    fiddle (use uvDatGtr(..'variance'..).
c    rjs   23jul93  Only write pbfwhm parameter if its valid.
c    rjs   24aug93  Change "shift" to "offset", to be consistent.
c    rjs   31aug93  vsloppy option.
c    rjs   24sep93  Long time bug dealing with insufficient space in the
c		    weight array under certain conditions.
c    rjs    8oct93  Increase buffer size.
c    rjs   15nov93  Image sizes do not need to be powers of 2. Double option.
c    rjs   13jan93  Use double precision to avoid roundoff error, when
c		    beam scale factor.
c    rjs    9aug94  Remember if its an E-W array. Also minor change to
c		    usage of dra, to bring it into line with whats written
c		    in the uv var "bible".
c    rjs   11aug94  Better scaling for sloppy and vsloppy options. Also
c		    describe this in the help.
c
c  Bugs:
c    ?? Perfect??
c
c= invert - Transform visibility data into a map
c& rjs
c: map making
c+
c	INVERT is a MIRIAD task which forms a map from visibility data
c	via a convolutional gridding and FFT approach. Multiple Stokes
c	maps as well as multiple frequency/velocity channels can be made
c	in one run.
c
c	By default the weight used for each visibility is proportional to
c	the integration time divided by the density of visibility points.
c	See the SUP parameter to adjust how the density function is calculated.
c	See the OPTIONS parameter to weight according to the nominal system
c	temperature.
c@ vis
c	Input visibility data files. Several files can be given. No default.
c@ map
c	Output map file name. Each output file consists of a single
c	polarization/Stokes parameter. If several different pols/Stokes
c	maps are being made, then several file names should be given. No
c	default.
c@ beam
c	Output beam file name. Default is not to make the beam.
c@ imsize
c	The size of the output image in pixels. Two values can be given,
c	giving the image size in RA and DEC. If only one value
c	is given, the output is a square image. At least one value must
c	be given. INVERT increases the sizes to the next power of two (if
c	they were not initially a power of 2). No default.
c@ offset
c	An offset (arcsec) to shift the image center away from the observing
c	center. Positive values result in the image center being to the
c	North and East of the observing center.  If one value is given, both
c	RA and DEC are shifted by this amount.  If two values are given,
c	then they are the RA and DEC shifts. Default is 0,0 (no shift).
c@ cell
c	Image cell size, in arcsec. If two values are given, they give
c	the RA and DEC cell sizes. If only one value is given, the cells
c	are made square. No default.
c@ fwhm
c	Full width at half maximum, in arcsec, of a gaussian which
c	represents the typical resolution of interest in the source. If
c	two values are given, they are used as the fwhm in RA and DEC
c	respectively. If one value is given, it is used for both RA and
c	DEC. This parameter is used to determine the uv-taper to apply to
c	the data to optimize the signal to noise for sources of that
c	particular angular size. Set FWHM to zero if you want the full
c	resolution of the data. Default is 0.
c
c	If you are more accustomed to giving this parameter in the uv plane
c	(as AIPS requires), then:
c	  fwhm(image plane) = 182 / fwhm(uv plane)
c	where the image plane fwhm is measured in arcseconds, and the uv plane
c	fwhm is measured in kilowavelengths.
c@ sup
c	Sidelobe suppression area, given in arcseconds. This parameter
c	gives the area around a source where INVERT attempts to suppress
c	sidelobes. Two values (for the RA and DEC directions respectively)
c	can be given. If only one value is given, the suppression area is
c	made square. The default is to suppress sidelobes in an area as
c	large as the field being mapped.
c
c	The suppression area is essentially an alternate way of specifying
c	the weighting scheme being used. Suppressing sidelobes in the entire
c	field corresponds to uniform weighting (so the default corresponds to
c	uniform weighting). Natural weighting gives the best signal to noise
c	ratio, at the expense of no sidelobe suppression. Natural weighting
c	corresponds to SUP=0. Values between these extremes give a tradeoff
c	between signal to noise and sidelobe suppression, and roughly
c	correspond to AIPS ``super-uniform'' weighting.
c@ line
c	Standard "line" parameter, with the normal defaults. See the
c	help on "line" for more information.
c	More specifically, the "line" parameter consists of a string
c	followed by up to four numbers, viz:
c	  linetype,nchan,start,width,step
c	where ``linetype'' is one of "channel", "wide" and "velocity".
c	The default ``linetype'' is "channel" if spectral data is present
c	in the data-set. Otherwise the default is ``wide''.
c	If the ``mfs'' option is being used, then the default ``nchan'' is all
c	channels, otherwise the default is just the first channel.
c@ ref
c	Line type of the reference channel, specified in a similar to the
c	"line" parameter. Specifically, it is in the form:
c	  linetype,start,width
c	Before mapping, the visibility data are divided by the reference
c	channel. The default is no reference channel.
c@ select
c	This allows a subset of the uv data to be used in the mapping
c	process. See the Users Manual for information on how to specify
c	this parameter. The default is to use all data.
c	If the "stokes" keyword is used, and the input files contain
c	multiple simultaneous polarizations, then "polarization" selection
c	cannot be used. If the inputs contains multiple pointings, then
c	"dra" and "ddec" selection should be used.
c@ stokes
c	This allows the user to select the polarization/Stokes parameter
c	to be mapped. Several polarisation/Stokes parameters can be given,
c	separated by commas. Where several are given, corresponding output
c	map names (parameter `map') also need to be given. Depending on the
c	data present, possible values are: i,q,u or v (Stokes
c	parameters), ii (Stokes-I, given the assumption that the source is
c	unpolarized), xx,yy,xy or yx (linear feeds) or rr,ll,rl, or lr
c	(circular feeds).
c	The default is to perform no Stokes/polarization processing.
c	Note that if the data contains several different polarization
c	types, this default makes no sense.
c@ options
c	This gives extra processing options. Several options can be
c	given (abbreviated to uniqueness), and separated by commas:
c	  systemp  Weight each visibility in inverse proportion to the
c		   noise variance. Normally visibilities are weighted in
c	           proportion to the integration time. This weighting is in
c	           addition to weighting given by the "sup" parameter. To use
c	           this option the data-sets should have sufficient
c	           information to determine the noise variance. This means the 
c	           uv variables sdf and systemp (or wwidth and wsystemp for
c	           wide linetypes), inttime and jyperk. If some of these
c	           variables are missing (e.g. it is usual for older data-sets
c	           to be lacking jyperk), a value can be added to the data-set
c	           using tasks puthd or uvputhd.
c	           For example, assuming jyperk is 200 (a typical value for
c	           Hat Creek), you can add it with:
c	                 puthd in=dataset/jyperk value=200.0
c	  nocal    Do not apply gains table calibration to the data.
c	  nopol    Do not apply polarisation leakage corrections.
c	  nopass   Do not apply bandpass table calibration to the data.
c	  mfs      Perform multi-frequency synthesis. The causes all the
c	           channel data to be used in forming a single map. The
c	           frequency dependence of the uv coordinate is thus used to
c	           give better uv coverage and/or avoid frequency
c	           smearing. For this option to produce useful maps, the
c	           intensity change over the frequency band must be low
c	           and/or the deconvolution step has compensate for the
c	           spectral variation. See the ``sbeams'' parameter, and
c	           task MFCLEAN.   You should set the ``line'' parameter
c	           to select the channels that you wish to grid.
c	  double   Make the beam twice as big as the size given.
c	  amplitude Produce a map, using the data amplitude only. The phase
c	           is set to zero.
c	  phase	   Produce a map, using the data phase only. The amplitude
c	           of the data is set to 1.
c	  slow     Rather than use a grid-and-FFT approach, use a direct
c	           Fourier transform approach. Though this option produces
c	           fewer artefacts in the final images, be prepared to wait
c	           overnight for small jobs.
c	  vslow    The "very slow" option. This uses medians rather than sums
c	           at one point, and so should be less susceptible to bad
c	           data. Be prepared for this to take an order of magnitude
c	           more that the "slow" option -- that it, start the job
c	           before going on holidays. Maps produced using this option
c	           do not obey a convolution relationship, and so one would
c	           not necessarily expect CLEANing, etc, to work.
c	  imaginary Make imaginary image for non-Hermitian data.
c	  sloppy   Accept, for mapping, visibilities if more than 90% of the
c	           channels are good. The default is to insist that all
c	           channels are good.
c	  vsloppy  The "very sloppy" option. Accept, for mapping, visibilities
c	           if at least one channel is good. The default is to insist
c	           that all channels are good.
c
c	           NOTES when using sloppy and vsloppy options: The beam
c	           computed by INVERT assumes that all the channels used in
c	           mapping were good. This will probably not be so if the
c	           sloppy or vsloppy options were used -- in these cases the
c	           true beam (point-spread function) will vary from plane to
c	           plane, and will differ from the computed beam. You cannot
c	           do a proper deconvolution in this case. INVERT attempts to
c	           scale each plane so that the true beam has a peak value
c	           of 1. However, this scaling is exact only for naturally
c	           weighted maps and when no tapering is used (i.e. sup=0 and
c	           fwhm is unset). For other weightings and taperings, the
c	           peak value of the true beam will not be exactly 1 -- the
c	           flux density unit will no longer be what is conventionally
c	           understood by Jy/dirty beam. Additionally the beam peak value
c	           will vary from plane to plane, and so the flux density scale
c	           may not be comparable from plane to plane.
c@ sbeams
c	This parameter only has meaning when using ``options=mfs''. Two
c	values can be given. The first value determines the highest order
c	``spectral dirty beam'' that will be formed. The second gives
c	the reference frequency of the spectral dirty beams. The default order
c	is 0 (i.e. form the normal beam only), and the default reference
c	frequency is the geometric mean of the data frequencies.
c--
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Invert: version 1.0 15-Nov-93')
	integer MAXPOL
	parameter(MAXPOL=4)
	include 'maxdim.h'
	include 'mirconst.h'
	include 'invert.h'
c
c  Parameters which fix the gridding function. Though these are fixed here,
c  all subroutines called treat them as variables.
c
	integer maxgcf,width
	character func*(*)
	real alpha
	parameter(maxgcf=1024,width=6,alpha=1.0,func='spheroidal')
c
c  A buffer used for the gridding and FFT process.
c
	integer maxout
	parameter(maxout=5)
	real fwhmx,fwhmy,Supx,Supy,cellx,celly,shftx,shfty
	real Tu,Tv,gdu,gdv,wdu,wdv,umax,vmax
	real Rms,Tsys,JyperK,totint,freq0,freq1
	character vis*64,outs(maxout)*64,line*64,flags*16,slop*4
	logical Natural,Systemp,NoCal,NoPol,Amp,Phase,Mfs,NoPass
	logical Slow,VSlow,doimag,doubit
	real xcorr(maxdim),ycorr(maxdim),gcf(maxgcf)
	real slopfac(MAXCHAN*MAXPOL),Sca,Scale
	integer wnu,wnv,ngcf,nx,ny,nz,nxd,nyd,nx1,ny1,nu,nv,u0,v0
	integer nbeams,i,percent
	integer nschan(maxout),length,n(2)
	integer nvis,nstart,ncount,nstep,iostat,npol,nout,ipl,npl,nc
	integer nplanes,iout,pnt,Size
	integer tvis,tno,tscr
	real array(maxbuf)
	common array
c
c  Externals.
c
	character itoaf*8
	integer len1,MemBuf,Nextpow2
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(systemp,nocal,nopass,nopol,slop,mfs,amp,phase,
     *	  slow,vslow,doimag,doubit)
	slow = slow.or.vslow
c
c  Determine the flags to the uvDatInp routines.
c
	flags = 'xwprlds'
	length = len1(flags)
	if(.not.nocal)then
	  length = length + 1
	  flags(length:length) = 'c'
	endif
	if(.not.nopol)then
	  length = length + 1
	  flags(length:length) = 'e'
	endif
	if(.not.nopass)then
	  length = length + 1
	  flags(length:length) = 'f'
	endif
	if(.not.mfs)then
	  length = length + 1
	  flags(length:length) = '1'
	endif
	call uvDatInp('vis',flags(1:length))
c
	call keya('beam',outs(1),' ')
	call mkeya('map',outs(2),maxout-1,nout)
	if(nout.eq.0)call bug('f','An output must be given')
	nout = nout + 1
c
	call keyi('imsize',nxd,0)
	call keyi('imsize',nyd,nxd)
	if(nxd.le.0.or.nyd.le.0)
     *	  call bug('f','Bad value for imsize parameter')
	nx = nxd
	ny = nyd
	if(doubit)then
	  nx = 2*nx - 1
	  ny = 2*ny - 1
	endif
	nx = Nextpow2(nx)
	ny = Nextpow2(ny)
c
	call keyi('sbeams',nbeams,0)
	if(outs(1).eq.' '.and.nbeams.gt.0)
     *	  call bug('f','A beam name must given, if sbeams is non-zero')
	call keyr('sbeams',freq0,0.)
        call keyr('offset',shftx,0.0)
        call keyr('offset',shfty,shftx)
	call keyr('fwhm',fwhmx,0.)
	call keyr('fwhm',fwhmy,fwhmx)
	call keyr('cell',cellx,0.)
	call keyr('cell',celly,cellx)
c
	if(cellx.le.0.or.celly.le.0)
     *	   call bug('f','Negative cell size is invalid ')
	call keyr('sup',Supx,max(nx*cellx,ny*celly))
	call keyr('sup',Supy,Supx)
	call keyfin
c
c  Check that the user has given a consistent number of polarisations and
c  output files.
c
	call uvDatGti('npol',npol)
	if(npol.eq.0)npol = 1
	if(npol.ne.nout-1) call bug('f',
     *     'Bad number of maps for the requested polarizations')
c
c  Check whether the output files already exist.
c
	do i=1,nout
	  if(outs(i).ne.' ')call assertf(outs(i),.false.,
     *	    'Data-set already exists: '//outs(i))
	enddo
c
c  Convert all the junk the user input into units, etc, that are of
c  greater use to me.
c
	call Sizes(cellx,Supx,nx,Fwhmx,width,gdu,wnu,wdu,umax,Tu)
	call Sizes(celly,Supy,ny,Fwhmy,width,gdv,wnv,wdv,vmax,Tv)
	if(min(nx,ny).le.width+1)
     *	  call bug('f','Minimum image size is '//itoaf(width+1))
	if(max(nx,ny).gt.maxdim)
     *	  call bug('f','Maximum image size is '//itoaf(maxdim))
	Natural = wnu.eq.1.and.wnv.eq.1
c
c  Tell about the weighting scheme.
c
	n(1) = nint( 3600 * 180 / pi / wdu)
	n(2) = nint( 3600 * 180 / pi / wdv)
	call mitoaf(n,2,line,length)
	i = index(line(1:length),',')
	line(i:i) = 'x'
	call output('Sidelobe suppression area is '//
     *				line(1:length)//' arcsec')
	if(Natural)then
	  call output(' ... this corresponds to natural weighting')
	else if(0.99*gdu.lt.wdu.and.0.99*gdv.lt.wdv)then
	  call output(' ... this corresponds to uniform weighting')
	else
	  call output(' ... this corresponds to '//
     *			'a super-uniform weighting')
	endif
	gdu = -gdu
	wdu = -wdu
c
c  Convert cell and shifts to radians.
c
	cellx = pi/(180*3600)*cellx
	celly = pi/(180*3600)*celly
	shftx = pi/(180*3600)*shftx
	shfty = pi/(180*3600)*shfty
c
c  Put cdelt1, cdelt2 into common for use in HdCheck
c  -further rationalization would also use these in makemap
c
	cdelt1 = cellx
	cdelt2 = celly
c
c  Generate the gridding convolution and correction functions.
c
	ngcf = width*( (maxgcf-1)/width ) + 1
	call gcffun(func,gcf,ngcf,width,alpha)
	call corrfun(func,xcorr,nx,width,alpha)
	call corrfun(func,ycorr,ny,width,alpha)
c
c  Read in the visibilities.
c
	call output('Reading the visibility data ...')
	call scropen(tscr)
	if(mfs)then
	  call GetVsMFS(systemp,npol,nbeams,tscr,
     *		vis,nvis,totint,umax,vmax,freq1,doimag)
	  if(freq0.le.0)freq0 = freq1
	  nz = 1
	else
	  call GetVis(slop,slopfac,systemp,npol,tscr,
     *		vis,nz,nvis,totint,umax,vmax,doimag)
	  nbeams = 0
	endif
	call HdFin(freq0)
c
c  Wake the user.
c
	call output(  'Number of visibilities accepted: '//itoaf(nvis))
	write(line,'(a,1pg9.3)')'Total integration time (hours):',totint
	call output(line)
	if(mfs)then
	  write(line,'(a,1pg9.3)')'Mean Frequency(GHz):    ',freq1
	  call output(line)
	endif
c
c  Determine the "channels" that correspond to each output.
c
	nschan(1) = nbeams + 1
	do i=1,npol
	  nschan(i+1) = nz
	enddo
	nplanes = nbeams + npol * nz
c
c  Determine image sizes.
c
	nv = 2*int(abs(vmax/gdv) + 0.5*width) + 1
	nv = ny
	nu =   int(abs(umax/gdu) + 0.5*width) + width/2 + 1
	v0 = nv/2 + 1
	u0 = width/2 + 1
c
	wnv = 2*nint(abs(vmax/wdv)) + 3
	wnu = 2*nint(abs(umax/wdu)) + 3
c
	if(slow)then
	  nstep = 1
	  Size = max(5*nvis,wnv*(wnu/2 + 1)) 
	else
	  nstep = max(1,MemBuf()/(2*nv*nu))
	  Size = 2*nstep*nv*nu
	endif
	call memalloc(pnt,Size,'r')
c
c  Calculate the weights for uniform weighting.
c
	if(.not.Natural)then
	  call output('Calculating weights ...')
	  call CalcWts(tscr,Array(pnt),wdu,wdv,wnu,wnv,nvis,nplanes)
	endif
c
c  Apply weights, shift data and calculate statistics, then give a message
c  to that extent.
c
	call output('Applying weights and calculating statistics ...')
	call AppWts(tscr,Natural,Tu,Tv,Array(pnt),wdu,wdv,wnu,wnv,
     *	  amp,phase,nvis,nplanes,nbeams,shftx,shfty,
     *	  Rms,Tsys,JyperK,freq0)
c
	write(line,'(a,1pg9.3)')'Theoretical rms noise: ',rms
	call output(line)
	write(line,'(a,i15)')'Average Tsys:',nint(tsys)
	call output(line)
	write(line,'(a,1pg9.3)')'Average Jy/K:          ',JyperK
	call output(line)
c
c  Open the first input visibility file again, so copy the history file.
c
	call hopen(tvis,Vis,'old',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  The following rather cumbersome loop creates the beams and maps. As
c  many as possible beams and maps are gridded together, and so there
c  is some rather ugly code dealing with determining which gridded data
c  goes into which plane of which file.
c
	ipl = 0
	iout = 1
	nstart = 0
	call output('Gridding and saving images ...')
	dowhile(nstart.le.nplanes)
	  ncount = min(nplanes-nstart+1,nstep)
	  if(.not.slow)then
	    call GridVis(tscr,gcf,ngcf,width,nvis,nstart,ncount,nplanes,
     *		Array(pnt),nu,nv,u0,v0,gdu,gdv)
	    if(nstart.eq.0)call GetScale(Array(pnt),nu,nv,Scale)
	  else if(nstart.eq.0)then
	    call GetScalS(tscr,nvis,nstart,nplanes,Scale)
	  endif
	  nc = 0
	  dowhile(nc.lt.ncount)
	    npl = min(ncount-nc,nschan(iout)-ipl)
c	    if(slop.ne.'none'.and.iout.ne.1)npl = 1
	    if(outs(iout).ne.' ')then
	      if(ipl.eq.0)then
		nx1 = nxd
		ny1 = nyd
		if(doubit.and.iout.eq.1)then
		  nx1 = 2*nx1 - 1
		  ny1 = 2*ny1 - 1
		endif
		if(iout.eq.1)then
		  call makemap(tno,outs(iout),nx1,ny1,nschan(iout),
     *			0.,0.,-cellx,celly,0.,iout-1,.true.)
		else
		  call makemap(tno,outs(iout),nx1,ny1,nschan(iout),
     *			shftx,shfty,-cellx,celly,rms,iout-1,.false.)
		endif
	        call history(tvis,tno,nvis,width,func,alpha,
     *			rms,Tsys,JyperK,TotInt,version)
	      endif
	      if(slop.ne.'none'.and.iout.gt.1)then
		Sca = Scale * SlopFac(nstart + nc - nbeams)
	      else
		Sca = Scale
	      endif
	      if(slow)then
		call DirectFT(tscr,nvis,nstart,nplanes,gdu,gdv,
     *		  vslow,Array(pnt),nx1,ny1,tno,ipl+1,Sca)
	      else
	        call ProcMap(tno,Array(2*nc*nu*nv+pnt),nu,nv,u0,v0,
     *		  ipl+1,npl,nx,ny,nx1,ny1,Sca,xCorr,yCorr)
	      endif
	    endif
	    ipl = ipl + npl
	    nc = nc + npl
	    if(ipl.eq.nschan(iout))then
	      if(outs(iout).ne.' ') call xyclose(tno)
	      iout = iout + 1
	      ipl = 0
	    endif
	  enddo
	  nstart = nstart + ncount
	  if(nstart.le.nplanes)then
	    percent = (100*nstart)/(nplanes+1)
	    write(line,'(a,i3,a)')'Completed',percent,'% ...'
	    call output(line)
	  endif
	enddo
c
c  All said and done. Close up the visibility file, and go home.
c
        call output('Completed 100%')
	call memfree(pnt,Size,'r')
	call hclose(tvis)
	call scrclose(tscr)
	end
c************************************************************************
	subroutine GetOpt(systemp,nocal,nopass,nopol,slop,mfs,
     *				amp,phase,slow,vslow,doimag,doubit)
c
	implicit none
	logical systemp,nocal,nopass,nopol,mfs,amp,phase
	logical slow,vslow,doimag,doubit
	character slop*(*)
c
c  Determine extra processing options.
c
c  Output:
c    systemp	If true, do systemp weighting.
c    nocal	If true, do not apply selfcal corrections.
c    nopass	If true, do not apply bandpass corrections.
c    nopol	If true, do not apply polarisation leakage corrections.
c    slop	Either "none", "some" or "all".
c    mfs	If true, perform multi-frequency synthesis.
c    amp	If true, map only the amplitude of the data.
c    phase	If true, map only the the phase of the data.
c    slow	Use direct Fourier transform.
c    vslow	Use direct Fourier transform and medians.
c    doimag	Make a map of the imaginary part.
c    doubit	Make the beam twice as big as the map.
c-------------------------------------------------------------------------
	integer nopt
	parameter(nopt=13)
	character opts(nopt)*9
	logical present(nopt)
	data opts/'amplitude','mfs      ','nocal    ','phase    ',
     *	          'sloppy   ','systemp  ','nopol    ','nopass   ',
     *		  'slow     ','vslow    ','imaginary','vsloppy  ',
     *		  'double   '/
	call options('options',opts,present,nopt)
	amp = present(1)
	mfs = present(2)
	nocal = present(3)
	phase = present(4)
	slop = 'none'
	if(present(5)) slop = 'some'
	if(present(12))slop = 'all'
	if(mfs) slop = 'none'
	systemp = present(6)
	nopol = present(7)
	nopass = present(8)
	slow = present(9)
	vslow = present(10)
	doimag = present(11)
	doubit = present(13)
c
	if(slop.ne.'none'.and.mfs)call bug('w',
     *	  'The sloppy option has no meaning in multi-freq synthesis')
	end
c************************************************************************
	subroutine GetVsMFS(systemp,npol,nbeams,tscr,
     *		vis,nvis,totint,umax,vmax,freq0,doimag)
c
	implicit none
	logical systemp,doimag
	integer tscr,nvis,npol,nbeams
	real umax,vmax,totint,freq0
	character vis*(*)
c
c  Read a visibility file, selects the appropriate data, then writes
c  it out to a scratch file. The scratch file consists of "nvis"
c  records, each record consisting of:
c    u,v,wt,???,r1,i1,r2,i2,...rn,in
c  Here u and v are the u and v coordinates in wavelengths. If "u" is
c  positive, the data are conjugated and u and v are then negated. Wt is the
c  weight of the data, and r and i are a correlation pair.
c  This returns the abs max u and v values.
c
c  Inputs:
c    systemp	Logical. True if systemp weighting is to be applied.
c    npol	Number of polarisations to map.
c    nbeams	Number of spectral dirty beams to reserve space for.
c    tscr	Handle of the output visibility scratch file.
c  Input/Output:
c    umax,vmax	The abs maximum u and v coordinates (lambda).
c  Outputs:
c    vis	The name of the first uv file.
c    nchan	Number of channels.
c    nvis	Number of visibilities written to the scratch file.
c    totint	The total integration time (in hours!).
c    freq0	Reference frequency.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer InU,InV,InWt,InTsys,InRms,InJyperK,InFreq,InData
	parameter(InU=0,InV=1,InWt=2,InTsys=3,InRms=4,Injyperk=5,
     *		  InFreq=6,InData=8)
	integer nChk,OutSize,maxPol
	parameter(nChk=101,OutSize=(maxchan+2)*(InData+8),maxpol=4)
c
	integer i,j,offset,bad,tvis,size,mxchan,Indx(maxchan),nchan
	integer nval,off,counter
	logical flags(maxchan,maxpol),cnjgate
	complex data(maxchan,maxpol)
	real out(OutSize)
	real uumax,vvmax,wt,tsys,inttime,bw,JyperK,temp,rms2
	double precision sfreq(maxchan)
	double precision dfreq0,t,SumWt
	double precision preamble(4)
	common/scratch/ out,data,flags
c
c  Externals.
c
	character itoaf*9
c
c  Initialise.
c
	SumWt = 0
	dfreq0 = 0
	totint = 0
	nvis = 0
	counter = -NChk
	bad = 0
	offset = 0
	uumax = 0
	vvmax = 0
	mxchan = min(OutSize / (InData + 2*npol + 2*nbeams), maxchan)
	if(mxchan.le.0) call bug('f','Buffers too small, in GetVsMFS')
	if(npol.gt.maxpol) call bug('f','Too many polarizations')
c
c  Open the first file, Read the first visibility, get info, and
c  perform various checks.
c
	call FileOpen(systemp,tvis,preamble,data,flags,mxchan,nchan)
	if(nchan.eq.0) call bug('f','No visibilities to map')
	call uvDatGta('name',vis)
	call HdIni(tvis,nchan,npol,.true.,bw)
	size = InData + 2*(npol+nbeams)
c------------------------------------------------------------------------
c  The start of the main loop.
c
	dowhile(nchan.gt.0)
c
c  Read the rest of the polarization data.
c
	  do i=2,npol
	    call uvDatRd(preamble,data(1,i),flags(1,i),mxchan,nchan)
	  enddo
c
c  Determine which ones we are going to keep. Reject the entire record
c  if systemp weighting, and no value for systemp. Otherwise reject a
c  channel the uv value is out of limits, or if one of the polarisations
c  has been flagged.
c
	  call uvDatGtr('variance',rms2)
	  if(systemp.and.rms2.le.0)then
	    nval = 0
	  else
	    call uvinfo(tvis,'sfreq',sfreq)
	    preamble(1) = preamble(1) / sfreq(1)
	    preamble(2) = preamble(2) / sfreq(1)
c
	    do i=1,nchan
	      flags(i,1) = flags(i,1).and.
     *			 abs(real(preamble(1)*sfreq(i))).lt.umax.and.
     *		         abs(real(preamble(2)*sfreq(i))).lt.vmax
	    enddo
c
	    do j=2,npol
	      do i=1,nchan
		flags(i,1) = flags(i,1).and.flags(i,j)
	      enddo
	    enddo
	    call whenieq(nchan,flags,1,.true.,indx,nval)
	  endif
c
c  Copy the good ones to the output. Remember to conjugate them if necessary.
c
	  if(nval.gt.0)then
	    if(counter.ge.0)then
	      call HdCheck(tvis,nchan,bw)
	      counter = -NChk
	    endif
	    counter = counter + 1
c
	    call uvrdvrr(tvis,'systemp',tsys,0.)
	    call uvrdvrr(tvis,'inttime',inttime,60.)
	    call uvrdvrr(tvis,'jyperk',JyperK,0.)
c
	    wt = inttime
	    if(systemp) wt = 1./rms2
	    totint = totint + nval*inttime
	    SumWt = SumWt + nval * Wt
c
	    cnjgate = preamble(1).gt.0
	    if(cnjgate)then
	      preamble(1) = -preamble(1)
	      preamble(2) = -preamble(2)
	    endif
c
	    off = 1
	    do i=1,nval
	      Out(off+InU) = preamble(1) * sfreq(Indx(i))
	      Out(off+InV) = preamble(2) * sfreq(Indx(i))
	      uumax = max(uumax,abs(Out(off+InU)))
	      vvmax = max(vvmax,abs(Out(off+InV)))
	      Out(off+InWt) = Wt
	      Out(off+InRms) = rms2
	      Out(off+InJyperK) = JyperK
	      Out(off+InTsys) = Tsys
	      t = log(sfreq(Indx(i)))
	      Out(off+InFreq) = t
	      dfreq0 = dfreq0 + Wt * t
	      off = off + size
	    enddo
c
c  Copy the data across.
c
	    do j=1,npol
	      off = InData + 2*nbeams + 2*(j-1) + 1
	      do i=1,nval
	        Out(off) =   real( Data(Indx(i),j))
	        Out(off+1) = aimag(Data(Indx(i),j))
	        off = off + size
	      enddo
	    enddo
c
c  Multiple by sqrt(-1) is necessary.
c
	    if(doimag)then
	      do j=1,npol
		off = InData + 2*nbeams + 2*(j-1) + 1
		do i=1,nval
		  temp = Out(off)
		  Out(off) = -Out(off+1)
		  Out(off+1) = temp
		  off = off + size
		enddo
	      enddo
	    endif
c
c  Conjugate if necessary.
c
	    if(cnjgate)then
	      do j=1,npol
		off = InData + 2*nbeams + 2*(j-1) + 1
		do i=1,nval
		  Out(off+1) = -Out(off+1)
		  off = off + size
		enddo
	      enddo
	    endif
c
c  Write out the data to the scratch file, and update everything.
c
	    call scrwrite(tscr,out,offset,nval*size)
	    nvis = nvis + nval
	    offset = offset + nval*size
	  endif
c
c  Remember how many bad channels.
c
	  bad = bad + nchan - nval
c
c  Loop the loop. Open a new file if needed.
c
	  call uvDatRd(preamble,data,flags,mxchan,nchan)
	  if(nchan.eq.0)then
	    call uvDatCls
	    call FileOpen(systemp,tvis,preamble,data,flags,mxchan,nchan)
	    counter = 0
	  endif
	enddo
c
c  End of main loop.
c------------------------------------------------------------------------
c  Finish up.
c
	if(bad.ne.0)
     *	  call bug('w','Number of rejected visibilities: '//itoaf(bad))
	if(nvis.eq.0) call bug('f','No visibilities to map')
	freq0 = exp( dfreq0 / SumWt )
	umax = uumax
	vmax = vvmax
	totint = totint / 3600.
c
	end
c************************************************************************
	subroutine GetVis(slop,slopfac,systemp,npol,tscr,
     *		vis,nchan,nvis,totint,umax,vmax,doimag)
c
	implicit none
	logical systemp,doimag
	integer nchan,tscr,nvis,npol
	real umax,vmax,totint
	character vis*(*),slop*(*)
	real slopfac(*)
c
c  Read a visibility file, selects the appropriate data, then writes
c  it out to a scratch file. The scratch file consists of "nvis"
c  records, each record consisting of:
c    u,v,wt,???,r1,i1,r2,i2,...rn,in
c  Here u and v are the u and v coordinates in wavelengths. If "u" is
c  positive, the data are conjugated and u and v are then negated. Wt is the
c  weight of the data, and r and i are a correlation pair.
c  This returns the abs max u and v values.
c
c  Inputs:
c    slop	Either "none", "some" or "all". If "some" then accept
c		visibilities where 90% 	of the channels are OK. For "none"
c		insist that 100% of the channels have to be good to be
c		accepted. For "all", accept a visibility is some are good.
c    systemp	Logical. True if systemp weighting is to be applied.
c    npol	Number of polarisations to map.
c    tscr	Handle of the output visibility scratch file.
c  Outputs:
c    vis	The name of the first uv file.
c    nchan	Number of channels.
c    nvis	Number of visibilities written to the scratch file.
c    totint	The total integration time (in hours!).
c    slopfac	Slop factor, for options=sloppy or vsloppy.
c  Input/Output:
c    umax,vmax	The abs maximum u and v coordinates (lambda).
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer InU,InV,InWt,InTsys,InRms,InJyperK,InFreq,InData
	parameter(InU=0,InV=1,InWt=2,InTsys=3,InRms=4,Injyperk=5,
     *		  InFreq=6,InData=8)
	integer nChk
	parameter(nChk=101)
	integer i,j,nread,offset,tvis,size,badchan,nproc,counter
	integer noutside,nbadtsys,npartbad,nallbad,nzero
	logical flags(4*maxchan),ok
	real data(InData+8*maxchan),SumWt
	real uumax,vvmax,wt,tsys,inttime,bw,JyperK,temp,rms2
	double precision preamble(4)
	common/scratch/ data,flags
c
c  Externals.
c
	character itoaf*9
c
c  Initialise.
c
	totint = 0
	nvis = 0
	noutside = 0
	nbadtsys = 0
	npartbad = 0
	nallbad  = 0
	counter = -NChk
	offset = 0
	uumax = 0
	vvmax = 0
c
c  Open the first file, Read the first visibility, get info, and
c  perform various checks.
c
	call FileOpen(systemp,tvis,preamble,
     *	  data(InData+1),flags,maxchan,nchan)
	if(nchan.eq.0) call bug('f','No visibilities to map')
	call uvDatGta('name',vis)
	call HdIni(tvis,nchan,npol,.false.,bw)
c
	nread = nchan
	nproc = npol * nchan
	size = InData + 2*nproc
c
c  Initialise the slop factor array.
c
	if(slop.ne.'none')then
	  do i=1,npol * nchan
	    slopfac(i) = 0
	  enddo
	  SumWt = 0
	endif
c------------------------------------------------------------------------
c  The start of the main loop.
c
	dowhile(nread.eq.nchan)
c
c  Read the rest of the polarization data.
c
	  do i=2,npol
	    call uvDatRd(preamble,data(InData+2*(i-1)*nchan+1),
     *		flags((i-1)*nchan+1),maxchan,nread)
	  enddo
c
c  Check if we accept this data. Reject it if u or v is out of range,
c  or if too many channels are flagged bad.
c
	  ok = abs(real(preamble(1))).lt.umax.and.
     *	       abs(real(preamble(2))).lt.vmax
	  if(ok)then
	    badchan = 0
	    do i=1,nproc
	      if(.not.flags(i)) badchan = badchan + 1
	    enddo
	    ok = badchan.eq.0.or.
     *		(badchan.lt.0.1*nproc.and.slop.eq.'some').or.
     *		(badchan.lt.nproc.and.slop.eq.'all')
	    if(.not.ok)then
	      if(badchan.eq.nproc)then
	        nallbad = nallbad + 1
	      else
	        npartbad = npartbad + 1
	      endif
	    endif
	  else
	    noutside = noutside + 1
	  endif
c
c  Get systemp, rms, etc, and make sure its OK.
c
	  if(ok)then
	    call uvrdvrr(tvis,'systemp',tsys,0.)
	    call uvrdvrr(tvis,'inttime',inttime,60.)
	    call uvrdvrr(tvis,'jyperk',JyperK,0.)
	    call uvDatGtr('variance',rms2)
	    ok = .not.systemp.or.rms2.gt.0
	    if(.not.ok) nbadtsys = nbadtsys + 1
	  endif
c
c  Process the data. 
c
	  if(ok)then
c
	    if(counter.ge.0)then
	      call HdCheck(tvis,nchan,bw)
	      counter = -NChk
	    endif
	    counter = counter + 1
c
	    wt = inttime
	    if(systemp) wt = 1/rms2
	    totint = totint + inttime
c
	    data(InU+1) = preamble(1)
	    data(InV+1) = preamble(2)
	    data(InWt+1) = Wt
	    data(InRms+1) = rms2
	    data(InJyperK+1) = JyperK
	    data(InTsys+1) = Tsys
	    data(InFreq+1) = 0
c
c  Zero out bad data (sloppy options).
c
	    if(badchan.gt.0)then
	      j = InData + 1
	      SumWt = SumWt + Wt
	      do i=1,nproc
		if(.not.flags(i))then
		  data(j)   = 0
		  data(j+1) = 0
		else
		  SlopFac(i) = SlopFac(i) + Wt
		endif
		j = j + 2
	      enddo
	    else if(slop.ne.'none')then
	      do i=1,nproc
		SlopFac(i) = SlopFac(i) + Wt
	      enddo
	      SumWt = SumWt + Wt
	    endif
c
c  Make imaginary image for non Hermitian data.
c
	    if(doimag)then
	      do i=InData+1,InData+2*nproc,2
		temp = data(i)
		data(i) = -data(i+1)
		data(i+1) = temp
	      enddo
	    endif
c
c  Conjugate the data where necessary.
c
	    if(data(InU+1).gt.0)then
	      data(InU+1) = -data(InU+1)
	      data(InV+1) = -data(InV+1)
	      do i=InData+1,InData+2*nproc,2
		data(i+1) = -data(i+1)
	      enddo
	    endif
c
c  Write out the data to the scratch file, and update everything.
c
	    call scrwrite(tscr,data,offset,size)
	    nvis = nvis + 1
	    offset = offset + size
	    uumax = max(uumax,abs(real(preamble(1))))
	    vvmax = max(vvmax,abs(real(preamble(2))))
	  endif
c
c  Loop the loop. Open a new file if needed.
c
	  call uvDatRd(preamble,data(InData+1),flags,maxchan,nread)
	  if(nread.eq.0)then
	    call uvDatCls
	    call FileOpen(systemp,tvis,preamble,data(InData+1),flags,
     *						  maxchan,nread)
	    counter = 0
	  endif
	enddo
c
c  End of main loop.
c------------------------------------------------------------------------
c  Finish up.
c
	if(nread.ne.0)    call bug('f',
     *	  'Number of channels changed will reading the data')
	if(npartbad.ne.0) call bug('w',
     *	  'Visibs rejected because of partially bad records: '//
     *							itoaf(npartbad))
	if(nallbad.ne.0)  call bug('w',
     *	  'Visibs rejected because of entirely bad records: '//
     *							itoaf(nallbad))
	if(noutside.ne.0) call bug('w',
     *	  'Visibs rejected because of too large a spatial frequency: '//
     *							itoaf(noutside))
	if(nbadtsys.ne.0) call bug('w',
     *	  'Visibs rejected with bad noise variance values: '//
     *							itoaf(nbadtsys))
	if(nvis.eq.0)     call bug('f',
     *	  'No visibilities to map')
	umax = uumax
	vmax = vvmax
	totint = totint / 3600.
c
c  Determine the slop factors, if needed.
c
	if(slop.ne.'none')then
	  nzero = 0
	  do i=1,nproc
	    if(SlopFac(i).le.0)then
	      nzero = nzero + 1
	    else
	      SlopFac(i) = SumWt / SlopFac(i)
	    endif
	  enddo
	  if(nzero.gt.0)call bug('w',
     *	    'Total number of planes containing no good data: '//
     *							itoaf(nzero))
	endif
c
	end
c************************************************************************
	subroutine FileOpen(systemp,tvis,preamble,data,flags,
     *						maxchan,nread)
c
	implicit none
	integer tvis,maxchan,nread
	double precision preamble(4)
	complex data(maxchan)
	logical systemp,flags(maxchan)
c
c  This opens a visibility file, and reads the first visibility.
c  If there are no visibilities in the file, the file is closed, and
c  a new one opened.
c
c  Input:
c    maxchan	Max number of channels to read.
c    systemp	Systemp weighting is being used.
c  Outputs:
c    tvis
c    preamble	Normal preamble.
c    data	The correlation data.
c    flags	The data flags.
c    nread	The number of channels read. When we find no more
c		open files, this returns with 0.
c------------------------------------------------------------------------
	integer WIDE
	parameter(WIDE=2)
	character name*32
	real rtemp,rms2
	logical more,dowide
	double precision line(6),dtemp
c
c  Externals.
c
	logical uvDatOpn
c
	nread = 0
	more = uvDatOpn(tvis)
	dowhile(more.and.nread.eq.0)
	  call uvDatRd(preamble,data,flags,maxchan,nread)
	  if(nread.eq.0)then
	    call uvDatGta('name',name)
	    call bug('w','No data contributed by file '//name)
	    call uvDatCls
	    more = uvDatOpn(tvis)
	  endif
	enddo
c
c  Check whether systemp is computable, if systemp weighting is being used.
c  If its not, try to determine the missing variable.
c
c
	if(nread.gt.0.and.systemp)then
	  call uvDatGtr('variance',rms2)
	  if(rms2.le.0)then
	    call uvDatGta('name',name)
	    call bug('w',
     *	      'Noise variance of data are not determinable for '//name)
	    call uvrdvrr(tvis,'inttime',rtemp,0.)
	    if(rtemp.le.0)call bug('w',
     *	      '... Uv variable inttime was missing or non-positive')
	    call uvrdvrr(tvis,'jyperk',rtemp,0.)
	    if(rtemp.le.0)call bug('w',
     *	      '... Uv variable jyperk was missing or non-positive')
	    call uvinfo(tvis,'line',line)
	    dowide = nint(line(1)).eq.WIDE
	    if(dowide)then
	      call uvrdvrr(tvis,'wsystemp',rtemp,0.)
	      if(rtemp.le.0)call bug('w',
     *	        '... Uv variable wsystemp is missing or non-positive')
	      call uvrdvrr(tvis,'wwidth',rtemp,0.)
	      if(rtemp.le.0)call bug('w',
     *	        '... Uv variable wwidth is missing or non-positive')
	    else
	      call uvrdvrr(tvis,'systemp',rtemp,0.)
	      if(rtemp.le.0)call bug('w',
     *	        '... Uv variable systemp is missing or non-positive')
	      call uvrdvrd(tvis,'sdf',dtemp,0.d0)
	      if(dtemp.le.0)call bug('w',
     *	        '... Uv variable sdf is missing or non-positive')
	    endif
	    call bug('w','Set the variable(s) using puthd')
	  endif	    
	endif
	end
c************************************************************************
	subroutine HdIni(tvis,nchan,npol,mfs,bw)
c
	implicit none
	integer tvis,nchan,npol
	logical mfs
	real bw
c
c  This gets the value of a swag full of uv variables.
c
c  Input:
c    tvis	Handle of the visibility file.
c    nchan	Number of channels in the visibility file.
c    mfs	True if we are performing multi-freq synthesis.
c  Output:
c    bw		Channel bandwidth, in Hz.
c    All the variables in the invert.h commons.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer CHANNEL,WIDE,VELOCITY
	parameter(CHANNEL=1,WIDE=2,VELOCITY=3)
	include 'invert.h'
c
	integer itype
	logical ew
	double precision dtemp
	real dra,ddec,vsource
	double precision vepsi,repsi,dbw,epsi,data(6)
c
c  Determine the polarisations that could be returned.
c
	npols = npol
	if(npols.gt.maxpols)
     *	  call bug('f','Too many polarizations')
	if(npols.gt.1)then
	  call uvDatGti('pols',Pols)
	else
	  call uvDatGti('pol',pols)
	endif
c
c  Determine channel bandwidth.
c
	call uvfit1(tvis,'bandwidth',nchan,dbw,epsi)
	if(epsi.gt.0.1*dbw) call bug('w',
     *	  'Channel bandwidths differ by greater than 10%')
	if(dbw.le.0) call bug('f','Channels have zero bandwidth')
	bw = 1.0e9 * dbw
c
c  Get all the values we could possibly want.		
c
	call uvrdvrd(tvis,'ra',crval1,0.d0)
	call uvrdvrr(tvis,'dra',dra,0.)
	call uvrdvrd(tvis,'dec',crval2,0.d0)
	call uvrdvrr(tvis,'ddec',ddec,0.)
	crval1 = crval1 + dra/cos(crval2)
	crval2 = crval2 + ddec
	call uvrdvrd(tvis,'obsra',obsra,crval1)
	call uvrdvrd(tvis,'obsdec',obsdec,crval2)
	call uvrdvrr(tvis,'epoch',epoch,1950.)
	call uvrdvrr(tvis,'pbfwhm',pbfwhm,-1.)
	call uvrdvra(tvis,'source',  source,' ')
	call uvrdvra(tvis,'telescop',telescop,' ')
	call uvrdvra(tvis,'observer',observer,' ')
c
c  Is this an E-W array.
c
	ew = .false.
	if(telescop.ne.' ')call obspar(telescop,'ew',dtemp,ew)
	if(ew) ew = dtemp.gt.0
	if(ew)then
	  ctype1 = 'RA---NCP'
	  ctype2 = 'DEC--NCP'
	else
	  ctype1 = 'RA---SIN'
	  ctype2 = 'DEC--SIN'
	endif
c
c  Get the velocity of the observatory.
c
	call uvrdvrr(tvis,'vsource',vsource,0.)
	call uvrdvrr(tvis,'veldop',vobs,0.)
	vobs = vobs - vsource
c
c  Set the flags which remember changes and deviations from desired states.
c
	Vchang   = .false.
	Rchang   = .false.
	RAchang  = .false.
	DecChang = .false.
	SrcChang = .false.
	PolStat  = 0
	VLin = .true.
	Rconst = .true.
c
c  Determine channel increments and rest frequencies.
c
	restfreq = 0
	call uvinfo(tvis,'line',data)
	itype = nint(data(1))
	if(itype.eq.CHANNEL)  ltype = 'channel'
	if(itype.eq.WIDE)     ltype = 'wide'
	if(itype.eq.VELOCITY) ltype = 'velocity'
	lstart = data(3)
	lwidth = data(4)
	lstep  = data(5)
c
	if(mfs)then
	  cdelt3 = dbw
	  crval3 = 0
	  ctype3 = 'FREQ'
	else if(ltype.eq.'wide')then
	  call uvfit2(tvis,'sfreq',nchan,cdelt3,crval3,vepsi)
	  if(nchan.eq.1) cdelt3 = dbw
	  ctype3 = 'FREQ'
	else
	  call uvfit1(tvis,'restfreq',nchan,restfreq,repsi)
	  if(restfreq.le.0)then
	    call uvfit2(tvis,'sfreq',nchan,cdelt3,crval3,vepsi)
	    if(nchan.eq.1) cdelt3 = dbw
	    ctype3 = 'FREQ'
	  else if(nchan.eq.1)then
	    call uvinfo(tvis,'velocity',crval3)
	    cdelt3 = dbw * 0.001 * CMKS / restfreq
	    call uvrdvra(tvis,'veltype',ctype3,'VELO-LSR')
	  else
	    call uvfit2(tvis,'velocity',nchan,cdelt3,crval3,vepsi)
	    Vlin   = vepsi.lt.0.1*abs(cdelt3).or.vepsi.le.0
	    Rconst = repsi.lt.0.001*Restfreq
	    call uvrdvra(tvis,'veltype',ctype3,'VELO-LSR')
	  endif
	endif
	end
c************************************************************************
	subroutine HdFin(freq0)
c
	implicit none
	real freq0
c
c  This generates some error messages about things changing, not being
c  linear, or the like, while reading through the visibility file.
c
c------------------------------------------------------------------------
	include 'invert.h'
c
	if(crval3.eq.0.and.ctype3.eq.'FREQ') crval3 = freq0
c
	if(.not.Rconst) call bug('w',
     *	  'Rest frequencies varied between channels by > 0.1%')
	if(Rchang) call bug('w',
     *	  'Rest frequencies varied by > 0.1% while reading data')
	if(.not.Vlin) call bug('w',
     *	  'Channel velocities deviated by > 10% from linearity')
	if(Vchang) call bug('w',
     *	  'Channel velocities varied by > 10% while reading data')
	if(RAchang) call bug('w',
     *	  'Source RA changed while reading through the data')
	if(DECchang) call bug('w',
     *	  'Source DEC changed while reading through the data')
	if(SRCchang) call bug('w',
     *	  'The source changed while reading through the data')
	if(PolStat.eq.1) call bug('w',
     *	  'Mix of polarizations found -- source assumed unpolarized')
	if(PolStat.eq.2) call bug('w',
     *	  'Incompatible polarizations found -- map may be meaningless')
	end
c************************************************************************
	subroutine HdCheck(tvis,nchan,bw)
c
	implicit none
	integer tvis,nchan
	real bw
c
c  This checks some values of uv variables to see if they have changed.
c
c  Input:
c    tvis	Handle of the visibility file.
c    nchan	Number of channels in the visibility file.
c  Input thru invert.h commons:
c    Several variables are check to make sure they have not changed.
c  Output:
c    bw		Bandwidth of a channel.
c------------------------------------------------------------------------
	integer PolMin,PolMax,PolI
	parameter(PolMin=-6,PolMax=1,PolI=1)
	include 'invert.h'
	integer i1
	real t2
	character line*32
	double precision V0,dV,R0,vepsi,repsi,dbw,epsi,t1
	logical inten(PolMin:PolMax)
c
	data inten/.true.,.true.,.false.,.false.,.true., .true.,
     *		   .false.,.true./
c
c  Determine channel bandwidth.
c
	call uvfit1(tvis,'bandwidth',nchan,dbw,epsi)
	if(epsi.gt.0.1*dbw) call bug('w',
     *	  'Channel bandwidths differ by greater than 10%')
	if(dbw.le.0) call bug('f','Channels have zero bandwidth')
	bw = 1.0e9 * dbw
c
c  Check for changes when observing velocities.
c
	if(ctype3(1:5).eq.'VELO-')then
	  if(.not.Rchang)then
	    call uvfit1(tvis,'restfreq',nchan,R0,repsi)
	    Rconst = Rconst.and.repsi.lt.0.001*Restfreq
	    Rchang = Rchang.or.abs(Restfreq-R0).gt.0.001*Restfreq
	  endif
c
	  if(.not.Vchang)then
	    if(nchan.eq.1)then
	      call uvinfo(tvis,'velocity',v0)
	      Vchang = abs(crval3-V0).gt.0.1*abs(cdelt3)
	    else
	      call uvfit2(tvis,'velocity',nchan,dv,v0,vepsi)
	      Vlin   =
     *		Vlin  .and.(vepsi.lt.0.1*abs(cdelt3).or.vepsi.le.0)
	      Vchang = 
     *		max(abs(crval3-V0),abs(cdelt3-dV)).gt.0.1*abs(cdelt3)
	    endif
	  endif
	endif
c
	if(.not.RAchang)then
	  call uvrdvrd(tvis,'ra',t1,0.d0)
	  call uvrdvrr(tvis,'dra',t2,0.)
	  t1 = t1 + t2/cos(crval2)
	  RAchang = abs(crval1-t1).gt.0.01*abs(cdelt1)
	endif
	if(.not.DECchang)then
	  call uvrdvrd(tvis,'dec',t1,0.d0)
	  call uvrdvrr(tvis,'ddec',t2,0.)
	  t1 = t1 + t2
	  DECchang = abs(crval2-t1).gt.0.01*abs(cdelt2)
	endif
	if(.not.SRCchang)then
	  call uvrdvra(tvis,'source',line,' ')
	  SRCchang = line.ne.Source
	endif
c
	if(PolStat.ne.2.and.npols.eq.1)then
	  call uvDatGti('pol',i1)
	  if(pols(1).ne.i1)then
	    if(pols(1).lt.PolMin.or.pols(1).gt.PolMax.or.
     *	       i1.lt.PolMin.or.i1.gt.PolMax)then
	      PolStat = 2
	    else if(inten(pols(1)).and.inten(i1))then
	      Pols(1) = PolI
	      PolStat = 1
	    else
	      PolStat = 2
	    endif
	  endif
	endif
c
	end
c************************************************************************
	subroutine GetScale(Beam,nu,nv,Scale)
c
	implicit none
	integer nu,nv
	real Beam(2*nu*nv)
	real Scale
c
c  Determine the factor to scale the beam by so that it will have a peak
c  of unity after FFTing.
c
c  Input:
c    nu,nv	Size of the beam.
c    Beam	The gridded beam.
c
c  Output:
c    Scale	The flux.
c
c------------------------------------------------------------------------
	integer i
	double precision temp
c
	temp = 0
	do i=1,2*nu*nv,2
	  temp = temp + Beam(i)
	enddo
c
	Scale = 1/(2*temp)
	end
c************************************************************************
	subroutine ProcMap(tno,Grd,nu,nv,u0,v0,nstart,ncount,nx,ny,
     *	  nx1,ny1,Scale,xCorr,yCorr)
c
	implicit none
	integer tno,nu,nv,nstart,ncount,u0,v0,nx,ny,nx1,ny1
	real Scale,xCorr(nx),yCorr(ny)
	complex Grd(nv,nu,ncount)
c
c  Perform FFT, grid correction and scaling on the gridded data,
c  then write out the resultant map.
c
c  This, along with the routines called, assumes that nv==ny (the rest
c  of the software does not assume this). This is required as Grd is
c  used as a temporary to store the partially FFTed data.
c
c  Input:
c    tno	Handle of the output map.
c    nu,nv	Size of gridded visibility array.
c    ncount	Number of channels to process.
c    nstart	Index number of first map to output.
c    u0,v0	Index of pixel corresponding to (u,v)=(0,0) in the Grd array.
c    nx,ny	Size of input array. map. The Grd array is essentially blank
c		padded to make it this size.
c    nx1,ny1	Size of output map. Only the central portion is retained.
c    Scale	Scale factor to apply to map.
c    xCorr	Gridding correction function, in x.
c    yCorr	Gridding correction function, in y.
c
c  Input/Output:
c    Grd	Array containing the gridded visibility. On output,
c		this has been corrupted by the FFT process.
c
c------------------------------------------------------------------------
	integer k
c
	if(nv.ne.ny) call bug('f','Assumption failed: Nv != Ny')
c
	do k=1,ncount
	  call xysetpl(tno,1,k+nstart-1)
	  call FFTpass1(Grd(1,1,k),nu,nv,u0,v0)
	  call FFTpass2(tno,Grd(1,1,k),nu,nv,nx,ny,nx1,ny1,
     *						u0,Scale,xCorr,yCorr)
	enddo
	end
c************************************************************************
	subroutine FFTpass2(tno,Grd,nu,nv,nx,ny,nx1,ny1,
     *						u0,Scale,xCorr,yCorr)
c
	implicit none
	integer tno,nv,nu,nx,ny,u0,nx1,ny1
	real Scale,xCorr(nx),yCorr(ny)
	complex Grd(nv,nu)
c
c  Perform second pass of FFT, apply grid corrections and scaling, and
c  write out the result.
c
c  Input:
c    tno	Handle of the output map file.
c    Grd	Gridded visibility, which has been through the first
c		pass FFT.
c    nu,nv	Size of the grid array.
c    u0		Index of
c    nx,ny	Input size.
c    nx1,ny1	Output map size.
c    Scale	Scale factor to apply.
c    xCorr	)  Gridding corrections.
c    yCorr	)
c
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	complex cdata(maxdim)
	real rdata(maxdim)
	integer i,j,ilo,ihi,jlo,jhi
	real yc
c
	common/scratch/ cdata,rdata
c
c  Set the low and high portions of the image to save.
c
	ilo = nx/2 - nx1/2 + 1
	ihi = ilo + nx1 - 1
	jlo = ny/2 - ny1/2 + 1
	jhi = jlo + ny1 - 1
c
c  We have done the row FFTs. Now do the column FFTs and store the result
c  in the output array.
c
	do j=nu-u0+2,nx/2+1
	  cdata(j) = 0.
	enddo
c
	do j=jlo,jhi
c
c  Do the second pass of the FFT.
c
	  do i=u0,nu
	    cdata(i-u0+1) = Grd(j,i)
	  enddo
	  call fftcr(cdata,rdata,-1,nx)
c
c  Perform grid correction and scaling.
c
	  yc = Scale * yCorr(ny/2+1) * xCorr(nx/2+1) / yCorr(j)
	  do i=ilo,ihi
	    rdata(i) = rdata(i) * (yc/xCorr(i))
	  enddo
c
c  Write out the result.
c
	  call xywrite(tno,j-jlo+1,rdata(ilo))
	enddo
c
	end
c************************************************************************
	subroutine FFTpass1(Grd,nu,nv,u0,v0)
c
	implicit none
	integer nv,nu,u0,v0
	complex Grd(nv,nu)
c
c  This takes the gridded visibility, performs fudges on it to put it into
c  a state appreciated by the FFT routines, and then the first pass FFT.
c  The fudges it performs are,
c   1.	To reflect the small number of grid cells, with negative values of
c	u into positive values of u (by conjugating it and adding it to the
c	appropriate positive cell).
c   2.  Switch the ordering of the data (so that the pixel corresponding
c	to (u,v)=(0,0) gets shifted to pixel (1,1), and multiply by
c	(-1)**(i+j), so that the output map has the centre at pixel
c	(nx/2+1,ny/2+1).
c
c  Inputs:
c    u0,v0	Index of the pixel in the grid array corresponding to
c		(u,v) = (0,0).
c    nu,nv	Input gridded visibility data size.
c
c  Input/Output:
c    Grd	Input gridded visibility. Destroyed in the FFT process.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	integer i,j,id,jd,nd
	complex cdata(maxdim),temp
	common/scratch/ cdata
c
	nd = nv / 2
	if(v0.ne.nd+1)call bug('f','Assumption failed in FFTpass1')
c
c  Add negative part of u if necessary. The point at nd+1 is done manually
c  to avoid a vector dependency when jd = j.
c
	do j=u0,nu
	  if(j.lt.2*u0)then
	    jd = 2*u0 - j
	    id = nv
	    do i=2,nd
	      temp = Grd(i,j)  + conjg(Grd(id,jd))
	      Grd(id,j) = Grd(id,j) + conjg(Grd(i,jd))
	      Grd(i,j)  = temp
	      id = id - 1
	    enddo
	    Grd(nd+1,j) = Grd(nd+1,j) + conjg(Grd(nd+1,jd))
	  endif
c
c  Copy the data, performing the shift and multiplication by (-1)**(i+j).
c  It is not the most elegant code, but it vectorises without any help.
c
	  if(2*((j-u0)/2).eq.(j-u0))then
	    do i=1,nd,2
	      cdata(i)   =   Grd(i+nd,j)
	      cdata(i+1) = - Grd(i+nd+1,j)
	    enddo
	    do i=1,nd,2
	      cdata(i+nd)   =   Grd(i,j)
	      cdata(i+nd+1) = - Grd(i+1,j)
	    enddo
	  else
	    do i=1,nd,2
	      cdata(i)   = - Grd(i+nd,j)
	      cdata(i+1) =   Grd(i+nd+1,j)
	    enddo
	    do i=1,nd,2
	      cdata(i+nd)   = - Grd(i,j)
	      cdata(i+nd+1) =   Grd(i+1,j)
	    enddo
	  endif
c
c  Perform the FFT on this row.
c
	  call fftcc(cdata,Grd(1,j),-1,nv)
	enddo
	end
c************************************************************************
	subroutine AppWts(tscr,Natural,Tu,Tv,UWts,wdu,wdv,wnu,wnv,amp,
     *	  phase,nvis,nz,nbeams,shftx,shfty,Rms,Tsys,JyperK,freq0)
c
	implicit none
	integer tscr,wnu,wnv,nvis,nz,nbeams
	logical Natural,amp,phase
	real Tu,Tv,shftx,shfty,wdu,wdv,UWts(wnv,wnu/2+1)
	real Rms,Tsys,JyperK,freq0
c
c  Apply weights, perform phase shift, and calculate statistics.
c
c  Input:
c    tscr	Scratch file of the visibility data.
c    Natural	True if natural weighting is to be used.
c    amp	Make an amplitude-only map.
c    phase	Make a phase-only map.
c    Tu,Tv	Scale factors for determining taper.
c    UWts	If its not natural weighting, this contains the
c		uniform weight information.
c    nvis	Number of visibilities.
c    nz		Number of "channels" to map, including the nbeams.
c    nbeams	Number of spectral beams.
c    freq0	Reference frequency, when calculating spectral beams.
c    shftx,shfty Source center shift, in radians.
c  Output:
c    Rms	An estimate of the rms noise in the output map.
c    Tsys	The rms system temperature.
c    JyperK	The rms JyperK gain factor.
c------------------------------------------------------------------------
	integer InU,InV,InWt,InTsys,InRms,InJyperK,InFreq,InData
	parameter(InU=0,InV=1,InWt=2,InTsys=3,InRms=4,Injyperk=5,
     *		  InFreq=6,InData=8)
	integer maxrun
	real pi
	parameter(maxrun=4096,pi=3.141592653589793)
	real Wts(maxrun),VisBuf(maxrun),rshift(maxrun),ishift(maxrun)
	real SDB(maxrun),logFreq0
c
	real SumWt,RmsWt,TsysWt,JyperKWt,p1,p2,theta,visr,visi
	real rrshft,iishft,Wt,scale,t
	integer i,j,k,l,size,step,n,u,v
c
	size = 2*nz + InData
	step = maxrun/size
	if(step.le.0)
     *	  call bug('f','Too many channels for buffer in AppWts')
c
	if(nbeams.gt.0) logFreq0 = log(Freq0)
c
	SumWt = 0.
	RmsWt = 0.
	TsysWt = 0.
	JyperKWt = 0.
c
	do l=1,nvis,step
	  n = min(nvis-l+1,step)
	  call scrread(tscr,VisBuf,(l-1)*size,n*size)
c
c  Calculate the basic weight, either natural or pseudo-uniform.
c
	  if(Natural)then
	    k = 1
	    do i=1,n
	      Wts(i) = Visbuf(k+InWt)
	      k = k + size
	    enddo
	  else
	    k = 1
	    do i=1,n
	      u = nint(Visbuf(k+InU)/wdu)   + 1
	      v = nint(Visbuf(k+InV)/wdv) + wnv/2 + 1
	      Wts(i) = Visbuf(k+InWt) / UWts(v,u)
	      k = k + size
	    enddo
	  endif
c
c  Multiply in a taper to the weights, if necessary.
c
	  if(abs(Tu)+abs(Tv).gt.0)then
	    k = 1
	    do i=1,n
	      Wts(i) = Wts(i) * exp( Tu*Visbuf(k+InU)*Visbuf(k+InU) +
     *				     Tv*Visbuf(k+InV)*Visbuf(k+InV))
	      k = k + size
	    enddo
	  endif
c
c  Calculate the statistics.
c
	  do i=1,n
	    SumWt = SumWt + Wts(i)
	  enddo
	  call UvStat(RmsWt,Wts,Visbuf(1+InRms),size,n,2)
	  call UvStat(TsysWt,Wts,Visbuf(1+InTsys),size,n,1)
	  call UvStat(JyperKWt,Wts,Visbuf(1+InJyperK),size,n,1)
c
c  Calculcate the spectral dirty beams, if needed.
c
	  if(nbeams.gt.0)then
	    k = 1 + InFreq
	    do i=1,n
	      SDB(i) = VisBuf(k) - logFreq0
	      k = k + size
	    enddo
c
	    do j=1,nbeams
	      k = InData + 2*(j-1) + 1
	      if(j.eq.1)then
		do i=1,n
		  VisBuf(k) = Wts(i) * SDB(i)
		  VisBuf(k+1) = 0
		  k = k + size
		enddo
	      else
		scale = 1.0 / j
c#ivdep
		do i=1,n
		  VisBuf(k) = scale * VisBuf(k-2) * SDB(i)
		  VisBuf(k+1) = 0
		  k = k + size
		enddo
	      endif
	    enddo
	  endif
c
c  Replace Visbuf(k+InData-2) with the weight, so we can make a beam
c  later on. This overwrites some of the measures we have been using.
c
	  k = InData - 1
	  do i=1,n
	    Visbuf(k) = Wts(i)
	    Visbuf(k+1) = 0
	    k = k + size
	  enddo
c
c  The following sections of code have two alternate branches, one
c  when the number of channels is greater than the number of visibilities,
c  the other when the number of visibilities is greater than the number
c  of channels.
c
c  Perform amplitude processing, if required.
c
	  if(amp)then
	    if(n.gt.nz-nbeams)then
	      do j=1+nbeams,nz
		k = 2*(j-1) + InData + 1
		do i=1,n
		  t = sqrt(VisBuf(k)*VisBuf(k)+VisBuf(k+1)*VisBuf(k+1))
		  Visbuf(k) = t
		  Visbuf(k+1) =	 0
		  k = k + size
		enddo
	      enddo
	    else
	      do i=1,n
		k = size*(i-1) + InData + 1
		do j=1+nbeams,nz
		  t = sqrt(VisBuf(k)*VisBuf(k)+VisBuf(k+1)*VisBuf(k+1))
		  Visbuf(k) = t
		  Visbuf(k+1) =	 0
		  k = k + 2
		enddo
	      enddo
	    endif
	  endif
c
c  Phase processing.
c
	  if(phase)then
	    if(n.gt.nz-nbeams)then
	      do j=1+nbeams,nz
		k = 2*(j-1) + InData + 1
		do i=1,n
		  t = 1/sqrt(VisBuf(k)  *VisBuf(k)+
     *			     VisBuf(k+1)*VisBuf(k+1))
		  Visbuf(k) = t * VisBuf(k)
		  Visbuf(k+1) =	 t * VisBuf(k+1)
		  k = k + size
		enddo
	      enddo
	    else
	      do i=1,n
		k = size*(i-1) + InData + 1
		do j=1+nbeams,nz
		  t = 1/sqrt(VisBuf(k)  *VisBuf(k)+
     *			     VisBuf(k+1)*VisBuf(k+1))
		  Visbuf(k) = t * VisBuf(k)
		  Visbuf(k+1) =	t * VisBuf(k+1)
		  k = k + 2
		enddo
	      enddo
	    endif
	  endif
c
c  Work out, and apply, a phase rotation if necessary.
c
	  if(abs(shftx)+abs(shfty).gt.0)then
	    P1 = -2*pi*shftx
	    P2 = -2*pi*shfty
	    k = 1
	    do i=1,n
	      theta = P1*Visbuf(k+InU) + P2*Visbuf(k+InV)
	      rshift(i) = Wts(i) * cos(theta)
	      ishift(i) = Wts(i) * sin(theta)
	      k = k + size
	    enddo
c
	    if(n.gt.nz-nbeams)then
	      do j=1+nbeams,nz
		k = 2*(j-1) + InData + 1
		do i=1,n
		  visr = Visbuf(k)
		  visi = Visbuf(k+1)
		  Visbuf(k) =	  rshift(i)*visr - ishift(i)*visi
		  Visbuf(k+1) =	  ishift(i)*visr + rshift(i)*visi
		  k = k + size
		enddo
	      enddo
	    else
	      do i=1,n
		rrshft = rshift(i)
		iishft = ishift(i)
		k = size*(i-1) + InData + 1
		do j=1+nbeams,nz
		  visr = Visbuf(k)
		  visi = Visbuf(k+1)
		  Visbuf(k)   = rrshft*visr - iishft*visi
		  Visbuf(k+1) = iishft*visr + rrshft*visi
		  k = k + 2
		enddo
	      enddo
	    endif
c
c  Otherwise just apply the weights.
c
	  else
	    if(n.gt.nz-nbeams)then
	      do j=1+nbeams,nz
		k = 2*(j-1) + InData + 1
		do i=1,n
		  Visbuf(k) =	  Wts(i)*Visbuf(k)
		  Visbuf(k+1) =	  Wts(i)*Visbuf(k+1)
		  k = k + size
		enddo
	      enddo
	    else
	      do i=1,n
		Wt = Wts(i)
		k = size*(i-1) + InData + 1
		do j=1+nbeams,nz
		  Visbuf(k)   = Wt*Visbuf(k)
		  Visbuf(k+1) = Wt*Visbuf(k+1)
		  k = k + 2
		enddo
	      enddo
	    endif
	  endif
c
c  All done. Write out the results.
c
	  call scrwrite(tscr,Visbuf,(l-1)*size,n*size)
	enddo
c
c  Return statistics.
c
	Rms = sqrt(RmsWt)/SumWt
	JyperK = JyperKWt/SumWt
	Tsys = TsysWt/SumWt
	end
c************************************************************************
	subroutine UvStat(Stat,Wts,Measure,size,n,order)
c
	implicit none
	integer size,n,order
	real Stat,Measure(size*n),Wts(n)
c
c  Accumulate some statistics to do with uv data.
c
c  Input:
c    size
c    n
c    mesasure
c  Output:
c    stat
c------------------------------------------------------------------------
	integer i,k
c
	k = 1
	if(order.eq.1)then
	  do i=1,n
	    Stat = Stat + Wts(i)*Measure(k)
	    k = k + size
	  enddo
	else
	  do i=1,n
	    Stat = Stat + Wts(i)*Wts(i)*Measure(k)
	    k = k + size
	  enddo
	endif
	end
c************************************************************************
	subroutine CalcWts(tvis,Wts,wdu,wdv,wnu,wnv,nvis,nchan)
c
	implicit none
	integer tvis,wnu,wnv,nvis,nchan
	real Wts(wnv,wnu/2+1),wdu,wdv
c
c  Calculate the weight to be applied to each visibility.
c
c  Input:
c    tvis	Handle of the visibility scratch file.
c    wnu,wnv	Full size of the weights array.
c    wdu,wdv	Cell increments (wavelengths).
c    nvis	Number of visibilities.
c    nchan	Number of channels.
c
c  Output:
c    Wts	Array containing the visibility weights.
c
c------------------------------------------------------------------------
	integer InU,InV,InWt,InTsys,InRms,InJyperK,InFreq,InData
	parameter(InU=0,InV=1,InWt=2,InTsys=3,InRms=4,Injyperk=5,
     *		  InFreq=6,InData=8)
	integer Maxrun
	parameter(Maxrun=2048)
	integer i,id,j,VispBuf, VisSize,u,v,k,ktot,l,ltot
	real Visibs(Maxrun)
	common/scratch/ Visibs
c
c  Determine the number of visibilities perr buffer.
c
	VisSize = InData + 2*nchan
	VispBuf = Maxrun/VisSize
	if(VispBuf.eq.0)
     *    call bug('f','Too many channels for buffer in CalcWts')
c
c  Zero out the array.
c
	do j=1,wnu/2+1
	  do i=1,wnv
	    Wts(i,j) = 0.
	  enddo
	enddo
c
c  Accumulate the weight function.
c
	k = 0
	ktot = nvis
	dowhile(k.lt.ktot)
	  ltot = min(VispBuf,ktot-k)
	  call scrread(tvis,Visibs,k*VisSize,ltot*VisSize)
	  do l=1,ltot*VisSize,VisSize
	    u = nint(Visibs(l+InU)/wdu) + 1
	    v = nint(Visibs(l+InV)/wdv) + wnv/2 + 1
	    Wts(v,u) = Wts(v,u) + Visibs(l+InWt)
	  enddo
	  k = k + ltot
	enddo
c
c  Correct the first row.
c
	id = wnv
	do i=1,wnv/2+1
	  Wts(i,1) = Wts(i,1) + Wts(id,1)
	  Wts(id,1) = Wts(i,1)
	  id = id - 1
	enddo
c
	end
c************************************************************************
	subroutine GridVis(tvis,gcf,ngcf,width,nvis,nstart,ncount,nchan,
     *	  Grd,nu,nv,u0,v0,gdu,gdv)
c
	implicit none
	integer tvis,ngcf,width,nu,nv,u0,v0
	integer nstart,ncount,nchan,nvis
	real gdu,gdv
	real gcf(ngcf)
	complex Grd(nv,nu,ncount)
c
c  The start of the gridding process.
c
c  Inputs:
c    tvis	Handle of the visibility scratch file.
c    Beam	Logical. True if making beams.
c    width	Width of gridding convolution function.
c    gcf	Tabulated values of gridding convolutiuon function.
c    ngcf	Number of tabulated values of gridding convolution function.
c    nu,nv	Size of grid array.
c    u0,v0	Index of grid point corresponding to (u,v) = (0,0).
c    gdu,gdv	Grid array uv cell size (wavelengths).
c    nvis	Number of visibilities.
c    nchan	Number of frequency channels.
c    nstart	First frequency channel to map.
c    ncount	Number of frequency channels to map.
c
c  Output:
c    Grd	Gridded visibiliites.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer InU,InV,InWt,InTsys,InRms,InJyperK,InFreq,InData
	parameter(InU=0,InV=1,InWt=2,InTsys=3,InRms=4,Injyperk=5,
     *		  InFreq=6,InData=8)
	integer maxrun,maxwidth
	parameter(maxrun=8*MAXCHAN+InData,maxwidth=8)
	integer i,j,k,ktot,ltot,VisSize,VispBuf,offset
	integer poff(maxwidth**2),qoff(maxwidth**2),goff(maxwidth**2)
	real Visibs(maxrun)
	common/scratch/ Visibs
c
c  Some constants.
c
	VisSize = InData + 2*nchan
	VispBuf = Maxrun/VisSize
	if(VispBuf.eq.0)
     *	  call bug('f','Too many channels for buffer in GridVis')
c
c  Initialise the index arrays used to make the gridding process
c  vectorise.
c
	if(width.gt.maxwidth)
     *	  call bug('f','Convolving function too large in Gridit')
	call IndxIni(ngcf,width,nv,poff,qoff,goff)
c
c  Zero the grid array.
c
	do k=1,ncount
	  do j=1,nu
	    do i=1,nv
	      Grd(i,j,k) = 0
	    enddo
	  enddo
	enddo
c
c  Loop through the visibilities, gridding the appropriate ones.
c
	k = 0
	ktot = nvis
	dowhile(k.lt.ktot)
	  ltot = min(VispBuf,ktot-k)
	  call scrread(tvis,Visibs,k*VisSize,ltot*VisSize)
	  offset = InData + 2*nstart - 1 
	  call Gridit(Visibs,ltot,offset,ncount,VisSize,
     *	    Grd,nu,nv,u0,v0,gdu,gdv,Gcf,ngcf,width,poff,qoff,goff)
	  k = k + ltot
	enddo
c
	end
c***********************************************************************
	subroutine Gridit(VisBuf,Nvis,offset,ncount,VisSize,
     *		Grd,nu,nv,u0,v0,gdu,gdv,
     *		gcf,ngcf,width,poff,qoff,goff)
c
	implicit none
	integer NVis,VisSize,offset,ncount
	integer nu,nv,u0,v0
	real gdu,gdv
	complex Grd(nv*nu,ncount)
	real VisBuf(VisSize,Nvis)
	integer ngcf,width
	integer poff(width*width),qoff(width*width),goff(width*width)
	real Gcf(ngcf)
c
c  Grid a buffer of visibilities. This is the version for a vector
c  machine.
c
c------------------------------------------------------------------------
	integer i,j,k,l,uu,vv,p0,q0,g0,gg,pp,qq,Step,chan
	complex Vis,Vis1
	real Weight,u,v,hwd
c
c  Initialise.
c
	Step = (Ngcf-1)/Width
	hwd = 0.5 * (width - 1)
c
c  Loop thru this buffer of visibilities. The early part is the
c  initialisation for the most important loop in the code.
c
c  Convert u and v to grid units and work out the limits of the convolving
c  region.
c
	do l=1,NVis
	  u = VisBuf(1,l)/gdu + u0 - 1
	  v = VisBuf(2,l)/gdv + v0 - 1
	  vv = nint(v - hwd)
	  uu = nint(u - hwd)
	  g0 = vv + nv*uu
c
	  p0 = ngcf/2 - nint( Step * (v-vv) )
	  q0 = ngcf/2 - nint( Step * (u-uu) )
c
	  if(ncount.lt.width)then
	    k = offset
	    do chan=1,ncount
	      Vis = cmplx(VisBuf(k,l),VisBuf(k+1,l))
#ifdef vector
c#ivdep
c#maxloop 64
	      do i=1,width*width
	        Weight = Gcf(p0+poff(i)) * Gcf(q0+qoff(i))
	        Grd(g0+  goff(i),chan) = Grd(g0+  goff(i),chan) +
     *				Weight * Vis
	      enddo
#else
	      qq = q0 + 1
	      gg = g0
	      do j=1,width
		Vis1 = Gcf(qq) * Vis
		pp = p0 + 1
		do i=1,width
		  Grd(gg+i,chan) = Grd(gg+i,chan) + Gcf(pp) * Vis1
		  pp = pp + Step
		enddo
		qq = qq + Step
		gg = gg + nv
	      enddo
#endif
	      k = k + 2
	    enddo
c
	  else
	    do i=1,width*width
	      Weight = Gcf(p0+poff(i)) * Gcf(q0+qoff(i))
	      gg = g0 + goff(i)
	      k = offset
	      do chan=1,ncount
	        Vis = cmplx(VisBuf(k,l),VisBuf(k+1,l))
		Grd(gg, chan ) = Grd(gg,  chan) +
     *				Weight * Vis
		k = k + 2
	      enddo
	    enddo
	  endif
c
	enddo
	end
c************************************************************************
	subroutine IndxIni(ngcf,width,nv,poff,qoff,goff)
c
	implicit none
	integer ngcf,width,nv
	integer poff(width*width),qoff(width*width),goff(width*width)
c
c  Initialise arrays used to help make the inner gridding loop
c  vectorise.
c
c  Input:
c    ngcf	Number of tabulated values of the convolution function.
c    width	Width of the convolution function.
c    nv	Width of grid array.
c
c  Output:
c    poff	Convolution array index, in x.
c    qoff	Convolution array index, in y.
c    goff	Grid array index.
c
c------------------------------------------------------------------------
	integer i,j,k,p0,q0,g0,g0d,Step
c
c  Initialise the index arrays used by the gridding routine. This will
c  not vectorise, but who cares.
c
	Step = (Ngcf-1)/Width
	if(Step*Width+1.ne.Ngcf)
     *	  call bug('f','Ngcf not mult. of Step in IndxIni')
	k = 0
	q0 = 1
	g0d = 1
	do j=1,width
	  p0 = 1
	  g0 = g0d
	  do i=1,width
	    k = k + 1
	    poff(k) = p0
	    qoff(k) = q0
	    goff(k) = g0
	    p0 = p0 + step
	    g0 = g0 + 1
	  enddo
	  q0 = q0 + step
	  g0d = g0d + nv
	enddo
	end
c************************************************************************
	subroutine makemap(tmap,name,nx,ny,nz,shftx,shfty,dra,ddec,rms,
     *	  ipol,dobeam)
c
	implicit none
	integer tmap,nx,ny,nz,ipol
	character name*(*)
	real dra,ddec,shftx,shfty,rms
	logical dobeam
c
c  Create a map and its basic header.
c
c  Input:
c    name	Name of output map.
c    nx,ny,nz	Dimensions of the map along ra, dec and velocity respectively.
c    shftx,y    Shift of map centre (radians).
c    dra,ddec	Map grid increments (radians).
c    rms	Rms map noise (Jy/beam).
c    doBeam	True if the data is a beam.
c
c  Input thru COMMON (invert.h):
c  These have the normal meaning of the so named header variables.
c    crval3,cdelt3,ctype3,restfreq,vobs,crval1,crval2,epoch,
c    observer,telescop,source,obsra,obsdec,pbfwhm,pols,lstart,lstep,lwidth
c    itype
c
c  Output:
c    tmap	Handle of the image file.
c
c------------------------------------------------------------------------
	include 'invert.h'
	integer nsize(4),naxis,pol
c
	pol = 0
	if(ipol.gt.0.and.ipol.le.npols) pol = pols(ipol)
	nsize(1) = nx
	nsize(2) = ny
	nsize(3) = nz
	nsize(4) = 1
	naxis = 2
	if(cdelt3.ne.0) naxis = 3
	if(pol.ne.0)    naxis = 4
	call xyopen(tmap,name,'new',naxis,nsize)
	call wrhda(tmap,'bunit','JY/BEAM')
	if(doBeam)then
	  call wrbtype(tmap,'beam')
	else
	  call wrbtype(tmap,'intensity')
	endif
	if(rms.gt.0)call wrhdr(tmap,'rms',rms)
c
	call wrhdd(tmap,'crval1',crval1)
	call wrhdr(tmap,'cdelt1',dra)
	call wrhdr(tmap,'crpix1',real(nx/2+1)-shftx/dra)
	call wrhda(tmap,'ctype1',ctype1)
        if(shftx.ne.0.0) call wrhdr(tmap,'xshift',shftx)
c
	call wrhdd(tmap,'crval2',crval2)
	call wrhdr(tmap,'cdelt2',ddec)
	call wrhdr(tmap,'crpix2',real(ny/2+1)-shfty/ddec)
	call wrhda(tmap,'ctype2',ctype2)
        if(shfty.ne.0.0) call wrhdr(tmap,'yshift',shfty)
c
	if(cdelt3.ne.0)then
	  call wrhdr(tmap,'crval3',real(crval3))
	  call wrhdr(tmap,'cdelt3',real(cdelt3))
	  call wrhdr(tmap,'crpix3',1.0)
	  call wrhda(tmap,'ctype3',ctype3)
	endif
c
	if(restfreq.gt.0)call wrhdd(tmap,'restfreq',restfreq)
	call wrhdr(tmap,'vobs',vobs)
c
	if(pol.ne.0)then
	  call wrhdr(tmap,'crval4',real(pol))
	  call wrhdr(tmap,'cdelt4',real(sign(1,pol)))
	  call wrhdr(tmap,'crpix4',1.0)
	  call wrhda(tmap,'ctype4','STOKES')
	endif
c
c  Write some more items to the map header.
c
	call wrhdd(tmap,'obsra',obsra)
	call wrhdd(tmap,'obsdec',obsdec)
	call wrhdr(tmap,'epoch',epoch)
	if(pbfwhm.gt.0)call wrhdr(tmap,'pbfwhm',pbfwhm)
	if(source  .ne.' ')call wrhda(tmap,'object',  source)
	if(telescop.ne.' ')call wrhda(tmap,'telescop',telescop)
	if(observer.ne.' ')call wrhda(tmap,'observer',observer)
c
c  Output the linetype parameters.
c
	call wrhda(tmap,'ltype',ltype)
	call wrhdr(tmap,'lstart',lstart)
	call wrhdr(tmap,'lwidth',lwidth)
	call wrhdr(tmap,'lstep',lstep)
	end
c************************************************************************
	subroutine history(tvis,tmap,nvis,width,func,alpha,
     *					rms,Tsys,JyperK,Totint,version)
c
	implicit none
 	integer tvis,tmap,width,nvis
	character func*(*),version*(*)
	real alpha,Tsys,JyperK,Totint,rms
c
c  Create the history file of an output map.
c
c  Input:
c    tvis	Handle of the input visibility file.
c    tmap	Handle of the output map file.
c    nvis	Number of visibilities.
c    width	Gridding function width.
c    func	Gridding function type.
c    alpha	Gridding function parameter.
c    rms	Rms noise.
c    Tsys	Average system temperature.
c    Totint	Total system integration time.
c    JyperK	Average Jy/K.
c
c------------------------------------------------------------------------
	character line*80
c
	call hdcopy(tvis,tmap,'history')
	call HisOpen(tmap,'append')
	line = 'INVERT: Miriad '//version
        call HisWrite(tmap,line)
c
	call HisInput(tmap,'INVERT')
c
	write(line,30)func,width,alpha
   30	format('INVERT: Conv. function, ',a,' width =',i2,', alpha=',
     *								1pe11.4)
	call HisWrite(tmap,line)
c
	write(line,35)nvis
   35	format('INVERT: Number of visibilities =',i8)
	call HisWrite(tmap,line)
c
	write(line,'(a,1pg9.3)')
     *	  'INVERT: Theoretical map noise (Jy/beam): ',rms
	call HisWrite(tmap,line)
c
	write(line,'(a,1pg9.3)')
     *	  'INVERT: Total integration time (hours): ',totint
	call HisWrite(tmap,line)
c
	write(line,'(a,1pg9.3)')
     *	  'INVERT: Average system temperature (Kelvin): ',Tsys
	call HisWrite(tmap,line)
c
	write(line,'(a,1pg9.3)')
     *	  'INVERT: Average system gain (Jy/K): ',JyperK
	call HisWrite(tmap,line)
c
	call HisClose(tmap)
	end
c************************************************************************
	subroutine Sizes(cell,sup,gn,fwhm,width,gd,wn,wd,uvmax,T)
c
	implicit none
	real cell,sup,gd,wd,uvmax,fwhm,T
	integer gn,wn,width
c
c  Determine various cell and size parameters, which are independent of
c  whether we are dealing with the x or y axis.
c
c  Input:
c    cell	Image cell size (arcseconds).
c    sup	Suppression region (arcseconds).
c    fwhm	Gausian taper fwhm (arcseconds).
c    width	Convolution function width (pixels).
c    gn		Image dimension (pixels).
c
c  Output:
c    gd		Uv plain gridding cell size (wavelengths).
c    wn		Dimension of weights array (pixels).
c    wd		Uv plain weights cell size (wavelengths).
c    uvmax	Maximum spacing (wavelengths).
c    T		Taper exponent parameter (nepers/wavelength**2).
c
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	real Field
c
c  Find field of view, etc, in appropriate units.
c
	Field = Cell * (pi/180/3600) * gn
	gd = 1. / Field
	uvmax = 0.5 * (gn - width - 1) * gd
c
c  Convert the gaussian taper spec to a uv plane based taper, T. Also
c  reduce uvmax so that T*uvmax**2 < 20. This discards values in the
c  uv plane which have too small a weight to worry about (and prevents
c  possible floating underfloat caused by exp(-x) where x > 20.
c
	T = ( Fwhm * (pi/180/3600) )**2 * (pi**2 / (4.*log(2.)))
	if(T.gt.0) uvmax = min(sqrt(20./T),uvmax)
	T = - T
c
	if(Sup.le.0)then
	  wd = uvmax
	  wn = 1.
	else
	  wd = 1./min(Field, Sup * (pi/180/3600) )
	  wn = 2*nint( uvmax / wd ) + 1
	endif
c
	end
c************************************************************************
	subroutine DirectFT(tscr,nvis,nstart,nchan,du,dv,median,
     *		Array,nx,ny,tno,ipl,Scale)
c
	implicit none
	integer tscr,nvis,nstart,nchan,nx,ny,tno,ipl
	logical median
	real du,dv,Scale
	real Array(5*nvis)
c
c  This computes and writes a plane of the data using a direct Fourier
c  transform approach. It takes a scratch file containing the visibilities,
c  and writes the output to an image file.
c
c  Input:
c    tscr	Handle of the scratch file.
c    nvis	Number of visibilities.
c    nstart	Channel number to process.
c    nchan	Total number of channels.
c    du,dv	UV cell size.
c    median	Use a "median value" approach.
c    nx,ny	Output image size.
c    tno	Handle of the output image file.
c    ipl	Plane number in the output.
c    Scale	Scale to multiple the data by.
c  Scratch:
c    Array
c------------------------------------------------------------------------
	integer InU,InV,InWt,InTsys,InRms,InJyperK,InFreq,InData
	parameter(InU=0,InV=1,InWt=2,InTsys=3,InRms=4,Injyperk=5,
     *		  InFreq=6,InData=8)
	integer maxrun
	parameter(maxrun=2048)
	integer VisSize,VispBuf,k,ktot,l,ltot,p,q,offset
	real Visibs(maxrun)
	common/scratch/Visibs
c
c  Some constants.
c
	VisSize = InData + 2*nchan
	VispBuf = Maxrun/VisSize
	if(VispBuf.eq.0)
     *	  call bug('f','Too many channels for buffer in DirectFT')
c
c  Read the data into the scratch array, scaling U and V as we go.
c
	p = 0
	k = 0
	ktot = nvis
	dowhile(k.lt.ktot)
	  ltot = min(VispBuf,ktot-k)
	  call scrread(tscr,Visibs,k*VisSize,ltot*VisSize)
	  offset = InData + 2*nstart - 2
	  q = 1
	  do l=1,ltot
	    Array(p+1) = Visibs(q+InU) / (du*nx)
	    Array(p+2) = Visibs(q+InV) / (dv*ny)
	    Array(p+3) = Visibs(q+offset)
	    Array(p+4) = Visibs(q+offset+1)
	    p = p + 4
	    q = q + VisSize
	  enddo
	  k = k + ltot
	enddo
c
c  Set the plane that we wish to access.
c
	call xysetpl(tno,1,ipl)
c
c  Call the routine which does the real work (all too much of it).
c
	call DFTProc(tno,Array,Array(4*nvis+1),nvis,nx,ny,
     *						median,scale)
c
c  Finished at last.
c
	end
c************************************************************************
	subroutine GetScalS(tscr,nvis,nstart,nchan,Scale)
c
	integer tscr,nvis,nstart,nchan
	real Scale
c
c  Determine the scale factor to apply to the data, so that the
c  beam comes out with a peak value of 1.
c
c  Input:
c  Output:
c    scale	Scale factor.
c------------------------------------------------------------------------
	integer InU,InV,InWt,InTsys,InRms,InJyperK,InFreq,InData
	parameter(InU=0,InV=1,InWt=2,InTsys=3,InRms=4,Injyperk=5,
     *		  InFreq=6,InData=8)
	integer maxrun
	parameter(maxrun=2048)
	integer VisSize,VispBuf,offset,k,ktot,l,ltot
	real Visibs(maxrun),Sum
	common/scratch/Visibs
c
c  Some constants.
c
	VisSize = InData + 2*nchan
	VispBuf = Maxrun/VisSize
	if(VispBuf.eq.0)
     *	  call bug('f','Too many channels for buffer in DirectFT')
c
c  Read the data into the scratch array, scaling U and V as we go.
c
	Sum = 0
	k = 0
	ktot = nvis
	dowhile(k.lt.ktot)
	  ltot = min(VispBuf,ktot-k)
	  call scrread(tscr,Visibs,k*VisSize,ltot*VisSize)
	  offset = InData + 2*nstart - 1
	  do l=1,ltot
	    Sum = Sum + Visibs(offset)
	    offset = offset + VisSize
	  enddo
	  k = k + ltot
	enddo
c
	Scale = 1/Sum
c
	end
c************************************************************************
	subroutine DFTProc(tno,Dat,Pxl,nvis,nx,ny,med,scale)
c
	implicit none
	integer tno,nvis,nx,ny
	logical med
	real scale,Dat(4,nvis),Pxl(nvis)
c
c  Compute an image from visibility data, using a direct Fourier transform.
c
c  Inputs:
c    tno	Handle of the output image.
c    Dat	Array containing
c		  Dat(1,i) -- Scaled U coordinate.
c		  Dat(2,i) -- Scaled V coordinate.
c		  Dat(3,i) -- Real part of visibility.
c		  Dat(4,i) -- Imaginary part of the visibility.
c    med	Use a media approach, rather than a conventional Fourier
c		transform.
c    scale	Scale factor to apply to the data.
c  Scratch:
c    Pxl	Array to hold temporary values.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer i,x0,y0,ix,iy,nrow,irow
	real theta,x,row(maxdim)
c
c  Externals.
c
	character itoaf*5
c
	x0 = nx/2 + 1
	y0 = ny/2 + 1
c
c  Determine how often to give a message.
c
	nrow = 1000000 / nvis / nx
	if(med) nrow = nrow * log(2.) / log(real(nvis))
	nrow = max(nrow,1)
c
	irow = 0
	do iy=1,ny
	  irow = irow + 1
	  if(irow.ge.nrow)then
	    call output('Doing row '//itoaf(iy))
	    irow = 0
	  endif
	  do ix=1,nx
	    do i=1,nvis
	      theta = -2*pi*( (ix-x0)*dat(1,i) + (iy-y0)*dat(2,i) )
	      pxl(i) = cos(theta)*dat(3,i) - sin(theta)*dat(4,i)
	    enddo
	    if(med)then
	      call median(pxl,nvis,x)
	      x = nvis * x
	    else
	      x = 0
	      do i=1,nvis
		x = x + pxl(i)
	      enddo
	    endif
	    row(ix) = scale*x
	  enddo
	  call xywrite(tno,iy,row)
	enddo
c
	end
