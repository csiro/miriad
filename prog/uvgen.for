c************************************************************************
	program  uvgen
	implicit none
c
c  UVGEN computes model visibility data from source components file
c  and antennas file.
c
c  History:
c    jan84 mchw  with polarization and new record format.
c    feb85 mchw  record, only 1st 3 baselines are in header
c    dec85 mchw  convert visibilities to Jy
c    mar86 mchw  allow up to 27 antennas
c    mar88 mchw  calculate model visibility separately for each frequency
c		      in subroutine MODINT
c    mar88 mchw  antenna based amplitude and phase noise
c    nov88 wh    converted to mirth
c    6apr89 wh    write out new miriad variables
c    10apr89 rjs   Fixed bugs to do with confusing double and real
c		      variables. Broke some lines up so they are less
c		      than 72 chars. Declared variables.
c    28sep89 mchw  Write wideband data option, made baseout double,
c		      Fixed output to terminal, changed name to uvgen.
c    25oct89 rjs   Fixed bug to do with putvrd(..,uvout). Corrected
c		      alignment problem in the common block.
c    31oct90 mchw  Moved some telescope dependent parameters into CORAM.
c     7nov90 rjs   Clean up, and addition of the "site" keyword, to
c		      simplify simulation of other telescopes. Improved
c		      documenation.
c    13nov90 rjs   More tidying.
c    28nov90 mchw  Minor patches and bugs.
c    05dec90 mchw  Added total power, and keywords harange and out.
c			Allowed filenames as specified in doc.
c     4feb91 rjs   Allow the user to input the start observing time. This
c			observing time is used as random number seed as well.
c    20mar91 rjs   Fixed bugs in coramoth when there is channel data. Some
c			work on documentation.
c    22mar91 rjs   Fixed  bug dealing with point source.
c    17apr91 rjs   Used uvwwrite to write out wide data.
c    23may91 rjs   Added polarisation leakage terms, "cycle", and rise/set
c		   time warning.
c    29may91 nebk  Fix call to HANGLE
c    11jun91 rjs   Changed linear polarisation feeds sign convention.
c    17jun91 rjs   More sign convention fiddling.
c     1jul91 rjs   Leakage parameters depend on the random number
c		   generator seed now.
c     7jul91 rjs   Changes to the doc only.
c    04aug91 mjs   Replaced local maxant with maxdim.h value MAXANT and
c		   renamed local maxchan to avoid conflict with MAXCHAN
c    05aug91 rjs   Corrected various bugs introduced with MAXANT change.
c    19sep91 rjs   Changes to the doc.
c    20nov91 rjs   Added ability to have multiple pointings and primary beam.
c    16jun92 rjs   Doc change only.
c    23jun92 rjs   More doc changes.
c    26jun92 rjs   Save latitud and longitu variables.
c    03feb93 edg/pjt  Optional reading in of IF freq for Hat Creek only
c    04feb93 pjt   Merged the option to append output to out=
c    12feb93 mchw  Changed ra and dec to double precision.
c    29mar93 rjs   Fiddles with noise level. Better documentation on it.
c    08jul93 mchw  Fix bug in start time.
c    21jul93 rjs   Fiddles with noise levels for Stokes parameters.
c    26oct93 rjs   Use keyt and correct calculations of u,v when doing
c		   mosaiced experiment.
c    03mar94 mchw  Added elevation limit. Standard keywords dec and elev.
c			Generalize hybrid correlator model.
c    18aug94 rjs   Exact geometry for point sources, and better
c		   geometry for other sources.
c    29aug94 rjs   Write w axis value.
c    15sep94 mchw  Change the site keyword to be the 'telescop' uv-variable.
c    21sep94 mchw  Better value for sfreq in coramhat.
c    28sep94 rjs   Merge mchw/rjs changes.
c    19jan95 mchw  Added atmospheric phase model to pnoise input.
c    25jan95 mchw  Fix bug (lst = ha + ra)
c    27nov95 mchw  Correct sign of source position angle.
c    27nov95 rjs   (Re-)add some commas to appease g77.
c    20dec95 mchw  Added polarization switching.
c    04jun96 mchw  Doc change only.
c
c  Bugs/Shortcomings:
c    * Frequency and time smearing is not simulated.
c    * Primary beam is a gaussian -- which is too ideal.
c    * Primary beam is not a function of frequency.
c    * Geometry for extended sources could be improved.
c------------------------------------------------------------------------
c= UVGEN - Compute visibilities for a model source.
c& rjs
c: uv analysis, map making
c+
c	UVGEN is a MIRIAD task which computes visibility data for a model
c	source distribution at u-v data points specified by a set of
c	antenna positions, hour angle range and sample interval. The model
c	is specified by a set of Gaussian sources with given positions and
c	flux densities. Analytic expressions are used to calculate the
c	value of the visibilities. The calculation includes the response to
c	polarized sources with linear and circularly polarized feeds. U-V
c	trajectories for all pairs of antennas are computed.
c@ source
c	The name of a text file containing the source components. The
c	default is "uvgen.source". If the specified model components file
c	does not exist, UVGEN interactively prompts the user for information,
c	and then generates the file. The source components are elliptical
c	Gaussian components described by the total flux (Jy) and
c	position offsets (arcsecs) from the phase center in the directions
c	of ra and dec. The sources are specified by the full width to half
c	maximum of the major and minor axes; the position angle of the
c	major axis measured from north to the east. The default half width
c	for a "point" source is 0."0001. The sources can be partially linearly
c	polarized. This information is given as a percentage polarization and
c	a position angle. A value of 0 for the percentage polarization forms
c	an unpolarized source.
c@ ant
c	The name of a text file containing the position of the antennae.
c	The default is "uvgen.ant". If the specified antenna file does
c	not exist, UVGEN interactively prompts the user for the coordinates
c	of the antennas, and then generates the antenna file.
c	The antenna positions can be given in either a right handed
c	equatorial system or as a local ground based coordinates measured to the
c	north, east and in elevation. See the "baseunit" parameter to
c	specify the coordinate system. Some standard antenna configurations
c	can be found in $MIRCAT/*.ant for ATCA, BIMA and VLA telescopes.
c	The BIMA antpos files (edit header line) can also be used with baseunit=1.
c@ baseunit
c	This specifies the coordinate system used in the antenna file.
c	A positive value for "baseunit" indicates an equatorial system,
c	whereas a negative value indicates a local system. The magnitude of
c	"baseunit" gives the conversion factor between the baseline units
c	used in the antenna file, and nanoseconds. The default value is +1,
c	which means that the antenna file gives the antenna position in an
c	equatorial system measured in nanoseconds.
c	E.g. 	baseunit=-1 for topocentric coordinates in nanosecs, 
c		baseunit=3.33564 for geocentric coordinates in meters.
c@ telescop
c	This can take on the value of "hatcreek", "atca" or "other".
c	This determines miscellaneous parameters. In particular, this
c	determines the interpretation of the correlator setup file
c	(see below), the "evector" variable and the telescope name.
c	The default is "hatcreek".
c@ corr
c	This gives the name of a text file specifying the correlator
c	setup, and a spectral line model. The default name is "uvgen.corr".
c	If it does not exist, UVGEN prompts interactively, and then creates
c	the file.
c	The values are:
c	  Number of channels in each spectral window. 0=wideband only.
c	  Number of spectra: up to 4 spectral windows can be specified.
c	  Four starting frequencies and bandwidths for each spectral window. 
c	   These are specified in MHz. For other than Hat Creek, only
c	   the first correlator bandwidth is used.
c	   No checking is made for valid combinations.
c	  Three parameters, famp, fcen and fwid, giving line to continuum
c	   ratio, freq and width (GHz). This gives a simple spectral line.
c	   In particular, the visibility value for a channel is scaled by
c	   a factor:
c	    1 + famp*( 1-min(1,|(f-fcen)/fwid|) )
c@ time
c	The start time of the observation. This is in the form
c	  yymmmdd.ddd or yymmmdd:hh:mm:ss.s
c	The default is 80JAN01.0. A function of this is also used
c	as a seed for the random number generator.
c@ freq
c	Frequency and 2nd IF frequency for the model in GHz.
c	Defaults are 100,0.250 GHz. 
c	The IF parameter is currently ignored if the telescope is not 
c	hatcreek.
c@ radec
c	Source right ascension and declination. These can be given in
c	hh:mm:ss,dd:mm:ss format, or as decimal hours and decimal
c	degrees. The default is 0,30.
c@ harange
c	Hour Angle range (start,stop,step) in hours. Default is
c	-6 hrs to + 6 hrs, with a sample interval=0.1 (6 minute)
c@ elev
c	Elevation limit in degrees. Default=15 degrees. Both harange
c	and elev are used to limit the extent of the u-v track.
c@ stokes
c	This selects the polarization parameters formed. Up to 4
c	polarizations can be formed in one run . They can be 'i' (default),
c	'xx', 'yy', 'xy', 'yx, 'lr', 'rl', 'rr' or 'll'. For example:
c	  stokes=xx,yy,xy,yx
c	will form a file with the 4 polarisations corresponding to an array
c	with linear feeds.
c	For linear feeds the convention is that the X feed has a position
c	angle of 0, and the Y feed is 90 (measured north towards east).
c@ polar
c	Polarization patterns for generating time shared polarization data. 
c	Up to MAXPOLAR=20 strings of the characters R and L, or X and Y, 
c	to represent the polarization of each antenna
c	R(right circular polarization), L(left circular polarization)
c	X(linear polarization PA=0), Y(linear polarization PA=90).
c	E.g. for 3 antennas, the polar=LLL,LRR,RRL,RLR cycles
c	through all combinations of LCP and RCP for each baseline every
c	4 integrations. The default is to use the stokes keyword.
c@ lat
c	Latitude of observatory, in degrees. Default is 40 degrees.
c@ cycle
c	This gives two values, being the time on-source, and the time
c	off-source cycle times, both in hours. This allows simulation of
c	time segments lost while observing calibrators, etc. For example,
c	if simulating an observation which observes the source for 24 minutes
c	and then is off-source (observing a calibrator) for 6 minutes, use:
c	  cycle=0.4,0.1
c	Similarly, if simulating this calibrator, use:
c	  cycle=0.1,0.4
c	The default is harange(3),0 (i.e. do not interrupt the observations).
c@ pbfwhm
c	The primary beam fwhm, in arcseconds. The value of this will be
c	approximately 66000/(diam*freq), where "diam" is the antenna
c	diameter in meters, and "freq" is the observing frequency. The
c	default is an infinite primary beam (i.e. no primary beam effects).
c@ center
c	Offset observing centers for a mosaiced observation, in arcseconds.
c	Two values (x and y offset) are required per pointing. Several
c	values can be given. Default is 0,0 (i.e. a plain, single pointing
c	observation). The time spent on each pointing is given by the value of
c	``cycle(1)''. Note that the default value of cycle(1) means that the
c	observing center changes every integration.
c@ gnoise
c	Antenna based gain noise, given as a percentage. This gives the
c	multiplicative gain variations, specified by the rms amplitude to be
c	added to the gain of each antenna at each sample interval. The
c	gain error stays constant over the period given by the ``cycle(1)''
c	parameter (see above). Thus ``cycle(1)'' can be varied to give
c	different atmosphere/instrument stabilities. Note that the default
c	of the ``cycle'' parameter means that the gain changes every
c	integration. 
c
c	A gain error can also be used to mimic random pointing errors
c	provided the source is a point source.
c	The default is 0 (i.e. no gain error).
c@ pnoise
c	Antenna based phase noise, in degrees. This gives the phase
c	noise, specified by the rms phase noise to be added to each
c	antenna. Up to 4 values can be given to compute the phase noise
c	  pnoise(1) + pnoise(2)*(baseline)**pnoise(3)*sinel**pnoise(4)
c	where ``baseline'' is the baseline length in km. For
c	Kolmogorov turbulence pnoise(3)=5/6 for baseline < outer scale
c	and 0.33 for baseline > outer scale of turbulent layer.
c	See also the ``gnoise'' parameter. The default is 0,0,0,0 (i.e.
c	no phase error).
c@ systemp
c	System temperature for additive noise, in Kelvin. This is used
c	to generate random Gaussian noise to add to each data point. The
c	default is 0 K (i.e. no noise).
c@ leakage
c	Polarization leakage errors, given as a percent. This gives the
c	rms value of leakages of one polarisation feed into another.
c	Polarization leakage errors are constant over the observation.
c	To use this, you must set
c	  stokes=xx,yy,xy,yx
c	or
c	  stokes=rr,ll,rl,lr
c	The default is 0 (i.e. no polarization leakage).
c@ tpower
c	Three numbers can be given to represent the total power variations
c	due to receiver instability, telescope elevation dependence, and
c	atmospheric noise. The total power is computed as:
c	  tpower = systemp + trms + telev * cos(el) + tatm * antpnoise
c	The receiver instablity is modeled as additive Gaussian noise.
c	The atmospheric noise is modeled to be correlated with the antenna
c	phase noise. Units of trms, telev in Kelvin and tatm in Kelvin/radian.
c	The systemp is not changed. Typical values for millimeter wavelengths
c	are trms=1 K (10-4 * systemp) telev=100 K and tatm=0.5 K/radian.
c	Default is tpower=0,0,0
c@ jyperk
c	The system sensitivity, in Jy/K. Its value is given by 2*k/(eta * A)
c	where k is Boltzmans constant (1.38e3 Jy m**2 / K), A is the physical
c	area of each antenna (pi/4 * D**2), and eta is an efficiency.
c	For the ATCA, D is 22 meters, and eta is composed of a correlator
c	efficiency (0.88) and an antenna efficiency (0.65 at 6 cm). The
c	overall result is jyperk=12.7. The default jyperk=150, a typical
c	value for the Hat Creek 6.1 m antennas.
c@ out
c	This gives the name of the output Miriad data file. The default
c	it "uvgen". If the dataset exists, visibilities are appended to
c	the dataset, with an appropriate informational message.
c--
c------------------------------------------------------------------------
	real sqrt2
	character version*(*)
	parameter(sqrt2=1.414214)
	parameter(version = 'Uvgen: version 1.0 04-JUN-96')
	include 'mirconst.h'
	include 'maxdim.h'
	integer maxsrc,maxpol,maxpnt,maxpolar
	parameter(maxsrc=1000,maxpol=4,maxpnt=100,maxpolar=20)
	include 'uvgen.h'
c
	real corfin(4),corbw(4)
	complex vis,gain(MAXANT),leak(2,MAXANT)
	complex wcorr(maxspect,maxpol),chan(MAXCHAN,maxpol)
	real wsignal(maxspect),tpower(MAXANT),pnoise(MAXANT)
	logical flags(MAXCHAN)
	logical donoise,dogains,doleak,dopoint
	real sind,cosd,sinl,cosl,sinel,cosel,flux,dra,ddec
	double precision freq,iffreq
	real wmaj,wmin,wpa,poln,polpa,x,z,h,sinha,cosha,ha,haend
	double precision bxx,byy,bzz,bxy,byx
	real pbfwhm,center(2,MAXPNT),evector
	integer n,nant,npnt,ipnt,i,j,jj,m,is,ic,nchan,nospect,ntemp
	double precision preamble(5),timeout
	real b1(MAXANT),b2(MAXANT),b3(MAXANT),temp,psi,sinq,cosq,leakrms
	real systemp(MAXANT*maxspect),inttime
	double precision restfreq(maxspect),lst
	double precision antpos(3*MAXANT),ra,dec
	integer item, unit, iostat
	integer tunit,leng,status
	character line*132, umsg*80
	complex gatm
	real baseline,patm,pslope,pelev
	logical doatm,dopolar
c
c  Parameters from the user.
c
	integer NTELS
	parameter(NTELS=3)
	character sfile*64,antfile*64,corfile*64,outfile*64
	real hbeg, hend, hint, arms, prms, tsys, utns
	real trms,telev,tatm,cycleon,cycleoff
	double precision alat,sdec,sra,elev
	integer pol(maxpol),npol,ipol,npolar,ipolar
	character telescop*16,tels(NTELS)*8
	character polar(MAXPOLAR)*27,xpolar*27
c	character polar(MAXPOLAR)*MAXANT,xpolar*MAXANT
c
c  Variables describing the source.
c
 	integer ns
	real ta(maxsrc),sx(maxsrc),sy(maxsrc),smaj(maxsrc),smin(maxsrc)
	real spa(maxsrc),per(maxsrc),pa(maxsrc)
	real smajd(maxsrc),smind(maxsrc),tad(maxsrc),sxd(maxsrc),
     *	     syd(maxsrc),szd(maxsrc)
c
c  Model spectra and noise.
c
	real famp, fcen, fwid
	real wrms(maxspect,maxpol), rrms(MAXCHAN,MAXPOL),jyperk
c
c  Externals.
c
	complex expi
	real rang
        integer PolsP2C
        integer len1
c
c  Data initialisation.
c
	data flags /MAXCHAN*.true./
	data tels/'hatcreek','other   ','atca    '/
	data gatm/(1.,0.)/
c	data nospect/0/,famp/0./,fcen/0./,fwid/0./
c
c  Get command line arguments.
c
	call output( version )
	call keyini
	call keya('source',sfile,' ')
	if(sfile.eq.' ')then
	  sfile='uvgen.source'
	  call bug('w','Source file will be uvgen.source')
	endif
c
	call keya('ant',antfile,' ')
	if(antfile.eq.' ')then
	  antfile='uvgen.ant'
	  call bug('w','Ant file will be uvgen.ant')
	endif
c
	call keymatch('telescop',NTELS,tels,1,telescop,ntemp)
	if(ntemp.eq.0) telescop = tels(1)
	call ucase(telescop)
c
	call keya('corr',corfile,' ')
	if(corfile.eq.' ')then
	  corfile='uvgen.corr'
	  call bug('w','Corr file will be uvgen.corr')
	endif
c
	call keyr('baseunit',utns,1.0)
	call keyd('freq',freq,100.d0)
	call keyd('freq',iffreq,0.250d0)
	call keyt('time',timeout,'atime',0.d0)
	if(timeout.le.1)call dayjul('80JAN01',timeout)
	call keyt('radec',sra,'hms',0.d0)
	call keyt('radec',sdec,'dms',30.d0*dpi/180.)
	call keyt('elev',elev,'dms',15.d0*pi/180.)
	sind = sin(sdec)
	cosd = cos(sdec)
	call GetPol(pol,npol,maxpol)
	call mkeya('polar',polar,MAXPOLAR,npolar)
	dopolar = npolar.gt.0
	call keyt('lat',alat,'dms',40.d0*pi/180)
	sinl=sin(alat)
	cosl=cos(alat)
c
	call keyr('harange',hbeg,-6.)
	call keyr('harange',hend,6.)
	call keyr('harange',hint,.1)
	if(hbeg.ge.hend.or.hint.lt.0)
     *	  call bug('w','Invalid harange parameter')
	call keyr('cycle',cycleon,hint)
	call keyr('cycle',cycleoff,0.)
	if(cycleon.le.0.or.cycleoff.lt.0)
     *	  call bug('f','Bad cycle parameters')
c
c  Mosaicing/primary beam parameters.
c
	call keyr('pbfwhm',pbfwhm,-1.)
	pbfwhm = pi/180/3600 * pbfwhm
	call mkeyr('center',center,2*MAXPNT,npnt)
	if(mod(npnt,2).ne.0) call bug('f',
     *	  'There must be an even number of values for "center"')
	npnt = npnt / 2
	if(npnt.eq.0)then
	  npnt = 1
	  center(1,1) = 0
	  center(2,1) = 0
	else
	  do i=1,npnt
	    center(1,i) = center(1,i) * pi/180/3600
	    center(2,i) = center(2,i) * pi/180/3600
	  enddo
	endif
c
	call keyr('gnoise',arms,0.)
	arms = arms /100.
	call keyr('pnoise',prms,0.)
	prms = prms * pi/180
	call keyr('pnoise',patm,0.)
	patm = patm * pi/180
	call keyr('pnoise',pslope,0.)
	call keyr('pnoise',pelev,0.)
	call keyr('leakage',leakrms,0.)
	leakrms = leakrms / 100.
	call keyr('systemp',tsys,0.)
	call keyr('tpower',trms,0.)
	call keyr('tpower',telev,0.)
	call keyr('tpower',tatm,0.)
	call keyr('jyperk',jyperk,150.0)
c
	call keya('out',outfile,'uvgen')
	if(outfile.eq.'uvgen')
     *	  call bug('w','Output file will be UVGEN')
	call keyfin
c
c  Determine the rise and set times of the source, at the minimum
c  elevation angle.
c
	sinel = sin(elev)
	temp = (sinel - sinl*sind ) / ( cosl*cosd )
	if(abs(temp).gt.1)then
	  if(sdec*alat.lt.0)then
	    call output('Source never rises above elevation limit.')
	    stop
	  else
	    call output('Source never sets below elevation limit.')
	    temp = 12.
	  endif
	else
	  temp = acos(temp)
	  temp = 12/pi * temp
	  write(line,'(a,f5.1,a,f5.1,a)') 'Hour angle limit is ',temp,
     *		' hrs at ',elev*180./pi,' degrees elevation'
	  call output(line)
	  if(hbeg.lt.-temp.or.hend.gt.temp)
     *	    call bug('w','Source is not always up for given HA range')
	endif
c
c  Find HA limits.
c
	hbeg = max(hbeg,-temp) 
	hend = min(hend,temp) 
	write(line,'(a,f5.1,a,f5.1,a)') 'Hour angle range is ',hbeg,
     *		' to ',hend,' hours'
	call output(line)
c
	donoise = tsys.gt.0
	dogains = arms.gt.0.or.prms.gt.0.or.patm.gt.0.
	doleak = leakrms.gt.0
	doatm = patm.gt.0.
	if(doleak)then
	  doleak = npol.eq.4
	  if(doleak)doleak = (pol(1).eq.-1.and.pol(4).eq.-4).or.
     *			     (pol(1).eq.-5.and.pol(4).eq.-8)
	  if(.not.doleak)then
	    call bug('w','Unable to simulate polarization leakage')
	    call bug('w','See help for notes on simulating leakage')
	  endif
	endif
c
c  Start the history file. 
c  First test if the outfile exists, if so, open output file
c  in append mode.
c
        call hopen(unit,outfile,'old',iostat)
        if(iostat.eq.0) then
	  call hclose(unit)
	  call uvopen(unit,outfile,'append')
	  call hisopen(unit,'append')
        else
	  call uvopen(unit,outfile,'new')
	  call hisopen(unit,'write')
        endif
	call uvset(unit,'preamble','uvw/time/baseline',0,0.,0.,0.)
        call hiswrite(unit,'UVGEN: Miriad '//version)
        call hisinput(unit,'UVGEN')
        umsg = 'UVGEN: '//line
	call hiswrite(unit, umsg )

c
c  Open the source components file.
c
	call modcomp(sfile,tunit)
c
	call hiswrite(unit,'UVGEN: Source specifications:')
	write(line,'(a)')'     Flux  Offset   Offset   Major  Minor  '//
     *    'Axis   Polar    Polar'
	call output(line(1:80))
        umsg = 'UVGEN: '//line(1:75)
	call hiswrite(unit, umsg )
	write(line,'(a)')'             Ra      Dec     Axis   Axis   '//
     *    'Angle  ization  Angle'
	call output(line)
        umsg = 'UVGEN: '//line
	call hiswrite(unit, umsg )
	write(line,'(a)')'     (Jy)   (")      (")     (")    (")    '//
     *    '(deg)   (%)      (deg)'
	call output(line(1:80))
        umsg = 'UVGEN: '//line(1:75)
	call hiswrite(unit, umsg )
c
c  Read the source component file.
c
	ns = 0
	call txtread(tunit,line,leng,status)
	dowhile(status.eq.0.and.ns.lt.maxsrc)
	  read(line(1:leng),100) flux,dra,ddec,wmaj,wmin,wpa,poln,polpa
100	  format(1x,8f10.4)
	  ns = ns + 1
	  ta(ns) = flux
	  sx(ns) = dra  * pi/180/3600
	  sy(ns) = ddec * pi/180/3600
	  if(wmaj .eq. 0.) wmaj = 0.0001
	  if(wmin .eq. 0.) wmin = 0.0001
	  smaj(ns) = wmaj * pi/180/3600
	  smin(ns) = wmin * pi/180/3600
	  spa(ns)  =  wpa * pi/180
	  per(ns) = poln / 100.
	  pa(ns)  = polpa * pi/180
	  write(line,101) ns,flux,dra,ddec,wmaj,wmin,wpa,poln,polpa
101	  format(i2,F7.2,2F8.1,2F8.2,F8.1,F8.1,F8.2)
	  call output(line)
          umsg = 'UVGEN: '//line
	  call hiswrite(unit, umsg )
	  call txtread(tunit,line,leng,status)
	enddo
c
	call txtclose(tunit)
	write(line,'(i4,a)')  ns,' sources read from model'
	if(ns.eq.maxsrc)
     *	  call bug('w','Max number of source components read')
	call output(line)
c
c  Read the antenna positions file.
c
	call modant(antfile,tunit)
c
	call output('Antenna positions :')
	call hiswrite(unit,'UVGEN: Antenna positions :')
	nant = 0
	call txtread(tunit,line,leng,status)
	dowhile(status.eq.0.and.nant.lt.MAXANT)
	  nant = nant + 1
	  read(line(1:leng),'(3f12.4)') b1(nant),b2(nant),b3(nant)
	  write(line,'(a,3f12.4)')'Inputs x,y,z:   ',
     *				b1(nant),b2(nant),b3(nant)
	  call output(line(1:53))
          umsg = 'UVGEN: '//line(1:62)
	  call hiswrite(unit, umsg )
c
c  Convert to equatorial coordinates.
c
	  if(utns .lt. 0.) then
	    x = b1(nant)
	    z = b3(nant)
	    b1(nant) = -x * sinl + z * cosl
	    b3(nant) =  x * cosl + z * sinl
 	  end if
c
c  Convert to nanosecs.
c
	  if(utns .ne. 0.) then
	    b1(nant) = abs(utns) * b1(nant)
	    b2(nant) = abs(utns) * b2(nant)
	    b3(nant) = abs(utns) * b3(nant)
 	  end if
	  write(line,'(a,3f12.4)') 'Equatorial (ns):',
     *				b1(nant),b2(nant),b3(nant)
	  call output(line(1:53))
	  call txtread(tunit,line,leng,status)
	enddo
c
	call txtclose(tunit)
	if(nant.eq.MAXANT)
     *	  call bug('w','Max number of antenna positions read')
	if(dopolar)then
	  do ipolar=1,npolar
	    if(len1(polar(ipolar)).lt.nant) call bug('f',
     *            'Less than NANT characters in polarization cycle')
	  enddo
	endif
c
c  Get frequency/correlator parameters.
c
	call modcor(corfile,tunit)
c
	call txtread(tunit,line,leng,status)
	read(line(1:leng),'(i5,i2,8f9.2,3f8.1)')
     *		nchan,nospect,corfin,corbw,famp,fcen,fwid
	write(line,150) (corfin(i),i=1,4), (corbw(i),i=1,4)
	call output(line)
        umsg = 'UVGEN: '//line
	call hiswrite(unit, umsg)
150	format('Correlator freq:',4F7.2,' BW:',4F7.2, ' MHz')
c
	write(line,'(A,F7.2,A,F7.2,A,F7.2,A)')
     *	 'Line/continuum:',FAMP,' center:', FCEN,', width:',FWID,' GHz'
	call output(line)
	umsg = 'UVGEN: '//line
	call hiswrite(unit, umsg)
c
	call txtclose(tunit)
c
c  Calculate spectra frequencies from correlator setup
c
	if(telescop.eq.'HATCREEK')then
	  call coramhat(nospect,nchan,corfin,corbw,freq,iffreq)
	else
	  call coramoth(nospect,nchan,corfin,corbw,freq)
	endif
c
c  Give some messages to the user.
c
	write(line,'(a,i5,a,i5,a,i5)')'Channels:', numchan,
     *		'   Spectra:', nspect, '   Wideband:', nwide
	call output(line)
	umsg = 'UVGEN: '//line
	call hiswrite(unit, umsg)
c
c  If its a wide-only dataset, tell uvset as much.
c
	if(numchan.eq.0)then
	  if(nwide.eq.0)call bug('f','No channels to be generated')
	  call uvset(unit,'data','wide',0,1.,1.,1.)
	endif
c
c  Fill data header record.
c
	call wrhda(unit,'obstype','crosscorrelation')
	call uvputvra(unit,'source',outfile)
	call uvputvra(unit,'operator','uvgen')
	call uvputvra(unit,'version',version)
c
	call uvputvrd(unit,'ra',sra,1)
	call uvputvrd(unit,'obsra',sra,1)
	call uvputvrd(unit,'dec',sdec,1)
	call uvputvrd(unit,'obsdec',sdec,1)
	if(pbfwhm.gt.0)call uvputvrr(unit,'pbfwhm',3600*180/pi*pbfwhm,1)
c
	call uvputvrd(unit,'lo1',lo1,1)
	call uvputvrd(unit,'lo2',lo2,1)
	call uvputvrd(unit,'freq',freq,1)
	call uvputvrd(unit,'freqif',freqif,1)
	call uvputvrd(unit,'latitud',alat,1)
	call uvputvrd(unit,'longitu',0.d0,1)
c
c  Miscellaneous.
c
	if(telescop.eq.'ATCA')then
	  evector = pi/4
	else
	  evector = 0
	endif
	call uvputvrr(unit,'evector',evector,1)
	call uvputvra(unit,'telescop',telescop)
c
c  Fake some header information.
c
	systemp(1) = tsys
	jj = 1
	do j = 2,nant*max(nwide,nspect)
	  jj = jj + 1
	  systemp(jj)=tsys
	end do
	call uvputvrr(unit,'systemp',systemp,max(1,nant*nspect))
	call uvputvrr(unit,'wsystemp',systemp,max(1,nant*nwide))

	call uvputvrr(unit,'jyperk',jyperk,1)
	inttime = max(3600*hint,1.)
	call uvputvrr(unit,'inttime',inttime,1)
	call uvputvrr(unit,'vsource',0.,1)
	call uvputvrr(unit,'veldop',0.,1)
	call uvputvrr(unit,'epoch',1950.,1)
	call uvputvri(unit,'nants',nant,1)
	call uvputvri(unit,'ntemp',0,1)
	call uvputvri(unit,'npol',npol,1)
	call wrhdi(unit,'npol',npol)
	if(npol.eq.1) call uvputvri(unit,'pol',pol(1),1)
c
	if(nspect.gt.0)then
	  call uvputvri(unit,'nchan',numchan,1)
	  call uvputvri(unit,'nspect',nspect,1)
	  call uvputvrd(unit,'sfreq',sfreq,nspect)
	  call uvputvrd(unit,'sdf',sdf,nspect)
	  call uvputvri(unit,'ischan',ischan,nspect)
	  call uvputvri(unit,'nschan',nschan,nspect)
	  do i=1,nspect
	    restfreq(i)=freq
	  end do
	  call uvputvrd(unit,'restfreq',restfreq,nspect)
	endif
c
	if(nwide.gt.0)then
	  call uvputvri(unit,'nwide',nwide,1)
	  call uvputvrr(unit,'wfreq',wfreq,nwide)
	  call uvputvrr(unit,'wwidth',wwidth,nwide)
	endif
C
	do jj=1,nant
	  antpos(jj) = b1(jj)
	  antpos(jj+nant) = b2(jj)
	  antpos(jj+nant*2) = b3(jj)
        end do
	call uvputvrd(unit,'antpos',antpos,nant*3)
c
c  Calculate random noise based on Tsys, integration time and bandwidth.
c
	do i=1,nwide
	  wrms(i,1) = jyperk*tsys / sqrt(2*wwidth(i)*1e9*inttime)
	  wsignal(i) = 0
	  do ipol=2,npol
	    wrms(i,ipol) = wrms(i,1)
	  enddo
	enddo
	do j = 1,nspect
	  temp = jyperk * tsys / sqrt(2 * abs(sdf(j)) * 1e9 * inttime)
	  do ipol=1,npol
	    do i = ischan(j), ischan(j)+nschan(j)-1
	      rrms(i,ipol) = temp
	    enddo
	  enddo
	enddo
c
c  Divide noise levels by sqrt(2) if its a true Stokes correlation.
c
	do ipol=1,npol
	  if(pol(ipol).gt.0)then
	    do i=1,nwide
	      wrms(i,ipol) = wrms(i,ipol)/sqrt2
	    enddo
	    do i=1,numchan
	      rrms(i,ipol) = rrms(i,ipol)/sqrt2
	    enddo
	  endif
	enddo
c
c  Determine the rms for each polarisation.
c
c
c  Write noise info to the history file.
c
	write(line,170) Tsys,Trms,Telev,Tatm
170	format('Tsys(K): ',f6.0,'  Trms(K): ',f8.2,
     *			'  Telev(K): ',f6.0,'  Tatm(K/rad): ',f5.2)
	call output(line)
	umsg = 'UVGEN: '//line
	call hiswrite(unit, umsg )
c
	if(nwide.gt.0)then
	  write(line,171)  (1e3*wwidth(i), i=1,nwide)
171	  format('Wide widths (MHz): ',8f8.3)
	  call output(line)
	  umsg = 'UVGEN: '//line
	  call hiswrite(unit, umsg )
c
	  write(line,172) (wrms(i,1),i=1,nwide)
172	  format('Wide noise   (Jy): ',8f8.3)
	  call output(line)
	  umsg = 'UVGEN: '//line
	  call hiswrite(unit, umsg )
	endif
c
	if(nspect.gt.0)then
	  write(line,173)  (1e3*sdf(i),i=1,nspect)
173	  format('Channel widths (MHz): ',8f8.3)
	  call output(line)
	  umsg = 'UVGEN: '//line
	  call hiswrite(unit, umsg )
c
	  write(line,174)  (rrms(nschan(i),1),i=1,nspect)
174	  format('Channel noise   (Jy): ',8f8.3)
	  call output(line)
	  call hiswrite(unit, 'UVGEN: '//line)
	endif
c
c  Miscellaneous initialization.
c
	item = 0
	call Randset(nint(10*timeout))
c	timeout = int(timeout-0.5) + 0.5
c
c  Initialise the effective source parameters, to account for pointing
c  center and 
c
	dopoint = npnt.gt.1
	if(.not.dopoint)call GetSrc(pbfwhm,sra,sdec,center(1,1),
     *	    ns,ta,smaj,smin,spa,sx,sy,tad,smajd,smind,sxd,syd,szd)
c
c  Determine the polarization leakage parameters.
c
	if(doleak)then
	  call output('Polarization Leakage Terms')
	  call hiswrite(unit,'UVGEN: Polarization Leakage Terms')
	  call Gaus(Leak,4*nant)
	  do i=1,nant
	    Leak(1,i) = leakrms * leak(1,i)
	    Leak(2,i) = leakrms * leak(2,i)
	    write(line,'(1x,a,i2,a,f6.3,a,f6.3,a,f6.3,a,f6.3,a)')
     *        'Ant',i,':Dx,Dy = (',real(Leak(1,i)),',',
     *				  aimag(Leak(1,i)),'),(',
     *				   real(Leak(2,i)),',',
     *				  aimag(Leak(2,i)),')'
	    call output(line)
	    call hiswrite(unit,'UVGEN: '//line)
	  enddo
	endif
c
c  Start the polarization switching cycle.
c
	ipolar = -1
c
c  Compute visibility for each hour angle.
c
	ha = hbeg
	ipnt = 0
	ra = sra
	dec = sdec
	dowhile(ha.lt.hend)
	  haend = min(ha + cycleon,hend)
c
c  Increment the polarization switching cycle.
c
	if(dopolar) then
	  ipolar = mod(ipolar+1,npolar)
	  xpolar = polar(ipolar+1)
	endif
c
c  Compute effective source parameters when mosaicing.
c
	  ipnt = mod(ipnt,npnt) + 1
	  if(dopoint)then
	    call uvputvrr(unit,'dra',center(1,ipnt),1)
	    call uvputvrr(unit,'ddec',center(2,ipnt),1)
	    call GetSrc(pbfwhm,sra,sdec,center(1,ipnt),
     *	      ns,ta,smaj,smin,spa,sx,sy,tad,smajd,smind,sxd,syd,szd)
	    dec = sdec + center(2,ipnt)
	    sind = sin(dec)
	    cosd = cos(dec)
	    ra = sra + center(1,ipnt) / cos(sdec)
	    if(ra.lt.0) ra = ra + 2*dpi
	  endif
c
c  Compute new antenna gains.
c
	  if(dogains)then
	    do n = 1, nant
	      pnoise(n) = rang(0.,prms)
	      gain(n) = rang(1.,arms) * expi(pnoise(n))
	    enddo
	  endif
c
	  dowhile(ha.lt.haend)
	    lst = ha * pi / 12 + ra
	    if (lst.lt.0.d0) lst = lst + 2*pi
	    h = lst - ra
	    call uvputvrd(unit,'ut',lst,1)
	    call uvputvrd(unit,'lst',lst,1)
	    preamble(4) = timeout + ha/24.
	    sinha = sin(h)
	    cosha = cos(h)
	    sinq = cosl*sinha
	    cosq = sinl*cosd - cosl*sind*cosha
	    sinel=sinl*sind+cosl*cosd*cosha
c
c  Offset the parallactic angle by evector.
c
	    psi = atan2(sinq,cosq) + evector
	    call uvputvrr(unit,'chi',psi,1)
c
c  Compute total power variations for each antenna.
c
	    if (trms + telev + tatm .gt. 0.) then
	      cosel=sqrt(1.-sinel*sinel)
	      do n = 1, nant
	        tpower(n) = tsys + rang(1.,trms) + telev * cosel 
     * 						 + tatm  * pnoise(n) 
	      enddo
	      call uvputvri(unit,'ntpower',nant,1)
	      call uvputvrr(unit,'tpower',tpower,nant)
	    endif
c
c  Compute visibility for each baseline.
c
	    do n = 2, nant
	      do m = 1, n-1
	        preamble(5) = 256*m + n
	        bxx = b1(n) - b1(m)
	        byy = b2(n) - b2(m)
	        bzz = b3(n) - b3(m)
		bxy = bxx * sinha + byy * cosha
		byx =-bxx * cosha + byy * sinha
	        preamble(1) = bxy
	        preamble(2) =  byx*sind + bzz*cosd
		preamble(3) = -byx*cosd + bzz*sind
		baseline = 3.e-4 * sqrt(bxx*bxx + byy*byy + bzz*bzz)
c
c  Find the polcode for the polarization switching cycle.
c
		if(dopolar)then
		  pol(1) = PolsP2C(xpolar(m:m)//xpolar(n:n))
		endif
c
c  Calculate wideband correlations.
c
		if(nwide.gt.0)then
	          do ipol = 1, npol
		    do is = 1,nwide
		      call modvis(preamble(1),preamble(2),preamble(3),
     *			dble(wfreq(is)),ns,tad,sxd,syd,szd,smaj,smind,
     *			spa,per,pa,pol(ipol),psi,vis)
		      wsignal(is) = wsignal(is) + 
     *					real(vis)**2 + aimag(vis)**2
	              wcorr(is,ipol) = vis
	            enddo
		  enddo
		  if(doatm) gatm = expi(
     *		     rang(0.,patm * baseline**pslope * sinel**pelev))	
		  if(doleak)
     *		     call PolLeak(wcorr,nwide,maxspect,npol,
     *			leak(1,m),leak(1,n))
		  if(dogains)
     *		     call AntGain(wcorr,nwide,maxspect,npol,
     *			Gain(m)*conjg(gain(n)*gatm))
		  if(donoise)
     *		     call NoiseAdd(wcorr,nwide,maxspect,npol,wrms)
	        endif
c
c  Calculate spectral data.  Compute model spectra, and add random noise.
c
	        if(numchan.gt.0) then
		  do ipol=1,npol
		    do is = 1, nspect
		      do ic = ischan(is), ischan(is)+nschan(is)-1
		        freq = sfreq(is) + (ic-ischan(is))*sdf(is)
		        call modvis(preamble(1),preamble(2),preamble(3),
     *			  freq,ns,tad,sxd,syd,szd,smajd,
     *			  smind,spa,per,pa,pol(ipol),psi,vis)
		        if (famp.ne.0. .and. fwid.ne.0.)
     *		          vis = vis * (1.+ famp *
     *			  (1.-min(1.,abs((real(freq)-fcen)/fwid))))
		        chan(ic,ipol) = vis
		      enddo
		    enddo
		  enddo
		  if(dogains)
     *		     call AntGain(chan,numchan,MAXCHAN,npol,
     *			Gain(m)*conjg(gain(n)))
		  if(doleak)
     *		     call PolLeak(chan,numchan,MAXCHAN,npol,
     *			leak(1,m),leak(1,n))
		  if(donoise)
     *		     call NoiseAdd(chan,numchan,MAXCHAN,npol,rrms)
	        endif

c
c  Write data records.
c
		do ipol=1,npol
                  if(npol.gt.1.or.npolar.gt.1) 
     *                  call uvputvri(unit,'pol',pol(ipol),1)
	          if(numchan.gt.0) then
		    if(nwide.gt.0)
     *		      call uvwwrite(unit,wcorr(1,ipol),flags,nwide)
	            call uvwrite(unit,preamble,chan(1,ipol),flags,
     *								numchan)
	          else
		    call uvwrite(unit,preamble,wcorr(1,ipol),flags,
     *								nwide)
	          endif
	          item = item + 1
	        enddo
c
c  Finished doing two antenna number do loops, and two ha dowhile loops.
c
	      enddo
	    enddo
	    ha = ha + hint
	  enddo
	  ha    = ha + cycleoff
	enddo
c
c  All done. Summarize, tidy up and exit.
c
	write(line,'(i5,a,a)')
     *	  Item,' records written to file:',outfile
	call output(line)
	umsg = 'UVGEN: '//line
	call hiswrite(unit, umsg )
c
c  Write out the signal to noise ratio in the wide channels.
c
	if(nwide.gt.0.and.tsys.gt.0)then
	  do i=1,nwide
	    wsignal(i) = sqrt(wsignal(i)/(2*item*wrms(i,1)*wrms(i,1)))
	  enddo
	  write(line,180) (wsignal(i),i=1,nwide)
180	  format('Wide SNR: ',8f8.3)
	  call output(line)
	  umsg = 'UVGEN: '//line
	  call hiswrite(unit, umsg )
	endif
c
	call hisclose(unit)
	call uvclose(unit)
	end
c************************************************************************
	subroutine GetSrc(pbfwhm,ra0,dec0,center,
     *	    ns,ta,smaj,smin,spa,sx,sy,tad,smajd,smind,sxd,syd,szd)
c
	implicit none
	integer ns
	double precision ra0,dec0
	real pbfwhm,center(2)
	real ta(ns),smaj(ns),smin(ns),spa(ns),sx(ns),sy(ns)
	real tad(ns),smajd(ns),smind(ns),sxd(ns),syd(ns),szd(ns)
c
c  Calculate the effective source parameters, considering the primary
c  beam, and offset pointing.
c
c  Input:
c    unit	Handle of the output data-set. Only used if
c
c------------------------------------------------------------------------
	integer i
	double precision ra,dec,rap,decp,cosdec0
	real ll,mm
c
c  Get the true pointing center RA and DEC.
c
	cosdec0 = cos(dec0)
	rap  = ra0  + center(1)/cosdec0
	decp = dec0 + center(2)          
c
c  Derive the true direction cosines relative to the pointing centre.
c
	do i=1,ns
	  ra  = ra0  + sx(i) / cosdec0
	  dec = dec0 + sy(i)
	  ll = sin(ra-rap) * cos(dec)
	  mm = sin(dec)*cos(decp) - cos(ra-rap) * cos(dec) * sin(decp)
	  call points(pbfwhm,
     *     smaj(i),smin(i),spa(i),ta(i),ll,mm,smajd(i),smind(i),tad(i),
     *	   sxd(i),syd(i),szd(i))
	enddo
c
	end
c************************************************************************
	subroutine points(fwhmpb,fwhm1, fwhm2, pa,flux, x0, y0,
     *				 fwhm1d,fwhm2d,   fluxd,x0d,y0d,z0d)
c
	implicit none
	real fwhmpb,fwhm1,fwhm2,pa,flux,x0,y0
	real fwhm1d,fwhm2d,fluxd,x0d,y0d,z0d
c
c  Determine the parameters of a gaussian source, attenuated by a
c  primary beam.
c
c  Input:
c    fwhmpb	FWHM of the primary beam (radians). A negative or zero
c		value indicates an infinite primary beam.
c    fwhm1,fwhm2 Gaussian FWHM of the source (radians).
c    pa		Position angle of the source (radians).
c    flux	Source flux.
c    x0,y0	True direction cosines.
c  Output:
c    fwhm1d,fwhm2d Gaussian FWHM of the source after attenuation (radians).
c    fluxd	 Source flux after attenuation.
c    x0d,y0d,z0d Direction cosines after attenuation (radians).
c------------------------------------------------------------------------
	real x1,y1,temp
c
c
	if(fwhmpb.gt.0)then
	  x1 = cos(pa)*x0 - sin(pa)*y0
	  y1 = sin(pa)*x0 + cos(pa)*y0
c
	  x1 = x1 * fwhmpb**2 / (fwhm1**2 + fwhmpb**2)
	  y1 = y1 * fwhmpb**2 / (fwhm2**2 + fwhmpb**2)
c
	  x0d =  cos(pa)*x1 + sin(pa)*y1
	  y0d = -sin(pa)*x1 + cos(pa)*y1
c
	  fwhm1d = fwhmpb*fwhm1/sqrt(fwhmpb**2 + fwhm1**2)
	  fwhm2d = fwhmpb*fwhm2/sqrt(fwhmpb**2 + fwhm2**2)
	  temp = 4*log(2.)*(x0*x0d + y0*y0d)/(fwhmpb**2)
	  fluxd = flux * exp(-temp)
	else
	  x0d = x0
	  y0d = y0
	  fwhm1d = fwhm1
	  fwhm2d = fwhm2
	  fluxd = flux
	endif
c
	z0d = sqrt(1 - x0d*x0d - y0d*y0d)
c
	end
c************************************************************************
	subroutine PolLeak(vis,nchan,maxchan,npol,D1,D2)
c
	implicit none
	integer nchan,maxchan,npol
	complex vis(maxchan,npol)
	complex D1(2),D2(2)
c
c  Allow some of the signal to leak from one polarization to another. Despite
c  the variable names, this works equally well fro circular and linear feeds
c  (the algebra is the same).
c
c  NOTE: The number of polarisations must be 4, and they must be in the
c  order XX,YY,XY,YX (or RR,LL,RL,LR).
c
c  Input:
c    D1,D2	Leakage terms of the two antennas. Leak1(1) is the leakage
c   		of Y into the X feed (or L into R) for antenna 1. Leak1(2) is the
c    		leakage of X into the Y (or R into L) for antenna 1.
c    npol	Number of polarizations. It must be 4!
c    maxchan	Size of the "vis" array.
c    nchan	Number of used channels.
c  Input/Output:
c    vis	The visibility data.
c------------------------------------------------------------------------
	integer X,Y,XX,YY,XY,YX
	parameter(X=1,Y=2,XX=1,YY=2,XY=3,YX=4)
	integer i
	complex Vxx,Vxy,Vyx,Vyy
c
	if(npol.ne.4)call bug('f','npol not 4, in PolLeak')
c
	do i=1,nchan
	  Vxx = vis(i,XX)
	  Vyy = vis(i,YY)
	  Vxy = vis(i,XY)
	  Vyx = vis(i,YX)
	  vis(i,XX) = Vxx + D1(X)*Vyx + conjg(D2(X))*Vxy
     *			  + D1(X)*      conjg(D2(X))*Vyy
	  vis(i,YY) = Vyy + D1(Y)*Vxy + conjg(D2(Y))*Vyx
     *			  + D1(Y)*      conjg(D2(Y))*Vxx
	  vis(i,XY) = Vxy + D1(X)*Vyy + conjg(D2(Y))*Vxx
     *			  + D1(X)*      conjg(D2(Y))*Vyx
	  vis(i,YX) = Vyx + D1(Y)*Vxx + conjg(D2(X))*Vyy
     *			  + D1(Y)*      conjg(D2(X))*Vxy
	enddo
c
	end
c************************************************************************
	subroutine AntGain(vis,nchan,maxchan,npol,Gain)
c
	implicit none
	integer nchan,maxchan,npol
	complex vis(maxchan,npol)
	complex Gain
c
c  Apply a gain to the data.
c
c  Input:
c    Gain	The gain to apply.
c    npol	Number of polarizations.
c    maxchan	Size of the "vis" array.
c    nchan	Number of used channels.
c  Input/Output:
c    vis	The visibility data.
c------------------------------------------------------------------------
	integer i,j
c
	do j=1,npol
	  do i=1,nchan
	    vis(i,j) = Gain * vis(i,j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine NoiseAdd(vis,nchan,maxchan1,npol,rms)
c
	implicit none
	integer nchan,maxchan1,npol
	complex vis(maxchan1,npol)
	real rms(maxchan1,npol)
c
c  Apply a gain to the data.
c
c  Input:
c    rms	Rms value of the noise to add.
c    npol	Number of polarizations.
c    maxchan	Size of the "vis" array.
c    nchan	Number of used channels.
c  Input/Output:
c    vis	The visibility data.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	complex data(MAXCHAN)
	if(nchan.gt.MAXCHAN)call bug('f','Too many chans, in NoiseAdd')
c
	do j=1,npol
	  call Gaus(data,2*nchan)
	  do i=1,nchan
	    vis(i,j) = vis(i,j) + rms(i,j) * data(i) 
	  enddo
	enddo
c
	end
c************************************************************************
	real function rang(xmean,xsd)
c
	implicit none
	real xsd,xmean
c
c  This generates a gaussian random number with mean "xmean" and standard
c  deviation "xsd".
c------------------------------------------------------------------------
	real data(5)
c
	call uniform(data,5)
	rang = data(1) + data(2) + data(3) + data(4) + data(5)
	rang = (rang*0.2-0.5) * xsd * sqrt(60.0) + xmean
	end
c************************************************************************
	subroutine GetPol(pol,npol,maxpol)
c
	implicit none
	integer npol,maxpol
	integer pol(maxpol)
c
c  Get the polarisations that the user wants to form.
c
c  Input:
c    maxpol	Max number of polarisations than this can handle.
c  Output:
c    npol	The number of polarisations that the user wants to form.
c    pol	The polarisations that the user wants to form.
c------------------------------------------------------------------------
	integer i,j,t
	character user*4
c
c  Externals.
c
	integer PolsP2C
	character PolsC2P*2
	logical keyprsnt
c
	call keya('stokes',user,'i')
	npol = 0
	dowhile(user.ne.' '.and.npol.lt.maxpol)
	  npol = npol + 1
	  pol(npol) = PolsP2C(user)
	  call keya('stokes',user,' ')
	enddo
	if(keyprsnt('stokes'))
     *	  call bug('f','Too many stokes/polarizations for me to handle')
c
c  Sort the polarisations into descending order. Bubble sort because who cares
c  with only 4 elements.
c
	do j=1,npol-1
	  do i=1,npol-j
	    if(pol(i).lt.pol(i+1))then
	      t = pol(i)
	      pol(i) = pol(i+1)
	      pol(i+1) = t
	    endif
	  enddo
	enddo
c
c  Squeeze out replications (pretty paranoid eh?).
c
	j = 1
	do i=2,npol
	  if(pol(i).ne.pol(j))then
	    j = j + 1
	    pol(j) = pol(i)
	  endif
	enddo
	npol = j
c
c  Check that the max polarisation is 1.
c
	if(pol(1).gt.1)call bug('f','Unsupported polarisation '//
     *	  PolsC2P(pol(1)))
c
	end
c************************************************************************
	subroutine modvis (uns,vns,wns,freq,ns,ta,sx,sy,sz,
     *		smaj,smin,spa,per,pa,code,psi,cont)
c
	implicit none
	double precision uns,vns,wns,freq
	integer ns,code
	real ta(ns),sx(ns),sy(ns),sz(ns),smaj(ns),smin(ns)
	real spa(ns),per(ns),pa(ns)
	real psi
	complex cont
c
c  This computes the visibility of model source distribution of linearly
c  polarized Gaussian components, observed with linear or circular feeds,
c  at paralactic angle PSI.
c
c  Input:
c    uns,vns,wns The (u,v,w) coordinate of the visibility, in nanosec.
c    freq	The frequency of the visibility, in GHz.
c    code	Polarisation code (AIPS/FITS style).
c    ns		Number of gaussians in the model.
c    psi	Parallactic angle, in radians.
c	The following give parameters of the gaussians.
c    ta		Peak flux, in Jy.
c    sx,sy,sz	l,m,n direction cosines.
c    smaj,smin	Gaussian source FWHM size in the image plane.
c    spa	Gaussian position angle, in radians.
c    per	Fractional polarisation (in range [0,1]).
c    pa		Position angle of the polarisation, in radians.
c  Output:
c    cont	The computed visibility.
c------------------------------------------------------------------------
	integer PolI,PolRR,PolLL,PolRL,PolLR,PolXX,PolYY,PolXY,PolYX
	parameter(PolI=1,PolRR=-1,PolLL=-2,PolRL=-3,PolLR=-4)
	parameter(	 PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
	real pi
	parameter(pi=3.141592653589793)
	real p,umaj,umin,uvmaj,uvmin,gaus,flux
	complex vis
	double precision u,v,w
	integer j
	logical inten(-8:1)
c
c  Externals.
c
	complex expi
c
	data inten/.false.,.false.,.true.,.true.,
     *		   .false.,.false.,.true.,.true.,
     *		   .false.,.true./
c
c  Convert u and v to wavelengths.
c
	u = uns * freq
	v = vns * freq
	w = wns * freq
c
c  Loop over the source components. Avoid as much work as possible by
c  checking for situations where the component contributes nothing.
c
	cont = (0.,0.)
	do j = 1,ns
	  flux = 0
	  if(per(j).ne.0.or.inten(code))then
	    uvmaj = 1. / ( pi * 0.6 * smaj(j) )
	    uvmin = 1. / ( pi * 0.6 * smin(j) )
	    umaj = (u * sin(spa(j)) + v * cos(spa(j))) / uvmaj
	    umin = (u * cos(spa(j)) - v * sin(spa(j))) / uvmin
	    gaus = umaj*umaj + umin*umin
	    if (gaus .lt. 20) flux = ta(j) * exp(-gaus)
	  endif
c
c  Determine the contribution for the various polarisations.
c  NOTE! I have multiplied the RL and LR correlations by expi(2*psi),
c  as it is the VLA convention to perform this on line. The polarisation
c  handling software in Miriad assumes this!!!!
c
	  if (flux.ne.0)then
	    if((per(j).eq.0.and.inten(code)).or.
     *		code.eq.PolI.or.code.eq.PolRR.or.code.eq.PolLL)then
	      vis = 1
	    else if(code.eq.PolXX)then
	      vis = 1 + per(j) * cos(2.* (pa(j) - psi))
	    else if(code.eq.PolYY)then
	      vis = 1 - per(j) * cos(2.* (pa(j) - psi))
	    else if(code.eq.PolXY.or.code.eq.PolYX)then
	      vis = per(j) * sin(2.* (pa(j) - psi))
	    else if(code.eq.PolRL)then
	      vis = per(j) * expi(-2.* (pa(j)))
	    else if(code.eq.PolLR)then
	      vis = per(j) * expi( 2.* (pa(j)))
	    else
	      call bug('f','Unsupported polarization, in ModVis')
	    endif
	    p=2*pi*(u*sx(j)+v*sy(j)+w*(sz(j)-1))
	    cont = cont + vis * expi(p) * flux
	  endif
	enddo
	end
c************************************************************************
	subroutine modcomp(sfile,tunit)
c
	implicit none
	character sfile*(*)
	integer tunit
c
c	open or create source component list for model program
c		mchw	july 1983
c------------------------------------------------------------------------
	integer status
	character*90 line
        character*80 umsg
c
	integer leng
	real flux,ra,dec,wmaj,wmin,wpa,pol,pa

	call txtopen(tunit,sfile,'old',status)
	if (status.ne.0) then
	  call txtopen(tunit,sfile,'new',status)
	  call output('Enter Flux,position,FWHP size and polarization')
	  call output(' End list with RETURN')
	  call output(' Enter 8 numbers separated by commas')
          umsg = ' Flux  Offset   Offset   Major  Minor  Axis '//
     *	         '  Polar    Polar'
	  call output( umsg )
          umsg = '         Ra      Dec     Axis   Axis   Angle'//
     *           '  ization  Angle'
	  call output( umsg )
          umsg = ' (Jy)   (")      (")     (")    (")    (deg)'//
     *           '   (%)      (deg)'
	  call output( umsg )

10	  call prompt(line,leng,'F,RA,DEC,M,m,A,P%,A:')
	  if(leng.eq.0) goto 20
	    read(line(1:leng),'(8F15.0)')
     *			 flux,ra,dec,wmaj,wmin,wpa,pol,pa
	    if (wmaj .eq. 0.) wmaj = 0.0001
	    if (wmin .eq. 0.) wmin = 0.0001
	    write(line,'(8F10.4)') flux,ra,dec,wmaj,wmin,wpa,pol,pa
	    call txtwrite(tunit,line,80,status)
	  goto 10
c
20	  call txtclose(tunit)
c
	  call txtopen(tunit,sfile,'old',status)
	end if
	end
c************************************************************************
	subroutine modant(antfile,tunit)
c
	implicit none
	character antfile*(*)
	integer tunit
c
c	create antenna position file for model programs
c		mchw	july 1983
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer status
	character*90 line
        character*80 umsg
c
	integer n,leng
	real bx,by,bz

	call txtopen(tunit,antfile,'old',status)
	if (status.ne.0) then
	  call txtopen(tunit,antfile,'new',status)
          umsg = 'Enter antenna coordinates: bx,by,bz '//
     *	         '(or North,East,Elev)'
	  call output( umsg )
	  call output(' End list with RETURN')

	  n=0
10	  call prompt(line,leng,'Bx,By,Bz:')
	  if(leng.eq.0) goto 20
	    read(line(1:leng),'(3F20.0)') bx,by,bz
	    write(line,'(3F12.4)') bx,by,bz
	    call txtwrite(tunit,line,36,status)
	    n=n+1
	  if(n.lt.MAXANT) go to 10
	  write (umsg,'(a,i2,a)')'Maximum of ',MAXANT,' antennas'
	  call output(umsg)

20	  call txtclose(tunit)

	  call txtopen(tunit,antfile,'old',status)
	end if
	end
c************************************************************************
	subroutine modcor(corfile,tunit)
c
	implicit none
	character corfile*(*)
	integer tunit
c
c  Create correlator settup file.
c------------------------------------------------------------------------
	integer status
	real corfin(4),corbw(4)
	character*120 line
	integer leng,nospect,numchn,i
	real famp,fcen,fwid
c	data nospect/0/,numchn/0/,famp/0./,fcen/0./,fwid/0./
c
c  Initialize variables.
c
	nospect = 0
	numchn = 0
	famp = 0
	fcen = 0
	fwid = 0
	do i=1,4
	  corfin(i) = 0.
	  corbw(i) = 0.
	enddo
c
	call txtopen(tunit,corfile,'old',status)
	if (status.ne.0) then
	  call txtopen(tunit,corfile,'new',status)
	  call output('Enter correlator parameters')
20	  call prompt(line,leng,
     *      'Enter number of spectral windows (1 to 4): ')
	  read(line(1:leng),'(i4)',err=20) nospect
c
25	  call prompt(line,leng,
     *      'Enter number of channels per spectra (0=wideband only): ')
	  read(line(1:leng),'(i4)',err=25) numchn
c
30	  call prompt(line,leng,
     *		'Enter 4 correlator frequencies (70 - 900) MHz: ')
	  read(line(1:leng),'(4f20.0)',err=30) (corfin(i),i=1,4)
c
40	  call prompt(line,leng,
     *		'Enter 4 correlator bandwidths (6.25 - 100) MHz: ')
	  read(line(1:leng),'(4f20.0)',err=40) (corbw(i),i=1,4)
c
50	  call output('Enter model spectrum parameters')
	  call prompt(line,leng,
     *	   'Enter line/continuum ratio, center freq and width (GHz): ')
	  read(line(1:leng),'(3f20.0)',err=50) famp, fcen, fwid
c
	  write(line,'(i5,i2,8f9.2,3f8.1)')
     *		numchn, nospect, corfin, corbw, famp, fcen, fwid
	  call txtwrite(tunit,line,87,status)
	  call txtclose(tunit)
	  call txtopen(tunit,corfile,'old',status)
	endif
	end
c************************************************************************
	subroutine coramoth(nospect,nchan,corfin,corbw,freq)
c
	implicit none
	integer nospect,nchan
	real corfin(4),corbw(4)
	double precision freq
c
c  Derives spectra parameters from correlator setup.
c
c  Inputs:
c    nospect	The number of spectra.
c    nchan	Number of channels.
c    corfin	Correlator frequencies.
c    corbw	Correlator bandwidths.
c    freq	Frequency of primary line, in upper sideband.
c
c  Outputs via the uvgencom common.
c------------------------------------------------------------------------
	include 'uvgen.h'
c
c  Determine down-conversion chain characteristics.
c
	freqif = 4e-3*corbw(1)
	lo1 = freq - freqif
	lo2 = 0.d0
c
c  Set up the wide correlator characteristics.
c
	nwide = 1
	wfreq(1) = freq
	wwidth(1) = 1e-3*corbw(1)
c
c  Fill in spectral correlator details.
c
	if((nospect.ne.0.and.nospect.ne.1).or.
     *	   (nospect.eq.1.and.nchan.le.0))
     *	  call bug('f','Illegal value of nspect or nchan')
	nspect = nospect
	numchan = nspect*nchan
c
	if(nspect.eq.1)then
	  sdf(1) = 1e-3 * corbw(1) / numchan
	  sfreq(1) = freq - 0.5 * (numchan-1) * sdf(1)
	  ischan(1) = 1
	  nschan(1) = numchan
	endif
	end
c************************************************************************
	subroutine coramhat(nospect,nchan,corfin,corbw,freq,iffreq)
c
	implicit none
	integer nospect,nchan
	real corfin(4),corbw(4)
	double precision freq,iffreq
c
c  Derive spectra parameters from correlator setup.
c
c  Inputs:
c    nospect	The number of spectral windows.
c    nchan	Number of channels per spectra.
c    corfin	Correlator frequencies.
c    corbw	Correlator bandwidths.
c    freq	Frequency of primary line.
c    iffreq	Frequency of primary line in 2nd IF. 
c
c  Outputs via uvgencom common (in uvgen.h).
c
c  History:
c    mchw 22feb84  For revised 4 modes.
c    mchw 02feb94  generalized hybrid correlator.
c------------------------------------------------------------------------
	include 'uvgen.h'
	integer i
c
	lo2  = 1.27d0
	freqif = lo2 + iffreq
	lo1 = freq - freqif
	numchan = nospect*nchan
	if(numchan.gt.0)then
	  nspect = nospect
	  do i = 1,nspect
	    sdf(i) = 1e-3 * corbw(i) / nchan
	    sfreq(i) = lo1 + lo2 + 1e-3 * corfin(i)
	    ischan(i) = (i-1)*nchan + 1
	    nschan(i) = nchan
	  enddo
	else
	  nspect = 0
	endif
c
c  Determine characteristics of the wideband correlations.
c
	nwide = 2
	wfreq(1) = lo1 - freqif
	wfreq(2) = lo1 + freqif
	wwidth(1) = 0.
	do i = 1,nospect
	  wwidth(1) = wwidth(1) + 1e-3*corbw(i)
	enddo
	wwidth(2) = wwidth(1)
c
	end
