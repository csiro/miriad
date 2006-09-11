c************************************************************************
	program atfix
	implicit none
c
c= atfix - Apply various miscellaneous corrections to ATCA visibility data
c& rjs
c: uv analysis
c+
c	ATFIX performs various miscellaneous offline corrections appropriate
c	for ATCA 3mm data. Steps that can be performed are
c	  Apply gain/elevation effect
c	  Apply system temperature measurements
c	  Correct for incorrect baseline length and instrumental phase.
c	  Correct antenna table (when some antennas were off-line)
c
c	NOTE: This task will usually be used very early in the data reduction
c	process. It should be used before calibration. 
c@ vis
c	The names of the input uv data sets. No default.
c@ select
c	Standard visibility data selection. See the help on "select" for
c	more information. The default is to select all data.
c@ out
c	The name of the output uv data set. No default.
c@ dantpos
c	Currently the on-line system uses the antenna locations derived from 
c	preliminary observations.
c	If a higher precision solution for the baseline lengths is available,
c	you will want to correct your data to account for this.
c
c	This also invokes corrections of known instrumental phase errors.
c
c	The inputs are the equatorial coordinate offsets entered in the
c	following order (NO checking is done for consistency):
c	    dantpos = A1,X1,Y1,Z1,A2,X2,Y2,Z2,A3,X3,Y3,Z3,....
c	The input values are the antenna number and the three equatorial
c	coordinate offsets (entered in units of nanoseconds).  These input
c	values are added to the absolute coordinates read from the data.
c	Antenna present in the data but not included in the input value
c	list are treated as having a zero coordinate offset.
c
c	The arcane unit of nanoseconds is used for historical compatibililty.
c	Note 1 nanosec = 0.2997 meters.
c
c	A collection of parameter files giving the corrections to apply are
c	stored in $MIRCAT. These have names of the form "dantpos.yymmdd"
c	where "yymmdd" is the date of the start of a new array configuration.
c	Baseline corrections are believed to be constant between array
c	configurations. 
c
c	If a data file is present, you can instruct atfix to read this
c	directly using the indirect parameter input. For example, to read
c	parameters appropriate for a hypothetical array configuration
c	starting on 16 October 2002, use
c	    dantpos=@$MIRCAT/dantpos.021016
c@ tsyscal
c	The determines the way that system temperature measurements are
c	applied to the visibility data. Usually this will be used
c	to make changes to the way system temperature measurements have
c	been applied to the data by the on-line system. The default
c	is to interpolate values between successive system temperature
c	measurements. Possible values
c	for this parameter are:
c	  none        Do not apply any system temperature measurements. If the
c	              the measurements were applied on-line, they are undone.
c	  any         Do nothing. Leave the Tsys calibration as is.
c         constant    Apply a constant Tsys between successive Tsys measurements.
c	              This is the approach used by the on-line system at 3mm
c	              wavelength.
c	  extrapolate Apply a Tsys value between measurements which accounts
c	              for predicted changes in Tsys with elevation and measured
c	              weather. 
c         interpolate Interpolate a Tsys value between measurements which
c	              accounts for predicted changes in Tsys with elevation
c	              and measured weather. This is the default.
c@ options
c	Extra processing options. Several options can be given,
c	separated by commas. Minimum match is supported. There is only
c	one possible option at the moment:
c	  nogainel  This disables applying a instrumental gain/elevation
c	            correction to the data. Currently the gains of the
c	            antennas are a function of elevation. By default,
c	            a correction is made for gain/elevation effects.
c	  noinst    Do not correct for instrumental baseline effects. By
c	            default a phase correction is made for instrumental
c	            baseline effects.
c@ array
c	One of the flaws in the current ATCA datafiles is that antenna locations
c	are not recorded for antennas that are off-line (and hence not producing
c	data). While this might not seem a serious flaw, the off-line antennas
c	can still cause shadowing. In particular, this was an issue when
c	using the 3-antenna system in compact arrays.
c
c	By giving a value to this parameter, atfix will fill in any
c	antenna locations that are missing from the input visibility file.
c	NOTE: This just fills in missing antenna locations, it does not perform
c	any flagging of shadowed data.
c
c	The value given to this parameter is either an array configuration name
c	(e.g. EW352 or 750A) or a list of six station names (e.g. W106 or N3).
c	When giving the station names, these must be in the order of the antennas
c	(i.e. CA01, CA02, CA03 etc).
c
c	NOTE: When antennas are in a shuffled order, or for arrays using
c	the north spur, you should generally give the list of station
c	names, as the standard array configuration names assume the
c	standard antenna order (not the shuffled order).
c
c	If in doubt, see the on-line history of configurations:
c	  http://www.narrabri.atnf.csiro.au/operations/array_configurations/config_hist.html
c
c--
c  History:
c    04may03 rjs  Original version.
c    15may03 rjs  Better jyperk.
c    14aug03 rjs  Fix bug causing seg violation.
c    17sep03 rjs  New gain/elevation curve at 3mm.
c    19oct03 rjs  Update array tables.
c    14nov03 rjs  Fix bug in getjpk
c    06dec03 rjs  Fish out met parameters from the dataset directly.
c    19sep04 rjs  Changes to the way Tsys scaling is handled.
c    11oct04 rjs  Use jyperk=10 rather than 13 in Tsys correction.
c    24jun05 rjs  Updated gain/elevation curve at 3mm
c    10aug05 rjs  Fixed horrible bug related to filling in the array.
c	          Subsequent shadowing calculation would not work!
c    19aug05 rjs  Include correction for instrumental phase of CA01.
c    28aug05 rjs  Include correction for instrumental phase of CA05.
c    12dec05 rjs  It failed to use the select keyword.
c    29jan06 rjs  Major revisions.
c    18feb06 rjs  Various tidy up and change in defaults.
c    24feb06 rjs  Fix bug in tsyscal=none and some tidying.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	integer MAXSELS,ATANT
	parameter(version='AtFix: version 1.0 24-Feb-06')
	parameter(MAXSELS=256,ATANT=6)
c
	real sels(MAXSELS),xyz(3*MAXANT)
	character array(MAXANT)*8,aname*8,tsyscal*12
	integer lVis,lOut,vant,vgmet,vnif
	integer pol,npol,i,j,k
	logical updated,dogel,doinst,dobl
	character vis*64,out*64,type*1
	integer nschan(MAXWIN),nif,nchan,nant,length,na,ifreq
	real gel
	logical dojpk
c
	real delta(3,MAXANT)
	real freq0(MAXWIN),jyperk
	double precision ra,dec,lat,lst,az,el
c
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	double precision preamble(6),ptime,freq(MAXCHAN)
c
c  Externals.
c
	logical uvvarUpd,selProbe,hdPrsnt,keyprsnt
	integer uvscan
	real elescale
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call selInput('select',sels,MAXSELS)
	call keya('out',out,' ')
	call mkeya('array',array,MAXANT,nant)
	do i=1,nant
	  call ucase(array(i))
	enddo
c
	do i=1,ATANT
	  delta(1,i) = 0
	  delta(2,i) = 0
	  delta(3,i) = 0
	enddo
	dobl = .false.
	dowhile(keyprsnt('dantpos'))
	  call keyi('dantpos',i,0)
	  if(i.le.0.or.i.gt.ATANT)
     *	    call bug('f','Invalid values in dantpos parameter')
	  call keyr('dantpos',delta(1,i),0.0)
	  call keyr('dantpos',delta(2,i),0.0)
	  call keyr('dantpos',delta(3,i),0.0)
	  dobl = dobl.or.(abs(delta(1,i))+abs(delta(2,i))+
     *					  abs(delta(3,i)).gt.0)
	enddo
c
	call GetOpt(dogel,doinst,tsyscal)
	dobl = dobl.or.doinst
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
c    
c  This program cannot tolerate polarisation, visibility, increment
c  or window selection (for obscure reasons).
c
        if(selProbe(sels,'increment?',0.d0))
     *    call bug('f','AtFix not support select=inc')
        if(selProbe(sels,'visibility?',0.d0))
     *    call bug('f','AtFix not support select=vis')
        if(selProbe(sels,'polarization?',0.d0))
     *    call bug('f','AtFix does not support select=pol')
        if(selProbe(sels,'window?',0.d0))
     *	  call bug('f','AtFix does not support select=win')
c
c  Fill out the array name if needed.
c
	if(nant.eq.1)then
	  aname = array(1)
	  call ucase(aname)
	  call aryDec(aname,array,ATANT)
	  nant = ATANT
	endif
	if(nant.ne.0)then
	  if(nant.ne.ATANT)
     *	    call bug('f','Incorrect number of antenna locations')
	  call aryXyz(array,xyz,nant)
	endif
c
c  Get ready to copy the data.
c
	call uvopen(lVis,vis,'old')
	call SelApply(lVis,sels,.true.)
	call uvset(lVis,'preamble','uvw/time/baseline/pol',0,0.,0.,0.)
	call varInit(lVis,'channel')
c
	call uvvarIni(lVis,vnif)
	call uvvarSet(vnif,'nschan')
c
c  Check and warn about calibration tables.
c
        if(hdprsnt(lVis,'gains').or.hdprsnt(lVis,'leakage').or.
     *     hdprsnt(lVis,'bandpass'))
     *     call bug('f','Calibration tables present: this looks bad')
c
c  Get ready to handle the antenna location correction.
c
	if(nant.gt.0)then
	  call uvvarIni(lVis,vant)
	  call uvvarSet(vant,'antpos')
	endif
c
c  Get ready to handle gain/elevation correction.
c
	call uvvarIni(lVis,vgmet)
	call uvvarSet(vgmet,'nschan')
	call uvvarSet(vgmet,'sfreq')
	call uvvarSet(vgmet,'sdf')
	call uvvarSet(vgmet,'ra')
	call uvvarSet(vgmet,'obsra')
	call uvvarSet(vgmet,'dec')
	call uvvarSet(vgmet,'obsdec')
	call uvvarSet(vgmet,'telescop')
	call uvvarSet(vgmet,'latitud')
c
c  Open the output, and make its history.
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'ATFIX: Miriad '//version)
	call hisinput(lOut,'ATFIX')
	call hisclose(lOut)
c
c  Initialise things.
c
c
c  Do we need to fill the tables needed for system temperature calibration?
c
	if(tsyscal.ne.'any')then
	  call tcalIni(lVis,tsyscal)
	  call freqIni(lVis)
	  do while(uvscan(lVis,' ').eq.0)
            call freqGet(lVis,ifreq)
            call tcalUpd(lVis,ifreq)
          enddo
          call output('Finished scanning input, now writing output')
          call uvrewind(lVis)
	endif
c
c  Get first record of the final pass.
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	if(nchan.eq.0)call bug('f','No data found')
c
c  For anything by tsyscal=any, check that the "tcorr" variable is present.
c
	if(tsyscal.ne.'any')then
	  call uvprobvr(lVis,'tcorr',type,length,updated)
	  if(length.ne.1)call bug('f',
     *		'Required info for tsys calibration is missing')
	endif
	call uvrdvri(lVis,'nants',na,0)
	ptime = preamble(4) - 1
c
	dowhile(nchan.gt.0)
	  call uvrdvrr(lVis,'jyperk',jyperk,0.0)
	  call varCopy(lVis,lOut)
	  call uvrdvri(lVis,'pol',pol,0)
	  call uvrdvri(lVis,'npol',npol,0)
c
c  Check whether nif and nschan parameters have changed.
c
	  if(uvvarUpd(vnif))then
	    call uvprobvr(lVis,'nschan',type,length,updated)
	    nif = length
	    if(type.ne.'i'.or.length.le.0.or.length.gt.MAXWIN)
     *	        call bug('f','Invalid nschan parameter')
	    call uvgetvri(lVis,'nschan',nschan,nif)
	  endif
	  call felget(lVis,vgmet,nif,nschan,freq0,freq,nchan,
     *                                                  ra,dec,lat)
	  dojpk = .false.
c
c  Apply the Tsys correction, if needed.
c
	  if(tsyscal.ne.'any')call tcalApp(lVis,lOut,
     *					preamble(4),data,nchan)
c
c  Do antenna table correction, if needed.
c
	  if(nant.gt.0)then
	    if(uvvarUpd(vant))call aryFill(lVis,lOut,xyz,nant)
	  endif
c
c  For elevation-related changes, check if the time has changed, and so 
c  we need to update all the associated information.
c
	  if(dogel.or.dobl)then
	    if(abs(ptime-preamble(4)).gt.5.d0/86400.d0)then
	      ptime = preamble(4)
	      call getlst(lVis,lst)
	      call azel(ra,dec,lst,lat,az,el)
	    endif
	  endif
c
c  Apply gain/elevation correction, if needed.
c
	  if(dogel)then
	    k = 0
	    do i=1,nif
              gel = EleScale(el,freq0(i),preamble(5))
	      if(i.eq.1)jyperk = jyperk / gel
	      dojpk = .true.
	      do j=1,nschan(i)
	        k = k + 1
	        data(k) = data(k) / gel
	      enddo
	    enddo
	  endif
c
c  Apply baseline correction, if needed.
c
	  if(dobl)call blcorr(data,freq,nchan,ra,dec,lst,el,
     *				preamble(5),delta,ATANT,doinst)
c
	  if(npol.gt.0)then
	    call uvputvri(lOut,'npol',npol,1)
	    call uvputvri(lOut,'pol',pol,1)
	  endif
	  if(dojpk.and.jyperk.gt.0)
     *	    call uvputvrr(lOut,'jyperk',jyperk,1)
c
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine getopt(dogel,doinst,tsyscal)
c
	implicit none
	logical dogel,doinst
	character tsyscal*(*)
c------------------------------------------------------------------------
	integer nout
	integer NOPTS
	parameter(NOPTS=2)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	integer NTYPES
	parameter(NTYPES=5)
	character types(NTYPES)*12
c
	data types/'interpolate ','any         ','none        ',
     *		   'constant    ','extrapolate '/
c
	data opts/'nogainel','noinst  '/
c
	call options('options',opts,present,NOPTS)
	dogel    = .not.present(1)
	doinst   = .not.present(2)
c
	call keymatch('tsyscal',NTYPES,types,1,tsyscal,nout)
	if(nout.eq.0)tsyscal=types(1)
c
	end
c************************************************************************
	subroutine aryfill(lIn,lOut,xyzu,nantu)
c
	implicit none
	integer lIn,lOut,nantu
	real xyzu(3*nantu)
c
c------------------------------------------------------------------------
	include 'maxdim.h'
c
c  For a disabled antenna, the antenna location is given as the centre of
c  the Earth. TOL is a parameter giving a lower limit on the distance
c  to the centre of the Earth, in nanosec.
c
	real TOL
	parameter(TOL=14000000)
	integer nantf,n,i,j
	character type*1
	double precision xyzf(3*MAXANT)
	real x
	logical update,docorr
c
	
        call uvprobvr(lIn,'antpos',type,nantf,update)
        if(type.ne.'d')call bug('f','Unrecognised antenna table')
        if(mod(nantf,3).ne.0)call bug('f','Invalid antenna table size')
        nantf = nantf/3
        if(nantf.gt.MAXANT)call bug('f','Too many antennas for me')
        call uvgetvrd(lIn,'antpos',xyzf,3*nantf)
c
	if(nantf.ne.nantu)
     *	  call bug('w','Inconsistent number of antennas')
	n = min(nantf,nantu)
c
c  Find a valid antenna. The centre of the 
c
	j = 0
	do i=n,1,-1
	  x = xyzf(i)**2 + xyzf(i+nantf)**2 + xyzf(i+2*nantf)**2
	  if(x.lt.TOL*TOL)j = i
	enddo
c
	if(j.eq.0)call bug('f','Could not find a reference antenna')
c
c  If an antenna location is a dud, add in the true location.
c
	docorr = .false.
	do i=1,n
	  x = xyzf(i)**2 + xyzf(i+nantf)**2 + xyzf(i+2*nantf)**2
	  if(x.gt.TOL*TOL)then
	    docorr = .true.
	    xyzf(i)         = xyzu(i)         - xyzu(j)         
     *					      + xyzf(j)
	    xyzf(i+nantf)   = xyzu(i+nantu)   - xyzu(j+nantu)
     *					      + xyzf(j+nantf)
	    xyzf(i+2*nantf) = xyzu(i+2*nantu) - xyzu(j+2*nantu)
     *					      + xyzf(j+2*nantf)
	  endif
	enddo
c
c  Write the fiddled table to the output.
c
	if(docorr)call uvputvrd(lOut,'antpos',xyzf,3*nantf)
	end
c************************************************************************
	subroutine arydec(aname,array,nant)
c
	implicit none
	integer nant
	character aname*(*),array(nant)*(*)
c
c  Given an array name, this gives the stations used for that
c  array.
c
c  Input:
c    aname  Array name.
c    nant   Number of antennas.  
c  Output:
c    array  Name of the stations used in that array.
c
c------------------------------------------------------------------------
	integer ATANT,NARRAYS
	parameter(ATANT=6,NARRAYS=33)
c
	integer i,j
	character anames(NARRAYS)*6,arrays(ATANT,NARRAYS)*4
c
	data anames(1),(arrays(i,1),i=1,ATANT)/
     *	'1.5A  ','W100','W110','W147','W168','W196','W392'/
	data anames(2),(arrays(i,2),i=1,ATANT)/
     *	'1.5B  ','W111','W113','W163','W182','W195','W392'/
	data anames(3),(arrays(i,3),i=1,ATANT)/
     *	'1.5C  ','W8  ','W128','W173','W190','W195','W392'/
	data anames(4),(arrays(i,4),i=1,ATANT)/
     *	'1.5D  ','W102','W109','W140','W182','W196','W392'/
	data anames(5),(arrays(i,5),i=1,ATANT)/
     *	'1.5G  ','W168','W111','W113','W148','W195','W392'/
	data anames(6),(arrays(i,6),i=1,ATANT)/
     *	'122A  ','W0  ','W2  ','W4  ','W6  ','W8  ','W392'/
	data anames(7),(arrays(i,7),i=1,ATANT)/
     *	'122B  ','W8  ','W10 ','W12 ','W14 ','W16 ','W392'/
	data anames(8),(arrays(i,8),i=1,ATANT)/
     *	'375   ','W2  ','W10 ','W14 ','W16 ','W32 ','W392'/
	data anames(9),(arrays(i,9),i=1,ATANT)/
     *	'6A    ','W4  ','W45 ','W102','W173','W195','W392'/
	data anames(10),(arrays(i,10),i=1,ATANT)/
     *	'6B    ','W2  ','W64 ','W147','W182','W196','W392'/
	data anames(11),(arrays(i,11),i=1,ATANT)/
     *	'6C    ','W0  ','W10 ','W113','W140','W182','W392'/
	data anames(12),(arrays(i,12),i=1,ATANT)/
     *	'6D    ','W8  ','W32 ','W84 ','W168','W173','W392'/
	data anames(13),(arrays(i,13),i=1,ATANT)/
     *	'6G    ','W0  ','W111','W113','W140','W182','W392'/
	data anames(14),(arrays(i,14),i=1,ATANT)/
     *	'6H    ','W4  ','W100','W102','W173','W195','W392'/
	data anames(15),(arrays(i,15),i=1,ATANT)/
     *	'750A  ','W147','W163','W172','W190','W195','W392'/
	data anames(16),(arrays(i,16),i=1,ATANT)/
     *	'750B  ','W98 ','W109','W113','W140','W148','W392'/
	data anames(17),(arrays(i,17),i=1,ATANT)/
     *	'750C  ','W64 ','W84 ','W100','W110','W113','W392'/
	data anames(18),(arrays(i,18),i=1,ATANT)/
     *	'750D  ','W100','W102','W128','W140','W147','W392'/
	data anames(19),(arrays(i,19),i=1,ATANT)/
     *	'750F  ','W140','W98 ','W109','W113','W148','W392'/
	data anames(20),(arrays(i,20),i=1,ATANT)/
     *	'EW214 ','W98 ','W102','W104','W109','W112','W392'/
	data anames(21),(arrays(i,21),i=1,ATANT)/
     *	'EW214B','W109','W98 ','W102','W104','W112','W392'/
	data anames(22),(arrays(i,22),i=1,ATANT)/
     *	'EW352 ','W102','W104','W109','W112','W125','W392'/
	data anames(23),(arrays(i,23),i=1,ATANT)/
     *	'EW367 ','W104','W110','W113','W124','W128','W392'/
	data anames(24),(arrays(i,24),i=1,ATANT)/
     *	'EW367B','W124','W104','W110','W113','W128','W392'/
	data anames(25),(arrays(i,25),i=1,ATANT)/
     *	'H168  ','W100','W104','W111','N7  ','N11 ','W392'/
	data anames(26),(arrays(i,26),i=1,ATANT)/
     *	'H168B ','W100','W104','N11 ','N7  ','W111','W392'/
	data anames(27),(arrays(i,27),i=1,ATANT)/
     *	'H214  ','W98 ','W104','W113','N5  ','N14 ','W392'/
	data anames(28),(arrays(i,28),i=1,ATANT)/
     *	'H75   ','W104','W106','W109','N2  ','N5  ','W392'/
	data anames(29),(arrays(i,29),i=1,ATANT)/
     *	'H75B  ','W104','N5  ','N2  ','W106','W109','W392'/
	data anames(30),(arrays(i,30),i=1,ATANT)/
     *	'NS214 ','W106','N2  ','N7  ','N11 ','N14 ','W392'/
	data anames(31),(arrays(i,31),i=1,ATANT)/
     *	'H214B ','W98 ','W104','N14 ','N5  ','W113','W392'/
	data anames(32),(arrays(i,32),i=1,ATANT)/
     *	'WBCOR ','W112','W102','W104','W106','W125','W392'/
	data anames(33),(arrays(i,33),i=1,ATANT)/
     *	'EW352B','W112','W102','W104','W109','W125','W392'/
c
	if(nant.ne.ATANT)call bug('f','Incompatible number of antennas')
c
	j = 0
	do i=1,NARRAYS
	  if(aname.eq.anames(i))j=i
	enddo
	if(j.eq.0)call bug('f','Unrecognised array name')
	do i=1,ATANT
	  array(i) = arrays(i,j)
	enddo
c
	end
c************************************************************************
	subroutine aryxyz(array,xyz,nant)
c
	implicit none
	integer nant
	character array(nant)*(*)
	real xyz(3*nant)
c
c  Given the ATCA station names, this creates a Miriad-style antenna
c  table, giving xyz locations in nanosec.
c
c  Input:
c    array  Station names
c    nant   Number of antennas
c  Output:
c    xyz    Antenna locations.
c------------------------------------------------------------------------
c
c  FAC is the basic 15.3 m increment of the ATCA expressed in nanosec.
c
	real FAC
	parameter(FAC=51.0204)
	double precision lat
	integer i,j
	character line*80
	logical ok
	real x,y
c
	call obspar('ATCA','latitude',lat,ok)
	if(.not.ok)call bug('f','Failed to find Narrabris latitude')
c
	do i=1,nant
	  line = 'Unrecognised station name: '//array(i)
	  call atoif(array(i)(2:),j,ok)
	  if(.not.ok)call bug('f',line)	  
	  if(array(i)(1:1).eq.'N')then
	    x = j
	    y = -106
	  else if(array(i)(1:1).eq.'W')then
	    x = 0
	    y = -j
	  else
	    call bug('f',line)
	  endif
	  xyz(i)        = FAC*x*sin(-lat)
	  xyz(i+nant)   = FAC*y
	  xyz(i+2*nant) = FAC*x*cos(-lat)
	enddo
c
	end
c************************************************************************
      subroutine getlst (lin, lst)
c
      implicit none
      integer lin
      double precision lst
c
c  Get lst of the current data point.
c
c  Input:
c    lin         Handle of file
c  Output:
c    lst         LAST in radians
c-----------------------------------------------------------------------
      double precision time,ra,long,dtemp
      character type*1
      integer length
      logical ok
c
c  Externals.
c
      double precision eqeq
c
      lst = 0.0d0
      call uvprobvr (lin, 'lst', type, length, ok)
      if (type(1:1).eq.' ') then
	call uvrdvrd (lin, 'ra', dtemp, 0.d0)
	call uvrdvrd (lin, 'obsra', ra, dtemp)
	call getlong(lin,long)
	call uvrdvrd (lin, 'time', time, 0.d0)
        call jullst (time, long, lst)
	lst = lst + eqeq(time)
      else
         call uvrdvrd (lin, 'lst', lst, 0.0d0)
      end if
c
      end
c************************************************************************
      subroutine getlong (lin, long)
c
c     Get longitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    longitude   Longitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision long
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      long = 0.0d0
      call uvprobvr (lin, 'longitu', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No longitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out longitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'longitude', long, ok)
            if (.not.ok) call bug('f', 
     +          'No valid longitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'longitu', long, 0.0d0)
      end if
c
      end
c************************************************************************
      subroutine getlat (lin, lat)
c
c     Get latitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    lat        Latitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision lat
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      lat = 0.0d0
      call uvprobvr (lin, 'latitud', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No latitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out latitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'latitude', lat, ok)
            if (.not.ok) call bug('f', 
     +          'No valid latitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'latitud', lat, 0.0d0)
      end if
c
      end
c************************************************************************
	subroutine felget(lVis,vmet,nif,nschan,freq0,freq,nchan,
     *							ra,dec,lat)
c
	implicit none
	integer vmet,nif,nschan(nif),lVis,nchan
	real freq0(nif)
	double precision ra,dec,lat,freq(nchan)
c
c  Input:
c    vmet
c    nif
c    nschan
c  Input/Output:
c    freq0
c    ra,dec
c    lat
c    freq
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k
	double precision dtemp,sfreq(MAXWIN),sdf(MAXWIN)
c
	logical uvvarUpd
c
	if(uvvarUpd(vmet))then
	  call uvgetvrd(lVis,'sfreq',sfreq,nif)
	  call uvgetvrd(lVis,'sdf',sdf,nif)
	  k = 0
	  do i=1,nif
	    freq0(i) = sfreq(i) + 0.5*(nschan(i)-1)*sdf(i)
	    freq0(i) = freq0(i) * 1e9
	    do j=1,nschan(i)
	      k = k + 1
	      freq(k) = sfreq(i) + sdf(i)*(j-1)
	    enddo
	  enddo
	  if(k.ne.nchan)call bug('f','Inconsistent number of channels')
	  call uvrdvrd(lVis,'ra',dtemp,0.d0)
	  call uvrdvrd(lVis,'obsra',ra,dtemp)
	  call uvrdvrd(lVis,'dec',dtemp,0.d0)
	  call uvrdvrd(lVis,'obsdec',dec,dtemp)
	  call getlat(lVis,lat)
	endif
c
	end
c************************************************************************
	real function EleScale(el,freq0,baseline)
c
	implicit none
	double precision el
	real freq0
	double precision baseline
c
c  Input:
c     el        elevation   (rad)
c     freq0     freq of obs (Hz)
c     baseline  baseline number
c  Output:
c     EleScale  factor to multiply vis by
c-----------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer ATANT
	parameter(ATANT=6)
c
	integer ant1, ant2
	real elev,za
	real g1,g2
c
	integer j
c
c  3cm gain/elevation
c
	integer XPARAMS
	parameter(XPARAMS=3)
	real    xpc(ATANT,XPARAMS)
c
c  3mm gain/elevation
c
	integer WPARAMS
	parameter(WPARAMS=3)
	real wpc(ATANT,WPARAMS)
c
        data (xpc(1,j),j=1,3)/1.00000,  -2.81563e-04,   6.41390e-06/
	data (xpc(2,j),j=1,3)/1.00000,  -2.89640e-04,   6.36535e-06/
	data (xpc(3,j),j=1,3)/1.00000,  -5.85414e-04,   1.09251e-05/
	data (xpc(4,j),j=1,3)/1.00000,  -1.61529e-04,   7.44309e-06/
	data (xpc(5,j),j=1,3)/1.00000,  -1.81528e-05,   5.27955e-06/
	data (xpc(6,j),j=1,3)/1.00000,  -1.02589e-03,   8.71686e-06/
c
c  The original gain/elevation curve, derived by a G/T vs elevation curve
c  measured by Ravi and a Tsys curve measured by rjs on 03-jun-02.
c
c	data (wpc(1,j),j=1,3)/1.0000, 0.0000,     0.0000/
c	data (wpc(2,j),j=1,3)/0.7441, 0.9221e-2, -0.8306e-4/
c	data (wpc(3,j),j=1,3)/0.4043, 1.6802e-2, -1.1847e-4/
c	data (wpc(4,j),j=1,3)/0.8405, 0.8094e-2, -1.0268e-4/
c	data (wpc(5,j),j=1,3)/1.0000, 0.0000,     0.0000/
c	data (wpc(6,j),j=1,3)/1.0000, 0.0000,     0.0000/
c
c  Gain curve deduced by Tony Wong on 04-Jun-02.
c
c	data (wpc(1,j),j=1,3)/1.0000, 0.0000,     0.0000/
c	data (wpc(2,j),j=1,3)/0.4927, 1.6729e-2, -1.3791e-4/
c	data (wpc(3,j),j=1,3)/0.2367, 2.0487e-2, -1.3748e-4/
c	data (wpc(4,j),j=1,3)/0.5405, 1.7040e-2, -1.5798e-4/
c	data (wpc(5,j),j=1,3)/1.0000, 0.0000,     0.0000/
c	data (wpc(6,j),j=1,3)/1.0000, 0.0000,     0.0000/
c
c  Gain curve deduced from Ravi's data of 04-Sep-03.
c
c	data (wpc(2,j),j=1,3)/0.4877, 1.7936e-2, -1.5699e-4/
c	data (wpc(3,j),j=1,3)/0.7881, 0.8458e-2, -0.8442e-4/
c	data (wpc(4,j),j=1,3)/0.7549, 1.2585e-2, -1.6153e-4/
c
c  Data from Bob on 21-May-2004
c
	data (wpc(1,j),j=1,3)/0.4917, 1.7709e-2, -1.5423e-4/
	data (wpc(2,j),j=1,3)/0.2262, 2.3075e-2, -1.7204e-4/
	data (wpc(3,j),j=1,3)/0.3060, 2.1610e-2, -1.6821e-4/
	data (wpc(4,j),j=1,3)/0.7939, 0.8148e-2, -0.8054e-4/
	data (wpc(5,j),j=1,3)/0.4487, 1.7665e-2, -1.4149e-4/
	data (wpc(6,j),j=1,3)/1.0000, 0.0000,     0.0000/
c
	call basant(baseline, ant1, ant2)
	if(max(ant1,ant2).gt.ATANT.or.min(ant1,ant2).lt.1)
     *	  call bug('f','Bad antenna numbers')
c
	elev = real(el*180.0/PI)
	za=90.0 - elev
c
c  3cm correction.
c
	if ((8.0e+9 .le. freq0) .and. (freq0 .le. 9.2e+9)) then
	  g1=1/(xpc(ant1,1) + za*(xpc(ant1,2) + za*xpc(ant1,3)))
	  g2=1/(xpc(ant2,1) + za*(xpc(ant2,2) + za*xpc(ant2,3)))
c
c  3mm correction.
c
	else if(70e9.le.freq0 .and. freq0.le.120e9)then
	  elev = max(elev,40.0)
	  g1=wpc(ant1,1) + elev*(wpc(ant1,2)+elev*wpc(ant1,3))
	  g2=wpc(ant2,1) + elev*(wpc(ant2,2)+elev*wpc(ant2,3))
	else
	  g1=1.
	  g2=1.
	endif
c
	EleScale=g1*g2
c
	end
c************************************************************************
	subroutine blcorr(data,freq,nchan,ra,dec,lst,el,bl,xyz,nant,
     *								doinst)
c
	implicit none
	integer nchan,nant
	complex data(nchan)
	double precision ra,dec,lst,el,bl,freq(nchan)
	real xyz(3,nant)
	logical doinst
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer ant1,ant2,i
	real delX,delY,delZ,theta,antphz,cosha,sinha,cosdec,sindec
	real offset
	complex w
	double precision HA
c
	real factor(6)
	data factor/1.0,0.0,0.0,0.0,0.25,0.0/
c
	call basant(bl,ant1,ant2)
	if(min(ant1,ant2).lt.1.or.max(ant1,ant2).gt.nant)
     *	  call bug('f','Invalid antenna number')
c
	delX = xyz(1,ant2) - xyz(1,ant1)
	delY = xyz(2,ant2) - xyz(2,ant1)
	delZ = xyz(3,ant2) - xyz(3,ant1)
c
c  The following is the model of the elevation dependence
c  of the phase of CA01.
c
	if(doinst)then
	  offset = 7.4563 - (15.2243 - 7.2267*el)*el
	  offset = offset * 1e-3 / CMKS * 1e9 * 
     *		  (factor(ant1)-factor(ant2))
	else
	  offset = 0
	endif
c
	if(abs(delX)+abs(delY)+abs(delZ).ne.0)then
          HA = lst - ra
          cosHA = cos(HA)
          sinHA = sin(HA)
          cosdec = cos(dec)
          sindec = sin(dec)
          antphz = 2*DPI*((delX*cosHA - delY*sinHA)*cosdec +
     *			   delZ*sindec + offset)
c
	  do i=1,nchan
	    theta = -antphz*freq(i)
	    w = cmplx(cos(theta),sin(theta))
	    data(i) = w*data(i)
	  enddo
	endif
c
	end
c************************************************************************
	real function getjpk(freq)
c
	implicit none
	real freq
c------------------------------------------------------------------------
	if(freq.lt.15)then
	  getjpk = 13
	else if(freq.lt.30)then
	  getjpk = 13
	else
	  getjpk = 25
	endif
c
	end
c************************************************************************
c************************************************************************
	subroutine tcalIni(lVis,tsyscal)
c
	implicit none
	integer lVis
	character tsyscal*(*)
c------------------------------------------------------------------------
	include 'atfix.h'
c
c  Set up a variable handle to track changes in the correlator/freq setup.
c
	call uvvarIni(lVis,vtcal1)
	call uvvarSet(vtcal1,'xtsys')
	call uvvarSet(vtcal1,'ytsys')
c
	call uvvarIni(lVis,vtcal2)
	call uvvarSet(vtcal2,'airtemp')
	call uvvarSet(vtcal2,'pressmb')
	call uvvarSet(vtcal2,'relhumid')
	call uvvarSet(vtcal2,'antel')
c
c  Initialise the counters.
c
	t1 = 1
	t2 = 0
	ntcal = 0
	if(tsyscal.eq.'none')then
	  tmode = TMNONE
	else if(tsyscal.eq.'constant')then
	  tmode = TMCONST
	else if(tsyscal.eq.'extrapolate')then
	  tmode = TMEXTRAP
	else if(tsyscal.eq.'interpolate')then
	  tmode = TMINTERP
	else
	  call bug('f','Invalid tsyscal mode')
	endif
c
	end
c************************************************************************
	subroutine tcalUpd(lVis,ifreq1)
c
	implicit none
	integer lVis,ifreq1
c------------------------------------------------------------------------
	include 'atfix.h'
c
	logical neednew,needupd
	character type*1
	logical updated,doatm
	integer length
c
c  Externals.
c
        logical uvvarUpd
C
        if(ntcal.eq.0)then
          doatm = tmode.eq.TMEXTRAP.or.tmode.eq.TMINTERP
          call uvrdvri(lVis,'nants',nants,0)
          if(nants.lt.2.or.nants.gt.MAXANT)call bug('f',
     *                          'Invalid number of antennas')
          neednew = .true.    
c
c  Check the right variables are present.
c
	  if(doatm)then
	    call uvprobvr(lVis,'airtemp',type,length,updated)
	    if(type.ne.'r'.or.length.ne.1)call bug('f',
     *          'Met data missing from input')
 	    call uvprobvr(lVis,'antel',type,length,updated)
	    if(type.ne.'d'.or.length.ne.1)call bug('f',
     *          'Antenna elevation information missing')
	  endif
	  call uvprobvr(lVis,'xtsys',type,length,updated)
	  if(type.ne.'r'.or.length.lt.6)call bug('f',
     *          'Raw Tsys measurements seem to be missing')
	else
	  neednew = .false.
	endif
	if(.not.neednew)neednew = tfreq(ntcal).ne.ifreq1
c
	needupd = uvvarUpd(vtcal1)
	if(neednew.or.needupd)then
	  ntcal = ntcal + 1
	  if(ntcal.gt.MAXTCAL)call bug('f','Too many Tsys scans')
	  call uvrdvrd(lVis,'time',ttime(ntcal),0.d0)
	  tfreq(ntcal) = ifreq1
	endif
	if(neednew.and..not.needupd)then
	  call bug('w',
     *	  'New frequency without new system temperature measurement')
	  needupd = .true.
	endif

	if(needupd)then
	  call tcalLoad(lVis,doatm,ifreq1,nants,MAXANT,MAXWIN,
     *	    xtsys(1,1,ntcal),ytsys(1,1,ntcal),
     *	    xtrec(1,1,ntcal),ytrec(1,1,ntcal))
	  tvalid(ntcal) = .true.
c	else if(neednew)then
c	  call bug('w',
c     *	  'New frequency without new system temperature measurement')
c	  tvalid(ntcal) = .false.
	endif
c
	end
c************************************************************************
	subroutine tcalLoad(lVis,doatm,ifreq,nants,mants,mwin,
     *				xtsys,ytsys,xtrec,ytrec)
c
	implicit none
	integer lVis,nants,mants,mwin,ifreq
	logical doatm
	real xtrec(mants,mwin),ytrec(mants,mwin)
	real xtsys(mants,mwin),ytsys(mants,mwin)
c
c  Load up the system temperature measurement information.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k,nwin
	real fac(MAXWIN),Tb(MAXWIN),freq(MAXWIN)
	real buffx(MAXANT*MAXWIN),buffy(MAXANT*MAXWIN)
c
	if(doatm)then
	  call tcalFTB(lVis,ifreq,fac,Tb,MAXWIN,nwin)
	else
	  call freqMFrq(ifreq,MAXWIN,nwin,freq)
	  do j=1,nwin
	    fac(j) = 1
	    Tb(j) = 0
	  enddo
	endif
c
	if(nants*nwin.gt.MAXANT*MAXWIN)
     *	  call bug('f','Buffers to small')
c
	call uvgetvrr(lVis,'xtsys',buffx,nants*nwin)
	call uvgetvrr(lVis,'ytsys',buffy,nants*nwin)
c
	k = 1
	do j=1,nwin
	  do i=1,nants
	    xtsys(i,j) = buffx(k)
	    ytsys(i,j) = buffy(k)
	    xtrec(i,j) = fac(j)*buffx(k) - Tb(j)
	    ytrec(i,j) = fac(j)*buffy(k) - Tb(j)
	    k = k + 1
	  enddo
	enddo
c
	end
c**************************************************************
	subroutine tcalFTB(lVis,ifreq,fac,Tb,mwin,nwin)
c
	implicit none
	integer lVis,mwin,nwin,ifreq
	real fac(mwin),Tb(mwin)
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	real t0,h0,p0,elev,freq(MAXWIN)
	integer i
c
	call freqMFrq(ifreq,MAXWIN,nwin,freq)
	do i=1,nwin
	  freq(i) = 1e9*freq(i)
	enddo
	if(nwin.gt.mwin)call bug('f','Buffer overflow1')
        call uvgetvrr(lVis,'airtemp',t0,1)
        call uvgetvrr(lVis,'pressmb',p0,1)
        call uvgetvrr(lVis,'relhumid',h0,1)
	call uvrdvrr(lVis,'antel',elev,0.0)
c
        t0 = t0 + 273.15
        h0 = 0.01*h0
        p0 = 100*p0
	elev = PI/180.0*elev
	call opacget(nwin,freq,elev,t0,p0,h0,fac,Tb)
	end
c************************************************************************
	subroutine tcalApp(lVis,lOut,tbp,data,nread)
c
	implicit none
	integer lVis,lOut,nread
	double precision tbp(3)
	complex data(nread)
c------------------------------------------------------------------------
        integer XX,YY,XY,YX
        parameter(XX=-5,YY=-6,XY=-7,YX=-8)
c
	include 'atfix.h'
	integer i,j,k,i1,i2,pol,nwin,nsc(MAXWIN),tcorr
	real fac,num,denom
	logical notsys
c
c  External.
c
	logical uvvarUpd
c
c
	if(uvvarUpd(vtcal2))call tcalNew(lVis,lOut,tbp(1))
c
	pol = nint(tbp(3))
	call basant(tbp(2),i1,i2)
c
c  Check for the do nothing option.
c
	call uvrdvri(lVis,'tcorr',tcorr,1)
	notsys = tcorr.eq.0	
	if(.not.tvalid(t1).or.
     *	   (notsys.and.tmode.eq.TMNONE).or.
     *	   (.not.notsys.and.tmode.eq.TMCONST))then
	  continue
c
c  Otherwise work out what to apply and apply it.
c
	else
	  call freqDesc(ifreq,MAXWIN,nwin,nsc)
	  k = 0
	  do j=1,nwin
	    if(pol.eq.XX)then
              num   = xtcur(i1,j)*xtcur(i2,j)
	      denom = xtsys(i1,j,t1)*xtsys(i2,j,t1)
            else if(pol.eq.YY)then
              num   = ytcur(i1,j)*ytcur(i2,j)
	      denom = ytsys(i1,j,t1)*ytsys(i2,j,t1)
            else if(pol.eq.XY)then
              num   = xtcur(i1,j)*ytcur(i2,j)
	      denom = xtsys(i1,j,t1)*ytsys(i2,j,t1)
            else if(pol.eq.YX)then
	      num   = ytcur(i1,j)*xtcur(i2,j)
	      denom = ytsys(i1,j,t1)*xtsys(i2,j,t1)
            else
	      call bug('f','Invalid polarisation code')
	    endif
c
	    if(notsys)then
	      fac = sqrt(num)/50
	    else if(tmode.eq.TMNONE)then
	      fac = 50/sqrt(denom)
	    else
	      fac = sqrt(num/denom)
	    endif
c
	if(i1.eq.1.and.i2.eq.1)write(*,*)fac
	    do i=1,nsc(j)
	      k = k + 1
	      data(k) = fac * data(k)
	    enddo
	  enddo
	endif
c
	if(k.ne.nread)call bug('f','Channel number inconsistency')
c
	end
c************************************************************************
	subroutine tcalNew(lVis,lOut,time)
c
	implicit none
	integer lVis,lOut
	double precision time
c
c------------------------------------------------------------------------
	include 'atfix.h'
	integer i,j
	real fac(MAXWIN),Tb(MAXWIN),freq(MAXWIN),a1,a2,t
	integer nwin
	logical more
c
	more = .true.
	dowhile(more)
	  if(t1.eq.ntcal)then
	    more = .false.
	  elseif(time.lt.ttime(t1+1))then
	    more = .false.
	  else
	    t1 = t1 + 1
	  endif
	enddo
c
	t2 = t1
	more = tmode.eq.TMINTERP
	dowhile(more)
	  if(t2.eq.ntcal)then
	    more = .false.
	  else
	    t2 = t2 + 1
	    more = tfreq(t1).ne.tfreq(t2)
	  endif
	enddo
	if(.not.tvalid(t2))t2 = t1
c
	if(tvalid(t1))then
	  if(tmode.eq.TMNONE.or.tmode.eq.TMCONST)then
	    call freqMFrq(ifreq,MAXWIN,nwin,freq)
	    do j=1,nwin
	      do i=1,nants
		xtcur(i,j) = xtsys(i,j,t1)
		ytcur(i,j) = ytsys(i,j,t1)
	      enddo
	    enddo
	  else
	    call tcalFTB(lVis,tfreq(t1),fac,Tb,MAXWIN,nwin)
	    if(t1.ne.t2)then
	      a1 = (ttime(t2)-time)/(ttime(t2)-ttime(t1))
	      a2 = (time-ttime(t1))/(ttime(t2)-ttime(t1))
	      do j=1,nwin
	        do i=1,nants
	          t = a1*xtrec(i,j,t1) + a2*xtrec(i,j,t2)
	          xtcur(i,j) = (t+Tb(j))/fac(j)
	          t = a1*ytrec(i,j,t1) + a2*ytrec(i,j,t2)
	          ytcur(i,j) = (t+Tb(j))/fac(j)
	        enddo
	      enddo
	    else
	      do j=1,nwin
	        do i=1,nants
	          xtcur(i,j) = (xtrec(i,j,t1)+Tb(j))/fac(j)
	          ytcur(i,j) = (ytrec(i,j,t1)+Tb(j))/fac(j)
	        enddo
	      enddo
	    endif
	  endif
c
	  call tcalPut(lOut,nants,MAXANT,nwin,xtcur,ytcur)
	endif
c
	end
c************************************************************************
	subroutine tcalPut(lOut,nants,mants,nwin,xtcur,ytcur)
c
	implicit none
	integer lOut,nants,mants,nwin
	real xtcur(mants,nwin),ytcur(mants,nwin)
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k
	real buffx(MAXANT*MAXWIN),buffy(MAXANT*MAXWIN)
	real buffz(MAXANT*MAXWIN)
c
	if(nants*nwin.gt.MAXANT*MAXWIN)call bug('f','Buffer overflow2')
	k=1
	do j=1,nwin
	  do i=1,nants
	    buffx(k) = xtcur(i,j)
	    buffy(k) = ytcur(i,j)
	    buffz(k) = sqrt(abs(xtcur(i,j)*ytcur(i,j)))
	    k = k + 1
	  enddo
	enddo
c
	
	call uvputvrr(lOut,'xtsys',buffx,nants*nwin)
	call uvputvrr(lOut,'ytsys',buffy,nants*nwin)
	call uvputvrr(lOut,'systemp',buffz,nants*nwin)
c
	end
c************************************************************************
c************************************************************************
	subroutine freqini(lVis)
c
	implicit none
	integer lVis
c
c------------------------------------------------------------------------
	include 'atfix.h'
	call uvvarIni(lVis,vfreq)
	call uvvarSet(vfreq,'nchan')
	call uvvarSet(vfreq,'nspect')
	call uvvarSet(vfreq,'nschan')
	call uvvarSet(vfreq,'sfreq')
	call uvvarSet(vfreq,'sdf')
	ifreq = 0
	nfreq = 0
	end
c************************************************************************
	subroutine freqGet(lVis,ifreq1)
c
	implicit none
	integer lVis,ifreq1
c------------------------------------------------------------------------
	include 'atfix.h'
	logical uvvarUpd
	if(uvvarupd(vfreq))  call freqGet1(lVis,ifreq,nfreq,
     *		nchan,nspect,nschan,sfreqs,MAXFREQ,MAXWIN)
	ifreq1 = ifreq
	end
c************************************************************************
	subroutine freqGet1(lVis,ifreq,nfreq,
     *		nchan,nspect,nschan,sfreqs,
     *		MAXFREQ,MAXWIN)
c
	implicit none
	integer MAXFREQ,MAXWIN
	integer lVis,ifreq,nfreq,nchan(MAXFREQ),nspect(MAXFREQ)
	integer nschan(MAXWIN,MAXFREQ)
	double precision sfreqs(MAXWIN,3,MAXFREQ)
c
c  Keep track of frequency/correlator setups. Determine whether we
c  have a new correlator or freq setup.
c
c  The frequency setup arrays, sfreqs (spectral
c  frequencies respectively) contain three values,
c	freqs(?,1,?) is the start frequency in the first record.
c	freqs(?,2,?) is the bandwidth or channel increment.
c       freqs(?,3,?) is the start frequency in the last record.
c  The start frequency in the first and last record can differ, owing to
c  slow changes in the frequency caused by Doppler tracking and the like.
c------------------------------------------------------------------------
	integer itmp,i
	logical more,newfreq
c
c  Externals.
c
	logical freqEq
c
	itmp = nfreq + 1
	if(itmp.gt.MAXFREQ)call bug('f','Frequency table overflow')
c
c  Load the current freq/correlator description.
c
	call uvrdvri(lVis,'nchan',nchan(itmp),0)
	call uvrdvri(lVis,'nspect',nspect(itmp),0)
	if(nspect(itmp).gt.MAXWIN)call bug('f','Too many windows')
	if(nchan(itmp).gt.0)then
	  call uvgetvrd(lVis,'sfreq', sfreqs(1,1,itmp),nspect(itmp))
	  call uvgetvrd(lVis,'sdf',   sfreqs(1,2,itmp),nspect(itmp))
	  call uvgetvri(lVis,'nschan',nschan(1,itmp),nspect(itmp))
	endif
c
c  Is it a new frequency/correlator setup?
c
	if(nfreq.eq.0)then
	  newfreq = .true.
	else
	  newfreq = .not.FreqEq(ifreq,itmp,nchan,nspect,nschan,sfreqs,
     *			MAXWIN,MAXFREQ)
	endif
c
c  Process a new frequency.
c
	if(newfreq)then
	  ifreq = 1
	  more = .true.
	  dowhile(ifreq.le.nfreq.and.more)
	    more = .not.FreqEq(ifreq,itmp,nchan,nspect,nschan,sfreqs,
     *			MAXWIN,MAXFREQ)
	    if(more)ifreq = ifreq + 1
	  enddo
c
c  If its a totally new frequency, complete the description of it.
c  Most of the description is already in the right place.
c
	  if(more)nfreq = nfreq + 1
	endif
c
c  Update the start frequency column of the last record.
c
	do i=1,nspect(ifreq)
	  sfreqs(i,3,ifreq) = sfreqs(i,1,itmp)
	enddo
c
	end
c************************************************************************
	logical function freqEq(i1,i2,nchan,nspect,nschan,sfreqs,
     *		MAXWIN,MAXFREQ)
c
	implicit none
	integer MAXWIN,MAXFREQ
	integer i1,i2,nchan(MAXFREQ),nspect(MAXFREQ)
	integer nschan(MAXWIN,MAXFREQ)
	double precision sfreqs(MAXWIN,3,MAXFREQ)
c
c  Determine whether two correlator/frequency setups are the same.
c  For them to be the same, nchan, nspec, nschan have to
c  match exactly. wwidth and sdf has to match to 1%, and sfreq have to 
c  match to within half a channel.
c
c------------------------------------------------------------------------
	integer i
	real w
c
	FreqEq = .false.
	if(nchan(i1).ne.nchan(i2).or.
     *	   nspect(i1).ne.nspect(i2))return
c
	do i=1,nspect(i1)
	  if(nschan(i,i1).ne.nschan(i,i2))return
	  w = abs(sfreqs(i,2,i1))
	  if(abs(sfreqs(i,2,i1)-sfreqs(i,2,i2)).gt.0.01*w)return
	  if(abs(sfreqs(i,3,i1)-sfreqs(i,1,i2)).gt.0.5*w)return
	enddo
c
	FreqEq = .true.
	end
c************************************************************************
	subroutine freqDesc(ifreq1,mwin,nwin,nsc)
c
	implicit none
	integer ifreq1,mwin,nwin,nsc(mwin)
c------------------------------------------------------------------------
	integer i
	include 'atfix.h'
c
	if(ifreq1.lt.1.or.ifreq1.gt.nfreq)
     *	  call bug('f','Invalid frequency index')
	if(nspect(ifreq1).gt.mwin)call bug('f','Buffer overflow3')
	nwin = nspect(ifreq)
	do i=1,nwin
	  nsc(i) = nschan(i,ifreq1)
	enddo
c
	end
c************************************************************************
	subroutine freqMFrq(ifreq1,mwin,nwin,freq)
c
	implicit none
	integer ifreq1,mwin,nwin
	real freq(mwin)
c
c  Input:
c    ifreq1
c    mwin
c  Output:
c    nwin
c    freq
c------------------------------------------------------------------------
	integer i
	include 'atfix.h'
c
	if(ifreq1.lt.1.or.ifreq1.gt.nfreq)
     *	  call bug('f','Invalid frequency index')
	if(nspect(ifreq1).gt.mwin)call bug('f','Buffer overflow4')
	nwin = nspect(ifreq1)
c
	do i=1,nwin
	  freq(i) = 0.5*(sfreqs(i,2,ifreq1)*(nschan(i,ifreq1)-1) +
     *			 sfreqs(i,1,ifreq1) + sfreqs(i,3,ifreq1) )
	enddo
c
	end
