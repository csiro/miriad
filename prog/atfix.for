c************************************************************************
	program atfix
	implicit none
c
c= atfix - Apply various miscellaneous corrections to ATCA visibility data
c& rjs
c: uv analysis
c+
c	ATFIX performs various miscellaneous corrections appropriate for
c	ATCA data. These corrections are typically more important for
c	data produced by the 12- and 3-mm systems. These systems still contain
c	imperfections that need to be fixed off-line. Steps that can be
c	performed are
c	  Correct for atmospheric opacity
c	  Apply gain/elevation effect
c	  Apply system temperature measurements
c	  Correct for incorrect baseline length
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
c@ array
c	One of the flaws in the current ATCA datafiles is that antenna locations
c	are not recorded for antennas that are off-line (and hence not producing
c	data). While this might not seem a serious flaw, the off-line antennas
c	can still cause shadowing. This will be an issue when using the
c	3-antenna system in compact arrays.
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
c@ dantpos
c	For poorly understood reasons, the effective locations of the antennas
c	appear to be a function of frequency. Currently the on-line system uses
c	the antenna locations derived from centimetre wavelength observations.
c	If millimetre wavelength solutions for the baseline lengths are available,
c	you will want to correct your data to account for these.
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
c	If a data file is present, you can instruct atfix to read this
c	directly using the indirect parameter input. For example, to read
c	parameters appropriate for a hypothetical array configuration
c	starting on 16 October 2002, use
c	    dantpos=@$MIRCAT/dantpos.021016
c
c@ options
c	Extra processing options. Several options can be given,
c	separated by commas. Minimum match is supported. Possible values
c	are:
c	  opcorr    Apply a correction to account for the opacity of the
c	            atmosphere. This option should generally be used
c	            at 12mm only. For observations taken before October 2003,
c	            you will need to set the ``mdata'' parameter (see below)
c	            when using this option.
c	  tsys      Ensure the system temperature correction is applied to
c	            the data. It is not uncommon when observing at 3mm to
c	            not apply the system temperature correction to the data
c	            on-line.
c	  gainel    This applies a instrumental gain/elevation correction
c	            to the data. Currently the gains of the antennas are
c	            a function of elevation.
c@ mdata
c	To apply an opacity correction, this task needs to estimate the
c	atmospheric opacity based on meteorological conditions. Prior to
c	October 2003, meteorological data were not saved in the datasets, and
c	so the meterological data needs to be provided separately.
c
c	This input parameter gives a data file containing the meteorological
c	conditions at the observatory. It is only required if the dataset
c	does not already contain this information. 
c
c	Given a model of the atmosphere, and knowing
c	the elevation of the observation, a model correction for the atmosphere
c	can be deduced and applied. Note that this is just that - a model
c	correction - which will have limitations.
c
c	NOTE: This correction should NOT be used with 3-mm data when the 3mm
c	system temperature corrections are used. The 3-mm system temperatures
c	are, by their nature, so-called "above atmosphere" system temperature
c	value, which corrects for atmospheric opacity.
c
c	This parameter can be used with the 12-mm system, where the system
c	temperatures are those measured at the ground.
c
c	For more help on getting the meteorological data appropriate to your
c	observation, see the help on "weatherdata".
c--
c  History:
c    04may03 rjs  Original version.
c    15may03 rjs  Better jyperk.
c    14aug03 rjs  Fix bug causing seg violation.
c    17sep03 rjs  New gain/elevation curve at 3mm.
c    19oct03 rjs  Update array tables.
c    14nov03 rjs  Fix bug in getjpk
c    06dec03 rjs  Fish out met parameters from the dataset directly.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	integer MAXSELS,ATANT
	parameter(version='AtFix: version 1.0 06-Dec-03')
	parameter(MAXSELS=256,ATANT=6)
c
	real sels(MAXSELS),xyz(3*MAXANT)
	character array(MAXANT)*8,aname*8,mdata*80
	integer lVis,lOut,vtsys,vant,vgmet,vnif,vmdata
	integer pol,npol,i1,i2,nant,i,j,k
	logical updated,dogel,dotsys,domet,newel,dobl,doopcorr
	character vis*64,out*64,type*1
	integer nschan(MAXWIN),nif,nchan,nants,length,tcorr,na
	real xtsys(MAXANT*MAXWIN),ytsys(MAXANT*MAXWIN)
	real gel
	logical dojpk
c
	real delta(3,MAXANT)
	real freq0(MAXWIN),scale,fac(MAXWIN),Tb(MAXWIN),t0,p0,h0,jyperk
	double precision ra,dec,lat,lst,az,el
c
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	double precision preamble(5),ptime,freq(MAXCHAN)
c
c  Externals.
c
	logical uvvarUpd,selProbe,hdPrsnt,keyprsnt
	real elescale,getjpk
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call selInput('select',sels,MAXSELS)
	call keya('out',out,' ')
	call keya('mdata',mdata,' ')
	call mkeya('array',array,MAXANT,nant)
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
	call GetOpt(dogel,dotsys,doopcorr)
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
	domet = mdata.ne.' '
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
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
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
c  Get ready to handle atmospheric opacity correction and gain/elevation
c  correction.
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
	newel = .true.
	if(doopcorr)then
	  if(domet)then
	    call metInit(mdata)
	  else
	    call uvvarIni(lVis,vmdata)
	    call uvvarSet(vmdata,'airtemp')
	    call uvvarSet(vmdata,'pressmb')
	    call uvvarSet(vmdata,'relhumid')
	  endif
	endif
c
c  Get ready to handle Tsys correction.
c
	if(dotsys)then
	  call uvvarIni(lVis,vtsys)
	  call uvvarSet(vtsys,'nschan')
	  call uvvarSet(vtsys,'xtsys')
	  call uvvarSet(vtsys,'ytsys')
	endif
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
c  Get first record.
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	if(nchan.eq.0)call bug('f','No data found')
c
c  If dotsys mode has been requested, check that the "tcorr" variable
c  is present.
c
	if(dotsys)then
	  call uvprobvr(lVis,'tcorr',type,length,updated)
	  if(length.ne.1)call bug('f',
     *		'Required info for options=auto is missing')
	endif
c
c  If opacity correction is requested and the met file was not
c  given, check that the dataset has the met data.
c
	if(doopcorr.and..not.domet)then
	  call uvprobvr(lVis,'airtemp',type,length,updated)
	  if(type.ne.'r'.and.length.ne.1)call bug('f',
     *		'Met data missing from input')
	endif
	call uvrdvri(lVis,'nants',na,0)
	ptime = preamble(4) - 1
c
	dowhile(nchan.gt.0)
c	  call uvrdvrr(lVis,'jyperk',jyperk,0.0)
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
	  jyperk = getjpk(freq0(1)*1e-9)
	  dojpk = .false.
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
	  if((doopcorr.or.dogel.or.dobl))then
	    if(abs(ptime-preamble(4)).gt.5.d0/86400.d0)then
	      ptime = preamble(4)
	      call getlst(lVis,lst)
	      call azel(ra,dec,lst,lat,az,el)
	      newel = .true.
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
c  Apply atmospheric opacity correction, if needed. Apply this both
c  to the data, and to the jyperk system efficiency factor.
c
	  if(doopcorr.and.newel)then
	    if(domet)then
	      call metGet(ptime,t0,p0,h0)
	    else if(uvvarUpd(vmdata))then
	      call uvgetvrr(lVis,'airtemp',t0,1)
	      call uvgetvrr(lVis,'pressmb',p0,1)
	      call uvgetvrr(lVis,'relhumid',h0,1)
	      t0 = t0 + 273.15
	      h0 = 0.01*h0
	      p0 = 100*p0
	    endif
	    call opacGet(nif,freq0,real(el),t0,p0,h0,fac,Tb)
	    scale = 1
	    do i=1,nif
	      scale = scale*fac(i)
	    enddo
	    scale = scale**(-1.0/real(nif))
	    call uvputvrr(lOut,'tsky',Tb,nif)
	    call uvputvrr(lOut,'trans',fac,nif)
	    call uvputvrd(lOut,'antel',180.0d0/DPI*el,1)
	    call uvputvrr(lOut,'airmass',1./sin(real(el)),1)
	  endif
c
	  if(doopcorr)then
	    k = 0
	    do i=1,nif
	      do j=1,nschan(i)
	        k = k + 1
	        data(k) = data(k) / fac(i)
	      enddo
	    enddo
            jyperk = scale * jyperk
	    dojpk = .true.
	  endif
c
c  Apply baseline correction, if needed.
c
	if(dobl)call blcorr(data,freq,nchan,ra,dec,lst,
     *				preamble(5),delta,ATANT)
c
c  Apply the Tsys correction, if needed.
c
	  tcorr = 1
	  if(dotsys)call uvrdvri(lVis,'tcorr',tcorr,0)
	  if(tcorr.eq.0)then
	    if(uvvarUpd(vtsys))then
	      call uvprobvr(lVis,'xtsys',type,length,updated)
	      nants = length/nif
	      if(nants*nif.ne.length.or.nants.le.0.or.nants.gt.MAXANT
     *	        .or.type.ne.'r')call bug('f','Invalid tsys parameter')
	      if(na.ne.nants)
     *		call bug('f','Inconsistency in number of IFs')
	      call uvgetvrr(lVis,'xtsys',xtsys,nants*nif)
	      call uvprobvr(lVis,'ytsys',type,length,updated)
	      if(nants*nif.ne.length.or.type.ne.'r')
     *			      call bug('f','Invalid ytsys parameter')
	      call uvgetvrr(lVis,'ytsys',ytsys,nants*nif)
	    endif
c
	    call basant(preamble(5),i1,i2)
	    call tsysap(data,nchan,nschan,xtsys,ytsys,
     *	      nants,nif,i1,i2,pol)
	  endif
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
	  newel = .false.
	enddo
c
	if(domet)call metFin
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine tsysap(data,nchan,nschan,xtsys,ytsys,nants,nif,
     *							    i1,i2,pol)
c
	implicit none
	integer nchan,nants,nif,nschan(nif),i1,i2,pol
	real xtsys(nants,nif),ytsys(nants,nif)
	complex data(nchan)
c
c------------------------------------------------------------------------
	integer XX,YY,XY,YX
	parameter(XX=-5,YY=-6,XY=-7,YX=-8)
	integer i,j,k
	real T1T2
c
	i = 0
	do k=1,nif
	  if(i+nschan(k).gt.nchan)call bug('f','Invalid description')
	  do j=1,nschan(k)
	    i = i + 1
	    if(pol.eq.XX)then
	      T1T2 = xtsys(i1,k)*xtsys(i2,k)
	    else if(pol.eq.YY)then
	      T1T2 = ytsys(i1,k)*ytsys(i2,k)
	    else if(pol.eq.XY)then
	      T1T2 = xtsys(i1,k)*ytsys(i2,k)
	    else if(pol.eq.YX)then
	      T1T2 = ytsys(i1,k)*xtsys(i2,k)
	    else
	      call bug('f','Invalid polarization code')
	    endif
c
	    data(i) = data(i)*sqrt(T1T2)/50.0
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine getopt(dogel,dotsys,doopcorr)
c
	implicit none
	logical dogel,dotsys,doopcorr
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'gainel  ','tsys     ','opcorr  '/
c
	call options('options',opts,present,NOPTS)
	dogel    = present(1)
	dotsys   = present(2)
	doopcorr = present(3)
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
	integer i,j
	character line*80
	logical ok
	real x,y
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
	  xyz(i)        = FAC*x
	  xyz(i+nant)   = FAC*y
	  xyz(i+2*nant) = 0 
	enddo
c
	end
c************************************************************************
	subroutine metInit(mdata)
c
	implicit none
	character mdata*(*)
c------------------------------------------------------------------------
	real t(2),p(2),h(2)
	double precision time(2)
	common/metcom/time,t,p,h
c
	call tinOpen(mdata,'n')
	call metRec(time(1),t(1),p(1),h(1))
	call metRec(time(2),t(2),p(2),h(2))
	end
c************************************************************************
	subroutine metRec(time,t0,p0,h0)
c
	implicit none
	real t0,p0,h0
	double precision time
c------------------------------------------------------------------------
	double precision dtime
	character type*12
	logical   ok
c
c  Externals.
c
	integer tinNext
c
	if(tinNext().le.0)call bug('f','Error getting met data')
	call tinGeta(type,' ')
	if(type.eq.'dsd34')then
	  call tinGett(time,0.d0,'atime')
	  call tinSkip(21)
	  call tinGetr(t0,0.0)
	  call tinGetr(p0,0.0)
	  call tinGetr(h0,0.0)
	else if(type.eq.'met')then
	  call tinGett(time,0.d0,'atime')
          call tinGett(dtime,0.0d0,'dtime')
	  time = time + dtime - 10.0d0/24.0d0
	  call tinSkip(1)
	  call tinGetr(t0,0.0)
	  call tinSkip(2)
	  call tinGetr(p0,0.0)
	  call tinSkip(1)
	  call tinGetr(h0,0.0)
	else
          call dectime(type,time,'atime',ok)
	  if(.not.ok)
     *    call tinbug('f','Error decoding time in met data')
c	  call tinGett(time,0.d0,'atime')
	  call tinGett(dtime,0.0d0,'dtime')
	  time = time + dtime - 10.0d0/24.0d0
	  call tinSkip(1)
	  call tinGetr(t0,0.0)
	  call tinSkip(2)
	  call tinGetr(p0,0.0)
	  call tinSkip(1)
	  call tinGetr(h0,0.0)
	endif
c
c  Convert time to UT, temperature to kelvin, pressue to Pascals at Narrabri
c  and humidity to a fraciton.
c
	t0 = t0 + 273.15
	p0 = 0.975*100.0*p0
	h0 = 0.01*h0
c
	end
c************************************************************************
	subroutine metGet(time0,t0,p0,h0)
c
	double precision time0
	real t0,p0,h0
c------------------------------------------------------------------------
	integer i
c
	real t(2),p(2),h(2)
	double precision time(2)
	common/metcom/time,t,p,h
c
c  Point to the earlier time.
c
	i = 1
	if(time(1).gt.time(2))i = 2
c
c  Step through until we straddle two sets of measurements.
c
	dowhile(time0.gt.time(3-i))
	  call metRec(time(i),t(i),p(i),h(i))
	  i = 3 - i
	enddo
c
c  Return the measurements closest to the requested time.
c
	i = 1
	if(abs(time0-time(1)).gt.abs(time0-time(2)))i = 2
	t0 = t(i)
	p0 = p(i)
	h0 = h(i)
c
	end
c************************************************************************
	subroutine metFin
	call tinClose
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
	data (wpc(1,j),j=1,3)/1.0000, 0.0000,     0.0000/
	data (wpc(2,j),j=1,3)/0.4877, 1.7936e-2, -1.5699e-4/
	data (wpc(3,j),j=1,3)/0.7881, 0.8458e-2, -0.8442e-4/
	data (wpc(4,j),j=1,3)/0.7549, 1.2585e-2, -1.6153e-4/
	data (wpc(5,j),j=1,3)/1.0000, 0.0000,     0.0000/
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
	subroutine blcorr(data,freq,nchan,ra,dec,lst,bl,xyz,nant)
c
	implicit none
	integer nchan,nant
	complex data(nchan)
	double precision ra,dec,lst,bl,freq(nchan)
	real xyz(3,nant)
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer ant1,ant2,i
	real delX,delY,delZ,theta,antphz,cosha,sinha,cosdec,sindec
	complex w
	double precision HA
c
	call basant(bl,ant1,ant2)
	if(min(ant1,ant2).lt.1.or.max(ant1,ant2).gt.nant)
     *	  call bug('f','Invalid antenna number')
c
	delX = xyz(1,ant2) - xyz(1,ant1)
	delY = xyz(2,ant2) - xyz(2,ant1)
	delZ = xyz(3,ant2) - xyz(3,ant1)
	if(abs(delX)+abs(delY)+abs(delZ).ne.0)then
          HA = lst - ra
          cosHA = cos(HA)
          sinHA = sin(HA)
          cosdec = cos(dec)
          sindec = sin(dec)
          antphz = 2*DPI*((delX*cosHA - delY*sinHA)*cosdec +
     *			   delZ*sindec)
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
	  getjpk = 15
	else
	  getjpk = 25
	endif
c
	end
