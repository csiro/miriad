c**********************************************************************c
	program uvindex
	implicit none
c
c= UVINDEX - Scan a uvdata file, and note when uvvariables change.
c& mchw
c: uv analysis, checking
c+
c	UVINDEX is a Miriad program which scans a uvdata file.
c	Changes in source name, pointing center, number of wideband
c	and spectral line channels, the observing frequency, and
c	polarization are listed in the output.
c@ vis
c	The input visibility file. No default.
c@ interval
c	Reissue source information if no data for this period in minutes.
c	The default is 60 minutes.
c@ log
c	The output log file. Default is the terminal.
c@ options
c	Extra processing options. Currently there is but one of these:
c	  mosaic  Do not generate messages for different pointings
c	          of a mosaic.
c--
c
c  History:
c    mchw 29nov90  Initial version.
c    mchw 14dec90  Cleaned up. Used LogWrit.
c    mchw 08jan91  List pointing centers.
c    mchw 19mar91  Initialized some variables for cray. Added recnum.
c    rjs   9may92  Altered listing for multi-source/multi-pointing files.
c    mchw 25jun92  Added count of polarizations.
c    rjs  10jul92  Labelled polarisation counts.
c    rjs  22jan93  Bug when there is no (0,0) offset pointing.
c		   Various standardisation.
c    mchw 09feb93  Changed ra,dec to double precision.
c    mchw 19feb93  Format change for 2048 channels.
c    pjt  10mar93  fixed problems with length in logwrite() - no need
c    pjt  15apr93  maxpnt -> 512 for SUN etc. fixing wrong ddec table
c    pjt  23apr93  need special case if data contain unknown polarization
c    rjs  16jul93  Significant rewrite. More efficient scanning and treatment
c		   of polarisations. Better freq summary.
c    rjs  30aug93  Call logclose.
c    rjs  16sep93  Rename bsrch to binsrch.
c    rjs   5nov93  Longer source names.
c    rjs  30nov93  Correct units of first pointing offset. Fix formating
c		   bug.
c    rjs  26jan94  New message when there is a jump in time.
c    nebk 28mar94  Add keyword interval
c    rjs  23aug94  Minor formatting change.
c    rjs   2nov94  Changed the way sources were stored, and increased
c		   the number of sources/pointings.
c    rjs  16nov94  Fix bug introduced in the above.
c    rjs   1may96  Compute and print out total observing time.
c    mchw 01aug96  Increased MAXSPECT=18.
c    rjs  18oct96  Don't output a line when just dra/ddec changes.
c    rjs  08jan97  options=mosaic
c    rjs  08jun97  Fix bug in error message
c----------------------------------------------------------------------c
	include 'mirconst.h'
	include 'maxdim.h'
	character*(*) version
	integer MAXSRC,MAXFREQ,MAXSPECT
	integer PolMin,PolMax,PolI
	parameter(MAXSRC=2048,MAXFREQ=32,MAXSPECT=18)
	parameter(PolMin=-8,PolMax=4,PolI=1)
	parameter(version='UVINDEX: version 1.0 08-Jun-97')
c
	integer pols(PolMin:PolMax),pol
	integer lIn,i,j,j1,nvis,nants,l
	character vis*64,logf*64,date*18,line*80,ras*14,decs*14
	double precision time,tprev,total
	real dra,ddec,interval,inttime
c
	integer ifreq,nfreq,vfreq
	logical newfreq,mosaic
	integer nwide(MAXFREQ),nchan(MAXFREQ),nspect(MAXFREQ)
	integer nschan(MAXSPECT,MAXFREQ)
	real wfreqs(MAXSPECT,3,MAXFREQ)
	double precision sfreqs(MAXSPECT,3,MAXFREQ)
c
	integer isrc,nsrc,vsource
	logical newsrc,solar(MAXSRC)
	double precision ra0(MAXSRC),dec0(MAXSRC)
	real pntoff(2,MAXSRC)
	character sources(MAXSRC)*16,prevsrc*16
	integer indx(MAXSRC)
c
c  Externals
c
	integer len1,uvscan
	character rangle*14,hangle*14,PolsC2P*2,itoaf*8
	logical uvvarupd
c
c  Get the parameters given by the user.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
        call keyr('interval',interval,0.0)
	call keya('log',logf,' ')
	call GetOpt(mosaic)
	call keyfin
c
c  Check that the inputs are reasonable.
c
	if(vis.eq.' ') call bug ('f', 'Input name must be given')
c
c  Initialise the source table.
c
	do i=1,MAXSRC
	  sources(i) = ' '
	enddo
c
c  Open an old visibility file, and apply selection criteria.
c
	call uvopen(lIn,vis,'old')
c
c  Open log file and write title.
c
	call LogOpen(logf,' ')
	if (logf.ne.' ')call LogWrit(version)
	call LogWrit(' ')
	line = 'Summary listing for data-set '//vis
	call LogWrit(line(1:len1(line)))
	call LogWrit(' ')
	write(line,'(7x,a,8x,a,7x,a,a,a,a,a,8x,a)') 'Time','Source',
     *	  ' Antennas',' Spectral',' Wideband','  Freq ',' Record'
	call LogWrit(line(1:len1(line)))
	write(line,'(19x,a,7x,a,a,a,a,a)')		  ' Name ',
     *	  '         ',' Channels',' Channels',' Config','   No. '
	call LogWrit(line(1:len1(line)))
	call LogWrit(' ')
c
c  Set up a variable handle to track changes in the source.
c
	call uvvarini(lIn,vsource)
	call uvvarset(vsource,'source')
	call uvvarset(vsource,'ra')
	call uvvarset(vsource,'dec')
	call uvvarset(vsource,'dra')
	call uvvarset(vsource,'ddec')
c
c  Set up a variable handle to track changes in the correlator/freq setup.
c
	call uvvarini(lIn,vfreq)
	call uvvarset(vfreq,'nchan')
	call uvvarset(vfreq,'nspect')
	call uvvarset(vfreq,'nschan')
	call uvvarset(vfreq,'sfreq')
	call uvvarset(vfreq,'sdf')
	call uvvarset(vfreq,'nwide')
	call uvvarset(vfreq,'wfreq')
	call uvvarset(vfreq,'wwidth')
c
c  Initialise the counters.
c
	nsrc = 0
	nvis = 0
	nfreq = 0
	do i=PolMin,PolMax
	  pols(i) = 0
	enddo
	isrc = 0
	ifreq = 0
	tprev = 0.
        interval = interval/24.0/60.0
	if(interval.le.0)interval = 1.d0/24.d0
	total = 0.d0
c
c  Scan through the uvdata noting when a number of things change.
c
	do while(uvscan(lin,' ').eq.0)
	  nvis = nvis + 1
	  call uvrdvrd(lIn,'time',time,0.d0)
	  call uvrdvri(lIn,'pol',pol,PolI)
	  if(pol.lt.PolMin.or.Pol.gt.PolMax)pol = 0
	  pols(pol) = pols(pol) + 1
c
c  Determine if anything has changed, and update the records
c  accordingly.
c
	  newsrc = .false.
	  newfreq= .false.
	  if(uvvarupd(vsource))call GetSrc(lIn,mosaic,newsrc,isrc,nsrc,
     *		sources,ra0,dec0,pntoff,solar,MAXSRC)
	  if(uvvarupd(vfreq))  call GetFreq(lIn,newfreq,ifreq,nfreq,
     *		nchan,nspect,nschan,sfreqs,nwide,wfreqs,
     *		MAXFREQ,MAXSPECT)
c
c  If something has changed, give a summary of things.
c
	  if(newsrc.or.newfreq.or.time.gt.tprev+interval.or.
     *	     time.lt.tprev)then
	    call JulDay(time,'H',date)
	    call uvrdvri(lIn,'nants',nants,0)
	    write(line,'(a,x,a,i3,i10,i9,i7,i8)')date,sources(isrc),
     *		nants,nchan(ifreq),nwide(ifreq),ifreq,nvis
	    call LogWrit(line)
	  endif
c
c  Increment the total observing time if this is a new integration.
c
	  call uvrdvrr(lIn,'inttime',inttime,30.0)
	  inttime = inttime/86400
	  if(abs(time-tprev).gt.1.d0/86400.d0)then
	    tprev = time
	    total = total + inttime
	  endif
	enddo
c
c  Give a summary to the user.
c
	call uvrdvrd(lIn,'time',time,0.d0)
	call JulDay(time,'H',date)
	write(line,'(a,a,i30)') date,' Total number of records',nvis
	call LogWrit(line)
c
c  Frequency setup summary.
c
	call LogWrit(' ')
	call LogWrit('------------------------------------------------')
	call LogWrit(' ')
c
	total = 24*total
	write(line,'(a,f6.2,a)')'Total observing time is',total,' hours'
	call LogWrit(line)
	call LogWrit(' ')	
	call LogWrit('The input data-set contains '//
     *		'the following frequency configurations:')
	do ifreq=1,nfreq
	  call LogWrit(' ')
	  call logwrit('Frequency Configuration '//itoaf(ifreq))
	  if(nchan(ifreq).gt.0)call SpecSum(
     *	    nspect(ifreq),nschan(1,ifreq),sfreqs(1,1,ifreq),MAXSPECT)
	  if(nwide(ifreq).gt.0)call WideSum(
     *	    nwide(ifreq),wfreqs(1,1,ifreq),MAXSPECT)
	enddo
c
c  Polarisation summary.
c
	call LogWrit(' ')
	call LogWrit('------------------------------------------------')
	call LogWrit(' ')
	call LogWrit(
     *	  'The input data-set contains the following polarizations:')
	do i=PolMin,PolMax
	  if(i.ne.0.and.pols(i).gt.0)then
	    line = 'There were '//itoaf(pols(i))
	    l = len1(line)
	    line(l+1:) = ' records of polarization '//PolsC2P(i)
	    call LogWrit(line)
          endif
	enddo
	if(pols(0).ne.0)then
 	  write(line,'(a,i6,a)')
     *      'Unrecognised polarization ',pols(0),' records'
	  call LogWrit(line)
	endif
c
c  Pointing centres summary.
c
	call LogWrit(' ')
	call LogWrit('------------------------------------------------')
	call LogWrit(' ')
	call LogWrit(
     *	  'The input data-set contains the following pointings:')
	call LogWrit(
     *	  ' Source                   RA            DEC        '//
     *	  '     dra(arcsec) ddec(arcsec)')
c
c  Sort the sources.
c
	call hsorta(MAXSRC,sources,indx)
	prevsrc = ' '
	do j=1,MAXSRC
	  j1 = indx(j)
	  if(sources(j1).ne.' ')then
	    if(sources(j1).ne.prevsrc)then
	      ras = hangle(ra0(j1))
	      decs = rangle(dec0(j1))
	      write(line,'(a,7x,a,a,2f13.2)')sources(j1),ras,decs,
     *		180*3600/pi * pntoff(1,j1),
     *		180*3600/pi * pntoff(2,j1)
	      call LogWrit(line)
	    else
	      dra  = 180*3600/pi * pntoff(1,j1)
	      ddec = 180*3600/pi * pntoff(2,j1)
	      write(line,'(51x,2f13.2)')dra,ddec
	      call LogWrit(line)
	    endif
	  endif
	  prevsrc = sources(j1)
	enddo
	call LogWrit(' ')
	call LogWrit('------------------------------------------------')
	call LogClose
c
	end
c************************************************************************
	subroutine GetOpt(mosaic)
c
	implicit none
	logical mosaic
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'mosaic  '/
c
	call options('options',opts,present,NOPTS)
	mosaic = present(1)
	end
c************************************************************************
	subroutine GetSrc(lIn,mosaic,newsrc,isrc,nsrc,
     *		sources,ra0,dec0,pntoff,solar,MAXSRC)
c
	implicit none
	integer MAXSRC
	integer lIn,isrc,nsrc
	logical newsrc,solar(MAXSRC),mosaic
	character sources(MAXSRC)*(*)
	double precision ra0(MAXSRC),dec0(MAXSRC)
	real pntoff(2,MAXSRC)
c
c  Determine whether we have a new source and pointing or not.
c
c  Input:
c    mosaic	Treat the observation as a mosaic.
c  Output:
c    newsrc	True if the source has changes.
c------------------------------------------------------------------------
c
c  Tolerance in assuming a new pointing is 1 arcsec.
c
	include 'mirconst.h'
	real tol
	parameter(tol=pi/180.0/3600.0)
	character source*16,osource*16
	double precision ra,dec
	real dra,ddec
	logical more,refed,found
	integer hash,i,i1,i2
c
c  Externals.
c
	logical DetSolar
	integer len1
c
c  Get source parameters.
c
	call uvrdvra(lIn,'source',source,' ')
	call uvrdvrd(lIn,'ra',ra,0.d0)
	call uvrdvrd(lIn,'dec',dec,0.d0)
	call uvrdvrr(lIn,'dra',dra,0.)
	call uvrdvrr(lIn,'ddec',ddec,0.)
c
c  Is it a new source?
c
	refed = .false.
	if(nsrc.eq.0)then
	  osource = ' '
	  newsrc = .true.
	else
	  osource = sources(isrc)
	  if(source.eq.sources(isrc))then
	    if(.not.solar(isrc))
     *	      call GetDelta(dra,ddec,ra,dec,ra0(isrc),dec0(isrc))
	    newsrc = abs(dra-pntoff(1,isrc)).gt.tol.or.
     *		     abs(ddec-pntoff(2,isrc)).gt.tol
	    refed = .true.
	  else
	    newsrc = .true.
	  endif
	endif
c
c  Process a new source.
c
	if(newsrc)then
	  hash = 0
	  do i=1,len1(source)
	    hash = 3*hash + ichar(source(i:i))
	  enddo
	  isrc = mod(hash,MAXSRC) + 1
	  more = .true.
	  found = .false.
	  dowhile(more)
	    if(sources(isrc).eq.source)then
	      if(.not.refed.and..not.solar(isrc))
     *	        call GetDelta(dra,ddec,ra,dec,ra0(isrc),dec0(isrc))
	      refed = .true.
	      found = abs(dra-pntoff(1,isrc)).lt.tol.and.
     *		      abs(ddec-pntoff(2,isrc)).lt.tol
	      more = .not.found
	    else if(sources(isrc).eq.' ')then
	      more = .false.
	    endif
	    if(more)then
	      isrc = isrc + 1
	      if(isrc.gt.MAXSRC) isrc = 1
	    endif
	  enddo
c
c  Did we find this source?
c
	  if(.not.found)then
	    nsrc = nsrc + 1
	    if(nsrc.ge.MAXSRC)call bug('f','Too many sources')
	    sources(isrc) = source
	    ra0(isrc) = ra
	    dec0(isrc) = dec
	    solar(isrc) = DetSolar(lIn,source)
	    pntoff(1,isrc) = dra
	    pntoff(2,isrc) = ddec
	  endif
	endif
c
	if(mosaic)then
	  i1 = index(sources(isrc),'_')
	  i2 = index(osource,'_')
	  if(i1.eq.0.or.i2.eq.0)then
	    newsrc = sources(isrc).ne.osource
	  else
	    newsrc = sources(isrc)(1:i1).ne.osource(1:i2)
	  endif
	else
	  newsrc = sources(isrc).ne.osource
	endif
c
	end
c************************************************************************
	subroutine GetDelta(dra,ddec,ra,dec,ra0,dec0)
c
	implicit none
	double precision ra,dec,ra0,dec0
	real dra,ddec
c
c  Reference a dra,ddec to the true reference.
c
c  Input:
c    ra0,dec0
c  Input/Output:
c   dra,ddec,ra,dec
c
c------------------------------------------------------------------------
	dra = dra * cos(dec0)/cos(dec) + (ra-ra0)*cos(dec0)
	ddec = ddec + (dec-dec0)
	ra = ra0
	dec = dec0
	end
c************************************************************************
	logical function DetSolar(lIn,source)
c
	implicit none
	integer lIn
	character source*(*)
c
c  Attempt to determine whether the current object of interest is a
c  solar system object. This will probably fail for objects such as
c  comets.
c------------------------------------------------------------------------
	character string*16
	real plmaj,plmin
	integer i
c
c  A table of solar system objects. NOTE: The entries must be in
c  alphabetic order and lower case.
c
	integer NSOLAR
	parameter(NSOLAR=11)
	character solar(NSOLAR)*8
c
c  Externals.
c
	integer binsrcha
c
	data solar/'earth   ','jupiter ','mars    ','mercury ',
     *	'moon    ','neptune ','pluto   ','saturn  ','sun     ',
     *	'uranus  ','venus   '/
c
c  Look for the source name in the list of solar system objects.
c
	string = source
	call lcase(string)
	i = binsrcha(string,solar,NSOLAR)
	if(i.ne.0)then
	  DetSolar = .true.
c
c  If it was not found in the list of known solar system objects,
c  see if it has plmaj and plmin variables. If so, its probably
c  a solar system object.
c
	else
	  call uvrdvrr(lIn,'plmaj',plmaj,0.)
	  call uvrdvrr(lIn,'plmin',plmin,0.)
	  DetSolar = abs(plmaj)+abs(plmin).gt.0
	endif
	end
c************************************************************************
	subroutine GetFreq(lIn,newfreq,ifreq,nfreq,
     *		nchan,nspect,nschan,sfreqs,nwide,
     *		wfreqs,MAXFREQ,MAXSPECT)
c
	implicit none
	integer MAXFREQ,MAXSPECT
	integer lIn,ifreq,nfreq,nchan(MAXFREQ),nspect(MAXFREQ)
	integer nschan(MAXSPECT,MAXFREQ),nwide(MAXFREQ)
	double precision sfreqs(MAXSPECT,3,MAXFREQ)
	real wfreqs(MAXSPECT,3,MAXFREQ)
	logical newfreq
c
c  Keep track of frequency/correlator setups. Determine whether we
c  have a new correlator or freq setup.
c
c  The frequency setup arrays, sfreqs and wfreqs (spectral and wide
c  frequencies respectively) contain three values,
c	freqs(?,1,?) is the start frequency in the first record.
c	freqs(?,2,?) is the bandwidth or channel increment.
c       freqs(?,3,?) is the start frequency in the last record.
c  The start frequency in the first and last record can differ, owing to
c  slow changes in the frequency caused by Doppler tracking and the like.
c------------------------------------------------------------------------
	integer itmp,i
	logical more
c
c  Externals.
c
	logical FreqEq
c
	itmp = nfreq + 1
	if(itmp.gt.MAXFREQ)call bug('f','Frequency table overflow')
c
c  Load the current freq/correlator description.
c
	call uvrdvri(lIn,'nchan',nchan(itmp),0)
	call uvrdvri(lIn,'nspect',nspect(itmp),0)
	if(nspect(itmp).gt.MAXSPECT)call bug('f','Too many windows')
	call uvrdvri(lIn,'nwide',nwide(itmp),0)
	if(nwide(itmp).gt.MAXSPECT)call bug('f','Too many wide chans')
	if(nchan(itmp).gt.0)then
	  call uvgetvrd(lIn,'sfreq', sfreqs(1,1,itmp),nspect(itmp))
	  call uvgetvrd(lIn,'sdf',   sfreqs(1,2,itmp),nspect(itmp))
	  call uvgetvri(lIn,'nschan',nschan(1,itmp),nspect(itmp))
	endif
	if(nwide(itmp).gt.0)then
	  call uvgetvrr(lIn,'wfreq', wfreqs(1,1,itmp),nwide(itmp))
	  call uvgetvrr(lIn,'wwidth',wfreqs(1,2,itmp),nwide(itmp))
	endif
c
c  Is it a new frequency/correlator setup?
c
	if(nfreq.eq.0)then
	  newfreq = .true.
	else
	  newfreq = .not.FreqEq(ifreq,itmp,nchan,nspect,nschan,sfreqs,
     *			nwide,wfreqs,MAXSPECT,MAXFREQ)
	endif
c
c  Process a new frequency.
c
	if(newfreq)then
	  ifreq = 1
	  more = .true.
	  dowhile(ifreq.le.nfreq.and.more)
	    more = .not.FreqEq(ifreq,itmp,nchan,nspect,nschan,sfreqs,
     *			nwide,wfreqs,MAXSPECT,MAXFREQ)
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
	do i=1,nwide(ifreq)
	  wfreqs(i,3,ifreq) = wfreqs(i,1,itmp)
	enddo
c
	end
c************************************************************************
	logical function FreqEq(i1,i2,nchan,nspect,nschan,sfreqs,
     *		nwide,wfreqs,MAXSPECT,MAXFREQ)
c
	implicit none
	integer MAXSPECT,MAXFREQ
	integer i1,i2,nchan(MAXFREQ),nspect(MAXFREQ)
	integer nschan(MAXSPECT,MAXFREQ),nwide(MAXFREQ)
	double precision sfreqs(MAXSPECT,3,MAXFREQ)
	real wfreqs(MAXSPECT,3,MAXFREQ)
c
c  Determine whether two correlator/frequency setups are the same.
c  For them to be the same, nchan, nwide, nspec, nschan have to
c  match exactly. wwidth and sdf has to match to 1%, and sfreq and
c  wfreq have to match to within half a channel.
c
c------------------------------------------------------------------------
	integer i
	real w
c
	FreqEq = .false.
	if(nchan(i1).ne.nchan(i2).or.
     *	   nspect(i1).ne.nspect(i2).or.
     *	   nwide(i1).ne.nwide(i2))return
c
	do i=1,nspect(i1)
	  if(nschan(i,i1).ne.nschan(i,i2))return
	  w = abs(sfreqs(i,2,i1))
	  if(abs(sfreqs(i,2,i1)-sfreqs(i,2,i2)).gt.0.01*w)return
	  if(abs(sfreqs(i,3,i1)-sfreqs(i,1,i2)).gt.0.5*w)return
	enddo
c
	do i=1,nwide(i1)
	  w = abs(wfreqs(i,2,i1))
	  if(abs(wfreqs(i,2,i1)-wfreqs(i,2,i2)).gt.0.01*w)return
	  if(abs(wfreqs(i,3,i1)-wfreqs(i,1,i2)).gt.0.5*w)return
	enddo
c
	FreqEq = .true.
	end
c************************************************************************
	subroutine SpecSum(nspect,nschan,sfreqs,MAXSPECT)
c
	implicit none
	integer nspect,nschan(nspect),MAXSPECT
	double precision sfreqs(MAXSPECT,3)
c
c  Write a summary about this spectral correlator configuration.
c  Assume Doppler tracking is being used if the difference between
c  the first and last start frequencies is more than 0.1 of a channel.
c------------------------------------------------------------------------
	character line*80,vary*16
	integer i
c
	call logwrit('  Spectral Channels  Freq(chan=1)  Increment')
	do i=1,nspect
	  if(abs(sfreqs(i,1)-sfreqs(i,3)).gt.0.1*abs(sfreqs(i,2)))then
	    vary = ' Doppler tracked'
	  else
	    vary = ' '
	  endif
	  write(line,'(i17,f14.5,f13.6,a,a)')
     *		nschan(i),sfreqs(i,1),sfreqs(i,2),' GHz',vary
	  call logwrit(line)
	enddo
	end
c************************************************************************
	subroutine WideSum(nwide,wfreqs,MAXSPECT)
c
	implicit none
	integer nwide,MAXSPECT
	real wfreqs(MAXSPECT,3)
c
c  Write a summary about this wideband correlator configuration.
c------------------------------------------------------------------------
	character line*80,vary*8
	integer i
c
	call logwrit('  Wideband Channels  Frequency     Bandwidth')
	do i=1,nwide
	  if(abs(wfreqs(i,1)-wfreqs(i,3)).gt.0.1*abs(wfreqs(i,2)))then
	    vary = ' Varying'
	  else
	    vary = ' '
	  endif
	  write(line,'(17x,f14.5,f13.6,a,a)')
     *		wfreqs(i,1),wfreqs(i,2),' GHz',vary
	  call logwrit(line)
	enddo
	end

