c************************************************************************
	program gplist
	implicit none
c
c= GpList -- List information about calibration tables.
c& rjs
c: calibration
c+
c	GpList is a Miriad task to list information about visibility
c	calibration tables and to check their consistency.
c@ vis
c	The input dataset containing the visibility calibration tables.
c--
c  History:
c    rjs     14may09 Original version.
c    mhw     31oct13 List gainsf/leakagef table info
c-----------------------------------------------------------------------
        include 'maxdim.h'
        include 'mem.h'
	character version*(*)
	parameter(version='GpList: version 1.0 14-May-09')
	character vis*80,line*64,senmodl*8
	integer nfeeds,ntau,nsols,ngains,nants,size,iostat, nLeaks
	integer nchan,nspect,off,i,nschan, maxgains, nfbin, nfbin2
	double precision freqs(2),interval,freq(MAXFBIN)
	integer pnants,p2nants
	integer iGains,iLeak,iPass,tVis
        integer pGains,pTimes
	logical dopol,dopol2,dopolf
c
c  Externals.
c
	integer hsize
	character itoaf*9, dtoaf*6
	logical hdprsnt
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keyfin
	if(vis.eq.' ')call bug('f','An input file must be given')
c
c  Open the visibility file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call myBug(iostat,'Error opening '//vis)
c
c  Give information about polarisation calibration tables.
c
	dopol = hdprsnt(tVis,'leakage')
	if(dopol)then
	  call output(' ')
	  call output('Polarisation calibration')
	  call output('------------------------')
	  call haccess(tVis,iLeak,'leakage','read',iostat)
	  if(iostat.ne.0)call myBug(iostat,
     *			'Error checking leakage table')
	  size = hsize(iLeak)
	  pnants = (size-8)/16
	  if(size.ne.16*pnants+8)call bug('w',
     *	    'Leakage table size does not look correct')
	  call output('Number of antennas: '//itoaf(pnants))
	  call hdaccess(iLeak,iostat)
	  if(iostat.ne.0)call myBug(iostat,
     *		'Error closing leakage table')
	endif
c
c  Handle second leakage table.
c
	dopol2 = hdprsnt(tVis,'leakage2')
	if(dopol2)then
	  call output(' ')
	  call output('Second Polarisation calibration')
	  call output('-------------------------------')
	  call haccess(tVis,iLeak,'leakage2','read',iostat)
	  if(iostat.ne.0)call myBug(iostat,
     *			'Error checking second leakage table')
	  size = hsize(iLeak)
	  p2nants = (size-8)/16
	  if(size.ne.16*p2nants+8)call bug('w',
     *	    'Second leakage table size does not look correct')
	  call output('Number of antennas: '//itoaf(p2nants))
	  if(dopol)then
	    if(pnants.ne.p2nants)call bug('w','Polarisation leakage'//
     *		' tables imply different number of antennas')
	  else
	    call bug('w','Main leakage table not present')
	    dopol = .true.
	    pnants = p2nants
	  endif
	  call hdaccess(iLeak,iostat)
	  if(iostat.ne.0)call myBug(iostat,
     *		'Error closing second leakage table')
	endif
c
c  Check for frequency dependent leakages
c
        dopolf = hdprsnt(tVis,'leakagef')
        if(dopolf)then
          call output(' ')
          call output('Frequency dependent Polarisation calibration')
          call output('--------------------------------------------')
	  call rdhdi(tVis,'nfbin',  nfbin,  0)
          call haccess(tVis,iLeak,'leakagef','read',iostat)
          if (iostat.ne.0) then
            call mybug(iostat,'Error checking the  leakagef table')
          endif
          nLeaks = (hsize(iLeak)-8)/8
          if (nLeaks.lt.3) call bug('w',
     *      'Freq Dep leakage table size too small')
          call hreadi(iLeak,nfbin2,4,4,iostat)
          if (iostat.ne.0) then
            call mybug(iostat,'Error checking the  leakagef table')
          endif
          call output('Number of bins:    '//itoaf(nfbin2))
          if (nfbin2.ne.nfbin) call bug('f',
     *       'Number of bins in header and leakagef table differ')
          nLeaks = (nLeaks/nfbin-1)/2
          call output('Number of antenas: '//itoaf(nLeaks))
	  call hdaccess(iLeak,iostat)
	  if(iostat.ne.0)call myBug(iostat,
     *		'Error closing leakagef table')
        endif    
c
c  Give information about bandpass calibration tables.
c
	if(hdprsnt(tVis,'bandpass'))then
	  call rdhdi(tVis,'ngains',ngains,0)
	  call rdhdi(tVis,'nfeeds',nfeeds,1)
	  call rdhdi(tVis,'ntau',  ntau,  0)
	  if(nfeeds.le.0.or.nfeeds.gt.2.or.mod(ngains,nfeeds+ntau).ne.0
     *	    .or.ntau.gt.1.or.ntau.lt.0)
     *	    call bug('f','Bad number of gains or feeds in '//vis)
	  nants = ngains / (nfeeds + ntau)
	  call rdhdi(tVis,'nchan0',nchan,0)
	  call rdhdi(tVis,'nspect0',nspect,0)
	  if(nchan.le.0.or.nspect.le.0)call bug('f',
     *	    'Invalid number of channels or windows')
	  call haccess(tVis,iPass,'freqs','read',iostat)
	  if(iostat.ne.0)call myBug(iostat,
     *		'Error accessing bandpass frequency table')
	  call output(' ')
	  call output('Bandpass response calibration')
	  call output('-----------------------------')
	  call output('Number of antennas: '//itoaf(nants))
	  call output('Number of feeds:    '//itoaf(nfeeds))
	  call output('Number of windows:  '//itoaf(nspect))
	  call output('Total channels:     '//itoaf(nchan))
	  call output(' ')
	  call output('  Spectrum  Channels  Freq(chan=1)   Increment')
          call output('  --------  --------  ------------   ---------')
	  off = 8
	  do i=1,nspect
	    call hreadi(iPass,nschan,off,4,iostat)
	    off = off + 8
	    if(iostat.eq.0)call hreadd(iPass,freqs,off,2*8,iostat)
	    off = off + 2*8
	    if(iostat.ne.0)call myBug(iostat,
     *		'Error reading bandpass frequency table')
	    write(line,'(i7,i11,f15.6,f13.6,a)')
     *			i,nschan,freqs(1),freqs(2),' GHz'
	    call output(line)
	    nchan = nchan - nschan
	  enddo
	  if(nchan.ne.0)call bug('w',
     *	    'Inconsistency in the total number of channels')
	  call hdaccess(iPass,iostat)
	  if(iostat.ne.0)call myBug(iostat,
     *		'Error closing bandpass frequency table')
	endif
c
c  Give information about the antenna gains table.
c
	if(hdprsnt(tVis,'gains'))then
	  call rdhdi(tVis,'ngains',ngains,0)
	  call rdhdi(tVis,'nfeeds',nfeeds,1)
	  call rdhdi(tVis,'ntau',  ntau,  0)
	  if(nfeeds.le.0.or.nfeeds.gt.2.or.mod(ngains,nfeeds+ntau).ne.0
     *	    .or.ntau.gt.1.or.ntau.lt.0)
     *	    call bug('f','Bad number of gains or feeds in '//vis)
	  nants = ngains / (nfeeds + ntau)
	  call rdhdi(tVis,'nsols',nsols,0)
	  if(nsols.le.0)
     *	    call bug('f','Bad number of antenna gain solutions')
	  call output(' ')
	  call output('Antenna gain calibration')
	  call output('------------------------')
	  call output('Number of antennas:  '//itoaf(nants))
	  call output('Number of feeds:     '//itoaf(nfeeds))
	  call output('Number of solutions: '//itoaf(nsols))
	  call rdhdd(tVis,'interval',interval,0.d0)
	  interval = 24*60 * interval
	  write(line,'(a,f6.1)')'Interpolation tolerance (minutes):',
     *				interval
	  call output(line)
	  call rdhda(tVis,'senmodel',senmodl,' ')
	  if(senmodl.eq.'GSV')then
	    call output('Theoretical variance estimates'//
     *			' scale with calibration gains')
	  else
	    call output('Theoretical variance estimates'//
     *			' are independent of calibration')
	  endif
	  if(ntau.ne.0)call output('Delay terms are present')
	  call output(' ')
	  call haccess(tVis,iGains,'gains','read',iostat)
	  if(iostat.ne.0)call myBug(iostat,'Error checking gains table')
	  size = hsize(iGains)
	  if(size.ne.8+(ngains+1)*8*nsols)
     *	    call bug('w','Gain table size does not look correct')
	  call hdaccess(iGains,iostat)
	  if(iostat.ne.0)call myBug(iostat,'Error closing gains table')
	  if(dopol)then
	    if(nants.ne.pnants)call bug('w',
     *	    'Number of antennas disagrees with polarisation tables')
	    if(nfeeds.ne.2)call bug('w','Polarisation calibration'//
     *		' present but only single feed gains')
	  endif
	endif
c
c  Give information about the antenna gainsf table.
c
	if(hdprsnt(tVis,'gainsf'))then
	  call rdhdi(tVis,'ngains',ngains,0)
	  call rdhdi(tVis,'nfeeds',nfeeds,1)
	  call rdhdi(tVis,'ntau',  ntau,  0)
	  call rdhdi(tVis,'nfbin',  nfbin,  0)
	  if(nfeeds.le.0.or.nfeeds.gt.2.or.mod(ngains,nfeeds+ntau).ne.0
     *	    .or.ntau.gt.1.or.ntau.lt.0)
     *	    call bug('f','Bad number of gains or feeds in '//vis)
          if (nfbin.le.1) call bug('f','Bad number of bins in '//vis)
	  nants = ngains / (nfeeds + ntau)
	  call rdhdi(tVis,'nsols',nsols,0)
	  if(nsols.le.0)
     *	    call bug('f','Bad number of antenna gain solutions')
	  call output(' ')
	  call output('Freq Dependent Antenna gain calibration')
	  call output('---------------------------------------')
	  call output('Number of antennas:  '//itoaf(nants))
	  call output('Number of feeds:     '//itoaf(nfeeds))
	  call output('Number of solutions: '//itoaf(nsols))
	  call output('Number of freq bins: '//itoaf(nfbin))
	  call rdhdd(tVis,'interval',interval,0.d0)
	  interval = 24*60 * interval
	  write(line,'(a,f6.1)')'Interpolation tolerance (minutes):',
     *				interval
	  call output(line)
	  call rdhda(tVis,'senmodel',senmodl,' ')
	  if(senmodl.eq.'GSV')then
	    call output('Theoretical variance estimates'//
     *			' scale with calibration gains')
	  else
	    call output('Theoretical variance estimates'//
     *			' are independent of calibration')
	  endif
	  if(ntau.ne.0)call output('Delay terms are present')
          maxgains = nsols*nants*2*maxfbin
          call memAlloc(pGains,maxgains,'c')
          call memAlloc(pTimes,nsols,'d')
          call  uvGnRead(tVis,memc(pGains),memc(pTimes),freq,ngains,
     *      nfeeds,ntau,nsols,nfbin,maxgains,nsols,maxfbin)
          do i=1,nfbin
            if (freq(i).gt.0)
     *        call output('Mean frequency for bin '//itoaf(i)//
     *        ': '//dtoaf(freq(i),1,4)//' GHz') 
          enddo
	  call output(' ')
          call memfree(pGains,maxgains,'c')
          call memfree(pTimes,nsols,'d')
	endif
c
c  Close up everything.
c
	call hclose(tVis)	
	end
c************************************************************************
	subroutine myBug(iostat,message)
c
	implicit none
	integer iostat
	character message*(*)
c
c  Give an error message, and bugger off.
c------------------------------------------------------------------------
	call bug('w',message)
	call bugno('f',iostat)
	end
