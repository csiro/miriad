	program TPGAINS
	implicit none
c
c= TPGAINS - Estimate antenna gains from total power measurements.
c& mchw
c: tpower, antenna gains
c+
c	TPGAINS is a Miriad task to estimate antenna gains due to
c	atmospheric phase fluctuations correlated with total power 
c	fluctuations. Antenna phase corrections are estimated from the
c	total power measurments and written into the gains item.
c@ vis
c	Name of input visibility data file. No default.
c@ tgain
c	The total power is modeled to be correlated with the atmospheric
c	phase noise. tgain gives the total power in units of Kelvin/radian.
c	Default value for millimeter wavelengths is tgain=0.5 K/radian.
c@ refant
c	The gain of this antenna is set to cmplx(1.,0.).
c		Default is antenna 1.
c@ out
c       Name of output calibration file. The default is to write the gain
c       corrections into the input visibility file.
c--
c  History:
c    11dec90 mchw
c    25feb91 mjs  Changed references of itoa to itoaf.
c    04aug91 mjs  Replaced local maxants to use maxdim.h value MAXANT
c------------------------------------------------------------------------
	character version*(*),vis*80,out*80
	parameter(version='(version 1.0 11-dec-90)')
	include	'maxdim.h'
	integer length,item
	complex gains(MAXANT)
	real tpower(MAXANT),tgain
	double precision time,time0,interval
	integer refant,tvis,tgains,nants,nsols,i,iostat,offset,header(2)
	character obstype*32,type*1
	logical updated
c
c  Externals.
c
	character itoaf*4
	integer uvscan
	complex expi
c
c  Get input parameters.
c
	call output('TPGAINS '//version)
	call keyini
	call keyf('vis',vis,' ')
	call keyi('refant',refant,1)
	call keyr('tgain',tgain,0.5)
	call keya('out',out,' ')
	call keyfin
c
c  Open the uvdata file.
c
	call uvopen(tvis,vis,'old')
	if(vis.eq.' ') call bug('f','Input visibility file is missing')
	call rdhda(tvis,'obstype',obstype,'crosscorrelation')
	if(obstype(1:5).ne.'cross')
     *	  call bug('f','The vis file is not cross correlation data')
c
c  Check that nants and tpower are present.
c
	call uvprobvr(tvis,'tpower',type,length,updated)
	if(type.ne.'r') call bug('f','tpower is not in uvdata')
	call uvprobvr(tvis,'nants',type,length,updated)
	if(type.ne.'i') call bug('f','nants is not in uvdata')
c
c  Open the output file to contain the gain solutions. Start History.
c
	if(out.eq.' ')then
	  tgains = tvis
	  call HisOpen(tgains,'append')
	else
	  call hopen(tgains,out,'new',iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error opening output gains file '//out)
	    call bugno('f',iostat)
	  endif
	  call HisOpen(tgains,'write')
	endif
	call HisWrite(tgains,'TPGAINS: Miriad TPGains '//version)
	call HisInput(tgains,'TPGAINS')
c
c  Start the gains file.
c
	call haccess(tgains,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	header(1) = 0
	header(2) = 0
	offset = 0
	call hwritei(item,header,offset,8,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	offset = 8
c
c  Scan the uvdata looking for tpower and write the gains.
c
	nsols = 0
	tgain = 1./tgain
	do while(uvscan(tvis,'tpower').eq.0)
	  call uvrdvrd(tvis,'time',time,0.d0)
	  if(nsols.eq.0) then
	    time0 = time
  	    call uvrdvri(tvis,'nants',nants,0)
	    if(nants.eq.0)call bug('f','nants is zero')
	  endif
	  call uvgetvrr(tvis,'tpower',tpower,nants)
	  do i=1,nants
	    gains(i) = expi(tgain*(tpower(i)-tpower(refant)))
	  enddo
	  call hwrited(item,time,offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)call hwriter(item,gains,offset,8*nants,iostat)
	  if(iostat.ne.0)then
	    call bug('w','I/O error while writing to gains item')
	    call bugno('f',iostat)
	  endif
	  offset = offset + 8*nants
	  nsols = nsols + 1
	enddo
c
c  Write some header information for the gains file.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing output gains item')
	  call bugno('f',iostat)
	endif
	if(nsols.gt.1) then
	  interval=(time-time0)/float(nsols-1)
	  call wrhdd(tgains,'interval',interval)
	  call wrhdi(tgains,'ngains',nants)
	  call wrhdi(tgains,'nsols',nsols)
	  call output('Number of solution intervals: '//itoaf(nSols))
	else
	  call bug('f','too few gains')
	endif
c
c  Close up.
c
	call HisClose(tgains)
	call uvclose(tvis)
	if(out.ne.' ') call hclose(tgains,iostat)
	end
c************************************************************************
