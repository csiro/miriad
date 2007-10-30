c************************************************************************
	program qvack
	implicit none
c
c= qvack -- Flag after change to observing setup
c& rjs
c: calibration
c+
c	Qvack is a Miriad task to flag data for a number of integrations
c	after the observing setup has been changed. This is used to
c	flag data that is suspect because the instrument had not settled
c	correctly after the change in the setup. The data are assumed to
c	be in time order.
c@ vis
c	Input visibility data file. No default.
c@ select
c	Standard uv data selection. Default is to select all data.
c@ interval
c	The time interval, in minutes, after a change in the setup, for
c	which the data are flagged. Default is 0.1, which flags for the
c	first 6 seconds after a switch.
c@ mode
c	This determines what changes in setup cause flagging. Several
c	values can be given, separated by commas, Minimum match is used.
c	Possible values are:
c	  "frequency"    A change in the frequency setup. Do not use this
c	                 option if Doppler tracking is used.
c	  "source"       A change in the source observed.
c	  "mosaic"       A change in the observing centre of a mosaiced
c	                 observation.
c--
c  History:
c    rjs   1sep92 Original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	integer MAXSELS
	parameter(version='Qvack: version 1.0 1-Sept-92')
	parameter(MAXSELS=1024)
c
	character vis*64
	double precision interval,preamble(4),time
	real sels(MAXSELS)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	integer tno,vhand,nchan,n,nrec,ncorr,i
	logical dofreq,dosrc,domos
c
c  Externals.
c
	logical uvvarupd
	character itoaf*8
c
c  Read the inputs.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input vis file must be given')
	call keyd('interval',interval,0.1d0)
	if(interval.le.0)call bug('f','Bad value for interval')
	interval = interval / (60.0*24.0)
	call SelInput('select',sels,MAXSELS)
	call GetMode(dofreq,dosrc,domos)
	call keyfin
c
c  Open the data-set and initialise.
c
	call uvopen(tno,vis,'old')
	call SelApply(tno,sels,.true.)
c
c  Determine which variables to keep track of.
c
	call uvvarini(tno,vhand)
	if(dofreq)then
	  call uvvarset(vhand,'sfreq')
	  call uvvarset(vhand,'sdf')
	  call uvvarset(vhand,'nschan')
	  call uvvarset(vhand,'wfreq')
	  call uvvarset(vhand,'wwidth')
	endif
	if(dosrc)call uvvarset(vhand,'source')
	if(domos)then
	  call uvvarset(vhand,'dra')
	  call uvvarset(vhand,'ddec')
	endif
c
c  Read all the relevant data, flagging as we go.
c
	nrec = 0
	ncorr = 0
	call uvread(tno,preamble,data,flags,maxchan,nchan)
	time = preamble(3) - interval
	dowhile(nchan.gt.0)
	  if(uvvarupd(vhand))time = preamble(3) + interval
	  if(preamble(3).lt.time)then
	    n = 0
	    do i=1,nchan
	      if(flags(i))n = n + 1
	      flags(i) = .false.
	    enddo
	    if(n.gt.0)nrec = nrec + 1
	    ncorr = ncorr + n
	    call uvflgwr(tno,flags)
	  endif
	  call uvread(tno,preamble,data,flags,maxchan,nchan)
	enddo
c
c  Wake the user up with useless information.
c
	call output('Records flagged:      '//itoaf(nrec))
	call output('Correlations flagged: '//itoaf(ncorr))
c
c  Write some history.
c
	call HisOpen(tno,'append')
	call HisWrite(tno,'QVACK: Miriad '//version)
	call HisInput(tno,'QVACK')
	call HisClose(tno)
c
c  Close up and done.
c
	call uvclose(tno)
	end
c************************************************************************
	subroutine GetMode(dofreq,dosrc,domos)
c
	implicit none
	logical dofreq,dosrc,domos
c
c  Get processing modes.
c
c  Output:
c    dofreq
c    dosrc
c    domos
c------------------------------------------------------------------------
	integer nmodes
	parameter(nmodes=3)
	logical present(nmodes)
	character modes(nmodes)*12
	data modes/'frequency   ','source      ','mosaic      '/
c
c  Get the processing modes.
c
	call options('mode',modes,present,nmodes)
	dofreq = present(1)
	dosrc  = present(2)
	domos  = present(3)
	if(.not.(dofreq.or.dosrc.or.domos))
     *	  call bug('f','A flagging mode must be given')
	end
