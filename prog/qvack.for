c************************************************************************
	program qvack
	implicit none
c
c= qvack -- Flag after change to observing setup
c& rjs
c: calibration
c+
c	Qvack is a Miriad task to flag data for a number of integrations
c	near an observing setup change. For example, this is used to
c	flag data that is suspect because the instrument had not settled
c	correctly after the change in the setup. The data are assumed to
c	be in time order.
c@ vis
c	Input visibility data file. No default.
c@ select
c	Standard uv data selection. Default is to select all data.
c@ interval
c	The time interval, in minutes, after a change in the setup, for 
c	which the data are flagged. Default is 0.1, which flags for 
c	the first 6 seconds after a switch. A negative value flags
c	data before the switch.
c@ force
c	The time interval, in minutes, which indicates the amount of time 
c	that should ellapse before forcing a (virtual) setup change.
c	Thus you can QVACK on data that has distinct scans but no actual 
c	change in observing setup.  Note that this means the first 
c	INTERVAL amount of data is always flagged. The MODE option is 
c	not needed, nor is it used if you set FORCE
c	Defaults to no forced setup change.
c@ mode
c	This determines what changes in setup cause flagging. Several
c	values can be given, separated by commas, Minimum match is used.
c
c	Possible values are:
c	  "frequency"    A change in the frequency setup. Do not use this
c	                 option if Doppler tracking is used.
c	  "source"       A change in the source observed.
c	  "mosaic"       A change in the observing centre of a mosaiced
c	                 observation.
c--
c  History:
c    rjs   1sep92 Original version.
c    nebk  1dec95 Add keyword FORCE.  Will RJS talk to me again ?
c    rjs  17dec97 Allow "interval" to be negative.
c    rjs  06jan98 Make the above change work!
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	integer MAXSELS
	parameter(version='Qvack: version 1.0 06-Jan-98')
	parameter(MAXSELS=1024)
c
	character vis*64
	double precision interval,porg(4),pcopy(4),t1,t2,tforce,t0
	real sels(MAXSELS)
	complex data(MAXCHAN)
	logical flags(MAXCHAN),cflags(MAXCHAN)
	integer tOrg,tCopy,vhand,nchan,n,nrec,ncorr,i,cnchan
	logical dofreq,dosrc,domos,first,doflag
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
	call keyd('force',tforce,0.0d0)
	if(interval.eq.0)call bug('f','Bad value for keyword interval')
	if(tforce.lt.0)call bug('f','Bad value for keyword force')
	interval = interval / (60.0*24.0)
	tforce = tforce / (60.0*24.0)
	call SelInput('select',sels,MAXSELS)
	call GetMode(tforce,dofreq,dosrc,domos)
	call keyfin
c
c  Open the data-set and initialise.
c
	call uvopen(tOrg,vis,'old')
	call SelApply(tOrg,sels,.true.)
	call uvopen(tCopy,vis,'old')
	call SelApply(tCopy,sels,.true.)
	call uvread(tCopy,pCopy,data,cflags,maxchan,cnchan)
c
c  Determine which variables to keep track of.
c
	call uvvarini(tOrg,vhand)
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
	call uvread(tOrg,porg,data,flags,maxchan,nchan)
        t0 = porg(3)
        first = .true.
	doFlag = .true.
c
	dowhile(nchan.gt.0)
          if(tforce.gt.0)then
            if(first.or.porg(3).gt.t0+tforce)then
	      t0 = porg(3) 
              first = .false.
	      doFlag = .true.
            end if
          else if(uvvarupd(vhand))then
	    doFlag = .true.
          end if
c
c  Flag as necessary.
c
	  if(doFlag)then
	    t1 = min(porg(3),porg(3)+interval)
	    t2 = max(porg(3),porg(3)+interval)
	    n = 0
	    dowhile(pCopy(3).lt.t1.and.cnchan.gt.0)
	      call uvread(tCopy,pCopy,data,cflags,maxchan,cnchan)
	    enddo
	    dowhile(pCopy(3).le.t2.and.cnchan.gt.0)
	      do i=1,cnchan
	        if(cflags(i))n = n + 1
	        cflags(i) = .false.
	      enddo
	      if(n.gt.0)nrec = nrec + 1
	      ncorr = ncorr + n
	      call uvflgwr(tCopy,cflags)
	      call uvread(tCopy,pCopy,data,cflags,maxchan,cnchan)
	    enddo
	    doFlag = .false.
	  endif
c
	  call uvread(tOrg,porg,data,flags,maxchan,nchan)
	enddo
	call uvclose(tCopy)
c
c  Wake the user up with useless information.
c
	call output('Records flagged:      '//itoaf(nrec))
	call output('Correlations flagged: '//itoaf(ncorr))
c
c  Write some history.
c
	call HisOpen(tOrg,'append')
	call HisWrite(tOrg,'QVACK: Miriad '//version)
	call HisInput(tOrg,'QVACK')
	call HisClose(tOrg)
c
c  Close up and done.
c
	call uvclose(tOrg)
	end
c************************************************************************
	subroutine GetMode(tforce,dofreq,dosrc,domos)
c
	implicit none
	double precision tforce
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
        if (tforce.le.0.0d0) then
  	  if(.not.(dofreq.or.dosrc.or.domos))
     *	    call bug('f','A flagging mode must be given')
        end if
	end
