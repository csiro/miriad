c************************************************************************
	program gperror
	implicit none
c
c  This adds a gain table, which effectively adds gain errors to the data.
c
c= GPERROR - Add dummy gains to a visibility file.
c& rjs
c: uv analysis
c+
c	GPERROR is a Miriad task which adds a gains table to a uv dataset.
c	This gain table consists of random phase and amplitude errors. 
c       GPERROR's use is in simulating such phase and amplitude errors in 
c       the data. See also UVGEN's GNOISE and PNOISE parameters.
c@ vis
c	The input visibility file. This is used to determine the number
c	of antennas and the time range. 
c	No default.
c@ interval
c	Averaging time interval, over which variations will occur, 
c	in minutes. 
c	Default is 5.
c@ pnoise
c	Anntena based rms phase noise, in degrees. 
c	Default is 0.
c@ gnoise
c       Antenna based rms gain noise, given as a percentage. 
c	Default is 0.
c--
c  History:
c    rjs  20nov90 Original version.
c    pjt   1mar91 added one '+' sign to make doc work
c    rjs  19jul91 Changed name.
c    pjt  18may93 added gnoise parameter, use mirconst.h for PI
c    rjs  05feb01 Use basant routine.
c
c  Bugs/Shortcomings:
c    * Needs a gnoise and leakage parameters as well.
c    * Write gain table for two feeds.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='GpError: version 1.0 05-Feb-01')
	include 'maxdim.h'
	include 'mirconst.h'
c
	character vis*64
	integer lu,item,offset,nants,iostat,i,n,header(2),i1,i2
	real interval,prms,arms,baseline
	double precision time,tmin,tmax
	real theta(maxant)
	complex g(maxant)
c
c  Externals.
c
	integer uvscan
	logical hdprsnt
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keyr('interval',interval,5.0)
	call keyr('pnoise',prms,0.0)
	call keyr('gnoise',arms,0.0)
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ') call bug('f',
     *	  'Input visibility file must be given (vis=)')
	interval = interval / (24.*60.)
	if(interval.le.0) call bug('f',
     *	  'The interval must be positive valued (interval=)')
	if(prms.lt.0) call bug('f',
     *	  'The antenna phase noise must be positive (pnoise=)')
	if(arms.lt.0) call bug('f',
     *	  'The antenna amplitude noise must be positive (gnoise=)')
c
c  Check if a gains table is already present.
c
	call uvopen(lu,vis,'old')
	if(hdprsnt(lu,'gains'))
     *	  call bug('w','Overwriting existing gains table')
c
c  First get the initial values, then scan through the file, 
c  determining the number of antennas and the time range.
c
	if(uvscan(lu,'baseline').ne.0)
     *	  call bug('f','Failed to find the baseline uv variable')
	call uvrdvrr(lu,'baseline',baseline,0.)
	call basant(dble(baseline),i1,i2)
	nants = max(i1,i2)
	call uvrdvrd(lu,'time',tmin,0.d0)
	tmax = tmin
c
	dowhile(uvscan(lu,'baseline').eq.0)
	  call uvrdvrr(lu,'baseline',baseline,0.)
	  call basant(dble(baseline),i1,i2)
	  nants = max(nants,i1,i2)
	  call uvrdvrd(lu,'time',time,0.d0)
	  tmax = max(tmax,time)
	  tmin = min(tmin,time)
	enddo
c
c  We know the time and baseline ranges we are interested in now.
c
	time = int(tmin) + 0.5
	n = nint((tmax-time)/interval) - nint((tmin-time)/interval) + 1
	time = interval*nint((tmin-time)/interval) + time
c
c  Write ancillary info.
c
	call wrhdd(lu,'interval',dble(interval))
	call wrhdi(lu,'ngains',nants)
	call wrhdi(lu,'nsols',n)
c
c  Write the dummy gains.
c
	call haccess(lu,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	header(1) = 0
	header(2) = 0
	offset = 0
	call hwritei(item,header,offset,8,iostat)
	offset = offset + 8
c
	prms = PI/180*prms
	arms = arms / 100
	dowhile(n.gt.0.and.iostat.eq.0)
	  call hwrited(item,time,offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)then
	    call gaus(theta,nants)
	    do i=1,nants
	      g(i) = cmplx(cos(prms*theta(i)),sin(prms*theta(i)))
	    enddo
            if (arms.gt.0) then
              call gaus(theta,nants)
              do i=1,nants
                g(i) = g(i) * (1.0 + arms*theta(i))
              enddo
            endif
	    call hwriter(item,g,offset,8*nants,iostat)
	  endif
	  offset = offset + 8*nants
	  n = n - 1
	  time = time + interval
	enddo
	if(iostat.ne.0)then
	  call bug('w','Error writing to gains file')
	  call bugno('f',iostat)
	endif
	call hdaccess(item,iostat)
c
c  Write some history information.
c
	call hisopen(lu,'append')
	call hiswrite(lu,'GPERROR: Miriad '//version)
	call hisinput(lu,'GPERROR')
	call hisclose(lu)
c
	call uvclose(lu)
	end
