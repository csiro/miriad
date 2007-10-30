c************************************************************************
	program gpaver
	implicit none
c
c= GpAver -- Average (smooth) gain solutions.
c& rjs
c: calibration
c+
c	GpAver is a Miriad task to average (smooth) antenna gains.
c	Currently only boxcar averaging is supported.
c@ vis
c	The input visibility file, containing the gain file to average.
c@ interval
c	The time averaging interval, in minutes. The default is 10 minutes.
c@ options
c	  vector  Do vector averaging of the gain amplitudes and
c		  phases. This is the default.
c	  scalar  Do scalar averaging of the gain amplitudes and vector
c		  averaging of the gain phases
c--
c  History:
c    rjs     12oct93 Original version.
c    nebk    23nov93 Doc change to give rjs the grumps.
c    mchw    04jan95 write out new interval.
c    rjs     06jan95 Make interval the max of the old and new.
c    rjs     10jan95 Fixed a serious bug I introduced on 6jan.
c
c  Bugs and Shortcomings:
c    ? Perfect ?
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='GpAver: version 1.0 10-JAN-95')
	logical dovec
	double precision interval
	character vis*64
	integer ngains,nfeeds,ntau,nants,iostat
	integer tVis
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keyd('interval',interval,10.0d0)
	call GetOpt(dovec)
	call keyfin
	if(vis.eq.' ')call bug('f','An input file must be given')
	if(interval.le.0)call bug('f',
     *	   'The averaging interval must be a positive number')
	interval = interval / (24.d0*60.d0)
c
c  Open the visibility file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error opening '//vis)
c
c  Determine the number of feeds in the gain table.
c
	call rdhdi(tVis,'ngains',ngains,0)
	call rdhdi(tVis,'nfeeds',nfeeds,1)
	call rdhdi(tVis,'ntau',  ntau,  0)
	if(nfeeds.le.0.or.nfeeds.gt.2.or.mod(ngains,nfeeds+ntau).ne.0
     *	  .or.ntau.gt.1.or.ntau.lt.0)
     *	  call bug('f','Bad number of gains or feeds in '//vis)
	nants = ngains / (nfeeds + ntau)
c
c  Average the gains now.
c
	call AverGain(tVis,dovec,nfeeds,ntau,nants,interval)
c
c  Write out some history now.
c
	call hisopen(tVis,'append')
	call hiswrite(tVis,'GPAVER: Miriad '//version)
	call hisinput(tVis,'GPAVER')
	call hisclose(tVis)
c
c  Close up everything.
c
	call hclose(tVis)	
	end
c************************************************************************
	subroutine GetOpt(dovec)
c
	implicit none
	logical dovec
c
c  Get "Task Enrichment Parameters".
c
c  Output:
c    dovec	Do vecotr averaging.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=2)
	logical present(nopts)
	character opts(nopts)*8
	data opts/'vector  ','scalar  '/
c
	call options('options',opts,present,nopts)
	dovec = .not.present(2)
	if(present(1).and.present(2))call bug('f',
     *	    'You cannot specify both vector and scalar averaging')
c
	end
c************************************************************************
	subroutine AverBug(iostat,message)
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
c************************************************************************
	subroutine AverGain(tVis,dovec,nfeeds,ntau,nants,interval)
c
	implicit none
	logical dovec
	integer nfeeds,ntau,nants,tVis
	double precision interval
c
c  Read and write the gains, and call the averaging routine.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSOLS,MAXGAINS
	parameter(MAXSOLS=10000,MAXGAINS=3*MAXSOLS*MAXANT)
	complex Gains(MAXGAINS)
	double precision time(MAXSOLS),int1
	integer nsols,offset,pnt,i,tGains,iostat,nnsols,ngains
c
c  Externals.
c
	integer hsize
c
c  Open the gains table and read them all in.
c
	call haccess(tVis,tGains,'gains','read',iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error opening the gains')
	nsols = (hsize(tGains)-8)/(8*nants*(nfeeds+ntau)+8)
	if(nsols.gt.MAXSOLS.or.nsols*nants*(nfeeds+ntau).gt.MAXGAINS)
     *	  call bug('f','Gain table too big for me to handle')
c
	ngains = nants*(nfeeds+ntau)
	offset = 8
	pnt = 1
	do i=1,nsols
	  call hreadd(tGains,time(i),offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)call hreadr(tGains,Gains(pnt),offset,8*ngains,
     *								 iostat)
	  pnt = pnt + ngains
	  offset = offset + 8*ngains
	  if(iostat.ne.0)call Averbug(iostat,'Error reading gain table')
	enddo
c
c  Close up.
c
	call hdaccess(tGains,iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error closing gain table')
c
c  Do the averaging.
c
	call AverIt(time,Gains,nsols,nants,ntau,nfeeds,
     *	  interval,dovec,nnsols)
c
c  Now write out the new gain solutions.
c
	call wrhdi(tVis,'nsols',nnsols)
	call rdhdd(tVis,'interval',int1,0.d0)
	call wrhdd(tVis,'interval',max(int1,interval))
	call haccess(tVis,tGains,'gains','write',iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error reopening gain table')
c
	call hwritei(tGains,0,0,4,iostat)
	if(iostat.ne.0)
     *	  call AverBug(iostat,'Error writing gain table preamble')
c
	offset = 8
	pnt = 1
	do i=1,nnsols
	  call hwrited(tGains,time(i),offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)call hwriter(tGains,Gains(pnt),offset,8*ngains,
     *								 iostat)
	  pnt = pnt + ngains
	  offset = offset + 8*ngains
	  if(iostat.ne.0)call Averbug(iostat,'Error writing gain table')
	enddo
c
	call hdaccess(tGains,iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error reclosing gain table')
c
	end
c************************************************************************
	subroutine AverIt(time,Gains,nsols,nants,ntau,nfeeds,
     *	  interval,dovec,nnsols)
c
	implicit none
	integer nsols,nants,ntau,nfeeds,nnsols
	double precision time(nsols),interval
	logical dovec
	complex Gains((nfeeds+ntau)*nants,nsols)
c
c  Do the actual averaging.
c
c  Input:
c    nsols	Number of input solutions.
c    nants,ntau,nfeeds Number of antenna, delay/atten terms, feeds.
c    interval	Averaging interval.
c    dovec	Do vector averaging if true.
c  Input/Output:
c    time	Solution times.
c    Gains	The gains.
c  Output:
c    nnsols	The new number of solutions.
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex AvGains(3*MAXANT),t
	real AvAmps(3*MAXANT)
	integer Count(3*MAXANT),CountT,totgood,ngains,i,j,n
	double precision Tend,AvT
	logical tau
c
c  Check we have enough space in out internal buffers.
c
	ngains = nants*(nfeeds+ntau)
	if(ngains.gt.3*MAXANT)
     *	  call bug('f','Buffers too small, in AverIt')
c
c  Loop over all the gains, accumulating while we go.
c
	totgood = 0
	Tend = time(1) - 1
	do j=1,nsols
c
c  We have finished a solution interval. Update the gains.
c
	  if(time(j).gt.Tend)then
	    if(totgood.gt.0)then
	      nnsols = nnsols + 1
	      time(nnsols) = AvT / CountT
	      call NewG(nfeeds,ntau,nants,Gains(1,nnsols),
     *			AvGains,AvAmps,Count,dovec)
	    endif
c
c  Initialise the accumulates with a new interval.
c
	    n = 0
	    do i=1,ngains
	      t = Gains(i,j)
	      tau = ntau.eq.1.and.mod(j,nfeeds+ntau).eq.0
	      if(tau)then
		AvGains(i) = t
		Count(i) = 1
	      else if(abs(real(t))+abs(aimag(t)).gt.0)then
		t = 1/t
		AvGains(i) = t
		AvAmps(i) = abs(t)
		Count(i) = 1
		n = n + 1
	      else
		AvGains(i) = 0
		AvAmps(i) = 0
		Count(i) = 0
	      endif
	    enddo
	    AvT = time(j)
	    CountT = 1
	    totgood = n
	    if(totgood.gt.0)Tend = AvT + interval
c
c  Otherwise keep on accumulating.
c
	  else
	    call AccG(nfeeds,ntau,nants,Gains(1,j),
     *				AvGains,AvAmps,Count,n)
	    totgood = totgood + n
	    if(n.gt.0)then
	      AvT = AvT + time(j)
	      CountT = CountT + 1
	    endif
	  endif
c
	enddo
c
c  Finish with the last solution interval.
c
	if(totgood.gt.0)then
	  nnsols = nnsols + 1
	  time(nnsols) = AvT / CountT
	  call NewG(nfeeds,ntau,nants,Gains(1,nnsols),
     *			AvGains,AvAmps,Count,dovec)
	endif
c
	end
c************************************************************************
	subroutine AccG(nfeeds,ntau,nants,Gains,AvGains,AvAmps,
     *	  Count,n)
c
	implicit none
	integer nfeeds,ntau,nants,n
	integer Count(nants*(Nfeeds+ntau))
	complex Gains(nants*(nfeeds+ntau)),AvGains(nants*(nfeeds+ntau))
	real AvAmps(nants*(nfeeds+ntau))
c
c  Accumulate the gains.
c
c------------------------------------------------------------------------
	complex Sum12,alpha,t
	real Sum22
	integer ngains,i
	logical tau
c
	ngains = nants*(nfeeds+ntau)
c
c  Determine a phase factor to multiply the gains by to make them line
c  up, in phase, with the average gains, as best as we can.
c
	n = 0
	Sum22 = 0
	Sum12 = (0.,0.)
	do i=1,ngains
	  t = Gains(i)
	  tau = ntau.eq.1.and.mod(i,nfeeds+ntau).eq.0
	  if(.not.tau.and.Count(i).gt.0.and.
     *	    abs(real(t))+abs(aimag(t)).gt.0)then
	    t = 1/t
	    Sum12 = Sum12 + AvGains(i)*conjg(t)
	    Sum22 = Sum22 + real(t)**2 + aimag(t)**2
	    n = n + 1
	  endif
	enddo
c
	if(n.eq.0)return
	alpha = Sum12 / Sum22
	alpha = alpha / abs(alpha)
c
c  Now accumulate.
c
	n = 0
	do i=1,ngains
	  t = Gains(i)
	  tau = ntau.eq.1.and.mod(i,nfeeds+ntau).eq.0
	  if(tau)then
	    AvGains(i) = AvGains(i) + Gains(i)
	    Count(i) = Count(i) + 1
	  else if(abs(real(t))+abs(aimag(t)).gt.0)then
	    t = alpha/t
	    AvGains(i) = AvGains(i) + t
	    AvAmps(i) = AvAmps(i) + abs(t)
	    Count(i) = Count(i) + 1
	    n = n + 1
	  endif
	enddo
c
	end
c************************************************************************
	subroutine NewG(nfeeds,ntau,nants,Gains,
     *		AvGains,AvAmps,Count,dovec)
c
	implicit none
	integer nfeeds,ntau,nants
	integer Count(nants*(nfeeds+ntau))
	complex Gains(nants*(nfeeds+ntau)),AvGains(nants*(nfeeds+ntau))
	real AvAmps(nants*(nfeeds+ntau))
	logical dovec
c
c  Determine the average gains.
c------------------------------------------------------------------------
	integer ngains,i
	logical tau
c
	ngains = nants*(nfeeds+ntau)
c
	do i=1,ngains
	  tau = ntau.eq.1.and.mod(i,nfeeds+ntau).eq.0
	  if(Count(i).eq.0)then
	    Gains(i) = 0
	  else if(tau)then
	    Gains(i) = AvGains(i) / Count(i)
	  else if(dovec)then
	    Gains(i) = Count(i) / AvGains(i)
	  else
	    Gains(i) = Count(i) * abs(AvGains(i)) / AvAmps(i)
     *						  / AvGains(i)
	  endif
	enddo
c
	end
