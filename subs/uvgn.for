c************************************************************************
c
c  A set of routines, called by uvdat.for, which apply calibration
c  corrections to the data.
c
c  History:
c    04aug92 rjs  Derived from some code in uvdat, plus lots of new code.
c    28aug92 rjs  Cosmetic error message change.
c    22oct92 rjs  Another cosmetic error message change.
c     2apr93 rjs  Corrected a bug checking whether the gains were good.
c    25jun93 rjs  Changed gain interpolation method to make any errors
c		  introduced to be closing ones.
c     5aug93 rjs  Changed definition of "attenuation" parameter.
c    19jul94 rjs  Set bad gains to 0 in uvgnpsma (previously the gain
c		  flag was marked as bad, but the gain was not initialised.
c
c************************************************************************
	subroutine uvGnIni(tno1,dogains1,dopass1)
	implicit none
c
	integer tno1
	logical dogains1,dopass1
c
c  This initialises the "gains" routines, and readies it to start reading
c  data.
c------------------------------------------------------------------------
	include 'uvgn.h'
c
c  Externals.
c
	logical hdprsnt
c
	tno = tno1
c
c  Work out what to do.
c
	dogains = dogains1
	dopass = dopass1.and.hdprsnt(tno,'bandpass')
	if(dopass1.and..not.dopass)then
	  docgains = hdprsnt(tno,'cgains')
	  dowgains = hdprsnt(tno,'wgains')
	endif
c
c  Initialise everything we are interested in.
c
	if(dogains.or.dopass)then
	  call rdhdi(tno,'ngains',ngains,0)
	  call rdhdi(tno,'nfeeds',nfeeds,1)
	  call rdhdi(tno,'ntau',  ntau,  0)
	  nants = ngains / (ntau + nfeeds)
	  dotau = dogains.and.ntau.gt.0
	  if(ngains.le.0) call bug('f',
     *	    'Number of gains is missing or bad, in uvGnIni')
	  if(nfeeds.le.0.or.nfeeds.gt.MAXFEEDS) call bug('f',
     *	    'Number of feeds is bad')
	  if(ntau.lt.0.or.ntau.gt.1)call bug('f',
     *	    'Number of delay terms is bad')
	  if(nants*(ntau + nfeeds).ne.ngains) call bug('f',
     *	    'Bad number of gains or feeds')
	  if(nants.gt.MAXANT) call bug('f',
     *	    'Too many antennae for me to handle, in uvGnIni')
	else
	  nants = MAXANT
	  ngains = MAXANT
	  nfeeds = 1
	  ntau = 0
	  dotau = .false.
	endif
c
c  Initialise the gains stuff.
c
	if(dogains)call uvGnGnIn
	if(docgains)call uvGnInic
	if(dowgains)call uvGnIniw
	if(dopass.or.dotau)call uvGnPsIn
	end
c************************************************************************
	subroutine uvGnGnIn
c
	implicit none
c------------------------------------------------------------------------
	include 'uvgn.h'
	integer iostat
c
	call rdhdi(tno,'nsols', nsols,0)
	call rdhdd(tno,'interval',dtime,0.d0)
	if(nsols.le.0) call bug('f',
     *	    'Number of gain solutions is missing or bad, in uvGnIni')
	if(dtime.le.0) call bug('f',
     *	    'The time interval increment is missing or bad, in uvGnIni')
c
	call haccess(tno,gitem,'gains','read',iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'accessing gains table')
c
c  Read in the first gain solution.
c
	t1 = 1
	t2 = 2
c
	solno(t2) = 1
	call hreadd(gitem,timetab(t2),8,8,iostat)
	if(iostat.ne.0)
     *	  call uvGnBug(iostat,'reading first time')
	call uvGnGet(gitem,solno(t2),Gains(1,t2),gflag(1,t2),
     *	  nsols,ngains)
c
	solno(t1) = 0
	timetab(t1) = timetab(t2) - 1e6*dtime
	call uvGnGet(gitem,solno(t1),Gains(1,t1),gflag(1,t1),
     *	  nsols,ngains)
c
	end
c************************************************************************
	subroutine uvGnPsIn
c
	implicit none
c
c  Initialise ready to perform bandpass and delay correction on the
c  correlation data.
c
c------------------------------------------------------------------------
	include 'uvgn.h'
c
c  Set the dynamic memory pointers to NULL.
c
	nTab    = 0
	nDat(1) = 0
	nDat(2) = 0
	nFreq(1)= 0
	nFreq(2)= 0
c
	aver = .false.
c
c  Make sure we keep track of variables that affect the frequency of the
c  channels.
c
	call uvvarini(tno,vwide)
	call uvvarset(vwide,'wfreq')
	call uvvarset(vwide,'wwidth')
c
	call uvvarini(tno,vline)
	call uvvarset(vline,'sfreq')
	call uvvarset(vline,'sdf')
	call uvvarset(vline,'nschan')
c
c  If the are to do bandpass correction, read the bandpass tables.
c
	if(dopass)call uvGnPsLd(tno,MAXSPECT,nfeeds*nants,nchan,
     *	  nspect,sfreq,sdf,nschan,pTab,nTab)
c
c  Get the reference frequency, in case we are doing the delay correction.
c
	call rdhdd(tno,'freq0',freq0,0.d0)
c
	end
c************************************************************************
	subroutine uvGnFin()
c
	implicit none
c
c  Close up the bandpass and delay routines. This does not do much!
c------------------------------------------------------------------------
	integer iostat
	include 'uvgn.h'
c
c  Close the gains file, if needed.
c
	if(dogains)then
	  call hdaccess(gitem,iostat)
	  if(iostat.ne.0)call uvGnBug(iostat,'closing gains table')
	endif
c
c  Free up all the memory that may have been allocated for the antenna
c  based bandpass calibration.
c
	if(nTab.ne.0) call MemFree(pTab, nTab, 'c')
	if(nDat(1).ne.0)then
	  call MemFree(pDat(1),nDat(1),'c')
	  call MemFree(pFlags(1),nDat(1),'l')
	endif
	if(nDat(2).ne.0)then
	  call MemFree(pDat(2),nDat(2),'c')
	  call MemFree(pFlags(2),nDat(2),'l')
	endif
	if(nFreq(1).ne.0)call MemFree(pFreq(1),nFreq(1),'d')
	if(nFreq(2).ne.0)call MemFree(pFreq(2),nFreq(2),'d')
c
c  Free up all the memory that may have been allocated for the baseline
c  based bandpass calibration.
c
	if(docgains)call MemFree(pCgains,ncgains*ncbase,'c')
	if(dowgains)call MemFree(pWgains,nwgains*nwbase,'c')
c
c  Clear out everything.
c
	dopass = .false.
	dogains = .false.
	docgains = .false.
	dowgains = .false.
	dotau = .false.
c
	end
c************************************************************************
	subroutine uvGnFac(time,baseline,pol,dowide,data,flags,nread)
c
	implicit none
	integer nread
	complex data(nread)
	logical flags(nread),dowide
	double precision time
	real baseline
	integer pol
c
c  Determine the gain factor for a particular visibility.
c
c  Input:
c    pol	Polarization. If 0, and the info is needed, this is
c		determined from the data file
c    time	Julian date of the data.
c    baseline	Baseline number.
c    dowide	True if the data was retrieved using uvwread (rather than
c		uvread).
c  Input/Output:
c    data	The correlation data. On input this is uncalibrated. On
c		output, it is gain/bandpass calibrated.
c    flags	Data flags. If the antenna gains were bad for some reason,
c		the data are flagged as bad.
c------------------------------------------------------------------------
	include 'uvgn.h'
	logical t1valid,t2valid,t1good,t2good,flag
	integer i,i1,i2,ant1,ant2,s,itemp,n,offset,iostat,p,gpant
	double precision dtemp
	real mag,epsi
	complex gain,tau1,tau2,tau,g1,g2,g
	integer f1(4),f2(4)
	save f1,f2
	data f1/0,1,0,1/
	data f2/0,1,1,0/
c
c  Assume that we fail!
c
	flag = .false.
c
c  Determine the polarisation type index.
c
	p = 1
	if(nfeeds.gt.1)then
	  p = pol
	  if(p.eq.0) call uvrdvri(tno,'pol',p,0)
	  if(p.ge.0) goto 100
	  if(p.le.-5)then
	    p = -4 - p
	  else
	    p = - p
	  endif
	  if(p.gt.4)goto 100
	endif
c
c  Determine the gain indices based on antenna numbers.
c
	ant2 = nint(baseline)
	ant1 = ant2 / 256
	ant2 = ant2 - 256 * ant1
	if(ant1.lt.1.or.ant1.gt.nants.or.ant2.lt.1.or.ant2.gt.nants)
     *	  goto 100
c
c  If we are applying the gains, get the solution.
c  Check if (t1,t2) bounds the solution. If not, find t1,t2 which do. This
c  uses a binary step through the gains file.
c
	tau = 0
	if(dogains)then
	  if((time-timetab(t1))*(time-timetab(t2)).gt.0)then
	    t1valid = .true.
	    t2valid = .true.
	    n = 1
	    dowhile(timetab(t2).lt.time.and.solno(t2).lt.nsols)
	      itemp = t2
	      t2 = t1
	      t1 = itemp
	      t1valid = t2valid
	      t2valid = .false.
	      solno(t2) = min(solno(t1)+n,nsols)
	      n = n + n
	      offset = 8*(ngains+1)*(solno(t2)-1) + 8
	      call hreadd(gitem,timetab(t2),offset,8,iostat)
	      if(iostat.ne.0)call uvGnBug(iostat,'reading gains time')
	    enddo
	    dowhile(timetab(t1).gt.time.and.solno(t1).gt.1)
	      itemp = t2
	      t2 = t1
	      t1 = itemp
	      t2valid = t1valid
	      t1valid = .false.
	      solno(t1) = max(solno(t2)-n,1)
	      n = n + n
	      offset = 8*(ngains+1)*(solno(t1)-1) + 8
	      call hreadd(gitem,timetab(t1),offset,8,iostat)
	      if(iostat.ne.0)call uvGnBug(iostat,'reading gains time')
	    enddo
c
c  Check for the case that we have fallen off the end of the gains table.
c
	    if(time.gt.timetab(t2))then
	      itemp = t2
	      t2 = t1
	      t1 = itemp
	      t1valid = t2valid
	      t2valid = .false.
	      solno(t2) = nsols + 1
	      timetab(t2) = time + 1e6*dtime
	    else if(time.lt.timetab(t1))then
	      itemp = t2
	      t2 = t1
	      t1 = itemp
	      t2valid = t1valid
	      t1valid = .false.
	      solno(t1) = 0
	      timetab(t1) = time - 1e6*dtime
	    endif
c
c  We have solution intervals which bound "time", but they may not
c  be adjacent solutions. Home in on an adjacent solution pair. We use
c  a binary bisection technique.
c
	    dowhile(solno(t1)+1.ne.solno(t2))
	      s = (solno(t1)+solno(t2))/2
	      offset = 8*(ngains+1)*(s-1) + 8
	      call hreadd(gitem,dtemp,offset,8,iostat)
	      if(iostat.ne.0)call uvGnBug(iostat,'reading gains time')
	      if(dtemp.gt.time)then
	        solno(t2) = s
	        timetab(t2) = dtemp
	        t2valid = .false.
	      else
	        solno(t1) = s
	        timetab(t1) = dtemp
	        t1valid = .false.
	      endif
	    enddo
c
c  Read in the gains if necessary.
c
	    if(.not.t1valid) call uvGnGet(gitem,solno(t1),
     *	      Gains(1,t1),gflag(1,t1),nsols,ngains)
	    if(.not.t2valid) call uvGnGet(gitem,solno(t2),
     *	      Gains(1,t2),gflag(1,t2),nsols,ngains)
	  endif
c
c  Determine the indices of the gains.
c
	  gpant = nfeeds + ntau
	  i1 = gpant*(ant1-1) + 1
	  i2 = gpant*(ant2-1) + 1
c
c  Determine which of the gains are good.
c
	  t1good = abs(time-timetab(t1)).lt.dtime.and.
     *	    gflag(i1+f1(p),t1).and.gflag(i2+f2(p),t1)
	  t2good = abs(time-timetab(t2)).lt.dtime.and.
     *	    gflag(i1+f1(p),t2).and.gflag(i2+f2(p),t2)
c
c  Now determine the baseline gain. If both t1 and t2 are good, do logarithmic
c  interpolation. Otherwise use the good gain, if any.
c  Failing this, give up!
c
	  flag = .true.
	  if(t1good.and.t2good)then
	    epsi = (timetab(t2)-time)/(timetab(t2)-timetab(t1))
c
	    g1 = gains(i1+f1(p),t1)
	    g2 = gains(i1+f1(p),t2)
	    g = g1/g2
	    mag = abs(g)
	    gain = g2 * (1 + (mag-1)*epsi) * (g/mag) ** epsi
c
	    g1 = gains(i2+f2(p),t1)
	    g2 = gains(i2+f2(p),t2)
	    g = g1/g2
	    mag = abs(g)
	    gain = gain * 
     *		   conjg(g2 * (1 + (mag-1)*epsi) * (g/mag) ** epsi)
c
	    if(ntau.eq.1)then
	      tau1 = gains(i1+nfeeds,t1) + conjg(gains(i2+nfeeds,t1))
	      tau2 = gains(i1+nfeeds,t2) + conjg(gains(i2+nfeeds,t2))
	      tau = tau2 - epsi * (tau2 - tau1)
	    endif
	  else if(t1good)then
	    gain = gains(i1+f1(p),t1)*conjg(gains(i2+f2(p),t1))
	    if(ntau.eq.1)
     *		tau = gains(i1+nfeeds,t1) + conjg(gains(i2+nfeeds,t1))
	  else if(t2good)then
	    gain = gains(i1+f1(p),t2)*conjg(gains(i2+f2(p),t2))
	    if(ntau.eq.1)
     *		tau = gains(i1+nfeeds,t2) + conjg(gains(i2+nfeeds,t2))
	  else
	    flag = .false.
	  endif
	else
	  flag = .true.
	endif
c
c  Apply the gain to the data.
c
  100	continue
	if(flag)then
	  if(dogains)then
	    do i=1,nread
	      data(i) = gain * data(i)
	    enddo
	  endif
	  if(dopass.or.dotau)
     *	    call uvGnPsAp(dowide,ant1,ant2,p,tau,data,flags,nread)
	  if(docgains.or.dowgains)
     *	    call uvGnCWAp(dowide,ant1,ant2,data,flags,nread)
	else
	  do i=1,nread
	    flags(i) = .false.
	  enddo
	endif
c	  
	end
c************************************************************************
	subroutine uvGnCWAp(dowide,ant1,ant2,data,flags,nread)
c
	implicit none
	integer ant1,ant2,nread
	logical dowide,flags(nread)
	complex data(nread)
c
c  Apply either CGains or WGains as is appropriate.
c
c  Input:
c    dowide
c    ant1,ant2
c    nread
c  Input/Output:
c    data
c    flags
c------------------------------------------------------------------------
	include 'uvgn.h'
	integer LINE,WIDE,VELO
	parameter(LINE=1,WIDE=2,VELO=3)
	integer TYPE,COUNT,START,WIDTH,STEP
	parameter(TYPE=1,COUNT=2,START=3,WIDTH=4,STEP=5)
	integer ltype,i,j,bl
	logical willcg,ok
	double precision dat(6)
c
c  Dynamic memory bull.
c
	complex cref(MAXBUF/2)
	common cref
c
c  Determine whether its cgains or wgains that we are to apply.
c
	if(dowide)then
	  willcg = .false.
	else
	  call uvinfo(tno,'line',dat)
	  ltype  = nint(dat(TYPE))
	  willcg = ltype.eq.LINE
	  if(ltype.eq.VELO)call bug('f',
     *	    'Cannot apply bandpass correction with velocity linetype')
	endif
c
c  Determine the baseline number.
c
	ok = ant1.ne.ant2
	if(ant1.lt.ant2)then
	  bl = ((ant2-2)*(ant2-1))/2 + ant1
	else
	  bl = ((ant1-2)*(ant1-1))/2 + ant2
	endif
c
c  Get the appropriate table.
c
	if(willcg.and.docgains)then
	  j = pCgains + ncgains*(bl-1)
	  ok = ok.and.bl.le.ncbase.and.ncgains.eq.nread
	else if(.not.willcg.and.dowgains)then
	  j = pWgains + nwgains*(bl-1)
	  ok = ok.and.bl.le.nwbase.and.nwgains.eq.nread
	else
	  ok = .false.
	endif
c
c Check it all makes sense.
c
	if(.not.ok)then
	  call output('Error applying baseline gains')
	  do i=1,nread
	    flags(i) = .false.
	  enddo
	else if(ant1.le.ant2)then
	  do i=1,nread
	    data(i) = data(i) * cref(j)
	    j = j + 1
	  enddo
	else
	  do i=1,nread
	    data(i) = data(i) * conjg(cref(j))
	    j = j + 1
	  enddo
	endif
	end
c************************************************************************
	subroutine uvGnBug(iostat,text)
c
	implicit none
	integer iostat
	character text*(*)
c
c  This gives two error messages, then aborts. The first message is formed
c  from the text passed in, the second from the i/o status indicator.
c
c  Input:
c    iostat	I/O status indicator, returned by an unsuccessful i/o
c		operation.
c    text	Some text saying what was being done when the i/o error
c		occurred.
c------------------------------------------------------------------------
	character umsg*64
c
	umsg = 'I/O error when '//text//', in Gains routines.'
	call bug('w',umsg)
	call bugno('f',iostat)
	end
c************************************************************************
	subroutine uvGnGet(gitem,solno,Gains,Flags,nsols,ngains)
c
	implicit none
	integer solno,nsols,ngains,gitem
	complex Gains(ngains)
	logical Flags(ngains)
c
c  This reads in the gains for a particular time instant. If the gains
c  are beyond the edge of the table, then zero gains are returned.
c
c  Input:
c    gitem
c    solno	Row number of read.
c    nsols
c    ngains	Total number of gains in each record.
c  Output:
c    Gains
c    Flags	Set to true or false, depending whether the gains are good.
c------------------------------------------------------------------------
	integer offset,iostat,i
c
	if(solno.lt.1.or.solno.gt.nsols)then
	  do i=1,ngains
	    Flags(i) = .false.
	  enddo
	else
	  offset = 8*(ngains+1)*(solno-1) + 16
	  call hreadr(gitem,Gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)call uvGnBug(iostat,'reading gains entry')
	  do i=1,ngains
	    Flags(i) = abs(real(Gains(i)))+abs(aimag(Gains(i))).ne.0
	  enddo
	endif
	end
c************************************************************************
	subroutine uvGnPsAp(dowide,ant1,ant2,p,tau,data,flags,nread)
c
	implicit none
	logical dowide
	integer ant1,ant2,p,nread
	complex tau,data(nread)
	logical flags(nread)
c
c  Apply bandpass and delay corrections.
c
c  Input:
c    dowide	Do "wide" channels rather than the standard line.
c    ant1,ant2	Two antenna numbers.
c    p		Polarisation code, in range 1 to 4.
c    tau	Delay/attenuation term. data = data * exp(tau*(f-f0))
c    nread	Number of data channels.
c  Input/Output:
c    data	The correlation data. Corrected on output.
c    flags	The data flags.
c------------------------------------------------------------------------
	include 'uvgn.h'
	integer table
	logical upd
c
c  Dynamic memory commons.
c
	logical lref(MAXBUF)
	double precision dref(MAXBUF/2)
	complex cref(MAXBUF/2)
	equivalence (lref,dref,cref)
	common cref
c
c  Externals.
c
	logical uvvarupd
c
c  Determine which table we are to use, and whether the data has
c  been updated recently.
c
	if(dowide)then
	  table = 2
	  upd = uvvarupd(vwide)
	else
	  table = 1
	  upd = uvvarupd(vline)
	endif
c
	if(upd)call uvGnPsRd(tno,dowide,nread,nchan,nfeeds,nants,
     *	  aver,nspect,sfreq,sdf,nschan,pTab,pFlags(table),pDat(table),
     *	  nDat(table),pFreq(table),nFreq(table),dotau,dopass)
c
c  Having the up-to-date gains and frequencies, apply them.
c
	if(dopass)call uvGnPsPB(ant1,ant2,p,nfeeds,nants,
     *	  cref(pDat(table)),lref(pFlags(table)),data,flags,nread)
c
	if(dotau)call uvGnPsDl(tau,data,dref(pFreq(table)),freq0,nread)
	end
c************************************************************************
	subroutine uvGnPsPB(ant1,ant2,p,nfeeds,nants,
     *				Gains,GFlags,data,flags,nread)
c
	integer ant1,ant2,p,nfeeds,nants,nread
	complex Gains(nread,nfeeds*nants),data(nread)
	logical Gflags(nread,nfeeds*nants),flags(nread)
c
c  Apply the bandpass corrections to the data.
c
c  Input:
c    ant1,ant2	The antenna numbers of the data.
c    p		Polarisation number in range 1 - 4.
c    nfeeds	Number of feed gains.
c    nants	Number of antennae.
c    nread	Number of channels.
c    Gains	The bandpass gains.
c    Gflags	Bandpass flags.
c  Input/Output:
c    data	Correlation data.
c    flags	Flags for the correlation data.
c------------------------------------------------------------------------
	integer i,i1,i2
	integer f1(4),f2(4)
	save f1,f2
	data f1/1,2,1,2/
	data f2/1,2,2,1/
c
	i1 = nfeeds*(ant1-1) + f1(p)
	i2 = nfeeds*(ant2-1) + f2(p)
c
	do i=1,nread
	  data(i)  = data(i)*Gains(i,i1)*conjg(Gains(i,i2))
	  flags(i) = flags(i).and.Gflags(i,i1).and.Gflags(i,i2)
	enddo
	end
c************************************************************************
	subroutine uvGnPsDl(tau,data,freq,freq0,nread)
c
	implicit none
	integer nread
	double precision freq(nread),freq0
	complex tau,data(nread)
c
c  Apply delay correction to the data.
c
c  Input:
c    nread	Number of channels.
c    tau	The complex "delay" correction.
c    freq	Channel frequency.
c    freq0	Reference frequency.
c  Input/Output:
c    data	The correlation data.
c------------------------------------------------------------------------
	integer i
	real atten,theta,s,t
c
	atten = real(tau)
	theta = aimag(tau)
c
	if(theta.ne.0)then
	  if(atten.ne.0)then
	    do i=1,nread
	      s = (real(freq(i)/freq0))**atten
	      t = theta*(freq(i)-freq0)
	      data(i) = data(i) * s * cmplx(cos(t),sin(t))
	    enddo
	  else
	    do i=1,nread
	      t = theta*(freq(i)-freq0)
	      data(i) = data(i) * cmplx(cos(t),sin(t))
	    enddo
	  endif
	else if(atten.ne.0)then
	  do i=1,nread
	    s = (real(freq(i)/freq0))**atten
	    data(i) = s * data(i)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine uvGnPsLd(tno,maxspect,ngains,nchan,
     *	  nspect,sfreq,sdf,nschan,pGains,Size)
c
	implicit none
	integer tno,maxspect,ngains,nchan,nspect
	integer nschan(maxspect),Size,pGains
	double precision sfreq(maxspect),sdf(maxspect)
c
c  Load in the bandpass gains.
c
c  Input:
c    tno
c    ngains
c    maxspect
c  Output:
c    nchan	Total number of channels.
c    nspect
c    sfreq
c    sdf
c    nschan
c    Gains
c------------------------------------------------------------------------
	integer item,iostat,i,off
	double precision freqs(2)
c
c  Dynamic memory rubbish.
c
	include 'maxdim.h'
	complex cref(MAXBUF/2)
	common cref
c
c  Get the dimensionality numbers.
c
	call rdhdi(tno,'nchan0',nchan,0)
	call rdhdi(tno,'nspect0',nspect,0)
	if(ngains.le.0.or.nchan.le.0.or.nspect.le.0) call bug('f',
     *	  'Invalid value for nchan, ngains or nspect, in uvDat')
	Size = ngains*nchan
	call MemAlloc(pGains,Size,'c')
	if(nspect.gt.maxspect)call bug('f',
     *	  'Too many spectral windows for me to handle, in uvDat')
c
c  Load the frequency table.
c
	call haccess(tno,item,'freqs','read',iostat)
	if(iostat.ne.0)call uvGnBug(iostat,'accessing freqs table')
	off = 8
	do i=1,nspect
	  call hreadi(item,nschan(i),off,4,iostat)
	  if(iostat.ne.0)call uvGnBug(iostat,'reading freqs table')
	  off = off + 8
	  call hreadd(item,freqs,off,16,iostat)
	  if(iostat.ne.0)call uvGnBug(iostat,'reading freqs table')
	  off = off + 16
	  sfreq(i) = freqs(1)
	  sdf(i)   = freqs(2)
	enddo
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call uvGnBug(iostat,'closing freqs table')
c
c  Read in the gains themselves now.
c
	call haccess(tno,item,'bandpass','read',iostat)
	if(iostat.ne.0)call uvGnBug(iostat,'accessing bandpass table')
	call hreadr(item,cref(pGains),8,8*ngains*nchan,iostat)
	if(iostat.ne.0)call uvGnBug(iostat,'reading bandpass table')
	call hdaccess(item,iostat)
	if(iostat.ne.0)call uvGnBug(iostat,'closing bandpass table')
c
	end
c************************************************************************
	subroutine uvGnPsRd(tno,dowide,nread,nchan,nfeeds,nants,aver,
     *	  nspect,sfreq,sdf,nschan,pTab,pFlags,pDat,nDat,
     *	  pFreq,nFreq,dotau,dopass)
c
	implicit none
	integer tno,nread,nchan,nfeeds,nants,nspect,nschan(nspect)
	double precision sfreq(nspect),sdf(nspect)
	logical dowide,dotau,dopass,aver
	integer pTab,pFlags,pDat,nDat,pFreq,nFreq
c
c  Match and compute the passband gains.
c
c  Input:
c    tno
c    nread	Number of correlations read.
c    nchan	Total number of channels.
c    nfeeds
c    nants
c    nspect
c    nschan
c    sfreq
c    sdf
c    dowide
c    pTab
c  Input/Output:
c    pDat,nDat
c    pFlags
c    pFreq,nFreq
c------------------------------------------------------------------------
	integer MAXSPECT
	parameter(MAXSPECT=32)
	integer nspect0,nschan0(MAXSPECT)
	double precision sfreq0(MAXSPECT),sdf0(MAXSPECT)
	double precision swidth0(MAXSPECT)
	logical doaver
c
c  Dynamic memory commons.
c
	include 'maxdim.h'
	logical lref(MAXBUF)
	double precision dref(MAXBUF/2)
	complex cref(MAXBUF/2)
	equivalence (lref,dref,cref)
	common cref
c
c  Load the description of the data line.
c
	call uvGnPsGt(tno,dowide,nread,MAXSPECT,doaver,
     *		nspect0,sfreq0,sdf0,swidth0,nschan0)
	if(doaver.and.dopass.and..not.aver)then
	  aver = .true.
	  call bug('w',
     *	  'Performing linetype averaging before applying bandpass!!')
	  call bug('w',
     *	  ' ... this may be very unwise')
	endif
c
c  Check if we have enough space to store the data gains.
c  Then generate the gains.
c
	if(dopass)then
	  if(nDat.lt.nfeeds*nants*nread)then
	    if(nDat.gt.0)then
	      call MemFree(pDat,nDat,'c')
	      call MemFree(pFlags,nDat,'l')
	    endif
	    nDat = nfeeds*nants*nread
	    call MemAlloc(pDat,nDat,'c')
	    call MemAlloc(pFlags,nDat,'l')
	  endif
c
	  call uvGnPsMa(nfeeds*nants,nchan,nspect,sfreq,sdf,nschan,
     *	    cref(pTab),nread,nspect0,sfreq0,sdf0,swidth0,nschan0,
     *	    cref(pDat),lref(pFlags))
	endif
c
c  Check if we have enough space for the frequency table.
c  Then generate the frequency table.
c
	if(dotau)then
	  if(nFreq.lt.nread)then
	    if(nFreq.gt.0)call MemFree(pFreq,nFreq,'d')
	    nFreq = nread
	    call MemAlloc(pFreq,nFreq,'d')
	  endif
c
	  call uvGnPsFq(nread,nspect0,sfreq0,sdf0,nschan0,dref(pFreq))
	endif
c
	end
c************************************************************************
	subroutine uvGnPsFq(nchan,nspect,sfreq,sdf,nschan,freq)
c
	implicit none
	integer nchan,nspect,nschan(nspect)
	double precision sfreq(nspect),sdf(nspect),freq(nchan)
c
c  Determine the offset frequency for each channel of data.
c
c  Input:
c    nchan
c    nspect
c    sfreq
c    sdf
c    nschan
c  Output:
c    freq
c------------------------------------------------------------------------
	integer i,j,k
c
	k = 1
	do j=1,nspect
	  do i=1,nschan(j)
	    freq(k) = sfreq(j) + sdf(j)*(i-1)
	    k = k + 1
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine uvGnPsGt(tno,dowide,nread,mspect,doaver,
     *	  nspect,sfreq,sdf,swidth,nschan)
c
	implicit none
	integer tno,mspect,nspect,nschan(mspect),nread
	logical dowide,doaver
	double precision sfreq(mspect),sdf(mspect),swidth(mspect)
c
c  Determine the frequency setup of the data.
c
c  Input:
c    tno	Handle of the input visibility file.
c    mspect	Max number of windows that can be set.
c    dowide	Ignore standard linetype, and process WIDE channels.
c    nread	Number of channels returned by uvread or uvwread.
c  Output:
c    doaver	Set to true if averaging is being performed.
c    nspect	Number of windows in the output.
c    sfreq	Centre freq of the first channel of each output window.
c    sdf	Frequency increment between each output channel.
c    swidth	Bandwidth of each output channel.
c    nschan	Number of channels in each window.
c------------------------------------------------------------------------
	integer LINE,WIDE,VELO
	parameter(LINE=1,WIDE=2,VELO=3)
	integer TYPE,COUNT,START,WIDTH,STEP
	parameter(TYPE=1,COUNT=2,START=3,WIDTH=4,STEP=5)
	integer lstart,lwidth,lstep,ltype,nchan
	double precision data(6)
c
	integer MSPECT1
	parameter(MSPECT1=32)
	double precision sfreq0(MSPECT1),sdf0(MSPECT1)
	real wfreq(MSPECT1),wwidth(MSPECT1)
	integer nschan0(MSPECT1),nspect0,maxspect
	integer ispect,nwide,i,i0,j
c
	maxspect = min(mspect,MSPECT1)
c
c  Get the linetype description.
c
	if(dowide)then
	  ltype = WIDE
	  nchan = nread
	  lstart = 1
	  lwidth = 1
	  lstep = lwidth
	else
	  call uvinfo(tno,'line',data)
	  ltype  = nint(data(TYPE))
	  nchan  = nint(data(COUNT))
	  lstart = nint(data(START))
	  lwidth = nint(data(WIDTH))
	  lstep  = nint(data(STEP))
	endif
	if(nread.ne.nchan)call bug('f',
     *	  'Inconsistent number of channels, in uvGn(pass-get)')
	doaver = lwidth.gt.1
c
c  Handle the case of channel data.
c
	if(ltype.eq.LINE)then
	  call uvrdvri(tno,'nspect',nspect0,1)
	  if(nspect0.le.0.or.nspect0.gt.maxspect)call bug('f',
     *	    'Bad value for variable nspect, in uvGn(pass-get)')
c
	  call uvgetvri(tno,'nschan',nschan0,nspect0)
	  call uvgetvrd(tno,'sdf',sdf0,nspect0)
	  call uvgetvrd(tno,'sfreq',sfreq0,nspect0)
c
c  Generate the window description parameters for the output.
c
	  ispect = 1
	  nspect = 0
	  dowhile(nchan.gt.0)
	    dowhile(lstart.gt.nschan0(ispect))
	      lstart = lstart - nschan0(ispect)
	      ispect = ispect + 1
	    enddo
	    nspect = nspect + 1
	    sfreq(nspect) = sfreq0(ispect) +
     *		(lstart-1)*sdf0(ispect) + 0.5*(lwidth-1)*sdf0(ispect)
	    nschan(nspect) = min((nschan0(ispect)-lstart)/lstep+1,nchan)
	    swidth(nspect) = lwidth*abs(sdf0(ispect))
	    sdf(nspect)    = lstep *sdf0(ispect)
	    nchan = nchan - nschan(nspect)
	    lstart = lstart + lstep*nschan(nspect)
	  enddo
c
c  Handle "wide" data.
c
	else if(ltype.eq.WIDE)then
	  call uvrdvri(tno,'nwide',nwide,0)
	  if(nchan.gt.maxspect)
     *	    call bug('f','nchan.gt.maxspect, in uvGn(pass-get)')
	  if(nwide.lt.0.or.nwide.gt.maxspect)
     *	    call bug('f','nwide.gt.maxspect, in uvGn(pass-get)')
	  call uvgetvrr(tno,'wfreq',wfreq,nwide)
	  call uvgetvrr(tno,'wwidth',wwidth,nwide)
	  nspect = nchan
	  do j=1,nchan
	    i0 = lstart + (j-1)*lstep
	    sfreq(j) = 0
	    swidth(j) = 0
	    do i=1,lwidth
	      sfreq(j) = sfreq(j) + wfreq(i0)*wwidth(i0)
	      swidth(j) = swidth(j) + wwidth(i0)
	      i0 = i0 + 1
	    enddo
	    sfreq(j) = sfreq(j) / swidth(j)
	    sdf(j) = swidth(j)
	    nschan(j) = 1
	  enddo
c
c  Something else, which I cannot handle.
c
	else
	  call bug('f','Unsupported linetype, in uvGn(pass-get)')
	endif
	end
c************************************************************************
	subroutine uvGnPsMa(ngains,nchan,nspect,sfreq,sdf,nschan,
     *	  Tab,nread,nspect0,sfreq0,sdf0,swidth0,nschan0,Dat,flags)
c
	implicit none
	integer nchan,nread,nspect,nspect0,ngains
	integer nschan(nspect),nschan0(nspect0)
	double precision sfreq(nspect),sdf(nspect)
	double precision sfreq0(nspect0),sdf0(nspect0),swidth0(nspect0)
	complex Tab(nchan,ngains),Dat(nread,ngains)
	logical flags(nread,ngains)
c
c  Match up the data with the measured bandpass functions.
c
c------------------------------------------------------------------------
	integer MAXSPECT
	parameter(MAXSPECT=32)
	integer i,j,k,l,ibeg,iend,n,win(MAXSPECT),ischan(MAXSPECT),off
	integer i0
	double precision startt,endt,startd,endd,width
	real chan,inc,hwidth,goodness,good(MAXSPECT)
c
	ischan(1) = 1
	do j=2,nspect
	  ischan(j) = ischan(j-1) + nschan(j-1)
	enddo
c
	do j=1,nspect0
	  if(sdf0(j).gt.0)then
	    startd = sfreq0(j) - 0.5*swidth0(j)
	    endd   = sfreq0(j) + sdf0(j) * (nschan0(j) - 1)
     *						 + 0.5*swidth0(j)
	  else
	    endd   = sfreq0(j) + 0.5*swidth0(j)
	    startd = sfreq0(j) + sdf0(j) * (nschan0(j) - 1)
     *						 - 0.5*swidth0(j)
	  endif
	  good(j) = 0
	  win(j) = 0
c
	  do i=1,nspect
	    if(sdf(i).gt.0)then
	      startt = sfreq(i) - 0.5*sdf(i)
	      endt   = sfreq(i) + sdf(i) * (nschan(i) - 0.5)
	    else
	      endt   = sfreq(i) - 0.5*sdf(i)
	      startt = sfreq(i) + sdf(i) * (nschan(i) - 0.5)
	    endif
c
c  Is there an overlap in the frequency range of the calibrations tables
c  and the data?
c
	    width = min(endt,endd) - max(startt,startd)
	    if(width.gt.0)then
	      hwidth = abs(swidth0(j) / sdf(i))
	      goodness = width / (endd - startd)
	      if(hwidth.gt.0.8) goodness = goodness + 1
	      if(hwidth.lt.1.2) goodness = goodness + 1
	      if(goodness.gt.good(j))then
		good(j) = goodness
		win(j) = i
	      endif
	    endif
	  enddo
	enddo
c
c  We have now found the best matching bandpass for each spectral window.
c  Now work out the actual bandpass values.
c
	do k=1,ngains
	  off = 1
	  do j=1,nspect0
	    i0 = win(j)
c
c  The case of no matching gains.
c
	    if(i0.eq.0)then
	      do i=1,nschan0(j)
	        flags(off,k) = .false.
	        off = off + 1
	      enddo
	    else
c
c  The case of one corresponding gain in the table for each channel of data.
c
	      chan = (sfreq0(j) - sfreq(i0)) / sdf(i0)
	      inc  = sdf0(j) / sdf(i0)
	      hwidth = 0.5*abs(swidth0(j) / sdf(i0))
	      if(hwidth.lt.0.8)then
	        do i=1,nschan0(j)
	          l = nint(chan)
		  flags(off,k) = l.ge.0.and.l.lt.nschan(i0)
		  l = l + ischan(i0)
		  if(flags(off,k)) flags(off,k) =
     *		    real(tab(l,k)).ne.0.or.aimag(tab(l,k)).ne.0
		  if(flags(off,k)) then
		    dat(off,k) = tab(l,k)
		  else
		    dat(off,k) = 0
		  endif
	          off = off + 1
	          chan = chan + inc
	        enddo
c
c  The case of many corresponding gains in the table for each channel of
c  data. Average the gains together.
c
	      else
	        do i=1,nschan0(j)
	          ibeg = max(1,nint(chan-hwidth)+ischan(i0))
	          iend = min(nint(chan+hwidth)+ischan(i0),nschan(i0))
	          n = 0
	          dat(off,k) = 0
		  do l=ibeg,iend
	            if(real(tab(l,k)).ne.0.or.aimag(tab(l,k)).ne.0)then
	              dat(off,k) = dat(off,k) + tab(l,k)
		      n = n + 1
	            endif
		  enddo
		  flags(off,k) = n.gt.0
		  if(n.gt.1) dat(off,k) = dat(off,k) / n
	          chan = chan + inc
	        enddo
	      endif
	    endif
	  enddo
	enddo
c
	end
c***********************************************************************
	subroutine uvGnInic()
	implicit none
c
c  This reads the channel gains into common from the cgains item.
c--------1---------2---------3---------4---------5---------6---------7--
	integer iostat,item
	include 'uvgn.h'
c
c  Dynamic memory commons.
c
	complex cref(MAXBUF/2)
	common cref
c
c  Get the number of channel gains.
c
	call rdhdi(tno,'ncgains',ncgains,0)
	if(ncgains.le.0.or.ncgains.gt.MAXCHAN) call bug('f',
     *	  'Number of gains is missing or bad, in uvGnInic')
	call rdhdi(tno,'ncbase',ncbase,0)
	if(ncbase.le.0.or.ncbase.gt.MAXBASE) call bug('f',
     *	  'Number of baselines is missing or bad, in uvGnInic')
c
c  Allocate memory.
c
	call MemAlloc(pCgains,ncgains*ncbase,'c')
c
c  Open the cgains item.
c
	call haccess(tno,item,'cgains','read',iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'accessing cgains table')
c
c  Read in channel gains and finish up..
c
	call hreadr(item,cref(pCgains),0,8*ncgains*ncbase,iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'reading cgains table')
	call hdaccess(item,iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'closing cgains table')
c
	end
c***********************************************************************
	subroutine uvGnIniw()
	implicit none
c
c  This reads the wideband gains into common from the wgains item.
c--------1---------2---------3---------4---------5---------6---------7--
	integer iostat,item
	include 'uvgn.h'
c
c  Dynamic memory commons.
c
	complex cref(MAXBUF/2)
	common cref
c
c  Get the number of wideband gains.
c
	call rdhdi(tno,'nwgains',nwgains,0)
	if(nwgains.le.0.or.nwgains.gt.MAXCHAN) call bug('f',
     *	  'Number of gains is missing or bad, in uvGnIniw')
	call rdhdi(tno,'nwbase',nwbase,0)
	if(nwbase.le.0.or.nwbase.gt.MAXBASE) call bug('f',
     *	  'Number of baselines is missing or bad, in uvGnIniw')
c
c  Allocate memory.
c
	call MemAlloc(pWgains,nwgains*nwbase,'c')
c
c  Open the wgains item.
c
	call haccess(tno,item,'wgains','read',iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'accessing wgains table')
c
c  Read in channel gains and finish up..
c
	call hreadr(item,cref(pWgains),0,8*nwgains*nwbase,iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'reading wgains table')
	call hdaccess(item,iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'closing wgains table')
c
	end
