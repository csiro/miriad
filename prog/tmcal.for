c************************************************************************
	program tmcal
	implicit none
c
c= tmcal -- Gain/phase/polarization calibration for SMA data.
c& rjs
c: calibration
c+
c	tmcal is a MIRIAD task which determines calibration corrections
c	(both antenna gains and instrumental polarisation characteristics).
c@ vis
c	Input visibility data file. No default. The visibility data
c	must be in time	order.
c@ select
c	Standard uv selection. Default is all data.
c@ line
c	Standard line-type specification. Multiple channels can be given.
c	Generally it is better to give multiple channels, rather than
c	averaging them into a `channel-0'. The default is all the channel
c	data (or all the wide data, if there is no channel data).
c@ flux
c	The values of the I,Q,U Stokes parameters. If no values are
c	given, and it is a source known to tmcal, tmcal uses its known
c	flux as the default. If tmcal does not know the source, the
c	flux is determined by assuming that the rms gain amplitude is 1.
c	For a planet, the values are interpreted as brightness temperature
c	in Kelvin (planets are assumed to be unpolarised). Otherwise the values
c	are assumed to be in Janskys.
c
c	If the option `qusolve' is used, the given fluxes for Q and U are
c	used as the initial estimates.
c@ refant
c	The reference antenna. Default is 1. The reference antenna needs to be
c	present throughout the observation, and so needs to be chosen with some
c	care. Any solution intervals where the reference antenna is missing are
c	discarded.
c@ minants
c	The minimum number of antenna that must be present before a
c	solution is attempted. Default is 2.
c@ interval
c	This gives the solution interval, in minutes. Antenna gains are assumed
c	to be constant over the solution interval.
c@ tol
c	Error tolerance. The default is 0.001, which should be adequate.
c@ options
c	These options determine what TMCAL solves for. Several options
c	can be given,separated by commas.
c	  nopass     Do not apply any passband calibration.
c	  qusolve    Solve for Q and U fluxes. Good parallactic
c	             angle coverage is required for this.
c	  twoleak    Solve for a "two part" leakage solution,
c	  onegain    Solve for a single antenna gain, which is assumed to be
c	             independent of polarisation. When time-multiplexing of a single
c	             feed is used to measure two states (e.g. R and L), then it should
c	             be accurate to assume the antenna gains to R and L are the same.
c--
c  History:
c    rjs     27nov06 Start on original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSOL,MAXPNT,MAXITER
	parameter(MAXSOL=200,MAXITER=30,MAXPNT=2000000)

	character version*(*)
	parameter(version='tmcal: version 1.0 27-Nov-2006')
c
	complex data(MAXPNT)
	real chi1(MAXPNT),chi2(MAXPNT),wt(MAXPNT),time(MAXPNT)
	real modiqu(3,MAXPNT),qu(2),temp
	integer bl(MAXPNT),pid(MAXPNT),npnt
	double precision timeoff
c
	integer nsol,npart
	complex gains(2*MAXANT*MAXSOL),d(4*MAXANT)
	integer np(MAXSOL)
	double precision t0(MAXSOL)
c
	logical dopass,qusolve,defflux,onegain,present(MAXANT),circ
	character uvflags*8
	real flux(3),tol,epsi,pepsi,gepsi
	double precision interval
	integer refant,minant,tIn,nants,npnt1,niter,i
	character line*64
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
	character itoaf*6
c
c  Get inputs.
c
	call output(version)
	call keyini
	call GetOpt(dopass,qusolve,npart,onegain)
	uvflags = 'dlb'
	if(dopass) uvflags = 'dlbf'
	call uvDatInp('vis',uvflags)
	defflux = .not.keyprsnt('flux')
	call keyr('flux',flux(1),1.)
	call keyr('flux',flux(2),0.)
	call keyr('flux',flux(3),0.)
	call keyi('refant',refant,1)
	call keyi('minants',minant,2)
	call keyd('interval',interval,5.d0)
	call keyr('tol',tol,0.001)
	call keyfin
c
c  Check the input parameters.
c
	if(refant.le.0)call bug('f','Invalid reference antenna number')
	if(minant.le.1)call bug('f','Bad number of mininum antennae')
	if(interval.le.0) call bug('f','Negative value for interval')
	interval = interval / (24*60)
c
c  Open the uv files.
c
	if(.not.uvDatOpn(tIn)) call bug('f','Error opening input file')
        call HisOpen(tIn,'append')
        call HisWrite(tIn,'TMCAL: '//version)
	call HisInput(tIn,'TMCAL')
c 
c  Determine the number of antennae.
c
	call uvnext(tIn)
	call uvrdvri(tIn,'nants',nants,0)
	if(nants.le.0)call bug('f',
     *	  'Unable to determine the number of antennae')
	call output('Number of antennae: '//itoaf(nants))
	if(nants.gt.MAXANT)
     *	  call bug('f','Too many antennae for me to handle')
	if(refant.gt.nants)
     *	  call bug('f','Bad reference antenna number')
	if(minant.gt.nants)
     *	  call bug('f','Number of antennae is less than minimum')
c
c  Read the data, forming the sums that we need.
c
	call output('Reading the data ...')
	call DatRead(tIn,nants,defflux,circ,flux,npart,
     *	  timeoff,
     *	  MAXPNT,data,wt,modiqu,chi1,chi2,bl,time,pid,npnt)
c
c  Break the data into solution intervals.
c
	call Splitup(nants,npnt,data,wt,modiqu,chi1,chi2,bl,time,pid,
     *	  present,interval,refant,minant,onegain,MAXSOL,nsol,np,npnt1)
c
c  Report on how much data was discarded, etc.
c
	if(npnt1.lt.npnt)then
	  call bug('w','Visibilities discarded: '//itoaf(npnt-npnt1))
	  call output(
     *	    'It was not possible to gain-calibrate these visibilities')
	endif
	call output('Number of visibilities accepted: '//itoaf(npnt1))
	call output('Number of solution intervals: '//itoaf(nsol))
	if(nsol.eq.0)call bug('f','There are no data to calibrate')
	npnt = npnt1
	if(circ)then
	  call output('The antenna feeds are circularly polarised')
	else
	  call output('The antenna feeds are linearly polarised')
	endif
c
c  Report on the antennas that were not present, and do a sanity check.
c
	do i=1,nants
	  if(.not.present(i))
     *	    call bug('w','No data found for antenna '//itoaf(i))
	enddo
	if(.not.present(refant))
     *		call bug('f','Refant sanity check failed!')
c
c  Initialise the leakages and source polarisation.
c
	gains(1) = 0
	do i=1,2*nants*npart
	  D(i) = 0
	enddo
	qu(1) = 0
	qu(2) = 0
c
c  Iterate until we have converged.
c
	call output(' ')
	call output('Starting to iterate ...')
	niter = 0
	epsi = tol + 1
	dowhile(epsi.gt.tol.and.niter.lt.MAXITER)
	  niter = niter + 1
c
c  Solve for the gains.
c
	  call ampsolve(nsol,gains,D,nants,
     *	    onegain,npart,circ,niter.eq.1,
     *	    data,wt,modiqu,qu,chi1,chi2,bl,pid,refant,np,npnt,tol,gepsi)
	  write(line,'(a,i2,a,f8.3)')'Iter=',niter,
     *	    ', Amplit/phase solution error:',gepsi
	  call output(line)
c
c  Solve for the leakages.
c
	  call polsolve(nsol,nants,npnt,np,gains,
     *	    data,wt,modiqu,qu,chi1,chi2,bl,pid,D,refant,npart,
     *	    circ,qusolve,pepsi)
	  write(line,'(a,a,f8.3)')    '       ',
     *	    '  Polarization solution error:',pepsi
	  call output(line)
c
	  epsi = max(gepsi,pepsi)
	enddo
c
c  Some final messages.
c
	if(niter.ge.MAXITER)then
	  call bug('w','Failed to converge ... saving solution anyway')
	  call writeo(tIn,
     *		'Warning: Calibration solution failed to converge')
	else
	  call output('Solution converged')
	endif
	call output(' ')
c
c  If no flux was given, scale the gains so that they have an rms of
c  1. Determine what flux this implies.
c
	if(defflux)then
	  temp = modiqu(1,1)
	  call GainScal(temp,Gains,2*nants*nsol)
	  write(line,'(a,f8.3)')'I flux density: ',temp
	  call writeo(tIn,line)
	endif
c
c  Tell about the source polarisation.
c
	if(qusolve)then
	  write(line,'(a,f6.1)')'     Percent Q: ',100*qu(1)
	  call writeo(tIn,line)
	  write(line,'(a,f6.1)')'     Percent U: ',100*qu(2)
	  call writeo(tIn,line)
	endif
c
c  Write out the gain table.
c
	call gettime(time,npnt,t0,np,nsol,timeoff)
	call GainTab(tIn,t0,Gains,nants,nsol)
c
c  Tell about the leakage parameters, and save them.
c
	call leakrep(tIn,nants,D(1),1)
	if(npart.ge.2)call leakrep(tIn,nants,D(2*nants+1),2)
c
c  All said and done.
c
        call HisClose(tIn)
	call uvDatCls()
c
	end
c************************************************************************
	subroutine writeo(tIn,line)
c
	implicit none
	integer tIn
	character line*(*)
c
c  Write out a line to the history file (if open) and the output.
c
c------------------------------------------------------------------------
	character string*80
c
	string = 'TMCAL: '//line
	call HisWrite(tIn,string)
	call output(line)
	end
c************************************************************************
	subroutine leakrep(tIn,nants,D,ln)
c
	implicit none
	integer nants,ln,tIn
	complex D(2,nants)
c
c  Report on and save the leakage solution.
c
c------------------------------------------------------------------------
	character tag*8,line*80
	integer item,j,iostat
c
	if(ln.eq.1)then
	  tag = 'leakage'
	  call writeo(tIn,'Leakage terms:')
	else if(ln.eq.2)then
	  tag = 'leakage2'
	  call writeo(tIn,'Second leakage terms:')
	else
	  call bug('f','Invalid leakage index number in leakrep')
	endif
	do j=1,nants
	  write(line,'(1x,a,i2,a,f6.3,a,f6.3,a,f6.3,a,f6.3,a)')
     *	    'Ant',j,':Da,Db = (',real(D(1,j)),',',aimag(D(1,j)),'),(',
     *			         real(D(2,j)),',',aimag(D(2,j)),')'
	  call writeo(tIn,line)
	enddo
c
c  Create a complex-valued item.
c
	call wrhdc(tin,tag,(1.,0.))
c
	call haccess(tIn,item,tag,'append',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output leakage table')
	  call bugno('f',iostat)
	endif
c
c  The item alignment requirement specifies that we start writing at
c  byte 9. Bytes 0-3 are the header and bytes 4-7 are unused.
c
	call hwriter(item,D,8,2*8*nants,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error writing to the leakage table')
	  call bugno('f',iostat)
	endif
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
	end
c************************************************************************
	subroutine GainTab(tIn,time,Gain,nants,nsol)
c
	implicit none
	integer tIn
	integer nsol,nants
	double precision time(nsol)
	complex Gain(2*nants,nsol)
c
c  Write out the amp/phase gain table.
c
c  Input:
c    tIn	Handle of the file.
c    time	Time (midpoint) for each solution interval.
c    Gain	The gains to be written out.
c    nfeeds	Number of feeds.
c    nants	Number of antenna.
c    nsol	Number of solution intervals.
c------------------------------------------------------------------------
	integer nfeeds
	parameter(nfeeds=2)
	include 'maxdim.h'
	integer iostat,off,item,i,j
	complex G(2*MAXANT)
c
	call haccess(tIn,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output amp/phase table.')
	  call bugno('f',iostat)
	endif
	call hwritei(item,0,0,4,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error writing header of amp/phase table')
	  call bugno('f',iostat)
	endif
c
c  Write out all the gains.
c
	off = 8
	do i=1,nsol
	  call hwrited(item,time(i),off,8,iostat)
	  off = off + 8
	  if(iostat.ne.0)then
	    call bug('w','Error writing time to amp/phase table')
	    call bugno('f',iostat)
	  endif
	  do j=1,nfeeds*nants
	    if(abs(real(Gain(j,i)))+abs(aimag(Gain(j,i))).ne.0)then
	      G(j) = 1/Gain(j,i)
	    else
	      G(j) = (0.,0.)
	    endif
	  enddo
	  call hwriter(item,G,off,8*nfeeds*nants,iostat)
	  off = off + 8*nfeeds*nants
	  if(iostat.ne.0)then
	    call bug('w','Error writing gains to amp/phase table')
	    call bugno('f',iostat)
	  endif
	enddo
c
c  Finished writing the gain table.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Now write out the other parameters that need to go along with this.
c
	call wrhdi(tIn,'nfeeds',nfeeds)
	call wrhdi(tIn,'ntau',0)
	call wrhdi(tIn,'ngains',nfeeds*nants)
	call wrhdi(tIn,'nsols',nsol)
	call wrhdd(tIn,'interval',0.5d0)
c	  
	end
c************************************************************************
	subroutine gettime(time,npnt,t,np,nsol,timeoff)
c
	implicit none
	integer npnt,nsol,np(nsol)
	real time(npnt)
	double precision t(nsol),timeoff
c
c  Get the average time of a solution interval.
c------------------------------------------------------------------------
	integer i,j,k
	double precision t0
c
	i = 0
	do k=1,nsol
	  t0 = 0
	  do j=1,np(k)
	    i = i + 1
	    t0 = t0 + time(i)
	  enddo
	  t(k) = t0/np(k) + timeoff
	enddo
c
	end
c************************************************************************
	subroutine GainScal(flux,Gains,ngains)
c
	implicit none
	integer ngains
	complex Gains(ngains)
	real flux
c
c  Scale the gains and the flux so that the rms gain is 1.
c
c  Input:
c    ngains	Number of gains.
c  Input/Output:
c    Gains	The gains.
c    flux	Nominal source flux.
c------------------------------------------------------------------------
	real Sum2,t
	integer n,i
c
	n = 0
	Sum2 = 0
	do i=1,ngains
	  t = real(Gains(i))**2 + aimag(Gains(i))**2
	  if(t.gt.0)then
	    Sum2 = Sum2 + t
	    n = n + 1
	  endif
	enddo
c
c  Return if all the gains are flagged bad.
c
	if(Sum2.eq.0)return
c
c  Scale the gains.
c
	t = sqrt(n/Sum2)
	do i=1,ngains
	  Gains(i) = t*Gains(i)
	enddo
c
c  Scale the fluxes.
c
	t = Sum2/n
	flux = t*flux
c
	end
c************************************************************************
	subroutine ampsolve(nsol,gain,D,nants,
     *	  onegain,npart,circ,first,
     *	  data,wt,modiqu,qu,chi1,chi2,bl,pid,refant,np,npnt,tol,epsi)
c
	implicit none
	integer nsol,nants,npnt,npart
	integer np(nsol),pid(npnt),refant,bl(npnt)
	logical circ,first,onegain
	complex gain(2,nants,nsol),D(2,nants,npart)
	complex data(npnt)
	real wt(npnt),modiqu(3,npnt),qu(2),chi1(npnt),chi2(npnt)
	real tol,epsi
c
c  Input:
c    first	True if this is the first time through.
c    tol	Convergence tolerance.
c  Output:
c    epsi	RMS change in gains.
c------------------------------------------------------------------------
	integer X,Y
	parameter(X=1,Y=2)
	include 'maxdim.h'
	integer i,j,b,p,k,l,idx,i1,i2
	complex sumvm(MAXANT*(MAXANT-1)/2,2),model,g(MAXANT)
	real    summm(MAXANT*(MAXANT-1)/2,2),eps1,eps2
c
	if(nants.gt.MAXANT)call bug('f','nants > MAXANT in ampsolve')
	epsi = 0
	idx = 0
	do l=1,nsol
c
c  Zero the accumulators.
c
	  b = 0
	  do j=2,nants
	    do i=1,j-1
	      b = b + 1
	      sumvm(b,1) = 0
	      summm(b,1) = 0
	      sumvm(b,2) = 0
	      summm(b,2) = 0
	    enddo
	  enddo
c
c  Accumulate models of the data.
c
	  do k=idx+1,idx+np(l)
	    p = pid(k)
	    if(onegain)p = 1
	    if(p.le.2)then
	      call basant(dble(bl(k)),i1,i2)
	      call genvis(nants,i1,i2,pid(k),qu,modiqu(1,k),circ,
     *					chi1(k),chi2(k),D,npart,model)
	      b = i1 + (i2-1)*(i2-2)/2
	      sumvm(b,p) = sumvm(b,p) + Wt(k)*conjg(Model)*Data(k)
	      summm(b,p) = summm(b,p) + Wt(k)*(real( Model)*real(Model)+
     *					      aimag(Model)*aimag(Model))
	    endif
	  enddo
c
c  Now solve.
c
	  if(onegain)then
	    if(.not.first)call gunpack(gain(X,1,l),g,nants)
	    call ampsol(nants,sumvm,summm,	    g,refant,
     *							first,tol,eps1)
	    call gpack(g,gain(X,1,l),nants)
	    call gpack(g,gain(Y,1,l),nants)
	    epsi = max(epsi,eps1)
	  else
	    if(.not.first)call gunpack(gain(X,1,l),g,nants)
	    call ampsol(nants,sumvm(1,1),summm(1,1),g,refant,
     *							first,tol,eps1)
	    call gpack(g,gain(X,1,l),nants)
	    if(.not.first)call gunpack(gain(Y,1,l),g,nants)
	    call ampsol(nants,sumvm(1,2),summm(1,2),g,refant,
     *							first,tol,eps2)
	    call gpack(g,gain(Y,1,l),nants)
	    epsi = max(epsi,eps1,eps2)
	  endif
c
c  Loop the loop.
c
	  idx = idx + np(l)
	enddo
c
	end
c************************************************************************
	subroutine gunpack(gain,g,nants)
c
	implicit none
	integer nants
	complex gain(2,nants),g(nants)
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,nants
	  g(i) = gain(1,i)
	enddo
c
	end
c************************************************************************
	subroutine gpack(g,gain,nants)
c
	implicit none
	integer nants
	complex gain(2,nants),g(nants)
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,nants
	  gain(1,i) = g(i)
	enddo
c
	end
c************************************************************************
	subroutine ampsol(nants,sumvm,summm,gain,refant,first,tol,epsi)
c
	implicit none
	integer nants
	logical first
	complex sumvm((nants*(nants-1))/2),gain(nants)
	real summm((nants*(nants-1))/2),tol,epsi
	integer refant
c
c  This routine does a self-cal solution for a single interval and polarisation.
c
c  Input:
c    first
c    nants
c    sumvm
c    summm
c    refant
c  Input/Output:
c    gain
c  Output:
c    epsi	RMS gain error.
c------------------------------------------------------------------------
	integer MAXITER
	parameter(MAXITER=50)
	include 'maxdim.h'
	integer b,i,j,niter,i1,i2
	complex sum(MAXANT),t,w
	real alpha,rvm,rmm,sum2(MAXANT),temp,change,sumwt
c
	if(nants.gt.MAXANT)call bug('f','Too many antennas in ampsol')
c
c  If its the first time through, initialise the gains with an estimate which
c  has the right overall value.
c
	if(first)then
	  rmm = 0
	  rvm = 0
	  b = 0
	  do j=2,nants
	    do i=1,j-1
	      b = b + 1
	      rmm = rmm + summm(b)
	      rvm = rvm + abs(sumvm(b))
	    enddo
	  enddo
	  alpha = sqrt(rvm/rmm)
	  do i=1,nants
	    gain(i) = alpha
	  enddo
	endif
c
c  Now iterate.
c
	niter = 0
	epsi = tol + 1
	dowhile(niter.lt.MAXITER.and.epsi.gt.tol)
	  niter = niter + 1
c
c  Initialise accumulators.
c
	  do i=1,nants
	    sum(i) = 0
	    sum2(i) = 0
	  enddo
c
c  Accumulate.
c
	  b = 0
	  do i2=2,nants
	    do i1=1,i2-1
	      b = b + 1
	      sum(i1) = sum(i1) + gain(i2)*sumvm(b)
	      temp = real(gain(i2))*real(gain(i2)) + 
     *		     aimag(gain(i2))*aimag(gain(i2))
	      sum2(i1) = sum2(i1) + temp*summm(b)
c
	      sum(i2) = sum(i2) + gain(i1)*conjg(sumvm(b))
	      temp = real(gain(i1))*real(gain(i1)) + 
     *		  aimag(gain(i1))*aimag(gain(i1))
	      sum2(i2) = sum2(i2) + temp*summm(b)
	    enddo
	  enddo
c
c  Update the gains.
c
	  change = 0
	  sumwt  = 0
	  do i=1,nants
	    if(sum2(i).gt.0)then
	      t = 0.5* ( sum(i)/sum2(i) - gain(i) )
	      gain(i) = gain(i) + t
	      change = change + real(t)*real(t) + aimag(t)*aimag(t)
	      sumwt = sumwt +  real(gain(i))*real(gain(i))
     *			    + aimag(gain(i))*aimag(gain(i))
	      sum(i) = 0
	      sum2(i) = 0
	    else
	      gain(i) = 0
	    endif
	  enddo
	  epsi = sqrt(change/sumwt)
	enddo
c
c  Standardise so that the phase of the reference antenna is 0.
c
	temp = abs(gain(refant))
	if(temp.eq.0)call bug('f','No reference antenna in ampsol')
	w = conjg(gain(refant))/temp
	do i=1,nants
	  gain(i) = w*gain(i)
	enddo
c
	end
c************************************************************************
	subroutine DatRead(tIn,nants,defflux,circ,flux,npart,
     *	  timeoff,
     *	  MAXPNT,data,wt,modiqu,chi1,chi2,bl,time,pid,npnt)
c
	implicit none
	integer tIn,nants,MAXPNT,bl(MAXPNT),pid(MAXPNT),npnt
	integer npart
	logical defflux,circ
	real flux(3),wt(MAXPNT),modiqu(3,MAXPNT)
	real chi1(MAXPNT),chi2(MAXPNT),time(MAXPNT)
	double precision timeoff
	complex data(MAXPNT)
c
c  Read in the data, average over frequency, and save the data. Work out the model of 
c  the data.
c
c  Input:
c    tIn
c    nants
c    flux
c    MAXPNT
c  Input/Output:
c    defflux
c    npart
c  Outputs:
c    circ
c    timeoff
c    data
c    wt
c    modiqu
c    chi1
c    chi2
c    bl
c    time
c    pid
c    npnt
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nchan,i,n,pol,iplanet,i1,i2,poloff
	complex dat(MAXCHAN),sum
	real rms1,rms2,iqu(3),uu,vv
	logical flags(MAXCHAN)
	double precision sfreq(MAXCHAN),freq,schi2,schi22,schi1,schi12
	double precision preamble(4)
	character source*32
c
c  Initialise various counters and accumulators.
c
	schi1 = 0
	schi12 = 0
	schi2 = 0
	schi22 = 0
	npnt = 0
	call uvrewind(tIn)
c
c  Loop.
c
	call uvDatRd(preamble,dat,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
	  call uvDatGtr('variance',rms2)
	  call uvinfo(tIn,'sfreq',sfreq)
	  Sum = 0
	  n = 0
c
c  Sum over frequency.
c
	  call basant(preamble(4),i1,i2)
	  if(i2.le.nants.and.i1.gt.0.and.i1.lt.i2)then
	    freq = 0
	    do i=1,nchan
	      if(flags(i))then
	        Sum = Sum + Dat(i)
	        freq = freq + sfreq(i)
	        n = n + 1
	      endif
	    enddo
	  endif
c
c  Do we accept this point?
c
	  if(n.gt.0)then
	    freq = freq / n
	    uu = preamble(1)*freq
	    vv = preamble(2)*freq
	    call uvrdvri(tIn,'pol',pol,0)
	    if(pol.ge.0)call bug('f','Invalid polarisation code')
c
c  For the first accepted visibility, work out a few things ...
c
	    if(npnt.eq.0)then
	      timeoff = preamble(3)
	      timeoff = int(timeoff - 0.5) + 0.5
	      poloff  = -((-pol-1)/4)*4
	      if(poloff.ne.0.and.poloff.ne.-4)
     *		call bug('f','Polarisation type cannot be handled')
	      circ = poloff.eq.0
	      call uvrdvra(tIn,'source',source,' ')
	      call getmodel(source,defflux,flux,freq,iplanet,iqu)
	    endif
c
c  Save information on the accepted visibility.
c
	    npnt = npnt + 1
	    if(npnt.gt.MAXPNT)call bug('f','Buffer overflow')
	    Data(npnt) = Sum/n
	    if(rms2.le.0)then
	     Wt(npnt) = 1
	    else
	      Wt(npnt)   = n/rms2
	    endif
	    time(npnt) = preamble(3) - timeoff
	    pid(npnt) = -pol + poloff
	    if(pid(npnt).lt.1.or.pid(npnt).gt.4)call bug('f',
     *	      'Invalid polarisation type')
	    bl(npnt) = nint(preamble(4))
	    call uvrdvrr(tIn,'chi',chi1(npnt),0.)
	    if(npart.eq.1)then
	      chi1(npnt) = chi1(npnt)
	      chi2(npnt) = 0
	    else
	      call uvrdvrr(tIn,'chi2',chi2(npnt),0.)
	      chi1(npnt) = chi1(npnt) - chi2(npnt)
	      schi1  = schi1 + chi1(npnt)
	      schi12 = schi12 + chi1(npnt)*chi1(npnt)
	      schi2  = schi2 + chi2(npnt)
	      schi22 = schi22 + chi2(npnt)*chi2(npnt)
	    endif
c
c  Determine the model for these data.
c
	    if(iplanet.ne.0)then
	      call gtplanet(iplanet,iqu(1),uu,vv,freq,preamble(3),
     *						modiqu(1,npnt))
	    else
	      modiqu(1,npnt) = iqu(1)
	      modiqu(2,npnt) = iqu(2)
	      modiqu(3,npnt) = iqu(3)
	    endif
	  endif
c
c  Loop the loop.
c
	  call uvDatRd(preamble,dat,flags,MAXCHAN,nchan)
	enddo
c
	if(npnt.eq.0)call bug('f','No data were accepted')
c
c  Do we have a one or two axis problem. Determine the rms variation
c  of the second angle. If its less that 0.1 radians, assume we have a
c  single axis system.
c
	if(npart.gt.1)then
	  rms1 = sqrt((npnt*schi12-schi1*schi1)/(npnt*npnt))
	  rms2 = sqrt((npnt*schi22-schi2*schi2)/(npnt*npnt))
	  if(rms1.lt.0.1.or.rms2.lt.0.1)npart = 1
	  if(npart.eq.1)then
	    call bug('w',
     *	      'Too little rotation for a two-part leakage solution')
	    call bug('w',
     *	      'Reverting to a one-part leakage solution')
	    do i=1,npnt
	      chi1(npnt) = chi1(npnt) + chi2(npnt)
	      chi2(npnt) = 0
	    enddo
	  endif
	endif
c
c  All said and done.
c
	end
c************************************************************************
	subroutine getmodel(source,defflux,flux,freq,iplanet,iqu)
c
	implicit none
	character source*(*)
	logical defflux
	real flux(3),iqu(3)
	double precision freq
	integer iplanet
c
c  Determine what model we should use for this source.
c
c  Input:
c    source
c    flux
c    freq
c  Input/Output:
c    defflux
c  Output:
c    iplanet
c    iqu
c------------------------------------------------------------------------
	character line*64
	real temp
	integer ierr
c
c  Externals.
c
	integer plLook
	real plTbs
	character itoaf*5
c
	iplanet = 0
	iqu(1) = flux(1)
	iqu(2) = flux(2)
	iqu(3) = flux(3)
	iplanet = plLook(source)
	if(defflux)then
	  if(iplanet.ne.0)then
	    iqu(1) = pltbs(iplanet,real(freq))
	    defflux = .false.
	  else
	    call calstoke(source,'i',real(freq),temp,1,ierr)
	    if(ierr.eq.0)then
	      line = 'Found flux for calibrator '//source
	      call output(line)
	    else if(ierr.eq.1)then
	      line = 'Extrapolating flux for calibrator '//source
	      call output(line)
	    else
	      line = 'Source assumed to be point-like: '//source
	      call output(line)
	    endif
	    if(ierr.eq.0.or.ierr.eq.1)then
	      iqu(1) = temp
	      call calstoke(source,'q',real(freq),iqu(2),1,ierr)
	      call calstoke(source,'u',real(freq),iqu(3),1,ierr)
	      write(line,'(''Using IQU = '',3(f7.3'',''))')
     *						iqu(1),iqu(2),iqu(3)
	      call output(line)
	      defflux = .false.
	    endif
	  endif
	endif
c
	if(iplanet.ne.0)then
	  line = 'Found planet '//source
	  call output(line)
	  line = 'Assumed brightness temperature (Kelvin): '//
     *					itoaf(nint(iqu(1)))
	  call output(line)
	  if(abs(iqu(2))+abs(iqu(3)).gt.0)call bug('w',
     *	    'Setting the planet to be unpolarised')
	  iqu(2) = 0
	  iqu(3) = 0
	endif
c
	end
c************************************************************************
	subroutine gtplanet(iplanet,pltb,uu,vv,freq,utc,iqu)
c
	implicit none
	integer iplanet
	real pltb,uu,vv,iqu(3)
	double precision freq,utc
c
c  Determine the characteristics of a planet.
c
c  Input:
c    iplanet
c    pltb
c    uu,vv
c    freq
c    time
c  Output:
c    iqu
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision sub(3),tdb
	real bmaj,bmin,bpa,cospa,sinpa,a,b,dist
c
c  Externals.
c
	double precision deltime
	real j1xbyx
c
c  For the time being, planets are assumed to have no polarisation.
c
	iqu(2) = 0
	iqu(3) = 0
c
	tdb = utc + deltime(utc,'tdb')
	call plpar(tdb,iplanet,sub,dist,bmaj,bmin,bpa)
	cospa = cos(bpa)
	sinpa = sin(bpa)
	b = PI * sqrt((bmaj*(uu*cospa-vv*sinpa))**2
     *              + (bmin*(uu*sinpa+vv*cospa))**2)
	a = 2 * pltb * (KMKS*1e18)/(CMKS*CMKS*1e-26)
     *	  * 2 * PI/4 * bmaj*bmin
	iqu(1) = a*freq*freq*j1xbyx(b)
c
	end
c************************************************************************
	subroutine Splitup(nants,npnt,
     *	  data,wt,modiqu,chi1,chi2,bl,time,pid,
     *	  present,interval,refant,minant,onegain,MAXSOL,nsol,np,npnt1)
c
	implicit none
	integer npnt,refant,minant,MAXSOL,nsol,np(MAXSOL),npnt1,nants
	complex data(npnt)
	integer pid(npnt),bl(npnt)
	real time(npnt),chi1(npnt),chi2(npnt),wt(npnt),modiqu(3,npnt)
	double precision interval
	logical onegain,present(nants)
c
c  Determine solution intervals and compact out data that are not useful.
c
c  Input:
c    npnt
c    interval
c    refant
c    minant
c    onegain
c    maxsol
c  Output:
c    nsol
c    np
c    npnt1
c  Input/Output:
c    data
c    wt
c    modiqu
c    chi1
c    chi2
c    bl
c    time
c    pid
c  Output:
c    present
c------------------------------------------------------------------------
	include 'maxdim.h'
	logical ok(2,MAXANT)
	real tmin,tmax
	integer i,i1,i2,npd,p1,p2,k
c
	do i=1,nants
	  present(i) = .false.
	enddo
c
	nsol = 0
	tmin = time(1)
	tmax = tmin
c
c  A dummy statement to keep my lint-checker appeased.
c
	ok(1,1) = .false.
c
c Loop, starting by flushing the buffer if needed.
c
	npd = 0
	p1 = 1
	p2 = 1
	do k=1,npnt
	  if((time(k).gt.tmin+interval.or.time(k).lt.tmax-interval).and.
     *	      						npd.gt.0)then
	    call accepter(npnt,nants,ok,minant,refant,onegain,
     *	      present,p1,p2,data,wt,time,bl,pid,chi1,chi2,modiqu,npd)
	    if(npd.gt.0)then
	      nsol = nsol + 1
	      if(nsol.gt.MAXSOL)
     *		call bug('f','Too many solution intervals')
	      np(nsol) = npd
	      p2 = p2 + npd
	      npd = 0
	    endif
	    p1 = k
	    tmin = time(k)
	    tmax = tmin
	  endif
c
c  Get ready for a new interval.
c
	  if(npd.eq.0)then
	    do i=1,nants
	      ok(1,i) = .false.
	      ok(2,i) = .false.
	    enddo
	  endif
c
c  Process this visibility.
c
	  npd = npd + 1
	  tmin = min(time(k),tmin)
	  tmax = max(time(k),tmax)
	  if(pid(k).eq.1.or.pid(k).eq.2)then
	    call basant(dble(bl(k)),i1,i2)
	    if(i1.eq.refant.or.i2.eq.refant)then
	      ok(pid(k),i1) = .true.
	      ok(pid(k),i2) = .true.
	    endif
	  endif
	enddo
c
c  Accept the last one, if necessary.
c
	if(npd.gt.0)then
	  call accepter(npnt,nants,ok,minant,refant,onegain,
     *	    present,p1,p2,data,wt,time,bl,pid,chi1,chi2,modiqu,npd)
	  if(npd.gt.0)then
	    nsol = nsol + 1
	    if(nsol.gt.MAXSOL)
     *		call bug('f','Too many solution intervals')
	    np(nsol) = npd
	    p2 = p2 + npd
	  endif
	endif
c
	npnt1 = p2 - 1
c
	end
c************************************************************************
	subroutine accepter(npnt,nants,ok,minant,refant,onegain,
     *	    present,p1,p2,data,wt,time,bl,pid,chi1,chi2,modiqu,npd)
c
	implicit none
	integer nants,npnt,minant,refant,p1,p2,bl(npnt),pid(npnt),npd
	logical ok(2,nants),onegain,present(nants)
	complex data(npnt)
	real wt(npnt),time(npnt),chi1(npnt),chi2(npnt),modiqu(3,npnt)
c
c  Check whether this interval is acceptable or not.
c------------------------------------------------------------------------
	integer na,i,k1,k2,i1,i2
	na = 0
	do i=1,nants
	  if(onegain)then
	    ok(1,i) = ok(1,i).or.ok(2,i)
	  else
	    ok(1,i) = ok(1,i).and.ok(2,i)
	  endif
	  if(ok(1,i))na = na + 1
	enddo
c
c  Reject if necessary.
c
	k1 = p1 - 1
	k2 = p1 - 1
	if(ok(1,refant).and.na.ge.minant)then
	  do i=1,nants
	    present(i) = present(i).or.ok(1,i)
	  enddo
c
	  do i=1,npd
	    k1 = k1 + 1
	    call basant(dble(bl(k1)),i1,i2)
c
c  Select only data where there is a possibility of a gain solution.
c
	    if(ok(1,i1).and.ok(1,i2))then
	      k2 = k2 + 1
	      if(k2.lt.k1)then
		data(k2) = data(k1)
		wt(k2) = wt(k1)
		pid(k2) = pid(k1)
		bl(k2) = bl(k1)
		time(k2) = time(k1)
		chi1(k2) = chi1(k1)
		chi2(k2) = chi2(k1)
		modiqu(1,k2) = modiqu(1,k1)
		modiqu(2,k2) = modiqu(2,k1)
		modiqu(3,k2) = modiqu(3,k1)
	      endif
	    endif
	  enddo
	endif
c
	npd = k2 - p2 + 1
c
	end
c************************************************************************
	subroutine getopt(dopass,qusolve,npart,onegain)
c
	implicit none
	logical dopass,qusolve,onegain
	integer npart
c
c  Get processing options.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=4)
	logical present(NOPT)
	character opts(NOPT)*8
	data opts/'nopass  ','qusolve ','twoleak ','onegain '/
c
	call options('options',opts,present,NOPT)
	dopass  = .not.present(1)
	qusolve =      present(2)
	onegain =      present(4)
	if(present(3))then
	  npart = 2
	else
	  npart = 1
	endif
c
	end
c************************************************************************
	subroutine genvis(nants,i1,i2,pid,qu,modiqu,circ,
     *						chi1,chi2,D,npart,v0)
c
	implicit none
	integer nants,i1,i2,pid,npart
	real qu(2),modiqu(3),chi1,chi2
	complex D(2,nants,npart),v0
	logical circ
c
c  Given the model visibilies, leakage parameters and rotation angles,
c  this computes the expected visibilities *without the effect of antennna
c  gain*.
c
c  Input:
c    i1,i2	Antenna numbers.
c    pid	Polarisation index.
c    modiqu	Model I,Q,U of the source.
c    qu		Additional Q,U components (proportional to I) to add.
c    chi1,chi2	Rotation angles.
c    npart	The number of leakages to solve for.
c    circ	True if the feeds are nominally circularly polarised.
c  Output:
c    v0		Estimate of the visibility function.
c************************************************************************
	complex v(4)
c
	call genmodel(qu,modiqu,circ,v)
	call rotleak(chi1,circ,D(1,1,1),i1,i2,nants,v)
	if(npart.gt.1)call rotleak(chi2,circ,D(1,1,2),i1,i2,nants,v)
c
c  Return the bacon.
c
	v0 = v(pid)
	end
c************************************************************************
	subroutine genmodel(qu,modiqu,circ,v)
c
	implicit none
	real qu(2),modiqu(3)
	logical circ
	complex v(4)
c
c  Determine the model polarisation visibilities for a given I,Q,U model.
c------------------------------------------------------------------------
	real ii,qq,uu
c
	ii = modiqu(1)
	qq = modiqu(2) + qu(1)*ii
	uu = modiqu(3) + qu(2)*ii
	if(circ)then
	  v(1) = ii
	  v(3) = cmplx(qq,-uu)
	  v(4) = cmplx(qq, uu)
	  v(2) = ii
	else
	  v(1) = ii + qq
	  v(3) = uu
	  v(4) = uu
	  v(2) = ii - qq
	endif
c
	end
c************************************************************************
	subroutine rotleak(chi,circ,D,i1,i2,nants,V)
c
	implicit none
	integer nants,i1,i2
	real chi
	complex D(2,nants),v(4)
	logical circ
c
c  Rotate the visibilities and do leakage.
c------------------------------------------------------------------------
	integer xx,xy,yx,yy
	parameter(xx=1,xy=3,yx=4,yy=2)
	integer P,Q
	parameter(P=1,Q=2)
	real c,s,cc,ss,cs,c2,s2
	complex vpp,vpq,vqp,vqq
	complex vaa,vab,vba,vbb
c
	c = cos(chi)
	s = sin(chi)
	cc = c*c
	ss = s*s
	cs = c*s
	c2 = cc - ss
	s2 = 2*cs
c
	vaa = v(xx)
	vab = v(xy)
	vba = v(yx)
	vbb = v(yy)
	if(circ)then
	  vpp = vaa
	  vpq = vab*cmplx(c2, s2)
	  vqp = vba*cmplx(c2,-s2)
	  vqq = vbb
	else
	  vpp =	cc*vaa + cs*(vab+vba) + ss*vbb
	  vpq =	cs*(vbb-vaa) + cc*vab - ss*vba
	  vqp =	cs*(vbb-vaa) - ss*vab + cc*vba
	  vqq =	cc*vbb - cs*(vab+vba) + ss*vaa
	endif
	v(xx) = vpp + D(P,i1)*vqp + conjg(D(P,i2))*vpq + 
     *		      D(P,i1)     * conjg(D(P,i2))*vqq
	v(xy) = vpq + D(P,i1)*vqq + conjg(D(Q,i2))*vpp + 
     *		      D(P,i1)     * conjg(D(Q,i2))*vqp
	v(yx) = vqp + D(Q,i1)*vpp + conjg(D(P,i2))*vqq + 
     *		      D(Q,i1)     * conjg(D(P,i2))*vpq
	v(yy) = vqq + D(Q,i1)*vpq + conjg(D(Q,i2))*vqp + 
     *		      D(Q,i1)     * conjg(D(Q,i2))*vpp
	end
c************************************************************************
	subroutine rotvis(chi,circ,V)
c
	implicit none
	real chi
	complex v(4)
	logical circ
c
c  Rotate the visibilities and do leakage.
c------------------------------------------------------------------------
	integer P,Q
	parameter(P=1,Q=2)
	real c,s,cc,ss,cs,c2,s2
	complex vaa,vab,vba,vbb
c
	c = cos(chi)
	s = sin(chi)
	cc = c*c
	ss = s*s
	cs = c*s
	c2 = cc - ss
	s2 = 2*cs
c
	if(circ)then
	  v(3) = v(3)*cmplx(c2, s2)
	  v(4) = v(4)*cmplx(c2,-s2)
	else
	  vaa = v(1)
	  vab = v(3)
	  vba = v(4)
	  vbb = v(2)
	  v(1) = cc*vaa + cs*(vab+vba) + ss*vbb
	  v(3) = cs*(vbb-vaa) + cc*vab - ss*vba
	  v(4) = cs*(vbb-vaa) - ss*vab + cc*vba
	  v(2) = cc*vbb - cs*(vab+vba) + ss*vaa
	endif
	end
c************************************************************************
	subroutine geneqn(qu,modiqu,circ,chi1,chi2,npart,pid,c)
c
	implicit none
	integer pid,npart
	real c(2,8,npart),qu(2),modiqu(3),chi1,chi2
	logical circ
c
c  Generate the coefficients of the linearised equation matching the 
c  model visibilites with the observed visibilities.
c
c  Input:
c    qu		Additional Q,U
c    modiqu	Model I,Q,U
c    circ	True if the data are circularly polarised.
c    chi1	First feed position angle.
c    chi2	Second feed position angle.
c    npart	Number of leakage levels.
c    pid	Polarisation index.
c
c  Output:
c    c		The coefficients of the equation relating leakage to
c		visibility.
c------------------------------------------------------------------------
	integer rp,ip
	parameter(rp=1,ip=2)
c
	complex v(4)
	real coeff(2,8,4,2)
	integer i,j
c
	call genmodel(qu,modiqu,circ,v)
	call rotvis(chi1,circ,v)
c
	call coeffill(v,coeff(1,1,1,1))
	if(npart.eq.2)then
	  call rotcoeff(chi2,circ,coeff(1,1,1,1))
	  call rotvis(chi2,circ,v)
	  call coeffill(v,coeff(1,1,1,2))
	endif
c
	do j=1,npart
	  do i=1,8
	    c(rp,i,j) = coeff(rp,i,pid,j)
	    c(ip,i,j) = coeff(ip,i,pid,j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine gencqu(modiqu,circ,chi1,chi2,npart,pid,cqu)
c
	implicit none
	integer npart,pid
	logical circ
	real modiqu(3),chi1,chi2,cqu(2,2)
c------------------------------------------------------------------------
	integer XX,YY
	parameter(XX=1,YY=2)
	real dmodiqu(3),qu(2),chi
	complex v(4)
	data qu/0.0,0.0/
c
c  The following is an empirical fudge that works!
c  A problem for linear polarisations with the technique of going a gain calibration
c  followed by solving for Q and U is that any signature of Q and U has been 
c  gain calibrated out of the XX and YY visibilities. A fudge is to reflect this
c  in the equations of the visibility function. Only the XY and YX visibilities
c  will contribute to determining Q and U.
c
	if(.not.circ.and.(pid.eq.XX.or.pid.eq.YY))then
	  cqu(1,1) = 0
	  cqu(2,1) = 0
	  cqu(1,2) = 0
	  cqu(2,2) = 0
c
c  If its not linear polarisation, parallel hand correlations, get the coefficients
c  that the text book would tell you are needed.
c
	else
	  chi = chi1
	  if(npart.gt.1)chi = chi + chi2
c
	  dmodiqu(1) = 0
	  dmodiqu(2) = modiqu(1)
	  dmodiqu(3) = 0
	  call genmodel(qu,dmodiqu,circ,v)
	  call rotvis(chi,circ,v)
	  cqu(1,1) =  real(v(pid))
	  cqu(2,1) = aimag(v(pid))
c
	  dmodiqu(1) = 0
	  dmodiqu(2) = 0
	  dmodiqu(3) = modiqu(1)
	  call genmodel(qu,dmodiqu,circ,v)
	  call rotvis(chi,circ,v)
	  cqu(1,2) =  real(v(pid))
	  cqu(2,2) = aimag(v(pid))
	endif
c
	end
c************************************************************************
	subroutine coeffill(v,coeff)
c
	implicit none
	complex v(4)
	real coeff(2,8,4)
c
c  Input:
c    v 		Model visibility.
c  Output:
c    coeff	Coefficients relating leakage to measured visibility.
c
c  On output, the coeff array contains the coefficients giving the relationship
c  between the measured visibilities and the unknown leakages. coeff(*,1)
c  and coeff(*,2) corresponds to the "real equation" and "imaginary equation"
c  respectively.
c
c  The ordering of coefficients is
c    real(D1X),imag(D1X),real(D1Y),imag(D1Y),real(D2X),imag(D2X),real(D2Y),imag(D2Y)
c
c------------------------------------------------------------------------
	integer rp,ip
	parameter(rp=1,ip=2)
	integer XX,XY,YX,YY
	parameter(XX=1,XY=3,YX=4,YY=2)
	integer x1r,x1i,y1r,y1i,x2r,x2i,y2r,y2i
	parameter(x1r=1,x1i=2,y1r=3,y1i=4,x2r=5,x2i=6,y2r=7,y2i=8)
	integer i,j
c
	do j=1,4
	  do i=1,8
	    coeff(1,i,j) = 0
	    coeff(2,i,j) = 0
	  enddo
	enddo
c
	coeff(rp,x1r,xx) =   real(v(yx))
	coeff(rp,x1i,xx) = -aimag(v(yx))
	coeff(ip,x1r,xx) =  aimag(v(yx))
	coeff(ip,x1i,xx) =   real(v(yx))
c
	coeff(rp,x2r,xx) =   real(v(xy))
	coeff(rp,x2i,xx) =  aimag(v(xy))
	coeff(ip,x2r,xx) =  aimag(v(xy))
	coeff(ip,x2i,xx) =  -real(v(xy))
c
c
	coeff(rp,x1r,xy) =   real(v(yy))
	coeff(rp,x1i,xy) = -aimag(v(yy))
	coeff(ip,x1r,xy) =  aimag(v(yy))
	coeff(ip,x1i,xy) =   real(v(yy))
c
	coeff(rp,y2r,xy) =   real(v(xx))
	coeff(rp,y2i,xy) =  aimag(v(xx))
	coeff(ip,y2r,xy) =  aimag(v(xx))
	coeff(ip,y2i,xy) =  -real(v(xx))
c
c
	coeff(rp,y1r,yx) =   real(v(xx))
	coeff(rp,y1i,yx) = -aimag(v(xx))
	coeff(ip,y1r,yx) =  aimag(v(xx))
	coeff(ip,y1i,yx) =   real(v(xx))
c
	coeff(rp,x2r,yx) =   real(v(yy))
	coeff(rp,x2i,yx) =  aimag(v(yy))
	coeff(ip,x2r,yx) =  aimag(v(yy))
	coeff(ip,x2i,yx) =  -real(v(yy))
c
c
	coeff(rp,y1r,yy) =   real(v(xy))
	coeff(rp,y1i,yy) = -aimag(v(xy))
	coeff(ip,y1r,yy) =  aimag(v(xy))
	coeff(ip,y1i,yy) =   real(v(xy))
c
	coeff(rp,y2r,yy) =   real(v(yx))
	coeff(rp,y2i,yy) =  aimag(v(yx))
	coeff(ip,y2r,yy) =  aimag(v(yx))
	coeff(ip,y2i,yy) =  -real(v(yx))
c
	end
c************************************************************************
	subroutine rotcoeff(chi,circ,coeff)
c
	implicit none
	real chi,coeff(2,8,4)
	logical circ
c
c------------------------------------------------------------------------
	integer xx,xy,yx,yy
	parameter(xx=1,xy=3,yx=4,yy=2)
	real c,s,cc,ss,cs,c2,s2
	real vaa,vab,vba,vbb,vabr,vabi,vbar,vbai
	integer j,p
c
	c = cos(chi)
	s = sin(chi)
	cc = c*c
	ss = s*s
	cs = c*s
	c2 = cc - ss
	s2 = 2*cs
c
	do j=1,8
	  if(circ)then
	    vabr = coeff(1,j,xy)
	    vabi = coeff(2,j,xy)
	    vbar = coeff(1,j,yx)
	    vbai = coeff(2,j,yx)
	    coeff(1,j,xy) = c2*vabr - s2*vabi
	    coeff(2,j,xy) = c2*vabi + s2*vabr
	    coeff(1,j,yx) = c2*vbar + s2*vbai
	    coeff(2,j,yx) = c2*vbai - s2*vbar
	  else
	    do p=1,2
	      vaa = coeff(p,j,xx)
	      vab = coeff(p,j,xy)
	      vba = coeff(p,j,yx)
	      vbb = coeff(p,j,yy)
	      coeff(p,j,xx) = cc*vaa + cs*(vab+vba) + ss*vbb
	      coeff(p,j,xy) = cs*(vbb-vaa) + cc*vab - ss*vba
	      coeff(p,j,yx) = cs*(vbb-vaa) - ss*vab + cc*vba
	      coeff(p,j,yy) = cc*vbb - cs*(vab+vba) + ss*vaa
	    enddo
	  endif
	enddo
c
	end
C************************************************************************
	subroutine polsolve(nsol,nants,npnt,np,gains,
     *	  data,wt,modiqu,qu,chi1,chi2,bl,pid,D,refant,npart,
     *	  circ,qusolve,epsi)
c
	implicit none
	integer nsol,nants,npnt,bl(npnt),pid(npnt),refant,np(nsol)
	integer npart
	complex data(npnt),D(2,nants,npart),gains(2,nants,nsol)
	real wt(npnt),modiqu(3,npnt),qu(2),chi1(npnt),chi2(npnt),epsi
	logical circ,qusolve
c
c  Solve for the leakages.
c
c  Input:
c    nsol	Number of gain solution intervals.
c    np		Number of visibilities in each solution interval.
c    npnt	Number of visibilities.
c    nants	Number of antennas.
c    gains	Antenna gains.
c    npart	Number of leakage stages.
c    data	Visibility data.
c    wt		1/sigma**2 for the visibility data.
c    bl		Baseline number of visibility data.
c    pid	Polarisation index of visibility data.
c    refant	The reference antenna number.
c    modiqu	Model I,Q,U
c    circ	True if the data are (nominally) circularly polarised.
c    qusolve	True if the routine is to solve for Stokes Q and U.
c    chi1	First feed position angle.
c    chi2	Second feed position angle.
c  Input/Output:
c    D		Leakages.
c    qu		Additional Q,U component.
c  Output:
c    epsi	RMS change.
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	integer MAXVAR,X,Y
	parameter(X=1,Y=2)
	parameter(MAXVAR=8*MAXANT-2)
	integer nvar,idx(4,2,MAXANT),idxqu,i,j,pivot(MAXVAR),i1,i2,k,l
	real cqu(2,2),coeff(2,8,npart),acc(MAXVAR,MAXVAR),b(MAXVAR)
	real Dxr,Dxi,Dyr,Dyi
	complex v
c
	integer p1(4),p2(4)
	data p1/1,2,1,2/
	data p2/1,2,2,1/
c
c  Get the upper bound on the number of equations, and zero out the
c  arrays used to accumulate the least squares problem.
c
	nvar = (4*nants-2)*npart
	if(qusolve)nvar = nvar + 2
	nvar = min(nvar,MAXVAR)
	do j=1,nvar
	  do i=1,j
	    acc(i,j) = 0
	  enddo
	  b(j) = 0
	enddo
c
c  Set up variable numbers for the reference antenna and Q,U.
c
	do k=1,nants
	  do j=1,npart
	    do i=1,4
	      idx(i,j,k) = 0
	    enddo
	  enddo
	enddo
	nvar = 0
	do k=1,npart
	  idx(1,k,refant) = -1
	  idx(2,k,refant) = -1
	  if(k.eq.1)then
	    idx(3,k,refant) = nvar + 1
	    idx(4,k,refant) = nvar + 2
	    nvar = nvar + 2
	  else
	    idx(3,k,refant) = -1
	    idx(4,k,refant) = -1
	  endif
	enddo
	idxqu = 0
	if(qusolve)then
	  idxqu = nvar + 1
	  nvar = nvar + 2
	endif
c
c  Loop over all the data.
c
	j = 0
	do l=1,nsol
	  do k=1,np(l)
	    j = j + 1
c
c  Determine the variable numbers for this equation.
c
	    call basant(dble(bl(j)),i1,i2)
	    if(idx(1,1,i1).eq.0)then
	      do i=1,npart
	        idx(1,i,i1) = nvar + 1
	        idx(2,i,i1) = nvar + 2
		nvar = nvar + 2
		if(i.eq.1)then
		  idx(3,i,i1) = nvar + 1
		  idx(4,i,i1) = nvar + 2
		  nvar = nvar + 2
		else
		  idx(3,i,i1) = -1
		  idx(4,i,i1) = -1
		endif
	      enddo
	    endif
	    if(idx(1,1,i2).eq.0)then
	      do i=1,npart
	        idx(1,i,i2) = nvar + 1
	        idx(2,i,i2) = nvar + 2
	        nvar = nvar + 2
		if(i.eq.1)then
		  idx(3,i,i2) = nvar + 1
		  idx(4,i,i2) = nvar + 2
		  nvar = nvar + 2
		else
		  idx(3,i,i2) = -1
		  idx(4,i,i2) = -1
		endif
	      enddo
	    endif
	    if(nvar.gt.MAXVAR)call bug('f','Too many variables')
c
c  Get the coefficients for this equation.
c
	    call geneqn(qu,modiqu(1,j),circ,chi1(j),chi2(j),
     *					  npart,pid(j),coeff)
	    call gencqu(   modiqu(1,j),circ,chi1(j),chi2(j),
     *					  npart,pid(j),cqu)
	    call genvis(nants,i1,i2,pid(j),qu,modiqu(1,j),circ,
     *					chi1(j),chi2(j),D,npart,v)
	    v = data(j)/( 
     *		 gains(p1(pid(j)),i1,l) * conjg(gains(p2(pid(j)),i2,l)))
     *		- v
c
c  Accumulate this pair of equations.
c
	    call doacc(v,cqu,coeff,npart,acc,b,MAXVAR,idxqu,idx(1,1,i1),
     *		idx(1,1,i2),wt(j))
	  enddo
	enddo
c
c  Now solve the problem.
c
	call polinvrt(acc,b,nvar,MAXVAR,pivot)
c
c  Return the rms change.
c
	epsi = 0
	do i=1,nvar
	  epsi = epsi + b(i)*b(i)
	enddo
	epsi = sqrt(epsi/nvar)
c
c  Now unpack the solution and return with the bacon.
c
	if(qusolve)then
	  qu(1) = qu(1) + b(idxqu)
	  qu(2) = qu(2) + b(idxqu+1)
	endif
c
	do j=1,npart
	  do i=1,nants
	    Dxr = 0
	    Dxi = 0
	    Dyr = 0
	    Dyi = 0
	    if(idx(1,j,i).gt.0)Dxr = b(idx(1,j,i))
	    if(idx(2,j,i).gt.0)Dxi = b(idx(2,j,i))
	    if(idx(3,j,i).gt.0)Dyr = b(idx(3,j,i))
	    if(idx(4,j,i).gt.0)Dyi = b(idx(4,j,i))
	    D(X,i,j) = D(X,i,j) + cmplx(Dxr,Dxi)
	    D(Y,i,j) = D(Y,i,j) + cmplx(Dyr,Dyi)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine doacc(v,cqu,coeff,npart,acc,b,nvar,
     *		idxqu,idx1,idx2,wt)
c
	implicit none
	integer nvar,npart,idxqu,idx1(4,npart),idx2(4,npart)
	real cqu(2,2),coeff(2,8,npart),wt
	complex v
	real acc(nvar,nvar),b(nvar)
c
c  Accumulate the least squares matrix.
c------------------------------------------------------------------------
	integer MAXNV
	parameter(MAXNV=18)
	real c(MAXNV,2)
	integer idx(MAXNV),nv,i,j,id,jd
c
	nv = 0
	if(idxqu.ne.0)then
	  c(nv+1,1) = cqu(1,1)
	  c(nv+2,1) = cqu(1,2)
	  c(nv+1,2) = cqu(2,1)
	  c(nv+2,2) = cqu(2,2)
	  idx(nv+1) = idxqu
	  idx(nv+2) = idxqu + 1
	  nv = nv + 2
	endif
	do j=1,npart
	  do i=1,4
	    if(idx1(i,j).gt.0)then
	      c(nv+1,1) = coeff(1,i,j)
	      c(nv+1,2) = coeff(2,i,j)
	      idx(nv+1) = idx1(i,j)
	      nv = nv + 1
	    endif
	    if(idx2(i,j).gt.0)then
	      c(nv+1,1) = coeff(1,i+4,j)
	      c(nv+1,2) = coeff(2,i+4,j)
	      idx(nv+1) = idx2(i,j)
	      nv = nv + 1
	    endif
	  enddo
	enddo
c
	if(nv.gt.MAXNV)call bug('f','Assertion failed in doacc')
c
	do j=1,nv
	  do i=1,nv
	    if(idx(i).le.idx(j))then
	      id = idx(i)
	      jd = idx(j)
	      acc(id,jd) = acc(id,jd) + (c(i,1)*c(j,1)+c(i,2)*c(j,2))*wt
	    endif
	  enddo
	  b(idx(j)) = b(idx(j)) +(  real(v)*c(j,1)
     *				+  aimag(v)*c(j,2) ) * wt
	enddo
c
	end
c************************************************************************
	subroutine polinvrt(acc,b,nvar,maxvar,pivot)
c
	implicit none	
	integer nvar,maxvar,pivot(nvar)
	real acc(maxvar,nvar),b(nvar)
c
c------------------------------------------------------------------------
	integer i,j,ifail
c
c  Reflect the other part of the symmetric matrix.
c
	do j=2,nvar
	  do i=1,j-1
	    acc(j,i) = acc(i,j)
	  enddo
	enddo
c
c  Solve.
c
	call sgefa(acc,maxvar,nvar,pivot,ifail)
	if(ifail.ne.0)call bug('f',
     *	  'Matrix inversion failed- singular matrix?')
	call sgesl(acc,maxvar,nvar,pivot,b,1)
c
	end
