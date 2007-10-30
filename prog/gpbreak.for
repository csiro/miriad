c************************************************************************
	program gpbreak
	implicit none
c
c= GpBreak -- Insert a break point in the gain table.
c& rjs
c: calibration
c+
c	GpBreak is a MIRIAD task which modifies a gain table to include a
c	break point. This prevents normal interpolation of gains across
c	this point.
c@ vis
c	The input visibility file, containing the gain file to modify.
c	No default.
c@ break
c	The times, in the normal Miriad format, where to insert the
c	break point. Several times can be given. No default.
c@ ants
c	The antennas affected. The default is all antennas.
c@ feeds
c	The polarisation feeds affected (e.g. R, L, X or Y). Default is
c	all feeds.
c
c	Example:
c	  gpbreak vis=cyga times=20:30,21:15 ants=1,2 feeds=X
c--
c  History:
c    rjs      4sep91 Original version.
c    rjs     16sep91 Fixed bugs which affected applying multiple breaks at
c		     a time.
c    rjs     30aug93 Increase MAXTIMES.
c    rjs     16sep93 Rename bsrch to binsrch.
c    rjs     13oct93 Changed the keyword "times" to "break".
c    rjs     25nov93 Use library version of mkeyt.
c    rjs     31jan97 Fix "feeds" keyword, which could never have worked.
c    rjs      5mar97 Check that there are no delays in the gain table.
c  Bugs and Shortcomings:
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXFEED,MAXTIME,MAXSOLN
	character version*(*)
	parameter(version='GpBreak: version 1.0 05-Mar-97')
	parameter(MAXFEED=2,MAXTIME=64,MAXSOLN=4096)
c
	character vis*64
	integer iostat,tVis,itGain,nants,nfeeds,nsols,i
	integer numtime,numant,numfeed,ntau
	double precision btimes(MAXTIME),times(MAXSOLN)
	integer ants(MAXANT),feeds(MAXFEED)
	complex gains(2*MAXANT*MAXSOLN)
	logical mask(2*MAXANT)
c
c  Externals.
c
	character itoaf*8
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call mkeyt('break',btimes,MAXTIME,numtime,'time')
	call mkeyi('ants',ants,MAXANT,numant)
	call mkeyfd('feeds',feeds,MAXFEED,numfeed)
	call keyfin
	if(vis.eq.' ')call bug('f','No input vis data-set given')
	if(numtime.eq.0)call bug('f','No break times given')
c
c  Open the input file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call BreakBug(iostat,'Error opening '//vis)
c
c  Determine the number of things in the gain table.
c
	call rdhdi(tVis,'ngains',nants,0)
	call rdhdi(tVis,'nfeeds',nfeeds,1)
	if(nfeeds.le.0.or.nfeeds.gt.2.or.nants.lt.nfeeds.or.
     *	    mod(nants,nfeeds).ne.0)
     *	  call bug('f','Bad number of gains or feeds in '//vis)
	call rdhdi(tVis,'ntau',ntau,0)
	if(ntau.ne.0)call bug('f',
     *	  'Cannot deal with files with delays')
	nants = nants / nfeeds
	call rdhdi(tVis,'nsols',nsols,0)
	if(nsols.le.0)
     *	  call bug('f','Bad number of solutions')
c
c  See if we have enough space.
c
	if(nants.gt.MAXANT)
     *	  call bug('f','Too many antennae for me to cope with')
	if(nsols+numtime.gt.MAXSOLN)
     *	  call bug('f','Too many solution intervals for my small brain')
c
c  Check the given antenna numbers, and set the default antenna numbers
c  if needed.
c
	if(numant.gt.0)then
	  do i=1,numant
	    if(ants(i).lt.1.or.ants(i).gt.nants)
     *	      call bug('f','Invalid antenna number: '//itoaf(ants(i)))
	  enddo
	else
	  do i=1,nants
	    ants(i) = i
	  enddo
	  numant = nants
	endif
c
c  Check the given feed numbers, and set the default feed numbers if needed.
c
	if(numfeed.gt.0)then
	  do i=1,numfeed
	    if(feeds(i).gt.nfeeds)then
	      call bug('f','Invalid feeds specified.')
	      call bug('f','Gain table has only one feed')
	    endif
	  enddo
	else
	  do i=1,nfeeds
	    feeds(i) = i
	  enddo
	  numfeed = nfeeds
	endif
c
c  Set up the breakpoint mask.
c
	call SetMask(nfeeds,nants,mask,feeds,numfeed,ants,numant)
c
c  Open the gains file. Mode=='append' so that we can overwrite it.
c
	call haccess(tVis,itGain,'gains','append',iostat)
	if(iostat.ne.0)call BreakBug(iostat,'Error accessing gains')
c
c  Read the gains.
c
	call GainRd(itGain,nsols,nants,nfeeds,times,Gains)
c
c  Sort the times, and delete unnecessary ones.
c
	call tsort(times,nsols,btimes,numtime)
c
c  Edit the gains.
c
	call GainEdt(nsols+numtime,nsols,nants*nfeeds,times,Gains,
     *			mask,btimes,numtime)
c
c  Write out the gains.
c
	call wrhdi(tVis,'nsols',nsols)
	call GainWr(itGain,nsols,nants,nfeeds,times,Gains)
c
c  Write out some history now.
c
	call hisopen(tVis,'append')
	call hiswrite(tVis,'GPBREAK: Miriad '//version)
	call hisinput(tVis,'GPBREAK')
	call hisclose(tVis)
c
c  Close up everything.
c
	call hdaccess(itGain,iostat)
	call hclose(tVis)	
	end
c************************************************************************
	subroutine SetMask(nfeeds,nants,mask,feeds,numfeed,ants,numant)
c
	implicit none
	integer nfeeds,nants,numfeed,numant
	integer feeds(numfeed),ants(numant)
	logical mask(nfeeds,nants)
c
c  Set up the breakpoint mask.
c
c  Input:
c    nfeeds
c    nants
c    feeds
c    numfeed
c    ants
c    numant
c  Output:
c    mask
c------------------------------------------------------------------------
	integer i,j,i0,j0
c
	do j=1,nfeeds
	  do i=1,nants
	    mask(j,i) = .true.
	  enddo
	enddo
c
	do j=1,numfeed
	  j0 = feeds(j)
	  do i=1,numant
	    i0 = ants(i)
	    mask(j0,i0) = .false.
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GainRd(itGain,nsols,nants,nfeeds,times,Gains)
c
	implicit none
	integer itGain,nsols,nants,nfeeds
	complex Gains(nfeeds*nants,nsols)
	double precision times(nsols)
c
c  Read the gains from the gains table.
c
c  Input:
c    itGain	The item handle of the gains table.
c    nsols	Number of solutions.
c    nants	Number of antennae
c    nfeeds	Number of feeds.
c  Output:
c    times	The read times.
c    gains	The gains.
c------------------------------------------------------------------------
	integer offset,iostat,k
c
	offset = 8
	do k=1,nsols
	  call hreadd(itGain,times(k),offset,8,iostat)
	  if(iostat.ne.0)call BreakBug(iostat,'Error reading gain time')
	  offset = offset + 8
	  call hreadr(itGain,Gains(1,k),offset,8*nfeeds*nants,iostat)
	  if(iostat.ne.0)call BreakBug(iostat,'Error reading gains')
	  offset = offset + 8*nfeeds*nants
	enddo
	end
c************************************************************************
	subroutine GainEdt(maxsols,nsols,nants,times,Gains,mask,
     *			btimes,numtime)
c
	implicit none
	integer maxsols,nsols,nants,numtime
	complex Gains(nants,maxsols)
	double precision times(maxsols),btimes(numtime)
	logical mask(nants)
c
c  Edit the gains. This consists of putting in extra gains at the break
c  points. The gains for the antennae/feeds with the break point are
c  set as bad (zeroed). The other unaffected gains are linearly
c  interpolated.
c
c  Input:
c    maxsols	Max number of solutions.
c    nants	Number of antennae times the number of feeds.
c    btimes	Times for the breakpoints.
c    numtime	Number of times.
c    ants	Ants to apply the breakpoint to.
c    numant	Number of antennae.
c    feeds	Feeds to apply the breakpoints to.
c    numfeed	Number of feeds.
c  Input/Output:
c    nsols	Number of solutions.
c    times	The read times.
c    gains	The gains.
c------------------------------------------------------------------------
	integer kout,kin,kb
c
	kb = numtime
	kout = nsols + numtime
	kin = nsols
c
	dowhile(kb.gt.0)
	  dowhile(kin.gt.0.and.times(kin).gt.btimes(kb))
	    call BrMove(gains(1,kin),gains(1,kout),nants)
	    times(kout) = times(kin)
	    kin = kin - 1
	    kout = kout - 1
	  enddo
c
	  times(kout) = btimes(kb)
c
	  if(kout.eq.nsols+numtime)then
	    call BrFlag(gains(1,kin),mask,gains(1,kout),nants)
	  else if(kin.eq.0)then
	    call BrFlag(gains(1,kout+1),mask,gains(1,kout),nants)
	  else
	    call BrInterp(times(kin),gains(1,kin),
     *			  times(kout+1),gains(1,kout+1),
     *			  times(kout),gains(1,kout),mask,nants)
	  endif
	  kout = kout - 1
	  kb = kb - 1
	enddo
c
c  Correct the number of solutions.
c
	nsols = nsols + numtime
c
	end
c************************************************************************
	subroutine BrInterp(t1,G1,t2,G2,tout,Gout,mask,nants)
c
	implicit none
	integer nants
	logical mask(nants)
	double precision t1,t2,tout
	complex G1(nants),G2(nants),Gout(nants)
c
c  Interpolate between the two gain steps, and apply the mask. Before
c  we interpolate, we determine a phase factor which makes G1 as much
c  like G2 (in a least squares sense), so that we can avoid the problem
c  of the two solutions possibly having different reference antennae.
c
c  Input:
c    t1,t2
c    G1,G2
c    mask
c    nants
c    tout
c  Output:
c    Gout
c------------------------------------------------------------------------
	integer i,n
	complex Sum12,alpha1,alpha2
	real Sum22
	logical good1,good2
c
c  Determine a phase factor, alpha, which makes G1 = alpha*G2 (approx).
c
	n = 0
	Sum22 = 0
	Sum12 = (0.,0.)
	do i=1,nants
	  good1 = abs(real(G1(i)))+abs(aimag(G1(i))).gt.0
	  good2 = abs(real(G2(i)))+abs(aimag(G2(i))).gt.0
	  if(mask(i).and.good1.and.good2)then
	    Sum12 = Sum12 + G1(i)*conjg(G2(i))
	    Sum22 = Sum22 + real(G2(i))**2 + aimag(G2(i))**2
	    n = n + 1
	  endif
	enddo
c
	if(n.gt.0) then
	  alpha1 = (1.,0.)
	  alpha2 = Sum12 / Sum22
	  alpha2 = alpha2 / abs(alpha2)
	else if(abs(tout-t1).lt.abs(t2-tout))then
	  alpha1 = (1.,0.)
	  alpha2 = (0.,0.)
	else
	  alpha1 = (0.,0.)
	  alpha2 = (1.,0.)
	endif
c
c  We have a value of alpha. Now linearly interpolate between
c  G1 and alpha*G2.
c
	do i=1,nants
	  Gout(i) = (0.,0.)
	  if(mask(i))then
	    good1 = abs(real(G1(i)))+abs(aimag(G1(i))).gt.0
	    good2 = abs(real(G2(i)))+abs(aimag(G2(i))).gt.0
	    if(good1.and.good2)then
	     Gout(i) = ((tout-t1)*alpha2*G2(i) - (tout-t2)*alpha1*G1(i))
	     Gout(i) = Gout(i) / (t2 - t1)
	    else if(good1)then
	      Gout(i) = alpha1 * G1(i)
	    else if(good2)then
	      Gout(i) = alpha2 * G2(i)
	    endif
	  endif
	enddo
c
	end	  
c************************************************************************
	subroutine BrFlag(Gin,mask,Gout,nants)
c
	implicit none
	integer nants
	complex Gin(nants),Gout(nants)
	logical mask(nants)
c
c  Copy across the gains.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nants
	  if(mask(i))then
	    Gout(i) = Gin(i)
	  else
	    Gout(i) = (0.,0.)
	  endif
	enddo
	end
c************************************************************************
	subroutine BrMove(Gin,Gout,nants)
c
	implicit none
	integer nants
	complex Gin(nants),Gout(nants)
c
c  Copy across the gains.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nants
	  Gout(i) = Gin(i)
	enddo
	end
c************************************************************************
	subroutine GainWr(itGain,nsols,nants,nfeeds,times,Gains)
c
	implicit none
	integer itGain,nsols,nants,nfeeds
	complex Gains(nfeeds*nants,nsols)
	double precision times(nsols)
c
c  Write the gains from the gains table.
c
c  Input:
c    itGain	The item handle of the gains table.
c    nsols	Number of solutions.
c    nants	Number of antennae
c    nfeeds	Number of feeds.
c    times	The read times.
c    gains	The gains.
c------------------------------------------------------------------------
	integer offset,iostat,k
c
	offset = 8
	do k=1,nsols
	  call hwrited(itGain,times(k),offset,8,iostat)
	  if(iostat.ne.0)call BreakBug(iostat,'Error writing gain time')
	  offset = offset + 8
	  call hwriter(itGain,Gains(1,k),offset,8*nfeeds*nants,iostat)
	  if(iostat.ne.0)call BreakBug(iostat,'Error writing gains')
	  offset = offset + 8*nfeeds*nants
	enddo
	end
c************************************************************************
	subroutine mkeyfd(keyw,feeds,maxfeeds,nfeeds)
c
	implicit none
	character keyw*(*)
	integer maxfeeds,nfeeds
	integer feeds(maxfeeds)
c
c  This gets feed codes from the user (i.e. 'r','l','x' or 'y'), and
c  converts them to 1 (for 'r' and 'x') or 2 (for 'l' and 'y').
c
c  Input:
c    keyw	Keyword to use in calling keya.
c    maxfeeds	Max number of feeds to return.
c  Output:
c    feeds	The given feeds.
c    nfeeds	The number of feeds retrieved.
c------------------------------------------------------------------------
	character string*4
	logical more
	integer i
	integer nallfds
	parameter(nallfds=8)
	character allfds(nallfds)
	integer codes(nallfds)
c
c  Externals.
c
	integer binsrcha
	logical keyprsnt
c
c  Data statements.
c
	data allfds/'L','R','X','Y','l','r','x','y'/
	data codes / 2 , 1 , 1 , 2 , 2 , 1 , 1 , 2 /
c
	nfeeds = 0
	more = keyprsnt(keyw)
	dowhile(nfeeds.lt.maxfeeds.and.more)
	  call keya(keyw,string,' ')
	  i = binsrcha(string,allfds,nallfds)
	  if(i.eq.0)call bug('f','Unrecognised feed mnemonic: '//string)
	  nfeeds = nfeeds + 1
	  feeds(nfeeds) = codes(i)
	  more = keyprsnt(keyw)
	enddo
c
	if(more)call bug('f','Too many feeds given')
	end
c************************************************************************
	subroutine tsort(times,ntimes,btimes,numtime)
c
	implicit none
	integer ntimes,numtime
	double precision times(ntimes),btimes(numtime)
c
c  Convert the times to absolute times (rather than possibly daytimes),
c  and sort them into ascending order.
c
c  Input:
c    ntimes		The number of solution times.
c    times		The solution times.
c  Input/Output:
c    btimes		The times to be converted and sorted.
c    numtime		The number of btimes.
c------------------------------------------------------------------------
	integer i,j,k
	double precision t,toff,tbase
c
c  Convert to absolute time.
c
	tbase = nint(times(1)) - 0.5
	toff  = times(1) - tbase
	do i=1,numtime
	  if(btimes(i).ge.0.and.btimes(i).le.1)then
	    if(btimes(i).lt.toff)then
	      btimes(i) = btimes(i) + tbase + 1
	    else
	      btimes(i) = btimes(i) + tbase
	    endif
	  endif
	enddo
c
c  Now sort the times. Do an insert sort, because I am too lazy to do
c  anything else.
c
	do j=2,numtime
	  t = btimes(j)
	  k = j
	  dowhile(k.gt.1.and.btimes(k-1).gt.t)
	    btimes(k) = btimes(k-1)
	    k = k - 1
	  enddo
	  btimes(k) = t
	enddo
c
	end
c************************************************************************
	subroutine BreakBug(iostat,message)
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
