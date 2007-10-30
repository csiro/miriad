c************************************************************************
	program blcal
	implicit none
c
c= blcal - Compute and apply baseline calibration.
c& rjs
c: uv analysis
c+
c	BLCAL computes and applies baseline-based calibration to a visibility
c	dataset.
c
c	This reads two datasets. It uses the first dataset to determine
c	baseline-based calibration, and applies these calibration corrections
c	to the second dataset.
c@ vis
c	Normally, this gives the name of two visibility datasets, being
c	the "reference" and the "source" datasets respectively. The
c	reference dataset is used to determine the baseline calibration,
c	whereas the source contains the data that is corrected and written
c	out. If only a single dataset is given, then this is self-calibrated.
c	Baseline-based self-calibration is a very dubious operation, and
c	generally should not be performed.
c@ select
c	The normal uv selection commands -- see the help on "select" for
c	more information. This selection is applied to both the reference
c	and source input datasets.
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	See the help on "line" for more information.
c	The default is all channels (or all wide channels if there are no
c	spectral channels). The output will consist of only spectral or
c	wideband data (but not both).
c@ stokes
c	Normal Stokes/polarisation processing. See the help on "stokes"
c	for more information. The default is to use the Stokes/polarisations
c	present in the dataset.
c@ interval
c	Solution time interval, in minutes. The default is 5 minutes.
c@ out
c	The name of the output uv data set. No default.
c--
c  History:
c    rjs  14mar97 Original version.
c    rjs  17mar97 Enhanced version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='BlCal: version 1.0 17-Mar-97')
	character out*64,line*32
	integer lVis,lRef,lOut
	integer nchan,pol,npol,nfiles
	double precision interval,preamble(6)
	complex data(MAXCHAN)
	logical flags(MAXCHAN),self
c
c  Externals.
c
	logical uvDatOpn
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call uvDatInp('vis','sdlwcef3')
	call keyd('interval',interval,5.d0)
	if(interval.le.0)call bug('f','Invalid value for interval')
	interval = interval/(60.*24.)
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
	call keyfin
c
c  Check the number of input files.
c
	call uvDatGti('nfiles',nfiles)
	self = nfiles.eq.1
	if(nfiles.ne.1.and.nfiles.ne.2)call bug('f',
     *	  'Either one or two input datasets must be given')
	if(self)call bug('w','Baseline-based self-calibration '//
     *	  'is a rather dangerous/dubious operation')
c
c  Open the reference, get the solution, and close.
c
	call output('Calculating the baseline gains ...')
	if(.not.uvDatOpn(lRef))call bug('f',
     *	  'Error opening reference dataset')
	call datLoad(interval)
	call uvDatCls
	call datNorm
c
c  Open the input and output.
c
	if(self)call uvDatRew
	call output('Applying the baseline gains ...')
	if(.not.uvDatOpn(lVis))call bug('f',
     *	  'Error opening source dataset')
	call uvDatGta('ltype',line)
	call varInit(lVis,line)
	call uvopen(lOut,out,'new')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varOnit(lVis,lOut,line)
c
	call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
	  call uvDatGti('npol',npol)
	  if(npol.le.0)call bug('f',
     *	    'Unable to determine number of polarisations')
	  call uvDatGti('pol',pol)
	  preamble(6) = pol
	  call DatCorr(preamble(4),data,flags,nchan)
	  call varCopy(lVis,lOut)
	  if(pol.ne.0)then
	    call uvputvri(lOut,'pol',pol,1)
	    call uvputvri(lOut,'npol',npol,1)
	  endif
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call hdcopy(lVis,lOut,'history')
	call hisOpen(lOut,'append')
	call hisWrite(lOut,'BLCAL: Miriad '//version)
	call hisInput(lOut,'BLCAL')
	call hisClose(lOut)
c
	call uvDatCls
	call uvclose(lOut)
	end
c************************************************************************
	subroutine datCorr(co,data,flags,nchan1)
c
	implicit none
	integer nchan1
	double precision co(3)
	complex data(nchan1)
	logical flags(nchan1)
c------------------------------------------------------------------------
	include 'blcal.h'
	integer pol,bl,i1,i2,i
	logical more
c
	if(nchan.ne.nchan1)call bug('f','Channel mismatch')
c
	pol = nint(co(3)) + 9
	if(pol.le.0.or.pol.gt.MAXPOL)
     *	  call bug('f','Invalid polarisation')
	call basant(co(2),i1,i2)
	bl = ((i2-1)*i2)/2 + i1
c
c  Search for the appropriate solution.
c
	i1 = head(pol,bl)
	i2 = i1
	more = .true.
	dowhile(i2.ne.0.and.more)
	  if(time(i1).gt.co(1).or.co(1).gt.time(i2))then
	    i1 = i2
	    i2 = next(i2)
	  else
	    more = .false.
	  endif
	enddo
c
c  If one was found, then apply it.
c
	if(.not.more)then
	  call datApply(memc(gidx(i1)),memi(fidx(i1)),time(i1),
     *			memc(gidx(i2)),memi(fidx(i2)),time(i2),
     *			data,flags,co(1),nchan)
	else
	  do i=1,nchan
	    flags(i) = .false.
	  enddo
	endif
c
	end
c************************************************************************
	subroutine datApply(vis1,cnt1,t1,vis2,cnt2,t2,
     *			data,flags,time,nchan)
c
	implicit none
	integer nchan
	double precision t1,t2,time
	complex vis1(nchan),vis2(nchan),data(nchan)
	integer cnt1(nchan),cnt2(nchan)
	logical flags(nchan)
c
c------------------------------------------------------------------------
	integer i
	real a,b
	complex temp
c
	if(time.eq.t1)then
	  do i=1,nchan
	    if(cnt1(i).gt.0)then
	      data(i) = data(i)/vis1(i)
	    else
	      flags(i) = .false.
	    endif
	  enddo
	else if(time.eq.t2)then
	  do i=1,nchan
	    if(cnt2(i).gt.0)then
	      data(i) = data(i)/vis2(i)
	    else
	      flags(i) = .false.
	    endif
	  enddo
	else
	  a = 1 - (time-t1)/(t2-t1)
	  b = 1 - a
	  do i=1,nchan
	    if(cnt1(i)*cnt2(i).gt.0)then
	      temp = a*vis1(i) + b*vis2(i)
	      data(i) = data(i)/temp
	    else
	      flags(i) = .false.
	    endif
	  enddo
	endif
c
	end
c************************************************************************
	subroutine datLoad(interval)
c
	implicit none
	double precision interval
c------------------------------------------------------------------------
	include 'blcal.h'
	double precision co(5)
	integer pol,bl,pmin,pmax,bmin,bmax,nread,ngood,i,j,i1,i2
	integer Tail(MAXPOL,MAXBASE)
	logical buffered
	double precision t,tmin,tmax
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
c
c  Initialise
c
	nsols = 0
	do j=1,MAXBASE
	  do i=1,MAXPOL
	    Head(i,j) = 0
	    Tail(i,j) = 0
	  enddo
	enddo
c
	call uvDatRd(co,data,flags,MAXCHAN,nread)
	nchan = nread
	buffered = .false.
	tmin = 0
	tmax = 0
	dowhile(nchan.eq.nread)
	  ngood = 0
	  do i=1,nchan
	    if(flags(i))ngood = ngood + 1
	  enddo
	  if(ngood.gt.0)then
c
c  Get the parameters of this record.
c
	    call uvDatGti('pol',pol)
	    pol = pol + 9
	    if(pol.le.0.or.pol.gt.MAXPOL)
     *	      call bug('f','Invalid polarisation')
	    call basant(co(5),i1,i2)
	    bl = ((i2-1)*i2)/2 + i1
	    t = co(4)
c
c  Is this the end of an integration.
c
	    if(buffered.and.(t-tmin.gt.interval.or.
     *			     tmax-t.gt.interval))then
	      call datFlush(Tail,MAXPOL,MAXBASE,pmin,pmax,bmin,bmax)
	      buffered = .false.
	    endif
c
	    if(tail(pol,bl).eq.0)then
	      nsols = nsols + 1
	      tail(pol,bl) = nsols
	      call memAlloc(gidx(nsols),nchan,'c')
	      call ZeroC(memc(gidx(nsols)),nchan)
	      call memAlloc(fidx(nsols),nchan,'i')
	      call ZeroI(memi(fidx(nsols)),nchan)
	      time(nsols) = 0
	    endif
	    i = tail(pol,bl)
c
c  Add in this solution.
c
 	    call datAdd(memc(gidx(i)),memi(fidx(i)),time(i),
     *		data,flags,co(4),nchan)
c
c  Book keeping.
c	
	    if(.not.buffered)then
	      pmin = pol
	      pmax = pmin
	      bmin = bl
	      bmax = bmin
	      tmin = t
	      tmax = tmin
	    else
	      pmin = min(pmin,pol)
	      pmax = max(pmax,pol)
	      bmin = min(bmin,bl)
	      bmax = max(bmax,bl)
	      tmin = min(tmin,t)
	      tmax = max(tmax,t)
	    endif
	    buffered = .true.
	  endif
c
c  Go back for more.
c
	  call uvDatRd(co,data,flags,MAXCHAN,nread)
	enddo
	if(nread.ne.0)call bug('f','Number of channels changed')
c
	if(buffered)call datFlush(Tail,MAXPOL,MAXBASE,pmin,pmax,
     *							bmin,bmax)
c
	end
c************************************************************************
	subroutine datFlush(Tail,MpOL,MBASE,pmin,pmax,bmin,bmax)
c
	implicit none
	integer MPOL,MBASE,pmin,pmax,bmin,bmax
	integer Tail(MPOL,MBASE)
c------------------------------------------------------------------------
	include 'blcal.h'
	integer b,p,i
c
	do b=bmin,bmax
	  do p=pmin,pmax
	    i = Tail(p,b)
	    if(i.ne.0)call datLink(Head(p,b),i)
	    Tail(p,b) = 0
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine datNorm
c
	implicit none
c------------------------------------------------------------------------
	include 'blcal.h'
	double precision Sum
	integer cnt,p,b,pnt
	real g
c
	Sum = 0
	Cnt = 0
	do b=1,MAXBASE
	  do p=1,MAXPOL
	    pnt = Head(p,b)
	    dowhile(pnt.ne.0)
	      call datNorm1(memc(gidx(pnt)),memi(fidx(pnt)),
     *					time(pnt),nchan,Sum,Cnt)
	      pnt = Next(pnt)
	    enddo
	  enddo
	enddo
c
	if(cnt.eq.0)call bug('f','No good data found')
c
	g = sqrt(Cnt/Sum)
	do b=1,MAXBASE
	  do p=1,MAXPOL
	    pnt = Head(p,b)
	    dowhile(pnt.ne.0)
	      call datNorm2(memc(gidx(pnt)),nchan,g)
	      pnt = Next(pnt)
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine datNorm1(vis,cnt,time,nchan,Sum,SumCnt)
c
	implicit none
	integer nchan,cnt(nchan),SumCnt
	complex vis(nchan)
	double precision time,Sum
c------------------------------------------------------------------------
	integer i,n
c
	n = 0
	do i=1,nchan
	  n = n + cnt(i)
	  if(cnt(i).gt.0)then
	    sum = sum + 
     *		(real(vis(i))**2 + aimag(vis(i))**2)/cnt(i)
	    vis(i) = vis(i) / cnt(i)
	  else
	    vis(i) = 1
	  endif
	enddo
	SumCnt = SumCnt + n
	time = time / n
c
	end
c************************************************************************
	subroutine datNorm2(vis,nchan,g)
c
	implicit none
	integer nchan
	complex vis(nchan)
	real g
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  vis(i) = g*vis(i)
	enddo
c
	end
c************************************************************************
	subroutine datLink(Hd,pnt)
c
	implicit none
	integer Hd,pnt
c------------------------------------------------------------------------
	include 'blcal.h'
	integer i
c
	if(hd.eq.0)then
	  hd = pnt
	else
	  i = hd
	  dowhile(next(i).ne.0)
	    i = next(i)
	  enddo
	  next(i) = pnt
	endif
c
	end
c************************************************************************
	subroutine datAdd(vis,cnt,t,data,flags,time,nchan)
c
	implicit none
	integer nchan
	double precision t,time
	logical flags(nchan)
	complex vis(nchan),data(nchan)
	integer cnt(nchan)
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  if(flags(i))then
	    t = t + time
	    vis(i) = vis(i) + data(i)
	    cnt(i) = cnt(i) + 1
	  endif
	enddo
c
	end
c************************************************************************
	subroutine zeroc(data,n)
c
	implicit none
	integer n
	complex data(n)
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  data(i) = 0
	enddo
	end
c************************************************************************
	subroutine zeroi(data,n)
c
	implicit none
	integer n
	integer data(n)
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  data(i) = 0
	enddo
	end
