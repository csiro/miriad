c************************************************************************
	program uvaver
	implicit none
c
c= uvaver - Copy and average a uv dataset.
c& rjs
c: uv analysis
c+
c	UVAVER copies a uv dataset, performing averaging, both in time
c	and/or frequency.
c@ vis
c	The name of the input uv data sets. Several can be given (wild
c	cards are supported). No default.
c@ select
c	The normal uv selection commands. The default is copy everything.
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	The default is all channels (or all wide channels if there are no
c	spectral channels). The output will consist of only spectral or
c	wideband data (but not both).
c@ ref
c	The normal reference linetype, in the form:
c	  line,start,width
c	The default is no reference line.
c@ stokes
c	If a value is given, uvaver converts the input into the required
c	polarizations before writing to the output. Default is to copy
c	across the polarizations present in the input files.
c@ interval
c	Time averaging interval, in minutes. The default is 0 (i.e. no
c	averaging).
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreivated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   nocal       Do not apply the gains file. By default, UVAVER
c	               applies the gains file in copying the data.
c	   nopass      Do not apply bandpass corrections. By default,
c	               UVAVER corrects for the bandpass shape if the
c	               required information is available.
c	   nopol       Do not apply polarizatiopn corrections. By default
c	               UVAVER corrects for polarization corss-talk.
c	   relax       Normally UVAVER discards a correlation record if
c	               all the correlations are bad. This option causes
c	               UVAVER to retain all records.
c	   vector      Means do vector averaging.  This is the default.
c	   scalar      Means do scalar averaging for the amplitudes
c	               and vector averaging for the phase.
c	   scavec      Means do scalar averaging for the amplitudes of
c	               the  `parallel-hand' polarisations (i,xx,yy,rr,ll).
c	               Vector averaging is used for the amplitudes of
c	               the cross-hand visibilities (q,u,v,xy,yx,rl,lr)
c	               and for the phases for all visibilities.
c@ out
c	The name of the output uv data set. No default.
c--
c  History:
c    mchw 30nov90 Tentative version.
c    rjs  19jun91 Original version.
c    rjs   5jul91 Fixed bug in the uvset(...,'wide',...) call.
c    rjs  18jul91 Fixed nuerous bugs dealing with time averaging.
c    rjs  29aug91 Fiddled copying of systemp in WindUpd.
c    rjs   3oct91 Fiddles to writing out the wide and channel descriptions.
c    rjs  18oct91 Copies the "freq" variable.
c    rjs  13dec91 Deleted obstype processing (now done in uvio).
c    mchw 31dec91 Copied across 'airtemp' and 'tpower' variables.
c    nebk 26feb92 Add ampscalar averaging
c    rjs  27feb92 Better attempt at things coming out in time order.
c    nebk 28feb92 Rearranged rjs' rearrangement of my amp-scalar
c                 vector computation for more clarity
c    rjs  15jun92 General cleanup. Use Var routines.
c    rjs  10jul92 Fixed unimaginable bug. Also fiddles with the doc.
c    rjs   4aug92 Added nopass option.
c    rjs  17aug92 Call sequence change to var routines.
c    rjs  18sep92 Handle autocorrelation data correctly.
c    rjs  29jan93 Added vecamp option.
c    nebk/rjs
c         01feb93 Rename vector/scalar options to something consistent
c    rjs  10feb93 Cosmetic change to history comments.
c    rjs  25sep93 Check when we run out of baseline slots in bufacc.
c    rjs  19oct93 Check that data is read.
c    rjs  23sep93 W axis change.
c    rjs  10oct94 relax option.
c    rjs  24oct94 Weight data (in time averaging) according to integration
c		  time.
c    rjs  19sep95 Handle data that is not quite in time order.
c    rjs  21sep95 Really do it this time.
c    rjs  14dec95 Increase buffer in averaging (MAXAVER).
c    rjs  14jun96 Add warning about time averaging pulsar bin data.
c    rjs  10feb97 Improve averaging in the face of bad, out-of-sequence, data.
c    rjs  10oct97 Eliminate incorrect call to uvvarcopy just before the
c		  final buffer flush.
c    rjs  23oct97 Do not average across changes in the "on" variable.
c    mchw 02jan98 Increase buffer in averaging (MAXAVER=163840).
c
c  Bugs:
c    * The way of determining whether a source has changed is imperfect.
c    * This can write out either massaged channels, or massaged wides,
c      but not both simultaneously.
c    * Too much of this code worries about polarisations.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='UvAver: version 1.0 02-jan-98')
	character uvflags*12,ltype*16,out*64
	integer npol,Snpol,pol,tIn,tOut,vupd,nread,nrec,i,nbin
	real inttime
	logical dotaver,doflush,buffered,PolVary,ampsc,vecamp,first
	logical relax,ok,donenpol
	double precision preamble(5),Tmin,Tmax,Tprev,interval
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
c
c  Externals.
c
	logical uvDatPrb,uvDatOpn,uvVarUpd
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags,ampsc,vecamp,relax)
	call uvDatInp('vis',uvflags)
	call keyd('interval',interval,0.d0)
	call keya('out',out,' ')
	call keyfin
c
c  Check the input parameters.
c
	if(out.eq.' ')call bug('f','Output file must be specified')
	if(interval.lt.0)call bug('f','Illegal value for interval')
        if(ampsc)call output('Amp-scalar averaging used')
	if(vecamp)
     *	  call output('Amp-scalar averaging on parallel hands used')
c
c  Various initialisation.
c
	interval = interval/(24.*60.)
	Snpol = 0
	first = .true.
	PolVary = .false.
	doflush = .false.
	buffered = .false.
	donenpol = .false.
	dotaver = interval.gt.0.or.uvDatPrb('polarization?',0.d0)
	call BufIni
	nrec = 0
c
c  Open the input and the output files.
c
	dowhile(uvDatOpn(tIn))
	  nbin = 1
	  if(dotaver)then
	    call uvrdvri(tIn,'nbin',nbin,1)
	    if(nbin.gt.1)then
	      call bug('w',
     *	      'Time averaging or pol''n selection of bin-mode data')
	      call bug('w',
     *	      'This will average all bins together')
	    endif
	  endif
	  call uvDatGta('ltype',ltype)
	  call VarInit(tIn,ltype)
	  call uvVarIni(tIn,vupd)
	  call uvVarSet(vupd,'dra')
	  call uvVarSet(vupd,'ddec')
	  call uvVarSet(vupd,'source')
	  call uvVarSet(vupd,'on')
c
c Special processing the first time around.
c
	  if(first)then
	    call uvopen(tOut,out,'new')
	    call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	    call hdcopy(tIn,tOut,'history')
	    call hisopen(tOut,'append')
	    call hiswrite(tOut,'UVAVER: Miriad '//version)
	    call hisinput(tOut,'UVAVER')
	    call hisclose(tOut)
	    first = .false.
	  endif
	  call VarOnit(tIn,tOut,ltype)
c
c  Loop over the data.
c
	  call uvDatRd(preamble,data,flags,maxchan,nread)
	  Tprev = preamble(4)
	  Tmin = Tprev
	  Tmax = Tmin
	  dowhile(nread.gt.0)
c
c  Count the number of records read.
c
	    nrec = nrec + 1
c
c  Do we want to keep this record.
c
	    ok = relax.or.donenpol
	    if(.not.ok)then
	      do i=1,nread
		ok = ok.or.flags(i)
	      enddo
	    endif
c
c  Determine if we need to flush out the averaged data.
c
	    doflush = ok.and.dotaver
	    if(doflush)then
	      doflush = uvVarUpd(vupd)
	      doflush = (doflush.or.preamble(4)-Tmin.gt.interval.or.
     *				    Tmax-preamble(4).gt.interval)
     *			.and.buffered
	    endif
c
c  Flush out the accumulated data -- the case of time averaging.
c
	    if(doflush)then
	      call BufFlush(tOut,ampsc,vecamp,npol)
	      PolVary = PolVary.or.npol.eq.0.or.
     *		(Snpol.ne.npol.and.Snpol.gt.0)
	      Snpol = npol
	      Tmin = preamble(4)
	      Tmax = Tmin
	      buffered = .false.
c
c  Flush out the accumulated data -- the case of no time averaging.
c
	    else if(.not.dotaver)then
	      if(npol.le.0)call uvDatGti('npol',npol)
	      if(ok)then
		if(.not.donenpol)then
		  call uvputvri(tOut,'npol',npol,1)
		  PolVary = PolVary.or.
     *		    (Snpol.ne.npol.and.Snpol.gt.0)
	          Snpol = npol
		endif
		call uvDatGti('pol',pol)
		call uvputvri(tOut,'pol',pol,1)
		call VarCopy(tIn,tOut)
		call uvwrite(tOut,preamble,data,flags,nread)
		donenpol = npol.gt.1
	      endif
	      npol = npol - 1
	    endif
c
c  Accumulate more data, if we are time averaging.
c
	    if(dotaver.and.ok)then
	      call uvrdvrr(tIn,'inttime',inttime,10.)
	      call BufAcc(preamble,inttime,data,flags,nread)
	      buffered = .true.
	      call VarCopy(tIn,tOut)
	      if(nbin.gt.1)call uvputvri(tOut,'nbin',1,1)
	    endif
c
c  Keep on going. Read in another record.
c
	    if(ok)then
	      Tprev = preamble(4)
	      Tmin = min(Tmin,Tprev)
	      Tmax = max(Tmax,Tprev)
	    endif
	    call uvDatRd(preamble,data,flags,maxchan,nread)
	  enddo
c
c  Flush out anything remaining.
c
	  if(buffered)then
	    call BufFlush(tOut,ampsc,vecamp,npol)
	    PolVary = PolVary.or.npol.le.0.or.
     *	      (Snpol.ne.npol.and.Snpol.gt.0)
	    Snpol = npol
	    buffered = .false.
	  endif
	  call uvDatCls
	enddo
c
c  Write things to the header which describe the data as a whole.
c
	if(first)call bug('f','Error opening input')
	if(nrec.eq.0)call bug('f','No data found')
	if(.not.PolVary)call wrhdi(tOut,'npol',Snpol)
c
c  Update the history and close up files.
c
	call uvclose(tOut)
	end
c************************************************************************
	subroutine GetOpt(uvflags, ampsc, vecamp,relax)
c
	implicit none
        logical ampsc,vecamp,relax
	character uvflags*(*)
c
c  Determine the flags to pass to the uvdat routines.
c
c  Output:
c    uvflags	Flags to pass to the uvdat routines.
c    ampsc      True for amp-scalar averaging
c    vecamp	True for vector averaging on everything except
c		parallel-hand amplitudes.
c    relax	Do not discard bad records.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=7)
	character opts(nopts)*9
	integer l
	logical present(nopts),docal,dopol,dopass,vector
	data opts/'nocal    ','nopol    ','nopass   ',
     *            'vector   ','scalar   ','scavec   ','relax    '/
c
	call options('options',opts,present,nopts)
	docal = .not.present(1)
	dopol = .not.present(2)
	dopass= .not.present(3)
        vector = present(4)
        ampsc  = present(5)
        vecamp = present(6)
	relax  = present(7)
c
c Default averaging is vector
c
        if (vector .and. ampsc) call bug ('f',
     *     'You can''t have options=vector and options=scalar')
        if (vector .and. vecamp) call bug ('f',
     *     'You can''t have options=vector and options=scavec')
        if (ampsc .and. vecamp) call bug ('f',
     *     'You can''t have options=scalar and options=scavec')
c
c Set up calibration flags
c
	uvflags = 'dslr3'
	l = 5
	if(docal)then
	  l = l + 1
	  uvflags(l:l) = 'c'
	endif
	if(dopass)then
	  l = l + 1
	  uvflags(l:l) = 'f'
	endif
	if(dopol)then
	  l = l + 1
	  uvflags(l:l) = 'e'
	endif
	end
c************************************************************************
	subroutine BufIni
	implicit none
c
c  Initialise the routines which do the buffering and averaging of
c  the visibility data.
c  All the buffering/averaging is performed in arrays stored in a
c  common block.
c
c------------------------------------------------------------------------
	include 'uvaver.h'
	free = 1
	mbase = 0
	end
c************************************************************************
	subroutine BufFlush(tOut,ampsc,vecamp,npol)
c
	implicit none
	integer tOut,npol
        logical ampsc,vecamp
c
c  This writes out the averaged data. The accumulated data is in common.
c  This starts by dividing the accumulated data by "N", and then writes
c  it out.
c
c  Inputs:
c    tOut	The handle of the output file.
c    ampsc      True for amp scalar averaging
c    vecamp	True for amp scalar averaging of only parallel hand
c		amplitudes.
c  Output:
c    npol	The number of polarisations in the output. If this
c		varies, a zero is returned.
c------------------------------------------------------------------------
	include 'uvaver.h'
	complex data(MAXCHAN)
        real amp,inttime
	double precision preambl(5),time(MAXBASE)
	logical flags(MAXCHAN)
	integer i,j,jd,k,ngood,nbp,p,idx1(MAXBASE),idx2(MAXBASE)
	logical PolVary,doamp
c
c  Externals.
c
	logical PolsPara
c
c  Determine the number of good baselines, and sort them so we have an
c  index of increasing time.
c
	ngood = 0
	do j=1,mbase
	  if(cnt(j).gt.0)then
	    ngood = ngood + 1
	    time(ngood) = preamble(4,j) / cnt(j)
	    idx2(ngood) = j
	  endif
	enddo
	if(ngood.le.0)return
	call sortidxd(ngood,time,idx1)
c
c  Now loop through the good baselines, writing them out.
c
	nbp = 0
	npol = 0
	do jd=1,ngood
	  j = idx2(idx1(jd))
	  if(npols(j).ne.npol)then
	    call uvputvri(tOut,'npol',npols(j),1)
	    PolVary = npol.gt.0
	    npol = npols(j)
	  endif
	  preambl(1) = preamble(1,j) / cnt(j)
	  preambl(2) = preamble(2,j) / cnt(j)
	  preambl(3) = preamble(3,j) / cnt(j)
	  preambl(4) = preamble(4,j) / cnt(j)
	  preambl(5) = preamble(5,j) / cnt(j)
	  inttime    = preamble(6,j) / npols(j)
	  call uvputvrr(tOut,'inttime',inttime,1)
c
c  Average the data in each polarisation. If there is only one scan in the
c  average, not bother to average it.
c
	  do i=1,npol
	    p = pnt(i,j) - 1
	    call uvputvri(tOut,'pol',pols(i,j),1)
	    doamp = ampsc.or.(vecamp.and.PolsPara(pols(i,j)))
	    nbp = nbp + 1
c
c  Loop over the channels. If we are doing amp-scalar averaging, and
c  the average visibility is zero, flag the data. Otherwise just
c  depend on whether we have good data or not.
c
	    do k=1,nchan(i,j)
	      if(doamp.and.
     *		abs(real(buf(k+p)))+abs(aimag(buf(k+p))).eq.0)
     *		count(k+p) = 0
	      flags(k) = count(k+p).gt.0
	      if(.not.flags(k))then
		data(k) = 0
	      else if(doamp)then
                amp = abs(buf(k+p))
		data(k) = (bufr(k+p) / count(k+p)) *  
     *                          (buf(k+p) / amp)
	      else
		data(k) = buf(k+p) / count(k+p)
              endif
	    enddo
 	    call uvwrite(tOut,preambl,data,flags,nchan(i,j))
	  enddo
	enddo
c
c  Reset the counters.
c
	free = 1
	mbase = 0

c  If the number of polarisations varied, zero npol.
c
	if(PolVary) npol = 0
	end
c************************************************************************
	subroutine BufAcc(preambl,inttime,data,flags,nread)
c
	implicit none
	integer nread
	double precision preambl(5)
	real inttime
	complex data(nread)
	logical flags(nread)
c
c  This accumulates the visibility data. The accumulated data is left
c  in common.
c
c  Input/Output:
c    preambl	Preamble. Destroyed on output.
c    data	The correlation data to be averaged. Destroyed on output.
c    flags	The data flags.
c    nread	The number of channels.
c------------------------------------------------------------------------
	include 'uvaver.h'
	integer i,i1,i2,p,bl,pol
c
c  Determine the baseline number, and conjugate the data if necessary.
c
	call BasAnt(preambl(5),i1,i2)
	bl = ((i2-1)*i2)/2 + i1
	if(bl.gt.MAXBASE)
     *	  call bug('f','Too many baselines for me to handle, in BUFACC')
c
c  Zero up to, and including, this baseline.
c
	do i=mbase+1,bl
	  cnt(i) = 0
	enddo
	mbase = max(mbase,bl)
c
c  Add in this visibility.
c
	if(cnt(bl).eq.0)then
	  cnt(bl) = inttime
	  npols(bl) = 0
	  preamble(1,bl) = inttime * preambl(1)
	  preamble(2,bl) = inttime * preambl(2)
	  preamble(3,bl) = inttime * preambl(3)
	  preamble(4,bl) = inttime * preambl(4)
	  preamble(5,bl) = inttime * preambl(5)
	  preamble(6,bl) = inttime
	else
	  cnt(bl) = cnt(bl) + inttime
	  preamble(1,bl) = preamble(1,bl) + inttime * preambl(1)
	  preamble(2,bl) = preamble(2,bl) + inttime * preambl(2)
	  preamble(3,bl) = preamble(3,bl) + inttime * preambl(3)
	  preamble(4,bl) = preamble(4,bl) + inttime * preambl(4)
	  preamble(5,bl) = preamble(5,bl) + inttime * preambl(5)
	  preamble(6,bl) = preamble(6,bl) + inttime
	endif
c
c  Determine the polarisation.
c
	call uvDatGti('pol',pol)
	p = 0
	do i=1,npols(bl)
	  if(pols(i,bl).eq.pol) p = i
	enddo
c
c  A new baseline. Set up the description of it.
c
	if(p.eq.0)then
	  npols(bl) = npols(bl) + 1
	  p = npols(bl)
	  if(p.gt.MAXPOL) call bug('f',
     *	    'Too many polarizations, in BufAcc')
	  pols(p,bl) = pol
	  nchan(p,bl) = nread
	  pnt(p,bl) = free
	  free = free + nread
	  if(free.gt.MAXAVER)call bug('f',
     *	    'Too much data to accumulate, in BufAcc')
c
c  Copy across the new data.
c
	  p = pnt(p,bl) - 1
	  do i=1,nread
	    if(flags(i))then
	      buf(i+p) = inttime * data(i)
              bufr(i+p) = inttime * abs(data(i))
	      count(i+p) = inttime
	    else
	      buf(i+p) = (0.0,0.0)
              bufr(i+p) = 0.0
	      count(i+p) = 0
	    endif
	  enddo
c
c  Else accumulate new data for old baseline.
c
	else
	  nread = min(nread,nchan(p,bl))
	  nchan(p,bl) = nread
	  p = pnt(p,bl) - 1
	  do i=1,nread
	    if(flags(i))then
	      buf(i+p) = buf(i+p) + inttime * data(i)
              bufr(i+p) = bufr(i+p) + inttime * abs(data(i))
	      count(i+p) = count(i+p) + inttime
	    endif
	  enddo
	endif
c
	end
