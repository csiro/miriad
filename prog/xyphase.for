c************************************************************************
	program xyphase
	implicit none
c
c= xyphase -- Determine XY phase corrections from on-line measurements.
c& rjs
c: uv analysis
c+
c	XYPHASE is a MIRIAD task, which determines XY phase corrections
c	from the on-line XY phase measurements. This can calculate
c	an antenna gain and/or a bandpass function.
c
c	This cannot cope with multi-IF data-sets. Bad solutions will result
c	if you attempt to use it for this data.
c
c	The input data should contain either the "xyphase" uv variable
c	(the on-line XY phase measurement), and/or autocorrelation data. The
c	autocorrelation data is required if the bandpass is being solved for.
c	Note that the "xyphase" variable is lost if the data enters Miriad
c	via FITS (unless ATMERGE is used). The autocorrelation data is
c	currently not written when using the standard correlator
c	configurations.
c@ vis
c	The input visibility data-set. No default. This should contain
c	raw ATCA polarisation data (i.e. XX,YY,XY,YX correlations).
c@ select
c	Normal uv selection criteria. The default is to select all data.
c	Polarisation selection should not be used.
c@ refant
c	The reference antenna. By default XYPHASE computes the gains and
c	bandpasses for all antennae. By specifying a reference antenna,
c	XYPHASE finds solutions for only the reference antenna, and sets the
c	other phases to zero. This is as other programs (such as GPCAL and
c	MFCAL) can determine all but the reference antennas XY phases.
c@ interval
c	This gives one or two numbers, both given in minutes, both being used
c	to determine the interval over which the antenna XY phase is assumed
c	to remain constant. The first number gives the maximum length of
c	a solution interval. The second number gives the maximum gap size in
c	a solution interval. A new solution is started when either the
c	maximum time length is exceeded, or a gap larger than the max gap is
c	encountered. The default is a maximum length of 5 minutes, and a
c	gap size which is the same as the maximum length.
c@ break
c	The times, in the normal Miriad format, where to insert the
c	break point. A ``break point'' gives a time where some form of
c	instrumental glitch took place, and the xyphases jumped. By
c	giving a break point, you prevent a solution interval from spanning
c	this time, and you inhibit gain interpolation across the time.
c@ options
c	Extra processing options. Several options can be given, separated
c	by commas. Minimum match is used.
c	  noxy    Do not determine the antenna XY phases. By default
c	          XYPHASE will solve for this when the "xyphase" variable
c	          is present in the data.
c	  nopass  Do not determine the XY bandpass function. By default
c	          XYPHASE will solve for this when autocorrelations are
c	          present in the data.
c--
c  History:
c    rjs  18mar93 Original version.
c    rjs   1apr93 Significant rewrite.
c    rjs  12oct93 Some additional consistency checks. Break points.
c    rjs  25nov93 Use library version of mkeyt.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXVAL,MAXVIS,MAXIF,MAXPOL,GAINSIZE,MAXSELS,MAXTIME
	parameter(MAXVAL=10000,MAXVIS=100000,MAXTIME=32)
	parameter(MAXPOL=2,MAXIF=8)
	parameter(GAINSIZE=MAXPOL*MAXANT*MAXCHAN,MAXSELS=1024)
	character version*(*)
	parameter(version='XyPhase: version 1.0 12-Oct-93')
c
	character in*64
	integer tIn,nif,npb,nxy,nchan,refant,ntau,maxsol,nsols,off
	integer nbreak
	double precision interval,gap,break(MAXTIME)
	integer pee(MAXPOL)
	logical doxy,dopass,freqch
	double precision sfreq(MAXIF),sdf(MAXIF)
	integer nschan(MAXIF),nants
	real sels(MAXSELS)
c
	double precision xytimes(MAXVAL),pbtimes(MAXVAL)
	integer xyants(MAXVAL),pbants(MAXVAL)
	logical flags(MAXVIS)
	complex vis(MAXVIS)
	complex xy(MAXVAL)
	complex Gains(GAINSIZE)
c
c  Get input parameters.
c
	call output(version)
	call keyini
	call keya('vis',in,' ')
	if(in.eq.' ')
     *	  call bug('f','File names must be given')
	call mkeyt('break',break,MAXTIME,nbreak,'time')
	call keyi('refant',refant,0)
	call keyd('interval',interval,60.d0)
	call keyd('interval',gap,interval)
	if(interval.le.0.or.gap.le.0)
     *	  call bug('f','Invalid interval parameter')
	call SelInput('select',sels,MAXSELS)
	call GetOpt(doxy,dopass)
	call keyfin
c
c  Convert the times to days.
c
	interval = interval / (24*60)
	gap      = gap      / (24*60)
c
c  Open and initialise the input file.
c
	call uvopen(tIn,in,'old')
	call uvset(tIn,'data','channel',0,1.,1.,1.)
	call SelApply(tIn,sels,.true.)
c
c  Get the appropriate visibility data records and the XY phase records.
c
	call output('Reading the data ...')
	call DatRead(tIn,refant,
     *	  MAXVAL,nxy,xytimes,xyants,xy,
     *    MAXVAL,MAXVIS,npb,nchan,pbtimes,pbants,vis,flags,
     *	  MAXIF,nif,sfreq,sdf,nschan,nants,freqch)
c
	if(dopass.and.(npb.eq.0.or.nchan.eq.1))then
	  call bug('w',
     *	    'No appropriate data present to determine bandpass')
	  dopass = .false.
	endif
	if((nxy.eq.0.and.npb.eq.0).and.doxy)then
	  call bug('w',
     *	    'No data present to determine antenna XY phase')
	  doxy = .false.
	endif
	if(.not.(dopass.or.doxy))
     *	  call bug('f','Nothing left to solve for')
c
c  If we are to estimate the XY phase, and there is more than one
c  IF, and no bandpass, then we are in deep shit.
c  If we are to create the bandpass, and the frequency descriptors
c  changes, then we are in deep shit.
c
	if(doxy.and.nif.gt.1.and..not.dopass)call bug('f',
     *	  'Cannot estimate the xy phase for multiple IFs')
	if(dopass.and.freqch)call bug('f',
     *	  'Cannot handle changing freq description when doing bandpass')
c
c  Write out header info.
c
	ntau = 0
	if(.not.doxy)call rdhdi(tIn,'ntau',ntau,0)
	call wrhdi(tIn,'ntau',ntau)
	call wrhdi(tIn,'nfeeds',2)
	call wrhdi(tIn,'ngains',(2+ntau)*nants)
c
c  Solve for the bandpass phase. Always do this if the autocorrelations
c  are present, so that the XY phase is determined from these (rather
c  then the SYSCAL group values). Write out the solution if required.
c
	if(npb.gt.0)then
	  if(2*nchan*nants.gt.GAINSIZE)
     *	    call bug('f','Too many nants/chans')
	  call output('Processing autocorrelation data ...')
	  call Solver(pbants,vis,flags,Gains,nants,nchan,npb,xy,pee)
	endif
	if(dopass)then
	  off = 1
	  if(pee(1).eq.1)off = nants*nchan + 1
	  if(refant.gt.0)call RefSet(Gains(off),nants,nchan,refant)
	  call PassTab(tIn,2,nants,nchan,nif,sfreq,sdf,nschan,Gains,pee)
	endif
c
c  Solve for the XY antenna phase. One of the time and ant arrays is
c  used as scratch, as is the vis array, while the other time and ant
c  arrays contains the needed info.
c
	if(doxy)then
	  maxsol = min(GAINSIZE/nants,MAXVAL)
	  if(npb.gt.0)then
	    call output('Determining antenna XY phases from '//
     *	      'autocorrelation solution ...')
	    call XySol(npb,pbants,pbtimes,xy,interval,gap,break,nbreak,
     *	      maxsol,nsols,nants,Gains,xytimes,xyants,vis)
	    if(refant.gt.0)call RefSet(Gains,nants,nsols,refant)
	    call GainTab(tIn,xytimes,Gains,nants,nsols)
	  else
	    call output('Averaging xyphase variable to determine '//
     *	      'antenna XY phase ...')
	    call XySol(nxy,xyants,xytimes,xy,interval,gap,break,nbreak,
     *	      maxsol,nsols,nants,Gains,pbtimes,pbants,vis)
	    if(refant.gt.0)call RefSet(Gains,nants,nsols,refant)
	    call GainTab(tIn,pbtimes,Gains,nants,nsols)
	  endif
	  call wrhdi(tIn,'nsols',nsols)
	  call wrhdd(tIn,'interval',0.5d0)
	endif
c
c  Write some history.
c
	call hisopen(tIn,'append')
	call hiswrite(tIn,'XYPHASE: Miriad '//version)
	call hisinput(tIn,'XYPHASE')
	call hisclose(tIn)
c
c  All said and done.
c
	call uvclose(tIn)
	end
c************************************************************************
	subroutine GainTab(tIn,times,Gains,nants,nsols)
c
	implicit none
	integer tIn,nsols,nants
	double precision times(nsols)
	complex Gains(nants,nsols)
c
c  Write out the gain table.
c
c  Input:
c    everything
c
c------------------------------------------------------------------------
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
	do i=1,nsols
	  call hwrited(item,times(i),off,8,iostat)
	  off = off + 8
	  if(iostat.ne.0)then
	    call bug('w','Error writing time to amp/phase table')
	    call bugno('f',iostat)
	  endif
	  do j=1,nants
	    G(2*j-1) = 1
	    G(2*j)   = 0
	    if(abs(real(Gains(j,i)))+abs(aimag(Gains(j,i))).gt.0)
     *	      G(2*j) = 1/Gains(j,i)
	  enddo
	  call hwriter(item,G,off,8*2*nants,iostat)
	  off = off + 8*2*nants
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
	end
c************************************************************************
	subroutine XYSol(nxy,ants,times,xy,interval,gap,break,nbreak,
     *	  maxsol,nsols,nants,Gains,gtimes,indx,buf)
c
	implicit none
	integer nxy,ants(nxy),maxsol,nsols,nants,nbreak
	double precision times(nxy),interval,gap,gtimes(maxsol)
	complex xy(nxy),Gains(nants,maxsol)
	integer indx(nxy)
	real buf(nxy,2)
	double precision break(nbreak+1)
c
c  Determine the average antenna XY phases. This takes the medians of the
c  gains.
c
c  Input:
c    nxy	The number of xy phases.
c    ants	The antenna number associated with each xy phase.
c    times	The time assosiated with each xy phase.
c    xy		The xy phases (as a complex quantity)
c    interval,gap Solution interval parameters.
c    maxsol	The max number of solutions.
c    nants	The number of antennas.
c    nbreak	Number of break points.
c    break	The preak points.
c  Output:
c    nsols	The number of solutions.
c    Gains	The xy gains.
c    gtimes	The times associated with each xy gain.
c  Scratch:
c    indx	Used for index sorting.
c    buf	Used to store the gains.
c------------------------------------------------------------------------
	integer i,j,k,k0,k1,n,nd
	complex t
	logical more,dobreak
	double precision tfirst,tlast,tsum,tf0,te0,tend
	real rp,ip
c
c  Externals.
c
	integer isrchine
c
c  Find the end of a solution interval.
c
	nsols = 0
	k = 1
	tlast = times(1)
	dowhile(k.le.nxy)
c
c  Determine the end of the solution interval.
c
	  tfirst = tlast
	  tend = tfirst + interval
c
	  tf0 = mod(tfirst - 0.5d0,1.d0)
	  te0 = mod(tend   - 0.5d0,1.d0)
	  if(te0.lt.tf0)te0 = te0 + 1
	  dobreak = .false.
	  do i=1,nbreak
	    if(tf0.lt.break(i).and.break(i).le.te0)then
	      tend = tend - (te0 - break(i))
	      dobreak = .true.
	    else if(tfirst.lt.break(i).and.break(i).le.tend)then
	      tend = break(i)
	      dobreak = .true.
	    endif
	  enddo
c
c  Locate the end of a interval.
c
	  k0 = k
	  more = .true.
	  dowhile(more.and.k.le.nxy)
	    more = times(k).le.tend.and.
     *		   times(k).le.tlast+gap
	    tlast = times(k)
	    if(more)k = k + 1
	  enddo
	  dobreak = dobreak.and.tlast.gt.tend
c
	  n = k - k0
c
c  Work out the index, so that we can group values which have the same
c  antenna numbers.
c
	  call hsorti(n,ants(k0),indx)
c
c  Copy all the data to indx and buf. In the output of this, the data are
c  now in order of increasing antenna number.
c
	  tsum = 0
	  nd = 0
	  k0 = k0 - 1
	  do i=1,n
	    j = indx(i) + k0
	    rp = real(xy(j))
	    ip = aimag(xy(j))
	    if(abs(rp)+abs(ip).gt.0)then
	      tsum = tsum + times(j)
	      nd = nd + 1
	      buf(nd,1) = rp
	      buf(nd,2) = ip
	      indx(nd) = ants(j)
	    endif
	  enddo
	  n = nd
c
c  Locate data for all the antennas, find the median of it, and save the
c  normalised gain.
c
	  if(n.gt.0)then
	    nsols = nsols + 1
	    if(nsols.gt.maxsol)
     *	      call bug('f','Too many solution intervals')
	    gtimes(nsols) = tsum / n
	    k0 = 1
	    k1 = 1
	    do i=1,nants
	      if(n.gt.0)k1 = isrchine(nd,indx(k0),1,i) + k0 - 1
	      if(k0.eq.k1)then
	        Gains(i,nsols) = 0
	      else
	        call median(buf(k0,1),k1-k0,rp)
	        call median(buf(k0,2),k1-k0,ip)
	        t = cmplx(rp,ip)
	        Gains(i,nsols) = t / abs(t)
	        k0 = k1
	        n = n - (k1-k0)
	      endif
	    enddo
	  endif
c
c  Generate a break point if needed.
c
	  if(dobreak)then
	    nsols = nsols + 1
	    if(nsols.gt.maxsol)
     *	      call bug('f','Too many solution intervals')
	    gtimes(nsols) = tend
	    do i=1,nants
	      Gains(i,nsols) = 0
	    enddo
	  endif
	enddo
c
	end	      
c************************************************************************
	subroutine DatRead(tIn,refant,
     *	  maxxy,nxy,xytimes,xyants,xy,
     *	  maxpb,maxvis,npb,nchan,pbtimes,pbants,vis,flags,
     *	  maxif,nif,sfreq,sdf,nschan,nants,freqch)
c
	implicit none
	integer tIn,refant,maxxy,maxpb,maxvis,maxif
	integer nxy,xyants(maxxy),npb,nchan,pbants(maxpb)
	integer nif,nschan(maxif),nants
	double precision xytimes(maxxy),pbtimes(maxpb)
	double precision sfreq(maxif),sdf(maxif)
	complex xy(maxxy),vis(maxvis)
	logical flags(maxvis),freqch
c
c  Read all the relevant data from the visibility file.
c
c  Input:
c    tIn
c    refant
c    maxxy
c    maxpb
c    maxvis
c    maxif
c  Output:
c    nxy
c    xytimes
c    xyants
c    xy
c    npb
c    nchan
c    pbtimes
c    pbants
c    vis
c    nif
c    sfreq
c    sdf
c    nschan
c    nants
c    freqch	True if the frequency descriptors changes.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer PolXY,PolYX
	parameter(PolXY=-7,PolYX=-8)
c
	real xyphase(MAXANT)
	integer iXY,iYX,vhan,vfreq,i,j,off,i1,i2,length
	logical updated,xypres
	character type*1
	integer n(2),pol(2)
	double precision preamble(4,2)
	complex data(MAXCHAN,2)
	logical flgs(MAXCHAN,2)
c
c  Externals.
c
	logical uvVarUpd
c
c  Initialise.
c
	nants = 0
	npb = 0
	nxy = 0
	j = 1
	n(2) = 0
	pol(2) = 0
	preamble(3,2) = 0
	preamble(4,2) = 0
c
c  Is the xyphase variable present? Is so get a variable handle to it.
c
	call uvprobvr(tIn,'xyphase',type,length,updated)
	xypres = type.eq.'r'
	if(xypres)then
	  call uvVarini(tIn,vhan)
	  call uvVarSet(vhan,'xyphase')
	endif
c
c  Select the appropriate data.
c
	call uvselect(tIn,'and',0.d0,0.d0,.true.)
	if(refant.gt.0)
     *	  call uvselect(tIn,'antennae',dble(refant),0.d0,.true.)
	call uvselect(tIn,'polarization',dble(PolXY),0.d0,.true.)
	call uvselect(tIn,'polarization',dble(PolYX),0.d0,.true.)
c
c  Read the first, and remember important info.
c
	call uvread(tIn,preamble(1,j),data(1,j),flgs(1,j),
     *	  MAXCHAN,n(j))
c
	nchan = n(j)
	call uvrdvri(tIn,'nants',nants,0)
	if(nants.le.0.or.nants.gt.MAXANT)
     *	  call bug('f','Invalid value for the nants variable')
	if(refant.gt.nants)
     *	  call bug('f','Invalid value for the refant parameter')
	call uvrdvri(tIn,'nspect',nif,1)
	if(nif.le.0.or.nif.gt.MAXIF)
     *	  call bug('f','Invalid value for the nspect variable')
	call uvgetvrd(tIn,'sfreq',sfreq,nif)
	call uvgetvrd(tIn,'sdf',sdf,nif)
	call uvgetvri(tIn,'nschan',nschan,nif)
c
c  See if the frequency descriptors change in the future.
c
	freqch = .false.
	call uvVarIni(tIn,vfreq)
	call uvVarSet(vfreq,'nschan')
	call uvVarSet(vfreq,'sfreq')
	call uvVarSet(vfreq,'sdf')
c------------------------------------------------------------------------
c  Loop over processing the data, and reading the rest.
c
	dowhile(n(j).eq.nchan)
	  freqch = freqch.or.uvVarUpd(vfreq)
c
c  Read the "xyphase" variable, if needed.
c
	  if(xypres)then
	    if(uvVarUpd(vhan))then
	      call uvgetvrr(tIn,'xyphase',xyphase,nants)
	      if(refant.gt.0)then
	        if(nxy+1.gt.MAXXY)
     *		  call bug('f','XY arrays overflowed')
		xy(nxy+1) = cmplx(cos(xyphase(refant)),
     *				  sin(xyphase(refant)))
		xyants(nxy+1) = refant
		xytimes(nxy+1) = preamble(3,j)
		nxy = nxy + 1
	      else
	        if(nxy+nants.gt.MAXXY)
     *		  call bug('f','XY arrays overflowed')
	        do i=1,nants
		  xy(i+nxy) = cmplx(cos(xyphase(i)),sin(xyphase(i)))
		  xyants(i+nxy) = i
		  xytimes(i+nxy) = preamble(3,j)
	        enddo
	        nxy = nxy + nants
	      endif
	    endif
	  endif
c
c  Is this appropriate autocorrelation data. If so, difference them and
c  store them.
c
	  call basant(preamble(4,j),i1,i2)
	  call uvrdvri(tIn,'pol',pol(j),0)
	  if((i1.eq.i2).and.
     *	     (preamble(3,1).eq.preamble(3,2)).and.
     *	     (preamble(4,1).eq.preamble(4,2)).and.
     *	     (pol(1).ne.pol(2)))then
c
	    off = nchan * npb
	    npb = npb + 1
	    if(npb.gt.MAXPB)call bug('f','Too many spectra')
	    if(nchan+off.gt.MAXVIS)call bug('f','Too many visibilities')
c
	    pbants(npb) = i1
	    pbtimes(npb) = preamble(3,j)
c
	    iXY = 1
	    if(pol(1).eq.PolYX)iXY = 2
	    iYX = 3 - iXY
c
	    do i=1,nchan
	      vis(i+off) = data(i,iYX) - data(i,iXY)
	      flags(i+off) = flgs(i,iXY).and.flgs(i,iYX)
	    enddo
	    pol(j) = 0
	  endif
c
	  if(i1.eq.i2)j = 3 - j
c
c  Get another record.
c
	  call uvread(tIn,preamble(1,j),data(1,j),flgs(1,j),
     *	    MAXCHAN,n(j))
	enddo
	end
c************************************************************************
	subroutine GetOpt(doxy,dopass)
c
	implicit none
	logical doxy,dopass
c
c  Get extra processing options.
c
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=2)
	logical present(NOPTS)
	character opts(NOPTS)*8
	data opts/'noxy    ','nopass  '/
c
	call options('options',opts,present,NOPTS)
c
	doxy   = .not.present(1)
	dopass = .not.present(2)
c
	if(.not.(doxy.or.dopass))call bug('f','Nothing to do')
	end
c************************************************************************
	subroutine Solver(Ant,data,flags,Pass,nants,nchan,npb,xy,pee)
c
	implicit none
	integer nants,nchan,npb
	integer Ant(npb),pee(2)
	complex data(nchan,npb),Pass(nants,nchan,2),xy(npb)
	logical flags(nchan,npb)
c
c  Determine the XY bandpass phase.
c
c  Input:
c    ant
c    data
c    flags
c    nants,nchan,npb
c  Output:
c    xy
c    Pass
c    pee
c------------------------------------------------------------------------
	integer MAXITER
	parameter(MAXITER=30)
	integer i,j,i0,p1,p2,niter,chlo,chhi
	logical more
	complex c
	real t,epsi
	character line*64
c
c  Determine the range of channels that we will work with in determining
c  the XY antenna phase. Use the middle 50%.
c
	chlo = nchan/4 + 1
	chhi = chlo + nchan/2
c
c  The initial estimate of the bandpass is a phase of 0.
c
	p1 = 1
	p2 = 2
	do j=1,nchan
	  do i=1,nants
	    Pass(i,j,1) = 1
	  enddo
	enddo
c
	more = .true.
	niter = 0
	dowhile(more.and.niter.lt.MAXITER)
	  niter = niter + 1
c
c  Zero the accumulators.
c
	  do j=1,nchan
	    do i=1,nants
	      Pass(i,j,p2) = 0
	    enddo
	  enddo
c
c  Given the current estimate of the XY phase spectrum,
c  determine the XY phase.
c
	  do i=1,npb
	    i0 = Ant(i)
	    c = 0
	    do j=chlo,chhi
	      if(flags(j,i))c = c + conjg(Pass(i0,j,p1))*Data(j,i)
	    enddo
	    t = abs(c)
	    if(t.gt.0)then
	      c = c / t
	    else
	      c = 0
	    endif
c
	    xy(i) = c
c
	    c = conjg(c)
	    do j=1,nchan
	      if(flags(j,i))Pass(i0,j,p2) = Pass(i0,j,p2) + 
     *		c*Data(j,i)
	    enddo
	  enddo
c
c  Normalise the solution to a value of 1.
c
	  epsi = 0
	  do j=1,nchan
	    do i=1,nants
	      t = abs(Pass(i,j,p2))
	      if(t.gt.0)then
		Pass(i,j,p2) = Pass(i,j,p2) / t
	      else
		Pass(i,j,p2) = 0
	      endif
	      c = (Pass(i,j,p1) - Pass(i,j,p2))
	      epsi = epsi + real(c)**2 + aimag(c)**2
	    enddo
	  enddo
	  p1 = p2
	  p2 = 3 - p1
	  epsi = sqrt(epsi / (nchan*nants))
	  more = epsi.gt.1e-4
	  write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *	    ', Solution Error:    ',epsi
	  call output(line)
	enddo
c
	if(more)call bug('w','Failed to converge ... continuing anyway')
c
c  Set the phase of the X channel to be zero.
c
	do j=1,nchan
	  do i=1,nants
	    Pass(i,j,p2) = 1
	  enddo
	enddo
c
c  Set the mapping so that the table writter nows which polarisation is
c  which.
c
	pee(1) = p2
	pee(2) = p1
	end
c************************************************************************
	subroutine RefSet(Gains,nants,nsols,refant)
c
	implicit none
	integer nants,nsols,refant
	complex Gains(nants,nsols)
c
c  Set all but the reference antenna to have a gain of 1.
c
c  Input:
c    nants,nsols,npol
c    refant
c  Input/Output:
c    Gains
c------------------------------------------------------------------------
	integer i,j
c
	do j=1,nsols
	  do i=1,refant-1
	    Gains(i,j) = 1
	  enddo
	  do i=refant+1,nants
	    Gains(i,j) = 1
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine PassTab(tno,npol,nants,nchan,
     *			nspect,sfreq,sdf,nschan,Pass,pee)
c
	implicit none
	integer tno,npol,nants,nchan,nspect,nschan(nspect),pee(npol)
	complex Pass(nants,nchan,npol)
	double precision sdf(nspect),sfreq(nspect)
c
c  Write out the bandpass shapes. This assumes that the parameters
c    ngains, nfeeds
c
c  Input:
c    pee	Mapping from internal polarisation number to the order
c		that we write the gains out in.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSPECT
	parameter(MAXSPECT=16)
	integer iostat,off,item,i,j,k,n,p,pd
	complex G(MAXCHAN),temp
	double precision freqs(2)
c
c  Fudge to create a "complex" table, then open it again.
c
	call wrhdc(tno,'bandpass',(0.,0.))
	call haccess(tno,item,'bandpass','append',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output bandpass table')
	  call bugno('f',iostat)
	endif
c
c  Write out all the gains.
c
	off = 8
	do i=1,nants
	  do p=1,npol
	    pd = pee(p)
	    do j=1,nchan,MAXCHAN
	      n = min(MAXCHAN,nchan-j+1)
	      do k=1,n
	        temp = Pass(i,j+k-1,pd)
	        if(abs(real(temp))+abs(aimag(temp)).ne.0)then
		  G(k) = 1/temp
	        else
		  G(k) = 0
		endif
	      enddo
	    enddo
	    call hwriter(item,G,off,8*n,iostat)
	    off = off + 8*n
	    if(iostat.ne.0)then
	      call bug('w','Error writing gains to bandpass table')
	      call bugno('f',iostat)
	    endif
	  enddo
	enddo
c
c  Finished writing the bandpass table.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Access the frequencies description table.
c
	call haccess(tno,item,'freqs','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output frequency table.')
	  call bugno('f',iostat)
	endif
	call hwritei(item,0,0,4,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error writing header of frequency table')
	  call bugno('f',iostat)
	endif
c
c  Write out all the frequencies.
c
	off = 8
	do i=1,nspect
	  call hwritei(item,nschan(i),off,4,iostat)
	  off = off + 8
	  if(iostat.ne.0)then
	    call bug('w','Error writing nschan to freq table')
	    call bugno('f',iostat)
	  endif
	  freqs(1) = sfreq(i)
	  freqs(2) = sdf(i)
	  call hwrited(item,freqs,off,2*8,iostat)
	  off = off + 2*8
	  if(iostat.ne.0)then
	    call bug('w','Error writing freqs to freq table')
	    call bugno('f',iostat)
	  endif
	enddo
c
c  Finished writing the frequency table.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Now write out the other parameters that need to go along with this.
c
	call wrhdi(tno,'nspect0',nspect)
	call wrhdi(tno,'nchan0',nchan)
c
	end
