c************************************************************************
	program mfplan
	implicit none
c
c= mfplan - Determine good frequency setups for multi-frequency synthesis.
c& rjs
c: utility
c+
c	MFPLAN helps determine observing frequencies for multi-frequency
c	synthesis experiments with the ATCA. 
c@ freq
c	Frequency bands (in GHz) to search for good mfs setups. This consists
c	of a number of pairs of frequencies, giving the lower and upper
c	limits on bands appropriate for observing. Several bands will often
c	be given to avoid frequencies which suffer from interference
c	or birdies. The nominal frequency bands of the AT receivers are:
c	  1.35,1.78    (L band)
c	  2.20,2.50    (S band)
c	  4.544,6.336  (C band)
c	  8.00,9.20    (X band)
c	The default is 4.544,6.336.
c@ nfreq
c	Number of frequencies per configuration. Generally 2 or 4 are
c	the most usual numbers for the ATCA. Default is 2.
c@ nants
c	Number of antennae to use. Sensible values are either 5 or 6.
c	Default is 6.
c@ config
c	Array configuration. Several values can be given. Valid values are:
c	3.0a (6.0a), 3.0b (6.0b), 3.0c (6.0c), 3.0d (6.0d), 1.5a, 1.5b,
c	1.5c, 1.5d, 0.75a, 0.75b, 0.75c, 0.75d, 0.375, 0.122. These correspond 
c	to the 14 standard ATCA configurations.
c@ fixed
c	List of fixed frequencies that are fixed (not varied) by MFPLAN.
c	Usually these correspond to frequencies used in completed
c	observations. The first "nfreq" frequencies will be
c	associated with the first configuration, the second "nfreq"
c	frequencies correspond to the second configuration, etc.
c	The default is that all frequencies are varied.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Minimum match is used. Possible values are:
c	  no128    Frequencies that are multiples of 128 MHz are to be
c	           avoided. All resulting frequencies are odd multiples
c	           of 64 MHz.
c	  infinity Minimise the worst gap in the uv coverage. The default
c	           is to minimise the rms gap in the uv coverage. The
c	           infinity option is the best option if you have very good
c	           uv coverage. Otherwise it produces poorer results.
c	  colours  Plot each configuration with a different colour
c@ device
c	PGPLOT device. If this is given, then a plot of the u-v coverage
c	is made. Default is no plot.
c--
c  History:
c    rjs  13may92 Original version.
c    rjs  31may92 Many fiddles.
c    nebk 11dec92 GETFREQ -> GETFRQ to avoid confusion with library one
c    rjs  16sep93 Rename bsrch to binsrch.
c    rjs  20sep93 Infinity option.
c    nebk 23sep93 Add 6.0 arrays to silence akt (no easy feat)
c    rjs   7oct93 Change default frequencies to silence akt (certainly no easy
c		  feat).
c    rjs  04jul97 "fixed" keyword.
c    nebk 13oct97 Add 210m array.  Draw arrys in colours
c    rjs   6apr04 Added ew214, ew352, ew367
c  Bugs:
c------------------------------------------------------------------------
	character version*(*)
	integer nants,nbl,MAXCONFG,NCONFGS,MAXBANDS
	parameter(nants=6,nbl=(nants*(nants-1))/2,MAXBANDS=128)
	parameter(MAXCONFG=32,NCONFGS=22)
	parameter(version='MfPlan: version 1.0 6-Apr-04 ')
c
	real bands(2,MAXBANDS),uni(MAXBANDS+1)
	integer nbands
	integer nfreq,nconfg,nans,confgno(MAXCONFG)
	integer idx(nbl*MAXCONFG+2),coord(nants*MAXCONFG)
	real freqs(MAXCONFG),bls(nbl*MAXCONFG+2)
	real oldfreq,maxfreq,freqsum
	integer i,j,k,l,llabel
	real T,E,oldE,p,rand(3),maxbase
	integer nfail,nsucc,trials,nfix
	character config*8,device*16,label*80,line*64
	logical no128,inf,docol
c
c  Externals.
c
	real eval,GetFrq
	integer binsrcha,len1
c
c  Configurations tables.
c
	character name(NCONFGS)*5
	integer uu(NANTS,NCONFGS)
  	data name(1),(uu(i,1),i=1,6)/'0.122',  0,  2,  4,  6,  8,392/
        data name(2),(uu(i,2),i=1,6)/'0.210', 98,100,102,109,112,392/
	data name(3),(uu(i,3),i=1,6)/'0.375',  2, 10, 14, 16, 32,392/
	data name(4),(uu(i,4),i=1,6)/'0.75a',147,163,172,190,195,392/
	data name(5),(uu(i,5),i=1,6)/'0.75b', 98,109,113,140,148,392/
	data name(6),(uu(i,6),i=1,6)/'0.75c', 64, 84,100,110,113,392/
	data name(7),(uu(i,7),i=1,6)/'0.75d',100,102,128,140,147,392/
	data name(8),(uu(i,8),i=1,6)/'1.5a ',100,110,147,168,196,392/
	data name(9),(uu(i,9),i=1,6)/'1.5b ',111,113,163,182,195,392/
	data name(10),(uu(i,10),i=1,6)/'1.5c ', 98,128,173,190,195,392/
	data name(11),(uu(i,11),i=1,6)/'1.5d ',102,109,140,182,196,392/
	data name(12),(uu(i,12),i=1,6)/'3.0a ',  4, 45,102,173,195,392/
	data name(13),(uu(i,13),i=1,6)/'3.0b ',  2, 64,147,182,196,392/
	data name(14),(uu(i,14),i=1,6)/'3.0c ',  0, 10,113,140,182,392/
	data name(15),(uu(i,15),i=1,6)/'3.0d ',  8, 32, 84,168,173,392/
	data name(16),(uu(i,16),i=1,6)/'6.0a ',  4, 45,102,173,195,392/
	data name(17),(uu(i,17),i=1,6)/'6.0b ',  2, 64,147,182,196,392/
	data name(18),(uu(i,18),i=1,6)/'6.0c ',  0, 10,113,140,182,392/
	data name(19),(uu(i,19),i=1,6)/'6.0d ',  8, 32, 84,168,173,392/
	data name(20),(uu(i,20),i=1,6)/'ew214', 98,102,104,109,112,392/
	data name(21),(uu(i,21),i=1,6)/'ew352',102,104,109,112,125,392/
	data name(22),(uu(i,22),i=1,6)/'ew367',104,110,113,124,128,392/

c
c Get the users input.
c
	call output(version)
	call keyini
	call GetOpt(no128,inf,docol)
	call GetBands(bands,uni,MAXBANDS,nbands,
     *	  maxfreq,freqsum,no128)
	call keyi('nfreq',nfreq,2)
	if(nfreq.lt.1)call bug('f','Invalid number of frequencies')
	call keyi('nants',nans,6)
	if(nans.lt.2.or.nans.gt.nants)
     *	  call bug('f','Invalid number of antennae')
c
	write(label,'(a,f7.3,a,i2,a)')'MaxFreq=',maxfreq,
     *					', Nfreq=',nfreq,
     *					', Config='
	llabel = len1(label)
c
	maxbase = 0
	k = 0
	nconfg = 0
	call keya('config',config,' ')
	dowhile(config.ne.' ')
	  l = binsrcha(config,name,NCONFGS)
	  if(l.eq.0)call bug('f','Unrecognised configuration '//config)
	  label(llabel+1:llabel+5) = config
	  llabel = len1(label)
	  llabel = llabel+1
	  label(llabel:llabel) = ','
	  do j=1,nfreq
	    nconfg = nconfg + 1
	    if(nconfg.gt.MAXCONFG)
     *	      call bug('f','Too many configurations*frequencies')
	    do i=1,nans
	      k = k + 1
	      coord(k) = uu(i,l)
	    enddo
	    maxbase = max(maxbase,real(uu(nans,l)-uu(1,l)))
	    confgno(nconfg) = l
	  enddo
	  call keya('config',config,' ')
	enddo
	maxbase = maxbase * maxfreq
	llabel = llabel - 1
c
	if(nconfg.le.0)call bug('f','No configurations given')
	call mkeyr('fixed',freqs,nconfg-1,nfix)
	call keya('device',device,' ')
	call keyfin
c
c  Get the initial guess at the frequencies.
c
	call uniform(freqs(nfix+1),nconfg-nfix)
	do i=nfix+1,nconfg
	  freqs(i) = GetFrq(freqs(i),bands,uni,nbands,freqsum,no128)
	enddo
	E = eval(nconfg,nans,coord,freqs,bls,idx,maxbase,inf)
c
c  Start to iterate.
c
	T = 5
	trials = 0
	nfail = 0
	nsucc = 0
	dowhile(T.gt.1e-4)
c
c  Get a new possible solution.
c
	  call uniform(rand,3)
	  i = (nconfg-nfix)*rand(1) + 1 + nfix
	  oldfreq = freqs(i)
	  oldE = E
	  freqs(i) = GetFrq(rand(2),bands,uni,nbands,freqsum,no128)
	  E = eval(nconfg,nans,coord,freqs,bls,idx,maxbase,inf)
	  p = (oldE - E)/T
	  if(p.lt.-20)then
	    p = 0
	  else if(p.gt.0)then
	    p = 1
	  else
	    p = exp(p)
	  endif
	  if(p.gt.rand(3))then
	    nsucc = nsucc + 1
	  else
	    E = oldE
	    freqs(i) = oldfreq
	    nfail = nfail + 1
	  endif
	  trials = trials + 1
	  if(mod(trials,5000).eq.0)then
	    write(line,'(a,f6.3,a,f7.4)') 'Goodness:',1/E,
     *					', Temperature:',T
	    call output(line)
	  endif
c
c  Lower the temperature if needed.
c
	  if(nsucc.gt.3*nfreq*nconfg.or.nfail.gt.30*nfreq*nconfg)then
	    T = 0.9*T
	    nsucc = 0
	    nfail = 0
	  endif
	enddo
c
c  It seems to have frozen. Tell the user as much.
c
	call report(nconfg,confgno,name,freqs,1/E)
c
c  Plot the final solution.
c
	if(device.ne.' ')call Plotit(device,nconfg,nans,coord,
     *                               freqs,bls,docol,label(1:llabel))
	end
c************************************************************************
	subroutine GetOpt(no128,inf,docol)
c
	implicit none
	logical no128,inf,docol
c
c  Get task enrichment parameters.
c
c  Output:
c    no128	Avoid multiples of 128 MHz.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=3)
	character opts(nopts)*8
	logical present(nopts)
c
	data opts/'no128   ','infinity','colours'/
	call options('options',opts,present,nopts)
c
	no128 = present(1)
	inf   = present(2)
        docol = present(3)
	end
c************************************************************************
	subroutine GetBands(bands,uni,maxbands,nbands,
     *	  maxfreq,freqsum,no128)
c
	implicit none
	integer maxbands,nbands
	real bands(2,maxbands),uni(maxbands+1),maxfreq,freqsum
	logical no128
c
c  Get the frequency bands of interest.
c
c  Inputs:
c    no128	Avoid multiples of 128 MHz.
c    maxbands
c  Output:
c    freqsum
c    maxfreq
c    bands
c    uni
c    nbands
c------------------------------------------------------------------------
	integer i
	real sum
c
	call mkeyr('freq',bands,2*maxbands,nbands)
	if(2*(nbands/2).ne.nbands)
     *	  call bug('f','Odd number of frequencies given')
	nbands = nbands/2
	if(nbands.eq.0)then
	  nbands = 1
	  bands(1,1) = 4.544
	  bands(2,1) = 6.336
	endif
c
c  Sum up all the bands, and check legality.
c
	freqsum = 0
	maxfreq = 0
	do i=1,nbands
	  if(no128)then
	    bands(1,i) = 0.128*nint((bands(1,i)-0.064)/0.128 + 0.5)
     *							      + 0.064
	    bands(2,i) = 0.128*nint((bands(2,i)-0.064)/0.128 - 0.5)
     *							      + 0.064
	  endif
	  if(bands(1,i).le.0.or.bands(2,i).le.bands(1,i))
     *	    call bug('f','Illegal frequency bands')
	  maxfreq = max(maxfreq,bands(2,i))
	  freqsum = freqsum + bands(2,i) - bands(1,i)
	enddo
c
c  Set up the "uni" array.
c
	sum = 0
	uni(1) = 0
	do i=1,nbands
	  uni(i) = sum / freqsum
	  sum = sum + bands(2,i) - bands(1,i)
	enddo
	uni(nbands+1) = 1
c
	end
c************************************************************************
	real function GetFrq(rand,bands,uni,nbands,freqsum,no128)
c
	implicit none
	integer nbands
	real rand,bands(2,nbands),uni(nbands+1),freqsum
	logical no128
c
c  Given a random number, determine the next frequency.
c
c------------------------------------------------------------------------
	integer i,j,k,l
	real f
c
c  Search for the frequency band of interest.
c
	k = 1
	l = nbands
	dowhile(k.le.l)
	  j = (k+l)/2
	  if(rand.lt.uni(j))then
	    l = j - 1
	  else if(rand.lt.uni(j+1))then
	    i = j
	    k = l + 1
	  else
	    k = j + 1
	  endif
	enddo
c
	f = (rand-uni(i)) * freqsum + bands(1,i)
	if(no128)f = 0.128*nint((f-0.064)/0.128) + 0.064
	GetFrq = 0.001*nint(1000*f)
	end
c************************************************************************
	subroutine Plotit(device,nconfg,nants,coord,freqs,bls,
     *                    docol,label)
c
	implicit none
	integer nconfg,nants,coord(nants,nconfg)
	real freqs(nconfg),bls((nconfg*nants*(nants-1))/2)
	character device*(*),label*(*)
        logical docol
c
c  Plot the uv coverage.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer NPTS
	parameter(NPTS=128)
	real x(NPTS),y(NPTS)
	integer i,j,k,l,n,np,nbase,icol
	real factor,maxbase,theta
c
c  Externals.
c
	integer pgbeg
c
c  Determine the scale factor. 15.306 is the increment between AT stations
c  (in metres). The result comes out in kilowavelengths.
c
	factor = 15.306 * 1e9 * 1e-3/CMKS
c
c  Determine the baselines.
c
	maxbase = 0
	l = 0
	do k=1,nconfg
	  do j=2,nants
	    do i=1,j-1
	      l = l + 1
	      bls(l) = factor*freqs(k)*(coord(j,k) - coord(i,k))
	      maxbase = max(maxbase,bls(l))
	    enddo
	  enddo
	enddo
	n = nconfg*nants*(nants-1)/2
	maxbase = 1.05*maxbase
        nbase = nants*(nants-1)/2
c
c  Initialise PGPLOT.
c
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Error opening graphics device')
	endif
	call pgpage
	call pgwnad(-maxbase,maxbase,-maxbase,maxbase)
	call pgtbox('BCNST',0.,0,'BCNST',0.,0)
	call pglab('u (k\gl)','v (k\gl)',label)
        icol = 1
        if (docol) icol = 0
	do j=1,n
	  np = max(10,int(sqrt(bls(j)/maxbase)*NPTS)+1)
	  do i=1,np
	    theta = 2*PI*(i-1)/(np-1)
	    x(i) = bls(j)*cos(theta)
	    y(i) = bls(j)*sin(theta)
	  enddo
          if (docol .and. mod(j,nbase).eq.1) icol = icol + 1
          call pgsci(icol)
	  call pgline(np,x,y)
	enddo
	call pgend
	end
c************************************************************************
	subroutine report(nconfg,confgno,name,freqs,E0)
c
	implicit none
	integer nconfg
	character name(*)*(*)
	integer confgno(nconfg)
	real E0,freqs(nconfg)
c------------------------------------------------------------------------
	integer i,old
	character line*64
c
	write(line,'(a,1pg10.3)')'Final solution goodness:',E0
	call output(line)
	old = 0
	do i=1,nconfg
	  if(confgno(i).eq.old)then
	    write(line,'(f34.3)')freqs(i)
	  else
	    old = confgno(i)
	    write(line,'(a,a,a,f7.3)')'Configuration: ',name(old),
     *				    '  Freq:',freqs(i)
	  endif
	  call output(line)
	enddo
	end
c************************************************************************
	real function eval(nconfg,nants,coord,freqs,bls,idx,maxbase,inf)
c
	implicit none
	integer nconfg,nants
	integer coord(nants,nconfg),idx((nconfg*nants*(nants-1))/2+2)
	real freqs(nconfg),bls((nconfg*nants*(nants-1))/2+2),maxbase
	logical inf
c
c  Evaluate the error measure.
c------------------------------------------------------------------------
	integer i,j,k,n,i0,i1
	real error,r,wt
c
	bls(1) = 0
	k = 0
c	k = 1
	do n=1,nconfg
	  do j=2,nants
	    do i=1,j-1
	      k = k + 1
	      bls(k) = ( coord(j,n) - coord(i,n) ) * freqs(n) / maxbase
	    enddo
	  enddo
	enddo 
	n = (nconfg*nants*(nants-1))/2
c	n = (nconfg*nants*(nants-1))/2 + 1
c	n = (nconfg*nants*(nants-1))/2 + 2
c	bls(n) = 1
c
c  Sort the baselines.
c
	call sortidxr(n,bls,idx)
c
c  Work out the error measure.
c
	wt = 0
	error = 0
	do i=2,n
	  i0 = idx(i)
	  i1 = idx(i-1)
	  r = abs(bls(i0) - bls(i1))
	  if(inf)then
	    error = max(error,r)
	    wt = 1
	  else
	    error = error + r*r
	    wt = wt + 1
	  endif
	enddo
c
	if(inf)then
	  eval = (n-1) * (error/wt)
	else
	  eval = (n-1) * sqrt(error/wt)
	endif
	end
