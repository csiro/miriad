c************************************************************************
	program mfcal
	implicit none
c
c= MfCal -- Multifrequency antenna and passband calibration.
c& rjs
c: calibration
c+
c	MfCal is a Miriad task which determines calibration corrections
c	(antenna gains, delay terms and passband shapes) from a
c	multi-frequency observation.  The delays and passband are
c	determined from an average of all the selected data.  The gains
c	are worked out periodically depending upon the user 
c	selected interval.
c@ vis
c	Input visibility data file. No default. This can (indeed should)
c	contain multiple channels and spectral windows. The frequency
c	set-up can vary with time.
c@ line
c	Standard line parameter, with standard defaults.
c@ edge
c	The number of channels, at the edges of each spectral window, that
c	are to be dropped. Either one or two numbers can be given, being the
c	number of channels at the start and end of each spectral window to be
c	dropped. If only one number is given, then this number of channels
c	is dropped from both the start and end. The default value is 0.
c@ select
c	Standard uv selection. Default is all data.
c@ flux
c	Three numbers, giving the source flux, the reference frequency
c	(in GHz) and the source spectral index. The flux and spectral index
c	are at the reference frequency. If not values are given, then MFCAL
c	checks whether the source is one of its known sources, and uses the
c	appropriate flux variation with frequency. Otherwise the default flux
c	is determined so that the rms gain amplitude is 1, and the default
c	spectral index is 0. The default reference frequency is the mean of
c	the frequencies in the input data. Also see the `oldflux' option.
c@ refant
c	The reference antenna. Default is 3. The reference antenna needs
c	to be present throughout the observation. Any solution intervals
c	where the reference antenna is missing are discarded.
c@ minants
c	The minimum number of antennae that must be present before a
c	solution is attempted. Default is 2.
c@ interval
c	This gives one or two numbers, both given in minutes, both being
c	used to determine the extents of the gains calibration solution
c	interval. The first gives the max length of a solution interval. The
c	second gives the max gap size in a solution interval. A new solution
c	interval is started when either the max times length is exceeded, or a
c	gap larger than the max gap is encountered. The default max length is
c	5 minutes, and the max gap size is the same as the max length.
c@ options
c	Extra processing options. Several values can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  delay     Attempt to solve for the delay parameters. This can
c	            be a large sink of CPU time.
c	  nopassol  Do not solve for bandpass shape. In this case if a bandpass
c	            table is present in the visibility data-set, then it will
c	            be applied to the data.
c	  interpolate Interpolate (and extrapolate) via a spline fit (to
c	            the real and imaginary parts) bandpass values for
c	            channels with no solution (because of flagging).  If 
c	            less than 50% of the channels are unflagged, the 
c	            interpolation (extrapolation) is not done and those
c	            channels will not have a bandpass solution
c	  oldflux   This causes MFCAL to use a pre-August 1994 ATCA flux
c	            density scale. See the help on "oldflux" for more
c	            information.
c@ tol
c	Solution convergence tolerance. Default is 0.001.
c--
c  History:
c    rjs   8jul92 Original version.
c    rjs   1oct92 Time averaging.
c    rjs  19oct92 Simple polarisation handling.
c    rjs  30oct92 Change minant to minants.
c    rjs  22nov92 Fixed dimensioning of b1,b2 in Solve.
c    rjs  19jan93 Fix bug handling single IF.
c    rjs   9feb93 Use memalloc more, to reduce size of executable.
c    rjs  19mar93 Discard autocorrelation data.
c    nebk 18jun93 Documentation clarification
c    rjs  24jun93 Determine nants in a different way.
c    rjs   9jul93 Increase MAXHASH, MAXVIS. Other fiddles to increase buffers.
c    rjs  30mar94 Added some comments.
c    nebk 30mar94 Add options=interpolate
c    rjs   3aug94 Add options=oldflux.
c    rjs  23aug94 Improve hashing algorithm.
c    rjs   6sep94 Improve error message.
c    rjs   5oct94 Support linetype averaging.
c    rjs  13jan96 Increase MAXHASH.
c    rjs  27jul96 Re-wrote interp option to make it more robust.
c    rjs  28jul97 Make options=delay more robust.
c    rjs  31jul97 Make it work for wide-only files.
c    rjs  20jan99 The frequency assigned to a channel was not being
c		  correctly computed when line width was not equal to
c		  line step.
c
c  Problems:
c    * Should do simple spectral index fit.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSPECT,MAXVIS,MAXSOLN,MAXITER,MAXPOL
	parameter(MAXSPECT=33,MAXVIS=700000,MAXITER=30,MAXSOLN=1024)
	parameter(MAXPOL=2)
c
	character version*(*)
	parameter(version='MfCal: version 1.0 20-Jan-99')
c
	integer tno
	integer pWGains,pFreq,pSource,pPass,pGains,pTau
	integer pOPass,pOGains,pOTau
	integer nvis,VID(MAXVIS)
	integer nspect,nschan(MAXSPECT),ischan(MAXSPECT)
	integer npol,nants,nsoln,Count(MAXSOLN),nchan,refant,minant
	integer niter,edge(2),PolMap(MAXPOL),pee(MAXPOL)
	real flux(3),tol,epsi
	double precision freq0,sfreq(MAXSPECT),sdf(MAXSPECT)
	double precision freqc(MAXSPECT)
	double precision interval(2),time(MAXSOLN)
	complex Vis(MAXVIS)
	real Wt(MAXVIS)
	character line*64,uvflags*16,Source*64
	logical dodelay,dopass,defflux,interp,oldflux
c
c  Dynamic memory stuff.
c
	real ref(MAXBUF)
	complex cref(MAXBUF/2)
	double precision dref(MAXBUF/2)
	equivalence(ref,cref,dref)
	common ref
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
c
c  Get inputs and check them.
c
	call output(version)
	call keyini
	call GetOpt(dodelay,dopass,interp,oldflux)
	uvflags = 'dlbx'
	if(.not.dopass)uvflags(5:5) = 'f'
	call uvDatInp('vis',uvflags)
	call keyi('refant',refant,3)
	call keyi('minants',minant,2)
	defflux = .not.keyprsnt('flux')
	call keyr('flux',flux(1),1.0)
	call keyr('flux',flux(2),0.0)
	call keyr('flux',flux(3),0.0)
	call keyi('edge',edge(1),0)
	call keyi('edge',edge(2),edge(1))
	call keyd('interval',interval(1),5.0d0)
	call keyd('interval',interval(2),interval(1))
	call keyr('tol',tol,0.001)
	call keyfin
c
	if(minant.lt.2)
     *	  call bug('f','Bad value minant parameter')
	if(refant.le.0)
     *	  call bug('f','Bad value for the reference antenna parameter')
	if(flux(1).le.0.or.flux(2).lt.0)
     *	  call bug('f','Bad values for the flux parameter')
	if(edge(1).lt.0.or.edge(2).lt.0)
     *	  call bug('f','Bad value for the edge parameter')
c
	if(interval(1).le.0.or.interval(2).le.0)
     *	  call bug('f','Bad value for the interval parameter')
	interval(2) = min(interval(1),interval(2))
	interval(1) = interval(1) / (24*60)
	interval(2) = interval(2) / (24*60)
c
	if(tol.le.0.or.tol.ge.1)
     *	  call bug('f','Bad value for the tol parameter')
c
c  Open the input file.
c
	if(.not.uvDatOpn(tno))call bug('f','Error opening input file')
	call HisOpen(tno,'append')
	call HisWrite(tno,'MFCAL: '//version)
	call HisInput(tno,'MFCAL')
c
c  Get the input data.
c
	call output('Reading the data ...')
	call DatRead(tno,MAXVIS,nvis,npol,Vis,Wt,VID,
     *		     MAXSPECT,nspect,sfreq,sdf,nschan,nants,
     *		     MAXSOLN,nsoln,time,Count,minant,refant,interval,
     *		     edge,Source,PolMap)
c
c  Check that the polarisations present are commensurate.
c
	call PolCheck(npol,PolMap,pee)
c
c  Determine the nschan thingo.
c
	call output('Initialising ...')
	call ChanCvt(nspect,nschan,nchan,ischan)
c
c  Determine the reference frequency.
c
	freq0 = flux(2)
	if(freq0.le.0)call AverFreq(nspect,nschan,sfreq,sdf,freq0)
c
c  Generate the frequencies for each channel in the total passband.
c
	call MemAlloc(pFreq,nchan,'d')
	call FreqGen(nspect,nschan,sfreq,sdf,
     *				freqc,dref(pFreq),nchan)
c
c  Generate the source model.
c
	call MemAlloc(pSource,nchan,'r')
	call SrcGen(Source,oldflux,defflux,
     *	  ref(pSource),nchan,dref(pFreq),freq0,flux(1),flux(3))
c
c  Now make the frequency relative to the reference frequency.
c
	call FreqRel(dref(pFreq),freq0,nchan)
	call FreqRel(freqc,freq0,nspect)
c
c  Allocate some extra memory.
c
	call MemAlloc(pPass,nants*nchan*npol,'c')
	call MemAlloc(pGains,nants*nsoln*npol,'c')
	call MemAlloc(pTau,nants*nsoln,'r')
	call MemAlloc(pOPass,nants*nchan*npol,'c')
	call MemAlloc(pOGains,nants*nsoln*npol,'c')
	call MemAlloc(pOTau,nants*nsoln,'r')
c
c  Get an initial estimate of the wide gains and passband.
c
	call output('Generating initial solution estimate ...')
	call MemAlloc(pWGains,nants*nspect*nsoln*npol,'c')
	call WGIni(Vis,Wt,VID,ischan,nvis,npol,Count,nsoln,nchan,
     *	  nants,nspect,ref(pSource),cref(pWGains),refant,minant)
c
	call BPIni(npol,nants,nchan,nsoln,nspect,nschan,cref(pWGains),
     *	  freqc,cref(pPass),cref(pGains),ref(pTau),dodelay,dopass)
	call MemFree(pWGains,nants*nspect*nsoln*npol,'c')
c
c  Normalise the gains, and make a copy for later comparison.
c
	if(dopass)call Norm(npol,nants,nchan,nsoln,cref(pPass),
     *	  cref(pGains),ref(pTau),dref(pFreq))
c
	call GainCpy(npol,nants,nchan,nsoln,cref(pPass),cref(pGains),
     *	  ref(pTau),cref(pOPass),cref(pOGains),ref(pOTau))
c
c  We have estimates of the antenna gains (Gains), the delay term
c  (Tau) and the passbands (Pass). Perform the main solver iterations.
c
	call output('Doing solution refinement ...')
	niter = 0
	epsi = 1
	dowhile(epsi.gt.tol.and.niter.lt.MAXITER)
	  niter = niter + 1
c
c  Get the antenna gains and delay.
c
	  call SolveGT(refant,minant,nants,nspect,nchan,nsoln,
     *	    cref(pPass),ref(pSource),dref(pFreq),Vis,Wt,VID,ischan,
     *	    Count,nvis,npol,cref(pGains),ref(pTau),
     *	    dodelay.and.niter.ne.1,tol)
c
c  Get the passband.
c
	  if(dopass)call SolveBP(refant,minant,nants,nspect,nchan,nsoln,
     *	    cref(pPass),ref(pSource),dref(pFreq),Vis,Wt,VID,ischan,
     *	    Count,nvis,npol,cref(pGains),ref(pTau),tol)
c
c  Normalise the total gains so that the average delay is zero and
c  the rms passband gain is 1.
c
	  if(dopass)call Norm(npol,nants,nchan,nsoln,cref(pPass),
     *	    cref(pGains),ref(pTau),dref(pFreq))
c
c  Compare the solution with previous solutions.
c
	  call GainCmp(npol,nants,nchan,nsoln,cref(pPass),cref(pGains),
     *		ref(pTau),cref(pOPass),cref(pOGains),ref(pOTau),epsi)
c
c  Keep the user awake.
c
	  write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *				     ', Solution Error:',epsi
	  call output(line)
	enddo
c
c  Scale the gains if we have no idea what the source flux really was.
c
	if(defflux)then
	  call GainScal(flux,cref(pGains),npol*nants*nsoln)
	  write(line,'(a,f8.3)')'I flux density: ',flux(1)
	  call output(line)
	endif
c
	if(epsi.gt.tol)call bug('w','Failed to converge')
	call output('Saving solution ...')
	call GainTab(tno,time,cref(pGains),ref(pTau),npol,nants,nsoln,
     *	  freq0,dodelay,pee)
	if (dopass.and.interp) call intext(npol,nants,nchan,nspect,
     *    nschan,cref(pPass))
	if(dopass)call PassTab(tno,npol,nants,nchan,
     *	  nspect,sfreq,sdf,nschan,cref(pPass),pee)
c
c  Free up all the memory, and close down shop.
c
	call MemFree(pOTau,nants*nsoln,'r')
	call MemFree(pOGains,nants*nsoln*npol,'c')
	call MemFree(pOPass,nants*nchan*npol,'c')
	call MemFree(pTau,nants*nsoln,'r')
	call MemFree(pGains,nants*nsoln*npol,'c')
	call MemFree(pPass,nants*nchan*npol,'c')
	call MemFree(pSource,nchan,'r')
	call MemFree(pFreq,nchan,'d')
	call hisclose(tno)
	call uvDatCls
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
c  Scale the flux.
c
	t = Sum2/n
	flux = t*flux
c
	end
c************************************************************************
	subroutine PolCheck(npol,PolMap,pee)
c
	implicit none
	integer npol,PolMap(npol),pee(npol)
c
c  This checks that the polarisations present are commensurate (either
c  both circulars or both linears).
c  It also initialises the array which determines the order that the
c  solutions are written out in.
c
c------------------------------------------------------------------------
	pee(1) = 1
	if(npol.eq.1)return
	if(npol.gt.2)call bug('f','Something is screwy')
	if(abs(PolMap(1)-PolMap(2)).ne.1)call bug('f',
     *	    'Incommensurate polarisations selected')
	if(PolMap(2).gt.PolMap(1))then
	  pee(1) = 2
	  pee(2) = 1
	else
	  pee(1) = 1
	  pee(2) = 2
	endif
	end
c************************************************************************
	subroutine GetOpt(dodelay,dopass,interp,oldflux)
c
	implicit none
	logical dodelay,dopass,interp,oldflux
c
c  Get extra processing options.
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=4)
	logical present(nopt)
	character opts(nopt)*11
	data opts/ 'delay      ','nopassol   ','interpolate',
     *		   'oldflux    '/
c
	call options('options',opts,present,nopt)
c
	dodelay =      present(1)
	dopass  = .not.present(2)
        interp  =      present(3)
	oldflux =      present(4)
	end
c************************************************************************
	subroutine GainTab(tno,time,Gains,Tau,npol,nants,nsoln,
     *						freq0,dodelay,pee)
c
	implicit none
	integer tno,nants,nsoln,npol,pee(npol)
	double precision time(nsoln),freq0
	real Tau(nants,nsoln)
	complex Gains(nants,npol,nsoln)
	logical dodelay
c
c  Write out the antenna gains and the delays.
c
c  Input:
c    tno
c    time
c    Gains
c    Tau
c    npol	Number of polarisations. Either 1 or 2.
c    nants
c    nsoln
c    dodelay	True if the delays are to be written out.
c    pee	Mapping from internal polarisation number to the order
c		that we write the gains out in.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer iostat,off,item,i,j,p,pd,j1,ngains
	complex G(3*MAXANT)
c
	call haccess(tno,item,'gains','write',iostat)
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
	ngains = npol*nants
	if(dodelay) ngains = (npol+1)*nants 
c
	off = 8
	do i=1,nsoln
	  call hwrited(item,time(i),off,8,iostat)
	  off = off + 8
	  if(iostat.ne.0)then
	    call bug('w','Error writing time to amp/phase table')
	    call bugno('f',iostat)
	  endif
	  j1 = 1
	  do j=1,nants
	    do p=1,npol
	      pd = pee(p)
	      if(abs(real( Gains(j,pd,i)))+
     *		 abs(aimag(Gains(j,pd,i))).ne.0)then
	        G(j1) = 1/Gains(j,pd,i)
	      else
	        G(j1) = (0.,0.)
	      endif
	      j1 = j1 + 1
	    enddo
	    if(dodelay)then
	      G(j1) = cmplx(0.,-2*pi*Tau(j,i))
	      j1 = j1 + 1
	    endif
	  enddo
	  call hwriter(item,G,off,8*ngains,iostat)
	  off = off + 8*ngains
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
	call wrhdi(tno,'nfeeds',npol)
	call wrhdi(tno,'ngains',ngains)
	call wrhdi(tno,'nsols',nsoln)
	call wrhdd(tno,'interval',0.5d0)
	if(dodelay)then
	  call wrhdi(tno,'ntau',1)
	  call wrhdd(tno,'freq0',freq0)
	else
	  call wrhdi(tno,'ntau',0)
	endif
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
c  Write out the bandpass table and frequency description table (with a
c  few other assorted parameters). This assumes that the parameters
c    ngains, nfeeds
c  have already been written out.
c
c  Input:
c    tno	Handle of the output dataset.
c    nants	Number of antennas.
c    npol	Number of polarisations (either 1 or 2).
c    nspect	The total number of frequency bands observed. This is the
c		product of the number of simultaneous spectral windows and
c		the number of different frequency settings.
c    nschan	The number of channels in each observing band.
c    nchan	Total number of channels.
c		NOTE: Here (as elsewhere in this task) "nchan" is the total
c		number of channels (the sum of all the channels from all the
c		bands observed).
c		i.e. nchan = Sum nschan(i)
c    Pass	The bandpass function. This is of size nants * nchan * npol.
c		The bandpass table that we have to write out is in the order
c		nchan * npol * nants, so we have to do some reorganising
c		before we write out.
c    pee	Mapping from internal polarisation number to the order
c		that we write the gains out in. We always write X then Y
c		(or R then L).
c    sdf	Frequency increment for each observing band.
c    sfreq	Start frequency for each observing band.
c------------------------------------------------------------------------
	include 'maxdim.h'
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
c  Write out all the gains. Write one antenna and one polarisation at
c  a time. Because the input ("Pass") is in antenna/channel/pol order,
c  and the output table is in channel/pol/antenna order, we have to
c  rearraneg before writing out. Also convert from a "error" to a "correction"
c  by taking the inverse. 
c  Because "nchan" is the sum of all the channels from the frequency
c  bands observed, nchan may be larger than MAXCHAN. To cope with this,
c  copy the output channels in a strip-mining approach.
c
c  Loop over antenna, polarisation, strip, and channel within a strip.
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
c
c  Write a strip, and check for errors.
c
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
c************************************************************************
	subroutine Norm(npol,nants,nchan,nsoln,Pass,Gains,Tau,freq)
c
	implicit none
	integer npol,nants,nchan,nsoln
	complex Pass(nants,nchan,npol),Gains(nants,npol,nsoln)
	real Tau(nants,nsoln)
	double precision freq(nchan)
c
c  Normalise the total gains so that the average delay is zero and the
c  rms passband gain is 1.
c
c  Input:
c    npol	Number of intensity polarisations -- either 1 or 2.
c    nants
c    nchan
c    nsoln
c    freq
c  Input/Output:
c    Pass	Passband function.
c    Gains	Antenna gains.
c    Tau	Delay.
c------------------------------------------------------------------------
	integer MAXPOL
	parameter(MAXPOL=2)
	include 'maxdim.h'
	include 'mirconst.h'
	real SumTau(MAXANT),RmsPass(MAXANT,MAXPOL),temp,theta
	integer i,j,p,nPass(MAXANT,MAXPOL)
c
c  Zero the accumulators.
c
	do p=1,npol
	  do i=1,nants
	    SumTau(i) = 0
	    RmsPass(i,p) = 0
	    nPass(i,p) = 0
	  enddo
	enddo
c
c  Accumulate the average delay.
c
	do j=1,nsoln
	  do i=1,nants
	    SumTau(i) = SumTau(i) + Tau(i,j)
	  enddo
	enddo
c
c  Accumulate the rms passband gain.
c
	do p=1,npol
	  do j=1,nchan
	    do i=1,nants
	      temp = real(Pass(i,j,p))**2 + aimag(Pass(i,j,p))**2
	      if(temp.gt.0)then
	        nPass(i,p) = nPass(i,p) + 1
	        RmsPass(i,p) = RmsPass(i,p) + temp
	      endif
	    enddo
	  enddo
	enddo
c
c  Calculate the average delay and rms passband gain.
c
	do i=1,nants
	  SumTau(i) = SumTau(i) / nsoln
	  do p=1,npol
	    if(nPass(i,p).gt.0)then
	      RmsPass(i,p) = sqrt(RmsPass(i,p)/nPass(i,p))
	    else
	      RmsPass(i,p) = 1
	    endif
	  enddo
	enddo
c
c  Correct the delay and antenna gains.
c
	do j=1,nsoln
	  do i=1,nants
	    Tau(i,j) = Tau(i,j) - SumTau(i)
	  enddo
	enddo
c
	do p=1,npol
	  do j=1,nsoln
	    do i=1,nants
	      Gains(i,p,j) = Gains(i,p,j) * RmsPass(i,p)
	    enddo
	  enddo
	enddo
c
c  Correct the passband gains.
c
	do p=1,npol
	  do j=1,nchan
	    do i=1,nants
	      theta = 2*pi * SumTau(i) * freq(j)
	      Pass(i,j,p) = Pass(i,j,p) * cmplx(cos(theta),sin(theta))
     *							/ RmsPass(i,p)
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GainCpy(npol,nants,nchan,nsoln,Pass,Gains,
     *		Tau,OPass,OGains,OTau)
c
	implicit none
	integer npol,nants,nchan,nsoln
	complex Pass(nants,nchan,npol),Gains(nants,npol,nsoln)
	complex OPass(nants,nchan,npol),OGains(nants,npol,nsoln)
	real Tau(nants,nsoln),OTau(nants,nsoln)
c
c  Copy the current gains to the old gains.
c------------------------------------------------------------------------
	integer i,j,p
c
	do p=1,npol
	  do j=1,nchan
	    do i=1,nants
	      OPass(i,j,p) = Pass(i,j,p)
	    enddo
	  enddo
	enddo
c
	do p=1,npol
	  do j=1,nsoln
	    do i=1,nants
	      OGains(i,p,j) = Gains(i,p,j)
	    enddo
	  enddo
	enddo
c
	do j=1,nsoln
	  do i=1,nants
	    OTau(i,j) = Tau(i,j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GainCmp(npol,nants,nchan,nsoln,Pass,Gains,
     *		Tau,OPass,OGains,OTau,epsi)
c
	implicit none
	integer npol,nants,nchan,nsoln
	complex Pass(nants,nchan,npol),Gains(nants,npol,nsoln)
	complex OPass(nants,nchan,npol),OGains(nants,npol,nsoln)
	real Tau(nants,nsoln),OTau(nants,nsoln),epsi
c
c  Copy and compare the current and old gains.
c------------------------------------------------------------------------
	integer i,j,p
	real Change,Sum2,rtemp
	complex temp
c
	epsi = 0
c
	Change = 0
	Sum2 = 0
	do p=1,npol
	  do j=1,nchan
	    do i=1,nants
	      temp = Pass(i,j,p) - OPass(i,j,p)
	      OPass(i,j,p) = Pass(i,j,p)
	      Change = Change + real(temp)**2 + aimag(temp)**2
	      Sum2 = Sum2 + real( Pass(i,j,p))**2
     *			  + aimag(Pass(i,j,p))**2
	    enddo
	  enddo
	enddo
	if(Sum2.gt.0)epsi = max(epsi, Change / Sum2)
c
	Change = 0
	Sum2 = 0
	do p=1,npol
	  do j=1,nsoln
	    do i=1,nants
	      temp = Gains(i,p,j) - OGains(i,p,j)
	      OGains(i,p,j) = Gains(i,p,j)
	      Change = Change + real(temp)**2 + aimag(temp)**2
	      Sum2 = Sum2 + real( Gains(i,p,j))**2
     *			  + aimag(Gains(i,p,j))**2
	    enddo
	  enddo
	enddo
	if(Sum2.gt.0)epsi = max(epsi, Change / Sum2)
c
	Change = 0
	Sum2 = 0
	do j=1,nsoln
	  do i=1,nants
	    rtemp = Tau(i,j) - OTau(i,j)
	    OTau(i,j) = Tau(i,j)
	    Change = Change + rtemp*rtemp
	    Sum2 = Sum2 + Tau(i,j)*Tau(i,j)
	  enddo
	enddo
	if(Sum2.gt.0)epsi = max(epsi, Change / Sum2)
c
	epsi = sqrt(epsi)
c
	end
c************************************************************************
	subroutine SrcGen(Source,oldflux,defflux,sflux,nchan,freq,
     *						freq0,flux,alpha)
c
	implicit none
	integer nchan
	real flux,alpha,sflux(nchan)
	double precision freq(nchan),freq0
	logical defflux,oldflux
	character Source*(*)
c
c  Generate the source flux as a function of frequency.
c
c  Input:
c    Source	Source name.
c    nchan	Number of spectral channels.
c    freq	Offset frequency of each channel.
c    freq0	Reference frequency.
c    flux	Source flux at the reference frequency.
c    alpha	Spectral index.
c    oldflux	True if we are to use the old ATCA 1934 flux scales.
c  Input/Output:
c    defflux	Check for a known source, and use this if possible.
c  Output:
c    source	Flux of the source at each frequency.
c------------------------------------------------------------------------
	integer i,ierr
	character umsg*64,src*16
c
	ierr = 2
	src = source
	if(src.eq.'1934-638'.or.src.eq.'1934'.or.
     *			src.eq.'1939-637')then
	  if(oldflux)then
	    src = 'old1934'
	    call output('Using pre-Aug94 ATCA flux scale for 1934-638')
	  else
	    call bug('w',
     *			'Using post-Aug94 ATCA flux scale for 1934-638')
	  endif
	endif
	if(defflux)call CalStoke(src,'i',freq,sflux,nchan,ierr)
	if(ierr.eq.2)then
	  if(alpha.eq.0)then
	    do i=1,nchan
	      sflux(i) = flux
	    enddo
	  else
	    do i=1,nchan
	      sflux(i) = flux * (freq(i)/freq0) ** alpha
	    enddo
	  endif
	else if(ierr.eq.1)then
	  defflux = .false.
	  umsg = 'Extrapolating to get frequency variation of '//source
	  call bug('w',umsg)
	else
	  defflux = .false.
	  umsg = 'Using known frequency variation of '//source
	  call output(umsg)
	endif
c
	end
c************************************************************************
	subroutine AverFreq(nspect,nschan,sfreq,sdf,freq0)
c
	implicit none
	integer nspect,nschan(nspect)
	double precision sfreq(nspect),sdf(nspect),freq0
c
c  Determine the average frequency of the data.
c------------------------------------------------------------------------
	integer i,nchan
c
	nchan = 0
	freq0 = 0
	do i=1,nspect
	  nchan = nchan + nschan(i)
	  freq0 = freq0 + nschan(i)*(sfreq(i)+0.5*(nschan(i)-1)*sdf(i))
	enddo
c
	freq0 = freq0 / nchan
	end
c************************************************************************
	subroutine FreqRel(freq,freq0,nchan)
c
	implicit none
	integer nchan
	double precision freq(nchan),freq0
c
c  Subtract off the reference frequency.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  freq(i) = freq(i) - freq0
	enddo
	end
c************************************************************************
	subroutine FreqGen(nspect,nschan,sfreq,sdf,freqc,freq,nchan)
c
	implicit none
	integer nchan,nspect,nschan(nspect)
	double precision sfreq(nspect),sdf(nspect),freq(nchan)
	double precision freqc(nspect)
c
c  Generate the frequency corresponding to each channel.
c
c  Input:
c    sfreq
c    sdf
c    nschan
c  Output:
c    freqc	The average offset frequency of each window.
c    freq	The offset frequency corresponding to each channel.
c------------------------------------------------------------------------
	integer i,j,off
c
	off = 0
	do j=1,nspect
	  freqc(j) = sfreq(j) + 0.5*sdf(j)*(nschan(j)-1)
	  do i=1,nschan(j)
	    off = off + 1
	    freq(off) = sfreq(j) + sdf(j) * (i-1)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine ChanCvt(nspect,nschan,nchan,ischan)
c
	implicit none
	integer nspect,nschan(nspect),nchan,ischan(nspect)
c
c  Determine the ischan array.
c
c  Input:
c    nspect
c    nschan
c    nvis
c  Output:
c    nchan
c    ischan
c------------------------------------------------------------------------
	integer i
c
	nchan = 0
	do i=1,nspect
	  ischan(i) = nchan
 	  nchan = nchan + nschan(i)
	enddo
c
	end
c************************************************************************
	subroutine WGIni(Vis,Wt,VID,ischan,nvis,npol,Count,nsoln,nchan,
     *	  nants,nspect,Source,WGains,refant,minant)
c
	implicit none
	integer nvis,nsoln,nants,nspect,nchan,minant,refant,npol
	complex Vis(nvis),WGains(nants,nspect,npol,nsoln)
	integer VID(nvis),Count(nsoln),ischan(nspect)
	real Source(nchan),Wt(nvis)
c
c  Estimate the "wide" gains. The gains in each spectral window are
c  solved for independently.
c
c  Input:
c    Vis
c    Wt
c    VID
c    nvis
c    Count
c    nsoln
c    nants
c    nspect
c    nchan
c    Source
c    ischan
c  Output:
c    WGains
c------------------------------------------------------------------------
	integer MAXPOL
	parameter(MAXPOL=2)
	include 'maxdim.h'
c
	integer i,j,k,i1,i2,bl,spect,chan,off,nbl,p
	complex SumVM(MAXBASE,MAXWIN,MAXPOL)
	real SumMM(MAXBASE,MAXWIN,MAXPOL),epsi
c
	nbl = nants*(nants-1)/2
c
	off = 0
	do k=1,nsoln
	  do p=1,npol
	    do j=1,nspect
	      do i=1,nbl
	        SumVM(i,j,p) = 0
	        SumMM(i,j,p) = 0
	      enddo
	    enddo
	  enddo
c
c  Accumulate the data for this solution interval.
c
	  do i=1,Count(k)
	    off = off + 1
	    call unpack(i1,i2,p,spect,chan,VID(off))
	    chan = chan + ischan(spect)
	    bl = (i2-1)*(i2-2)/2 + i1
	    SumVM(bl,spect,p) = SumVM(bl,spect,p) +
     *			Vis(off)*Source(chan)
	    SumMM(bl,spect,p) = SumMM(bl,spect,p) +
     *			Wt(off)*Source(chan)**2
	  enddo
c
c  Solve for the gains for this interval.
c
	  do p=1,npol
	    do i=1,nspect
	      call Solve(nants,nbl,SumVM(1,i,p),SumMM(1,i,p),
     *		WGains(1,i,p,k),refant,minant,1e-4,epsi,.true.)
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine DatRead(tno,maxvis,nvis,npol,Vis,Wt,VID,
     *			maxspect,nspect,sfreq,sdf,nschan,nants,
     *			maxsoln,nsoln,time,Count,minant,refant,interval,
     *			edge,Source,PolMap)
c
	implicit none
	integer tno,maxvis,nvis,maxspect,nspect,nants,maxsoln,nsoln
	integer minant,refant,npol
	double precision time(maxsoln),interval(2)
	double precision sfreq(maxspect),sdf(maxspect)
	integer nschan(maxspect),Count(maxsoln),edge(2)
	complex Vis(maxvis)
	real Wt(maxvis)
	integer VID(maxvis),PolMap(*)
	character Source*(*)
c
c  Read the data, and return information on what we have read.
c
c  Input:
c    tno
c    maxvis
c    maxspect
c    maxsoln
c    minant
c    refant
c    interval
c    edge
c  Output:
c    nvis
c    nspect
c    nants
c    nsoln
c    Source	The source name.
c    npol	Number of polarisations.
c    PolMap	Map between internal polarisation number and external
c		polarisation. Must be of dimension at least MAXPOL.
c    time
c    sfreq
c    sdf
c    nschan
c    VID
c    Wt
c    Vis
c    Count
c------------------------------------------------------------------------
	integer PolXX,PolYY,PolRR,PolLL,PolI
	parameter(PolXX=-5,PolYY=-6,PolRR=-1,PolLL=-2,PolI=1)
	integer PolMin,PolMax
	parameter(PolMin=-6,PolMax=1)
	include 'maxdim.h'
	integer MAXHASH,MAXPOL
	parameter(MAXHASH=2*MAXANT*MAXCHAN,MAXPOL=2)
c
	integer nchan,nbad,nauto,nreg,ngood,ninter,i1,i2,p,i,VisId
	double precision preamble(4),tfirst,tlast
	complex Data(MAXCHAN)
	logical flags(MAXCHAN),present(MAXANT,MAXPOL),updated,ok
	integer chan(MAXCHAN),spect(MAXCHAN),state(MAXCHAN)
	integer Hash(2,MAXHASH),vupd
	integer pols(PolMin:PolMax)
c
c  Externals.
c
	logical uvVarUpd,accept
	character itoaf*8
c
c  Is the size of the "state" array OK?
c
	if(3*(maxspect+2).gt.MAXCHAN)
     *	  call bug('f','State array too small in DatRead')
c
c  Initialise thing.
c
	call uvVarIni(tno,vupd)
	call uvVarSet(vupd,'nspect')
	call uvVarSet(vupd,'sfreq')
	call uvVarSet(vupd,'sdf')
	call uvVarSet(vupd,'nschan')
	call uvVarSet(vupd,'wfreq')
	call uvVarSet(vupd,'wwidth')
	call uvselect(tno,'and',0.d0,0.d0,.true.)
	call uvselect(tno,'polarization',dble(PolXX),0.d0,.true.)
	call uvselect(tno,'polarization',dble(PolYY),0.d0,.true.)
	call uvselect(tno,'polarization',dble(PolRR),0.d0,.true.)
	call uvselect(tno,'polarization',dble(PolLL),0.d0,.true.)
	call uvselect(tno,'polarization',dble(PolI),0.d0,.true.)
c
	do p=PolMin,PolMax
	  pols(p) = 0
	enddo
	npol = 0
c
	nsoln = 0
	nspect = 0
	nvis = 0
c
	updated = .false.
	tfirst = 0
	tlast = 0
	nbad = 0
	nauto = 0
	nreg = 0
	ngood = 0
c
	do p=1,MAXPOL
	  do i=1,MAXANT
	    present(i,p) = .false.
	  enddo
	enddo
c
c  Loop over everything.
c
	call uvDatRd(preamble,Data,flags,MAXCHAN,nchan)
	call uvrdvra(tno,'source',source,' ')
	call uvrdvri(tno,'nants',nants,0)
	if(nants.le.0.or.nants.gt.MAXANT)
     *	  call bug('f','Bad value for nants, in DatRead')
	dowhile(nchan.gt.0)
	  updated = updated.or.uvVarUpd(vupd)
	  call BasAnt(preamble(4),i1,i2)
	  call uvDatGti('pol',p)
	  ok = i1.gt.0.and.i1.le.MAXANT.and.
     *	       i2.gt.0.and.i2.le.MAXANT.and.
     *	       p.ge.PolMin.and.p.le.PolMax
c
c  Finish up an old solution interval, and initialise a new slot.
c
	  if(ok)then
	    if(	nsoln.eq.0.or.
     *		preamble(3).gt.tfirst+interval(1).or.
     *		preamble(3).gt.tlast+interval(2))then
	      if(nsoln.ne.0)then
		time(nsoln) = (tfirst+tlast)/2
		if(.not.accept(present,nants,npol,refant,minant,MAXANT)
     *								   )then
		  nvis = nvis - Count(nsoln)
		  nreg = nreg + ninter
		  nsoln = nsoln - 1
		else
		  ngood = ngood + ninter
		endif
	      endif
	      nsoln = nsoln + 1
	      if(nsoln.gt.maxsoln)
     *		call bug('f','Too many solution intervals')
	      tfirst = preamble(3)
	      tlast  = tfirst
	      ninter = 0
	      Count(nsoln) = 0
	      call AccumIni(Hash,MAXHASH)
	    else if(preamble(3).lt.tfirst)then
	      call bug('f','Data does not appear to be in time order')
 	    endif
c
c  Determine the polarisation number.
c
	    if(pols(p).eq.0)then
	      npol = npol + 1
	      if(npol.gt.MAXPOL)
     *		call bug('f','Too many different polarisations')
	      pols(p) = npol
	      PolMap(npol) = p
	    endif
	    p = pols(p)
c
	    if(nchan+nvis.gt.maxvis)
     *	      call bug('f','Buffer overflow: set interval larger')
c
	    tlast = max(tlast,preamble(3))
c
	    call despect(updated,tno,nchan,edge,chan,spect,
     *		maxspect,nspect,sfreq,sdf,nschan,state)
c
	    do i=1,nchan
	      if(flags(i).and.chan(i).gt.0)then
	        present(i1,p) = .true.
	        present(i2,p) = .true.
	        call pack(i1,i2,p,spect(i),chan(i),VisId)
		ninter = ninter + 1
		call Accum(Hash,Data(i),VisId,
     *			nsoln,nvis,Vis,Wt,VID,Count)
	      else
	        nbad = nbad + 1
	      endif
	    enddo
c
	  else
	    nauto = nauto + 1
	  endif
	  call uvDatRd(preamble,Data,flags,MAXCHAN,nchan)
	enddo
c
c  Check if the last time interval is to be accepted.
c
	if(nsoln.ne.0)then
	  time(nsoln) = (tfirst+tlast)/2
	  if(.not.accept(present,nants,npol,refant,minant,MAXANT))then
	    nvis = nvis - Count(nsoln)
	    nreg = nreg + ninter
	    nsoln = nsoln - 1
	  else
	    ngood = ngood + ninter
	  endif
	endif
c
c  Tell the user whats what.
c
	call output('Number of solution intervals: '
     *    //itoaf(nsoln))
	if(nauto.ne.0)
     *	  call bug('w','Number of autocorrelations discarded: '
     *	  //itoaf(nauto))
	if(nbad.ne.0)
     *	  call bug('w','Correlations flagged or edge-rejected: '
     *	  //itoaf(nbad))
	if(nreg.ne.0)
     *	  call bug('w','Number correlations lacking minant/refant: '
     *	  //itoaf(nreg))
	call output('Number correlations accepted: '
     *	  //itoaf(ngood))
	call output('Number of frequency bands/settings: '
     *	  //itoaf(nspect))
	call output('Number of polarisations selected: '
     *	  //itoaf(npol))
	if(nsoln.eq.0.or.nvis.eq.0)
     *	  call bug('f','No data to process!')
	end
c************************************************************************
	subroutine pack(i1,i2,p,spect,chan,VID)
c
	implicit none
	integer i1,i2,p,spect,chan,VID
c
c  Pack antenna and polarisation numbers into one number.
c
c  Input:
c    i1
c    i2
c    p
c    spect
c    chan
c  Output:
c    VID
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXPOL
	parameter(MAXPOL=2)
	VID = chan - 1
	VID = MAXANT * VID + i1 - 1
	VID = MAXANT  *VID + i2 - 1
	VID = MAXPOL  *VID + p  - 1
	VID = MAXWIN  *VID + spect - 1
	end
c************************************************************************
	subroutine unpack(i1,i2,p,spect,chan,VID)
c
	implicit none
	integer i1,i2,p,spect,chan,VID
c
c  Unpack antenna and polarisation number.
c
c  Input:
c    VID
c  Output:
c    i1
c    i2
c    p
c    spect
c    chan
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXPOL
	parameter(MAXPOL=2)
	integer VisId
c
	VisId = VID
	spect = mod(VisId,MAXWIN)
	VisId = VisId/MAXWIN
	p     = mod(VisId,MAXPOL)
	VisId = VisId/MAXPOL
	i2    = mod(VisId,MAXANT)
	VisId = VisId/MAXANT
	i1    = mod(VisId,MAXANT)
	chan  = VisId/MAXANT
c
	i1 = i1 + 1
	i2 = i2 + 1
	p = p + 1
	spect = spect + 1
	chan = chan + 1
c
	end
c************************************************************************
	subroutine AccumIni(Hash,maxhash)
c
 	implicit none
	integer maxHash,Hash(2,MaxHash)
c
c  Initialise the hash table.
c
c------------------------------------------------------------------------
	integer i
c
c  Externals.
c
	integer prime
c
	do i=1,maxHash
	  Hash(1,i) = 0
	enddo
c
	Hash(1,1) = prime(maxHash-2)
	end
c************************************************************************
	subroutine Accum(Hash,Data,VisId,nsoln,nvis,Vis,Wt,
     *						VID,Count)
c
	implicit none
	integer VisId,nsoln,nvis,VID(*),Count(*)
	real Wt(*)
	integer Hash(2,*)
	complex Data,Vis(*)
c
c  Inputs:
c    VisId	Identifier, giving channel, IF band, antennae and polarisation.
c------------------------------------------------------------------------
	integer nHash,iHash,indx,i
c
c  Find this channel in the hash table.
c
	nHash = Hash(1,1)
	iHash = VisId + 1
	indx = mod(iHash,nHash) + 2
	dowhile(Hash(1,indx).ne.0.and.Hash(1,indx).ne.iHash)
	  indx = indx + 1
	enddo
	if(indx.ge.nHash+2)then
	  indx = 2
	  dowhile(Hash(1,indx).ne.0.and.Hash(1,indx).ne.iHash)
	    indx = indx + 1
	  enddo
	  if(indx.ge.nHash+2)
     *		call bug('f','Hash table overflow, in Accum')
	endif
c
c  Is it a new slot?
c
	if(Hash(1,indx).eq.0)then
	  nvis = nvis + 1
	  Hash(1,indx) = iHash
	  Hash(2,indx) = nvis
	  i = nvis
	  Vis(i) = Data
	  Wt(i) = 1
	  VID(i) = VisId
	  Count(nsoln) = Count(nsoln) + 1
	else
	  i = Hash(2,indx)
	  Vis(i) = Vis(i) + Data
	  Wt(i) = Wt(i) + 1
	endif
c
	end
c************************************************************************
	logical function accept(present,nants,npol,refant,minant,maxant)
c
	implicit none
	integer nants,refant,minant,maxant,npol
	logical present(maxant,npol)
c
c  Determine whether we should accept this solution interval. In particular
c  check whether the reference antenna is present, and whether the minimum
c  number of antennae are present.
c------------------------------------------------------------------------
	integer n,i,p
	logical gotone
c
c  If some data from a particular polarisation is present, then the
c  reference antenna should also be present.
c
	accept = .false.
	do p=1,npol
	  do i=1,nants
	    if(present(i,p).and..not.present(refant,p))return
	  enddo
	enddo
c
c  Determine how many antenna are present.
c
	n = 0
	do i=1,nants
	  gotone = .false.
	  do p=1,npol
	    gotone = gotone.or.present(i,p)
	    present(i,p) = .false.
	  enddo
	  if(gotone)n = n + 1
	enddo
c
c  Is the minants requirement satisfied?
c
	accept = n.ge.minant
	end
c************************************************************************
	subroutine despect(updated,tno,nchan,edge,chan,spect,
     *			maxspect,nspect,sfreq,sdf,nschan,state)
c
	implicit none
	integer tno,nchan,chan(nchan),spect(nchan),edge(2)
	integer nspect,maxspect,nschan(maxspect),state(3,maxspect+2)
	double precision sfreq(maxspect),sdf(maxspect)
	logical updated
c
c  Determine the chan/spect parameters for a particular
c  set of correlations that have just been read.
c
c  Input:
c    tno
c    nchan
c    maxspect
c    updated
c    edge	Number of channels to reject at band edges.
c  Input/Output:
c    nspect
c    sfreq
c    sdf
c    nschan
c    state
c  Output:
c    chan
c    spect
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer CHANNEL,WIDE,MSPECT
	parameter(CHANNEL=1,WIDE=2,MSPECT=32)
	integer i,j,n,ispect,ltype,start,nschan0(MSPECT),nspect0,nwide
	integer chans,ibeg,iend,bdrop,edrop,nwidth,nstep
	double precision line(6),sfreq0(MSPECT),sdf0(MSPECT),f
	real wfreq(MSPECT),wwidth(MSPECT)
c
c  Determine what the current frequency setup is.
c
	if(updated)then
	  call uvinfo(tno,'line',line)
	  if(nint(line(2)).ne.nchan)
     *	    call bug('f','Number of channels disagree')
	  nstep  = nint(line(5))
	  nwidth = nint(line(4))
	  ltype = nint(line(1))
	  start = nint(line(3))
	  state(1,1) = 0
c
	  if(ltype.eq.CHANNEL)then
	    call uvrdvri(tno,'nspect',nspect0,0)
	    if(nspect0.le.0.or.nspect0.gt.MSPECT)
     *	      call bug('f','Bad value for nspect, in DESPECT')
	    call uvgetvrd(tno,'sfreq',sfreq0,nspect0)
	    call uvgetvrd(tno,'sdf',sdf0,nspect0)
	    call uvgetvri(tno,'nschan',nschan0,nspect0)
c
c  Go through the windows that we have. Match this with any
c  previous windows of the same sort.
c
	    ispect = 1
	    n = nchan
	    dowhile(n.gt.0)
	      dowhile(start.gt.nschan0(ispect))
	        start = start - nschan0(ispect)
	        ispect = ispect + 1
	      enddo
	      chans = min(n,
     *		(nschan0(ispect) - start + 1 + nstep - 1)/nstep)
	      bdrop = max(0,edge(1)-start+1 + nstep - 1)/nstep
	      edrop = max(0,
     *		nstep*chans+start-1+edge(2)-nschan0(ispect)+nstep-1 )
     *		/ nstep
	      if(bdrop+edrop.ge.chans)
     *		call bug('f','Illegal edge parameter')
	      f = sfreq0(ispect) +
     *		  sdf0(ispect) * (start - 1 + 0.5*(nwidth-1))
	      call SetState(state,f,nstep*sdf0(ispect),chans,
     *		maxspect,nspect,sfreq,sdf,nschan,bdrop,edrop)
	      n = n - chans
	      start = start + nstep * chans
	    enddo
c
c  Handle "wide" linetype.
c
	  else if(ltype.eq.WIDE)then
	    if(nstep.ne.1.or.nwidth.ne.1)call bug('f',
     *	      'Line width and step parameters must be 1 for line=wide')
	    call uvrdvri(tno,'nwide',nwide,0)
	    if(nwide.le.0.or.nwide.gt.MSPECT)
     *		call bug('f','Bad value for nwide in DESPECT')
	    call uvgetvrr(tno,'wfreq',wfreq,nwide)
	    call uvgetvrr(tno,'wwidth',wwidth,nwide)
	    do j=start,start+nchan-1
	      call SetState(state,dble(wfreq(j)),dble(wwidth(j)),1,
     *		    maxspect,nspect,sfreq,sdf,nschan,0,0)
	    enddo
	  else
	    call bug('f','Unsupported linetype')
	  endif
	  updated = .false.
	endif
c
c We know the frequency setup. Now fill the chan and spect arrays with this
c setup.
c
	n = 0
	do j=2,state(1,1)+1
	  ispect = state(1,j)
	  ibeg  = state(2,j)
	  iend  = state(3,j)
	  if(ispect.eq.0)then
	    do i=ibeg,iend
	      n = n + 1
	      chan(n) = 0
	    enddo
	  else
	    do i=ibeg,iend
	      n = n + 1
	      chan(n) = i
	      spect(n) = ispect
	    enddo
	  endif
	enddo
c
	end
c************************************************************************
	subroutine SetState(state,f,df,nchan,
     *		maxspect,nspect,sfreq,sdf,nschan,bdrop,edrop)
c
	implicit none
	integer maxspect,nspect,nchan,bdrop,edrop
	integer state(3,maxspect+2),nschan(maxspect)
	double precision f,df,sfreq(maxspect),sdf(maxspect)
c
c  Find the current frequency setup in the list of previous setups.
c
c  Input:
c    f		Frequency of the first channel (ignoring channels to drop).
c    df		Frequency increment between channels.
c    nchan	Total number of channels (ignoring channels to drop).
c    bdrop,edrop Number of channels to drop at beginning and end of window.
c    maxspect
c  Input/Output:
c    nspect	Number of spectral windows stored in sfreq,sdf,nschan
c    sfreq
c    sdf
c    nschan
c    state
c------------------------------------------------------------------------
	logical more
	integer ispect,i
	double precision f0
c
c  See if we have a match.
c
	f0 = f + bdrop * df
	more = nspect.gt.0
	ispect = 1
	dowhile(more)
	  if(abs(f0-sfreq(ispect)).lt.0.5*abs(sdf(ispect)).and.
     *	     abs(df-sdf(ispect)).lt.0.01*abs(sdf(ispect)))then
	    more = .false.
	  else
	    ispect = ispect + 1
	    more = ispect.le.nspect
	  endif
	enddo
c
c  If there was not a match, fill in a new slot for it. Otherwise
c  make sure the total number of channels is OK.
c
	if(ispect.gt.nspect)then
	  nspect = nspect + 1
	  if(nspect.gt.maxspect)
     *	    call bug('f','Too many spectral windows for me to handle')
	  sdf(nspect) = df
	  sfreq(nspect) = f0
	  nschan(nspect) = nchan - bdrop - edrop
	else
	  nschan(ispect) = max(nschan(ispect),nchan - bdrop - edrop)
	endif
c
c  Return the new frequency setup description.
c
	i = state(1,1) + 1
	if(bdrop.gt.0)then
	  if(i.gt.1.and.state(1,i).eq.0)then
	    state(3,i) = state(3,i) + bdrop
	  else
	    i = i + 1
	    if(i.gt.maxspect+2)
     *	      call bug('f','Buffer overflow, in despect-1')
	    state(1,i) = 0
	    state(2,i) = 1
	    state(3,i) = bdrop
	  endif
	endif
c
	i = i + 1
	if(i.gt.maxspect+2)
     *	  call bug('f','Buffer overflow, in despect-2')
	state(1,i) = ispect
	state(2,i) = 1
	state(3,i) = nchan - bdrop - edrop
c
	if(edrop.gt.0)then
	  i = i + 1
	  if(i.gt.maxspect+2)
     *	    call bug('f','Buffer overflow, in despect-3')
	  state(1,i) = 0
	  state(2,i) = 1
	  state(3,i) = edrop
	endif
c
	state(1,1) = i - 1
c
	end
c************************************************************************
	subroutine BPIni(npol,nants,nchan,nsoln,nspect,nschan,WGains,
     *	  freq,Pass,Gains,Tau,dodelay,dopass)
c
	implicit none
	integer npol,nants,nspect,nsoln,nchan,nschan(nspect)
	real Tau(nants,nsoln)
	double precision freq(nspect)
	complex WGains(nants,nspect,npol,nsoln),Gains(nants,npol,nsoln)
	complex Pass(nants,nchan,npol)
	logical dodelay,dopass
c
c  Given the gains for each antenna for each band (WGains), estimate the
c  passband gain (Pass), atmospheric delay (Tau) and antenna gain (Gains).
c  These are estimates that are used in a full-blown solver later on.
c
c  Input:
c    WGains	The antenna gains for each antenna for each band.
c    freq	Centre frequency of each band.
c    nants	Number of antennae.
c    nspect	Number of frequency bands.
c    nsoln	Number of solution intervals.
c    npol	Number of polarisations.
c    dodelay	Estimate the delay parameters?
c  Output:
c    Pass	Estimate of the passband gains.
c    Gains	Estimate of the antenna gains.
c    Tau	Estimate of the atmospheric delay.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXPOL
	parameter(MAXPOL=2)
c
	integer i,j,k,p
	complex g,WPass(MAXANT*MAXWIN*MAXPOL)
	logical more
	real epsi
c
c  Set the antenna gains as the gain of the lowest band.
c
	do k=1,nsoln
	  do p=1,npol
	    do j=1,nants
	      Gains(j,p,k) = 0
	      Tau(j,k) = 0
	      i = 0
	      more = .true.
	      dowhile(more)
		i = i + 1
		g = WGains(j,i,p,k)
		if(abs(real(g))+abs(aimag(g)).gt.0)then
		  Gains(j,p,k) = g
		  more = .false.
		else
		  more = i.lt.nspect
		endif
	      enddo
	    enddo
	  enddo
	enddo
c
c  If we want the passband, iteratively solve for the band gain, the antenna
c  gain and atmospheric delay.
c
	if(dopass)then
	  epsi = 1
	  dowhile(epsi.gt.1e-2)
	    call BandEst(nants,npol,nspect,nsoln,WGains,freq,
     *	      WPass,Gains,Tau)
	    call GainEst(nants,npol,nspect,nsoln,WGains,freq,
     *	      WPass,Gains,Tau,dodelay,epsi)
	  enddo
c
c  Otherwise, initialise the band gain to 1, and solve for the
c  antenna gain and delay.
c
	else
	  do i=1,nspect*nants*npol
	    WPass(i) = 1
	  enddo
	  call GainEst(nants,npol,nspect,nsoln,WGains,freq,
     *	    WPass,Gains,Tau,dodelay,epsi)
	endif
c
c  OK, we now have estimates of everything that we want. Fill in the
c  "Pass" array.
c
	call PassFill(nants,npol,nchan,nspect,nschan,WPass,Pass)
c
	end
c************************************************************************
	subroutine PassFill(nants,npol,nchan,nspect,nschan,WPass,Pass)
c
	implicit none
	integer nants,npol,nchan,nspect,nschan(nspect)
	complex WPass(nants,nspect,npol),Pass(nants,nchan,npol)
c
c  Fill in the passband estimate.
c
c  Input:
c    nants,nchan,nspect,nschan,npol
c    WPass
c  Output:
c    Pass
c------------------------------------------------------------------------
	integer i,j,k,p,chan
	complex temp
c
	do p=1,npol
	  chan = 0
	  do k=1,nspect
	    do i=1,nants
	      temp = WPass(i,k,p)
	      do j=chan+1,chan+nschan(k)
	        Pass(i,j,p) = temp
	      enddo
	    enddo
	    chan = chan + nschan(k)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GainEst(nants,npol,nspect,nsoln,WGains,freq,
     *	  WPass,Gains,Tau,dodelay,epsi)
c
	implicit none
	integer nants,nspect,nsoln,npol
	real Tau(nants,nsoln),epsi
	double precision freq(nspect)
	complex WGains(nants,nspect,npol,nsoln),Gains(nants,npol,nsoln)
	complex WPass(nants,nspect,npol)
	logical dodelay
c
c  Given the band gain (wpass) and the overall gain (wgain), estimate the
c  antenna gain and the delay term.
c
c  Input:
c    WGains	The antenna gains for each antenna for each band.
c    freq	Centre frequency of each band.
c    nants	Number of antennae.
c    nspect	Number of frequency bands.
c    nsoln	Number of solution intervals.
c    WPass	Estimate of the band gains.
c    dodelay	Solve for the delay terms.
c  Input/Output:
c    Gains	Estimate of the antenna gains.
c    Tau	Estimate of the atmospheric delay.
c  Output:
c    epsi	Fractional change in the gains.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,j,k,n,p
	real theta,SumMM,delta,SumF,SumT,SumTF,SumFF
	real SumDT2,SumT2,SumDG2,SumG2,f
	complex V,M,SumVM,cdelta,temp
c
c  Initialise.
c
	SumDT2 = 0
	SumT2  = 0
	SumDG2 = 0
	SumG2  = 0
c
c  Loop over all solutions for all antennae.
c
	do k=1,nsoln
	  do i=1,nants
c
c  Solve for the delay term. Use a simple linear approach. 
c
	    if(dodelay)then
	      SumF = 0
	      SumT = 0
	      SumTF = 0
	      SumFF = 0
	      n = 0
	      do p=1,npol
	        do j=1,nspect
		  V = WGains(i,j,p,k)
		  if(abs(real(V))+abs(aimag(V)).gt.0)then
		    f = freq(j)
		    temp = V / (WPass(i,j,p) * Gains(i,p,k))
		    theta = atan2(aimag(temp),real(temp))
     *    			  - 2*pi*Tau(i,k)*f
		    theta = mod(theta,2*pi)
		    if(theta.gt.pi)  theta = theta - 2*pi
		    if(theta.lt.-pi) theta = theta + 2*pi
		    SumF = SumF + f
		    SumT = SumT + theta
		    SumTF = SumTF + theta*f
		    SumFF = SumFF + f*f
		    n = n + 1
		  endif
		enddo
	      enddo
	      if(n.gt.1.and.SumFF.gt.0)then
		delta = (n*SumTF - SumT*SumF) /
     *			(2*pi*(n*SumFF - SumF*SumF))
		Tau(i,k) = Tau(i,k) + delta
		SumDT2 = SumDT2 + delta*delta
		SumT2 = SumT2 + Tau(i,k)*Tau(i,k)
	      else
		Tau(i,k) = 0
	      endif
	    endif
c
c  Now solve for the antenna gain.
c
	    do p=1,npol
	      SumVM = 0
	      SumMM = 0
	      do j=1,nspect
		V = WGains(i,j,p,k)
		if(abs(real(V))+abs(aimag(V)).gt.0)then
		  theta = 2*pi * Tau(i,k) * freq(j)
		  M = WPass(i,j,p) * cmplx(cos(theta),sin(theta))
		  SumVM = SumVM + V*conjg(M)
		  SumMM = SumMM + real(M)**2 + aimag(M)**2
		endif
	      enddo
	      if(SumMM.gt.0)then
		cdelta = SumVM / SumMM - Gains(i,p,k)
		Gains(i,p,k) = Gains(i,p,k) + cdelta
		SumDG2 = SumDG2 + real(cdelta)**2 + aimag(cdelta)**2
		SumG2 = SumG2 +
     *		  real(Gains(i,p,k))**2 + aimag(Gains(i,p,k))**2
	      else
		Gains(i,p,k) = 0
	      endif
	    enddo
c
	  enddo
	enddo
c
	epsi = 0
	if(SumG2.gt.0) epsi = max(epsi, SumDG2 / SumG2)
	if(SumT2.gt.0) epsi = max(epsi, SumDT2 / SumT2)
	epsi = sqrt(epsi)
	end
c************************************************************************
	subroutine BandEst(nants,npol,nspect,nsoln,WGains,freq,
     *	  WPass,Gains,Tau)
c
	implicit none
	integer nants,nspect,nsoln,npol
	real Tau(nants,nsoln)
	double precision freq(nspect)
	complex WGains(nants,nspect,npol,nsoln),Gains(nants,npol,nsoln)
	complex WPass(nants,nspect,npol)
c
c  Estimate the band gain, given the gains for each antenna for each band,
c  the antenna gains, and the atmospheric gains.
c
c  Input:
c    WGains	The antenna gains for each antenna for each band.
c    freq	Centre frequency of each band.
c    nants	Number of antennae.
c    nspect	Number of frequency bands.
c    nsoln	Number of solution intervals.
c    Gains	Estimate of the antenna gains.
c    Tau	Estimate of the atmospheric delay.
c  Output:
c    WPass	Estimate of the band gains.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	integer MAXPOL
	parameter(MAXPOL=2)
	integer i,j,k,p
	real SumMM(MAXANT,MAXWIN,MAXPOL),theta
	complex SumVM(MAXANT,MAXWIN,MAXPOL),g,V
c
c  Initialise the accumulators.
c
	do p=1,npol
	  do j=1,nspect
	    do i=1,nants
	      SumMM(i,j,p) = 0
	      SumVM(i,j,p) = 0
	    enddo
	  enddo
	enddo
c
c  Accumulate the rubbish.
c
	do k=1,nsoln
	  do p=1,npol
	    do j=1,nspect
	      do i=1,nants
	        g = WGains(i,j,p,k)
	        if(abs(real(g))+abs(aimag(g)).gt.0)then
		  theta = 2*pi * freq(j) * Tau(i,k)
		  V = Gains(i,p,k) * cmplx(cos(theta),sin(theta))
		  SumVM(i,j,p) = SumVM(i,j,p) + g*conjg(V)
		  SumMM(i,j,p) = SumMM(i,j,p) + real(V)**2 + aimag(V)**2
	        endif
	      enddo
	    enddo
	  enddo
	enddo
c
c  Now we need to fill in the resulting solution.
c
	do p=1,npol
	  do j=1,nspect
	    do i=1,nants
	      if(SumMM(i,j,p).gt.0)then
	        WPass(i,j,p) = SumVM(i,j,p) / SumMM(i,j,p)
	      else
	        WPass(i,j,p) = 0
	      endif
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine SolveBP(refant,minant,nants,nspect,nchan,nsoln,Pass,
     *	  Source,freq,Dat,Wt,VID,ischan,Count,n,npol,Gains,Tau,tol)
c
	implicit none
	integer nants,nchan,n,nsoln,refant,minant,nspect,npol
	real Tau(nants,nsoln),Source(nchan),tol,Wt(n)
	double precision freq(nchan)
	complex Pass(nants,nchan,npol),Gains(nants,npol,nsoln)
	complex Dat(n)
	integer VID(n),Count(nsoln),ischan(nspect)
c
c  Given the source, antenna gains and atmospheric delays, solve for
c  the pass band.
c
c  Input:
c    nants	Number of antennae.
c    nspect
c    nchan	Total number of channels.
c    n		Number of data points.
c    refant	The reference antenna.
c    source	Source flux as a function of frequency.
c    freq	Frequency of each channel.
c    Pass	Passband gain.
c    Dat	Visibility data.
c    Wt		Weight of each visibility.
c    VID
c    SolNo	Antenna solution number.
c    Gains	Antenna gains.
c    Tau	Delay term.
c    tol	Convergence tolerance.
c  Input/Output:
c    Pass	Pass band.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer pSumVM,pSumMM
	real epsi
	integer nbl,off,i,p
c
	real ref(MAXBUF)
	complex cref(MAXBUF/2)
	equivalence(ref,cref)
	common ref
c
	nbl = nants*(nants-1)/2
c
c  Allocate memory.
c
	call MemAlloc(pSumVM,nbl*nchan*npol,'c')
	call MemAlloc(pSumMM,nbl*nchan*npol,'r')
c
c  Accumulate statistics.
c
	call AccPB(nants,nspect,nbl,nchan,npol,nsoln,Source,freq,Dat,Wt,
     *		VID,ischan,Count,n,Gains,Tau,cref(pSumVM),ref(pSumMM))
c
c  Having accumulated the crap, go and get a solution.
c
	off = 0
	do p=1,npol
	  do i=1,nchan
	    call Solve(nants,nbl,cref(pSumVM+off),ref(pSumMM+off),
     *	      Pass(1,i,p),refant,minant,tol*tol,epsi,.false.)
	    off = off + nbl
	  enddo
	enddo
c
c  Free up the allocated memory.
c
	call MemFree(pSumVM,nbl*nchan*npol,'c')
	call MemFree(pSumMM,nbl*nchan*npol,'r')
	end
c************************************************************************
	subroutine Solve(nants,nbl,SumVM,SumMM,Gains,
     *				refant,minant,tol,epsi,init)
c
	implicit none
	integer nants,nbl,refant,minant
	complex SumVM(nbl),Gains(nants)
	real SumMM(nbl),tol,epsi
	logical init
c
c  Given the accumulated crap, solve for the passband value for this
c  particular frequency channel.
c
c  Input:
c    nants	Number of antennae.
c    nbl	Total number of baselines = nants*(nants-1)/2
c    SumVM,SumMM Accumulated rubbish.
c    init	No initial guess of the gains has been given. This routine
c		works it out from scratch.
c    minant	Minimum number of antenna before a solution will be attempted.
c    refant	The reference antenna.
c    tol	Solution convergence tolerance.
c  Input/Output:
c    Gains	On entry, an estimate of the antenna gains. On exit,
c		an updated estimate! If "init" is true, then the gains
c		are not input, and an initial estimate should be made.
c  Output:
c    epsi	Fractional change
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,i1,i2,k,nantsd,nbld,Idx(MAXANT)
	integer b1(MAXBASE),b2(MAXBASE)
	complex ref,G(MAXANT),SVM(MAXBASE)
	real SMM(MAXBASE)
c
	do i=1,nants
	  Idx(i) = 0
	enddo
c
	if(init)then
	  do i=1,nants
	    Gains(i) = 1
	  enddo
	endif
c
	nantsd = 0
	nbld = 0
	k = 0
	do i2=2,nants
	  do i1=1,i2-1
	    k = k + 1
	    if(SumMM(k).gt.0)then
	      nbld = nbld + 1
	      if(Idx(i1).eq.0)then
		nantsd = nantsd + 1
		Idx(i1) = nantsd
		G(nantsd) = Gains(i1)
	      endif
	      if(Idx(i2).eq.0)then
		nantsd = nantsd + 1
		Idx(i2) = nantsd
		G(nantsd) = Gains(i2)
	      endif
	      b1(nbld) = Idx(i1)
	      b2(nbld) = Idx(i2)
	      SVM(nbld) = SumVM(k)
	      SMM(nbld) = SumMM(k)
	    endif
	  enddo
	enddo
c
c  Get the solution and unpack it.
c
	if(nantsd.ge.minant)then
	  if(init)then
	    call Phasol(nantsd,nbld,SVM,b1,b2,G,tol)
	    call Scale(nantsd,nbld,SVM,SMM,b1,b2,G)
	  endif
	  call Amphasol(nantsd,nbld,SVM,SMM,b1,b2,G,tol,epsi)
c
	  if(Idx(refant).gt.0)then
	    ref = conjg(G(Idx(refant))) / abs(G(Idx(refant)))
	  else
	    ref = 1
	  endif
c	
	  do i=1,nants
	    if(Idx(i).gt.0)then
	      Gains(i) = ref * G(Idx(i))
	    else
	      Gains(i) = 0
	    endif
	  enddo
c
c  Otherwise things have gone wrong. Set everything to zero.
c
	else
	  do i=1,nants
	    Gains(i) = 0
	  enddo
	endif
	end
c************************************************************************
	subroutine phasol(nants,nbl,SumVM,b1,b2,Gain,tol)
c
	implicit none
	integer nbl,nants
	integer b1(nbl),b2(nbl)
	complex SumVM(nbl),Gain(nants)
	real tol
c
c  Solve for the phase corrections which minimise the error. This uses
c  a nonlinear Jacobi iteration, as suggested by Fred Schwab in "Adaptive
c  calibration of radio interferomter data", SPIE Vol. 231, 1980
c  International Optical Computing Conference (pp 18-24). The damping
c  heuristics are copied from AIPS ASCAL.
c
c  Input:
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of Model*conjg(Vis)
c  Input/Output:
c    Gain	The antenna gain solution.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXITER
	parameter(MAXITER=100)
c
	real Factor,Change
	complex Temp
	integer Niter,i
	complex Sum(MAXANT)
	logical convrg
c
c  Initialise.
c
	do i=1,nants
	  Sum(i) = (0.,0.)
	enddo
c
	Factor = 0.8
	if(nants.le.6)Factor = 0.5
c
c  Iterate.
c
	Convrg = .false.
	Niter = 0
	do while(.not.Convrg.and.Niter.lt.MaxIter)
	  Niter = Niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
	  do i=1,nbl
	    Sum(b1(i)) = Sum(b1(i)) + Gain(b2(i)) *       SumVM(i)
	    Sum(b2(i)) = Sum(b2(i)) + Gain(b1(i)) * conjg(SumVM(i))
	  enddo
c
c  Update the gains.
c
	  Change = 0
c#maxloop 32
	  do i=1,nants
	    Temp = ( Sum(i)/abs(Sum(i)) )
	    Temp = Gain(i) + Factor * ( Temp - Gain(i) )
	    Temp = Temp/abs(Temp)
	    Change = Change + real(Gain(i)-Temp)**2
     *			    + aimag(Gain(i)-Temp)**2
	    Gain(i) = Temp
	    Sum(i) = (0.,0.)
	  enddo
	  Convrg = Change/nants .lt. tol
	enddo
	end
c************************************************************************
	subroutine Scale(nants,nbl,SumVM,SumMM,b1,b2,Gain)
c
	implicit none
	integer nants,nbl
	integer b1(nbl),b2(nbl)
	complex SumVM(nbl),Gain(nants)
	real SumMM(nbl)
c
c  Get an initial approximation of the gain solution. This finds a single
c  real gain which minimises the error. This helps stablise the algorithm
c  when the gain solution is very different from 1 (e.g. when we are
c  calculating a priori calibration factors).
c
c  Input:
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of Vis*conjg(Model), for each baseline.
c    SumMM	Sum of Model*conjg(Model), for each baseline.
c  Input/Output:
c    Gain	The antenna gain solution.
c
c------------------------------------------------------------------------
	integer i
	real Factor,SumRMM,SumRVM
	complex t
c
	SumRVM = 0
	SumRMM = 0
	do i=1,nbl
	  t = conjg(Gain(b1(i))) * Gain(b2(i))
	  SumRVM = SumRVM + t*SumVM(i)
	  SumRMM = SumRMM + (real(t)**2 + aimag(t)**2) * SumMM(i)
	enddo
	Factor = sqrt(abs(SumRVM / SumRMM))
c
	do i=1,nants
	  Gain(i) = Factor * Gain(i)
	enddo
	end
c************************************************************************
	subroutine Amphasol(nants,nbl,SumVM,SumMM,b1,b2,G,tol,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	complex SumVM(nbl),G(nants)
	real SumMM(nbl),epsi,tol
c
c  Inputs:
c    nbl,nants	Number of baselines and number of antennae.
c    SumMM,SumVM Accumulated rubbish used in the solution process.
c    b1,b2	Antenna numbers of the given baseline.
c    tol	Tolerance in determining the solutions.
c  Input/Output:
c    G		Current estimate of the X gains.
c  Output:
c    epsi	Fractional change in gains.
c------------------------------------------------------------------------
	integer MAXITER
	parameter(MAXITER=100)
	include 'maxdim.h'
c
	integer i,niter
	logical convrg
	real Sum2(MAXANT),factor,SumWt,Change,t
	complex Sum(MAXANT),Temp
c
	real Factors(11)
	data Factors/0.5,0.75,8*0.9,0.5/
c
c  Initialise.
c
	epsi = 0
	do i=1,nants
	  Sum(i) = (0.,0.)
	  Sum2(i) = 0.
	enddo
c
	convrg = .false.
	niter = 0
	dowhile(.not.convrg.and.niter.lt.MAXITER)
	  niter = niter + 1
c
c  Get the same damping factor as AIPS.
c
	  if(nants.le.6)then
	    factor = 0.5
	  else
	    factor = factors(min(11,niter))
	  endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
	  do i=1,nbl
	    Sum(b1(i))  = Sum(b1(i)) + G(b2(i)) * SumVM(i)
	    Sum(b2(i))  = Sum(b2(i)) + G(b1(i)) * conjg(SumVM(i))
c
	    Sum2(b1(i)) = Sum2(b1(i)) +
     *	      (real(G(b2(i)))**2 + aimag(G(b2(i)))**2)*SumMM(i)
	    Sum2(b2(i)) = Sum2(b2(i)) +
     *	      (real(G(b1(i)))**2 + aimag(G(b1(i)))**2)*SumMM(i)
	  enddo
c
c  Update the gains.
c
	  Change = 0
	  SumWt = 0
c
c  Evaluate gain, and zero counters.
c
c#maxloop 32
	  do i=1,nants
	    t = 1./Sum2(i)
	    Temp = t * Sum(i) - G(i)
	    G(i) = G(i) + Factor * Temp
	    Change = Change + real(Temp)**2 + aimag(Temp)**2
	    SumWt = SumWt + real(G(i))**2  + aimag(G(i))**2
	    Sum(i) = 0
	    Sum2(i) = 0
	  enddo
	  t = Change/SumWt
	  epsi = max(epsi,t)
	  convrg = t.lt.tol
	enddo
c
	end
c************************************************************************
	subroutine AccPB(nants,nspect,nbl,nchan,npol,nsoln,Source,freq,
     *	  Vis,Wt,VID,ischan,Count,nvis,Gains,Tau,SumVM,SumMM)
c
	implicit none
	integer nants,nbl,nchan,nsoln,nvis,nspect,npol
	real Source(nchan),Tau(nants,nsoln),Wt(nvis)
	double precision freq(nchan)
	complex Vis(nvis),Gains(nants,npol,nsoln)
	integer VID(nvis),Count(nsoln),ischan(nspect)
	complex SumVM(nbl,nchan,npol)
	real SumMM(nbl,nchan,npol)
c
c  Accumulate rubbish.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,j,bl,off,spect,chan,i1,i2,p
	real theta,W
	complex V,Model
c
	do p=1,npol
	  do j=1,nchan
	    do i=1,nbl
	      SumVM(i,j,p) = 0
	      SumMM(i,j,p) = 0
	    enddo
	  enddo
	enddo
c
c  Go through things, accumulating all the rubbish we could possibly
c  want.
c
	off = 0
	do j=1,nsoln
	  do i=1,Count(j)
	    off = off + 1
	    call unpack(i1,i2,p,spect,chan,VID(off))
	    chan =  chan + ischan(spect)
	    V = Vis(off)
	    W = Wt(off)
c
	    theta = 2*pi* freq(chan) * ( Tau(i1,j) - Tau(i2,j) )
	    Model = Source(chan)*Gains(i1,p,j)*conjg(Gains(i2,p,j))
     *		  * cmplx(cos(theta),sin(theta))
	    bl = (i2-1)*(i2-2)/2 + i1
	    SumVM(bl,chan,p) = SumVM(bl,chan,p) + V*conjg(Model)
	    SumMM(bl,chan,p) = SumMM(bl,chan,p) + W*Model*conjg(Model)
	  enddo
	enddo
	end
c************************************************************************
	subroutine SolveGT(refant,minant,nants,nspect,nchan,nsoln,Pass,
     *	  Source,Freq,Vis,Wt,VID,ischan,Count,nvis,npol,Gains,Tau,
     *	  dodelay,tol)
c
	implicit none
	integer refant,minant,nants,nchan,nsoln,nvis,nspect,npol
	integer VID(nvis),Count(nsoln),ischan(nspect)
	complex Pass(nants,nchan,npol),Vis(nvis),Gains(nants,npol,nsoln)
	real Source(nchan),Tau(nants,nsoln),tol,Wt(nvis)
	double precision Freq(nchan)
	logical dodelay
c
c  Driver for the routine to solve for the antenna gains and delays.
c------------------------------------------------------------------------
	integer i,off
c
	off = 1
	do i=1,nsoln
	  if(dodelay)then
	    call SolveGT1(refant,nants,nspect,nchan,npol,Pass,Source,
     *		freq,Vis(off),Wt(off),VID(off),ischan,Count(i),
     *		Gains(1,1,i),Tau(1,i),tol)
	  else
	    call SolveGT2(refant,minant,nants,nspect,nchan,npol,Pass,
     *		Source,Vis(off),Wt(off),VID(off),ischan,Count(i),
     *		Gains(1,1,i),tol)
	  endif
	  off = off + Count(i)
	enddo
	end
c************************************************************************
	subroutine SolveGT1(refant,nants,nspect,nchan,npol,
     *	  Pass,Source,freq,Dat,Wt,VID,ischan,n,Gains,Tau,tol)
c
	implicit none
	integer nants,nchan,n,refant,nspect,npol
	real Tau(nants),Source(nchan),tol
	double precision freq(nchan)
	complex Pass(nants,nchan,npol),Gains(nants,npol)
	complex Dat(n)
	real Wt(n)
	integer VID(n),ischan(nspect)
c
c  Solve for the antenna gains and the atmospheric delay.
c
c  Input:
c    nants	Number of antennae.
c    nspect
c    nchan	Total number of channels.
c    n		Number of data points.
c    refant	The reference antenna.
c    source	Source flux as a function of frequency.
c    freq	Frequency of each channel.
c    Pass	Passband gain.
c    Dat	Visibility data.
c    Wt		Weight for each data point.
c    VID	Visibility antennae, polarisation, channel, band.
c    tol	Convergence tolerance.
c  Input/Output:
c    Gains
c    Tau
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mfcal.h'
	integer MAXITER,MAXVAR
	parameter(MAXITER=200,MAXVAR=(1+2*MAXPOL)*MAXANT)
	include 'mirconst.h'
c
	integer i,Idx(MAXANT,MAXPOL),TIdx(MAXANT),p
	integer ifail,spect,chan,i1,i2,nvar,neqn
c
c  Scratch arrays for the least squares solver.
c
	real x(MAXVAR),dx(MAXVAR),W
	integer dfdx,aa,f,fp
c
c  Dynamic memory commons.
c
	real ref(MAXBUF)
	common ref
c
c  Externals.
c
	character itoaf*4
	external FUNC,DERIVE
c
c  Check we have enough space.
c
	if(n.gt.MAXDATA)call bug('f','Too many data points')
c
c  Initialise the indices to keep track of things.
c
	do p=1,npol
	  do i=1,nants
	    Idx(i,p) = 0
	    TIdx(i) = 0
	  enddo
	enddo
c
c  Copy the data across into common, determining the variable index as we go.
c
	nvar = 0
	do i=1,n
	  call unpack(i1,i2,p,spect,chan,VID(i))
	  chan = chan + ischan(spect)
	  if(Idx(i1,p).eq.0)then
	    Idx(i1,p) = nvar + 1
	    nvar = nvar + 2
	  endif
	  if(TIdx(i1).eq.0)then
	    nvar = nvar + 1
	    TIdx(i1) = nvar
	  endif
	  if(Idx(i2,p).eq.0)then
	    Idx(i2,p) = nvar + 1
	    nvar = nvar + 2
	  endif
	  if(TIdx(i2).eq.0)then
	    nvar = nvar + 1
	    TIdx(i2) = nvar
	  endif
	  b1(i) = Idx(i1,p)
	  b2(i) = Idx(i2,p)
	  t1(i) = TIdx(i1)
	  t2(i) = TIdx(i2)
	  angfreq(i) = 2*pi*freq(chan)
	  W = sqrt(Wt(i))
	  Model(i) = W * Source(chan) *
     *			Pass(i1,chan,p) * conjg(Pass(i2,chan,p))
	  Vis(i) = Dat(i) / W
	enddo
c
c  Make a list of the variables that are to be constrained to be zero. This
c  is to make life simpler in the solver. The are the delay of the
c  reference antenna, and the imaginary parts of the gains of the reference
c  antenna.
c
	nzero = 1
	zerovar(nzero) = TIdx(refant)
	do p=1,npol
	  if(Idx(refant,p).gt.0)then
	    nzero = nzero + 1
	    zerovar(nzero) = Idx(refant,p) + 1
	  endif
	enddo
	neqn = 2*n+nzero
c
c  Copy across the current estimate of the variables.
c
	do i=1,nants
	  do p=1,npol
	    if(Idx(i,p).gt.0)then
	      x(Idx(i,p))   = real(Gains(i,p))
	      x(Idx(i,p)+1) = aimag(Gains(i,p))
	    endif
	  enddo
	  if(TIdx(i).gt.0) x(Tidx(i)) = Tau(i)
	enddo
c
c  Allocate memory for scratch arrays.
c
	call memalloc(dfdx,neqn*nvar,'r')
	call memalloc(aa,nvar*nvar,'r')
	call memalloc(f,neqn,'r')
	call memalloc(fp,neqn,'r')
c
c  Call the solver at last.
c
	call nllsqu(nvar,neqn,x,x,MAXITER,0.,tol,.true.,
     *	  ifail,FUNC,DERIVE,ref(f),ref(fp),dx,ref(dfdx),ref(aa))
	if(ifail.ne.0)call bug('w',
     *	  'Solver failed to converge: ifail='//itoaf(ifail))
c
c  Free up the memory now.
c
	call memfree(fp,neqn,'r')
	call memfree(f,neqn,'r')
	call memfree(aa,nvar*nvar,'r')
	call memfree(dfdx,neqn*nvar,'r')
c
c  Now unpack the solution.
c
	do i=1,nants
	  do p=1,npol
	    if(Idx(i,p).gt.0)then
	      Gains(i,p) = cmplx(x(Idx(i,p)),x(Idx(i,p)+1))
	    else
	      Gains(i,p) = 0
	    endif
	  enddo
	  if(TIdx(i).gt.0)then
	    Tau(i) = x(Tidx(i))
	  else
	    Tau(i) = 0
	  endif
	enddo
c
	end
c************************************************************************
	subroutine FUNC(x,f,n,m)
c
	implicit none
	integer m,n
	real x(n),f(m)
c------------------------------------------------------------------------
	include 'mfcal.h'
	integer i,i1,i2,j1,j2,off
	complex temp
	real theta
c
c  Fudge equations to ensure that the phase and delay of the reference antenna 
c  is 0.
c
	do i=1,nzero
	  f(i) = m*x(zerovar(i))
	enddo
c
	off = 0
	do i=nzero+1,m,2
	  off = off + 1
	  i1 = b1(off)
	  i2 = b2(off)
	  j1 = t1(off)
	  j2 = t2(off)
	  theta = angfreq(off) * (x(j1)-x(j2))
	  temp = Vis(off) - cmplx(x(i1),x(i1+1))*cmplx(x(i2),-x(i2+1))
     *			   * cmplx(cos(theta),sin(theta)) * Model(off)
	  f(i)   = real(temp)
	  f(i+1) = aimag(temp)
	enddo
	end
c************************************************************************
	subroutine DERIVE(x,dfdx,n,m)
c
	implicit none
	integer m,n
	real x(n),dfdx(n,m)
c------------------------------------------------------------------------
	include 'mfcal.h'
	integer i,j,i1,i2,j1,j2,off
	complex g1,g2,temp,w
	real theta
c
	do j=1,m
	  do i=1,n
	    dfdx(i,j) = 0
	  enddo
	enddo
c
c  Fudge equations to make sure that the phase and delay of the referenece
c  antenna is zero.
c
	do i=1,nzero
	  dfdx(zerovar(i),i) = m
	enddo
c
c  The real equations.
c
	off = 0
	do i=nzero+1,m,2
	  off = off + 1
	  i1 = b1(off)
	  i2 = b2(off)
	  j1 = t1(off)
	  j2 = t2(off)
	  theta = angfreq(off) * (x(j1)-x(j2))
	  w = cmplx(cos(theta),sin(theta))
	  g1 = cmplx(x(i1),x(i1+1))
	  g2 = cmplx(x(i2),-x(i2+1))
c
	  temp = g2*w*Model(off)
	  dfdx(i1,i)     = -real(temp)
	  dfdx(i1+1,i)   =  aimag(temp)
	  dfdx(i1,i+1)   = -aimag(temp)
	  dfdx(i1+1,i+1) = -real(temp)
c
	  temp = g1*w*Model(off)
	  dfdx(i2,i)     = -real(temp)
	  dfdx(i2+1,i)   = -aimag(temp)
	  dfdx(i2,i+1)   = -aimag(temp)
	  dfdx(i2+1,i+1) =  real(temp)
c
	  temp = angfreq(off) * g1*g2*w * Model(off)
	  dfdx(j1,i)   =  aimag(temp)
	  dfdx(j1,i+1) = -real(temp)
	  dfdx(j2,i)   = -aimag(temp)
	  dfdx(j2,i+1) =  real(temp)
	enddo
	end
c************************************************************************
	subroutine SolveGT2(refant,minant,nants,nspect,nchan,npol,
     *	  Pass,Source,Dat,Wt,VID,ischan,n,Gains,tol)
c
	implicit none
	integer nants,nchan,n,refant,minant,nspect,npol
	real Source(nchan),tol
	complex Pass(nants,nchan,npol),Gains(nants,npol)
	complex Dat(n)
	real Wt(n)
	integer VID(n),ischan(nspect)
c
c  Solve for the antenna gains, but not atmospheric delay.
c
c  Input:
c    nants	Number of antennae.
c    nspect
c    nchan	Total number of channels.
c    n		Number of data points.
c    refant	The reference antenna.
c    source	Source flux as a function of frequency.
c    Pass	Passband gain.
c    Dat	Visibility data.
c    Wt		Weight for each data point.
c    VID	Visibility antennae, polarisation, channel, band.
c    tol	Convergence tolerance.
c  Input/Output:
c    Gains
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXPOL
	parameter(MAXPOL=2)
c
	integer nbl,bl,p,i,i1,i2,spect,chan
	real SumMM(MAXBASE,MAXPOL),epsi
	complex SumVM(MAXBASE,MAXPOL),Model
c
c  Initialise the accumulators.
c
	nbl = nants*(nants-1)/2
	do p=1,npol
	  do i=1,nbl
	    SumMM(i,p) = 0
	    SumVM(i,p) = 0
	  enddo
	enddo
c
c  Now accumulate all the rubbish.
c
	do i=1,n
	  call unpack(i1,i2,p,spect,chan,VID(i))
	  chan = chan + ischan(spect)
	  bl = (i2-1)*(i2-2)/2 + i1
	  Model = Source(chan) *
     *		  Pass(i1,chan,p) * conjg(Pass(i2,chan,p))
	  SumVM(bl,p) = SumVM(bl,p) + Dat(i)*conjg(Model)
	  SumMM(bl,p) = SumMM(bl,p) + Wt(i)*(real(Model)**2+
     *					    aimag(Model)**2)
	enddo
c
c  Get the solution from the accumulated crap.
c
	do p=1,npol
	  call Solve(nants,nbl,SumVM(1,p),SumMM(1,p),
     *	      Gains(1,p),refant,minant,tol*tol,epsi,.false.)
	enddo
	
	end
c************************************************************************
	subroutine intext(npol,nants,nchan,nspect,nschan, pass)
c
	implicit none
	integer npol, nants, nchan, nspect, nschan(nspect)
	complex Pass(nants,nchan,npol)
c
c  Spline fit the band pass table and evaluate the fit for channels
c  that have no solutions.  Do this for real and imaginary separately.
c
c  Input:
c    nants	Number of antennas.
c    npol	Number of polarisations (either 1 or 2).
c    nspect	The total number of frequency bands observed. This is the
c		product of the number of simultaneous spectral windows and
c		the number of different frequency settings.
c    nschan	The number of channels in each observing band.
c    nchan	Total number of channels.
c		NOTE: Here (as elsewhere in this task) "nchan" is the total
c		number of channels (the sum of all the channels from all the
c		bands observed).
c		i.e. nchan = Sum nschan(i)
c    Pass	The bandpass function. This is of size nants * nchan * npol.
c		The bandpass table that we have to write out is in the order
c		nchan * npol * nants, so we have to do some reorganising
c		before we write out.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k,l,ngaps,ischan,ig,chanlo,chanhi,nwidth,npnt,order
	integer ifail
	real rcoeff(3),icoeff(3),x
	real wp(MAXCHAN),xp(MAXCHAN),rp(MAXCHAN),ip(MAXCHAN)
	logical ok(MAXCHAN)
	integer chan1(MAXCHAN/2),chan2(MAXCHAN/2)
	real wrk1(6),wrk2(3*MAXCHAN),a,b,rnorm
	logical within
	complex temp
c
	do l=1,npol
	  do k=1,nants
	    ischan = 0
	    do j=1,nspect
	      if(nschan(j).gt.MAXCHAN)
     *		call bug('f','Too many channels')
c
c  Find the gaps in this spectrum.
c
	      ngaps = 0
	      within = .false.

	      do i=1,nschan(j)
		temp = Pass(k,i+ischan,l)
		ok(i) = abs(real(temp))+abs(aimag(temp)).gt.0
		if(ok(i))then
		  if(within.and.ngaps.gt.0)then
		    chan2(ngaps) = i-1
		  endif
		  within = .false.
		else
		  if(.not.within.and.i.gt.1)then
		    ngaps = ngaps + 1
		    chan1(ngaps) = i
		  endif
		  within = .true.
		endif
	      enddo
c
	      if(within)ngaps = max(ngaps - 1,0)
c
c  We have a list of the gaps in the spectrum. For a gap width of "nwidth",
c  fit a quadratic to the good channels on within "nwidth" channels of
c  the band edge.
c
	      a = 2.0/real(nschan(j)-1)
	      b = 0.5*(nschan(j)+1)
	      do ig=1,ngaps
		nwidth = chan2(ig) - chan1(ig) + 1
		chanlo = max(chan1(ig) - nwidth,1)
		chanhi = min(chan2(ig) + nwidth,nschan(j))
		npnt = 0
		do i=chanlo,chanhi
		  temp = Pass(k,i+ischan,l)
		  if(ok(i))then
		    npnt = npnt + 1
		    xp(npnt) = a*(i-b)
		    rp(npnt) = real(temp)
		    ip(npnt) = aimag(temp)
		    wp(npnt) = 1
		  endif
		enddo
		order = 2
		if(npnt.lt.5)then
		  order = 1
		  rcoeff(3) = 0
		  icoeff(3) = 0
		endif
		call wpfit(order,npnt,xp,rp,wp,rcoeff,
     *					rnorm,wrk1,wrk2,ifail)
		if(ifail.eq.0)
     *		    call wpfit(order,npnt,xp,ip,wp,icoeff,
     *					rnorm,wrk1,wrk2,ifail)
		if(ifail.ne.0)call bug('f','Poly fit failed')
c
c  Interpolate the missing channels.
c
		do i=chan1(ig),chan2(ig)
		  x = a*(i-b)
		  Pass(k,i+ischan,l) = cmplx(
     *		    rcoeff(1) + (rcoeff(2) + rcoeff(3)*x)*x ,
     *		    icoeff(1) + (icoeff(2) + icoeff(3)*x)*x )
		enddo 
	      enddo
c
	      ischan = ischan + nschan(j)
	    enddo
	  enddo
	enddo
c
	end
