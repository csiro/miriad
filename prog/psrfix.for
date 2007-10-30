c************************************************************************
	program psrfix
	implicit none
c
c= psrfix -- Process and copy a pulsar dataset.
c& rjs
c: uv analysis
c+
c	PSRFIX performs de-dispersion, bin averaging/differencing and
c	ephemeris correction on an ATCA dataset observed in pulsar
c	bin mode.
c@ vis
c	The name of the input uv data sets. Several can be given. No default.
c@ select
c	The normal uv selection commands. The default is copy everything.
c	See the help on "select" for more information.
c@ dm
c	This gives up to three numbers, being the pulsar dispersion
c	measure, the pulsar period (in seconds) and the frequency to
c	de-disperse to (in GHz). The default is a dispersion
c	measure of 0, and to de-disperse to the average
c	frequency of the first record. If de-dispersing, a pulsar period
c	must be given.
c       De-dispersion is done including fractional bin shifts.
c@ binsl
c	Bin selection input. It takes an input string of the form
c                      (1,2,4),(4,5,-6),(7,8)
c       The output will be three bins, the first will contain the 
c       sum of the bins 1,2 and 4, the next will contain the sum
c       of 4 and 5 minus 6, and the last will contain the sum of
c       7 and 8. It is possible to have any combinations of bins.
c       However, the number of output bins must be less than or
c       equal to the number of input bins.
c
c	NOTE: This simply adds and differences multiple bins. It does
c	NOT perform any normalization of the result. For example, if
c	you sum three bins of equal value, you will get three times the
c	flux of each bin in the output
c@ out
c	The name of the output uv data set. No default.
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match is supported.
c	  nocal   This option suppresses antenna gain calibration. The
c	          default behaviour is to apply antenna calibration.
c	  nopol   This option suppresses polarisation calibration. The
c	          default behaviour is to apply polarisation calibration.
c	  nopass  This option suppresses bandpass calibration. The
c	          default behaviour is to apply bandpass calibration.
c--
c  History:
c    bs,rjs 28may96 Original version.
c    rjs  29jul96 Fix calibration options, and som FORTRAN standardisation.
c    rjs  10dec96 Better error message when nbin is missing.
c    rjs  12dec96 Fix writing of variables to the right moment.
c    rjs  21apr99 Allow bin selection and de-dispersation in the one go.
c    dpr  09nov00 Fix (+n,-n) bug in bin selection
c    sj   15apr02 Allowed for fractional bin shifts.
c                 This included fixing -ve signs in delay calc.
c                 Flag is set if either bin flag is set
c------------------------------------------------------------------------
c
c  Common block to communicate to the "process" routine.
c
	integer MAXBIN
	parameter(MAXBIN=32)
	integer obins
	integer addarr(MAXBIN,MAXBIN)
	common/pcom/addarr,obins

c
	include 'maxdim.h'
	character version*(*)
	parameter(version='PsrFix: 15-Apr-2002')
	character uvflags*16,ltype*16,out*64,binsl(MAXBIN)*64
	integer tIn,tOut,nchan
	integer i,j,l,n,s
	character state*1,c*1
	logical ok
	logical first,flags(MAXCHAN)
	complex data(MAXCHAN)
	double precision preamble(5),Tprev
	double precision dm,pperiod,dfreq,epsi
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
	integer len1
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags)
	call uvDatInp('vis',uvflags)
	call keya('out',out,' ')
	call keyd('dm',dm,0.d0)
	call keyd('dm',pperiod,0.d0)
	call keyd('dm',dfreq,0.d0)
c
	obins = 0
	dowhile(keyprsnt('binsl'))
	  obins = obins + 1
	  call keya('binsl',binsl(obins),' ')
	enddo
	call keyfin
c
c  Check the input parameters.
c
	if(out.eq.' ')call bug('f','Output file must be specified')
	if(dm.gt.0.and.pperiod.le.0)
     *		call bug('f','Invalid pulsar period given')

c
c  Initialize the adding array.
c
	do i=1,obins
	  do j=1,MAXBIN
	    addarr(i,j) = 0
	  enddo
	enddo
c
c  Decode the bin selection.
c
	do i=1,obins
	  l = len1(binsl(i))
	  ok = .false.
	  if(l.gt.3)ok = binsl(i)(1:1).eq.'('.and.
     *		         binsl(i)(l:l).eq.')'
	  if(.not.ok)call bug('f','Invalid bin selection: '//binsl(i))
c  dpr 09-11-00:
c  n is the bin num we to add/subtract this time through
c  for this output chan
	  n = 0
c  s is sign
	  s = +1
c  state is what to expect next:
c  's' - start?
c  'd' - integer
c  ',' - comma
c  c is the current character
	  state ='s'
	  do j=2,l-1
	    c = binsl(i)(j:j)
	    if(state.eq.'s'.and.c.eq.'-')then
	      s = -1
	      state= 'd'
	    else if(state.eq.'s'.and.c.eq.'+')then
	      s = +1
	      state = 'd'
	    else if(state.eq.','.and.c.eq.',')then
	      if(n.eq.0.or.n.gt.MAXBIN)
     *		call bug('f','Invalid bin selection: '//binsl(i))
c  dpr 09-11-00 ->
	      if (addarr(i,n)+s .eq. 0 ) then
		addarr(i,n) = 0
	      else
		addarr(i,n) = s
	      endif
c <-
	      n = 0
	      s = +1
	      state = 's'
	    else if(c.ge.'0'.and.c.le.'9')then
	      n = 10*n + ichar(binsl(i)(j:j)) - ichar('0')
	      state = ','
	    else
	      call bug('f','Invalid bin selection: '//binsl(i))
	    endif
	  enddo
	  if(state.ne.','.or.n.eq.0.or.n.gt.MAXBIN)
     *	    call bug('f','Invalid bin selection: '//binsl(i))
c  dpr 09-11-00 ->
	      if (addarr(i,n)+s .eq. 0 ) then
		addarr(i,n) = 0
	      else
		addarr(i,n) = s
	      endif
c <-
	enddo
c
c  Various initialisation.
c
	first = .true.
c
c  Open the input and the output files.
c
	call BufIni
	dowhile(uvDatOpn(tIn))
	  call uvDatGta('ltype',ltype)
	  call VarInit(tIn,ltype)
c
c Special processing the first time around.
c
	  if(first)then
	    call uvopen(tOut,out,'new')
	    call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	    call hdcopy(tIn,tOut,'history')
	    call hisopen(tOut,'append')
	    call hiswrite(tOut,'PSRFIX: Miriad '//version)
	    call hisinput(tOut,'PSRFIX')
	    call hisclose(tOut)
	  endif
	  call VarOnit(tIn,tOut,ltype)
c
c  Loop over the data.
c
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  if(nchan.eq.0)call bug('f','No data found')
	  if(first.and.dfreq.le.0)
     *		call uvfit1(tIn,'sfreq',nchan,dfreq,epsi)
	  Tprev = preamble(4)
	  first = .false.
	  dowhile(nchan.gt.0)
c
c  Flush if needed.
c
	    if(abs(preamble(4)-Tprev).gt.1d0/86400d0)then
	      call BufFlush(tOut,dm,pperiod,dfreq)
	    endif
	    call BufAcc(tIn,preamble,data,flags,nchan)
	    call VarCopy(tIn,tOut)
	    Tprev = preamble(4)
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
c
c  Flush out anything remaining.
c
	  call BufFlush(tOut,dm,pperiod,dfreq)
	  call uvDatCls
	enddo
c
c  Close up files.
c
	call uvclose(tOut)
	end
c************************************************************************
	subroutine GetOpt(uvflags)
c
	implicit none
	character uvflags*(*)
c
c  Determine the flags to pass to the uvdat routines.
c
c  Output:
c    uvflags	Flags to pass to the uvdat routines.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=3)
	character opts(nopts)*9
	logical present(nopts)
	data opts/'nocal    ','nopol    ','nopass   '/
c
	call options('options',opts,present,nopts)
c
c Set up calibration flags
c
	uvflags = 'd3'
	if(.not.present(1))uvflags(3:3) = 'c'
	if(.not.present(2))uvflags(4:4) = 'e'
	if(.not.present(3))uvflags(5:5) = 'f'
	end
c************************************************************************
	subroutine BufIni
c
	implicit none
c------------------------------------------------------------------------
	include 'psrfix.h'
	nslots = 0
	npols = 0
	nants = 0
	nbins = 0
	end
c************************************************************************
	subroutine BufAcc(tIn,preamble,data,flags,nchan)
c
	implicit none
	integer tIn,nchan
	double precision preamble(5)
	logical flags(nchan)
	complex data(nchan)
c
c  Buffer up an integration.
c------------------------------------------------------------------------
	include 'psrfix.h'
	integer bl,i1,i2,p,b,i,j,nb1,nb2
c
c  Save some stuff that should not vary during an integration.
c
	if(nslots.eq.0)then
	  time = preamble(4)
	  nchans = nchan
	  call uvinfo(tIn,'sfreq',sfreq)
	  call uvrdvri(tIn,'nbin',nbins,0)
	  if(nbins.eq.0)call bug('f',
     *	    'Variable "nbin" is missing from the vis. dataset')
	  if(nbins.gt.MAXBINS)call bug('f','Too many pulsar bins')
	endif
c
	if(nchan.ne.nchans)call bug('f',
     *	    'Varying number of channels within one integration')
c
c  Determine the baseline and polarisation indices.
c  If its a new baseline, save some info on it.
c
	call Basant(preamble(5),i1,i2)
	bl = ((i2-1)*i2)/2 + i1
	if(bl.gt.MAXBASE)
     *	  call bug('f','Too many baselines for me')
c
	nb1 = ((nants-1)*nants)/2 + nants
	nants = max(nants,i1,i2)
	nb2 = ((nants-1)*nants)/2 + nants
	do j=nb1+1,nb2
	  do i=1,MAXPOL
	    slot(i,j) = 0
	  enddo
	enddo
c
c  Copy the uvw coordinates.
c
	do i=1,3
	  uvw(i,bl) = preamble(i)
	enddo
c
c  Determine the polarisation index.
c
	call uvDatGti('pol',j)
	p = 0
	do i=1,npols
	  if(pols(i).eq.j)p = i
	enddo
	if(p.eq.0)then
	  npols = npols + 1
	  if(npols.gt.MAXPOL)call bug('f','Too many polarisations')
	  p = npols
	  pols(p) = j
	endif
c
	if(slot(p,bl).eq.0)then
	  nslots = nslots + 1
	  if(nslots.gt.MAXSLOT)call bug('f','Too many slots')
	  slot(p,bl) = nslots
	  do i=1,MAXBINS
	    pFlags(i,nslots) = 0
	    pData( i,nslots) = 0
	  enddo
	endif
	p = slot(p,bl)
c
c  Determine the pulsar bin number.
c
	call uvrdvri(tIn,'bin',b,1)
	if(b.le.0)call bug('f','Illegal bin number')
	if(b.gt.nbins)call bug('f','Too many bins')
c
c  Store the data and flags.
c
	if(pFlags(b,p).eq.0)call memAlloc(pFlags(b,p),nchan,'l')
	if(pData(b,p).eq.0)call memAlloc(pData(b,p),nchan,'c')
	call DatCpy(nchan,Data,Flags,
     *				memc(pData(b,p)),meml(pFlags(b,p)))
c
	end
c************************************************************************
	subroutine BufFlush(tOut,dm,pperiod,dfreq)
c
	implicit none
	integer tOut
	double precision dm,pperiod,dfreq
c
c  Pass the data to the pulsar processing routine, and then write it
c  out.
c
c------------------------------------------------------------------------
	include 'psrfix.h'
	integer b,p,i,j,i1,i2,np,bl,obins
	integer pslot(MAXPOL),ptype(MAXPOL)
	integer rFlags,rData,iFlags,iData,oFlags,oData
	double precision preamble(5)
c
c  Allocate scratch buffers.
c
	call memAlloc(iData,nbins*npols*nchans,'c')
	call memAlloc(iFlags,nbins*npols*nchans,'l')
	call memAlloc(oData,nbins*npols*nchans,'c')
	call memAlloc(oFlags,nbins*npols*nchans,'l')
c
c  Duplicate the frequency axis thingo "npols" times.
c
	b = nchans
	do j=2,npols
	  do i=1,nchans
	    b = b + 1
	    sfreq(b) = sfreq(i)
	  enddo
	enddo
c
c  Loop over all baselines.
c
	bl = 0
	do i2=1,nants
	  do i1=1,i2
	    bl = bl + 1
	    np = 0
	    do p=1,npols
	      if(slot(p,bl).ne.0)then
		np = np + 1
		pslot(np) = slot(p,bl)
		ptype(np) = pols(p)
	      endif
	    enddo
c
	    if(np.gt.0)then
c
c  Pack the data into the form required by the pulsar processing
c  routine.
c
	      rData  = iData
	      rFlags = iFlags
	      do b=1,nbins
		do i=1,np
		  p = pslot(i)
		  if(pData(b,p).gt.0)then
		    call DatCpy(nchans,
     *			memc(pData(b,p)),meml(pFlags(b,p)),
     *			memc(rData),     meml(rFlags))
		  else
		    call DatFlg(nchans,memc(rData),meml(rFlags))
		  endif
		  rData  = rData + nchans
		  rFlags = rFlags + nchans
		enddo
	      enddo
c
c  We have packed this baseline into a pulsar record. Now pass it
c  to the main processing routine, at last!!!
c
	      call Process(npols*nchans,nbins,sfreq,dm,dfreq,pperiod,
     *		memc(iData),meml(iFlags),memc(oData),meml(oFlags),obins)
c
c  Do some things common to all baselines.
c
	      do i=1,3
	        preamble(i) = uvw(i,bl)
	      enddo
	      preamble(4) = time
	      preamble(5) = 256*i1 + i2
	      call uvputvri(tOut,'npol',np,1)
	      call uvputvri(tOut,'nbin',obins,1)
c
c  Now unpack it and write it out.
c
	      rData = oData
	      rFlags = oFlags
	      do b=1,obins
		call uvputvri(tOut,'bin',b,1)
		do i=1,np
		  call uvputvri(tOut,'pol',ptype(i),1)
		  call uvwrite(tOut,preamble,
     *				memc(rData),meml(rFlags),nchans)
		  rData = rData + nchans
		  rFlags = rFlags + nchans
		enddo
	      enddo
	    endif
c
	  enddo
	enddo
c
c  Free up the scratch buffers.
c
	call memFree(iData,nbins*npols*nchans,'c')
	call memFree(iFlags,nbins*npols*nchans,'l')
	call memFree(oData,nbins*npols*nchans,'c')
	call memFree(oFlags,nbins*npols*nchans,'l')
c
c  Free up all the slots and re-initialise.
c
	do j=1,nslots
	  do i=1,nbins
	    if(pData(i,j).ne.0) call memFree(pData(i,j), nchans,'c')
	    if(pFlags(i,j).ne.0)call memFree(pFlags(i,j),nchans,'l')
	  enddo
	enddo
	call BufIni
c
	end
c************************************************************************
	subroutine Process(nchan,nbins,sfreq,dm,dfreq,pperiod,
     *	  DatIn,FlgIn,DatOut,FlgOut,obins1)
c
	implicit none
	integer nchan,nbins,obins1
	double precision sfreq(nchan),dm,dfreq,pperiod
	complex DatIn(nchan,nbins),DatOut(nchan,nbins)
	logical FlgIn(nchan,nbins),FlgOut(nchan,nbins)
c
c  Process this batch of stuff.
c
c  Input:
c    nchan	Total number of channels.
c    nbins	Number of pulsar bins.
c    sfreq	Observing frequency of the channel, in GHz.
c    dm		Dispersion measure.
c    dfreq	Frequency to de-disperse to, in GHz.
c    pperiod	Pulsar period in seconds.
c    DatIn	Input correlation data.
c    FlgIn	Flags associated with the correlations.
c  Output:
c    DatOut	Output correlation data.
c    FlgOut	Flags associated with the correlations.
c    obins1	Number of output bins.

c------------------------------------------------------------------------
	integer MAXBIN
	parameter(MAXBIN=32)
	integer addarr(MAXBIN,MAXBIN),obins
	common/pcom/addarr,obins
	double precision dtcfreq,dt
	real mv,fmv
	integer i,j,k,imv,bb
c
c       Initializeing the Output arrays
c
	if(obins.gt.nbins)call bug('f','Too many output bins')

	do i=1,nchan
	   do j=1,nbins
	      DatOut(i,j) = 0
	      FlgOut(i,j) = .false.
	   enddo
	enddo
c
	if(dm.gt.0)then
	   
c       The delay relative to infinite frequency for the 
c       specified frequency (dfreq)
c	1e3 factor for GHz->MHz
c       SJ NOTE - delay should always be -ve.
	   
	   dtcfreq = -4.149383E3 * dm/(dfreq * 1e3)**2
	   
c       Calculate the delay for each of the channels relative to the above freq. 
	   
	   do i=1,nchan

c dt in secs
c SJ NOTE - again the -ve sign
	      dt = (-4.149383E3 * dm/(sfreq(i)*1e3)**2) - dtcfreq

c work out mv in terms of bins and force it to lie between 0 and nbins
	      mv = dt/pperiod * nbins
	      dowhile (mv.gt.nbins)
		mv = mv - nbins
	      enddo
	      dowhile (mv.lt.0.)
		mv = mv + nbins
	      enddo
c imv is the integer part and fmv the fractional part
	      imv= int(mv)
	      fmv = mv - imv

c move to right forced by above while loops
	      do j=1,nbins
	         bb=j+imv
	         if(bb.gt.nbins)bb=bb-nbins
		 if(j.eq.1)then
		    DatOut(i,bb) = (1.-fmv) * DatIn(i,j) +
     :                             fmv * DatIn(i,nbins)
		    if(FlgIn(i,j).and.Flgin(i,nbins))FlgOut(i,bb)=.true.
		 else
		    DatOut(i,bb) = (1.-fmv)*DatIn(i,j)+fmv*DatIn(i,j-1)
		    FlgOut(i,bb) = FlgIn(i,j)
		    if(FlgIn(i,j) .and. Flgin(i,j-1))FlgOut(i,bb)=.true.
		 endif
	      enddo
	   enddo
	   obins1 = nbins
	endif

c
c	Copy back to the input, if needed.
c

	if(dm.gt.0.and.obins.gt.0)then
	  do i=1,nchan
	     do j=1,nbins
		DatIn(i,j) = DatOut(i,j)
		FlgIn(i,j) = FlgOut(i,j)
	        DatOut(i,j) = 0
	        FlgOut(i,j) = .true.
	     enddo
	  enddo
	endif
c
c       The adding of the bins in groups as defined above by binsl.
c	

	if(obins.gt.0)then
	   do i=1,obins
	      do j=1,nbins
		 if (addarr(i,j).gt.0) then
		    do k=1,nchan
		       DatOut(k,i) = DatOut(k,i) + DatIn(k,j)
		       FlgOut(k,i) = FlgOut(k,i).and.FlgIn(k,j)
		    enddo
		 elseif (addarr(i,j).lt.0) then
		    do k=1,nchan
		       DatOut(k,i) = DatOut(k,i) - DatIn(k,j)
		       FlgOut(k,i) = FlgOut(k,i).and.FlgIn(k,j)
		    enddo
		 endif
	      enddo
	   enddo
	   obins1 = obins
	endif

c
	end
c************************************************************************
	subroutine DatFlg(nchan,Data,Flags)
c
	implicit none
	integer nchan
	complex Data(nchan)
	logical Flags(nchan)
c------------------------------------------------------------------------
	integer i
	do i=1,nchan
	  Data(i) = 0
	  Flags(i) = .false.
	enddo
c
	end
c************************************************************************
	subroutine DatCpy(nchan,Inc,Inf,Outc,Outf)
c
	implicit none
	integer nchan
	complex Inc(nchan),Outc(nchan)
	logical Inf(nchan),Outf(nchan)
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  Outc(i) = Inc(i)
	  Outf(i) = Inf(i)
	enddo
c
	end

