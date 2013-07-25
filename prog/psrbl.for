c************************************************************************
	program psrbl
	implicit none
c
c= psrbl -- Subtract off-pulse uv baseline
c& dpr
c: uv analysis
c+
c	PSRBL subtracts the off-pulse bins (the baseline) of visibilities 
c       from the on-pulse bins of the visibilities, to remove continuum 
c       sources. 
c
c       Note: Although PSRBL does an excellent job of removing continuum
c       sources, it has the side-effect of scattering noise OUT OF the
c       off-pulse bins and INTO the on-pulse bins. For weak sources,
c       this can make plots made with PSRPLT, mode=amplitude look
c       artifically significant (or just plain strange). To moderate
c       this effect, always set the binsl key to cover all the available
c       off-pulse bins.
c@ vis
c	The name of the input uv data sets. Several can be given. No default.
c@ select
c	The normal uv selection commands. The default is copy everything.
c	See the help on "select" for more information.
c@ binsl
c	Bin selection to use for the baseline. This should consist
c       of comma-separated start/stop pairs, eg.
c         binsl = 1,14,17,32
c       In this example, the pulsar is detected in bins 15 and 16,
c       so all other bins (1-14 and 17-32) are used for the baseline.   
c
c       To select a single bin, use eg. 1,1
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
c    dpr  Original version, copied from psrfix. 
c         After a trial period, the common routines should be consolidated
c         into a library?
c------------------------------------------------------------------------
c
c
	integer MAXBIN
	parameter(MAXBIN=32)
	
c
	include 'maxdim.h'
	character version*(*)
	parameter(version='PsrBl: version 1.0 14-Mar-01')
	character uvflags*16,ltype*16,out*64
	integer tIn,tOut,nchan
	integer i
	logical first,flags(MAXCHAN)
	complex data(MAXCHAN)
	double precision preamble(5),Tprev

c  number bin start/stop pairs specified:
	integer npairs

c  for getting binsl for use
        integer binstart,binstop

c  The factor to multiply each bin by before adding
c  - For non-baseline bins, will be zero
c  - For baseline bins, will be -1/nbaselinebins
c  keep one number for each bin - might be useful one
c  day.
	double precision  binfac(MAXBIN)
        integer nbaselinebins
c  binuse says which ones to use for the subtraction
c  used to apply flagging
	logical   binuse(MAXBIN)
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags)
	call uvDatInp('vis',uvflags)
	call keya('out',out,' ')
c
c  Get the bins for the baseline
c
c  init
	npairs = 0
	nbaselinebins = 0
	do i=1,MAXBIN
	  binfac(i)=0.d0
	end do
c
c  Get the start/stop
c
	dowhile(keyprsnt('binsl'))
	  call keyi('binsl',binstart,0)
	  if (binstart .eq. 0) 
     *      call bug('f','Bad bin start pattern specifier')
	  if (keyprsnt('binsl')) then
	    call keyi('binsl',binstop,0)
	    if (binstart .eq. 0) 
     *        call bug('f','Bad bin stop pattern specifier')
          else
	    call bug('f','Odd number of binsl start/stop pairs')
	  end if
c
c  We have the start and stop; fill in binfac with ones to match
c
	  do i=binstart,binstop
	    binfac(i)=1
	    binuse(i) = .true.
	    nbaselinebins=nbaselinebins+1
	  end do
	  npairs = npairs + 1
	enddo
	call keyfin
c
c  multiply binfac by the scale factor
c
	do i=1,MAXBIN
	  if (binuse(i)) binfac(i)=-1.0*binfac(i)/nbaselinebins
	end do
c
c  Check the input parameters.
c
	if(out.eq.' ')call bug('f','Output file must be specified')
	if(nbaselinebins .eq. 0 ) 
     *    call bug('f','No bins selected for the baseline')
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
	    call hiswrite(tOut,'PSRBL: Miriad '//version)
	    call hisinput(tOut,'PSRBL')
	    call hisclose(tOut)
	  endif
	  call VarOnit(tIn,tOut,ltype)
c
c  Loop over the data.
c
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  if(nchan.eq.0)call bug('f','No data found')
	  Tprev = preamble(4)
	  first = .false.
	  dowhile(nchan.gt.0)
c
c  Flush if needed.
c
	    if(abs(preamble(4)-Tprev).gt.1d0/86400d0)then
	      call BufFlush(tOut,binfac,binuse)
	    endif
	    call BufAcc(tIn,preamble,data,flags,nchan)
	    call VarCopy(tIn,tOut)
	    Tprev = preamble(4)
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
c
c  Flush out anything remaining.
c
	  call BufFlush(tOut,binfac,binuse)
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
	uvflags = 'd3l'
	if(.not.present(1))uvflags(4:4) = 'c'
	if(.not.present(2))uvflags(5:5) = 'e'
	if(.not.present(3))uvflags(6:6) = 'f'
	end
c************************************************************************
	subroutine BufIni
c
	implicit none
c------------------------------------------------------------------------
	include 'psrbl.h'
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
	include 'psrbl.h'
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
	subroutine BufFlush(tOut,binfac,binuse)
c
	implicit none
	integer tOut
c
c  Pass the data to the pulsar processing routine, and then write it
c  out.
c
c------------------------------------------------------------------------
	include 'psrbl.h'
	integer b,p,i,j,i1,i2,np,bl
	integer pslot(MAXPOL),ptype(MAXPOL)
	integer rFlags,rData,iFlags,iData,oFlags,oData
	double precision preamble(5),binfac(nbins)
        logical binuse(nbins)
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
	      call Process(npols*nchans,nbins,sfreq,
     *		memc(iData),meml(iFlags),memc(oData),meml(oFlags),
     *          binfac,binuse)
c
c  Do some things common to all baselines.
c
	      do i=1,3
	        preamble(i) = uvw(i,bl)
	      enddo
	      preamble(4) = time
	      preamble(5) = 256*i1 + i2
	      call uvputvri(tOut,'npol',np,1)
	      call uvputvri(tOut,'nbin',nbins,1)
c
c  Now unpack it and write it out.
c
	      rData = oData
	      rFlags = oFlags
	      do b=1,nbins
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
	subroutine Process(nchan,nbins,sfreq,
     *	  DatIn,FlgIn,DatOut,FlgOut,binfac,binuse)
c
	implicit none
	integer nchan,nbins
	double precision sfreq(nchan),binfac(nbins)
        logical binuse(nbins)
	complex DatIn(nchan,nbins),DatOut(nchan,nbins)
	logical FlgIn(nchan,nbins),FlgOut(nchan,nbins)
	
c
c  Process this batch of stuff to remove the baseline
c
c  Input:
c    nchan	Total number of channels.
c    nbins	Number of pulsar bins.
c    sfreq	Observing frequency of the channel, in GHz.
c    DatIn	Input correlation data.
c    FlgIn	Flags associated with the correlations.
c    binfac     Factor to multiply bin value by for subtraction
c    binuse     Is this bin in the baseline list?
c  Output:
c    DatOut	Output correlation data.
c    FlgOut	Flags associated with the correlations.

c------------------------------------------------------------------------
	integer MAXBIN
	parameter(MAXBIN=32)

	integer i,j,k

c
c       Subtract the flux
c

	if(nbins.gt.0) then
	  do i=1,nbins
c
c           assign original bin value
c
	    do k=1,nchan
	      DatOut(k,i) = DatIn(k,i)
	      FlgOut(k,i) = FlgIn(k,i)
	    enddo
c
c           subtract the baseline channels
c
	    do j=1,nbins
	      do k=1,nchan
		if (binuse(j)) then
		  DatOut(k,i) = DatOut(k,i) + binfac(j)*DatIn(k,j)
		  FlgOut(k,i) = FlgOut(k,i).and.FlgIn(k,j)
		endif
	      enddo
	    enddo
	  enddo
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

