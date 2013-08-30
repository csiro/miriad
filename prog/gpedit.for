c********1*********2*********3*********4*********5*********6*********7*c
	program gpedit
	implicit none
c
c= Gpedit -- Edit the gain table.
c& mchw
c: calibration
c+
c	Gpedit is a MIRIAD task which modifies calibration tables.
c@ vis
c	The input visibility file, containing the gain file to modify.
c	No default.
c@ select
c	Normal uv data selection commands. See the help on "select" for
c       more information. Currently only antenna, time, and amplitude
c       selection is supported. The amplitude selection applies to the 
c       gains, not the uv-data, and can be used to flag or replace bad 
c       gain amplitudes. The default is to select everything.
c@ feeds
c	The polarisation feeds affected (e.g. R, L, X or Y). Default is
c	all feeds.
c@ gain
c	This gives the complex-valued gain used in the `multiply' and
c	`replace' options (see below). It is given as an amplitude
c	and phase (in degrees). For example gain=2,90 produces a gain
c	with a amplitude of 2 and phase of 90 degrees. The default is 1,0.
c@ options
c	This gives extra processing options. Several values can be given,
c	separated by commas. Option values can be abbreviated to
c       uniqueness.
c
c	The following options operate on the gains:
c	  replace   The existing gains are replaced by the value
c	            given by the `gain' keyword. NOTE: If no other
c	            options are given, the replace option is performed.
c	  multiply  The existing gains are multiplied by the gain
c	            given by the `gain' keyword.
c	  flag      The existing gains are flagged as bad.
c	  amplitude The phases of the existing gains are set to 0.
c	  phase     The amplitudes of the existing gains are set to 1.
c	  scale     The phase of the gains is multiplied by the factor 
c	            given by the `gain' keyword. 
c	  dup       Convert a single-polarization gain table into a dual
c	            polarization table.
c         invert    The existing gains are inverted (1/gain, phase 
c                   negated) to allow undoing calibration that has been 
c                   applied to the uvdata with uvaver.
c
c	The following option operates on the polarization leakages:
c	  reflect   The existing leakages are made to possess a
c	            symmetry: For each antenna, we make
c	            DX = -conjg(DY).
c	  zmean     Subtract an offset, so that the mean leakage is 0.
c
c	Example:
c	  gpedit vis=cyga gain=14.4,90 select=ant(1,2) feeds=X
c--
c  History:
c    rjs    4sep91 gpbreak. gpedit code copied from gpbreak.
c    mchw  23apr96 keyword driven task for the squeamish. Instead of BEE.
c    rjs   25feb97 Tidy up and extend the possibilities.
c    rjs   26feb97 Fix feeds reading.
c    rjs   24jun97 Add the reflect option.
c    rjs   01aug97 Added options=zmean.
c    mchw  18nov98 Added options=scale.
c    rjs   01dec98 Added options=dup.
c    tw    16aug03 Allow gain amplitude selection
c    rjs   02jan05 Correct gain selection.
c    mhw   01mar11 Added options=invert
c    mhw   08sep11 Handle freq binned gains
c-----------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
        include 'mirconst.h'
	integer MAXFEED,MAXSELS
	character version*72
	parameter(MAXFEED=2,MAXSELS=300)
c
	character vis*64
	logical domult,dorep,doflag,doamp,dophas,dorefl,dozm,doscal
	logical dogain,doleak,dup,doinv
	integer iostat,tVis,itLeak,nants,nfeeds,nsols,ntau,i
	integer numfeed,feeds(MAXFEED),nleaks,nfbin,ngains,maxgains
	complex gain,Leaks(2,MAXANT)
	real amp,phi,sels(MAXSELS)
	logical mask(2*MAXANT)
	integer pGains,pTimes
        double precision freq(MAXFBIN)
c
c  Externals.
c
	integer hsize
	logical hdprsnt
        character versan*72
	external MultOp,RepOp,FlagOp,AmpOp,PhasOp,ScalOp,InvOp
c
c  Get the input parameters.
c
        version = versan('gpedit',
     *                   '$Revision: 1.0',
     *                   '$Date: ')
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','No input vis data-set given')
	call SelInput('select',sels,MAXSELS)
	call keyr('gain',amp,1.)
	call keyr('gain',phi,0.)
	call mkeyfd('feeds',feeds,MAXFEED,numfeed)
        call GetOpt(dorep,domult,doflag,doamp,dophas,dorefl,dozm,
     *	  doscal,dup,doinv)
	dogain = dorep.or.domult.or.doflag.or.doamp.or.
     *				 dophas.or.doscal.or.dup.or.doinv
	doleak = dorefl.or.dozm
	call keyfin
c
c  Open the input file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call EditBug(iostat,'Error opening '//vis)
	if(dogain.and..not.hdprsnt(tVis,'gains'))
     *	  call bug('f','The dataset has no gain table')
	if(doleak.and..not.hdprsnt(tVis,'leakage'))
     *	  call bug('f','The dataset has no leakage table')
c
c  Determine the number of things in the gain table.
c
	if(dogain)then
	  call rdhdi(tVis,'ntau',ntau,0)
	  if(ntau.ne.0)call bug('f',
     *	  'GPEDIT cannot cope with a gain table with delays')
	  call rdhdi(tVis,'ngains',nants,0)
	  call rdhdi(tVis,'nfeeds',nfeeds,1)
	  if(nfeeds.le.0.or.nfeeds.gt.2.or.nants.lt.nfeeds.or.
     *	    mod(nants,nfeeds).ne.0)
     *	    call bug('f','Bad number of gains or feeds in '//vis)
	  nants = nants / nfeeds
	  if(nfeeds.eq.2.and.dup)call bug('w',
     *	    'Gain table is already a dual polarization table')
	  dup = dup.and.nfeeds.eq.1
	  call rdhdi(tVis,'nsols',nsols,0)
c
c  See if we have enough space.
c
	  if(nants.gt.MAXANT)
     *	    call bug('f','Too many antennae for me to cope with')
	  if(nsols.le.0)
     *	    call bug('f','Bad number of solutions')
          maxgains = nsols*nants*2*(maxfbin+1)
	  call memAlloc(pGains,maxgains,'c')
	  call memAlloc(pTimes,nsols,'d')
c
c  Check the given feed numbers, and set the default feed numbers 
c  if needed.
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
	  call SetMask(nfeeds,nants,mask,feeds,numfeed,sels)
c
c  Open the gains file. Mode=='append' so that we can overwrite it.
c
c	  call haccess(tVis,itGain,'gains','append',iostat)
c	  if(iostat.ne.0)call EditBug(iostat,'Error accessing gains')
c
c  Read the gains.
c
          call uvGnRead(tVis,memc(pGains),memd(pTimes),freq,
     *    ngains,nfeeds,ntau,nsols,nfbin,maxgains,nsols,maxfbin)
c	  call GainRd(itGain,nsols,nants,nfeeds,
c     *				memd(pTimes),memc(pGains))
c
c  Edit the gains.
c
	  gain = amp * cmplx(cos(phi*pi/180.),sin(phi*pi/180.))
	  if(dorep) call GainEdt(nsols,nants*nfeeds,nfbin,
     *	    memd(pTimes),memc(pGains),mask,sels,gain,RepOp)
	  if(domult)call GainEdt(nsols,nants*nfeeds,nfbin,
     *	    memd(pTimes),memc(pGains),mask,sels,gain,MultOp)
	  if(doflag)call GainEdt(nsols,nants*nfeeds,nfbin,
     *	    memd(pTimes),memc(pGains),mask,sels,gain,FlagOp)
	  if(doamp )call GainEdt(nsols,nants*nfeeds,nfbin,
     *	    memd(pTimes),memc(pGains),mask,sels,gain,AmpOp)
	  if(dophas)call GainEdt(nsols,nants*nfeeds,nfbin,
     *	    memd(pTimes),memc(pGains),mask,sels,gain,PhasOp)
	  if(doscal)call GainEdt(nsols,nants*nfeeds,nfbin,
     *	    memd(pTimes),memc(pGains),mask,sels,gain,ScalOp)
	  if(doinv) call GainEdt(nsols,nants*nfeeds,nfbin,
     *	    memd(pTimes),memc(pGains),mask,sels,gain,InvOp)
c
c  Duplicate gains if needed
c     
          if (dup) call GainDup(memc(pGains),ngains,maxgains)
            
c
c  Write out the gains.
c
          call uvGnWrit(tVis,memc(pGains),memd(pTimes),freq,ngains,
     *      nsols,nfbin,maxgains,nsols,maxfbin,.false.)
c	  call GainWr(itGain,dup,nsols,nants,nfeeds,
c     *				memd(pTimes),memc(pGains))
	  call memFree(pTimes,nsols,'d')
	  call memFree(pGains,nfeeds*nants*nsols,'c')
c	  call hdaccess(itGain,iostat)
	  if(dup)then
	    call wrhdi(tVis,'nfeeds',2)
	    call wrhdi(tVis,'ngains',2*nants)
	  endif
	endif
c
c  Process the leakages if needed.
c
	if(doleak)then
	  call haccess(tVis,itLeak,'leakage','append',iostat)
	  if(iostat.ne.0)call EditBug(iostat,'Error accessing leakages')
	  nLeaks = (hsize(itLeak)-8)/16
	  if(nLeaks.lt.1)call bug('f','Leakage table appears bad')
	  call hreadr(itLeak,Leaks,8,16*nLeaks,iostat)
	  if(iostat.ne.0)call EditBug(iostat,
     *				'Error reading leakage table')
	  if(dorefl.or.dozm)call LeakIt(Leaks,nLeaks,sels,dorefl,dozm)
	  call hwriter(itLeak,Leaks,8,16*nLeaks,iostat)
	  call hdaccess(itLeak,iostat)
	endif
c
c  Write out some history now.
c
	call hisopen(tVis,'append')
	call hiswrite(tVis,'GPEDIT: Miriad '//version)
	call hisinput(tVis,'GPEDIT')
	call hisclose(tVis)
c
c  Close up everything.
c
	call hclose(tVis)	
	end
c************************************************************************
	subroutine Leakit(Leaks,nants,sels,dorefl,dozm)
c
	implicit none
	integer nants
	real sels(*)
	complex Leaks(2,nants)
	logical dorefl,dozm
c
c  Make the leakages have that funny symmetry.
c------------------------------------------------------------------------
	integer i,n
	complex D
c
c  Externals.
c
	logical SelProbe
c
	if(dorefl)then
	  do i=1,nants
	    if(SelProbe(sels,'antennae',dble(257*i)))then
	      D = 0.5*(Leaks(1,i) - conjg(Leaks(2,i)))
	      Leaks(1,i) = D
	      Leaks(2,i) = -conjg(D)
	    endif
	  enddo
	endif
c
	if(dozm)then
	  D = 0
	  n = 0
	  do i=1,nants
	    if(SelProbe(sels,'antennae',dble(257*i)))then
	      D = D + Leaks(1,i) - conjg(Leaks(2,i))
	      n = n + 2
	    endif
	  enddo
	  D = D / n
	  do i=1,nants
	    if(SelProbe(sels,'antennae',dble(257*i)))then
	      Leaks(1,i) = Leaks(1,i) - D
	      Leaks(2,i) = Leaks(2,i) + conjg(D)
	    endif
	  enddo
	endif
c
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine SetMask(nfeeds,nants,mask,feeds,numfeed,sels)
c
	implicit none
	integer nfeeds,nants,numfeed
	integer feeds(numfeed)
	real sels(*)
	logical mask(nfeeds,nants)
c
c  Set up the breakpoint mask.
c
c  Input:
c    nfeeds
c    nants
c    feeds
c    numfeed
c    sels
c  Output:
c    mask
c-----------------------------------------------------------------------
	integer i,i0,j
c
c  Externals.
c
	logical SelProbe
c
	do j=1,nants
	  do i=1,nfeeds
	    mask(i,j) = .false.
	  enddo
	  if(SelProbe(sels,'antennae',dble(257*j)))then
	    do i0=1,numfeed
	      i = feeds(i0)
	      mask(i,j) = .true.
	    enddo
	  endif
	enddo
c
	end
c********1*********2*********3*********4*********5*********6*********7*c
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
c-----------------------------------------------------------------------
	integer offset,iostat,k
c
	offset = 8
	do k=1,nsols
	  call hreadd(itGain,times(k),offset,8,iostat)
	  if(iostat.ne.0)call EditBug(iostat,'Error reading gain time')
	  offset = offset + 8
	  call hreadr(itGain,Gains(1,k),offset,8*nfeeds*nants,iostat)
	  if(iostat.ne.0)call EditBug(iostat,'Error reading gains')
	  offset = offset + 8*nfeeds*nants
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine GainEdt(nsols,nants,nfbin,times,Gains,mask,sels,
     *							gain,oper)
c
	implicit none
	integer nsols,nants,nfbin
	double precision times(nsols)
	complex Gains(nants,nsols,0:nfbin),gain
	real sels(*)
	logical mask(nants)
	external oper
c
c  Edit the gains.
c
c  Input:
c    nsols	number of solutions.
c    nants	Number of antennae times the number of feeds.
c    mask	Antenna/feed mask.
c    sels	UV selection array.
c    gain	A complex gain to be used.
c    oper	The routine to perform the operation.
c  Input/Output:
c    gains	The gains.
c-----------------------------------------------------------------------
	integer i,j,k
c
c  Externals.
c
	logical SelProbe
c
        do k=0,nfbin
	  do j=1,nsols
	    if(SelProbe(sels,'time',times(j)))then
	      do i=1,nants
	        if (mask(i)) then
		   if (SelProbe(sels,'amplitude',
     *			dble(abs(Gains(i,j,k))))) then
		      call oper(Gains(i,j,k),gain)
		   endif
	        endif
	      enddo
	    endif
          enddo
	enddo
c
	end
c************************************************************************
c
c  These are the service routines to perform the desired operation.
c
	subroutine MultOp(Gain,fac)
c
	implicit none
	complex Gain,fac
c
	Gain = Gain * fac
	end
	subroutine RepOp(Gain,fac)
c
	implicit none
	complex Gain,fac
c
	Gain = fac
	end
	subroutine FlagOp(Gain,fac)
c
	implicit none
	complex Gain,fac
c
	Gain = 0
	end
	subroutine AmpOp(Gain,fac)
c
	implicit none
	complex Gain,fac
c
	Gain = abs(Gain)
	end
	subroutine PhasOp(Gain,fac)
c
	implicit none
	complex Gain,fac
c
	real t
	t = abs(Gain)
	if(t.gt.0)Gain = Gain / t
	end
c
	subroutine ScalOp(Gain,fac)
c
	implicit none
	complex Gain,fac
c
	complex expi
	real phase
	Gain = abs(Gain)*expi(real(fac)*phase(Gain))
	end
c
	subroutine InvOp(Gain,fac)
c
	implicit none
	complex Gain,fac
c
        real t
        t = abs(Gain)
	if (t.gt.0) Gain = 1 / Gain
	end
        
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine GainDup(Gains,ngains,maxgains)
c
	implicit none
	integer ngains,maxgains
	complex Gains(maxgains)
c
c  Duplicate the gains to turn a single feed table into a dual feed one
c
c  Input/Output:
c    Gains	The single feed gains
c    ngains	Number gains
c    maxgains	Max number of gains
c-----------------------------------------------------------------------
	include 'maxdim.h'
c
	integer i
c
	if(2*ngains.gt.maxgains)call bug('f',
     *     'Too many gains in GainDup')
        do i=ngains,1,-1
          gains(i*2-1)=gains(i)
          gains(i*2)=gains(i)
	enddo
        ngains=ngains*2
	end
        
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine GainWr(itGain,dup,nsols,nants,nfeeds,times,Gains)
c
	implicit none
	integer itGain,nsols,nants,nfeeds
	complex Gains(nfeeds*nants,nsols)
	double precision times(nsols)
	logical dup
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
c    dup	If true, convert a single into a dual polarization table.
c-----------------------------------------------------------------------
	include 'maxdim.h'
c
	complex G(MAXANT)
	integer offset,iostat,i,j,k
c
	if(nants.gt.MAXANT)call bug('f','Too many gains in GainWr')
	offset = 8
	do k=1,nsols
	  call hwrited(itGain,times(k),offset,8,iostat)
	  if(iostat.ne.0)call EditBug(iostat,'Error writing gain time')
	  offset = offset + 8
	  if(dup)then
	    j = 1
	    do i=1,nants
	      G(j) = Gains(i,k)
	      G(j+1) = G(j)
	      j = j + 2
	    enddo
	    call hwriter(itGain,G,offset,8*2*nants,iostat)
	    offset = offset + 8*2*nants
	  else
	    call hwriter(itGain,Gains(1,k),offset,8*nfeeds*nants,
     *							    iostat)
	    offset = offset + 8*nfeeds*nants
	  endif
	  if(iostat.ne.0)call EditBug(iostat,'Error writing gains')
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
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
c-----------------------------------------------------------------------
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
	call keya(keyw,string,' ')
	dowhile(nfeeds.lt.maxfeeds.and.more)
	  i = binsrcha(string,allfds,nallfds)
	  if(i.eq.0)call bug('f','Unrecognised feed mnemonic: '//string)
	  nfeeds = nfeeds + 1
	  feeds(nfeeds) = codes(i)
	  call keya(keyw,string,' ')
	  more = keyprsnt(keyw)
	enddo
c
	if(more)call bug('f','Too many feeds given')
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine EditBug(iostat,message)
c
	implicit none
	integer iostat
	character message*(*)
c
c  Give an error message, and bugger off.
c-----------------------------------------------------------------------
	call bug('w',message)
	call bugno('f',iostat)
	end
c********1*********2*********3*********4*********5*********6*********7*c
        subroutine GetOpt(dorep,domult,doflag,doamp,dophas,dorefl,dozm,
     *		doscal,dup,doinv)
c       
        implicit none
	logical dorep,domult,doflag,doamp,dophas,dorefl,dozm,doscal
	logical dup, doinv
c
c  Get the various processing options.
c
c-----------------------------------------------------------------------
        integer NOPTS
        parameter(NOPTS=10)
        character opts(NOPTS)*10
        logical present(NOPTS)
        data opts/'replace  ','multiply ','flag     ',
     *		  'amplitude','phase    ','reflect  ',
     *		  'zmean    ','scale    ','dup      ',
     *            'invert   '/
c
        call options('options',opts,present,NOPTS)
c
	domult = present(2)
	dorep  = present(1)
	doflag = present(3)
	doamp  = present(4)
	dophas = present(5)
	dorefl = present(6)
	dozm   = present(7)
	doscal = present(8)
	dup    = present(9)
        doinv  = present(10)
	if(.not.(domult.or.dorep.or.doflag.or.doamp.or.dophas.or.
     *	  dorefl.or.dozm.or.doscal.or.dup.or.doinv)) dorep = .true.
c
	end
