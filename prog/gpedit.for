c********1*********2*********3*********4*********5*********6*********7*c
	program gpedit
	implicit none
c
c= Gpedit -- Edit the gain table.
c& mchw
c: calibration
c+
c	Gpedit is a MIRIAD task which modifies a gain table.
c@ vis
c	The input visibility file, containing the gain file to modify.
c	No default.
c@ select
c	Normal uv data selection commands. See the help on "select" for
c	more information. Currently only antenna and time selection is
c	supported. The default is to select all gains.
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
c	separated by commas. Option values can be abbreviated to uniqueness.
c	Possible options are:
c	  replace   The existing gains are replaced by the gain
c	            given by the `gain' keyword. NOTE: If no other
c	            options are given, the replace option is performed.
c	  multiply  The existing gains are multiplied by the gain
c	            given by the `gain' keyword.
c	  flag      The existing gains are flagged as bad.
c	  amplitude The phases of the existing gains are set to 0.
c	  phase     The amplitudes of the existing gains are set 1.
c
c	Example:
c	  gpedit vis=cyga gain=14.4,90 select=ant(1,2) feeds=X
c--
c  History:
c    rjs    4sep91 gpbreak. gpedit code copied from gpbreak.
c    mchw  23apr96 keyword driven task for the squeamish. Instead of BEE.
c    rjs   25feb97 Tidy up and extend the possibilities.
c    rjs   26feb97 Fix feeds reading.
c-----------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
        include 'mirconst.h'
	integer MAXFEED,MAXSELS
	character version*(*)
	parameter(version='Gpedit: version 1.0 26-Feb-97')
	parameter(MAXFEED=2,MAXSELS=300)
c
	character vis*64
	logical domult,dorep,doflag,doamp,dophas
	integer iostat,tVis,itGain,nants,nfeeds,nsols,ntau,i
	integer numfeed,feeds(MAXFEED)
	complex gain
	real amp,phi,sels(MAXSELS)
	logical mask(2*MAXANT)
	integer pGains,pTimes
c
c  Externals.
c
	external MultOp,RepOp,FlagOp,AmpOp,PhasOp
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','No input vis data-set given')
	call SelInput('select',sels,MAXSELS)
	call keyr('gain',amp,1.)
	call keyr('gain',phi,0.)
	call mkeyfd('feeds',feeds,MAXFEED,numfeed)
        call GetOpt(dorep,domult,doflag,doamp,dophas)
	call keyfin
c
c  Open the input file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call EditBug(iostat,'Error opening '//vis)
c
c  Determine the number of things in the gain table.
c
	call rdhdi(tVis,'ntau',ntau,0)
	if(ntau.ne.0)call bug('f',
     *	  'GPEDIT cannot copy with a gain table with delays')
	call rdhdi(tVis,'ngains',nants,0)
	call rdhdi(tVis,'nfeeds',nfeeds,1)
	if(nfeeds.le.0.or.nfeeds.gt.2.or.nants.lt.nfeeds.or.
     *	    mod(nants,nfeeds).ne.0)
     *	  call bug('f','Bad number of gains or feeds in '//vis)
	nants = nants / nfeeds
	call rdhdi(tVis,'nsols',nsols,0)
c
c  See if we have enough space.
c
	if(nants.gt.MAXANT)
     *	  call bug('f','Too many antennae for me to cope with')
	if(nsols.le.0)
     *	  call bug('f','Bad number of solutions')
	call memAlloc(pGains,nsols*nants*nfeeds,'c')
	call memAlloc(pTimes,nsols,'d')
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
	call SetMask(nfeeds,nants,mask,feeds,numfeed,sels)
c
c  Open the gains file. Mode=='append' so that we can overwrite it.
c
	call haccess(tVis,itGain,'gains','append',iostat)
	if(iostat.ne.0)call EditBug(iostat,'Error accessing gains')
c
c  Read the gains.
c
	call GainRd(itGain,nsols,nants,nfeeds,
     *				memd(pTimes),memc(pGains))
c
c  Edit the gains.
c
	gain = amp * cmplx(cos(phi*pi/180.),sin(phi*pi/180.))
	if(dorep) call GainEdt(nsols,nants*nfeeds,
     *	  memd(pTimes),memc(pGains),mask,sels,gain,RepOp)
	if(domult)call GainEdt(nsols,nants*nfeeds,
     *	  memd(pTimes),memc(pGains),mask,sels,gain,MultOp)
	if(doflag)call GainEdt(nsols,nants*nfeeds,
     *	  memd(pTimes),memc(pGains),mask,sels,gain,FlagOp)
	if(doamp )call GainEdt(nsols,nants*nfeeds,
     *	  memd(pTimes),memc(pGains),mask,sels,gain,AmpOp)
	if(dophas)call GainEdt(nsols,nants*nfeeds,
     *	  memd(pTimes),memc(pGains),mask,sels,gain,PhasOp)
c
c  Write out the gains.
c
	call GainWr(itGain,nsols,nants,nfeeds,
     *				memd(pTimes),memc(pGains))
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
	call memFree(pTimes,nsols,'d')
	call memFree(pGains,nfeeds*nants*nsols,'c')
	call hdaccess(itGain,iostat)
	call hclose(tVis)	
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
	subroutine GainEdt(nsols,nants,times,Gains,mask,sels,
     *							gain,oper)
c
	implicit none
	integer nsols,nants
	double precision times(nsols)
	complex Gains(nants,nsols),gain
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
	integer i,j
c
c  Externals.
c
	logical SelProbe
c
	do j=1,nsols
	  if(SelProbe(sels,'time',times(j)))then
	    do i=1,nants
	      if(mask(i))call oper(Gains(i,j),gain)
	    enddo
	  endif
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
c********1*********2*********3*********4*********5*********6*********7*c
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
c-----------------------------------------------------------------------
	integer offset,iostat,k
c
	offset = 8
	do k=1,nsols
	  call hwrited(itGain,times(k),offset,8,iostat)
	  if(iostat.ne.0)call EditBug(iostat,'Error writing gain time')
	  offset = offset + 8
	  call hwriter(itGain,Gains(1,k),offset,8*nfeeds*nants,iostat)
	  if(iostat.ne.0)call EditBug(iostat,'Error writing gains')
	  offset = offset + 8*nfeeds*nants
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
        subroutine GetOpt(dorep,domult,doflag,doamp,dophas)
c       
        implicit none
	logical dorep,domult,doflag,doamp,dophas
c
c  Get the various processing options.
c
c-----------------------------------------------------------------------
        integer NOPTS
        parameter(NOPTS=5)
        character opts(NOPTS)*9
        logical present(NOPTS)
        data opts/'replace  ','multiply ','flag     ',
     *		  'amplitude','phase    '/
c
        call options('options',opts,present,NOPTS)
c
	domult = present(2)
	dorep  = present(1)
	doflag = present(3)
	doamp  = present(4)
	dophas = present(5)
	if(.not.(domult.or.dorep.or.doflag.or.doamp.or.dophas))
     *	  dorep = .true.
c
	end
