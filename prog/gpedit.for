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
c@ gain
c	Antenna gain to be used for the antennas affected. Given
c	as amplitude (sqrt(Jy/K), and phase (degrees). Default=1,0.
c@ ants
c	The antennas affected. The default is all antennas.
c@ feeds
c	The polarisation feeds affected (e.g. R, L, X or Y). Default is
c	all feeds.
c@ options
c       This gives extra processing options. Several values can be given
c       (though many values are mutually exclusive), separated by commas.
c       Option values can be abbreviated to uniqueness.
c       Possible options are:
c         multiply  The existing gains are multiplied by the complex gain.
c	            i.e. the amplitude is multiplied and the phase added.
c         replace   The existing gains are replaced by the complex gain
c		    This is the default option.
c         flag	    The existing gains are flagged as bad.
c
c	Example:
c	  gpedit vis=cyga gain=14.4,90 ants=1,2 feeds=X
c--
c  History:
c    rjs    4sep91 gpbreak. gpedit code copied from gpbreak.
c    mchw  23apr96 keyword driven task for the squeamish. Instead of BEE.
c-----------------------------------------------------------------------
	include 'maxdim.h'
        include 'mirconst.h'
	integer MAXFEED,MAXTIME,MAXSOLN
	character version*(*)
	parameter(version='Gpedit: version 1.0 23-APR-96')
	parameter(MAXFEED=2,MAXTIME=64,MAXSOLN=1024)
c
	character vis*64,oper*8
	integer iostat,tVis,itGain,nants,nfeeds,nsols,i
	integer numtime,numant,numfeed
	double precision btimes(MAXTIME),times(MAXSOLN)
	integer ants(MAXANT),feeds(MAXFEED)
	complex gains(2*MAXANT*MAXSOLN),gain
	real amp,phi
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
	call mkeyi('ants',ants,MAXANT,numant)
	call keyr('gain',amp,1.)
	call keyr('gain',phi,0.)
	call mkeyfd('feeds',feeds,MAXFEED,numfeed)
        call GetOpt(oper)
	call keyfin
	if(vis.eq.' ')call bug('f','No input vis data-set given')
c
c  Open the input file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call EditBug(iostat,'Error opening '//vis)
c
c  Determine the number of things in the gain table.
c
	call rdhdi(tVis,'ngains',nants,0)
	call rdhdi(tVis,'nfeeds',nfeeds,1)
	if(nfeeds.le.0.or.nfeeds.gt.2.or.nants.lt.nfeeds.or.
     *	    mod(nants,nfeeds).ne.0)
     *	  call bug('f','Bad number of gains or feeds in '//vis)
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
	if(iostat.ne.0)call EditBug(iostat,'Error accessing gains')
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
	gain = amp * cmplx(cos(phi*pi/180.),sin(phi*pi/180.))
	call GainEdt(nsols,nants*nfeeds,Gains,mask,gain,oper)
c
c  Write out the gains.
c
	call wrhdi(tVis,'nsols',nsols)
	call GainWr(itGain,nsols,nants,nfeeds,times,Gains)
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
	call hdaccess(itGain,iostat)
	call hclose(tVis)	
	end
c********1*********2*********3*********4*********5*********6*********7*c
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
c-----------------------------------------------------------------------
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
	subroutine GainEdt(nsols,nants,Gains,mask,gain,oper)
c
	implicit none
	integer nsols,nants
	complex Gains(nants,nsols),gain
	logical mask(nants)
	character*(*) oper
c
c  Edit the gains.
c
c  Input:
c    nsols	number of solutions.
c    nants	Number of antennae times the number of feeds.
c    mask	antenna/feed mask.
c    gain	a complex gain to be used.
c    oper	the operation to be performed.
c  Input/Output:
c    gains	The gains.
c-----------------------------------------------------------------------
	integer i,k
c
c  Do a replace operation.
c
        if(oper.eq.'replace')then
	  do i=1,nants
	    if(.not.mask(i))then
	      do k=1,nsols
	        gains(i,k) = gain
  	      enddo
	    endif
	  enddo
c
c  Multiply operation.
c
        else if(oper.eq.'multiply')then
	  do i=1,nants
	    if(.not.mask(i))then
	      do k=1,nsols
	        gains(i,k) = gains(i,k)*gain
  	      enddo
	    endif
	  enddo
c
c  Flag operation.
c
        else if(oper.eq.'flag')then
	  do i=1,nants
	    if(.not.mask(i))then
	      do k=1,nsols
	        gains(i,k) = (0.,0.)
  	      enddo
	    endif
	  enddo
	else
          call bug('f','Unrecognised operation, in GainEdt')
	endif
c
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
	dowhile(nfeeds.lt.maxfeeds.and.more)
	  i = binsrcha(string,allfds,nallfds)
	  if(i.eq.0)call bug('f','Unrecognised feed mnemonic: '//string)
	  nfeeds = nfeeds + 1
	  feeds(nfeeds) = codes(i)
	  more = keyprsnt(keyw)
	enddo
c
	if(more)call bug('f','Too many feeds given')
	end
c********1*********2*********3*********4*********5*********6*********7*c
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
c-----------------------------------------------------------------------
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
        subroutine GetOpt(oper)
c       
        implicit none
        character oper*(*)
c
c  Get the various processing options.
c
c  Output:
c    oper       One of 'replace',multiply','flag'.
c               The default is 'replace'.
c-----------------------------------------------------------------------
        integer i,j
        integer nopt
        parameter(nopt=3)
        character opts(nopt)*9
        logical present(nopt)
        data opts/    'replace  ','multiply ','flag     '/
        call options('options',opts,present,nopt)
c
        j = 0
        do i=1,3
          if(present(i))then
            if(j.ne.0)call bug('f',
     *          'Options '//opts(j)//' and '//opts(i)//
     *          ' are mutually exclusive')
            j = i
            oper = opts(j)
          endif
        enddo
        if(j.eq.0) then
	  call output ('Default options=replace')
          oper = 'replace'
	endif
c
	end
c********1*********2*********3*********4*********5*********6*********7*c
