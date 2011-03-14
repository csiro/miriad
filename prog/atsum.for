c************************************************************************
	program atsum
	implicit none
c& rjs
c: data transfer
c+
c  atsum is a task to summarise the amount of good and bad data
c  in an RPFITS file.
c
c@ in
c	Name of the input RPFITS files. Only a single file can be given.
c@ ignore
c	Antennas to ignore.
c--
c
c  History:
c    rjs  25may04 Derived from atlod.
c------------------------------------------------------------------------
	include 'atsum.h'
	character in*64
	integer iostat,ignore(ATANT),nig,i
	character line*64
c
c  Externals.
c
	character rperr*32
c
c  Get the input parameters.
c
	call keyini
	call keya('in',in,' ')
	call mkeyi('ignore',ignore,ATANT,nig)
	do i=1,ATANT
	  doant(i) = .true.
	enddo
	do i=1,nig
	  doant(ignore(i)) = .false.
	enddo
	call keyfin
c
	call RPDisp(in,iostat)
	if(iostat.ne.0)then
	  line = 'RPFITS i/o error: '//rperr(iostat)
	  call bug('w',line)
	  call bug('w','Prematurely finishing because of errors')
	endif
c
	end
c************************************************************************
	subroutine Poke1st(time)
c
	implicit none
	double precision time
c------------------------------------------------------------------------
	integer i
c
	include 'atsum.h'
c
	ngaps = 0
	tstart = time
	do i=1,ATANT
	  times(i) = time
	  timesys(i) = 0
	enddo
c
	do i=1,nbands
	  band(i) = 0
	enddo
c
	end
c************************************************************************
	subroutine PokeLast(time)
c
	implicit none
	double precision time
c------------------------------------------------------------------------
	include 'atsum.h'
	integer i,aflag,id
	double precision ts,te
	integer summer(ATANT)
	integer idx(MAXGAPS)
	character descr*16
c
c  Externals.
c
	character mydate*16,line*128,bands*7
	integer l
c
c  Externals.
c
	integer len1
	character itoaf*8
c
	save summer
	data summer/100000,20000,3000,400,50,6/
c
	tend = time
c
	do i=1,ATANT
	  call PokeData(time,i,i,0.0,0.0d0)
	enddo
c
	call output('Start time: '//mydate(tstart))
	call output('End time: '//mydate(tend))
c
c  Write out band statistics.
c
	bands = 'LSCXKQW'
	l = 0
	do i=1,nbands
	  if(band(i).gt.0)then
	    if(l.ne.0)then
	      line(l+1:l+1) = ';'
	      l = l + 1
	    endif
	    line(l+1:) = bands(i:i)//'='//itoaf(band(i))
	    l = len1(line)
	  endif
	enddo
	call output('Bands: '//line(1:l))
c
	call sortidxd(ngaps,tgaps,idx)
c
	if(ngaps.gt.0)then
	  ts = tgaps(idx(1))
	  te = tgape(idx(1))
	  aflag = summer(agap(idx(1)))
	  descr = dgap(idx(1))
	  do i=2,ngaps
	    id = idx(i)
	    if(abs(tgaps(id)-ts).le.120.0/86400.0.and.
     *	       abs(tgape(id)-te).le.120.0/86400.0.and.
     *	       dgap(id).eq.descr)then
	      aflag = aflag + summer(agap(id))
	      ts = min(tgaps(id),ts)
	      te = max(tgape(id),te)
	    else
	      call flushgap(ts,te,aflag,descr)
	      ts = tgaps(id)
	      te = tgape(id)
	      aflag = summer(agap(id))
	      descr = dgap(id)
	    endif
	  enddo
	  call flushgap(ts,te,aflag,descr)
	endif
c
c  Geenrate an "EVENT" message is there are antennas ignored.
c
	aflag = 0
	do i=1,ATANT
	  if(.not.doant(i))aflag = aflag + summer(i)
	enddo
	if(aflag.gt.0)call flushgap(tstart,tend,aflag,
     *		'EVENT: Antennas ignored')
c
	end
c************************************************************************
	subroutine flushgap(ts,te,aflag,descr)
c
	implicit none
	double precision ts,te
	integer aflag
	character descr*(*)
c------------------------------------------------------------------------
	character flags*6,fdash*6,line*80
	integer i,j,l
c
c  Externals.
c
	character mydate*16
	integer len1
c
	write(flags,'(i6.6)')aflag
c
	j = 0
	do i=1,6
	  if(flags(i:i).ne.'0')then
	    j = j + 1
	    fdash(j:j) = flags(i:i)
	  endif
	enddo
	l = len1(descr)
	line = descr(1:l)//': Range='//mydate(ts)//','//
     *                 mydate(te)//'; Antennas='//fdash(1:j)
	call output(line)
	end
c************************************************************************
	character*16 function mydate(time)
c
	implicit none
	double precision time
c------------------------------------------------------------------------
	character datobs*32
	call julday(time,'t',datobs)
	datobs(11:11) = ' '
	datobs(17:) = ' '
	mydate = datobs
c
	end
c************************************************************************
	subroutine PokeData(time,i1,i2,tsys,freq)
c
	implicit none
	integer i1,i2
	double precision time,freq
	real tsys
c
c------------------------------------------------------------------------
	include 'atsum.h'
	integer ant6
	parameter(ant6=6)
c
	if(freq.lt.1e9)then
	  continue
	else if(freq.lt.2e9)then
	  band(1) = band(1) + 1
	else if(freq.lt.3e9)then
	  band(2) = band(2) + 1
	else if(freq.lt.7e9)then
	  band(3) = band(3) + 1
	else if(freq.lt.10e9)then
	  band(4) = band(4) + 1
	else if(freq.lt.26e9)then
	  band(5) = band(5) + 1
	else if(freq.lt.50e9)then
	  band(6) = band(6) + 1
	else if(freq.lt.120e9)then
	  band(7) = band(7) + 1
	endif
c
	if(tsys.lt.1000.or.time-times(i1).gt.THRESH)then
	  if(timesys(i1).gt.0.and.times(i1)-timesys(i1).gt.THRESH)
     *            call addgap(i1,timesys(i1),times(i1),'Event: HiTsys')
	  timesys(i1) = 0
	endif
	if(time-times(i1).gt.THRESH)call addgap(i1,times(i1),time,'Gap')
	if(tsys.ge.1000.and.timesys(i1).le.0)timesys(i1) = time
	times(i1) = time
c
	if(tsys.lt.1000.or.time-times(i2).gt.THRESH)then
	  if(timesys(i2).gt.0.and.times(i2)-timesys(i2).gt.THRESH)
     *            call addgap(i2,timesys(i2),times(i2),'Event: HiTsys')
	  timesys(i2) = 0
	endif
	if(time-times(i2).gt.THRESH)call addgap(i2,times(i2),time,'Gap')
	if(tsys.ge.1000.and.timesys(i2).le.0)timesys(i2) = time
	times(i2) = time
c
c  If its a 3mm observation, pretend ca06 was observed as well.
c
	if(freq.gt.75e9)then
	  if(time-times(ant6).gt.THRESH)then
	    if(timesys(ant6).gt.0.and.
     *	      times(ant6)-timesys(ant6).gt.THRESH)
     *       call addgap(ant6,timesys(ant6),times(ant6),'Event: HiTsys')
	    timesys(ant6) = 0
	  endif
	  if(time-times(ant6).gt.THRESH)
     *		call addgap(ant6,times(ant6),time,'Gap')
	  times(ant6) = time
	endif
c
	end
c************************************************************************
	subroutine addgap(i,t1,t2,descr)
c
	implicit none
	integer i
	double precision t1,t2
	character descr*(*)
c------------------------------------------------------------------------
	include 'atsum.h'
	if(doant(i))then
	  ngaps = ngaps + 1
	  if(ngaps.gt.MAXGAPS)call bug('f','Gaps array overflow')
	  agap(ngaps) = i
	  tgaps(ngaps) = t1
	  tgape(ngaps) = t2
	  dgap(ngaps) = descr
	endif
	end
c************************************************************************
	subroutine RPDisp(in,iostat)
c
	implicit none
	character in*(*)
	integer iostat
c
c  Process an RPFITS file. Dispatch information to the
c  PokeData routine.
c
c------------------------------------------------------------------------
	logical relax
	parameter(relax=.false.)
	include 'maxdim.h'
	include 'mirconst.h'
	integer MAXPOL,MAXSIM,MAXXYP
	parameter(MAXPOL=4,MAXSIM=4,MAXXYP=5)
	include 'rpfits.inc'
	integer i1,i2,baseln,i,j
	character tfudge*32,tmp*2
	logical NewScan,NewSrc,NewFreq,NewTime,ok,badbit
	logical corrfud,wband,first,Accum,cabb
	integer jstat,flag,bin,ifno,srcno,ssrcno,simno,ssimno
	integer If2Sim(MAX_IF),nifs(MAX_IF),Sim2If(MAXSIM,MAX_IF)
	integer Sif(MAX_IF)
	real ut,utprev,utprevsc,u,v,w,weight(MAXCHAN*MAXPOL)
	complex vis(MAXCHAN*MAXPOL)
c
c  Variables to track the sysc records.
c
	logical scinit(MAX_IF,ANT_MAX),scbuf(MAX_IF,ANT_MAX)
	logical xflag(MAX_IF,ANT_MAX),yflag(MAX_IF,ANT_MAX)
	integer ptag(MAXXYP,MAX_IF,ANT_MAX)
	integer atag(MAXXYP,MAX_IF,ANT_MAX)
	integer nxyp(MAX_IF,ANT_MAX)
	real xyp(MAXXYP,MAX_IF,ANT_MAX),xya(MAXXYP,MAX_IF,ANT_MAX)
	real xyphase(MAX_IF,ANT_MAX),xyamp(MAX_IF,ANT_MAX)
	real pntrms(ANT_MAX),pntmax(ANT_MAX)
	real xsamp(3,MAX_IF,ANT_MAX),ysamp(3,MAX_IF,ANT_MAX)
	real xtsys(MAX_IF,ANT_MAX),ytsys(MAX_IF,ANT_MAX),tsys
c
	logical antvalid(ANT_MAX)
	double precision jday0,time,tprev
c
c  Externals.
c
	integer len1
c
c  Initial preparation.
c
	i2 = len1(in)
	i1 = i2
	do i=i2,1,-1
	  if(in(i1:i1).eq.'/')goto 10
	  i1 = i1 - 1
	enddo
  10	continue
	i1 = i1 + 1
	tfudge = ' '
	j = 0
	do i=i1,i2
	  if(in(i:i).gt.' '.and.in(i:i).le.'z')then
	    j = j + 1
	    tfudge(j:j) = in(i:i)
	  endif
	enddo
	call output('File: '//tfudge)
	first = .true.
c
c  Open the RPFITS file.
c
	call RPOpen(in,iostat)
	if(iostat.ne.0)goto 20
c
c  Initialise.
c
	do j=1,ANT_MAX
	  do i=1,MAX_IF
	    nxyp(i,j) = 0
	  enddo
	enddo
	call screset(scinit,xflag,yflag,MAX_IF,ANT_MAX)
c
	wband = .false.
        cabb = instrument(1:6).eq.'ATCABB' 
c
c  Initialise flagging information.
c
	utprev   = -1
	utprevsc = -1
	Accum = .false.
	NewScan = .true.
	corrfud=.false.
        Ssrcno = 0
        Ssimno = 0
	tprev = 0
c
c  Loop the loop getting data.
c
	jstat = 0
	badbit = .false.
	dowhile(jstat.eq.0)
	  call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	  if(jstat.ne.5)badbit = .false.
c
c  Handle header encountered.  Note that the next header
c  read will be the same one we just encountered, not the
c  next one
c
	  if(jstat.eq.1)then
	    jstat = -1
	    call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	    NewScan = .true.
c
c  Flush the number of buffered samples.
c
	    do j=1,ANT_MAX
	      do i=1,MAX_IF
	        nxyp(i,j) = 0
	      enddo
	    enddo
c
c  Handle end-of-scan 
c
	  else if(jstat.eq.2)then
	    jstat = 0
c
c  Handle FG table encountered (ignore it)
c
          else if(jstat.eq.4)then
            jstat = 0
c
c  Handle some i/o error. First time tolerate it, but if it happens
c  immediately again, skip to the next header.
c
	  else if(jstat.eq.5)then
	    if(badbit)then
c
c  I/O error occurred with jstat=5. Look for next header
c
	      jstat = -1
	      call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	      NewScan = .true.
	    else
	      badbit = .true.
	      jstat = 0
	    endif
c
c  Other errors, including EOF.
c
          else if(jstat.ne.0)then
	    continue
c
c  Handle a SYSCAL record. If it appears to belong to this integration,
c  send it through to the Poke routines right away. Otherwise, end the
c  integration and buffer up the SYSCAL record for later delivery.
c
	  else if(baseln.eq.-1)then
	    NewTime = abs(sc_ut-utprevsc).gt.0.04
	    if(NewScan.or.an_found.or.NewTime)then
	      call screset(scinit,xflag,yflag,MAX_IF,ANT_MAX)
	      Accum = .false.
	      utprevsc = sc_ut
	    endif
	    call SetSC(scinit,scbuf,MAX_IF,ANT_MAX,sc_q,sc_if,sc_ant,
     *		sc_cal,if_invert,.false.,
     *		xyphase,xyamp,xtsys,ytsys,xsamp,ysamp,
     *		pntrms,pntmax,
     *		nxyp,xyp,ptag,xya,atag,MAXXYP,xflag,yflag,
     *          wband,cabb)
c
c  Data record. Check whether we want to accept it.
c  If OK, and we have a new scan, calculate the new scan info.
c
	  else if(ifno.lt.1.or.ifno.gt.n_if.or.srcno.lt.1)then
	    continue
	  else
	    if(NewScan)then
	      call dayjul(datobs,jday0)
	      time = ut / (3600.d0*24.d0) + jday0
c
c  Now we cludge a correlator fix:
c  It can happen that the datobs is written just before the 
c  change of a ut day, but the ut is written after the change of
c  ut day. What the correlator should do is write the ut as ut+8640,
c  but as of 10-04-01 it didn't. 
c  So, in this case, jday0 should be incremented here.
c  We assume no-one is going to average more than 30 sec...
c
	      if ((ut .lt. 30) .and. (86400*(time-tprev).lt.-1)) then
		jday0=jday0+1
		time = ut / (3600.d0*24.d0) + jday0
		corrfud=.true.
	      endif
	      call SimMap(if_num,n_if,if_simul,if_chain,
     *		  If2Sim,nifs,Sim2If,Sif,MAXSIM)
	      call ChkAnt(x,y,z,antvalid,nant)
	    endif
c
	    time = ut / (3600.d0*24.d0) + jday0
	    if(first)call Poke1st(time)
	    first = .false.
c
c  Determine whether to flush the buffers.
c
	    simno = If2Sim(ifno)
            NewFreq = simno.ne.Ssimno
            NewTime = abs(ut-utprev).gt.0.04
            NewSrc = srcno.ne.Ssrcno
            if(Accum.and.(NewScan.or.an_found.or.NewSrc.or.NewFreq.or.
     *                                                  NewTime))then
	      call screset(scinit,xflag,yflag,MAX_IF,ANT_MAX)
	      Accum = .false.
            endif
c
	    i1 = baseln/256
	    i2 = mod(baseln,256)
	    ok = ifno.ge.1.and.ifno.le.n_if.and.
     *	         min(i1,i2).ge.1.and.max(i1,i2).le.nant.and.
     *	         bin.ge.0
            if(ok)ok = If2Sim(ifno).gt.0
	    if(ok) then
	      if(.not.(antvalid(i1).and.antvalid(i2)))then
		flag = 1
	      else if(.not.(scinit(ifno,i1).and.scinit(ifno,i2)))then
		flag = 1
	      endif
	      if(ok) ok = flag.eq.0
	    endif
c
c  Determine the flags for each polarisation based on the sampler
c  statistics if the samplers have been initialised.
c
	    if(ok)ok = min(i1,i2).gt.0.and.max(i1,i2).le.ANT_MAX
	    if(ok)then
	      call GetFg(if_nstok(ifno),if_cstok(1,ifno),flag,
     *		xflag(ifno,i1).or.relax,yflag(ifno,i1).or.relax,
     *		xflag(ifno,i2).or.relax,yflag(ifno,i2).or.relax,
     *		xtsys(ifno,i1),ytsys(ifno,i1),
     *		xtsys(ifno,i2),ytsys(ifno,i2),tsys,
     *		ok)
c
c  Send the data record to the Poke routines.
c
	      if(ok)call PokeData(time,i1,i2,tsys,if_freq(ifno))
c
c  Reinitialise things.
c
	      if (86400*(time-tprev).lt.-1) then
		if (.not. corrfud) then
                  call bug('w',
     *		   'Data are out of time order')
		else
		  corrfud = .false.
		endif
	      endif
	      tprev = time
	      utprev = ut
	      Accum = .true.
              Ssrcno = srcno
              Ssimno = simno
 	      NewScan = .false.
	      an_found = .false.
	    endif
	  endif
	enddo
c
c  We are done. Close up, and return the error code.
c
  20	if(first)then
	  tfudge(11:11) = ' '
	  tmp = tfudge(14:15)
	  tfudge(14:14) = ':'
	  tfudge(14:) = ':'//tmp
	  call output('Start time: '//tfudge)
	  call output('End time: '//tfudge)
	else
	  call PokeLast(time)
	endif
	call RPClose(iostat)
	if(iostat.eq.0.and.jstat.ne.3)iostat = jstat
c
	end
c************************************************************************
	subroutine screset(scinit,xflag,yflag,nx,ny)
c
	implicit none
	integer nx,ny
	logical scinit(nx,ny),xflag(nx,ny),yflag(nx,ny)
c------------------------------------------------------------------------
	integer i,j
c
	do j=1,ny
	  do i=1,nx
	    scinit(i,j) = .false.
	    xflag(i,j) = .false.
	    yflag(i,j) = .false.
	  enddo
	enddo
c
	end
c************************************************************************
	character*(*) function RPErr(jstat)
c
	implicit none
	integer jstat
c
c  Translate an RPFITSIN jstat value into something a bit more
c  meaningful.
c------------------------------------------------------------------------
	character itoaf*8
c
	integer NMESS
	parameter(NMESS=7)
	character mess(NMESS)*32
	data mess/'Operation unsuccessful          ',
     *		  'Operation successful            ',
     *            'Encountered header while reading',
     *		  'Probably OK ... End of scan     ',
     *		  'Encountered end-of-file	   ',
     *		  'Encountered FG table            ',
     *		  'Illegal parameter encountered   '/
c
	if(jstat.ge.-1.and.jstat.le.5)then
	  rperr = mess(jstat+2)
	else
	  rperr = 'RPFITS error: jstat='//itoaf(jstat)
	endif
c
	end
c************************************************************************
	subroutine RPClose(jstat)
c
	implicit none
	integer jstat
c------------------------------------------------------------------------
	integer flag,baseln,bin,ifno,srcno
	real ut,u,v,w,weight
	complex vis
c
	character rperr*32
c
	jstat = 1
	call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	if(jstat.ne.0)call bug('w',
     *		'Error closing file: '//rperr(jstat))
	end
c************************************************************************
	subroutine RPOpen(in,jstat)
c
	implicit none
	character in*(*)
	integer jstat
c
c  Open the RPFITS file.
c------------------------------------------------------------------------
	include 'rpfits.inc'
c
	integer flag,baseln,bin,ifno,srcno
	real ut,u,v,w,weight
	complex vis
c
c  External.
c
	character rperr*32
c
	file = in
c
	jstat = -3
	an_found = .false.
	call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	if(jstat.ne.0) call bug('w',
     *	    'Error opening RPFITS file: '//rperr(jstat))
	if(jstat.ne.0)return
c
c  Read the header.
c
	ncard = 20
	card(1) = 'FORMAT'
	jstat = -1
	call rpfitsin(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	end
c************************************************************************
	subroutine ChkAnt(x,y,z,antvalid,nant)
c
	implicit none
	integer nant
	double precision x(nant),y(nant),z(nant)
	logical antvalid(nant)
c
c  Check for a valid antenna position.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nant
	  antvalid(i) = (abs(x(i)) + abs(y(i)) + abs(z(i)).gt.0)
	enddo
c
	end
c************************************************************************
	subroutine GetFg(nstok,cstok,flag,xflag1,yflag1,xflag2,yflag2,
     *		xtsys1,ytsys1,xtsys2,ytsys2,tsys,polflag)
c
	implicit none
	integer nstok,flag
	character cstok(nstok)*(*)
	logical polflag,xflag1,yflag1,xflag2,yflag2
	real xtsys1,ytsys1,xtsys2,ytsys2,tsys
c
c  Flag a polarisation either if "flag" indicates that the entire record
c  is bad, or if the syscal-based flags are bad.
c------------------------------------------------------------------------
	integer p

c
	polflag = .false.
	if(flag.eq.0)then
	  do p=1,nstok
	    if(cstok(p).eq.'XX')then
	      polflag = polflag.or.(xflag1.and.xflag2)
	      tsys = sqrt(xtsys1*xtsys2)
	    else if(cstok(p).eq.'YY')then
	      polflag = polflag.or.(yflag1.and.yflag2)
	      tsys = sqrt(ytsys1*ytsys2)
	    else if(cstok(p).eq.'XY')then
	      polflag = polflag.or.(xflag1.and.yflag2)
	      tsys = sqrt(xtsys1*ytsys2)
	    else if(cstok(p).eq.'YX')then
	      polflag = polflag.or.(yflag1.and.xflag2)
	      tsys = sqrt(ytsys1*xtsys2)
	    else
	      call bug('f','Unrecognised polarisation type, in GetFg')
	    endif
	  enddo
	endif
c
	end
c************************************************************************
	subroutine syscflag(polflag,xsamp,ysamp,xyphase,xyamp,
     *		nxyp,maxxyp,xyp,ptag,xya,atag,xflag,yflag,mmrelax,cabb)
c
	implicit none
	real xsamp(3),ysamp(3),xyphase,xyamp
	integer nxyp,maxxyp,ptag(maxxyp),atag(maxxyp)
	real xyp(maxxyp),xya(maxxyp)
	logical polflag,xflag,yflag,mmrelax,cabb
c
c  Determine data flags based on the values of syscal statistics.
c
c  The data will be flagged bad if:
c    * The sampler stats deviate by 3% from 17.3%, or 0.5% from 50.0%
c    * There is a 10 degree change in the xyphase relative to the median
c      of the "nxyp" values.
c    * There is a 1 Jy or 10% change in the xyamp relative to the median
c      of the "nxyp" values.
c
c  Input:
c    xsamp	The x sampler statistics (percent).
c    ysamp	The y sampler statistics (percent).
c    xyphase	The online xyphase measurement (radians).
c    xyamp	XY amplitude, in (pseudo)Jy.
c    maxxyp	The max number of xy phase 
c  Input/Output:
c    nxyp	Number of buffered xyphase measurements.
c    tag	Tags for xyphase measurements. The oldest xyphase measurement
c		has the smallest tag value.
c    xyp	Buffered xyphase measurements. These are always
c		sorted into ascending order. In radians.
c    xya	Buffered xyamp measurements. These are always
c		sorted into ascending order. In (pseudo)Jy.
c
c  Output:
c    xflag	Flag for the X channel.
c    yflag	Flag for the y channel.
c------------------------------------------------------------------------
	include 'mirconst.h'
	real mxyp,mxya
	integer ntemp
c
	ntemp = nxyp
	call MedMerge(nxyp,maxxyp,xyphase,xyp,ptag,mxyp)
	call MedMerge(ntemp,maxxyp,xyamp,xya,atag,mxya)
c
c  Flag both x and y as bad if there is a glitch in the xy phase.
c  Otherwise flag according to the goodness of the sampler stats.
c
	if((abs(xyamp-mxya).gt.max(1.0,0.1*mxya).or.
     *	   abs(xyphase-mxyp).gt.10.*pi/180.).and..not.mmrelax)then
	  xflag = .false.
	  yflag = .false.
	else if (.not.cabb) then
	  xflag = abs(xsamp(2)-50.0).lt.0.5 .and.
     *		  abs(xsamp(1)-17.3).lt.3.0 .and.
     *		  abs(xsamp(3)-17.3).lt.3.0
	  yflag = abs(ysamp(2)-50.0).lt.0.5 .and.
     *		  abs(ysamp(1)-17.3).lt.3.0 .and.
     *		  abs(ysamp(3)-17.3).lt.3.0
	  if(polflag)then
	    xflag = xflag.and.yflag
	    yflag = xflag
	  endif
        else
          xflag=.true.
          yflag=.true.
	endif
c
	end
c************************************************************************
	subroutine MedMerge(nxyp,maxxyp,xyphase,xyp,tag,mxyp)
c
	implicit none
	integer nxyp,maxxyp,tag(maxxyp)
	real xyphase,xyp(maxxyp),mxyp
c------------------------------------------------------------------------
	integer tmin,tmax,i,nxyp2
	logical more
c
c  Find the xyphase with the biggest and smallest tags.
c
	if(nxyp.gt.0)then
	  tmax = 1
	  tmin = 1
	  do i=2,nxyp
	    if(tag(i).gt.tag(tmax))tmax = i
	    if(tag(i).lt.tag(tmin))tmin = i
	  enddo
	  tmax = tag(tmax) + 1
	else
	  tmax = 1
	endif
c
c  If the buffer is full, discard the xyphase with the minimum tag,
c  by squeezing it out.
c
	if(nxyp.eq.maxxyp)then
	  do i=tmin+1,nxyp
	    xyp(i-1) = xyp(i)
	    tag(i-1) = tag(i)
	  enddo
	else
	  nxyp = nxyp + 1
	endif
c
c  Merge in the new xyphase.
c
	i = nxyp
	more = i.gt.1
	dowhile(more)
	  more = xyp(i-1).gt.xyphase
	  if(more)then
	    xyp(i) = xyp(i-1)
	    tag(i) = tag(i-1)
	    i = i - 1
	    more = i.gt.1
	  endif
	enddo
	xyp(i) = xyphase
	tag(i) = tmax
c
c  Determine the median xyphase
c
	nxyp2 = nxyp/2
	if(2*nxyp2.ne.nxyp)then
	  mxyp = xyp(nxyp2+1)
	else
	  mxyp = 0.5*(xyp(nxyp2)+xyp(nxyp2+1))
	endif
c
	end
c************************************************************************
	subroutine SetSC(scinit,scbuf,MAXIF,MAXANT,nq,nif,nant,
     *		syscal,invert,polflag,
     *		xyphase,xyamp,xtsys,ytsys,xsamp,ysamp,
     *		pntrms,pntmax,nxyp,xyp,ptag,xya,atag,MAXXYP,
     *		xflag,yflag,mmrelax,cabb)
c
	implicit none
	integer MAXIF,MAXANT,MAXXYP,nq,nif,nant,invert(MAXIF)
	real syscal(nq,nif,nant)
	logical polflag
	logical scinit(MAXIF,MAXANT),scbuf(MAXIF,MAXANT)
	real xyphase(MAXIF,MAXANT),xyamp(MAXIF,MAXANT)
	real xtsys(MAXIF,MAXANT),ytsys(MAXIF,MAXANT)
	real xsamp(3,MAXIF,MAXANT),ysamp(3,MAXIF,MAXANT)
	real pntrms(MAXANT),pntmax(MAXANT)
	real xyp(MAXXYP,MAXIF,MAXANT),xya(MAXXYP,MAXIF,MAXANT)
	integer ptag(MAXXYP,MAXIF,MAXANT)
	integer atag(MAXXYP,MAXIF,MAXANT)
	integer nxyp(MAXIF,MAXANT)
	logical xflag(MAXIF,MAXANT),yflag(MAXIF,MAXANT),mmrelax,cabb
c
c  Copy across SYSCAL records. Do any necessary fiddles on the way.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer j,k,ij,ik
	logical ok
c
	do k=1,nant
	  do j=1,nif
	    ik = nint(syscal(1,j,k))
	    ij = nint(syscal(2,j,k))
	    ok = ij.gt.0.and.ik.gt.0.and.ij.le.maxif.and.ik.le.maxant
	    if(ok.and.nq.ge.13) ok = syscal(13,j,k).eq.0
	    if(ok)then
	      scinit(ij,ik) = .true.
	      scbuf(ij,ik)  = .true.
	      xyphase(ij,ik) = invert(ij)*syscal(3,j,k)
	      xyamp(ij,ik) = 0
	      if(nq.ge.14)xyamp(ij,ik) = syscal(14,j,k)
	      pntrms(ik) = 0
	      if(nq.ge.16)pntrms(ik) = syscal(16,j,k)
	      pntmax(ik) = 0
	      if(nq.ge.15)pntmax(ik) = syscal(15,j,k)
	      xtsys(ij,ik) = abs(0.1* syscal(4,j,k) * syscal(4,j,k))
	      ytsys(ij,ik) = abs(0.1* syscal(5,j,k) * syscal(5,j,k))
	      xsamp(1,ij,ik) = syscal(6,j,k)
	      xsamp(2,ij,ik) = syscal(7,j,k)
	      xsamp(3,ij,ik) = syscal(8,j,k)
	      ysamp(1,ij,ik) = syscal(9,j,k)
	      ysamp(2,ij,ik) = syscal(10,j,k)
	      ysamp(3,ij,ik) = syscal(11,j,k)
	      call syscflag(polflag,xsamp(1,ij,ik),ysamp(1,ij,ik),
     *		xyphase(ij,ik),xyamp(ij,ik),nxyp(ij,ik),maxxyp,
     *		xyp(1,ij,ik),ptag(1,ij,ik),xya(1,ij,ik),atag(1,ij,ik),
     *		xflag(ij,ik),yflag(ij,ik),mmrelax,cabb)
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine SimMap(ifnum,nif,ifsimul,ifchain,
     *		  If2Sim,nifs,Sim2If,Sif,MAXSIM)
c
	implicit none
	integer nif,ifnum(nif),ifsimul(nif),ifchain(nif),MAXSIM
	integer If2Sim(nif),nifs(nif),Sim2IF(MAXSIM,nif),Sif(nif)
c
c  Using the RPFITS IF table, determine a map between RPFITS "ifno",
c  to a simultaneous group number. Then determine a map between the
c  simultaneous group number and the RPFITS "ifno" number.
c
c  What the &%^$&^%&^ is the RPFITS entry "IF_NUM" used for? Is it
c  an extra level of indirection in the the IF table or what? Avoid
c  attempting to understand this (no one else does). Just make sure
c  that IF_NUM(i).eq.i, which means that IF_NUM must be redundant and
c  irrelevant.
c
c  Input:
c    nif	Total number of entries in the RPFITS IF table.
c    ifnum	RPFITS IF_NUM column. Just check that IF_NUM(i)==i.
c    ifsimul,ifchain RPFITS columns.
c    MAXSIM	Maximum number of simultaneous frequencies.
c  Output:
c    If2Sim	Map from ifno to "simultaneous group number".
c    Sim2If	Map from "simultaneous group number" to "ifno". There can
c		be up to MAXSIM entries per "sim. group no.".
c    nifs	Number of simultaneous IFs in each sim. group.
c    Sif	Maps from RPFITS ifno to the position on the Miriad IF axis.
c------------------------------------------------------------------------
	integer i,j,nsimgrp,s
	logical more
c
	do i=1,nif
	  if(ifnum(i).ne.i)call bug('f',
     *		'IF_NUM(i).ne.i ... I do not understand')
	enddo
c
c  Assign a simultaneous IF to each of them.
c
	nsimgrp = 0
	do i=1,nif
	  If2Sim(i) = 0
	  do j=1,i-1
	    if(ifsimul(i).eq.ifsimul(j).and.If2Sim(j).gt.0)
     *	      If2Sim(i) = If2Sim(j)
	  enddo
	  if(If2Sim(i).eq.0)then
	    nsimgrp = nsimgrp + 1
	    If2Sim(i) = nsimgrp
	    nifs(nsimgrp) = 0
	  endif
	enddo
c
c  Map from simultaneous group number to ifno.
c
	do i=1,nif
	  s = If2Sim(i)
	  if(s.gt.0)then
	    nifs(s) = nifs(s) + 1
	    Sim2If(nifs(s),s) = i
	  endif
	enddo
c
c  Sort the Sim2If index so that the ifno with smaller IF_CHAIN come
c  first.
c
	do i=1,nsimgrp
	  more = .true.
	  dowhile(more)
	    more = .false.
	    do j=2,nifs(i)
	      if(ifchain(Sim2If(j,i)).lt.ifchain(Sim2If(j-1,i)))then
		s = Sim2If(j,i)
		Sim2If(j,i) = Sim2If(j-1,i)
		Sim2If(j-1,i) = s
		more = .true.
	      endif
	    enddo
	  enddo
	enddo
c
c  Determine the map from the RPFITS ifno variable to the position on the
c  Miriad IF axis.
c
	do i=1,nif
	  Sif(i) = 0
	enddo
c
	do i=1,nsimgrp
	  do j=1,nifs(i)
	    Sif(Sim2If(j,i)) = j
	  enddo
	enddo
c
	end
