c************************************************************************
	program fgflag
	implicit none
c
c= fgflag - Apply an AIPS flagging table to visibility data.
c& rjs
c: calibration, uv analysis
c+
c	FGFLAG is a Miriad task which flags visibility data according to
c	the contents of an AIPS flagging table (``AIPS FG'' tables).
c	These flagging tables can be created by AIPS, and transfered by
c	FITS file. If a visibility FITS file read using Miriad task ``fits''
c	contains AIPS FG tables, they will be copied (without applying them)
c	to the output Miriad data-set. FGFLAG can then be used to apply these
c	flagging tables to the data.
c
c	Note: The flagging tables, and information needed to apply them, is
c	not copied by any Miriad tasks such as ``uvaver'' or ``uvcat'',
c	nor do any other Miriad tasks interpret them. It is best to apply
c	the flagging table soon after loading in a FITS file.
c
c	Note: The information to flag based upon polarization is lost by
c	task ``fits''. Consequently fgflag flags without regard to
c	polarization type.
c
c@ vis
c	The input visibility data-set to flag. No default.
c@ select
c	Normal uv selection. This selects which data to apply the flagging
c	table to. Window and amplitude selection are not allowed.
c	The default is to apply the flagging table to the entire data-set.
c@ fgtable
c	When there are multiple flagging tables in the data-set, this gives
c	the table number to apply. The default is the highest versioned
c	table.
c--
c
c  History:
c    27jul93 rjs   Original version.
c    25nov93 rjs   Increase number of rows.
c    23apr09 rjs   Increase buffer size to allow it to work on bigger problems.
c    08jun11 rjs   Make "interval" 0.5 sec.
c    01sep12 rjs   Rewrite and fix some flawed code (get rid of interval).
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='FgFlag: version 1.0 01-Sep-12')
	integer MAXROWS,MAXSELS
	parameter(MAXROWS=300000,MAXSELS=256)
	integer fgtable
	integer list(MAXROWS)
	double precision time(2,MAXROWS)
	integer subarray(MAXROWS),srcid(MAXROWS),freqid(MAXROWS)
	integer chans(2,MAXROWS),ants(2,MAXROWS),ifs(2,MAXROWS)
c	
	real sels(MAXSELS)
	character vis*64,line*64
	integer iostat,lIn,lTab,nrows,nflag
c
c  Externals.
c
	logical hdprsnt,SelProbe
	character itoaf*10
c
c  Get the input parameters, and check them.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call SelInput('select',sels,MAXSELS)
	call keyi('fgtable',fgtable,0)
	call keyfin
	if(vis.eq.' ')call bug('f','An input data-set must be given')
c
c  Check for window and amplitude selection -- these are not allowed.
c
	if(SelProbe(sels,'window?',0.d0))
     *	  call bug('f','Window selection not supported')
	if(SelProbe(sels,'amplitude?',0.d0))
     *	  call bug('f','Amplitude selection not supported')
c
c  Open the visibility file.
c
	call uvopen(lIn,vis,'old')
c
c  Determine the table to apply.
c
	if(fgtable.eq.0)then
	  dowhile(hdprsnt(lIn,'aipsfg'//itoaf(fgtable+1)))
	    fgtable = fgtable + 1
	  enddo
	  if(fgtable.eq.0)
     *	    call bug('f','No flagging tables could be found')
	  call output('Applying flagging table '//itoaf(fgtable))
	endif
c
c  Load the FG table.
c
	call haccess(lIn,lTab,'aipsfg'//itoaf(fgtable),'read',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call FgRead(lTab,MAXROWS,nrows,
     *	  Time,SrcId,FreqId,SubArray,Ants,Chans,Ifs)
	call hdaccess(lTab,iostat)
	call output('Number of flagging commands: '//itoaf(nrows))
	if(nrows.eq.0)
     *	  call bug('f','No flagging commands found in the aipsfg table')
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Apply the flagging information.
c
	call FgApply(lIn,nrows,list,Time,SrcId,FreqId,SubArray,
     *					  Ants,Chans,Ifs,nflag)
c
	if(nflag.gt.0)then
	  call output('Number of correlations flagged: '//itoaf(nflag))
	else
	  call bug('w','No correlations were flagged')
	endif
c
c  Write some history.
c
	call hisopen(lIn,'append')
	call hiswrite(lIn,'FGFLAG: Miriad '//version)
	call hisinput(lIn,'FGFLAG')
	line = 'FGFLAG: Number of correlations flagged: '//itoaf(nflag)
	call hiswrite(lIn,line)
	call hisclose(lIn)
c
c  All said and done.
c
	call uvclose(lIn)
	end
c************************************************************************
	subroutine FgRead(lTab,MAXROWS,nrows,
     *	  Time,SrcId,FreqId,SubArray,Ants,Chans,Ifs)
c
	implicit none
	integer MAXROWS,nrows,lTab
	integer SrcId(MAXROWS),FreqId(MAXROWS),SubArray(MAXROWS)
	integer Chans(2,MAXROWS),Ifs(2,MAXROWS),Ants(2,MAXROWS)
	double precision Time(2,MAXROWS)
c
c  Read in the AIPS FG table.
c
c  Input:
c    lTab
c    MAXROWS
c  Output:
c    Time
c    SrcId
c    FreqId
c    SubArray
c    Chan
c    Ifs
c    Ants
c------------------------------------------------------------------------
	character line*132
	integer iostat
c
	nrows = 0
	call hreada(lTab,line,iostat)
	dowhile(iostat.eq.0)
	  nrows = nrows + 1
	  if(nrows.gt.MAXROWS)
     *	    call bug('f','Aipsfg table too big for me to handle')
	  call FgDecode(line,Time(1,nrows),SrcId(nrows),FreqId(nrows),
     *	    SubArray(nrows),Chans(1,nrows),Ifs(1,nrows),Ants(1,nrows))
	  call hreada(lTab,line,iostat)
	enddo
c
	if(iostat.ne.-1)then
	  call bug('w','Error reading aipsfg table')
	  call bugno('f',iostat)
	endif
	end
c************************************************************************
	subroutine FgDecode(line,Time,SrcId,FreqId,SubArray,
     *						Chans,Ifs,Ants)
c
	implicit none
	character line*(*)
	double precision Time(2)
	integer SrcId,FreqId,SubArray,Chans(2),Ifs(2),Ants(2)
c
c  Decode a line from an "aipsfg" table.
c------------------------------------------------------------------------
	double precision Tminf,Tpinf
	parameter(Tminf=0.d0,Tpinf=7000000.d0)
c
	integer k1,k2,k1d,k2d,length,nvals,temp
	character type*16,field*64
	double precision vals(2)
c
c  Externals.
c
	integer len1
c
	Time(1) = Tminf
	Time(2) = Tpinf
	SrcId = 0
	FreqId = 0
	SubArray = 0
	Chans(1) = 0
	Chans(2) = 0
	Ifs(1) = 0
	Ifs(2) = 0
	Ants(1) = 0
	Ants(2) = 0
c
	k2 = len1(line)
	k1 = 1
	dowhile(k1.le.k2)
	  call GetTok(line,k1,k2,type,length)
	  call GetField(line,k1,k2,field,k2d)
	  k1d = 1
	  if(type.eq.'time')then
	    call FgValIn(field,k1d,k2d,'t',vals,2,nvals)
	  else
	    call FgValIn(field,k1d,k2d,'d',vals,2,nvals)
	  endif
c
	  if(type.eq.'srcid')then
	    srcid = nint(vals(1))
	    if(nvals.ne.1.or.srcid.lt.0)
     *		call bug('f','Bad srcid entry in aipsfg table')
c
	  else if(type.eq.'freqid')then
	    freqid = nint(vals(1))
	    if(nvals.ne.1.or.freqid.lt.0)
     *		call bug('f','Bad freqid entry in aipsfg table')
c
	  else if(type.eq.'time')then
	    if(nvals.ne.2)
     *		call bug('f','Bad time entry in aipsfg table')
	    if(vals(1).eq.0)then
	      time(1) = Tminf
	    else
	      time(1) = vals(1)
	      if(vals(1).lt.Tminf)
     *		call bug('f','Time before Big Bang')
	    endif
	    if(vals(2).eq.0)then
	      time(2) = Tpinf
	    else
	      time(2) = vals(2)
	      if(vals(2).gt.Tpinf)
     *		call bug('f','Time is after end of Universe')
	    endif
c
	  else if(type.eq.'array')then
	    subarray = nint(vals(1))
	    if(nvals.ne.1.or.subarray.lt.0)
     *		call bug('f','Bad array entry in aipsfg table')
c
	  else if(type.eq.'chan')then
	    if(nvals.ne.2.or.vals(1).lt.0.or.vals(2).lt.0)
     *		call bug('f','Bad chan entry in aipsfg table')
	    chans(1) = nint(vals(1))
	    chans(2) = nint(vals(2))
c
	  else if(type.eq.'ifs')then
	    if(nvals.ne.2.or.vals(1).lt.0.or.vals(2).lt.0)
     *		call bug('f','Bad ifs entry in aipsfg table')
	    ifs(1) = nint(vals(1))
	    ifs(2) = nint(vals(2))
c
	  else if(type.eq.'ant')then
	    if(nvals.ne.2.or.vals(1).lt.0.or.vals(2).lt.0)
     *		call bug('f','Bad ants entry in aipsfg table')
	    ants(1) = nint(vals(1))
	    ants(2) = nint(vals(2))
	    if(ants(1).gt.ants(2).and.ants(2).gt.0)then
	      temp = ants(2)
	      ants(2) = ants(1)
	      ants(1) = temp
	    endif
c
	  else
	    call bug('f','Unrecognised type in aipsfg table')
	  endif
	  if(k1.lt.k2.and.line(k1:k1).ne.',')
     *	    call bug('f','Bad aipsfg table command format')
	  k1 = k1 + 1
	enddo
c
	end
c************************************************************************
	subroutine FgValIn(line,k1,k2,type,vals,MAXVALS,nvals)
c
	implicit none
	character line*(*),type*1
	integer k1,k2,MAXVALS,nvals
	double precision vals(MAXVALS)
c
c  Decode some stuff.
c
c  Input:
c    line
c    type
c    MAXVALS
c  Input/Output:
c    k1
c  Output:
c    vals
c    nvals
c------------------------------------------------------------------------
	integer k0
	logical more,ok
c
	if(line(k1:k1).ne.'(')
     *	  call bug('f','Error decoding aipsfg entry')
	k1 = k1 + 1
	k0 = k1
	nvals = 0
	more = .true.
	dowhile(k1.le.k2.and.more)
	  if(line(k1:k1).eq.','.or.line(k1:k1).eq.')')then
	    more = line(k1:k1).eq.','
	    if(k1.le.k0)call bug('f','Bad aipsfg subcommand')
	    nvals = nvals + 1
	    if(nvals.gt.MAXVALS)
     *	    call bug('f','Too many values in aipsfg subcommand')
	    if(type.eq.'d')then
	      call atodf(line(k0:k1-1),vals(nvals),ok)
	      if(.not.ok)call bug('f','Error decoding a value')
	    else if(type.eq.'t')then
	      call dayjul(line(k0:k1-1),vals(nvals))
	    else
	      call bug('f','Unrecognised format in FgValIn')
	    endif
	    k0 = k1 + 1
	  endif
	  if(more) k1 = k1 + 1
	enddo
c
c  Do some more checks.
c
	if(k1.gt.k2)call bug('f','Bad aipsfg command')
	if(line(k1:k1).ne.')')call bug('f','Bad aipsfg command')
	k1 = k1 + 1
	if(nvals.eq.0)
     *	  call bug('f','Bad parameters in aipsfg command')
c
	end
c************************************************************************
	subroutine FgApply(lIn,nrows,list,Time,SrcId,FreqId,SubArray,
     *						Ants,Chans,Ifs,nflag)
c
	implicit none
	integer lIn,nrows,list(nrows)
	integer SrcId(nrows),Freqid(nrows),SubArray(nrows)
	integer Ants(2,nrows),Chans(2,nrows),Ifs(2,nrows)
	integer nflag
	double precision Time(2,nrows)
c
c  Apply the flagging table to all the data.
c
c  Input:
c    All arguments are inputs.
c  Output:
c    None!
c------------------------------------------------------------------------
	include 'maxdim.h'
	double precision Tminf,Tpinf
	parameter(Tminf=0.d0,Tpinf=7000000.d0)
c
	complex data(MAXCHAN)
	logical flags(MAXCHAN),flagged
	integer nchan,nspect,vupd,nschan(MAXWIN)
	integer fgsrcid,fgfreqid,fgarray,i,id,ant1,ant2,nlist
	double precision t,preamble(4),tmin,tmax
c
c  Externals.
c
	logical uvVarUpd
c
c  Keep track of source id, freqid, subarray, nspect, nschan
c
	call uvVarini(lIn,vupd)
	call uvVarSet(vupd,'fgsrcid')
	call uvVarSet(vupd,'fgfreqid')
	call uvVarSet(vupd,'fgarray')
	call uvVarSet(vupd,'nspect')
	call uvVarSet(vupd,'nschan')
	fgsrcid  = 1
	fgfreqid = 1
	fgarray  = 1
c
	call uvread(lIn,preamble,data,flags,MAXCHAN,nchan)
	tmin = preamble(3) + 1
	tmax = preamble(3) + 2
	nflag = 0
	dowhile(nchan.gt.0)
c
c  If things have been updated, load the new values.
c
	  if(uvVarUpd(vupd))then
	    call uvrdvri(lIn,'fgsrcid', fgsrcid,1)
	    call uvrdvri(lIn,'fgfreqid',fgfreqid,1)
	    call uvrdvri(lIn,'fgarray', fgarray,1)
	    call uvrdvri(lIn,'nspect',nspect,1)
	    if(nspect.gt.MAXWIN)
     *	      call bug('f','Nspect too big, in FgApply')
	    call uvgetvri(lIn,'nschan',nschan,nspect)
	    do i=2,nspect
	      if(nschan(i).ne.nschan(1))call bug('f',
     *	       'Cannot cope with variable value for nschan, in FgApply')
	    enddo
	    if(nschan(1)*nspect.ne.nchan)call bug('f',
     *	       'Inconsistent number of channels, in FgApply')
	  endif
c
c  Determine time and baseline number.
c
	  t = preamble(3)
	  if(t.lt.Tminf.or.t.gt.Tpinf)
     *	    call bug('f','Invalid assumption about time range')
	  call Basant(preamble(4),ant1,ant2)
c
c  Determine flagging commands that might apply to these data.
c
	  if(t.lt.tmin.or.t.gt.tmax)then
	    call fglist(nrows,time,t,list,nlist,tmin,tmax)
#ifdef TEST
	    call writeout(nlist,t,tmin,tmax)
#endif
	  endif

c
c  Apply the universally applicable time flagging.
c
	  flagged = .false.
	  do i=1,nlist
	    id = list(i)
	    call FgFlg(flags,nspect,nschan,
     *		fgsrcid,fgfreqid,fgarray,ant1,ant2,
     *		Srcid(id),Freqid(id),SubArray(id),
     *		Ants(1,id),Chans(1,id),Ifs(1,id),flagged,nflag)
	  enddo
c
	  if(flagged)call uvflgwr(lIn,flags)
	  call uvread(lIn,preamble,data,flags,MAXCHAN,nchan)	
	enddo
c
	end
c************************************************************************
	subroutine FgFlg(flags,nspect,nschan,
     *	  fgsrcid,fgfreqid,fgarray,ant1,ant2,
     *	  Srcid,Freqid,SubArray,Ants,Chans,Ifs,flagged,nflag)
c
	implicit none
	integer nspect,nschan,nflag
	logical flags(nschan,nspect),flagged
	integer fgsrcid,fgfreqid,fgarray,ant1,ant2
	integer Srcid,Freqid,SubArray,Ants(2),Chans(2),Ifs(2)
c
c  Flag the appropriate channels, if needed.
c
c------------------------------------------------------------------------
	integer chan1,chan2,if1,if2,i,j
c
	if((Srcid.eq.0.or.fgsrcid.eq.Srcid).and.
     *	   (Freqid.eq.0.or.fgfreqid.eq.Freqid).and.
     *	   (SubArray.eq.0.or.fgarray.eq.SubArray).and.
     *	   ((Ants(1).eq.0).or.
     *	    (Ants(2).eq.0.and.(Ants(1).eq.ant1.or.Ants(1).eq.ant2)).or.
     *	    (Ants(1).eq.ant1.and.Ants(2).eq.ant2)))then
	  chan1 = chans(1)
	  if(chan1.le.0)chan1 = 1
	  chan2 = chans(2)
	  if(chan2.le.0.or.chan2.gt.nschan)chan2 = nschan
	  if1 = ifs(1)
	  if(if1.le.0)if1 = 1
	  if2 = ifs(2)
	  if(if2.le.0.or.if2.gt.nspect) if2 = nspect
	  if(if1.le.if2.and.chan1.le.chan2)then
	    do j=if1,if2
	      do i=chan1,chan2
		if(flags(i,j))then
		  flags(i,j) = .false.
	          flagged = .true.
		  nflag = nflag + 1
		endif
	      enddo
	    enddo
	  endif
	endif
c
	end
c************************************************************************
	subroutine fglist(nrows,time,t0,list,nlist,tmin,tmax)
c
	implicit none
	integer nlist,nrows,list(nrows)
	double precision time(2,nrows),t0,tmin,tmax
c
c  Determine the time flagging commands that are active for this
c  particular time, and determine the time range over which this list
c  will remain valid.
c------------------------------------------------------------------------
	double precision Tminf,Tpinf
	parameter(Tminf=0.d0,Tpinf=7000000.d0)
c
	integer i
c
	tmin = Tminf
	tmax = Tpinf
	nlist = 0
c
	do i=1,nrows
	  if(time(1,i).le.t0.and.t0.le.time(2,i))then
	    nlist = nlist + 1
	    list(nlist) = i
	    tmin = max(tmin,time(1,i))
	    tmax = min(tmax,time(2,i))
	  else if(t0.lt.time(1,i))then
	    tmax = min(tmax,time(1,i))
	  else if(t0.gt.time(2,i))then
	    tmin = max(tmin,time(2,i))
	  endif
	enddo
c
	end
#ifdef TEST
c************************************************************************
	subroutine writeout(n,t,tmin,tmax)
c
	implicit none
	integer n
	double precision t,tmin,tmax
c
c------------------------------------------------------------------------
	character line*128,ts*32,tmins*32,tmaxs*32
c
c  Externals.
c
	character stcat*128,itoaf*8
c
	call julday(t,'H',ts)
	if(tmin.le.0.d0)then
	  tmins = '-inf'
	else
	  call julday(tmin,'H',tmins)
	endif
	if(tmax.gt.5000000.d0)then
	  tmaxs = '+inf'
	else
	  call julday(tmax,'H',tmaxs)
	endif
c
	line = stcat(stcat(stcat(stcat('For time',' '//ts),
     *	  ' n='//itoaf(n)),' trange='//tmins),','//tmaxs)
	call output(line)
	end
#endif
