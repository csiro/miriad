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
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='FgFlag: version 1.0 27-Jul-93')
	integer MAXROWS,MAXRCPNT,MAXSELS
	parameter(MAXROWS=3000,MAXRCPNT=4096,MAXSELS=256)
	integer nrange,nrcpnt,fgtable
	integer indx(2*MAXROWS),pnt(2,MAXROWS+1),RcPnt(MAXRCPNT)
	double precision time(2,MAXROWS)
	double precision timrange(MAXROWS+1)
	integer subarray(MAXROWS),srcid(MAXROWS),freqid(MAXROWS)
	integer chans(2,MAXROWS),ants(2,MAXROWS),ifs(2,MAXROWS)
	real sels(MAXSELS)
	character vis*64
	integer iostat,lIn,lTab,nrows,nUni
c
c  Externals.
c
	logical hdprsnt,SelProbe
	character itoaf*3
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
     *	  SrcId,SubArray,FreqId,Ants,Time,Ifs,Chans)
	call hdaccess(lTab,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Determine the appropriate index tables for the data.
c
	call FgIndx(time,nrows,timrange,pnt,indx,nrange,nUni,
     *	  RcPnt,MAXRCPNT,nrcpnt)
c
c  Apply the flagging information.
c
	call FgApply(lIn,TimRange,Pnt,nRange,nUni,RcPnt,nRcPnt,
     *	  nrows,Time,SrcId,FreqId,SubArray,Ants,Chans,Ifs)
c
c  Write some history.
c
	call hisopen(lIn,'append')
	call hiswrite(lIn,'FGFLAG: Miriad '//version)
	call hisinput(lIn,'FGFLAG')
	call hisclose(lIn)
c
c  All said and done.
c
	call uvclose(lIn)
	end
c************************************************************************
	subroutine FgRead(lTab,MAXROWS,nrows,
     *	  SrcId,SubArray,FreqId,Ants,Time,Ifs,Chans)
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
	integer k1,k2,k1d,k2d,length,nvals,temp
	character type*16,field*64
	double precision vals(2)
c
c  Externals.
c
	integer len1
c
	Time(1) = 0
	Time(2) = 0
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
	    time(1) = vals(1)
	    time(2) = vals(2)
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
	subroutine FgIndx(time,nrows,timrange,pnt,indx,nrange,nUni,
     *	  RcPnt,MAXRCPNT,nrcpnt)
c
	implicit none
	integer nrows,nrange,nrcpnt,MAXRCPNT,nUni
	integer pnt(2,nrows),indx(2*nrows),RcPnt(MAXRCPNT)
	double precision time(2*nrows),TimRange(nrows+1)
c
c  Work out various structures that we need to perform the on-the-fly
c  flagging.
c
c  Input:
c    time
c    nrows
c    MAXRCPNT
c  Output:
c    timrange
c    pnt
c    nrange
c    RcPnt
c    nrcpnt
c    nUni
c  Scratch:
c    indx
c------------------------------------------------------------------------
	integer MAXREC
	double precision DT,INTERVAL
	parameter(DT=5.d0/3600.d0/24.d0,INTERVAL=5.d0/60.d0/24.d0)
	parameter(MAXREC=128)
	integer k,kd,current(MAXREC),expired(MAXREC),ncur,nexp
	double precision t0,tprev
c
c  Sort the times.
c
	call hsortd(2*nrows,time,indx)
c
c  Do through the records, determining which ones are active at a particular
c  time.
c
	t0 = 0
	ncur = 0
	nexp = 0
	nrange = 0
	nrcpnt = 0
	do k=1,2*nrows
	  kd = indx(k)
c
c  Handle the case of a zero time.
c
	  if(time(kd).eq.0)then
	    if(2*(kd/2).ne.kd)then
	      nrcpnt = nrcpnt + 1
	      if(nrcpnt.gt.MAXRCPNT)
     *	        call bug('f','Buffer overflow, in FgIndx')
	      rcpnt(nrcpnt) = kd/2 + 1
	      nUni = nrcpnt
	    endif
c
c  Handle the case of a start time.
c
	  else if(2*(kd/2).ne.kd)then
	    if(t0.eq.0)t0 = time(kd) - DT
	    if(ncur.eq.MAXREC.or.time(kd).gt.t0+INTERVAL.or.
     *	      (time(kd).gt.tprev.and.2*nexp.gt.ncur))then
	      nrange = nrange + 1
	      timrange(nrange) = t0
	      pnt(1,nrange) = nrcpnt + 1
	      pnt(2,nrange) = nrcpnt + ncur
	      call FgOrg(ncur,current,nexp,expired,
     *				rcpnt,nrcpnt,MAXRCPNT)
	      ncur = ncur - nexp
	      nexp = 0
	      t0 = max(0.5*(time(kd)+tprev),time(kd)-DT)
	    endif
	    ncur = ncur + 1
	    current(ncur) = kd/2 + 1
c
c  Handle the case of an end time.
c
	  else
	    nexp = nexp + 1
	    expired(nexp) = kd/2
	  endif
	  tprev = time(kd)
	enddo
c
c  If there are some current records, flush them out.
c
	if(ncur.gt.0)then
	  nrange = nrange + 1
	  timrange(nrange) = t0
	  pnt(1,nrange) = nrcpnt + 1
	  pnt(2,nrange) = nrcpnt + ncur
	  call FgOrg(ncur,current,nexp,expired,rcpnt,nrcpnt,MAXRCPNT)
	  ncur = ncur - nexp
	endif
	if(ncur.gt.0)call bug('f','Assertion failure, in FgIndx')
c
c  Add the final thing to the end.
c
	TimRange(nRange+1) = tprev + DT
c
	end
c************************************************************************
	subroutine FgOrg(ncur,current,nexp,expired,
     *					rcpnt,nrcpnt,MAXRCPNT)
c
	implicit none
	integer ncur,nexp,nrcpnt,MAXRCPNT
	integer current(ncur),expired(nexp),rcpnt(MAXRCPNT)
c
c  Copy the list of current records, and then delete the ones that have
c  expired from the list.
c
c  Input:
c    ncur
c    nexp
c    MAXRCPNT
c  Input/Output:
c    current	Those records whose times are current. On output, it
c		contains those remaining after deleting the expired ones.
c    expired	Those records whose times have expired. Destroyed on exit.
c    rcpnt
c    nrcpnt
c------------------------------------------------------------------------
	integer i,j,k
	logical delete
c
c  Sort both the current and expired lists.
c
	if(ncur.gt.0)call sorti(current,ncur)
	if(nexp.gt.0)call sorti(expired,nexp)
c
c  Copy the current ones to the output.
c
	if(ncur+nrcpnt.gt.MAXRCPNT)
     *	  call bug('f','Buffer overflow, in Organ')
	do i=1,ncur
	  nrcpnt = nrcpnt + 1
	  rcpnt(nrcpnt) = current(i)
	enddo
c
c  Update the current list.
c
	j = 0
	k = 1
	do i=1,ncur
	  delete = k.le.nexp
	  if(delete) delete = current(i).eq.expired(k)
	  if(delete)then
	    k = k + 1
	  else
	    j = j + 1
	    current(j) = current(i)
	  endif
	enddo
	if(j.ne.ncur-nexp)call bug('f','Assertion failure, in Organ')
c
	end
c************************************************************************
	subroutine FgApply(lIn,TimRange,Pnt,nRange,nUni,RcPnt,nRcPnt,
     *	  nrows,Time,SrcId,FreqId,SubArray,Ants,Chans,Ifs)
c
	implicit none
	integer lIn,nRange,nUni,nRcPnt,nrows
	integer Pnt(2,nRange),RcPnt(nRcPnt)
	integer SrcId(nrows),Freqid(nrows),SubArray(nrows)
	integer Ants(2,nrows),Chans(2,nrows),Ifs(2,nrows)
	double precision Time(2,nrows),TimRange(nRange+1)
c
c  Apply the flagging table to all the data.
c
c  Input:
c    All arguments are inputs.
c  Output:
c    None!
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSPECT
	parameter(MAXSPECT=16)
	complex data(MAXCHAN)
	logical flags(MAXCHAN),flagged
	integer nchan,nspect,ipt,vupd,nschan(MAXSPECT)
	integer fgsrcid,fgfreqid,fgarray,i,id,ant1,ant2
	double precision t,preamble(4)
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
	ipt = 1
	call uvread(lIn,preamble,data,flags,MAXCHAN,nchan)	
	dowhile(nchan.gt.0)
c
c  If things have been updated, load the new values.
c
	  if(uvVarUpd(vupd))then
	    call uvrdvri(lIn,'fgsrcid', fgsrcid,1)
	    call uvrdvri(lIn,'fgfreqid',fgfreqid,1)
	    call uvrdvri(lIn,'fgarray', fgarray,1)
	    call uvrdvri(lIn,'nspect',nspect,1)
	    if(nspect.gt.MAXSPECT)
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
	  call Basant(preamble(4),ant1,ant2)
c
c  Find the appropriate time record.
c
	  call FgLoc(t,TimRange,nrange,ipt)
c
c  Apply the universally applicable time flagging.
c
	  flagged = .false.
	  do i=1,nUni
	    id = RcPnt(i)
	    call FgFlg(flags,nspect,nschan,
     *		fgsrcid,fgfreqid,fgarray,ant1,ant2,
     *		Srcid(id),Freqid(id),SubArray(id),
     *		Ants(1,id),Chans(1,id),Ifs(1,id),flagged)
	  enddo

c
c  Apply the flagging corresponding to this interval.
c
	  if(t.gt.timrange(ipt).and.t.lt.timrange(ipt+1))then
	    do i=pnt(1,ipt),pnt(2,ipt)
	      id = RcPnt(i)
	      if(t.gt.Time(1,id).and.t.lt.Time(2,id))then
	        call FgFlg(flags,nspect,nschan,
     *		  fgsrcid,fgfreqid,fgarray,ant1,ant2,
     *		  Srcid(id),Freqid(id),SubArray(id),
     *		  Ants(1,id),Chans(1,id),Ifs(1,id),flagged)
	      endif
	    enddo
	  endif
	  if(flagged)call uvflgwr(lIn,flags)
	  call uvread(lIn,preamble,data,flags,MAXCHAN,nchan)	
	enddo
c
	end
c************************************************************************
	subroutine FgFlg(flags,nspect,nschan,
     *	  fgsrcid,fgfreqid,fgarray,ant1,ant2,
     *	  Srcid,Freqid,SubArray,Ants,Chans,Ifs,flagged)
c
	implicit none
	integer nspect,nschan
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
	    flagged = .true.
	    do j=if1,if2
	      do i=chan1,chan2
		flags(i,j) = .false.
	      enddo
	    enddo
	  endif
	endif
c
	end
c************************************************************************
	subroutine FgLoc(t,TimRange,nrange,ipt)
c
	implicit none
	integer nrange,ipt
	double precision t,Timrange(nrange+1)
c
c  Input:
c    t
c    Timrange
c    nrange
c  Input/Output:
c    ipt
c------------------------------------------------------------------------
	integer step,jpt,k
c
	step = 1
	jpt = ipt
c
c  DO a binary expansion, to boudn teh valid time range.
c
	dowhile(t.lt.Timrange(ipt).and.ipt.gt.1)
	  jpt = max(ipt-1,1)
	  ipt = max(ipt-step,1)
	  step = step + step
	enddo
c
	dowhile(t.gt.Timrange(jpt+1).and.jpt.lt.nrange)
	  ipt = min(jpt+1,nrange)
	  jpt = min(jpt+step,nrange)
	  step = step + step
	enddo
	if(t.lt.Timrange(ipt))then
	  continue
	else if(t.gt.Timrange(jpt+1))then
	  ipt = jpt
	else
c
c  We have bounded the time interval. It lies between ipt and jpt. Home
c  in on it now.
c
	  dowhile(ipt.ne.jpt)
	    k = (ipt+jpt)/2
	    if(t.gt.timrange(k+1))then
	      ipt = k+1
	    else
	      jpt = k
	    endif
	  enddo
	endif
c
	end
