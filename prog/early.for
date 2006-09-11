c************************************************************************
	program early
	implicit none
c
c= early -- Flag data which has early/normal problems.
c& rjs
c: calibration, uv analysis
c+
c	EARLY is a Miriad task which flags visibility data that it suspects
c	contains early/normal problems. The input must be a multi-channel
c	dataset (NOT channel 0). Early works by comparing the scatter of
c	data within a visibility record with that expected because of thermal
c	noise. If the scatter exceeds some number of sigmas, then the data
c	are assumed to be affected by the early/normal problem, and the
c	entire record is flagged.
c
c	Several assumptions about the data are make:
c	Firsly there is significant signal in the visibility
c	record so that the phase slope across the band causes a substantial
c	increase in the scatter. This will not be true if the signal is
c	weak, and the visibility records are noise dominated. If the signal
c	is noise dominated, no early/normal problems can be detected, and
c	the data will not be flagged.
c
c	Secondly the data have been bandpass calibrated. EARLY always applies
c	a bandpass correction to the data if there is one present.
c
c	Thirdly that the noise level is fairly constant across the band,
c
c	Fourthly that any end channels that might be bad have already been
c	flagged.
c
c	Fifthly there is no substantial variation of the signal across
c	the band. This will be true for continuum, but not spectral
c	line work.
c@ vis
c	The input visibility data-set to flag. Several files can be
c	given. Wildcards are supported. No default.
c@ select
c	Normal uv selection. This selects which data to check for early/normal
c	problems.
c@ sigma
c	Number of sigmas. Default is 10.
c@ log
c	Output log file for messages. The default is the terminal.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Possible values are:
c	  noapply    Do not do any flagging -- just check the data, and
c	             produce the appropriate messages.
c	  verbose    Give details about what is (or would be) flagged.
c--
c  History:
c    10nov93 rjs   Original version.
c     9sep94 rjs   Support for felocity linetype.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSPECT
	character version*(*)
	parameter(version='Early: version 1.0 9-Sep-94')
	parameter(MAXSPECT=16)
c
	real sigma,Sum,SumS,SumS2,s
	integer nrec,nflag,nchan,tno,n,vupd,nspect,nschan(MAXSPECT)
	logical doapply,verbose
	character logf*64,line*64
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	double precision preamble(4)
c
c  Externals.
c
	logical uvDatOpn
	character itoaf*8
c
c  Get the input parameters, and check them. Flags to uvDatInp are
c  to do data selection and bandpass correction. Do not perform antenna
c  or leakage correction, or any Stokes conversion.
c
	call output(version)
	call keyini
	call uvDatInp('vis','df')
	call keyr('sigma',sigma,10.)
	call keya('log',logf,' ')
	call GetOpt(doapply,verbose)
	call keyfin
c
	if(sigma.lt.0)call bug('f','Sigma must be positive')
	if(sigma.lt.3)call bug('w','Sigma is dangerously low')
c
c  Open the log file.
c
	call LogOpen(logf,' ')
	if(logf.ne.' ')call logwrit(version)
c
	dowhile(uvDatOpn(tno))
	  call uvDatGta('name',line)
	  call logwrit('Processing '//line)
	  nrec = 0
	  nflag = 0
	  Sum = 0
	  SumS = 0
	  SumS2 = 0
c
	  call dspecini(tno,vupd)
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    call dspect(tno,vupd,nchan,MAXSPECT,nspect,nschan)
	    call Process(data,flags,nchan,nspect,nschan,
     *					sigma,n,Sum,SumS,SumS2)
	    if(n.gt.0)then
	      if(verbose)call Verbage(tno,preamble,n)
	      if(doapply)call uvflgwr(tno,flags)
	      nrec = nrec + 1
	      nflag = nflag + n
	    endif
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
c
c  More verbage.
c
	  call logwrit('Records flagged: '//itoaf(nrec))
	  call logwrit('Correlations flagged: '//itoaf(nflag))
	  s = SumS / Sum
	  write(line,'(a,1pe10.3)')'Mean of the scatter/sigma ratio: ',s
	  call logwrit(line)
	  s = sqrt(SumS2/Sum - s*s)
	  write(line,'(a,1pe10.3)')'Rms of the scatter/sigma ratio: ',s
	  call logwrit(line)
c
c  Write some history.
c
	  if(doapply)then
	    call hisopen(tno,'append')
	    call hiswrite(tno,'EARLY: Miriad '//version)
	    call hisinput(tno,'EARLY')
	    call hiswrite(tno,'EARLY: Records flagged: '//itoaf(nrec))
	    call hiswrite(tno,'EARLY: Correlations flagged: '
     *							//itoaf(nflag))
	    call hisclose(tno)
	  endif
	  call uvDatCls
	enddo
c
c  All said and done.
c
	call logclose
	end
c************************************************************************
	subroutine GetOpt(doapply,verbose)
c
	implicit none
	logical doapply,verbose
c
c  Get extra processing options.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=2)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'noapply ','verbose '/
c
	call options('options',opts,present,NOPTS)
	doapply = .not.present(1)
	verbose = present(2)
c
	end
c************************************************************************
	subroutine Process(data,flags,nchan,nspect,nschan,
     *					sigma,nflag,Sum,SumS,SumS2)
c
	implicit none
	integer nflag,nchan,nspect,nschan(nspect)
	real sigma,Sum,SumS,SumS2
	logical flags(nchan)
	complex data(nchan)
c
c  Input:
c    nchan
c    nspect
c    nschan
c    data
c    sigma
c  Input/Output:
c    Sum
c    SumS
c    SumS2
c    flags
c  Output:
c    nflag
c------------------------------------------------------------------------
	real var2,scat2,sig,s2
	integer off,n,i,j
	complex d,s
c
c  Determine the number of channels in each spectral window.
c
	call uvDatGtr('variance',var2)
	if(var2.eq.0)call bug('f','Variance could not be determined')
c
	nflag = 0
	off = 0
	do j=1,nspect
	  s = 0
	  s2 = 0
	  n = 0
	  do i=1,nschan(j)
	    if(flags(off+i))then
	      d = data(off+i)
	      s = s + d
	      s2 = s2 + real(d)**2+aimag(d)**2
	      n = n + 1
	    endif
	  enddo
c
c  Do we have enough to determine an rms?
c
	  if(n.gt.1)then
	    s = s /n
	    scat2 = 0.5 * (s2/n - real(s)**2 - aimag(s)**2)
	    sig = sqrt(abs(scat2) / var2)
	    SumS = SumS + sig
	    SumS2 = SumS2 + sig*sig
	    Sum = Sum + 1
c
c  Flag the data if appropriate.
c
	    if(sig.gt.sigma)then
	      do i=1,nschan(j)
		if(flags(off+i))nflag = nflag + 1
		flags(off+i) = .false.
	      enddo
	    endif
	  endif
c
	  off = off + nschan(j)
	enddo
c
	end
c************************************************************************
	subroutine dspecini(lIn,vupd)
c
	implicit none
	integer lIn,vupd
c------------------------------------------------------------------------
	character ltype*16
c
	call uvDatGta('ltype',ltype)
	if(ltype.eq.'channel')then
	  call uvVarini(lIn,vupd)
	  call uvVarSet(vupd,'nschan')
	else if(ltype.eq.'velocity'.or.ltype.eq.'felocity')then
	  vupd = 0
	else
	  call bug('f','Unrecognised or unsupported linetype '//ltype)
	endif
	end
c************************************************************************
	subroutine dspect(lIn,vupd,nchan,maxspect,nspect,nschan)
c
	implicit none
	integer lIn,vupd,nchan,maxspect,nspect,nschan(MAXSPECT)
c
c  Determine the number of channels in each spectral window.
c
c  Input:
c    lIn
c    vupd
c    nchan
c    maxspect
c  Output:
c    nspect
c    nschan
c------------------------------------------------------------------------
	integer MSPECT
	parameter(MSPECT=16)
	double precision line(6)
	integer start,step,n,nschand(MSPECT),ispect
c
c  Externals.
c
	logical uvVarUpd
c
	if(vupd.eq.0)then
	  nspect = 1
	  nschan(1) = nchan
	else if(uvVarupd(vupd))then
	  call uvinfo(lIn,'line',line)
	  n = nint(line(2))
	  start = nint(line(3))
	  step  = nint(line(5))
	  call uvrdvri(lIn,'nspect',nspect,1)
	  if(nspect.eq.1)then
	    nschan(1) = n
	  else
	    if(nspect.gt.MSPECT)
     *		call bug('f','Too many spectral windows')
	    call uvgetvri(lIn,'nschan',nschand,nspect)
	    ispect = 1
	    nspect = 0
	    dowhile(n.gt.0)
	      dowhile(start.gt.nschand(ispect))
	        start = start - nschand(ispect)
	        ispect = ispect + 1
	      enddo
	      nspect = nspect + 1
	      if(nspect.gt.maxspect)
     *		call bug('f','Too many sepctral windows')
	      nschan(nspect) = min((nschand(ispect)-start)/step+1,n)
	      n = n - nschan(nspect)
	      start = start + step*nschan(nspect)
	    enddo
	  endif
	endif
	end
c************************************************************************
	subroutine Verbage(tno,preamble,n)
c
	implicit none
	integer tno,n
	double precision preamble(4)
c
c  Give a message about the data that was (or protentially was) flagged.
c
c------------------------------------------------------------------------
	character line*72
	integer i1,i2,pol,l
c
c  Externals.
c
	integer len1
	character itoaf*8,PolsC2P*2
c
	call uvrdvri(tno,'pol',pol,1)
	call basant(preamble(4),i1,i2)
c
	line = 'Flagging '//itoaf(n)
	l = len1(line)
	line(l+1:) = ' correlations, Time='
	l = len1(line)
	call julday(preamble(3),'H',line(l+1:))
	l = len1(line)
	line(l+1:) = ', Ants='//itoaf(i1)
	l = len1(line)
	line(l+1:) = '-'//itoaf(i2)
	l = len1(line)
	line(l+1:) = ', Pol='//PolsC2P(pol)
c
	call logwrit(line)
	end
