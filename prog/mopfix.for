c
c= MOPFIX - Recalculate (u,v,w) in a Mopra RPFITS file
c& tw
c: analysis
c+
c	MOPFIX reads in an RPF file and a table of positions from RPFREAD.
c       The position table is interpolated onto the spectral timestamps
c       and a corrected RPFITS file is produced.
c
c@ in
c	Input RPFITS file.  No default.
c@ out
c	Output RPFITS file.  Default name has 'ix' appended (i.e.
c       2004-06-24_2300.rpf --> 2004-06-24_2300.rpfix)
c@ postab
c       Table of positions, default is pos.tab.  If the file does not exist
c       the position correction is not done.  The table can be created
c       with RPFREAD using the posout= keyword.
c@ maxgap
c       Maximum position jump in arcmin for interpolation.  If the distance
c       between the flanking position stamps exceeds this, the spectrum is
c       flagged.  Default: 1 arcmin.
c@ flagtime
c       Optional time range to flag spectra.  Give two values in UT 
c       hh:mm:ss format separated by comma.  Default is no flagging.
c@ options
c       Extra processing options.
c       'keepflag'  Preserve online flagging.  By default all spectra are 
c                   unflagged and only flagged based on maxgap above.
c       'doextrap'  Use linear extrapolation if gap between flanking position
c                   stamps exceeds maxgap.
c       'noref'     Eliminate the reference scans from the file.
c
c 28jun04 - tw - created from rpfread and mapread
c 13jul04 - tw - write OBSTYPE card for LiveData
c 20jul04 - tw - fixed for Linux compiler
c 22jun04 - tw - don't unflag spectra
c 02sep04 - tw - unflag spectra flagged online
c 03sep04 - tw - if maxgap fails then flag the spectrum
c 08dec04 - tw - allow flagging of a time range
c 25jul05 - tw - new code to interpret syscal record
c 07sep05 - tw - noref option
c 14sep05 - tw - polflag parameter
c 15jul06 - tw - allow >2 IFs
c 20jul06 - tw - remove polflag parameter (didn't work); tidy for miriad

	program mopfix
	implicit none

	include 'rpfits.inc'

	double precision pi
	parameter (pi = 3.1415 92653 58979 3238 d0)
	integer MAXPOS
	parameter (MAXPOS=8192)
	integer MAXCHAN
	parameter (MAXCHAN=8192)

	character ctime*10, rastr*12, dcstr*12, fitsfile*80, wuvfmt*10
	character infile*80, outfile*80, postab*20, src*16, cline*80
	integer jstat, flag, bin, if_no, source_no, baseline
	real m1, m2, dra, ddc, dc0, utrel, midtime
	real ut, weight(2*MAXCHAN), u, v, w, sc_buffer(1)
	real maxgap, shift, loshift, hishift, lotimegap, hitimegap
	real utpos(MAXPOS), rapos(MAXPOS), dcpos(MAXPOS), utrelp(MAXPOS)
	double precision ftime(2)
	complex vis(2*MAXCHAN)
	integer az, el, tsysa, tsysb
	integer i, i1, i2, iref, j, nsp, nhead, ncyc
	integer ln, npos, ifac, bw1, bw2, freq1, freq2, srclen
	logical dohms, extrap, keepflag, doextrap, isref, noref, dopos
	integer iptr, bufdim, iant, ifno
	equivalence ( sc_buffer(1), sc_cal(1,1,1) )

* functions
	integer len1
	character*12 rangle,hangle

        character*50 vers
        parameter ( vers = 'MOPFIX: version 20-jul-2006' )
        call output(vers)

*--------------------------------------------------------------

c Get input parameters
	call keyini ()
	call keya ('in', infile, ' ')
        if (infile.eq.' ') then
	   call bug('f','Input file must be given (in=)')
	endif
	call keya ('out', outfile, ' ')
        if (outfile.eq.' ') then
	   ln = len1(infile)
	   write (outfile,'(A,A)') infile(1:ln),'ix'
	endif
	call keya ('postab', postab, 'pos.tab')
	call keyr ('maxgap', maxgap, 1.)
        call keyt ('flagtime', ftime(1), 'dtime', -1.d0)
        call keyt ('flagtime', ftime(2), 'dtime', -1.d0)
	call GetOpt(keepflag,doextrap,noref)
	call keyfin ()

*-------------------------------------------------------

c This card is needed for LiveData
	ncard = 1 
	card(1) = 'OBSTYPE'

c Convert flag range to seconds
	ftime(1) = 86400. * ftime(1)
	ftime(2) = 86400. * ftime(2)
	if (ftime(1) .gt. 0) then
	   write(*,*) 'Flagging times: ',ftime(1),ftime(2)
	endif

c Read in the positions table
	dopos = .true.
	ln = len1(postab)
	open(unit=11,file=postab,status='old',err=1000)

c Convert RA and DEC to radians
 	i = 1
 98	read(11,'(a80)',end=99) cline
	if (cline(1:1) .eq. '#') goto 98
	read (cline,*) utpos(i), rapos(i), dcpos(i)
	rapos(i) = rapos(i) * pi/12.
	dcpos(i) = dcpos(i) * pi/180.
	if (i .ge. 2) then
	   if (abs(utpos(i)-utpos(i-1)) .lt. 0.1) i = i - 1
	endif
	i = i + 1
	goto 98
 99	npos = i - 1
	write(6,*) 'Read in ',npos,' unique lines from ',postab(1:ln)
	close(11)

c Use the first dec for dc0
	dc0 = dcpos(1)

c Convert maxgap from arcmin to radians
	maxgap = maxgap * 60 / 206265.
	goto 41

 1000	dopos = .false.
	write(6,*) 'Positions file ',postab(1:ln),' not found'
	write(6,*) 'Will not change (u,v,w) information'

c Open the RPFITS file

 41	file = infile
	jstat = -3  ! open the file
	call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)

	if (jstat .eq. -1) then
	  write (6, '('' Error reading RPFITS file'')')
	  goto 999
	end if
	write (6, '(a,a70)') ' Reading ', file

c Open the output RPFITS file

	file = outfile
	jstat = -3
	call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)
	if (jstat .eq. -1) then
	  write (6, '('' Error writing RPFITS file'')')
	  goto 999
	end if
	write (6, '(a,a40)') ' Writing ', file

	nsp = 0
	nhead = 0

c BEGIN LOOP	
	do while (.true.)

c Try to read data

	  file = infile
	  jstat = 0
	  call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)

c CASE 1: HEADER ENCOUNTERED OR END OF FILE

	  if (jstat .eq. 1 .or. jstat .eq. 3) then

c Output the number of cycles in the preceding record
	    if (nhead .gt. 0) then
	       ncyc = nsp/n_if * nint(intbase/intime)
	       if (ncyc .gt. 0) write(6,'(i3)') ncyc
	    endif 
	    nhead = nhead + 1

c IF END OF FILE: close

	    if (jstat .eq. 3) then
	       write(6,*) 'END OF FILE'
	       goto 999
	    endif

c Flush the buffer of the output file
	    if (nhead .gt. 0 .and. .not.(noref.and.isref)) then
	       file = outfile
	       jstat = 3
	       call rpfitsout (jstat, vis, weight, baseline, ut,u,v,w,
     :                        flag, bin, if_no, source_no)
	    endif

c load the header of the next scan

	    file = infile
	    jstat = -1
	    call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)

c figure out observation type (source or reference)
	    src = su_name(1)
	    srclen = len1(src)
	    if (src(srclen-1:srclen).eq.'_R') then
	       isref = .true.
	    else
	       isref = .false.
	    endif

c get up to two frequencies from header for display
	    freq1 = nint(if_freq(1)/1e6)
	    bw1 = nint(if_bw(1)/1e6)
	    if (n_if .ge. 2) then
	       freq2 = nint(if_freq(2)/1e6)
	       bw2 = nint(if_bw(2)/1e6)
	    else if (if_nstok(1) .eq. 2) then
	       freq2 = freq1
	       bw2 = bw1
	    else
	       freq2 = 0
	       bw2 = 0
	    endif

c write header to the output file (unless ref scans to be omitted)
	    if (.not.(noref .and. isref)) then
	       file = outfile
	       jstat = -1
	       call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)
	    endif

c extract header position

c	    ra  = ra  * 180. / pi
c	    dec = dec * 180. / pi
c	    call ang2str(ra,rastr,'H')
c	    call ang2str(dec,dcstr,'D')
	    rastr = hangle(ra)
	    dcstr = rangle(dec)

	    nsp  = 0
	    goto 900

	  end if         ! jstat .eq. 1 or 3

c CASE 2: DATA IN SYSCAL RECORD

	  if (baseline .eq. -1) then	! SYS cal data
	     call format_time(sc_ut,ctime)
	     iptr = 1
	     bufdim = sc_q * sc_if * sc_ant
	     do while (iptr .lt. bufdim)
		iant = (sc_buffer(iptr) + 0.5)
		if (iant .eq. 1) then ! Ant 1 record
		   ifno = nint(sc_buffer(iptr+1))
		   tsysa = nint(sc_buffer(iptr+3)**2)
		   tsysb = nint(sc_buffer(iptr+4)**2)
c Unflag records flagged as off source
		   if (.not. keepflag) then
		      if (nint(sc_buffer(iptr+12)).eq.1) then
c			 write(*,*) 'This record is flagged'
			 sc_buffer(iptr+12) = 0.
		      endif
		   endif
		else if (iant .eq. 0) then ! Ant 0 record
		   az  = nint(sc_buffer(iptr+1)*180./pi)
		   el  = nint(sc_buffer(iptr+2)*180./pi)
		endif
		iptr = iptr + sc_q
	     enddo
	     if (ifno.eq.1 .and. nsp.eq.0) then
		write(6,104) ctime,su_name(1),rastr,dcstr,az,el,
     *             freq1,freq2,bw1,bw2,tsysa,tsysb
	     endif
 104	     format(a8,1x,a10,1x,a10,1x,a9,i4,1x,i2,1x,i6,1x,i6,
     *             1x,i3,1x,i3,2i4,$)

c
c	  if (baseline .eq. -1) then	! SYS cal data
c	     call format_time(sc_ut,ctime)
c	     az = nint(sc_cal(6,2,1) * 180./pi)
c	     el = nint(sc_cal(7,2,1) * 180./pi)
c	     par = nint(sc_cal(8,2,1) * 180./pi)
c	     tsysa = nint(sc_cal(4,1,1)**2)
c	     tsysb = nint(sc_cal(5,1,1)**2)
c	     if (int(sc_cal(2,1,1)).eq.1 .and. nsp.eq.0) then
c		write(6,104) ctime,su_name(1),rastr,dcstr,az,el,
c     *             freq1,freq2,bw1,bw2,tsysa,tsysb
c	     endif
c 104	     format(a8,1x,a10,1x,a10,1x,a9,1x,i3,1x,i2,1x,i6,1x,i6,
c     *             1x,i3,1x,i3,2i4,$)
c
c
c	     write(44,'(i2,2f4.0,$)') sc_if,sc_cal(13,1,1),sc_cal(13,2,1)
c	     do i = 1, sc_q
c		write(*,*) sc_cal(i,1,1),sc_cal(i,2,1)
c	     enddo
c
	     if (.not.(noref .and. isref)) then
		file = outfile
		jstat = 0
		call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)
	     endif

	     goto 900
	  endif

c CASE 3: CLOSE/REOPEN OPERATION

	  if (jstat .eq. 5) then
	     if (.not.(noref .and. isref)) then
		file = outfile
		jstat = 3
		call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)
	     endif
	     goto 900
	  endif
	  
c CASE 4: DATA IN SPECTRUM

	  nsp = nsp + 1

c End of processing if reference scans to be omitted
	  if (noref .and. isref) goto 900

c Skip all the position fixing if no positions table given
	  if (dopos) then
c Deal with UT day transition
	     do i = 1,npos
		if (utpos(i).lt.utpos(1)) then
		   utrelp(i) = utpos(i) + 86400.
		else
		   utrelp(i) = utpos(i)
		endif
	     enddo
	     if (ut .lt. utpos(1)-60) then
		utrel = ut + 86400.
	     else
		utrel = ut
	     endif

c Look up position in table
	     if (utrel .lt. utrelp(1)) then
		i1 = 1
		i2 = 2
		iref = 1
		extrap = .false. ! ok since probably ref obs
	     else if (utrel .gt. utrelp(npos)) then
		i1 = npos-1
		i2 = npos
		iref = npos
		extrap = .true.
	     else
		i = 1
		do while (utrel .ge. utrelp(i))
		   i = i + 1
		enddo
		dra = (rapos(i) - rapos(i-1)) * cos(dc0)
		ddc = (dcpos(i) - dcpos(i-1))
		shift = sqrt(dra**2 + ddc**2)
		if (shift .gt. maxgap .and. i .gt. 2) then
		   extrap = .true.
		   dra = (rapos(i-1) - rapos(i-2)) * cos(dc0)
		   ddc = (dcpos(i-1) - dcpos(i-2))
		   loshift = sqrt(dra**2 + ddc**2)
		   lotimegap = utrel - utrelp(i-1)
		   dra = (rapos(i+1) - rapos(i)) * cos(dc0)
		   ddc = (dcpos(i+1) - dcpos(i))
		   hishift = sqrt(dra**2 + ddc**2)
		   hitimegap = utrelp(i) - utrel
		   midtime = 0.5*(utrelp(i-1)+utrelp(i))
c Extrapolate from above if slew rate is lower and time not too far
		   if (hishift .lt. loshift .and. hitimegap .lt. 
     :		        2*lotimegap .and. i .lt. npos) then
		      i1 = i
		      i2 = i+1
		      iref = i
		   else
		      i1 = i-2
		      i2 = i-1
		      iref = i-1
		   endif
		else
		   extrap = .false.
		   i1 = i-1
		   i2 = i
		   iref = i
		endif
	     endif

c Recompute uvw
	     m1 = (rapos(i2)-rapos(i1))/(utrelp(i2)-utrelp(i1))
	     m2 = (dcpos(i2)-dcpos(i1))/(utrelp(i2)-utrelp(i1))
	     w = ut
	     if (w .gt. 86400) w = w - 86400. ! in case ut timestamp > 86400
	     u = rapos(iref) + (utrel - utrelp(iref)) * m1
	     v = dcpos(iref) + (utrel - utrelp(iref)) * m2
	     shift = (utrel-utrelp(iref)) * sqrt(m1**2+(m2*cos(dc0))**2)

c Change flags if not reference
	     if (.not. isref) then
		if (extrap .and. .not.doextrap) then
		   flag = 1
		else if (abs(shift) .gt. maxgap) then
		   flag = 1
		else if (.not. keepflag) then
		   flag = 0
		endif
	     endif
	  endif

c Change flags according to flagtime parameter
	  if (ftime(1) .ge. 0) then
	     if (ftime(1) .le. ftime(2)) then
	        if (ut .gt. ftime(1) .and. ut .lt. ftime(2)) flag = 1
	     else  
		if (ut .lt. ftime(1) .and. ut .lt. ftime(2)) flag = 1
		if (ut .gt. ftime(1) .and. ut .gt. ftime(2)) flag = 1
	     endif
	  endif

	  file = outfile
	  jstat = 0
	  call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)

 900	end do

 999	continue

	file = infile
	jstat = 1
	call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)
        if (jstat.ne.0) THEN
	   write(6,*)  'Error closing input file'
	endif

	file = outfile
	jstat = 1
	call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)
        if (jstat.ne.0) THEN
	   write(6,*)  'Error closing output file'
	endif

	stop
	end


*****************************************************

        subroutine  format_time (uts, tim_str)
        implicit none

        real ut,uts,left
        character*(*) tim_str

        integer ihr, imn, isec, fsec

*-------------------------------------------------

	ut = uts/3600.
        ihr = ut
        left = (ut - ihr) * 60.
        imn = left
        left = (left - imn) * 60.
        isec = left
        fsec = nint((left - isec)*10.)

        if (fsec .eq. 10) then
           isec = isec + 1
           fsec = 0
        endif

        if (isec .ge. 60) then
          isec = isec - 60
          imn = imn + 1
        end if

        if (imn .ge. 60) then
          imn = imn - 60
          ihr = ihr + 1
        end if

        write (tim_str, 10) ihr, imn, isec, fsec
 10     format (I2.2, ':', I2.2, ':', I2.2, '.', I1)

	return
        end

c************************************************************************
        subroutine GetOpt(keepflag,doextrap,noref)
c
        implicit none
        logical keepflag,doextrap,noref
c
c  Determine extra processing options.
c
c------------------------------------------------------------------------
        integer nopt
        parameter(nopt=3)
        character opts(nopt)*8
        logical present(nopt)
        data opts/'keepflag','doextrap','noref'/
        call options('options',opts,present,nopt)
        keepflag = present(1)
        doextrap = present(2)
	noref = present(3)
        return
        end
