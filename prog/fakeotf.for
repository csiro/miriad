c
c= FAKEOTF - Generate a fake RPFITS file from an existing one
c& tw
c: analysis
c+
c	FAKEOTF reads an RPFITS file and replaces all spectra with a
c       single Gaussian profile.  The amplitude of the Gaussian is either
c       constant across the map (fwhm=0) or falls of as a circular 2-D 
c       Gaussian in the image plane.  A new RPFITS file is produced.
c       Use this to simulate the effects of Gridzilla on Mopra OTF data.
c       Limitation: The same parameters will be used for all IFs.
c
c@ in
c	Input RPFITS file, which should be an OTF map.  It is assumed that
c       the position and spectral timestamps match; if not run MOPFIX first.
c@ out
c	Output RPFITS file.  No default.
c@ specpar
c       Amplitude and sigma of spectral Gaussian, in K and channel units.
c       Default is 1,20 - i.e. 1 K and 20 channels.
c@ centre
c       RA,DEC of centre position for image Gaussian.  No default, but
c       can be omitted if fwhm=0.
c@ fwhm
c       FWHM of image Gaussian in arcsec (default 30). Give 0 for flat plane.
c@ tsys
c       System temps for channels A and B respectively.  If only one value
c       is given, this is used for both channels.  This is also used to 
c       determine the amount of noise in each spectrum.  Default is to use
c       the original Tsys measurements from the input file.
c       
c 21jun05 - tw - created
c 20jul06 - tw - clean up for MOPS and miriad

	program fakeotf
	implicit none

	include 'rpfits.inc'

	double precision pi
	parameter (pi = 3.1415 92653 58979 3238 d0)
	integer MAXCHAN
	parameter (MAXCHAN=8192)

	character ctime*10, rastr*12, dcstr*12, fitsfile*80
	character infile*80, outfile*80, postab*20, src*16
	integer jstat, flag, bin, if_no, source_no, baseline
	real m1, m2, dra, ddc, dc0, utrel, midtime
	real ut, weight(2*MAXCHAN), u, v, w, sc_buffer(1)
	real width, amp0, amp, midch, func, imfwhm, noise,tsys(2),rms(2)
	double precision ractr, dcctr, imsig, dist2
	complex vis(2*MAXCHAN)
	integer az, el, par, tsysa, tsysb
	integer i, i1, i2, iref, j, nsp, nhead, ncyc
	integer ln, npos, bw1, bw2, freq1, freq2, srclen
	logical isref, redotsys
	integer iptr, bufdim, iant, numif
	equivalence ( sc_buffer(1), sc_cal(1,1,1) )

* functions
	integer len1
	character*12 rangle,hangle

        character*50 vers
        parameter ( vers = 'FAKEOTF: version 20-jul-2006' )
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
	   call bug('f','Output file must be given (out=)')
	endif
c Centre for Gaussian source
	call keyt ('centre', ractr, 'hms', 0.d0)
	call keyt ('centre', dcctr, 'dms', 0.d0)
c FWHM of image Gaussian in arcsec
	call keyr ('fwhm', imfwhm, 30.)
c Width and peak amplitude of Gaussian line profile
	call keyr ('specpar', amp0, 1.)
	call keyr ('specpar', width, 20.)
c Width and peak amplitude of Gaussian line profile
	call keyr ('tsys', tsys(1), 0.)
	call keyr ('tsys', tsys(2), tsys(1))
	call keyfin ()

*-------------------------------------------------------

c Decide whether to redo Tsys
	redotsys = (tsys(1).gt.0)

c Convert imfwhm to sigma in radians
	imsig = imfwhm / (2.355*206265.)

c This card is needed for LiveData
	ncard = 1 
	card(1) = 'OBSTYPE'

c Open the RPFITS file

	file = infile
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
	    if (nhead .gt. 0) then
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

c and write it to the output file
	    file = outfile
	    jstat = -1
	    call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)

c report some parameters

	    rastr = hangle(ra)
	    dcstr = rangle(dec)

c -- figure out observation type
	    src = su_name(1)
	    srclen = len1(src)
	    if (src(srclen-1:srclen).eq.'_R') then
	       isref = .true.
	    else
	       isref = .false.
	    endif

c get frequency info from header
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
		   numif = nint(sc_buffer(iptr+1))
		   tsysa = nint(sc_buffer(iptr+3)**2)
		   tsysb = nint(sc_buffer(iptr+4)**2)
c Reset the Tsys values if requested
		   if (redotsys) then
		      sc_buffer(iptr+3) = sqrt(tsys(1))
		      sc_buffer(iptr+4) = sqrt(tsys(2))
		   else
		      tsys(1) = sc_buffer(iptr+3)**2
		      tsys(2) = sc_buffer(iptr+4)**2
		   endif
c Unflag records flagged by online system
		   if (nint(sc_buffer(iptr+12)).eq.1) then
		      sc_buffer(iptr+12) = 0.
		   endif
		else if (iant .eq. 0) then ! Ant 0 record
		   az  = nint(sc_buffer(iptr+1)*180./pi)
		   el  = nint(sc_buffer(iptr+2)*180./pi)
		endif
		iptr = iptr + sc_q
	     enddo
	     if (numif.eq.1 .and. nsp.eq.0) then
		write(6,104) ctime,su_name(1),rastr,dcstr,az,el,
     *             freq1,freq2,bw1,bw2,tsysa,tsysb
	     endif
 104	     format(a8,1x,a10,1x,a10,1x,a9,i4,1x,i2,1x,i6,1x,i6,
     *             1x,i3,1x,i3,2i4,$)

	     file = outfile
	     jstat = 0
	     call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)

	     goto 900
	  endif

c CASE 3: CLOSE/REOPEN OPERATION

	  if (jstat .eq. 5) then
	     file = outfile
	     jstat = 3
	     call rpfitsout (jstat, vis, weight, baseline, ut, u, v, w,
     :                        flag, bin, if_no, source_no)
	     goto 900
	  endif
	  
c CASE 4: DATA IN SPECTRUM

	  nsp = nsp + 1

c Calculate model line profile

	  if (.not. isref) then
	     if (imfwhm .gt. 1e-10) then
		dist2 = ((ractr-u)*cos(dcctr))**2 + (dcctr-v)**2
		amp = amp0*exp(-dist2/(2.*imsig**2))
	     else
		amp = amp0
	     endif
	     midch = if_nfreq(if_no)/2.
	  endif

c Replace data with model

	  do i = 1, if_nstok(if_no)
c	     write(*,*) 'Pol ',i,' has ',if_nfreq(if_no),' chans'
	     rms(i) = tsys(i)/sqrt(intime*if_bw(if_no)/if_nfreq(if_no))
c	     write(*,*) 'The rms for pol ',i,' is ',rms(i)
	     do j = 1, if_nfreq(if_no)
		call gaus(noise,1)
		if (.not. isref) then
		   func = (j-midch)**2/(2*width**2)
		   func = tsys(i) + amp*exp(-func) + noise*rms(i)
		else
		   func = tsys(i) + noise*rms(i)
		endif
		if (if_nstok(if_no) .eq. 2) then
		   vis(2*j-(2-i)) = cmplx(func)
		else
		   vis(j) = cmplx(func)
		endif
	     enddo
	  enddo

c Unset flags
	  if (flag .ne. 0) then
	     flag = 0
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
 1000	write(*,*) 'Error opening ',postab
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

