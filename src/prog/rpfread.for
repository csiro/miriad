      program rpfread

c= RPFREAD - Output information about an ATNF RPFITS file
c& tw
c: utilities
c+
c       RPFREAD summarises basic information about an RPFITS file.  Less
c       information is reported for ATCA data, which is detected based
c       on the "INSTRUMENT" card.
c
c@ in
c       Input RPFITS file.  No default.
c@ tformat
c       Time format, choose "hms" for HH:MM:SS.S or "raw" for decimal.
c       Default is decimal.
c@ posout
c       Optional text file to write out (u,v,w) values (Mopra/PKS only).
c       These contain the UT time (seconds), RA & DEC position stamps
c       for OTF mapping.  Default is not to write this file.
c@ tsysout
c       Optional text file to write out Tsys values (Mopra/PKS only).
c       These contain the UT time (seconds), systemps, SDO, and GTP
c       values.  Default is not to write this file.  Cannot be used with
c       options=brief or options=header.
c@ options
c       Extra processing options.
c       'brief'  Output a one-line summary for each scan.
c       'header' Output only the source/frequency header information.
c
c$Id: rpfread.for,v 1.12 2010/09/21 23:49:07 cal103 Exp $
c--
c History:
c 19 Feb 2004 by tw.
c minor improvements - 20 Feb
c hms or raw - 24 mar
c apply hms/raw to spectrum timestamp - 6 Jun
c 25jul05 - tw - new code to interpret syscal record
c 12aug05 - tw - distinguish Ref and Src spectra in u,v,w line
c 10sep05 - tw - distinguish flagged and unflagged Ref spectra
c 14jan06 - tw - allow for 2 pol, 2 IF MOPS data
c 11jul06 - tw - rewritten for miriad, MOPS, ATCA data
c 17jul06 - tw - increase precision for UT spectral timestamp
c 27jul06 - tw - fix bug in output call
c 02aug06 - tw - accommodate Parkes MB data, report obstype
c 25sep06 - tw - output az/el every cycle (Mopra)
c 16may07 - tw - detab; change label 900 from GOTO to CONTINUE
c 24nov09 - tw - output Tsys table
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'rpfits.inc'

      integer MAXOPT
      parameter (MAXOPT = 2)
      integer MAXCHAN, MAXPOL
      parameter (MAXCHAN = 8192)
      parameter (MAXPOL = 4)

      logical dohms, isref, atca, brief, header, present(MAXOPT)
      character ctime*10, rastr*12, dcstr*12, fitsfile*80, wuvfmt*10
      character cdash*2, src*16, opts(MAXOPT)*8, posfile*40, tline*80
      character fline*24, versn*80, tsysfile*40, otype*9
      integer jstat, flag, bin, if_no, source_no, baseline
      integer gtpa,gtpb,sdoa,sdob,iant1,iant2
      integer i, j, nsp, nhead, srclen, ncyc
      integer ln, iptr, bufdim, iant, ifno
      integer freq1, bw1, freq2, bw2, nch1, nch2
      integer tun
      real    ut, u, v, w, weight(MAXCHAN*MAXPOL), sc_buffer(1)
      real    rmin, rmax
      real    az, el, par, tsysa, tsysb
      double precision rvis, rvisq
      complex vis(MAXCHAN*MAXPOL)
      equivalence (sc_buffer(1), sc_cal(1,1,1))
      data    opts /'brief','header'/

c functions
      integer   len1
      real      amin1, amax1
      character dangle*12, versan*80
c-----------------------------------------------------------------------
      dohms = .false.
      atca = .false.
      call keyini ()
      call keya ('in', fitsfile, ' ')
      call keya ('tformat', wuvfmt, 'raw')
      call keya ('posout', posfile, '')
      call keya ('tsysout', tsysfile, '')
      if (wuvfmt(1:3).eq.'hms') dohms = .true.
      call options ('options',opts,present,2)
      brief = present(1)
      header = present(2)

      if (.not.brief) then
        versn = versan ('rpfread',
     *                  '$Revision: 1.12 $',
     *                  '$Date: 2010/09/21 23:49:07 $')
      endif

      if (fitsfile.eq.' ') then
         call bug('f','Input file must be given (in=)')
      endif
      if (brief .and. header) then
         call bug('f','Only one option can be given')
      endif
      if (brief .or. header) then
         if (tsysfile.ne.'') then
           call bug('f','No options allowed when outputting Tsys')
         endif
      endif
      call keyfin ()


      ln = len1(fitsfile)
      write (file,'(A)') fitsfile(1:ln)

c Open the log files if requested
      if (posfile.ne.'') then
         write (tline,'(a,a)') 'Writing positions to file ',
     *         posfile(1:len1(posfile))
         call output(tline)
         open(unit=11,file=posfile,status='unknown',err=1000)
      endif
      if (tsysfile.ne.'') then
         write (tline,'(a,a)') 'Writing systemps to file ',
     *         tsysfile(1:len1(tsysfile))
         call output(tline)
         open(unit=12,file=tsysfile,status='unknown',err=1000)
      endif

c Open the RPFITS file

      jstat = -3  ! open and read first header
      call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     *                        flag, bin, if_no, source_no)

      if (jstat.eq.-1) then
        call output(' file open troubles')
        goto 999
      endif

      if (.not.brief) then
         call output(' Reading ' // file)
      endif

      nsp = 0
      ncyc = 0
      nhead = 0

      do while (.true.)

c Try to read data

        jstat = 0
        call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     *                 flag, bin, if_no, source_no)

c CASE 1: HEADER ENCOUNTERED OR END OF FILE

        if (jstat.eq.1 .or. jstat.eq.3) then

          if (nhead.gt.0) then            ! output if not start
             if (nsp.gt.0) ncyc = ncyc + 1
             if (brief) then
                if (ncyc.gt.0) then
                   if (atca) then
                      write(6,'(i4)') ncyc
                   else
                      write(6,'(i3)') ncyc
                   endif
                endif
             else
                if (.not.header) then
                  write(6,*)
                  if (tsysfile.ne.'') write(12,*)
                endif
                write(6,*) 'Number of spectra: ', nsp
                write(6,*) 'Number of cycles: ', ncyc
             endif
          endif
          nhead = nhead + 1

c IF END OF FILE: exit here

          if (jstat.eq.3) then
             if (.not.brief) write(6,*) 'END OF FILE'
             goto 999
          endif

c load the header of the next scan

          jstat = -1
          call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     *                   flag, bin, if_no, source_no)
          if (nhead.eq.1) then
             if (instrument(1:4).eq.'ATCA') atca=.true.
             if (.not.brief) then
                write (6,'(a,a12)') ' UT Date: ',datobs
                write (6,'(a,a12)') ' Instrument: ',instrument
                if (tsysfile.ne.'') then
                   write(12,'(a,6x,a,2x,a,$)') '#UT','Bm','Obs '
                   do i = 1, n_if
                      do j = 1, if_nstok(i)
                        if (j.eq.1) write(12,121) '_A',i,'_A',i,'_A',i
                        if (j.eq.2) write(12,121) '_B',i,'_B',i,'_B',i
                      enddo
                   enddo
                   write(12,*)
                endif
             endif
          endif
121       format(2x,'Tsys',a,i1,2x,'GTP',a,i1,2x,'SDO',a,i1,$)
          if (.not.brief) then
             if (.not.header) write(6,*)
             write(6,99)
             write(6,99)
             write(6,*)
          endif
 99       format('--------------------------------------',$)
          ra  = ra  * DR2D
          dec = dec * DR2D
          rastr = dangle(ra/15.0)
          dcstr = dangle(dec)
          otype = obstype(1:9)
          if (otype(3:3).eq.' ' .and. otype(4:4).ne.' ')
     *       otype(3:3) = '_'

c report some parameters

          if (.not.brief) then
             write (6, 100) su_name(1), rastr(1:11), dcstr(1:11),
     *              intime, otype
             do i = 1, n_if
               write (6,101) i, if_bw(i)/1e6, if_nfreq(i), if_nstok(i)
               write (6,102) if_freq(i)/1e6,if_ref(i),if_sampl(i)
             enddo
             if (.not.header) write (6,*)
          endif
 100      format (' Source: ',a16,' RA ',a11,' DEC ',a11,'  cyc',i3,
     *            's  obs ',a9)
 101      format(' IF ',i2,' bw:',f7.2,' chans:',i5,' pols:',i2,$)
 102      format(' freq: ',f10.3,' @ chan: ',f5.0,'  nbit: ',i1)

c Get frequency info (brief mode)
          freq1 = nint(if_freq(1)/1e6)
          bw1 = nint(if_bw(1)/1e6)
          nch1 = if_nfreq(1)
          if (n_if.gt.1) then
             freq2 = nint(if_freq(2)/1e6)
             bw2 = nint(if_bw(2)/1e6)
             nch2 = if_nfreq(2)
          else if (if_nstok(1).eq.2) then
             freq2 = freq1
             bw2 = bw1
             nch2 = nch1
          else
             freq2 = 0
             bw2 = 0
             nch2 = 0
          endif

c determine if this is a reference obsn
          src = su_name(1)
          srclen = len1(src)
          if (src(srclen-1:srclen).eq.'_R') then
             isref = .true.
          else
             isref = .false.
          endif

          nsp  = 0
          ncyc = 0
          goto 900

        endif         ! jstat.eq.1 or 3

c CASE 2: SYSCAL RECORD

        if (baseline.eq.-1) then    ! SYS cal data
           call format_time(sc_ut,ctime)
           iptr = 1
           bufdim = sc_q * sc_if * sc_ant
c          write(*,*) 'sc_q is ', sc_q, ' bufdim is ',bufdim
c          do i = 1, bufdim
c             write(*,*) i, sc_buffer(i)
c           enddo
c          write(*,*)
           do while (iptr.lt.bufdim)
              iant = (sc_buffer(iptr) + 0.5)
              if (iant.eq.0) then ! Ant 0 record
                 az  = sc_buffer(iptr+1)*DR2D
                 el  = sc_buffer(iptr+2)*DR2D
                 par = sc_buffer(iptr+3)*DR2D
                 if (ifno.eq.1) then
                    if (.not.(atca .or. brief .or. header)) then
                       write(6,104) az,el,par
                    endif
                 endif
              else
                 ifno = nint(sc_buffer(iptr+1))
                 tsysa = sc_buffer(iptr+3)**2
                 tsysb = sc_buffer(iptr+4)**2
                 gtpa = sc_buffer(iptr+18)
                 gtpb = sc_buffer(iptr+20)
                 sdoa = sc_buffer(iptr+19)-sc_buffer(iptr+18)
                 sdob = sc_buffer(iptr+21)-sc_buffer(iptr+20)
                 if (.not.atca .and. .not.brief .and. .not.header) then
                    write(6,105) iant, ifno, 1, tsysa, gtpa, sdoa
                    if (tsysfile.ne.'') then
                        if (ifno.eq.1)
     *                     write(12,109) sc_ut, iant, otype(1:5)
                        write(12,120) tsysa, gtpa, sdoa
                    endif
                    if (if_nstok(ifno).gt.1) then
                       write(6,105) iant, ifno, 2, tsysb, gtpb, sdob
                       if (tsysfile.ne.'') write(12,120) tsysb,gtpb,sdob
                    endif
                 else if (.not.brief .and. .not.header) then
                    write(6,106) iant,sc_ut,ifno,tsysa/10,tsysb/10
                 endif
              endif
              iptr = iptr + sc_q
           enddo
           if (nsp.eq.0 .and. brief) then
              if (atca) then
                 write(6,107) ctime,su_name(1),rastr,dcstr,
     *              freq1,freq2,bw1,nch1,bw2,nch2
              else
                 write(6,108) ctime,su_name(1),rastr,dcstr,int(az),
     *              int(el),freq1,freq2,bw1,bw2,int(tsysa),int(tsysb)
              endif
           endif

 104       format(' Az, El, par ang (deg): ',3f8.2)
 105       format(' Beam',i2,' IF',i2,' Pol ',i1,': Tsys, GTP, SDO: ',
     *            f8.2,i10,i10)
 106       format(' Ant',i2,' UT ',f8.2,', Tsys for chan A,B IF ',
     *            i1,': ',2f8.2)
 107       format(a8,1x,a10,1x,a11,1x,a9,i7,i7,4i5,$)
 108       format(a8,1x,a10,1x,a8,1x,a9,i4,i3,2i7,2i5,2i4,$)
 109       format(f8.2,i3,1x,a5,$)
 120       format(1x,f8.2,i8,i8,$)
           goto 900
        endif

c CASE 3: BAD RECORD

        if (baseline.lt.257) then
          write (6, '('' bad baseline # : '', I5)') baseline
          goto 900
        endif

        if (jstat.eq.5) then
c            write(6,*) 'JSTAT=5 (bad record skipped)'
           if (.not.brief .and. .not.header) then
              write(6,*)
              if (tsysfile.ne.'') write(12,*)
           endif
           ncyc = ncyc + 1
           goto 900
        endif

c CASE 4: SPECTRUM TO READ IN

        nsp = nsp + if_nstok(if_no)
        iant1 = baseline/256
        iant2 = baseline - (256*iant1)

        if (flag.eq.0) then
           cdash = '--'
           if (isref) cdash = '-R'
        else
           cdash = '##'
           if (isref) cdash = '#R'
        endif

        if (dohms) then
           call format_time(ut,ctime)
           write(tline,110) iant1, iant2, if_no, ctime, bin, intbase
        else
           write(tline,114) iant1, iant2, if_no, ut, bin, intbase
        endif
 110    format (' Spectrum for bsln ',i2,'-',i2,' IF ',i2,' at UT ',
     *          a,'  bin ',i2,' inttime',f8.3)
 114    format (' Spectrum for bsln ',i2,'-',i2,' IF ',i2,' at UT ',
     *          f9.3,'  bin ',i2,' inttime',f8.3)
        if (.not.brief .and. .not.header) call output(tline)
        if (atca) goto 900

        if (bin.eq.1 .and. if_no.eq.1) then
           write (fline, 116) cdash, iant1
           if (dohms) then
              call format_time(w,ctime)
              rastr = dangle(dble(u)*DR2D/15d0)
              dcstr = dangle(dble(v)*DR2D)
              write (tline, 111) ctime, rastr, dcstr
           else
              write (tline, 113) w, u*DR2D/15d0, v*DR2D
           endif
           if (.not.brief .and. .not.header) call output(fline//tline)
           if (posfile.ne.'') then
              write (11, 115) w, u*DR2D/15d0, v*DR2D, iant1, iant2
           endif
        endif
 111    format (a,2x,a,2x,a)
 113    format (f11.3,2x,f11.7,2x,f11.7)
 115    format (f11.3,2x,f11.7,2x,f11.7,5x,i2,'-',i2)
 116    format (1x,a2,' Beam',i2,' Fake w,u,v: ')

        if (brief .or. header) goto 900

c Calculate the max, min, mean and rms of the spectrum

        do i = 1, if_nstok(if_no)
          rmin = 9999.0
          rmax = -9999.0
          rvis = 0.0
          rvisq = 0.0
c         write(*,*) 'Pol ',i,' has ',if_nfreq(if_no),' chans'
          do j = 1, if_nfreq(if_no)
            if (if_nstok(if_no).eq.2) then
               rvis = rvis + real(vis(2*j-(2-i)))
               rvisq = rvisq + real(vis(2*j-(2-i)))**2
               rmin = amin1(rmin,real(vis(2*j-(2-i))))
               rmax = amax1(rmax,real(vis(2*j-(2-i))))
            else
               rvis = rvis + real(vis(j))
               rvisq = rvisq + real(vis(j))**2
               rmin = amin1(rmin,real(vis(j)))
               rmax = amax1(rmax,real(vis(j)))
            endif
          enddo
          rvis = rvis / if_nfreq(if_no)
          rvisq = sqrt(rvisq / if_nfreq(if_no) - rvis**2)
          write(6,112) cdash, i,': ',rvis,' rms: ',rvisq,' min: ',rmin,
     *                 ' max: ',rmax
        enddo
 112    format(1x,a2,' Data mean, pol',i2,a,f9.3,a,f9.3,a,f9.3,a,f9.3)
        if (flag.ne.0) then
           write(6,*) 'THIS SPECTRUM IS FLAGGED'
        endif

 900    continue
      enddo


      jstat = 1
      call rpfitsin (jstat, vis, weight, baseline, ut, u, v, w,
     *                        flag, bin, if_no, source_no)

      if (jstat.ne.0) THEN
         write(6,*)  'Error closing file'
      endif
      if (posfile.ne.'') then
         call txtclose(tun)
      endif

 999  continue

      stop
 1000 call bug('f','Unable to create output file')
      stop
      end

c***********************************************************************

      subroutine  format_time (uts, tim_str)

      real    uts
      character*(*) tim_str
c-----------------------------------------------------------------------
      integer ihr, imn, isec, fsec
      real    left, ut
c-----------------------------------------------------------------------
      ut   = uts/3600.0
      ihr  = ut
      left = (ut - ihr) * 60.0
      imn  = left
      left = (left - imn) * 60.0
      isec = left
      fsec = nint((left - isec)*10.0)

      if (fsec.eq.10) then
         isec = isec + 1
         fsec = 0
      endif

      if (isec.ge.60) then
        isec = isec - 60
        imn  = imn + 1
      endif

      if (imn.ge.60) then
        imn = imn - 60
        ihr = ihr + 1
      endif

      write (tim_str, 10) ihr, imn, isec, fsec
 10   format (I2.2, ':', I2.2, ':', I2.2, '.', I1)

      return
      end
