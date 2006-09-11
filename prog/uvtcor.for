      program uvtcor
c--------------------------------------------------------------------------
c     UVTCOR redoes the Tsys correction by constructing a median
c     smoothed Tsys curve for each specified mosaiced field, 
c     polarization, antenna and frequency.
c
c= uvtcor - Redo the Tsys correction, using a median smoothed Tsys curve
c	     
c& nebk
c: calibration
c+
c     UVTCOR redoes the Tsys correction by constructing a median
c     smoothed Tsys curve for each specified mosaiced field, 
c     polarization, antenna and frequency.
c
c@ vis
c	Input visibility file. No default.
c@ out
c	Output visibility file.  No default.
c@ freq
c	Band centre frequencies of interest in MHz. No default.
c@ source
c	Root name of mosaiced field.  E.g. "cena_" with fields like
c	cena_1, cena_12  etc.  No default.
c@ fields
c	Integer range of fields such as 1,20  No default.
c@ device
c	PGPLOT device.  No default.
c@ options
c	tsysm   means when undoing the Tsys correction, use the "xtsysm" 
c	  and "ytsysm" variables instead of "xtsys" and "ytsys".  This 
c	  would be appropriate if you have already run UVTCOR once, and 
c	  want to redo something.  It will always median smooth the 
c	  "xtsys"  and "ytsys" variables
c	
c	first   means automatically edit out the first cycle of each
c	  visit to each field before determining the median Tsys curve
c--
c
c     nebk 20mar95 Original version
c     nebk 11apr95 Add check for tsys variables
c
c  Bugs
c    Assumes linear polarizations
c
c---------------------------------------------------------------------------
      include 'maxdim.h'
      integer frqmax, fldmax, polmax, antmax, timemax, allmax
      character version*(*)
      parameter(version='version 11-Apr-95', frqmax=2,
     +          polmax = 2, fldmax = 200, antmax=6, timemax=300,
     +          allmax=fldmax*antmax*polmax*frqmax)
c
      character vis*64, out*64, roots*20, source*20, type*1, device*60
      real time(timemax,fldmax,antmax,polmax,frqmax), 
     +  tsys(timemax,fldmax,antmax,polmax,frqmax),
     +  vtsys(maxant*maxwin*2), mtsys(maxant*maxwin*2),
     +  w1(timemax), w2(timemax), w3(timemax)
      integer ntsys(fldmax,antmax,polmax,frqmax)
c
      integer nf, nread, tvis, tout, freqs(frqmax),
     +  i, j, k, l, ifield1, ifield2, ir, ip, ip2, it, it1, it2,
     +  ifreq, ia1, ia2, ifield, ipol1, ipol2, npols, pol,
     +  nants, nspect, nschan(maxwin), jd0, ii, ierr, length
      double precision sfreq(maxwin), sdf(maxwin), preamble(4),
     + jd, jdold
      complex data(maxchan)
      logical flags(maxchan), corr, keep, ok1, ok2, dom, updated,
     +  readmv, first
c
      integer len1, pgbeg
      character itoaf*4
      data ntsys /allmax*0.0/
c------------------------------------------------------------------------
c
c Get the input parameters
c
      call output ('UvTCor: '//version)
      call keyini
      call keya ('vis', vis, ' ')
      call keya ('out', out, ' ')
      if (vis.eq.' ' .or. out.eq.' ') call bug ('f',
     +    'Input and output must both be given') 
      call mkeyi ('freq', freqs, frqmax, nf)
      if (nf.eq.0) call bug ('f', 'Must give frequencies')
      call keya ('source', roots, ' ')
      if (roots.eq.' ') call bug ('f', 'No source name given')
      ir = len1(roots)
      call keyi ('fields', ifield1, 1)
      call keyi ('fields', ifield2, ifield1)
      if (ifield1.le.0 .or. ifield2.le.0) 
     +  call bug ('f', 'Invalid field range')
      if (ifield2-ifield1+1.gt.fldmax) 
     +  call bug ('f', 'Too many fields, max='//itoaf(fldmax))
      call keya ('device', device, ' ')
      if (device.eq.' ') call bug ('f', 'No plotting device given')
      call getopt (dom, first)
      call keyfin
c
c Read through data and generate arrays of Tsys for each antenna,field,
c polarization and frequency of interest
c
      call output ('Generating Tsys arrays')
      call uvopen (tvis, vis, 'old')
      call varinit (tvis, 'channel')
c
c Read data
c
      jdold = 0.0d0
      call uvread (tvis, preamble, data, flags, maxchan, nread)
      call uvprobvr (tvis,'xtsys', type, length, updated)
      if (.not.dom .and. type.ne.'r') call bug ('f',
     +   'xtsys variable not in data')
      call uvprobvr (tvis,'ytsys', type, length, updated)
      if (.not.dom .and. type.ne.'r') call bug ('f',
     +   'ytsys variable not in data')
c
      do while (nread.gt.0) 
c
c Get variables of interest
c
        call uvgetvri (tvis, 'nants', nants, 1)
        call uvgetvra (tvis, 'source', source)
        call uvgetvri (tvis, 'nspect', nspect, 1)
        call uvgetvrd (tvis, 'sfreq', sfreq, nspect)
        call uvgetvrd (tvis, 'sdf', sdf, nspect)
        call uvgetvri (tvis, 'nschan', nschan, nspect)
        call uvgetvrr (tvis, 'xtsys', vtsys, nants*nspect)
        call uvgetvrr (tvis, 'ytsys', vtsys(nants*nspect+1), 
     +                 nants*nspect)
        if (jdold.le.0.0d0) jd0 = int(preamble(3) + 0.5d0)
        jd = preamble(3) + 0.5d0 - jd0
c
c If the time has changed, its a new integration, so see if this
c is one of the desired mosaic fields 
c
        if (jd.gt.jdold .and. index(source,roots(1:ir)).ne.0) then
c
c Loop over spectral windows and get pointers into Tsys time arrays
c and out of Tsys variable array
c
          ip = 1
          do k = 1, nspect
            call point1 (nschan(k), sdf(k), sfreq(k), source, nf, 
     +         freqs, ifield1, ifield2, roots(1:ir), ifreq, 
     +         ifield, keep)
            if (keep) then
              do j = 1, 2
                do i = 1, nants
                  it = (j-1)*nants*nspect + (k-1)*nants + i
                  ii = ntsys(ifield,i,j,ifreq)
                  if (ii.eq.timemax) 
     +              call bug ('f', 'Too many integrations for '//
     +                 'internal buffers, max='//itoaf(timemax))
c
                  ii = ii + 1
                  ntsys(ifield,i,j,ifreq) =ii
                  time(ii,ifield,i,j,ifreq) = jd*86400.0
                  tsys(ii,ifield,i,j,ifreq) = vtsys(it)
                end do
              end do
            end if
          end do
        end if
        jdold  = jd
c
c Get next vis
c
        call uvread (tvis, preamble, data, flags, maxchan, nread)
      end do
      call uvclose (tvis)
c
c Now prepare median smoothed arrays
c
      call output ('Prepare median smoothed arrays')
      ierr = pgbeg (0, device, 1, 1)
      call pgask (.false.)
      do l = 1, nf
        do k = 1, 2
          do i = ifield1, ifield2
            do j = 1, nants
              call medsm (first, freqs(l), k, j, i, ntsys(i,j,k,l),
     +          time(1,i,j,k,l), tsys(1,i,j,k,l), w1, w2, w3)
            end do
          end do
        end do
      end do
      call pgend
c
c Now correct the data with the median smoothed Tsys arrays
c
      call output ('Begin correcting visibilities')
      call uvopen (tvis, vis, 'old')
      call uvopen (tout, out, 'new')
      call varinit (tvis, 'channel')
      call varonit (tvis, tout, 'channel')
c
c Read data
c
      jdold = 0.0d0
      call uvread (tvis, preamble, data, flags, maxchan, nread)
      call uvprobvr (tvis,'xtsysm', type, length, updated)
      readmv = .false.
      if (type.eq.'r') readmv = .true.
c
      do while (nread.gt.0) 
c
c Get variables of interest
c
        call uvgetvri (tvis, 'nants', nants, 1)
        call uvgetvri (tvis, 'npol', npols, 1)
        call uvgetvri (tvis, 'pol', pol, 1)
        call uvgetvra (tvis, 'source', source)
        call uvgetvri (tvis, 'nspect', nspect, 1)
        call uvgetvrd (tvis, 'sfreq', sfreq, nspect)
        call uvgetvrd (tvis, 'sdf', sdf, nspect)
        call uvgetvri (tvis, 'nschan', nschan, nspect)
        call uvprobvr (tvis,'xtsys', type, length, updated)
        call uvgetvrr (tvis, 'xtsys', vtsys, nants*nspect)
        call uvgetvrr (tvis, 'ytsys', vtsys(nants*nspect+1), 
     +                 nants*nspect)
c
c Read median Tsys variables if there
c
        do i = 1, nants*nspect*2
          mtsys(i) = 0.0
        end do
c 
        if (readmv) then
          call uvgetvrr (tvis, 'xtsysm', mtsys, nants*nspect)
          call uvgetvrr (tvis, 'ytsysm', mtsys(nants*nspect+1),
     +                    nants*nspect)
        end if
c
c Set time
c
        if (jdold.le.0.0d0) jd0 = int(preamble(3) + 0.5d0)
        jd = preamble(3) + 0.5d0 - jd0
c
c Get pointers to median smoothed Tsys arrays not depending 
c on spectral window
c
        call point2 (preamble(4), pol, source, ifield1, ifield2,
     +    roots, ia1, ia2, ifield, ipol1, ipol2, corr)
c
c Loop over spectral windows and correct data
c
        ip = 1
        do i = 1, nspect
c
c Get frequency pointer to median smoothed Tsys arrays
c
          call point3 (nschan(i), sdf(i), sfreq(i), nf, 
     +                 freqs, ifreq, corr)
c
c Correct data if required
c
          if (corr) then
            it1 = (ipol1-1)*nants*nspect + (i-1)*nants + ia1
            it2 = (ipol2-1)*nants*nspect + (i-1)*nants + ia2
c
            if (dom) then
c
c This program already applied, undo assuming Tsys that has been
c applied is that stored in the median variables
c
              call tcor (ntsys(ifield,ia1,ipol1,ifreq),
     +                   ntsys(ifield,ia2,ipol2,ifreq),
     +                   time(1,ifield,ia1,ipol1,ifreq), 
     +                   time(1,ifield,ia2,ipol2,ifreq), 
     +                   tsys(1,ifield,ia1,ipol1,ifreq), 
     +                   tsys(1,ifield,ia2,ipol2,ifreq),
     +                   jd*86400.0d0, nschan(i), data(ip), 
     +                   mtsys(it1), mtsys(it2), ok1, ok2)
            else
c
c This program not applied, undo assuming Tsys that has been
c applied is that stored in the normal tsys variables

              call tcor (ntsys(ifield,ia1,ipol1,ifreq),
     +                   ntsys(ifield,ia2,ipol2,ifreq),
     +                   time(1,ifield,ia1,ipol1,ifreq), 
     +                   time(1,ifield,ia2,ipol2,ifreq), 
     +                   tsys(1,ifield,ia1,ipol1,ifreq), 
     +                   tsys(1,ifield,ia2,ipol2,ifreq),
     +                   jd*86400.0d0, nschan(i), data(ip), 
     +                   vtsys(it1), vtsys(it2), ok1, ok2)
            end if
c
            if (.not.ok1) then
              write (*,*) 'Interpolation failure'
              write (*,*) 'time,ispec,ipol,iant=',jd,i,ipol1,ia1
            end if
            if (.not.ok2) then
              write (*,*) 'Interpolation failure'
              write (*,*) 'time,ispec,ipol,iant=',jd,i,ipol1,ia1
            end if
          end if
c
c Generate array of median smoothed Tsys for writing out
c to new variables "xtsysm" and "ytsysm". Only do this
c when a new cycle begins
c
          if ((jd-jdold)*86400.0d0.gt.1.0d0) then
            if (ifreq.ne.0 .and. ifield.ne.0) then
c
c Generate interpolated values 
c
              do j = 1, 2
                do k = 1, nants
                  ip2 = (j-1)*nants*nspect + (i-1)*nants + k
                  call linint (real(jd*86400.0d0), 
     +                       ntsys(ifield,k,j,ifreq),
     +                       time(1,ifield,k,j,ifreq),
     +                       tsys(1,ifield,k,j,ifreq), 
     +                       mtsys(ip2), ok1)
                  if (.not.ok1) then
                    write (*,*) 
     +                'Interpolation failure for "tsysm" variable'
                    write (*,*) 'time,ispec,ipol,iant=',jd,i,j,k
                  end if
                end do
              end do
            end if
          end if
c
          ip = ip + nschan(i)
        end do
c
c Write out new data
c
        call varcopy (tvis, tout)
        call uvputvri (tout, 'npol', npols, 1)
        call uvputvri (tout, 'pol', pol, 1)
        if ((jd-jdold)*86400.0d0.gt.1.0d0) then
          call uvputvrr (tout, 'xtsys', vtsys, nants*nspect)
          call uvputvrr (tout, 'ytsys', vtsys(nants*nspect+1), 
     +                   nants*nspect)
c
c If these variables already existed, we either write out the
c new values, or whatever was already there (maybe zero).
c
          call uvputvrr (tout, 'xtsysm', mtsys, nants*nspect)
          call uvputvrr (tout, 'ytsysm', mtsys(nants*nspect+1), 
     +                   nants*nspect)
        end if
        call uvwrite (tout, preamble, data, flags, nread)
c
        jdold = jd
c
c Get next vis
c
        call uvread (tvis, preamble, data, flags, maxchan, nread)
      end do
c
c  Make the history of the output and close up shop.
c
      call hdcopy (tvis, tout, 'history')
      call hisopen (tout, 'append')
      call hiswrite (tout, 'UVTCOR: Miriad UvTCor '//version)
      call hisinput (tout, 'UVTCOR')
      call hisclose (tout)
c
      call uvclose (tvis)
      call uvclose (tout)
c
      end
c
c
      subroutine point1 (nchan, sdf, sfreq, source, nf, freqs, ifield1, 
     +                   ifield2, sroot, ifreq, ifield, keep)
c-----------------------------------------------------------------------
c  From this visibility, work out some pointers into the appropriate
c  Tsys text file
c       
c  Input
c    nchan       NUmber of channels
c    sdf         Channel frequency increment (GHz)
c    sfreq       Frequency pf channel 1 (GHz)
c    source      Source name for this visibility
c    nf          Number of frequenices given by user
c    freqs       Frequencies given by user (MHz)
c    ifield1,2   Range of field numbers given by user to work on
c    sroot       Root mosaic field name given by user. 
c  Output
c    ifreq       Frequency pointer
c    ifield      Field pointer
c   
c    keep        True to keep this record
c-----------------------------------------------------------------------
      implicit none
      integer nchan, ifreq, ifield, nf, freqs(nf), ifield1, ifield2
      double precision sdf, sfreq
      character sroot*(*), source*(*)
      logical keep
cc
      integer ip, i, il1, il2
      double precision freq
      logical ok
      integer len1
c-----------------------------------------------------------------------
      keep = .false.
c
c Frequency
c
      ip = nchan/2 + 1
      freq = (ip - 1)*sdf + sfreq
      freq = freq * 1000.0
      ifreq = 0
      do i = 1, nf
c
c Check tolerance of central frequency to nearest 0.25MHz
c as central frequency must be set in steps of 1MHz
c
        if (abs(freqs(i)-freq).le.0.25) ifreq = i
      end do
      if (ifreq.eq.0) return
c
c Field
c
      ifield = 0
      il1 = len1(source)
      il2 = len(sroot)
      ip = index(source,sroot(1:il2))
      if (ip.ne.0) then
        ip = il2 + 1
        call atoif(source(ip:il1), ifield, ok)
      end if
      if (ifield.lt.ifield1 .or. ifield.gt.ifield2) ifield = 0
c
      if (ifield*ifreq.ne.0) keep = .true.
c
      end
c
c
      subroutine point2 (baseln, pol, source, ifield1, ifield2, sroot, 
     + ia1, ia2, ifield, ipol1, ipol2, corr)
c-----------------------------------------------------------------------
c  From this visibility, work out spectral windwo independent
c  pointers into the median arrays
c       
c  Input
c    baseln      Baseline number
c    pol         Polarization code (-5 -> -6)
c    source      Source name for this visibility
c    ifield1,2   Range of field numbers given by user to work on
c    sroot       Root mosaic field name given by user. 
c  Output
c                Median smoothed arrays are in order (ifield,iant,ipol,ifreq)
c                If the pointer is 0, then this visibility is
c                not to be corrected, just copied
c    ipol1,2     Polarization pointer for each antenna
c    ia1,2       Antenna pointers
c    ifield      Field pointer
c   
c    corr        True to correct this visibility, else just 
c                copy to output
c-----------------------------------------------------------------------
      implicit none
      integer pol, ia1, ia2, ifield, ipol1, ipol2, ifield1, ifield2
      double precision baseln
      character sroot*(*), source*(*)
      logical corr
cc
      integer ip, il1, il2
      logical ok
      integer len1
c-----------------------------------------------------------------------
c
c Polarization, XX,YY,XY,YX
c
      ipol1 = 0
      ipol2 = 0
      if (pol.eq.-5) then
        ipol1 = 1
        ipol2 = 1        
      else if (pol.eq.-6) then
        ipol1 = 2
        ipol2 = 2
      else if (pol.eq.-7) then
        ipol1 = 1
        ipol2 = 2
      else if (pol.eq.-8) then
        ipol1 = 2
        ipol2 = 1
      else
        call bug ('f', 'Expecting linear polarizations in data')
      end if
c
c Antennas
c
      ia1 = 0
      ia2 = 0
      call basant (baseln, ia1, ia2)
c
c Field
c
      ifield = 0
      il1 = len1(source)
      il2 = len1(sroot)
      ip = index(source,sroot(1:il2))
      if (ip.ne.0) then
        ip = il2 + 1
        call atoif(source(ip:il1), ifield, ok)
      end if
      if (ifield.lt.ifield1 .or. ifield.gt.ifield2) ifield = 0
c
      corr = .true.
      if (ifield*ipol1*ipol2*ia1*ia2.eq.0) corr = .false.
c
      end
c
c
      subroutine point3 (nchan, sdf, sfreq, nf, freqs, ifreq, corr)
c-----------------------------------------------------------------------
c  From this visibility, work out spectral windwo dependent
c  pointers into the median arrays
c       
c  Input
c    nchan       NUmber of channels
c    sdf         Channel frequency increment (GHz)
c    sfreq       Frequency pf channel 1 (GHz)
c    nf          Number of frequenices given by user
c    freqs       Frequencies given by user (MHz)
c  Output
c                median smoothed arrays are in order (ifield,iant,ipol,ifreq)
c                If the pointer is 0, then this visibility is
c                not to be corrected, just copied
c    ifreq       Frequency pointer
c  Input/output
c    corr        If true, correct data
c-----------------------------------------------------------------------
      implicit none
      integer nchan, ifreq, nf, freqs(nf)
      double precision sdf, sfreq
      logical corr
cc
      integer i, ip
      double precision freq
c-----------------------------------------------------------------------
c
c Frequency
c
      ip = nchan/2 + 1
      freq = (ip - 1)*sdf + sfreq
      freq = freq * 1000.0
      ifreq = 0
      do i = 1, nf
c
c Check tolerance of central frequency to nearest 0.25MHz
c as central frequency must be set in steps of 1MHz
c
        if (abs(freqs(i)-freq).le.0.25) ifreq = i
      end do
      if (ifreq.eq.0) corr = .false.
c
      end
c
c
      subroutine tcor (n1, n2, time1, time2, tsys1, tsys2, time,
     +                 nchan, data, vtsys1, vtsys2, ok1, ok2)
c-----------------------------------------------------------------------
c   This subroutine undoes the on-line Tsys correction and reapplies
c   it using the Tsys values in the smoothed Tsys text files
c
c
c  Input
c    n1,n2        Number of time stamps for the following arrays
c    time1,tsys1  These are arrays of time and Tsys for the
c                 appropriate polarization,antenna,frequency and
c                 field for the FIRST antenna involved in the visibility
c    time2,tsys2  These are arrays of time and Tsys for the
c                 appropriate polarization,antenna,frequency and
c                 field for the SECOND antenna involved in the visibility
c    time         The time of the visibility (seconds)
c    nchan        Number of channels to correct
c    vtsys1,2     Online Tsys values for the two antennas for 
c                 this visibility
c  Input/output
c    data         Visibility
c-----------------------------------------------------------------------
      implicit none
      integer n1, n2, nchan
      real time1(n1), time2(n2), tsys1(n1), tsys2(n2), vtsys1, vtsys2
      complex data(nchan)
      double precision time
      logical ok1, ok2
cc
      real tint1, tint2, fac
      integer i
c-----------------------------------------------------------------------
c
c Linearly interpolate the Tsys measurements from the median
c smoothed array
c 
      call linint (real(time), n1, time1, tsys1, tint1, ok1)
      call linint (real(time), n2, time2, tsys2, tint2, ok2)
c
c Now redo the correction
c
      do i = 1, nchan
        fac = sqrt(tint1*tint2/(vtsys1*vtsys2))
        data(i) = fac*data(i)
      end do
c
      end
c
c
      subroutine linint (xx, n, x, y, yy, ok)
c-----------------------------------------------------------------------
c  Linearly interpolate Tsys measurement
c
c  Input
c     xx      Find the Tsys for this time
c     n,x,y   Number of points, time and Tsys
c  Output
c     yy      Interpolated value
c-----------------------------------------------------------------------
      implicit none
      integer n
      real xx, yy, x(n), y(n)
      logical ok
cc
      integer i1, i2, i, ip
      real m, b, xmin, xdiff
c-----------------------------------------------------------------------
c
c Find points that bracket the one of interest
c
      i1 = 0
      i2 = 0
      if (xx.le.x(1)) then
        i1 = 1
        i2 = 2
      else if (xx.ge.x(n)) then
        i1 = n-1
        i2 = n
      else
        do i = 1, n
           if (x(i).gt.xx) then
             i1 = i - 1
             i2 = i 
            goto 10
          end if
        end do
      end if
c
10    if (i1.eq.0 .or. i2.eq.0) then
c
c Interpolation in a tangle, give it the
c nearest value
c
        ok = .false.
        xmin = 1.0e30
        do i = 1, n
          xdiff = abs(x(i) - xx)
          if (xdiff.lt.xmin) then
            ip = i
            xmin = xdiff
          end if
        end do
        yy = y(ip)
      else
c
c Do linear interpolation
c
        ok = .true.
        m = (y(i2)-y(i1)) / (x(i2)-x(i1))
        b = y(i1) - m*x(i1)
        yy = m*xx + b
      end if
c
      end
c
c
      subroutine medsm (dfirst, freq, ipol, iant, ifield, n, x, 
     +                  y, yt, xm, ym)
c-----------------------------------------------------------------------
c  Generate median smoothed Tsys
c
c Input
c   dfirst                   Drop first cycle of each visit
c   freq,iant,ipol,ifield    Which Tsys curve is this ?
c Input/output
c   n,x,y                    Number of points, time, Tsys
c Scratch
c   yt,xm,ym
c 
c-----------------------------------------------------------------------
      integer n, ipol, iant, ifield, nm, freq
      real x(n), y(n), yt(n), xm(n), ym(n)
      logical dfirst
cc
      real ymed, xsum , xx, yy, xold
      integer i, j, k, l, is, ie, ioff, ivis
      logical done, more, first
      character*1 ch*1 
      data xx, yy /0.0, 0.0/
c-----------------------------------------------------------------------
      call output (' ')
      call output ('Begin new time series')
c
c Drop first cycle if desired
c
      if (dfirst) then
        xold = 0.0
        j = 0
        do i = 1, n
          if (x(i)-xold.lt.60.0) then
            j = j + 1
            x(j) = x(i)
            y(j) = y(i)
          end if
          xold = x(i)
        end do
        n = j
      end if
c
c Find median value of entire array and reject obviously bad points
c
      do i = 1, n
        yt(i) = y(i)
      end do
      call median (yt, n, ymed)
      j = 0
      do i = 1, n
        if (abs(y(i)-ymed).lt.10.0) then
          j = j + 1
          x(j) = x(i)
          y(j) = y(i)
        end if
      end do
      write (*,*) 'Rejected ', n-j, ' bad points'
      n = j
c
c Plot data
c
      call plot (n, x, y, ifield, iant, ipol, freq)
c
c Go around median/edit loop 
c
      more = .true.
      do while (more)
c
c We may want to try more than one median
c
        first = .true.
        done = .false.
        ivis = 11
        do while (.not.done)
          if (.not.first) then
            write (*,*) 
     +        'Enter odd number of median smooth points, def =', ivis
            read (*,*) ivis
          end if
          first = .false.
c            
          ioff = ivis/2
          l = 1
          do i = 1, n
            is = max(1,i-ioff)
            ie = min(n,i+ioff)
            k = 1
            xsum = 0.0
            do j = is, ie
              yt(k) = y(j)
              xsum = xsum + x(j)
              k = k + 1
            end do
            call median (yt, ie-is+1, ym(l))
            xm(l) = xsum/real(ie-is+1)
            l = l + 1
          end do
          nm = l - 1
          call pgline (nm, xm, ym)
          call output ('Another smoothing length ?')
          call output ('Click left for yes, right for no')
          call pgcurs (xx, yy, ch)
          if (ch.eq.'x' .or. ch.eq.'X') done = .true.
        end do
c
c Edit
c  
        call output (' ')
        call output ('Click left to edit, right to finish')
        call pgcurs (xx, yy, ch)
        if (ch.ne.'x' .and. ch.ne.'X') then
          call output ('Edit options are')
          call output ('left    = nearest point')
          call output ('middle  = clip below')
          call output ('enter f = first cycle')
          call output ('right   = exit')
          call plot (n, x, y, ifield, iant, ipol, freq)
          call edit (n, x, y)
          call plot (n, x, y, ifield, iant, ipol, freq)
        else
          more = .false.
        end if
      end do
c
c Copy back
c
      do i = 1, nm
        x(i) = xm(i)
        y(i) = ym(i)
      end do
      n = nm
c
      end
c
c
      subroutine plot (n, x, y, ifield, iant, ipol, freq)
c-----------------------------------------------------------------------
c  Plot up the Tsys curve and label it meaningfully
c
c-----------------------------------------------------------------------
      implicit none
c
      integer n, ifield, iant, i, ipol, freq
      real x(n), y(n)
cc
      real ymin, ymax, xl, x2, y1, y2
      character aline*80, cpol*1
c----------------------------------------------------------------------- 
c
c Get limits
c
      call pgvstd
      xl = x(1) - 0.05*(x(n)-x(1))      
      x2 = x(n) + 0.05*(x(n)-x(1))      
      ymin=1.0e30
      ymax=-1.0e30
      do i=1,n
        ymin=min(ymin,y(i))
        ymax=max(ymax,y(i))
      end do
      y1 = ymin - 0.05*(ymax-ymin)
      y2 = ymax + 0.05*(ymax-ymin)
c
c Plot
c
      call pgswin (xl, x2, y1, y2)
      call pgpage
      call pgtbox ('BCNSTZ', 0.0, 0, 'BCNST', 0.0, 0)
      call pgpt (n, x, y, 1)
      cpol = 'X'
      if (ipol.eq.2) cpol = 'Y'
      write (aline, 200) freq, cpol, ifield, iant
200   format (' Freq. ', i4, ' MHz', '   Pol ', a1,
     +        '   Field ', i2, '    Antenna ', i2)
      call pglabel ('UT', 'Tsys', aline)
c
      end
c
c
      subroutine edit (n, x, y)
c-----------------------------------------------------------------------
c  Do some interactive editing of the Tsys curve.  Options
c  to delete the nearest point, clip below a level,
c  and flag the first point of each visit to the field
c-----------------------------------------------------------------------
      implicit none
      integer n, ip,maxdim
      parameter (maxdim=500)
      integer ilist(maxdim), i, j, n2
      real x(n), y(n), xx, yy, rad, rmin, oldx
      character ch*1
c-----------------------------------------------------------------------
      do i = 1, maxdim
        ilist(i) = 0
      end do
      ch= 'A'
      j = 0
      do while (ch.ne.'X' .and. ch.ne.'x')
        call pgcurs (xx,yy,ch)
        if (ch.eq.'A' .or. ch.eq.'a') then
c
c Delete nearest point
c
          rmin = 1.0e30
          ip = 0
          do i = 1, n
            if (ilist(i).eq.0) then
              call radius (xx, yy, x(i), y(i), rad)
              if (rad.lt.rmin) then   
                ip = i
                rmin = rad
              end if
            end if
          end do
          if (ip.gt.0) then
            call pgsci (0)
            call pgpt (1, x(ip), y(ip), 1)
            ilist(ip) = -1
            j = j + 1
          end if
        else if (ch.eq.'D' .or. ch.eq.'d') then
c
c Clip all below this level
c
          do i = 1, n
            if (ilist(i).eq.0) then
              if (y(i).lt.yy) then
                call pgsci (0)
                call pgpt (1, x(i), y(i), 1)
                ilist(i) = -1
                j = j + 1
              end if
            end if         
          end do
        else if (ch.eq.'F' .or. ch.eq.'f') then
c
c Flag first cycle of group by looking for more than a minute
c since last integration
c
          oldx = 0.0
          do i = 1, n
            if (x(i)-oldx.gt.60.0) then
              call pgsci (0)
              call pgpt (1, x(i), y(i), 1)
              ilist(i) = -1
              j = j + 1
            end if
            oldx = x(i)
          end do
        end if
      end do
      call pgsci (1)
      n2 = j
c
c Eradicate the flagged points
c
      if (n2.gt.0) then
        j = 1
        do i = 1, n
          if (ilist(i).ne.-1) then
            x(j) = x(i)
            y(j) = y(i)
            j = j + 1
          end if
        end do
        n = j - 1
      end if
c
      end
c
c   
      subroutine radius (x, y, xr, yr, r)
c-----------------------------------------------------------------------
c  Find the radius between two points on the plot in mm
c-----------------------------------------------------------------------
      implicit none
      real vx1, vx2, vy1, vy2, fx, fy, dx, dy,
     +wx1, wx2, wy1, wy2, x, y, xr, yr, r
c-----------------------------------------------------------------------
      call pgqvp (2, vx1, vx2, vy1, vy2)
      call pgqwin (wx1, wx2, wy1, wy2)
c
      dx = x - xr
      dy = y - yr
c
      fx = dx / (wx2-wx1) * (vx2 - vx1)
      fy = dy / (wy2-wy1) * (vy2 - vy1)
c
      r = sqrt(fx**2 + fy**2)
c
      end
c
c
      subroutine getopt (dom, first)
c-----------------------------------------------------------------------
c     Get user options
c
c   Output:
c     dom     Means use "x,ytsysm" vraibales instead of "x,ytsys"
c             variables when undoing the Tsys correction
c     first   Drop first cycle
c-----------------------------------------------------------------------
      implicit none
c
      logical dom, first
cc
      integer nopt
      parameter (nopt = 2)
c
      character opts(nopt)*8
      logical present(nopt)
      data opts /'tsysm', 'first'/
c-----------------------------------------------------------------------
      call options ('options', opts, present, nopt)
      dom   = present(1)
      first = present(2)
c
      end

