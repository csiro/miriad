      program uvtplt
c------------------------------------------------------------------------
c     UVTPLT plots Tsys for each field of a mosaiced observation
c
c= uvtplt - 
c	     
c& nebk
c: plotting
c+
c     UVTPLT plots Tsys for each field of a mosaiced observation
c
c@ vis
c	Input visibility file. No default.
c@ freq
c	Band centre frequencies of interest in MHz. No default.
c@ source
c	Root name of mosaiced field.  E.g. "cena_" with fields like
c	cena_1, cena_12  etc.  No default.
c@ fields
c	Integer range of fields such as 1,20  No default.
c@ device
c	PGPLOT device.  No default.
c@ nxy
c	Number of plots in z and y directions per page
c@ options
c	tsysm  Get Tsys from median smoothed variables "xtsysm"
c	  and "ytsysm" rather than usual "xtsys" and "ytsys"
c	first  Drop the first cycle of each new mosaic field
c--
c
c     nebk 20mar95 Original version
c     nebk 11apr95 Add check for Tsys variables
c
c  Bugs
c    Assumes linear polarizations
c
c---------------------------------------------------------------------------
      include 'maxdim.h'
      integer frqmax, fldmax, polmax, antmax, timemax, allmax
      character version*(*)
      parameter(version='version 11-Apr-95', frqmax=4, 
     +          polmax = 2, fldmax = 20, antmax=6, timemax=300,
     +          allmax=fldmax*antmax*polmax*frqmax)
c
      character vis*64, roots*20, source*20, device*60, type*1
      real time(timemax,fldmax,antmax,polmax,frqmax), 
     +  tsys(timemax,fldmax,antmax,polmax,frqmax),
     +  vtsys(maxant*maxwin*2)
      integer ntsys(fldmax,antmax,polmax,frqmax)
c
      integer nf, nread, tvis, freqs(frqmax), nx, ny,
     +  i, j, k, l, ifield1, ifield2, ir, it, ifreq, ifield, 
     +  nants, nspect, nschan(maxwin), jd0, ii, ierr, length
      double precision sfreq(maxwin), sdf(maxwin), preamble(4),
     + jd, jdold
      complex data(maxchan)
      logical flags(maxchan), keep, dom, updated, first
      integer len1, pgbeg
      data ntsys /allmax*0.0/
c------------------------------------------------------------------------
c
c Get the input parameters
c
      call output ('UvTPlt: '//version)
      call keyini
      call keya ('vis', vis, ' ')
      if (vis.eq.' ') call bug ('f', 'Input file name not given')
      call mkeyi ('freq', freqs, frqmax, nf)
      if (nf.eq.0) call bug ('f', 'Must give frequencies')
      call keya ('source', roots, ' ')
      if (roots.eq.' ') call bug ('f', 'No source name given')
      ir = len1(roots)
      call keyi ('fields', ifield1, 1)
      call keyi ('fields', ifield2, ifield1)
      if (ifield1.le.0 .or. ifield2.le.0) 
     +  call bug ('f', 'Invalid field range')
      call keya ('device', device, ' ')
      if (device.eq.' ') call bug ('f', 'No plotting device given')
      call keyi ('nxy', nx, 3)
      call keyi ('nxy', ny, 2)
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
      call uvprobvr (tvis,'xtsysm', type, length, updated)
      if (dom .and. type.ne.'r') call bug ('f',
     +  'Median smoothed variables do not exists in this dataset')
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
        if (dom) then
          call uvgetvrr (tvis, 'xtsysm', vtsys, nants*nspect)
          call uvgetvrr (tvis, 'ytsysm', vtsys(nants*nspect+1), 
     +                   nants*nspect)
        else
          call uvgetvrr (tvis, 'xtsys', vtsys, nants*nspect)
          call uvgetvrr (tvis, 'ytsys', vtsys(nants*nspect+1), 
     +                   nants*nspect)
        end if
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
          do k = 1, nspect
            call point1 (nschan(k), sdf(k), sfreq(k), source, nf, 
     +         freqs, ifield1, ifield2, roots(1:ir), ifreq, 
     +         ifield, keep)
            if (keep) then
              do j = 1, 2
                do i = 1, nants
                  it = (j-1)*nants*nspect + (k-1)*nants + i
                  ii = ntsys(ifield,i,j,ifreq)
                  if (ii.eq.timemax) call bug ('f', 
     +               'Too many integrations for internal buffers')
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
c Now do plots
c
      ierr = pgbeg (0, device, nx, ny)
      if (ierr.ne.1) call bug ('f', 'Error opening PGPLOT device')
c      
      do l = 1, nf
        do k = 1, 2
          do j = ifield1, ifield2
            do i = 1, 6
              call plot (first, ntsys(j,i,k,l), time(1,j,i,k,l),
     +          tsys(1,j,i,k,l), roots(1:ir), j, i, k, freqs(l))
            end do
          end do
        end do
      end do
      call pgend

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
      subroutine plot (first, n, x, y, roots, ifield, iant, ipol, freq)
c-----------------------------------------------------------------------
c   Plot em up
c-----------------------------------------------------------------------
      implicit none
      integer n, ifield, iant, i, j, ipol, freq
      real x(n), y(n)
      logical first
      character roots*(*)
cc  
      real ymin, ymax, xl, x2, y1, y2, xold
      character aline*80, cpol*1
c-----------------------------------------------------------------------
c 
c Drop first cycle of each visit
c
      if (first) then
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
c Find extrema
c
      call pgvstd
      xl = x(1) - 0.05*(x(n)-x(1))      
      x2 = x(n) + 0.05*(x(n)-x(1))      
      ymin=1.0e30
      ymax=-1.0e30
      do i = 1, n
        ymin = min(ymin,y(i))
        ymax = max(ymax,y(i))
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
      if (ifield.le.9) then
        write (aline, 100) freq, cpol, roots, ifield, iant
100     format (' Freq. ', i4, ' MHz', '  Pol ', a1,
     +          '  Field ', a, i1, '  Antenna ', i2)
      else 
        write (aline, 200) freq, cpol, roots, ifield, iant
200     format (' Freq. ', i4, ' MHz', '  Pol ', a1,
     +          '  Field ', a, i2, '  Antenna ', i2)
      end if
      call pglabel ('UT', 'Tsys', aline)
      end
c
c
      subroutine getopt (dom, first)
c-----------------------------------------------------------------------
c     Get user options
c
c   Output:
c     dom     Means plot "x,ytsysm" variables instead of "x,ytsys"
c     first   Drop first cycle of each field
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
      dom =   present(1)
      first = present(2)
c
      end

