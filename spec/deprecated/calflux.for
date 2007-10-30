c***********************************************************************
c  Routine to list the calibrator sources.
c
c  History:
c    jm    30jan90    Original code.
c    jm    07feb90    Modified for default file name and yymmmdd.d
c                     format.
c    jm    26feb90    Modified date structure and cleaned up.
c    jm    09mar90    Performs a check to see of month appear in date.
c                     Also changed list direct read to formatted read.
c    jm    28mar90    Corrected bug calls and cleaned up code a bit.
c    jm    03apr90    Modified to account for changes to TabFlux.  This
c                     includes the addition of inputs ``deldate'' and
c                     ``delfreq''.
c    jm    15dec90    Changed inline doc to reflect default calibrator
c                     flux file name change and limit lines to 72 chars.
c    mchw  01sep94    Added plot.
c    mchw  10may96    Different plot symbols with frequency.
c***********************************************************************
c= CalFlux - Print or plot flux data for a calibrator source.
c& jm
c: calibration, flux
c+
      program calflux
      implicit none
c
c	CALFLUX returns the flux (in Jy) of a calibrator source at a
c	given frequency (in GHz).  The source resides in a calibration
c	file that is formatted such that each record is composed of
c	white space separated fields ordered Source, Day (yymmmdd.d),
c	Freq (GHz), Flux (Jy), and rms (Jy).  Lines beginning with
c	a "!" are excluded.  Warnings are returned if there is no match
c	of the source in the calibrator file, if there is no matching
c	entry at the desired frequency, or the observation date is
c	greater than 4 years.  The Day field, presently, is formatted
c	as yymmmdd.d where yy is the year field 19yy, mmm is the three
c	character string of the month, and dd.d is the decimal value
c	of days.  None of the inputs are required, but they provide a
c	means of bracketing the desired source(s).
c@ in
c	Name of the calibration data file (Default is the file
c	cals.fluxes in the directory MIRCAT).
c@ source
c	Name of the calibration source to list (default is all sources).
c	The source name is minimum match format.
c@ freq
c	Frequency that the source was observed at in GHz (default is
c       0.0, which implies all frequencies are valid matches).
c@ delfreq
c	A full width in GHz (default is 5.0) in which to accept
c       deviations from the value of ``freq'' as a match.  If ``freq''
c       is not given or is set to the default value, this input
c       is ignored.
c@ date
c	The cutoff date before or after which no observations are
c       listed (default is date=1.0, which implies all dates are valid
c       matches).  If ``date=0'', then only the most recent data is
c       listed.  If ``date>0,'' then all data more recent than ``date''
c       are listed; ``date<0,'' then all data prior to ``abs(date)''
c       are presented.  The format for ``date'' is the same as the DATE
c       field in the flux calibration file:  either ``yymmmdd.d'' or
c       ``yymmmdd:hh:mm:ss.s'' with no internal spaces (the first 7
c       characters are required).
c@ deldate
c	A full width in Julian days (default is 0.0) in which to accept
c	deviations from the value of ``date'' as a match.  If ``date''
c	is not given or it is set to the default value, then this input
c       is ignored.
c@ flux
c	The lower limit flux value to consider as a match (default is
c	all matching fluxes).
c@ device
c	PGPLOT device to plot flux versus time. Default is no plot.
c@ xrange
c	Plot range in the x-direction. 2 values in format year.fraction
c	Default is to self scale.
c@ yrange
c	Plot range in the y-direction. 2 values.
c	Default is to self scale.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      include 'maxdim.h'
c
      integer MAXPTS
      parameter(MAXPTS=MAXBUF/100)
c
      character*80 FileName, source, tmpsrc, mesg, device
      character*30 sdate
      integer j, iostat, nlen, ndate, Line
      integer npts
      real delfreq, deldate
      real freq, flux, rms, tday
      real sfreq, sflux
      real xlo, xhi, ylo, yhi, diff
      real xrange(2), yrange(2)
      real x(MAXPTS), y(MAXPTS), y1(MAXPTS), y2(MAXPTS)
      integer symbol(MAXPTS)
      double precision date, day
      logical notchar, swild, dwild, fwild, wildcard
      logical doplot, scalex, scaley
c
c External functions.
c
      integer Len1
      integer pgbeg
      logical keyprsnt
c
c End declarations.
c-----------------------------------------------------------------------
c Announce program.
c
      mesg = 'Calflux: version 1.5 10-MAY-96'
      nlen = len1(mesg)
      call output(mesg(1:nlen))
c
c-----------------------------------------------------------------------
c  Read in command line inputs and check for bad inputs.
c
      call KeyIni
      call Keya('in', FileName, ' ')
      call Keya('source', source, '*')
      call Keyr('freq', freq, 0.0)
      call Keyr('delfreq', delfreq, 5.0)
      call Keya('date', sdate, ' ')
      doplot = Keyprsnt('device')
      call Keya('device', device, ' ')
      call Keyr('deldate', deldate, 0.0)
      call Keyr('flux', sflux, 0.0)
      scalex = .not. Keyprsnt('xrange')
      scaley = .not. Keyprsnt('yrange')
      call Keyr('xrange', xrange(1), 0.0)
      call Keyr('xrange', xrange(2), xrange(1))
      call Keyr('yrange', yrange(1), 0.0)
      call Keyr('yrange', yrange(2), yrange(1))
      call KeyFin
c
c  Check for bad inputs and convert the date string to Julian.
c
      if (freq .lt. 0.0) call bug('f',
     *  'CALFLUX: Calibration frequency cannot be negative')
      if (delfreq .lt. 0.0) call bug('f',
     *  'CALFLUX: Frequency width cannot be negative')
      if (deldate .lt. 0.0) call bug('f',
     *  'CALFLUX: Date width cannot be negative')
c
      if ((.not. scalex) .and. (xrange(1) .eq. xrange(2))) then
        call bug('w', 'CALFLUX: X range will be autoscaled.')
        scalex = .TRUE.
      endif
      if ((.not. scaley) .and. (yrange(1) .eq. yrange(2))) then
        call bug('w', 'CALFLUX: Y range will be autoscaled.')
        scaley = .TRUE.
      endif
c
c  Initialise the plot limits.
c
      xlo = 1.0E9
      xhi = 0.0
      ylo = 1.0E9
      yhi = 0.0
c
c  Read through the data file.
c
      nlen = Len1(sdate)
      notchar = .TRUE.
      if (nlen .le. 0) goto 11
      do 10 j = 1, nlen
        if (((sdate(j:j) .ge. 'A') .and. (sdate(j:j) .le. 'Z')) .or.
     *      ((sdate(j:j) .ge. 'a') .and. (sdate(j:j) .le. 'z'))) then
          notchar = .FALSE.
          goto 11
        endif
   10 continue
   11 continue
      ndate = 1
      if (nlen .le. 0) then
        date = 1.0
      elseif (notchar) then
        read(sdate, 100, err=99) tday
  100   format(G30.0)
        if (tday .lt. 0) ndate = -1
        date = abs(tday)
      else
        j = 1
        do while (sdate(j:j) .le. ' ')
          j = j + 1
        enddo
        if (sdate(j:j) .eq. '-') then
          j = j + 1
          ndate = -1
        endif
        call DayJul(sdate(j:nlen), date)
      endif
      date = ndate * date
c
c  Check for possible defaults that leads to looping (wildcard).
c
      swild = .false.
      fwild = .false.
      dwild = .false.
      if (source .eq. '*') swild = .true.
      if (freq .eq. 0.0) fwild = .true.
      if (date .ne. 0.0) dwild = .true.
      wildcard = (swild .or. fwild .or. dwild)
      sfreq = freq
c
c  Search for all matches of wildcards or the particular source.
c
      npts = 0
      Line = 1
   20 continue
      tmpsrc = source
      freq = sfreq
      day = date
      flux = sflux
      call TabFlux(FileName, tmpsrc, freq, delfreq, day, deldate, flux,
     *  rms, Line, iostat)
      if (iostat .eq. -2) goto 30
      if (iostat .eq. -1) then
          call bugno('w', iostat)
          wildcard = .false.
      endif
      if ((iostat .lt. 0) .and. (iostat .ne. -5)) goto 30
      iostat = 0
c
c  Output information about the source including the retrieved flux.
c
      nlen = 10
      call JulDay(day, 'D', sdate)
      ndate = len1(sdate)
      if (rms .gt. 0.0) then
        write(mesg,110) tmpsrc(1:nlen),sdate(1:ndate),day,freq,flux,rms
      else
        write(mesg,120) tmpsrc(1:nlen), sdate(1:ndate), day, freq, flux
      endif
  110 format('Flux of: ', a10, a, f10.0 ' at ', F5.1,
     *       ' GHz:', F6.2, ' Jy; rms:', F5.2, ' Jy')
  120 format('Flux of: ', a10, a, f10.0 ' at ', F5.1,
     *       ' GHz:', F6.2, ' Jy')
      nlen = len1(mesg)
      call output(mesg(1:nlen))
c
c  Save the data to plot flux versus day (19xx.00).
c
      if (doplot .and. (npts .lt. MAXPTS)) then
        npts = npts + 1
        x(npts) = ((day - 2451545.0D0) / 365.25D0) + 2000.0
        y(npts) = flux
	symbol(npts) = freq/5
	print *, symbol(npts)
        y1(npts) = y(npts) - rms
        y2(npts) = y(npts) + rms
        if (scalex) then
          xlo = min(xlo, x(npts))
          xhi = max(xhi, x(npts))
        endif
        if (scaley) then
          ylo = min(ylo, y1(npts))
          yhi = max(yhi, y2(npts))
        endif
      endif
c
c  Get the next point.
c
   30 continue
      if ((iostat .eq. 0) .and. wildcard) goto 20
      goto 90
   99 continue
      call bug ('f', 'CALFLUX: Date format incorrectly specified')
   90 continue
c
c  Do the plotting.
c
      if (doplot .and. (npts .gt. 0)) then
        if (pgbeg(0, device, 1, 1) .ne. 1) then
          call pgldev
          call bug('f', 'CALFLUX: Error opening the graphics device.')
        endif
        call pgpage
        call pgvstd
        call pgbbuf
c
        if (.not. scalex) then
          xlo = xrange(1)
          xhi = xrange(2)
        endif
        diff = xhi - xlo
        if (diff .eq. 0.0) then
          diff = xlo
          if (diff .eq. 0.0) diff = 1
        endif
        xlo = xlo - (0.05 * diff)
        xhi = xhi + (0.05 * diff)
c
        if (.not. scaley) then
          ylo = yrange(1)
          yhi = yrange(2)
        endif
        diff = yhi - ylo
        if (diff .eq. 0.0) then
          diff = ylo
          if (diff .eq. 0.0) diff = 1
        endif
        ylo = ylo - (0.05 * diff)
        yhi = yhi + (0.05 * diff)
c
        call pgswin(xlo, xhi, ylo, yhi)
        call pgtbox('BCNST', 0.0, 0, 'BCNSTV', 0.0, 0)
c        call pgpt(npts, x, y, 16)
        do j=1,npts
	  call pgpt(1, x(j), y(j), symbol(j))
	enddo
        call pgerry(npts, x, y1, y2, 0.0)
        mesg = 'Source = ' // source
        call pglab('Epoch (Year)', 'Flux (Jy)', mesg)
        call pgebuf
        call pgend
      endif
c
      end
