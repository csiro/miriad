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
c    rjs   22aug97    Better listing format. Better keywords.
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
c	The frequency range of interest. Two values can be given (in GHz).
c	If no values are given, it defaults to any frequency. If a single
c	value is given, then frequencys within 2.5 GHz of this are accepted.
c@ epochs
c	This gives the range of epochs of the data that are of interest.
c	The default is all data.
c@ flux
c	The lower limit flux value to consider as a match (default is
c	all matching fluxes).
c@ device
c	PGPLOT device to plot flux versus time. Default is no plot.
c@ axis
c	X axis for the plot. Possible values are:
c	  time  The epoch of the observation. This is the default.
c	  freq  The frequency of the observation.
c@ xrange
c	Plot range in the x-direction. 2 values in years or GHz.
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
      integer j, iostat, nlen, Line, ndate
      integer npts,nout
      real delfreq, deldate
      real freq, flux, rms
      real sfreq, sflux
      real xlo, xhi, ylo, yhi, diff
      real xrange(2), yrange(2)
      real x(MAXPTS), y(MAXPTS), y1(MAXPTS), y2(MAXPTS)
      integer symbol(MAXPTS)
      double precision day1,day2, date, day
      logical swild, dwild, fwild, wildcard,dofreq
      logical doplot, scalex, scaley
c
      integer NXAXIS
      parameter(NXAXIS=2)
      character xaxes(NXAXIS)*12,xaxis*12
c
c External functions.
c
      integer Len1
      integer pgbeg
      logical keyprsnt
c
      data xaxes/'time        ','frequency   '/
c
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
c
      call keymatch('axis',NXAXIS,xaxes,1,xaxis,nout)
      if(nout.eq.0)xaxis = xaxes(1)
      dofreq = xaxis.eq.'frequency'
c
      call Keyr('freq', freq, 0.0)
      call Keyr('freq', delfreq, 0.0)
c
      call keyt('epochs',day1,'atime',0.d0)
      call keyt('epochs',day2,'atime',0.d0)
c
      call Keya('device', device, ' ')
      doplot = device.ne.' '
      call Keyr('flux', sflux, 0.0)
      scalex = .not. Keyprsnt('xrange')
      scaley = .not. Keyprsnt('yrange')
      call Keyr('xrange', xrange(1), 0.0)
      call Keyr('xrange', xrange(2), xrange(1))
      call Keyr('yrange', yrange(1), 0.0)
      call Keyr('yrange', yrange(2), yrange(1))
      call KeyFin
c
      if(day1*day2.ne.0)then
	date = min(day1,day2)
	deldate = 2*abs(day1-day2)
      else
	date = 1
	deldate = 0
      endif
c
      if(freq*delfreq.ne.0)then
	diff = 0.5*(freq+delfreq)
	delfreq = abs(freq-delfreq)
	freq = diff
      else
	delfreq = 5
      endif
c
c  Check for bad inputs.
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
        write(mesg,110) tmpsrc(1:nlen),sdate(1:ndate),freq,flux,rms
      else
        write(mesg,120) tmpsrc(1:nlen),sdate(1:ndate),freq,flux
      endif
  110 format('Flux of: ', a10, a,' at ', F5.1,
     *       ' GHz:', F6.2, ' Jy; rms:', F5.2, ' Jy')
  120 format('Flux of: ', a10, a,' at ', F5.1,
     *       ' GHz:', F6.2, ' Jy')
      nlen = len1(mesg)
      call output(mesg(1:nlen))
c
c  Save the data to plot flux versus day (19xx.00).
c
      if (doplot) then
        npts = npts + 1
	if(npts.gt.MAXPTS)call bug('f','Too many points')
	if(dofreq)then
	  x(npts) = freq
	else
          x(npts) = ((day - 2451545.0D0) / 365.25D0) + 2000.0
	endif
        y(npts) = flux
	symbol(npts) = freq/5
        y1(npts) = y(npts) - rms
        y2(npts) = y(npts) + rms
        if (scalex) then
	  if(npts.eq.1)then
	    xlo = x(1)
	    xhi = xlo
	  else
            xlo = min(xlo, x(npts))
            xhi = max(xhi, x(npts))
	  endif
        endif
        if (scaley) then
	  if(npts.eq.1)then
	    ylo = y(1)
	    yhi = ylo
	  else
            ylo = min(ylo, y1(npts))
            yhi = max(yhi, y2(npts))
	  endif
        endif
      endif
c
c  Get the next point.
c
   30 continue
      if ((iostat .eq. 0) .and. wildcard) goto 20
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
        else
          diff = xhi - xlo
          if (diff .eq. 0.0) then
            diff = xlo
            if (diff .eq. 0.0) diff = 1
          endif
          xlo = xlo - (0.05 * diff)
          xhi = xhi + (0.05 * diff)
	endif
c
        if (.not. scaley) then
          ylo = yrange(1)
          yhi = yrange(2)
        else
          diff = yhi - ylo
          if (diff .eq. 0.0) then
            diff = ylo
            if (diff .eq. 0.0) diff = 1
          endif
          ylo = ylo - (0.05 * diff)
          yhi = yhi + (0.05 * diff)
	endif
c
        call pgswin(xlo, xhi, ylo, yhi)
        call pgtbox('BCNST', 0.0, 0, 'BCNSTV', 0.0, 0)
c        call pgpt(npts, x, y, 16)
        do j=1,npts
	  call pgpt(1, x(j), y(j), symbol(j))
	enddo
        call pgerry(npts, x, y1, y2, 0.0)
        mesg = 'Source = ' // source
	if(dofreq)then
	  call pglab('Frequency (GHz)','Flux (Jy)',mesg)
	else
          call pglab('Epoch (year)', 'Flux (Jy)', mesg)
	endif
        call pgebuf
        call pgend
      endif
c
      end
