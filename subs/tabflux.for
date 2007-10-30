c***********************************************************************
c  The table routines access the flux calibration data.
c
c  History:
c    jm    26jan90    Original code.
c    jm    31jan90    Modified to allow wildcard entries.
c    jm    07feb90    Modified for default file name and date format.
c    jm    26feb90    Made final date modifications and cleaned up.
c    jm    02mar90    Added a check to see if source, etc. are the
c                     same as the last call to tabflux.
c    jm    09mar90    Changed from string cats in bug call to variable
c                     assignments and changed list direct reads to a
c                     standard format read.
c    jm    03apr90    Included Inputs delfreq and delday as well as I/O
c                     Line and cleaned up code a bit more.
c    jm    03apr90    Included CalGet routine which interpolates
c                     fluxes over time.
c    jm    20jun90    Fixed bug to return freq in CalGet.
c    jm    01aug90    Changed CalGet to only call TabFlux until the
c                     returned date is greater than the input date.
c    jm    08dec90    Changed default catalog name to cals.fluxes
c                     and commented out some routines from doc.
c    jm    17dec90    Corrected if conditions in CalGet for change
c                     of date 01aug90.
c   bpw    18dec90    Replaced findname by fullname.
c    jm    19dec90    Added declaration of fullname.
c    jm    17jun91    TabFind:  4 year difference now computed from
c                     observation date rather than today's date.  Also,
c                     TabParse: variable ``len'' changed to ``tlen''.
c    jm    09dec93    TabLoad:  Now ignores blank lines too.
c    jm    11mar94    TabLoad:  Now it really ignores blank lines.
c    jm    11apr94    TabFind:  Date=0 now really returns latest flux.
c
c***********************************************************************
c* CalGet -- Routine to retrieve an interpolated calibrator flux.
c& jm
c: calibration, flux
c+
      subroutine calget(FileName, source, freq, delfreq, day, deldate,
     *  flux, iostat)
c
      implicit none
      character FileName*(*), source*(*)
      integer iostat
      real freq, delfreq, deldate, flux
      double precision day
c
c Input:
c   FileName The name of the calibration file.  Default is
c            MIRCAT/cals.fluxes.
c   source   The name of the calibrator source to match.  No default.
c   freq     The observation frequency (GHz) (default of 0.0 means
c            that a match occurs at any frequency).
c   delfreq  A range of frequency (in GHz) to bracket the freq
c            parameter.  If ``freq'' is 0.0, then this term is ignored.
c            The value of ``delfreq'' is used to range matching
c            frequencies such that a match occurs when
c            [freq-(delfreq/2) <= freq[table] <= freq+(delfreq/2)].
c   day      The date of the observation to match (default is the
c            current date).  If day is = 0, then the current date is
c            used; if the date is less than the earliest matching date
c            or later than the latest matching date, then the nearest
c            matching date is used.  If neither of these apply, than the
c            two nearest observation dates are used to interpolate the
c            flux over time.  Day MUST be a Julian day number.
c   deldate  A range of time (in Julian days) to bracket the date input.
c            The value of ``deldate'' is used to range matching dates
c            such that a match occurs when
c            [day-(deldate/2) <= day[table] <= day+(deldate/2)].
c   flux     The lower limit of fluxes to consider as a match
c            (default is 0.0 and implies all matching fluxes).
c
c Output:
c   source   The name of the first matched source.  This may not have
c            the same value as on input.
c   freq     The freq of the matched observation.  This may not
c            have the same value as on input and has no relation to
c            the determined flux value.  If the flux is interpolated
c            over time between two values, then freq is set to 0.0 on
c            output.
c   day      The day of the matched observation.  This will not have
c            the same value as on input, unless the flux is the
c            interpolation between two values.
c   flux     The flux of the matched observation.
c   iostat   An error integer.  Iostat = 0 means no error;
c            -1 if there are no data found in the file FileName;
c            -2 if an EOF; -3 if no source name match; -4 if no flux at
c            desired frequency; and -5 if the date of the latest flux
c            measurement is longer than 4 years.
c
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      character*80 tmpsrc
      integer Line, nlen
      real outflux, sfreq, sflux, rms
      real oldfreq, ratio, diffday, diffreq
      real fluxes(2), freqlast(2)
      double precision date, dates(2), olddate
      logical more, init
c
c External functions.
c
      integer Len1
      data tmpsrc / ' '/
      data outflux, oldfreq, olddate / 0.0, 0.0, 0.0/
c
c End declarations.
c-----------------------------------------------------------------------
c Check for bad inputs and convert the date string to Julian.
c
      call Ucase(source)
      nlen = len1(source)
      if (nlen .le. 0) call bug('f',
     *  'GETFLUX: A source name must be provided.')
      if (freq .lt. 0.0) call bug('f',
     *  'GETFLUX: Calibration frequency cannot be negative.')
      if (delfreq .lt. 0.0) call bug('f',
     *  'GETFLUX: Frequency width cannot be negative.')
      if (day .lt. 0.0) call bug('f',
     *  'GETFLUX: Date cannot be negative.')
      if (deldate .lt. 0.0) call bug('f',
     *  'GETFLUX: Date width cannot be negative.')
c
      iostat = 0
      date = day
      if (date .le. 0.0) call TodayJul(date)
c
c If the source, frequency, and date have not changed significantly
c since the last call, simply return the saved values.
c
      diffday = 2.0 * abs(date - olddate)
      diffreq = 2.0 * abs(freq - oldfreq)
      if ((diffday .le. deldate) .and. (diffreq .le. delfreq) .and.
     *    (source(1:nlen) .eq. tmpsrc(1:nlen))) then
        source = tmpsrc
        freq = oldfreq
        day = olddate
        flux = outflux
        return
      endif
c
      outflux = 0.0
      sfreq = freq
      sflux = flux
      Line = 1
      more = .true.
      init = .true.
      olddate = date
c
c Search for all matches of wildcards or the particular source.
c
      do while (more)
        tmpsrc = source
        freq = sfreq
        day = date
        flux = sflux
        call TabFlux(FileName, tmpsrc, freq, delfreq, day, deldate,
     *    flux, rms, Line, iostat)
        if (iostat .lt. 0) then
          more = .false.
        else
          if (init) then
            dates(1) = day
            dates(2) = 0.0
            fluxes(1) = flux
            fluxes(2) = 0.0
            source = tmpsrc
            freqlast(1) = freq
            freqlast(2) = 0.0
            init = .false.
          elseif (dates(2) .eq. 0.0) then
            dates(2) = day
            fluxes(2) = flux
            freqlast(2) = freq
          elseif (day .gt. dates(2)) then
            dates(1) = dates(2)
            dates(2) = day
            fluxes(1) = fluxes(2)
            fluxes(2) = flux
            freqlast(1) = freqlast(2)
            freqlast(2) = freq
          endif
c
c  If the returned date is greater than the input date, stop reading
c  from the routine TabFlux.  We have either bracketed the desired
c  date or have no further matches to make.
c
          if (day .gt. date) then
            more = .false.
          endif
        endif
      enddo
c
      if (init) then
        call bug('w', 'GETFLUX: No match for this source.')
        return
      elseif (dates(2) .eq. 0.0) then
        outflux = fluxes(1)
        day = dates(1)
        freq = freqlast(1)
      elseif (date .ge. dates(2)) then
        outflux = fluxes(2)
        day = dates(2)
        freq = freqlast(2)
      elseif (date .le. dates(1)) then
        outflux = fluxes(1)
        day = dates(1)
        freq = freqlast(1)
      else
        day = date
        ratio = (date - dates(1)) / (dates(2) - dates(1))
        outflux = fluxes(1) + ((fluxes(2) - fluxes(1)) * ratio)
c       freq = freqlast(1) + ((freqlast(2) - freqlast(1)) * ratio)
        freq = 0.0
        if (freqlast(1) .eq. freqlast(2)) freq = freqlast(1)
      endif
c
      oldfreq = freq
      olddate = day
      flux = outflux
c
c Special iostat for EOF.  Flag to no error.
c
      if (iostat .eq. -2) iostat = 0
c
      return
      end
c
c***********************************************************************
c* TabFlux -- Return the flux of a calibrator source at an input freq.
c& jm
c: calibration, flux, frequency
c+
      subroutine tabflux(FileName, source, freq, delfreq, day, delday,
     *  flux, rms, Line, iostat)
c
      implicit none
      character FileName*(*), source*(*)
      real freq, delfreq, delday, flux, rms
      double precision day
      integer Line, iostat
c
c Input:
c   FileName The name of the flux calibrator file.  This defaults to
c            MIRCAT/cals.fluxes.
c   delfreq  The frequency width (real: GHz) around the parameter
c            ``freq'' in which to include a frequency match.
c   delday   The date width (real: Julian days) around the parameter
c            ``day'' in which to include a date match.
c
c Input/Output:
c   source   The calibrator source name ('*' means all sources;
c            minimum match of name is in effect).
c            Output is the full name of the source that matches.
c   freq     The frequency (GHz) of the calibrator data desired (input
c            value of 0.0 defaults match to all frequencies).  Output
c            is the freq (GHz) of the match.
c   day      The day flag for the routine 'tabfind.' Day = 0 means the
c            most recent match; day means first match since day; and -day
c            means first match less than abs(day).  The internal format
c            for day (when day != 0) is the same as for DATE entries in
c            the flux calibration file (Julian Day).  On Output, day
c            is the Julian Day of the match(s).
c   flux     On Input, flux is the lower limit flux (Jy) of the calibrator
c            in which to consider a match and defaults to all matching
c            fluxes if set to 0.0.  On Output, flux (Jy) is the matching
c            source(s) flux value at freq GHz.
c   Line     On Input, this integer represents the next entry in the
c            sorted flux table (not the file) to consider for a match.
c            Line = 1 is the first table entry, so to effectively "rewind"
c            the sorted table, reset Line to 1 on each subsequent call.
c            On output, Line is next item to consider as a possible match.
c
c Output:
c   rms      The rms (Jy) of the calibrator (set to 0 if not listed).
c   iostat   The returned error code.  0 means no error; -1 means no
c            data read; -2, EOF.  Other errors from routine TABFIND.
c
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      character oldname*132, tmpname*132, newname*132
      character errmsg*80, DEFFILE*30
c--   character lastsrc*80
c--   real oldfreq, oldflux, oldrms
c--   double precision oldday
      integer newlen
      real deltnu, deltime
c
      integer len1
      character fullname*132
c
      parameter (DEFFILE='MIRCAT:cals.fluxes')
      data oldname /' '/
c--   data lastsrc /' '/
c--   data oldfreq, oldflux, oldrms, oldday / 0.0, 0.0, 0.0, 0.0/
c
c End declarations.
c Initialize output variables.
c
      rms = 0.0
      iostat = 0
      deltnu = delfreq / 2.0
      deltime = delday / 2.0
c
c If the input source name matches the last found source,
c return the previously found parameters.
c-- I removed this because it caused calflux to loop indefinitely. jm
c
c--   tmpname = source
c--   newlen = len1(tmpname)
c--   if (newlen .le. 0) tmpname = ' '
c--   call Ucase(tmpname)
c--   if (tmpname(1:newlen) .eq. lastsrc(1:newlen)) then
c--     source = lastsrc
c--     freq = oldfreq
c--     day = oldday
c--     flux = oldflux
c--     rms = oldrms
c--     return
c--   endif
c
c If input name is empty, set the file name to default value.
c
      newlen = len1(FileName)
      if (newlen .gt. 0) then
          tmpname = FileName
      else
          tmpname = DEFFILE
      endif
      newname = fullname(tmpname)
      newlen = len1(newname)
      if (newlen .le. 0) then
          call bug('f', 'TABFLUX: Error finding calibrator table name ')
          return
      endif
c
c If input calibrator file name has changed since the last time this
c routine was called, then open the new file and load the source data
c into memory.
c
      if (newname(1:newlen) .ne. oldname(1:newlen)) then
          call TabLoad(newname, iostat)
          if (iostat .ne. 0) then
            errmsg = ' TABFLUX: Error loading file ' // newname
            newlen = len1(errmsg)
            call bug('f', errmsg(1:newlen))
          endif
          oldname = newname(1:newlen)
          Line = 1
      endif
c
c Search the table for the flux value at the frequency desired.
c The search method will return the next entry at the input frequency
c for the named source that fits within the date and flux constraints.
c The indexed (by source) file can be searched for source and then
c incremented to find last entry at proper frequency.  Iostat = -2
c when EOF is encountered (that is no more matching entries).
c
      call TabFind(source, freq, deltnu, day, deltime, flux, rms, Line,
     *  iostat)
c--   if (iostat .eq. 0) then
c--     lastsrc = source
c--     oldfreq = freq
c--     oldday = day
c--     oldflux = flux
c--     oldrms = rms
c--   endif
      return
      end
c
c***********************************************************************
cc* TabLoad -- Read the calibration file.
cc& jm
cc: calibration
cc+
      subroutine tabload(name, iostat)
c
c This routine will read a calibration file loading sources, observation
c dates, frequencies, fluxes, and rms values into the table arrays.
c THIS ROUTINE IS VERY FORMAT SPECIFIC IN THAT THE FIRST CHARACTER MUST
c BE THE CHARACTER "!" IF THE LINE IS TO BE IGNORED.  ALL other lines
c MUST have the following form:
c     'Source'  DATE.UT  FREQ(GHz)  FLUX(Jy)  rms(Jy)
c with each field separated by at least one space or TAB character.
c
      implicit none
      character name*(*)
      integer iostat
c
c Input:
c   name     The name of the flux calibrator file.
c
c Output:
c   iostat   The returned error code.  0 means no error.
c
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer lu, length, nentry
      character*132 string
c
      integer len1
c
c End declarations.
c
      call TxtOpen(lu, name, 'old', iostat)
      if (iostat .ne. 0) then
          string = ' TABLOAD: Error opening file '// name
          length = len1(string)
          call bug('w', string(1:length))
          call bugno('f', iostat)
          return
      endif
c
c Read file until either an EOF or error is identified (iostat=-1).
c Skip the string if it is preceeded by the character "!" or is
c filled entirely with blanks.
c Otherwise, increment the counter and parse the array values.
c
      nentry = 0
      call TxtRead(lu, string, length, iostat)
      do while (iostat .eq. 0)
          length = len1(string)
          if ((string(1:1) .ne. '!') .and. (length .gt. 0)) then
            nentry = nentry + 1
            call TabParse(string, length, nentry)
          endif
          call TxtRead(lu, string, length, iostat)
      enddo
c
c If the last return code was not an EOF, it was an error.  If an EOF
c was returned, close the file and then sort the table of data by
c source name.
c
      if (iostat .ne. -1) call bugno('f', iostat)
      call TxtClose(lu)
      if (nentry .eq. 0) then
          call bug('f', 'TABLOAD: No entries in this calibration file')
          return
      endif
      call TabIndex
c
      iostat = 0
      return
      end
c
c***********************************************************************
cc* TabFind -- Return the flux for a given source and frequency.
cc& jm
cc: calibration, flux, frequency
cc+
      subroutine tabfind(source, freq, deltnu, day, deltime, flux, rms,
     *  Line, iostat)
c
c This routine will find the entry 'source' in the flux calibration
c table and match frequencies to the variable 'freq' and returns the
c the flux and rms (if it exists) that matches the qualifier 'day'.
c Recursive calls to TabFind will find the next match or return an
c EOF flag in iostat.
c
      implicit none
      character source*(*)
      real freq, deltnu, deltime, flux, rms
      double precision day
      integer Line, iostat
c
c Input:
c   deltnu   The half width (real: GHz) of the frequency in which
c            to consider a source a match.
c   deltime  The half width (real: Julian days) of the day in which
c            to consider a source a match.
c
c Input/Output:
c   source   The name of the flux calibrator source.  An empty string
c            or source='*' on input implies any source is a match.
c            Minimum match is in effect.  ``Source'' contains the full
c            name of the matched source on output.
c   freq     The frequency of observation of the source.
c   day      Day = 0 means the most recent match; day means first match
c            since day; and -day means first match less than abs(day).
c            The internal format for day is the same as for DATE entries
c            in the flux calibration file (Julian day).
c   Line     On input, this integer indicates the next item to consider
c            as a match from the sorted array.  On output, this points
c            to the next entry to start with on the next call.
c
c Output:
c   flux     The flux (Jy) of the source at freq GHz.
c   rms      The rms error on the flux measurement (Jy).
c   iostat   The returned error code.  0 means a correct match;
c            -1 if there are no values loaded into the arrays yet; -2 if
c            an EOF; -3 if no source name match; -4 if no flux at
c            desired frequency; and -5 if the date of the latest flux
c            measurement is longer than TOOBIG days (4 years).
c
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer nlen, i, j, match, nday
      real testday, testfreq, TOOBIG
      double precision date
c---  double precision TODAY
      logical swild, fwild, dwild, vwild, more
      character STRBIG*45, errmsg*80
      character PROG*9
c
      integer len1
      include 'tabflux.h'
c
      parameter (TOOBIG=4*365.25, PROG='TABFIND: ')
      parameter (STRBIG='Next calibration item is older than 4 years.')
c
c End declarations.
c
c Find source match.  This assumes the data has had the source names
c sorted alphanumerically prior to entering this routine.  If the
c entry last (which holds the last correct item) is larger than NTAB,
c then return an EOF.
c
      if (Line .lt. 1) Line = 1
      if (NTAB .lt. 1) then
          errmsg = PROG // 'No calibration sources loaded yet'
          nlen = len1(errmsg)
          call bug('w', errmsg(1:nlen))
          iostat = -1
          return
      endif
      if (Line .gt. NTAB) then
          iostat = -2
          return
      endif
      nlen = len1(source)
      swild = .false.
      if ((source .eq. '*') .or. (nlen .le. 0)) swild = .true.
      if (nlen .gt. 0) call Ucase(source(1:nlen))
      do i = Line, NTAB
            j = TINDEX(i)
            if (swild) goto 10
            if (source(1:nlen) .eq. TSOURCE(j)(1:nlen)) goto 10
      enddo
      j = 0
   10 continue
      if ((Line .gt. 1) .and. (j .eq. 0)) then
          iostat = -2
          return
      elseif (j .eq. 0) then
          iostat = -3
          errmsg = PROG // 'No match of source name: ' // source
          nlen = len1(errmsg)
          call bug('i', errmsg(1:nlen))
          return
      endif
c
c Source found.  Loop to find the frequency match that agrees with the
c day and flux flags.
c
      fwild = .false.
      vwild = .false.
      dwild = .false.
      if (freq .eq. 0.0) fwild = .true.
      if (flux .le. 0.0) vwild = .true.
      if (day .eq. 0.0) dwild = .true.
      nday = 1
      if (day .lt. 0.0) nday = -1
      date = abs(day)
      if ((dwild) .or. (date .eq. 1.0)) call TodayJul(date)
      more = .true.
      match = 0
   20 continue
      testday = abs((nday * TDATE(j)) - day)
      if (dwild .or. (testday .le. deltime) .or. (day .eq. 1.0)) then
          testfreq = abs(TFREQ(j) - freq)
          if (fwild .or. (testfreq .le. deltnu)) then
            if (vwild .or. (flux .le. TFLUX(j))) then
              match = j
              if (swild) then
                source = TSOURCE(j)
                nlen = len1(TSOURCE(j))
              endif
              if (fwild) freq = TFREQ(j)
c---          date = TDATE(j)
              flux = TFLUX(j)
              rms = TRMS(j)
              more = dwild
            endif
          endif
      endif
      i = i + 1
      if (more .and. (i .le. NTAB)) then
          j = TINDEX(i)
          if (swild .or. (source(1:nlen) .eq. TSOURCE(j)(1:nlen)))
     *      goto 20
      endif
c
c No frequency match found.
c
      if ((Line .gt. 1) .and. (match .eq. 0)) then
          iostat = -2
          return
      elseif (match .eq. 0) then
          iostat = -4
          errmsg = PROG // 'No match at freq desired'
          nlen = len1(errmsg)
          call bug('i', errmsg(1:nlen))
          return
      endif
c
c Frequency match found.  Check how recent.  If the change in time
c is too large, return a warning.  Otherwise, return no error.
c
      Line = i
      source = TSOURCE(match)
      nlen = len1(source)
      day = TDATE(match)
      freq = TFREQ(match)
      flux = TFLUX(match)
      rms = TRMS(match)
c
c---  call TodayJul(TODAY)
c---  testday = TODAY - date
      testday = abs(day - date)
      if (testday .gt. TOOBIG) then
          iostat = -5
          errmsg = PROG // STRBIG
          nlen = len1(errmsg)
          call bug('i', errmsg(1:nlen))
          return
      endif
c
      iostat = 0
      return
      end
c
c***********************************************************************
cc* TabParse -- Internal routine to parse the calibrator string.
cc& jm
cc: calibration
cc+
      subroutine tabparse(string, length, nentry)
c
      implicit none
      character string*(*)
      integer length, nentry
c
c Input:
c   string   The input string to parse.
c   length   The length of the input string.
c   nentry   A counter to index to table entries.
c
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c tabflux.h: Common block and declarations of table entries.
c
      character token*40
      integer k1, k2, tlen, j1, j2
      include 'tabflux.h'
c
c End declarations.
c
      if (nentry .gt. NTABLE) then
        call bug('w',
     *    'TABPARSE: Include file tabflux.h must be adjusted.')
        call bug('f',
     *    'TABPARSE: Too many entries in the calibrator flux table.')
      endif
      NTAB = nentry
      TINDEX(NTAB) = NTAB
      k1 = 1
      k2 = length
      call getfield(string, k1, k2, token, tlen)
        call Ucase(token)
        j1 = 1
        j2 = tlen
        if (token(j1:j1) .eq. '''') j1 = j1 + 1
        if (token(j2:j2) .eq. '''') j2 = j2 - 1
      TSOURCE(NTAB) = token(j1:j2)
      call getfield(string, k1, k2, token, tlen)
      call DayJul(token(1:tlen), TDATE(NTAB))
      call getfield(string, k1, k2, token, tlen)
      read(token(1:tlen), 10) TFREQ(NTAB)
      call getfield(string, k1, k2, token, tlen)
      read(token(1:tlen), 10) TFLUX(NTAB)
      call getfield(string, k1, k2, token, tlen)
      TRMS(NTAB) = 0.0
      if (tlen .gt. 0) read(token(1:tlen), 10) TRMS(NTAB)
   10 format(G30.0)
      return
      end
c
c***********************************************************************
cc* TabIndex -- Internal routine to sort the calibrator table.
cc& jm
cc: calibration
cc+
      subroutine tabindex
c
      implicit none
c
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c tabflux.h: Common block and declarations of table entries.
c
      include 'tabflux.h'
c
c End declarations.
c
      call HSortAD(NTAB, TSOURCE, TDATE, TINDEX)
      return
      end
