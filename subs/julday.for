c***********************************************************************
c
c  History:
c    rjs    ???????  Original version.
c     jm    22feb90  Switched to new format of date.
c    rjs     9mar90  Fixed a portability bug. Changed some DATA to
c                    PARAMETER statements. Some work on getting it to
c                    work on VMS.
c     jm     9mar90  Removed string cats to a variable for bug.for and
c                    changed list direct reads to standard formats.
c     jm    13mar90  Corrected julian time so it is based on UT and
c                    fixed JulDay roundoff error.
c    rjs    27mar90  Deleted JulCal.
c    rjs     4aug90  Fixed minor portability problem in a FORMAT statement.
c     jm    11feb91  Let JulDay work with abbreviated input strings.
c     jm    11feb91  Changed min to minute to avoid intrinsic func name.
c     jm    15feb91  Fixed a bug that occurred when a call to index was
c                    used to replace a loop in DayJul.
c     jm    20feb91  Modified DayJul to be a bit more flexible and allow
c                    strings with only time (eg. hh only).
c     jm    26feb91  Changed references of Isalpha to Isalphaf.
c     jm    27feb91  Corrected a bug with the year 2000 and changed the
c                    way the hh:mm:ss (or subset) was parsed in DayJul.
c     jm     4mar91  Fixed a fractional day parser bug in DayJul.
c    rjs     5mar91  Used ichar function to convert from char to int.
c     jm     6mar91  Corrected decimal days/secs conversion in DayJul.
c mjs/jm    11mar91  Corrected date/time call for Cray in TodayJul.
c    rjs     5sep91  Various error checking in DayJul.
c    rjs     1oct91  Added JulFDate and FDateJul routines.
c    rjs    18feb92  Improved error checking in DayJul.
c     jm    03mar92  Corrected a leap year bug in JulDay.  This meant
c                    promoting, to dble(), integers in real expressions.
c                    In particular, this happened in setting item "c".
c    rjs    23dec92  Eliminate conditional compilation, and use mitime,
c                    midate.
c    rjs    16sep93  Rename bsrch to binsrch.
c     jm    14mar94  Added check for blank input strings; removed tabs.
c    rjs    10oct94  Round date in julfdate.
c     jm    07jun96  To avoid integer overflow in dayjul, I added code
c                    to strip off trailing 0s beyond the decimal point.
c    rjs    10sep97  Added "caljul" function.
c     jm    10sep97  Added functionality for IAU 'T' format.
c     jm    11sep97  Moved fdate functionality into julday routines.
c                    Also added VMS date style dd-mmm-ccyy.
c                    Major modification of dayjul to handle new formats.
c                    Removed julfdate() and datefjul() routines (these
c                    are now handled by dayjul().
c    rjs    25sep97  Added subroutine julcal.
c    rjs    09jan97  Make datepars tolerant of spaces in names.
c***********************************************************************
c* JulDay -- Format a Julian day into a conventional calendar day.
c& jm
c: Julian-day, date, utilities
c+
      subroutine julday(julian, form, calday)
c
      implicit none
      double precision julian
      character form*(*), calday*(*)
c
c  Convert from Julian date to calendar date.  This is not to high
c  accuracy, but it achieves the accuracy required.  See "Astronomical
c  Formulae for Calculators", Jean Meeus (Wiillmann-Bell Inc).
c  The day is assumed to begin at 0 hours UT.
c
c  Input:
c    julian      Julian date.
c    form        Output selection flag.  It must be one of
c                'D', 'F', 'H', 'T', or 'V' (case independent).
c
c  Output:
c    calday      (Gregorian) Calendar day (UT time).
c                The output if form = 'D' is like:
c                       'yymmmdd.dd'
c                The output if form = 'F' is like:
c                       'dd/mm/yy' (loss of fractional days)
c                The output if form = 'H' is like:
c                       'yymmmdd:hh:mm:ss.s'
c                The output if form = 'T' is like:
c                       'ccyy-mm-ddThh:mm:ss.s' (the T is literal)
c                The output if form = 'V' is like:
c                       'dd-mmm-ccyy' (loss of fractional days)
c
c                Note that cc is used to denote the century; yy the
c                year (mod 100); mm the decimal month number (1-12);
c                mmm a string describing the month; dd the day number;
c                dd.dd the fractional day number; and hh:mm:ss.s the
c                hours, minutes, and seconds of the day.
c
c--
c-----------------------------------------------------------------------
      character PROG*8
      parameter(PROG='JulDay: ')
      double precision FUDGE
c     parameter(fudge=0.5/(24.0*60.0*60.0*10.0))
      parameter(FUDGE=1.0/1728000.0)
c
      double precision f
      integer month,year,day,hr,minute,sec
      integer dsec, decday, nchar, century
      character months(12)*3
      character outform*80, outstr*25
c
      integer Len1
c
      data months/'JAN','FEB','MAR','APR','MAY',
     *      'JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
c
      call julcal(julian+FUDGE,year,month,f)
      day = f
c
      decday = 100 * (f - day)
      hr = 24 * (f - day)
      minute = 60 * (24 * (f - day) - hr)
      sec = int(600 * (60 * (24 * (f - day) - hr) - minute))
      dsec = mod(sec, 10)
      sec = sec / 10
      century = year / 100
      year = mod(year, 100)
c
      nchar = Len1(form)
      if (nchar .lt. 1) then
        nchar = 1
        outform = 'D'
      else
        nchar = min(nchar, 80)
        outform = form(1:nchar)
      endif
      call Ucase(outform(1:nchar))
c
      if (index(outform(1:nchar), 'D') .ne. 0) then
        write (outstr, 10) year, months(month), day, decday
   10   format(i2.2, a, i2.2, '.', i2.2)
        nchar = 10
      else if (index(outform(1:nchar), 'F') .ne. 0) then
        write (outstr, 20) day, month, year
   20   format(i2.2, '/', i2.2, '/', i2.2)
        nchar = 8
      else if (index(outform(1:nchar), 'H') .ne. 0) then
        write (outstr, 30) year, months(month), day,
     *    hr, minute, sec, dsec
   30   format(i2.2, a, i2.2, ':', i2.2, ':', i2.2, ':', i2.2, '.', i1)
        nchar = 18
      else if (index(outform(1:nchar), 'T') .ne. 0) then
        write (outstr, 40) century, year, month, day,
     *    hr, minute, sec, dsec
   40   format(i2.2, i2.2, '-', i2.2, '-', i2.2, 'T',
     *    i2.2, ':', i2.2, ':', i2.2, '.', i1)
        nchar = 21
      else if (index(outform(1:nchar), 'V') .ne. 0) then
        write (outstr, 50) day, months(month), century, year
   50   format(i2.2, '-', a, '-', i2.2, i2.2)
        nchar = 11
      else
        outstr = ' '
        outform = PROG // 'Unrecognized style format: ' // form(1:nchar)
        nchar = Len1(outform)
        call bug('w', outform(1:nchar))
        nchar = 1
      endif
      nchar = min(len(calday), nchar)
      calday = outstr(1:nchar)
      return
      end
c************************************************************************
c* JulCal -- Convert a Julian day into a calendar day.
c& jm
c: Julian-day, date, utilities
c+
      subroutine julcal(julian,year,month,day)
c
      implicit none
      double precision julian,day
      integer year,month
c
c  Convert from Julian date to calendar date.  This is not to high
c  accuracy, but it achieves the accuracy required.  See "Astronomical
c  Formulae for Calculators", Jean Meeus (Wiillmann-Bell Inc).
c  The day is assumed to begin at 0 hours UT.
c
c  Input:
c    julian	Julian day.
c  Output:
c    year	Year (e.g. 1990)
c    month	Month [1,12].
c    day	Day [1-32).
c------------------------------------------------------------------------
      double precision f
      integer z,a,b,c,d,e,alpha
c
      z = julian + 0.5
      f = julian + 0.5 - z
      if(z .lt. 2299161)then
        a = z
      else
        alpha = int((dble(z) - 1867216.25) / 36524.25)
        a = z + 1 + alpha - int(0.25 * alpha)
      endif
      b = a + 1524
      c = (dble(b) - 122.1) / 365.25
      d = 365.25 * c
      e = dble(b - d) / 30.6001
      f = f + dble(b - d - int(30.6001 * e))
      day = f
c
      if(e .le. 13)then
        month = e - 1
      else
        month = e - 13
      endif
      if(month .gt. 2)then
        year = c - 4716
      else
        year = c - 4715
      endif
      end
c***********************************************************************
c* DayJul -- Format a conventional calendar day into a Julian day.
c& jm
c: Julian-day, date, utilities
c+
      subroutine dayjul(calday, julian)
c
      implicit none
      character calday*(*)
      double precision julian
c
c  Convert to Julian date from calendar date. This is not to high
c  accuracy, but it achieves the accuracy required. See "Astronomical
c  Formulae for Calculators", Jean Meeus (Wiillmann-Bell Inc).
c  The day is assumed to begin at 0 hours UT.
c
c  Input:
c    calday   (Gregorian) Calendar day (UT time).
c             The input must be one of the following forms:
c                     `yymmmdd.dd'                     (D)
c             or:
c                     `dd/mm/yy'                       (F)
c             or:
c                     `[yymmmdd:][hh[:mm[:ss.s]]]'     (H)
c             or:
c                     `ccyy-mm-dd[Thh[:mm[:ss.s]]]'    (T)
c             or:
c                     `dd-mmm-ccyy'                    (V)
c             where the brackets delimit optional values.
c
c             If dashes and the literal `T' are present, then the
c             (T) format is assumed and decoded.  The presence of
c             slashes (/) indicate the (F) format and the presence
c             of dashes and an alpha month indicate the (V) format.
c             The presence (or lack of) a period (`.') immediately
c             after the `yymmmdd' string determines whether to use
c             the (D) or (H) formats.  If the period is present,
c             the entire decimal day string is expected to be present
c             (ie. `yymmmdd.dd').  If the (H) format is entered, then
c             the presence (or lack of) the `yymmmdd' string determines
c             the range of the output value.  Without the `yymmmdd'
c             string, the output julian date will be in the range [0, 1].
c             If the `yymmmdd' portion of the string is present, then
c             the full julian date is computed.  If the (H) format is
c             entered but no time string is included, then the trailing
c             colon may be omitted (eg. `98feb01').
c
c             When the input string is of the (H) type, it may
c             be abbreviated with the missing entries internally
c             filled with a value of zero.  With the `yymmmdd' string
c             included, the year, month, and day values are decoded
c             and then the string is decoded for the time value.
c             Without the `yymmmdd' string or after it has been
c             decoded, the routine will interpret the string up to
c             the next colon as `hh'; text after that colon as `mm';
c             and any text after the next colon as `ss.s'.  Hence,
c             an input string of `3' will be interpreted as if it were
c             entered as `03:00:00.0'.  Also, if the input string was
c             `3:10', it will be interpreted as `03:10:00.0'.
c
c  Output:
c    julian   Julian date (may be an offset date; see `calday' above).
c--
c-----------------------------------------------------------------------
      character PROG*8
      parameter(PROG='DayJul: ')
c
      double precision f
      integer j, a, b, c, z
      integer nchar, value, zero
      integer ndash, nslash, ndot, ncolon
      integer month, year, day, hr, minute, sec, dsec, decday
      integer iarray(3)
      integer days(12)
      character errmsg*80
      logical alpha, fset, ok
c
      integer Len1
      logical Isalphaf
      double precision caljul
c
      data days / 31,   29,   31,   30,   31,   30,
     *            31,   31,   30,   31,   30,   31/
c
c  Remove leading blanks and find if there is a decimal point.
c
      julian = 0.0
      a = 1
      z = Len1(calday)
      if (z .lt. 1) z = 1
   10 continue
      if (a .ge. z) then
        errmsg = PROG // 'Date format incorrect ' // calday(1:z)
        nchar = Len1(errmsg)
        call bug('f', errmsg(1:nchar))
      endif
      if (calday(a:a) .le. ' ') then
        a = a + 1
        goto 10
      endif
      b = index(calday(a:z), '.')
      if (b .eq. 0) b = z + 1
c
c  Initialize all integers in case any are not present in the string.
c
      year = 0
      month = 0
      day = 0
      hr = 0
      minute = 0
      sec = 0
      dsec = 0
      decday = 0
      f = 0
      fset = .FALSE.
      alpha = .TRUE.
      ok = .TRUE.
      zero = ichar('0')
c
c  Find the input format of the string.
c
      ndash  = index(calday(a:b-1), '-')
      nslash = index(calday(a:b-1), '/')
      ncolon = index(calday(a:b-1), ':')
      ndot   = index(calday(a:z),   '.')
c
      if (nslash .gt. 0) then
c       Format F
        call datepars(calday, a, b, .FALSE., '/', iarray, ok)
        day = iarray(1)
        month = iarray(2)
        year = iarray(3) + 1900
        if (year .lt. 1950) year = year + 100
      else if (ndash .gt. 0) then
c       Format T or V
        if (Isalphaf(calday(ndash+1:ndash+1))) then
c         Format V
          call datepars(calday, a, b, .TRUE., '-', iarray, ok)
          day = iarray(1)
          month = iarray(2)
          year = iarray(3)
        else
c         Format T
          c = index(calday(a:b-1), 'T')
          if (c .eq. 0) c = b
          call datepars(calday, a, c, .FALSE., '-', iarray, ok)
          day = iarray(3)
          month = iarray(2)
          year = iarray(1)
c
c         If there is more, then there must be a 'T' separating
c         the rest.  Check that it is there and then skip over it.
c
          if (ok .and. (a .lt. b)) then
            if (calday(a:a) .ne. 'T') then
              errmsg = PROG // 'Date T-format incorrect ' // calday(1:z)
              nchar = Len1(errmsg)
              call bug('f', errmsg(1:nchar))
            endif
            a = a + 1
          endif
        endif
      else if (ncolon .gt. 0) then
c       Format H
        alpha = .FALSE.
        do j = a, (ncolon - 1)
          if (Isalphaf(calday(j:j))) alpha = .TRUE.
        enddo
c
c       If there is alpha text before the first colon, then it is the
c       month designation.  Get it and skip over the colon separator.
c
        if (alpha) then
          call datepars(calday, a, ncolon, .TRUE., ' ', iarray, ok)
          day = iarray(3)
          month = iarray(2)
          year = iarray(1) + 1900
          if (year .lt. 1950) year = year + 100
          a = a + 1
        endif
      else if (ndot .gt. 0) then
c       Format D
        call datepars(calday, a, ndot, .TRUE., ' ', iarray, ok)
        day = iarray(3)
        month = iarray(2)
        year = iarray(1) + 1900
        if (year .lt. 1950) year = year + 100
c
c       Now get the fractional day part.
c       Start by removing any trailing zeros.
c
        if (b .lt. z) then
          do while ((z .gt. b) .and. (ichar(calday(z:z)) .eq. zero))
            z = z - 1
          enddo
          j = b + 1
c
c         Add a sanity check for too many digits in fractional day.
c
          if (z .gt. (j + 6)) z = j + 6
          nchar = 0
          do while ((ok) .and. (j .le. z))
            value = ichar(calday(j:j)) - zero
            if ((value .ge. 0) .and. (value .le. 9)) then
              decday = (decday * 10) + value
              nchar = nchar - 1
            else
              ok = .FALSE.
            endif
            j = j + 1
          enddo
          if ((ok) .and. (decday .gt. 0)) then
            f = decday * 10.0**(nchar)
            fset = .TRUE.
          endif
        endif
      else if (((b - a) .eq. 7) .and. Isalphaf(calday(a+2:a+2))) then
c       Format H (with no time string)
        alpha = .TRUE.
        call datepars(calday, a, b, .TRUE., ' ', iarray, ok)
        day = iarray(3)
        month = iarray(2)
        year = iarray(1) + 1900
        if (year .lt. 1950) year = year + 100
        a = a + 1
      else
        errmsg = PROG // 'Unrecognized Date format: ' // calday(1:z)
        nchar = Len1(errmsg)
        call bug('f', errmsg(1:nchar))
      endif
c
c  Check that all is OK so far.
c
      if (.not. ok) then
        errmsg = PROG // 'Incorrectly formatted date ' // calday(1:z)
        nchar = Len1(errmsg)
        call bug('f', errmsg(1:nchar))
      endif
c
c  Check the month and the day of the month.
c
      if (year .gt. 0) then
        if ((month .lt. 1) .or. (month .gt. 12)) then
          errmsg = PROG // 'Bad month: '// calday(1:z)
          call bug('f', errmsg)
        endif
        if ((day .lt. 1) .or. (day .gt. days(month))) then
          errmsg = PROG // 'Bad number of days in month: '// calday(1:z)
          call bug('f', errmsg)
        endif
      endif
c
c  At this point, the year, month, and day have been extracted.  If
c  there is anything left, it should be a time designation like:
c    hh[:mm[:ss.ss]]
c
      if (.not. fset) then
        value = 0
        do while ((ok) .and. (a .lt. b))
          if (calday(a:a) .eq. ':') then
            value = value + 1
          else
            j = ichar(calday(a:a)) - zero
            ok = ((j .ge. 0) .and. (j .le. 9))
            if (value .eq. 0) then
              hr = (hr * 10) + j
            else if (value .eq. 1) then
              minute = (minute * 10) + j
            else if (value .eq. 2) then
              sec = (sec * 10) + j
            else
              ok = .FALSE.
            endif
          endif
          a = a + 1
        enddo
c
c  Check the hours, minutes and seconds.
c
        if(hr.gt.23.or.minute.gt.59.or.sec.gt.59)then
          errmsg = PROG // 'Bad hh:mm:ss format found: ' // calday(1:z)
          call bug('f', errmsg)
        endif
c
c  Are there any fractional seconds?  If so, get them too.
c
        if ((ok) .and. (b .lt. z)) then
          do while ((z .gt. b) .and. (ichar(calday(z:z)) .eq. zero))
            z = z - 1
          enddo
          j = b + 1
c
c  Add a sanity check for too many digits in fractional day.
c
          if (z .gt. (j + 6)) z = j + 6
c
          nchar = 0
          do while ((ok) .and. (j .le. z))
            value = ichar(calday(j:j)) - zero
            if ((value .ge. 0) .and. (value .le. 9)) then
              dsec = (dsec * 10) + value
              nchar = nchar + 1
            else
              ok = .FALSE.
            endif
            j = j + 1
          enddo
          if ((ok) .and. (dsec .gt. 0)) then
            f = dsec / (3600.0D0 * 10**(nchar))
          endif
        endif
c
c  Add in the fractional day material.
c
        f = f + (hr + (minute / 60.0D0) + (sec / 3600.0D0))
        f = f / 24.0D0
      endif
c
c  If things are not okay, then some part of the string
c  was formatted incorrectly.
c
      if (.not. ok) then
        errmsg = PROG // 'Incorrectly formatted date ' // calday(1:z)
        call bug('f', errmsg)
      endif
c
c  f is now in units of days.  The 'H' style permits fractional days;
c  this will only happen when alpha = FALSE; otherwise the year, month,
c  and fractional day information are combined to create a Julian day.
c
      f = f + day
      if (alpha) then
        julian = caljul(year, month, f)
      else
        julian = f
      endif
c
      end
c************************************************************************
      subroutine datepars(calday, a, z, alpha, delim, iarray, ok)
      implicit none
      character calday*(*)
      character delim*1
      integer a, z
      integer iarray(3)
      logical alpha, ok
c
c  Extract the year, month, and decimal day from the input string.
c  Which index holds which value is format specific and the caller
c  should take care that they get a proper range of values back.
c
c  Inputs:
c    calday     The string to parse.
c    a          The first index of calday to parse.
c    z          One index position beyond the end of calday to parse.
c    alpha      If true, get the month as a string; otherwise, a number.
c    delim      If non-blank, it specifies the field delimiter.
c  Outputs:
c    a          Where the scanning stopped.  It should be equal to z.
c    iarray     Array of year, month, day (format specific).  Strings
c               are converted to month numbers.
c    ok         True if decoding went okay.
c------------------------------------------------------------------------
      character mon*3
      character errmsg*80
      character months(12)*3
      integer j, k, mk, zero
      integer mmonth(12)
c
      integer binsrcha
      logical Isalphaf
c
      data months/'APR','AUG','DEC','FEB','JAN','JUL',
     *            'JUN','MAR','MAY','NOV','OCT','SEP'/
      data mmonth/    4,    8,   12,    2,    1,    7,
     *                6,    3,    5,   11,   10,    9/
c
      mon = ' '
      zero = ichar('0')
      k = 1
      iarray(k) = 0
c
      if (alpha) then
        do while ((ok) .and. (a .lt. z))
          j = ichar(calday(a:a)) - zero
	  if (calday(a:a).eq.' ')then
	    continue
          else if (Isalphaf(calday(a:a))) then
            if (mon .ne. ' ') then
              ok = .FALSE.
            else
              mon = calday(a:a+2)
              a = a + 2
              if (delim .eq. ' ') then
                mk = k + 1
                k = k + 2
                if (k .gt. 3) then
                  errmsg = 'Badly formatted date string: ' // calday
                  call bug('f', errmsg)
                endif
                iarray(k) = 0
              else
                mk = k
              endif
            endif
          else if ((j .ge. 0) .and. (j .le. 9)) then
            iarray(k) = (iarray(k) * 10) + j
          else if (calday(a:a) .eq. delim) then
            k = k + 1
            if (k .gt. 3) then
              errmsg = 'Badly formatted date string: ' // calday
              call bug('f', errmsg)
            endif
            iarray(k) = 0
          else
            ok = .FALSE.
          endif
          if (ok) a = a + 1
        enddo
c
c  Convert the month string to an integer.
c
        if (mon .eq. ' ') then
          errmsg = 'Badly formatted date string: ' // calday
          call bug('f', errmsg)
        endif
        call Ucase(mon)
        j = binsrcha(mon, months, 12)
        if (j .eq. 0) then
          errmsg = 'Unrecognised month: ' // calday
          call bug('f', errmsg)
        endif
        iarray(mk) = mmonth(j)
      else
        do while ((ok) .and. (a .lt. z))
          j = ichar(calday(a:a)) - zero
	  if(calday(a:a).eq.' ')then
	    continue
          else if ((j .ge. 0) .and. (j .le. 9)) then
            iarray(k) = (iarray(k) * 10) + j
          else if (calday(a:a) .eq. delim) then
            k = k + 1
            if (k .gt. 3) then
              call bug('f', 'Badly formatted date string')
            endif
            iarray(k) = 0
          else
            ok = .FALSE.
          endif
          if (ok) a = a + 1
        enddo
      endif
      return
      end
c************************************************************************
      double precision function caljul(yy, mm, dd)
c
      implicit none
      integer yy, mm
      double precision dd
c
c  Compute the Julian day based on input year, month, and decimal day.
c
c  Inputs:
c    yy         Year (e.g. 1990).
c    mm         Month (1-12).
c    dd         Day of the month, and fraction of a day (1-31.99999...).
c------------------------------------------------------------------------
      integer GREG
c     parameter(GREG=15+31*(10+12*1582))
      parameter(GREG=588829)
c
      integer a, z, yy1, mm1
c
      z = dd + (31 * (mm + (12 * yy)))
      if (mm .gt. 2) then
        mm1 = mm + 1
        yy1 = yy
      else
        yy1 = yy - 1
        mm1 = mm + 13
      endif
      caljul = 1720994.5D0 + int(365.25*yy1) + int(30.6001*mm1) + dd
      if (z .ge. GREG) then
        a = int(0.01 * yy1)
        caljul = caljul + 2 - a + int(0.25 * a)
      endif
c
      end
c***********************************************************************
c* TodayJul -- Format the current day into a Julian day.
c& jm
c: Julian-day, date, utilities
c+
      subroutine todayjul(julian)
c
      implicit none
      double precision julian
c
c  Convert the current date to Julian date.  This is not to high
c  accuracy, but it achieves the accuracy required. See "Astronomical
c  Formulae for Calculators", Jean Meeus (Wiillmann-Bell Inc).
c  The day is assumed to begin at 0 hours UT.
c
c  Input:
c    none
c
c  Output:
c    julian      Julian date.
c--
c-----------------------------------------------------------------------
      integer iarray(3), jarray(3)
      integer year, hr, minute, sec, month
      double precision day
c
      double precision caljul
c
c  Get the current date.
c
      call mitime(jarray)
      call midate(iarray)
c
      day =   iarray(1)
      month = iarray(2)
      year =  iarray(3)
      hr =     jarray(1)
      minute = jarray(2)
      sec =    jarray(3)
      day = day + ((hr + (minute + (sec / 60.0D0)) / 60.0D0) / 24.0D0)
      julian = caljul(year, month, day)
c
      return
      end
#ifdef TEST
c************************************************************************
      program test
      character string*50
      character fmt(5)*1
      integer j
      double precision jul, jul2
c
      data fmt / 'D', 'F', 'H', 'T', 'V'/
c
      call todayjul(jul)
      write (*,*) 'TodayJul => ', jul
      write (*,*) ' '
c
      do j = 1, 5
        write (*,*) 'Format => ', fmt(j)
        call julday(jul, fmt(j), string)
        write (*,*) 'JulDay => ', string
        call dayjul(string, jul2)
        write (*,*) 'DayJul => ', jul2
        write (*,*) ' '
      enddo
c
      string = '18/ 1/95'
      write (*,*) 'string => ', string
      call dayjul(string, jul2)
      write (*,*) 'DayJul => ', jul2
      write (*,*) ' '
c      
      string = '98feb01'
      write (*,*) 'string => ', string
      call dayjul(string, jul2)
      write (*,*) 'DayJul => ', jul2
      write (*,*) ' '
c
      write (*,*) '== Now test for mistakes (last one should bomb) =='
      write (*,*) ' '
      write (*,*) 'Format => X [Should not exist]'
      call julday(jul, 'X', string)
      write (*,*) 'JulDay => ', string
      write (*,*) ' '
      string = '97ocr11.5'
      write (*,*) 'Incorrect Date String => ', string
      call dayjul(string, jul2)
      end
#endif
