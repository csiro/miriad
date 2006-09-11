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
c                   midate.
c    rjs    16sep93  Rename bsrch to binsrch.
c     jm    14mar94  Added check for blank input strings; removed tabs.
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
c    form        Output selection flag (Must be either 'H' or 'D').
c
c  Output:
c    calday      (Gregorian) Calendar day (UT time).
c                The output if form = 'D' is like:
c                       'yymmmdd.dd'
c                The output if form = 'H' is like:
c                       'yymmmdd:hh:mm:ss.s'
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
      integer z,a,b,c,d,e,alpha,month,year,day,hr,minute,sec
      integer dsec, decday,nchar
      character months(12)*3
      character outform*80, outstr*20
c
      integer Len1
c
      data months/'JAN','FEB','MAR','APR','MAY',
     *      'JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
c
      z = julian + 0.5 + FUDGE
      f = julian + 0.5 + FUDGE - z
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
      decday = 100 * (f - day)
      hr = 24 * (f - day)
      minute = 60 * (24 * (f - day) - hr)
      sec = int(600 * (60 * (24 * (f - day) - hr) - minute))
      dsec = mod(sec, 10)
      sec = sec / 10
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
      year = mod(year, 100)
c
      nchar = Len1(form)
      if (nchar .lt. 1) then
        outform = 'D'
        nchar = 1
      else
        outform = form(1:nchar)
      endif
      call Ucase(outform(1:nchar))
c
      if (index(outform(1:nchar), 'D') .ne. 0) then
        write (outstr, 10) year, months(month), day, decday
   10   format(i2.2, a, i2.2, '.', i2.2)
        nchar = 10
      else if (index(outform(1:nchar), 'H') .ne. 0) then
        write (outstr, 20) year, months(month), day,
     *    hr, minute, sec, dsec
   20   format(i2.2, a, i2.2, ':', i2.2, ':', i2.2, ':', i2.2, '.', i1)
        nchar = 18
      else
        outstr = ' '
        outform = PROG // 'Incorrect format selected ' // form(1:nchar)
        nchar = Len1(outform)
        call Bug('w', outform(1:nchar))
        nchar = 1
      endif
      nchar = min(len(calday), nchar)
      calday = outstr(1:nchar)
      return
      end
c
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
c             The input must be either of the form:
c                     ``yymmmdd.dd''
c             or:
c                     ``[yymmmdd:][hh[:mm[:ss.s]]]''
c             where the brackets delimit optional values.
c
c             The presence (or lack of) a period (``.'') immediately
c             after the ``yymmmdd'' string determines which format to
c             decode.  If the period is present, the entire string
c             is expected to be present (ie. ``yymmmdd.dd'').  If
c             the second format is entered, the presence (or lack of)
c             the ``yymmmdd'' string determines the range of the
c             output value.  Without the ``yymmmdd'' string, the
c             output julian date will be in the range [0, 1].  If
c             the ``yymmmdd'' portion of the string is present, the
c             full julian date is computed.
c
c             When the input string is of the second type, it
c             may be abbreviated with the missing entries internally
c             filled with a value of zero.  With the ``yymmmdd''
c             string included, the year, month, and day values
c             are decoded and then the string is decoded for the
c             time value.  Without the ``yymmmdd'' string or after
c             it has been decoded, the routine will interpret the
c             string up to the next colon as ``hh''; text after
c             that colon as ``mm''; and any text after the next
c             colon as ``ss.s''.  Hence, an input string of ``3''
c             will be interpreted as if it were entered as
c             ``03:00:00.0''.  Also, if the input string was
c             ``3:10'', it will be interpreted as ``03:10:00.0''.
c
c  Output:
c    julian   Julian date (may be an offset date; see ``calday'' above).
c--
c-----------------------------------------------------------------------
      character PROG*8
      parameter(PROG='DayJul: ')
c     Gregorian Calendar adopted on Oct. 15, 1582.
      integer GREG
c     parameter(GREG=15+31*(10+12*1582))
      parameter(GREG=588829)
c
      double precision f
      integer j, a, b, z
      integer ncolon, nchar, value, zero
      integer month, year, day, hr, minute, sec, dsec, decday
      integer mmonth(12),days(12)
      character months(12)*3, mon*3
      character errmsg*80
      logical alpha, fset, ok
c
      integer Len1,binsrcha
      logical Isalphaf
c
      data months/'APR','AUG','DEC','FEB','JAN','JUL',
     *            'JUN','MAR','MAY','NOV','OCT','SEP'/
      data mmonth/    4,    8,   12,    2,    1,    7,
     *                6,    3,    5,   11,   10,    9/
      data days  /   31,   29,   31,   30,   31,   30,
     *               31,   31,   30,   31,   30,   31/
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
        call Bug('f', errmsg(1:nchar))
      endif
      if (calday(a:a) .le. ' ') then
        a = a + 1
        goto 10
      endif
      b = index(calday(a:z), '.')
      if (b .eq. 0) b = z + 1
      ncolon = 0
      alpha = .FALSE.
      do j = a, (b - 1)
        if (calday(j:j) .eq. ':') then
          if (ncolon .eq. 0) value = j
          ncolon = ncolon + 1
        endif
        if (ncolon .eq. 0) then
          if (Isalphaf(calday(j:j))) alpha = .TRUE.
        endif
      enddo
      if (ncolon .ne. 0) ncolon = value
c
c  Initialize all integers in case they are not present in the string.
c
      year = 0
      mon = ' '
      month = 0
      day = 0
      hr = 0
      minute = 0
      sec = 0
      dsec = 0
      decday = 0
      f = 0
      fset = .FALSE.
      ok = .TRUE.
      zero = ichar('0')
c
c  Decode formatted string.
c  First get the yymmmdd string and, if in 'D' format, get the decimal date.
c
      if (alpha) then
        value = 0
        nchar = b
        if (ncolon .gt. 0) nchar = min(nchar, ncolon)
        do while ((ok) .and. (a .lt. nchar))
          if (Isalphaf(calday(a:a))) then
            if (mon .ne. ' '.or.a.eq.1)then
              ok = .FALSE.
            else
              value = value + 1
              mon = calday(a:a+2)
              a = a + 3
            endif
          else
            j = ichar(calday(a:a)) - zero
            ok = ((j .ge. 0) .and. (j .le. 9))
            if (value .eq. 0) then
              year = (year * 10) + j
            else if (value .eq. 1) then
              day = (day * 10) + j
            endif
            a = a + 1
          endif
        enddo
        a = a + 1
c
c  Check that all is OK.
c
        if(.not.ok)then
          errmsg = PROG // 'Incorrectly formatted date ' // calday(1:z)
          nchar = Len1(errmsg)
          call Bug('f', errmsg(1:nchar))
        endif
c
c  Check the month and the day of the month. Get them into a standard form.
c
        if(year.gt.99)then
          errmsg = PROG // 'Bad year format: ' // calday(1:z)
          call Bug('f', errmsg)
        endif
        year = year + 1900
        if (year .lt. 1950) year = year + 100
        call Ucase(mon)
        month = binsrcha(mon,months,12)
        if(month.eq.0)then
          errmsg = PROG // 'Unrecognised month: ' // calday(1:z)
          call Bug('f', errmsg)
        endif
        month = mmonth(month)
        if(day.gt.days(month)) then
          errmsg = PROG // 'Bad number of days in month: '// calday(1:z)
          call Bug('f', errmsg)
        endif
c
c  Process something in 'D' format.
c
        if ((ncolon .eq. 0) .and. (b .lt. z)) then
c
c  Get decimal day value.
c
          j = b + 1
          nchar = 0
          do while ((ok) .and. (j .le. z))
            value = ichar(calday(j:j)) - zero
            if ((value .ge. 0) .and. (value .le. 9)) then
              decday = (decday * 10) + value
              nchar = nchar + 1
            else
              ok = .FALSE.
            endif
            j = j + 1
          enddo
          if ((ok) .and. (decday .gt. 0)) then
C--old      j = -1 - (log(real(decday)) / log(10.0))
            j = -nchar
            f = decday * 10.0**j
            fset = .TRUE.
          endif
        endif
      endif
c
c  Next get the hours, minutes, seconds, and fractional seconds if any
c  are present.  This should only happen with the 'H' format or some
c  subset of it.
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
          call Bug('f', errmsg)
        endif
c
c  Are there any fractional seconds?  If so, get them too.
c
        if ((ok) .and. (b .lt. z)) then
          j = b + 1
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
C--old      j = 1 + (log(real(dsec)) / log(10.0))
            j = nchar
            f = dsec / (3600.0 * 10**j)
          endif
        endif
      endif
c
c  If things are not okay, then some part of the string
c  was formatted incorrectly.
c
      if (.not. ok) then
        errmsg = PROG // 'Incorrectly formatted date ' // calday(1:z)
        call Bug('f', errmsg)
      endif
c
c  Add in the fractional day material if it has not already been done.
c
      if (.not. fset) then
        f = f + (hr + (minute / 60.0) + (sec / 3600.0))
        f = f / 24.0
      endif
      f = f + day
c
c  f is now in units of days.  If there was a month string present,
c  then add the year and month information.  If there was no month
c  present (alpha = FALSE), then the time is a fraction of a day.
c
      if (alpha) then
        z = f + (31 * (month + (12 * year)))
        if (month .gt. 2) then
          month = month + 1
        else
          year = year - 1
          month = month + 13
        endif
        f = 1720994.5 + int(365.25 * year) + int(30.6001 * month) + f
        if (z .ge. GREG) then
          a = int(0.01 * year)
          f = f + 2 - a + int(0.25 * a)
        endif
      endif
      julian = f
      end
c************************************************************************
c* FDateJul -- Convert from 'dd/mm/yy' format into a Julian day.
c& jm
c: Julian-day, date, utilities
c+
      subroutine FDateJul(date,julian)
c
      implicit none
      double precision julian
      character date*(*)
c
c  Convert a date, given as a 'dd/mm/yy' string into a Julian day.
c
c  Input:
c    date       The date, in 'dd/mm/yy' form.
c
c  Output:
c    julian     Julian date.
c--
c-----------------------------------------------------------------------
      integer array(3),i,k
      character string*16
      character months(12)*3
      data months/'JAN','FEB','MAR','APR','MAY','JUN',
     *            'JUL','AUG','SEP','OCT','NOV','DEC'/
c
c  Decode the string.
c
      array(1) = 0
      k = 1
      do i=1,len(date)
        if(date(i:i).ge.'0'.and.date(i:i).le.'9')then
          array(k) = 10*array(k) + ichar(date(i:i)) - ichar('0')
        else if(date(i:i).eq.'/')then
          k = k + 1
          if(k.gt.3)call bug('f','Badly formatted dd/mm/yy string')
          array(k) = 0
        else if(date(i:i).ne.' ')then
          call bug('f','Badly formatted dd/mm/yy string')
        endif
      enddo
      if(k.ne.3)call bug('f','Badly formatted dd/mm/yy string')
      if(array(1).le.0.or.array(1).gt.31.or.
     *   array(2).le.0.or.array(2).gt.12.or.
     *   array(3).lt.0.or.array(3).gt.99)
     *  call bug('f','Illegal dd/mm/yy string')
c
      write(string,'(i2.2,a,i3.3)')array(3),months(array(2)),array(1)
      call DayJul(string,julian)
      return
      end
c************************************************************************
c* JulFDate -- Convert from Julian day into 'dd/mm/yy' format.
c& jm
c: Julian-day, date, utilities
c+
      subroutine JulFDate(jday,date)
c
      double precision jday
      character date*(*)
c
c  Convert a Julian day into the form 'dd/mm/yy'.
c
c  Input:
c    jday      Julian day.
c  Output:
c    date      Date in 'dd/mm/yy'.
c------------------------------------------------------------------------
      character string*24,mmm*3
      integer yy,dd,mm
c
      character months(12)*3
      integer monthno(12)
c
c  Externals.
c
      integer binsrcha
c
      data months/'APR','AUG','DEC','FEB','JAN','JUL','JUN',
     *            'MAR','MAY','NOV','OCT','SEP'/
      data monthno/ 4,    8,   12,    2,    1,    7,    6,
     *              3,    5,   11,   10,    9 /
c
      call JulDay(jday,'H',string)
      read(string,'(i2,a3,i2)')yy,mmm,dd
      mm = binsrcha(mmm,months,12)
      if(mm.eq.0) call bug('f','Could not determine date!')
      if(mm.ne.0) mm = monthno(mm)
      write(string,'(i2.2,a,i2.2,a,i2.2)')dd,'/',mm,'/',yy
      date = string
c
      return
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
      integer day, year, hr, minute, sec
      character string*16
      character months(12)*3, month*3
      data months/'JAN','FEB','MAR','APR','MAY','JUN',
     *            'JUL','AUG','SEP','OCT','NOV','DEC'/
c
c  Get the current date.
c
      julian = 0
      call mitime(jarray)
      call midate(iarray)
c
      day = iarray(1)
      month = months(iarray(2))
      year = mod(iarray(3), 100)
      hr = jarray(1)
      minute = jarray(2)
      sec = jarray(3)
      write (string, 10) year, month, day, hr, minute, sec
   10 format (i2.2, a, i2.2, ':', i2.2, ':', i2.2, ':', i2.2)
      call dayjul(string, julian)
      return
      end
