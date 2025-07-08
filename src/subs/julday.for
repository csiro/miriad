c***********************************************************************
c
c  History:
c    rjs    ???????  Original version.
c     jm    22feb90  Switched to new format of date.
c    rjs     9mar90  Fixed a portability bug. Changed some DATA to
c		     PARAMETER statements. Some work on getting it to
c		     work on VMS.
c     jm     9mar90  Removed string cats to a variable for bug.for and
c		     changed list direct reads to standard formats.
c     jm    13mar90  Corrected julian time so it is based on UT and
c		     fixed JulDay roundoff error.
c    rjs    27mar90  Deleted JulCal.
c    rjs     4aug90  Fixed minor portability problem in a FORMAT statement.
c     jm    11feb91  Let JulDay work with abbreviated input strings.
c     jm    11feb91  Changed min to minute to avoid intrinsic func name.
c     jm    15feb91  Fixed a bug that occurred when a call to index was
c		     used to replace a loop in DayJul.
c     jm    20feb91  Modified DayJul to be a bit more flexible and allow
c		     strings with only time (eg. hh only).
c     jm    26feb91  Changed references of Isalpha to Isalphaf.
c     jm    27feb91  Corrected a bug with the year 2000 and changed the
c		     way the hh:mm:ss (or subset) was parsed in DayJul.
c     jm     4mar91  Fixed a fractional day parser bug in DayJul.
c    rjs     5mar91  Used ichar function to convert from char to int.
c     jm     6mar91  Corrected decimal days/secs conversion in DayJul.
c mjs/jm    11mar91  Corrected date/time call for Cray in TodayJul.
c    rjs     5sep91  Various error checking in DayJul.
c    rjs     1oct91  Added JulFDate and FDateJul routines.
c    rjs    18feb92  Improved error checking in DayJul.
c     jm    03mar92  Corrected a leap year bug in JulDay.  This meant
c		     promoting, to dble(), integers in real expressions.
c		     In particular, this happened in setting item "c".
c    rjs    23dec92  Eliminate conditional compilation, and use mitime,
c		     midate.
c    rjs    16sep93  Rename bsrch to binsrch.
c     jm    14mar94  Added check for blank input strings; removed tabs.
c    rjs    10oct94  Round date in julfdate.
c     jm    07jun96  To avoid integer overflow in dayjul, I added code
c		     to strip off trailing 0s beyond the decimal point.
c    rjs    10sep97  Added "caljul" function.
c     jm    10sep97  Added functionality for IAU 'T' format.
c     jm    11sep97  Moved fdate functionality into julday routines.
c		     Also added VMS date style dd-mmm-ccyy.
c		     Major modification of dayjul to handle new formats.
c		     Removed julfdate() and datefjul() routines (these
c		     are now handled by dayjul().
c    rjs    25sep97  Added subroutine julcal.
c    rjs    09jan97  Make datepars tolerant of spaces in names.
c    rjs    02jan06  General clean up and generalisation of format.
c    rjs    05feb06  Correct bug in above dealing with 'dtime' formats.
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
c    julian	 Julian date.
c    form	 Output selection flag.  It must be one of
c		 'D', 'F', 'H', 'T', or 'V' (case independent).
c
c  Output:
c    calday	 (Gregorian) Calendar day (UT time).
c		 The output if form = 'D' is like:
c			'yymmmdd.dd'
c		 The output if form = 'F' is like:
c			'dd/mm/yy' (loss of fractional days)
c		 The output if form = 'H' is like:
c			'yymmmdd:hh:mm:ss.s'
c		 The output if form = 'T' is like:
c			'ccyy-mm-ddThh:mm:ss.s' (the T is literal)
c		 The output if form = 'V' is like:
c			'dd-mmm-ccyy' (loss of fractional days)
c
c		 Note that cc is used to denote the century; yy the
c		 year (mod 100); mm the decimal month number (1-12);
c		 mmm a string describing the month; dd the day number;
c		 dd.dd the fractional day number; and hh:mm:ss.s the
c		 hours, minutes, and seconds of the day.
c
c--
c-----------------------------------------------------------------------
      double precision FUDGE
c     parameter(fudge=0.5/(24.0*60.0*60.0*10.0))
      parameter(FUDGE=1.0/1728000.0)
c
      double precision f
      integer month,year,day,hr,minute,sec
      integer dsec, decday, nchar, century
      character months(12)*3
      character outstr*25
c
      data months/'JAN','FEB','MAR','APR','MAY',
     *	    'JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
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
      if(form.eq.'d'.or.form.eq.'D')then
	write (outstr, 10) year, months(month), day, decday
   10	format(i2.2, a, i2.2, '.', i2.2)
	nchar = 10
      else if(form.eq.'f'.or.form.eq.'F')then
	write (outstr, 20) day, month, year
   20	format(i2.2, '/', i2.2, '/', i2.2)
	nchar = 8
      else if(form.eq.'h'.or.form.eq.'H')then
	write (outstr, 30) year, months(month), day,
     *	  hr, minute, sec, dsec
   30	format(i2.2, a, i2.2, ':', i2.2, ':', i2.2, ':', i2.2, '.', i1)
	nchar = 18
      elseif(form.eq.'t'.or.form.eq.'T')then
	write (outstr, 40) century, year, month, day,
     *	  hr, minute, sec, dsec
   40	format(i2.2, i2.2, '-', i2.2, '-', i2.2, 'T',
     *	  i2.2, ':', i2.2, ':', i2.2, '.', i1)
	nchar = 21
      elseif(form.eq.'v'.or.form.eq.'V')then
	write (outstr, 50) day, months(month), century, year
   50	format(i2.2, '-', a, '-', i2.2, i2.2)
	nchar = 11
      else
	call bug('f','Unrecognized style format in julday')
      endif
      calday = outstr(1:nchar)
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
c	      The input must be one of the following forms:
c		      `yymmmdd.dd'		       (D)
c	      or:
c		      `dd/mm/yy'		       (F)
c	      or:
c		      `[yymmmdd:][hh[:mm[:ss.s]]]'     (H)
c	      or:
c		      `ccyy-mm-dd[Thh[:mm[:ss.s]]]'    (T)
c	      or:
c		      `dd-mmm-ccyy'		       (V)
c	      where the brackets delimit optional values.
c
c	      If dashes and the literal `T' are present, then the
c	      (T) format is assumed and decoded.  The presence of
c	      slashes (/) indicate the (F) format and the presence
c	      of dashes and an alpha month indicate the (V) format.
c	      The presence (or lack of) a period (`.') immediately
c	      after the `yymmmdd' string determines whether to use
c	      the (D) or (H) formats.  If the period is present,
c	      the entire decimal day string is expected to be present
c	      (ie. `yymmmdd.dd').  If the (H) format is entered, then
c	      the presence (or lack of) the `yymmmdd' string determines
c	      the range of the output value.  Without the `yymmmdd'
c	      string, the output julian date will be in the range [0, 1].
c	      If the `yymmmdd' portion of the string is present, then
c	      the full julian date is computed.  If the (H) format is
c	      entered but no time string is included, then the trailing
c	      colon may be omitted (eg. `98feb01').
c
c	      When the input string is of the (H) type, it may
c	      be abbreviated with the missing entries internally
c	      filled with a value of zero.  With the `yymmmdd' string
c	      included, the year, month, and day values are decoded
c	      and then the string is decoded for the time value.
c	      Without the `yymmmdd' string or after it has been
c	      decoded, the routine will interpret the string up to
c	      the next colon as `hh'; text after that colon as `mm';
c	      and any text after the next colon as `ss.s'.  Hence,
c	      an input string of `3' will be interpreted as if it were
c	      entered as `03:00:00.0'.	Also, if the input string was
c	      `3:10', it will be interpreted as `03:10:00.0'.
c
c  Output:
c    julian   Julian date (may be an offset date; see `calday' above).
c--
c------------------------------------------------------------------------
	integer days(12),mmonth(12),i,length,zero,day,month,year,n
	integer id
	logical more,amonth
	double precision fday
	character months(12)*3,mon*3,c*1,sep*1
c
c  Externals.
c
	integer binsrcha,len1
	double precision caljul
c
c  Data statements.
c
        data days / 31,	 29,   31,   30,   31,	30,
     *		    31,	 31,   30,   31,   30,	31/
	data months/'APR','AUG','DEC','FEB','JAN','JUL',
     *		    'JUN','MAR','MAY','NOV','OCT','SEP'/
	data mmonth/   4,    8,	 12,	2,    1,    7,
     *		       6,    3,	  5,   11,   10,    9/
c
	zero = ichar('0')
	i = 1
	length = len1(calday)
c
c  Skip leading white characters.
c
	i = 1
	more = .true.
	dowhile(more.and.i.le.length)
	  more = calday(i:i).le.' '
	  if(more)i = i + 1
	enddo
c
c  Get the first number.
c
	id = i
	n = 0
	day = 0
	more = .true.
	dowhile(more.and.i.le.length)
	  more = calday(i:i).ge.'0'.and.calday(i:i).le.'9'
	  if(more)then
	    day = 10*day + ichar(calday(i:i)) - zero
	    i = i + 1
	    n = n + 1
	  endif
	enddo
	if(n.eq.0.or.i.ge.length)call daybug(calday,'Invalid date')
c
c  Get the separator character. If its not alphabetic, it must be a separator.
c
	c = calday(i:i)
	if((c.ge.'a'.and.c.le.'z').or.(c.ge.'A'.and.c.le.'Z'))then
	  sep = ' '
	else if(c.eq.':')then
	  call decangle(calday(id:length),julian,'dtime',more)
	  if(.not.more)call daybug(calday,'Invalid format')
	  return
	else
	  i = i + 1
	  sep = c
	endif
	if(i.ge.length)call daybug(calday,'Invalid date format')
c
c  Get the month field. This can be either alphabetic or numeric.
c
	c = calday(i:i)
	amonth = (c.ge.'a'.and.c.le.'z').or.(c.ge.'A'.and.c.le.'Z')
	if(amonth)then
	  id = i
	  more = .true.
	  dowhile(more.and.i.le.length)
	    c = calday(i:i)
	    more = (c.ge.'a'.and.c.le.'z').or.(c.ge.'A'.and.c.le.'Z')
	    if(more)i = i + 1
	  enddo
	  mon = calday(id:i-1)
	  call ucase(mon)
	  month = binsrcha(mon,months,12)
	  if(month.eq.0)call daybug(calday,'Invalid month')
	  month = mmonth(month)
	else
	  n = 0
	  month = 0
	  more = .true.
	  dowhile(more.and.i.le.length)
	    more = calday(i:i).ge.'0'.and.calday(i:i).le.'9'
	    if(more)then
	      month = 10*month + ichar(calday(i:i)) - zero
	      i = i + 1
	      n = n + 1
	     endif
	  enddo
	  if(n.eq.0)call daybug(calday,'Invalid date')
	endif
	if(i.gt.length)call daybug(calday,'Invalid date')
c
c  Get the separator character. If its not alphabetic, it must be a separator.
c
	c = calday(i:i)
	if(c.ge.'0'.and.c.le.'9')then
	  c = ' '
	else
	  i = i + 1
	endif
	if(c.ne.sep)call daybug(calday,'Invalid date format')
	if(i.gt.length)call daybug(calday,'Invalid date format')
c
c  Finally get the final number.
c
	n = 0
	year = 0
	more = .true.
	dowhile(more.and.i.le.length)
	  more = calday(i:i).ge.'0'.and.calday(i:i).le.'9'
	  if(more)then
	    year = 10*year + ichar(calday(i:i)) - zero
	    i = i + 1
	    n = n + 1
	  endif
	enddo
	if(n.eq.0)call daybug(calday,'Invalid date')
c
c  Flip around day and year if needed.
c
	if(sep.eq.' '.or.(sep.eq.'-'.and..not.amonth))then
	  n = year
	  year = day
	  day = n
	endif
c
c  Radio astronomy was not invented in the first century.
c
	if(year.lt.40)then
	  year = year + 2000
	else if(year.lt.100)then
	  year = year + 1900
	endif
c
c  Sanity check on the day of the month and month of the year.
c
	if(month.eq.0.or.month.gt.12)call daybug(calday,'Invalid month')
	if(day.eq.0.or.day.gt.days(month))
     *	  call daybug(calday,'Invalid day of the month')
c
c  Get the day fraction, if there is one.
c
	if(i.lt.length)then
	  c = calday(i:i)
	  if(c.eq.'.')then
	    call atodf(calday(i:length),fday,more)
	    if(.not.more)call daybug(calday,'Invalid time of day')
	  else if(c.eq.'T'.or.c.eq.'t'.or.c.eq.':')then
	    i = i + 1
	    call decangle(calday(i:length),fday,'dtime',more)
	  else
	    call daybug(calday,'Invalid time of day')
	  endif
	else
	  fday = 0
	endif
	fday = fday + day
c
	julian = caljul(year,month,fday)
c
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
c    yy 	Year (e.g. 1990).
c    mm 	Month (1-12).
c    dd 	Day of the month, and fraction of a day (1-31.99999...).
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
c    julian	 Julian date.
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
      end
c************************************************************************
	subroutine daybug(day,message)
c
	implicit none
	character day*(*),message*(*)
c------------------------------------------------------------------------
	character string*128
	string = message//': '//day
	call bug('f',string)
	end
c************************************************************************
	integer function MonthNo(mon)
c
	implicit none
	character mon*(*)
c
c  Convert a month to a month number.
c
c------------------------------------------------------------------------
	character str*3
	integer i
	character months(12)*3
	integer mmonth(12)
c
c  Externals.
c
	integer binsrcha
c
	data months/'APR','AUG','DEC','FEB','JAN','JUL',
     *		    'JUN','MAR','MAY','NOV','OCT','SEP'/
	data mmonth/   4,    8,	 12,	2,    1,    7,
     *		       6,    3,	  5,   11,   10,    9/
c
	str = mon
	call ucase(str)
	i = binsrcha(str,months,12)
	if(i.ne.0)i = mmonth(i)
	MonthNo = i
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
      string = '18/1/95'
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
