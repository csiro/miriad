c* deghms - write out ra and dec in hms/dms from input in degrees
c& bpw
c: coordinates
c+
      subroutine deghms ( a, d, radec )
      double precision a, d
      character*(*)    radec
 
c deghms takes as input the right ascension and declination in degrees and
c constructs the string 'hh mm ss.ss sdd mm ss.ss'.
c characters 1 through 12 correspond to ra, 13 through 24 to dec.
c Care is taken that hours are between 0 and 24, minutes and seconds between
c 0 and 60. I.e. ra=0 60 00.0 can not occur.
c Further, if minutes or seconds are less than 10, an extra 0 is added, i.e.
c 9 minutes is written as 09 minutes.
c Finally, if hours or degrees are less than 10, there is no extra space, i.e.
c 1 hours comes out as 'ra=1 00 00.0', not 'ra= 1 00 00.0' and -1 degree comes
c out as 'dec=-1 00 00.0', not 'dec=- 1 00 00.0'.
c
c Input:
c   a:     right ascension in degrees
c   d:     declination in degrees
c
c Output:
c   radec: resulting output string
c--
c
c History:
c   bpw:   11sep90   added some niceties to lgm's version (really: rewritten)
c   rjs	   27mar91   Corrected a use of non-standard FORTRAN, which was causing
c		     a problem on a Convex
c   bpw    12aug91   Add rounding of degrees
c   bpw    08nov91   Add a range test, because one user found it is possible
c                    that radhms is called with degrees, giving weird degrees
c                    inside deghms.
c   bow    16feb93   inputs and calculations now are double precision
c
c-----------------------------------------------------------------------

      integer          hour, deg
      integer          min
      double precision sec
      double precision rest
      character*30     string
      character*12     string1
      integer          rtf(2), nfigi, nfigd
      character*80     rtfmt
      double precision a1, d1, round
      double precision radian
      parameter ( round = 0.5/360000. )
      radian=dacos(-1.d0)/180.

      a1 = a + round
      d1 = dsign( 1.d0, d ) * dabs( d + round )

      if( a1.lt.0. .or. a1.gt.360. ) then
         call bug( 'w', 'RA <0 or >24; wrapped into this range' )
         rtf(1) = nfigd( radian*a1 ) + 2
         write( string, rtfmt(' ''pi/180*RA is: '',f<>.1',rtf,1 ) )
     *          radian*a1
         call bug( 'i', string )
         if( a1.lt.  0. ) a1 = dmod( a1, 360.d0 ) + 360.d0
         if( a1.gt.360. ) a1 = dmod( a1, 360.d0 )
      endif
      if( d1.lt.-90. .or. d1.gt.90. ) then
         call bug( 'w', 'Dec <-90 or >90; wrapped into this range' )
         rtf(1) = nfigd( radian*d1 ) + 2
         write( string, rtfmt(' ''pi/180*dec is: '',f<>.1',rtf,1 ) )
     *          radian*d1
         call bug( 'i', string )
         d1 = d1 + 180.
         if( d1.lt.  0. ) d1 = dmod( d1, 180.d0 ) + 180.d0
         if( d1.gt.180. ) d1 = dmod( d1, 180.d0 )
         d1 = d1 - 180.
      endif

c- separate right ascension
      hour = a1/15.
      rest = a1 - dble(hour*15)
      min  = idint( rest * 4. )
      rest = rest - dble(min) / 4.
      sec  = rest * 240.
c-    make sure that sec, min and hour are between
c-    0 & 59, 0 & 59 and 0 & 24, respectively
      if( dabs(sec-60.d0) .lt. 1.e-5 ) min  = min + 1
      if( dabs(sec-60.d0) .lt. 1.e-5 ) sec  = 0.
      if( min             .eq. 60    ) hour = hour + 1
      if( min             .eq. 60    ) min  = 0
      if( hour            .eq. 24    ) hour = 0
      rtf(1) = nfigi(hour)
      rtf(2) = 2-rtf(1)
      if( rtf(2).eq.0 ) write ( string(1:11), rtfmt(
     *    'i<>,1x,i2,1x,f5.2',    rtf,1 ) ) hour, min, sec
      if( rtf(2).ne.0 ) write ( string(1:11), rtfmt(
     *    'i<>,1x,i2,1x,f5.2,<>x',rtf,2 ) ) hour, min, sec
c-    if min and sec are less than 10, change ' ' to 0
      if( min.lt.10  ) string(2+rtf(1):2+rtf(1)) = '0'
      if( sec.lt.10. ) string(5+rtf(1):5+rtf(1)) = '0'

      string(12:12) = ' '

c- separate declination
      deg  = idint( dabs(d1) )
      rest = dabs(d1) - dble(deg)
      min  = idint( rest * 60. )
      rest = rest - dble(min) / 60.
      sec  = rest * 3600.
c-    make sure that sec and min are between 0 & 59
      if( dabs(sec-60.d0) .lt. 1.e-5 ) min = min + 1
      if( dabs(sec-60.d0) .lt. 1.e-5 ) sec = 0.
      if( min             .eq. 60    ) deg = deg + 1
      if( min             .eq. 60    ) min = 0
      rtf(1) = nfigi(deg)
      rtf(2) = 3-rtf(1)
      if( rtf(2).eq.0 ) write ( string(13:24), rtfmt(
     *    'i<>,1x,i2,1x,f5.2',    rtf,1 ) ) deg,min,sec
      if( rtf(2).ne.0 ) write ( string(13:24), rtfmt(
     *    'i<>,1x,i2,1x,f5.2,<>x',rtf,2 ) ) deg,min,sec
c-    if min and sec are less than 10, change ' ' to 0
      if( min.lt.10  ) string(14+rtf(1):14+rtf(1)) = '0'
      if( sec.lt.10. ) string(17+rtf(1):17+rtf(1)) = '0'
      if( d1 .lt.0.  ) then
        string1 = '-'//string(13:23)
        string(13:24) = string1
      endif

      radec = string

      return
      end


c* radhms - write out ra and dec in hms/dms from input in radians
c& bpw
c: coordinates
c+
      subroutine radhms ( a, d, radec )
      double precision a, d
      character*(*)    radec
 
c radhms takes as input the right ascension and declination in radians and
c constructs the string 'hh mm ss.ss sdd mm ss.ss'.
c characters 1 through 12 correspond to ra, 13 through 24 to dec.
c Care is taken that hours are between 0 and 24, minutes and seconds between
c 0 and 60. I.e. ra=0 60 00.0 can not occur.
c Further, if minutes or seconds are less than 10, an extra 0 is added, i.e.
c 9 minutes is written as 09 minutes.
c Finally, if hours or degrees are less than 10, there is no extra space, i.e.
c 1 hours comes out as 'ra=1 00 00.0', not 'ra= 1 00 00.0' and -1 degree comes
c out as 'dec=-1 00 00.0', not 'dec=- 1 00 00.0'.
c
c Input:
c   a:     right ascension in radians
c   d:     declination in radians
c
c Output:
c   radec: resulting output string
c--
c
c History:
c   bpw    11sep90   created
c   bpw    16feb93   inputs change to double precision
c
c-----------------------------------------------------------------------
      double precision radian
      radian = 180. / dacos(-1.d0)
      call deghms( a*radian, d*radian, radec )
      return
      end
