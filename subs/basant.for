c***********************************************************************
c  Routines to deal with the  baseline <-> antennae pair   translations
c  
c  basants     baseline -> antennae pair, with or without checking
c  basant      strict check, will warn if ant1 > ant2 is used etc.
c  basanta     liberal, user must decide what to do with bad ant#'s
c  antbas      antenna pair -> baseline
c
c  History:
c    jm    05dec90    Initial version.
c    jm    21sep92    Modified to call BUG if ant1>ant2.
c    pjt    8mar95    &!@$*(*& when ant1||ant2 out of range 1..256
c    rjs   27oct00    New baseline numbering convention.
c    pjt   12may03    documented some thoughts on allowing MAXANT 32768
c                     and included maxdim.h
c    pjt   17oct03    return 0's if record has invalid baseline
c    pjt    6jan05    provide a less strict version that allows ant1 > ant2
c    pkgw   5dec11    move definition of MAXIANT here, document value
c***********************************************************************

      subroutine basant(baseline, ant1, ant2)
      implicit none
      integer ant1, ant2
      double precision baseline
      call basants(baseline,ant1,ant2,.TRUE.)
      end

      subroutine basanta(baseline, ant1, ant2)
      implicit none
      integer ant1, ant2
      double precision baseline
      call basants(baseline,ant1,ant2,.FALSE.)
      end

c* BasAnts - determine antennas from baseline number
c& pjt
c: calibration, uv-i/o, uv-data, utilities
c+
      subroutine basants(baseline, ant1, ant2, check)
      implicit none
      integer ant1, ant2
      double precision baseline
      logical check
c
c  BasAnt is a Miriad routine that returns the antenna numbers that are
c  required to produce the input baseline number.  According to the
c  Miriad programming manual, the relationship between the baseline
c  and the antenna numbers is defined as
c    baseline = (Ant1 * 256) + Ant2.
c  if Ant2 < 256 or
c    baseline = (Ant1 * 2048) + Ant2 + 65536.
c  otherwise. As should be clear, at most 2047 antennas are
c  supported, regardless of the value of the MAXANT configuration
c  parameter, because of the data storage format (see below).
c
c  The output values are not clipped to MAXANT.
c
c  Note:  Because Ant1 is ALWAYS supposed to be less than Ant2,
c         it is considered a fatal error if Ant2 is larger than
c         Ant1.  (No restriction is placed on the condition Ant1
c         equal to Ant2 to allow for autocorrelation data.) This
c         is true even if check is .FALSE.
c
c  Older versions of MIRIAD only support 255 antennas and use only the
c  first part of the encoding scheme shown above. The modified scheme
c  maintains compatibility with these datasets but also allows eight
c  times as many antennas.
c
c  The factor of 2048 in the encoding scheme is referred to as MAXIANT
c  in the implementation of BASANT. The value of MAXIANT is limited by
c  the MIRIAD visibility data format. The 'baseline' UV variable is
c  stored as a 32-bit floating point number, which means it has about 24
c  bits of available precision. MAXIANT = 2048 consumes approximately 22
c  of those bits, while MAXIANT = 4096 would push the limit. An
c  easy-to-write test program confirms that 4096 in fact fails for large
c  values of Ant1 and Ant2.
c
c  There is hypothetical room for increasing the number of representable
c  antennas because (1) MAXIANT need not be a power of 2 and (2) one
c  could offset the antenna numbers by 256 in the high-antenna-number
c  case. Note also that if the 32-bit baseline value was treated as an
c  unsigned integer and antenna numbers started at zero, one could
c  handle 65536 distinct antennas. This encoding would not, however, be
c  compatible with any existing MIRIAD datasets and would probably lead
c  to problems elsewhere within MIRIAD.
c
c
c  Input:
c    baseline The baseline number.  This value is usually obtained
c             as the fourth or fifth element in the double precision array
c             PREAMBLE (for example, see UVREAD).
c             It is the fifth element if 
c               call uvset(tvis,'preamble','uvw/time/baseline',0,0.,0.,0.)
c             was used.
c
c  Output:
c    ant1     The first antenna number. Numbered 1...MAXIANT
c    ant2     The second antenna number. Numbered 1...MAXIANT
c
c
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer MAXIANT
      parameter(MAXIANT=2048)
      integer mant
c
      ant2 = nint(baseline)
      if(ant2.gt.65536)then
	ant2 = ant2 - 65536
	mant = MAXIANT
      else
	mant = 256
      endif
      ant1 = ant2 / mant
      ant2 = ant2 - (ant1 * mant)
      if (max(ant1,ant2).ge. mant) call bug('f', 
     *  'BASANT: possibly a bad baseline number!')
      if (check) then
         if (ant1 .gt. ant2) then
            call bug('f','BASANT: a1>a2: bad baseline #!')
         endif
         if (ant1 .lt. 1) then
            call bug('f','BASANT: ant1<1: bad baseline #!')
         endif
         if (ant2 .lt. 1) then
            call bug('f','BASANT: ant2<1: bad baseline #!')
         endif
      endif
      end
c************************************************************************
c* AntBas - determine baseline from antenna numbers
c& pjt
c: calibration, uv-i/o, uv-data, utilities
c+
	double precision function antbas(i1,i2)
c
	implicit none
	integer i1,i2
c
c  Determine the baseline number of a pair of antennas.
c
c  Note: i1 <= i2.
c  See also MAXANT in maxant.h and maxantc.h on limits on i2, although
c  these limits are not enforced by ANTBAS.
c
c  See BASANT for a thorough description of the encoding scheme
c  used to determine baseline numbers.
c
c------------------------------------------------------------------------
        include 'maxdim.h'
        integer MAXIANT
        parameter(MAXIANT=2048)
	if(i1.gt.i2) then
           call bug('f','Illegal baseline number in antbas')
        endif
	if(i2.gt.255)then
	  antbas = MAXIANT*i1 + i2 + 65536
	else
	  antbas =  256*i1 + i2
	endif
	end
