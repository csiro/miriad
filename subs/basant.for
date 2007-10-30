c***********************************************************************
c  Returns the antenna numbers corresponding to a given baseline.
c
c  History:
c    jm    05dec90    Initial version.
c    jm    21sep92    Modified to call BUG if ant1>ant2.
c    pjt    8mar95    &!@$*(*& when ant1||ant2 out of range 1..256
c    rjs   27oct00    New baseline numbering convention.
c***********************************************************************
c* BasAnt - determine antennas from baseline number.
c& jm
c: calibration, uv-i/o, uv-data, utilities
c+
      subroutine basant(baseline, ant1, ant2)
      implicit none
      integer ant1, ant2
      double precision baseline
c
c  BasAnt is a Miriad routine that returns the antenna numbers that are
c  required to produce the input baseline number.  According to the
c  Miriad programming manual, the relationship between the baseline
c  and the antenna numbers is defined as either
c    baseline = (Ant1 * 256) + Ant2.
c  or
c    baseline = (Ant1 * 2048) + Ant2 + 65536.
c
c  Note:  Because Ant1 is ALWAYS suppose to be less than Ant2,
c         it is considered a fatal error if Ant2 is larger than
c         Ant1.  (No restriction is placed on the condition Ant1
c         equal to Ant2 to allow for autocorrelation data)
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
c    ant1     The first antenna number.
c    ant2     The second antenna number.
c
c--
c-----------------------------------------------------------------------
      integer mant
c
      ant2 = nint(baseline)
      if(ant2.gt.65536)then
	ant2 = ant2 - 65536
	mant = 2048
      else
	mant = 256
      endif
      ant1 = ant2 / mant
      ant2 = ant2 - (ant1 * mant)
      if (max(ant1,ant2).ge. mant) call bug('f', 
     *  'BASANT: possibly a bad baseline number!')
      if (ant1 .gt. ant2) call bug('f', 
     *  'BASANT: a1>a2: Incorrectly formatted baseline number!')
      if (ant1 .lt. 1) call bug('f', 
     *  'BASANT: ant1 < 1: possibly a bad baseline number!')
      if (ant2 .lt. 1) call bug('f', 
     *  'BASANT: ant2 < 1: possibly a bad baseline number!')
      return
      end
c************************************************************************
	double precision function antbas(i1,i2)
c
	implicit none
	integer i1,i2
c
c  Determine the baseline number of a pair of antennas.
c
c  Note: i1 <= i2.
c
c------------------------------------------------------------------------
	if(i1.gt.i2)call bug('f','Illegal baseline number in antbas')
	if(i2.gt.255)then
	  antbas = 2048*i1 + i2 + 65536
	else
	  antbas =  256*i1 + i2
	endif
	end
