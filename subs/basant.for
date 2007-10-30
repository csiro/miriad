c***********************************************************************
c  Returns the antenna numbers corresponding to a given baseline.
c
c  History:
c    jm    05dec90    Initial version.
c    jm    21sep92    Modified to call BUG if ant1>ant2.
c    pjt    8mar95    &!@$*(*& when ant1||ant2 out of range 1..256
c
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
c  and the antenna numbers is defined as baseline = (Ant1 * 256) + Ant2.
c
c  Note:  Because Ant1 is ALWAYS suppose to be less than Ant2,
c         it is considered a fatal error if Ant2 is larger than
c         Ant1.  (No restriction is placed on the condition Ant1
c         equal to Ant2 to allow for autocorrellation data and 
c         antenna based calibration files, see GMAKE)
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
c    ant1     The first antenna number. Guarenteed range 1 .. 256
c    ant2     The second antenna number. Guarenteed range 1 .. 256
c
c--
c-----------------------------------------------------------------------
c
      ant2 = nint(baseline)
      ant1 = ant2 / 256
      ant2 = ant2 - (ant1 * 256)
      if (ant1 .gt. ant2) call bug('f', 
     *  'BASANT: a1>a2: Incorrectly formatted baseline number!')
      if (ant1 .lt. 1) call bug('f', 
     *  'BASANT: ant1 < 1: possibly a bad baseline number!')
      if (ant2 .lt. 1) call bug('f', 
     *  'BASANT: ant2 < 1: possibly a bad baseline number!')
      if (ant1 .gt. 256) call bug('f', 
     *  'BASANT: ant1 > 256: possibly a bad baseline number!')
      if (ant2 .gt. 256) call bug('f', 
     *  'BASANT: ant2 > 256: possibly a bad baseline number!')
      return
      end
