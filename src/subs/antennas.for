c---------------------------------------------------------------------
c A couple of subroutines for use by MIRIAD programs UVFLAG, UVCLIP, 
c UVPLT in calculating the baseline mask from two lists of antennas
c
c Neil Killeen, May 25 1989
c 20-jun-90  PJT  Made 'antind' symmetric
c
c*antmask - logical mask of baselines from two antenna lists
c:baselines, antenna
c+
      subroutine antmask (maxant, nant1, nant2, ant1, ant2, blmask)
c
      implicit none
      integer maxant, nant1, nant2, ant1(maxant), ant2(maxant)
      logical blmask((maxant*(maxant-1))/2)
c
c     Make a logical mask of baselines from two antenna lists.
c     The mask is .true. if that baseline can be formed from the
c     two antenna lists, oltherwise .false.  The index of the
c     mask is computed from:
c
c                   index = SUM_i=1,A1-1 (maxant-i) + (A2-A1)
c
c     which reduces to
c
c                   index = maxant*(A1-1) - (A1*(A1-1)/2) + (A2-A1)
c
c     where MAXANT is the maximum number of antennas possible,
c     A1 is antenna 1, A2 is antenna 2, and A1 < A2   
c
c  Input:
c    maxant   Maximum number of allowed antennas
c    nant1    Number of antennas in list ANT1
c    nant2    Number of antennas in list ANT2
c    ant1     First list of antennas. 
c    ant2     Second list of antennas. 
c  Output:
c    nblmask  Number of baselines 
c    blmask   Mask for baselines.  If nant1=0=nant2, all locations
c             are .true. 
c---------------------------------------------------------------------
c
      integer i, j, a1, a2, idx, nmax
      logical init
c---------------------------------------------------------------------
c
c Initialize list
c
      if (nant1.eq.0 .and. nant2.eq.0) then 
        init = .true.
      else
        init = .false.
      end if
      nmax = (maxant * (maxant-1)) / 2
      do i = 1, nmax
        blmask(i) = init
      end do
c
c Turn on mask when baseline present
c
      if (nant1.ne.0 .and. nant2.ne.0) then
        do j = 1, nant2
          do i = 1, nant1
            if (ant1(i).ne.ant2(j)) then
              a1 = min(ant1(i), ant2(j))
              a2 = max(ant1(i), ant2(j))
              call antind (maxant, a1, a2, idx)
              blmask(idx) = .true.
            end if
          end do
        end do
      else if (nant1.ne.0 .and. nant2.eq.0) then
        do j = 1, nant1
          do i = 1, maxant
            if (ant1(j).ne.i) then
              a1 = min(ant1(j), i)
              a2 = max(ant1(j), i)
              call antind (maxant, a1, a2, idx)
              blmask(idx) = .true.
            end if
          end do
        end do
      else if (nant1.eq.0 .and. nant2.ne.0) then
        call bug ('f', 
     *   'First antenna list zero, second non-zero is not allowed')
      end if
c
      end
c
c*antind - get index of an antenna pair
c:baselines, antenna
c+
      subroutine antind (maxant, a1, a2, idx)
c
      implicit none
      integer maxant, a1, a2, idx
c
c     Compute the pointer to the logical baseline mask for a pair
c     of antennas.
c
c   Input:
c     maxant Maximum number of antennas allowed
c     a1     First antenna
c     a2     Second antenna
c   Output:
c     idx  Pointer
c-----------------------------------------------------------------------
      if (a1.lt.a2) then
        idx = maxant*(a1-1) - ((a1*(a1-1))/2) + (a2-a1)
      elseif (a1.gt.a2) then
c        call bug ('f', 'Ant1 number must be less than that of Ant2')
        idx = maxant*(a2-1) - ((a2*(a2-1))/2) + (a1-a2)
      else
         call bug('f','ANTIND: Ant1 must be different from Ant2')
      end if
c
      end

