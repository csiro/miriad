c************************************************************************
c*Nextpow2 -- Find the next power of two.
c:numbers
c+
      integer function nextpow2 (k)
c
      implicit none
      integer k
c
c  Find next integer, which is a power of two, greater than or equal to k.
c
c  Input:
c    k		Number of interest.
c  Output:
c    nextpow2	Next power of 2 higher than or equal to k.
c--
c  History:
c    nebk Dark-ages Adapted from Bob Sault's codes.
c    rjs   1mar90   Changed it to be a function, and return power of 2
c		    greater of equal to k.
c    rjs   8mar90   Deleted a redundant variable.
c------------------------------------------------------------------------
      integer l
c
      l = 1
      dowhile (l.lt.k)
         l = l + l
      enddo
      nextpow2 = l
c
      end
