c***********************************************************************
c* Nextpow2 -- Find the next power of two greater than or equal to n.
c& mchw
c: numbers,mathematics
c+
      integer function nextpow2 (n)
c
      integer n
c
c  Input:
c    n		Number of interest.
c
c  Output:
c    nextpow2	Next power of 2 greater than or equal to n.
c--
c  History:
c    nebk Dark-ages Adapted from Bob Sault's codes.
c    rjs   1mar90   Changed it to be a function, and return power of 2
c		    greater of equal to k.
c    rjs   8mar90   Deleted a redundant variable.
c
c $Id: nextpow2.for,v 1.2 2009/10/29 06:01:40 cal103 Exp $
c-----------------------------------------------------------------------

      nextpow2 = 1
      dowhile (nextpow2.lt.n)
         nextpow2 = nextpow2 + nextpow2
      enddo

      end
