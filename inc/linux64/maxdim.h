c=======================================================================
c $Id: maxdim.h,v 1.1 2007/03/23 05:30:35 cal103 Exp $
c-----------------------------------------------------------------------
c     Size of an INTEGER array used to implement a memory heap.  Must be
c     less than the maximum 4-byte Fortran INTEGER value of 2147483647
c     which causes expressions of the form (MAXBUF+1)/2 to overflow.
      INTEGER   MAXBUF
      PARAMETER(MAXBUF=2147483646)

c     Maximum image dimension.
      INTEGER   MAXDIM
      PARAMETER(MAXDIM=65536)

c     Maximum number of antennae (HC=3/6/9/..., WSRT=14, VLA=27).
      INTEGER   MAXANT
      PARAMETER(MAXANT=30)

c     Maximum number of baselines.
      INTEGER   MAXBASE
      PARAMETER(MAXBASE=((MAXANT*(MAXANT+1))/2))

c     Maximum number of channels in spectral data.
      INTEGER   MAXCHAN
      PARAMETER(MAXCHAN=4097)

c     Maximum number of windows in visibility data.
      INTEGER   MAXWIN
      PARAMETER(MAXWIN=16)

c     Maximum number of wideband channels.
      INTEGER   MAXWIDE
      PARAMETER(MAXWIDE=18)
c=======================================================================
