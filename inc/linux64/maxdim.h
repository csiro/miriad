c=======================================================================
c $Id: maxdim.h,v 1.7 2009/04/28 05:23:10 mirmgr Exp $
c-----------------------------------------------------------------------
c     Size of an INTEGER array used to implement a memory heap.  This
c     array is the sole variable in blank COMMON in Miriad.  Trial-and-
c     error compilations on an x86-64 system with gcc/g77 show that the
c     limit on MAXBUF for which most tasks build successfully is
c     1073741823 (2**30 - 1) which corresponds to 4GiB.  With MAXDIM
c     less than 32768 the limit for which all tasks build successfully
c     is about 260000000; unsuccessful links produce messages about
c     truncated relocations, imom being the worst offender.
c     The default value allocates 128MiB (for normal 4-byte INTEGERs).
      INTEGER   MAXBUF
      PARAMETER(MAXBUF=  33554432)

c     Maximum image axis length.  Array dimensions are typically a few
c     times MAXDIM (never MAXDIM**2) so MAXDIM is associated with a much
c     smaller allocation of static memory than MAXBUF.  Thus the default
c     value of MAXDIM is quite generous.  Values of MAXDIM > 32767 cause
c     segvs in mfclean.  Note that, depending on the algorithm, MAXBUF
c     may also play an important role in determining the maximum image
c     size that can be handled.
      INTEGER   MAXDIM
      PARAMETER(MAXDIM=16384)

c     Maximum number of antennae (HC=3/6/9/..., WSRT=14, VLA=27).
      INTEGER   MAXANT
      PARAMETER(MAXANT=64)

c     Maximum number of baselines.
      INTEGER   MAXBASE
      PARAMETER(MAXBASE=((MAXANT*(MAXANT+1))/2))

c     Maximum number of channels in spectral data.
      INTEGER   MAXCHAN
      PARAMETER(MAXCHAN=70000)

c     Maximum number of windows in visibility data.
      INTEGER   MAXWIN
      PARAMETER(MAXWIN=16)

c     Maximum number of wideband channels.
      INTEGER   MAXWIDE
      PARAMETER(MAXWIDE=18)
c=======================================================================
