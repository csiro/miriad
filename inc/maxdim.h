C=======================================================================
C $Id: maxdim.h,v 1.5 2009/08/31 02:36:08 cal103 Exp $
C-----------------------------------------------------------------------
C     Size of an INTEGER array used to implement a memory heap.  This
C     array is the sole variable in blank COMMON in Miriad.  The default
C     value allocates 4MiB (for normal 4-byte INTEGERs).
      INTEGER   MAXBUF
      PARAMETER(MAXBUF = 1024*1024)

C     Maximum image axis length.  Array dimensions are typically a few
C     times MAXDIM (never MAXDIM**2) so MAXDIM is associated with a much
C     smaller allocation of static memory than MAXBUF.  Thus the default
C     value of MAXDIM is quite generous.  Values of MAXDIM > 32767 cause
C     segvs in mfclean.  Note that, depending on the algorithm, MAXBUF
C     may also play an important role in determining the maximum image
C     size that can be handled.
      INTEGER   MAXDIM
      PARAMETER(MAXDIM = 16*1024)

C     Maximum number of antennas (SCAMP=96).
      INTEGER   MAXANT
      PARAMETER(MAXANT = 96)

C     Maximum number of baselines, including autocorrelations.
      INTEGER   MAXBASE
      PARAMETER(MAXBASE = ((MAXANT*(MAXANT+1))/2))

C     Maximum number of channels in spectral data.
      INTEGER   MAXCHAN
      PARAMETER(MAXCHAN = 32*1024 + 4)

C     Maximum number of windows in visibility data.
      INTEGER   MAXWIN
      PARAMETER(MAXWIN = 16)

C     Maximum number of wideband channels.
      INTEGER   MAXWIDE
      PARAMETER(MAXWIDE = 18)
C=======================================================================
