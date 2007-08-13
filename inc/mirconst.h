c=======================================================================
c - mirconst.h  Include file for various fundamental physical constants.
c
c  History:
c    jm  18dec90  Original code.  Constants taken from the paper
c                 "The Fundamental Physical Constants" by E. Richard
c                 Cohen and Barry N. Taylor (PHYICS TODAY, August 1989).
c
c $Id: mirconst.h,v 1.2 2007/08/13 09:07:47 cal103 Exp $
c ----------------------------------------------------------------------
c  Pi.
      double precision DPI, DPI_2, DPI_4, DTWOPI
      parameter (DPI = 3.14159265358979323846D0)
      parameter (DTWOPI = DPI * 2D0)
      parameter (DPI_2  = DPI / 2D0)
      parameter (DPI_4  = DPI / 4D0)

      real PI, PI_2, PI_4, TWOPI
      parameter (PI    = DPI)
      parameter (TWOPI = DTWOPI)
      parameter (PI_2  = DPI_2)
      parameter (PI_4  = DPI_4)
c ----------------------------------------------------------------------
c  Angular conversion factors.
      double precision DAS2R, DD2R, DR2AS, DR2D
      parameter (DD2R  =  DPI / 180D0)
      parameter (DR2D  =  1D0 / DD2R)
      parameter (DAS2R = DD2R / 3600D0)
      parameter (DR2AS =  1D0 / DAS2R)

      real AS2R, D2R, R2AS, R2D
      parameter (D2R   = DD2R)
      parameter (R2D   = DR2D)
      parameter (AS2R  = DAS2R)
      parameter (R2AS  = DR2AS)
c ----------------------------------------------------------------------
c  Speed of light (meter/second).
      double precision DCMKS
      parameter (DCMKS = 299792458D0)

      real CMKS
      parameter (CMKS = DCMKS)
c ----------------------------------------------------------------------
c  Boltzmann constant (Joule/Kelvin).
      double precision DKMKS
      parameter (DKMKS = 1.380658D-23)

      real KMKS
      parameter (KMKS = DKMKS)
c ----------------------------------------------------------------------
c  Planck constant (Joule-second).
      double precision DHMKS
      parameter (DHMKS = 6.6260755D-34)

      real HMKS
      parameter (HMKS = DHMKS)
c ----------------------------------------------------------------------
c  Planck constant divided by Boltzmann constant (Kelvin/GHz).
      double precision DHOVERK
      parameter (DHOVERK = 1D9 * DHMKS / DKMKS)

      real HOVERK
      parameter (HOVERK = DHOVERK)
c=======================================================================
