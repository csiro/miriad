c-----------------------------------------------------------------------
c  co.h
c-----------------------------------------------------------------------
c  Include file for the coordinate conversion routines.  Intended for
c  the exclusive use of co.for.
c
c  $Id: co.h,v 1.4 2010/10/29 01:34:22 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'wcslib/cel.inc'

c     Coordinate types.
      integer   LINEAR, LNGTYP, LATTYP, FRQTYP, VELTYP, FELTYP
      parameter (LINEAR=1, LNGTYP=2, LATTYP=3, FRQTYP=4, VELTYP=5,
     *           FELTYP=6)

      integer MAXCRD
      parameter (MAXCRD = 16)

      logical   frqscl(MAXCRD)
      integer   cel(CELLEN,MAXCRD), cotype(MAXNAX,MAXCRD),
     *          frqax(MAXCRD), latax(MAXCRD), lngax(MAXCRD),
     *          lus(MAXCRD), nalloc(MAXCRD), naxis(MAXCRD)
      double precision cdelt(MAXNAX,MAXCRD), crpix(MAXNAX,MAXCRD),
     *          crval(MAXNAX,MAXCRD), eqnox(MAXCRD), cosrot(MAXCRD),
     *          sinrot(MAXCRD), obstime(MAXCRD), restfrq(MAXCRD),
     *          vobs(MAXCRD)
      character ctype(MAXNAX,MAXCRD)*16

c     N.B. though declared as an integer array, cel must be aligned on
c     a double precision boundary.  Especially important on Suns.
      common /cocom/  crpix, cdelt, crval, cosrot, sinrot, restfrq,
     *                vobs, eqnox, obstime, cel, lus, nalloc, naxis,
     *                lngax, latax, frqax, cotype, frqscl
      common /cocomc/ ctype
