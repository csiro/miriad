c-----------------------------------------------------------------------
c  co.h
c-----------------------------------------------------------------------
c  Include file for the coordinate conversion routines.  Intended for
c  the exclusive use of co.for.
c
c  $Id: co.h,v 1.6 2010/12/07 04:58:10 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'wcslib/cel.inc'

c     Coordinate types.
      integer   LINEAR, LNGTYP, LATTYP, FRQTYP, VELTYP, FELTYP
      parameter (LINEAR=1, LNGTYP=2, LATTYP=3, FRQTYP=4, VELTYP=5,
     *           FELTYP=6)

      integer MAXCRD
      parameter (MAXCRD = 16)

c     Currently, WCSLIB is only used for celestial coordinates with
c     entry at the celprm level.  Thus, CEL stores lng0, lat0, phiP,
c     thetaP, phi0, and theta0, as well as the projection parameters and
c     there is no need to duplicate them in separate variables, except
c     for the two crval values that correspond to lng0 and lat0.
c     For bookkeeping purposes, logical DEFS, records whether phiP,
c     thetaP, phi0, or theta0 (respectively) were set (.true.) or
c     defaulted.  Once WCSLIB is used for all coordinates, celprm and
c     most of the remaining variables will be replaced by wcsprm.

      logical   defs(4,MAXCRD), frqscl(MAXCRD)
      integer   cel(CELLEN,MAXCRD), cotype(MAXNAX,MAXCRD),
     *          frqax(MAXCRD), latax(MAXCRD), lngax(MAXCRD),
     *          lus(MAXCRD), nalloc(MAXCRD), naxis(MAXCRD)
      double precision cdelt(MAXNAX,MAXCRD), cosrot(MAXCRD),
     *          crpix(MAXNAX,MAXCRD), crval(MAXNAX,MAXCRD),
     *          eqnox(MAXCRD), obstime(MAXCRD), restfrq(MAXCRD),
     *          sinrot(MAXCRD), vobs(MAXCRD)
      character ctype(MAXNAX,MAXCRD)*16

c     N.B. though declared as an integer array, cel must be aligned on
c     a double precision boundary.  Especially important on Suns.
      common /cocom/  crpix, cdelt, crval, cosrot, sinrot, restfrq,
     *                vobs, eqnox, obstime, cel, lus, nalloc, naxis,
     *                lngax, latax, frqax, cotype, defs, frqscl
      common /cocomc/ ctype
