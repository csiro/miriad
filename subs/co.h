c-----------------------------------------------------------------------
c  co.h
c-----------------------------------------------------------------------
c  Include file for the coordinate conversion routines.  Intended for
c  the exclusive use of co.for.
c
c  $Id: co.h,v 1.5 2010/11/22 04:19:47 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'wcslib/cel.inc'

c     Coordinate types.
      integer   LINEAR, LNGTYP, LATTYP, FRQTYP, VELTYP, FELTYP
      parameter (LINEAR=1, LNGTYP=2, LATTYP=3, FRQTYP=4, VELTYP=5,
     *           FELTYP=6)

      integer MAXCRD
      parameter (MAXCRD = 16)

c     WCSLIB, currently, is only used for celestial coordinates with
c     entry at the celprm level.  Thus, CEL stores phi0, theta0, lng0,
c     lat0, phiP, and thetaP as well as the projection parameters and
c     there is no need to duplicate them in separate variables, except
c     for the two crval values that correspond to lng0 and lat0.  Once
c     WCSLIB is used for all coordinates, celprm and most of the
c     remaining variables will be replaced by wcsprm.

      logical   frqscl(MAXCRD)
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
     *                lngax, latax, frqax, cotype, frqscl
      common /cocomc/ ctype
