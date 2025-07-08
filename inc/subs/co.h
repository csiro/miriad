c-----------------------------------------------------------------------
c  co.h
c-----------------------------------------------------------------------
c  Include file for the coordinate conversion routines.  Intended for
c  the exclusive use of co.for.
c
c  $Id: co.h,v 1.9 2012/03/05 03:36:22 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'wcslib/cel.inc'
      include 'wcslib/spc.inc'

c     Coordinate types.
      integer   LINEAR, LNGTYP, LATTYP, SPTYPE
      parameter (LINEAR=1, LNGTYP=2, LATTYP=3, SPTYPE=4)

      integer MAXCRD
      parameter (MAXCRD = 16)

c     Currently, WCSLIB is only used for celestial and spectral
c     coordinates with entry separately at the celprm and spcprm levels.
c     Thus, CEL stores lng0, lat0, phiP, thetaP, phi0, and theta0, as
c     well as the projection parameters and there is no need to
c     duplicate them in separate variables, except for the two crval
c     values that correspond to lng0 and lat0.  For bookkeeping
c     purposes, logical DEFS, records whether phiP, thetaP, phi0, or
c     theta0 (respectively) were set (.true.) or defaulted.

      logical   defs(4,MAXCRD), frqscl(MAXCRD)
      integer   cel(CELLEN,MAXCRD), cotype(MAXNAX,MAXCRD),
     *          latax(MAXCRD), lngax(MAXCRD), lus(MAXCRD),
     *          nalloc(MAXCRD), naxis(MAXCRD), spc(SPCLEN,MAXCRD),
     *          spcax(MAXCRD)
      double precision cdelt(MAXNAX,MAXCRD), cosrot(MAXCRD),
     *          crpix(MAXNAX,MAXCRD), crval(MAXNAX,MAXCRD),
     *          eqnox(MAXCRD), obstime(MAXCRD), sinrot(MAXCRD),
     *          spcvt(MAXCRD), vobs(MAXCRD)
      character ctype(MAXNAX,MAXCRD)*16, specsys(MAXCRD)*8

c     N.B. though declared as an integer array, cel and spc must be
c     aligned on double precision boundaries.  Especially important on
c     Suns.
      common /cocom/  crpix, cdelt, crval, cosrot, sinrot, eqnox,
     *                obstime, spcvt, vobs, cel, spc, lus, nalloc,
     *                naxis, lngax, latax, spcax, cotype, defs, frqscl
      common /cocomc/ ctype, specsys
