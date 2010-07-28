      include 'maxnax.h'
      include 'wcslib/cel.inc'

c     Coordinate types.
      integer   LINEAR, LNGTYP, LATTYP, FRQTYP, VELTYP, FELTYP
      parameter (LINEAR=1, LNGTYP=2, LATTYP=3, FRQTYP=4, VELTYP=5,
     *           FELTYP=6)

      integer MAXCRD
      parameter (MAXCRD = 15)

      logical   frqscl(MAXCRD)
      integer   cel(CELLEN,MAXCRD), cotype(MAXNAX,MAXCRD),
     *          frqax(MAXCRD), latax(MAXCRD), lngax(MAXCRD),
     *          lus(MAXCRD), nalloc(MAXCRD), naxis(MAXCRD)
      double precision cdelt(MAXNAX,MAXCRD), crpix(MAXNAX,MAXCRD),
     *          crval(MAXNAX,MAXCRD), eqnox(MAXCRD), cosrot(MAXCRD),
     *          sinrot(MAXCRD), obstime(MAXCRD), restfrq(MAXCRD),
     *          vobs(MAXCRD)
      character ctype(MAXNAX,MAXCRD)*16

      common /cocom/  crpix, cdelt, crval, cosrot, sinrot, restfrq,
     *                vobs, eqnox, obstime, lus, nalloc, naxis,
     *                lngax, latax, frqax, cotype, cel, frqscl
      common /cocomc/ ctype
