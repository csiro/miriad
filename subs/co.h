      include 'maxnax.h'

c     Coordinate types.
      integer LATTYP, LNGTYP, VELTYP, FELTYP, FRQTYP, LINEAR
      parameter (LATTYP=1, LNGTYP=2, VELTYP=3, FELTYP=4, FRQTYP=5,
     *           LINEAR=6)

      integer MAXCRD
      parameter (MAXCRD = 15)

      logical   frqscl(MAXCRD)
      integer   cotype(MAXNAX,MAXCRD), frqax(MAXCRD), latax(MAXCRD),
     *          lngax(MAXCRD), lus(MAXCRD), nalloc(MAXCRD),
     *          naxis(MAXCRD)
      double precision cdelt(MAXNAX,MAXCRD), crpix(MAXNAX,MAXCRD),
     *          crval(MAXNAX,MAXCRD), epoch(MAXCRD), cosrot(MAXCRD),
     *          sinrot(MAXCRD), obstime(MAXCRD), restfrq(MAXCRD),
     *          vobs(MAXCRD)
      character coproj(MAXCRD)*3, ctype(MAXNAX,MAXCRD)*16

      common /cocom/  crpix, cdelt, crval, cosrot, sinrot, restfrq,
     *                vobs, epoch, obstime, lus, nalloc, naxis,
     *                lngax, latax, frqax, cotype, frqscl
      common /cocomc/ ctype, coproj
