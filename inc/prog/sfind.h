      integer LEVEL, GAUSSIAN, DISK, POINT, BEAM
      parameter (LEVEL=1, GAUSSIAN=2, DISK=3, POINT=4, BEAM=5)

      integer MAXDATA
      parameter (MAXDATA=100000)

      integer    MAXPIX
      parameter (MAXPIX = 256)

      logical vflux, vfwhm1, vfwhm2, vl0, vm0, vpa
      integer gdim, ndata
      real    data(MAXDATA), data2(MAXPIX*MAXPIX), flux, fwhm1, fwhm2,
     :        l0, m0, pa, sflux, sfwhm1, sfwhm2, sl0, sm0, spa,
     :        x(MAXDATA), xd(MAXPIX*MAXPIX), xoff, y(MAXDATA), yoff

      common /JmFit/ vflux, vl0, vm0, vfwhm1, vfwhm2, vpa, ndata, x, y,
     :               l0, m0, fwhm1, fwhm2, pa, flux, data, sflux, sl0,
     :               sm0, sfwhm1, sfwhm2, spa, xoff, yoff, gdim,
     :               data2, xd
