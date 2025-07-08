      integer   MAXCOUNT
      parameter (MAXCOUNT=101)

      logical   doinit, mfs, mosaic, rChange, rConst, sChange, vChange,
     *          vLinear, xChange, yChange
      integer   count, mcount, naver, nchan
      real      epoch, lstart, lstep, lwidth, sumlumv, sumuuvv, vobs
      double precision cdelt1, cdelt2, cdelt3, crval1, crval2, crval3,
     *          obsdec, obsra, obstime, restfreq
      character ctype1*12, ctype2*12, ctype3*12, ltype*12, observer*16,
     *          pbtype*16, source*16, telescop*12

      common/hdtab1/ cdelt1, cdelt2, cdelt3, crval1, crval2, crval3,
     *          obsra, obsdec, obstime, restfreq,
     *          epoch, lstart, lstep, lwidth, sumlumv, sumuuvv, vobs,
     *          count, mcount, naver, nchan,
     *          doinit, mfs, mosaic, rChange, rConst, sChange, vChange,
     *          vLinear, xChange, yChange

      common/hdtab2/ ctype1, ctype2, ctype3, ltype, observer, pbtype,
     *          source, telescop
