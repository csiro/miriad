c
c Include file for pb.for
c
      include 'mirconst.h'
      integer maxco
      double precision rtmsq, atr
      parameter (maxco = 5, atr = dpi / (180.0 * 3600.0),
     +           rtmsq = 180.0 * 180.0 * 3600.0 / dpi / dpi)
c
      double precision pbcoeff(maxco), pbfreq
      real pbcut, pbfac
      character pbtype*10, pbdone*4
c
      common /pbdat1/ pbcoeff, pbfreq
      common /pbdat2/ pbfac, pbcut
      common /pbdat3/ pbtype, pbdone
