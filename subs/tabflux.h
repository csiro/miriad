      integer NTABLE
      parameter (NTABLE=2000)
c
      character TSOURCE(NTABLE)*40
      integer NTAB, TINDEX(NTABLE)
      real TFREQ(NTABLE), TFLUX(NTABLE), TRMS(NTABLE)
      double precision TDATE(NTABLE)
c
      common / TCOMI /NTAB, TINDEX
      common / TCOMR /TFREQ, TFLUX, TRMS
      common / TCOMD /TDATE
      common / TCOMS /TSOURCE
