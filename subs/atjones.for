      subroutine atJones(rad,psi,freq,Jo,pb)

      real Jo(2,2), rad, psi, pb
      double precision freq

c  Compute the Jones matrix of the ATCA antennas for a particular
c  position in the primary beam.  Also compute the primary beam
c  response.
c
c  Inputs:
c    rad  The distance of the point from the field centre, in radians.
c    psi  Position angle of the point, in radians.
c    freq Observing frequency, in GHz.
c
c  Output:
c    Jo   The 2x2 Jones matrix which is real-valued for the ATCA.
c    pb   The mean total intensity primary beam response at this point.
c
c $Id: atjones.for,v 1.2 2010/08/03 08:20:55 cal103 Exp $
c-----------------------------------------------------------------------
      include 'mirconst.h'

c     alpha2 = 4*log(2).
      real alpha2
      parameter (alpha2 = 2.772589)

      integer i
      real    aC(2,5), aL(2,5), aS(2,7), aX(2,7), px, py, rdist, x(7)

      data aL/ 1.3523E+00,   0.0000E+00,
     *        -8.9658E-02,   4.1000E+00,
     *        -1.2942E-02,   6.4604E-03,
     *         1.5156E-02,  -1.0285E-02,
     *        -1.5113E-03,   5.0859E-03/

      data aS/ 1.3992E+00,   0.0000E+00,
     *         6.6962E-02,   9.2800E-01,
     *        -8.1047E-02,   4.6582E-02,
     *         5.5058E-02,  -4.5728E-02,
     *         4.2927E-02,  -1.5807E-02,
     *         5.2665E-02,  -3.8708E-02,
     *        -1.8535E-02,   1.3006E-02/

      data aC/ 1.3804E+00,   0.0000E+00,
     *        -6.0461E-03,   8.2200E-01,
     *        -3.9537E-02,   2.5856E-02,
     *         3.9076E-02,  -2.5159E-02,
     *        -2.6902E-03,  -4.2609E-03/

      data aX/ 1.4175E+00,   0.0000E+00,
     *         3.0893E-02,   1.1840E+00,
     *        -1.0202E-01,   6.1286E-02,
     *         7.9883E-02,  -4.8667E-02,
     *         3.5436E-03,   3.1695E-02,
     *         2.9788E-02,  -1.7744E-02,
     *        -3.3598E-02,   1.7741E-02/
c-----------------------------------------------------------------------
      if (freq.gt.1d0 .and. freq.lt.2d0) then
c       L-band (20cm) response.
        rdist =  rad / (1.384/freq * (34.61/60.0)*D2R)
        x(1) = exp(-alpha2*(rdist/aL(1,1))**2)
        x(2) = aL(1,2)*sin(PI_2*rdist/aL(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i = 3, 5
          x(i) = (aL(2,i)*rdist + aL(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) =  x(5)*sin(2.0*px)
        Jo(2,1) = -x(5)*sin(2.0*py)

      else if (freq.gt.2d0 .and. freq.lt.3d0) then
c       S-band (13cm) response.
        rdist =  rad / (2.368/freq * (20.9882/60.0)*D2R)
        x(1) = exp(-alpha2*(rdist/aS(1,1))**2)
        x(2) = aS(1,2)*sin(PI_2*rdist/aS(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i=3,7
          x(i) = (aS(2,i)*rdist + aS(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) =   cmplx(x(5),x(6))*sin(px) + cmplx(x(7),0.0)*cos(px)
        Jo(2,1) = -(cmplx(x(5),x(6))*sin(py) + cmplx(x(7),0.0)*cos(py))

      else if (freq.gt.4d0 .and. freq.lt.6d0) then
c       C-band (6cm) response.
        rdist =  rad / (4.800/freq * (10.06250/60.0)*D2R)
        x(1) = exp(-alpha2*(rdist/aC(1,1))**2)
        x(2) = aC(1,2)*sin(PI_2*rdist/aC(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i = 3, 5
          x(i) = (aC(2,i)*rdist + aC(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) =  x(5)*sin(2.0*px)
        Jo(2,1) = -x(5)*sin(2.0*py)

      else if (freq.gt.8d0 .and. freq.lt.9d0) then
c       X-band (3cm) response.
        rdist =  rad / (8.640/freq * (5.86/60.0)*D2R)
        x(1) = exp(-alpha2*(rdist/aX(1,1))**2)
        x(2) = aX(1,2)*sin(PI_2*rdist/aX(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i = 3, 7
          x(i) = (aX(2,i)*rdist + aX(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) = cmplx(x(5),0.0)*sin(2.0*px) + cmplx(x(6),x(7))*sin(px)
        Jo(2,1) = cmplx(x(5),0.0)*sin(2.0*py) + cmplx(x(6),x(7))*sin(py)

      else
        call bug('f','Polarimetric response not known at this freq')
      endif

      end
