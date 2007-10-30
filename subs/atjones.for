c************************************************************************
	subroutine atJones(rad,psi,freq,Jo,pb)
c
	implicit none
	real rad,psi,pb
	double precision freq
	complex Jo(2,2)
c
c  Compute the Jones matrix of the ATCA antennas for a particular position
c  in the primary beam.
c
c  Inputs:
c    rad  The distance of the point from the field centre, in radians.
c    psi  Position angle of the point, in radians.
c    freq Observing frequency, in GHz.
c
c  Output:
c    Jo   The 2x2 complex-valued Jones matrix.
c    pb   The mean total intensity primary beam response at this point.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
c  Alpha2 = 4*log(2).
c
	real alpha2
	parameter(alpha2=2.772589)
c
	integer i
	real rdist,x(7),px,py
	real coeffs(2,7),coeffl(2,5),coeffx(2,7),coeffc(2,5)
	save coeffs,coeffl,coeffx,coeffc
	data coeffs/
     *  1.3992E+00,   0.0000E+00,
     *  6.6962E-02,   9.2800E-01,
     * -8.1047E-02,   4.6582E-02,
     *  5.5058E-02,  -4.5728E-02,
     *  4.2927E-02,  -1.5807E-02,
     *  5.2665E-02,  -3.8708E-02,
     * -1.8535E-02,   1.3006E-02/
c
	data coeffl/
     *  1.3523E+00,   0.0000E+00,
     * -8.9658E-02,   4.1000E+00,
     * -1.2942E-02,   6.4604E-03,
     *  1.5156E-02,  -1.0285E-02,
     * -1.5113E-03,   5.0859E-03/
c
	data coeffc/
     *  1.3804E+00,   0.0000E+00,
     * -6.0461E-03,   8.2200E-01,
     * -3.9537E-02,   2.5856E-02,
     *  3.9076E-02,  -2.5159E-02,
     * -2.6902E-03,  -4.2609E-03/
c
	data coeffx/
     *  1.4175E+00,   0.0000E+00,
     *  3.0893E-02,   1.1840E+00,
     * -1.0202E-01,   6.1286E-02,
     *  7.9883E-02,  -4.8667E-02,
     *  3.5436E-03,   3.1695E-02,
     *  2.9788E-02,  -1.7744E-02,
     * -3.3598E-02,   1.7741E-02/
c
c  Compute the coefficients of the trig functions in the Jones
c  matrix. Also compute the primary beam response.
c
c  13-cm response.
c
	if(freq.gt.2.and.freq.lt.3)then
	  rdist =  rad / ( 2.368/freq * 20.9882*PI/180/60 )
	  x(1) = exp(-alpha2*(rdist/coeffs(1,1))**2)
	  x(2) = coeffs(1,2)*sin(0.5*PI*rdist/coeffs(2,2))**2
	  pb = x(1)*x(1) + 0.5*x(2)*x(2)
	  do i=3,7
	    x(i) = (coeffs(2,i)*rdist + coeffs(1,i))*rdist
	    pb = pb + 0.5*x(i)*x(i)
	  enddo
c
	  px = psi
	  py = -psi - 0.5*PI
  	  Jo(1,1) = x(1) + x(2)*cos(2*px) + x(3)*cos(px) + x(4)*sin(px)
	  Jo(2,2) = x(1) + x(2)*cos(2*py) + x(3)*cos(py) + x(4)*sin(py)
	  py = -py
	  Jo(1,2) =   cmplx(x(5),x(6))*sin(px) + cmplx(x(7),0.)*cos(px)
	  Jo(2,1) = -(cmplx(x(5),x(6))*sin(py) + cmplx(x(7),0.)*cos(py))
c
c  20-cm response.
c
	else if(freq.gt.1.and.freq.lt.2)then
	  rdist =  rad / ( 1.384/freq * 34.61*PI/180/60 )
	  x(1) = exp(-alpha2*(rdist/coeffl(1,1))**2)
	  x(2) = coeffl(1,2)*sin(0.5*PI*rdist/coeffl(2,2))**2
	  pb = x(1)*x(1) + 0.5*x(2)*x(2)
	  do i=3,5
	    x(i) = (coeffl(2,i)*rdist + coeffl(1,i))*rdist
	    pb = pb + 0.5*x(i)*x(i)
	  enddo
c
	  px = psi
	  py = -psi - 0.5*PI
  	  Jo(1,1) = x(1) + x(2)*cos(2*px) + x(3)*cos(px) + x(4)*sin(px)
	  Jo(2,2) = x(1) + x(2)*cos(2*py) + x(3)*cos(py) + x(4)*sin(py)
	  py = -py
	  Jo(1,2) =  x(5)*sin(2*px)
	  Jo(2,1) = -x(5)*sin(2*py)
c
c  6-cm response.
c
	else if(freq.gt.4.and.freq.lt.6)then
	  rdist =  rad / ( 4.800/freq * 10.06250*PI/180/60 )
	  x(1) = exp(-alpha2*(rdist/coeffx(1,1))**2)
	  x(2) = coeffs(1,2)*sin(0.5*PI*rdist/coeffx(2,2))**2
	  pb = x(1)*x(1) + 0.5*x(2)*x(2)
	  do i=3,5
	    x(i) = (coeffc(2,i)*rdist + coeffc(1,i))*rdist
	    pb = pb + 0.5*x(i)*x(i)
	  enddo
c
	  px = psi
	  py = -psi - 0.5*PI
  	  Jo(1,1) = x(1) + x(2)*cos(2*px) + x(3)*cos(px) + x(4)*sin(px)
	  Jo(2,2) = x(1) + x(2)*cos(2*py) + x(3)*cos(py) + x(4)*sin(py)
	  py = -py
	  Jo(1,2) =  x(5)*sin(2*px)
	  Jo(2,1) = -x(5)*sin(2*py)
c
c  3-cm response.
c
	else if(freq.gt.8.and.freq.lt.9)then
	  rdist =  rad / ( 8.640/freq * 5.86*PI/180/60 )
	  x(1) = exp(-alpha2*(rdist/coeffx(1,1))**2)
	  x(2) = coeffs(1,2)*sin(0.5*PI*rdist/coeffx(2,2))**2
	  pb = x(1)*x(1) + 0.5*x(2)*x(2)
	  do i=3,7
	    x(i) = (coeffx(2,i)*rdist + coeffx(1,i))*rdist
	    pb = pb + 0.5*x(i)*x(i)
	  enddo
c
	  px = psi
	  py = -psi - 0.5*PI
  	  Jo(1,1) = x(1) + x(2)*cos(2*px) + x(3)*cos(px) + x(4)*sin(px)
	  Jo(2,2) = x(1) + x(2)*cos(2*py) + x(3)*cos(py) + x(4)*sin(py)
	  py = -py
          Jo(1,2) = cmplx(x(5),0.)*sin(2*px) + cmplx(x(6),x(7))*sin(px)
          Jo(2,1) = cmplx(x(5),0.)*sin(2*py) + cmplx(x(6),x(7))*sin(py)
	else
	  call bug('f','Polarimetric response not known at this freq')	  
	endif	
c
	end
