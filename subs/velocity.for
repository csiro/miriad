c************************************************************************
c
c  A collection of routines to determine the velocity of a point on
c  the earth, with respect to various rest frames.
c
c  These have been stolen and adapted from the SLALIB Starlink routines
c  by P.T.Wallace (circa 1986).
c
c  History:
c    13jul93 rjs  Original Miriad version.
c     2nov01 rjs  Changed definition of LSR velocity by 0.3 km/s
c************************************************************************
      SUBROUTINE VEARTH (JDAY, POS, VEL)
c
      IMPLICIT NONE
      DOUBLE PRECISION JDAY,POS(3),VEL(3)
*
*  Approximate heliocentric position and velocity of the Earth
*  The date and time should really be in the TDB in the Gregorian
*  calendar, and is interpreted in a manner which is valid between
*  1900 March 1 and 2100 February 28.
*
*  Input:
*    jday	Time of interest (as Julian day).
*  Output:
*    pos	Position, in km.
*    vel	Velocity, in km/sec.
*
*  The Earth heliocentric position/velocity is for mean equator and equinox
*  of date.
*
*  Max/RMS errors 1950-2050:
*     13/5 E-5 AU = 19200/7600 km in position
*     47/26 E-10 AU/s = 0.0070/0.0039 km/s in speed
*------------------------------------------------------------------------
      REAL TWOPI,SPEED,REMB,SEMB
      DOUBLE PRECISION AUKM,J1901
      REAL YF,T,ELM,GAMMA,EM,ELT,EPS0,DAY,
     :     E,ESQ,V,R,ELMM,COSELT,SINEPS,COSEPS,W1,W2,SELMM,CELMM
      INTEGER IY,QUAD

      PARAMETER (TWOPI=6.28318530718)

*  Mean orbital speed of Earth, AU/s
      PARAMETER (SPEED=1.9913E-7)

*  Mean Earth:EMB distance and speed, AU and AU/s
      PARAMETER (REMB=3.12E-5,SEMB=8.31E-11)

* AU to km.
      PARAMETER(AUKM=149.597870D6)

* Julian date for 1 January, 1901.
      PARAMETER(J1901=2415385.5d0)

*  Whole years & fraction of year, and years since 1900.
      QUAD = INT(JDAY - J1901) / 1461
      IY   = INT(JDAY - J1901 - 1461*QUAD) / 365
      DAY   = JDAY - J1901 - 1461*QUAD - 365*IY + 1

      IY = 4*QUAD + IY + 1
      YF = (4*DAY - 4*(1/(MOD(IY,4)+1)) - MOD(IY,4) - 2) / 1461.0
      T = IY + YF

*  Geometric mean longitude of Sun
*  (cf 4.881627938+6.283319509911*T MOD 2PI)
      ELM=MOD(4.881628+TWOPI*YF+0.00013420*T,TWOPI)

*  Mean longitude of perihelion
      GAMMA=4.908230+3.0005E-4*T

*  Mean anomaly
      EM=ELM-GAMMA

*  Mean obliquity
      EPS0=0.40931975-2.27E-6*T

*  Eccentricity
      E=0.016751-4.2E-7*T
      ESQ=E*E

*  True anomaly
      V=EM+2.0*E*SIN(EM)+1.25*ESQ*SIN(2.0*EM)

*  True ecliptic longitude
      ELT=V+GAMMA

*  True distance
      R=(1.0-ESQ)/(1.0+E*COS(V))

*  Moon's mean longitude
      ELMM=MOD(4.72+83.9971*T,TWOPI)

*  Useful functions
      COSELT=COS(ELT)
      SINEPS=SIN(EPS0)
      COSEPS=COS(EPS0)
      W1=-R*SIN(ELT)
      W2=-SPEED*(COSELT+E*COS(GAMMA))
      SELMM=SIN(ELMM)
      CELMM=COS(ELMM)

*  Earth position and velocity
      POS(1) = AUKM * (-R*COSELT-REMB*CELMM)
      POS(2) = AUKM * (W1-REMB*SELMM)*COSEPS
      POS(3) = AUKM * W1*SINEPS
      VEL(1) = AUKM * (SPEED*(SIN(ELT)+E*SIN(GAMMA))+SEMB*SELMM)
      VEL(2) = AUKM * (W2-SEMB*CELMM)*COSEPS
      VEL(3) = AUKM * W2*SINEPS

      END
************************************************************************
      SUBROUTINE VSITE(PHI, ST, VEL)
      IMPLICIT NONE
      DOUBLE PRECISION PHI, ST, VEL(3)
*+
*
*  Velocity due to Earth rotation
*
*  Input:
*     PHI	latitude of observing station (geodetic)
*     ST 	local apparent sidereal time
*  Output:
*     VEL	velocity in km/s.
*
*  PHI and ST are all in radians.
*  Accuracy:
*     The simple algorithm used assumes a spherical Earth and
*     an observing station at sea level.  For actual observing
*     sites, the error is unlikely to be greater than 0.0005 km/s.
*-----------------------------------------------------------------------

*  Sidereal speed of Earth equator, adjusted to compensate for
*  the simple algorithm used.  (The true value is 0.4651.)
      REAL ESPEED
      PARAMETER (ESPEED=0.4655)

      VEL(1) = -ESPEED*COS(PHI)*SIN(ST)
      VEL(2) =  ESPEED*COS(PHI)*COS(ST)
      VEL(3) = 0d0
      END
************************************************************************
      SUBROUTINE VSUN(VEL)
      DOUBLE PRECISION VEL(3)
*
*  Velocity of the Sun with respect to the Local Standard of Rest
*   
*  Output:
*     VEL	Velocity of the Sun.
*------------------------------------------------------------------------

*  Speed = 20 km/s
*
*  Apex = RA 270 deg, Dec +30deg, 1900.0
*  = 18 07 50.3, +30 00 52, J2000.0
*
*  This is expressed in the form of a J2000.0 x,y,z vector:
*
*      VA(1) = X = -SPEED*COS(RA)*COS(DEC)
*      VA(2) = Y = -SPEED*SIN(RA)*COS(DEC)
*      VA(3) = Z = -SPEED*SIN(DEC)
      REAL VA(3)
      DATA VA / -0.29000, +17.31726, -10.00141 /
      VEL(1) = VA(1)
      VEL(2) = VA(2)
      VEL(3) = VA(3)
      END
