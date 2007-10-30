c+
c Include        constants
c Function       Mathematical and physical constants for global use by
C                Compact Array software
c 
c Author         David McConnell et al.
c Date           23-Mar-1994
c 
c-
      double precision pi, twopi,
     -                 epoch_J2000, ! standard epoch
     -                 mjd_SUN_origin,  ! 1950, or thereabouts
     -                 sidsol,      ! Ratio of sidereal to solar time
     -                 cvel,        ! speed of light (m/s)
     -                 angvel,      ! Earth's angular velocity, rad/sec
     -                 radarcs,     ! 1 radian in arcseconds
     -                 arcsrad,     ! 1 arcsecond in radians 
     -                 raddeg,      ! 1 radian in degrees
     -                 degrad,      ! 1 degree in radians
     -                 aultsc,      ! AU in light seconds (AA, 1989)
     -                 aum,         ! AU in metres
     -                 rschw        ! Schwartzchild radius of sun,
c                                   !    GM/c**2 in light seconds

c     -                c,           ! vel. light, (m/s).  DEPRECATED
c     -           c      = 299792458.0d0,


      parameter (PI     = 3.1415 92653 58979 3238 d0,
     -           TWOPI  = 6.2831 85307 17958 6477 d0,
     -     epoch_J2000  = 2451545.0d0,
     -  mjd_SUN_origin  = 40587.0D0,
     -           SIDSOL = 1.00273 79093 50795 d0,
     -           CVEL   = 299792458.0d0,
     -           ANGVEL = 7.29211 51467 D-5,
     -           RADARCS= 1296000.0d0/TWOPI,
     -           ARCSRAD= TWOPI/1296000.0d0,
     -           RADDEG = 180.0d0/PI,
     -           DEGRAD = PI/180.0d0,
     -           AULTSC = 499.0047837d0,
     -           AUM    = AULTSC*cvel,
     -           RSCHW  = 4.92550D-06)

*
*  general constants used by MULTI
*



	real*8 halfpi
	parameter (halfpi = pi/2.0D0)


	real*8 R2D				! radians to degrees
	parameter (R2D = 180.0d0/pi)

	real*8 R2msec				! radians to millisec
	parameter (R2msec = 8.64D7 / twopi)


	real*8 R2asec				! radians to asec
	parameter (R2asec = R2d * 3600.0D0)
