c* sfetra - transformation between equatorial and other coordinate systems
c: coordinates
c& bpw
c+
      subroutine sfetra( slon, slat, inv, sys )
c      subroutine dsfetra( dlon, dlat, inv, sys )

      real                 slon, slat
      double precision     dlon, dlat
      logical              inv
      integer              sys
c
c Trigonometric coordinate transform by matrix multiplication. Defines
c transformation from equatorial to galactic, ecliptic or supergalactic
c coordinates.
c Sfetra takes a real variable as in/output, for dsfetra coordinates
c must be in double precision.
c
c Input/Output:
c   slon/dlon:   longitude (in radians)
c   slat/dlat:   latitude  (in radians)
c
c Input:
c   inv:         direction of transformation
c                .false. from equatorial to other systems
c                .true.  from other systems to equatorial
c   sys:         in-/output system
c                1: galactic coordinates
c                2: ecliptic coordinates
c                3: supergalactic coordinates
c--
c
c History:
c   bpw   11sep90   created
c   bpw   20jun91   corrected document (dsfetra instead of dsfetr)
c   mjs   20jun91   changed dsfetr call to dsfetra call below
c
c-----------------------------------------------------------------------

c                     single precision calculation

c     convert to double precision
      dlon = dble( slon )
      dlat = dble( slat )
c     do the transformation
      call dsfetra( dlon, dlat, inv, sys )
c     prepare results
      slon = sngl( dlon )
      slat = sngl( dlat )
c     return to calling program
      return
      end

c                     double precision calculation

      subroutine dsfetra( lon, lat, inv, sys )

c                  ***** declarations *****

c     working longitude/latitude
      double precision     lon, lat
      logical              inv
      integer              sys

c     pi divided by 2
      double precision     pih
      parameter  ( pih = 1.5707963267949d0 )

c     transformation matrix
      double precision     tm( 3, 3, 3 )
c     input vector
      double precision     v0( 3 )
c     output vector
      double precision     v ( 3 )
c     counters
      integer              i, j

c     matrix with definition of rotation
      data tm  / -0.0669887394d0 , -0.8727557659d0 , -0.4835389146d0 ,
     1            0.4927284660d0 , -0.450346958d0  ,  0.7445846333d0 ,
     2           -0.8676008112d0 , -0.1883746012d0 ,  0.4601997848d0 ,
c     ecliptic transform
     3            1.0000000000d0 ,  0.0000000000d0 ,  0.0000000000d0 ,
     4            0.0000000000d0 ,  0.9174369529d0 ,  0.3978811850d0 ,
     5            0.0000000000d0 , -0.3978811850d0 ,  0.9174369529d0 ,
c     supergalactic transform
     6            0.3831583954d0 ,  0.3366379840d0 ,  0.8601537722d0 ,
     7           -0.8972185056d0 , -0.0856688522d0 ,  0.4331971849d0 ,
     8            0.2195190133d0 , -0.9377290203d0 ,  0.2692130889d0 /

c     set up input vector
      v0( 1 ) = dcos( lon ) * dcos( lat )
      v0( 2 ) = dsin( lon ) * dcos( lat )
      v0( 3 ) = dsin( lat )

c     perform matrix multiplication
c     loop over all rows
      do j = 1, 3
c     reset output vector
         v( j ) = 0.0d0
c     loop over all columns
         do i = 1, 3
c     inverse transformation?
            if(       inv ) v( j ) = v( j ) + tm( j, i, sys ) * v0( i )
            if( .not. inv ) v( j ) = v( j ) + tm( i, j, sys ) * v0( i )
c     end loop over all columns
         enddo
c     end loop over all rows
      enddo

c          turn resulting vector into sky coordinates

c     one of the poles found?
      if(  v(1).eq.0.d0 .and. v(2).eq.0.d0 ) then
c     longitude
         lon = 0.d0
c     latitude
         lat = dsign( pih, v(3) )
      else
c     longitude
         lon = datan2( v(2), v(1) )
c     check quadrant (and add 360 degrees )
         if( lon .lt. 0.0d0 ) lon = lon + 4.d0 * pih
c     latitude
         lat = datan2(  v(3), dsqrt( v(1)*v(1) + v(2)*v(2) )  )
      endif

      return
      end

