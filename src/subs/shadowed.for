c* shadowed
c& bpw
c: uv-data
c+
c 
      integer function shadowed( tno, ants, limit )
      integer          tno
      double precision ants
      real             limit

c Shadowed returns whether shadowing occured on the baseline coded in
c ants. The coding is done in the usual manner, i.e. ant1=ants/256 and
c ant2=ants-256*ant1, or ants=ant1*256+ant2. One can use the variable
c preamble(4) returned by uvread as input.
c The check is done for the last record read with uvread.
c The returned value is 0 if there was no shadowing, ant1*256 if ant1
c was shadowed, ant2 if ant2 was shadowed and 256*ant1*ant2 if both were
c shadowed.
c The antenna positions and pointings are read from the visibility
c file to which the handle 'tno' corresponds.
c Data is considered shadowed when the projected baseline is less than
c the value given by the variable limit (in units of meters).
c If the value of limit equals 0, shadowed returns with the value 0.
c
c Inputs:
c   tno         The handle of the uv data file
c   ants        A code giving the antennas to check on shadowing.
c               the code is the same as the one in the preamble(4)
c               variable returned by uvread.
c   limit       Projected baseline below which data is considered
c               shadowed.
c--
c----------------------------------------------------------------------
c  History:
c     bpw  30jan91  Created
c     bpw  09may91  Included forgotten loop
c     bpw  19jun91  Corrected error (reversed sign of test on w)
c     bpw  20jun91  Got rid of maxdim.h inclusion
c     bpw  13apr92  Changed call to assert into assertl
c----------------------------------------------------------------------

      integer          MAXANT
      parameter        ( MAXANT = 50 )
      integer          limitoc2
      double precision ra, dec, lst, ha
      double precision sinh, cosh, sind, cosd
      integer          ant(2), antenna, nants, j, shadow(2)
      double precision antpos(MAXANT,3), posx, posy, posz      
      double precision bx, by, bxy, byx, bz
      double precision u, v, w
      include          'mirconst.h'
      if( limit .eq. 0. ) then
         shadowed = 0
         return
      endif
      limitoc2 = ( limit / (dcmks*1.e-9) ) ** 2
      call uvgetvri( tno, 'nants',  nants, 1 )
      call assertl( nants.le.MAXANT,
     *     'SHADOWED: # of antennas in record larger than array size' )
      call uvgetvrd( tno, 'obsra',  ra,    1 )
      call uvgetvrd( tno, 'obsdec', dec,   1 )
      call uvgetvrd( tno, 'lst',    lst,   1 )
      call uvgetvrd( tno, 'antpos', antpos, nants*3 )
      ha   = lst - ra
      sinh = sin(ha)
      cosh = cos(ha)
      sind = sin(dec)
      cosd = cos(dec)
      call basant( ants, ant(1), ant(2) )
      do j = 1, 2
         shadow(j) = 0
         posx = antpos( ant(j), 1 )
         posy = antpos( ant(j), 2 )
         posz = antpos( ant(j), 3 )
         do antenna = 1, nants
            if( antenna.ne.ant(j) ) then
               bx  = antpos(antenna,1) - posx
               by  = antpos(antenna,2) - posy
               bz  = antpos(antenna,3) - posz
               bxy =  bx*sinh + by*cosh
               byx = -bx*cosh + by*sinh
               u   =  bxy
               v   =  byx*sind + bz*cosd
               w   = -byx*cosd + bz*sind
               if( u*u+v*v .lt. limitoc2  .and.  w.gt.0.d0 )
     *             shadow(j) = ant(j)
            endif
         enddo
      enddo
      shadowed = shadow(1)*256 + shadow(2)
      return
      end

