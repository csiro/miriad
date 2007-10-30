c************************************************************************
c* headcopy - Copy the header of a dataset
c& bpw
c: utilities, image-i/o
c+
      subroutine headcopy( tnoinp, tnoout, axnum, naxis, blc, trc )

      integer tnoinp, tnoout
      integer axnum(*)
      integer naxis
      integer blc(*), trc(*)

c Headcopy copies all the small items (the header) of one dataset to
c another. It's list supposedly contains all official header elements.
c A few items must be treated in a special way, as described below.
c
c - The following items are copied directly:
c    'history ','bmaj    ','bmin    ','bpa     ','bunit   ',
c    'date-obs','epoch   ','instrume','ltype   ','lstart  ',
c    'lwidth  ','lstep   ','niters  ','object  ','observer',
c    'obsdec  ','obsra   ','pbfwhm  ','restfreq','telescop',
c    'vobs    ','xshift  ','yshift  ','btype'
c
c - The datamin and datamax items are explicitly excluded. One should
c recalculate the min and max of the output dataset and update these
c two items separately with wrhdr.
c
c - The naxis and naxis# items are not treated by headcopy, as
c xy(z)open already must take care of them.
c
c - crpix, crval, cdelt, crota and ctype are copied, but: axes can be
c exchanged, deleted or reversed and then the order or values must be
c changed. The array axnum gives the relation between old and new axes.
c The following table gives a few examples.
c                                    axnum(1) axnum(2) axnum(3) axnum(4)
c direct copy                           1        2        3        4
c delete z-axis (e.g. for contsub)      1        2        0        0
c delete x-axis (contsub on cube with
c                velocities on x-axis   2        3        0        0
c output is zxy cube                    3        1        2        0
c x-axis was reversed                  -1        2        3        4
c
c - For reversed axes cdelt is multiplied by -1 and 180 is added to
c crota.
c - For output datasets whose corners are different from the input
c dataset, crpix may have to be corrected. This is done using the arrays
c blc and trc, which give the corners of the output dataset relative to
c the pixel numbers of the input dataset. blc is used for non-reversed
c axes and trc for reversed axes. Often one can use the output of
c subroutine boxinfo to get blc and trc.
c
c It is often the case that no axes are reversed and that the sizes of
c the input and output datasets are the same. Then it suffices to set
c axnum(1) to zero, i.e. headcopy( tnoinp, tnoout, 0, naxis, 0,0 ) will
c do the trick.
c
c   Input:
c      tnoinp     handle of input image
c      tnoout     handle of output image
c      axnum      array giving relation betweem old and new axes
c                 (negative values imply axis reversal)
c      naxis      dimension of input dataset
c      blc        list of bottom-left-corners of input dataset region
c      trc        list of top-right-corners of input dataset region
c--
c History:
c
c    bpw  22jun91  Installed
c    bpw  04aug91  More info in document
c    bpw  05aug91  Add axnum(1)=0 possibility
c    bpw  11dec92  Add btype
c    bpw  16dec92  No special copy for btype, per Neil's remark
c    bpw   1feb93  Made crpix double precision too
c
c***********************************************************************

      character*8      c
      integer          n, k
      character*1      itoaf
      double precision dvalue
      character*80     avalue
      logical          hdprsnt

      integer          NKEYS
      parameter        ( NKEYS = 29 )
      character*8      keyw(NKEYS)
      data keyw/
     *    'crpix   ','crval   ','cdelt   ','crota   ','ctype   ',
     *    'history ',
     *    'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'date-obs','epoch   ','instrume',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ',
     *    'niters  ','object  ','observer','obsdec  ','obsra   ',
     *    'pbfwhm  ','restfreq','telescop','vobs    ',
     *    'xshift  ','yshift  ','btype'/

      do k = 1, 5
         do n = 1, naxis
            if( axnum(1) .eq. 0 ) then
               c = keyw(k)(1:5) // itoaf( n )
               call hdcopy( tnoinp, tnoout, c )
            else
            if( axnum(n) .ne. 0 ) then
               c = keyw(k)(1:5) // itoaf( abs(axnum(n)) )
               if( hdprsnt( tnoinp, c ) ) then
                  if( k.eq.1 ) call rdhdd( tnoinp, c, dvalue, 0. )
                  if( k.eq.2 ) call rdhdd( tnoinp, c, dvalue, 0. )
                  if( k.eq.3 ) call rdhdd( tnoinp, c, dvalue, 0. )
                  if( k.eq.4 ) call rdhdd( tnoinp, c, dvalue, 0. )
                  if( k.eq.5 ) call rdhda( tnoinp, c, avalue, ' ' )
                  if( axnum(n).gt.0 ) then
                     if( k.eq.1 ) dvalue = dvalue - blc( axnum(n)) + 1
                  endif
                  if( axnum(n).lt.0 ) then
                     if( k.eq.1 ) dvalue = trc(-axnum(n)) - dvalue + 1
                     if( k.eq.3 ) dvalue = -dvalue
                     if( k.eq.4 ) dvalue = dvalue - 180.d0
                  endif
                  c = keyw(k)(1:5) // itoaf(       n  )
                  if( k.eq.1 ) call wrhdd( tnoout, c, dvalue )
                  if( k.eq.2 ) call wrhdd( tnoout, c, dvalue )
                  if( k.eq.3 ) call wrhdd( tnoout, c, dvalue )
                  if( k.eq.4 ) call wrhdd( tnoout, c, dvalue )
                  if( k.eq.5 ) call wrhda( tnoout, c, avalue )
               endif
            endif
            endif
         enddo
      enddo

      do k = 6, NKEYS
         call hdcopy( tnoinp, tnoout, keyw(k) )
      enddo

      return
      end
