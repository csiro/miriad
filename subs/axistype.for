c-----------------------------------------------------------------------
c* Axistype - Find the axis label and plane value in user fiendly units
c& mchw
c: plotting
c+
      subroutine axisType(lIn,axis,plane,ctype,label,value,units)

      integer   lIn, axis, plane
      character ctype*(*), label*13
      double precision value
      character units*13
c-----------------------------------------------------------------------
c  Find the axis label and plane value in user friendly units.
c
c  Input:
c    lIn        The handle of the image.
c    axis       The image axis.
c    plane      The image plane along this axis.
c
c  Output:
c    ctype      The official ctype for the input axis.
c    label      A nice label for this axis.
c    value      The value at the plane along this axis.
c    units      User friendly units for this axis.
c--
c $Id: axistype.for,v 1.3 2011/10/27 06:42:41 cal103 Exp $
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   naxis
      double precision cdelt, crpix, crval
      character cax*2

      external  angles, itoaf
      character angles*13, itoaf*2
c-----------------------------------------------------------------------
c     Get ctype and value for input plane.
      cax = itoaf(axis)
      call rdhdi(lIn, 'naxis', naxis, 1)
      if (axis.le.naxis) then
        call coInit(lIn)
        call coCvt1(lIn, axis, 'ap', dble(plane), 'aw', value)
        call coAxGet(lIn, axis, ctype, crpix, crval, cdelt)

c       Convert to user friendly label and units.
        if (ctype(1:2).eq.'RA') then
          label = '     RA      '
          units = angles(value*DR2D/15d0)

        else if (ctype(1:3).eq.'DEC') then
          label = '     DEC     '
          units = angles(value*DR2D)

        else if (ctype(1:4).eq.'VRAD' .or. ctype(1:4).eq.'VOPT' .or.
     *           ctype(1:4).eq.'VELO' .or. ctype(1:4).eq.'FELO') then
          label = '  Velocity   '
          write(units,'(g13.3)') value

        else if (ctype(1:4).eq.'FREQ') then
          label = '  Frequency  '
          write(units,'(g13.6)') value

        else
          label = ctype
          write(units,'(g13.6)') value
        endif

      else
        value = 0d0
        label = 'no axis '//cax
        units = ' '
      endif

      end
