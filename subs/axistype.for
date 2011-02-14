c-----------------------------------------------------------------------
c* Axistype - Find the axis label and plane value in user friendly units
c& mchw
c: plotting
c+
      subroutine AxisType(lIn,axis,plane,ctype,label,value,units)

      integer lIn,axis,plane
      character ctype*9,label*13,units*13
      double precision value

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
c $Id: axistype.for,v 1.2 2011/02/14 05:07:48 cal103 Exp $
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   naxis
      real      cdelt, crpix, crval
      character caxis*1

      character angles*13, itoaf*1
      external  angles, itoaf
c-----------------------------------------------------------------------
c     Get ctype and value for input plane.
      caxis = itoaf(axis)
      call rdhdi(lIn, 'naxis', naxis, 1)
      if (axis.le.naxis) then
        call rdhda(lIn, 'ctype'//caxis, ctype, ' ')
        call rdhdr(lIn, 'crval'//caxis, crval, 0.0)
        call rdhdr(lIn, 'crpix'//caxis, crpix, 0.0)
        call rdhdr(lIn, 'cdelt'//caxis, cdelt, 0.0)
        value = crval + (plane-crpix)*cdelt

c       Convert to user friendly label and units.
        if (ctype(1:2).eq.'RA') then
          label = '     RA      '
          units = angles(value*DR2D/15d0)
        else if (ctype(1:3).eq.'DEC') then
          label = '     DEC     '
          units = angles(value*DR2D)
        else if (ctype(1:4).eq.'VELO') then
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
        label = 'no axis '//caxis
        units = ' '
      endif

      end
