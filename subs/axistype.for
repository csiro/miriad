c-----------------------------------------------------------------------
c* Axistype - Find the axis label and plane value in user fiendly units
c& mchw
c: plotting
c+
      subroutine axisType(lIn,axis,plane,ctype,label,value,valstr)

      integer   lIn, axis, plane
      character ctype*(*), label*13
      double precision value
      character valstr*13
c  ---------------------------------------------------------------------
c  Find the axis label and plane value in user friendly string.
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
c    valstr     value as a formatted string.
c--
c $Id: axistype.for,v 1.5 2012/02/29 05:12:18 cal103 Exp $
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   k, naxis
      character algo*3, axtype*16, cax*2, units*8, wtype*16

      external  angles, itoaf, len1
      integer   len1
      character angles*13, itoaf*2
c-----------------------------------------------------------------------
c     Get coordinate type and value for input plane.
      cax = itoaf(axis)
      call rdhdi(lIn, 'naxis', naxis, 1)
      if (axis.le.naxis) then
        call coInit(lIn)
        call coCvt1(lIn, axis, 'ap', dble(plane), 'aw', value)
        call coGetA(lIn, 'ctype'//cax, ctype)
        call coCtype(ctype, axtype, wtype, algo, units)

c       Convert to user friendly label and formatted value.
        k = (13 - len1(wtype))/2 + 1
        if (wtype.eq.'RA') then
          label(k:) = wtype
          valstr = angles(value*DR2D/15d0)

        else if (units.eq.'rad') then
          label(k:) = wtype
          valstr = angles(value*DR2D)

        else if (units.eq.'km/s') then
          label(k:) = wtype
          write(valstr,'(g13.3)') value

        else if (units.eq.'GHz') then
          label = '  Frequency  '
          write(valstr,'(g13.6)') value

        else
          label(k:) = wtype(:13)
          write(valstr,'(g13.6)') value
        endif

      else
        value  = 0d0
        label  = 'no axis '//cax
        valstr = ' '
      endif

      end
