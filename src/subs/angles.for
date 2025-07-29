c***********************************************************************
c  A set of routines for formatting angles and times.
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c***********************************************************************

c* dangle - Convert degrees/hours value into a formatted string.
c& jm
c: utilities
c+
      character*(*) function dangle(theta)

      double precision theta
c  ---------------------------------------------------------------------
c  Convert an angle expressed in degrees or hours into a string.
c
c  Inputs:
c    theta    Angle in decimal degrees or hours.
c
c  Output:
c    dangle   Angle formated into a string with format:
c               [+/-]DD:MM:SS.SS
c-----------------------------------------------------------------------
      double precision ROUND
      parameter (ROUND=0.5d0/3600d2)

      double precision deg
      character text*12
c-----------------------------------------------------------------------
      deg = abs(theta) + ROUND
      write(text,10) int(deg), mod(int(60d0*deg),60),
     *  mod(int(3600d0*deg),60), mod(int(3600d2*deg),100)
 10   format(i3.2,':',i2.2,':',i2.2,'.',i2.2)

      if (theta.lt.0d0) then
        dangle = '-'
      else
        dangle = '+'
      endif

      if (text(1:1).ne.' ') then
        dangle(2:) = text
      else if (text(2:2).ne.' ') then
        dangle(2:) = text(2:)
      else
        dangle(2:) = text(3:)
      endif

      end

c***********************************************************************

c* dangleh - Convert degrees/hours value into a formatted string
c& jm
c: utilities
c+
      character*(*) function dangleh(theta)

      double precision theta
c  ---------------------------------------------------------------------
c  Convert an angle expressed in degrees or hours into a string.
c  Gives one more decimal place than dangle in the seconds string.
c
c  Inputs:
c    theta    Angle in decimal degrees or hours.
c
c  Output:
c    dangleh  Angle formated into a string with format:
c               [+/-]DD:MM:SS.SSS
c-----------------------------------------------------------------------
      double precision ROUND
      parameter (ROUND=0.5d0/3600d3)

      double precision deg
      character text*13
c-----------------------------------------------------------------------
      deg = abs(theta) + ROUND
      write(text,10) int(deg), mod(int(60d0*deg),60),
     *  mod(int(3600d0*deg),60), mod(int(3600d3*deg),1000)
 10   format(i3.2,':',i2.2,':',i2.2,'.',i3.3)

      if (theta.lt.0d0) then
        dangleh = '-'
      else
        dangleh = '+'
      endif

      if (text(1:1).ne.' ') then
        dangleh(2:) = text
      else if (text(2:2).ne.' ') then
        dangleh(2:) = text(2:)
      else
        dangleh(2:) = text(3:)
      endif

      end

c***********************************************************************

c* rangle - Convert degrees value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function rangle(theta)

      double precision theta
c  ---------------------------------------------------------------------
c  Convert an angle expressed in radians into a string.
c
c  Inputs:
c    theta    Angle in radians.
c
c  Output:
c    rangle   Angle formated into a string with format:
c               [+/-]DD:MM:SS.SS
c-----------------------------------------------------------------------
      include 'mirconst.h'

      external  dangle
      character dangle*13
c-----------------------------------------------------------------------
      rangle = dangle(theta*DR2D)

      end

c***********************************************************************

c* rangleh - Convert degrees value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function rangleh(theta)

      double precision theta
c  ---------------------------------------------------------------------
c  Convert an angle expressed in radians into a string.  Gives one more
c  decimal place than rangle in the seconds string.
c
c  Inputs:
c    theta    Angle in radians.
c
c  Output:
c    rangleh  Angle formated into a string with format:
c               [+/-]DD:MM:SS.SSS
c-----------------------------------------------------------------------
      include 'mirconst.h'

      external  dangleh
      character dangleh*14
c-----------------------------------------------------------------------
      rangleh = dangleh(theta*DR2D)

      end

c***********************************************************************

c* hangle - Convert hours value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function hangle(theta)

      double precision theta
c  ---------------------------------------------------------------------
c  Convert an angle expressed in radians into a string.
c
c  Inputs:
c    theta    Angle in radians.
c
c  Output:
c    rangle   Angle formated into a string with format:
c               [+/-]HH:MM:SS.SS
c-----------------------------------------------------------------------
      include 'mirconst.h'

      character text*13

      external  dangle
      character dangle*13
c-----------------------------------------------------------------------
      text = dangle(theta*DR2D/15d0)

      if (text(:1).eq.'+') then
        hangle = text(2:)
      else
        hangle = text
      endif

      end

c***********************************************************************

c* hangleh - Convert hours value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function hangleh(theta)

      double precision theta
c  ---------------------------------------------------------------------
c  Convert an angle expressed in radians into a string.  Gives one more
c  decimal place than hangle in the seconds string.
c
c  Inputs:
c    theta    Angle in radians.
c
c  Output:
c    rangle   Angle formated into a string with format:
c               [+/-]HH:MM:SS.SSS
c-----------------------------------------------------------------------
      include 'mirconst.h'

      character text*13

      external  dangle
      character dangleh*13
c-----------------------------------------------------------------------
      text = dangleh(theta*DR2D/15d0)

      if (text(:1).eq.'+') then
        hangleh = text(2:)
      else
        hangleh = text
      endif

      end
