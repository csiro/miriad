c***********************************************************************
c
c  History:
c
c     ??oct88  wh  Original version (called angles).
c     ??jul90 pxt  Fix the position of the minus sign and add round off.
c      5feb91  jm  Convert routine into Miriad format and added the
c                  functions rangle and hangle.
c     3sep93 nebk  Add dangleh (h=high precision)
c    17aug94 nebk  Add hangleh (h=high precision)
c
c***********************************************************************
c* Dangle - Convert degrees/hours value into a formatted string.
c& jm
c: utilities
c+
      character*(*) function dangle(theta)
c
      implicit none
      double precision theta
c
c  Converts an angle expressed in degrees or hours into a string.
c
c  Inputs:
c    theta    Angle in decimal degrees or hours.

c  Output:
c    dangle   Angle formated into a string with format:
c               [+/-]DD:MM:SS.SS
c
c--
c-----------------------------------------------------------------------
      double precision ROUND
      parameter (ROUND=0.5/360000.0d0)
c
      integer n
      character line*13, string*13
      double precision deg
c
      deg = abs(theta) + round
      n = 0
      if (theta .lt. 0) then
        n = n + 1
        string(n:n) = '-'
      end if
c
      write (line,'(i3,'':'',i2.2,'':'',i2.2,''.'',i2.2)')
     *  int(deg), mod(int(60*deg),60), mod(int(3600*deg),60),
     *  mod(int(3600*100*deg),100)
      if (line(1:1) .ne. ' ') then
        string(n+1:n+2) = line(1:2)
        n = n + 2
      else if (line(2:2) .ne. ' ') then
        string(n+1:n+1) = line(2:2)
        n = n + 1
      end if
      string(n+1:n+10) = line(3:12)
      n = n + 10
      dangle = string(1:n)
      return
      end
c***********************************************************************
c* Dangleh - Convert degrees/hours value into a formatted string
c& jm
c: utilities
c+
      character*(*) function dangleh(theta)
c
      implicit none
      double precision theta
c
c  Converts an angle expressed in degrees or hours into a string.
c  Gives one more decimal place than dangle in the seconds string.
c
c  Inputs:
c    theta    Angle in decimal degrees or hours.

c  Output:
c    dangleh  Angle formated into a string with format:
c               [+/-]DD:MM:SS.SSS
c
c--
c-----------------------------------------------------------------------
      double precision ROUND
      parameter (ROUND=0.5/3600000.0d0)
c
      integer n
      character line*14, string*14
      double precision deg
c
      deg = abs(theta) + round
      n = 0
      if (theta .lt. 0) then
        n = n + 1
        string(n:n) = '-'
      end if
c
      write (line,'(i3,'':'',i2.2,'':'',i2.2,''.'',i3.3)')
     *  int(deg), mod(int(60*deg),60), mod(int(3600*deg),60),
     *  mod(int(3600*1000*deg),1000)
      if (line(1:1) .ne. ' ') then
        string(n+1:n+2) = line(1:2)
        n = n + 2
      else if (line(2:2) .ne. ' ') then
        string(n+1:n+1) = line(2:2)
        n = n + 1
      end if
      string(n+1:n+11) = line(3:13)
      n = n + 11
      dangleh = string(1:n)
      return
      end
c***********************************************************************
c* Rangle - Convert degrees value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function rangle(theta)
c
      implicit none
      double precision theta
c
c  Converts an angle expressed in radians into a string.
c
c  Inputs:
c    theta    Angle in radians.

c  Output:
c    rangle   Angle formated into a string with format:
c               [+/-]DD:MM:SS.SS
c
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      character Dangle*13
c
      rangle = Dangle((180.0 * theta) / DPI)
      return
      end
c***********************************************************************
c* Hangle - Convert hours value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function hangle(theta)
c
      implicit none
      double precision theta
c
c  Converts an angle expressed in radians into a string.
c
c  Inputs:
c    theta    Angle in radians.

c  Output:
c    rangle   Angle formated into a string with format:
c               [+/-]HH:MM:SS.SS
c
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      character Dangle*13
c
      hangle = Dangle((12.0 * theta) / DPI)
      return
      end
c***********************************************************************
c* Hangleh - Convert hours value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function hangleh(theta)
c
      implicit none
      double precision theta
c
c  Converts an angle expressed in radians into a string.
c  Gives one more decimal place than hangle in the seconds string.
c
c  Inputs:
c    theta    Angle in radians.

c  Output:
c    rangle   Angle formated into a string with format:
c               [+/-]HH:MM:SS.SS
c
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      character Dangleh*13
c
      hangleh = Dangleh((12.0d0 * theta) / DPI)
      return
      end
c***********************************************************************
c* Rangleh - Convert degrees value (in radians) into a formatted string.
c& jm
c: utilities
c+
      character*(*) function rangleh(theta)
c
      implicit none
      double precision theta
c
c  Converts an angle expressed in radians into a string.
c  Gives one more decimal place than rangle in the seconds string.
c
c  Inputs:
c    theta    Angle in radians.

c  Output:
c    rangle   Angle formated into a string with format:
c               [+/-]DD:MM:SS.SS
c
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      character Dangle*14
c
      rangleh = Dangleh((180.0d0 * theta) / DPI)
      return
      end

