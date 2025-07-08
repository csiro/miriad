c**********************************************************************
c   These subroutines take a value of type double, real or integer
c   and write them left justified into a string with the specified 
c   Fortran format.  The length of the string is returned.
c
c   strfd    Format double precision value
c   strfr    Format real value
c   strfi    Format integer value
c
c  History:
c     nebk   25aug93    Original version
c                     
c**********************************************************************
c
c* strfd -- Left justify a double into string and return length
c& nebk
c: strings
c+
      subroutine strfd (val, fmt, string, il)
      implicit none
c
      character*(*) string, fmt
      double precision val
      integer il
c
c  Left justify a double precision number into a string with the given 
c  format and return the length of the string.
c
c  Input:
c   val     Double precision value to format
c   fmt     Fortran format such as "(f7.2)".   No checks are made to
c           ensure that VALUE and FMT do not cause a type mismatch
c  Input/output:
c   string  String to hold result
c   il      Length of string.  If the output string is blank, il=1
c--
c-----------------------------------------------------------------------
      integer len1, i
      character*132 tmp
c-----------------------------------------------------------------------
      string = ' '
      write (tmp, fmt) val
      if (tmp.eq.' ') then
        il = 1
      else          
c
        il = len1(tmp)
        i = 1
        do while (tmp(i:i).eq.' ' .and. i.lt.il)
          i = i + 1
        end do
      end if
c
      string = tmp(i:)
      il = il - i + 1
c
      end
c
c
c* strfr -- Left justify a real into string and return length
c& nebk
c: strings
c+
      subroutine strfr (val, fmt, string, il)
      implicit none
c
      character*(*) string, fmt
      real val
      integer il
c
c  Left justify a real number into a string with the given 
c  format and return the length of the string.
c
c  Input:
c   val     Real value to format
c   fmt     Fortran format such as "(f7.2)".   No checks are made to
c           ensure that VALUE and FMT do not cause a type mismatch
c  Input/output:
c   string  String to hold result
c   il      Length of string.  If the output string is blank, il=1
c--
c-----------------------------------------------------------------------
      integer len1, i
      character tmp*132
c-----------------------------------------------------------------------
      string = ' '
      write (tmp, fmt) val
      if (tmp.eq.' ') then
        il = 1
      else          
c
        il = len1(tmp)
        i = 1
        do while (tmp(i:i).eq.' ' .and. i.lt.il)
          i = i + 1
        end do
      end if
c
      string = tmp(i:)
      il = il - i + 1
c
      end
c
c
c* strfi -- Left justify an integer into string and return length
c& nebk
c: strings
c+
      subroutine strfi (ival, fmt, string, il)
      implicit none
c
      character*(*) string, fmt
      integer ival, il
c
c  Left justify an integer number into a string with the given 
c  format and return the length of the string.
c
c  Input:
c   ival    Integer value to format
c   fmt     Fortran format such as "(i4)".  No checks are made to
c           ensure that VALUE and FMT do not cause a type mismatch
c  Input/output:
c   string  String to hold result
c   il      Length of string.  If the output string is blank, il=1
c--
c-----------------------------------------------------------------------
      integer len1, i
      character tmp*132
c-----------------------------------------------------------------------
      string = ' '
      write (tmp, fmt) ival
      if (tmp.eq.' ') then
        il = 1
      else          
c
        il = len1(tmp)
        i = 1
        do while (tmp(i:i).eq.' ' .and. i.lt.il)
          i = i + 1
        end do
      end if
c
      string = tmp(i:)
      il = il - i + 1
c
      end
