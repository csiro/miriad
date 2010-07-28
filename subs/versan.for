c-----------------------------------------------------------------------
c* versan - Announce task revision information.
c& mrc
c: terminal-i/o
c+
        character*80 function versan (task, rcsrev, rcsdat)

        character task*(*), rcsrev*(*), rcsdat*(*)

c  Construct task revision information from the RCS Revision and Date
c  strings and announce it on standard output (usually the user's
c  terminal).  The string is also returned as the value of the function,
c  e.g. for use in the history log.
c
c  Input:
c    task       The task name.  If prefixed with '-' the revision will
c               not be reported.
c    rcsrev     RCS Revision string.
c    rcsid      RCS Date string.
c--
c  $Id: versan.for,v 1.6 2010/07/28 05:33:03 cal103 Exp $
c-----------------------------------------------------------------------
      logical   quiet
      integer   i0, i1, i2, l, len1
c-----------------------------------------------------------------------
c     Quiet mode?
      quiet = task(:1).eq.'-'
      if (quiet) then
        versan = task(2:)
      else
        versan = task
      end if

      call lcase (versan)
      i0 = len1(versan) + 1

      versan(i0:) = ': Revision '
      i0 = i0 + 11

c     Parse the RCS revision information.
      i1 = 12
      l  = len1(rcsrev)
      if (rcsrev(:9).eq.'$Revision' .and. l.gt.i1) then
c       Extract the revision number.
        i2 = i1
        call scanchar (rcsrev, i2, l, ' ')
        i2 = i2 - 1

        versan(i0:) = rcsrev(i1:i2)
        i0 = i0 + (i2 - i1 + 1)

c       Extract the revision date and time.
        i1 = 8
        l  = len1(rcsdat)
        if (rcsdat(:5).eq.'$Date' .and. l.gt.i1) then
c         Date.
          i2 = i1
          call scanchar (rcsdat, i2, l, ' ')

c         Time.
          i2 = i2 + 1
          call scanchar (rcsdat, i2, l, ' ')

          versan(i0:) = ', ' // rcsdat(i1:i2) // 'UTC'
        end if

      else
        versan(i0:) = '(not recorded)'
      endif

      if (.not.quiet) then
        call output (' ')
        call output (versan(:len1(versan)))
        call output (' ')
      end if

      return
      end
