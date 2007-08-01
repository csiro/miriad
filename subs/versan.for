c-----------------------------------------------------------------------
c* versan - Announce task version information.
c& mrc
c: terminal-i/o
c+
        character*80 function versan (task, rcsid)

        character task*(*), rcsid*(*)

c  Construct task version information from the RCS Id string and
c  announce it on standard output (usually the user's terminal).  The
c  string is also returned as the value of the function, e.g. for use
c  in the history log.
c
c  Input:
c    task       The task name.  If prefixed with '-' the version will
c               not be reported.
c    rcsid      RCS version Id string.
c--
c  $Id: versan.for,v 1.2 2007/08/01 00:22:40 cal103 Exp $
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

      versan(i0:) = ': Version'
      i0 = i0 + 9

      i1 = 7
      l  = len1(rcsid)
      if (rcsid(:3).eq.'$Id' .and. l.gt.i1) then
        call scanchar (rcsid, i1, l, ' ')

        i2 = i1 + 1
        call scanchar (rcsid, i2, l, ' ')
        i2 = i2 - 1

        versan(i0:) = rcsid(i1:i2)
        i0 = i0 + (i2 - i1 + 1)

        i1 = i2 + 1
        i2 = i1 + 1
        call scanchar (rcsid, i2, l, ' ')
        i2 = i2 + 1
        call scanchar (rcsid, i2, l, ' ')

        versan(i0:) = ',' // rcsid(i1:i2) // 'UTC'

      else
        versan(i0:) = ' (not recorded)'
      endif

      if (.not.quiet) then
        call output (' ')
        call output (versan(:len1(versan)))
        call output (' ')
      end if

      return
      end
