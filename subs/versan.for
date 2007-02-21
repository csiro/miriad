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
c    task       The task name.
c    rcsid      RCS version Id string.
c--
c  $Id: versan.for,v 1.1 2007/02/21 05:27:50 cal103 Exp $
c-----------------------------------------------------------------------
      integer   i0, i1, i2, l, len1
c-----------------------------------------------------------------------
      versan = task
      call lcase (versan)
      i0 = len1(task) + 1

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

      call output (' ')
      call output (versan(:len1(versan)))
      call output (' ')

      return
      end
