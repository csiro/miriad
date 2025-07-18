c-----------------------------------------------------------------------
c* versan - Announce task revision information.
c& mrc
c: terminal-i/o
c+
        character*72 function versan(task)

        character task*(*)
c  ---------------------------------------------------------------------
c  Construct task revision information from the version
c  string and announce it on standard output (usually the user's
c  terminal).  The string is also returned as the value of the function,
c  e.g. for use in the history log.
c
c  Input:
c    task       The task name.  If prefixed with '-' the version will
c               not be reported.
c--
c-----------------------------------------------------------------------
      logical   quiet
      integer   i0

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      include 'version.h'
c     Quiet mode?
      quiet = task(:1).eq.'-'
      if (quiet) then
        versan = task(2:)
      else
        versan = task
      endif

      call lcase(versan)
      i0 = len1(versan) + 1

      versan(i0:) = ': Version '//VERSION_STRING

      if (.not.quiet) then
        call output(' ')
        call output(versan(:len1(versan)))
        call output(' ')
      endif

      end
