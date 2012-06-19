c************************************************************************
c
c  Check that a collection of assertions are met.
c
c  History:
c    pjt  16-may-90	Originally written
c    pjt  18-mar-91	see if INQUIRE will do... since file != directory
c			this may file on VMS??
c    pjt  18-mar-92	renamed from assert to   assertl to avoid potential name
c			pollution of needed C libraries
c    rjs  13-jul-00	Replace "inquire" with "hexists" routine, to
c			avoid a bug on some Digital UNIX systems.
c    pjt  13-jul-00     Fixed a documentation bug
c    pjt  14-apr-06     Added assertigti and assertigei (asserti2 will disappear)
c
c************************************************************************
c	SUBROUTINE assert(cond,mesg)
c	CHARACTER mesg*(*)
c	LOGICAL   cond
c	CALL bug('w','Old style routine ASSERT called - use ASSERTL')
c	CALL assertl(cond,mesg)
c	END	
c* assertl -- Assert a boolean condition
c& pjt
c: error-handling
c+
	SUBROUTINE assertl (cond,mesg)
c
	CHARACTER mesg*(*)
	LOGICAL   cond
c
c   Assertb that 'cond' is .TRUE., otherwise bail out by calling
c   'bug' with 'f' and the mesg as provided in the arguments.
c   Although an IF-statement will often do just as well, this make
c   the code a bit smaller and readable:
c
c   Example:
c       CALL assertl(file.NE.' ','No filename specified')
c
c   Input:
c       cond    -- logical to test, if FALSE bail out
c       mesg    -- message passed to bug
c----------------------------------------------------------------------|
      IF (cond) RETURN

      CALL bug('f',mesg)
      RETURN
      END
c************************************************************************
c* assertf -- Assert a file existence condition
c& pjt
c: error-handling, file i/o
c+
      SUBROUTINE assertf (name,cond,mesg)
c
      CHARACTER name*(*), mesg*(*)
      LOGICAL   cond
c
c   Assert that a file exists or does not. Useful to check after
c   the keyroutines are called, to prevent lot's of CPU burning
c   before program decides it couldn't write it's information.
c
c   Example:
c	CALL keya('in',fin,' ')
c       CALL assertf(fin,.TRUE.,'File not present')
c	CALL keya('out',fout,' ')
c	CALL assertf(fout,.FALSE.,'File present')
c
c   Input:
c       name  -- name of file to test for
c       cond  -- if TRUE file must exist, if FALSE file must not exist
c       mesg  -- message passed to bug
c----------------------------------------------------------------------|
      LOGICAL   fex
c
c  Externals.
c
      logical hexists
c
c     old technique:    INQUIRE(FILE=name(1:len1(name)),EXIST=fex)
c                   [this seems to fail on Digital Unix for directories]
c
      fex = hexists(0,name)
      IF (cond .AND. .NOT.fex  .OR.  .NOT.cond .AND. fex) THEN
         CALL bug('f',mesg)
      ENDIF

      END
c************************************************************************
c* asserti2 -- Assert a condition (to be deprecated)
c& pjt 
c: error-handling
c+
      SUBROUTINE asserti2 (i1,i2,mesg)
c
      INTEGER   i1, i2
      CHARACTER mesg*(*)
c
c  This routine is deprecated, please use assertigt2 or assertigei
c  asserti2 is the same as assertigei
c
c   Input:
c       i1      -- first integer, often an available size (of an array)
c       i2      -- second integer, often size needed (for the array)
c       mesg    -- message passed to bug
c----------------------------------------------------------------------|

      CHARACTER   line*80

      IF (i1.GE.i2) RETURN

      WRITE(line,'(''### '',I7,'' is less than '',I7)') i1, i2
      CALL output(line)
      CALL bug('f',mesg)

      RETURN
      END


c************************************************************************
c* assertigti -- Assert a condition integer1 greater than integer2
c& pjt 
c: error-handling
c+
      SUBROUTINE assertigti (i1,i2,mesg)
c
      INTEGER   i1, i2
      CHARACTER mesg*(*)
c
c   Assert that I1 > I2, otherwise bail out by calling
c   'bug' with the type 'f' and mesg as provided in the arguments.
c   Is often used to check if enough memory in arrays for operations
c   Example:
c       CALL asserti2(MAXBUF,naxis1*naxis2,'MAXBUF vs. naxis1*naxis2')
c   Note that
c       CALL assert(MAXBUF.GT.naxis1*naxis2,'MAXBUF vs. naxis1*naxis2')
c   would also do the trick, except it will not print out values.
c
c   Input:
c       i1      -- first integer, often an available size (of an array)
c       i2      -- second integer, often size needed (for the array)
c       mesg    -- message passed to bug
c----------------------------------------------------------------------|

      CHARACTER   line*80

      IF (i1.GT.i2) RETURN

      WRITE(line,'(''### '',I7,'' is less or equal than '',I7)') i1, i2
      CALL output(line)
      CALL bug('f',mesg)

      RETURN
      END
c************************************************************************
c* assertigei -- Assert a condition integer1 greater or equal than integer2
c& pjt 
c: error-handling
c+
      SUBROUTINE assertigei (i1,i2,mesg)
c
      INTEGER   i1, i2
      CHARACTER mesg*(*)
c
c   Assert that I1 >= I2, otherwise bail out by calling
c   'bug' with the type 'f' and mesg as provided in the arguments.
c   Is often used to check if enough memory in arrays for operations
c   Example:
c       CALL asserti2(MAXBUF,naxis1*naxis2,'MAXBUF vs. naxis1*naxis2')
c   Note that
c       CALL assert(MAXBUF.GT.naxis1*naxis2,'MAXBUF vs. naxis1*naxis2')
c   would also do the trick, except it will not print out values.
c
c   Input:
c       i1      -- first integer, often an available size (of an array)
c       i2      -- second integer, often size needed (for the array)
c       mesg    -- message passed to bug
c----------------------------------------------------------------------|

      CHARACTER   line*80

      IF (i1.GE.i2) RETURN

      WRITE(line,'(''### '',I7,'' is less than '',I7)') i1, i2
      CALL output(line)
      CALL bug('f',mesg)

      RETURN
      END




