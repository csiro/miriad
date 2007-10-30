c	SUBROUTINE assert(cond,mesg)
c	CHARACTER mesg*(*)
c	LOGICAL   cond
c	CALL bug('w','Old style routine ASSERT called - use ASSERTL')
c	CALL assertl(cond,mesg)
c	END	
c* assertl -- Assert a condition
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
c       CALL assertb(file.NE.' ','No filename specified')
c
c   Input:
c       cond    -- logical to test, if FALSE bail out
c       mesg    -- message passed to bug
c----------------------------------------------------------------------|
c   10-may-90   Peter Teuben - originally written
c   18-mar-92   renamed from assert to   assertl to avoid potential name
c		pollution of needed C libraries

      IF (cond) RETURN

      CALL bug('f',mesg)
      RETURN
      END

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
c   16-may-90   Peter Teuben - originally written
c   18-mar-91   see if INQUIRE will do... since file != directory
c		this may file on VMS??
      LOGICAL   fex
      INTEGER   len1

      INQUIRE(FILE=name(1:len1(name)),EXIST=fex)
      IF (cond .AND. .NOT.fex  .OR.  .NOT.cond .AND. fex) THEN
         CALL bug('f',mesg)
      ENDIF

      END

c* asserti2 -- Assert a condition, otherwise bug out
c& pjt 
c: error-handling
c+
      SUBROUTINE asserti2 (i1,i2,mesg)
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
c   10-may-90   Peter Teuben - originally written

      CHARACTER   line*80

      IF (i1.GE.i2) RETURN

      WRITE(line,'(''### '',I7,'' is less than '',I7)') i1, i2
      CALL output(line)
      CALL bug('f',mesg)

      RETURN
      END



