c***********************************************************************
c*LSearchD -- Linear search in double precision array
c:misc
c&pjt
c+
      SUBROUTINE lsearchd(n,x,t,i)
c
      INTEGER n, i
      DOUBLE PRECISION x(n), t
c
c  Input:
c	n	length of array x
c	x	sorted array of values
c	t	value to look for it's location w.r.t. array
c	i	initial guess, if non-negative
c  Output:
c	i	interval where t is inside x
c
c  Linear search, returns the interval 
c  in which t is in the array x():   (0 <= i <= n)
c  0 and n are left and right of the min and max,
c  all others internal to the array.
c  It is assumed the array is sorted
c-----------------------------------------------------------------------
      LOGICAL found

      IF(t.LT.x(1)) THEN
         i = 0
         RETURN
      ELSE IF (t.GE.x(n)) THEN
         i = n
         RETURN
      ELSE
         i = 0
         found = .FALSE.
         DOWHILE(i.lt.n .AND. .NOT.found)
            i = i+1
            IF(t.LT.x(i))THEN
               found = .TRUE.
               i=i-1
            ENDIF
         ENDDO
      ENDIF
         
      END
c***********************************************************************

