c* sortidxd -- index sort of a an array of double precision values
c& pjt
c: sorting, indexing
c+
      subroutine sortidxd (n, x ,idx)
c
      integer n, idx(n)
      double precision x(n)
c
c  sortidxX:  (shell) sort of an array into a integer index-array
c		See also e.g. K&R pp108
c	Various versions exist:
c	    sortidxa    index sorting of character array
c	    sortidxi    index sorting of integer array
c	    sortidxr    index sorting of real array
c	    sortidxd    index sorting of double precision array
c
c	input:  x[]   array of values
c		n     number of elements in x to sort
c
c	output: idx[] 'pointer' array, such that x[idx[i-1]]<x[idx[i]] for
c		      i=2..n
c
c--
c
      integer gap, i, j, tmp

      do 100 i=1,n
         idx(i) = i
  100 continue

      gap = n/2
  200 continue
      if (gap.le.0) goto 999
         do 500 i=gap,n-1
            do 400 j=i-gap+1,1,-gap
              if (x(idx(j)).le.x(idx(j+gap))) goto 450
               tmp = idx(j)
               idx(j) = idx(j+gap)
               idx(j+gap) = tmp
  400       continue
  450    continue
  500 continue
      gap = gap/2
      goto 200
  999 continue      
      return
      end

c* sortidxr -- index sort of a an array of real values
c& pjt
c: sorting, indexing
c+
      subroutine sortidxr (n, x ,idx)
c
      integer n, idx(n)
      real x(n)
c
c       -- see documentation on sortidxd
c--
c
      integer gap, i, j, tmp

      do 100 i=1,n
         idx(i) = i
  100 continue

      gap = n/2
  200 continue
      if (gap.le.0) goto 999
         do 500 i=gap,n-1
            do 400 j=i-gap+1,1,-gap
              if (x(idx(j)).le.x(idx(j+gap))) goto 450
               tmp = idx(j)
               idx(j) = idx(j+gap)
               idx(j+gap) = tmp
  400       continue
  450    continue
  500 continue
      gap = gap/2
      goto 200
  999 continue      
      return
      end

c* sortidxi -- index sort of a an array of integer values
c& pjt
c: sorting, indexing
c+
      subroutine sortidxi (n, x ,idx)
c
      integer n, idx(n)
      integer x(n)
c
c       -- see documentation on sortidxd
c--
c
      integer gap, i, j, tmp

      do 100 i=1,n
         idx(i) = i
  100 continue

      gap = n/2
  200 continue
      if (gap.le.0) goto 999
         do 500 i=gap,n-1
            do 400 j=i-gap+1,1,-gap
              if (x(idx(j)).le.x(idx(j+gap))) goto 450
               tmp = idx(j)
               idx(j) = idx(j+gap)
               idx(j+gap) = tmp
  400       continue
  450    continue
  500 continue
      gap = gap/2
      goto 200
  999 continue      
      return
      end

c* sortidxa -- index sort of a an array of character values
c& pjt
c: sorting, indexing
c+
      subroutine sortidxa (n, x ,idx)
c
      integer n, idx(n)
      character*(*) x(n)
c
c       -- see documentation on sortidxd
c--
c
      integer gap, i, j, tmp
c     integer charcomp

      do 100 i=1,n
         idx(i) = i
  100 continue

      gap = n/2
  200 continue
      if (gap.le.0) goto 999
         do 500 i=gap,n-1
            do 400 j=i-gap+1,1,-gap
c -- fix  below for character variables
c              if (x(idx(j)).le.x(idx(j+gap))) goto 450
c
        call bug('f','sortidxa: not yet implemented')
c                if (charcomp(x(idx(j)),x(idx(j+gap)))) goto 450
c -- fix above
               tmp = idx(j)
               idx(j) = idx(j+gap)
               idx(j+gap) = tmp
  400       continue
  450    continue
  500 continue
      gap = gap/2
      goto 200
  999 continue      
      return
      end

