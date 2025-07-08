c***********************************************************************
c  The sort routines provide a fast method for sorting large arrays.
c
c  History:
c    jm    26jan90    Original sort code from NUMERICAL RECIPES (pp 229).
c    jm    26jan90    Modified for strings.
c    jm    30jan90    Added ability for secondary sort value.
c    jm    03apr90    Added double precision and integer variables.
c    jm    27apr90    Corrected each routine for n=1 bug.
c    jm    18nov91    Improved documentation and changed declarations
c                     implicit undefined (a-z) to implicit none's.
c
c***********************************************************************
c* HSortA -- Perform an index heapsort on a character string array.
c& jm
c: sorting
c+
      subroutine hsorta(n, array, indx)
c
      implicit none
      integer n, indx(n)
      character*(*) array(n)
c
c  HSORTA performs an index based heapsort on a character string array.
c  The number of elements in the array and the array are left unchanged.
c  The size of the indx array should be at least as large as the size of
c  the string array.
c
c     Input:
c       n        The number of elements in the character array.
c       array    The character array on which to base the sort.
c
c     Output:
c       indx     Sorted integer index array such that array(indx(1)) is
c                the smallest and array(indx(n)) is the largest string.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer i, j, k, L, ir, indxt
      character*132 q
c
c External functions.
c
      integer len1
c
c End declarations.
c
      do 11 j = 1, n
          indx(j) = j
   11 continue
      if (n .eq. 1) return
      L = (n / 2) + 1
      ir = n
   10 continue
      if (L .gt. 1)then
          L = L - 1
          indxt = indx(L)
          k = len1(array(indxt))
          q = array(indxt)(1:k)
      else
          indxt = indx(ir)
          k = len1(array(indxt))
          q = array(indxt)(1:k)
          indx(ir) = indx(1)
          ir = ir - 1
          if (ir .eq. 1) then
            indx(1) = indxt
            return
          endif
      endif
      i = L
      j = L + L
   20 continue
      if (j .le. ir) then
          if (j .lt. ir) then
            if (array(indx(j)) .lt. array(indx(j+1))) j = j + 1
          endif
          if (q .lt. array(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j+j
          else
            j = ir+1
          endif
          go to 20
      endif
      indx(i) = indxt
      go to 10
      end
c
c***********************************************************************
c* HSortI -- Perform an indexed heapsort on a integer array.
c& jm
c: sorting
c+
      subroutine hsorti(n, array, indx)
c
      implicit none
      integer n, indx(n)
      integer array(n)
c
c  HSORTI performs an index based heapsort on an integer array.
c  The number of elements in the array and the array are left unchanged.
c  The size of the indx array should be at least as large as the size of
c  the integer array.
c
c     Input:
c       n        The number of elements in the array.
c       array    The integer array on which to base the sort.
c
c     Output:
c       indx     Sorted integer index array such that array(indx(1)) is
c                the smallest and array(indx(n)) is the largest element.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer i, j, L, ir, indxt
      integer q
c
c End declarations.
c
      do 11 j = 1, n
          indx(j) = j
   11 continue
      if (n .eq. 1) return
      L = (n / 2) + 1
      ir = n
   10 continue
      if (L .gt. 1)then
          L = L - 1
          indxt = indx(L)
          q = array(indxt)
      else
          indxt = indx(ir)
          q = array(indxt)
          indx(ir) = indx(1)
          ir = ir - 1
          if (ir .eq. 1) then
            indx(1) = indxt
            return
          endif
      endif
      i = L
      j = L + L
   20 if (j .le. ir) then
          if (j .lt. ir) then
            if (array(indx(j)) .lt. array(indx(j+1))) j = j + 1
          endif
          if (q .lt. array(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j + j
          else
            j = ir + 1
          endif
          go to 20
      endif
      indx(i) = indxt
      goto 10
      end
c
c***********************************************************************
c* HSortR -- Perform an indexed heapsort on a real array.
c& jm
c: sorting
c+
      subroutine hsortr(n, array, indx)
c
      implicit none
      integer n, indx(n)
      real array(n)
c
c  HSORTR performs an index based heapsort on a real array.
c  The number of elements in the array and the array are left unchanged.
c  The size of the indx array should be at least as large as the size of
c  the real array.
c
c     Input:
c       n        The number of elements in the array.
c       array    The real array on which to base the sort.
c
c     Output:
c       indx     Sorted integer index array such that array(indx(1)) is
c                the smallest and array(indx(n)) is the largest element.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer i, j, L, ir, indxt
      real q
c
c End declarations.
c
      do 11 j = 1, n
          indx(j) = j
   11 continue
      if (n .eq. 1) return
      L = (n / 2) + 1
      ir = n
   10 continue
      if (L .gt. 1)then
          L = L - 1
          indxt = indx(L)
          q = array(indxt)
      else
          indxt = indx(ir)
          q = array(indxt)
          indx(ir) = indx(1)
          ir = ir - 1
          if (ir .eq. 1) then
            indx(1) = indxt
            return
          endif
      endif
      i = L
      j = L + L
   20 if (j .le. ir) then
          if (j .lt. ir) then
            if (array(indx(j)) .lt. array(indx(j+1))) j = j + 1
          endif
          if (q .lt. array(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j + j
          else
            j = ir + 1
          endif
          go to 20
      endif
      indx(i) = indxt
      goto 10
      end
c
c***********************************************************************
c* HSortD -- Perform an indexed heapsort on a double precision array.
c& jm
c: sorting
c+
      subroutine hsortd(n, array, indx)
c
      implicit none
      integer n, indx(n)
      double precision array(n)
c
c  HSORTD performs an index based heapsort on a double precision array.
c  The number of elements in the array and the array are left unchanged.
c  The size of the indx array should be at least as large as the size of
c  the double precision array.
c
c     Input:
c       n        The number of elements in the array.
c       array    The double precision array on which to base the sort.
c
c     Output:
c       indx     Sorted integer index array such that array(indx(1)) is
c                the smallest and array(indx(n)) is the largest element.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer i, j, L, ir, indxt
      double precision q
c
c End declarations.
c
      do 11 j = 1, n
          indx(j) = j
   11 continue
      if (n .eq. 1) return
      L = (n / 2) + 1
      ir = n
   10 continue
      if (L .gt. 1)then
          L = L - 1
          indxt = indx(L)
          q = array(indxt)
      else
          indxt = indx(ir)
          q = array(indxt)
          indx(ir) = indx(1)
          ir = ir - 1
          if (ir .eq. 1) then
            indx(1) = indxt
            return
          endif
      endif
      i = L
      j = L + L
   20 if (j .le. ir) then
          if (j .lt. ir) then
            if (array(indx(j)) .lt. array(indx(j+1))) j = j + 1
          endif
          if (q .lt. array(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j + j
          else
            j = ir + 1
          endif
          go to 20
      endif
      indx(i) = indxt
      goto 10
      end
c
c***********************************************************************
c* HSortAR -- Perform a dual index heapsort on a character string array.
c& jm
c: sorting
c+
      subroutine hsortar(n, array, second, indx)
c
      implicit none
      integer n, indx(n)
      character*(*) array(n)
      real second(n)
c
c  HSORTAR performs an index based heapsort on a character string array.
c  If there are matching elements in the primary (character) array, the
c  corresponding secondary (real) array elements are used to resolve the
c  ambiguity.  The number of elements in the array and the character and
c  real arrays are left unchanged.  The size of the indx array should be
c  at least as large as the size of the character and real arrays.
c
c     Input:
c       n        The number of elements in the character array.
c       array    The character array on which to base the primary sort.
c       second   The real array on which to base the secondary sort.
c
c     Output:
c       indx     Sorted integer index array such that array(indx(1)) is
c                the smallest and array(indx(n)) is the largest string.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer i, j, k, L, ir, indxt
      character*132 q
      real qr
c
c External functions.
c
      integer len1
c
c End declarations.
c
      do 11 j = 1, n
          indx(j) = j
   11 continue
      if (n .eq. 1) return
      L = (n / 2) + 1
      ir = n
   10 continue
      if (L .gt. 1)then
          L = L - 1
          indxt = indx(L)
          k = len1(array(indxt))
          q = array(indxt)(1:k)
          qr = second(indxt)
      else
          indxt = indx(ir)
          k = len1(array(indxt))
          q = array(indxt)(1:k)
          qr = second(indxt)
          indx(ir) = indx(1)
          ir = ir - 1
          if (ir .eq. 1) then
            indx(1) = indxt
            return
          endif
      endif
      i = L
      j = L + L
   20 continue
      if (j .le. ir) then
          if (j .lt. ir) then
            if (array(indx(j)) .lt. array(indx(j+1))) then
              j = j + 1
            elseif (array(indx(j)) .eq. array(indx(j+1))) then
              if (second(indx(j)) .lt. second(indx(j+1))) j = j + 1
            endif
          endif
          if (q .lt. array(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j+j
          elseif (q .eq. array(indx(j))) then
            if (qr .lt. second(indx(j))) then
              indx(i) = indx(j)
              i = j
              j = j+j
            else
              j = ir+1
            endif
          else
            j = ir+1
          endif
          go to 20
      endif
      indx(i) = indxt
      go to 10
      end
c
c***********************************************************************
c* HSortAD -- Perform a dual index heapsort on a character string array.
c& jm
c: sorting
c+
      subroutine hsortad(n, array, second, indx)
c
      implicit none
      integer n, indx(n)
      character*(*) array(n)
      double precision second(n)
c
c  HSORTAD performs an index based heapsort on a character string array.
c  If there are matching elements in the primary (character) array, the
c  corresponding secondary (double precision) array elements are used to
c  resolve the ambiguity.  The number of elements in the array and the
c  character and double precision arrays are left unchanged.  The size
c  of the indx array should be at least as large as the size of the
c  character and double precision arrays.
c
c     Input:
c       n        The number of elements in the character array.
c       array    The character array on which to base the primary sort.
c       second   The double precision array used for the secondary sort.
c
c     Output:
c       indx     Sorted integer index array such that array(indx(1)) is
c                the smallest and array(indx(n)) is the largest string.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer i, j, k, L, ir, indxt
      character*132 q
      double precision qr
c
c External functions.
c
      integer len1
c
c End declarations.
c
      do 11 j = 1, n
          indx(j) = j
   11 continue
      if (n .eq. 1) return
      L = (n / 2) + 1
      ir = n
   10 continue
      if (L .gt. 1)then
          L = L - 1
          indxt = indx(L)
          k = len1(array(indxt))
          q = array(indxt)(1:k)
          qr = second(indxt)
      else
          indxt = indx(ir)
          k = len1(array(indxt))
          q = array(indxt)(1:k)
          qr = second(indxt)
          indx(ir) = indx(1)
          ir = ir - 1
          if (ir .eq. 1) then
            indx(1) = indxt
            return
          endif
      endif
      i = L
      j = L + L
   20 continue
      if (j .le. ir) then
          if (j .lt. ir) then
            if (array(indx(j)) .lt. array(indx(j+1))) then
              j = j + 1
            elseif (array(indx(j)) .eq. array(indx(j+1))) then
              if (second(indx(j)) .lt. second(indx(j+1))) j = j + 1
            endif
          endif
          if (q .lt. array(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j+j
          elseif (q .eq. array(indx(j))) then
            if (qr .lt. second(indx(j))) then
              indx(i) = indx(j)
              i = j
              j = j+j
            else
              j = ir+1
            endif
          else
            j = ir+1
          endif
          go to 20
      endif
      indx(i) = indxt
      go to 10
      end
c
c***********************************************************************
c* HSortRR -- Perform a dual index heapsort on a real array.
c& jm
c: sorting
c+
      subroutine hsortrr(n, array, second, indx)
c
      implicit none
      integer n, indx(n)
      real array(n), second(n)
c
c  HSORTRR performs an index based heapsort on a real array.
c  If there are matching elements in the primary (real) array, the
c  corresponding secondary (real) array elements are used to resolve the
c  ambiguity.  The number of elements in the array and the two real
c  arrays are left unchanged.  The size of the indx array should be
c  at least as large as the size of the two real arrays.
c
c     Input:
c       n        The number of elements in the array.
c       array    The real array on which to base the primary sort.
c       second   The real array on which to base the secondary sort.
c
c     Output:
c       indx     Sorted integer index array such that array(indx(1)) is
c                the smallest and array(indx(n)) is the largest element.
c--
c-----------------------------------------------------------------------
c
c Internal variables.
c
      integer i, j, L, ir, indxt
      real q, q2
c
c End declarations.
c
      do 11 j = 1, n
          indx(j) = j
   11 continue
      if (n .eq. 1) return
      L = (n / 2) + 1
      ir = n
   10 continue
      if (L .gt. 1)then
          L = L - 1
          indxt = indx(L)
          q = array(indxt)
          q2 = second(indxt)
      else
          indxt = indx(ir)
          q = array(indxt)
          q2 = second(indxt)
          indx(ir) = indx(1)
          ir = ir - 1
          if (ir .eq. 1) then
            indx(1) = indxt
            return
          endif
      endif
      i = L
      j = L + L
   20 if (j .le. ir) then
          if (j .lt. ir) then
            if (array(indx(j)) .lt. array(indx(j+1))) then
              j = j + 1
            elseif (array(indx(j)) .eq. array(indx(j+1))) then
              if (second(indx(j)) .lt. second(indx(j+1))) j = j + 1
            endif
          endif
          if (q .lt. array(indx(j))) then
            indx(i) = indx(j)
            i = j
            j = j + j
          elseif (q .eq. array(indx(j))) then
            if (q2 .lt. second(indx(j))) then
              indx(i) = indx(j)
              i = j
              j = j + j
            else
              j = ir + 1
            endif
          else
            j = ir + 1
          endif
          go to 20
      endif
      indx(i) = indxt
      goto 10
      end
