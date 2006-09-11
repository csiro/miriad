c************************************************************************
c  Some subroutines to do in-situ matrix transposition.
c  Provides for double, real, integer and logical matrices.
c  The input 2-D matrix is passed in as a 1-D array.
c  A small work array is needed.
c
c  
c  TRANSD, TRANSR, TRANSI, TRANSL
c
c History:
c
c  15dec92  nebk   Original version
c
c***********************************************************************
c*transd -- Transpose double precision matrix
c:transpose,reorder
c& nebk
c+
      subroutine transd (a, m, n, move, iwrk, iok)
c
      implicit none
      integer m, n, iwrk, move(iwrk), iok
      double precision a(m*n)
c
c     In situ double precision matrix transpose
c
c     From Laflin and Brebner, CACM, Vol 13, No.5, May 1970
c
c  Input
c    a    2-D matrix passed in as 1-D
c    m,n  matrix dimensions
c    move work array
c    iwrk Size of move, should be (m+n)/2
c  Output
c    iok  0 => OK
c        -1 => iwrk <= 0
c--
c-----------------------------------------------------------------------
      integer i, j, i1, i2, j1, ncount, m2, ia, ib, mn,
     +k, kmi, imax
      double precision b
c-----------------------------------------------------------------------
      if (m.lt.2 .or. n.lt.2) then
c
c Nothing to do 
c
        iok = 0
        return
      end if
c
      if (iwrk.lt.1) then
c
c Invalid inputs
c
        iok = -1
        return
      end if
c
c Transpose matrix 
c
      mn = m * n
      if (m.eq.n) then
c
c Square matrix is easy
c
        do i = 1, n-1
          j1 = i + 1
          do j = j1, n
            i1 = i + (j-1)*n
            i2 = j + (i-1)*m
            b = a(i1)
            a(i1) = a(i2)
            a(i2) = b
          end do
        end do
        iok = 0 
      else
c
c Non-square is harder
c
        ncount = 2
        m2 = m -2
c
        do i = 1, iwrk
          move(i) = 0
        end do
c
        if (m2.ge.1) then
c
c Count number, ncount, of single points
c
          do ia = 1, m2
            ib = ia*(n-1)/(m-1)
            if (ia*(n-1).eq.ib*(m-1)) then
              ncount = ncount + 1
              i = ia*n + ib
              if (i.le.iwrk) move(i) = 1
            end if
          end do
        end if
c
c Set initial values for search
c
        k = mn - 1
        kmi = k - 1
        imax = mn
        i = 1
c
c At least one loop must be rearranged
c
        goto 30
c
c Search for loops to rearrange
c
20      imax = k - i
        i = i + 1
        kmi = k - i
        if (i.gt.imax) then
c
c Problem return
c
          iok = i
          return
        end if
c
        if (i.gt.iwrk) goto 21
        if (move(i).lt.1) goto 30
        goto 20
21      if (i.eq.m*i-k*(i/n)) goto 20
        i1 = i
22      i2 = m*i1 - k*(i1/n)
        if (i2.le.i .or. i2.ge.imax) goto 23
        i1 = i2
        goto 22
23      if (i2.ne.i) goto 20
c
c Rearrange elements of a loop
c
30      i1 = i
31      b = a(i1+1)
32      i2 = m*i1 - k*(i1/n)
        if (i1.le.iwrk) move(i1) = 2
        ncount = ncount + 1
        if (i2.eq.i .or. i2.ge.kmi) goto 35
34      a(i1+1) = a(i2+1)
        i1 = i2
        goto 32
35      if (imax.eq.kmi .or. i2.eq.i) goto 41
        imax = kmi
        goto 34
c
c Test for symmetric pair of loops
c
41      a(i1+1) = b
        if (ncount.ge.mn) then
c
c Finished
c
          iok = 0
          return
        end if
c
        if (i2.eq.imax .or. imax.eq.kmi)  goto 20
        imax = kmi
        i1 = imax
        goto 31
      end if
c
      end
c***********************************************************************
c*transr -- Transpose real matrix
c:transpose,reorder
c& nebk
c+
      subroutine transr (a, m, n, move, iwrk, iok)
c
      implicit none
      integer m, n, iwrk, move(iwrk), iok
      real a(m*n)
c
c     In situ real matrix transpose
c
c     From Laflin and Brebner, CACM, Vol 13, No.5, May 1970
c
c  Input
c    a    2-D matrix passed in as 1-D
c    m,n  matrix dimensions
c    move work array
c    iwrk Size of move, should be (m+n)/2
c  Output
c    iok  0 => OK
c        -1 => iwrk <= 0
c--
c-----------------------------------------------------------------------
      integer i, j, i1, i2, j1, ncount, m2, ia, ib, mn,
     +k, kmi, imax
      real b
c-----------------------------------------------------------------------
      if (m.lt.2 .or. n.lt.2) then
c
c Nothing to do 
c
        iok = 0
        return
      end if
c
      if (iwrk.lt.1) then
c
c Invalid inputs
c
        iok = -1
        return
      end if
c
c Transpose matrix 
c
      mn = m * n
      if (m.eq.n) then
c
c Square matrix is easy
c
        do i = 1, n-1
          j1 = i + 1
          do j = j1, n
            i1 = i + (j-1)*n
            i2 = j + (i-1)*m
            b = a(i1)
            a(i1) = a(i2)
            a(i2) = b
          end do
        end do
        iok = 0 
      else
c
c Non-square is harder
c
        ncount = 2
        m2 = m -2
c
        do i = 1, iwrk
          move(i) = 0
        end do
c
        if (m2.ge.1) then
c
c Count number, ncount, of single points
c
          do ia = 1, m2
            ib = ia*(n-1)/(m-1)
            if (ia*(n-1).eq.ib*(m-1)) then
              ncount = ncount + 1
              i = ia*n + ib
              if (i.le.iwrk) move(i) = 1
            end if
          end do
        end if
c
c Set initial values for search
c
        k = mn - 1
        kmi = k - 1
        imax = mn
        i = 1
c
c At least one loop must be rearranged
c
        goto 30
c
c Search for loops to rearrange
c
20      imax = k - i
        i = i + 1
        kmi = k - i
        if (i.gt.imax) then
c
c Problem return
c
          iok = i
          return
        end if
c
        if (i.gt.iwrk) goto 21
        if (move(i).lt.1) goto 30
        goto 20
21      if (i.eq.m*i-k*(i/n)) goto 20
        i1 = i
22      i2 = m*i1 - k*(i1/n)
        if (i2.le.i .or. i2.ge.imax) goto 23
        i1 = i2
        goto 22
23      if (i2.ne.i) goto 20
c
c Rearrange elements of a loop
c
30      i1 = i
31      b = a(i1+1)
32      i2 = m*i1 - k*(i1/n)
        if (i1.le.iwrk) move(i1) = 2
        ncount = ncount + 1
        if (i2.eq.i .or. i2.ge.kmi) goto 35
34      a(i1+1) = a(i2+1)
        i1 = i2
        goto 32
35      if (imax.eq.kmi .or. i2.eq.i) goto 41
        imax = kmi
        goto 34
c
c Test for symmetric pair of loops
c
41      a(i1+1) = b
        if (ncount.ge.mn) then
c
c Finished
c
          iok = 0
          return
        end if
c
        if (i2.eq.imax .or. imax.eq.kmi)  goto 20
        imax = kmi
        i1 = imax
        goto 31
      end if
c
      end
c************************************************************************
c*transi -- Transpose integer matrix
c:transpose,reorder
c& nebk
c+
      subroutine transi (a, m, n, move, iwrk, iok)
c
      implicit none
      integer m, n, iwrk, move(iwrk), iok
      integer a(m*n)
c
c     In situ integer matrix transpose
c
c     From Laflin and Brebner, CACM, Vol 13, No.5, May 1970
c
c  Input
c    a    2-D matrix passed in as 1-D
c    m,n  matrix dimensions
c    move work array
c    iwrk Size of move, should be (m+n)/2
c  Output
c    iok  0 => OK
c        -1 => iwrk <= 0
c--
c-----------------------------------------------------------------------
      integer i, j, i1, i2, j1, ncount, m2, ia, ib, mn,
     +k, kmi, imax
      integer b
c-----------------------------------------------------------------------
      if (m.lt.2 .or. n.lt.2) then
c
c Nothing to do 
c
        iok = 0
        return
      end if
c
      if (iwrk.lt.1) then
c
c Invalid inputs
c
        iok = -1
        return
      end if
c
c Transpose matrix 
c
      mn = m * n
      if (m.eq.n) then
c
c Square matrix is easy
c
        do i = 1, n-1
          j1 = i + 1
          do j = j1, n
            i1 = i + (j-1)*n
            i2 = j + (i-1)*m
            b = a(i1)
            a(i1) = a(i2)
            a(i2) = b
          end do
        end do
        iok = 0 
      else
c
c Non-square is harder
c
        ncount = 2
        m2 = m -2
c
        do i = 1, iwrk
          move(i) = 0
        end do
c
        if (m2.ge.1) then
c
c Count number, ncount, of single points
c
          do ia = 1, m2
            ib = ia*(n-1)/(m-1)
            if (ia*(n-1).eq.ib*(m-1)) then
              ncount = ncount + 1
              i = ia*n + ib
              if (i.le.iwrk) move(i) = 1
            end if
          end do
        end if
c
c Set initial values for search
c
        k = mn - 1
        kmi = k - 1
        imax = mn
        i = 1
c
c At least one loop must be rearranged
c
        goto 30
c
c Search for loops to rearrange
c
20      imax = k - i
        i = i + 1
        kmi = k - i
        if (i.gt.imax) then
c
c Problem return
c
          iok = i
          return
        end if
c
        if (i.gt.iwrk) goto 21
        if (move(i).lt.1) goto 30
        goto 20
21      if (i.eq.m*i-k*(i/n)) goto 20
        i1 = i
22      i2 = m*i1 - k*(i1/n)
        if (i2.le.i .or. i2.ge.imax) goto 23
        i1 = i2
        goto 22
23      if (i2.ne.i) goto 20
c
c Rearrange elements of a loop
c
30      i1 = i
31      b = a(i1+1)
32      i2 = m*i1 - k*(i1/n)
        if (i1.le.iwrk) move(i1) = 2
        ncount = ncount + 1
        if (i2.eq.i .or. i2.ge.kmi) goto 35
34      a(i1+1) = a(i2+1)
        i1 = i2
        goto 32
35      if (imax.eq.kmi .or. i2.eq.i) goto 41
        imax = kmi
        goto 34
c
c Test for symmetric pair of loops
c
41      a(i1+1) = b
        if (ncount.ge.mn) then
c
c Finished
c
          iok = 0
          return
        end if
c
        if (i2.eq.imax .or. imax.eq.kmi)  goto 20
        imax = kmi
        i1 = imax
        goto 31
      end if
c
      end
c************************************************************************
c*transl -- Transpose logical matrix
c:transpose,reorder
c& nebk
c+
      subroutine transl (a, m, n, move, iwrk, iok)
c
      implicit none
      integer m, n, iwrk, move(iwrk), iok
      logical a(m*n)
c
c     In situ logical matrix transpose
c
c     From Laflin and Brebner, CACM, Vol 13, No.5, May 1970
c
c  Input
c    a    2-D matrix passed in as 1-D
c    m,n  matrix dimensions
c    move work array
c    iwrk Size of move, should be (m+n)/2
c  Output
c    iok  0 => OK
c        -1 => iwrk <= 0
c--
c-----------------------------------------------------------------------
      integer i, j, i1, i2, j1, ncount, m2, ia, ib, mn,
     +k, kmi, imax
      logical b
c-----------------------------------------------------------------------
      if (m.lt.2 .or. n.lt.2) then
c
c Nothing to do 
c
        iok = 0
        return
      end if
c
      if (iwrk.lt.1) then
c
c Invalid inputs
c
        iok = -1
        return
      end if
c
c Transpose matrix 
c
      mn = m * n
      if (m.eq.n) then
c
c Square matrix is easy
c
        do i = 1, n-1
          j1 = i + 1
          do j = j1, n
            i1 = i + (j-1)*n
            i2 = j + (i-1)*m
            b = a(i1)
            a(i1) = a(i2)
            a(i2) = b
          end do
        end do
        iok = 0 
      else
c
c Non-square is harder
c
        ncount = 2
        m2 = m -2
c
        do i = 1, iwrk
          move(i) = 0
        end do
c
        if (m2.ge.1) then
c
c Count number, ncount, of single points
c
          do ia = 1, m2
            ib = ia*(n-1)/(m-1)
            if (ia*(n-1).eq.ib*(m-1)) then
              ncount = ncount + 1
              i = ia*n + ib
              if (i.le.iwrk) move(i) = 1
            end if
          end do
        end if
c
c Set initial values for search
c
        k = mn - 1
        kmi = k - 1
        imax = mn
        i = 1
c
c At least one loop must be rearranged
c
        goto 30
c
c Search for loops to rearrange
c
20      imax = k - i
        i = i + 1
        kmi = k - i
        if (i.gt.imax) then
c
c Problem return
c
          iok = i
          return
        end if
c
        if (i.gt.iwrk) goto 21
        if (move(i).lt.1) goto 30
        goto 20
21      if (i.eq.m*i-k*(i/n)) goto 20
        i1 = i
22      i2 = m*i1 - k*(i1/n)
        if (i2.le.i .or. i2.ge.imax) goto 23
        i1 = i2
        goto 22
23      if (i2.ne.i) goto 20
c
c Rearrange elements of a loop
c
30      i1 = i
31      b = a(i1+1)
32      i2 = m*i1 - k*(i1/n)
        if (i1.le.iwrk) move(i1) = 2
        ncount = ncount + 1
        if (i2.eq.i .or. i2.ge.kmi) goto 35
34      a(i1+1) = a(i2+1)
        i1 = i2
        goto 32
35      if (imax.eq.kmi .or. i2.eq.i) goto 41
        imax = kmi
        goto 34
c
c Test for symmetric pair of loops
c
41      a(i1+1) = b
        if (ncount.ge.mn) then
c
c Finished
c
          iok = 0
          return
        end if
c
        if (i2.eq.imax .or. imax.eq.kmi)  goto 20
        imax = kmi
        i1 = imax
        goto 31
      end if
c
      end

