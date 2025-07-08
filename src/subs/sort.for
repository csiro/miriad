c************************************************************************
c  History:
c    rjs  ??????? Original version of sorti.
c    rjs  25sep92 Added sortr.
c    mjs  24feb93 Added pgmr doc lines; no code mods.
c    rjs   2may94 Corrected bug in sortr, which cause it to bomb when
c		  sorting 1 element.
c************************************************************************
c*Sortr -- Sort a real array.
c&rjs
c:sorting
c+
	subroutine sortr(array,n)
c
	implicit none
	integer n
	real array(n)
c
c  Sorts an array, ARRAY, of length N into ascending order using a
c  Heapsort algorithm. ARRAY is replaced on output by its sorted
c  rearrangement.
c
c  Input:
c    n		Number of elements to be sorted.
c
c  Input/Output:
c    array	Input: Elements to be sorted.
c		Output: Sorted elements.
c--
c------------------------------------------------------------------------
      INTEGER L,IR,J,I
      REAL RRA
c
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=ARRAY(L)
        ELSE
          RRA=ARRAY(IR)
          ARRAY(IR)=ARRAY(1)
          IR=IR-1
          IF(IR.LE.1)THEN
            ARRAY(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRAY(J).LT.ARRAY(J+1))J=J+1
          ENDIF
          IF(RRA.LT.ARRAY(J))THEN
            ARRAY(I)=ARRAY(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        ARRAY(I)=RRA
      GO TO 10
      END
c************************************************************************
c*Sorti -- Sort an integer array.
c&rjs
c:sorting
c+
	subroutine sorti(array,n)
c
	implicit none
	integer n,array(n)
c
c  The main quick sort routine. Routine SPLIT does the choosing of
c  the pivot, and the partitioning around it. Routine INSERT does
c  a bubble sort variant when the number of elements to be sorted is
c  less than 8.
c
c  NOTE: The stack arrays START and END limit the sort to 2**nmax
c        items.
c
c  Input:
c    n		Number of elements to be sorted.
c
c  Input/Output:
c    array	Input: Elements to be sorted.
c		Output: Sorted elements.
c--
c-----------------------------------------------------------------
	integer nmax
	parameter(nmax=32)
	integer start(nmax),end(nmax)
	integer stack,k1,k2,i,temp
c
	stack = 1
	start(stack) = 1
	end(stack) = n
	do while(stack.ne.0)
	  k1 = start(stack)
	  k2 = end(stack)
	  stack = stack - 1
	  do while(k1.lt.k2)
	    if(k2-k1.lt.10)then
	      call insert(array(k1),k2-k1+1)
	      k1 = k2
	    else
	      call split(array,k1,k2,temp,i)
	      if(i-k1.lt.k2-i+1)then
	        stack = stack + 1
	        start(stack) = i+1
	        end(stack)   = k2
	        k2           = i-1
	      else
	        stack = stack + 1
	        start(stack) = k1
	        end(stack)   = i-1
	        k1           = i+1
	      endif
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine split(array,i,j,pivot,part)
c
	implicit none
	integer i,j,part
c
	integer array(*),pivot
c
c  The split routine for a quick sort.
c
c-------------------------------------------------------------------
	integer l,k,n(3),nn(6),index
	data nn/3,2,1,1,2,3/
c
c  Choose the median of the first, last and middle as the split point.
c
	n(1) = i
	n(2) = j
	k = (i+j)/2
	n(3) = k
	index=0
	if(array(i).gt.array(j)) index=1
	if(array(k).ge.array(i)) index=index+2
	if(array(j).ge.array(k)) index=index+4
	k = n(nn(index))
	pivot = array(k)
	if(k.ne.i)array(k) = array(i)
c
c  Ready to go!
c
	k = i
	l = j
c
  100	continue
  110	  if(k.eq.l)goto 200
	  if(array(l).lt.pivot)goto 120
	   l = l - 1
	  goto 110
c
  120	  array(k) = array(l)
	  k = k + 1
c
  130	  if(k.eq.l)goto 200
	  if(array(k).gt.pivot)goto 140
	    k = k + 1
	  goto 130
c
  140	  array(l) = array(k)
	  l = l - 1
c
	goto 100
c
  200	part = l
	array(part) = pivot
c
	end
c************************************************************************
	subroutine insert(array,n)
c
	implicit none
	integer n,array(n)
c
c  Perform an insertion sort.
c
c------------------------------------------------------------------------
	integer j,k,temp
c
	do j=2,n
	  temp = array(j)
	  k = j
	  do while( k.gt.1 .and. array(k-1).gt.temp )
	    array(k) = array(k-1)
	    k = k - 1
	  enddo
	  array(k) = temp
	enddo
c
	end
