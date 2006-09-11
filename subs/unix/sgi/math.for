c************************************************************************
c
c  This contains some mathematical functions which probably vectorise
c  on a vector machine.
c
c  History:
c    rjs    ???     - Created.
c    nebk   10sep89 - Changed spelling of isrcheq to isrchieq and isrchne 
c                     to isrchine in final assignment statements of 
c                     isrchieq and isrchine
c    rjs    19mar91   Added whenieq,whenine,isrchfeq,isrchfne
c
c************************************************************************
c*Ismin -- Return index of minimum value of a real array.
c:scilib
c+
	integer function ismin(n,data,step)
c
	implicit none
	integer n,step
	real data(*)
c
c  Find the index of the minimum value of a real array.
c
c  Input:
c    n		Number of elements to be searched.
c    data	The real array to be searched.
c    step	Skip distance between elements of the searched array.
c  Output:
c    ismin	Index of the minimum value in the array.
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,id,k
	real temp
c
	k = 1
	temp = data(k)
	if(step.eq.1)then
	  do i=2,n
	    if(data(i).lt.temp)then
	      k = i
	      temp = data(i)
	    endif
	  enddo
	else
	  id = 1
	  do i=1,n
	    if(data(id).lt.temp)then
	      k = i
	      temp = data(id)
	    endif
	    id = id + step
	  enddo
	endif
c
	ismin = k
	end
c************************************************************************
c*Ismax -- Return index of maximum value of a real array.
c:scilib
c+
	integer function ismax(n,data,step)
c
	implicit none
	integer n,step
	real data(*)
c
c  Find the index of the maximum value of a real array.
c
c  Input:
c    n		Number of elements to be searched.
c    data	The real array to be searched.
c    step	Skip distance between elements of the searched array.
c  Output:
c    ismax	Index of the maximum value in the array.
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c
c------------------------------------------------------------------------
	integer i,id,k
	real temp
c
	k = 1
	temp = data(k)
	if(step.eq.1)then
	  do i=2,n
	    if(data(i).gt.temp)then
	      k = i
	      temp = data(i)
	    endif
	  enddo
	else
	  id = 1
	  do i=1,n
	    if(data(id).gt.temp)then
	      k = i
	      temp = data(id)
	    endif
	    id = id + step
	  enddo
	endif
	ismax = k
	end
