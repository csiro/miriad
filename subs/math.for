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
c*Isrchfeq -- Search real vector for target.
c:scilib
c+
	integer function isrchfeq(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c
c  Isrchfeq returns the first location in a real array that is equal
c  to the real target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value searched for in the array. If target is not
c		found, then the routine returns n+1.
c  Output:
c    isrchfeq	Index of the first occurrence of "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).eq.target)goto 200
	  j = j + inc
	enddo
 200	isrchfeq = i
	end
c************************************************************************
c*Isrchfne -- Search real vector for target.
c:scilib
c+
	integer function isrchfne(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c
c
c  Isrchfne returns the first location in a real array that is not equal
c  to the real target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value searched for in the array. If all elements
c		in the array are equal to "target", then the routine
c		returns n+1.
c  Output:
c    isrchfne	Index of the element not equal to "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).ne.target)goto 200
	  j = j + inc
	enddo
 200	isrchfne = i
	end
c************************************************************************
c*Isrchflt -- Search real vector for target.
c:scilib
c+
	integer function isrchflt(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c
c  Isrchflt returns the first location in a real array less than a target value.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value searched for in the array. If all elements
c		in the array are greater than or equal to "target", then
c		the routine returns n+1.
c  Output:
c    isrchflt	Index of the element less than "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).lt.target)goto 200
	  j = j + inc
	enddo
 200	isrchflt = i
	end
c************************************************************************
c*Isrchfle -- Search real vector for target.
c:scilib
c+
	integer function isrchfle(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c
c  Isrchfle returns the first location in a real array less than or equal to
c  a target value.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value searched for in the array. If all elements
c		in the array are greater than "target", then
c		the routine returns n+1.
c  Output:
c    isrchfle	Index of the element less than or equal to "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).le.target)goto 200
	  j = j + inc
	enddo
 200	isrchfle = i
	end
c************************************************************************
c*Isrchfgt -- Search real vector for target.
c:scilib
c+
	integer function isrchfgt(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c
c  Isrchfgt returns the first location in a real array less than a target value.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value searched for in the array. If all elements
c		in the array are greater than or equal to "target", then
c		the routine returns n+1.
c  Output:
c    isrchfgt	Index of the element less than "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).gt.target)goto 200
	  j = j + inc
	enddo
 200	isrchfgt = i
	end
c************************************************************************
c*Isrchfge -- Search real vector for target.
c:scilib
c+
	integer function isrchfge(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c
c  Isrchfge returns the first location in a real array that is greater
c  than or equal to the real target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value searched for in the array. If target is not
c		found, then the routine returns n+1.
c  Output:
c    isrchfge	Index of the first occurrence of greater or equal to "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).ge.target)goto 200
	  j = j + inc
	enddo
 200	isrchfge = i
	end
c************************************************************************
c*Isrchieq -- Search integer vector for target.
c:scilib
c+
	integer function isrchieq(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c
c  Isrchieq returns the first location in an integer array that is equal
c  to the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value searched for in the array. If target is not
c		found, then the routine returns n+1.
c  Output:
c    isrchieq	Index of thefirst occurrence of "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).eq.target)goto 200
	  j = j + inc
	enddo
 200	isrchieq = i
	end
c************************************************************************
c*Isrchine -- Search integer vector for target.
c:scilib
c+
	integer function isrchine(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c
c
c  Isrchine returns the first location in an integer array that is not equal
c  to the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value searched for in the array. If all elements
c		in the array are equal to "target", then the routine
c		returns n+1.
c  Output:
c    isrchine	Index of the element not equal to "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).ne.target)goto 200
	  j = j + inc
	enddo
 200	isrchine = i
	end
c************************************************************************
c*Isrchilt -- Search integer vector for target.
c:scilib
c+
	integer function isrchilt(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c
c  Isrchilt returns the first location in an integer array that is less 
c  than the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value searched for in the array. If target is not
c		found, then the routine returns n+1.
c  Output:
c    isrchilt	Index of the first occurrence of something less than "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).lt.target)goto 200
	  j = j + inc
	enddo
 200	isrchilt = i
	end
c************************************************************************
c*Isrchile -- Search integer vector for target.
c:scilib
c+
	integer function isrchile(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c
c  Isrchile returns the first location in an integer array that is less than
c  of equal to the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value searched for in the array. If target is not
c		found, then the routine returns n+1.
c  Output:
c    isrchieq	Index of the first occurrence of something less than or
c		equal to "target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).le.target)goto 200
	  j = j + inc
	enddo
 200	isrchile = i
	end
c************************************************************************
c*Isrchigt -- Search integer vector for target.
c:scilib
c+
	integer function isrchigt(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c
c  Isrchigt returns the first location in an integer array that is greater than
c  the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value searched for in the array. If target is not
c		found, then the routine returns n+1.
c  Output:
c    isrchigt	Index of the first occurrence of something greater than
c		"target".
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).gt.target)goto 200
	  j = j + inc
	enddo
 200	isrchigt = i
	end
c************************************************************************
c*Isrchige -- Search integer vector for target.
c:scilib
c+
	integer function isrchige(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c
c  Isrchige returns the first location in an integer array that is greater
c  or equal to the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value searched for in the array. If target is not
c		found, then the routine returns n+1.
c  Output:
c    isrchieq	Index of the first occurrence of something greater or equal
c		to the integer target.
c
c  Reference:
c  See page 4-59 to 4-64 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,j
	j = 1
	if(inc.lt.0) j = -inc*(n-1)+1
	do i=1,n
	  if(array(j).ge.target)goto 200
	  j = j + inc
	enddo
 200	isrchige = i
	end
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
c************************************************************************
c*Whenfeq -- Return locations equal to target.
c:scilib
c+
	subroutine whenfeq(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c
c  Whenfeq returns the all the locations in a real array equal to the real
c  target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		equal to the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	if(inc.eq.1)then
	  do i=1,n
	    if(array(i).eq.target)then
	      nval = nval + 1
	      index(nval) = i
	    endif
	  enddo
	else
	  do i=1,n
	    if(array(ina).eq.target)then
	      nval = nval + 1
	      index(nval) = i
	    endif
	    ina = ina + inc
	  enddo
	endif
	end
c************************************************************************
c*Whenfne -- Return locations not equal to target.
c:scilib
c+
	subroutine whenfne(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c
c  Whenfne returns the all the locations in a real array not equal to the real
c  target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		not equal to the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).ne.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenflt -- Return locations less than the target.
c:scilib
c+
	subroutine whenflt(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c
c  Whenflt returns the all the locations in a real array less than the real
c  target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		less than the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).lt.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenfle -- Return locations less than or equal to the target.
c:scilib
c+
	subroutine whenfle(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c
c  Whenfle returns the all the locations in a real array less than or equal
c  to the real target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		less than or equal to the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).le.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenfgt -- Return locations greater than the target.
c:scilib
c+
	subroutine whenfgt(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c
c  Whenfgt returns the all the locations in a real array greater than the real
c  target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		greater than the real target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).gt.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenfge -- Return locations greater than or equal to the target.
c:scilib
c+
	subroutine whenfge(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c
c  Whenfge returns the all the locations in a real array greater than or equal
c  to the real target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The real array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Real value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		greater than or equal to the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).ge.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenieq -- Return locations equal to the target.
c:scilib
c+
	subroutine whenieq(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c
c  Whenilt returns the all the locations in an integer array equal to the
c  integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		equal to the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).eq.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenine -- Return locations not equal to the target.
c:scilib
c+
	subroutine whenine(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c
c  Whenilt returns the all the locations in an integer array not equal to the
c  integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		less than the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).ne.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenilt -- Return locations less than the target.
c:scilib
c+
	subroutine whenilt(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c
c  Whenilt returns the all the locations in an integer array less than the
c  integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		less than the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).lt.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenile -- Return locations less than or equal to the integer target.
c:scilib
c+
	subroutine whenile(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c
c  Whenile returns the all the locations in an integer array less than or
c  equal to the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		less than or equal to the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).le.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenigt -- Return locations greater than the integer target.
c:scilib
c+
	subroutine whenigt(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c
c  Whenigt returns the all the locations in an integer array greater than
c  the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		greater than the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).gt.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
c************************************************************************
c*Whenige -- Return locations greater than or equal to the integer target.
c:scilib
c+
	subroutine whenige(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c
c  Whenige returns the all the locations in an integer array greater than or
c  equal to the integer target.
c
c  Inputs:
c    n		Number of elements to be searched.
c    array	The integer array to be searched.
c    inc	Skip distance between elements of the searched array.
c    target	Integer value to be searched for in the array.
c
c  Output:
c    index	The integer array containing the index of locations
c		greater than or equal to the target.
c    nval	Number of values put in the index array.
c
c  Reference:
c  See page 4-65 to 4-71 of the Cray "Library Reference Manual".
c--
c------------------------------------------------------------------------
	integer i,ina
c
	ina = 1
	nval = 0
	if(inc.lt.0)ina = -inc*(n-1) + 1
c
	do i=1,n
	  if(array(ina).ge.target)then
	    nval = nval + 1
	    index(nval) = i
	  endif
	  ina = ina + inc
	enddo
	end
