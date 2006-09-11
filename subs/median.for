c************************************************************************
c* Median -- Find the median of an array of data.
c& rjs
c: miscellaneous
c+
	subroutine median(x,n,xmed)
c
	implicit none
	integer n
	real x(n),xmed
c
c  Determine the median of an array of real numbers. This sorts the input
c  array. On output, the input data array is sorted.
c
c  Input:
c    n		Number of points.
c  Input/Output:
c    x		Data to find the median of. On output, it is sorted in
c		ascending order.
c  Output:
c    xmed	The median of the data.
c--
c------------------------------------------------------------------------
	integer i
c
	call sortr(x,n)
	i = n/2
	if(2*i.eq.n)then
	  xmed = 0.5*(x(i) + x(i+1))
	else
	  xmed = x(i+1)
	endif
c
	end
