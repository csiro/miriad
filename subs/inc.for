c************************************************************************
c  History:
c    rjs  xxfeb93 Pinched from reorder.for and developed further.
c    rjs   9mar93 Added IncOff
c************************************************************************
c* IncIni -- Initialise an incrementer.
c& rjs
c: miscellaneous
c+
	subroutine IncIni(n,size,dims)
c
	implicit none
	integer n,dims(n),size(n)
c
c  This initialises the "dims" array, which is to be used as an incrementer
c  through an N dimensional grid. The grid coordinates are assumed to
c  vary from 1 to M (i.e. FORTRAN convention).
c
c  Input:
c    n		Dimensionality of the grid.
c    size	Size of each axis of the grid, size(i) gives the number
c		of grid elements along the i'th dimension.
c  Output:
c    dims	Ready for use by the incrementer.
c--
c------------------------------------------------------------------------
	dims(1) = 0
	end
c************************************************************************
c* Inc2More -- Increment to the next plane of interest.
c& rjs
c: miscellaneous
c+
	logical function Inc2More(n,size,dims)
c
	implicit none
	integer n,dims(n),size(n)
c
c  Increment to the next plane.
c
c  Input:
c    n		Dimensionality of the grid.
c    size	Size of the grid in each dimension.
c  Input/Output:
c    dims	On output, the current grid coordinate of interest.
c  Output:
c    Inc2More	Set the false if there are no more grid coordinates to visit.
c--
c------------------------------------------------------------------------
	logical incmore
	integer i,nd
c
	if(dims(1).eq.0)then
	  do i=1,n
	    dims(i) = 1
	  enddo
	  Inc2More = .true.
	else
	  incmore = .true.
	  nd = 2
	  dowhile(incmore.and.nd.le.n)
	    dims(nd) = dims(nd) + 1
	    if(dims(nd).gt.size(nd))then
	      dims(nd) = 1
	      nd = nd + 1
	    else
	      incmore = .false.
	    endif
	  enddo
	  Inc2More = .not.incmore
	endif
c
	end
c************************************************************************
c* Inc3More -- Increment to the next plane of interest.
c& rjs
c: miscellaneous
c+
	logical function Inc3More(n,size,dims)
c
	implicit none
	integer n,dims(n),size(n)
c
c  Increment to the next cube.
c
c  Input:
c    n		Dimensionality of the grid.
c    size	Size of the grid in each dimension.
c  Input/Output:
c    dims	On output, the current grid coordinate of interest.
c  Output:
c    Inc3More	Set the false if there are no more grid coordinates to visit.
c--
c------------------------------------------------------------------------
	logical incmore
	integer i,nd
c
	if(dims(1).eq.0)then
	  do i=1,n
	    dims(i) = 1
	  enddo
	  Inc3More = .true.
	else
	  incmore = .true.
	  nd = 3
	  dowhile(incmore.and.nd.le.n)
	    dims(nd) = dims(nd) + 1
	    if(dims(nd).gt.size(nd))then
	      dims(nd) = 1
	      nd = nd + 1
	    else
	      incmore = .false.
	    endif
	  enddo
	  Inc3More = .not.incmore
	endif
c
	end
c************************************************************************
c* IncnMore -- Increment to the next grid coordinate of interest.
c& rjs
c: miscellaneous
c+
	logical function IncnMore(n,ninc,size,dims)
c
	implicit none
	integer n,dims(n),size(n),ninc
c
c  Increment to the next grid coordinate of interest. It increments
c  along the ninc'th dimension
c
c  Input:
c    n		Dimensionality of the grid.
c    size	Size of the grid in each dimension.
c    ninc	The dimension to increment along.
c		Seeting it equal to 2 gives the same behaviour as Inc2More.
c		Setting it equal to 3 gives the same behaviour as Inc3More.
c
c  Input/Output:
c    dims	On output, the current grid coordinate of interest.
c  Output:
c    IncnMore	Set the false if there are no more grid coordinates to visit.
c--
c------------------------------------------------------------------------
	logical incmore
	integer i,nd
c
	if(dims(1).eq.0)then
	  do i=1,n
	    dims(i) = 1
	  enddo
	  IncnMore = .true.
	else
	  incmore = .true.
	  nd = ninc
	  dowhile(incmore.and.nd.le.n)
	    dims(nd) = dims(nd) + 1
	    if(dims(nd).gt.size(nd))then
	      dims(nd) = 1
	      nd = nd + 1
	    else
	      incmore = .false.
	    endif
	  enddo
	  IncnMore = .not.incmore
	endif
c
	end
c************************************************************************
c* IncOff -- Offset one index by another.
c& rjs
c: miscellaneous
c+
	subroutine IncOff(n,size1,size2,out)
c
	implicit none
	integer n,size1(n),size2(n),out(n)
c
c  Offset one index by another.
c
c  Input:
c    n		Dimensionality of the grid.
c    size1,size2 The input indices, to be added.
c  Output:
c    out	On output, out(i) = size1(i) + size2(i)
c--
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  out(i) = size1(i) + size2(i) - 1
	enddo
	end

