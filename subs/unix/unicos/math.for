c************************************************************************
c Most of the routines
c	ISMIN
c	ISMAX
c	WHEN*
c	ISRCH*
c  should be in the SCILIB library.
c
c  We give a few here to rename some WHEN and ISRCH routines.
c************************************************************************
	subroutine whenfne(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c------------------------------------------------------------------------
	call whenne(n,array,inc,target,index,nval)
	end
c************************************************************************
	subroutine whenfeq(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	real array(*),target
c------------------------------------------------------------------------
	call wheneq(n,array,inc,target,index,nval)
	end
c************************************************************************
	subroutine whenine(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c------------------------------------------------------------------------
	call whenne(n,array,inc,target,index,nval)
	end
c************************************************************************
	subroutine whenieq(n,array,inc,target,index,nval)
c
	implicit none
	integer n,inc,nval
	integer index(*)
	integer array(*),target
c------------------------------------------------------------------------
	call wheneq(n,array,inc,target,index,nval)
	end
c************************************************************************
	integer function isrchieq(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c------------------------------------------------------------------------
	integer isrcheq
	isrchieq = isrcheq(n,array,inc,target)
	end
c************************************************************************
	integer function isrchine(n,array,inc,target)
c
	implicit none
	integer n,inc
	integer array(*),target
c------------------------------------------------------------------------
	integer isrchne
	isrchine = isrchne(n,array,inc,target)
	end
c************************************************************************
	integer function isrchfeq(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c------------------------------------------------------------------------
	integer isrcheq
	isrchfeq = isrcheq(n,array,inc,target)
	end
c************************************************************************
	integer function isrchfne(n,array,inc,target)
c
	implicit none
	integer n,inc
	real array(*),target
c------------------------------------------------------------------------
	integer isrchne
	isrchfne = isrchne(n,array,inc,target)
	end

