c************************************************************************
	subroutine getarg(i,string)
c
	implicit none
	integer i
	character string*(*)
c
c  Get a command line argument.
c------------------------------------------------------------------------
	call igetarg(i,string,len(string))
	end
