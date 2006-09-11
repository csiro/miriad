c************************************************************************
c  Least squares fitting routines.
c  History:
c    rjs 21feb91   Added the LlsquIni,LLsquAcc and LLsquSol routines, for
c		   problems where the design matrix is too large to fix into
c		   memory (original version).
c    mjs 28feb91   Broke it out of lsqu.for to get 1 owner per sourcefile.
c************************************************************************
c* LlsquIni - Initalize the "large" linear least square routines.
c& rjs
c: least-squares,fitting
c+
	subroutine LlsquIni(x,B,n)
c
	implicit none
	integer n
	real B(n,n),x(n)
c
c  This routine is used with the LlsquAcc and LlsquSol routines, to solve
c  a least squares problem, where the number of "equations" is large, but the
c  number of unknowns is comparatively small.
c
c  The system of equations for which we are looking for a least square solution is:
c	f = Ax
c
c  This routine initalises, and the LlsquAcc routine is called with a set of
c  equations (and accumulates the needed statistics). Finally the LlsquSol
c  routine is called to finally solve the problem.
c
c  Inputs:
c    n		Number of unknowns in the system of equations.
c  Output:
c    B		Matrix used to store statistics. This is initialised.
c    x		This holds more statistics, and eventually the solution to
c		to the unknowns.
c------------------------------------------------------------------------
	integer i,j
	do j=1,n
	  do i=1,n
	    B(i,j) = 0
	  enddo
	enddo
c
	do i=1,n
	  x(i) = 0
	enddo
c
	end
************************************************************************
c* LlsquAcc - Accumulate info for the "large" linear least square routines.
c& rjs
c: least-squares,fitting
c+
	subroutine LlsquAcc(f,A,x,B,m,n)
c
	implicit none
	integer n,m
	real B(n,n),x(n),A(n,m),f(m)
c
c  This accumulates info for the "large" least squares routines. The routine
c  is called with a number of equations, that we eventually want to solve,
c  in a least squares sense. LlsquAcc accumulates the necessary info about
c  these equations, and then routines. LlsquAcc will generally be called
c  many times, so that effectively more and more equations are given to
c  contrain the solution. For more information on general usage, see the
c  preamble comments to LlsquIni.
c
c  The system of equations for which we are looking for a least square solution is:
c	f = Ax
c
c  Input:
c    f		The left hand side
c    A		The coefficients of the unknowns.
c    n		Number of unknowns.
c    m		Number of equations. This will typically be 1, but several
c		eqautions can be given on one call.
c  Input/Output:
c    B		The statistics used to solve the problem.
c    c		Eventually this will contain the solution.
c------------------------------------------------------------------------
	integer i,j,k
c
	do k=1,m
	  do j=1,n
	    do i=j,n
	      B(i,j) = B(i,j) + A(i,k)*A(j,k)
	    enddo
	  enddo
	  do i=1,n
	    x(i) = x(i) + A(i,k)*f(k)
	  enddo
	enddo
c
	end
c************************************************************************
c* LlsquSol - Solve the "large" linear least square routines.
c& rjs
c: least-squares,fitting
c+
	subroutine LlsquSol(x,B,n,ifail,pivot)
c
	implicit none
	integer n,ifail
	real B(n,n),x(n),pivot(n)
c
c  Input:
c    n		Number of unknowns.
c  Input/Output:
c    B		On input, this contains statistics accumulated by LlsquAcc.
c		These statistics are destroyed in solving the problem.
c    x		On input, this contains statistics accumulated by LlsquAcc.
c		On output, it contains the solutions to the lsqu problem.
c
c    ifail	Success status:
c		  0	Finished OK.
c		  1	Singular matrix encountered.
c  Scratch:
c    pivot
c--
c------------------------------------------------------------------------
	integer i,j
c
c  The LlsquAcc routine only accumulates half of the matrix (seeing it is a 
c  symmetrix matrix). Get the mirror part.
c
	do j=1,n
	  do i=1,j-1
	    B(i,j) = B(j,i)
	  enddo
	enddo
c
c  Form a LU decomposition.
c
	call sgefa(B,n,n,pivot,ifail)
c
c  If we did not find the matrix singular, finish the solution.
c
	if(ifail.ne.0)then
	  ifail = 1
	else
	  call sgesl(B,n,n,pivot,x,1)
	endif
	end
