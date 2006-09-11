c************************************************************************
	subroutine LagWt(wts,nwts,fac)
c
	implicit none
	integer nwts
	real wts(nwts),fac
c
c  Determine the best re-weighting scheme to apply. This involves
c  solving a minimax problem to determine the optimum filter coefficients.
c  A simple Remez exchange algorithm is used.
c------------------------------------------------------------------------
	integer MAXCOEFF
	parameter(MAXCOEFF=31)
	integer i,ncoeff,ngrid
	double precision coeff(MAXCOEFF+1),temp,scale
	integer indx(MAXCOEFF+1),zero(MAXCOEFF+2)
	integer pEval,pA,pGrid
	logical convrg
	character line*64
	include 'maxdim.h'
	include 'mem.h'
c
c  Allocate memory for the cosine lookup table, and initialise it.
c
	ncoeff = nWts/2 - 1
	ngrid = 20*ncoeff
	if(ncoeff.gt.MAXCOEFF)
     *	  call bug('f','Not enough buffer space, in Calcwt')
	call memAlloc(pEval,ngrid,'d')
	call memAlloc(pGrid,ncoeff*ngrid,'d')
	call memAlloc(pA,(ncoeff+1)*(ncoeff+1),'d')
c
c  Initialise the cosine table.
c
	call IniTab(memD(pGrid),ngrid,ncoeff,fac)
c
c  Set initial estimates of the ripple locations -- just equispaced.
c
	do i=1,ncoeff+1
	  indx(i) = ((ngrid-1)*(i-1))/ncoeff + 1
	enddo
c
c  Invoke the Remez exchange algorithm to solve the filter design
c  problem.
c
	convrg = .false.
	dowhile(.not.convrg)
	  call exchange(ncoeff,coeff,indx,memD(pGrid),ngrid,zero,
     *	    memD(pEval),memD(pA),convrg)
	enddo
c
c  Report on the spectral sidelobe level.
c
	write(line,10)'Maximum spectral sidelobe after reweighting is',
     *		      real(abs(coeff(ncoeff+1)))
  10	format(a,1pe8.1)
	call output(line)
c
c  Determine normalisation coefficient.
c
	temp = 1
	do i=1,ncoeff
	  temp = temp - coeff(i)
	enddo
c
c  Copy the coefficients to the output, putting them in the strange
c  order, and normalising.
c
	wts(1) = 1
	wts(ncoeff+2) = 1
	scale = 0.5d0/temp
c# ivdep
	do i=1,ncoeff
	  temp = scale*coeff(i)/(1.0d0-i/dble(ncoeff+1))
	  wts(i+1)      = temp
	  wts(nwts-i+1) = temp
	enddo
c
c  Free the memory now.
c
	call memFree(pEval,ngrid,'d')
	call memFree(pGrid,ncoeff*ngrid,'d')
	call memFree(pA,(ncoeff+1)*(ncoeff+1),'d')
c
	end
c************************************************************************
	subroutine exchange(n,coeff,indx,grid,m,zero,eval,a,convrg)
c
	implicit none
	integer m,n,indx(n+1),zero(n+2)
	double precision coeff(n+1),grid(m,n),eval(m),a(n+1,n+1)
	logical convrg
c
c  Perform a Remez exchange.
c------------------------------------------------------------------------
	integer i,j,itemp,themaxi,themaxj,ifail
	double precision themax,maxpk,s,temp
c
c  Set up the matrix to determine the coefficients.
c
	s = 1
	do j=1,n+1
	  itemp = indx(j)
	  do i=1,n
	    a(i,j) = grid(itemp,i)
	  enddo
	  a(n+1,j) = s
	  s = -s
	  coeff(j) = -1
	enddo
c
c  Solve for the coefficients.
c
	call dgefa(a,n+1,n+1,eval,ifail)
	if(ifail.ne.0)call bug('f','Matrix inversion failed')
	call dgesl(a,n+1,n+1,eval,coeff,1)
c
c  Evaluate the function at out grid.
c
	do i=1,m
	  eval(i) = 1
	enddo
c
	do j=1,n
	  do i=1,m
	    eval(i) = eval(i) + coeff(j)*grid(i,j)
	  enddo
	enddo
c
c  Find the n+1 zeros or end-points.
c
	zero(1) = 1
	do j=1,n
	  do i=indx(j),indx(j+1)-1
	    if(eval(i).eq.0.or.eval(i)*eval(i+1).lt.0)zero(j+1) = i
	  enddo
	enddo
	zero(n+2) = m
c
c  Locate the trial set of points.
c
	convrg = .true.
	themax = 0
	themaxi = 1
	themaxj = 1
	maxpk = 0
c
	do j=1,n+1
	  temp = eval(indx(j))
	  itemp = indx(j)
	  do i=zero(j),zero(j+1)
	    if(temp*eval(i).gt.temp*eval(itemp))then
	      itemp = i
	    else if(abs(eval(i)).gt.themax)then
	      themax = abs(eval(i))
	      themaxi = i
	      themaxj = j
	    endif
	  enddo
	  convrg = convrg.and.indx(j).eq.itemp
	  indx(j) = itemp
	  maxpk = max(maxpk,abs(eval(itemp)))
	enddo
c
c  Do we have the right set, or do we need to exchange.
c
	if(themax.gt.maxpk)then
	  convrg = .false.
	  if(themaxi.gt.indx(themaxj))then
	    if(themaxj.eq.n+1)then
	      do j=1,n
		indx(j) = indx(j+1)
	      enddo
	      indx(n) = themaxi
	    else
	      indx(themaxj+1) = themaxi
	    endif
	  else if(themaxi.lt.indx(themaxj))then
	    if(themaxj.eq.1)then
	      do j=n+1,2,-1
	        indx(j) = indx(j-1)
	      enddo
	      indx(1) = themaxi
	    else
	      indx(themaxj-1) = themaxi
	    endif
	  endif
	endif
c
	end
c************************************************************************
	subroutine IniTab(Grid,ngrid,ncoeff,fac)
c
	implicit none
	integer ncoeff,ngrid
	double precision Grid(ngrid,ncoeff)
	real fac
c
c  Initialise the cosine lookup table.
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision theta,t0,dt
	integer i,j
c
	t0 = 2*fac*DPI
	dt = (DPI-t0)/(ngrid-1)
	do i=1,ngrid
	  theta = t0 + (i-1)*dt
	  do j=1,ncoeff
	    grid(i,j) = cos(j*theta) - 1
	  enddo
	enddo
c
	end
