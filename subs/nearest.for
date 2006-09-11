c************************************************************************
c*Nearest -- Calculate the nearest neighbour in a list of 2D coordinates
c& rjs
c:miscellaneous
c+
	subroutine nearest(x,y,indx,n,ip,mnmax)
c
	implicit none
	integer n
	real x(n),y(n),mnmax(n,5)
	integer indx(n),ip(n,5)
c
c  Determine nearest neighbours. The input arrays x,y, contain the
c  coordinates of points. On output, indx contains the index of the
c  closest other point.
c
c  Inputs:
c    n		Number of points.
c    x,y	The coordinates of the points.
c  Output:
c    indx	An integer array, giving the index of the nearest neighbour.
c		The nearest point to x(i),y(i) is x(indx(i)),y(indx(i)).
c  Scratch:
c    mnmax	Real scratch array of size 5*n.
c    ip		Integer scratch array of size 5*n.
c--
c  History:
c    rjs  01jun92 Original version.
c  Bugs:
c    * When N is small, it is faster to use the N**2 algorithm than the
c      quadtree approach.
c------------------------------------------------------------------------
	integer stack,tltree,trtree,bltree,brtree
	parameter(bltree=1,brtree=2,tltree=3,trtree=4,stack=5)
	integer xmin,xmax,ymin,ymax,radius
	parameter(xmin=1,xmax=2,ymin=3,ymax=4,radius=5)
c
	real xlo,xhi,ylo,yhi,dist,rad,x0,y0,xdist,ydist,xc,yc
	integer i,j,k,i0,pnt,quad,prev,npt,nextra
	logical possible,more
c
c  Determine the range of x and y.
c
	xlo = x(1)
	xhi = xlo
	ylo = y(1)
	yhi = ylo
	do i=2,n
	  xlo = min(xlo,x(i))
	  xhi = max(xhi,x(i))
	  ylo = min(ylo,y(i))
	  yhi = max(yhi,y(i))
	enddo
c
c  Build up a quadtree.
c
	ip(1,tltree) = 0
	ip(1,trtree) = 0
	ip(1,bltree) = 0
	ip(1,brtree) = 0
	mnmax(1,xmin) = xlo
	mnmax(1,xmax) = xhi
	mnmax(1,ymin) = ylo
	mnmax(1,ymax) = yhi
	do i=2,n
	  pnt = 1
	  dowhile(pnt.ne.0)
	    quad = 1
	    xc = 0.5*(mnmax(pnt,xmin)+mnmax(pnt,xmax))
	    yc = 0.5*(mnmax(pnt,ymin)+mnmax(pnt,ymax))
	    if(x(i).ge.xc)quad = quad + 1
	    if(y(i).ge.yc)quad = quad + 2
	    prev = pnt
	    pnt = ip(pnt,quad)
	  enddo
c
c  Found the leaf node.
c
	  ip(prev,quad) = i
	  ip(i,tltree) = 0
	  ip(i,trtree) = 0
	  ip(i,bltree) = 0
	  ip(i,brtree) = 0
c
c  Determine the area this node overlooks.
c
	  if(x(i).ge.xc)then
	    mnmax(i,xmin) = xc
	    mnmax(i,xmax) = mnmax(prev,xmax)
	  else
	    mnmax(i,xmin) = mnmax(prev,xmin)
	    mnmax(i,xmax) = xc
	  endif
c
	  if(y(i).ge.yc)then
	    mnmax(i,ymin) = yc
	    mnmax(i,ymax) = mnmax(prev,ymax)
	  else
	    mnmax(i,ymin) = mnmax(prev,ymin)
	    mnmax(i,ymax) = yc
	  endif
	enddo
c
c  Now that we have formed the quadtree, we can go through and search
c  for the nearest neighbours. For each point, start by initialising the
c  search stack.
c
	do j=1,n
	  x0 = x(j)
	  y0 = y(j)
	  pnt = 1
	  if(j.eq.1) pnt = 2
	  rad = (x(pnt)-x0)**2 + (y(pnt)-y0)**2
	  npt = 1
	  ip(npt,stack) = 1
	  mnmax(npt,radius) = 0	  
c
c  Is this point, and its subtrees, a possibility?
c
	  dowhile(npt.gt.0)
	    i = ip(npt,stack)
	    possible = mnmax(npt,radius).lt.rad
	    npt = npt - 1
	    nextra = 0
c
c  If its a possiblility, check this point.
c
	    if(possible)then
	      if(i.ne.j)then
	        dist = (x(i)-x0)**2 + (y(i)-y0)**2
	        if(dist.lt.rad)then
		  pnt = i
		  rad = dist
		endif
	      endif
c
c  Check the associated subtrees. This calculates the minimum distance
c  from the point of interest to each of the subtrees. If the min distance
c  is less than the current distance, then this tree is remembered for
c  later searching.
c
	      do k=1,4
		i0 = ip(i,k)
		if(i0.ne.0)then
		  if(mnmax(i0,xmin).le.x0.and.
     *		     x0.le.mnmax(i0,xmax))then
		    xdist = 0
		  else
		    xdist = min(abs(mnmax(i0,xmin)-x0),
     *			        abs(mnmax(i0,xmax)-x0))
		  endif
		  if(mnmax(i0,ymin).le.y0.and.
     *		     y0.le.mnmax(i0,ymax))then
		    ydist = 0
		  else
		    ydist = min(abs(mnmax(i0,ymin)-y0),
     *			        abs(mnmax(i0,ymax)-y0))
		  endif
		  dist = xdist*xdist + ydist*ydist
		  if(dist.lt.rad)then
		    nextra = nextra + 1
		    mnmax(npt+nextra,radius) = dist
		    ip(npt+nextra,stack) = i0
		  endif
	        endif
	      enddo
	    endif
c
c  We have the candidate trees to search. Sort the last few into
c  order so that we search the most likely one first.
c
	  if(nextra.gt.0)then
	    do i=2,nextra
	      i0 = i
	      dist = mnmax(i+npt,radius)
	      k = ip(i+npt,stack)
	      more = mnmax(i+npt-1,radius).lt.dist
	      dowhile(more)
		mnmax(i0+npt,radius) = mnmax(i0+npt-1,radius)
		ip(i0+npt,stack) = ip(i0+npt-1,stack)
		i0 = i0 - 1
		more = i0.gt.1
	        if(more)more = mnmax(i0+npt-1,radius).lt.dist
	      enddo
	      mnmax(i0+npt,radius) = dist
	      ip(i0+npt,stack) = k
	    enddo
	    npt = npt + nextra
	  endif
c
c  Loop the loop.
c
	  enddo
c
c  We have finished checking for this point. Return the result.
c
	  indx(j) = pnt
	enddo
	end
