c************************************************************************
c* PgHline -- Histogram line plot for pgplot.
c& mchw
c: plotting,uv-data
c+
	subroutine PgHline(npts,x,y,gapfac)
c
	implicit none
	integer npts
	real x(npts), y(npts), gapfac
c
c  Histogram style line plot of y-array versus x-array. Points are not
c  connected over gaps or reversals in the x-array.
c
c  Inputs:
c    npts	number of points
c    x		x-array to be plotted
c    y		y-array to be plotted
c    gapfac	factor to define a gap in x-array. E.g. 2= 2*(x(i+1)-x(i)) 
c
c--
c History
c    02nov89	mchw	original version
c    02apr94    nebk    add pgbbuf/pgebuf calls
c    24may94    mjs     reinserted Mel's docs
c-------------------------------------------------------------------------
	integer start,end,i
	logical gap,reverse
c
c  Look for gaps or reversals in x-array
c
        call pgbbuf
	start = 1
	end = 2
	do while(end.le.npts)
	 if(npts.gt.2 .and. end.lt.npts) then
	  gap = abs(x(end+1)-x(end)).gt.abs(gapfac*(x(end)-x(end-1)))
	  reverse = sign(1.,x(end+1)-x(end)).ne.sign(1.,x(end)-x(end-1))
	 else
	  gap = .true.
	 endif
c
c  Connect sequences of points between gaps and reversals
c
	 if(gap.or.reverse) then
	   call pgmove(x(start),y(start))
	   do i=start,end-1
	     call pgdraw (0.5*(x(i+1)+x(i)), y(i))
	     call pgdraw (0.5*(x(i+1)+x(i)), y(i+1))
	   enddo
	   call pgdraw(x(end),y(end))
	   start = end + 1
	   end = end + 2
	 else
	   end = end + 1
	 endif
 	enddo
        call pgebuf
	end
