C* PlotOne -- plot one or more set of x,y points on a single graph
c& lgm
C: plotting,PGPLOT,points
C+
      subroutine PlotOne(nvals,x,dim1,dim2,npls,yarr,xlab,ylab,glab,
     -                   device)
C
C Makes a x,y line plot of a set of points. Multiple x,y data sets
C are drawn on the same plot page and are distinguished by different
C colors (if available on plot device) or line styles. Routine does
C automatic plot scaling and all calls to PGPLOT.
C
C Arguments:
C
C NVALS (input, integer): number of points in each data set to plot
C X (input, real)       : values for x-axis
C dim1 (input, integer) : actual first dimension of YARR
C dim2 (input, integer) : actual second dimension of YARR
C npls (input, integer) : number of data sets to be plotted
C yarr (input, real)    : array of y values, (npls,nvals)
C xlab (input, char)    : label for x-axis
C ylab (input, char)    : label for y axis
C glab (input, char)    : label for the global plot
C device(input, char)   : plot device name
C----
C 12nov89 lgm inital writing
C 14dec89 pjt added a global label, hence variable top not needed
C 13mar93 mjs pgplot subr names have less than 7 chars.
c  9sep93 rjs Attempt to use colour, if the device supports it. Correct
c	      call to pgbeg.
C------------------------------------------------------------
	integer MAXVALS
	parameter(MAXVALS=10000)
	integer dim1,dim2,nvals,npls
        real yarr(dim1,dim2),x(dim2)
	character*(*) xlab,ylab,glab,device
	real xmin,xmax,ymin,ymax,y(MAXVALS)
	integer ip,iv,just,axis,iline,icolor
	integer ixmin, ixmax, iymin, iymax, amin1, amax1
c
c  Externals.
c
	integer ismin, ismax, pgbeg
c
c  Check.
c
	if(nvals.gt.MAXVALS)
     *	  call bug('f','Too many points to plot, in PLOTONE')
c           
c   find min and max of x and all y values to be plotted 
c
        ymin = 1.0e20 
        ymax =-1.0e20
        do 120 ip = 1,npls
           do 110 iv = 1,nvals
              y(iv) = yarr(ip,iv)
  110      continue
           iymin = ismin(nvals,y,1)
           iymax = ismax(nvals,y,1)
           ymin = amin1(ymin,y(iymin))
           ymax = amax1(ymax,y(iymax))
  120   continue
        ixmin = ismin(nvals,x,1)
        ixmax = ismax(nvals,x,1)
        xmin = x(ixmin)
	xmax = x(ixmax)
c
c  judge min and max a little to make sure that they are not the same
c  and that both are not zero
c
	if(xmax .le. xmin) then
	   xmax = xmin
	   xmin = xmin - 0.01 * abs(xmin)
	   xmax = xmax + 0.01 * abs(xmax)
        endif
	if(abs(xmax-xmin).le.1.0e-8.and.abs(xmin).le.1.0e-6) then
	   xmin = -1.0e-5
	   xmax =  1.0e-5
        endif
        if(ymax .le. ymin) then 
           ymax = ymin
           ymin = ymin - 0.01 * abs(ymin) 
	   ymax = ymax + 0.01 * abs(ymax) 
	endif            
	if(abs(ymax-ymin).le.1.0e-8.and.abs(ymin).le.1.0e-6) then 
           ymin = -1.0e-5 
	   ymax =  1.0e-5 
	endif
	xmin = xmin - 0.05*abs(xmax-xmin)
	xmax = xmax + 0.05*abs(xmax-xmin)
	ymin = ymin - 0.05*abs(ymax-ymin)
	ymax = ymax + 0.05*abs(ymax-ymin)
c
c   make basic border and scaling for plot
c
	
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Error opening graphics device')
	endif
	just = 0
	axis = 0
	call pgenv(xmin,xmax,ymin,ymax,just,axis)
	call pglab(xlab,ylab,glab)
	call pgslw(2)
c
c   plot the y values with each line in a different color or line
c   type if possible
c
        do 200 ip=1,npls
	   do 180 iv=1,nvals
	      y(iv)=yarr(ip,iv)
  180      continue
c
c  Try and set a color. If this does not work, set a line type.
c
	   icolor = mod(ip-1,15) + 1
	   call pgsci(icolor)
	   call pgqci(iline)
	   if(iline.ne.icolor)then
	      iline = mod(ip-1,5) + 1
	      call pgsls(iline)
           endif
	   call pgline(nvals,x,y)
  200   continue
	call pgend
c
	end
