c************************************************************************
	program varplt
	implicit none
c
c= VarPlt -- Plot and list variables.
c& rjs
c: plotting
c+
c	VarPlt is a MIRIAD task which plots and lists variables from a
c	uv file.
c@ vis
c	The name of the input data-set. No default.
c@ device
c	The PGPLOT plotting device to use. The default is no plot.
c@ log
c	The log to give a listing of the variables. The	default is no log.
c@ xaxis
c	Variable to plot on the X axis. Default is "time".
c@ yaxis
c	Variable to plot on the Y axis. No default.
c@ nxy
c	Number of plots in the x and y directions. The default varies.
c@ xrange
c	The min and max range along the x axis of the plots. The default
c	is to autoscale. Note that for "time" should be given in normal Miriad
c	time format (either absolute time or time-of-day).
c@ yrange
c	The min and max range along the y axis of the plots. The default
c	is to autoscale. Note that for "time" should be given in normal Miriad
c	time format (either absolute time or time-of-day).
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match is used.
c	  "dtime"    If the x-axis is "time", plot the time in fractions
c	             of a day. The default is to plot in hh:mm:ss.
c	  "compress" If the variable is a 2D array of values, average
c	             the variables along the second dimension.
c	  "overlay"  By default, when a variable takes on several values,
c	             each is plotted on a separate plot. The overlay
c	             option makes a single plot.
c	  "unwrap"   Unwrap the phases on the yaxis variable. Cannot unwrap
c	             an xaxis variable. By default phases are not unwrapped.
c--
c  History:
c    rjs   8aug91 Original version.
c    rjs  19aug91 Bug fixes.
c    rjs   3sep91 Increased buffer size. Corrected bug in writing
c		  multi-line sequences to the log file.
c    nebk 19feb92 Add file name to plot title
c    rjs  22apr92 Pgplot changes.
c    rjs  22jul92 Added more variables to its list.
c    nebk 29jul93 Add 'O' to PGTBOX option string
c    rjs   6jul93 Change longit to longitu, in the variables list.
c    pjt  20sep93 added optional options=unwrap (temporary bsrch -> binsrch)
c    pjt  27sep93 fixed up binsrch
c    rjs  12oct93 Tidy up a bit.
c    rjs  19nov93 Add a few more ATCA-specific variables. Fix bug with length
c		  of cdim2.
c    nebk 01jun94 DOc change
c    rjs  23sep94 Fixed bug determining default axis ranges when there were
c		  multiple X axes.
c    rjs  27apr95 Handle case where a variable does not appear in the first
c		  records.
c    rjs  13oct95 xrange and yrange handle times in normal Miriad format.
c    rjs  02feb01 Added options=equal.
c    rjs  16nov03 Added extra variables. pressmb, wind,winddir,axismax
c    rjs  10may05 Increase buffer size.
c  Bugs:
c    ?? Perfect?
c------------------------------------------------------------------------
	character version*(*)
	integer MAXPNTS
	parameter(MAXPNTS=1000000)
	parameter(version='VarPlt: version 1.1 10-May-05')
	logical doplot,dolog,dotime,dounwrap
	character vis*64,device*64,logfile*64,xaxis*16,yaxis*16
	character xtype*1,ytype*1,xunit*16,yunit*16,calday*24
	real xrange(2),yrange(2),xvals(MAXPNTS),yvals(MAXPNTS)
	double precision xscale,xoff,yscale,yoff
	double precision xtime1,xtime2,ytime1,ytime2
	integer nx,ny,tIn,xdim1,xdim2,ydim1,ydim2,n0,n1,maxpnt,npnts
	logical xaver,yaver,compress,dtime,overlay,more,equal
c
c  Externals.
c
	integer pgbeg
c
c  Get the user parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input data-set must be given')
	call keya('device',device,' ')
	doplot = device.ne.' '
	call keya('log',logfile,' ')
	dolog = logfile.ne.' '
	if(.not.(dolog.or.doplot))
     *	  call bug('f','One of the device and log must be given')
	call keyi('nxy',nx,0)
	call keyi('nxy',ny,nx)
	call keya('xaxis',xaxis,'time')
	if(xaxis.eq.' ')
     *	  call bug('f','Bad Xaxis value')
	call keya('yaxis',yaxis,' ')
	if(yaxis.eq.' ')
     *	  call bug('f','Yaxis variable name must be given')
	call GetOpt(compress,dtime,overlay,dounwrap,equal)
	if(xaxis.eq.'time')then
	  call keyt('xrange',xtime1,'time',0.d0)
	  call keyt('xrange',xtime2,'time',0.d0)
	else
	  call keyr('xrange',xrange(1),0.)
	  call keyr('xrange',xrange(2),xrange(1)-1.)
	endif
	if(yaxis.eq.'time')then
	  call keyt('yrange',ytime1,'time',0.d0)
	  call keyt('yrange',ytime2,'time',0.d0)
	else
	  call keyr('yrange',yrange(1),0.)
	  call keyr('yrange',yrange(2),yrange(1)-1.)
	endif
	call keyfin
c
c  Open up all the inputs.
c
	call uvopen(tIn,vis,'old')
	call VarChar(tIn,xaxis,xtype,xdim1,xdim2,xunit,xscale,xoff)
	call VarChar(tIn,yaxis,ytype,ydim1,ydim2,yunit,yscale,yoff)
	call uvrewind(tIn)
c
c  Time along the x axis is treated as a special case.
c
	dotime = xaxis.eq.'time'.and..not.dtime
	if(dotime)then
	  xunit = 'hh:mm:ss'
	  xscale = 24*3600
	endif
	if(xaxis.eq.'time')
     *	  call TimeFid(xtime1,xtime2,xscale,xoff,xrange)
c
	if(yaxis.eq.'time')
     *	  call TimeFid(ytime1,ytime2,yscale,yoff,yrange)
c
c  Determine the max number of visibs that can be read.
c
	if(xdim1.eq.1)then
	  xdim1 = xdim2
	  xdim2 = 1
	endif
	if(ydim1.eq.1)then
	  ydim1 = ydim2
	  ydim2 = 1
	endif
	xaver = xdim2.gt.1.and.compress
	yaver = ydim2.gt.1.and.compress
	n0 = xdim1*xdim2
	if(n0.gt.1.and..not.xaver) n0 = n0 + 1
	n1 = ydim1*ydim2
	if(n1.gt.1.and..not.yaver) n1 = n1 + 1
	maxpnt = MAXPNTS / max(n0,n1)
c
c  Read in the data.
c
	call DatRead(tIn,maxpnt,npnts,
     *		xaxis,xvals,xdim1*xdim2,xscale,xoff,xtype,
     *		yaxis,yvals,ydim1*ydim2,yscale,yoff,ytype)
	call uvclose(tIn)

c
c  Unwrap the phases
c
        if(dounwrap) call Unwrapit(npnts,yvals,ydim1,ydim2)
c
c  Average the data.
c
	if(xaver)then
	  call compact(xvals,xdim1,xdim2,npnts)
	  xdim2 = 1
	endif
	if(yaver)then
	  call compact(yvals,ydim1,ydim2,npnts)
	  ydim2 = 1
	endif
c
c  Plot the data, if needed.
c
	if(doplot)then
	  if(nx.le.0.or.ny.le.0)then
	    if((xdim1*xdim2.eq.1.and.ydim1*ydim2.eq.1).or.overlay)then
	      nx = 1
	      ny = 1
	    else
	      nx = 2
	      ny = 2
	    endif
	  endif
	  if(pgbeg(0,device,nx,ny).ne.1)then
	    call pgldev
	    call bug('f','Error opening graphics device')
	  endif
	  call pgsch(real(max(nx,ny))**0.4)
	  call Plotit(npnts,dotime,equal,overlay,vis,
     *	    xvals,xdim1,xdim2,xaxis,xrange,xunit,
     *	    yvals,ydim1,ydim2,yaxis,yrange,yunit)
	  call pgend
	endif
c
c  Write it out to the log file, if needed.
c
	if(dolog)then
	  call LogOpen(logfile,' ')
	  call LogWrite('# Variable Listing for '//vis,more)
	  if(xaxis.eq.'time')then
	    call JulDay(xoff,'H',calday)
	    call LogWrite('# Base time is '//calday,more)
	  endif
	  call Logit(npnts,dotime,
     *	    xvals,xdim1,xdim2,xaxis,xunit,
     *	    yvals,ydim1,ydim2,yaxis,yunit)
	  call LogClose
	endif
c
c  Bye bye.
c
	end
c************************************************************************
	subroutine TimeFid(time1,time2,scale,off,range)
c
	implicit none
	double precision time1,time2,scale,off
	real range(2)
c
c  Fiddle for times.
c------------------------------------------------------------------------
	double precision t1,t2
c
	if(time1.ne.time2)then
	  t1 = time1
	  t2 = time2
	  if(t1.lt.1.and.t2.lt.t1)t2 = t2 + 1
	  if(t1.gt.1)t1 = t1 - off
	  if(t2.gt.2)t2 = t2 - off
	  if(t1.gt.t2)call bug('f','Invalid time range')
	  range(1) = scale*t1
	  range(2) = scale*t2
	else
	  range(1) = 0
	  range(2) = 0
	endif
c
	end
c************************************************************************
	subroutine GetOpt(compress,dtime,overlay,dounwrap,equal)
c
	implicit none
	logical compress,dtime,overlay,dounwrap,equal
c
c  Get extra processing options.
c
c  Output:
c    compress	True if we are to compress the variables.
c    dtime	Show time as fractions of a day (xaxis only).
c    overlay	Do all the plots on one plot.
c    dounwrap   Unwrap phases
c    equal	Make axes equal scales.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=5)
	character opts(nopts)*8
	logical present(nopts)
	data opts/'compress','dtime   ','overlay ','unwrap  ',
     *		  'equal   '/
c
	call options('options',opts,present,nopts)
	compress = present(1)
	dtime    = present(2)
	overlay  = present(3)
	dounwrap = present(4)
	equal    = present(5)
	end
c************************************************************************
        subroutine compact(vals,n1,n2,n3)
c
	implicit none
	integer n1,n2,n3
	real vals(n1*n2*n3)
c------------------------------------------------------------------------
	integer i,j,k,k0,k1
	real sum
c
	k0 = 1
	k1 = 1
	do k=1,n3
	  do i=1,n1
	    sum = 0
	    do j=1,n2
	      sum = sum + vals(k1+(j-1)*n1)
	    enddo
	    vals(k0) = sum / n2
	    k0 = k0 + 1
	    k1 = k1 + 1
	  enddo
	  k1 = k1 - n1 + n1*n2
	enddo
c
        end
c************************************************************************
	subroutine Unwrapit(npnts,yvals,ydim1,ydim2)
c
	implicit none
	integer npnts,ydim1,ydim2
	real yvals(*)
c------------------------------------------------------------------------
	integer y1,y2,yoff,ky
	logical yext
c
c  Determine if we have to extract the data. Also determine offsets
c  of were we have to extract it to.
c
	yext = ydim1.ne.1.or.ydim2.ne.1
	if(yext)then
	  yoff = ydim1*ydim2*npnts+1
	else
	  yoff = 1
	endif
c
c  Do the unwrapping
c
	ky = 0
	do y2=1,ydim2
	  do y1=1,ydim1
	    ky = ky + 1
	    if(yext)then
	      call Extract(yvals(ky),ydim1*ydim2,npnts,yvals(yoff))
	    endif
            call unwrap(.TRUE.,npnts,yvals(yoff))
            if(yext)then
	      call Intract(yvals(ky),ydim1*ydim2,npnts,yvals(yoff))
            endif
	  enddo
	enddo
	end
c************************************************************************
      SUBROUTINE unwrap (dowrap, n, phs)
c
c     Unwrap phases
c       dowrap      logical, if to do it at all
c       n           number of points in array
c       phs         array of 'n' phases [in degrees] to unwrap in-place 
c
c------------------------------------------------------------------------
      LOGICAL dowrap
      INTEGER n
      REAL phs(n)
c
      REAL theta0
      INTEGER i
c------------------------------------------------------------------------
      IF (.not.dowrap) RETURN
      theta0 = phs(1)
      DO i = 2, n
         phs(i) = phs(i) - 360*NINT((phs(i)-theta0)/360.0)
         theta0 = 0.5*(phs(i) + theta0)
      ENDDO
c
      END
c************************************************************************
	subroutine Logit(npnts,dotime,
     *	    xvals,xdim1,xdim2,xaxis,xunit,
     *	    yvals,ydim1,ydim2,yaxis,yunit)
c
	implicit none
	integer npnts,xdim1,xdim2,ydim1,ydim2
	character xaxis*(*),yaxis*(*),xunit*(*),yunit*(*)
	logical dotime
	real xvals(xdim1*xdim2*npnts),yvals(ydim1*ydim2*npnts)
c------------------------------------------------------------------------
	character line*80,label*32
	integer xstep,ystep,xpnt,ypnt,j,length
	logical more
c
c  Externals.
c
	character itoaf*2
c
c  Some preamble lines to the log file.
c
	xstep = xdim1*xdim2
	call GetLabel(label,xaxis,xunit,xdim1,xdim1,xdim2,xdim2)
	if(xstep.eq.1)then
	  call LogWrite('# First column is '//label,more)
	else
	  call LogWrite('# First '//itoaf(xstep)//' columns are '
     *	    //label,more)
	endif
c
	ystep = ydim1*ydim2
	call GetLabel(label,yaxis,yunit,ydim1,ydim1,ydim2,ydim2)
	if(ystep.eq.1)then
	  call LogWrite('# Next column is '//label,more)
	else
	  call LogWrite('# Next '//itoaf(ystep)//' columns are '
     *	    //label,more)
	endif
c
c  Do the listing.
c
	xpnt = 1
	ypnt = 1
	do j=1,npnts
	  length = 0
	  call doLine(Line,length,xvals(xpnt),xstep,dotime,.true.)
	  call doLine(Line,length,yvals(ypnt),ystep,.false.,.false.)
	  if(Length.gt.0)call LogWrite(line(1:length),more)
	  xpnt = xpnt + xstep
	  ypnt = ypnt + ystep
	enddo
	end
c************************************************************************
	subroutine doLine(Line,length,vals,nvals,dotime,first)
c
	implicit none
	character Line*(*)
	integer length,nvals
	real vals(nvals)
	logical dotime,first
c------------------------------------------------------------------------
	integer isec,imin,ihr,iday,i
	logical dospace,more
c
	dospace = .not.first
	do i=1,nvals
	  if(length.eq.0.and.dospace)then
	    line(1:12) = ' '
	    length = 12
	  else if(length+12.gt.len(line))then
	    call LogWrite(line(1:length),more)
	    line(1:12) = ' '
	    length = 12
	    dospace = .true.
	  endif
	  if(dotime)then
	    isec = nint(vals(i))
	    iday = isec / (24*3600)
	    if(isec.lt.0)iday = iday - 1
	    isec = isec - 24*3600*iday
	    ihr  = isec/3600
	    isec = isec - 3600*ihr
	    imin = isec /60
	    isec = isec - 60*imin
	    write(line(length+1:length+12),'(i2,a,i2.2,a,i2.2,a,i2.2)')
     *		iday,' ',ihr,':',imin,':',isec
	  else
	    write(line(length+1:length+12),'(1pg12.5)')vals(i)
	  endif
	  length = length + 12
	enddo
	end
c************************************************************************
	subroutine Plotit(npnts,dotime,equal,overlay,vis,
     *	    xvals,xdim1,xdim2,xaxis,xrange,xunit,
     *	    yvals,ydim1,ydim2,yaxis,yrange,yunit)
c
	implicit none
	integer npnts,xdim1,xdim2,ydim1,ydim2
	real xrange(2),yrange(2)
	character xaxis*(*),yaxis*(*),xunit*(*),yunit*(*),vis*(*)
	logical dotime,overlay,equal
	real xvals(*),yvals(*)
c------------------------------------------------------------------------
	integer x1,x2,y1,y2,xoff,yoff,kx,ky
	logical xext,yext,xr,yr
	real xlo,xhi,ylo,yhi
c
c  Determine if we have to extract the data. Also determine offsets
c  of were we have to extract it to.
c
	xext = xdim1.ne.1.or.xdim2.ne.1
	if(xext)then
	  xoff = xdim1*xdim2*npnts+1
	else
	  xoff = 1
	endif
	yext = ydim1.ne.1.or.ydim2.ne.1
	if(yext)then
	  yoff = ydim1*ydim2*npnts+1
	else
	  yoff = 1
	endif
c
c  Determin global scale factors if needed.
c
	call doScale(xrange,overlay.or.xdim1*xdim2.eq.1,
     *	  xvals,xdim1*xdim2*npnts,xr,xlo,xhi)
	call doScale(yrange,overlay.or.ydim1*ydim2.eq.1,
     *	  yvals,ydim1*ydim2*npnts,yr,ylo,yhi)
c
c  Do the plots.
c
	if(overlay)call PGSet(dotime,equal,vis,
     *	  xaxis,xunit,xlo,xhi,1,1,1,1,yaxis,yunit,ylo,yhi,1,1,1,1)
c	  
	ky = 0
	do y2=1,ydim2
	  do y1=1,ydim1
	    ky = ky + 1
	    if(yext)then
	      call Extract(yvals(ky),ydim1*ydim2,npnts,yvals(yoff))
	      if(yr)call GetScale(yvals(yoff),npnts,ylo,yhi)
	    endif
	    kx = 0
	    do x2=1,xdim2
	      do x1=1,xdim1
		kx = kx + 1
		if(xext)then
		  call Extract(xvals(kx),xdim1*xdim2,npnts,xvals(xoff))
		  if(xr)call GetScale(xvals(xoff),npnts,xlo,xhi)
		endif
	        if(.not.overlay)call PGSet(dotime,equal,vis,
     *		  xaxis,xunit,xlo,xhi,x1,xdim1,x2,xdim2,
     *		  yaxis,yunit,ylo,yhi,y1,ydim1,y2,ydim2)
		call pgpt(npnts,xvals(xoff),yvals(yoff),1)
	      enddo
	    enddo
	  enddo
	enddo
	end
c************************************************************************
	subroutine Extract(in,n1,n2,out)
c
	implicit none
	integer n1,n2
	real in(n1,n2),out(n2)
c------------------------------------------------------------------------
	integer i
	do i=1,n2
	  out(i) = in(1,i)
	enddo
	end
c************************************************************************
	subroutine Intract(in,n1,n2,out)
c  inverse of extract  (:-)
c  stuff 'out' back into 'in'
	implicit none
	integer n1,n2
	real in(n1,n2),out(n2)
c------------------------------------------------------------------------
	integer i
	do i=1,n2
	  in(1,i) = out(i)
	enddo
	end
c************************************************************************
	subroutine PGSet(dotime,equal,vis,
     *                   xaxis,xunit,xlo,xhi,x1,xdim1,x2,xdim2,
     *		         yaxis,yunit,ylo,yhi,y1,ydim1,y2,ydim2)
c
	implicit none
	logical dotime,equal
	character xaxis*(*),yaxis*(*),xunit*(*),yunit*(*),vis*(*)
	integer x1,x2,xdim1,xdim2,y1,y2,ydim1,ydim2
	real xlo,xhi,ylo,yhi
c
c  Set up the plotting window.
c
c  Input:
c    Everything is input.
c------------------------------------------------------------------------
	character xlabel*32,ylabel*32
	call pgpage
	call pgvstd
	if(equal)then
	  call pgwnad(xlo,xhi,ylo,yhi)
	else
	  call pgswin(xlo,xhi,ylo,yhi)
	endif
	if(dotime)then
	  call pgtbox('BCNSTHZO',0.,0,'BCNST',0.,0)
	else
	  call pgtbox('BCNST',0.,0,'BCNST',0.,0)
	endif
	call GetLabel(xlabel,xaxis,xunit,x1,xdim1,x2,xdim2)
	call GetLabel(ylabel,yaxis,yunit,y1,ydim1,y2,ydim2)
	call pglab(xlabel,ylabel,vis)
	end
c************************************************************************
	subroutine GetLabel(xlabel,xaxis,xunit,x1,xdim1,x2,xdim2)
c
	implicit none
	character xlabel*(*),xaxis*(*),xunit*(*)
	integer x1,xdim1,x2,xdim2
c------------------------------------------------------------------------
	integer n,nsize(2),lints,lunits,laxis
	character ints*24
c
c  Externals.
c
	integer len1
c
	n = 0
	if(xdim1.gt.1)then
	  n = n + 1
	  nsize(n) = x1
	endif
	if(xdim2.gt.1)then
	  n = n + 1
	  nsize(n) = x2
	endif
c
	lunits = len1(xunit)
	laxis  = len1(xaxis)
	if(lunits.eq.0)then
	  if(n.eq.0)then
	    xlabel = xaxis(1:laxis)
	  else
	    call mitoaf(nsize,n,ints,lints)
	    xlabel = xaxis(1:laxis)//'('//ints(1:lints)//')'
	  endif
	else
	  if(n.eq.0)then
	    xlabel = xaxis(1:laxis)//' ('//xunit(1:lunits)//')'
	  else
	    call mitoaf(nsize,n,ints,lints)
	    xlabel = xaxis(1:laxis)//'('//ints(1:lints)//') ('//
     *	      xunit(1:lunits)//')'
	  endif
	endif
	end
c************************************************************************
	subroutine doScale(range,oneplot,vals,npnts,dor,loval,hival)
c
	implicit none
	integer npnts
	logical dor,oneplot
	real range(2),vals(npnts),loval,hival
c
c  Determine, if we can, what scale factor to use.
c
c------------------------------------------------------------------------
	if(range(1).lt.range(2))then
	  loval = range(1)
	  hival = range(2)
	  dor = .false.
	else if(oneplot)then
	  call GetScale(vals,npnts,loval,hival)
	  dor = .false.
	else
	  dor = .true.
	endif
	end
c**********************************************************************
	subroutine GetScale(vals,npnts,loval,hival)
c
	implicit none
	integer npnts
	real vals(npnts),loval,hival
c
c------------------------------------------------------------------------
	real delta,absmax
c
c  Externals.
c
	integer ismax,ismin
c
	loval = vals(ismin(npnts,vals,1))
	hival = vals(ismax(npnts,vals,1))
	delta = 0.05*(hival-loval)
	absmax = max(abs(hival),abs(loval))
	if(delta.le.1e-4*absmax) delta = 0.01*absmax
	if(delta.eq.0) delta = 1
	loval = loval - delta
	hival = hival + delta
	end
c************************************************************************
	subroutine DatRead(tIn,maxpnt,npnts,
     *		xaxis,xvals,xdim,xscale,xoff,xtype,
     *		yaxis,yvals,ydim,yscale,yoff,ytype)
c
	implicit none
	integer tIn,maxpnt,npnts,xdim,ydim
	character xtype*1,ytype*1,xaxis*(*),yaxis*(*)
	real xvals(xdim*maxpnt),yvals(ydim*maxpnt)
	double precision xscale,xoff,yscale,yoff
c
c------------------------------------------------------------------------
	integer MAXRUNS
	parameter(MAXRUNS=512)
	double precision xdrun(MAXRUNS),ydrun(MAXRUNS)
	integer xirun(MAXRUNS),yirun(MAXRUNS)
	real xrrun(MAXRUNS),yrrun(MAXRUNS)
	integer xpnt,ypnt,xdims,ydims,iostat,k
	logical xupd,yupd
	character xt*1,yt*1
c
c  Externals.
c
	integer uvscan
c
	if(max(xdim,ydim).gt.MAXRUNS)
     *	  call bug('f','Too many variables to hold in buffer')
	npnts = 0
	xpnt = 0
	ypnt = 0
c
c  Read the data.
c
	iostat = uvscan(tIn,' ')
	dowhile(iostat.eq.0.and.npnts.lt.maxpnt)
	  call uvprobvr(tIn,xaxis,xt,xdims,xupd)
	  call uvprobvr(tIn,yaxis,yt,ydims,yupd)
	  if((xupd.or.yupd).and.(xdims.eq.xdim.and.ydims.eq.ydim))then
	    if(max(xpnt+xdim,ypnt+ydim).gt.MAXRUNS)then
	      k = min(xpnt/xdim,maxpnt-npnts)
	      call TransF(k,npnts,
     *	        xtype,xirun,xrrun,xdrun,xdim,xvals,xscale,xoff,
     *	        ytype,yirun,yrrun,ydrun,ydim,yvals,yscale,yoff)
	      xpnt = 0
	      ypnt = 0
	    endif
c
	    if(xtype.eq.'i')then
	      call uvgetvri(tIn,xaxis,xirun(xpnt+1),xdim)
	    else if(xtype.eq.'r')then
	      call uvgetvrr(tIn,xaxis,xrrun(xpnt+1),xdim)
	    else if(xtype.eq.'d')then
	      call uvgetvrd(tIn,xaxis,xdrun(xpnt+1),xdim)
	    endif
c
	    if(ytype.eq.'i')then
	      call uvgetvri(tIn,yaxis,yirun(ypnt+1),ydim)
	    else if(ytype.eq.'r')then
	      call uvgetvrr(tIn,yaxis,yrrun(ypnt+1),ydim)
	    else if(ytype.eq.'d')then
	      call uvgetvrd(tIn,yaxis,ydrun(ypnt+1),ydim)
	    endif
c
	    xpnt = xpnt + xdim
	    ypnt = ypnt + ydim
c
	  endif
	  iostat = uvscan(tIn,' ')
	enddo
c
c  Check if all is ok.
c
	if(iostat.eq.0)call bug('w',
     *    'Buffer overflow -- some variables lost')
c
c  Flush out anything remaining.
c
	if(xpnt.gt.0)then
	  k = min(xpnt/xdim,maxpnt-npnts)
	  call TransF(k,npnts,
     *	      xtype,xirun,xrrun,xdrun,xdim,xvals,xscale,xoff,
     *	      ytype,yirun,yrrun,ydrun,ydim,yvals,yscale,yoff)
	endif
	end
c************************************************************************
	subroutine TransF(k,npnts,
     *	      xtype,xirun,xrrun,xdrun,xdim,xvals,xscale,xoff,
     *	      ytype,yirun,yrrun,ydrun,ydim,yvals,yscale,yoff)
c
	implicit none
	integer k,npnts,xdim,ydim
	character xtype*1,ytype*1
	integer xirun(k*xdim),yirun(k*ydim)
	real xrrun(k*xdim),yrrun(k*ydim)
	double precision xdrun(k*xdim),ydrun(k*ydim)
	real xvals(*),yvals(*)
	double precision xscale,xoff,yscale,yoff
c
c------------------------------------------------------------------------
	integer xpnt,ypnt
	xpnt = npnts*xdim+1
	ypnt = npnts*ydim+1
	if(xtype.eq.'i')then
	  call Cvtir(xirun,xvals(xpnt),k*xdim,xscale,xoff)
	else if(xtype.eq.'r')then
	  call Cvtrr(xrrun,xvals(xpnt),k*xdim,xscale,xoff)
	else if(xtype.eq.'d')then
	  call Cvtdr(xdrun,xvals(xpnt),k*xdim,xscale,xoff)
	endif
	if(ytype.eq.'i')then
	  call Cvtir(yirun,yvals(ypnt),k*ydim,yscale,yoff)
	else if(ytype.eq.'r')then
	  call Cvtrr(yrrun,yvals(ypnt),k*ydim,yscale,yoff)
	else if(ytype.eq.'d')then
	  call Cvtdr(ydrun,yvals(ypnt),k*ydim,yscale,yoff)
	endif
	npnts = npnts + k
	end
c************************************************************************
	subroutine Cvtir(in,out,n,scale,offset)
c
	integer n
	integer in(n)
	real out(n)
	double precision scale,offset
c------------------------------------------------------------------------
	integer i
	real rscal
	integer ioff
c
	ioff = nint(offset)
	rscal = scale
c
	do i=1,n
	  out(i) = rscal*(in(i) - ioff)
	enddo
	end
c************************************************************************
	subroutine Cvtrr(in,out,n,scale,offset)
c
	integer n
	real in(n)
	real out(n)
	double precision scale,offset
c------------------------------------------------------------------------
	integer i
	real rscal,roff
c
	roff = offset
	rscal = scale
c
	do i=1,n
	  out(i) = rscal*(in(i) - roff)
	enddo
	end
c************************************************************************
	subroutine Cvtdr(in,out,n,scale,offset)
c
	integer n
	double precision in(n)
	real out(n)
	double precision scale,offset
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  out(i) = scale*(in(i) - offset)
	enddo
	end
c************************************************************************
	subroutine VarChar(tno,name,type,ndim1,ndim2,unit,scale,offset)
c
	implicit none
	integer tno
	character name*(*),unit*(*),type*1
	integer ndim1,ndim2
	double precision scale,offset
c
c  Determine characteristics about a uv variable.
c
c  Input:
c    tno	Handle of the uv data set.
c    name	Name of the uv variable.
c  Output:
c    ndim1	Number of values along its first dimension.
c    ndim2	Number of values along its second dimension (if any).
c    unit	Gives the units of the variable.
c    scale,offset Conversion factors. User-val = scale*(raw-val - offset)
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,iostat
	logical update
c
c  Externals.
c
	integer uvscan
c
c  The following table gives the info about known variables. THIS MUST BE
c  IN ALPHABETIC ORDER. This table does not include variables which have
c  no units, or for which we would not add any extra info by putting them
c  in the table.
c
	integer nvars
	double precision rad2deg,rad2arc,rad2hr
	parameter(nvars=62)
	parameter(rad2deg=180.d0/pi,rad2arc=3600.d0*rad2deg)
	parameter(rad2hr=12.d0/pi)
c
	character names(nvars)*8,units(nvars)*8,tmp*16
	double precision scales(nvars)
	integer dim2s(nvars)
c
	integer NANTS,NSPECT,NTEMP,NTPOWER,NWIDE
	parameter(   NANTS=-1, NSPECT=-2, NTEMP=-3,  NTPOWER=-4,
     *	  NWIDE=-5)
	character cdim2s(5)*8
c
c  Externals.
c
	integer binsrcha
c
	data cdim2s/'nants   ','nspect  ','ntemp   ','ntpower ',
     *	  'nwide   '/
c
	data (names(i),units(i),dim2s(i),scales(i),i=1,17)/
     *	  'airtemp ','Celsius ',	1, 1.d0,
     *	  'antdiam ','meters  ',	1, 1.d0,
     *	  'antpos  ','nanosec ',	3, 1.d0,
     *	  'atten   ','dB      ',	1, 1.d0,
     *	  'axismax ','arcsec  ',    NANTS, 1.d0,
     *	  'axisrms ','arcsec  ',    NANTS, 1.d0,
     *	  'chi     ','degrees ',	1, rad2deg,
     *	  'coord   ','nanosec ',	1, 1.d0,
     *	  'corbw   ','GHz     ',	1, 1.d0,
     *	  'corfin  ','GHz     ',	1, 1.d0,
     *	  'ddec    ','arcsec  ',	1, rad2arc,
     *	  'dec     ','degrees ',	1, rad2deg,
     *	  'dewpoint','celsius ',	1, 1.d0,
     *	  'dra     ','arcsec  ',	1, rad2arc,
     *	  'epoch   ','years   ',	1, 1.d0,
     *	  'evector ','degrees ',	1, rad2deg,
     *	  'focus   ','volts   ',	1, 1.d0/
	data (names(i),units(i),dim2s(i),scales(i),i=18,34)/	
     *	  'freq    ','GHz     ',	1, 1.d0,
     *	  'freqif  ','GHz     ',	1, 1.d0,
     *	  'inttime ','seconds ',	1, 1.d0,
     *	  'jyperk  ','Jy/K    ',	1, 1.d0,
     *	  'latitud ','degrees ',	1, rad2deg,
     *	  'lo1     ','GHz     ',	1, 1.d0,
     *	  'lo2     ','GHz     ',	1, 1.d0,
     *	  'longitu ','degrees ',	1, rad2deg,
     *	  'lst     ','hours   ',	1, rad2hr,
     *	  'obsdec  ','degrees ',	1, rad2deg,
     *	  'obsra   ','hours   ',	1, rad2hr,
     *	  'pbfwhm  ','arcsec  ',	1, 1.d0,
     *	  'phaselo1','degrees ',	1, rad2deg,
     *	  'phaselo2','degrees ',	1, rad2deg,
     *	  'phasem1 ','degrees ',	1, rad2deg,
     *	  'plangle ','degrees ',	1, 1.d0,
     *	  'plmaj   ','arcsec  ',	1, 1.d0/
	data (names(i),units(i),dim2s(i),scales(i),i=35,50)/
     *	  'plmin   ','arcsec  ',	1, 1.d0,
     *	  'pltb    ','Kelvin  ',	1, 1.d0,
     *	  'precipmm','mm      ',	1, 1.d0,
     *	  'pressmb ','mB      ',        1, 1.d0,
     *	  'ra      ','hours   ',	1, rad2hr,
     *	  'relhumid','percent?',	1, 1.d0,
     *	  'restfreq','GHz     ',	1, 1.d0,
     *	  'sdf     ','GHz     ',	1, 1.d0,
     *	  'sfreq   ','GHz     ',	1, 1.d0,
     *	  'systemp ','Kelvin  ',   NSPECT, 1.d0,
     *	  'temp    ','celsius ',    NTEMP, 1.d0,
     *	  'time    ','hours   ',	1, 0.d0,
     *	  'tpower  ','volts   ',  NTPOWER, 1.d0,
     *	  'ut      ','hours   ',	1, rad2hr,
     *	  'veldop  ','km/sec  ',	1, 1.d0,
     *	  'vsource ','km/sec  ',	1, 1.d0/
	data (names(i),units(i),dim2s(i),scales(i),i=51,nvars)/
     *	  'wfreq   ','GHz     ',	1, 1.d0,
     *	  'wind    ','km/h    ',        1, 1.d0,
     *    'winddir ','degrees ',        1, 1.d0,
     *	  'windmph ','mph     ',	1, 1.d0,
     *	  'wsystemp','Kelvin  ',    NWIDE, 1.d0,
     *	  'wwidth  ','GHz     ',	1, 1.d0,
     *    'xsampler','percent ',   NSPECT, 1.d0,
     *	  'xtsys   ','Kelvin  ',   NSPECT, 1.d0,
     *    'xyamp   ','Jy      ',   NSPECT, 1.d0,
     *	  'xyphase ','degrees ',   NSPECT, rad2deg,
     *    'ysampler','percent ',   NSPECT, 1.d0,
     *	  'ytsys   ','Kelvin  ',   NSPECT, 1.d0/
c
	ndim1 = 0
	ndim2 = 0
	tmp = name
c
c  Wait until we have a valid record for this variable.
c
	ndim1 = 0
	iostat = 0
	dowhile(iostat.eq.0.and.ndim1.eq.0)
	  call uvprobvr(tno,name,type,ndim1,update)
	  if(type.eq.' ')call bug('f','Variable not in the file: '//tmp)
	  if(type.ne.'r'.and.type.ne.'d'.and.type.ne.'i')
     *	   call bug('f','Cannot plot variables of this datatype: '//tmp)
	  if(ndim1.eq.0)iostat = uvscan(tno,' ')
	enddo
c
	if(ndim1.le.0)
     *	  call bug('f','Needed length info not initialise for: '//tmp)
c
c  See if we know anything about this variable. If not, just use default info.
c
	i = binsrcha(name,names,nvars)
	if(i.eq.0)then
	  ndim2 = 1
	  unit = ' '
	  scale = 1.d0
	  offset = 0
c
c  We know something about this variable. Fill in the info.
c
	else
	  ndim2 = dim2s(i)
	  if(ndim2.lt.0)
     *	    call uvrdvri(tno,cdim2s(abs(dim2s(i))),ndim2,1)
	  if(ndim2.gt.ndim1)ndim2 = 1
	  if(mod(ndim1,ndim2).ne.0)
     *	      call bug('f','Inconsisten dimension info for var: '//tmp)
	  ndim1 = ndim1 / ndim2
	  unit = units(i)
	  scale = scales(i)
	  offset = 0
	  if(scale.eq.0)then
	    if(name.eq.'time')then
	      call uvrdvrd(tno,'time',offset,0.d0)
	      offset = int(offset - 0.5) + 0.5
	      scale = 24.d0
	    else
	      scale = 1.d0
	    endif
	  endif
	endif
c
	end
