c************************************************************************
	program blflag
	implicit none
c
c= blflag -- Interactive flagging task.
c& rjs
c: uv analysis
c+
c	BLFLAG is an interactive flagger. BLFLAG plots the visibilities
c	(e.g. time vs amplitude) either a baseline at a time or all at once,
c	and allows you to flag discrepant points using the plotting cursor.
c	There are a few simple flagging commands, which you enter as
c	a single character at the keyboard. The following commands are
c	possible:
c	  Left-Button  Left mouse button flags the nearest visibility.
c	  Right-Button Right mouse button causes BLFLAG to precede to the
c	               next baseline.
c	  <CR>         Carriage-return gives help.
c	  ?            Help.
c	  a	       Flag nearest visibility (same as left mouse button).
c	  c            Clear the flagging for this baseline, and redraw plot.
c	  h            Give help (same as carriage return).
c	  p            Define a polygonal region, and flag visibilities
c	               within this region. You define the vertices of the
c	               polygon by moving the cursor and then hitting the
c	               left mouse button (or a). You finish defining the
c	               polygon by hitting the right mouse button (or x).
c	               You can delete vertices with the middle mouse
c	               button (or d).
c	  q            Abort completely. This does not apply flagging.
c	  r            Redraw plot.
c	  u            Unzoom.
c	  x            Move to next baseline (same as right mouse button).
c	  z            Zoom in. You follow this by clicking the mouse on the
c	               left and right limits to zoom.
c@ vis
c	Input visibility dataset to be flagged. No default.
c@ line
c	The normal Miriad linetype specification. BLFLAG will average
c	all channels together before displaying them, and any flagging
c	that you do will be applied to all selected channels. The default
c	is all channels.
c@ device
c	Normal PGPLOT plotting device. It must be interactive. No default.
c@ stokes
c	Normal Stokes/polarisation parameter selection. The default
c	is `ii' (i.e. Stokes-I assuming the source is unpolarised).
c	NOTE BLFLAG plots the average of all the selected Stokes/polarisation
c	quantities. Also it flags ALL quantities, regardless of whether they
c	were selected or not.
c@ select
c	Normal visibility data selection. Only selected data can be
c	flagged. The default is to select all data.
c@ axis
c	Two character strings, giving the X and Y axes of the plot. Possible
c	axis values are:
c	  time         (the default for the X axis)
c	  lst	       Local apparent sidereal time.
c	  uvdistance   sqrt(u**2+v**2)
c	  hangle       (hour angle)
c         channel      (implies nofqav)
c	  amplitude    (the default for the Y axis)
c	  phase
c	  real
c	  imaginary
c	  rms          Theoretical rms noise.
c@ xrange
c       Plot range in the x-direction
c         If axis = uvdistance            [kilo-lambda;   2 values]
c         If axis = time                  [dd,hh,mm,ss.s; 8 values]
c         If axis = amplitude, real, imag [natural units; 2 values]
c         If axis = phase                 [degrees;       2 values]
c         If axis = hangle                [hh,mm,ss.s;    6 values]
c         If axis = rms                   [flux units;    2 values]
c         If axis = lst                   [decimal hours; 2 values]
c         If axis = freq                  [GHz;           2 values]
c
c       For axis types other than 'time' or 'hangle', one or other of
c       the limits may be set with the other self-scaled by specifying
c       the lower limit as 'min' and the upper as 'max' (or simply
c       omitting it).  For example,
c
c         xrange=min,0
c
c       self-scales the lower limit while pinning the upper limit to
c       zero, whereas either of the following
c
c         xrange=0,max
c         xrange=0
c
c       set the lower limit to zero while self-scaling the upper limit.
c
c       Default is to self-scale both limits.
c@ yrange
c       Plot range for the y-axis as for the x-axis.  The default is to
c       self-scale. For amplitude type plots you can greatly reduce the
c       number of points to plot by using something like yrange=0.3 to 
c       cut out noise.
c@ options
c	Task enrichment parameters. Several can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  nobase  Normally BLFLAG plots a single baseline at a time.
c	          This option causes all baselines to be plotted on
c	          a single plot.
c	  selgen  Generate a file appropriate for selecting the bad
c	          data (via a "select" keyword). The output is a text
c	          file called "blflag.select".
c	  noapply Do not apply the flagging.
c         rms     When processing spectra, blflag normally plots the
c	          mean value of the spectra. Using options=rms causes
c	          it to plot the rms value instead.
c	  scalar  When processing spectra, blflag normally forms an
c	          average value by vector averaging. The "scalar" option
c	          causes it to generate the scalar average. This option
c	          should be used with significant caution.
c         nofqaver Do not average spectra - the resulting number of points
c                 may be too large to handle. Use select to break the data up
c                 in time ranges or use yrange to exclude noise.
c	The following options can be used to disable calibration.
c	  nocal   Do not apply antenna gain calibration.
c	  nopass  Do not apply bandpass correction.
c	  nopol   Do not apply polarisation leakage correction.
c--
c  History:
c    26jun96 rjs  Original version.
c    30jul96 rjs  Correct labelling of uvdist plots.
c    26feb97 rjs  Fix bug in the dimensioning of ltemp.
c     6may97 rjs  Better auto-range determination. Change zoom somewhat.
c		  Better doc and help.
c    12may97 rjs  Check that linetype is OK for flagging.
c    09nov98 rjs  Added "hangle" axis type.
c    12jan99 rjs  Doc change only.
c    09apr99 rjs  Add an extra error check only.
c    05oct99 rjs  Added options=scalar
c    23apr09 rjs  Increase buffer sizes.
c    02jun09 rjs  Flag by theoretical rms.
c    27jan10 mhw  Add option nofqav and increase buffers some more
C    04feb10 mhw  Add channel as an axis value and increase MAXEDIT
c    24feb10 mhw  Add xrange, yrange; use MAXBUF for 64bit machines
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	integer MILLION,MAXDAT,MAXPLT,MAXEDIT
	parameter(version='BlFlag: version 23-Apr-09')
	parameter(MILLION=1000000,MAXDAT=MAX(MAXBUF,5*MILLION),
     *            MAXPLT=4*MILLION,MAXEDIT=MILLION)
c
	logical present(MAXBASE),nobase,selgen,noapply,rms,scalar,
     *    nofqaver
	integer tno,i,j,k,length,npol
        real xmin, xmax, ymin, ymax
      	character xaxis*12,yaxis*12,title*32,device*64
	character val*16,uvflags*12
c
c  Store of all the data.
c
	integer ndat,bldat(MAXDAT),chdat(MAXDAT)
	double precision timedat(MAXDAT)
	real xdat(MAXDAT),ydat(MAXDAT)
c
c  Plot buffer.
c
	integer nplt,blplt(MAXPLT),chplt(MAXPLT)
	double precision timeplt(MAXPLT)
	real xplt(MAXPLT),yplt(MAXPLT)
	logical ltemp(MAXDAT)
c
c  The editting buffer.
c
	integer nedit,bledit(MAXEDIT),chedit(MAXEDIT)
	double precision timeedit(MAXEDIT)
c
c  Externals.
c
	character itoaf*10
	integer pgbeg,len1
	logical uvDatOpn
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('device',device,' ')
	if(device.eq.' ')call bug('f','A PGPLOT device must be given')
	call GetAxis(xaxis,yaxis)
	call GetOpt(nobase,selgen,noapply,rms,scalar,nofqaver,
     *    uvflags)
        if (xaxis.eq.'channel'.or.yaxis.eq.'channel') nofqaver=.true.
c
c Get axis ranges
c
        call getrng ('xrange', xaxis, xmin, xmax)
        call getrng ('yrange', yaxis, ymin, ymax)
	call uvDatInp('vis',uvflags)
	call keyfin
c
c  Set the default polarisation type if needed.
c
	call uvDatGti('npol',npol)
	if(npol.eq.0)call uvDatSet('stokes',0)
c
c  Open the input data.
c
	if(.not.uvDatOpn(tno))call bug('f','Error opening input')
c
c  Open the plot device.
c
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Unable to open PGPLOT device')
	endif
	call pgqinf('CURSOR',val,length)
	if(val.eq.'NO')call bug('f','PGPLOT device is not interactive')
	call pgask(.false.)
c
c  Get the data.
c
	call GetDat(tno,rms,scalar,nofqaver,xaxis,yaxis,xmin,xmax,
     *   ymin,ymax,present,MAXBASE, xdat,ydat,bldat,chdat,timedat,
     *   ndat,MAXDAT)
	if(ndat.eq.0)call bug('f','No points to flag')
c
c  Loop over the baselines.
c
	call output('Entering interactive mode ...')
	nedit = 0
	if(nobase)then
          call output('Processing '//itoaf(ndat)//' points')
	  call Edit(xdat,ydat,bldat,chdat,timedat,ltemp,ndat,
     *	    xaxis,yaxis,'All baselines',
     *	    timeedit,bledit,chedit,MAXEDIT,nedit)
	else
	  k = 0
	  do j=1,MAXANT
	    do i=1,j
	      k = k + 1
	      if(present(k))then
	        title = 'Baseline '//itoaf(i)
	        length = len1(title)
		title(length+1:) = '-'//itoaf(j)
	        call Extract(k,xdat,ydat,bldat,chdat,timedat,ndat,
     *		  xplt,yplt,blplt,chplt,timeplt,MAXPLT,nplt)
		if (nplt.gt.0) 
     *            call Edit(xplt,yplt,blplt,chplt,timeplt,ltemp,nplt,
     *		    xaxis,yaxis,title,timeedit,bledit,chedit,
     *              MAXEDIT,nedit)
	      endif
	    enddo
	  enddo
	endif
c
	call pgend
	call uvDatCls
c
c  Generate the "blflag.select" file, if needed.
c
	if(selgen)then
	  if(nedit.eq.0)then
	    call bug('w','No edit commands to write out!')
	  else
	    call doSelGen(timeedit,bledit,chedit,nedit)
	  endif
	endif
c
c  Apply the changes.
c
	if(nedit.gt.0.and..not.noapply)then
	  call output('Applying the flagging ...')
	  call uvDatRew
	  call uvDatSet('disable',0)
	  if(.not.uvDatOpn(tno))call bug('f','Error reopening input')
	  call FlagApp(tno,timeedit,bledit,chedit,nedit,version)
	  call uvDatCls
	endif
c
	end
c************************************************************************
	subroutine doSelGen(time,bl,chn,n)
c
	implicit none
	integer n
	double precision time(n)
	integer bl(n),chn(n)
c
c  Generate a file of select commands.
c------------------------------------------------------------------------
	double precision TTOL
	parameter(TTOL=1.d0/86400.d0)
	include 'maxdim.h'
	integer i,j,k,lu,iostat,length
	integer i1(MAXBASE),i2(MAXBASE)
	character line*80,time1*24,time2*24
        logical warn
c
c  Externals.
c
	integer len1
	character itoaf*4
c
c  Generate a table to allow translation from baseline number to
c  antenna pairs.
c
	k = 0
	do j=1,MAXANT
	  do i=1,j
	    k = k + 1
	    i1(k) = i
	    i2(k) = j
	  enddo
	enddo
c
	call txtopen(lu,'blflag.select','new',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output text file blflag.select')
	  call bugno('f',iostat)
	endif
c
        warn=.true.
	do i=1,n
          if (chn(i).eq.0) then
  	    call julday(time(i)-TTOL,'H',time1)
	    call julday(time(i)+TTOL,'H',time2)
	    line = 'ant('//itoaf(i1(bl(i)))
	    length = len1(line)
	    line(length+1:) = ')('//itoaf(i2(bl(i)))
	    length = len1(line)
	    line(length+1:) = '),time('//time1
	    length = len1(line)
	    line(length+1:) = ','//time2
	    length = len1(line)
	    line(length+1:) = ')'
	    length = length + 1
	    if(i.ne.n)then
	      line(length+1:length+3) = ',or'
	      length = length + 3
	    endif
	    call txtwrite(lu,line,length,iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error writing to text file blflag.select')
	      call bugno('f',iostat)
	    endif
          else
            if (warn) then
              call bug('w',
     *         'Omitting channel specific flags in text file')
              warn=.false.
            endif
          endif
	enddo
c
	call txtclose(lu)
	end
c************************************************************************
	subroutine FlagApp(tno,timeedit,bledit,chedit,nedit,version)
c
	implicit none
	integer tno,nedit
	integer bledit(nedit),chedit(nedit)
	double precision timeedit(nedit)
	character version*(*)
c
c  Apply flagging to the dataset.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	double precision TTOL
	parameter(TTOL=1.d0/86400.d0)
c
	integer nchan,bl,i1,i2,i,k
	double precision preamble(4),time
	complex data(MAXCHAN)
	logical flags(MAXCHAN),match,chflag
	integer nflag,ncorr
	character line*64
c
c  Externals.
c
	character itoaf*10
c
	nflag = 0
	ncorr = 0
c
	call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
          ncorr = ncorr + nchan
	  time = preamble(3)
	  call basant(preamble(4),i1,i2)
	  bl = (i2*(i2-1))/2 + i1
          chflag=.false.
c
c  Search for this integration.
c
  	  do i=1,nedit
	    match = bledit(i).eq.bl.and.
     *		       abs(timeedit(i)-time).le.TTOL
c
c  Flag bad channels if found.
c
	    if(match)then
              if (chedit(i).eq.0) then
                do k=1,nchan
	          if(flags(k))then
	  	    flags(k) = .false.
		    nflag = nflag + 1
                    chflag=.true.
	          endif
	        enddo
              else
                if(flags(chedit(i)))then
                  flags(chedit(i))=.false.
                  nflag = nflag +1
                  chflag=.true.
                endif
              endif
            endif
          enddo
	  if (chflag) call uvflgwr(tno,flags)
c
c  Go back for more.
c
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	enddo
c
c  Write some history.
c
	call hisOpen(tno,'append')
	line = 'BLFLAG: Miriad '//version
	call hisWrite(tno,line)
	call hisInput(tno,'BLFLAG')
	call hisWrite(tno,'BLFLAG: Number of correlations flagged: '//
     *				itoaf(nflag))
	call hisClose(tno)
c
c  Say how much we have done.
c
	call output('Total number of correlations:   '//itoaf(ncorr))
	call output('Number of correlations flagged: '//itoaf(nflag))
c
	end
c************************************************************************
	subroutine Edit(xplt,yplt,blplt,chplt,timeplt,flag,nplt,
     *		xaxis,yaxis,title,timeedit,bledit,chedit,MAXEDIT,nedit)
c
	implicit none
	integer nplt,MAXEDIT,nedit
	integer blplt(nplt),chplt(nplt),bledit(MAXEDIT),chedit(MAXEDIT)
	logical flag(nplt)
	double precision timeplt(nplt),timeedit(MAXEDIT)
	real xplt(nplt),yplt(nplt)
	character xaxis*(*),yaxis*(*),title*(*)
c------------------------------------------------------------------------
	character mode*1
	integer nedit0,i
	logical more
	real xv,yv,xs,ys,xmin,xmax
c
	nedit0 = nedit
	mode = 'c'
	more = .true.
	dowhile(more)
	  call lcase(mode)
	  if(mode.eq.'q')then
	    call pgend
	    call bug('f','Aborting at users request')
	  else if(mode.eq.'c')then
	    nedit = nedit0
	    xmin = 0
	    xmax = 0
	    do i=1,nplt
	      flag(i) = .true.
	    enddo
	    call Draw(xmin,xmax,xplt,yplt,flag,nplt,
     *			xaxis,yaxis,title,xs,ys)
	  else if(mode.eq.'r')then
	    call Draw(xmin,xmax,xplt,yplt,flag,nplt,
     *			xaxis,yaxis,title,xs,ys)
	  else if(mode.eq.'h'.or.mode.le.' '.or.mode.eq.'?')then
	    call output('-------------------------------------')
	    call output('Single key commands are')
	    call output(' Left-button  Delete nearest point')
	    call output(' Right-button Next baseline')
	    call output(' <CR>      Help')
	    call output(' ?         Help')
	    call output(' a         Delete nearest point')
	    call output(' c         Clear flagging of this baseline')
	    call output(' h         Help -- these messages')
	    call output(' p         Define and delete polygonal region')
	    call output(' q         Quit -- Abort completely')
	    call output(' r         Redraw')
	    call output(' u         Unzoom')
	    call output(' x         Next baseline')
	    call output(' z         Zoom in')
	    call output('-------------------------------------')
	  else if(mode.eq.'u')then
	    xmin = 0
	    xmax = 0
	    call Draw(xmin,xmax,xplt,yplt,flag,nplt,
     *			xaxis,yaxis,title,xs,ys)
	  else if(mode.eq.'x')then
	    more = .false.
	  else if(mode.eq.'z')then
	    call output('Click on left-hand edge of the zoomed region')
	    call pgcurs(xmin,yv,mode)
	    call output('Click on right-hand edge of the zoomed region')
	    call pgcurs(xmax,yv,mode)
	    call Draw(xmin,xmax,xplt,yplt,flag,nplt,
     *			xaxis,yaxis,title,xs,ys)
	  else if(mode.eq.'a')then
	    call Nearest(xv,yv,xs,ys,xplt,yplt,blplt,chplt,timeplt,
     *	      flag,nplt,bledit,chedit,timeedit,nedit,MAXEDIT)
	  else if(mode.eq.'p')then
	    call Region(xplt,yplt,blplt,chplt,timeplt,flag,nplt,
     *	      bledit,chedit,timeedit,nedit,MAXEDIT)
	  else
	    call bug('w','Unrecognised keystroke - use h for help')
	  endif
	  if(more)call pgcurs(xv,yv,mode)
	enddo
c
	end
c************************************************************************
	subroutine Region(xplt,yplt,blplt,chplt,timeplt,flag,nplt,
     *	      bledit,chedit,timeedit,nedit,MAXEDIT)
c
	implicit none
	integer nplt,nedit,MAXEDIT
	real xplt(nplt),yplt(nplt)
	logical flag(nplt)
	double precision timeplt(nplt),timeedit(MAXEDIT)
	integer blplt(nplt),chplt(nplt),bledit(MAXEDIT),chedit(MAXEDIT)
c------------------------------------------------------------------------
	integer MAXV
	parameter(MAXV=100)
	real xv(MAXV+1),yv(MAXV+1)
	integer nv,i,j
	logical within
c
	call output('Define a region - exit with x')
	call pgsci(3)
	nv = 0
	call pgolin(MAXV,nv,xv,yv,17)
	if(nv.lt.3)then
	  call bug('w','Too few vertices')
	else if(nv.gt.MAXV)then
	  call bug('w','Too many vertices for me!')
	else
	  call pgsfs(2)
	  call pgslw(2)
	  call pgpoly(nv,xv,yv)
	  call pgslw(1)
	  call pgsci(2)
c
	  xv(nv+1) = xv(1)
	  yv(nv+1) = yv(1)
c
c  Find all the points that are within the poly.
c
	  call pgbbuf
	  do i=1,nplt
	    if(flag(i))then
	      within = .false.
	      do j=1,nv
		if((xplt(i)-xv(j))*(xplt(i)-xv(j+1)).le.0.and.
     *		   abs(xplt(i)-xv(j))*(yv(j+1)-yv(j)).lt.
     *		   abs(xv(j+1)-xv(j))*(yplt(i)-yv(j)))
     *		   within = .not.within
	      enddo
	      if(within)then
		nedit = nedit + 1
		if(nedit.gt.MAXEDIT)
     *		  call bug('f','Too many editing ops')
		bledit(nedit)   = blplt(i)
                chedit(nedit)   = chplt(i)
		timeedit(nedit) = timeplt(i)
		flag(i) = .false.
		call pgpt(1,xplt(i),yplt(i),1)
	      endif
	    endif
	  enddo
	  call pgebuf
	endif
	end
c************************************************************************
	subroutine Draw(xmin,xmax,xplt,yplt,flag,nplt,
     *				xaxis,yaxis,title,xs,ys)
c
	implicit none
	integer nplt
	real xplt(nplt),yplt(nplt),xs,ys,xmin,xmax
	logical flag(nplt)
	character xaxis*(*),yaxis*(*),title*(*)
c------------------------------------------------------------------------
	real xlo,xhi,ylo,yhi
	integer i,n
	logical good
	character xtitle*32,ytitle*32,xflags*12,yflags*12
c
c  Determine the min and max values.
c
	call pgbbuf
	call SetUp(xplt,flag,nplt,xaxis,xlo,xhi,xtitle,xflags)
	if(xmin.lt.xmax)then
	  xlo = xmin
	  xhi = xmax
	endif
	call SetUp(yplt,flag,nplt,yaxis,ylo,yhi,ytitle,yflags)
c
	xs = 1/(xhi-xlo)**2
	ys = 1/(yhi-ylo)**2
c
c  Draw the plot.
c
	call pgsci(1)
	call pgpage
	if((xaxis.eq.'real'.or.xaxis.eq.'imaginary').and.
     *	   (yaxis.eq.'real'.or.yaxis.eq.'imaginary'))then
	  call pgvstd
	  call pgwnad(xlo,xhi,ylo,yhi)
	else
	  call pgvstd
	  call pgswin(xlo,xhi,ylo,yhi)
	endif
	call pgtbox(xflags,0,0.,yflags,0,0.)
	call pglab(xtitle,ytitle,title)
c
c  Plot all the good data.
c
	n = 1
	good = flag(1)
	do i=2,nplt
	  if(good.neqv.flag(i))then
	    if(good)then
	      call pgpt(i-n,xplt(n),yplt(n),1)
	    else
	      n = i
	    endif
	    good = flag(i)
	  endif
	enddo
	if(good)call pgpt(nplt-n+1,xplt(n),yplt(n),1)
c
c  Change the colour to red.
c
	call pgsci(2)
	call pgebuf
	end
c************************************************************************
	subroutine SetUp(plt,flag,nplt,axis,lo,hi,title,flags)
c
	integer nplt
	real plt(nplt),lo,hi
	logical flag(nplt)
	character axis*(*),title*(*),flags*(*)
c------------------------------------------------------------------------
	integer i
	logical first
	real x1,x2,delta,absmax
c
	first = .true.
	do i=1,nplt
	  if(flag(i))then
	    if(first)then
	      x1 = plt(i)
	      x2 = x1
	      first = .false.
	    else
	      x1 = min(x1,plt(i))
	      x2 = max(x2,plt(i))
	    endif
	  endif
	enddo
c
	delta = 0.05*(x2-x1)   
        absmax = max(abs(x2),abs(x1))
        if(delta.le.1e-4*absmax) delta = 0.01*absmax
        if(delta.eq.0) delta = 1
        lo = x1 - delta
        hi = x2 + delta
c
	if(axis.eq.'time'.or.axis.eq.'lst'.or.axis.eq.'hangle')then
	  flags = 'BCNSTHZ0' 
	else
	  flags = 'BCNST'
	endif
c
	if(axis.eq.'uvdistance')then
	  title = '(u\u2\d+v\u2\d)\u1/2\d (k\gl)'
	else if(axis.eq.'phase')then
	  title = 'Phase (degrees)'
	else if(axis.eq.'rms')then
	  title = 'Theoretical rms noise'
	else
	  title = axis
	  call ucase(title(1:1))
	endif
c
	end
c************************************************************************
	subroutine Nearest(xv,yv,xs,ys,
     *	      xplt,yplt,blplt,chplt,timeplt,flag,nplt,
     *	      bledit,chedit,timeedit,nedit,MAXEDIT)
c
	implicit none
	integer nplt,nedit,MAXEDIT
	real xv,yv,xs,ys,xplt(nplt),yplt(nplt)
	logical flag(nplt)
	integer blplt(nplt),chplt(nplt)
        integer bledit(MAXEDIT),chedit(MAXEDIT)
	double precision timeplt(nplt),timeedit(MAXEDIT)
c------------------------------------------------------------------------
	integer i,k
	logical first
	real r2,r2d
c
	first = .true.
	r2 = 0
c
	do i=1,nplt
	  if(flag(i))then
	    r2d = xs*(xplt(i)-xv)**2 + ys*(yplt(i)-yv)**2
	    if(first.or.r2d.lt.r2)then
	      r2 = r2d
	      k = i
	      first = .false.
	    endif
	  endif
	enddo
	if(first)then
	  call bug('w','No points left to edit')
	else
	  nedit = nedit + 1
	  if(nedit.gt.MAXEDIT)call bug('f','Too many ops')
	  bledit(nedit)   = blplt(k)
	  timeedit(nedit) = timeplt(k)
          chedit(nedit) = chplt(k)
	  flag(k) = .false.
	  call pgpt(1,xplt(k),yplt(k),1)
	endif
	end
c************************************************************************
	subroutine Extract(k,xdat,ydat,bldat,chdat,timedat,ndat,
     *			     xplt,yplt,blplt,chplt,timeplt,MAXPLT,nplt)
c
	implicit none
	integer k,ndat,MAXPLT,nplt
	real xdat(ndat),ydat(ndat),xplt(MAXPLT),yplt(MAXPLT)
	integer bldat(ndat),blplt(MAXPLT),chdat(ndat),chplt(MAXPLT)
	double precision timedat(ndat),timeplt(MAXPLT)
c------------------------------------------------------------------------
	integer i
c
	nplt = 0
c
	nplt = 0
	do i=1,ndat
	  if(bldat(i).eq.k)then
	    nplt = nplt + 1
	    if(nplt.gt.MAXPLT)call bug('f','Too many points')
	    blplt(nplt) = k
	    timeplt(nplt) = timedat(i)
            chplt(nplt) = chdat(i)
	    xplt(nplt)    = xdat(i)
	    yplt(nplt)    = ydat(i)
	  endif
	enddo
c
	end	  
c************************************************************************
	subroutine GetDat(tno,rms,scalar,nofqaver,xaxis,yaxis,xmin,xmax,
     *          ymin,ymax,present,maxbase1,xdat,ydat,bldat,chdat,
     *          timedat,ndat,MAXDAT)
c
	implicit none
	integer tno,maxbase1,MAXDAT,ndat
	integer bldat(MAXDAT),chdat(MAXDAT)
	logical present(maxbase1),rms,scalar,nofqaver
	double precision timedat(MAXDAT)
	real xdat(MAXDAT),ydat(MAXDAT),xmin,xmax,ymin,ymax
	character xaxis*(*),yaxis*(*)
c------------------------------------------------------------------------
	include 'maxdim.h'
	double precision TTOL
	parameter(TTOL=1d0/86400d0)
        integer maxchan1
        parameter (MAXCHAN1=4098)
c
	logical flags(MAXCHAN1),ok
	complex data(MAXCHAN1)
	complex corr(MAXBASE),corr1(MAXBASE),corr2(MAXBASE)
	complex fcorr(MAXBASE,MAXCHAN1),fcorr1(MAXBASE,MAXCHAN1),
     *    fcorr2(MAXBASE,MAXCHAN1)
	double precision preamble(4),time,time0,tprev,lst,ra
	real uvdist2(MAXBASE),var(MAXBASE),temp
	integer i,j,n,bl,i1,i2,nants,npnt(MAXBASE),
     *    fnpnt(MAXBASE,MAXCHAN1),mbase,nchan
c
c  Miscellaneous initialisation.
c
	mbase = min(MAXBASE,maxbase1)
	do i=1,MAXBASE
	  present(i) = .false.
	enddo
c
	do i=1,mbase
	  npnt(i)    = 0
	  uvdist2(i) = 0
	  corr(i)    = 0
	  corr1(i)   = 0
	  corr2(i)   = 0
	  var(i) = 0
          do j=1,maxchan1
            fnpnt(i,j)  = 0
            fcorr(i,j)  = 0
            fcorr1(i,j) = 0
            fcorr2(i,j) = 0
          enddo
	enddo
	ndat = 0
c
c  Lets get going.
c
	call output('Reading the data ...')
	call uvDatRd(preamble,data,flags,MAXCHAN1,nchan)
	if(nchan.eq.0)call bug('f','No visibility data found')
        if (nchan.eq.MAXCHAN1) call bug('f','Too many channels for me')
	call flagchk(tno)
	nants = 0
	tprev = preamble(3)
	time0 = int(tprev - 0.5d0) + 0.5d0
	call uvrdvrd(tno,'lst',lst,0.d0)
	call uvrdvrd(tno,'ra',ra,0.d0)
	dowhile(nchan.gt.0)
	  call BasAnt(preamble(4),i1,i2)
	  bl = (i2*(i2-1))/2 + i1
	  ok = bl.lt.mbase
	  if(ok)then
	    time = preamble(3)
            if (nofqaver) then
	      if(abs(time-tprev).gt.TTOL)then
	        if(nants.gt.0) call IntFlushF(nants,rms,scalar,ra,lst,
     *		  tprev,uvdist2,var,fcorr,fcorr1,fcorr2,xaxis,yaxis,
     *		  xmin,xmax,ymin,ymax,fnpnt,time0,present,mbase,
     *            xdat,ydat,timedat,bldat,chdat,ndat,MAXDAT,nchan)
	        nants = 0
	        tprev = time
	        call uvrdvrd(tno,'lst',lst,0.d0)
	        call uvrdvrd(tno,'ra',ra,0.d0)
	      endif
	      n = 0
	      do i=1,nchan
	        if(flags(i))then
		  n = n + 1
	          fnpnt(bl,i) = fnpnt(bl,i) + 1
		  fcorr(bl,i) = fcorr(bl,i) + data(i)
		  fcorr1(bl,i) = fcorr1(bl,i) + abs(data(i))
		  fcorr2(bl,i) = fcorr2(bl,i) +
     *			    cmplx(real(data(i))**2,aimag(data(i))**2)
	        endif
	      enddo
            else
	      if(abs(time-tprev).gt.TTOL)then
	        if(nants.gt.0) call IntFlush(nants,rms,scalar,ra,lst,
     *		  tprev,uvdist2,var,corr,corr1,corr2,xaxis,yaxis,
     *		  xmin,xmax,ymin,ymax,npnt,time0,present,mbase,
     *            xdat,ydat,timedat,bldat,chdat,ndat,MAXDAT)
	        nants = 0
	        tprev = time
	        call uvrdvrd(tno,'lst',lst,0.d0)
	        call uvrdvrd(tno,'ra',ra,0.d0)
	      endif
	      n = 0
	      do i=1,nchan
	        if(flags(i))then
		  n = n + 1
	          npnt(bl) = npnt(bl) + 1
		  corr(bl) = corr(bl) + data(i)
		  corr1(bl) = corr1(bl) + abs(data(i))
		  corr2(bl) = corr2(bl) +
     *			    cmplx(real(data(i))**2,aimag(data(i))**2)
	        endif
	      enddo
	    endif
	    if(n.gt.0)then
	      call uvDatGtr('variance',temp)
              var(bl) = var(bl) + n*temp
	      uvdist2(bl) = uvdist2(bl) +
     *         n * (preamble(1)*preamble(1)+preamble(2)*preamble(2))
              nants = max(nants,i1,i2)
            endif
          endif
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)	  
	enddo
c
	if(nants.gt.0) then
          if (nofqaver) then
            call IntFlushF(nants,rms,scalar,ra,lst,time,uvdist2,var,
     *		fcorr,fcorr1,fcorr2,xaxis,yaxis,xmin,xmax,ymin,ymax,
     *		fnpnt,time0,present,mbase,xdat,ydat,timedat,bldat,
     *          chdat,ndat,MAXDAT,nchan)
	  else
            call IntFlush(nants,rms,scalar,ra,lst,time,uvdist2,var,
     *		corr,corr1,corr2,xaxis,yaxis,xmin,xmax,ymin,ymax,npnt,
     *		time0,present,mbase,xdat,ydat,timedat,bldat,chdat,
     *          ndat,MAXDAT)
          endif
        endif
c
	end
c************************************************************************
	subroutine IntFlush(nants,rms,scalar,ra,lst,time,uvdist2,var,
     *	  corr,corr1,corr2,xaxis,yaxis,xmin,xmax,ymin,ymax,npnt,
     *	  time0,present,MAXBASE,xdat,ydat,timedat,bldat,chdat,
     *    ndat,MAXDAT)
c
	implicit none
	integer MAXBASE,MAXDAT,nants,npnt(MAXBASE),bldat(MAXDAT),
     *    chdat(MAXDAT),ndat
	double precision ra,lst,time,time0,timedat(MAXDAT)
	real uvdist2(MAXBASE),var(MAXBASE),xdat(MAXDAT),ydat(MAXDAT)
        real xmin,xmax,ymin,ymax
	complex corr(MAXBASE),corr1(MAXBASE),corr2(MAXBASE)
	logical present(MAXBASE),rms,scalar
	character xaxis*(*),yaxis*(*)
c
c------------------------------------------------------------------------
	integer i,j,k,ic
        real x,y
c
c  Externals.
c
	real GetVal
c
        ic=0
	k = 0
	do j=1,nants
	  do i=1,j
	    k = k + 1
	    if(npnt(k).gt.0)then
              x = GetVal(xaxis,uvdist2(k),var(k),corr(k),
     *		corr1(k),corr2(k),npnt(k),lst,time,ra,ic,time0,
     *          rms,scalar)
              y = GetVal(yaxis,uvdist2(k),var(k),corr(k),
     *		corr1(k),corr2(k),npnt(k),lst,time,ra,ic,time0,
     *          rms,scalar)
              if (x.ge.xmin.and.x.lt.xmax.and.y.gt.ymin.and.
     *            y.lt.ymax) then
                ndat = ndat + 1
	        if(ndat.gt.MAXDAT)call bug('f','Too many points.')
	        xdat(ndat) = x
	        ydat(ndat) = y
	        bldat(ndat) = k
	        timedat(ndat) = time
c
c   Use 0 to indicate whole spectrum
c
                chdat(ndat) = 0
              endif
	      present(k) = .true.
	      npnt(k) = 0
	      uvdist2(k) = 0
	      var(k) = 0
	      corr(k) = 0
	      corr1(k) = 0
	      corr2(k) = 0
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine IntFlushF(nants,rms,scalar,ra,lst,time,uvdist2,var,
     *	  corr,corr1,corr2,xaxis,yaxis,xmin,xmax,ymin,ymax,npnt,
     *	  time0,present,MAXBASE,xdat,ydat,timedat,bldat,chdat,
     *    ndat,MAXDAT,nchan)
c
	implicit none
	integer MAXBASE,MAXDAT,nants,nchan,npnt(MAXBASE,nchan),
     *    bldat(MAXDAT),chdat(MAXDAT),ndat
	double precision ra,lst,time,time0,timedat(MAXDAT)
	real uvdist2(MAXBASE),var(MAXBASE),xdat(MAXDAT),ydat(MAXDAT)
        real xmin,xmax,ymin,ymax
	complex corr(MAXBASE,nchan),corr1(MAXBASE,nchan),
     *    corr2(MAXBASE,nchan)
	logical present(MAXBASE),rms,scalar
	character xaxis*(*),yaxis*(*)
c
c------------------------------------------------------------------------
	integer i,j,k,ic
        real x,y
c
c  Externals.
c
	real GetVal
c
	k = 0
	do j=1,nants
	  do i=1,j
	    k = k + 1
            do ic=1,nchan
	      if(npnt(k,ic).gt.0)then
                x = GetVal(xaxis,uvdist2(k),var(k),corr(k,ic),
     *		  corr1(k,ic),corr2(k,ic),npnt(k,ic),lst,time,ic,ra,
     *            time0,rms,scalar)
                y = GetVal(yaxis,uvdist2(k),var(k),corr(k,ic),
     *		  corr1(k,ic),corr2(k,ic),npnt(k,ic),lst,time,ic,ra,
     *            time0,rms,scalar)
                if (x.ge.xmin.and.x.lt.xmax.and.y.gt.ymin.and.
     *              y.lt.ymax) then
	          ndat = ndat + 1
	          if(ndat.gt.MAXDAT)call bug('f','Too many points!')
	          xdat(ndat) = x
	          ydat(ndat) = y
	          bldat(ndat) = k
                  chdat(ndat) = ic
	          timedat(ndat) = time
                endif
	        npnt(k,ic) = 0
	        uvdist2(k) = 0
	        corr(k,ic) = 0
	        corr1(k,ic) = 0
	        corr2(k,ic) = 0
	      endif
            enddo
	    present(k) = .true.
	    var(k) = 0
	  enddo
	enddo
	end
c************************************************************************
	real function GetVal(axis,uvdist2,var,corr,corr1,corr2,npnt,
     *		lst,time,chn,ra,time0,rms,scalar)
c
	implicit none
	character axis*(*)
	real uvdist2,var
	complex corr,corr1,corr2
	double precision time,time0,lst,ra
	integer npnt,chn
	logical rms,scalar
c------------------------------------------------------------------------
	include 'mirconst.h'
	complex data
	double precision dtemp
c
	if(rms)then
	  data = cmplx(sqrt(real(corr2)/npnt - real(corr/npnt)**2),
     *		       sqrt(aimag(corr2)/npnt- aimag(corr/npnt)**2))
	else if(scalar)then
	  data = corr1/npnt
	else
	  data = corr/npnt
	endif
c
	if(axis.eq.'real')then
	  GetVal = real(data)
	else if(axis.eq.'imaginary')then
	  GetVal = aimag(data)
	else if(axis.eq.'amplitude')then
	  GetVal = abs(data)
	else if(axis.eq.'phase')then
	  GetVal = 180/pi * atan2(aimag(data),real(data))
	else if(axis.eq.'uvdistance')then
	  GetVal = 0.001 * sqrt(uvdist2/npnt)
	else if(axis.eq.'rms')then
	  GetVal = sqrt(var/npnt)
	else if(axis.eq.'time')then
	  GetVal = 86400*(time - time0)
	else if(axis.eq.'lst')then
	  GetVal = 86400*lst/(2*pi)
	else if(axis.eq.'hangle')then
	  dtemp = lst - ra
	  if(dtemp.gt.DPI)then
	    dtemp = dtemp - 2*DPI
	  else if(dtemp.lt.-DPI)then
	    dtemp = dtemp + 2*DPI
	  endif
	  GetVal = 86400d0*dtemp/(2*DPI)
        else if(axis.eq.'channel') then
          GetVal = chn
	else
	  call bug('f','I should never get here')
	endif
	end
c************************************************************************
	subroutine GetAxis(xaxis,yaxis)
c
	implicit none
	character xaxis*(*),yaxis*(*)
c------------------------------------------------------------------------
	integer NAX
	parameter(NAX=10)
	integer n
	character axes(NAX)*12
	data axes/'amplitude   ','phase       ',
     *		  'real        ','imaginary   ',
     *		  'time        ','uvdistance  ',
     *		  'lst         ','hangle      ',
     *		  'rms         ','channel     '/
	call keymatch('axis',NAX,axes,1,xaxis,n)
	if(n.eq.0)xaxis = 'time'
	call keymatch('axis',NAX,axes,1,yaxis,n)
	if(n.eq.0)yaxis = 'amplitude'
	end
c************************************************************************
	subroutine GetOpt(nobase,selgen,noapply,rms,scalar,nofqaver,
     *    uvflags)
c
	implicit none
	logical nobase,selgen,noapply,rms,scalar,nofqaver
	character uvflags*(*)
c
c  Get extra processing options.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=9)
	logical present(NOPTS)
	character opts(NOPTS)*8
	data opts/'nobase  ','nocal   ','nopass  ','nopol   ',
     *		  'selgen  ','noapply ','rms     ','scalar  ',
     *            'nofqaver'/
c
	call options('options',opts,present,NOPTS)
c
	nobase = present(1)
	selgen = present(5)
	noapply= present(6)
	rms    = present(7)
	scalar = present(8)
        nofqaver=present(9)
	if(scalar.and.rms)
     *	  call bug('f','Options scalar and rms cannot be used together')
	uvflags = 'sdlwb'
	if(.not.present(2))uvflags(6:6) = 'c'
	if(.not.present(3))uvflags(7:7) = 'f'
	if(.not.present(4))uvflags(8:8) = 'e'
	end
c************************************************************************
	subroutine flagchk(tno)
c
	implicit none
	integer tno
c
c  Check that the user's linetype is not going to cause the flagging
c  routine to vomit when the flagging is applied.
c
c------------------------------------------------------------------------
	integer CHANNEL,WIDE
	parameter(CHANNEL=1,WIDE=2)
	double precision line(6)
c
	call uvinfo(tno,'line',line)
	if(nint(line(1)).ne.CHANNEL.and.nint(line(1)).ne.WIDE)
     *	  call bug('f','Can only flag "channel" or "wide" linetypes')
	if(nint(line(4)).ne.1)
     *	  call bug('f','Cannot flag when the linetype width is not 1')
c
	end

c
      subroutine getrng (keyw, axis, rmin, rmax)
c-----------------------------------------------------------------------
c     Get the axis ranges given by the user
c
c  Input
c    keyw     Keyword to get from user
c    axis     Axis type
c  Output
c    rmin,max Range in appropriate units
c
c-----------------------------------------------------------------------
      character*(*) axis, keyw
      real rmin, rmax
cc
      logical ok
      integer il, len1, nt, s
      real trange(8)
      character cval*64
c-----------------------------------------------------------------------
      il = len1(keyw)
      if (axis.eq.'time') then
        call mkeyr (keyw(1:il), trange, 8, nt)
        if (nt.gt.0) then
          if (nt.ne.8) then
            call bug ('f',
     +        'You must specify 8 numbers for the time range')
          else

c           Convert to seconds.
            rmin = 24.0*3600.0*trange(1) + 3600.0*trange(2) +
     +                    60.0*trange(3) + trange(4)
            rmax = 24.0*3600.0*trange(5) + 3600.0*trange(6) +
     +                    60.0*trange(7) + trange(8)
          end if
        else
          rmin = -1.0e32
          rmax =  1.0e32
        end if
      else if (axis.eq.'hangle') then
        call mkeyr (keyw(1:il), trange, 6, nt)
        if (nt.gt.0) then
          if (nt.ne.6) then
            call bug ('f',
     +        'You must specify 6 numbers for the hangle range')
          else

c           Convert to seconds.
            s = 1
            if (trange(1).lt.0.0) s = -1
            rmin = 3600.0*abs(trange(1)) + 60.0*trange(2) + trange(3)
            rmin = s * rmin

            s = 1
            if (trange(4).lt.0.0) s = -1
            rmax = 3600.0*abs(trange(4)) + 60.0*trange(5) + trange(6)
            rmax = s * rmax
          end if
        else
          rmin = -1.0e32
          rmax =  1.0e32
        end if

      else
        call keya (keyw(:il), cval, 'min')
        if (cval.eq.'min') then
          rmin = -1.0e32
        else
          call atorf(cval, rmin, ok)
          if (.not.ok) then
            cval = 'Conversion error decoding parameter ' // keyw(:il)
            call bug('f', cval)
          end if
        end if

        call keya (keyw(:il), cval, 'max')
        if (cval.eq.'max') then
          rmax = 1.0e32
        else
          call atorf(cval, rmax, ok)
          if (.not.ok) then
            cval = 'Conversion error decoding parameter ' // keyw(:il)
            call bug('f',cval)
          end if
        end if

c       Because atorf actually uses atodf and conversion between
c       double and real may introduce rounding errors.
        if (-1.000001e32.lt.rmin .and. rmin.lt.-0.999999e32) then
          rmin = -1.0e32
        end if

        if ( 0.999999e32.lt.rmax .and. rmax.lt. 1.000001e32) then
          rmax =  1.0e32
        end if
      end if

      end
