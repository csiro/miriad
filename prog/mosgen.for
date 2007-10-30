c************************************************************************
	program mosgen
	implicit none
c
c= mosgen - Generate a mosaic file, for the on-line system or for uvgen.
c& rjs
c: uv analysis
c+
c	MOSGEN generates a file containing mosaic pointing centres.
c	The format generated is either suitable either for the ATCA 
c	on-line system or uvgen.
c@ radec
c	The RA and DEC of the centre of the mosaic. This is given in
c	hh:mm:ss,dd:mm:ss format, or decimal hours and degrees. The default
c	is 0,0
c@ width
c	The width of the field to mosaic, in degrees. One or two
c	values can be given, being the width in RA and DEC. The default
c	is 0.1 degrees.
c@ freq
c	The observing frequency, in GHz. The default is 1.4 GHz.
c@ device
c	Standard PGPLOT plotting device, to display a diagram of the
c	pointing centres. The default is to not produce a plot.
c@ log
c	Output log file, giving the pointing centres. The default is
c	the terminal.
c@ mode
c	The mode determines the format of the output log file. Possible
c	values are:
c	  atmosaic  This is the mosaic file format understood by the ATCA
c	            on-line system.
c	  uvgen     This is the format required for uvgen's "center" keyword.
c@ name
c	For mode=atmosaic, a name used to derive the pointing name. For
c	example, using name=lmc, will generate pointing names of
c	lmc_1, lmc_2, etc.
c@ cycles
c	For mode=atmosaic, the number of cycles spent on each pointing.
c------------------------------------------------------------------------
	include 'mirconst.h'
	character device*64,line*16,line1*80,name*12,logf*80,mode*8
	logical more,doplot
	integer i,j,nx,ny,s,npnt,lu,cycles,l,nout
	double precision ra,dec,x1(2),x2(2)
	real h,v,widthx,widthy,freq
c
	integer NMODES
	parameter(NMODES=2)
	character modes(NMODES)*8
c
c  Externals.
c
	integer pgbeg,len1
	character itoaf*3,rangleh*32,hangleh*32,stcat*70
c
	data modes/'atmosaic','uvgen   '/
c
	call keyini
	call keya('device',device,' ')
	doplot = device.ne.' '
	call keyt('radec',ra,'hms',0.d0)
	call keyt('radec',dec,'dms',0.d0)
	call keyr('width',widthx,0.1)
	call keyr('width',widthy,widthx)
	widthx = 0.5*PI/180*widthx
	widthy = 0.5*PI/180*widthy
	call keyr('freq',freq,1.4)
	call keya('name',name,'pnt')
	call keyi('cycles',cycles,3)
	call keymatch('mode',NMODES,modes,1,mode,nout)
	if(nout.eq.0)mode = modes(1)
	call keya('log',logf,' ')
	call keyfin
c
	l = len1(name)
c
	call logOpen(logf,' ')
	line1 = stcat('# Reference position is '//hangleh(ra),
     *					     ','//rangleh(dec))
	call logWrite(line1,more)
	call logInput('mosgen')
c
	call coRaDec(lu,'SIN',ra,dec)
	if(doplot)then
	  if(pgbeg(0,device,1,1).ne.1)then
	    call pgldev
	    call bug('f','Error opening graphics device')
	  endif
	  call pgscf(2)
	  call pgpage
	  call pgvstd
	  call setwidth(lu,widthx,widthy)
	  call pgbox('BCNST',0.,0,'BCNST',0.,0)
	  call pglab('RA offset (degrees)',
     *		     'DEC offset (degrees)',
     *		     ' ')
	endif
	h = PI/180*19.6*1.4/freq/60.*cos(PI/3.)
	v = PI/180*19.6*1.4/freq/60.*sin(PI/3.)
	nx = widthx/h
	nx = 2*(nx/2)
	ny = widthy/v
	ny = 2*(ny/2)
c	h = PI/180*h
c	v = PI/180*v
	i = -nx
	j = -ny
	s =  1
	npnt = 0
	more = .true.
	dowhile(more)
	  x1(1) = i*h
	  x1(2) = j*v
	  call CoCvt(lu,'op/op',x1,'aw/aw',x2)
	  call CoCvt(lu,'aw/aw',x2,'ow/ow',x1)
	  if(doplot)call pgpt(1,real(180/PI*x1(1)),real(180/PI*x1(2)),1)
c
	  x2(1) = x2(1) - ra
	  x2(2) = x2(2) - dec
	  npnt = npnt + 1
	  if(mode.eq.'atmosaic')then
	    write(line,'(2f8.4)')180/PI*x2(1),180/PI*x2(2)
	    line1 = line//' '//itoaf(cycles)//' $'//
     *					name(1:l)//'_'//itoaf(npnt)
	  else
	    write(line1,'(2f10.1)')180/PI*3600*x1(1),180/PI*3600*x1(2)
	  endif
	  call logwrite(line1,more)
	  if(j*s.eq.ny)then
	    if(i.eq.nx)then
	      more = .false.
	    else
	      i = i + 2
	      s = -s
	    endif
	  else
	    j = j + s
	    if(2*(j/2).eq.j)then
	      i = i - 1
	    else
	      i = i + 1
	    endif
	  endif
	enddo
	if(doplot)call pgend
	call output('Total number of pointings: '//itoaf(npnt))
	call logClose
	end
c************************************************************************
	subroutine setwidth(lu,widthx,widthy)
c
	implicit none
	integer lu
	real widthx,widthy
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision x1(2),x2(2)
	logical first
	real xmin,xmax,ymin,ymax
	integer i,j
c
	first = .true.
	do j=-1,1
	  do i=-1,1
	    x1(1) = i*widthx
	    x1(2) = j*widthy
	    call coCvt(lu,'op/op',x1,'ow/ow',x2)
	    if(first)then
	      xmin = x2(1)
	      xmax = xmin
	      ymin = x2(2)
	      ymax = ymin
	      first = .false.
	    else
	      xmin = min(xmin,real(x2(1)))
	      xmax = max(xmax,real(x2(1)))
	      ymin = min(ymin,real(x2(2)))
	      ymax = max(ymax,real(x2(2)))
	    endif
	  enddo
	enddo
	call pgwnad(180/PI*xmin,180/PI*xmax,180/PI*ymin,180/PI*ymax)
	end
