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
c@ telescop
c	The primary beam type. The default is ATCA.
c@ name
c	For mode=atmosaic, a name used to derive the pointing name. For
c	example, using name=lmc, will generate pointing names of
c	lmc_1, lmc_2, etc.
c@ cycles
c	For mode=atmosaic, the number of cycles spent on each pointing.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer MAXPNT
	parameter(MAXPNT=2048)
	character device*64,line*16,line1*80,name*12,logf*80,mode*8
	character telescop*16
	logical more
	integer i,j,nx,ny,s,npnt,lu,cycles,l,nout
	double precision ra,dec,x1(2),x2(2)
	real h,v,widthx,widthy,freq
	integer pbObj
	real pbfwhm,cutoff,maxrad,x(MAXPNT),y(MAXPNT)
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
	call keya('telescop',telescop,'atca')
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
        call coAxSet(lu,3,'FREQ',1.d0,dble(freq),0.1d0*dble(freq))
        call coReinit(lu)
	call pbInit(pbObj,telescop,lu)  
        call pbInfo(pbObj,pbfwhm,cutoff,maxrad)
c
	h = 0.566*pbfwhm*cos(PI/3.)
	v = 0.566*pbfwhm*sin(PI/3.)
	nx = widthx/h
c	nx = 2*(nx/2)
	ny = widthy/v
c	ny = 2*(ny/2)
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
	  npnt = npnt + 1
	  if(npnt.gt.MAXPNT)call bug('f','Too many pointings')
	  x(npnt) = 180/PI*x1(1)
	  y(npnt) = 180/PI*x1(2)
c
	  x2(1) = x2(1) - ra
	  x2(2) = x2(2) - dec
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
	if(device.ne.' ')then
	  if(pgbeg(0,device,1,1).ne.1)then
	    call pgldev
	    call bug('f','Error opening graphics device')
	  endif
	  call pgscf(2)
	  call pgpage
	  call pgvstd
	  call setwidth(x,y,npnt)
	  call pgbox('BCNST',0.,0,'BCNST',0.,0)
	  call pglab('RA offset (degrees)',
     *		     'DEC offset (degrees)',
     *		     ' ')
	  call pgpt(npnt,x,y,1)
	  call pgend
	endif
	call output('Total number of pointings: '//itoaf(npnt))
	call logClose
	end
c************************************************************************
	subroutine setwidth(x,y,npnt)
c
	implicit none
	integer npnt
	real x(npnt),y(npnt)
c------------------------------------------------------------------------
	real xmin,xmax,ymin,ymax,del
	integer i
c
	xmin = x(1)
	xmax = xmin
	ymin = y(1)
	ymax = ymin
	do i=2,npnt
	  xmin = min(xmin,x(i))
	  xmax = max(xmax,x(i))
	  ymin = min(ymin,y(i))
	  ymax = max(ymax,y(i))
	enddo
	del = 0.05*max(xmax-xmin,ymax-ymin)
	call pgwnad(xmax+del,xmin-del,ymin-del,ymax+del)
	end
