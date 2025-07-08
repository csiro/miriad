c************************************************************************
	program moslst
c
	implicit none
c
c= MOSLST -- LST coverage of the different pointings in a mosaic.
c& rjs
c: uv analysis
c+
c	Determine the LSTs at which particular fields in a mosaic
c	run are observed.
c@ vis
c	Input vis datasets. No default.
c@ fields
c	Indices of the pointings to check. If nothing is given, all 
c	pointings are checked.
c@ source
c	Base source name of the mosaic. By default, MOSLST assumes that
c	any source whose names ends in digits is a pointing of a mosaic,
c	and takes this digits as the pointing number. If the SOURCE parameter
c	is given, then MOSLST takes only those sources starting with that
c	name, ending in digits, and with a possible underscore character
c	in between.
c@ device
c	PGPLOT device for the LST ranges. The default is no plot.
c@ ppp
c	"Pointings per plot". This is used for plotting, and allows the 
c	output to be broken into a number of subplots. Each subplot will
c	have (at most) the given number of pointings in it. The default
c	is to not break it into subplots.
c@ log
c	Output log file. Default is the terminal.
c@ gap
c	Gap tolerance. This is used for the log output to indicate
c	large gaps in the LST coverage. The default is 36 minutes.
c--
c  History:
c    rjs   2nov94 Original version.
c    rjs  27jan00 Reborn as MOSLST
c------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	parameter(version='MOSLST: version 1.0 27-Jan-00')
	integer MAXFLDS,MAXVIS,MAXPNTS
	parameter(MAXFLDS=20,MAXVIS=100,MAXPNTS=30000)
	integer fields(MAXFLDS),nvis,nflds,npnt,iostat,npnt2
	integer Field(MAXPNTS),indx(MAXPNTS),i,j,k,tno,n,ipnt,fldno
	integer Field2(MAXPNTS),ppp,fac,l,fmin,fmax,f
	double precision LST(MAXPNTS),LST2(MAXPNTS),gap
	real x(MAXPNTS),y(MAXPNTS)
	character vis(MAXVIS)*64,logf*64,device*64,source*16,num*4
	character line*64,csource*64,c*1
	logical ok,doinit,doall,more
c
c  Externals.
c
	integer len1,uvscan,pgbeg
	character hangle*16,itoaf*4
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call mkeyf('vis',vis,MAXVIS,nvis)
	call mkeyi('fields',fields,MAXFLDS,nflds)
	call keya('source',csource,' ')
	call keyi('ppp',ppp,0)
	call keya('log',logf,' ')
	call keya('device',device,' ')
	call keyd('gap',gap,36.0d0)
	call keyfin
c
c  Check the inputs and fill in any defaults.
c
	if(nvis.le.0)call bug('f','Input vis sets must be given')
	doall = nflds.le.0
	gap = 2*pi*gap/60./24.
	call lcase(csource)
c
c   Process a file.
c
	npnt = 0
	do k=1,nvis
	  call output('Processing '//vis(k))
	  call uvopen(tno,vis(k),'old')
	  iostat = uvscan(tno,'lst')
	  dowhile(iostat.eq.0.and.npnt.lt.MAXPNTS)
	    call uvrdvra(tno,'source',source,' ')
	    call lcase(source)
	    n = 0
	    l = len1(source)
	    fac = 1
	    more = .true.
	    dowhile(l.gt.0.and.more)
	      c = source(l:l)
	      if(c.ge.'0'.and.c.le.'9')then
		n = n + fac*(ichar(c) - ichar('0'))
	        fac = 10*fac
		l = l - 1
	      else
	        more = .false.
	      endif
	    enddo
	    ok = n.gt.0.and.l.gt.0
	    if(ok.and.csource.ne.' ')then
	      if(source(l:l).eq.'_')l = l-1
	      if(ok.and.l.gt.0) ok = csource.eq.source(1:l)
	    endif
	    if(ok.and..not.doall)then
	      ok = .false.
	      do i=1,nflds
	        ok = ok.or.n.eq.fields(i)
	      enddo
	    endif
	    if(ok)then
	      npnt = npnt + 1
	      if(npnt.gt.MAXPNTS)call bug('f','Too many points')
	      Field(npnt) = n
	      call uvrdvrd(tno,'lst',LST(npnt),0.d0)
	    endif
	    iostat = uvscan(tno,'lst')
	  enddo
	  call uvclose(tno)
	enddo
c
	if(npnt.eq.0)call bug('f','No pointings found')
c
c  Determine the min and max pointing.
c
	fmin = field(1)
	fmax = fmin
	do i=2,npnt
	  fmax = max(fmax,field(i))
	  fmin = min(fmin,field(i))
	enddo
	if(ppp.le.0)ppp = fmax
c
c  Open the outputs.
c
	call logopen(logf,' ')
	if(device.ne.' ')then
	  if(pgbeg(0,device,1,3).ne.1)then
	    call pgldev
	    call bug('f','Error opening graphics device')
	  endif
	  call pgsch(2.0)
	endif
c
c  Now process each field.
c
	call output('Creating the output ...')
	call hsorti(npnt,Field,indx)
	ipnt = 0
c
	if(doall)then
	  fmin = ppp*((fmin-1)/ppp)+1
	  do f=fmin,fmax,ppp
	    call GetR(f,f+ppp-1,ipnt,npnt,indx,Field,LST,
     *					Field2,LST2,npnt2)
	    if(npnt2.gt.0)then
	      call pgpage
	      call pgvstd
	      call pgswin(-1000.0,45000.0,
     *			real(f-4),real(f+ppp+3))
	      call pgtbox('BCNSTHZO',0.,0.,'BCNT',0.,0.)
	      call pglab('LST Range','Field',' ')
	      do i=1,npnt2
	        x(i) = 12/pi * 3600 * LST2(i)
	        y(i) = Field2(i)
	      enddo
	    call pgpt(npnt2,x,y,1)
	    endif
	  enddo
	else
	  do j=1,nflds
	    call GetF(ipnt,npnt,indx,Field,LST,Fldno,LST2,npnt2)
	    if(npnt2.gt.0)then
	      call logwrit(' ')
	      call logwrit('Number of samples: '//itoaf(npnt2))
	      num = itoaf(Fldno)
	      line = 'Observed LSTs for field '//num
	      call logwrit(line)
	      do i=1,npnt2
	        line = '  '//hangle(LST2(i))
	        call logwrit(line)
	      enddo
	      doinit = .true.
	      do i=1,npnt2-1
	        call Gapper(doinit,LST2(i+1),LST2(i),gap)
	      enddo
	      call Gapper(doinit,LST2(1)+pi,LST2(npnt2),gap)
	      if(doinit)
     *		call logwrit('There were no gaps in the coverage')
	      if(device.ne.' ')then
	        call pgpage
	        call pgvstd
	        call pgswin(-1000.0,45000.0,-0.5,0.5)
	        call pgtbox('BCNSTHZO',0.,0.,'BC',0.,0.)
	        call pglab('LST Range',' ','Field '//num)
	        do i=1,npnt2
	          x(i) = 12/pi * 3600 * LST2(i)
	          y(i) = 0
	        enddo
	        call pgpt(npnt2,x,y,17)
	      endif
	    endif
	  enddo
	endif
c
	call logclose
	if(device.ne.' ')call pgend
	end
c************************************************************************
	subroutine Gapper(doinit,LST1,LST2,gap)
c
	implicit none
	logical doinit
	double precision LST1,LST2,gap
c------------------------------------------------------------------------
	include 'mirconst.h'
	character line*64
	integer mins,l
c
	integer len1
	character dangle*8,itoaf*8
c
	if(LST1.gt.LST2+gap)then
	  if(doinit)call logwrit('Large gaps for this field:')
	  mins = nint(12/pi*60 * (LST1-LST2))
	  line = dangle(12/pi * LST2)//' to '//dangle(12/pi * LST1)
     *		//' ('//itoaf(mins)
	  l = len1(line)
	  line(l+1:) = ' minutes)'
	  call logwrit(line)
	  doinit = .false.
	endif
c
	end
c************************************************************************
	subroutine GetR(ilo,ihi,ipnt,npnt,indx,Field,LST,
     *					Field2,LST2,npnt2)
c
	implicit none
	integer ilo,ihi,ipnt,npnt,indx(npnt),Field(npnt)
	integer Field2(npnt),npnt2
	double precision LST(npnt),LST2(npnt)
c------------------------------------------------------------------------
	include 'mirconst.h'
	logical more
	integer j
	double precision d
c
	npnt2 = 0
	if(ipnt.eq.npnt)return
c
	more = .true.
	dowhile(ipnt.lt.npnt.and.more)
	  j = indx(ipnt+1)
	  if(Field(j).ge.ilo.and.Field(j).le.ihi)then
	    npnt2 = npnt2 + 1
	    ipnt = ipnt + 1
	    d = mod(LST(j),2*dpi)
	    if(d.lt.   0)d = d + 2*dpi
	    if(d.gt. dpi)d = d - pi
	    LST2(npnt2) = d
	    Field2(npnt2) = Field(j)
	  else
	    more = .false.
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GetF(ipnt,npnt,indx,Field,LST,Fldno,LST2,npnt2)
c
	implicit none
	integer ipnt,npnt,npnt2
	integer indx(npnt),Field(npnt),FldNo
	double precision LST(npnt),LST2(npnt)
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	logical more
	integer i,j
	double precision d
c
	npnt2 = 0
	if(ipnt.eq.npnt)return
c
	FldNo = Field(indx(ipnt+1))
	npnt2 = 0
c
	more = .true.
	dowhile(ipnt.lt.npnt.and.more)
	  j = indx(ipnt+1)
	  if(Field(j).eq.FldNo)then
	    npnt2 = npnt2 + 1
	    ipnt = ipnt + 1
	    d = mod(LST(j),2*dpi)
	    if(d.lt.   0)d = d + 2*dpi
	    if(d.gt. dpi)d = d - pi
	    LST2(npnt2) = d
	  else
	    more = .false.
	  endif
	enddo
c
c  Sort them into order.
c
	more = .true.
	dowhile(more)
	  more = .false.
	  do i=1,npnt2-1
	    if(LST2(i).gt.LST2(i+1))then
	      more = .true.
	      d = LST2(i)
	      LST2(i) = LST2(i+1)
	      LST2(i+1) = d
	    endif
	  enddo
	enddo
c
	end
