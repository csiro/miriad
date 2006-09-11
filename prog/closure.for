c************************************************************************
	program closure
	implicit none
c
c= closure
c& rjs
c+
c@ vis
c@ select
c@ line
c@ stokes
c@ device
c@ refant
c@ naver
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version='version 7-Sep-94')
	integer MAXPNTS,MAXPROD,MCHAN
	parameter(MAXPNTS=20000,MAXPROD=20,MCHAN=257)
	character uvflags*16
	character title(MAXPROD)*12,device*64
	complex data(MCHAN),buf(MAXBASE,MCHAN)
	logical flags(MCHAN),bflag(MAXBASE,MCHAN),first,overlay
	integer nx,ny,nchan,nread,i1,i2,i3,ant1,ant2,i,j,k
	integer mbase,nants
	integer bl12,bl23,bl13,bl
	double precision time0,preamble(4),t,t1
	real x(MAXPNTS,MAXPROD),y(MAXPNTS,MAXPROD),xlo,xhi,ylo,yhi
	real yrange(2)
	integer npts(MAXPROD),refant,tno,npol
	complex temp
	real xt,yts
	complex yt
	integer nt,naver
c
c  Externals.
c
	integer pgbeg
	logical uvDatOpn
	character itoaf*1
	real phase
c
	call output('Closure: '//version)
	call keyini
	call GetOpt(overlay)
c
c c -- docal
c f -- dopass
c e -- dopol
c
	uvflags = 'dslx'
	call uvDatInp('vis',uvflags)
	call keya('device',device,' ')
	if(device.eq.' ')call bug('f','A plot device must be given')
	if(overlay)then
	  call keyi('nxy',nx,1)
	  call keyi('nxy',ny,1)
	else
	  call keyi('nxy',nx,3)
	  call keyi('nxy',ny,2)
	endif
	call keyi('refant',refant,1)
	call keyi('naver',naver,1)
	call keyr('yrange',yrange(1),0.)
	call keyr('yrange',yrange(2),0.)
	call keyfin
c
	if(.not.uvDatOpn(tno))call bug('f','Failed to open input')
	call uvDatRd(preamble,data,flags,MCHAN,nchan)
	call uvDatGti('npol',npol)
	if(npol.ne.1)call bug('f','Bad number of polarisations')
c
c  Initialise ready for the loop.
c
	nread = nchan
	call uvrdvri(tno,'nants',nants,0)
	time0 = int(preamble(3) - 0.5d0) + 0.5d0
	t = preamble(3) - time0
c
	mbase = ((nants+1)*(nants+2))/2
	do j=1,nchan
	  do i=1,mbase
	    bflag(i,j) = .false.
	  enddo
	enddo
c
	k = 0
	  do i3=3,nants
	    do i2=2,i3-1
	      do i1=1,i2-1
	        k = k + 1
	        title(k) = 'Triple '//itoaf(i1)//'-'//
     *				       itoaf(i2)//'-'//
     *				       itoaf(i3)
	      enddo
	    enddo
	  enddo
c
	do i=1,MAXPROD
	  npts(i) = 0
	enddo
c
	dowhile(nchan.eq.nread)
	  call Basant(preamble(4),ant1,ant2)
	  if(min(ant1,ant2).ge.1.and.max(ant1,ant2).le.nants.and.
     *	     ant1.ne.ant2)then
	    t1 = preamble(3) - time0
	    if(t1.ne.t)then
	      k = 0
	      do i3=3,nants
		do i2=2,i3-1
		  do i1=1,i2-1
		    k = k + 1
		    bl12 = ((i2-1)*(i2-2))/2 + i1
		    bl13 = ((i3-1)*(i3-2))/2 + i1
		    bl23 = ((i3-1)*(i3-2))/2 + i2
		    nt = 0
		    yt = 0
		    do i=1,nchan
		      if(bflag(bl12,i).and.bflag(bl13,i).and.
     *		         bflag(bl23,i))then
		        yt = yt + buf(bl12,i)*buf(bl23,i)*
     *					     conjg(buf(bl13,i))
		        nt = nt + 1
		      endif
		    enddo
		    if(nt.gt.0)then
		      npts(k) = npts(k) + 1
		      x(npts(k),k) = 86400*t
		      y(npts(k),k) = 180/pi * atan2(aimag(yt),real(yt))
		    endif
		  enddo
	        enddo
	      enddo
c
	      do j=1,nchan
	        do i=1,mbase
		  bflag(i,j) = .false.
		enddo
	      enddo
	    endif
	    t = t1
	    bl = (ant2-1)*(ant2-2)/2 + ant1
	    do i=1,nchan
	      buf(bl,i) = data(i)
	      bflag(bl,i) = flags(i)
	    enddo
	  endif
	  call uvDatRd(preamble,data,flags,MCHAN,nread)
	enddo
	call uvDatCls
c
c  Average the points together.
c
	if(naver.gt.1)then
	  do k=1,MAXPROD
	    j = 0
	    nt = 0
	    xt = 0
	    yts = 0
	    temp = 0
	    do i=1,npts(k)
	      xt = xt + x(i,k)
	      yts = yts + y(i,k)
	      nt = nt + 1
	      if(nt.eq.naver)then
	        j = j + 1
	        x(j,k) = xt/naver
	        y(j,k) = yts/naver
		xt = 0
		yts = 0
		nt = 0
	      endif
	    enddo
	    npts(k) = j
	  enddo
	endif
c
c  Find the min and max times and amps.
c
	yts = 0
	nt = 0
	first = .true.
	do j=1,MAXPROD
	  do i=1,npts(j)
	    if(first)then
	      xlo = x(i,j)
	      xhi = xlo
	      ylo = y(i,j)
	      yhi = ylo
	      first = .false.
	    else
	      xlo = min(xlo,x(i,j))
	      xhi = max(xhi,x(i,j))
	      ylo = min(ylo,y(i,j))
	      yhi = max(yhi,y(i,j))
	    endif
	    yts = yt + y(i,j)**2
	    nt = nt + 1
	  enddo
	enddo
c
	write(*,*)'RMS closure phase is ',sqrt(yts/nt)
	if(yrange(2).gt.yrange(1))then
	  yhi = yrange(2)
	  ylo = yrange(1)
	endif
c
	if(pgbeg(0,device,nx,ny).ne.1)
     *	 call bug('f','Failed to open plot device')
	call pgsch(real(max(nx,ny))**0.4)
c
	if(overlay)then
	  call pgpage
	  call pgvstd
	  call pgswin(xlo,xhi,ylo,yhi)
	  call pgtbox('BCNSTHZO',0.,0.,'BCNST',0.,0.)
	  call pglab('Time','Closure Phase (degrees)','All triples')
	endif
	do j=1,MAXPROD
	  if(npts(j).gt.0)then
	    if(.not.overlay)then
	      call pgpage
	      call pgvstd
	      call pgswin(xlo,xhi,ylo,yhi)
	      call pgtbox('BCNSTHZO',0.,0.,'BCNST',0.,0.)
	      call pglab('Time','Closure Phase (degrees)',title(j))
	    endif
	    call pgpt(npts(j),x(1,j),y(1,j),1)
	  endif
	enddo
	call pgend
	end
c************************************************************************
	subroutine getopt(overlay)
c
	implicit none
	logical overlay
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'overlay '/
c
	call options('options',opts,present,NOPTS)
	overlay = present(1)
	end
