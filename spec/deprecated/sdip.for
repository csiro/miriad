c************************************************************************
	program sdip
c
	implicit none
c
c= sdip - Compute receiver, sky and diode temperatures.
c& rjs
c: analysis
c+
c@ dsdlog
c	DSD log file.
c@ thot
c	One or two values, giving the hot load temperature and the
c	antenna temperature, in Kelvin. The default is 300,0.
c@ htime
c	Time range of hot loads.
c@ stime
c	Time range of sky dips.
c@ ant
c	The antenna to process. Use either 3 or 4. The default is 3.
c@ options
c	Extra processing options.
c	  onoff  Use this option if the antenna has a noise diode.
c@ device
c	PGPLOT device to plot the result. The data are plotted in black, and
c	the "fit" in red. The default is to not plot the results.
c--
c  History:
c    27apr01 rjs   Original version.
c------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	parameter(version='Sdip: version 27-Apr-01')
	integer MAXT,MAXEQ
	parameter(MAXT=10,MAXEQ=2000)
c
	logical onoff,ht,st
	character dsdlog*64,device*64,state*2,ch(8)*5,line*64
	double precision htime(2,MAXT),stime(2,MAXT),time,time0
	integer neq,nht,nst,nhts,nsts,i,j,ifail,ant,off,offsets(8)
	real times(MAXEQ),y1(MAXEQ),y2(MAXEQ)
	real Thot,Tant,Trec,Tcal,Tsky,xlo,xhi,ylo,yhi,el,dsdon,dsdoff
	real ymin,ymax
	real eqs(3,MAXEQ,4),bs(MAXEQ,4),temps(3)
c
c  Scratch arrays.
c
	real rtmp(9)
	integer itmp(3)
c
c  Externals.
c
	character itoaf*4
	integer pgBeg,tinNext
c
	data offsets/ -286,  155, -259,  437,  -38,  -35,  608, -324/
	data ch/    'A3   ','B3   ','C3   ','D3   ',
     *		    'A4   ','B4   ','C4   ','D4   '/
c
	call output(version)
	call keyini
	call keyi('ant',ant,3)
	call keya('dsdlog',dsdlog,' ')
	call keyr('thot',thot,300.0)
	call keyr('thot',tant,0.0)
	call keya('device',device,' ')
	call mkeyt('htime',htime,2*MAXT,nht,'atime')
	if(2*(nht/2).ne.nht)call bug('f','Odd no. of hot load times')
	if(nht.lt.2)call bug('f','Hot load times must be given')
	call mkeyt('stime',stime,2*MAXT,nst,'atime')
	if(2*(nst/2).ne.nst)call bug('f','Odd no. of sky dip times')
	if(nst.lt.2)call bug('f','Sky dip times must be given')
	call getopt(onoff)
	call keyfin
c
	off = 0
	if(ant.eq.4)off = 4
c
	neq = 0
	nhts = 0
	nsts = 0
	call tinOpen(dsdlog,'n')
	dowhile(tinNext().gt.0)
	  call tinSkip(1)
	  call tinGett(time,0.d0,'atime')
	  call tinGeta(state,' ')
	  if(state.eq.'i'.or.state.eq.'t')then
	    ht = .false.
	    st = .false.
	    do j=1,nht
	      ht = ht.or.(htime(1,j).le.time.and.time.le.htime(2,j))
	    enddo
	    do j=1,nst
	      st = st.or.(stime(1,j).le.time.and.time.le.stime(2,j))
	    enddo
	    if(ht.or.st)then
	      neq = neq + 1
	      if(neq.gt.MAXEQ)call bug('f','Too many equations')
	      call tinSkip(1)
	      call tinGetr(el,0.)
	      el = PI/180.0 * el
	      if(neq.eq.1)time0 = int(time-0.5) + 0.5
	      times(neq) = 86400*(time-time0)
	      if(ht)nhts = nhts + 1
	      if(st)nsts = nsts + 1
c
	      if(ant.eq.4)call tinSkip(8)
	      do j=1,4
		call tinGetr(dsdon,0.)
		call tinGetr(dsdoff,0.)
		if(dsdon.gt.61000.or.dsdoff.gt.61000)then
		  dsdon = 0
		else if(onoff)then
		  dsdon = (dsdoff-offsets(j+off))/(dsdon-dsdoff)
		else
		  dsdon = 0.5*(dsdon+dsdoff)-offsets(j+off)
		endif
		if(dsdon.eq.0)then
		  eqs(1,neq,j) = 0
		  eqs(2,neq,j) = 0
		  eqs(3,neq,j) = 0
		  bs(neq,j) = 0
		else if(ht)then
		  eqs(1,neq,j) = 1
		  eqs(2,neq,j) = -dsdon
		  eqs(3,neq,j) = 0
		  bs(neq,j) = -thot
		else
		  eqs(1,neq,j) = 1
		  eqs(2,neq,j) = -dsdon
		  eqs(3,neq,j) = 1./sin(el)
		  bs(neq,j) = -tant
		endif
	      enddo
	    endif
	  endif
	enddo
	call tinClose
	call output('Hot load samples: '//itoaf(nhts))
	call output('Sky dip samples:  '//itoaf(nsts))
	if(nhts.eq.0.or.nsts.eq.0)call bug('f','Ill conditioned')
c
c  Now solve for all the parameters.
c
	if(device.ne.' ')then
	  if(pgbeg(0,device,2,2).ne.1)
     *	    call bug('f','Failed to open plot device')
	  call pgscf(2)
	  call pgsch(2.0)
	endif
	call output('Channel Trec   Tcal   Tsky')
	call output('------- ----   ----   ----')
c
	do j=1,4
	  call llsqu(bs(1,j),eqs(1,1,j),3,neq,temps,
     *						ifail,rtmp,itmp)
	  if(ifail.ne.0)then
	    call bug('w','Ifail = '//itoaf(ifail))
	    trec = 0
	    tsky = 0
	    tcal = 0
	  else
	    trec = temps(1)
	    tcal = temps(2)
	    tsky = temps(3)
	  endif
	  write(line,'(a,3f7.1)')ch(j+off),Trec,Tcal,Tsky
	  call output(line)
c
	  if(device.ne.' ')then
	    do i=1,neq
	      y1(i) = -eqs(2,i,j)*Tcal
	      if(eqs(1,i,j).eq.0)then
		y2(i) = 0
	      else if(eqs(3,i,j).gt.0)then
	        y2(i) = (trec+tant+tsky*eqs(3,i,j))
	      else
	        y2(i) = (trec+thot)
	      endif
	    enddo
	    xlo = times(1)
	    xhi = times(neq)
	    xlo = xlo - 0.1*(xhi-xlo)
	    xhi = xhi + 0.1*(xhi-xlo)
	    ymin = min(y1(1),y2(1))
	    ymax = max(y1(1),y2(1))
	    do i=1,neq
	      ymin = min(ymin,y1(i),y2(i))
	      ymax = max(ymax,y1(i),y2(i))
	    enddo
	    call pgrnge(ymin,ymax,ylo,yhi)
	    yhi = max(600.0,yhi+0.1*(yhi-ylo))
	    ylo = min(0.0,ylo-0.1*(yhi-ylo))
c
	    call pgpage
	    call pgvstd
	    call pgswin(xlo,xhi,ylo,yhi)
	    call pgtbox('BCSTNHZO',0.,0,'BCNST',0.,0)
	    call pglab('Time (UTC)','DSD Value (K)',
     *				'Channel '//ch(j+off))
	    call pgpt(neq,times,y1,1)
	    call pgsci(2)
	    call pgpt(neq,times,y2,1)
	    call pgsci(1)
	  endif
	enddo
	if(device.ne.' ')call pgend
c
	end
c************************************************************************
	subroutine getopt(onoff)
c
	implicit none
	logical onoff
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'onoff   '/
	call options('options',opts,present,NOPTS)
	onoff = present(1)
	end
