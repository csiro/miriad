c************************************************************************
	program sfunc
	implicit none
c
c= sfunc - Plot phase structure functions.
c+
c sfunc - Plot phase structure functions.
c
c@ vis
c	Input visibility dataset. No default. Several files can be given.
c	Wildcards are supported.
c@ select
c	Standard visibility data selection. The default is to select all
c	data.
c@ interval
c	The interval over which to average to determine inidividual points
c	in the structure function. The default is 30 min.
c@ device
c	Output PGPLOT plotting device of a structure function plot. The
c	default is to not generate a plot.
c@ range
c	Minimum and maximum baseline ranges used in the fitting process.
c	These are given in metres.
c@ log
c	Output log file. The default is to not generate log output.
c@ options
c	Extra process options. Minimum match applies.
c	  nocal      Do not apply any calibration.
c--
c  History:
c    04oct97 rjs	Original version.
c    21oct98 rjs	Significantly enhanced.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version='Sfunc: version 1.0 21-Oct-98')
	integer MAXPNT
	parameter(MAXPNT=30000)
	integer npnt,n,nfile,tno
	double precision S(MAXBASE),S2(MAXBASE),am(MAXBASE)
	double precision tmin,tmax,preamble(4)
	integer ncount(MAXBASE)
	real d(MAXBASE),dmin,dmax
	real D2l(MAXPNT),b(MAXPNT),mass(MAXPNT)
	real x(MAXPNT),y(MAXPNT),w(MAXPNT)
	real xmin,xmax,ymin,ymax,temp,lambda
	integer file(MAXPNT),ngood,i1,i2,bl,i,j,k,nchan,vUpd
	complex data(MAXCHAN),fac(MAXBASE),sum
	logical flags(MAXCHAN),docal,dolog,doplot
	character device*64,logf*64,line*80
	real massmin(4),massmax(4)
	double precision interval
c
c  Externals.
c
	logical uvDatOpn,uvVarUpd
	integer pgbeg
	data massmin/1.0,1.5,2.0,3.0/
	data massmax/1.5,2.0,3.0,5.0/
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call getopt(docal)
	if(docal)then
	  call uvDatInp('vis','dxcef')
	else
	  call uvDatInp('vis','dxef')
	endif
	call keyr('range',dmin,1.)
	call keyr('range',dmax,10000.)
	dmin = log10(dmin)
	dmax = log10(dmax)
	call keya('device',device,' ')
	call keyd('interval',interval,30.0d0)
	interval = interval /60.0/24.0
	call keya('log',logf,' ')
c
	dolog = logf.ne.' '
	doplot = device.ne.' '
	if(.not.dolog.and..not.doplot)call bug('f',
     *	  'No operation to perform')
	call keyfin
c
	call uvDatSet('stokes',0)
c
	if(dolog)call logOpen(logf,' ')
c
	nfile = 0
	npnt = 0
	dowhile(uvDatOpn(tno))
	  nfile = nfile + 1
	  tmin = 0
	  tmax = 0
	  do i=1,MAXBASE
	    ncount(i) = 0
	  enddo
	  call uvVarini(tno,vupd)
	  call uvvarSet(vupd,'antpos')
	  call uvvarSet(vupd,'ra')
	  call uvvarSet(vupd,'dec')
c
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    if(uvvarUpd(vupd))then
	      call flushit(0.5d0*(tmin+tmax),MAXBASE,ncount,S,S2,
     *		d,am,nfile,D2l,b,mass,file,w,MAXPNT,npnt,docal,dolog)
	      call Setd(tno,d,MAXBASE)
	    endif
	    Sum = 0
	    ngood = 0
	    do i=1,nchan
	      if(flags(i))then
		Sum = Sum + data(i)
		ngood = ngood + 1
	      endif
	    enddo
	    if(ngood.gt.0)then
	      Sum = Sum / ngood
	      call basant(preamble(4),i1,i2)
	      bl = (i2-1)*(i2-2)/2 + i1
	      call getam(tno,temp,lambda)
	      if(max(preamble(3),tmax)-min(preamble(3),tmin).gt.
     *							interval)then
     	    	call flushit(0.5d0*(tmin+tmax),MAXBASE,ncount,S,S2,d,
     *		  am,nfile,D2l,b,mass,file,w,MAXPNT,npnt,docal,dolog)
		tmin = preamble(3)
		tmax = tmin
	      endif
	      if(ncount(bl).eq.0)then
		S(bl) = 0
		S2(bl) = 0
		am(bl) = 0
		if(docal)then
		  fac(bl) = 1
		else
		  fac(bl) = conjg(Sum)
		endif
	      endif
	      ncount(bl) = ncount(bl) + 1
	      am(bl) = am(bl) + temp
	      tmin = min(tmin,preamble(3))
	      tmax = max(tmax,preamble(3))
	      Sum = Sum*fac(bl)
	      temp = lambda/(2*PI)*atan2(aimag(Sum),real(Sum))
	      S(bl) = S(bl) + temp
	      S2(bl) = S2(bl) + temp*temp
	    endif
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvDatCls
     	  call flushit(0.5d0*(tmin+tmax),MAXBASE,ncount,S,S2,d,
     *	    am,nfile,D2l,b,mass,file,w,MAXPNT,npnt,docal,dolog)
	enddo
c
c  Do the plot.
c
	if(doplot)then
	  if(pgbeg(0,device,2,2).ne.1)
     *	    call bug('f','Error opening PGPLOT device')
	  call pgscf(2)
c
	  xmin = 1
	  xmax = 4
	  ymin = -3
	  ymax = 3
c
	  do j=1,4
	    call pgenv(xmin,xmax,ymin,ymax,0,30)
	    do k=1,nfile
	      n = 0
	      do i=1,npnt
	        if(massmin(j).le.mass(i).and.
     *	           mass(i).le.massmax(j).and.file(i).eq.k)then
	          n = n + 1
	          x(n) = b(i)
	          y(n) = D2l(i)
	        endif
	      enddo
	      call pgsci(k)
	      if(n.gt.0)call pgpt(n,x,y,17)
	    enddo
	    call pgsci(1)
	    call fitit(massmin(j),massmax(j),dmin,dmax,
     *		  mass,b,D2l,npnt,x,y,w,line)
	    call pglab('Baseline (m)',
     *		'D\dL\b\u\u2\d(b) ( (mm)\u2\d)',
     *		line)
	  enddo
c
	  call pgend
	endif
c
	if(dolog)call logClose
c

	end
c************************************************************************
	subroutine getopt(docal)
c
	implicit none
	logical docal
c
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'nocal   '/
c
	call options('options',opts,present,NOPTS)
	docal = .not.present(1)
	end
c************************************************************************
	subroutine fitit(mass1,mass2,dmin,dmax,mass,b,d2l,n,x,y,w,line)
c
	implicit none
	integer n
	real mass1,mass2,mass(n),b(n),d2l(n),x(n),y(n),w(n),dmin,dmax
	character line*(*)
c------------------------------------------------------------------------
	integer i,nd
	real m,bb,sigm,sigb,chisq,q,xd(2),yd(2)
c
c  Extract the relevant data.
c
	nd = 0
	do i=1,n
	  if(mass1.le.mass(i).and.mass(i).le.mass2.and.
     *	     dmin.le.b(i).and.b(i).le.dmax)then
	    nd = nd + 1
	    w(nd) = 1
	    x(nd) = b(i)
	    y(nd) = d2l(i)
	  endif
	enddo
c
c  Fit with an line.
c
	line = ' '
	if(nd.gt.8)then
	  call lsf(.true.,nd,x,y,w,m,bb,sigm,sigb,chisq,q)
	  xd(1) = 1
	  yd(1) = m*xd(1) + bb
	  xd(2) = 4
	  yd(2) = m*xd(2) + bb
	  call pgline(2,xd,yd)
	  bb = 10**(0.5*(3*m + bb))
	  sigb = 10**(0.5*sigb)
	  if(sigb.lt.9.95)then
	    write(line,'(a,f4.1,a,f4.1,a,f5.2,a,f4.2,a,f5.2,a,f3.1,a)')
     *	    'Airmass',mass1,' to',mass2,' has \gb',m,'(',sigm,
     *	    ') and D\dL\u(1km)',bb,'(',sigb,') mm'
	  else
	    write(line,'(a,f4.1,a,f4.1,a,f5.2,a,f4.2,a,f5.2,a,f4.1,a)')
     *	    'Airmass',mass1,' to',mass2,' has \gb',m,'(',sigm,
     *	    ') and D\dL\u(1km)',bb,'(',sigb,') mm'
	  endif
	  call output(line)
	endif
c
	end
c************************************************************************
	subroutine getam(tno,mass,lambda)
c
	implicit none
	integer tno
	real mass,lambda
c
c  Return the current airmass and observing wavelength (in mm).
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision obsra,obsdec,ha,lst,latitude,sfreq
c
	call uvrdvrd(tno,'obsra',obsra,0.d0)
	call uvrdvrd(tno,'obsdec',obsdec,0.d0)
	call uvrdvrd(tno,'lst',lst,0.d0)
	call uvrdvrd(tno,'latitud',latitude,0.d0)
        ha = lst - obsra
	mass = 1.0/(sin(latitude)*sin(obsdec) +
     *      cos(latitude)*cos(obsdec)*cos(ha))
	call uvrdvrd(tno,'sfreq',sfreq,0.d0)
	lambda = 1000 * CMKS / (sfreq*1e9)
	end
c************************************************************************
	subroutine Setd(tno,d,nbase)
c
	implicit none
	integer tno,nbase
	real d(nbase)
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer nants,i,j,k
	character type
	logical updated
	double precision xyz(3*MAXANT)
c
	call uvprobvr(tno,'antpos',type,nants,updated)
	if(mod(nants,3).ne.0.or.nants.le.0)
     *	  call bug('f','Invalid size to antenna table')
	nants = nants / 3
	call uvgetvrd(tno,'antpos',xyz,3*nants)
c
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    d(k) = CMKS * 1e-9 * sqrt((xyz(j) - xyz(i))**2 +
     *		   (xyz(j+nants) - xyz(i+nants))**2 +
     *		   (xyz(j+2*nants) - xyz(i+2*nants))**2)
	  enddo
	enddo
c
	do j=k+1,nbase
	  d(j) = -1
	enddo
c
	end
c************************************************************************
	subroutine flushit(time,MAXBASE,ncount,S,S2,d,am,nfile,
     *			D2l,b,mass,file,w,MAXPNT,npnt,docal,dolog)
c
	implicit none
	integer MAXBASE,npnt,MAXPNT,nfile
	integer file(MAXPNT),ncount(MAXBASE)
	double precision S(MAXBASE),S2(MAXBASE),am(MAXBASE),time
	real d(MAXBASE),D2l(MAXPNT),b(MAXPNT),mass(MAXPNT)
	real w(MAXPNT)
	logical docal,dolog
c------------------------------------------------------------------------
	integer i,n0,ntmass,nd
	real m,bb,sigm,sigb,chisq,q
	character date*16,line*80
	real tmass
c
	tmass = 0
	ntmass = 0
	n0 = npnt
c
	do i=1,MAXBASE
	  if(ncount(i).gt.10)then
	    npnt = npnt + 1
	    if(npnt.gt.MAXPNT)call bug('f','Too many points')
	    if(docal)then
	      D2l(npnt) = log10(S2(i)/ncount(i))
	    else
	      D2l(npnt) = log10(S2(i)/ncount(i) - 
     *				(S(i)/ncount(i))*(S(i)/ncount(i)))
	    endif
	    b(npnt) = log10(d(i))
	    tmass = tmass + am(i)
	    ntmass = ntmass + ncount(i)
	    mass(npnt) = am(i)/ncount(i)
	    file(npnt) = nfile
	    w(npnt) = 1
	  endif
	  ncount(i) = 0
	enddo
c
	nd = npnt - n0
	if(nd.gt.8.and.dolog)then
	  tmass = tmass/ntmass
	  call lsf(.true.,nd,b(n0+1),D2l(n0+1),w(n0+1),
     *					m,bb,sigm,sigb,chisq,q)
	  bb = 10**(0.5*(3*m + bb))
	  sigb = 10**(0.5*sigb)
	  call julday(time,'H',date)
c				 time,mass,beta,sigbeta,D_L(1km),fac
	  write(line,'(a,5f7.3)')date,tmass,m,sigm,bb,sigb
	  call logwrit(line)
	endif
c
	end
