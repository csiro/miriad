c************************************************************************
	program dsdcal
	implicit none
c
c= dsdcal - This is used to calibrate ATCA mm data.
c: rjs
c+
c	DSDCAL is used to calibrate ATCA mm data. It attempts to deduce
c	the system temperature of the receiver, and to calibrate for
c	this and (optionally) atmospheric extinction.
c@ vis
c	Input visibility file. No default.
c@ out
c	The output visibility file, containing nominally Tsys calibrated
c	data. The default is to not generate this file.
c@ select
c	Sky dip data. The default is to treat all data as a sky dip.
c@ device
c	Plotting device to show the fit to the sky dip data. The default
c	is to not display a plot.
c@ tol
c	Time tolerance between the data and the DSD log file.
c@ dsdlog
c	The so-called DSD log file, which is an extra file containing
c	recordings of the digital synchronous demodulators. No default.
c@ pol
c	Either X or Y, giving the relevant polarisation. Only one can
c	be processed at a time.
c@ options
c	  atmcor  This option causes DSDCAL to correct the data for
c	          atmospheric attenuation, and to correct the system
c	          temperatures to "above atmosphere" values. The default
c	          is to not correct for these effects.
c--
c  History:
c    rjs  15mar01 Original version.
c    rjs  24apr01 Extract opacGet. Allow selecting by polarisation.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='DSDCAL version 1.0 24-Apr-01')
	include 'maxdim.h'
	include 'mirconst.h'
	integer MAXDSD,MAXSELS,ATANTS
	parameter(MAXDSD=10000,MAXSELS=200,ATANTS=6)
	integer ndsd,i,j,k,tIn,tOut,nselect,vhandle,npol,pol
	double precision times(MAXDSD),time
	real els(MAXDSD),dsd(MAXDSD,4),sels(MAXSELS)
	real temps(MAXDSD),presss(MAXDSD),humids(MAXDSD)
	real x(MAXDSD),y(MAXDSD),y2(MAXDSD)
	real tol,xhi,xlo,yhi,ylo,dsdon,dsdoff,freq,m(4),b(4)
	real factor(MAXDSD),tsky(MAXDSD),airmass(MAXDSD),dsds(MAXDSD,4)
	real systemp(ATANTS),t3,t4,t3t4,jyperk
	character device*64,vis*64,state*1,out*64,dsdlog*64
	character ct(4)*5
	logical atmcor
c
	integer NPOLTS
	parameter(NPOLTS=2)
	integer nout
	character polts(NPOLTS)*1,polt*1
c
	double precision preamble(5),sfreq(MAXCHAN)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	integer nchan
c
c  Externals.
c
	integer tinNext,pgBeg
	character itoaf*8
	logical uvvarUpd
c
	data polts/'x','y'/
c
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','An input dataset must be given')
	call selinput('select',sels,MAXSELS)
	call keya('device',device,' ')
	call keya('out',out,' ')
	if(device.eq.' '.and.out.eq.' ')call bug('f','No work to do')
	call keyd('tol',tol,4.0d0)
	if(tol.le.0)call bug('f','Invalid tolerance')
	call getopt(atmcor)
	call keya('dsdlog',dsdlog,' ')
	if(dsdlog.eq.' ')call bug('f','A DSD log file must be given')
	call keymatch('pol',NPOLTS,polts,1,polt,nout)
	if(nout.eq.0)polt = polts(1)
	call keyfin
c
c  Set the polt labels.
c
	if(polt.eq.'x')then
	  ct(1) = '3X(1)'
	  ct(2) = '3X(2)'
	  ct(3) = '4X(1)'
	  ct(4) = '4X(2)'
	else
	  ct(1) = '3Y(1)'
	  ct(2) = '3Y(2)'
	  ct(3) = '4Y(1)'
	  ct(4) = '4Y(2)'
	endif
c
c  Get all the DSD log data.
c
	ndsd = 0
	call tinOpen(dsdlog,'n')
	dowhile(tinNext().gt.0)
	  call tinSkip(1)
	  call tinGett(time,0.d0,'atime')
	  call tinGeta(state,' ')
	  if(state.eq.'t')then
	    ndsd = ndsd + 1
	    if(ndsd.gt.MAXDSD)call bug('f','Too many points')
	    times(ndsd) = time
	    call tinSkip(1)
	    call tinGetr(els(ndsd),0.)
	    els(ndsd) = PI/180.*els(ndsd)
	    do k=1,4
	      if(polt.eq.'y')call tinSkip(2)
	      call tinGetr(dsdon,0.)
	      call tinGetr(dsdoff,0.)
	      dsd(ndsd,k) = 0.5*(dsdon+dsdoff)
	      if(polt.eq.'x')call tinSkip(2)
	    enddo
	    call tinSkip(2)
	    call tinGetr(temps(ndsd),0.)
	    temps(ndsd) = temps(ndsd) + 273.15
	    call tinGetr(presss(ndsd),0.)
	    presss(ndsd) = 0.975*100*presss(ndsd)
	    call tinGetr(humids(ndsd),0.)
	    humids(ndsd) = 0.01*humids(ndsd)
	  endif
	enddo
	call tinClose
	call output('Number of DSD samples: '//itoaf(ndsd))
c
c  Open the visibility data.
c
	call uvopen(tIn,vis,'old')
	call uvset(tIn,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call selapply(tIn,sels,.true.)
	call uvselect(tIn,'and',0.d0,0.d0,.true.)
	if(polt.eq.'x')then
	  call uvselect(tIn,'polarization',-5.d0,0.d0,.true.)
	else
	  call uvselect(tIn,'polarization',-6.d0,0.d0,.true.)
	endif
c
	j = 1
	nselect = 0
	call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
	  time = preamble(4)
          dowhile(j.lt.ndsd.and.times(j).lt.time-tol/86400d0)
            j = j + 1
          enddo
          if(abs(time-times(j)).le.tol/86400d0)then
	    nselect = nselect + 1
	    if(nselect.gt.MAXDSD)call bug('f','Too many points')
	    call uvinfo(tIn,'sfreq',sfreq)
	    freq = sfreq(1)
	    do i=2,nchan
	      freq = freq + sfreq(i)
	    enddo
	    freq = freq / nchan
	    call opacGet(1,freq,els(j),temps(j),
     *	      presss(j),humids(j),factor(nselect),Tsky(nselect))
	    airmass(nselect) = 1/sin(els(j))
	    do k=1,4
	      dsds(nselect,k) = dsd(j,k)
	    enddo
	  endif
	  call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvclose(tIn)
	call output('Number of selected data points: '//itoaf(nselect))
c
c  Now determine the scale factors for the 4 channels.
c
	call output('Doing the median fitting')
	do k=1,4
	  call medfit(dsds(1,k),tsky,nselect,m(k),b(k),.true.)
	  write(*,'(a,a,a,f6.1)')'Channel ',ct(k),', Trec',-b(k)
	enddo
	call output('Completed median fitting')
c
c  Plot the first channel as a function of time
c
	if(device.ne.' ')then
	  time = int(times(1)-0.5d0) + 0.5d0
c
	  if(pgbeg(0,device,2,1).ne.1)
     *	    call bug('f','Failed to open device')
	  call pgscf(2)
	  call pgsci(1)
	  do k=1,4
	    do i=1,nselect
	      x(i) = airmass(i)
	      y(i) = m(k)*dsds(i,k)
	      y2(i) = tsky(i) - b(k)
	    enddo
c
	    call range(x,nselect,xlo,xhi)
	    call range(y,nselect,ylo,yhi)	  
c
            call pgenv(xlo,xhi,ylo,yhi,0,0)
            call pglab('Airmass [1/sin(el)]','Estimated T\dsys\u',
     *					'Channel '//ct(k))
            call pgpt(nselect,x,y,1)
	    call pgsci(2)
	    call pgpt(nselect,x,y2,1)
	    call pgsci(1)
	  enddo
	  call pgend
	endif
c
c  Now go through the old dataset and convert it across, writing a new
c  dataset.
c
	if(out.ne.' ')then
	  do i=1,ATANTS
	    systemp(i) = 0
	  enddo
c
	  call uvopen(tIn,vis,'old')
	  call uvset(tIn,'preamble','uvw/time/baseline',0,0.,0.,0.)
	  call uvvarIni(tIn,vhandle)
	  call uvvarSet(vhandle,'systemp')
	  call uvopen(tOut,out,'new')
	  call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	  call varInit(tIn,'channel')
	  call varOnit(tIn,tOut,'channel')
c
c  Create the output history.
c
	  call hdCopy(tIn,tOut,'history')
	  call hisOpen(tOut,'append')
	  call hisWrite(tOut,'DSDCAL: Miriad '//version)
	  call hisInput(tOut,'DSDCAL')
	  call hisClose(tOut)
c
	  call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	  j = 1
	  dowhile(nchan.gt.0)
	    call varCopy(tIn,tOut)
	    time = preamble(4)
            dowhile(j.lt.ndsd.and.times(j).lt.time-tol/86400d0)
              j = j + 1
            enddo
c
	    t3 = m(1)*dsd(j,1) * m(2)*dsd(j,2)
	    t3 = sqrt(abs(t3))
	    t4 = m(3)*dsd(j,3) * m(4)*dsd(j,4)
	    t4 = sqrt(abs(t4))
	    t3t4 = sqrt(t3*t4)
c
	    if(atmcor)then
	      call uvinfo(tIn,'sfreq',sfreq)
	      freq = sfreq(1)
	      do i=2,nchan
	        freq = freq + sfreq(i)
	      enddo
	      freq = freq / nchan
	      call opacGet(1,freq,els(j),temps(j),
     *	      presss(j),humids(j),factor,Tsky)
	      t3 = t3 / factor(1)
	      t4 = t4 / factor(1)
	      call uvputvrr(tOut,'tsky',tsky,1)
	      call uvputvrr(tOut,'trans',factor,1)
	    endif
c
	    call uvrdvrr(tIn,'jyperk',jyperk,1.0)
	    do i=1,nchan
	      data(i) = data(i) * jyperk*t3t4/500.0
	    enddo
	    call uvputvrr(tOut,'airmass',1.0/sin(els(j)),1)
c
	    if(uvvarUpd(vhandle))then
	      systemp(3) = t3
	      systemp(4) = t4
	      call uvputvrr(tOut,'systemp',systemp,ATANTS)
	    endif
	    call uvrdvri(tIn,'npol',npol,0)
	    call uvrdvri(tIn,'pol',pol,1)
	    if(npol.ge.1)then
	      call uvputvri(tOut,'npol',npol,1)
	      call uvputvri(tOut,'pol',pol,1)
	    endif
	    call uvwrite(tOut,preamble,data,flags,nchan)
	    call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvclose(tOut)
	  call uvclose(tIn)
	endif
c
        end
c************************************************************************
	subroutine getopt(atmcor)
c
	implicit none
	logical atmcor
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'atmcor  '/
	call options('options',opts,present,NOPTS)
	atmcor = present(1)
	end
c************************************************************************
	subroutine range(x,n,xlo,xhi)
c
	implicit none
	integer n
	real x(n),xlo,xhi
c------------------------------------------------------------------------
	integer i
	real xmin,xmax
c
	xmin = x(1)
	xmax = xmin
	do i=2,n
	  xmin = min(xmin,x(i))
	  xmax = max(xmax,x(i))
	enddo
	call pgrnge(xmin,xmax,xlo,xhi)
	end
