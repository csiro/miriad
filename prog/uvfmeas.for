c************************************************************************
	program uvfmeas
	implicit none
c
c= uvfmeas - Calculate the fluxes of a point source at the phase
c               centre.
c& jbs
c: uv analysis
c+
c	UVSPEC plots averaged spectra of a visibility dataset. Averaging can
c	be in both time and frequency
c@ vis
c	The name of the input uv data sets. Several can be given (wild
c	cards are supported). No default.
c@ select
c	The normal uv selection commands. The default is plot everything.
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	The default is all channels (or all wide channels if there are no
c	spectral channels).
c@ stokes
c	The Stokes/polarization types to be plotted. The default is to
c	plot those polarizations present in the input files.
c@ hann
c	Hanning smoothing width (an odd integer).  Smoothing is
c	applied after averaging. Default is 1 (no Hanning smoothing).
c@ offset
c	An offset (in arcsec) to shift the data. Positive values result in
c	the data center being shifted to the North and East. Two values
c	should be given, being the shift in the RA and DEC directions.
c	The default is 0,0 (i.e. no shift).
c@ order
c       The order of the highest term in the polynomial fit this task
c       should make to the spectrum it plots.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreviated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   'nocal'       Do not apply the gains file. By default, UVSPEC
c	                 applies the gains file in copying the data.
c	   'nopass'      Do not apply bandpass corrections. By default,
c	                 UVSPEC corrects for the bandpass shape if the
c	                 required information is available.
c	   'nopol'       Do not apply polarization corrections. By default
c	                 UVSPEC corrects for polarization cross-talk.
c          'log'         Do the spectral fitting in log space. By default
c                        the fitting is done with the raw values. The plots
c                        will still show the raw values when this option
c                        is used. This option will be overridden, and a
c                        warning displayed, if there are negative raw
c                        values that make it impossible to do a log fit.
c          'plotvec'     Use the vector average values for the plot and
c                        the fit. By default, the scalar average values 
c                        are used.
c          'uvhist'      Plot the uvdistance vs spectrally-corrected
c                        amplitudes after plotting the spectrum and fit.
c                        This will also show the histogram fit to the
c                        data used to estimate the usability of the source
c                        as a calibrator.
c          'plotfit'     Plot a user-specified fit over the spectrum. The
c                        coefficients of the fit are given with the fitp
c                        parameter.
c@ yrange
c	The min and max range along the y axis of the plots. The default
c	is to autoscale.
c@ device
c	PGPLOT plot device/type. No default.
c@ nxy
c	Number of plots in the x and y directions. The default is
c	determined from the number of plots that are requested.
c@ log
c	Log file into which the spectra are dumped in the order in which
c	they are plotted.  Really only useful if your plot is quite simple.
c@ fitp
c       The coefficients of a fit that you would like this task to
c       overplot onto the spectrum. The coefficients must relate to the
c       same type of fit (ie. log space or raw values) as the main fit
c       would use.
c
c$Id: uvfmeas.for,v 1.2 2013/09/19 04:54:35 ste616 Exp $
c--
c  History:
c    jbs  05jan12 Derived from uvspec.
c    jbs  09may12 First released source.
c  Bugs:
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
        integer maxco,MAXPOL,PolMin,PolMax,MAXPLT,MAXPNT
        parameter (maxco=15,MAXPOL=4,PolMin=-9,PolMax=4,MAXPLT=1024)
	parameter (MAXPNT=10000000)
c
	character version*80
	character uvflags*8,device*64,xaxis*12,yaxis*12,logf*64
	character xtitle*64,ytitle*64,cpoly*64,source*32,osource*32
	character line*132,PolCode*2
	logical nobase,avall,first,buffered,doflush,qfirst
	logical doshift,subpoly,dolog,dovec,douv,dopfit
	double precision interval,T0,T1,preamble(5),shift(2),lmn(3)
	double precision fluxr(MAXPOL,MAXCHAN),fluxi(MAXPOL,MAXCHAN)
	double precision amp(MAXPOL,MAXCHAN),amp2(MAXPOL,MAXCHAN)
	double precision rms2(MAXPOL,MAXCHAN),u,v
	double precision vecavgr,vecavgi,time0,tmin,tmax,tprev,tt
	integer tIn,vupd,poly,ncnt(MAXPOL,MAXCHAN),ipol,npol
	integer nxy(2),nchan,nread,nplot,PolIndx(PolMin:PolMax)
	integer p(MAXPOL),pp(MAXPOL),lmax,mnchan,vecavgn,scalavgn
	integer dnx,dny,tcm(2*MAXCHAN-2)
	real yrange(2),inttime,temp,scalamp(MAXCHAN),scalscat(MAXCHAN)
	real vecamp(MAXCHAN),vecpha(MAXCHAN),vecscat(MAXCHAN),sig2
	real work2(4*maxdim),weight(maxdim),fit(maxdim),serr
	real xrange(2),yp(MAXCHAN),scalavga,vecavgs,scalavgs
	real uvdist(MAXPNT),uvdistamp(MAXPNT),uvdistfreq(MAXPNT)
	real sexpect,qualn,qualp,plotfit(11),ufit(maxdim)
	real fitdiffsum,plfitx(maxdim)
	double precision x(2*MAXCHAN-2),xf(2*MAXCHAN-2)
	double precision xp(2*MAXCHAN-2),txf(2*MAXCHAN-2)
	double precision mx(2*MAXCHAN-2)
	complex data(MAXCHAN),vecaver(MAXCHAN)
	logical flags(MAXCHAN),fpresnt
	integer hann,ibin,i,j,nlines,t,plot(MAXPLT+1),nplts,k
	integer tncnt,chplot(MAXCHAN),ant1,ant2,bl,nants
	integer nuvdist,nachan
	real hc(maxco),hw(maxco),fitparams(11),fluxlines(2)
c
c  Externals.
c
	integer nextpow2
	logical uvDatOpn,uvVarUpd
	character PolsC2P*2
	character versan*80
c-----------------------------------------------------------------------
	version = versan ('uvfmeas',
     :                    '$Revision: 1.2 $',
     :                    '$Date: 2013/09/19 04:54:35 $')
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags,nobase,avall,dolog,dovec,douv,dopfit)
	call GetAxis(xaxis,yaxis)
	call uvDatInp('vis',uvflags)
	interval=99999.d0
        call keyi('hann',hann,1)
	call keya('device',device,' ')
	dnx=1
	dny=1
	if (douv) then
	   dny=2
	endif
	call keyi('nxy',nxy(1),dnx)
	call keyi('nxy',nxy(2),dny)
	call keyd('offset',shift(1),0.d0)
	call keyd('offset',shift(2),0.d0)
	call keyr('yrange',yrange(1),0.)
	call keyr('yrange',yrange(2),yrange(1)-1)
	do i=1,10
	   call keyr('fitp',plotfit(i),0.0)
	enddo
        call keya('log',logf,' ')
	call keya('order',cpoly,' ')
	if (cpoly.eq.' ') then
	   poly=-99
	else
	   read(cpoly, *, err=100) poly
	   if (cpoly(1:1).eq.'-') then
	      subpoly=.true.
	   else
	      subpoly=.false.
	   endif
	   goto 120
 100	   call bug('f', 'Order must be integer between -10 and 10')
 120	   poly=abs(poly)
	   if (poly.gt.10) goto 100
	endif
	call keyfin
c
c  Check the input parameters.
c
	if(interval.lt.0)call bug('f','Illegal value for interval')
	if(avall.and..not.nobase)
     *	  call bug('w','Option NOBASE being used because of AVALL')
	nobase = nobase.or.avall
        if (hann.lt.1 .or. hann.gt.maxco) call bug('f',
     *    'Illegal Hanning smoothing width')
c
c  Convert the shifts, and determine whether a shift is to be performed.
c
	shift(1) = pi/180/3600 * shift(1)
	shift(2) = pi/180/3600 * shift(2)
	doshift = abs(shift(1))+abs(shift(2)).gt.0
c
c  Various initialisation.
c
	ytitle = 'Amplitude (Jy)'
	call ucase(ytitle(1:1))
	interval = interval/(24.*60.)
	doflush = .false.
	buffered = .false.
	first = .true.
	qfirst = .true.
        ibin=0
	call BufIni
        if(hann.gt.1) call HCoeffs(hann,hc)
        if(logf.ne.' ') call LogOpen(logf,' ')
	do i=1,MAXPOL
	   do j=1,MAXCHAN
	      fluxr(i,j)=0
	      fluxi(i,j)=0
	      amp(i,j)=0
	      amp2(i,j)=0
	      ncnt(i,j)=0
	      rms2(i,j)=0
	   enddo
	enddo
	do j=1,MAXCHAN
	   chplot(i)=0
	enddo
	npol=0
	nants=0
	do i=PolMin,PolMax
	   PolIndx(i)=0
	enddo
	lmax=0
	mnchan=0
	nachan=0
	nuvdist=1
c
c  Open the input file(s).
c
	dowhile(uvDatOpn(tIn))
c
c No comment.
c
	  call uvVarIni(tIn,vupd)
	  call uvVarSet(vupd,'dra')
	  call uvVarSet(vupd,'ddec')
	  call uvVarSet(vupd,'source')
c
c  Loop over the data.
c
	  call uvdatrd(preamble,data,flags,maxchan,nread)
c
c  Determine the source name
c
	  call uvrdvra(tIn,'source',source,' ')
	  osource=source
	  nplot = nread
	  if(doshift)then
	    call coInit(tIn)
	    call coLMN(tIn,'ow/ow',shift,lmn)
	    call coFin(tIn)
	  endif
	  nchan = nread
	  if (nchan.gt.mnchan) then
	     mnchan=nchan
	  endif
	  T1 = preamble(4)
	  T0 = T1
	  dowhile(nread.gt.0)
c
c  Shift the data if needed.
c
	    if(doshift)call ShiftIt(tIn,preamble,data,nchan,lmn)
c
c  Determine the polarisation.
c
	    call uvDatGti('pol',ipol)
	    if (PolIndx(ipol).eq.0) then
	       npol=npol+1
	       PolIndx(ipol)=npol
	    endif
	    ipol=PolIndx(ipol)
c
c  Get the rms noise.
c
	    call uvDatGtr('variance',sig2)	    
c
c  Accumulate the data for the flux measurement.
c
	    call uvinfo(tIn,'sfreq',txf)
c
c  Make it possible to plot over a large frequency range.
c
	    do i=1,nchan
	       fpresnt=.false.
	       do j=1,nachan
		  if (txf(i).eq.xf(j)) then
		     fpresnt=.true.
		     tcm(i)=j
		     exit
		  endif
	       enddo
	       if (fpresnt.eqv..false.) then
		  nachan=nachan+1
		  xf(nachan)=txf(i)
		  tcm(i)=nachan
	       endif
	    enddo
c
c  Calculate the required quantities.
c
	    do i=1,nchan
	       if (flags(i)) then
		  chplot(tcm(i))=1
		  fluxr(ipol,tcm(i))=fluxr(ipol,tcm(i))+real(data(i))
		  fluxi(ipol,tcm(i))=fluxi(ipol,tcm(i))+aimag(data(i))
		  rms2(ipol,tcm(i)) = rms2(ipol,tcm(i)) + sig2
		  temp=abs(data(i))
		  amp(ipol,tcm(i))=amp(ipol,tcm(i))+temp
		  amp2(ipol,tcm(i))=amp2(ipol,tcm(i))+temp*temp
		  ncnt(ipol,tcm(i))=ncnt(ipol,tcm(i))+1
		  u=preamble(1)/1000.0
		  v=preamble(2)/1000.0
c		  if (u.ne.0.0.or.v.ne.0.0) then
		  uvdist(nuvdist)=real(sqrt(u*u+v*v)*txf(i)/txf(1))
c		  uvdist(nuvdist)=real(sqrt(u*u+v*v))
c		     write(line,'(1f10.3)') uvdist(nuvdist)
c		     call output(line)
		  uvdistamp(nuvdist)=real(data(i))
		  uvdistfreq(nuvdist)=real(txf(i))
c		     if (uvdist(nuvdist).gt.0.0) then
		  nuvdist=nuvdist+1
		  if (nuvdist.ge.MAXPNT) then
		     call bug('f','Too many points!')
		  endif
c		     endif
c		  endif
	       endif
	    enddo
	    nuvdist=nuvdist-1
c
c  Determine if we need to flush out the averaged data.
c
c	    doflush = uvVarUpd(vupd)
c	    doflush = nread.ne.nchan
c	    T0 = min(preamble(4),T0)
c	    T1 = max(preamble(4),T1)
c	    doflush = (doflush.or.T1-T0.gt.interval).and.buffered
c
c  Pull the chain and flush out and plot the accumulated data
c  in the case of time averaging.
c
c	    if(doflush)then
c	      call BufFlush(nobase,hann,hc,hw,first,
c     *	        device,x,nplot,xtitle,ytitle,nxy,yrange,logf,poly,
c     *          subpoly,xrange,dolog)
c	      T0 = preamble(4)
c	      T1 = T0
c	      buffered = .false.
c	    endif
c
c  Accumulate more data, if we are time averaging.
c
	    call GetXAxis(tIn,xaxis,xtitle,mx,nplot)
	    do i=1,nchan
	       x(tcm(i))=mx(i)
	    enddo
c	    if(avall)preamble(5) = 257
c	    call uvrdvrr(tIn,'inttime',inttime,0.)
c	    call BufAcc(preamble,inttime,data,flags,
c     *        nread,ibin)
c	    buffered = .true.
	    nchan = nread
c
c  Keep on going. Read in another record.
c
	    call uvDatRd(preamble,data,flags,maxchan,nread)
	  enddo
c
c  Flush out and plot anything remaining.
c
c	  if(buffered)then
c	    call BufFlush(nobase,hann,hc,hw,first,
c     *	      device,x,nplot,xtitle,ytitle,nxy,yrange,logf,poly,
c     *        subpoly,xrange,dolog)
c	    buffered = .false.
c	  endif
	  call uvDatCls
	enddo
c
c
c  Determine the order that we will print the polarisations out in.
c
	npol = 0
	do j=PolMin,PolMax
	  if(PolIndx(j).gt.0)then
	    npol = npol + 1
	    p(npol) = j
	    pp(npol) = PolIndx(j)
	    do i=npol,2,-1
	      if(abs(p(i)).lt.abs(p(i-1)))then
		t = p(i)
		p(i) = p(i-1)
		p(i-1) = t
		t = pp(i)
		pp(i) = pp(i-1)
		pp(i-1) = t
	      endif
	    enddo
	  endif
	enddo
c
c  Print out the results.
c
	call PltIni(device,1,nxy)
	source = osource
	write(line,'(a,a)') 'Source: ', source
	call output(line)
	do i=1,npol
	   ipol = pp(i)
	   PolCode = PolsC2P(p(i))
	   write(line,'(a,a)') 'Stokes ',PolCode
	   call output(line)
	   sig2=0
	   vecavgr=0.0d0
	   vecavgi=0.0d0
	   vecavgs=0.0d0
	   vecavgn=0
	   scalavga=0.0
	   scalavgn=0
	   scalavgs=0.0
	   tncnt=0
	   do j=1,MAXCHAN
	      if(ncnt(ipol,j).gt.0)then
		 fluxr(ipol,j) = fluxr(ipol,j) / ncnt(ipol,j)
		 fluxi(ipol,j) = fluxi(ipol,j) / ncnt(ipol,j)
		 tncnt=tncnt+ncnt(ipol,j)
		 vecavgr=vecavgr+fluxr(ipol,j)
		 vecavgi=vecavgi+fluxi(ipol,j)
		 vecavgn=vecavgn+1
		 vecaver(j)= cmplx(real(fluxr(ipol,j)),
     *			          real(fluxi(ipol,j)))
		 vecscat(j)= amp2(ipol,j) / (2*ncnt(ipol,j))
     *			   - 0.5*(fluxr(ipol,j)**2+fluxi(ipol,j)**2)
		 vecscat(j)= sqrt(abs(vecscat(j)))
		 vecavgs=vecavgs+vecscat(j)
		 call amphase(vecaver(j),vecamp(j),vecpha(j))
		 scalamp(j) = amp(ipol,j) / ncnt(ipol,j)
		 scalavga=scalavga+scalamp(j)
		 scalavgn=scalavgn+1
		 scalscat(j)= amp2(ipol,j) / ncnt(ipol,j)
     *			   - (amp(ipol,j) / ncnt(ipol,j))**2
		 scalscat(j)= sqrt(abs(scalscat(j)))
		 scalavgs=scalavgs+scalscat(j)
		 sig2 =sig2+sqrt(rms2(ipol,j)/ncnt(ipol,j))
		 source = ' '
c		 nlines = nlines + 1
	      endif
	   enddo
	   vecavgr=vecavgr/real(vecavgn)
	   vecavgi=vecavgi/real(vecavgn)
	   vecavgs=vecavgs/sqrt(real(tncnt))
	   scalavga=scalavga/real(scalavgn)
	   scalavgs=scalavgs/sqrt(real(tncnt))
	   fluxlines(1)=real(vecavgr)
	   fluxlines(2)=real(scalavga)
c  Do a fit.
	   write(line,'(a,a,1pe11.3,a,1pe11.3)')
     *       'Vector Average ','Amplitude: ',vecavgr,
     *       '         Phase: ',vecavgi
	   call output(line)
	   write(line,'(a,1pe11.3)') '             Uncertainty: ',
     *       vecavgs
	   call output(line)
	   write(line,'(a,a,1pe11.3,a,1pe11.3)') 
     *       'Scalar Average ','Amplitude: ',scalavga,
     *       '   Uncertainty: ',scalavgs
	   call output(line)
	   nchan=0
c	   do j=1,mnchan
	   do j=1,nachan
	      if (chplot(j).eq.1) then
		 nchan=nchan+1
		 xp(nchan)=x(j)
		 if (dovec) then
		    yp(nchan)=fluxr(i,j)
		 else
		    yp(nchan)=scalamp(j)
		 endif
	      endif
	   enddo
	   plot(1)=1
	   plot(2)=nchan
	   nplts=1
	   if (poly.gt.0) then
	      call polyfit(poly,nchan,xp,work2,weight,yp,fit,serr,dolog,
     *          fitparams,dopfit,plotfit,ufit,plfitx)
	      if (dovec) then
		 call output('Vector Average Fit Coefficients:')
	      else
		 call output('Scalar Average Fit Coefficients:')
	      endif
	      if (dolog) then
		 write(line,'(a,1pe11.3)') ' log S = ',fitparams(1)
	      else
		 write(line,'(a,1pe11.3)') '     S = ',fitparams(1)		 
	      endif
	      call output(line)
	      do j=2,poly+1
		 if (dolog) then
		    write(line,'(a,1pe11.3,a,i2)')
     *              '       + ',fitparams(j),' x (log f)^',j-1
		 else
		    write(line,'(a,1pe11.3,a,i2)')
     *              '       + ',fitparams(j),' x f^',j-1
		 endif
		 call output(line)
	      enddo
	      write(line,'(a,1pe11.3)') 'Scatter around fit: ',serr
	      call output(line)
c	      write(line,'(a,1pe11.3)')
c     *          'Scatter for single visibility: ',
c     *          (serr*sqrt(real(nuvdist)))
c	      call output(line)
	   endif
	   do j=1,nuvdist
c	      uvdistfreq(j)=xf(int(uvdistfreq(j)))
c	      uvdist(j)=uvdist(j)*uvdistfreq(j)/xf(1)
	      if (poly.gt.0) then
		 sexpect=fitparams(1)
		 do k=2,poly+1
		    if (dolog) then
		       sexpect=sexpect+fitparams(k)*
     *                  (log10(uvdistfreq(j)))**(k-1)
		    else
		       sexpect=sexpect+fitparams(k)*uvdistfreq(j)**(k-1)
		    endif
		 enddo
		 if (dolog) then
		    sexpect=10**(sexpect)
		 endif
		 uvdistamp(j)=uvdistamp(j)-real(sexpect)
	      endif
c	      if (uvdist(j).le.0.0) then
c		 write(line,'(1pe11.3,1pe11.3)') uvdist(j),uvdistamp(j)
c		 call output(line)
c	      endif
	   enddo
c
	   if (dopfit.and.poly.gt.0) then
c  Calculate the average error with the supplied fit.
	      fitdiffsum=0.0
	      do j=1,nchan
		 fitdiffsum = fitdiffsum + abs(fit(i) - ufit(i)) /
     *               ufit(i)
	      enddo
	      fitdiffsum = fitdiffsum / nchan
	      write(line,'(a,1pe11.3)') 'Average fit error: ',fitdiffsum
	      call output(line)
	   endif
c
	   call SetAxisD(xp,nchan,xrange)
	   call Plotit(nchan,xp,yp,xrange,yrange,plot,
     *         nplts,xtitle,ytitle,0,dble(0.),real(0.),p,npol,hann,hc,
     *         hw,logf,MAXPNT,poly,fit,fluxlines,2,i,uvdist,uvdistamp,
     *         nuvdist,qualn,qualp,douv,dopfit,ufit,plfitx)
	   write(line,'(a,1pe11.3,a,1pe11.3)') 
     *      'Calibrator quality: value = ',qualn,' ratio = ',qualp
	   call output(line)
	   call output('---------------------------------------------'//
     *		'-----------------------------------')
	enddo
c
c	if(nlines.eq.0)call bug('f','No valid data found')
c
c	if(first)call bug('f','Nothing to plot')
	if(logf.ne.' ') call LogClose
	call pgend
	end
c************************************************************************
	subroutine ShiftIt(tIn,uvw,data,nchan,lmn)
c
	implicit none
	integer tIn,nchan
	double precision uvw(3)
	double precision lmn(3)
	complex data(nchan)
c
c  Shift the data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	double precision sfreq(MAXCHAN)
	real theta,theta0
	complex w
	integer i
c
c  Get the sky frequency.
c
	call uvinfo(tIn,'sfreq',sfreq)
c
c  Shift the data.
c
	theta0 = -2*pi * (uvw(1)*lmn(1) + uvw(2)*lmn(2) + 
     *			  uvw(3)*(lmn(3)-1))
	do i=1,nchan
	  theta = theta0 * sfreq(i)
	  w = cmplx(cos(theta),sin(theta))
	  data(i) = w * data(i)
	enddo
c	
	end
c************************************************************************
	subroutine GetXAxis(tIn,xaxis,xtitle,x,nchan)
c
	implicit none
	integer tIn,nchan
	character xaxis*(*),xtitle*(*)
	double precision x(nchan)
c
c  Determine the X axis coordinates for each channel.
c
c  Input:
c    tIn
c    xaxis
c    nchan
c  Output:
c    x
c------------------------------------------------------------------------
	integer VELO
	parameter(VELO=3)
c
	integer i,i0
	double precision data(6),start,step
	character vel*32
c
c  Externals.
c
	integer len1
c
	if(xaxis.eq.'channel')then
	  call uvinfo(tIn,'line',data)
	  start = data(3)
	  if(nint(data(1)).ne.VELO)start = start + 0.5*(data(4)-1)
	  step = data(5)
	  do i=1,nchan
	    x(i) = start + (i-1)*step
	  enddo
	  if(nint(data(1)).eq.VELO)then
	    call VelSys(tIn,vel,'radio')
	    xtitle = 'Velocity Channels('//vel(1:len1(vel))//') (km/s)'
	  else
	    xtitle = 'Channels'
	  endif
	else if(xaxis.eq.'velocity')then
	  call VelSys(tIn,vel,'radio')
	  xtitle = 'Velocity('//vel(1:len1(vel))//') (km/s)'
	  call uvinfo(tIn,'velocity',x)
	else if(xaxis.eq.'felocity')then
	  call VelSys(tIn,vel,'optical')
	  xtitle = 'Velocity('//vel(1:len1(vel))//') (km/s)'
	  call uvinfo(tIn,'felocity',x)
	else if(xaxis.eq.'frequency')then
	  xtitle = 'Frequency (GHz)'
	  call uvinfo(tIn,'sfreq',x)
	else if(xaxis.eq.'dfrequency')then
	  xtitle = 'Doppler-Corrected Frequency (GHz)'
	  call uvinfo(tIn,'frequency',x)
	else if(xaxis.eq.'lag')then
	  i0 = -nchan/2
	  do i=1,nchan
	    x(i) = i0
	    i0 = i0 + 1
	  enddo
	  xtitle = 'Lag Number'
	else
	  call bug('f','Unrecognised xaxis')
	endif
	end
c************************************************************************
	subroutine VelSys(tIn,vel,type)
c
	implicit none
	integer tIn
	character vel*(*),type*(*)
c
c------------------------------------------------------------------------
	character veltype*32
c
	call uvrdvra(tIn,'veltype',veltype,'VELO-LSR')
	if(veltype(6:8).eq.'LSR')then
	  vel = type//',LSR'
	else if(veltype(6:8).eq.'HEL')then
	  vel = type//',Barycentric'
	else if(veltype(6:8).eq.'OBS')then
	  vel = type//',Topocentric'
	else
	  vel = type//',unknown'
	endif
	end
c************************************************************************
	subroutine GetAxis(xaxis,yaxis)
c
	implicit none
	character xaxis*(*),yaxis*(*)
c
c  Determine the X and Y axis to plot.
c
c  Output:
c    xaxis
c    yaxis
c------------------------------------------------------------------------
	integer NX,NY
	parameter(NX=6,NY=4)
c
	integer n
	character xaxes(NX)*10,yaxes(NY)*9
	data xaxes/'channel   ','frequency ','velocity  ','felocity  ',
     *		   'lag       ','dfrequency'/
	data yaxes/'amplitude','phase    ','real     ','imaginary'/
c
	xaxis = xaxes(2)
	yaxis = yaxes(1)
	end
c************************************************************************
	subroutine GetOpt(uvflags,nobase,avall,dolog,dovec,douv,dopfit)
c
	implicit none
        logical nobase,avall,dolog,dovec,douv,dopfit
	character uvflags*(*)
c
c  Determine the flags to pass to the uvdat routines.
c
c  Output:
c    uvflags	Flags to pass to the uvdat routines.
c    nobase
c    avall
c    dolog
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=7)
	character opts(nopts)*9
	logical present(nopts),docal,dopol,dopass
	data opts/'nocal    ','nopol    ','nopass   ','log      ',
     *            'plotvec  ','uvhist   ','plotfit  '/
c
	call options('options',opts,present,nopts)
	docal = .not.present(1)
	dopol = .not.present(2)
	dopass= .not.present(3)
	dolog=present(4)
	dovec=present(5)
	douv=present(6)
	dopfit=present(7)
	nobase=.true.
	avall=.true.
	uvflags = 'dswl3'
	if(dopass)uvflags(6:6) = 'f'
	if(dopol) uvflags(7:7) = 'e'
	if(docal) uvflags(8:8) = 'c'
	end
c************************************************************************
	subroutine BufIni
	implicit none
c
c  Initialise the routines which do the buffering and averaging of
c  the visibility data.
c  All the buffering/averaging is performed in arrays stored in a
c  common block.
c
c------------------------------------------------------------------------
	include 'uvspec.h'
	free = 1
	mbase = 0
	end
c************************************************************************
	subroutine BufFlush(nobase,hann,hc,hw,
     *	        first,device,x,n,xtitle,ytitle,nxy,yrange,logf,
     *          poly,subpoly,xrange,dolog)
c
	implicit none
	logical nobase,first,subpoly,dolog
	character device*(*),xtitle*(*),ytitle*(*),logf*(*)
	integer n,nxy(2),hann,poly
	real yrange(2),hc(*),hw(*),xrange(*)
	double precision x(n)
c
c  Plot the averaged data. On the first time through, also initialise the
c  plot device.
c
c------------------------------------------------------------------------
	include 'uvspec.h'
	integer PolMin,PolMax
	parameter(PolMin=-8,PolMax=4)
	integer MAXPLT,MAXPNT
	parameter(MAXPNT=1000000,MAXPLT=1024)
        double precision xp(MAXPNT)
	real inttime,yp(MAXPNT),work2(4*maxdim),weight(maxdim)
	real fit(maxdim),serr,fitp(MAXPNT),temp
	integer plot(MAXPLT+1)
	double precision time
	integer i,j,ngood,ng,ntime,npnts,nplts,nprev,p,k
	logical Hit(PolMin:PolMax)
	integer npol,pol(MAXPOL)
c
c  Determine the number of good baselines.
c
	ngood = 0
	do j=1,mbase
	  if(cnt(j).gt.0)ngood = ngood + 1
	enddo
	if(ngood.le.0)return
c
c  Initialise the plot device, if this is the first time through.
c
	ng = ngood
	if(nobase) ng = 1
	if(first)call PltIni(device,ng,nxy)
	first = .false.
c
c  Autoscale the X axis.
c
	call SetAxisD(x,n,xrange,dolog)
c
c  Now loop through the good baselines, plotting them.
c
	inttime = 0
	time = 0
	ntime = 0
	npnts = 0
	nplts = 0
	npol = 0
	do i=PolMin,PolMax
	  Hit(i) = .false.
	enddo
c
	do j=1,mbase
	  if(cnt(j).gt.0)then
	    inttime = inttime + preamble(6,j)
	    time = time + preamble(4,j)
	    ntime = ntime + cnt(j)
c
c  Average the data in each polarisation. If there is only one scan in the
c  average, not bother to average it.
c
	    do i=1,npols(j)
	      if(.not.Hit(pols(i,j)))then
		npol = npol + 1
		if(npol.gt.MAXPOL)call bug('f','Too many polarisations')
		pol(npol) = pols(i,j)
		Hit(Pols(i,j)) = .true.
	      endif
	      nprev = npnts
	      p = pnt(i,j)
	      if(cntp(i,j).ge.1)then
		 call VisExt(x,buf(p),buf2(p),bufr(p),count(p),
     *		    nchan(i,j),
     *		    xp,yp,MAXPNT,npnts)
	      endif
c
c  Did we find another plot.
c
	      if(npnts.gt.nprev)then
		nplts = nplts + 1
		if(nplts.gt.MAXPLT)call bug('f',
     *		  'Buffer overflow(plots), when accumulating plots')
		plot(nplts) = nprev + 1
		plot(nplts+1) = npnts + 1
	      endif
	    enddo
	    if(.not.nobase.and.npnts.gt.0)then
	      npol = 0
	      do i=PolMin,PolMax
		Hit(i) = .false.
	      enddo
	      npnts = 0
	      nplts = 0
	      ntime = 0
	      time = 0
	      inttime = 0
	    endif
	  endif
	enddo
c
c  Reset the counters.
c
	free = 1
	mbase = 0
c
	end
c************************************************************************
	subroutine VisExt(x,buf,buf2,bufr,count,nchan,
     *		    xp,yp,MAXPNT,npnts)
c
	implicit none
	integer nchan,npnts,MAXPNT,count(nchan)
	real buf2(nchan),bufr(nchan),yp(MAXPNT)
	double precision x(nchan),xp(MAXPNT)
	complex buf(nchan)
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer k
	real temp
	complex ctemp
c
c  Externals
c
        character*8 itoaf
c
	do k=1,nchan
	  if(count(k).gt.0)then
	     temp = abs(buf(k)) / count(k)
	     npnts = npnts + 1
	     if(npnts.gt.MAXPNT)call bug('f',
     *	      'Buffer overflow('//itoaf(npnts)//
     *        '> MAXPNT), when accumulating plots')
	     xp(npnts) = x(k)
	     yp(npnts) = temp
	  endif
	enddo
c
	end
c************************************************************************
	subroutine BufAcc(preambl,inttime,data,flags,nread,
     *     ibin)
c
	implicit none
	integer nread,ibin
	double precision preambl(5)
	real inttime
	complex data(nread)
	logical flags(nread)
c
c  This accumulates the visibility data. The accumulated data is left
c  in common.
c
c  Input
c    ibin       Bin number of current block of data (or 0)
c  Input/Output:
c    preambl	Preamble. Destroyed on output.
c    data	The correlation data to be averaged. Destroyed on output.
c    flags	The data flags.
c    nread	The number of channels.
c------------------------------------------------------------------------
	include 'uvspec.h'
	integer i,i1,i2,p,bl,pol
	real t
	logical ok,doflag
c
c  Does this spectrum contain some good data.
c
	ok = .false.
	doflag=.false.
	if(.not.ok)then
	  do i=1,nread
	    ok = ok.or.(flags(i).neqv.doflag)
	  enddo
	endif
	if(.not.ok)return
c
c  Determine the baseline number.
c
	call BasAnt(preambl(5),i1,i2)
	bl = (i2*(i2-1))/2 + i1
c
c  Zero up to, and including, this baseline.
c
	do i=mbase+1,bl
	  cnt(i) = 0
	enddo
	mbase = max(mbase,bl)
c
c  Add in this visibility.
c
	if(cnt(bl).eq.0)then
	  cnt(bl) = 1
	  npols(bl) = 0
	  preamble(1,bl) = preambl(1)
	  preamble(2,bl) = preambl(2)
	  preamble(3,bl) = preambl(3)
	  preamble(4,bl) = preambl(4)
	  preamble(5,bl) = preambl(5)
	  preamble(6,bl) = inttime
	else
	  cnt(bl) = cnt(bl) + 1
	  preamble(1,bl) = preamble(1,bl) + preambl(1)
	  preamble(2,bl) = preamble(2,bl) + preambl(2)
	  preamble(3,bl) = preamble(3,bl) + preambl(3)
	  preamble(4,bl) = preamble(4,bl) + preambl(4)
	  preamble(5,bl) = preamble(5,bl) + preambl(5)
	  preamble(6,bl) = preamble(6,bl) + inttime
	endif
c
c  Determine the polarisation.
c
	call uvDatGti('pol',pol)
	p = 0
	do i=1,npols(bl)
	  if(pols(i,bl).eq.pol) p = i
	enddo
c
c  A new baseline. Set up the description of it.
c
	if(p.eq.0)then
	  npols(bl) = npols(bl) + 1
	  p = npols(bl)
	  if(p.gt.MAXPOL) call bug('f',
     *	    'Too many polarizations, in BufAcc')
	  pols(p,bl) = pol
	  cntp(p,bl) = 1
	  nchan(p,bl) = nread
	  pnt(p,bl) = free
	  free = free + nread
	  if(free.gt.MAXAVER)call bug('f',
     *	    'Too much data to accumulate, in BufAcc')
c
c  Copy across the new data.
c
	  p = pnt(p,bl) - 1
	  do i=1,nread
	    if(doflag.neqv.flags(i))then
	      if(ibin.eq.0.or.ibin.eq.2) buf(i+p) = data(i)
	      if (ibin.eq.1) buf(i+p) = -data(i)
              t = abs(data(i))
              bufr(i+p) = t
	      buf2(i+p) = t*t
	      count(i+p) = 1
	    else
	      buf(i+p) = (0.0,0.0)
              bufr(i+p) = 0.0
	      buf2(i+p) = 0.0
	      count(i+p) = 0
	    endif
	  enddo
c
c  Else accumulate new data for old baseline.
c
	else
	  cntp(p,bl) = cntp(p,bl) + 1
	  nread = min(nread,nchan(p,bl))
	  nchan(p,bl) = nread
	  p = pnt(p,bl) - 1
	  do i=1,nread
	    if(doflag.neqv.flags(i))then
	      t = abs(data(i))
              if (ibin.eq.1) buf(i+p) = buf(i+p) - data(i)
              if (ibin.eq.0.or.ibin.eq.2) buf(i+p) = buf(i+p) + data(i)
              bufr(i+p) = bufr(i+p) + t
	      buf2(i+p) = buf2(i+p) + t*t
	      count(i+p) = count(i+p) + 1
	    endif
	  enddo
	endif
c
	end
c************************************************************************
	subroutine PltIni(device,ngood,nxy)
c
	implicit none
	character device*(*)
	integer ngood,nxy(2)
c
c  Initialise the plot device.
c
c------------------------------------------------------------------------
	integer nx,ny
	character hard*4
	integer hlen
c
c  Externals.
c
	integer pgbeg
c
c  Determine the default plots per page in X and Y
c
	nx = nxy(1)
	ny = nxy(2)
	if(nx.le.0.or.ny.le.0)then
	  nx = 2
	  if(mod(ngood,3).eq.0)nx = 3
	  ny = 2
	  if(mod(ngood,9).eq.0)ny = 2
	endif
	if(pgbeg(0,device,nx,ny).ne.1)then
	  call pgldev
	  call bug('f','Error opening graphics device')
	endif
	call pgsch(real(max(nx,ny))**0.4)
	call pgqinf('hardcopy',hard,hlen)
	if(hard.eq.'YES')call pgscf(2)
	end
c************************************************************************
	subroutine SetAxisD(data,npnts,range)
c
	implicit none
	integer npnts
	real range(2)
	double precision data(npnts)
c
c  Determine the range, for autoscaling, or the data.
c
c  Input:
c    data
c    npnts
c  Output:
c    range
c------------------------------------------------------------------------
	double precision dmax,dmin
	integer i
c
	dmax = data(1)
	dmin = dmax
	do i=2,npnts
	  dmax = max(data(i),dmax)
	  dmin = min(data(i),dmin)
	enddo
c
	call pgrnge(real(dmin),real(dmax),range(1),range(2))
c
	end
c************************************************************************
	subroutine SetAxisR(data,npnts,range)
c
	implicit none
	integer npnts
	real range(2)
	real data(npnts)
c
c  Determine the range, for autoscaling, or the data.
c
c  Input:
c    data
c    npnts
c  Output:
c    range
c------------------------------------------------------------------------
	real dmax,dmin,delta,maxv
	integer i
c
	dmax = data(1)
	dmin = dmax
	do i=2,npnts
	  dmax = max(data(i),dmax)
	  dmin = min(data(i),dmin)
	enddo
c
	delta = 0.05*(dmax - dmin)
	maxv = max(abs(dmax),abs(dmin))
	if(delta.le.1e-4*maxv) delta = 0.01*maxv
	if(delta.eq.0) delta = 1
	range(1) = dmin - delta
	range(2) = dmax + delta
	end
c************************************************************************
	subroutine Plotit(npnts,xp,yp,xrange,yrange,
     *		  plot,nplts,xtitle,ytitle,bl,time,inttime,
     *		  pol,npol,hann,hc,hw,logf,MAXPNT,poly,fit,
     *            fluxlines,nflux,wpol,uvd,uva,nuvd,qualn,
     *            qualp,plotuv,dopfit,ufit,plfitx)
c
	implicit none
	integer npnts,bl,nplts,plot(*),npol,pol(*),hann,MAXPNT
	integer poly,nflux,wpol,nuvd
	double precision time,xp(*)
        real x(MAXPNT),fit(*),fluxlines(*)
	real inttime,hc(*),hw(*),xrange(2),yrange(2),yp(*)
	real uvd(*),uva(*),qualn,qualp,ufit(*),plfitx(*)
	character xtitle*(*),ytitle*(*),logf*(*)
	logical plotuv,dopfit
c
c  Draw a plot
c------------------------------------------------------------------------
	integer NCOL,nd
	parameter(NCOL=12,nd=100)
        real TOL1,TOL2
        parameter(TOL1=1.e-5,TOL2=5.e-7)
	integer hr,mins,sec,b1,b2,l,i,j,k,xl,yl,symbol,lp,lt
	character title*64,baseline*12,tau*16,line*80
	character pollab*32,xtitle2*80
	double precision T0
	real xranged(2),yranged(2),xoff,delta1,delta2
	real xlen,ylen,xloc,size,linex(2),liney(2),dint
	real qualat,qualt
	integer k1,k2,bdn
	real bda(nd),bdc(nd)
c
c  Externals.
c
	integer len1
	character itoaf*4,PolsC2P*2
c
c
        symbol = 17
c
	call pgpage
	call pgvstd
c
        xoff = 0
        delta1 = abs((xrange(2)-xrange(1))/max(xrange(1),xrange(2)))
        delta2 = delta1
        if (npnts.gt.0) delta2=delta1/npnts*nplts
c
c  Check for potential axis labeling and plot accuracy issues, use offset 
c
        if (delta1.lt.TOL1.or.delta2.lt.TOL2) then
          xoff=min(xrange(1),xrange(2))
          xoff=int(xoff*1000)/1000.0
          xranged(1)=xrange(1)-xoff
          xranged(2)=xrange(2)-xoff
        else
          xranged(1)=xrange(1)
          xranged(2)=xrange(2)
        endif
        do i=1,npnts
          x(i)=xp(i)-xoff
        enddo
	if(yrange(2).le.yrange(1))then
	  call SetAxisR(yp,npnts,yranged)
	  call pgswin(xranged(1),xranged(2),yranged(1),yranged(2))
	else
	  call pgswin(xranged(1),xranged(2),yrange(1),yrange(2))
	endif
	call pgbox('BCNST',0.,0.,'BCNST',0.,0.)
	do i=1,nplts
	  call pgsci(mod(i-1,NCOL)+1)
	    if (hann.gt.1) call hannsm(hann,hc,plot(i+1)-plot(i),
     *                  yp(plot(i)),hw)
	    call pghline(plot(i+1)-plot(i),x(plot(i)),yp(plot(i)),2.0)
c  Plot the fit if we've done it.
	  if (poly.ge.0) then
	     call pgsci(mod(i-1,NCOL)+2)
	     call pgline(plot(i+1)-plot(i),plfitx(plot(i)),
     *                   fit(plot(i)))
	     call pgmtxt('T',0.6,0.0,0.0,'Fit Line')
	  endif
	  if (dopfit) then
	     call pgsci(mod(i-1,NCOL)+2)
	     call pgsls(2)
	     call pgline(plot(i+1)-plot(i),plfitx(plot(i)),
     *                   ufit(plot(i)))
	     call pgsls(1)
	  endif
          if (logf.ne.' ') then
  	    do j = 1, plot(i+1)-plot(i)
	      write(line,'(1pe13.6,2x,1pe13.6,2x,1pe13.6)') 
     *		xp(plot(i)+j-1),yp(plot(i)+j-1),fit(plot(i)+j-1)
 	      call logwrit(line)
            end do
	  end if
	enddo
c  Plot any flux indicator lines.
	do i=1,nflux
	   call pgsci(2+i)
	   linex(1)=xranged(1)
	   linex(2)=xranged(2)
	   liney(1)=fluxlines(i)
	   liney(2)=fluxlines(i)
	   call pgline(2,linex,liney)
	   if (i.eq.1) then
	      call pgmtxt('T',0.6,0.5,0.5,'Vector Average')
	   else if (i.eq.2) then
	      call pgmtxt('T',0.6,1.0,1.0,'Scalar Average')
	   endif
	enddo
	call pgsci(1)
c
c  The polarisation label.
c
	pollab = ' '
	lp = 0
	do i=1,npol
	   if (i.eq.wpol) then
	      pollab(lp+1:lp+2) = PolsC2P(pol(i))
	      lp = len1(pollab)
	      pollab(lp+1:lp+1) = ','
	      lp = lp + 1
	   endif
	enddo
c
c  The integration time label.
c
	write(tau,'(f16.1)')inttime/60.
	lt = 1
	dowhile(tau(lt:lt).eq.' ')
	  lt = lt + 1
	enddo
c
c  Time of day.
c
	T0 = nint(time - 1.d0) + 0.5
	sec = nint(24*3600*(time - T0))
	hr = sec / 3600
	sec = sec - 3600*hr
	mins = sec / 60
	sec = sec - 60*mins
c
	if(bl.eq.0)then
c	  write(title,'(a,i2.2,a,i2.2,a,i2.2)')
c     *	    pollab(1:lp)//' \gt='//tau(lt:)//' min, T=',
c     *	    hr,':',mins,':',sec
	   write(title,'(a,a,a)') 'Stokes ',pollab(1:lp),
     *      ' Spectrum Measurement'
	else
c
c  Decode baseline number into antenna numbers.
c
	  b2 = 1
	  l = 1
	  dowhile(bl.ge.l+b2)
	    l = l + b2
	    b2 = b2 + 1
	  enddo
	  b1 = bl - l + 1
c
	  baseline = itoaf(b1)
	  l = len1(baseline)
	  baseline(l+1:) = '-'//itoaf(b2)
	  l = len1(baseline)
c	  
	  write(title,'(a,i2.2,a,i2.2,a,i2.2)')
     *	    pollab(1:lp)//' \gt='//tau(lt:)//' min, Bl='//
     *	    baseline(1:l)//', T=',hr,':',mins,':',sec
	endif
	l = len1(title)
	xl = len1(xtitle)
	yl = len1(ytitle)
c
        xtitle2=xtitle
        if (xoff.gt.0) then
          i=index(xtitle,' (')
          if (i.gt.0) then
            write(xtitle2(i+1:i+8),'(a,F7.3)') '-',xoff
            xtitle2(i+9:xl+9)=xtitle(i:xl)
            xl=xl+9
          endif
        endif
	call pglab(xtitle2(1:xl),ytitle(1:yl),' ')
	call pglen(5,title(1:l),xlen,ylen)
	xloc = 0.5 - 0.5*xlen
	call pgqch(size)
	if(xloc.lt.0)then
	  call pgsch(size/xlen)
	  xloc = 0
	endif
c
	k1 = 1
	do i=1,npol
	  k2 = k1 + len1(polsc2p(pol(i))) - 1
	  if(i.ne.npol)k2 = k2 + 1
	  call pgsci(i)
	  call pgmtxt('T',2.0,xloc,0.,title(k1:k2))
	  call pglen(5,title(k1:k2),xlen,ylen)
	  xloc = xloc + xlen
	  k1 = k2 + 1
	enddo
	call pgsci(1)
	k2 = l
	call pgmtxt('T',2.0,xloc,0.,title(k1:k2))
	call pgsch(size)
c
c Make a plot for the uvdist vs amp
c
	call SetAxisR(uvd,nuvd,xranged)
	call SetAxisR(uva,nuvd,yranged)
	dint=(xranged(2)-xranged(1))/real(nd)
	qualat=0.0
	qualt=0.0
	do j=1,nd
	   bda(j)=0.
	   bdc(j)=dint*(j-1)
	   bdn=0
	   do k=1,nuvd
	      if (uvd(k).ge.(dint*(j-1)).and.
     *            uvd(k).lt.(dint*j)) then
		 bda(j)=bda(j)+uva(k)
		 bdn=bdn+1
	      endif
	   enddo
	   if (bdn.gt.0) then
	      bda(j)=bda(j)/real(bdn)
	   endif
	   qualat=qualat+abs(bda(j))
	   qualt=qualt+bda(j)
	enddo
	qualn=qualat-qualt
	qualp=abs(qualt/qualat)
c	write(line,'(1pe11.3,1pe11.3,1pe11.3)') qualat,qualt,qualn
c	call output(line)
	if (plotuv) then
	   call pgpage
	   call pgvstd
	   call pgswin(xranged(1),xranged(2),yranged(1),yranged(2))
	   call pgbox('BCNST',0.,0.,'BCNST',0.,0.)
	   call pgpt(nuvd,uvd,uva,-1)
	   call pgsci(2)
	   call pgbin(nd,bdc,bda,0)
	   call pgsci(1)
	   write(title,'(a,a,a)') 'Stokes ',pollab(1:lp),
     *       ' Calibrator Quality Measurement'
	   call pglab('uv Distance (k\gl)',
     *                'Spectrally-corrected Residual Amplitude (Jy)',
     *                 title)
	endif
c	call pghist(nuvd,uva,yranged(1),yranged(2),100,0)
	end
c***********************************************************************
	subroutine polyfit(poly,nchan,value,work2,weight,
     *                     spec,fit,serr,dolog,fitparams,
     *                     dopfit,ufitparams,ufit,plfitx)

	integer nchan,poly
	real spec(*),fit(*),work2(*),weight(*),serr,ufit(*)
	double precision value(*)
	real xlim1,xlim2,fitparams(*),ufitparams(*),plfitx(*)
	logical dolog,dopfit
c-----------------------------------------------------------------------
c     Polynomial fit of spectrum
c
c   Inputs:
c     poly         order of fit
c     nchan        Number of channels
c     value        Array of x-values.
c     work2        Work array (4*maxdim)
c     weight       Weight array (maxdim)
c     spec         Spectrum.
c     dolog        Make the fit in log space.
c   Outputs:
c     weight       Weight array (maxdim)
c     fit          Polynomial fit
c     serr         rms
c-----------------------------------------------------------------------
	real clip
	integer i,j,ifail,npts,niter,sn
	double precision dfit
	real coef(11),test2,work3(24),rvalue(nchan)
	real rspec(nchan),d(nchan),ss,sa,minx,maxx,tx
	character*80 line
	logical hasneg
c-----------------------------------------------------------------------
c  Number of clipping iterations
	niter=10
	
c  Clip level (sigma)
	clip=3.0

c  Apply mask and check for negative numbers.
	hasneg=.FALSE.
	do i = 1, nchan
	   weight(i)=1.0
	   if (spec(i).le.0.) then
	      hasneg=.TRUE.
	   endif
	enddo
	if (hasneg.and.dolog) then
	   call bug('w','Log fitting prevented due to negative nums')
	   dolog=.FALSE.
	endif

c  Iterate
 100	if (niter.eq.0) goto 1000

c  Count unclipped values
	npts=0
	do i = 1, nchan
	   if (weight(i).gt.0.0)  npts=npts+1
	enddo
c	write(line,'(a,i6)'), 'npts = ',npts
c	call output(line)
	if (npts.eq.0) goto 1000

c  Initialize
	serr=0.0
	do i = 1, nchan
	   fit(i)=0.0
	enddo

	do i = 1, 11
	   coef(i)=0.0
	enddo

c  Polynomial fit
	ifail=1
	if (npts.gt.poly+1) then
	   if (poly.gt.0) then
	      do i=1,nchan
		 rvalue(i)=real(value(i))
		 rspec(i)=spec(i)
		 if (dolog) then
		    rvalue(i)=log10(rvalue(i))
		    rspec(i)=log10(spec(i))
		 endif
	      enddo
	      call wpfit(poly,nchan,rvalue,rspec,weight,coef,test2,
     *                   work3,work2,ifail)
	   else
	      coef(1)=0.0
	      test2=0.0
	      do i = 1, nchan
		 coef(1)=coef(1)+weight(i)*spec(i)/real(npts)
	      enddo
	      do i = 1, nchan
		 test2=test2+weight(i)*(spec(i)-coef(1))**2
	      enddo
	      test2=sqrt(test2)
	      ifail=0
	   endif
	endif
	if (ifail.ne.0) call bug('f', 'Clipped polynomial fit error')
c
c  RMS error corrected for dof (trap zero divides)
c
	if (npts.eq.11 .and. poly.eq.10) then
	   serr=0.0
	else
	   serr=test2/sqrt(real(npts-poly-1))
	endif
c
c  Evaluate polynomial
c
	do i = 1, nchan
	   d(i)=0.0
	   dfit=dble(coef(1))
	   fit(i)=coef(1)
	   if (dopfit) then
	      ufit(i)=ufitparams(1)
	      do j = 2, 10
		 ufit(i)=ufit(i)+ufitparams(j)*(rvalue(i)**(j-1))
c		 ufit(i)=ufit(i)+ufitparams(j)*(plfitx(i)**(j-1))
	      enddo
	   endif
	   if (poly.gt.0) then
	      do j = 2, poly+1
		 if (rvalue(i).ne.0.0) then
		    dfit=dfit+dble(coef(j))*dble(rvalue(i))**(j-1)
		    fit(i)=fit(i)+coef(j)*(rvalue(i)**(j-1))
c		    dfit=dfit+dble(coef(j))*dble(plfitx(i))**(j-1)
c		    fit(i)=fit(i)+coef(j)*(plfitx(i)**(j-1))
		 endif
	      enddo
	   endif
	   if (dolog) then
	      fit(i)=10**(fit(i))
	      if (dopfit) then
		 ufit(i)=10**(ufit(i))
	      endif
	   endif
	   if (dolog.and.weight(i).gt.0.0) then
	      d(i)=real(dble(spec(i))-10**dfit)
	   endif
	enddo

c  sigma clip
	if (dolog) then
	   sn=0
	   sa=0.0
	   do i=1,nchan
	      if (weight(i).gt.0.0) then
		 sn=sn+1
		 sa=sa+d(i)
	      endif
	   enddo
	   sa=sa/real(sn)
	   ss=0
	   do i=1,nchan
	      if (weight(i).gt.0.0) then
		 ss=ss+(d(i)-sa)**2
	      endif
	   enddo
	   serr=sqrt(ss/(real(sn-poly-1)))
	endif

	do i = 1, nchan
	   if (weight(i).gt.0.0) then
	      if (abs(spec(i)-fit(i)).gt.clip*serr)
     *           weight(i)=0.0
	   endif
	enddo

c  Iteration count

	niter=niter-1
	goto 100

c  Return the fit coefficients.
 1000	do i=1,poly+1
	   fitparams(i)=coef(i)
	enddo
c  Make the final fit, over the entire range.
	minx=real(value(1))
	maxx=real(value(1))
	do i=2,nchan
	   minx=min(real(value(i)),minx)
	   maxx=max(real(value(i)),maxx)
	enddo
	do i=1,nchan
	   plfitx(i)=minx+real(i-1)*(maxx-minx)/real(nchan)
	   fit(i)=fitparams(1)
	   tx=plfitx(i)
	   if (dolog) then
	      tx=log10(tx)
	   endif
	   if (dopfit) then
	      ufit(i)=ufitparams(1)
	      do j=2,10
		 ufit(i)=ufit(i)+ufitparams(j)*(tx**(j-1))
	      enddo
	   endif
	   if (poly.gt.0) then
	      do j=2,poly+1
		 fit(i)=fit(i)+fitparams(j)*(tx**(j-1))
	      enddo
	   endif
	   if (dolog) then
	      fit(i)=10**(fit(i))
	      if (dopfit) then
		 ufit(i)=10**(ufit(i))
	      endif
	   endif
	enddo
c
	end
c************************************************************************
	subroutine IntFlush(nants,npol,time,quad,avall,
     *	  init,Corrs,CorrPnt,Flags,FlagPnt,nchan,sigma2,maxbase,maxpol,
     *	  ntrip,trip,triptime,tripsig2,maxtrip)
c
	integer nants,npol,maxbase,maxpol,maxtrip
	double precision time,triptime(maxtrip)
	integer CorrPnt(maxbase,maxpol),FlagPnt(maxbase,maxpol)
	integer ntrip(maxtrip),nchan(maxbase,maxpol)
	logical quad,avall,init(maxbase,maxpol),Flags(*)
	real sigma2(maxbase,maxpol),tripsig2(maxtrip)
	complex Corrs(*),trip(maxtrip)
c------------------------------------------------------------------------
	real flux
	integer p,i4,i3,i2,i1,bl12,bl13,bl23,bl14,bl34,k,i,nread
	integer pflag12,pflag13,pflag23,pdata12,pdata23,pdata13
	integer pflag14,pflag34,        pdata14,pdata34
	complex denom
c
	do p=1,npol
	  if(avall)then
	    k = 1
	  else
	    k = 0
	  endif
c
c  Quad quantity.
c
	  if(quad)then
	    do i4=4,nants
	    do i3=3,i4-1
	      do i2=2,i3-1
	        do i1=1,i2-1
	 	  if(.not.avall)k = k + 1
	          bl12 = ((i2-1)*(i2-2))/2 + i1
	          bl34 = ((i4-1)*(i4-2))/2 + i3
	          bl14 = ((i4-1)*(i4-2))/2 + i1
	          bl23 = ((i3-1)*(i3-2))/2 + i2
		  if(init(bl12,p).and.init(bl34,p).and.init(bl14,p).and.
     *		     init(bl23,p))then
		    if(ntrip(k).eq.0)then
		      triptime(k) = 0
		      tripsig2(k) = 0
	 	      trip(k) = 0
		    endif
		    nread = min(nchan(bl12,p),nchan(bl34,p),
     *				nchan(bl14,p),nchan(bl23,p))
		    pdata12 = corrpnt(bl12,p) - 1
		    pdata34 = corrpnt(bl34,p) - 1
		    pdata14 = corrpnt(bl14,p) - 1
		    pdata23 = corrpnt(bl23,p) - 1
		    pflag12 = flagpnt(bl12,p) - 1
		    pflag34 = flagpnt(bl34,p) - 1
		    pflag14 = flagpnt(bl14,p) - 1
		    pflag23 = flagpnt(bl23,p) - 1
		    do i=1,nread
		      if(Flags(pflag12+i).and.Flags(pflag34+i).and.
     *		         Flags(pflag14+i).and.Flags(pflag23+i))then
			flux = 0.25*(abs(Corrs(pdata12+i)) + 
     *				     abs(Corrs(pdata34+i)) +
     *				     abs(Corrs(pdata14+i)) +
     *				     abs(Corrs(pdata23+i)))
			denom = Corrs(pdata14+i)*conjg(Corrs(pdata23+i))
			if(abs(real(denom))+abs(aimag(denom)).eq.0.or.
     *			   abs(flux).eq.0)call bug('f',
     *		  'Flux quantity identically zero when doing division')
		        trip(k) = trip(k) +
     *			  (Corrs(pdata12+i) *       Corrs(pdata34+i))/
     *			  denom
		        tripsig2(k) = tripsig2(k) + 
     *			  (sigma2(bl12,p) + sigma2(bl34,p) +
     *			   sigma2(bl14,p) + sigma2(bl23,p))/(flux*flux)
		        triptime(k) = triptime(k) + time
		        ntrip(k) = ntrip(k) + 1
		      endif
		    enddo
		  endif
	        enddo
	      enddo
	      enddo
	    enddo
c
c  Triple quantity.
c
	  else
	    do i3=3,nants
	      do i2=2,i3-1
	        do i1=1,i2-1
	 	  if(.not.avall)k = k + 1
	          bl12 = ((i2-1)*(i2-2))/2 + i1
	          bl13 = ((i3-1)*(i3-2))/2 + i1
	          bl23 = ((i3-1)*(i3-2))/2 + i2
		  if(init(bl12,p).and.init(bl13,p).and.init(bl23,p))then
		    if(ntrip(k).eq.0)then
		      triptime(k) = 0
		      tripsig2(k) = 0
	 	      trip(k) = 0
		    endif
		    nread = min(nchan(bl12,p),nchan(bl13,p),
     *				nchan(bl23,p))
		    pdata12 = corrpnt(bl12,p) - 1
		    pdata13 = corrpnt(bl13,p) - 1
		    pdata23 = corrpnt(bl23,p) - 1
		    pflag12 = flagpnt(bl12,p) - 1
		    pflag13 = flagpnt(bl13,p) - 1
		    pflag23 = flagpnt(bl23,p) - 1
		    do i=1,nread
		      if(Flags(pflag12+i).and.Flags(pflag13+i).and.
     *		         Flags(pflag23+i))then
		        trip(k) = trip(k) + Corrs(pdata12+i)
     *					* Corrs(pdata23+i)
     *				  * conjg(Corrs(pdata13+i))
		        tripsig2(k) = tripsig2(k) + sigma2(bl12,p)
     *						+ sigma2(bl23,p)
     *						+ sigma2(bl13,p)
		        triptime(k) = triptime(k) + time
		        ntrip(k) = ntrip(k) + 1
		      endif
		    enddo
		  endif
	        enddo
	      enddo
	    enddo
	  endif
c
c  Reset the baseline.
c
	  do i=1,(nants*(nants-1))/2
	    init(i,p) = .false.
	  enddo
	enddo
	end
c************************************************************************
	subroutine PolIdx(p,npol,polcvt,PolMin,PolMax,MAXPOL)
c
	integer p,PolMin,PolMax,npol,MAXPOL
	integer polcvt(PolMin:PolMax)
c------------------------------------------------------------------------
	integer pol
c
c  Externals.
c
	logical polspara
c
	p = 0
	call uvDatGti('pol',pol)
	if(pol.lt.PolMin.or.pol.gt.PolMax)return
	if(polcvt(pol).eq.0)then
	  if(PolsPara(pol))then
	    npol = npol + 1
	    if(npol.gt.MAXPOL)call bug('f','Too many polarisations')
	    polcvt(pol) = npol
	  endif
	endif
	p = polcvt(pol)
	end
