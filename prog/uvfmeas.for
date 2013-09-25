c************************************************************************
	program uvfmeas
	implicit none
c
c= uvfmeas - Calculate the fluxes of a point source at the phase
c               centre.
c& jbs
c: uv analysis
c+
c	UVFMEAS plots averaged spectra of a visibility dataset. It is
c       used to measure the spectral behaviour of a continuum source,
c       and can perform high order polynomial fits to the data. In this
c       way, a flux density at a particular frequency can be more
c       accurately determined. Multiple sets at different frequencies
c       can be simultaneously plotted, allowing you to make a global
c       fit over a wider range of frequencies, which may be more accurate.
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
c	plot those polarizations present in the input files. Stokes
c       parameters will not however be overplot on each other; only one
c       polarization will be plotted at a time.
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
c          'machine'     Output the fit coefficients on a single line,
c                        separated by spaces, suitable for parsing by
c                        another program.
c          'mfflux'      Output the fit coefficients in a way that can be
c                        input as the flux parameter in mfcal. Only really
c                        works for order=1 (linear fit).
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
c@ feval
c       A frequency (in GHz) at which to evaluate the fit, and output
c       the flux density in Jy.
c
c$Id: uvfmeas.for,v 1.5 2013/09/25 04:23:57 ste616 Exp $
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
	character line*132,PolCode*2,oline*132
	logical nobase,avall,first,buffered,doflush,qfirst
	logical doshift,subpoly,dolog,dovec,douv,dopfit,domachine
	logical domfflux
	double precision interval,T0,T1,preamble(5),shift(2),lmn(3)
	double precision fluxr(MAXPOL,MAXCHAN),fluxi(MAXPOL,MAXCHAN)
	double precision amp(MAXPOL,MAXCHAN),amp2(MAXPOL,MAXCHAN)
	double precision rms2(MAXPOL,MAXCHAN),u,v
	double precision vecavgr,vecavgi
	integer tIn,vupd,poly,ncnt(MAXPOL,MAXCHAN),ipol,npol
	integer nxy(2),nchan,nread,nplot,PolIndx(PolMin:PolMax)
	integer p(MAXPOL),pp(MAXPOL),lmax,mnchan,vecavgn,scalavgn
	integer dnx,dny,tcm(2*MAXCHAN-2)
	real yrange(2),temp,scalamp(MAXCHAN),scalscat(MAXCHAN)
	real vecamp(MAXCHAN),vecpha(MAXCHAN),vecscat(MAXCHAN),sig2
	real work2(4*maxdim),weight(maxdim),fit(maxdim),serr
	real xrange(2),yp(MAXCHAN),scalavga,vecavgs,scalavgs
	real uvdist(MAXPNT),uvdistamp(MAXPNT),uvdistfreq(MAXPNT)
	real sexpect,qualn,qualp,plotfit(11),ufit(maxdim)
	real fitdiffsum,plfitx(maxdim),evxp,evfx,polyeval,feval
	double precision x(2*MAXCHAN-2),xf(2*MAXCHAN-2)
	double precision xp(2*MAXCHAN-2),txf(2*MAXCHAN-2)
	double precision mx(2*MAXCHAN-2)
	complex data(MAXCHAN),vecaver(MAXCHAN)
	logical flags(MAXCHAN),fpresnt
	integer hann,ibin,i,j,t,plot(MAXPLT+1),nplts,k
	integer tncnt,chplot(MAXCHAN),nants
	integer nuvdist,nachan
	real hc(maxco),hw(maxco),fitparams(11),fluxlines(2)
c
c  Externals.
c
	logical uvDatOpn
	character PolsC2P*2
	character versan*80
c-----------------------------------------------------------------------
	version = versan ('uvfmeas',
     :                    '$Revision: 1.5 $',
     :                    '$Date: 2013/09/25 04:23:57 $')
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags,nobase,avall,dolog,dovec,douv,dopfit,
     *              domachine,domfflux)
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
	call keyr('feval',feval,0.0)
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
		  uvdist(nuvdist)=real(sqrt(u*u+v*v)*txf(i)/txf(1))
		  uvdistamp(nuvdist)=real(data(i))
		  uvdistfreq(nuvdist)=real(txf(i))
		  nuvdist=nuvdist+1
		  if (nuvdist.ge.MAXPNT) then
		     call bug('f','Too many points!')
		  endif
	       endif
	    enddo
	    nuvdist=nuvdist-1
c
c  Accumulate more data, if we are time averaging.
c
	    call GetXAxis(tIn,xaxis,xtitle,mx,nplot)
	    do i=1,nchan
	       x(tcm(i))=mx(i)
	    enddo
	    nchan = nread
c
c  Keep on going. Read in another record.
c
	    call uvDatRd(preamble,data,flags,maxchan,nread)
	  enddo
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
	      if (domachine) then
		 write(line, '(a)') 'Coeff:'
		 do j=1,poly+1
		    oline=line
		    write(line,'(a,1pe11.3)') oline(1:len_trim(oline)),
     *                 fitparams(j)
		 enddo
		 oline=line
		 if (dolog) then
		    write(line,'(a,a)') oline(1:len_trim(oline)),' log'
		 else
		    write(line,'(a,a)') oline(1:len_trim(oline)),' lin'
		 endif
		 call output(line)
	      endif
	      if (domfflux) then
c              Evaluate at the integer frequency closest to the first.
		 evxp=float(int(xp(1)))
		 if (dolog) then
		    evxp=log10(evxp)
		 endif
		 evfx=polyeval(poly,dolog,evxp,fitparams)
		 evxp=float(int(xp(1)))
		 write(line,'(a11,f7.4,a1,f5.1,a1,f7.4)') 
     *	           'MFCAL flux=',evfx,',',evxp,',',fitparams(2)
		 call output(line)
	      endif
	      if (feval.gt.0.0) then
		 evxp=feval
		 if (dolog) then
		    evxp=log10(evxp)
		 endif
		 evfx=polyeval(poly,dolog,evxp,fitparams)
		 write(line,'(a,f7.3,a,f8.4,a)') 
     *	           'Flux density at ',feval,' GHz = ',evfx,' Jy.'
		 call output(line)
	      endif
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
	character xaxes(NX)*10,yaxes(NY)*9
	data xaxes/'channel   ','frequency ','velocity  ','felocity  ',
     *		   'lag       ','dfrequency'/
	data yaxes/'amplitude','phase    ','real     ','imaginary'/
c
	xaxis = xaxes(2)
	yaxis = yaxes(1)
	end
c************************************************************************
	subroutine GetOpt(uvflags,nobase,avall,dolog,dovec,douv,dopfit,
     *                    domachine,domfflux)
c
	implicit none
        logical nobase,avall,dolog,dovec,douv,dopfit,domachine,domfflux
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
	parameter(nopts=9)
	character opts(nopts)*9
	logical present(nopts),docal,dopol,dopass
	data opts/'nocal    ','nopol    ','nopass   ','log      ',
     *            'plotvec  ','uvhist   ','plotfit  ','machine  ',
     *            'mfflux   '/
c
	call options('options',opts,present,nopts)
	docal = .not.present(1)
	dopol = .not.present(2)
	dopass= .not.present(3)
	dolog=present(4)
	dovec=present(5)
	douv=present(6)
	dopfit=present(7)
	domachine=present(8)
	domfflux=present(9)
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
	real fitparams(*),ufitparams(*),plfitx(*),polyeval
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
	   fit(i)=polyeval(poly,dolog,rvalue(i),coef)
	   if (dopfit) then
	      ufit(i)=polyeval(9,dolog,rvalue(i),ufitparams)
	   endif
	   if (poly.gt.0) then
	      do j = 2, poly+1
		 if (rvalue(i).ne.0.0) then
		    dfit=dfit+dble(coef(j))*dble(rvalue(i))**(j-1)
		 endif
	      enddo
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
	   tx=plfitx(i)
	   if (dolog) then
	      tx=log10(tx)
	   endif
	   fit(i)=polyeval(poly,dolog,tx,fitparams)
	   if (dopfit) then
	      ufit(i)=polyeval(9,dolog,tx,ufitparams)
	   endif
	enddo
c
	end
c***********************************************************************
	real function polyeval(poly,dolog,freq,fitparams)
c
	integer poly
	real fitparams(*),freq
	logical dolog
c-----------------------------------------------------------------------
c   Evaluate the polynomial fit at a particular frequency.
c
c  Inputs:
c    poly           order of fit
c    dolog          The fit is in log space
c    freq           The frequency to evaluate at
c    fitparams      The polynomial fit parameters
c-----------------------------------------------------------------------
	integer i
	if (poly.gt.0) then
	   polyeval=fitparams(1)
	   do i=2,poly+1
	      polyeval=polyeval+fitparams(i)*(freq**(i-1))
	   enddo
	endif
	if (dolog) then
	   polyeval=10**polyeval
	endif
	return
c
	end
