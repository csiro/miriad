c************************************************************************
	program opcal
c
	implicit none
c
c= opcal - Correct data for atmospheric opacity and flux miscalibration.
c& rjs
c: uv analysis
c+
c	opcal corrects a visibility dataset for atmospheric opacity
c	and can also attempt to correct for errors in the flux calibration
c	scale. This should be the first step in the calibration of visibility
c	data (i.e. before any other calibration steps). It should be performed
c	on both the source of interest as well as all calibrators.
c
c	opcal works by computing the brightness and opacity of the atmosphere.
c	This is estimated from a model of the atmosphere, given the observing
c	frequency and elevation, as well as meteorological data.
c	Opacity correction is probably not warranted if the
c	fluctuations in opacity are small and the calibrator is quite close
c	to the program source. However opacity correction will not be
c	damaging in these instances (it would be just an unnecessary extra
c	step). At frequencies above 15 GHz, opacity correction is generally
c	recommended.
c
c	To correct for atmospheric opacity, opcal scales up the measured
c	visibility data to account for this attenuation. It also scales the
c	system temperature data to an ``above atmosphere'' value.
c
c	opcal can also estimate and correct for a miscalibration in the flux
c	calibration scale. The miscalibration might arise from incorrect
c	value assumed for the on-line calibrator source (noise diode), or
c	an incorrect conversion to system temperature. opcal works by 
c	comparing its computed estimates of the sky brightness with the
c	measured values of system temperature. In estimating a miscalibration
c	factor, it assumes that the measured system temperature is a result of
c	the the atmosphere and CMB, plus a constant (i.e. contributions to
c	system temperature from the receivers, spillover, astronomical source,
c	etc, are assumed constant). A scale factor to correct the measured
c	system temperature is then computed. Using this procedure should only
c	be attempted when the observation samples an appreciable range of
c	elevations.
c
c	NOTE: This procedure to estimate flux calibration factor is not 
c	necessarily correct. The system temperature scale may differ from
c	with the model data because the system efficiency used in the
c	conversion process (the telescope Jy/Kelvin) was wrong, and the
c	system temperature scale may have been adjusted to account for this.
c@ vis
c	The names of the input uv data sets. No default.
c@ out
c	The name of the output uv data set. No default.
c	The output dataset contains extra uv variables, which might be
c	instructive to examine. These are
c	  tsky      Expected sky brightness, in Kelvins
c	  trans     Expected atmospheric transmissivity (i.e. a fraction,
c	            with 1 indicating a transparent atmosphere).
c	  airtemp   Measured air temperature, in celsius.
c	  pressmb   Measured air pressure, in millibars.
c	  relhumid  Measured relative humidity, as a percent.
c         antel     Antenna elevation, in degrees.
c	  airmass   Airmass - cosec(antel)
c@ mdata
c	Input text file giving the ATCA meteorology data. No default.
c@ mode
c	This determines the calibration operations to perform. Possible
c	values are ``opacity'', ``flux'' or ``both''. The default is to
c	perform opacity correciton only.
c--
c  History:
c    02feb01 rjs  Original version.
c    07feb01 rjs  Write out elevation variable.
c------------------------------------------------------------------------
	integer MAXPOL
	parameter(MAXPOL=2)
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version='opcal: version 1.0 07-Feb-01')
	integer PolXX,PolYY,PolXY,PolYX
	parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
	integer lVis,lOut,vupd,pol,npol,i,j,k,i1,i2,p1,p2,nants
	real systemp(MAXWIN*MAXANT),scale
	real xtsys(MAXWIN*MAXANT),ytsys(MAXWIN*MAXANT)
	logical updated,dodiode,dotrans
	character vis*64,out*64,type*1,mdata*64,line*128
	integer nschan(MAXWIN),nif,nchan,length
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	double precision preamble(5),ptime
	double precision sfreq(MAXWIN),sdf(MAXWIN)
	double precision lst,lat,az,el,ra,dec,dtemp
	real freq0(MAXWIN),t0,p0,h0,fac(MAXWIN),Tb(MAXWIN)
	real dfac(MAXPOL,MAXWIN,MAXANT),doff(MAXPOL,MAXWIN,MAXANT)
c
	integer NMODES
	parameter(NMODES=3)
	character modes(NMODES)*8,mode*8
	integer nout
c
c  Externals.
c
	logical uvvarUpd
c
	data modes/'opacity ','flux    ','both    '/
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call keya('mdata',mdata,' ')
	call keymatch('mode',NMODES,modes,1,mode,nout)
	if(nout.eq.0)mode = modes(1)
	dodiode = mode.eq.'flux'.or.mode.eq.'both'
	dotrans = mode.eq.'opacity'.or.mode.eq.'both'
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
	if(mdata.eq.' ')call bug('f',
     *			'An input met data file must be given')
c
c  Get the diode calibration if needed.
c
	if(dodiode)then
	  call diodeGet(vis,mdata,dfac,doff,MAXPOL,MAXWIN,MAXANT,
     *							nif,nants)
	  call output('System temperature scale factor and offsets')
	  call output('===========================================')
	  do i=1,nif
	    do j=1,nants
	      do k=1,2
	        if(dfac(k,i,j).gt.0)then
		  dfac(k,i,j) = 1/dfac(k,i,j)
		  doff(k,i,j) = dfac(k,i,j)*doff(k,i,j)
		else
		  dfac(k,i,j) = 1
		  doff(k,i,j) = 0
	        endif
	      enddo
	    enddo
	    write(line,'(a,i2)')'Frequency',i
	    call output(line)
	    write(line,'(a,10f8.2)')'  Pol X: ',(dfac(1,i,j),j=1,nants)
	    call output(line)
	    write(line,'(a,10f8.2)')'         ',(doff(1,i,j),j=1,nants)
	    call output(line)
	    write(line,'(a,10f8.2)')'  Pol Y: ',(dfac(2,i,j),j=1,nants)
	    call output(line)
	    write(line,'(a,10f8.2)')'         ',(doff(2,i,j),j=1,nants)
	    call output(line)
	  enddo
	endif
c
c  Get ready to copy the data.
c
	call metInit(mdata)
	call uvopen(lVis,vis,'old')
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(lVis,'channel')
c
	call uvvarIni(lVis,vupd)
	call uvvarSet(vupd,'nschan')
	call uvvarSet(vupd,'sfreq')
	call uvvarSet(vupd,'sdf')
	call uvvarSet(vupd,'ra')
	call uvvarSet(vupd,'obsra')
	call uvvarSet(vupd,'dec')
	call uvvarSet(vupd,'obsdec')
	call uvvarSet(vupd,'telescop')
	call uvvarSet(vupd,'latitud')
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
c  Make the output history.
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'OPCAL: Miriad '//version)
	call hisinput(lOut,'OPCAL')
	call hisclose(lOut)
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	ptime = preamble(4) - 1
	dowhile(nchan.gt.0)
	  call uvrdvri(lVis,'pol',pol,0)
	  call uvrdvri(lVis,'npol',npol,0)
c
	  if(uvvarUpd(vupd))then
	    call uvrdvri(lVis,'nants',nants,0)
	    call uvprobvr(lVis,'nschan',type,length,updated)
	    nif = length
	    if(type.ne.'i'.or.length.le.0.or.length.gt.MAXWIN)
     *	      call bug('f','Invalid nschan parameter')
	    call uvgetvri(lVis,'nschan',nschan,nif)
c
	    call uvgetvrd(lVis,'sfreq',sfreq,nif)
	    call uvgetvrd(lVis,'sdf',sdf,nif)
	    do i=1,nif
	      freq0(i) = sfreq(i) + 0.5*(nschan(i)-1)*sdf(i)
	    enddo
	    call uvrdvrd(lVis,'ra',dtemp,0.d0)
	    call uvrdvrd(lVis,'obsra',ra,dtemp)
	    call uvrdvrd(lVis,'dec',dtemp,0.d0)
	    call uvrdvrd(lVis,'obsdec',dec,dtemp)
	    call getlat(lVis,lat)
	  endif
c
	  call varCopy(lVis,lOut)
c
	  if(abs(ptime-preamble(4)).gt.5.d0/86400.d0)then
	    ptime = preamble(4)
	    call getlst(lVis,lst)
	    call azel(ra,dec,lst,lat,az,el)
	    call metGet(ptime,t0,p0,h0)
	    call opacGet(nif,freq0,real(el),t0,p0,h0,fac,Tb)
	    call uvputvrr(lOut,'tsky',Tb,nif)
	    call uvputvrr(lOut,'trans',fac,nif)
	    call uvputvrr(lOut,'airtemp',t0-273.15,1)
	    call uvputvrr(lOut,'pressmb',p0/100.0,1)
	    call uvputvrr(lOut,'relhumid',100.0*h0,1)
	    call uvputvrd(lOut,'antel',180.0d0/DPI*el,1)
	    call uvputvrr(lOut,'airmass',1./sin(real(el)),1)
c
	    if(.not.dotrans)then
	      do i=1,nif
		fac(i) = 1
	      enddo
	    endif
c
c  Correct the system temperature records.
c
	    call uvgetvrr(lVis,'systemp',systemp,nif*nants)
	    call uvgetvrr(lVis,'xtsys',xtsys,nif*nants)
	    call uvgetvrr(lVis,'ytsys',ytsys,nif*nants)
	    k = 1
	    do i=1,nif
	      do j=1,nants
	        systemp(k) = systemp(k)/fac(i)
	        if(dodiode)then
		  systemp(k) = systemp(k)*sqrt(dfac(1,i,j)*dfac(2,i,j))
	          xtsys(k) = xtsys(k) * dfac(1,i,j)
	          ytsys(k) = ytsys(k) * dfac(2,i,j)
	        endif
	        k = k + 1
	      enddo
	    enddo
	    call uvputvrr(lOut,'systemp',systemp,nif*nants)
	    call uvputvrr(lOut,'xtsys',xtsys,nif*nants)
	    call uvputvrr(lOut,'ytsys',ytsys,nif*nants)
	  endif
c
	  if(npol.gt.0)then
	    call uvputvri(lOut,'npol',npol,1)
	    call uvputvri(lOut,'pol',pol,1)
	  endif
	  if(dodiode)then
	    call basant(preamble(5),i1,i2)
	    if(pol.gt.-5.or.pol.lt.-8)
     *		call bug('f','Invalid polarization')
	    p1 = 1
	    if(pol.eq.PolYY.or.pol.eq.PolYX)p1 = 2 
	    p2 = 1
	    if(pol.eq.PolYY.or.pol.eq.PolXY)p2 = 2
	  endif

c
	  k = 0
	  do i=1,nif
	    if(dodiode)then
	      scale = sqrt(dfac(p1,i,i1)*dfac(p2,i,i2))
	    else
	      scale = 1
	    endif
	    do j=1,nschan(i)
	      k = k + 1
	      data(k) = data(k) * scale / fac(i)
	    enddo
	  enddo
c
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call metFin
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine metInit(mdata)
c
	implicit none
	character mdata*(*)
c------------------------------------------------------------------------
	real t(2),p(2),h(2)
	double precision time(2)
	common/metcom/time,t,p,h
c
	call tinOpen(mdata,'n')
	call metRec(time(1),t(1),p(1),h(1))
	call metRec(time(2),t(2),p(2),h(2))
	end
c************************************************************************
	subroutine metRec(time,t0,p0,h0)
c
	implicit none
	real t0,p0,h0
	double precision time
c------------------------------------------------------------------------
	double precision dtime
c
c  Externals.
c
	integer tinNext
c
	if(tinNext().le.0)call bug('f','Error getting met data')
	call tinGett(time,0.d0,'atime')
        call tinGett(dtime,0.0d0,'dtime')
	call tinSkip(1)
	call tinGetr(t0,0.0)
	call tinSkip(2)
	call tinGetr(p0,0.0)
	call tinSkip(1)
	call tinGetr(h0,0.0)
c
c  Convert time to UT, temperature to kelvin, pressue to Pascals at Narrabri
c  and humidity to a fraciton.
c
	time = time + dtime - 10.0d0/24.0d0
	t0 = t0 + 273.15
	p0 = 0.975*100.0*p0
	h0 = 0.01*h0
c
	end
c************************************************************************
	subroutine metGet(time0,t0,p0,h0)
c
	double precision time0
	real t0,p0,h0
c------------------------------------------------------------------------
	integer i
c
	real t(2),p(2),h(2)
	double precision time(2)
	common/metcom/time,t,p,h
c
c  Point to the earlier time.
c
	i = 1
	if(time(1).gt.time(2))i = 2
c
c  Step through until we straddle two sets of measurements.
c
	dowhile(time0.gt.time(3-i))
	  call metRec(time(i),t(i),p(i),h(i))
	  i = 3 - i
	enddo
c
c  Return the measurements closest to the requested time.
c
	i = 1
	if(abs(time0-time(1)).gt.abs(time0-time(2)))i = 2
	t0 = t(i)
	p0 = p(i)
	h0 = h(i)
c
	end
c************************************************************************
	subroutine metFin
	call tinClose
	end
c************************************************************************
      subroutine getlst (lin, lst)
c
      implicit none
      integer lin
      double precision lst
c
c  Get lst of the current data point.
c
c  Input:
c    lin         Handle of file
c  Output:
c    lst         LAST in radians
c-----------------------------------------------------------------------
      double precision time,ra,long,dtemp
      character type*1
      integer length
      logical ok
c
c  Externals.
c
      double precision eqeq
c
      lst = 0.0d0
      call uvprobvr (lin, 'lst', type, length, ok)
      if (type(1:1).eq.' ') then
	call uvrdvrd (lin, 'ra', dtemp, 0.d0)
	call uvrdvrd (lin, 'obsra', ra, dtemp)
	call getlong(lin,long)
        call jullst (time, long, lst)
	lst = lst + eqeq(time)
      else
         call uvrdvrd (lin, 'lst', lst, 0.0d0)
      end if
c
      end
c************************************************************************
      subroutine getlong (lin, long)
c
c     Get longitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    longitude   Longitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision long
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      long = 0.0d0
      call uvprobvr (lin, 'longitu', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No longitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out longitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'longitude', long, ok)
            if (.not.ok) call bug('f', 
     +          'No valid longitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'longitu', long, 0.0d0)
      end if
c
      end
c************************************************************************
      subroutine getlat (lin, lat)
c
c     Get latitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    lat        Latitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision lat
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      lat = 0.0d0
      call uvprobvr (lin, 'latitud', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No latitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out latitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'latitude', lat, ok)
            if (.not.ok) call bug('f', 
     +          'No valid latitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'latitud', lat, 0.0d0)
      end if
c
      end
c************************************************************************
	subroutine opacGet(nfreq,freq,el,t0,p0,h0,fac,Tb)
c
	implicit none
	integer nfreq
	real freq(nfreq),el,t0,p0,h0,fac(nfreq),Tb(nfreq)
c
c  Return the transmissivity of the atmosphere given frequency, elevation
c  angle and met data.
c
c  Input:
c    freq	Frequency (GHz).
c    el		Elevation angle (radians).
c    t0,p0,h0	Met data - Observatory temperature, pressure and humidity
c		(Kelvins, Pascals and fraction).
c
c  Output:
c    fac	Transmissivity.
c    Tb		Sky brightness temperature.
c------------------------------------------------------------------------
	integer N
	parameter(N=50)
	real tau,Ldry,Lvap,T(N),Pdry(N),Pvap(N),z(N),P,zd
	integer i
c
c  Atmospheric parameters.
c
        real M,R,Mv,g,rho0
        parameter(M=28.96e-3,R=8.314,Mv=18e-3,rho0=1e3,g=9.81)
c
c  d - Temperature lapse rate                0.0065 K/m.
c  z0 - Water vapour scale height,           2000 m.
c  zmax - Max altitude of model atmosphere,  10000 m.
c
	real d,z0,zmax
	parameter(d=0.0065,z0=2000.0,zmax=10000.0)
c
c  Externals.
c
	real pvapsat
c
c  Generate a model of the atmosphere -- T is temperature,
c  Pdry is partial pressure of "dry" consistuents, Pvap is the
c  partial pressure of the water vapour.
c
	do i=1,N
	  zd = (i-1)*zmax/real(N)
	  z(i) = zd
	  T(i) = T0/(1+d/T0*zd)
	  P = P0*exp(-M*g/(R*T0)*(zd+0.5*d*zd*zd/T0))
	  Pvap(i) = min(h0*exp(-zd/z0)*pvapsat(T(1)),pvapsat(T(i)))
	  Pdry(i) = P - Pvap(i)
	enddo
c
c  Determine the transmissivity and sky brightness.
c
	do i=1,nfreq
	  call refract(T,Pdry,Pvap,z,N,freq(i)*1e9,2.7,el,Tb(i),tau,
     *							    Ldry,Lvap)
	  fac(i) = exp(-tau)
	enddo
c
	end
c************************************************************************
	subroutine diodeGet(vis,mdata,dfac,doff,MPOL,MWIN,MANT,
     *							nifs,nants)
c
	implicit none
	character vis*(*),mdata*(*)
	integer MPOL,MWIN,MANT,nifs,nants
	real dfac(MPOL,MWIN,MANT),doff(MPOL,MWIN,MANT)
c
c  Determine a scale factor to apply to the system temperatures to
c  make the model data agree with the nominal data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXPNTS,MAXN
	parameter(MAXPNTS=1000000,MAXN=10000)
c
	real t0,p0,h0,freq0(MAXWIN),tau(MAXWIN)
	integer lVis,iostat,nschan(MAXWIN),npnts,i,j,k,n
	double precision sfreq(MAXWIN),sdf(MAXWIN)
	real data(MAXPNTS),x(MAXN),y(MAXN)
	logical doinit
	double precision dtemp,time,ra,dec,lst,lat,az,el
	character xt*1,yt*1
	integer xn,yn
	logical xupd,yupd
c
c  Externals.
c
	integer uvscan
c
	call output('Getting data for flux scale calibration')
	npnts = 1
	doinit = .true.
	call metInit(mdata)
	call uvopen(lVis,vis,'old')
	iostat = uvscan(lVis,'systemp')
	dowhile(iostat.eq.0)
c
c  On the first time through, get all the information which we
c  assume will not change.
c
	  if(doinit)then
	    doinit = .false.
	    call uvprobvr(lVis,'nschan',xt,nifs,xupd)
	    call uvprobvr(lVis,'xtsys',xt,nants,xupd)
	    nants = nants / nifs
	    if(nants.gt.MAXANT.or.nifs.gt.MAXWIN.or.
     *	       nifs.gt.MWIN)call bug('f','Too many channels or ants')
	    call uvgetvri(lVis,'nschan',nschan,nifs)
	    call uvgetvrd(lVis,'sfreq',sfreq,nifs)
	    call uvgetvrd(lVis,'sdf',sdf,nifs)
	    do i=1,nifs
	      freq0(i) = sfreq(i) + 0.5*(nschan(i)-1)*sdf(i)
	    enddo
	    call getlat(lVis,lat)
	  endif
c
c  Check whether the system temperature records have changed.
c  If so, load then, and compute the corresponding estimates of the
c  sky brightness.
c
	  call uvprobvr(lVis,'xtsys',xt,xn,xupd)
	  call uvprobvr(lVis,'ytsys',yt,yn,yupd)
	  if(xupd.or.yupd)then
	    if(npnts+nifs*(1+2*nants).gt.MAXPNTS)
     *	      call bug('f','Too many points')
	    call uvrdvrd(lVis,'time',time,0.d0)
	    call uvrdvrd(lVis,'ra',dtemp,0.d0)
	    call uvrdvrd(lVis,'obsra',ra,dtemp)
	    call uvrdvrd(lVis,'dec',dtemp,0.d0)
	    call uvrdvrd(lVis,'obsdec',dec,dtemp)
	    call getlst(lVis,lst)
	    call azel(ra,dec,lst,lat,az,el)
	    call metGet(time,t0,p0,h0)
	    call opacGet(nifs,freq0,real(el),t0,p0,h0,tau,data(npnts))
	    npnts = npnts + nifs
	    call uvgetvrr(lVis,'xtsys',data(npnts),nants*nifs)
	    npnts = npnts + nants*nifs
	    call uvgetvrr(lVis,'ytsys',data(npnts),nants*nifs)
	    npnts = npnts + nants*nifs
	  endif
	  iostat = uvscan(lVis,'systemp')
	enddo
	call uvclose(lVis)
	call metFin
c
c  Now copy the data and fit it.
c
	call output('Doing the flux scale calibration step')
	n = npnts/(nifs*(1+2*nants))
	do j=1,nifs
	  k = nifs + 1 + (j-1)*nants
	  call getdat(nifs*(1+2*nants),n,data(j),x)
	  do i=1,nants
	    call getdat(nifs*(1+2*nants),n,data(k),y)
	    call linfit(x,y,n,dfac(1,j,i),doff(1,j,i),.true.)
	    call getdat(nifs*(1+2*nants),n,data(k+nifs*nants),y)
	    call linfit(x,y,n,dfac(2,j,i),doff(2,j,i),.true.)
	    k = k + 1
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine linfit(x,y,n,m,b,flag)
c
	implicit none
	integer n
	real x(n),y(n),m,b
	logical flag
c------------------------------------------------------------------------
	double precision SumXX,SumYY,SumXY,SumX,SumY,delta
	integer i
c
c  Determine the least squares fit first.
c
        SumX  = 0
        SumY  = 0
        SumXX = 0
        SumYY = 0
        SumXY = 0
        do i=1,n
          SumX  = SumX  + X(i)   
          SumY  = SumY  + Y(i)
          SumXX = SumXX + X(i)*X(i)
          SumYY = SumYY + Y(i)*Y(i)
          SumXY = SumXY + X(i)*Y(i)
        enddo
        delta = (n*SumXX - SumX*SumX)
        m = (n*SumXY - SumX*SumY)/delta
        b = (SumXX*SumY - SumX*SumXY)/delta
	end	
c************************************************************************
	subroutine getdat(n1,n2,in,out)
c
	implicit none
	integer n1,n2
	real in(n1,n2),out(n2)
c------------------------------------------------------------------------
	integer i
c
	do i=1,n2
	  out(i) = in(1,i)
	enddo
c
	end
