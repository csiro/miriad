c************************************************************************
	program opplt
c
	implicit none
c
c= opplt - Plot opacity and sky brightness.
c& rjs
c: miscellaneous
c+
c	Opplt plots model opacity and atmospheric brightness temperatures 
c	given site information. These quantities can be plotted as a function
c	of frequency or elevation.
c
c       To plot as a function of frequency, give two values for the 
c       freq key and one for the el key.
c
c       To plot as a function of elevation, give two values for the 
c       el key and one for the freq key.
c
c       You must give two values for either freq or el.
c
c@ freq
c	Frequency range of interest, in GHz. One or two values can be given.
c	If two values are given, then the plot is as a function of frequency,
c       and the values are the limits for the plot.
c	The default is 22 GHz.
c@ el
c	Elevation angle, in degrees. One or two values can be given.
c	If two values are given, then the plot is as a function of elevation,
c       and the values are the limits for the plot.
c	The default is 90 degrees (i.e. zenith).
c@ device
c	Plot device. Default is not to plot anything.
c@ options
c	Extra processing options. Currently there is only a single option.
c	  airmass  When plotting as a function of elevation, express the
c	           x-axis as cosec(elevation), which is also known as the
c	           airmass.
c@ t
c	Temperature, in Kelvin. Default is 300.
c@ z
c	Observatory altitude, in m. Default is 200 (i.e. the
c	altitude of Narrabri).
c@ p
c	Sea-level atmospheric pressure, in hPa (i.e. millibars). The default
c	is 1013 hPa.
c@ h
c	Relative humidity, as a percentage. The default is 20%.
c
c--
c
c  History:
c    rjs  04Feb01  Original version.
c    dpr  05Feb01  Update doc and err messages.
c    rjs  06feb01  Added options=airmass.
c    rjs  24apr01  Split out the code to determine brightness etc.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer nmax
	parameter(nmax=250)
c
	real f1,f2,finc,el1,el2,elinc,P0,z0,el,t0,h0,fac,Tb,nu
	logical dofr,doel,airmass
	real x(NMAX),y1(NMAX),y2(NMAX),ymin,y1max,y2max,ylo,yhi
	integer i,npnt
	character device*64
c                  
c  Atmospheric parameters. 
c   
        real M,R,Mv,g,rho0
        parameter(M=28.96e-3,R=8.314,Mv=18e-3,rho0=1e3,g=9.81)
c       
c  d - Temperature lapse rate                0.0065 K/m.
c       
        real d
        parameter(d=0.0065)
c
c  Externals.
c
	integer pgbeg
c
	call keyini
	call output('opplt: version 1.0 24-Apr-01')
	call keya('device',device,' ')
c
c  Get frequency range of interest, in GHz.
c
	call keyr('freq',f1,22.)
	call keyr('freq',f2,f1)
	call keyr('freq',finc,0.)
	call keyr('el',el1,90.0)
	call keyr('el',el2,el1)
	call keyr('el',elinc,0.)
c
	if((min(f1,f2).le.0) .or.(max(f1,f2).gt.1000))
     *		call bug('f',
     * 'Frequency range invalid: use 0 < freq < 1000 GHz')
	if((min(el1,el2).le.5).or.(max(el1,el2).gt.90))
     *		call bug('f',
     * 'Elevation range invalid: use 5 < el <= 90 deg')
	dofr = f2.gt.f1
	doel = el2.gt.el1
	if(dofr.and.doel)call bug('f',
     *	  'Cannot plot both as a function of frequency and elevation')
	if((.not. dofr) .and. (.not. doel)) call bug('f',
     *	  'You must specify a range for either freq and el')
	
c
c  Get temperature at observatory in Kelvin.
c  Get percent relative humidity at observatory.
c
	call keyr('p',p0,1013.0)
	call keyr('t',t0,300.0)
	call keyr('z',z0,200.0)
        P0 = p0*1e2*exp(-M*g/(R*T0)*(z0-0.5*d*z0*z0/T0))
	call keyr('h',h0,20.0)
	h0 = 0.01*h0
	call getopt(airmass)
	call keyfin
c
	el1 = el1 * PI/180.0
	el2 = el2 * PI/180.0
	elinc = elinc * PI/180.0
	npnt = NMAX
c
c  Determine characteristics.
c
	nu = f1
	el = el1
	ymin = 0
	y1max = 0
	y2max = 0
	do i=1,npnt
	  if(dofr)then
	    nu = f1 + (f2-f1)*real(i-1)/(NPNT-1)
	    x(i) = nu
	  else
	    el = el1 + (el2-el1)*real(i-1)/(NPNT-1)
	    if(airmass)then
	      x(i) = 1/sin(el)
	    else
	      x(i) = 180.0/PI * el
	    endif
	  endif
	  call opacget(1,nu*1e9,el,t0,p0,h0,fac,Tb)
	  y1(i) = Tb
	  y2(i) = -log(fac)
	  y1max = max(y1max,y1(i))
	  y2max = max(y2max,y2(i))
	enddo
c
	if(device.ne.' ')then
	  if(pgbeg(0,device,1,2).ne.1)
     *	    call bug('f','Error opening PGPLOT device')
	  call pgsch(1.6)
	  call pgscf(2)
	  call pgrnge(ymin,y1max,ylo,yhi)
	  if(dofr)then
	    call pgenv(f1,f2,ylo,yhi,0,0)
	    call pglab('Frequency (GHz)',
     *				'Brightness Temperature (K)',' ')
	  else if(airmass)then
	    call pgenv(x(npnt),x(1),ylo,yhi,0,0)
	    call pglab('Airmass [cosec(\fiel\fr)]',
     *				'Brightness Temperature (K)',' ')
	  else
	    call pgenv(x(1),x(npnt),ylo,yhi,0,0)
	    call pglab('Elevation (degrees)',
     *				'Brightness Temperature (K)',' ')
	  endif
	  call pgline(NPNT,x,y1)
	  call pgrnge(ymin,y2max,ylo,yhi)
	  if(dofr)then
	    call pgenv(f1,f2,ylo,yhi,0,0)
	    call pglab('Frequency (GHz)','Opacity (nepers)',' ')
	  else if(airmass)then
	    call pgenv(x(npnt),x(1),ylo,yhi,0,0)
	    call pglab('Airmass [cosec(\fiel\fr)]',
     *					'Opacity (nepers)',' ')
	  else
	    call pgenv(x(1),x(npnt),ylo,yhi,0,0)
	    call pglab('Elevation (degrees)','Opacity (nepers)',' ')
	  endif
	  call pgline(NPNT,x,y2)
	  call pgend
	endif
	end
c************************************************************************
	subroutine getopt(airmass)
c
	implicit none
	logical airmass
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	logical present(NOPTS)
	character opts(NOPTS)*8
	data opts/'airmass '/
c
	call options('options',opts,present,NOPTS)
	airmass = present(1)
	end
