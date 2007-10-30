c************************************************************************
	program uvamiba
	implicit none
c= uvamiba - Generate uv tracks for an AMIBA observation.
c+
c	Generate (u,v)tracks for an ABIMA observation. This generates
c	a dataset containing data of a blank, noiseless sky.
c	The observation is centred on RA = 0. 
c@ out
c	Name of the output visibility dataset. There is no default.
c@ config
c	Platform configuration filename. The default is closepack.cfg.
c@ sctype
c	Scan type - drift or point. The default is drift.
c@ drift
c	Two parameters, giving the drift scan extent, and the drift
c	scan deadtime . Both are in hours, with the default being 0.25,0
c@ offset
c	Drift scan offset at LST=0, in decimal minutes. The default is 0.
c@ lat
c	Observatory latitude, in degrees. The default is 45 degrees.
c@ freq
c	Two parameters giving the observing frequency and bandwidth, in GHz.
c	The default is 95,20.
c@ nchan
c	Number of correlator channels. The default is 8.
c@ model
c	Image giving the model of the sky. This must be in units of
c	Kelvin, and must be a power of 2 in size on each axis. Either one
c	or three models can be given, with these representing a Stokes I image
c	or I, Q and U images. There is no default.
c@ elmin
c	Minimum platform elevation in degrees. The default is 20.
c@ trange
c	Three parameters giving the LST of start, end and increment, all in
c	hours. The default is -6,6,0.1
c@ dish
c	This gives two parameters: the dish diameter (in metres) and the
c	overall efficiency (including any correlator sensitivity loss).
c	These parameters are used to compute the system sensitivity (in Jy/K)
c	and the primary beam response. The default is 0.3,0.7.
c@ systemp
c	This gives two parameters, T1 and T2, which are used to determine
c	the system temperature. The system temperature is modelled as
c	  Tsys = T1 + T2/sin(elevation)
c	The default for both parameters is 0 (i.e. no system noise).
c@ options
c	Extra processing options.
c	  noparallactify   Do not parallactify the platform. The default
c	                   is to parallactify.
c	  linear           Simulate linear feeds. The default is to simulate
c	                   circular feeds.
c-
c  History:
c    mjk  16aug01 Original version.
c    rjs  23aug01 Change to output Miriad dataset.
c------------------------------------------------------------------------
	character version*(*)
	integer MAXMOD
	parameter(MAXMOD=3)
	parameter(version='version 1.0 23-Aug-01')
	include 'maxdim.h'
	include 'mirconst.h'
c
	character config*80,out*80,model(MAXMOD)*80
	logical dopara,err,circ,onflag
	double precision ra,dec,ra0,dec0
	real Dextent,overhead,offset,lat,freq,bw,elmin,sigma
	real T,Tstart,Tstop,Tinc,inttime,psi,systemp
	integer nchan,nant,tno,i,j,k,nrec,nout,offsrc,errcnt
	integer id,npr,nmod
	complex data(MAXCHAN*MAXMOD)
	logical flags(MAXCHAN)
	real uvw(3,MAXANT),pbfwhm
	real az,el,dT,sfreq,sdf,dishdiam,eta,jyperk,Trec,Tsky
	double precision along,t0,preamble(5),u,v,w
	character sctypes(2)*8,sctype*8
c
c  Externals.
c
	character itoaf*2,stcat*80
	double precision antbas,epo2jul
	real chat
	external chat
c
	data sctypes/'drift   ','point   '/
c
c  Get the input parameters.
c
	call output('uvambia: '//version)
	call keyini
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output filename must be given')
	call keya ('config',config,' ')
	if(config.eq.' ')call bug('f','A config file must be given')
	call getopt(dopara,circ)
	call keymatch('sctype',2,sctypes,1,sctype,nout)
	if(nout.eq.0)sctype = sctypes(1)
	call keyr('drift',Dextent,0.25)
	call keyr('drift',overhead,0.)
	call keyr('offset',offset,0.)
	call keyr('lat',lat,45.)
	call mkeya('model',model,MAXMOD,nmod)
	if(nmod.ne.1.and.nmod.ne.3)call bug('f',
     *	  'Invalid number of models')
	call keyr('freq',freq,95.)
	call keyr('freq',bw,20.)
	call keyi('nchan',nchan,8)
	if(nchan.lt.1)call bug('f','Must be a positive no. channels')
	sfreq = freq - real(nchan-1)/real(2*nchan)*bw
	sdf = bw/nchan
	if(sdf.le.0.or.sfreq.le.0.5*sdf)
     *	  call bug('f','Invalid frequencies')
	call keyr('elmin',elmin,20.)
	call keyr('trange',Tstart,-6.)
	call keyr('trange',Tstop,6.)
	call keyr('trange',Tinc,0.1)
	if(tstart.gt.tstop.or.tinc.le.0)
     *	  call bug('f','Invalid time range parameter')
	call keyr('dish',dishdiam,0.3)
	call keyr('dish',eta,0.7)
	call keyr('systemp',Trec,0.)
	call keyr('systemp',Tsky,0.)
	call keyfin
c
c  Various derived system parameters.
c
	jyperk = 2*KMKS*1e26/(0.25*DPI*dishdiam*dishdiam*eta)
	pbfwhm = 66000/dishdiam/freq
	inttime = 3600.*365.25/366.25*Tinc
	t0 = epo2jul(2000.d0,'J')
        call jullst(t0,ra,along)  
	along = -along
        if(along.gt.DPI)along = along - 2*DPI
c
c  Initailise the model of the sky.
c
	call output('Initalising model of the sky ...')
	call visInit(model,nmod,sfreq,sdf,nchan,ra,dec)
c
c  Initialise the description of the observation.
c
	call output('Initialising description of the array ...')
	call amInit(config,sctype,dopara,Dextent,overhead,offset,
     *	   real(180.d0/DPI*dec),lat,CMKS*1e-9,elmin,nant,err)
	if(err)
     *	  call bug('f','Error initialising the observation description')
c
c  Open the output file.
c
	call uvopen(tno,out,'new')
	call uvset(tno,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call hisopen(tno,'write')
	call hiswrite(tno,'UVAMIBA: Miriad '//version)
	call hisinput(tno,'UVAMIBA')
c
c  Write some info about the array.
c
	call uvputvrr(tno,'epoch',2000.0,1)
	call uvputvri(tno,'nants',nant,1)
	call uvputvrr(tno,'veldop',0.0,1)
	call uvputvrr(tno,'vsource',0.0,1)
	call uvputvra(tno,'source','AmibaField')
	call uvputvrd(tno,'ra',ra,1)
	call uvputvrd(tno,'obsra',ra,1)
	call uvputvrd(tno,'dec',dec,1)
	call uvputvrd(tno,'obsdec',dec,1)
	call uvputvrd(tno,'latitud',DPI/180.d0*lat,1)
        call uvputvrd(tno,'longitu',along,1)
	call uvputvra(tno,'telescop','AMiBA Simulator')
	call uvputvrr(tno,'inttime',inttime,1)
	call uvputvri(tno,'nchan',nchan,1)
        call uvputvri(tno,'nspect',1,1)
        call uvputvrd(tno,'sfreq',dble(sfreq),1)
        call uvputvrd(tno,'sdf',dble(sdf),1)
	call uvputvrd(tno,'restfreq',0.d0,1)
        call uvputvri(tno,'ischan',1,1)
        call uvputvri(tno,'nschan',nchan,1)
	call uvputvrr(tno,'jyperk',jyperk,1)
	call uvputvrr(tno,'pbfwhm',pbfwhm,1)
	if(nmod.eq.1)then
	  call uvputvri(tno,'npol',1,1)
	  call uvputvri(tno,'pol',1,1)
	else
	  call uvputvri(tno,'npol',4,1)
	endif
c
c  Initialise the dummy data.
c
	do k=1,nchan
	  flags(k) = .true.
	enddo
c
c  Now generate the data.
c
	call output('Starting model computation ...')
	offsrc = 0
	errcnt = 0
	nrec = int((Tstop - Tstart) / Tinc) + 1
	npr = max(10,nint(0.01*nrec))
	T = Tstart
	do k=1,nrec
	  call amComp(T,dT,uvw,psi,az,el,onflag,err)
	  if(err)then
	    errcnt = errcnt + 1
	  else if(onflag)then
	    call uvputvrr(tno,'dra',real(dT*DPI/12.d0*cos(dec)),1)
	    call uvputvrr(tno,'chi',psi,1)
	    ra0 = ra + dT*DPI/12.d0
	    dec0 = dec
	    call visNext(ra0,dec0)
c
	    systemp = Trec + Tsky/sin(el)
	    if(systemp.gt.0)call uvputvrr(tno,'systemp',systemp,1)
	    sigma = jyperk*systemp/sqrt(2*abs(sdf)*1e9*inttime)
	    do j=2,nant
	      do i=1,j-1
		u = uvw(1,j) - uvw(1,i)
		v = uvw(2,j) - uvw(2,i)
		w = uvw(3,j) - uvw(3,i)
		preamble(1) = u/(CMKS * 1e-9)
		preamble(2) = v/(CMKS * 1e-9)
		preamble(3) = w/(CMKS * 1e-9)
		preamble(4) = 365.25/366.25/24.0*T + T0
		preamble(5) = antbas(i,j)
c
		call visPnt(data,nchan,u,v,w,chat,dishdiam)
		if(nmod.eq.1)then
		  if(sigma.gt.0)call AddNoise(data,nchan,sigma)
		  call uvwrite(tno,preamble,data,flags,nchan)
		else
		  call writeout(tno,preamble,data,flags,nchan,nmod,
     *							psi,sigma,circ)
		endif
	      enddo
	    enddo
	  else
	    offsrc = offsrc + 1
	  endif
	  T = T + Tinc
	  if(mod(k,npr).eq.0)then
	    id = nint(100.0*real(k)/nrec)
	    if(id.lt.100)
     *		call output(stcat(' Completed '//itoaf(id),'% ...'))
	  endif
	enddo
c
	call visFin
	call hisclose(tno)
	call uvclose(tno)
	end
c************************************************************************
	subroutine getopt(dopara,circ)
c
	implicit none
	logical dopara,circ
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=2)
	character opts(NOPTS)*16
	logical present(NOPTS)
c
	data opts/'noparallactify  ','linear          '/
c
	call options('options',opts,present,NOPTS)
	dopara = .not.present(1)
	circ   = .not.present(2)
	end
c************************************************************************
	subroutine writeout(tno,preamble,data,flags,nchan,nmod,
     *	  psi,sigma,circ)
c
	implicit none
	integer nchan,nmod,tno
	real psi,sigma
	double precision preamble(5)
	complex data(nchan,nmod)
	logical flags(nchan)
	logical circ
c
c  Generate the correct correlation type from the data.
c------------------------------------------------------------------------
	integer XX,YY,XY,YX
	parameter(XX=1,YY=2,XY=3,YX=4)
	include 'maxdim.h'
	real cos2chi,sin2chi
	integer i,off
	complex out(MAXCHAN,4),W,iW
	if(nchan.gt.MAXCHAN)call bug('f','Too many channels')
	if(nmod.ne.3)call bug('f','Invalid number of models')
c
	cos2chi = cos(2*psi)
	sin2chi = sin(2*psi)
	if(circ)then
	  off = 0
	  W = cmplx(cos2chi,sin2chi)
	  iW = cmplx(-sin2chi,cos2chi)
	  do i=1,nchan
	    out(i,XX) = data(i,1)
	    out(i,YY) = data(i,1)
	    out(i,XY) = W*data(i,2) - iW*data(i,3)
	    out(i,YX) = conjg(W)*data(i,2)-conjg(iW)*data(i,3)
	  enddo
	else
	  off = -4
	  do i=1,nchan
	    out(i,XX) = data(i,1)+cos2chi*data(i,2)+sin2chi*data(i,3)
	    out(i,YY) = data(i,1)-cos2chi*data(i,2)-sin2chi*data(i,3)
	    out(i,XY) =          -sin2chi*data(i,2)+cos2chi*data(i,3)
	    out(i,YX) = out(i,XY)
	  enddo
	endif
c
	do i=1,4
	  if(sigma.gt.0)call AddNoise(out(1,i),nchan,sigma)
	  call uvputvri(tno,'pol',off-i,1)
	  call uvwrite(tno,preamble,out(1,i),flags,nchan)
	enddo
c
	end
c************************************************************************
	subroutine AddNoise(data,nchan,sigma)
c
	implicit none
	real sigma
	integer nchan
	complex data(nchan)
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i
	complex noise(MAXCHAN)
c
	if(nchan.gt.MAXCHAN)call bug('f','Too many channels')
	call gaus(noise,2*nchan)
	do i=1,nchan
	  data(i) = data(i) + sigma*noise(i)
	enddo
c
	end
