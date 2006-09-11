c************************************************************************
	program calred
	implicit none
c
c= calred
c& rjs
c: uv analysis, plotting
c+
c	CALRED is a program used to analyse flux densities of sources.
c@ vis
c	The input visibility datasets. Several datasets can be given.
c@ select
c	Standard visibility selection. See help on "select" for more
c	information.
c@ line
c	Standard visibility linetype. See the help "line" for more information.
c@ stokes
c	Normal Stokes/polaization selection. The default is to process all
c	parallel-hand polarisation.
c@ options
c	  triple    Do triple processing.
c	  nocal     Do not perform gain calibration.
c	  nopol     Do not perform polarisation calibration on the data.
c	  nopass    Do not perform bandpass calibration on the data.
c--
c  History:
c    rjs  23feb00 Original version.
c    rjs  04feb01 Some small tidying.
c    rjs  02apr02 Get it to work for Stokes parameters.
c    rjs  04apr02 Realy fix it this time.
c    rjs  08apr02 Allow negative values when taking cube roots,
c		  and print out number of triples.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer MAXDAT,MAXSRC,MAXPOL
	integer PolMin,PolMax
	character version*(*)
	parameter(version='version 08-Apr-02')
	parameter(MAXDAT=15,MAXPOL=2,MAXSRC=1024)
	parameter(PolMin=-8,PolMax=4)
c
	logical dotrip,polp,dopara
	character uvflags*16,line*80,con*6
	character sources(MAXSRC)*16
	real scat2,SSms,flux,flux2,SSmm,rp,ip
	integer ncorr
	complex SSdm
	integer isrc,nsrc,iplanet,tno,vsource,pnt1,pnt2,pnt3
c
	real Smm(MAXDAT,MAXSRC),Sms(MAXDAT,MAXSRC),Sdd(MAXDAT,MAXSRC)
	complex Sdm(MAXDAT,MAXSRC)
	integer npnt(MAXDAT,MAXSRC)
	integer indx(MAXBASE,MAXPOL)
c
	integer nread,i,j
	integer npol,polcvt(PolMin:PolMax),p,ant1,ant2,nants,bl,ndat
	double precision preamble(4),tprev,t0,interval
	logical first,newsrc,doflush
	complex data(MAXCHAN)
	logical flag(MAXCHAN)
c
c  Integration buffers.
c
	integer corrpnt(MAXBASE,MAXPOL),flagpnt(MAXBASE,MAXPOL)
	integer modpnt(MAXBASE,MAXPOL)
	integer nchan(MAXBASE,MAXPOL)
	logical init(MAXBASE,MAXPOL)
	real sigma2(MAXBASE,MAXPOL)
c
c  Externals.
c
	logical uvDatOpn,uvVarUpd
c
c Lets go! Get user inputs.
c
	call output('Calred: '//version)
	call keyini
	call GetOpt(dotrip,uvflags)
	call uvDatInp('vis',uvflags)
	call keyd('interval',interval,5.d0)
	interval = interval / (24.d0*60.d0)
	call keyfin
c
	call uvDatGti('npol',npol)
	dopara = npol.eq.0
c
c  Initialise the integration buffers.
c
	do j=1,MAXPOL
	  do i=1,MAXBASE
	    nchan(i,j) = 0
	    init(i,j) = .false.
	    indx(i,j) = 0
	  enddo
	enddo
c
	do j=1,MAXSRC
	  do i=1,MAXDAT
	    npnt(i,j) = 0
	  enddo
	enddo
c
c Miscellaneous initialisation.
c
	first = .true.
c
	isrc = 0
	nsrc = 0
	ndat = 0
	nants = 0
	npol = 0
c
	do i=PolMin,PolMax
	  polcvt(i) = 0
	enddo
c
	dowhile(uvDatOpn(tno))
	  call uvVarIni(tno,vsource)
	  call uvVarSet(vsource,'source')
	  call uvDatRd(preamble,data,flag,MAXCHAN,nread)
	  dowhile(nread.gt.0)
	    ip = iSrc
	    p = 0
	    call basant(preamble(4),ant1,ant2)
	    if(min(ant1,ant2).ge.1.and.max(ant1,ant2).le.MAXANT.and.
     *	        ant1.ne.ant2)
     *		call PolIdx(p,npol,dopara,polcvt,PolMin,PolMax,MAXPOL,
     *		polp)
c
c  Handle time information, and do fiddles for the first time through.
c
	    if(p.gt.0)then	
	      bl = (ant2-1)*(ant2-2)/2 + ant1
	      if(first)then
	        tprev = preamble(3)
		t0 = tprev
		first = .false.
	      endif
c
c  Flush the integration buffers if needed.
c
	      newSrc = uvVarUpd(vsource)
	      doflush = nants.gt.0.and.(newSrc.or.
     *		abs(preamble(3)-tprev).gt.1./(3600.*24.))
	      if(doflush)then
		call IntFlush(nants,npol,dotrip,init,indx,sigma2,ndat,
     *		  memc,corrpnt,memr,modpnt,meml,flagpnt,
     *		  nchan,MAXBASE,MAXPOL,MAXDAT,npnt(1,isrc),
     *		  Smm(1,isrc),Sms(1,isrc),Sdd(1,isrc),Sdm(1,isrc))
		nants = 0
		tprev = preamble(3)
	      endif
	      if(abs(preamble(3)-t0).gt.interval)iSrc = 0
c
c  Get the new source name.
c
	      if(newSrc.or.iSrc.eq.0)
     *		call GetSrc(tno,Sources,MAXSRC,nsrc,isrc,iplanet)
	      if(ip.ne.iSrc)t0 = preamble(3)
c
c  Save this baseline.
c
	      nants = max(nants,ant1,ant2)
c
	      if(nchan(bl,p).ne.nread)then
		if(nchan(bl,p).gt.0)then
		  call MemFree(corrpnt(bl,p),nchan(bl,p),'c')
		  call MemFree(flagpnt(bl,p),nchan(bl,p),'l')
		  call MemFree(modpnt(bl,p),nchan(bl,p),'r')
		endif
		nchan(bl,p) = nread
		call MemAlloc(corrpnt(bl,p),nchan(bl,p),'c')
		call MemAlloc(flagpnt(bl,p),nchan(bl,p),'l')
		call MemAlloc(modpnt(bl,p),nchan(bl,p),'r')
	      endif
c
	      pnt1 = corrpnt(bl,p) - 1
	      pnt2 = flagpnt(bl,p) - 1
	      pnt3 = modpnt(bl,p) - 1
	      do i=1,nchan(bl,p)
		memc(pnt1+i) = data(i)
		meml(pnt2+i) = flag(i)
		memr(pnt3+i) = 1
	      enddo
	      if(iplanet.gt.0.and.polp)
     *		call SetPl(tno,preamble,preamble(3),iplanet,
     *		  memr(pnt3+1),nchan(bl,p))
	      call uvDatGtr('variance',sigma2(bl,p))
	      init(bl,p) = .true.
	    endif
	    call uvDatRd(preamble,data,flag,MAXCHAN,nread)
	  enddo
	  call uvDatCls
	enddo
c
c  Flush the last integration.
c
	if(nants.gt.0)then
	  call IntFlush(nants,npol,dotrip,init,indx,sigma2,ndat,
     *	    memc,corrpnt,memr,modpnt,meml,flagpnt,
     *	    nchan,MAXBASE,MAXPOL,MAXDAT,npnt(1,isrc),
     *	    Smm(1,isrc),Sms(1,isrc),Sdd(1,isrc),Sdm(1,isrc))
	endif
c
c  We have grabbed and averaged all the relevant data.
c
	if(dotrip)then
	  call output('Source        Sca(mJy)  Tri(mJy) Con  '//
     *						'The  Act   NCorr')
	else
	  call output('Source        Sca(mJy)  Vec(mJy) Con  '//
     *						'The  Act   NCorr')
	endif
	do j=1,nsrc
	  scat2 = 0
	  ncorr = 0
	  SSdm = 0
	  SSmm = 0
	  SSms = 0
	  flux2 = 0
	  do i=1,ndat
	    if(npnt(i,j).gt.0)then
	      rp = real(Sdm(i,j))
	      ip = aimag(Sdm(i,j))
	      scat2 = scat2 + Sdd(i,j) - (rp*rp+ip*ip)/Smm(i,j)
	      flux2 = flux2 + Sdd(i,j)
	      ncorr= ncorr + npnt(i,j)
	      SSms = SSms + Sms(i,j)
	      SSmm = SSmm + Smm(i,j)
	      if(dotrip)then
	        SSdm = SSdm + Sdm(i,j)
	      else
		SSdm = SSdm + sqrt(rp*rp+ip*ip)
	      endif
	    endif
	  enddo
	  if(SSmm.gt.0)then
	    SSdm = SSdm / SSmm
	    SSms = sqrt(SSms/SSmm)
	    scat2 = sqrt(scat2/(2*ncorr))
	    flux2 = sqrt(flux2/ncorr)
	    if(dotrip)then
	      flux = real(SSdm)
	      flux = sign(abs(flux)** 0.333333,flux)
	      flux2 = flux2 ** 0.333333
	      SSms  = SSms/sqrt(3.0)
	      if(flux.gt.3*SSms)then
		scat2 = scat2/(sqrt(3.0)*flux*flux)
	      else
		scat2 = scat2**0.3333
	      endif
	      write(con,'(i6)')
     *		nint(100*min(abs(aimag(SSdm)/real(SSdm)),1.))
	    else
	      flux = SSdm
	      con = '     -'
	    endif
	    write(line,10)sources(j),nint(1000*flux2),
     *			nint(1000*flux),con,
     *			nint(1000*SSms),nint(1000*scat2),
     *			ncorr
  10	    format(a14,i7,i9,a6,i6,i6,i6)
	    call output(line)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(dotrip,uvflags)
c
	implicit none
	logical dotrip
	character uvflags*(*)
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=4)
	character opts(NOPTS)*9
	logical present(NOPTS)
	data opts/'triple   ','nocal    ','nopol    ','nopass   '/
c
	call options('options',opts,present,NOPTS)
	dotrip = present(1)
c
c c -- docal
c f -- dopass
c e -- dopol
c
	uvflags = 'dslx'
	if(.not.present(2))uvflags(5:5) = 'c'
	if(.not.present(3))uvflags(6:6) = 'e'
	if(.not.present(4))uvflags(7:7) = 'f'
	end
c************************************************************************
	subroutine PolIdx(p,npol,dopara,polcvt,PolMin,PolMax,MAXPOL,
     *								polp)
c
	implicit none
	integer p,PolMin,PolMax,npol,MAXPOL
	integer polcvt(PolMin:PolMax)
	logical polp,dopara
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
	  if((dopara.and.PolsPara(pol)).or..not.dopara)then
	    npol = npol + 1
	    if(npol.gt.MAXPOL)call bug('f','Too many polarisations')
	    polcvt(pol) = npol
	  endif
	endif
	p = polcvt(pol)
	polp = PolsPara(pol)
	end
c************************************************************************
	subroutine SetPl(tno,uv,time,iplanet,model,nchan)
c
	implicit none
	integer tno,iplanet,nchan
	double precision uv(2),time
	real model(nchan)
c
c  Determine the model visibility of a planet.c
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer i
	real a,b,cospa,sinpa,pltb,bmaj,bmin,bpa
	double precision freq(MAXCHAN),sub(3),dist
c
c  Externals.
c
	real j1xbyx,pltbs
	double precision deltime
c
	call plpar(time+deltime(time,'tdb'),iplanet,sub,
     *						dist,bmaj,bmin,bpa)
	call uvinfo(tno,'sfreq',freq)
	pltb = pltbs(iplanet,real(freq(1)))
        cospa = cos(bpa)
        sinpa = sin(bpa)
        b = PI * sqrt((bmaj*(uv(1)*cospa-uv(2)*sinpa))**2
     *              + (bmin*(uv(1)*sinpa+uv(2)*cospa))**2)
        a = 2 * pltb * (KMKS*1e18)/(CMKS*CMKS*1e-26)
     *    * 2 * PI/4 * bmaj*bmin
        do i=1,nchan
          model(i) = a*freq(i)*freq(i)*j1xbyx(real(b*freq(i)))
	enddo
c
	end
c************************************************************************
	subroutine GetSrc(tno,sources,maxsrc,nsrc,isrc,iplanet)
c
	implicit none
	integer tno,maxsrc,nsrc,isrc,iplanet
	character sources(MAXSRC)*(*)
c------------------------------------------------------------------------
	logical found
	character source*32
	integer NPLANETS
	parameter(NPLANETS=7)
	integer i
	character planets(NPLANETS)*8
	save planets
	data planets/'mercury ','venus   ','earth   ','mars    ',
     *		     'jupiter ','saturn  ','uranus  '/
c
	call uvrdvra(tno,'source',source,' ')
	if(isrc.ne.0)then
	  found = sources(isrc).eq.source
	else
	  found = .false.
	endif
c
c  Add a new source, if needed.
c
	if(.not.found)then
	  nsrc = nsrc + 1
	  if(nsrc.gt.MAXSRC)call bug('f','Too many sources')
	  sources(nsrc) = source
	  isrc = nsrc
	endif
c
c  Determine whether its a planet or not.
c
	iplanet = 0
	call lcase(source)
	do i=1,NPLANETS
	  if(source.eq.planets(i))iplanet = i
	enddo
c
	end
c************************************************************************
	subroutine IntFlush(nants,npol,dotrip,init,indx,sigma2,ndat,
     *	  Corrs,CorrPnt,Modl,ModPnt,Flags,FlagPnt,
     *	  nchan,maxbase,maxpol,maxdat,npnt,Smm,Sms,Sdd,Sdm)
c
	implicit none
	integer nants,npol,maxbase,maxpol,maxdat,ndat
	integer indx(maxbase,maxpol)
	integer CorrPnt(maxbase,maxpol),FlagPnt(maxbase,maxpol)
	integer ModPnt(maxbase,maxpol)
	integer nchan(maxbase,maxpol)
	logical dotrip,init(maxbase,maxpol)
	complex Corrs(*)
	logical Flags(*)
	real Modl(*),sigma2(maxbase,maxpol)
	integer npnt(maxdat)
	real Smm(maxdat),Sms(maxdat),Sdd(maxdat)
	complex Sdm(maxdat)
c------------------------------------------------------------------------
	integer p,i3,i2,i1,bl12,bl13,bl23,k,id,n,i
	integer pflag12,pflag13,pflag23,pdata12,pdata23,pdata13
	integer pmod12,pmod23,pmod13,pflag,pdata,pmod
	complex data
	real model
c
	do p=1,npol
c
c  Triple quantity.
c
	  if(dotrip)then
	    k = 1
	    ndat = 1
	    do i3=3,nants
	      do i2=2,i3-1
	        do i1=1,i2-1
	          bl12 = ((i2-1)*(i2-2))/2 + i1
	          bl13 = ((i3-1)*(i3-2))/2 + i1
	          bl23 = ((i3-1)*(i3-2))/2 + i2
		  if(init(bl12,p).and.init(bl13,p).and.init(bl23,p))then
		    n = min(nchan(bl12,p),nchan(bl13,p),nchan(bl23,p))
		    pdata12 = corrpnt(bl12,p) - 1
		    pdata13 = corrpnt(bl13,p) - 1
		    pdata23 = corrpnt(bl23,p) - 1
		    pflag12 = flagpnt(bl12,p) - 1
		    pflag13 = flagpnt(bl13,p) - 1
		    pflag23 = flagpnt(bl23,p) - 1
		    pmod12  = modpnt(bl12,p) - 1
		    pmod13  = modpnt(bl13,p) - 1
		    pmod23  = modpnt(bl23,p) - 1
		    do i=1,n
		      if(Flags(pflag12+i).and.Flags(pflag13+i).and.
     *		         Flags(pflag23+i))then
		        if(npnt(k).eq.0)then
		          Smm(k) = 0
		          Sdm(k) = 0
		          Sdd(k) = 0
		          Sms(k) = 0
		        endif
			model = Modl(pmod12+i)*
     *			        Modl(pmod23+i)*Modl(pmod13+i)
			data  = Corrs(pdata12+i)*
     *                          Corrs(pdata23+i)*conjg(Corrs(pdata13+i))
			npnt(k) = npnt(k) + 1
			Smm(k) = Smm(k) + model*model
		        Sdm(k) = Sdm(k) + model*data
			Sdd(k) = Sdd(k) + real(data)**2+aimag(data)**2
			Sms(k) = Sms(k) + 
     *		      (Modl(pmod12+i)*Modl(pmod13+i))**2*sigma2(bl23,p)+
     *		      (Modl(pmod23+i)*Modl(pmod12+i))**2*sigma2(bl13,p)+
     *		      (Modl(pmod13+i)*Modl(pmod23+i))**2*sigma2(bl12,p)
		      endif
		    enddo
		  endif
	        enddo
	      enddo
	    enddo
c
c  Do stragith baseline processing.
c
	  else
	    id = 0
	    do i2=2,nants
	      do i1=1,i2-1
		id = id + 1
		if(init(id,p))then
	  	  if(indx(id,p).eq.0)then
		    ndat = ndat + 1
		    if(ndat.gt.MAXDAT)call bug('f','Too many thingos')
		    indx(id,p) = ndat
		  endif
		  k = indx(id,p)
		  n = nchan(id,p)
		  pdata = corrpnt(id,p) - 1
		  pflag = flagpnt(id,p) - 1
		  pmod  = modpnt(id,p) - 1
		  do i=1,n
		    if(Flags(pflag+i))then
		      if(npnt(k).eq.0)then
		        Smm(k) = 0
		        Sdm(k) = 0
		        Sdd(k) = 0
		        Sms(k) = 0
		      endif
		      model = Modl(pmod+i)
		      data  = Corrs(pdata+i)
		      npnt(k) = npnt(k) + 1
		      Smm(k) = Smm(k) + model*model
		      Sdm(k) = Sdm(k) + model*data
		      Sdd(k) = Sdd(k) + real(data)**2+aimag(data)**2
		      Sms(k) = Sms(k) + model*model*sigma2(id,p)
		    endif
		  enddo
		endif
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
