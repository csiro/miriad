c************************************************************************
	program mfboot
	implicit none
c
c= mfboot -- Set the flux scale of a visibility dataset.
c& rjs
c: calibration
c+
c	MFBOOT is a MIRIAD program which corrects the flux scale in
c	visibility datasets. In doing this, it assumes that the flux
c	density scale is out by a constant scale factor. MFBOOT computes
c	and applies a scale factor to the calibration tables of visibility
c	datasets to account for this.
c
c	To determine this factor, MFBOOT compares the flux 
c	density of measured visibilities with models of the expected
c	visibility. The visibilities can correspond to measurements of
c	planets or point sources. For point sources, they can be either
c	standard calibrators with known flux density, or others. In the
c	latter case the user must specify its flux density.
c
c@ vis
c	Input visibility datasets. Several datasets can be given (wildcards
c	are supported).
c@ line
c	Normal uv data "line" parameter, with the normal defaults. See
c	the help on "line" for more information.
c@ select
c	Normal uv-selection parameter. This selects the data in the input
c	datasets to analyse. The data selected should consist of
c	a single planet or point source. See the help on ``select'' for more
c	information. For planets, you may wish to select just the shortest
c	spacing, where the planet is strongest.
c@ flux
c	This is used to specify the flux density, and will be used to either
c	give the flux density of a point source that is not known to MFBOOT,
c	or to override MFBOOT's built-in models. MFBOOT has built-in models
c	for a few calibrators as well as the planets - see calplot and plplt.
c
c	For a point source, this parameter gives the flux density of the
c	calibrator in Janskys. For a planet, this parameter gives the
c	brightness temperature of the planet in Kelvins.
c@ mode
c	MFBOOT can process the data in one of three modes: "triple", "scalar"
c	and "vector". The default is "triple" for point sources and "scalar"
c	for planets.
c
c	In "triple" mode, MFBOOT compares the triple product of the data
c	and model. The advantage of comparing triple products is that the
c	process is robust to phase errors, and the data do not need to be
c	phase calibrated. Because planets can be significantly resolved,
c	triple mode is often not appropriate for them. However if the
c	resolution is modest on the selected baselines, triple mode can
c	be used.
c
c	In "scalar" mode, the amplitude of the data and model are compared.
c	This is also robust to phase errors, but will experience a noise
c	bias when the signal to noise is poor. Planets are often sufficiently
c	strong that noise bias is not an issue.
c
c	In "vector" mode, the real part of the data is compare with the model.
c	The data need to be phase calibrated.
c@ clip
c	For planets, this parameter can be used to discard data for baselines
c	that are significantly resolved. Data for a particular baseline will 
c	be discarded when the expected flux density on that baseline is
c	less than the clip factor times the total flux density. The clip
c	parameter takes on values between 0 to 1.0, with the default being 0
c	(ie the default is to accept all data).
c@ device
c	PGPLOT device to plot the model data as well as the visibility
c	data. The default is to not produce a plot. No plot is produced
c	in triple mode.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	  noapply Do not apply the scale factor - just evaluate it.
c--
c  History:
c    rjs     15jan06 Original version adapted from plboot.
c    rjs     19jan06 Fix call to subroutine with //char*(*) arg.
c    rjs     09may06 Increase size of MAXPNT.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='mfBoot: version 1.0 19-Jan-06')
	integer MAXVIS,MAXPNT
	parameter(MAXVIS=32,MAXPNT=1000000)
c
	character vis(MAXVIS)*64,source*32,line*64,device*64,mode*8
	character psource*32
	logical noapply
	integer nvis,lVis,vsource,nchan,iplanet,i,n,nants
	real fac,m,s,uflux,clip
	double precision SumXX,SumXY,preamble(4),time0
	complex data(MAXCHAN),d
	logical flags(MAXCHAN)
	real xval(MAXPNT),ydval(MAXPNT),ymval(MAXPNT)
	integer npnt
c
	complex db(MAXBASE)
	real sb(MAXBASE),mb(MAXBASE)
	integer nb(MAXBASE),nbl
c
c  Externals.
c
	logical uvDatOpn,uvVarUpd,hdPrsnt
	integer plLook
	character streal*16
c
c  Get the user input.
c
	call output(version)
	call keyini
	call uvDatInp('vis','xcefdwl')
	call uvDatSet('stokes',0)
	call keya('device',device,' ')
	call keyr('flux',uflux,0.0)
	call keyr('clip',clip,0.0)
	call getopt(mode,noapply)
	call keyfin
c
c  Process the data.
c
	psource = ' '
	SumXX = 0
	SumXY = 0
	nvis = 0
	npnt = 0
c
	nbl = 0
	time0 = 0
	call intIni(nb,MAXBASE)
	dowhile(uvDatOpn(lVis))
	  nvis = nvis + 1
	  if(nvis.gt.MAXVIS)call bug('f','Too many inputs for me!')
	  call uvDatGta('name',vis(nvis))
	  call uvVarIni(lVis,vsource)
	  call uvVarSet(vsource,'source')
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
c
	    if(abs(preamble(3)-time0).gt.2/86400.0)then
	      if(nbl.gt.0)then
	        call intFlush(db,mb,sb,nb,nbl,SumXX,SumXY)
	        call intIni(nb,nbl)
	      endif
	      nbl = 0
	      time0 = preamble(3)
	    endif
c
	    if(uvVarUpd(vsource))then
	      call uvrdvra(lVis,'source',source,' ')
	      iplanet = plLook(source)
	      if(iplanet.lt.1.or.iplanet.gt.9.or.iplanet.eq.3)
     *						     iplanet = 0
	      if(source.ne.psource.and.psource.ne.' ')then
		call bug('w','Data also processed for '//source)
	      else if(psource.eq.' ')then
		call output('Found data for source '//source)
		if(mode.eq.' ')then
		  mode = 'scalar'
		  if(iplanet.eq.0)mode = 'triple'
		endif
	      endif
	    endif
c
	    call avdat(lVis,data,flags,nchan,source,
     *		preamble(3),preamble,iplanet,clip,uflux,d,m,s,n)
	    if(mode.eq.'triple')then
	      call intAcc(preamble(4),d,m,s,n,MAXBASE,db,mb,sb,nb,nbl)
	    else
	      call acc(mode.eq.'vector',preamble,d,m,s,n,
     *			SumXX,SumXY,MAXPNT,npnt,xval,ydval,ymval)
	    endif
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvDatCls
	enddo
	if(nbl.gt.0)call intFlush(db,mb,sb,nb,nbl,SumXX,SumXY)
c
	if(SumXX.le.0)call bug('f','No good data found')
	if(SumXY.le.0)call bug('f','Problem seems ill-conditioned')
	fac = SumXX/SumXY
	if(mode.eq.'triple')fac = fac ** (1.0/3.0)
	line = 'Scaling the data by '//streal(fac,'(f13.3)')
	call output(line)
c
c  Generate the plot, if needed.
c
	do i=1,npnt
	  ydval(i) = fac*ydval(i)
	enddo
	if(npnt.gt.0.and.device.ne.' ')
     *	  call plotit(xval,ydval,ymval,npnt,device)
c
c  Now apply the scale factor to all the input datasets.
c
	fac = sqrt(fac)
	if(.not.noapply)then
	  do i=1,nvis
	    call uvopen(lVis,vis(i),'old')
	    if(hdPrsnt(lVis,'gains'))then
	      call gainSca(lVis,fac)
	    else
	      call uvscan(lVis,'baseline')
	      call uvrdvrd(lVis,'time',time0,0.d0)
	      call uvrdvri(lVis,'nants',nants,0)
	      call gainWri(lVis,fac,time0,nants)
	    endif
	    call hisopen(lVis,'append')
	    call hiswrite(lVis,'MFBOOT: Miriad '//version)
	    call hisinput(lVis,'MFBOOT')
	    call hiswrite(lVis,'MFBOOT: '//line)
	    call hisclose(lVis)
	    call uvclose(lVis)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine intIni(nb,nbl)
c
	implicit none
	integer nbl,nb(nbl)
c------------------------------------------------------------------------
	integer i
c
	do i=1,nbl
	  nb(i) = 0
	enddo
	end
c************************************************************************
	subroutine intAcc(bl,d,m,s,n,MAXBASE,db,mb,sb,nb,nbl)
c
	implicit none
	double precision bl
	integer MAXBASE,nbl
	complex d,db(MAXBASE)
	real s,sb(MAXBASE),m,mb(MAXBASE)
	integer n,nb(MAXBASE)
c------------------------------------------------------------------------
	integer i1,i2,i
c
	call basant(bl,i1,i2)
	i = ((i2-1)*(i2-2))/2 + i1
	if(i.gt.MAXBASE)call bug('f','Buffer overflow in intAcc')
	nbl = max(nbl,i)
	if(nb(i).eq.0)then
	  db(i) = 0
	  mb(i) = 0
	  nb(i) = 0
	  sb(i) = 0
	endif
	db(i) = db(i) + d
	mb(i) = mb(i) + m
	sb(i) = sb(i) + s
	nb(i) = nb(i) + n
	end
c************************************************************************
	subroutine intFlush(db,mb,sb,nb,nbl,SumXX,SumXY)
c
	implicit none
	integer nbl
	integer nb(nbl)
	real mb(nbl),sb(nbl)
	complex db(nbl)
	double precision SumXX,SumXY
c------------------------------------------------------------------------
	integer b12,b23,b13,i1,i2,i3
	logical more,ok
	real tm,sigma2
	complex td
c
	i3 = 3
	more = .true.
	dowhile(more)
          do i2=2,i3-1
            do i1=1,i2-1
              b12 = ((i2-1)*(i2-2))/2 + i1
              b13 = ((i3-1)*(i3-2))/2 + i1
              b23 = ((i3-1)*(i3-2))/2 + i2
	      more = b23.le.nbl
	      ok = more
	      if(more)ok = nb(b12).gt.0.and.nb(b13).gt.0.and.
     *			   nb(b23).gt.0
	      if(ok)then
		td =  db(b12)*db(b23)*conjg(db(b13))/
     *		     (nb(b12)*nb(b23)*      nb(b13))
		tm =  mb(b12)*mb(b23)*      mb(b13) /
     *		     (nb(b12)*nb(b23)*      nb(b13))
		sigma2 = 1
	        SumXX = SumXX +     (tm*tm/sigma2)
	        SumXY = SumXY + real(tm*td/sigma2)
	      endif
	    enddo
	  enddo
	  i3 = i3 + 1
	enddo

	end
c************************************************************************
	subroutine avdat(lVis,data,flags,nchan,source,time,uv,iplanet,
     *						clip,uflux,d,m,s,n)
c
	implicit none
	integer lVis,nchan
	complex data(nchan)
	logical flags(nchan)
	character source*(*)
	double precision time,uv(2)
	integer iplanet
	real uflux,clip
c
	complex d
	real m,s
	integer n
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	double precision sfreq(MAXCHAN)
	real model(MAXCHAN)
	integer i,ierr
	real a,b,cospa,sinpa,pltb,bmaj,bmin,bpa,rms2
        double precision sub(3),dist
	logical ok
	character line*80
c
c  Externals.
c
        real j1xbyx,pltbs
        double precision deltime
c
	if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	call uvinfo(lVis,'sfreq',sfreq)
c
c  Work out the model of the visibility data.
c
	ok = .true.
	if(iplanet.ne.0)then
	  if(uflux.gt.0)then
	    pltb = uflux
	  else
	    pltb = pltbs(iplanet,real(sfreq(1)))
	  endif
c
	  call plpar(time+deltime(time,'tdb'),iplanet,sub,
     *                                          dist,bmaj,bmin,bpa)
	  cospa = cos(bpa)
	  sinpa = sin(bpa)
	  b = PI * sqrt((bmaj*(uv(1)*cospa-uv(2)*sinpa))**2
     *              + (bmin*(uv(1)*sinpa+uv(2)*cospa))**2)/sfreq(1)
          a = 2 * pltb * (KMKS*1e18)/(CMKS*CMKS*1e-26)
     *          * 2 * PI/4 * bmaj*bmin
	  do i=1,nchan
	    model(i) = a*sfreq(i)*sfreq(i)*j1xbyx(real(b*sfreq(i)))
	  enddo
	  ok = abs(model(1)).ge.abs(clip*a*sfreq(1)*sfreq(1)*0.5)
	else if(uflux.gt.0)then
	  do i=1,nchan
	    model(i) = uflux
	  enddo
	else
	  call calstoke(source,'i',sfreq,model,nchan,ierr)
	  if(ierr.gt.1)then
	    line = 'Source is not recognised: '//source
	    call bug('f',line)
	  endif
	endif
c
	d = 0
	m = 0
	s = 0
	n = 0
	if(ok)then
	  call uvdatGtr('variance',rms2)
	  do i=1,nchan
	    if(flags(i))then
	      d = d + data(i)
	      m = m + model(i)
	      s = s + rms2
	      n = n + 1
	    endif
	  enddo
	endif
c
	end
c************************************************************************
	subroutine plotit(xval,ydval,ymval,npnt,device)
c
	implicit none
	integer npnt
	character device*(*)
	real xval(npnt),ydval(npnt),ymval(npnt)
c
c  Generate a plot.
c
c------------------------------------------------------------------------
	integer i
	real xmin,xmax,ymin,ymax,xlo,xhi,ylo,yhi
c
c  Externals.
c
	integer pgbeg
c
	xmin = xval(1)
	xmax = xmin
	ymin = ydval(1)
	ymax = ymin
c
	do i=1,npnt
	  xmin = min(xmin,xval(i))
	  xmax = max(xmax,xval(i))
	  ymin = min(ymin,ydval(i),ymval(i))
	  ymax = max(ymax,ydval(i),ymval(i))
	enddo
c
       if(pgbeg(0,device,1,1).ne.1)then
          call pgldev
          call bug('f','Error opeing graphics device')
        endif
        call pgscf(2)
        call pgrnge(0.0,xmax,xlo,xhi)
        call pgrnge(ymin,ymax,ylo,yhi)
        call pgpage
        call pgvstd
        call pgswin(xlo,xhi,ylo,yhi)
        call pgbox('BCNST',0.,0,'BCNST',0.,0)
        call pgpt(npnt,xval,ydval,1)
	call pgsci(2)
	call pgpt(npnt,xval,ymval,1)
	call pgsci(1)
        call pglab('Baseline Length (k\gl)','Flux Density (Jy)',' ')
        call pgend
c
	end
c************************************************************************
	subroutine Acc(vector,uv,data,model,sigma2,n,SumXX,SumXY,
     *		MAXPNT,npnt,xval,ydval,ymval)
c
	implicit none
	logical vector
	complex data
	real model,sigma2
	integer n
	double precision uv(2),SumXX,SumXY
	integer MAXPNT,npnt
	real xval(MAXPNT),ydval(MAXPNT),ymval(MAXPNT)
c
c  Accumulate info given the planetary data.
c------------------------------------------------------------------------
        include 'mirconst.h'
c
	if(n.gt.0)then
	  npnt = npnt + 1
	  if(npnt.gt.MAXPNT)call bug('f','Too many points')
	  xval(npnt) = 1e-3*sqrt(uv(1)*uv(1) + uv(2)*uv(2))
	  if(vector)then
	    ydval(npnt) = real(data/n)
	    ymval(npnt) = real(model/n)
	    SumXX = SumXX +     (model*model/sigma2)
	    SumXY = SumXY + real(model*data/sigma2)
	  else
	    ydval(npnt) = abs(data/n)
	    ymval(npnt) = abs(model/n)
	    SumXX = SumXX + abs(model*model/sigma2)
	    SumXY = SumXY + abs(model*data/sigma2)
	  endif
	endif
c
        end
c************************************************************************
	subroutine getopt(mode,noapply)
c
	implicit none
	logical noapply
	character mode*(*)
c
c  Get extra processing parameters.
c------------------------------------------------------------------------
	integer NMODES
	parameter(NMODES=3)
	character modes(NMODES)*8
	integer nout
c
	integer NOPTS
	parameter(NOPTS=1)
	logical present(NOPTS)
	character opts(NOPTS)*8
c
	data modes/'triple  ','scalar  ','vector  '/
	data opts/'noapply '/
c
	call options('options',opts,present,NOPTS)
	noapply = present(1)
c
	call keymatch('mode',NMODES,modes,1,mode,nout)
	if(nout.eq.0)mode = ' '
	end
c************************************************************************
	subroutine gainSca(lVis,fac)
c
	implicit none
	integer lVis
	real fac
c
c  Scale the gains table present in the data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex Gains(3*MAXANT)
	real scale(3*MAXANT)
	integer item,iostat,nfeeds,ntau,nsols,ngains,offset,i,j
c
c  Externals.
c
	integer hsize
c
	call haccess(lVis,item,'gains','append',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening to modify gains item')
	  call bugno('f',iostat)
	endif
	call rdhdi(lVis,'nfeeds',nfeeds,1)
	call rdhdi(lVis,'ntau',ntau,0)
	call rdhdi(lVis,'ngains',ngains,0)
	if(mod(ngains,nfeeds+ntau).ne.0)
     *	  call bug('f','Bad number of gains or feeds in table')
	nsols = hsize(item)
	if(mod(nsols-8,8*ngains+8).ne.0)
     *	  call bug('f','Size of gain table looks wrong')
	nsols = (nsols-8)/(8*ngains+8)
c
	if(ngains.gt.3*MAXANT)call bug('f','Too many gains for me!')
	do i=1,ngains,nfeeds+ntau
	  scale(i) = fac
	  if(nfeeds.eq.2)scale(i+1) = fac
	  if(ntau.eq.1)scale(i+nfeeds) = 1
	enddo
c
c  Now correct the data.
c
	offset = 16
	do i=1,nsols
	  call hreadr(item,gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)call bugno('f',iostat)
	  do j=1,ngains
	    gains(j) = scale(j)*gains(j)
	  enddo
	  call hwriter(item,gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)call bugno('f',iostat)
	  offset = offset + 8 + 8*ngains
	enddo
c
	call hdaccess(item,iostat)
	end
c************************************************************************
	subroutine gainWri(lVis,fac,time,nants)
c
	implicit none
	real fac
	double precision time
	integer nants,lVis
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex gains(MAXANT)
	integer header(2),i,item,iostat
c
	do i=1,nants
	  gains(i) = fac
	enddo
c
c  Create the gains table.
c
	call wrhdd(lVis,'interval',2.d0)
	call wrhdi(lVis,'ngains',nants)
	call wrhdi(lVis,'nsols',1)
	call wrhdi(lVis,'ntau',0)
	call wrhdi(lVis,'nfeeds',1)
	call haccess(lVis,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
c
	header(1) = 0
	header(2) = 0
	call hwritei(item,header,0,8,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hwrited(item,time,8,8,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hwriter(item,gains,16,8*nants,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hdaccess(item,iostat)
	end
