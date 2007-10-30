c************************************************************************
c  A set of routines, used by INVERT, to build up the header of a dataset.
c
c  History:
c    25oct94 rjs  Original version.
c    23nov94 rjs  Fix labelling of velocity axis for velocity linetype.
c    25apr95 rjs  Write "object" rather than "source" item.
c     1nov95 rjs  Added HdDefSiz
c     1jul99 rjs  Create somewhat better headers.
c    28jul99 rjs  Initialise pbtype.
c************************************************************************
	subroutine HdInit(mfs1,mosaic1)
c
	implicit none
	logical mfs1,mosaic1
c------------------------------------------------------------------------
	include 'hdtab.h'
	mfs = mfs1
	mosaic = mosaic1
	doinit = .true.
	end
c************************************************************************
	subroutine HdChk(tno)
c
	implicit none
	integer tno
c
c  Check that the header is the same.
c------------------------------------------------------------------------
	include 'mirconst.h'
	real tol
	parameter(tol=pi/180./3600.)
	include 'hdtab.h'
	double precision t1,dv,v0,epsi,r0,line(6),dtemp
	real t2,rtemp,vsource
	character s*16
c
c  Get all the values we can possibly want when we see a file for the
c  first time.
c
	if(doinit)then
	  call HdFirst(tno)
	  doinit = .false.
	else if(Count.eq.0)then
c
c  Form an average time and observatory velocity.
c
	  call uvrdvrd(tno,'time',dtemp,0.d0)
	  obstime = obstime + dtemp
	  call uvrdvrr(tno,'vsource',vsource,0.0)
	  call uvrdvrr(tno,'veldop',rtemp,0.0)
	  vobs = vobs  + rtemp - vsource
	  naver = naver + 1
c
c  Check for a change in the spectral axis.
c
	  if(ctype3(1:4).eq.'VELO')then
	    call uvinfo(tno,'line',line)
	    nchan = nint(line(2))
	    if(.not.RChange)then
	      call uvfit1(tno,'restfreq',nchan,r0,epsi)
	      RConst = RConst.and.epsi.lt.0.001*restfreq
	      RChange = abs(restfreq-r0).gt.0.001*restfreq
	    endif
	    if(.not.VChange)then
	      if(nchan.eq.1)then
	        call uvinfo(tno,'velocity',v0)
		call uvinfo(tno,'bandwidth',dv)
		dv = 0.001 * CMKS * dv / restfreq
	      else
		call uvfit2(tno,'velocity',nchan,dv,v0,epsi)
		VLinear = VLinear.and.(epsi.lt.0.1*abs(cdelt3))
	      endif
	      VChange = max(abs(crval3-v0),abs(cdelt3-dv)).gt.
     *						0.1*abs(cdelt3)
	    endif
	  endif
c
c  Check for a change in RA.
c
	  if(.not.XChange.and..not.mosaic)then
	    call uvrdvrd(tno,'dec',t1,0.d0)
	    call uvrdvrr(tno,'dra',t2,0.0)
	    t2 = t2 / cos(t1)
	    call uvrdvrd(tno,'ra',t1,0.d0)
	    t1 = t1 + t2
	    XChange = abs(crval1-t1).gt.tol
	  endif
c
c  Check for a change in DEC.
c
	  if(.not.YChange.and..not.mosaic)then
	    call uvrdvrd(tno,'dec',t1,0.d0)
	    call uvrdvrr(tno,'ddec',t2,0.0)
	    t1 = t1 + t2
	    YChange = abs(crval2-t1).gt.tol
	  endif
c
c  Check for a change in source name.
c
	  if(.not.SChange)then
	    call uvrdvra(tno,'source',s,source)
	    SChange = s.ne.source
	  endif
	endif
c
	Count = mod(Count+1,MAXCOUNT)
	end
c************************************************************************
	subroutine HdFirst(tno)
c
	implicit none
	integer tno
c------------------------------------------------------------------------
	integer CHANNEL,WIDE,VELOCITY
	parameter(CHANNEL=1,WIDE=2,VELOCITY=3)
	include 'hdtab.h'
	include 'mirconst.h'
	double precision line(6),epsi
	real vsource,dra,ddec
	integer itype
	logical ew
c
c  Initialise all the logicals to check whether things are consistent.
c
	VChange = .false.
	VLinear = .true.
	Rconst  = .true.
	RChange = .false.
	XChange = .false.
	YChange = .false.
	SChange = .false.
c
c  Handle the first two axes.
c
	if(.not.mosaic)then
	  call uvrdvrd(tno,'ra',crval1,0.d0)
	  call uvrdvrd(tno,'dec',crval2,0.d0)
	  call uvrdvrr(tno,'dra',dra,0.0)
	  call uvrdvrr(tno,'ddec',ddec,0.0)
	  crval1 = crval1 + dra/cos(crval2)
	  crval2 = crval2 + ddec
	  call uvrdvrd(tno,'pntra', obsra,crval1)
	  call uvrdvrd(tno,'pntdec',obsdec,crval2)
	endif
	call pbRead(tno,pbtype)
	call uvrdvrr(tno,'epoch',epoch,1950.0)
c
c  Miscellaneous.
c
	call uvrdvra(tno,'source',source,' ')
	call uvrdvra(tno,'telescop',telescop,' ')
	call uvrdvra(tno,'observer',observer,' ')
c
c  Is this an East-west array.
c
	ew = .false.
	if(telescop.ne.' ')call obspar(telescop,'ew',epsi,ew)
	if(ew) ew = epsi.gt.0
	if(ew)then
	  ctype1 = 'RA---NCP'
	  ctype2 = 'DEC--NCP'
	else
	  ctype1 = 'RA---SIN'
	  ctype2 = 'DEC--SIN'
	endif
c
c  Handle the spectral axis.
c
	call uvrdvrr(tno,'vsource',vsource,0.0)
	call uvrdvrr(tno,'veldop',vobs,0.0)
	vobs = vobs - vsource
	call uvrdvra(tno,'veltype',ctype3,' ')
	call uvrdvrd(tno,'time',obstime,0.d0)
	naver = 1
c
	call uvinfo(tno,'line',line)
	itype = nint(line(1))
	if(itype.eq.CHANNEL)then
	  ltype = 'channel'
	else if(itype.eq.WIDE)then
	  ltype = 'wide'
	else if(itype.eq.VELOCITY)then
	  ltype = 'velocity'
	else
	  call bug('f','Unrecognised line type')
	endif
	nchan = nint(line(2))
	lstart = line(3)
	lwidth = line(4)
	lstep  = line(5)
c
	call uvfit1(tno,'bandwidth',nchan,cdelt3,epsi)
	if(ltype.ne.'wide')then
	  call uvfit1(tno,'restfreq',nchan,restfreq,epsi)
	  RConst = epsi.le.0.001*restfreq
	else
	  restfreq = 0
	endif
c
	if(mfs)then
	  ctype3(1:4) = 'FREQ'
	else if(restfreq.eq.0)then
	  if(nchan.gt.1)then
	    call uvfit2(tno,'sfreq',nchan,cdelt3,crval3,epsi)
	    VLinear = epsi.le.0.1*abs(cdelt3)
	  else
	    call uvinfo(tno,'sfreq',crval3)
	  endif
	  ctype3(1:4) = 'FREQ'
	else
	  if(nchan.gt.1)then
	    call uvfit2(tno,'velocity',nchan,cdelt3,crval3,epsi)
	    VLinear = epsi.le.0.1*abs(cdelt3)
	  else
	    call uvinfo(tno,'velocity',crval3)
	    cdelt3 = 0.001 * CMKS * cdelt3 / restfreq
	  endif
	  ctype3(1:4) = 'VELO'
	endif
c
	end
c************************************************************************
	subroutine HdDone(tno)
c
	implicit none
	integer tno
c------------------------------------------------------------------------
	include 'hdtab.h'
	Count = 0
	end
c************************************************************************
	subroutine HdSet(cellx,celly,ra0,dec0,proj,freq0)
c
	real cellx,celly,freq0
	double precision ra0,dec0
	character proj*(*)
c
c  Set some things that have not been set yet, and create a coordinate
c  object.
c
c  Input:
c    cellx,celly cdelt1,cdelt2
c    proj	Projection geometry code (mosaicing only).
c    ra0,dec0	crval1,crval2 (mosaicing only).
c    freq0	Reference frequency (mfs only).
c------------------------------------------------------------------------
	include 'hdtab.h'
c
	if(mosaic)then
	  crval1 = ra0
	  crval2 = dec0
	  ctype1 = 'RA---'//proj
	  ctype2 = 'DEC--'//proj
	endif
	if(mfs)crval3 = freq0
c
	cdelt1 = cellx
	cdelt2 = celly
c
c  Check for things that have changed.
c
	if(.not.RConst)call bug('w',
     *	  'Rest frequecies varied between channels by > 0.1%')
	if(RChange)call bug('w',
     *	  'Rest frequencies varied by > 0.1% while reading data')
	if(.not.VLinear)call bug('w',
     *	  'Channel frequencies/velocities deviated by > 10% from'//
     *	  ' linearity')
	if(VChange)call bug('w',
     *	  'Channel velocities varied by > 10% while reading data')
	if(XChange)call bug('w',
     *	  'The source RA changed by > 1 arcsec while reading data')
	if(YChange)call bug('w',
     *	  'The source DEC changed by > 1 arcsec while reading data')
	end
c************************************************************************
	subroutine HdCoObj(coObj)
c
	implicit none
	integer coObj
c
c  Generate a coordinate object.
c------------------------------------------------------------------------
	include 'hdtab.h'
c
	call coCreate(coObj)
	call coAxSet(coObj,1,ctype1,0.d0,crval1,cdelt1)
	call coAxSet(coObj,2,ctype2,0.d0,crval2,cdelt2)
	call coAxSet(coObj,3,ctype3,1.d0,crval3,cdelt3)
	call coSetd(coObj,'vobs',dble(vobs/naver))
	if(restfreq.ne.0)call coSetd(coObj,'restfreq',restfreq)
	call coSetd(coObj,'obstime',obstime/naver)
	call coSetd(coObj,'epoch',dble(epoch))
	if(nchan.eq.1.or.mfs)call coSeta(coObj,'cellscal','CONSTANT')
	call coReinit(coObj)
c
	end
c************************************************************************
	subroutine HdDefSiz(nx,ny)
c
	implicit none
	integer nx,ny
c
c  Determine the default size of an image.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'hdtab.h'
	real fwhm,cutoff,maxrad
	integer coObj,pbObj
c
c  Determine the FWHM of the telescope.
c
	if(pbtype.eq.' ')then
	  call bug('f',
     *	    'Unknown telescope -- cannot determine default image size')
	else
	  call HdCoObj(coObj)
	  call pbInit(pbObj,pbtype,coObj)
	  call pbInfo(pbObj,fwhm,cutoff,maxrad)
	  call pbFin(pbObj)
	  call coFin(coObj)
	endif
c
	nx = max(nint(abs(fwhm/cdelt1)),1)
	ny = max(nint(abs(fwhm/cdelt2)),1)
	end
c************************************************************************
	subroutine HdWrite(tno,rms,nx,ny)
c
	implicit none
	integer tno
	real rms
	integer nx,ny
c
c  Write all the header rubbish out to an image dataset.
c
c------------------------------------------------------------------------
	include 'hdtab.h'
c
	call wrhda(tno,'ltype', ltype)
	call wrhdr(tno,'lstart',lstart)
	call wrhdr(tno,'lwidth',lwidth)
	call wrhdr(tno,'lstep', lstep)
c
	if(telescop.ne.' ')call wrhda(tno,'telescop',telescop)
	if(source.ne.' ')  call wrhda(tno,'object',  source)
	if(observer.ne.' ')call wrhda(tno,'observer',observer)
	if(rms.gt.0)call wrhdr(tno,'rms',rms)
	if(.not.mosaic)then
	  if(obsra.ne.crval1.or.obsdec.ne.crval2)then
	    call mosInit(nx,ny)
	    call mosSet(1,obsra,obsdec,rms,pbtype)
	    call mosSave(tno)
	  else if(pbtype.ne.telescop)then
	    call wrhda(tno,'pbtype',pbtype)
	  endif
	endif
c
	end
