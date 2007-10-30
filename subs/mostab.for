c************************************************************************
c
c  A set of routines to handle multi-pointing vis data sets, mosaic
c  tables, and to mosaic images.
c
c   subroutine MosCini
c   subroutine MosChk
c   subroutine MosCDone
c
c   subrouitne MosGeom
c   subroutine MosGet
c   subroutine MosSize
c   subroutine MosGFin
c
c   subroutine MosSave
c   subroutine MosLoad
c   subroutine MosPrint
c
c   subroutine MosMIni
c   subroutine Mosaicer
c   subroutine MosMFin
c
c   subroutine MosPnt
c
c  History:
c    rjs  26oct94 Original version
c    rjs  16nov94 Added mosWt
c    rjs  26nov94 Fix bug which allocated too many coordinate objects.
c    rjs   3dec94 Changes to the way a single pointing is mosaiced.
c    rjs  31jan95 Added mosIni, mosAdd. Changes to mosGeom and its helpers.
c    rjs   2feb95 Change in mosHash to help avoid integer overflows. Avoid
c    		  initialisation problem.
c    rjs  13mar95 Add tolerance to checking for primary beam size.
c    rjs  15oct96 Modify call sequence to coGeom.
c    rjs  26mar97 Better support for "pbtype" parameter in vis datasets.
c************************************************************************
	subroutine MosCIni
c
	implicit none
c------------------------------------------------------------------------
	include 'mostab.h'
	integer i
c
c  Externals.
c
	integer prime
c
	npnt = 0
	nxy = 0
	doinit = .true.
	HashSize = prime(MAXHASH)
c
	do i=1,HashSize
	  Hash(i) = 0
	enddo
c
	end
c************************************************************************
	subroutine MosCDone(lIn)
c
	implicit none
	integer lIn
c------------------------------------------------------------------------
	include 'mostab.h'
	doinit = .true.
	end
c************************************************************************
	subroutine MosChk(lIn,i)
c
	implicit none
	integer lIn,i
c------------------------------------------------------------------------
	include 'mostab.h'
	include 'mirconst.h'
	character tel1*16
	double precision radec1(2),dra1,ddec1
c
c  Externals.
c
	integer MosLoc
	logical MosSolar,uvVarUpd
c
c  Set the reference position, and determine if we are to operate in
c  a mode where appreciable proper motion is anticipated.
c
	if(npnt.eq.0)then
	  solar = MosSolar(lIn)
	  call uvrdvrd(lIn,'ra',radec0(1),0.d0)
	  call uvrdvrd(lIn,'dec',radec0(2),0.d0)
	  call coRaDec(coRef,'SIN',radec0(1),radec0(2))
	endif
c
c  Initialise the handle to track changes in the primary beam.
c
	if(doinit)then
	  call uvVarIni(lIn,vPntUpd)
	  if(.not.solar)call uvVarSet(vPntUpd,'ra')
	  if(.not.solar)call uvVarSet(vPntUpd,'dec')
	  call uvVarSet(vPntUpd,'telescop')
	  call uvVarSet(vPntUpd,'pbfwhm')
	  call uvVarSet(vPntUpd,'pbtype')
	  call uvVarSet(vPntUpd,'dra')
	  call uvVarSet(vPntUpd,'ddec')
	endif
c
c  Process a change in primary beam model.
c
	if(doinit.or.uvVarUpd(vPntUpd))then
c
c  Get the RA,DEC and primary beam type for this record.
c
	  call uvrdvrd(lIn,'dra',dra1,0.d0)
	  call uvrdvrd(lIn,'ddec',ddec1,0.d0)
	  if(solar)then
	    radec1(1) = radec0(1)
	    radec1(2) = radec0(2)
	  else
	    call uvrdvrd(lIn,'ra',radec1(1),0.d0)
	    call uvrdvrd(lIn,'dec',radec1(2),0.d0)
	  endif
	  if(abs(dra1)+abs(ddec1).gt.0)then
	    radec1(1) = radec1(1) + dra1 / cos(radec1(2))
	    radec1(2) = radec1(2) + ddec1
	  endif
c
	  call pbRead(lIn,tel1)
c
	  pntno = MosLoc(tel1,radec1)
	endif
	i = pntno
	doinit = .false.
c
	end
c************************************************************************
	integer function MosLoc(tel1,radec1)
c
	implicit none
	character tel1*(*)
	double precision radec1(2)
c
c  Locate a particular pointing in my table.
c------------------------------------------------------------------------
	include 'mostab.h'
	include 'mirconst.h'
c
c  Tolerances are 1 arcsec in pointing and in primary beam size.
c
	double precision tol
	parameter(tol=dpi/180.d0/3600.d0)
	logical found
	integer indx,pnt
	double precision lm(2),ll1,mm1
c
c  Externals.
c
	integer MosHash
c
c  Locate this pointing in our list of previously encountered pointings.
c
	found = .false.
	call coCvt(coRef,'aw/aw',radec1,'ap/ap',lm)
	ll1 = lm(1)
	mm1 = lm(2)
	if(npnt.gt.0)then
	  if(abs(ll1-llmm(1,pntno)).lt.tol.and.
     *	     abs(mm1-llmm(2,pntno)).lt.tol.and.
     *	     tel1.eq.telescop(pntno))then
	    found = .true.
	    pnt = pntno
	  else
	    indx = MosHash(ll1,mm1,HashSize)
	    dowhile(.not.found.and.Hash(indx).ne.0)
	      pnt = Hash(indx)
	      if(abs(ll1-llmm(1,pnt)).lt.tol.and.
     *		 abs(mm1-llmm(2,pnt)).lt.tol.and.
     *		 tel1.eq.telescop(pnt))then
		found = .true.
	      else
		indx = indx + 1
		if(indx.gt.HashSize)indx = 1
	      endif
	    enddo
	  endif
	endif
c
c  This pointing was not found. Add it to our list.
c
	if(.not.found)then
	  npnt = npnt + 1
	  pnt = npnt
	  if(npnt.gt.MAXPNT)call bug('f','Pointing table overflow')
	  telescop(npnt) = tel1
	  llmm(1,npnt) = ll1
	  llmm(2,npnt) = mm1
	  radec(1,npnt) = radec1(1)
	  radec(2,npnt) = radec1(2)
	  indx = MosHash(ll1,mm1,HashSize)
	  dowhile(Hash(indx).ne.0)
	    indx = indx + 1
	    if(indx.gt.HashSize) indx = 1
	  enddo
	  Hash(indx) = npnt	  
	endif
c
	MosLoc = pnt
c
	end
c************************************************************************
	integer function MosHash(ll,mm,HashSize)
c
	implicit none
	double precision ll,mm
	integer HashSize
c
c  Return a number in the range [1,HashSize] which is uniquely associated
c  with this pointing position.
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision tol
	parameter(tol=5d0*dpi/180d0/3600d0)
	integer i,j,n,indx
c
	i = nint(ll/tol)
	j = nint(mm/tol)
	n = mod(2*max(abs(i),abs(j)) - 1,HashSize)
c
	if(n.lt.0)then
	  indx = 0
	else
	  indx = n*n + n + i + j + 2
	  if(i.gt.j)indx = indx + 2*n + 2
	  indx = mod(indx,HashSize)
	  if(indx.lt.0)indx = indx + HashSize
	endif
	MosHash = indx + 1
	end
c************************************************************************
	subroutine mosChar(ra1,dec1,npnt1,proj)
c
	implicit none
	double precision ra1,dec1
	integer npnt1
	character proj*(*)
c
c  Get the reference location, number of pointings and the projection
c  geometry.
c
c  Output:
c    ra1,dec1	Reference pointing.
c    npnt1	Number of pointings.
c    proj	Projection geometry.
c------------------------------------------------------------------------
	include 'mostab.h'
	include 'mirconst.h'
	integer i,i0
	double precision l0,m0,dtemp
	character tel1*16
	logical ew
c
c  Determine the average (l,m).
c
	if(.not.solar)then
	  l0 = 0
	  m0 = 0
	  do i=1,npnt
	    l0 = l0 + llmm(1,i)
	    m0 = m0 + llmm(2,i)
	  enddo
	  l0 = l0 / npnt
	  m0 = m0 / npnt
c
c  Determine the pointing that is closest to this one.
c
	  i0 = 1
	  do i=2,npnt
	    if(abs(llmm(1,i )-l0)+abs(llmm(2,i )-m0).lt.
     *	       abs(llmm(1,i0)-l0)+abs(llmm(2,i0)-m0))i0 = i
	  enddo
c
c  Convert from direction cosine to RA,DEC.
c
	  call coCvt(coRef,'ap/ap',llmm(1,i0),'aw/aw',radec0)
	endif
c
c  Determine whether all the telescopes are of an E-W type.
c
	ew = .true.
	i = 0
	tel1 = '??'
	dowhile(ew.and.i.lt.npnt)
	  i = i + 1
	  if(telescop(i).ne.tel1.or.i.eq.1)then
	    tel1 = telescop(i)
	    ew = tel1.ne.' '
	    if(ew)call obspar(tel1,'ew',dtemp,ew)
	    if(ew)ew = dtemp.gt.0
	  endif
	enddo
c
c  Return with all the goodies.
c
	ra1 = radec0(1)
	dec1 = radec0(2)
	npnt1 = npnt
	if(ew)then
	  proj = 'NCP'
	else
	  proj = 'SIN'
	endif
c
	call coFin(coRef)
c
	end
c************************************************************************
	logical function MosSolar(lIn)
c
	implicit none
	integer lIn
c
c  Determine whether this is a solar system object, and so if the "solar
c  system" intepretation ra/dec changes should be used.
c------------------------------------------------------------------------
	integer i
	real plmaj,plmin
	character source*32
c
c  A table of solar system objects. NOTE: The entries must be in
c  alphabetic order and lower case.   
c
        integer NSOLAR
        parameter(NSOLAR=11)
        character solar(NSOLAR)*8
c
c  Externals.
c
        integer binsrcha
c
        data solar/'earth   ','jupiter ','mars    ','mercury ',
     *  'moon    ','neptune ','pluto   ','saturn  ','sun     ',
     *  'uranus  ','venus   '/
c
c  Look for the source name in the list of solar system objects.
c
	call uvrdvra(lIn,'source',source,' ')
	call lcase(source)
        i = binsrcha(source,solar,NSOLAR)
        if(i.ne.0)then
          MosSolar = .true.
c  
c  If it was not found in the list of known solar system objects,
c  see if it has plmaj and plmin variables. If so, its probably
c  a solar system object.   
c
        else
          call uvrdvrr(lIn,'plmaj',plmaj,0.)
          call uvrdvrr(lIn,'plmin',plmin,0.)
          MosSolar = abs(plmaj)+abs(plmin).gt.0
	endif
c
	end
c************************************************************************
	subroutine MosGinit(coObj,nx,ny,nchan,mnx,mny)
c
	implicit none
	integer coObj,nx,ny,nchan,mnx,mny
c
c  Do geometry, shift and size calculations for a mosaiced imaging
c  sequence.
c
c  Input:
c    nx,ny	Size of the image.
c    nchan	Number of channels.
c    coObj	Coordinate object. On output, the reference pixel is set.
c  Output:
c    mnx,mny	Mosaiced image size.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'mostab.h'
c
	integer i
	double precision crpix1,crpix2,wcoeff(3)
	character ctype*16
c
c  Get the cell increment -- for use later on.
c
	call coAxDesc(coObj,1,ctype,crpix1,radec0(1),cdelt1)
	call coAxDesc(coObj,2,ctype,crpix2,radec0(2),cdelt2)
c
c  Do goemetry correction calculations.
c
	do i=1,npnt
	  call coGeom(coObj,'aw/aw',radec(1,i),
     *			ucoeff(1,i),vcoeff(1,i),wcoeff)
	enddo
c
c  Allocate the arrays to determine shifts.
c
	nxy = nchan*npnt
	call MemAlloc(pX,nxy,'r')
	call MemAlloc(pY,nxy,'r')
c
c  Do the shift calculations.
c
	call MosShift(coObj,npnt,nchan,memr(pX),memr(pY))
c
c  Get the size of the output image.
c
	nx2 = (nx-1)/2
	ny2 = (ny-1)/2
	call MosSizer(nx2,ny2,memr(pX),memr(pY),npnt,nchan,mnx,mny,
     *						crpix1,crpix2)
c
c  Corrrect the coordinate object for this change in reference
c  pixel.
c
	call coSetd(coObj,'crpix1',crpix1)
	call coSetd(coObj,'crpix2',crpix2)
c
c  Initialise the RMS arrays.
c
	do i=1,npnt
	  Rms2(i) = 0
	  SumWt(i) = 0
	enddo
c
	end
c************************************************************************
	subroutine MosShift(coObj,npnt1,nchan,x,y)
c
	implicit none
	integer coObj,npnt1,nchan
	real x(npnt1,nchan),y(npnt1,nchan)
c
c  Determine the fractional pixel shifts and the resulting
c  alignment between the different pointings.
c
c  Input:
c    coObj	Coordinate object handle.
c    npnt1	Number of pointings.
c    nchan	Number of frequency channels.
c  Output:
c    x,y	Pixel location, relative to the reference pixel,
c		of the resulting pointing.
c--
c------------------------------------------------------------------------
	include 'mostab.h'
	integer i,j
	double precision x1(3),x2(3)
c
	if(npnt.ne.npnt1)
     *	  call bug('f','Inconsistent number of pointings')
c
	do j=1,nchan
	  do i=1,npnt
	    x1(1) = radec(1,i)
	    x1(2) = radec(2,i)
	    x1(3) = j
	    call coCvt(coObj,'aw/aw/ap',x1,'op/op/ap',x2)
	    x(i,j) = x2(1)
	    y(i,j) = x2(2)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine MosSizer(nx2,ny2,x,y,npnt,nchan,mnx,mny,
     *						crpix1,crpix2)
c
	implicit none
	integer nx2,ny2,npnt,nchan,mnx,mny
	real x(npnt,nchan),y(npnt,nchan)
	double precision crpix1,crpix2
c
c  Determine the size of the output image.
c
c  Input:
c    nx2,ny2	Half sizes of the base images.
c    nchan	Number of channels.
c    npnt	Number of pointins.
c  Input/Output:
c    x,y	These give the pixel coordinates of the centre of each
c		pointing. On input, this is relative to a reference pixel
c		of 0. On output, it it relative to a non-zero reference
c		pixel.
c  Output:
c    crpix1,crpix2 Reference pixels.
c    mnx,mny	Size of mosaiced image.
c------------------------------------------------------------------------
	integer i,j,imin,imax,jmin,jmax
	real xmin,xmax,ymin,ymax
c
	xmin = x(1,1)
	xmax = xmin
	ymin = y(1,1)
	ymax = ymin
c
	do j=1,nchan
	  do i=1,npnt
	    xmin = min(xmin,x(i,j))
	    xmax = max(xmax,x(i,j))
	    ymin = min(ymin,y(i,j))
	    ymax = max(ymax,y(i,j))
	  enddo
	enddo
c
	imin = nint(xmin - nx2 + 0.5)
	imax = nint(xmax + nx2 - 0.5)
	jmin = nint(ymin - ny2 + 0.5)
	jmax = nint(ymax + ny2 - 0.5)
	mnx = imax - imin + 1
	mny = jmax - jmin + 1
	crpix1 = 1 - imin
	crpix2 = 1 - jmin
c
	do j=1,nchan
	  do i=1,npnt
	    x(i,j) = x(i,j) + crpix1
	    y(i,j) = y(i,j) + crpix2
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine MosGeom(size,n,nchan,npol,Vis,Wts)
c
	implicit none
	integer size,n,nchan,npol
	complex Vis(size,n)
	real Wts(n)
c
c  Perform geometry corrections, shifts and calculation of the rms noise
c  for a set of visibilities.
c
c  Input:
c    size	Record size
c    n		Number of records.
c    nchan	Number of channels.
c    npol	Number of polarisations.
c    Wts	Weights to be used -- used to determine rms noise.
c  Input/Output:
c    Vis	Visibility data.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'mostab.h'
c
c  Call the routine which does the real work.
c
	call MosGeom1(size,n,nchan,npol,npnt,Vis,Wts,ucoeff,vcoeff,
     *	  memr(pX),memr(pY),cdelt1,cdelt2,Rms2,SumWt)
c
	end
c************************************************************************
	subroutine MosGeom1(size,n,nchan,npol,npnt,Vis,Wts,
     *			ucoeff,vcoeff,x,y,cdelt1,cdelt2,Rms2,SumWt)
c
	implicit none
	integer size,n,nchan,npol,npnt
	complex Vis(size,n)
	double precision ucoeff(3,npnt),vcoeff(3,npnt)
	real x(nchan,npnt),y(nchan,npnt),RMS2(npnt),SumWt(npnt),Wts(n)
	double precision cdelt1,cdelt2
c
c  Apply all geometry and shift corrections for a mosaiced observation.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer InUV,InWPnt,InRmsFq,InData
	parameter(InUV=1,InWPnt=2,InRmsFq=3,InData=5)
c
	complex fac(MAXCHAN)
	real uu,vv,ww,ud,vd,theta,sigma2
	integer i,j,i0,k,pnt
c
c  Consistency check.
c
	if(nchan.gt.MAXCHAN)
     *		call bug('f','Too many channels for me, in MosGeom1')
	if(InData-1+nchan*npol.ne.size)
     *		call bug('f','Inconsistent, in MosGeom1')
c
c  Loop the loop.
c
	do k=1,n
	  uu = real (Vis(InUV,k))
	  vv = aimag(Vis(InUV,k))
	  ww = real (Vis(InWPnt,k))
	  pnt = nint(aimag(Vis(InWPnt,k)))
	  sigma2 = real(Vis(InRmsFq,k))
c
c  Accumulate rms noise information.
c
	  Rms2(pnt) = Rms2(pnt) + Wts(k)*Wts(k)*sigma2
	  SumWt(pnt) = SumWt(pnt) + Wts(k)
c
c  Do geometry corrections.
c
	  ud = uu*ucoeff(1,pnt) + vv*ucoeff(2,pnt) + ww*ucoeff(3,pnt)
	  vd = uu*vcoeff(1,pnt) + vv*vcoeff(2,pnt) + ww*vcoeff(3,pnt)
	  Vis(InUV,k) = cmplx(ud,vd)
	  ud = ud * cdelt1
	  vd = vd * cdelt2
c
c  Do fractional pixel shift.
c
	  do i=1,nchan
	    theta = -2*pi * (ud* (anint(x(i,pnt)) - x(i,pnt)) +
     *			     vd* (anint(y(i,pnt)) - y(i,pnt)) )
	    fac(i) = cmplx(cos(theta),sin(theta))
	  enddo
c
	  i0 = InData
	  do j=1,npol
	    do i=1,nchan
	      Vis(i0,k) = Vis(i0,k) * fac(i)
	      i0 = i0 + 1
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine MosLoad(tno,npnt1)
c
	implicit none
	integer tno,npnt1
c
c  Read in a mosaic table from a dataset.
c
c  Input:
c    tno	Handle of the input dataset.
c  Output:
c    npnt1	The number of pointings.
c------------------------------------------------------------------------
	include 'mostab.h'
	integer i,item,iostat,offset,ival(2),size,n,iax
	double precision crpix
	real rval(2)
c
c  Externals.
c
	integer hsize
	logical hdprsnt
	character itoaf*2
c
c  Open the pointing table.
c
	if(hdprsnt(tno,'mostable'))then
	  call haccess(tno,item,'mostable','read',iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error opening input mosaic table')
	    call bugno('f',iostat)
	  endif
c
c  Write the main body of the pointing table.
c
	  offset = 8
	  size = hsize(item)
	  if(mod(size-offset,48).ne.0)
     *	    call bug('f','Bad size for mosaic table')
	  npnt = (size - offset)/48
	  if(npnt.gt.MAXPNT)
     *		call bug('f','Too many pointings, in mosLoad')
	  do i=1,npnt
	    if(iostat.eq.0)call hreadi(item,ival,offset,8,iostat)
	    nx2 = (ival(1)-1)/2
	    ny2 = (ival(2)-1)/2
	    offset = offset + 8
	    if(iostat.eq.0)call hreadd(item,radec(1,i),offset,16,iostat)
	    offset = offset + 16
	    if(iostat.eq.0)
     *		call hreadb(item,telescop(i),offset,16,iostat)
	    offset = offset + 16
	    if(iostat.eq.0)call hreadr(item,rval,offset,8,iostat)
	    Rms2(i) = rval(1)
	    offset = offset + 8
	  enddo
c
c  Finish up. Check for errors and then close the dataset.
c
	  if(iostat.ne.0)then
	    call bug('w','Error reading from mosaic table')
	    call bugno('f',iostat)
	  endif
	  call hdaccess(item,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error closing mosaic table')
	    call bugno('f',iostat)
	  endif
c
c  Handle the case of no mosaicing table -- treat it as if its
c  just a single pointing.
c
	else
	  npnt = 1
	  call coInit(tno)
	  call coFindAx(tno,'longitude',iax)
	  if(iax.eq.0)call bug('f','Failed to find RA axis')
	  call coCvt1(tno,iax,'op',0.d0,'aw',radec(1,1))
	  call coCvt1(tno,iax,'op',0.d0,'ap',crpix)
	  call rdhdi(tno,'naxis'//itoaf(iax),n,0)
	  nx2 = max(crpix-1,n-crpix) + 1
c
	  call coFindAx(tno,'latitude',iax)
	  if(iax.eq.0)call bug('f','Failed to find DEC axis')
	  call coCvt1(tno,iax,'op',0.d0,'aw',radec(2,1))
	  call coCvt1(tno,iax,'op',0.d0,'ap',crpix)
	  call rdhdi(tno,'naxis'//itoaf(iax),n,0)
	  ny2 = max(crpix-1,n-crpix) + 1
c
	  call rdhdr(tno,'rms',rms2(1),0.)
	  if(rms2(1).le.0)rms2(1) = 1
	  call pbRead(tno,telescop(1))
c
	  call coFin(tno)
	endif
c
	npnt1 = npnt
c
	end
c************************************************************************
	subroutine MosPrint
c
	implicit none
c
c  Write -- to standard output -- the contents of the mosaic table.
c
c------------------------------------------------------------------------
	include 'mostab.h'
c
	integer i
	character line*80,ras*12,decs*14
c
c  Externals.
c
	character itoaf*8,rangle*32,hangle*32
c
	call output('    Number of pointing centers: '//itoaf(npnt))
	call output(' ')
	call output('     Sub-Image    Pointing Center       Primary '//
     *		'Beam      Field')
	call output('       Size       RA           DEC      Type    '//
     *		'           RMS')
	call output('      ------- ------------------------  --------'//
     *		'----    --------')
	do i=1,npnt
	  ras = hangle(radec(1,i))
	  decs = rangle(radec(2,i))
	  write(line,10)i,2*nx2+1,2*ny2+1,ras,decs,telescop(i),rms2(i)
  10	  format(i4,i5,i4,1x,3a,1pe8.2)
	  call output(line)
	enddo
	end
c************************************************************************
	subroutine mosInit(nx,ny)
c
	implicit none
	integer nx,ny
c
c  Initialise a mosaic table.
c------------------------------------------------------------------------
	include 'mostab.h'
c
	npnt = 0
	nx2 = (nx-1)/2 
	ny2 = (ny-1)/2
	end
c************************************************************************
	subroutine mosGet(i,ra1,dec1,rms1,pbtype1)
c
	implicit none
	integer i
	double precision ra1,dec1
	real rms1
	character pbtype1*(*)
c
c  Get information from the table.
c------------------------------------------------------------------------
	include 'mostab.h'
	include 'mirconst.h'
c
	if(i.lt.1.or.i.gt.npnt)
     *	  call bug('f','Invalid pointing number, in mosGet')
c
	ra1 = radec(1,i)
	dec1 = radec(2,i)
	rms1 = rms2(i)
	pbtype1 = telescop(i)
	end
c************************************************************************
	subroutine mosSet(i,ra1,dec1,rms1,pbtype1)
c
	implicit none
	integer i
	double precision ra1,dec1
	real rms1
	character pbtype1*(*)
c
c  Add an extry to the mosaic table.
c
c  Input:
c    ra1,dec1	Pointing centre ra,dec.
c    rms1	Nominal field rms noise.
c    pbtype1	Primary beam type.
c------------------------------------------------------------------------
	include 'mostab.h'
	npnt = max(npnt,i)
	if(npnt.gt.MAXPNT)call bug('f','Mosaic table overflow')
	radec(1,i) = ra1
	radec(2,i) = dec1
	rms2(i) = rms1
	telescop(i) = pbtype1
	end
c************************************************************************
	subroutine MosSave(tno)
c
	implicit none
	integer tno
c
c  Write out a mosaicing table to this dataset.
c
c  Input:
c    tno	Handle of the output dataset.
c------------------------------------------------------------------------
	include 'mostab.h'
	integer i,item,iostat,offset,ival(2)
	real rval(2)
c
c  Open the pointing table.
c
	call haccess(tno,item,'mostable','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output mosaic table')
	  call bugno('f',iostat)
	endif
c
c  Write the header.
c
	ival(1) = 0
	ival(2) = 0
	offset = 0
	call hwritei(item,ival,offset,8,iostat)
	offset = offset + 8
c
c  Write the main body of the pointing table.
c
	do i=1,npnt
	  ival(1) = 2*nx2 + 1
	  ival(2) = 2*ny2 + 1
	  if(iostat.eq.0)call hwritei(item,ival,offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)call hwrited(item,radec(1,i),offset,16,iostat)
	  offset = offset + 16
	  if(iostat.eq.0)call hwriteb(item,telescop(i),offset,16,iostat)
	  offset = offset + 16
	  rval(1) = Rms2(i)
	  rval(2) = 0
	  if(iostat.eq.0)call hwriter(item,rval,offset,8,iostat)
	  offset = offset + 8
	enddo
c
c  Finish up. Check for errors and then close the dataset.
c
	if(iostat.ne.0)then
	  call bug('w','Error writing to mosaic table')
	  call bugno('f',iostat)
	endif
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing mosaic table')
	  call bugno('f',iostat)
	endif
c
	end
c************************************************************************
	subroutine MosGFin
c
	implicit none
c
c  Tidy up.
c
c------------------------------------------------------------------------
	include 'mostab.h'
	integer i,ngood
	real Sig
c
	if(nxy.gt.0)then
	  call MemFree(pX,nxy,'r')
	  call MemFree(pY,nxy,'r')
	  nxy = 0
	endif
c
	ngood = 0
	Sig = 0
	do i=1,npnt
	  if(SumWt(i).gt.0)then
	    Rms2(i) = Rms2(i) / (SumWt(i)*SumWt(i))
	  else
	    Rms2(i) = 0
	  endif
	  if(Rms2(i).gt.0)then
	    ngood = ngood + 1
	    Sig = Sig + Rms2(i)
	    Rms2(i) = sqrt(Rms2(i))
	  endif
	enddo
c
c  If some of the RMS values were zero, fill them in with an average
c  value. If they are all bad, fill them all in with 1!
c
	if(ngood.eq.0)then
	  Sig = 1
	else
	  Sig = sqrt(Sig/ngood)
	endif
c
	if(ngood.lt.npnt)then
	  do i=1,npnt
	    if(Rms2(i).le.0)Rms2(i) = Sig
	  enddo
	endif
c
	end
c************************************************************************
	subroutine MosMIni(coObj,chan)
c
	implicit none
	integer coObj
	real chan
c
c  Initialise ready for a mosaic operation.
c------------------------------------------------------------------------
	include 'mostab.h'
	double precision x1(3),x2(2)
	integer i
c
	x1(3) = chan
	do i=1,npnt
	  x1(1) = radec(1,i)
	  x1(2) = radec(2,i)
	  call coCvt(coObj,'aw/aw/ap',x1,'ap/ap',x2)
	  x0(i) = x2(1)
	  y0(i) = x2(2)
	  call pbInitc(pbObj(i),telescop(i),coObj,'ap/ap',x2)
	enddo
c
	end
c************************************************************************
	subroutine Mosaicer(In,Out,nx,ny,npnt1,mnx,mny,
     *					Runs,MAXRUNS,nRuns)
c
	implicit none
	integer nx,ny,npnt1,mnx,mny,MAXRUNS,nRuns,Runs(3,MAXRUNS)
	real In(nx,ny,npnt1),Out(mnx,mny)
c
c  Mosaic the different fields together.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'mostab.h'
c
	integer pWts
c
	if(npnt1.ne.npnt)
     *	  call bug('f','Inconsistency in Mosaic')
c
	if(npnt.eq.1)then
	  call Mosaic1(In,Out,nx,ny,mnx,mny,Runs,MAXRUNS,nRuns)
	else
	  call MemAlloc(pWts,mnx*mny,'r')
	  if(nx2.gt.(nx-1)/2.or.ny2.gt.(ny-1)/2)
     *	    call bug('f','Inconsistency in Mosaicer')
	  call Mosaic2(In,Out,memr(pWts),nx,ny,npnt,mnx,mny,Rms2)
	  call mosRuns(memr(pWts),mnx,mny,Runs,MAXRUNS,nRuns)
	  call memFree(pWts,mnx*mny,'r')
	endif
	end
c************************************************************************
	subroutine Mosaic1(In,Out,nx,ny,mnx,mny,Runs,maxruns,nruns)
c
	implicit none
	integer nx,ny,mnx,mny,MAXRUNS,nRuns,Runs(3,MAXRUNS)
	real In(nx,ny),Out(mnx,mny)
c
c  DO the linear mosaic on all the fields in the simple case that
c  there is only one field. This consists of simply copying the input
c  to the output.
c------------------------------------------------------------------------
	integer pbObj,imin,imax,jmin,jmax,ic,jc,ioff,joff,i,j
	real x,y,xext,yext
c
c  Externals.
c
	integer mosPb
c
	pbObj = mosPb(1)
	call mosExt(1,imin,imax,jmin,jmax)
	call pbExtent(pbObj,x,y,xext,yext)
	ic = nx/2 + 1
	jc = ny/2 + 1
	ioff = ic - nint(x)
	joff = jc - nint(y)
	imin = max(imin,1)
	imax = min(imax,mnx)
	jmin = max(jmin,1)
	jmax = min(jmax,mny)
c
	if(MAXRUNS.lt.jmax-jmin+2)
     *	  call bug('f','Runs buffer overflow, in Mosaicer')
c
	nRuns = 0
c
	do j=1,jmin-1
	  do i=1,mnx
	    Out(i,j) = 0
	  enddo
	enddo
c
	do j=jmin,jmax
	  do i=1,imin-1
	    Out(i,j) = 0
	  enddo
	  do i=imin,imax
	    Out(i,j) = In(i+ioff,j+joff)
	  enddo
	  do i=imax+1,mnx
	    Out(i,j) = 0
	  enddo
c
	  nRuns = nRuns + 1
	  Runs(1,nRuns) = j
	  Runs(2,nRuns) = imin
	  Runs(3,nRuns) = imax
	enddo
c
	do j=jmax+1,mny
	  do i=1,mnx
	    Out(i,j) = 0
	  enddo
	enddo
c
	Runs(1,nRuns+1) = 0
	end		
c************************************************************************
	subroutine Mosaic2(In,Out,Wts,nx,ny,npnt,mnx,mny,Rms2)
c
	implicit none
	integer nx,ny,npnt,mnx,mny
	real In(nx,ny,npnt),Out(mnx,mny),Wts(mnx,mny),Rms2(npnt)
c
c  Do a linear mosaic of all the fields. Weight is so that the
c  RMS never exceeds the max RMS in the input data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,ic,jc,ioff,joff,imin,jmin,imax,jmax,k,pbObj
	real Pb(MAXDIM),Wt3,x,y,xext,yext
c
c  Externals.
c
	real pbGet,mosWt3
	integer mosPb
c
c  Check that we have enough space.
c
	if(mnx.gt.MAXDIM)call bug('f','Buffer overflow, in Mosaicer')
c
c  Initialise the output and weights arrays.
c
	do j=1,mny
	  do i=1,mnx
	    Out(i,j) = 0
	    Wts(i,j) = 0
	  enddo
	enddo
c
	ic = nx/2 + 1
	jc = ny/2 + 1
c
	do k=1,npnt
	  Wt3 = mosWt3(k)
	  pbObj = mosPb(k)
	  call mosExt(k,imin,imax,jmin,jmax)
	  call pbExtent(pbObj,x,y,xext,yext)
	  ioff = ic - nint(x)
	  joff = jc - nint(y)
	  imin = max(imin,1)
	  imax = min(imax,mnx)
	  jmin = max(jmin,1)
	  jmax = min(jmax,mny)
c
	  do j=jmin,jmax
	    do i=imin,imax
	      Pb(i) = pbGet(pbObj,real(i),real(j))
	    enddo
	    do i=imin,imax
	      Out(i,j) = Out(i,j) + Wt3*Pb(i)*In(i+ioff,j+joff,k)
	      Wts(i,j) = Wts(i,j) + Wt3*Pb(i)*Pb(i)
	    enddo
	  enddo
	enddo
c
	call MosWt(Rms2,npnt,Out,Wts,mnx*mny)
c
	end
c************************************************************************
	subroutine mosRuns(Wts,nx,ny,Runs,MAXRUNS,nRuns)
c
	implicit none
	integer nx,ny,MAXRUNS,nRuns,Runs(3,MAXRUNS)
	real Wts(nx,ny)
c
c  Make a Runs array to indicate which pixels are good.
c------------------------------------------------------------------------
	integer i,j,ngood
c
	nRuns = 0
c
	do j=1,ny
	  ngood = 0
	  do i=1,nx
	    if(Wts(i,j).gt.0)then
	      ngood = ngood + 1
	    else if(ngood.gt.0)then
	      nruns = nruns + 1
	      if(nruns.ge.MAXRUNS)call bug('f','Runs buffer overflow')
	      Runs(1,nRuns) = j
	      Runs(2,nRuns) = i - ngood
	      Runs(3,nRuns) = i - 1
	      ngood = 0
	    endif
	  enddo
	  if(ngood.gt.0)then
	    nruns = nruns + 1
	    if(nruns.ge.MAXRUNS)call bug('f','Runs buffer overflow')
	    Runs(1,nRuns) = j
	    Runs(2,nRuns) = nx - ngood + 1
	    Runs(3,nRuns) = nx
	  endif
	enddo
c
	Runs(1,nRuns+1) = 0
c
	end
c************************************************************************
	subroutine MosWt(Rms2,npnt,Out,Wts,n)
c
	implicit none
	integer npnt,n
	real Rms2(npnt),Out(n),Wts(n)
c
c  Reweight the data according to some scheme.
c------------------------------------------------------------------------
	real Sigt,scale
	integer i,k
c
c  Determine the maximum RMS.
c
	Sigt = Rms2(1)
	do k=2,npnt
	  Sigt = max(Sigt,Rms2(k))
	enddo
	Sigt = Sigt*Sigt
c
c  Now rescale to correct for the weights.
c
	do i=1,n
	  scale = Sigt*Wts(i)
	  if(scale.le.0)then
	    scale = 0
	  else if(scale.lt.1)then
	    scale = sqrt(scale)/Wts(i)
	  else
	    scale = 1/Wts(i)
	  endif
	  Out(i) = scale*Out(i)
	enddo
c
	end
c************************************************************************
	subroutine MosMFin
c
	implicit none
c
c  Tidy up after mosaicing.
c
c------------------------------------------------------------------------
	include 'mostab.h'
	integer i
c
	do i=1,npnt
	  call pbFin(pbObj(i))
	enddo
c
	end
c************************************************************************
	subroutine MosPnt(coObj,in,x,beams,psf,nx,ny,npnt1)
c
	implicit none
	integer nx,ny,npnt1,coObj
	character in*(*)
	double precision x(*)
	real beams(nx,ny,npnt1),psf(nx,ny)
c
c  Determine the true point-spread function of a mosaiced image.
c
c  Input:
c    coObj	Coordinate object.
c    in,x	These are the normal arguments to coCvr, giving the
c		location (in RA,DEC,freq) of interest.
c    beams	The beam patterns for each pointing.
c    nx,ny	The size of the individual beam patterns.
c    npnt1	The number of pointings.
c  Output:
c    psf	The point-spread function.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer pWts
	double precision xref(3)
c
c  Determine the location of the reference position, in grid units.
c
	call coCvt(coObj,in,x,'ap/ap/ap',xref)
	call MosMini(coObj,real(xref(3)))
c
	call MemAlloc(pWts,nx*ny,'r')
	call MosPnt1(beams,psf,memr(pWts),nx,ny,npnt1,
     *				  nint(xref(1)),nint(xref(2)))
	call MemFree(pWts,nx*ny,'r')
c
	call MosMFin
	end
c************************************************************************
	subroutine MosPnt1(beams,psf,wts,nx,ny,npnt1,xr,yr)
c
	implicit none
	integer nx,ny,npnt1,xr,yr
	real beams(nx,ny,npnt1),psf(nx,ny),wts(nx,ny)
c
c  Determine the true point-spread function of a mosaiced image.
c
c  Input:
c    beams	The beam patterns for each pointing.
c    nx,ny	The size of the individual beam patterns.
c    npnt1	The number of pointings.
c    xr,yr	Pixel location where we want to work out the PSF.
c  Scratch:
c    Wts	The weight array.
c  Output:
c    psf	The point-spread function.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mostab.h'
	integer i,j,k,imin,imax,jmin,jmax,xoff,yoff
	real Pb(MAXDIM),Wt3,scale
c
c  Externals.
c
	real pbGet
	real mosWt3
c
c  Check!
c
	if(npnt.ne.npnt1)
     *	  call bug('f','Inconsistent number of pointings')
c
c  Initialise the output and weights array.
c
	do j=1,ny
	  do i=1,nx
	    PSF(i,j) = 0
	    Wts(i,j) = 0
	  enddo
	enddo
c
	xoff = xr - nx/2 - 1
	yoff = yr - ny/2 - 1
c
c  Loop over all the pointings.
c
	do k=1,npnt
	  call mosExt(k,imin,imax,jmin,jmax)
	  imin = max(imin-xoff,1)
	  imax = min(imax-xoff,nx)
	  jmin = max(jmin-yoff,1)
	  jmax = min(jmax-yoff,ny)
	  if(imin.le.imax.and.jmin.le.jmax)then
	    Wt3 = mosWt3(k)
	    scale = Wt3 * pbGet(pbObj(k),real(xr),real(yr))
	    if(scale.gt.0)then
	      do j=jmin,jmax
	        do i=imin,imax
	          Pb(i) = pbGet(pbObj(k),real(i+xoff),real(j+yoff))
	        enddo
	        do i=imin,imax
	          PSF(i,j) = PSF(i,j) + scale*Pb(i)*Beams(i,j,k)
	          Wts(i,j) = Wts(i,j) + Wt3*Pb(i)*Pb(i)
	        enddo
	      enddo
	    endif
	  endif
	enddo
c
c  Reweight the data.
c
	call MosWt(Rms2,npnt,PSF,Wts,nx*ny)
c
	end
c************************************************************************
	subroutine mosRaDec(k,ra,dec)
c
	implicit none
	integer k
	double precision ra,dec
c
c  Return the RA and DEC of the k'th pointing.
c
c  Input:
c    k		Pointing number
c  Output:
c    ra,dec	RA and DEC (radians) of the pointing.
c------------------------------------------------------------------------
	include 'mostab.h'
c
	if(k.lt.1.or.k.gt.npnt)call bug('f',
     *		'Invalid pointing number if mosRaDec')
c
	ra = radec(1,k)
	dec = radec(2,k)
	end
c************************************************************************
	subroutine mosInfo(nx2d,ny2d,npnt1)
c
	implicit none
	integer nx2d,ny2d,npnt1
c
c  Return information about the mosaicing setup.
c------------------------------------------------------------------------
	include 'mostab.h'
c
	nx2d = nx2
	ny2d = ny2
	npnt1 = npnt
	end
c************************************************************************
	real function mosWt3(k)
c
	implicit none
	integer k
c
c  Return the field-dependent weight to be used in mosaicing.
c
c------------------------------------------------------------------------
	include 'mostab.h'
c
	MosWt3 = 1/(Rms2(k)*Rms2(k))
	end
c************************************************************************
	integer function mosPb(k)
c
	implicit none
	integer k
c
c  Return the primary beam corresponding to a particular pointing.
c------------------------------------------------------------------------
	include 'mostab.h'
	mosPb = pbObj(k)
	end
c************************************************************************
	subroutine mosExt(k,imin,imax,jmin,jmax)
c
	implicit none
	integer k,imin,imax,jmin,jmax
c
c  Determine the region that this pointing will add to.
c  NOTE: We form "ceil" and "floor" functions from nint(x+0.5) and nint(x-0.5)
c  respectively.
c  Also note that we add then subtract an integer offset to make the arg
c  of the nint function positive, because nint(-0.5) + 1 is not equal to
c  nint(-0.5+1), and we need to be consistent (independent of any offset
c  added to the pixel coordinates).
c------------------------------------------------------------------------
	include 'mostab.h'
	integer offset
c
	offset = abs(x0(k)) + nx2 + 1
	imin = nint(x0(k)-nx2+0.5 + offset) - offset
	imax = nint(x0(k)+nx2-0.5 + offset) - offset
	offset = abs(y0(k)) + ny2 + 1
	jmin = nint(y0(k)-ny2+0.5 + offset) - offset
	jmax = nint(y0(k)+ny2-0.5 + offset) - offset
c
	end
c************************************************************************
c************************************************************************
	subroutine mosWts(Wt1,Wt2,nx,ny,xoff,yoff)
c
	implicit none
	integer nx,ny,xoff,yoff
	real Wt1(nx,ny),Wt2(nx,ny)
c
c  Determine the weights to apply to data during the mosaicing process.
c  The region-of-interest is a rectangular one.
c  Input:
c    xoff,yoff	Add these to the pbObj coordinates to get the local
c		pixel coordinates.
c------------------------------------------------------------------------
	integer nx2,ny2,npnt,imin,imax,jmin,jmax,i,j,k,pbObj
	real Wt3,Pb
c
c  Externals.
c
	real mosWt3,pbGet
	integer mosPb
c
c  Initialise the weight array.
c
	do j=1,ny
	  do i=1,nx
	    Wt1(i,j) = 0
	  enddo
	enddo
c
c  Deternime some things.
c
	call mosInfo(nx2,ny2,npnt)
c
	do k=1,npnt
	  pbObj = mosPb(k)
	  Wt3 = mosWt3(k)
	  call mosExt(k,imin,imax,jmin,jmax)
	  imin = max(imin+xoff,1)
	  imax = min(imax+xoff,nx)
	  jmin = max(jmin+yoff,1)
	  jmax = min(jmax+yoff,ny)
c
	  do j=jmin,jmax
	    do i=imin,imax
	      Pb = pbGet(pbObj,real(i-xoff),real(j-yoff))
	      Wt1(i,j) = Wt1(i,j) + Wt3 * Pb * Pb
	    enddo
	  enddo
	enddo
c
c  Now normalise.
c
	call mosWtC(Wt1,Wt2,nx*ny)
	end
c************************************************************************
	subroutine mosWtsR(Runs,nRuns,Wt1,Wt2,npix)
c
	implicit none
	integer nRuns,Runs(3,nRuns),npix
	real Wt1(npix),Wt2(npix)
c
c  Determine weights to apply to data during the mosaicing process.
c  The region-of-interest is specified by the much unloved Runs format.
c
c------------------------------------------------------------------------
	integer k,i,j,n,jmin,jmax,imin,imax,ilo,ihi,iRuns,nin
	integer nx2,ny2,npnt,pbObj
	real Wt3,Pb
c
c  Externals.
c
	real mosWt3,pbGet
	integer mosPb
c
c  Initialise the weight array.
c
	do i=1,npix
	  Wt1(i) = 0
	enddo
c
c  Deternime some things.
c
	call mosInfo(nx2,ny2,npnt)
c
	do k=1,npnt
	  pbObj = mosPb(k)
	  Wt3 = mosWt3(k)
	  call mosExt(k,imin,imax,jmin,jmax)
c
	  n = 0
	  do iRuns=1,nRuns
	    if(Runs(1,iRuns).ge.jmin.and.Runs(1,iRuns).le.jmax.and.
     *	       Runs(3,iRuns).ge.imin.and.Runs(2,iRuns).le.imax)then
	      j = Runs(1,iRuns)
	      ilo = max(Runs(2,iRuns),imin)
	      ihi = min(Runs(3,iRuns),imax)
	      nin = n + ilo - Runs(2,iRuns)
	      do i=ilo,ihi
		nin = nin + 1
		Pb = pbGet(pbObj,real(i),real(j))
	        Wt1(nin) = Wt1(nin) + Wt3 * Pb * Pb
	      enddo
	    endif
	    n = n + Runs(3,iRuns) - Runs(2,iRuns) + 1
	  enddo
	enddo
c
c  Now normlise.
c
	call mosWtC(Wt1,Wt2,npix)
	end
c************************************************************************
	subroutine mosWtC(Wt1,Wt2,npix)
c
	implicit none
	integer npix
	real Wt1(npix),Wt2(npix)
c------------------------------------------------------------------------
	include 'mostab.h'
	integer i,k
	real scale,Sigt
c
c  Determine the maximum RMS.
c
	Sigt = Rms2(1)
	do k=2,npnt
	  Sigt = max(Sigt,Rms2(k))
	enddo
	Sigt = Sigt*Sigt
c
c  Now compute the weights for the data.
c
	do i=1,npix
	  scale = Sigt*Wt1(i)
	  if(scale.le.0)then
	    Wt1(i) = 0
	    Wt2(i) = 0
	  else if(scale.lt.1)then
	    scale = sqrt(scale)
	    Wt2(i) = scale/Wt1(i)
	    Wt1(i) = 1/scale
	  else
	    Wt2(i) = 1/Wt1(i)
	    Wt1(i) = 1
	  endif
	enddo
c
	end
