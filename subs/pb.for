c************************************************************************
c
c  Some subroutines to get the primary beam function.
c
c  User callable routines are:
c
c  subroutine pbRead(tno,pbtype)
c  subroutine pbWrite(tno,pbtype)
c
c  subroutine pbInit(pbObj,pbtype,coObj)
c  subroutine pbInitc(pbObj,pbtype,coObj,in,x1)
c  subroutine pbInfo(pbObj,pbfwhm,cutoff,maxrad)
c  real function pbGet(pbObj,x,y)
c  real function pbDer(pbObj,x,y)
c  subroutine pbFin(pbObj)
c
c  Basically, a "primary beam object" is created with either pbInit or
c  pbInitc -- the latter is used if the pointing centre is somewhere
c  other that the reference pixel. It also takes a primary beam type.
c  Generally this will be just a telescope name (e.g. "atca" or "hatcreek"),
c  but is can also be "gaus(xxxx)" where xxxx gives the FWHM (in arcsec) of
c  a gaussian primary beam.
c
c  pbRead returns the primary beam type of a particular dataset.
c  pbWrite writes a primary beam type to a dataset.
c
c  pbGet and pbDer return the value of the primary beam or its
c  derivative, respectively. Inputs are grid coordinates in the coordinate
c  system used when initialising the primary beam object.
c
c  pbInfo returns information about the primary beam -- firstly its FWHM.
c  A primary beam is also assumed to cut-off at some value. The minimum
c  value and the radius at which it occurs are also give.
c
c  Finally pbFin tidies up whatever it needs to.
c
c************************************************************************
c* pbRead -- Determine the primary beam type of a dataset.
c& rjs
c: image-data
c+
	subroutine pbRead(tno,pbtype)
c
	implicit none
	integer tno
	character pbtype*(*)
c
c  Determine the primary beam type associated with a dataset.
c
c  Input:
c    tno	Handle of the input dataset
c  Output:
c    pbtype	Primary beam type. Generally this will be just a telescope
c		name (e.g. "atca" or "hatcreek"), but it could also be
c		"gaus(xxx)", where xxx gives the FWHM of a gaussian
c		primary beam in arcsec.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	character telescop*16
	real pbfwhm
c
c  Externals.
c
	logical hdprsnt
c
c  Get telescope and primary beam parameters.
c
	if(hdprsnt(tno,'visdata'))then
	  call uvrdvra(tno,'telescop',telescop,' ')
	  call uvrdvrr(tno,'pbfwhm',pbfwhm,-1.0)
	else
	  call rdhda(tno,'telescop',telescop,' ')
	  call rdhdr(tno,'pbfwhm',pbfwhm,-1.0)
	endif
c
c  If the primary beam parameter is zero, treat it as a single dish.
c
	if(pbfwhm.eq.0)then
	  pbtype = 'SINGLE'
	else if(pbfwhm.gt.0)then
	  call pbEncode(pbtype,'gaus',pi/180/3600 * pbfwhm)
	else
	  pbtype = telescop
	endif
c
	end
c************************************************************************
c* pbWrite -- Write the primary beam type out to a dataset.
c& rjs
c: image-data
c+
	subroutine pbWrite(tno,pbtype)
c
	implicit none
	integer tno
	character pbtype*(*)
c
c  Determine the primary beam type associated with a dataset.
c
c  Input:
c    tno	Handle of the input dataset
c    pbtype	Primary beam type. Generally this will be just a telescope
c		name (e.g. "atca" or "hatcreek"), but it could also be
c		"gaus(xxx)", where xxx gives the FWHM of a gaussian
c		primary beam in arcsec.
c--
c------------------------------------------------------------------------
	double precision dtemp
	real pbfwhm
	character telescop*16
	logical dotel,ok
	integer l
c
c  Externals.
c
	logical hdprsnt
	integer len1
c
c  Determine the primary beam type.
c
	telescop = pbtype
	call ucase(telescop)
	dotel = telescop(1:5).ne.'GAUS('
c
	if(telescop.eq.'SINGLE')then
	  pbfwhm = 0
	  dotel = .false.
	else if(.not.dotel)then
	  l = len1(telescop)
	  ok = l.gt.6.and.telescop(l:l).eq.')'
	  if(ok)call atodf(telescop(6:l-1),dtemp,ok)
	  if(.not.ok)
     *	    call bug('f','Error with gaussian pb parameter: '//telescop)
	  pbfwhm = dtemp
	endif
c
c  Handle a visibility dataset.
c
	if(hdprsnt(tno,'visdata'))then
	  if(dotel)then
	    call uvputvra(tno,'telescop',telescop)
	  else
	    call uvputvrr(tno,'pbfwhm',pbfwhm,1)
	  endif
	else
	  if(dotel)then
	    call wrhda(tno,'telescop',telescop)
	  else
	    call wrhdr(tno,'pbfwhm',pbfwhm)
	  endif
	endif
c
	end
c************************************************************************
	subroutine pbEncode(pbtype,type,val)
c
	implicit none
	character pbtype*(*),type*(*)
	real val
c------------------------------------------------------------------------
	integer i1,i2
	character string*10
c
c  Externals.
c
	integer len1
c
	if(type.eq.'gaus')then
	  write(string,'(1pe10.4)')val
	  i1 = 1
	  i2 = len1(string)
	  dowhile(i1.lt.i2.and.string(i1:i1).eq.' ')
	    i1 = i1 + 1
	  enddo
	  pbtype = 'GAUS('//string(i1:i2)//')'
	else if(type.eq.'single')then
	  pbtype = 'SINGLE'
	else
	  call bug('f','Unrecognised primary beam type, in pbEncode')
	endif
c
	end
c************************************************************************
c* pbInit -- Initialise a primary beam object.
c& rjs
c: image-data
c+
	subroutine pbInit(pbObj,pbtype,coObj)
c
	implicit none
	integer pbObj,coObj
	character pbtype*(*)
c
c  Initialise a primary beam object. The primary beam is assumed to
c  be centred at the reference pixel of the coordinate system.
c
c  Input:
c    pbtype	Primary beam type.
c    coObj	The coordinate system used to form the primary beam object.
c  Output:
c    pbObj	The primary beam object.
c--
c------------------------------------------------------------------------
	call pbInitc(pbObj,pbtype,coObj,'op',0.d0)
	end
c************************************************************************
c* pbInitc -- Initialise a primary beam object.
c& rjs
c+ image-data
c+
	subroutine pbInitc(pbObj,type,coObj,in,x1)
c
	implicit none
	character type*(*),in*(*)
	integer pbObj,coObj
	double precision x1(*)
c
c  Initialise a primary beam object. The primary beam is assumed to
c  be centred at the location given by the coordinate system (coObj),
c  the coordinate specification (in) and the coordinate (x1).
c  
c  Input:
c    pbtype	Primary beam type.
c    coObj	The coordinate system.
c    in		Form of the input coordinate defining the reference
c		location (passed to the co routines).
c    x1		The reference location.
c  Output:
c    pbObj	The primary beam object.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'pb.h'
c
	logical init,more,ok
	integer l1,l2,iax,k,kd
	double precision f,dtemp,x2(2),antdiam
	double precision crpix,crval,cdelt1,cdelt2
	real error,t
	character ctype*16,line*64
c
c  Externals.
c
	integer len1
	data init/.false./
c
c  Initialise the PB routines first up.
c
	if(.not.init)call pbFirst
	init = .true.
c
c  Get a PB object from the free list.
c
	pbObj = pbHead
	if(pbObj.eq.0)call bug('f','Exhausted all primary beam objects')
	pbHead = pnt(pbHead)
c
c  Determine the primary beam type.
c
	l2 = len1(type)
	l1 = index(type,'(') - 1
	if(l1.le.0)l1 = l2
c
c  Get information about this coordinate system.
c
	call coFindAx(coObj,'frequency',iax)
	if(iax.ne.0)then
	  call coFreq(coObj,'op',0.d0,f)
	else
	  f = 0
	endif
	call coCvt(coObj,in,x1,'ap/ap',x2)
	call coAxDesc(coObj,1,ctype,crpix,crval,cdelt1)
	call coAxDesc(coObj,2,ctype,crpix,crval,cdelt2)
c
c  Find a matching primary beam type. Also look for a near match.
c
	ctype = type
	call ucase(ctype)
	error = 0
	kd = 0
	k = 0
	more = .true.
	dowhile(more.and.k.lt.npb)
	  k = k + 1
	  more = pb(k).ne.ctype(1:l1).or.f.lt.f1(k).or.f.gt.f2(k)
	  if(more.and.pb(k).eq.ctype(1:l1))then
	    t = min(abs(f-f1(k)),abs(f-f2(k)))
	    if(kd.eq.0.or.t.lt.error)then
	      kd = k
	      error = t
	    endif
	  endif
	enddo
c
c  If we did not find anything, lets see if we know the diameter of
c  the antenna, and use a gaussian approximation for this.
c
	if(more.and.kd.eq.0)then
	  call obspar(ctype(1:l1),'antdiam',antdiam,ok)
	  if(ok)then
	    call pbAdd(ctype(1:l1),0.1,1e4,real(1100.d0/antdiam),
     *							0.05,GAUS,0,0.)
	    more = .false.
	    k = npb
	  endif
	endif
c
c  Check that we have all the information we need.
c
	if(more)then
	  if(f.le.0)
     *	    call bug('f','Frequency could not be determined, in pbInit')
	  line = 'Unrecognised primary beam type '//type
	  if(kd.eq.0)call bug('f',line)
	  k = kd
	  line = 'Using nearest frequency band'//
     *	    ' for primary beam type '//type
	  call bug('w',line)
	endif
c
c  Fill in the details.
c
	freq(pbObj) = f
	pnt(pbObj) = k
	if(ctype(1:l1).eq.'GAUS')then
	  ok = l2-l1-2 .gt. 0
	  if(ok)call atodf(ctype(l1+2:l2-1),dtemp,ok)
	  if(.not.ok)call bug('f','Bad parameter for gaussian beam')
	  fwhm(pbObj) = dtemp/60.d0
	else if(ctype(1:l1).eq.'SINGLE')then
	  fwhm(pbObj) = 0
	else
	  fwhm(pbObj) = pbfwhm(k) / f
	endif
c
c  Now set the scaling parameters.
c
	x0(pbObj) = x2(1)
	y0(pbObj) = x2(2)
	if(pbtype(k).eq.POLY)then
	  xc(pbObj) = (f*cdelt1*180*60/pi)**2
	  yc(pbObj) = (f*cdelt2*180*60/pi)**2
	else if(pbtype(k).eq.GAUS)then
	  xc(pbObj) = 4*log(2.) * (cdelt1*180*60/pi/fwhm(pbObj))**2
	  yc(pbObj) = 4*log(2.) * (cdelt2*180*60/pi/fwhm(pbObj))**2
	else if(pbtype(k).eq.SINGLE)then
	  xc(pbObj) = 0
	  yc(pbObj) = 0
	endif
c
	end
c************************************************************************
c* pbFin -- Delete a primary beam object and tidy up.
c& rjs
c: image-data
c+
	subroutine pbFin(pbObj)
c
	implicit none
	integer pbObj
c
c  This deletes a primary beam object and tidies up.
c
c  Input:
c    pbObj	Handle of the primary beam object.
c--
c------------------------------------------------------------------------
	include 'pb.h'
	pnt(pbObj) = pbHead
	pbHead = pbObj
	end
c************************************************************************
c* pbGet -- Get the value of a primary beam.
c& rjs
c: image-data
c+
	real function pbget(pbObj,x,y)
c
	implicit none
	integer pbObj
	real x,y
c
c  Determine the value of the primary beam at a given location.
c
c  Input:
c    pbObj	Handle of the primary beam object.
c    x,y	Grid location of the position of interest.
c  Output:
c    pbGet	Value of the primary beam.
c--
c------------------------------------------------------------------------
	include 'pb.h'
	real r2,P
	integer k,off,i
c
	r2 = xc(pbObj)*(x-x0(pbObj))**2 + yc(pbObj)*(y-y0(pbObj))**2
c
	k = pnt(pbObj)
c
	if(r2.gt.maxrad(k))then
	  pbGet = 0
	else if(pbtype(k).eq.POLY.and.nvals(k).eq.5)then
	  off = indx(k)
	  pbGet = 1/(pbvals(off) + r2*( pbvals(off+1) +
     *				    r2*( pbvals(off+2) +
     *				    r2*( pbvals(off+3) +
     *				    r2*( pbvals(off+4) ) ) ) ) )
	else if(pbtype(k).eq.POLY)then
	  off = indx(k)
	  P = pbvals(off+nvals(k)-1)
	  do i=off+nvals(k)-2,off,-1
	    P = P*r2 + pbvals(i)
	  enddo
	  pbGet = 1/P
	else if(pbtype(k).eq.GAUS)then
	  pbGet = exp(-r2)
	else if(pbtype(k).eq.SINGLE)then
	  pbGet = 1
	endif
c
	end
c************************************************************************
c* pbDer -- Get the value of the derivative of a primary beam.
c& rjs
c: image-data
c+
	real function pbder(pbObj,x,y)
c
	implicit none
	integer pbObj
	real x,y
c
c  Determine the derivative (w.r.t. frequency) of the primary beam
c  at a particular location.
c
c  Input:
c    pbObj	The primary beam object.
c    x,y	Grid location of the position of interest.
c  Output:
c    pbDer	The primary beam derivative wrt frequency.
c--
c------------------------------------------------------------------------
	include 'pb.h'
	real r2,P,Pdash
	integer k,off,n,i
c
c  Externals.
c
	if(freq(pbObj).le.0)
     *	  call bug('f','Observing frequency is not known, in pbder')
c
	r2 = xc(pbObj)*(x-x0(pbObj))**2 + yc(pbObj)*(y-y0(pbObj))**2
c
	k = pnt(pbObj)
c
	if(r2.gt.maxrad(k))then
	  pbDer = 0
	else if(pbtype(k).eq.POLY)then
	  off = indx(k)
	  P = 0
	  Pdash = 0
	  n = 2*nvals(k)-2
	  do i=off+nvals(k)-1,off,-1
	    P     = P    *r2 +   pbvals(i)
	    Pdash = Pdash*r2 + n*pbvals(i)
	    n = n - 2
	  enddo
	  pbDer = -Pdash/(freq(pbObj)*P*P)
	else if(pbtype(k).eq.GAUS)then
	  pbDer = -2*r2*exp(-r2)/freq(pbObj)
	else if(pbtype(k).eq.SINGLE)then
	  pbDer = 0
	endif
c
	end
c************************************************************************
c* pbInfo -- Return information about a primary beam.
c& rjs
c: image-data
c+
	subroutine pbinfo(pbObj,pbfwhmd,cutoffd,maxradd)
c
	implicit none
	integer pbObj
	real pbfwhmd,cutoffd,maxradd
c
c  This returns information about a primary beam.
c
c  Input:
c    pbObj	Handle of the primary beam object.
c  Output:
c    pbfwhmd	Primary beam FWHM, in radians.
c    cutoffd	Minimum non-zero value of the primary beam.
c    maxradd	Maximum radious (in radians) where the primary beam
c		model is non-zero.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'pb.h'
	integer k
c
	k = pnt(pbObj)
	pbfwhmd = pi/180/60 * fwhm(pbObj)
	cutoffd = cutoff(k)
	if(pbtype(k).eq.GAUS)then
	  maxradd = pbfwhmd * sqrt( -log(cutoffd)/(4*log(2.)) )
	else if(pbtype(k).eq.POLY)then
	  maxradd = pi/180/60 * sqrt(maxrad(k))/freq(pbObj)
	else
	  maxradd = 0
	endif
c
	end
c************************************************************************
	subroutine pbFirst
c
	implicit none
c
c  Store all the primary beams that I know about.
c------------------------------------------------------------------------
	include 'pb.h'
	integer i
c
c Set coefficients for each telescope and frequency range
c
	integer NCOEFF
	parameter(NCOEFF=5)
	real atcal(NCOEFF),atcas(NCOEFF),atcac(NCOEFF),atcax(NCOEFF)
	real vla(NCOEFF)
c
	data atcal /1.0, 8.99e-4, 2.15e-6, -2.23e-9,  1.56e-12/
	data atcas /1.0, 1.02e-3, 9.48e-7, -3.68e-10, 4.88e-13/
	data atcac /1.0, 1.08e-3, 1.31e-6, -1.17e-9,  1.07e-12/
	data atcax /1.0, 1.04e-3, 8.36e-7, -4.68e-10, 5.50e-13/
c
	data vla /0.9920378, 0.9956885e-3, 0.3814573e-5, -0.5311695e-8,
     *		  0.3980963e-11/
c
c  Initialise the common block. In particular, form a linked list
c  of free PB objects.
c  
	npb = 0
	npbvals = 0
c
	pbHead = 1
	do i=1,MAXOBJ-1
	  pnt(i) = i + 1
	enddo
	pnt(MAXOBJ) = 0
c
c  Make the list of known primary beam objects.
c
	call pbAdd('ATCA',    1.15,1.88,    47.9, 0.03, POLY,
     *							 NCOEFF,atcal)
	call pbAdd('ATCA',    2.10,2.60,    49.7, 0.03, POLY,
     *							 NCOEFF,atcas)
	call pbAdd('ATCA',    4.30,6.70,    48.3, 0.03, POLY,
     *							 NCOEFF,atcac)
	call pbAdd('ATCA',    7.90,9.30,    50.6, 0.03, POLY,
     *							 NCOEFF,atcax)
	call pbAdd('VLA',     0.071,24.510, 44.3, 0.023,POLY,
     *							 NCOEFF,vla)
	call pbAdd('HATCREEK',74.0,116.0,   184., 0.05, GAUS,0,0.)
	call pbAdd('FST',     1.00,2.00,    67.0, 0.05, GAUS,0,0.)
	call pbAdd('GAUS',    0.0,1e4,	      1.0, 0.05, GAUS,0,0.)
	call pbAdd('SINGLE',  0.0,1e4,	      0.0, 0.5,  SINGLE,0,0.)
c
	end
c************************************************************************
	subroutine pbAdd(tel,f1d,f2d,pbfwhmd,cutoffd,pbtyped,nval,vals)
c
	implicit none
	character tel*(*)
	integer nval,pbtyped
	real f1d,f2d,pbfwhmd,cutoffd,vals(nval)
c
c  Add a primary beam to our list of known primary beams.
c
c  Input:
c    tel	Primary beam name.
c    f1,f2	Frequency range where valid (in GHz).
c    pbfwhm	Approx primary beam FWHM, in arcsec, at 1 GHz.
c    cutoff	Level below which primary beam is invalid.
c    pbtype	Functional form used to represent the primary beam.
c    nval	Number of values used to parameterize the functional form.
c    vals	The parameterisation of the functional form.
c------------------------------------------------------------------------
	include 'pb.h'
	integer i
c
	npb = npb + 1
	if(npb.gt.MAXPB)call bug('f','Too many primary beams')
	pb(npb) = tel
	f1(npb) = f1d
	f2(npb) = f2d
	pbfwhm(npb) = pbfwhmd
	cutoff(npb) = cutoffd
	pbtype(npb) = pbtyped
	nvals(npb) = nval
	if(nval.gt.0)then
	  if(npbvals+nval.gt.MAXVAL)
     *	    call bug('f','Too many primary beam parameters')
	  indx(npb) = npbvals + 1
	  do i=1,nval
	    pbvals(i+npbvals) = vals(i)
	  enddo
	  npbvals = npbvals + nval
	else
	  indx(npb) = 0
	endif
c
c  Determine the maximum radius**2 that the function goes out to.
c
	if(pbtyped.eq.GAUS)then
	  maxrad(npb) = -log(cutoff(npb))
	else if(pbtyped.eq.SINGLE)then
	  maxrad(npb) = 1
	else if(pbtyped.eq.POLY)then
	  call pbradp(cutoffd,vals,nval,pbfwhmd,maxrad(npb))
	endif
	end
c************************************************************************
	subroutine pbradp(cutoff,coeff,ncoeff,pbfwhm,maxrad)
c
	implicit none
	integer ncoeff
	real cutoff,coeff(ncoeff),pbfwhm,maxrad
c
c  Determine the maximum radius at which the primary beam is still
c  non-zero.
c
c  Input:
c    cutoff
c    ncoeff
c    coeff
c    pbfwhm
c  Output:
c    maxrad	Maximum radius**2, in (arcmin * GHz) ** 2
c------------------------------------------------------------------------
	integer MAXORDER
	parameter(MAXORDER=8)
	real a(MAXORDER)
	complex roots(MAXORDER-1)
	real fac
	integer i,ifail,k
	logical found
c
c  Put the poly coefficients in the form wanted by the solver.
c
	fac = 1
	if(ncoeff.gt.MAXORDER)call bug('f','Too high a poly for me')
	do i=1,ncoeff
	  a(ncoeff-i+1) = fac * coeff(i)
	  fac = fac * pbfwhm * pbfwhm
	enddo
	a(ncoeff) = a(ncoeff) - 1/cutoff
c
c  Now find the roots of the poly.
c
	call rpolyzr(a,ncoeff-1,roots,ifail)
	if(ifail.ne.0)call bug('f','Failed to find the poly roots')
c
c  Look for the smallest positive root with no imaginary part.
c
	found = .false.
	do i=1,ncoeff-1
	  if(aimag(roots(i)).eq.0.and.real(roots(i)).gt.0)then
	    if(.not.found)then
	      k = i
	      found = .true.
	    else
	      if(real(roots(i)).lt.real(roots(k))) k = i
	    endif
	  endif
	enddo
c
	if(.not.found)call bug('f','Primary beam does not die away')
	maxrad = pbfwhm * pbfwhm * real(roots(k))
c
	end
