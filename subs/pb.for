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
c  subroutine pbExtent(pbObj,x0,y0,xrad,yrad)
c  subroutine pbList
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
c  Similarly pbExtent returns information about a primary beam, such as
c  its centre (x0,y0), and maximum extent in x and y (all in grid units).
c
c  pbList lists the available primary beam models.
c
c  Finally pbFin tidies up whatever it needs to.
c
c  History:
c
c   10nov92   nebk   Original version.
c   24nov92   nebk   More header comments to appease pjt.
c   07jan93   nebk   Rewrite calling interface again
c   15feb93   nebk   pbfwhm=0 -> single dish, <0 -> unknown
c   25oct93   rjs    Prevent floating underflow in exp function.
c   25oct94   rjs    Complete rewrite.
c   15mar95   rjs    Better model for Hat Ck and WSRT.
c   27jul95   rjs    Initialise ifail before calling rpolyzr
c   06nov95   rjs    Larger ATCA primary beam size.
c   29nov95   rjs    Use "pbtype" to describe primary beam type in
c		     datasets. Added pblist.
c   26mar97   rjs    Less precision in pbencode.
c   07jul97   rjs    Change call to coaxdesc to coaxget.
c   05sep97   mchw   Change lower freq for HATCREEK to 24 GHz.
c   09may00   rjs    Add extra check.
c   10may02   rjs    Add model for OVRO.
c   13oct03   rjs    Added Ravi's 12mm model.
c   23jun03   pjt    add LOFAR
c   30jun04   gmx    Updated WSRT beam
c   01jan05   rjs    Merge ATNF and BIMA versions.
c   21may05   rjs    Improved beamshape at 12mm.
c   23jun05   rjs    Add model for 3mm primary beam.
c************************************************************************
c* pbList -- List known primary beam types.
c& rjs
c: image-data
c+
	subroutine pbList
c
	implicit none
c
c  List the primary beam types that are modelled.
c
c--
c------------------------------------------------------------------------
	include 'pb.h'
c
	integer i
	character line*80
c
	call pbFirst
c
c         12345678901234561234567890123456 xxx.xx xxx.xx 0.xxxx
	call output(
     *	 'Name            Description                     '//
     *   '      Freq Range    Cutoff')
	call output(
     *   '                                                '//
     *   '         (GHz)')
	call output(
     *	 '----            -----------                     '//
     *	 '    --------------  ------')
	do i=1,npb
	  write(line,'(2a,2f9.3,f8.4)')
     *	    pb(i),descrip(i),f1(i),f2(i),cutoff(i)
	  call output(line)
	enddo
	end
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
	pbfwhm = -1
	if(hdprsnt(tno,'visdata'))then
	  call uvrdvra(tno,'pbtype',telescop,' ')
	  if(telescop.eq.' ')then
	    call uvrdvra(tno,'telescop',telescop,' ')
	    call uvrdvrr(tno,'pbfwhm',pbfwhm,-1.0)
	  endif
	else
	  call rdhda(tno,'pbtype',telescop,' ')
	  if(telescop.eq.' ')then
	    call rdhda(tno,'telescop',telescop,' ')
	    call rdhdr(tno,'pbfwhm',pbfwhm,-1.0)
	  endif
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
	logical hdprsnt
c
c  Handle a visibility dataset.
c
	if(hdprsnt(tno,'visdata'))then
	  call uvputvra(tno,'pbtype',pbtype)
	else
	  call wrhda(tno,'pbtype',pbtype)
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
	include 'mirconst.h'
	integer i1,i2
	character string*10
c
c  Externals.
c
	integer len1
c
	if(type.eq.'gaus')then
	  write(string,'(1pe10.3)')3600*180/pi * val
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
	logical more,ok
	integer l1,l2,iax,k,kd
	double precision f,dtemp,x2(2),antdiam
	double precision crpix,crval,cdelt1,cdelt2
	real error,t,alpha
	character ctype*16,line*64
c
c  Externals.
c
	integer len1
c
c  Initialise the PB routines first up.
c
	call pbFirst
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
	call coAxGet(coObj,1,ctype,crpix,crval,cdelt1)
	call coAxGet(coObj,2,ctype,crpix,crval,cdelt2)
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
     *				0.05,GAUS,0,0.,'Truncated Gaussian')
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
	if(pbtype(k).eq.IPOLY.or.pbtype(k).eq.POLY)then
	  xc(pbObj) = (f*cdelt1*180*60/pi)**2
	  yc(pbObj) = (f*cdelt2*180*60/pi)**2
	else if(pbtype(k).eq.BLOCKED)then
	  alpha = 2*0.514497 * pi
	  xc(pbObj) = (alpha*cdelt1*180*60/pi/fwhm(pbObj))**2
	  yc(pbObj) = (alpha*cdelt2*180*60/pi/fwhm(pbObj))**2
	else if(pbtype(k).eq.COS6)then
	  alpha = 2*acos(2**(-0.1666667))
	  xc(pbObj) = (alpha*cdelt1*180*60/pi/fwhm(pbObj)) ** 2
	  yc(pbObj) = (alpha*cdelt2*180*60/pi/fwhm(pbObj)) ** 2
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
	double precision r2,P
	real ax,r,b
	integer k,off,i
c
c  Externals.
c
	real j1xbyx
c
	r2 = xc(pbObj)*(x-x0(pbObj))**2 + yc(pbObj)*(y-y0(pbObj))**2
c
	k = pnt(pbObj)
	off = indx(k)
c
	if(r2.gt.maxrad(k))then
	  pbGet = 0
	else if(pbtype(k).eq.IPOLY.and.nvals(k).eq.5)then
	  pbGet = 1/(pbvals(off) + r2*( pbvals(off+1) +
     *				    r2*( pbvals(off+2) +
     *				    r2*( pbvals(off+3) +
     *				    r2*( pbvals(off+4) ) ) ) ) )
	else if(pbtype(k).eq.IPOLY.or.pbtype(k).eq.POLY)then
	  off = indx(k)
	  P = pbvals(off+nvals(k)-1)
	  do i=off+nvals(k)-2,off,-1
	    P = P*r2 + pbvals(i)
	  enddo
	  if(pbtype(k).eq.IPOLY)then
	    pbGet = 1/P
	  else
	    pbGet = P
	  endif
	else if(pbtype(k).eq.BLOCKED)then
	  ax = sqrt(r2)
	  r = pbvals(off)
	  b = pbvals(off+1)
	  P = 2*j1xbyx(ax)
	  if(r.gt.0)P = (P - 2*r*j1xbyx(b*ax))/(1-r)
	  pbGet = P*P
	else if(pbtype(k).eq.COS6)then
	  pbGet = cos(sqrt(r2))**6
	else if(pbtype(k).eq.GAUS)then
	  pbGet = exp(-r2)
	else if(pbtype(k).eq.SINGLE)then
	  pbGet = 1
	endif
	if(pbGet.le.cutoff(k))pbGet = 0
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
	else if(pbtype(k).eq.IPOLY.or.pbtype(k).eq.POLY)then
	  off = indx(k)
	  P = 0
	  Pdash = 0
	  n = 2*nvals(k)-2
	  do i=off+nvals(k)-1,off,-1
	    P     = P    *r2 +   pbvals(i)
	    Pdash = Pdash*r2 + n*pbvals(i)
	    n = n - 2
	  enddo
	  Pdash = Pdash / freq(pbObj)
	  if(pbtype(k).eq.IPOLY)then
	    pbDer = -Pdash/(P*P)
	  else
	    pbDer = Pdash
	  endif
	else if(pbtype(k).eq.COS6)then
	  P = sqrt(r2)
	  pbDer = -6*P/freq(pbObj)*cos(P)**5*sin(P)
	else if(pbtype(k).eq.GAUS)then
	  pbDer = -2*r2*exp(-r2)/freq(pbObj)
	else if(pbtype(k).eq.BLOCKED)then
	  call bug('f','Derivative of J1(x)/x p.b. not supported')
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
	integer k,off
	real alpha
c
	k = pnt(pbObj)
	off = indx(k)
	pbfwhmd = pi/180/60 * fwhm(pbObj)
	cutoffd = cutoff(k)
	if(pbtype(k).eq.GAUS)then
	  maxradd = pbfwhmd * sqrt( -log(cutoffd)/(4*log(2.)) )
	else if(pbtype(k).eq.BLOCKED)then
	  maxradd = pbvals(off+2)*pbfwhmd
	else if(pbtype(k).eq.COS6)then
	  alpha = 2*acos(2.0**(-0.1666667))
	  maxradd = pbfwhmd*acos(cutoffd**(0.1666667))/alpha
	else if(pbtype(k).eq.POLY.or.pbtype(k).eq.IPOLY)then
	  maxradd = pi/180/60 * sqrt(maxrad(k))/freq(pbObj)
	else
	  maxradd = 0
	endif
c
	end
c************************************************************************
c* pbExtent -- Return the extent of a primary beam.
c& rjs
c: image-data
c+
	subroutine pbExtent(pbObj,x,y,xext,yext)
c
	implicit none
	integer pbObj
	real x,y,xext,yext
c
c  This returns information about a primary beam.
c
c  Input:
c    pbObj	Handle of the primary beam object.
c  Output:
c    x,y	The primary beam centre, in grid units.
c    xext,yext	The distance to which the primary beam
c		can be assumed to be non-zero.
c    The primary beam will be non-zero only in the region
c    [x0-xext,x0+xext] x [y0-yext,y0+yext].
c--
c------------------------------------------------------------------------
	include 'pb.h'
	integer k
c
	k = pnt(pbObj)
c
	x = x0(pbObj)
	y = y0(pbObj)
c
	if(xc(pbObj).le.0.or.yc(pbObj).le.0)call bug('f',
     *	  'Extent of primary beam could not be determined')
	xext = sqrt(maxrad(k)/xc(pbObj))
	yext = sqrt(maxrad(k)/yc(pbObj))
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
	logical init
	save init
c
c Set coefficients for each telescope and frequency range
c
	integer NCOEFF,NATCAL1,NATCAL2,NATCAL3,NATCAK,NATCAW
	parameter(NCOEFF=5,NATCAL3=3,NATCAL1=5,NATCAL2=7)
	parameter(NATCAK=4,NATCAW=4)
	real atcas(NCOEFF),atcac(NCOEFF),atcax(NCOEFF)
	real atcak2(NATCAK),atcak(NCOEFF),atcaw(NATCAW)
	real atcal3(NATCAL3),atcal1(NATCAL1),atcal2(NATCAL2)
	real vla(NCOEFF)
c
	data init/.false./
	data atcal1 /1.0, 8.99e-4, 2.15e-6, -2.23e-9,  1.56e-12/
	data atcal2 /1.0,-1.0781341990755E-03,
     *			 4.6179146405726E-07,
     *		      	-1.0108079576125E-10,
     *		      	 1.2073518438662E-14,
     *			-7.5132629268134E-19,
     *			 1.9083641820123E-23/
	data atcal3/0.023, 0.631, 4.0/
	data atcas /1.0, 1.02e-3, 9.48e-7, -3.68e-10, 4.88e-13/
	data atcac /1.0, 1.08e-3, 1.31e-6, -1.17e-9,  1.07e-12/
	data atcax /1.0, 1.04e-3,  8.36e-7,  -4.68e-10, 5.50e-13/
	data atcak /1.0, 9.832e-4, 1.081e-6, -4.676e-10, 6.650e-13/
	data atcaw /1.0, 1.271e-3,-3.040e-7,  1.410e-9/
c
c Model by Ravi at 22.235 GHz - 4th order poly.
c
	data atcak2/1.0, -9.5793797E-04, 3.2279621E-07, 
     *			 -3.8065801E-11/
c
	data vla /0.9920378, 0.9956885e-3, 0.3814573e-5, -0.5311695e-8,
     *		  0.3980963e-11/
c
c  Return if we are already initialised.
c
	if(init)return
	init = .true.
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
c  Make the list of known primary beam objects. The ATCA primary beams
c  are taken from ATNF technical memo by Wieringa and Kesteven.
c
	call pbAdd('ATCA',    1.15,1.88,    47.9, 0.03,  IPOLY,
     *			NATCAL1,atcal1,'Recipocal 4th order poly')
	call pbAdd('ATCA.2',  1.15,1.88,    47.9, 0.002, POLY,
     *			NATCAL2,atcal2,'Sixth order poly')
	call pbAdd('ATCA.3',  1.15,1.88,    58.713*2*0.514497/1.22, 
     *			1e-3,BLOCKED, NATCAL3,atcal3,
     *			'Blocked aperture J1(x)/x form')
	call pbAdd('ATCA',    2.10,2.60,    49.7, 0.03,  IPOLY,
     *			NCOEFF,atcas,'Reciprocal 4th order poly')
	call pbAdd('ATCA',    4.00,6.90,    48.3, 0.03,  IPOLY,
     *			NCOEFF,atcac,'Reciprocal 4th order poly')
	call pbAdd('ATCA',    7.90,9.90,    50.6, 0.03,  IPOLY,
     *			NCOEFF,atcax,'Reciprocal 4th order poly')
	call pbAdd('ATCA.2',  15.5,25.5,    50.6, 0.10,  POLY,
     *			NATCAK,atcak2,'Fourth order poly')
	call pbAdd('ATCA',  15.5,25.5,    50.6, 0.03,  IPOLY,
     *			NCOEFF,atcak,'Reciprocal 4th order poly')
	call pbAdd('ATCA',  80.0,120.0,   50.6, 0.03,  IPOLY,
     *			NATCAW,atcaw,'Reciprocal 3th order poly')
c
c  VLA primary beam is taken from AIPS code.
c
	call pbAdd('VLA',     0.071,24.510, 44.3, 0.023,IPOLY,
     *			NCOEFF,vla,'Reciprocal 4th order poly')
c
c  The OVRO primary beam is a gaussian of size is 128.4 arcmin.GHz
c  according to numbers from Shardha Jogee.
c
	call pbAdd('OVRO',24.0,350.0,   107.3, 0.05, GAUS,0,0.,
     *				   'Truncated Gaussian')
c
c  The Hat Ck primary beam is a gaussian of size is 191.67 arcmin.GHz
c  according to "John L"
c
	call pbAdd('HATCREEK',24.0,350.0,   191.67, 0.05, GAUS,0,0.,
     *				   'Truncated Gaussian')
c
c  The following values for the WSRT are derived from the NEWSTAR
c  manual, which gives pb = cos**6(beta*freq(MHz)*angle(degrees))
c  where beta = 0.0629 for f < 500 MHz, and 0.065 for f > 500 MHz.
c  These numbers look a bit large (WSRT under-illuminated?).
c
c  GMX (30Jun2004): added a factor 1.07 to the second line. This
c  number came out of new measurements of the primary beam characteristics, 
c  but is (as yet) not written in stone. Check with Rob Braun or 
c  Tom Oosterloo for more info.

c
	call pbAdd('WSRT',    0.0,0.5,	     51.54, 0.02,  COS6,0,0.,
     *				   'Cos**6 function')
	call pbAdd('WSRT',    0.5,8.0,	49.87*1.07, 0.02,  COS6,0,0.,
     *				   'Cos**6 function')
c
c  Miscellaneous.
c
	call pbAdd('FST',     1.00,2.00,     67.00, 0.05, GAUS,0,0.,
     *				   'Truncated Gaussian')
	call pbAdd('GAUS',    0.0,999.,	      1.00, 0.05, GAUS,0,0.,
     *				   'Truncated Gaussian')
	call pbAdd('SINGLE',  0.0,999.,	      0.00, 0.5,  SINGLE,0,0.,
     *				   'Single dish')
c
c  LOFAR - for simulations
c
	call pbAdd('LOFAR',    0.071,24.510, 44.3, 0.023,IPOLY,
     *                        NCOEFF,vla,'Reciprocal 4th order poly')
c
	end
c************************************************************************
	subroutine pbAdd(tel,f1d,f2d,pbfwhmd,cutoffd,pbtyped,nval,vals,
     *				   descripd)
c
	implicit none
	character tel*(*),descripd*(*)
	integer nval,pbtyped
	real f1d,f2d,pbfwhmd,cutoffd,vals(nval)
c
c  Add a primary beam to our list of known primary beams.
c
c  Input:
c    tel	Primary beam name.
c    f1,f2	Frequency range where valid (in GHz).
c    pbfwhm	Approx primary beam FWHM, in arcmin, at 1 GHz.
c    cutoff	Level below which primary beam is invalid.
c    pbtype	Functional form used to represent the primary beam.
c    nval	Number of values used to parameterize the functional form.
c    vals	The parameterisation of the functional form.
c------------------------------------------------------------------------
	include 'pb.h'
	include 'mirconst.h'
	integer i
c
	npb = npb + 1
	if(npb.gt.MAXPB)call bug('f','Too many primary beams')
	pb(npb) = tel
	f1(npb) = f1d
	f2(npb) = f2d
	descrip(npb) = descripd
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
	else if(pbtyped.eq.BLOCKED)then
	  maxrad(npb) = (2*0.514497*pi*vals(3))**2
	else if(pbtyped.eq.COS6)then
	  maxrad(npb) = acos(cutoff(npb)**0.1666667)**2
	else if(pbtyped.eq.POLY)then
	  call pbradp(.false.,cutoffd,vals,nval,pbfwhmd,maxrad(npb))
	else if(pbtyped.eq.IPOLY)then
	  call pbradp(.true.,cutoffd,vals,nval,pbfwhmd,maxrad(npb))
	endif
	end
c************************************************************************
	subroutine pbradp(doinv,cutoff,coeff,ncoeff,pbfwhm,maxrad)
c
	implicit none
	integer ncoeff
	real cutoff,coeff(ncoeff),pbfwhm,maxrad
	logical doinv
c
c  Determine the maximum radius at which the primary beam is still
c  non-zero.
c
c  Input:
c    doinv	True if the primary beam is 1/P(x)
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
	if(doinv)then
	  a(ncoeff) = a(ncoeff) - 1/cutoff
	else
	  a(ncoeff) = a(ncoeff) - cutoff
	endif
c
c  Now find the roots of the poly.
c
	ifail = 0
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
	      if(real(roots(i)).gt.real(roots(k))) k = i
	    endif
	  endif
	enddo
c
	if(.not.found)call bug('f','Primary beam does not die away')
	maxrad = pbfwhm * pbfwhm * real(roots(k))
c
	end
