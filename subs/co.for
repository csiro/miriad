c************************************************************************
c
c  A set of routines to convert between differing coordinate systems.
c  User callable routines are:
c
c    subroutine coInit(lu)
c    subroutine coCreate(lu)
c    subroutine coDup(lin,lout)
c    subroutine coRaDec(lu,proj,ra0,dec0)
c    subroutine coReinit(lu)
c    subroutine coAxSet(lu,iax,ctype,crpix,crval,cdelt)
c    subroutine coCvt(lu,in,x1,out,x2)
c    subroutine coCvt1(lu,iax,in,x1,out,x2)
c    subroutine coLMN(lu,in,x1,lmn)
c    subroutine coFreq(lu,in,x1,freq)
c    subroutine coVelSet(lu,axis)
c    subroutine coPrjSet(lu)
c    subroutine coFindAx(lu,axis,iax)
c    subroutine coSetd(lu,object,value)
c    subroutine coAxDesc(lu,iax,ctype,crpix,crval,cdelt)
c    subroutine coGauCvt(lu,in,x1,io,bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2)
c    logical function coCompar(lu1,lu2,match)
c    subroutine coLin(lu1,in,x1,n,ctype,crpix,crval,cdelt)
c    subroutine coPrint(lu)
c    subroutine coWrite(lu,tno)
c    subroutine coFin(lu)
c
c  History:
c    rjs   9aug94 Original version.
c    rjs  13sep94 Support 'VELOCITY' and 'FELOCITY' axes.
c    rjs  12oct94 Added a good many things ... for mosaicing.
c************************************************************************
c* coInit -- Initialise coordinate conversion routines.
c& rjs
c: coordinates
c+
	subroutine coInit(lu)
c
	implicit none
	integer lu
c
c  Initialise the coordinate conversion system.
c
c  The coordinate conversion routines supports simultaneously a number
c  of coordinate systems. A coordinate system is initialised by a call
c  to coInit, giving the handle of the Miriad data-set in question.
c  This initialises a new coordinate system. Subsequent calls to the
c  coordinate routines also give the handle of the data-set, which is
c  used to determine the appropriate coordinate system to use.
c
c  Input:
c    lu		The handle of the Miriad data-set. This can be either an
c		image or visibility data-set. For a visibility data-set,
c		the coordinate system simply consists of RA and DEC
c		axes, centered on the observing centre.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k
c
c  Externals.
c
	integer CoLoc
	logical hdprsnt
c
	k = CoLoc(lu,.true.)
	if(nalloc(k).gt.1)return
c
c  Is this an image of a visibility data set? Assume its visibility is
c  the "visdata" item is present.
c
	if(hdprsnt(Lus(k),'visdata'))then
	  call CoInitUV(k)
	else if(hdprsnt(lus(k),'image'))then
	  call CoInitXY(k)
	else
	  call bug('f','Unrecognised dataset type, in CoInit')
	endif
c
c  Finish up initialising this coordinate object.
c
	call coReinit(lu)
	end
c************************************************************************
c* coDup -- Duplicate a coodinate object.
c& rjs
c: coordinates
c+
	subroutine coDup(lin,lout)
c
	implicit none
	integer lin,lout
c
c  Duplicate a coordinate object.
c
c  Input:
c    lin	Handle of the input coordinate object to be
c		duplicated.
c  Output:
c    lout	Duplicated coordinate object.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer i,k1,k2
c
c  Externals.
c
	integer coLoc
c
	call coCreate(lout)
	k1 = coLoc(lin,.false.)
	k2 = coLoc(lout,.false.)
c
	naxis(k2) = naxis(k1)
	do i=1,naxis(k2)
	  ctype(i,k2) = ctype(i,k1)
	  crpix(i,k2) = crpix(i,k1)
	  crval(i,k2) = crval(i,k1)
	  cdelt(i,k2) = cdelt(i,k1)
	enddo
c
	restfreq(k2) = restfreq(k1)
	vobs(k2) = vobs(k1)
c
	call coReinit(lout)
c
	end
c************************************************************************
c* coRaDec -- Create a simple RA/DEC coordinate system.
c& rjs
c: coordinates
c+
	subroutine coRaDec(lu,proj,ra0,dec0)
c
	implicit none
	integer lu
	character proj*(*)
	double precision ra0,dec0
c
c  Create a simple RA/DEC coordinate system.
c
c  Input:
c    proj	Projection geomery (e.g. 'SIN', 'NCP', etc)
c    ra0,dec0	RA,DEC of the reference point.
c  Output:
c    lu		Handle of the output coordinate object.
c--
c------------------------------------------------------------------------
	character ctype*16
c
	call coCreate(lu)
	ctype = 'RA---'//proj
	call coAxSet(lu,1,ctype,0.d0,ra0,1.d0)
	ctype = 'DEC--'//proj
	call coAxSet(lu,2,ctype,0.d0,dec0,1.d0)
	call coReinit(lu)
c
	end
c************************************************************************
c* coCreate -- Begin intialisation of a coordinate object.
c& rjs
c: coordinates
c+
	subroutine CoCreate(lu)
c
	implicit none
	integer lu
c
c  Begin building up a coordinate object from scratch.
c
c  Output:
c    lu		Handle of the coordinate object.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(0,.true.)
	lu = -k
	restfreq(k) = 0
	vobs(k) = 0
	naxis(k) = 0
	end
c************************************************************************
c* coAxSet -- Set the characteristics of a particular axis.
c& rjs
c: coordinates
c+
	subroutine coAxSet(lu,iax,ctypei,crpixi,crvali,cdelti)
c
	implicit none
	integer lu,iax
	character ctypei*(*)
	double precision crpixi,crvali,cdelti
c
c  Set the coordinates of an axis to something.
c
c  Input:
c    lu		Handle of the coordinate object.
c    iax	Axis number.
c    ctypei,...	FITS-style ctype,crpix,crval,cdelt
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer i,k
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
	if(iax.gt.MAXNAX.or.iax.lt.1)
     *	  call bug('f','Illegal axis number')
c
	do i=naxis(k)+1,iax-1
	  ctype(i,k) = ' '
	  crpix(i,k) = 1
	  crval(i,k) = 0
	  cdelt(i,k) = 1
	enddo
c	  	
	naxis(k) = max(naxis(k),iax)
	ctype(iax,k) = ctypei
	crpix(iax,k) = crpixi
	crval(iax,k) = crvali
	cdelt(iax,k) = cdelti
c
	end
c************************************************************************
c* coSetd -- Set the value in the guts of the coordinate routines.
c& rjs
c: coordinates
c+
	subroutine coSetd(lu,object,value)
c
	implicit none
	integer lu
	character object*(*)
	double precision value
c
c  Set a value in the guts of the coordinate routines!
c
c  Input:
c    lu		Handle of the coordinate object.
c    object	Name of the thing to set.
c    value	Value to use.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k,l
	logical ok
	character obj*8
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
c
	obj = object
	l = ichar(obj(6:6)) - ichar('0')
	ok = l.ge.1.and.l.le.MAXNAX
c
	if(obj.eq.'restfreq')then
	  restfreq(k) = value
	else if(obj.eq.'vobs')then
	  vobs(k) = value
	else if(obj(1:5).eq.'crval'.and.ok)then
	  crval(l,k) = value
	else if(obj(1:5).eq.'crpix'.and.ok)then
	  crpix(l,k) = value
	else if(obj(1:5).eq.'cdelt'.and.ok)then
	  cdelt(l,k) = value
	else
	  call bug('f','Unrecognised object in coSetd')
	endif
c
	end
c************************************************************************
c* coReinit -- Finish initialisation of a coordinate object.
c& rjs
c: coordinates
c+
	subroutine CoReinit(lu)
c
	implicit none
	integer lu
c
c  Finish up initialising a coordinate object.
c
c  Input:
c    lu		Handle of the coordinate object.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k,i
	logical ok
c
c  Externals.
c
	integer coLoc
c
c  Find the index of this object.
c
	k = coLoc(lu,.false.)
c
c  Convert the coordinate type to an enumerated type, and check for
c  consistency of celestial coordinates.
c
	ilat(k) = 0
	ilong(k) = 0
	ifreq(k) = 0
	ok = .true.
	do i=1,naxis(k)
	  call CoTyCvt(ctype(i,k),cotype(i,k),coproj(k))
c
c  Check that we have a compatible set of celestial coordinates.
c
	  if(cotype(i,k).eq.FREQ.or.cotype(i,k).eq.VELO.or.
     *	    cotype(i,k).eq.FELO)then
	    ok = ok.and.ifreq(k).eq.0
	    ifreq(k) = i
	  else if(cotype(i,k).eq.LAT)then
	    ok = ok.and.ilat(k).eq.0
	    ilat(k) = i
	  else if(cotype(i,k).eq.LON)then
	    ok = ok.and.ilong(k).eq.0
	    ilong(k) = i
	  endif
	enddo
c
c  Check the celestial coordinates.
c
	if(ilat(k).ne.0.and.ilong(k).ne.0)then
	  if(ok)call CoCompat(ctype(ilong(k),k),ctype(ilat(k),k),ok)
	else if(ilat(k).ne.0.or.ilong(k).ne.0)then
	  ok = .false.
	else
	  ok = .true.
	endif
c
c  Check everything makes sense.
c
	if(.not.ok)then
	  call bug('w','Something is screwy with the axes definitions')
	  call bug('w',' ... assuming linear coordinate systems')
	  do i=1,naxis(k)
	    if(cotype(i,k).eq.LAT.or.cotype(i,k).eq.LON)
     *					cotype(i,k) = LINEAR
	  enddo
	  ifreq(k) = 0
	endif
c
	end
c************************************************************************
c* coFin -- Finish up after completing coordinate conversion.
c& rjs
c: coordinates
c+
	subroutine coFin(lu)
c
	implicit none
	integer lu
c
c  This tidies up, and deletes a coordinate system previously initialised
c  with coInit.
c
c  Inputs:
c    lu		Handle of the coordinate system.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
	nalloc(k) = nalloc(k) - 1
	if(nalloc(k).eq.0)Lus(k) = 0
	end
c************************************************************************
c* coCvt -- Convert coordinates.
c& rjs
c: coordinates
c+
	subroutine coCvt(lu,in,x1,out,x2)
c
	implicit none
	integer lu
	character in*(*),out*(*)
	double precision x1(*),x2(*)
c
c  Convert coordinates from one coordinate system to another.
c  Input and output coordinates can be either "pixel" or "world"
c  coordinates, and either "absolute" or "offset".
c
c  "World" coordinates are the normal physical units associated with
c  a coordinate. World coordinates are given in radians (for astronomical
c  positions), GHz (frequencies), km/s (velocities) and lambda (U-V axes).
c
c  Pixel coordinates are fairly conventional.
c
c  For world coordinates, absolute and offset values differ only by the
c  reference world value (crval). The exception is longitude-type axes
c  (e.g. RA), where offset coordinates are offsets on the sky -- that is
c  the offsets are multiplied by the cos(latitude) (cos(DEC)) term.
c
c  For pixel coordinates, absolute and offset values differ by reference
c  pixel value (crpix).
c
c  For visibility datasets (where the axes are simply RA and DEC), "pixel
c  coordinates" are defined as those that would result from imaging (using
c  a conventional 2D Fourier transform algorithm) the data-set with a cell
c  size of 1 radian. Additionally the reference pixel values (crpix) are set
c  to 0. This means that the "absolute pixel" and "offset pixel" coordinates
c  are identical, and that "offset pixel" and "offset world" coordinates
c  differ only by the inaccuracy in the normal small angle approximation for
c  offsets in RA and DEC.
c
c  Input:
c    in		This indicates the units of the input coordinates.
c		It consists of a sequence of,
c		  'op'	Offset pixel coordinate
c		  'ap'	Absolute pixel coordinate
c		  'ow'	Offset world coordinate
c		  'aw'	Absolute world coordinate,
c		one for each coordinate requiring conversion. Each
c		coordinate unit specifier is separated from the other
c		by a slash (/).
c		For example 'ap/ap' indicates two coordinates, both in
c		absolute pixels.
c    x1		The input coordinates, in units as givien by the `in'
c		parameter. The dimensionality must agree with the number
c		of units given in `in'.
c    out	This indicates the units of the output coordinates, in the
c		same fashion as the `in' value. The outputs must correspond
c		one-for-one with the inputs.
c  Output:
c    x2		The output coordinates, in units given by `out'. 
c--
c------------------------------------------------------------------------
	include 'co.h'
c
	logical x1pix(MAXNAX),x1off(MAXNAX),x2pix(MAXNAX),x2off(MAXNAX)
	logical docelest
	integer i,n,nt,k,ira,idec,ivel
	double precision bscal,bzero,scal,temp
c
c  Externals.
c
	integer coLoc
c
c  Determine the operation to be performed.
c
	k = coLoc(lu,.false.)
	call coCrack(in,x1pix,x1off,naxis(k),MAXNAX,n)
	call coCrack(out,x2pix,x2off,n,MAXNAX,nt)
c
c  Convert each of the axes.
c
	do i=1,nt
	  if(i.gt.naxis(k))then
	    if(i.le.n)then
	      x2(i) = x1(i)
	    else
	      x2(i) = 0
	    endif
	  else if(i.gt.n)then
	    if(x2off(i))then
	      x2(i) = 0
	    else if(x2pix(i))then
	      x2(i) = crpix(i,k)
	    else
	      x2(i) = crval(i,k)
	    endif
	  else if(cotype(i,k).eq.LINEAR.or.cotype(i,k).eq.VELO.or.
     *	     cotype(i,k).eq.FREQ)then
	    call CoLinear(crval(i,k),crpix(i,k),cdelt(i,k),
     *		x1pix(i),x1off(i),x2pix(i),x2off(i),bscal,bzero)
	    x2(i) = bscal * x1(i) + bzero
	  else if(cotype(i,k).eq.FELO)then
	    call coFelo(x1(i),x2(i),crval(i,k),crpix(i,k),cdelt(i,k),
     *	     vobs(k),x1pix(i),x1off(i),x2pix(i),x2off(i))
	  else if(cotype(i,k).eq.LAT.or.cotype(i,k).eq.LON)then
	    docelest = .true.
	  else
	    call bug('f','Internal software bug, in coCvt')
	  endif
	enddo
c
	if(docelest)then
	  ira = ilong(k)
	  idec = ilat(k)
	  ivel = ifreq(k)
c
c  Determine the frequency-dependent scale factor.
c
	  if(ivel.eq.0.or.ivel.gt.n)then
	    scal = 1
	  else
	    call coFqFac(x1(ivel),ctype(ivel,k),
     *	      crval(ivel,k),crpix(ivel,k),cdelt(ivel,k),vobs(k),
     *	      x1off(ivel),x1pix(ivel),scal)
	  endif
c
c  Do the conversion. If one of the latitude or longitude is missing,
c  use the reference pixel as the corresponding value.
c
	  if(ira.le.n.and.idec.le.n)then
	    call coCelest(x1(ira),x1(idec),x2(ira),x2(idec),coproj(k),
     *	      crval(ira,k),crpix(ira,k),scal*cdelt(ira,k),
     *	      crval(idec,k),crpix(idec,k),scal*cdelt(idec,k),
     *	      x1pix(ira),x1pix(idec),x2pix(ira),x2pix(idec),
     *	      x1off(ira),x1off(idec),x2off(ira),x2off(idec))
	  else if(idec.le.n)then
	    call coCelest(0.d0,x1(idec),temp,x2(idec),coproj(k),
     *	      crval(ira,k),crpix(ira,k),scal*cdelt(ira,k),
     *	      crval(idec,k),crpix(idec,k),scal*cdelt(idec,k),
     *	      .true.,x1pix(idec),.true.,x2pix(idec),
     *	      .true.,x1off(idec),.true.,x2off(idec))
	  else
	    call coCelest(x1(ira),0.d0,x2(ira),temp,coproj(k),
     *	      crval(ira,k),crpix(ira,k),scal*cdelt(ira,k),
     *	      crval(idec,k),crpix(idec,k),scal*cdelt(idec,k),
     *	      x1pix(ira),.true.,x2pix(ira),.true.,
     *	      x1off(ira),.true.,x2off(ira),.true.)
	  endif
	endif
c
	end
c************************************************************************
c* coFreq -- Convert spectral coordinates to frequency.
c& rjs
c: coordinates
c+
	subroutine coFreq(lu,in,x1,freq1)
c
	implicit none
	integer lu
	character in*(*)
	double precision x1(*),freq1
c
c  Get the frequency corresponding to a particular coordinate.
c
c  Input:
c    lu		Handle of the coordinate object.
c    in		As with coCvt
c    x1		As with coCvt
c  Output:
c    freq1	The frequency.
c--
c------------------------------------------------------------------------
	include 'co.h'
	include 'mirconst.h'
	double precision ckms
	parameter(ckms=0.001*DCMKS)
c
	double precision x2(MAXNAX)
	integer k,itype
	logical ok
c
c  Externals.
c
	integer coLoc
c
c  Check validity.
c
	k = coLoc(lu,.false.)
	ok = ifreq(k).gt.0
	if(ok) ok = restfreq(k).gt.0.or.cotype(ifreq(k),k).eq.FREQ
	if(.not.ok)
     *	  call bug('f','Non-spectral coordinate system, in coFreq')
c
c  Convert the users coordinate to absolute world coordinates.
c  Fill in the reference location in the output, just in case the
c  user was silly enough not to give enough inputs.
c
	x2(ifreq(k)) = crval(ifreq(k),k)
	call coCvt(lu,in,x1,'aw/...',x2)
	freq1 = x2(ifreq(k))
c
c  Convert from velocityes
c
	itype = cotype(ifreq(k),k)
	if(itype.eq.FREQ)then
	  continue
	else if(itype.eq.FELO)then
	  freq1 = restfreq(k) / (1+(freq1+vobs(k))/ckms)
	else if(itype.eq.VELO)then
	  freq1 = restfreq(k) * (1-(freq1+vobs(k))/ckms)
	else
	  call bug('f','Something is screwy, in coFreq')
	endif
c
	end
c************************************************************************
c* coLMN -- Convert celestial coordinates to direction cosines.
c& rjs
c: coordinates
c+
	subroutine coLMN(lu,in,x1,lmn)
c
	implicit none
	integer lu
	character in*(*)
	double precision x1(*),lmn(3)
c
c  Get the direction cosines corresponding to a particular coordinate.
c
c  Input:
c    lu		Handle of the coordinate object.
c    in		As with coCvt
c    x1		As with coCvt
c  Output:
c    lmn	The direction cosines (with respect to the reference
c		position) of the celestial coordinate given by x1.
c--
c------------------------------------------------------------------------
	include 'co.h'
	double precision x2(MAXNAX),ra,dec,ra0,dec0
	integer k
c
c  Externals.
c
	integer coLoc
c
c  Check validity.
c
	k = coLoc(lu,.false.)
	if(ilong(k).eq.0.or.ilat(k).eq.0)
     *	  call bug('f','Non-celestial coordinate system, in coLMN')
c
c  Convert the users coordinate to absolute world coordinates.
c  Fill in the reference location in the output, just in case the
c  user was silly enough not to give enough inputs.
c
	ra0 = crval(ilong(k),k)
	dec0 = crval(ilat(k),k)
	x2(ilong(k)) = ra0
	x2(ilat(k))  = dec0
c
	call coCvt(lu,in,x1,'aw/...',x2)
c
	ra = x2(ilong(k))
	dec = x2(ilat(k))
c
c  Convert to direction cosines.
c
	lmn(1) = sin(ra-ra0) * cos(dec)
	lmn(2) = sin(dec)*cos(dec0) - cos(ra-ra0)*cos(dec)*sin(dec0)
	lmn(3) = sin(dec)*sin(dec0) + cos(ra-ra0)*cos(dec0)*cos(dec) 
	end
c************************************************************************
c* coCvt1 -- Do coordinate conversion on one axis only.
c& rjs
c: coordinates
c+
	subroutine coCvt1(lu,iax,in,x1,out,x2)
c
	implicit none
	integer lu,iax
	double precision x1,x2
	character in*(*),out*(*)
c
c  This converts a coordinate, for a particular axis, from one system to
c  another.
c
c  Input:
c    lu		Handle of the coordinate system.
c    iax	Axis number.
c    in,out	These indicate the conversion to be performed.
c    x1		Input coordinate.
c  Output:
c    x2		Output, converted, coordinate.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k,ira,idec,n
	logical x1off,x2off,x1pix,x2pix
	double precision bscal,bzero,dtemp
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
c
	if(iax.lt.1)call bug('f','Invalid axis, in coCvt1')
	x2 = x1
	if(iax.gt.naxis(k))return
c
	call coCrack(in,x1pix,x1off,1,1,n)
	if(n.ne.1)call bug('f','Invalid conversion, in coCvt1')
	call coCrack(out,x2pix,x2off,1,1,n)
	if(n.ne.1)call bug('f','Invalid conversion, in coCvt1')
c
c  Convert a linear axis.
c
	if(cotype(iax,k).eq.LINEAR.or.cotype(iax,k).eq.VELO.or.
     *	   cotype(iax,k).eq.FREQ)then
	  call CoLinear(crval(iax,k),crpix(iax,k),cdelt(iax,k),
     *		x1pix,x1off,x2pix,x2off,bscal,bzero)
	  x2 = bscal * x1 + bzero
c
c  Convert an RA axis.
c
	else if(cotype(iax,k).eq.LON)then
	  ira  = ilong(k)
	  idec = ilat(k)
	  call coCelest(x1,0.d0,x2,dtemp,coproj(k),
     *	    crval(ira,k),crpix(ira,k),cdelt(ira,k),
     *	    crval(idec,k),crpix(idec,k),cdelt(idec,k),
     *	    x1pix,.true.,x2pix,.true.,x1off,.true.,x2off,.true.)
c
c  Convert a DEC axis.
c
	else if(cotype(iax,k).eq.LAT)then
	  ira  = ilong(k)
	  idec = ilat(k)
	  call coCelest(0.d0,x1,dtemp,x2,coproj(k),
     *	    crval(ira,k),crpix(ira,k),cdelt(ira,k),
     *	    crval(idec,k),crpix(idec,k),cdelt(idec,k),
     *	    .true.,x1pix,.true.,x2pix,.true.,x1off,.true.,x2off)
c
c  Convert a FELO axis.
c
	else if(cotype(iax,k).eq.FELO)then
	  call coFelo(x1,x2,crval(iax,k),crpix(iax,k),cdelt(iax,k),
     *	     vobs(k),x1pix,x1off,x2pix,x2off)
	endif
c
	end
c************************************************************************
c* coGauCvt -- Change gaussian parameters between world and pixel coords
c& rjs
c: coordinates
c+
	subroutine coGauCvt(lu,in,x1,ing,bmaj1,bmin1,bpa1,
     *				     outg,bmaj2,bmin2,bpa2)
c
	implicit none
	integer lu
	double precision x1(*)
	character in(*),ing*(*),outg*(*)
	real bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2
c
c  This converts the parameters that describe a gaussian between pixel and
c  world coordinate systems. The gaussian lies in the image formed by the
c  first two axes of the coordinate system.
c
c  Input:
c    lu		Handle of the coordinate system.
c    x1		This gives the coordinate of the centroid of the gaussian.
c    in		This gives the units of the input gaussian centroid. This is
c		in the same format as the "in" parameter of coCvt. For
c		example, to indicate that the input coordinate consists of
c		3 numbers in absolute world units, use 'aw/aw/aw'.
c    ing,outg	These give the conversion to be performed. Possible values
c		are:
c		  'w'	Input or output is in world units.
c		  'p'	Input or output is in pixel units.
c    bmaj1,bmin1,bpa1  Input gaussian parameters: major axis, minor axis
c		and position angle of major axis. The position angle is
c		measured from "north" through "east" where north is the
c		direction of increasing value along the second axis,
c		and east is the direction of increasing value along the
c		first axis. bmaj and bmin will either be in world or
c		pixel units. bpa will be in radians.
c  Output:
c    bmaj2,bmin2,bpa2 Output gaussian parameters.
c--
c------------------------------------------------------------------------
	include 'co.h'
	include 'mirconst.h'
c
	logical x1pix(MAXNAX),x1off(MAXNAX)
	integer k,ivel,n
	real dx,dy,sinpa,cospa
	double precision scale
c
c  Externals.
c
	integer coLoc
c
c  Determine the operation to be performed.
c
	k = coLoc(lu,.false.)
	call coCrack(in,x1pix,x1off,naxis(k),MAXNAX,n)
c
c  Get the scale factors to scale cdelt1 and cdelt2 by for this coordinate.
c
	dx = cdelt(1,k)
	dy = cdelt(2,k)
	if(cotype(1,k).eq.LAT.or.cotype(1,k).eq.LON.or.
     *	   cotype(2,k).eq.LAT.or.cotype(2,k).eq.LON)then
	  ivel = ifreq(k)
	  if(ivel.ne.0.and.ivel.le.n)then
	    call coFqFac(x1(ivel),ctype(ivel,k),
     *	      crval(ivel,k),crpix(ivel,k),cdelt(ivel,k),vobs(k),
     *	      x1off(ivel),x1pix(ivel),scale)
	    if(cotype(1,k).eq.LAT.or.cotype(1,k).eq.LON)dx = dx * scale
	    if(cotype(2,k).eq.LAT.or.cotype(2,k).eq.LON)dy = dy * scale
	  endif
	endif
c
c  Get the increments in a standard form.
c
	if(ing.eq.outg)then
	  dx = 1
	  dy = 1
	else if(ing.eq.'w')then
	  continue
	else if(ing.eq.'p')then
	  dx = 1/dx
	  dy = 1/dy
	else
	  call bug('f','Unrecognised operation in coGauCvt')
	endif
c
c  Do the conversion.
c
	sinpa = sin(bpa1)
	cospa = cos(bpa1)
c
	bmaj2 = bmaj1 * sqrt( (cospa/dy)**2 + (sinpa/dx)**2 )
	bmin2 = bmin1 * sqrt( (cospa/dx)**2 + (sinpa/dy)**2 )
c
c  Now the position angle. Fiddle the angle to be in the range
c  [-0.5*pi,0.5*pi]. Also handle the case where the pa is about 0.5*pi
c
	bpa2 = mod(bpa1,pi)
	if(bpa2.gt. 0.5*pi) bpa2 = bpa2 - pi
	if(bpa2.lt.-0.5*pi) bpa2 = bpa2 + pi
	if(abs(bpa2).lt.0.25*pi)then
	  bpa2 = atan(dy/dx * tan(bpa2) )
	else
	  bpa2 = 0.5*pi - atan(dx/dy * tan(0.5*pi - bpa2) )
	  if(bpa2.gt.0.5*pi) bpa2 = bpa2 - pi
	endif
c
	end
c************************************************************************
c* coVelSet -- Change the velocity axis between freq/velo/felo formats.
c& rjs
c: coordinates
c+
	subroutine coVelSet(lu,type)
c
	implicit none
	integer lu
	character type*(*)
c
c  This changes the axis type for the `velocity' axis of a coordinate
c  system. The `velocity' axis can be changed between a frequency,
c  radio velocity or optical velocity axis.
c
c  Input:
c    lu		Handle of the coordinate system.
c    type	Either 'frequency', 'radio' or 'optical'. This causes
c		the velocity axis to be changed to frequency, radio
c		velocity or optical velocity respectively.
c--
c------------------------------------------------------------------------
	include 'co.h'
	include 'mirconst.h'
	double precision ckms
	parameter(ckms=0.001*DCMKS)
c
	integer k,ivel,otype,itype
	double precision f,df
	character frame*4
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
	ivel = ifreq(k)
	if(ivel.eq.0)
     *	  call bug('f','Call to coVelSet for non-velocity axis')
	itype = cotype(ivel,k)
c
c  Determine the type.
c
	if(type.eq.'frequency')then
	  otype = FREQ
	else if(type.eq.'radio')then
	  otype = VELO
	else if(type.eq.'optical')then
	  otype = FELO
	else
	  call bug('f','Unrecognised conversion type, in coVelSet')
	endif
	if(otype.eq.itype)return
	if(restfreq(k).le.0)
     *	  call bug('f','Unable to do axis conversion as restfreq==0')
c
c Save the reference frame
c
      if (ctype(ivel,k)(5:5).eq.'-')then
        frame = ctype(ivel,k)(6:8)
      else
        frame = '???'
      end if
c
c Determine the frequency of the reference pixel
c
      if (itype.eq.FELO) then
        f = restfreq(k) / (1+(crval(ivel,k)+vobs(k))/ckms)
        df = -(cdelt(ivel,k)/ckms) * f * (f/restfreq(k))
      else if (itype.eq.VELO) then
        f = restfreq(k) * (1-(crval(ivel,k)+vobs(k))/ckms)
        df = -(cdelt(ivel,k)/ckms) * restfreq(k)
      else
        f = crval(ivel,k)
        df = cdelt(ivel,k)
      end if
c
c Perform the transformation
c
      if (otype.eq.FELO) then
        crval(ivel,k) =  ckms*(restfreq(k)/f-1) - vobs(k)
        cdelt(ivel,k) = -ckms*(df/f)*(restfreq(k)/f)
        ctype(ivel,k) = 'FELO-'//frame
      else if (otype.eq.VELO) then
        crval(ivel,k) =  ckms*(1-f/restfreq(k)) - vobs(k)
        cdelt(ivel,k) = -ckms*(df/restfreq(k))
        ctype(ivel,k) = 'VELO-'//frame
      else
        crval(ivel,k) = f
        cdelt(ivel,k) = df
        ctype(ivel,k) = 'FREQ-'//frame
      endif
      cotype(ivel,k) = otype
c
      end
c************************************************************************
c* coAxDesc -- Give information about a particular axis.
c& rjs
c: coordinates
c+
	subroutine coAxDesc(lu,iax,ctypei,crpixi,crvali,cdelti)
c
	implicit none
	integer lu,iax
	character ctypei*(*)
	double precision crpixi,crvali,cdelti
c
c  Return information about a particular axis.
c
c  Input:
c    lu		Handle of the coordinate system.
c    iax	Axis number.
c  Output:
c    ctypei	Axis type (FITS-style).
c    crpixi	Reference pixel.
c    crvali	Reference value.
c    cdelti	Increment in linear system approximation.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
	if(iax.gt.naxis(k))then
	  ctypei = ' '
	  crpixi = 1
	  crvali = 0
	  cdelti = 1
	else
	  ctypei = ctype(iax,k)
	  crpixi = crpix(iax,k)
	  crvali = crval(iax,k)
	  cdelti = cdelt(iax,k)
	endif
c
	end
c************************************************************************
c* coFindAx -- Locate a particular axis in the coordinate system.
c& rjs
c: coordinates
c+
	subroutine coFindAx(lu,axis,iax)
c
	implicit none
	integer lu,iax
	character axis*(*)
c
c  Locate an axis of a particular type. The type of the axis is given
c  by the "axis" parameter.
c
c  Input:
c    lu		Handle of the coordinate system.
c    axis	This can be either. Case is unimportant.
c		Often this will be a normal FITS-style ctype value, with
c		or without the projection/rest-frame part.
c		If the projection/rest-frame part is omitted, "axis" will
c		match all projections/rest-frames.
c
c		For example axis = 'ra' will match 'ra---ncp' and 'ra---sin'
c		whereas     axis = 'ra---sin' will match only 'ra---sin'.
c
c		It can also be one of:
c		  'spectral'   for FREQ, VELO and FELO axes
c		  'frequency'  as above, but only if there is enough
c			       information to convert to frequency.
c		  'latitude'   for DEC, GLAT and ELAT axes
c		  'longitude'  for RA, GLON and ELON axes.
c
c  Output:
c    iax	The axis index of the desired axis. A value of 0 is
c		returned if the axis is not found.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer i,k,length
	character type*16
	logical match
c
c  Externals.
c
	integer coLoc,len1
c
	k = coLoc(lu,.false.)
	type = axis
	call ucase(type)
	length = len1(axis)
c
c  Do the special cases.
c
	iax = 0
	if(type.eq.'SPECTRAL')then
	  iax = ifreq(k)
	else if(type.eq.'FREQUENCY')then
	  iax = ifreq(k)
	  if(iax.gt.0.and.restfreq(k).le.0.and.
     *		cotype(iax,k).ne.FREQ) iax = 0
	else if(type.eq.'LONGITUDE')then
	  iax = ilong(k)
	else if(type.eq.'LATITUDE')then
	  iax = ilat(k)
	else if(length.gt.len1(ctype(1,k)))then
	  continue
	else
	  do i=1,naxis(k)
	    if(type(1:length).eq.ctype(i,k)(1:length))then
	      match =  length.eq.len(ctype(i,k))
	      if(.not.match)match = ctype(i,k)(length+1:)        .eq.' '
     *				.or.ctype(i,k)(length+1:length+1).eq.'-'
	      if(match.and.iax.ne.0)call bug('f',
     *		'Multiple matching axes in coFndAx, for axis='//type)
	      if(match) iax = i
	    endif
	  enddo
	endif
c
	end
c************************************************************************
c* coCompar - Compare two coordinate systems for likeness
c& rjs
c: coordinates
c+
	logical function coCompar(lu1,lu2,match)
c
	implicit none
	integer lu1,lu2
	character match*(*)
c
c  Compare two coordinate systems for likeness. This returns .true.
c  if they are alike, and .false. otherwise. The "match" argument
c  determines the tests performed for likeness.
c
c  Input:
c    lu1,lu2	Handles of the two coordinate systems to be compared.
c    match	This determines the tests for likeness to be applied.
c		Tolerances are 0.01%, except for match='approx' (see below).
c		Possible values are:
c
c		  Value		Description
c		  -----		-----------
c		  'exact'	Check that ctype,crval,crpix,cdelt are
c				identical.
c		  'projection'	Check that ctype,crval are the same. Thus it
c				check whether the coordinate systems have the
c				same projection and reference value.
c		  'offset'	Check ctype,crval,cdelt. Thus this checks
c				whether the coordinate 	systems are identical
c				within a shift of the pixel coordinates.
c		  'approx'	Check whether the two coordinate systems are
c				approximately the same (less than 0.1 pixel
c				difference at the reference pixel of the first
c				system, cdelts that agree to within 1%, and
c				compatible ctypes.
c		  'align'	crpixs are the same.
c
c  Output:
c    coCompar	True if the two coordinate systems are alike, and false
c		otherwise.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer k1,k2,i,n
	double precision x1(7),x2(7)
c
c  Externals.
c
	integer coLoc
c
	k1 = coLoc(lu1,.false.)
	k2 = coLoc(lu2,.false.)
	coCompar = .false.
c
c  Check that the number of axes are the same.
c
	if(naxis(k1).ne.naxis(k2))return
	n = naxis(k1)
c
c  Switch to the right operation.
c
	if(match.eq.'exact')then
	  do i=1,n
	    if(ctype(i,k1).ne.ctype(i,k2))return
	    if(abs(crpix(i,k1)-crpix(i,k2)).gt.1e-4)return
	    if(abs(crval(i,k1)-crval(i,k2)).gt.
     *		1e-4*min(abs(cdelt(i,k1)),abs(cdelt(i,k2))) ) return
	    if(abs(cdelt(i,k1)-cdelt(i,k2)).gt.
     *			1e-4*abs(cdelt(i,k1))) return
	  enddo
	else if(match.eq.'projection')then
	  do i=1,n
	    if(ctype(i,k1).ne.ctype(i,k2))return
	    if(abs(crval(i,k1)-crval(i,k2)).gt.
     *		1e-4*min(abs(cdelt(i,k1)),abs(cdelt(i,k2))) ) return
	  enddo
	else if(match.eq.'offset')then
	  do i=1,n
	    if(ctype(i,k1).ne.ctype(i,k2))return
	    if(abs(crval(i,k1)-crval(i,k2)).gt.
     *		1e-4*min(abs(cdelt(i,k1)),abs(cdelt(i,k2))) ) return
	    if(abs(cdelt(i,k1)-cdelt(i,k2)).gt.
     *			1e-4*abs(cdelt(i,k1))) return
	  enddo
	else if(match.eq.'align')then
	  do i=1,n
	    if(abs(crpix(i,k1)-crpix(i,k2)).gt.1e-4)return
	  enddo
	else if(match.eq.'approx')then
c
c  Check the two systems for comparibility.
c
	  do i=1,n
	    if(ctype(i,k1)(1:5).ne.ctype(i,k2)(1:5))return
	    if(abs(cdelt(i,k1)-cdelt(i,k2)).gt.
     *			1e-2*abs(cdelt(i,k1))) return
	  enddo
c
c  Determine the absolute pixel location, in system 2, of the reference
c  pixel, in system 1.
c
	  do i=1,7
	    x1(i) = 0
	  enddo
	  call coCvt(lu1,'op/op/op/op/op/op/op',x1,
     *			 'aw/aw/aw/aw/aw/aw/aw',x2)
	  call coCvt(lu2,'aw/aw/aw/aw/aw/aw/aw',x2,
     *			 'ap/ap/ap/ap/ap/ap/ap',x1)
c
c  Check whether they more or less line up at the reference pixel.
c
	  do i=1,n
	    if(abs(x1(i)-crpix(i,k1)).gt.0.1)return
	  enddo
	else
	  call bug('f','Unrecognised match operation, in coCompar')
	endif
c
	coCompar = .true.
c
	end
c************************************************************************
c* coLin -- Generate a linearized approximation to a nonlinear system.
c& rjs
c: coordinates
c+
	subroutine coLin(lu,in,x1,n,ctype1,crpix1,crval1,cdelt1)
c
	implicit none
	integer lu,n
	character in*(*),ctype1(n)*(*)
	double precision x1(*),crpix1(n),crval1(n),cdelt1(n)
c
c  Generate a linearised approximation to a non-linear coordinate
c  system.
c
c  Input:
c    lu		Handle of the input coordinate system.
c    x1		Input coordinate. The linearised approximation
c		is performed around this point.
c    in		String describing the format of x1 coordinate, as
c		per coCvt.
c    n		Number of axes to output.
c  Output:
c    ctype1 )
c    crpix1 )	Output coordinate descriptions.
c    crval1 )
c    cdelt1 )
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'co.h'
	integer i,k
	double precision xp(MAXNAX),xw(MAXNAX),delta
	double precision xp1(MAXNAX),xw1(MAXNAX),xp2(MAXNAX),xw2(MAXNAX)
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
c
c  Convert to absolute pixels.
c
	do i=1,naxis(k)
	  xp(i) = crpix(i,k)
	  xw(i) = crval(i,k)
	enddo
	call coCvt(lu,in,x1,'aw/...',xw)
	call coCvt(lu,in,x1,'ap/...',xp)
c
c  Increment by one pixel in all directions.
c
	do i=1,naxis(k)
	  xp1(i) = xp(i)
	  xp2(i) = xp(i)
	enddo
c
	do i=1,n
	  if(i.le.naxis(k))then
	    ctype1(i) = ctype(i,k)
	    crval1(i) = crval(i,k)
	    if(cotype(i,k).eq.LON.or.cotype(i,k).eq.LAT.or.
     *	       cotype(i,k).eq.FELO)then
	      xp1(i) = xp(i) - 1
	      xp2(i) = xp(i) + 1
	      call coCvt(lu,'ap/...',xp1,'aw/...',xw1)
	      call coCvt(lu,'ap/...',xp2,'aw/...',xw2)
	      xp1(i) = xp(i)
	      xp2(i) = xp(i)
	      cdelt1(i) = xw2(i) - xw1(i)
	      delta     = xw(i) - crval1(i)
	      if(cotype(i,k).eq.LON)then
	        if(cdelt1(i).lt.-dpi)cdelt1(i) = cdelt1(i) + 2*dpi
	        if(cdelt1(i).gt. dpi)cdelt1(i) = cdelt1(i) - 2*dpi
		if(delta    .lt.-dpi)delta     = delta     + 2*dpi
		if(delta    .gt. dpi)delta     = delta     - 2*dpi
	      endif
	      cdelt1(i) = 0.5d0 * cdelt1(i)
	      crpix1(i) = xp(i) - delta/cdelt1(i)
	      if(cotype(i,k).eq.LON)
     *	        cdelt1(i) = cdelt1(i) * cos( crval(ilat(k),k) )
	      if(cotype(i,k).ne.FELO)ctype1(i)(6:8) = 'CAR'
	    else
	      cdelt1(i) = cdelt(i,k)
	      crpix1(i) = crpix(i,k)
	    endif
	  else
	    ctype1(i) = ' '
	    crpix1(i) = 1
	    crval1(i) = 1
	    cdelt1(i) = 1
	  endif
	enddo
	end
c************************************************************************
c* coPrint -- Print a summary of the coordinate conversion information.
c& rjs
c: coordinates
c+
	subroutine coPrint(lu)
c
	implicit none
	integer lu
c
c  Print out a description of a coordinate system.
c
c  Input:
c    lu		Handle of the coordinate system.
c--
c------------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'
      integer i,k,p
      character line*80,aval*8,units*8,radec*20,pols*4
c
c  Externals.
c
      integer coLoc
      character rangle*32,hangle*32,PolsC2P*2
c
      k = coLoc(lu,.false.)
      do i=1,naxis(k)
	units = ' '
        aval = ctype(i,k)
c
c  RA.
c
        if (aval(1:4).eq.'RA--') then
	  radec = hangle(crval(i,k))
          write (line, 20) aval, radec,
     *                      crpix(i,k),180*3600/pi*cdelt(i,k),'  arcsec'
20        format (a8, 3x, a11, f10.2, 3x, 1pe13.6,a)
c
c  DEC, Galactic and Ecliptic coordinates.
c
        else if (aval(1:4).eq.'DEC-'.or.
     *		 aval(1:4).eq.'GLON'.or.aval(1:4).eq.'GLAT'.or.
     *		 aval(1:4).eq.'ELON'.or.aval(1:4).eq.'ELAT') then
	  radec = rangle(crval(i,k))
          write (line, 30) aval, radec,
     *                      crpix(i,k),180*3600/pi*cdelt(i,k),'  arcsec'
30        format (a8, 2x, a12, f10.2, 3x, 1pe13.6,a)
c
c  STOKES.
c
	else if(aval(1:6).eq.'STOKES')then
	  p = nint(crval(i,k))
	  if(p.eq.0)then
	    pols = 'beam'
	  else
	    pols = PolsC2P(p)
	  endif
          write (line, 35) aval, pols
35        format (a8, 8x, a)
c
c  Others.
c
        else
	  if(aval(1:4).eq.'FELO'.or.aval(1:4).eq.'VELO')then
	    units = 'km/sec'
	  else if(aval(1:4).eq.'FREQ')then
	    units = 'GHz'
	  else if(aval(1:3).eq.'UU-'.or.aval(1:3).eq.'VV-')then
	    units = 'lambda'
	  endif
          write(line, 40)aval,crval(i,k),crpix(i,k),cdelt(i,k),units
40        format (a8, 2x, 1pe13.6, 0pf9.2, 3x, 1pe13.6,2x,a)
        endif
c
        call output(line)
      enddo
c
      end
c************************************************************************
c* coPrjSet -- Set celestial projection type.
c& rjs
c: coordinates
c+
	subroutine coPrjSet(lu,p)
c
	implicit none
	integer lu
	character p*(*)
c
c  Set changes the coordinate system celestial projection type. This will
c  rarely be a useful routine. Probably its only use is for changing
c  projection type from SIN to NCP for old datasets.
c
c  Input:
c    lu		Handle of the coordinate system.
c    p		Projection type. This should be one of 'ncp','sin',
c		'tan','arc' and 'car'.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer i,k
	character proj*8,base*5
c
c  Externals.
c
	integer coLoc
c
	k = coLoc(lu,.false.)
	coproj(k) = p
	proj = p
	call ucase(proj)
	do i=1,naxis(k)
	  base = ctype(i,k)(1:5)
	  if(base.eq.'RA---'.or.base.eq.'DEC--'.or.base.eq.'ELON-'.or.
     *	     base.eq.'ELAT-'.or.base.eq.'GLON-'.or.base.eq.'GLAT-')
     *	    ctype(i,k)(6:) = proj
	enddo
c
	end
c************************************************************************
c* coWrite -- Write out the coordinate description to an image dataset.
c& rjs
c: coordinates
c+
	subroutine coWrite(lu,tno)
c
	implicit none
	integer lu,tno
c
c  This writes out the coordinate system description to an image
c  dataset.
c
c  Input:
c    lu		Handle of the coordinate object.
c    tno	Handle of the output dataset.
c--
c------------------------------------------------------------------------
	include 'co.h'
	integer i,k
	character num*2
c
c  Externals.
c
	integer coLoc
	character itoaf*2
c
	k = coLoc(lu,.false.)
c
	do i=1,naxis(k)
	  num = itoaf(i)
	  call wrhdd(tno,'crval'//num,crval(i,k))
	  call wrhdd(tno,'crpix'//num,crpix(i,k))
	  call wrhdd(tno,'cdelt'//num,cdelt(i,k))
	  call wrhda(tno,'ctype'//num,ctype(i,k))
	enddo
c
	if(restfreq(k).ne.0)call wrhdd(tno,'restfreq',restfreq(k))
	call wrhdd(tno,'vobs',vobs(k))
	end
c************************************************************************
	subroutine CoCompat(ctype1,ctype2,ok)
c
	implicit none
	character ctype1*(*),ctype2*(*)
	logical ok
c
c  Check that two celestial coordinates represent a consistent pair.
c------------------------------------------------------------------------
	character type1*16,type2*16
c
c  "Sort" them.
c
	if(ctype1.lt.ctype2)then
	  type1 = ctype1
	  type2 = ctype2
	else
	  type1 = ctype2
	  type2 = ctype1
	endif
c
c  The projection code (characters 6:8) should be the same,
c  and make sure We should have a RA/DEC, GLAT/GLON, ELAT/ELON pair.
c
	ok = (type1(6:8).eq.type2(6:8)).and.
     *	     ((type1(1:5).eq.'DEC--'.and.type2(1:5).eq.'RA---').or.
     *	      (type1(1:5).eq.'GLAT-'.and.type2(1:5).eq.'GLON-').or.
     *	      (type1(1:5).eq.'ELAT-'.and.type2(1:5).eq.'ELON-'))
	end
c************************************************************************
	subroutine CoInitXY(k)
c
	implicit none
	integer k
c
c  Initialise the coordinate information for an image data-set.
c  Note that we have commented out the code to change SIN projections
c  into NCP for E-W telescopes.
c------------------------------------------------------------------------
	include 'co.h'
c
	integer i
	character num*2
c	character telescop*16
c	double precision dtemp
c	logical ewdone,ew
c
c  Externals.
c
	character itoaf*2
c	logical hdprsnt
c
	call rdhdi(lus(k),'naxis',naxis(k),0)
	if(naxis(k).eq.0)
     *	  call bug('f','Invalid value for NAXIS, in CoInit')
c
	call rdhdd(lus(k),'restfreq',restfreq(k),0.d0)
c	if(.not.hdprsnt(lus(k),'vobs'))
c    *	  call bug('w','VOBS item missing -- assuming it is 0')
	call rdhdd(lus(k),'vobs',vobs(k),0.d0)
c
c	ewdone = .false.
	do i=1,naxis(k)
	  num = itoaf(i)
	  call rdhdd(lus(k),'crval'//num,crval(i,k),0.d0)
	  call rdhdd(lus(k),'crpix'//num,crpix(i,k),0.d0)
	  call rdhdd(lus(k),'cdelt'//num,cdelt(i,k),0.d0)
	  call rdhda(lus(k),'ctype'//num,ctype(i,k),' ')
	  if(cdelt(i,k).eq.0)then
	    if(ctype(i,k).ne.' ')then
	      call bug('w',
     *		'Axis increment (cdelt) is 0 for '//ctype(i,k))
	      call bug('w','Assuming an axis increment of 1')
	    endif
	    cdelt(i,k) = 1
	  endif

c
c	  if(ctype(i,k)(5:8).eq.'-SIN')then
c	    if(.not.ewdone)then
c	      call rdhda(lus(k),'telescop',telescop,' ')
c	      ew = telescop.ne.' '
c	      if(ew)call obspar(telescop,'ew',dtemp,ew)
c	      if(ew) ew = dtemp.gt.0
c	      ewdone = .true.
c	    endif
c	    if(ew)ctype(i,k)(5:8) = '-NCP'
c	  endif
	enddo
c
	end
c************************************************************************
	subroutine CoTyCvt(type,itype,proj)
c
	implicit none
	character type*(*),proj*(*)
	integer itype
c
c  Convert a coordinate type to an enumerated type.
c
c------------------------------------------------------------------------
	include 'co.h'
c
	integer NTYPES
	parameter(NTYPES=16)
	character types(NTYPES)*8
	integer itypes(NTYPES)
c
	character umsg*64
	integer l,l1,l2,i
	logical more
c
c  Externals.
c
	integer len1,binsrcha
c
c  The list of recognised ctypes. Note this list MUST be in
c  alphabetic order.
c
	data (types(i),itypes(i),i=1,NTYPES)/
     *	  'DEC     ',   LAT,
     *	  'ELAT    ',	LAT,
     *	  'ELON    ',	LON,
     *	  'FELO    ',	FELO,
     *	  'FELOCITY',	FELO,
     *	  'FREQ    ',	FREQ,
     *	  'GLAT    ',	LAT,
     *	  'GLON    ',	LON,
     *	  'POINTING',   LINEAR,
     *	  'RA      ',	LON,
     *	  'SDBEAM  ',	LINEAR,
     *	  'STOKES  ',	LINEAR,
     *	  'UU      ',	LINEAR,
     *	  'VELO    ',	VELO,
     *	  'VELOCITY',	VELO,
     *	  'VV      ',	LINEAR/
c
c  Get the first part, and check for a match.
c
	itype = 0
	l = index(type,'-') - 1
	if(l.le.0) l = len1(type)
	if(l.gt.0) l = binsrcha(type(1:l),types,ntypes)
	if(l.gt.0) itype = itypes(l)
c
c  If its a latitude or longitude, check the last part for a known one.
c
	if(itype.eq.LAT.or.itype.eq.LON)then
	  l2 = len1(type)
	  l1 = l2
	  more = .true.
	  dowhile(l1.gt.1.and.more)
	    more = type(l1-1:l1-1).ne.'-'
	    if(more)l1 = l1 - 1
	  enddo
	  if(type(l1:l2).eq.'NCP')then
	    proj = 'ncp'
	  else if(type(l1:l2).eq.'SIN')then
	    proj = 'sin'
	  else if(type(l1:l2).eq.'ARC')then
	    proj = 'arc'
	  else if(type(l1:l2).eq.'TAN')then
	    proj = 'tan'
	  else if(type(l1:l2).eq.'CAR')then
	    proj = 'car'
	  else 
	    umsg = 'Using cartesian projection for axis '//type
	    call bug('w',umsg)
	    proj = 'car'
	  endif
	else if(type.eq.' ')then
	  itype = LINEAR
	else if(itype.eq.0)then
	  l = len1(type)
	  umsg = 'Assuming axis '//type(1:l)//
     *		' is a linear coordinate system'
	  call bug('w',umsg)
	  itype = LINEAR
	endif
c
	end	
c************************************************************************
	subroutine CoInitUV(k)
c
	implicit none
	integer k
c
c  Initialise the coordinate information for a visibility data-set.
c
c------------------------------------------------------------------------
	include 'co.h'
	character telescop*16
	real dra,ddec
	logical ew
	double precision dtemp
c
	naxis(k) = 2
	call uvrdvra(lus(k),'telescop',telescop,' ')
	ew = .false.
	if(telescop.ne.' ')call obspar(telescop,'ew',dtemp,ew)
	if(ew) ew = dtemp.gt.0
	if(ew)then
	  ctype(1,k) = 'RA---NCP'
	  ctype(2,k) = 'DEC--NCP'
	else
	  ctype(1,k) = 'RA---SIN'
	  ctype(2,k) = 'DEC--SIN'
	endif
	call uvrdvrd(lus(k),'ra', crval(1,k),0.d0)
	call uvrdvrd(lus(k),'dec',crval(2,k),0.d0)
	call uvrdvrr(lus(k),'dra',dra,0.)
	if(dra.ne.0) crval(1,k) = crval(1,k) + dra / cos(crval(2,k))
	call uvrdvrr(lus(k),'ddec',ddec,0.)
	crval(2,k) = crval(2,k) + ddec
c
	crpix(1,k) = 0
	crpix(2,k) = 0
	cdelt(1,k) = 1
	cdelt(2,k) = 1
c
	end
c************************************************************************
	integer function CoLoc(lu,alloc)
c
	implicit none
	integer lu
	logical alloc
c------------------------------------------------------------------------
	include 'co.h'
	integer i,free
c
	logical first
	save first
	data first/.true./
c
	if(first)then
	  do i=1,MAXOPEN
	    Lus(i) = 0
	    nalloc(i) = 0
	  enddo
	  first = .false.
	endif
c
c  Locate the slot for this lu.
c
	free = 0
	do i=1,MAXOPEN
	  if(lus(i).eq.lu.and.nalloc(i).gt.0)then
	    if(alloc)nalloc(i) = nalloc(i) + 1
	    CoLoc = i
	    return
	  else if(nalloc(i).eq.0)then
	    free = i
	  endif
	enddo
c
c  We did not find it. If we are allowed to allocate one, do so.
c
	if(alloc.and.free.ne.0)then
	  if(lu.eq.0)then
	    lus(free) = -free
	  else
	    lus(free) = lu
	  endif
	  nalloc(free) = 1
	  CoLoc = free
	  return
	endif
c
	call bug('f','Unable to find coordinate object in CoInit')
c
	end
c************************************************************************
	subroutine coCrack(code,x1pix,x1off,defn,maxnax,n)
c
	integer maxnax,n,defn
	logical x1pix(maxnax),x1off(maxnax)
	character code*(*)
c
c  Decode a coordinate conversion specifier code.
c
c  Input:
c    code
c    maxnax
c  Output:
c    x1pix,x1off
c    n
c------------------------------------------------------------------------
	logical new,pad
	integer i
	character c*1
c
	n = 0
	new = .true.
	pad = .false.
c
	do i=1,len(code)
	  c = code(i:i)
	  if(c.le.' ')then
	    continue
	  else if(c.eq.'/')then
	    new = .true.
	  else if(c.eq.'.')then
	    pad = .true.
	  else
	    if(new)then
	      n = n + 1
	      if(n.gt.maxnax)call bug('f','Too many axes, in coCvt')
	      x1pix(n) = .false.
	      x1off(n) = .false.
	      new = .false.
	    endif
	    if(c.eq.'p'.or.c.eq.'P')then
	      x1pix(n) = .true.
	    else if(c.eq.'w'.or.c.eq.'W')then
	      x1pix(n) = .false.
	    else if(c.eq.'o'.or.c.eq.'O')then
	      x1off(n) = .true.
	    else if(c.eq.'a'.or.c.eq.'A')then
	      x1off(n) = .false.
	    else
	      call bug('f','Unrecognised conversion code, in coCvt')
	    endif
	  endif
	enddo
c
	if(pad.and.n.ge.1.and.n.lt.defn)then
	  do i=n+1,defn
	    x1pix(i) = x1pix(n)
	    x1off(i) = x1off(n)
	  enddo
	  n = defn
	endif
c
	end
c************************************************************************
	subroutine coFqFac(x1,type,xval,xpix,dx,vobs,x1off,x1pix,scal)
c
	implicit none
	double precision x1,xval,xpix,dx,vobs,scal
	logical x1off,x1pix
	character type*4
c
c  Determine the frequency-dependent scaling factor of the increments in
c  the RA and DEC axes.
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision ckms
	parameter(ckms = 0.001d0 * DCMKS)
	double precision x
c
c  Convert to offset units.
c
	if(.not.x1off)then
	  if(x1pix)then
	    x = x1 - xpix
	  else
	    x = x1 - xval
	  endif
	else
	  x = x1
	endif
	if(x1pix) x = x * dx
c
	if(type.eq.'FREQ')then
	  scal = xval / ( xval + x )
	else if(type.eq.'VELO')then
	  scal = ( ckms - (xval + vobs) )/( ckms - (xval + x + vobs) )
	else if(type.eq.'FELO')then
	  if(x1pix)x = x  / ( 1 - x / (ckms + xval + vobs) )
	  scal = ( ckms + (xval + x + vobs) )/( ckms + (xval + vobs) )
	else
	  call bug('f','Unrecognised axis type in coFqFac: '//type)
	endif
c
	end
c************************************************************************
	subroutine coFelo(x10,x2,xval,xpix,dx,vobs,
     *					x1pix,x1off,x2pix,x2off)
c
	implicit none
	double precision x10,x2,xval,xpix,dx,vobs
	logical x1pix,x1off,x2pix,x2off
c
c  Convert between pixels and velocities, to a axes in the optical
c  velocity convention (but derived from data measured at equal frequency
c  increments).
c
c  Input:
c    x10
c    xval,xpix,dx,vobs
c    x1pix,x1off,x2pix,x2off
c  Output:
c    x2
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision ckms
	parameter(ckms = 0.001d0 * DCMKS)
	double precision x1,t
c
c  Convert from absolute to offset units, if required.
c
	if(.not.x1off)then
	  if(x1pix)then
	    x1 = x10 - xpix
	  else
	    x1 = x10 - xval
	  endif
	else
	  x1 = x10
	endif
c
c  Do the conversion.
c
	t = 1 + (xval + vobs) / ckms
	if(x1pix.eqv.x2pix)then
	  x2 = x1
c
c  Convert from pixels to optical velocity.
c
	else if(x1pix)then
	  x2 = dx * x1 / (1 - dx*x1/(t*ckms))
c
c  Convert from optical velocity to pixels.
c
	else
	  x2 = x1 / dx * t / ( 1 + (x1+xval+vobs)/ckms )
	endif
c
c  Convert from offset to absolute units, if required.
c
	if(.not.x2off)then
	  if(x2pix)then
	    x2 = x2 + xpix
	  else
	    x2 = x2 + xval
	  endif
	endif
c
	end
c************************************************************************
	subroutine coCelest(x10,y10,x2,y2,proj,
     *	  xval,xpix,dx,yval,ypix,dy,
     *	  x1pix,y1pix,x2pix,y2pix,x1off,y1off,x2off,y2off)
c
	implicit none
	double precision x10,y10,x2,y2
	character proj*(*)
	double precision xval,yval,xpix,ypix,dx,dy
	logical x1pix,y1pix,x2pix,y2pix
	logical x1off,y1off,x2off,y2off
c
c  Convert celestial coordinates between grid and world coordinates.
c
c  Input:
c    x10,y10	Input celestial coordinate, some combination of pixels or
c		radians.
c    proj	Projection code. One of ncp,sin,tan,car,arc
c    x1pix,y1pix,x2pix,y2pix Logicals. True if the particular coordinate
c		is a pixel coordinate.
c    x1off,y1off,x2off,y2off Logicals. True if the particular coordinate
c		is an offset coordinate.
c
c    dx,xval,xpix: cdelt,crval and crpix values.
c    dy,yval,ypix
c  Output:
c    x2,y2
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision r,s,t,u,L,M,cosyval,sinyval,sinDalp,Dalp,eps
	double precision x1,y1
	logical more
	integer niters
	integer MAXITER
	double precision TOL
	parameter(MAXITER=50,TOL=1d-12)
c
	cosyval = cos(yval)
	sinyval = sin(yval)
c
c  Convert from offset to absolute, if needed.
c
	x1 = x10
	y1 = y10
	if(x1off)then
	  if(x1pix)then
	    x1 = x1 + xpix
	  else
	    x1 = x1 / cosyval + xval
	  endif
	endif
	if(y1off)then
	  if(y1pix)then
	    y1 = y1 + ypix
	  else
	    y1 = y1 + yval
	  endif
	endif
c
c  Convert from x,y to RA,DEC.
c
	if((x1pix.eqv.x2pix).and.(y1pix.eqv.y2pix))then
	  continue
	else if(x1pix.and.y1pix)then
	  L = (x1 - xpix) * dx
	  M = (y1 - ypix) * dy
c
	  if(proj.eq.'ncp')then
	    t = cosyval - M*sinyval
	    Dalp = atan2(L,t)
	    y2 = sign(acos(t/cos(Dalp)),yval)
c
	  else if(proj.eq.'sin')then
	    t = sqrt(1-L*L-M*M)
	    y2 = asin(M*cosyval+sinyval*t)
	    Dalp = atan2(L,(cosyval*t - M*sinyval))
c
	  else if(proj.eq.'car')then
	    Dalp = L / cosyval
	    y2 = yval + M
c
	  else if(proj.eq.'tan')then
	    t = cosyval - M*sinyval
	    Dalp = atan2(L,t)
	    y2 = atan(cos(Dalp)/t * (M*cosyval + sinyval))
c
	  else if(proj.eq.'arc')then
	    t = sqrt(L*L + M*M)
	    s = cos(t)
	    if(t.gt.0)then
	      t = sin(t) / t
	    else
	      t = 1
	    endif
	    r  = M*cosyval * t + sinyval*s
	    y2 = asin(r)
	    Dalp = atan2(L*t*cosyval,s-r*sinyval)
	  endif
c
	  x2 = Dalp + xval
c
c  Convert from RA,y to x,DEC.
c
	else if(y1pix)then
	  Dalp = x1 - xval
	  M = (y1 - ypix) * dy
c
	  if(proj.eq.'ncp')then
	    t = cosyval - M*sinyval
	    y2 = sign(acos(t/cos(Dalp)),yval)
	    L = tan(Dalp)*t
c
	  else if(proj.eq.'sin')then
	    r = sinyval*cos(Dalp)
	    s = cosyval
	    y2 = atan2(r,s) + asin(M/sqrt(r*r+s*s))
	    L = sin(Dalp) * cos(y2)
c
	  else if(proj.eq.'car')then
	    L = Dalp * cos(yval)
	    y2 = yval + y1
c
	  else if(proj.eq.'tan')then
	    y2 = atan(cos(Dalp)*(sinyval + M*cosyval)/
     *			(cosyval-M*sinyval))
	    L = sin(Dalp)/(tan(y2)*sinyval+cos(Dalp)*cosyval)
c
	  else if(proj.eq.'arc')then
	    sinDalp = sin(Dalp)
	    r = atan2(sinyval,cosyval*cos(Dalp))
	    y2 = r
	    s = sqrt(1-cosyval*cosyval*sin(Dalp)**2)
	    L = cosyval*sinDalp
	    niters = 0
	    more = .true.
	    dowhile(more)
	      t = sqrt(L*L + M*M)
	      u = cos(t) / s
	      if(abs(u).lt.1)then
		y2 = r + sign(acos(u),M)
	      else
		y2 = r
	      endif
	      if(t.ne.0)then
	        eps = cos(y2) * sinDalp * t/sin(t) - L
	      else
	        eps = cos(y2) * sinDalp - L
	      endif
	      if(niters.gt.10)then
		L = L + 0.5 * eps
	      else
	        L = L + eps
	      endif
	      niters = niters + 1
	      more = abs(eps).gt.TOL.and.niters.lt.MAXITER
	    enddo
	  endif
c
	  x2 = L/dx + xpix
c
c  Convert from x,DEC to RA,y.
c
	else if(x1pix)then
	  L = (x1 - xpix) * dx
c
	  if(proj.eq.'ncp')then
	    Dalp = asin(L/cos(y1))
	    M = (cosyval - cos(y1)*cos(Dalp))/sinyval
c
	  else if(proj.eq.'sin')then
	    Dalp = asin(L/cos(y1))
	    M = sin(y1)*cosyval - cos(y1)*sinyval*cos(Dalp)
c
	  else if(proj.eq.'car')then
	    Dalp = L / cosyval
	    M = (y1 - yval)
c
	  else if(proj.eq.'tan')then
	    Dalp = atan(L*cosyval) + asin(tan(y1)*L*sinyval /
     *			sqrt(1+L*L*cosyval*cosyval) )
	    M = ( sin(y1)*cosyval - cos(y1)*sinyval*cos(Dalp) ) /
     *			(sin(y1)*sinyval + cos(y1)*cosyval*cos(Dalp))
c
	  else if(proj.eq.'arc')then
	    if(abs(cosyval).le.1e-5)then
	      M = acos(sin(y1)/sinyval)
	      M = -sqrt(M*M - L*L)
	      t = sqrt(L*L + M*M)
	      if(t.eq.0)then
		t = 1
	      else
		t = t / sin(t)
	      endif
	    else
	      r = sin(y1)
	      niters = 0
	      more = .true.
	      M = 0
	      dowhile(more)
	        t = sqrt(L*L + M*M)
	        if(t.eq.0)then
		  s = 1
		  t = 1
	        else
		  s = cos(t)
		  t = t/sin(t)
	        endif
	        eps = t*(r - s*sinyval)/cosyval - M
		if(niters.gt.10)then
		  M = M + 0.5*eps
		else
		  M = M + eps
		endif
		niters = niters + 1
	        more = abs(eps).gt.TOL.and.niters.lt.MAXITER
	      enddo
	    endif
	    Dalp = asin(L/(t*cos(y1)))
	  endif
c
	  x2 = Dalp + xval
	  y2 = M / dy + ypix
c
c  Convert from RA,DEC to x,y.
c
	else
	  Dalp = x1 - xval
c
	  if(proj.eq.'ncp')then
	    L = sin(Dalp) * cos(y1)
	    M = (cosyval - cos(y1)*cos(Dalp))/sinyval
c
	  else if(proj.eq.'sin')then
	    L = sin(Dalp) * cos(y1)
	    M = (sin(y1)*cosyval - cos(y1)*sinyval*cos(Dalp))
c
	  else if(proj.eq.'tan')then
	    t = 1/(sin(y1)*sinyval+cos(y1)*cosyval*cos(Dalp))
	    L = sin(Dalp) * cos(y1) * t
	    M = (sin(y1)*cosyval - cos(y1)*sinyval*cos(Dalp)) * t
c
	  else if(proj.eq.'arc')then
	    t = acos(sin(y1)*sinyval+cos(y1)*cosyval*cos(Dalp))
	    if(t.eq.0)then
	      t = 1
	    else
	      t = t / sin(t)
	    endif
	    L = sin(Dalp) * cos(y1) * t
	    M = (sin(y1)*cosyval - cos(y1)*sinyval*cos(Dalp)) * t
c
	  else if(proj.eq.'car')then
	    L = Dalp * cosyval
	    M = (y1 - yval)
	  endif
c
	  x2 = L / dx + xpix
	  y2 = M / dy + ypix
	endif
c
c  We now have the full set of possibilities. Return the variety the
c  caller really wanted.
c
	if(x1pix.eqv.x2pix) x2 = x1
	if(y1pix.eqv.y2pix) y2 = y1
c
c  Convert from absolute to offset coordinates, if needed.
c  Also convert RA and offset RA to a standard range.
c
	if(x2off)then
	  if(x2pix)then
	    x2 = x2 - xpix
	  else
	    x2 = x2 - xval
	    if(x2.gt.dpi)then
	      x2 = x2 - 2*dpi
	    else if(x2.lt.-dpi)then
	      x2 = x2 + 2*dpi
	    endif
	    x2 = x2 * cosyval
	  endif
	else if(.not.x2pix)then
	  if(x2.ge.2*dpi)then
	    x2 = x2 - 2*dpi
	  else if(x2.lt.0)then
	    x2 = x2 + 2*dpi
	  endif
	endif
	if(y2off)then
	  if(y2pix)then
	    y2 = y2 - ypix
	  else
	    y2 = y2 - yval
	  endif
	endif
c
	end
c************************************************************************
	subroutine CoLinear(xval,xpix,dx,x1pix,x1off,x2pix,x2off,
     *	  bscal,bzero)
c
	implicit none
	logical x1pix,x1off,x2pix,x2off
	double precision xpix,dx,xval
	double precision bscal,bzero
c
c  Determine scale factor and offsets to convert from one coordinate
c  system to another.
c
c  The variables bscal and bzero are calculated so that
c
c    out = bscal * in + bzero
c
c  Input:
c    x1pix,x1off
c    x2pix,x2off
c    xpix,dx,xval
c  Output:
c    bscal,bzero
c------------------------------------------------------------------------
	bscal = 1
	bzero = 0
c
c  Convert from absolute to offset units, if needed.
c
	if(.not.x1off)then
	  if(x1pix)then
	    bzero = -xpix
	  else
	    bzero = -xval
	  endif
	endif
c
c  Convert between offset world and offset pixel units, if needed.
c
	if(x1pix.neqv.x2pix)then
	  if(x1pix)then
	    bscal = dx
	  else
	    bscal = 1/dx
	  endif
	  bzero = bscal * bzero
	endif
c
c  Convert from offset to absolute units, if needed.
c
	if(.not.x2off)then
	  if(x2pix)then
	    bzero = bzero + xpix
	  else
	    bzero = bzero + xval
	  endif
	endif
c

	end
