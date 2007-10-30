c************************************************************************
c  History:
c    rjs   mar89  Created.
c    rjs  9jun89  BoxRuns was not correctly calculating jmin and jmax
c		  when shapes were being ANDed in.
c    rjs 11sep89  Improved documentation.
c    rjs 19oct89  @ files are now handled by the key routines, so the code
c		  to handle these was removed.
c    rjs  6nov89  BoxSet takes the 's' flag.
c    rjs  1dec89  Fixed bug in BoxInput, which apparently crept in on 19oct.
c    rjs 25jan90  Minor documentation correction.
c    rjs 27feb90  Added "no user input" option to BoxInput.
c    rjs 30apr90  Specification of coordinates in units other than
c		  absolute pixels. Some more checks.
c   mchw  7may90  Corrected the documentation at head of program.
c    mjs 05mar91  Corrected in-code doc for BoxRuns.
c    rjs 06mar91  More documentation and comments corrections.
c    mjs 27mar91  call to "atod" changed to "atodf".
c    rjs  7jun91  Made region defined by a poly consistent, depending
c		  if you traverse it clockwise or anticlockwise.
c    rjs  5sep91  Added 'relcenter' to appease nebk.
c    rjs 23sep91  Fixed bug which affected relcen axis 2 and 3.
c    rjs  9oct91  Check for rubbish after a box command.
c    nebk11oct91  Increase length of region of interest line to 400
c                 characters in BOXINPUT to allow complex regions.
c    rjs 14oct91  Fixed some bugs in BoxPolyX. Also better rounding
c		  when dealing with polys.
c    rjs  5nov91  Added "quarter" command.
c    rjs 18mar92  Fixed yet more bugs in BoxPolyX
c    rjs 05jan93  Added subroutine BoxBound
c    rjs 09feb93  Accidently called the select routines!!
c    rjs 24nov93  Increase string buffer to 512.
c    rjs 25apr94  Corrected call sequence to hclose.
c    rjs  2sep94  Changes to use the co.for routines.
c    rjs 20oct94  Increase max number of separate regions
c    rjs 23nov94  Added BoxCount to count the pixels in a plane.
c    rjs  6sep95  Check selected region really does fall within the image.
c    rjs 13dec95  Increase MAXSHAPES in BoxRuns.
c    rjs 29jan97  Added BoxDef -- to set the default region of interest,
c		  and removed this capacity from BoxSet.
c    rjs 07jul97  Change argument to coVelSet and replace coAxDesc with coAxGet
c    rjs 09jul97  Correctly handle ANDing with completely flagged plane.
c************************************************************************
c* Boxes -- Summary of region of interest routines.
c& mjs
c: region-of-interest
c+
c  These routines are a collection which allow the user to specify a
c  fairly arbitrary region of interest, and for the programmer to retrieve
c  these specifications in a reasonably convenient form. There are six
c  programmer callable routines:
c
c	subroutine BoxInput(key,file,boxes,maxboxes)
c	subroutine BoxMask(tno,boxes,maxboxes)
c	subroutine BoxSet(boxes,naxis,nsize,flags)
c	subroutine BoxDef(boxes,naxis,blc,trc)
c	subroutine BoxInfo(boxes,naxis,blc,trc)
c	subroutine BoxBound(boxes,shape,naxis,type,blx,trc)
c	logical function BoxRect(boxes)
c	subroutine BoxRuns(naxis,plane,flags,boxes,
c    *			runs,maxruns,nruns,xminv,xmaxv,yminv,ymaxv)
c
c  BoxInput	This reads the region of interest from the task parameters.
c  BoxMask	"AND" in a mask from a data file.
c  BoxSet	The programmer calls this to indicate the size of the
c		image of interest.
c  BoxInfo	Returns information about the region currently selected.
c  BoxRect	This returns .true. if the selected region is purely
c		rectangular (i.e. describable by blc and trc).
c  BoxRuns	The programmer calls this to retrieve the region-of-interest
c		selected for a particular plane.
c
c  The BOX routines work by reading the region sepcified on the command line
c  and breaking it into an intermediate form, stored in the "boxes" array.
c  This intermeidate form consists of a number of subregion specifications
c  which are ANDed and ORed together to produce the output region. The
c  subregions can consist of IMAGE (a rectangular subregion of a range of
c  planes), BOX (a number of rectangular subregions of a range of planes),
c  POLY (a polygonal region of a range of planes) and MASK (an arbitrary
c  region, specified by a mask file). A positive value (the norm) indicates
c  the subregion is "ORed", whereas a negative value (rare) indicates it
c  is "ANDed". Each shape is described by XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
c  which gives the cube which bounds the shape. In some cases these are
c  unknown, In this case, they are set to zero.
c
c  Some shapes require extra DATA (the size of which is given by SIZE):
c    BOX  This requires the blc and trc for the corners of the boxes,
c	  stored as (xmin,ymin),(xmax,ymax). For N boxes, SIZE=4*N.
c	  The boxes are sorted in increasing order of xmin.
c    POLY This also gives the (x1,y1),(x2,y2) coordinate pair of each of
c	  the line segments which make the polygon (except horizontal segments
c	  are discarded). For a poly made of N (non-horizontal) line segments,
c	  SIZE=4*N. The line segments are sorted in increasing order of
c	  min(x1,x2).
c    MASK Additional info is the handle of the mask file.
c
c  The routine BoxRuns returns the region of a particular plane that was
c  selected. This is returned in "runs", which is a table of entries of
c  (j,xmin,xmax). One entry indicates that
c  pixels (xmin,j) to (xmax,j) are selected. There may be multiple (or
c  zero) entries for a particular value of j, though all entries are
c  non-overlapping. The table is in increasing j and xmin.
c--
c************************************************************************
c* BoxInput -- Read command line box specification.
c& mjs
c: region-of-interest
c+
	subroutine BoxInput(key,file,boxes,maxboxes)
c
	implicit none
	character key*(*),file*(*)
	integer maxboxes,boxes(maxboxes)
c
c  Read the box specification from the command line.
c
c  Input:
c    key	The keyword associated with the parameter, generally
c		"region". If this is a blank string, then the user
c		input routines are not called, and the default region
c		of interest is used.
c    file	Name of the Miriad data-set used to extrac coordinate
c		transformation info (in particular crval,crpix and cdelt).
c    maxboxes	The size of the boxes array.
c  Output:
c    boxes	An intermediate form of the boxes specification. This is
c		used by subsequent BOX routines to
c--
c  The variables xytype and ztype give the units that are currently
c  expected for specifying coordinates in x and y, and z. Possible
c  values are
c    xytype: 'abspix', 'relpix' or 'arcsec'.
c    ztype:  'abspix' or 'kms  '.
c  The default is 'abspix'.
c------------------------------------------------------------------------
	include 'boxes.h'
c
	integer ntypes
	parameter(ntypes=10)
	integer nshape,length,k1,k2,n,spare,offset,i,boxtype,lu(3)
	integer iostat
	character types(ntypes)*9,type*9,spec*512,xytype*6,ztype*6
	character line*64
	integer iax,iax1,iax2,tmp(4)
	double precision t1,t2,t3
	logical more,coordini,units
c
c  Externals.
c
	logical keyprsnt
	integer len1
c
	data types(BOX   )/'boxes    '/
	data types(MASK  )/'mask     '/
	data types(POLY  )/'polygon  '/
	data types(IMAGE )/'images   '/
	data types(ARCSEC)/'arcsec   '/
	data types(KMS   )/'kms      '/
	data types(RELPIX)/'relpixel '/
	data types(ABSPIX)/'abspixel '/
	data types(RELCEN)/'relcenter'/
	data types(QUART) /'quarter  '/
c
	coordini = .false.
	xytype = 'abspix'
	ztype  = 'abspix'
	offset = OFFSET0
	nshape = 0
	if(key.ne.' ')then
	  more = keyprsnt(key)
	else
	  more = .false.
	endif
	if(more) call keya(key,spec,' ')
	dowhile(more)
	  k1 = 1
	  k2 = len1(spec)
	  spare = maxboxes - offset - HDR
	  if(spare.lt.0)
     *	    call BoxBug(spec,'Region too complex')
c
c  Determine the subcommand type.
c
	  call GetTok(spec,k1,k2,type,length)
	  boxtype = 0
	  if(length.le.len(types(1)).and.length.gt.0)then
	    do i=1,ntypes
	      if(type(1:length).eq.types(i)(1:length))boxtype = i
	    enddo
	  endif
c
c  Process unit specification subcommands.
c
	  units = .false.
	  if(boxtype.eq.ABSPIX)then
	    xytype = 'abspix'
	    ztype  = 'abspix'
	  else if(boxtype.eq.RELPIX)then
	    xytype = 'relpix'
	    units = .true.
	  else if(boxtype.eq.RELCEN)then
	    xytype = 'relcen'
	    units = .true.
	  else if(boxtype.eq.ARCSEC)then
	    xytype = 'arcsec'
	    units = .true.
	  else if(boxtype.eq.KMS)then
	    ztype = 'kms'
	    units = .true.
c
c  Process region specification subcommands.
c
	  else if(boxtype.eq.IMAGE.or.boxtype.eq.QUART)then
	    boxes(offset+XMIN) = 0
	    boxes(offset+XMAX) = 0
	    boxes(offset+YMIN) = 0
	    boxes(offset+YMAX) = 0
	    boxes(offset+SIZE) = 0
	    call BoxZRnge(spec,k1,k2,boxes(offset+ZMIN),ztype,lu)
	  else if(boxtype.eq.BOX)then
	    boxtype = IMAGE
	    call BoxInt(spec,k1,k2,tmp,n,4,4,xytype,lu)
	    if(n.ne.4)call BoxBug(spec,'Invalid BOX subcommand')
	    boxes(offset+XMIN)	= min(tmp(1),tmp(3))
	    boxes(offset+XMAX)	= max(tmp(1),tmp(3))
	    boxes(offset+YMIN)	= min(tmp(2),tmp(4))
	    boxes(offset+YMAX)	= max(tmp(2),tmp(4))
	    boxes(offset+SIZE)	= 0
	    call BoxZRnge(spec,k1,k2,boxes(offset+ZMIN),ztype,lu)
	  else if(boxtype.eq.POLY)then
	    call BoxPoly(spec,k1,k2,boxes(offset+DATA),n,spare,
     *		boxes(offset+XMIN),xytype,lu)
	    boxes(offset+SIZE) = n
	    call BoxZRnge(spec,k1,k2,boxes(offset+ZMIN),ztype,lu)
	  else if(boxtype.eq.MASK)then
	    if(spare.lt.3)call BoxBug(spec,'Subregion too complex')
	    call BoxMsk(spec,k1,k2,boxes(offset+DATA),
     *		 boxes(offset+XMIN))
	    boxes(offset+SIZE) = 1
	  else
	    call BoxBug(spec,'Unrecognised subregion command')
	  endif
c
c  Finish up with this subcommand.
c
	  if(boxtype.eq.ABSPIX)then
	    continue
	  else if(units)then
	    if(.not.coordini)then
	      coordini = .true.
	      if(file.eq.' ') call bug('f',
     *	        'Only absolute pixel region specification supported')
	      call hopen(lu,file,'old',iostat)
	      if(iostat.ne.0)then
	        line = 'Error opening coordinate info file: '//file
	        call bug('w',line)
	        call bugno('f',iostat)
	      endif
	      call coInit(lu)
	      call rdhdi(lu,'naxis1',lu(2),1)
	      call rdhdi(lu,'naxis2',lu(3),1)
	      call hclose(lu)
	    endif
	    if(boxtype.eq.ARCSEC)then
	      call coFindAx(lu,'longitude',iax1)
	      call coFindAx(lu,'latitude',iax2)
	      if(min(iax1,iax2).ne.1.or.max(iax1,iax2).ne.2)
     *		call BoxBug(spec,'First two axes are not in arcsec')
	    else if(boxtype.eq.KMS)then
	      call coFindAx(lu,'spectral',iax)
	      if(iax.ne.3)call BoxBug(spec,'No spectral axis present')
	      call coAxGet(lu,iax,line,t1,t2,t3)
	      if(line(1:4).ne.'FELO'.and.line(1:4).ne.'VELO')
     *		call coVelSet(lu,'VELO')
	    endif
	  else
	    boxes(offset+ITYPE) = boxtype
	    offset = offset + boxes(offset+SIZE) + HDR
	    nshape = nshape + 1
	  endif
c
c  Check that we have finished this command.
c
	  if(k1.le.k2)call BoxBug(spec,'Unexpected trailing characters')
c
c  Get the next subcommand.
c
	  more = keyprsnt(key)
	  if(more) call keya(key,spec,' ')
	enddo
c
c  Release the coordinate system, if one has been allocated.
c
	if(coordini)call coFin(lu)
c
c  Fill in a default object if none was given.
c
	if(nshape.eq.0)then
	  boxes(offset+ITYPE) = IMAGE
	  boxes(offset+XMIN)  = 0
	  boxes(offset+XMAX)  = 0
	  boxes(offset+YMIN)  = 0
	  boxes(offset+YMAX)  = 0
	  boxes(offset+ZMIN)  = 0
	  boxes(offset+ZMAX)  = 0
	  boxes(offset+SIZE)  = 0
	  nshape = 1
	endif
c
	Boxes(1) = nshape
	Boxes(NX) = 0
	Boxes(NY) = 0
	Boxes(NZ) = 0
	end
c************************************************************************
	subroutine BoxZRnge(spec,k1,k2,zrange,ztype,lu)
c
	implicit none
	character spec*(*),ztype*(*)
	integer k1,k2,zrange(2),lu(3)
c
c  Return the zrange specification.
c
c  Input:
c    spec	The subregion command.
c    ztype	The units used for the z axis. Either 'abspix' or 'kms'.
c    lu		Information used to convert from km/s to pixels.
c  In/Out:
c    k1,k2	The substring of spec still needing to be processed.
c  Output:
c    zrange	The output range in planes.
c
c------------------------------------------------------------------------
	integer n
c
	if(k1.gt.k2)then
	  zrange(1) = 1
	  zrange(2) = 0
	else
	  call BoxInt(spec,k1,k2,zrange,n,1,2,ztype,lu)
	  if(n.eq.1)then
	    zrange(2) = zrange(1)
	  else if(zrange(1).gt.zrange(2))then
	    n = zrange(2)
	    zrange(2) = zrange(1)
	    zrange(1) = n
	  endif
	endif
c
	end
c************************************************************************
	subroutine BoxInt(spec,k1,k2,boxes,n,modulo,nmax,type,lu)
c
	implicit none
	character spec*(*),type*(*)
	integer lu(3)
	integer k1,k2,nmax,n,modulo,boxes(nmax)
c
c  Decode some numbers, that are enclosed within brackets.
c  It should look something like: '(1,2,3,4)'. The numbers can be
c  in a number of units, as given by "type". This routine then
c  converts them to grid units (absolute pixels).
c
c  Input:
c    spec	The string containing the piece to be decoded.
c    nmax	The maximum number of integers to be returned.
c    modulo	The number of integers returned must be some
c		multiple of "modulo".
c    type	One of 'abspix','relpix','arcsec','kms','relcen'.
c    lu		Information used in converting between physical coordinates
c		and grid units.
c  In/Out:
c    k1,k2	This indicates the substring of spec left to process.
c  Output:
c    boxes	Array containing the integers found.
c    n		The number of integers found.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer k0,i
	logical more,ok
	double precision temp,temp2,x1(2),x2(2)
c
	if(spec(k1:k1).ne.'(')
     *	  call BoxBug(spec,'Bad subregion command')
	k1 = k1 + 1
	k0 = k1
	n = 0
	more = .true.
	dowhile(k1.le.k2.and.more)
	  if(spec(k1:k1).eq.','.or.spec(k1:k1).eq.')')then
	    more = spec(k1:k1).eq.','
	    if(k1.le.k0)call BoxBug(spec,'Bad region subcommand')
	    n = n + 1
	    if(n.gt.nmax)
     *	    call BoxBug(spec,'Subregion too complex -- buffer overflow')
	    call atodf(spec(k0:k1-1),temp,ok)
	    if(.not.ok)call BoxBug(spec,'Error decoding a value')
	    if(type.eq.'abspix')then
	      boxes(n) = nint(temp)
	    else if(type.eq.'relpix')then
	      i = mod(n-1,2) + 1
	      call coCvt1(lu,i,'op',temp,'ap',temp2)
	      boxes(n) = nint(temp2)
	    else if(type.eq.'relcen')then
	      i = mod(n-1,2) + 1
	      boxes(n) = nint(temp) + lu(i+1)/2 + 1
	    else if(type.eq.'arcsec')then
	      i = mod(n-1,2) + 1
	      x1(i) = dpi/180/3600 * temp
	      if(i.eq.2)then
		call coCvt(lu,'ow/ow',x1,'ap/ap',x2)
		boxes(n-1) = nint(x2(1))
		boxes(n)   = nint(x2(2))
	      endif
	    else if(type.eq.'kms')then
	      call coCvt1(lu,3,'aw',temp,'ap',temp2)
	      boxes(n) = nint(temp2)
	    endif
	    k0 = k1 + 1
	  endif
	  if(more) k1 = k1 + 1
	enddo
c
c  Do some more checks.
c
	if(k1.gt.k2)call BoxBug(spec,'Bad subregion command')
	if(spec(k1:k1).ne.')')call BoxBug(spec,'Bad subregion command')
	k1 = k1 + 1
	if(mod(n,modulo).ne.0.or.n.eq.0)
     *	  call BoxBug(spec,'Bad number of indices in subregion command')
c
	end
c************************************************************************
	subroutine BoxPoly(spec,k1,k2,verts,n,nmax,xyrange,
     *						xytype,lu)
c
	implicit none
	character spec*(*),xytype*(*)
	integer k1,k2,nmax,verts(2,nmax/2),n,xyrange(4),lu(3)
c
c  Read in and process a description of a polygon region of interest.
c
c  Inputs:
c    spec	The region specification string.
c    xytype	Coordinate units.
c    lu		Information used in converting between physical and
c		grid coordinates.
c    nmax	The max size of boxes that we can handle.
c  In/Out:
c    k1,k2	Delimit the portion of the string left to process.
c  Output:
c    verts	The poly vertices.
c    n		The number of elements in the poly description.
c    xyrange	The value of xmin,xmax,ymin,ymax (in that order).
c
c------------------------------------------------------------------------
	integer k,kd,t
c
	call BoxInt(spec,k1,k2,verts,n,2,nmax-2,xytype,lu)
	n = n /2
c
c  Eliminate redundant vertices -- i.e. those vertices that are colinear
c  with the neighbouring vertices. Also add a vertex at the end to wrap it
c  around
c
	verts(1,n+1) = verts(1,1)
	verts(2,n+1) = verts(2,1)
	kd = 1
	do k=2,n
	  if( (verts(2,k+1)-verts(2,k))*(verts(1,k)-verts(1,kd)).ne.
     *	      (verts(2,k)-verts(2,kd))*(verts(1,k+1)-verts(1,k)))then
	    kd = kd + 1
	    verts(1,kd) = verts(1,k)
	    verts(2,kd) = verts(2,k)
	  endif
	enddo
c
	n = kd
	if(n.lt.3)call BoxBug(spec,'Degenerate polygon in BoxInput')
c
c  Check if the first pixel is colinear. This cannot deal with this, and
c  craps out.
c
	if((verts(2,2)-verts(2,1))*(verts(1,1)-verts(1,n)).eq.
     *	   (verts(2,1)-verts(2,n))*(verts(1,2)-verts(1,1)))then
	  verts(1,1) = verts(1,n)
	  verts(2,1) = verts(2,n)
	  n = n - 1
	endif
c
	if(n.lt.3)call BoxBug(spec,'Degenerate polygon in BoxInput')
c
	n = n + 1
	verts(1,n) = verts(1,1)
	verts(2,n) = verts(2,1)
c
c  Check if it is in clockwise order.
c
	t = 0
	do k=1,n-1
	  t = t + verts(1,k)*verts(2,k+1) - verts(2,k)*verts(1,k+1)
	enddo
c
c  If its clockwise, convert it to anti-clockwise.
c
	if(t.lt.0)then
	  do k=2,n/2
	    kd = n - k + 1
	    t = verts(1,k)
	    verts(1,k) = verts(1,kd)
	    verts(1,kd) = t
	    t = verts(2,k)
	    verts(2,k) = verts(2,kd)
	    verts(2,kd) = t
	  enddo
	endif
c
c  Finally, find the range of the vertices.
c
	xyrange(1) = verts(1,1)
	xyrange(2) = verts(1,1)
	xyrange(3) = verts(2,1)
	xyrange(4) = verts(2,1)
	do k=2,n-1
	  xyrange(1) = min(xyrange(1),verts(1,k))
	  xyrange(2) = max(xyrange(2),verts(1,k))
	  xyrange(3) = min(xyrange(3),verts(2,k))
	  xyrange(4) = max(xyrange(4),verts(2,k))
	enddo
c
c  Return the goodies.
c
	n = n + n
	end
c************************************************************************
	subroutine BoxSort(boxes,n,xyrange)
c
	implicit none
	integer n,boxes(n),xyrange(4)
c
c  The array "boxes" contains n/4 coordinate pairs (x1,y1),(x2,y2).
c  Perhaps reverse these pairs, so that the coordinate with minimum
c  x is first, and then sort all the pairs into order of increasing xmin.
c
c  Input:
c    n		Size of the box array. n/4 is the number of coordinate
c		pairs.
c  In/Out:
c    boxes	Contains the coordinate pairs.
c  Output:
c    xyrange	The minimum and maximum values of x and y in the pairs.
c
c------------------------------------------------------------------------
	integer i,j,x1,y1,x2,y2
c
c
c  Sort the box specifications into increasing order of xmin. This starts
c  by possibly swapping the two coordinates around. Then a simple
c  insert-sort is performed. As n is probably quite small, an insert-sort
c  is quite adequate.
c
	xyrange(1) = boxes(1)
	xyrange(2) = xyrange(1)
	xyrange(3) = boxes(2)
	xyrange(4) = xyrange(3)
c
	do j=1,n,4
	  i = j
c
c  Get the coordinate pair. X1 has the smaller value of x.
c
	  x1 = min(boxes(i  ),boxes(i+2))
	  x2 = max(boxes(i  ),boxes(i+2))
	  y1 = min(boxes(i+1),boxes(i+3))
	  y2 = max(boxes(i+1),boxes(i+3))
c
c  Determine the maximum and minimum values of y.
c
	  xyrange(1) = min(xyrange(1),x1)
	  xyrange(2) = max(xyrange(2),x2)
	  xyrange(3) = min(xyrange(3),y1)
	  xyrange(4) = max(xyrange(4),y2)
c
c  Move all those coordinate pairs which should come after this pair.
c
	  dowhile(i.gt.1.and.boxes(i-4).gt.x1)
	    boxes(i  ) = boxes(i-4)
	    boxes(i+1) = boxes(i-3)
	    boxes(i+2) = boxes(i-2)
	    boxes(i+3) = boxes(i-1)
	    i = i - 4
	  enddo
c
c  Put them back.
c
	  boxes(i  ) = x1
	  boxes(i+1) = y1
	  boxes(i+2) = x2
	  boxes(i+3) = y2
	enddo
c
	end
c************************************************************************
	subroutine BoxMsk(spec,k1,k2,tno,xyzrange)
c
	implicit none
	character spec*(*)
	integer k1,k2,tno,xyzrange(6)
c
c  Open up a mask file, and get information about it.
c
c  Input:
c    spec	The mask-subregion command.
c  In/Out:
c    k1,k2	spec(k1:k2) contains the remainder of the file to be
c		processed.
c  Output:
c    tno	The handle of the mask file.
c    xyzrange	The region of the mask file which is set.
c
c------------------------------------------------------------------------
	integer nsize(3)
c
c  Externals.
c
	logical hdprsnt
c
	if(spec(k1:k1).ne.'('.or.spec(k2:k2).ne.')'.or.k2-k1.le.1)
     *	  call BoxBug(spec,'Bad or missing mask file name')
	call xyopen(tno,spec(k1+1:k2-1),'old',3,nsize)
	if(.not.hdprsnt(tno,'mask'))
     *	  call BoxBug(spec,'Bad or missing mask file')
	call BoxMskPr(tno,xyzrange)
c
	k1 = k2 + 1
	end
c************************************************************************
c* BoxMask -- AND in a mask to the region of interest.
c& mjs
c: region-of-interest
c+
	subroutine BoxMask(tno,boxes,maxboxes)
c
	implicit none
	integer tno,maxboxes,boxes(maxboxes)
c
c  Indicate that a mask is to be applied to the data.
c
c  Input:
c    tno	The handle of the input mask.
c    maxboxes	Size of the boxes array.
c  Input/Output:
c    boxes	The boxes specification.
c--
c------------------------------------------------------------------------
	include 'boxes.h'
	integer offset,i
c
c  Externals.
c
	logical hdprsnt
c
	if(boxes(1).le.0)call bug('f','No input region, in BoxMask')
c
c  If there is no mask of the correct type, just ignore it. Otherwise
c  process it.
c
	if(hdprsnt(tno,'mask'))then
	  offset = OFFSET0
	  do i=1,boxes(1)
	    offset = offset + boxes(offset+SIZE) + HDR
	  enddo
	  if(offset+HDR+1.gt.maxboxes)
     *	    call bug('f','Buffer overflow in BoxMask')
	  boxes(1) = boxes(1) + 1
	  boxes(offset+ITYPE) = -MASK
	  boxes(offset+SIZE) = 1
	  boxes(offset+DATA) = tno  
	  call BoxMskPr(tno,boxes(offset+XMIN))
	endif
	end
c************************************************************************
	subroutine BoxMskPr(tno,xyzrange)
c
	implicit none
	integer tno,xyzrange(6)
c
c  Get information about the masking file that we are about to use.
c
c  Input:
c    tno	The handle of the mask file.
c  Output:
c    xyzrange	The values xmin,xmax,ymin,ymax,zmin,zmax, where the
c		mask file indicates a good pixel.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer n,n1,n2,n3,j,k
	integer mask(maxdim)
	logical found
c
c  Determine the dimensions of the mask.
c
	call rdhdi(tno,'naxis1',n1,1)
	call rdhdi(tno,'naxis2',n2,1)
	call rdhdi(tno,'naxis3',n3,1)
c
c  Now loop through the entire mask, determining what is the blc and trc
c  of the region selected.
c
	xyzrange(1) = n1+1
	xyzrange(2) = 0
	xyzrange(3) = n2+1
	xyzrange(4) = 0
	xyzrange(5) = n3+1
	xyzrange(6) = 0
c
	found = .false.
	do k=1,n3
	  call xysetpl(tno,1,k)
	  do j=1,n2
	    call xymkrd(tno,j,mask,maxdim,n)
	    if(n.ne.0)then
	      xyzrange(1) = min(xyzrange(1),mask(1))
	      xyzrange(2) = max(xyzrange(2),mask(n))
	      xyzrange(3) = min(xyzrange(3),j)
	      xyzrange(4) = max(xyzrange(4),j)
	      xyzrange(5) = min(xyzrange(5),k)
	      xyzrange(6) = max(xyzrange(6),k)
	      found = .true.
	    endif
	  enddo
	enddo
c
	if(.not.found) call bug('f','Image is completely blanked')
c
	end
c************************************************************************
c* BoxDef -- Set the default region of interest.
c& mjs
c: region-of-interest
c+
	subroutine BoxDef(boxes,naxis,blc,trc)
c
	implicit none
	integer naxis,blc(naxis),trc(naxis),boxes(*)
c
c  Set the default region of interest.
c
c  Input:
c    naxis	The dimension of blc and trc.
c    blc
c    trc
c  Input/Output:
c    boxes	This contains an intermediate form of the subregion
c		specified by the user. On output, certain defaults are
c		filled in.
c--
c------------------------------------------------------------------------
	include 'boxes.h'
	integer offset,i,xminv,xmaxv,yminv,ymaxv,zminv,zmaxv
c
	xminv = blc(1)
	xmaxv = trc(1)
	if(naxis.ge.2)then
	  yminv = blc(2)
	  ymaxv = trc(2)
	else
	  yminv = 0
	  ymaxv = 0
	endif
c
	if(naxis.ge.3)then
	  zminv = blc(3)
	  zmaxv = trc(3)
	else
	  zminv = 0
	  zmaxv = 0
	endif
c
	do i=4,naxis
	  if(blc(i).ne.1.or.trc(i).ne.1)call bug('f',
     *	      'Region of interest routines inadequate!')
	enddo
c
	offset = OFFSET0
	if(boxes(1).eq.0)then
	  boxes(1) = 1
	  boxes(offset+ITYPE) = IMAGE
	  boxes(offset+SIZE) = 0
	  boxes(offset+XMIN) = xminv
	  boxes(offset+XMAX) = xmaxv
	  boxes(offset+YMIN) = yminv
	  boxes(offset+YMAX) = ymaxv
	  boxes(offset+ZMIN) = zminv
	  boxes(offset+ZMAX) = zmaxv
	else
	  do i=1,boxes(1)
	    if(boxes(offset+ITYPE).ne.QUART)then
	      if(boxes(offset+XMIN).eq.0) boxes(offset+XMIN) = xminv
	      if(boxes(offset+XMAX).eq.0) boxes(offset+XMAX) = xmaxv
	      if(boxes(offset+YMIN).eq.0) boxes(offset+YMIN) = yminv
	      if(boxes(offset+YMAX).eq.0) boxes(offset+YMAX) = ymaxv
	      if(boxes(offset+ZMIN).eq.0) boxes(offset+ZMIN) = zminv
	      if(boxes(offset+ZMAX).eq.0) boxes(offset+ZMAX) = zmaxv
	    endif
	    offset = offset + HDR + boxes(offset+SIZE)
	  enddo
	endif
c
	end
c************************************************************************
c* BoxSet -- Set default region of interest.
c& mjs
c: region-of-interest
c+
	subroutine BoxSet(boxes,naxis,nsize,flags)
c
	implicit none
	integer naxis,nsize(naxis),boxes(*)
	character flags*(*)
c
c  This checks the box specification given by the user, and applies
c  defaults.
c
c  Input:
c    naxis	The dimension of nsize, minv and maxv.
c    nsize	The dimensions of the data set.
c    flags	Character string giving some extra options. Flags are:
c		  's'	Give warning if region-of-interest is not a regular
c			shape.
c  Input/Output:
c    boxes	This contains an intermediate form of the subregion
c		specified by the user. On output, certain defaults are
c		filled in.
c--
c------------------------------------------------------------------------
	include 'boxes.h'
	integer blc(3),trc(3)
	integer i,offset
c
	if(naxis.lt.1) call bug('f','Bad dimension')
	boxes(NX) = nsize(1)
	if(naxis.lt.2)then
	  boxes(NY) = 1
	else
	  boxes(NY) = nsize(2)
	endif
	if(naxis.lt.3)then
	  boxes(NZ) = 1
	else
	  boxes(NZ) = nsize(3)
	endif
	if(naxis.gt.3)then
	  do i=4,naxis
	    if(nsize(i).gt.1)call bug('f','Can only handle 3D images')
	  enddo
	endif
c
c  If the user did not give any subregion spec, fill in the default box as
c  the region.
c
	offset = OFFSET0
	if(boxes(1).eq.0)then
	  boxes(1) = 1
	  boxes(offset+ITYPE) = IMAGE
	  boxes(offset+SIZE) = 0
	  boxes(offset+XMIN) = 1
	  boxes(offset+XMAX) = boxes(NX)
	  boxes(offset+YMIN) = 1
	  boxes(offset+YMAX) = boxes(NY)
	  boxes(offset+ZMIN) = 1
	  boxes(offset+ZMAX) = boxes(NZ)
	else
c
	  do i=1,boxes(1)
	    if(boxes(offset+ITYPE).eq.QUART)then
	      boxes(offset+ITYPE) = IMAGE
	      boxes(offset+XMIN) = boxes(NX)/4 + 1
	      boxes(offset+XMAX) = max(1,boxes(NX)/4 + boxes(NX)/2)
	      boxes(offset+YMIN) = boxes(NY)/4 + 1
	      boxes(offset+YMAX) = max(1,boxes(NY)/4 + boxes(NY)/2)
	    endif
c
	    if(boxes(offset+XMIN).eq.0) boxes(offset+XMIN) = 1
	    if(boxes(offset+XMAX).eq.0) boxes(offset+XMAX) = boxes(NX)
	    if(boxes(offset+YMIN).eq.0) boxes(offset+YMIN) = 1
	    if(boxes(offset+YMAX).eq.0) boxes(offset+YMAX) = boxes(NY)
	    if(boxes(offset+ZMIN).eq.0) boxes(offset+ZMIN) = 1
	    if(boxes(offset+ZMAX).eq.0) boxes(offset+ZMAX) = boxes(NZ)
c
	    if( boxes(offset+XMIN).lt.1.or.
     *		boxes(offset+XMAX).gt.boxes(NX))
     *	    call bug('f','Subregion extends beyond image on axis 1')
	    if( boxes(offset+YMIN).lt.1.or.
     *		boxes(offset+YMAX).gt.boxes(NY))
     *	    call bug('f','Subregion extends beyond image on axis 2')
	    if( boxes(offset+ZMIN).lt.1.or.
     *		boxes(offset+ZMAX).gt.boxes(NZ))
     *	    call bug('f','Subregion extends beyond image on axis 3')
	    offset = offset + HDR + boxes(offset+SIZE)
	  enddo
	endif
c
c  Check if the region of interest needs to be rectangular, and give
c  a warning message if it needs to be, and is not.
c
	if(index(flags,'s').ne.0)then
	  if(boxes(1).ne.1.or.boxes(OFFSET0+ITYPE).ne.IMAGE)then
	    call bug('w','Only regular regions-of-interest supported')
	    call bug('w','Using bounding box of the selected region')
	    call BoxInfo(boxes,3,blc,trc)
	    boxes(1) = 1
	    boxes(OFFSET0+ITYPE) = IMAGE
	    boxes(OFFSET0+SIZE) = 0
	    boxes(OFFSET0+XMIN) = blc(1)
	    boxes(OFFSET0+XMAX) = trc(1)
	    boxes(OFFSET0+YMIN) = blc(2)
	    boxes(OFFSET0+YMAX) = trc(2)
	    boxes(OFFSET0+ZMIN) = blc(3)
	    boxes(OFFSET0+ZMAX) = trc(3)
	  endif
	endif
	end
c************************************************************************
c* BoxInfo -- Determine bounding box of the region of interest.
c& mjs
c: region-of-interest
c+
	subroutine BoxInfo(boxes,naxis,blc,trc)
c
	implicit none
	integer boxes(*),naxis,blc(naxis),trc(naxis)
c
c  This returns the bounding box of the region of interest.
c
c  Input:
c    boxes	This contains an intermediate form of the subregion
c		specified by the user.
c
c  Output:
c    blc,trc	Min and max values of the region selected along each axis.
c
c--
c------------------------------------------------------------------------
	include 'boxes.h'
	integer xminv,xmaxv,yminv,ymaxv,zminv,zmaxv
	integer i,offset
	logical first
c
c  Go through the region specifications, finding the min and max values
c  of x,y and z.
c
	offset = OFFSET0
	first = .true.
	do i=1,boxes(1)
c
c  Handle the first subregion. This must be ORed with whatever.
c
	  if(first)then
	    if(boxes(offset+ITYPE).gt.0)then
	      xminv = boxes(offset+XMIN)
	      xmaxv = boxes(offset+XMAX)
	      yminv = boxes(offset+YMIN)
	      ymaxv = boxes(offset+YMAX)
	      zminv = boxes(offset+ZMIN)
	      zmaxv = boxes(offset+ZMAX)
	      first = .false.
	    endif
c
c  Handle other than the first, which is ORed.
c
	  else if(boxes(offset+ITYPE).gt.0)then
	    xmaxv = max(xmaxv,boxes(offset+XMAX))
	    xminv = min(xminv,boxes(offset+XMIN))
	    ymaxv = max(ymaxv,boxes(offset+YMAX))
	    yminv = min(yminv,boxes(offset+YMIN))
	    zmaxv = max(zmaxv,boxes(offset+ZMAX))
	    zminv = min(zminv,boxes(offset+ZMIN))
c
c  Handle other than the first, which is ANDed.
c
	  else
	    xmaxv = min(xmaxv,boxes(offset+XMAX))
	    xminv = max(xminv,boxes(offset+XMIN))
	    ymaxv = min(ymaxv,boxes(offset+YMAX))
	    yminv = max(yminv,boxes(offset+YMIN))
	    zmaxv = min(zmaxv,boxes(offset+ZMAX))
	    zminv = max(zminv,boxes(offset+ZMIN))
	  endif
c
	  offset = offset + HDR + boxes(offset+SIZE)
	enddo
c
	if(first) call bug('f','No subregion selected')
	if(xmaxv.lt.xminv.or.ymaxv.lt.yminv.or.zmaxv.lt.zminv)
     *		  call bug('f','No subregion selected')
c
c  Return the info we have to the caller.
c
	blc(1) = xminv
	trc(1) = xmaxv
	if(naxis.ge.2)then
	  blc(2) = yminv
	  trc(2) = ymaxv
	endif
	if(naxis.ge.3)then
	  blc(3) = zminv
	  trc(3) = zmaxv
	endif
c
	do i=4,naxis
	  blc(i) = 1
	  trc(i) = 1
	enddo
	end
c************************************************************************
c* BoxRect -- Determine if a region-of-interest is rectangular.
c& mjs
c: region-of-interest
c+
	logical function BoxRect(boxes)
c
	implicit none
	integer boxes(*)
c
c  This returns .true. if the region described by the boxes array is purely
c  rectangular (i.e. describable by blc and trc).
c
c  Input:
c    boxes	Integer array describing intermediate form of the region
c		selected.
c  Output:
c    BoxRect	True if the region is rectangular.
c--
c------------------------------------------------------------------------
	include 'boxes.h'
	integer i,offset
	logical first,rect
c
c  Go through the region specifications, finding the min and max values
c  of x,y and z.
c
	offset = OFFSET0
	first = .true.
	do i=1,boxes(1)
	  if(first)then
	    if(boxes(offset+ITYPE).gt.0)then
	      first = .false.
	      rect = boxes(offset+ITYPE).eq.IMAGE
	    endif
	  else
	    rect = .false.
	  endif
c
	  offset = offset + HDR + boxes(offset+SIZE)
	enddo
c
	BoxRect = rect
	end
c************************************************************************
c* BoxBound -- Return bounding boxes for each "region" subcommand.
c& mjs
c: region-of-interest
c+
	subroutine BoxBound(boxes,subcmd,naxis,type,mode,blc,trc)
c
	implicit none
	integer boxes(*),subcmd,naxis
	character type*(*),mode*(*)
	integer blc(naxis),trc(naxis)
c
c  Return the bounding box for each region subcommand.
c
c  Input:
c    boxes	The boxes array.
c    subcmd	The subcommand number.
c    naxis	The dimensionality of blc and trc.
c  Output:
c    type	Either 'box','poly' or 'mask'.
c    mode	Either 'or' or 'and'.
c    blc	Blc of the bounding box.
c    trc	Trc of the bounding box.
c--
c------------------------------------------------------------------------
	include 'boxes.h'
	integer i,offset,btype
c
c  Handle the case where we are asked for a non-existent subcommand.
c
	if(subcmd.gt.boxes(1))then
	  type = ' '
	else
c
c  Go through the region specifications to find the required shape.
c  of x,y and z.
c
	  offset = OFFSET0
	  do i=1,subcmd-1
	    offset = offset + HDR + boxes(offset+SIZE)
	  enddo
	  btype = boxes(offset+ITYPE)
	  if(btype.gt.0)then
	    mode = 'or'
	  else
	    mode = 'and'
	  endif
	  btype = abs(btype)
	  if(btype.eq.IMAGE.or.btype.eq.BOX)then
	    type = 'box'
	  else if(btype.eq.POLY)then
	    type = 'poly'
	  else if(btype.eq.MASK)then
	    type = 'mask'
	  else
	    call bug('f','Unrecognised region subcommand, in BoxBound')
	  endif
	  blc(1) = boxes(offset+XMIN)
	  trc(1) = boxes(offset+XMAX)
	  if(naxis.ge.2)then
	    blc(2) = boxes(offset+YMIN)
	    trc(2) = boxes(offset+YMAX)
	  endif
	  if(naxis.ge.3)then
	    blc(3) = boxes(offset+ZMIN)
	    trc(3) = boxes(offset+ZMAX)
	  endif
	  do i=4,naxis
	    blc(i) = 1
	    trc(i) = 1
	  enddo
	endif
	end
c************************************************************************
c* BoxCount -- Count the pixels in the region-of-interest in a plane.
c& mjs
c: region-of-interest
c+
	subroutine BoxCount(Runs,nRuns,nPoint)
c
	implicit none
	integer nRuns,Runs(3,nRuns+1),nPoint
c
c  Count the number of pixels in the region-of-interest in a given
c  plane.
c
c  Input:
c    runs	Runs specifications, as returned by BoxRuns.
c    nruns	Number of runs.
c  Output:
c    nPoint	Number of pixels in the region of interest.
c--
c------------------------------------------------------------------------
	integer i
c
	nPoint = 0
	do i=1,nRuns
	  nPoint = nPoint + Runs(3,i) - Runs(2,i) + 1
	enddo
c
	end
c************************************************************************
c* BoxRuns -- Return region of interest in "runs" form.
c& mjs
c: region-of-interest
c+
	subroutine BoxRuns(naxis,plane,flags,boxes,
     *			runs,maxruns,nruns,xminv,xmaxv,yminv,ymaxv)
c
	implicit none
	character flags*(*)
	integer naxis,plane(naxis)
	integer boxes(*),maxruns,nruns,runs(3,maxruns)
	integer xminv,xmaxv,yminv,ymaxv
c
c  This returns the caller a runs specification for the particular plane
c  that the user is currently interested in.
c
c  Input:
c    naxis	The dimension of "plane".
c    plane	The index, along the third, fourth, etc dimension of the
c		cube to be accessed.
c    flags	A string. Each character indicates an extra step to perform.
c		Currently there is only one possibility:
c		  'r' Make runs specification relative to xminv,yminv.
c    boxes	Some intermediate form of the boxes specified by the user.
c    maxruns	Max number of runs that this program can cope with.
c
c  Output:
c    runs	Runs specifications.
c    nruns	Number of runs.
c    xminv,yminv)	Minimum regions containing the area selected.
c    xmaxv,ymaxv)
c--
c------------------------------------------------------------------------
	include 'boxes.h'
	integer WORKSIZE,MAXSHAPE
	parameter(WORKSIZE=512,MAXSHAPE=256)
	integer n1,n2,n3,pnt1,pnt2,pnt3,i,j,k,boxtype
	integer offset,jmin,jmax,nshapes,shapes(MAXSHAPE)
	integer work(WORKSIZE,3)
c
c  Thios routine can only handle 3D objects. Check that the higher
c  diemsions are all 1.
c
	if(naxis.gt.1)then
	  do i=2,naxis
	    if(plane(i).ne.1) call bug('f','Can only handle 3D images')
	  enddo
	endif
c
c  Determine the subcommands that are appropriate to this plane. If there
c  are no commands, return straight away.
c
	nshapes = 0
	offset = OFFSET0
	do i=1,boxes(1)
	  if(((boxes(offset+ZMIN).le.plane(1).and.
     *	      boxes(offset+ZMAX).ge.plane(1)).or.
     *	      (boxes(offset+ITYPE).lt.0.and.nshapes.gt.0)).and.
     *	    (boxes(offset+ITYPE).gt.0.or.nshapes.gt.0))then
	    nshapes = nshapes + 1
	    if(nshapes.gt.MAXSHAPE)
     *	      call bug('f','Buffer overflow in BoxRuns(nshapes)')
	    shapes(nshapes) = offset
	    if(boxes(offset+ITYPE).gt.0)then
	      if(nshapes.eq.1)then
	        jmin = boxes(offset+YMIN)
	        jmax = boxes(offset+YMAX)
	      else
	        jmin = min(jmin,boxes(offset+YMIN))
	        jmax = max(jmax,boxes(offset+YMAX))
	      endif
	    else
	      jmin = max(jmin,boxes(offset+YMIN))
	      jmax = min(jmax,boxes(offset+YMAX))
	    endif
	  endif
	  offset = offset + HDR + boxes(offset+SIZE)
	enddo
c
	nruns = 0
	runs(1,1) = 0
	if(nshapes.eq.0)return
c
c  Determine the runs appropriate to this plane. This gets the runs from
c  each relavant shape, then merges it with the runs from previous shapes.
c  The previous runs are in one buffer (pointed to by pnt1), the current
c  runs are written in another buffer (pointed to by pnt2), and the merged
c  result is written to a third buffer (pointed to by pnt3). Pnt1 and pnt2
c  vary cyclicly.
c
	do j=jmin,jmax
	  n1 = 0
	  pnt1 = 1
	  do k=1,nshapes
	    pnt2 = mod(pnt1,3) + 1
	    offset = shapes(k)
	    boxtype = boxes(offset+ITYPE)
	    if(boxes(offset+YMIN).le.j.and.boxes(offset+YMAX).ge.j.and.
     *	      (n1.gt.0.or.boxtype.gt.0))then
	      n2 = 0
	      if(abs(boxtype).eq.IMAGE)then
		work(1,pnt2) = boxes(offset+XMIN)
		work(2,pnt2) = boxes(offset+XMAX)
		n2 = 2
	      else if(abs(boxtype).eq.BOX)then
		call BoxBoxX(work(1,pnt2),WORKSIZE,j,
     *			boxes(offset+SIZE)/4,boxes(offset+DATA),n2)
	      else if(abs(boxtype).eq.POLY)then
		call BoxPolyX(work(1,pnt2),WORKSIZE,j,
     *			boxes(offset+SIZE)/2,boxes(offset+DATA),n2)
	      else if(abs(boxtype).eq.MASK)then
		call BoxMskX(work(1,pnt2),WORKSIZE,j,
     *			plane(1),boxes(offset+DATA),n2)
	      else
		call bug('f','Boxes structure is corrupt in BoxRuns')
	      endif
c
c  Merge the specs from multiple shapes together.
c
	      if(boxtype.gt.0)then
		if(n1.le.0)then
		  pnt1 = pnt2
		  n1 = n2
		else
		  pnt3 = mod(pnt2,3) + 1
		  call BoxOr(n1,work(1,pnt1),n2,work(1,pnt2),
     *				n3,work(1,pnt3),WORKSIZE)
		  n1 = n3
		  pnt1 = pnt3
		endif
	      else if(n1.le.0.or.n2.le.0)then
		n1 = 0
	      else
		pnt3 = mod(pnt2,3) + 1
		call BoxAnd(n1,work(1,pnt1),n2,work(1,pnt2),
     *				n3,work(1,pnt3),WORKSIZE)
		n1 = n3
		pnt1 = pnt3
	      endif
	    endif
	  enddo
c
c  Copy the runs to the output buffer.
c
	  if(n1.gt.0)then
	    if(nruns.eq.0)then
	      xminv = work(1,pnt1)
	      xmaxv = work(n1,pnt1)
	      yminv = j
	    else
	      xminv = min(xminv,work(1,pnt1))
	      xmaxv = max(xmaxv,work(n1,pnt1))
	    endif
	    ymaxv = j
	    if(maxruns-nruns.lt.n1/2+1)
     *		call bug('f','Buffer overflow in BoxRuns(output)')
	    do i=1,n1,2
	      nruns = nruns+1
	      runs(1,nruns) = j
	      runs(2,nruns) = work(i,pnt1)
	      runs(3,nruns) = work(i+1,pnt1)
	    enddo
	  endif
	enddo
c
c  We are in the home stretch now. Convert things to indices relative to
c  (xmin,ymin).
c
	if(index(flags,'r').ne.0)then
	  do i=1,nruns
	    runs(1,i) = runs(1,i) - yminv + 1
	    runs(2,i) = runs(2,i) - xminv + 1
	    runs(3,i) = runs(3,i) - xminv + 1
	  enddo
	endif
c
c  Append a trailing "null".
c
	runs(1,nruns+1) = 0
	end
c************************************************************************
	subroutine BoxAnd(n1,in1,n2,in2,nout,out,maxout)
c
	implicit none
	integer n1,n2,nout,maxout
	integer in1(n1),in2(n2),out(maxout)
c
c  "AND" together two runs specifications.
c
c  Input:
c    n1,n2	Sizes of the two input runs.
c    in1,in2	The two input runs.
c    maxout	Size of the output array.
c
c  Output:
c    out	Output buffer to receive the runs.
c    nout	The size of the output runs.
c
c------------------------------------------------------------------------
	integer i1,i2,io,t1,t2
c
	i1 = 1
	i2 = 1
	io = 1
c
c  And the two.
c
	dowhile(i1.lt.n1.and.i2.lt.n2)
	  t1 = max(in1(i1),in2(i2))
	  t2 = min(in1(i1+1),in2(i2+1))
	  if(t1.le.t2)then
	    if(io.gt.maxout)
     *	      call bug('f','Buffer overflow in BoxRuns(and)')
	    out(io) = t1
	    out(io+1) = t2
	    io = io + 2
	  endif
c
	  if(in1(i1+1).lt.in2(i2+1))then
	    i1 = i1 + 2
	  else
	    i2 = i2 + 2
	  endif
	enddo
c
c  Return the size of the output.
c
	nout = io - 1
c
	end
c************************************************************************
	subroutine BoxOr(n1,in1,n2,in2,nout,out,maxout)
c
	implicit none
	integer n1,n2,nout,maxout
	integer in1(n1),in2(n2),out(maxout)
c
c  Merge two runs specifications together.
c
c  Input:
c    n1,n2	Sizes of the two input runs.
c    in1,in2	The two input runs.
c    maxout	Size of the output array.
c
c  Output:
c    out	Output buffer to receive the runs.
c    nout	The size of the output runs.
c
c------------------------------------------------------------------------
	integer i1,i2,io,i,xmax
c
	i1 = 1
	i2 = 1
	io = 1
	xmax = -1
c
c  Merge the two.
c
	dowhile(i1.lt.n1.and.i2.lt.n2)
	  if(in1(i1).lt.in2(i2))then
	    if(in1(i1)-xmax.le.1)then
	      out(io-1) = max(in1(i1+1),xmax)
	    else
	      if(io.gt.maxout)
     *		call bug('f','Buffer overflow in BoxRuns(or)')
	      out(io) = in1(i1)
	      out(io+1) = in1(i1+1)
	      io = io + 2
	    endif
	    i1 = i1 + 2
	  else
	    if(in2(i2)-xmax.le.1)then
	      out(io-1) = max(in2(i2+1),xmax)
	    else
	      if(io.gt.maxout)
     *		call bug('f','Buffer overflow in BoxRuns(or)')
	      out(io) = in2(i2)
	      out(io+1) = in2(i2+1)
	      io = io + 2
	    endif
	    i2 = i2 + 2
	  endif
	  xmax = out(io-1)
	enddo
c
c  Copy whichever one is remaining.
c
	if(i1.lt.n1)then
	  do i=i1,n1,2
	    if(in1(i)-xmax.le.1)then
	      out(io-1) = max(in1(i+1),xmax)
	    else
	      if(io.gt.maxout)
     *		call bug('f','Buffer overflow in BoxRuns(or)')
	      out(io) = in1(i)
	      out(io+1) = in1(i+1)
	      io = io + 2
	    endif
	    xmax = out(io-1)
	  enddo
	else if(i2.lt.n2)then
	  do i=i2,n2,2
	    if(in2(i)-xmax.le.1)then
	      out(io-1) = max(in2(i+1),xmax)
	    else
	      if(io.gt.maxout)
     *		call bug('f','Buffer overflow in BoxRuns(or)')
	      out(io) = in2(i)
	      out(io+1) = in2(i+1)
	      io = io + 2
	    endif
	    xmax = out(io-1)
	  enddo
	else
	  call bug('f','Algorithmic bug in BoxOr')
	endif
c
c  Return the size of the output.
c
	nout = io - 1
c
	end
c************************************************************************
	subroutine BoxMskX(buf,nbuf,y,z,tno,nout)
c
	implicit none
	integer nbuf,buf(nbuf),y,z,tno,nout
c
c------------------------------------------------------------------------
	call xysetpl(tno,1,z)
	call xymkrd(tno,y,buf,nbuf,nout)
	end
c************************************************************************
	subroutine BoxBoxX(goes,maxgoes,j0,nbox,box,ngoes)
c
	implicit none
	integer maxgoes,goes(maxgoes),j0,nbox,box(4,nbox),ngoes
c
c  This breaks up the specification of a number of boxes into runs for
c  row j.
c
c  Input:
c    maxgoes
c    j0
c    box
c    nbox
c  Output:
c    ngoes
c    goes
c
c------------------------------------------------------------------------
	integer k,xmax
c
	xmax = -1
	ngoes = 0
	do k=1,nbox
	  if((j0-box(2,k))*(box(4,k)-j0).ge.0)then
	    if(box(1,k).gt.xmax+1)then
	      if(ngoes+2.gt.maxgoes)
     *		call bug('f','Buffer overflow in BoxRuns(polyx)')
	      goes(ngoes+1) = box(1,k)
	      goes(ngoes+2) = box(3,k)
	      ngoes = ngoes + 2
	    else
	      goes(ngoes) = max(goes(ngoes),box(3,k))
	    endif
	    xmax = goes(ngoes)
	  endif
	enddo
	end
c************************************************************************
	subroutine BoxPolyX(goes,maxgoes,j0,nverts,verts,ngoes)
c
	implicit none
	integer maxgoes,goes(maxgoes),j0,nverts,verts(2,nverts),ngoes
c
c  Calculate the runs which lie within a polygon. It does this by
c  calculating the intersections of a horizontal line with the polygon,
c  then sorting the intersections. There should be an even number of
c  intersections.
c  An added complication is the intersection of the horizontal line with
c  a vertex. Three situations are possibilities: go through a vertex
c  into the interior of the poly, clip a vertex, or
c  go along the edge of the poly. The first case counts as
c  one intersection, the second as two, and the third counts as 0 or 1
c  depending whether we are entering or leaving the selected area.
c
c    /		---^---		   _________
c   /_____	  / \		  /
c   \		 /   \		 /
c    \		/     \		/
c
c  Input:
c    nverts	Number of veritces of the polygon.
c    verts	The vertices of the polygon. The vertices are assumes to have
c		no redundancies (i.e. all vertices are distinct), and to
c		trace out a anti-clockwise path. 
c    j0		The value of y for which we want to determine the runs inside
c		the polygon.
c    maxgoes	Max number of runs is maxgoes/2.
c  Output:
c    goes	The runs for this value of y.
c    ngoes	The number of runs is ngoes/2.
c------------------------------------------------------------------------
	integer k,kprev,l,t
	logical more
c
	ngoes = 0
	kprev = nverts-1
	do k=1,nverts-1,1
c
c  Case of an intersection with a vertex.
c
	  if(verts(2,k).eq.j0)then
	    t = (j0-verts(2,kprev))*(j0-verts(2,k+1))
	    if(t.gt.0)then
	      ngoes = ngoes + 2
	      goes(ngoes-1) = verts(1,k)
	      goes(ngoes)   = verts(1,k)
	    else if(t.lt.0)then
	      ngoes = ngoes + 1
	      goes(ngoes) = verts(1,k)
	    else
	      t =   verts(1,kprev)*( verts(2,k)    -verts(2,k+1)   )
     *		  + verts(1,k)    *( verts(2,k+1)  -verts(2,kprev) )
     *		  + verts(1,k+1)  *( verts(2,kprev)-verts(2,k)     )
	      if(t.gt.0)then
	        ngoes = ngoes + 1
		goes(ngoes) = verts(1,k)
	      endif
	    endif
c
c  Case of an intersection with the line segment between vertices.
c
	  else if((j0-verts(2,k))*(verts(2,k+1)-j0).gt.0)then
	    ngoes = ngoes + 1
	    goes(ngoes) =  nint( verts(1,k+1) +
     *		real( (j0-verts(2,k+1)) * (verts(1,k)-verts(1,k+1)))
     *			/ (verts(2,k)-verts(2,k+1)) )
	  endif
	  kprev = k
	enddo
c
	if(2*(ngoes/2).ne.ngoes)
     *	  call bug('f','Algorithmic failure in BoxRuns(polyx)')
c
c  The list of intersections are not in order. The number of intersections
c  is also likely to be small (probably only two!). Sort the intersections,
c  but use an insert-sort, because its probably ordered, and small.
c
	do k=2,ngoes
	  l = k
	  t = goes(l)
	  more = goes(l-1).gt.t
	  dowhile(more)
	    goes(l) = goes(l-1)
	    l = l - 1
	    more = .false.
	    if(l.gt.1)more = goes(l-1).gt.t
	  enddo
	  goes(l) = t
	enddo
c
c  There are possibly redundancies in the list of runs. Eliminate these.
c
	l = 3
	do k=3,ngoes,2
	  if(goes(k)-goes(l-1).le.1)then
	    goes(l-1) = goes(k+1)
	  else
	    goes(l) = goes(k)
	    goes(l+1) = goes(k+1)
	    l = l + 2
	  endif
	enddo
	ngoes = l-1
c
	end
c************************************************************************
	subroutine BoxBug(spec,message)
c
	implicit none
	character message*(*),spec*(*)
c
c  This generates an error message when a subregion command appears
c  no good. This never returns.
c
c  Input:
c    spec	The subregion command.
c    message	The error message.
c
c------------------------------------------------------------------------
	character line*80
	integer l
c
c  Externals.
c
	integer len1
c
	l = len1(spec)
	line = message//': '//spec(1:l)
	l = min(len(line),l + 2 + len(message))
	call bug('f',line(1:l))
	end

