c************************************************************************
      program regrid
      implicit none
c
c= regrid - regrid an image dataset
c& rjs
c: map analysis
c+
c	REGRID regrids an image by interpolating. The output coordinate
c	system can be specified by a template image or by
c	axis descriptors. Blanked input pixels are excluded from the
c	interpolation. Regridding of any combination of the first three
c	axes of an image is supported.
c
c	REGRID correctly handles conversion
c	  * Between different projection geometries (e.g. SIN,NCP,TAN etc).
c	  * Between equatorial and galactic coordinates.
c	  * From a B1950 input to J2000 output and visa versa,
c	    as well as B1950 to B1950, and J2000 to J2000.
c	  * Between radio/optical velocity definitions and LSR/barycentric
c	    velocity rest frames.
c
c	Nearest neighbour `interpolation' is used for axes smaller in size
c	that 5 pixels. Otherwise an cubic interpolation is used.
c@ in
c	The input image name. No default.
c@ out
c	The output image name.  No default.
c@ axes
c	Specify axes to regrid. For example, axes=1,2 will regrid
c	axes 1 and 2, or axes=2,3 will regrid axes 2 and 3.
c	The default is all axes.
c@ tin
c	Input template image.  The axis descriptors of the regridded
c	image, for those axes specified by keyword "axis", are those
c	of the template image.
c	If neither a template nor a set of "descriptors" (see below) are
c	given, the input is used as an initial template.
c@ desc
c	If "tin" is unset, then this gives the reference value, reference
c	pixel, pixel increment, and number of pixels for the axes 
c	selected by keyword "axes" of the output image, in the same order
c	that they are given.
c
c	Note that for RA/DEC axes, the reference values are in radians, and
c	the increments are in radians on 
c	the sky. Thus, dRA = dX / cos(dec) and dDEC = dY where you 
c	specify dX and dY.
c
c	Defaults are no axis descriptors.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Only the minimum characters to avoid ambiguity is needed.
c	  noscale   Produce a cube where the RA/DEC cell size does not scale
c	            with frequency/velocity.
c	  offset    The coordinate system described by the template or
c	            descriptors is modified (shift and expansion/contraction)
c	            by an integral number of pixels so that it completely
c	            encloses the input.
c	  nearest   Use nearest neighbour interpolation rather than the 
c	            default higher order interpolation scheme.
c	  galeqsw   Switch the output coordinate system between equatorial
c	            and galactic.
c	  equisw    Switch the equinox of the output bwtween B1950 and J2000.
c	            Note that REGRID always produces galactic coordinates in
c	            the B1950 frame.
c@ project
c	This allows the projection of the output to be changed. Possible
c	values are "sin", "tan", "arc", "ncp", "car" or "gls". Note that
c	the NCP projection has a singularity at the equator. The default is
c	to not change the output projection.
c@ rotate
c	Set the rotation between the sky and the image to be this angle,
c	in degrees. The positive value of the angle gives an eastward
c	rotation of the sky grid relative to the pixel grid.
c	The default is not to change the rotation from that of
c	the template, or to have no rotation when there is no template.
c@ tol
c	Interpolation tolerance. Tolerate an error of as much as "tol" in
c	converting pixel locations in the input to the output. The default is
c	0.05. It must be less that 0.5.
c--
c
c  History:
c     7jul92 pjt   Written to keep Neil quiet (not an easy thing to do)
c    23nov92 nebk  CHange to include all three regridding input styles
c		   (template, axis descriptors and start/end/inc).
c                  Deal with blanking, and generally tidy up.  Attempt
c		   to add new code in yucky pjt style. 
c    01dec92 nebk  More sig figs in printout
c      jan93 nebk  Rewrite to implement regridding of upto 3 axes in one
c		   pass. All good fun. NEBK becomes owner. 
c     2feb93 pjt   extracted spline.for to MIRSUBS 
c    10feb93 rjs   redefined double precision dynamic memory array to
c		   size MAXBUF/2.
c    12mar93 mjs   Use maxnax.h file instead of its own set value.
c    22jun93 nebk  Take cos(DEC) into account for RA/DEC axes
c    15apr94 nebk  Keyword range input was failing.
c    03jan95 nebk  REGZ was failing if output longer than input
c    26jun95 nebk  Warning instead of fatal if ungridded axes
c		   have silly axis descriptors. options=quiet
c    16nov96 nebk  AXFNDCG -> AXFNDCO and new call sequence
c    24mar97 rjs   At least warn people when it may screw up because of
c		   blanked pixels.
c    07jul97 rjs   Gone back to the drawing board and rewrite program.
c    22jul97 rjs   Fix bug when subimaging grossly.
c    22jul97 rjs   Support epoch,projection and galactic/equatorial
c		   axis conversion.
c    23jul97 rjs   Add warning about blanked pixels. Add pbtype
c    05aug97 rjs   Messages about equinox fiddles.
c    20nov98 rjs   Handle and eliminate sky rotation.
c    25nov98 rjs   Better handling of sky rotation parameter.
c    18may99 rjs   Handle wrap arounds in the offsets.
c    10jul00 rjs   Add "rotate" keyword, and get rid of "norotate" option.
c    27jun02 rjs   An arg to mkeyd was single precision.
c
c To do:
c----------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	include 'mirconst.h'
c
	character version*(*)
	parameter(version='Regrid: version 1.0 27-Jun-02')
c
	character in*64,out*64,tin*64,ctype*16,cellscal*12,proj*3
	character line*64
	double precision desc(4,MAXNAX),crpix,crval,cdelt,epoch1,epoch2
	double precision llrot,rot
	logical noscale,dooff,doepo,dogaleq,nearest,dorot
	integer ndesc,nax,naxis,nin(MAXNAX),nout(MAXNAX),ntin(MAXNAX)
	integer i,k,n,lIn,lOut,lTmp,cOut,axes(MAXNAX),ilat
	integer GridSize,gnx,gny,minv(3),maxv(3),order(3)
	integer nBuf(3),off(3),minc(3),maxc(3),BufSize,rBuf,lBuf
	integer offset,nxy,nblank
	real tol
c
	integer Xv,Yv,Zv
c
	integer NPROJS
	parameter(NPROJS=6)
	integer nproj
	character projs(NPROJS)*3
c
c  Externals.
c
	logical keyprsnt
c
	data projs/'sin','tan','arc','ncp','car','gls'/
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('in',in,' ')
	if(in.eq.' ')call bug('f','An input must be given')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
	call mkeyi('axes',axes,MAXNAX,nax)
	call keya('tin',tin,' ')
	call mkeyd('desc',desc,4*MAXNAX,ndesc)
	if(mod(ndesc,4).ne.0)
     *	  call bug('f','Invalid number of desc descriptors')
	ndesc = ndesc/4
	call keyr('tol',tol,0.05)
	if(tol.le.0.or.tol.ge.0.5)
     *	  call bug('f','Invalid value for the tol parameter')
	call getopt(noscale,dooff,doepo,dogaleq,nearest)
	call keymatch('project',NPROJS,projs,1,proj,nproj)
	if(nproj.eq.0)proj = ' '
	dorot = keyprsnt('rotate')
	call keyd('rotate',rot,0.0d0)
	rot = DPI/180.0d0 * rot
	call keyfin
c
c  Open the input dataset.
c
	call xyopen(lIn,in,'old',MAXNAX,nin)
	call rdhdi(lIn,'naxis',naxis,0)
	naxis = min(naxis,MAXNAX)
	do i=1,naxis
	  nout(i) = nin(i)
	enddo
	do i=naxis+1,MAXNAX
	  nout(i) = 1
	enddo
c
c  Check the users "axes" specification.
c
	if(nax.gt.0)then
	  do i=1,nax
	    if(axes(i).lt.1.or.axes(i).gt.naxis)
     *	      call bug('f','Invalid "axes" value')
	  enddo
	else
	  nax = naxis
	  do i=1,nax
	    axes(i) = i
	  enddo
	endif
	if(ndesc.ne.0.and.ndesc.ne.nax)call bug('f',
     *	  'Inconsistent number of axes descriptors given')
	call coInit(lIn)
	call coDup(lIn,cOut)
c
c  Set up the output size/coordinate system given template or descriptors.
c
	if(tin.ne.' ')then
	  call xyopen(lTmp,tin,'old',MAXNAX,ntin)
	  call rdhdi(lTmp,'naxis',n,0)
	  n = min(n,MAXNAX)
	  call coInit(lTmp)
	  call coFindAx(lTmp,'latitude',ilat)
	  do i=1,nax
	    if(axes(i).gt.n)call bug('f',
     *		'Requested axis does not exist in the template')
	    nout(axes(i)) = ntin(axes(i))
	    call coAxGet(lTmp,axes(i),ctype,crpix,crval,cdelt)
	    call coAxSet(cOut,axes(i),ctype,crpix,crval,cdelt)
	    if(axes(i).eq.ilat)then
	      call coGetd(lTmp,'llrot',llrot)
	      call coSetd(cOut,'llrot',llrot)
	    endif
	  enddo
	  call coGetd(lTmp,'epoch',epoch1)
	  call coSetd(cOut,'epoch',epoch1)
	  call coGeta(lTmp,'cellscal',cellscal)
	  call coSeta(cOut,'cellscal',cellscal)
	  call xyclose(lTmp)
	else if(ndesc.ne.0)then
	  do i=1,nax
	    nout(axes(i)) = nint(desc(4,i))
	    if(nout(axes(i)).lt.1.or.nout(axes(i)).gt.MAXDIM)
     *	      call bug('f','Invalid axis size in axis descriptor')
	    call coAxGet(lIn,axes(i),ctype,crpix,crval,cdelt)
	    call coAxSet(cOut,axes(i),ctype,desc(2,i),
     *					desc(1,i),desc(3,i))
	  enddo
	else
	  dooff = .true.
	endif
c
c  Set that it does not scale, if requested.
c
	if(noscale)call coSeta(cOut,'cellscal','CONSTANT')
	if(dorot)  call coSetd(cOut,'llrot',rot)
	if(proj.ne.' ')call coPrjSet(cOut,proj)
	call coReInit(cOut)
c
c  Perform epoch and galactic/equatorial switches.
c  If the user wants to produce an offset one ... do that.
c
	if(doepo.or.dogaleq)call CoordSw(cOut,doepo,dogaleq)
	if(dooff)call DoOffset(lIn,nIn,cOut,nOut)
c
c  Check that we can do this.
c
	if(nout(1).gt.MAXDIM)call bug('f','Output too big for me')
	do k=4,naxis
	  if(nOut(k).gt.1)call bug('f','Cannot handle hypercubes')
	enddo
c
c  Create the output.
c
	call xyopen(lOut,out,'new',naxis,nout)
	call MkHeader(lIn,lOut,cOut,version)
c
c  Initialise things.
c
	GridSize = 0
	call pcvtInit(cOut,lIn)
	call BufIni(nBuf,off,minc,maxc,BufSize)
c
c  Give a message about any epoch conversion that is going to happen.
c
	call coGetd(lIn,'epoch',epoch1)
	call coGetd(cOut,'epoch',epoch2)
	if(abs(epoch1-epoch2).gt.0.5)then
	  if(epoch1.eq.0)then
	    call bug('w','Assuming the equinox of the input is B1950')
	    epoch1 = 1950
	  else if(epoch2.eq.0)then
	    call bug('w','Assuming the equinox of the template is '
     *							    //'B1950')
	    epoch2 = 1950
	  endif
	  write(line,'(a,i5,a,i5,a)')'Input equinox is',
     *		   nint(epoch1),'; Output equinox is',nint(epoch2),'.'
	  call output(line)
	endif
c
c
	nblank = 0
	do k=1,nOut(3)
	  call xysetpl(lOut,1,k)
c
c  Determine the size of the coordinate translation grid.
c
	  call GridEst(nOut(1),nOut(2),k,gnx,gny,tol)
c
c  Allocate space used for the coordinate translation grid.
c
	  if(gnx*gny.gt.GridSize)then
	    if(GridSize.gt.0)then
	      call memFree(Xv,GridSize,'r')
	      call memFree(Yv,GridSize,'r')
	      call memFree(Zv,GridSize,'r')
	    endif
	    GridSize = gnx*gny
	    call memAlloc(Xv,GridSize,'r')
	    call memAlloc(Yv,GridSize,'r')
	    call memAlloc(Zv,GridSize,'r')
	  endif
c
c  Calculate the coordinates translation grid, and work out some
c  statistics about it.
c
	  call GridGen(nOut(1),nOut(2),k,
     *		memr(Xv),memr(Yv),memr(Zv),gnx,gny)
	  call GridStat(nearest,memr(Xv),memr(Yv),memr(Zv),gnx,gny,
     *	    nin(1),nin(2),nin(3),tol,minv,maxv,order)
c
	  if(minv(1).gt.maxv(1).or.minv(2).gt.maxv(2).or.
     *				   minv(3).gt.maxv(3))then
	    nblank = nblank + nOut(1)*nOut(2)
	    call BadPlane(lOut,nOut(1),nOut(2))
c
c  Get the required data.
c
	  else
	    call BufGet(lIn,minv,maxv,nIn,nBuf,off,minc,maxc,
     *						rBuf,lBuf,BufSize)
	    offset = minc(3) - off(3) - 1
	    nxy = nBuf(1)*nBuf(2)
c
c  Finally to the interpolation.
c
	    call Intp(lOut,order,nOut(1),nOut(2),
     *	      memr(rBuf+offset*nxy),meml(lBuf+offset*nxy),
     *	      nBuf(1),nBuf(2),maxc(3)-minc(3)+1,
     *	      off(1),off(2),minc(3)-1,
     *	      memr(Xv),memr(Yv),memr(Zv),gnx,gny,nblank)
	  endif
	enddo
c
c  Give warning about the number of blanked pixels.
c
	nblank = (100*nblank)/(nOut(1)*nOut(2)*nOut(3))
	write(line,'(a,i3,a)')
     *	  'Overall fraction of blanked pixels: ',nblank,'%'
	if(nblank.ge.50)then
	  call bug('w',line)
	else if(nblank.ne.0)then
	  call output(line)
	endif
c	
c  All done. Tidy up.
c
	if(BufSize.gt.0)then
	  call memFree(rBuf,BufSize,'r')
	  call memFree(lBuf,BufSize,'l')
	endif
	if(GridSize.gt.0)then
	  call memFree(Xv,GridSize,'r')
	  call memFree(Yv,GridSize,'r')
	  call memFree(Zv,GridSize,'r')
	endif
c
	call xyclose(lOut)
	call xyclose(lIn)
c
	end
c************************************************************************
	subroutine getopt(noscale,offset,doepo,dogaleq,nearest)
c
	implicit none
	logical noscale,offset,doepo,dogaleq,nearest
c
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=5)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'noscale ','offset  ','equisw  ','galeqsw ',
     *		  'nearest '/
c
	call options('options',opts,present,NOPTS)
	noscale = present(1)
	offset  = present(2)
	doepo   = present(3)
	dogaleq = present(4)
	nearest = present(5)
	end
c************************************************************************
	subroutine CoordSw(lOut,doepo,dogaleq)
c
	implicit none
	integer lOut
	logical doepo,dogaleq
c
c  Switch the coordinate system of the output.
c------------------------------------------------------------------------
	double precision crpix1,crval1,cdelt1,crpix2,crval2,cdelt2
	double precision epoch,obstime,ra,dec,dra,ddec
	character ctype1*16,ctype2*16
	integer ira,idec
c
c  Externals.
c
	double precision epo2jul
c
	call coFindAx(lOut,'latitude',idec)
	call coFindAx(lOut,'longitude',ira)
	if(ira.eq.0.or.idec.eq.0)
     *	  call bug('f','Cannot find RA/DEC or GLON/GLAT axes')
	call coAxGet(lOut,ira,ctype1,crpix1,crval1,cdelt1)
	call coAxGet(lOut,idec,ctype2,crpix2,crval2,cdelt2)
	call cogetd(lOut,'epoch',epoch)
	if(epoch.eq.0)then
	  call bug('w','Assuming the equinox of the template '//
     *						'is B1950')
	  epoch = 1950
	endif
	call cogetd(lOut,'obstime',obstime)
	if(obstime.eq.0)obstime = epo2jul(1950.0d0,'B')
c
	if(ctype1(1:4).eq.'RA--'.and.ctype2(1:4).eq.'DEC-'.and.
     *							 dogaleq)then
	  if(abs(epoch-2000).lt.0.1)then
	    call fk54z(crval1,crval2,obstime,ra,dec,dra,ddec)
	    crval1 = ra
	    crval2 = dec
	    epoch = 1950.
	  endif
	  if(abs(epoch-1950).gt.0.1)call bug('f',
     *		'I am unable to convert to B1950 coordinates')
	  call dsfetra(crval1,crval2,.false.,1)
	  ctype1(1:4) = 'GLON'
	  ctype2(1:4) = 'GLAT'
	else if(ctype1(1:4).eq.'GLON'.and.ctype2(1:4).eq.'GLAT'.and.
     *							  dogaleq)then
	  if(abs(epoch-1950.).gt.0.1)call bug('f',
     *	  'Galactic coordinates in non-B1950 system are not understood')
	  call dsfetra(crval1,crval2,.true.,1)
	  ctype1(1:4) = 'RA--'
	  ctype2(1:4) = 'DEC-'
	endif
c
c  Convert the epoch, if needed.
c
	if(ctype1(1:4).eq.'RA--'.and.ctype2(1:4).eq.'DEC-'.and.
     *							   doepo)then
	  if(abs(epoch-1950).lt.0.1)then
	    call fk45z(crval1,crval2,obstime,ra,dec)
	    epoch = 2000
	  else if(abs(epoch-2000).lt.0.1)then
	    call fk54z(crval1,crval2,obstime,ra,dec,dra,ddec)
	    epoch = 1950
	  else
	    call bug('f',
     *		'Cannot convert other than B1950/J2000 equinoxs')
	  endif	    
	  crval1 = ra
	  crval2 = dec
	endif
c
	call coAxSet(lOut,ira,ctype1,crpix1,crval1,cdelt1)
	call coAxSet(lOut,idec,ctype2,crpix2,crval2,cdelt2)
	call cosetd(lOut,'epoch',epoch)
c
	call coReinit(lOut)
c
	end
c************************************************************************
	subroutine DoOffset(lIn,nIn,lOut,nOut)
c
	implicit none
	integer lIn,lOut,nIn(3),nOut(3)
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	double precision tol
	parameter(tol=0.49)
	double precision In(3),Out(3,3,3,3),crpix
	integer minv(3),maxv(3),i,j,k,l
	logical weird(3),warned
c
c  Externals.
c
	character itoaf*2
c
	call pcvtInit(lIn,lOut)
c
	do k=1,3
	  do j=1,3
	    do i=1,3
	      In(1) = 1 + 0.5*(i-1)*(nIn(1)-1)
	      In(2) = 1 + 0.5*(j-1)*(nIn(2)-1)
	      In(3) = 1 + 0.5*(k-1)*(nIn(3)-1)
	      call pcvt(in,out(1,i,j,k),3)
	    enddo
	  enddo
	enddo
c
	do i=1,3
	  weird(i) = .false.
	  minv(i) = nint(Out(i,1,1,1)-0.49)
	  maxv(i) = nint(Out(i,1,1,1)+0.49)
	enddo
c
c  Determine the min and max pixels that we are interested in.
c
	do k=1,3
	  do j=1,3
	    do i=1,3
	      do l=1,3
		minv(l) = min(minv(l),nint(Out(l,i,j,k)-0.49))
		maxv(l) = max(maxv(l),nint(Out(l,i,j,k)+0.49))
	      enddo
	    enddo
	  enddo
	enddo
c
c  Look for weird twists or wrap arounds.
c
	do j=1,3
	 do i=1,3
	   weird(1) = weird(1).or.
     *	    (Out(1,3,i,j)-Out(1,2,i,j))*(Out(1,2,i,j)-Out(1,1,i,j)).lt.0
	   weird(2) = weird(2).or.
     *	    (Out(2,i,3,j)-Out(2,i,2,j))*(Out(2,i,2,j)-Out(2,i,1,j)).lt.0
	   weird(3) = weird(3).or.
     *	    (Out(3,i,j,3)-Out(3,i,j,2))*(Out(3,i,j,2)-Out(3,i,j,1)).lt.0
	 enddo
	enddo
c
c  Set the template ranges. If its weird, just go with the template range.
c
	warned = .false.
	do i=1,3
	  if(weird(i))then
	    minv(i) = min(1,minv(i))
	    maxv(i) = max(nOut(i),maxv(i))
	  endif
	  nOut(i) = maxv(i) - minv(i) + 1
	  if(nout(i).gt.MAXDIM)then
	    nout(i) = MAXDIM
	    if(.not.warned)call bug('w',
     *	      'Output image too large -- being truncated')
	    warned = .true.
	  endif
	  call coGetd(lOut,'crpix'//itoaf(i),crpix)
	  crpix = crpix - minv(i) + 1
	  call coSetd(lOut,'crpix'//itoaf(i),crpix)
	enddo
	call coReinit(lOut)
c
	end
c************************************************************************
	subroutine GridStat(nearest,Xv,Yv,Zv,gnx,gny,n1,n2,n3,
     *	    tol,minv,maxv,order)
c
	implicit none
	logical nearest
	integer gnx,gny,n1,n2,n3,minv(3),maxv(3),order(3)
	real Xv(gnx,gny),Yv(gnx,gny),Zv(gnx,gny),tol
c------------------------------------------------------------------------
	integer i,j,n(3)
	real minr(3),maxr(3),diff
c
	minr(1) = Xv(1,1)
	maxr(1) = minr(1)
	minr(2) = Yv(1,1)
	maxr(2) = minr(2)
	minr(3) = Zv(1,1)
	maxr(3) = minr(3)
	do j=1,gny
	  do i=1,gnx
	    minr(1) = min(Xv(i,j),minr(1))
	    maxr(1) = max(Xv(i,j),maxr(1))
	    minr(2) = min(Yv(i,j),minr(2))
	    maxr(2) = max(Yv(i,j),maxr(2))
	    minr(3) = min(Zv(i,j),minr(3))
	    maxr(3) = max(Zv(i,j),maxr(3))
	  enddo
	enddo
c
c  Determine which are going to be done by nearest neighbour, and
c  what are the min and max limits needed for interpolation.
c
	n(1) = n1
	n(2) = n2
	n(3) = n3
	do i=1,3
	  minr(i) = max(minr(i),1.0)
	  maxr(i) = min(maxr(i),real(n(i)))
	  diff = max(maxr(i)-minr(i),abs(nint(minr(i))-minr(i)))
	  if(n(i).le.5.or.diff.lt.tol.or.nearest)then
	    order(i) = 0
	    minv(i) = max(1,   nint(minr(i)))
	    maxv(i) = min(n(i),nint(maxr(i)))
	  else
	    order(i) = 3
	    minv(i) = max(1,   nint(minr(i)-1.5))
	    maxv(i) = min(n(i),nint(maxr(i)+1.5))
	  endif
	enddo
c
	end
c************************************************************************
	subroutine MkHeader(lIn,lOut,cOut,version)
c
	implicit none
	integer lIn,lOut,cOut
	character version*(*)
c
c------------------------------------------------------------------------
	integer i
	character line*64
c
	integer nkeys
	parameter(nkeys=21)
	character key(nkeys)*8
        data key /   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'history ','instrume','niters  ',
     *    'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *    'telescop','btype   ','rms     ','pbtype  ',
     *    'ltype   ','lstart  ','lwidth  ','lstep   ','mostable'/
c
	do i=1,nkeys
	  call hdcopy(lIn,lOut,key(i))
	enddo
c
	call coWrite(cOut,lOut)
	call hisOpen(lOut,'append')
	line = 'REGRID: Miriad '//version
	call hisWrite(lOut,line)
	call hisInput(lOut,'REGRID')
	call hisClose(lOut)
c
	end
c************************************************************************
	subroutine Intp(lOut,order,nx,ny,In,flagIn,nix,niy,niz,
     *		xoff,yoff,zoff,Xv,Yv,Zv,gnx,gny,nblank)
c
	implicit none
	integer nx,ny,nix,niy,niz,gnx,gny,order(3),lOut,xoff,yoff,zoff
	integer nblank
	real In(nix,niy,niz),Xv(gnx,gny),Yv(gnx,gny),Zv(gnx,gny)
	logical flagIn(nix,niy,niz)
c------------------------------------------------------------------------
	include 'maxdim.h'
	real WtTol
	parameter(WtTol=0.5)
c
	integer i,j,jx,jy
	double precision fx,fy,x,y,z
	integer id,jd,kd,imin,imax,jmin,jmax,kmin,kmax
	real Sum,SumWt,Wt,Wtx(4),Wty(4),Wtz(4)
	real Out(MAXDIM)
	logical flagOut(MAXDIM)
c
	do j=1,ny
	  y = dble((gny-1)*(j-1))/dble(ny-1) + 1
	  jy = nint(y - 0.5d0)
	  fy = y - jy
	  do i=1,nx
	    x = dble((gnx-1)*(i-1))/dble(nx-1) + 1
	    jx = nint(x - 0.5d0)
	    fx = x - jx
	    if(fx.eq.0)then
	      if(fy.eq.0)then
		x = Xv(jx,jy)
		y = Yv(jx,jy)
		z = Zv(jx,jy)
	      else
		x = (1-fy) * Xv(jx,jy) + fy * Xv(jx,jy+1)
		y = (1-fy) * Yv(jx,jy) + fy * Yv(jx,jy+1)
		z = (1-fy) * Zv(jx,jy) + fy * Zv(jx,jy+1)
	      endif
	    else
	      if(fy.eq.0)then
	        x = (1-fx) * Xv(jx,jy) + fx * Xv(jx+1,jy)
	        y = (1-fx) * Yv(jx,jy) + fx * Yv(jx+1,jy)
	        z = (1-fx) * Zv(jx,jy) + fx * Zv(jx+1,jy)
	      else
	        x = (1-fy)*((1-fx)*Xv(jx,jy)  +fx*Xv(jx+1,jy)  )
     *	             + fy *((1-fx)*Xv(jx,jy+1)+fx*Xv(jx+1,jy+1))
	        y = (1-fy)*((1-fx)*Yv(jx,jy)  +fx*Yv(jx+1,jy)  )
     *	             + fy *((1-fx)*Yv(jx,jy+1)+fx*Yv(jx+1,jy+1))
	        z = (1-fy)*((1-fx)*Zv(jx,jy)  +fx*Zv(jx+1,jy)  )
     *	             + fy *((1-fx)*Zv(jx,jy+1)+fx*Zv(jx+1,jy+1))
	      endif
	    endif
c
c  We now have the coordinates of the point that we want in the output
c  in terms of the input coordinate system. Interpolate this point.
c
	    call coeff(order(1),x-xoff,nix,imin,imax,Wtx)
	    call coeff(order(2),y-yoff,niy,jmin,jmax,Wty)
	    call coeff(order(3),z-zoff,niz,kmin,kmax,Wtz)
c
	    Sum = 0
	    SumWt = 0
	    do kd=kmin,kmax
	      do jd=jmin,jmax
		do id=imin,imax
		  if(flagIn(id,jd,kd))then
		    Wt = Wtx(id-imin+1)*Wty(jd-jmin+1)*Wtz(kd-kmin+1)
		    Sum = Sum + Wt * In(id,jd,kd)
		    SumWt = SumWt + Wt
		  endif
		enddo
	      enddo
	    enddo
c
c  Determine whether the output is good or not.
c
	    flagOut(i) = SumWt.gt.WtTol
	    if(flagOut(i))then
	      Out(i) = Sum / SumWt
	    else
	      nblank = nblank + 1
	      Out(i) = 0
	    endif
	  enddo
	  call xywrite(lOut,j,Out)
	  call xyflgwr(lOut,j,flagOut)
	enddo
c
	end
c************************************************************************
	subroutine coeff(order,x,nix,imin,imax,Wtx)
c
	implicit none
	double precision x
	integer nix,imin,imax,order
	real Wtx(4)
c------------------------------------------------------------------------
	real WtTol
	parameter(WtTol=0.5)
	double precision fx,SumWt
	integer jx,off,i
	logical clip
c
	if(order.eq.0)then
	  imin = nint(x)
	  imax = imin
	  Wtx(1) = 1
	else if(order.eq.3)then
	  jx = nint(x - 0.5d0)
	  imin = jx-1
	  imax = jx+2
c
	  fx = x - jx
	  Wtx(1) = ((-0.5*fx+1.0)*fx-0.5)*fx
	  Wtx(2) = (( 1.5*fx-2.5)*fx    )*fx + 1.0
	  Wtx(3) = ((-1.5*fx+2.0)*fx+0.5)*fx
	  Wtx(4) = (( 0.5*fx-0.5)*fx    )*fx
	else
	  call bug('f','Unsupported order, in Coeff')
	endif
c
c  Clip back anything that went outside the range of the valid pixels.
c
	clip = .false.
	if(imax.gt.nix)then
	  clip = .true.
	  imax = nix
	endif
	if(imin.lt.1)then
	  clip = .true.
	  off = 1 - imin
	  imin = 1
	  do i=1,imax-imin+1
	    Wtx(i) = Wtx(i+off)
	  enddo
	endif
c
c  If the range had to be clipped back, check that there is still enough
c  weighting to bother to interpolate.
c
	if(clip)then
	  SumWt = 0
	  do i=1,imax-imin+1
	    SumWt = SumWt + Wtx(i)
	  enddo
	  if(SumWt.lt.WtTol)then
	    imin = nix + 1
	    imax = 0
	  endif
	endif
c
	end
c************************************************************************
	subroutine GridGen(nx,ny,plane,Xv,Yv,Zv,gnx,gny)
c
	implicit none
	integer nx,ny,plane,gnx,gny
	real Xv(gnx,gny),Yv(gnx,gny),Zv(gnx,gny)
c
c  Determine the translation between the output and input pixel coordinates
c  on a grid.
c
c  Input:
c    nx,ny
c    plane
c    gnx,gny
c  Output:
c    Xv,Yv,Zv
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	integer i,j
	double precision In(3),Out(3)
c
	In(3) = plane
c
	do j=1,gny
	  In(2) = dble(ny-1)/dble(gny-1) * (j-1) + 1
	  do i=1,gnx
	    In(1) = dble(nx-1)/dble(gnx-1) * (i-1) + 1
	    call pCvt(In,Out,3)
	    Xv(i,j) = Out(1)
	    Yv(i,j) = Out(2)
	    Zv(i,j) = Out(3)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GridEst(nx,ny,plane,gnx,gny,tol)
c
	implicit none
	integer nx,ny,plane,gnx,gny
	real tol
c
c  Compute the grid size needed to allow translation between the
c  different pixel coordinate systems.
c
c  Input:
c    nx,ny	Image size.
c    plane	Plane of interest.
c    tol	Pixel tolerance.
c  Output:
c    gnx,gny	Translation grid size.
c------------------------------------------------------------------------
	double precision x(3,4),tx(3,4),mid(3),tmid(3)
	double precision xmid(3,4),txmid(3,4)
	integer n,i,j,k,kmax,k1,k2,k3
	real err,errmax
	logical more
c
	n = 1
	k = 0
	do j=1,2
	  do i=1,2
	    k = k + 1
	    x(1,k) = (2-i) + (i-1)*nx
	    x(2,k) = (2-j) + (j-1)*ny
	    x(3,k) = plane
	    call pcvt(x(1,k),tx(1,k),3)
	  enddo
	enddo
	mid(1) = 0.5*(nx+1)
	mid(2) = 0.5*(ny+1)
	mid(3) = plane
	call pcvt(mid,tmid,3)
c
	more = .true.
	dowhile(more)
	  n = 2*n
	  errmax = -1
	  do k=1,4
	    xmid(1,k) = 0.5*(x(1,k) + mid(1))
	    xmid(2,k) = 0.5*(x(2,k) + mid(2))
	    xmid(3,k) = 0.5*(x(3,k) + mid(3))
	    call pcvt(xmid(1,k),txmid(1,k),3)
	    err = max( abs(0.5*(tx(1,k)+tmid(1)) - txmid(1,k)),
     *		       abs(0.5*(tx(2,k)+tmid(2)) - txmid(2,k)),
     *		       abs(0.5*(tx(3,k)+tmid(3)) - txmid(3,k)))
	    if(err.gt.errmax)then
	      kmax = k
	      errmax = err
	    endif
	  enddo
	  more = errmax.gt.tol.and.max(nx,ny).gt.n+1
c
c  If the tolerance has not yet been reached, home in on the
c  region where the fit was worst.
c
	  if(more)then
	    k1 = 1
	    if(k1.eq.kmax)k1 = k1 + 1
	    k2 = k1 + 1
	    if(k2.eq.kmax)k2 = k2 + 1
	    k3 = k2 + 1
	    if(k3.eq.kmax)k3 = k3 + 1
	    call TripMv(mid(1),   mid(2), mid(3),      x(1,k1))
	    call TripMv(tmid(1),  tmid(2),tmid(3),     tx(1,k1))
	    call TripMv(x(1,kmax),mid(2), dble(plane), x(1,k2))
	    call TripMv(mid(1),x(2,kmax), dble(plane), x(1,k3))
	    call pcvt(x(1,k2),tx(1,k2),3)
	    call pcvt(x(1,k3),tx(1,k3),3)
	    call TripMv(xmid(1,kmax), xmid(2,kmax), xmid(3,kmax),mid)
	    call TripMv(txmid(1,kmax),txmid(2,kmax),txmid(3,kmax),tmid)
	  endif
	enddo
c
	gnx = min(nx,n+1)
	gny = min(ny,n+1)
c
	end
c************************************************************************
	subroutine TripMv(a,b,c,x)
c
	implicit none
	double precision a,b,c,x(3)
	x(1) = a
	x(2) = b
	x(3) = c
	end
c************************************************************************
	subroutine BufIni(nBuf,off,minc,maxc,BufSize)
c
	implicit none
	integer nBuf(3),off(3),minc(3),maxc(3),BufSize
c------------------------------------------------------------------------
	integer i
c
	do i=1,3
	  nBuf(i) = 0
	  off(i) = 0
	  minc(i) = 0
	  maxc(i) = 0
	enddo
c
	BufSize = 0
c
	end
c************************************************************************
	subroutine BufGet(lIn,minr,maxr,n,nBuf,off,minc,maxc,
     *						rBuf,lBuf,BufSize)
c
	implicit none
	integer lIn
	integer minr(3),maxr(3),n(3)
	integer nBuf(3),off(3),minc(3),maxc(3)
	integer rBuf,lBuf,BufSize
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer i
	logical redo
c
c  Externals.
c
	integer memBuf
c
	redo = .false.
	do i=1,2
	  redo = redo.or.minr(i).lt.minc(i)
     *		     .or.maxr(i).gt.maxc(i)
	enddo
	redo = redo.or.(maxr(3)-minr(3)+1).gt.nBuf(3).or.
     *		maxr(3).lt.minc(3).or.minr(3).gt.maxc(3)
c
c  If it looks as if we cannot use the previous buffers, recalculate
c  what currently looks best, and load the needed data.
c
	if(redo)then
	  nBuf(1) = n(1)
	  nBuf(2) = min(nint(1.2*(maxr(2)-minr(2)))+1,n(2))
	  nBuf(3) = min(max(max(memBuf()/2,BufSize)/(nBuf(1)*nBuf(2)),
     *		            maxr(3)-minr(3)+1),n(3))
	  if(nBuf(1)*nBuf(2)*nBuf(3).gt.BufSize)then
	    if(BufSize.gt.0)then
	      call memFree(rBuf,BufSize,'r')
	      call memFree(lBuf,BufSize,'l')
	    endif
	    BufSize = nBuf(1)*nBuf(2)*nBuf(3)
	    call memAlloc(rBuf,BufSize,'r')
	    call memAlloc(lBuf,BufSize,'l')
	  endif
	  do i=1,3
	    off(i) = minr(i) - (nBuf(i) - (maxr(i)-minr(i)+1))/2 - 1
	    off(i) = max(0,min(off(i),n(i)-nBuf(i)))
	    if(minr(i).lt.off(i)+1.or.maxr(i).gt.off(i)+nBuf(i))
     *	      call bug('f','Algorithmic failure in BufGet')	    
	    if(i.ne.3)then
	      minc(i) = 1 + off(i)
	      maxc(i) = nBuf(i) + off(i)
	    else
	      minc(3) = minr(i)
	      maxc(3) = maxr(i)
	    endif
	  enddo
	  call BufLoad(lIn,minc,maxc,memr(rBuf),meml(lBuf),
     *	    nBuf(1),nBuf(2),nBuf(3),off(1),off(2),off(3))
c
c  Handle the case of there being useful data already in the buffers,
c  and that the buffers are OK in size.
c
	else
	  call BufCycle(lIn,minc,maxc,minr,maxr,memr(rBuf),meml(lBuf),
     *	    nBuf(1),nBuf(2),nBuf(3),off(1),off(2),off(3))
	endif
	end
c************************************************************************
	subroutine BufCycle(lIn,minc,maxc,minr,maxr,Dat,Flags,
     *				nx,ny,nz,xoff,yoff,zoff)
c
	implicit none
	integer lIn,minc(3),maxc(3),minr(3),maxr(3)
	integer nx,ny,nz,xoff,yoff,zoff
	real Dat(nx,ny,nz)
	logical Flags(nx,ny,nz)
c------------------------------------------------------------------------
	integer j,k,zoff1
c
c  Shuffle around planes that we already have in memory.
c
	if(minr(3).lt.1+zoff)then
	  zoff1 = max(0,maxr(3) - nz)
	  minc(3) = max(minc(3),minr(3))
	  maxc(3) = min(maxc(3),maxr(3))
	  do k=maxc(3),minc(3),-1
	    call scopy(nx*ny,Dat(1,1,k-zoff),1,Dat(1,1,k-zoff1),1)
	    call logcopy(nx*ny,Flags(1,1,k-zoff),Flags(1,1,k-zoff1))
	  enddo
	  zoff = zoff1
	else if(maxr(3).gt.nz+zoff)then
	  zoff1 = minr(3) - 1
	  minc(3) = max(minc(3),minr(3))
	  maxc(3) = min(maxc(3),maxr(3))
	  do k=minc(3),maxc(3)
	    call scopy(nx*ny,Dat(1,1,k-zoff),1,Dat(1,1,k-zoff1),1)
	    call logcopy(nx*ny,Flags(1,1,k-zoff),Flags(1,1,k-zoff1))
	  enddo
	  zoff = zoff1
	endif
c
	if(xoff.ne.0)call bug('f','Cycle assertion failure')
c
c  Read in the extra planes.
c
	do k=minr(3),minc(3)-1
	  call xysetpl(lIn,1,k)
	  do j=minc(2),maxc(2)
	    call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
	    call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
	  enddo
	enddo
	do k=maxc(3)+1,maxr(3)
	  call xysetpl(lIn,1,k)
	  do j=minc(2),maxc(2)
	    call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
	    call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
	  enddo
	enddo
c
	minc(3) = min(minr(3),minc(3))
	maxc(3) = max(maxr(3),maxc(3))
c
	end
c************************************************************************
	subroutine logcopy(n,In,Out)
c
	implicit none
	integer n
	logical In(n),Out(n)
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  Out(i) = In(i)
	enddo
c
	end
c************************************************************************
	subroutine BufLoad(lIn,minc,maxc,Dat,Flags,
     *				nx,ny,nz,xoff,yoff,zoff)
c
	implicit none
	integer lIn,minc(3),maxc(3),nx,ny,nz,xoff,yoff,zoff
	real Dat(nx,ny,nz)
	logical Flags(nx,ny,nz)
c
c  Fill buffers up with the appropriate data.
c
c------------------------------------------------------------------------
	integer j,k
c
	if(xoff.ne.0)call bug('f','Load assertion failure')
c
	do k=minc(3),maxc(3)
	  call xysetpl(lIn,1,k)
	  do j=minc(2),maxc(2)
	    call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
	    call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine BadPlane(lOut,n1,n2)
c
	implicit none
	integer n1,n2,lOut
c
c  Blank out a completely bad plane.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	real data(MAXDIM)
	logical flag(MAXDIM)
	integer i,j
c
	do i=1,n1
	  data(i) = 0
	  flag(i) = .false.
	enddo
c
	do j=1,n2
	  call xywrite(lOut,j,Data)
	  call xyflgwr(lOut,j,Flag)
	enddo
c
	end
