c************************************************************************
	program linmos
	implicit none
c
c  LINMOS is a simple linear mosaicing task. It takes cubes as its
c  input, and produces a single cube as its output. The correction weights
c  used are optimum in a minimum mean square sense.
c
c= linmos - Linear mosaicing of datacubes
c& rjs
c: map combination
c+
c	LINMOS is a MIRIAD task which performs a simple linear mosaicing of
c	input cubes, to produce a single output cube. If only a single
c	input cube is given, LINMOS essentially does primary beam correction
c	on this input. When several, overlapping, inputs are given, then
c	LINMOS combines the overlapping regions in such a way as to minimize
c	the rms error in the output.
c
c	To determine the primary beam of the telescope, LINMOS first checks
c	the map header for the presence of the "pbtype" and then "pbfwhm"
c	parameters. If present,	LINMOS assumes the primary beam is the
c	given type. If these parameters are missing, LINMOS checks if the
c	telescope is one that it knows. If so, then the known form for the
c	primary beam is used. See task "pbplot" to check LINMOS's primary
c	beam models.
c@ in
c	This gives the names of the input cubes. Many cubes can be given.
c	There is no default. Inputs should generally be on the same grid
c	system. However, if they are not, linear
c	interpolation is performed to regrid using the first image as the
c	template. LINMOS's ability to do this is quite inferior to task REGRID.
c	The intensity units of all the inputs, and the pixel size 
c	and alignment of the third dimension are assumed to be the same.
c@ out
c	The name of the output cube. No default. The center and pixel size
c	of the first input image is used as the grid system of the output.
c@ rms
c	The rms noise levels in the input cubes. The default is determined
c	from the input images. If this is not possible for an image,
c	then the rms of the previous image is used. If no value could be
c	determined for the first image, all images are given equal weight.
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match is supported. Possible values are:
c	  taper        By default, LINMOS fully corrects for primary
c	               beam attenutation. This can cause excessive noise
c	               amplification at the edge of the mosaiced field.
c	               The `taper' option aims at achieving approximately
c	               uniform noise across the image. This prevents full
c	               primary beam correction at the edge of the mosaic.
c	               See equation 2 in Sault, Staveley-Smith and Brouw, A&AS,
c	               120, 376 (1996) or use "options=gains" to see the form
c	               of the tapering.
c	  sensitivity  Rather than a mosaiced image, produce an image
c	               giving the rms noise across the field.
c	  gain         Rather than a mosaiced image, produce an image
c	               giving the effective gain across the field. If
c	               options=taper is used, this will be a smooth function.
c	               Otherwise it will be 1 or 0 (blanked).
c--
c
c  History:
c    rjs  27oct89 Original version.
c    rjs   6nov89 Interpolation, better i/o, more error checks.
c    rjs   8feb90 The routine to determine default primary beam size did the
c		  calculation wrong!
c    rjs  18feb90 Added offseting of x0,y0. Corrected determination of
c		  dra,ddec. Corrected bug in GetPB
c    rjs  14apr90 Fixed bug in the interpolation code.
c    rjs  24apr90 Made GetPB into a separate file.
c    rjs  26apr90 Copied linetype keywords to the output.
c    mchw 09nov90 Get pbfwhm from map header and set pbfwhm=0 in output map.
c    rjs   5nov91 Eliminated maxdim**2 arrays, and standardised history.
c    rjs  20nov91 Replace keya('in'...) with keyf('in'...).
c    mchw 19mar92 Check third axis alignment.
c    rjs  20mar92 Added "save" statement to above code.
c    rjs  12nov92 Significant mods, to use nebk's primary beam routines,
c		  and to partially handle blanked images.
c    nebk 25nov92 Copy btype to output
c    rjs  10dec92 New CALL sequence for GETFREQ and handle zero frequency
c    nebk 28jan93 new P.B. interface
c    rjs   4oct93 Increase number of cubes that can be handled.
c    rjs  26oct93 Fixed geometry problem.
c    rjs   6dec93 Improve some messages.
c    mhw  13jan94 Relax alignment requirement on the third axis.
c    rjs  22jul94 Added options=sensivitivy and options=gain. Use some
c		  standard include files.
c    rjs  26jul94 Doc changes only.
c    rjs  17aug94 Change projection code to cartesian.
c    rjs  24oct94 Use new pb routines.
c    rjs  30nov94 Preserve the projection geometry when all the inputs
c		  have the same geometry. Handle single-pointing
c		  mosaic tables.
c    rjs   3dec94 More lenient "exactness" algorithm in thingchk.
c    rjs  30jan95 Taper option. Eliminate "signal" parameter.
c    mhw  13aug95 Clip input if output too big, instead of giving up.
c    rjs  14aug95 Fix to the above.
c    rjs  02jul97 cellscal change.
c    rjs  23jul97 pbtype change.
c    rjs  04aug97 Doc change only.
c    rjs  25aug97 Doc change and an extra error message.
c
c  Bugs:
c    * Blanked images are not handled when interpolation is necessary.
c    * Alignment of images with frequency is not correct.
c    * The handling of geometry issues is abysmal.
c
c  Program parameters:
c    maxIn	Maximum number of input files.
c    maxOpen	Maximum number of input files to leave open.
c    maxlen	Size of buffer to hold all the input file names.
c    tol	Tolerance. When the grid locations of two pixels differ
c		by less than "tol", they are taken as being the same
c		pixel (i.e. no interpolation done).
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Linmos: version 1.0 25-Aug-97')
	include 'maxdim.h'
c
	real tol
	integer maxIn,maxLen,maxOpen
	parameter(maxIn=350,maxLen=17000,maxOpen=6,tol=0.01)
c
	character inbuf*(maxlen),in*64,out*64
	integer tScr,tWts,tIn(maxIn),tOut,k1(maxIn),k2(maxIn),nIn
	integer offset,length
	integer nsize(3,maxIn),nOut(4)
	integer i,nOpen,naxis,itemp
	real xoff,yoff,Sigt
	real rms(maxIn),BlcTrc(4,maxIn)
	real Extent(4)
	double precision crval(3),cdelt(3),crpix(3)
	character ctype(3)*16
	logical defrms,init,dosen,dogain,docar,exact,taper
c
	integer pOut,pWts
	include 'mem.h'
c
c  Externals.
c
	integer len1
c
c  Get the input parameters, and do some checking.
c
	call output(version)
	call keyini
	nIn = 0
	offset = 0
	call keyf('in',in,' ')
	dowhile(in.ne.' ')
	  nIn = nIn + 1
	  if(nIn.gt.maxIn) call bug('f','Too many input cubes')
	  length = len1(in)
	  if(offset+length.gt.maxLen)
     *	    call bug('f','Input name buffer overflow')
	  k1(nIn) = offset + 1
	  k2(nIn) = offset + length
	  Inbuf(k1(nIn):k2(nIn)) = In
	  offset = offset + length
	  call keyf('in',in,' ')
	enddo
	if(nIn.eq.0)	   call bug('f','No input cubes given')
	call keya('out',out,' ')
	if(out.eq.' ') call bug('f','No output name given')
c
	do i=1,nIn
	  call keyr('rms',rms(i),0.)
	  if(rms(i).lt.0)
     *	      call bug('f','Non-positive rms noise parameter.')
	enddo
c
c  Get processing options.
c
	call GetOpt(dosen,dogain,taper)
	call keyfin
c
	if(nIn.eq.1.and.taper)call bug('f',
     *	  'options=taper reduces to no correction for single pointings')
c
c  Open the files, determine the size of the output. Determine the grid
c  system from the first map.
c
	if(nIn.le.maxOpen)then
	  nOpen = nIn
	else
	  nOpen = maxOpen - 1
	endif
c
	docar = .false.
	do i=1,nIn
	  call xyopen(tin(i),InBuf(k1(i):k2(i)),'old',3,nsize(1,i))
	  if(max(nsize(1,i),nsize(2,i)).gt.maxdim)
     *	    call bug('f','Input map is too big')

	  if(i.eq.1)then
	    call ThingIni(tIn(i),nsize(1,i),ctype,crpix,crval,cdelt,
     *					blctrc(1,i),extent)
	    call rdhdi(tin(i),'naxis',naxis,3)
	    naxis = min(naxis,4)
	    if(rms(i).eq.0)call rdhdr(tIn(i),'rms',rms(i),0.0)
	    defrms = rms(i).le.0
	    if(defrms)rms(i) = 1
	  else
	    call ThingChk(tIn(i),nsize(1,i),ctype,crpix,crval,cdelt,
     *				  exact,blctrc(1,i),extent)
	    docar = docar.or..not.exact
	    if(nsize(3,i).ne.nsize(3,1))
     *	      call bug('f','Different lengths for 3rd axis')
	    if(defrms)then
	      rms(i) = 1
	    else
	      if(rms(i).le.0)call rdhdr(tIn(i),'rms',rms(i),0.0)
	      if(rms(i).le.0)rms(i) = rms(i-1)
	    endif
	  endif
	  if(i.gt.nOpen) call xyclose(tIn(i))
	enddo
c
c  Create the output image, and make a header for it.
c
	do i=1,4
	  itemp = nint(extent(i))
	  if(abs(extent(i)-itemp).gt.tol)then
	    if(i.ge.3) itemp = nint(extent(i)+0.49)
	    if(i.lt.3) itemp = nint(extent(i)-0.49)
	  endif
	  extent(i) = itemp
	enddo
c
	nOut(1) = nint(extent(3) - extent(1)) + 1
	nOut(2) = nint(extent(4) - extent(2)) + 1
	if(max(nOut(1),nOut(2)).gt.maxdim) then
     	  call bug('w','Output image is too large, clipping input')
          if (nOut(1).gt.maxdim) then
             extent(1)=extent(1)+(nOut(1)-maxdim+1)/2
             extent(3)=extent(3)-(nOut(1)-maxdim+1)/2
	     nOut(1) = nint(extent(3) - extent(1)) + 1
	  endif
          if (nOut(2).gt.maxdim) then
             extent(2)=extent(2)+(nOut(2)-maxdim+1)/2
             extent(4)=extent(4)-(nOut(2)-maxdim+1)/2
	     nOut(2) = nint(extent(4) - extent(2)) + 1
	  endif
	endif
	nOut(3) = nsize(3,1)
	if(dosen.or.dogain)nOut(3) = 1
	nOut(4) = 1
c
	call xyopen(tout,out,'new',naxis,nout)
	call hdout(tIn(1),tout,nsize(1,1),extent,version,
     *					dosen,dogain,docar)
	call coInit(tout)
c
c  Correct the blctrc parameter for the extent of the image.
c
	xoff = extent(1) - 1
	yoff = extent(2) - 1
	do i=1,nIn
	  BlcTrc(1,i) = BlcTrc(1,i) - xoff
	  BlcTrc(2,i) = BlcTrc(2,i) - yoff
	  BlcTrc(3,i) = BlcTrc(3,i) - xoff
	  BlcTrc(4,i) = BlcTrc(4,i) - yoff
	enddo
c
c  Allocate memory.
c
	call MemAlloc(pOut,nOut(1)*nOut(2),'r')
	call MemAlloc(pWts,nOut(1)*nOut(2),'r')
c
c  Process each of the files.
c
	call ScrOpen(tScr)
	call ScrOpen(tWts)
	init = .false.
	do i=1,nIn
          call output ('Processing image '//inbuf(k1(i):k2(i)))
	  if(i.gt.nOpen) call xyopen(tIn(i),InBuf(k1(i):k2(i)),
     *						'old',3,nsize(1,i))
	  call Process(init,tScr,tWts,tIn(i),tout,memR(pOut),memR(pWts),
     *	    nsize(1,i),nsize(2,i),nOut(1),nOut(2),nOut(3),
     *      dosen.or.dogain,BlcTrc(1,i),rms(i))
	  call xyclose(tin(i))
	enddo
	if(.not.init) call bug('f','Something is screwy')
c
c  Determine the maximum noise to aim at.
c
	if(taper)then
	  Sigt = Rms(1)
	  do i=2,nIn
	    Sigt = max(Sigt,Rms(i))
	  enddo
	  Sigt = Sigt*Sigt
	else
	  Sigt = 0
	endif
c
c  Go through the scratch file one more time, correcting for the change
c  in the weights, and writting out the final data.
c
	call LastPass(tOut,tScr,tWts,memR(pOut),memR(pWts),Sigt,
     *				nOut(1),nOut(2),nOut(3),dosen)
c
c  Free up the memory.
c
	call MemFree(pOut,nOut(1)*nOut(2),'r')
	call MemFree(pWts,nOut(1)*nOut(2),'r')
c
c  All said and done. Close up anything that is still open.
c
	call ScrClose(tScr)
	call ScrClose(tWts)
	call xyclose(tOut)
	end
c************************************************************************
	subroutine GetOpt(dosen,dogain,taper)
c
	implicit none
	logical dosen,dogain,taper
c
c  Get extra processing options.
c
c  Output:
c    dosen	True if we are to produce a sensitivity image.
c    dogain	True if we are to produce an image of the effective gain.
c    taper	True if the output is to be tapered to achieve quasi-uniform
c		noise across the image.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	logical present(NOPTS)
	character opts(NOPTS)*11
	data opts/'sensitivity','gain       ','taper      '/
c
	call options('options',opts,present,NOPTS)
c
	dosen  = present(1)
	dogain = present(2)
	taper  = present(3)
c
	if(dosen.and.dogain)call bug('f',
     *	  'Cannot do options=sensitivity,gains simultaneously')
	end
c************************************************************************
	subroutine Process(init,tScr,tWts,tIn,tOut,Out,Wts,
     *	  nx,ny,n1,n2,n3,dosen,BlcTrc,rms)
c
	implicit none
	logical init
	integer tScr,tWts,tIn,tOut
	integer nx,ny,n1,n2,n3
	real Out(n1,n2),Wts(n1,n2)
	real BlcTrc(4),rms
	logical dosen
c
c  First determine the initial weight to apply to each pixel, and accumulate
c  info so that we can determine the normalisation factor later on.
c  Then successively read each plane, apply the weight, and accumulate
c  the information in the scratch file.
c
c  Inputs:
c    tScr	Handle of the image scratch file.
c    tWts	Handle of the weights scratch file.
c    tIn	Handle of the input file.
c    tOut	Coordinate system of the output dataset.
c    nx,ny	Size of the image cube.
c    n1,n2,n3	Size of the output cube.
c    BlcTrc	Grid corrdinates, in the output, that the input maps to.
c    rms	Rms noise parameter.
c    dosen	True if we are to compute the sensitivity or gain
c		function rather than the normal mosaic.
c  Input/Output:
c    init	True if things have been initialised.
c  Scratch:
c    In		Used for the interpolated version of the input.
c    Out	Used for the output.
c    Wts	Accumulation of the weights array.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real tol
	parameter(tol=0.01)
	real xinc,yinc,Sect(4),oneonsig
	integer i,j,k,xlo,xhi,ylo,yhi,xoff,yoff,pbObj
	double precision x(3)
	logical interp,mask
	character pbtype*16
	real Pb(MAXDIM),In(MAXDIM)
c
c  Externals.
c
	logical hdprsnt
c
c  Some useless constant.
c
	oneonsig = 1/(rms*rms)
c
c  Determine if we have to interpolate.
c
	interp = .false.
	do i=1,4
	  interp = interp.or.abs(BlcTrc(i)-anint(BlcTrc(i))).gt.Tol
	enddo
	interp = interp.or.nint(BlcTrc(3)-BlcTrc(1)+1).ne.nx
	interp = interp.or.nint(BlcTrc(4)-BlcTrc(2)+1).ne.ny
c
c  Determine the width of the guard band, if we have to interpolate.
c
	xinc = 0
	yinc = 0
	if(interp)then
	  xinc = 2*(BlcTrc(3)-BlcTrc(1))/(nx-1)
	  yinc = 2*(BlcTrc(4)-BlcTrc(2))/(ny-1)
	endif
	Sect(1) = max(1., BlcTrc(1) + xinc)
	Sect(2) = max(1., BlcTrc(2) + yinc)
	Sect(3) = min(real(n1),BlcTrc(3) - xinc)
	Sect(4) = min(real(n2),BlcTrc(4) - yinc)
c
c  We have determined the section of the output we can calculate. Round this
c  to integer values.
c
	xlo = nint( Sect(1) + 0.5 - tol )
	ylo = nint( Sect(2) + 0.5 - tol )
	xhi = nint( Sect(3) - 0.5 + tol )
	yhi = nint( Sect(4) - 0.5 + tol )
	if(xlo.gt.xhi.or.ylo.gt.yhi) return
c
c  If we are interpolating, initialise the interpolation routine.
c
	if(interp)then
	  Sect(1) = (nx-1)/(BlcTrc(3)-BlcTrc(1))*(xlo-BlcTrc(1)) + 1
	  Sect(2) = (ny-1)/(BlcTrc(4)-BlcTrc(2))*(ylo-BlcTrc(2)) + 1
	  Sect(3) = (nx-1)/(BlcTrc(3)-BlcTrc(1))*(xhi-BlcTrc(1)) + 1
	  Sect(4) = (ny-1)/(BlcTrc(4)-BlcTrc(2))*(yhi-BlcTrc(2)) + 1
	  call IntpIni(xhi-xlo+1,yhi-ylo+1,Sect)
	endif
c
c  Is there a mask file associated with this image?
c
	mask = hdprsnt(tIn,'mask')
	if(mask.and.interp)call bug('f',
     *	  'Blanked pixels cannot be used when interpolating')
c
c  Ready to construct the primary beam object.
c
	call pntcent(tIn,pbtype,x(1),x(2))
c
c  Loop over all planes.
c
	do k=1,n3
	  x(3) = k
	  call pbInitc(pbObj,pbtype,tOut,'aw/aw/ap',x)
	  call xysetpl(tIn,1,k)
	  if(interp)call IntpRIni
c
c  Get a plane from the scratch array.
c
	  if(init)then
	    call GetSec(tScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)
	    call GetSec(tWts,Wts,k,n1,n2,xlo,xhi,ylo,yhi)
	  else
	    do j=1,n2
	      do i=1,n1
		Wts(i,j) = 0
		Out(i,j) = 0
	      enddo
	    enddo
	  endif
c
c  Determine the offsets to start reading.
c
	  if(interp)then
	    xoff = xlo
	    yoff = 1
	  else
	    xoff = nint(BlcTrc(1))
	    yoff = ylo - nint(BlcTrc(2)) + 1
	  endif
c
c  Determine the weights, and add the contribution of the input plane.
c
	  do j=ylo,yhi
	    call GetDat(tIn,nx,xoff,yoff,xlo,xhi,j,pbObj,
     *						In,Pb,n1,interp,mask)
	    yoff = yoff + 1
c
c  Accumulate the sensitivity function.
c
	    if(dosen)then
	      do i=xlo,xhi
	        Wts(i,j) = Wts(i,j) + Pb(i)*Pb(i)*oneonsig
	        Out(i,j) = Out(i,j) + Pb(i)*Pb(i)*oneonsig
	      enddo
c
c  Accumulate the mosaiced field.
c
	    else
	      do i=xlo,xhi
	        Wts(i,j) = Wts(i,j) + Pb(i)*Pb(i)*oneonsig
	        Out(i,j) = Out(i,j) + In(i)*Pb(i)*oneonsig
	      enddo
	    endif
	  enddo
c
c  Save the output.
c
	  if(init)then
	    call PutSec(tScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)
	    call PutSec(tWts,Wts,k,n1,n2,xlo,xhi,ylo,yhi)
	  else
	    call PutSec(tScr,Out,k,n1,n2,1,n1,1,n2)
	    call PutSec(tWts,Wts,k,n1,n2,1,n1,1,n2)
	  endif
c
c  Release the primary beam object.
c
	  call pbFin(pbObj)
	enddo
c
c  All done.
c
	init = .true.
c
	end
c************************************************************************
	subroutine GetDat(tIn,nx,xoff,yoff,xlo,xhi,j,pbObj,
     *						In,Pb,n1,interp,mask)
c
	implicit none
	integer tIn,xoff,yoff,xlo,xhi,n1,j,pbObj,nx
	logical interp,mask
	real In(n1),Pb(n1)
c
c  Get a row of data (either from xyread, or the interpolation routines).
c
c  Input:
c    tIn
c    xoff
c    yoff
c    xlo,xhi
c    n1
c    interp
c    mask
c    pbObj	Primary beam object.
c  Output:
c    In		Image data.
c    Pb		Primary beam response (zeroed out where the data are
c		blanked).
c------------------------------------------------------------------------
	include 'maxdim.h'
	logical flags(MAXDIM)
	real dat(MAXDIM)
	integer i
c
	external xyread
	real PbGet
c
c  Get the data.
c
	if(interp)then
	  call IntpRd(tIn,yoff,In(xoff),xyread)
	else if(xoff.lt.1.or.xoff+nx-1.gt.n1)then
	  call xyread(tin,yoff,Dat)
	  do i=xlo,xhi
	    In(i) = Dat(i-xoff+1)
	  enddo
	else
	  call xyread(tIn,yoff,In(xoff))
	endif
c
c  Determine the primary beam.
c
	do i=xlo,xhi
	  Pb(i) = PbGet(pbObj,real(i),real(j))
	enddo
c
c  Zero the primary beam where the data are flagged.
c
	if(mask)then
	  call xyflgrd(tIn,yoff,flags)
	  do i=xlo,xhi
	    if(.not.flags(i-xoff+1))Pb(i) = 0
	  enddo
	endif
	end
c************************************************************************
	subroutine GetSec(tScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)
c
	implicit none
	integer tScr,k,n1,n2,xlo,xhi,ylo,yhi
	real Out(n1,n2)
c
c  Write out to the scratch file the region of interest.
c
c  Inputs:
c    tScr	Handle of the scratch file.
c    Out	Array containing the data to write.
c    k		Plane number.
c    n1,n2	Dimensions of the Out array.
c    xlo,ylo	Blc of area to write.
c    xhi,yhi	Trc of area to write.
c------------------------------------------------------------------------
	integer j,offset,length
c
c  If the section of the x dimension that we want to right is pretty
c  well the entire x axis, read the whole lot.
c
	offset = (k-1)*n1*n2 + (ylo-1)*n1 + (xlo-1)
	if(10*(xhi-xlo+1).ge.8*n1)then
	  length = n1*(yhi-ylo-1) + (n1-xlo+1) + xhi
	  call ScrRead(tScr,Out(xlo,ylo),offset,length)
	else
	  length = xhi - xlo + 1
	  do j=ylo,yhi
	    call ScrRead(tScr,Out(xlo,j),offset,length)
	    offset = offset + n1
	  enddo
	endif
	end
c************************************************************************
	subroutine PutSec(tScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)
c
	implicit none
	integer tScr,k,n1,n2,xlo,xhi,ylo,yhi
	real Out(n1,n2)
c
c  Write out to the scratch file the region of interest.
c
c  Inputs:
c    tScr	Handle of the scratch file.
c    Out	Array containing the data to write.
c    k		Plane number.
c    n1,n2	Dimensions of the Out array.
c    xlo,ylo	Blc of area to write.
c    xhi,yhi	Trc of area to write.
c------------------------------------------------------------------------
	integer j,offset,length
c
c  Try and block it into one call if that is possible.
c
	offset = (k-1)*n1*n2 + (ylo-1)*n1 + (xlo-1)
	if(xlo.eq.1.and.xhi.eq.n1)then
	  length = n1*(yhi-ylo-1) + (n1-xlo+1) + xhi
	  call ScrWrite(tScr,Out(xlo,ylo),offset,length)
	else
	  length = xhi - xlo + 1
	  do j=ylo,yhi
	    call ScrWrite(tScr,Out(xlo,j),offset,length)
	    offset = offset + n1
	  enddo
	endif
	end    
c************************************************************************
	subroutine LastPass(tout,tScr,tWts,Out,Wts,Sigt,n1,n2,n3,
     *								dosen)
c
	implicit none
	integer tOut,tScr,tWts,n1,n2,n3
	real Sigt,Out(n1,n2),Wts(n1,n2)
	logical dosen
c
c  Read in the data from the scratch file, multiply by the scale factor,
c  and then write out the data.
c
c  Inputs:
c    tOut	Handle of the output image file.
c    tScr	Handle of the input image file.
c    tWts	Handle of the input weights file.
c    Sigt	Critical noise variance. 
c    n1,n2,n3	Size of the output cube.
c    dosen	Determine the sensitivity function.
c  Scratch:
c    Wts	Array containing the weights to be applied.
c    Out	Used to store a plane of the output image.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k
	logical doflag,doneflag,flags(MAXDIM)
	real scale
c
c  Loop over all planes. Get the image and weight planes.
c
	doflag = .false.
	doneflag = .false.
c
	do k=1,n3
	  call GetSec(tScr,Out,k,n1,n2,1,n1,1,n2)
	  call GetSec(tWts,Wts,k,n1,n2,1,n1,1,n2)
	  call xysetpl(tOut,1,k)
c
c  Calculate and apply the weights.
c
	  do j=1,n2
c
c  Determine the sensitivity function.
c
	    if(dosen)then
	      do i=1,n1
		flags(i) = Wts(i,j).gt.0
		if(.not.flags(i))then
		  Out(i,j) = 0
		  doflag = .true.
		else
		  scale = Sigt*Wts(i,j)
		  if(scale.ge.1.or.scale.le.0)then
		    Out(i,j) = sqrt(Out(i,j))/Wts(i,j)
		  else
		    Out(i,j) = sqrt(scale*Out(i,j))/Wts(i,j)
		  endif
		endif
	      enddo
c
c  Determine the gain function or the mosaic.
c
	    else
	      do i=1,n1
		flags(i) = Wts(i,j).gt.0
		if(.not.flags(i))then
		  Out(i,j) = 0
		  doflag = .true.
		else
		  scale = Sigt*Wts(i,j)
		  if(scale.ge.1.or.scale.le.0)then
		    scale = 1/Wts(i,j)
		  else
		    scale = sqrt(scale)/Wts(i,j)
		  endif
		  Out(i,j) = scale*Out(i,j)
		endif
	      enddo
	    endif
c
c  Write out the flags. We do not write any flagging info
c  until we have found a bad pixel.
c
	    if(doflag.and..not.doneflag)then
	      call CatchUp(tOut,j,n1,n2,k)
	      doneflag = .true.
	    endif
	    if(doflag)call xyflgwr(tOut,j,flags)
c
c  Write out the data.
c
	    call xywrite(tOut,j,Out(1,j))
	  enddo
	enddo
	end
c************************************************************************
	subroutine CatchUp(tOut,j0,n1,n2,n3)
c
	implicit none
	integer tOut,j0,n1,n2,n3
c
c  Write out a batch of "good" flags to the image mask file. This is
c  to catch up on the flags that should have been written earlier.
c
c  Input:
c    tOut
c    j0
c    n1
c    n2
c    n3
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k
	logical flags(MAXDIM)
c
c  Initialise the flags array.
c
	do i=1,n1
	  flags(i) = .true.
	enddo
c
c  Flag as good all planes before the first bad plane.
c
	do k=1,n3-1
	  call xysetpl(tOut,1,k)
	  do j=1,n2
	    call xyflgwr(tOut,j,flags)
	  enddo
	enddo
c
c  Flag as good all rows before the first bad row.
c
	call xysetpl(tOut,1,n3)
	do j=1,j0-1
	  call xyflgwr(tOut,j,flags)
	enddo
c
	end
c************************************************************************
	subroutine hdout(tin,tout,nsize,extent,version,
     *						dosen,dogain,docar)
c
	implicit none
	integer tin,tout,nsize(3)
	real extent(4)
	character version*(*)
	logical dosen,dogain,docar
c
c  Make up the header of the output file.
c
c  Input:
c    tin	The handle of the input file, which is to be used as a template.
c    tout	The handle of the output file.
c    nsize	The size of the input image.
c    extent	The expanded extent of the output.
c    dosen	True if the sensitivity function is being evaluated.
c    dogain	True if the gain function is being evaluated.
c    docar	True if we are to label with RA---CAR, DEC--CAR
c------------------------------------------------------------------------
	real crpix1,crpix2
	character ctype1*16,ctype2*16
	integer i
	character line*80
c
	integer nkeys
	parameter(nkeys=36)
	character keyw(nkeys)*8
	data keyw/   'bunit   ','crval1  ','crval2  ','crval3  ',
     *	  'crval4  ','crval5  ','cdelt1  ','cdelt2  ','cdelt3  ',
     *	  'cdelt4  ','cdelt5  ','crpix3  ','crpix4  ','crpix5  ',
     *	  'ctype3  ','ctype4  ','ctype5  ',
     *	  'obstime ','epoch   ','bmaj    ','bmin    ','bpa     ',
     *	  'niters  ','object  ','telescop','observer',
     *	  'restfreq','vobs    ','obsra   ','obsdec  ','lstart  ',
     *	  'lstep   ','ltype   ','lwidth  ','btype   ','cellscal'/
c
c  Write the output projection as cartesian.
c
	if(docar)then
	  call rdhda(tIn,'ctype1',ctype1,'RA---CAR')
	  call rdhda(tIn,'ctype2',ctype2,'DEC--CAR')
	  if(ctype1(5:5).eq.'-')ctype1(5:8) = '-CAR'
	  if(ctype2(5:5).eq.'-')ctype2(5:8) = '-CAR'
	  call wrhda(tOut,'ctype1',ctype1)
	  call wrhda(tOut,'ctype2',ctype2)
	else
	  call hdcopy(tIn,tOut,'ctype1')
	  call hdcopy(tIn,tOut,'ctype2')
	endif
	
c
c  Determine the location of the reference pixel in the output image.
c

	call rdhdr(tIn,'crpix1',crpix1,real(nsize(1)/2+1))
	call rdhdr(tIn,'crpix2',crpix2,real(nsize(2)/2+1))
	crpix1 = crpix1 - (extent(1)-1)
	crpix2 = crpix2 - (extent(2)-1)
	call wrhdr(tOut,'crpix1',crpix1)
	call wrhdr(tOut,'crpix2',crpix2)
c
c  Set the primary beam size to indicate that it is primary beam corrected.
c
	call wrhda(tOut,'pbtype','SINGLE')
c
c  Copy other parameters.
c
	do i=1,nkeys
	  call hdcopy(tIn,tOut,keyw(i))
	enddo
c
c  Create the output history.
c
	call hdcopy(tin,tout,'history')
	call hisopen(tout,'append')
c
	line = 'LINMOS: Miriad '//version
	call hiswrite(tout,line)
	call hisinput(tout,'LINMOS')
	if(dosen.and.dogain)then
	  call hiswrite(tout,
     *		'LINMOS: First plane is the rms noise function')
	  call hiswrite(tout,
     *		'LINMOS: Second plane is the gain function')
	else if(dosen)then
	  call hiswrite(tout,
     *		'LINMOS: The image is the rms noise function')
	else if(dogain)then
	  call hiswrite(tout,
     *		'LINMOS: The image is the gain function')
	endif
	call hisclose(tout)
	end
************************************************************************
	subroutine ThingIni(tno,nsize,ctype,crpix,crval,cdelt,
     *		blctrc,extent)
c
	implicit none
	integer tno,nsize(2)
	character ctype(3)*(*)
	double precision crpix(3),crval(3),cdelt(3)
	real blctrc(4),extent(4)
c
c------------------------------------------------------------------------
	character num*2
	integer i
c
c  Externals.
c
	character itoaf*2
c
c  Read the axis descriptors.
c
	do i=1,3
	  num = itoaf(i)
	  call rdhda(tno,'ctype'//num,ctype(i),' ')
	  call rdhdd(tno,'crpix'//num,crpix(i),1.d0)
	  call rdhdd(tno,'crval'//num,crval(i),1.d0)
	  call rdhdd(tno,'cdelt'//num,cdelt(i),1.d0)
	enddo
c
	blctrc(1) = 1
	blctrc(2) = 1
	blctrc(3) = nsize(1)
	blctrc(4) = nsize(2)
c
	do i=1,4
	  extent(i) = blctrc(i)
	enddo
c
	end
c************************************************************************
	subroutine ThingChk(tno,nsize,ctype,crpix,crval,cdelt,
     *		exact,blctrc,extent)
c
	implicit none
	integer tno,nsize(3)
	logical exact
	character ctype(3)*(*)
	double precision crpix(3),crval(3),cdelt(3)
	real blctrc(4),extent(4)
c
c------------------------------------------------------------------------
	double precision crvalx(3),crpixx(3),cdeltx(3)
	double precision x1,x1x,y1,y1x,z1,z1x,dx,dy,cosdec,cosdecx
	double precision cdelt1,cdelt1x
	character ctypex(3)*16,num*2
	integer i
c
c  Externals.
c
	character itoaf*2
c
c  Read the axis descriptors.
c
	do i=1,3
	  num = itoaf(i)
	  call rdhda(tno,'ctype'//num,ctypex(i),' ')
	  call rdhdd(tno,'crpix'//num,crpixx(i),1.d0)
	  call rdhdd(tno,'crval'//num,crvalx(i),1.d0)
	  call rdhdd(tno,'cdelt'//num,cdeltx(i),1.d0)
	enddo
c
	cosdec  = cos(crval(2))
	cosdecx = cos(crvalx(2))
c
	cdelt1 = cdelt(1)/cosdec
	cdelt1x = cdeltx(1)/cosdecx
c
	x1  = (1-crpix(1) )*cdelt1   + crval(1)
	x1x = (1-crpixx(1))*cdelt1x  + crvalx(1)
	y1  = (1-crpix(2) )*cdelt(2) + crval(2)
	y1x = (1-crpixx(2))*cdeltx(2)+ crvalx(2)
	dx = cdelt1x/cdelt1
	dy = cdeltx(2)/cdelt(2)
c
	blctrc(1) = (x1x - x1)/cdelt1 + 1
	blctrc(2) = (y1x - y1)/cdelt(2) + 1
	blctrc(3) = blctrc(1) + (nsize(1)-1) * dx
	blctrc(4) = blctrc(2) + (nsize(2)-1) * dy
	if(blctrc(3).lt.blctrc(1).or.blctrc(4).lt.blctrc(2))
     *    call bug('f','Signs of cdelt of the inputs are not identical') 
c
c  Update the extent.
c
	Extent(1) = min(BlcTrc(1),Extent(1))
	Extent(2) = min(BlcTrc(2),Extent(2))
	Extent(3) = max(BlcTrc(3),Extent(3))
	Extent(4) = max(BlcTrc(4),Extent(4))
c
c  Are the axes identical?
c
	exact = .true.
	do i=1,2
	  exact = exact.and.
     *		ctype(i).eq.ctypex(i).and.
     *		abs(cdelt(i)-cdeltx(i)).le.0.01*abs(cdelt(i)).and.
     *		abs(crval(i)-crvalx(i)).lt.0.01*abs(cdelt(i))
	enddo
c
c  Check third axis alignment.
c
	z1  = crval(3)  + (1-crpix(3))* cdelt(3)
	z1x = crvalx(3) + (1-crpixx(3))*cdeltx(3)
	if(max( abs(z1-z1x),
     *	        abs(cdelt(3)-cdeltx(3)) ).gt.0.5*abs(cdelt(3)))
     *	  call bug('w','Third axis of inputs do not align')
c
	end
c************************************************************************
	subroutine pntcent(tno,pbtype,pra,pdec)
c
	implicit none
	integer tno
	double precision pra,pdec
	character pbtype*(*)
c
c  Determine the pointing centre and the primary beam type.
c
c  Inputs:
c    tno	Handle of the input image dataset
c  Output:
c    pra,pdec	Pointing centre RA and DEC, in radians.
c    pbtype	Primary beam type. This will normally just be the
c		name of a telescope (e.g. 'HATCREEK' or 'ATCA'), but it
c		can also be 'GAUS(xxx)', where xxx is a Gaussian primary
c		beam size, with its FWHM given in arcseconds. For example
c		'GAUS(120)' is a Gaussian primary beam with FWHM 120 arcsec.
c------------------------------------------------------------------------
	integer mit,size,iostat
	character string*16
c
c  Externals.
c
	logical hdprsnt
	integer hsize
c
c  Is the mosaic table present?
c
	if(hdprsnt(tno,'mostable'))then
	  call haccess(tno,mit,'mostable','read',iostat)
	  if(iostat.ne.0)call bugno('f',iostat)
	  size = hsize(mit)
	  if(size.ne.56)
     *	    call bug('f','Bad size for mosaic table')
	  call hreadd(mit,pra,16,8,iostat)
	  if(iostat.eq.0)call hreadd(mit,pdec,24,8,iostat)
	  if(iostat.eq.0)call hreadb(mit,string,32,16,iostat)
	  call hdaccess(mit,iostat)
	  if(iostat.ne.0)call bugno('f',iostat)
	  pbtype = string
c
c  Otherwise treat a regular synthesis image.
c
	else
	  call rdhdd(tno,'crval1',pra, 0.d0)
	  call rdhdd(tno,'crval2',pdec,0.d0)
	  call pbRead(tno,pbtype)
	endif
c
	end
