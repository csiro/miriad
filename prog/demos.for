c************************************************************************
	program demos
	implicit none
c
c= demos - Inverse mosaicing operation
c& mchw
c: map manipulation
c+
c	DEMOS (de-mosaic) is a MIRIAD task which takes a primary beam
c	corrected cube, and forms output cubes. The output cubes are formed
c	by applying a primary beam at various pointing centres. Thus this task
c	performs the inverse operation of mosaicing. The input pointing
c	centres and the primary beam size are specified either directly, or
c	by giving a uvdata file containing multiple pointings.
c
c	Because the output of DEMOS are not primary beam corrected, they can
c	be used for comparison with other uncorrected images and uvdata. In
c	particular SELFCAL cannot handle a model which is primary beam
c	corrected, though it can handle a visibility data file containing
c	multiple pointings. Thus you could use DEMOS to break the model into
c	several models which are not primary beam corrected.
c@ map
c	This is the name of image, that is to be de-mosaiced. No default.
c	The input is primary beam corrected image, or single dish image.
c@ vis
c	This is an input uv file, whose pointing centres can act as
c	defaults (templates) for the ``center'' parameter. As this is only
c	used to determine defaults, it makes no sense to specify both ``vis''
c	and ``center''.
c@ center
c	This gives a list of pairs of offset pointing positions, relative
c	to the reference pixel of ``map''. The offsets are measured in
c	arcseconds in the plane of the sky, in the directions of (ra,dec).
c	A pair of values must be given for each pointing, giving the offset
c	in x and y. If this is not given, the pointing centers are determined
c	from the ``vis'' file. Either ``vis'' or ``center'' must be present.
c@ pbtype
c	The demosaicing procedure requires you to apply a primary beam.
c	Normally DEMOS determines the primary beam type from the visibility
c	dataset. To override this, or if you do not give a visibility
c	dataset, you must specify the primary beam type.
c	The primary beam type is normally just the name of a telescope whose
c	primary beam is known (e.g. atca, hatcreek, vla). However to specify
c	a gaussian primary beam of a particular size, use "gaus(xxx)", 
c	where xxx is the primary beam FWHM in arcseconds. For example 
c	"gaus(120)" corresponds to a primary beam with FWHM of 2 arcminute.
c@ out
c	This gives a template name for the output images. The actual output
c	image names are formed by appending a number corresponding to each
c	pointing center to this output name. For example, if out=cygnus,
c	then the output images will be called cygnus1, cygnus2, etc.
c@ imsize
c	This gives two values, being the output image size, in x and y.
c	If no value is given, then the outputs will be one primary beam
c	width in size. If one value is given, then this is used for both
c	x and y. Each output size might be smaller than this, to prevent
c	each output from extending beyond the edges of the input image.
c--
c  History:
c    rjs  25apr90 Original version.
c    rjs  30apr90 Changed it so that the reference pixels of the output
c		  maps is the pointing centre.
c    mchw 09nov90 Added bmaj bmin bpa and pbfwhm to map header.
c    mjs  25feb91 Changed references of itoa to itoaf.
c    pjt  16oct91 Changed to double precision coordinate crval/cdelt/crpix
c    nebk 12nov92 Adapt for new primary beam routines and do blanking.
c    nebk 25nov92 Copy btype to output
c    nebk 17dec92 Adapt to new FNDAXNUM
c    nebk 28jan93 New primary beam interface.
c    mchw 12feb93 Convert uvvariables ra and dec to double precision.
c    rjs  26aug94 Rework to use new co routines.
c    rjs  24oct94 Use new pb routines.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='version 24-Oct-94')
	include 'maxdim.h'
        include 'mirconst.h'
c
	integer maxpnt
	parameter(maxpnt=16)
c
	real pntoff(2,maxpnt)
	character map*64,vis*64,out*64,name*64,pbtype*16
	integer imsize(2),nsize(3),npnt,lout,i,tmap,iax,tvis
c
c  Externals.
c
	character itoaf*2
	integer len1
c
c  Get the input parameters.
c
	call output('Demos: '//version)
	call keyini
	call keya('map',map,' ')
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call mkeyr('center',pntoff,2*maxpnt,npnt)
	call keya('pbtype',pbtype,' ')
	call keyi('imsize',imsize(1),0)
	call keyi('imsize',imsize(2),imsize(1))
	call keyfin
c
c  Check that the input parameters are reasonable.
c
	if(map.eq.' ')
     *	  call bug('f','An input map must be given')
	if(out.eq.' ')
     *	  call bug('f','An output template name must be given')
	if(mod(npnt,2).ne.0) call bug('f',
     *	 'There must be an even number of values in the "center"')
	npnt = npnt / 2
	if(vis.eq.' '.and.npnt.eq.0)
     *	  call bug('f','No pointing centers were given')
	if(vis.ne.' '.and.npnt.gt.0)
     *	  call bug('w','Values for center override vis file')
	if(vis.eq.' '.and.pbtype.eq.' ')
     *	  call bug('f','Primary beam type should be given')
c
c  Open the input map.
c
	call xyopen(tmap,map,'old',3,nsize)
	call coInit(tmap)
	if(max(nsize(1),nsize(2)).gt.maxdim)
     *	  call bug('f','Input map too big for me to handle')
c
c  Some checks for primary beam stuff; convert user value to radians
c
	call coFindAx(tmap,'longitude',iax)
	if(iax.ne.1)call bug('f','RA axis must be the first axis')
	call coFindAx(tmap,'latitude',iax)
	if(iax.ne.2)call bug('f','DEC axis must be the second axis')
	call coFindAx(tmap,'spectral',iax)
	if(iax.gt.0.and.iax.ne.3) call bug('f',
     *    'The spectral axis of this image must be number 3')
c
c
c  Do we need the frequency for primary beam correction ?
c
        if (vis.ne.' ') then
          call uvopen (tvis, vis, 'old')
          call uvnext (tvis)
        end if
c
c  Get the primary beam type, if needed.
c
	if(pbtype.eq.' ')call pbread(tvis,pbtype)
c
c  Convert from user-units to radians.
c
	do i=1,npnt
	  pntoff(1,i) = pntoff(1,i) / 3600 / 180 * dpi
	  pntoff(2,i) = pntoff(2,i) / 3600 / 180 * dpi
	enddo
c
c  Determine the default pointings, if needed.
c
	if(npnt.eq.0) call Defaults(tmap,tvis,pntoff,maxpnt,npnt)
c
c  If the image size was not given, determine this from the size of the
c  primary beam.
c	
	if(imsize(1).le.0.or.imsize(2).le.0)
     *    call defsiz(tmap,pbtype,nsize,imsize)
c
	if(imsize(1).le.0)
     *	    call bug('f','Internal bug in DEMOS: bad imsize1')
	if(imsize(2).le.0)
     *	    call bug('f','Internal bug in DEMOS: bad imsize2')
c
c  We have fooled around with the inputs long enough! Lets go and start
c  de-mosaicing!
c
	lout = len1(out)
	do i=1,npnt
	  name = out(1:lout)//itoaf(i)
c
	  call Process(tmap,pbtype,name,nsize,imsize,
     *				pntoff(1,i),pntoff(2,i),version)
	enddo
c
	call xyclose(tmap)
        if (vis.ne.' ') call uvclose (tvis)
c
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine Process(tmap,pbtype,name,insize,outsize,
     *				pntoffx,pntoffy,version)
c
	implicit none
	character name*(*),version*(*),pbtype*(*)
	integer tmap,insize(3),outsize(2)
	real pntoffx,pntoffy
c
c  This is the main processing routine in DEMOS. It reads a part of the
c  input map, applies the primary beam, and writes it out. It also
c  processes the history and header bull.
c
c  Inputs:
c    tmap	Handle of the input cube.
c    name	Name of the output cube.
c    insize	Size of the input image.
c    outsize	Max size of the output image.
c    pntoffx	Offset pointing, in x, in radians.
c    pntoffy	Offset pointing, in y, in radians.
c    version	A version id, for the history file.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
        include 'mirconst.h'
c
	integer tout,naxis,pbObj
	integer nsize(MAXNAX),i,j,k,n3,x1,x2,y1,y2
	real x0,y0,data(MAXDIM)
        logical flags(MAXDIM)
	double precision crpix1,crpix2,xin(3),xout(3)
	character line*64
        real pbfac
c
c  Externals.
c
        real pbget
c
c  Header keywords.
c
	integer nkeys
	parameter(nkeys=38)
	character keyw(nkeys)*8
	data keyw/   'bunit   ','btype   ',
     *	  'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *				'crpix3  ','crpix4  ','crpix5  ',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *	  'epoch   ','history ','niters  ','object  ','telescop',
     *	  'observer','restfreq','vobs    ','obsra   ','obsdec  ',
     *	  'obstime ','lstart  ','lstep   ','ltype   ','lwidth  ',
     *	  'bmaj    ','bmin    ','bpa     '/
c
	n3 = insize(3)
	nsize(3) = n3
c
c  Determine the field extent in pixels.
c
	xin(1) = pntoffx
	xin(2) = pntoffy
	xin(3) = 1
	call coCvt(tmap,'ow/ow/ap',xin,'ap/ap/ap',xout)
	x0 = xout(1)
	y0 = xout(2)
c
	x1 = nint(x0 - 0.5*outsize(1))
	x2 = x1 + outsize(1) - 1
	x1 = max(x1,1)
	x2 = min(x2,insize(1))
c
	y1 = nint(y0 - 0.5*outsize(2))
	y2 = y1 + outsize(2) - 1
	y1 = max(y1,1)
	y2 = min(y2,insize(2))
c
c Open the output file.
c
	call rdhdi(tmap,'naxis',naxis,1)
	naxis = min(naxis,MAXNAX)
	nsize(1) = x2 - x1 + 1
	nsize(2) = y2 - y1 + 1
	nsize(3) = n3
	do i=4,naxis
	  nsize(i) = 1
	enddo
	call xyopen(tout,name,'new',naxis,nsize)
c
c  Process its header.
c
	call coCvt1(tmap,1,'op',0.d0,'ap',crpix1)
	call coCvt1(tmap,2,'op',0.d0,'ap',crpix2)
c
	crpix1 = crpix1 - x1 + 1
	crpix2 = crpix2 - y1 + 1
c
	call wrhdd(tOut,'crpix1',crpix1)
	call wrhdd(tOut,'crpix2',crpix2)
c
	do i=1,nkeys
	  call hdcopy(tMap,tOut,keyw(i))
	enddo
	call pbWrite(tOut,pbtype)
c
c  Write some history info.
c
	call hisopen(tOut,'append')
	line = 'DEMOS: Miriad DeMos '//version
        call hiswrite(tOut, line)
	call hisinput(tOut,'DEMOS')
	write(line,'(a,1p2g9.2)')'DEMOS: Pointing offsets(arcsec):',
     *		3600*180/pi * pntoffx, 3600*180/pi * pntoffy
	call hiswrite(tOut,line)
	line = 'DEMOS: Primary beam type used: '//pbtype
	call hiswrite(tOut,line)
	call hisclose(tOut)
c
c  Loop over all planes
c
	do k=1,n3
	  call xysetpl(tmap,1,k)
	  call xysetpl(tOut,1,k)
c
c  Initialise the primary beam object.
c
	  xin(1) = pntoffx
	  xin(2) = pntoffy
	  xin(3) = k
	  call pbInitc(pbObj,pbtype,tmap,'ow/ow/ap',xin)
c
c  Do the real work.
c
	  do j=y1,y2
	    call xyread(tmap,j,data)
	    call xyflgrd(tmap,j,flags)
	    do i=x1,x2
              pbfac = pbget(pbObj,real(i),real(j))
              data(i) = pbfac*data(i)
              flags(i) = pbfac.gt.0.and.flags(i)
	    enddo
	    call xywrite(tOut,j-y1+1,data(x1))
            call xyflgwr(tout,j-y1+1,flags(x1))
	  enddo
	  call pbFin(pbObj)
	enddo
c
	call xyclose(tOut)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine Defaults(tmap,tvis,pntoff,maxpnt,npnt)
c
	implicit none
	integer tmap,tvis
	integer maxpnt,npnt
	real pntoff(2,maxpnt)
c
c  This determines the defaults for pointing offsets from the visibility data
c  file.
c
c  Inputs:
c    tmap	The handle of the input map.
c    tvis	The handle of the visibility file to determine the defaults
c		from.
c    maxpnt	Maximum number of pointings.
c  Output:
c    npnt	Number of pointings given by the user. If this is zero, then
c		it is determined from the data file.
c    pntoff	Pointing offsets. This is output, if on entry npnt is 0.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
	double precision ra,dec,cdelt1,cdelt2,x1(2),x2(2)
	real tol,dra,ddec,offra,offdec
	character line*64
	logical more,found
c
c  Externals.
c
	integer uvscan
	logical uvupdate
c
c  Get info about the map.
c
	call rdhdd(tmap,'cdelt1',cdelt1,dpi/(180*3600))
	call rdhdd(tmap,'cdelt2',cdelt2,dpi/(180*3600))
	tol = max(abs(cdelt1),abs(cdelt2))
c
c  Rewind vis file, get the first record.
c
        call uvrewind (tvis)
	call uvtrack(tvis,'ra','u')
	call uvtrack(tvis,'dra','u')
	call uvtrack(tvis,'dec','u')
	call uvtrack(tvis,'ddec','u')
	call uvnext(tvis)
c
c  Loop through the file, getting the data.
c
	more = .true.
	dowhile(more)
	  call uvgetvrd(tvis,'ra',ra,1)
	  call uvrdvrr(tvis,'dra',dra,0.)
	  call uvgetvrd(tvis,'dec',dec,1)
	  call uvrdvrr(tvis,'ddec',ddec,0.)
c
c  Convert to offset coordinates.
c
	  x1(1) = ra + dra/cos(dec)
	  x1(2) = dec + ddec
	  call coCvt(tmap,'aw/aw',x1,'ow/ow',x2)
	  offra  = x2(1)
	  offdec = x2(2)
c
	  found = .false.
	  i = 1
	  dowhile(.not.found.and.i.le.npnt)
	    found = abs(offra -pntoff(1,i)).lt.tol.and.
     *		    abs(offdec-pntoff(2,i)).lt.tol
	    i = i + 1
	  enddo
	  if(.not.found)then
	    if(npnt.ge.maxpnt)
     *	      call bug('f','Too many different pointing in vis')
	    npnt = npnt + 1
	    pntoff(1,npnt) = offra
	    pntoff(2,npnt) = offdec
	  endif
c
c  Wait for one of the RA, DEC, etc variables to be updated.
c
	  found = .false.
	  more = .true.
	  dowhile(more.and..not.found)
	    more = uvscan(tvis,' ').eq.0
	    if(more)found = uvupdate(tvis)
	  enddo
	enddo
c
c  Give a summary to the user.
c
	call output(
     *	  'The visibility file contains the following pointings')
	call output(
     *	  'Number  xoff(arcsec)  yoff(arcsec)')
	do i=1,npnt
	  write(line,'(i4,f13.2,f14.2)')i,
     *	    180*3600/pi * pntoff(1,i),
     *	    180*3600/pi * pntoff(2,i)
	  call output(line)
	enddo
c
c Rewind vis file
c
        call uvrewind (tvis)
	end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine defsiz(tmap,pbtype,nsize,size)
c
      implicit none
      integer tmap,nsize(2),size(2)
      character pbtype*(*)
c
c  Set default size of image to one primary beam
c
c   Input
c    tmap	Handle of the input dataset.
c    nsize	Max size of the axis.
c    pbtype	Primary beam type.
c  Output
c    size     Size of image
c
c-----------------------------------------------------------------------
	double precision cdelt,crval,crpix
	character ctype*16
	real pbfwhm,cutoff,maxrad
	integer pbObj,i
c
	call pbInit(pbObj,pbtype,tmap)
	call pbInfo(pbObj,pbfwhm,cutoff,maxrad)
c
	do i=1,2
	  if(size(i).le.0)then
	    call coAxDesc(tmap,i,ctype,crpix,crval,cdelt)
	    size(i) = min(2 * nint(pbfwhm / abs(cdelt)), nsize(i))
	  endif
	enddo
c
	call pbFin(pbObj)
c
	end
