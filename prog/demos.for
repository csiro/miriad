c************************************************************************
	program demos
	implicit none
c
c  This is a task to perform a de-mosaicing operation. The input is a
c  primary beam corrected image, or single dish image. The output is a
c  number of images, with different pointing centres, each with
c  a primary beam applied.
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
c@ pbfwhm
c	The demosaicing procedure requires you to apply a primary beam.
c	Here, you can specify the FWHM (in arcseconds) of a Gaussian primary
c	beam. If there is a spectral axis in the image, "pbfwhm" is assumed
c	to be at the frequency of the spectral axis reference pixel.
c	If there is no spectral axis, "pbfwhm" is used unscaled.  If "pbfwhm"
c	is unset, DEMOS will construct the primary beam from the "pbfwhm"
c	visibility variable or if that is missing, the telescope name (variable
c	"telescop") in the template visibility file.   In the latter case, for
c	the ATCA and VLA the primary beam is a polynomial which cuts off at 
c	about the 2% level. For HatCreek, a Gaussian with no cutoff is used.
c	If the primary is neither specified here nor can be worked out from 
c	template file (if given), DEMOS will stop. 
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
c	If "imsize" & "pbfwhm" are unset, and the "pbfwhm" variable in 
c	"vis" is missing, you MUST have a frequency axis in the "map" 
c	input image so that the internal knowledge about primary beams
c	(worked out from the "telescop" variable and stored in a frequency 
c	independent way) can be accessed.
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
c------------------------------------------------------------------------
	character version*(*)
	integer maxpnt
	parameter(version='version 12-feb-93',maxpnt=16)
	include 'maxdim.h'
        include 'mirconst.h'
	real pntoff(2,maxpnt)
        double precision crpix1,crpix2,cdelt1,cdelt2,crpix3,frqref,finc
	real x0,y0,pbfwhm
	integer x1,x2,y1,y2
	character map*64,vis*64,out*64,name*64
	integer imsize(2),nsize(3),npnt,lout,i,tmap,ifax,tvis,ierr
        character*80 umsg,strax
        logical needf
c
c  Externals.
c
	character itoaf*2
	integer len1
c
c  Get the input parameters.
c
        umsg = 'Demos: '//version
	call output( umsg )
	call keyini
	call keya('map',map,' ')
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call mkeyr('center',pntoff,2*maxpnt,npnt)
        call keyr('pbfwhm',pbfwhm,-1.0)
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
c
c  Open the input map.
c
	call xyopen(tmap,map,'old',3,nsize)
	if(max(nsize(1),nsize(2)).gt.maxdim)
     *	  call bug('f','Input map too big for me to handle')
	call rdhdd(tmap,'crpix1',crpix1,dble(nsize(1)/2+1))
	call rdhdd(tmap,'crpix2',crpix2,dble(nsize(2)/2+1))
	call rdhdd(tmap,'cdelt1',cdelt1,0.0d0)
	if(cdelt1.eq.0)
     *	    call bug('f','Cdelt1 was not present in map header')
	call rdhdd(tmap,'cdelt2',cdelt2,0.0d0)
	if(cdelt2.eq.0)
     *	    call bug('f','Cdelt2 was not present in map header')
c
c  Some checks for primary beam stuff; convert user value to radians
c
        strax = ' '
        call fndaxnum(tmap,'freq',strax,ifax)
        if (ifax.gt.0 .and. ifax.ne.3) call bug ('f',
     *    'The spectral axis of this image must be number 3')
c
        if (pbfwhm.le.0.0 .and. vis.eq.' ') call bug ('f',
     *  'No PBFWHM keyword or vis. file given; can''t set primary beam')
        if (pbfwhm.gt.0.0) pbfwhm = pbfwhm * dpi / 180.0d0 / 3600.0d0
c
c  Do we need the frequency for primary beam correction ?
c
        if (vis.ne.' ') then
          call uvopen (tvis, vis, 'old')
          call uvnext (tvis)
        end if
        call pbcheck (pbfwhm, tvis, .false., .false., needf)
        if (needf) then
          if (ifax.ne.3) then
            call output 
     *         ('The input image needs a spectral axis (#3) so that')
            call output 
     *         ('the frequency can be found for primary beam purposes')
            call bug ('f', ' ')
          else
c
c  Get frequency of reference pixel
c
            call rdhdd (tmap, 'crpix'//itoaf(ifax), crpix3, 1.0d0)
            call getfreq (tmap, crpix3, ifax, frqref, finc, ierr)
            if (ierr.gt.1) call bug ('f',
     +        'Could not find frequency of spectral ref. pixel')
          end if
        else
          frqref = -1.0d0
        end if
c
c  Set primary beam.  Use reference frequency since pixel size is
c  scaled to reference frequency.
c
        call pbinit (pbfwhm, frqref, tvis, .false.)
c
c  Determine the default pointings, if needed.
c
	if(npnt.eq.0) call Defaults(tmap,tvis,pntoff,maxpnt,npnt)
c
c  Convert from user-units to radians.
c
	do i=1,npnt
	  pntoff(1,i) = pntoff(1,i) / 3600 / 180 * dpi
	  pntoff(2,i) = pntoff(2,i) / 3600 / 180 * dpi
	enddo
c
c  If the image size was not given, determine this from the size of the
c  primary beam.
c
	if(imsize(1).le.0) 
     *    call defsiz(cdelt1,imsize(1))
	imsize(1) = min(imsize(1),nsize(1))
	if(imsize(2).le.0) 
     *    call defsiz(cdelt2,imsize(2))
        imsize(2) = min(imsize(2),nsize(2))
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
	  x0 = crpix1 + pntoff(1,i)/cdelt1
	  x1 = nint(x0 - 0.5*imsize(1))
	  x2 = x1 + imsize(1) - 1
	  x1 = max(x1,1)
	  x2 = min(x2,nsize(1))
c
	  y0 = crpix2 + pntoff(2,i)/cdelt2
	  y1 = nint(y0 - 0.5*imsize(2))
	  y2 = y1 + imsize(2) - 1
	  y1 = max(y1,1)
	  y2 = min(y2,nsize(2))
c
	  call Process(tmap,name,x0,y0,x1,y1,x2,y2,nsize(3),
     *		pntoff(1,i),pntoff(2,i),version)
	enddo
c
	call xyclose(tmap)
        if (vis.ne.' ') call uvclose (tvis)
c
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine Process(tmap,name,x0,y0,x1,y1,x2,y2,n3,
     *	  pntoffx,pntoffy,version)
c
	implicit none
	character name*(*),version*(*)
	integer tmap,x1,y1,x2,y2,n3
	real x0,y0
	real pntoffx,pntoffy
c
c  This is the main processing routine in DEMOS. It reads a part of the
c  input map, applies the primary beam, and writes it out. It also
c  processes the history and header bull.
c
c  Inputs:
c    tmap	Handle of the input cube.
c    name	Name of the output cube.
c    x1,y1,x2,y2 Subportion of the input to place in the output (grid units).
c    x0,y0	Pointing center, in grid units.
c    pntoffx	Offset pointing, in x, in radians.
c    pntoffy	Offset pointing, in y, in radians.
c    n3		Size of the cubes 3rd dimension.
c    version	A version id, for the history file.
c------------------------------------------------------------------------
	include 'maxdim.h'
        include 'mirconst.h'
	integer naxis
	parameter(naxis=3)
	integer tout
	integer nsize(3),i,j,k
	real x,y,data(maxdim)
	double precision cdelt1,cdelt2,crpix1,crpix2,crval1,crval2
        logical flags(maxdim)
	character line*64
        real pbfac
        real pbget
c
c  Header keywords.
c
	integer nkeys
	parameter(nkeys=29)
	character keyw(nkeys)*8
	data keyw/   'bunit   ','cdelt3  ','crpix3  ','crval3  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','date-obs',
     *	  'epoch   ','history ','instrume','niters  ','object  ',
     *	  'telescop','observer','restfreq','vobs    ','xshift  ',
     *	  'yshift  ','obsra   ','obsdec  ','lstart  ','lstep   ',
     *	  'ltype   ','lwidth  ','bmaj    ','bmin    ','bpa     ',
     *    'btype   '/
c
c Open the output file.
c
	nsize(1) = x2 - x1 + 1
	nsize(2) = y2 - y1 + 1
	nsize(3) = n3
	call xyopen(tout,name,'new',naxis,nsize)
c
c  Process its header.
c
	call rdhdd(tMap,'cdelt1',cdelt1,dpi/(3600*180))
	call rdhdd(tMap,'crpix1',crpix1,1.0d0)
	call rdhdd(tMap,'crval1',crval1,0.0d0)
	call rdhdd(tMap,'cdelt2',cdelt2,dpi/(3600*180))
	call rdhdd(tMap,'crpix2',crpix2,1.0d0)
	call rdhdd(tMap,'crval2',crval2,0.0d0)
c
	crval2 = crval2 + (y0-crpix2)*cdelt2
	crpix2 = y0 - y1 + 1
	crval1 = crval1 + (x0-crpix1)*cdelt1/cos(crval2)
	crpix1 = x0 - x1 + 1
c
	call wrhdd(tOut,'cdelt1',cdelt1)
	call wrhdd(tOut,'crpix1',crpix1)
	call wrhdd(tOut,'crval1',crval1)
	call wrhdd(tOut,'cdelt2',cdelt2)
	call wrhdd(tOut,'crpix2',crpix2)
	call wrhdd(tOut,'crval2',crval2)
c
c  Write primary beam into header in arcsec if Gaussian
c
        call gausspb (tout)
	do i=1,nkeys
	  call hdcopy(tMap,tOut,keyw(i))
	enddo
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
	call hisclose(tOut)
c
c  Loop over all planes
c
	do k=1,n3
	  call xysetpl(tmap,1,k)
	  call xysetpl(tOut,1,k)
	  do j=y1,y2
	    call xyread(tmap,j,data)
	    call xyflgrd(tmap,j,flags)
            y = ((j-y0)*cdelt2)**2
	    do i=x1,x2
              x = ((i-x0)*cdelt1)**2 
              pbfac = pbget(x+y)
              data(i) = pbfac*data(i)
              flags(i) = pbfac.gt.0.0
	    enddo
	    call xywrite(tOut,j-y1+1,data(x1))
            call xyflgwr(tout,j-y1+1,flags(x1))
	  enddo
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
	double precision ra,dec,crval1,crval2,cdelt1,cdelt2
	real tol,dra,ddec,cosdec,offra,offdec
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
	call rdhdd(tmap,'crval1',crval1,0.d0)
	call rdhdd(tmap,'cdelt1',cdelt1,dpi/(180*3600))
	call rdhdd(tmap,'crval2',crval2,0.d0)
	call rdhdd(tmap,'cdelt2',cdelt2,dpi/(180*3600))
	cosdec = cos(crval2)
	tol = 3600*180/pi * max(abs(cdelt1),abs(cdelt2))
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
	  call uvrdvrd(tvis,'ra',ra,crval1)
	  call uvrdvrr(tvis,'dra',dra,0.)
	  call uvrdvrd(tvis,'dec',dec,crval2)
	  call uvrdvrr(tvis,'ddec',ddec,0.)
	  offra =  ((ra - crval1)*cosdec + dra ) * 3600*180/pi
	  offdec = ((dec - crval2)       + ddec) * 3600*180/pi
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
	  write(line,'(i4,f13.2,f14.2)')i,pntoff(1,i),pntoff(2,i)
	  call output(line)
	enddo
c
c Rewind vis file
c
        call uvrewind (tvis)
	end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine defsiz (cdelt, size)
c
      implicit none
      integer size
      double precision cdelt
c
c  Set default size of image to one primary beam
c
c   Input
c    cdelt    Image pixel increment
c  Output
c    size     Size of image
c
c-----------------------------------------------------------------------
      double precision coeffs(5)
      real pbfwhm, cut
      character type*6
c
c Fish out Gaussian FWHM.  WIll be "pbfwhm" or internal FWHM value.
c
      call pbinfo (pbfwhm, coeffs, cut, type)
c
c Set default size
c
      size = 2 * nint(pbfwhm / abs(cdelt))
      end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine gausspb (tout)
      implicit none
      integer tout
c
c     Return FWHM of primary beam if Gaussian set.
c
c Input
c   tout   Handle of output image
c-----------------------------------------------------------------------
      include 'mirconst.h'
      double precision coeffs(5)
      real cut, pbfwhm
      character type*10
c
c Fish out Gaussian FWHM.  WIll be "pbfwhm" or internal FWHM value.
c
      call pbinfo (pbfwhm, coeffs, cut, type)
c
      if (type.eq.'SINGLE') then
        call bug ('f', 
     +  'The "pbfwhm" visibility variable indicates a single dish')
      else if (type.eq.'GAUSS') then
        pbfwhm = pbfwhm * 3600.0d0 * 180.0d0 / dpi
        call wrhdr (tout, 'pbfwhm', pbfwhm)
      end if
      end
c********1*********2*********3*********4*********5*********6*********7*c
