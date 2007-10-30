c***************************************************************
	program imgen
c
	implicit none
c
c= imgen - All-purpose image manipulator/creator
c& mchw
c: utility, map manipulation
c+
c	IMGEN is a MIRIAD task which modifies an image, or creates a new
c	image. 
c@ in
c	The input image, which is to be modified. The default is a map
c	with one plane which consists entirely of zeros. 
c@ out
c	The output image has the same dimensions as the input. No default. 
c@ factor
c	Factor to multiply the input image by. This is meaningless if no
c	input image is given. The default is 1. 
c@ imsize
c	If not input image is given, then this determines the size, in
c	pixels, of the output image. Either one or two numbers can be
c	given. If only one number is given, then the output is square.
c	The default is 256 pixels square. 
c@ object
c	This determines the type of object added to the input image.
c	Valid objects are "gaussian" (elliptical gaussians), "disk"
c	(elliptical disks), "j1x", (a J1(x)/x function), "point"
c	(a point source), "noise" (gaussian noise) and "level" (a dc
c	level). The same object is added to each plane of the input image.
c	The default is "gaussian" . 
c@ spar
c	Parameters which give the characteristics of the object. For
c	gaussian and j1x, spar consists of up to three numbers, being
c	the amplitude (default 1), the object full-width at half-max in
c	RA (default 1), and full-width half-max in DEC (default is width
c	in RA). For a disk, spar consists of up to four numbers: 
c	the amplitude (default 1), the object full-width major axis
c	(default 1), full-width half-max minor axis (default is width
c	of major axis), and the position angle of the major axis (defined
c       as angle east of north). Widths are given in arcseconds and the
c       position angle is in degrees. For points and levels, only one 
c       number, the amplitude, is needed, which has a default of 1. 
c@ xy
c	This is two numbers giving the relative offset of the center of
c	the object, in arcseconds. This is relative to the reference
c	point of the input, or the image center if there is no input.
c	This is ignored if the object is a level. The default is 0.
c@ cell
c	The increment between pixels, in arcseconds. This is only used if
c	there is no input map. The default is 1 arcsec.
c--
c
c  History:
c    rjs  Dark-ages Original version.
c    rjs  27oct89   Renamed it IMGEN.
c    rjs  30may91   Write some extra header variables.
c    mchw 03jun91   Fixed a bug and added more header variables.
c    rjs  29aug91   Included 'mirconst'. The old value of pi was wrong.
c    nebk 20sep91   Made gaussian noise image start with different
c                   seed on each application of imgen.
c    pjt  15oct91   multiplied julian date by 100000 in randset() for seed
c                   created crval, cdelt and crpix all in double now
c                   filename buffers (in,out) a bit longer
c    pjt  16oct91   fixed some style ambiguities
c    pjt   7nov91   added 'bpa' item in case gaussian model
c    mchw  10jul92  fixed bug in bpa definition.
c    nebk  09sep92  Make reference pixel 1.0 for axes of length
c                   1, otherwise point models fall of map
c    nebk  04nov92  Copy BTYPE to output
c    pjt   13nov92  Initial version to handle 3D input cubes
c                   also updated pbfwhm into copy header list!!!!!!
c    rjs   24nov92  Merged pjt and nebk versions.
c    mchw  10apr93  Improved warning messages.
c    lgm   27apr93  Added feature allowing specification of position angle
c                   for disk model.
c    mchw  19may93  Merged lgm and mchw versions; Adjust doc to match code.
c  Bugs/Wishlist:
c    * The objects could take position angles.
c---------------------------------------------------------------
	character version*(*)
	parameter(version='Imgen: version 1.1 19-May-93' )
	include 'mirconst.h'
	include 'maxdim.h'
	integer n1,n2,n3,j,k,lIn,lOut,nsize(3),naxis
	double precision crpix1,crpix2,crpix3,cdelt1,cdelt2,cdelt3
	real factor,amp,fwhm1,fwhm2,posang,x,y
	character In*80,Out*80,Object*10
	real Buff(maxdim)
        double precision juld
c
c  Get the parameters from the user.
c
	call output(version)
	call keyini
	call keya('in',In,' ')
	call keya('out',Out,' ')
	call keyr('factor',Factor,1.)
	call keya('object',Object,'gaussian')
	call keyi('imsize',n1,128)
	call keyi('imsize',n2,n1)
	call keyr('spar',amp,1.)
	call keyr('spar',fwhm1,1.)
	call keyr('spar',fwhm2,fwhm1)
        call keyr('spar',posang,0.0)
	call keyr('xy',x,0.)
	call keyr('xy',y,0.)
	call keyd('cell',cdelt1,0.0d0)
	call keyd('cell',cdelt2,cdelt1)
	call keyfin
c
c  Convert to radians.
c
	x = x/3600 * pi/180.
	y = y/3600 * pi/180.
	fwhm1 = fwhm1/3600 * pi/180.
	fwhm2 = fwhm2/3600 * pi/180.
        posang = posang * pi/180.
	cdelt1 = cdelt1/3600 * pi/180.
	cdelt2 = cdelt2/3600 * pi/180.
c
c  If there is an input file, open it and get parameters about it.
c  Otherwise set the default parameters.
c
	if(Out.eq.' ')call bug('f','Output file name missing')
	if( (fwhm1.le.0.or.fwhm2.le.0.) .and. (object.ne.'point') )
     *	     call bug('f','FWHM parameters must be > zero')
	if(In.ne.' ')then
	  call xyopen(lIn,in,'old',3,nsize)
	  n1 = nsize(1)
	  n2 = nsize(2)
 	  n3 = nsize(3)
	  if(nsize(3).ne.1)call bug('w','Crude handling of 3D images')
	  call rdhdd(lIn,'crpix1',crpix1,dble(n1/2.0+1))
	  call rdhdd(lIn,'crpix2',crpix2,dble(n2/2.0+1))
	  call rdhdd(lIn,'crpix3',crpix3,dble(n3/2.0+1))
	  call rdhdd(lIn,'cdelt1',cdelt1,0.0d0)
	  call rdhdd(lIn,'cdelt2',cdelt2,0.0d0)
	  call rdhdd(lIn,'cdelt3',cdelt3,0.0d0)
	  if(cdelt1*cdelt2.eq.0)call bug('f','Pixel increment missing')
	  call rdhdi(lIn,'naxis',naxis,1)
	  naxis = min(naxis,3)
	else
	  n3=1
	  nsize(1) = n1
	  nsize(2) = n2
	  nsize(3) = 1
	  lIn = 0
	  if(n1.le.0.or.n2.le.0)call bug('f','Image size error')
	  crpix1 = dble(n1/2.0 + 1)
          if (nsize(1).eq.1) crpix1 = 1.0
	  crpix2 = dble(n2/2.0 + 1)
          if (nsize(2).eq.1) crpix2 = 1.0
	  if(cdelt1.eq.0)cdelt1 = -1./3600. * pi/180.
	  cdelt1 = - abs(cdelt1)
	  if(cdelt2.eq.0)cdelt2 =  1./3600. * pi/180.
	  naxis = 2
	endif
c
c  Now open the output, and add a header to it.
c
	call xyopen(lOut,Out,'new',naxis,nsize)
	call header(lIn,lOut,crpix1,crpix2,cdelt1,cdelt2,
     *	  object,fwhm1,fwhm2,version)
c
c  Convert to units that we want, namely x and y in grid coordinates
c  and fwhm in pixels.
c
	x = x/cdelt1 + crpix1
	y = y/cdelt2 + crpix2
	fwhm1 = fwhm1/abs(cdelt1)
	fwhm2 = fwhm2/abs(cdelt2)
c
c  Do a second run, and write it out this time.
c
        if (object.eq.'noise') then
          call todayjul (juld)
          juld = mod(juld*100000.0d0,100000.0d0)
          call randset (nint(juld))
        end if
c
	if (naxis.gt.2) then
	  do k=1,n3
	    call xysetpl(lIn,1,k)
	    call xysetpl(lOut,1,k)
	    do j=1,n2
             call DoMod(lIn,j,object,Buff,n1,factor,amp,fwhm1,fwhm2,
     *                    posang,x,y)
	     call xywrite(lOut,j,Buff)
	    enddo
	  enddo
	else
	  do j=1,n2
	    call DoMod(lIn,j,object,Buff,n1,factor,amp,fwhm1,fwhm2,
     *                    posang,x,y)
	    call xywrite(lOut,j,Buff)
	  enddo
	endif
c
c  Close up shop.
c
	if(lIn.ne.0)call xyclose(lIn)
	call xyclose(lOut)
	end
c*******************************************************************
	subroutine header(lIn,lOut,crpix1,crpix2,cdelt1,cdelt2,
     *	  object,fwhm1,fwhm2,version)
c
	implicit none
	integer lIn,lOut
	real fwhm1,fwhm2
        double precision crpix1,crpix2,cdelt1,cdelt2
	character object*(*),version*(*)
c
c  Make a header for the output image.
c
c------------------------------------------------------------------------
	integer nkeys
	parameter(nkeys=39)
	character line*64
	integer i
	character keyw(nkeys)*8
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','crpix1  ',
     *    'crpix2  ','crpix3  ','crpix4  ','crval1  ','crval2  ',
     *    'crval3  ','crval4  ','ctype1  ','ctype2  ','ctype3  ',
     *    'ctype4  ','date-obs','epoch   ','instrume','ltype   ',
     *    'lstart  ','lwidth  ','lstep   ','niters  ','object  ',
     *    'observer','obsra   ','obsdec  ','restfreq','telescop',
     *	  'vobs    ','xshift  ','yshift  ','pbfwhm  ','btype   '/
c
c  Either create a new header, or copy the old one.
c
	if(lIn.eq.0)then
	  call wrhdd(lOut,'crpix1',crpix1)
	  call wrhdd(lOut,'crpix2',crpix2)
	  call wrhdd(lOut,'cdelt1',cdelt1)
	  call wrhdd(lOut,'cdelt2',cdelt2)
	  call wrhdd(lOut,'crval1',0.0d0)
	  call wrhdd(lOut,'crval2',0.0d0)
	  call wrhda(lOut,'ctype1','RA---SIN')
	  call wrhda(lOut,'ctype2','DEC--SIN')
	  if(object.eq.'gaussian')then
	    call wrhda(lOut,'bunit','JY/BEAM')
	    call wrhdr(lOut,'bmaj',fwhm1)
	    call wrhdr(lOut,'bmin',fwhm2)
            call wrhdr(lOut,'bpa',90.0)
	  else if(object.eq.'point')then
	    call wrhda(lOut,'bunit','JY/PIXEL')
	  endif
	else
	  do i=1,nkeys
	    call hdcopy(lIn,lOut,keyw(i))
	  enddo
	endif
c
c  Update the history.
c
	call hisopen(lOut,'append')
	line = 'IMGEN: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'IMGEN')
	call hisclose(lOut)
c
	end
c************************************************************************
	subroutine DoMod(lu,j0,object,Data,n1,factor,
     *				amp,fwhm1,fwhm2,posang,x,y)
c
	implicit none
	integer lu,n1,j0
	character object*(*)
	real Data(n1)
        real factor,amp,fwhm1,fwhm2,posang,x,y
c
c  Add the contribution.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,ymin,ymax,xmin,xmax
	real xx,yy,xp,yp,scale
	real Buff(maxdim)
c
c  Externals.
c
	real j1xbyx
c
c  Get the old data.
c
	if(lu.eq.0.or.factor.eq.0)then
	  do i=1,n1
	    Data(i) = 0
	  enddo
	else
	  call xyread(lu,j0,Data)
	  do i=1,n1
	    Data(i) = Factor * Data(i)
	  enddo
	endif
c
c  Add the new contribution.First the gaussian. Work out the region
c  where the exponential greater than exp(-25), and don't bother
c  processing those regions.
c
	if(object.eq.'gaussian')then
	  scale = 2. * sqrt(log(2.))
	  ymin = nint(y-(5/scale)*fwhm2)
	  ymax = nint(y+(5/scale)*fwhm2)
	  xmin = max(nint(x-(5/scale)*fwhm1),1)
	  xmax = min(nint(x+(5/scale)*fwhm1),n1)
	  if(ymin.le.j0.and.j0.le.ymax)then
	    yy = (scale/fwhm2) * (j0-y)
	    yy = yy*yy
	    do i=xmin,xmax
	      xx = (scale/fwhm1) * (i-x)
	      xx = xx * xx
	      data(i) = data(i) + amp*exp(-(xx+yy))
	    enddo
	  endif
c
c  Handle a J1(x)/x function.
c
	else if(object.eq.'j1x')then
	  scale = 3.83
	  yy = (scale/fwhm2) * (j0-y)
	  yy = yy*yy
	  do i=1,n1
	    xx = (scale/fwhm1) * (i-x)
	    xx = xx*xx
	    data(i) = data(i) + 2 * amp * j1xbyx(sqrt(xx+yy))
	  enddo
c
c  Handle a disk.
c
	else if(object.eq.'disk')then
	  ymin = nint(y-0.5*fwhm1)
	  ymax = nint(y+0.5*fwhm1)
	  xmin = max(nint(x-0.5*fwhm1),1)
	  xmax = min(nint(x+0.5*fwhm1),n1)
	  if(ymin.le.j0.and.j0.le.ymax)then
	    yy = (j0-y)
	    do i=xmin,xmax
	      xx = (i-x)
              xp =  xx*cos(posang) + yy*sin(posang)
              yp = -xx*sin(posang) + yy*cos(posang)
              xp = (xp*xp)/(fwhm2*fwhm2)
              yp = (yp*yp)/(fwhm1*fwhm1)
	      if(xp+yp.lt.0.25) data(i) = data(i) + amp
	    enddo
	  endif
c
c  Handle a DC level.
c
	else if(object.eq.'level')then
	  do i=1,n1
	    data(i) = data(i) + amp
	  enddo
c
c  Handle a Noise level.
c
	else if(object.eq.'noise')then
	  call gaus(buff,n1)
	  do i=1,n1
	    data(i) = data(i) + amp * buff(i)
	  enddo
c
c  Handle a point source.
c
	else if(object.eq.'point')then
	  i = nint(x)
	  j = nint(y)
	  if(j.eq.j0)Data(i) = Data(i) + Amp
c
c  Should never get here.
c
	else
	  call bug('f','Unknown object type')
	endif
	end
