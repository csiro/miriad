c***************************************************************
	program imgen
c
	implicit none
c
c= imgen - All-purpose image manipulator/creator
c& rjs
c: utility, map manipulation
c+
c	IMGEN is a MIRIAD task which modifies an image, or creates a new
c	image. 
c@ in
c	The input image, which is to be modified. The default is a map
c	with one plane which consists entirely of zeros. 
c@ out
c	The name of the output image. No default. The output image has the
c	same characteristics as the input image, if present. If no input
c	image is given, the `imsize', `cell' and `radec' keywords give the
c	characteristics of the output.
c@ factor
c	Factor to multiply the input image by. This is meaningless if no
c	input image is given. The default is 1. 
c@ object
c	This determines the type of objects added to the input image.
c	Several objects can be given (the objects can be the same type, or
c	different), and minimum match is supported. Possible objects are:
c	   level      An offset (DC) level.
c	   noise      Noise (gaussian distribution).
c	   point      A point source.
c	   gaussian   An elliptical or circular gaussian.
c	   disk       An elliptical or circular disk.
c	   j1x        A J1(x)/x function
c	   shell      2D projection of an optically-thin spherical shell
c	   comet      2D projection of a parent molecule in comet.
c@ spar
c	Parameters which give the characteristics of the object. The
c	parameters are given as a sequence of values, with one to six
c	values needed per object (depending on the object type). When
c	there are multiple objects, the parameter value for the second
c	object follow those for the first object, etc. The values are
c	as follows:
c	  Object Type           SPAR values
c	  -----------           -----------
c	   level                  offset
c	   noise                  rms
c	   point                  amp,x,y
c	   gaussian               amp,x,y,bmaj,bmin,pa
c	   disk                   amp,x,y,bmaj,bmin,pa
c	   j1x                    amp,x,y,bmaj,bmin,pa
c	   shell                  amp,x,y,bmaj
c	   comet                  amp,x,y,scalelength
c	Here "offset" is the offset level, "rms" is the rms value of
c	the noise, "amp" is the normally peak value of the object (but
c	see options=totflux below), "x" and "y" are the offset positions (in
c	arcsec) of the object relative to the reference pixel, "bmaj" and
c	"bmin" are the major and minor axes FWHM (in arcsec), and "pa" is
c	the position angle of an elliptical component (in degrees). The
c	position angle is measured from north towards east.
c	The default is an object of unit amplitude, at the reference pixel,
c	with a FWHM of 5 arcsec.
c@ imsize
c	If not input image is given, then this determines the size, in
c	pixels, of the output image. Either one or two numbers can be
c	given. If only one number is given, then the output is square.
c	The default is 256 pixels square. 
c@ cell
c	The increment between pixels, in arcseconds. This is only used if
c	there is no input map. The default is 1 arcsec.
c@ radec
c	If no input image is given, this gives the RA and DEC of the
c	image, in hours and degrees, respectively. They can be given in
c	hh:mm:ss,dd:mm:ss, or as decimal hours and degrees. The default is
c	RA=0, DEC=45.
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match is used. Possible values are:
c	  totflux  Interpret the "amp" values in the spar keyword as
c	           total integrated flux densities (Normally the "amp"
c	           parameters are interpretted as peak values).
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
c    rjs   19aug94  Major rework. Multiple objects. Position angle. Better
c		    coords.
c    rjs   12sep94  totflux option.
c    bmg   08may96  Added object=shell
c    rjs   24sep96  Some corrections to object=shell.
c    rjs   13dec96  Increase max number of objects.
c    rjs   02jul97  cellscal change.
c    rjs   14jul97  Check when there are too many objects and increase
c		    max number of objects.
c    rjs   23jul97  added pbtype.
c    mchw  23oct97  added comet model for parent molecule.
c    rjs   29oct97  Check that the coordinates for a point source fall
c		    within the image.
c    rjs   11dec97  Make total flux option consistent when there is an
c		    input image.
c  Bugs/Wishlist:
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Imgen: version 1.1 11-Dec-97' )
	include 'mirconst.h'
	include 'maxdim.h'
	include 'maxnax.h'
	integer n1,n2,n3,i,j,k,lIn,lOut,nsize(MAXNAX),naxis
	double precision crpix1,crpix2,cdelt1,cdelt2,crval1,crval2
	double precision x1(3),x2(3)
	real factor,bmaj,bmin,bpa,fac
	character In*80,Out*80
	logical totflux
	real Buff(maxdim)
c
c  Source parameters.
c
	integer MAXOBJS
	parameter(MAXOBJS=3000)
	real fwhm1(MAXOBJS),fwhm2(MAXOBJS),posang(MAXOBJS)
	real amp(MAXOBJS),x(MAXOBJS),y(MAXOBJS)
	real fwhm1d(MAXOBJS),fwhm2d(MAXOBJS),posangd(MAXOBJS)
	real xd(MAXOBJS),yd(MAXOBJS)
	character objs(MAXOBJS)*8
c
	integer NOBJECTS
	parameter(NOBJECTS=8)
	integer nobjs
	character objects(NOBJECTS)*8
c
c  Externals.
c
	logical keyprsnt
c
	data objects/'level   ','noise   ','point   ',
     *		     'gaussian','disk    ','j1x     ',
     *               'shell   ','comet   '/
c
c  Get the parameters from the user.
c
	call output(version)
	call keyini
	call keya('in',In,' ')
	call keya('out',Out,' ')
	call keyr('factor',Factor,1.)
	call keymatch('object',NOBJECTS,objects,MAXOBJS,objs,nobjs)
	if(nobjs.eq.0)then
	  objs(1) = 'gaussian'
	  nobjs = 1
	endif
	if(keyprsnt('object'))call bug('f','Too many object for me!')
c
c  Get the source parameters.
c
	do i=1,nobjs
	  call keyr('spar',amp(i),1.)
	  if(objs(i).ne.'level'.and.objs(i).ne.'noise')then
	    call keyr('spar',x(i),0.)
	    call keyr('spar',y(i),0.)
	    x(i) = x(i) / 3600. * pi/180.
	    y(i) = y(i) / 3600. * pi/180.
	  else
	    x(i) = 0
	    y(i) = 0
	  endif
	  if(objs(i).eq.'gaussian'.or.objs(i).eq.'disk'.or.
     *	     objs(i).eq.'j1x')then
	    call keyr('spar',fwhm1(i),5.)
	    call keyr('spar',fwhm2(i),5.)
	    call keyr('spar',posang(i),0.)
	    fwhm1(i) = fwhm1(i) / 3600. * pi/180.
	    fwhm2(i) = fwhm2(i) / 3600. * pi/180.
	    if(min(fwhm1(i),fwhm2(i)).le.0)
     *	      call bug('f','BMAJ and BMIN parameters must be positive')
	    posang(i) = posang(i) * pi/180.
	  elseif(objs(i).eq.'shell'.or.objs(i).eq.'comet') then
            call keyr('spar',fwhm1(i),5.)
	    fwhm1(i) = fwhm1(i) / 3600. * pi/180.
	    if(fwhm1(i).le.0)
     *	      call bug('f','BMAJ and BMIN parameters must be positive')
	    fwhm2(i) = fwhm1(i)
	    posang(i) = 0
	  else
	    fwhm1(i) = 0
	    fwhm2(i) = 0
	    posang(i) = 0
	  endif
	enddo
c
c  Get parameters used to construct the output image (if needed).
c
	call keyd('cell',cdelt1,-1.0d0)
	call keyd('cell',cdelt2,cdelt1)
	cdelt1 = -abs(cdelt1/3600 * pi/180.)
	cdelt2 =  abs(cdelt2/3600 * pi/180.)
	call keyi('imsize',n1,256)
	call keyi('imsize',n2,n1)
	n3 = 1
	call keyt('radec',crval1,'hms',0.d0)
	call keyt('radec',crval2,'dms',0.25*dpi)
	crpix1 = n1/2 + 1
	crpix2 = n2/2 + 1
c
	call GetOpt(totflux)
c
	call keyfin
c
c  If there is an input file, open it and get parameters about it.
c  Otherwise set the default parameters.
c
	if(Out.eq.' ')call bug('f','Output file name missing')
	if(In.ne.' ')then
	  call xyopen(lIn,in,'old',3,nsize)
	  n1 = nsize(1)
	  n2 = nsize(2)
 	  n3 = nsize(3)
	  if(nsize(3).ne.1)call bug('w','Crude handling of 3D images')
	  call rdhdi(lIn,'naxis',naxis,1)
	  naxis = min(naxis,MAXNAX)
	  do i=4,naxis
	    nsize(i) = 1
	  enddo
	  call rdhdr(lIn,'bmaj',bmaj,0.)
	  call rdhdr(lIn,'bmin',bmin,0.)
	  call rdhdr(lIn,'bpa',bpa,0.)
	  call rdhdd(lIn,'cdelt1',cdelt1,1d0*cdelt1)
	  call rdhdd(lIn,'cdelt2',cdelt2,1d0*cdelt2)
	else
	  naxis = 2
	  nsize(1) = n1
	  nsize(2) = n2
	  nsize(3) = n3
	  lIn = 0
	  bmaj = 0
	  bmin = 0
	  bpa = 0
	  if(n1.le.0.or.n2.le.0)call bug('f','Image size error')
	endif
	if(n1.gt.MAXDIM)call bug('f','Image dimension too big')
c
c  If we have a single gaussian object, use this as the beam
c  parameters.
c
	if(nobjs.eq.1.and.objs(1).eq.'gaussian'.and..not.
     *		totflux.and.abs(bmaj*bmin).eq.0)then
	  if(fwhm1(1).gt.fwhm2(1))then
	    bmaj = fwhm1(1)
	    bmin = fwhm2(1)
	    bpa  = 180/pi * posang(1)
	  else
	    bmaj = fwhm2(1)
	    bmin = fwhm1(1)
	    bpa  = 180/pi * posang(1) - 90
	  endif
	  if(bpa.lt.-90)bpa = bpa + 180
	  if(bpa.gt. 90)bpa = bpa - 180
	endif
c
c  Now open the output, and add a header to it.
c
	call xyopen(lOut,Out,'new',naxis,nsize)
	call header(lIn,lOut,crpix1,crpix2,crval1,crval2,cdelt1,cdelt2,
     *	  bmaj,bmin,bpa,version)
c
c  Convert to units that we want, namely x and y in grid coordinates
c  and fwhm in pixels.
c
	call coInit(lOut)
c
c  Fiddle fwhm and position angle parameters to be with respect to the 
c  pixel grid.
c
	do k=1,n3
	  if(lIn.ne.0)call xysetpl(lIn,1,k)
	  call xysetpl(lOut,1,k)
c
c  Convert the offsets and gaussian parameters from world to pixel units.
c
	  do i=1,nobjs
	    x1(1) = x(i)
	    x1(2) = y(i)
	    x1(3) = k
	    call coCvt(lOut,'ow/ow/p',x1,'ap/ap/ap',x2)
	    xd(i) = x2(1)
	    yd(i) = x2(2)
	    if(fwhm1(i)*fwhm2(i).gt.0)then
	      call coGauCvt(lOut,'ow/ow/p',x1,
     *	        'w',fwhm1(i), fwhm2(i), posang(i),
     *	        'p',fwhm1d(i),fwhm2d(i),posangd(i))
	    else
	      fwhm1d(i) = 0
	      fwhm2d(i) = 0
	      posangd(i) = 0
	    endif
	  enddo
c
c  Convert the flux units.
c
	  if(totflux)then
	    if(abs(bmaj*bmin).gt.0)then
	      fac = 0.25*PI/log(2.0)*abs(bmaj*bmin/(cdelt1*cdelt2))
	    else
	      fac = 1
	    endif
	  else
	    fac = 0
	  endif
c
c  Do the real work.
c
	  do j=1,n2
	    call GetBuf(lIn,j,Buff,n1,factor)
	    do i=1,nobjs
	      call DoMod(j,objs(i),Buff,n1,amp(i),fwhm1d(i),fwhm2d(i),
     *                    posangd(i),xd(i),yd(i),fac)
	    enddo
	    call xywrite(lOut,j,Buff)
	  enddo
	enddo
c
c  Close up shop.
c
	if(lIn.ne.0)call xyclose(lIn)
	call xyclose(lOut)
	end
c************************************************************************
	subroutine GetOpt(totflux)
c
	implicit none
	logical totflux
c
c  Get extra processing options.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'totflux '/
c
	call options('options',opts,present,NOPTS)
	totflux = present(1)
	end
c************************************************************************
	subroutine GetBuf(lIn,j,Buff,n1,factor)
c
	implicit none
	integer lIn,j,n1
	real factor,Buff(n1)
c
c  Initialise a row.
c
c------------------------------------------------------------------------
	integer i
c
	if(lIn.eq.0.or.factor.eq.0)then
	  do i=1,n1
	    Buff(i) = 0
	  enddo
	else
	  call xyread(lIn,j,Buff)
	  do i=1,n1
	    Buff(i) = factor * Buff(i)
	  enddo
	endif
c
	end
c*******************************************************************
	subroutine header(lIn,lOut,crpix1,crpix2,crval1,crval2,
     *	  cdelt1,cdelt2,bmaj,bmin,bpa,version)
c
	implicit none
	integer lIn,lOut
        double precision crpix1,crpix2,cdelt1,cdelt2,crval1,crval2
	real bmaj,bmin,bpa
	character version*(*)
c
c  Make a header for the output image.
c
c------------------------------------------------------------------------
	integer nkeys
	parameter(nkeys=44)
	character line*64
	integer i
	character keyw(nkeys)*8
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *	  'crpix1  ','crpix2  ','crpix3  ','crpix4  ','crpix5  ',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *	  'epoch   ','ltype   ','lstart  ','lwidth  ',
     *	  'lstep   ','mask    ','niters  ','object  ','history ',
     *    'observer','obsra   ','obsdec  ','restfreq','telescop',
     *	  'vobs    ','cellscal','obstime ','pbfwhm  ','btype   ',
     *	  'pbtype  '/
c
c  Either create a new header, or copy the old one.
c
	if(lIn.eq.0)then
	  call wrhdd(lOut,'crpix1',crpix1)
	  call wrhdd(lOut,'crpix2',crpix2)
	  call wrhdd(lOut,'cdelt1',cdelt1)
	  call wrhdd(lOut,'cdelt2',cdelt2)
	  call wrhdd(lOut,'crval1',crval1)
	  call wrhdd(lOut,'crval2',crval2)
	  call wrhda(lOut,'ctype1','RA---SIN')
	  call wrhda(lOut,'ctype2','DEC--SIN')
	  if(bmaj*bmin.gt.0)then
	    call wrhda(lOut,'bunit','JY/BEAM')
	    call wrhdr(lOut,'bmaj',bmaj)
	    call wrhdr(lOut,'bmin',bmin)
	    call wrhdr(lOut,'bpa',bpa)
	  else
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
	subroutine DoMod(j0,object,Data,n1,amp,fwhm1,fwhm2,posang,x,y,
     *								fac)
c
	implicit none
	integer n1,j0
	character object*(*)
	real Data(n1)
        real amp,fwhm1,fwhm2,posang,x,y,fac
c
c  Add the contribution of a particular component.
c
c  Input:
c    fac	Flux adjustment.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer i,j,ymin,ymax,xmin,xmax,maxit,it
	real xx,yy,xp,yp,scale,cospa,sinpa,t,a,log2,limit,p,theta,sum
	real Buff(maxdim)
c
c  Externals.
c
	real j1xbyx
c
c  Add the new contribution.First the gaussian. Work out the region
c  where the exponential greater than exp(-25), and don't bother
c  processing those regions.
c
c  Note: pi/4/log(2) == 1.1331.
c
	if(object.eq.'gaussian')then
	  log2 = log(2.0)
	  if(fac.ne.0)then
	    a = fac * amp / (pi/4/log2 * fwhm1 * fwhm2)
	  else
	    a = amp
	  endif
	  cospa = cos(posang)
	  sinpa = sin(posang)
	  scale = 2. * sqrt(log2)
	  limit = 5/scale * max(fwhm1,fwhm2)
	  ymin = nint(y-limit)
	  ymax = nint(y+limit)
	  xmin = max(nint(x-limit),1)
	  xmax = min(nint(x+limit),n1)
	  if(ymin.le.j0.and.j0.le.ymax)then
	    yy = scale * (j0-y)
	    do i=xmin,xmax
	      xx = scale * (i-x)
              yp =  yy*cospa + xx*sinpa
              xp = -yy*sinpa + xx*cospa
              t = (xp*xp)/(fwhm2*fwhm2) + (yp*yp)/(fwhm1*fwhm1)
	      if(t.lt.25)data(i) = data(i) + a*exp(-t)
	    enddo
	  endif
c
c  Handle a J1(x)/x function.
c
	else if(object.eq.'j1x')then
	  scale = 3.83
	  if(fac.ne.0)then
	    a = fac * amp / (4*pi/scale/scale * fwhm1 * fwhm2)
	  else
	    a = amp
	  endif
	  cospa = cos(posang)
	  sinpa = sin(posang)
	  yy = scale * (j0-y)
	  do i=1,n1
	    xx = scale * (i-x)
            yp =  yy*cospa + xx*sinpa
            xp = -yy*sinpa + xx*cospa
            t = (xp*xp)/(fwhm2*fwhm2) + (yp*yp)/(fwhm1*fwhm1)
	    data(i) = data(i) + 2 * a * j1xbyx(sqrt(t))
	  enddo
c
c  Handle a comet.
c
	else if(object.eq.'comet')then
	  maxit = 50
	  yy = (j0-y)
	  do i=1,n1
	    xx = (i-x)
	    p = sqrt(xx*xx+yy*yy)
            sum = 0.
            do it = -maxit+1,maxit-1
              theta = it*pi/2./maxit
              sum = sum +
     *          exp(-p/fwhm1/(cos(theta)))*pi/2./(maxit-2)
            enddo
	    if(p.ne.0.)then
	      a = amp / p * sum 
	      data(i) = data(i) + a
	    endif
	  enddo
c
c  Handle a disk.
c
	else if(object.eq.'disk')then
	  if(fac.ne.0)then
	    a = fac * amp / (pi/4 * fwhm1 * fwhm2)
	  else
	    a = amp
	  endif
	  cospa = cos(posang)
	  sinpa = sin(posang)
	  limit = 0.5 * max(fwhm1,fwhm2)
	  ymin = nint(y-limit)
	  ymax = nint(y+limit)
	  xmin = max(nint(x-limit),1)
	  xmax = min(nint(x+limit),n1)
	  if(ymin.le.j0.and.j0.le.ymax)then
	    yy = (j0-y)
	    do i=xmin,xmax
	      xx = (i-x)
              yp =  yy*cospa + xx*sinpa
              xp = -yy*sinpa + xx*cospa
              t = (xp*xp)/(fwhm2*fwhm2) + (yp*yp)/(fwhm1*fwhm1)
	      if(t.lt.0.25) data(i) = data(i) + a
	    enddo
	  endif 
c
c  Handle a spherical shell.
c
	else if(object.eq.'shell')then
	  if(fac.ne.0)then
	    a = fac * amp / (pi * sqrt(fwhm1 * fwhm1))
	  else
	    a = amp
	  endif
	  cospa = cos(posang)
	  sinpa = sin(posang)
	  limit = 0.5 * max(fwhm1,fwhm1)
	  ymin = nint(y-limit)
	  ymax = nint(y+limit)
	  xmin = max(nint(x-limit),1)
	  xmax = min(nint(x+limit),n1)
	  if(ymin.le.j0.and.j0.le.ymax)then
	    yy = (j0-y)
	    do i=xmin,xmax
	      xx = (i-x)
              yp =  yy*cospa + xx*sinpa
              xp = -yy*sinpa + xx*cospa
              t = (xp*xp)/(fwhm1*fwhm1) + (yp*yp)/(fwhm1*fwhm1)
	      if(t.lt.0.25) data(i) = data(i) + a/0.5/fwhm1/
      *            sqrt(1.-4.*t)
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
	  if(j.eq.j0.and.i.ge.1.and.i.le.n1)
     *		Data(i) = Data(i) + Amp
c
c  Should never get here.
c
	else
	  call bug('f','Unknown object type')
	endif
	end
