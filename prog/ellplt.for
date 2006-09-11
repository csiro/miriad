c**********************************************************************c
        program ellplt
        implicit none
c
c= ELLPLT - Plot points in elliptical annuli.
c& rjs
c: image analysis
c+
c	ELLPLT plots the values of a Miriad image along ellipses.
c@ in
c	Input image name. No default.
c@ region
c	Region of image to be displayed. See the help on "region" for more
c	information.
c@ center
c	The offset of the center of the annuli in arcsec (or natural units
c	if options=natural is used) offset from the reference pixel.
c@ pa
c	Position angle of ellipse major axis in degrees. Default is 0 (north).
c@ incline
c	The ellipse is assumed to be a circular structure that appears
c	ellitpical because it is viewed at some inclination. The "incline"
c	parameter gives this inclination angle in degrees. Default=0. (face on)
c@ radius
c	This gives one or two values, determining the radii of the major
c	axis of the annulus of interest. If one value is given, then this
c	is taken as the radius at the center of the annulus, and the annulus
c	is 5% wide. If two values are given, these are taken as the inner
c	and outer radii of the annulus. The units are (by default) arcsec,
c	but can be natural units if options=natural is used. No default.
c@ range
c	Min and max greyscale values for the greyscale plot. The default is
c	the image minimum and maximum.
c@ device
c	Plotting device. No default.
c@ options
c	Task enrichment options.  Minimum match is active.
c	  natural   Assume keywords "center" and "radius" are in natural
c	            units rather than arcsec.
c--
c  History:
c    rjs   06mar97	Adapted from ellint.
c----------------------------------------------------------------------c
	include 'mirconst.h'
	include 'maxdim.h'
	include 'mem.h'
c
        character*(*) version
        parameter(version='version 1.0 06-Mar-97')
        integer maxboxes
        parameter(maxboxes=2048)
c
	real pa,incline,rmin,rmax,cospa,sinpa,cosi,zmin,zmax
        integer boxes(maxboxes),i,blc(3),trc(2),nsize(2),lIn,nx,ny
	character in*64,cin*2,xlab*24,ylab*24,device*64
	logical natural
	double precision center(2),cdelt(2),crpix(2),rts
c
	integer MAXPNT
	parameter(MAXPNT=1000000)
	integer npnt,mpnt
	integer pxval,pyval,pval,ptheta,parray
c
c  Externals.
c
	character itoaf*8
	integer pgbeg
c
c Get inputs.
c
        call output( 'ELLPLT: '//version )
        call keyini
        call keya('in',in,' ')
        if(in .eq. ' ') call bug('f','No input specified.')
        call boxinput('region',in,boxes,maxboxes)
        call keyd('center',center(1),0.)
        call keyd('center',center(2),0.)
        call keyr('pa',pa,0.)
        call keyr('incline',incline,0.)
        call keyr('radius',rmin,0.)
	if(rmin.le.0)call bug('f','Invalid value for radius')
        call keyr('radius',rmax,rmin)
	if(rmax.eq.rmin)then
	  rmin = rmin - 0.025*rmin
	  rmax = rmax + 0.025*rmax
	endif
	call keyr('range',zmin,0.)
	call keyr('range',zmax,zmin)
	call keya('device',device,' ')
	if(device.eq.' ')call bug('f',
     *	  'A plotting device must be given')
        call getopt(natural)
        call keyfin
c
c  Open the input.
c
        call xyopen(lIn,in,'old',2,nsize)
        if(nsize(1).gt.maxdim)call bug('f','Input file too big for me')
c
c  Set up the region of interest.
c
        call boxmask(lIn,boxes,maxboxes)
        call boxset(boxes,2,nsize,'s')
        call boxinfo(boxes,2,blc,trc)
c
c  Get the image min and max if needed.
c
	if(zmin.eq.zmax)call ImMinMax(lIn,2,nsize,zmin,zmax)
	if(zmin.eq.zmax)call bug('f','All image pixels are identical')
c
c  Get center and pixel size from image.
c
        do i=1,2
          cin = itoaf(i)
          call rdhdd(lIn,'crpix'//cin,crpix(i),0.)
          call rdhdd(lIn,'cdelt'//cin,cdelt(i),0.)
          if(i.le.2)then
            if(crpix(i).eq.0)then
              crpix(i) = nsize(i)/2+1
              call bug('w','Center pixel missing - assume naxis/2+1')
            endif
            if(cdelt(i).eq.0)call bug('f','Pixel size missing')
          endif
        enddo
	call rdhda(lIn,'ctype1',xlab,'Offset RA (arcsec)')
	if(xlab(1:4).eq.'RA--')xlab = 'Offset RA (arcsec)'
	call rdhda(lIn,'ctype2',ylab,'Offset DEC (arcsec)')
	if(ylab(1:4).eq.'DEC-')ylab = 'Offset DEC (arcsec)'
c
c  Convert the inputs to more useful numbers, and defaults.
c
	if(natural)then
	  rts = 1
	else
	  rts = 3600.*180./PI
	endif
c
        cospa = cos(pa*pi/180.)
        sinpa = sin(pa*pi/180.)
        cosi = cos(incline*pi/180.)
c
	nx = trc(1) - blc(1)
	ny = trc(2) - blc(2)
	mpnt = min(nx*ny,MAXPNT)
	call memAlloc(pxval,mpnt,'r')
	call memAlloc(pyval,mpnt,'r')
	call memAlloc(pval,mpnt,'r')
	call memAlloc(ptheta,mpnt,'r')
	call memAlloc(pArray,nx*ny,'r')
c
c  Load the points.
c
	call Load(lIn,memr(pArray),nx,ny,blc(1),blc(2),
     *	  rts*cdelt(1),rts*cdelt(2),crpix(1),crpix(2),
     *	  center(1),center(2),
     *	  memr(pxval),memr(pyval),memr(ptheta),memr(pval),mpnt,npnt,
     *	  cospa,sinpa,cosi,rmin,rmax)
c
c  Plot the data.
c
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Error opening graphics device')
	endif
	call pgscf(2)
c
	call Plotit(memr(pArray),nx,ny,rts*cdelt(1),rts*cdelt(2),
     *	  crpix(1)-blc(1)+1,crpix(2)-blc(2)+1,center(1),center(2),
     *	  zmin,zmax,xlab,ylab,
     *	  memr(pxval),memr(pyval),memr(ptheta),memr(pval),npnt)
c
	call pgend
        call xyclose(lin)
c
	end
c************************************************************************
	subroutine Plotit(Array,nx,ny,dx,dy,x0,y0,xc,yc,zmin,zmax,
     *	  xlab,ylab,xval,yval,theta,val,npnt)
c
	implicit none
	integer nx,ny,npnt
	real Array(nx,ny),zmin,zmax
	real xval(npnt),yval(npnt),theta(npnt),val(npnt)
	double precision dx,dy,x0,y0,xc,yc
	character xlab*(*),ylab*(*)
c------------------------------------------------------------------------
	real xmin,xmax,ymin,ymax,zlo,zhi,minv,maxv,tr(6)
	integer symbol,mark,i
c
	if(npnt.lt.100)then
	  symbol = 17
	else
	  symbol = 1
	endif
	mark = 2
c
	xmin = (1-x0)*dx - xc
	xmax = (nx-x0)*dx - xc
	ymin = (1-y0)*dy - yc
	ymax = (ny-y0)*dy - yc
c
	call pgsvp(0.1,0.9,0.42,0.9)
	call pgwnad(xmin,xmax,ymin,ymax)
	tr(1) = xmin - dx
	tr(2) = dx
	tr(3) = 0
	tr(4) = ymin - dy
	tr(5) = 0
	tr(6) = dy
	call pggray(Array,nx,ny,1,nx,1,ny,zmax,zmin,tr)
	call pgsci(mark)
	call pgpt(npnt,xval,yval,symbol)
	call pgsci(1)
	call pgbox('BCMST',0.,0,'BCNST',0.,0)
	call pgmtxt('t',2.,0.5,0.5,xlab)
	call pgmtxt('l',2.,0.5,0.5,ylab)
c
c  Determine the min and max intensity.
c
	maxv = val(1)
	minv = maxv
	do i=2,npnt
	  maxv = max(maxv,val(i))
	  minv = min(minv,val(i))
	enddo
	call pgrnge(minv,maxv,zlo,zhi)
c
	call pgsvp(0.1,0.9,0.1,0.4)
	call pgswin(-10.,370.,zlo,zhi)
	call pgsci(mark)
	call pgpt(npnt,theta,val,symbol)
	call pgsci(1)
	call pgbox('BCNST',0.,0,'BCNST',0.,0)
	call pglab('CML (degrees)','Intensity',' ')
	end
c************************************************************************
	subroutine Load(lIn,Data,nx,ny,i0,j0,dx,dy,x0,y0,xc,yc,
     *	  xval,yval,theta,val,maxpnt,npnt,cospa,sinpa,cosi,rmin,rmax)
c
	implicit none
	integer lIn,nx,ny,i0,j0,maxpnt,npnt
	double precision x0,y0,dx,dy,xc,yc
	real xval(maxpnt),yval(maxpnt),val(maxpnt),theta(maxpnt)
	real data(nx,ny),cospa,sinpa,cosi,rmin,rmax
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	integer i,j,id,jd
	real x,y,r,buf(MAXDIM)
	logical mask(MAXDIM)
c
c  Initialize integrals for each axis.
c
	npnt = 0
	jd = j0 - 1
        do j = 1,ny
	  jd = jd + 1
          call xyread(lin,jd,buf)
          call xyflgrd(lin,jd,mask)
          y = (jd-y0)*dy - yc
	  id = i0 - 1
          do i = 1,nx
	    id = id + 1
	    data(i,j) = buf(id)
            x = (id-x0)*dx - xc
            r = sqrt((y*cospa-x*sinpa)**2+((y*sinpa+x*cospa)/cosi)**2)
            if(r.ge.rmin.and.r.le.rmax.and.mask(id))then
	      npnt = npnt + 1
	      if(npnt.gt.MAXPNT)call bug('f','Too many points to plot')
	      if(x.eq.0.and.y.eq.0)then
	        theta(npnt) = 0
	      else
		theta(npnt) = 180/PI*atan2(-y,x)
		if(theta(npnt).lt.0)theta(npnt) = theta(npnt) + 360
	      endif
	      val(npnt) = buf(id)
	      xval(npnt) = x
	      yval(npnt) = y
	    endif
          enddo
        enddo
c
        end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine getopt(natural)
      implicit none
c
      logical natural
c
c  Decode options array into named variables.
c
c   Output:
c     natural	 Use natural units rather than arcsec.
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 1)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'natural '/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      natural = present(1)
c
      end
