c************************************************************************
	program views
	implicit none
c
c  History:
c    rjs  Dark-ages Original version.
c    rjs   7nov89   Added 's' flag to BoxSet.
c    rjs  30apr90   Changed call sequence to BoxInput.
c    bpw  28jan91   Include standard keywords
c    rjs   3apr92   Uses memalloc. Standardised history.
c    rjs   8mar93   Standardise history again?
c
c= VIEWS - Generate a projection of the datacube
c& bpw
c: visual display
c+
c	VIEWS is a MIRIAD task which generates projections of a cube, viewed
c	as if the cube was rotating around its y axis. The view is also
c	from an angle "phi" to the x-z plane. The output is intended to be
c	fed into a tvmovie program, to view a rotating cube. The projection is
c	a thresholding operation -- pixels values less than the threshold are
c	transparent, a pixel greater than the threshold is opaque, with a
c	brightness given by the pixel value.
c< in
c< region
c< out
c@ phi
c	The elevation angle, with respect to the x-z plane, of the observer
c	(degrees). This must be in the range [0,90]. Default is 30.
c@ aspect
c	Aspect ratio of the different axes. Two values can be given, giving
c	the y/x aspect ratio, and the z/x aspect ratio. Default should be
c	fine.
c@ thresh
c	Threshold used in determining which details appear in the output.
c	Default is the average of the image minima and maxima.
c@ imsize
c	Size of the output cube. Default is 128x128x64 (should be adequate).
c--
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Views: version 1.0 3-Apr-92')
	include 'maxdim.h'
	integer maxboxes
        parameter(maxboxes=2048)
	character in*64,out*64
	real theta,phi,aspect(2),cdelt1,cdelt2,thresh,bmin,bmax
	integer outsize(3),insize(3),indsize(3),blc(3),trc(3),i,j,k
	integer tin,tout
	integer pCube,pPlane
	integer boxes(maxboxes)
	logical default
c
c  Commons for dynamic memory.
c
	real ref(MAXBUF)
	common ref
c
c  Externals.
c
	logical keyprsnt
c
c  Get the input parameters.
c
        call output( version )
	call keyini
	call keya('in',in,' ')
	call boxinput('region',in,boxes,maxboxes)
	call keya('out',out,' ')
	call keyr('phi',phi,30.)
	default = .not.keyprsnt('thresh')
	if(.not.default)call keyr('thresh',thresh,0.)
	call keyi('imsize',outsize(1),128)
	call keyi('imsize',outsize(2),outsize(1))
	call keyi('imsize',outsize(3),32)
	call keyr('aspect',aspect(1),0.)
	call keyr('aspect',aspect(2),0.)
	call keyfin
c
c  Check the parameters.
c
	if(in.eq.' '.or.out.eq.' ')
     *	  call bug('f','Input or output file names missing')
	if(phi.le.0.or.phi.ge.90)
     *	  call bug('f','Bad projection angle')
	if(min(outsize(1),outsize(2),outsize(3)).le.1)
     *	  call bug('f','Bad output image size')
c
c  Open the input image and set up the trc and blc.
c
	call xyopen(tin,in,'old',3,insize)
c
	call boxset(boxes,3,insize,'s')
	call boxinfo(boxes,3,blc,trc)
	do i=1,3
	  indsize(i) = trc(i) - blc(i) + 1
	enddo
	if(insize(1).gt.maxdim)
     *	  call bug('f','Input image too big to handle')
c
c  Allocate memory.
c
	call MemAlloc(pCube,indsize(1)*indsize(2)*indsize(3),'r')
	call MemAlloc(pPlane,outsize(1)*outsize(2),'r')
c
c  Determine the default aspect ratio and threshold. Also get the image
c  min and max.
c
	if(aspect(1).le.0)then
	  call rdhdr(tin,'cdelt1',cdelt1,1.)
	  call rdhdr(tin,'cdelt2',cdelt2,1.)
	  aspect(1) = abs( (indsize(2)*cdelt2)/(indsize(1)*cdelt1) )
	  if(aspect(1).gt.2.5.or.aspect(1).lt.0.4) aspect(1) = 1
	endif
	if(aspect(2).le.0) aspect(2) = sqrt(aspect(1))
c
	call ImMinMax(tin,3,insize,bmin,bmax)
	if(default) thresh = 0.5*(bmin+bmax)
c
c  Read the input image.
c
	call GetIn(tin,ref(pCube),indsize(1),indsize(2),indsize(3),
     *	  blc,bmax)
c
c  Calculate and write the output image, one plane at a time.
c
	call xyopen(tout,out,'new',3,outsize)
	call hdout(tin,tout,360./outsize(3),thresh,phi,version)
	do k=1,outsize(3)
	  theta = 360./outsize(3) * (k-1)
	  call CalcOut(ref(pCube),indsize(1),indsize(2),indsize(3),
     *	    bmin,thresh,phi,theta,aspect(1),aspect(2),
     *	    ref(pPlane),outsize(1),outsize(2))
	  call xysetpl(tout,1,k)
	  i = pPlane
	  do j=1,outsize(2)
	    call xywrite(tout,j,ref(i))
	    i = i + outsize(2)
	  enddo
	enddo
c
c  All done.
c
	call MemFree(pCube,indsize(1)*indsize(2)*indsize(3),'r')
	call MemFree(pPlane,outsize(1)*outsize(2),'r')
	call xyclose(tout)
	call xyclose(tin)
c
	end
c************************************************************************
	subroutine hdout(tin,tout,cdelt3,thresh,phi,version)
c
	implicit none
	integer tin,tout
	real cdelt3,thresh,phi
	character version*(*)
c
c  Create the header of the output image.
c
c  Inputs:
c    tin	Handle of the input cube.
c    tout	Handle of the output cube.
c    cdelt3	Increment between views, in degrees.
c    thresh	Threshold used in determining projection.
c    phi	Projection angle.
c    version	Program version.
c
c------------------------------------------------------------------------
	integer nkeys
	parameter(nkeys=10)
	integer i
	character keyw(nkeys)*8,card*72
	data keyw/   'bunit   ','date-obs','epoch   ','history ',
     *	  'instrume','niters  ','object  ','telescop','xshift  ',
     *	  'yshift  '/
c
c  Copy crap from the input to the output header.
c
	do i=1,nkeys
	  call hdcopy(tin,tout,keyw(i))
	enddo
c
	call wrhdr(tout,'crval3',0.)
	call wrhdr(tout,'crpix3',1.)
	call wrhdr(tout,'cdelt3',cdelt3)
	call wrhda(tout,'ctype3','ROTATION')
c
	call hisopen(tOut,'append')
	Card = 'VIEWS: Miriad '//version
	call hiswrite(tOut,card)
	call hisinput(tOut,'VIEWS')
	write(Card,10)thresh,phi
   10	format('VIEWS: Threshold =',1pe10.3,' Phi =',0pf4.0)
	call hiswrite(tOut,card)
	call hisclose(tOut)
	end
c************************************************************************
	subroutine GetIn(tno,in,nx,ny,nz,blc,bmax)
c
	implicit none
	integer tno,nx,ny,nz,blc(3)
	real in(nx,ny,nz),bmax
c
c  Read in the input image. The edges of the input are set to "bmax",
c  so that when they are displayed, they appear as struts around the 
c  enclosed image.
c
c  The code commented out adds a wire cube at the edges of the input cube.
c
c  Inputs:
c    tno	Handle of the input image.
c    nx,ny,nz	Dimensions of the portion of the input to select.
c    insize	Dimensions of the input image.
c    blc	Bottom left corner of the image to select.
c    bmax	Image max.
c  Output:
c    in		The portion of the input image selected.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k
	real data(maxdim)
c
	do k=1,nz
	  call xysetpl(tno,1,k+blc(3)-1)
	  do j=1,ny
c	    if((k.eq.1.or.k.eq.nz).and.(j.eq.1.or.j.eq.ny))then
c	      do i=1,nx
c		in(i,j,k) = bmax
c	      enddo
c	    else
	      call xyread(tno,j+blc(2)-1,data)
	      do i=1,nx
	        in(i,j,k) = data(i+blc(1)-1)
	      enddo
c	      if(k.eq.1.or.k.eq.nz.or.j.eq.1.or.j.eq.ny)then
c		in(1,j,k) = bmax
c		in(nx,j,k) = bmax
c	      endif
c	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine CalcOut(in,nx,ny,nz,bmin,thresh,phi,theta,
     *							a1,a2,out,np,nq)
c
	implicit none
	integer nx,ny,nz,np,nq
	real theta,phi,bmin,thresh,a1,a2
	real in(nx,ny,nz),out(np,nq)
c
c  Calculate the projection of the input cube, at a particular angle.
c  This takes an input cube, adjusts the aspect ratio, then rotates and
c  projects it onto a plane.
c
c  The algorithm use is, for a given (p,q), determine the corresponding
c  (x,y,z) at various depths (values of r), until we hit a pixel above
c  the threshold, or we pass through the cube. This is far from a good
c  algorithm, and should be improved, as this is the rate determining step.
c
c  Inputs:
c    in		Input cube.
c    nx,ny,nz	Dimensions of the input cube.
c    bmin	Image minimum.
c    thresh	Threshold used in determining the output.
c    phi	Elevation viewing angle (degrees).
c    theta	Rotation viewing angle (degrees).
c    a1,a2	Aspect ratios. Before rotation and elevation, the input
c		cube is stretched to give it aspect rations of 1:a1:a2.
c    np,nq	Dimensions of the output projection.
c
c  Output:
c    out	The output projection.
c
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	real px,pz,p0,qx,qy,qz,q0,rx,rz,xp,xr,yp,yq,yr,zp,zr
	real theta0,phi0,scale,t,r,rdelt,rmin,rmax
	integer x,y,z,p,q
	logical more
c
	theta0 = pi/180 * theta
	phi0   = pi/180 * phi
c
c  Constants to convert the (x,y,z) grid indices to (p,q) indices. r is a 
c  depth measure.
c
c    p = px*x        + pz*z + p0
c    q = qx*x + qy*y + qz*z + q0
c    r = rx*x        + rz*z
c
	t = sqrt(1+a2*a2)
	scale = 0.95/max(t,a1*cos(phi0)+t*sin(phi0))
c
	px =    (np-1)*scale*cos(theta0)	  /(nx-1)
	pz = a2*(np-1)*scale*sin(theta0)	  /(nz-1)
	p0 = 0.5*( (np+1) - px*(nx+1) - pz*(nz+1) )
c
	qx =   -(nq-1)*scale*sin(theta0)*sin(phi0)/(nx-1)
	qy = a1*(nq-1)*scale*cos(phi0)		  /(ny-1)
	qz = a2*(nq-1)*scale*cos(theta0)*sin(phi0)/(nz-1)
	q0 = 0.5*( (nq+1) - qx*(nx+1) - qy*(ny+1) - qz*(nz+1) )
c
	rx =   -sin(theta0)/(nx-1)
	rz = a2*cos(theta0)/(nz-1)
c
c  Calculate the inverse transform coefficients.
c
c    x = xp*(p-p0)	       + xr*r
c    y = yp*(p-q0) + yq*(q-q0) + yr*r
c    z = zp*(p-p0)	       + zr*r
c
	t = (px*rz - rx*pz)
	xp =  rz/t
	xr = -pz/t
	yp = -(qx*rz-rx*qz)/(qy*t)
	yq =  1/qy
	yr = -(px*qz-qx*pz)/(qy*t)
	zp = -rx/t
	zr =  px/t
c
c  Set the increment to follow in r, and the start and end points.
c
	if(rx.lt.0)then
	  rmin = rx*nx
	  rmax = rx
	else
	  rmin = rx
	  rmax = rx*nx
	endif
	if(rz.lt.0)then
	  rmin = rmin + rz*nz
	  rmax = rmax + rz
	else
	  rmin = rmin + rz
	  rmax = rmax + rz*nz
	endif
	rdelt = 0.8*max( abs(rx), abs(rz) )
	rdelt = (rmax-rmin)/nint((rmax-rmin)/rdelt+1)
c
c  Generate the output image.
c
	do q=1,nq
	  do p=1,np
	    r = rmin
	    more = .true.
	    dowhile(r.lt.rmax.and.more)
	      x = nint( xp*(p-p0)	      + xr*r )
	      y = nint( yp*(p-p0) + yq*(q-q0) + yr*r )
	      z = nint( zp*(p-p0)             + zr*r )
	      if(x.ge.1.and.x.le.nx.and.y.ge.1.and.y.le.ny.and.
     *		 z.ge.1.and.z.le.nz)
     *		 more = thresh.gt.in(x,y,z)
	      r = r + rdelt
	    enddo
c
	    if(more)then
	      out(p,q) = bmin
	    else
	      out(p,q) = in(x,y,z)
	    endif
	  enddo
	enddo
c
	end

