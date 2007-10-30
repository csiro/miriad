c************************************************************************
	subroutine VisInit(model,nmod1,sfreq1,sdf1,nchan1,ra,dec)
c
	implicit none
	integer nchan1,nmod1
	character model(nmod1)*(*)
	real sfreq1,sdf1
	double precision ra,dec
c
c  Initialise the routine used to compute the visibilities.
c
c  Input:
c    model	Name of the sky model. The units of this must be
c		Kelvin, and the image size must be a power of 2.
c    sfreq,sdf,nchan
c		Description of the correlator setup.
c  Output:
c    ra,dec	Reference coordinate of the observation.
c------------------------------------------------------------------------
	include 'viscomp.h'
	include 'mirconst.h'
	real Tcmb
	parameter(Tcmb=2.7)
	integer nsize(2),nx,ny,i,ira,idec,tno1
	character ctype*16,bunit*16
	double precision crpix,cdelt1,cdelt2
	real f,hnuonkt,temp,lambda
c
	nmod = nmod1
	if(nmod.lt.1.or.nmod.gt.MAXMOD)call bug('f',
     *	  'Invalid number of models')
	sfreq = sfreq1
	sdf = sdf1
	nchan = nchan1
	if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	do i=1,nmod
	  call xyopen(tno1,model(i),'old',2,nsize)
	  if(i.eq.1)then
	    tno = tno1
	    call coInit(tno)
	    nx = nsize(1)
	    ny = nsize(2)
	    call rdhda(tno,'bunit',bunit,'K')
	    call ucase(bunit)
	    if(bunit.ne.'K'.and.bunit.ne.'KELVIN')
     *	      call bug('f','Unsupported brightness unit of input image')
c
	    nu = nx/2+1
	    nv = ny
	  endif
	  call memAlloc(pGrid(i),nu*nv,'c')
	  call VisFFT(tno1,nx,ny,memc(pGrid(i)),nu,nv,du,dv,u0,v0)
	  call xyclose(tno1)
	enddo
c
c  Get info about the image coordinate system.
c
	call coFindAx(tno,'ra',ira)
	call coFindAx(tno,'dec',idec)
	if(ira.ne.1.and.idec.ne.2)
     *		call bug('f','Image must be an RA/DEC one')
	call coAxGet(tno,ira,ctype,crpix,ra,cdelt1)
	call coAxGet(tno,idec,ctype,crpix,dec,cdelt2)
c
c  Compute the conversion factors from Kelvin to Jy. This is the
c  derivative of the Planck function evaluationed at the observing
c  frequency and the temperature of the CMB.
c
	do i=1,nchan
	  f = (sfreq + sdf*(i-1) ) * 1e9
	  hnuonkt = (HMKS*f)/(KMKS*Tcmb)
	  lambda = CMKS/f
	  temp = exp(hnuonkt)
	  fac(i) = 2*hnuonkt*hnuonkt*(KMKS*1e26)/lambda/lambda*
     *	    temp/(temp-1)/(temp-1)*abs(cdelt1*cdelt2)
	enddo
c	
	end
c************************************************************************
	subroutine VisFin
	implicit none
c------------------------------------------------------------------------
	integer i
	include 'viscomp.h'
c
	call coFin(tno)
	do i=1,nmod
	  call memFree(pGrid(i),nu*nv,'c')
	enddo
	end
c************************************************************************
	subroutine VisFFT(tno,nx,ny,Grid,nu,nv,du,dv,u0,v0)
c
	implicit none
	integer tno,nx,ny,nu,nv,u0,v0
	real du,dv
	complex Grid(nv,nu)
c
c  Form the Fourier transform of the image.
c
c  Inputs:
c    tno	Handle of the input image.
c    nx,ny	Size of the input image. Must be a power of 2.
c    nu,nv	Size of the output grid.
c		  nu = nx/2+1
c		  nv = ny
c  Output:
c    du,dv	Grid increment, in wavelengths.
c    u0,v0	Pixel coordinate of the origin of the Fourier plane.
c    Grid	Fourier transform of the input image.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real in(MAXDIM),rtemp(MAXDIM),temp,cdelt1,cdelt2,fac
	complex ctemp(MAXDIM)
	integer i,j,j0,xref,yref,nxd,nyd,offset
c
c  Externals.
c
	integer nextpow2
c
	if(nu.ne.nx/2+1.or.nv.ne.ny)
     *	  call bug('f','nu or nv has wrong value')
	nxd = nextpow2(nx)
	nyd = nextpow2(ny)
	if(nx.ne.nxd.or.ny.ne.nyd)
     *	  call bug('f','Image dimension must be a power of 2')
	u0 = 1
	v0 = ny/2+1
c
	call rdhdr(tno,'crpix1',temp,real(nx/2+1))
	xref = temp
	if(abs(temp-xref).gt.0.01)
     *		call bug('f','Image X reference not on pixel grid')
	call rdhdr(tno,'crpix2',temp,real(ny/2+1))
	yref = temp
	if(abs(temp-yref).gt.0.01)
     *		call bug('f','Image Y reference not on pixel grid')
c
	call rdhdr(tno,'cdelt1',cdelt1,0.)
	call rdhdr(tno,'cdelt2',cdelt2,0.)
	if(cdelt1*cdelt2.eq.0)
     *	  call bug('f','Missing pixel increments in the input')
	du = 1/(nx*cdelt1)
	dv = 1/(ny*cdelt2)
c
	fac = 1
	do j=1,ny
	  j0 = j + yref - 1
	  if(j0.gt.ny)j0 = j0 - ny
	  call xyread(tno,j0,in)
	  offset = xref - 1
	  do i=1,nx-xref+1
	    rtemp(i) = fac*in(i+offset)
	  enddo
	  offset = -(nx-xref+1)
	  do i=nx-xref+2,nx
	    rtemp(i) = fac*in(i+offset)
	  enddo
	  call fftrc(rtemp,ctemp,1,nx)
	  do i=1,nx/2+1
	    Grid(j,i) = ctemp(i)
	  enddo
	  fac = -fac
	enddo
c
	do i=1,nx/2+1
	  do j=1,ny
	    ctemp(j) = Grid(j,i)
	  enddo
	  call fftcc(ctemp,Grid(1,i),1,ny)
	enddo
c
c	call dumpc(Grid,nv,nu,'grid')
c
	end
c************************************************************************
	subroutine VisNext(ra,dec)
c
	implicit none
	double precision ra,dec
c
c  Get ready for the next pointing.
c
c  Inputs:
c    ra,dec	Input coordinates of the next pointing centre, in radians.
c------------------------------------------------------------------------
	include 'viscomp.h'
	double precision wcoeff(3),x1(2),x2(2)
c
	x1(1) = ra
	x1(2) = dec
	call coGeom(tno,'aw/aw',x1,ucoeff,vcoeff,wcoeff)
	call coCvt(tno,'aw/aw',x1,'op/op',x2)
	ll = x2(1)/du/(2*nu-1)
	mm = x2(2)/dv/nv
c
	end
c************************************************************************
	subroutine VisPnt(vis,nchan1,u,v,w,pbfunc,pbwidth)
c
	implicit none
	integer nchan1
	complex vis(nchan1)
	real pbwidth
	double precision u,v,w
c
	real pbfunc
	external pbfunc
c
c  Estimate an observed visibility.
c
c  Input:
c    nchan1	Number of channels.
c    u,v,w	uvw coordinates of the baseline, in metres.
c
c    pbwidth	Dish diameter, in metres.
c    pbfunc	Subroutine returning normalised illumination power pattern.
c
c  Output:
c    vis	Visibility, in Jy.
c------------------------------------------------------------------------
	include 'viscomp.h'
	include 'mirconst.h'
	integer i,id,j
	real lambda,ud,vd
c
	if(nchan.ne.nchan1)
     *	  call bug('f','Number of channels disagrees')
c
	id = 0
	do j=1,nmod
	  do i=1,nchan
	    id = id + 1
	    lambda = CMKS*1e-9/(sfreq+sdf*(i-1))
	    ud = ( u*ucoeff(1) + v*ucoeff(2) + w*ucoeff(3) ) / lambda
	    vd = ( u*vcoeff(1) + v*vcoeff(2) + w*vcoeff(3) ) / lambda
	    call VisPnt1(vis(id),ll,mm,ud,vd,pbfunc,pbwidth/lambda,
     *				   memc(pGrid(j)),nu,nv,du,dv,u0,v0)
	    vis(id) = vis(id) * fac(i) *
     *		(4*lambda/(PI*pbwidth))**2 * abs(du*dv)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine VisPnt1(vis,l,m,u,v,pbfunc,pbwidth,
     *					Grid,nu,nv,du,dv,u0,v0)
c
	implicit none
	integer nu,nv,u0,v0
	real l,m,u,v,pbwidth,du,dv
	complex vis,Grid(nv,nu)
	real pbfunc
	external pbfunc
c
c  Estimate an observed visibility.
c
c  Inputs:
c    nu,nv	Size of the Fourier plane grid.
c    Grid	Fourier plane grid.
c    du,dv	Grid increment, in wavelengths.
c    u0,v0	Pixel coordinate of the origin of the Fourier plane.
c    pbwidth    Antenna diameter, in wavelengths.
c    pbfunc	Function returning the autocorrelation of the illumination
c		pattern.
c    u,v	uv coordinate of the point of interest, in wavelengths
c    l,m	Direction cosines of the offset the pointing centre.
c
c  Output:
c    vis	Observed visibility.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer umin,umax,vmin,vmax,ju,jv
	real uu,vv,uval,vval,r,theta,dpu,dpv
	complex W
	logical conj
c
	conj = u*du.lt.0
	if(conj)then
	  uu = -u
	  vv = -v
	else
	  uu = u
	  vv = v
	endif
c
	dpu = abs(pbwidth/du)
	dpv = abs(pbwidth/dv)
	umin = nint(uu/du-dpu - 0.5)
	umax = nint(uu/du+dpu + 0.5)
	vmin = nint(vv/dv-dpv - 0.5)
	vmax = nint(vv/dv+dpv + 0.5)
c
c	call output('---------------------')
	vis = 0
	do ju=max(umin+u0,u0),min(umax+u0,nu)
	  do jv=max(vmin+v0,1),min(vmax+v0,nv)
	    uval = (ju-u0)*du
	    vval = (jv-v0)*dv
	    r = (uu-uval)**2 + (vv-vval)**2
	    if(r.lt.pbwidth*pbwidth)then
c	write(*,*)vis,Grid(jv,ju),pbfunc(sqrt(r)/pbwidth)
	      theta = 2*PI*(uval*l+vval*m)
	      W = cmplx(cos(theta),-sin(theta))
	      vis = vis + W*Grid(jv,ju)*pbfunc(sqrt(r)/pbwidth)
	    endif
	  enddo
	enddo
c
	do ju=max(-umax+u0,u0+1),min(-umin+u0,nu)
	  do jv=max(-vmax+v0,1),min(-vmin+v0,nv)
	    uval = -(ju-u0)*du
	    vval = -(jv-v0)*dv
	    r = (uu-uval)**2 + (vv-vval)**2
	    if(r.lt.pbwidth*pbwidth)then
	      theta = 2*PI*(uval*l+vval*m)
	      W = cmplx(cos(theta),-sin(theta))
	      vis = vis + W*conjg(Grid(jv,ju))*pbfunc(sqrt(r)/pbwidth)
	    endif
	  enddo
	enddo
c
	if(conj)vis=conjg(vis)
c
	end
c************************************************************************
c	subroutine dumpc(image,n1,n2,out)
cc
c	implicit none
c	integer n1,n2
c	complex image(n1,n2)
c	character out*(*)
cc------------------------------------------------------------------------
c	include 'maxdim.h'
c	real rp(MAXDIM),ip(MAXDIM)
c	integer nsize(2),i,j,tno1,tno2
cc
c	nsize(1) = n1
c	nsize(2) = n2
c	call xyopen(tno1,out//'.r','new',2,nsize)
c	call xyopen(tno2,out//'.i','new',2,nsize)
cc
c	do j=1,n2
c	  do i=1,n1
c	    rp(i) = real(image(i,j))
c	    ip(i) = aimag(image(i,j))
c	  enddo
c	  call xywrite(tno1,j,rp)
c	  call xywrite(tno2,j,ip)
c	enddo
c	call xyclose(tno1)
c	call xyclose(tno2)
c	end
