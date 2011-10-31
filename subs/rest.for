c************************************************************************
c  A set of routines to restore images.
c
c  History:
c    rjs  18nov94  Original version.
c    rjs   4dec94? Fix bug when dealing with non-MFS models and MFS beams.
c************************************************************************
	subroutine RestIni(lBeam,nx1,ny1,fwhm1,fwhm2,pa,mode)
c
	implicit none
	integer lBeam,nx1,ny1
	real fwhm1,fwhm2,pa
	character mode*(*)
c
c  Initialise the restoration routines.
c
c  Input:
c    lBeam
c    fwhm1,fwhm2,pa Gaussian beam parameters.
c    mode	Either 'clean', 'dirty' or 'convolve'.
c------------------------------------------------------------------------
	include 'rest.h'
	include 'maxdim.h'
	include 'mem.h'
c
	integer naxis1,naxis2,naxis3,ipnt,isdb,nxy,x0,y0,ic,jc
        ptrdiff pGaus
	double precision crpix1,crpix2
	real bmaj,bmin,bpa
c
	nx = nx1
	ny = ny1
c
c  Is the beam an mfs or mosaic beam?
c
	x0 = nx/2 + 1
	y0 = ny/2 + 1
	call coInit(lBeam)
	if(mode.eq.'convolve')then
	  naxis1 = nx
	  naxis2 = ny
	  naxis3 = 1
	  mfs = .false.
	  mosaic = .false.
	else
	  call coFindAx(lBeam,'pointing',ipnt)
	  call coFindAx(lBeam,'sdbeam',isdb)
	  call rdhdi(lBeam,'naxis1',naxis1,0)
	  call rdhdi(lBeam,'naxis2',naxis2,0)
	  call rdhdi(lBeam,'naxis3',naxis3,0)
	  call rdhdd(lBeam,'crpix1',crpix1,0.d0)
	  call rdhdd(lBeam,'crpix2',crpix2,0.d0)
	  ic = nint(crpix1)
	  jc = nint(crpix2)
	  mosaic = naxis3.gt.1.and.ipnt.eq.3
	  mfs    = naxis3.gt.1.and.isdb.eq.3
	  if(mfs.and.naxis3.gt.2)then
	    call bug('w',
     *	    'Only the I and I*alpha planes handled in mfs processing')
	    naxis3 = 2
	  endif
	endif
c
c  Convert the gaussian parameters.
c
	if(mode.ne.'dirty')
     *	  call coGauCvt(lBeam,'op',0.d0,'w',fwhm1,fwhm2,pa,
     *					'p',bmaj,bmin,bpa)
c
c  Get the gaussian.
c
	if(mosaic)then
	  nxy = 0
	else
	  nxy = nx*ny
	  call memAllop(pGaus,nxy,'r')
	  if(mode.eq.'clean'.or.mode.eq.'convolve')
     *	    call RestGaus(memr(pGaus),nx,ny,x0,y0,bmaj,bmin,bpa)
	endif
c
	if(mode.eq.'clean')then
	  if(mosaic)then
	    call mcInitFG(lBeam,bmaj,bmin,bpa)
	  else
	    call RestDiff(lBeam,memr(pGaus),naxis1,naxis2,nx,ny,ic,jc)
	    call CnvlIniA(cnvl1,memr(pGaus),nx,ny,x0,y0,0.,'s')
	  endif
	else if(mode.eq.'dirty')then
	  if(mosaic)then
	    call mcInitF(lBeam)
	  else
	    call RestGet(lBeam,memr(pGaus),naxis1,naxis2,nx,ny,ic,jc)
	    call CnvlIniA(cnvl1,memr(pGaus),nx,ny,x0,y0,0.,'s')
	  endif
	else if(mode.eq.'convolve')then
	  call CnvlIniA(cnvl1,memr(pGaus),nx,ny,x0,y0,0.,'s')
	endif
c
	if(mfs)then
	  call xysetpl(lBeam,1,2)
	  call RestGet(lBeam,memr(pGaus),naxis1,naxis2,nx,ny,ic,jc)
	  call CnvlIniA(cnvl2,memr(pGaus),nx,ny,x0,y0,0.,'s')
	endif
c
c  All said and done.
c
	if(nxy.gt.0)call memFrep(pGaus,nxy,'r')
	call coFin(lBeam)
c
	end
c************************************************************************
	subroutine RestGet(lBeam,Data,n1,n2,nx,ny,ic,jc)
c
	implicit none
	integer lBeam,n1,n2,nx,ny,ic,jc
	real Data(nx,ny)
c
c  Get the beam.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,xoff,yoff
	real Row(MAXDIM)
c
	if(n1.gt.MAXDIM)then
	  call bug('w','Internal array too small, in RestGet')
	  call bug('f','Recompile with a bigger value for MAXDIM')
	endif
c
	xoff = nx/2 + 1 - ic
	yoff = ny/2 + 1 - jc
c
	do j=1,ny
	  if(j-yoff.lt.1.or.j-yoff.gt.n2)then
	    do i=1,nx
	      Data(i,j) = 0
	    enddo
	  else
	    call xyread(lBeam,j-yoff,Row)
	    do i=1,xoff
	      Data(i,j) = 0
	    enddo
	    do i=max(xoff+1,1),min(xoff+n1,nx)
	      Data(i,j) = Row(i-xoff)
	    enddo
	    do i=xoff+n1+1,nx
	      Data(i,j) = 0
	    enddo
	  endif
	enddo
c
	end
c************************************************************************
	subroutine RestDiff(lBeam,Gaus,n1,n2,nx,ny,ic,jc)
c
	implicit none
	integer lBeam,n1,n2,nx,ny,ic,jc
	real Gaus(nx,ny)
c
c  Return the difference between the beam and the gaussian.
c
c  Input:
c    lBeam	The beam dataset.
c    n1,n2	Beam size.
c    ic,jc	Centre of the beam.
c    nx,ny	Output image size.
c  Input/Output:
c    Gaus	On input, this contains the CLEAN beam. On output,
c		it contains the dirty beam minus the CLEAN beam.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,xoff,yoff
	real Row(MAXDIM)
c
c  Check.
c
	if(n1.gt.MAXDIM)then
	  call bug('w','Internal array too small, in RestDiff')
	  call bug('f','Recompile with a bigger value for MAXDIM')
	endif
c
	xoff = nx/2 + 1 - ic
	yoff = ny/2 + 1 - jc
c
c  Difference the dirty beam from the CLEAN beam.
c
	do j=1,ny
	  if(j-yoff.lt.1.or.j-yoff.gt.n2)then
	    do i=1,nx
	      Gaus(i,j) = -Gaus(i,j)
	    enddo
	  else
	    call xyread(lBeam,j-yoff,Row)
	    do i=1,xoff
	      Gaus(i,j) = -Gaus(i,j)
	    enddo
	    do i=max(xoff+1,1),min(xoff+n1,nx)
	      Gaus(i,j) = Row(i-xoff) - Gaus(i,j)
	    enddo
	    do i=xoff+n1+1,nx
	      Gaus(i,j) = -Gaus(i,j)
	    enddo
	  endif
	enddo
c
	end
c************************************************************************
	subroutine RestGaus(Gaus,nx,ny,x0,y0,bmaj,bmin,bpa)
c
	implicit none
	integer nx,ny,x0,y0
	real Gaus(nx,ny),bmaj,bmin,bpa
c
c  Generate a gaussian.
c
c  Input:
c    bmaj,bmin	Major and minor axes, in pixels.
c    bpa	Gaussian position angle, in radians.
c    nx,ny	Size of the image.
c    x0,y0	Centre of the beam.
c  Output:
c    Gaus	The generated gaussian.
c
c------------------------------------------------------------------------
	integer i,j
	real cospa,sinpa,scale,xx,yy,xp,yp,t
c
	cospa = cos(bpa)
	sinpa = sin(bpa)
	scale = 2 * sqrt(log(2.0))
	do j=1,ny
	  yy = scale * (j-y0)
	  do i=1,nx
	    xx = scale * (i-x0)
	    yp =  yy*cospa + xx*sinpa
	    xp = -yy*sinpa + xx*cospa
	    t = (xp*xp)/(bmin*bmin) + (yp*yp)/(bmaj*bmaj)
	    if(t.lt.25)then
	      Gaus(i,j) = exp(-t)
	    else
	      Gaus(i,j) = 0
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine Restore(lModel,i,Out)
c
	implicit none
	integer lModel,i
	real Out(*)
c
c  Get the thing we want.
c  Input:
c    lModel
c  Output:
c    Out
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'rest.h'
	ptrdiff pDat
        integer naxis1,naxis2,naxis3
c
	call xysetpl(lModel,1,i)
	call rdhdi(lModel,'naxis1',naxis1,0)
	call rdhdi(lModel,'naxis2',naxis2,0)
	call rdhdi(lModel,'naxis3',naxis3,1)
c
	if(mosaic)then
	  call memAllop(pDat,naxis1*naxis2,'r')
	  call RestGet(lModel,memr(pDat),
     *	    naxis1,naxis2,naxis1,naxis2,naxis1/2+1,naxis2/2+1)
	  call mcPlane(lModel,i)
	  call mcCnvl(memr(pDat),naxis1,naxis2,Out,nx,ny)
	  call memFrep(pDat,naxis1*naxis2,'r')
	else
	  call CnvlF(cnvl1,lModel,naxis1,naxis2,Out,' ')
	endif
c
	if(mfs.and.naxis3.eq.2)then
	  call memAllop(pDat,nx*ny,'r')
	  call xysetpl(lModel,1,2)
	  call CnvlF(cnvl2,lModel,naxis1,naxis2,memr(pDat),' ')
	  call RestAdd(Out,memr(pDat),nx,ny)
	  call memFrep(pDat,nx*ny,'r')
	endif
c
	end
c************************************************************************
	subroutine RestAdd(Out,Dat,nx,ny)
c
	implicit none
	integer nx,ny
	real Out(nx,ny),Dat(nx,ny)
c
c------------------------------------------------------------------------
	integer i,j
c
	do j=1,ny
	  do i=1,nx
	    Out(i,j) = Out(i,j) + Dat(i,j)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine RestFin
	implicit none
c
c  Tidy up and end.
c------------------------------------------------------------------------
	include 'rest.h'
c
	if(mosaic)then
	  call mcFin
	else
	  call CnvlFin(cnvl1)
	  if(mfs)call CnvlFin(cnvl2)
	endif
c
	end
