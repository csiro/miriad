c************************************************************************
	subroutine mcInitFG(tno1,bmaj1,bmin1,bpa1)
c
	implicit none
	integer tno1
	real bmaj1,bmin1,bpa1
c
c  Initialise the mosaic convolver routnes, ready to subtract off a
c  gaussian.
c------------------------------------------------------------------------
	include 'mc.h'
c
	call mcInitF(tno1)
	dogaus = .true.
	bmaj = bmaj1
	bmin = bmin1
	bpa  = bpa1
	end
c************************************************************************
	subroutine mcInitF(tno1)
c
	implicit none
	integer tno1
c
c  Initialise the mosaic convolver routines.
c
c  Input:
c    tno1	Handle of the beam dataset.
c    flags	Flags, as for CnvlIniF
c
c------------------------------------------------------------------------
	include 'mc.h'
	integer npnt1,k,nx2,ny2
	double precision crpix1,crpix2
c
c  Externals.
c
	integer nextpow2
c
	tno = tno1
c
	call rdhdi(tno,'naxis1',n1,0)
	call rdhdi(tno,'naxis2',n2,0)
	call rdhdi(tno,'naxis3',npnt,0)
	if(n1.le.0.or.n2.le.0.or.npnt.le.0)
     *	  call bug('f','Something wrong with beam dataset')
	if(npnt.gt.MAXPNT)call bug('f','Too many pointings for me')
c
	call rdhdd(tno,'crpix1',crpix1,0.d0)
	call rdhdd(tno,'crpix2',crpix2,0.d0)
	ic = nint(crpix1)
	jc = nint(crpix2)
	if(abs(crpix1-ic).gt.0.01.or.abs(crpix2-jc).gt.0.01)
     *	  call bug('f','Beam reference pixel is not aligned on a pixel')
	if(abs(ic-n1/2).gt.2.or.abs(jc-n2/2).gt.2)call bug('f',
     *	  'Reference pixel is not at the centre of the beam')
c
c  Load the mosaic table.
c
	call mosLoad(tno,npnt1)
	call mosInfo(nx2,ny2,npnt1)
	if(npnt1.ne.npnt)
     *	  call bug('f','Inconsistent number of pointings')
c
c  Set the convolver handles to zero.
c
	do k=1,npnt
	  cnvl(k) = 0
	enddo
c
c  Guess the size of the convolver.
c
	flags = 's'
	n1d = nextpow2(n1)
	n2d = nextpow2(n2)
	if(n1d.lt.4*nx2.or.n2d.lt.4*ny2)then
	  flags = 'se'
	  n1d = n1d + n1d
	  n2d = n2d + n2d
	endif
	nWrk = n1d*n2d
	call memAlloc(pWrk1,2*nWrk,'r')
	pWrk2 = pWrk1 + nWrk
c
c  Initialise a few other things.
c
	mosini = .false.
	dogaus = .false.
	nWts = 0
	end
c************************************************************************
	subroutine mcPlane(coObj,k)
c
	implicit none
	integer coObj,k
c
c  Initialise for convolving a plane.
c------------------------------------------------------------------------
	include 'mc.h'
c
	if(mosini)call mosMFin
	call mosMini(coObj,real(k))
	mosini = .true.
	end
c************************************************************************
	subroutine mcCnvl(In,nix,niy,Out,nox,noy)
c
	implicit none
	integer nix,niy,nox,noy
	real In(nix,niy),Out(nox,noy)
c
c  Convolve a plane.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'mc.h'
	integer i,j,k,xoff,yoff,pbObj,mnx,mny
	integer xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax
	real Wts3
c
c  Externals.
c
	real mosWt3
	integer mosPb
c
c  Allocate weight arrays if needed.
c
	if(nWts.lt.nox*noy)then
	  if(nWts.gt.0)call memFree(pWts1,2*nWts,'r')
	  nWts = nox*noy
	  call memAlloc(pWts1,2*nWts,'r')
	  pWts2 = pWts1 + nWts
	endif
c
c  Get the weights to apply to the data.
c
	xoff = nox/2 - nix/2
	yoff = noy/2 - niy/2
	call mosWts(memr(pWts1),memr(pWts2),nox,noy,xoff,yoff)
c
c  Zero the output array.
c
	do j=1,noy
	  do i=1,nox
	    Out(i,j) = 0
	  enddo
	enddo
c
c  Do the real work.
c
	do k=1,npnt
	  Wts3 = mosWt3(k)
	  pbObj = mosPb(k)
c
c  Determine the region we are interested in.
c
	  call mcExtent(k,pbObj,n1,n2,n1d,n2d,
     *	    xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax)
	  xmin = max(xmin,1-xoff)
	  ymin = max(ymin,1-yoff)
	  xmax = min(xmax,nox-xoff)
	  ymax = min(ymax,noy-yoff)
c
	  if(xlo.lt.1)  xlo = xmin
	  if(xhi.gt.nix)xhi = xmax
	  if(ylo.lt.1)  ylo = ymin
	  if(yhi.gt.niy)yhi = ymax
c
	  mnx = xhi - xlo + 1
	  mny = yhi - ylo + 1
c
c  Aloocate memory, if needed.
c
	  if(xmin.le.xmax.and.ymin.le.ymax)then
	    if(nWrk.lt.mnx*mny)then
	      if(nWrk.gt.0)call memFree(pWrk1,2*nWrk,'r')
	      nWrk = mnx*mny
	      call memAlloc(pWrk1,2*nWrk,'r')
	      pWrk2 = pWrk1 + nWrk
	    endif
c
c  Convolve.
c
	    call mcCnvl1(k,cnvl(k),pbObj,In,nix,niy,
     *	      Out,memr(pWts1),nox,noy,
     *	      Wts3,xoff,yoff,xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax,
     *	      memr(pWrk1),memr(pWrk2),mnx,mny)
	  endif
	enddo
c
c  Apply the final weights.
c
	call mcWt(Out,memr(pWts2),nox*noy)
c
	end
c************************************************************************
	subroutine mcCnvl1(k,cnvl,pbObj,In,nix,niy,Out,Wts1,
     *	  nox,noy,Wts3,xoff,yoff,xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax,
     *	  Resid,Pb,mnx,mny)
c
	implicit none
	integer k,cnvl,pbObj,nix,niy,nox,noy,xoff,yoff
	integer xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax,mnx,mny
	real In(nix,niy),Out(nox,noy),Wts1(nox,noy),Wts3
	real Resid(mnx,mny),Pb(mnx,mny)
c
c  Add to the output the contribution from this particular field.
c
c------------------------------------------------------------------------
	integer i,j,ioff,joff
	real t
c
c  Externals.
c
	real pbGet
c
	if(mnx.ne.xhi-xlo+1.or.mny.ne.yhi-ylo+1)
     *	  call bug('f','Buffer too small in mcCnvl')
	ioff = 1 - xlo
	joff = 1 - ylo
c
c  Get the data and primary beam that we are interested in.
c
	do j=ylo,yhi
	  if(j.gt.1.and.j.le.niy)then
	    do i=xlo,0
	      Resid(i+ioff,j+joff) = 0
	      Pb(i+ioff,j+joff) = pbGet(pbObj,real(i),real(j))
	    enddo
	    do i=max(1,xlo),min(nix,xhi)
	      t = pbGet(pbObj,real(i),real(j))
	      Pb(i+ioff,j+joff) = t
	      Resid(i+ioff,j+joff) = In(i,j)*t*Wts1(i+xoff,j+yoff)
	    enddo
	    do i=nix+1,xhi
	      Pb(i+ioff,j+joff) = pbGet(pbObj,real(i),real(j))
	      Resid(i+ioff,j+joff) = 0
	    enddo
	  else
	    do i=xlo,xhi
	      Pb(i+ioff,j+joff) = pbGet(pbObj,real(i),real(j))
	      Resid(i+ioff,j+joff) = 0
	    enddo
	  endif
	enddo
c
c  Convolve it.
c
	if(cnvl.eq.0)call mcInitC(k,cnvl)
	call cnvla(cnvl,Resid,mnx,mny,Resid,'c')
c
c  Add the contribution to the output.
c
	do j=ymin,ymax
	  do i=xmin,xmax
	    Out(i+xoff,j+yoff) = Out(i+xoff,j+yoff) + 
     *		Wts3 * Resid(i+ioff,j+joff)*Pb(i+ioff,j+joff)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine mcPlaneR(coObj,k,Runs,nRuns)
c
	implicit none
	integer coObj,k,nRuns,Runs(3,nRuns)
c
c  Initialise the mosaic convolution routines for a particular plane.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'mc.h'
c
	integer iRuns
c
c  Release any old primary beams, if needed.
c
	if(mosini)call MosMFin
c
c  Initialise the mosaicing routines.
c
	call mosMini(coObj,real(k))
	mosini = .true.
c
c  Count the number of pixels.
c
	npix = 0
	do iRuns=1,nRuns
	  npix = npix + Runs(3,iRuns) - Runs(2,iRuns) + 1
	enddo
c
c  Do we have enough buffer space?
c
	if(nWts.lt.npix)then
	  if(nWts.gt.0)call memFree(pWts1,2*nWts,'r')
	  nWts = npix
	  call memAlloc(pWts1,2*nWts,'r')
	  pWts2 = pWts1 + npix
	endif
c
c  Compute the mosaicing weights.
c
	call mosWtsR(Runs,nRuns,memr(pWts1),memr(pWts2),npix)
c
	end
c************************************************************************
	subroutine mcCnvlR(In,Runs,nRuns,Out)
c
	implicit none
	integer nRuns,Runs(3,nRuns+1)
	real In(*),Out(*)
c
c  Convolve a model with the mosaicing PSF.
c
c  Input:
c    In		The input model to be convolved.
c    Out	The convolved result.
c    Runs	The runs describing the region to be convolved.
c    nRuns	The number of runs.
c    npix	The number of pixels.
c  Output:
c    Out	The convolved model.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'mc.h'
c
	integer i,k,xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax,pbObj
	real Wts3
c
c  Externals.
c
	integer mosPb
	real mosWt3
c
c  Zero the output array.
c
	do i=1,npix
	  Out(i) = 0
	enddo
c
c  Do the real work.
c
	do k=1,npnt
	  Wts3 = mosWt3(k)
	  pbObj = mosPb(k)
	  call mcExtent(k,pbObj,n1,n2,n1d,n2d,
     *	    xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax)
	  call mcCnvl2(k,cnvl(k),pbObj,In,memr(pWts1),
     *	    Wts3,xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax,
     *	    npix,Out,Runs,nRuns,memr(pWrk1),memr(pWrk2),nWrk)
	enddo
c
c  Apply some extra weights.
c
	call mcWt(Out,memr(pWts2),npix)
c
	end
c************************************************************************
	subroutine mcCnvl2(k,cnvl,pbObj,In,Wt1,Wt3,
     *	  xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax,
     *	  n,Out,Runs,nRuns,Pb,Resid,nscr)
c
	implicit none
	integer cnvl,pbObj,n,nRuns,Runs(3,nRuns),nscr,k
	integer xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax
	real In(n),Out(n),Wt1(n),Wt3,Pb(nscr),Resid(nscr)
c
c  Add the contribution to the output residual image of this particular
c  field.
c
c  Inputs:
c    cnvl	The synthesised beam convolver.
c    pbObj	The primary beam object.
c    In		The model image.
c    Wt1	Renormalise a model component by this before doing
c		anything.
c    Scale,Wt2	The weight of a pixel in the output is Scale*Wt2(i)
c    Runs,nRuns	The runs array, describing the region of interest.
c    nscr	Size of the scratch arrays.
c
c    xlo,ylo)	Region to be included in the convolution operation.
c    xhi,yhi)
c
c    xmin,ymin)	Region to be included in the mosaiced output.
c    xmax,ymax)
c  Scratch:
c    Pb,Resid	Used to hold the primary beam and residuals for this
c		plane.
c  Input/Output:
c    Out	The dirty image.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXRUNS
	parameter(MAXRUNS=3*MAXDIM)
	integer Runs2(3,MAXRUNS),indx(MAXRUNS),nRuns2
	integer xa,xb,ix,iy,ncomp
	integer xoff,yoff,npix,nin,nout,mnx,mny,iRuns,x1,x2,y1,y2
	real wrk(MAXDIM)
c
c  Externals.
c
	real pbGet
c
c  Determine the runs which are within this region.
c
	xoff = xlo - 1
	npix = 0
	nout = 0
	ncomp = 0
	nRuns2 = 0
	do iruns=1,nRuns
	  if(Runs(1,iRuns).ge.ylo.and.Runs(1,iRuns).le.yhi.and.
     *	     Runs(3,iRuns).ge.xlo.and.Runs(2,iRuns).le.xhi)then
c
c  Determine the range where the primary beam is non-zero.
c
	    x1 = max(xlo,Runs(2,iRuns))
	    x2 = min(xhi,Runs(3,iRuns))
	    if(x2.gt.MAXDIM)call bug('f','Buffer overflow, in ???')
	    xa = x2 + 1
	    xb = x1 - 1
	    iy = Runs(1,iRuns)
	    nin = npix + x1 - Runs(2,iRuns)
	    do ix=x1,x2
	      Wrk(ix) = pbGet(pbObj,real(ix),real(iy))
	      if(Wrk(ix).gt.0)then
	        xa = min(ix,xa)
	        xb = max(ix,xb)
	      endif
	    enddo
c
c  If there are some good pixels, copy them across, applying primary beam and the weighting
c  as we go.
c
	    if(xa.le.xb)then
	      if(nout.eq.0)yoff = Runs(1,iRuns) - 1
	      nRuns2 = nRuns2 + 1
	      if(nRuns2.ge.MAXRUNS)call bug('f','Runs buffer overflow')
	      Runs2(1,nRuns2) = Runs(1,iRuns) - yoff
	      Runs2(2,nRuns2) = xa - xoff
	      Runs2(3,nRuns2) = xb - xoff
	      nin = npix + xa - Runs(2,iRuns)
	      indx(nRuns2) = nin
	      if(xb-xa+1+nout.gt.nscr)call bug('f','Too many pixels')
	      do ix=xa,xb
		nout = nout + 1
		nin  = nin + 1
		Pb(nout) = Wrk(ix)
		Resid(nout) = Pb(nout) * In(nin) * Wt1(nin)
		if(abs(Resid(nout)).gt.0)ncomp = ncomp + 1
	      enddo
	    endif
	  endif
	  npix = npix + Runs(3,iRuns) - Runs(2,iRuns) + 1
	enddo
	Runs2(1,nRuns2+1) = 0
c
	if(n.eq.0)return
c
c  Initialise the convolver if needed.
c
	if(cnvl.eq.0)call mcInitC(k,cnvl)
c
c  Convolve with the synthesised beam.
c
	mnx = xhi - xlo + 1
	mny = Runs2(1,nRuns2)
	call cnvlr(cnvl,Resid,mnx,mny,Runs2,nRuns2,Resid,'c')
c
c  Add the contribution of interest.
c
	x1 = xmin - xoff
	x2 = xmax - xoff
	y1 = ymin - yoff
	y2 = ymax - yoff
c
	npix = 0
	do iRuns=1,nRuns2
	  if(Runs2(1,iRuns).ge.y1.and.Runs2(1,iRuns).le.y2.and.
     *	     Runs2(3,iRuns).ge.x1.and.Runs2(2,iRuns).le.x2)then
	    xa = max(x1,Runs2(2,iRuns))
	    xb = min(x2,Runs2(3,iRuns))
	    nin  = npix + xa - Runs2(2,iRuns)
	    nout = indx(iRuns) + xa - Runs2(2,iRuns)
	    do ix=xa,xb
	      nin = nin + 1
	      nout = nout + 1
	      Out(nout) = Out(nout) + Resid(nin) * Pb(nin) * Wt3
	    enddo
	  endif
	  npix = npix + Runs2(3,iRuns) - Runs2(2,iRuns) + 1
	enddo
c
	end
c************************************************************************
	subroutine mcWt(Out,Wts,n)
c
	implicit none
	integer n
	real Out(n),Wts(n)
c
c  Apply the final weights.
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  Out(i) = Wts(i) * Out(i)
	enddo
c
	end
c************************************************************************
	subroutine mcExtent(k,pbObj,n1,n2,n1d,n2d,
     *	  xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax)
c
	implicit none
	integer pbObj,n1,n2,n1d,n2d,k
	integer xlo,ylo,xhi,yhi,xmin,ymin,xmax,ymax
c
c  Determine the extent of this sub-image that should be added into
c  the final mosaic, and the sub-portion of the image that needs to
c  be convolved.
c
c  Input:
c    pbObj	The primary beam object.
c    n1,n2	Size of the beam.
c    n1d,n2d	Padded size of the beam -- which limits the biggest region
c		we can convolve.
c
c  Output:
c    xlo,ylo)	Region that should be included in the
c    xhi,yhi)	convolution.
c
c    xmin,ymin) Region that should be included in the
c    xmax,ymax)	final output.
c------------------------------------------------------------------------
	real x0,y0,xrad,yrad
	integer x1,y1
c
c  Determine the extent that is to be added into the final output.
c
	call mosExt(k,xmin,xmax,ymin,ymax)
c
c  Determine the area to convolve. We make it as small as possible.
c  Its size is set by
c    * the extent where the primary beam is non-zero (radius (xrad,yrad)),
c    * the size of the beam function that we are convolving with (n1,n2), and
c    * the maximum size that the convolution routines impose (n1d,n2d)
c
	call pbExtent(pbObj,x0,y0,xrad,yrad)
c
	xlo = nint(x0 - xrad + 0.5)
	xhi = nint(x0 + xrad - 0.5)
	x1 = nint(x0)
	xlo = max(xlo, xmin-n1/2,     x1-n1d/2)
	xhi = min(xhi, xmax+(n1-1)/2, x1+(n1d-1)/2)
c
	ylo = nint(y0 - yrad + 0.5)
	yhi = nint(y0 + yrad - 0.5)
	y1 = nint(y0)
	ylo = max(ylo, ymin-n2/2,     y1-n2d/2)
	yhi = min(yhi, ymax+(n2-1)/2, y1+(n2d-1)/2)
c
	end
c************************************************************************
	subroutine mcInitC(k,cnvl1)
c
	implicit none
	integer k,cnvl1
c
c  Initialise a convolver. We wait until the last minute to do this
c  to avoid unnecessarily doing it if its not necessary.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	include 'mc.h'
	integer pGaus
c
c  If we have a gaussian, do it first.
c
	call xysetpl(tno,1,k)
	if(doGaus)then
	  call memAlloc(pGaus,n1*n2,'r')
	  call mcGaus(tno,memr(pGaus),n1,n2,ic,jc,bmaj,bmin,bpa)
	  call cnvlIniA(cnvl1,memr(pGaus),n1,n2,ic,jc,0.,flags)
	  call memFree(pGaus,n1*n2,'r')
	else
	  call cnvlIniF(cnvl1,tno,n1,n2,ic,jc,0.,flags)
	endif
c
	end
c************************************************************************
	subroutine mcGaus(tno,Beam,n1,n2,ic,jc,bmaj,bmin,bpa)
c
	implicit none
	integer tno,n1,n2,ic,jc
	real Beam(n1,n2),bmaj,bmin,bpa
c
c  Get the beam less a gaussian.
c------------------------------------------------------------------------
	integer i,j
	real cospa,sinpa,scale,xx,yy,xp,yp,t
c
	cospa = cos(bpa)
	sinpa = sin(bpa)
	scale = 2*sqrt(log(2.0))
c
	do j=1,n2
	  call xyread(tno,j,Beam(1,j))
	  yy = scale * (j-jc)
	  do i=1,n1
	    xx = scale * (i-ic)
	    yp =  yy*cospa + xx*sinpa
	    xp = -yy*sinpa + xx*cospa
	    t = (xp*xp)/(bmin*bmin) + (yp*yp)/(bmaj*bmaj)
	    if(t.lt.25)Beam(i,j) = Beam(i,j) - exp(-t)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine mcFin
c
	implicit none
c
c  Tidy up the mosaic convolution routines.
c
c------------------------------------------------------------------------
	include 'mc.h'
	integer k
c
	if(mosini)call mosMFin
	do k=1,npnt
	  if(cnvl(k).ne.0)call cnvlFin(cnvl(k))
	enddo
c
	if(nWrk.gt.0)call memFree(pWrk1,2*nWrk,'r')
	if(nWts.gt.0)call memFree(pWts1,2*nWts,'r')
	end
