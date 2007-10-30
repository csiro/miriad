c************************************************************************
	program  offaxis
	implicit none
c
c= offaxis -- Remove (or simulate) ATCA off-axis instrumental polarisation.
c& rjs
c: uv analysis
c+
c	OFFAXIS is a Miriad task which attempts to remove off-axis
c	instrumental polarisation from ATCA observations. As input,
c	OFFAXIS takes a visibility dataset and a model of the source
c	total intensity (Stokes I). Given this, it computes the expected
c	off-axis effects (given its internal models of the effects),
c	subtracts this from the data, and produces an output (nominally)
c	corrected dataset.
c
c	Currently this only handle 13-cm off-axis effects!!
c@ vis
c	Input visibility dataset. No default.
c@ select
c	Normal visibility selection parameter. The default is to
c	select all data. See the help on "select" for more information.
c@ line
c	Normal line parameter with the normal defaults. See the help on
c	"line" for more information.
c@ model
c	Input model of the total intensity image. No default. This should
c	be the apparent total intensity (i.e. not corrected for primary
c	beam attenuation).
c@ out
c	The name of the output, corrected, visibility dataset. No default.
c@ clip
c	Pixels in the model less (in absolute value) than the clip level
c	are treated as if they are 0. The default is 0.
c@ options
c	Task enrichment parameters. Several can be given, separated by
c	commas. Only the minimum number of characters to guarantee
c	uniqueness are needed. Possible options are:
c	  replace   Normally OFFAXIS subtracts the computed off-axis
c	            response from the data. This option causes the
c	            visibility to be replaced with the model.
c	  nocal     Do not apply any antenna gain calibration. The default
c	            is to apply these if they are available.
c	  nopol     Do not apply any polarization leakage correction. The
c	            default is to apply these if they are available.
c	  nopass    Do not apply bandpass calibration. The default is to
c	            apply these calibrations if they are available.
c--
c  History:
c    rjs  02may96 Original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	character version*(*)
	parameter(version='Offaxis: version 1.0 2-May-96')
c
	logical replace
	integer nx,ny,nsize(2),tMod,tVis,tOut,nchan,i,j
	integer pFlux,pll,pmm,pRad,pPsi
	integer nCmp
	real clip,chi
	character uvflags*16,model*64,ltype*16,out*64
	complex data(MAXCHAN,4),sim(MAXCHAN,4)
	logical flags(MAXCHAN,4),first
	double precision sfreq(MAXCHAN),preamble(5)
c
c  Externals.
c
	character itoaf*8
	logical uvDatOpn
c
c  Get and check the inputs.
c
	call output(version)
	call keyini
	call GetOpt(replace,uvflags)
	call uvDatInp('vis',uvflags)
	call keya('model',model,' ')
	call keya('out',out,' ')
	call keyr('clip',clip,0.)
	call keyfin
c
	if(clip.lt.0)call bug('w','Clip level set to 0')
	clip = max(clip,0.)
	if(model.eq.' ')call bug('f','An input model must be given')
	if(out.eq.' ')call bug('f','An output dataset must be given')
c
c  We must process all the 4 polarisation parameters.
c
	call uvDatSet('stokes',-5)
	call uvDatSet('stokes',-6)
	call uvDatSet('stokes',-7)
	call uvDatSet('stokes',-8)
c
c  Open the model, get the components.
c
	call xyopen(tMod,model,'old',2,nsize)
	nx = nsize(1)
	ny = nsize(2)
c
	call memAlloc(pFlux,nx*ny,'r')
	call memAlloc(pll,nx*ny,'r')
	call memAlloc(pmm,nx*ny,'r')
	call GetMod(tMod,nx,ny,clip,nx*ny,
     *	  nCmp,memr(pFlux),memr(pll),memr(pmm))
	call output('Number of components found: '//itoaf(nCmp))
c
	call xyclose(tMod)
c
c  Compute the radius and position angle of each component.
c
	call memAlloc(pRad,nCmp,'r')
	call memAlloc(pPsi,nCmp,'r')
	call ToPolar(nCmp,memr(pll),memr(pmm),memr(pRad),memr(pPsi))
c
c  Now we want to process the visibility data.
c
	first = .true.
	dowhile(uvDatOpn(tVis))
	  call uvDatGta('ltype',ltype)
	  call VarInit(tVis,ltype)
	  if(first)then
	    first = .false.
	    call uvopen(tOut,out,'new')
	    call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	    call hdcopy(tVis,tOut,'history')
	    call hisOpen(tOut,'append')
	    call hisWrite(tOut,'OFFAXIS: Miriad '//version)
	    call hisInput(tOut,'OFFAXIS')
	    call hisClose(tOut)
	    call uvputvri(tOut,'npol',4,1)
	    call wrhdi(tOut,'npol',4)
	  endif
	  call VarOnit(tVis,tOut,ltype)
c
	  call uvDatRd(preamble,data(1,1),flags(1,1),MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    call uvDatRd(preamble,data(1,2),flags(1,2),MAXCHAN,nchan)
	    call uvDatRd(preamble,data(1,3),flags(1,3),MAXCHAN,nchan)
	    call uvDatRd(preamble,data(1,4),flags(1,4),MAXCHAN,nchan)
	    call uvinfo(tVis,'sfreq',sfreq)
c
c  Generate the simulated stuff.
c
	    call uvgetvrr(tVis,'chi',chi,1)
	    call Compute(sfreq,preamble,sim,nchan,MAXCHAN,chi,
     *	     memr(pFlux),memr(pll),memr(pmm),memr(pRad),memr(pPsi),nCmp)
c
c  Difference or replace it now.
c
	    do j=1,4
	      if(replace)then
	        do i=1,nchan
		  data(i,j) = sim(i,j)
	        enddo
	      else
		do i=1,nchan
		  data(i,j) = data(i,j) - sim(i,j)
		enddo
	      endif
	    enddo
c
c  Finish up and go back for more.
c
	    call VarCopy(tVis,tOut)
	    do i=1,4
	      call uvputvri(tOut,'pol',-4-i,1)
	      call uvwrite(tOut,preamble,data(1,i),flags(1,i),nchan)
	    enddo
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvDatCls
	enddo
c
c  All said and done.
c
	call memFree(pRad,nCmp,'r')
	call memFree(pPsi,nCmp,'r')
	call memFree(pFlux,nx*ny,'r')
	call memFree(pll,nx*ny,'r')
	call memFree(pmm,nx*ny,'r')
	call uvclose(tOut)
	end
c************************************************************************
	subroutine Compute(sfreq,uv,sim,nchan,maxchan,chi,
     *	  				Flux,ll,mm,Rad,Psi,nCmp)
c
	implicit none
	integer nchan,maxchan,nCmp
	complex sim(maxchan,4)
	double precision sfreq(nchan),uv(2)
	real Flux(nCmp),ll(nCmp),mm(nCmp),Rad(nCmp),Psi(nCmp),chi
c
c  Compute the expected response for a particular component.
c
c  Inputs:
c    sfreq	Sky frequency of each channel, in GHz.
c    uv		UV coordinates, in nanosec.
c    nchan	Number of channels.
c    maxchan	Dimension of the sim array.
c    Flux	Flux density of each component.
c    ll,mm	Cartesian coordinate of each component.
c    rad,psi	Polar coordinate of the component (both radians).
c    chi	Paralactic angle (radians).
c  Output:
c    sim	Expected response to the given field.
c------------------------------------------------------------------------
	include 'mirconst.h'
	complex Jo(2,2),XX,YY,XY,YX,t,W
	real theta,pb
	integer i,j
c
	do j=1,nchan
	  XX = 0
	  YY = 0
	  XY = 0
	  YX = 0
	  do i=1,nCmp
	    call GetJones(rad(i),psi(i)-chi,sfreq(j),Jo,pb)
	    theta = 2*PI*sfreq(j)*(uv(1)*ll(i) + uv(2)*mm(i))
	    W = Flux(i) / pb * cmplx(cos(theta),sin(theta))
	    XX = XX + W * ( real(Jo(1,1))**2 + aimag(Jo(1,1))**2 +
     *			    real(Jo(1,2))**2 + aimag(Jo(1,2))**2 - pb)
	    YY = YY + W * ( real(Jo(2,2))**2 + aimag(Jo(2,2))**2 +
     *			    real(Jo(2,1))**2 + aimag(Jo(2,1))**2 - pb)
	    t =  Jo(1,1)*conjg(Jo(2,1)) + conjg(Jo(2,2))*Jo(1,2)
	    XY = XY + W * t
	    YX = YX + W * conjg(t)
	  enddo
	  sim(j,1) = XX
	  sim(j,2) = YY
	  sim(j,3) = XY
	  sim(j,4) = YX
	enddo
c
	end
c************************************************************************
	subroutine GetJones(rad,psi,freq,Jo,pb)
c
	implicit none
	real rad,psi,pb
	double precision freq
	complex Jo(2,2)
c
c  Compute the corresponding Jones matrix at a particular position in the
c  primary beam.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
c  Alpha2 = 4*log(2).
c
	real alpha2
	parameter(alpha2=2.772589)
c
	integer i
	real rdist,x(7),px,py
	real coeff(2,7)
	save coeff
	data coeff/
     *  1.39602,        0.000000,
     *  6.87069E-02,    0.963600,
     * -8.92810E-02,    5.82036E-02,
     * -8.14360E-03,    5.98567E-03,
     *  4.41056E-02,   -1.98040E-02,
     *  5.25764E-02,   -3.98546E-02,
     * -2.01919E-02,    1.48593E-02/
c
c  Compute the coefficients of the trig functions in the Jones
c  matrix. Also compute the primary beam response.
c
	rdist =  rad / ( 2.368/freq * 20.9882*PI/180/60 )
	x(1) = exp(-alpha2*(rdist/coeff(1,1))**2)
	x(2) = coeff(1,2)*sin(0.5*PI*rdist/coeff(2,2))**2
	pb = x(1)*x(1) + 0.5*x(2)*x(2)
	do i=3,7
	  x(i) = (coeff(2,i)*rdist + coeff(1,i))*rdist
	  pb = pb + 0.5*x(i)*x(i)
	enddo
c
c  Now compute the actual Jones matrix.
c
	px = psi
	py = psi - 1.5*PI
	Jo(1,1) = x(1) + x(2)*cos(2*px) + x(3)*cos(px) + x(4)*sin(px)
	Jo(2,2) = x(1) + x(2)*cos(2*py) + x(3)*cos(py) + x(4)*sin(py)
	Jo(1,2) =   cmplx(x(5),x(6))*sin(px) + cmplx(x(7),0.)*cos(px)
        Jo(2,1) = -(cmplx(x(5),x(6))*sin(py) + cmplx(x(7),0.)*cos(py))
c
	end
c************************************************************************
	subroutine GetMod(tMod,nx,ny,clip,maxCmp,nCmp,Flux,ll,mm)
c
	implicit none
	integer tMod,nx,ny,maxCmp,nCmp
	real clip,Flux(maxCmp),ll(maxCmp),mm(maxCmp)
c
c  Get all those components above a particular clip level.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	real Data(MAXDIM)
	double precision cdelt1,cdelt2,crpix1,crpix2
	logical flags(MAXDIM)
c
	if(nx.gt.MAXDIM)call bug('f','Image too big for me')
c
	call rdhdd(tMod,'cdelt1',cdelt1,0.d0)
	call rdhdd(tMod,'cdelt2',cdelt2,0.d0)
	call rdhdd(tMod,'crpix1',crpix1,dble(nx/2+1))
	call rdhdd(tMod,'crpix2',crpix2,dble(ny/2+1))
	if(cdelt1.eq.0.or.cdelt2.eq.0)
     *	  call bug('f','Pixel increments missing from the dataset')
c
	nCmp = 0
	do j=1,ny
	  call xyread(tMod,j,data)
	  call xyflgrd(tMod,j,flags)
	  do i=1,nx
	    if(flags(i).and.abs(data(i)).gt.clip.and.
     *	      (nint(i-crpix1).ne.0.or.nint(j-crpix2).ne.0))then
	      nCmp = nCmp + 1
	      if(nCmp.gt.maxCmp)call bug('f','Too many components')
	      Flux(nCmp) = data(i)
	      ll(nCmp) = cdelt1*(i-crpix1)
	      mm(nCmp) = cdelt2*(j-crpix2)
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine ToPolar(nCmp,ll,mm,Rad,Psi)
c
	implicit none
	integer nCmp
	real ll(nCmp),mm(nCmp),Rad(nCmp),Psi(nCmp)
c
c  Convert the (l,m) coordinates to the polar form used to determine
c  the Jones matrix.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nCmp
	  Rad(i) = sqrt(ll(i)*ll(i) + mm(i)*mm(i))
	  Psi(i) = atan2(ll(i),mm(i))
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(replace,uvflags)
c
	implicit none
	character uvflags*(*)
	logical replace
c
c  Get processing options.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=4)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'replace ','nocal   ','nopol   ','nopass  '/
c
	call options('options',opts,present,NOPTS)
	replace = present(1)
c
	uvflags = 'dl3'
	if(.not.present(2))uvflags(4:4) = 'c'
	if(.not.present(3))uvflags(5:5) = 'e'
	if(.not.present(4))uvflags(6:6) = 'f'
c
	end
