c************************************************************************
c NOTE: This is one of the routines where maxdim.h can play a crucial role
c	because often there is not enough memory.
c
c	in ModMap: bufsize ~ 4.nx.ny.nz (nx,ny,nz = cubesize)
c
c  History:
c    rjs  13mar90 Original version.
c    rjs  29mar90 Fixed bug in calling of ModStat by ModMap.
c    rjs   9apr90 Planet flux calculation. Changed the name of some
c		  subroutines. Changed  TabFluxto CalGet
c    rjs  23apr90 Changed calinit to modinit.
c    rjs  26apr90 An extra check that cdelt1 and cdelt2 are present in the
c		  model header. Readded changes to ModPlant -- changes which
c		  apparently got lost somewhere along the way.
c    pjt   1may90 removed 'maxchan', assuming it's in maxdim.h
c    pjt   3may90 verbosity to bug calls
c    mchw 17jul90 increased frequency search range in CalGet since flux
c			table does not have entries for many freqencies.
c    rjs   2nov90 Fixed bug affecting points where u=0, in ModPlane.
c    mchw 19nov90 Added flag "h" to use image header for phase center.
c    rjs  16jan91 Check for a zero model.
c    rjs  23jan91 Fixed bug in ModFFT which crept in on 19nov90.
c************************************************************************
c*ModelIni -- Ready the uv data file for processing by the Model routine.
c&mchw
c:model
c+
	subroutine ModelIni(tmod,tvis,sels,flags)
c
	implicit none
	integer tmod,tvis
	character flags*(*)
	real sels(*)
c
c  This performs some standard setting up of the visibility data file,
c  ready for model calculation. This routine can easily be replaced
c  by a more appropriate user routine if necessary. In particular,
c  the steps performed are: rewind the uv data file, setup the line
c  type, resets the uv-selection criteria, and then "AND"s in a specification
c  to make sure we only get data with the same pointing centre. We skip
c  this pointing centre processing if we find that the visibility file
c  does not contain pointing offsets.
c
c  Inputs:
c    tmod	The handle of the model image.
c    tvis	The handle of the input visibility data.
c    sels	The uv selection intermediate array.
c    flags	A character variable, each character of which specifies
c		a processing stp.
c		 'l'	Set up the linetype.
c		 'p'	Set up the pointing parameters.
c--
c------------------------------------------------------------------------
	double precision pi,tol
	parameter(pi=3.141592653589793d0,tol=10.d0/3600.d0/180.d0*pi)
	double precision ra,dec,cosd
	logical update
	integer length,nchan
	real lstart,lwidth,lstep
	character tra*1,tdec*1,ltype*64
c
c  Rewind the uv data file and apply the user selection.
c
	call uvrewind(tvis)
c
c  Set up the linetype.
c
	if(index(flags,'l').ne.0)then
	  call rdhdi(tmod,'naxis3',nchan,1)
	  call rdhda(tmod,'ltype',ltype,'channel')
	  call rdhdr(tmod,'lstart',lstart,1.)
	  call rdhdr(tmod,'lwidth',lwidth,1.)
	  call rdhdr(tmod,'lstep',lstep,1.)
	  call uvset(tvis,'data',ltype,nchan,lstart,lwidth,lstep)
	endif
c
	call uvselect(tvis,'clear',0.d0,0.d0,.true.)
	call SelApply(tvis,sels,.true.)
c
c  Determine if the visibility file contains offset pointing parameters.
c
	if(index(flags,'p').ne.0)then
	  call uvprobvr(tvis,'dra',tra,length,update)
	  call uvprobvr(tvis,'ddec',tdec,length,update)
c
	  if(tra.eq.'r'.or.tdec.eq.'r')then
	    call rdhdd(tmod,'crval1',ra,0.d0)
	    call rdhdd(tmod,'crval2',dec,0.d0)
	    cosd = abs(cos(dec))
	    call uvselect(tvis,'and',0.d0,0.d0,.true.)
	    call uvselect(tvis,'ra',ra-tol*cosd,ra+tol*cosd,.true.)
	    call uvselect(tvis,'dec',dec-tol,dec+tol,.true.)
	  endif
	endif
	end
c************************************************************************
c*Model -- Calculate model visibilities, given a model image.
c& mchw
c:model
c+
	subroutine Model(flags,tvis,tmod,offset,level,tscr,
     *					nhead,header,nchan,nvis)
c
	implicit none
	character flags*(*)
	integer tmod,tvis,tscr,nchan,nhead,nvis
	real offset(2),level
	external header
c
c  Calculate the model data corresponding to a visibility data file.
c  The model can either be a point source, or a full map. If the model
c  is a full image cube, then the image is FFTed, and an interpolation
c  scheme is used to determine the corresponding visibility model.
c
c  Input:
c    tvis	The visibility data file. The caller can set up the uv file
c		using the uvselect and uvset routines.
c    tmod	The model image file. If this is 0, then the input model
c		is assumed to be a point source.
c    flags	This selects extra processing options. It is a character
c		string, each character of which has the following meaning:
c		 'c'  Calibration scaling. Look up the source in the
c		      calibrators file, to determine the flux of the source.
c		 'a'  Autoscale. After model calculation, the model is scaled
c		      so that it has the same power as the visibilities.
c		 'h'	Use image header for phase center.
c    offset	The offset, in arcsec, in RA and DEC, of the point
c		source model. This is only used if tmod.eq.0.
c    level	Either a clip level to apply to the data (tmod.ne.0), or
c		the amplitude of the point source (tmod.eq.0).
c    nhead	Number of "header" values to write out to the scratch file.
c		These are filled in by the "header" routine. If "nhead" is
c		zero, header is not called.
c    header	A service routine called after each visibility record
c		is processed. Arguments to this routine are:
c		  subroutine header(tvis,preamble,data,flags,nchan,
c		    accept,Out,nhead)
c		where
c		  Input:
c		    tvis	Handle of the visibility file.
c		    nhead	The value of nhead
c		    nchan	The number of channels.
c		  Input/Output(?):
c		    preamble	Preamble returned by uvread.
c		    data	A complex array of nchan elements, giving
c				the correlation data.
c		    flags	The data flags.
c		  Output:
c		    out		The nhead values to save with the data.
c		    accept	This determines whether the data is
c				accepted or discarded.
c  Output:
c    tscr	Output scratch file containing the data.
c    nchan	The number of channels in the scratch file.
c    nvis	The number of visibilities written to the scratch file.
c
c  In the output scratch file, there are nhead + 5*nchan values per
c  visibility record processed. The "nhead" values are filled in by
c  the "header" routine. The 5 values per channel consist of
c	real(vis),aimag(vis),real(model),aimag(model),flag
c  Where "vis" and "model" are the visibility and model data. "Flag" is
c  either positive or negative, indicating whether this visibility is
c  deemed good or bad.
c
c  Bugs and Shortcomings:
c    * The FFT of the entire cube must fit into memory.
c    * Disk models (planets) are not supported.
c    * Calibration scaling is not handled yet.
c    * It would be nice to give a frequency-independent model, and
c      replicate it for all planes. Strictly we should do some extra
c      stretching of the model at different frequencies.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer maxlen
	parameter(maxlen=5*maxchan+10)
c
	real Out(maxlen),a,VisPow,ModPow
	integer i,j,length
	logical calscale,imhead
c
	call ModInit
	call ScrOpen(tscr)
	call uvset(tvis,'coord','wavelength',0,0.,0.,0.)
	calscale = index(flags,'c').ne.0
	imhead = index(flags,'h').ne.0
c
	if(tmod.ne.0)then
	  call ModMap(calscale,tvis,tmod,level,tscr,nhead,header,
     *	    imhead,nchan,nvis,VisPow,ModPow)
	else
	  call ModPnt(calscale,tvis,offset,level,tscr,nhead,header,
     *	    nchan,nvis,VisPow,ModPow)
	endif
	if(ModPow.le.0) call bug('w','Model visibilities are all zero')
	if(VisPow.le.0) call bug('w','All visibilities are zero')
c
c  If we are to autoscale, then run through the output, rescaling all the
c  data so that power is conserved.
c
	if(index(flags,'a').ne.0)then
	  length = nhead + 5*nchan
	  if(length.gt.maxlen) call bug('f','MODEL: Buffer overflow')
	  if(ModPow.le.0)
     *	    call bug('f','Cannot autoscale to a zero model')
	  a = sqrt(VisPow/ModPow)
	  do j=1,nvis
	    call scrread(tscr,Out,(j-1)*length,length)
	    do i=nhead+1,nhead+5*nchan,5
	      Out(i+2) = a*Out(i+2)
	      Out(i+3) = a*Out(i+3)
	    enddo
	    call scrwrite(tscr,Out,(j-1)*length,length)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine ModMap(calscale,tvis,tmod,level,tscr,nhead,header,
     *	    imhead,nchan,nvis,VisPow,ModPow)
c
	implicit none
	logical calscale,imhead
	integer tvis,tscr,nhead,nchan,nvis,tmod
	real level,ModPow,VisPow
	external header
c
c  Input:
c    calscale
c    tvis
c    tmod
c    level
c    tscr
c    nhead
c    header
c    imhead	Use image header for phase center.
c  Output:
c    nchan
c    nvis
c    VisPow
c    ModPow
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer BufSize,maxgcf,width,maxlen
	parameter(BufSize = maxbuf/2,maxgcf=2048,width=6)
	parameter(maxlen=5*maxchan+10)
c
	integer length,nx,ny,nxd,nyd,nu,nv,ngcf,u0,v0,nread,i,j
	double precision preamble(4)
	real du,dv,umax,vmax,xref,yref,gcf(maxgcf)
	real Out(maxlen),uu,vv
	logical accept,flags(maxchan),doshift
	complex Buffer(BufSize),In(maxchan),Intp(maxchan)
c
c  Externals.
c
	integer nextpow2
c
c  Get all the info we could possibly want about the file.
c
	call rdhdi(tmod,'naxis1',nx,1)
	call rdhdi(tmod,'naxis2',ny,1)
	if(nx.le.1.or.ny.le.1)
     *	  call bug('f','MODEL: Input model is not two dimensional')
	call rdhdi(tmod,'naxis3',nchan,1)
	nxd = nextpow2(nx+1)
	nyd = nextpow2(ny+1)
	call rdhdr(tmod,'cdelt1',du,0.)
	call rdhdr(tmod,'cdelt2',dv,0.)
	if(du*dv.eq.0) call bug('f',
     *    'MODEL: cdelt1 or cdelt2 is missing from the model')
c
c  Calculate various thingos.
c
	du = 1/(nxd*du)
	dv = 1/(nyd*dv)
	nu = (width/2-1) + (nxd/2+1)
	nv = nyd
	u0 = width/2
	v0 = nyd/2 + 1
	umax = 0.5*(nxd-1-width) * abs(du)
	vmax = 0.5*(nyd-1-width) * abs(dv)
	if(nu*nv*nchan.gt.BufSize)
     *	 call bug('f','MODEL: Buffer overflow when calculating model')
c
	nvis = 0
	VisPow = 0
	ModPow = 0
	length = nhead + 5*nchan
c
	call uvread(tvis,preamble,In,flags,maxchan,nread)
	if(nread.ne.nchan)
     *	  call bug('f','The number of model and data channels differ')
c
c  Now that we have the info, we can find the FFT of the model.
c
	call ModFFT(tvis,tmod,nx,ny,nchan,nxd,nyd,level,imhead,
     *		Buffer,nv,nu,xref,yref)
	ngcf = width*((maxgcf-1)/width) + 1
	doshift = abs(xref)+abs(yref).gt.0
	call gcffun('spheroidal',gcf,ngcf,width,1.)
c
c  Loop the loop.
c
	dowhile(nread.eq.nchan)
	  call header(tvis,preamble,In,flags,nchan,accept,Out,nhead)
	  if(accept)then
	    if(abs(preamble(1)).gt.umax.or.abs(preamble(2)).gt.vmax)then
	      j = 1
	      do i=nhead+1,nhead+5*nchan,5
		Out(i  ) = real(In(j))
		Out(i+1) = aimag(In(j))
		Out(i+2) = 0
		Out(i+3) = 0
		Out(i+4) = -1
		j = j + 1
	      enddo
	    else
	      uu = preamble(1)/du
	      vv = preamble(2)/dv
	      call ModGrid(uu,vv,Buffer,nu,nv,nchan,u0,v0,gcf,ngcf,Intp)
	      if(doshift) call ModShift(preamble,xref,yref,Intp,nchan)
	      j = 1
	      do i=nhead+1,nhead+5*nchan,5
		Out(i  ) = real(In(j))
		Out(i+1) = aimag(In(j))
		Out(i+2) = real(Intp(j))
		Out(i+3) = aimag(Intp(j))
		Out(i+4) = 1
		if(.not.flags(j)) Out(i+4) = -1
		j = j + 1
	      enddo
	      call ModStat(calscale,tvis,Out(nhead+1),
     *					nchan,VisPow,ModPow)
	    endif
c
	    call scrwrite(tscr,Out,nvis*length,length)
	    nvis = nvis + 1
	  endif
	  call uvread(tvis,preamble,In,flags,maxchan,nread)
	enddo
c
	if(nread.ne.0) call bug('w',
     *	  'Stopped reading vis data when number of channels changed')
	end
c************************************************************************
	subroutine ModFFT(tvis,tmod,nx,ny,nchan,nxd,nyd,level,imhead,
     *	  Buffer,nv,nu,xref,yref)
c
	implicit none
	integer tvis,tmod,nx,ny,nchan,nxd,nyd,nv,nu
	complex Buffer(nv,nu,nchan)
	real xref,yref,level
	logical imhead
c
c  Input:
c    tvis
c    tmod
c    nx,ny
c    nchan
c    nxd,nyd
c    nv,nu
c    level
c    imhead	Use image header for phase center.
c  Output:
c    xref,yref	Offset, in radians, between the model and the visibility
c		phase center.
c    Buffer
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer width
	parameter(width=6)
	integer k,iref,jref
	real vra,vdec,dra,ddec,mra,mdec
	real xcorr(maxdim),ycorr(maxdim)
c
c  Determine the location of the visibility phase center in the image.
c
	call rdhdr(tmod,'crval1',mra,0.)
	call rdhdr(tmod,'crval2',mdec,0.)
c
	if(imhead)then
	  vra = mra
	  vdec = mdec
	else
	  call uvrdvrr(tvis,'ra',vra,0.)
	  call uvrdvrr(tvis,'dra',dra,0.)
	  call uvrdvrr(tvis,'dec',vdec,0.)
	  call uvrdvrr(tvis,'ddec',ddec,0.)
	  vdec = vdec + ddec
	  vra = vra + dra/cos(vdec)
	endif
c
	call rdhdr(tmod,'cdelt1',dra,0.)
	call rdhdr(tmod,'cdelt2',ddec,0.)
	call rdhdr(tmod,'crpix1',xref,real(nx/2+1))
	call rdhdr(tmod,'crpix2',yref,real(ny/2+1))
	if(ddec*dra.eq.0)
     *	  call bug('f','Pixel increment missing in model header')
c
	xref = cos(vdec) * (vra  - mra ) / dra  + xref
	yref =             (vdec - mdec) / ddec + yref
	iref = nint(xref)
	jref = nint(yref)
	xref = dra  * (xref - iref)
	yref = ddec * (yref - jref)
	if(iref.lt.1.or.iref.gt.nx.or.jref.lt.1.or.jref.gt.ny)
     *	  call bug('f','Visibility phase center is off the map')
c
c  Set up the gridding correction function.
c
	call ModCorr(xcorr,ycorr,nxd,nyd)
c
	do k=1,nchan
	  call xysetpl(tmod,1,k)
	  call ModPlane(tmod,nx,ny,nxd,nyd,xcorr,ycorr,Level,
     *				iref,jref,Buffer(1,1,k),nu,nv)
	enddo
	end
c************************************************************************
	subroutine ModGrid(uu,vv,Grd,nu,nv,nchan,u0,v0,gcf,ngcf,Intp)
c
	implicit none
	integer nu,nv,nchan,u0,v0,ngcf
	complex Intp(nchan),Grd(nv,nu,nchan)
	real uu,vv,gcf(ngcf)
c
c  This interpolates the visibilities from the FFT of the model.
c
c  Input:
c    uu,vv	U and V coordinates (measured in pixel, zero relative)
c		of the visibility values to find.
c    Grd	Contains the Fourier transform of the model.
c    nv,nu	Size of Buffer.
c    v0,u0	Pixel which is the origin in Buffer.
c    nchan	Number of channels.
c    gcf	Interpolation weights.
c    ngcf	Number of interpolation weights.
c  Output:
c    Intp	The interpolated visibilities.
c------------------------------------------------------------------------
	integer width
	parameter(width=6)
	integer Step,ju,jv,p,q,i
	real u,v
	real wu1,wu2,wu3,wu4,wu5,wu6,wv1,wv2,wv3,wv4,wv5,wv6,w
	logical Conjgate
c
	Step = (ngcf-1)/width
c
	conjgate = uu.lt.0
	if(conjgate)then
	  u = u0 - uu
	  v = v0 - vv
	else
	  u = u0 + uu
	  v = v0 + vv
	endif
	ju = u
	jv = v
	p = ngcf/2 - nint( Step*(v-jv) ) + 1
	q = ngcf/2 - nint( Step*(u-ju) ) + 1
c
c  NOTE: From here on down, the fact that width=6 is hard coded into the
c  algorithm!
c
	wu1 = gcf(q-2*Step)
	wu2 = gcf(q-  Step)
	wu3 = gcf(q       )
	wu4 = gcf(q+  Step)
	wu5 = gcf(q+2*Step)
	wu6 = gcf(q+3*Step)
c
	wv1 = gcf(p-2*Step)
	wv2 = gcf(p-  Step)
	wv3 = gcf(p       )
	wv4 = gcf(p+  Step)
	wv5 = gcf(p+2*Step)
	wv6 = gcf(p+3*Step)
c
	w = (wu1+wu2+wu3+wu4+wu5+wu6)*(wv1+wv2+wv3+wv4+wv5+wv6)
c
	do i=1,nchan
	  Intp(i) =
     *     wu1 * ( wv1*Grd(jv-2,ju-2,i) + wv2*Grd(jv-1,ju-2,i) +
     *		   wv3*Grd(jv  ,ju-2,i) + wv4*Grd(jv+1,ju-2,i) +
     *		   wv5*Grd(jv+2,ju-2,i) + wv6*Grd(jv+3,ju-2,i) ) +
     *	   wu2 * ( wv1*Grd(jv-2,ju-1,i) + wv2*Grd(jv-1,ju-1,i) +
     *		   wv3*Grd(jv  ,ju-1,i) + wv4*Grd(jv+1,ju-1,i) +
     *		   wv5*Grd(jv+2,ju-1,i) + wv6*Grd(jv+3,ju-1,i) ) +
     *     wu3 * ( wv1*Grd(jv-2,ju  ,i) + wv2*Grd(jv-1,ju  ,i) +
     *		   wv3*Grd(jv  ,ju  ,i) + wv4*Grd(jv+1,ju  ,i) +
     *		   wv5*Grd(jv+2,ju  ,i) + wv6*Grd(jv+3,ju  ,i) ) +
     *	   wu4 * ( wv1*Grd(jv-2,ju+1,i) + wv2*Grd(jv-1,ju+1,i) +
     *		   wv3*Grd(jv  ,ju+1,i) + wv4*Grd(jv+1,ju+1,i) +
     *		   wv5*Grd(jv+2,ju+1,i) + wv6*Grd(jv+3,ju+1,i) ) +
     *	   wu5 * ( wv1*Grd(jv-2,ju+2,i) + wv2*Grd(jv-1,ju+2,i) +
     *		   wv3*Grd(jv  ,ju+2,i) + wv4*Grd(jv+1,ju+2,i) +
     *		   wv5*Grd(jv+2,ju+2,i) + wv6*Grd(jv+3,ju+2,i) ) +
     *	   wu6 * ( wv1*Grd(jv-2,ju+3,i) + wv2*Grd(jv-1,ju+3,i) +
     *		   wv3*Grd(jv  ,ju+3,i) + wv4*Grd(jv+1,ju+3,i) +
     *		   wv5*Grd(jv+2,ju+3,i) + wv6*Grd(jv+3,ju+3,i) )
	   Intp(i) = Intp(i)/w
	enddo
c
c  Conjugate the data if necessary.
c
	if(conjgate) then
	  do i=1,nchan
	    Intp(i) = conjg(Intp(i))
	  enddo
	endif
c
	end
c************************************************************************
	subroutine ModShift(preamble,xref,yref,Intp,nchan)
c
	implicit none
	integer nchan
	double precision preamble(2)
	real xref,yref
	complex Intp(nchan)
c
c  Apply a phase rotation, which corresponds to a given image domain shift.
c
c  Input:
c    preamble(2)	The u and v value of the point, in wavelengths.
c    xref,yref		The shift to apply in the image domain, in radians
c			on the sky.
c    nchan		The number of channels to rotate.
c  Input/Output:
c    Intp		The data to be phase rotated.
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	real theta
	integer i
	complex W
c
	theta = 2*pi*(preamble(1)*xref + preamble(2)*yref)
	W = cmplx(cos(theta),sin(theta))
	do i=1,nchan
	  Intp(i) = W * Intp(i)
	enddo
c	  
	end
c************************************************************************
	subroutine ModCorr(xcorr,ycorr,nxd,nyd)
c
	implicit none
	integer nxd,nyd
	real xcorr(nxd),ycorr(nyd)
c
c  Generate the interpolation correction function. This also throws in a a
c  half image shift, and multiplying by (-1)**(j-1).
c  The interpolation function is assumed to be a spheroidal of width=6
c  and alpha=1.0.
c
c  Input:
c    nxd,nyd	Image size in x and y.
c  Output:
c    xcorr,ycorr Correction function in x and y, with a half image shift,
c		and multiplication by (-1)**(j-1) taken into account.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer width
	parameter(width=6)
	real data(maxdim)
	integer i,offset
c
	call corrfun('spheroidal',data,nxd,width,1.)
	offset = nxd/2
	do i=1,nxd/2
	  xcorr(i) = data(i+offset)
	enddo
	do i=nxd/2+1,nxd
	  xcorr(i) = data(i-offset)
	enddo
c
	call corrfun('spheroidal',data,nyd,width,1.)
	offset = nyd/2
	do i=1,nyd/2,2
	  ycorr(i)   =  data(i  +offset)
	  ycorr(i+1) = -data(i+1+offset)
	enddo
	do i=nyd/2+1,nyd,2
	  ycorr(i)   =  data(i  -offset)
	  ycorr(i+1) = -data(i+1-offset)
	enddo
	end
c************************************************************************
	subroutine ModPlane(tmod,nx,ny,nxd,nyd,xcorr,ycorr,Level,
     *						iref,jref,Buffer,nu,nv)
c
	implicit none
	integer tmod,nx,ny,nxd,nyd,iref,jref,nu,nv
	real xcorr(nxd),ycorr(nyd),Level
	complex Buffer(nv,nu)
c
c  Generate FFTs of a plane.
c
c  Input:
c    nv,nu	Dimensions of the FFT.
c  Output:
c    Buffer	FFTs of the planes.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer width
	parameter(width=6)
	real Data(maxdim),Shifted(maxdim)
	complex Cdat(maxdim)
	logical flags(maxdim)
	integer i,j,u0,i0,j0,offset
c
	if(nv.ne.nyd.or.nu.ne.(width/2-1)+(nxd/2+1))
     *	  call bug('f','ModPlane: Software bug -- wrong buffer dims')
	u0 = width/2
c
c Zero the middle of the shifted array.
c
	do i=nx-iref+2,nxd-iref+1
	  Shifted(i) = 0
	enddo
c
c  Loop over all the rows. Get the data, clip it, apply correction and
c  shift it. Do the first pass of the FFT.
c
	do j=1,nyd
	  j0 = j + jref - 1
	  if(j0.gt.nyd) j0 = j0 - nyd
	  if(j0.le.ny)then
	    call xyread(tmod,j0,Data)
	    call xyflgrd(tmod,j0,flags)
	    offset = iref - 1
	    do i=1,nx-iref+1
	      if(.not.flags(i+offset).or.Data(i+offset).lt.Level)then
		Shifted(i) = 0
	      else
		Shifted(i) = Data(i+offset)/(xcorr(i)*ycorr(j))
	      endif
	    enddo
	    offset = -(nxd-iref+1)
	    do i=nxd-iref+2,nxd
	      if(.not.flags(i+offset).or.Data(i+offset).lt.Level)then
		Shifted(i) = 0
	      else
		Shifted(i) = Data(i+offset)/(xcorr(i)*ycorr(j))
	      endif
	    enddo
	    call fftrc(shifted,Cdat,1,nxd)
	    do i=1,nxd/2+1
	      Buffer(j,i+u0-1) = Cdat(i)
	    enddo
	  endif
	enddo
c
c  Do the second pass of the FFT.
c
	do j=ny-jref+2,nyd-jref+1
	  CDat(j) = 0
	enddo

	do i=1,nxd/2+1
	  do j=1,ny-jref+1
	    Cdat(j) = Buffer(j,i+u0-1)
	  enddo
	  do j=nyd-jref+2,nyd
	    CDat(j) = Buffer(j,i+u0-1)
	  enddo
	  call fftcc(CDat,Buffer(1,i+u0-1),1,nyd)
	enddo
c
c  Copy the conjugate part to the first few rows.
c
	do i=1,u0-1
	  i0 = width - i
c#ivdep
	  do j=2,nyd
	    Buffer(j,i) = conjg(Buffer(nyd+2-j,i0))
	  enddo
	  Buffer(1,i) = Buffer(1,i0)
	enddo
c
	end
c************************************************************************
	subroutine ModPnt(calscale,tvis,offset,level,tscr,nhead,header,
     *	    nchan,nvis,VisPow,ModPow)
c
	implicit none
	logical calscale
	integer tvis,tscr,nhead,nchan,nvis
	real offset(2),level,ModPow,VisPow
	external header
c------------------------------------------------------------------------
	include 'maxdim.h'
	real pi
	integer maxlen
	parameter(pi=3.141592653589793,maxlen=5*maxchan+10)
c
	integer nread,length,j,i
	real xref,yref,Rp,Ip,theta,Out(maxlen)
	double precision preamble(4)
	logical accept,flags(maxchan)
	complex In(maxchan)
c
	xref = (offset(1)/3600.) * (pi/180)
	yref = (offset(2)/3600.) * (pi/180)
c
	ModPow = 0
	VisPow = 0
	nvis = 0
c
	call uvread(tvis,preamble,In,flags,maxchan,nchan)
	nread = nchan
	length = nhead + 5*nchan
c
c  Copy the data to the output, and compute the point model.
c
	dowhile(nread.eq.nchan)
	  call header(tvis,preamble,In,flags,nchan,accept,Out,nhead)
	  if(accept)then
	    theta = 2*pi*(xref*preamble(1) + yref*preamble(2))
	    Rp = level * cos(theta)
	    Ip = level * sin(theta)
	    j = 1
	    do i=nhead+1,nhead+5*nchan,5
	      Out(i  ) = real(In(j))
	      Out(i+1) = aimag(In(j))
	      Out(i+2) = Rp
	      Out(i+3) = Ip
	      Out(i+4) = 1
	      if(.not.flags(j)) Out(i+4) = -1
	      j = j + 1
	    enddo
c
	    call ModStat(calscale,tvis,Out(nhead+1),nchan,VisPow,ModPow)
	    call scrwrite(tscr,Out,nvis*length,length)
	    nvis = nvis + 1
	  endif
	  call uvread(tvis,preamble,In,flags,maxchan,nread)
	enddo
c
	if(nread.ne.0) call bug('w',
     *	  'Stopped reading vis data when number of channels changed')
	end
c************************************************************************
	subroutine ModStat(calscale,tvis,Out,nchan,VisPow,ModPow)
c
	implicit none
	logical calscale
	integer tvis,nchan
	real Out(5*nchan),VisPow,ModPow
c
c  This routine does a few steps common to both point-source and model
c  calculations. It applies the
c  calibrator scale factors (if needed) and accumulates the power in the
c  visibility and model.
c
c  Input:
c    calscale	True if we are to call ModGet to determine the calibrator
c		scale factor.
c    tvis	Handle of the input visibility file.
c    nchan
c    Out	These are the actual and model correlations.
c  Input/Output:
c    VisPow	Updated to reflect the added power.
c    ModPow	Updated to reflect the added power.
c------------------------------------------------------------------------
	integer i
	real a
c
	if(calscale)then
	  call ModGet(tvis,nchan,a)
	  do i=1,5*nchan,5
	    Out(i+2) = a * Out(i+2)
	    Out(i+3) = a * Out(i+3)
	    if(Out(i+4).gt.0)then
	      VisPow = VisPow + Out(i  )*Out(i  ) + Out(i+1)*Out(i+1)
	      ModPow = ModPow + Out(i+2)*Out(i+2) + Out(i+3)*Out(i+3)
	    endif
	  enddo
	else
	  do i=1,5*nchan,5
	    if(Out(i+4).gt.0)then
	      VisPow = VisPow + Out(i  )*Out(i  ) + Out(i+1)*Out(i+1)
	      ModPow = ModPow + Out(i+2)*Out(i+2) + Out(i+3)*Out(i+3)
	    endif
	  enddo
	endif
	end
c************************************************************************
	subroutine ModGet(tvis,nchan,a)
c
	implicit none
	integer tvis
	real a
	integer nchan
c
c  Determine the calibrator flux.
c
c  Input:
c    tvis	Handle of the visibility data file.
c    nchan	Number of channels.
c  Output:
c    a		Flux of the calibrator.
c------------------------------------------------------------------------
	include 'model.h'
	character source*16,source1*16,type*1
	logical more,update,isplanet
	real flux,freq
	double precision day,dfreq,delta
	integer n,iostat,length
        character*80 umsg
c
c  Externals.
c
	integer bsrcha
	real ModPlant
c
c  Get the current source name, and check if we already have information
c  about it.
c
	call uvrdvra(tvis,'source',source,'unknown')
	if(calcur.gt.0)then
	  if(cals(calcur).ne.source) calcur = 0
	endif
	if(calcur.eq.0.and.ncals.gt.0)
     *	  calcur = bsrcha(source,cals,ncals)
c
c  We do not have information about this calibrator. We will have to
c  get if from the calibrator text file. Determine a number of things
c  about the uv data file, and then call the CalGet routine to find
c  out the flux.
c
	if(calcur.eq.0)then
	  if(ncals.eq.maxcals)
     *	    call bug('f','ModGet: Ran out of space in cal table')
c
	  call uvrdvrd(tvis,'time',day,0.d0)
	  source1 = source
	  call uvfit1(tvis,'frequency',nchan,dfreq,delta)
	  freq = dfreq
c
c  Check whether this source is a planet.
c
	  call uvprobvr(tvis,'pltb',type,length,update)
	  isplanet = type.eq.'r'.and.length.eq.1
	  if(isplanet)then
	    call uvrdvrr(tvis,'pltb',flux,0.)
	    isplanet = flux.gt.0
	  endif
	  if(.not.isplanet)then
	    flux = 0
	    call CalGet(' ',source1,freq,100.,day,1000.,flux,iostat)
            umsg = 'Error determining flux of '//source
	    if(iostat.ne.0) call bug('f', umsg )
	  endif
c
c  Add this to our list of calibrators, in alphabetic order.
c
	  n = ncals
	  more = .true.
	  dowhile(more.and.n.gt.0)
	    more = source.lt.cals(n)
	    if(more)then
	      cals(n+1) = cals(n)
	      calfreq(n+1) = calfreq(n)
	      planet(n+1) = planet(n)
	      calflux(n+1) = calflux(n)
	      n = n - 1
	    endif
	  enddo
	  calcur = n + 1
	  ncals = ncals + 1
c
c  Save parameters.
c
	  cals(calcur) = source
	  calflux(calcur) = flux
	  calfreq(calcur) = freq
	  planet(calcur) = isplanet
	endif
c
c  Return the flux.
c
	if(planet(calcur))then
	  a = ModPlant(tvis,calfreq(calcur))
	else
	  a = calflux(calcur)
	endif
	end
c************************************************************************
	real function ModPlant(tvis,freq)
c
	implicit none
	integer tvis
	real freq
c
c  This determines the flux of a planet for the current visibility. This
c  looks for variables in the visibility file which give the characteristics
c  of the planet.
c
c  Input:
c    tvis	Handle of the visibility file.
c    freq	Observing frequency, in GHz.
c  Output:
c    ModPlant	The flux of the planet for this baseline.
c------------------------------------------------------------------------
	real pi,h,c,k
	parameter(pi=3.141592653589793,h=6.6252e-34,c=2.99792458e8)
	parameter(k=1.38045e-23)
	double precision coord(2)
	real plmaj,plmin,plangle,pltb,u,v,flux,cosi,sini,beta,omega
c
c  Externals.
c
	real j1xbyx
c
c  Get info from the visibility file.
c  Units returned by the uv routines.
c    u,v -- nanosec.
c    plmaj,plmin -- arcsec.
c    plangle -- degrees
c    pltb -- Kelvin
	call uvgetvrd(tvis,'coord',coord,2)
	u = coord(1)
	v = coord(2)
	call uvgetvrr(tvis,'plmaj',plmaj,1)
	call uvrdvrr(tvis,'plmin',plmin,plmaj)
	call uvrdvrr(tvis,'plangle',plangle,0.)
	call uvgetvrr(tvis,'pltb',pltb,1)
c
c  Unit conversion.
c
	plangle = pi/180 * plangle
	plmaj = pi * plmaj / 180 / 3600 
	plmin = pi * plmin / 180 / 3600
c
c  We have the characteristics of the source. Now compute the flux (in Jy).
c    plange -- radians.
c    plmaj,plmin -- radians.
c    pltb -- Kelvin.
c    u,v  -- nanosec.
c    freq -- GHz
c  The factor 1e26 converts between W/m**2/Hz to Janksy.
c
	cosi = cos(plangle)
	sini = sin(plangle)
	beta = pi * sqrt((plmaj*(u*cosi-v*sini))**2
     *	  	       + (plmin*(u*sini+v*cosi))**2)
	omega = pi/4 * plmaj*plmin
	flux = omega * 2*(h*1e26)/(c*c)*(freq**3*1e27)/
     *	 ( exp(((h/k)*1e9)*freq/pltb) - 1. )
	ModPlant = 2.*j1xbyx(beta*freq) * flux
	end		
c************************************************************************
	subroutine ModInit
c
	implicit none
c
c  This initialises the routines to determine the calibration flux.
c
c------------------------------------------------------------------------
	include 'model.h'
	calcur = 0
	ncals = 0
	end
