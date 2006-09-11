c************************************************************************
c  Bugs and Shortcomings:
c    This is one of the routines where maxdim.h can play a crucial role
c      because often there is not enough memory -- in ModMap:
c      bufsize ~ 4.nx.ny.nz (nx,ny,nz = cubesize)
c    Handles polarisations through the select mechanism, rather than the
c      uvDat way.
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
c    rjs   5apr91 More tolerant of big difference between vis phase center
c		  and map center. Simple polarisation processing. Use
c		  MemAlloc routine.
c    rjs   2aug91 Changed "pol" to "polarization" in simple polarization
c	          handling.
c    rjs  30aug91 Correct determination of u-v when computing model off
c		  the phase center.
c    rjs   1nov91 Slightly better (though still crude) polarisation
c		  handling. Handling of mfs data and shifts of models.
c    rjs  29jan91 Fixed sign error in the direction of the shift, in
c		  ModShift.
c    rjs  30mar92 Pointing selection always performed in ModelIni, if
c		  'p' flag given. Better error messages(?).
c    rjs  26apr92 Added check for clip level processing.
c    rjs  10may92 Reworked the way clipping is done.
c    rjs  17may92 Clipping message, to appease NEBK.
c    rjs  15feb93 Changes to make ra/dec variables double.
c    mchw 02may93 Added default flux to apriori option.
c    mchw 02jul93 Change flux limit to zero (all entries in flux table)
c    rjs   6jul93 Do not exceed MAXDIM in model calculation. Fix
c		  clippind message for cross hand polarisations.
c    rjs   3aug93 Make sure the FFT is at least 16 in size.
c    rjs  16sep93 Rename bsrch to binsrch.
c    rjs  11nov93 Correct definition of alpha for mfs work.
c    rjs  17aug93 Handle offsets somewhat better, using co routines.
c		  W-axis change.
c    rjs  31jan95 Correct u-v-w coordinate geometry. Use 3D equation
c		  for phases of a point source.
c    rjs   9mar95 Turn off geometry correction for "imhead" option.
c    mhw  05jan96 Add zero option for Model and ModMap
c************************************************************************
c*ModelIni -- Ready the uv data file for processing by the Model routine.
c&rjs
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
c		a processing step.
c		 'l'	Set up the linetype.
c		 'p'	Set up the pointing parameters.
c		 's'	Perform polarisation selection.
c		 't'	Source is polarised.
c--
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer PolI,PolXX,PolYY,PolRR,PolLL
	parameter(PolI=1,PolRR=-1,PolLL=-2,PolXX=-5,PolYY=-6)
c
	double precision ra,dec,cosd,tol,tmp
	logical doPol
	integer nchan,polm,polv,npnt
	real lstart,lwidth,lstep
	character ltype*64
c
c  Externals.
c
	character PolsC2P*2
	logical PolsPara
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
	  call mosLoad(tmod,npnt)
	  if(npnt.ne.1)call bug('f',
     *	    'Expecting only a single pointing, but got several')
	  call mosRaDec(1,ra,dec)
	  call rdhdd(tmod,'cdelt1',tol,0.d0)
	  call rdhdd(tmod,'cdelt2',tmp,0.d0)
	  tol = 3*max(abs(tol),abs(tmp))
	  cosd = abs(cos(dec))
	  call uvselect(tvis,'and',0.d0,0.d0,.true.)
	  call uvselect(tvis,'ra',ra-tol*cosd,ra+tol*cosd,.true.)
	  call uvselect(tvis,'dec',dec-tol,dec+tol,.true.)
	endif
c
c  See what we can work out about the polarisation of both the map
c  and the visibility data.
c
	if(index(flags,'s').ne.0)then
	  doPol = index(flags,'t').ne.0
	  call coInit(tmod)
	  call ModPolM(tmod,polm)
	  call coFin(tmod)
	  call ModPolV(tvis,polv)
c
c  Handle a polarised source. Model and visibility have to agree.
c
	  if(doPol)then
	    if(polm.eq.polv)then
	      continue
	    else if(polv.ne.0)then
	      call bug('f',
     *	        'Model and visibility are not the same polarisation')
	    else
	      call uvselect(tvis,'and',0.d0,0.d0,.true.)
	      call uvselect(tvis,'polarization',dble(polm),0.d0,.true.)
	      call output('Selecting polarization type '//PolsC2P(polm))
	    endif
c
c  Handle an unpolarised source. As long as they are both intensity-type
c  polarisations, all is OK.
c
	  else
	    if(.not.PolsPara(polm))call bug('f',
     *	      'Polarized model used for nominally unpolarised data')
	    if(polv.ne.0)then
	      if(.not.PolsPara(polv))call bug('f',
     *		'Incompatible model and visibility polarisations')
	    else
	      call uvselect(tvis,'and',0.d0,0.d0,.true.)
	      call uvselect(tvis,'polarization',dble(PolI), 0d0,.true.)
	      call uvselect(tvis,'polarization',dble(PolRR),0d0,.true.)
	      call uvselect(tvis,'polarization',dble(PolLL),0d0,.true.)
	      call uvselect(tvis,'polarization',dble(PolXX),0d0,.true.)
	      call uvselect(tvis,'polarization',dble(PolYY),0d0,.true.)
	    endif
	  endif
	endif
c
	end
c************************************************************************
c*Model -- Calculate model visibilities, given a model image.
c& mchw
c:model
c+
	subroutine Model(flags,tvis,tmod,offset,level,tscr,
     *				nhead,header,calget,nchan,nvis)
c
	implicit none
	character flags*(*)
	integer tmod,tvis,tscr,nchan,nhead,nvis
	real offset(2),level
	external header,calget
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
c		 'h'  Use image header for phase center.
c		 'm'  Multi-freq data. The model is either one or two
c		      planes, which were formed using INVERT's mfs option.
c		 'l'  Apply clipping.
c                'z'  Use zero if model cannot be calculated (don't flag)
c    offset	The offset, in arcsec, in RA and DEC, of the point
c		source model. This is only used if tmod.eq.0.
c    level	Either a clip level to apply to the data (tmod.ne.0), or
c		the amplitude of the point source (tmod.eq.0). If it is
c		a clip level, it is applied only if the 'l' flag (see
c		above) is given. Additionally, depending on the type of the
c		model, then the clip level can be used in one of two ways.
c		If the model is Stokes-Q,U or V, or a raw cross-hands
c		polarisation type (strange!!), or I-alpha map from multi-freq
c		synthesis, then pixels in the range -Level to Level are clipped.
c		If the model is none of the above, then pixels below
c		Level are clipped.
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
c    calget	Returns the flux for a point source.
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
c    * Calibration scaling is not handled yet.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer maxlen
	parameter(maxlen=5*maxchan+10)
c
	real Out(maxlen),a,VisPow,ModPow
	integer i,j,length
	logical calscale,imhead,mfs,doclip,zero
c
c  Initialise the coordinate handles.
c
	if(tmod.ne.0)call coInit(tmod)
c
	call ModInit
	call ScrOpen(tscr)
	call uvset(tvis,'coord','wavelength',0,0.,0.,0.)
	call uvset(tvis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	calscale = index(flags,'c').ne.0
	imhead = index(flags,'h').ne.0
	mfs = index(flags,'m').ne.0
	doclip = index(flags,'l').ne.0
	zero = index(flags,'z').ne.0
c
	if(tmod.ne.0)then
	  call ModMap(calscale,tvis,tmod,level,doclip,zero,tscr,nhead,
     *      header,calget,imhead,mfs,nchan,nvis,VisPow,ModPow)
	else
	  call ModPnt(calscale,tvis,offset,level,tscr,nhead,header,
     *	    calget,nchan,nvis,VisPow,ModPow)
	endif
	if(ModPow.le.0) call bug('w','Model visibilities are all zero')
	if(VisPow.le.0) call bug('w','All visibilities are zero')
c
c  Release the coordinate system handles.
c
	call coFin(tvis)
	if(tmod.ne.0)call coFin(tmod)
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
	subroutine ModMap(calscale,tvis,tmod,level,doclip,zero,tscr,
     *      nhead,header,calget,imhead,mfs,nchan,nvis,VisPow,ModPow)
c
	implicit none
	logical calscale,imhead,mfs,doclip,zero
	integer tvis,tscr,nhead,nchan,nvis,tmod
	real level,ModPow,VisPow
	external header,calget
c
c  Input:
c    calscale
c    tvis
c    tmod
c    level
c    doclip
c    zero
c    tscr
c    nhead
c    header
c    calget	Service routine to return flux of calibrator.
c    imhead	Use image header for phase center.
c  Output:
c    nchan
c    nvis
c    VisPow
c    ModPow
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer maxgcf,width,maxlen
	parameter(maxgcf=2048,width=6)
	parameter(maxlen=5*maxchan+10)
c
	integer length,nx,ny,nz,nxd,nyd,nu,nv,ngcf,u0,v0,nread,i,j,pnt
	integer polm
	double precision preamble(5),ucoeff(3),vcoeff(3),x1(2),x2(2)
	real du,dv,umax,vmax,gcf(maxgcf)
	double precision xref1,yref1,xref2,yref2,ud,vd
	real Out(maxlen),uu,vv,u,v
	logical accept,flags(maxchan),doshift,GotFreq
	complex Buffer((maxbuf+1)/2)
	complex In(maxchan),Intp(maxchan+1)
	double precision sfreq(maxchan),freq0
	common Buffer
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
	call rdhdi(tmod,'naxis3',nz,1)
	if(nz.le.0) call bug('f','Bad value for NAXIS3')
	if(mfs.and.nz.gt.2)
     *	  call bug('f','Invalid value for NAXIS3 for MFS data')
	nxd = max(min(MAXDIM,nextpow2(2*nx)),16)
	nyd = max(min(MAXDIM,nextpow2(2*ny)),16)
	if(nxd.lt.nx.or.nyd.lt.ny)call bug('f','Model too big')
	call rdhdr(tmod,'cdelt1',du,0.)
	call rdhdr(tmod,'cdelt2',dv,0.)
	if(du*dv.eq.0) call bug('f',
     *    'MODEL: cdelt1 or cdelt2 is missing from the model')
c
c  If its a MFCLEAN model, then get then reference frequency.
c
	if(mfs.and.nz.eq.2)call ModFreqM(tmod,freq0)
c
c  Calculate various thingos.
c
	du = 1/(nxd*du)
	dv = 1/(nyd*dv)
	nu = (width/2-1) + (nxd/2+1)
	nv = nyd
	u0 = width/2
	v0 = nyd/2 + 1
	umax = 0.5*(nxd-1-width)
	vmax = 0.5*(nyd-1-width)
	call MemAlloc(pnt,2*nu*nv*nz+1,'r')
c
	nvis = 0
	VisPow = 0
	ModPow = 0
c
	call uvread(tvis,preamble,In,flags,maxchan,nchan)
	call coInit(tvis)
	if(nchan.eq.0)
     *	  call bug('f','No visibility data selected, in Model(map)')
	if(nchan.ne.nz.and..not.mfs)
     *	  call bug('f','The number of model and data channels differ')
c
c  Determine the uvw conversion matrix to account for geometry differences.
c
	if(imhead)then
	  ucoeff(1) = 1
	  ucoeff(2) = 0
	  ucoeff(3) = 0
	  vcoeff(1) = 0
	  vcoeff(2) = 1
	  vcoeff(3) = 0
	else
	  x1(1) = 0
	  x1(2) = 0
	  call coCvt(tvis,'op/op',x1,'aw/aw',x2)
	  call coGeom(tmod,'aw/aw',x2,ucoeff,vcoeff)
	endif
c
c  If we are applying clipping, we need to know the polarisation type
c  of the model.
c
	polm = 0
	if(doclip)call ModPolM(tmod,polm)
c
c  Now that we have the info, we can find the FFT of the model.
c
	call ModFFT(tvis,tmod,nx,ny,nz,nxd,nyd,level,polm,doclip,
     *	  imhead,Buffer(pnt/2+1),nv,nu,mfs,xref1,yref1,xref2,yref2)
	ngcf = width*((maxgcf-1)/width) + 1
	doshift = abs(xref1)+abs(yref1)+abs(xref2)+abs(yref2).gt.0
	call gcffun('spheroidal',gcf,ngcf,width,1.)
c
c  Loop the loop.
c
	nread = nchan
	length = nhead + 5*nread
	dowhile(nread.eq.nchan)
	  call header(tvis,preamble,In,flags,nread,accept,Out,nhead)
	  if(accept)then
	    GotFreq = .false.
	    sfreq(1) = 1
c
c  Correct u and v for geometry differences and convert them to grid units.
c
	    ud = ( ucoeff(1)*preamble(1) + ucoeff(2)*preamble(2) + 
     *		  ucoeff(3)*preamble(3) )
	    vd = ( vcoeff(1)*preamble(1) + vcoeff(2)*preamble(2) +
     *		  vcoeff(3)*preamble(3) )
	    u = ud / du
	    v = vd / dv
c
c  Handle the case of a single image being replicated along the frequency
c  axis.
c
	    if(mfs)then
	      if((nread.gt.1.or.nz.eq.2).and..not.GotFreq)then
		call uvinfo(tvis,'sfreq',sfreq)
		GotFreq = .true.
	      endif
	      u = u / sfreq(1)
	      v = v / sfreq(1)
	      do j=1,nread
		uu = u * sfreq(j)
		vv = v * sfreq(j)
		if(abs(uu).gt.umax.or.abs(vv).gt.vmax)then
		  Intp(j) = 0
c                 Flag data if zero option not used
		  if(.not.zero) flags(j) = .false.
		else
		  call ModGrid(uu,vv,Buffer(pnt/2+1),nu,nv,nz,u0,v0,
     *		    gcf,ngcf,Intp(j))
		  if(nz.eq.2) Intp(j) = Intp(j) +
     *			log(real(sfreq(j)/freq0))*Intp(j+1)
		endif
	      enddo
c
c  Handle the case of a data cube.
c
	    else
	      if(abs(u).gt.umax.or.abs(v).gt.vmax)then
		do j=1,nread
		  Intp(j) = 0.
		  if(.not.zero) flags(j) = .false.
		  sfreq(j) = 1
		enddo
		GotFreq = .true.
	      else
	        call ModGrid(u,v,Buffer(pnt/2+1),nu,nv,nread,u0,v0,
     *		  gcf,ngcf,Intp)
	      endif
	    endif
c
c  Perform a shift, if necessary.
c
	    if(doshift)then
	      if(.not.GotFreq.and.nread.gt.1)
     *		call uvinfo(tvis,'sfreq',sfreq)
	      call ModShift(ud,vd,xref1,yref1,xref2,yref2,sfreq,
     *							Intp,nread)
	    endif
c
c  Copy the data to the output, and determine statistics.
c
	    j = 1
	    do i=nhead+1,nhead+5*nread,5
	      Out(i  ) = real(In(j))
	      Out(i+1) = aimag(In(j))
	      Out(i+2) = real(Intp(j))
	      Out(i+3) = aimag(Intp(j))
	      Out(i+4) = 1
	      if(.not.flags(j)) Out(i+4) = -1
	      j = j + 1
	    enddo
	    call ModStat(calscale,tvis,Out(nhead+1),nread,
     *		calget,level,VisPow,ModPow)
	    call scrwrite(tscr,Out,nvis*length,length)
	    nvis = nvis + 1
	  endif
	  call uvread(tvis,preamble,In,flags,maxchan,nread)
	enddo
c
	if(nread.ne.0) call bug('w',
     *	  'Stopped reading vis data when number of channels changed')
	call MemFree(pnt,2*nu*nv*nz+1,'r')
	end
c************************************************************************
	subroutine ModPolM(tmod,polm)
c
	implicit none
	integer tmod,polm
c
c  Determine the polarisation type of the model.
c
c  Input:
c    tmod	The handle of the input model.
c  Output:
c    polm	The polarisation type of the map. If this info is missing
c		then Stokes-I is assumed.
c------------------------------------------------------------------------
	integer StokesI
	parameter(StokesI=1)
	integer iax
	double precision t
c
	call coFindAx(tmod,'stokes',iax)
	if(iax.ne.0)then
	  call coCvt1(tmod,iax,'ap',1.d0,'aw',t)
	  polm = nint(t)	  
	else
	  polm = StokesI
	endif
	end
c************************************************************************
	subroutine ModPolV(tvis,polv)
c
	implicit none
	integer tvis,polv
c
c  Determine the polarisation type of the visibility data.
c
c  Input:
c    tvis	The handle of the input visibility.
c  Output:
c    polv	The polarisation type of the visibility data. If this info
c		is missing, then Stokes-I is assumed. If the information
c		is indeterminant, then 0 is returned.
c------------------------------------------------------------------------
	integer StokesI
	parameter(StokesI=1)
	integer length
	character tpol*1
	logical update
c
c  Determine the polarisation type of the visibility data.
c
	call uvprobvr(tvis,'pol',tpol,length,update)
	if(tpol.eq.'i')then
	  call rdhdi(tvis,'pol',polv,0)
	else
	  polv = StokesI
	endif
c
	end
c************************************************************************
	subroutine ModFreqM(tmod,freq0)
c
	implicit none
	integer tmod
	double precision freq0
c
c  Get the reference frequency of the model.
c
c  Input:
c    tmod	Handle of the model.
c  Output:
c    freq0	The reference frequency.
c------------------------------------------------------------------------
	integer iax
c
	call coFindAx(tmod,'freq',iax)
	if(iax.eq.0)call bug('f',
     *	  'Unable to determine MFS reference frequency')
	call coCvt1(tmod,iax,'op',0.d0,'aw',freq0)
	end
c************************************************************************
	subroutine ModFFT(tvis,tmod,nx,ny,nchan,nxd,nyd,level,polm,
     *	  doclip,imhead,Buffer,nv,nu,mfs,xref1,yref1,xref2,yref2)
c
	implicit none
	integer tvis,tmod,nx,ny,nchan,nxd,nyd,nv,nu,polm
	complex Buffer(nv,nu,nchan)
	double precision xref1,yref1,xref2,yref2
	real level
	logical imhead,mfs,doclip
c
c  Input:
c    tvis
c    tmod
c    nx,ny
c    nchan
c    nxd,nyd
c    nv,nu
c    level
c    imhead	Assume visibility observing center and image reference
c		pixel are the same location.
c    mfs	If true, then the input model is a multi-freq synthesis
c		image.
c  Output:
c    xref1,yref1
c    xref2,yref2 Amount to shift the model. The total shift is the sum of the
c		 frequency-independent portion, and the frequency-dependent
c		 portion. The frequency-independent portion caused by a map
c		 whose reference pixel is at a fractional pixel. The frequency-
c		 dependent portion is due to the map and visibility phase
c		 centres being at different pixels.
c    Buffer
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer width
	parameter(width=6)
	integer k,iref,jref,nclip
	double precision dra,ddec
	real xcorr(maxdim),ycorr(maxdim)
	character val*9
	double precision x1(2),x2(2)
c
c  Externals.
c
	logical PolsPara
c
c  Determine the properties of the model coordinate system.
c
	call rdhdd(tmod,'cdelt1',dra,0.d0)
	call rdhdd(tmod,'cdelt2',ddec,0.d0)
	if(ddec*dra.eq.0)
     *	  call bug('f','Pixel increment missing in model header')
c
	iref = nx/2 + 1
	jref = ny/2 + 1
	x1(1) = 0
	x1(2) = 0
	call coCvt(tmod,'op/op',x1,'ap/ap',x2)
c
	xref1 = dra  * (x2(1) - iref)
	yref1 = ddec * (x2(2) - jref)
c
c  Determine the shift required to phase the model to the visibility dataset.
c
	if(imhead)then
	  xref2 = 0
	  yref2 = 0
	else
	  x1(1) = 0
	  x1(2) = 0
	  call coCvt(tvis,'op/op',x1,'aw/aw',x2)
	  call coCvt(tmod,'aw/aw',x2,'op/op',x1)
	  xref2 = dra  * x1(1)
	  yref2 = ddec * x1(2)
	endif
c
c  All shifts are the same for a model with a single plane.
c
	if(mfs.or.nchan.eq.1)then
	  xref2 = xref2 + xref1
	  xref1 = 0
	  yref2 = yref2 + yref1
	  yref1 = 0
	endif
c
c  Set up the gridding correction function.
c
	call ModCorr(xcorr,ycorr,nxd,nyd)
c
c  Determine the clipping mode. If no clipping is to be done, then
c  nclip = 0. If the model is an intensity-type polarisation (nclip=1)
c  then any value below "Level" is clipped. Otherwise (nclip=2) any data
c  in the range -Level to Level is clipped.
c
	nclip = 0
	if(doclip)then
	  nclip = 1
	  if(.not.PolsPara(polm))nclip = 2
	endif
	write(val,'(1pg9.2)')Level
	if(nclip*Level.ne.0.and.nclip.eq.0)then
	  call output('No clipping being Performed on the model')
	else if(nclip.eq.1)then
	  call output('Clipping model when: pixval < '//val)
	else if(nclip.eq.2)then
	  call output('Clipping model when: abs(pixval) < '//val)
	endif
c
	do k=1,nchan
	  call xysetpl(tmod,1,k)
	  call ModPlane(tmod,nx,ny,nxd,nyd,xcorr,ycorr,Level,nclip,
     *				iref,jref,Buffer(1,1,k),nu,nv)
	  if(mfs.and.doclip) nclip = 2
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
	subroutine ModShift(uu,vv,xref1,yref1,xref2,yref2,freq,
     *							Intp,nchan)
c
	implicit none
	integer nchan
	double precision xref1,yref1,xref2,yref2,uu,vv
	double precision freq(nchan)
	complex Intp(nchan)
c
c  Apply a phase rotation, which corresponds to a given image domain shift.
c
c  Input:
c    preamble	 The u and v value of the firsst channel, in wavelengths.
c    freq	 The sky frequencies of the channels.
c    xref1,yref1
c    xref2,yref2 Amount to shift the model. The total shift is the sum of the
c		 frequency-independent portion, and the frequency-dependent
c		 portion. The frequency-independent portion caused by a map
c		 whose reference pixel is at a fractional pixel. The frequency-
c		 dependent portion is due to the map and visibility phase
c		 centres being at different pixels.
c    nchan		The number of channels to rotate.
c  Input/Output:
c    Intp		The data to be phase rotated.
c------------------------------------------------------------------------
	include 'mirconst.h'
	real theta
	double precision t1,t2
	integer i
	complex W
c
	t1 = -2*dpi*( uu*xref1 + vv*yref1 )
	t2 = -2*dpi*( uu*xref2 + vv*yref2 ) / freq(1)
c
	do i=1,nchan
	  theta = t1 + t2 * freq(i)
	  W = cmplx(cos(theta),sin(theta))
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
	subroutine ModPlane(tmod,nx,ny,nxd,nyd,xcorr,ycorr,Level,nclip,
     *						iref,jref,Buffer,nu,nv)
c
	implicit none
	integer tmod,nx,ny,nxd,nyd,iref,jref,nu,nv,nclip
	real xcorr(nxd),ycorr(nyd),Level
	complex Buffer(nv,nu)
c
c  Generate FFTs of a plane.
c
c  Input:
c    nv,nu	Dimensions of the FFT.
c    nclip	Clipping mode. nclip = 0: No clipping.
c				     = 1: Clip below level.
c				     = 2: Clip in the range -Level to Level.
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
c
c  Get the flags and do the clipping.
c
	    call xyflgrd(tmod,j0,flags)
	    if(nclip.eq.1)then
	      do i=1,nx
		flags(i) = flags(i).and.Data(i).gt.Level
	      enddo
	    else if(nclip.eq.2)then
	      if(Level.lt.0)call bug('f','Invalid clip level')
	      do i=1,nx
		flags(i) = flags(i).and.
     *		  (Data(i).gt.Level.or.Data(i).lt.-Level)
	      enddo
	    endif
	    offset = iref - 1
	    do i=1,nx-iref+1
	      if(flags(i+offset))then
		Shifted(i) = Data(i+offset)/(xcorr(i)*ycorr(j))
	      else
		Shifted(i) = 0
	      endif
	    enddo
	    offset = -(nxd-iref+1)
	    do i=nxd-iref+2,nxd
	      if(flags(i+offset))then
		Shifted(i) = Data(i+offset)/(xcorr(i)*ycorr(j))
	      else
		Shifted(i) = 0
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
     *	    calget,nchan,nvis,VisPow,ModPow)
c
	implicit none
	logical calscale
	integer tvis,tscr,nhead,nchan,nvis
	real offset(2),level,ModPow,VisPow
	external header,calget
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer maxlen
	parameter(maxlen=5*maxchan+10)
c
	integer nread,length,j,i
	real theta,temp,Out(maxlen)
	double precision preamble(5),lmn(3),off(2)
	logical accept,flags(maxchan)
	complex In(maxchan)
	double precision skyfreq(maxchan)
c
	ModPow = 0
	VisPow = 0
	nvis = 0
c
c  Get the first record.
c
	call uvread(tvis,preamble,In,flags,maxchan,nchan)
	call coInit(tvis)
c
c  If there is an offset to the point source, determine its true
c  position.
c
	if(abs(offset(1))+abs(offset(2)).gt.0)then
	  off(1) = (offset(1)/3600.) * (pi/180)
	  off(2) = (offset(2)/3600.) * (pi/180)
	  call coLMN(tvis,'ow/ow',off,lmn)
	else
	  lmn(1) = 0
	  lmn(2) = 0
	  lmn(3) = 1
	endif
c
	nread = nchan
	length = nhead + 5*nchan
c
c  Copy the data to the output, and compute the point model.
c
	dowhile(nread.eq.nchan)
	  call header(tvis,preamble,In,flags,nchan,accept,Out,nhead)
	  if(accept)then
	    if(abs(lmn(1))+abs(lmn(2)).eq.0)then
	      j = 1
	      do i=nhead+1,nhead+5*nchan,5
	        Out(i  ) = real(In(j))
	        Out(i+1) = aimag(In(j))
	        Out(i+2) = level
	        Out(i+3) = 0
	        Out(i+4) = 1
	        if(.not.flags(j)) Out(i+4) = -1
	        j = j + 1
	      enddo
	    else
	      theta = 2*dpi*(lmn(1)*preamble(1) + lmn(2)*preamble(2) +
     *			     (lmn(3)-1)*preamble(3) )
	      if(nchan.eq.1)then
		skyfreq(1) = 1
	      else
		call uvinfo(tvis,'sfreq',skyfreq)
	        theta = theta / skyfreq(1)
	      endif
	      j = 1
	      do i=nhead+1,nhead+5*nchan,5
	        Out(i  ) = real(In(j))
	        Out(i+1) = aimag(In(j))
		temp = theta * skyfreq(j)
		Out(i+2) = level * cos(temp)
		Out(i+3) = level * sin(temp)
	        Out(i+4) = 1
	        if(.not.flags(j)) Out(i+4) = -1
	        j = j + 1
	      enddo
	    endif
c
	    call ModStat(calscale,tvis,Out(nhead+1),nchan,calget,level,
     *		VisPow,ModPow)
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
	subroutine ModStat(calscale,tvis,Out,nchan,calget,level,
     *							VisPow,ModPow)
c
	implicit none
	logical calscale
	integer tvis,nchan
	real Out(5*nchan),VisPow,ModPow,level
	external calget
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
c    calget	Service routine to return the flux of the calibrator.
c    level	The default flux or level of the model.
c  Input/Output:
c    VisPow	Updated to reflect the added power.
c    ModPow	Updated to reflect the added power.
c------------------------------------------------------------------------
	integer i
	real a
c
	if(calscale)then
	  call ModGet(calget,tvis,nchan,a,level)
	  do i=1,5*nchan,5
	    Out(i+2) = a / level * Out(i+2)
	    Out(i+3) = a / level * Out(i+3)
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
	subroutine ModGet(calget,tvis,nchan,a,level)
c
	implicit none
	integer tvis
	real a
	integer nchan
	external calget
c
c  Determine the calibrator flux.
c
c  Input:
c    tvis	Handle of the visibility data file.
c    nchan	Number of channels.
c    calget	Service routine to return the flux of the calibrator.
c    level	The default flux or level of the model.
c  Output:
c    a		Flux of the calibrator.
c------------------------------------------------------------------------
	include 'model.h'
	character source*16,source1*16,type*1,line*80
	logical more,update,isplanet
	real flux,freq,level
	double precision day,dfreq,delta
	integer n,iostat,length
c
c  Externals.
c
	integer binsrcha
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
     *	  calcur = binsrcha(source,cals,ncals)
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
	    flux = 0.
	    call CalGet(' ',source1,freq,100.,day,1000.,flux,iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error determining flux of '//source)
	      write(line,'(a,f8.3,a)') 'Setting flux to ',level,' Jy'
	      call output(line)
	      flux = level
	    endif
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
	include 'mirconst.h'
	double precision coord(3)
	real plmaj,plmin,plangle,pltb,u,v,flux,cosi,sini,beta,omega
	character type*1
	integer length
	logical update
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
c
	call uvprobvr(tvis,'coord',type,length,update)
	if(type.ne.'d'.or.length.lt.2.or.length.gt.3)
     *	  call bug('f','Screwy uvw coordinate, in ModPlant')
	call uvgetvrd(tvis,'coord',coord,length)
	u = coord(1)
	v = coord(2)
	call uvgetvrr(tvis,'plmaj',plmaj,1)
	call uvrdvrr(tvis,'plmin',plmin,plmaj)
	call uvrdvrr(tvis,'plangle',plangle,0.)
	call uvgetvrr(tvis,'pltb',pltb,1)
c
c  Unit conversion.
c
	plangle = PI/180 * plangle
	plmaj = PI * plmaj / 180 / 3600 
	plmin = PI * plmin / 180 / 3600
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
	beta = PI * sqrt((plmaj*(u*cosi-v*sini))**2
     *	  	       + (plmin*(u*sini+v*cosi))**2)
	omega = pi/4 * plmaj*plmin
	flux = omega * 2*(HMKS*1e26)/(CMKS*CMKS)*(freq**3*1e27)/
     *	 ( exp(((HMKS/KMKS)*1e9)*freq/pltb) - 1. )
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

