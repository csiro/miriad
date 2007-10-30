c************************************************************************
	program mospsf
	implicit none
c
c= mospsf - Determine approximate PSF of a mosaic experiment
c& rjs
c: map combination
c+
c	MOSPSF is a MIRIAD task which determines the point spread function
c	for a linear mosaic of dirty images (as produced by INVERT).
c
c	Strictly speaking, the PSF varies with position and frequency.
c	However if the pointing grid is fairly complete and the individual
c	synthesised beam patterns are similar, the PSF is reasonably
c	independent of position. It is also usually a good approximation
c	that it is independent of frequency.
c@ beam
c	This gives the name of the input beam-cube (as produced by INVERT).
c	No default.
c@ out
c	The name of the output point-spread function. It will be the same size
c	as the input.
c@ radec
c	The RA and DEC (either in the form hh:mm:ss,dd:mm:ss or decimals hours
c	and degrees) at which to compute the point-spread function. The default
c	is the reference RA,DEC of the beam-cube.
c@ freq
c	The frequency (in GHz) at which to compute the point-spread function.
c	The default is the reference frequency of the beam-cube. The
c	point-spread function is reasonably independent of frequency for most
c	spectral line observations.
c--
c
c  History:
c    rjs  25oct93 Adapted from LINMOS.
c    nebk 22aug94 Adapt to GETFREQ error status change
c    rjs  26oct94 Complete rewrite to cope with the new INVERT.
c
c  Bugs:
c
c  Program parameters:
c    maxIn	Maximum number of input files.
c    maxlen	Size of buffer to hold all the input file names.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='MosPsf: version 1.0 26-Oct-94')
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	character beam*64,out*64,coin*12
	double precision ra,dec,freq,x(3)
	logical doradec,dofreq
	integer i,nx,ny,npnt,naxis,nsize(MAXNAX),tIn,tOut,pIn,pOut
c
c  Externals.
c
	logical keyprsnt
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('beam',beam,' ')
	if(beam.eq.' ')call bug('f','Input beam must be given')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','Output PSF must be given')
	doradec = .not.keyprsnt('radec')
	call keyt('radec',ra,'hms',0.d0)
	call keyt('radec',dec,'dms',0.d0)
	dofreq = .not.keyprsnt('freq')
	call keyd('freq',freq,0.d0)
	call keyfin
c
c  Open the input and the output.
c
	call xyopen(tIn,beam,'old',3,nsize)
	call coInit(tIn)
	call mosLoad(tIn,npnt)
	nx = nsize(1)
	ny = nsize(2)
	if(npnt.ne.nsize(3))
     *	  call bug('f','Inconsistent number of pointings')
c
c  Set the position at which to compute the PSF.
c
	x(1) = ra
	x(2) = dec
	x(3) = freq
	if(doradec.and.dofreq)then
	  coin = 'op/op/op'
	else if(doradec)then
	  coin = 'op/op/aw'
	else if(dofreq)then
	  coin = 'aw/aw/op'
	else
	  coin = 'aw/aw/aw'
	endif
c
c  Allocate memory and load the input PSF data.
c
	call memAlloc(pIn,nx*ny*npnt,'r')
	call memAlloc(pOut,nx*ny,'r')
c
c  Read the input data.
c
	call GetDat(tIn,memr(pIn),nx,ny,npnt)
c
c  Do the real work.
c
	call mosPnt(tIn,coin,x,memr(pIn),memr(pOut),nx,ny,npnt)	
c
c  Create the output dataset.
c
	call rdhdi(tIn,'naxis',naxis,0)
	naxis = min(naxis-1,MAXNAX)
	do i=3,naxis
	  nsize(i) = 1
	enddo
	call xyopen(tOut,out,'new',naxis,nsize)
	call MakeHd(tIn,tOut,version,naxis)
c
c  Write the output.
c
	call PutDat(tOut,memr(pOut),nx,ny)
c
c  All said and done. Close up.
c
	call xyclose(tOut)
	call MemFree(pOut,nx*ny,'r')
	call MemFree(pIn,nx*ny*npnt,'r')
	call xyclose(tIn)
c
	end
c************************************************************************
	subroutine MakeHd(tIn,tOut,version,naxis)
c
	implicit none
	integer tIn,tOut,naxis
	character version*(*)
c
c  Make a header for the output dataset.
c------------------------------------------------------------------------
	character line*64,ctype*16,numi*2,numo*2
	integer i
	double precision dval
c
c  Externals.
c
	character itoaf*2
c
	integer nkeys
	parameter(nkeys=27)
	character keyw(nkeys)*8
	data keyw/   'bunit   ','crval1  ','crval2  ','ctype1  ',
     *	  'ctype2  ','cdelt1  ','cdelt2  ','crpix1  ','crpix2  ',
     *	  'obstime ','epoch   ','bmaj    ','bmin    ','bpa     ',
     *	  'niters  ','object  ','telescop','observer','restfreq',
     *	  'vobs    ','lstart  ','lstep   ','ltype   ','lwidth  ',
     *    'btype   ','history ','mask    '/
c
c  Copy other parameters.
c
	do i=1,nkeys
	  call hdcopy(tIn,tOut,keyw(i))
	enddo
c
c  Copy the coordinate information about the axes that want.
c
	do i=3,naxis
	  numi = itoaf(i+1)
	  numo = itoaf(i)
	  call rdhda(tIn,'ctype'//numi,ctype,' ')
	  if(ctype.ne.' ')then
	    call wrhda(tOut,'ctype'//numo,ctype)
	    call rdhdd(tIn, 'crpix'//numi,dval,0.d0)
	    call wrhdd(tOut,'crpix'//numo,dval)
	    call rdhdd(tIn, 'crval'//numi,dval,0.d0)
	    call wrhdd(tOut,'crval'//numo,dval)
	    call rdhdd(tIn, 'cdelt'//numi,dval,0.d0)
	    call wrhdd(tOut,'cdelt'//numo,dval)
	  endif
	enddo
c
c  Handle the history.
c
	call hisopen(tOut,'append')
	line = 'MOSPSF: Miriad '//version
	call hiswrite(tOut,line)
	call hisinput(tOut,'MOSPSF')
	call hisclose(tOut)
c
	end
c************************************************************************
	subroutine GetDat(tIn,In,nx,ny,npnt)
c
	implicit none
	integer tIn,nx,ny,npnt
	real In(nx,ny,npnt)
c
c  Read the input data.
c------------------------------------------------------------------------
	integer j,k
c
	do k=1,npnt
	  call xysetpl(tIn,1,k)
	  do j=1,ny
	    call xyread(tIn,j,In(1,j,k))
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine PutDat(tOut,Out,nx,ny)
c
	implicit none
	integer tOut,nx,ny
	real Out(nx,ny)
c
c  Write the output data.
c------------------------------------------------------------------------
	integer j
	do j=1,ny
	  call xywrite(tOut,j,Out(1,j))
	enddo
	end
