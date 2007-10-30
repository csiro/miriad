c************************************************************************
	program transfix
	implicit none
c= transfix -- Correct for leakage near transit of DEC ~ -30 sources.
c& rjs
c: uv analysis
c+
c	This corrects for polarisation leakage caused by errors in
c	the ATCA antenna orientation. The importance of this effect
c	goes inversely with the square of the difference of the source
c	declination from -30. It is most extreme at transit.
c
c	To run this task, you must have the pointing parameter file used
c	at the ATCA during your observation.
c
c	To first order, it is not too important whether this correction is
c	applied before or after gpcal is used, particularly for sources
c	very close to DEC=-30. Some experimentation might be helpful.
c@ vis
c	The name of the input uv data set. No default.
c@ select
c	The normal uv selection commands. The default is to select everything.
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	The default is all channels. Note that if there are multiple
c	spectral windows, the fitting process is performed on each window
c	separately.
c@ pparams
c	Pointing parameter table. These tables are stored on LEON at
c	Narrabri in the file at$log:pparams.log. It is usually entered as an
c	indirect parameter file. The table consists of 6 lines (one per
c	antenna, each consisting of 12 numbers (the first number being the
c	anntenna number).
c@ options
c	Task enrichment options. Possible values are
c	  replace  Replace the XY and YX correlations with an estimate
c                  of the error. This option is only meaningful for
c	           a polarisation-calibrated unpolarised dataset.
c	Additionally there are the standard options to turn off calibration:
c	  nocal    Do not apply antenna calibration.
c	  nopol    Do not apply leakage correction.
c	  nopass   Do no apply bandpass calibration.
c	
c@ out
c	The name of the output uv data-set. There is no default name.
c
c  History:
c    27jun97 rjs  Adapted from uvredo
c
c  Bugs:
c------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	parameter(version='Transfix: version 1.0 27-Jun-97')
	integer ATANTS,ATPARS
	parameter(ATANTS=6,ATPARS=12)
c
	character out*64,ltype*16,uvflags*16
	integer lIn,lOut
	real pparams(ATPARS,ATANTS)
	integer i,j,np
	logical replace
c
c  Externals.
c
	logical uvDatOpn
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags,replace)
	call uvDatInp('vis',uvflags)
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
c
c  Get and check the pointing parameter table.
c
	call mkeyr('pparams',pparams,ATANTS*ATPARS,np)
	if(np.eq.0)call bug('f',
     *	  'Pointing parameter table must be given')
	if(np.ne.ATANTS*ATPARS)call bug('f',
     *	  'Incorrect number of values in pointing parameter table')
	do i=1,ATANTS
	  if(nint(pparams(1,i)).ne.i)call bug('f',
     *	    'Pointing parameter table is bad')
	enddo
c
	call keyfin
c
c  Set that we want the raw polarisations.
c
	call uvDatSet('stokes',-5)
	call uvDatSet('stokes',-6)
	call uvDatSet('stokes',-7)
	call uvDatSet('stokes',-8)
c
c  Convert the pointing parameters from arcsec to radians.
c
	do j=1,ATANTS
	  do i=2,ATPARS
	    pparams(i,j) = PI/180./3600. * pparams(i,j)
	  enddo
	enddo
c
c  Open the inputs and the outputs.
c
	if(.not.uvDatOpn(lIn))
     *	  call bug('f','Failed to open the input data-set')
	call uvDatGta('ltype',ltype)
	call VarInit(lIn,ltype)
c
	call uvopen(lOut,out,'new')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call hdcopy(lIn,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'TRANSFIX: Miriad '//version)
	call hisinput(lOut,'TRANSFIX')
	call hisclose(lOut)
	call VarOnit(lIn,lOut,ltype)
c
c  Do the work.
c
	call Process(lIn,lOut,pparams,ATPARS,ATANTS,replace)
c
c  All said and done. Close up shop.
c
	call uvDatCls
	call uvclose(lOut)
	end
c************************************************************************
	subroutine GetOpt(uvflags,replace)
c
	implicit none
	character uvflags*(*)
	logical replace
c
c  Determine extra processing options.
c
c  Output:
c    uvflags
c    replace
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=4)
	logical present(NOPTS)
	character opts(NOPTS)*8
c
	data opts/'nocal   ','nopol   ','nopass  ','replace '/
c
	call options('options',opts,present,NOPTS)
c
c  Determine the flags to pass to the uvDat routines.
c    d - Data selection.
c    l - Linetype processing.
c    b - Input must be a single file.
c    3 - Return w in the preamble.
c    c - Apply gain table.
c    e - Apply leakage correction.
c    f - Apply bandpass correction.
c
	uvflags = 'dlb3'
	if(.not.present(1))uvflags(5:5) = 'c'
	if(.not.present(2))uvflags(6:6) = 'e'
	if(.not.present(3))uvflags(7:7) = 'f'
c
	replace = present(4)
c
	end
c************************************************************************
	subroutine Process(lIn,lOut,pparams,npars,nants,replace)
c
	implicit none
	integer lIn,lOut,npars,nants
	real pparams(npars,nants)
	logical replace
c
c  Do all the real work.
c
c  Input:
c    lIn
c    lOut
c    pparams	Pointing parameters, as retrieved from Narrabri.
c    npars,nants Number of pointing parameters and antennas.
c------------------------------------------------------------------------
	integer PAY,PAX,PFZ
	parameter(PAX=2,PAY=3,PFZ=6)
	include 'maxdim.h'
c
	integer nchan
	double precision preamble(5)
	logical flags(MAXCHAN,4)
	complex data(MAXCHAN,4),Exy,Eyx
	double precision ra,dec,lat,long,ha,az,el,lst
	integer i1,i2,i
	real chi,daz1,daz2,dchi1,dchi2
c
c  Stuff which is common.
c
	call uvputvri(lOut,'npol',4,1)
	call wrhdi(lOut,'npol',4)
c
c  Read the first record.
c
	call uvDatRd(preamble,data(1,1),flags(1,1),MAXCHAN,nchan)
c
c  the default rest frame, if we are doing velocity recomputation
c  and a rest frame has not been given.
c
	dowhile(nchan.gt.0)
	  do i=2,4
	    call uvDatRd(preamble,data(1,i),flags(1,i),MAXCHAN,nchan)
	  enddo
c
c  Get or get azimuth,parallactic angle, latitude, HA and declination.
c
	call uvgetvrd(lIn,'obsra',ra,1)
	call uvgetvrd(lIn,'obsdec',dec,1)
	call uvgetvrd(lIn,'longitu',long,1)
	call uvgetvrd(lIn,'latitud',lat,1)
	call uvgetvrd(lIn,'lst',lst,1)
c
	call parang(ra,dec,lst,lat,chi)
        ha = lst - ra
        el = asin(sin(lat)*sin(dec) + cos(lat)*cos(dec)*cos(ha))
        az = atan2(-cos(dec)*sin(ha),
     *          cos(lat)*sin(dec)-sin(lat)*cos(dec)*cos(ha))
c
c  Determine the error for this baseline.
c
	call basant(preamble(5),i1,i2)
c
	daz1 = ( (-pparams(PAY,i1)*cos(az) + pparams(PAX,i1)*sin(az))
     *		 * sin(el) + pparams(PFZ,i1) ) / cos(el)
	daz2 = ( (-pparams(PAY,i2)*cos(az) + pparams(PAX,i2)*sin(az))
     *		 * sin(el) + pparams(PFZ,i2) ) / cos(el)
	dchi1 = cos(lat)*cos(az)/(cos(dec)*cos(chi)) * daz1
	dchi2 = cos(lat)*cos(az)/(cos(dec)*cos(chi)) * daz2
c
	do i=1,nchan
	  Exy =  dchi1*data(i,2) - dchi2*data(i,1)
	  Eyx = -dchi1*data(i,1) + dchi2*data(i,2)
	  if(replace)then
	    data(i,3) = Exy
	    data(i,4) = Eyx
	  else
	    data(i,3) = data(i,3) - Exy
	    data(i,4) = data(i,4) - Eyx
	  endif
	enddo
c
c  Copy all the variables from the input to the output.
c
	  call VarCopy(lIn,lOut)
c
c  All done. Loop the loop.
c
	  do i=1,4
	    call uvputvri(lOut,'pol',-4-i,1)
	    call uvwrite(lOut,preamble,data(1,i),flags(1,i),nchan)
	  enddo
	  call uvDatRd(preamble,data(1,1),flags(1,1),MAXCHAN,nchan)
	enddo
c
	end

