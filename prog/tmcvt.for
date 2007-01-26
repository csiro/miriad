c************************************************************************
	program tmcvt
	implicit none
c
c= tmcvt - Convert time-multiplexed polarization visibilities to Stokes.
c& rjs
c: uv analysis
c+
c	TMCVT converts time-multiplexed polarization observations to
c	Stokes parameters, applying any leakage correction in the process. 
c	For an observation where polarizations are time multiplexed
c	(i.e. four polarization states are not simultaneously measured),
c	there is no simple way to convert between raw polarization
c	visibilities and Stokes parameters. This task operates by finding
c	the four polarizations measurements at a given instant which result in
c	the minimum time mismatch. It then uses this set in the conversion process.
c@ vis
c	The names of the input uv data sets. Wildcards are supported.
c	No default.
c@ line
c	Standard line parameter, with standard defaults. See the help on
c	"line" for more information.
c@ select
c	Standard visibility data selection. See the help on "select" for
c	more information. The default is to select all data.
c@ out
c	The name of the output uv data set. No default.
c@ stokes
c	Several Stokes parameters of the output. Possible values are
c	i, q, u and v. There is no default - this parameter must be
c	specified.
c@ interval
c	Time interval tolerance, in minutes. This sets the maximum mismatch
c	between polarization correlations that can be tolerated. The default
c	is 10 minutes,
c@ options
c	Extra processing options. Several options can be given,
c	separated by commas. Minimum match is supported.
c	  nopol     Do not apply polarization leakage correction.
c	  nocal     Do not apply antenna gain correction.
c	  nopass    Do not apply bandpass calibration correction.
c	  pseudo    By default, tmcvt generates a visibility stream where
c	            the error from time mismatches is minimized for each output
c	            Stokes visibility. This usually results with different
c	            Stokes visibilities for a given baseline having different
c	            time tags. The "pseudo" option causes tmcvt to generate an output
c	            dataset where all the Stokes visibilities for a given baseline
c	            are pseudo-simultaneous (same time tag). This results in larger
c	            errors, but may be more convenient if you must form an output
c	            image where the beams are the same for the different Stokes
c	            parameters, or where you are going to written out to a FITS
c	            file.
c--
c  History:
c    08jan07 rjs  Original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXVIS,MAXPOL
	parameter(MAXVIS=1000000,MAXPOL=4)
	character version*(*)
	parameter(version='tmcvt: version 1.0 08-Jan-07')
c
	integer npol,pols(MAXPOL)
	logical dopol,docal,dopass
	character uvflags*8,out*64
	real interval
	integer first(MAXVIS),last(MAXVIS),nvis
c
c  Get the inputs.
c
	call output(version)
	call keyini
	uvflags = 'dl3'
	call getopt(dopol,docal,dopass)
	if(dopass)uvflags(4:4) = 'f'
	if(docal) uvflags(5:5) = 'c'
	call uvdatInp('vis',uvflags)
	call keyr('interval',interval,10.0)
	if(interval.le.0)
     *	  call bug('f','Invalid value for interval parameter')
	interval = interval / (24.0*60.0)
	call getstoke(MAXPOL,pols,npol)
	if(npol.eq.0)
     *	  call bug('f','Output Stokes parameters must be given')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
	call keyfin
c
c  First pass determines all the data that we want to process.
c
	call output('Pass 1: Determine best combinations ...')
	call explore(first,last,nvis,MAXVIS,pols,npol,dopol,interval)
c
c  Second pass. Actually process the data.
c
	call output('Pass 2: Generate the output dataset ...')
	call uvdatRew()
	call generate(out,pols,npol,docal,dopol,dopass,
     *					first,last,nvis,version)
c
c  All said and done.
c
	end
c************************************************************************
	subroutine generate(out,pols,npol,docal,dopol,dopass,
     *					first,last,nvis,version)
c
	implicit none
	integer nvis,first(nvis),last(nvis),npol,pols(npol)
	logical docal,dopol,dopass
	character out*(*),version*(*)
c
c  Generate the output data.
c
c------------------------------------------------------------------------
	integer MAXPOL
	parameter(MAXPOL=4)
	include 'maxdim.h'
	logical doinit,ok
	integer lIn,lOut,i1,i2,bl,blmax,pidxt(MAXPOL),npidx,poff
	integer ifirst,ilast,vidx,nchan,nants,i,pol,ipol
	character ltype*16,line*80
	logical flags(MAXCHAN),buffered(MAXBASE),doleak1,doleak2
	real chi1,chi2,jyperk,inttime
	complex data(MAXCHAN),leak1(2,MAXANT),leak2(2,MAXANT)
	double precision preamble(5)
c
c  Externals.
c
	logical hdprsnt,uvdatOpn
c
	call bufini
	ifirst = 1
	ilast = 1
	vidx = 0
	doinit = .true.
	dowhile(uvdatOpn(lIn))
	  call uvdatGta('ltype',ltype)
	  call varInit(lIn,ltype)
	  if(.not.docal.and.hdprsnt(lIn,'gains'))call bug('w',
     *		'Gain table is being ignored')
	  if(.not.dopass.and.hdprsnt(lIn,'bandpass'))call bug('w',
     *		'Bandpass table is being ignored')
	  call checkpol(lIn,dopol,
     *		doleak1,doleak2,leak1,leak2,MAXANT,nants)
	  if(doinit)then
	    doinit = .false.
	    call uvopen(lOut,out,'new')
	    call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	    call hdcopy(lIn,lOut,'history')
	    call hisopen(lOut,'append')
	    line = 'TMCVT: Miriad '//version
	    call hiswrite(lOut,line)
	    call hisinput(lOut,'TMCVT')
	    call hisclose(lOut)
	  endif
	  call varOnit(lIn,lOut,ltype)
	  blmax = 0
	  poff = 100
c
c  Loop over all visibilities in this file.
c
	  call uvdatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    vidx = vidx + 1
c
c  Does this record contain some good data.
c
	    ok = .false.
	    do i=1,nchan
	      ok = ok.or.flags(i)
	    enddo
c
	    if(ok)then
	      call uvrdvri(lIn,'pol',pol,1)
	      ipol = -pol - poff
	      if(ipol.lt.1.or.ipol.gt.MAXPOL)then
	        call psetup(pol,pols,npol,doleak1,poff,pidxt,npidx)
	        ipol =  -pol - poff
	      endif
	      ok = pidxt(ipol).ne.0
	    endif
c
c  Process this record, if needed.
c  Convert to a baseline, and keep our baseline records up-to-date.
c
	    if(ok)then
	      call basant(preamble(5),i1,i2)
	      bl = (i2*(i2-1))/2 + i1
	      if(bl.gt.blmax)then
		do i=blmax+1,bl
		  buffered(i) = .false.
		enddo
		blmax = bl
	      endif
c
c  Catch up on the pointers into the "first" and "last" arrays.
c
	      dowhile(first(ifirst).lt.vidx)
	        ifirst = ifirst + 1
	      enddo
	      dowhile(last(ilast).lt.vidx)
	        ilast = ilast + 1
	      enddo
c
c  Buffer and flush, as needed.
c
	      if(vidx.eq.first(ifirst))then
		if(buffered(bl))then
		  call bug('f','Algorithm inconsist 1')
	        endif
		buffered(bl) = .true.
	      endif
	      if(buffered(bl))then
		call uvrdvrr(lIn,'chi2',chi2,0.0)
		call uvrdvrr(lIn,'chi',chi1,0.0)
		chi1 = chi1 - chi2
		call uvrdvrr(lIn,'inttime',inttime,10.0)
		call bufadd(bl,preamble,ipol,chi1,chi2,inttime,
     *						data,flags,nchan)
	      endif
	      if(vidx.eq.last(ilast))then
		call varcopy(lIn,lOut)
		call uvdatGtr('jyperk',jyperk)
		call uvputvrr(lOut,'jyperk',jyperk,1)
		call bufflush(lOut,bl,pols,npol,poff.eq.0,
     *			doleak1,doleak2,leak1,leak2,nants)
		buffered(bl) = .false.
	      endif
	    endif
c
c  Loop the loop.
c
	    call uvdatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
c
c  At this stage, nothing should be buffered. Check this for consistency.
c
	  do bl=1,blmax
	    if(buffered(bl))call bug('f','Algorithm inconsistency 2')
	  enddo
	  call uvdatCls()
	enddo
c
c  All done.
c
	call uvclose(lOut)
	end
c************************************************************************
	subroutine bufini
c
	implicit none
c
c  This adds the first record of a new record to a buffer.
c------------------------------------------------------------------------
	include 'tmcvt.h'
c
	blmax = 0
	end
c************************************************************************
	subroutine bufadd(bl,preambl,ipol,chi1,chi2,inttime,
     *						data,flags,nchan)
c
	implicit none
	integer bl,ipol,nchan
	double precision preambl(5)
	real chi1,chi2,inttime
	complex data(nchan)
	logical flags(nchan)
c
c  This adds the first record of a new record to a buffer.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'tmcvt.h'
	integer i
	real tint,chi
	include 'mem.h'
c
c  Initialise the buffers up to the current baseline.
c
	if(bl.gt.blmax)then
	  do i=blmax+1,bl
	    cnt(i) = 0
	  enddo
	  blmax = bl
	endif
c
c  If its new, initialise a set of ancillary information.
c
	if(cnt(bl).eq.0)then
	  preamble(1,bl) = inttime*preambl(1)
	  preamble(2,bl) = inttime*preambl(2)
	  preamble(3,bl) = inttime*preambl(3)
	  preamble(iTime,bl) = inttime*preambl(iTime)
	  preamble(iBl,bl) = preambl(iBl)
	  preamble(iChi1,bl) = inttime*chi1
	  preamble(iChi2,bl) = inttime*chi2
	  nchans(bl) = nchan
	  do i=1,MAXPOL
	    tints(i,bl) = 0
	    pntc(i,bl) = 0
	  enddo
	else
c
c  Otherwise add in the current contribution.
c  Remember to unwrap the angles before adding them in.
c
	  preamble(1,bl) = preamble(1,bl) + inttime*preambl(1)
	  preamble(2,bl) = preamble(2,bl) + inttime*preambl(2)
	  preamble(3,bl) = preamble(3,bl) + inttime*preambl(3)
	  preamble(iTime,bl) = preamble(iTime,bl) + 
     *			         inttime*preambl(iTime)
	  tint = 0
	  do i=1,MAXPOL
	    tint = tint + tints(i,bl)
	  enddo
c
	  chi = preamble(iChi1,bl) / tint
	  chi = chi1 - 2*PI*nint((chi1-chi)/(2*PI))
	  preamble(iChi1,bl) = preamble(iChi1,bl) + inttime*chi
	  chi = preamble(iChi2,bl) / tint
	  chi = chi2 - 2*PI*nint((chi2-chi)/(2*PI))
	  preamble(iChi2,bl) = preamble(iChi2,bl) + inttime*chi
	endif
	cnt(bl) = cnt(bl) + 1
	tints(ipol,bl) = tints(ipol,bl) + inttime
	if(nchan.ne.nchans(bl))call bug('f',
     *	  'Inconsistency in the number of channels')
c
c  Now add it this baseline data.
c
	if(pntc(ipol,bl).eq.0)then
	  call memalloc(pntc(ipol,bl),nchan,'c')
	  call memalloc(pntr(ipol,bl),nchan,'r')
	  call bufadd1(memc(pntc(ipol,bl)),memr(pntr(ipol,bl)),
     *		data,flags,nchan,inttime)
	else
	  call bufadd2(memc(pntc(ipol,bl)),memr(pntr(ipol,bl)),
     *		data,flags,nchan,inttime)
	endif
c
	end
c************************************************************************
	subroutine bufadd1(bdata,bwts,data,flags,nchan,inttime)
c
	implicit none
	integer nchan
	complex bdata(nchan),data(nchan)
	logical flags(nchan)
	real bwts(nchan),inttime
c
c  Initialise a buffer with a record.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  if(flags(i))then
	    bwts(i) = inttime
	    bdata(i) = inttime*data(i)
	  else
	    bwts(i) = 0
	    bdata(i) = 0
	  endif
	enddo
c
	end
c************************************************************************
	subroutine bufadd2(bdata,bwts,data,flags,nchan,inttime)
c
	implicit none
	integer nchan
	complex bdata(nchan),data(nchan)
	logical flags(nchan)
	real bwts(nchan),inttime
c
c  Initialise a buffer with a record.
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  if(flags(i))then
	    bwts(i) = bwts(i) + inttime
	    bdata(i) = bdata(i) + inttime*data(i)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine bufflush(lOut,bl,pols,npol,circ,
     *			doleak1,doleak2,leak1,leak2,nants)
c
	implicit none
	integer lOut,bl,npol,pols(npol),nants
	logical circ,doleak1,doleak2
	complex leak1(2,nants),leak2(2,nants)
c
c------------------------------------------------------------------------
	integer RR,LL,RL,LR,XX,YY,XY,YX
	parameter(RR=1,LL=2,RL=3,LR=4,XX=1,YY=2,XY=3,YX=4)
	integer StokesI,StokesQ,StokesU,StokesV
	parameter(StokesI=1,StokesQ=2,StokesU=3,StokesV=4)
	include 'tmcvt.h'
	include 'mem.h'
	complex out(MAXCHAN),coeff(MAXPOL,MAXPOL)
	logical flags(MAXCHAN)
	integer i,i1,i2,np,ip
	real tint,chi1,chi2,inttime
	double precision preambl(5)
c
	if(cnt(bl).le.0)call bug('f','Inconsistency in bufflush')
	tint = 0
	np = 0
	do i=1,MAXPOL
	  if(tints(i,bl).gt.0)np = np + 1
	  tint = tint + tints(i,bl)
	enddo
c
	preambl(1) = preamble(1,bl) / tint
	preambl(2) = preamble(2,bl) / tint
	preambl(3) = preamble(3,bl) / tint
	preambl(iTime) = preamble(iTime,bl) / tint
	preambl(iBl)   = preamble(iBl,bl)
c
	inttime = tint / np
	chi1 = preamble(iChi1,bl)/tint
	chi2 = preamble(iChi2,bl)/tint
	call basant(preambl(iBl),i1,i2)
c
	call getcoeff(i1,i2,circ,doleak1,doleak2,leak1,leak2,nants,
     *					chi1,chi2,coeff)
c
	call uvputvri(lOut,'npol',npol,1)
	call uvputvrr(lOut,'inttime',inttime,1)
	do i=1,npol
	  ip = pols(i)
c
c  Handle polarisations where leakage correction is being performed.
c
	  if(doleak1.or.doleak2)then
	    call buffl4(nchans(bl),out,flags,
     *	        memc(pntc(1,bl)),memr(pntr(1,bl)),coeff(1,ip),
     *	        memc(pntc(2,bl)),memr(pntr(2,bl)),coeff(2,ip),
     *	        memc(pntc(3,bl)),memr(pntr(3,bl)),coeff(3,ip),
     *		memc(pntc(4,bl)),memr(pntr(4,bl)),coeff(4,ip))
c
c  Handle circular polarisations with no leakage.
c
	  else if(circ)then
	    if(ip.eq.StokesI.or.ip.eq.StokesV)then
	      call buffl2(nchans(bl),out,flags,
     *		memc(pntc(RR,bl)),memr(pntr(RR,bl)),coeff(RR,ip),
     *		memc(pntc(LL,bl)),memr(pntr(LL,bl)),coeff(LL,ip))
	    else if(ip.eq.StokesQ.or.ip.eq.StokesU)then
	      call buffl2(nchans(bl),out,flags,
     *		memc(pntc(RL,bl)),memr(pntr(RL,bl)),coeff(RL,ip),
     *		memc(pntc(LR,bl)),memr(pntr(LR,bl)),coeff(LR,ip))
	    else
	      call bug('f','Invalid polarisation type')
	    endif
c
c  Handle linear polarisations with no leakage.
c
	  else
	    if(ip.eq.StokesI)then
	      call buffl2(nchans(bl),out,flags,
     *		memc(pntc(XX,bl)),memr(pntr(XX,bl)),coeff(XX,ip),
     *		memc(pntc(YY,bl)),memr(pntr(YY,bl)),coeff(YY,ip))
	    else if(ip.eq.StokesQ.or.ip.eq.StokesU)then
	      call buffl4(nchans(bl),out,flags,
     *	        memc(pntc(XX,bl)),memr(pntr(XX,bl)),coeff(XX,ip),
     *	        memc(pntc(YY,bl)),memr(pntr(YY,bl)),coeff(YY,ip),
     *	        memc(pntc(XY,bl)),memr(pntr(XY,bl)),coeff(XY,ip),
     *		memc(pntc(YX,bl)),memr(pntr(YX,bl)),coeff(YX,ip))
	    else if(ip.eq.StokesV)then
	      call buffl2(nchans(bl),out,flags,
     *		memc(pntc(XY,bl)),memr(pntr(XY,bl)),coeff(XY,ip),
     *		memc(pntc(YX,bl)),memr(pntr(YX,bl)),coeff(YX,ip))
	    else
	      call bug('f','Invalid polarisation type')
	    endif
	  endif
	  call uvputvri(lOut,'pol',ip,1)
	  call uvwrite(lOut,preambl,out,flags,nchans(bl))
	enddo
c
c  Now free up the memory and reset the counter back to 0.
c
	do i=1,MAXPOL
	  if(pntr(i,bl).ne.0)call memfree(pntr(i,bl),nchans(bl),'r')
	  if(pntc(i,bl).ne.0)call memfree(pntc(i,bl),nchans(bl),'c')
	enddo
	cnt(bl) = 0
c
	end
c************************************************************************
	subroutine getcoeff(ia,ib,circ,doleak1,doleak2,leak1,leak2,
     *						nants,chi1,chi2,coeff)
c
	implicit none
	integer ia,ib,nants
	logical circ,doleak1,doleak2
	complex leak1(2,nants),leak2(2,nants),coeff(4,4)
	real chi1,chi2
c
c  Determine coefficients to convert raw polarisation correlations into
c  Stokes parameters.
c
c  Input:
c    i1,i2
c    circ
c    doleak1,doleak2
c    leak1,leak2
c    nants
c  Output:
c    coeff
c------------------------------------------------------------------------
	integer i1(4),i2(4),pcvt(4)
	integer i,j,p
	complex c1(2,2),c2(2,2),c(4,4)
	data i1/1,1,2,2/
	data i2/1,2,1,2/
	data pcvt/1,3,4,2/
c
c  Get the inverse of the Jones matrices for the two antennas.
c
	call getco1(circ,doleak1,doleak2,leak1(1,ia),leak2(1,ia),
     *						chi1,chi2,c1)
	call getco1(circ,doleak1,doleak2,leak1(1,ib),leak2(1,ib),
     *						chi1,chi2,c2)
c
c  Convert this to a inverse of the Muller matrix, which relates the
c  measured to ideal polarization correlations.
c
	do j=1,4
	  do i=1,4
	    c(i,j) = c1(i1(i),i1(j))*conjg(c2(i2(i),i2(j)))
	  enddo
	enddo
c
c  Adjust the inverse Muller matrix to convert between measured polarizations
c  and Stokes visibilities. Also put into an XX,YY,XY,YX rather than XX,XY,YX,YY order.
c  
	if(circ)then
	  do i=1,4
	    p = pcvt(i)
	    coeff(p,1) = 0.5*(c(i,1)+c(i,4))
	    coeff(p,2) = 0.5*(c(i,2)+c(i,3))
	    coeff(p,3) = cmplx(0.0,0.5)*(c(i,2)-c(i,3))
	    coeff(p,4) = 0.5*(c(i,1)-c(i,4))
	  enddo
	else
	  do i=1,4
	    p = pcvt(i)
	    coeff(p,1) = 0.5*(c(i,1)+c(i,4))
	    coeff(p,2) = 0.5*(c(i,1)-c(i,4))
	    coeff(p,3) = 0.5*(c(i,2)+c(i,3))
	    coeff(p,4) = cmplx(0.0,0.5)*(c(i,2)-c(i,3))
	  enddo
	endif
c
	end
c************************************************************************
	subroutine getco1(circ,doleak1,doleak2,leak1,leak2,chi1,chi2,c)
c
	implicit none
	logical circ,doleak1,doleak2
	complex leak1(2),leak2(2),c(2,2)
	real chi1,chi2
c
c  Determine the inverse of the Jones matrix for a given antenna.
c
c------------------------------------------------------------------------
	complex det,temp
	complex leak0(2),c1(2,2),c2(2,2)
	data leak0/(0.0,0.0),(0.0,0.0)/
c
	if(doleak1)then
	  call getj(circ,chi1,leak1,c)
	else
	  call getj(circ,chi1,leak0,c)
	endif
	if(doleak2)then
	  c1(1,1) = c(1,1)
	  c1(2,1) = c(2,1)
	  c1(1,2) = c(1,2)
	  c1(2,2) = c(2,2)
	  call getj(circ,chi2,leak2,c2)
	  c(1,1) = c2(1,1)*c1(1,1) + c2(2,1)*c1(1,2)
	  c(2,1) = c2(1,1)*c1(2,1) + c2(2,1)*c1(2,2)
	  c(1,2) = c2(1,2)*c1(1,1) + c2(2,2)*c1(1,2)
	  c(2,2) = c2(1,2)*c1(2,1) + c2(2,2)*c1(2,2)
	endif
	det = 1/(c(1,1)*c(2,2) - c(2,1)*c(1,2))
	temp =   c(1,1)
	c(1,1) =  det*c(2,2)
	c(2,2) =  det*temp
	c(2,1) = -det*c(2,1)
	c(1,2) = -det*c(1,2)
c
	end
c************************************************************************
	subroutine getj(circ,chi,leak,c)
c
	implicit none
	logical circ
	real chi
	complex leak(2),c(2,2)
c------------------------------------------------------------------------
	real coschi,sinchi
c
	coschi = cos(chi)
	sinchi = sin(chi)
	if(circ)then
	  c(1,1) = cmplx(coschi,sinchi)
	  c(2,1) = leak(1)*cmplx(coschi,-sinchi)
	  c(1,2) = leak(2)*cmplx(coschi, sinchi)
	  c(2,2) = cmplx(coschi,-sinchi)
	else
	  c(1,1) = coschi - leak(1)*sinchi
	  c(2,1) = sinchi + leak(1)*coschi
	  c(1,2) = leak(2)*coschi - sinchi
	  c(2,2) = leak(2)*sinchi + coschi
	endif
c
	end

c************************************************************************
	subroutine buffl2(nchan,out,flags,in1,wt1,coeff1,in2,wt2,coeff2)
c
	implicit none
	integer nchan
	complex out(nchan),in1(nchan),in2(nchan),coeff1,coeff2
	real wt1(nchan),wt2(nchan)
	logical flags(nchan)
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  flags(i) = wt1(i).gt.0.and.wt2(i).gt.0
	  if(flags(i))then
	    out(i) = coeff1*in1(i)/wt1(i) + coeff2*in2(i)/wt2(i)
	  else
	    out(i) = (0.,0.)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine buffl4(nchan,out,flags,in1,wt1,coeff1,in2,wt2,coeff2,
     *					  in3,wt3,coeff3,in4,wt4,coeff4)
c
	implicit none
	integer nchan
	complex out(nchan),in1(nchan),in2(nchan),in3(nchan),in4(nchan)
	complex coeff1,coeff2,coeff3,coeff4
	real wt1(nchan),wt2(nchan),wt3(nchan),wt4(nchan)
	logical flags(nchan)
c------------------------------------------------------------------------
	integer i
c
	do i=1,nchan
	  flags(i) = wt1(i).gt.0.and.wt2(i).gt.0.and.
     *		     wt3(i).gt.0.and.wt4(i).gt.0
	  if(flags(i))then
	    out(i) = coeff1*in1(i)/wt1(i) + coeff2*in2(i)/wt2(i) +
     *		     coeff3*in3(i)/wt3(i) + coeff4*in4(i)/wt4(i)
	  else
	    out(i) = (0.,0.)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine checkpol(lIn,dopol,
     *		doleak1,doleak2,leak1,leak2,mants,nants)
c
	implicit none
	integer lIn,mants,nants
	logical dopol,doleak1,doleak2
	complex leak1(2,mants),leak2(2,mants)
c
c  Check if we need to do polarisation correction, and load the leakages
c  if needed.
c
c------------------------------------------------------------------------
	logical doleak,updated
	character type*4
	integer length,ntemp
c
c  Externals.
c
	logical hdprsnt
c
	doleak = hdprsnt(lIn,'leakage')
	if(.not.dopol.and.doleak)call bug('w',
     *		'Polarization leakage table is being ignored')
	doleak1 = doleak.and.dopol
	doleak2 = hdprsnt(lIn,'leakage2')
	doleak2 = doleak2.and.doleak1
	if(doleak2)then
	  call uvprobvr(lIn,'chi2',type,length,updated)
	  if(type.eq.' ')call bug('w',
     *	      'Second feed angle variation (chi2) not found in dataset')
	endif
c
	if(doleak1)call loadleak(lIn,'leakage',leak1,mants,nants)
	if(doleak2)then
	  call loadleak(lIn,'leakage2',leak2,mants,ntemp)
	  if(ntemp.ne.nants)
     *	    call bug('f','Discrepancy in the number of antennas')
	endif
c
	end
c************************************************************************
	subroutine loadleak(lIn,name,leak,mants,nants)
c
	implicit none
	integer lIn,mants,nants
	character name*(*)
	complex leak(2,mants)
c
c  Load the leakages in from a leakage table.
c
c------------------------------------------------------------------------
	integer item,iostat,ntemp
c
c  Externals.
c
	integer hsize
c
	call haccess(lIn,item,name,'read',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	ntemp = hsize(item)
	nants = (ntemp-8)/16
	if(16*nants+8.ne.ntemp)
     *	  call bug('f','Leakage table size discrepancy')
	if(nants.gt.mants)call bug('f','Leakage table too big for me')
	call hreadr(item,leak,8,16*nants,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	end	
c************************************************************************
	subroutine getstoke(mpol,pols,npol)
c
	implicit none
	integer mpol,pols(mpol),npol
c------------------------------------------------------------------------
	integer MAXPOL
	parameter(MAXPOL=4)
	character types(MAXPOL)*4,cpols(MAXPOL)*4
	integer i
c
c  Externals.
c
	integer polsP2C
c
	data types/'i','q','u','v'/
	call keymatch('stokes',MAXPOL,types,MAXPOL,cpols,npol)
	if(npol.eq.0)call bug('f','Stokes parameters must be given')
	do i=1,npol
	  pols(i) = polsP2C(cpols(i))
	  if(pols(i).lt.1.or.pols(i).gt.4)
     *	    call bug('f','Invalid stokes parameter: '//cpols(i))
	enddo
c
	end
c************************************************************************
	subroutine getopt(dopol,docal,dopass)
c
	implicit none
	logical dopol,docal,dopass
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'nopol   ','nocal   ','nopass  '/
c
	call options('options',opts,present,NOPTS)
	dopol  = .not.present(1)
	docal  = .not.present(2)
	dopass = .not.present(3)
c
	end
c************************************************************************
	subroutine explore(first,last,nvis,mvis,pols,npol,dopol,
     *							interval)
c
	implicit none
	integer mvis,first(mvis),last(mvis),nvis,npol,pols(npol)
	real interval
	logical dopol
c
c  Determine the combinations of data that we are to generate.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXCBUF,MAXPOL
	parameter(MAXCBUF=40000,MAXPOL=4)
	integer head,nxt(MAXCBUF),pnts(MAXBASE)
	double precision times(MAXCBUF)
	integer vidxs(MAXCBUF),pidxs(MAXCBUF)
	integer npidx,pidxt(MAXPOL),poff,itemp
	integer ngood,nbad,nflagged,nwrong,nants,vidx,nchan,pol,i,ipol
	integer pidx,i1,i2,bl
	integer ostatus,onval,ovmin,ovmax,nstatus,nnval,nvmin,nvmax
	integer lIn,vupd
	logical doinit,flags(MAXCHAN),ok,changed,doleak,dotrim,doflush
	double precision preamble(5),otrange,ntrange
	complex data(MAXCHAN)
c
c  Externals.
c
	logical uvdatOpn,uvvarUpd,oncmp,hdprsnt
	character itoaf*8
c
c  Initialise.
c
	ngood = 0
	nbad = 0
	nflagged = 0
	nwrong = 0
c
	doinit = .true.
	nvis = 0
	vidx = 0
c
c  Initialise the linked list.
c
	call linkini(head,nxt,MAXCBUF)
c
c  Dummy statements to keep the lint checker happy.
c
	vidxs(1) = 0
	pidxs(1) = 0
	pnts(1) = 0
	times(1) = 0
	npidx = 0
c
c  Loop over all the input files.
c
	dowhile(uvdatOpn(lIn))
c
c  Set up a monitor of change in observing status.
c
	  call uvvarIni(lIn,vupd)
	  call uvvarSet(vupd,'dra')
	  call uvvarSet(vupd,'ddec')
	  call uvvarSet(vupd,'source')
	  call uvvarSet(vupd,'ra')
	  call uvvarSet(vupd,'dec')
	  call uvvarSet(vupd,'on')
	  call uvvarSet(vupd,'sfreq')
	  call uvvarSet(vupd,'sdf')
	  call uvvarSet(vupd,'nschan')
	  nants = 0
c
c  Do we want to do polarisation leakage correction?
c
	  doleak = dopol
	  if(doleak)doleak = hdprsnt(lIn,'leakage')
c
	  call uvdatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    vidx = vidx + 1
	    changed = uvvarUpd(vupd)
	    if(changed)
     *	      call flushall(head,pnts,nants,nxt,pidxs,vidxs,MAXCBUF,
     *	      npidx,first,last,nvis,mvis,ngood,nbad)
c
c  Check that there is at least some good data in this record.
c
	    ok = .false.
	    do i=1,nchan
	      ok = ok.or.flags(i)
	    enddo
c
c  Check whether this is a polarisation that is desired, and make sure that we are
c  set up if we want this record.
c
	    if(ok)then
	      call uvrdvri(lIn,'pol',pol,1)
	      if(doinit)call psetup(pol,pols,npol,doleak,
     *						poff,pidxt,npidx)
	      ipol = -pol - poff
	      if(ipol.lt.1.or.ipol.gt.MAXPOL)then
	        call flushall(head,pnts,nants,nxt,pidxs,vidxs,MAXCBUF,
     *				npidx,first,last,nvis,mvis,ngood,nbad)
	        call psetup(pol,pols,npol,doleak,poff,pidxt,npidx)
	        ipol =  -pol - poff
	      endif
	      pidx = pidxt(ipol)
	      if(pidx.eq.0)nwrong = nwrong + 1
	    else
	      pidx = 0
	      nflagged = nflagged + 1
	    endif
c
c  These data are good and we want this polarisation. Lets add this to the list
c  to be saved.
c
	    if(pidx.gt.0)then
	      call basant(preamble(5),i1,i2)
	      if(i2.gt.nants)call asetup(i2,nants,pnts,MAXBASE)
	      bl = (i2*(i2-1))/2 + i1
c
c  Get the info about the existing data.
c
	      call measure(pnts(bl),nxt,times,pidxs,vidxs,MAXCBUF,
     *	        npidx,interval,ostatus,onval,otrange,ovmax,ovmin,dotrim)
c
c  Add the new point and get information about the new data.
c
	      itemp = head
	      if(itemp.eq.0)call bug('f','Exhausted list')
	      head = nxt(itemp)
	      nxt(itemp) = pnts(bl)
	      times(itemp) = preamble(4)
	      vidxs(itemp) = vidx
	      pidxs(itemp) = pidx
	      pnts(bl) = itemp
c	    
	      call measure(pnts(bl),nxt,times,pidxs,vidxs,MAXCBUF,
     *	        npidx,interval,nstatus,nnval,ntrange,nvmax,nvmin,dotrim)
c
c  Determine what to do.
c
	      doflush = oncmp(ostatus,onval,otrange,
     *			      nstatus,nnval,ntrange)
c
c  Flush out the data if needed. Keep track of a count of records accepted
c  and discarded.
c
	      if(doflush)then
	        nvis = nvis + 1
	        if(nvis.gt.mvis)
     *		  call bug('f','Overflowed first/last buffer')
	        first(nvis) = ovmin
	        last(nvis)  = ovmax
	        ngood = ngood + onval
	      else
	        nbad = nbad + onval + 1 - nnval
	      endif
c
c  Discard data out of the list that are no longer useful.
c
	      if(dotrim.or.doflush)call trim(doflush,head,
     *				pnts(bl),nxt,times,MAXCBUF,interval)
	    endif
c
c  Loop the loop.
c
	    call uvdatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
c
c  Flush the remaining buffered information.
c
	  call flushall(head,pnts,nants,nxt,pidxs,vidxs,MAXCBUF,
     *			npidx,first,last,nvis,mvis,ngood,nbad)
	  call uvdatCls
	enddo
c
c  Give the user some messages about the data that were accepted and rejected.
c
	if(nflagged.gt.0)call bug('w',
     *	  'Visibility records completely flagged: '//itoaf(nflagged))
	if(nbad.gt.0)call bug('w',
     *	  'Visibility records without matching records discarded:'
     *						//itoaf(nbad))
	if(nwrong.gt.0)call output(
     *	  'Visibility records of wrong polarisation discarded: '
     *						//itoaf(nwrong))
	if(ngood.eq.0)call bug('f','No data were accepted')
	call output('Visibility records accepted: '//itoaf(ngood))
c
c  Sort the arrays of indices.
c
	call sorti(first,nvis)
	call sorti(last,nvis)
c
c  Terminate these buffers with something.
c
	nvis = nvis + 1
	if(nvis.gt.mvis)
     *	  call bug('f','Overflowed first/last buffer')
	first(nvis) = vidx+1
	last(nvis)  = vidx+1
c
c  Check. 
c
	if(ngood+nbad+nwrong+nflagged.ne.vidx)
     *			 call bug('f','Count mismatch in explore')
	end
c************************************************************************
	subroutine linkini(head,nxt,nentry)
c
	implicit none
	integer head,nentry,nxt(nentry)
c
c------------------------------------------------------------------------
	integer i
c
	head = 1
	do i=1,nentry-1
	  nxt(i) = i + 1
	enddo
	nxt(nentry) = 0
	end
c************************************************************************
	subroutine trim(doflush,head,pnt,nxt,times,nentry,interval)
c
	implicit none
	logical doflush
	integer head,pnt,nentry,nxt(nentry)
	double precision times(nentry)
	real interval
c
c  Update the list of information for this baseline.
c
c------------------------------------------------------------------------
	double precision tmin,tmax
	logical trimming
	integer idx,igood
c
	if(pnt.eq.0)return
	tmin = times(pnt)
	tmax = tmin
c
c  Entry "igood" is the last good entry.
c
	igood = pnt
	idx = pnt
	trimming = doflush
	dowhile(nxt(idx).ne.0)
	  idx = nxt(idx)
	  trimming = trimming.or.
     *		      abs(tmin-times(idx)).gt.interval.or.
     *		      abs(tmax-times(idx)).gt.interval
	  if(.not.trimming)igood = idx
	  tmin = min(tmin,times(idx))
	  tmax = max(tmax,times(idx))
	enddo
c
	if(nxt(igood).ne.0)then
	  nxt(idx) = head
	  head = nxt(igood)
	  nxt(igood) = 0
	endif
c
	end
c************************************************************************
	logical function oncmp(ostatus,onval,otrange,
     *			       nstatus,nnval,ntrange)
c
	implicit none
	integer ostatus,onval,nstatus,nnval
	double precision otrange,ntrange
c
c  This routine determines whether it is "better" to keep the current data,
c  and add the new visibility record to it, or to flush out the current
c  data and start accumulating afresh with the new visibility record.
c
c  Output:
c    doflush	If true, then it is better to flush out the current data
c		and start afresh.
c------------------------------------------------------------------------
c  STBAD  - Insufficient data to form the requested Stokes parameters.
c  STGOOD - Sufficient data, and there are an equal number of all the
c	    polarisation record.
c  STOK   - Sufficient data, but there are more of some polarisations
c	    than others.
c
	integer STBAD,STOK,STGOOD
	parameter(STBAD=0,STOK=1,STGOOD=2)
c
	logical doflush
c
c  Never flush if there are not enough polarisations to work.
c
	if(ostatus.eq.STBAD)then
	  doflush = .false.
c
c  Always flush if the old set is better than the new set.
c
	else if(ostatus.gt.nstatus)then
	  doflush = .true.
c
c  At this stage the old and new are equally "complete". Do not
c  flush if the new set does not involve dropping data.
c
	else if(onval.lt.nnval)then
	  doflush = .false.
c
c   OK. The new set requires us to drop data. Only do this is the new set
c   looks like its going to get us to a appreciably better set in terms of the
c   spread in times. If the average spacing between polarisations is 70%
c   less for the new set, and there are at least 70% of the data in the
c   next set
c
	else if((nnval-1)*otrange.gt.0.7*(onval-1)*ntrange.and.
     *					10*nnval.ge.7*onval)then
	  doflush = .false.
c
c  Otherwise, we can make an output from the old data. Adding the new data does not
c  look any better than the old set, but would require the dropping of data.
c  Flush and start afresh.
c
	else
	  doflush = .true.
	endif
c
	oncmp = doflush
c	
	end
c***********************************************************************
	subroutine flushall(head,pnts,nants,nxt,pidxs,vidxs,nentry,
     *				npidx,first,last,nvis,mvis,ngood,nbad)
c
	implicit none
	integer head,pnts(*),nants
	integer nentry,nxt(nentry),pidxs(nentry),vidxs(nentry)
	integer mvis,first(mvis),last(mvis),nvis,ngood,nbad,npidx
c
c  Flush out all accummulated data - where there is sufficient data to
c  form the desired polarisations.
c
c------------------------------------------------------------------------
	integer MAXPOL
	parameter(MAXPOL=4)
	integer nbl,bl,i,ip,idx,vmin,vmax,count(MAXPOL),nval
	logical ok
c
	if(npidx.gt.MAXPOL)call bug('f',
     *			'Too many polarisations, in flushall')
c
	nbl = (nants*(nants+1))/2
	do bl=1,nbl
	  if(pnts(bl).gt.0)then
c
c  Go through the data for this baseline, and make sure we have all
c  the data that we need.
c
	    do i=1,npidx
	      count(i) = 0
	    enddo
	    idx = pnts(bl)
	    ip = 0
	    vmin = vidxs(idx)
	    vmax = vmin
	    dowhile(idx.ne.0)
	      count(pidxs(idx)) = count(pidxs(idx)) + 1
	      vmin = min(vmin,vidxs(idx))
	      vmax = max(vmax,vidxs(idx))
	      ip = idx
	      idx = nxt(idx)
	    enddo
c
c  Check.
c
	    nval = 0
	    ok = .true.
	    do i=1,npidx
	      ok = ok.and.count(i).gt.0
	      nval = nval + count(i)
	    enddo
c
c  If we have all the data, save information to remember to handle these
c  data.
c
	    if(ok)then
	      nvis = nvis + 1
	      if(nvis.gt.mvis)call bug('f',
     *			'Buffer overflow, in flushall')
	      first(nvis) = vmin
	      last(nvis)  = vmax
	      ngood = ngood + nval
	    else
	      nbad = nbad + nval
	    endif
c
c  Add the entries back to our list.
c
	    nxt(ip) = head
	    head = pnts(bl)
	    pnts(bl) = 0
	  endif
	enddo
c
	end
c************************************************************************
	subroutine asetup(i2,nants,pnts,mbase)
c
	implicit none
	integer mbase,pnts(mbase),nants,i2
c
c  Initialise the index to the list of antennas.
c
c------------------------------------------------------------------------
	integer p1,p2,i
c
	p1 = (nants*(nants+1))/2 + 1
	p2 = (i2*(i2+1))/2
	if(p2.gt.mbase)call bug('f',
     *		'Too many baselines for me to handle')
c
	do i=p1,p2
	  pnts(i) = 0
	enddo
	nants = i2
c
	end
c************************************************************************
	subroutine psetup(pol,pols,npol,doleak,poff,pidxt,npidx)
c
	implicit none
	integer npol,pols(npol),pol,poff,pidxt(4),npidx
	logical doleak
c
c  Set up for the polarisations that we need.
c------------------------------------------------------------------------
	integer i,j,k
	logical needs(4,4,2)
c                    RR      LL      RL      LR
c                    XX      YY      XY      YX
	data needs /.true., .true., .false.,.false.,
     *		    .false.,.false.,.true., .true.,
     *		    .false.,.false.,.true., .true.,
     *		    .true., .true., .false.,.false.,
     *		    .true., .true., .false.,.false.,
     *		    .true., .true., .true., .true.,
     *		    .true., .true., .true., .true.,
     *		    .false.,.false.,.true., .true./
c
	poff = ((-pol-1)/4)*4
	if(poff.ne.0.and.poff.ne.4)
     *	  call bug('f','Unrecognised polarisation type')
c	
	if(doleak)then
	  npidx = 4
	  do i=1,4
	    pidxt(i) = i
	  enddo
	else
	  do i=1,4
	    pidxt(i) = 0
	  enddo
c
	  npidx = 0
	  k = 1
	  if(poff.eq.4)k=2
	  do j=1,npol
	    do i=1,4
	      if(pidxt(i).eq.0.and.needs(i,pols(j),k))then
		npidx = npidx + 1
		pidxt(i) = npidx
	      endif
	    enddo
	  enddo
	endif
c
	end
c************************************************************************
	subroutine measure(pnt,lists,times,pidxs,vidxs,nentry,
     *	  npidx,interval,status,nval,trange,vmax,vmin,dotrim)
c
	implicit none
	integer pnt,nentry,lists(nentry),pidxs(nentry),vidxs(nentry)
	integer npidx,status,nval,vmax,vmin
	double precision times(nentry),trange
	real interval
	logical dotrim
c
c  Measure the characteristic of adding the a polarisation to the current
c  list.
c
c  Input:
c    pnt
c    lists
c    times
c    pidxs
c    vidxs
c    nentry
c    npidx
c    interval
c  Output:
c    status
c    nval
c    trange
c    vmax,vmin
c------------------------------------------------------------------------
	integer STBAD,STOK,STGOOD
	parameter(STBAD=0,STOK=1,STGOOD=2)
	integer MAXPOL
	parameter(MAXPOL=4)
	integer i,count(MAXPOL),cmin,cmax,idx
	double precision tmin,tmax
c
c  The simple case. There are no data.
c
	if(pnt.eq.0)then
	  status = STBAD
	  nval = 0
	  trange = 0
	  vmax = 0
	  vmin = 0
c
c  Handle data.
c
	else
	  if(npidx.gt.MAXPOL)call bug('f',
     *	    'Too many polarisations in explore')
	  do i=1,npidx
	    count(i) = 0
	  enddo
c
	  vmax = vidxs(pnt)
	  vmin = vmax
	  tmin = times(pnt)
	  tmax = tmin
	  count(pidxs(pnt)) = 1
c
	  idx = lists(pnt)
	  dotrim = .false.
	  dowhile(idx.gt.0)
	    if(abs(tmax-times(idx)).gt.interval.or.
     *	       abs(tmin-times(idx)).gt.interval)then
	      dotrim = .true.
	      idx = 0
	    else
	      count(pidxs(idx)) = count(pidxs(idx))+1
	      tmax = max(tmax,times(idx))
	      tmin = min(tmin,times(idx))
	      vmin = min(vidxs(idx),vmin)
	      vmax = max(vidxs(idx),vmax)
	      idx = lists(idx)
	    endif
	  enddo
c
	  nval = count(1)
	  cmin = nval
	  cmax = cmin
	  do i=2,npidx
	    nval = nval + count(i)
	    cmin = min(cmin,count(i))
	    cmax = max(cmax,count(i))
	  enddo
c
	  if(cmin.eq.0)then
	    status = STBAD
	  else if(cmin.eq.cmax)then
	    status = STGOOD
	  else
	    status = STOK
	  endif
c
	  trange = tmax - tmin
	endif
c
	end
