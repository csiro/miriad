c************************************************************************
	program psrplt
	implicit none
c= psrplt -- Plot pulsar visibility data as a function of bin.
c& rjs
c: uv analysis
c+
c	PSRPLT plots pulsar visibility data as a function of bin number.
c	The normal display is a pulse profile (bin vs real part of the
c	average visibility), but it can also produce a plot against
c	frequency.
c@ vis
c	The name of the input uv data sets. Several can be given. No default.
c@ stokes
c	The Stokes/polarisation parameters to plot. The default is total
c	intensity (stokes=i).
c@ line
c	The normal line-type specification. See the help on "line" for
c	more information. The default is all channels.
c@ select
c	The normal uv selection commands. The default is copy everything.
c	See the help on "select" for more information.
c@ device
c	Normal PGPLOT plotting device specification. No default.
c@ offset
c	The offset (in arcsec) to shift the uv data. The sign convention
c	is the same as the rest of Miriad. See the help on "offset"
c	for more information. The default is no shift.
c@ axis
c	This determines the x- and y-axes of the plot. At the moment, the
c	only possibility for the x-axis is:
c	  bin        Bin number.
c	Whereas the y-axis can be
c	  flux       Plot the flux value. This is the default.
c	  frequency  Produce a bin vs frequency greyscale display.
c	  channel    Produce a bin vs channel greyscale display.
c@ mode
c	This determines what `flux' quantity is plotted. Possible
c	values are:
c         real      Plot the real part of the data. This is the default.
c	  imaginary Plot the imaginary part of the data.
c         amplitude Plot the amplitude of the data.
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match is supported.
c	  nocal     This option suppresses antenna gain calibration. The
c	            default behaviour is to apply antenna calibration.
c	  nopol     This option suppresses polarisation calibration. The
c	            default behaviour is to apply polarisation calibration.
c	  nopass    This option suppresses bandpass calibration. The
c	            default behaviour is to apply bandpass calibration.
c@ log
c	File in which to write the data, if y-axis is amplitude.
c	The default is no logfile.
c--
c  History:
c    rjs  03jun96 Original version.
c    rjs  21aug97 Count the number of accepted correlations.
c    bmg  26nov97 Added log keyword
c    rjs  29feb00 mode keyword to allow plots of real/imag/amp.
c    rjs  08may00 Change incorrect call of keyf to keya.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer MAXPOL,MAXBIN,PolMin,PolMax
	character version*(*)
	parameter(MAXPOL=4,MAXBIN=32,PolMin=-9,PolMax=4)
	parameter(version='PsrPlt: version 1.0 29-Feb-00')
c
	character uvflags*16,device*32,logfile*80,flux*9
	logical docal,dopol,dopass,doshift
	logical dogrey,dochan
c
	integer nchan,nread,nbin,npol,ipol,ibin,i,j,k,nout,tno,mpol
	integer ngood,nbad,llog
	integer polIndx(PolMin:PolMax),pols(MAXPOL)
	double precision preamble(4),offset(2),shift(2)
	complex data(MAXCHAN)
	logical flags(MAXCHAN),dolog
	complex acc(MAXCHAN,MAXBIN,MAXPOL)
	real    wt(MAXCHAN,MAXBIN,MAXPOL),sig2,w
	double precision sfreq(MAXCHAN)
c
	integer NXAXES,NYAXES,NFLUX
	parameter(NXAXES=1,NYAXES=3,NFLUX=3)
	character xaxes(NXAXES)*9,yaxes(NYAXES)*9,xaxis*9,yaxis*9
	character fluxes(NFLUX)*9
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
	integer pgbeg
	character itoaf*8
c
	data xaxes/'bin      '/
	data yaxes/'flux     ','frequency','channel  '/
	data fluxes/'real     ','imaginary','amplitude'/
c
c  Get the user parameters.
c
	call output(version)
	call keyini
	call GetOpt(docal,dopol,dopass)
	call keya('device',device,' ')
	call keymatch('mode',NFLUX,fluxes,1,flux,nout)
	if(nout.eq.0)flux = fluxes(1)
	call keymatch('axis',NXAXES,xaxes,1,xaxis,nout)
 	if(nout.eq.0)xaxis = xaxes(1)
	call keymatch('axis',NYAXES,yaxes,1,yaxis,nout)
	if(nout.eq.0)yaxis = yaxes(1)
	dogrey = yaxis.ne.'flux'
	dochan = yaxis.eq.'channel'
	dolog = (keyprsnt('log').and.(.not.dogrey))
	if(dolog) call keya('log',logfile,' ')
c
c  Determine the shift.
c
	call keyd('offset',shift(1),0.d0)
	call keyd('offset',shift(2),0.d0)
	doshift = abs(shift(1))+abs(shift(2)).gt.0
c
c Determine uvDat parameters flags.
c s: stokes processing, d: data selection, l: linetype
c c: gain calibration,  e: pol calib,      f: bandpass calibration.
c
	uvflags = 'sdl'
	if(docal)  uvflags(5:5) = 'c'
	if(dopol)  uvflags(6:6) = 'e'
	if(dopass) uvflags(7:7) = 'f'
	call uvDatInp('vis',uvflags)
c
	call keyfin
c
c  Set Stokes-I as the default polarisation type.
c
	call uvDatGti('npol',npol)
	if(npol.eq.0)call uvDatSet('stokes',1)
c
c  Open the pgplot device.
c
	if(device.eq.' ')call bug('f','A PGPLOT device must be given')
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Error opening the PGPLOT device')
	endif
	call pgscf(2)
c
c  Initialise.
c
	nbin = 0
	npol = 0
	do i=PolMin,PolMax
	  PolIndx(i) = 0
	enddo
	mpol = MAXPOL
	if(dogrey)mpol = 1
c
	do k=1,mpol
	  do j=1,MAXBIN
	    do i=1,MAXCHAN
	      acc(i,j,k) = 0
	      wt (i,j,k) = 0
	    enddo
	  enddo
	enddo
c
c  Convert the shift to radians.
c
	shift(1) = pi/180/3600 * shift(1)
	shift(2) = pi/180/3600 * shift(2)
	ngood = 0
	nbad  = 0
c
c  Loop the loop until we have no more files.
c
	dowhile(uvDatOpn(tno))
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  if(nchan.eq.1.and.dogrey)
     *		call bug('f','Only one channel to plot against')
	  if(nchan.eq.0)call bug('f','No data present')
	  call uvinfo(tno,'sfreq',sfreq)
	  if(doshift)then
	    call coInit(tno)
	    call coCvt(tno,'ow/ow',shift,'op/op',offset)
	    call coFin(tno)
	  endif
	  nread = nchan
	  dowhile(nread.eq.nchan)
c
c  Determine the polarisation.
c
	    call uvDatGti('pol',ipol)
	    if(PolIndx(ipol).eq.0)then
	      npol = npol + 1
	      if(npol.gt.mpol)
     *		call bug('f','Too many polarisations')
	      PolIndx(ipol) = npol
	      pols(npol) = ipol
	    endif
	    ipol = PolIndx(ipol)
	    call uvrdvri(tno,'bin',ibin,1)
	    if(ibin.gt.nbin)then
	      nbin = ibin
	      if(nbin.gt.MAXBIN)call bug('f','Too many pulsar bins')
	    endif
c
c  Shift the data if necessary.
c
	    if(doshift)then
	      call uvinfo(tno,'sfreq',sfreq)
	      call Shiftit(preamble,sfreq,data,nchan,offset)
	    endif
c
c  Copy the data to the averaging buffer.
c
	    call uvDatGtr('variance',sig2)
	    if(sig2.eq.0)call bug('f','Noise variance is zero!')
	    w = 1/sig2
	    do i=1,nchan
	      if(flags(i))then
		if(flux.eq.'amplitude')then
		  acc(i,ibin,ipol) = acc(i,ibin,ipol) + w*abs(data(i))
		else
		  acc(i,ibin,ipol) = acc(i,ibin,ipol) + w*data(i)
		endif
		wt (i,ibin,ipol) = wt (i,ibin,ipol) + w
		ngood = ngood + 1
	      else
		nbad = nbad + 1
	      endif
	    enddo
c
c  Loop the loop.
c
	    call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	  enddo
	  if(nread.gt.0)call bug('f',
     *		'Number of channels changed while reading data')
	  call uvDatCls
	enddo
	if(nbad.gt.0)call bug('w',
     *	  'Number of flagged correlations rejected: '//itoaf(nbad))
	call output('Number of correlations accepted: '//itoaf(ngood))
	if(ngood.eq.0)call bug('f','No correlations to plot')
c
	if(dogrey)then
	  call FrPlot(sfreq,acc,wt,MAXCHAN,nchan,nbin,pols(1),
     *					flux,dochan)
	else
	  call PrPlot(acc,wt,MAXCHAN,MAXBIN,nchan,npol,nbin,
     *	                 pols,flux,llog,dolog,logfile)
	  continue
	endif
c
	call pgend
	end
c************************************************************************
	subroutine FrPlot(sfreq,acc,wt,mchan,nchan,nbin,pol,
     *						flux,dochan)
c
	implicit none
	integer mchan,nchan,nbin,pol
	logical dochan
	character flux*(*)
	double precision sfreq(nchan)
	complex acc(mchan,nbin)
	real wt(mchan,nbin)
c
c  Make a bin vs frequency plot.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer l
	integer pImage
	real zmin,zmax,tr(6),ymin,ymax,delta
	character title*64
c
c  Externals.
c
	integer len1
	character polsc2p*2
c
c  Allocate some memory to contain the image.
c
	call memAlloc(pImage,nchan*nbin,'r')
	call GetIm(memr(pImage),acc,wt,mchan,nchan,nbin,flux,
     *							zmin,zmax)
c
	if(dochan)then
	  ymin = 1
	  ymax = nchan
	else
	  ymin = sfreq(1)
	  ymax = sfreq(nchan)
	endif
	delta = (ymax - ymin)/(nchan-1)
	tr(1) = 0
	tr(2) = 1
	tr(3) = 0
	tr(4) = ymin - delta
	tr(5) = 0
	tr(6) = delta
	call pgpage
	call pgvstd
	call pgswin(0.5,nbin+0.5,ymin-0.5*delta,ymax+0.5*delta)
	call pgimag(memr(pImage),nbin,nchan,1,nbin,1,nchan,
     *				zmin,zmax,tr)
	call pgbox('BCNST',0.,0,'BCNST',0.,0)
	if(flux.eq.'imaginary')then
	  title = 'Imaginary Part: Stokes = '//polsc2p(pol)
	else if(flux.eq.'real')then
	  title = 'Real Part: Stokes = '//polsc2p(pol)
	else
	  title = 'Ampltitude: Stokes = '//polsc2p(pol)
	endif
	l = len1(title)
	if(dochan)then
	  call pglab('Bin Number','Channel Number',title(1:l))
	else
	  call pglab('Bin Number','Frequency (GHz)',title(1:l))
	endif
c
c  Deallocate the memory now.
c
	call memFree(pImage,nchan*nbin,'r')
	end
c************************************************************************
	subroutine GetIm(Image,Acc,Wt,mchan,nchan,nbin,flux,zmin,zmax)
c
	implicit none
	integer mchan,nbin,nchan
	character flux*(*)
	complex Acc(mchan,nbin)
	real Image(nbin,nchan),Wt(mchan,nbin),zmin,zmax
c------------------------------------------------------------------------
	integer i,j
	real temp
	logical first
c
c  Determine the min and max value.
c
	first = .true.
	do j=1,nbin
	  do i=1,nchan
	    if(Wt(i,j).gt.0)then
	      if(flux.eq.'imaginary')then
		temp = aimag(Acc(i,j))/Wt(i,j)
	      else
		temp = real(Acc(i,j))/Wt(i,j)
	      endif
	      if(first)then
		zmin = temp
		zmax = zmin
		first = .false.
	      else
		zmin = min(zmin,temp)
		zmax = max(zmax,temp)
	      endif
	    endif
	  enddo
	enddo
c
c  Copy the result to the output array, setting undefined values to the
c  min value.
c
	do j=1,nbin
	  do i=1,nchan
	    if(Wt(i,j).gt.0)then
	      if(flux.eq.'imaginary')then
		temp = aimag(Acc(i,j))/Wt(i,j)
	      else
		temp = real(Acc(i,j))/Wt(i,j)
	      endif
	    else
	      temp = zmin
	    endif
	    Image(j,i) = temp
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine PrPlot(acc,wt,mchan,mbin,nchan,npol,nbin,pols,flux,
     + 	 	llog,dolog,logfile)
c
	implicit none
	character logfile*(*),flux*(*)
	integer nchan,npol,nbin,mchan,mbin,llog
	integer pols(npol)
	logical dolog
	complex acc(mchan,mbin,npol)
	real    wt (mchan,mbin,npol)
c------------------------------------------------------------------------
	integer MAXPOL,MAXBIN
	parameter(MAXPOL=4,MAXBIN=32)
	real ymin,ymax,xlo,xhi,ylo,yhi
	real x(MAXBIN,MAXPOL),y(MAXBIN,MAXPOL),loglist(MAXBIN,MAXPOL)
	integer npnt(MAXPOL),i,j,k,iostat,len1	
	complex ctemp
	real vtemp,wtemp
	logical first
	character line*256
c
c  Externals.
c
	character polsc2p*2
c
	if(npol.gt.MAXPOL.or.nbin.gt.MAXBIN)
     *	  call bug('f','Buffer overflow in PrPlot')
c
c  Average and normalise the data.
c
	first = .true.
	do k=1,npol
	  npnt(k) = 0
	  do j=1,nbin
	    ctemp = 0
	    wtemp = 0
	    do i=1,nchan
	      ctemp = ctemp + Acc(i,j,k)
	      wtemp = wtemp + Wt( i,j,k)
	    enddo
	    if(flux.eq.'imaginary')then
	      vtemp = aimag(ctemp)
	    else
	      vtemp = real(ctemp)
	    endif
c
	    if(wtemp.gt.0)then
	      vtemp = vtemp / wtemp
	      npnt(k) = npnt(k) + 1
	      x(npnt(k),k) = j
	      y(npnt(k),k) = vtemp
	      if(first)then
		ymax = vtemp
		ymin = vtemp
		first = .false.
	      else
		ymax = max(vtemp,ymax)
		ymin = min(vtemp,ymin)
	      endif
	    endif
	  enddo
	enddo
c
	call pgrnge(1.,real(nbin),xlo,xhi)
	call pgrnge(ymin,ymax,ylo,yhi)
	call pgpage
	call pgvstd
	call pgswin(xlo,xhi,ylo,yhi)
	call pgbox('BCNST',0.,0,'BCNST',0.,0)
	if(flux.eq.'real')then
	  call Label('Bin Number','Real Part',pols,npol)
	else if(flux.eq.'imaginary')then
	  call Label('Bin Number','Imaginary Part',pols,npol)
	else
	  call Label('Bin Number','Amplitude',pols,npol)
	endif
	do j=1,npol
	  if(npnt(j).gt.0)then
	    call pgsci(j)
	    call pghline(npnt(j),x(1,j),y(1,j),2.0)
	  endif
	enddo
	call pgsci(1)
c
c  Write the data to a file if log option is present
c
      if (dolog) then
         call txtopen(llog,logfile,'new',iostat)
         if(iostat.NE.0) then
            call bug('i','Error opening logfile')
            call bugno('f',iostat)
         endif
	 do i=1,nbin
	   do j=1,npol
	      loglist(i,j)=0.
	      do k=1,npnt(j)
		  if (int(x(k,j)).eq.i) loglist(i,j) = y(k,j)
	      enddo
	   enddo
	 enddo
	 write(line,'(''  Bin   '',4(5x,a2,8x))') 
     +             (polsc2p(pols(i)),i=1,npol)
         call txtwrite(llog,line,len1(line),iostat)
         if(iostat.ne.0) then
               call bug('i','Error writing to logfile')
               call bugno('f',iostat)
         endif
	 write(line,'(1x)')
         call txtwrite(llog,line,len1(line),iostat)

         do i=1,nbin
           write(line,'(x,i4,4(3x,e12.5))') i,(loglist(i,j), j=1,npol)
           call txtwrite(llog,line,len1(line),iostat)
           if(iostat.ne.0) then
               call bug('i','Error writing to logfile')
               call bugno('f',iostat)
           endif
         enddo
         call txtclose(llog)
      endif
c
c
	end
c************************************************************************
	subroutine Label(xtitle,ytitle,pols,npol)
c
	implicit none
	integer npol,pols(npol)
	character xtitle*(*),ytitle*(*)
c------------------------------------------------------------------------
	real vlen,xlen,ylen,xloc
	character ctemp*3
	integer i,l
c
	character polsc2p*2
	integer len1
c
	call pglab(xtitle,ytitle,' ')
c
	vlen = 0
	do i=1,npol
	  ctemp = polsc2p(pols(i))
	  l = len1(ctemp)
	  if(i.ne.npol)then
	    l = l + 1
	    ctemp(l:l) = ','
	  endif
	  call pglen(5,ctemp(1:l),xlen,ylen)
	  vlen = vlen + xlen
	enddo
	xloc = 0.5 - 0.5*vlen
c
	do i=1,npol
	  ctemp = polsc2p(pols(i))
	  l = len1(ctemp)
	  if(i.ne.npol)then
	    l = l + 1
	    ctemp(l:l) = ','
	  endif
	  call pgsci(i)
	  call pgmtxt('T',2.0,xloc,0.,ctemp(1:l))
	  call pglen(5,ctemp(1:l),xlen,ylen)
	  xloc = xloc + xlen
	enddo
	call pgsci(1)
c
	end
c************************************************************************
	subroutine Shiftit(uv,sfreq,data,nchan,shift)
c
	implicit none
	integer nchan
	double precision uv(2),sfreq(nchan),shift(2)
	complex data(nchan)
c
c  Perform a shift on the data.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
	real theta
	complex w
c
c  Perform the shift.
c
	do i=1,nchan
	  theta = -2*pi * (shift(1)*uv(1) + shift(2)*uv(2)) * sfreq(i)
	  w = cmplx(cos(theta),sin(theta))
	  data(i) = w * data(i)
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(docal,dopol,dopass)
c
	implicit none
	logical docal,dopol,dopass
c
c  Outputs:
c    docal	Apply calibration corrections.
c    dopol	Apply polarisation leakage corrections.
c    dopass	Apply bandpass corrections.
c------------------------------------------------------------------------
	integer NOPT
	parameter(NOPT=3)
	character opts(NOPT)*9
	logical present(NOPT)
	data opts/'nocal    ','nopol    ','nopass   '/
c
	call options('options',opts,present,NOPT)
	docal = .not.present(1)
	dopol = .not.present(2)
	dopass= .not.present(3)
c
	end
