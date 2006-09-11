c************************************************************************
	program wbplt
	implicit none
c
c= wbplt - WBPLT is a program used to analyse wbcorr data.
c& lss
c: uv analysis, plotting
c+
c	WBPLT is a program used to analyse wbcorr data.
c@ vis
c	The input visibility datasets. Several datasets can be given.
c@ select
c	Standard visibility selection. See help on "select" for more
c	information.
c@ stokes
c	Normal Stokes/polarization selection. The default is to process all
c	parallel-hand polarisation.
c@ inc
c       Visibility increment (default is 1)
c@ chans
c       A pair of values giving the range of channels to be plotted.
c@ calfile
c       wbcorr lag-based calibration file.
c@ bpfile
c       wbcorr frequency-based calibration file.
c@ calfac
c	Factor to convert flux scale to Jy. Applied in addition to any
c       calibration tables. Default = 1.0
c@ xrange
c       X-axis range for plot. The default is to self-scale. The units
c       are Jy for flux density, hrs for time ,and RA and degrees for DEC
c       and phase.
c@ yrange
c       Y-axis range for plot. The default is to self-scale. The units
c       are Jy for flux density, hrs for time, and RA and degrees for DEC
c       and phase.
c@ axis
c       For plotting. Two values (minimum match active), one for each 
c       of the x and y axes chosen from:
c        time                     [DD HH MM SS.S format]
c        amplitude, real, imag    [natural units; Jy]
c        phase                    [degrees]
c        closure                  [degrees] - needs 3 baselines
c        triple                   [natural units; Jy] - needs 3 baselines
c        ra                       [HH MM SS.S format]
c        dec                      [DD MM SS.S format]
c        lst                      [HH MM SS.S format]
c        hangle                   [HH MM SS.S format]
c        systemp1 (geometric mean systemp - IF1 all antennas) [Jy]
c        systemp2 (geometric mean systemp - IF2 all antennas) [Jy]
c       Defaults are axis=time,amp  (x and y axes).
c@ device
c	Standard PGPLOT device. See the help on "device" for more information.
c@ log
c       Write ascii data to this log file. Format is specified in comment
c       line. Maximum 9 channels. Default is no output file.
c@ lags
c       A pair of values giving the range of lags to be plotted if
c       options=dofft. There are 2n-2 lag channels starting at 0 and
c       ending at 2n-3.
c@ mask
c       Don't plot a value if the amplitude (not necessarly the yaxis) is 
c       within this pair of values.
c@ options
c	nocal     Do not perform gain calibration.
c	nopol     Do not perform polarisation calibration on the data.
c	nopass    Do not perform bandpass calibration on the data.
c	relax     Plots flagged data (only works if data not averaged in
c                 frequency with the line parameter) or data without
c                 valid positions.
c       stats     Calculate some rudimentary statistics on y-values
c       dtrack    Switch on delay tracking for beam centre (i.e. non-
c                 meridian scans). wbcorr only!
c       dofft     Transform frequency channels into lag channels before
c                 plotting. All frequency channels should be read in 
c                 (NB. lag spectrum is a real quantity)
c       lagwt     For options=dofft or if calfile is set, this applies
c                 a triangular weight function to the lag domain
c       flag0     Filter on channel 0 for wbcorr data
c       avall     Average all frequency channels
c--
c  History:
c    lss 14sep02 Original version copied from calred.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer MAXDAT,MAXSRC,MAXPOL,MAXPT
	integer PolMin,PolMax
	character version*(*)
	integer MAXPL
	parameter(version='version 08-oct-02')
	parameter(MAXDAT=15,MAXPOL=2,MAXSRC=1024,MAXPT=5000000)
	parameter(MAXPL=32)
	parameter(PolMin=-8,PolMax=4)
c
	character uvflags*16
	character*64 device, line*200
	character*132 logf, calfile, bpfile
	character*10 xaxis, yaxis, zaxis, xform, yform, axis(2)
	character type*1
	real xrange(2), yrange(2), mask(2), zmult
	integer plags(2),chans(2)
	integer ipen, nfiles, ncode(MAXSRC), inpt, iref
	integer iostat, lOut, lCal, lBP
	integer tno,vsource,nsyst,nsyst2

	integer nread,nout,jref
	integer npol, nvis, ivis, inc, nflag, nflag0, nflagpos
	integer i1, i2, j1, j2, j3, kmax, ki1, ki2, k, kk
	double precision preamble(4),lst
	complex data(MAXCHAN), cdata(MAXCHAN), avdata
	real lag(2*MAXCHAN-2), xph(2*MAXCHAN-2)
	double precision freq(MAXCHAN)
	logical flag(MAXCHAN),relax,stats,dtrack,dofft,first,lagwt
	logical flag0,flagzero,upd,avall
	real pi, calfac, base, dph, x1, x2
	real qmask
	real kamp(MAXCHAN), kph(MAXCHAN), zpt, zpt1, zpt2,zf
	real bamp(MAXANT,MAXANT,MAXPL),bph(MAXANT,MAXANT,MAXPL)
	real kclose(MAXANT,MAXANT,MAXPL)
	real ktrip (MAXANT,MAXANT,MAXPL)
	real tsys(MAXANT),tsys2(MAXANT)
	integer iclose(MAXPL)
        parameter (pi = 3.141592653589793, qmask=-999.9)

c  Cal array

	real kcal(MAXPL,2,MAXANT,MAXANT), ftreal,ftimag

c  Plotting arrays

	real xplot(MAXPT), yplot(MAXPT)
	integer npt, i, j
	real xdmin,xdmax,ydmin,ydmax, xdtmp

c  Stats

	double precision smean, svar

c  mjd parameters

	double precision mjd, fd0, ra, dec, mjdlast, mjd0
	double precision ra2, dec2, ra3, dec3, ra4, dec4

c
c  Externals.
c
	integer pgbeg, len1

c
c  Externals.
c
	logical uvDatOpn
	integer nextpow2
	double precision epo2jul
c
c Types of axes allowed
c
        integer naxmax, nax
        parameter (naxmax = 13)
        character axtyp(naxmax)*10
        data axtyp /  'time      ','amplitude ','phase     ',
     1                'real      ','imag      ','ra        ',
     2                'dec       ','lst       ','hangle    ',
     3                'closure   ','systemp1  ','systemp2  ',
     4                'triple    '/

c-----------------------------------------------------------------------

c  Initialising

	do i=1,MAXPL
	   iclose(i)=0
	   do j=1,2
	      do k=1,MAXANT
		 do kk=1,MAXANT
		    kcal(i,j,k,kk)=qmask
		 end do
	      end do
	   end do
	end do
	do i=1,MAXANT
	   do j=1,MAXANT
	      do k=1,MAXPL
		 bamp(i,j,k)=qmask
		 bph(i,j,k)=qmask
		 kclose(i,j,k)=0.0
		 ktrip(i,j,k)=0.0
	      end do
	   end do
	end do
	
c
c Lets go! Get user inputs.
c
	call output('wbplt: '//version)
	call keyini
	call GetOpt(uvflags,relax,stats,dtrack,dofft,lagwt,flag0,avall)
	call uvDatInp('vis',uvflags)
	call keyr('calfac',calfac,1.0)
        call keymatch ('axis', naxmax, axtyp, 2, axis, nax)
	call keya('log',logf,' ')
	call keya('calfile',calfile,' ')
	call keya('bpfile',bpfile,' ')
        call keya('device',device,' ')
        call keyr('xrange',xrange(1),0.0)
        call keyr('xrange',xrange(2),xrange(1))
        call keyr('yrange',yrange(1),0.0)
        call keyr('yrange',yrange(2),yrange(1))
        call keyi('lags',plags(1),0)
        call keyi('lags',plags(2),plags(1))
        call keyi('inc',inc,1)
        call keyi('chans',chans(1),0)
        call keyi('chans',chans(2),chans(1))
        call keyr('mask',mask(1),qmask)
        call keyr('mask',mask(2),mask(1))
	call keyfin
c
c  Check user inputs
c
        xaxis = axis(1)
        yaxis = axis(2)
        if (xaxis.eq.' ') xaxis = 'time'
        if (yaxis.eq.' ') yaxis = 'amplitude'
        if (xaxis.eq.yaxis) call bug ('f', 'x and y axes identical')
        if (xrange(1).eq.xrange(2).and.xrange(1).ne.0.0)
     *    call bug ('f', 'Invalid x-axis plot range')
        if (yrange(1).eq.yrange(2).and.yrange(1).ne.0.0)
     *    call bug ('f', 'Invalid y-axis plot range')
	if(inc.lt.1) inc=1
c
c  Open log file if desired.
c
	if(logf.ne.' ') then
	  call txtopen(lOut, logf, 'new', iostat)
	  if(iostat.ne.0) call bug ('f', 'Error opening output file')
	  line='# Output from wbplt'
	  call txtwrite(lOut,line,len1(line),iostat)
	  if(iostat.ne.0) call bug ('f', 'Error writing output file')
	  line='# MJD, RA, DEC (J2000,rad), BASELINE, AMP, PHASE'//
     1        ' (deg) for each channel'
	  call txtwrite(lOut,line,len1(line),iostat)
	  if(iostat.ne.0) call bug ('f', 'Error writing output file')
	endif

c
c  Open bandpass file if desired.
c
	if(bpfile.ne.' ') then
	     kmax=0
	     call txtopen(lBP, bpfile, 'old', iostat)
	     if(iostat.ne.0) call bug ('f', 'Error opening bpfile')
	     call output('Reading bpass file...')
	     do while(iostat.eq.0)
	       call txtread(lBP,line,j,iostat)
	       call output(line)
	       if(line(1:1).ne.'#'.and.line.ne.' ') then
	         read(line(1:1),*) ki1
		 read(line(3:3),*) ki2
		 if(ki1.lt.2.or.ki1.gt.4.or.ki2.lt.2.or.ki2.gt.4) then
		    call bug('f','Invalid baseline in  bpfile')
		 end if
	         read(line(4:200),*) (bamp(ki1,ki2,i),bph(ki1,ki2,i),
     1            i=1,9)
	       end if
	     end do
	     call txtclose(lBP)
	endif

c
c  Open cal file if desired.
c
	if(calfile.ne.' ') then
	     kmax=0
	     call txtopen(lCal, calfile, 'old', iostat)
	     if(iostat.ne.0) call bug ('f', 'Error opening calfile')
	     call output('Reading cal lag file...')
	     do while(iostat.eq.0)
	       call txtread(lCal,line,j,iostat)
	       call output(line)
	       if(line(1:1).ne.'#'.and.line.ne.' ') then
	         read(line(1:1),*) ki1
		 read(line(3:3),*) ki2
		 if(ki1.lt.2.or.ki1.gt.4.or.ki2.lt.2.or.ki2.gt.4) then
		    call bug('f','Invalid baseline in cal file')
		 end if
	         read(line(9:10),*) k
	         read(line(52:58),*) kcal(k+1,2,ki1,ki2)
	         read(line(69:75),*) kcal(k+1,1,ki1,ki2)
		 if(k.gt.kmax) kmax=k
	       end if
	     end do
	     call txtclose(lCal)
	endif

	call output('Processing uv data...')
c
 	call uvDatGti('npol',npol)

c
c Miscellaneous initialisation.
c
	npol = 0
	npt=0
	nvis=0
	ivis=0
	nflag=0
	nflag0=0
	nflagpos=0
	first=.true.
	mjdlast=0.0
	nfiles=0

c

	dowhile(uvDatOpn(tno))
	  call uvVarIni(tno,vsource)
	  call uvVarSet(vsource,'source')
	  nread=1
	  nfiles=nfiles+1
	  ncode(nfiles)=0
	  dowhile(nread.gt.0)
	    call uvDatRd(preamble,data,flag,MAXCHAN,nread)

c  Unfortunately, we can't let the last non-data point into the fft engine

	    if(nread.eq.0) goto 80

c  If this is the first visibility, extract the frequency and 
c  write the log file header

	    if(first) then
	       call uvinfo(tno, 'frequency', freq)

c Is channel range valid?

	       if(chans(1).eq.0) chans(1)=1
	       if(chans(2).eq.0) chans(2)=nread
	       if(chans(1).lt.1.or.chans(2).gt.nread) then
		   call bug('f','Invalid chans parameter')
	       end if

c  Initial mjd

	       mjd0=preamble(3)-2400000.5d0
	       fd0=mjd0-int(mjd0)

	       if(logf.ne.' ') then
	          line=' '
	          nout=chans(2)
	          if(nout.gt.9) nout=9
	          write(line,30) (freq(j),j=chans(1),nout)
 30	          format('# Channel frequencies (GHz):',9(1x,f8.4))
	          call txtwrite(lOut,line,len1(line),iostat)
	          if(iostat.ne.0) then
                     call bug ('f', 'Error writing output file')
                  end if
	       end if
	    end if
	    first=.false.

c  Skip visibilities if requested

	    ivis=ivis+1
	    if(mod(ivis,inc).ne.0) goto 80

c  Flag according to zero-frequency channel (if line parameter is not present,
c  this is the first channel)

	    call amphase(data(1),kamp(1),kph(1))
	    flagzero=.false.
	    if(flag0.and.(kamp(1).gt.0.02)) flagzero=.true.

c  Scale data

	    do i=1,nread
	       data(i)=calfac*data(i)
	    end do

c  Check enough channels have been read in to do lag calibration

	    if(calfile.ne.' ') then
	       if(nread.lt.(kmax+3)/2) then
		  call bug('f','Need more frequency channels to apply'//
	1	       ' lag calibration')
	       end if
	    end if

c  Positions 

	    call uvgetvrd(tno,'ra',ra,1)
	    call uvgetvrd(tno,'dec',dec,1)

c  LST (may be a few sec wrong for station W104 - need to alter observatory
c       longitude in wblod)

            call getlst(tno, lst)

c  Tsys

            call uvprobvr(tno,'systemp',type,nsyst,upd)
	    if(upd) call uvgetvrr(tno,'systemp',tsys,nsyst)
            call uvprobvr(tno,'systemp2',type,nsyst2,upd)
	    if(upd) call uvgetvrr(tno,'systemp2',tsys2,nsyst2)


c  Flags (assume whole spectrum is flagged if ch.1 is flagged)

	    if((flag(1).and.ra.ne.0.0.and.dec.ne.0.0.and..not.flagzero).
     *           or.relax) then
	      nvis=nvis+1
c	      ncode(nfiles)=nvis
	    else
	      if(flagzero) nflag0=nflag0+1
	      if(.not.flag(1)) nflag=nflag+1
	      if(ra.eq.0.0.or.dec.eq.0.0) nflagpos=nflagpos+1
	      goto 80
	    end if

c  Time
	    mjd=preamble(3)-2400000.5d0

c
c  Precess, nutate and aberrate.
c
	    ra4=0.0
	    dec4=0.0
	    if(yaxis.eq.'hangle'.or.xaxis.eq.'hangle'.or.dtrack) then
              call precess(epo2jul(2000.0d0,'J'),ra,dec,preamble(3),
     1                   ra2,dec2)
              call nutate(preamble(3),ra2,dec2,ra3,dec3)
              call aberrate(preamble(3),ra3,dec3,ra4,dec4)
	    end if

c  Baseline

	    i1=int(preamble(4)/256.0)
	    i2=int(preamble(4))-256*i1


c  compute lag spectrum if lag calibration is required, or if a lag plot
c  is desired

	    j1=1
	    j2=nread
	    if(dofft.or.calfile.ne.' ') then
		 nread = 2*nread-2
                 if(nextpow2(nread).ne.nread)then
                    call bug('f',
     *              'Number of correlator channels must be 2**n+1')
		 end if
		 call fftcr(data,lag,-1,nread)
                 call shifty(lag,nread)
		 j1=plags(1)+1
		 j2=plags(2)+1
		 if(j1.lt.1.or.j2.gt.nread) call bug('f',
     *             'Specified lag channel not present')

c  lag weighting

		 if(lagwt) then
		    do k=j1,j2
		      if(k.lt.8) then
			 lag(k)=2.0*lag(k)*real(k)/8.0
		      else
		         lag(k)=2.0*lag(k)*real(16-k)/8.0
		      end if
		   end do
		 end if


c  apply calibration

		 if(calfile.ne.' ') then
		    if(.not.dofft) then
		       if(j1.ne.1.or.j2.ne.nread) then
                          call bug('f',
     *                    'All lag channels need to be present')
		       end if
		    end if
		    do k=j1,j2
		       if(kcal(k,1,i1,i2).eq.qmask.or.
	1	          kcal(k,2,i1,i2).eq.qmask) then
		           call bug('f','Required cal info missing')
		       end if
		       lag(k)=lag(k)/kcal(k,1,i1,i2)
		       xph(k)=-pi*kcal(k,2,i1,i2)/180.0
		    end do
		 
c  transform back

		    if(.not.dofft) then
c  FFT for testing
c                       call shifty(lag,nread)
c                       call fftrc(lag,data,1,nread)
c  Complex transform - if used, this needs shifting in lag space (by jref?)
c		       do i=1,nread
c			  cdata(i)=cmplx(lag(i),0.0)
c		       end do
c		       call fftcc(cdata,data,1,nread)

c  Discrete transform - is the sign of xph correct?

		       jref=(nread+2)/2
		       do i=1,jref
			  ftreal=0.0
			  ftimag=0.0
			  do j=1,nread
			    ftreal=ftreal+lag(j)*cos(2.*pi*
     1                       real(i-1)*real(j-jref)/real(nread)-xph(j))     
			    ftimag=ftimag+lag(j)*sin(2.*pi*
     1                       real(i-1)*real(j-jref)/real(nread)-xph(j))
			  end do
			  data(i)=cmplx(ftreal,ftimag)
		       end do
		       
                       do i=1,jref
			  call amphase(data(i),kamp(i),kph(i))
			  kamp(i)=kamp(i)/real(nread)
                          data(i)=cmplx(kamp(i)*cos(pi*kph(i)/180.0),
	1		       kamp(i)*sin(pi*kph(i)/180.0))
	               end do
		       nread=jref
		    end if
		 end if
	    end if

c  bandpass cal

	    if(bpfile.ne.' ') then
	       if(nread.ne.9) then
		  call bug('f','Wrong number of frequency channels')
	       end if
	       do i=1,nread
		  call amphase(data(i),kamp(i),kph(i))
		  if(bamp(i1,i2,i).eq.qmask) then
		     call bug('f','No bandpass calibration for this'//
     1                  ' baseline')
		  end if
		  if(bamp(i1,i2,i).ne.0.0) then
	             kamp(i)=kamp(i)/bamp(i1,i2,i)
		     kph(i)=kph(i)-bph(i1,i2,i)
		  end if
                  data(i)=cmplx(kamp(i)*cos(pi*kph(i)/180.0),
	1		       kamp(i)*sin(pi*kph(i)/180.0))
	       end do
	    end if

c  delay tracking

	      if(dtrack) then
	       if(i1.lt.2.or.i1.gt.4.or.i2.lt.1.or.i2.gt.4) then
		  call bug('f',
     *               'Unrecognised baseline for phase tracking')
	       end if
	       if(i1.eq.3.or.i2.eq.3) then
		  base=30.6
	       else
		  base=61.2
	       end if
	       do i=1,nread
                 call amphase (data(i), kamp(i), kph(i))
	         dph=360.0*base*freq(i)*1.0e9*cos(dec)*
     *             sin(real(lst)-ra4)/2.9979246e8
	         kph(i)=kph(i)+dph
	         kph(i)=kph(i)-360.0*int(kph(i)/360.0)
	         if(kph(i).gt. 180.0) kph(i)=kph(i)-360.0
	         if(kph(i).lt.-180.0) kph(i)=kph(i)+360.0
                 x1=kamp(i)*cos(pi*kph(i)/180.0)
                 x2=kamp(i)*sin(pi*kph(i)/180.0)
		 data(i)=cmplx(x1,x2)
	        end do
	      end if

c 2004 change

	      if(calfile.ne.' ') then
	         j1=chans(1)
	         j2=chans(2)
	      end if
	      if(.not.dofft) then
	        j1=chans(1)
	        j2=chans(2)
              end if

c Average frequency channels

	    if(avall) then
	      avdata=(0.0,0.0)
	      do i=j1,j2
	         avdata=avdata+data(i)
	      end do
	      data(j1)=avdata/real(j2-j1+1)
	      j3=j1
	    else
	      j3=j2
	    end if

	    do i=j1,j3
	      npt=npt+1
	      if(npt.eq.(MAXPT+1).and.device.ne.' ') then
	        call bug('w','Maximum number of points exceeded'
     1           //' in plot')
	      end if

c  amplitude and phase

	      if(dofft) then
		 kamp(i)=lag(i)
		 kph(i)=0.0
	      else
                 call amphase (data(i), kamp(i), kph(i))
	      end if

c  store required axes

	      if(device.ne.' '.and.npt.le.MAXPT) then
	      do j=1,2
		 if(j.eq.1) then
		    zaxis=xaxis
		 else
		    zaxis=yaxis
		 end if
	         if(zaxis.eq.'real') then
	           zpt=kamp(i)*cos(pi*kph(i)/180.0)
	         else if(zaxis.eq.'imag') then
	           zpt=kamp(i)*sin(pi*kph(i)/180.0)
	         else if(zaxis.eq.'amplitude') then
		   zpt=kamp(i)
	         else if(zaxis.eq.'phase') then
		   zpt=kph(i)
	         else if(zaxis.eq.'closure'.or.zaxis.eq.'triple') then
		   if(mjd.eq.mjdlast) then
		      iclose(i)=iclose(i)+1
		   else
		      iclose(i)=1
		   end if
		   kclose(i1,i2,i)=kph(i)
		   ktrip(i1,i2,i)=kamp(i)
		   if(iclose(i).eq.3) then
		      if(kclose(2,3,i).ne.0.0.and.kclose(3,4,i).
     1                   ne.0.0.and.kclose(2,4,i).ne.0.0) then
		         zpt1=kclose(2,3,i)+kclose(3,4,i)-kclose(2,4,i)
			 if(zpt1.gt.180.0) zpt1=zpt1-360.0
			 if(zpt1.lt.-180.0) zpt1=zpt1+360.0
			 zpt2=(ktrip(2,3,i)*ktrip(3,4,i)*
     1                        ktrip(2,4,i))**(1.0/3.0)
		      else
			 call bug('f','Expected baseline data missing')
		      end if
		   else
		      npt=npt-1
		      goto 50
		   end if
		   iclose(i)=0
		 else if(zaxis.eq.'systemp1') then
		    zpt=(tsys(2)*tsys(3)*tsys(4))**(1.0/3.0)
		 else if(zaxis.eq.'systemp2') then
		    zpt=(tsys2(2)*tsys2(3)*tsys2(4))**(1.0/3.0)
	         else if(zaxis.eq.'time') then
 	           zpt=real(mjd-mjd0+fd0)*86400.0
	         else if(zaxis.eq.'lst') then
 	           zpt=real(lst)*86400.0/(2.0*pi)
	         else if(zaxis.eq.'hangle') then
 	           zpt=real(lst-ra4)*86400.0/(2.0*pi)
	         else if(zaxis.eq.'ra') then
 	           zpt=real(ra)*86400.0/(2.0*pi)
	         else if(zaxis.eq.'dec') then
	           zpt=real(dec)*15.0*86400.0/(2.0*pi)
		 end if

		 if(j.eq.1) then
		    if(xaxis.eq.'closure') zpt=zpt1
		    if(xaxis.eq.'triple') zpt=zpt2
		    xplot(npt)=zpt
		 else

c  Mask on amplitude

		    if(kamp(i).gt.mask(1).and.kamp(i).lt.mask(2)) then
		       npt=npt-1
		       goto 50
		    else
		       if(yaxis.eq.'closure') zpt=zpt1
		       if(yaxis.eq.'triple') zpt=zpt2
		       yplot(npt)=zpt
		    end if
		 end if
 50		 continue
	      end do
	      end if
	    end do

c  keep log file if desired

 	    if(logf.ne.' ') then
	       line=' '
	       if(nout.gt.9) nout=9
	       write(line,60) mjd, lst, ra, dec, i1, i2, 
     *                        (kamp(j), kph(j), j=chans(1),nout)
 60	       format(f13.7,1x,f9.7,1x,f9.6,1x,f9.6,1x,i1,'-',i1,
     *                9(1x,e10.3,1x,f6.1))
	       call txtwrite(lOut,line,len1(line),iostat)
	       if(iostat.ne.0) then
                 call bug ('f', 'Error writing output file')
               end if
 	    endif
 80	    continue
	    mjdlast=mjd
	  enddo
 	  call uvDatCls
	  ncode(nfiles)=npt
	enddo

	write(line,'(i8,1x,a)') nvis, 'visibilities accepted'
	call output(line)
	write(line,'(i8,1x,a)') nflag, 
     1         'visibilities rejected (visibility flag)'
	call output(line)
	write(line,'(i8,1x,a)') nflagpos, 
     1         'visibilities rejected (no positions)'
	call output(line)
	write(line,'(i8,1x,a)') nflag0, 
     1         'visibilities rejected (wbcorr channel 0 filter)'
	call output(line)
	write(line,'(i8,1x,a)') nfiles, 
     1         'file(s) read'
	call output(line)

c
c  Close log file
c
	if(logf.ne.' ') then
	  call txtclose(lOut)
	endif
c
c  Open plot device if requested.
c
	if(device.ne.' ') then
	  if(npt.gt.MAXPT) npt=MAXPT
	  iostat = pgbeg(0,device,1,1)
	  if(iostat.eq.0) call bug ('f', 'Error opening plot device') 

c
c  axis scale
c
	  xdmin=xplot(1)
	  xdmax=xdmin
	  ydmax=yplot(1)
	  ydmin=ydmax

	  do i=1,npt
	     if(xplot(i).gt.xdmax) xdmax=xplot(i)
	     if(xplot(i).lt.xdmin) xdmin=xplot(i)
	     if(yplot(i).gt.ydmax) ydmax=yplot(i)
	     if(yplot(i).lt.ydmin) ydmin=yplot(i)
	  end do

	  if(xaxis.eq.'ra') then
	     xdtmp=xdmax
	     xdmax=xdmin
	     xdmin=xdtmp
	  end if

c  expand axis slightly beyond extrema

	  zf=0.05
	  xdmax=xdmax+zf*(xdmax-xdmin)
	  xdmin=xdmin-zf*(xdmax-xdmin)
	  ydmax=ydmax+zf*(ydmax-ydmin)
	  ydmin=ydmin-zf*(ydmax-ydmin)

c  manual axis boundaries

	  if(xrange(1).ne.0.0.or.xrange(2).ne.0.0) then
	     zmult=1.0
	     if(xaxis.eq.'time') zmult=3600.0
	     if(xaxis.eq.'ra')   zmult=3600.0
	     if(xaxis.eq.'dec')  zmult=3600.0
	     xdmin=xrange(1)*zmult
	     xdmax=xrange(2)*zmult
	  end if

	  if(yrange(1).ne.0.0.or.yrange(2).ne.0.0) then
	     zmult=1.0
	     if(yaxis.eq.'time') zmult=3600.0
	     if(yaxis.eq.'lst')  zmult=3600.0
	     if(yaxis.eq.'hangle')zmult=3600.0
	     if(yaxis.eq.'ra')   zmult=3600.0
	     if(yaxis.eq.'dec')  zmult=3600.0
	     ydmin=yrange(1)*zmult
	     ydmax=yrange(2)*zmult
	  end if

c  Deal with the RA 0/24 hr boundary if xrange selected manually
c  Don't attempt to do this otherwise

	  if(xaxis.eq.'ra') then
	     if(xdmax.lt.0.0) then
		do i=1,npt
		   if(xplot(i).gt.xdmax+86400.0) then
			xplot(i)=xplot(i)-86400.0
		   end if
		end do
	     end if
	  end if

	  call pgswin(xdmin, xdmax, ydmin, ydmax)

	  if(xaxis.eq.'time'.or.xaxis.eq.'lst'.or.
     *       xaxis.eq.'hangle') then
	     xform='ZHBCNTS'
	  else if(xaxis.eq.'ra') then
	     xform='ZYHBCNTS'
	  else if(xaxis.eq.'dec') then
	     xform='ZDBCNTS'
	  else
	     xform='BCNTS'
	  end if

	  if(yaxis.eq.'time'.or.yaxis.eq.'lst'.or.
     *       yaxis.eq.'hangle') then
	     yform='ZHBCNTS'
	  else if(yaxis.eq.'ra') then
	     yform='ZYHBCNTS'
	  else if(yaxis.eq.'dec') then
	     yform='ZDBCNTS'
	  else
	     yform='BCNTS'
	  end if

	  call pgtbox(xform,0.0,0.0,yform,0.0,0.0)
	  call pglabel(xaxis,yaxis,' ')

c  Plot points from different files in different colours

	  do i=1,nfiles
	    if(i.eq.1) then
	       iref=1
	       inpt=ncode(i)
	    else
	       iref=1+ncode(i-1)
	       inpt=1+ncode(i)-ncode(i-1)
	    end if
	    if(nfiles.eq.1) then
	       ipen=1
	    else
	       ipen=2+mod(i-1,13)
	    end if
            call pgsci(ipen)
	    call pgpt(inpt,xplot(iref),yplot(iref),-1)
	    call pgsci(1)
	  end do
c
c  Close graphics
c
	  call pgend
	end if

c  Stats

	if(stats) then
	   smean=0.0
	   svar=0.0
	   do i=1,npt
	      smean=smean+dble(yplot(i))/dble(npt)
	   end do
	   do i=1,npt
	      svar=svar+(dble(yplot(i)-smean))**2
	   end do
	   svar=sqrt(svar/dble(npt))
	   write(line,'(a,e11.3)') ' Mean ( '//zaxis(1:5)//') = ', smean
	   call output(line)
	   write(line,'(a,e11.3)') '  rms ( '//zaxis(1:5)//') = ', svar
	   call output(line)
	end if

	end
c************************************************************************
	subroutine GetOpt(uvflags,relax,stats,dtrack,dofft,lagwt,
     1                    flag0,avall)
c
	implicit none
	character uvflags*(*)
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=10)
	character opts(NOPTS)*9
	logical present(NOPTS),relax,stats,dtrack,dofft,lagwt,flag0,
     1          avall
	data opts/'nocal    ','nopol    ','nopass   ','relax',
     *            'stats','dtrack','dofft','lagwt','flag0','avall'/
c
	call options('options',opts,present,NOPTS)
c
c c -- docal
c f -- dopass
c e -- dopol
c
	uvflags = 'dslx'
	if(.not.present(1))uvflags(5:5) = 'c'
	if(.not.present(2))uvflags(6:6) = 'e'
	if(.not.present(3))uvflags(7:7) = 'f'

	relax=.true.
	stats=.true.
	dtrack=.true.
	dofft=.true.
	lagwt=.true.
	flag0=.true.
	avall=.true.
	if(.not.present(4))relax=.false. 
	if(.not.present(5))stats=.false. 
	if(.not.present(6))dtrack=.false. 
	if(.not.present(7))dofft=.false. 
	if(.not.present(8))lagwt=.false. 
	if(.not.present(9))flag0=.false. 
	if(.not.present(10))avall=.false. 
	end



c************************************************************************
      subroutine getlst (lin, lst)
c
      implicit none
      integer lin
      double precision lst
c
c  Get lst of the current data point.
c
c  Input:
c    lin         Handle of file
c  Output:
c    lst         LAST in radians
c-----------------------------------------------------------------------
      double precision time,ra,long,dtemp
      character type*1
      integer length
      logical ok
c
c  Externals.
c
      double precision eqeq
c
      lst = 0.0d0
      call uvprobvr (lin, 'lst', type, length, ok)
      if (type(1:1).eq.' ') then
	call uvrdvrd (lin, 'ra', dtemp, 0.d0)
	call uvrdvrd (lin, 'obsra', ra, dtemp)
	call getlong(lin,long)
	call uvrdvrd(lin,'time',time,0.d0)
        call jullst (time, long, lst)
	lst = lst + eqeq(time)
      else
         call uvrdvrd (lin, 'lst', lst, 0.0d0)
      end if
c
      end
c
c
c
c
      subroutine getlong (lin, long)
c-----------------------------------------------------------------------
c     Get longitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    longitude   Longitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision long
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      long = 0.0d0
      call uvprobvr (lin, 'longitu', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No longitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out longitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'longitude', long, ok)
            if (.not.ok) call bug('f', 
     +          'No valid longitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'longitu', long, 0.0d0)
      end if
c
      end
c
c

c************************************************************************
        subroutine shifty(rdata,n)
c
        implicit none
        integer n
        real rdata(n)
c------------------------------------------------------------------------
        integer i
        real t
c
        do i=1,n/2
          t = rdata(i)
          rdata(i) = rdata(i+n/2)
          rdata(i+n/2) = t
        enddo
        end
