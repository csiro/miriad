c************************************************************************
      program prthd
      implicit none
c
c= PRTHD -- Print a summary about the contents of a data-set.
c& rjs
c: miscellaneous
c	PRTHD is a Miriad task which gives a summary about a Miriad data-set.
c@ in
c	Name of the input data-set. No default. This may be either a uv
c	or image data-set. Several names can be given. Names can include
c	wildcards.
c@ log
c	Output log file. Default is the terminal.
c@ options
c	Extra processing options. Possible values are:
c	  brief   Give one line description of each file.
c	  full    Several line description of each file (default).
c--
c  History:
c    rjs   9sep91 Adapted from the interior Werong version, developed by nebk.
c    nebk 12sep91 Fix formatting bug with image min/max/units
c    rjs  17sep91 Some extra labeling.
c    rjs   1oct91 Fixed reporting of wide channels in uv files.
c    rjs  13dec91 Give total number of correlations messages.
c    rjs  26may92 Print out btype,pbfwhm and rms of an image.
c    nebk 23nov92 Data is -> Data are
c    rjs  11dec92 Add brief option and log file.
c    rjs  15feb93 Items crval and cdelt, and variables ra,dec now double.
c    rjs   8mar93 Tell whether bandpass table is present.
c    rjs  19aug93 Handle galactic and ecliptic coordinates.
c    rjs  24nov93 Correct units of xshift and yshift.
c    rjs  24jul94 Message about aipsfg tables.
c    rjs  15aug94 More decimal places for restfreq.
c    rjs  24oct94 More information for images.
c  Bugs and Shortcomings:
c    * Descriptions in brief mode could be a bit more verbose!
c------------------------------------------------------------------------
	character version*(*)
	integer MAXIN
	parameter(version='Prthd: version 24-Oct-94')
	parameter(MAXIN=256)
	integer tno,i,iostat,nin
	character in(MAXIN)*64,logf*64,line*80
	logical full,more
c
c  Externals.
c
	logical hdprsnt
c
c  Get the input parameter.
c
	call output(version)
	call keyini
	call mkeyf('in',in,MAXIN,nin)
	call keya('log',logf,' ')
	call GetOpt(full)
	call keyfin
	if(nin.eq.0)call bug('f','No inputs given')
c
c  Open the output log file.
c
	call logopen(logf,' ')
c
c  Loop around, determining the sort of file, and giving a summary
c  about it.
c
c  Full listing.
c
	if(full)then
	  do i=1,nin
	    call logwrite('********************************'//
     *		      '********************************',more)
	    call hopen(tno,in(i),'old',iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error opening '//in(i))
	      call bugno('w',iostat)
	    else if(hdprsnt(tno,'image'))then
	      call ImHead(tno,in(i))
	      call hclose(tno)
	    else if(hdprsnt(tno,'visdata'))then
	      call hclose(tno)
	      call VisHead(in(i))
	    else if(hdprsnt(tno,'rdata'))then
	      call CalHead(tno,in(i))
	      call hclose(tno)
	    else
	      call bug('w','Unrecognised format for '//in(i))
	      call bug('w','Use ITEMIZE to inspect this data-set')
	      call hclose(tno)
	    endif
	  enddo
c
c  Brief listing.
c
	else
	  do i=1,nin
	    line(1:32) = in(i)
	    call hopen(tno,in(i),'old',iostat)
	    if(iostat.ne.0)then
	      line(33:) = ': Regular file or directory'
	    else if(hdprsnt(tno,'image'))then
	      line(33:) = ': Image data-set'
	    else if(hdprsnt(tno,'visdata'))then
	      line(33:) = ': Visibility data-set'
	    else if(hdprsnt(tno,'rdata'))then
	      line(33:) = ': Calibration data-set'
	    else
	      line(33:) = ': Miriad data-set in unknown format'
	    endif
	    call logwrite(line,more)
	    if(iostat.eq.0)call hclose(tno)
	  enddo
	endif
c
	call logclose
	end
c************************************************************************
	subroutine GetOpt(full)
c
	implicit none
	logical full
c
c  Get extra processing options.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=2)
	logical present(nopts),brief
	character opts(nopts)*8
	data opts/'brief   ','full    '/
c
	call options('options',opts,present,nopts)
	brief = present(1)
	full  = present(2).or..not.present(1)
	if(brief.and.full)call bug('f',
     *	  'Cannot mix options brief and full')
	end
c************************************************************************
	subroutine ImHead(tno,in)
c
	implicit none
	integer tno
	character in*(*)
c
c  Give a summary about an image.
c------------------------------------------------------------------------
      include 'mirconst.h'
      real rval1,rval2,rval3
      double precision dval
      integer ival,il1,il2
      logical more
      character line*80,aval1*72,aval2*72
c
c  Externals.
c
      integer len1
      character itoaf*12
      logical hdprsnt
c
      line = 'Filename: '//in
      call logwrite(line,more)
c
c  Telescope/observer/object parameters.
c
      call rdhda (tno, 'instrume', aval1, ' ')
      call rdhda (tno, 'telescop', aval2, ' ')
      il1 = len1(aval1)
      il2 = len1(aval2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Instrument: '//aval1(1:15)
     *               //' Telescope: '//aval2(1:il2),more)
      else if(il1.gt.0)then
        call logwrite('Instrument: '//aval1(1:il1),more)
      else if(il2.gt.0)then
        call logwrite('Telescope: '//aval2(1:il2),more)
      end if
c
      call rdhda (tno, 'object', aval1, ' ')
      call rdhda (tno, 'observer', aval2, ' ')
      il1 = len1(aval1)
      il2 = len1(aval2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Object: '//aval1(1:19)
     *               //' Observer: '//aval2(1:il2),more)
      else if(il1.gt.0)then
	call logwrite('Object: '//aval1(1:il1),more)
      else if(il2.gt.0)then
        call logwrite('Observer: '//aval2(1:il2),more)
      endif
c
c  Image type.
c
      call rdhda(tno,'btype',aval1,' ')
      il1 = len1(aval1)
      if(il1.gt.0)call logwrite('Image type: '//aval1(1:il1),more)
c
c  Min, max values and units.
c
      call rdhdr (tno,'datamax',rval1,0.0)
      call rdhdr (tno,'datamin',rval2,rval1+1)
      call rdhda (tno,'bunit',aval1, ' ')
      if(rval1.ge.rval2)then
        write (line, 40) rval1, rval2, aval1(1:len1(aval1))
40      format ('Maximum: ', 1pe15.8, 4x, 'Minimum: ', 1pe15.8, 2x, a)
        call logwrite(line,more)
      else
	call logwrite('Map flux units: '//aval1,more)
      endif
c
c  Synthesised beam parameters.
c
      call rdhdr(tno,'bmaj',rval1,0.)
      call rdhdr(tno,'bmin',rval2,0.)
      call rdhdr(tno,'bpa',rval3,0.)
      if(rval1.gt.0.and.rval2.gt.0)then
	write(line,'(a,f6.2,a,f6.2,a)')'Beam Size:',3600*180/pi*rval1,
     *				       ' by',	    3600*180/pi*rval2,
     *				       ' arcsec.'
	call logwrite(line,more)
	write(line,'(a,f7.1,a)')'Position angle:',rval3,' degrees.'
	call logwrite(line,more)
      endif
c
c  Dimension info.
c
      call rdhdi (tno, 'naxis', ival, 0)
      write (line,'(a,i2,a)')'This image has',ival,' axes.'
      call logwrite(line,more)
      call doaxes (tno,ival)
c
c  Parameters related to coordinates.
c
      call rdhdd (tno, 'obstime', dval, 0.d0)
      if(dval.gt.0)then
	call julday(dval,'H',aval1)
	line =
     *	  'Average Time of observation: '//aval1
	call logwrite(line,more)
      endif
      call rdhdr (tno, 'epoch', rval1, 0.0)
      if(rval1.gt.0)then
	if(rval1.lt.1984)then
	  aval1 = 'B'
	else
	  aval1 = 'J'
	endif
	il1 = len1(aval1)
	write (line, '(a,a,f7.2,a)')
     *	  'Equinox:                     ',aval1(1:1),rval1
	call logwrite(line,more)
      endif
      call rdhdd(tno,'restfreq',dval,0.d0)
      if(dval.gt.0)then
	write(line,'(a,f13.6,a)')
     *	  'Rest frequency:         ',dval,' GHz'
	call logwrite(line,more)
      endif
      if(hdprsnt(tno,'vobs'))then
	call rdhdr(tno,'vobs',rval1,0.0)
	write(line,'(a,f8.2,a)')
     *	  'Observatory radial velocity:',rval1,' km/s'
	call logwrite(line,more)
      endif
      
c
c  Rms noise.
c
      call rdhdr (tno, 'rms', rval1, -1.0)
      if(rval1.gt.0)then
        write (line,'(a,1pe10.3)')
     *	  'Nominal Theoretical Rms:    ',rval1
        call logwrite(line,more)
      endif
c
c  Primary beam parameters.
c
      call rdhdr (tno, 'pbfwhm', rval1, -1.0)
      if(rval1.gt.0)then
        write (line,'(a,1pg10.3)')
     *	  'Primary beam size (arcsec): ',rval1
        call logwrite(line,more)
      endif
c
c  Number of clean components.
c
      call rdhdi(tno,'niters',ival,0)
      if(ival.gt.0)call logwrite(
     *	  'Number of iterations:       '//itoaf(ival),more)
c
c  Check for extra tables, etc.
c
      if(hdprsnt(tno,'mask'))call logwrite(
     *	'Mask item is present ... some data are blanked',more)
      if(hdprsnt(tno,'history'))call logwrite(
     *	'History item is present',more)
      if(hdprsnt(tno,'mostable'))call logwrite(
     *	'Mosaicing information table is present',more)
c
      end
c************************************************************************
      subroutine doaxes (tno,naxis)
c
      implicit none
      integer tno,naxis
c
c     List axes 
c-----------------------------------------------------------------------
      include 'mirconst.h'
      double precision crval,cdelt
      real crpix
      integer n,i,j,length,p
      logical more
      character line*80,aval*72,str*2,radec*12,units*12,pols*32
c
c  Externals.
c
      integer len1
      character itoaf*2,rangle*32,hangle*32,PolsC2P*2
c
      call logwrite('--------------------------------'//
     *		  '--------------------------------',more)
      call logwrite(
     * 'Type     Pixels  Coord Value  at  Pixel     Coord Incr   Units',
     * more)
c
      do i=1,naxis
	units = ' '
        str = itoaf(i)
        call rdhda(tno, 'ctype'//str, aval, 'none')
        call rdhdi(tno,'naxis'//str,n,0)
        call rdhdd (tno, 'crval'//str, crval,0.0d0)
        call rdhdr (tno, 'crpix'//str, crpix,0.0)
        call rdhdd (tno, 'cdelt'//str, cdelt,0.0d0)
c
c  RA.
        if (aval(1:4).eq.'RA--') then
	  radec = hangle(crval)
          write (line, 20) aval(1:8), n, radec,
     *                          crpix,180*3600/pi*cdelt,'  arcsec'
20        format (a8, i7, 3x, a11, f10.2, 3x, 1pe13.6,a)
c
c  DEC, Galactic and Ecliptic coordinates.
        else if (aval(1:4).eq.'DEC-'.or.
     *		 aval(1:4).eq.'GLON'.or.aval(1:4).eq.'GLAT'.or.
     *		 aval(1:4).eq.'ELON'.or.aval(1:4).eq.'ELAT') then
	  radec = rangle(crval)
          write (line, 30) aval(1:8), n, radec,
     *                          crpix,180*3600/pi*cdelt,'  arcsec'
30        format (a8, i7, 2x, a12, f10.2, 3x, 1pe13.6,a)
c
c  STOKES.
	else if(aval(1:6).eq.'STOKES')then
	  length = 0
	  do j=1,n
	    if(length+5.lt.len(pols))then
	      p = nint(crval + cdelt*(j-crpix))
	      if(p.eq.0)then
		pols(length+1:length+5) = ',beam'
		length = length + 5
	      else
	        pols(length+1:length+3) = ','//PolsC2P(p)
		length = len1(pols(1:length+3))
	      endif
	    endif
	  enddo
          write (line, 35) aval(1:8), n, pols(2:length)
35        format (a8, i7, 8x, a)
c
c  Others.
        else
	  if(aval(1:5).eq.'FELO-'.or.aval(1:5).eq.'VELO-')then
	    units = 'km/sec'
	  else if(aval(1:4).eq.'FREQ')then
	    units = 'GHz'
	  else if(aval(1:3).eq.'UU-'.or.aval(1:3).eq.'VV-')then
	    units = 'lambda'
	  endif
          write(line, 40)aval(1:8),n,crval,crpix,cdelt,units
40        format (a8, i7, 2x, 1pe13.6, 0pf9.2, 3x, 1pe13.6,2x,a)
        endif
        call logwrite(line,more)
      enddo
      call logwrite('--------------------------------'//
     *		  '--------------------------------',more)
c
      end
c************************************************************************
	subroutine CalHead(tno,in)
c
	implicit none
	integer tno
	character in*(*)
c------------------------------------------------------------------------
	character line*80
	logical more
c
	line = 'Filename: '//in
	call logwrite(line,more)
	call logwrite('Calibration Data Set',more)
	end
c************************************************************************
	subroutine VisHead(in)
c
	implicit none
	character in*(*)
c
c  Give a summary about a uv data-set.
c------------------------------------------------------------------------
      integer MAXSPECT
      parameter(MAXSPECT=16)
      character line*80,aval1*64,aval2*64,type*1,obstype*32
      integer il1,il2,length,nschan(MAXSPECT),nchan,nspect,npol,pol,i
      integer nants,ival,ncorr,tno,n
      real epoch
      double precision sdf(MAXSPECT),sfreq(MAXSPECT),restfreq(MAXSPECT)
      double precision time,obsra,obsdec,ra,dec
      real wwidth(MAXSPECT),wfreq(MAXSPECT)
      logical updated,present,more
c
c  Externals.
c
      integer len1
      character itoaf*8,PolsC2P*2,hangle*12,rangle*12
      logical hdprsnt
c
c  Close and reopen the file as a visibility file.
c
      call uvopen(tno,in,'old')
      call uvnext(tno)
c
      line = 'Filename: '//in
      call logwrite(line,more)
c
c  Telescope/observer/object parameters.
c
      call uvrdvra(tno,'instrume',aval1,' ')
      call uvrdvra(tno,'telescop',aval2,' ')
      il1 = len1(aval1)
      il2 = len1(aval2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Instrument: '//aval1(1:15)
     *               //' Telescope: '//aval2(1:il2),more)
      else if(il1.gt.0)then
        call logwrite('Instrument: '//aval1(1:il1),more)
      else if(il2.gt.0)then
        call logwrite('Telescope: '//aval2(1:il2),more)
      end if
c
      call uvrdvra(tno,'source',aval1,' ')
      call uvrdvra(tno,'observer',aval2,' ')
      il1 = len1(aval1)
      il2 = len1(aval2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Object: '//aval1(1:19)
     *               //' Observer: '//aval2(1:il2),more)
      else if(il1.gt.0)then
	call logwrite('Object: '//aval1(1:il1),more)
      else if(il2.gt.0)then
        call logwrite('Observer: '//aval2(1:il2),more)
      endif
c
c  Get the start time.
c
      call uvrdvrd(tno,'time',time,0.d0)
      call JulDay(time,'H',aval1)
      call logwrite('First time: '//aval1,more)
c
c  Antennae.
c
      call uvrdvri(tno,'nants',nants,0)
      call logwrite('Number of antennae: '//itoaf(nants),more)
c
c  Determine the polarisations present.
c
      aval1 = 'Polarisations Present: '
      il1 = len('Polarisations Present: ')
      call uvrdvri(tno,'npol',npol,1)
      call uvrdvri(tno,'pol',pol,1)
      aval1(il1+1:il1+2) = PolsC2P(pol)
      il1 = len1(aval1(1:il1+2))
      do i=2,npol
	call uvnext(tno)
	call uvrdvri(tno,'pol',pol,1)
	aval1(il1+1:il1+3) = ','//PolsC2P(pol)
	il1 = len1(aval1(1:il1+3))
      enddo
      call logwrite(aval1(1:il1),more)
c
c  Give message about whether its auto/cross or whatever
c
      call rdhda(tno,'obstype',obstype,' ')
      if(obstype.ne.' ') call logwrite(
     *	'Type of correlations present: '//obstype,more)
c
c  Summarise spectra data.
c
      present = .false.
      call logwrite('--------------------------------'//
     *		  '--------------------------------',more)
      call uvprobvr(tno,'corr',type,length,updated)
      if(type.ne.' ')then
	present = .true.
	call uvrdvri(tno,'nchan',nchan,1)
	call uvrdvri(tno,'nspect',nspect,1)
	call logwrite('Spectral Correlations:',more)
	if(nspect.le.MAXSPECT)then
	  call uvgetvri(tno,'nschan',nschan,nspect)
	  call uvgetvrd(tno,'sfreq',sfreq,nspect)
	  call uvgetvrd(tno,'sdf',sdf,nspect)
	  call uvgetvrd(tno,'restfreq',restfreq,nspect)
	  call logwrite(
     *	    '  Spectrum  Channels  Freq(chan=1)  Increment  Restfreq',
     *	    more)
	  do i=1,nspect
	    write(line,'(i7,i11,f14.5,f13.6,f10.5,a)')
     *		i,nschan(i),sfreq(i),sdf(i),restfreq(i),' GHz'
	    call logwrite(line,more)
	  enddo
	  if(nspect.gt.1)call logwrite('  Total number of channels: '
     *		//itoaf(nchan),more)
	endif
        call rdhdi(tno,'ncorr',ncorr,0)
        if(ncorr.gt.0) call logwrite(
     *	'  Total number of correlations: '//itoaf(ncorr),more)
        if(type.eq.'j')then
          call logwrite('  Correlations are stored in 16-bit form',more)
        else if(type.ne.' ')then
          call logwrite('  Correlations are stored in 32-bit form',more)
        endif
        if(.not.hdprsnt(tno,'flags'))
     *	  call logwrite('  Flagging table is not present',more)
      endif
c
c  Summarize wideband channels.
c
      call uvprobvr(tno,'wcorr',type,length,updated)
      if(type.ne.' ')then
	if(present)call logwrite(' ',more)
	call logwrite('Continuum (wide) correlations: ',more)
	call uvrdvri(tno,'nwide',nchan,1)
	if(nchan.le.MAXSPECT)then
	  call uvgetvrr(tno,'wfreq',wfreq,nchan)
	  call uvgetvrr(tno,'wwidth',wwidth,nchan)
	  call logwrite(
     *	    '  Corr No.  Frequency Bandwidth',more)
	  do i=1,nchan
	    write(line,'(i7,f12.3,f12.6,a)')
     *		i,wfreq(i),wwidth(i),'  GHz'
	    call logwrite(line,more)
	  enddo
	endif
        call rdhdi(tno,'nwcorr',ncorr,0)
        if(ncorr.gt.0)
     *	  call logwrite('  Total number of correlations: '//
     *	  itoaf(ncorr),more)
        if(.not.hdprsnt(tno,'wflags'))
     *	  call logwrite('  Flagging table is not present',more)
      endif
      call logwrite('--------------------------------'//
     *		  '--------------------------------',more)
c
c  RA and DEC.
c
      call uvrdvrd(tno,'ra',ra,0.d0)
      call uvrdvrd(tno,'dec',dec,0.d0)
      call uvrdvrr(tno,'epoch',epoch,2000.)
      ival = nint(epoch)
      if(ival.eq.1950)then
	aval1(1:9) = 'B1950'
      else if(ival.eq.2000)then
	aval1(1:9) = 'J2000'
      else
	aval1(1:9) = itoaf(nint(epoch))
      endif
      line = aval1(1:9)//'Source RA: '//hangle(ra)//
     *			     '  Dec: '//rangle(dec)
      call logwrite(line,more)
      call uvrdvrd(tno,'obsra',obsra,0.d0)
      call uvrdvrd(tno,'obsdec',obsdec,0.d0)
      line = 'Apparent Source RA: '//hangle(obsra)//
     *	         '  Dec: '//rangle(obsdec)
      call logwrite(line,more)
      call uvprobvr(tno,'dra',type,length,updated)
      present = type.ne.' '
      call uvprobvr(tno,'ddec',type,length,updated)
      present = present.or.type.ne.' '
      if(present)call logwrite(
     *	'This may be a multi-pointing data-set',more)
c
c  Tell about which other tables are present.
c
      call logwrite(' ',more)
      if(hdprsnt(tno,'gains'))
     *	call logwrite('Antenna gains table is present',more)
      if(hdprsnt(tno,'bandpass'))
     *	call logwrite('Bandpass correction table is present',more)
      if(hdprsnt(tno,'leakage'))
     *	call logwrite('Polarisation leakage table is present',more)
      if(hdprsnt(tno,'history'))
     *	call logwrite('History item is present',more)
c
c  Determine which aipsfg tables are present.
c
      n = 0
      dowhile(hdprsnt(tno,'aipsfg'//itoaf(n+1)))
	n = n + 1
      enddo
      if(n.gt.0)call logwrite(
     *	'Number of AIPS flagging tables present: '//itoaf(n),more)
c
      call uvclose(tno)
c
      end
