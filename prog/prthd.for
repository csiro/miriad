      program prthd

c= PRTHD -- Summarise the contents of a data-set.
c& rjs
c: miscellaneous
c+
c       PRTHD is a Miriad task that summarises a Miriad data-set.
c@ in
c       Input image and/or uv data-sets.  Several may be given, and
c       their names may include wildcards.  No default.
c@ log
c       Output log file.  Default is the terminal.
c@ options
c       Extra processing options.  Possible values are:
c         brief   Give one line description of each file.
c         full    Several line description of each file (default).
c
c$Id: prthd.for,v 1.10 2012/03/02 06:13:02 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c  Bugs and Shortcomings:
c    * Descriptions in brief mode could be a bit more verbose!
c-----------------------------------------------------------------------
      integer    MAXIN
      parameter (MAXIN=256)

      logical   full, more
      integer   i, iostat, nin, tno
      character in(MAXIN)*132, line*80, logf*132, version*72

      external  hdprsnt, versan
      logical   hdprsnt
      character versan*72
c-----------------------------------------------------------------------
      version = versan('prthd',
     *                 '$Revision: 1.10 $',
     *                 '$Date: 2012/03/02 06:13:02 $')

c     Get input parameters.
      call keyini
      call mkeyf('in',in,MAXIN,nin)
      call keya('log',logf,' ')
      call getOpt(full)
      call keyfin
      if (nin.eq.0) call bug('f','No inputs given')

c     Open the output log file.
      call logopen(logf,' ')

c     Summarise each of the input files according to type.
      if (full) then
c       Full listing.
        do i = 1, nin
          call logwrite('********************************'//
     *                  '********************************',more)
          call hopen(tno,in(i),'old',iostat)
          if (iostat.ne.0) then
            call bug('w','Error opening '//in(i))
            call bugno('w',iostat)
          else if (hdprsnt(tno,'image')) then
            call imHead(tno,in(i))
            call hclose(tno)
          else if (hdprsnt(tno,'visdata')) then
            call hclose(tno)
            call visHead(in(i))
          else if (hdprsnt(tno,'rdata')) then
            call logwrite('Filename: '//in(i),more)
            call logwrite('Calibration Data Set',more)
            call hclose(tno)
          else
            call bug('w','Unrecognised format for '//in(i))
            call bug('w','Use ITEMIZE to inspect this data-set')
            call hclose(tno)
          endif
        enddo

      else
c       Brief listing.
        do i = 1, nin
          line(1:32) = in(i)
          call hopen(tno,in(i),'old',iostat)
          if (iostat.ne.0) then
            line(33:) = ': Regular file or directory'
          else if (hdprsnt(tno,'image')) then
            line(33:) = ': Image data-set'
          else if (hdprsnt(tno,'visdata')) then
            line(33:) = ': Visibility data-set'
          else if (hdprsnt(tno,'rdata')) then
            line(33:) = ': Calibration data-set'
          else
            line(33:) = ': Miriad data-set in unknown format'
          endif
          call logwrite(line,more)
          if (iostat.eq.0) call hclose(tno)
        enddo
      endif

      call logclose

      end

c***********************************************************************

      subroutine getOpt(full)

      logical full
c-----------------------------------------------------------------------
c  Get extra processing options.
c-----------------------------------------------------------------------
      integer    NOPTS
      parameter (NOPTS=2)

      logical   brief, present(NOPTS)
      character opts(NOPTS)*8

      data opts /'brief   ','full    '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      brief = present(1)
      full  = present(2) .or. .not.present(1)
      if (brief .and. full) call bug('f',
     *  'Cannot mix options brief and full')

      end

c***********************************************************************

      subroutine imHead(tno,in)

      integer tno
      character in*(*)
c-----------------------------------------------------------------------
c  Summarise an image.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      logical   more
      integer   ival, m
      real      rVal1, rVal2, rVal3
      double precision dVal
      character aVal1*72, aVal2*72, keyw*72, line*132

      external  hfff, hdprsnt, itoaf, spaste
      logical   hdprsnt
      character hfff*32, itoaf*12, spaste*72
c-----------------------------------------------------------------------
      line = 'Filename: '//in
      call logwrite(line,more)

c     Telescope/observer/object parameters.
      call rdhda(tno, 'instrume', aVal1, ' ')
      call rdhda(tno, 'telescop', aVal2, ' ')
      if (aVal1.ne.' ') then
        if (aVal2.ne.' ') then
          call logwrite('Instrument: '//aVal1(1:15)//
     *                  ' Telescope: '//aVal2, more)
        else
          call logwrite('Instrument: '//aVal1, more)
        endif
      else if (aVal2.ne.' ') then
        call logwrite('Telescope: '//aVal2, more)
      endif

      call rdhda(tno, 'object',   aVal1, ' ')
      call rdhda(tno, 'observer', aVal2, ' ')
      if (aVal1.ne.' ') then
        if (aVal2.ne.' ') then
          call logwrite('Object: '//aVal1(1:19)//
     *                  ' Observer: '//aVal2, more)
        else
          call logwrite('Object: '//aVal1, more)
        endif
      else if (aVal2.ne.' ') then
        call logwrite('Observer: '//aVal2, more)
      endif

c     Image type.
      call rdhda(tno,'btype',aVal1,' ')
      if (aVal1.ne.' ') call logwrite('Image type: '//aVal1,more)

c     Min, max values and units.
      call rdhdr(tno,'datamax',rVal1,0.0)
      call rdhdr(tno,'datamin',rVal2,rVal1+1.0)
      call rdhda(tno,'bunit',aVal1, ' ')
      if (rVal1.ge.rVal2) then
        write(line, 10) rVal1, rVal2, aVal1(:16)
 10     format('Maximum: ',1pe15.8,4x,'Minimum: ',1pe15.8,2x,a)
        call logwrite(line,more)
      else
        call logwrite('Map flux units: '//aVal1,more)
      endif

c     Synthesised beam parameters.
      call rdhdr(tno, 'bmaj', rVal1, 0.0)
      call rdhdr(tno, 'bmin', rVal2, 0.0)
      call rdhdr(tno, 'bpa',  rVal3, 0.0)
      if (rVal1.gt.0.0 .and. rVal2.gt.0.0) then
        if (max(rVal1,rVal2)*R2AS.lt.1000.0) then
          write(line,'(a,f7.2,a,f7.2,a)')
     *      'Beam Size:',rVal1*R2AS,' by',rVal2*R2AS,' arcsec.'
        else
          write(line,'(a,f7.2,a,f7.2,a)')
     *      'Beam Size:',rVal1*R2D*60.0,' by',rVal2*R2D*60.0,' arcmin.'
        endif
        call logwrite(line,more)
        write(line,'(a,f7.1,a)')'Position angle:',rVal3,' degrees.'
        call logwrite(line,more)
      endif

c     Summarise axis coordinate info.
      call rdhdi(tno, 'naxis', ival, 0)
      write(line,'(a,i2,a)')'This image has',ival,' axes.'
      call logwrite(line,more)
      call doaxes(tno,ival)

c     Parameters related to coordinates.
      call rdhdd(tno, 'llrot', dVal, 0d0)
      if (dVal.ne.0d0) then
        write(line,'(a,f6.1,a)')
     *    'Image/Sky Rotation Angle:',dVal*DR2D,' degrees'
        call logwrite(line, more)
      endif

c     lonpole and latpole.
      if (hdprsnt(tno,'lonpole')) then
        call rdhdd(tno, 'lonpole', dVal, 0d0)
        aVal1 = hfff(dVal, 0.1d0, 1d3, 1, 'f10.2', '1pe10.2')

        if (hdprsnt(tno,'latpole')) then
          line = spaste('lonpole,latpole: (', '//', aVal1, ' ')

          call rdhdd(tno, 'latpole', dVal, 0d0)
          aVal1 = hfff(dVal, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
          line = spaste(line, ',', aVal1, ') deg')
        else
          line = spaste('lonpole:', ' ', aVal1, ' deg')
        endif

        call logwrite(line, more)
      else if (hdprsnt(tno,'latpole')) then
        call rdhdd(tno, 'latpole', dVal, 0d0)
        aVal1 = hfff(dVal, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
        line = spaste('latpole:', ' ', aVal1, ' deg')
        call logwrite(line, more)
      endif

c     phi0, theta0, and xyzero.
      if (hdprsnt(tno,'phi0') .or. hdprsnt(tno,'theta0')) then
        if (hdprsnt(tno,'phi0')) then
          call rdhdd(tno, 'phi0', dVal, 0d0)
          aVal1 = hfff(dVal, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
          if (hdprsnt(tno,'theta0')) then
            line = spaste('phi0,theta0: (', '//', aVal1, ' ')
            call rdhdd(tno, 'theta0', dVal, 0d0)
            aVal1 = hfff(dVal, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
            line = spaste(line, ',', aVal1, ') deg')
          else
            line = spaste('phi0:', ' ', aVal1, '  deg')
          endif
        else if (hdprsnt(tno,'theta0')) then
          call rdhdd(tno, 'theta0', dVal, 0d0)
          aVal1 = hfff(dVal, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
          line = spaste('theta0:', ' ', aVal1, ' deg')
        endif

        call rdhdi(tno, 'xyzero', ival, 0)
        if (ival.eq.0) then
          line = spaste(line, ', ', 'offset not applied', ' ')
        else
          line = spaste(line, ', ', 'offset applied', ' ')
        endif

        call logwrite(line, more)
      endif

c     Projection parameters.
      do m = 0, 29
        keyw = 'pv' // itoaf(m)
        if (hdprsnt(tno,keyw)) then
          call rdhdd(tno, keyw, dVal, 0d0)
          keyw = spaste(keyw, '//', ':', ' ')
          line = keyw(:6) // hfff(dVal,1d-3,1d3,1,'f13.8','1pe17.8')
          call logwrite(line, more)
        endif
      enddo

c     Time of observation.
      call rdhdd(tno, 'obstime', dVal, 0d0)
      if (dVal.gt.0) then
        call julday(dVal,'H',aVal1)
        line = 'Average time of observation: '//aVal1
        call logwrite(line,more)
      endif

c     Equinox.
      call rdhdr(tno, 'epoch', rVal1, 0.0)
      if (rVal1.gt.0) then
        if (rVal1.lt.1984.0) then
          aVal1 = 'B'
        else
          aVal1 = 'J'
        endif

        write(line, '(a,a,f6.1,a)')
     *    'Equinox:                     ',aVal1(1:1),rVal1
        call logwrite(line,more)
      endif

c     Rest frequency.
      call rdhdd(tno,'restfreq',dVal,0d0)
      if (dVal.gt.0d0) then
        write(line,'(a,f13.6,a)')
     *    'Rest frequency:         ',dVal,' GHz'
        call logwrite(line,more)
      endif

c     Doppler frame.
      call rdhda(tno, 'specsys', aVal1, ' ')
      if (aVal1.ne.' ') then
        call logwrite('Doppler reference frame:     '//aVal1,more)
      endif

c     Observatory radial velocity.
      if (hdprsnt(tno,'vobs')) then
        call rdhdr(tno,'vobs',rVal1,0.0)
        write(line,'(a,f8.2,a)')
     *    'Observatory radial velocity:',rVal1,' km/s'
        call logwrite(line,more)
      endif

c     Rms noise.
      call rdhdr(tno, 'rms', rVal1, -1.0)
      if (rVal1.gt.0) then
        write(line,'(a,1pe10.3)')
     *    'Nominal Theoretical Rms:    ',rVal1
        call logwrite(line,more)
      endif

c     Primary beam parameters.
      call rdhda(tno, 'pbtype', aVal1, ' ')
      if (aVal1.ne.' ') then
        call logwrite('Primary beam type: '//aVal1,more)
      else
        call rdhdr(tno, 'pbfwhm', rVal1, -1.0)
        if (rVal1.gt.0) then
          write(line,'(a,1pg10.3)')
     *      'Primary beam size (arcsec): ',rVal1
          call logwrite(line,more)
        endif
      endif

c     Number of clean components.
      call rdhdi(tno,'niters',ival,0)
      if (ival.gt.0) call logwrite(
     *  'Number of iterations:       '//itoaf(ival),more)

c     Check for extra tables, etc.
      if (hdprsnt(tno,'mask')) call logwrite(
     *  'Mask item is present ... some data are blanked',more)
      if (hdprsnt(tno,'history')) call logwrite(
     *  'History item is present',more)
      if (hdprsnt(tno,'mostable')) call logwrite(
     *  'Mosaicing information table is present',more)

      end

c***********************************************************************

      subroutine doaxes (tno,naxis)

      integer tno, naxis
c-----------------------------------------------------------------------
c  List axes.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      logical   more
      integer   iax, ipol, j, length, naxisj
      double precision cdelt, crpix, crval, scl
      character algo*8, axtype*16, cax*2, ctype*72, line*80, pols*32,
     *          radec*13, units*8, wtype*16

      external  itoaf, hangleh, len1, polsC2P, rangleh
      integer   len1
      character itoaf*2, hangleh*13, polsC2P*2, rangleh*13
c-----------------------------------------------------------------------
      call logwrite('--------------------------------'//
     *              '--------------------------------',more)
      call logwrite(
     * 'Type     Pixels  Coord Value  at  Pixel     Coord Incr   Units',
     * more)

      do iax = 1, naxis
        cax = itoaf(iax)
        call rdhdi(tno, 'naxis'//cax, naxisj, 0)
        call rdhda(tno, 'ctype'//cax, ctype, 'none')
        call rdhdd(tno, 'crval'//cax, crval, 0d0)
        call rdhdd(tno, 'crpix'//cax, crpix, 0d0)
        call rdhdd(tno, 'cdelt'//cax, cdelt, 0d0)

        call coCtype(ctype, axtype, wtype, algo, units, scl)
        if (wtype.eq.'RA') then
c         RA.
          radec = hangleh(crval)
          write(line, 10) ctype(:8), naxisj, radec, crpix, cdelt*R2AS
 10       format(a8, i7, 3x, a12, f9.2, 1pe16.6, '  arcsec')

        else if (wtype.eq.'DEC') then
c         Dec.
          radec = rangleh(crval)
          write(line, 20) ctype(:8), naxisj, radec, crpix, cdelt*R2AS
 20       format(a8, i7, 2x, a13, f9.2, 1pe16.6, '  arcsec')

        else if (ctype.eq.'ANGLE') then
c         Angles on the sky.
          write(line, 40) ctype(:8), naxisj, crval*DR2D, crpix,
      *     cdelt*DR2AS, 'arcsec'

        else if (units.eq.'rad') then
c         Galactic and Ecliptic coordinates.
          write(line,30) ctype(:8), naxisj, crval*DR2D, crpix,
      *     cdelt*DR2D
 30       format(a8,i7,f14.6,f10.2,1pe16.6,'  deg')

        else if (ctype.eq.'STOKES') then
c         Stokes.
          length = 0
          do j = 1, naxisj
            if (length+5.lt.len(pols)) then
              ipol = nint(crval + cdelt*(j-crpix))
              if (ipol.eq.0) then
                pols(length+1:length+5) = ',beam'
                length = length + 5
              else
                pols(length+1:length+3) = ','//polsC2P(ipol)
                length = len1(pols(1:length+3))
              endif
            endif
          enddo

          write(line,40) ctype(:8), naxisj, pols(2:length)
 40       format(a8,i7,8x,a)

        else
c         Others.
          write(line,50) ctype(:8), naxisj, crval, crpix, cdelt, units
 50       format(a8,i7,2x,1pe13.6,0pf9.2,3x,1pe13.6,2x,a)
        endif

        call logwrite(line,more)
      enddo

      call logwrite('--------------------------------'//
     *              '--------------------------------',more)

      end

c***********************************************************************

      subroutine visHead(in)

      character in*(*)
c-----------------------------------------------------------------------
c  Summarize a uv data-set.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical   more, present, updated
      integer   i, il1, il2, ival, length, n, nants, nchan, ncorr, npol,
     *          nschan(MAXWIN), nspect, pol, tno
      real      epoch, wfreq(MAXWIDE), wwidth(MAXWIDE)
      double precision dec, deldec, delra, obsdec, obsra, pntdec, pntra,
     *          ra, restfreq(MAXWIN), sdf(MAXWIN), sfreq(MAXWIN), time
      character aVal1*64, aVal2*64, line*132, obstype*32, type*1

      external  hangleh, hdprsnt, itoaf, len1, polsC2P, rangleh
      logical   hdprsnt
      integer   len1
      character hangleh*12, itoaf*12, polsC2P*2, rangleh*12
c-----------------------------------------------------------------------
c     Close and reopen the file as a visibility file.
      call uvopen(tno,in,'old')
      call uvnext(tno)

      line = 'Filename: '//in
      call logwrite(line,more)

c     Telescope/observer/object parameters.
      call uvrdvra(tno,'instrume',aVal1,' ')
      call uvrdvra(tno,'telescop',aVal2,' ')
      il1 = len1(aVal1)
      il2 = len1(aVal2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Instrument: '//aVal1(1:15)//
     *                ' Telescope: '//aVal2(1:il2),more)
      else if (il1.gt.0) then
        call logwrite('Instrument: '//aVal1(1:il1),more)
      else if (il2.gt.0) then
        call logwrite('Telescope: '//aVal2(1:il2),more)
      endif

      call uvrdvra(tno,'source',aVal1,' ')
      call uvrdvra(tno,'observer',aVal2,' ')
      il1 = len1(aVal1)
      il2 = len1(aVal2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Object: '//aVal1(1:19)
     *               //' Observer: '//aVal2(1:il2),more)
      else if (il1.gt.0) then
        call logwrite('Object: '//aVal1(1:il1),more)
      else if (il2.gt.0) then
        call logwrite('Observer: '//aVal2(1:il2),more)
      endif

c     Get the start time.
      call uvrdvrd(tno,'time',time,0d0)
      call JulDay(time,'H',aVal1)
      call logwrite('First time: '//aVal1,more)

c     Antennae.
      call uvrdvri(tno,'nants',nants,0)
      call logwrite('Number of antennae: '//itoaf(nants),more)

c     Determine the polarisations present.
      aVal1 = 'Polarisations Present: '
      il1 = len('Polarisations Present: ')
      call uvrdvri(tno,'npol',npol,1)
      call uvrdvri(tno,'pol',pol,1)
      aVal1(il1+1:il1+2) = polsC2P(pol)
      il1 = len1(aVal1(1:il1+2))
      do i = 2, npol
        call uvnext(tno)
        call uvrdvri(tno,'pol',pol,1)
        aVal1(il1+1:il1+3) = ','//polsC2P(pol)
        il1 = len1(aVal1(1:il1+3))
      enddo
      call logwrite(aVal1(1:il1),more)

c     Give message about whether its auto/cross or whatever
      call rdhda(tno,'obstype',obstype,' ')
      if (obstype.ne.' ') call logwrite(
     *  'Type of correlations present: '//obstype,more)

c     Summarise spectra data.
      present = .false.
      call logwrite('--------------------------------'//
     *              '--------------------------------',more)
      call uvprobvr(tno,'corr',type,length,updated)
      if (type.ne.' ') then
        present = .true.
        call uvrdvri(tno,'nchan',nchan,1)
        call uvrdvri(tno,'nspect',nspect,1)
        call logwrite('Spectral Correlations:',more)
        if (nspect.le.MAXWIN) then
          call uvgetvri(tno,'nschan',nschan,nspect)
          call uvgetvrd(tno,'sfreq',sfreq,nspect)
          call uvgetvrd(tno,'sdf',sdf,nspect)
          call uvgetvrd(tno,'restfreq',restfreq,nspect)
          call logwrite(
     *      '  Spectrum  Channels  Freq(chan=1)  Increment  Restfreq',
     *      more)
          do i = 1, nspect
            write(line,'(i7,i11,f14.5,f13.6,f10.5,a)')
     *          i,nschan(i),sfreq(i),sdf(i),restfreq(i),' GHz'
            call logwrite(line,more)
          enddo
          if (nspect.gt.1) call logwrite('  Total number of channels: '
     *          //itoaf(nchan),more)
        endif
        call rdhdi(tno,'ncorr',ncorr,0)
        if (ncorr.gt.0) call logwrite(
     *  '  Total number of correlations: '//itoaf(ncorr),more)
        if (type.eq.'j') then
          call logwrite('  Correlations are stored in 16-bit form',more)
        else if (type.ne.' ') then
          call logwrite('  Correlations are stored in 32-bit form',more)
        endif
        if (.not.hdprsnt(tno,'flags'))
     *    call logwrite('  Flagging table is not present',more)
      endif

c     Summarize wideband channels.
      call uvprobvr(tno,'wcorr',type,length,updated)
      if (type.ne.' ') then
        if (present) call logwrite(' ',more)
        call logwrite('Continuum (wide) correlations: ',more)
        call uvrdvri(tno,'nwide',nchan,1)
        if (nchan.le.MAXWIDE) then
          call uvgetvrr(tno,'wfreq',wfreq,nchan)
          call uvgetvrr(tno,'wwidth',wwidth,nchan)
          call logwrite(
     *      '  Corr No.  Frequency Bandwidth',more)
          do i = 1, nchan
            write(line,'(i7,f12.3,f12.6,a)')
     *          i,wfreq(i),wwidth(i),'  GHz'
            call logwrite(line,more)
          enddo
        endif
        call rdhdi(tno,'nwcorr',ncorr,0)
        if (ncorr.gt.0)
     *    call logwrite('  Total number of correlations: '//
     *    itoaf(ncorr),more)
        if (.not.hdprsnt(tno,'wflags'))
     *    call logwrite('  Flagging table is not present',more)
      endif
      call logwrite('--------------------------------'//
     *              '--------------------------------',more)

c     RA and DEC.
      call uvrdvrd(tno,'ra',ra,0d0)
      call uvrdvrd(tno,'dec',dec,0d0)
      call uvrdvrr(tno,'epoch',epoch,2000.0)
      ival = nint(epoch)
      if (ival.eq.1950) then
        aVal1(1:9) = 'B1950'
      else if (ival.eq.2000) then
        aVal1(1:9) = 'J2000'
      else
        aVal1(1:9) = itoaf(nint(epoch))
      endif
      line = aVal1(1:9)//'Source RA: '//hangleh(ra)//
     *                       '  Dec: '//rangleh(dec)
      call logwrite(line,more)

      call uvprobvr(tno,'delra', type,length,updated)
      present = type.ne.' '
      call uvprobvr(tno,'deldec',type,length,updated)
      present = present .or. type.ne.' '
      if (present) then
        call uvrdvrd(tno,'delra', delra,ra)
        call uvrdvrd(tno,'deldec',deldec,dec)
        line = 'Delay Tracking  RA: '//hangleh(delra)//
     *           '  Dec: '//rangleh(deldec)
        call logwrite(line,more)
      endif

      call uvprobvr(tno,'pntra', type,length,updated)
      present = type.ne.' '
      call uvprobvr(tno,'pntdec',type,length,updated)
      present = present .or. type.ne.' '
      if (present) then
        call uvrdvrd(tno,'pntra', pntra,ra)
        call uvrdvrd(tno,'pntdec',pntdec,dec)
        line = 'Pointing Centre RA: '//hangleh(pntra)//
     *           '  Dec: '//rangleh(pntdec)
        call logwrite(line,more)
      endif

      call uvrdvrd(tno,'obsra',obsra,ra)
      call uvrdvrd(tno,'obsdec',obsdec,dec)
      line = 'Apparent Source RA: '//hangleh(obsra)//
     *           '  Dec: '//rangleh(obsdec)
      call logwrite(line,more)

      call uvprobvr(tno,'dra',type,length,updated)
      present = type.ne.' '
      call uvprobvr(tno,'ddec',type,length,updated)
      present = present .or. type.ne.' '
      if (present) call logwrite(
     *  'This may be a multi-pointing data-set',more)

c     Report which other tables are present.
      call logwrite(' ',more)
      if (hdprsnt(tno,'gains'))
     *  call logwrite('Antenna gains table is present',more)
      if (hdprsnt(tno,'bandpass'))
     *  call logwrite('Bandpass correction table is present',more)
      if (hdprsnt(tno,'leakage'))
     *  call logwrite('Polarisation leakage table is present',more)
      if (hdprsnt(tno,'history'))
     *  call logwrite('History item is present',more)

c     Determine which aipsfg tables are present.
      n = 0
      do while (hdprsnt(tno,'aipsfg'//itoaf(n+1)))
        n = n + 1
      enddo
      if (n.gt.0) call logwrite(
     *  'Number of AIPS flagging tables present: '//itoaf(n),more)

      call uvclose(tno)

      end
