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
c$Id: prthd.for,v 1.7 2011/10/06 05:10:12 cal103 Exp $
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
      character in(MAXIN)*64, line*80, logf*64, version*72

      external  hdprsnt, versan
      logical   hdprsnt
      character versan*80
c-----------------------------------------------------------------------
      version = versan('prthd',
     *                 '$Revision: 1.7 $',
     *                 '$Date: 2011/10/06 05:10:12 $')

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
            call calHead(tno,in(i))
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
      real      rval1, rval2, rval3
      double precision dval
      character aval1*72, aval2*72, keyw*6, line*80

      external  hfff, hdprsnt, itoaf, spaste
      logical   hdprsnt
      character hfff*32, itoaf*12, spaste*80
c-----------------------------------------------------------------------
      line = 'Filename: '//in
      call logwrite(line,more)

c     Telescope/observer/object parameters.
      call rdhda(tno, 'instrume', aval1, ' ')
      call rdhda(tno, 'telescop', aval2, ' ')
      if (aval1.ne.' ') then
        if (aval2.ne.' ') then
          call logwrite('Instrument: '//aval1(1:15)//
     *                  ' Telescope: '//aval2, more)
        else
          call logwrite('Instrument: '//aval1, more)
        endif
      else if (aval2.ne.' ') then
        call logwrite('Telescope: '//aval2, more)
      endif

      call rdhda(tno, 'object',   aval1, ' ')
      call rdhda(tno, 'observer', aval2, ' ')
      if (aval1.ne.' ') then
        if (aval2.ne.' ') then
          call logwrite('Object: '//aval1(1:19)//
     *                  ' Observer: '//aval2, more)
        else
          call logwrite('Object: '//aval1, more)
        endif
      else if (aval2.ne.' ') then
        call logwrite('Observer: '//aval2, more)
      endif

c     Image type.
      call rdhda(tno,'btype',aval1,' ')
      if (aval1.ne.' ') call logwrite('Image type: '//aval1,more)

c     Min, max values and units.
      call rdhdr(tno,'datamax',rval1,0.0)
      call rdhdr(tno,'datamin',rval2,rval1+1.0)
      call rdhda(tno,'bunit',aval1, ' ')
      if (rval1.ge.rval2) then
        write(line, 10) rval1, rval2, aval1(:16)
 10     format('Maximum: ',1pe15.8,4x,'Minimum: ',1pe15.8,2x,a)
        call logwrite(line,more)
      else
        call logwrite('Map flux units: '//aval1,more)
      endif

c     Synthesised beam parameters.
      call rdhdr(tno, 'bmaj', rval1, 0.0)
      call rdhdr(tno, 'bmin', rval2, 0.0)
      call rdhdr(tno, 'bpa',  rval3, 0.0)
      if (rval1.gt.0.0 .and. rval2.gt.0.0) then
        if (max(rval1,rval2)*R2AS.lt.1000.0) then
          write(line,'(a,f7.2,a,f7.2,a)')
     *      'Beam Size:',rval1*R2AS,' by',rval2*R2AS,' arcsec.'
        else
          write(line,'(a,f7.2,a,f7.2,a)')
     *      'Beam Size:',rval1*R2D*60.0,' by',rval2*R2D*60.0,' arcmin.'
        endif
        call logwrite(line,more)
        write(line,'(a,f7.1,a)')'Position angle:',rval3,' degrees.'
        call logwrite(line,more)
      endif

c     Summarise axis coordinate info.
      call rdhdi(tno, 'naxis', ival, 0)
      write(line,'(a,i2,a)')'This image has',ival,' axes.'
      call logwrite(line,more)
      call doaxes(tno,ival)

c     Parameters related to coordinates.
      call rdhdd(tno, 'llrot', dval, 0d0)
      if (dval.ne.0d0) then
        write(line,'(a,f6.1,a)')
     *    'Image/Sky Rotation Angle:',dval*R2D,' degrees'
        call logwrite(line, more)
      endif

c     lonpole and latpole.
      if (hdprsnt(tno,'lonpole')) then
        call rdhdd(tno, 'lonpole', dval, 0d0)
        aval1 = hfff(dval, 0.1d0, 1d3, 1, 'f10.2', '1pe10.2')

        if (hdprsnt(tno,'latpole')) then
          line = spaste('lonpole,latpole: (', '//', aval1, ' ')

          call rdhdd(tno, 'latpole', dval, 0d0)
          aval1 = hfff(dval, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
          line = spaste(line, ',', aval1, ') deg')
        else
          line = spaste('lonpole:', ' ', aval1, ' deg')
        endif

        call logwrite(line, more)
      else if (hdprsnt(tno,'latpole')) then
        call rdhdd(tno, 'latpole', dval, 0d0)
        aval1 = hfff(dval, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
        line = spaste('latpole:', ' ', aval1, ' deg')
        call logwrite(line, more)
      endif

c     phi0, theta0, and xyzero.
      if (hdprsnt(tno,'phi0') .or. hdprsnt(tno,'theta0')) then
        if (hdprsnt(tno,'phi0')) then
          call rdhdd(tno, 'phi0', dval, 0d0)
          aval1 = hfff(dval, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
          if (hdprsnt(tno,'theta0')) then
            line = spaste('phi0,theta0: (', '//', aval1, ' ')
            call rdhdd(tno, 'theta0', dval, 0d0)
            aval1 = hfff(dval, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
            line = spaste(line, ',', aval1, ') deg')
          else
            line = spaste('phi0:', ' ', aval1, '  deg')
          endif
        else if (hdprsnt(tno,'theta0')) then
          call rdhdd(tno, 'theta0', dval, 0d0)
          aval1 = hfff(dval, 1d0, 1d3, 1, 'f10.2', '1pe10.2')
          line = spaste('theta0:', ' ', aval1, ' deg')
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
          call rdhdd(tno, keyw, dval, 0d0)
          keyw = spaste(keyw, '//', ':', ' ')
          line = keyw // hfff(dval, 1d-3, 1d3, 1, 'f13.8', '1pe17.8')
          call logwrite(line, more)
        endif
      enddo

c     Time of observation.
      call rdhdd(tno, 'obstime', dval, 0d0)
      if (dval.gt.0) then
        call julday(dval,'H',aval1)
        line = 'Average time of observation: '//aval1
        call logwrite(line,more)
      endif

c     Equinox.
      call rdhdr(tno, 'epoch', rval1, 0.0)
      if (rval1.gt.0) then
        if (rval1.lt.1984.0) then
          aval1 = 'B'
        else
          aval1 = 'J'
        endif

        write(line, '(a,a,f6.1,a)')
     *    'Equinox:                     ',aval1(1:1),rval1
        call logwrite(line,more)
      endif

c     Rest frequency.
      call rdhdd(tno,'restfreq',dval,0d0)
      if (dval.gt.0d0) then
        write(line,'(a,f13.6,a)')
     *    'Rest frequency:         ',dval,' GHz'
        call logwrite(line,more)
      endif

c     Observatory radial velocity.
      if (hdprsnt(tno,'vobs')) then
        call rdhdr(tno,'vobs',rval1,0.0)
        write(line,'(a,f8.2,a)')
     *    'Observatory radial velocity:',rval1,' km/s'
        call logwrite(line,more)
      endif

c     Rms noise.
      call rdhdr(tno, 'rms', rval1, -1.0)
      if (rval1.gt.0) then
        write(line,'(a,1pe10.3)')
     *    'Nominal Theoretical Rms:    ',rval1
        call logwrite(line,more)
      endif

c     Primary beam parameters.
      call rdhda(tno, 'pbtype', aval1, ' ')
      if (aval1.ne.' ') then
        call logwrite('Primary beam type: '//aval1,more)
      else
        call rdhdr(tno, 'pbfwhm', rval1, -1.0)
        if (rval1.gt.0) then
          write(line,'(a,1pg10.3)')
     *      'Primary beam size (arcsec): ',rval1
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
      integer   i, j, length, n, p
      double precision cdelt, crpix, crval
      character ctype*72, line*80, pols*32, radec*12, str*2, units*12

      external  itoaf, hangleh, len1, polsC2P, rangleh
      integer   len1
      character itoaf*2, hangleh*32, polsC2P*2, rangleh*32
c-----------------------------------------------------------------------
      call logwrite('--------------------------------'//
     *              '--------------------------------',more)
      call logwrite(
     * 'Type     Pixels  Coord Value  at  Pixel     Coord Incr   Units',
     * more)

      do i = 1, naxis
        units = ' '
        str = itoaf(i)
        call rdhdi(tno, 'naxis'//str, n, 0)
        call rdhda(tno, 'ctype'//str, ctype, 'none')
        call rdhdd(tno, 'crval'//str, crval, 0d0)
        call rdhdd(tno, 'crpix'//str, crpix, 0d0)
        call rdhdd(tno, 'cdelt'//str, cdelt, 0d0)

        if (ctype(1:4).eq.'RA--' .or. ctype.eq.'RA') then
c         RA.
          radec = hangleh(crval)
          write(line, 10) ctype(1:8), n, radec, crpix, cdelt*R2AS
 10       format(a8, i7, 3x, a11, f10.2, 1pe16.6, '  arcsec')

        else if (ctype(1:4).eq.'DEC-' .or. ctype.eq.'DEC') then
c         Dec.
          radec = rangleh(crval)
          write(line, 20) ctype(1:8), n, radec, crpix, cdelt*R2AS
 20       format(a8, i7, 2x, a12, f10.2, 1pe16.6, '  arcsec')
        else if (ctype(1:4).eq.'GLON' .or. ctype(1:4).eq.'GLAT' .or.
     *           ctype(1:4).eq.'ELON' .or. ctype(1:4).eq.'ELAT') then
c         Galactic and Ecliptic coordinates.
          write(line,30) ctype(1:8), n, crval*DR2D, crpix, cdelt*DR2D
 30       format(a8,i7,f14.6,f10.2,1pe16.6,'  deg')

        else if (ctype.eq.'ANGLE') then
c         Angles on the sky.
          write(line, 40) ctype(1:8), n, crval*DR2D, crpix, cdelt*DR2AS,
      *     'arcsec'

        else if (ctype.eq.'STOKES') then
c         Stokes.
          length = 0
          do j = 1, n
            if (length+5.lt.len(pols)) then
              p = nint(crval + cdelt*(j-crpix))
              if (p.eq.0) then
                pols(length+1:length+5) = ',beam'
                length = length + 5
              else
                pols(length+1:length+3) = ','//polsC2P(p)
                length = len1(pols(1:length+3))
              endif
            endif
          enddo

          write(line,40) ctype(1:8), n, pols(2:length)
 40       format(a8,i7,8x,a)

        else
c         Others.
          if (ctype(1:4).eq.'FREQ') then
            units = 'GHz'
          else if (ctype(:5).eq.'VRAD-' .or. ctype.eq.'VRAD' .or.
     *             ctype(:5).eq.'VOPT-' .or. ctype.eq.'VOPT' .or.
     *             ctype(:5).eq.'VELO-' .or.
     *             ctype(:5).eq.'FELO-') then
            units = 'km/s'
          else if (ctype(:3).eq.'UU-' .or. ctype(:3).eq.'VV-') then
            units = 'lambda'
          else if (ctype.eq.'TIME') then
            units = 's'
          endif

          write(line,50) ctype(1:8), n, crval, crpix, cdelt, units
 50       format(a8,i7,2x,1pe13.6,0pf9.2,3x,1pe13.6,2x,a)
        endif

        call logwrite(line,more)
      enddo

      call logwrite('--------------------------------'//
     *              '--------------------------------',more)

      end

c***********************************************************************

      subroutine calHead(tno,in)

      integer tno
      character in*(*)
c-----------------------------------------------------------------------
      character line*80
      logical more
c-----------------------------------------------------------------------
      line = 'Filename: '//in
      call logwrite(line,more)
      call logwrite('Calibration Data Set',more)

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
      character aval1*64, aval2*64, line*80, obstype*32, type*1

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
      call uvrdvra(tno,'instrume',aval1,' ')
      call uvrdvra(tno,'telescop',aval2,' ')
      il1 = len1(aval1)
      il2 = len1(aval2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Instrument: '//aval1(1:15)//
     *                ' Telescope: '//aval2(1:il2),more)
      else if (il1.gt.0) then
        call logwrite('Instrument: '//aval1(1:il1),more)
      else if (il2.gt.0) then
        call logwrite('Telescope: '//aval2(1:il2),more)
      endif

      call uvrdvra(tno,'source',aval1,' ')
      call uvrdvra(tno,'observer',aval2,' ')
      il1 = len1(aval1)
      il2 = len1(aval2)
      if (il1.gt.0 .and. il2.gt.0) then
        call logwrite('Object: '//aval1(1:19)
     *               //' Observer: '//aval2(1:il2),more)
      else if (il1.gt.0) then
        call logwrite('Object: '//aval1(1:il1),more)
      else if (il2.gt.0) then
        call logwrite('Observer: '//aval2(1:il2),more)
      endif

c     Get the start time.
      call uvrdvrd(tno,'time',time,0d0)
      call JulDay(time,'H',aval1)
      call logwrite('First time: '//aval1,more)

c     Antennae.
      call uvrdvri(tno,'nants',nants,0)
      call logwrite('Number of antennae: '//itoaf(nants),more)

c     Determine the polarisations present.
      aval1 = 'Polarisations Present: '
      il1 = len('Polarisations Present: ')
      call uvrdvri(tno,'npol',npol,1)
      call uvrdvri(tno,'pol',pol,1)
      aval1(il1+1:il1+2) = polsC2P(pol)
      il1 = len1(aval1(1:il1+2))
      do i = 2, npol
        call uvnext(tno)
        call uvrdvri(tno,'pol',pol,1)
        aval1(il1+1:il1+3) = ','//polsC2P(pol)
        il1 = len1(aval1(1:il1+3))
      enddo
      call logwrite(aval1(1:il1),more)

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
        aval1(1:9) = 'B1950'
      else if (ival.eq.2000) then
        aval1(1:9) = 'J2000'
      else
        aval1(1:9) = itoaf(nint(epoch))
      endif
      line = aval1(1:9)//'Source RA: '//hangleh(ra)//
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

c     Tell about which other tables are present.
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
