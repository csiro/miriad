      program impos
c
c= IMPOS - Converts image coordinates between different systems.
c& nebk
c: image analysis
c+
c       IMPOS takes a coordinate in a specified system (such
c       as "abspix" or "arcsec") and converts it to all appropriate
c       coordinate systems (absolute world, offset world, pixels,
c       offset pixels).   Spectral axes are converted to values in
c       frequency, radio and optical velocities.
c
c       If the input is an image and the specified coordinate represents
c       a valid pixel, its value is reported as well.
c
c@ in
c       The input image or visibility dataset. For a visibility dataset,
c       the coordinate system is relative to the first visibility
c       record.
c@ coord
c       Specify the coordinate for each axis that you are interested
c       in; you don't necessarily need one for every axis in the image.
c       No default.
c@ type
c       Specify the coordinate system of the input coordinate for each
c       axis.  Different axes can be in different systems. Choose from:
c
c          "hms"         HH:MM:SS.S  (e.g. for RA)
c          "dms"         DD:MM:SS.S  (e.g. for DEC)
c          "arcsec"      Arcseconds relative to the reference pixel
c          "absdeg"      Absolute degrees
c          "reldeg"      Degrees relative to the reference pixel
c          "abspix"      Pixels
c          "relpix"      Pixels relative to the reference pixel
c          "absghz"      GHz
c          "relghz"      GHz relative to the reference pixel
c          "abskms"      km/s
c          "relkms"      km/s relative to the reference pixel
c          "abslin"      Linear coordinate
c          "rellin"      Linear coordinate relative to the reference
c                        pixel
c
c       The default is "abspix".
c@ stype
c       'radio', 'optical', or 'frequency'.  If you specify a spectral
c       axis coordinate, this indicates what convention it is in.  For
c       example, you might give an optical velocity with "type=abskms",
c       but the header indicates a frequency axis.  If unset, it is
c       assumed the coordinate is in the convention defined by the image
c       header.
c
c$Id: impos.for,v 1.5 2011/02/15 07:00:31 cal103 Exp $
c--
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'

      integer MAXTYP
      parameter (MAXTYP = 13)

      logical   doim, dospec, off
      integer   i, il, iostat, ipix(MAXNAX), lun, naxis, nco,
     *          nsize(MAXNAX), nstypes, ntypei, sax, strlen1(MAXNAX),
     *          strlen2(MAXNAX), strlen3(MAXNAX)
      real      data(MAXDIM), value
      double precision dtemp, rfreq, pixel(MAXNAX), win(MAXNAX)
      character bunit*9, ctypes(MAXNAX)*9, file*80, labtyp(MAXTYP)*6,
     *          sctypes(3)*4, str1*132, strout1(MAXNAX)*80,
     *          strout2(MAXNAX)*80, strout3(MAXNAX)*80, stypei*9,
     *          stypes(3)*9, text*132, trail*6, typei(MAXNAX)*6,
     *          typeo(MAXNAX)*6, typeo2(MAXNAX)*6, typeo3(MAXNAX)*6,
     *          version*80

      external  hdprsnt, versan
      logical   hdprsnt
      character versan*80

      data labtyp /'hms   ', 'dms   ', 'abspix', 'relpix',
     *             'arcsec', 'absghz', 'relghz', 'abskms',
     *             'relkms', 'abslin', 'rellin', 'absdeg',
     *             'reldeg'/
      data stypes /'frequency', 'radio', 'optical'/
      data typei /MAXNAX*' '/
      data nco, ipix /0, MAXNAX*1/
c-----------------------------------------------------------------------
      version = versan ('impos',
     *                  '$Revision: 1.5 $',
     *                  '$Date: 2011/02/15 07:00:31 $')

c     Get inputs.
      call keyini
      call keyf('in', file, ' ')
      if (file.eq.' ') call bug('f', 'Input file must be given')
      call keymatch('type', MAXTYP, labtyp, MAXNAX, typei, ntypei)

      do i = 1, MAXNAX
c       Set type defaults.
        if (typei(i).eq.' ') typei(i) = 'abspix'

c       Get coordinate.
        if (typei(i).eq.'hms' .or. typei(i).eq.'dms') then
          call keyt('coord', win(i), typei(i), -123456789d0)
          if (win(i).ne.-123456789d0) nco = nco + 1
        else
          call keyd('coord', win(i), -123456789d0)
          if (win(i).ne.-123456789d0) nco = nco + 1
        endif
      enddo

      if (nco.eq.0) call bug('f', 'You must give a coordinate')

c     Get spectral-axis type.
      call keymatch('stype', 3, stypes, 1, stypei, nstypes)
      call keyfin

c     Open file.
      call hopen(lun, file, 'old', iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening input')
        call bugno('f',iostat)
      endif

      doim = hdprsnt(lun,'image')
      call hclose(lun)
      if (doim) then
        call xyopen(lun, file, 'old', MAXNAX, nsize)
        call rdhda(lun, 'bunit', bunit, ' ')
      else
        call uvopen(lun, file, 'old')
        call uvnext(lun)
      endif

      call initco(lun)
      call cogetd(lun,'naxis',dtemp)
      naxis = nint(dtemp)
      call cogetd(lun,'restfreq',rfreq)

c     Initialize coordinate transformation routines and fish out CTYPES.
      do i = 1, naxis
        call ctypeco(lun, i, ctypes(i), il)
      enddo

c     Check spectral-axis type, set default value if needed and
c     convention order in which spectral axes will be listed.
      call sstdef(lun, nco, typei, stypei, sax)
      dospec = sax.ne.0 .and. nco.ge.sax
      if (sax.gt.0) then
        trail = ctypes(sax)(5:)
        sctypes(1) = ctypes(sax)(1:4)
        if (rfreq.gt.0d0) then
          if (sctypes(1).eq.'FREQ') then
            sctypes(2) = 'VELO'
            sctypes(3) = 'FELO'
            stypes(1) = 'frequency'
            stypes(2) = 'radio'
            stypes(3) = 'optical'
          else if (sctypes(1).eq.'VELO') then
            sctypes(2) = 'FELO'
            sctypes(3) = 'FREQ'
            stypes(1) = 'radio'
            stypes(2) = 'optical'
            stypes(3) = 'frequency'
          else if (sctypes(1).eq.'FELO') then
            sctypes(2) = 'VELO'
            sctypes(3) = 'FREQ'
            stypes(1) = 'optical'
            stypes(2) = 'radio'
            stypes(3) = 'frequency'
          endif
        else
          dospec = .false.
          stypes(1) = ' '
          stypes(2) = ' '
          stypes(3) = ' '
        endif
      endif

c     -----------------
c     World coordinate.
c     -----------------
      call setoaco(lun, 'abs', nco, 0, typeo)

c     Convert & format and inform.
      call w2wfco(lun, nco, typei, stypei, win, typeo, stypes(1),
     *             .false., strout1, strlen1)

      if (dospec) then
        call repspc(sax, stypes, nco, typeo, typeo2, typeo3)
        call w2wfco(lun, nco, typei, stypei, win, typeo2, stypes(2),
     *              .false., strout2, strlen2)
        call w2wfco(lun, nco, typei, stypei, win, typeo3, stypes(3),
     *              .false., strout3, strlen3)
      endif

      call output('World coordinates')
      do i = 1, nco
        call pader(typeo(i), strout1(i), strlen1(i))

        if (i.eq.sax) then
          write(text, 100) i, sctypes(1)//trail,
     *                     strout1(i)(1:strlen1(i))
          call output(text)

          if (dospec) then
            write(text, 100) i, sctypes(2)//trail,
     *                       strout2(i)(1:strlen2(i))
            call output(text)
            write(text, 100) i, sctypes(3)//trail,
     *                       strout3(i)(1:strlen3(i))
            call output(text)
          endif
        else
          write(text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
100       format('Axis ', i1, ': ',  a, ' = ', a)
          call output(text)
        endif
      enddo

c     ------------------------
c     Offset world coordinate.
c     ------------------------
      call setoaco(lun, 'off', nco, 0, typeo)
      call w2wfco(lun, nco, typei, stypei, win, typeo, stypes(1),
     *            .false., strout1, strlen1)

      if (dospec) then
        call repspc(sax, stypes, nco, typeo, typeo2, typeo3)
        call w2wfco(lun, nco, typei, stypei, win, typeo2, stypes(2),
     *              .false., strout2, strlen2)
        call w2wfco(lun, nco, typei, stypei, win, typeo3, stypes(3),
     *              .false., strout3, strlen3)
      endif

      call output(' ')
      call output('Offset world coordinates')
      do i = 1, nco
        if (i.eq.sax) then
          write(text, 100) i, sctypes(1)//trail,
     *                     strout1(i)(1:strlen1(i))
          call output(text)

          if (dospec) then
            write(text, 100) i, sctypes(2)//trail,
     *                       strout2(i)(1:strlen2(i))
            call output(text)
            write(text, 100) i, sctypes(3)//trail,
     *                       strout3(i)(1:strlen3(i))
            call output(text)
          endif
        else
          write(text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
          call output(text)
        endif
      enddo

c     ----------------
c     Absolute pixels.
c     ----------------
      if (doim) then
        do i = 1, nco
          typeo(i) = 'abspix'
        enddo
        call w2wco(lun, nco, typei, stypei, win, typeo, ' ', pixel)
        call w2wfco(lun, nco, typei, stypei, win, typeo, stypes(1),
     *              .true., strout1, strlen1)

        call output(' ')
        call output('Absolute pixels')
        do i = 1, nco
          write(text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
          call output(text)
        enddo
      endif

c     --------------
c     Offset pixels.
c     --------------
      if (doim) then
        do i = 1, nco
          typeo(i) = 'relpix'
        enddo
        call w2wfco(lun, nco, typei, stypei, win, typeo, stypes(1),
     *              .true., strout1, strlen1)

        call output(' ')
        call output('Offset pixels')
        do i = 1, nco
          write(text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
          call output(text)
        enddo
      endif

c     Find nearest pixel to coordinate location.
      if (doim) then
        if (nsize(1).le.MAXDIM) then
          off = .false.
          do i = 1, nco
            ipix(i) = nint(pixel(i))
            if (ipix(i).lt.1 .or. ipix(i).gt.nsize(i)) off = .true.
          enddo

c         Find value if on image.
          if (.not.off) then
            call xysetpl(lun, MAXNAX-2, ipix(3))
            call xyread(lun, ipix(2), data)
            value = data(ipix(1))

            call output(' ')
            call mitoaf(ipix, nco, str1, il)
            write(text, 200) str1(1:il), value, bunit
200         format('Nearest pixel = ',a,'.  Value = ',1pe13.6,' ',a)
            call output(text)
          endif
        else
          call output(' ')
          write(text, 210) nsize(1), MAXDIM
210       format('Image size',i6,' exceeds MAXDIM,',i6,
     *           ', skipping pixel value.')
          call output(text)
        endif
      endif

c     All done
      if (doim) then
        call xyclose(lun)
      else
        call uvclose(lun)
      endif

      call finco(lun)

      end

c***********************************************************************

      subroutine pader (type, str, ilen)

      character str*(*), type*(*)
      integer   ilen
c-----------------------------------------------------------------------
      character str2*132
      integer len1, it
c-----------------------------------------------------------------------
      if (type.eq.'hms' .or. type.eq.'dms') then
        str2 = str
        it  = index(str2,':')
        str = ' '
        str(3-it+2:) = str2(1:len1(str2))
        ilen = len1(str)
      endif

      end

c***********************************************************************

      subroutine sstdef (lun, n, typei, stypei, sax)

      integer   lun, n, sax
      character typei(n)*(*), stypei*(*)
c-----------------------------------------------------------------------
c  Check consistency of spectral-axis type and set a default if needed.
c
c  Input
c    typei     User specified coordinate types ('hms' etc)
c  Output
c    stypei    Will be ' ' if the user has not given a coordinate for
c              the spectral axis.  Else 'radio', 'optical'
c              or 'frequency'
c    sax       Spectral axis number of image
c-----------------------------------------------------------------------
      integer   i
      character ltype*3, dstype*9, line*80
c-----------------------------------------------------------------------
c     First set a default spectral axis type based upon the header.
      dstype = ' '
      sax = 0
      i = 1
      do while (i.le.n .and. dstype.eq.' ')
c       See if this axis is spectral.
        call specco(lun, i, dstype)
        if (dstype.ne.' ') sax = i
        i = i + 1
      enddo

c     Has user given a spectral coordinate for a non-spectral axis?
      do i = 1, n
        if (typei(i)(4:6).eq.'ghz' .or. typei(i)(4:6).eq.'kms') then
          if (i.ne.sax) call bug('f',
     *      'Spectral coordinate given for non-spectral axis')
        endif
      enddo

c     Now, if the image has a spectral axis, continue on to see what
c     the user is offering for that axis.
      if (sax.ne.0 .and. sax.le.n) then
c       Fish out latter half of user given coordinate type and check
c       given spectral-axis type, or set one if not given.
        ltype = typei(sax)(4:6)

        if (ltype.eq.'kms') then
c         User says coordinate in km/s.
          if (stypei.eq.' ') then
            if (dstype.ne.'frequency') then
c             Default to whatever type of velocity definition we have.
              stypei = dstype
            else
c             Frequency axis, can't work out what type of velocity user
c             wants.
              call bug('f', 'You must give keyword "stype" as '//
     *                 'the axis is frequency')
            endif
          else
c           Inconsistent, must give radio or optical.
            if (stypei.eq.'frequency') then
              line = 'Coord. type "'//typei(sax)//'" & spectral type "'
     *                //stypei//'" do not match'
              call bug('f', line)
            endif
          endif

        else if (ltype.eq.'ghz') then
c         User says coordinate in GHz.
          if (stypei.eq.' ') then
c           Give them frequency.
            stypei = 'frequency'
          else
c           Inconsistent, must be frequency.
            if (stypei.ne.'frequency') then
              line = 'Coordinate type '//typei(sax)//
     *               ' & spectral type '//stypei//' do not match'
              call bug('f', line)
            endif
          endif

        else
c         Make spectral axis type that indicated by header.
          stypei = dstype
        endif
      else
        stypei = ' '
      endif

      end

c***********************************************************************

      subroutine repspc (sax, stypes, n, typeo, typeo2, typeo3)

      integer   n, sax
      character stypes(3)*(*), typeo(n)*(*), typeo2(n)*(*),
     *          typeo3(n)*(*)
c-----------------------------------------------------------------------
c  See if any of the axes is a spectral axis.  If so, then we want to
c  list the spectral axis in frequency, radio and optical velocities.
c-----------------------------------------------------------------------
      integer i
      character*3 lstype(3)
c-----------------------------------------------------------------------
      do i = 1, n
        typeo2(i) = typeo(i)
        typeo3(i) = typeo(i)
      enddo

      if (sax.gt.0) then
        do i = 1, 3
          if (stypes(i).eq.'optical' .or. stypes(i).eq.'radio') then
            lstype(i) = 'kms'
          else
            lstype(i) = 'ghz'
          endif
        enddo

        typeo2(sax)(4:6) = lstype(2)
        typeo3(sax)(4:6) = lstype(3)
      endif

      end
