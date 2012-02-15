      program impos
c
c= IMPOS - Converts image coordinates between different systems.
c& nebk
c: image analysis
c+
c       IMPOS takes a coordinate in a specified system (such as "abspix"
c       or "arcsec") and converts it to all appropriate coordinate
c       systems (absolute world, offset world, pixels, offset pixels).
c       Spectral axes are converted to values in frequency, radio and
c       optical velocities.
c
c       If the input is an image and the specified coordinate represents
c       a valid pixel, its value is reported as well.
c
c@ in
c       The input image or visibility dataset.  For a visibility
c       dataset, the coordinate system is relative to the first
c       visibility record.
c@ coord
c       Specify the coordinate for each axis that you are interested
c       in; you don't necessarily need one for every axis in the image.
c       No default.
c@ type
c       Specify the coordinate system of the input coordinate for each
c       axis.  Different axes can be in different systems.  Choose from:
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
c       'FREQ' (or 'frequency'), 'VOPT' (or 'optical'), 'VRAD' (or
c       'radio') - the velocity convention for a spectral coordinate.
c       For example, the header might indicate a frequency axis but you
c       could request an optical velocity with "type=abskms".  If unset,
c       the velocity convention must be defined by the image header,
c       i.e. either VOPT or VRAD.
c@ options
c       Extra processing options.  Several can be given, separated by
c       commas, with minimum-match.
c         altprj    Interpret a CAR (plate carée) projection in the
c                   input ot template image as a simple linear
c                   coordinate system with an additional 1/cos(lat0)
c                   scaling factor applied when computing the longitude,
c                   e.g.
c                      RA = (p1 - CRPIX1)*CDELT1/cos(CRVAL2).
c                   This interpretation differs significantly from the
c                   FITS standard when lat0 (i.e. CRVAL2) is non-zero.
c
c$Id: impos.for,v 1.9 2012/02/15 06:08:06 cal103 Exp $
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

      logical   altPrj, doim, dospec, off
      integer   iax, ielem, il, iostat, ipix(MAXNAX), ispc, lIn, naxis,
     *          nelem, nsize(MAXNAX), nstypes, ntypei, strlen1(MAXNAX),
     *          strlen2(MAXNAX), strlen3(MAXNAX)
      real      data(MAXDIM), value
      double precision rfreq, pixel(MAXNAX), win(MAXNAX)
      character bunit*9, ctypes(MAXNAX)*8, file*80, labtyp(MAXTYP)*6,
     *          sctypes(3)*8, str1*132, strout1(MAXNAX)*80,
     *          strout2(MAXNAX)*80, strout3(MAXNAX)*80, stypei*9,
     *          stypes(8)*12, text*132, typei(MAXNAX)*6,
     *          typeo(MAXNAX)*6, typeo2(MAXNAX)*6, typeo3(MAXNAX)*6,
     *          version*80

      external  hdprsnt, itoaf, versan
      logical   hdprsnt
      character itoaf*2, versan*80

      data labtyp /'hms   ', 'dms   ', 'abspix', 'relpix',
     *             'arcsec', 'absghz', 'relghz', 'abskms',
     *             'relkms', 'abslin', 'rellin', 'absdeg',
     *             'reldeg'/
      data stypes /'FREQ', 'frequency',
     *             'VOPT', 'optical',
     *             'VRAD', 'radio',
     *             'VELO', 'relativistic'/
      data typei /MAXNAX*' '/
      data nelem, ipix /0, MAXNAX*1/
c-----------------------------------------------------------------------
      version = versan ('impos',
     *                  '$Revision: 1.9 $',
     *                  '$Date: 2012/02/15 06:08:06 $')

c     Get inputs.
      call keyini
      call keyf('in', file, ' ')
      if (file.eq.' ') call bug('f', 'Input file must be given')
      call keymatch('type', MAXTYP, labtyp, MAXNAX, typei, ntypei)

      do iax = 1, MAXNAX
c       Set type defaults.
        if (typei(iax).eq.' ') typei(iax) = 'abspix'

c       Get coordinate elements.
        if (typei(iax).eq.'hms' .or. typei(iax).eq.'dms') then
          call keyt('coord', win(iax), typei(iax), -123456789d0)
          if (win(iax).ne.-123456789d0) nelem = nelem + 1
        else
          call keyd('coord', win(iax), -123456789d0)
          if (win(iax).ne.-123456789d0) nelem = nelem + 1
        endif
      enddo

      if (nelem.eq.0) call bug('f', 'You must give a coordinate')

c     Get spectral-axis type.
      call keymatch('stype', 8, stypes, 1, stypei, nstypes)

c     Get options.
      call options('options', 'altprj', altPrj, 1)
      call keyfin

c     Open file.
      call hopen(lIn, file, 'old', iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening input')
        call bugno('f',iostat)
      endif

      doim = hdprsnt(lIn,'image')
      call hclose(lIn)
      if (doim) then
        call xyopen(lIn, file, 'old', MAXNAX, nsize)
        call rdhda(lIn, 'bunit', bunit, ' ')
      else
        call uvopen(lIn, file, 'old')
        call uvnext(lIn)
      endif

      call coInit(lIn)
      if (altPrj) call coAltPrj(lIn)
      call coGetI(lIn, 'naxis', naxis)
      call coGetD(lIn, 'restfreq', rfreq)

c     Initialize coordinate transformation routines and fish out CTYPES.
      do iax = 1, naxis
        call coGetA(lIn, 'ctype'//itoaf(iax), ctypes(iax))
        if (ctypes(iax).eq.' ') ctypes(iax) = 'Axis '//itoaf(iax)
      enddo

c     Check spectral-axis type, set default value if needed and
c     convention order in which spectral axes will be listed.
      call sstdef(lIn, nelem, typei, stypei, ispc)
      dospec = ispc.ne.0 .and. nelem.ge.ispc
      if (ispc.gt.0) then
        sctypes(1) = ctypes(ispc)
        if (sctypes(1)(:4).eq.'VELO') sctypes(1) = 'VRAD'
        if (sctypes(1)(:4).eq.'FELO') sctypes(1) = 'VOPT-F2W'

        if (rfreq.gt.0d0) then
          if (sctypes(1).eq.'FREQ') then
            sctypes(2) = 'VRAD'
            sctypes(3) = 'VOPT-F2W'
            stypes(1) = 'FREQ'
            stypes(2) = 'VRAD'
            stypes(3) = 'VOPT'
          else if (sctypes(1).eq.'VRAD') then
            sctypes(2) = 'VOPT-F2W'
            sctypes(3) = 'FREQ'
            stypes(1) = 'VRAD'
            stypes(2) = 'VOPT'
            stypes(3) = 'FREQ'
          else if (sctypes(1).eq.'VOPT-F2W') then
            sctypes(2) = 'VRAD'
            sctypes(3) = 'FREQ'
            stypes(1) = 'VOPT'
            stypes(2) = 'VRAD'
            stypes(3) = 'FREQ'
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
      call setoaco(lIn, 'abs', nelem, 0, typeo)

c     Convert & format and inform.
      call w2wfco(lIn, nelem, typei, stypei, win, typeo, stypes(1),
     *            .false., strout1, strlen1)

      if (dospec) then
        call repspc(ispc, stypes, nelem, typeo, typeo2, typeo3)
        call w2wfco(lIn, nelem, typei, stypei, win, typeo2, stypes(2),
     *              .false., strout2, strlen2)
        call w2wfco(lIn, nelem, typei, stypei, win, typeo3, stypes(3),
     *              .false., strout3, strlen3)
      endif

      call output('World coordinates')
      do ielem = 1, nelem
        il = strlen1(ielem)
        call pader(typeo(ielem), strout1(ielem), il)

        if (ielem.eq.ispc) then
          write(text, 100) ielem, sctypes(1), strout1(ielem)(:il)
          call output(text)

          if (dospec) then
            il = strlen2(ielem)
            write(text, 100) ielem, sctypes(2), strout2(ielem)(:il)
            call output(text)
            il = strlen3(ielem)
            write(text, 100) ielem, sctypes(3), strout3(ielem)(:il)
            call output(text)
          endif
        else
          write(text, 100) ielem, ctypes(ielem), strout1(ielem)(:il)
100       format('Axis',i2,': ',a8,' = ',a)
          call output(text)
        endif
      enddo

c     ------------------------
c     Offset world coordinate.
c     ------------------------
      call setoaco(lIn, 'off', nelem, 0, typeo)
      call w2wfco(lIn, nelem, typei, stypei, win, typeo, stypes(1),
     *            .false., strout1, strlen1)

      if (dospec) then
        call repspc(ispc, stypes, nelem, typeo, typeo2, typeo3)
        call w2wfco(lIn, nelem, typei, stypei, win, typeo2, stypes(2),
     *              .false., strout2, strlen2)
        call w2wfco(lIn, nelem, typei, stypei, win, typeo3, stypes(3),
     *              .false., strout3, strlen3)
      endif

      call output(' ')
      call output('Offset world coordinates')
      do ielem = 1, nelem
        if (ielem.eq.ispc) then
          il = strlen1(ielem)
          write(text, 100) ielem, sctypes(1), strout1(ielem)(:il)
          call output(text)

          if (dospec) then
            il = strlen2(ielem)
            write(text, 100) ielem, sctypes(2), strout2(ielem)(:il)
            call output(text)
            il = strlen3(ielem)
            write(text, 100) ielem, sctypes(3), strout3(ielem)(:il)
            call output(text)
          endif
        else
          write(text, 100) ielem, ctypes(ielem), strout1(ielem)(:il)
          call output(text)
        endif
      enddo

c     ----------------
c     Absolute pixels.
c     ----------------
      if (doim) then
        do ielem = 1, nelem
          typeo(ielem) = 'abspix'
        enddo
        call w2wco(lIn, nelem, typei, stypei, win, typeo, ' ', pixel)
        call w2wfco(lIn, nelem, typei, stypei, win, typeo, stypes(1),
     *              .true., strout1, strlen1)

        call output(' ')
        call output('Absolute pixels')
        do ielem = 1, nelem
          il = strlen1(ielem)
          write(text, 100) ielem, ctypes(ielem), strout1(ielem)(:il)
          call output(text)
        enddo
      endif

c     --------------
c     Offset pixels.
c     --------------
      if (doim) then
        do ielem = 1, nelem
          typeo(ielem) = 'relpix'
        enddo
        call w2wfco(lIn, nelem, typei, stypei, win, typeo, stypes(1),
     *              .true., strout1, strlen1)

        call output(' ')
        call output('Offset pixels')
        do ielem = 1, nelem
          il = strlen1(ielem)
          write(text, 100) ielem, ctypes(ielem), strout1(ielem)(:il)
          call output(text)
        enddo
      endif

c     Find nearest pixel to coordinate location.
      if (doim) then
        if (nsize(1).le.MAXDIM) then
          off = .false.
          do ielem = 1, nelem
            ipix(ielem) = nint(pixel(ielem))
            if (ipix(ielem).lt.1 .or.
     *          ipix(ielem).gt.nsize(ielem)) off = .true.
          enddo

c         Find value if on image.
          if (.not.off) then
            call xysetpl(lIn, MAXNAX-2, ipix(3))
            call xyread(lIn, ipix(2), data)
            value = data(ipix(1))

            call output(' ')
            call mitoaf(ipix, nelem, str1, il)
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
        call xyclose(lIn)
      else
        call uvclose(lIn)
      endif

      call coFin(lIn)

      end

c***********************************************************************

      subroutine pader (type, str, ilen)

      character type*(*), str*(*)
      integer   ilen
c-----------------------------------------------------------------------
      integer   it
      character str2*132

      external  len1
      integer   len1
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

      subroutine sstdef (lIn, NAXIS, typei, stypei, ispc)

      integer   lIn, NAXIS
      character typei(NAXIS)*(*), stypei*(*)
      integer   ispc
c-----------------------------------------------------------------------
c  Check consistency of spectral-axis type and set a default if needed.
c
c  Input
c    lIn       Handle of input image.
c    NAXIS     Number of image axes.
c    typei     User specified coordinate types ('hms' etc)
c  In/out:
c    stypei    stype specified by user, 'FREQ', 'VOPT', or 'VRAD'.
c              If blank (user has not given a coordinate for the
c              spectral axis) will be set here.  Will be returned blank
c              if there is no spectral axis.
c  Output
c    ispc      Spectral axis number of image
c-----------------------------------------------------------------------
      integer   iax
      character algo*16, axtype*9, line*80, ltype*3, units*6, wtype*16
c-----------------------------------------------------------------------
c     Look for the spectral axis.
      call coFindAx(lIn, 'spectral', ispc)
      if (ispc.eq.0) then
        stypei = ' '
        return
      endif

c     Get spectral axis type from header.
      call coAxType(lIn, ispc, axtype, wtype, algo, units)

c     Was a spectral coordinate requested for a non-spectral axis?
      do iax = 1, NAXIS
        if (iax.ne.ispc) then
          ltype = typei(iax)(4:6)
          if (ltype.eq.'ghz' .or. ltype.eq.'kms') call bug('f',
     *      'Spectral coordinate given for non-spectral axis')
        endif
      enddo

c     Check user-specified spectral units and against requested type.
      ltype = typei(ispc)(4:6)
      if (ltype.eq.'kms') then
c       User requests coordinate in km/s.
        if (stypei.eq.'FREQ') then
c         Inconsistent, must give VOPT, VRAD, or VELO.
          line = 'Coord. type "'//typei(ispc)//'" & spectral type "'
     *            //stypei//'" do not match'
          call bug('f', line)

        else if (stypei.eq.' ') then
          if (wtype.eq.'FREQ') then
c           Can't determine velocity convention for frequency axes.
            call bug('f', 'You must give keyword "stype" as '//
     *               'the axis is frequency')
          else
c           Set spectral type to header value.
            stypei = wtype
          endif
        endif

      else if (ltype.eq.'ghz') then
c       User requests coordinate in GHz.
        if (stypei.eq.' ') then
c         Give them frequency.
          stypei = 'FREQ'
        else if (stypei.ne.'FREQ') then
c         Inconsistent, must be frequency.
          line = 'Coordinate type '//typei(ispc)//
     *           ' & spectral type '//stypei//' do not match'
          call bug('f', line)
        endif

      else
c       Set spectral type to header value.
        stypei = wtype
      endif

      end

c***********************************************************************

      subroutine repspc (ispc, stypes, NAXIS, typeo, typeo2, typeo3)

      integer   NAXIS, ispc
      character stypes(3)*(*), typeo(NAXIS)*(*), typeo2(NAXIS)*(*),
     *          typeo3(NAXIS)*(*)
c-----------------------------------------------------------------------
c  List a spectral axis in frequency, and radio and optical velocity.
c-----------------------------------------------------------------------
      integer   iax
      character lstype(3)*3
c-----------------------------------------------------------------------
      do iax = 1, NAXIS
        typeo2(iax) = typeo(iax)
        typeo3(iax) = typeo(iax)
      enddo

      if (ispc.gt.0) then
        do iax = 1, 3
          if (stypes(iax).eq.'VOPT' .or. stypes(iax).eq.'VRAD') then
            lstype(iax) = 'kms'
          else
            lstype(iax) = 'ghz'
          endif
        enddo

        typeo2(ispc)(4:6) = lstype(2)
        typeo3(ispc)(4:6) = lstype(3)
      endif

      end
