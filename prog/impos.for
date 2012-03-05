      program impos
c
c= IMPOS - Converts image coordinates between different systems.
c& nebk
c: image analysis
c+
c       IMPOS takes a coordinate in a specified system (such as "abspix"
c       or "arcsec") and converts it to all appropriate coordinate
c       systems (absolute world, offset world, pixels, offset pixels).
c
c       If a rest frequency is stored in the header, then in addition to
c       the type specified in the header, spectral axes are converted
c       to frequency (FREQ), radio velocity (VRAD), optical velocity
c       (VOPT), redshift (ZOPT), and relativistic velocity (VELO).
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
c       The default for unspecified axes is the type of the previous
c       axis.  The default for the first axis is "relpix".
c@ stype
c       'VRAD' (or 'radio'), 'VOPT' (or 'optical'), 'VELO' (or
c       'relativistic') - the velocity convention for a spectral
c       coordinate.  For example, the header might define a frequency
c       axis but you could provide a velocity with "type=abskms", in
c       which case you must qualify the velocity convention via stype,
c       i.e. VRAD, VOPT, or VELO.
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
c         altspc    Report FREQ-{HEL,LSR} axes as is, namely
c                   topocentric, i.e. don't Doppler shift to barycentric
c                   or LSRK (see help for velsw).
c
c$Id: impos.for,v 1.14 2012/03/05 05:08:45 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'

      integer    NOPTS, MAXTYP
      parameter (NOPTS = 2, MAXTYP = 13)

      logical   altPrj, altSpc, doim, dospec, off, present(NOPTS)
      integer   iax, ielem, il, iostat, ipix(MAXNAX), ispc, lIn, j,
     *          naxis, nelem, nsize(MAXNAX), nstypes, ntypei,
     *          strlen(MAXNAX)
      real      map(MAXDIM), value
      double precision rfreq, pixcrd(MAXNAX), win(MAXNAX)
      character algo*8, axtype*16, bunit*9, ctypes(MAXNAX)*8, file*80,
     *          labtyp(MAXTYP)*6, opts(NOPTS)*8, sctypes(5)*16,
     *          specsys*16, str1*132, strout(MAXNAX)*80, stypei*16,
     *          stypes(6)*16, text*132, typei(MAXNAX)*6,
     *          typeo(MAXNAX)*6, typep(MAXNAX)*6, units*8, version*72

      external  hdprsnt, itoaf, len1, versan
      logical   hdprsnt
      integer   len1
      character itoaf*2, versan*72

      data opts   /'altprj', 'altspc'/
      data labtyp /'hms   ', 'dms   ', 'abspix', 'relpix',
     *             'arcsec', 'absghz', 'relghz', 'abskms',
     *             'relkms', 'abslin', 'rellin', 'absdeg',
     *             'reldeg'/
      data stypes /'VRAD', 'radio',
     *             'VOPT', 'optical',
     *             'VELO', 'relativistic'/
      data typei /MAXNAX*' '/
      data nelem, ipix /0, MAXNAX*1/
c-----------------------------------------------------------------------
      version = versan ('impos',
     *                  '$Revision: 1.14 $',
     *                  '$Date: 2012/03/05 05:08:45 $')

c     Get inputs.
      call keyini
      call keyf('in', file, ' ')
      if (file.eq.' ') call bug('f', 'Input file must be given')
      call keymatch('type', MAXTYP, labtyp, MAXNAX, typei, ntypei)

      if (typei(1).eq.' ') typei(1) = 'relpix'
      do iax = 1, MAXNAX
c       Set type defaults.
        if (typei(iax).eq.' ') typei(iax) = typei(iax-1)

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
      call keymatch('stype', 6, stypes, 1, stypei, nstypes)

c     Get options.
      call options('options', opts, present, NOPTS)
      altPrj = present(1)
      altSpc = present(2)
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

c     Initialize coordinate transformation routines.
      call coInit(lIn)
      if (altPrj) call coAltPrj(lIn)

c     Check and set required spectral-axis type.
      call sstdef(lIn, nelem, typei, stypei, ispc)
      if (ispc.ne.0 .and. altSpc) then
        call coGetA(lIn, 'ctype'//itoaf(ispc), ctypes(ispc))
        if (ctypes(ispc).eq.'FREQ-HEL' .or.
     *      ctypes(ispc).eq.'FREQ-LSR') then
c         Change ctype and specsys to match crval and cdelt.
          call coSetA(lIn, 'ctype'//itoaf(ispc), 'FREQ')
          call coSetA(lIn, 'specsys', 'TOPOCENT')
          call coSetD(lIn, 'vobs', 0d0)
        endif
      endif
      call coSpcSet(lIn, stypei, ' ', ispc, algo)

c     Convert what we have been given into pixel coordinates.
      do ielem = 1, nelem
        typep(ielem) = 'abspix'
      enddo
      call w2wco(lIn, nelem, typei, win, typep, pixcrd)

c     Fish out ctypes.
      call coGetI(lIn, 'naxis', naxis)
      do iax = 1, naxis
        call coGetA(lIn, 'ctype'//itoaf(iax), ctypes(iax))
        if (ctypes(iax).eq.' ') ctypes(iax) = 'Axis '//itoaf(iax)
      enddo

c     Set order in which spectral axes will be listed.
      dospec = ispc.ne.0 .and. nelem.ge.ispc
      if (dospec) then
        call coAxType(lIn, ispc, axtype, sctypes(1), units)

        call coGetD(lIn, 'restfreq', rfreq)
        if (rfreq.gt.0d0) then
          if (sctypes(1).eq.'FREQ') then
            sctypes(2) = 'VRAD'
            sctypes(3) = 'VOPT'
            sctypes(4) = 'ZOPT'
            sctypes(5) = 'VELO'
          else if (sctypes(1).eq.'VRAD') then
            sctypes(2) = 'VOPT'
            sctypes(3) = 'ZOPT'
            sctypes(4) = 'VELO'
            sctypes(5) = 'FREQ'
          else if (sctypes(1).eq.'VOPT') then
            sctypes(2) = 'ZOPT'
            sctypes(3) = 'VRAD'
            sctypes(4) = 'VELO'
            sctypes(5) = 'FREQ'
          else
            dospec = .false.
          endif
        else
          dospec = .false.
        endif
      endif

c     Get Doppler frame and reformat for reporting purposes.
      call coGetA(lIn, 'specsys', specsys)
      if (specsys.eq.'TOPOCENT') then
        specsys = ' (topocentric)'
      else if (specsys.eq.'BARYCENT') then
        specsys = ' (barycentric)'
      else if (specsys.eq.'LSRK') then
        specsys = ' (LSRK)'
      endif


c     World coordinate.
      if (dospec) then
        call coSpcSet(lIn, sctypes(1), ' ', ispc, algo)
        if (algo.ne.' ') sctypes(1)(5:) = '-'//algo
      endif
      call setoaco(lIn, 'abs', nelem, 0, typeo)
      call w2wfco(lIn, nelem, typep, pixcrd, typeo, .false., strout,
     *            strlen)

      call output('World coordinates')
      do ielem = 1, nelem
        call pader(strout(ielem), il)
        write(text, 10) ielem, ctypes(ielem), strout(ielem)(:il)
        if (ielem.eq.ispc .and. specsys.ne.' ') then
          il = len1(text) + 1
          text(il:) = specsys
        endif
        call output(text)

        if (dospec .and. ielem.eq.ispc) then
          do j = 2, 5
            call coSpcSet(lIn, sctypes(j), ' ', ispc, algo)
            if (algo.ne.' ') sctypes(j)(5:) = '-'//algo
            call setoaco(lIn, 'abs', nelem, 0, typeo)
            call w2wfco(lIn, nelem, typep, pixcrd, typeo, .false.,
     *                  strout, strlen)

            call pader(strout(ielem), il)
            write(text, 10) ielem, sctypes(j), strout(ielem)(:il)
            call output(text)
          enddo
        endif
      enddo
 10   format('Axis',i2,': ',a8,' = ',a)


c     Offset world coordinate.
      if (dospec) then
        call coSpcSet(lIn, sctypes(1)(:4), ' ', ispc, algo)
      endif
      call setoaco(lIn, 'off', nelem, 0, typeo)
      call w2wfco(lIn, nelem, typep, pixcrd, typeo, .false., strout,
     *            strlen)

      call output(' ')
      call output('Offset world coordinates')
      do ielem = 1, nelem
        call pader(strout(ielem), il)
        write(text, 10) ielem, ctypes(ielem), strout(ielem)(:il)
        if (ielem.eq.ispc .and. specsys.ne.' ') then
          il = len1(text) + 1
          text(il:) = specsys
        endif
        call output(text)

        if (dospec .and. ielem.eq.ispc) then
          do j = 2, 5
            call coSpcSet(lIn, sctypes(j)(:4), ' ', ispc, algo)
            call setoaco(lIn, 'off', nelem, 0, typeo)
            call w2wfco(lIn, nelem, typep, pixcrd, typeo, .false.,
     *                  strout, strlen)

            call pader(strout(ielem), il)
            write(text, 10) ielem, sctypes(j), strout(ielem)(:il)
            call output(text)
          enddo
        endif
      enddo


c     Absolute pixels.
      if (doim) then
        call w2wfco(lIn, nelem, typep, pixcrd, typep, .true., strout,
     *              strlen)

        call output(' ')
        call output('Absolute pixels')
        do ielem = 1, nelem
          il = strlen(ielem)
          write(text, 10) ielem, ctypes(ielem), strout(ielem)(:il)
          call output(text)
        enddo
      endif


c     Offset pixels.
      if (doim) then
        do ielem = 1, nelem
          typeo(ielem) = 'relpix'
        enddo
        call w2wfco(lIn, nelem, typep, pixcrd, typeo, .true., strout,
     *              strlen)

        call output(' ')
        call output('Offset pixels')
        do ielem = 1, nelem
          il = strlen(ielem)
          write(text, 10) ielem, ctypes(ielem), strout(ielem)(:il)
          call output(text)
        enddo
      endif

c     Find nearest pixel to coordinate location.
      if (doim) then
        if (nsize(1).le.MAXDIM) then
          off = .false.
          do ielem = 1, nelem
            ipix(ielem) = nint(pixcrd(ielem))
            if (ipix(ielem).lt.1 .or.
     *          ipix(ielem).gt.nsize(ielem)) off = .true.
          enddo

c         Find value if on image.
          if (.not.off) then
            call xysetpl(lIn, MAXNAX-2, ipix(3))
            call xyread(lIn, ipix(2), map)
            value = map(ipix(1))

            call output(' ')
            call mitoaf(ipix, nelem, str1, il)
            write(text, 20) str1(1:il), value, bunit
 20         format('Nearest pixel = ',a,'.  Value = ',1pe13.6,' ',a)
            call output(text)
          endif
        else
          call output(' ')
          write(text, 30) nsize(1), MAXDIM
 30       format('Image size',i6,' exceeds MAXDIM,',i6,
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
c    stypei    stype specified by user, 'VRAD', 'VOPT', or 'VELO'.
c              If blank (user has not given a coordinate for the
c              spectral axis) will be set here.  Will be returned blank
c              if there is no spectral axis.
c  Output
c    ispc      Spectral axis number of image
c-----------------------------------------------------------------------
      integer   iax
      character axtype*16, line*80, ltype*3, units*8, wtype*16
c-----------------------------------------------------------------------
c     Look for the spectral axis.
      call coFindAx(lIn, 'spectral', ispc)
      if (ispc.eq.0) then
        stypei = ' '
        return
      endif

c     Was a spectral coordinate requested for a non-spectral axis?
      do iax = 1, NAXIS
        if (iax.ne.ispc) then
          ltype = typei(iax)(4:6)
          if (ltype.eq.'ghz' .or. ltype.eq.'kms') call bug('f',
     *      'Spectral coordinate given for non-spectral axis')
        endif
      enddo

c     Get spectral axis type from header.
      call coAxType(lIn, ispc, axtype, wtype, units)

c     Check user-specified spectral units and against requested type.
      ltype = typei(ispc)(4:6)
      if (ltype.eq.'kms') then
c       User requests coordinate in km/s.
        if (stypei.eq.' ') then
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
          stypei = 'FREQ'
        else
c         A velocity convention was specified.
          line = 'Coordinate type '//typei(ispc)//
     *           ' & stype '//stypei//' do not match'
          call bug('f', line)
        endif

      else
c       Set spectral type to header value.
        stypei = wtype
      endif

      end

c***********************************************************************

      subroutine pader (str, il)

      character str*(*)
      integer   il
c-----------------------------------------------------------------------
c  Insert leading blanks to format numbers nicely.
c-----------------------------------------------------------------------
      integer   k
      character str2*64

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      str2 = ' '

      k = index(str,':')
      if (k.gt.0) then
        str2(5-k:) = str(1:len1(str))
      else
        k = index(str,'.')
        str2(4-k:) = str(1:len1(str))
      endif

      str = str2
      il  = len1(str)

      end
