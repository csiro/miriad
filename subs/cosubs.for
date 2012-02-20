c***********************************************************************
c  These subroutines provide an interface between NEBK style coordinate
c  handling (the 'hms', 'dms', 'arcsec', 'arcmin', 'reldeg', 'abspix',
c  'relpix', 'absghz', 'relghz', 'abskms', 'relkms', 'absnat',
c  'relnat', 'none') and RJS' new coordinate routines (co.for).
c
c  Code that is known to call these routines directly -
c    subs: cgsubs.for, cgpgsubs.for
c    prog: cgcurs.for, cgdisp.for, cgslice.for, cgspec.for, gpcomb.for,
c          gpdof.for, impos.for, maxfit.for, sfind.for
c
c  User callable routines are:
c   chkaxco : Check axis CTYPE and axis label type for consistency
c   setoaco : Set default absolute or offset coordinate conversion
c             string depending upon the axis CTYPE
c   sunitco : Set the units of a pixel based upon the requested type and
c             the axis type.
c   w2wco   : Convert coordinates between different world types
c   w2wcov  : As for w2wco but with status return rather than fatal
c             error.
c   w2wfco  : As for w2wco but results formatted in a string
c   w2wsco  : As for w2wco but just one axis, with the rest assumed
c             to be at the reference pixel
c   w2wsfco : As for w2wsco but results formatted in a string
c
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c $Id: cosubs.for,v 1.14 2012/02/20 07:30:59 cal103 Exp $
c***********************************************************************

c* chkaxCO -- Check axis type and coordinate type for compatibility
c& nebk
c: coordinates
c+
      subroutine chkaxco (lun, ltype, iax)

      integer iax, lun
      character*(*) ltype
c  ---------------------------------------------------------------------
c  Check axis type and desired coordinate type are compatible.
c
c  Input
c    lun    Image handle.
c    ltype  Coordinate type user has asked for; one of
c               'hms',    'dms',
c               'arcsec', 'arcmin', 'arcmas',
c               'absdeg', 'reldeg',
c               'absghz', 'relghz',
c               'abskms', 'relkms',
c               'absnat', 'relnat',
c               'abspix', 'relpix',
c               'none'
c    iax    Axis of interest.
c-----------------------------------------------------------------------
      logical   bads, good
      character algo*3, axtype*9, ctype*32, str*132, units*6, wtype*9

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
c     Parse the axis type.
      call coAxType(lun, iax, axtype, wtype, algo, units)

c     Compare generic axis type with label type.
      bads = .false.

      if (ltype.eq.'hms') then
        good = wtype.eq.'RA'  .or. wtype.eq.'LL'

      else if (ltype.eq.'dms') then
        good = units.eq.'rad' .and. wtype.ne.'RA' .and. wtype.ne.'LL'

      else if (ltype.eq.'absdeg' .or. ltype.eq.'reldeg' .or.
     *         ltype.eq.'arcmin' .or. ltype.eq.'arcsec' .or.
     *         ltype.eq.'arcmas') then
        good = units.eq.'rad'

      else if (ltype.eq.'abskms' .or. ltype.eq.'relkms') then
        good = axtype.eq.'spectral'
        if (units.ne.'km/s') bads = .true.

      else if (ltype.eq.'absghz' .or. ltype.eq.'relghz') then
        good = axtype.eq.'spectral'
        if (units.ne.'GHz') bads = .true.

      else
        good = .true.
        continue
      endif

c     Bug out if no good.
      if (.not.good .or. bads) then
        call coGetA(lun, 'ctype'//itoaf(iax), ctype)
        if (ctype.eq.' ') ctype = 'Axis '//itoaf(iax)
        call output('Axis ctype = '//ctype)
        str = 'Coordinate type = '//ltype
        call output(str)

        if (bads) then
          call output('Spectral axis convention unspecified')
        endif

        call bug('f', 'CHKAXCO: These are inconsistent')
      endif

      end

c***********************************************************************

c* setoaCO -- Set default (abs or rel) coord type depending on CTYPE
c& nebk
c: coordinates
c+
      subroutine setoaco (lun, absoff, NAXIS, iax, types)

      integer   lun
      character absoff*3
      integer   NAXIS, iax
      character types(NAXIS)*(*)
c  ---------------------------------------------------------------------
c  Set a string dictating what units the coordinate will be presented
c  in.  This is set by looking at the CTYPE for each axis.
c
c  Input
c    lun      Image handle
c    absoff   'abs' or 'off' for absolute of offset world coordinate
c    n        Number of axes
c    iax      Specific axis if N=0
c  Output
c    types    Desired NEBK style coordinate types.  One of
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'absghz', 'relghz', 'abskms', 'relkms',
c               'absnat', 'relnat', 'none'
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   j, jax, jax1, jax2
      character algo*3, axtype*9, units*6, wtype*9
c-----------------------------------------------------------------------
      if (NAXIS.eq.0) then
        jax1 = iax
        jax2 = iax
      else
        jax1 = 1
        jax2 = NAXIS
      endif

      do jax = jax1, jax2
c       Get generic axis type and set default.
        call coAxType(lun, jax, axtype, wtype, algo, units)

        j = 1
        if (NAXIS.ne.0) j = jax

        if (wtype.eq.'RA') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'hms'
          endif
        else if (wtype.eq.'DEC') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'dms'
          endif
        else if (wtype.eq.'ANGL') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'absnat'
          endif
        else if (units.eq.'rad') then
          if (absoff.eq.'off') then
            types(j) = 'reldeg'
          else
            types(j) = 'absdeg'
          endif
        else if (units.eq.'km/s') then
          if (absoff.eq.'off') then
            types(j) = 'relkms'
          else
            types(j) = 'abskms'
          endif
        else if (units.eq.'GHz') then
          if (absoff.eq.'off') then
            types(j) = 'relghz'
          else
            types(j) = 'absghz'
          endif
        else
          if (absoff.eq.'off') then
            types(j) = 'relnat'
          else
            types(j) = 'absnat'
          endif
        endif
      enddo

      end

c***********************************************************************

      subroutine sunitco (lun, iax, utype, units)

      integer   lun, iax
      character utype*(*), units*(*)
c-----------------------------------------------------------------------
c  Set the units of a pixel based upon the requested type and the axis
c  type.  Used for ascii not graphical output so no PGPLOT escape
c  sequences.
c
c  Inputs:
c    lun    Image handle
c    iax    Axis of interest
c    utype  User requested coordinate type
c  Output:
c    units  Axis units
c-----------------------------------------------------------------------
      character algo*3, axtype*9, str*132, wtype*9
c-----------------------------------------------------------------------
      if (utype.eq.'hms' .or. utype.eq.'dms' .or. utype.eq.'none') then
        units = ' '
      else if (utype.eq.'arcsec') then
        units = 'arcsec'
      else if (utype.eq.'arcmin') then
        units = 'arcmin'
      else if (utype.eq.'arcmas') then
        units = 'mas'
      else if (utype.eq.'absdeg') then
        units = 'degrees'
      else if (utype.eq.'reldeg') then
        units = 'offset degrees'
      else if (utype.eq.'abspix') then
        units = 'pixels'
      else if (utype.eq.'relpix') then
        units = 'offset pixels'
      else if (utype.eq.'absghz') then
        units = 'GHz'
      else if (utype.eq.'relghz') then
        units = 'offset GHz'
      else if (utype.eq.'abskms') then
        units = 'km/s'
      else if (utype.eq.'relkms') then
        units = 'offset km/s'
      else if (utype.eq.'absnat' .or. utype.eq.'relnat') then
        call coAxType(lun, iax, axtype, wtype, algo, units)
        if (units.eq.'lambda') then
          units = 'wavelengths'
        else if (units.eq.'rad') then
          units = 'radians'
        endif

        if (utype.eq.'relnat') then
          units = 'offset '//units
        endif
      else
        str = 'SUNITCO: Unrecognized label type ('//utype//')'
        call bug('f', str)
      endif

      end

c***********************************************************************

c* w2wCO -- Convert an array of coordinates
c& mrc
c: coordinates
c+
      subroutine w2wco (lun, n, typei, win, typeo, wout)

      integer lun, n
      double precision win(n), wout(n)
      character*(*) typei(n), typeo(n)
c  ---------------------------------------------------------------------
c  For backwards-compatibility, call w2wcov to convert an NEBK-style
c  coordinate vector and go belly-up if the coordinate conversion fails.
c  Refer to the prologue of w2wcov for usage information.
c-----------------------------------------------------------------------
      logical valid
c-----------------------------------------------------------------------
      call w2wcov(lun, n, typei, win, typeo, wout,
     *  valid)
      if (.not.valid) then
        call bug('f', 'Invalid coordinate conversion in coCvtv')
      endif

      end

c***********************************************************************

c* w2wCOv -- Convert an array of coordinates, with validation.
c& nebk
c: coordinates
c+
      subroutine w2wcov (lun, NAXIS, typei, win, typeo, wout, valid)

      logical   valid
      integer   lun, NAXIS
      double precision win(NAXIS), wout(NAXIS)
      character typei(NAXIS)*(*), typeo(NAXIS)*(*)
c  ---------------------------------------------------------------------
c  Convert an NEBK-style coordinate vector using the CO routines.
c
c  Input
c    lun     Handle of open file
c    NAXIS   Number of axes to convert
c    typei   Array of input coordinate types, Should be from list
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c    win     Array of coordinates to be converted
c               'hms', 'dms' in radians
c               '*  deg'     in degrees
c               'arcsec'     in arcsec
c               'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in km/s
c               '*  nat'     in natural axis coordinates
c    typeo   Array of output coordinate types from above list
c            requested.
c  Output
c    wout    Array of converted output coordinates in same units
c            as described above.
c    valid   True if the coordinate conversion succeeded.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      logical   copy(MAXNAX), done, gotone
      integer   iax, ip
      double precision wloc(MAXNAX), xdum
      character cti*21, cto*21, str*2
c-----------------------------------------------------------------------
c     There may be nothing to do for some axes.  Make sure we just copy
c     the coordinates in these cases rather than converting to and from
c     pixels, thus losing precision.
      gotone = .false.
      do iax = 1, NAXIS
        copy(iax) = typei(iax).eq.typeo(iax)
        if (.not.copy(iax)) gotone = .true.
      enddo

      if (.not.gotone) then
        do iax = 1, NAXIS
          wout(iax) = win(iax)
        enddo

        valid = .true.
        return
      endif

c     Convert coordinates to absolute pixels first; loop over axes.
      cti = '  '
      cto = '  '
      ip = 1
      do iax = 1, NAXIS
c       Check input coordinate type consistent with actual axis type.
        call chkaxco(lun, typei(iax), iax)

c       Set coordinate transformation strings and convert angular
c       units if required to radians.
        wloc(iax) = win(iax)
        call sctico(typei(iax), wloc(iax),  str)
        cti(ip:ip+2) = str//'/'
        cto(ip:ip+2) = 'ap/'
        ip = ip + 3
      enddo

c     Convert to pixels (pixels being converted to pixels here will be
c     done with no loss of precision so don't bother with extra code to
c     trap it.
      call coCvtV(lun, cti, wloc, cto, wout, valid)
      if (.not.valid) return

c     Check that we need to go on.  The user may want absolute pixels
c     whereupon we are done.  Note that absolute pixels are the same
c     regardless of the spectral convention!
      done = .true.
      do iax = 1, NAXIS
        if (typeo(iax).ne.'abspix') done = .false.
      enddo

      if (.not.done) then
c       Having turned the coordinate into a pixel, we can now convert
c       it to the desired output coordinate type.

c       Loop over axes
        cti = '  '
        cto = '  '
        ip = 1
        do iax = 1, NAXIS
c         Check output coordinate type consistent with actual axis type.
          call chkaxco(lun, typeo(iax), iax)

c         Set coordinate transformation strings.
          wloc(iax) = wout(iax)
          call sctico(typeo(iax), xdum, str)
          cti(ip:ip+2) = 'ap/'
          cto(ip:ip+2) = str//'/'
          ip = ip + 3
        enddo

c       Now convert the absolute pixels to the desired coordinate type.
        call coCvt(lun, cti, wloc, cto, wout)

c       Convert coordinates given in radians to the appropriate output
c       units (degrees, arcsec etc).
        do iax = 1, NAXIS
          call sctoco(typeo(iax), wout(iax))
        enddo
      endif

c     Overwrite any coordinates that did not really need converting by
c     the input values to improve precision.
      do iax = 1, naxis
        if (copy(iax)) wout(iax) = win(iax)
      enddo

      end

c***********************************************************************

c* w2wfCO -- Convert an array of coordinates and format
c& nebk
c: coordinates
c+
      subroutine w2wfco (lun, NAXIS, typei, win, typeo, nounit, strout,
     *  strlen)

      integer   lun, NAXIS
      character typei(NAXIS)*(*)
      double precision win(NAXIS)
      character typeo(NAXIS)*(*)
      logical   nounit
      character strout(NAXIS)*(*)
      integer   strlen(NAXIS)
c  ---------------------------------------------------------------------
c  Convert an array of NEBK style coordinates with the COCVT routines
c  and format the results with units into strings
c
c  Input
c    lun     Handle of open file.
c    NAXIS   Number of axes.
c    typei   Array of input coordinate types, Should be from list
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c    win     Array of coordinates to be converted
c               'hms', 'dms' in radians
c               '*  deg'     in degrees
c               'arcsec'     in arcsec
c               'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in km/s
c               '*  nat'     in natural axis coordinates
c    typeo   Array of output coordinate types from above list
c            requested.
c    nounit  Don't append units.
c  Output
c    strout  Array of formatted converted output coordinates.
c    strlen  Length of strings in strout.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   iax, il
      double precision wout(maxnax)
      character hangleh*30, rangle*30, str*132, units*30

      external  len1
      integer   len1
c-----------------------------------------------------------------------
c     Convert coordinates.
      call w2wco(lun, NAXIS, typei, win, typeo, wout)

c     Format results.
      do iax = 1, NAXIS
        strout(iax) = ' '
        if (     typeo(iax).eq.'abspix' .or.
     *           typeo(iax).eq.'relpix' .or.
     *           typeo(iax).eq.'none') then
          call strfd(wout(iax), '(f9.2)', strout(iax), il)

        else if (typeo(iax).eq.'abskms' .or.
     *           typeo(iax).eq.'relkms') then
          call strfd(wout(iax), '(1pe12.5)', strout(iax), il)

        else if (typeo(iax).eq.'absghz' .or.
     *           typeo(iax).eq.'relghz') then
          call strfd(wout(iax), '(1pe15.8)', strout(iax), il)

        else if (typeo(iax).eq.'absdeg' .or.
     *           typeo(iax).eq.'reldeg') then
          call strfd(wout(iax), '(f8.3)', strout(iax), il)

        else if (typeo(iax).eq.'arcsec' .or.
     *           typeo(iax).eq.'arcmin' .or.
     *           typeo(iax).eq.'arcmas') then
          call strfd(wout(iax), '(1pe15.8)', strout(iax), il)

        else if (typeo(iax).eq.'absnat' .or.
     *           typeo(iax).eq.'relnat') then
          call strfd(wout(iax), '(1pe15.8)', strout(iax), il)

        else if (typeo(iax).eq.'hms') then
          strout(iax) = hangleh(wout(iax))

        else if (typeo(iax).eq.'dms') then
          strout(iax) = rangle(wout(iax))

        else
          str='W2WFCO: Unrecognized coordinate type ('//typeo(iax)//')'
          call bug('f', str)
        endif

c       Work out units.
        if (nounit) then
          strlen(iax) = il
        else
          call sunitco(lun, iax, typeo(iax), units)

c         Add units to formatted number.
          strout(iax)(il+2:) = units
          strlen(iax) = len1(strout(iax))
        endif
      enddo

      end

c***********************************************************************

c* w2wsCO -- Convert NEBK style coordinate for a single axis
c& nebk
c: coordinates
c+
      subroutine w2wsco (lun, iax, typei, win, typeo, wout)

      integer   lun, iax
      double precision win, wout
      character typei*(*), typeo*(*)
c  ---------------------------------------------------------------------
c  Convert one NEBK style coordinate with the COCVT routines.
c  Coordinates for the other axes are assumed to be at the
c  reference pixel
c
c  Input
c    lun     Handle of open file
c    iax     Axis
c    typei   Input coordinate type, Should be from list
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c    win     Coordinate to be converted
c               'hms', 'dms' in radians
c               '*  deg'     in degrees
c               'arcsec'     in arcsec
c               'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in km/s
c               '*  nat'     in natural axis coordinates
c    typeo   Output coordinate type from above list
c  Output
c    wout    Converted output coordinate
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   i, naxis
      double precision lwin(MAXNAX), lwout(MAXNAX)
      character ltypei(MAXNAX)*6, ltypeo(MAXNAX)*6
c-----------------------------------------------------------------------
c     Load reference pixel for dummy locations.
      call coGetI(lun, 'naxis', naxis)
      if (iax.le.0 .or. iax.gt.naxis)
     *  call bug('f', 'W2WSCO: invalid axis number')
      do i = 1, naxis
        ltypei(i) = 'relpix'
        lwin(i) = 0d0
        ltypeo(i) = 'relpix'
      enddo

c     Load axis of interest.
      ltypei(iax) = typei
      lwin(iax) = win
      ltypeo(iax) = typeo

c     Convert.
      call w2wco(lun, naxis, ltypei, lwin, ltypeo, lwout)

c     Fish out axis.
      wout = lwout(iax)

      end

c***********************************************************************

c* w2wfsCO -- Convert a coordinate for a single axis and format
c& nebk
c: coordinates
c+
      subroutine w2wsfco (lun, iax, typei, win, typeo, nounit, strout,
     *                    strlen)

      integer   lun, iax, strlen
      double precision win
      character*(*) typei, typeo, strout
      logical   nounit
c  ---------------------------------------------------------------------
c  Convert one NEBK style coordinate with the COCVT routines and format
c  into a string.  Coordinates for the other axes are assumed to be at
c  the reference pixel
c
c  Input
c    lun     Handle of open file
c    iax     Axis of interest
c    typei   Coordinate type, should be from list
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c    win     Coordinate to be converted
c               'hms', 'dms' in radians
c               '*  deg'     in degrees
c               'arcsec'     in arcsec
c               'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in km/s
c    typeo   Output coordinate type from above list
c    nounit  Don't append units
c  Output
c    strout  Formatted converted output coordinate
c    strlen  Length of string in STROUT
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   i, lstrlen(MAXNAX), naxis
      double precision lwin(MAXNAX)
      character lstrout(MAXNAX)*50, ltypei(MAXNAX)*6, ltypeo(MAXNAX)*6
c-----------------------------------------------------------------------
c     Load dummy array values and actual value into conversion arrays.
      call coGetI(lun, 'naxis', naxis)
      if (iax.le.0 .or. iax.gt.naxis)
     *  call bug('f', 'W2WSFCO: invalid axis number')

      do i = 1, naxis
        lwin(i) = 0.0
        ltypei(i) = 'relpix'
        ltypeo(i) = 'relpix'
      enddo

      lwin(iax) = win
      ltypei(iax) = typei
      ltypeo(iax) = typeo

c     Convert and format.
      call w2wfco(lun, iax, ltypei, lwin, ltypeo, nounit, lstrout,
     *            lstrlen)

c     Return formatted string.
      strout = lstrout(iax)
      strlen = lstrlen(iax)

      end

c***********************************************************************

      subroutine sctico (type, win, cti)

      character*(*) type, cti
      double precision win
c-----------------------------------------------------------------------
c  This subroutine takes an NEBK style coordinate type descriptor, and
c  generates an RJS type coordinate descriptor making the appropriate
c  conversion to radians for angular coordinates where needed ready for
c  cocvt
c
c  Input
c    type   NEBK style type of world coordinate; one of
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', none'
c  Input/output
c    win    Coordinate value.  Any coordinate of an angular type (hms,
c           dms, *deg, arcsec, arcmin) will be in radians on output
c  Output
c    cti    RJS style coordinate type ('ap', 'op', 'aw', 'ow')
c-----------------------------------------------------------------------
      include 'mirconst.h'
      character*132 str
c-----------------------------------------------------------------------
c     Set coordinate conversion string and convert units to radians
c     where needed.
      cti = ' '

      if (type.eq.'hms' .or. type.eq.'dms') then
        cti = 'aw'
      else if (type.eq.'abspix' .or. type.eq.'none') then
        cti = 'ap'
      else if (type.eq.'relpix') then
        cti = 'op'
      else if (type.eq.'arcsec') then
        cti = 'ow'
        win = win * DAS2R
      else if (type.eq.'arcmin') then
        cti = 'ow'
        win = win * DAS2R * 60d0
      else if (type.eq.'arcmas') then
        cti = 'ow'
        win = win * AS2R * 1d-3
      else if (type.eq.'absghz' .or. type.eq.'abskms' .or.
     *         type.eq.'absnat') then
        cti = 'aw'
      else if (type.eq.'relghz' .or. type.eq.'relkms' .or.
     *         type.eq.'relnat') then
        cti = 'ow'
      else if (type.eq.'absdeg') then
        cti = 'aw'
        win = win * DD2R
      else if (type.eq.'reldeg') then
        cti = 'ow'
        win = win * DD2R
      else
        str = 'SCTICO: Unrecognized axis type ('//type//')'
        call bug('f', str)
      endif

      end

c***********************************************************************

      subroutine sctoco (type, wout)

      character*(*) type
      double precision wout
c-----------------------------------------------------------------------
c  Convert an angular coordinate after it has been returned by RJS'
c  cocvt into the units appropriate to NEBK style coordinates.
c
c  Input
c    type   NEBK style type of world coordinate that we want
c           to convert to;
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c  Input/output
c    wout   Coordinate value.  On input, all angular coordiantes are
c           in radians, on exit, 'arcsec' in arcsec, '*deg" in degrees
c           'arcmin' in arcmin
c-----------------------------------------------------------------------
      include 'mirconst.h'
c-----------------------------------------------------------------------
      if (type.eq.'arcsec') then
        wout = wout * DR2AS
      else if (type.eq.'arcmin') then
        wout = wout * DR2D * 60D0
      else if (type.eq.'arcmas') then
        wout = wout * DR2AS * 1d3
      else if (type.eq.'absdeg' .or. type.eq.'reldeg') then
        wout = wout * DR2D
      endif

      end
