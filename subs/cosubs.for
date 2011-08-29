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
c
c   initco  : Initialize coordinate object
c   finco   : Free coordinate object
c
c   axfndco : Find axis of specified generic type
c   axtypco : Return generic axis type
c   chkaxco : Check axis CTYPE and axis label type for consistency
c   ctypeco : Get axis CTYPE
c   setoaco : Set default absolute or offset coordinate conversion
c             string depending upon the axis CTYPE
c   specco  : See if an axis is spectral and what type it is if so
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
c $Id: cosubs.for,v 1.10 2011/08/29 02:15:48 cal103 Exp $
c***********************************************************************

c* axfndCO -- Find a specified generic axis in an image
c& nebk
c: coordinates
c+
      subroutine axfndco (lun, type, n, iax, jax)

      integer n, iax, jax, lun
      character*(*) type
c  ---------------------------------------------------------------------
c  Find generic axis type in image.
c
c  Input
c    lun    Image handle
c    type   Generic axis type to find.  The first axis encountered
c           that has this type is returned.  The type should be one of:
c
c             RA   -> RA, LL, ELON, GLON
c             DEC  -> DEC, MM, ELAT, GLAT
c             LNG  -> ELON, GLON
c             LAT  -> ELAT, GLAT
c             VELO -> VELO, FELO
c             FREQ -> FREQ
c             UV   -> UU, VV
c             ANGL -> ANGLE
c             RAD  -> An axis whose increment should be in radians.
c                     These are RA, DEC, LNG, LAT, ANGL axes as
c                     described by the LHS above.
c           Other types are searched for exactly as specified
c    n      Number of axes to search starting from 1
c    iax    SPecific axis to match if N=0
c  Output
c    jax    Axis number that matches "type".  0 if not present
c-----------------------------------------------------------------------
      integer i, i1, i2, il
      character ltype*9, lctype*9
c-----------------------------------------------------------------------
      if (n.eq.0) then
        i1 = iax
        i2 = iax
      else
        i1 = 1
        i2 = n
      endif
      ltype = type
      call ucase(ltype)
      jax = 0

      do i = i1, i2
c
c Get CTYPE and find length prior to projection piece of string
c
        call ctypeco(lun, i, lctype, il)

        if (ltype.eq.'RA') then
          if (lctype(1:il).eq.'RA' .or.
     *        lctype(1:il).eq.'LL') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'DEC') then
          if (lctype(1:il).eq.'DEC' .or.
     *        lctype(1:il).eq.'MM') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'LNG') then
          if (lctype(1:il).eq.'ELON' .or.
     *        lctype(1:il).eq.'GLON') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'LAT') then
          if (lctype(1:il).eq.'ELAT' .or.
     *        lctype(1:il).eq.'GLAT') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'VELO') then
          if (lctype(1:il).eq.'VELO' .or.
     *        lctype(1:il).eq.'FELO') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'FREQ') then
          if (lctype(1:il).eq.'FREQ') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'ANGL') then
          if (lctype(1:il).eq.'ANGLE') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'UV') then
          if (lctype(1:il).eq.'UU' .or.
     *        lctype(1:il).eq.'VV') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else if (ltype.eq.'RAD') then
          if (lctype(1:il).eq.'RA'  .or.
     *        lctype(1:il).eq.'LL'  .or.
     *        lctype(1:il).eq.'DEC' .or.
     *        lctype(1:il).eq.'MM'  .or.
     *        lctype(1:il).eq.'ELON' .or.
     *        lctype(1:il).eq.'GLON' .or.
     *        lctype(1:il).eq.'ELAT' .or.
     *        lctype(1:il).eq.'GLAT' .or.
     *        lctype(1:il).eq.'ANGLE') then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        else
          if (index(lctype,ltype).ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return
          endif
        endif
      enddo

      end

c***********************************************************************

c* axtypCO -- Return generic axis type for specified axes
c& nebk
c: coordinates
c+
      subroutine axtypco (lun, n, iax, type)

      integer n, iax, lun
      character*4 type(*)
c  ---------------------------------------------------------------------
c  Return a generic axis type for each axis type.
c
c  Input
c    lun      Image handle
c    n        Number of axes to consider starting from 1
c    iax      Specific axis if N=0
c    ctype    Array of axis type descriptors
c  Output
c    type     Array of generic axis types describing each axis
c             The generic names returned are one of RA, DEC, LNG, LAT,
c             VELO, FREQ, UV, ANGL, NONE  where
c
c             RA   means CTYPE was one of RA, LL
c             DEC  means CTYPE was one of DEC, MM
c             LNG  means CTYPE was one of ELON, GLON
c             LAT  means CTYPE was one of ELAT, GLAT
c             VELO means CTYPE was one of VELO, FELO
c             FREQ means CTYPE was one of FREQ
c             UV   means CTYPE was one of UU, VV
c             ANGL means CTYPE was        ANGLE
c             NONE means CTYPE was not recognized
c-----------------------------------------------------------------------
      integer i, i1, i2, j, il
      character lctype*9
c-----------------------------------------------------------------------
      if (n.eq.0) then
        i1 = iax
        i2 = iax
      else
        i1 = 1
        i2 = n
      endif

      do i = i1, i2
        call ctypeco(lun, i, lctype, il)
        call ucase(lctype)

        j = 1
        if (n.ne.0) j = i
        if (lctype(1:il).eq.'RA' .or.
     *      lctype(1:il).eq.'LL') then
          type(j) = 'RA'
        else if
     *     (lctype(1:il).eq.'DEC' .or.
     *      lctype(1:il).eq.'MM') then
          type(j) = 'DEC'
        else if
     *     (lctype(1:il).eq.'ELON' .or.
     *      lctype(1:il).eq.'GLON') then
          type(j) = 'LNG'
        else if
     *     (lctype(1:il).eq.'ELAT' .or.
     *      lctype(1:il).eq.'GLAT') then
          type(j) = 'LAT'
        else if
     *     (lctype(1:4).eq.'VELO' .or.
     *      lctype(1:4).eq.'FELO') then
          type(j) = 'VELO'
        else if (lctype(1:4).eq.'FREQ') then
c
c Use "4" rather than "il" so "Velocity" is recognized
c
          type(j) = 'FREQ'
        else if (lctype(1:il).eq.'ANGLE') then
          type(j) = 'ANGL'
        else if (lctype(1:il).eq.'UU' .or.
     *           lctype(1:il).eq.'VV') then
          type(j) = 'UV'
        else
          type(j) = 'NONE'
        endif
      enddo

      end

c***********************************************************************

c* chkaxCO -- Check axis type and coordinate type for compatibility
c& nebk
c: coordinates
c+
      subroutine chkaxco (lun, ltype, iax, stype)

      integer iax, lun
      character*(*) ltype, stype
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
c    stype  Spectral axis descriptor.  If blank, then CTYPE must match
c           the TYPE (i.e. VELO/abskms is good, FELO/absghz is bad).
c           Otherwise, it is assumed that the spectral axis is going to
c           be switched to the desired STYPE so that any spectral CTYPE
c           is compatible with any spectral TYPE.
c-----------------------------------------------------------------------
      logical   bads, good
      integer   il, jax
      character ctype*32, gtype*4, str*132
c-----------------------------------------------------------------------
      if (stype.ne.' ' .and. stype.ne.'frequency' .and.
     *    stype.ne.'optical' .and. stype.ne.'radio') then
        str = 'CHKAXCO: invalid spectral axis type ('//stype//') given'
        call bug('f', str)
      endif

c     Get generic axis type.
      call axtypco(lun, 0, iax, gtype)

c     Compare generic axis type with label type.
      bads = .false.

      if (ltype.eq.'hms') then
        good = gtype.eq.'RA'  .or. gtype.eq.'LL'

      else if (ltype.eq.'dms') then
        good = gtype.eq.'DEC' .or. gtype.eq.'MM' .or.
     *         gtype.eq.'LNG' .or. gtype.eq.'LAT'

      else if (ltype.eq.'arcsec' .or. ltype.eq.'arcmin' .or.
     *         ltype.eq.'arcmas' .or.
     *         ltype.eq.'absdeg' .or. ltype.eq.'reldeg') then
        call axfndco(lun, 'RAD', 0, iax, jax)
        good = jax.ne.0

      else if (ltype.eq.'abskms' .or. ltype.eq.'relkms') then
        good = gtype.eq.'VELO' .or. gtype.eq.'FREQ'
        if (gtype.eq.'FREQ' .and. stype.eq.' ') bads = .true.

      else if (ltype.eq.'absghz' .or. ltype.eq.'relghz') then
        good = gtype.eq.'VELO' .or. gtype.eq.'FREQ'
        if (gtype.eq.'VELO' .and. stype.eq.' ') bads = .true.

      else
        good = .true.
        continue
      endif

c     Bug out if no good.
      if (.not.good .or. bads) then
        call ctypeco(lun, iax, ctype, il)
        call output('Axis ctype = '//ctype)
        str = 'Coordinate type = '//ltype
        call output(str)

        if (bads) then
          if (stype.eq.' ') then
            call output('Spectral axis convention unspecified')
          else
            str = 'Spectral axis convention = '//stype
            call output(str)
          endif
        endif

        call bug('f', 'CHKAXCO: These are inconsistent')
      endif

      end

c***********************************************************************

c* ctypeCO -- Return CTYPE for one axis
c& nebk
c: coordinates
c+
      subroutine ctypeco (lun, iax, ctype, il)

      integer iax, lun, il
      character*(*) ctype
c  ---------------------------------------------------------------------
c     Return CTYPE for one axis
c
c  Input
c    lun    Handle
c    iax    Axis to get CTYPE for
c  Output
c    ctype  CTYPE (upper case)
c    il     Length of string prior to project "--*" string
c-----------------------------------------------------------------------
      integer len1, il2
      character itoaf*2
      double precision crpix,crval,cdelt
c-----------------------------------------------------------------------
      call coAxGet(lun,iax,ctype,crpix,crval,cdelt)
      if (ctype.eq.' ') ctype = 'Axis '//itoaf(iax)

      il2 = len1(ctype)
      il = 1
      do while (ctype(il:il).ne.'-' .and. il.le.il2)
        il = il + 1
      enddo
      il = il - 1

      end

c***********************************************************************

c* finCO -- Finish up after coordinate conversion routines
c& nebk
c: coordinates
c+
      subroutine finco (lun)

      integer lun
c  ---------------------------------------------------------------------
c     Tidy up after coordinate conversion routines have been accessed
c
c  Input
c    lun    Handle of image
c-----------------------------------------------------------------------
      call coFin(lun)

      end

c***********************************************************************
c* initCO -- Initialize coordinate conversion routines
c& nebk
c: coordinates
c+
      subroutine initco (lun)

      integer lun
c  ---------------------------------------------------------------------
c     Initialize coordinate conversion routine
c
c  Input
c    lun    Handle of image
c-----------------------------------------------------------------------
      call coInit(lun)

      end

c***********************************************************************

      subroutine sctico (type, win, cti)

      character*(*) type, cti
      double precision win
c-----------------------------------------------------------------------
c     This subroutine takes an NEBK style coordinate type descriptor,
c     and generates an RJS type coordinate descriptor making the
c     appropriate conversion to radians for angular coordinates
c     where needed ready for cocvt
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
c
c Set coordinate conversion string and convert units to radian
c where needed
c
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
c     Convert an angular coordinate after it has been returned by
c     RJS' cocvt into the units appropriate to NEBK style coordinates.
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

c***********************************************************************

c* setoaCO -- Set default (abs or rel) coord type depending on CTYPE
c& nebk
c: coordinates
c+
      subroutine setoaco (lun, absoff, n, iax, types)

      integer n, lun, iax
      character*(*) types(n), absoff*3
c  ---------------------------------------------------------------------
c     Set a string dictating what units the coordinate will
c     be presented in.  This is set by looking at the CTYPE
c     for each axis.
c
c  Input
c    lun      Image handle
c    absoff   'abs' or 'off' for absolute of offset world coordinate
c    n        Number of axes
c    iax      SPecific axis if N=0
c  Output
c    types    Desired NEBK style coordinate types.  One of
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'absghz', 'relghz', 'abskms', 'relkms',
c               'absnat', 'relnat', 'none'
c-----------------------------------------------------------------------
      include 'maxnax.h'
      integer i, i1, i2, j
      character*4 gtype
c-----------------------------------------------------------------------
      if (n.eq.0) then
        i1 = iax
        i2 = iax
      else
        i1 = 1
        i2 = n
      endif

      do i = i1, i2
c
c Get generic axis type and set default
c
        call axtypco(lun, 0, i, gtype)

        j = 1
        if (n.ne.0) j = i
        if (gtype.eq.'RA') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'hms'
          endif
        else if (gtype.eq.'DEC') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'dms'
          endif
        else if (gtype.eq.'LNG' .or. gtype.eq.'LAT') then
          if (absoff.eq.'off') then
            types(j) = 'reldeg'
          else
            types(j) = 'absdeg'
          endif
        else if (gtype.eq.'VELO') then
          if (absoff.eq.'off') then
            types(j) = 'relkms'
          else
            types(j) = 'abskms'
          endif
        else if (gtype.eq.'FREQ') then
          if (absoff.eq.'off') then
            types(j) = 'relghz'
          else
            types(j) = 'absghz'
          endif
        else if (gtype.eq.'ANGL') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'absnat'
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

c* specCO -- See if this axis is spectral and what type it is
c& nebk
c: coordinates
c+
      subroutine specCo (lun, iax, stype)

      integer lun, iax
      character*(*) stype
c  ---------------------------------------------------------------------
c  Is this a spectral axis and if so what type is it?
c
c  Input
c    lun    Handle of image
c    iax    Axis number
c  Output:
c    stype  ' ' if not spectral, else 'radio', 'optical', 'frequency'
c-----------------------------------------------------------------------
      integer   il
      character ctype*9
c-----------------------------------------------------------------------
      call ctypeco(lun, iax, ctype, il)

      if (ctype(1:il).eq.'VELO') then
        stype = 'radio'
      else if (ctype(1:il).eq.'FELO') then
        stype = 'optical'
      else if (ctype(1:il).eq.'FREQ') then
        stype = 'frequency'
      else
        stype = ' '
      endif

      end

c***********************************************************************

      subroutine sunitco (lun, iax, type, units)

      integer lun, iax
      character*(*) type, units
c-----------------------------------------------------------------------
c  Set the units of a pixel based upon the requested type and the
c  axis type.  Used for ascii not graphical output so no PGPLOT escape
c  sequences.
c
c  Inputs:
c    lun    Image handle
c    iax    Axis of interest
c    type   User requested coordinate type
c  Output:
c    units  Axis units
c-----------------------------------------------------------------------
      integer jax
      character units2*10, str*132, gtype*4
c-----------------------------------------------------------------------
      if (type.eq.'hms' .or. type.eq.'dms' .or. type.eq.'none') then
        units = ' '
      else if (type.eq.'arcsec') then
        units = 'arcsec'
      else if (type.eq.'arcmin') then
        units = 'arcmin'
      else if (type.eq.'arcmas') then
        units = 'mas'
      else if (type.eq.'absdeg') then
        units = 'degrees'
      else if (type.eq.'reldeg') then
        units = 'offset degrees'
      else if (type.eq.'abspix') then
        units = 'pixels'
      else if (type.eq.'relpix') then
        units = 'offset pixels'
      else if (type.eq.'absghz') then
        units = 'GHz'
      else if (type.eq.'relghz') then
        units = 'offset GHz'
      else if (type.eq.'abskms') then
        units = 'km/s'
      else if (type.eq.'relkms') then
        units = 'offset km/s'
      else if (type.eq.'absnat' .or. type.eq.'relnat') then
        call axtypco(lun, 0, iax, gtype)
        if (gtype.eq.'VELO') then
          units2 = 'km/s'
        else if (gtype.eq.'FREQ') then
          units2 = 'GHz'
        else if (gtype.eq.'UV') then
          units = 'wavelengths'
        else
          call axfndco(lun, 'RAD', 0, iax, jax)
          if (jax.ne.0) then
            units2 = 'radians'
          else
            units2 = ' '
          endif
        endif

        if (type.eq.'absnat') then
          units = units2
        else if (type.eq.'relnat') then
          units = 'offset '//units2
        endif
      else
        str = 'SUNITCO: Unrecognized label type ('//type//')'
        call bug('f', str)
      endif

      end

c***********************************************************************

c* w2wCO -- Convert an array of coordinates
c& mrc
c: coordinates
c+
      subroutine w2wco (lun, n, typei, stypei, win, typeo, stypeo, wout)

      integer lun, n
      double precision win(n), wout(n)
      character*(*) typei(n), typeo(n), stypei, stypeo
c  ---------------------------------------------------------------------
c  For backwards-compatibility, call w2wcov to convert an NEBK-style
c  coordinate vector and go belly-up if the coordinate conversion fails.
c  Refer to the prologue of w2wcov for usage information.
c-----------------------------------------------------------------------
      logical valid
c-----------------------------------------------------------------------
      call w2wcov(lun, n, typei, stypei, win, typeo, stypeo, wout,
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
      subroutine w2wcov (lun, naxis, typei, stypei, win, typeo, stypeo,
     *  wout, valid)

      logical   valid
      integer   lun, naxis
      double precision win(naxis), wout(naxis)
      character typei(naxis)*(*), stypei*(*), typeo(naxis)*(*),
     *          stypeo*(*)
c  ---------------------------------------------------------------------
c  Convert an NEBK-style coordinate vector using the CO routines.
c
c  Input
c    lun     Handle of open file
c    naxis   Number of axes to convert
c    typei   Array of input coordinate types, Should be from list
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c    stypei  Indicates the convention for a spectral coordinate,
c            regardless of what the header initially defines: 'radio',
c            'optical', 'frequency'.  If ' ', the header prevails,
c            though a mismatch between TYPEI and CTYPE (e.g.
c            absghz/VELO-LSR) would then result in a fatal error.
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
c    stypeo  'radio', 'optical', 'frequency'.  If a spectral coordinate
c            is given, this indicates what convention it is to be
c            converted to, regardless of what the header defined.
c            If ' ', then it assumed the coordinate is in the convention
c            indicated by STYPEI.  If STYPEI is blank too, then
c            CTYPE of the header and TYPEO must match or a fatal
c            error will result
c  Output
c    wout    Array of converted output coordinates in same units
c            as described above
c-----------------------------------------------------------------------
      include 'maxnax.h'

      logical   done, nix(maxnax), none
      integer   iax, ifrq, ip
      double precision wloc(maxnax), xdum
      character algo*3, cti*21, cto*21, lstype*9, str*2
c-----------------------------------------------------------------------
c     There may be nothing to do for some axes.  Make sure we just copy
c     the coordinates in these cases, rather than converting to and from
c     pixels, thus losing precision.  Save initial spectral convention
c     while we are it.
      none = .true.
      do iax = 1, naxis
        nix(iax) = .false.

        if (typei(iax).eq.typeo(iax)) then
          if (lstype.ne.' ') then
            if (stypei.eq.stypeo .or. typei(iax)(4:6).eq.'pix')
     *        nix(iax) = .true.
          else
            nix(iax) = .true.
          endif
        endif

        if (.not.nix(iax)) none = .false.
      enddo

      if (none) then
        do iax = 1, naxis
          wout(iax) = win(iax)
        enddo

        valid = .true.
        return
      endif

c     Switch spectral axis to type of input coordinate.
      if (stypei.ne.' ') call coSpcSet(lun, stypei, ifrq, algo)

c     Convert coordinates to absolute pixels first; loop over axes.
      cti = '  '
      cto = '  '
      ip = 1
      do iax = 1, naxis
c       Check input coordinate type consistent with actual axis type.
        call chkaxco(lun, typei(iax), iax, stypei)

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
      do iax = 1, naxis
        if (typeo(iax).ne.'abspix') done = .false.
      enddo

      if (.not.done) then
c       Having turned the coordinate into a pixel, we can now convert
c       it to the desired output coordinate type.

c       Switch spectral axis to type of output coordinate.
        if (stypeo.ne.' ') call coSpcSet(lun, stypeo, ifrq, algo)

c       Loop over axes
        cti = '  '
        cto = '  '
        ip = 1
        do iax = 1, naxis
c         Check output coordinate type consistent with actual axis type.
          call chkaxco(lun, typeo(iax), iax, stypeo)

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
        do iax = 1, naxis
          call sctoco(typeo(iax), wout(iax))
        enddo
      endif

c     Overwrite any coordinates that did not really need converting by
c     the input values to improve precision.
      do iax = 1, naxis
        if (nix(iax)) wout(iax) = win(iax)
      enddo

c     Restore spectral axis type from header.
      call coSpcSet(lun, ' ', ifrq, algo)

      end

c***********************************************************************

c* w2wfCO -- Convert an array of coordinates and format
c& nebk
c: coordinates
c+
      subroutine w2wfco (lun, n, typei, stypei, win, typeo, stypeo,
     *                   nounit, strout, strlen)

      integer lun, n, strlen(n)
      double precision win(n)
      character*(*) typei(n), typeo(n), strout(n), stypei, stypeo
      logical nounit
c  ---------------------------------------------------------------------
c  Convert an array of NEBK style coordinates with the COCVT routines
c  and format the results with units into strings
c
c  Input
c    lun     Handle of open file
c    n       Number of axes
c    typei   Array of input coordinate types, Should be from list
c               'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz',
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c    stypei  'radio', 'optical', 'frequency'.  If a spectral coordinate
c            is given, this indicates what convention it is in,
c            regardless of what the header initially defines. If ' ',
c            then it assumed to be as the header defines unless
c            there is a mismatch between TYPEI and CTYPE for that axis
c            (e.g. absghz/VELO-LSR) wherupon a fatal error will result
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
c    stypeo  'radio', 'optical', 'frequency'.  If a spectral coordinate
c            is given, this indicates what convention it is to be
c            converted to, regardless of what the header defined.
c            If ' ', then it assumed the coordinate is in the convention
c            indicated by STYPEI.  If STYPEI is blank too, then
c            CTYPE of the header and TYPEO must match or a fatal
c            error witll result
c    nounit  Don't append units
c  Output
c    strout  Array of formatted converted output coordinates
c    strlen  Length of strings in STROUT
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer i, len1
      double precision wout(maxnax)
      character hangleh*30, rangle*30, str*132, units*30
c-----------------------------------------------------------------------
c
c Convert coordinates
c
      call w2wco(lun, n, typei, stypei, win, typeo, stypeo, wout)
c
c Format results
c
      do i = 1, n
        strout(i) = ' '
        if (typeo(i).eq.'abspix' .or. typeo(i).eq.'relpix' .or.
     *      typeo(i).eq.'none') then
          call strfd(wout(i), '(f9.2)', strout(i), strlen(i))
        else if (typeo(i).eq.'abskms' .or. typeo(i).eq.'relkms') then
          call strfd(wout(i), '(1pe12.5)', strout(i), strlen(i))
        else if (typeo(i).eq.'absghz' .or. typeo(i).eq.'relghz') then
          call strfd(wout(i), '(1pe15.8)', strout(i), strlen(i))
        else if (typeo(i).eq.'absdeg' .or. typeo(i).eq.'reldeg') then
          call strfd(wout(i), '(f8.3)', strout(i), strlen(i))
        else if (typeo(i).eq.'arcsec' .or. typeo(i).eq.'arcmin'
     *           .or. typeo(i).eq.'arcmas') then
          call strfd(wout(i), '(1pe15.8)', strout(i), strlen(i))
        else if (typeo(i).eq.'absnat' .or. typeo(i).eq.'relnat') then
          call strfd(wout(i), '(1pe15.8)', strout(i), strlen(i))
        else if (typeo(i).eq.'hms') then
          strout(i) = hangleh(wout(i))
          strlen(i) = len1(strout(i))
        else if (typeo(i).eq.'dms') then
          strout(i) = rangle(wout(i))
          strlen(i) = len1(strout(i))
        else
          str = 'W2WFCO: Unrecognized coordinate type ('//typeo(i)//')'
          call bug('f', str)
        endif
c
c Work out units
c
        if (.not.nounit) then
          call sunitco(lun, i, typeo(i), units)
c
c Add units to formatted number
c
          strout(i)(strlen(i)+2:) = units
          strlen(i) = len1(strout(i))
        endif
      enddo

      end

c***********************************************************************

c* w2wsCO -- Convert NEBK style coordinate for a single axis
c& nebk
c: coordinates
c+
      subroutine w2wsco (lun, iax, typei, stypei, win, typeo, stypeo,
     *                   wout)

      integer   lun, iax
      double precision win, wout
      character typei*(*), stypei*(*), typeo*(*), stypeo*(*)
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
c    stypei  'radio', 'optical', 'frequency'.  If a spectral coordinate
c            is given, this indicates what convention it is in,
c            regardless of what the header initially defines. If ' ',
c            then it assumed to be as the header defines unless
c            there is a mismatch between TYPEI and CTYPE for that axis
c            (e.g. absghz/VELO-LSR) wherupon a fatal error will result
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
c    stypeo  'radio', 'optical', 'frequency'.  If a spectral coordinate
c            is given, this indicates what convention it is to be
c            converted to, regardless of what the header defined.
c            If ' ', then it assumed the coordinate is in the convention
c            indicated by STYPEI.  If STYPEI is blank too, then the
c            CTYPE of the header and TYPEO must match or a fatal
c            error will result
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
      call w2wco(lun, naxis, ltypei, stypei, lwin, ltypeo, stypeo,
     *           lwout)

c     Fish out axis.
      wout = lwout(iax)

      end

c***********************************************************************

c* w2wfsCO -- Convert a coordinate for a single axis and format
c& nebk
c: coordinates
c+
      subroutine w2wsfco (lun, iax, typei, stypei, win, typeo, stypeo,
     *                    nounit, strout, strlen)

      integer   lun, iax, strlen
      double precision win
      character*(*) typei, typeo, strout, stypei, stypeo
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
c    stypei  'radio', 'optical', 'frequency'.  If a spectral coordinate
c            is given, this indicates what convention it is in,
c            regardless of what the header initially defines. If ' ',
c            then it assumed to be as the header defines.
c    win     Coordinate to be converted
c               'hms', 'dms' in radians
c               '*  deg'     in degrees
c               'arcsec'     in arcsec
c               'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in km/s
c    typeo   Output coordinate type from above list
c    stypeo  'radio', 'optical', 'frequency'.  If a spectral coordinate
c            is given, this indicates what convention it is to be
c            converted to, regardless of what the header defined.
c            If ' ', then it assumed the coordinate is in the convention
c            indicated by STYPEI
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
      call w2wfco(lun, iax, ltypei, stypei, lwin, ltypeo, stypeo,
     *            nounit, lstrout, lstrlen)
c
c     Return formatted string.
      strout = lstrout(iax)
      strlen = lstrlen(iax)

      end
