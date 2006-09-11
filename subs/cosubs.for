c***********************************************************************
c  These subroutines provide an interface between NEBK style coordinate
c  handling (the 'hms', 'dms', 'arcsec', 'arcmin', 'reldeg', 'abspix', 
c  'relpix', 'absghz', 'relghz', 'abskms', 'relkms', 'absnat', 
c  'relnat', 'none') and RJS' new coordinate routines (co.for). 
c
c  Code that is known to use these routines is: cgdisp.for,
c  cgcurs.for, cgslice.for, cgspec.for, maxfit.for, impos.for,
c  regrid.for, cgsubs.for, cgpgsubs.for
c
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
c               string depending upon the axis CTYPE
c   specco  : See if an axis is spectral and what type it is if so
c
c   w2wco   : Convert coordinates between different world types
c   w2wfco  : As for w2wco but results formatted in a string
c   w2wsco  : As for w2wco but just one axis, with the rest assumed
c               to be at the reference pixel
c   w2wsfco : As for w2wsco but results formatted in a string
c
c
c  History:
c    nebk   18aug94    Initial version
c    nebk   29dec94    Remove concatenated char*(*) in subroutine calls
c    nebk   11aug95    Add arcmin labels
c    nebk   14nov95    Remove LINCO; its use has been eradicated, add
c                      some new ones which have migrated from CGSUB.FOR
c                      These are AXFNDCO, AXTYPCO, SUNITCO
c    nebk   29nov95    Eliminate indexed searching of axis types so that
c                      e.g. if CTYPE=RADIUS it is not treated as RA.  
c                      Consolidate CTYPE searching into AXFNDCO and AXTYPCO
c    nebk   03dec95    Declaration of TYPE in AXTYPCO was (n) not (*), and
c	               removed useless access to multiple axes in CHKAXCO
c    nebk   18dec95    Recognize new CTYPE "angle"
c    rjs    06feb96    Increase ctype string in chkaxco.
c***********************************************************************
c
c* axfndCO -- Find a specified generic axis in an image
c& nebk
c: coordinates
c+
      subroutine axfndco (lun, type, n, iax, jax)
c
      implicit none
      integer n, iax, jax, lun
      character*(*) type
c
c  Find generic axis type in image.
c
c  Input
c    lun    Image handle
c    type   Generic axis type to find.  The first axis encountered
c           that has this type is returned.  The type should be one of:
c
c             RA   ->  RA, LL, ELON, GLON
c             DEC  ->  DEC, MM, ELAT, GLAT
c             LONG ->  ELON, GLON
c             LATI ->  ELAT, GLAT
c             VELO ->  VELO, FELO
c             FREQ ->  FREQ
c             UV   ->  UU, VV
c	      ANGL ->  ANGLE
c             RAD  ->  An axis whose increment should be in
c                      radians.  These are RA, DEC, LAT, LONG, 
c		       ANGL axes as described by the LHS above.
c           Other types are searched for exactly as specified
c    n      Number of axes to search starting from 1
c    iax    SPecific axis to match if N=0
c  Output
c    jax    Axis number that matches "type".  0 if not present
c--
c-----------------------------------------------------------------
      integer i, i1, i2, il
      character ltype*9, lctype*9
c-----------------------------------------------------------------
      if (n.eq.0) then
        i1 = iax
        i2 = iax
      else
        i1 = 1
        i2 = n
      end if
      ltype = type
      call ucase (ltype)
      jax = 0
c
      do i = i1, i2
c
c Get CTYPE and find length prior to projection piece of string
c
        call ctypeco (lun, i, lctype, il)
c
        if (ltype.eq.'RA') then
          if (lctype(1:il).eq.'RA' .or.   
     +        lctype(1:il).eq.'LL') then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'DEC') then
          if (lctype(1:il).eq.'DEC' .or.
     +        lctype(1:il).eq.'MM') then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'LONG') then
          if (lctype(1:il).eq.'ELON' .or. 
     +        lctype(1:il).eq.'GLON') then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'LATI') then
          if (lctype(1:il).eq.'ELAT' .or.
     +        lctype(1:il).eq.'GLAT') then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'VELO') then
          if (lctype(1:il).eq.'VELO' .or. 
     +        lctype(1:il).eq.'FELO') then
            jax = 1
            if (n.ne.0) jax = i
            return 
          end if
        else if (ltype.eq.'FREQ') then
          if (lctype(1:il).eq.'FREQ') then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if  
        else if (ltype.eq.'ANGL') then
          if (lctype(1:il).eq.'ANGLE') then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if  
        else if (ltype.eq.'UV') then
          if (lctype(1:il).eq.'UU' .or.
     +        lctype(1:il).eq.'VV') then
            jax = 1
            if (n.ne.0) jax = i
            return 
          end if  
        else if (ltype.eq.'RAD') then
          if (lctype(1:il).eq.'RA'  .or.
     +        lctype(1:il).eq.'LL'  .or.
     +        lctype(1:il).eq.'DEC' .or.
     +        lctype(1:il).eq.'MM'  .or.
     +        lctype(1:il).eq.'ELON'.or.
     +        lctype(1:il).eq.'GLON'.or.
     +        lctype(1:il).eq.'ELAT'.or.
     +        lctype(1:il).eq.'GLAT'.or.
     +        lctype(1:il).eq.'ANGLE') then
            jax = 1
            if (n.ne.0) jax = i
            return 
          end if  
        else
          if (index(lctype,ltype).ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if   
        end if
      end do
c
      end
c
c
c* axtypCO -- Return generic axis type for specified axes
c& nebk
c: coordinates
c+
      subroutine axtypco (lun, n, iax, type)
c
      implicit none
      integer n, iax, lun
      character*4 type(*)
c
c  Return a generic axis type for each axis type.
c
c  Input
c    lun      Image handle
c    n        Number of axes to consider starting from 1
c    iax      Specific axis if N=0
c    ctype    Array of axis type descriptors
c  Output
c    type     Array of generic axis types describing each axis
c             The generic names returned are one of
c              RA, DEC, LATI, LONG, VELO, FREQ, UV, ANGL, NONE  where
c
c             RA   means CTYPE was one of   RA, LL
c             DEC  means CTYPE was one of   DEC, MM
c             LONG means CTYPE was one of   ELON, GLON
c             LATI means CTYPE was one of   ELAT, GLAT
c             VELO means CTYPE was one of   VELO, FELO
c             FREQ means CTYPE was one of   FREQ
c             UV   means CTYPE was one of   UU, VV
c	      ANGL means CTYPE was          ANGLE
c             NONE means CTYPE was not recognized
c
c--
c-----------------------------------------------------------------
      integer i, i1, i2, j, il
      character lctype*9
c-----------------------------------------------------------------
      if (n.eq.0) then
        i1 = iax
        i2 = iax
      else
        i1 = 1
        i2 = n
      end if
c
      do i = i1, i2
        call ctypeco (lun, i, lctype, il)
c
        j = 1
        if (n.ne.0) j = i
        if (lctype(1:il).eq.'RA' .or.
     +      lctype(1:il).eq.'LL') then
          type(j) = 'RA'
        else if
     +     (lctype(1:il).eq.'DEC' .or.
     +      lctype(1:il).eq.'MM') then
          type(j) = 'DEC'
        else if
     +     (lctype(1:il).eq.'ELON' .or.
     +      lctype(1:il).eq.'GLON') then
          type(j) = 'LONG'
        else if
     +     (lctype(1:il).eq.'ELAT' .or.
     +      lctype(1:il).eq.'GLAT') then
          type(j) = 'LATI'
        else if
     +     (lctype(1:il).eq.'VELO' .or.
     +      lctype(1:il).eq.'FELO') then
          type(j) = 'VELO'
        else if (lctype(1:il).eq.'FREQ') then
          type(j) = 'FREQ'
        else if (lctype(1:il).eq.'ANGLE') then
          type(j) = 'ANGL'
        else if (lctype(1:il).eq.'UU' .or.
     +           lctype(1:il).eq.'VV') then
          type(j) = 'UV'
        else
          type(j) = 'NONE'
        end if
      end do
c
      end
c
c
c* chkaxCO -- Check axis type and coordinate type for compatibility
c& nebk
c: coordinates
c+
      subroutine chkaxco (lun, ltype, iax, stype)
      implicit none
      integer iax, lun
      character*(*) ltype, stype
c
c  Check axis type and desired coordinate type are compatible.  
c
c  Input
c    lun    Image handle
c    ltype  Coordinate type user has asked for; one of
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz', 
c               'abskms', 'relkms', 'absnat', 'relnat', none'
c    iax    Axis of interest 
c    stype  Spectral axis descriptor.  If this is ' ', then
c           the CTYPE must match the TYPE (i.e. VELO/abskms is
c           good, FELO/absghz is bad).  Otherwise, it is assumed
c           that the spectral axis is going to be switched to
c           the desired STYPE so that any spectral CTYPE is
c           compatible with any spectral TYPE
c--
c-----------------------------------------------------------------------
      integer il, jax
      character ctype*32, str*132, gtype*4
      logical bad, bads
c-----------------------------------------------------------------
      if (stype.ne.' ' .and. stype.ne.'frequency' .and.
     +    stype.ne.'optical' .and. stype.ne.'radio') then
        str = 'CHKAXCO: invalid spectral axis type ('//stype//') given'
        call bug ('f', str)
      end if
c
c Get generic axis type
c
      call axtypco (lun, 0, iax, gtype)
c
      bad = .false.
      bads = .false.
c
c Compare generic axis type with label type
c
      if (ltype.eq.'hms') then
        if (gtype.ne.'RA' .and. gtype.ne.'LL') bad = .true.
      else if (ltype.eq.'dms') then
        if (gtype.ne.'DEC' .and. gtype.ne.'MM') bad = .true.
      else if (ltype.eq.'arcsec' .or. ltype.eq.'arcmin' .or.
     +         ltype.eq.'absdeg' .or. ltype.eq.'reldeg') then          
        call axfndco (lun, 'RAD', 0, iax, jax)
        if (jax.eq.0) bad = .true.
      else if (ltype.eq.'abskms' .or. ltype.eq.'relkms') then     
        if (gtype.ne.'VELO' .and. gtype.ne.'FREQ') bad = .true.
        if (gtype.eq.'FREQ' .and. stype.eq.' ') bads = .true.
      else if (ltype.eq.'absghz' .or. ltype.eq.'relghz') then     
        if (gtype.ne.'VELO' .and. gtype.ne.'FREQ') bad = .true.
        if (gtype.eq.'VELO' .and. stype.eq.' ') bads = .true.
      else if (ltype.eq.'absnat' .or. ltype.eq.'relnat') then
        continue
      end if
c
c Bug out if no good
c
      if (bad .or. bads) then
        call ctypeco (lun, iax, ctype, il)
        call output ('Axis ctype = '//ctype)
        str = 'Coordinate type = '//ltype
        call output (str)
        if (bads) then
          if (stype.eq.' ') then
            call output ('Spectral axis convention unspecified')
          else
            str = 'Spectral axis convention = '//stype
            call output (str)
          end if
        end if
        call bug ('f', 'CHKAXCO: These are inconsistent')
      end if
c
      end
c
c 
c* ctypeCO -- Return CTYPE for one axis
c& nebk
c: coordinates
c+
c
      subroutine ctypeco (lun, iax, ctype, il)
      implicit none
c
      integer iax, lun, il
      character*(*) ctype
c
c     Return CTYPE for one axis
c
c  Input
c    lun    Handle
c    iax    Axis to get CTYPE for
c  Output
c    ctype  CTYPE (upper case)
c    il     Length of string prior to project "--*" string
c--
c-----------------------------------------------------------------------
      character itoaf*1, str*6
      integer len1, il2
c----------------------------------------------------------------------- 
      str = 'ctype'//itoaf(iax)
      call rdhda (lun, str, ctype, ' ')
      if (ctype.eq.' ') call bug ('f', 'CTYPECO: '//str//' is blank')
c
      il2 = len1(ctype)
      il = 1
      do while (ctype(il:il).ne.'-' .and. il.le.il2)
        il = il + 1
      end do
      il = il - 1
c
      end
c
c* finCO -- Finish up after coordinate conversion routines
c& nebk
c: coordinates
c+
      subroutine finco (lun)
      implicit none
c
      integer lun
c
c     Tidy up after coordinate conversion routines have been accessed
c
c  Input
c    lun    Handle of image
c--
c-----------------------------------------------------------------------
      call cofin (lun)
c
      end
c  
c* initCO -- Initialize coordinate conversion routines
c& nebk
c: coordinates
c+
      subroutine initco (lun)
      implicit none
c
      integer lun
c
c     Initialize coordinate conversion routine
c
c  Input
c    lun    Handle of image
c--
c-----------------------------------------------------------------------
      call coinit (lun)
c
      end
c
c
      subroutine sctico (type, win, cti)
c-----------------------------------------------------------------------
c     This subroutine takes an NEBK style coordinate type descriptor,
c     and generates an RJS type coordinate descriptor making the
c     appropriate conversion to radians for angular coordinates
c     where needed ready for cocvt
c
c  Input
c    type   NEBK style type of world coordinate; one of
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz', 
c               'abskms', 'relkms', 'absnat', 'relnat', none'
c  Input/output
c    win    Coordinate value.  Any coordinate of an angular type
c           (hms, dms, *deg, arcsec, arcmin) will be in radians on output
c  Output
c    cti    RJS style coordinate type ('ap', 'op', 'aw', 'ow')
c-----------------------------------------------------------------------
      implicit none
      character*(*) type, cti
      double precision win
cc
      include 'mirconst.h'
      double precision a2r, d2r
      parameter (a2r = dpi/180.0d0/3600.0d0, d2r = dpi/180.0d0)
      character*132 str
c-----------------------------------------------------------------------
c
c Set coordinate conversion string and convert units to radian
c where needed
c
      cti = ' '
c
      if (type.eq.'hms' .or. type.eq.'dms') then
        cti = 'aw'
      else if (type.eq.'abspix' .or. type.eq.'none') then
        cti = 'ap'
      else if (type.eq.'relpix') then
        cti = 'op'
      else if (type.eq.'arcsec') then
        cti = 'ow'
        win = win * a2r
      else if (type.eq.'arcmin') then
        cti = 'ow'
        win = win * a2r * 60.0d0
      else if (type.eq.'absghz' .or. type.eq.'abskms' .or.
     +         type.eq.'absnat') then
        cti = 'aw'
      else if (type.eq.'relghz' .or. type.eq.'relkms' .or. 
     +         type.eq.'relnat') then
        cti = 'ow'
      else if (type.eq.'absdeg') then
        cti = 'aw'
        win = win * d2r
      else if (type.eq.'reldeg') then
        cti = 'ow'
        win = win * d2r
      else
        str = 'SCTICO: Unrecognized axis type ('//type//')'
        call bug ('f', str)
      end if
c
      end
c
c
      subroutine sctoco (type, wout)
c-----------------------------------------------------------------------
c     Convert an angular coordinate after it has been returned by 
c     RJS' cocvt into the units appropriate to NEBK style coordinates.  
c
c  Input 
c    type   NEBK style type of world coordinate that we want
c           to convert to;
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg', 
c               'reldeg', 'abspix', 'relpix', 'absghz', 'relghz', 
c               'abskms', 'relkms', 'absnat', 'relnat', 'none'
c  Input/output
c    wout   Coordinate value.  On input, all angular coordiantes are
c           in radians, on exit, 'arcsec' in arcsec, '*deg" in degrees
c	    'arcmin' in arcmin
c
c-----------------------------------------------------------------------
      implicit none
      character*(*) type
      double precision wout
cc
      include 'mirconst.h'
      double precision r2a, r2d
      parameter (r2a = 180.0d0*3600.0d0/dpi, r2d = 180.0d0/dpi)
c-----------------------------------------------------------------------
      if (type.eq.'arcsec') then
        wout = wout * r2a
      else if (type.eq.'arcmin') then
        wout = wout * r2a / 60.0d0
      else if (type.eq.'absdeg' .or. type.eq.'reldeg') then
        wout = wout * r2d
      end if
c
      end
c
c
c* setoaCO -- Set default (abs or rel) coordinate type depending on CTYPE
c& nebk
c: coordinates
c+
      subroutine setoaco (lun, absoff, n, iax, types)
      implicit none
c
      integer n, lun, iax
      character*(*) types(n), absoff*3
c
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
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg', 
c               'reldeg', 'absghz', 'relghz', 'abskms', 'relkms', 
c               'absnat', 'relnat', 'none'
c--
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
      end if
c      
      do i = i1, i2
c
c Get generic axis type and set default
c
        call axtypco (lun, 0, i, gtype)
c
        j = 1
        if (n.ne.0) j = i
        if (gtype.eq.'RA') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'hms'
          end if
        else if (gtype.eq.'DEC') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'dms'
          end if
        else if (gtype.eq.'LONG' .or. gtype.eq.'LATI') then
          if (absoff.eq.'off') then
            types(j) = 'reldeg'
          else
            types(j) = 'absdeg'
          end if
        else if (gtype.eq.'VELO') then
          if (absoff.eq.'off') then
            types(j) = 'relkms'
          else
            types(j) = 'abskms'
          end if
        else if (gtype.eq.'FREQ') then
          if (absoff.eq.'off') then
            types(j) = 'relghz'
          else
            types(j) = 'absghz'
          end if
        else if (gtype.eq.'ANGL') then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'absnat'
          end if
        else
          if (absoff.eq.'off') then
            types(j) = 'relnat'
          else
            types(j) = 'absnat'
          end if
        end if
      end do
c
      end
c
c
c* specCO -- See if this axis is spectral and what type it is
c& nebk
c: coordinates
c+
      subroutine specco (lun, iax, stype)
      implicit none
c
      integer lun, iax
      character*(*) stype
c
c     See if this axis is a spectral one and what type if it is
c
c  Input
c    lun    Handle of image
c    iax    Axis number
c  Output:
c    stype  ' ' if not spectral, else 'radio', 'optical', 'frequency'
c--
c-----------------------------------------------------------------------
      character*9 ctype
      integer il
c-----------------------------------------------------------------------
      call ctypeco (lun, iax, ctype, il)
c
      if (ctype(1:il).eq.'VELO') then
        stype = 'radio'
      else if(ctype(1:il).eq.'FELO') then
        stype = 'optical'
      else if(ctype(1:il).eq.'FREQ') then
        stype = 'frequency'
      else
        stype = ' '
      end if
c
      end
c
c
      subroutine sunitco (lun, iax, type, units)
c----------------------------------------------------------------------
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
c--
c-----------------------------------------------------------------------
      implicit none
      integer lun, iax
      character*(*) type, units
cc
      integer jax
      character units2*10, str*132, gtype*4
c-----------------------------------------------------------------------
      if (type.eq.'hms' .or. type.eq.'dms' .or. type.eq.'none') then
        units = ' '
      else if (type.eq.'arcsec') then
        units = 'arcsec'
      else if (type.eq.'arcmin') then
        units = 'arcmin'
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
        units = 'Km/s'
      else if (type.eq.'relkms') then
        units = 'offset Km/s'
      else if (type.eq.'absnat' .or. type.eq.'relnat') then
        call axtypco (lun, 0, iax, gtype)
        if (gtype.eq.'VELO') then
          units2 = 'Km/s'
        else if (gtype.eq.'FREQ') then
          units2 = 'GHz'
        else if (gtype.eq.'UV') then
          units = 'wavelengths'
        else 
          call axfndco (lun, 'RAD', 0, iax, jax)
          if (jax.ne.0) then
            units2 = 'radians'
          else 
            units2 = ' '
          end if
        end if
c
        if (type.eq.'absnat') then
          units = units2
        else if (type.eq.'relnat') then
          units = 'offset '//units2
        end if
      else
        str = 'SUNITCO: Unrecognized label type ('//type//')'
        call bug ('f', str)
      end if
c
      end
c
c
c* w2wCO -- Convert an array of coordinates
c& nebk
c: coordinates
c+
      subroutine w2wco (lun, n, typei, stypei, win, typeo, stypeo, wout)
      implicit none
c
      integer lun, n
      double precision win(n), wout(n)
      character*(*) typei(n), typeo(n), stypei, stypeo
c
c  Convert an array of NEBK style coordinates with the COCVT routines.
c
c  Input
c    lun     Handle of open file
c    n       Number of axes to convert
c    typei   Array of input coordinate types, Should be from list
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
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
c	        'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in Km/s
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
c--
c-----------------------------------------------------------------------
      include 'maxnax.h'
      integer i, ip
      character cti*21, cto*21, str*2 
      character*9 sstype, lstype
      double precision wloc(maxnax), xdum
      logical done, nix(maxnax), none
c-----------------------------------------------------------------------
c
c There maybe nothing to do for some axes.  Make sure we just
c copy the coordinates in these cases, rather than converting
c to and from pixels, thus losing precision. Save initial spectral
c convention while we are it.
c
      none = .true.
      sstype = ' '
      do i = 1, n
        nix(i) = .false.
        call specco (lun, i, lstype)
        if (lstype.ne.' ') sstype = lstype
c
        if (typei(i).eq.typeo(i)) then
          if (lstype.ne.' ') then
            if (stypei.eq.stypeo .or. typei(i)(4:6).eq.'pix')
     +        nix(i) = .true.
          else
            nix(i) = .true.
          end if
        end if
        if (.not.nix(i)) none = .false.
      end do
      if (none) then
        do i = 1, n
          wout(i) = win(i)
        end do
        return
      end if
c
c Switch spectral axis if required to type of input coordinate and
c fish out the CTYPES
c
      if (sstype.ne.' ' .and. stypei.ne.' ')
     +  call covelset (lun, stypei)
c
c Convert coordinates to absolute pixels first; loop over axes
c
      cti = '  '
      cto = '  '
      ip = 1
      do i = 1, n
c
c Check input coordinate type consistent with actual axis type
c
        call chkaxco (lun, typei(i), i, stypei)
c
c Set coordinate transformation strings and convert angular
c units if required to radians
c
        wloc(i) = win(i)
        call sctico (typei(i), wloc(i),  str)
        cti(ip:ip+2) = str//'/'
        cto(ip:ip+2) = 'ap/'
        ip = ip + 3
      end do
c
c Now convert to pixels (pixels being converted to pixels here
c will be done with no loss of precision so don't bother with
c extra code to trap it
c
      call cocvt (lun, cti, wloc, cto, wout)
c
c Now check that we need to go on.  The user may want absolute
c pixels whereupon we are done.  Note that absolute pixels
c are the same regardless of the spectral convention !
c
      done = .true.
      do i = 1, n
        if (typeo(i).ne.'abspix') done = .false.
      end do
c
      if (.not.done) then
c
c Having turned the coordinate into a pixel, we can now convert
c it to the desired output coordinate type.  First, once again
c switch the spectral axis if needed.
c
        if (sstype.ne.' ' .and. stypeo.ne.' ') 
     +    call covelset (lun, stypeo)
c
c Loop over axes 
c
        cti = '  '
        cto = '  '
        ip = 1
        do i = 1, n
c
c Check output coordinate type consistent with actual axis type
c
          call chkaxco (lun, typeo(i), i, stypeo)
c
c Set coordinate transformation strings
c
          wloc(i) = wout(i)
          call sctico (typeo(i), xdum, str)
          cti(ip:ip+2) = 'ap/'
          cto(ip:ip+2) = str//'/'
          ip = ip + 3
        end do
c
c Now convert the absolute pixels to the desired coordinate type
c
        call cocvt (lun, cti, wloc, cto, wout)
c
c Now we must convert (some of the) coordinates given in radians to the 
c appropriate output units (degrees, arcsec etc)
c
        do i = 1, n
          call sctoco (typeo(i), wout(i))
        end do
      end if
c
c Overwrite any coordinates that did not really need converting
c by the input values to improve precision
c
      do i = 1, n
        if (nix(i)) wout(i) = win(i)
      end do
c
c Restore initial spectral axis convention to common held header
c
      if (sstype.ne.' ') call covelset (lun, sstype)
c
      end
c
c
c* w2wfCO -- Convert an array of coordinates and format 
c& nebk
c: coordinates
c+
      subroutine w2wfco (lun, n, typei, stypei, win, typeo, stypeo,
     +                   nounit, strout, strlen)
      implicit none
c
      integer lun, n, strlen(n)
      double precision win(n)
      character*(*) typei(n), typeo(n), strout(n), stypei, stypeo
      logical nounit
c
c  Convert an array of NEBK style coordinates with the COCVT routines 
c  and format the results with units into strings
c
c  Input
c    lun     Handle of open file
c    n       Number of axes
c    typei   Array of input coordinate types, Should be from list
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg', 
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
c		'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in Km/s
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
c--
c-----------------------------------------------------------------------
      include 'maxnax.h'
      double precision wout(maxnax)
      character*30 rangle, hangleh, units
      character str*132
      integer i, len1
c-----------------------------------------------------------------------
c
c Convert coordinates
c
      call w2wco (lun, n, typei, stypei, win, typeo, stypeo, wout)
c
c Format results
c
      do i = 1, n
        strout(i) = ' '
        if (typeo(i).eq.'abspix' .or. typeo(i).eq.'relpix' .or.
     +      typeo(i).eq.'none') then
          call strfd (wout(i), '(f9.2)', strout(i), strlen(i))
        else if (typeo(i).eq.'abskms' .or. typeo(i).eq.'relkms') then
          call strfd (wout(i), '(1pe12.5)', strout(i), strlen(i))
        else if (typeo(i).eq.'absghz' .or. typeo(i).eq.'relghz') then
          call strfd (wout(i), '(1pe15.8)', strout(i), strlen(i))
        else if (typeo(i).eq.'absdeg' .or. typeo(i).eq.'reldeg') then
          call strfd (wout(i), '(f8.3)', strout(i), strlen(i))  
        else if (typeo(i).eq.'arcsec' .or. typeo(i).eq.'arcmin') then
          call strfd (wout(i), '(1pe15.8)', strout(i), strlen(i))
        else if (typeo(i).eq.'absnat' .or. typeo(i).eq.'relnat') then
          call strfd (wout(i), '(1pe15.8)', strout(i), strlen(i))
        else if (typeo(i).eq.'hms') then
          strout(i) = hangleh(wout(i))
          strlen(i) = len1(strout(i))
        else if (typeo(i).eq.'dms') then
          strout(i) = rangle(wout(i))
          strlen(i) = len1(strout(i))
        else
          str = 'W2WFCO: Unrecognized coordinate type ('//typeo(i)//')'
          call bug ('f', str)
        end if
c
c Work out units
c
        if (.not.nounit) then
          call sunitco (lun, i, typeo(i), units)
c
c Add units to formatted number
c
          strout(i)(strlen(i)+2:) = units
          strlen(i) = len1(strout(i))
        end if
      end do
c
      end
c
c
c* w2wsCO -- Convert NEBK style coordinate for a single axis
c& nebk
c: coordinates
c+
      subroutine w2wsco (lun, iax, typei, stypei, win, typeo, stypeo,
     +                   wout)
      implicit none
c
      integer lun, iax
      double precision win, wout
      character*(*) typei, typeo, stypei, stypeo
c
c  Convert one NEBK style coordinate with the COCVT routines.
c  Coordinates for the other axes are assumed to be at the
c  reference pixel
c
c  Input
c    lun     Handle of open file
c    iax     Axis
c    typei   Input coordinate type, Should be from list
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg',
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
c		'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in Km/s
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
c--
c-----------------------------------------------------------------------
      include 'maxnax.h'
      character*6 ltypei(maxnax), ltypeo(maxnax)
      double precision lwin(maxnax), lwout(maxnax)
      integer i
cc
      integer naxis
c-----------------------------------------------------------------------
c
c Load reference pixel for dummy locations
c
      call rdhdi (lun, 'naxis', naxis, 0)
      if (iax.le.0 .or. iax.gt.naxis)  
     +  call bug ('f', 'W2WSCO: invalid axis number')
      do i = 1, naxis
        ltypei(i) = 'relpix'
        lwin(i) = 0.0d0
        ltypeo(i) = 'relpix'
      end do
c
c Load axis of interest
c
      ltypei(iax) = typei
      lwin(iax) = win
      ltypeo(iax) = typeo
c
c Convert
c
      call w2wco (lun, naxis, ltypei, stypei, lwin, ltypeo, 
     +            stypeo, lwout)
c
c Fish out axis
c
      wout = lwout(iax)
c
      end
c
c
c* w2wfsCO -- Convert a coordinate for a single axis and format 
c& nebk
c: coordinates
c+
      subroutine w2wsfco (lun, iax, typei, stypei, win, typeo, stypeo,
     +                    nounit, strout, strlen)
      implicit none
c
      integer lun, iax, strlen
      double precision win
      character*(*) typei, typeo, strout, stypei, stypeo
      logical nounit
c
c  Convert one NEBK style coordinate with the COCVT routines and format
c  into a string.  Coordinates for the other axes are assumed to be at the
c  reference pixel
c
c  Input
c    lun     Handle of open file
c    iax     Axis of interest
c    typei   Coordinate type, should be from list
c	        'hms',    'dms',    'arcsec', 'arcmin', 'absdeg', 
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
c		'arcmin'     in arcmin
c               '*  pix'     in pixels
c               '*  ghz'     in GHz
c               '*  kms'     in Km/s
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
c--
c-----------------------------------------------------------------------
      include 'maxnax.h'
      double precision lwin(maxnax)
      character*6 ltypei(maxnax), ltypeo(maxnax)
      character*50 lstrout(maxnax)
      integer i, lstrlen(maxnax)
cc
      integer naxis
c-----------------------------------------------------------------------
c
c Load dummy array values and actual value into conversion arrays
c
      call rdhdi (lun, 'naxis', naxis, 0)
      if (iax.le.0 .or. iax.gt.naxis)  
     +  call bug ('f', 'W2WSFCO: invalid axis number')
      do i = 1, naxis
        lwin(i) = 0.0
        ltypei(i) = 'relpix'
        ltypeo(i) = 'relpix'
      end do
      lwin(iax) = win
      ltypei(iax) = typei
      ltypeo(iax) = typeo
c
c Convert and format
c
      call w2wfco (lun, iax, ltypei, stypei, lwin, ltypeo, stypeo, 
     +             nounit, lstrout, lstrlen)
c
c Return formatted string
c
      strout = lstrout(iax)
      strlen = lstrlen(iax)
c
      end
