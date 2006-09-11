c***********************************************************************
c  These subroutines provide an interface between NEBK style coordinate
c  handling (the 'hms', 'dms', 'arcsec', 'arcmin', 'reldeg', 'abspix', 
c  'relpix', 'absghz', 'relghz', 'abskms', 'relkms', 'absnat', 
c  'relnat', 'none') and RJS' new coordinate routines (co.for). 
c
c
c  User callable routines are:
c
c   initco  (lun)                         
c   finco   (lun)
c
c   axfndco (lun, gentyp, n, iax, jax)    
c   axtypco (lun, n, iax, type)
c   chkaxco (lun, ltype, n, iax, stype)
c   ctypeco (lun, iax, ctype)
c   setoaco (lun, absoff, n, iax, types)
c   specco  (lun, iax, stype)
c
c   w2wco   (lun, n, typei, stypei, win, typeo, stypeo, wout)
c   w2wfco  (lun, n, typei, stypei, win, typeo, stypeo,
c            nounit, strout, strlen)
c   w2wsco  (lun, iax, typei, stypei, win, typeo, stypeo, wout)
c   w2wsfco (lun, iax, typei, stypei, win, typeo, stypeo,
c            nounit, strout, strlen)
c
c  History:
c    nebk   18aug94    Initial version
c    nebk   29dec94    Remove concatenated char*(*) in subroutine calls
c    nebk   11aug95    Add arcmin labels
c    nebk   14nov95    Remove LINCO; its use has been eradicated, add
c                      some new ones which have migrated from CGSUB.FOR
c                      These are AXFNDCO, AXTYPCO, SUNITCO
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
c    type   Generic axis type to find in axis string.  The first axis
c           encountered that has this type is returned.  The type
c           should be one of:
c
c             RA   ->  RA, LL, ELON, GLON
c             DEC  ->  DEC, MM, ELAT, GLAT
c             LONG ->  ELON, GLON
c             LATI ->  ELAT, GLAT
c             VELO ->  VELO, FELO
c             FREQ ->  FREQ
c             UV   ->  UU, VV
c             RAD  ->  An axis whose increment should be in
c                      radians.  These are RA, DEC, LAT, LONG axes
c                      as described by the LHS above.
c           Other types are searched for exactly as specified
c    n      Number of axes to search starting from 1
c    iax    SPecific axis to match if N=0
c  Output
c    jax    Axis number that matches "type".  0 if not present
c--
c-----------------------------------------------------------------
      integer i, i1, i2
      character ltype*8, lctype*8
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
        call ctypeco (lun, i, lctype)
c
        if (ltype.eq.'RA') then
          if (index(lctype,'RA').ne.0 .or.   
     +        index(lctype,'LL').ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'DEC') then
          if (index(lctype,'DEC').ne.0 .or.
     +        index(lctype,'MM').ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'LONG') then
          if (index(lctype,'ELON').ne.0 .or.
     +        index(lctype,'GLON').ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'LATI') then
          if (index(lctype,'ELAT').ne.0 .or.
     +        index(lctype,'GLAT').ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if
        else if (ltype.eq.'VELO') then
          if (index(lctype,'VELO').ne.0 .or. 
     +        index(lctype,'FELO').ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return 
          end if
        else if (ltype.eq.'FREQ') then
          if (index(lctype,'FREQ').ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return
          end if  
        else if (ltype.eq.'RAD') then
          if (index(lctype,'RA').ne.0 .or.
     +        index(lctype,'LL').ne.0 .or.
     +        index(lctype,'DEC').ne.0 .or.
     +        index(lctype,'MM').ne.0 .or.
     +        index(lctype,'ELON').ne.0 .or.
     +        index(lctype,'GLON').ne.0 .or.
     +        index(lctype,'ELAT').ne.0 .or.
     +        index(lctype,'GLAT').ne.0) then
            jax = 1
            if (n.ne.0) jax = i
            return 
          end if  
        else if (ltype.eq.'UV') then
          if (index(lctype,'UU').ne.0 .or.
     +        index(lctype,'VV').ne.0) then
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
      character*4 type(n)
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
c                 RA, DEC, LATI, LONG, VELO, FREQ, UV, NONE  where
c
c             RA   means CTYPE was one of   RA, LL
c             DEC  means CTYPE was one of   DEC, MM
c             LONG means CTYPE was one of   ELON, GLON
c             LATI means CTYPE was one of   ELAT, GLAT
c             VELO means CTYPE was one of   VELO, FELO
c             FREQ means CTYPE was one of   FREQ
c             UV   means CTYPE was one of   UU, VV
c             NONE means CTYPE was not recognized
c
c--
c-----------------------------------------------------------------
      integer i, i1, i2, j
      character lctype*8
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
        call ctypeco (lun, i, lctype)
c
        j = 1
        if (n.ne.0) j = i
        if (index(lctype,'RA').ne.0 .or.
     +      index(lctype,'LL').ne.0) then
          type(j) = 'RA'
        else if
     +     (index(lctype,'DEC').ne.0 .or.
     +      index(lctype,'MM').ne.0) then
          type(j) = 'DEC'
        else if
     +     (index(lctype,'ELON').ne.0 .or.
     +      index(lctype,'GLON').ne.0) then
          type(j) = 'LONG'
        else if
     +     (index(lctype,'ELAT').ne.0 .or.
     +      index(lctype,'GLAT').ne.0) then
          type(j) = 'LATI'
        else if
     +     (index(lctype,'VELO').ne.0 .or.
     +      index(lctype,'FELO').ne.0) then
          type(j) = 'VELO'
        else if (index(lctype,'FREQ').ne.0) then
          type(j) = 'FREQ'
        else if
     +     (index(lctype,'UU').ne.0 .or.
     +      index(lctype,'VV').ne.0) then
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
      subroutine chkaxco (lun, ltype, n, iax, stype)
      implicit none
      integer n, iax, lun
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
c    n      NUmber of axes to check starting from 1
c    iax    Specific axis if n=0
c    stype  Spectral axis descriptor.  If this is ' ', then
c           the CTYPE must match the TYPE (i.e. VELO/abskms is
c           good, FELO/absghz is bad).  Otherwise, it is assumed
c           that the spectral axis is going to be switched to
c           the desired STYPE so that any spectral CTYPE is
c           compatible with any spectral TYPE
c--
c-----------------------------------------------------------------------
      integer i1, i2, i
      character ctype*8, str*132
      logical bad, bads
c-----------------------------------------------------------------
      if (stype.ne.' ' .and. stype.ne.'frequency' .and.
     +    stype.ne.'optical' .and. stype.ne.'radio') then
        str = 'CHKAXCO: invalid spectral axis type ('//stype//') given'
        call bug ('f', str)
      end if
c
      if (n.eq.0) then
        i1 = iax
        i2 = iax
      else
        i1 = 1
        i2 = n
      end if
c
      do i = i1, i2
        call ctypeco (lun, i, ctype)
c
        bad = .false.
        bads = .false.
c
        if (ltype.eq.'hms') then
          if (index(ctype,'RA').eq.0 .and.
     +        index(ctype,'LL').eq.0) bad = .true.
        else if (ltype.eq.'dms') then
          if (index(ctype,'DEC').eq.0 .and.
     +        index(ctype,'MM') .eq.0) bad = .true.
        else if (ltype.eq.'arcsec' .or. ltype.eq.'arcmin') then
          if (index(ctype,'RA') .eq.0 .and.
     +        index(ctype,'LL') .eq.0 .and.
     +        index(ctype,'DEC').eq.0 .and.
     +        index(ltype,'MM') .eq.0) bad = .true.
        else if (ltype.eq.'absdeg' .or. ltype.eq.'reldeg') then
          if (index(ctype,'RA') .eq.0 .and.
     +        index(ctype,'LL') .eq.0 .and.
     +        index(ctype,'DEC').eq.0 .and.
     +        index(ctype,'MM') .eq.0 .and.
     +        index(ctype,'ELON').eq.0 .and.
     +        index(ctype,'GLON').eq.0 .and.
     +        index(ctype,'ELAT').eq.0 .and.
     +        index(ctype,'GLAT').eq.0) bad = .true.
        else if (ltype.eq.'abskms' .or. ltype.eq.'relkms') then     
          if (index(ctype,'VELO').eq.0 .and.
     +        index(ctype,'FELO').eq.0 .and.
     +        index(ctype,'FREQ').eq.0) bad = .true.
              if (index(ctype,'FREQ').ne.0 .and. 
     +           stype.eq.' ') bads = .true.
        else if (ltype.eq.'absghz' .or. ltype.eq.'relghz') then     
          if (index(ctype,'FREQ').eq.0 .and.
     +        index(ctype,'VELO').eq.0 .and.
     +        index(ctype,'FELO').eq.0) bad = .true.
              if ((index(ctype,'VELO').ne.0 .or. 
     +             index(ctype,'FELO').ne.0) .and.
     +           stype.eq.' ') bads = .true.
        else if (ltype.eq.'absnat' .or. ltype.eq.'relnat') then
          continue
        end if
c
c Bug out if no good
c
        if (bad .or. bads) then
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
      end do
c
      end
c
c 
c* ctypeCO -- Return axis CTYPEs
c& nebk
c: coordinates
c+
c
      subroutine ctypeco (lun, iax, ctype)
      implicit none
c
      integer iax, lun
      character*(*) ctype(*)  
c
c     Return ctypes for several or one axis
c
c  Input
c    lun    Handle
c    iax    Axis to get CTYPE for
c  Output
c    ctype  CTYPE (upper case)
c--
c-----------------------------------------------------------------------
      character itoaf*1
c-----------------------------------------------------------------------
      call rdhda (lun, 'ctype'//itoaf(iax), ctype, ' ')
      call ucase (ctype)
c
      end
c
c* finco -- Finish up after coordinate conversion routines
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
c* initco -- Initialize coordinate conversion routines
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
c* setoaco -- Set default (abs or rel) coordinate type depending on CTYPE
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
      character*9 lctype
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
c Get CTYPE for this axis
c
        call ctypeco (lun, i, lctype)
c
c Set default
c
        j = 1
        if (n.ne.0) j = i
        if (index(lctype,'RA') .ne.0 .or.
     +      index(lctype,'LL') .ne.0 ) then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'hms'
          end if
        else if (index(lctype,'DEC').ne.0 .or.
     +           index(lctype,'MM') .ne.0) then
          if (absoff.eq.'off') then
            types(j) = 'arcsec'
          else
            types(j) = 'dms'
          end if
        else if (index(lctype,'ELON').ne.0 .or.
     +           index(lctype,'GLON').ne.0 .or.
     +           index(lctype,'ELAT').ne.0 .or.
     +           index(lctype,'GLAT').ne.0) then
          if (absoff.eq.'off') then
            types(j) = 'reldeg'
          else
            types(j) = 'absdeg'
          end if
        else if (index(lctype,'VELO').ne.0) then
          if (absoff.eq.'off') then
            types(j) = 'relkms'
          else
            types(j) = 'abskms'
          end if
        else if (index(lctype,'FELO').ne.0) then
          if (absoff.eq.'off') then
            types(j) = 'relkms'
          else
            types(j) = 'abskms'
          end if
        else if (index(lctype,'FREQ').ne.0) then
          if (absoff.eq.'off') then
            types(j) = 'relghz'
          else
            types(j) = 'absghz'
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
c* specco -- See if this axis is spectral and what type it is
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
c-----------------------------------------------------------------------
      call ctypeco (lun, iax, ctype)
      if (index(ctype,'VELO').ne.0) then
        stype = 'radio'
      else if(index(ctype,'FELO').ne.0) then
        stype = 'optical'
      else if(index(ctype,'FREQ').ne.0) then
        stype = 'frequency'
      else
        stype = ' '
      end if
c
      end
c
c
      subroutine sunitco (ctype, type, units)
c----------------------------------------------------------------------
c  Set the units of a pixel based upon the requested type and the 
c  axis type.  Used for ascii not graphical output so no PGPLOT escape 
c  sequences.
c
c  Inputs:
c    ctype  Axis header CTYPE value
c    type   User requested coordinate type
c  Output:
c    units  Axis units
c--
c-----------------------------------------------------------------------
      implicit none
      character*(*) type, units, ctype
cc
      character units2*10, str*132
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
        if (index(ctype,'VELO').ne.0 .or.
     +      index(ctype,'FELO').ne.0) then
          units2 = 'Km/s'
        else if (index(ctype,'FREQ').ne.0) then
          units2 = 'GHz'
        else if (index(ctype,'UU').ne.0 .or.
     +           index(ctype,'VV').ne.0) then
          units = 'wavelengths'
        else if (index(ctype,'RA').ne.0 .or.
     +           index(ctype,'DEC').ne.0 .or.
     +           index(ctype,'LL').ne.0 .or.
     +           index(ctype,'MM').ne.0 .or.
     +           index(ctype,'ELON').ne.0 .or.
     +           index(ctype,'ELAT').ne.0 .or.
     +           index(ctype,'GLON').ne.0 .or.
     +           index(ctype,'GLAT').ne.0) then
          units2 = 'radians'
        else 
          units2 = ' '
        end if
c
        if (type.eq.'absnat') then
          units = units2
        else if (type.eq.'relnat') then
          units = 'offset '//units2
        end if
      else
        str = 'SUNITCO: Unrecognized coordinate type ('//type//')'
        call bug ('f', str)
      end if
c
      end
c
c
c* w2wco -- Convert an array of coordinates
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
        call chkaxco (lun, typei(i), 0, i, stypei)
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
          call chkaxco (lun, typeo(i), 0, i, stypeo)
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
c* w2wfco -- Convert an array of coordinates and format 
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
      character ctype*9, str*132
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
          call ctypeco (lun, i, ctype)
          call sunitco (ctype, typeo(i), units)
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
c* w2wsco -- Convert NEBK style coordinate for a single axis
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
c-----------------------------------------------------------------------
c
c Load reference pixel for dummy locations
c
      do i = 1, maxnax
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
      call w2wco (lun, maxnax, ltypei, stypei, lwin, ltypeo, 
     +            stypeo, lwout)
c
c Fish out axis
c
      wout = lwout(iax)
c
      end
c
c
c* w2wfsco -- Convert a coordinate for a single axis and format 
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
c-----------------------------------------------------------------------
c
c Load dummy array values and actual value into conversion arrays
c
      if (iax.le.0)  call bug ('f', 'W2WSFCO: invalid axis number')
      do i = 1, maxnax
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
