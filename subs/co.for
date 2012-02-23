c***********************************************************************
c
c  A set of routines to convert between different coordinate systems.
c  User callable routines are as follows.
c
c    Create a coordinate object (call coFin to release it):
c      coInit(lu)
c      coDup(lin,lout)
c      coRaDec(lu,proj,ra0,dec0)
c      coCreate(naxis,lu)
c
c  In general one of the above must be called before any of the
c  following:
c
c    Enquire about a coordinate object:
c      coCompar(lu1,lu2,match)
c      coFindAx(lu,axis,iax)
c      coCtype(ctype,axtype,wtype,algo,units) - coInit not required.
c      coAxType(lu,iax,axtype,wtype,algo,units)
c      coLin(lu1,in,x1,n,ctype,crpix,crval,cdelt)
c      coAxGet(lu,iax,ctype,crpix,crval,cdelt)
c      coGetD(lu,object,value)
c      coGetI(lu,object,value)
c      coGetA(lu,object,value)
c
c    Manipulate a coordinate object:
c      coAxSet(lu,iax,ctype,crpix,crval,cdelt)
c      coSetD(lu,object,value)
c      coSetI(lu,object,value)
c      coSetA(lu,object,value)
c      coCpyD(lu1,lu2,object)
c      coCpyI(lu1,lu2,object)
c      coCpyA(lu1,lu2,object)
c      coPrjSet(lu,proj)
c
c    Manipulate a coordinate object:
c      coAltPrj(lu)
c      coSpcSet(lu,stype,frame,iax,algo)
c
c    Perform conversions of various kinds:
c      coFreq(lu,in,x1,freq)
c      coCvt1(lu,iax,in,x1,out,x2)
c      coCvt(lu,in,x1,out,x2)
c      coCvtv(lu,in,x1,out,x2,valid)
c      coGauCvt(lu,in,x1,io,bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2)
c      coLMN(lu,in,x1,lmn)
c      coGeom(lu,in,x1,ucoeff,vcoeff)
c
c    Miscellaneous operations:
c      coReinit(lu)
c      coPrint(lu)
c      coWrite(lu,tno)
c      coFin(lu)
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c $Id: co.for,v 1.34 2012/02/23 23:52:09 cal103 Exp $
c***********************************************************************

c* coInit -- Initialise coordinate conversion routines.
c& rjs
c: coordinates
c+
      subroutine coInit(lu)

      integer lu
c  ---------------------------------------------------------------------
c  Initialise the coordinate conversion system.
c
c  The coordinate conversion routines supports simultaneously a number
c  of coordinate systems.  A coordinate system is initialised by a call
c  to coInit, giving the handle of the Miriad data-set in question.
c  This initialises a new coordinate system.  Subsequent calls to the
c  coordinate routines also give the handle of the data-set, which is
c  used to determine the appropriate coordinate system to use.
c
c  Input:
c    lu         The handle of the Miriad data-set, either an image or
c               visibility data-set.  For a visibility data-set, the
c               coordinate system simply consists of RA and DEC axes,
c               centered on the observing centre.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, status

      external  coLoc, hdprsnt
      logical   hdprsnt
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.true.)

      if (nalloc(icrd).gt.1) return

c     Initialize the celprm struct.
      status = celini(cel(1,icrd))

c     Image or visibility data set?
      if (hdprsnt(lus(icrd),'visdata')) then
c       Apparently a visibility data set.
        call coInitUV(icrd)
      else if (hdprsnt(lus(icrd),'image')) then
c       Apparently an image.
        call coInitXY(icrd)
      else
        call bug('f','Unrecognised dataset type in coInit')
      endif

c     Finish initialising this coordinate object.
      call coReinit(lu)

      end

c***********************************************************************

c* coDup -- Duplicate a coodinate object.
c& rjs
c: coordinates
c+
      subroutine coDup(lin,lout)

      integer   lin, lout
c  ---------------------------------------------------------------------
c  Duplicate a coordinate object.
c
c  Input:
c    lin        Handle of the coordinate object to be duplicated.
c  Output:
c    lout       Duplicated coordinate object.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd1, icrd2, j

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd1 = coLoc(lin,  .false.)

      call coCreate(naxis(icrd1), lout)
      icrd2 = coLoc(lout, .false.)

      do iax = 1, naxis(icrd2)
        crpix(iax,icrd2) = crpix(iax,icrd1)
        cdelt(iax,icrd2) = cdelt(iax,icrd1)
        crval(iax,icrd2) = crval(iax,icrd1)
        ctype(iax,icrd2) = ctype(iax,icrd1)
      enddo
      cosrot(icrd2) = cosrot(icrd1)
      sinrot(icrd2) = sinrot(icrd1)

      restfrq(icrd2) = restfrq(icrd1)
      eqnox(icrd2)   = eqnox(icrd1)
      obstime(icrd2) = obstime(icrd1)
      vobs(icrd2)    = vobs(icrd1)
      frqscl(icrd2)  = frqscl(icrd1)

      defs(1,icrd2)  = defs(1,icrd1)
      defs(2,icrd2)  = defs(2,icrd1)
      defs(3,icrd2)  = defs(3,icrd1)
      defs(4,icrd2)  = defs(4,icrd1)

      do j = 1, CELLEN
        cel(j,icrd2) = cel(j,icrd1)
      enddo

      call coReinit(lout)

      end

c***********************************************************************

c* coRaDec -- Create a simple RA/DEC coordinate system.
c& rjs
c: coordinates
c+
      subroutine coRaDec(lu,proj,ra0,dec0)

      integer lu
      character proj*(*)
      double precision ra0,dec0
c  ---------------------------------------------------------------------
c  Create a simple RA/DEC coordinate system.
c
c  Input:
c    proj       Projection code: NCP, SIN, etc.
c    ra0,dec0   RA,DEC of the reference point.
c  Output:
c    lu         Handle of the output coordinate object.
c-----------------------------------------------------------------------
      character ctypei*8
c-----------------------------------------------------------------------
      call coCreate(2, lu)

      if (proj.ne.' ') then
        ctypei = 'RA---' // proj
        call coAxSet(lu, 1, ctypei, 0d0, ra0,  1d0)
        ctypei = 'DEC--' // proj
        call coAxSet(lu, 2, ctypei, 0d0, dec0, 1d0)
      else
        call coAxSet(lu, 1, 'RA',  0d0, ra0,  1d0)
        call coAxSet(lu, 2, 'DEC', 0d0, dec0, 1d0)
      endif

      call coReinit(lu)

      end

c***********************************************************************

c* coCreate -- Begin intialisation of a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coCreate(nax, lu)

      integer nax, lu
c  ---------------------------------------------------------------------
c  Begin building up a coordinate object from scratch.
c
c  Output:
c    lu         Handle of the coordinate object.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, status

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(0, .true.)
      lu = -icrd

      naxis(icrd) = nax

c     Initialize the celprm struct.
      status = celini(cel(1,icrd))

      cosrot(icrd)  = 1d0
      sinrot(icrd)  = 0d0

      restfrq(icrd) = 0d0
      eqnox(icrd)   = 0d0
      obstime(icrd) = 0d0
      vobs(icrd)    = 0d0
      frqscl(icrd)  = .true.

      end

c***********************************************************************

c* coCompar - Compare two coordinate systems
c& rjs
c: coordinates
c+
      logical function coCompar(lu1,lu2,match)

      integer lu1,lu2
      character match*(*)
c  ---------------------------------------------------------------------
c  Compare two coordinate systems, returning .true. if they are alike,
c  and .false. otherwise.  The "match" argument determines the tests
c  performed.
c
c  Input:
c    lu1,lu2    Handles of the two coordinate systems to be compared.
c    match      This determines the tests to be applied.  Tolerances
c               are 0.01%, except for match='approx' (see below).
c               Possible values are:
c
c                 Value         Description
c                 -----         -----------
c                 'exact'       Check that ctype, crval, crpix, and
c                               cdelt are identical.
c                 'projection'  Check that ctype and crval are the same,
c                               i.e. whether the coordinate systems have
c                               the same projection and reference value.
c                 'offset'      Check ctype, crval, and cdelt, i.e.
c                               whether the coordinate systems are
c                               identical to within a shift of the
c                               pixel coordinates.
c                 'approx'      Check whether the two coordinate systems
c                               are approximately the same - less than
c                               0.1 pixel difference at the reference
c                               pixel of the first system, cdelts that
c                               agree to within 1%, and compatible
c                               ctypes.
c                 'align'       crpixs are the same.
c
c  Output:
c    coCompar   True if the two coordinate systems are alike, and false
c               otherwise.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd1, icrd2, nax
      double precision dscr, x1(7), x2(7)

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd1 = coLoc(lu1,.false.)
      icrd2 = coLoc(lu2,.false.)
      coCompar = .false.

c     Check that the number of axes are the same.
      if (naxis(icrd1).ne.naxis(icrd2)) return
      nax = naxis(icrd1)

c     Switch to the right operation.
      if (match.eq.'exact') then
        do iax = 1, nax
          if (ctype(iax,icrd1).ne.ctype(iax,icrd2)) return
          if (abs(crpix(iax,icrd1)-crpix(iax,icrd2)).gt.1d-4) return
          dscr = 1d-4*min(abs(cdelt(iax,icrd1)),abs(cdelt(iax,icrd2)))
          if (abs(crval(iax,icrd1)-crval(iax,icrd2)).gt.dscr) return
          if (abs(cdelt(iax,icrd1)-cdelt(iax,icrd2)).gt.
     *           1d-4*abs(cdelt(iax,icrd1))) return
        enddo

      else if (match.eq.'projection') then
        do iax = 1, nax
          if (ctype(iax,icrd1).ne.ctype(iax,icrd2)) return
          dscr = 1d-4*min(abs(cdelt(iax,icrd1)),abs(cdelt(iax,icrd2)))
          if (abs(crval(iax,icrd1)-crval(iax,icrd2)).gt.dscr) return
        enddo

      else if (match.eq.'offset') then
        do iax = 1, nax
          if (ctype(iax,icrd1).ne.ctype(iax,icrd2)) return
          dscr = 1d-4*min(abs(cdelt(iax,icrd1)),abs(cdelt(iax,icrd2)))
          if (abs(crval(iax,icrd1)-crval(iax,icrd2)).gt.dscr) return
          if (abs(cdelt(iax,icrd1)-cdelt(iax,icrd2)).gt.
     *           1d-4*abs(cdelt(iax,icrd1))) return
        enddo

      else if (match.eq.'align') then
        do iax = 1, nax
          if (abs(crpix(iax,icrd1)-crpix(iax,icrd2)).gt.1d-4) return
        enddo

      else if (match.eq.'approx') then
c       Are the two systems comparable?
        do iax = 1, nax
          if (ctype(iax,icrd1)(1:5).ne.ctype(iax,icrd2)(1:5)) return
          if (abs(cdelt(iax,icrd1)-cdelt(iax,icrd2)).gt.
     *           1d-2*abs(cdelt(iax,icrd1))) return
        enddo

c       Determine the absolute pixel location, in system 2, of the
c       reference pixel, in system 1.
        do iax = 1, 7
          x1(iax) = 0d0
        enddo
        call coCvt(lu1,'op/op/op/op/op/op/op',x1,
     *                 'aw/aw/aw/aw/aw/aw/aw',x2)
        call coCvt(lu2,'aw/aw/aw/aw/aw/aw/aw',x2,
     *                 'ap/ap/ap/ap/ap/ap/ap',x1)

c       Check that they more or less line up at the reference pixel.
        do iax = 1, nax
          if (abs(x1(iax)-crpix(iax,icrd1)).gt.0.1d0) return
        enddo

      else
        call bug('f','Unrecognised match operation in coCompar')
      endif

      coCompar = .true.

      end

c***********************************************************************

c* coFindAx -- Locate a particular axis in the coordinate system.
c& rjs
c: coordinates
c+
      subroutine coFindAx(lu,axis,iax)

      integer   lu
      character axis*(*)
      integer   iax
c  ---------------------------------------------------------------------
c  Locate an axis of a particular type.  The type of the axis is given
c  by the "axis" parameter.
c
c  Input:
c    lu         Handle of the coordinate system.
c    axis       This can be either.  Case is unimportant.
c               Often this will be a normal FITS-style ctype value, with
c               or without the projection/rest-frame part.
c               If the projection/rest-frame part is omitted, "axis"
c               will match all projections/rest-frames.  For example,
c               axis = 'RA' matches 'RA---NCP' and 'RA---SIN' whereas
c               axis = 'RA---SIN' only matches 'RA---SIN'.
c
c               It can also be one of:
c                 'longitude'  for RA,  GLON and ELON axes.
c                 'latitude'   for DEC, GLAT and ELAT axes.
c                 'spectral'   for FREQ, VELO and FELO axes.
c                 'frequency'  as above, but only if there is enough
c                              information to convert to frequency.
c
c  Output:
c    iax        The axis index of the desired axis.  A value of 0 is
c               returned if the axis is not found.
c-----------------------------------------------------------------------
      include 'co.h'

      logical   match
      integer   icrd, jax, length
      character axtype*16

      external  coLoc, len1
      integer   coLoc, len1
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      axtype = axis
      call ucase(axtype)
      length = len1(axis)

c     Do the special cases.
      iax = 0
      if (axtype.eq.'SPECTRAL') then
        iax = spcax(icrd)

      else if (axtype.eq.'FREQUENCY') then
        iax = spcax(icrd)
        if (iax.gt.0 .and. restfrq(icrd).le.0d0 .and.
     *      cotype(iax,icrd).ne.FRQTYP) iax = 0

      else if (axtype.eq.'LONGITUDE') then
        iax = lngax(icrd)

      else if (axtype.eq.'LATITUDE') then
        iax = latax(icrd)

      else if (length.gt.len1(ctype(1,icrd))) then
        continue

      else
        do jax = 1, naxis(icrd)
          if (axtype(1:length).eq.ctype(jax,icrd)(1:length)) then
            match = length.eq.len(ctype(jax,icrd))
            if (.not.match) then
              match = ctype(jax,icrd)(length+1:).eq.' ' .or.
     *                ctype(jax,icrd)(length+1:length+1).eq.'-'
            endif

            if (match .and. iax.ne.0) call bug('f',
     *        'Multiple matching axes in coFindAx, for axis='//axtype)
            if (match) iax = jax
          endif
        enddo
      endif

      end

c***********************************************************************

      subroutine coCtype(ctypei, axtype, wtype, algo, units)

      character ctypei*(*), axtype*(*)
      character wtype*(*), algo*(*), units*(*)
c-----------------------------------------------------------------------
c  Parse a world coordinate ctype returning the generic type and other
c  information.
c
c  Input:
c    ctypei     The FITS-style ctype of the world coordinate.  AIPS-
c               convention spectral types will be translated.
c  Output:
c    axtype     Generic world coordinate type:
c                 'linear'     for simple linear axes other than
c                              celestial or spectral.
c                 'longitude'  for RA,  GLON and ELON axes.
c                 'latitude'   for DEC, GLAT and ELAT axes.
c                 'spectral'   for spectral axes.
c    wtype      World coordinate type, i.e. ctype with the algorithm
c               code stripped off, e.g. 'RA---NCP' yields 'RA', and
c               'VOPT-F2W' yields 'VOPT'.
c    algo       World coordinate algorithm code, e.g. 'RA---NCP' yields
c               'NCP', and 'VOPT-F2W' yields 'F2W'.  Blank for linear
c               axes.
c    units      Miriad internal units for this world coordinate in a
c               form suitable for reporting, e.g. 'VOPT-F2W' would
c               yield 'km/s'.
c-----------------------------------------------------------------------
      integer    NWTYPE, NPCODE
      parameter (NWTYPE = 20, NPCODE = 29)

      integer   i, k
      character axtypes(NWTYPE)*9, cunits(NWTYPE)*6, pcodes(NPCODE)*3,
     *          umsg*64, wtypes(NWTYPE)*9

      external  binsrcha, len1
      integer   binsrcha, len1

c     Recognised wtypes; this list MUST be in alphabetic order.
      data (wtypes(i),axtypes(i),cunits(i),i=1,NWTYPE)/
     *  'ANGLE    ', 'linear   ', 'rad   ',
     *  'DEC      ', 'latitude ', 'rad   ',
     *  'ELAT     ', 'latitude ', 'rad   ',
     *  'ELON     ', 'longitude', 'rad   ',
     *  'FELO     ', 'spectral ', 'km/s  ',
     *  'FELOCITY ', 'spectral ', 'km/s  ',
     *  'FREQ     ', 'spectral ', 'GHz   ',
     *  'FREQUENCY', 'spectral ', 'GHz   ',
     *  'GLAT     ', 'latitude ', 'rad   ',
     *  'GLON     ', 'longitude', 'rad   ',
     *  'LL       ', 'longitude', 'rad   ',
     *  'MM       ', 'latitude ', 'rad   ',
     *  'POINTING ', 'linear   ', '      ',
     *  'RA       ', 'longitude', 'rad   ',
     *  'SDBEAM   ', 'linear   ', '      ',
     *  'STOKES   ', 'linear   ', '      ',
     *  'UU       ', 'linear   ', 'lambda',
     *  'VELO     ', 'spectral ', 'km/s  ',
     *  'VELOCITY ', 'spectral ', 'km/s  ',
     *  'VV       ', 'linear   ', 'lambda'/

c     Recognized celestial projection codes.
      data pcodes /
     *  'AZP', 'SZP', 'TAN', 'STG', 'SIN', 'NCP', 'ARC', 'ZPN',
     *  'ZEA', 'AIR', 'CYP', 'CEA', 'CAR', 'MER', 'COP', 'COE',
     *  'COD', 'COO', 'SFL', 'GLS', 'PAR', 'MOL', 'AIT', 'BON',
     *  'PCO', 'TSC', 'CSC', 'QSC', 'HPX'/
c-----------------------------------------------------------------------
c     Assume linear unless recognized as otherwise.
      axtype = 'linear'
      wtype  = ' '
      algo   = ' '
      units  = ' '

      if (ctypei.eq.' ') return

c     Parse ctype.
      call coExt(ctypei, wtype, algo)

      i = binsrcha(wtype, wtypes, NWTYPE)

      if (i.eq.0) then
c       Unrecognized, assumed linear.
        wtype = ctypei
        algo  = ' '

        k = len1(ctypei)
        umsg = 'Assuming the '//ctypei(:k)//' axis is linear.'
        call bug('w', umsg)
        return
      endif

      axtype = axtypes(i)
      units  = cunits(i)

      if (axtype.eq.'longitude' .or. axtype.eq.'latitude') then
c       Celestial axis, check the projection code.
        if (algo.eq.' ') then
c         No projection code = simple linear axis, not CAR!  But still
c         needs to be recorded as LNGTYP or LATTYP.
          return
        endif

c       Is it a recognized projection code?
        do i = 1, NPCODE
          if (algo.eq.pcodes(i)) then
c           It's recognized.
            return
          endif
        enddo

c       Unrecognized projection code, revert to linear.
        axtype = 'linear'
        wtype  = ctypei
        algo   = ' '

        k = len1(ctypei)
        umsg = 'Assuming the '//ctypei(:k)//' axis is linear.'
        call bug('w', umsg)
      endif

      end

c***********************************************************************

c* coAxType -- Get information about the world coordinates on an axis.
c& mrc
c: coordinates
c+
      subroutine coAxType(lu,iax,axtype,wtype,algo,units)

      integer   lu, iax
      character axtype*(*), wtype*(*), algo*(*), units*(*)
c  ---------------------------------------------------------------------
c  Get information about the world coordinates on an axis.  Provides a
c  convenient interface to coCtype.
c
c  Input:
c    lu         Handle of the coordinate object.
c  In/out:
c    iax        The axis number.  If zero, then it will be set from
c               axtype.
c    axtype     Generic world coordinate type:
c                 'longitude'  for RA,  GLON and ELON axes.
c                 'latitude'   for DEC, GLAT and ELAT axes.
c                 'spectral'   for spectral axes.
c               Not reset if iax was zero on input.
c  Output:
c    wtype      World coordinate type, i.e. ctype with the algorithm
c               code stripped off, e.g. 'RA---NCP' would yield 'RA', and
c               'VOPT-F2W' would yield 'VOPT'.
c    algo       World coordinate algorithm code, e.g. 'RA---NCP' would
c               yield 'NCP', and 'VOPT-F2W' would yield 'F2W'.  Blank
c               for linear axes.
c    units      Miriad internal units for this world coordinate in a
c               form suitable for reporting, e.g. 'VOPT-F2W' would
c               yield 'km/s'.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd
      character dummy*16

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      axtype = ' '
      wtype  = ' '
      algo   = ' '
      units  = ' '

      if (iax.lt.0 .or. naxis(icrd).lt.iax) return

      if (iax.eq.0) then
        call coFindAx(lu, axtype, iax)
        if (iax.eq.0) return

c       Don't alter axtype - it might be given as a character constant.
        dummy = axtype
        call coCtype(ctype(iax,icrd), dummy, wtype, algo, units)

      else
        call coCtype(ctype(iax,icrd), axtype, wtype, algo, units)
      endif

      end

c***********************************************************************

c* coLin -- Generate a linearized approximation to a nonlinear system.
c& rjs
c: coordinates
c+
      subroutine coLin(lu,in,x1,n,ctypel,crpixl,crvall,cdeltl)

      integer lu,n
      character in*(*),ctypel(n)*(*)
      double precision x1(*),crpixl(n),crvall(n),cdeltl(n)
c  ---------------------------------------------------------------------
c  Generate a linearised approximation to a non-linear coordinate
c  system.
c
c  Input:
c    lu         Handle of the input coordinate system.
c    x1         Input coordinate.  The linearised approximation
c               is performed around this point.
c    in         String describing the format of x1 coordinate, as
c               per coCvt.
c    n          Number of axes to output.
c  Output:
c    ctypel )
c    crpixl )   Output coordinate descriptions.
c    crvall )
c    cdeltl )
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'co.h'

      integer   iax, icrd
      double precision delta, xp(MAXNAX), xp1(MAXNAX), xp2(MAXNAX),
     *          xw(MAXNAX), xw1(MAXNAX), xw2(MAXNAX)

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      if (sinrot(icrd).ne.0d0) then
        call bug('w','Cannot handle sky rotation')
      endif

c     Convert to absolute pixels.
      do iax = 1, naxis(icrd)
        xp(iax) = crpix(iax,icrd)
        xw(iax) = crval(iax,icrd)
      enddo
      call coCvt(lu,in,x1,'ap/...',xp)
      call coCvt(lu,in,x1,'aw/...',xw)

c     Increment by one pixel in all directions.
      do iax = 1, naxis(icrd)
        xp1(iax) = xp(iax)
        xp2(iax) = xp(iax)
      enddo

      do iax = 1, n
        if (iax.le.naxis(icrd)) then
          ctypel(iax) = ctype(iax,icrd)
          crvall(iax) = crval(iax,icrd)
          if (cotype(iax,icrd).eq.LNGTYP .or.
     *        cotype(iax,icrd).eq.LATTYP .or.
     *        cotype(iax,icrd).eq.FELTYP) then
            xp1(iax) = xp(iax) - 1d0
            xp2(iax) = xp(iax) + 1d0
            call coCvt(lu,'ap/...',xp1,'aw/...',xw1)
            call coCvt(lu,'ap/...',xp2,'aw/...',xw2)
            xp1(iax) = xp(iax)
            xp2(iax) = xp(iax)
            cdeltl(iax) = xw2(iax) - xw1(iax)
            delta       = xw(iax) - crvall(iax)

            if (cotype(iax,icrd).eq.LNGTYP) then
              if (cdeltl(iax).lt.-DPI) then
                cdeltl(iax) = cdeltl(iax) + DTWOPI
              endif
              if (cdeltl(iax).gt.DPI) then
                cdeltl(iax) = cdeltl(iax) - DTWOPI
              endif
              if (delta.lt.-DPI) delta = delta + DTWOPI
              if (delta.gt.DPI)  delta = delta - DTWOPI
            endif

            cdeltl(iax) = 0.5d0 * cdeltl(iax)
            crpixl(iax) = xp(iax) - delta/cdeltl(iax)
            if (cotype(iax,icrd).eq.LNGTYP) then
              cdeltl(iax) = cdeltl(iax) * cos(crval(latax(icrd),icrd))
            endif
          else
            cdeltl(iax) = cdelt(iax,icrd)
            crpixl(iax) = crpix(iax,icrd)
          endif
        else
          ctypel(iax) = ' '
          crpixl(iax) = 1d0
          crvall(iax) = 1d0
          cdeltl(iax) = 1d0
        endif
      enddo

      end

c***********************************************************************

c* coAxGet -- Get information about a particular axis.
c& rjs
c: coordinates
c+
      subroutine coAxGet(lu,iax,ctypei,crpixi,crvali,cdelti)

      integer lu,iax
      character ctypei*(*)
      double precision crpixi,crvali,cdelti
c  ---------------------------------------------------------------------
c  Return information about a particular axis.
c
c  Input:
c    lu         Handle of the coordinate system.
c    iax        Axis number.
c  Output:
c    ctypei     Axis type (FITS-style).
c    crpixi     Reference pixel.
c    crvali     Reference value.
c    cdelti     Increment in linear system approximation.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      if (iax.gt.naxis(icrd)) then
        ctypei = ' '
        crpixi = 0d0
        crvali = 0d0
        cdelti = 1d0
      else
        ctypei = ctype(iax,icrd)
        crpixi = crpix(iax,icrd)
        crvali = crval(iax,icrd)
        cdelti = cdelt(iax,icrd)
      endif

      end

c***********************************************************************

c* coGetD -- Get a value from a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coGetD(lu,object,dVal)

      integer lu
      character object*(*)
      double precision dVal
c  ---------------------------------------------------------------------
c  Get a floating point value from a coordinate object.
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c  Output:
c    dVal       The value.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'wcslib/prj.inc'

      integer   iax, icrd, m, prj(PRJLEN), status
      double precision pv(0:29), ref(4)
      character obj*8

      external  coLoc, len1
      integer   coLoc, len1
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Parse parameterized keywords.
      obj = object
      iax = 0
      m   = -1
      if (obj(:1).eq.'c' .and. len1(obj).eq.6) then
        iax = ichar(obj(6:6)) - ichar('0')
        if (1.le.iax .and. iax.le.MAXNAX) then
          obj(6:6) = ' '
        else
          iax = 0
        endif
      else if (obj(:2).eq.'pv') then
        if (len1(obj).eq.3) then
          m = ichar(obj(3:3)) - ichar('0')
          if (0.le.m .and. m.le.9) obj = 'pv'
        else if (len1(obj).eq.4) then
          m = 10*(ichar(obj(3:3)) - ichar('0')) +
     *            ichar(obj(4:4)) - ichar('0')
          if (0.le.m .and. m.le.29) obj = 'pv'
        endif
      endif

      if (obj(1:5).eq.'crpix' .and. iax.gt.0) then
        dVal = crpix(iax,icrd)
      else if (obj(1:5).eq.'cdelt' .and. iax.gt.0) then
        dVal = cdelt(iax,icrd)
      else if (obj(1:5).eq.'crval' .and. iax.gt.0) then
        dVal = crval(iax,icrd)

      else if (obj.eq.'llrot') then
        if (sinrot(icrd).eq.0d0) then
          dVal = 0d0
        else
          dVal = atan2(sinrot(icrd),cosrot(icrd))
        endif

      else if (obj.eq.'lonpole') then
        status = celgtd(cel(1,icrd), CEL_REF, ref)
        dVal = ref(3)
      else if (obj.eq.'latpole') then
        status = celgtd(cel(1,icrd), CEL_REF, ref)
        dVal = ref(4)
      else if (obj.eq.'phi0') then
        status = celgtd(cel(1,icrd), CEL_PHI0, dVal)
      else if (obj.eq.'theta0') then
        status = celgtd(cel(1,icrd), CEL_THETA0, dVal)
      else if (obj.eq.'pv' .and. m.ge.0) then
        status = celgti(cel(1,icrd), CEL_PRJ, prj)
        status = prjgtd(prj, PRJ_PV, pv)
        dVal = pv(m)

      else if (obj.eq.'restfreq') then
        dVal = restfrq(icrd)
      else if (obj.eq.'vobs') then
        dVal = vobs(icrd)
      else if (obj.eq.'epoch') then
        dVal = eqnox(icrd)
      else if (obj.eq.'obstime') then
        dVal = obstime(icrd)
      else
        call bug('f','Unrecognised object in coGetD: '//obj)
      endif

      end

c***********************************************************************

c* coGetI -- Get a value from a coordinate object.
c& mrc
c: coordinates
c+
      subroutine coGetI(lu, object, iVal)

      integer   lu
      character object*(*)
      integer   iVal
c  ---------------------------------------------------------------------
c  Get an integer value from a coordinate object.
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c  Output:
c    iVal       The value.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, status

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Parse parameterized keywords.
      if (object.eq.'naxis') then
        iVal = naxis(icrd)
      else if (object.eq.'xyzero') then
        status = celgti(cel(1,icrd), CEL_OFFSET, iVal)
      else
        call bug('f','Unrecognised object in coGetI: '//object(:8))
      endif

      end

c***********************************************************************

c* coGetA -- Get a value from a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coGetA(lu,object,cVal)

      integer   lu
      character object*(*), cVal*(*)
c  ---------------------------------------------------------------------
c  Get a character value from a coordinate object.
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c  Output:
c    cVal       The value.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd
      character obj*8

      external  coLoc, len1
      integer   coLoc, len1
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Parse parameterized keywords.
      obj = object
      iax = 0
      if (obj(:5).eq.'ctype' .and. len1(obj).eq.6) then
        iax = ichar(obj(6:6)) - ichar('0')
        if (1.le.iax .and. iax.le.MAXNAX) then
          obj(6:6) = ' '
        else
          iax = 0
        endif
      endif

      if (obj.eq.'ctype' .and. iax.gt.0) then
        cVal = ctype(iax,icrd)

      else if (obj.eq.'cellscal') then
        if (frqscl(icrd)) then
          cVal = '1/F'
        else
          cVal = 'CONSTANT'
        endif

      else
        call bug('f','Unrecognised object in coGetA: '//obj)
      endif

      end

c***********************************************************************

c* coAxSet -- Set basic coordinate parameters for a particular axis.
c& rjs
c: coordinates
c+
      subroutine coAxSet(lu,iax,ctypei,crpixi,crvali,cdelti)

      integer lu, iax
      character ctypei*(*)
      double precision crpixi, crvali, cdelti
c  ---------------------------------------------------------------------
c  Set basic coordinate parameters (crpix, cdelt, crval, and ctype) for
c  the specified image axis.  Updates naxis, and initializes parameters
c  for any axes between the old value of naxis and the new.
c
c  This is a convenience routine.  See also coSetD, coSetI, and coSetA
c  for setting other parameters one at a time.
c
c  Input:
c    lu         Handle of the coordinate object.
c    iax        Axis number.
c    ctypei,... FITS-style ctype, crpix, crval, cdelt.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, jax

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      if (iax.lt.1 .or. iax.gt.MAXNAX) then
        call bug('f','Illegal axis number')
      endif

      icrd = coLoc(lu,.false.)

      do jax = naxis(icrd)+1, iax-1
        crpix(jax,icrd) = 0d0
        cdelt(jax,icrd) = 1d0
        crval(jax,icrd) = 0d0
        ctype(jax,icrd) = ' '
      enddo

      naxis(icrd) = max(naxis(icrd),iax)
      crpix(iax,icrd) = crpixi
      cdelt(iax,icrd) = cdelti
      crval(iax,icrd) = crvali
      ctype(iax,icrd) = ctypei

      end

c***********************************************************************

c* coSetD -- Set a value in a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coSetD(lu,object,value)

      integer lu
      character object*(*)
      double precision value
c  ---------------------------------------------------------------------
c  Set a floating point value in a coordinate object.
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c    value      Value to use.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'wcslib/prj.inc'

      integer   iax, icrd, m, prj(PRJLEN), status
      character obj*8

      external  coLoc, len1
      integer   coLoc, len1
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Parse parameterized keywords.
      obj = object
      iax = 0
      m   = -1
      if (obj(:1).eq.'c' .and. len1(obj).eq.6) then
        iax = ichar(obj(6:6)) - ichar('0')
        if (1.le.iax .and. iax.le.MAXNAX) then
          obj(6:6) = ' '
        else
          iax = 0
        endif
      else if (obj(:2).eq.'pv') then
        if (len1(obj).eq.3) then
          m = ichar(obj(3:3)) - ichar('0')
          if (0.le.m .and. m.le.9) obj = 'pv'
        else if (len1(obj).eq.4) then
          m = 10*(ichar(obj(3:3)) - ichar('0')) +
     *            ichar(obj(4:4)) - ichar('0')
          if (0.le.m .and. m.le.29) obj = 'pv'
        endif
      endif

      if (obj.eq.'crpix' .and. iax.gt.0) then
        crpix(iax,icrd) = value
      else if (obj.eq.'cdelt' .and. iax.gt.0) then
        cdelt(iax,icrd) = value
      else if (obj.eq.'crval' .and. iax.gt.0) then
        crval(iax,icrd) = value
      else if (obj.eq.'llrot') then
        cosrot(icrd) = cos(value)
        sinrot(icrd) = sin(value)

      else if (obj.eq.'lonpole') then
        status = celptd(cel(1,icrd), CEL_REF, value, 3)
        defs(1,icrd) = .true.
      else if (obj.eq.'latpole') then
        status = celptd(cel(1,icrd), CEL_REF, value, 4)
        defs(2,icrd) = .true.
      else if (obj.eq.'phi0') then
        status = celptd(cel(1,icrd), CEL_PHI0, value, 0)
        defs(3,icrd) = .true.
      else if (obj.eq.'theta0') then
        status = celptd(cel(1,icrd), CEL_THETA0, value, 0)
        defs(4,icrd) = .true.
      else if (obj.eq.'pv' .and. m.ge.0) then
        status = celgti(cel(1,icrd), CEL_PRJ, prj)
        status = prjptd(prj, PRJ_PV, value, m)
        status = celpti(cel(1,icrd), CEL_PRJ, prj, 0)

      else if (obj.eq.'restfreq') then
        restfrq(icrd) = value
      else if (obj.eq.'vobs') then
        vobs(icrd) = value
      else if (obj.eq.'epoch') then
        eqnox(icrd) = value
      else if (obj.eq.'obstime') then
        obstime(icrd) = value
      else
        call bug('f','Unrecognised object in coSetD: '//obj)
      endif

      end

c***********************************************************************

c* coSetI -- Set a value in a coordinate object.
c& mrc
c: coordinates
c+
      subroutine coSetI(lu,object,value)

      integer lu
      character object*(*)
      integer value
c  ---------------------------------------------------------------------
c  Set an integer value in a coordinate object.
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c    value      Value to use.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, status

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      if (object.eq.'xyzero') then
        status = celpti(cel(1,icrd), CEL_OFFSET, value, 0)
      else
        call bug('f','Unrecognised object in coSetI: '//object(:8))
      endif

      end

c***********************************************************************

c* coSetA -- Set a value in a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coSetA(lu,object,value)

      integer lu
      character object*(*),value*(*)
c  ---------------------------------------------------------------------
c  Set a character value in a coordinate object.
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c    value      Value to use.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd
      character obj*8

      external  coLoc, len1
      integer   coLoc, len1
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Parse parameterized keywords.
      obj = object
      iax = 0
      if (obj(:5).eq.'ctype' .and. len1(obj).eq.6) then
        iax = ichar(obj(6:6)) - ichar('0')
        if (1.le.iax .and. iax.le.MAXNAX) then
          obj(6:6) = ' '
        else
          iax = 0
        endif
      endif

      if (obj.eq.'ctype' .and. iax.gt.0) then
        ctype(iax,icrd) = value

      else if (obj.eq.'cellscal') then
        if (value.eq.'CONSTANT') then
          frqscl(icrd) = .false.
        else if (value.eq.'1/F') then
          frqscl(icrd) = .true.
        else
          call bug('f','Unrecognised value for cellscal in coSetA')
        endif
      else
        call bug('f','Unrecognised object in coSetA: '//obj)
      endif

      end

c***********************************************************************

c* coCpyD -- Copy a value from one coordinate object to another.
c& mrc
c: coordinates
c+
      subroutine coCpyD(lu1, lu2, object)

      integer   lu1, lu2
      character object*(*)
c  ---------------------------------------------------------------------
c  Copy a floating point value from one coordinate object to another.
c
c  Input:
c    lu1        Handle of the source coordinate object.
c    lu2        Handle of the destination coordinate object.
c    object     Name of the thing to set.
c-----------------------------------------------------------------------
      double precision dVal
c-----------------------------------------------------------------------
      call coGetD(lu1, object, dVal)
      call coSetD(lu2, object, dVal)

      end

c***********************************************************************

c* coCpyI -- Copy a value from one coordinate object to another.
c& mrc
c: coordinates
c+
      subroutine coCpyI(lu1, lu2, object)

      integer   lu1, lu2
      character object*(*)
c  ---------------------------------------------------------------------
c  Copy an integer value from one coordinate object to another.
c
c  Input:
c    lu1        Handle of the source coordinate object.
c    lu2        Handle of the destination coordinate object.
c    object     Name of the thing to set.
c-----------------------------------------------------------------------
      integer   iVal
c-----------------------------------------------------------------------
      call coGetI(lu1, object, iVal)
      call coSetI(lu2, object, iVal)

      end

c***********************************************************************

c* coCpyA -- Copy a value from one coordinate object to another.
c& mrc
c: coordinates
c+
      subroutine coCpyA(lu1, lu2, object)

      integer   lu1, lu2
      character object*(*)
c  ---------------------------------------------------------------------
c  Copy a character value from one coordinate object to another.
c
c  Input:
c    lu1        Handle of the source coordinate object.
c    lu2        Handle of the destination coordinate object.
c    object     Name of the thing to set.
c-----------------------------------------------------------------------
      character cVal*128
c-----------------------------------------------------------------------
      call coGetA(lu1, object, cVal)
      call coSetA(lu2, object, cVal)

      end

c***********************************************************************

c* coPrjSet -- Set celestial projection type.
c& rjs
c: coordinates
c+
      subroutine coPrjSet(lu, code, npv, pv)

      integer lu, npv
      double precision pv(30)
      character code*(*)
c  ---------------------------------------------------------------------
c  Set the coordinate system celestial projection type.
c
c  Input:
c    lu         Handle of the coordinate system.
c    code       Projection code: NCP, SIN, etc. or blank, which on
c               interpretation will be translated to CAR.  Can also be
c               '-' to leave it alone in case all you want to do is set
c               projection parameters.
c    npv        Number of elements in pv.
c    pv         Array of projection parameters.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'wcslib/prj.inc'

      integer   iax, icrd, ipv, m, prj(PRJLEN), status
      character ctypei*16, pcode*3

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      pcode = code
      call ucase(pcode)

      if (pcode.eq.' ') then
        do iax = 1, naxis(icrd)
          ctypei = ctype(iax,icrd)(:5)

          if (ctypei.eq.'RA---' .or. ctypei.eq.'DEC--' .or.
     *        ctypei.eq.'ELON-' .or. ctypei.eq.'ELAT-' .or.
     *        ctypei.eq.'GLON-' .or. ctypei.eq.'GLAT-') then

            if (ctypei(3:).eq.'-') then
              ctypei(3:) = ' '
            else if (ctypei(4:).eq.'-') then
              ctypei(4:) = ' '
            else if (ctypei(5:).eq.'-') then
              ctypei(5:) = ' '
            endif

            ctype(iax,icrd) = ctypei
          endif
        enddo

      else if (pcode.ne.'-') then
        do iax = 1, naxis(icrd)
          ctypei = ctype(iax,icrd)

          if (ctypei(3:).eq.' ') then
            ctypei(3:) = '---'
          else if (ctypei(4:).eq.' ') then
            ctypei(4:) = '--'
          else if (ctypei(5:).eq.' ') then
            ctypei(5:) = '-'
          endif

          if (ctypei(:5).eq.'RA---' .or. ctypei(:5).eq.'DEC--' .or.
     *        ctypei(:5).eq.'ELON-' .or. ctypei(:5).eq.'ELAT-' .or.
     *        ctypei(:5).eq.'GLON-' .or. ctypei(:5).eq.'GLAT-') then
            ctype(iax,icrd) = ctypei(:5) // pcode
          endif
        enddo

      endif

c     Projection parameters.
      if (npv.gt.0) then
        status = celgti(cel(1,icrd), CEL_PRJ, prj)

c       Reset them all.
        do m = 0, 29
          status = prjptd(prj, PRJ_PV, 0d0, m)
        end do

        do ipv = 1, npv
          if (pcode.eq.'ZPN') then
c           ZPN's first parameter has m == 0.
            m = ipv - 1
          else
c           The others start with m == 1.
            m = ipv
          endif

          status = prjptd(prj, PRJ_PV, pv(ipv), m)
        enddo

        status = celpti(cel(1,icrd), CEL_PRJ, prj, 0)
      endif

      end

c***********************************************************************

c* coAltPrj -- Translate a non-standard CAR projection.
c& mrc
c: coordinates
c+
      subroutine coAltPrj(lu)
      include 'mirconst.h'

      integer lu
c  ---------------------------------------------------------------------
c  Translate the oft-used, non-standard implementation of the CAR
c  projection.  This is essentially a simple linear axis pair except
c  with an implicit cos(lat0) rescaling on longitude.
c
c  The coordinate object should already have been initialized by a call
c  to coInit or coReinit, or have been created by coDup or coRaDec.  It
c  will be reinitialized if necessary.
c
c  This routine could also be extended to handle the non-standard
c  definition of the AIT and MER projections given by AIPS memo 46.
c  Note that there is no contention for NCP or GLS which are handled as
c  first-class projections by coReinit, i.e. not translated to SIN or
c  SFL (except within WCSLIB).
c
c  Input:
c    lu         Handle of the coordinate object.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, ilat, ilng
      double precision lat0
      character lat*8, pcode*3

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Do we have a longitude/latitude axis pair?
      ilng = lngax(icrd)
      ilat = latax(icrd)
      if (ilng.ne.0 .and. ilat.ne.0) then
        lat0 = crval(ilat,icrd)

        if (lat0.ne.0d0) then
c         Get the projection code.
          call coExt(ctype(ilat,icrd), lat, pcode)

          if (pcode.eq.'CAR') then
c           Transfer the implicit cos(lat) rescaling to cdelt itself.
            cdelt(ilng,icrd) = cdelt(ilng,icrd) / cos(lat0)

c           Shift the reference point.
            crpix(ilat,icrd) = crpix(ilat,icrd) - lat0/cdelt(ilat,icrd)
            crval(ilat,icrd) = 0d0

            call coReinit(lu)
          endif
        endif
      endif

      end

c***********************************************************************

c* coSpcSet -- Change the spectral axis to the specified type.
c& rjs
c: coordinates
c+

c WARNING, THIS SUBROUTINE IS UNDER DEVELOPMENT, THE API MAY CHANGE!
      subroutine coSpcSet(lu, stype, frame, ispc, algo)
c WARNING, THIS SUBROUTINE IS UNDER DEVELOPMENT, THE API MAY CHANGE!

      integer   lu
      character stype*(*), frame*(*)
      integer   ispc
      character algo*3
c  ---------------------------------------------------------------------
c  Switch the spectral axis from its current type to another spectral
c  type.
c
c  Input:
c    lu         Handle of the coordinate system.
c    stype      Spectral world coordinate type to switch to (case-
c               insensitive):
c                 FREQ: frequency
c                 VRAD: radio velocity (frequency-like)
c                 VOPT: optical velocity (wavelength-like)
c               These may be suffixed with '-' and a spectral algorithm
c               code (see below) which is ignored.
c
c               stype may also be specified as 'VELOCITY' to switch from
c               frequency to VRAD but leave VRAD and VOPT unchanged.
c
c               stype may also be blank to revert to the spectral type
c               specified in the image header.
c
c               For backwards compatibility, the following are also
c               recognised: '{FREQ,VELO,FELO}{,-{OBS,HEL,LSR}}' meaning
c               'FREQ', 'VRAD', and 'VOPT'.  Likewise, 'FREQUENCY',
c               'RADIO', and 'OPTICAL'.
c
c    frame      Spectral frame: 'TOPOCENTRIC', 'BARYCENTRIC', or 'LSRK',
c               but only conversions between the last two are supported.
c
c               frame may be blank to leave it unspecified or leave it
c               as is, i.e. no Doppler shift, except that, for backwards
c               compatibility, if stype was specified as one of
c               '{FREQ,VELO,FELO}-{OBS,HEL,LSR}' then frame will be set
c               from that.
c
c  Output:
c    ispc       Spectral axis number.
c    algo       Three-letter spectral algorithm code.  Currently only
c               returned as blank which means linear.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      double precision CKMS
      parameter (CKMS = DCMKS * 1d-3)

      integer   icrd, ilat, ilng, itype, otype
      double precision df, frq, vel
      character ctype1*16, ctype2*16, iframe*4, num*2, oframe*4,
     *          ttype*16

      external  coLoc, itoaf
      integer   coLoc
      character itoaf*2
c-----------------------------------------------------------------------
c     Identify the spectral axis.
      icrd = coLoc(lu,.false.)
      ispc = spcax(icrd)
      algo = ' '
      if (ispc.eq.0) return

      ttype = stype
      call ucase(ttype)

c     Reset to header type?
      if (ttype.eq.' ') then
        num = itoaf(ispc)
        call rdhda(lu, 'ctype'//num, ttype, ' ')
      endif

c     Compatibility between the old and new API.  As an interim measure
c     we continue to do it the old way.
      itype = cotype(ispc,icrd)
      if (ttype.eq.'FREQUENCY') then
        ttype = 'FREQ'
      else if (ttype(:4).eq.'VRAD' .or. ttype.eq.'RADIO') then
        ttype = 'VELO'
      else if (ttype(:4).eq.'VOPT' .or. ttype.eq.'OPTICAL') then
        ttype = 'FELO'
      else if (ttype.eq.'VELOCITY') then
        if (itype.eq.FELTYP) then
          ttype = 'FELO'
        else
          ttype = 'VELO'
        endif
      endif

c     Determine the type.
      if (ttype(1:4).eq.'FREQ') then
        otype = FRQTYP
      else if (ttype(1:4).eq.'VELO') then
        otype = VELTYP
      else if (ttype(1:4).eq.'FELO') then
        otype = FELTYP
      else
        call bug('f','Unrecognised conversion type in coSpcSet')
      endif

c     Determine the reference frame conversion.
      if (specsys(icrd).eq.'TOPOCENT') then
        iframe = '-OBS'
      else if (specsys(icrd).eq.'BARYCENT') then
        iframe = '-HEL'
      else if (specsys(icrd).eq.'LSRK') then
        iframe = '-LSR'
      else if (ctype(ispc,icrd)(5:).eq.'-OBS' .or.
     *         ctype(ispc,icrd)(5:).eq.'-HEL' .or.
     *         ctype(ispc,icrd)(5:).eq.'-LSR') then
        iframe = ctype(ispc,icrd)(5:8)
      else
        iframe = ' '
      endif

      if (frame.eq.'TOPOCENT') then
        oframe = '-OBS'
      else if (frame.eq.'BARYCENT') then
        oframe = '-HEL'
      else if (frame.eq.'LSRK') then
        oframe = '-LSR'
      else if (ttype(5:).eq.'-OBS' .or.
     *         ttype(5:).eq.'-HEL' .or.
     *         ttype(5:).eq.'-LSR') then
        oframe = ttype(5:)
      else
        oframe = ' '
      endif
      if (oframe.eq.' ') oframe = iframe
      if (iframe.eq.' ') iframe = oframe

      if (otype.eq.itype .and. oframe.eq.iframe) then
c       Nothing to do.
        return
      endif

      if (restfrq(icrd).le.0d0)
     *  call bug('f','Unable to do axis conversion as restfreq==0')

c     Determine the frequency of the reference pixel
      if (itype.eq.FELTYP) then
        frq = restfrq(icrd) / (1d0 + crval(ispc,icrd)/CKMS)
        df  = -(cdelt(ispc,icrd)/CKMS) * frq * (frq/restfrq(icrd))
        frq = frq - restfrq(icrd)*vobs(icrd)/CKMS
      else if (itype.eq.VELTYP) then
        frq = restfrq(icrd) * (1d0 - crval(ispc,icrd)/CKMS)
     *          - restfrq(icrd)*vobs(icrd)/CKMS
        df  = -(cdelt(ispc,icrd)/CKMS) * restfrq(icrd)
      else
        frq = crval(ispc,icrd)
        df  = cdelt(ispc,icrd)
      endif

      if (oframe.ne.iframe) then
c       Determine the relative velocity between Doppler frames.
        if ((iframe.eq.'-HEL' .and. oframe.eq.'-LSR') .or.
     *      (oframe.eq.'-HEL' .and. iframe.eq.'-LSR')) then
          ilng = lngax(icrd)
          ilat = latax(icrd)
          if (ilng.eq.0 .or. ilat.eq.0) call bug('f',
     *      'Missing RA/DEC -- unable to do velocity frame conversion')

          ctype1 = ctype(ilng,icrd)
          ctype2 = ctype(ilat,icrd)
          if ((ctype1.eq.'RA'  .or. ctype1(1:4).eq.'RA--') .and.
     *        (ctype2.eq.'DEC' .or. ctype2(1:4).eq.'DEC-')) then
            call coGetVel(crval(ilng,icrd),crval(ilat,icrd),eqnox(icrd),
     *                    vel)
          else
            call bug('f', 'Missing RA/DEC' //
     *        ' -- unable to do velocity frame conversion')
          endif

          if (oframe.eq.'-HEL') then
            vobs(icrd) = vobs(icrd) - vel
          else
            vobs(icrd) = vobs(icrd) + vel
          endif
        else
          call bug('f','Unable to convert between '//iframe(2:4)//
     *    ' and '//oframe(2:4)//' velocity frames')
        endif
      endif

c     Perform the transformation.
      if (otype.eq.FELTYP) then
        frq = frq + restfrq(icrd)*vobs(icrd)/CKMS
        crval(ispc,icrd) =  CKMS*(restfrq(icrd)/frq - 1d0)
        cdelt(ispc,icrd) = -CKMS*(df/frq)*(restfrq(icrd)/frq)
        ctype(ispc,icrd) = 'FELO' // oframe
      else if (otype.eq.VELTYP) then
        frq = frq + restfrq(icrd)*vobs(icrd)/CKMS
        crval(ispc,icrd) =  CKMS*(1d0 - frq/restfrq(icrd))
        cdelt(ispc,icrd) = -CKMS*(df/restfrq(icrd))
        ctype(ispc,icrd) = 'VELO' // oframe
      else
        crval(ispc,icrd) = frq
        cdelt(ispc,icrd) = df
        ctype(ispc,icrd) = 'FREQ' // oframe
      endif
      cotype(ispc,icrd) = otype

      end

c***********************************************************************

      subroutine coVelSet(lu, stype)
      integer   lu
      character stype*(*)
c-----------------------------------------------------------------------
c  Maintained for backwards compatibility only, do not use it.
c-----------------------------------------------------------------------
      integer   ispc
      character algo*3
c-----------------------------------------------------------------------
      call coSpcSet(lu, stype, ' ', ispc, algo)

      if (ispc.eq.0) then
        call bug('f','Call to coVelSet for non-spectral axis')
      endif

      end

c***********************************************************************

c* coFreq -- Convert spectral coordinates to frequency.
c& rjs
c: coordinates
c+
      subroutine coFreq(lu,in,x1,freq)

      integer   lu
      character in*(*)
      double precision x1(*), freq
c  ---------------------------------------------------------------------
c  Get the frequency corresponding to a particular coordinate.
c
c  Input:
c    lu         Handle of the coordinate object.
c    in         As with coCvt
c    x1         As with coCvt
c  Output:
c    freq       The frequency.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      double precision CKMS
      parameter (CKMS = DCMKS * 1d-3)

      integer   icrd, ispc, itype
      double precision x2(MAXNAX)

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Check validity.
      ispc = spcax(icrd)
      if (ispc.eq.0)
     *  call bug('f','No spectral coordinate axis in coFreq')

      if (restfrq(icrd).eq.0d0 .and. cotype(ispc,icrd).ne.FRQTYP)
     *  call bug('f','Can''t convert to frequency in coFreq')

c     Convert the user's coordinate to absolute world coordinates.
c     Fill in the reference location in the output, just in case the
c     user was silly enough not to give enough inputs.
      x2(ispc) = crval(ispc,icrd)
      call coCvt(lu, in, x1, 'aw/...', x2)
      freq = x2(ispc)

c     Convert from velocities
      itype = cotype(ispc,icrd)
      if (itype.eq.FRQTYP) then
        continue
      else if (itype.eq.FELTYP) then
        freq = restfrq(icrd) / (1d0 + freq/CKMS)
     *                - restfrq(icrd)*vobs(icrd)/CKMS
      else if (itype.eq.VELTYP) then
        freq = restfrq(icrd) * (1d0 - freq/CKMS)
     *                - restfrq(icrd)*vobs(icrd)/CKMS
      else
        call bug('f','Something is screwy, in coFreq')
      endif

      end

c***********************************************************************

c* coCvt1 -- Do coordinate conversion on one axis only.
c& rjs
c: coordinates
c+
      subroutine coCvt1(lu,iax,in,x1,out,x2)

      integer   lu, iax
      double precision x1, x2
      character in*(*), out*(*)
c  ---------------------------------------------------------------------
c  Do coordinate conversion on one axis only.
c
c  Input:
c    lu         Handle of the coordinate system.
c    iax        Axis number.
c    in,out     These indicate the conversion to be performed.  See the
c               prologue of coCrack.
c    x1         Input coordinate.
c  Output:
c    x2         Output, converted, coordinate.
c-----------------------------------------------------------------------
      include 'co.h'

      logical   valid, x1off, x1pix, x2off, x2pix
      integer   icrd, ilat, ilng, n
      double precision dtemp, bscal, bzero

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      if (iax.lt.1) call bug('f','Invalid axis in coCvt1')
      x2 = x1
      if (iax.gt.naxis(icrd)) return

      call coCrack(1,in,x1pix,x1off,1,n)
      if (n.ne.1) call bug('f','Invalid conversion in coCvt1')
      call coCrack(1,out,x2pix,x2off,1,n)
      if (n.ne.1) call bug('f','Invalid conversion in coCvt1')

      valid = .true.

      if (cotype(iax,icrd).eq.LINEAR .or.
     *    cotype(iax,icrd).eq.VELTYP .or.
     *    cotype(iax,icrd).eq.FRQTYP) then
c       Convert a linear axis.
        call coLinear(crval(iax,icrd),crpix(iax,icrd),cdelt(iax,icrd),
     *        x1pix,x1off,x2pix,x2off,bscal,bzero)
        x2 = bscal * x1 + bzero

      else if (cotype(iax,icrd).eq.LNGTYP) then
c       Convert a longitude axis.
        ilng = lngax(icrd)
        ilat = latax(icrd)
        call coCel(cel(1,icrd), x1,0d0,x2,dtemp,
     *    cosrot(icrd),sinrot(icrd),
     *    crpix(ilng,icrd),cdelt(ilng,icrd),
     *    crpix(ilat,icrd),cdelt(ilat,icrd),
     *    x1pix,x1pix,x1off,.true.,
     *    x2pix,x2pix,x2off,.true.,valid)

      else if (cotype(iax,icrd).eq.LATTYP) then
c       Convert a latitude axis.
        ilng = lngax(icrd)
        ilat = latax(icrd)
        call coCel(cel(1,icrd), 0d0,x1,dtemp,x2,
     *    cosrot(icrd),sinrot(icrd),
     *    crpix(ilng,icrd),cdelt(ilng,icrd),
     *    crpix(ilat,icrd),cdelt(ilat,icrd),
     *    x1pix,x1pix,.true.,x1off,
     *    x2pix,x2pix,.true.,x2off,valid)

      else if (cotype(iax,icrd).eq.FELTYP) then
c       Convert a FELO axis.
        call coFelo(x1,x2,
     *    crpix(iax,icrd),cdelt(iax,icrd),crval(iax,icrd),
     *    x1pix,x1off, x2pix,x2off)
      endif

      if (.not.valid) call bug('f',
     *  'An invalid coordinate conversion was requested in coCvt1')

      end

c***********************************************************************

c* coCvt -- Convert coordinates, die on error.
c& mrc
c: coordinates
c+
      subroutine coCvt(lu,in,x1,out,x2)

      integer lu
      character in*(*),out*(*)
      double precision x1(*),x2(*)
c  ---------------------------------------------------------------------
c  Call coCvtv and die immediately if the conversion is invalid.  This
c  interface exists for backwards compatibility only.  It should not be
c  used in new code.
c-----------------------------------------------------------------------
      logical valid
c-----------------------------------------------------------------------
      call coCvtv(lu, in, x1, out, x2, valid)
      if (.not.valid) then
        call bug('f', 'Invalid coordinate conversion in coCvt')
      endif

      end

c***********************************************************************

c* coCvtv -- Convert coordinates - with validation.
c& rjs
c: coordinates
c+
      subroutine coCvtv(lu,in,x1,out,x2,valid)

      integer lu
      character in*(*),out*(*)
      double precision x1(*),x2(*)
      logical valid
c  ---------------------------------------------------------------------
c  Convert coordinates from one coordinate system to another.  Input and
c  output coordinates can be either "pixel" or "world" coordinates, and
c  either "absolute" or "offset".
c
c  "World" coordinates are the normal physical coordinates.  They are
c  given in radians (for astronomical positions), GHz (frequencies),
c  km/s (velocities) and lambda (U-V axes).
c
c  Pixel coordinates are fairly conventional.
c
c  For world coordinates, absolute and offset values differ only by the
c  reference world value (crval).  The exception is longitude-type axes
c  (e.g. RA), where offset coordinates are offsets on the sky -- that is
c  the offsets are multiplied by the cos(latitude) (cos(DEC)) term.
c
c  For pixel coordinates, absolute and offset values differ by reference
c  pixel value (crpix).
c
c  For visibility datasets (where the axes are simply RA and DEC),
c  "pixel coordinates" are defined as those that would result from
c  imaging (using a conventional 2D Fourier transform algorithm) the
c  data-set with a cell size of 1 radian.  Additionally the reference
c  pixel values (crpix) are set to 0.  This means that the "absolute
c  pixel" and "offset pixel" coordinates are identical, and that "offset
c  pixel" and "offset world" coordinates differ only by the inaccuracy
c  in the normal small angle approximation for offsets in RA and DEC.
c
c  Input:
c    in         A character string indicating the input coordinate
c               types.  See the prologue of coCrack.
c    x1         Input coordinate elements of type specified by the 'in'
c               parameter.  The dimensionality must agree with 'in'.
c    out        Output coordinate codes, in the same fashion as 'in',
c               corresponding one-for-one with the inputs.
c  Output:
c    x2         Output coordinate elements of type specified by 'out'.
c    valid      If true, all is OK, otherwise the coordinate conversion
c               requested was invalid.
c-----------------------------------------------------------------------
      include 'co.h'

      logical   docelest, x1off(MAXNAX), x1pix(MAXNAX), x2off(MAXNAX),
     *          x2pix(MAXNAX)
      integer   iax, icrd, ilat, ilng, ispc, n, nt
      double precision bscal, bzero, fqfac, temp

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
c     Determine the operation to be performed.
      icrd = coLoc(lu,.false.)
      call coCrack(MAXNAX,in,x1pix,x1off,naxis(icrd),n)
      call coCrack(MAXNAX,out,x2pix,x2off,n,nt)
      docelest = .false.
      valid = .true.

c     Convert each of the axes.
      do iax = 1, nt
        if (iax.gt.naxis(icrd)) then
          if (iax.le.n) then
            x2(iax) = x1(iax)
          else
            x2(iax) = 0d0
          endif

        else if (iax.gt.n) then
          if (x2off(iax)) then
            x2(iax) = 0d0
          else if (x2pix(iax)) then
            x2(iax) = crpix(iax,icrd)
          else
            x2(iax) = crval(iax,icrd)
          endif

        else if (cotype(iax,icrd).eq.LINEAR .or.
     *           cotype(iax,icrd).eq.VELTYP .or.
     *           cotype(iax,icrd).eq.FRQTYP) then
          call coLinear(crval(iax,icrd),crpix(iax,icrd),cdelt(iax,icrd),
     *        x1pix(iax),x1off(iax),x2pix(iax),x2off(iax),bscal,bzero)
          x2(iax) = bscal * x1(iax) + bzero

        else if (cotype(iax,icrd).eq.LNGTYP .or.
     *           cotype(iax,icrd).eq.LATTYP) then
          docelest = .true.

        else if (cotype(iax,icrd).eq.FELTYP) then
          call coFelo(x1(iax),x2(iax),
     *      crpix(iax,icrd),cdelt(iax,icrd),crval(iax,icrd),
     *      x1pix(iax),x1off(iax), x2pix(iax),x2off(iax))

        else
          call bug('f','Internal software bug in coCvt')
        endif
      enddo

      if (docelest) then
        ilng = lngax(icrd)
        ilat = latax(icrd)
        ispc = spcax(icrd)

c       Determine the frequency-dependent scale factor.
        if (ispc.eq.0 .or. ispc.gt.n .or. .not.frqscl(icrd)) then
          fqfac = 1d0
        else
          call coFqFac(ctype(ispc,icrd), x1(ispc), crpix(ispc,icrd),
     *      cdelt(ispc,icrd),crval(ispc,icrd),vobs(icrd),
     *      x1pix(ispc),x1off(ispc),fqfac)
        endif

c       Do the conversion.  If longitude or latitude is missing use the
c       reference pixel as the corresponding value.
        if (ilng.le.n .and. ilat.le.n) then
          call coCel(cel(1,icrd), x1(ilng),x1(ilat),x2(ilng),
     *      x2(ilat), cosrot(icrd),sinrot(icrd),
     *      crpix(ilng,icrd),fqfac*cdelt(ilng,icrd),
     *      crpix(ilat,icrd),fqfac*cdelt(ilat,icrd),
     *      x1pix(ilng),x1pix(ilat),x1off(ilng),x1off(ilat),
     *      x2pix(ilng),x2pix(ilat),x2off(ilng),x2off(ilat),valid)
        else if (ilat.le.n) then
          call coCel(cel(1,icrd), 0d0,x1(ilat),temp,x2(ilat),
     *      cosrot(icrd),sinrot(icrd),
     *      crpix(ilng,icrd),fqfac*cdelt(ilng,icrd),
     *      crpix(ilat,icrd),fqfac*cdelt(ilat,icrd),
     *      .true.,x1pix(ilat),.true.,x1off(ilat),
     *      .true.,x2pix(ilat),.true.,x2off(ilat),valid)
        else
          call coCel(cel(1,icrd), x1(ilng),0d0,x2(ilng),temp,
     *      cosrot(icrd),sinrot(icrd),
     *      crpix(ilng,icrd),fqfac*cdelt(ilng,icrd),
     *      crpix(ilat,icrd),fqfac*cdelt(ilat,icrd),
     *      x1pix(ilng),.true.,x1off(ilng),.true.,
     *      x2pix(ilng),.true.,x2off(ilng),.true.,valid)
        endif
      endif

      end

c***********************************************************************

c* coGauCvt -- Change gaussian parameters between world and pixel coords
c& rjs
c: coordinates
c+
      subroutine coGauCvt(lu,in,x1,ing,bmaj1,bmin1,bpa1,
     *                             outg,bmaj2,bmin2,bpa2)

      integer lu
      double precision x1(*)
      character in(*),ing*(*),outg*(*)
      real bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2
c  ---------------------------------------------------------------------
c  Convert the parameters that describe a gaussian between pixel and
c  world coordinate systems.  The gaussian lies in the image formed by
c  the first two axes of the coordinate system.
c
c  Input:
c    lu         Handle of the coordinate system.
c    x1         This gives the coordinate of the centroid of the
c               gaussian.
c    in         This gives the units of the input gaussian centroid in
c               the same format as the "in" parameter of coCvt.  See the
c               prologue of coCrack.
c    ing,outg   The conversion to be performed.  Possible values are:
c                 'w'   Input or output is in world units.
c                 'p'   Input or output is in pixel units.
c    bmaj1,bmin1,bpa1  Input gaussian parameters: major axis, minor axis
c               and position angle of major axis.  The position angle is
c               measured from "north" through "east" where north is the
c               direction of increasing value along the second axis,
c               and east is the direction of increasing value along the
c               first axis.  bmaj and bmin will either be in world or
c               pixel units.  bpa will be in radians.
c  Output:
c    bmaj2,bmin2,bpa2 Output gaussian parameters.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      logical   x1off(MAXNAX), x1pix(MAXNAX)
      integer   icrd, ispc, n
      real      cdelt1, cdelt2, cospa, rotn, rotn1, rotn2, sinpa
      double precision alpha, beta, fqfac, gamma, s, t

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
c     Determine the operation to be performed.
      icrd = coLoc(lu,.false.)

      call coCrack(MAXNAX,in,x1pix,x1off,naxis(icrd),n)

c     Get the factors to scale cdelt1 and cdelt2 for this coordinate.
      cdelt1 = cdelt(1,icrd)
      cdelt2 = cdelt(2,icrd)
      if (cotype(1,icrd).eq.LNGTYP .or.
     *    cotype(1,icrd).eq.LATTYP .or.
     *    cotype(2,icrd).eq.LNGTYP .or.
     *    cotype(2,icrd).eq.LATTYP) then
        ispc = spcax(icrd)
        if (ispc.ne.0 .and. ispc.le.n .and. frqscl(icrd)) then
          call coFqFac(ctype(ispc,icrd), x1(ispc), crpix(ispc,icrd),
     *      cdelt(ispc,icrd),crval(ispc,icrd),vobs(icrd),
     *      x1pix(ispc),x1off(ispc), fqfac)

          if (cotype(1,icrd).eq.LNGTYP .or.
     *        cotype(1,icrd).eq.LATTYP) then
            cdelt1 = cdelt1 * fqfac
          endif

          if (cotype(2,icrd).eq.LNGTYP .or.
     *        cotype(2,icrd).eq.LATTYP) then
            cdelt2 = cdelt2 * fqfac
          endif
        endif
      endif

c     Get coordinate grid rotation angle.
      if (abs(sinrot(icrd)).ne.0d0) then
        rotn = atan2(sinrot(icrd),cosrot(icrd))
      else
        rotn = 0.0
      endif
      rotn1 = 0.0
      rotn2 = 0.0
      if (ing.eq.'w')  rotn1 = rotn
      if (outg.eq.'w') rotn2 = rotn

c     Get the increments in a standard form.
      cospa = cos(bpa1+rotn1)
      sinpa = sin(bpa1+rotn1)

      alpha = (cospa/bmin1)**2 + (sinpa/bmaj1)**2
      beta =  (sinpa/bmin1)**2 + (cospa/bmaj1)**2
      gamma = (2d0/(bmin1**2) - 2d0/(bmaj1**2))*cospa*sinpa
      if (ing.eq.outg) then
        continue
      else if (ing.eq.'w' .and. outg.eq.'p') then
        alpha = alpha*(cdelt1*cdelt1)
        beta  = beta *(cdelt2*cdelt2)
        gamma = gamma*(cdelt1*cdelt2)
      else if (ing.eq.'p' .and. outg.eq.'w') then
        alpha = alpha/(cdelt1*cdelt1)
        beta  = beta /(cdelt2*cdelt2)
        gamma = gamma/(cdelt1*cdelt2)
      else
        call bug('f','Unrecognised operation in coGauCvt')
      endif

c     Do the conversion.
      s = alpha + beta
      t = sqrt((alpha-beta)**2 + gamma**2)
      bmin2 = sqrt(2/(s+t))
      bmaj2 = sqrt(2/(s-t))
      if (abs(gamma)+abs(alpha-beta).eq.0d0) then
        bpa2 = 0.0
      else
        bpa2 = 0.5*atan2(gamma,alpha-beta)
      endif
      bpa2 = bpa2 - rotn2

      end

c***********************************************************************

c* coLMN -- Convert celestial coordinates to direction cosines.
c& rjs
c: coordinates
c+
      subroutine coLMN(lu,in,x1,lmn)

      integer lu
      character in*(*)
      double precision x1(*), lmn(3)
c  ---------------------------------------------------------------------
c  Get the direction cosines corresponding to a particular coordinate.
c
c  Input:
c    lu         Handle of the coordinate object.
c    in         As with coCvt
c    x1         As with coCvt
c  Output:
c    lmn        The direction cosines (with respect to the reference
c               position) of the celestial coordinate given by x1.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd
      double precision dec, dec0, ra, ra0, x2(MAXNAX)

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
c     Check validity.
      icrd = coLoc(lu,.false.)
      if (lngax(icrd).eq.0 .or. latax(icrd).eq.0)
     *  call bug('f','Non-celestial coordinate system in coLMN')

c     Convert the users coordinate to absolute world coordinates.
c     Fill in the reference location in the output, just in case the
c     user was silly enough not to give enough inputs.
      ra0  = crval(lngax(icrd),icrd)
      dec0 = crval(latax(icrd),icrd)
      x2(lngax(icrd)) = ra0
      x2(latax(icrd)) = dec0

      call coCvt(lu,in,x1,'aw/...',x2)

      ra  = x2(lngax(icrd))
      dec = x2(latax(icrd))

c     Convert to direction cosines.
      lmn(1) = cos(dec)*sin(ra-ra0)
      lmn(2) = sin(dec)*cos(dec0) - cos(dec)*sin(dec0)*cos(ra-ra0)
      lmn(3) = sin(dec)*sin(dec0) + cos(dec)*cos(dec0)*cos(ra-ra0)

      end

c***********************************************************************

c* coGeom -- Compute linear coefficients to convert geometries.
c& rjs
c: coordinates
c+
      subroutine coGeom(lu,in,x1,ucoeff,vcoeff,wcoeff)

      integer   lu
      character in*(*)
      double precision x1(*), ucoeff(3), vcoeff(3), wcoeff(3)
c  ---------------------------------------------------------------------
c  Compute matrix elements for transforming (u,v,w) coordinates to the
c  specified field centre, for mosaicing, etc.
c
c  Input:
c    lu         Handle of the coordinate object.
c    in         As with coCvt
c    x1         As with coCvt
c  Output:
c    ucoeff,vcoeff Coefficients used to convert (u,v) from one geometry
c               to another.  In particular:
c                 u(corrected) = ucoeff(1)*u(raw) + ucoeff(2)*v(raw) +
c                                  ucoeff(3)*w(raw)
c                 v(corrected) = vcoeff(1)*u(raw) + vcoeff(2)*v(raw) +
c                                  vcoeff(3)*w(raw)
c                 w(corrected) = wcoeff(1)*u(raw) + wcoeff(2)*v(raw) +
c                                  wcoeff(3)*w(raw)
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, ilng
      double precision clat, clat0, clng, fac, lat, lat0, lng, lng0,
     *          slat, slat0, slng, x2(MAXNAX)
      character dummy*8, pcode*3

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
c     Check validity.
      icrd = coLoc(lu,.false.)
      if (lngax(icrd).eq.0 .or. latax(icrd).eq.0)
     *  call bug('f','Non-celestial coordinate system in coGeom.')

c     Convert the user's coordinate to absolute world coordinates.
c     Fill in the reference location in the output, just in case the
c     user was silly enough not to give enough inputs.
      lng0 = crval(lngax(icrd),icrd)
      lat0 = crval(latax(icrd),icrd)
      x2(lngax(icrd)) = lng0
      x2(latax(icrd)) = lat0

      call coCvt(lu,in,x1,'aw/...',x2)

      lng = x2(lngax(icrd))
      lat = x2(latax(icrd))

c     Get the projection type.
      ilng = lngax(icrd)
      call coExt(ctype(ilng,icrd), dummy, pcode)

c     Compute the matrix elements.
      clat0 = cos(lat0)
      slat0 = sin(lat0)
      clng  = cos(lng-lng0)
      slng  = sin(lng-lng0)
      clat  = cos(lat)
      slat  = sin(lat)

      if (pcode.eq.'NCP') then
        ucoeff(1) =  clng
        ucoeff(2) = -slng*slat
        ucoeff(3) =  slng*clat
        vcoeff(1) =  slng*slat0
        vcoeff(2) =  clat*clat0 + slat*slat0*clng
        vcoeff(3) =  slat*clat0 - clat*slat0*clng
        wcoeff(1) = -slng*clat0
        wcoeff(2) =  clat*slat0 - slat*clat0*clng
        wcoeff(3) =  slat*slat0 + clat*clat0*clng
      else if (pcode.eq.'SIN') then
        fac = 1d0 / (slat*slat0 + clat*clat0*clng)
        ucoeff(1) =  fac * (clat*clat0 + slat*slat0*clng)
        ucoeff(2) = -fac * slat0*slng
        ucoeff(3) =  0d0
        vcoeff(1) =  fac * slat*slng
        vcoeff(2) =  fac * clng
        vcoeff(3) =  0d0
        wcoeff(1) =  0d0
        wcoeff(2) =  0d0
        wcoeff(3) =  0d0
      else
        call bug('f',
     *   'Geometry conversion possible for NCP or SIN proj. only')
      endif

      end

c***********************************************************************

c* coReinit -- Finish initialising a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coReinit(lu)

      integer lu
c  ---------------------------------------------------------------------
c  Finish initialising a coordinate object.  Identifies the celestial
c  axes, if any, checks for consistency and sets up the celprm struct
c  for WCSLIB.
c
c  Input:
c    lu         Handle of the coordinate object.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'
      include 'wcslib/prj.inc'

      logical   ok
      integer   iax, icrd, ilat, ilng, prj(PRJLEN), status
      double precision lat0, lng0
      character lng*8, lat*8, pcode1*3, pcode2*3

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Convert to enumerated coordinate types.
      latax(icrd) = 0
      lngax(icrd) = 0
      spcax(icrd) = 0

      do iax = 1, naxis(icrd)
        call coTyCvt(ctype(iax,icrd), cotype(iax,icrd))
      enddo

c     Find the longitude, latitude, and spectral axes.
      ok = .true.
      do iax = 1, naxis(icrd)
        if (ok) then
          if (cotype(iax,icrd).eq.LNGTYP) then
            ok = lngax(icrd).eq.0
            lngax(icrd) = iax

          else if (cotype(iax,icrd).eq.LATTYP) then
            ok = latax(icrd).eq.0
            latax(icrd) = iax

          else if (cotype(iax,icrd).eq.FRQTYP .or.
     *             cotype(iax,icrd).eq.VELTYP .or.
     *             cotype(iax,icrd).eq.FELTYP) then
            ok = spcax(icrd).eq.0
            spcax(icrd) = iax
          endif
        endif
      enddo

c     Guard against multiple longitude, latitude, or spectral axes.
      if (.not.ok) then
        call bug('w', 'Multiple longitude, latitude, or spectral axes')
      endif

c     Set up the celestial axes.
      if (ok) then
        ilng = lngax(icrd)
        ilat = latax(icrd)

        if (ilng.ne.0 .and. ilat.ne.0) then
          call coExt(ctype(ilng,icrd), lng, pcode1)
          call coExt(ctype(ilat,icrd), lat, pcode2)

c         Check consistency.
          if (pcode1.ne.pcode2) then
            ok = .false.
            call bug('w', 'Incompatible projection types')
          else if (.not.((lng.eq.'RA'   .and. lat.eq.'DEC')  .or.
     *                   (lng.eq.'GLON' .and. lat.eq.'GLAT') .or.
     *                   (lng.eq.'ELON' .and. lat.eq.'ELAT'))) then
            ok = .false.
            call bug('w', 'Incompatible longitude/latitude axis pair')
          else
c           Update celprm now that we know which are the celestial axes.
            lng0 = crval(ilng,icrd)*DR2D
            lat0 = crval(ilat,icrd)*DR2D

c           crval was stored as real*4 in older Miriad images, whence
c           rounding errors may carry the reference latitude at the pole
c           beyond 90 deg by more than the tolerance allowed by WCSLIB.
            if (abs(lat0).gt.90d0) then
              if (abs(lat0).lt.90.00005d0) then
c               Assume it's a rounding error.
                lat0 = sign(90d0, lat0)
              else
c               Do something tricky.
                lat0 = sign(180d0-abs(lat0), lat0)
                lng0 = lng0 + 180d0
                if (lng0.gt.360d0) lng0 = lng0 - 360d0
              endif
            endif

            status = celptd(cel(1,icrd), CEL_REF, lng0, 1)
            status = celptd(cel(1,icrd), CEL_REF, lat0, 2)

            status = celgti(cel(1,icrd), CEL_PRJ, prj)

            call ucase(pcode1)
            if (pcode1.eq.'NCP') then
c             Convert NCP to SIN for WCSLIB.
              if (lat0.eq.0d0) then
                call bug('f', 'Invalid NCP projection')
              endif

              lat0 = lat0*DD2R
              status = prjptc(prj, PRJ_CODE, 'SIN', 0)
              status = prjptd(prj, PRJ_PV, 0d0, 1)
              status = prjptd(prj, PRJ_PV, cos(lat0)/sin(lat0), 2)

            else if (pcode1.eq.'GLS') then
c             Convert GLS to SFL for WCSLIB.
              status = celpti(cel(1,icrd), CEL_OFFSET, 1, 0)
              status = celptd(cel(1,icrd), CEL_PHI0,   0d0,  0)
              status = celptd(cel(1,icrd), CEL_THETA0, lat0, 0)
              status = prjptc(prj, PRJ_CODE, 'SFL', 0)

            else
              status = prjptc(prj, PRJ_CODE, pcode1, 0)
            endif

            status = celpti(cel(1,icrd), CEL_PRJ, prj, 0)
            status = celset(cel(1,icrd))
            if (status.ne.0) call bug('f',
     *        'Error initializing celestial coordinates in coReinit')
          endif

        else if (ilng.ne.0 .or. ilat.ne.0) then
c         Unpaired longitude or latitude axis.
          ok = .false.
          call bug('w', 'Unpaired longitude or latitude axis')
        endif
      endif

      if (.not.ok) then
        call bug('w', 'Assuming that all coordinate axes are linear')

        do iax = 1, naxis(icrd)
          cotype(iax,icrd) = LINEAR
        enddo

        lngax(icrd) = 0
        latax(icrd) = 0
        spcax(icrd) = 0
      endif

      end

c***********************************************************************

c* coPrint -- Print a summary of the coordinate conversion information.
c& rjs
c: coordinates
c+
      subroutine coPrint(lu)

      integer lu
c  ---------------------------------------------------------------------
c  Print a description of a coordinate system.
c
c  Input:
c    lu         Handle of the coordinate system.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      integer   iax, icrd, p
      double precision cdelti, crpixi, crvali
      character algo*8, axtype*16, ctypei*16, line*80, pols*4, radec*12,
     *          units*8, wtype*16

      external  coLoc, hangle, polsC2P, rangle
      integer   coLoc
      character hangle*12, PolsC2P*2, rangle*12
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      do iax = 1, naxis(icrd)
        crpixi = crpix(iax,icrd)
        cdelti = cdelt(iax,icrd)
        crvali = crval(iax,icrd)
        ctypei = ctype(iax,icrd)

        call coCtype(ctypei, axtype, wtype, algo, units)

        if (wtype.eq.'RA') then
c         Time.
          radec = hangle(crvali)
          write(line, 10) ctypei, radec, crpixi, cdelti*DR2AS
 10       format(a8,3x,a11,f10.2,3x,1pe13.6,'  arcsec')

        else if (units.eq.'rad') then
c         Angle.
          radec = rangle(crvali)
          write(line, 20) ctypei, radec, crpixi, cdelti*DR2AS
 20       format(a8,2x,a12,f10.2,3x,1pe13.6,'  arcsec')

        else if (ctypei(:6).eq.'STOKES') then
c         Polarization code.
          p = nint(crvali)
          if (p.eq.0) then
            pols = 'beam'
          else
            pols = PolsC2P(p)
          endif
          write(line, 30) ctypei, pols
 30       format(a8, 8x, a)

        else
          write(line, 40) ctypei, crvali, crpixi, cdelti, units
 40       format(a8,2x,1pe13.6,0pf9.2,3x,1pe13.6,2x,a)
        endif

        call output(line)
      enddo

      end

c***********************************************************************

c* coWrite -- Write out the coordinate description to an image dataset.
c& rjs
c: coordinates
c+
      subroutine coWrite(lu,tno)

      integer lu,tno
c  ---------------------------------------------------------------------
c  Write a coordinate system description to an image dataset header.
c  The coordinate object is expected to have been initialised by
c  coReinit prior to calling this.
c
c  Input:
c    lu         Handle of the coordinate object.
c    tno        Handle of the output dataset.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'wcslib/prj.inc'

      integer   iax, icrd, ilat, ilng, ival, m, prj(PRJLEN), pvrng,
     *          status
      double precision dval, pv(0:29), ref(4)
      character dummy*8, num*2, pcode*4

      external  coLoc, itoaf
      integer   coLoc
      character itoaf*2
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Basic coordinate parameters.
      do iax = 1, naxis(icrd)
        num = itoaf(iax)
        call wrhdd(tno, 'crval'//num, crval(iax,icrd))
        call wrhdd(tno, 'crpix'//num, crpix(iax,icrd))
        call wrhdd(tno, 'cdelt'//num, cdelt(iax,icrd))
        call wrhda(tno, 'ctype'//num, ctype(iax,icrd))
      enddo

c     Coordinate rotation; only written if non-zero.
      if (sinrot(icrd).ne.0d0) then
        dval = atan2(sinrot(icrd), cosrot(icrd))
        call wrhdd(tno, 'llrot', dval)
      endif

c     Celestial coordinates.
      ilng = lngax(icrd)
      ilat = latax(icrd)
      if (ilng.ne.0 .and. ilat.ne.0) then
        call coExt(ctype(ilng,icrd), dummy, pcode)

c       lonpole and latpole; only written if they were set explicitly.
        status = celgtd(cel(1,icrd), CEL_REF, ref)
        if (defs(1,icrd)) call wrhdd(tno, 'lonpole', ref(3))
        if (defs(2,icrd)) call wrhdd(tno, 'latpole', ref(4))

c       Projection parameters.
        if (index('NCP,GLS',pcode).gt.0) then
c         NCP was translated to SIN for WCSLIB in coReinit, don't record
c         parameters for it.  GLS (-> SFL) doesn't have any anyway.
        else
c         phi0, theta0, and offset; only written if set explicitly.
          if (defs(3,icrd) .or. defs(4,icrd)) then
            status = celgtd(cel(1,icrd), CEL_PHI0,   dval)
            status = celgtd(cel(1,icrd), CEL_THETA0, dval)
            status = celgti(cel(1,icrd), CEL_OFFSET, ival)
            if (defs(3,icrd)) call wrhdd(tno, 'phi0',   dval)
            if (defs(4,icrd)) call wrhdd(tno, 'theta0', dval)
            if (ival.ne.0)    call wrhdi(tno, 'xyzero', 1)
          endif

          status = celgti(cel(1,icrd), CEL_PRJ, prj)
          status = prjgtd(prj, PRJ_PV, pv)

          if (pcode.eq.'SIN') then
c           Don't frighten the users!
            if (pv(1).ne.0d0 .or. pv(2).ne.0d0) then
c             Only record parameters for SIN if either is non-zero.
              call wrhdd(tno, 'pv1', pv(1))
              call wrhdd(tno, 'pv2', pv(2))
            endif

          else if (pcode.eq.'ZPN') then
c           Only write non-zero parameters for ZPN.
            do m = 0, 29
              if (pv(m).ne.0d0) then
c               There must be at least one.
                call wrhdd(tno, 'pv'//itoaf(m), pv(m))
              endif
            enddo

          else
c           The remaining projections.
            status = prjgti(prj, PRJ_PVRANGE, pvrng)
            do m = 1, mod(pvrng,100)
c             Record all parameters, including zero-valued ones.
              call wrhdd(tno, 'pv'//itoaf(m), pv(m))
            enddo
          endif
        endif

        status = celpti(cel(1,icrd), CEL_PRJ, prj, 0)
      endif

c     The remaining parameters.
      if (frqscl(icrd)) then
        call wrhda(tno, 'cellscal', '1/F')
      else
        call wrhda(tno, 'cellscal', 'CONSTANT')
      endif

      if (eqnox(icrd).gt.1800d0) then
        call wrhdr(tno, 'epoch', real(eqnox(icrd)))
      endif

      if (obstime(icrd).gt.0d0) then
        call wrhdd(tno, 'obstime', obstime(icrd))
      endif

      if (restfrq(icrd).ne.0d0) then
        call wrhdd(tno, 'restfreq', restfrq(icrd))
      endif

      if (vobs(icrd).ne.0d0) then
        call wrhdd(tno, 'vobs', vobs(icrd))
      endif

      end

c***********************************************************************

c* coFin -- Finish up after completing coordinate conversion.
c& rjs
c: coordinates
c+
      subroutine coFin(lu)

      integer lu
c  ---------------------------------------------------------------------
c  Delete a coordinate system previously initialised with coInit.
c
c  Inputs:
c    lu         Handle of the coordinate system.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd

      external  coLoc
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      nalloc(icrd) = nalloc(icrd) - 1
      if (nalloc(icrd).eq.0) lus(icrd) = 0

      end

c***********************************************************************
c <<< The rest are for internal use only. >>>
c***********************************************************************

      integer function coLoc(lu,alloc)

      integer lu
      logical alloc
c-----------------------------------------------------------------------
      include 'co.h'

      logical first
      integer icrd, free

      save first
      data first/.true./
c-----------------------------------------------------------------------
      if (first) then
        do icrd = 1, MAXCRD
          lus(icrd) = 0
          nalloc(icrd) = 0
        enddo
        first = .false.
      endif

c     Locate the slot for this lu.
      free = 0
      do icrd = 1, MAXCRD
        if (lus(icrd).eq.lu .and. nalloc(icrd).gt.0) then
          if (alloc) nalloc(icrd) = nalloc(icrd) + 1
          coLoc = icrd
          return
        else if (nalloc(icrd).eq.0) then
          free = icrd
        endif
      enddo

c     We did not find it.  If we are allowed to allocate one, do so.
      if (alloc .and. free.ne.0) then
        if (lu.eq.0) then
          lus(free) = -free
        else
          lus(free) = lu
        endif
        nalloc(free) = 1
        coLoc = free
        return
      endif

      call bug('f','Unable to find coordinate object in coLoc')
      coLoc = 0

      end

c***********************************************************************

      subroutine coInitXY(icrd)

      integer icrd
c-----------------------------------------------------------------------
c  Initialise the coordinate information for an image data-set.
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'
      include 'wcslib/prj.inc'

      integer   iax, lu, m, n, prj(PRJLEN), status
      double precision dval
      character cscal*16, keywrd*8, num*2

      external  hdprsnt, itoaf
      logical   hdprsnt
      character itoaf*2
c-----------------------------------------------------------------------
      lu = lus(icrd)

      call rdhdi(lu, 'naxis', naxis(icrd), 0)
      if (naxis(icrd).le.0)
     *  call bug('f','Invalid value for NAXIS in coInit')

      do iax = 1, naxis(icrd)
        num = itoaf(iax)
        call rdhdi(lu, 'naxis'//num, n, 1)
        call rdhdd(lu, 'crval'//num, crval(iax,icrd), dble(n/2+1))
        call rdhdd(lu, 'crpix'//num, crpix(iax,icrd), dble(n/2+1))
        call rdhdd(lu, 'cdelt'//num, cdelt(iax,icrd), 1d0)
        call rdhda(lu, 'ctype'//num, ctype(iax,icrd), ' ')

        if (cdelt(iax,icrd).eq.0d0) then
          if (ctype(iax,icrd).ne.' ') then
            call bug('w',
     *        'Axis increment (cdelt) is 0 for '//ctype(iax,icrd))
            call bug('w','Assuming an axis increment of 1')
          endif
          cdelt(iax,icrd) = 1d0
        endif
      enddo

      call rdhdd(lu, 'llrot', dval, 0d0)
      cosrot(icrd) = cos(dval)
      sinrot(icrd) = sin(dval)


c     Fill in the celprm struct.  Currently we don't know which are the
c     celestial axes so CEL_REF 1 and 2 (i.e. CRVAL) are deferred till
c     coReinit.
      if (hdprsnt(lu, 'lonpole')) then
        call rdhdd(lu, 'lonpole', dval, 0d0)
        status = celptd(cel(1,icrd), CEL_REF, dval, 3)
        defs(1,icrd) = .true.
      endif

      if (hdprsnt(lu, 'latpole')) then
        call rdhdd(lu, 'latpole', dval, 0d0)
        status = celptd(cel(1,icrd), CEL_REF, dval, 4)
        defs(2,icrd) = .true.
      endif

      if (hdprsnt(lu, 'phi0')) then
        call rdhdd(lu, 'phi0', dval, 0d0)
        status = celptd(cel(1,icrd), CEL_PHI0, dval, 0)
        defs(3,icrd) = .true.
      endif

      if (hdprsnt(lu, 'theta0')) then
        call rdhdd(lu, 'theta0', dval, 0d0)
        status = celptd(cel(1,icrd), CEL_THETA0, dval, 0)
        defs(4,icrd) = .true.
      endif

c     Projection parameters.
      status = celgti(cel(1,icrd), CEL_PRJ, prj)

      do m = 0, 29
        keywrd = 'pv' // itoaf(m)
        if (hdprsnt(lu, keywrd)) then
          call rdhdd(lu, keywrd, dval, 0d0)
          status = prjptd(prj, PRJ_PV, dval, m)
        endif
      enddo

      status = celpti(cel(1,icrd), CEL_PRJ, prj, 0)


      call rdhdd(lu, 'restfreq', restfrq(icrd), 0d0)
      call rdhdd(lu, 'epoch',    eqnox(icrd),   0d0)
      call rdhdd(lu, 'obstime',  obstime(icrd), 0d0)
      call rdhdd(lu, 'vobs',     vobs(icrd),    0d0)
      call rdhda(lu, 'specsys',  specsys(icrd), ' ')
      call rdhda(lu, 'cellscal', cscal, '1/F')
      frqscl(icrd) = cscal.eq.'1/F'
      if (.not.frqscl(icrd) .and. cscal.ne.'CONSTANT') call bug('w',
     *  'Unrecognised cellscal value: '//cscal)

      end

c***********************************************************************

      subroutine coInitUV(icrd)

      integer icrd
c-----------------------------------------------------------------------
c  Initialise the coordinate information for a visibility data-set.
c-----------------------------------------------------------------------
      include 'co.h'

      logical   ew
      real      ddec, dra
      double precision dtemp
      character telescop*16
c-----------------------------------------------------------------------
      naxis(icrd) = 2
      call uvrdvra(lus(icrd),'telescop',telescop,' ')
      ew = .false.
      if (telescop.ne.' ') call obspar(telescop,'ew',dtemp,ew)
      if (ew) ew = dtemp.gt.0d0
      if (ew) then
        ctype(1,icrd) = 'RA---NCP'
        ctype(2,icrd) = 'DEC--NCP'
      else
        ctype(1,icrd) = 'RA---SIN'
        ctype(2,icrd) = 'DEC--SIN'
      endif

      call uvrdvrd(lus(icrd),'ra', crval(1,icrd),0d0)
      call uvrdvrd(lus(icrd),'dec',crval(2,icrd),0d0)
      call uvrdvrr(lus(icrd),'dra',dra,0.0)
      call uvrdvrr(lus(icrd),'ddec',ddec,0.0)
      crval(1,icrd) = crval(1,icrd) + dra / cos(crval(2,icrd))
      crval(2,icrd) = crval(2,icrd) + ddec

      crpix(1,icrd) = 0d0
      crpix(2,icrd) = 0d0
      cdelt(1,icrd) = 1d0
      cdelt(2,icrd) = 1d0
      cosrot(icrd)  = 1d0
      sinrot(icrd)  = 0d0
      call uvrdvrd(lus(icrd),'epoch',eqnox(icrd),0d0)
      call uvrdvrd(lus(icrd),'time',obstime(icrd),0d0)
      frqscl(icrd)  = .false.

      end

c***********************************************************************

      subroutine coTyCvt(ctypei, itype)

      character ctypei*(*)
      integer   itype
c-----------------------------------------------------------------------
c  Parse a world coordinate ctype returning an enumerated type.  Used by
c  coReinit so we can't assume that the coordinate object has been
c  initialized.
c
c  Input:
c    ctypei     The FITS-style ctype of the world coordinate.  AIPS-
c               convention spectral types will be translated.
c  Output:
c    itype      Enumerated type defined in co.h: LINEAR, LNGTYP, LATTYP,
c               FRQTYP, VELTYP, or FELTYP.
c-----------------------------------------------------------------------
c     Only needed to define the type codes.
      include 'co.h'

      character algo*8, axtype*16, units*8, wtype*16
c-----------------------------------------------------------------------
      call coCtype(ctypei, axtype, wtype, algo, units)

      if (axtype.eq.'linear') then
        itype = LINEAR
      else if (axtype.eq.'longitude') then
        itype = LNGTYP
      else if (axtype.eq.'latitude') then
        itype = LATTYP
      else if (axtype.eq.'spectral') then
        if (wtype.eq.'FREQ' .or. wtype.eq.'FREQUENCY') then
          itype = FRQTYP
        else if (wtype.eq.'VELO' .or. wtype.eq.'VELOCITY') then
          itype = VELTYP
        else if (wtype.eq.'FELO' .or. wtype.eq.'FELOCITY') then
          itype = FELTYP
        endif
      endif

      end

c***********************************************************************

      subroutine coExt(ctype, wtype, algo)

      character ctype*(*), wtype*(*), algo*(*)
c-----------------------------------------------------------------------
c  Decompose ctype into the world coordinate type and algorithm code.
c-----------------------------------------------------------------------
      integer   i, j
c-----------------------------------------------------------------------
      i = index(ctype, '-')
      if (i.eq.0) then
        wtype = ctype
        algo  = ' '
        return
      else if (i.eq.1) then
        wtype = ' '
      else
        wtype = ctype(:i-1)
      endif

      algo = ' '
      do j = i+1, len(ctype)
        if (ctype(j:j).ne.'-') then
          algo = ctype(j:)
          return
        endif
      enddo

      end

c***********************************************************************

      subroutine coCrack(maxnax,code,x1pix,x1off,defn,n)

      integer   maxnax
      logical   x1pix(maxnax),  x1off(maxnax)
      integer   defn, n
      character code*(*)
c-----------------------------------------------------------------------
c  Decode a coordinate conversion specifier code - refer to the prologue
c  of coCvtv.
c
c  Input:
c    maxnax
c    code       Two-letter codes, one for each coordinate requiring
c               conversion, separated from each other by a slash (/):
c                 'op'  Offset   pixel coordinate,
c                 'ap'  Absolute pixel coordinate,
c                 'ow'  Offset   world coordinate,
c                 'aw'  Absolute world coordinate,
c               For example 'ap/ap' indicates two coordinates, both in
c               absolute pixels.  A special code
c                 '...' ditto,
c               may be used to indicate repetition of the last type
c               specified up to the image dimensionality.  The following
c               abbreviations are understood:
c                 'p' is equivalent to 'ap/...',
c                 'w' is equivalent to 'aw/...'.
c    defn       Number of axes expected.
c  Output:
c    x1pix,x1off
c    n
c-----------------------------------------------------------------------
      logical   ditto, new
      integer   i
      character c*1
c-----------------------------------------------------------------------
      n = 0
      new = .true.
      ditto = .false.

      if (code.eq.'p' .or. code.eq.'P') then
        n = 1
        x1pix(1) = .true.
        x1off(1) = .false.
        ditto = .true.

      else if (code.eq.'w' .or. code.eq.'W') then
        n = 1
        x1pix(1) = .false.
        x1off(1) = .false.
        ditto = .true.

      else
        do i = 1, len(code)
          c = code(i:i)
          if (c.le.' ') then
            continue

          else if (c.eq.'/') then
            if (ditto)
     *        call bug('f','Unrecognised conversion code in coCvt')
            new = .true.

          else if (c.eq.'.') then
            ditto = .true.

          else
            if (ditto)
     *        call bug('f','Unrecognised conversion code in coCvt')

            if (new) then
              n = n + 1
              if (n.gt.maxnax) call bug('f','Too many axes in coCvt')
              new = .false.
            endif

            if (c.eq.'p' .or. c.eq.'P') then
              x1pix(n) = .true.
            else if (c.eq.'w' .or. c.eq.'W') then
              x1pix(n) = .false.
            else if (c.eq.'o' .or. c.eq.'O') then
              x1off(n) = .true.
            else if (c.eq.'a' .or. c.eq.'A') then
              x1off(n) = .false.
            else
              call bug('f','Unrecognised conversion code in coCvt')
            endif
          endif
        enddo
      endif

      if (ditto .and. n.gt.0 .and. n.lt.defn) then
        do i = n+1, defn
          x1pix(i) = x1pix(n)
          x1off(i) = x1off(n)
        enddo
        n = defn
      endif

      end

c***********************************************************************

      subroutine coGetVel(raepo,decepo,eqnox,vel)

      double precision raepo,decepo,eqnox,vel
c  ---------------------------------------------------------------------
c  Determine the Sun's LSR velocity component in a given direction.
c-----------------------------------------------------------------------
      integer i
      double precision dec2000, ra2000, lmn2000(3), velsun(3)

      external  epo2jul
      double precision epo2jul
c-----------------------------------------------------------------------
      if (abs(eqnox-2000d0).gt.0.001d0) then
        call precess(epo2jul(eqnox,' '),raepo,decepo,
     *               epo2jul(2000d0,'J'),ra2000,dec2000)
        call sph2lmn(ra2000,dec2000,lmn2000)
      else
        call sph2lmn(raepo,decepo,lmn2000)
      endif
      call vsun(velsun)
      vel = 0d0
      do i = 1, 3
        vel = vel + lmn2000(i)*velsun(i)
      enddo

      end

c***********************************************************************

      subroutine coFqFac(stype,x1,crpix,cdelt,crval,vobs,
     *  x1pix,x1off,fqfac)

      character stype*4
      double precision x1, crpix, cdelt, crval, vobs
      logical   x1pix, x1off
      double precision fqfac
c-----------------------------------------------------------------------
c  Determine the frequency-dependent scale factor for the celestial
c  axes.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision CKMS
      parameter (CKMS = DCMKS * 1d-3)

      double precision x2
c-----------------------------------------------------------------------
c     Convert to offset world coordinates.
      if (.not.x1off) then
        if (x1pix) then
          x2 = x1 - crpix
        else
          x2 = x1 - crval
        endif
      else
        x2 = x1
      endif
      if (x1pix) x2 = x2 * cdelt

      if (x2.eq.0d0) then
c       Scale is unity at the reference point for all spectral types.
        fqfac = 1d0
      else if (stype.eq.'FREQ') then
        fqfac = crval / (crval + x2)
      else if (stype.eq.'VELO') then
        fqfac = (CKMS - (crval + vobs))/(CKMS - (crval + x2 + vobs))
      else if (stype.eq.'FELO') then
        if (x1pix) x2 = x2  / (1d0 - x2 / (CKMS + crval))
        fqfac = vobs*(1d0 + (crval+x2)/CKMS)*(1d0 + crval/CKMS)
        fqfac = (CKMS + crval + x2 - fqfac) / (CKMS + crval - fqfac)
      else
        call bug('f','Unrecognised axis type in coFqFac: '//stype)
      endif

      end

c***********************************************************************

      subroutine coLinear(crval1,crpix1,cdelt1,x1pix,x1off,x2pix,x2off,
     *  bscal,bzero)

      logical x1pix,x1off,x2pix,x2off
      double precision crpix1,cdelt1,crval1
      double precision bscal,bzero
c-----------------------------------------------------------------------
c  Determine scale factor and offsets to convert from one coordinate
c  system to another.
c
c  The variables bscal and bzero are calculated so that
c
c    out = bscal * in + bzero
c
c  Input:
c    x1pix,x1off
c    x2pix,x2off
c    crpix1,cdelt1,crval1
c  Output:
c    bscal,bzero
c-----------------------------------------------------------------------
      bscal = 1d0
      bzero = 0d0

c     Convert from absolute to offset units, if needed.
      if (.not.x1off) then
        if (x1pix) then
          bzero = -crpix1
        else
          bzero = -crval1
        endif
      endif

c     Convert between offset world and offset pixel units, if needed.
      if (x1pix.neqv.x2pix) then
        if (x1pix) then
          bscal = cdelt1
        else
          bscal = 1d0/cdelt1
        endif
        bzero = bscal * bzero
      endif

c     Convert from offset to absolute units, if needed.
      if (.not.x2off) then
        if (x2pix) then
          bzero = bzero + crpix1
        else
          bzero = bzero + crval1
        endif
      endif

      end

c***********************************************************************

      subroutine coFelo(x1,x2, crpix,cdelt,crval,
     *  x1pix,x1off,x2pix,x2off)

      double precision x1, x2, crpix, cdelt, crval
      logical   x1pix, x1off, x2pix, x2off
c-----------------------------------------------------------------------
c  Convert between pixels and velocities, to a axes in the optical
c  velocity convention (but derived from data measured at equal
c  frequency increments).
c
c  Input:
c    x1         Input pixel or world coordinate.
c    crpix,cdelt,crval
c               WCS parameters for the spectral axis.
c    x1pix      True if a  pixel  coordinate has been given.
c    x1off      True if an offset coordinate has been given.
c    x2pix      True if a  pixel  coordinate is required.
c    x2off      True if an offset coordinate is required.
c  Output:
c    x2         Output pixel or world coordinate.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision CKMS
      parameter (CKMS = DCMKS * 1d-3)

      double precision t, x
c-----------------------------------------------------------------------
c     Convert from absolute to offset units, if required.
      if (.not.x1off) then
        if (x1pix) then
          x = x1 - crpix
        else
          x = x1 - crval
        endif
      else
        x = x1
      endif

c     Do the conversion.
      t = CKMS + crval
      if (x1pix.eqv.x2pix) then
        x2 = x

c     Convert from pixels to optical velocity.
      else if (x1pix) then
        x2 = cdelt * x / (1d0 - cdelt*x/t)

c     Convert from optical velocity to pixels.
      else
        x2 = x / cdelt * t / (CKMS + x + crval)
      endif

c     Convert from offset to absolute units, if required.
      if (.not.x2off) then
        if (x2pix) then
          x2 = x2 + crpix
        else
          x2 = x2 + crval
        endif
      endif

      end

c***********************************************************************

      subroutine coCel(cel, x10,y10, x2,y2,
     *  cosrot,sinrot,crpixx,cdeltx,crpixy,cdelty,
     *  x1pix,y1pix,x1off,y1off,
     *  x2pix,y2pix,x2off,y2off, valid)

      integer   cel(*)
      double precision x10,y10,x2,y2
      double precision cosrot,sinrot,crpixx,crpixy,cdeltx,cdelty
      logical   x1pix,y1pix,x1off,y1off
      logical   x2pix,y2pix,x2off,y2off,valid
c-----------------------------------------------------------------------
c  Convert between pixel coordinates and celestial coordinates.
c
c  Input:
c    cel        WCSLIB struct created by coReinit.
c    x10,y10    Input coordinate, some combination of pixel and/or
c               world coordinate elements.
c    crpixx,cdeltx
c               crpix and cdelt values for the longitude axis.
c    crpixy,cdelty
c               crpix and cdelt values for the latitude axis.
c    x1pix,y1pix
c               True if the input element is a pixel coordinate.
c    x1off,y1off
c               True if the input element is an offset.
c    x2pix,y2pix
c               True if the output element is a pixel coordinate.
c    x2off,y2off
c               True if the output element is an offset.
c  Output:
c    x2,y2      Output coordinate, some combination of pixel and/or
c               world coordinate elements.
c    valid      False if an illegal coordinate conversion was attempted.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'wcslib/cel.inc'

      integer   status
      double precision clat0, crvalx, crvaly, ref(4), x1, y1
c-----------------------------------------------------------------------
      status = celgtd(cel, CEL_REF, ref)
      crvalx = ref(1)*DD2R
      crvaly = ref(2)*DD2R

      clat0 = cos(crvaly)

c     Convert offset coordinates to absolute if needed.
      x1 = x10
      y1 = y10
      if (x1off) then
        if (x1pix) then
c         Convert offset pixel coordinate to absolute.
          x1 = x1 + crpixx
        else
c         Convert offset world coordinate to absolute.
          x1 = x1 / clat0 + crvalx
        endif
      endif

      if (y1off) then
        if (y1pix) then
c         Convert offset pixel coordinate to absolute.
          y1 = y1 + crpixy
        else
c         Convert offset world coordinate to absolute.
          y1 = y1 + crvaly
        endif
      endif


c     Transform coordinates.
      if ((x1pix.eqv.x2pix) .and. (y1pix.eqv.y2pix)) then
c       Nothing to do.
        continue

      else if (x1pix .and. y1pix) then
c       Convert (x,y) to (lng,lat).
        call coxy2ll(cel, x1,y1,x2,y2,
     *    crpixx,cdeltx,crpixy,cdelty,cosrot,sinrot, valid)

      else if (x1pix) then
c       Convert (x,lat) to (lng,y).
        call coMixed(cel, x1,y1,x2,y2,
     *    crpixx,cdeltx,crpixy,cdelty,cosrot,sinrot, .true.,valid)

      else if (y1pix) then
c       Convert (lng,y) to (x,lat).
        call coMixed(cel, x1,y1,x2,y2,
     *    crpixx,cdeltx,crpixy,cdelty,cosrot,sinrot, .false.,valid)

      else
c       Convert (lng,lat) to (x,y).
        call coll2xy(cel, x1,y1,x2,y2,
     *    crpixx,cdeltx,crpixy,cdelty,cosrot,sinrot, valid)
      endif

c     We now have the full set of possibilities.  Return the variety the
c     caller really wanted.
      if (x1pix.eqv.x2pix) x2 = x1
      if (y1pix.eqv.y2pix) y2 = y1

c     Convert from absolute to offset coordinates, if needed.
      if (x2off) then
        if (x2pix) then
c         Convert to offset pixel coordinates.
          x2 = x2 - crpixx
        else
c         Convert to offset world coordinates.
          x2 = x2 - crvalx

c         Convert offset longitude to [-pi,pi].
          if (x2.gt.DPI) then
            x2 = x2 - DTWOPI
          else if (x2.lt.-DPI) then
            x2 = x2 + DTWOPI
          endif

          x2 = x2 * clat0
        endif

      else if (.not.x2pix) then
c       Convert longitude to [0,2*pi].
        if (x2.ge.DTWOPI) then
          x2 = x2 - DTWOPI
        else if (x2.lt.0d0) then
          x2 = x2 + DTWOPI
        endif
      endif

      if (y2off) then
        if (y2pix) then
c         Convert to offset pixel coordinates.
          y2 = y2 - crpixy
        else
c         Convert to offset world coordinates.
          y2 = y2 - crvaly
        endif
      endif

      end

c***********************************************************************

      subroutine coll2xy(cel, lng,lat,p1,p2,
     *  crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)

      integer   cel(*)
      double precision lng,lat,p1,p2
      double precision crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot
      logical   valid
c-----------------------------------------------------------------------
c  (lng,lat) are the celestial coordinates.
c  (p1,p2)   are pixel coordinates.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'wcslib/cel.inc'
      include 'wcslib/prj.inc'

      integer   prj(PRJLEN), stat, status
      double precision dlng, lat0, lng0, phi, ref(4), t, theta, x, y
      character pcode*3
c-----------------------------------------------------------------------
c     Check for simple linear coordinates.
      status = celgti(cel, CEL_PRJ, prj)
      status = prjgtc(prj, PRJ_CODE, pcode)
      if (pcode.eq.' ') then
        status = celgtd(cel, CEL_REF, ref)
        lng0 = ref(1)*DD2R
        lat0 = ref(2)*DD2R

        dlng = mod(lng-lng0, DTWOPI)
        if (dlng.lt.-DPI) then
          dlng = dlng + DTWOPI
        else if (dlng.gt.DPI) then
          dlng = dlng - DTWOPI
        endif

        x = dlng
        y = lat - lat0
        valid = .true.

      else
        status = cels2x(cel, 1, 1, 1, 1, lng*DR2D, lat*DR2D, phi, theta,
     *                  x, y, stat)

        valid = status.eq.0
        if (valid) then
          x = x * DD2R
          y = y * DD2R
        endif
      endif

      if (valid) then
        t =  x*cosrot + y*sinrot
        y = -x*sinrot + y*cosrot
        x = t

        p1 = crpix1 + x/cdelt1
        p2 = crpix2 + y/cdelt2
      else
        p1 = 0d0
        p2 = 0d0
      endif

      end

c***********************************************************************

      subroutine coxy2ll(cel, p1,p2, lng,lat,
     *  crpix1,cdelt1, crpix2,cdelt2, cosrot,sinrot, valid)

      integer   cel(*)
      double precision p1,p2, lng,lat
      double precision crpix1,cdelt1, crpix2,cdelt2, cosrot,sinrot
      logical   valid
c-----------------------------------------------------------------------
c  (p1,p2)   are pixel coordinates.
c  (lng,lat) are the celestial coordinates.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'wcslib/cel.inc'
      include 'wcslib/prj.inc'

      integer   prj(PRJLEN), stat, status
      double precision lat0, lng0, phi, ref(4), t, theta, x, y
      character pcode*3
c-----------------------------------------------------------------------
      x = (p1 - crpix1) * cdelt1
      y = (p2 - crpix2) * cdelt2

      t = x*cosrot - y*sinrot
      y = x*sinrot + y*cosrot
      x = t

c     Check for simple linear coordinates.
      status = celgti(cel, CEL_PRJ, prj)
      status = prjgtc(prj, PRJ_CODE, pcode)
      if (pcode.eq.' ') then
        status = celgtd(cel, CEL_REF, ref)
        lng0 = ref(1)*DD2R
        lat0 = ref(2)*DD2R

        lng = lng0 + x
        lat = lat0 + y
        valid = .true.

      else
        status = celx2s(cel, 1, 1, 1, 1, x*DR2D, y*DR2D, phi, theta,
     *                  lng, lat, stat)
        lng = lng * DD2R
        lat = lat * DD2R

        valid = status.eq.0
      endif

      end

c***********************************************************************

      subroutine coMixed(cel, x1,y1,x2,y2,
     *  crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, pw2wp,valid)

      integer   cel(*)
      double precision cdelt1, cdelt2, crpix1, crpix2, cosrot, sinrot,
     *          x1, x2, y1, y2
      logical   pw2wp,valid
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer MAXITER
      real    TOL
      parameter (MAXITER = 1000, TOL = 1e-5)

      integer i, j, niter
      real    delta
      double precision A, B, C, dp1(2), dp2(2), dw1(2), dw2(2), p1(2),
     *        p2(2), p3(2), pj, w1(2), w2(2), w3(2)
c-----------------------------------------------------------------------
      x2 = 0d0
      y2 = 0d0
      valid = .true.

      if (pw2wp) then
c       (pixel,world) to (world,pixel).
        p1(1) = x1
        p1(2) = crpix2
        w2(2) = y1
        i = 1
        j = 2
      else
c       (world,pixel) to (pixel,world).
        p1(1) = crpix1
        p1(2) = y1
        w2(1) = x1
        if (w2(1).gt.DPI) w2(1) = w2(1) - DTWOPI
        i = 2
        j = 1
      endif

      delta = 1.0
      niter = 0
      do while (delta.gt.TOL .and. niter.lt.MAXITER)
        niter = niter + 1
c       P1: based on the best estimate for the pixel coordinates.
        call coxy2ll(cel, p1(1),p1(2),w1(1),w1(2),
     *         crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)
        if (.not.valid) return

c       P2: based on the best estimate for the world coordinates.
        w2(i) = w1(i)
        call coll2xy(cel, w2(1),w2(2),p2(1),p2(2),
     *         crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot,valid)
        if (.not.valid) return

c       P3: forms a right-angled isoceles triangle with P1 and P2.
        p3(1) = (p1(1) + p2(1) - (p2(2) - p1(2))) / 2d0
        p3(2) = (p1(2) + p2(2) + (p2(1) - p1(1))) / 2d0
        call coxy2ll(cel, p3(1),p3(2),w3(1),w3(2),
     *         crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)

        if (.not.valid) then
c         Try the other side.
          p3(1) = (p1(1) + p2(1) + (p2(2) - p1(2))) / 2d0
          p3(2) = (p1(2) + p2(2) - (p2(1) - p1(1))) / 2d0
          call coxy2ll(cel, p3(1),p3(2),w3(1),w3(2),
     *           crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)

          if (.not.valid) return
        endif

c       We have a plane passing through three points:
c         P1:(p1(i), p1(j), w1(j))
c         P2:(p2(i), p2(j), w2(j))
c         P3:(p3(i), p3(j), w3(j))
c       Determine coefficients for the equation of the plane in the form
c         A*(p(i) - p3(i)) +
c         B*(p(j) - p3(j)) +
c         C*(w(j) - w3(j)) = 0
c       and solve for p(j) using the known values, p1(i) and w2(j).
        dp1(i) = p1(i) - p3(i)
        dp1(j) = p1(j) - p3(j)
        dw1(j) = w1(j) - w3(j)

        if (dw1(j).gt.DPI) then
          dw1(j) = dw1(j) - DTWOPI
        else if (dw1(j).lt.-DPI) then
          dw1(j) = dw1(j) + DTWOPI
        endif

        dp2(i) = p2(i) - p3(i)
        dp2(j) = p2(j) - p3(j)
        dw2(j) = w2(j) - w3(j)

        if (dw2(j).gt.DPI) then
          dw2(j) = dw2(j) - DTWOPI
        else if (dw2(j).lt.-DPI) then
          dw2(j) = dw2(j) + DTWOPI
        endif

        B = dp1(i)*dw2(j) - dp2(i)*dw1(j)
        if (B.eq.0d0) then
          pj = p1(j)
        else
          A = dp2(j)*dw1(j) - dp1(j)*dw2(j)
          C = dp1(j)*dp2(i) - dp1(i)*dp2(j)
          pj = p3(j) - (A*dp1(i) + C*dw2(j)) / B
        endif

        delta = abs(pj - p1(j))

c       Set this as the new estimate of p1(j).
        p1(j) = pj
      enddo

c       if (delta.gt.tol) call bug('f','Failed to converge in coMixed')
      call coxy2ll(cel, p1(1),p1(2),w1(1),w1(2),
     *       crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)

      if (.not.valid) then
        x2 = 0d0
        y2 = 0d0
      else if (pw2wp) then
        x2 = w1(1)
        y2 = p1(2)
      else
        x2 = p1(1)
        y2 = w1(2)
      endif

      end
