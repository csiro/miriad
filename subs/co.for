c***********************************************************************
c
c  A set of routines to convert between differing coordinate systems.
c  User callable routines are:
c
c    subroutine coInit(lu)
c    subroutine coCreate(lu)
c    subroutine coDup(lin,lout)
c    subroutine coRaDec(lu,proj,ra0,dec0)
c    subroutine coReinit(lu)
c    subroutine coCvt(lu,in,x1,out,x2)
c    subroutine coCvtv(lu,in,x1,out,x2,valid)
c    subroutine coCvt1(lu,iax,in,x1,out,x2)
c    subroutine coLMN(lu,in,x1,lmn)
c    subroutine coGeom(lu,in,x1,ucoeff,vcoeff)
c    subroutine coFindAx(lu,axis,iax)
c    subroutine coFreq(lu,in,x1,freq)
c    subroutine coVelSet(lu,axis)
c    subroutine coPrjSet(lu,proj)
c    subroutine coAxGet(lu,iax,ctype,crpix,crval,cdelt)
c    subroutine coAxSet(lu,iax,ctype,crpix,crval,cdelt)
c    subroutine coGetd(lu,object,value)
c    subroutine coGeta(lu,object,value)
c    subroutine coSetd(lu,object,value)
c    subroutine coSeta(lu,object,value)
c    subroutine coGauCvt(lu,in,x1,io,bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2)
c    logical function coCompar(lu1,lu2,match)
c    subroutine coLin(lu1,in,x1,n,ctype,crpix,crval,cdelt)
c    subroutine coPrint(lu)
c    subroutine coWrite(lu,tno)
c    subroutine coFin(lu)
c
c  History:
c    rjs   9aug94 Original version.
c    rjs  13sep94 Support 'VELOCITY' and 'FELOCITY' axes.
c    rjs  12oct94 Added a good many things ... for mosaicing.
c    rjs  23nov94 A dummy statement to stop the alpha compiler
c                 complaining.
c    rjs  30jan95 Added cogeom.
c    rjs  26sep95 More tolerant of screwy headers.
c    rjs  22oct95 Comment out warning about assuming its a linear
c                 coordinate system.
c    nebk 20nov95 Terrible error.  Initialize docelest to false in
c                 cocvt.f.
c    rjs  15oct96 Change call sequence to cogeom.
c    rjs  16oct96 Correct cartesian conversions near RA=0.
c    rjs  02jul97 Support "cellscal" keyword.
c    rjs  07jul97 Treat "epoch" and "obstime" as part of the coordinate
c                 specification. Add coGetd. Improve coVelSet. Support
c                 gls projection.
c    rjs  14jul97 Fix bug in covelset introduced on above date.
c    rjs  21jul97 More robust to bad freq values. Added coSeta.
c    rjs  16oct97 Minor correction to the felo axis definition.
c    rjs  10nov97 Make a linear axis if there are no header values.
c    rjs  20nov98 Partial handling of sky rotation.
c    rjs  15dec98 More complete handling of sky rotation.
c    rjs   8jan98 Fix errors in cogaucvt when angle is non-zero and
c                 pixel increments differ.
c    rjs   8sep99 More robust algorithm in coMixed.
c    rjs  10may00 Comment out some code to hopefully get a better wrap
c                 algorithm.
c    dpr  16feb01 Bump up MAXITERS to 1000 in coMixed to handle Erik
c                 Muller data. Not a good solution.
c    rjs  31may06 Develop coCvtv (validate coordinate), and check for
c                 invalid coordinate conversions.
c    rjs  15jul10 Correct recent bug introduced in coVelSet. Correct order
c		  of "save" statement in coloc.
c
c $Id: co.for,v 1.6 2010/07/15 01:04:38 cal103 Exp $
c***********************************************************************
c* coInit -- Initialise coordinate conversion routines.
c& rjs
c: coordinates
c+
      subroutine coInit(lu)

      integer lu

c  Initialise the coordinate conversion system.
c
c  The coordinate conversion routines supports simultaneously a number
c  of coordinate systems. A coordinate system is initialised by a call
c  to coInit, giving the handle of the Miriad data-set in question.
c  This initialises a new coordinate system. Subsequent calls to the
c  coordinate routines also give the handle of the data-set, which is
c  used to determine the appropriate coordinate system to use.
c
c  Input:
c    lu         The handle of the Miriad data-set. This can be either an
c               image or visibility data-set. For a visibility data-set,
c               the coordinate system simply consists of RA and DEC
c               axes, centered on the observing centre.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd

c     Externals.
      logical   hdprsnt
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.true.)
      if (nalloc(icrd).gt.1) return

c  Is this an image of a visibility data set? Assume its visibility is
c  the "visdata" item is present.
c
      if (hdprsnt(lus(icrd),'visdata')) then
        call coInitUV(icrd)
      else if (hdprsnt(lus(icrd),'image')) then
        call coInitXY(icrd)
      else
        call bug('f','Unrecognised dataset type, in coInit')
      endif
c
c  Finish up initialising this coordinate object.
c
      call coReinit(lu)
      end
c***********************************************************************
c* coDup -- Duplicate a coodinate object.
c& rjs
c: coordinates
c+
      subroutine coDup(lin,lout)

      integer   lin, lout

c  Duplicate a coordinate object.
c
c  Input:
c    lin        Handle of the coordinate object to be duplicated.
c  Output:
c    lout       Duplicated coordinate object.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd1, icrd2

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      call coCreate(lout)
      icrd1 = coLoc(lin,.false.)
      icrd2 = coLoc(lout,.false.)

      naxis(icrd2) = naxis(icrd1)
      do iax = 1, naxis(icrd2)
        ctype(iax,icrd2) = ctype(iax,icrd1)
        crpix(iax,icrd2) = crpix(iax,icrd1)
        cdelt(iax,icrd2) = cdelt(iax,icrd1)
        crval(iax,icrd2) = crval(iax,icrd1)
      enddo
      cosrot(icrd2) = cosrot(icrd1)
      sinrot(icrd2) = sinrot(icrd1)

      restfrq(icrd2) = restfrq(icrd1)
      vobs(icrd2)    = vobs(icrd1)
      epoch(icrd2)   = epoch(icrd1)
      obstime(icrd2) = obstime(icrd1)
      frqscl(icrd2)  = frqscl(icrd1)

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

c  Create a simple RA/DEC coordinate system.
c
c  Input:
c    proj       Projection geomery (e.g. 'SIN', 'NCP', etc)
c    ra0,dec0   RA,DEC of the reference point.
c  Output:
c    lu         Handle of the output coordinate object.
c--
c-----------------------------------------------------------------------
      character ctype*16
c-----------------------------------------------------------------------
      call coCreate(lu)
      ctype = 'RA---' // proj
      call coAxSet(lu,1,ctype,0d0,ra0,1d0)
      ctype = 'DEC--' // proj
      call coAxSet(lu,2,ctype,0d0,dec0,1d0)
      call coReinit(lu)

      end
c***********************************************************************
c* coCreate -- Begin intialisation of a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coCreate(lu)

      integer lu

c  Begin building up a coordinate object from scratch.
c
c  Output:
c    lu         Handle of the coordinate object.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(0,.true.)
      lu = -icrd

      cosrot(icrd) = 1d0
      sinrot(icrd) = 0d0

      restfrq(icrd) = 0d0
      vobs(icrd)    = 0d0
      epoch(icrd)   = 0d0
      obstime(icrd) = 0d0
      naxis(icrd)   = 0
      frqscl(icrd)  = .true.

      end
c***********************************************************************
c* coAxSet -- Set the characteristics of a particular axis.
c& rjs
c: coordinates
c+
      subroutine coAxSet(lu,iax,ctypei,crpixi,crvali,cdelti)

      integer lu, iax
      character ctypei*(*)
      double precision crpixi,crvali,cdelti

c  Set the coordinates of an axis to something.
c
c  Input:
c    lu         Handle of the coordinate object.
c    iax        Axis number.
c    ctypei,... FITS-style ctype,crpix,crval,cdelt
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, jax

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      if (iax.lt.1 .or. iax.gt.MAXNAX) then
        call bug('f','Illegal axis number')
      endif

      icrd = coLoc(lu,.false.)

      do jax = naxis(icrd)+1, iax-1
        ctype(jax,icrd) = ' '
        crpix(jax,icrd) = 1d0
        crval(jax,icrd) = 0d0
        cdelt(jax,icrd) = 1d0
      enddo

      naxis(icrd) = max(naxis(icrd),iax)
      ctype(iax,icrd) = ctypei
      crpix(iax,icrd) = crpixi
      crval(iax,icrd) = crvali
      cdelt(iax,icrd) = cdelti

      end
c***********************************************************************
c* coSetd -- Set the value in the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coSetd(lu,object,value)

      integer lu
      character object*(*)
      double precision value

c  Set a value in the guts of the coordinate routines!
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c    value      Value to use.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   ok
      integer   iax, icrd
      character obj*8

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      obj = object
      iax = ichar(obj(6:6)) - ichar('0')
      ok  = 1.le.iax .and. iax.le.MAXNAX

      if (obj(1:5).eq.'crpix' .and. ok) then
        crpix(iax,icrd) = value
      else if (obj(1:5).eq.'cdelt' .and. ok) then
        cdelt(iax,icrd) = value
      else if (obj(1:5).eq.'crval' .and .ok) then
        crval(iax,icrd) = value
      else if (obj.eq.'llrot') then
        cosrot(icrd) = cos(value)
        sinrot(icrd) = sin(value)
      else if (obj.eq.'restfreq') then
        restfrq(icrd) = value
      else if (obj.eq.'vobs') then
        vobs(icrd) = value
      else if (obj.eq.'epoch') then
        epoch(icrd) = value
      else if (obj.eq.'obstime') then
        obstime(icrd) = value
      else
        call bug('f','Unrecognised object in coSetd')
      endif

      end
c***********************************************************************
c* coSeta -- Set the value in the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coSeta(lu,object,value)

      integer lu
      character object*(*),value*(*)

c  Set a value in the guts of the coordinate routines!
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c    value      Value to use.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   ok
      integer   iax, icrd
      character obj*8

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      obj = object
      iax = ichar(obj(6:6)) - ichar('0')
      ok  = 1.le.iax .and. iax.le.MAXNAX

      if (obj.eq.'cellscal') then
        if (value.eq.'CONSTANT') then
          frqscl(icrd) = .false.
        else if (value.eq.'1/F') then
          frqscl(icrd) = .true.
        else
          call bug('f','Unrecognised value for cellscal in coSetA')
        endif
      else if (obj(1:5).eq.'ctype' .and. ok) then
        ctype(iax,icrd) = value
      else
        call bug('f','Unrecognised object in coSetA')
      endif

      end
c***********************************************************************
c* coGetd -- Get the value from the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coGetd(lu,object,value)

      integer lu
      character object*(*)
      double precision value

c  Get a value from the guts of the coordinate routines!
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c  Output:
c    value      Value to use.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   ok
      integer   iax, icrd
      character obj*8

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      obj = object
      iax = ichar(obj(6:6)) - ichar('0')
      ok  = 1.le.iax .and. iax.le.MAXNAX

      if (obj.eq.'naxis') then
        value = naxis(icrd)
      else if (obj.eq.'restfreq') then
        value = restfrq(icrd)
      else if (obj.eq.'vobs') then
        value = vobs(icrd)
      else if (obj.eq.'epoch') then
        value = epoch(icrd)
      else if (obj.eq.'obstime') then
        value = obstime(icrd)
      else if (obj.eq.'llrot') then
        if (sinrot(icrd).eq.0d0) then
          value = 0d0
        else
          value = atan2(sinrot(icrd),cosrot(icrd))
        endif
      else if (obj(1:5).eq.'crval' .and. ok) then
        value = crval(iax,icrd)
      else if (obj(1:5).eq.'crpix' .and. ok) then
        value = crpix(iax,icrd)
      else if (obj(1:5).eq.'cdelt' .and. ok) then
        value = cdelt(iax,icrd)
      else
        call bug('f','Unrecognised object in coGetd')
      endif

      end
c***********************************************************************
c* coGeta -- Get the value from the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coGeta(lu,object,value)

      integer lu
      character object*(*),value*(*)

c  Get a value from the guts of the coordinate routines!
c
c  Input:
c    lu         Handle of the coordinate object.
c    object     Name of the thing to set.
c  Output:
c    value      Value to use.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   ok
      integer   iax, icrd
      character obj*8

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      obj = object
      iax = ichar(obj(6:6)) - ichar('0')
      ok  = 1.le.iax .and. iax.le.MAXNAX

      if (obj(1:5).eq.'ctype' .and. ok) then
        value = ctype(iax,icrd)
      else if (obj.eq.'cellscal') then
        if (frqscl(icrd)) then
          value = '1/F'
        else
          value = 'CONSTANT'
        endif
      else
        call bug('f','Unrecognised object in coGeta')
      endif

      end
c***********************************************************************
c* coReinit -- Finish initialisation of a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coReinit(lu)

      integer lu

c  Finish up initialising a coordinate object.
c
c  Input:
c    lu         Handle of the coordinate object.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   ok
      integer   iax, icrd

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
c
c  Find the index of this object.
c
      icrd = coLoc(lu,.false.)
c
c  Convert the coordinate type to an enumerated type, and check for
c  consistency of celestial coordinates.
c
      latax(icrd) = 0
      lngax(icrd) = 0
      frqax(icrd) = 0
      ok = .true.
      do iax = 1, naxis(icrd)
        call coTyCvt(ctype(iax,icrd),cotype(iax,icrd),coproj(icrd))
c
c  Check that we have a compatible set of celestial coordinates.
c
        if (cotype(iax,icrd).eq.FRQTYP .or.
     *      cotype(iax,icrd).eq.VELTYP .or.
     *      cotype(iax,icrd).eq.FELTYP) then
          ok = ok .and. frqax(icrd).eq.0
          frqax(icrd) = iax
        else if (cotype(iax,icrd).eq.LATTYP) then
          ok = ok .and. latax(icrd).eq.0
          latax(icrd) = iax
        else if (cotype(iax,icrd).eq.LNGTYP) then
          ok = ok .and. lngax(icrd).eq.0
          lngax(icrd) = iax
        endif
      enddo
c
c  Check the celestial coordinates.
c
      if (latax(icrd).ne.0 .and. lngax(icrd).ne.0) then
        if (ok) then
          call coCompat(ctype(lngax(icrd),icrd),ctype(latax(icrd),icrd),
     *                  ok)
        endif
      else if (latax(icrd).ne.0 .or. lngax(icrd).ne.0) then
        ok = .false.
c       else
c         ok = .true.
      endif
c
c  Check everything makes sense.
c
      if (.not.ok) then
        call bug('w','Something is screwy with the axes definitions')
        call bug('w',' ... assuming linear coordinate systems')
        do iax = 1, naxis(icrd)
          if (cotype(iax,icrd).eq.LATTYP .or.
     *        cotype(iax,icrd).eq.LNGTYP) then
            cotype(iax,icrd) = LINEAR
          endif
        enddo
        frqax(icrd) = 0
      endif

      end
c***********************************************************************
c* coFin -- Finish up after completing coordinate conversion.
c& rjs
c: coordinates
c+
      subroutine coFin(lu)

      integer lu

c  This tidies up and deletes a coordinate system previously initialised
c  with coInit.
c
c  Inputs:
c    lu         Handle of the coordinate system.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      nalloc(icrd) = nalloc(icrd) - 1
      if (nalloc(icrd).eq.0) lus(icrd) = 0

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

c  Call coCvtv and die immediately if the conversion is invalid.  This
c  interface exists for backwards compatibility only.  It should not be
c  used in new code.
c--
c-----------------------------------------------------------------------
      logical valid
c-----------------------------------------------------------------------
      call coCvtv (lu, in, x1, out, x2, valid)
      if (.not.valid) then
        call bug ('f', 'Invalid coordinate conversion in coCvt')
      end if

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

c  Convert coordinates from one coordinate system to another.
c  Input and output coordinates can be either "pixel" or "world"
c  coordinates, and either "absolute" or "offset".
c
c  "World" coordinates are the normal physical units associated with
c  a coordinate.  World coordinates are given in radians (for
c  astronomical positions), GHz (frequencies), km/s (velocities) and
c  lambda (U-V axes).
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
c    in         This indicates the units of the input coordinates.
c               It consists of a sequence of,
c                 'op'  Offset pixel coordinate
c                 'ap'  Absolute pixel coordinate
c                 'ow'  Offset world coordinate
c                 'aw'  Absolute world coordinate,
c               one for each coordinate requiring conversion. Each
c               coordinate unit specifier is separated from the other
c               by a slash (/).
c               For example 'ap/ap' indicates two coordinates, both in
c               absolute pixels.
c    x1         The input coordinates, in units as givien by the `in'
c               parameter. The dimensionality must agree with the number
c               of units given in `in'.
c    out        This indicates the units of the output coordinates, in
c               the same fashion as the `in' value.  The outputs must
c               correspond one-for-one with the inputs.
c  Output:
c    x2         The output coordinates, in units given by `out'.
c    valid      If true, all is OK, otherwise the coordinate conversion
c               requested was invalid.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   docelest, x1off(MAXNAX), x1pix(MAXNAX), x2off(MAXNAX),
     *          x2pix(MAXNAX)
      integer   iax, icrd, ifrq, ilat, ilng, n, nt
      double precision bscal, bzero, scal, temp

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
c
c  Determine the operation to be performed.
c
      icrd = coLoc(lu,.false.)
      call coCrack(in,x1pix,x1off,naxis(icrd),MAXNAX,n)
      call coCrack(out,x2pix,x2off,n,MAXNAX,nt)
      docelest = .false.
      valid = .true.
c
c  Convert each of the axes.
c
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

        else if (cotype(iax,icrd).eq.FELTYP) then
          call coFelo(x1(iax),x2(iax),crval(iax,icrd),crpix(iax,icrd),
     *     cdelt(iax,icrd),x1pix(iax),x1off(iax),x2pix(iax),x2off(iax))

        else if (cotype(iax,icrd).eq.LATTYP .or.
     *           cotype(iax,icrd).eq.LNGTYP) then
          docelest = .true.
        else
          call bug('f','Internal software bug, in coCvt')
        endif
      enddo

      if (docelest) then
        ilng = lngax(icrd)
        ilat = latax(icrd)
        ifrq = frqax(icrd)
c
c  Determine the frequency-dependent scale factor.
c
        if (ifrq.eq.0 .or. ifrq.gt.n .or. .not.frqscl(icrd)) then
          scal = 1d0
        else
          call coFqFac(x1(ifrq),ctype(ifrq,icrd),crval(ifrq,icrd),
     *      crpix(ifrq,icrd),cdelt(ifrq,icrd),vobs(icrd),
     *      x1off(ifrq),x1pix(ifrq),scal)
        endif
c
c  Do the conversion. If one of the latitude or longitude is missing,
c  use the reference pixel as the corresponding value.
c
        if (ilng.le.n .and. ilat.le.n) then
          call coCelest(x1(ilng),x1(ilat),x2(ilng),x2(ilat),
     *      coproj(icrd),cosrot(icrd),sinrot(icrd),
     *      crval(ilng,icrd),crpix(ilng,icrd),scal*cdelt(ilng,icrd),
     *      crval(ilat,icrd),crpix(ilat,icrd),scal*cdelt(ilat,icrd),
     *      x1pix(ilng),x1pix(ilat),x2pix(ilng),x2pix(ilat),
     *      x1off(ilng),x1off(ilat),x2off(ilng),x2off(ilat),valid)
        else if (ilat.le.n) then
          call coCelest(0d0,x1(ilat),temp,x2(ilat),coproj(icrd),
     *      cosrot(icrd),sinrot(icrd),
     *      crval(ilng,icrd),crpix(ilng,icrd),scal*cdelt(ilng,icrd),
     *      crval(ilat,icrd),crpix(ilat,icrd),scal*cdelt(ilat,icrd),
     *      .true.,x1pix(ilat),.true.,x2pix(ilat),
     *      .true.,x1off(ilat),.true.,x2off(ilat),valid)
        else
          call coCelest(x1(ilng),0d0,x2(ilng),temp,coproj(icrd),
     *      cosrot(icrd),sinrot(icrd),
     *      crval(ilng,icrd),crpix(ilng,icrd),scal*cdelt(ilng,icrd),
     *      crval(ilat,icrd),crpix(ilat,icrd),scal*cdelt(ilat,icrd),
     *      x1pix(ilng),.true.,x2pix(ilng),.true.,
     *      x1off(ilng),.true.,x2off(ilng),.true.,valid)
        endif
      endif

      end
c***********************************************************************
c* coFreq -- Convert spectral coordinates to frequency.
c& rjs
c: coordinates
c+
      subroutine coFreq(lu,in,x1,freq1)

      integer lu
      character in*(*)
      double precision x1(*),freq1

c  Get the frequency corresponding to a particular coordinate.
c
c  Input:
c    lu         Handle of the coordinate object.
c    in         As with coCvt
c    x1         As with coCvt
c  Output:
c    freq1      The frequency.
c--
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      double precision ckms
      parameter (ckms = 1d-3*DCMKS)

      logical   ok
      integer   icrd, itype
      double precision x2(MAXNAX)

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
c
c  Check validity.
c
      icrd = coLoc(lu,.false.)
      ok = frqax(icrd).gt.0 .and.
     *       (restfrq(icrd).gt.0d0 .or.
     *        cotype(frqax(icrd),icrd).eq.FRQTYP)
      if (.not.ok)
     *  call bug('f','Non-spectral coordinate system, in coFreq')
c
c  Convert the users coordinate to absolute world coordinates.
c  Fill in the reference location in the output, just in case the
c  user was silly enough not to give enough inputs.
c
      x2(frqax(icrd)) = crval(frqax(icrd),icrd)
      call coCvt(lu,in,x1,'aw/...',x2)
      freq1 = x2(frqax(icrd))
c
c  Convert from velocityes
c
      itype = cotype(frqax(icrd),icrd)
      if (itype.eq.FRQTYP) then
        continue
      else if (itype.eq.FELTYP) then
        freq1 = restfrq(icrd) / (1d0 + freq1/ckms)
     *                - restfrq(icrd)*vobs(icrd)/ckms
      else if (itype.eq.VELTYP) then
        freq1 = restfrq(icrd) * (1d0 - freq1/ckms)
     *                - restfrq(icrd)*vobs(icrd)/ckms
      else
        call bug('f','Something is screwy, in coFreq')
      endif

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

c  Get the direction cosines corresponding to a particular coordinate.
c
c  Input:
c    lu         Handle of the coordinate object.
c    in         As with coCvt
c    x1         As with coCvt
c  Output:
c    lmn        The direction cosines (with respect to the reference
c               position) of the celestial coordinate given by x1.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd
      double precision dec, dec0, ra, ra0, x2(MAXNAX)

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
c
c  Check validity.
c
      icrd = coLoc(lu,.false.)
      if (lngax(icrd).eq.0 .or. latax(icrd).eq.0)
     *  call bug('f','Non-celestial coordinate system, in coLMN')
c
c  Convert the users coordinate to absolute world coordinates.
c  Fill in the reference location in the output, just in case the
c  user was silly enough not to give enough inputs.
c
      ra0 = crval(lngax(icrd),icrd)
      dec0 = crval(latax(icrd),icrd)
      x2(lngax(icrd)) = ra0
      x2(latax(icrd))  = dec0

      call coCvt(lu,in,x1,'aw/...',x2)

      ra = x2(lngax(icrd))
      dec = x2(latax(icrd))
c
c  Convert to direction cosines.
c
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

      integer lu
      character in*(*)
      double precision x1(*),ucoeff(3),vcoeff(3),wcoeff(3)

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
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd
      double precision clat, clat0, clng, fac, lat, lat0, lng, lng0,
     *          slat, slat0, slng, x2(MAXNAX)

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
c
c  Check validity.
c
      icrd = coLoc(lu,.false.)
      if (lngax(icrd).eq.0 .or. latax(icrd).eq.0)
     *  call bug('f','Non-celestial coordinate system, in coLMN')
c
c  Convert the users coordinate to absolute world coordinates.
c  Fill in the reference location in the output, just in case the
c  user was silly enough not to give enough inputs.
c
      lng0 = crval(lngax(icrd),icrd)
      lat0 = crval(latax(icrd),icrd)
      x2(lngax(icrd)) = lng0
      x2(latax(icrd))  = lat0

      call coCvt(lu,in,x1,'aw/...',x2)

      lng = x2(lngax(icrd))
      lat = x2(latax(icrd))
c
c  Determine the conversion coefficients.
c
      clat0 = cos(lat0)
      slat0 = sin(lat0)
      clng  = cos(lng-lng0)
      slng  = sin(lng-lng0)
      slat  = sin(lat)
      clat  = cos(lat)

      if (coProj(icrd).eq.'ncp') then
        ucoeff(1) =  clng
        ucoeff(2) = -slng*slat
        ucoeff(3) =  slng*clat
        vcoeff(1) =  slng*slat0
        vcoeff(2) =  clat*clat0 + slat*slat0*clng
        vcoeff(3) =  slat*clat0 - clat*slat0*clng
        wcoeff(1) = -slng*clat0
        wcoeff(2) =  clat*slat0 - slat*clat0*clng
        wcoeff(3) =  slat*slat0 + clat*clat0*clng
      else if (coProj(icrd).eq.'sin') then
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
c* coCvt1 -- Do coordinate conversion on one axis only.
c& rjs
c: coordinates
c+
      subroutine coCvt1(lu,iax,in,x1,out,x2)

      integer lu,iax
      double precision x1,x2
      character in*(*),out*(*)

c  This converts a coordinate, for a particular axis, from one system to
c  another.
c
c  Input:
c    lu         Handle of the coordinate system.
c    iax        Axis number.
c    in,out     These indicate the conversion to be performed.
c    x1         Input coordinate.
c  Output:
c    x2         Output, converted, coordinate.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   valid, x1off, x1pix, x2off, x2pix
      integer   icrd, ilat, ilng, n
      double precision dtemp, bscal, bzero

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      if (iax.lt.1) call bug('f','Invalid axis, in coCvt1')
      x2 = x1
      if (iax.gt.naxis(icrd)) return

      call coCrack(in,x1pix,x1off,1,1,n)
      if (n.ne.1) call bug('f','Invalid conversion, in coCvt1')
      call coCrack(out,x2pix,x2off,1,1,n)
      if (n.ne.1) call bug('f','Invalid conversion, in coCvt1')

      valid = .true.
c
c  Convert a linear axis.
c
      if (cotype(iax,icrd).eq.LINEAR .or.
     *    cotype(iax,icrd).eq.VELTYP .or.
     *    cotype(iax,icrd).eq.FRQTYP) then
        call coLinear(crval(iax,icrd),crpix(iax,icrd),cdelt(iax,icrd),
     *        x1pix,x1off,x2pix,x2off,bscal,bzero)
        x2 = bscal * x1 + bzero
c
c  Convert an RA axis.
c
      else if (cotype(iax,icrd).eq.LNGTYP) then
        ilng = lngax(icrd)
        ilat = latax(icrd)
        call coCelest(x1,0d0,x2,dtemp,coproj(icrd),
     *    cosrot(icrd),sinrot(icrd),
     *    crval(ilng,icrd),crpix(ilng,icrd),cdelt(ilng,icrd),
     *    crval(ilat,icrd),crpix(ilat,icrd),cdelt(ilat,icrd),
     *    x1pix,x1pix,x2pix,x2pix,x1off,.true.,x2off,.true.,valid)
c
c  Convert a DEC axis.
c
      else if (cotype(iax,icrd).eq.LATTYP) then
        ilng = lngax(icrd)
        ilat = latax(icrd)
        call coCelest(0d0,x1,dtemp,x2,coproj(icrd),
     *    cosrot(icrd),sinrot(icrd),
     *    crval(ilng,icrd),crpix(ilng,icrd),cdelt(ilng,icrd),
     *    crval(ilat,icrd),crpix(ilat,icrd),cdelt(ilat,icrd),
     *    x1pix,x1pix,x2pix,x2pix,.true.,x1off,.true.,x2off,valid)
c
c  Convert a FELO axis.
c
      else if (cotype(iax,icrd).eq.FELTYP) then
        call coFelo(x1,x2,crval(iax,icrd),crpix(iax,icrd),
     *    cdelt(iax,icrd),x1pix,x1off,x2pix,x2off)
      endif

      if (.not.valid) call bug('f',
     *  'An invalid coordinate conversion was requested in coCvt1')

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

c  This converts the parameters that describe a gaussian between pixel
c  and world coordinate systems.  The gaussian lies in the image formed
c  by the first two axes of the coordinate system.
c
c  Input:
c    lu         Handle of the coordinate system.
c    x1         This gives the coordinate of the centroid of the
c               gaussian.
c    in         This gives the units of the input gaussian centroid in
c               the same format as the "in" parameter of coCvt.  For
c               example, to indicate that the input coordinate consists
c               of 3 numbers in absolute world units, use 'aw/aw/aw'.
c    ing,outg   The conversion to be performed.  Possible values are:
c                 'w'   Input or output is in world units.
c                 'p'   Input or output is in pixel units.
c    bmaj1,bmin1,bpa1  Input gaussian parameters: major axis, minor axis
c               and position angle of major axis. The position angle is
c               measured from "north" through "east" where north is the
c               direction of increasing value along the second axis,
c               and east is the direction of increasing value along the
c               first axis. bmaj and bmin will either be in world or
c               pixel units. bpa will be in radians.
c  Output:
c    bmaj2,bmin2,bpa2 Output gaussian parameters.
c--
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      logical   x1off(MAXNAX), x1pix(MAXNAX)
      integer   icrd, ifrq, n
      real      cdelt1, cdelt2, cospa, crota, crota1, crota2, sinpa
      double precision alpha, beta, gamma, s, scale, t

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
c
c  Determine the operation to be performed.
c
      icrd = coLoc(lu,.false.)

      call coCrack(in,x1pix,x1off,naxis(icrd),MAXNAX,n)
c
c  Get the factors to scale cdelt1 and cdelt2 for this coordinate.
c
      cdelt1 = cdelt(1,icrd)
      cdelt2 = cdelt(2,icrd)
      if (cotype(1,icrd).eq.LATTYP .or.
     *    cotype(1,icrd).eq.LNGTYP .or.
     *    cotype(2,icrd).eq.LATTYP .or.
     *    cotype(2,icrd).eq.LNGTYP) then
        ifrq = frqax(icrd)
        if (ifrq.ne.0 .and. ifrq.le.n .and. frqscl(icrd)) then
          call coFqFac(x1(ifrq),ctype(ifrq,icrd),crval(ifrq,icrd),
     *      crpix(ifrq,icrd),cdelt(ifrq,icrd),vobs(icrd),x1off(ifrq),
     *      x1pix(ifrq),scale)

          if (cotype(1,icrd).eq.LATTYP .or.
     *        cotype(1,icrd).eq.LNGTYP) then
            cdelt1 = cdelt1 * scale
          endif

          if (cotype(2,icrd).eq.LATTYP .or.
     *        cotype(2,icrd).eq.LNGTYP) then
            cdelt2 = cdelt2 * scale
          endif
        endif
      endif
c
c  Get coordinate grid rotation angle.
c
      if (abs(sinrot(icrd)).ne.0d0) then
        crota = atan2(sinrot(icrd),cosrot(icrd))
      else
        crota = 0.0
      endif
      crota1 = 0.0
      crota2 = 0.0
      if (ing.eq.'w')  crota1 = crota
      if (outg.eq.'w') crota2 = crota
c
c  Get the increments in a standard form.
c
      cospa = cos(bpa1+crota1)
      sinpa = sin(bpa1+crota1)

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
c
c  Do the conversion.
c
      s = alpha + beta
      t = sqrt((alpha-beta)**2 + gamma**2)
      bmin2 = sqrt(2/(s+t))
      bmaj2 = sqrt(2/(s-t))
      if (abs(gamma)+abs(alpha-beta).eq.0d0) then
        bpa2 = 0.0
      else
        bpa2 = 0.5*atan2(gamma,alpha-beta)
      endif
      bpa2 = bpa2 - crota2

      end
c***********************************************************************
c* coVelSet -- Change the velocity axis between freq/velo/felo formats.
c& rjs
c: coordinates
c+
      subroutine coVelSet(lu,type)

      integer lu
      character type*(*)

c  This changes the axis type for the `velocity' axis of a coordinate
c  system. The `velocity' axis can be changed between a frequency,
c  radio velocity or optical velocity axis.
c
c  Input:
c    lu         Handle of the coordinate system.
c    type       Something combination of 'FREQ','VELO' and 'FELO'
c               with '   ','-OBS','-HEL','-LSR.
c               For compatibility with the old calling sequence,
c               "type" can also be 'radio', 'optical' and 'frequency',
c               which are equivalent to 'VELO', 'FELO' and 'FREQ'.
c--
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      double precision ckms
      parameter (ckms = 1d-3*DCMKS)

      integer   icrd, ifrq, ilat, ilng, itype, otype
      double precision df, frq, vel
      character iframe*4, oframe*4, ttype*16

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
c  Standardise. For compatibility with old interface, use
      ttype = type
      call ucase(ttype)
      if (ttype.eq.'RADIO') then
        ttype = 'VELO'
      else if (ttype.eq.'OPTICAL') then
        ttype = 'FELO'
      else if (ttype.eq.'FREQUENCY') then
        ttype = 'FREQ'
      endif

      icrd = coLoc(lu,.false.)
      ifrq = frqax(icrd)
      if (ifrq.eq.0)
     *  call bug('f','Call to coVelSet for non-velocity axis')
      itype = cotype(ifrq,icrd)
c
c  Determine the type.
c
      if (ttype(1:4).eq.'FREQ') then
        otype = FRQTYP
      else if (ttype(1:4).eq.'VELO') then
        otype = VELTYP
      else if (ttype(1:4).eq.'FELO') then
        otype = FELTYP
      else
        call bug('f','Unrecognised conversion type, in coVelSet')
      endif
c
c Determine the reference frame conversion.
c
      if (ctype(ifrq,icrd)(5:5).eq.'-') then
        iframe = ctype(ifrq,icrd)(5:8)
      else
        iframe = ' '
      end if
      oframe = ttype(5:)
      if (oframe.eq.' ') oframe = iframe
      if (iframe.eq.' ') iframe = oframe

      if (otype.eq.itype .and. oframe.eq.iframe) return
      if (restfrq(icrd).le.0d0)
     *  call bug('f','Unable to do axis conversion as restfreq==0')
c
c Determine the frequency of the reference pixel
c
      if (itype.eq.FELTYP) then
        frq = restfrq(icrd) / (1d0 + crval(ifrq,icrd)/ckms)
        df  = -(cdelt(ifrq,icrd)/ckms) * frq * (frq/restfrq(icrd))
        frq = frq - restfrq(icrd)*vobs(icrd)/ckms
      else if (itype.eq.VELTYP) then
        frq = restfrq(icrd) * (1d0 - crval(ifrq,icrd)/ckms)
     *          - restfrq(icrd)*vobs(icrd)/ckms
        df  = -(cdelt(ifrq,icrd)/ckms) * restfrq(icrd)
      else
        frq = crval(ifrq,icrd)
        df  = cdelt(ifrq,icrd)
      endif
c
c  Determine velocity system conversion, if needed.
c
      if (oframe.ne.iframe) then
        if ((oframe.eq.'-HEL' .and. iframe.eq.'-LSR') .or.
     *      (oframe.eq.'-LSR' .and. iframe.eq.'-HEL')) then
          ilng = lngax(icrd)
          ilat = latax(icrd)
          if (ilng.eq.0 .or. ilat.eq.0) call bug('f',
     *      'Missing RA/DEC -- unable to do velocity frame conversion')
          if (ctype(ilng,icrd)(1:4).ne.'RA--' .or.
     *       ctype(ilat,icrd)(1:4).ne.'DEC-') call bug('f',
     *      'Missing RA/DEC -- unable to do velocity frame conversion')
          call coGetVel(crval(ilng,icrd),crval(ilat,icrd),epoch(icrd),
     *      vel)

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
c
c Perform the transformation
c
      if (otype.eq.FELTYP) then
        frq = frq + restfrq(icrd)*vobs(icrd)/ckms
        crval(ifrq,icrd) =  ckms*(restfrq(icrd)/frq - 1d0)
        cdelt(ifrq,icrd) = -ckms*(df/frq)*(restfrq(icrd)/frq)
        ctype(ifrq,icrd) = 'FELO' // oframe
      else if (otype.eq.VELTYP) then
        frq = frq + restfrq(icrd)*vobs(icrd)/ckms
        crval(ifrq,icrd) =  ckms*(1d0 - frq/restfrq(icrd))
        cdelt(ifrq,icrd) = -ckms*(df/restfrq(icrd))
        ctype(ifrq,icrd) = 'VELO' // oframe
      else
        crval(ifrq,icrd) = frq
        cdelt(ifrq,icrd) = df
        ctype(ifrq,icrd) = 'FREQ' // oframe
      endif
      cotype(ifrq,icrd) = otype

      end
c***********************************************************************
      subroutine coGetVel(raepo,decepo,epoch,vel)

      double precision raepo,decepo,epoch,vel

c  Determine the Sun's LSR velocity component in a given direction.
c
c-----------------------------------------------------------------------
      integer i
      double precision dec2000, ra2000, lmn2000(3), velsun(3)

c     External.
      double precision epo2jul
c-----------------------------------------------------------------------
      if (abs(epoch-2000d0).gt.0.001d0) then
        call precess(epo2jul(epoch,' '),raepo,decepo,
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
c* coAxGet -- Give information about a particular axis.
c& rjs
c: coordinates
c+
      subroutine coAxGet(lu,iax,ctypei,crpixi,crvali,cdelti)

      integer lu,iax
      character ctypei*(*)
      double precision crpixi,crvali,cdelti

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
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      if (iax.gt.naxis(icrd)) then
        ctypei = ' '
        crpixi = 1d0
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
c* coFindAx -- Locate a particular axis in the coordinate system.
c& rjs
c: coordinates
c+
      subroutine coFindAx(lu,axis,iax)

      integer lu,iax
      character axis*(*)

c  Locate an axis of a particular type. The type of the axis is given
c  by the "axis" parameter.
c
c  Input:
c    lu         Handle of the coordinate system.
c    axis       This can be either.  Case is unimportant.
c               Often this will be a normal FITS-style ctype value, with
c               or without the projection/rest-frame part.
c               If the projection/rest-frame part is omitted, "axis"
c               will match all projections/rest-frames.  For example,
c               axis = 'ra' matches 'ra---ncp' and 'ra---sin' whereas
c               axis = 'ra---sin' only matches 'ra---sin'.
c
c               It can also be one of:
c                 'spectral'   for FREQ, VELO and FELO axes
c                 'frequency'  as above, but only if there is enough
c                              information to convert to frequency.
c                 'latitude'   for DEC, GLAT and ELAT axes
c                 'longitude'  for RA, GLON and ELON axes.
c
c  Output:
c    iax        The axis index of the desired axis. A value of 0 is
c               returned if the axis is not found.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      logical   match
      integer   icrd, jax, length
      character type*16

c     Externals.
      integer   coLoc, len1
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      type = axis
      call ucase(type)
      length = len1(axis)
c
c  Do the special cases.
c
      iax = 0
      if (type.eq.'SPECTRAL') then
        iax = frqax(icrd)
      else if (type.eq.'FREQUENCY') then
        iax = frqax(icrd)
        if (iax.gt.0 .and. restfrq(icrd).le.0d0 .and.
     *      cotype(iax,icrd).ne.FRQTYP) iax = 0
      else if (type.eq.'LONGITUDE') then
        iax = lngax(icrd)
      else if (type.eq.'LATITUDE') then
        iax = latax(icrd)
      else if (length.gt.len1(ctype(1,icrd))) then
        continue
      else
        do jax = 1, naxis(icrd)
          if (type(1:length).eq.ctype(jax,icrd)(1:length)) then
            match =  length.eq.len(ctype(jax,icrd))
            if (.not.match) then
              match = ctype(jax,icrd)(length+1:).eq.' ' .or.
     *                ctype(jax,icrd)(length+1:length+1).eq.'-'
            endif

            if (match .and. iax.ne.0) call bug('f',
     *        'Multiple matching axes in coFndAx, for axis='//type)
            if (match) iax = jax
          endif
        enddo
      endif

      end
c***********************************************************************
c* coCompar - Compare two coordinate systems for likeness
c& rjs
c: coordinates
c+
      logical function coCompar(lu1,lu2,match)

      integer lu1,lu2
      character match*(*)

c  Compare two coordinate systems for likeness. This returns .true.
c  if they are alike, and .false. otherwise. The "match" argument
c  determines the tests performed for likeness.
c
c  Input:
c    lu1,lu2    Handles of the two coordinate systems to be compared.
c    match      This determines the tests for likeness to be applied.
c               Tolerances are 0.01%, except for match='approx' (see
c               below).  Possible values are:
c
c                 Value         Description
c                 -----         -----------
c                 'exact'       Check that ctype,crval,crpix,cdelt are
c                               identical.
c                 'projection'  Check that ctype,crval are the same.
c                               Thus it check whether the coordinate
c                               systems have the same projection and
c                               reference value.
c                 'offset'      Check ctype,crval,cdelt.  Thus this
c                               checks whether the coordinate systems
c                               are identical within a shift of the
c                               pixel coordinates.
c                 'approx'      Check whether the two coordinate systems
c                               are approximately the same (less than
c                               0.1 pixel difference at the reference
c                               pixel of the first system, cdelts that
c                               agree to within 1%, and compatible
c                               ctypes.
c                 'align'       crpixs are the same.
c
c  Output:
c    coCompar   True if the two coordinate systems are alike, and false
c               otherwise.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd1, icrd2, nax
      double precision dscr, x1(7), x2(7)

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd1 = coLoc(lu1,.false.)
      icrd2 = coLoc(lu2,.false.)
      coCompar = .false.
c
c  Check that the number of axes are the same.
c
      if (naxis(icrd1).ne.naxis(icrd2)) return
      nax = naxis(icrd1)
c
c  Switch to the right operation.
c
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
          if (ctype(iax,icrd1).ne.ctype(iax,icrd2))return
          dscr = 1d-4*min(abs(cdelt(iax,icrd1)),abs(cdelt(iax,icrd2)))
          if (abs(crval(iax,icrd1)-crval(iax,icrd2)).gt.dscr) return
          if (abs(cdelt(iax,icrd1)-cdelt(iax,icrd2)).gt.
     *           1d-4*abs(cdelt(iax,icrd1))) return
        enddo

      else if (match.eq.'align') then
        do iax = 1, nax
          if (abs(crpix(iax,icrd1)-crpix(iax,icrd2)).gt.1d-4)return
        enddo

      else if (match.eq.'approx') then
c       Are the two systems comparable?
        do iax = 1, nax
          if (ctype(iax,icrd1)(1:5).ne.ctype(iax,icrd2)(1:5))return
          if (abs(cdelt(iax,icrd1)-cdelt(iax,icrd2)).gt.
     *           1d-2*abs(cdelt(iax,icrd1))) return
        enddo
c
c  Determine the absolute pixel location, in system 2, of the reference
c  pixel, in system 1.
c
        do iax = 1, 7
          x1(iax) = 0d0
        enddo
        call coCvt(lu1,'op/op/op/op/op/op/op',x1,
     *                 'aw/aw/aw/aw/aw/aw/aw',x2)
        call coCvt(lu2,'aw/aw/aw/aw/aw/aw/aw',x2,
     *                 'ap/ap/ap/ap/ap/ap/ap',x1)
c
c  Check whether they more or less line up at the reference pixel.
c
        do iax = 1, nax
          if (abs(x1(iax)-crpix(iax,icrd1)).gt.0.1d0) return
        enddo

      else
        call bug('f','Unrecognised match operation, in coCompar')
      endif

      coCompar = .true.

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

c  Generate a linearised approximation to a non-linear coordinate
c  system.
c
c  Input:
c    lu         Handle of the input coordinate system.
c    x1         Input coordinate. The linearised approximation
c               is performed around this point.
c    in         String describing the format of x1 coordinate, as
c               per coCvt.
c    n          Number of axes to output.
c  Output:
c    ctypel )
c    crpixl )   Output coordinate descriptions.
c    crvall )
c    cdeltl )
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'co.h'

      integer   iax, icrd
      double precision delta, xp(MAXNAX), xp1(MAXNAX), xp2(MAXNAX),
     *          xw(MAXNAX), xw1(MAXNAX), xw2(MAXNAX)

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      if (sinrot(icrd).ne.0d0) then
        call bug('w','Cannot handle sky rotation')
      endif
c
c  Convert to absolute pixels.
c
      do iax = 1, naxis(icrd)
        xp(iax) = crpix(iax,icrd)
        xw(iax) = crval(iax,icrd)
      enddo
      call coCvt(lu,in,x1,'aw/...',xw)
      call coCvt(lu,in,x1,'ap/...',xp)
c
c  Increment by one pixel in all directions.
c
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
              if (delta.gt. DPI) delta = delta - DTWOPI
            endif

            cdeltl(iax) = 0.5d0 * cdeltl(iax)
            crpixl(iax) = xp(iax) - delta/cdeltl(iax)
            if (cotype(iax,icrd).eq.LNGTYP) then
              cdeltl(iax) = cdeltl(iax) * cos(crval(latax(icrd),icrd))
            end if
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
c* coPrint -- Print a summary of the coordinate conversion information.
c& rjs
c: coordinates
c+
      subroutine coPrint(lu)

      integer lu

c  Print out a description of a coordinate system.
c
c  Input:
c    lu         Handle of the coordinate system.
c--
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'

      integer   iax, icrd, p
      character aval*8, line*80, pols*4, radec*20, units*8

c     Externals.
      integer   coLoc
      character hangle*32, PolsC2P*2, rangle*32
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      do iax = 1, naxis(icrd)
        units = ' '
        aval = ctype(iax,icrd)
c
c  RA.
c
        if (aval(1:4).eq.'RA--') then
          radec = hangle(crval(iax,icrd))
          write (line, 10) aval, radec, crpix(iax,icrd),
     *      cdelt(iax,icrd)*DR2D*3600d0
 10       format (a8,3x,a11,f10.2,3x,1pe13.6,'  arcsec')
c
c  DEC, Galactic and Ecliptic coordinates.
c
        else if (aval(1:4).eq.'DEC-' .or.
     *           aval(1:4).eq.'GLON' .or.
     *           aval(1:4).eq.'GLAT' .or.
     *           aval(1:4).eq.'ELON' .or.
     *           aval(1:4).eq.'ELAT') then
          radec = rangle(crval(iax,icrd))
          write (line, 20) aval, radec, crpix(iax,icrd),
     *      cdelt(iax,icrd)*DR2D*3600d0
 20       format (a8,2x,a12,f10.2,3x,1pe13.6,'  arcsec')
c
c  STOKES.
c
        else if (aval(1:6).eq.'STOKES') then
          p = nint(crval(iax,icrd))
          if (p.eq.0) then
            pols = 'beam'
          else
            pols = PolsC2P(p)
          endif
          write (line, 30) aval, pols
 30       format (a8, 8x, a)
c
c  Others.
c
        else
          if (aval(1:4).eq.'FELO' .or.
     *        aval(1:4).eq.'VELO') then
            units = 'km/sec'
          else if (aval(1:4).eq.'FREQ') then
            units = 'GHz'
          else if (aval(1:3).eq.'UU-' .or.
     *             aval(1:3).eq.'VV-') then
            units = 'lambda'
          endif

          write (line, 40) aval, crval(iax,icrd), crpix(iax,icrd),
     *      cdelt(iax,icrd), units
 40       format (a8,2x,1pe13.6,0pf9.2,3x,1pe13.6,2x,a)
        endif

        call output(line)
      enddo

      end
c***********************************************************************
c* coPrjSet -- Set celestial projection type.
c& rjs
c: coordinates
c+
      subroutine coPrjSet(lu,p)

      integer lu
      character p*(*)

c  Set changes the coordinate system celestial projection type.  This
c  will rarely be a useful routine.  Probably its only use is for
c  changing projection type from SIN to NCP for old datasets.
c
c  Input:
c    lu         Handle of the coordinate system.
c    p          Projection type: ncp, sin, tan, arc, car, or gls.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd
      character base*5, proj*8

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)
      coproj(icrd) = p
      proj = p
      call ucase(proj)
      do iax = 1, naxis(icrd)
        base = ctype(iax,icrd)(1:5)
        if (base.eq.'RA---' .or.
     *      base.eq.'DEC--' .or.
     *      base.eq.'ELON-' .or.
     *      base.eq.'ELAT-' .or.
     *      base.eq.'GLON-' .or.
     *      base.eq.'GLAT-')
     *    ctype(iax,icrd)(6:) = proj
      enddo

      end
c***********************************************************************
c* coWrite -- Write out the coordinate description to an image dataset.
c& rjs
c: coordinates
c+
      subroutine coWrite(lu,tno)

      integer lu,tno

c  This writes out the coordinate system description to an image
c  dataset.
c
c  Input:
c    lu         Handle of the coordinate object.
c    tno        Handle of the output dataset.
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, icrd
      double precision dtemp
      character num*2

c     Externals.
      integer   coLoc
      character itoaf*2
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      do iax = 1, naxis(icrd)
        num = itoaf(iax)
        call wrhdd(tno,'crval'//num,crval(iax,icrd))
        call wrhdd(tno,'crpix'//num,crpix(iax,icrd))
        call wrhdd(tno,'cdelt'//num,cdelt(iax,icrd))
        call wrhda(tno,'ctype'//num,ctype(iax,icrd))
      enddo

      if (restfrq(icrd).ne.0d0) then
        call wrhdd(tno,'restfreq',restfrq(icrd))
      endif

      call wrhdd(tno,'vobs',vobs(icrd))
      if (epoch(icrd).gt.1800d0) then
        call wrhdr(tno,'epoch',real(epoch(icrd)))
      endif

      if (obstime(icrd).gt.0d0) then
        call wrhdd(tno,'obstime',obstime(icrd))
      endif

      if (sinrot(icrd).ne.0d0) then
        dtemp = atan2(sinrot(icrd),cosrot(icrd))
        call wrhdd(tno,'llrot',dtemp)
      endif

      if (frqscl(icrd)) then
        call wrhda(tno,'cellscal','1/F')
      else
        call wrhda(tno,'cellscal','CONSTANT')
      endif

      end
c***********************************************************************
      subroutine coCompat(ctype1,ctype2,ok)

      character ctype1*(*),ctype2*(*)
      logical ok

c  Check that two celestial coordinates represent a consistent pair.
c-----------------------------------------------------------------------
      character type1*16,type2*16,qual1*16,qual2*16
c-----------------------------------------------------------------------
c
c  "Sort" them.
c
      if (ctype1.lt.ctype2) then
        call coExt(ctype1,type1,qual1)
        call coExt(ctype2,type2,qual2)
      else
        call coExt(ctype2,type1,qual1)
        call coExt(ctype1,type2,qual2)
      endif
c
c  The projection code (characters 6:8) should be the same,
c  and make sure We should have a RA/DEC, GLAT/GLON, ELAT/ELON pair.
c
      ok = .true.
      if (qual1.ne.qual2) then
        ok = .false.
        call bug('w','Incompatible projection types')
      else if (.not.((type1.eq.'DEC'  .and. type2.eq.'RA')   .or.
     *               (type1.eq.'GLAT' .and. type2.eq.'GLON') .or.
     *               (type1.eq.'ELAT' .and. type2.eq.'ELON'))) then
        ok = .false.
        call bug('w','Incompatible latitude/longitude system')
      endif

      end
c***********************************************************************
      subroutine coExt(ctype,type,qual)

      character ctype*(*),type*(*),qual*(*)
c-----------------------------------------------------------------------
      logical   doqual
      integer   i, l, length

c     External.
      integer   len1
c-----------------------------------------------------------------------
      doqual = .false.
      l = 0
      type = ' '
      qual = ' '
      length = len1(ctype)
      do i = 1, length
        l = l + 1
        if (ctype(i:i).eq.'-') then
          l = 0
          doqual = .true.
        else if (doqual) then
          qual(l:l) = ctype(i:i)
        else
          type(l:l) = ctype(i:i)
        endif
      enddo

      end
c***********************************************************************
      subroutine coInitXY(icrd)

      integer icrd

c  Initialise the coordinate information for an image data-set.
c  Note that we have commented out the code to change SIN projections
c  into NCP for E-W telescopes.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, n
      double precision dtemp
      character num*2, cscal*16
c       logical ewdone,ew
c       character telescop*16

c     External.
      character itoaf*2
c       logical hdprsnt
c-----------------------------------------------------------------------
      call rdhdi(lus(icrd),'naxis',naxis(icrd),0)
      if (naxis(icrd).eq.0)
     *  call bug('f','Invalid value for NAXIS, in coInit')

      call rdhdd(lus(icrd),'restfreq',restfrq(icrd),0d0)
c       if (.not.hdprsnt(lus(icrd),'vobs'))
c    *    call bug('w','VOBS item missing -- assuming it is 0')
      call rdhdd(lus(icrd),'vobs',vobs(icrd),0d0)
      call rdhdd(lus(icrd),'epoch',epoch(icrd),0d0)
      call rdhdd(lus(icrd),'obstime',obstime(icrd),0d0)
      call rdhda(lus(icrd),'cellscal',cscal,'1/F')
      frqscl(icrd) = cscal.eq.'1/F'
      if (.not.frqscl(icrd) .and. cscal.ne.'CONSTANT') call bug('w',
     *  'Unrecognised cellscal value: '//cscal)

      call rdhdd(lus(icrd),'llrot',dtemp,0d0)
      cosrot(icrd) = cos(dtemp)
      sinrot(icrd) = sin(dtemp)

c       ewdone = .false.
      do iax = 1, naxis(icrd)
        num = itoaf(iax)
        call rdhdi(lus(icrd),'naxis'//num,n,1)
        call rdhdd(lus(icrd),'crval'//num,crval(iax,icrd),dble(n/2+1))
        call rdhdd(lus(icrd),'crpix'//num,crpix(iax,icrd),dble(n/2+1))
        call rdhdd(lus(icrd),'cdelt'//num,cdelt(iax,icrd),1d0)
        call rdhda(lus(icrd),'ctype'//num,ctype(iax,icrd),' ')
        if (cdelt(iax,icrd).eq.0d0) then
          if (ctype(iax,icrd).ne.' ') then
            call bug('w',
     *        'Axis increment (cdelt) is 0 for '//ctype(iax,icrd))
            call bug('w','Assuming an axis increment of 1')
          endif
          cdelt(iax,icrd) = 1d0
        endif

c         if (ctype(iax,icrd)(5:8).eq.'-SIN') then
c           if (.not.ewdone) then
c             call rdhda(lus(icrd),'telescop',telescop,' ')
c             ew = telescop.ne.' '
c             if (ew) call obspar(telescop,'ew',dtemp,ew)
c             if (ew) ew = dtemp.gt.0d0
c             ewdone = .true.
c           endif
c           if (ew) ctype(iax,icrd)(5:8) = '-NCP'
c         endif
      enddo

      end
c***********************************************************************
      subroutine coTyCvt(type,itype,proj)

      character type*(*), proj*(*)
      integer itype

c  Convert a coordinate type to an enumerated type.
c
c-----------------------------------------------------------------------
      include 'co.h'

      integer   NTYPES
      parameter (NTYPES = 16)

      logical   more
      integer   i, itypes(NTYPES), l, l1, l2
      character types(NTYPES)*8, umsg*64

c     Externals.
      integer   binsrcha, len1
c-----------------------------------------------------------------------
c  The list of recognised ctypes. Note this list MUST be in
c  alphabetic order.
c
      data (types(i),itypes(i),i=1,NTYPES)/
     *  'DEC     ',   LATTYP,
     *  'ELAT    ',   LATTYP,
     *  'ELON    ',   LNGTYP,
     *  'FELO    ',   FELTYP,
     *  'FELOCITY',   FELTYP,
     *  'FREQ    ',   FRQTYP,
     *  'GLAT    ',   LATTYP,
     *  'GLON    ',   LNGTYP,
     *  'POINTING',   LINEAR,
     *  'RA      ',   LNGTYP,
     *  'SDBEAM  ',   LINEAR,
     *  'STOKES  ',   LINEAR,
     *  'UU      ',   LINEAR,
     *  'VELO    ',   VELTYP,
     *  'VELOCITY',   VELTYP,
     *  'VV      ',   LINEAR/
c
c  Get the first part, and check for a match.
c
      itype = 0
      l = index(type,'-') - 1
      if (l.le.0) l = len1(type)
      if (l.gt.0) l = binsrcha(type(1:l),types,ntypes)
      if (l.gt.0) itype = itypes(l)
c
c  If its a latitude or longitude, check the last part for a known one.
c
      if (itype.eq.LATTYP .or. itype.eq.LNGTYP) then
        l2 = len1(type)
        l1 = l2
        more = .true.
        dowhile(l1.gt.1 .and. more)
          more = type(l1-1:l1-1).ne.'-'
          if (more) l1 = l1 - 1
        enddo
        if (type(l1:l2).eq.'NCP') then
          proj = 'ncp'
        else if (type(l1:l2).eq.'SIN') then
          proj = 'sin'
        else if (type(l1:l2).eq.'TAN') then
          proj = 'tan'
        else if (type(l1:l2).eq.'ARC') then
          proj = 'arc'
        else if (type(l1:l2).eq.'CAR') then
          proj = 'car'
        else if (type(l1:l2).eq.'GLS') then
          proj = 'gls'
        else
          umsg = 'Using cartesian projection for axis ' // type
          call bug('w',umsg)
          proj = 'car'
        endif
      else if (type.eq.' ') then
        itype = LINEAR
      else if (itype.eq.0) then
        l = len1(type)
        umsg = 'Assuming axis ' // type(1:l) //
     *         ' is a linear coordinate system'
c         call bug('w',umsg)
        itype = LINEAR
      endif

      end
c***********************************************************************
      subroutine coInitUV(icrd)

      integer icrd

c  Initialise the coordinate information for a visibility data-set.
c
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
      call uvrdvrd(lus(icrd),'epoch',epoch(icrd),0d0)
      call uvrdvrd(lus(icrd),'time',obstime(icrd),0d0)
      frqscl(icrd)  = .false.

      end
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
c
c  Locate the slot for this lu.
c
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
c
c  We did not find it. If we are allowed to allocate one, do so.
c
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

      call bug('f','Unable to find coordinate object in coInit')
      coLoc = 0

      end
c***********************************************************************
      subroutine coCrack(code,x1pix,x1off,defn,maxnax,n)

      integer maxnax,n,defn
      logical x1pix(maxnax),x1off(maxnax)
      character code*(*)

c  Decode a coordinate conversion specifier code.
c
c  Input:
c    code
c    maxnax
c  Output:
c    x1pix,x1off
c    n
c-----------------------------------------------------------------------
      logical new,pad
      integer i
      character c*1
c-----------------------------------------------------------------------
      n = 0
      new = .true.
      pad = .false.

      do i = 1, len(code)
        c = code(i:i)
        if (c.le.' ') then
          continue
        else if (c.eq.'/') then
          new = .true.
        else if (c.eq.'.') then
          pad = .true.
        else
          if (new) then
            n = n + 1
            if (n.gt.maxnax) call bug('f','Too many axes, in coCvt')
            x1pix(n) = .false.
            x1off(n) = .false.
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
            call bug('f','Unrecognised conversion code, in coCvt')
          endif
        endif
      enddo

      if (pad .and. n.ge.1 .and. n.lt.defn) then
        do i = n+1, defn
          x1pix(i) = x1pix(n)
          x1off(i) = x1off(n)
        enddo
        n = defn
      endif

      end
c***********************************************************************
      subroutine coFqFac(x1,type,crval,crpix,cdelt,vobs,x1off,x1pix,
     *  scal)

      double precision x1,crval,crpix,cdelt,vobs,scal
      logical x1off,x1pix
      character type*4

c  Determine the frequency-dependent scaling factor of the increments in
c  the RA and DEC axes.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision ckms
      parameter (ckms = 1d-3 * DCMKS)

      double precision x
c-----------------------------------------------------------------------
c
c  Convert to offset units.
c
      if (.not.x1off) then
        if (x1pix) then
          x = x1 - crpix
        else
          x = x1 - crval
        endif
      else
        x = x1
      endif
      if (x1pix) x = x * cdelt

      if (x.eq.0d0) then
        scal = 1d0
      else if (type.eq.'FREQ') then
        scal = crval / (crval + x)
      else if (type.eq.'VELO') then
        scal = (ckms - (crval + vobs))/(ckms - (crval + x + vobs))
      else if (type.eq.'FELO') then
        if (x1pix) x = x  / (1d0 - x / (ckms + crval))
        scal = vobs*(1d0 + (crval+x)/ckms)*(1d0 + crval/ckms)
        scal = (ckms + crval + x - scal) / (ckms + crval - scal)
      else
        call bug('f','Unrecognised axis type in coFqFac: '//type)
      endif

      end
c***********************************************************************
      subroutine coFelo(x10,x2,crval,crpix,cdelt,
     *                                x1pix,x1off,x2pix,x2off)

      double precision x10,x2,crval,crpix,cdelt
      logical x1pix,x1off,x2pix,x2off

c  Convert between pixels and velocities, to a axes in the optical
c  velocity convention (but derived from data measured at equal
c  frequency increments).
c
c  Input:
c    x10
c    crval,crpix,cdelt
c    x1pix,x1off,x2pix,x2off
c  Output:
c    x2
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision ckms
      parameter (ckms = 1d-3 * DCMKS)

      double precision t, x1
c-----------------------------------------------------------------------
c
c  Convert from absolute to offset units, if required.
c
      if (.not.x1off) then
        if (x1pix) then
          x1 = x10 - crpix
        else
          x1 = x10 - crval
        endif
      else
        x1 = x10
      endif
c
c  Do the conversion.
c
      t = ckms + crval
      if (x1pix.eqv.x2pix) then
        x2 = x1
c
c  Convert from pixels to optical velocity.
c
      else if (x1pix) then
        x2 = cdelt * x1 / (1d0 - cdelt*x1/t)
c
c  Convert from optical velocity to pixels.
c
      else
        x2 = x1 / cdelt * t / (ckms + x1 + crval)
      endif
c
c  Convert from offset to absolute units, if required.
c
      if (.not.x2off) then
        if (x2pix) then
          x2 = x2 + crpix
        else
          x2 = x2 + crval
        endif
      endif

      end
c***********************************************************************
      subroutine coCelest(x10,y10,x2,y2,proj,cosrot,sinrot,
     *  crval1,crpix1,cdelt1,crval2,crpix2,cdelt2,
     *  x1pix,y1pix,x2pix,y2pix,x1off,y1off,x2off,y2off,valid)

      double precision x10,y10,x2,y2,cosrot,sinrot
      character proj*(*)
      double precision crval1,crval2,crpix1,crpix2,cdelt1,cdelt2
      logical x1pix,y1pix,x2pix,y2pix
      logical x1off,y1off,x2off,y2off,valid

c  Convert celestial coordinates between grid and world coordinates.
c
c  Input:
c    x10,y10    Input coordinate, some combination of pixel or celestial
c               coordinate elements.
c    proj       Projection code: ncp, sin, tan, arc, car, or gls.
c    x1pix,y1pix,x2pix,y2pix
c               True if the particular coordinate is a pixel coordinate.
c    x1off,y1off,x2off,y2off
c               True if the particular coordinate element is an offset.
c
c    cdelt1,crval1,crpix1: cdelt,crval and crpix values.
c    cdelt2,crval2,crpix2
c  Output:
c    x2,y2
c    valid      False if an illegal coordinate conversion was attempted.
c
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision clat0, x1, y1
c-----------------------------------------------------------------------
      clat0 = cos(crval2)
c
c  Convert from offset to absolute, if needed.
c
      x1 = x10
      y1 = y10
      if (x1off) then
        if (x1pix) then
          x1 = x1 + crpix1
        else
          x1 = x1 / clat0 + crval1
        endif
      endif
      if (y1off) then
        if (y1pix) then
          y1 = y1 + crpix2
        else
          y1 = y1 + crval2
        endif
      endif
c
c  Convert from x,y to RA,DEC.
c
      if ((x1pix.eqv.x2pix) .and. (y1pix.eqv.y2pix)) then
        continue

      else if (x1pix .and. y1pix) then
        call coxy2ll(x1,y1,x2,y2,
     *    crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,proj,
     *    valid)
c
c  Handle a mixed conversion.
c
      else if (x1pix) then
        call coMixed(x1,y1,x2,y2,
     *    crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,proj,
     *    .true.,valid)

      else if (y1pix) then
        call coMixed(x1,y1,x2,y2,
     *    crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,proj,
     *    .false.,valid)
c
c  Convert from RA,DEC to x,y.
c
      else
        call coll2xy(x1,y1,x2,y2,
     *    crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,proj,
     *    valid)
      endif
c
c  We now have the full set of possibilities. Return the variety the
c  caller really wanted.
c
      if (x1pix.eqv.x2pix) x2 = x1
      if (y1pix.eqv.y2pix) y2 = y1
c
c  Convert from absolute to offset coordinates, if needed.
c  Also convert RA and offset RA to a standard range.
c
      if (x2off) then
        if (x2pix) then
          x2 = x2 - crpix1
        else
          x2 = x2 - crval1
          if (x2.gt.DPI) then
            x2 = x2 - DTWOPI
          else if (x2.lt.-DPI) then
            x2 = x2 + DTWOPI
          endif
          x2 = x2 * clat0
        endif

      else if (.not.x2pix) then
        if (x2.ge.DTWOPI) then
          x2 = x2 - DTWOPI
        else if (x2.lt.0d0) then
          x2 = x2 + DTWOPI
        endif
      endif

      if (y2off) then
        if (y2pix) then
          y2 = y2 - crpix2
        else
          y2 = y2 - crval2
        endif
      endif

      end
c***********************************************************************
      subroutine coLinear(crval1,crpix1,cdelt1,x1pix,x1off,x2pix,x2off,
     *  bscal,bzero)

      logical x1pix,x1off,x2pix,x2off
      double precision crpix1,cdelt1,crval1
      double precision bscal,bzero

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
c
c  Convert from absolute to offset units, if needed.
c
      if (.not.x1off) then
        if (x1pix) then
          bzero = -crpix1
        else
          bzero = -crval1
        endif
      endif
c
c  Convert between offset world and offset pixel units, if needed.
c
      if (x1pix.neqv.x2pix) then
        if (x1pix) then
          bscal = cdelt1
        else
          bscal = 1d0/cdelt1
        endif
        bzero = bscal * bzero
      endif
c
c  Convert from offset to absolute units, if needed.
c
      if (.not.x2off) then
        if (x2pix) then
          bzero = bzero + crpix1
        else
          bzero = bzero + crval1
        endif
      endif

      end
c***********************************************************************
      subroutine coll2xy(lng,lat,p1,p2,
     *  crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,proj,
     *  valid)

      double precision lng,lat,p1,p2
      double precision crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,
     *          sinrot
      character proj*(*)
      logical   valid

c  (lng,lat) are the celestial coordinates, i.e. (alpha,delta) in the
c            notation of AIPS memo 27.
c  (p1,p2)   are pixel coordinates.
c
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision clat0, costhe, dlng, dtemp, L, lat0, lng0, M,
     *       slat0, t, theta
c-----------------------------------------------------------------------
c     Celestial coordinates of the reference point - (alpha_0,delta_0)
c     in AIPS memo 27.
      lng0 = crval1
      lat0 = crval2

      slat0 = sin(lat0)
      clat0 = cos(lat0)
      dlng = lng - lng0

      dlng = mod(dlng,DTWOPI)
      if (dlng.lt.-DPI) then
        dlng = dlng + DTWOPI
      else if (dlng.gt.DPI) then
        dlng = dlng - DTWOPI
      endif

      valid = .true.
      if (proj.eq.'car') then
        L = dlng * clat0
        M = (lat - lat0)

      else if (proj.eq.'gls') then
        L = dlng * cos(lat)
        M = (lat - lat0)
      else
        costhe = sin(lat)*slat0 + cos(lat)*clat0*cos(dlng)

        if (costhe.lt.0d0) then
          valid = .false.

        else if (proj.eq.'ncp') then
          if (lat*lat0.le.0d0) then
            valid = .false.
          else
            L = sin(dlng) * cos(lat)
            M = (clat0 - cos(lat)*cos(dlng))/slat0
          endif

        else if (proj.eq.'sin') then
          L = sin(dlng) * cos(lat)
          M = (sin(lat)*clat0 - cos(lat)*slat0*cos(dlng))

        else if (proj.eq.'tan') then
          L = sin(dlng) * cos(lat) / costhe
          M = (sin(lat)*clat0 - cos(lat)*slat0*cos(dlng)) / costhe

        else if (proj.eq.'arc') then
          theta = acos(costhe)
          if (theta.eq.0d0) then
            t = 1d0
          else
            t = theta / sin(theta)
          endif

          L = sin(dlng) * cos(lat) * t
          M = (sin(lat)*clat0 - cos(lat)*slat0*cos(dlng)) * t

        endif
      endif

      if (valid) then
        dtemp = L*cosrot + M*sinrot
        M     = M*cosrot - L*sinrot
        L     = dtemp
        p1 = L / cdelt1 + crpix1
        p2 = M / cdelt2 + crpix2
      else
        p1 = 0d0
        p2 = 0d0
      endif

      end
c***********************************************************************
      subroutine coxy2ll(p1,p2,lng,lat,
     *  crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,proj,
     *  valid)

      double precision p1,p2,lng,lat
      double precision crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,
     *                 sinrot
      character proj*(*)
      logical valid


c  (p1,p2)   are pixel coordinates.
c  (lng,lat) are the celestial coordinates, i.e. (alpha,delta) in the
c            notation of AIPS memo 27.
c-----------------------------------------------------------------------
      double precision L,M,dlng,t,dtemp,lng0,lat0,clat0,slat0,s,r,t1
c-----------------------------------------------------------------------
      lng0 = crval1
      lat0 = crval2

      slat0 = sin(lat0)
      clat0 = cos(lat0)

      valid = .true.
      L = (p1 - crpix1) * cdelt1
      M = (p2 - crpix2) * cdelt2
      dtemp = L*cosrot - M*sinrot
      M = M*cosrot + L*sinrot
      L = dtemp

      if (proj.eq.'ncp') then
        t = clat0 - M*slat0
        dlng = atan2(L,t)
        if (abs(t/cos(dlng)).gt.1d0) then
          valid = .false.
        else
          lat = sign(acos(t/cos(dlng)),lat0)
        endif

      else if (proj.eq.'sin') then
        t1 = 1d0 - L*L - M*M
        if (t1.lt.0d0) then
          valid = .false.
        else
          t = sqrt(t1)
          t1 = M*clat0 + slat0*t
          if (abs(t1).gt.1d0) then
            valid = .false.
          else
            lat = asin(M*clat0+slat0*t)
            dlng = atan2(L,(clat0*t - M*slat0))
          endif
        endif

      else if (proj.eq.'tan') then
        t = clat0 - M*slat0
        dlng = atan2(L,t)
        lat  = atan(cos(dlng)/t * (M*clat0 + slat0))

      else if (proj.eq.'arc') then
        t = sqrt(L*L + M*M)
        s = cos(t)
        if (t.gt.0d0) then
          t = sin(t) / t
        else
          t = 1d0
        endif

        r = M*clat0 * t + slat0*s
        if (abs(r).gt.1d0) then
          valid = .false.
        else
          dlng = atan2(L*t*clat0, s-r*slat0)
          lat  = asin(r)
        endif

      else if (proj.eq.'car') then
        lat  = lat0 + M
        dlng = L / clat0

      else if (proj.eq.'gls') then
        lat  = lat0 + M
        dlng = L / cos(lat)
      endif

      if (valid) then
        lng = dlng + lng0
      else
        lng = 0d0
        lat = 0d0
      endif

      end
c***********************************************************************
      subroutine coMixed(x1,y1,x2,y2,
     *  crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,
     *  proj,pw2wp,valid)

      logical pw2wp,valid
      double precision cdelt1, cdelt2, crpix1, crpix2, crval1, crval2,
     *        cosrot, sinrot, x1, x2, y1, y2
      character proj*(*)
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer MAXITER
      real    TOL
      parameter (MAXITER = 1000, TOL = 1e-5)

      integer i, i1, i2, niter
      real    delta
      double precision d2, gamma, p(2), pd(2), pdd(2), pdr(2), pr(2),
     *        w(2), wd(2), wdd(2), wdr(2), wr(2)
c-----------------------------------------------------------------------
      x2 = 0d0
      y2 = 0d0
      valid = .true.

      if (pw2wp) then
c       (pixel,world) to (world,pixel).
        p(1) = x1
        p(2) = crpix2
        w(2) = y1
        i1 = 1
        i2 = 2
      else
c       (world,pixel) to (pixel,world).
        p(1) = crpix1
        p(2) = y1
        w(1) = x1
        i1 = 2
        i2 = 1
      endif

      delta = 1.0
      niter = 0
      dowhile (delta.gt.TOL .and. niter.lt.MAXITER)
        niter = niter + 1
c       Do the conversion at three points.
        call coxy2ll(p(1),p(2),wd(1),wd(2),
     *         crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,
     *         proj,valid)
        if (.not.valid) return

        w(i1) = wd(i1)
        call coll2xy(w(1),w(2),pd(1),pd(2),
     *         crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,
     *         proj,valid)
        if (.not.valid) return

        pdd(1) = 0.5d0*(p(1)+pd(1) + abs(cdelt2/cdelt1)*abs(p(2)-pd(2)))
        pdd(2) = 0.5d0*(p(2)+pd(2) + abs(cdelt1/cdelt2)*abs(p(1)-pd(1)))
        call coxy2ll(pdd(1),pdd(2),wdd(1),wdd(2),
     *         crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,
     *         proj,valid)
        if (.not.valid) return
c
c  Generate relative coordinates.
c
        do i = 1, 2
          pr(i)  = p(i)  - pdd(i)
          pdr(i) = pd(i) - pdd(i)
          wr(i)  = w(i)  - wdd(i)
          if (wr(i).gt.PI)  wr(i) = wr(i) - DTWOPI
          if (wr(i).lt.-PI) wr(i) = wr(i) + DTWOPI
          wdr(i) = wd(i) - wdd(i)
          if (wdr(i).gt.PI)  wdr(i) = wdr(i) - DTWOPI
          if (wdr(i).lt.-PI) wdr(i) = wdr(i) + DTWOPI
        enddo
c
c  Update the current estimate.
c
        gamma = pr(i1)*wr(i2) - pdr(i1)*wdr(i2)
        if (abs(gamma).le.0d0) then
          d2 = pr(i2)
        else
          d2 = (pr(i1)*(pr(i2)*wr(i2) - pdr(i2)*wdr(i2)) +
     *          wr(i2)*(pr(i1)*pdr(i2) - pr(i2)*pdr(i1))) / gamma
        endif
        delta = abs(d2-pr(i2))
        p(i2) = d2 + pdd(i2)
      enddo

c       if (delta.gt.tol) call bug('f','Failed to converge, in coMixed')
      call coxy2ll(p(1),p(2),w(1),w(2),
     *       crpix1,cdelt1,crval1,crpix2,cdelt2,crval2,cosrot,sinrot,
     *       proj,valid)

      if (.not.valid) then
        x2 = 0d0
        y2 = 0d0
      else if (pw2wp) then
        x2 = w(1)
        y2 = p(2)
      else
        x2 = p(1)
        y2 = w(2)
      endif

      end
