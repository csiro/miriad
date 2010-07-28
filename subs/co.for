c***********************************************************************
c
c  A set of routines to convert between differing coordinate systems.
c  User callable routines are:
c
c    subroutine coInit(lu)
c    subroutine coDup(lin,lout)
c    subroutine coRaDec(lu,proj,ra0,dec0)
c    subroutine coCreate(lu)
c    subroutine coAltPrj(lu)
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
c    subroutine coGetD(lu,object,value)
c    subroutine coGetA(lu,object,value)
c    subroutine coSetD(lu,object,value)
c    subroutine coSetA(lu,object,value)
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
c                 specification. Add coGetD. Improve coVelSet. Support
c                 gls projection.
c    rjs  14jul97 Fix bug in covelset introduced on above date.
c    rjs  21jul97 More robust to bad freq values. Added coSetA.
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
c    rjs  15jul10 Correct recent bug introduced in coVelSet.  Correct
c                 order of "save" statement in coloc.
c
c $Id: co.for,v 1.7 2010/07/28 07:12:41 cal103 Exp $
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

c     Image or visibility data set?
      if (hdprsnt(lus(icrd),'visdata')) then
c       Apparently a visibility data set.
        call coInitUV(icrd)
      else if (hdprsnt(lus(icrd),'image')) then
c       Apparently an image.
        call coInitXY(icrd)
      else
        call bug('f','Unrecognised dataset type, in coInit')
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
      eqnox(icrd2)   = eqnox(icrd1)
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
c    proj       Projection code: NCP, SIN, etc.
c    ra0,dec0   RA,DEC of the reference point.
c  Output:
c    lu         Handle of the output coordinate object.
c--
c-----------------------------------------------------------------------
      character ctypei*8
c-----------------------------------------------------------------------
      call coCreate(lu)

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
      eqnox(icrd)   = 0d0
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
c* coSetD -- Set the value in the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coSetD(lu,object,value)

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
        eqnox(icrd) = value
      else if (obj.eq.'obstime') then
        obstime(icrd) = value
      else
        call bug('f','Unrecognised object in coSetD')
      endif

      end
c***********************************************************************
c* coSetA -- Set the value in the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coSetA(lu,object,value)

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
c* coGetD -- Get the value from the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coGetD(lu,object,value)

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
        value = eqnox(icrd)
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
        call bug('f','Unrecognised object in coGetD')
      endif

      end
c***********************************************************************
c* coGetA -- Get the value from the guts of the coordinate routines.
c& rjs
c: coordinates
c+
      subroutine coGetA(lu,object,value)

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
        call bug('f','Unrecognised object in coGetA')
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

c  Translate the oft-used, non-standard implementation of the CAR
c  projection.  This is essentially a simple linear axis pair except
c  with an implicit cos(lat) rescaling on longitude.
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
c--
c-----------------------------------------------------------------------
      include 'co.h'

      integer   icrd, ilat, ilng
      double precision lat0
      character lat*8, pcode*3

c     External.
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
c* coReinit -- Finish initialising a coordinate object.
c& rjs
c: coordinates
c+
      subroutine coReinit(lu)

      integer lu

c  Finish initialising a coordinate object.  Identifies the celestial
c  axes, if any, checks for consistency and sets up the celprm struct
c  for WCSLIB.
c
c  Input:
c    lu         Handle of the coordinate object.
c--
c-----------------------------------------------------------------------
      include 'co.h'
      include 'mirconst.h'
      include 'wcslib/prj.inc'

      logical   ok
      integer   iax, icrd, ilat, ilng, prj(PRJLEN), status
      double precision lat0
      character lng*8, lat*8, pcode1*3, pcode2*3

      integer   iarg(2)
      double precision darg
      equivalence (iarg, darg)

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

c     Convert to enumerated coordinate types.
      latax(icrd) = 0
      lngax(icrd) = 0
      frqax(icrd) = 0

      ok = .true.
      do iax = 1, naxis(icrd)
        call coTyCvt(ctype(iax,icrd), cotype(iax,icrd))
      enddo

c     Find the longitude, latitude, and spectral axes.
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
            ok = frqax(icrd).eq.0
            frqax(icrd) = iax
          endif
        endif
      enddo

c     Guard against multiple longitude, latitude, or spectral axes.
      if (.not.ok) then
        call bug('w', 'Multiple longitude, latitude, or spectral axes')
      endif

c     Set up the celestial axes.
      ilng = lngax(icrd)
      ilat = latax(icrd)
      if (ok) then
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
c           Set up the celprm struct.
            status = celini(cel(1,icrd))

c           Use darg/iarg equivalence to appease the Fortran compiler.
            darg = crval(ilng,icrd)*DR2D
            status = celput(cel(1,icrd), cel_ref, iarg, 1)
            darg = crval(ilat,icrd)*DR2D
            status = celput(cel(1,icrd), cel_ref, iarg, 2)

            status = celget(cel(1,icrd), cel_prj, prj)

            call ucase(pcode1)
            if (pcode1.eq.'NCP') then
c             Convert NCP to SIN.
              lat0 = crval(ilat,icrd)
              if (lat0.eq.0d0) then
                call bug('f', 'Invalid NCP projection')
              endif

              status = prjput(prj, prj_code, 3HSIN, 0)
              darg = 0d0
              status = prjput(prj, prj_pv, iarg, 1)
              darg = cos(lat0)/sin(lat0)
              status = prjput(prj, prj_pv, iarg, 2)

            else if (pcode1.eq.'GLS') then
c             Convert GLS to SFL.
              status = celput(cel(1,icrd), cel_offset, 1, 0)
              darg = crval(ilng,icrd)*DR2D
              status = celput(cel(1,icrd), cel_phi0,   iarg, 0)
              darg = crval(ilat,icrd)*DR2D
              status = celput(cel(1,icrd), cel_theta0, iarg, 0)
              status = prjput(prj, prj_code, 3HSFL, 0)

            else
              status = prjput(prj, prj_code, pcode1, 0)
c             Projection parameters to come...
            endif

            status = celput(cel(1,icrd), cel_prj, prj, 0)
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
        frqax(icrd) = 0
      endif

      end
c***********************************************************************
      subroutine coExt(ctype, type, pcode)

      character ctype*(*), type*(*), pcode*(*)

c  Decompose CTYPE into the coordinate type and projection code.
c-----------------------------------------------------------------------
      logical   docode
      integer   i, j

c     External.
      integer   len1
c-----------------------------------------------------------------------
      type  = ' '
      pcode = ' '
      docode = .false.

      j = 0
      do i = 1, len1(ctype)
        j = j + 1
        if (ctype(i:i).eq.'-') then
          j = 0
          docode = .true.
        else if (docode) then
          pcode(j:j) = ctype(i:i)
        else
          type(j:j)  = ctype(i:i)
        endif
      enddo

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
          call coCelest(cel(1,icrd), x1(ilng),x1(ilat),x2(ilng),
     *      x2(ilat), cosrot(icrd),sinrot(icrd),
     *      crpix(ilng,icrd),scal*cdelt(ilng,icrd),
     *      crpix(ilat,icrd),scal*cdelt(ilat,icrd),
     *      x1pix(ilng),x1pix(ilat),x2pix(ilng),x2pix(ilat),
     *      x1off(ilng),x1off(ilat),x2off(ilng),x2off(ilat),valid)
        else if (ilat.le.n) then
          call coCelest(cel(1,icrd), 0d0,x1(ilat),temp,x2(ilat),
     *      cosrot(icrd),sinrot(icrd),
     *      crpix(ilng,icrd),scal*cdelt(ilng,icrd),
     *      crpix(ilat,icrd),scal*cdelt(ilat,icrd),
     *      .true.,x1pix(ilat),.true.,x2pix(ilat),
     *      .true.,x1off(ilat),.true.,x2off(ilat),valid)
        else
          call coCelest(cel(1,icrd), x1(ilng),0d0,x2(ilng),temp,
     *      cosrot(icrd),sinrot(icrd),
     *      crpix(ilng,icrd),scal*cdelt(ilng,icrd),
     *      crpix(ilat,icrd),scal*cdelt(ilat,icrd),
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
      include 'wcslib/prj.inc'

      integer   icrd, prj(PRJLEN), status
      double precision clat, clat0, clng, fac, lat, lat0, lng, lng0,
     *          slat, slat0, slng, x2(MAXNAX)
      character pcode*3

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
c  Convert the user's coordinate to absolute world coordinates.
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

      status = celget(cel(1,icrd), cel_prj, prj)
      status = prjget(prj, prj_code, pcode)

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
        call coCelest(cel(1,icrd), x1,0d0,x2,dtemp,
     *    cosrot(icrd),sinrot(icrd),
     *    crpix(ilng,icrd),cdelt(ilng,icrd),
     *    crpix(ilat,icrd),cdelt(ilat,icrd),
     *    x1pix,x1pix,x2pix,x2pix,x1off,.true.,x2off,.true.,valid)
c
c  Convert a DEC axis.
c
      else if (cotype(iax,icrd).eq.LATTYP) then
        ilng = lngax(icrd)
        ilat = latax(icrd)
        call coCelest(cel(1,icrd), 0d0,x1,dtemp,x2,
     *    cosrot(icrd),sinrot(icrd),
     *    crpix(ilng,icrd),cdelt(ilng,icrd),
     *    crpix(ilat,icrd),cdelt(ilat,icrd),
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
      character ctype1*8, ctype2*8, iframe*4, oframe*4, ttype*16

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
      subroutine coGetVel(raepo,decepo,eqnox,vel)

      double precision raepo,decepo,eqnox,vel

c  Determine the Sun's LSR velocity component in a given direction.
c
c-----------------------------------------------------------------------
      integer i
      double precision dec2000, ra2000, lmn2000(3), velsun(3)

c     External.
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
      character ctypei*8, line*80, pols*4, radec*20, units*8

c     Externals.
      integer   coLoc
      character hangle*32, PolsC2P*2, rangle*32
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      do iax = 1, naxis(icrd)
        units = ' '
        ctypei = ctype(iax,icrd)

        if (ctypei.eq.'RA' .or. ctypei(1:4).eq.'RA--') then
c         Time.
          radec = hangle(crval(iax,icrd))
          write (line, 10) ctypei, radec, crpix(iax,icrd),
     *      cdelt(iax,icrd)*DR2D*3600d0
 10       format (a8,3x,a11,f10.2,3x,1pe13.6,'  arcsec')

        else if (ctypei.eq.'DEC' .or. ctypei(1:4).eq.'DEC-' .or.
     *           ctypei(1:4).eq.'GLON' .or.
     *           ctypei(1:4).eq.'GLAT' .or.
     *           ctypei(1:4).eq.'ELON' .or.
     *           ctypei(1:4).eq.'ELAT') then
c         Angle.
          radec = rangle(crval(iax,icrd))
          write (line, 20) ctypei, radec, crpix(iax,icrd),
     *      cdelt(iax,icrd)*DR2D*3600d0
 20       format (a8,2x,a12,f10.2,3x,1pe13.6,'  arcsec')

        else if (ctypei(1:6).eq.'STOKES') then
c         Polarization code.
          p = nint(crval(iax,icrd))
          if (p.eq.0) then
            pols = 'beam'
          else
            pols = PolsC2P(p)
          endif
          write (line, 30) ctypei, pols
 30       format (a8, 8x, a)

        else
          if (ctypei(1:4).eq.'FELO' .or.  ctypei(1:4).eq.'VELO') then
c           Velocity.
            units = 'km/sec'
          else if (ctypei(1:4).eq.'FREQ') then
c           Frequency.
            units = 'GHz'
          else if (ctypei(1:3).eq.'UU-' .or. ctypei(1:3).eq.'VV-') then
c           Visibility coordinates (wavelengths).
            units = 'lambda'
          endif

          write (line, 40) ctypei, crval(iax,icrd), crpix(iax,icrd),
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
      subroutine coPrjSet(lu, code)

      integer lu
      character code*(*)

c  Set the coordinate system celestial projection type.
c
c  Input:
c    lu         Handle of the coordinate system.
c    code       Projection code: NCP, SIN, etc. or blank, which on
c               interpretation will be translated to CAR.
c--
c-----------------------------------------------------------------------
      include 'co.h'
      include 'wcslib/prj.inc'

      integer   iax, icrd, prj(PRJLEN), status
      character ctypei*16, pcode*3

c     External.
      integer   coLoc
c-----------------------------------------------------------------------
      icrd = coLoc(lu,.false.)

      pcode = code
      call ucase(pcode)

      status = celget(cel(1,icrd), cel_prj, prj)
      status = prjput(prj, prj_code, pcode, 0)
      status = celput(cel(1,icrd), cel_prj, prj, 0)

      if (pcode.ne.' ') then
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

      else
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
      endif

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
      if (eqnox(icrd).gt.1800d0) then
        call wrhdr(tno,'epoch',real(eqnox(icrd)))
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
      subroutine coInitXY(icrd)

      integer icrd

c  Initialise the coordinate information for an image data-set.
c-----------------------------------------------------------------------
      include 'co.h'

      integer   iax, n
      double precision dtemp
      character num*2, cscal*16

c     External.
      character itoaf*2
c-----------------------------------------------------------------------
      call rdhdi(lus(icrd),'naxis',naxis(icrd),0)
      if (naxis(icrd).eq.0)
     *  call bug('f','Invalid value for NAXIS, in coInit')

      call rdhdd(lus(icrd),'restfreq',restfrq(icrd),0d0)
      call rdhdd(lus(icrd),'vobs',vobs(icrd),0d0)
      call rdhdd(lus(icrd),'epoch',eqnox(icrd),0d0)
      call rdhdd(lus(icrd),'obstime',obstime(icrd),0d0)
      call rdhda(lus(icrd),'cellscal',cscal,'1/F')
      frqscl(icrd) = cscal.eq.'1/F'
      if (.not.frqscl(icrd) .and. cscal.ne.'CONSTANT') call bug('w',
     *  'Unrecognised cellscal value: '//cscal)

      call rdhdd(lus(icrd),'llrot',dtemp,0d0)
      cosrot(icrd) = cos(dtemp)
      sinrot(icrd) = sin(dtemp)

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
      enddo

      end
c***********************************************************************
      subroutine coTyCvt(ctypei, itype)

      character ctypei*(*)
      integer itype

c  Convert a coordinate type to an enumerated type.
c
c-----------------------------------------------------------------------
      include 'co.h'

      integer   NTYPES, NPCODE
      parameter (NTYPES = 16, NPCODE = 29)

      integer   i, itypes(NTYPES), j, k
      character pcode*3, pcodes(NPCODE)*3, ctypes(NTYPES)*8, umsg*64

c     Externals.
      integer   binsrcha, len1

c     Recognised ctypes; this list MUST be in alphabetic order.
      data (ctypes(i),itypes(i),i=1,NTYPES)/
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

c     Recognized projection codes.
      data pcodes /
     *  'AZP', 'SZP', 'TAN', 'STG', 'SIN', 'NCP', 'ARC', 'ZPN',
     *  'ZEA', 'AIR', 'CYP', 'CEA', 'CAR', 'MER', 'COP', 'COE',
     *  'COD', 'COO', 'SFL', 'GLS', 'PAR', 'MOL', 'AIT', 'BON',
     *  'PCO', 'TSC', 'CSC', 'QSC', 'HPX'/
c-----------------------------------------------------------------------
      if (ctypei.eq.' ') then
        itype = LINEAR
        return
      end if

c     Try to identify the first part.
      itype = 0
      k = index(ctypei,'-')
      if (k.eq.1) then
c       Dispense with this weirdness.
        k = len1(ctypei)
        umsg = 'Assuming the ' // ctypei(1:k) // ' axis is linear.'
        call bug('w',umsg)
        return
      endif

      if (k.eq.0) then
        j = len1(ctypei)
      else
        j = k - 1
      endif

      i = binsrcha(ctypei(1:j), ctypes, ntypes)
      if (i.gt.0) itype = itypes(i)

      if (itype.eq.0) then
c       Unrecognized, assume it's linear.
        itype = LINEAR

      else if (itype.eq.LNGTYP .or. itype.eq.LATTYP) then
c       Celestial axis, get the projection code.
        if (k.eq.0) then
c         No projection code = simple linear axis, not CAR!
          return
        endif

        k = len1(ctypei)
        do j = k-1, 1, -1
          if (ctypei(j:j).eq.'-') go to 10
        enddo

 10     if (k-j.eq.3) then
          pcode = ctypei(j+1:k)
          do i = 1, NPCODE
            if (pcode.eq.pcodes(i)) then
c             It's recognized.
              return
            end if
          end do
        end if

c       Unrecognized pcode.
        itype = LINEAR
        umsg  = 'Assuming the ' // ctypei(1:k) // ' axis is linear.'
        call bug('w',umsg)
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
      call uvrdvrd(lus(icrd),'epoch',eqnox(icrd),0d0)
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
      subroutine coCelest(cel, x10,y10,x2,y2,cosrot,sinrot,
     *  crpix1,cdelt1,crpix2,cdelt2,
     *  x1pix,y1pix,x2pix,y2pix,x1off,y1off,x2off,y2off,valid)

      include 'wcslib/cel.inc'

      integer   cel(CELLEN)
      double precision x10,y10,x2,y2,cosrot,sinrot
      double precision crpix1,crpix2,cdelt1,cdelt2
      logical x1pix,y1pix,x2pix,y2pix
      logical x1off,y1off,x2off,y2off,valid

c  Convert celestial coordinates between grid and world coordinates.
c
c  Input:
c    x10,y10    Input coordinate, some combination of pixel or celestial
c               coordinate elements.
c    x1pix,y1pix,x2pix,y2pix
c               True if the particular coordinate is a pixel coordinate.
c    x1off,y1off,x2off,y2off
c               True if the particular coordinate element is an offset.
c
c    cdelt1,crpix1: cdelt and crpix values.
c    cdelt2,crpix2
c  Output:
c    x2,y2
c    valid      False if an illegal coordinate conversion was attempted.
c
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   status
      double precision clat0, crval1, crval2, ref(4), x1, y1

      integer   iref(2)
      equivalence (iref, ref)
c-----------------------------------------------------------------------
      status = celget(cel, cel_ref, iref)
      crval1 = ref(1)*DD2R
      crval2 = ref(2)*DD2R

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
        call coxy2ll(cel, x1,y1,x2,y2,
     *    crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)
c
c  Handle a mixed conversion.
c
      else if (x1pix) then
        call coMixed(cel, x1,y1,x2,y2,
     *    crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, .true.,valid)

      else if (y1pix) then
        call coMixed(cel, x1,y1,x2,y2,
     *    crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, .false.,valid)
c
c  Convert from RA,DEC to x,y.
c
      else
        call coll2xy(cel, x1,y1,x2,y2,
     *    crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)
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
      subroutine coll2xy(cel, lng,lat,p1,p2,
     *  crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot, valid)

      include 'wcslib/cel.inc'

      integer   cel(CELLEN)
      double precision lng,lat,p1,p2
      double precision crpix1,cdelt1,crpix2,cdelt2,cosrot,sinrot
      logical   valid

c  (lng,lat) are the celestial coordinates.
c  (p1,p2)   are pixel coordinates.
c
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'wcslib/prj.inc'

      integer   prj(PRJLEN), stat, status
      double precision dlng, lat0, lng0, phi, ref(4), t, theta, x, y
      character pcode*3

      integer   iref(2)
      equivalence (iref, ref)
c-----------------------------------------------------------------------
c     Check for simple linear coordinates.
      status = celget(cel, cel_prj, prj)
      status = prjget(prj, prj_code, pcode)
      if (pcode.eq.' ') then
        status = celget(cel, cel_ref, iref)
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

      include 'wcslib/cel.inc'

      integer   cel(CELLEN)
      double precision p1,p2, lng,lat
      double precision crpix1,cdelt1, crpix2,cdelt2, cosrot,sinrot
      logical   valid

c  (p1,p2)   are pixel coordinates.
c  (lng,lat) are the celestial coordinates.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'wcslib/prj.inc'

      integer   prj(PRJLEN), stat, status
      double precision lat0, lng0, phi, ref(4), t, theta, x, y
      character pcode*3

      integer   iref(2)
      equivalence (iref, ref)
c-----------------------------------------------------------------------
      x = (p1 - crpix1) * cdelt1
      y = (p2 - crpix2) * cdelt2

      t = x*cosrot - y*sinrot
      y = x*sinrot + y*cosrot
      x = t

c     Check for simple linear coordinates.
      status = celget(cel, cel_prj, prj)
      status = prjget(prj, prj_code, pcode)
      if (pcode.eq.' ') then
        status = celget(cel, cel_ref, iref)
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

      include 'wcslib/cel.inc'

      integer   cel(CELLEN)
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
      dowhile (delta.gt.TOL .and. niter.lt.MAXITER)
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

c       if (delta.gt.tol) call bug('f','Failed to converge, in coMixed')
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
