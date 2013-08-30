      program regrid

c= regrid - regrid an image dataset
c& rjs
c: map analysis
c+
c       REGRID any combination of the first three axes of an image using
c       cubic interpolation, with exclusion of blanked input pixels.
c       The output coordinate system may be specified via a template
c       image or axis descriptors.
c
c       REGRID handles conversion
c         - between different map projections,
c         - between different map centres (reference points),
c         - between B1950 and J2000 equatorial coordinates,
c         - between equatorial and galactic coordinates,
c         - between radio and optical velocity definitions,
c         - between LSR and barycentric velocity frames.
c
c       Nearest neighbour interpolation is used for axes smaller than
c       five pixels in extent.
c
c       REGRID supports the FITS Celestial World Coordinate System
c       (WCS) standard as defined in "WCS Paper II",
c       Calabretta & Greisen (2002), A&A 395, 1077.
c@ in
c       The input image name.  In the first instance, coordinate
c       descriptors for the output image are copied from the input.  
c       They may then be overridden by other parameters as described
c       below.  No default.
c@ out
c       The output image name.  No default.
c@ axes
c       Specify the axes to which 'tin' and 'desc' refer.  For example,
c       axes=1,2 resets descriptors for axes 1 and 2.  Likewise,
c       axes=2,3 resets descriptors for axes 2 and 3.  In each case, it
c       is still possible that another axis will be regridded due to the
c       effect of other parameters such as 'rotate', etc.  Note that the
c       output always contains the same number of axes as the input.
c       The default is all axes.
c@ tin
c       Template image.  If present, coordinate descriptors for the axes
c       to be regridded, as selected by keyword 'axes', are taken from
c       the template image rather than the input image.  These axes must
c       exist in the template image.
c@ desc
c       This optionally specifies the reference value (CRVAL), reference
c       pixel (CRPIX), coordinate increment (CDELT), and number of
c       pixels (respectively) for each and every axis of the output
c       image selected by keyword 'axes'.  Thus, if there are any, then
c       there must be 4 x naxes values, separated by commas, where naxes
c       is the number of axes specified by 'axes'.  These values are not
c       changed by any options other than 'offset'.
c
c       Note that for celestial axes (RA/DEC, GLON/GLAT, etc.), the
c       reference values (CRVAL) and increments (CDELT) are in degrees.
c
c       The axis types themselves (CTYPE), including the equatorial
c       coordinate system (B1950 or J2000), are taken from the template
c       image, if given, else from the input image, subject to
c       modification by 'project' and 'options' (see below).
c@ rotate
c       Set the rotation between the sky and the image to be this angle,
c       in degrees.  A positive value of the angle gives an eastward
c       rotation of the sky grid relative to the pixel grid.  If the
c       celestial axis descriptors came from the template image then the
c       default rotation comes from that, else the input image.
c@ lonpole
c       The native longitude (deg), and...
c@ latpole
c       ...the native latitude (deg) of the celestial pole, being the
c       same as the celestial latitude of the native pole.  Normally
c       set by the LONPOLEa and LATPOLEa keywords in FITS, or else by
c       PVi_3a and PVi_4a which have precedence, where i is the
c       longitude axis number.  Normally only lonpole is needed.
c       Together with the two angles specified by CRVAL, these provide
c       the third Euler angle for a spherical coordinate transformation
c       between celestial spherical coordinates and the projection's
c       "native" spherical coordinates as defined by WCS Paper II.  You
c       should rarely need to set these.
c
c       Defaults are taken from the template image if the projection
c       (see below) defaulted from CTYPE in the template image.  Else
c       from the input image if the projection defaulted from that.
c       Else default values depend on the projection.
c@ phi0
c       The native longitude (deg), and...
c@ theta0
c       ...the native latitude (deg) of the fiducial point of the
c       projection (i.e. the point whose celestial coordinates are given
c       by CRVAL), as defined by WCS Paper II.  Fiddling with these is
c       "courageous" in the sense of Sir Humphrey Appleby
c       (http://en.wikiquote.org/wiki/Yes,_Minister).  Set by the PVi_1a
c       and PVi_2a keywords in FITS, where i is the longitude axis.
c
c       Defaults are taken from the template image if the projection
c       (see below) defaulted from CTYPE in the template image.  Else
c       from the input image if the projection defaulted from that.
c       Else default values depend on the projection.
c@ xyzero
c       A logical value.  If true, apply an offset so that the origin of
c       Cartesian coordinates in the plane of projection corresponds to
c       the fiducial point - i.e. (x,y) = (0,0) at (phi0,theta0).  This
c       is always the case if (phi0,theta0) assume their default values,
c       it only has effect if they are reset (see above).  In FITS this
c       logical is set by the PVi_0a keyword, where i is the longitude
c       axis.
c
c       The default is taken from the template image if the projection
c       (see below) defaulted from CTYPE in the template image.  Else
c       from the input image if the projection defaulted from that.
c       Else false.
c@ project
c       Three-letter code for the output map projection.  Projection
c       codes follow the FITS WCS standard where they are encoded in the
c       CTYPE keyword.
c
c         Zenithals:
c           AZP  Zenithal/azimuthal perspective (2,C?,D?,d)
c           SZP  Slant zenithal perspective (3,C?,D?,d)
c           TAN  Gnomonic (0,D)
c           STG  Stereographic (0,C,D)
c           SIN  Orthographic/synthesis (2,d)
c           NCP  North celestial pole (0,D,d) - an important special
c                case of the SIN projection, divergent at the equator
c           ARC  Zenithal/azimuthal equidistant (0,G)
c           ZPN  Zenithal/azimuthal polynomial (30*,G|d)
c           ZEA  Zenithal/azimuthal equal area (0,E,G)
c           AIR  Airy (1,D)
c         Cylindricals:
c           CYP  Cylindrical perspective (2,G|D)
c           CEA  Cylindrical equal area (1,E,G)
c           CAR  Plate carrée (aka Cartesian) (0,G) - please note that
c                this is NOT the same as the simple linear system used
c                previously unless the reference coordinates (CRVAL)
c                are (0,0)
c           MER  Mercator (0,C,D) - note that the variant defined by
c                AIPS memo 46 is not supported.
c         Pseudo-cylindricals:
c           SFL  Sanson-Flamsteed (0,E,G)
c           GLS  Global sinusoid (0,E,G) - old implementation of Sanson-
c                Flamsteed.  Do not use unless to match an existing map.
c           PAR  Parabolic (0,E,G)
c           MOL  Mollweide (0,E,G)
c         Conventional:
c           AIT  Hammer-Aitoff (0,E,G) - note that the variant defined
c                by AIPS memo 46 is not supported.
c         Conics:
c           COP  Conic perspective  (2*,D)
c           COE  Conic equal area   (2*,E,G)
c           COD  Conic equidistant  (2*,G)
c           COO  Conic orthomorphic (2*,C,D)
c         Polyconics:
c           BON  Bonne (1*,E,G)
c           PCO  Polyconic (0,G)
c         Quad-cubes:
c           TSC  Tangential spherical cube (0,G)
c           CSC  COBE spherical cube (0,G)
c           QSC  Quadrilateralized spherical cube (0,E,G)
c         Hybrid:
c           HPX  HEALPix (2,E,G)
c
c       The number of projection parameters is indicated in parentheses.
c       These may be set in the pv array (below).  An asterisk indicates
c       that the projection has at least one non-defaulting parameter.
c       Refer to WCS Paper II for the mathematical definition of the
c       projection parameters and their default values.
c
c       The letters in parentheses provide a terse summary of the
c       projection's special properties:
c         - C  conformal
c         - C? conformal for particular projection parameters
c         - D  divergent
c         - D? divergent for particular projection parameters
c         - d  degenerate (parts of the sphere overlap in the map plane)
c         - E  equi-areal
c         - G  global (capable of mapping the whole sphere)
c       Some projections may be divergent, degenerate, or global
c       depending on the projection parameters.
c
c       The default is taken from CTYPE in the template image if given,
c       else from the input image.
c
c       NOTE that if any value is specified for project, even if the
c       same as in the template or input image, then defaults for
c       lonpole, latpole, phi0, theta0, and pv will not be taken from
c       the template or input image.
c@ pv
c       Array of up to 30 projection parameters as per the above.  Set
c       by the PVi_ma keywords in FITS, where i is the latitude axis
c       number.  Note that at least one non-zero parameter must be given
c       for ZPN, COP, COE, COD, COO, and BON.
c
c       If no parameters are given, defaults are taken from the template
c       image if the projection (see above) defaulted from CTYPE in the
c       template image.  Else from the input image if the projection
c       defaulted from that.  Else default values, where applicable,
c       depend on the projection as per WCS Paper II.
c@ options
c       Extra processing options that alter the axis description defined
c       by the template image, axis descriptors, or input image.
c       Several can be given, separated by commas, with minimum-match.
c         altprj    Interpret a CAR (plate carée) projection in the
c                   input ot template image as a simple linear
c                   coordinate system with an additional 1/cos(lat0)
c                   scaling factor applied when computing the longitude,
c                   e.g.
c                      RA = (p1 - CRPIX1)*CDELT1/cos(CRVAL2).
c                   This interpretation differs significantly from the
c                   FITS standard when lat0 (i.e. CRVAL2) is non-zero.
c         noscale   Produce a cube where the RA/DEC cell size does not
c                   scale with frequency/velocity.
c         offset    The coordinate system described by the template or
c                   descriptors is modified (shift and expansion or
c                   contraction) by an integral number of pixels so that
c                   it completely encloses the input.
c         equisw    Switch the output coordinate system between J2000
c                   and B1950 equatorial.  The output map will be
c                   reoriented so that north is upwards, though the
c                   coordinate graticule may be oblique nonetheless.
c         galeqsw   Switch the output coordinate system between galactic
c                   and equatorial.  Galactic switches implicitly to
c                   equatorial J2000.  The output map will be reoriented
c                   so that the north pole (equatorial or galactic) is
c                   upwards, though the coordinate graticule may be
c                   oblique nonetheless.
c         nearest   Use nearest neighbour interpolation rather than the
c                   default cubic interpolation.
c       If the equatorial coordinate system is not specified in the
c       header (via the 'epoch' item), then J2000 is assumed.
c@ tol
c       Interpolation tolerance.  Tolerate an error of the specified
c       amount in converting pixel locations in the input to the output.
c       Must be less that 0.5.  The default is 0.05.
c
c$Id: regrid.for,v 1.17 2013/08/30 01:49:21 wie017 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'

      integer    NPCODE
      parameter (NPCODE = 29)

      double precision UNDEF
      parameter (UNDEF = 999d0)

      logical   altPrj, doDesc, doEqEq, doGalEq, doNear, doOff, noScale,
     *          doTCel, doZero
      integer   axMap(MAXNAX), BufSize, cOut, gnx, gny, GridSize, i,
     *          ilat, ilng, ispc, k, lBuf, lDef, lIn, lOut, lTem, m,
     *          maxc(3), maxv(3), minc(3), minv(3), n, naxes,
     *          nAxIn(MAXNAX), nAxOut(MAXNAX), nAxTem(MAXNAX), nblank,
     *          nBuf(3), ndesc, nIAxes, npv, nTAxes, nxy, off(3),
     *          offset, order(3), rBuf, valid, xv, xyzero, yv, zv
      real      tol
      double precision cdelt, crpix, crval, desc(4,MAXNAX), latpol,
     *          llrot, lonpol, phi0, pv(0:29), theta0
      character algo*3, ctype*16, in*64, keyw*8, line*64, out*64,
     *          pcode*3, pcodes(NPCODE)*3, tin*64, version*72

      external  hdprsnt, itoaf, keyprsnt, versan
      logical   hdprsnt, keyprsnt
      character itoaf*2, versan*72

c     Projection codes.
      data pcodes /
     *  'azp', 'szp', 'tan', 'stg', 'sin', 'ncp', 'arc', 'zpn',
     *  'zea', 'air', 'cyp', 'cea', 'car', 'mer', 'cop', 'coe',
     *  'cod', 'coo', 'sfl', 'gls', 'par', 'mol', 'ait', 'bon',
     *  'pco', 'tsc', 'csc', 'qsc', 'hpx'/
c-----------------------------------------------------------------------
      version = versan ('regrid',
     *                  '$Revision: 1.17 $',
     *                  '$Date: 2013/08/30 01:49:21 $')

c     Get the input parameters.
      call keyini
      call keya('in',in,' ')
      if (in.eq.' ') call bug('f','An input must be given')

      call keya('out',out,' ')
      if (out.eq.' ') call bug('f','An output must be given')

      call mkeyi('axes',axMap,MAXNAX,naxes)

      call keya('tin',tin,' ')

      call mkeyd('desc',desc,4*MAXNAX,ndesc)
      if (mod(ndesc,4).ne.0)
     *  call bug('f','Invalid number of axis descriptors')
      ndesc = ndesc/4

      call keyd('rotate', llrot, UNDEF)

      call keyd('lonpole', lonpol, UNDEF)
      call keyd('latpole', latpol, UNDEF)
      call keyd('phi0',    phi0,   UNDEF)
      call keyd('theta0',  theta0, UNDEF)
      xyzero = -1
      if (keyprsnt('xyzero')) then
        call keyl('xyzero', doZero, .false.)
        if (doZero) then
          xyzero = 1
        else
          xyzero = 0
        endif
      endif

      call keymatch('project',NPCODE,pcodes,1,pcode,n)
      if (n.eq.0) pcode = ' '
      call mkeyd('pv',pv,30,npv)

      call getopt(altPrj,noScale,doOff,doEqEq,doGalEq,doNear)

      call keyr('tol',tol,0.05)
      if (tol.lt.0.0 .or. tol.ge.0.5)
     *  call bug('f','Invalid value for the tol parameter')

      call keyfin

c     Open the input dataset.
      call xyopen(lIn,in,'old',MAXNAX,nAxIn)
      call rdhdi(lIn,'naxis',nIAxes,0)
      nIAxes = min(nIAxes,MAXNAX)
      do i = 1, nIAxes
        nAxOut(i) = nAxIn(i)
      enddo
      do i = nIAxes+1, MAXNAX
        nAxOut(i) = 1
      enddo

c     Check the user's 'axes' specification.
      if (naxes.gt.0) then
        do i = 1, naxes
          if (axMap(i).lt.1 .or. axMap(i).gt.nIAxes)
     *      call bug('f','Invalid "axes" value')
        enddo
      else
        naxes = nIAxes
        do i = 1, naxes
          axMap(i) = i
        enddo
      endif

c     Descriptors, if given, must match the number in 'axes'.
      doDesc = ndesc.ne.0
      if (doDesc .and. ndesc.ne.naxes) call bug('f',
     *  'Inconsistent number of axis descriptors given.')


c   Set up the output coordinate system.
      call coInit(lIn)
      if (altPrj) call coAltPrj(lIn)
      call coCreate(naxes, cOut)

      call coFindAx(lIn, 'longitude', ilng)
      call coFindAx(lIn, 'latitude',  ilat)
      call coFindAx(lIn, 'spectral',  ispc)

c     Axis defaults come initially from the input image.
      lDef = lIn
      doTCel = .false.
      do i = 1, nIAxes
c       Is this a celestial axis?
        if (i.le.naxes) then
          if (axMap(i).eq.ilng .or. axMap(i).eq.ilat) then
c         Flag that celestial axes are being regridded.
            doTCel = .true.

            if (doDesc) then
c             Convert degrees to radians.
              desc(1,i) = desc(1,i) * DD2R
              desc(3,i) = desc(3,i) * DD2R
            endif
          endif
        endif

c       Set descriptors for the axes that will remain unchanged.
        call coAxGet(lIn,  i, ctype, crpix, crval, cdelt)
        call coAxSet(cOut, i, ctype, crpix, crval, cdelt)
      enddo

c     Reset descriptors for the axes specified by 'axes' if need be.
      if (tin.ne.' ') then
c       Reset descriptors from the template image.
        if (ndesc.ne.0) then
          call bug('w','Using template, ignoring descriptors.')
        endif

        call xyopen(lTem, tin, 'old', MAXNAX, nAxTem)
        call rdhdi(lTem, 'naxis', nTAxes, 0)
        nTAxes = min(nTAxes, MAXNAX)

        call coInit(lTem)
        if (altPrj) call coAltPrj(lTem)

c       Reset descriptors from the template.
        do i = 1, naxes
          if (axMap(i).gt.nTAxes) call bug('f',
     *      'Requested axis does not exist in the template')

          nAxOut(axMap(i)) = nAxTem(axMap(i))
          call coAxGet(lTem, axMap(i), ctype, crpix, crval, cdelt)
          call coAxSet(cOut, axMap(i), ctype, crpix, crval, cdelt)

          if (axMap(i).eq.ispc) then
c           Reset the spectral axis of the input coordinate object to
c           match that of the output (may change vobs).  Preempts any
c           change that pCvtInit might make by calling coSpcSet.
            call coSpcSet(lIn, ctype, ' ', ispc, algo)
          endif
        enddo

c       Celestial parameters should default from the template iff
c       celestial axes are being regridded.
        if (doTCel) lDef = lTem

      else if (doDesc) then
c       Reset descriptors from 'desc'.
        do i = 1, naxes
          nAxOut(axMap(i)) = nint(desc(4,i))
          if (nAxOut(i).lt.1 .or. nAxOut(i).gt.MAXDIM) then
            call bug('f','Invalid axis size in axis descriptor')
          endif

          call coAxGet(lIn,  axMap(i), ctype, crpix, crval, cdelt)
          call coAxSet(cOut, axMap(i), ctype, desc(2,i), desc(1,i),
     *                 desc(3,i))
        enddo
      endif

c     Map rotation.
      if (llrot.eq.UNDEF) then
        call coGetD(lDef, 'llrot', llrot)
      else
c       Given by user.
        llrot = llrot * DD2R
      endif
      call coSetD(cOut, 'llrot', llrot)

c     Set celestial parameters.
      if (pcode.eq.' ') then
c       pcode not set; copy parameters relating to the fiducial point
c       only if present in the template header.
        if (lonpol.eq.UNDEF .and. hdprsnt(lDef, 'lonpole')) then
          call coGetD(lDef, 'lonpole', lonpol)
        endif
        if (latpol.eq.UNDEF .and. hdprsnt(lDef, 'latpole')) then
          call coGetD(lDef, 'latpole', latpol)
        endif
        if (phi0  .eq.UNDEF .and. hdprsnt(lDef, 'phi0')) then
          call coGetD(lDef, 'phi0',    phi0)
        endif
        if (theta0.eq.UNDEF .and. hdprsnt(lDef, 'theta0')) then
          call coGetD(lDef, 'theta0',  theta0)
        endif
        if (xyzero.eq.-1    .and. hdprsnt(lDef, 'xyzero')) then
          call coGetI(lDef, 'xyzero',  xyzero)
        endif
      endif

c     WCSLIB will provide the defaults if these are still undefined.
      if (lonpol.ne.UNDEF) call coSetD(cOut, 'lonpole', lonpol)
      if (latpol.ne.UNDEF) call coSetD(cOut, 'latpole', latpol)
      if (phi0  .ne.UNDEF) call coSetD(cOut, 'phi0',    phi0)
      if (theta0.ne.UNDEF) call coSetD(cOut, 'theta0',  theta0)
      if (xyzero.ne.-1)    call coSetI(cOut, 'xyzero',  xyzero)

      call coCpyD(lDef, cOut, 'epoch')

c     Projection parameters.
      if (pcode.eq.' ') then
        if (npv.eq.0) then
c         Copy them from the template or input image.
          do m = 0, 29
            keyw = 'pv'//itoaf(m)
            if (hdprsnt(lDef, keyw)) then
              call coCpyD(lDef, cOut, keyw)
            endif
          enddo
        else
c         'pv' was set but 'project' wasn't.  Presumably the user only
c         wants to tweak the projection parameters.
          call coPrjSet(cOut, '-', npv, pv)
        endif
      else
c       Have the required projection parameters been provided?
        if (index(pcode,'zpn,cop,coe,cod,coo,bon').gt.0 .and. npv.lt.1)
     *    call bug('f',
     *      'A projection parameter must be provided for '//pcode)

        call coPrjSet(cOut, pcode, npv, pv)
      endif

c     Options.
      if (noScale) then
        call coSetA(cOut, 'cellscal', 'CONSTANT')
      else
        call coCpyA(lDef, cOut, 'cellscal')
      endif

c     Finished with the template image.
      if (tin.ne.' ') call xyclose(lTem)

c     The remaining coordinate parameters come from the input image.
      call coCpyD(lIn, cOut, 'obstime')
      call coCpyD(lIn, cOut, 'restfreq')
      call coCpyA(lIn, cOut, 'specsys')
      call coCpyD(lIn, cOut, 'vobs')

c     Set up output celestial coordinates.
      call setCel(lIn, cOut, doDesc, doEqEq, doGalEq)

c     Set up offset coordinates.
      if (doOff) call doOffset(lIn,nAxIn,cOut,nAxOut)

c     Initialise coordinate conversions.
      call pCvtInit(cOut,lIn)


c     Check the output image dimensions.
      if (nAxOut(1).gt.MAXDIM) call bug('f','Output too big for me')
      do k = 4, nIAxes
        if (nAxOut(k).gt.1) call bug('f','Cannot handle hypercubes')
      enddo

c     Create the output.
      call xyopen(lOut,out,'new',nIAxes,nAxOut)
      call mkHead(lIn,lOut,cOut,version)

c     Initialise.
      GridSize = 0
      call BufIni(nBuf,off,minc,maxc,BufSize)

      nblank = 0
      do k = 1, nAxOut(3)
        call xysetpl(lOut,1,k)

c       Determine the size of the coordinate translation grid.
        if (tol.eq.0) then
          gnx = nAxOut(1)
          gny = nAxOut(2)
        else
          call GridEst(nAxOut(1),nAxOut(2),k,gnx,gny,tol)
        endif

c       Allocate space used for the coordinate translation grid.
        if (gnx*gny.gt.GridSize) then
          if (GridSize.gt.0) then
            call memFree(xv,GridSize,'r')
            call memFree(yv,GridSize,'r')
            call memFree(zv,GridSize,'r')
            call memFree(valid,GridSize,'l')
          endif
          GridSize = gnx*gny
          call memAlloc(xv,GridSize,'r')
          call memAlloc(yv,GridSize,'r')
          call memAlloc(zv,GridSize,'r')
          call memAlloc(valid,GridSize,'l')
        endif

c       Calculate the coordinates translation grid, and work out some
c       statistics about it.
        call GridGen(nAxOut(1),nAxOut(2),nAxOut(3),k,
     *        memr(xv),memr(yv),memr(zv),meml(Valid),gnx,gny)
        call GridStat(doNear,memr(xv),memr(yv),memr(zv),meml(valid),
     *        gnx,gny,nAxIn(1),nAxIn(2),nAxIn(3),tol,minv,maxv,order)

        if (minv(1).gt.maxv(1) .or.
     *      minv(2).gt.maxv(2) .or.
     *      minv(3).gt.maxv(3)) then
          nblank = nblank + nAxOut(1)*nAxOut(2)
          call BadPlane(lOut,nAxOut(1),nAxOut(2))

        else
c         Get the required data.
          call BufGet(lIn,minv,maxv,nAxIn,nBuf,off,minc,maxc,
     *                                        rBuf,lBuf,BufSize)
          offset = minc(3) - off(3) - 1
          nxy = nBuf(1)*nBuf(2)

c         Finally to the interpolation.
          call Intp(lOut,order,nAxOut(1),nAxOut(2),
     *      memr(rBuf+offset*nxy),meml(lBuf+offset*nxy),
     *      nBuf(1),nBuf(2),maxc(3)-minc(3)+1,
     *      off(1),off(2),minc(3)-1,
     *      memr(xv),memr(yv),memr(zv),meml(valid),gnx,gny,nblank)
        endif
      enddo

c     Warn about the number of blanked pixels.
      nblank = (100*nblank)/(nAxOut(1)*nAxOut(2)*nAxOut(3))
      write(line,'(a,i3,a)')
     *  'Overall fraction of blanked pixels: ',nblank,'%'
      if (nblank.ge.50) then
        call bug('w',line)
      else if (nblank.ne.0) then
        call output(line)
      endif

c     All done. Tidy up.
      if (BufSize.gt.0) then
        call memFree(rBuf,BufSize,'r')
        call memFree(lBuf,BufSize,'l')
      endif
      if (GridSize.gt.0) then
        call memFree(xv,GridSize,'r')
        call memFree(yv,GridSize,'r')
        call memFree(zv,GridSize,'r')
        call memFree(valid,GridSize,'l')
      endif

      call xyclose(lOut)
      call xyclose(lIn)

      end

c***********************************************************************

      subroutine getopt(altPrj,noScale,offset,doEqEq,doGalEq,doNear)

      logical altPrj,noScale,offset,doEqEq,doGalEq,doNear
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=6)
      character opts(NOPTS)*8
      logical present(NOPTS)
      data opts/'altprj', 'noscale ','offset  ','equisw  ','galeqsw ',
     *          'nearest '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      altPrj  = present(1)
      noScale = present(2)
      offset  = present(3)
      doEqEq  = present(4)
      doGalEq = present(5)
      doNear  = present(6)
      end

c***********************************************************************

      subroutine setCel(lIn, cOut, doDesc, doEqEq, doGalEq)

      integer lIn, cOut
      logical doDesc, doEqEq, doGalEq
c-----------------------------------------------------------------------
c  Set up output celestial coordinates.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision UNDEF
      parameter (UNDEF = 999d0)

      logical   doeqnx, gotone
      integer   ilat, ilng
      double precision cdelt1, cdelt2, crpix1, crpix2, crval1, crval2,
     *          ddec, dec, dra, eqnox, obstime, ra
      character ctype1*16, ctype2*16, type1*4, type2*4

      external  epo2jul
      double precision epo2jul
c-----------------------------------------------------------------------
      call coReInit(cOut)
      if (.not.(doEqEq .or. doGalEq)) return

c     Look for a celestial axis pair.
      call coFindAx(cOut,'longitude',ilng)
      call coFindAx(cOut,'latitude',ilat)
      if (ilng.eq.0 .or. ilat.eq.0)
     *  call bug('f','Cannot find RA/DEC or GLON/GLAT axes')

      call coAxGet(cOut,ilng,ctype1,crpix1,crval1,cdelt1)
      call coAxGet(cOut,ilat,ctype2,crpix2,crval2,cdelt2)

c     Extract the basic coordinate types, RA/DEC or GLON/GLAT.
      type1 = ' '
      type2 = ' '
      if ((ctype1(5:5).eq.'-' .or. ctype1(5:).eq.' ') .and.
     *    (ctype2(5:5).eq.'-' .or. ctype2(5:).eq.' ')) then
        type1 = ctype1(1:4)
        if (type1(4:4).eq.'-') then
          type1(4:4) = ' '
          if (type1(3:3).eq.'-') type1(3:3) = ' '
        endif

        type2 = ctype2(1:4)
        if (type2(4:4).eq.'-') then
          type2(4:4) = ' '
          if (type2(3:3).eq.'-') type2(3:3) = ' '
        endif
      endif

c     Get the equinox and the epoch of observation.
      call coGetD(cOut,'epoch',eqnox)
      call coGetD(lIn,'obstime',obstime)

      doeqnx = doEqEq
      gotone = .false.

c     Switch between equatorial and galactic coordinates?
      if (doGalEq) then
c       The Galactic and B1950/FK4 equatorial coordinate systems are
c       tied to the rotating Galaxy, whereas J2000/FK5 is tied to the
c       distant universe.  Thus, Galactic coordinates rotate slowly with
c       respect to J2000/FK5 (~250My period -> 0.5 arcsec/century).  For
c       accuracy, the conversion is always via B1950/FK4.

        if (type1.eq.'RA' .and. type2.eq.'DEC') then
c         Convert equatorial to galactic.
          if (eqnox.eq.0d0) then
            eqnox = 2000d0
            call bug('w','Assuming J2000 equatorial coordinates.')
          endif

          if (obstime.eq.0d0) then
            obstime = epo2jul(2000d0,'J')
            call bug('w','Assuming observation at epoch J2000.0.')
          endif

          if (abs(eqnox-2000d0).lt.0.1d0) then
c           Convert J2000 to B1950.
            if (.not.doDesc) then
              call fk54z(crval1,crval2,obstime,ra,dec,dra,ddec)
              crval1 = ra
              crval2 = dec
            endif
            call output('Converting J2000 equatorial to Galactic.')

          else if (abs(eqnox-1950d0).lt.0.1d0) then
            call output('Converting B1950 equatorial to Galactic.')

          else
            call bug('f', 'Unable to convert to B1950 coordinates')
          endif

c         Convert B1950 equatorial to galactic.
          if (.not.doDesc) then
            call dsfetra(crval1,crval2,.false.,1)
          endif

          ctype1(1:4) = 'GLON'
          ctype2(1:4) = 'GLAT'

c         Equinox doesn't make sense for galactic coordinates.
          type1 = 'GLON'
          type2 = 'GLAT'
          eqnox = 0d0

          doeqnx = .false.
          gotone = .true.

        else if (type1.eq.'GLON' .and. type2.eq.'GLAT') then
c         Convert galactic to B1950 equatorial.
          if (.not.doDesc) then
            call dsfetra(crval1,crval2,.true.,1)
          endif

          if (ctype1(5:).eq.' ' .and. ctype2(5:).eq.' ') then
            ctype1 = 'RA'
            ctype2 = 'DEC'
          else
            ctype1(1:4) = 'RA--'
            ctype2(1:4) = 'DEC-'
          endif

          type1 = 'RA'
          type2 = 'DEC'
          eqnox = 1950d0

c         We want J2000 by default.
          doeqnx = .not.doeqnx
          gotone = .true.

          if (doeqnx) then
            call output('Converting Galactic to J2000 equatorial.')
          else
            call output('Converting Galactic to B1950 equatorial.')
          endif
        endif
      endif

c     Switch equatorial coordinates?
      if (doeqnx .and. type1.eq.'RA' .and. type2.eq.'DEC') then
        if (eqnox.eq.0d0) then
          eqnox = 2000d0
          call bug('w','Assuming J2000 equatorial coordinates.')
        endif

        if (obstime.eq.0d0) then
          obstime = epo2jul(2000d0,'J')
          call bug('w','Assuming observation at epoch J2000.0.')
        endif

        if (abs(eqnox-1950d0).lt.0.1d0) then
          if (.not.doDesc) then
            call fk45z(crval1,crval2,obstime,ra,dec)
            crval1 = ra
            crval2 = dec
          endif

          eqnox = 2000d0

          if (.not.doGalEq) then
            call output('Converting B1950 to J2000 equatorial.')
          endif

        else if (abs(eqnox-2000d0).lt.0.1d0) then
          if (.not.doDesc) then
            call fk54z(crval1,crval2,obstime,ra,dec,dra,ddec)
            crval1 = ra
            crval2 = dec
          endif

          eqnox = 1950d0

          if (.not.doGalEq) then
            call output('Converting J2000 to B1950 equatorial.')
          endif

        else
          call bug('f',
     *      'Cannot convert other than B1950/J2000 equatorial')
        endif

        gotone = .true.
      endif

      if (gotone) then
        call coSetD(lIn,'obstime',obstime)

        call coAxSet(cOut,ilng,ctype1,crpix1,crval1,cdelt1)
        call coAxSet(cOut,ilat,ctype2,crpix2,crval2,cdelt2)

c       Put coordinates in the normal orientation, i.e. with north up.
        call coSetD(cOut,'lonpole',UNDEF)
        call coSetD(cOut,'latpole',UNDEF)

        call coSetD(cOut,'epoch',eqnox)
        call coSetD(cOut,'obstime',obstime)

        call coReinit(cOut)
      endif

      end

c***********************************************************************

      subroutine doOffset(lIn,nAxIn,lOut,nAxOut)

      integer lIn,lOut,nAxIn(3),nAxOut(3)
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer NV
      double precision TOL
      parameter (NV = 10, TOL = 0.49d0)

      logical first, valid(NV,NV,NV), warned, weird(3)
      integer i, j, k, l, maxv(3), minv(3), nv1, nv2, nv3
      double precision crpix, in(3), out(3,NV,NV,NV)

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
      call pCvtInit(lIn,lOut)

      nv1 = min(max(3,nAxIn(1)),NV)
      nv2 = min(max(3,nAxIn(2)),NV)
      nv3 = min(max(3,nAxIn(3)),NV)

      first = .true.
      do k = 1, nv3
        do j = 1, nv2
          do i = 1, nv1
            in(1) = 1 + (i-1)*(nAxIn(1)-1)/real(nv1-1)
            in(2) = 1 + (j-1)*(nAxIn(2)-1)/real(nv2-1)
            in(3) = 1 + (k-1)*(nAxIn(3)-1)/real(nv3-1)
            call pCvt(in,out(1,i,j,k),3,valid(i,j,k))

            if (first .and. valid(i,j,k)) then
              first = .false.
              do l = 1, 3
                weird(l) = .false.
                minv(l) = nint(out(l,i,j,k)-TOL)
                maxv(l) = nint(out(l,i,j,k)+TOL)
              enddo
            endif
          enddo
        enddo
      enddo

      if (first) call bug('f','No valid pixels found')

c     Determine the min and max pixels that we are interested in.
      do k = 1, nv3
        do j = 1, nv2
          do i = 1, nv1
            do l = 1, 3
              if (valid(i,j,k)) then
                minv(l) = min(minv(l),nint(out(l,i,j,k)-TOL))
                maxv(l) = max(maxv(l),nint(out(l,i,j,k)+TOL))
              endif
            enddo
          enddo
        enddo
      enddo

c     Look for twists or wrap arounds.
      do k = 1, nv3
        do j = 1, nv2
          do i = 1, nv1-2
            if (valid(i  ,j,k) .and.
     *          valid(i+1,j,k) .and.
     *          valid(i+2,j,k)) then
              weird(1) = weird(1) .or.
     *          (out(1,i+2,j,k) - out(1,i+1,j,k)) *
     *          (out(1,i+1,j,k) - out(1,i,j,k)).lt.0d0
            endif
          enddo
        enddo
      enddo

      do k = 1, nv3
        do j = 1, nv2-2
          do i = 1, nv1
            if (valid(i,j,  k) .and.
     *          valid(i,j+1,k) .and.
     *          valid(i,j+2,k)) then
              weird(2) = weird(2) .or.
     *          (out(2,i,j+2,k) - out(2,i,j+1,k)) *
     *          (out(2,i,j+1,k) - out(2,i,j,k)).lt.0d0
            endif
          enddo
        enddo
      enddo

      do k = 1, nv3-2
        do j = 1, nv2
          do i = 1, nv1
            if (valid(i,j,k)   .and.
     *          valid(i,j,k+1) .and.
     *          valid(i,j,k+2)) then
              weird(3) = weird(3) .or.
     *          (out(3,i,j,k+2) - out(3,i,j,k+1)) *
     *          (out(3,i,j,k+1) - out(3,i,j,k)).lt.0d0
            endif
          enddo
        enddo
      enddo

c     Set the template ranges.  If it's weird, just go with the template
c     range and the max and min of mapped pixels.
      warned = .false.
      do i = 1, 3
        if (weird(i)) then
          minv(i) = min(1,minv(i))
          maxv(i) = max(nAxOut(i),maxv(i))
        endif

        nAxOut(i) = maxv(i) - minv(i) + 1
        if (nAxOut(i).gt.MAXDIM) then
          nAxOut(i) = MAXDIM
          if (.not.warned) call bug('w',
     *      'Output image too large -- being truncated')
          warned = .true.
        endif

        call coGetD(lOut,'crpix'//itoaf(i),crpix)
        crpix = crpix - minv(i) + 1
        call coSetD(lOut,'crpix'//itoaf(i),crpix)
      enddo

      call coReinit(lOut)

      end

c***********************************************************************

      subroutine mkHead(lIn,lOut,cOut,version)

      integer   lIn,  lOut,  cOut
      character version*(*)
c-----------------------------------------------------------------------
      integer   i
      character line*64

c     Follows the list of keywords in HEADCP (headcopy.for) with the
c     omission of coordinate keywords, cellscal, epoch, obstime,
c     restfreq, specsys, and vobs, which are handled by coWrite, and the
c     addition of rms (but not datamin or datamax).
      integer   NKEYS
      parameter (NKEYS=22)
      character keyw(NKEYS)*8
      data keyw /
     *    'bmaj    ', 'bmin    ', 'bpa     ', 'btype   ', 'bunit   ',
     *    'date-obs', 'instrume', 'ltype   ', 'lstart  ', 'lstep   ',
     *    'lwidth  ', 'mostable', 'niters  ', 'object  ', 'observer',
     *    'obsra   ', 'obsdec  ', 'pbfwhm  ', 'pbtype  ', 'telescop',
     *    'history ', 'rms     '/
c-----------------------------------------------------------------------
      call coWrite(cOut,lOut)

      do i = 1, NKEYS
        call hdcopy(lIn,lOut,keyw(i))
      enddo

      call hisOpen(lOut,'append')
      line = 'REGRID: Miriad '//version
      call hisWrite(lOut,line)
      call hisInput(lOut,'REGRID')
      call hisClose(lOut)

      end

c***********************************************************************

      subroutine GridEst(nx,ny,plane,gnx,gny,tol)

      integer nx,ny,plane,gnx,gny
      real tol
c-----------------------------------------------------------------------
c  Compute the grid size needed to allow translation between the
c  different pixel coordinate systems.
c
c  Input:
c    nx,ny      Image size.
c    plane      Plane of interest.
c    tol        Pixel tolerance.
c  Output:
c    gnx,gny    Translation grid size.
c-----------------------------------------------------------------------
      double precision x(3,4),tx(3,4),mid(3),tmid(3)
      double precision xmid(3,4),txmid(3,4)
      integer n,i,j,k,kmax,k1,k2,k3
      real err,errmax
      logical more,valid
c-----------------------------------------------------------------------
      n = 1
      k = 0
      do j = 1, 2
        do i = 1, 2
          k = k + 1
          x(1,k) = (2-i) + (i-1)*nx
          x(2,k) = (2-j) + (j-1)*ny
          x(3,k) = plane
          call pCvt(x(1,k),tx(1,k),3,valid)
          if (.not.valid)
     *      call bug('f','Invalid coordinate: please use tol=0')
        enddo
      enddo
      mid(1) = 0.5*(nx+1)
      mid(2) = 0.5*(ny+1)
      mid(3) = plane
      call pCvt(mid,tmid,3,valid)
      if (.not.valid)
     *  call bug('f','Invalid coordinate: please use tol=0')

      more = .true.
      do while (more)
        n = 2*n
        errmax = -1
        do k = 1, 4
          xmid(1,k) = 0.5*(x(1,k) + mid(1))
          xmid(2,k) = 0.5*(x(2,k) + mid(2))
          xmid(3,k) = 0.5*(x(3,k) + mid(3))
          call pCvt(xmid(1,k),txmid(1,k),3,valid)
          if (.not.valid)
     *      call bug('f','Invalid coordinate: please use tol=0')
          err = max(abs(0.5*(tx(1,k)+tmid(1)) - txmid(1,k)),
     *              abs(0.5*(tx(2,k)+tmid(2)) - txmid(2,k)),
     *              abs(0.5*(tx(3,k)+tmid(3)) - txmid(3,k)))
          if (err.gt.errmax) then
            kmax = k
            errmax = err
          endif
        enddo
        more = errmax.gt.tol .and. max(nx,ny).gt.n+1

c       If the tolerance has not yet been reached, home in on the
c       region where the fit was worst.
        if (more) then
          k1 = 1
          if (k1.eq.kmax) k1 = k1 + 1
          k2 = k1 + 1
          if (k2.eq.kmax) k2 = k2 + 1
          k3 = k2 + 1
          if (k3.eq.kmax) k3 = k3 + 1

          call TripMv( mid(1),  mid(2), mid(3),      x(1,k1))
          call TripMv(tmid(1), tmid(2),tmid(3),     tx(1,k1))
          call TripMv(x(1,kmax),mid(2), dble(plane), x(1,k2))
          call TripMv(mid(1),x(2,kmax), dble(plane), x(1,k3))

          call pCvt(x(1,k2),tx(1,k2),3,valid)
          if (.not.valid)
     *      call bug('f','Invalid coordinate: please use tol=0')

          call pCvt(x(1,k3),tx(1,k3),3,valid)
          if (.not.valid)
     *      call bug('f','Invalid coordinate: please use tol=0')

          call TripMv(xmid(1,kmax), xmid(2,kmax), xmid(3,kmax),mid)
          call TripMv(txmid(1,kmax),txmid(2,kmax),txmid(3,kmax),tmid)
        endif
      enddo

      gnx = min(nx,n+1)
      gny = min(ny,n+1)

      end

c***********************************************************************

      subroutine TripMv(a,b,c,x)

      double precision a,b,c,x(3)
c-----------------------------------------------------------------------
      x(1) = a
      x(2) = b
      x(3) = c

      end

c***********************************************************************

      subroutine GridGen(nx,ny,nz,plane,xv,yv,zv,valid,gnx,gny)

      integer nx,ny,nz,plane,gnx,gny
      real xv(gnx,gny),yv(gnx,gny),zv(gnx,gny)
      logical valid(gnx,gny)
c-----------------------------------------------------------------------
c  Determine the translation between the output and input pixel
c  coordinates on a grid.
c
c  Input:
c    nx,ny,nz
c    plane
c    gnx,gny
c  Output:
c    xv,yv,zv,valid
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer i,j
      double precision in(3),out(3)
c-----------------------------------------------------------------------
      in(3) = plane

      do j = 1, gny
        in(2) = dble(ny-1)/dble(gny-1) * (j-1) + 1
        do i = 1, gnx
          in(1) = dble(nx-1)/dble(gnx-1) * (i-1) + 1
          call pCvt(in,out,3,valid(i,j))
          if (valid(i,j)) then
            xv(i,j) = out(1)
            yv(i,j) = out(2)
            if (nz.gt.1) then
              zv(i,j) = out(3)
            else
              zv(i,j) = in(3)
            endif            
          endif
        enddo
      enddo

      end

c***********************************************************************

      subroutine GridStat(doNear,xv,yv,zv,valid,gnx,gny,n1,n2,n3,
     *    tol,minv,maxv,order)

      logical doNear
      integer gnx,gny,n1,n2,n3,minv(3),maxv(3),order(3)
      real xv(gnx,gny),yv(gnx,gny),zv(gnx,gny),tol
      logical valid(gnx,gny)
c-----------------------------------------------------------------------
      integer i,j,n(3)
      real minr(3),maxr(3),diff
      logical first
c-----------------------------------------------------------------------
      first = .true.
      do j = 1, gny
        do i = 1, gnx
          if (valid(i,j)) then
            if (first) then
              first = .false.
              minr(1) = xv(i,j)
              maxr(1) = minr(1)
              minr(2) = yv(i,j)
              maxr(2) = minr(2)
              minr(3) = zv(i,j)
              maxr(3) = minr(3)
            else
              minr(1) = min(xv(i,j),minr(1))
              maxr(1) = max(xv(i,j),maxr(1))
              minr(2) = min(yv(i,j),minr(2))
              maxr(2) = max(yv(i,j),maxr(2))
              minr(3) = min(zv(i,j),minr(3))
              maxr(3) = max(zv(i,j),maxr(3))
            endif
          endif
        enddo
      enddo

      if (first) call bug('f','No valid pixels found here')

c     Determine which are going to be done by nearest neighbour, and
c     what are the min and max limits needed for interpolation.
      n(1) = n1
      n(2) = n2
      n(3) = n3
      do i = 1, 3
        minr(i) = max(minr(i),1.0)
        maxr(i) = min(maxr(i),real(n(i)))
        diff = max(maxr(i)-minr(i),abs(nint(minr(i))-minr(i)))
        if (n(i).le.5 .or. diff.lt.tol .or. doNear) then
          order(i) = 0
          minv(i) = max(1,   nint(minr(i)))
          maxv(i) = min(n(i),nint(maxr(i)))
        else
          order(i) = 3
          minv(i) = max(1,   nint(minr(i)-1.5))
          maxv(i) = min(n(i),nint(maxr(i)+1.5))
        endif
      enddo

      end

c***********************************************************************

      subroutine BufIni(nBuf,off,minc,maxc,BufSize)

      integer nBuf(3),off(3),minc(3),maxc(3),BufSize
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, 3
        nBuf(i) = 0
        off(i) = 0
        minc(i) = 0
        maxc(i) = 0
      enddo

      BufSize = 0

      end

c***********************************************************************

      subroutine BufGet(lIn,minr,maxr,n,nBuf,off,minc,maxc,
     *                                        rBuf,lBuf,BufSize)

      integer lIn
      integer minr(3),maxr(3),n(3)
      integer nBuf(3),off(3),minc(3),maxc(3)
      integer rBuf,lBuf,BufSize
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      integer i
      logical redo

      external memBuf
      integer  memBuf
c-----------------------------------------------------------------------
      redo = .false.
      do i = 1, 2
        redo = redo .or. minr(i).lt.minc(i)
     *              .or. maxr(i).gt.maxc(i)
      enddo
      redo = redo .or. (maxr(3)-minr(3)+1).gt.nBuf(3) .or.
     *        maxr(3).lt.minc(3) .or. minr(3).gt.maxc(3)

c     If it looks as if we cannot use the previous buffers, recalculate
c     what currently looks best, and load the needed data.
      if (redo) then
        nBuf(1) = n(1)
        nBuf(2) = min(nint(1.2*(maxr(2)-minr(2)))+1,n(2))
        nBuf(3) = min(max(max(memBuf()/2,BufSize)/(nBuf(1)*nBuf(2)),
     *                    maxr(3)-minr(3)+1),n(3))
        if (nBuf(1)*nBuf(2)*nBuf(3).gt.BufSize) then
          if (BufSize.gt.0) then
            call memFree(rBuf,BufSize,'r')
            call memFree(lBuf,BufSize,'l')
          endif
          BufSize = nBuf(1)*nBuf(2)*nBuf(3)
          call memAlloc(rBuf,BufSize,'r')
          call memAlloc(lBuf,BufSize,'l')
        endif
        do i = 1, 3
          off(i) = minr(i) - (nBuf(i) - (maxr(i)-minr(i)+1))/2 - 1
          off(i) = max(0,min(off(i),n(i)-nBuf(i)))
          if (minr(i).lt.off(i)+1 .or. maxr(i).gt.off(i)+nBuf(i))
     *      call bug('f','Algorithmic failure in BufGet')
          if (i.ne.3) then
            minc(i) = 1 + off(i)
            maxc(i) = nBuf(i) + off(i)
          else
            minc(3) = minr(i)
            maxc(3) = maxr(i)
          endif
        enddo
        call BufLoad(lIn,minc,maxc,memr(rBuf),meml(lBuf),
     *    nBuf(1),nBuf(2),nBuf(3),off(1),off(2),off(3))

      else
c       Useful data already in the buffers, and buffers are OK in size.
        call BufCycle(lIn,minc,maxc,minr,maxr,memr(rBuf),meml(lBuf),
     *    nBuf(1),nBuf(2),nBuf(3),off(1),off(2),off(3))
      endif
      end

c***********************************************************************

      subroutine BufLoad(lIn,minc,maxc,Dat,Flags,
     *                        nx,ny,nz,xoff,yoff,zoff)

      integer lIn,minc(3),maxc(3),nx,ny,nz,xoff,yoff,zoff
      real Dat(nx,ny,nz)
      logical Flags(nx,ny,nz)
c-----------------------------------------------------------------------
c  Fill buffers with the appropriate data.
c-----------------------------------------------------------------------
      integer j,k
c-----------------------------------------------------------------------
      if (xoff.ne.0) call bug('f','Load assertion failure')

      do k = minc(3), maxc(3)
        call xysetpl(lIn,1,k)
        do j = minc(2), maxc(2)
          call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
          call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
        enddo
      enddo

      end

c***********************************************************************

      subroutine BadPlane(lOut,n1,n2)

      integer n1,n2,lOut
c-----------------------------------------------------------------------
c  Blank a completely bad plane.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real data(MAXDIM)
      logical flag(MAXDIM)
      integer i,j
c-----------------------------------------------------------------------
      do i = 1, n1
        data(i) = 0
        flag(i) = .false.
      enddo

      do j = 1, n2
        call xywrite(lOut,j,Data)
        call xyflgwr(lOut,j,Flag)
      enddo

      end

c***********************************************************************

      subroutine BufCycle(lIn,minc,maxc,minr,maxr,Dat,Flags,
     *                        nx,ny,nz,xoff,yoff,zoff)

      integer lIn,minc(3),maxc(3),minr(3),maxr(3)
      integer nx,ny,nz,xoff,yoff,zoff
      real Dat(nx,ny,nz)
      logical Flags(nx,ny,nz)
c-----------------------------------------------------------------------
      integer j,k,zoff1
c-----------------------------------------------------------------------
c     Shuffle planes that we already have in memory.
      if (minr(3).lt.1+zoff) then
        zoff1 = max(0,maxr(3) - nz)
        minc(3) = max(minc(3),minr(3))
        maxc(3) = min(maxc(3),maxr(3))
        do k = maxc(3),minc(3),-1
          call scopy(nx*ny,Dat(1,1,k-zoff),1,Dat(1,1,k-zoff1),1)
          call logcopy(nx*ny,Flags(1,1,k-zoff),Flags(1,1,k-zoff1))
        enddo
        zoff = zoff1
      else if (maxr(3).gt.nz+zoff) then
        zoff1 = minr(3) - 1
        minc(3) = max(minc(3),minr(3))
        maxc(3) = min(maxc(3),maxr(3))
        do k = minc(3), maxc(3)
          call scopy(nx*ny,Dat(1,1,k-zoff),1,Dat(1,1,k-zoff1),1)
          call logcopy(nx*ny,Flags(1,1,k-zoff),Flags(1,1,k-zoff1))
        enddo
        zoff = zoff1
      endif

      if (xoff.ne.0) call bug('f','Cycle assertion failure')

c     Read the extra planes.
      do k = minr(3), minc(3)-1
        call xysetpl(lIn,1,k)
        do j = minc(2), maxc(2)
          call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
          call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
        enddo
      enddo

      do k = maxc(3)+1, maxr(3)
        call xysetpl(lIn,1,k)
        do j = minc(2), maxc(2)
          call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
          call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
        enddo
      enddo

      minc(3) = min(minr(3),minc(3))
      maxc(3) = max(maxr(3),maxc(3))

      end

c***********************************************************************

      subroutine logcopy(n,in,out)

      integer n
      logical in(n),out(n)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        out(i) = in(i)
      enddo

      end

c***********************************************************************

      subroutine Intp(lOut,order,nx,ny,in,flagIn,nix,niy,niz,
     *        xoff,yoff,zoff,xv,yv,zv,valid,gnx,gny,nblank)

      integer nx,ny,nix,niy,niz,gnx,gny,order(3),lOut,xoff,yoff,zoff
      integer nblank
      real in(nix,niy,niz),xv(gnx,gny),yv(gnx,gny),zv(gnx,gny)
      logical flagIn(nix,niy,niz),valid(gnx,gny)
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real WtTol
      parameter (WtTol=0.5)

      integer i,j,jx,jy
      double precision fx,fy,x,y,z
      integer id,jd,kd,imin,imax,jmin,jmax,kmin,kmax
      real Sum,SumWt,Wt,Wtx(4),Wty(4),Wtz(4)
      real out(MAXDIM)
      logical flagOut(MAXDIM),cok
c-----------------------------------------------------------------------
      do j = 1, ny
        y = dble((gny-1)*(j-1))/dble(ny-1) + 1
        jy = nint(y - 0.5d0)
        fy = y - jy
        do i = 1, nx
          cok = .true.
          x = dble((gnx-1)*(i-1))/dble(nx-1) + 1
          jx = nint(x - 0.5d0)
          fx = x - jx
          if (fx.eq.0d0) then
            if (fy.eq.0d0) then
              if (valid(jx,jy)) then
                x = xv(jx,jy)
                y = yv(jx,jy)
                z = zv(jx,jy)
              else
                cok = .false.
              endif
            else
              if (valid(jx,jy) .and. valid(jx,jy+1)) then
                x = (1d0-fy) * xv(jx,jy) + fy * xv(jx,jy+1)
                y = (1d0-fy) * yv(jx,jy) + fy * yv(jx,jy+1)
                z = (1d0-fy) * zv(jx,jy) + fy * zv(jx,jy+1)
              else
                cok = .false.
              endif
            endif
          else
            if (fy.eq.0d0) then
              if (valid(jx,jy) .and. valid(jx+1,jy)) then
                x = (1d0-fx) * xv(jx,jy) + fx * xv(jx+1,jy)
                y = (1d0-fx) * yv(jx,jy) + fx * yv(jx+1,jy)
                z = (1d0-fx) * zv(jx,jy) + fx * zv(jx+1,jy)
              else
                cok = .false.
              endif
            else
              if (valid(jx,jy) .and. valid(jx+1,jy) .and.
     *           valid(jx,jy+1) .and. valid(jx+1,jy+1)) then
                x = (1d0-fy)*((1d0-fx)*xv(jx,jy)   + fx*xv(jx+1,jy)) +
     *                   fy *((1d0-fx)*xv(jx,jy+1) + fx*xv(jx+1,jy+1))
                y = (1d0-fy)*((1d0-fx)*yv(jx,jy)   + fx*yv(jx+1,jy)) +
     *                   fy *((1d0-fx)*yv(jx,jy+1) + fx*yv(jx+1,jy+1))
                z = (1d0-fy)*((1d0-fx)*zv(jx,jy)   + fx*zv(jx+1,jy)) +
     *                   fy *((1d0-fx)*zv(jx,jy+1) + fx*zv(jx+1,jy+1))
              else
                cok = .false.
              endif
            endif
          endif

c         We now have the coordinates of the point that we want in the
c         output in terms of the input coordinate system.  Interpolate
c         this point.
          Sum = 0
          SumWt = 0
          if (cok) then
            call coeff(order(1),x-xoff,nix,imin,imax,Wtx)
            call coeff(order(2),y-yoff,niy,jmin,jmax,Wty)
            call coeff(order(3),z-zoff,niz,kmin,kmax,Wtz)

            do kd = kmin, kmax
              do jd = jmin, jmax
                do id = imin, imax
                  if (flagIn(id,jd,kd)) then
                    Wt = Wtx(id-imin+1)*Wty(jd-jmin+1)*Wtz(kd-kmin+1)
                    Sum = Sum + Wt * in(id,jd,kd)
                    SumWt = SumWt + Wt
                  endif
                enddo
              enddo
            enddo
          endif

c         Determine whether the output is good.
          flagOut(i) = SumWt.gt.WtTol
          if (flagOut(i)) then
            out(i) = Sum / SumWt
          else
            nblank = nblank + 1
            out(i) = 0
          endif
        enddo
        call xywrite(lOut,j,out)
        call xyflgwr(lOut,j,flagOut)
      enddo

      end

c***********************************************************************

      subroutine coeff(order,x,nix,imin,imax,Wtx)

      double precision x
      integer nix,imin,imax,order
      real Wtx(4)
c-----------------------------------------------------------------------
      real WtTol
      parameter (WtTol=0.5)
      double precision fx,SumWt
      integer jx,off,i
      logical clip
c-----------------------------------------------------------------------
      if (order.eq.0) then
        imin = nint(x)
        imax = imin
        Wtx(1) = 1
      else if (order.eq.3) then
        jx = nint(x - 0.5d0)
        imin = jx-1
        imax = jx+2

        fx = x - jx
        Wtx(1) = ((-0.5*fx + 1.0)*fx - 0.5)*fx
        Wtx(2) =  ((1.5*fx - 2.5)*fx      )*fx + 1.0
        Wtx(3) = ((-1.5*fx + 2.0)*fx + 0.5)*fx
        Wtx(4) =  ((0.5*fx - 0.5)*fx      )*fx
      else
        call bug('f','Unsupported order, in Coeff')
      endif

c     Clip anything that went outside the range of the valid pixels.
      clip = .false.
      if (imax.gt.nix) then
        clip = .true.
        imax = nix
      endif

      if (imin.lt.1) then
        clip = .true.
        off = 1 - imin
        imin = 1
        do i = 1, imax-imin+1
          Wtx(i) = Wtx(i+off)
        enddo
      endif

c     If the range was clipped, check that the weight is still high
c     enough to bother interpolating.
      if (clip) then
        SumWt = 0
        do i = 1, imax-imin+1
          SumWt = SumWt + Wtx(i)
        enddo

        if (SumWt.lt.WtTol) then
          imin = nix + 1
          imax = 0
        endif
      endif

      end
