c***********************************************************************
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
c@ in
c       The input image name.  If neither 'tin' nor 'desc' (see below)
c       are given, coordinate descriptors for the axes of the regridded
c       image, as selected by keyword 'axes', are copied initially from
c       the input image.  They may then be modified by 'options',
c       'project', and 'rotate'.  No default.
c@ out
c       The output image name.  No default.
c@ axes
c       Specify the axes to regrid.  For example, axes=1,2 regrids axes
c       1 and 2 and the output image will contain two axes.  Likewise,
c       axes=2,3 regrids axes 2 and 3.  The default is all axes.
c@ tin
c       Template image.  If present, coordinate descriptors for the axes
c       of the regridded image, selected by keyword 'axes', are copied
c       initially from the template image rather than the input image.
c@ desc
c       If 'tin' is unset, this optionally specifies the reference value
c       (CRVAL), reference pixel (CRPIX), coordinate increment (CDELT),
c       and number of pixels (respectively) for each and every axis of
c       the output image selected by keyword 'axes'.  Thus, if there are
c       any, then there should be 4 x NAXIS values, separated by commas.
c       These values are not changed by any options other than 'offset'.
c
c       The axis types themselves (CTYPE), including the equatorial
c       coordinate system (B1950 or J2000), are copied initially from
c       the input image, subject to modification by 'options' and
c       'project'.
c
c       Note that for celestial axes (RA/DEC, GLON/GLAT, etc.), the
c       reference values (CRVAL) and increments (CDELT) are in degrees.
c@ options
c       Extra processing options that alter the axis description defined
c       by the template image, axis descriptors, or input image.
c       Several can be given, separated by commas, with minimum-match.
c         altprj    Interpret a CAR (plate carée) projection in the
c                   input map as a simple linear coordinate system with
c                   an additional 1/cos(lat0) scaling factor applied
c                   when computing the longitude, e.g.
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
c                   and B1950 equatorial.
c         galeqsw   Switch the output coordinate system between galactic
c                   and equatorial.  Galactic switches implicitly to
c                   equatorial J2000.
c         nearest   Use nearest neighbour interpolation rather than the
c                   default cubic interpolation.
c       If the equatorial coordinate system is not specified in the
c       header (via the 'epoch' item), then J2000 is assumed.
c@ project
c       Three-letter code for the output map projection, unchanged by
c       default.  Projection codes follow the FITS WCS standard.
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
c       The number of projection parameters is indicated in parentheses.
c       An asterisk indicates that there is a non-defaulting parameter.
c       NOTE: As yet, there is no way to specify projection parameters
c       ~~~~  (thus ZPN, COP, COE, COD, COO, and BON cannot be used).
c       The letters in parentheses are
c         - C  conformal
c         - C? conformal for particular projection parameters
c         - D  divergent
c         - D? divergent for particular projection parameters
c         - d  degenerate (parts of the sphere overlap in the map plane)
c         - E  equi-areal
c         - G  global (capable of mapping the whole sphere)
c       Some projections may be divergent, degenerate, or global
c       depending on the projection parameters.  Refer to Calabretta &
c       Greisen (2002), A&A 395, 1077.
c@ rotate
c       Set the rotation between the sky and the image to be this angle,
c       in degrees.  A positive value of the angle gives an eastward
c       rotation of the sky grid relative to the pixel grid.  The
c       default is not to change the rotation from that of the template,
c       or to have no rotation when there is no template.
c@ tol
c       Interpolation tolerance.  Tolerate an error of the specified
c       amount in converting pixel locations in the input to the output.
c       Must be less that 0.5.  The default is 0.05.
c
c$Id: regrid.for,v 1.4 2010/07/28 07:39:33 cal103 Exp $
c--
c
c  History:
c     7jul92 pjt   Written to keep Neil quiet (not an easy thing to do)
c    23nov92 nebk  CHange to include all three regridding input styles
c                  (template, axis descriptors and start/end/inc).
c                  Deal with blanking, and generally tidy up.  Attempt
c                  to add new code in yucky pjt style.
c    01dec92 nebk  More sig figs in printout
c      jan93 nebk  Rewrite to implement regridding of upto 3 axes in one
c                  pass. All good fun. NEBK becomes owner.
c     2feb93 pjt   extracted spline.for to MIRSUBS
c    10feb93 rjs   redefined double precision dynamic memory array to
c                  size MAXBUF/2.
c    12mar93 mjs   Use maxnax.h file instead of its own set value.
c    22jun93 nebk  Take cos(DEC) into account for RA/DEC axes
c    15apr94 nebk  Keyword range input was failing.
c    03jan95 nebk  REGZ was failing if output longer than input
c    26jun95 nebk  Warning instead of fatal if ungridded axes
c                  have silly axis descriptors. options=quiet
c    16nov96 nebk  AXFNDCG -> AXFNDCO and new call sequence
c    24mar97 rjs   At least warn people when it may screw up because of
c                  blanked pixels.
c    07jul97 rjs   Gone back to the drawing board and rewrite program.
c    22jul97 rjs   Fix bug when subimaging grossly.
c    22jul97 rjs   Support epoch,projection and galactic/equatorial
c                  axis conversion.
c    23jul97 rjs   Add warning about blanked pixels. Add pbtype
c    05aug97 rjs   Messages about equinox fiddles.
c    20nov98 rjs   Handle and eliminate sky rotation.
c    25nov98 rjs   Better handling of sky rotation parameter.
c    18may99 rjs   Handle wrap arounds in the offsets.
c    10jul00 rjs   Add "rotate" keyword, and get rid of "norotate"
c                  option.
c    27jun02 rjs   An arg to mkeyd was single precision.
c    31may06 rjs   Changes to use new calling sequence to pCvt.
c                  Generally make more robust to regridding where a
c                  significant fraction of the sky is involved.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'

      integer NPCODE
      parameter (NPCODE = 29)

      logical   altPrj, doDesc, doEqEq, doGalEq, doNear, doOff, doRot,
     *          noScale
      integer   axes(MAXNAX), BufSize, cOut, gnx, gny, GridSize, i,
     *          ilat, k, lBuf, lIn, lOut, lTem, maxc(3), maxv(3),
     *          minc(3), minv(3), n, nax, naxis, nblank, nBuf(3),
     *          ndesc, nin(MAXNAX), nout(MAXNAX), ntin(MAXNAX), nxy,
     *          off(3), offset, order(3), rBuf, valid, Xv, Yv, Zv
      real      tol
      double precision cdelt, crpix, crval, desc(4,MAXNAX), eqnox,
     *          llrot, rot
      character cellscal*12, ctype*16, in*64, line*64, out*64, pcode*3,
     *          pcodes(NPCODE)*3, tin*64, version*80

c     Externals.
      logical keyprsnt
      character versan*80

c     Projection codes (those in upper-case require non-defaulting
c     projection parameters and are not currently supported).
      data pcodes /
     *  'azp', 'szp', 'tan', 'stg', 'sin', 'ncp', 'arc', 'ZPN',
     *  'zea', 'air', 'cyp', 'cea', 'car', 'mer', 'COP', 'COE',
     *  'COD', 'COO', 'sfl', 'gls', 'par', 'mol', 'ait', 'BON',
     *  'pco', 'tsc', 'csc', 'qsc', 'hpx'/
c-----------------------------------------------------------------------
      version = versan ('regrid',
     *                  '$Revision: 1.4 $',
     *                  '$Date: 2010/07/28 07:39:33 $')
c
c  Get the input parameters.
c
      call keyini
      call keya('in',in,' ')
      if (in.eq.' ')call bug('f','An input must be given')
      call keya('out',out,' ')
      if (out.eq.' ')call bug('f','An output must be given')
      call mkeyi('axes',axes,MAXNAX,nax)
      call keya('tin',tin,' ')
      call mkeyd('desc',desc,4*MAXNAX,ndesc)
      if (mod(ndesc,4).ne.0)
     *  call bug('f','Invalid number of axis descriptors')
      ndesc = ndesc/4
      call keyr('tol',tol,0.05)
      if (tol.lt.0.or.tol.ge.0.5)
     *  call bug('f','Invalid value for the tol parameter')
      call getopt(altPrj,noScale,doOff,doEqEq,doGalEq,doNear)
      call keymatch('project',NPCODE,pcodes,1,pcode,n)
      if (n.eq.0) pcode = ' '
      doRot = keyprsnt('rotate')
      call keyd('rotate',rot,0d0)
      rot = DPI/180d0 * rot
      call keyfin
c
c  Open the input dataset.
c
      call xyopen(lIn,in,'old',MAXNAX,nin)
      call rdhdi(lIn,'naxis',naxis,0)
      naxis = min(naxis,MAXNAX)
      do i=1,naxis
        nout(i) = nin(i)
      enddo
      do i=naxis+1,MAXNAX
        nout(i) = 1
      enddo
c
c  Check the users "axes" specification.
c
      if (nax.gt.0) then
        do i=1,nax
          if (axes(i).lt.1.or.axes(i).gt.naxis)
     *      call bug('f','Invalid "axes" value')
        enddo
      else
        nax = naxis
        do i=1,nax
          axes(i) = i
        enddo
      endif
c
c  Set up the output size/coordinate system given template or
c  descriptors.
c
      call coInit(lIn)
      if (altPrj) call coAltPrj(lIn)
      call coDup(lIn,cOut)

      doDesc = .false.
      if (tin.ne.' ') then
        if (ndesc.ne.0) then
          call bug('w','Using template, ignoring descriptors.')
        endif

        call xyopen(lTem,tin,'old',MAXNAX,ntin)
        call rdhdi(lTem,'naxis',n,0)
        n = min(n,MAXNAX)
        call coInit(lTem)
        call coFindAx(lTem,'latitude',ilat)

        do i=1,nax
          if (axes(i).gt.n)call bug('f',
     *        'Requested axis does not exist in the template')
          nout(axes(i)) = ntin(axes(i))
          call coAxGet(lTem,axes(i),ctype,crpix,crval,cdelt)
          call coAxSet(cOut,axes(i),ctype,crpix,crval,cdelt)
          if (axes(i).eq.ilat) then
            call coGetD(lTem,'llrot',llrot)
            call coSetD(cOut,'llrot',llrot)
          endif
        enddo

        call coGetD(lTem,'epoch',eqnox)
        call coSetD(cOut,'epoch',eqnox)
        call coGetA(lTem,'cellscal',cellscal)
        call coSetA(cOut,'cellscal',cellscal)
        call xyclose(lTem)

      else if (ndesc.ne.0) then
        if (ndesc.ne.nax) call bug('f',
     *    'Inconsistent number of axis descriptors given.')

        do i = 1, nax
          nout(axes(i)) = nint(desc(4,i))
          if (nout(axes(i)).lt.1 .or. nout(axes(i)).gt.MAXDIM) then
            call bug('f','Invalid axis size in axis descriptor')
          endif

          call coAxGet(lIn, axes(i),ctype,crpix,crval,cdelt)
          call coAxSet(cOut,axes(i),ctype,desc(2,i),desc(1,i),desc(3,i))
        enddo

        doDesc = .true.

      else
        doOff = .true.
      endif
c
c  Process options.
c
      if (noScale) call coSetA(cOut,'cellscal','CONSTANT')
      if (doRot)   call coSetD(cOut,'llrot',rot)
      if (pcode.ne.' ') call coPrjSet(cOut,pcode)

c     Set up output celestial coordinates.
      call setCel(lIn, cOut, doDesc, doEqEq, doGalEq)

c     Set up offset coordinates.
      if (doOff) call doOffset(lIn,nIn,cOut,nOut)
c
c  Create the output.
c
c     Do basic checks.
      if (nout(1).gt.MAXDIM) call bug('f','Output too big for me')
      do k = 4, naxis
        if (nOut(k).gt.1) call bug('f','Cannot handle hypercubes')
      enddo

      call xyopen(lOut,out,'new',naxis,nout)
      call MkHeader(lIn,lOut,cOut,version)
c
c  Initialise things.
c
      GridSize = 0
      call pCvtInit(cOut,lIn)
      call BufIni(nBuf,off,minc,maxc,BufSize)

      nblank = 0
      do k=1,nOut(3)
        call xysetpl(lOut,1,k)
c
c  Determine the size of the coordinate translation grid.
c
        if (tol.eq.0) then
          gnx = nOut(1)
          gny = nOut(2)
        else
          call GridEst(nOut(1),nOut(2),k,gnx,gny,tol)
        endif
c
c  Allocate space used for the coordinate translation grid.
c
        if (gnx*gny.gt.GridSize) then
          if (GridSize.gt.0) then
            call memFree(Xv,GridSize,'r')
            call memFree(Yv,GridSize,'r')
            call memFree(Zv,GridSize,'r')
            call memFree(valid,GridSize,'l')
          endif
          GridSize = gnx*gny
          call memAlloc(Xv,GridSize,'r')
          call memAlloc(Yv,GridSize,'r')
          call memAlloc(Zv,GridSize,'r')
          call memAlloc(valid,GridSize,'l')
        endif
c
c  Calculate the coordinates translation grid, and work out some
c  statistics about it.
c
        call GridGen(nOut(1),nOut(2),k,
     *        memr(Xv),memr(Yv),memr(Zv),meml(Valid),gnx,gny)
        call GridStat(doNear,memr(Xv),memr(Yv),memr(Zv),meml(valid),
     *        gnx,gny,nin(1),nin(2),nin(3),tol,minv,maxv,order)

        if (minv(1).gt.maxv(1) .or.
     *      minv(2).gt.maxv(2) .or.
     *      minv(3).gt.maxv(3)) then
          nblank = nblank + nOut(1)*nOut(2)
          call BadPlane(lOut,nOut(1),nOut(2))
c
c  Get the required data.
c
        else
          call BufGet(lIn,minv,maxv,nIn,nBuf,off,minc,maxc,
     *                                        rBuf,lBuf,BufSize)
          offset = minc(3) - off(3) - 1
          nxy = nBuf(1)*nBuf(2)
c
c  Finally to the interpolation.
c
          call Intp(lOut,order,nOut(1),nOut(2),
     *      memr(rBuf+offset*nxy),meml(lBuf+offset*nxy),
     *      nBuf(1),nBuf(2),maxc(3)-minc(3)+1,
     *      off(1),off(2),minc(3)-1,
     *      memr(Xv),memr(Yv),memr(Zv),meml(valid),gnx,gny,nblank)
        endif
      enddo
c
c  Give warning about the number of blanked pixels.
c
      nblank = (100*nblank)/(nOut(1)*nOut(2)*nOut(3))
      write(line,'(a,i3,a)')
     *  'Overall fraction of blanked pixels: ',nblank,'%'
      if (nblank.ge.50) then
        call bug('w',line)
      else if (nblank.ne.0) then
        call output(line)
      endif
c
c  All done. Tidy up.
c
      if (BufSize.gt.0) then
        call memFree(rBuf,BufSize,'r')
        call memFree(lBuf,BufSize,'l')
      endif
      if (GridSize.gt.0) then
        call memFree(Xv,GridSize,'r')
        call memFree(Yv,GridSize,'r')
        call memFree(Zv,GridSize,'r')
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
      parameter(NOPTS=6)
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
      subroutine setCel(lIn, lOut, doDesc, doEqEq, doGalEq)

      integer lIn, lOut
      logical doDesc, doEqEq, doGalEq

c  Set up output celestial coordinates.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      logical   doeqnx, gotone
      integer   ilat, ilng
      double precision cdelt1, cdelt2, crpix1, crpix2, crval1, crval2,
     *          ddec, dec, dra, eqnox, obstime, ra
      character ctype1*16, ctype2*16, type1*4, type2*4

c     Externals.
      double precision epo2jul
c-----------------------------------------------------------------------
      call coReInit(lOut)
      if (.not.(doDesc .or. doEqEq .or. doGalEq)) return

c     Look for a celestial axis pair.
      call coFindAx(lOut,'longitude',ilng)
      call coFindAx(lOut,'latitude',ilat)
      if (ilng.eq.0 .or. ilat.eq.0)
     *  call bug('f','Cannot find RA/DEC or GLON/GLAT axes')

      call coAxGet(lOut,ilng,ctype1,crpix1,crval1,cdelt1)
      call coAxGet(lOut,ilat,ctype2,crpix2,crval2,cdelt2)

      if (doDesc) then
c       Convert to radians.
        crval1 = crval1 * DD2R
        crval2 = crval2 * DD2R
        cdelt1 = cdelt1 * DD2R
        cdelt2 = cdelt2 * DD2R

        if (.not.(doEqEq .or. doGalEq)) then
          call coAxSet(lOut,ilng,ctype1,crpix1,crval1,cdelt1)
          call coAxSet(lOut,ilat,ctype2,crpix2,crval2,cdelt2)
          call coReInit(lOut)
          return
        endif
      endif

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
      call coGetD(lOut,'epoch',eqnox)
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
     *        'Cannot convert other than B1950/J2000 equatorial')
        endif

        gotone = .true.
      endif

      if (gotone) then
        call coSetD(lIn,'obstime',obstime)

        call coAxSet(lOut,ilng,ctype1,crpix1,crval1,cdelt1)
        call coAxSet(lOut,ilat,ctype2,crpix2,crval2,cdelt2)
        call coSetD(lOut,'epoch',eqnox)
        call coSetD(lOut,'obstime',obstime)

        call coReinit(lOut)
      endif

      end
c***********************************************************************
      subroutine doOffset(lIn,nIn,lOut,nOut)

      integer lIn,lOut,nIn(3),nOut(3)
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer NV
      double precision tol
      parameter(tol=0.49,NV=10)
      double precision In(3),Out(3,NV,NV,NV),crpix
      integer minv(3),maxv(3),i,j,k,l,nv1,nv2,nv3
      logical weird(3),warned,valid(NV,NV,NV),first

c     Externals.
      character itoaf*2
c-----------------------------------------------------------------------
      call pCvtInit(lIn,lOut)

      nv3 = min(max(3,nIn(3)),NV)
      nv2 = min(max(3,nIn(2)),NV)
      nv1 = min(max(3,nIn(1)),NV)

      first = .true.
      do k=1,nv3
        do j=1,nv2
          do i=1,nv1
            In(1) = 1 + (i-1)*(nIn(1)-1)/real(nv1-1)
            In(2) = 1 + (j-1)*(nIn(2)-1)/real(nv2-1)
            In(3) = 1 + (k-1)*(nIn(3)-1)/real(nv3-1)
            call pCvt(in,out(1,i,j,k),3,valid(i,j,k))

            if (valid(i,j,k).and.first) then
              first = .false.
              do l=1,3
                weird(l) = .false.
                minv(l) = nint(Out(l,i,j,k)-0.49)
                maxv(l) = nint(Out(l,i,j,k)+0.49)
              enddo
            endif
          enddo
        enddo
      enddo

      if (first)call bug('f','No valid pixels found')
c
c  Determine the min and max pixels that we are interested in.
c
      do k=1,nv3
        do j=1,nv2
          do i=1,nv1
            do l=1,3
              if (valid(i,j,k)) then
                minv(l) = min(minv(l),nint(Out(l,i,j,k)-0.49))
                maxv(l) = max(maxv(l),nint(Out(l,i,j,k)+0.49))
              endif
            enddo
          enddo
        enddo
      enddo
c
c  Look for weird twists or wrap arounds.
c
      do k=1,nv3
        do j=1,nv2
          do i=1,nv1-2
            if (valid(i+2,j,k).and.valid(i+1,j,k).and.valid(i,j,k)) then
              weird(1) = weird(1).or.
     *          (Out(1,i+2,j,k)-Out(1,i+1,j,k))*
     *          (Out(1,i+1,j,k)-Out(1,i,j,k)).lt.0
            endif
          enddo
        enddo
      enddo
      do k=1,nv3
        do j=1,nv2-2
          do i=1,nv1
            if (valid(i,j+2,k).and.valid(i,j+1,k).and.valid(i,j,k)) then
              weird(2) = weird(2).or.
     *          (Out(2,i,j+2,k)-Out(2,i,j+1,k))*
     *          (Out(2,i,j+1,k)-Out(2,i,j,k)).lt.0
            endif
          enddo
        enddo
      enddo
      do k=1,nv3-2
        do j=1,nv2
          do i=1,nv1
            if (valid(i,j,k+2).and.valid(i,j,k+1).and.valid(i,j,k)) then
              weird(3) = weird(3).or.
     *          (Out(3,i,j,k+2)-Out(3,i,j,k+1))*
     *          (Out(3,i,j,k+1)-Out(3,i,j,k)).lt.0
            endif
          enddo
        enddo
      enddo
c
c  Set the template ranges. If its weird, just go with the template
c  range and the max and min of mapped pixels.
c
      warned = .false.
      do i = 1, 3
        if (weird(i)) then
          minv(i) = min(1,minv(i))
          maxv(i) = max(nOut(i),maxv(i))
        endif

        nOut(i) = maxv(i) - minv(i) + 1
        if (nout(i).gt.MAXDIM) then
          nout(i) = MAXDIM
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
      subroutine MkHeader(lIn,lOut,cOut,version)

      integer lIn,lOut,cOut
      character version*(*)
c-----------------------------------------------------------------------
      integer i
      character line*64

      integer nkeys
      parameter(nkeys=21)
      character key(nkeys)*8
      data key /   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *  'history ','instrume','niters  ',
     *  'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *  'telescop','btype   ','rms     ','pbtype  ',
     *  'ltype   ','lstart  ','lwidth  ','lstep   ','mostable'/
c-----------------------------------------------------------------------
      do i=1,nkeys
        call hdcopy(lIn,lOut,key(i))
      enddo

      call coWrite(cOut,lOut)
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
      do j=1,2
        do i=1,2
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
      dowhile(more)
        n = 2*n
        errmax = -1
        do k=1,4
          xmid(1,k) = 0.5*(x(1,k) + mid(1))
          xmid(2,k) = 0.5*(x(2,k) + mid(2))
          xmid(3,k) = 0.5*(x(3,k) + mid(3))
          call pCvt(xmid(1,k),txmid(1,k),3,valid)
          if (.not.valid)
     *      call bug('f','Invalid coordinate: please use tol=0')
          err = max( abs(0.5*(tx(1,k)+tmid(1)) - txmid(1,k)),
     *               abs(0.5*(tx(2,k)+tmid(2)) - txmid(2,k)),
     *               abs(0.5*(tx(3,k)+tmid(3)) - txmid(3,k)))
          if (err.gt.errmax) then
            kmax = k
            errmax = err
          endif
        enddo
        more = errmax.gt.tol.and.max(nx,ny).gt.n+1
c
c  If the tolerance has not yet been reached, home in on the
c  region where the fit was worst.
c
        if (more) then
          k1 = 1
          if (k1.eq.kmax)k1 = k1 + 1
          k2 = k1 + 1
          if (k2.eq.kmax)k2 = k2 + 1
          k3 = k2 + 1
          if (k3.eq.kmax)k3 = k3 + 1
          call TripMv(mid(1),   mid(2), mid(3),      x(1,k1))
          call TripMv(tmid(1),  tmid(2),tmid(3),     tx(1,k1))
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
c-----------------------------------------------------------------------
      double precision a,b,c,x(3)
c-----------------------------------------------------------------------
      x(1) = a
      x(2) = b
      x(3) = c
      end
c***********************************************************************
      subroutine GridGen(nx,ny,plane,Xv,Yv,Zv,valid,gnx,gny)

      integer nx,ny,plane,gnx,gny
      real Xv(gnx,gny),Yv(gnx,gny),Zv(gnx,gny)
      logical valid(gnx,gny)

c  Determine the translation between the output and input pixel
c  coordinates on a grid.
c
c  Input:
c    nx,ny
c    plane
c    gnx,gny
c  Output:
c    Xv,Yv,Zv,valid
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer i,j
      double precision In(3),Out(3)
c-----------------------------------------------------------------------
      In(3) = plane

      do j=1,gny
        In(2) = dble(ny-1)/dble(gny-1) * (j-1) + 1
        do i=1,gnx
          In(1) = dble(nx-1)/dble(gnx-1) * (i-1) + 1
          call pCvt(In,Out,3,valid(i,j))
          if (valid(i,j)) then
            Xv(i,j) = Out(1)
            Yv(i,j) = Out(2)
            Zv(i,j) = Out(3)
          endif
        enddo
      enddo

      end
c***********************************************************************
      subroutine GridStat(doNear,Xv,Yv,Zv,valid,gnx,gny,n1,n2,n3,
     *    tol,minv,maxv,order)

      logical doNear
      integer gnx,gny,n1,n2,n3,minv(3),maxv(3),order(3)
      real Xv(gnx,gny),Yv(gnx,gny),Zv(gnx,gny),tol
      logical valid(gnx,gny)
c-----------------------------------------------------------------------
      integer i,j,n(3)
      real minr(3),maxr(3),diff
      logical first
c-----------------------------------------------------------------------
      first = .true.
      do j=1,gny
        do i=1,gnx
          if (valid(i,j)) then
            if (first) then
              first = .false.
              minr(1) = Xv(i,j)
              maxr(1) = minr(1)
              minr(2) = Yv(i,j)
              maxr(2) = minr(2)
              minr(3) = Zv(i,j)
              maxr(3) = minr(3)
            else
              minr(1) = min(Xv(i,j),minr(1))
              maxr(1) = max(Xv(i,j),maxr(1))
              minr(2) = min(Yv(i,j),minr(2))
              maxr(2) = max(Yv(i,j),maxr(2))
              minr(3) = min(Zv(i,j),minr(3))
              maxr(3) = max(Zv(i,j),maxr(3))
            endif
          endif
        enddo
      enddo

      if (first)call bug('f','No valid pixels found here')
c
c  Determine which are going to be done by nearest neighbour, and
c  what are the min and max limits needed for interpolation.
c
      n(1) = n1
      n(2) = n2
      n(3) = n3
      do i=1,3
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
      do i=1,3
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

c     Externals.
      integer memBuf
c-----------------------------------------------------------------------
      redo = .false.
      do i=1,2
        redo = redo.or.minr(i).lt.minc(i)
     *             .or.maxr(i).gt.maxc(i)
      enddo
      redo = redo.or.(maxr(3)-minr(3)+1).gt.nBuf(3).or.
     *        maxr(3).lt.minc(3).or.minr(3).gt.maxc(3)
c
c  If it looks as if we cannot use the previous buffers, recalculate
c  what currently looks best, and load the needed data.
c
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
        do i=1,3
          off(i) = minr(i) - (nBuf(i) - (maxr(i)-minr(i)+1))/2 - 1
          off(i) = max(0,min(off(i),n(i)-nBuf(i)))
          if (minr(i).lt.off(i)+1.or.maxr(i).gt.off(i)+nBuf(i))
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
c
c  Handle the case of there being useful data already in the buffers,
c  and that the buffers are OK in size.
c
      else
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

c  Fill buffers up with the appropriate data.
c-----------------------------------------------------------------------
      integer j,k
c-----------------------------------------------------------------------
      if (xoff.ne.0)call bug('f','Load assertion failure')

      do k=minc(3),maxc(3)
        call xysetpl(lIn,1,k)
        do j=minc(2),maxc(2)
          call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
          call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
        enddo
      enddo

      end
c***********************************************************************
      subroutine BadPlane(lOut,n1,n2)

      integer n1,n2,lOut

c  Blank out a completely bad plane.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real data(MAXDIM)
      logical flag(MAXDIM)
      integer i,j
c-----------------------------------------------------------------------
      do i=1,n1
        data(i) = 0
        flag(i) = .false.
      enddo

      do j=1,n2
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
c
c  Shuffle around planes that we already have in memory.
c
      if (minr(3).lt.1+zoff) then
        zoff1 = max(0,maxr(3) - nz)
        minc(3) = max(minc(3),minr(3))
        maxc(3) = min(maxc(3),maxr(3))
        do k=maxc(3),minc(3),-1
          call scopy(nx*ny,Dat(1,1,k-zoff),1,Dat(1,1,k-zoff1),1)
          call logcopy(nx*ny,Flags(1,1,k-zoff),Flags(1,1,k-zoff1))
        enddo
        zoff = zoff1
      else if (maxr(3).gt.nz+zoff) then
        zoff1 = minr(3) - 1
        minc(3) = max(minc(3),minr(3))
        maxc(3) = min(maxc(3),maxr(3))
        do k=minc(3),maxc(3)
          call scopy(nx*ny,Dat(1,1,k-zoff),1,Dat(1,1,k-zoff1),1)
          call logcopy(nx*ny,Flags(1,1,k-zoff),Flags(1,1,k-zoff1))
        enddo
        zoff = zoff1
      endif

      if (xoff.ne.0)call bug('f','Cycle assertion failure')
c
c  Read in the extra planes.
c
      do k=minr(3),minc(3)-1
        call xysetpl(lIn,1,k)
        do j=minc(2),maxc(2)
          call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
          call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
        enddo
      enddo
      do k=maxc(3)+1,maxr(3)
        call xysetpl(lIn,1,k)
        do j=minc(2),maxc(2)
          call xyread(lIn,j,Dat(1,j-yoff,k-zoff))
          call xyflgrd(lIn,j,Flags(1,j-yoff,k-zoff))
        enddo
      enddo

      minc(3) = min(minr(3),minc(3))
      maxc(3) = max(maxr(3),maxc(3))

      end
c***********************************************************************
      subroutine logcopy(n,In,Out)

      integer n
      logical In(n),Out(n)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i=1,n
        Out(i) = In(i)
      enddo

      end
c***********************************************************************
      subroutine Intp(lOut,order,nx,ny,In,flagIn,nix,niy,niz,
     *        xoff,yoff,zoff,Xv,Yv,Zv,valid,gnx,gny,nblank)

      integer nx,ny,nix,niy,niz,gnx,gny,order(3),lOut,xoff,yoff,zoff
      integer nblank
      real In(nix,niy,niz),Xv(gnx,gny),Yv(gnx,gny),Zv(gnx,gny)
      logical flagIn(nix,niy,niz),valid(gnx,gny)
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real WtTol
      parameter(WtTol=0.5)

      integer i,j,jx,jy
      double precision fx,fy,x,y,z
      integer id,jd,kd,imin,imax,jmin,jmax,kmin,kmax
      real Sum,SumWt,Wt,Wtx(4),Wty(4),Wtz(4)
      real Out(MAXDIM)
      logical flagOut(MAXDIM),cok
c-----------------------------------------------------------------------
      do j=1,ny
        y = dble((gny-1)*(j-1))/dble(ny-1) + 1
        jy = nint(y - 0.5d0)
        fy = y - jy
        do i=1,nx
          cok = .true.
          x = dble((gnx-1)*(i-1))/dble(nx-1) + 1
          jx = nint(x - 0.5d0)
          fx = x - jx
          if (fx.eq.0) then
            if (fy.eq.0) then
              if (valid(jx,jy)) then
                x = Xv(jx,jy)
                y = Yv(jx,jy)
                z = Zv(jx,jy)
              else
                cok = .false.
              endif
            else
              if (valid(jx,jy).and.valid(jx,jy+1)) then
                x = (1-fy) * Xv(jx,jy) + fy * Xv(jx,jy+1)
                y = (1-fy) * Yv(jx,jy) + fy * Yv(jx,jy+1)
                z = (1-fy) * Zv(jx,jy) + fy * Zv(jx,jy+1)
              else
                cok = .false.
              endif
            endif
          else
            if (fy.eq.0) then
              if (valid(jx,jy).and.valid(jx+1,jy)) then
                x = (1-fx) * Xv(jx,jy) + fx * Xv(jx+1,jy)
                y = (1-fx) * Yv(jx,jy) + fx * Yv(jx+1,jy)
                z = (1-fx) * Zv(jx,jy) + fx * Zv(jx+1,jy)
              else
                cok = .false.
              endif
            else
              if (valid(jx,jy).and.valid(jx+1,jy).and.
     *           valid(jx,jy+1).and.valid(jx+1,jy+1)) then
                x = (1-fy)*((1-fx)*Xv(jx,jy)  +fx*Xv(jx+1,jy)  )
     *              + fy *((1-fx)*Xv(jx,jy+1)+fx*Xv(jx+1,jy+1))
                y = (1-fy)*((1-fx)*Yv(jx,jy)  +fx*Yv(jx+1,jy)  )
     *               + fy *((1-fx)*Yv(jx,jy+1)+fx*Yv(jx+1,jy+1))
                z = (1-fy)*((1-fx)*Zv(jx,jy)  +fx*Zv(jx+1,jy)  )
     *               + fy *((1-fx)*Zv(jx,jy+1)+fx*Zv(jx+1,jy+1))
              else
                cok = .false.
              endif
            endif
          endif
c
c  We now have the coordinates of the point that we want in the output
c  in terms of the input coordinate system. Interpolate this point.
c
          Sum = 0
          SumWt = 0
          if (cok) then
            call coeff(order(1),x-xoff,nix,imin,imax,Wtx)
            call coeff(order(2),y-yoff,niy,jmin,jmax,Wty)
            call coeff(order(3),z-zoff,niz,kmin,kmax,Wtz)

            do kd=kmin,kmax
              do jd=jmin,jmax
                do id=imin,imax
                  if (flagIn(id,jd,kd)) then
                    Wt = Wtx(id-imin+1)*Wty(jd-jmin+1)*Wtz(kd-kmin+1)
                    Sum = Sum + Wt * In(id,jd,kd)
                    SumWt = SumWt + Wt
                  endif
                enddo
              enddo
            enddo
          endif
c
c  Determine whether the output is good or not.
c
          flagOut(i) = SumWt.gt.WtTol
          if (flagOut(i)) then
            Out(i) = Sum / SumWt
          else
            nblank = nblank + 1
            Out(i) = 0
          endif
        enddo
        call xywrite(lOut,j,Out)
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
      parameter(WtTol=0.5)
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
        Wtx(1) = ((-0.5*fx+1.0)*fx-0.5)*fx
        Wtx(2) = (( 1.5*fx-2.5)*fx    )*fx + 1.0
        Wtx(3) = ((-1.5*fx+2.0)*fx+0.5)*fx
        Wtx(4) = (( 0.5*fx-0.5)*fx    )*fx
      else
        call bug('f','Unsupported order, in Coeff')
      endif
c
c  Clip back anything that went outside the range of the valid pixels.
c
      clip = .false.
      if (imax.gt.nix) then
        clip = .true.
        imax = nix
      endif
      if (imin.lt.1) then
        clip = .true.
        off = 1 - imin
        imin = 1
        do i=1,imax-imin+1
          Wtx(i) = Wtx(i+off)
        enddo
      endif
c
c  If the range had to be clipped back, check that there is still enough
c  weighting to bother to interpolate.
c
      if (clip) then
        SumWt = 0
        do i=1,imax-imin+1
          SumWt = SumWt + Wtx(i)
        enddo

        if (SumWt.lt.WtTol) then
          imin = nix + 1
          imax = 0
        endif
      endif

      end
