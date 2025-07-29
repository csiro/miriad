      program convol

c= convol - Convolve a cube with a "beam function"
c& rjs mchw
c: map manipulation, map analysis
c+
c       CONVOL is a MIRIAD task which convolves a map or model with a
c       "beam".  WARNING: Convol does not do a very good job with masked
c       images.
c
c@ map
c       The input image. This can be two or three dimensional.
c       No default.
c@ beam
c       The input beam.  This cannot be 3-dimensional.  The beam is
c       generally assumed to be symmetrical about its reference pixel,
c       but see the "asymmetric" option if this is not so.  If this is
c       not given, then a Gaussian beam must be specified by the fwhm
c       and pa parameters.
c@ fwhm
c       This parameter is ignored if the "beam" keyword is given.
c       Otherwise, it specifies the width of the Gaussian convolving
c       function unless options=final, in which case it specifies the
c       required resolution of the output image.  The size, in arcsec,
c       will normally be two numbers giving the full-width at half-
c       maximum of the major and minor axes of the Gaussian.  At least
c       one number must be given.  If only one is given, the Gaussian
c       will have equal axes.
c@ pa
c       This parameter is ignored if the "beam" keyword is given.
c       Otherwise, it specifies the position angle of the Gaussian
c       convolving function, unless options=final, in which case it
c       specifies the required position angle of the effective beam of
c       the output image.  It is measured north through towards east, in
c       degrees.  Default 0.
c@ region
c       The region of the input map to convolve.  See the Users Manual
c       for instructions on how to specify this.  The default is the
c       entire input image.
c@ out
c       The output image. No default.
c@ options
c       Some extra processing options. Several can be given, separated
c       by commas. Minimum match is used.
c         "final"    When parameters are given by the FWHM and PA
c                    keywords, the "final" option causes CONVOL to
c                    interpret these as the resolution required for the
c                    final output image.
c         "divide"   Divide, rather than multiply, the transform of the
c                    map by the transform of the beam.  That is, perform
c                    deconvolution, rather than convolution.
c         "asymmetric" Normally the beam is assumed to be symmetric (as
c                    it normally is in radio astronomy).  This options
c                    causes CONVOL to go through the extra steps to
c                    handle a possibly asymmetric beam.  If the beam is
c                    asymmetric, and you do not give this option, CONVOL
c                    symmetrises the beam (i.e. discards the anti-
c                    symmetric component).
c         "correlate" Correlate rather than convolve with the beam.  The
c                    difference between correlation and convolution are
c                    only apparent for asymmetric beams.
c         "cube"     Like final, but calculate required convolution for
c                    each plane separately. This option implies final.
c                    Use this to get the same beamsize in arcsec in all
c                    planes rather than the same beamsize in pixels.
c                    Warning: analysis tasks are likely to get source 
c                    size and total flux wrong for the resulting cube.
c     @ scale
c       The scale factor to multiply the output by.  The default is for
c       CONVOL to determine the appropriate scale factor to make the
c       output in JY/BEAM.
c       NOTE: If the input image is in units of JY/BEAM, then to
c       determine the appropriate scale factor, CONVOL must know the
c       beam parameters (bmaj, bmin and bpa items) of both the input map
c       and the beam.  If these items are not present, CONVOL issues a
c       warning and uses a scale factor of 1.
c@ sigma
c       When doing devonvolution (options=divide), this gives a noise
c       parameter. Default is 0.
c
c--
c  History:
c    rjs,mchw 18aug89 Converted from RESTORE.
c    rjs      30apr90 Changed call sequence to Boxinput.
c    mchw     04dec90 Added pbfwhm to header.
c    mchw     03apr91 Minor mods to doc and checking inputs.
c    rjs      03apr91 Use the Mem routines. Fixed bug relating to beam
c                     and image of different sizes.
c    rjs,mchw 30may91 Make it handle asymmetric beams, by using the
c                     ConvlC routines. Better handling of units. Divide
c                     option.
c    rjs      13sep91 Friday the 13th change.  Use new convolution
c                     routines.  Writes mask.  Options=asymmetric.
c                     Corrections to the doc.
c    rjs      10mar92 Increased size of maxruns.
c    nebk     25nov92 Copy btype to output
c    rjs      22nov93 Handle gaussian beam. Get rid of "scale" option.
c    rjs      11jan93 Honour explicit scale factors.
c    mchw  06sep94 Set default bmin to be bmaj, and fix log line in doc.
c    rjs   15mar95 Add options=final.
c    rjs   06jan97 Improve output headers.
c    rjs   02jul97 cellscal change.
c    rjs   05dec97 Change order of boxmask and boxinfo calls.
c    bpw   12mar99 Increase size of map/beam/out to 512 to allow
c                  directories.
c    dpr   21jun01 Doc change only
c    mhw   27oct11 Use ptrdiff type for memory allocations
c    mhw   07apr17 Add cube option      
c  Bugs:
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
      include 'mem.h'

      integer    MAXBOX, MAXRUNS
      parameter (MAXBOX  = 1024, MAXRUNS = 3*maxdim)

      logical   asym, corr, divide, dogaus, doscale, final, rect,
     *          selfscal,dofscale,cube
      integer   Box(MAXBOX), Runs(3,MAXRUNS), blc(3), ifail,
     *          iref, jref, k, l, lBeam, lMap, lOut, n1, n2,
     *          nRuns, naxis, nsize(MAXNAX), nx, ny, nz, trc(3), xmax,
     *          xmin, xoff, ymax, ymin, yoff
      ptrdiff   handle, pDat, nPoint
      real      bmaj, bmaj1, bmin, bmin1, bpa, bpa1, crpix1, crpix2,
     *          factor, sigma, temp, bmaj0, bmin0, bpa0,
     *          bmajmap,bminmap,bpamap,bmaj2,bmin2,bpa2,fscale
      double precision cdelt1, cdelt2, x(3)
      character beam*512, bunit*32, flags*4, map*512, out*512, text*80,
     *          version*72, bunit1*32

      logical   BoxRect,keyprsnt
      character itoaf*8, versan*72, cellscal*8
      external  boxrect, itoaf, keyprsnt, versan
c-----------------------------------------------------------------------
      version = versan('convol')
c
c  Get the input parameters.
c
      call keyini
      call keya('map',Map,' ')
      call keya('beam',Beam,' ')
      doGaus = Beam.eq.' '
      if (doGaus) then
        call keyr('fwhm',bmaj1,0.0)
        call keyr('fwhm',bmin1,bmaj1)
        call keyr('pa',bpa1,0.0)
      endif
      call keya('out',Out,' ')
      call BoxInput('region',map,box,MAXBOX)
      call GetOpt(final,divide,asym,corr,cube)
      selfscal = .not.(divide .or. keyprsnt('scale'))
      call keyr('scale',factor,1.0)
      call keyr('sigma',sigma,0.0)
      call keyfin
c
c  Check the reasonableness of the inputs.
c
      if (map.eq.' ') call bug('f','Input map missing')
      if (out.eq.' ') call bug('f','Output map missing')
      if (.not.divide) sigma = 0
      if (divide .and. sigma.eq.0)
     *  call bug('f','Sigma must be set, when using options=divide')
      if (final .and. .not.doGaus) call bug('f',
     *  'You cannot set options=final and a beam parameter')
      if (asym .and. doGaus) then
        call bug('w','Gaussians are always symmetric')
        asym = .false.
      endif
      if (.not.asym .and. corr) call bug('w',
     *'Correlation and convolution do not differ for symmetric beams')
      if (final .and. divide) call bug('f',
     *'Cannot use options=final and divide together')
c
c  Open the map and handle the boxes to be processed.
c
      call xyopen(lMap,map,'old',3,nsize)
      nx = nsize(1)
      ny = nsize(2)
      nz = nsize(3)
      call rdhdi(lMap,'naxis',naxis,MAXNAX)
      call rdhdd(lMap,'cdelt1',cdelt1,1d0)
      call rdhdd(lMap,'cdelt2',cdelt2,1d0)
      naxis = min(naxis,MAXNAX)
      dofscale=.false.
      if (cube) then
        call rdhda(lMap,'cellscal',cellscal,'1/F')
        if (nz.gt.1.and.cellscal.eq.'1/F') then
          call coInit(lMap)
          call rdhdr(lMap,'bmaj',bmajmap,0.)
          call rdhdr(lMap,'bmin',bminmap,0.)
          call rdhdr(lMap,'bpa',bpamap,0.)
          dofscale=.true.
        endif
      endif
      call BoxSet(box,3,nsize,' ')
      call BoxInfo(box,3,blc,trc)
      call BoxMask(lMap,box,MAXBOX)
      rect = BoxRect(box)
c
c  Fiddle the gaussian parameters.
c
      doGaus = beam.eq.' '
      if (doGaus) then
        n1 = nx
        n2 = ny
        iref = nx/2 + 1
        jref = ny/2 + 1
        bmaj1 = (bmaj1/3600.0) * D2R
        bmin1 = (bmin1/3600.0) * D2R
        if (bmaj1*bmin1.le.0.0) call bug('f',
     *    'Either a beam or Gaussian must be given')
c
c  Open the beam.
c
      else
        call xyopen(lBeam,beam,'old',2,nsize)
        n1 = nsize(1)
        n2 = nsize(2)
        call rdhdr(lBeam, 'crpix1', crpix1, real(n1/2+1))
        call rdhdr(lBeam, 'crpix2', crpix2, real(n2/2+1))
        iref = nint(crpix1)
        jref = nint(crpix2)
      endif
c
c  Check that the map and beam sizes and deltas are the same.
c
      if (nx.gt.n1 .or. ny.gt.n2)
     *  call bug('f','Map must be smaller than the beam')
c
c  If we are operating in "final" mode, determine the convolving
c  beam.
c
      
      if (final) then
        call GauDPar1(lMap,bmaj1,bmin1,bpa1,
     *                                   bmaj,bmin,bpa,temp,ifail)
        if (ifail.eq.1) call bug('f',
     *    'The input has the required final resolution')
        if (ifail.ne.0) call bug('f',
     *    'The convolving beam is undefined for the final resolution')
        bmaj0 = bmaj1
        bmin0 = bmin1
        bpa0 = bpa1
        bmaj1 = bmaj
        bmin1 = bmin
        bpa1  = bpa

        bmaj = (bmaj*R2D) * 3600.0
        bmin = (bmin*R2D) * 3600.0
        write(text, 10) bmaj, bmin, bpa
 10     format('Convolving Gaussian FWHM:',f5.1,' x',f5.1,
     *          ' arcsec, PA',f6.1,' deg.')
        call output(text)
      endif

      
c
c  Determine the units,etc, of the output, along with any scale factors.
c
      if (selfscal) then
        if (doGaus) then
          call GauPar1(lMap,bmaj1,bmin1,bpa1,
     *                           bunit,bmaj,bmin,bpa,factor)
        else
          call GauPar2(lMap,lBeam,bunit,bmaj,bmin,bpa,factor)
        endif
      else
        call rdhda(lMap,'bunit',bunit,' ')
        bmaj = 0
        bmin = 0
        bpa = 0
      endif
      doscale = abs(factor-1).gt.1e-3
c
c  Calculate the transform of the beam.
c
      l = 0
      flags = ' '
      if (.not.asym) then
        l = l + 1
        flags(l:l) = 's'
      endif
      if (divide) then
        l = l + 1
        flags(l:l) = 'd'
      endif
      if (corr) then
        l = l + 1
        flags(l:l) = 'x'
      endif
      if (doGaus) then
        call CnvlIniG(handle, n1, n2, iref, jref, bmaj1, bmin1, bpa1,
     *                cdelt1, cdelt2, sigma, flags)
      else
        call CnvlIniF(handle,lBeam,n1,n2,iref,jref,sigma,flags)
        call xyclose(lBeam)
      endif
c
c  Open the output, and create its header.
c
      nsize(1) = trc(1) - blc(1) + 1
      nsize(2) = trc(2) - blc(2) + 1
      nsize(3) = trc(3) - blc(3) + 1
      do k = 4, naxis
        nsize(k) = 1
      enddo
      call xyopen(lOut,Out,'new',naxis,nsize)
      call header(lMap,lOut,min(naxis,3),blc,
     *  bunit,bmaj,bmin,bpa,version)
      call MemAllox(pDat,1_8*nsize(1)*nsize(2),'r')
c
c  Loop over the third dimension.
c
      do k = blc(3), trc(3)
        if (mod(k-blc(3),10).eq.0 .and. blc(3).ne.trc(3))
     *    call output('Beginning plane '//itoaf(k))
c
c  Get the run spec. and read in the data.
c
        call BoxRuns(1,k,'r',box,Runs,MAXRUNS,nRuns,
     *                                xmin,xmax,ymin,ymax)
        nx = xmax - xmin + 1
        ny = ymax - ymin + 1
        xoff = xmin - blc(1)
        yoff = ymin - blc(2)

        call xysetpl(lMap,1,k)
        call GetPlane(lMap,Runs,nRuns,xmin-1,ymin-1,nx,ny,
     *                memR(pDat),1_8*nsize(1)*nsize(2),nPoint)
c
c  Do the real work.
c
        if (dofscale) then
          if (bmajmap*bminmap.gt.0.0) then
            x(1) = 0d0
            x(2) = 0d0
            x(3) = 0d0
            call coGauCvt(lMap,'op/op/op',x,'w',bmajmap,bminmap,bpamap,
     *           'p',bmaj2,bmin2,bpa2)
            x(3) = dble(k)
            call coGauCvt(lMap,'op/op/ap',x,'p',bmaj2,bmin2,bpa2,
     *           'w',bmaj1,bmin1,bpa1)
            fscale=bmaj1/bmajmap
          endif
          call GauDfac(bmaj0,bmin0,bpa0,bmaj1,bmin1,bpa1,temp,
     *         bmaj,bmin,bpa,ifail)
          if (ifail.eq.1) call bug('f',
     *      'The input has the required final resolution')
          if (ifail.ne.0) call bug('f',
     *      'The convolving beam is undefined for the final resolution')

          call CnvlIniG(handle, n1, n2, iref, jref, bmaj, bmin, bpa,
     *         cdelt1*fscale, cdelt2*fscale, sigma, flags)
          if (selfscal) then
            call GauPar(bunit,cdelt1*fscale,cdelt2*fscale,bmaj,bmin,bpa,
     *         bunit,cdelt1*fscale,cdelt2*fscale,bmaj1,bmin1,
     *         bpa1,bunit1,bmaj2,bmin2,bpa2,factor)
          endif
        endif
        call CnvlR(handle,memR(pDat),nx,ny,Runs,nRuns,memR(pDat),'c')
c
c  Apply a scale factor, if needed.
c
        if (doscale) call Scale(memR(pDat),nPoint,factor)
c
c  Write out this plane.
c
        call xysetpl(lOut,1,k-blc(3)+1)
        call PutPlane(lOut,Runs,nRuns,xoff,yoff,
     *                        nsize(1),nsize(2),memR(pDat),nPoint)
c
c  Write out a blanking mask, if needed.
c
        if (.not.rect)
     *    call PutRuns(lOut,Runs,nRuns,xoff,yoff,nsize(1),nsize(2))
      enddo
      
      call memFrex(pDat,1_8*nsize(1)*nsize(2),'r')
c
c  All said and done. Close up the files, and leave.
c
      call xyclose(lOut)
      call xyclose(lMap)

      end
c***********************************************************************
      subroutine GetOpt(final,divide,asym,corr,cube)

      logical divide,asym,corr,final,cube

c  Get extra processing options.
c
c  Output:
c    final      Set the final output beam according to the fwhm/pa
c               parameters.
c    divide     True if we are really deconvolving.
c    asym       Is the beam asymmetric?
c    cube       Take beam scaling with freq into account 
c-----------------------------------------------------------------------
      integer nopts
      parameter (nopts=5)
      logical present(nopts)
      character opts(nopts)*10
      data opts/'divide    ','asymmetric','correlate ','final     ',
     *          'cube      '/
c-----------------------------------------------------------------------
      call options('options',opts,present,nopts)
      divide = present(1)
      asym    = present(2)
      corr = present(3)
      final = present(4).or.present(5)
      cube = present(5)
      end
c***********************************************************************
      subroutine Header(lMap, lOut, naxis, blc, bunit, bmaj, bmin, bpa,
     *  version)

      integer   lMap, lOut, naxis, blc(naxis)
      real      bmaj, bmin, bpa
      character bunit*(*), version*72

c  Write the output image header.
c
c  Inputs:
c    lMap       Handle of the input image.
c    lOut       Handle of the output image.
c    naxis      Number of dimensions in the input.
c    blc        Bottom left corner of the input which is going to
c               (1,1,1) of the output.
c    bunit      Units of the output.
c    bmaj,bmin,bpa Effective beam parameters of the output.
c    version    Version of this program.
c-----------------------------------------------------------------------
      integer nkeys
      parameter (nkeys=37)

      integer   iax
      double precision crpix
      character num*1

      character itoaf*2
      external  itoaf
c-----------------------------------------------------------------------
c     Start by making a verbatim copy of the input image header.
      call headcp(lMap, lOut, 0, 0, 0, 0)

c     Update the reference pixels.
      do iax = 1, naxis
        num = itoaf(iax)
        call rdhdd(lMap, 'crpix'//num, crpix, 1d0)
        call wrhdd(lOut, 'crpix'//num, crpix-blc(iax)+1d0)
      enddo

c     Write beam parameters.
      if (bunit.ne.' ') call wrhda(lOut, 'bunit', bunit)
      if (bmaj*bmin.ne.0.0) then
        call wrhdr(lOut, 'bmaj', bmaj)
        call wrhdr(lOut, 'bmin', bmin)
        call wrhdr(lOut, 'bpa',  bpa)
      endif

c     Write history.
      call hisopen(lOut, 'append')
      call hiswrite(lOut, 'CONVOL: Miriad '//version)
      call hisinput(lOut, 'CONVOL')
      call hisclose(lOut)

      end
