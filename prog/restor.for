      program restor

c= restor - Restore clean components to make the CLEAN map
c& rjs mchw
c: deconvolution
c+
c       RESTOR is a MIRIAD task that performs a number of functions
c       typically performed after the deconvolution step.  These include
c       generating a "CLEAN" map, calculating residuals and convolving
c       a model by a Gaussian beam.
c
c       RESTOR can also handle multi-frequency synthesis data.  In this
c       case the input dirty map should consist of just one plane.  The
c       input beam may contain several planes, and the input model may
c       have no more planes (but possibly fewer) than the beam.  To get
c       the residuals, each plane in the model is convolved with the
c       corresponding plane in the beam, and subtracted from the dirty
c       map.
c@ model
c       The model of the deconvolved cube, typically produced by CLEAN
c       or MAXEN.  The units of this image should be Jy/pixel.
c       No default.
c@ beam
c       The input dirty beam.  No default.
c@ map
c       The input dirty cube which should have units of Jy/beam.  May be
c       omitted when mode=convolve.  Otherwise, no default.
c@ mode
c       This can be one of the values:
c         "clean"     This is the normal use, and the default, where the
c                     output is the map, less the model convolved by the
c                     dirty beam, plus the model convolved by the
c                     Gaussian.
c         "residual"  The output is the map, less the model convolved by
c                     the dirty beam.
c         "convolve"  The output is the model convolved by the Gaussian.
c                     The beam is needed only if the Gaussian fwhm and
c                     pa is not specified.  The map is ignored.
c         "dirty"     The output is the map convolved with the beam.
c@ fwhm
c       The size, in arcsec, of the Gaussian beam to use in the
c       restoration.  This will normally be two numbers, giving the
c       full-width at half-maximum of the major and minor axes of the
c       Gaussian.  If only one number is given, the Gaussian will have
c       equal major and minor axes.  If no values are given, they are
c       either retrieved from the beam header, or computed by fitting a
c       Gaussian to the given dirty beam.
c
c       Note that the model image is convolved with this Gaussian beam,
c       and then added to the residuals.  These residuals are not
c       affected by the choice of this Gaussian beam size.  So if you
c       want the residuals and convolved image to have approximately the
c       same beam size, then the Gaussian beam size chosen should be the
c       same size as the dirty beam.  If you want coarser resolution
c       than that provided by this, you should use task CONVOL to smooth
c       the restored image afterwards.
c@ pa
c       The position angle, in degrees, of the Gaussian restoring beam,
c       measured east from north.  Ignored if no value is given for
c       FWHM.  The default is determined from the dirty beam fit.
c@ options
c       There is only a single option at present:
c         "mfs"  If the input model was produced by mfclean and contains 
c                a 2nd plane with alpha*I components, using "mfs" will  
c                cause restor to write a second plane in the output image 
c                containing the alpha*I model convolved with the 
c                Gaussian beam. Linmos will use this plane to do
c                wideband primary beam correction if the bw parameter
c                is used.
c@ out
c       The output restored image.  No default.
c
c$Id: restor.for,v 1.9 2013/02/15 00:20:59 wie017 Exp $
c--
c
c  History:
c    rjs Dark_ages Original version.
c    rjs 16aug89   Some minor formatting enhancements.
c    rjs 22sep89   Protected against the case when cdelt==0.
c    rjs  1mar90   Changed call sequence of nextpow2.
c    mchw 20jun90  Added linetype keywords to output image header.
c    mchw 17jul90  Increased filename lengths. Added version.
c    mchw 05oct90  Option to add sub-image into map.
c    rjs  20mar91  Tolerate multi-plane beams (for mfs).
c    rjs   8apr91  Properly handle multi-freq synthesis. Also use Mem
c                  routines. Various minor improvements.
c    mchw 24apr91  Restored the doc for mode=add.
c    rjs  11jun91  Changed doc slightly.
c    rjs  16sep91  Improved doc slightly. Uses the new cnvl routines.
c    rjs   9oct91  Fixed bug which happens when there is no beam.
c    mjs  13mar92  RESTORE -> RESTOR (name change)
c    mchw 19mar92  Restored pbfwhm to header parameters copied.
c    rjs  22apr92  Doc and output message changes (for Lauren).
c    rjs   4may92  Avoid floating underflows.
c    rjs  26may92  Add btype and rms to list of variables to copy.
c    rjs  25jun92  Use keymatch routine.
c    rjs  17jan94  Make sure beam is writable before writing out
c                  beam parameters.
c    rjs  16nov94  Substantial rework. Use new "restore" routines.
c    rjs   3dec94  Copy mosaic table across, if it exists.
c    rjs  02jul97  Cellscal change.
c    rjs  23jul97  Add pbtype.
c    rjs  25feb98  Honour documentation so that a beam is not needed
c                  when convolving.
c    rjs  28jun01  Doc change only.
c    mchw 07feb02  Change beamwidth format to handle ATA and ALMA.
c    mhw  27oct11  Use ptrdiff type for memory allocations
c    mhw  17jan13  Add mfs option
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      include 'mirconst.h'

      logical   doBeam, doFit, doGaus, doMap, doMap1, mfs
      integer   bmLen(3), cnvLen(2), i, lBeam, lMap, lModel, lOut,
     *          mapLen(4), modLen(3), naxis, offset(3), x0, y0
      ptrdiff   pOut
      real      fwhm(2), pa, rms
      character beam*64, iomode*8, line*72, map*64, mode*16, modl*64,
     *          outNam*64, version*72

      character itoaf*4, versan*80
      external  itoaf, versan
c-----------------------------------------------------------------------
      version = versan('restor',
     *                 '$Revision: 1.9 $',
     *                 '$Date: 2013/02/15 00:20:59 $')

c     Get the input parameters.
      call keyini
      call keya('map',map,' ')
      call keya('beam',beam,' ')
      call keya('model',modl,' ')
      call keya('out',outNam,' ')
      call keyr('fwhm',fwhm(1),0.0)
      call keyr('fwhm',fwhm(2),fwhm(1))
      call keyr('pa',pa,0.0)
      call getMode(mode)
      call getOpt(mfs)
      call keyfin

c     Check the inputs.
      doMap  = mode.eq.'clean' .or. mode.eq.'residual'
      doGaus = mode.eq.'convolve' .or. mode.eq.'clean'
      doFit  = doGaus .and. fwhm(1)*fwhm(2).eq.0.0
      doBeam = doFit .or. mode.ne.'convolve'

      if (Modl.eq.' ') call bug('f','Input model name missing')
      if (outNam.eq.' ') call bug('f','Output map name missing')
      if (beam.eq.' ' .and. doBeam)
     *  call bug('f','Input beam name missing')
      if (map.eq.' ' .and. doMap)
     *  call bug('f','Input map name missing')

c     Convert beam units.
      fwhm(1) = fwhm(1) * AS2R
      fwhm(2) = fwhm(2) * AS2R

c     Open the model and the beam.
      call xyopen(lModel, Modl, 'old', 3, modLen)
      call rdhdi(lModel,'naxis',naxis,3)
      naxis = min(naxis,4)
      call coInit(lModel)

      if (doBeam) then
        call xyopen(lBeam, beam, 'old', 3, bmLen)
        if (doFit) then
          call rdhdr(lBeam,'bmaj',fwhm(1),0.0)
          call rdhdr(lBeam,'bmin',fwhm(2),0.0)
          call rdhdr(lBeam,'bpa',pa,0.0)
          doFit = fwhm(1)*fwhm(2).le.0
        endif
      endif

c     Fit the beam, if necessary.
      if (doFit) then
        call beamFit(lBeam,bmLen,fwhm,pa)
        call hmode(lBeam,iomode)
        if (index(iomode,'w').ne.0) then
          call wrhdr(lBeam, 'bmaj', fwhm(1))
          call wrhdr(lBeam, 'bmin', fwhm(2))
          call wrhdr(lBeam, 'bpa',  pa)
        endif
      endif

c     Keep the user awake with spurious information.
      if (doGaus) then
        write(line,'(a,f9.3,a,f9.3,a)')
     *   'Using Gaussian beam fwhm of',fwhm(1)*R2AS,' by',fwhm(2)*R2AS,
     *   ' arcsec.'
        call output(line)
        write(line,'(a,f6.1,a)')'Position angle: ',pa,' degrees.'
        call output(line)
      endif

c     Reset the beam to a dummy size if we are just convolving with a
c     Gaussian.
      if (mode.eq.'convolve') then
        bmLen(1) = 1
        bmLen(2) = 1
      endif

c     Determine the alignment between the map and the model.  Subtract
c     offset from map pixel coordinates to get model pixel coordinates.
      if (doMap) then
        call xyopen(lMap, map, 'old', 3, mapLen)
        call rdhdr(lMap, 'rms', rms, 0.0)
        call align(lMap, lModel, mapLen, offset)
      else
        rms = 0
        mapLen(1) = modLen(1) + bmLen(1) - 1
        mapLen(2) = modLen(2) + bmLen(2) - 1
        mapLen(3) = modLen(3)
        offset(1) = bmLen(1)/2
        offset(2) = bmLen(2)/2
        offset(3) = 0
      endif
      if (mfs.and.modLen(3).eq.2) then
        mapLen(3)=2
      else
        mfs=.false.
      endif

      if (mapLen(1).le.bmLen(1)) then
        cnvLen(1) = bmLen(1)
      else
        cnvLen(1) = min(mapLen(1),bmLen(1)+modLen(1)-1)
      endif

      if (mapLen(2).le.bmLen(2)) then
        cnvLen(2) = bmLen(2)
      else
        cnvLen(2) = min(mapLen(2),bmLen(2)+modLen(2)-1)
      endif

      x0 = offset(1) - (cnvLen(1)/2 - modLen(1)/2)
      y0 = offset(2) - (cnvLen(2)/2 - modLen(2)/2)

c     Initialise the convolution routines.
      if (mode.eq.'residual') mode = 'dirty'
      if (mode.eq.'convolve') then
        call RestIni(lModel,cnvLen(1),cnvLen(2),fwhm(1),fwhm(2),pa*D2R,
     *    mode)
      else
        call RestIni(lBeam, cnvLen(1),cnvLen(2),fwhm(1),fwhm(2),pa*D2R,
     *    mode)
      endif

c     Open the output, and create its header.
      mapLen(4) = 1
      call xyopen(lOut, outNam, 'new', naxis, mapLen)

      call mkHead(lModel, modLen, lOut, version, doGaus, fwhm, pa, rms,
     *  offset)
      if (doMap) call hdcopy(lMap,lOut,'mostable')

c     Loop over the third dimension of the map.
      call memAllop(pOut,cnvLen(1)*cnvLen(2),'r')
      do i = 1, mapLen(3)
        doMap1 = doMap.and..not.(mfs.and.i.gt.1)
        if (mod(i,10).eq.0 .or. (i.eq.1 .and. mapLen(3).ge.10))
     *    call output('Beginning plane '//itoaf(i))
        call xysetpl(lOut,1,i)
        if (doMap1) call xysetpl(lMap,1,i)
        if (i-offset(3).ge.1 .and. i-offset(3).le.modLen(3)) then
          call Restore(lModel,i-offset(3),memr(pOut))
          if (doMap1) then
            call subModel(lMap,mapLen,memr(pOut),cnvLen,lOut,x0,y0)
          else if (mfs.and.i.gt.1) then
            call copyModel(mapLen,memr(pOut),cnvLen,lOut,x0,y0)
          else
            if (mapLen(1).ne.cnvLen(1) .or. mapLen(2).ne.cnvLen(2))
     *        call bug('f','Algorithmic failure')
            call writeOut(lOut,memr(pOut),mapLen(1),mapLen(2))
          endif
        else
          call copyOut(lMap,lOut,mapLen(1),mapLen(2))
        endif
      enddo

c     All said and done.  Close up the files, and leave.
      call memFrep(pOut,cnvLen(1)*cnvLen(2),'r')
      call restFin
      call xyclose(lModel)
      call xyclose(lOut)
      if (doMap)  call xyclose(lMap)
      if (doBeam) call xyclose(lBeam)

      end

c***********************************************************************

      subroutine getMode(mode)

      character mode*(*)
c-----------------------------------------------------------------------
c  Determine the operation to perform.
c  Output:
c    mode       The operation to be performed.
c-----------------------------------------------------------------------
      integer   NOPT
      parameter (NOPT=4)

      integer   nout
      character opts(NOPT)*8

      data opts/'clean   ','residual','convolve','dirty   '/
c-----------------------------------------------------------------------
      call keymatch('mode',NOPT,opts,1,mode,nout)
      if (nout.eq.0) mode = 'clean'

      end
***************************************************************** getOpt

      subroutine getOpt(mfs)

      logical mfs
c-----------------------------------------------------------------------
c  Get processing options.
c
c  Output:
c    mfs        True if we are to produce I*alpha plane in the output
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=1)
      logical present(NOPTS)
      character opts(NOPTS)*8
      data opts/'mfs     '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)

      mfs  = present(1)

      end

c***********************************************************************

      subroutine align(lMap, lModel, mapLen, offset)

      integer   lMap, lModel, mapLen(3), offset(3)
c-----------------------------------------------------------------------
c  Determine the alignment parameters between the map and the model.
c  This insists that they line up on pixels.
c
c  Input:
c    lMap       Handle of the map file.
c    lModel     Handle of the model file.
c    mapLen     Map axis lengths.
c
c  Output:
c    offset     Offset to subtract from map pixel coordinates to obtain
c               model pixel coordinates.
c-----------------------------------------------------------------------
      integer   iax
      double precision cdeltD, cdeltM, crpixD, crpixM, crvalD, crvalM,
     *          discr, doff
      character cax*1, ctypeD*16, ctypeM*16

c     Externals.
      character itoaf*1
c-----------------------------------------------------------------------
      do iax = 1, 3
        cax = itoaf(iax)
        call rdhdd(lMap,  'crpix'//cax, crpixM, 1d0)
        call rdhdd(lMap,  'cdelt'//cax, cdeltM, 1d0)
        call rdhdd(lMap,  'crval'//cax, crvalM, 0d0)
        call rdhda(lMap,  'ctype'//cax, ctypeM, ' ')

        call rdhdd(lModel,'crpix'//cax, crpixD, 1d0)
        call rdhdd(lModel,'cdelt'//cax, cdeltD, 1d0)
        call rdhdd(lModel,'crval'//cax, crvalD, 0d0)
        call rdhda(lModel,'ctype'//cax, ctypeD, ' ')

c       Check conformance between model and map.
        if (ctypeM.ne.ctypeD)
     *    call bug('f', 'Map and model ctype differ.')

        discr = 1d-2*abs(cdeltM)
        if (abs(cdeltM-cdeltD).ge.discr)
     *    call bug('f', 'Map and model cdelt differ.')

        if (abs(crvalM-crvalD).ge.discr)
     *    call bug('f', 'Map and model crval differ.')

        if (cdeltM.eq.0d0)
     *    call bug('f', 'cdelt on axis '//cax//' is zero')

c       Compute the offset.
        doff = crpixM - crpixD
        offset(iax) = nint(doff)

        if (abs(dble(offset(iax))-doff).gt.1d-2)
     *    call bug('f', 'Map and model do not align on pixels')

        if (offset(iax).lt.0 .or. offset(iax).ge.mapLen(iax))
     *    call bug('f', 'Map and model do not overlap well')
      enddo

      end

c***********************************************************************

      subroutine mkHead(lModel, modLen, lOut, version, doGaus, fwhm,
     *  pa, rms, offset)

      integer   lModel, modLen(2), lOut
      character version*72
      logical   doGaus
      real      fwhm(2), pa, rms
      integer   offset(3)
c-----------------------------------------------------------------------
c  Write header and history for the output map.
c
c  Inputs:
c    fwhm       Beam major and minor axes (radians).
c    pa         Position angle of the Gaussian beam (degrees).
c    rms        Theoretical RMS noise in the image.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   i
      double precision crpix
      character text*72
c-----------------------------------------------------------------------
c     Start with a verbatim copy of the model header.
      call headcp(lModel, lOut, 0, 0, 0, 0)

c     Update keywords that may have changed.
      call rdhdd(lModel, 'crpix1', crpix, dble(modLen(1)/2+1))
      call wrhdd(lOut,   'crpix1', crpix+offset(1))

      call rdhdd(lModel, 'crpix2', crpix, dble(modLen(2)/2+1))
      call wrhdd(lOut,   'crpix2', crpix+offset(2))

      call rdhdd(lModel, 'crpix3', crpix, 1d0)
      call wrhdd(lOut,   'crpix3', crpix+offset(3))

      call rdhda(lModel, 'bunit', text, ' ')
      i = index(text,'/PIXEL')
      if (i.eq.0 .and. text.ne.' ') then
        call bug('w', 'The model units are not /PIXEL')
      else if (i.ne.0) then
        text(i:len(text)) = '/BEAM'
        call wrhda(lOut, 'bunit', text)
      endif

      if (doGaus) then
        call wrhdd(lOut, 'bmaj', dble(fwhm(1)))
        call wrhdd(lOut, 'bmin', dble(fwhm(2)))
        call wrhdd(lOut, 'bpa',  dble(pa))
      endif

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'RESTOR: Miriad ' // version)
      call hisinput(lOut, 'RESTOR')

      if (rms.gt.0) call wrhdr(lOut, 'rms', rms)

      if (doGaus) then
        write(text, 10) fwhm(1)*R2AS, fwhm(2)*R2AS, pa
 10     format('RESTOR: Beam = ',1pe10.3,' x ',1pe10.3,
     *         ' arcsec, pa = ',1pe10.3,' degrees')
        call hiswrite(lOut,text)
      else
        call hiswrite(lOut,'RESTOR: No Gaussian')
      endif

      call hisclose(lOut)

      end

c***********************************************************************

      subroutine writeOut(lOut,Out,mOut,nOut)

      integer lOut,mOut,nOut
      real Out(mOut,nOut)
c-----------------------------------------------------------------------
c  Write out the convolved model to the output file.
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      do j = 1, nOut
        call xywrite(lOut,j,Out(1,j))
      enddo

      end

c***********************************************************************

      subroutine copyOut(lIn,lOut,mOut,nOut)

      integer lIn,lOut,mOut,nOut
c-----------------------------------------------------------------------
c  Copy a map from the input to the output.
c
c  Inputs:
c    lIn        Handle of the input map file.
c    lOut       Handle of the output map file.
c    mOut,nOut  Size of the map.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer j
      real Data(MAXDIM)
c-----------------------------------------------------------------------
      if (mOut.gt.MAXDIM) call bug('f','Output too big')

      do j = 1, nOut
        call xyread(lIn,j,Data)
        call xywrite(lOut,j,Data)
      enddo

      end

c***********************************************************************

      subroutine subModel(lMap,mapLen,Model,modLen,lOut,xoff,yoff)

      integer   lMap, mapLen(2), modLen(2), lOut, xoff, yoff
      real      Model(modLen(1),modLen(2))
c-----------------------------------------------------------------------
c  Subtract the convolved model from the map and write out the result.
c
c  Input:
c    lMap       Handle of the input map.
c    mapLen     Map size.
c    modLen     Model size.
c    Model      Model pixel values.
c    xoff,yoff  Offsets to subtract from a map pixel coordinate to get
c               the corresponding model pixel coordinate.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   i,j,ilo,ihi,jlo,jhi
      real      map(MAXDIM)
c-----------------------------------------------------------------------
c     Range of pixel coordinates in the map for the area that overlaps
c     with the model.
      ilo = max(1, 1+xoff)
      ihi = min(modLen(1)+xoff, mapLen(1))
      jlo = max(1, 1+yoff)
      jhi = min(modLen(2)+yoff, mapLen(2))

c     Read the map row-by-row, subtract the model, and write it out.
      do j = 1, mapLen(2)
        call xyread(lMap, j, map)
        if (j.ge.jlo .and. j.le.jhi) then
          do i = ilo, ihi
            map(i) = map(i) - Model(i-xoff,j-yoff)
          enddo
        endif

        call xywrite(lOut, j, map)
      enddo

      end

c***********************************************************************

      subroutine copyModel(mapLen,Model,modLen,lOut,xoff,yoff)

      integer   mapLen(2), modLen(2), lOut, xoff, yoff
      real      Model(modLen(1),modLen(2))
c-----------------------------------------------------------------------
c  Write the convolved model out as the result.
c
c  Input:
c    mapLen     Map size.
c    modLen     Model size.
c    Model      Model pixel values.
c    xoff,yoff  Offsets to subtract from a map pixel coordinate to get
c               the corresponding model pixel coordinate.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   i,j,ilo,ihi,jlo,jhi
      real      map(MAXDIM)
c-----------------------------------------------------------------------
c     Range of pixel coordinates in the map for the area that overlaps
c     with the model.
      ilo = max(1, 1+xoff)
      ihi = min(modLen(1)+xoff, mapLen(1))
      jlo = max(1, 1+yoff)
      jhi = min(modLen(2)+yoff, mapLen(2))

c     Read the map row-by-row, subtract the model, and write it out.
      do j = 1, mapLen(2)
c        call xyread(lMap, j, map)
        if (j.ge.jlo .and. j.le.jhi) then
          do i = ilo, ihi
            map(i) = Model(i-xoff,j-yoff)
          enddo
        endif

        call xywrite(lOut, j, map)
      enddo

      end

c***********************************************************************

      subroutine beamFit(lBeam, bmLen, fwhm, pa)

      integer   lBeam, bmLen(2)
      real      fwhm(2), pa
c-----------------------------------------------------------------------
c  Fit the beam parameters.
c
c  Input:
c    lBeam      Handle of the beam dataset.
c    bmLen      Size of the beam.
c  Output:
c    fwhm       Beam major and minor axes (radians).
c    bpa        Beam position angle (degrees).
c-----------------------------------------------------------------------
      integer   NP
      parameter (NP=11)

      integer   nPd, xBeam, yBeam
      real      cdelt1, cdelt2, patch(NP*NP)
c-----------------------------------------------------------------------
      nPd = min(NP,bmLen(1),bmLen(2))
      nPd = nPd - mod(nPd+1,2)
      call getPatch(lBeam,bmLen,patch,nPd,xBeam,yBeam)

      call rdhdr(lBeam, 'cdelt1', cdelt1, 0.0)
      call rdhdr(lBeam, 'cdelt2', cdelt2, 0.0)
      if (cdelt1*cdelt2.eq.0.0)
     *  call bug('f','Model pixel increment missing')

      call getFwhm(nPd, xBeam-bmLen(1)/2+nPd/2, yBeam-bmLen(2)/2+nPd/2,
     *             cdelt1, cdelt2, patch, fwhm, Pa)

      end

c***********************************************************************

      subroutine getPatch(lBeam,bmLen,patch,nP,xBeam,yBeam)

      integer   lBeam, bmLen(2), nP, xBeam, yBeam
      real      patch(nP,nP)
c-----------------------------------------------------------------------
c  Get the central portion of the beam and determine the location of the
c  beam maxima (assumed to be near the centre of the beam).
c
c  Inputs:
c    lBeam      Handle of the beam.
c    bmLen      Dimensions of the beam.
c    nP         Size of central patch to return.
c
c  Outputs:
c    xBeam,yBeam Location of beam peak.
c    patch      The central portion of the beam, centred around pixel
c               coordinate (bmLen(1)/2+1,bmLen(2)/2+1).
c
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer i,j,imin,imax,jmin,jmax
      real    Data(MAXDIM)

      integer  ismax
      external ismax
c-----------------------------------------------------------------------
c     Open the beam file and check its size.
      if (max(bmLen(1),bmLen(2)).gt.MAXDIM)
     *  call bug('f','Beam is too big')

      imin = bmLen(1)/2 - nP/2 + 1
      imax = imin + nP - 1
      jmin = bmLen(2)/2 - nP/2 + 1
      jmax = jmin + nP - 1
      if (imin.lt.1 .or. bmLen(1).lt.imax .or.
     *    jmin.lt.1 .or. bmLen(2).lt.jmax)
     *  call bug('f','Beam is too small')

c     Read in the central patch of the beam.
      do j = jmin, jmax
        call xyread(lBeam,j,Data)
        do i = imin, imax
          patch(i-imin+1,j-jmin+1) = Data(i)
        enddo
      enddo

c     Find the maximum, and hopefully it is 1.
      i = ismax(nP*nP,patch,1)
      xBeam = mod(i-1,nP) + 1
      yBeam = (i-1)/nP + 1
      if (abs(1.0-patch(xBeam,yBeam)).gt.0.01)
     *        call bug('w','Beam peak is not 1')
      xBeam = xBeam + imin - 1
      yBeam = yBeam + jmin - 1

      end

c***********************************************************************

      subroutine getFwhm(nP, xBeam, yBeam, cdelt1, cdelt2, beam, fwhm,
     *  pa)

      integer nP, xBeam, yBeam
      real    cdelt1, cdelt2, beam(nP*nP), fwhm(2), pa
c-----------------------------------------------------------------------
c  Get the full width half max parameters.  This calls a routine which
c  finds the least squares fit of the beam patch to a Gaussian.  The
c  result is then converted into more useful units.
c
c  Inputs:
c    np         Dimension of the beam patch.
c    x/yBeam    Location of the centre of the beam.
c    cdelt1/2   Grid increments, in degrees.
c    beam       The central portion of the beam.
c
c  Outputs:
c    fwhm       FWHM, in degrees along the major and minor axes.
c    pa         Position angle, in degrees, measured east of north.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'restor.h'

      integer   MAXITER
      parameter (MAXITER=100)

      integer   i, ifail, j, k
      real      aa(3*3), dfdx(3*nPM*nPM), dx(3), f(nPM*nPM),
     *          fp(nPM*nPM), t1, t2, x(3)

      external derive, func
c-----------------------------------------------------------------------
c     Initialise the arrays ready for the optimisation routine.
      if (nP.gt.nPM) call bug('f','Beam patch too big to handle')
      k = 0
      do j = 1, nP
        do i = 1, nP
          k = k + 1
          sxxc(k) = (i-xBeam)**2
          syyc(k) = (j-yBeam)**2
          sxyc(k) = (i-xBeam)*(j-yBeam)
          patch(k) = beam(k)
        enddo
      enddo

c     Form the initial estimate of the Gaussian beam by using the least
c     squares solution of a "linearised" version of the problem.  This
c     should be robust, though somewhat inaccurate.
      call linEst(nP,xBeam,yBeam,Beam,x)

c     Now perform the fit using a proper least squares routine.
      call nllsqu(3,nP*nP,x,dx,MAXITER,0.0,0.005/3,.true.,ifail,
     *  func,derive,f,fp,dx,dfdx,aa)
      if (ifail.ne.0) call bug('f','Beam fit failed')

c     Convert the results to meaningful units.  The fwhm are in grid
c     units and the pa is in degrees.  Use linear (small-field) approx.
      x(1) = -x(1) / (cdelt1*cdelt1)
      x(2) = -x(2) / (cdelt2*cdelt2)
      x(3) = -x(3) / (cdelt1*cdelt2)

      t1 = x(1) + x(2)
      t2 = sqrt((x(1)-x(2))**2 + x(3)**2)
      fwhm(1) = 0.5 * (t1 - t2)
      fwhm(2) = 0.5 * (t1 + t2)
      fwhm(1) = sqrt(4.0*log(2.0)/fwhm(1))
      fwhm(2) = sqrt(4.0*log(2.0)/fwhm(2))
      if (x(3).ne.0.0) then
        pa = atan2(-x(3),x(1)-x(2)) * R2D / 2.0
      else
        pa = 0.0
      endif

      end

c***********************************************************************

      subroutine linEst(nP,xBeam,yBeam,beam,b)

      integer nP, xBeam, yBeam
      real    beam(nP,nP), b(3)
c-----------------------------------------------------------------------
c  Estimate the parameters for the Gaussian fit using an approximate
c  but linear technique.  This finds values of b that minimise
c
c    SUM{log(beam(x,y)) - b(1)*x*x - b(2)*y*y - b(3)*x*y}**2,
c
c  where the sum is taken over the "main lobe" of the beam only (the
c  "main lobe" is the central part of the beam which is greater than
c  a threshold).  Because this is a linear least squares problem, it
c  should always produce a solution (i.e. no worries about convergence
c  of an iterative fitting process).
c
c  Inputs:
c    nP         Dimension of the beam patch.
c    x/yBeam    Centre pixel of the beam patch.
c    beam       The beam patch.
c
c  Output:
c    b          The estimates of the parameters.
c-----------------------------------------------------------------------
      real      THRESH
      parameter (THRESH=0.1)

      logical   more
      integer   i,j,ilo,ihi,ilod,ihid,ipvt(3),ifail
      real      a(3,3),x,y,z,f
c-----------------------------------------------------------------------
c     Check that centre pixel is within the patch.
      if (xBeam.lt.1 .or. xBeam.gt.nP .or. yBeam.lt.1 .or. yBeam.gt.nP)
     *  call bug('f','Centre pixel of beam is not in beam patch')

c     Determine the pixel range that spans the main lobe at x=0.
      more = .true.
      ihi = xBeam
      do while (ihi.lt.nP .and. more)
        more = beam(ihi+1,yBeam).gt.THRESH
        if (more) ihi = ihi + 1
      enddo
      ilo = xBeam - (ihi-xBeam)

c     Accumulate the info we want over the pixels of the main lobe.
c     For each row, this also keeps track of the range in x which
c     bridges the central lobe.
      do j = 1, 3
        b(j) = 0.0
        do i = 1, 3
          a(i,j) = 0.0
        enddo
      enddo

      j = yBeam
      do while (ilo.le.ihi .and. j.le.nP)
        ilod = nP + 1
        ihid = 0
        do i = max(ilo-1,1),min(ihi+1,nP)
          if (beam(i,j).gt.THRESH) then
            ilod = min(ilod,i)
            ihid = max(ihid,i)
            x = (i-xBeam)**2
            y = (j-yBeam)**2
            z = (i-xBeam)*(j-yBeam)
            f = log(beam(i,j))
            a(1,1) = a(1,1) + x*x
            a(2,1) = a(2,1) + x*y
            a(3,1) = a(3,1) + x*z
            a(2,2) = a(2,2) + y*y
            a(3,2) = a(3,2) + y*z
            a(3,3) = a(3,3) + z*z
            b(1) = b(1) + f*x
            b(2) = b(2) + f*y
            b(3) = b(3) + f*z
          endif
        enddo
        ilo = ilod
        ihi = ihid
        j = j + 1
      enddo

      a(1,2) = a(2,1)
      a(1,3) = a(3,1)
      a(2,3) = a(3,2)

c     Solve the 3x3 system of equations to find the numbers that we
c     really want.  If the matrix is singular, return the estimate as
c     two grid units.
      call sgefa(a,3,3,ipvt,ifail)
      if (ifail.eq.0) then
        call sgesl(a,3,3,ipvt,b,0)
      else
        b(1) = -log(2.0)
        b(2) = -log(2.0)
        b(3) = 0.0
      endif

      end

c***********************************************************************

      subroutine derive(x,dfdx,n,m)

      integer n,m
      real x(n),dfdx(n,m)
c-----------------------------------------------------------------------
      include 'restor.h'

      integer   i
      real      temp
c-----------------------------------------------------------------------
      do i = 1, m
        temp = sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3)
        if (temp.gt.-20.0) then
          temp = exp(temp)
        else
          temp = 0.0
        endif

        dfdx(1,i) = -sxxc(i) * temp
        dfdx(2,i) = -syyc(i) * temp
        dfdx(3,i) = -sxyc(i) * temp
      enddo

      end

c***********************************************************************

      subroutine func(x,f,n,m)

      integer n,m
      real x(n),f(m)
c-----------------------------------------------------------------------
c  Calculate the mismatch function.
c-----------------------------------------------------------------------
      include 'restor.h'

      integer   i
      real      temp
c-----------------------------------------------------------------------
      do i = 1, m
        temp = sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3)
        if (temp.gt.-20.0) then
          f(i) = patch(i) - exp(temp)
        else
          f(i) = patch(i)
        endif
      enddo

      end
