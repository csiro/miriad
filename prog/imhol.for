      program imhol

c= IMHOL - Compute amplitude and phase images from real and imaginary
c& mchw
c: image analysis
c+
c       IMHOL computes amplitude and phase images from real and
c       imaginary holographic images.  The amplitude image can be
c       debiased, and the phase image is computed as
c       atan2(imaginary/real).
c
c@ in
c       Two values; the real and imaginary images, respectively.
c       The imaginary image can be made using INVERT with
c       options=imaginary.  If the (u,v) coordinates for the holography
c       data are in arcsec units then the images have units of nanosec
c       (the inverse of the usual situation for astronomical imaging).
c       Alternatively, the real and imaginary images can be obtained
c       from images of the beam pattern using the MIRIAD task FFT.  The
c       default units for the axes are in wavelengths.  Wild card
c       expansion is supported.
c@ mag
c       Up to two values; the output intensity image and
c       optionally, its associated error image (which will be constant).
c       Default is no output images.
c@ phase
c       Up to two values; the output position angle image and
c       optionally, its associated error image (which will not be
c       constant).  These will be in degrees, radians or microns (see
c       'options').  Default is no output images.
c@ sigma
c       The mean standard deviation of the noise in the images.
c       Required when debiasing or blanking; try to make it as accurate
c       a value as possible.  The default is 0.
c@ sncut
c       This is the S/N ratio below which the output images
c       are blanked (see also options=zero below). It is STRONGLY
c       recommended that SNCUT of at least 2 is used.
c       The default is 0.
c@ pacut
c       The output images are blanked if the error in the phase image
c       (degrees, radians or mircrons depending on OPTIONS) is greater
c       than this value.  This is active even if you don't output the PA
c       image.   Note that there is no equivalent for the output error
c       of the POLI image because the error is constant and equal to
c       SIGMA.  Keyword SNCUT essentially takes care of this.
c       The default is no position angle error based blanking.
c@ options
c       Task enrichment options.  Minimum match is active.
c         bias    If computing polarized intensity, do NOT remove the
c                 Ricean bias in the image.  By default, the bias is
c                 removed to first order with
c                   P = sqrt(P_obs**2 - sigma**2).
c                 You should have a very good reason for using this
c                 option.  See VLA memo no. 161 by Patrick Leahy for
c                 more details of bias removal.
c         zero    When the output pixel is clipped, by setting CLIP(1),
c                 setting OPTIONS=ZERO will cause the output polarized
c                 intensity image (not the position angle image) pixel
c                 to be set to 0.0 rather than being masked out.  This
c                 is very important if you are interested in doing
c                 statistics on a region of low polarized intensity S/N
c                 ratio.  If you leave the region masked rather than
c                 zeroed, you will bias the statistics in that region --
c                 zero is a better estimate of the pixel than just
c                 excluding it from the statistics (provided the clip
c                 level is sufficiently small).  Residual bias in the
c                 statistical results from the area then depend upon how
c                 well the bias remover works and at what level clipping
c                 was performed.  See VLA memo no. 161 by Patrick Leahy.
c         radians Output the phase image in radians instead of degrees.
c         microns Output the phase image as equivalent surface error in
c                 microns.
c         relax   Only warn about image axis descriptor mismatches
c                 instead of giving a fatal error
c         bmfit   Fit focus and pointing offsets to aperture E-field
c                 maps.
c
c$Id: imhol.for,v 1.4 2010/11/22 05:46:53 cal103 Exp $
c--
c  History:
c    nebk 21may92 Original version.
c    nebk 18aug92 Add options=zero and keyword device.
c    nebk 04nov92 Better blanking
c    mjs  12mar93 Use maxnax.h file instead of setting own value.
c    mjs  13mar93 pgplot subr names have less than 7 chars.
c    mchw 09jul93 Added routine to make aperture E-field maps.
c    mchw 09aug93 Renamed IMHOL to keep Neil happy.
c    mchw 09nov93 Fixed a bug in bmproc for planet holography.
c    rjs  11oct95 Rework.
c    rjs  02jul97 cellscal change.
c    mchw 15apr98 Re-use and fix both doc and code in a few places.
c    mchw 05aug04 Change default image axes to wavelengths, and default
c                 antenna size 6.1m.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      logical   bmfit, debias, microns, radians, relax, zero
      integer   axLen(MAXNAX), axLenI(MAXNAX), istkax, lInIm, lOutM(2),
     *          lOutPA(2), lInRe, nOutPA, nOutM, naxis
      real      paclip, sigma, snclip
      character bflag, blstr*7, inIm*64, inRe*64, line*80, outMag(2)*64,
     *          outPA(2)*64, ustr*8, version*72

      character versan*80
      external  versan
c-----------------------------------------------------------------------
      version = versan('imhol',
     *                 '$Revision: 1.4 $',
     *                 '$Date: 2010/11/22 05:46:53 $')

c     Get the inputs.
      call keyini

      call keyf('in', inRe, ' ')
      call keyf('in', inIm, ' ')
      call mkeya('mag',  outMag, 2, nOutM)
      call mkeya('phase', outPA, 2, nOutPA)
      if (inRe.eq.' ' .or. inIm.eq.' ') call bug('f',
     *  'You must specify both real and imaginary input images')

      call getopt(debias, radians, microns, relax, zero, bmfit)

      call keyr('sncut', snclip, 0.0)
      snclip = max(snclip,0.0)
      call keyr('paclip', paclip, 0.0)
      blstr = 'blanked'
      if (zero) blstr = 'zeroed'

      call keyr('sigma', sigma, 0.0)

      bflag = 'f'
      if (relax) bflag = 'w'
      call keyfin

c     Issue some messages if producing an output image.
      write(line, 10) blstr, snclip
10    format('Output ',a,' when P/sigma < ',f6.2)
      call output(line)

      if (paclip.gt.0.0) then
        ustr = ' degrees'
        if (radians) ustr = ' radians'
        write(line, 30) blstr, paclip, ustr
30      format('Output images ',a,' when sigma(P.A.) > ',1pe10.4,a)
        call output(line)

        if ((snclip.gt.0.0 .or. paclip.gt.0.0 .or. debias) .and.
     *       sigma.le.0.0)
     *    call bug('f', 'You must specify sigma')

        if (nOutM.gt.0) then
          if (debias) then
            call output('The polarized intensity image '//
     *                  'will be debiased')
          else
            call bug('w', 'You are NOT debiasing the intensity image')
            if (snclip.lt.2.0) call bug('w',
     *        'Nor have you safely blanked the image with SNCUT > 2')
          endif
        endif
      endif

      if (nOutM.eq.0 .and. nOutPA.eq.0) then
        call bug('f', 'No output was specified.')
      endif


c     Open the input images.
      call xyopen(lInRe, inRe, 'old', MAXNAX, axLen)
      if (axLen(1).gt.MAXDIM) call bug('f',
     *  'First axis of real image exceeds storage.')

      call xyopen(lInIm, inIm, 'old', MAXNAX, axLenI)
      if (axLenI(1).gt.MAXDIM) call bug('f',
     *  'First axis of imaginary image exceeds storage.')

c     Check them for consistency.
      call chkdes(lInRe, lInIm, inRe, inIm, axLen, axLenI, bflag,
     *            istkax)

c     Open polarized intensity and position angle images as required.
      call rdhdi(lInRe, 'naxis', naxis, 0)
      naxis = min(naxis,MAXNAX)

      lOutM(1) = 0
      if (nOutM.gt.0) then
        call xyopen(lOutM(1), outMag(1), 'new', naxis, axLen)
        call mkHead(lInRe, lOutM(1), istkax, version)
      endif

      lOutM(2) = 0
      if (nOutM.eq.2) then
        call xyopen(lOutM(2), outMag(2), 'new', naxis, axLen)
        call mkHead(lInRe, lOutM(2), istkax, version)
      endif

      lOutPA(2) = 0
      if (nOutPA.gt.0) then
        call xyopen(lOutPA(1), outPA(1), 'new', naxis, axLen)
        call mkHead(lInRe, lOutPA(1), istkax, version)
      endif

      lOutPA(2) = 0
      if (nOutPA.eq.2) then
        call xyopen(lOutPA(2), outPA(2), 'new', naxis, axLen)
        call mkHead(lInRe, lOutPA(2), istkax, version)
      endif

c     Now compute and write out the output image(s).
       call bmproc(lInRe, lInIm, lOutM, lOutPA, naxis, axLen, debias,
     *   radians, microns, snclip, paclip, sigma, zero, bmfit)

c     Close up.
      call xyclose(lInRe)
      call xyclose(lInIm)
      if (lOutM(1).ne.0) call xyclose(lOutM(1))
      if (lOutM(2).ne.0) call xyclose(lOutM(2))
      if (lOutPA(1).ne.0) call xyclose(lOutPA(1))
      if (lOutPA(2).ne.0) call xyclose(lOutPA(2))

      end

c***********************************************************************

      subroutine getopt (debias, radians, microns, relax, zero, bmfit)

      logical debias, radians, relax, zero, bmfit, microns
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c     debias    Debias the polarized intensity image
c     radians   Output phase image is in radians
c     microns   Output "phase" image is in microns.
c     relax     Warnings only for axis descriptor mismatches
c     zero      Output zeros rather than setting flagging mask
c     bmfit     Fit focus and pointing offsets to aperture E-field maps.
c-----------------------------------------------------------------------
      integer MAXOPT
      parameter (MAXOPT = 6)

      character opshuns(MAXOPT)*8
      logical present(MAXOPT)
      data opshuns /'bias    ', 'radians ', 'microns ',
     *              'relax   ', 'zero    ', 'bmfit   '/
c-----------------------------------------------------------------------
      call options('options', opshuns, present, MAXOPT)

      debias  = .not.present(1)
      radians = present(2)
      microns = present(3)
      if (microns .and. radians) call bug('f',
     *  'Cannot use options=microns and radians together')
      relax   = present(4)
      zero    = present(5)
      bmfit   = present(6)

      end

c***********************************************************************

      subroutine chkdes(lIn1, lIn2, in1, in2, axLen1, axLen2, bflag,
     *                  istkax)

      integer   lIn1, lIn2, axLen1(*), axLen2(*), istkax
      character bflag*1, in1*(*), in2*(*)
c-----------------------------------------------------------------------
c  Compare axis descriptors.
c
c  Input:
c   lIn1,2       Image handles
c   in1,2        Images
c   axLen1,2     Sizes of each dimension
c   bflag        Bug handling flag, 'f' or 'w'.
c-----------------------------------------------------------------------
      integer   iax, l1, l2, naxis1, naxis2
      double precision dval1, dval2
      character cax*1, ctype1*9, ctype2*9, text*130

      integer   len1
      character itoaf*1
      external  itoaf, len1
c-----------------------------------------------------------------------
      l1 = len1(in1)
      l2 = len1(in2)

      call rdhdi(lIn1, 'naxis', naxis1, 0)
      if (naxis1.eq.0) then
        text = in1(:l1) // ' has zero dimensions!'
        call bug('f', text)
      endif

      call rdhdi(lIn2, 'naxis', naxis2, 0)
      if (naxis2.eq.0) then
        text = in2(:l2) // ' has zero dimensions!'
        call bug('f', text)
      endif

      if (naxis1.ne.naxis2) then
        text = 'Unequal number dimensions for images '//
     *         in1(:l1)//' & '//in2(:l2)
        call bug(bflag, text)
      endif

      istkax = 0
      do iax = 1, min(naxis1,naxis2)
        if (axLen1(iax).ne.axLen2(iax)) then
          write(text, 10) in1(1:l1), in2(1:l2), iax
10        format('Unequal axLens for images ',a,' & ',a,' on axis ',i1)
          call bug(bflag, text)
        endif

        cax = itoaf(iax)

        call rdhda(lIn1, 'ctype'//cax, ctype1, ' ')
        call rdhda(lIn2, 'ctype'//cax, ctype2, ' ')
        if (ctype1.eq.'STOKES' .and. ctype2.eq.'STOKES') then
          istkax = iax
        else
          if (ctype1.ne.ctype2) then
            write(text, 20) 'ctype', in1(:l1), in2(:l2), iax
20          format('Unequal ',a,' for images ',a,' & ',a,' on axis ',i1)
            call bug(bflag, text)
          endif

          call rdhdd(lIn1, 'crpix'//cax, dval1, dble(axLen1(iax))/2d0)
          call rdhdd(lIn2, 'crpix'//cax, dval2, dble(axLen2(iax))/2d0)
          if (abs(dval1-dval2).gt.0.01*max(abs(dval1),abs(dval2))) then
            write(text, 20) 'crpix', in1(:l1), in2(:l2), iax
            call bug(bflag, text)
          endif

          call rdhdd(lIn1, 'cdelt'//cax, dval1, 1d0)
          call rdhdd(lIn2, 'cdelt'//cax, dval2, 1d0)
          if (abs(dval1-dval2).gt.0.01*max(abs(dval1),abs(dval2))) then
            write(text, 20) 'cdelt', in1(:l1), in2(:l2), iax
            call bug(bflag, text)
          endif

          call rdhdd(lIn1, 'crval'//cax, dval1, 0d0)
          call rdhdd(lIn2, 'crval'//cax, dval2, 0d0)
          if (abs(dval1-dval2).gt.0.01*max(abs(dval1),abs(dval2))) then
            write(text, 20) 'crval', in1(:l1), in2(:l2), iax
            call bug(bflag, text)
          endif
        endif
      enddo

      call rdhdr(lIn1, 'epoch', dval1, 0.0)
      call rdhdr(lIn2, 'epoch', dval2, 0.0)
      if (dval1.ne.dval2) then
        text = 'Unequal epochs for images '//in1(:l1)//' & '//in2(:l2)
        call bug(bflag, text)
      endif

      end

c***********************************************************************

      subroutine mkHead(lIn, lOut, istkax, version)

      integer   lIn, lOut, istkax
      character version*72
c-----------------------------------------------------------------------
c  Write a header for the output file.
c
c  Input
c    lIn        Handle of input image.
c    lOut       Handle of output image.
c    istkax     Stokes axis (excluded).
c    version    Version of this program.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   axMap(MAXNAX), iax, jax, naxis
c-----------------------------------------------------------------------
c     Exclude the Stokes axis.
      call rdhdi(lIn, 'naxis', naxis, 0)
      naxis = min(naxis,MAXNAX)

      jax = 0
      do iax = 1, naxis
        if (iax.ne.istkax) then
          jax = jax + 1
          axmap(jax) = iax
        endif
      enddo

      call headcp(lIn, lOut, naxis, axMap, 0, 0)

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'IMHOL Miriad ' // version)
      call hisinput(lOut, 'IMHOL')
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine bmproc(lInRe, lInIm, lOutM, lOutPA, naxis, axLen,
     *   debias, radians, microns, snclip, paclip, sigma, zero,bmfit)

      integer lInRe, lInIm, lOutM(2), lOutPA(2), naxis, axLen(naxis)
      real    snclip, paclip, sigma
      logical radians, debias, zero, bmfit, microns
c-----------------------------------------------------------------------
c  Compute aperture E-field amplitude, position angle image and
c  associated error images.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      logical   epaflags(MAXDIM), epflags(MAXDIM), ok, paflags(MAXDIM),
     *          pass1, pflags(MAXDIM), flagRe(MAXDIM), flagIm(MAXDIM)
      integer   frqax, iax, jax, kax, lOut
      real      epaline(MAXDIM), epline(MAXDIM), p, paline(MAXDIM),
     *          pline(MAXDIM), psq, lineRe(MAXDIM), rms, rmsw, sigsq,
     *          snclipsq, snr, lineIm(MAXDIM)
      double precision a, antdiam, antr2, b, c, crpix(3), cdelt(3),
     *          crval(3), d, dd, det, fac, fitph, freq, lambda, r2,
     *          subdiam, subr2, sum, sumr2, sumr4, sumw, sumwz, sumwzz,
     *          sumxx, sumyy, sumz, sumzr2, sumzx, sumzy, sumzz, x, y
      character cax*1, ctype*9, text*80, telescop*10, ustr*8

      character itoaf*1
      external  itoaf
c-----------------------------------------------------------------------
c     Get dish and subreflector diameters (m) for masking.
      call rdhda(lInRe,'telescop',telescop,' ')

      call obspar(telescop,'antdiam',antdiam,ok)
      if (.not.ok) then
        call output('Unknown antenna diameter; setting to 6.1m.')
        antdiam = 6.1
      endif

      call obspar(telescop,'subdiam',subdiam,ok)
      if (.not.ok) then
        call output('Unknown subreflector diameter; setting to 0.')
        subdiam = 0.0
      endif

c     Get coordinate keywords and find the frequency axis.
      lOut = lOutM(1)
      if (lOut.eq.0) lOut = lOutM(2)
      if (lOut.eq.0) lOut = lOutPA(1)
      if (lOut.eq.0) lOut = lOutPA(2)

      frqax = 0
      do iax = 1, naxis
        if (iax.le.3) then
          cax = itoaf(iax)
          call rdhdd(lOut, 'crpix'//cax, crpix(iax), 0d0)
          call rdhdd(lOut, 'cdelt'//cax, cdelt(iax), 1d0)
          call rdhdd(lOut, 'crval'//cax, crval(iax), 0d0)
        endif

        call rdhda(lOut, 'ctype'//cax, ctype, ' ')
        if (index(ctype,'FREQ').ne.0) frqax = iax
      enddo

      if (frqax.eq.0) then
        call bug('w', 'Could not find frequency axis.')
      else if (frqax.le.2) then
        call bug('f',
     *    'Frequency axis is either 1 or 2; these should be spatial.')
      endif


c     Make amplitude and phase images for each plane (frequency axis).
      do kax = 1, axLen(3)
        call xysetpl(lInRe, 1, kax)
        call xysetpl(lInIm, 1, kax)
        if (lOutM(1).ne.0)  call xysetpl(lOutM(1),  1, kax)
        if (lOutM(2).ne.0)  call xysetpl(lOutM(2),  1, kax)
        if (lOutPA(1).ne.0) call xysetpl(lOutPA(1), 1, kax)
        if (lOutPA(2).ne.0) call xysetpl(lOutPA(2), 1, kax)

        if (radians) then
          fac  = 1.0
          ustr = 'radians'
        else
          fac  = DR2D
          ustr = 'degrees'
        endif

        if (frqax.eq.3) then
c         Get frequency in Hz.
          freq = ((dble(kax)-crpix(3))*cdelt(3) + crval(3))*1d9

c         Convert diameters to wavelengths.
          lambda  = DCMKS / freq
          antdiam = antdiam / lambda
          subdiam = subdiam / lambda

          if (microns) then
c           Convert output phase to micron.
            fac  = (0.5d0/DTWOPI)*lambda*1d6
            ustr = 'microns'
          endif
        else
c         Convert diameters to nanosec.
          antdiam = (antdiam/DCMKS)*1d9
          subdiam = (subdiam/DCMKS)*1d9
        endif

c       Antenna and subreflector radii squared.
        antr2 = (antdiam/2d0)**2
        subr2 = (subdiam/2d0)**2

        sigsq    = sigma * sigma
        snclipsq = snclip * snclip
        paclip   = fac * paclip

c       If(bmfit) then go thro' this loop twice: 1st pass to accumulate
c       the sums and 2nd pass to correct the data.
        pass1 = .true.
100     continue
        sum    = 0d0
        sumz   = 0d0
        sumzz  = 0d0
        sumw   = 0d0
        sumwz  = 0d0
        sumwzz = 0d0
        sumxx  = 0d0
        sumyy  = 0d0
        sumr2  = 0d0
        sumr4  = 0d0
        sumzx  = 0d0
        sumzy  = 0d0
        sumzr2 = 0d0

        do jax = 1, axLen(2)
          call xyread (lInRe, jax, lineRe)
          call xyflgrd(lInRe, jax, flagRe)
          call xyread (lInIm, jax, lineIm)
          call xyflgrd(lInIm, jax, flagIm)

c         Work out everything, but only write out what is requested.
          do iax = 1, axLen(1)
            psq = lineRe(iax)**2 + lineIm(iax)**2
            snr = 1.0
            if (snclip.gt.0.0) snr = psq / sigsq

            call allblnk(pline(iax), pflags(iax), epline(iax),
     *         epflags(iax), paline(iax), paflags(iax), epaline(iax),
     *         epaflags(iax))
            if (zero) pflags(iax) = .true.

            if ((lineIm(iax).eq.0.0 .and. lineRe(iax).eq.0.0) .or.
     *          (.not.flagRe(iax) .or. .not.flagIm(iax))) then
c             Undefined, so don't allow the "zero" blanking option.
              pflags(iax) = .false.

            else if (snr.gt.snclipsq) then
c             Passed the S/N ratio criterion; work out amp and phase.
              pline(iax)   = sqrt(psq)
              epline(iax)  = sigma
              pflags(iax)  = .true.
              epflags(iax) = .true.

              paline(iax)   = fac * atan2(lineIm(iax),lineRe(iax))
              epaline(iax)  = fac * sigma / sqrt(psq)
              paflags(iax)  = .true.
              epaflags(iax) = .true.

              if (paclip.gt.0.0 .and. epaline(iax).gt.paclip) then
c               Failed the phase error blanking test.  Don't allow
c               "zero" blanking here.  Blank both amplitude and phase.
                call allblnk(pline(iax), pflags(iax), epline(iax),
     *            epflags(iax), paline(iax), paflags(iax), epaline(iax),
     *            epaflags(iax))

              else
c               Debias intensity if required.
                if (debias) then
                  p = psq - sigsq
                  if (p.gt.0.0) then
                    pline(iax) = sqrt(p)
                  else

c                   Blank amp and phase if we can't debias amp.
                    call allblnk(pline(iax), pflags(iax), epline(iax),
     *                epflags(iax), paline(iax), paflags(iax),
     *                epaline(iax), epaflags(iax))

c                     Zero is a reasonable estimate for this pixel so if
c                     requested, leave the flag mask at good for the
c                     amplitude image.
                    if (zero) pflags(iax) = .true.
                  endif
                endif
              endif
            endif

c           Fit focus and pointing offsets to aperture E-field maps.
c           Fit linear and quadratic terms to phase across aperture
c           phase(x,y)=a+bx+cy+d(x*x+y*y).
            x  = (iax-crpix(1))*cdelt(1)
            y  = (jax-crpix(2))*cdelt(2)
            r2 = (x*x+y*y)

c           Mask amp and phase outside illuminated aperture surface.
            if (r2.lt.subr2 .or. antr2.lt.r2) then
              pflags(iax)  = .false.
              paflags(iax) = .false.
            endif

c           Accumulate the sums.
            if (pass1 .and. paflags(iax)) then
              sum    = sum    + 1.0
              sumz   = sumz   + paline(iax)
              sumzz  = sumzz  + paline(iax)*paline(iax)
              sumxx  = sumxx  + x*x
              sumyy  = sumyy  + y*y
              sumr2  = sumr2  + r2
              sumr4  = sumr4  + r2*r2
              sumzx  = sumzx  + paline(iax)*x
              sumzy  = sumzy  + paline(iax)*y
              sumzr2 = sumzr2 + paline(iax)*r2
            else if (bmfit .and. .not.pass1) then
              fitph  = a + b*x + c*y + d*r2
              paline(iax) = paline(iax) - fitph
              if (paflags(iax)) then
                sum   = sum    + 1.0
                sumz  = sumz   + paline(iax)
                sumzz = sumzz  + paline(iax)*paline(iax)
              endif
              if (pflags(iax)) then
                sumw   = sumw    + pline(iax)
                sumwz  = sumwz   + pline(iax)*paline(iax)
                sumwzz = sumwzz  + pline(iax)*paline(iax)*paline(iax)
              endif
            endif
          enddo

c         Write out the images.
          if (bmfit .and. .not.pass1 .or. pass1 .and. .not.bmfit) then
            if (lOutM(1).ne.0) then
              call xywrite(lOutM(1), jax, pline)
              call xyflgwr(lOutM(1), jax, pflags)
            endif
            if (lOutM(2).ne.0) then
              call xywrite(lOutM(2), jax, epline)
              call xyflgwr(lOutM(2), jax, epflags)
            endif

            if (lOutPA(1).ne.0) then
              call xywrite(lOutPA(1), jax, paline)
              call xyflgwr(lOutPA(1), jax, paflags)
            endif
            if (lOutPA(2).ne.0) then
              call xywrite(lOutPA(2), jax, epaline)
              call xyflgwr(lOutPA(2), jax, epaflags)
            endif
          endif

c         Get next image row
        enddo

c       Sumarize results of focus and pointing fits.
        if (pass1) then
          write(text,'(a,i8)')
     *      'Number of points in phase fit =',int(sum)
          call output(text)
          b   = sumzx/sumxx
          c   = sumzy/sumyy
          det = sumr2*sumr2 - sum*sumr4

          if (abs(det).gt.1d-10) then
            dd = (sumz*sumr2-sumzr2*sum)/det
            d  = dd
            a  = (sumz-dd*sumr2)/sum
          else
            call output('Not fitting focus')
            a  = sumz/sum
            d  = 0.0
          endif

          call output(
     *      'Fit linear and quadratic terms to phase across aperture')
          call output('Phase(x,y) = A + Bx + Cy + D(x*x+y*y) '//ustr)
          call output('Results of Phase Fit are:')
          write(text,'(a,4(g12.5,3x))') 'A,B,C,D = ',A,B,C,D
          call output(text)
        endif

c       Compute the surface rms.
        if (sum.gt.0.0) then
          rms = sqrt(sumzz/sum - (sumz/sum)**2)
          if (pass1) then
            write(text,'(a,g12.5,1x,a)')
     *           'Surface rms before fit = ',rms, ustr
          else
            write(text,'(a,g12.5,1x,a)')
     *           'Surface rms after fit = ',rms, ustr
          endif
          call output(text)
        endif

c       Compute the amplitude weighted surface rms.
        if (sumw.gt.0.0) then
          rmsw = sqrt(sumwzz/sumw - (sumwz/sumw)**2)
          if (.not.pass1) then
            write(text,'(a,g12.5,1x,a)')
     *        'Amplitude weighted surface rms after fit = ',
     *          rmsw, ustr
            call output(text)
          endif
        endif

c       Convert the fits to sensible units.  B and C have units of 1/fac
c       radians per wavelength.  Convert to arcsecs.
        if (frqax.eq.3) then
c         Get frequency in Hz, wavelength in metre.
          freq = ((dble(kax)-crpix(3))*cdelt(3) + crval(3))*1d9
          lambda = DCMKS/freq

          write(text,'(a,2g12.5,a)') 'Pointing offset in az,el = ',
     *      b/fac/DTWOPI*lambda*DR2AS,
     *      c/fac/DTWOPI*lambda*DR2AS, ' arcsecs'
          call output(text)

          if (pass1) then
            write(text,'(a,g12.5,a)') 'Surface rms before fit = ',
     *        0.5d0*rms/fac/DTWOPI*lambda*1d6, ' microns'
            call output(text)
          else
            write(text,'(a,g12.5,a)') 'Surface rms after fit = ',
     *        0.5d0*rms/fac/DTWOPI*lambda*1d6, ' microns'
            call output(text)
            write(text,'(a,g12.5,a)')
     *        'Amplitude weighted surface rms after fit = ',
     *        0.5d0*rmsw/fac/DTWOPI*lambda*1d6, ' microns'
            call output(text)
          endif
        endif

        if (pass1 .and. bmfit) then
          call output('  ')
          call output('Applying the fit to the output images')
          pass1 = .false.
          goto 100
        endif

c       Get next image plane.
      enddo

c     Write header info.
      call lcase(ustr)
      if (lOutPA(1).ne.0) call wrhda(lOutPA(1), 'bunit', ustr)
      if (lOutPA(2).ne.0) call wrhda(lOutPA(2), 'bunit', ustr)

      end

c***********************************************************************

      subroutine allblnk(p, pf, ep, epf, pa, paf, epa, epaf)
c-----------------------------------------------------------------------
      real p, ep, pa, epa
      logical pf, epf, paf, epaf
c-----------------------------------------------------------------------
      p    = 0.0
      pf   = .false.
      ep   = 0.0
      epf  = .false.
      pa   = 0.0
      paf  = .false.
      epa  = 0.0
      epaf = .false.

      end
