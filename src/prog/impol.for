      program impol

c= IMPOL - Compute polarized intensity and position angle from Q and U
c& nebk
c: image analysis
c+
c       IMPOL computes the total linearly polarized intensity
c       (optionally debiasing it) and position angle images from
c       Stokes Q and U images.  Position angle is positive N -> E.
c@ in
c       Up to three values; the Q, U and I images, respectively.
c       The I image is only needed if you want to compute the fractional
c       polarization image as well or if you want to blank the output
c       based upon an I S/N ratio.  Wild card expansion is supported.
c@ poli
c       Up to two values; the output polarized intensity image and
c       optionally, its associated error image (which will be constant).
c       Default is no output images.
c@ polm
c       Up to two values; the output fractional polarization image and
c       optionally, its associated error image.  You need to input an I
c       image to keyword "in" for this.
c@ pa
c       Up to two values; the output position angle image and
c       optionally, its associated error image (which will not be
c       constant).  These will be in degrees (but see OPTIONS=RADIANS),
c       Default is no output images.
c@ sigma
c       Up to two values; the mean standard deviation of the noise in
c       the Q & U images (i.e. one number for them both),  and the
c       standard deviation of the I image.
c
c       These are required for debiasing (Q,U only), or for generating
c       output error images, or for blanking the output.  Try to make
c       the Q,U value as accurate as possible for the debiasing.
c       Perhaps measure it from a V image
c       No default for sigma_QU, sigma_I defaults to sigma_QU
c@ sncut
c       Up to 2 values.  The first is the S/N ratio, P/SIGMA_QU, below
c       which the output images are blanked (see also options=zero
c       below).  It is generally recommended that an SNCUT of at least 2
c       is used.  The second value, which is only valid when you have
c       input an I image and sigma, is the S/N ratio, I/SIGMA_I, below
c       which output images are blanked (defaults to no I based
c       blanking) The default is 2.0 and 0.0.
c@ pacut
c       The output images are blanked if the error in the position
c       angle image (degrees or radians depending on OPTIONS) is greater
c       than this value.  This is active even if you don't output
c       the PA image.   Note that there is no equivalent for the output
c       error of the POLI image because the error is constant and
c       equal to SIGMA.  Keyword SNCUT essentially takes care of this.
c       The default is no position angle error based blanking.
c@ rm
c       After computing the position angle image, rotate the position
c       angles back to zero wavelength by an amount specified by
c       RM (rad/m**2).   Better to use IMRM to generate the rotation
c       measure and zero wavelength position angle images.
c       Default is 0.0
c@ options
c       Task enrichment options.  Minimum match is active.
c
c       "bias"     If computing polarized intensity, do NOT remove the
c                  Ricean bias in the image.  By default, the bias is
c                  removed to first order with P = sqrt(P_obs**2 -
c                  sigma**2).  You should have a very good reason for
c                  using this option (e.g. a detection experiment).  See
c                  VLA memo no. 161 by Patrick Leahy for more details of
c                  bias removal.
c
c       "zero"     When the output pixel is clipped because the
c                  debiasing fails (P**2 may become negative), setting
c                  OPTIONS=ZERO will cause the output polarized
c                  intensity image (not the position angle image) pixel
c                  to be set to 0.0 rather than being masked out.  This
c                  is very important if you are interested in doing
c                  statistics on a region of low polarized intensity S/N
c                  ratio.  If you leave the region masked rather than
c                  zeroed, you will bias the statistics in that region -
c                  zero is a better estimate of the pixel than just
c                  excluding it from the statistics (provided the clip
c                  level is sufficiently small). Residual bias in the
c                  statistical results from the area then depend upon
c                  how well the bias remover works and at what level
c                  clipping was performed.  See VLA memo no. 161 by
c                  Patrick Leahy.
c
c       "radians"  Output the position angle image in radians instead
c                  of degrees.
c
c       "relax"    Only warn about image axis descriptor mismatches
c                  instead of giving a fatal error
c@device
c       PGPLOT device on which to draw a plot showing the effect of bias
c       in polarized intensity images.  It plots true polarized
c       intensity versus the bias, which is the estimated polarized
c       intensity minus the true polarized intensity.  Three estimators
c       are shown; observed, first order, and maximum likelhood.  It is
c       assumed that sigma_P = 1  in these plots.  Because these plots
c       are drawn following a Monte Carlo simulation of some 15,000
c       trials of the noise, you will need to be patient.  You can just
c       make this bias plot without actually working on any data if you
c       wish. See also VLA memo no. 161 by Patrick Leahy.
c       Default is no plot.
c
c$Id: impol.for,v 1.20 2011/11/03 04:42:13 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer   I, Q, U
      parameter (Q = 1, U = 2, I = 3)

      logical   debias, doimage, emflags(MAXDIM), epaflags(MAXDIM),
     *          epflags(MAXDIM), iflags(MAXDIM), mflags(MAXDIM),
     *          paflags(MAXDIM), pflags(MAXDIM), qflags(MAXDIM),
     *          radians, relax, uflags(MAXDIM), zero
      integer   axmap(MAXNAX), naxes(3), naxis(MAXNAX,3), stkax(3),
     *          lmout(2), lpaout(2), lpout(2), tIn(3), nin, nmout,
     *          npaout, npout
      real      emline(MAXDIM), epaline(MAXDIM), epline(MAXDIM),
     *          epoch(3), iline(MAXDIM), mline(MAXDIM), paclip,
     *          paline(MAXDIM), pline(MAXDIM), qline(MAXDIM),
     *          rm, sigmai, sigmaqu, snclip(2), uline(MAXDIM)
      double precision cdelt(MAXNAX,3), crpix(MAXNAX,3), crval(MAXNAX,3)
      character bflag, blstr*7, device*80, ctype(MAXNAX,3)*9, in(3)*64,
     *          ins(3)*64, line*80, mout(2)*64, paout(2)*64, pout(2)*64,
     *          ustr*8, version*72

      external  len1, versan
      integer   len1
      character versan*72

      data tIn, lpout, lmout, lpaout /3*0, 2*0, 2*0, 2*0/
c-----------------------------------------------------------------------
      version = versan('impol',
     *                 '$Revision: 1.20 $',
     *                 '$Date: 2011/11/03 04:42:13 $')

c     Get user inputs.
      call keyini
      call mkeyf('in', ins, 3, nin)
      call mkeya('poli', pout, 2, npout)
      call mkeya('polm', mout, 2, nmout)
      call mkeya('pa', paout, 2, npaout)
      call getopt(debias, radians, relax, zero)
      call keyr('sncut', snclip(1), 2.0)
      call keyr('sncut', snclip(2), 0.0)
      call keyr('pacut', paclip, 0.0)
      call keyr('sigma', sigmaqu, 0.0)
      call keyr('sigma', sigmai, sigmaqu)
      call keyr('rm', rm, 0.0)
      call keya('device', device, ' ')
      call keyfin

c     Process the inputs.
      doimage = .true.
      if (device.eq.' ') then
        if (nin.eq.0) call bug('f', 'Nothing to do')
      else
        if (nin.eq.0) doimage = .false.
      endif

      if (doimage .and. nin.lt.2)
     *  call bug('f', 'Not enough input images')
      in(Q) = ins(1)
      in(U) = ins(2)
      in(I) = ' '
      if (nin.eq.3) in(I) = ins(3)

      if (doimage .and. npout.eq.0 .and. npaout.eq.0 .and. nmout.eq.0)
     *  call bug('f', 'You must specify an output image')

      snclip(1) = max(snclip(1),0.0)
      snclip(2) = max(snclip(2),0.0)
      blstr = 'blanked'
      if (zero) blstr = 'zeroed'
      if (npaout.eq.0) rm = 0.0

      bflag = 'f'
      if (relax) bflag = 'w'

      sigmaqu = abs(sigmaqu)
      sigmai  = abs(sigmai)

c     Issue some messages if producing an output image.
      if (doimage) then
        write(line, 10) blstr, snclip(1)
10      format('Output ', a, ' when     P/sigma < ', f6.2)
        call output(line)

        if (paclip.gt.0.0) then
          ustr = ' degrees'
          if (radians) ustr = ' radians'
          write(line, 30) blstr, paclip, ustr
30        format('Output images ', a, ' when sigma(P.A.) > ',
     *            1pe10.4, a)
          call output(line)
        else
          paclip = 0.0
        endif

        if (snclip(1).lt.2.0) call bug('w', 'Interpreting polarized '
     *    //'images below P/SIG=2 can be hazardous')
        if ((snclip(1).gt.0.0 .or. paclip.gt.0.0 .or. debias) .and.
     *       sigmaqu.le.0.0)
     *     call bug('f', 'You must specify sigma')

        if (npout.gt.0) then
          if (debias) then
            call output('The polarized intensity image '//
     *                   'will be debiased')
          else
            call bug('w',
     *      'The polarized intensity image will not be debiased')
            if (snclip(1).lt.2.0) call bug('w',
     *   'The polarized intensity image will not be blanked with SNCUT')
          endif
        endif
      endif

c     Open the input images.
      if (doimage) then
c       Stokes I.
        if (in(I).ne.' ') then
          call openin(bflag, MAXDIM, MAXNAX, in(I), tIn(I), naxes(I),
     *      naxis(1,I), epoch(I), crpix(1,I), cdelt(1,I), crval(1,I),
     *      ctype(1,I), stkax(I))
          if (stkax(I).ne.0) then
            if (crpix(stkax(I),I).ne.1) then
c             Shift the coordinate reference pixel.
              crval(stkax(I),I) = crval(stkax(I),I) +
     *          (1d0 - crpix(stkax(I),I)) * cdelt(stkax(I),I)
              crpix(stkax(I),I) = 1
            endif

            if (crval(stkax(I),I).ne.1) then
              call bug(bflag, in(I)(1:len1(in(I))) //
     *          ' does not appear to be an I image')
            endif
          endif
        endif

c       Stokes Q.
        call openin(bflag, MAXDIM, MAXNAX, in(Q), tIn(Q), naxes(Q),
     *    naxis(1,Q), epoch(Q), crpix(1,Q), cdelt(1,Q), crval(1,Q),
     *    ctype(1,Q), stkax(Q))
        if (stkax(Q).ne.0) then
          if (crpix(stkax(Q),Q).ne.1) then
c           Shift the coordinate reference pixel.
            crval(stkax(Q),Q) = crval(stkax(Q),Q) +
     *        (1d0 - crpix(stkax(Q),Q)) * cdelt(stkax(Q),Q)
            crpix(stkax(Q),Q) = 1
          endif

          if (crval(stkax(Q),Q).ne.2) then
            call bug(bflag, in(Q)(1:len1(in(Q))) //
     *        ' does not appear to be a Q image')
          endif
        endif

c       Stokes U.
        call openin(bflag, MAXDIM, MAXNAX, in(U), tIn(U), naxes(U),
     *      naxis(1,U), epoch(U), crpix(1,U), cdelt(1,U), crval(1,U),
     *      ctype(1,U), stkax(U))
        if (stkax(U).ne.0) then
          if (crpix(stkax(U),U).ne.1) then
c           Shift the coordinate reference pixel.
            crval(stkax(U),U) = crval(stkax(U),U) +
     *        (1d0 - crpix(stkax(U),U)) * cdelt(stkax(U),U)
            crpix(stkax(U),U) = 1
          endif

          if (crval(stkax(U),U).ne.3) then
            call bug(bflag, in(U)(1:len1(in(U))) //
     *        ' does not appear to be a U image')
          endif
        endif

c       Compare images for consistency.
        call chkdes(bflag, MAXNAX, Q, U, in, naxes, naxis, epoch,
     *     crpix, cdelt, crval, ctype, stkax)
        if (in(I).ne.' ') call chkdes(bflag, MAXNAX, Q, I, in, naxes,
     *     naxis, epoch, crpix, cdelt, crval, ctype, stkax)

c       Strip the Stokes axis from the input header (use the Q header
c       from now on as they are all consistent).
        call axstrip(stkax, naxes, axmap, naxis, crval, crpix, cdelt,
      *   ctype)

c       Open output images starting with polarized intensity...
        if (npout.gt.0) then
          call openout(tIn, naxes, naxis, axmap, pout(1),
     *      'polarized_intensity', version, lpout(1))
          if (npout.eq.2) then
            call openout(tIn, naxes, naxis, axmap, pout(2),
     *        'polarized_intensity', version, lpout(2))
          endif
        endif

c       ...fractional polarization...
        if (nmout.gt.0) then
          call openout(tIn, naxes, naxis, axmap, mout(1),
     *      'fractional_polarization', version, lmout(1))
          if (nmout.eq.2) then
            call openout(tIn, naxes, naxis, axmap, mout(2),
     *        'fractional_polarization', version, lmout(2))
          endif
        endif

c       ...position angle.
        if (npaout.gt.0) then
          call openout(tIn, naxes, naxis, axmap, paout(1),
     *      'position_angle', version, lpaout(1))
          if (npaout.eq.2) then
            call openout(tIn, naxes, naxis, axmap, paout(2),
     *        'position_angle', version, lpaout(2))
          endif
        endif

c       Compute and write out the output image(s).
        call polout(tIn(Q), tIn(U), tIn(I), lpout, lmout, lpaout,
     *    naxes, naxis, debias, radians, snclip, paclip, sigmai,
     *    sigmaqu, rm, iline, qline, uline, pline, mline, paline,
     *    epline, emline, epaline, iflags, qflags, uflags, pflags,
     *    mflags, paflags, epflags, emflags, epaflags, zero)

c       Close down.
        call xyclose(tIn(Q))
        call xyclose(tIn(U))
        if (tIn(I).ne.0) call xyclose(tIn(I))

        if (lpout(1).ne.0) call xyclose(lpout(1))
        if (lpout(2).ne.0) call xyclose(lpout(2))
        if (lmout(1).ne.0) call xyclose(lmout(1))
        if (lmout(2).ne.0) call xyclose(lmout(2))
        if (lpaout(1).ne.0) call xyclose(lpaout(1))
        if (lpaout(2).ne.0) call xyclose(lpaout(2))
      endif

c     Draw plot
      if (device.ne.' ') call pltbias(device)

      end

c***********************************************************************

      subroutine getopt(debias, radians, relax, zero)

      logical debias, radians, relax, zero
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c    debias     Debias the polarized intensity image
c    radians    Output position nagle in radians
c    relax      Warnings only for axis descriptor mismatches
c    zero       Output zeros rather than setting flagging mask
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 4)

      character opshuns(maxopt)*7
      logical present(maxopt)
      data opshuns /'bias', 'radians', 'relax', 'zero'/
c-----------------------------------------------------------------------
      call options('options', opshuns, present, maxopt)

      debias  = .not.present(1)
      radians = present(2)
      relax   = present(3)
      zero    = present(4)

      end

c***********************************************************************

      subroutine openin(bflag, maxdim, maxnax, in, lun, naxes, naxis,
     *                  epoch, crpix, cdelt, crval, ctype, stkax)

      integer maxdim, maxnax, lun, naxes, naxis(maxnax), stkax
      double precision cdelt(maxnax), crval(maxnax), crpix(maxnax)
      real epoch
      character*(*) ctype(maxnax), in, bflag
c-----------------------------------------------------------------------
c  Open an image and return some information about it
c
c  Input
c    bflag      Bug flag
c    maxdim     Maximum number of pixels on an axis.
c    maxnax     Maximum number of axes image can have
c    in         Image name
c  Output
c    lun        Handle
c    naxes      Number of axes
c    naxis      Number of pixels on each axis.
c    epoch      EPoch of image
c    crpix      Refernce pixels
c    cdelt      Increments
c    crval      Reference values
c    ctype      Axis types
c    stkax      Stokes axis
c-----------------------------------------------------------------------
      integer len1, i
      character*80 aline
c-----------------------------------------------------------------------
      call xyopen(lun, in, 'old', maxnax, naxis)
      call rdhdi(lun, 'naxis', naxes, 0)
      if (naxes.eq.0) then
        aline = in(1:len1(in))//' has zero dimensions !!'
        call bug('f', aline)
      endif

      if (naxis(1).gt.maxdim) then
        aline = 'First dimension of ' // in(1:len1(in)) //
     *          ' too large for storage'
        call bug('f', aline)
      endif
      call hedinf(lun, naxes, naxis, epoch, crpix, cdelt, crval, ctype)

      stkax = 0
      do i = 1, naxes
        if (ctype(i).eq.'STOKES') stkax = i
      enddo
      if (stkax.eq.0) then
        aline = 'Could not find Stokes axis in '//in(1:len1(in))
        call bug(bflag, aline)
      endif

      end

c***********************************************************************

      subroutine hedinf(lun, naxes, naxis, epoch, crpix, cdelt, crval,
     *                  ctype)

      integer   lun, naxes, naxis(naxes)
      real      epoch
      double precision cdelt(naxes), crval(naxes), crpix(naxes)
      character ctype(naxes)*(*)
c-----------------------------------------------------------------------
c  Get some header keywords from the image associated with LUN
c
c  Input
c    lun        Handle of image
c    naxes      Number of axes in image
c    naxis      Number of pixels on each axis.
c  Output
c    epoch      Epoch of image
c    crpix      Array of image reference pixels
c    cdelt      Array of image increments (natural inits; rad)
c    crval      Array of image reference values (natural units)
c    ctype      Array of image axis types
c
c-----------------------------------------------------------------------
      integer i
      character str*1, itoaf*1
c-----------------------------------------------------------------------
      do i = 1, naxes
        str = itoaf(i)
        call rdhdd(lun, 'crpix'//str, crpix(i), dble(naxis(i))/2d0)
        call rdhdd(lun, 'cdelt'//str, cdelt(i), 1d0)
        call rdhdd(lun, 'crval'//str, crval(i), 0d0)
        call rdhda(lun, 'ctype'//str, ctype(i), ' ')
      enddo
      call rdhdr(lun, 'epoch', epoch, 0.0)

      end

c***********************************************************************

      subroutine chkdes(bflag, maxnax, i1, i2, im, naxes, naxis, epoch,
     *  crpix, cdelt, crval, ctype, stkax)

      integer   i1, i2, maxnax, naxes(3), naxis(maxnax,3), stkax(3)
      real      epoch(3)
      double precision cdelt(maxnax,3), crpix(maxnax,3), crval(maxnax,3)
      character bflag, ctype(maxnax,3)*(*), im(3)*(*)
c-----------------------------------------------------------------------
c  Check axis descriptors for consistency.
c
c  Input:
c    im         Images
c    naxes      Number of axes
c    naxis      Number of pixels on each axis.
c    crpix      Reference pixels
c    cdelt      Increments
c    crval      Refernce values
c    ctype      Axis types.
c    epoch      Epochs
c  Output
c    stkax      Stokes axis
c-----------------------------------------------------------------------
      integer k, l1, l2, len1
      character line*130
c-----------------------------------------------------------------------
      l1 = len1(im(i1))
      l2 = len1(im(i2))

      if (epoch(i1).ne.epoch(i2)) then
        line = 'Unequal epochs for images ' // im(i1)(1:l1) // ' & '
     *         //im(i2)(1:l2)
        call bug(bflag, line)
      endif

      if (naxes(i1).ne.naxes(i2)) then
        line = 'Unequal number dimensions for images '//
     *         im(i1)(1:l1)//' & '//im(i2)(1:l2)
        call bug(bflag, line)
      endif

      do k = 1, min(naxes(i1),naxes(i2))
        if (naxis(k,i1).ne.naxis(k,i2)) then
          write(line, 10) im(i1)(1:l1), im(i2)(1:l2), k
 10       format('Unequal dimensions for images ', a, ' & ', a,
     *            ' on axis ', i1)
          call bug(bflag, line)
        endif

        if (ctype(k,i1).ne.ctype(k,i2)) then
          write(line, 20) im(i1)(1:l1), im(i2)(1:l2), k
 20       format('Unequal ctype for images ', a, ' & ', a,
     *            ' on axis ', i1)
          call bug(bflag, line)
        endif

        call chkds2(bflag, 'crpix', k, im(i1)(1:l1), im(i2)(1:l2),
     *               crpix(k,i1), crpix(k,i2))
        call chkds2(bflag, 'cdelt', k, im(i1)(1:l1), im(i2)(1:l2),
     *               cdelt(k,i1), cdelt(k,i2))
        if (k.ne.stkax(i1) .or. k.ne.stkax(i2)) then
          call chkds2(bflag, 'crval', k, im(i1)(1:l1), im(i2)(1:l2),
     *                 crval(k,i1), crval(k,i2))
        endif
      enddo

      end

c***********************************************************************

      subroutine chkds2(bflag, type, iaxis, im1, im2, des1, des2)

      character*(*) type, im1, im2, bflag
      integer iaxis
      double precision des1, des2
c-----------------------------------------------------------------------
c     Compare an axis descriptor from two images
c
c  Input:
c    type    Type fo descriptor
c    iaxis   Axis number
c    im1,2   Images
c    des1,2  Descriptors
c
c-----------------------------------------------------------------------
      character line*130
c-----------------------------------------------------------------------
      if (abs(des1-des2).gt.0.01*max(abs(des1),abs(des2))) then
        write(line, 10) type, im1, im2, iaxis
10      format('Unequal ', a, ' for images ', a, ' & ', a,
     *          ' on axis ', i1)
        call bug(bflag, line)
      endif

      end

c***********************************************************************

      subroutine axstrip(iax, naxes, axmap, naxis, crval, crpix, cdelt,
     *                   ctype)

      integer iax, naxes, axmap(naxes), naxis(naxes)
      double precision crval(naxes), cdelt(naxes), crpix(naxes)
      character*(*) ctype(naxes)
c-----------------------------------------------------------------------
c  Strip an axis from the header items
c
c  Input:
c    iax        Axis to strip
c  Input/output
c    naxes      Number of axes.
c    axmap      Axis mapping.
c    naxis      Number of pixels on each axis.
c    crval      Reference values.
c    crpix      Reference pixels.
c    cdelt      Increments.
c    ctype      Axis types.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      if (iax.eq.0) return

      if (naxes.eq.1) call bug('f',
     *   'This image has only one dimension; cannot strip it')

      naxes = naxes - 1

      do i = 1, iax-1
        axmap(i) = i
      enddo

      do i = iax, naxes
        axmap(i) = i+1
        naxis(i) = naxis(i+1)
        crval(i) = crval(i+1)
        crpix(i) = crpix(i+1)
        cdelt(i) = cdelt(i+1)
        ctype(i) = ctype(i+1)
      enddo

      end

c***********************************************************************

      subroutine openout(lin, naxes, naxis, axmap, out, btype, version,
     *  lout)

      integer   lin, naxes, naxis(naxes), axmap(naxes), lout
      character out*(*), btype*(*), version*(*)
c-----------------------------------------------------------------------
c  Open an output image, copy header and write the history.
c
c  Input
c    lin        Image to copy keywords from.
c    naxes      Number of axes.
c    naxis      Number of pixels on each axis.
c    axmap      Axis mapping.
c    out        Name of output image.
c    btype      The type of image being opened.
c                 'fractional_polarization'
c                 'polarized_intensity'
c                 'position_angle'
c    version    Version of this program.
c  Output
c    lout       Handle for output image
c-----------------------------------------------------------------------
      character aline*80

      integer   len1
      character itoaf*1
      external  itoaf, len1
c-----------------------------------------------------------------------
c     Create the output image and copy header from input.
      call xyopen(lout, out, 'new', naxes, naxis)
      call headcp(lin, lout, naxes, axmap, 0, 0)
      call wrbtype(lout, btype)

      call hisopen(lout, 'append')
      aline = 'IMPOL: Miriad ' // version(:len1(version))
      call hiswrite(lout, aline)
      call hisinput(lout, 'IMPOL')
      call hisclose(lout)

      end

c***********************************************************************

      subroutine polout(lq, lu, li, lpout, lmout, lpaout, naxes,
     *   naxis, debias, radians, snclip, paclip, sigmai, sigmaqu, rm,
     *   iline, qline, uline, pline, mline, paline, epline, emline,
     *   epaline, iflags, qflags, uflags, pflags, mflags, paflags,
     *   epflags, emflags, epaflags, zero)

      integer   lq, lu, li, lpout(2), lmout(2), lpaout(2), naxes,
     *          naxis(naxes)
      logical   debias, radians
      real      snclip(2), paclip, sigmai, sigmaqu, rm, iline(*),
     *          qline(*), uline(*), pline(*), mline(*), paline(*),
     *          epline(*), emline(*), epaline(*)
      logical   iflags(*), qflags(*), uflags(*), pflags(*), mflags(*),
     *          paflags(*), epflags(*), emflags(*), epaflags(*), zero
c-----------------------------------------------------------------------
c  Compute some combination of polarized intensity, position angle
c  image and associated error images
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   i, ifrq, j, k
      real      fac, freq, isnr, paerr, parot, psnr, psq, sigsq,
     *          snclipsq
      double precision dtemp
      character algo*3, aline*80, ustr*8
c-----------------------------------------------------------------------
      fac  = 1.0
      ustr = ' radians'
      if (.not.radians) then
        fac  = R2D
        ustr = ' degrees'
      endif

c     Find frequency axis if rotating position angles back.
      parot = 0.0
      if (rm.ne.0.0) then
        call coInit(lq)
        call coSpcSet(lq, 'FREQ', ' ', ifrq, algo)

        if (ifrq.eq.0) call bug('f',
     *    'Could not find frequency axis for applying RM')
        if (ifrq.le.2) call bug('f',
     *    'Frequency axis is either 1 or 2.  These should be spatial')

        if (ifrq.gt.3 .or. naxis(ifrq).eq.1) then
c         Find frequency of pixel one.
          call coCvt1(lq, ifrq, 'ap', 1d0, 'aw', dtemp)
          freq  = real(dtemp) * 1e9
          parot = rm * (dcmks / freq)**2

          write(aline, 10) parot*fac, ustr
 10       format('Rotating position angles back by ', 1pe11.4, a)
          call output(aline)
        endif
      endif

      sigsq = sigmaqu * sigmaqu
      snclipsq = snclip(1) * snclip(1)

c     Loop over planes.
      do k = 1, naxis(3)
        if (rm.ne.0.0 .and. ifrq.eq.3 .and. naxis(ifrq).gt.1) then
          call coCvt1(lq, ifrq, 'ap', dble(k), 'aw', dtemp)
          freq  = real(dtemp) * 1e9
          parot = rm * (dcmks / freq)**2
        endif

c       Set planes.
        if (li.ne.0) call xysetpl(li, 1, k)
        call xysetpl(lq, 1, k)
        call xysetpl(lu, 1, k)
        if (lpout(1).ne.0) call xysetpl(lpout(1), 1, k)
        if (lpout(2).ne.0) call xysetpl(lpout(2), 1, k)
        if (lmout(1).ne.0) call xysetpl(lmout(1), 1, k)
        if (lmout(2).ne.0) call xysetpl(lmout(2), 1, k)
        if (lpaout(1).ne.0) call xysetpl(lpaout(1), 1, k)
        if (lpaout(2).ne.0) call xysetpl(lpaout(2), 1, k)

c       Read lines of data.
        do j = 1, naxis(2)
          if (li.ne.0) then
            call xyread(li, j, iline)
            call xyflgrd(li, j, iflags)
          endif
          call xyread(lq, j, qline)
          call xyflgrd(lq, j, qflags)
          call xyread(lu, j, uline)
          call xyflgrd(lu, j, uflags)

c         Work out everything possible for this row.
          do i = 1, naxis(1)
c           Output values are zeroed and flagged by default.
            call allblnk(pline(i), pflags(i), epline(i), epflags(i),
     *         mline(i), mflags(i), emline(i), emflags(i),
     *         paline(i), paflags(i), epaline(i), epaflags(i))

c           See what we can validly work out.
            if (qflags(i) .and. uflags(i)) then
c             Square of the polarized intensity.
              psq = qline(i)**2 + uline(i)**2

c             Square of the polarized intensity SNR.
              if (snclip(1).gt.0.0) then
                psnr = psq / sigsq
              else
                psnr = 1.0
              endif

c             Stokes-I SNR.
              if (snclip(2).gt.0.0 .and. sigmai.gt.0.0 .and.
     *            li.ne.0) then
                isnr = iline(i) / sigmai
              else
c               By default, snclip(2)=0 so ISNR=1 means no I-based
c               blanking by default.
                isnr = 1.0
              endif

c             P.A. error.
              if (paclip.gt.0.0 .and. psq.gt.0.0) then
                paerr = 0.5 * (sigmaqu / sqrt(psq)) * fac
              else
                paerr = -1.0
              endif

              if (psnr.gt.snclipsq .and. isnr.gt.snclip(2) .and.
     *            paerr.lt.paclip) then
c               If required, debias P and use that for errors now.
                if (debias) psq = psq - sigsq

                if (psq.gt.0.0) then
c                 Work out all the output quantities.
                  pline(i)   = sqrt(psq)
                  epline(i)  = sigmaqu
                  pflags(i)  = .true.
                  epflags(i) = .true.

                  if (li.ne.0 .and. iline(i).ne.0.0) then
                    mline(i)  = pline(i) / iline(i)
                    emline(i) = mline(i) *
     *                sqrt((sigmaqu/pline(i))**2 + (sigmai/iline(i))**2)
                    mflags(i) = .true.
                    emflags(i) = .true.
                  endif

                  paline(i)   = (atan2(uline(i),qline(i))/2.0 - parot) *
     *                             fac
                  epaline(i)  = 0.5 * (sigmaqu / pline(i)) * fac
                  paflags(i)  = .true.
                  epaflags(i) = .true.

                else
c                 Debiassing failed.
                  if (zero) then
c                   Use zero as the estimate for P and m.
                    pflags(i) = .true.
                    mflags(i) = .true.
                  endif
                endif
              endif
            endif
          enddo

c         Write out polarized intensity and error.
          if (lpout(1).ne.0) then
            call xywrite(lpout(1), j, pline)
            call xyflgwr(lpout(1), j, pflags)
          endif
          if (lpout(2).ne.0) then
            call xywrite(lpout(2), j, epline)
            call xyflgwr(lpout(2), j, epflags)
          endif

c         Write out fractional polarization and error.
          if (lmout(1).ne.0) then
            call xywrite(lmout(1), j, mline)
            call xyflgwr(lmout(1), j, mflags)
          endif
          if (lmout(2).ne.0) then
            call xywrite(lmout(2), j, emline)
            call xyflgwr(lmout(2), j, emflags)
          endif

c         Write out position angle and error.
          if (lpaout(1).ne.0) then
            call xywrite(lpaout(1), j, paline)
            call xyflgwr(lpaout(1), j, paflags)
          endif
          if (lpaout(2).ne.0) then
            call xywrite(lpaout(2), j, epaline)
            call xyflgwr(lpaout(2), j, epaflags)
          endif
        enddo
      enddo

      if (rm.ne.0.0) call coFin(lq)

      if (lpout(1).ne.0) then
        call wrhda(lpout(1), 'bunit', 'JY/BEAM')
      endif
      if (lpout(2).ne.0) then
        call wrhda(lpout(2), 'bunit', 'JY/BEAM')
      endif

      if (lpaout(1).ne.0) then
        if (radians) then
          call wrhda(lpaout(1), 'bunit', 'RADIANS')
          if (lpaout(2).ne.0) call wrhda(lpaout(2), 'bunit', 'RADIANS')
        else
          call wrhda(lpaout(1), 'bunit', 'DEGREES')
          if (lpaout(2).ne.0) call wrhda(lpaout(2), 'bunit', 'DEGREES')
        endif
      endif

      end

c***********************************************************************

      subroutine allblnk(p, pf, ep, epf, m, mf, em, emf, pa, paf, epa,
     *  epaf)

      real p, ep, m, em, pa, epa
      logical pf, epf, mf, emf, paf, epaf
c-----------------------------------------------------------------------
      p   = 0.0
      pf  = .false.
      ep  = 0.0
      epf = .false.

      m   = 0.0
      mf  = .false.
      em  = 0.0
      emf = .false.

      pa  = 0.0
      paf = .false.
      epa = 0.0
      epaf = .false.

      end

c***********************************************************************

      subroutine pltbias(device)

      character device*(*)
c-----------------------------------------------------------------------
c  Make a plot of the biases  in different estimators
c-----------------------------------------------------------------------
      integer maxrun, maxpol
      parameter (maxrun = 15000, maxpol = 1000)

      real qumax, quinc, qu, pmlsum, pml, pp0, pfo, pfosum, pobssum

      real pmldi(maxpol), pfodi(maxpol), pobsdi(maxpol), ptrue(maxpol),
     *     qunoise(2*maxrun), pobs(maxrun)
      integer nruns, i, j, nrml

      integer pgbeg, ierr, hlen
      logical conv
      character hard*3, aline*80
      real xmin, xmax, ymin, ymax

      data ymin, ymax /1e30, -1e30/
c-----------------------------------------------------------------------
      call output(' ')
      call output('Compute bias plots')
      call output(' ')
      qumax = 4.0
      quinc = 0.15
      nruns = maxrun
c
c Loop over S/N; sigma = 1
c
      qu = qumax
      pp0 = 1.0
      j = 1
      do while (pp0.gt.0.1)
c
c True polarization
c
        pp0 = sqrt(qu**2 + qu**2)
        write(aline, 10) pp0
10      format('P_true / sigma = ', f5.3)
        call output(aline)
        ptrue(j) = pp0
c
c Maximum likelihood
c
        call noisy(nruns, qu, qunoise, pobs)
        pmlsum = 0.0
        nrml = 0
        do i = 1, nruns
          call ml(pobs(i), pml, conv)
          if (conv) then
            nrml = nrml + 1
            pmlsum = pmlsum + pml
          endif
        enddo
        if (nrml.gt.0) then
          pmldi(j) = (pmlsum / nrml) - pp0
        else
          pmldi(j) = 0.0
        endif
c
c First order
c
        pfosum = 0.0
        call noisy(nruns, qu, qunoise, pobs)
        do i = 1, nruns
          call firstord(pobs(i), pfo)
          pfosum = pfosum + pfo
        enddo
        pfodi(j) = (pfosum / nruns) - pp0
c
c Observed
c
        pobssum = 0.0
        call noisy(nruns, qu, qunoise, pobs)
        do i = 1, nruns
          pobssum = pobssum + pobs(i)
        enddo
        pobsdi(j) = (pobssum / nruns) - pp0
c
c Update extrema
c
        ymin = min(ymin,pmldi(j),pfodi(j),pobsdi(j))
        ymax = max(ymax,pmldi(j),pfodi(j),pobsdi(j))
c
c Increment P/sigma
c
        qu = qu - quinc
        j = j + 1
      enddo
c
c  Try to open plot device
c
      ierr = pgbeg (0, device, 1, 1)
      if (ierr.ne.1) then
        call pgldev
        call bug('f', 'Error opening plot device')
      else
        call pgqinf('hardcopy', hard, hlen)
        call pgscf(1)
        if (hard.eq.'YES') call pgscf(2)

        xmin = 0.0
        xmax = sqrt(2.0*qumax**2) + 0.1
        call limstr(ymin, ymax)
        call pgswin(xmin, xmax, ymin, ymax)
c
c  Draw box and label
c
        call pgpage
        call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        call pglab('P\dtrue\u(\gs\dP\u=1)',
     *                '<P\dest\u> - P\dtrue\u', 'Polarization bias')
c
c  Plot points
c
        call pgline(j-1, ptrue, pobsdi)
        call pgsls(2)
        call pgline(j-1, ptrue, pfodi)
        call pgsls(3)
        call pgline(j-1, ptrue, pmldi)

        call pgtext(4.0, 0.2, 'Observed')
        call pgtext(1.5, 0.075, 'First order')
        call pgtext(3.0, -0.125, 'Maximum likelihood')
        call pgend
      endif

      end

c***********************************************************************

      subroutine noisy(nruns, qu, qunoise, pobs)

      integer nruns
      real qu, qunoise(*), pobs(*)
c-----------------------------------------------------------------------
c  Make some Gaussian noise and add it to the signal
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      call gaus(qunoise,2*nruns)
      do i = 1, nruns
        pobs(i) = sqrt((qu+qunoise(i))**2 + (qu+qunoise(i+nruns))**2)
      enddo

      end

c***********************************************************************

      subroutine ml(ppobs, ppml, conv)

      real ppobs, ppml
      logical conv
c-----------------------------------------------------------------------
c  Maximum likelihood method
c  P = estimate of real polarization from measured polarization P'
c
c  PP'/sig**2=(P/sig)**2  * I1(PP'/sig**2) / I0(PP'/sig**2)
c-----------------------------------------------------------------------
      integer itmax
      double precision tol
      parameter (itmax = 2000, tol = 1e-5)

      double precision pobs, pml, pmlold, argi, bessi0, bessi1, di
      integer i
c-----------------------------------------------------------------------
      pobs = ppobs
      pmlold = pobs
      di = 1.0

c     Iterate solution of Pml/sigma at value of Pobs/sigma.
      i = 1
      do while (i.le.itmax .and. di.gt.tol)
        argi = pmlold * pobs
        pml = pobs * bessi1(argi) / bessi0(argi)
        di = abs(pml - pmlold)
        pmlold = pml
        i = i + 1
      enddo

      conv = .true.
      if (i.ge.itmax) conv = .false.
      ppml = pml

      end

c***********************************************************************

      subroutine firstord(pobs, pfo)

      real pobs, pfo
c-----------------------------------------------------------------------
c  First order
c  P = sqrt(P'**2 - sigma**2)
c  P = estimate of real polarization from measured polarization P'
c-----------------------------------------------------------------------
      real fac
c-----------------------------------------------------------------------
      fac = pobs**2 - 1.0
      if (fac.gt.0.0) then
        pfo = sqrt(fac)
      else
        pfo = 0.0
      endif

      end

c***********************************************************************

      subroutine limstr(dmin, dmax)

      real dmin, dmax
c-----------------------------------------------------------------------
c  Stretch limits by 5%
c
c  Input/output:
c    dmin,max   Minimum and maximum
c-----------------------------------------------------------------------
      real absmax, delta
c-----------------------------------------------------------------------
      delta = 0.05 * (dmax - dmin)
      absmax = max(abs(dmax),abs(dmin))
      if (delta.le.1e-4*absmax) delta = 0.01 * absmax
      if (delta.eq.0.0) delta = 1
      dmin = dmin - delta
      dmax = dmax + delta

      end
