        program imrm

c= IMRM - Compute rotation measure image from position angle images
c& nebk
c: image analysis
c+
c       IMRM computes rotation measure and zero wavelength position
c       angle images from at least 2 position angle images at different
c       frequencies.  This is done via a linear least squares fit to:
c
c                       PA = PA_0 + RM*LAMBDA**2
c
c       where RM is the rotation measure (rad/m**2) and PA_0 is the
c       position angle at zero wavelength.  The output rotation measure
c       image is in rad/m**2, and the output position angle image is in
c       degrees.  Optionally, plots of the fits can be made.
c
c       The more frequencies you have the better.  It is very important
c       to try and get at least two sufficiently close that there is no
c       ambiguity between them.
c
c       By default, IMRM attempts to remove N*pi ambiguities from the
c       data.  Its algorithm is (pixel by pixel)
c
c         0) First remove angle according to the amount given by the
c            user (keyword "rmi") and the equation PA = RM*LAMBDA**2
c
c         1) Put the position angles of the first two frequencies in the
c            range +/- 90 degrees.
c
c         2) Remove 180 degree ambiguity from the position angles given
c            by the FIRST TWO IMAGES (keyword "in").  Thus, it modifies
c            the position angle of the second frequency by 180 degrees
c            so that the absolute value of the angle between the two
c            position angles is less than 90 degrees.
c
c         3) Compute the initial RM and PA_0 from these FIRST TWO
c            position angles.
c
c         4) This RM and PA_0 is used to predict the expected position
c            angle at the other frequencies according to the expression
c            PA = PA_0 + RM*LAMBDA**2.  Integer amounts of 180 degrees
c            are then added or subtracted to the position angles at the
c            remaining frequencies in order to make the position angle
c            as close as possible to the expected value.
c
c         5) Then a least squares fit is used to solve for the RM and
c            PA_0
c
c         6) Finally, the procedure is repeated from step 0) where the
c            initial guess is now the value just determined above in
c            step 5).
c
c       The order in which the images are given is thus very important.
c       You should generally give your images in order of decreasing
c       frequency, with the assumption being that the smallest angle
c       between the first two represents a rough guess for the RM
c       with no ambiguities.  However, if you are very certain abou
c       the lack of ambiguity between certain frequencies, or there
c       are some of particularly high S/N and likely lack of ambiguity,
c       you may like to try these.  Its a nasty business and it is VERY
c       important that you look at the results carefully.
c
c       The attempt to remove ambiguities can be turned off with keyword
c       "options=ambiguous".  In this case, its algorithm is
c
c         0) First remove angle according to the intial guess given by
c            the user (keyword "rmi").
c
c         1) Put all position angles in the range +/- 90 degrees.
c
c         2) Then a least squares fit is used to solve for the RM and
c            PA_0.
c
c       In principle, you should never need to use this option.  If
c       there are no ambiguities, the first algorithm shouldn't find
c       any!
c
c       There are also a variety of methods offered with which to
c       blank the output images.  Most of these require error images
c       associated with the input position angle images.  Use IMPOL to
c       make the position angle images and position angle error images.
c
c@ in
c       Up to 5 input position angle (positive N -> E) images (in
c       degrees) at different frequencies.  Generally, you should give
c       the images in order of decreasing frequency.  Wild card
c       expansion is supported, no default.
c@ inerr
c       Up to 5 position angle error images (in degrees) used for
c       weighting the data during the least squares fit.  They are
c       assumed to be in one-to-one association with the position angle
c       images.  If no error images are given, each position angle image
c       is given equal weight and we must assume a goodness of fit of
c       unity in order to find the output image errors.  Wild card
c       expansion is supported, default is no error images.
c@ rmi
c       An amount of rotation measure to remove from the data before
c       fitting.  If you have a good idea of this, it helps enormously
c       in removing ambiguities.  See the detailed use in the discussion
c       of the algorithm above.  See also options=guess where it is used
c       slightly differently.  Default is 0.
c@ rm
c       Two values. The output fitted rotation measure image in
c       rad/m**2, and optionally, its associated error image.
c       The default is no output RM images.
c@ pa0
c       The output fitted (at zero wavelength) position angle image in
c       degrees, and optionally, its associated error image.
c       The default is no output PA images.
c@ qcut
c       Blank the output image (RM or PA) pixels if the goodness of fit
c       (Q) is less than this value.  If Q is larger than about 0.1 say,
c       the fit is believable.  If it is greater than 0.001, the fit may
c       be acceptable if the errors are non-normal or too small.  If Q
c       is less than 0.001 the model can be called into question.  The
c       probability distribution for position angle images approximates
c       a Gaussian at high S/N ratios.  At low S/N ratios (roughly, when
c       P/sigma < 2) it is non-Gaussian.  If you don't specify error
c       images, Q cannot be determined and is assumed to be one.  This
c       is also true if you give IMRM position angle images at two
c       frequencies only.  Default is 0.001
c@ errcut
c       Blank the output image (RM or PA) pixels if ANY of the input PA
c       image pixels has an error greater than this value (degrees).
c       Default is no input error based blanking.
c@ rmcut
c       Blank pixels in BOTH the output RM and PA_0 images when the
c       error in the fitted RM is greater than this value (rad/m**2).
c       Errors can be worked out if you give input error images, or if
c       you input images at more than two frequencies AND we assume the
c       goodness of fit is unity.  Default is no fitted RM error based
c       blanking.
c@ pacut
c       Blank pixels in BOTH the output RM and PA_0 images when the
c       error in the fitted PA_0 is greater than this value (degrees).
c       Errors can be worked out if you give input error images, or if
c       you input images at more than two frequencies AND we assume the
c       goodness of fit is unity.  Default is no fitted PA_0 error based
c       blanking.
c@ device
c       PGPLOT plotting device to see the fits to the data.  The
c       absolute pixel numbers in x and y are also written into the
c       corner of the plot (unless options=accumulate).  No default.
c@ nxy
c       Number of subplots per page in the x and y directions, to put on
c       the plotting device.  See options=accumulate.  The default is
c       2,2 (i.e. 2x2).
c@ csize
c       PGPLOT character height.  Default is 1.0.
c@ options
c       Task enrichment options.  Minimum match is active,
c       "relax"      issue warnings instead of a fatal error when image
c                    axis descriptors are inconsistent with each other,
c                    and when the input image headers do not indicate
c                    that they are position angle images
c                    (btype=position_angle).
c       "guess"      when removing ambiguities, this option causes IMRM
c                    to use the rotation measure input through the
c                    keyword "rmi" in step 3 above (on the first pass
c                    only), rather than working it out from the first
c                    two frequencies.  By default, angle is removed from
c                    the data according to the value of "rmi" and then
c                    the first guess made from the first two
c                    frequencies.  The angle is not removed in this way
c                    with this option.  This may prove useful if you
c                    have two close but perhaps noisy frequencies which
c                    is causing the initial guess of the RM to be wrong
c                    (because of noise) and driving the subsequent turn
c                    removal off.
c       "ambiguous"  Do not try to remove ambiguites.
c       "accumulate" means put all the plots on one sub-plot, rather
c                    than the default, which is to put the plot for each
c                    spatial pixel on a spearate subplot.
c       "yindependent"
c                    By default, the sub-plots are all drawn with the
c                    same Y-axis scale, that embraces all sub-plots.
c                    This option forces each sub-plot to be scaled
c                    independently.
c
c$Id: imrm.for,v 1.6 2010/09/27 07:29:36 cal103 Exp $
c--
c  History:
c    nebk 22may92   Original version.
c    nebk 26may92   Try to deal with ambiguities.
c    nebk 27may92   Improve warnings to users
c    nebk 04nov92   Reorganize and improve blanking, add output error
c                   images, rewrite lsf to include goodness of fit
c    mjs  12mar93   Use maxnax.h file instead of setting own value.
c    nebk 11nov93   Add options=ambiguous and output blanking info
c    nebk 30jan95   Work on ambiguity algorithm, add keywords "rmi",
c                   "device", "nxy", "csize", "options=acc,gues,yind"
c    nebk 28mar95   Trying to plot nowhere if device blank
c    nebk 21jun97   Was plotting garbage on some platforms if
c                   some output points were blanked.  Also in hedinfo
c                   rdhdd was being called with default real arg.
c    rjs  02jul97   cellscal change.
c    rjs  23jul97   added pbtype.
c    nebk 25aug00   bump to 20 image.  subroutine chkdes should not
c                   be checking cdelt/crpix on frequency axis
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
      include 'mem.h'

      integer MAXIM
      parameter (MAXIM = 20)

      logical   accum, ambig, blank, doerrbl, dopabl, dormbl,
     *          flags(MAXDIM,MAXIM), flagsE(MAXDIM,MAXIM), guess, noerr,
     *          oblank, oflags(MAXDIM), relax, yind
      integer   axLen(MAXNAX), axLenP, blsum, i, imin, ippyd, ippyf,
     *          ipyd, ipyf, j, jmin, k, l, left, lInE(MAXIM),
     *          lIn(MAXIM), lOutPA, lOutPAe, lOutRM, lOutRMe, naxis,
     *          nbl(6), nIn, nInE, nx, ny
      real      cs, d2, diff, errcut, lmbdsq(MAXIM), pa(MAXIM),
     *          pa2(MAXIM), pacut, padummy, q, qcut, rmcut, rmi,
     *          row(MAXDIM,MAXIM), rowE(MAXDIM,MAXIM), rowPA(MAXDIM),
     *          rowPAe(MAXDIM), rowRM(MAXDIM), rowRMe(MAXDIM), wt(MAXIM)
      double precision dummy, freq(MAXIM)
      character bflag, device*32, inE(MAXIM)*64, in(MAXIM)*64, outRM*64,
     *          outRMe*64, outPA*64, outPAe*64, text*100, version*72

      character versan*80
      external  versan

      data nbl /6*0/
      data padummy /-100000.0/
c-----------------------------------------------------------------------
      version = versan ('imrm',
     *                  '$Revision: 1.6 $',
     *                  '$Date: 2010/09/27 07:29:36 $')

c     Get the inputs.
      call keyini

      call mkeyf('in', in, MAXIM, nIn)
      if (nIn.lt.2) call bug('f',
     *  'You must specify at least two input images')
      call mkeyf('inerr', inE, MAXIM, nInE)
      if (nInE.gt.0 .and. nInE.ne.nIn) call bug('f',
     *  'You must give the same number of error as input PA images')

      call keya('rm', outRM, ' ')
      call keya('rm', outRMe, ' ')
      call keyr('rmi', rmi, 0.0)
      call keya('pa0', outPA, ' ')
      call keya('pa0', outPAe, ' ')
c      if (outRM.eq.' ' .and. outPA.eq.' ') call bug('f',
c     +  'You have not specified any output images')

      call keyr('qcut', qcut, 0.001)
      call keyr('errcut', errcut, -1.0)
      call keyr('rmcut', rmcut, -1.0)
      call keyr('pacut', pacut, -1.0)
      call keyr('csize', cs, 0.0)
      call getopt(relax, ambig, accum, yind, guess)
      if (guess .and. ambig) call bug('f',
     *  'Options=guess,ambiguous are mutually exclusive')
      call keya('device', device, ' ')
      call keyi('nxy', nx, 2)
      call keyi('nxy', ny, nx)

      call keyfin

c     Sort out inputs and give user some helpful (?) messages.
      if (qcut.gt.0.0 .and. nInE.le.2) then
        call bug('w',
     *   'You need at least three input error images to be able')
        call bug('w',
     *   'to compute the goodness of fit; setting q=1 & qcut=0.')
        call output(' ')
        qcut = 0.0
      endif
      if (qcut.lt.0.0 .or. qcut.gt.1.0) call bug('f',
     *  'Your goodness of fit blanking level is invalid')

      dormbl = .false.
      if (rmcut.gt.0.0) dormbl = .true.

      dopabl = .false.
      if (pacut.gt.0.0) dopabl = .true.

      doerrbl = .false.
      if (errcut.gt.0.0) doerrbl = .true.

      if (doerrbl .and. nInE.eq.0) call bug('f',
     *    'Input error blanking needs input error images')

      if ((dormbl .or. dopabl) .and. (nIn.le.2 .and. nInE.eq.0)) then
        call output
     *  ('### Fatal error: Output error blanking requires input error')
        call output
     *  ('### Fatal error: images or at least 3 input p.a. images and')
       call bug('f',
     *   'the assumption that the goodness of fit is unity')
      endif
      if ((outRMe.ne.' ' .or. outPAe.ne.' ') .and.
     *     (nIn.le.2 .and. nInE.eq.0)) then
        call output
     *  ('### Fatal error: Output error images require input error')
        call output
     *  ('### Fatal error: images or at least 3 input p.a. images and')
        call bug('f',
     *   'the assumption that the goodness of fit is unity')
      endif
      if ((dormbl .or. dopabl  .or. outRMe.ne.' ' .or.
     *     outPAe.ne.' ') .and. (nInE.eq.0 .and. nIn.gt.2)) then
        call bug('w', 'The output errors will be calculated')
        call bug('w', 'assuming the goodness of fit is unity')
        call output(' ')
      endif

      if (.not.dormbl .and. .not.dopabl .and. .not.doerrbl) then
        call bug('w',
     *    'It is advised that some error-based blanking be used.')
        call output(' ')
      endif

      if (nIn.eq.2) then
        call bug('w',
     *    'It must be assumed that there are no ambiguities')
        call bug('w', 'with only two different frequencies.')
        call output(' ')
      endif
      bflag = 'f'
      if (relax) bflag = 'w'

      if (accum) then
        nx = 1
        ny = 1
      else
        nx = max(1,nx)
        ny = max(1,ny)
      endif

c     Indicate that we have input error images.
      noerr = .true.
      if (nInE.gt.0) noerr = .false.

c     Open the input images and check consistency.
      do i = 1, nIn
        call openin(in(i), bflag, lIn(i), axLen)
        call chkdes(lIn(i), in(i), bflag, freq(i))
      enddo

c     Open the error images and check consistency.
      if (nInE.gt.0) then
        do i = 1, nInE
          call openin(inE(i), bflag, lInE(i), axLen)
          call chkdes(lInE(i), inE(i), bflag, dummy)
        enddo
      endif

c     Find wavelength of each image at pixel 1 in metres.
      call output(' ')
      do i = 1, nIn
        lmbdsq(i) = (DCMKS / (freq(i) * 1e9))**2
        write(text,10) freq(i)
10      format('Found frequency ',f8.4,' GHz')
        call output(text)
      enddo

c     Warn user if degenerate frequencies.
      diff = 1e30
      do i = 1, nIn-1
        do j = i+1, nIn
          d2 = abs(freq(j) - freq(i))
          if (d2.eq.0.0) call bug('w',
     *      'There are degenerate frequencies amongst the PA images')
          if (d2.lt.diff) then
            diff = d2
            imin = i
            jmin = j
          endif
        enddo
      enddo

      if (.not.ambig) then
        write(text,20) freq(imin), freq(jmin)
20      format('Closest frequencies are ',f8.4,' & ',f8.4,' GHz')
c        call output (text)
      endif

c     Use first 2 images given by user so they have control.  Using
c     closest frequencies is a lousy algorithm.
      imin = 1
      jmin = 2


c     Create the output images, copy the header keywords from the
c     first input image and add the new history.
      call rdhdi(lIn(1), 'naxis', naxis, 0)
      if (outRM.ne.' ') then
        call xyopen(lOutRM, outRM, 'new', naxis, axLen)
        call mkHead(lIn(1), lOutRM, version)
        call wrhda(lOutRM, 'bunit', 'RAD/M/M')
        call wrbtype(lOutRM, 'rotation_measure')
      endif
      if (outRMe.ne.' ') then
        call xyopen(lOutRMe, outRMe, 'new', naxis, axLen)
        call mkHead(lIn(1), lOutRMe, version)
        call wrhda(lOutRMe, 'bunit', 'RAD/M/M')
        call wrbtype(lOutRMe, 'rotation_measure')
      endif
      if (outPA.ne.' ') then
        call xyopen(lOutPA, outPA, 'new', naxis, axLen)
        call mkHead(lIn(1), lOutPA, version)
        call wrhda(lOutPA, 'bunit', 'DEGREES')
        call wrbtype(lOutPA, 'position_angle')
      endif
      if (outPAe.ne.' ') then
        call xyopen(lOutPAe, outPAe, 'new', naxis, axLen)
        call mkHead(lIn(1), lOutPAe, version)
        call wrhda(lOutPAe, 'bunit', 'DEGREES')
        call wrbtype(lOutPAe, 'position_angle')
      endif


c     Allocate space for plots.
      axLenP = nIn*axLen(1)*axLen(2)
      call memalloc(ipyd, axLenP, 'r')
      call memalloc(ipyf, axLenP, 'r')
      ippyd = ipyd
      ippyf = ipyf

c     Compute results; assume 2-D images only.
      do j = 1, axLen(2)
c       Read lines from each image.
        do k = 1, nIn
          call xyread (lIn(k), j,   row(1,k))
          call xyflgrd(lIn(k), j, flags(1,k))
          if (nInE.gt.0) then
            call xyread (lInE(k), j,   rowE(1,k))
            call xyflgrd(lInE(k), j, flagsE(1,k))
          endif
        enddo

c       Do fit for each image pixel; weighted fit if errors available.
        do i = 1, axLen(1)
          blank = .false.
          do k = 1, nIn
c           Do fit only if all the input and possibly input error image
c           pixels are unblanked.
            if ((.not.flags(i,k)) .or.
     *           (nInE.gt.0 .and. .not.flagsE(i,k))) then
              blank = .true.
              nbl(1) = nbl(1) + 1
              goto 100
            endif

c           Do fit only if all the input error image pixels are smaller
c           than the specified cutoff.
            if (doerrbl .and. rowE(i,k).gt.errcut) then
              blank = .true.
              nbl(2) = nbl(2) + 1
              goto 100
            endif

c           Work out weights if error images given.
            pa(k) = row(i,k) * DD2R
            wt(k) = 1.0
            if (.not.noerr) then
              if (rowE(i,k).ne.0.0) then
                wt(k) = 1.0 / (rowE(i,k)*DD2R)**2
              else
                blank = .true.
                nbl(3) = nbl(3) + 1
                goto 100
              endif
            endif
          enddo

100       if (.not.blank) then

c           Do the fit if all the input criteria are satisified.
            call rotfit(imin, jmin, noerr, nIn, lmbdsq, pa, pa2, wt,
     *        guess, ambig, rmi, rowRM(i), rowPA(i), rowRMe(i),
     *        rowPAe(i), q, memr(ippyd))
            ippyd = ippyd + nIn

c           Fill the fit into the plot buffer.
            do l = 1, nIn
              memr(ippyf) = (rowPA(i) + rowRM(i)*lmbdsq(l))*DR2D
              ippyf = ippyf + 1
            enddo

c           Put output position angle between +/- 90 degrees.
            call pm90(rowPA(i))
            rowPA(i) = rowPA(i) * DR2D
            rowPAe(i) = rowPAe(i) * DR2D

c           Ditch this pixel if output blanking criteria so dictate.
            oflags(i) = .true.
            oblank = .false.
            if (q.lt.qcut) then
              nbl(4) = nbl(4) + 1
              oblank = .true.
            else if (dormbl .and. rowRMe(i).gt.rmcut) then
              nbl(5) = nbl(5) + 1
              oblank = .true.
            else if (dopabl .and. rowPAe(i).gt.pacut) then
              nbl(6) = nbl(6) + 1
              oblank = .true.
            endif
            if (oblank) call blnkall(oflags(i), rowRM(i), rowPA(i),
     *        rowRMe(i), rowPAe(i))
          else
            call blnkall(oflags(i), rowRM(i), rowPA(i),
     *                    rowRMe(i), rowPAe(i))
            do l = 1, nIn
              memr(ippyf) = padummy
              memr(ippyd) = padummy
              ippyf = ippyf + 1
              ippyd = ippyd + 1
            enddo
          endif
        enddo

c       Write out the results.
        if (outRM.ne.' ') then
          call xywrite(lOutRM, j, rowRM)
          call xyflgwr(lOutRM, j, oflags)
        endif
        if (outRMe.ne.' ') then
          call xywrite(lOutRMe, j, rowRMe)
          call xyflgwr(lOutRMe, j, oflags)
        endif
        if (outPA.ne.' ') then
          call xywrite(lOutPA, j, rowPA)
          call xyflgwr(lOutPA, j, oflags)
        endif
        if (outPAe.ne.' ') then
          call xywrite(lOutPAe, j, rowPAe)
          call xyflgwr(lOutPAe, j, oflags)
        endif
      enddo

c     Close up.
      do k = 1, nIn
        call xyclose(lIn(k))
        if (nInE.gt.0) call xyclose(lInE(k))
      enddo
      if (outRM.ne.' ')  call xyclose(lOutRM)
      if (outRMe.ne.' ') call xyclose(lOutRMe)
      if (outPA.ne.' ')  call xyclose(lOutPA)
      if (outPAe.ne.' ') call xyclose(lOutPAe)

c     Tell user about blanking.
      call output(' ')
      if (nbl(1).gt.0) then
        write(text, '(i8, a)') nbl(1),
     *    ' output pixels were blanked because so were input pixels'
        call output(text)
      endif
      if (nbl(3).gt.0) then
        write(text, '(i8, a)') nbl(3),
     *  ' output pixels were blanked because input error pixels were 0'
        call output(text)
      endif
      if (nbl(2).gt.0) then
        write(text, '(i8, a)') nbl(2),
     *    ' output pixels were blanked because of "ERRCUT"'
        call output(text)
      endif
      if (nbl(4).gt.0) then
        write(text, '(i8, a)') nbl(4),
     *    ' output pixels were blanked because of "QCUT"'
        call output(text)
      endif
      if (nbl(5).gt.0) then
        write(text, '(i8, a)') nbl(5),
     *    ' output pixels were blanked because of "RMCUT"'
        call output(text)
      endif
      if (nbl(6).gt.0) then
        write(text, '(i8, a)') nbl(6),
     *    ' output pixels were blanked because of "PACUT"'
        call output(text)
      endif

      blsum = 0
      do i = 1, 6
        blsum = blsum + nbl(i)
      enddo
      left = axLen(1)*axLen(2) - blsum
      write(text, '(i8, a)') left, ' output pixels were unblanked'
      call output(text)
      call output(' ')

c     Plots.
      if (device.ne.' ') call plotit(device, nx, ny, accum, yind,
     *  axLen(1), axLen(2), nIn, lmbdsq, memr(ipyd), memr(ipyf), cs,
     *  padummy)
      call memfree(ipyd, axLenP, 'r')
      call memfree(ipyf, axLenP, 'r')

      end

c***********************************************************************

      subroutine blnkall (flag, rm, pa, erm, epa)

      real rm, pa, erm, epa
      logical flag
c-----------------------------------------------------------------------
      flag = .false.
      rm   = 0.0
      pa   = 0.0
      erm  = 0.0
      epa  = 0.0

      end

c***********************************************************************

      subroutine chkdes(lIn, inName, bflag, freq)

      integer   lIn
      character bflag*1, inName*(*)
      double precision freq
c-----------------------------------------------------------------------
c  Compare image headers for conformance.  Saves header keywords for the
c  image specified when first called and compares them with those for
c  images specified in subsequent calls.
c
c  Input:
c    lIn        Handle of input image.
c    inName     Image name for reporting
c    bflag      Error handling flag, either 'f' (fatal) or 'w' (warn).
c  Ouput:
c    freq       Frequency of first pixel.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      logical   doInit
      integer   axLen(2,MAXNAX), fqax(2), frqax, iax, k, l1, l2, len1,
     *          naxis(2)
      double precision cdelt(2,MAXNAX), crpix(2,MAXNAX),
     *          crval(2,MAXNAX), epoch(2)
      character cax*1, ctype(2,MAXNAX)*16, name1*80, text*130

      character itoaf*1
      external  itoaf

      save doInit, axLen, fqax, naxis, cdelt, crpix, crval, epoch,
     *     ctype

      data doInit /.true./
c-----------------------------------------------------------------------
      if (doInit) then
        k = 1
        name1 = inName
        l1 = len1(name1)
      else
        k = 2
      endif

      l2 = len1(inName)

c     Read all required descriptors.
      call rdhdi(lIn, 'naxis', naxis(k), 0)
      if (naxis(k).eq.0) then
        text = inName(:l2) // ' has zero dimensions!'
        call bug('f', text)
      endif

      do iax = 1, naxis(k)
        cax = itoaf(iax)
        call rdhdi(lIn, 'naxis'//cax, axLen(k,iax), 0)
        call rdhdd(lIn, 'crpix'//cax, crpix(k,iax), 0d0)
        call rdhdd(lIn, 'cdelt'//cax, cdelt(k,iax), 1d0)
        call rdhdd(lIn, 'crval'//cax, crval(k,iax), 0d0)
        call rdhda(lIn, 'ctype'//cax, ctype(k,iax), ' ')
      enddo
      call rdhdd(lIn, 'epoch', epoch(k), 0d0)

      if (axLen(k,1).gt.MAXDIM) then
        text = 'First dimension of ' // inName(:l2) //
     *         ' exceeds storage.'
        call bug('f', text)
      endif

c     Find the frequency axis and do basic checks.
      fqax(k) = 0
      do iax = 1, naxis(k)
        if (index(ctype(k,iax),'FREQ').ne.0) fqax(k) = iax
      enddo

      if (fqax(k).eq.0) then
        text = inName(:l2) // ' has no frequency axis'
        call bug('f', text)
      endif

      if (fqax(k).le.2) then
        text = 'Frequency axis for ' // inName(:l2) //
     *         ' is < 3; should be > 2.'
        call bug('f', text)
      endif

      if (axLen(k,fqax(k)).gt.1) call bug('f',
     *  'Frequency axis must be of length 1 only.')

      frqax = fqax(k)
      freq  = (1d0 - crpix(k,frqax))*cdelt(k,frqax) + crval(k,frqax)


c     Compare descriptors.
      if (doInit) then
        doInit = .false.
        return
      endif

      if (naxis(2).ne.naxis(1)) then
        text = 'Number of axes differs for ' //
     *          inName(:l2) // ' & ' // name1(:l1)
        call bug(bflag, text)
      endif

      if (fqax(2).ne.fqax(1)) then
        text = 'Frequency axis differs for ' //
     *          inName(:l2) // ' & ' // name1(:l1)
        call bug('f', text)
      endif

      do iax = 1, naxis(1)
        if (axLen(2,iax).ne.axLen(1,iax)) then
          write(text, 10) iax, inName(:l2), ' & ', name1(:l1)
10        format('Axis ',i1,' differs in length for ',a,' & ',a)
          call bug(bflag, text)
        endif

        if (iax.ne.fqax(1)) then
          if (crpix(2,iax).ne.crpix(1,iax)) then
            write(text,20) 'crpix', iax, inName(:l2),' & ',name1(:l1)
            call bug(bflag, text)
          endif

          if (crval(2,iax).ne.crval(1,iax)) then
            write(text,20) 'crval', iax, inName(:l2),' & ',name1(:l1)
            call bug(bflag, text)
          endif

          if (cdelt(2,iax).ne.cdelt(1,iax)) then
            write(text,20) 'cdelt', iax, inName(:l2),' & ',name1(:l1)
            call bug(bflag, text)
          endif

          if (ctype(2,iax).ne.ctype(1,iax)) then
            write(text,20) 'ctype', iax, inName(:l2),' & ',name1(:l1)
            call bug(bflag, text)
          endif
        endif

20      format(a,i1,' differs for ', a)
      enddo

      if (epoch(2).ne.epoch(1)) then
        text = 'Epoch differs for ' // inName(:l2) //' & '// name1(:l1)
        call bug(bflag, text)
      endif

      end

c***********************************************************************

      subroutine extreme (n, d1, d2, dmin, dmax, padummy)

      integer   n
      real      d1(n), d2(n), dmin, dmax, padummy
c-----------------------------------------------------------------------
      integer   i
c-----------------------------------------------------------------------
      dmin = 1e32
      dmax = -1e32
      do i = 1, n
        if (d1(i).ne.padummy .and. d2(i).ne.padummy) then
          dmin = min(d1(i),d2(i),dmin)
          dmax = max(d1(i),d2(i),dmax)
        endif
      enddo
      dmin = dmin - (0.05*(dmax-dmin))
      dmax = dmax + (0.05*(dmax-dmin))
      if (dmin.eq.dmax) then
        if (dmin.eq.0.0) then
          dmin = -1.0
          dmax = 1.0
        else
          dmin = dmin - 0.5*dmin
          dmax = dmax + 0.5*dmax
        endif
      endif

      end

c***********************************************************************

      subroutine getopt (relax, ambig, accum, yind, guess)

      logical   relax, ambig, accum, yind, guess
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c     relax     Warnings only for axis descriptor mismatches.
c     ambig     Do not remove ambuguites.
c     accum     Put all plots on one page.
c     yind      Each subplot scaled independently.
c     guess     Use "rmi" as first guess.
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 5)

      character opshuns(maxopt)*12
      logical present(maxopt)
      data opshuns /'relax', 'ambiguous', 'accumulate',
     *              'yindependent', 'guess'/
c-----------------------------------------------------------------------
      call options('options', opshuns, present, maxopt)

      relax = present(1)
      ambig = present(2)
      accum = present(3)
      yind  = present(4)
      guess = present(5)

      end

c***********************************************************************

      subroutine openin (in, bflag, lIn, axLen)

      character bflag*1, in*(*)
      integer   lIn, axLen(*)
c-----------------------------------------------------------------------
c  Open an image and return some information about it.
c
c  Input
c    in         Image name.
c    bflag      Bug flag; 'w' or 'f'.
c  Output
c    lIn        Handle.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      character btype*25, text*80

      integer   len1
      character itoaf*1
      external  itoaf, len1
c-----------------------------------------------------------------------
      call xyopen(lIn, in, 'old', MAXNAX, axLen)

      call rdbtype(lIn, btype, ' ')
      if (btype.ne.'position_angle') then
        text = in(1:len1(in)) // ' does not appear to be a PA image.'
        call bug(bflag, text)
      endif

      end

c***********************************************************************

      subroutine mkHead(lIn, lOut, version)

      integer   lIn, lOut
      character version*72
c-----------------------------------------------------------------------
c  Write header and history for an output file.
c
c  Input
c    lIn        Handle for an open input image from which to copy
c               header keywords.
c    lOut       Handleof open output image.
c    version    Version of task.
c-----------------------------------------------------------------------
      call headcopy(lIn, lOut, 0, 0, 0, 0)

      call hisopen(lOut, 'append')
      call hiswrite(lOut, 'IMRM Miriad' // version)
      call hisinput(lOut, 'IMRM')
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine plotit (device, nx, ny, accum, yind, npx, npy, nf,
     *                   x, yd, yf, cs, padummy)

      integer npx, npy, nx, ny, nf
      real x(nf), yd(npx*npy*nf), yf(npx*npy*nf), cs, padummy
      logical accum, yind
      character*(*) device
c-----------------------------------------------------------------------
c  Plot data and fits.
c
c  Input
c    device     PGPLOT device.
c    nx,ny      Number of subplots in x and y.
c    accum      Put all on one plot if true.
c    yind       Scale subplots independently.
c    npx,y      Number of pixels in images in x and y.
c    nf         Number of frequencies.
c    x,yd,yf    Plotting arrays, data and fits.
c    cs         Character height.
c-----------------------------------------------------------------------
      integer ierr, pgbeg, i, j, ip, npts, i1, i2
      real xmin, xmax, ymin, ymax
      character str1*10, str2*10, text*80
c-----------------------------------------------------------------------
c     Open plot device.
      ierr = pgbeg (0, device, nx, ny)
      if (ierr.ne.1) then
        call pgldev
        call bug('f', 'Error opening plot device')
      endif
      if (cs.le.0.0) cs = 1.0
      call pgsch(cs)

c     Find extrema.
      call extreme(nf, x, x, xmin, xmax, padummy)
      npts = npx*npy*nf
      call extreme(npts, yd, yf, ymin, ymax, padummy)

c     If accumulating all on one plot draw the box and label it now.
      call pgvstd
      call pgswin(xmin, xmax, ymin, ymax)
      if (accum) then
        call pgpage
        call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        call pglabel('\gl\u2\d (m\u2\d)',
     *                'Position angle (degrees)', ' ')
      endif

c     Loop over plots.
      ip = 1
      do j = 1, npy
        do i = 1, npx
          if  (yd(ip).ne.padummy .and. yf(ip).ne.padummy) then

c           If not accumulating, do some things for each subplot.
            if (.not.accum) then
              call pgpage
              if (yind) then
                call extreme(nf, yd(ip), yf(ip), ymin, ymax, padummy)
                call pgswin(xmin, xmax, ymin, ymax)
              endif

              call pgbox('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
              call pglabel('\gl\u2\d (m\u2\d)',
     *                      'Position angle (degrees)', ' ')

c             Write i,j pixel on plot.
              call strfi(i, '(i4)', str1, i1)
              call strfi(j, '(i4)', str2, i2)
              text = str1(1:i1)//','//str2(1:i2)
              call pgmtxt('T', -2.0, 0.1, 0.0, text(1:i1+i2+1))
            endif

c           Draw plot.
            call pgpt(nf, x, yd(ip), 17)
            call pgline(nf, x, yf(ip))
          endif
          ip = ip + nf
        enddo
      enddo

      call pgend

      end

c***********************************************************************

      subroutine pm90 (pa)

      real pa
c-----------------------------------------------------------------------
c  Put an angle into the range =/- 90 degrees.   Remember up is the
c  same as down for polarization postion angles.
c
c  Input/output
c     pa        Position angle in radians.
c-----------------------------------------------------------------------
      include 'mirconst.h'
c-----------------------------------------------------------------------
      pa = mod(dble(pa), DPI)
      if (pa.gt.DPI_2) then
        pa = pa - DPI
      else if (pa.lt.-DPI_2) then
        pa = pa + DPI
      endif

      end

c***********************************************************************

      subroutine rempi (pa1, pa2)

      real     pa1, pa2
c-----------------------------------------------------------------------
      include 'mirconst.h'
      double precision d
c-----------------------------------------------------------------------
      d = pa1 - pa2
      if (d.gt.DPI_2) then
        pa2 = pa2 + DPI
      else if (d.lt.-DPI_2) then
        pa2 = pa2 - DPI
      endif

      end

c***********************************************************************

      subroutine rfit (guess, c1, c2, noerr, n, lmbdsq, pa, wt, rmi,
     *                 rm, pa0, erm, epa0, q)

      integer n, c1, c2
      real    lmbdsq(n), pa(n), wt(n), rm, pa0, erm, epa0, q, chisq, rmi
      logical noerr, guess
c-----------------------------------------------------------------------
c  Low level least squares fit PA versus LAMBDA**2, with optional
c  ambiguity removal.
c
c  Input
c    guess      Use rmi as initial guess rather than removing rmi
c               rotation and getting it from first 2 frequencies.
c    c1,2       Pointers to frequencies to use for attempt to remove
c               ambiguities.
c    noerr      True if there were no input position angle error images.
c    n          Number of frequencies.
c    lmbdsq     Wavelength squared (m**2).
c    pa         Position angles (radians).
c    wt         Weights.
c    rmi        Amount of rotation measure to remove first.
c  Output
c    rm         Fitted rotation measure (rad/m**2).
c    pa0        Position angle at zero wavelength (radians).
c    erm        Error in RM.
c    epa0       Error in PA0.
c    q          Goodness of fit.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      real rmg, pag, yp
      integer i, nturns
c-----------------------------------------------------------------------
c     Remove initial guess for RM.
      if (.not.guess .and. rmi.ne.0.0) call rmsub(n, lmbdsq, pa, rmi)

c     Remove PI ambiguity from designated frequencies.
      call pm90(pa(c1))
      call pm90(pa(c2))
      call rempi(pa(c1), pa(c2))

c     Find initial guess for RM and PA0 from these frequencies or from
c     that input by the user.
      if (guess) then
        rmg = rmi
      else
        rmg = (pa(c2) - pa(c1)) / (lmbdsq(c2) - lmbdsq(c1))
      endif
      pag = pa(c1) - rmg*lmbdsq(c1)

c     Try to remove N*PI ambiguities from the other frequencies.
c     If only 2 frequencies, nothing will happen here.
      do i = 1, n
        if (i.ne.c1 .and. i.ne.c2) then

c         How many turns will place the actual position angle closest to
c         the predicted position angle?  Add these turns to the data.
          yp = rmg*lmbdsq(i) + pag
          nturns = nint((yp - pa(i))/DPI)
          pa(i) = pa(i) + nturns*DPI
        endif
      enddo

c     Do the fit.
      call lsf(noerr, n, lmbdsq, pa, wt, rm, pa0, erm, epa0, chisq, q)

      end

c***********************************************************************

      subroutine rmsub (n, lmbdsq, pa, rmi)

      integer n
      real    rmi, pa(n), lmbdsq(n)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        pa(i) = pa(i) - lmbdsq(i)*rmi
      enddo

      end

c***********************************************************************

      subroutine rotfit (c1, c2, noerr, n, lmbdsq, pa, pa2, wt, guess,
     *                   ambig, rm0, rmf, pa0f, erm, epa0, q, ypl)

      integer n, c1, c2
      real    lmbdsq(n), pa(n), pa2(n), wt(n), rm0, rmf, pa0f, erm,
     *        epa0, q, chisq, ypl(n)
      logical noerr, ambig, guess
c-----------------------------------------------------------------------
c  High level least squares fit PA versus LAMBDA**2.
c
c  Input
c    c1,2       Pointers to frequencies to use for attempt to remove
c               ambiguities.
c    noerr      True if there were no input position angle error images.
c    n          Number of frequencies.
c    lmbdsq     Wavelength squared (m**2).
c    pa         Position angles (radians).
c    wt         Weights for each frequency.
c    guess      Use rm0 as first guess instead.
c    ambig      Do not remove ambiguites.
c    rm0        Initial amount of RM to remove from data before fitting.
c  Output
c    rmf        Fitted rotation measure (rad/m**2).
c    pa0f       Fitted position angle at zero wavelength (radians).
c    erm        Error in RM.
c    epa0       Error in PA0.
c    q          Goodness of fit.
c    ypl        Data for plotting.
c  Input/output
c    pa2        Scratch array.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   i
      real      rmi
c-----------------------------------------------------------------------
      if (ambig) then
c       Remove initial angle for initial estimate of RM.
        if (rm0.ne.0.0) call rmsub(n, lmbdsq, pa, rm0)

c       Put position angles into +/- pi/2.
        do i = 1, n
          call pm90(pa(i))
        enddo

c       Fit.
        call lsf(noerr, n, lmbdsq, pa, wt, rmf, pa0f, erm, epa0,
     *            chisq, q)

c       Assign final value of rotation measure.  Note that zero
c       wavelength position angle is unaffected.
        rmf = rmf + rm0
      else

c       Save original position angles.
        do i = 1, n
          pa2(i) = pa(i)
        enddo

c       Do first fit; remove initial estimate, compute RM from
c       c1 and c2 frequencies, remove N*PI ambiguities, do
c       least squares fir to get rotation measure.
        rmi = rm0
        call rfit(guess, c1, c2, noerr, n, lmbdsq, pa2, wt, rmi,
     *             rmf, pa0f, erm, epa0, q)

c       Now redo with the new estimate of the rotation measure.  This
c       time, we always subtract the angle from for the first estimate
c       of the RM (i.e. guess = .false.).
        if (guess) then
          rmi = rmf
        else
          rmi = rmi + rmf
        endif
        call rfit(.false., c1, c2, noerr, n, lmbdsq, pa, wt, rmi,
     *             rmf, pa0f, erm, epa0, q)

c       Assign final value of rotation measure.  Note that zero
c       wavelength position angle is unaffected.
        rmf = rmf + rmi
      endif

c     Save data for plotting.  We want to plot the data that was finally
c     fitted, but with the angle subtracted for the initial RM estimate
c     added back in.
      if (ambig) rmi = rm0
      do i = 1, n
        ypl(i) = (pa(i) + rmi*lmbdsq(i)) * DR2D
      enddo

      end
