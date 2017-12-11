      program velimage

c= VELIMAGE - Make (x,y,z) image from an (x,y) image and model z-values.
c& mchw
c: image analysis
c+
c       VELIMAGE is a MIRIAD task to make an (x,y,z) image from an (x,y)
c       image and a model for the z-values and dispersion.  The
c       z-values, for example the mean velocity, are input as an (x,y)
c       image, whilst the dispersion can be input as either an (x,y)
c       image or a fixed value.  The output image is formed as
c
c          out(x,y,z) = in1(x,y) * exp(-(z-in2(x,y))**2/(2*sigma**2)))
c
c       VELIMAGE can be used to compare models with the data.  For
c       example one can generate a model image corresponding to a
c       rotating galaxy, convolve it with the actual beam and subtract
c       the model from the data to determine if there is residual
c       emission which deviates from the model rotation curve.
c@ in
c       The input image names, separated by commas.  The first image is
c       the (x,y) intensity distribution integrated over the z-axis,
c       e.g. velocity-integrated image.  The second image is an (x,y)
c       model for the z-values, e.g. a mean velocity image.  No default
c       for either.
c       A 3rd input image gives the z-dispersion at each point, e.g. a
c       velocity dispersion image.  If the 3rd image is not specified
c       then fixed dispersion, sigma must be specified.
c@ region
c       Region of image to be used.  See documentation on region for
c       details.  All pixels within the bounding box are used; pixel
c       masking is not used.  Default is the entire image.
c@ sigma
c       Fixed value for z-dispersion if not specified by 3rd input
c       image.
c@ nchan
c       Number of channels for z-axis of output image.  Default=1.
c@ start
c       Starting value for z-axis of output image.  No default.
c@ step
c       Interval for z-axis of output image.  No default.
c@ out
c       The output (x,y,z) image.  No default.
c@ options
c       Options.  Minimum match is active.
c         relax  ignore axis descriptor mismatches
c                (e.g. pixel increments etc).  Use with care.
c
c$Id: velimage.for,v 1.7 2017/12/11 22:26:28 wie017 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer   MAXBOXES, MAXRUNS
      parameter (MAXBOXES=2048, MAXRUNS=3*MAXDIM)

      logical   relax
      integer   blc(MAXNAX), boxes(MAXBOXES), i, iax, j, k, lIn(3),
     *          lOut, iMap, nchan, nMap, axLen(MAXNAX), axLen2(MAXNAX),
     *          trc(MAXNAX)
      real      amp(MAXDIM), buf(MAXDIM), sig(MAXDIM), sigma, start,
     *          step, v, val(MAXDIM)
      double precision cdelt(MAXNAX), cdelti, crpix(MAXNAX), crpixi,
     *          crval(MAXNAX), crvali, discr
      character cax*1, ctype(MAXNAX)*16, ctypei*16, inName(3)*80,
     *          outNam*80, wflag*1, version*72

      character itoaf*1, versan*72
      external  itoaf, versan
c-----------------------------------------------------------------------
      version = versan('velimage',
     *                 '$Revision: 1.7 $',
     *                 '$Date: 2017/12/11 22:26:28 $')

c     Get the input parameters.
      call keyini
      call mkeyf('in',inName,3,nMap)
      call keyr('sigma',sigma,0.0)
      call keyi('nchan',nchan,1)
      call keyr('start',start,0.0)
      call keyr('step', step, 0.0)
      call keya('out',outNam,' ')
      call getopt(relax)
      call BoxInput('region',inName,boxes,MAXBOXES)
      call keyfin

c     Check the inputs.
      if (nMap.lt.2) call bug('f','Must have at least two input maps')
      if (outNam.eq.' ') call bug('f','output image file not given')
      if (nMap.eq.2 .and. sigma.eq.0.0) call bug('f',
     *    'Either z-dispersion image or sigma must be specified')
      if (start.eq.0.0) call bug('f','No start given for z-axis')
      if (step.eq.0.0) call bug('f','No step given for z-axis')
      if (relax) then
        wflag = 'w'
        call bug('i','Axis descriptor mismatches will be tolerated')
      else
        wflag = 'f'
      endif

c     Open the input maps and check conformance.
      call xyopen(lIn(1), inName(1), 'old', MAXNAX, axLen)
      if (axLen(1).gt.MAXDIM .or. axLen(2).gt.MAXDIM)
     *  call bug('f', 'Image too big for MAXDIM')

      do i = 1, MAXNAX
        cax = itoaf(i)
        call rdhdd(lIn(1), 'crpix'//cax, crpix(i), 0d0)
        call rdhdd(lIn(1), 'cdelt'//cax, cdelt(i), 1d0)
        call rdhdd(lIn(1), 'crval'//cax, crval(i), 0d0)
        call rdhda(lIn(1), 'ctype'//cax, ctype(i), ' ')
      enddo

      do iMap = 2, nMap
        call xyopen(lIn(iMap), inName(iMap), 'old', MAXNAX, axLen2)
        if (axLen2(1).ne.axLen(1) .or. axLen2(2).ne.axLen(2))
     *    call bug('f', 'Each map must have the same dimensions.')

        do iax = 1, MAXNAX
          cax = itoaf(iax)

          call rdhdd(lIn(iMap), 'crpix'//cax, crpixi, 0d0)
          if (crpixi.ne.crpix(iax))
     *      call bug(wflag, 'crpix differs between input maps.')

          discr = abs(1d-2 * cdelt(iax))
          call rdhdd(lIn(iMap), 'cdelt'//cax, cdelti, 1d0)
          if (abs(cdelti-cdelt(iax)).gt.discr)
     *      call bug(wflag, 'cdelt differs between input maps.')

          call rdhdd(lIn(iMap), 'crval'//cax, crvali, 0d0)
          if (abs(crvali-crval(iax)).gt.discr)
     *      call bug(wflag, 'crval differs between input maps.')

          call rdhda(lIn(iMap), 'ctype'//cax, ctypei, ' ')
          if (ctypei.ne.ctype(iax))
     *      call bug(wflag, 'ctype differs between input maps.')
        enddo
      enddo

c     Set up the region of interest.
      call boxSet(boxes, MAXNAX, axLen, 's')
      call boxInfo(boxes, MAXNAX, blc, trc)

c     Open the output image and write its header.
      axLen(1) = trc(1)-blc(1)+1
      axLen(2) = trc(2)-blc(2)+1
      axLen(3) = nchan
      call xyopen(lOut, outNam, 'new', 3, axLen)
      call headcp(lIn, lOut, 2, 0, blc, trc)
      call wrhdd(lOut, 'crpix3', 1d0)
      call wrhdd(lOut, 'cdelt3', dble(step))
      call wrhdd(lOut, 'crval3', dble(start))
      call wrhda(lOut, 'ctype3', 'VRAD')
c      call wrhda(lOut, 'bunit',  'KM/S')

c     Generate the output image.
      do k = 1, nchan
        v = start + (k-1)*step
        call xysetpl(lOut,1,k)

        do j = blc(2), trc(2)
          call xyread(lIn(1),j,amp)
          call xyread(lIn(2),j,val)
          if (nMap.eq.3) call xyread(lIn(3),j,sig)

          do i = blc(1), trc(1)
            if (nMap.eq.3) sigma = sig(i)
            if (abs(v-val(i)).lt.5.0*sigma) then
              buf(i) = amp(i) * exp(-(v-val(i))**2 / (2.0*sigma**2))
            else
              buf(i) = 0.0
            endif
          enddo

          call xywrite(lOut,j-blc(2)+1,buf(blc(1)))
        enddo
      enddo

c     Update history and close files.
      call hisopen (lOut, 'append')
      call hisWrite(lOut, 'VELIMAGE: Miriad ' // version)
      call hisInput(lOut, 'VELIMAGE')
      call hisClose(lOut)
      call xyclose(lOut)
      do i = 1, nMap
        call xyclose(lIn(i))
      enddo

      end

c***********************************************************************

      subroutine getopt(relax)

      logical relax
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error.
c-----------------------------------------------------------------------
      integer MAXOPT
      parameter (MAXOPT = 1)

      logical   present(MAXOPT)
      character ops(MAXOPT)*5

      data ops /'relax'/
c-----------------------------------------------------------------------
      call options('options', ops, present, MAXOPT)
      relax = present(1)

      end
