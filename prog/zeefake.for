      program zeefake

c= zeefake - Generate fake Zeeman data.
c& nebk
c: profile analysis
c+
c       ZEEFAKE is a MIRIAD program to generate fake I and V cubes with
c       Zeeman splitting.  Zero velocity is put at the centre of the
c       band and image.  The cubes are output in VXY order ready for the
c       ZEESTAT program.
c
c@ iout
c       The output Stokes I cube. No default.
c@ iuout
c       The output Stokes I cube in the case of no splitting.  Default
c       is not to write this cube out.
c@ vout
c       The output Stokes V cube.  No default.
c@ imsize
c       The output cube sizes, all three dimensions required (VXY).
c@ vinc
c       The velocity increment along the cubes in Km/s.  No default.
c@ delv
c       The Zeeman splitting (separation of split lines) in km/s.
c       B < 0 if DELV > 0.  No default.
c@ fwhm
c       The FWHM of the Gaussian line profile in km/s.  No default.
c@ grvc
c       The velocity increment of the line centre in the x-direction
c       per pixel (km/s).  Makes a linear ramp across the source.
c@ grint
c       Intensity increment in the x direction per pixel.  Makes
c       a triangular weighting function.  Peak response is 1.0.
c@ grfwhm
c       FWHM increment in the x direction per pixel.  Makes
c       a linear ramp across the source.
c@ grsplit
c       Splitting increment in the x direction.  Makes a linear ramp
c       across the source.
c@ theta
c       The angle of the magnetic field to the line-of-sight in degrees.
c       No default.
c@ noise
c       The RMS noise to be added to the RR and LL responses.  Note that
c       the peak RR or LL response in this program is 1.0 for theta=0.
c@ restfreq
c       Line rest frequency in GHz.
c@ type
c       The line type - "e" for emission (default), "a" for absorption.
c
c$Id: zeefake.for,v 1.4 2010/09/20 02:50:42 cal103 Exp $
c--
c
c     nebk  May    1989
c     rl    Jun 12 1989   Changed so that each spectrum has different
c                         noise for the given r.m.s.
c     nebk  Jul 29 1990   Add emission/absorption
c     nebk  Aug 3  1990   Add velocity, intensity, width, and B
c                         gradients.
c     nebk  Nov 1  1990   Add eta hat determination
c     nebk  Dec 11 1990   Fix possible divide by zero.
c     nebk  Aug 07 1992   Add Stokes axis to output images
c     nebk  Nov 26 1992   Add btype to output
c     nebk  Nov 7  1995   Allow signed splitting
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      real EXPLIM
      parameter (EXPLIM = -35)

      logical   noline
      integer   axLen(4), i, j, k, luni, luniu, lunv
      real      alpha, arg, cfacm, cfacp, delv, delvx, etahat, fsplit,
     *          fwhm, gfac, gfac1, grfwhm, grint, grsplit, grvc,
     *          idata(MAXDIM), iscale, iudata(MAXDIM), ll, lln(MAXDIM),
     *          phix, restfreq, rms, rr, rrn(MAXDIM), rtheta, scale,
     *          sfac, tfac, theta, v, vdata(MAXDIM), vel, velx, vfac,
     *          vfacm, vfacp, vinc, vrefp, vrefv, vrms, vsumsq
      character iout*64, iuout*64, line*80, type*1, vout*64, version*72

      character versan*80
      external  versan
c-----------------------------------------------------------------------
      version = versan('zeefake',
     *                 'Revision',
     *                 'Date')

c     Get inputs.
      call keyini
      call keya ('iout', iout, ' ')
      call keya ('iuout', iuout, ' ')
      call keya ('vout', vout, ' ')
      if (iout.eq.' ' .or. vout.eq.' ')
     *  call bug ('f', 'You must specify both IOUT and VOUT')

      call keyi ('imsize', axLen(1), 64)
      call keyi ('imsize', axLen(2), 16)
      call keyi ('imsize', axLen(3), axLen(2))
      if (axLen(1).le.0 .or. axLen(1).gt.MAXDIM .or. axLen(2).le.0 .or.
     *    axLen(3).le.0) call bug ('f', 'Invalid image size')
      axLen(4) = 1

      call keyr ('vinc', vinc, 0.0)
      if (vinc.eq.0.0) call bug ('f', 'Channel increment not given')
      vinc = -abs(vinc)

      call keyr ('delv', delv, 0.0)
      if (delv.eq.0.0) call bug ('w', 'Splitting is 0')

      call keyr ('fwhm', fwhm, 0.0)
      if (fwhm.eq.0.0) call bug ('f', 'FWHM of line not given')

      call keyr ('grvc', grvc, 0.0)
      call keyr ('grint', grint, 0.0)
      call keyr ('grfwhm', grfwhm, 0.0)
      call keyr ('grsplit', grsplit, 0.0)

      call keyr ('theta', theta, 0.0)
      call keyr ('noise', rms, 0.0)
      call keyr ('restfreq', restfreq, 0.0)

      call keya ('type', type, ' ')
      if (type.ne.'a' .and. type.ne.'A') type = 'e'

      call keyfin

c     Create output images, write header and history.
      call xyopen(luni, iout, 'new', 4, axLen)
      call mkHead(luni, axLen, vinc, 'I', restfreq, version)

      call xyopen(lunv, vout, 'new', 4, axLen)
      call mkHead(lunv, axLen, vinc, 'V', restfreq, version)

      if (iuout.ne.' ') then
        call xyopen(luniu, iuout, 'new', 3, axLen)
        call mkHead(luniu, axLen, vinc, 'I', restfreq, version)
      endif

c     Compute unchanging factors.
      call rdhdr (luni, 'crpix1', vrefp, 0.0)
      call rdhdr (luni, 'crval1', vrefv, 0.0)
      rtheta = theta * pi / 180.0
      cfacm  = (cos(rtheta) - 1.0)**2
      cfacp  = (cos(rtheta) + 1.0)**2
      gfac1  = -(8.0 * log(2.0)) / 2.0
      sfac   = 2.0 * sin(rtheta)**2
      delv = delv / 2.0
      if (type.eq.'e' .or. type.eq.'E') then
        tfac = 1.0
      else
        tfac = -1.0
      endif

c     Compute B and tell user.
      call ZedScale (lunI, restfreq, scale, noline)
      if (noline) call bug ('f', 'Did not recognize line')
      fsplit = -2.0 * delv / abs(vinc) * abs(scale)

      write (line, 20) fsplit
20    format ('The magnetic field = ', 1pe12.5, ' Gauss')
      call output (line)

c     Work out eta_hat for zero gradient, zero noise line.
      vsumsq = 0.0
      gfac = gfac1 / (fwhm * fwhm)
      do i = 1, axLen(1)
        vel = vrefv + (i-vrefp)*vinc

        vfacm = exp(gfac * (vel - delv)**2)
        vfacp = exp(gfac * (vel + delv)**2)
        vfac  = exp(gfac * (vel)**2)

        rr = 0.25*(cfacm*vfacm + cfacp*vfacp + sfac*vfac)
        ll = 0.25*(cfacp*vfacm + cfacm*vfacp + sfac*vfac)
        v = (rr - ll) / 2.0
        vsumsq = vsumsq + v**2
      enddo
      alpha = delv / vinc
      vrms = sqrt(vsumsq/axLen(1))
      if (alpha*rms.ne.0.0) then
        etahat = sqrt(2.0) * vrms / (alpha * rms)
      else
        etahat = 99999.0
      endif

      call output (' ')
      call output ('For noiseless and gradientless line')
      write (*,*) 'Channel splitting alpha = ', alpha
      write (*,*) 'Noise level sig_I =       ', rms/sqrt(2.0)
      write (*,*) 'eta_hat =                 ', etahat
      call output (' ')

c     Fill images with spectra.
      do k = 1, axLen(3)
        call xysetpl (luni, 1, k)
        call xysetpl (lunv, 1, k)
        if (iuout.ne.' ') call xysetpl (luniu, 1, k)

        do j = 1, axLen(2)
c         Linear ramp for splitting.
          delvx = grsplit*(j-axLen(2)/2) + delv

c         Linear ramp in x for velocity. Line centre at velx.
c         Put to 0 at centre of image.
          velx = real(j-axLen(2)/2) * grvc

c         Linear gradient in x for intensity.  1.0 at image centre.
c         Makes triangular weighting function.
          if (j.le.axLen(2)/2) then
            iscale = grint*(j-axLen(2)/2) + 1.0
          else
            iscale = grint*(axLen(2)/2-j) + 1.0
          endif
          if (iscale.lt.0.0) iscale = 0.0
          iscale = iscale * tfac

c         Linear gradient in x of FWHM, user-given value is at the
c         centre of the image.  Arbitrary value chosen if it drops
c         below zero.
          phix = grfwhm*(j-axLen(2)/2) + fwhm
          if (phix.le.0.0) phix = 0.1 * fwhm
          gfac = gfac1 / (phix * phix)

c         Compute a band of RR and LL noises.
          if (rms.ne.0.0) then
            call gaus (rrn, axLen(1))
            call gaus (lln, axLen(1))
          else
            do i = 1, axLen(1)
              rrn(i) = 0.0
              lln(i) = 0.0
            enddo
          endif

c         Compute the spectrum.
          do i = 1, axLen(1)
c           Work out velocity factors for the Gaussian with channel
c           location.
            vel = vrefv + (i-vrefp)*vinc
            arg = gfac * (vel - delvx - velx)**2
            if (arg.lt.EXPLIM) then
              vfacm = 0.0
            else
              vfacm = exp(arg)
            endif

            arg = gfac * (vel + delvx - velx)**2
            if (arg.lt.EXPLIM) then
              vfacp = 0.0
            else
              vfacp = exp(arg)
            endif

            arg = gfac * (vel - velx)**2
            if (arg.lt.EXPLIM) then
              vfac = 0.0
            else
              vfac = exp(arg)
            endif

c           The factor of 0.25 makes the maximum possible RR and LL
c           response = 1.  Make sure don't scale noise by ISCALE.
            rr = iscale*0.25*(cfacm*vfacm + cfacp*vfacp +
     *                        sfac*vfac) + rms*rrn(i)
            ll = iscale*0.25*(cfacp*vfacm + cfacm*vfacp +
     *                        sfac*vfac) + rms*lln(i)

            idata(i)  = (rr + ll) / 2.0
            iudata(i) = iscale*vfac + rms*(rrn(i)+lln(i))/2.0
            vdata(i)  = (rr - ll) / 2.0
          enddo
          call xywrite (luni, j, idata)
          call xywrite (lunv, j, vdata)
          if (iuout.ne.' ') call xywrite (luniu, j, iudata)
        enddo
      enddo

      call xyclose (luni)
      call xyclose (lunv)
      if (iuout.ne.' ') call xyclose (luniu)

      end

c***********************************************************************

      subroutine mkHead (lun, axLen, vinc, stokes, restfreq, version)

      integer   lun, axLen(4)
      real      vinc, restfreq
      character stokes*1, version*72
c-----------------------------------------------------------------------
c  Write header for images.
c-----------------------------------------------------------------------
      call wrhdd(lun, 'crpix1', dble(axLen(1)/2+1))
      call wrhdd(lun, 'crpix2', dble(axLen(2)/2+1))
      call wrhdd(lun, 'crpix3', dble(axLen(3)/2+1))
      call wrhdd(lun, 'crpix4', 1d0)

      call wrhdd(lun, 'cdelt1', dble(vinc))
      call wrhdd(lun, 'cdelt2', -4.848136d-6)
      call wrhdd(lun, 'cdelt3',  4.848136d-6)
      call wrhdd(lun, 'cdelt4',  1d0)

      call wrhdd(lun, 'crval1', 0d0)
      call wrhdd(lun, 'crval2', 0d0)
      call wrhdd(lun, 'crval3', 0d0)
      call wrhdd(lun, 'crval4', dble(index(stokes,'IQUV')))

      call wrhda(lun, 'ctype1', 'VELO')
      call wrhda(lun, 'ctype2', 'RA---SIN')
      call wrhda(lun, 'ctype3', 'DEC--SIN')
      call wrhda(lun, 'ctype4', 'STOKES')

      call wrbtype(lun,'intensity')
      call wrhda(lun, 'bunit',  'JY/PIXEL')
      call wrhdr(lun, 'restfreq', restfreq)

      call hisopen (lun, 'append')
      call hiswrite(lun, 'ZEEFAKE: Miriad' // version)
      call hisinput(lun, 'ZEEFAKE')
      call hisclose(lun)

      end
