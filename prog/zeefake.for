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
c$Id: zeefake.for,v 1.3 2010/09/20 02:08:31 cal103 Exp $
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
      real explim
      parameter (explim = -35)
cc
      real idata(maxdim), iudata(maxdim), vdata(maxdim), vinc, delv,
     *phi, theta, rms, rrn(maxdim), lln(maxdim), restfreq, tfac,
     *grint, grvc, grfwhm, grsplit
      integer siz(4)
      character iout*64, vout*64, iuout*64, type*1

      real rtheta, vrefv, vrefp, gfac, cfacp, cfacm, sfac, v,
     *vfacp, vfacm, vfac, rr, ll, vel, fsplit, arg, velx, iscale,
     *phix, gfac1, delvx, vsumsq, vrms, etahat, alpha, scale
      integer luni, luniu, lunv, i, j, k
      character line*80
      logical noline
c-----------------------------------------------------------------------
      call output ('Zeefake: Version 26-Nov-92')
c
c     Get the input parameters.
c
      call keyini
      call keya ('iout', iout, ' ')
      call keya ('iuout', iuout, ' ')
      call keya ('vout', vout, ' ')
      if (iout.eq.' ' .or. vout.eq.' ')
     *  call bug ('f', 'You must specify both IOUT and VOUT')

      call keyi ('imsize', siz(1), 64)
      call keyi ('imsize', siz(2), 16)
      call keyi ('imsize', siz(3), siz(2))
      if (siz(1).le.0 .or. siz(1).gt.maxdim .or. siz(2).le.0 .or.
     *    siz(3).le.0) call bug ('f', 'Invalid image size')
      siz(4) = 1

      call keyr ('vinc', vinc, 0.0)
      if (vinc.eq.0.0) call bug ('f', 'Channel increment not given')
      vinc = -abs(vinc)

      call keyr ('delv', delv, 0.0)
      if (delv.eq.0.0) call bug ('w', 'Splitting is 0')

      call keyr ('fwhm', phi, 0.0)
      if (phi.eq.0.0) call bug ('f', 'FWHM of line not given')

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
c
c     Create output images, write header and history
c
      call xyopen  (luni, iout, 'new', 4, siz)
      call header  (luni, siz, vinc, restfreq, 'I')
      call history (luni, siz, vinc, delv, phi, theta, restfreq, rms)

      call xyopen  (lunv, vout, 'new', 4, siz)
      call header  (lunv, siz, vinc, restfreq, 'V')
      call history (lunv, siz, vinc, delv, phi, theta, restfreq, rms)

      if (iuout.ne.' ') then
        call xyopen  (luniu, iuout, 'new', 3, siz)
        call header  (luniu, siz, vinc, restfreq, 'I')
        call history (luniu, siz, vinc, 0.0, phi, theta, restfreq, rms)
      endif
c
c     Compute unchanging factors
c
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
c
c     Compute B and tell user
c
      call ZedScale (lunI, restfreq, scale, noline)
      if (noline) call bug ('f', 'Did not recognize line')
      fsplit = -2.0 * delv / abs(vinc) * abs(scale)

      write (line, 20) fsplit
20    format ('The magnetic field = ', 1pe12.5, ' Gauss')
      call output (line)
c
c Work out eta_hat for gradientless noiseless line
c
      vsumsq = 0.0
      gfac = gfac1 / (phi * phi)
      do i = 1, siz(1)
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
      vrms = sqrt(vsumsq/siz(1))
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
c
c     Fill images with spectra
c
      do k = 1, siz(3)
        call xysetpl (luni, 1, k)
        call xysetpl (lunv, 1, k)
        if (iuout.ne.' ') call xysetpl (luniu, 1, k)

        do j = 1, siz(2)
c
c         Linear ramp for splitting
c
          delvx = grsplit*(j-siz(2)/2) + delv
c
c         Linear ramp in x for velocity. Line centre at velx.
c         Put to 0 at centre of image
c
          velx = real(j-siz(2)/2) * grvc
c
c         Linear gradient in x for intensity.  1.0 at image centre.
c         Makes triangular weighting function.
c
          if (j.le.siz(2)/2) then
            iscale = grint*(j-siz(2)/2) + 1.0
          else
            iscale = grint*(siz(2)/2-j) + 1.0
          endif
          if (iscale.lt.0.0) iscale = 0.0
          iscale = iscale * tfac
c
c         Linear gradient in x of FWHM,  user given value
c         is at the centre of the image.  Arbitrary value
c         chosen if it drops below zero
c
          phix = grfwhm*(j-siz(2)/2) + phi
          if (phix.le.0.0) phix = 0.1 * phi
          gfac = gfac1 / (phix * phix)
c
c         Compute a band of RR and LL noises
c
          if (rms.ne.0.0) then
            call gaus (rrn, siz(1))
            call gaus (lln, siz(1))
          else
            do i = 1, siz(1)
              rrn(i) = 0.0
              lln(i) = 0.0
            enddo
          endif
c
c     Compute the spectrum
c
          do i = 1, siz(1)
c
c           Now work out velocity factors for the Gaussian with
c           channel location
c
            vel = vrefv + (i-vrefp)*vinc
            arg = gfac * (vel - delvx - velx)**2
            if (arg.lt.explim) then
              vfacm = 0.0
            else
              vfacm = exp(arg)
            endif

            arg = gfac * (vel + delvx - velx)**2
            if (arg.lt.explim) then
              vfacp = 0.0
            else
              vfacp = exp(arg)
            endif

            arg = gfac * (vel - velx)**2
            if (arg.lt.explim) then
              vfac = 0.0
            else
              vfac = exp(arg)
            endif
c
c           Factor of 0.25 makes the maximum possible RR and LL
c           response = 1.  Make sure don't scale noise by ISCALE.
c
            rr = iscale*0.25*(cfacm*vfacm + cfacp*vfacp +
     *                        sfac*vfac) + rms*rrn(i)
            ll = iscale*0.25*(cfacp*vfacm + cfacm*vfacp +
     *                        sfac*vfac) + rms*lln(i)

            idata(i) = (rr + ll) / 2.0
            iudata(i) = iscale*vfac + rms*(rrn(i)+lln(i))/2.0
            vdata(i) = (rr - ll) / 2.0
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

      subroutine header (lun, siz, vinc, restfreq, stokes)
c-----------------------------------------------------------------------
c     Write header for images
c-----------------------------------------------------------------------
      real vinc, restfreq, rval
      integer lun, siz(4)
      character stokes*1
c-----------------------------------------------------------------------
      call wrhda (lun, 'bunit', 'JY/PIXEL')
      call wrhda (lun, 'ctype1', 'VELO')
      call wrhda (lun, 'ctype2', 'RA---SIN')
      call wrhda (lun, 'ctype3', 'DEC--SIN')
      call wrhda (lun, 'ctype4', 'STOKES')
      call wrhdr (lun, 'cdelt1', vinc)
      call wrhdr (lun, 'cdelt2', -4.848136e-6)
      call wrhdr (lun, 'cdelt3',  4.848136e-6)
      call wrhdr (lun, 'cdelt4',  1.0)
      call wrhdr (lun, 'crpix1', real(siz(1)/2)+1)
      call wrhdr (lun, 'crpix2', real(siz(2)/2)+1)
      call wrhdr (lun, 'crpix3', real(siz(3)/2)+1)
      call wrhdr (lun, 'crpix4', 1.0)
      call wrhdr (lun, 'crval1', 0.0)
      call wrhdr (lun, 'crval2', 0.0)
      call wrhdr (lun, 'crval3', 0.0)
      rval = 1.0
      if (stokes.eq.'V') rval = 4.0
      call wrhdr (lun, 'crval4', rval)
      call wrhdr (lun, 'restfreq', restfreq)
      call wrbtype (lun,'intensity')

      end

c***********************************************************************

      subroutine history (lun, siz, vinc, delv, phi, theta, rf, rms)
c-----------------------------------------------------------------------
c     Write history for an output image
c-----------------------------------------------------------------------
      integer lun, siz(3)
      real vinc, delv, phi, theta, rf, rms
cc
      character aline*80
c-----------------------------------------------------------------------
      call hisopen (lun, 'append')

      call hiswrite (lun, 'ZEEFAKE: (MIRIAD)')
      write (aline, 10) siz(1), siz(2), siz(3)
10    format ('ZEEFAKE: output size = ', i4, ',', i4, ',', i4)
      call hiswrite (lun, aline)
      write (aline, 20) vinc
20    format ('ZEEFAKE: velocity increment = ', 1pe12.4, ' km/s')
      call hiswrite (lun, aline)
      write (aline, 30) delv
30    format ('ZEEFAKE: zeeman splitting = ', 1pe12.4, ' km/s')
      call hiswrite (lun, aline)
      write (aline, 40) phi, theta
40    format ('ZEEFAKE: line fwhm = ', 1pe12.4, ' km/s, theta = ',
     *         1pe12.4, ' degrees')
      call hiswrite (lun, aline)
      write (aline, 50) rms
50    format ('ZEEFAKE: rms noise for RR and LL = ', 1pe12.4)
      call hiswrite (lun, aline)
      write (aline, 60) rf
60    format ('ZEEFAKE: restfreq = ', 1pe12.4, ' GHz')
      call hiswrite (lun, aline)

      call hisclose (lun)

      end
