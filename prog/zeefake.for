      program zeefake
c-----------------------------------------------------------------------
c     ZEEFAKE is a MIRIAD program to generate fake I and V cubes in order
c     to test the ZEEMAP program.   Zero velocity is put at the 
c     centre of the band and image. The cubes are output in VXY order
c     ready for the ZEE* programs.
c
c= zeefake - Generate fake Zeeman data.
c& nebk
c: profile analysis
c@ iout
c	The output Stokes I cube. No default.
c@ iuout
c	The output Stokes I cube in the case of no splitting.  Default
c	is not to write this cube out.
c@ vout
c	The output Stokes V cube. No default.
c@ imsize
c	The output cube sizes, all three dimensions required (VXY)
c@ vinc
c	The velocity increment along the cubes in Km/s. No default.
c@ delv
c	The Zeeman splitting (separation of split lines) in Km/s. No default.
c@ fwhm
c	The FWHM of the Gaussian line profile in Km/s. No default.
c@ grvc
c	The velocity increment of the line centre in the x-direction 
c	per pixel (km/s).  Makes a linear ramp across the source.
c@ grint
c	Intensity increment in the x direction per pixel. Makes
c	a triangular weighting function.  Peak response is 1.0
c@ grfwhm
c	FWHM increment in the x direction per pixel.  Makes
c	a linear ramp across the source.
c@ grsplit
c	Splitting increment in the x direction.  Makes a linear ramp
c	across the source.
c@ theta
c	The angle of the magnetic field to the l-o-s in degrees. No default.
c@ noise
c	The r.m.s. noise to be added to the RR and LL responses. Note
c	that the peak RR or LL response in this program is 1.0 for theta=0
c@ restfreq
c	The rest frequency of your line in GHz.
c@ type
c	The line type; "e" for emission (default) "a" for absorption
c--
c
c     Inputs:
c       iout       Output stokes I cube
c       iuout      Output stokes I cube when no splitting
c       vout       Output stokes V cube
c       imsize     Output image sizes
c       vinc       Channel increment (km/s) of cubes
c       delv       Amount of splitting (separation of split lines) in km/s
c       fwhm       FWHM of line (assumed gaussian) in Km/s
c       grvc       Increment of line velocity centre per pixel (km/s)
c       grint      Increment of intensity per pixel.   Makes triangular
c                  weighting function.
c       grfwhm     Increment of FWHM per pixel
c       grspl      Increment of splitting channels per pixel
c       theta      Angle of B field to line of sight (degrees)
c       noise      Rms of noise in RR or LL (Peak RR or LL response is 1.0)
c       restfreq   Restfrequency of line in GHz
c       type       "e" for emission "a" for absorption
c
c     nebk  May    1989
c     rl    Jun 12 1989   Changed so that each spectrum has different 
c                         noise for the given r.m.s.
c     nebk  Jul 29 1990   Add emission/absorption
c     nebk  Aug 3  1990   Add velocity, intensity, width, and B gradients 
c     nebk  Nov 1  1990   Add eta hat determination
c     nebk  Dec 11 1990   Fix possible divide by zero.
c     nebk  Aug 07 1992   Add Stokes axis to output images
c     nebk  Nov 26 1992   Add btype to output
c----------------------------------------------------------------------------
      implicit none
c
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
c
      real rtheta, vrefv, vrefp, gfac, cfacp, cfacm, sfac, v,
     *vfacp, vfacm, vfac, rr, ll, vel, fsplit, arg, velx, iscale,
     *phix, gfac1, phisq, delvx, vsumsq, vrms, etahat, alpha
      integer luni, luniu, lunv, i, j, k
      character line*80
c
c     Define Zeeman splitting (i.e. separation between the split lines)
c     for different lines in Hz/Gauss
c
      integer nfreq
      parameter (nfreq = 4)
c
      integer ifreq(nfreq)
      real zsplit(nfreq)
      data ifreq  / 1420,      1665,    1667,      1720  /
      data zsplit /0.3571E6, 3.2787E6, 1.9608E6, 0.6536E6/
c-------------------------------------------------------------------------
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
c
      call keyi ('imsize', siz(1), 64)
      call keyi ('imsize', siz(2), 16)
      call keyi ('imsize', siz(3), siz(2))
      if (siz(1).le.0 .or. siz(1).gt.maxdim .or. siz(2).le.0 .or. 
     *    siz(3).le.0) call bug ('f', 'Invalid image size')
      siz(4) = 1
c
      call keyr ('vinc', vinc, 0.0)
      if (vinc.eq.0.0) call bug ('f', 'Channel increment not given')
      vinc = -abs(vinc)
c
      call keyr ('delv', delv, 0.0)
      if (delv.eq.0.0) call bug ('w', 'Splitting is 0')
c
      call keyr ('fwhm', phi, 0.0)
      if (phi.eq.0.0) call bug ('f', 'FWHM of line not given')
c
      call keyr ('grvc', grvc, 0.0)
      call keyr ('grint', grint, 0.0)
      call keyr ('grfwhm', grfwhm, 0.0)
      call keyr ('grsplit', grsplit, 0.0)
c
      call keyr ('theta', theta, 0.0)
      call keyr ('noise', rms, 0.0)
      call keyr ('restfreq', restfreq, 0.0)
c
      call keya ('type', type, ' ')
      if (type.ne.'a' .and. type.ne.'A') type = 'e'
c
      call keyfin
c
c     Create output images, write header and history
c
      call xyopen  (luni, iout, 'new', 4, siz)
      call header  (luni, siz, vinc, restfreq, 'I')
      call history (luni, siz, vinc, delv, phi, theta, restfreq, rms)
c
      call xyopen  (lunv, vout, 'new', 4, siz)
      call header  (lunv, siz, vinc, restfreq, 'V')
      call history (lunv, siz, vinc, delv, phi, theta, restfreq, rms)
c 
      if (iuout.ne.' ') then
        call xyopen  (luniu, iuout, 'new', 3, siz)
        call header  (luniu, siz, vinc, restfreq, 'I')
        call history (luniu, siz, vinc, 0.0, phi, theta, restfreq, rms)
      end if
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
      phisq = phi * phi
      delv = abs(delv) / 2.0
      if (type.eq.'e' .or. type.eq.'E') then
         tfac = 1.0
      else
         tfac = -1.0
      end if
c
c     Compute splitting in Hz and maybe B and tell user
c
      fsplit = 2.0 * delv * restfreq * 1.0e9 / 2.9979e5
      write (line, 10) fsplit
10    format ('The split lines are separated by ', 1pe12.5, ' Hz')
      call output (line)
c
      do j = 1, nfreq
        if(nint(restfreq*1000).eq.ifreq(j)) then
           write (line, 20) fsplit / zsplit(j)
20         format ('The magnetic field = ', 1pe12.5, ' Gauss')
           call output (line)
           goto 30
        end if
      end do
c
c Work out eta_hat for gradientless noiseless line
c
30    vsumsq = 0.0
      gfac = gfac1 / (phi * phi) 
      do i = 1, siz(1)
         vel = vrefv + (i-vrefp)*vinc
c
         vfacm = exp(gfac * (vel - delv)**2)
         vfacp = exp(gfac * (vel + delv)**2)
         vfac  = exp(gfac * (vel)**2)
c
         rr = 0.25*(cfacm*vfacm + cfacp*vfacp + sfac*vfac)
         ll = 0.25*(cfacp*vfacm + cfacm*vfacp + sfac*vfac)
         v = (rr - ll) / 2.0
         vsumsq = vsumsq + v**2
      end do
      alpha = abs(delv / vinc)
      vrms = sqrt(vsumsq/siz(1))
      if (alpha*rms.ne.0.0) then
         etahat = sqrt(2.0) * vrms / (alpha * rms)
      else
         etahat = 99999.0
      end if
c
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
c
         do j = 1, siz(2)
c
c          Linear ramp for splitting
c
            delvx = grsplit*(j-siz(2)/2) + delv
c
c          Linear ramp in x for velocity. Line centre at velx. 
c          Put to 0 at centre of image
c
            velx = real(j-siz(2)/2) * grvc 
c
c          Linear gradient in x for intensity.  1.0 at image centre.
c          Makes triangular weighting function.
c
            if (j.le.siz(2)/2) then
              iscale = grint*(j-siz(2)/2) + 1.0
            else
              iscale = grint*(siz(2)/2-j) + 1.0
            end if
            if (iscale.lt.0.0) iscale = 0.0
            iscale = iscale * tfac
c
c        Linear gradient in x of FWHM,  user given value
c        is at the centre of the image.  Arbitrary value
c        chosen if it drops below zero
c
            phix = grfwhm*(j-siz(2)/2) + phi
            if (phix.le.0.0) phix = 0.1 * phi
            gfac = gfac1 / (phix * phix) 
c
c     Compute a band of RR and LL noises
c
            if (rms.ne.0.0) then
               call gaus (rrn, siz(1))
               call gaus (lln, siz(1))
            else
               do i = 1, siz(1)
                 rrn(i) = 0.0
                 lln(i) = 0.0
               end do
            end if
c
c     Compute the spectrum 
c
            do i = 1, siz(1)
c
c             Now work out velocity factors for the Gaussian with
c             channel location
c
              vel = vrefv + (i-vrefp)*vinc
              arg = gfac * (vel - delvx - velx)**2
              if (arg.lt.explim) then
                 vfacm = 0.0
              else
                 vfacm = exp(arg)
              end if
c
              arg = gfac * (vel + delvx - velx)**2
              if (arg.lt.explim) then
                 vfacp = 0.0
              else
                 vfacp = exp(arg)
              end if
c
              arg = gfac * (vel - velx)**2
              if (arg.lt.explim) then
                 vfac = 0.0
              else
                 vfac = exp(arg)
              end if
c
c        Factor of 0.25 makes the maximum possible RR and LL response = 1
c        Make sure don't scale noise by ISCALE
c
              rr = iscale*0.25*(cfacm*vfacm + cfacp*vfacp +
     *                          sfac*vfac) + rms*rrn(i)
              ll = iscale*0.25*(cfacp*vfacm + cfacm*vfacp +
     *                          sfac*vfac) + rms*lln(i)
c
              idata(i) = (rr + ll) / 2.0
              iudata(i) = iscale*vfac + rms*(rrn(i)+lln(i))/2.0
              vdata(i) = (rr - ll) / 2.0
            end do
            call xywrite (luni, j, idata)
            call xywrite (lunv, j, vdata)
            if (iuout.ne.' ') call xywrite (luniu, j, iudata)
         end do
      end do
c
      call xyclose (luni)
      call xyclose (lunv)
      if (iuout.ne.' ') call xyclose (luniu)
c
      end
c
c
      subroutine header (lun, siz, vinc, restfreq, stokes)
c----------------------------------------------------------------------
c     Write header for images
c----------------------------------------------------------------------
      implicit none
c
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
c
      end
      subroutine history (lun, siz, vinc, delv, phi, theta, rf, rms)
c----------------------------------------------------------------------
c     Write history for an output image
c----------------------------------------------------------------------
      implicit none
c
      integer lun, siz(3)
      real vinc, delv, phi, theta, rf, rms
cc
      character aline*80
c---------------------------------------------------------------------
      call hisopen (lun, 'append')
c
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
c
      call hisclose (lun)
c
      end
