      program velfit

c= VELFIT - Fit a theoretical velocity pattern to isovelocity image.
c& mchw
c: image analysis
c+
c       VELFIT fits a theoretical velocity pattern to a MIRIAD
c       isovelocity image, weighted by the intensity image and a
c       geometric factor specified by model input paramaters.
c       The default option is to fit a rotation curve to an isovelocity
c       image of a rotating disk.  The rotation curve and rms for the
c       fit are printed out and can be used to find the best fit to the
c       other parameters.  For more details see paper by Warner, Wright
c       and Baldwin, 1973, MNRAS, 163,163.
c@ in
c       The input image names, separated by commas.  The first image is
c       the (x,y) intensity distribution integrated over the z-axis,
c       e.g. velocity-integrated image.  The second image is an (x,y)
c       model for the z-values, e.g. a mean velocity image.
c       No default for either.
c@ region
c       Region of image to be used in the fit.  See documentation on
c       region for details.  Only the bounding box is supported.
c       The default region is the entire image.
c@ center
c       The center of the annuli in arcsec from the center pixel,
c       measured in the directions of RA and DEC.
c       Default: mapcenter as defined by crpix.
c@ pa
c       Position angle of ellipse major axis in degrees.
c       Default is 0 (north).
c@ incline
c       Inclination angle in degrees.
c       Default=0 (face on).
c@ radius
c       Inner and outer radii and step size along major axis in arcsecs.
c       The default is the whole image in steps equal to the pixel size.
c@ vsys
c       Center value z-axis.  E.g. systemic velocity for rotation curve.
c       Default: 0
c@ frang
c       Free angle around the minor axis of points to ignore.  The total
c       angle will be 2*frang.  (1/cos(theta) problem)
c       Default: 0
c@ log
c       The output log file.  The default is the terminal.
c@ options
c       None yet.  Reserved for alternative models.
c
c--
c  History:
c    30sep92  mchw  New task for Miriad.
c    29aug02  pjt   Added frang= to prevent large divisions for models
c    31aug02  pjt   Ieck, rms calculation wrong, arrays not reset to 0
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      character*(*) label
      parameter (label='Fit a rotation curve to elliptical annuli')

      integer   MAXNAX, MAXBOXES, MAXRUNS, NAXIS
      parameter (MAXNAX=3,MAXBOXES=2048,NAXIS=3)
      parameter (MAXRUNS=3*MAXDIM)

      logical   mask(MAXDIM)
      integer   axLen1(MAXNAX), axLen2(MAXNAX), blc(MAXNAX),
     *          boxes(MAXBOXES), i, iax, ir, irmax, irmin, j, lIn(2),
     *          nmap, trc(MAXNAX)
      real      amp(MAXDIM), ave, center(2), cosi, cospa, cost, costmin,
     *          flux(MAXDIM), frang, fsum, incline, pa, pixe(MAXDIM), r,
     *          rmax, rmin, rms, rstep, sini, sinpa, tmp, vel(MAXDIM),
     *          vr, vrot(MAXDIM), vsqu(MAXDIM), vsum(MAXDIM), vsys,
     *          wsum(MAXDIM), wt, x, xt, y, yt
      double precision cdelt(2), cdelti, crpix(2), crpixi, crval(2),
     *          crvali
      character cax*1, ctype(2)*9, ctypei*9, inName(2)*80, line*80,
     *          logNam*80, version*72

      integer   len1
      character itoaf*1, versan*72
      external  itoaf, len1, versan
c-----------------------------------------------------------------------
      version = versan('velfit')

c     Get inputs.
      call keyini
      call mkeyf('in',inName,2,nmap)
      call boxInput('region',inName,boxes,MAXBOXES)
      call keyr('center',center(1),0.0)
      call keyr('center',center(2),0.0)
      call keyr('pa',pa,0.0)
      call keyr('incline',incline,0.0)
      call keyr('radius',rmin,0.0)
      call keyr('radius',rmax,0.0)
      call keyr('radius',rstep,0.0)
      call keyr('frang',frang,0.0)
      call keyr('vsys',vsys,0.0)
      call keya('log',logNam,' ')
      call keyfin

c     Check inputs.
      if (nmap.lt.2) call bug('f','Must have two input maps')
      costmin = sin(frang*D2R)

c     Open the input maps and check conformance.
      call xyopen(lIn(1), inName(1), 'old', NAXIS, axLen1)
      if (axLen1(1).gt.MAXDIM .or. axLen1(2).gt.MAXDIM)
     *  call bug('f','Image too big for MAXDIM')

      call xyopen(lIn(2),inName(2),'old',NAXIS,axLen2)
      if (axLen2(1).ne.axLen1(1) .or. axLen2(2).ne.axLen1(2))
     *  call bug('f','Each map must have same dimensions')

      do iax = 1, 2
        cax = itoaf(iax)

        call rdhdd(lIn(1), 'crpix'//cax, crpix(iax), 0d0)
        call rdhdd(lIn(2), 'crpix'//cax, crpixi, dble(axLen2(iax)/2+1))
        if (crpix(iax).eq.0d0) then
          crpix(iax) = dble(axLen1(iax)/2+1)
          call bug('w', 'Center pixel missing - assume NAXIS/2+1')
        endif
        if (crpixi.ne.crpix(iax))
     *    call bug('w', 'crpix differs between input maps.')

        call rdhdd(lIn(1), 'cdelt'//cax, cdelt(iax), 0d0)
        call rdhdd(lIn(2), 'cdelt'//cax, cdelti, 0d0)
        if (cdelt(iax).eq.0d0)
     *    call bug('f', 'cdelt is absent or zero.')
        if (cdelti.ne.cdelt(iax))
     *    call bug('w', 'cdelt differs between input maps.')

        call rdhdd(lIn(1), 'crval'//cax, crval(iax), 0d0)
        call rdhdd(lIn(2), 'crval'//cax, crvali, 0d0)
        if (crvali.ne.crval(iax))
     *    call bug('w', 'crval differs between input maps.')

        call rdhda(lIn(1), 'ctype'//cax, ctype(iax), ' ')
        call rdhda(lIn(2), 'ctype'//cax, ctypei,   ' ')
        if (ctype(iax)(1:2).ne.'RA' .and. ctype(iax)(1:3).ne.'DEC')
     *    call bug('w', 'Axes 1 and 2 are not RA and DEC')
        if (ctypei.ne.ctype(iax))
     *    call bug('w', 'ctype differs between input maps.')
      enddo

c     Set up the region of interest.
      call boxMask(lIn(1),boxes,MAXBOXES)
      call boxSet(boxes,MAXNAX,axLen2,'s')
      call boxInfo(boxes,MAXNAX,blc,trc)

c     Open the output text file and write title.
      call logOpen(logNam,'q')
      call logWrit(' ***** '//label//' *****')
      call logWrit(' Intensity image = '//inName(1)(1:len1(inName(1))))
      call logWrit(' Velocity  image = '//inName(2)(1:len1(inName(2))))
      write(line,'(a,2f7.1,a,f5.0,a,f4.0,a,f8.0)')
     *  '  center: ',center,'  Ellipse pa: ',pa,
     *        '  inclination: ',incline,' Vsys: ',vsys
      call logWrit(line)
      write(line,'(a,2f7.1)') '  free angle: ',frang
      call logWrit(line)

c     Convert the inputs to more useful numbers, and defaults.
      cdelt(1) = cdelt(1)*R2AS
      cdelt(2) = cdelt(2)*R2AS
      if (rmin.eq.0.0)  rmin  = abs(0.1*cdelt(1))
      if (rmax.eq.0.0)  rmax  = abs(axLen2(1)*cdelt(1))
      if (rstep.eq.0.0) rstep = abs(cdelt(1))
      cospa = cos(pa*D2R)
      sinpa = sin(pa*D2R)
      cosi  = cos(incline*D2R)
      sini  = sin(incline*D2R)

c     Initialize integrals for each axis.
      do ir = 1, MAXDIM
        pixe(ir) = 0.0
        flux(ir) = 0.0
        vsum(ir) = 0.0
        vsqu(ir) = 0.0
        wsum(ir) = 0.0
      enddo
      irmin = rmax/rstep
      irmax = 0

c     Integrate in elliptical annuli.
      do j = blc(2), trc(2)
        call xyread(lIn(1),j,amp)
        call xyread(lIn(2),j,vel)
        call xyflgrd(lIn(1),j,mask)
        y = (j-crpix(2))*cdelt(2) - center(2)

        do i = blc(1), trc(1)
          x  = (i-crpix(1))*cdelt(1) - center(1)
          yt =  x*sinpa + y*cospa
          xt = (x*cospa - y*sinpa)/cosi
          r  = sqrt(xt*xt+yt*yt)
          if (r.ge.rmin .and. r.le.rmax .and. mask(i)) then
            ir = r/rstep+1.5
            cost = yt/r
            vr = (vel(i)-vsys)/cost/sini
            wt = amp(i)*abs(cost)
            if (abs(cost).ge.costmin) then
              pixe(ir) = pixe(ir) + 1.0
              flux(ir) = flux(ir) + amp(i)
              vsum(ir) = vsum(ir) + wt*vr
              vsqu(ir) = vsqu(ir) + wt*vr*vr
              wsum(ir) = wsum(ir) + wt
              irmin = min(ir,irmin)
              irmax = max(ir,irmax)
            endif
          endif
        enddo
      enddo

c     Find the rotation curve and reset some arrays for rms calc.
      do ir = irmin, irmax
        if (wsum(ir).ne.0.0) then
          vrot(ir) = vsum(ir)/wsum(ir)
        else
          vrot(ir) = 0.0
        endif
      enddo

      do ir = 1, MAXDIM
        vsum(ir) = 0.0
        vsqu(ir) = 0.0
        wsum(ir) = 0.0
      enddo

c     Find the rms residuals from the fitted rotation curve.
      do j = blc(2), trc(2)
        call xyread(lIn(1),j,amp)
        call xyread(lIn(2),j,vel)
        call xyflgrd(lIn(1),j,mask)
        y = (j-crpix(2))*cdelt(2) - center(2)

        do i = blc(1), trc(1)
          x  = (i-crpix(1))*cdelt(1) - center(1)
          yt =  x*sinpa + y*cospa
          xt = (x*cospa - y*sinpa)/cosi
          r  = sqrt(xt*xt+yt*yt)
          if (r.ge.rmin .and. r.le.rmax .and. mask(i)) then
            ir = r/rstep+1.5
            cost = yt/r
            if (abs(cost).ge.costmin) then
              vr = vel(i)-vsys-vrot(ir)*cost*sini
              wt = amp(i)*abs(cost)
              vsum(ir) = vsum(ir) + wt*vr
              vsqu(ir) = vsqu(ir) + wt*vr*vr
              wsum(ir) = wsum(ir) + wt
            endif
          endif
        enddo
      enddo

c     Write out the results.
      call logWrit(' ')
      write(line,'(a,a,a,a,a,a)') '  Radius(") ',' # Pixels   ',
     *   '  intensity ','     fit    ','     rms    ','  total rms '
      call logWrit(line)

c     Find averages for each annulus.
      fsum = 0.0
      do ir = irmin, irmax
        r = (ir-1)*rstep
        if (wsum(ir).ne.0.0) then
          tmp = flux(ir)/pixe(ir)
          ave = vsum(ir)/wsum(ir)
          rms = vsqu(ir)/wsum(ir)-ave*ave
          if (rms.lt.0) rms=0.0
          rms = sqrt(rms)
        else
          tmp = 0.0
          rms = 0.0
        endif
        fsum = fsum + rms*rms
        write(line,'(6f12.4)')
     *         r,pixe(ir),tmp,vrot(ir),rms,sqrt(fsum/(ir-irmin+1.0))
        call logWrit(line)
      enddo

c     All done.
      call xyclose(lIn(1))
      call xyclose(lIn(2))
      call logClose

      end
