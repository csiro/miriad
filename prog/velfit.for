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
c$Id: velfit.for,v 1.2 2010/10/11 04:39:50 cal103 Exp $
c--
c  History:
c    30sep92  mchw  New task for Miriad.
c    29aug02  pjt   Added frang= to prevent large divisions for models
c    31aug02  pjt   Ieck, rms calculation wrong, arrays not reset to 0
c-----------------------------------------------------------------------
      include 'maxdim.h'

      character*(*) label,version
      parameter (label='Fit a rotation curve to elliptical annuli')
      parameter (version='version 1.0 31-aug-02')

      double precision PI,RTS
      parameter (PI=3.141592654,RTS=3600d0*180d0/PI)

      integer MAXNAX,MAXBOXES,MAXRUNS,NAXIS
      parameter (MAXNAX=3,MAXBOXES=2048,NAXIS=3)
      parameter (MAXRUNS=3*MAXDIM)

      integer boxes(MAXBOXES)
      integer nsize(MAXNAX),size(MAXNAX),blc(MAXNAX),trc(MAXNAX)
      integer irmin,irmax,nmap,map,i,j,ir,lIn(2)
      real crpix(MAXNAX),cdelt(MAXNAX),crval(MAXNAX),crpix1,cdelt1
      real center(2),pa,incline,rmin,rmax,rstep,vsys,crval1,vr,tmp
      real cospa,sinpa,cosi,sini,cost,wt,x,y,xt,yt,r,ave,rms,fsum
      real pixe(MAXDIM),amp(MAXDIM),flux(MAXDIM),vel(MAXDIM)
      real frang,costmin
      real vsum(MAXDIM),vsqu(MAXDIM),wsum(MAXDIM),vrot(MAXDIM)
      logical mask(MAXDIM)
      character*80 in(2),log,line
      character cin*1,ctype*9

c     Externals.
      integer len1
      character*1 itoaf
c-----------------------------------------------------------------------
c     Get inputs.
      call output('VELFIT: '//version)
      call keyini
      call mkeyf('in',in,2,nmap)
      call boxInput('region',in,boxes,MAXBOXES)
      call keyr('center',center(1),0.0)
      call keyr('center',center(2),0.0)
      call keyr('pa',pa,0.0)
      call keyr('incline',incline,0.0)
      call keyr('radius',rmin,0.0)
      call keyr('radius',rmax,0.0)
      call keyr('radius',rstep,0.0)
      call keyr('frang',frang,0.0)
      call keyr('vsys',vsys,0.0)
      call keya('log',log,' ')
      call keyfin

c     Check inputs.
      if (nmap.lt.2) call bug('f','Must have two input maps')
      costmin = sin(frang*PI/180.0)

c     Get center and pixel sizes from first image.
      call xyopen(lIn(1),in(1),'old',NAXIS,size)
      if (size(1).gt.MAXDIM .or. size(2).gt.MAXDIM)
     *  call bug('f','Image too big for MAXDIM')
      do i = 1, 2
        cin = itoaf(i)
        call rdhda(lIn(1),'ctype'//cin,ctype,' ')
        if (ctype(1:2).ne.'RA' .and. ctype(1:3).ne.'DEC')
     *    call bug('w','Axes 1 and 2 are not RA or DEC')
        call rdhdr(lIn(1),'crpix'//cin,crpix(i),0.0)
        if (crpix(i).eq.0) then
          crpix(i) = nsize(i)/2+1
          call bug('w','Center pixel missing - assume NAXIS/2+1')
        endif
        call rdhdr(lIn(1),'cdelt'//cin,cdelt(i),1.0)
        if (cdelt(i).eq.0) call bug('f','Pixel size missing')
        call rdhdr(lIn(1),'crval'//cin,crval(i),0.0)
      enddo

c     Open the second input map and check axis match.
      map = 2
      call xyopen(lIn(map),in(map),'old',NAXIS,nsize)
      if (nsize(1).ne.size(1) .or. nsize(2).ne.size(2))
     *  call bug('f','Each map must have same xy dimensions')

      do i = 1, 2
        cin = itoaf(i)
        call rdhdr(lIn(map),'cdelt'//cin,cdelt1,1.0)
        if (cdelt1.ne.cdelt(i)) call bug('w','cdelt not the same')
        call rdhdr(lIn(map),'crpix'//cin,crpix1,0.0)
        call rdhdr(lIn(map),'crval'//cin,crval1,0.0)
        if ((crval1+(1-crpix1)*cdelt1).ne.
     *      (crval(i)+(1-crpix(i))*cdelt(i)))
     *         call bug('w','reference positions not the same')
      enddo

c     Set up the region of interest.
      call boxMask(lIn(1),boxes,MAXBOXES)
      call boxSet(boxes,MAXNAX,nsize,'s')
      call boxInfo(boxes,MAXNAX,blc,trc)

c     Open the output text file and write title.
      call logOpen(log,'q')
      call logWrit(' ***** '//label//' *****')
      call logWrit(' Intensity Image = '//In(1)(1:len1(in(1))))
      call logWrit(' Velocity  Image = '//In(2)(1:len1(in(2))))
      write(line,'(a,2f7.1,a,f5.0,a,f4.0,a,f8.0)')
     *  '  center: ',center,'  Ellipse pa: ',pa,
     *        '  inclination: ',incline,' Vsys: ',vsys
      call logWrit(line)
      write(line,'(a,2f7.1)') '  free angle: ',frang
      call logWrit(line)

c     Convert the inputs to more useful numbers, and defaults.
      cdelt(1) = cdelt(1)*RTS
      cdelt(2) = cdelt(2)*RTS
      if (rmin.eq.0.0) rmin = abs(0.1*cdelt(1))
      if (rmax.eq.0.0) rmax = abs(nsize(1)*cdelt(1))
      if (rstep.eq.0.0) rstep = abs(cdelt(1))
      cospa = cos(pa*PI/180.0)
      sinpa = sin(pa*PI/180.0)
      cosi  = cos(incline*PI/180.0)
      sini  = sin(incline*PI/180.0)

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
          x = (i-crpix(1))*cdelt(1) - center(1)
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
          x = (i-crpix(1))*cdelt(1) - center(1)
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
