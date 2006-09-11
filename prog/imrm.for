      program imrm
c-----------------------------------------------------------------------
c= IMRM - Compute rotation measure image from position angle images
c& nebk
c: image analysis
c+
c	IMRM computes  rotation measure and zero wavelength position
c	angle images from at least 2 position angle images at 
c	different frequencies.   This is done via a linear least
c	squares fit to:
c
c                       PA = PA_0 + RM*LAMBDA**2
c
c	where RM is the rotation measure (rad/m**2) and PA_0 is the 
c	position angle at zero wavelength.  The output rotation
c	measure image is in rad/m**2, and the output position 
c	angle image is in degrees.  Optionally, plots of the fits
c 	can be made.
c
c	The more frequencies you have the better.  It is very important
c	to try and get at least two sufficiently close that there is
c	no ambiguity between them.
c
c
c	By default, IMRM attempts to remove n*pi ambiguities from the
c	data.  Its algorithm is (pixel by pixel)
c
c	 0) First remove angle according to the amount given by the
c	   user (keyword "rmi") and the equation PA = RM*LAMBDA**2
c
c	 1) Put the position angles of the first two frequencies 
c	   in the range +/- 90 degrees.
c
c	 2) Remove 180 degree ambiguity from the position angles given
c	   by the FIRST TWO IMAGES (keyword "in").  Thus, it modifies
c	   the position angle of the second frequency by 180 degrees 
c	   so that the absolute value of the angle between the 
c	   two position angles is less than 90 degrees.
c
c	 3) Compute the initial RM and PA_0 from these FIRST
c	   TWO position angles.
c	
c	 4) This RM and PA_0 is used to predict the expected position
c	   angle at the other frequencies according to the expression
c	   PA = PA_0 + RM*LAMBDA**2.  Integer amounts of 180 degrees
c	   are then added or subtracted to the position angles at the
c	   remaining frequencies in order to make the position angle
c	   as close as possible to the expected value.
c
c	 5) Then a least squares fit is used to solve for the RM and PA_0
c	
c	 6) Finally, the procedure is repeated from step 0) where the
c	  initial guess is now the value just determined above in
c	  step 5).   
c
c	The order in which the images are given is thus very important.
c	You should generally give your images in order of decreasing 
c	frequency, with the assumption being that the smallest angle
c	between the first two represents a rough guess for the RM
c	with no ambiguities.  However, if you are very certain abou
c	the lack of ambiguity between certain frequencies, or there 
c	are some of particularly high S/N and likely lack of ambiguity,
c	you may like to try these.  Its a nasty business and it is VERY
c	important that you look at the results carefully.
c
c	The attempt to remove ambiguities can be turned off with
c	keyword "options=ambiguous".  In this case, its algorithm is
c
c	 0) First remove angle according to the intial guess given
c	   by the user (keyword "rmi").
c
c        1) Put all position angles in the range +/- 90 degrees
c
c	 2) Then a least squares fit is used to solve for the RM and PA_0
c	
c	In principle, you should never need to use this option.
c	If there are no ambiguities, the first algorithm shouldn't
c	find any !
c
c	There are also a variety of methods offered with which to blank the 
c	output images.  Most of these require error images associated with
c	the input position angle images. Use the program IMPOL to make 
c	the position angle images and position angle error images.
c
c@ in
c	Up to 5 input position angle (positive N -> E) images 
c	(in degrees) at different frequencies.  Generally, you should
c	give the images in order of decreasing frequency.
c	Wild card expansion is supported, no default.
c@ inerr
c	Up to 5 position angle error images (in degrees) used for 
c	weighting the data during the least squares fit.  They are 
c	assumed to be in one-to-one association with the position 
c	angle images. If no error images are given, each position 
c	angle image is given equal weight and we must assume a goodness
c	of fit of unity in order to find the output image errors.
c	Wild card expansion is supported, default is no error images.
c@ rmi
c	An amount of rotation measure to remove from the data before fitting.
c	If you have a good idea of this, it helps enormously in removing 
c	ambiguities. See the detailed use in the discussion of the algorithm
c	above.  See also options=guess where it is used slightly differently.
c	Default is 0
c@ rm
c	Two values. The output fitted rotation measure image in 
c	rad/m**2, and optionally, its associated error image.
c	The default is no output RM images.
c@ pa0
c	The output fitted (at zero wavelength) position angle image 
c	in degrees, and optionally, its associated error image.
c	The default is no output PA images.
c@ qcut
c	Blank the output image (RM or PA) pixels if the goodness of fit
c	(Q) is less than this value.  If Q is larger than about 0.1 say,
c	the fit is believable.  If it is greater than 0.001, the fit 
c	may be acceptable if the errors are non-normal or too small. If
c	Q is less than 0.001 the model can be called into question.  The
c	probability distribution for position angle images approximates
c	a Gaussian at high S/N ratios.  At low S/N ratios (roughly, when
c	P/sigma < 2) it is non-Gaussian.  If you don't specify error 
c	images, Q cannot be determined and is assumed to be one.  This is 
c	also true if you give IMRM position angle images at two 
c	frequencies only.
c	Default is 0.001
c@ errcut
c	Blank the output image (RM or PA) pixels if ANY of the input PA 
c	image pixels has an error greater than this value (degrees).
c	Default is no input error based blanking.
c@ rmcut
c	Blank pixels in BOTH the output RM and PA_0 images when the error 
c	in the fitted RM is greater than this value (rad/m**2).
c	Errors can be worked out if you give input error images,
c	or if you input images at more than two frequencies AND we
c	assume the goodness of fit is unity.
c	Default is no fitted RM error based blanking.
c@ pacut
c	Blank pixels in BOTH the output RM and PA_0 images when the
c	error in the fitted PA_0 is greater than this value (degrees).
c	Errors can be worked out if you give input error images,
c	or if you input images at more than two frequencies AND we
c	assume the goodness of fit is unity.
c	Default is no fitted PA_0 error based blanking.
c@ device
c	PGPLOT plotting device to see the fits to the data.  The absolute 
c	pixel numbers in x and y are also written into the corner of the 
c	plot (unless options=accumulate).
c	
c	No default.
c@ nxy
c	Number of subplots per page in the x and y directions, to put 
c	on the plotting device.  See options=accumulate
c	The default is 10x10
c@ csize
c	PGPLOT character height.
c	Default is 1.0
c@ options
c	Task enrichment options.  Minimum match is active,
c
c	"relax"      issue warnings instead of a fatal error when image
c	             axis descriptors are inconsistent with each other,
c		     and when the input image headers do not indicate that
c		     they are position angle images (btype=position_angle)
c	"guess"      when removing ambiguities, this option causes IMRM to
c		     use the rotation measure input through the keyword 
c		     "rmi" in step 3 above (on the first pass only), rather
c		     than working it out from the first two frequencies. By 
c		     default, angle is removed from the data according to 
c		     the value of "rmi" and then the first guess made from 
c		     the first two frequencies.  The angle is not removed 
c		     in this way with this option.  This may prove useful if 
c		     you have two close but perhaps noisy frequencies which 
c		     is causing the initial guess of the RM to be wrong 
c		     (because of noise) and driving the subsequent turn 
c		     removal off.
c	"ambiguous"  Do not try to remove ambiguites. 
c	"accumulate" means put all the plots on one sub-plot, rather than
c	 	     the default, which is to put the plot for each 
c		     spatial pixel on a spearate subplot
c	"yindependent"
c		     By default, the sub-plots are all drawn with the same
c		     Y-axis scale, that embraces all sub-plots.  This option
c		     forces each sub-plot to be scaled independently.
c--
c  History:
c    nebk 22may92   Original version.
c    nebk 26may92   Try to deal with ambiguities.
c    nebk 27may92   Improve warnings to users
c    nebk 04nov92   Reorganize and improve blanking, add output error
c		    images, rewrite lsf to include goodness of fit
c    mjs  12mar93   Use maxnax.h file instead of setting own value.
c    nebk 11nov93   Add options=ambiguous and output blanking info
c    nebk 30jan95   Work on ambiguity algorithm, add keywords "rmi",
c		    "device", "nxy", "csize", "options=acc,gues,yind"
c    nebk 28mar95   Trying to plot nowhere if device blank
c    nebk 21jun97   Was plotting garbage on some platforms if 
c                   some output points were blanked.  Also in hedinfo
c                   rdhdd was being called with default real arg.
c    rjs  02jul97   cellscal change.
c------------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
      include 'mem.h'
c
      double precision r2d
      integer maxim
      character version*40
      parameter (version = 'ImRM: version 21-Jun-97' )
      parameter (maxim = 10, r2d = 180.0d0/dpi)
cc
      real lsq(maxim), pa(maxim), pa2(maxim), wt(maxim)
c 
      double precision cdelt(maxnax,maxim), crval(maxnax,maxim),
     +  ecdelt(maxnax,maxim), ecrval(maxnax,maxim)
c
      real line(maxdim,maxim), eline(maxdim,maxim), rmline(maxdim),
     +  ermline(maxdim), paline(maxdim), epaline(maxdim), 
     +  epoch(maxim), eepoch(maxim), crpix(maxnax,maxim),
     +  freq(maxim), ecrpix(maxnax,maxim), diff, d2, qcut, errcut,
     +  rmcut, pacut, q, rmi, cs, padummy
c
      integer lin(maxim), lein(maxim), lrm(2), lpa(2),
     +  size(maxnax,maxim), esize(maxnax,maxim), naxis(maxim), 
     +  enaxis(maxim), fqax(maxim), efqax(maxim), frqax, nim, 
     +  neim, i, j, k, l, imin, jmin, nbl(6), blsum, left,
     +  psize, ipyd, ipyf, ippyd, ippyf, nx, ny
c
      character aline*100, in(maxim)*64, ein(maxim)*64, rmout*64, 
     +  ermout*64, paout*64, epaout*64, ctype(maxnax,maxim)*9,
     +  ectype(maxnax,maxim)*9, bflag, device*32
c
      logical flags(maxdim,maxim), eflags(maxdim,maxim), oflags(maxdim),
     +  relax, blank, oblank, doerrbl, dormbl, dopabl, noerr, 
     +  ambig, accum, yind, guess
c
      integer nkeys
      parameter (nkeys = 45)
      character keyw(nkeys)*8
c
      data keyw/     'cdelt1  ','cdelt2  ','cdelt3  ',
     +    'cdelt4  ','cdelt5  ','crota1  ','crota2  ','crota3  ',
     +	  'crota4  ','crota5  ','crpix1  ','crpix2  ','crpix3  ',
     +    'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     +    'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     +    'obstime ','epoch   ','history ','instrume','niters  ',
     +    'object  ','restfreq','telescop','vobs    ','obsra   ',
     +    'obsdec  ','observer','cellscal ','bmaj    ',
     +    'bmin    ','bpa     ','pbfwhm  ','lstart  ','lstep   ',
     +    'ltype   ','lwidth  ','vobs    '/
      data nbl /6*0/
      data padummy /-100000.0/
c-------------------------------------------------------------------------
      call output (version)
      call output (' ')
      call output ('The order of the input position angle images is ')
      call output ('now IMPORTANT.  See the HELP file for details of ')
      call output ('the algorithm which has been improved')
      call output ('Plots are now available too')
      call output (' ')
c
c  Get the inputs
c
      call keyini
c
      call mkeyf ('in', in, maxim, nim)
      if (nim.lt.2) call bug ('f', 
     +  'You must specify at least two input images')
      call mkeyf ('inerr', ein, maxim, neim)
      if (neim.gt.0 .and. neim.ne.nim) call bug ('f',
     +  'You must give the same number of error as input PA images')
c
      call keya ('rm', rmout, ' ')
      call keya ('rm', ermout, ' ')
      call keyr ('rmi', rmi, 0.0)
      call keya ('pa0', paout, ' ')
      call keya ('pa0', epaout, ' ')
c      if (rmout.eq.' ' .and. paout.eq.' ') call bug ('f',
c     +  'You have not specified any output images')
c
      call keyr ('qcut', qcut, 0.001)
      call keyr ('errcut', errcut, -1.0)
      call keyr ('rmcut', rmcut, -1.0)
      call keyr ('pacut', pacut, -1.0)
      call keyr ('csize', cs, 0.0)
      call getopt (relax, ambig, accum, yind, guess)
      if (guess .and. ambig) call bug ('f',
     +  'Options=guess,ambiguous are mutually exclusive')
      call keya ('device', device, ' ')
      call keyi ('nxy', nx, 1)
      call keyi ('nxy', ny, nx)
c
      call keyfin
c
c Sort out inputs and give user some helpful (?) messages
c
      if (qcut.gt.0.0 .and. neim.le.2) then
        call bug ('w', 
     +   'You need at least three input error images to be able')
        call bug ('w', 
     +   'to compute the goodness of fit; setting q=1 & qcut=0.0')
        call output (' ')
        qcut = 0.0
      end if
      if (qcut.lt.0.0 .or. qcut.gt.1.0) call bug ('f',
     +  'Your goodness of fit blanking level is invalid')
c
      dormbl = .false.
      if (rmcut.gt.0.0) dormbl = .true.
c
      dopabl = .false.
      if (pacut.gt.0.0) dopabl = .true.
c
      doerrbl = .false.
      if (errcut.gt.0.0) doerrbl = .true.
c
      if (doerrbl .and. neim.eq.0) call bug ('f',
     +    'Input error blanking needs input error images')
c
      if ( (dormbl .or. dopabl) .and. (nim.le.2 .and. neim.eq.0) ) then
        call output 
     +  ('### Fatal error: Output error blanking requires input error')
        call output 
     +  ('### Fatal error: images or at least 3 input p.a. images and')
       call bug ('f', 
     +   'the assumption that the goodness of fit is unity')
      end if
      if ( (ermout.ne.' ' .or. epaout.ne.' ') .and.
     +     (nim.le.2 .and. neim.eq.0) ) then
        call output 
     +  ('### Fatal error: Output error images require input error')
        call output 
     +  ('### Fatal error: images or at least 3 input p.a. images and')
        call bug ('f', 
     +   'the assumption that the goodness of fit is unity')
      end if
      if ( (dormbl .or. dopabl  .or. ermout.ne.' ' .or.
     +      epaout.ne.' ') .and. (neim.eq.0 .and. nim.gt.2) ) then
        call bug ('w', 'The output errors will be calculated')
        call bug ('w', 'assuming the goodness of fit is unity')
        call output (' ')
      end if
c
      if (.not.dormbl.and..not.dopabl.and..not.doerrbl) then
        call bug ('w',
     +    'It is advised that some error based blanking is used')
        call output (' ')
      end if
c
      if (nim.eq.2) then
        call bug ('w', 
     +    'It must be assumed that there are no ambiguities')
        call bug ('w', 'with only two different frequencies')
        call output (' ')
      end if
      bflag = 'f'
      if (relax) bflag = 'w'
c
      if (accum) then
        nx = 1
        ny = 1
      else
        nx = max(1,nx)
        ny = max(1,ny)
      end if
c
c Indicate that we have input error images
c
      noerr = .true.
      if (neim.gt.0) noerr = .false.
c
c  Open the input images 
c

      do i = 1, nim
        call openin (maxdim, maxnax, bflag, in(i), lin(i), naxis(i), 
     +     size(1,i), epoch(i), crpix(1,i), cdelt(1,i), crval(1,i),
     +     ctype(1,i), fqax(i))
      end do
c
c Compare images for consistency
c
      do i = 1, nim-1
        do j = i+1, nim
          call chkdes (bflag, in(i), in(j), naxis(i), naxis(j), 
     +      size(1,i), size(1,j), crpix(1,i), crpix(1,j), cdelt(1,i),
     +      cdelt(1,j), crval(1,i), crval(1,j), epoch(i), epoch(j),
     +      ctype(1,i), ctype(1,j), fqax(i), fqax(j))
        end do
      end do
c
c  Open the error images 
c
      if (neim.gt.0) then
        do i = 1, neim
          call openin (maxdim, maxnax, bflag, ein(i), lein(i), 
     +       enaxis(i), esize(1,i), eepoch(i), ecrpix(1,i), ecdelt(1,i),
     +       ecrval(1,i), ectype(1,i), efqax(i))
        end do
c
c Compare error images for consistency
c
        do i = 1, neim-1
          do j = i+1, neim
            call chkdes (bflag, ein(i), ein(j), enaxis(i), enaxis(j),
     +        esize(1,i), esize(1,j), ecrpix(1,i), ecrpix(1,j), 
     +        ecdelt(1,i), ecdelt(1,j), ecrval(1,i), ecrval(1,j), 
     +        eepoch(i), eepoch(j), ectype(1,i), ectype(1,j), 
     +        efqax(i), efqax(j))
          end do
        end do
c
c Compare the first error image with the first position angle 
c image for consistency
c
        call chkdes (bflag, in, ein, naxis, enaxis, size, esize, 
     +        crpix, ecrpix, cdelt, ecdelt, crval, ecrval, epoch,
     +        eepoch, ctype, ectype, fqax, efqax)
      end if
c
c  Create the output images, copy the header keywords from the 
c  first input image and add the new history
c
      if (rmout.ne.' ') then
        call openout (lin(1), rmout, naxis(1), size(1,1), nkeys, 
     +                keyw, version, lrm(1))
        call wrhda (lrm, 'bunit', 'RAD/M/M')
        call wrbtype (lrm, 'rotation_measure')
      end if
      if (ermout.ne.' ') then
        call openout (lin(1), ermout, naxis(1), size(1,1), nkeys, 
     +                keyw, version, lrm(2))
        call wrhda (lrm, 'bunit', 'RAD/M/M')
        call wrbtype (lrm, 'rotation_measure')
      end if
      if (paout.ne.' ') then
        call openout (lin(1), paout, naxis(1), size(1,1), nkeys, 
     +                keyw, version, lpa(1))
        call wrhda (lpa, 'bunit', 'DEGREES')
        call wrbtype (lpa, 'position_angle')
      end if
      if (epaout.ne.' ') then
        call openout (lin(1), epaout, naxis(1), size(1,1), nkeys, 
     +                keyw, version, lpa(2))
        call wrhda (lpa, 'bunit', 'DEGREES')
        call wrbtype (lpa, 'position_angle')
      end if
c
c Find wavelength of each image (at pixel 1) in metres
c
      frqax = fqax(1)
      call output (' ')
      do i = 1, nim
        freq(i) = (1.0-crpix(frqax,i))*cdelt(frqax,i) + crval(frqax,i)
        lsq(i) = (dcmks / (freq(i) * 1.0e9))**2
        write (aline,10) freq(i)
10      format ('Found frequency ', f8.4, ' GHz')
        call output (aline)
      end do
c
c Warn user if degenerate frequencies
c
      diff = 1.0e30
      do i = 1, nim-1
        do j = i+1, nim
          d2 = abs(freq(j) - freq(i))
          if (d2.eq.0.0) call bug ('w',
     +      'There are degenerate frequencies amongst the PA images')
          if (d2.lt.diff) then
            diff = d2
            imin = i
            jmin = j
          end if
        end do
      end do
c
      if (.not.ambig) then
        write (aline,20) freq(imin), freq(jmin)
20      format ('Closest frequencies are ', f8.4, ' & ', f8.4, ' GHz')
c        call output (aline)
      end if
c
c Use first 2 images given by user so they have control.  Using closest
c frequencies is a lousy algorithm.
c
      imin = 1
      jmin = 2
c
c Allocate space for plots
c

      psize = nim*size(1,1)*size(2,1)
      call memalloc (ipyd, psize, 'r')  
      call memalloc (ipyf, psize, 'r')
      ippyd = ipyd
      ippyf = ipyf
c
c Compute results; assume 2-D images only
c
      do j = 1, size(2,1)
c
c Read lines from each image
c
        do k = 1, nim
          call xyread  (lin(k), j,  line(1,k))
          call xyflgrd (lin(k), j, flags(1,k))
          if (neim.gt.0) then
            call xyread  (lein(k), j,  eline(1,k))
            call xyflgrd (lein(k), j, eflags(1,k))
          end if
        end do
c
c Do fit for each image pixel. Do weighted fit if errors available.
c
        do i = 1, size(1,1)
          blank = .false.
          do k = 1, nim
c
c Do fit only if all the input and possibly input error image
c pixels are unblanked. 
c
            if ( (.not.flags(i,k)) .or. 
     +           (neim.gt.0 .and. .not.eflags(i,k))) then
              blank = .true.
              nbl(1) = nbl(1) + 1
              goto 100
            end if
c
c Do fit only if all the input error image pixels are smaller 
c than the specified cutoff
c 
            if (doerrbl .and. eline(i,k).gt.errcut) then
              blank = .true.
              nbl(2) = nbl(2) + 1
              goto 100
            end if
c
c Work out weights if error images given
c 
            pa(k) = line(i,k) / r2d
            wt(k) = 1.0
            if (.not.noerr) then
              if (eline(i,k).ne.0.0) then
                wt(k) = 1.0 / (eline(i,k)/r2d)**2
              else
                blank = .true.
                nbl(3) = nbl(3) + 1
                goto 100
              end if
            end if
          end do
c 
100       if (.not.blank) then
c 
c Do the fit if all the input criteria are satisified
c
            call rotfit (imin, jmin, noerr, nim, lsq, pa, pa2,
     +        wt, guess, ambig, rmi, rmline(i), paline(i), 
     +        ermline(i), epaline(i), q, memr(ippyd))
            ippyd = ippyd + nim
c
c Fill the fit into the plot buffer
c
            do l = 1, nim
              memr(ippyf) = (paline(i) + rmline(i)*lsq(l))*r2d
              ippyf = ippyf + 1
            end do
c
c Put output position angle between +/- 90 degrees
c
            call pm90 (paline(i))
            paline(i) = paline(i) * r2d
            epaline(i) = epaline(i) * r2d
c
c Ditch this pixel if output blanking criteria so dictate
c
            oflags(i) = .true.
            oblank = .false.
            if (q.lt.qcut) then
              nbl(4) = nbl(4) + 1
              oblank = .true.
            else if (dormbl .and. ermline(i).gt.rmcut) then
              nbl(5) = nbl(5) + 1
              oblank = .true.
            else if (dopabl .and. epaline(i).gt.pacut) then
              nbl(6) = nbl(6) + 1
              oblank = .true.
            end if
            if (oblank) call blnkall (oflags(i), rmline(i), paline(i),
     +        ermline(i), epaline(i))
          else
            call blnkall (oflags(i), rmline(i), paline(i),
     +                    ermline(i), epaline(i))
            do l = 1, nim
              memr(ippyf) = padummy
              memr(ippyd) = padummy
              ippyf = ippyf + 1
              ippyd = ippyd + 1
            end do
          end if
        end do
c
c Write out the results
c
        if (rmout.ne.' ') then
          call xywrite (lrm(1), j, rmline)
          call xyflgwr (lrm(1), j, oflags)
        end if
        if (ermout.ne.' ') then
          call xywrite (lrm(2), j, ermline)
          call xyflgwr (lrm(2), j,  oflags)
        end if
        if (paout.ne.' ') then
          call xywrite (lpa(1), j, paline)
          call xyflgwr (lpa(1), j, oflags)
        end if
        if (epaout.ne.' ') then
          call xywrite (lpa(2), j, epaline)
          call xyflgwr (lpa(2), j,  oflags)
        end if
      end do
c
c  Close up
c
      do k = 1, nim
	call xyclose (lin(k))
        if (neim.gt.0) call xyclose (lein(k))
      end do
      if (rmout.ne.' ')  call xyclose (lrm(1))
      if (ermout.ne.' ') call xyclose (lrm(2))
      if (paout.ne.' ')  call xyclose (lpa(1))
      if (epaout.ne.' ') call xyclose (lpa(2))
c
c Tell user about blanking
c
      call output (' ')
      if (nbl(1).gt.0) then
        write (aline, '(i8, a)') nbl(1),
     +    ' output pixels were blanked because so were input pixels'
        call output (aline)
      end if
      if (nbl(3).gt.0) then
        write (aline, '(i8, a)') nbl(3),
     +  ' output pixels were blanked because input error pixels were 0'
        call output (aline)
      end if
      if (nbl(2).gt.0) then
        write (aline, '(i8, a)') nbl(2),
     +    ' output pixels were blanked because of "ERRCUT"'
        call output (aline)
      end if
      if (nbl(4).gt.0) then
        write (aline, '(i8, a)') nbl(4),
     +    ' output pixels were blanked because of "QCUT"'
        call output (aline)
      end if
      if (nbl(5).gt.0) then
        write (aline, '(i8, a)') nbl(5),
     +    ' output pixels were blanked because of "RMCUT"'
        call output (aline)
      end if
      if (nbl(6).gt.0) then
        write (aline, '(i8, a)') nbl(6),
     +    ' output pixels were blanked because of "PACUT"'
        call output (aline)
      end if
c
      blsum = 0
      do i = 1, 6
        blsum = blsum + nbl(i)
      end do
      left = size(1,1)*size(2,1) - blsum
      write (aline, '(i8, a)') left, ' output pixels were unblanked'
      call output (aline)
      call output (' ')
c
c Plots
c
      if (device.ne.' ') call plotit (device, nx, ny, accum, yind,
     +  size(1,1), size(2,1), nim, lsq, memr(ipyd), memr(ipyf), cs,
     +  padummy)
      call memfree (ipyd, psize, 'r')
      call memfree (ipyf, psize, 'r')

c
      end
c
c
      subroutine blnkall (flag, rm, pa, erm, epa)
      implicit none
      real rm, pa, erm, epa
      logical flag
      flag = .false.
      rm = 0.0
      pa = 0.0
      erm = 0.0
      epa = 0.0
c
      end
c
c
      subroutine chkdes (bflag, im1, im2, naxis1, naxis2, size1, size2,
     +   crpix1, crpix2, cdelt1, cdelt2, crval1, crval2, epoch1, 
     +   epoch2, ctype1, ctype2, fqax1, fqax2)
c-----------------------------------------------------------------------
c     Compare axis descriptors 
c
c  Input:
c   im1,2        Images
c   naxis1,2     Number of axes
c   size1,2      Sizes of each dimension
c   crpix1,2     Reference pixels
c   cdelt1,2     Increments
c   crval1,2     Refernce values
c   ctype1,2     types of axes
c   epoch1,2     Epochs
c   fqax         Frequency axis
c-----------------------------------------------------------------------
      implicit none
c
      integer naxis1, naxis2, size1(*), size2(*), fqax1, fqax2
      character*(*) im1, im2, ctype1(*), ctype2(*), bflag
      double precision crval1(*), crval2(*), cdelt1(*), cdelt2(*)
      real crpix1(*), crpix2(*), epoch1, epoch2
cc
      integer k, l1, l2, len1
      character line*130
c-----------------------------------------------------------------------
      l1 = len1(im1)
      l2 = len1(im2)
c
      if (epoch1.ne.epoch2) then
        line = 'Unequal epochs for images '//im1(1:l1)//' & '//im2(1:l2)
        call bug (bflag, line)
      end if
c
      if (naxis1.ne.naxis2) then
        line = 'Unequal number dimensions for images '//
     +         im1(1:l1)//' & '//im2(1:l2)
        call bug (bflag, line)
      end if
c
      if (fqax1.ne.fqax2) then
        line = 'The frequency axis number are different for images '//
     +         im1(1:l1)//' & '//im2(1:l2)
        call bug ('f', line)
      end if
c
      do k = 1, min(naxis1,naxis2)
        if (size1(k).ne.size2(k)) then
          write (line, 10) im1(1:l1), im2(1:l2), k
10        format ('Unequal sizes for images ', a, ' & ', a, 
     +            ' on axis ', i1)
          call bug (bflag, line)
        end if
c
        if (ctype1(k).ne.ctype2(k)) then
          write (line, 20) im1(1:l1), im2(1:l2), k
20        format ('Unequal ctype for images ', a, ' & ', a, 
     +            ' on axis ', i1)
          call bug (bflag, line)
        end if
c
        call chkds2 (bflag, 'crpix', k, im1(1:l1), im2(1:l2), 
     +               crpix1(k), crpix2(k))
        call chkds2 (bflag, 'cdelt', k, im1(1:l1), im2(1:l2), 
     +               real(cdelt1(k)), real(cdelt2(k)))
        if (k.ne.fqax1) call chkds2 (bflag, 'crval', k, im1(1:l1), 
     +    im2(1:l2), real(crval1(k)), real(crval2(k)))
      end do
c
      end
c
c
      subroutine chkds2 (bflag, type, iaxis, im1, im2, des1, des2)
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
      implicit none
c
      character*(*) type, im1, im2, bflag
      integer iaxis
      real des1, des2
cc
      character line*130
c-----------------------------------------------------------------------
      if (des1.ne.des2) then
        write (line, 10) type, im1, im2, iaxis
10      format ('Unequal ', a, ' for images ', a, ' & ', a, 
     +          ' on axis ', i1)
        call bug (bflag, line)
      end if
c
      end
c
c
      subroutine extreme (n, d1, d2, dmin, dmax, padummy)
      implicit none
      integer n, i
      real d1(n), d2(n), dmin, dmax, padummy
c
      dmin = 1.0e32
      dmax = -1.0e32
      do i = 1, n
        if (d1(i).ne.padummy .and. d2(i).ne.padummy) then
          dmin = min(d1(i),d2(i),dmin)
          dmax = max(d1(i),d2(i),dmax)
        end if
      end do
      dmin = dmin - (0.05*(dmax-dmin))
      dmax = dmax + (0.05*(dmax-dmin))
      if (dmin.eq.dmax) then
        if (dmin.eq.0.0) then
          dmin = -1.0
          dmax = 1.0
        else
          dmin = dmin - 0.5*dmin
          dmax = dmax + 0.5*dmax
        end if
      end if
c
      end
c
c
      subroutine getopt (relax, ambig, accum, yind, guess)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     relax     Warnings only for axis descriptor mismatches
c     ambig     Do not remove ambuguites
c     accum     Put all plots on one page
c     yind      Each subplot scaled independently
c     guess     USe "rmi" as first guess
c-----------------------------------------------------------------------
      implicit none
c
      logical relax, ambig, accum, yind, guess
cc
      integer maxopt
      parameter (maxopt = 5)
c
      character opshuns(maxopt)*12
      logical present(maxopt)
      data opshuns /'relax', 'ambiguous', 'accumulate', 
     +              'yindependent', 'guess'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      relax = present(1)
      ambig = present(2)
      accum = present(3)
      yind  = present(4)
      guess = present(5)
c
      end
c
c
      subroutine hedinf (lun, naxis, size, epoch, crpix, cdelt,
     +                   crval, ctype)
c------------------------------------------------------------------------
c     Get some header keywords from the image associated with LUN
c 
c     Input
c       lun      Handle of image
c       naxis    Number of dimensions in image
c       size     Size of each axis
c     Output
c       epoch    Epoch of image
c       crpix    Array of image reference pixels
c       cdelt    Array of image increments (natural inits; rad)
c       crval    Array of image reference values (natural units)
c       ctype    Array of image axis types
c--
c------------------------------------------------------------------------
      implicit none
c
      integer lun, naxis, size(naxis)
      double precision cdelt(naxis), crval(naxis)
      real crpix(naxis), epoch
      character*(*) ctype(naxis)
cc
      integer i
      character str*1, itoaf*1
c---------------------------------------------------------------------
      do i = 1, naxis
        str = itoaf(i)
c
        call rdhdr (lun, 'crpix'//str, crpix(i), real(size(i))/2.0)
        call rdhdd (lun, 'cdelt'//str, cdelt(i), 1.0d0)
        call rdhda (lun, 'ctype'//str, ctype(i), ' ')
        call rdhdd (lun, 'crval'//str, crval(i), 0.0d0)
      end do
      call rdhdr (lun, 'epoch', epoch, 0.0)
c
      end 
c
c
      subroutine openin (maxdim, maxnax, bflag, in, lun, naxis, size,
     +                   epoch, crpix, cdelt, crval, ctype, fqax)
c-----------------------------------------------------------------------
c     Open an image and return some information about it
c
c  Input
c    maxdim     Maximum size a row can be
c    maxnax     Maximum number of axes image can have
c    in         Image name
c    bflag      Bug flag ; 'w' or 'f'
c  Output
c    lun        Handle
c    naxis      Number of axes
c    size       Size of each axis
c    epoch      EPoch of image
c    crpix      Refernce pixels
c    cdelt      Increments
c    crval      Reference values
c    ctype      Axis types
c    fqax       Frequencxy axis
c-----------------------------------------------------------------------
      implicit none
c
      integer maxdim, maxnax, lun, naxis, size(maxnax), fqax
      double precision cdelt(maxnax), crval(maxnax)
      real epoch, crpix(maxnax)
      character*(*) ctype(maxnax), in, bflag
cc
      integer len1, i
      character*80 aline, btype*25
c-----------------------------------------------------------------------     
      call xyopen (lun, in, 'old', maxnax, size)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) then
        aline = in(1:len1(in))//' has zero dimensions !!'
        call bug ('f', aline)
      end if
c
      if (size(1).gt.maxdim) then
        aline = 'First dimension of '//in(1:len1(in))//
     +          ' too large for storage'
        call bug ('f', aline)
      end if
c
      call hedinf (lun, naxis, size, epoch, crpix, cdelt, crval, ctype)
c
      fqax = 0
      do i = 1, naxis
        if (index(ctype(i),'FREQ').ne.0) fqax = i
      end do
      if (fqax.eq.0) then
        aline = in(1:len1(in))//'has no frequency axis'
        call bug ('f', aline)
      end if
c     
      if (fqax.le.2) then
        aline = 'Frequency axis for '//in(1:len1(in))//
     +          ' is < 3; should be > 2'
        call bug ('f', aline)
      end if
      if (size(fqax).gt.1) call bug ('f',
     +  'Frequency axis must be of length 1 only')
c
      call rdbtype (lun, btype, ' ')
      if (btype.ne.'position_angle') then
        aline = in(1:len1(in))//' does not appear to be a PA image'
        call bug (bflag, aline)
      end if
c
      end
c
c
      subroutine openout (lin, out, naxis, size, nkeys, keyw, 
     +                    version, lout)
c-----------------------------------------------------------------------
c     Open an output image and write history
c
c  Input
c    lin    Handle for an open input image from which to copy 
c           header keywords
c    out    FIle name
c    naxis  Numebr of axes
c    size   SIze of axes
c    nkeys  Number of keywords to copy from header
c    keyw   Keywords to copy
c    versionVerion of task
c  Output
c    lout   Handle
c-----------------------------------------------------------------------
      implicit none
c
      integer lin, lout, nkeys, naxis, size(naxis)
      character*(*) out, keyw(nkeys), version
cc
      integer i
      character*132 aline
c-----------------------------------------------------------------------
      call xyopen (lout, out, 'new', naxis, size)
      do i = 1, nkeys
        call hdcopy (lin, lout, keyw(i))
      end do
c
      call hisopen  (lout, 'append')
      aline = 'IMRM Miriad'//version
      call hiswrite (lout, aline)
      call hisinput (lout, 'IMRM')
      call hisclose (lout)
c
      end
c
c
      subroutine plotit (device, nx, ny, accum, yind, npx, npy, nf,
     +                   x, yd, yf, cs, padummy)
c-----------------------------------------------------------------------
c     Plot data and fits
c
c  Input
c    device    PGPLOT device
c    nx,ny     Number of subplots in x and y
c    accum     Put all on one plot if true
c    yind      Scale subplots independently
c    npx,y     Number of pixels in images in x and y.
c    nf         Number of frequencies
c    x,yd,yf   PLotting arrays, data and fits
c    cs        Character height
c-----------------------------------------------------------------------
      implicit none
      integer npx, npy, nx, ny, nf
      real x(nf), yd(npx*npy*nf), yf(npx*npy*nf), cs, padummy
      logical accum, yind
      character*(*) device
cc
      integer ierr, pgbeg, i, j, ip, npts, i1, i2
      real xmin, xmax, ymin, ymax
      character str1*10, str2*10, line*80
c-----------------------------------------------------------------------      
c
c Open plot device
c
      ierr = pgbeg (0, device, nx, ny)
      if (ierr.ne.1) then
        call pgldev
        call bug ('f', 'Error opening plot device')
      end if
      if (cs.le.0.0) cs = 1.0
      call pgsch (cs)
c
c Find extrema
c
      call extreme (nf, x, x, xmin, xmax, padummy)
      npts = npx*npy*nf
      call extreme (npts, yd, yf, ymin, ymax, padummy)
c
c If accumulating all on one plot draw the box and label it now
c
c
      call pgvstd     
      call pgswin (xmin, xmax, ymin, ymax)
      if (accum) then
        call pgpage
        call pgbox ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        call pglabel ('\gl\u2\d (m\u2\d)', 
     +                'Position angle (degrees)', ' ')
      end if
c
c Loop over plots
c
      ip = 1
      do j = 1, npy
        do i = 1, npx
          if  (yd(ip).ne.padummy .and. yf(ip).ne.padummy) then
c
c If not accumulating, do some things for each subplot
c
            if (.not.accum) then
              call pgpage
              if (yind) then
                call extreme (nf, yd(ip), yf(ip), ymin, ymax, padummy)
                call pgswin (xmin, xmax, ymin, ymax)
              end if
c
              call pgbox ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
              call pglabel ('\gl\u2\d (m\u2\d)', 
     +                      'Position angle (degrees)', ' ')
c
c Write i,j pixel on plot
c
              call strfi (i, '(i4)', str1, i1)
              call strfi (j, '(i4)', str2, i2)
              line = str1(1:i1)//','//str2(1:i2)
              call pgmtxt ('T', -2.0, 0.1, 0.0, line(1:i1+i2+1))
            end if
c
c Draw plot
c 
            call pgpt (nf, x, yd(ip), 17)
            call pgline (nf, x, yf(ip))
          end if
          ip = ip + nf
        end do
      end do
c
      call pgend
c
      end
c
c
      subroutine pm90 (pa)
c-----------------------------------------------------------------------
c     Put an angle into the range =/- 90 degrees.   Remember up is the 
c     same as down for polarization postion angles
c
c  Input/output
c     pa    position angle in radians
c-----------------------------------------------------------------------
      implicit none
      include 'mirconst.h'
c
      real pa
c-----------------------------------------------------------------------
      pa = mod(dble(pa), dpi)
      if (pa.gt.dpi/2.0d0) then
        pa = pa - dpi
      else if (pa.lt.-dpi/2.0) then
        pa = pa + dpi
      end if
c
      end
c
c
      subroutine rempi (pa1, pa2)
      implicit none
      real pa1, pa2
cc
      include 'mirconst.h'
      double precision d
c
      d = pa1 - pa2
      if (d.gt.dpi/2.0d0) then
        pa2 = pa2 + dpi
      else if (d.lt.-dpi/2.0d0) then
        pa2 = pa2 - dpi
      end if
c
      end
c
c
      subroutine rfit (guess, c1, c2, noerr, n, lsq, pa, wt, rmi, 
     +                 rm, pa0, erm, epa0, q)
c-----------------------------------------------------------------------
c     Low level least squares fit PA versus LAMBDA**2, with 
c     optional ambiguity removal
c
c  Input
c    guess Use rmi as initial guess rather than removing rmi rotation
c	   and getting it from first 2 frequencies
c    c1,2  Pointers to frequencies to use for attempt to 
c          remove ambiguities
c    noerr True if there were no input position angle error images
c    n     Number of frequencies
c    lsq Wavelength squared (m**2)
c    pa    POsition angles (radians)
c    wt    Weights. 
c    rmi   Amount of rotation measure to remove first
c  Output
c    rm    Fitted rotaition measure (rad/m**2)
c    pa0   Position angle at zero wavelength (radians)
c    erm   Error in RM
c    epa0  Error in PA0
c    q     Goodness of fit
c-----------------------------------------------------------------------
      implicit none
c
      integer n, c1, c2
      real lsq(n), pa(n), wt(n), rm, pa0, erm, epa0, q, chisq, rmi
      logical noerr, guess
cc
      include 'mirconst.h'
      real rmg, pag, yp
      double precision r2d
      integer i, nturns
      parameter (r2d = 180.0d0/dpi)
c-----------------------------------------------------------------------
c
c Remove initial guess for RM
c
      if (.not.guess .and. rmi.ne.0.0) call rmsub (n, lsq, pa, rmi)
c
c Remove PI ambiguity from designated frequencies
c   
      call pm90 (pa(c1))
      call pm90 (pa(c2))
      call rempi (pa(c1), pa(c2))
c
c Find initial guess for RM and PA0 from these frequencies
c or from that input by the user
c
      if (guess) then
        rmg = rmi
      else
        rmg = (pa(c2) - pa(c1)) / (lsq(c2) - lsq(c1))
      end if
      pag = pa(c1) - rmg*lsq(c1)
c
c Try to remove N*PI ambiguities from the other frequencies
c If only 2 frequencies, nothing will happen here.
c
      do i = 1, n
        if (i.ne.c1 .and. i.ne.c2) then
c
c Work out how many turns will place the actual position angle closest
c to the predicted position angle. Add these turns to the data.
c
          yp = rmg*lsq(i) + pag
          nturns = nint((yp - pa(i))/dpi)
          pa(i) = pa(i) + nturns*dpi
        end if
      end do
c
c Do the fit
c
      call lsf (noerr, n, lsq, pa, wt, rm, pa0, erm, epa0, 
     +          chisq, q)
c
      end
c
c
      subroutine rmsub (n, lsq, pa, rmi)
      implicit none
      integer n
      real rmi, pa(n), lsq(n)
      integer i
c
      do i = 1, n
        pa(i) = pa(i) - lsq(i)*rmi
      end do
c
      end
c
c
      subroutine rotfit (c1, c2, noerr, n, lsq, pa, pa2, wt, guess,
     +                   ambig, rm0, rmf, pa0f, erm, epa0, q, ypl)
c-----------------------------------------------------------------------
c     High level least squares fit PA versus LAMBDA**2
c
c  Input
c    c1,2  Pointers to frequencies to use for attempt to 
c          remove ambiguities
c    noerr True if there were no input position angle error images
c    n     Number of frequencies
c    lsq   Wavelength squared (m**2)
c    pa    Position angles (radians)
c    wt    Weights for each frequency
c    guess USe rm0 as first guess instead
c    ambig Do not remove ambiguites
c    rm0   Initial amoutn of RM to remove from data before fitting
c  Output
c    rmf   Fitted rotation measure (rad/m**2)
c    pa0f  Fitted position angle at zero wavelength (radians)
c    erm   Error in RM
c    epa0  Error in PA0
c    q     Goodness of fit
c    ypl   Data for plotting
c  Input/output
c    pa2   Scratch array
c-----------------------------------------------------------------------
      implicit none
c
      integer n, c1, c2
      real lsq(n), pa(n), pa2(n), wt(n), rm0, rmf, pa0f, erm, epa0, 
     +  q, chisq, ypl(n)
      logical noerr, ambig, guess
cc
      include 'mirconst.h'
      real rmi
      integer i
c-----------------------------------------------------------------------
      if (ambig) then
c
c Remove initial angle for initial estimate of RM
c
        if (rm0.ne.0.0) call rmsub (n, lsq, pa, rm0)
c
c Put position angles into +/- pi/2
c
        do i = 1, n
          call pm90 (pa(i))
        end do
c
c Fit
c
        call lsf (noerr, n, lsq, pa, wt, rmf, pa0f, erm, epa0, 
     +            chisq, q)
c
c Assign final value of rotation measure. Note that zero wavelength
c position angle is unaffected
c
        rmf = rmf + rm0
      else
c
c Save original position angles
c 
        do i = 1, n
          pa2(i) = pa(i)
        end do
c
c Do first fit; remove initial estimate, compute RM from
c c1 and c2 frequencies, remove N*PI ambiguities, do 
c least squares fir to get rotation measure
c
        rmi = rm0
        call rfit (guess, c1, c2, noerr, n, lsq, pa2, wt, rmi,
     +             rmf, pa0f, erm, epa0, q)
c
c Now redo with the new estimate of the rotation measure. This
c time, we always subtract the angle from for the first estimate
c of the RM (i.e. guess = .false.)
c
        if (guess) then
          rmi = rmf
        else
          rmi = rmi + rmf
        end if
        call rfit (.false., c1, c2, noerr, n, lsq, pa, wt, rmi,
     +             rmf, pa0f, erm, epa0, q)
c
c Assign final value of rotation measure. Note that zero wavelength
c position angle is unaffected
c
        rmf = rmf + rmi
      end if
c
c Save data for plotting.  We want to plot the data that was finally 
c fitted, but with the angle subtracted for the initial RM estimate 
c added back in. 
c
      if (ambig) rmi = rm0
      do i = 1, n
        ypl(i) = (pa(i) + rmi*lsq(i)) * 180.0d0/dpi
      end do        
c
      end
