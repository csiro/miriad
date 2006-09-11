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
c	postion angle at zero wavelength. 
c
c	By default, IMRM attempts to remove n*pi ambiguities by measuring 
c	the RM between the closest spaced frequencies (ASSUMIMG NO AMBIGUITY) 
c	and then uses this to predict the position angle of the other 
c	frequencies.  The measured position angles at the other frequencies are
c	then modified by as many pi turns as is necessary to bring them as 
c	close as possible to the prediction.  Then the data are fitted. 
c	This can be turned off with options=ambiguous as the algorithm
c	may fail if the measurements at the closest frequencies differ
c	only by noise.
c
c	There are also a variety of methods offered with which to blank the 
c	output images.  Most of these require error images associated with
c	the input position angle images. Use the program IMPOL to make 
c	the position angle images and position angle error images.
c
c@ in
c	Up to 5 input position angle (positive N -> E) images 
c	(in degrees) at different frequencies.
c	Wild card expansion is supported, no default.
c@ inerr
c	Up to 5 position angle error images (in degrees) used for 
c	weighting the data during the least squares fit.  They are 
c	assumed to be in one-to-one association with the position 
c	angle images. If no error images are given, each position 
c	angle image is given equal weight and we must assume a goodness
c	of fit of unity in order to find the output image errors.
c	Wild card expansion is supported, default is no error images.
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
c@ options
c	Task enrichment options.  Minimum match is active,
c
c	"relax"      issue warnings instead of a fatal error when image
c	             axis descriptors are inconsistent with each other,
c		     and when the input image headers do not indicate that
c		     they are position angle images (btype=position_angle)
c	"rot90"      adds an extra 90 degrees to the output position angle
c	             zero wavelength image.  This is useful to make B.
c	"ambiguous"  Do not try to remove ambiguites.  If the two most 
c		     closely spaced frequencies are very close such that 
c		     their measurements differ only because of noise, the 
c		     ambiguity removing algorithm will fail. 
c--
c  History:
c    nebk 22may92   Original version.
c    nebk 26may92   Try to deal with ambiguities.
c    nebk 27may92   Improve warnings to users
c    nebk 04nov92   Reorganize and improve blanking, add output error
c		    images, rewrite lsf to include goodness of fit
c    mjs  12mar93   Use maxnax.h file instead of setting own value.
c    nebk 11nov93   Add options=ambiguous and output blanking info
c------------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
c
      double precision r2d
      integer maxim
      character version*(*)
      parameter (version = 'ImRM: version 11-Nov-93' )
      parameter (maxim = 5, r2d = 180.0d0/dpi)
cc
      real lambdasq(maxim), pa(maxim), wt(maxim)
c 
      double precision cdelt(maxnax,maxim), crval(maxnax,maxim),
     +ecdelt(maxnax,maxim), ecrval(maxnax,maxim)
      real line(maxdim,maxim), eline(maxdim,maxim), rmline(maxdim),
     +ermline(maxdim), paline(maxdim), epaline(maxdim), 
     +epoch(maxim), eepoch(maxim), crpix(maxnax,maxim),
     +freq(maxim), ecrpix(maxnax,maxim), diff, d2, qcut, errcut,
     +rmcut, pacut, q
c
      integer lin(maxim), lein(maxim), lrm(2), lpa(2),
     +size(maxnax,maxim), esize(maxnax,maxim), naxis(maxim), 
     +enaxis(maxim), fqax(maxim), efqax(maxim), frqax, nim, 
     +neim, i, j, k, l, imin, jmin, nbl(6), blsum, left, nt(maxim),
     +ntmax(maxim)
c
      character aline*100, in(maxim)*64, ein(maxim)*64, rmout*64, 
     +ermout*64, paout*64, epaout*64, ctype(maxnax,maxim)*9,
     +ectype(maxnax,maxim)*9, bflag
c
      logical flags(maxdim,maxim), eflags(maxdim,maxim), oflags(maxdim),
     +relax, rot90, blank, oblank, doerrbl, dormbl, dopabl, noerr, 
     +ambig, wd(maxim), msg
c
      integer nkeys
      parameter (nkeys = 46)
      character keyw(nkeys)*8
c
      data keyw/     'cdelt1  ','cdelt2  ','cdelt3  ',
     +    'cdelt4  ','cdelt5  ','crota1  ','crota2  ','crota3  ',
     +	  'crota4  ','crota5  ','crpix1  ','crpix2  ','crpix3  ',
     +    'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     +    'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     +    'date-obs','epoch   ','history ','instrume','niters  ',
     +    'object  ','restfreq','telescop','vobs    ','obsra   ',
     +    'obsdec  ','observer','xshift  ','yshift  ','bmaj    ',
     +    'bmin    ','bpa     ','pbfwhm  ','lstart  ','lstep   ',
     +    'ltype   ','lwidth  ','vobs    '/
      data nbl /6*0/
      data nt /maxim*0/
      data ntmax /maxim*0/
c-------------------------------------------------------------------------
      call output (version)
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
      call keya ('pa0', paout, ' ')
      call keya ('pa0', epaout, ' ')
      if (rmout.eq.' ' .and. paout.eq.' ') call bug ('f',
     +  'You have not specified any output images')
c
      call keyr ('qcut', qcut, 0.001)
      call keyr ('errcut', errcut, -1.0)
      call keyr ('rmcut', rmcut, -1.0)
      call keyr ('pacut', pacut, -1.0)
      call getopt (relax, rot90, ambig)
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
        lambdasq(i) = (dcmks / (freq(i) * 1.0e9))**2
        write (aline,10) freq(i)
10      format ('Found frequency ', f8.4, ' GHz')
        call output (aline)
      end do
c
c Find closest spaced frequencies
c
      diff = 1.0e30
      do i = 1, nim-1
        do j = i+1, nim
          d2 = abs(freq(j) - freq(i))
          if (d2.eq.0.0) call bug ('f', 
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
        call output (aline)
      end if
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
            call pm90 (line(i,k))
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
          end  do
c 
100       if (.not.blank) then
c 
c Do the fit if all the input criteria are satisified
c
            call rotfit (imin, jmin, noerr, nim, lambdasq, pa,
     +        wt, ambig, rmline(i), paline(i), ermline(i), 
     +        epaline(i), q, nt, ntmax, wd)
            oflags(i) = .true.
            paline(i) = paline(i) * r2d
            epaline(i) = epaline(i) * r2d
            if (rot90) paline(i) = paline(i) + 90.0
            call pm90 (paline(i))
c
c Ditch this pixel if output blanking criteria so dictate
c
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
            if (oblank) then
              call blnkall (oflags(i), rmline(i), paline(i),
     +                      ermline(i), epaline(i))
              do l = 1, nim
                if (wd(l)) nt(l) = nt(l) - 1
              end do
            end if
          else
            call blnkall (oflags(i), rmline(i), paline(i),
     +                      ermline(i), epaline(i))
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
c Warn user if excessive turns removed
c
      msg = .false.
      do i = 1, nim
        if (nt(i).gt.0) then
          write (aline,200) nt(i), freq(i), ntmax(i)
200       format (i8, ' output pixels at ', f8.4, 
     +           ' GHz had > 3 turns removed; max. turns=', i4)
          call output (aline)
          msg = .true.
        end if
      end do
      if (msg) call output 
     +  ('If excessive turn removal occurred, try options=ambiguous')
c
      end
c
c
      subroutine getopt (relax, rot90, ambig)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     relax     Warnings only for axis descriptor mismatches
c     rot90     Add 90 degrees to output zero wavelength p.a. image
c     ambig     Do not remove ambuguites
c-----------------------------------------------------------------------
      implicit none
c
      logical relax, rot90, ambig
cc
      integer maxopt
      parameter (maxopt = 3)
c
      character opshuns(maxopt)*9
      logical present(maxopt)
      data opshuns /'relax', 'rot90', 'ambiguous'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      relax = present(1)
      rot90 = present(2)
      ambig = present(3)
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
        call rdhdd (lun, 'cdelt'//str, cdelt(i), 1.0)
        call rdhda (lun, 'ctype'//str, ctype(i), ' ')
        call rdhdd (lun, 'crval'//str, crval(i), 0.0)
      end do
      call rdhdr (lun, 'epoch', epoch, 0.0)
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
c-----------------------------------------------------------------------
      call xyopen (lout, out, 'new', naxis, size)
      do i = 1, nkeys
        call hdcopy (lin, lout, keyw(i))
      end do
c
      call hisopen  (lout, 'append')
      call hiswrite (lout, 'IMRM Miriad'//version)
      call hisinput (lout, 'IMRM')
      call hisclose (lout)
c
      end
c
c
      subroutine pm90 (pa)
c-----------------------------------------------------------------------
c     Put an angle into the range =/- 90 degrees.   Remember up is the 
c     same as down for polarization postion angles
c
c-----------------------------------------------------------------------
      implicit none
c
      real pa
c-----------------------------------------------------------------------
      pa = mod(pa, 180.0)
      if (pa.gt.90.0) then
        pa = pa - 180.0
      else if (pa.lt.-90.0) then
        pa = pa + 180.0
      end if
c
      end
c
c
      subroutine rotfit (c1, c2, noerr, n, lamsq, pa, wt, ambig,
     +                   rm, pa0, erm, epa0, q, nt, ntm, wd)
c-----------------------------------------------------------------------
c     Set up for least squares fit, by trying to remove ambiguities
c
c  Input
c    c1,2  CLosest frequency pointers
c    noerr True if there were no input position angle error images
c    n     Number of frequencies
c    lamsq Wavelength squared (m**2)
c    pa    POsition angles (radians)
c    wt    Weights. 
c    ambig Do not remove ambiguites
c  Output
c    rm    Fitted rotiaiton measure (rad/m**2)
c    pa0   Position angle at zero wavelength (radians)
c    erm   Error in RM
c    epa0  Error in PA0
c    q     Goodness of fit
c  Input/output
c    nt    Number of pixels where there were more than three
c          turns applied for each  frequency
c    
c    wd    True if nt incremented this call 
c-----------------------------------------------------------------------
      implicit none
c
      integer n, c1, c2, nt(n), ntm(n)
      real lamsq(n), pa(n), wt(n), rm, pa0, erm, epa0, q, chisq
      logical noerr, ambig, wd(n)
cc
      double precision rmi, pa0i, yp, d1, d2, d3, pa1, pa2
      integer i, nturns
c
      include 'mirconst.h'
c-----------------------------------------------------------------------
      if (.not.ambig) then
c
c Find initial guess for RM and PA0 from closest spaced frequencies.
c Make sure the initial RM is worked out from the smallest difference
c between the angles, allowing for the pi ambiguity
c
        d1 = abs(pa(c2) - pa(c1))
        pa1 = pa(c1) + dpi
        d2 = abs(pa(c2) - pa1)
        pa2 = pa(c2) + dpi
        d3 = abs(pa2 - pa(c1))
c
        if (d1.lt.d2 .and. d1.lt.d3) then
          continue
        else if (d2.lt.d1 .and. d2.lt.d3) then
          pa(c1) = pa1
        else if (d3.lt.d1 .and. d3.lt.d2) then
          pa(c2) = pa2
        end if
c
c Work out initial RM and PA0 
c
        rmi = pa(c2) - pa(c1) / (lamsq(c2) - lamsq(c1))
        pa0i = pa(c1) - rmi*lamsq(c1)
c
c Try to remove ambiguities 
c
        do i = 1, n
          wd(i) = .false.
          if (i.ne.c1 .and. i.ne.c2) then
c
c Work out how many turns will place the actual position angle
c closest to the predicted position angle, based on the RM from
c the closest frequencies. Add these turns to the data.
c
            yp = rmi*lamsq(i) + pa0i
            nturns = nint((yp - pa(i))/dpi)
            pa(i) = pa(i) + nturns*dpi
            if (nturns.gt.3) then
              nt(i) = nt(i) + 1
              wd(i) = .true.
              ntm(i) = max(ntm(i),nturns)
            end if
          end if
        end do
      end if
c
c Do the fit
c
      call lsf (noerr, n, lamsq, pa, wt, rm, pa0, erm, epa0, chisq, q)
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

