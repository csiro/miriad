c************************************************************************
      program zeesim
c
c= zeesim - Test reliability of Zeeman splitting errors
c& nebk
c: profile analysis
c+
c
c  Estimate the reliability of Zeeman error estimates for spatial 
c  summing or averaging.    This is generally essential for spatial
c  summing, and possibly needed for spatial averaging when eta is 
c  of the order of unity.
c
c  Program in four logical sections
c  1)  Read in data
c  2)  Fit data and get estimates of noise free spectra
c  3)  Find the FFT of the beam
c  4)  Simulate by adding correlated noise and refitting
c
c
c@ iin
c	Input I cube (vxy order). Wild card expansion supported.  
c	No default.
c@ vin
c	Input V cube (vxy order). Wild card expansion supported. 
c	No default.
c@ beam
c	The beam of the observation.  Wild card expansion supported.  
c	Not needed for AVEOP=`a'
c@ mode
c	This is a character string that determines the algorithm used 
c	in the fitting process. It consists of several flags, which 
c	can be:
c	  m Use maximum likelihood technique.
c	  l Include a leakage term in the fitting.
c	  2 Use a two sided derivative estimate.
c	  x Perform extra checks for better solutions when using the 
c	    maximum likelihood technique.
c	  d Debiased least squares estimate.
c	The default is ' ' i.e., least squares and 1.
c@ aveop
c	`a' for averaged spectrum in window, `s' for summed spectra
c	Summed computational load is orders of magnitude greater than for 
c	averaged spectra.  'h' for hybrid of averaging and summing.
c	Default is `a'.  The hybrid is the same as `s', except when 
c	fitting the simulated spectra, whereupon the spectra are first
c	averaged for each window.
c@ chan
c	Channel range. Default is all channels.
c@ freq
c	Frequency (GHz) for conversion of channel splitting to B field.
c@ blc
c	Bottom left corner of spatial region to examine. Default is (1,1).
c@ trc
c	Top right corner of spatial region to examine. Default is all
c	of image.
c@ bin
c	Binning widths for all three (v,x,y) dimensions. Default = 1. 
c	This keyword enables each region (spectral and spatial) to 
c	be binned up before fitting.  It's as if you had observed
c	with lower resolution.  Spatial binning is of no use if
c	you are using spatial averaging.
c@ split
c	Splitting (split to unsplit in channels) to use for calculation
c	of hatV and all simulation (i.e., don't use actual splitting
c	as predicted by fitting algorithm when generating V spectra.
c@ nruns
c	The number of simulation runs to undertake.   If 0, then just
c	the initial fits are done.  Default is 0.
c@ infile
c	File containing a list of windows.  If this file specified,
c	blc and trc are ignored.  Should be in format:
c	  NWIN
c	  I XCEN YCEN XOFF YOFF
c	And there are nwin of these lines.  XOFF and YOFF define the
c	box half-sizes from their specified centres, XCEN and YCEN.
c	You can leave off XOFF and YOFF and they will default to 2
c	All units are pixels.
c@ log
c       If NRUNS = 0
c	  One value which is a root file name, appended to which is
c         the box number as specified in the INFILE (or = 1 for
c         a box specified with BLC and TRC).   These files contain
c         the fitted results for each window.   The files are opened
c         in APPEND mode.
c
c       If NRUNS > 0
c         Two values.  The first is a root file name, appended to which 
c         is the box number as specified in the INFILE (or = 1 for
c         a box specified with BLC and TRC).   These files contain the
c         results for each simulation with a cumulative fiddle factor
c         worked out.  The first line is the initially fit results
c         before simulation.
c         The second value is a file containing a statistical summary
c         of the final results of the simulations from all windows, 
c         including the initially fit results before simulation began.
c@ nran
c	Throw away NRAN random numbers before starting.  Use this to 
c	continue a set of simulations with different random numbers.
c
c--
c
c  History:
c
c    nebk  17aug89   Original and fine version
c    rjs   18aug89   Fixed glaring bug left by Beauchamp Killeen
c	             Moved ConvlIni to top level routine.
c		     Improved some of the poor layout.
c    nebk  18aug89   Deproved some of the improved poor layout.
c                    Removed all horrid TABS recently introduced
c                    by unknown hacker.  Tried to speed it up a
c                    bit more in NOISEFIL.
c          25aug89   Add multiple window capability 
c          15sep89   Add user input splitting capability
c          18sep89   Add averaging capability
c          19sep89   Fix stupid bug causing incorrect Sigma_I to be
c                    used for some windows in averaging mode. Add
c                    Fiddle factor file.  Add some warnings about
c                    about noise image wrap around.
c          20sep89   Remove FORTRAN READ statements and replace
c                    by TXTREAD and decoding routines.
c          05aug90   Add hybrid summing/average option
c          09oct90   Add binning options requiring a major rewrite
c          25oct90   Add multiple log file closing to force the output
c                    log file buffers to be flushed during long batch
c                    jobs.  Merge with .DOC file.
c          26oct90   Orders of magnitude speed improvement by more
c                    efficicent use of noise images.  Remove call to 
c                    BEMRSSQ with empirical noise scale factor 
c                    determination. Reorganize top level to make 
c                    "structure" clearer (?).  Means more subroutines.
c          26nov90   Change contents of output files somewhat and fix
c                    bug with generation of first noise image.
c          08jan91   Removed unused variable FIRST, complete 
c                    documentation of subroutines.  Rename internal log 
c                    file names to LOG1 and LOG2.  Fix bug with default
c                    TRC(1,1) in subroutine USERIN
c           5mar91   Change itoaf to itoaf, atoi to atoif, atod to atodf
c                    Change KEYA to KEYF for input files, remove 
c                    concatenated char*(*) variables from sub. calls
c          11apr91   Make changes reflecting new HANNSM call and some
c                    other cosmetic changes
c     rjs  10mar92   Increased size of MAXRUNS
c     nebk/mjs  
c          28mar92   Converted to use memalloc/memfree routines and made
c                    a variety of other small changes in the process
c     nebk 11jul92   Call to TXTOPENA has changed and replaces TXTAPP
c     rjs  10nov93   Replace txtopena with txtopen.
c     nebk 20nov93   Improve documentation
c     nebk 09apr94   maxwin -> maxreg so maxwin can go in maxdim.h
c     nebk 27jun94   Beam opened asking for 3 instead of 2 dimensions
c     rjs  18sep05   Cosmetic correction.
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
c
      integer maxruns, maxreg
      parameter (maxruns = 3*maxdim, maxreg = 30)
c
      integer ptispe, ptvspe, ptihat, ptvhat, ptnois, ptnoic, ptbemf
      real    data(maxbuf)
      common  data
c
      real row(maxdim), a(maxreg), siga(maxreg), suma(maxreg), 
     +sumsqa(maxreg), sigi(maxreg), eta2(maxreg)
c
      real split, sigim, mean, var, sdev, freq, scale, nsigma, 
     +meanb, sigb
c
      integer liin, lvin, lbin, llog(maxreg), blc(3,maxreg),
     +trc(3,maxreg), iwin(maxreg), nbin(3,maxreg), isize(3), vsize(3), 
     +bsize(2), nsum(maxreg), nsp(maxreg), isp(maxreg), runs(3,maxruns),
     +pt(maxreg), nruns, nsim, h, i, j, iostat, nchan, choff, nchuck, 
     +nwin, xoff, yoff, bxref, byref, lfid, idiv, nsdim, obin(3),
     +bin(3,maxreg), wsize(maxreg), totsize, nnbin(2), nblc(2), ntrc(2),
     +delta, ilen(maxreg), maxy, fftdim
c
      character mode*8, iin*64, vin*64, beam*64, log1*64, log2*64, 
     +aline*100, infile*64, itoaf*3, str*3, aveop*1, outfile(maxreg)*64
c
      logical old(maxreg)
c
      integer len1
      data ptispe, ptvspe, ptihat, ptvhat, ptnois, ptnoic, ptbemf
     +   /0, 0, 0, 0, 0, 0, 0/
c-----------------------------------------------------------------------
      call output ('ZeeSim : Version 27-Jun-94')
c
c First section: read in data
c----------------------------
c
c Get user inputs
c
      call userin (maxreg, iin, vin, beam, mode, aveop, freq, blc, trc,
     +             obin, split, nsim, infile, log1, log2, nchuck)
c
c  Open the input images and make dimension checks
c
      call xyopen (liin, iin, 'old', 3, isize)
      call xyopen (lvin, vin, 'old', 3, vsize)
      if (beam.ne.' ') call xyopen (lbin, beam, 'old', 2, bsize)
      call dimcheck (liin, lvin, lbin, beam, isize, vsize, bsize, 
     +               maxdim, bxref, byref)
c
c Read input regions from file if necessary and fix them up to deal 
c with defaults and binning.  Regions are adjusted to that an integral
c number of bin widths fit into them (x and y directions only, not v)
c
      call getwin (infile, maxreg, nwin, blc, trc, iwin)
      call fixwin (beam, aveop, maxreg, isize, vsize, bsize, obin, bin,
     +             blc, trc, nwin, nbin, wsize, totsize, maxy)
c
c Get conversion factor from splitting in channels to B field strength
c
      call bfac (liin, obin, freq, scale)
c
c Allocate memory for I and V cubes in the DATA array and return
c pointers with filthy memalloc routines
c
      call memalloc (ptispe, totsize, 'r')
      call memalloc (ptvspe, totsize, 'r')
c
c  Read in the region(s) requested by the user, and apply all binning.
c  Spectra from regions are stored sequentially in DATA(ptISPE)
c  and DATA(ptVSPE)
c
      call readwin (liin, lvin, totsize, maxreg, wsize, iwin, bin, nbin,
     +              blc, trc, nwin, pt, row, data(ptispe), 
     +              data(ptvspe), nsp)
      call xyclose (liin)
      call xyclose (lvin)
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c Second section: do initial fits to spectra 
c -------------------------------------------
c
c
      call output (' ')
      call output ('Regions read in, begin initial fits')
      call output ('***********************************')
      call output (' ')
c
c Open output log files ; one for each region of interest
c Contents depend on whether doing simulations or not.
c
      call openlog (log1, nwin, iwin, ilen, outfile, llog, old)
      if (nsim.eq.0 .and. log1.ne.' ') call logtit (nwin, llog, old)
c
c  Allocate memory for estimates of noiseless I and V spectra
c
      call memalloc (ptihat, totsize, 'r')
      call memalloc (ptvhat, totsize, 'r')
c      
c  For each region, estimate the splitting, compute the smoothed 
c  noiseless spectra estimates and find eta.  Write log files
c  if not doing any simulations and report results to user.
c
      call fits (llog, mode, scale, split, nsim, nwin, sigim, totsize,
     +           data(ptispe), data(ptvspe), pt, nbin, nsp, iwin, a,
     +           siga, eta2, sigi, data(ptihat), data(ptvhat), delta)
c
c Bail out here if not doing any simulations
c
      if (nsim.eq.0) then
         if (log1.ne.' ') then
            do h = 1, nwin       
              call txtclose (llog(h))
            end do
         end if
         goto 999
      end if
c
c
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c Third section: FFT the beam if doing summing or hybrid
c -------------------------------------------------------
c
c
      if (aveop.ne.'a') then
         call output (' ')
         write (aline, '(a, 1pe12.4)') 
     +        'Mean noise level from all windows = ', sigim
         call output (aline)
      end if
c
c Allocate memory for noise and convolved noise arrays; only need a 
c spectrum of noise for I and V if averaging, but need full image 
c otherwise.  Toss away random numbers here too.
c
      nchan = nbin(1,1)
      if (aveop.eq.'a') then
        nsdim = 2 * nchan
      else
        nsdim = bsize(1) * bsize(2)
      end if
      call memalloc (ptnois, nsdim, 'r')
      if (aveop.ne.'a') call memalloc (ptnoic, nsdim, 'r')
c
      call tossran (nchuck, nsdim, data(ptnois))
c
c FFT the beam if summing or hybrid
c
      if (beam.ne.' ') then
c
c Allocate memory and initialize
c
        fftdim = (bsize(1)/2+1) * bsize(2)
        call memalloc (ptbemf, fftdim, 'r')
c
c FFT and close up beam image
c
        call runfill (bsize, maxruns, nruns, runs)
        call ConvlIni (lbin, data(ptbemf), bsize(1), bsize(2), 0.0, 
     +                 bxref, byref)
        call xyclose (lbin)
c
c Find window on noise image to integrally fit binning sizes
c
        call noisefid (obin(2), bsize(1), nblc(1), ntrc(1), nnbin(1))
        call noisefid (obin(3), bsize(2), nblc(2), ntrc(2), nnbin(2))
c
c Work out sigma for uncorrelated noise image. The correlated and binned
c noise image must have a noise level equal to that of the fitted 
c spectra. As these convolution processes change sigma, we must adjust 
c the uncorrelated noise image noise level to allow for this.  
c
        call output ('Work out noise scaling factor')
        call output ('*****************************')
c
c Convolve image of uncorrelated noise and bin up as per I and V cubes
c Reuse the uncorrelated noise array for the binned correlated noise.
c
        call gencor (bsize, nsdim, fftdim, 1.0, data(ptbemf),
     +               data(ptnois), data(ptnoic), nruns, runs)
        call binoise (bsize(1), bsize(2), nsdim, nnbin, obin(2), 
     +                nblc, ntrc, data(ptnoic), data(ptnois))
        call stats (nnbin(1)*nnbin(2), data(ptnois), meanb, sigb,
     +              .false.)
        nsigma = sigim / sigb
c
        call telluser (nblc, ntrc, obin, nnbin, sigb, nsigma)
      end if

c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c Fourth section: simulation loop
c Remember the channel range is the same for all regions 
c
c Initialize sums, open log files and fiddle factor file
c
      call simini (mode, scale, nwin, log1, log2, nsum, suma,
     +             sumsqa, llog, lfid, a, siga, eta2)
c
c Loop over number of simulations
c
      choff = blc(1,1) - 1
      do j = 1, nsim
c
c Add noise back into noiseless estimates of spectra
c
        if (aveop.eq.'s' .or. aveop.eq.'h') then
c
c If using spatial summing then add correlated noise to I and V spectra 
c one channel at a time
c
          str = itoaf(j)
          call output ('Beginning simulation # '//str)
          do i = 1, nchan
            call tonsils (1, j, choff, i, nwin, maxy, nnbin, nbin,
     +         xoff, yoff, iwin, bsize, nsdim, fftdim, nsigma,
     +         data(ptbemf), data(ptnois), data(ptnoic),
     +         nruns, runs, obin(2), nblc, ntrc, nchan,
     +         nsp, totsize, data(ptihat), pt, data(ptispe))
c
            call tonsils (2, j, choff, i, nwin, maxy, nnbin, nbin,
     +         xoff, yoff, iwin, bsize, nsdim, fftdim, nsigma,
     +         data(ptbemf), data(ptnois), data(ptnoic), nruns,
     +         runs, obin(2), nblc, ntrc, nchan, nsp, totsize,
     +         data(ptvhat), pt, data(ptvspe))
          end do
        else
c
c Add uncorrelated noise into the smoothed noiseless I and V estimates
c in the case that one averaged spectrum is used
c
          str = itoaf(j)   
          idiv = min(nsim,5)
          if (mod(j,nsim/idiv).eq.1) 
     +       call output ('Beginning simulation # '//str)
c
          call adnoids (totsize, nwin, nchan, pt, sigi, data(ptihat),
     +                  data(ptvhat), data(ptnois), data(ptispe),
     +                  data(ptvspe))
        end if
c
c In hybrid case, reform averaged spectra for fitting
c
        call reform (aveop, nwin, nchan, nsp, totsize, pt,
     +               data(ptispe), data(ptvspe), isp)
c
c Fit the spectra with ZED again, accumulate sums and write results
c from the current simulation to the log files. Tell user on last
c simulation what the fiddle factors are.
c
        call refit (aveop, mode, a, siga, nsim, j, nwin, iwin, totsize,
     +              pt, data(ptispe), data(ptvspe), nchan, isp, log1,
     +              llog, outfile, ilen, delta, nsum, suma, sumsqa)
      end do
c
c Write fiddle factor file
c
      if (log2.ne.' ') then
        do h = 1, nwin
          call facs (nsum(h), suma(h), sumsqa(h), mean, var, sdev)
          write (aline, 
     *      '(i4, 1x, f5.2, 1x, 5(1pe10.3,1x), i4, 1x, 0pf5.2)') 
     *	    iwin(h), sqrt(eta2(h)), a(h), 
     *      siga(h), 2.0*scale, mean, sdev, nsum(h), sdev/siga(h)
          call txtwrite (lfid, aline, len1(aline), iostat)
        end do
        call txtclose (lfid)
      end if
c
c Close up
c
      if (log1.ne.' ') then
        do j = 1, nwin
          call txtclose (llog(j))
        end do
      end if
c
999   continue
c
c Deallocate dynamic memory
c
      if (ptispe.ne.0) call memfree (ptispe, totsize, 'r')
      if (ptvspe.ne.0) call memfree (ptvspe, totsize, 'r')
      if (ptihat.ne.0) call memfree (ptihat, totsize, 'r')
      if (ptvhat.ne.0) call memfree (ptvhat, totsize, 'r')
      if (ptnois.ne.0) call memfree (ptnois, nsdim,   'r')
      if (ptnoic.ne.0) call memfree (ptnoic, nsdim,   'r')
      if (ptbemf.ne.0) call memfree (ptbemf, fftdim,  'r')
c
      end
c
c
      subroutine userin (maxreg, iin, vin, beam, mode, aveop, freq, blc,
     +   trc, obin, split, nsim, infile, log1, log2, nchuck)
c-----------------------------------------------------------------------
c     Get user inputs
c
c     Input: 
c        maxreg    i     Maximum number of allowed regions of interest
c     Output:
c        iin,vin   c     Names of I and V images
c        beam      c     Name of BEAM image
c        mode      c     Fitting mode for estimator
c        aveop     c     Spatial averaging or summing switch
c        freq      r     Frequency of spectral line in GHz
c        blc,trc   i     Region of interest as supplied directly from
c                        user.   There is room for maxreg regions, but
c                        the user can only supply one with the CHAN,
c                        BLC, and TRC inputs.  Can supply a list with
c                        the INFILE.  Only first location used here,
c                        i.e. BLC(1:3,1) to TRC(1:3,1) and the locations
c                        1:3 are for channel, x and y from a VXY cube.
c        obin      i     OBIN is the number of pixels to be binned
c                        up in the image spectrally (OBIN(1)) and 
c                        spatially (OBIN(2:3)) before fitting.
c        split     r     User input splitting (split to unsplit lines)
c                        for simulation procedure. This user input value
c                        over-rides the use of the actual splitting 
c                        determined from the actual data for the 
c                        simulation procedure.
c        nsim      i     Number of simulations
c        infile    c     The name of a file containing a list of regions
c                        of interest to simulate (spatial parts only)
c                        The spectral region of interest is always given
c                        by CHAN and is therefore identical for ALL 
c                        regions and specified in BLC(1,i) and TRC(1,i)
c        log1,2    c     Log files.  See HELP file
c        nchuck    i     Number of random numbers to throw away before 
c                        beginning the simulation procedure.
c
c-----------------------------------------------------------------------
      implicit none
c
      character*(*) iin, vin, beam, infile, log1, log2, mode, aveop
      real freq, split
      integer maxreg, obin(3), nsim, nchuck, blc(3,maxreg),trc(3,maxreg)
c-----------------------------------------------------------------------
      call keyini
      call keyf ('iin', iin, ' ')
      call keyf ('vin', vin, ' ')
      if (iin.eq.' '.or.vin.eq.' ') 
     +   call bug ('f', 'Input I and V images not both specified')
      call keyf ('beam', beam, ' ')
c
      call keya ('mode', mode, ' ')
      call keya ('aveop', aveop, 'a')
      if(aveop.ne.'a' .and. aveop.ne.'s' .and. aveop.ne.'h') aveop = 'a'
      if (aveop.eq.'a') beam = ' '
c
      call keyr ('freq', freq, 0.0)
c
      call keyi ('chan', blc(1,1), 1)
      call keyi ('chan', trc(1,1), 0)
c
      call keyi ('blc', blc(2,1), 1)
      call keyi ('blc', blc(3,1), 1)
      call keyi ('trc', trc(2,1), 0)
      call keyi ('trc', trc(3,1), trc(2,1))
c
      call keyi ('bin', obin(1), 1)
      call keyi ('bin', obin(2), 1)
      call keyi ('bin', obin(3), obin(2))
c
      call keyr ('split', split, 0.0)
c
      call keyi ('nruns', nsim, 0)
      if (nsim.lt.0) call bug ('f', 'Need some NRUNS please')
      if (nsim.ne.0) then
         if (aveop.ne.'a' .and. beam.eq.' ') 
     +    call bug ('f', 'Beam image not specified')
      else
         beam = ' '
      end if
c
      call keya ('infile', infile, ' ')
      call keya ('log', log1, ' ')
      call keya ('log', log2, ' ')
c
      call keyi ('nran', nchuck, 0)
      if (nchuck.lt.0) nchuck = 1
c
      call keyfin
c
      end
c
c
      subroutine dimcheck (liin, lvin, lbin, beam, isize, vsize, bsize, 
     +                     maxdim, bxref, byref)
c-----------------------------------------------------------------------
c     Make dimension checks on arrays and images
c
c     Input:
c       l*in   i   Handles for I, V and Beam images
c       beam   r   The beam
c       *size  i   Dimensions of I, V, and Beam  images
c       maxdim i   Max allowed size for images firts dimension
c     Output:
c       b*ref  r   Reference pixel
c
c-----------------------------------------------------------------------
      implicit none
c
      integer liin, lvin, lbin, isize(3), vsize(3), bsize(2), maxdim,
     +bxref, byref
      character beam*(*)
cc
      real crpix1, crpix2
      character ctype1*8
c-------------------------------------------------------------------
      call rdhda (liin, 'ctype1', ctype1, ' ')
      if (ctype1(1:4).ne.'FREQ' .and. ctype1(1:4).ne.'VELO' .and.
     +    ctype1(1:4).ne.'FELO')
     +    call bug ('f', 'I cube not in vxy order')
c
      call rdhda (lvin, 'ctype1', ctype1, ' ')
      if (ctype1(1:4).ne.'FREQ' .and. ctype1(1:4).ne.'VELO' .and.
     +    ctype1(1:4).ne.'FELO')
     +    call bug ('f', 'V cube not in vxy order')
c
      if (isize(1).gt.maxdim .or. vsize(1).gt.maxdim)
     +    call bug ('f', 'First dimension of I or V images too large')
c
      if (beam.ne.' ') then
        call rdhdr (lbin, 'crpix1', crpix1, 0.0)
        call rdhdr (lbin, 'crpix2', crpix2, 0.0)
        if (crpix1.eq.0.or. crpix2.eq.0)
     +	  call bug ('f', 'No reference pixel for beam')
        bxref = nint(crpix1)
        byref = nint(crpix2)
        if (bsize(1).gt.maxdim)
     +    call bug ('f', 'First dimension of beam image too large')
        if (bsize(1)*bsize(2).gt.maxdim*maxdim) call bug ('f', 
     +      'Internal storage insufficient for noise generation')
      end if
c
      end
c
c
      subroutine getwin (infile, maxreg, nwin, blc, trc, iwin)
c-----------------------------------------------------------------------
c     Read multiple spatial regions from file
c
c     Input:
c       infile  c   Text file with spatial regions defined
c       maxreg  i   Max no. of allowed windows
c     Output:
c       nwin    i    Number of spatial windows
c       blc,trc i    Window corners 
c                    blc(2,iwin),blc(3,iwin) -> trc(2,iwin),trc(3,iwin)
c                    FOr the moment, the first axis window (velocity)
c                    is set to the user input values for each spatial
c                    window.  The channel range is the same for all 
c                    windows, so although there is space in blc(1,j)
c                    trc(1,j) to include a varyinf channel range,
c                    this is not yet implimented.
c                 
c       iwin    i    Window number (not necessarily in any order)
c
c-----------------------------------------------------------------------
      implicit none
c
      integer maxreg, nwin, blc(3,maxreg), trc(3,maxreg), iwin(maxreg)
      character infile*(*)
cc
      integer iostat, lInfile, pos(5), i, ilen
      character aline*100
c
      integer len1
c----------------------------------------------------------------
c
c Open text file
c
      if (infile.ne.' ') then
        call txtopen (lInfile, infile, 'old', iostat)
        if (iostat.ne.0) call bug ('f', 'Error opening window list')
c
c Read number of windows
c
        call txtread (lInfile, aline, ilen, iostat)
        if (iostat.ne.0) call bug('f','Error reading number of windows')
        ilen = len1(aline)
        call posdec1 (aline, ilen, nwin)
        if (nwin.gt.maxreg) then
          nwin = maxreg
          call bug ('w',
     +              'Reducing # of input windows to maximum allowed')
        end if
c
c Read list of window locations
c
        do i = 1, nwin
          aline = ' '
          call txtread (lInfile, aline, ilen, iostat)
          if (iostat.ne.0) call bug ('f', 
     +                    'Error reading from windows file')
          ilen = len1(aline)
          call posdec2 (i, aline, ilen, pos)
c
          iwin(i) = pos(1)
          blc(1,i) = blc(1,1)
          blc(2,i) = pos(2) - pos(4)
          blc(3,i) = pos(3) - pos(5)
          trc(1,i) = trc(1,1)
          trc(2,i) = pos(2) + pos(4)
          trc(3,i) = pos(3) + pos(5)
        end do
        call txtclose (lInfile)
      else
        iwin(1) = 1
        nwin = 1
      end if
c
      end
c
c
      subroutine posdec1 (aline, ilen, npos)
c----------------------------------------------------------------------
c     Decode string into number of positions read from positions
c     list file 
c
c     Input
c       aline   Input string
c       ilen    Length of string with trailing blanks stripped
c     Output
c       npos    Number of boxes
c----------------------------------------------------------------------
      implicit none
c
      integer npos, ilen
      character*(*)aline
cc
      integer i
      logical ok
c----------------------------------------------------------------------
      if (ilen.gt.0) then
        i = 1
        do while (aline(i:i).eq.' ' .and. i.le.ilen)
          i = i + 1
        end do
        call atoif (aline(i:ilen), npos, ok)
        if (.not.ok) call bug ('f', 
     +              'Error decoding first line of position list')
      else
        call bug ('f', 'Error decoding first line of position list')
      end if
c
      end
c
c
      subroutine posdec2 (iline, aline, ilen, pos)
c---------------------------------------------------------------------
c     Decode string into positions list
c
c     Input
c       iline    Current line number
c       aline    Input string
c       ilen     Length of string with trailing blanks ignored
c     Output
c       pos      List of: window #, xpos, ypos, xoff, yoff
c                xoff, yoff are optional, and default to 2 and 2
c                
c---------------------------------------------------------------------
      implicit none
c
      integer ilen, pos(5), iline
      character*(*) aline
cc 
      double precision val
      integer ist, iend, j, slen
      logical ok
      character estr*50, str*3
c
      integer len1
      character itoaf*3
c--------------------------------------------------------------------
      str = itoaf(iline)
      slen = len1(str)
      estr = 'Error decoding line '//str(1:slen)//' from positions list'
c
      if (ilen.gt.0) then
c
c Decode line and convert to pixels
c
        ist = 1
        j = 1
        ok = .true.
        do while (j.le.5 .and. ok)
c
c Find start and end locations of current number in string
c
          do while (aline(ist:ist).eq.' ' .and. ist.le.ilen)
            ist = ist + 1
          end do
          iend = index(aline(ist:),' ')
          if (iend.eq.0) then
            iend = ilen
          else
            iend = iend + (ist - 1) - 1
          end if
c 
c Decode number
c
          if (ist.gt.iend) then
c
c Last number was at the end of the string
c
            ok = .false.
          else
c
c Try to extract current number from string
c
            call atodf (aline(ist:iend), val, ok) 
          end if
          if (ok) then
c
c Decode
c
            if (j.le.3) then
              pos(j) = nint(val)
            else
              pos(j) = abs(val)
            end if
          else 
c
c Catch error or set defaults
c
            if (j.le.3) then
              call bug ('f', estr)
            else if (j.eq.4) then
              pos(4) = 2
              pos(5) = 2
            else if (j.eq.5) then
              pos(5) = 2
            end if
          end if
          ist = iend + 1
          j = j + 1
        end do
      else
        call bug ('f', 'Error decoding  line from position list')
      end if
c
      end
c
c
      subroutine fixwin (beam, aveop, maxreg, isize, vsize, bsize, obin,
     +   bin, blc, trc, nwin, nbin, wsize, totsize, maxy)
c-----------------------------------------------------------------------
c     Do some more dimension checking, fix up blc, trc defaults and
c     deal with binning
c
c     Input:
c        beam    c    Beam image name.  WIll be filled only for
c                     summing (or hybrid) and if NSIM non-zero
c        aveop   c    'a' for averaging, 's' for summing, 'h' for
c                     averaging/summing hybrid
c        maxreg  i    Maximum allowed number of windows
c        isize   i    Dimensions of I image
c        vsize   i    Dimensions of V image
c        bsize   i    Dimensions of beam image
c        obin    i    User given binning widths, v,x,y
c        nwin    i    Number of regions of interest
c     Input/output:
c        blc,trc i    Corners of regions of interest. May be
c                     adjusted so bin widths fit integrally
c     Output:
c        bin     i    Binning widths, one for each region. For spatial
c                     averaging, the bin width is set to the window 
c                     size. The bin width on the first axis (v) is always
c                     that given by the user (OBIN(1)) 
c                     For summing, BIN is the same for all windows 
c                     and equal to OBIN
c                    
c        nbin    i    Binned sizes of regions of interest (windows)
c                     For spatial averaging, these will be 1 for each
c                     winodw on the second and third (spatial) axes
c        wsize   i    Total number of (binned) pixels (v*x*y) for 
c                     each window
c        totsize i    Sum of WSIZEs over all  windows.  Does not
c                     get checked to see whether it is too big for
c                     available memory until subroutine READWIN
c        maxy    i    This is the maximum y-height of all the binned
c                     spatial regions.  Used for stepping up the noise
c                     image.
c
c-----------------------------------------------------------------------
      implicit none
c
      integer maxreg, isize(3), vsize(3), bsize(2), nwin, obin(3),
     +blc(3,maxreg), trc(3,maxreg), nbin(3,maxreg), wsize(nwin), 
     +totsize, bin(3,maxreg), maxy
      character aveop*1, beam*(*)
cc
      integer i, j
c----------------------------------------------------------------------
      do i = 1, 3
         if (isize(i).ne.vsize(i))
     +     call bug('f', 'Input I and V images are different sizes')
      end do
c
      totsize = 0
      maxy = 0
      do j = 1, nwin
        wsize(j) = 1
        do i = 1, 3
c
c Fill in defaults and check for invalid windows
c
          if (trc(i,j).eq.0) trc(i,j) = isize(i)
          if (blc(i,j).lt.1 .or. trc(i,j).gt.isize(i) .or.
     +        blc(i,j).gt.trc(i,j)) call bug ('f', 'Invalid blc or trc')
c
c Fix up window sizes to reflect binning
c
          bin(i,j) = obin(i)
          call binfid (j, aveop, isize(i), i, bin(i,j), blc(i,j), 
     +                 trc(i,j), nbin(i,j))
          wsize(j) = wsize(j) * nbin(i,j)
        end do
        maxy = max(maxy,nbin(3,j))
        totsize = totsize + wsize(j)
c
        if (beam.ne.' ' .and. 
     +    (nbin(2,j).gt.bsize(1) .or. nbin(3,j).gt.bsize(2)) )
     +    call bug ('f', 'Spatial window bigger than beam')
      end do
c
      end
c
c
      subroutine bfac (liin, obin, freq, scale)
c----------------------------------------------------------------------
c     Find the conversion factor from a splitting in channels to
c     magnetic field strength
c
c     Input:
c       liin    i    handle for I image
c       obin    i    SPectral binning width
c       freq    r    Frequency of line (GHz)
c     Output:
c       scale   r    B=2*scale*alpha Gauss, where alpha is the
c                    splitting returned by subroutine ZED
c                    If the line is not recognize,  SCALE=0.0
c-----------------------------------------------------------------------
      implicit none
c
      integer liin, obin
      real freq, scale
cc
      logical noline
c-----------------------------------------------------------------------
      if (freq.ne.0.0) call zedscale (liin, freq, scale, noline)
      if (noline .or. freq.eq.0.0) then
         if (noline) call bug ('w', 'Spectral line not matched')
         scale = 0.0
      else
         scale = real(obin) * scale
      end if
c
      end
c
c
      subroutine readwin (liin, lvin, totsize, maxreg, wsize, iwin,
     +                    bin, nbin, blc, trc, nwin, pt, row, 
     +                    ispect, vspect, nsp)
c-----------------------------------------------------------------------
c     Read in the I and V spectra for all regions, and bin them up
c     in all three dimensions as specified by the user.
c
c     Memory allocation is done via the pseudo dynamic memalloc
c     routines before calling this subroutine.   If there is
c     insufficient memory for all the regions, we will have bombed
c     already.   There is no longer dropping of some of the number of
c     specified regions to get some of them in memory.  All or nothing.
c
c     Input:
c        liin,lvin  i   Handles for I and V images
c        maxreg     i   Maximum number of regions allowed
c        wsize      i   Size of each (binned) region (v*x*y) 
c                       This is the product of the nbin(1)*(2)*(3)
c        iwin       i   Region number (not necesarily in any order)
c        bin        i   Binning widths (v,x,y), one for each region
c        nbin       i   Binned sizes of regions of interest. One for 
c                       each of v,x, and y
c        blc,trc    i   Corners of regions of interest. 
c        totsize    i   Size of sum of sizes of all regions (WSIZE)
c        nwin       i   Number of regions of interest. 
c     Input/output:
c        pt         i   Points to the first memory location in ispect
c                       and vspect for the specified region.  Thus,
c                       pt(3) is the first location for region number 3
c        row        r   I/O array for reading one row from an image
c        i,vspect   r   Arrays to hold all (binned) spectra from all 
c                       regions from I and V images.  Spectra from regions
c                       are stored sequentially, v,x,y.  The regions
c                       are read in the order given by BLC and TRC
c      Output:
c        nsp        i   Number of spectra binned together for each 
c                       region.  For averaging, this would be the number
c                       of spatial pixels in the region.  For summing
c                       and no spatial binning, this would be 1.
c
c-----------------------------------------------------------------------
      implicit none
c
      integer maxreg, totsize, iwin(maxreg), pt(maxreg), nwin, 
     +wsize(nwin), bin(3,maxreg), blc(3,maxreg), trc(3,maxreg), 
     +nbin(3,maxreg), nsp(maxreg), liin, lvin
      real ispect(*), vspect(*), row(*)
cc
      integer h
      character line*80
c-----------------------------------------------------------------------
c
c Initialize array locations to 0.0 for binning procedure.
c
      call initar (totsize, ispect, vspect)
c
c Loop over number of windows
c
      pt(1) = 1
      do h = 1, nwin
c
c Read in spectra for current window; bin them as required
c The array PT points to the next available memory location in
c ispect and vspect for the current region of interest
c
        write (line,'(a,i5,a,3(i7,2x))') 'Region ', iwin(h), 
     +   ':  Memory size,start,end=', wsize(h), pt(h), pt(h)+wsize(h)-1
        call output (line)
c
        call binrd2 (liin, lvin, bin(1,h), blc(1,h), trc(1,h),
     +               nbin(1,h), row, ispect(pt(h)), vspect(pt(h)))
        nsp(h) = nbin(2,h) * nbin(3,h)
c
c Work out start pointer for next window
c
        if (h.lt.nwin) pt(h+1) = pt(h) + wsize(h)
      end do
c
      end
c
c   
      subroutine initar (totsize, ispect, vspect)
c-----------------------------------------------------------------------
c     Initialize all locations that spectra will be read into to 0.0
c     Must do this because the binning routines ADD data to output
c     array locations. 
c-----------------------------------------------------------------------
      implicit none
c 
      integer totsize
      real ispect(totsize), vspect(totsize)
cc
      integer i
      character aline*80
c-----------------------------------------------------------------------
      do i = 1, totsize
         ispect(i) = 0.0
         vspect(i) = 0.0
      end do
      call output (' ')
      write (aline,'(a,2x,i7)') 
     +   'Total memory size required for spectra = 2 x', totsize
      call output (aline) 
c
      end
c
c
      subroutine fits (llog, mode, scale, split, nsim, nwin, sigim,
     +                 totsize, ispect, vspect, pt, nbin, nsp, iwin,
     +                 a, siga, eta2, sigi, ihat, vhat, delta)
c-----------------------------------------------------------------------
c     Find the splitting for each region of interest and compute the
c     estimates of the noiseless spectra used in the simulation
c     procedure
c
c     Input:
c       llog        i    Handles for log files when doing no simulations
c       mode        c    Fitting mode
c       scale       r    Conversion factor from channels to B
c       split       r    User input channel splitting for calculating
c                        vhat.  If not given, fitted splitting used
c       nsim        i    Number of simulations.  If 0, don;t do extra
c                        steps of finding smoothed noiseless spectra 
c       nwin        i    Number of regions of interest
c       totsize     i    Array size needed for all binned spectra from 
c                        all regions
c       i,vspect    r    Binned I and V spectra from regions of interest
c       pt          i    Points to first memory location in data arrays
c                        for each window.
c       nbin        i    Binned sizes of regions of interest.  One for
c                        each of v,x, and y
c       nsp         i    Number of (binned) spectra in each region
c       iwin        i   Region number (not necesarily in any order)
c     Output:
c       sigim       r    Mean of estimates for noise per pixel from all
c                        regions of interest.  Used for summing and
c                        hybrid options
c       a           r    Estimates of splitting for each region 
c       siga        r    Estimates of error in splitting for each region
c       sigi        r    Estimates of noise per pixel in I spectra for 
c                        each region.  Used for spatial averaging only.
c       i,vhat      r    Estimates of 3-point Hanning smoothed smoothed 
c                        noiseless I and V spectra.  vhat computed from 
c                        ihat and splitting.  The latter may be specified 
c                        by the user. 
c       delta       i    1 for 2-sided derivative, 0 for 1-sided
c       eta2        r    Computed eta**2 for each window
c                     
c-----------------------------------------------------------------------
      implicit none
c
      integer nwin, totsize, pt(nwin), nbin(3,nwin), nsp(nwin),
     +iwin(nwin), delta, nsim, llog(nwin)
      real sigim, ispect(totsize), vspect(totsize), ihat(totsize), 
     +vhat(totsize), a(nwin), siga(nwin), sigi(nwin), eta2(nwin), 
     +scale, split
      character mode*(*)
cc
      integer h
      real b, sigb, etahat2
      character str*3, itoaf*3
      logical convrg
c-----------------------------------------------------------------------
      if (index(mode, '2').ne.0) then
        delta = 1
      else
        delta = 0
      end if
c
c Loop over windows
c 
      sigim = 0.0
      do h = 1, nwin
c
c Do fit and find bias parameter, eta
c
        call zed (mode, ispect(pt(h)), vspect(pt(h)), nbin(1,h), nsp(h),
     +            a(h), b, siga(h), sigb, sigi(h), convrg)
        call zedeta (ispect(pt(h)), nbin(1,h), nsp(h), sigi(h), delta, 
     +               eta2(h), etahat2)
        sigim = sigim + sigi(h)
c        
c Tell user results
c
        if (.not.convrg)
     +      call bug('w', 'Fitting algorithm did not converge')
c
        call output (' ')
        call output (' ')
        str = itoaf(iwin(h))
        call output ('Initial results for window  '//str)
        call lisres (nsim, llog(h), mode, scale, nbin(1,h), nsp(h),
     +               a(h), siga(h), b, sigb, sigi(h), eta2(h), convrg)
c
c Compute the hatI and HatV spectra, and 3 point hanning smooth them
c
        if (nsim.ne.0) then
          call zedihat (mode, ispect(pt(h)), vspect(pt(h)), nbin(1,h),
     +                  nsp(h), a(h), b, ihat(pt(h)))
          if (split.ne.0.0) a(h) = split      
          call zedvhat (mode, ihat(pt(h)), nbin(1,h), nsp(h), a(h), b,
     +                  vhat(pt(h)))
          call h3sm (nbin(1,h), nsp(h), ihat(pt(h)), vhat(pt(h)))
        end if
      end do
c
      sigim = sigim / nwin
c
      end
c
c
      subroutine noisefid (bin, size, blc, trc, nbin)
c-----------------------------------------------------------------------
c     Adjust the size of one axis of the noise image so that the bin 
c     width fits an integer number of times 
c
c     Input:
c        bin       i    Binning size
c        size      i    Unbinned size of beam axis  
c     Output:
c        blc,trc   i    Adjusted window
c        nbin      i    Binned size of axis
c
c-----------------------------------------------------------------------
      implicit none
      integer size, bin, blc, trc, nbin
cc
      integer rem
c-----------------------------------------------------------------------
      nbin = size
      blc = 1
      trc = size
      rem = mod(nbin,bin)
c
c Adjust window to fit integral number of bins.  Increment BLC by 1
c and decrement TRC by 1 until ok.
c
      do while (rem.ne.0) 
         if (blc.eq.trc) call bug ('f', 'Error binning beam')
c
         blc = blc + 1
         nbin = trc - blc + 1
         rem = mod(nbin,bin)
c
         if (rem.ne.0) trc = trc - 1
         nbin = trc - blc + 1
         rem = mod(nbin,bin)
      end do
c
c Adjust window size to binned size
c
      nbin = nbin / bin
c
      end
c
c
      subroutine adnoids (totsize, nwin, nchan, pt, sigi, ihat, vhat,
     +                    noise, ispect, vspect)
c-----------------------------------------------------------------------
c     Add uncorrelated noise back into the I and V spectra
c
c     Input:
c       totsize  i    SIze of spectrum arrays
c       nwin     i    Number of windows
c       nchan    i    Number of channels
c       pt       i    Array pointers for each region
c       sigi     r    Noise per pixel for each spectrum
c       ihat     r    smoothed estimate of noiseless I spectrum
c       vhat     r    smoothed estimate of noiseless V spectrum
c     Input/output:
c       noise    r    Array to put noise in
c     Output:
c       ispect   r    Regenerated noisy I spectrum
c       vspect   r    Regenerated noisy V spectrum
c
c-----------------------------------------------------------------------
      implicit none
c
      integer totsize, nwin, nchan, pt(nwin)
      real sigi(nwin), ihat(totsize), vhat(totsize),
     +ispect(totsize), vspect(totsize), noise(2*nchan)
cc
      integer h, i
c-----------------------------------------------------------------------
      do h = 1, nwin
         call genuncor (nchan*2, sigi(h), noise)
         do i = 1, nchan
            ispect(pt(h)+i-1) = ihat(pt(h)+i-1) + noise(i)
            vspect(pt(h)+i-1) = vhat(pt(h)+i-1) + noise(i+nchan)
         end do
      end do
c
      end
c
c
      subroutine reform (aveop, nwin, nchan, nsp, idim, pt, ispect, 
     +                   vspect, isp)
c-----------------------------------------------------------------------
c     Reform the spatial region averaged I and V spectra from the 
c     simulated noisy spectra if using the hybrid mode.
c
c     Input:
c        aveop     a     SVeraging option
c        nwin      i     Number of regions of interest
c        nchan     i     Number of channels
c        nsp       i     Number of spectra in each region
c        idim      i     Dimensions of ISPECT and VSPECT
c     Input/output:
c        i,vspect  r     I and V spectra.  Average spectra overwritten
c                        into these arrays
c        isp       i     Number of avearged spectra in each
c                        region.  This will change from NSP to 1
c                        in the hybrid mode.  Otherwise, ISP=NSP
c                        
c-----------------------------------------------------------------------
      implicit none
c
      character aveop*1
      integer nwin, nchan, nsp(nwin), idim, isp(nwin), pt(nwin)
      real ispect(idim), vspect(idim)
cc
      integer h
c-----------------------------------------------------------------------
      if (aveop.eq.'h') then
         do h = 1, nwin
            call specav (nchan, nsp(h), ispect(pt(h)))
            call specav (nchan, nsp(h), vspect(pt(h)))
            isp(h) = 1
         end do
      else
         do h = 1, nwin
           isp(h) = nsp(h)
         end do
      end if
c
      end
c
c
      subroutine specav (nchan, nspec, spec)
c----------------------------------------------------------------------
c     Average spectra over spatial window for hybrid 
c     average/summing option and overwrite first spectrum
c     in the array with it.
c----------------------------------------------------------------------
      implicit none
c
      integer nchan, nspec
      real spec(nchan,nspec)
cc
      integer i, j
      real sum
c----------------------------------------------------------------------
      do i = 1, nchan
         sum = 0.0
         do j = 1, nspec
            sum = sum + spec(i,j)
         end do
         spec(i,1) = sum / nspec
      end do
c
      end
c
c
      subroutine lisres (nsim, llog, mode, scale, nchan, nsp, a,
     +                   siga, b, sigb, sigi, eta2, convrg)
c-----------------------------------------------------------------------
c     Output results of ZED to terminal and log file if not doing
c     simulations
c----------------------------------------------------------------------
      implicit none
c
      integer nsp, nchan, llog, iostat, nsim
      real a, siga, b, sigb, sigi, eta2, scale
      character mode*(*), aline*100
      logical convrg
cc
      integer len1
      character conv*1
c-----------------------------------------------------------------
c
c Output to log file
c
      if (llog.ne.-1 .and. nsim.eq.0) then
         conv = 'n'
         if (convrg) conv = 'y'
         write (aline, 50) mode(1:len1(mode)), sqrt(eta2), 
     +      sigi, a, siga, 2.0E3*scale*a, 2.0E3*abs(scale)*siga, conv
50       format (a, 2x, f6.2, 2x, 1pe11.4, 2x, 0pf7.4, 3x, 0pf6.4, 
     +           2x, 2(1pe11.4, 2x), a)
         call txtwrite (llog, aline, len1(aline), iostat)
      end if
c
c Output to terminal
c
      write (aline, 100) mode
100   format ('Mode:                          ', a)
      call output (aline)
c
      write (aline, 200) nchan
200   format ('Number of channels:            ', i4)
      call output (aline)
c
      write (aline, 300) nsp
300   format ('Number of spectra:             ', i4)
      call output (aline)
c
      if (convrg) then
         call output ('Convergence achieved')
      else
         call output ('No convergence ahieved')
      end if
c
      write (aline, 400) a, siga
400   format ('Channel Splitting,  sigma:     ',1pe12.4,',',1pe12.4)
      call output (aline)
c
      if (scale.ne.0.0) then
         write (aline,500) 1.0E3*scale*2.0*a,1.0E3*abs(scale)*2.0*siga
500      format ('B field strength, sigma (mG):  ', f8.3, ',', f8.3)
         call output (aline)
      end if
c
      write (aline, 600) b, sigb
600   format ('Leakage gain, sigma:       ', f8.3, ',', f8.3)
      call output (aline)
c
      write (aline, 700) sigi
700   format ('Rms noise per pixel:       ', 1pe12.4)
      call output (aline)
c
      write (aline, 800) sqrt(eta2)
800   format ('Estimated eta:             ', 1pe12.4)
      call output (aline)
c
      end
c
c
      subroutine h3sm (nchan, nsp, ihat, vhat)
c------------------------------------------------------------------
c     3 point Hanning smooth the estimates of the noiseless
c     I and V spectra.  
c
c  Input:
c    nchan     Number of channels in each spectrum
c    nsp       Number of spectra 
c
c------------------------------------------------------------------
      implicit none
c
      integer nchan, nsp
      real ihat(nchan,nsp), vhat(nchan,nsp)
cc
      integer hann
      parameter (hann = 3)
c
      real coeffs(hann), work(hann)
      integer j
c------------------------------------------------------------------
      call hcoeffs (hann, coeffs)
      do j = 1, nsp
        call hannsm (hann, coeffs, nchan, ihat(1,j), work)
        call hannsm (hann, coeffs, nchan, vhat(1,j), work)
      end do
c
      end
c
c
      subroutine runfill (bsize, maxruns, nruns, runs)
c-------------------------------------------------------------------------
c     The convolution routines need the blanking array RUNS.  Fill
c     it so that the beam image is completely unblanked
c-------------------------------------------------------------------------
      implicit none
c
      integer maxruns, nruns, runs(3,maxruns), bsize(2)
cc
      integer i
c-------------------------------------------------------------------------
      nruns = bsize(2)
      do i = 1, bsize(2)
        runs(1,i) = i
        runs(2,i) = 1
        runs(3,i) = bsize(1)
      end do
c
      end
c
c
      subroutine tossran (nchuck, idim, noise)
c-------------------------------------------------------------------
c     Throw away some random numbers
c-------------------------------------------------------------------
      implicit none
c
      integer idim, nchuck
      real noise(idim)
cc
      integer npass, irem, ntoss, i
c-------------------------------------------------------------------
      if (nchuck.ge.1) then
        call output ('Throwing away random numbers')
        if (nchuck.lt.idim) then
          npass = 1
          irem = nchuck
        else
          irem = mod(nchuck,idim)
          if (irem.eq.0) then
            npass = nchuck / idim
          else
            npass = nchuck / idim + 1
          end if
        end if
c
        ntoss = idim
        do i = 1, npass
          if (i.eq.npass) ntoss = irem
          call gaus (noise, ntoss)
        end do
      end if
c
      end
c
c
      subroutine telluser (nblc, ntrc, obin, nnbin, sigb, nsigma)
c-----------------------------------------------------------------------
c     Tell user some things about noise levels
c-----------------------------------------------------------------------
      implicit none
c
      integer nblc(2), ntrc(2), obin(3), nnbin(2)
      real sigb, nsigma
cc
      character aline*80
c-----------------------------------------------------------------------
      write (aline,'(4(a,i4))') 'Noise image binned over', nblc(1), 
     +                 ',', nblc(2), ' to ', ntrc(1), ',', ntrc(2)
      call output (aline)
c
      write (aline,'(2(a,i4))') 'Noise binning size = ',obin(2),',',
     +                           obin(3)
      call output (aline)
c
      write (aline,'(2(a,i4))') 'Binned noise image size = ',
     +                           nnbin(1),',',nnbin(2)
      call output (aline)
      call output (' ')
c
      write (aline,'(a,1pe12.4)') 
     +    'After convolution and binning, noise increases by', sigb
      call output (aline)
c
      write (aline,'(a,1pe12.4)') 
     +    'Therefore, uncorrelated noise level = ', nsigma
      call output (aline)
c
      end
c
c
      subroutine morenois (maxy, nnbin, nbin, xoff, yoff, more)
c----------------------------------------------------------------------
c     See if there is room on the noise image for the current window.  
c
c     Input:
c       maxy    i     Greatest y-height from all the spatial regions
c       nnbin   i     x and y sizes of the (binned) noise image
c       nbin    i     x,y sizes of the current (binned) spatial window 
c     Inout/output:
c       x,yoff  i     Input: x,y offsets from (1,1) in the noise image 
c                     defining the BLC of the noise image to use for 
c                     this window
c                     Output: If the right hand edge or the top of
c                     the noise image is reached, they will be adjusted.
c                     In the former case, XOFF will be reset to 0, and
c                     YOFF incremented up the image.  In the latter case,
c                     both will be reset to 0, and MORE set to .TRUE.
c    Output:
c       more    l     Need to make more noise
c       
c-----------------------------------------------------------------------
      implicit none
c
      integer nbin(2), nnbin(2), xoff, yoff, maxy
      logical more
c-----------------------------------------------------------------------
      more = .false.
      if (xoff+nbin(1).gt.nnbin(1)) then
c
c Off the right-hand edge
c
         xoff = 0
         yoff = yoff + maxy
      end if
c
      if (yoff+nbin(2).gt.nnbin(2)) then
c
c Off the top; need new image
c
         more = .true.
         xoff = 0
         yoff = 0
      end if
c
      end
c
c
      subroutine gencor (bsize, nsdim, fftdim, sigi, bemfft, noise, 
     +                   noisec, nruns, runs)
c---------------------------------------------------------------------
c     Generate a correlated noise image
c
c     Input:
c        bsize     i     Size of beam
c        nsdim     i     Size of noise arrays
c        fftdim    i     Size of FFT array
c        sigi      r     Sigma of noise to add to noiseless estimates
c        bemfft    r     Array containing beam FFT
c     Input/output:
c        noise     r     Array to put uncorrelated noise in
c        noisec    r     Array to put correlated noise in
c        nruns     i     nruns for blanking mask regions
c        runs      i     blanking mask
c
c---------------------------------------------------------------------
      implicit none
c
      integer bsize(2), nsdim, fftdim, nruns, runs(3,nruns)
      real noise(nsdim), noisec(nsdim), bemfft(fftdim), sigi
cc
      integer k
c---------------------------------------------------------------------
c
c Compute an image of uncorrelated noise
c
      call gaus (noise, nsdim)
c
      if (sigi.ne.1.0) then
         do k = 1, nsdim
           noise(k) = sigi * noise(k)
         end do
      end if
c
c Convolve noise image by the beam
c
      call Convl (noise, noisec, nsdim, bsize(1), bsize(2), runs,
     +            nruns, bemfft, bsize(1), bsize(2))
c
      end
c
c
      subroutine binoise (nx, ny, nsdim, nbin, bin, blc, trc, 
     +                    noisein, noisebin)
c-----------------------------------------------------------------------
c     Bin up the correlated noise image in the same way as the 
c     I and V spectra were binned up spatially.
c
c     Input:
c       nx,ny    i    Unbinned x and y sizes of the noise image
c       nsdim    i    Size of noise arrays
c       nbin     i    Binned x and y sizes of noise image
c       bin      i    x and y binning sizes
c       blc,trc  i    Window on image to bin.  This has an integral 
c                     number of binned cells in it.
c       noisein  r    Correlated noise image
c    Output:
c       noisebin r    Binned correlated noise image.
c
c-----------------------------------------------------------------------
      implicit none
      integer nsdim, bin(2), nbin(2), nx, ny, blc(2), trc(2)
      real noisein(nx,ny), noisebin(nsdim)
cc
      real norm
      integer j, joff, jbin, pt
c-----------------------------------------------------------------------
c
c Initialize output array
c
      do j = 1, nbin(1) * nbin(2)
         noisebin(j) = 0.0
      end do
c
c Output data are normalized by BIN(1)*BIN(2).  The BIN(1) part of
c the normalization occurs in BINUP.  BIN(2) is passed in as an
c additional normalization factor.
c
      norm = real(bin(2))
c
c Loop over all rows
c
      do j = blc(2), trc(2)
        joff = j - blc(2) + 1
        jbin = joff / bin(2)
        if (mod(joff,bin(2)).ne.0) jbin = jbin + 1
c
c PT is the desired (to accomplish y-binning) location in the output array
c of the first pixel of the first binned point from the next row.  
c Binned rows are joined end to end
c
        pt = (jbin - 1)*nbin(1) + 1
c
c Binup in x-direction and apply correct x-y normalization factor
c
        call binup (noisein(1,j), blc(1), trc(1), bin(1),
     +              norm, pt, noisebin, .false.)
      end do
c
      end
c
c
      subroutine simini (mode, scale, nwin, log1, log2, nsum, suma,
     +                   sumsqa, llog, lfid, a, siga, eta2)
c-------------------------------------------------------------------------
c     Initialize sums and files for simulation
c-------------------------------------------------------------------------
      implicit none
c
      integer nwin, nsum(nwin), llog(nwin), lfid
      real suma(nwin), sumsqa(nwin), a(nwin), siga(nwin), scale, 
     +eta2(nwin)
      character*(*) log1, log2, mode
cc
      integer iostat, h
      character aline*100
c
      integer len1
c-------------------------------------------------------------------------
      call output (' ')
      call output (' ')
      call output ('Begin simulation procedure')
      call output ('**************************')
c
c Loop over number of regions
c
      do h = 1, nwin
c
c Write initial fit results; a, siga, B, sigB to cumulative log file
c
         if (log1.ne.' ') then
            if (scale.ne.0.0) then
              write(aline, 100) a(h), siga(h), sqrt(eta2(h))
100           format (' alpha = ', 1pe12.4, ', sig_alpha = ', 1pe12.4,
     +                ', eta = ', 0pf7.3)
              call txtwrite (llog(h), aline, len1(aline), iostat)
              if (iostat.ne.0) call bug('f','Error writing to log file')
c
              write(aline, 150)2.0E3*scale*a(h),2.0E3*abs(scale)*siga(h)
150           format (5x, 'B = ', 1pe12.4, ',', 5x, 'sig_B = ', 
     +                1pe12.4, ' mG')
            else
              write (aline, 200) a(h), siga(h)
200           format (' alpha = ', 1pe12.4, ',  sig_alpha = ', 1pe12.4)
            end if
            call txtwrite (llog(h), aline, len1(aline), iostat)
            if (iostat.ne.0) call bug ('f', 'Error writing to log file')
c
c Write titles
c
            aline = 'SIM     alpha     sig_alpha      '//
     +              'sigi         eta        ff     conv'
            call txtwrite (llog(h), aline, len1(aline), iostat)
            if (iostat.ne.0) call bug ('f', 'Error writing to log file')
        end if
c
c Initialize sums
c
        suma(h) = 0.0
        sumsqa(h) = 0.0
        nsum(h) = 0
      end do
c
c Open fiddle factor file if requested.  Do it now rather than
c later in case an error occurs opening it.  Don't want to waste
c all those simulations do we ??
c
      if (log2.ne.' ') then
        call txtopen (lfid, log2, 'new', iostat)
        if (iostat.ne.0) call bug ('f', 
     +                             'Error opening fiddle factor file')
        aline = 'mode = '//mode
        call txtwrite (lfid, aline, len1(aline), iostat)
        aline = ' Win   eta    alpha    sig_alpha     scale     '//
     +          'mean       sdev    ncon  FF'
        call txtwrite (lfid, aline, len1(aline), iostat)
        if (iostat.ne.0) 
     +     call bug ('f', 'Error writing to fiddle factor file')
      end if
c
      end
c
c
      subroutine openlog (log1, nwin, iwin, ilen, outfile, llog, old)
c-----------------------------------------------------------------------
c     Open a log file for each region of interest with a common root.
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nwin, llog(nwin), ilen(nwin), iwin(nwin)
      character*(*) outfile(nwin), log1
      logical old(nwin)
cc
      integer h, len1, iostat
      character str*3, itoaf*3, aline*132
c-----------------------------------------------------------------------
      if (log1.ne.' ') then
         do h = 1, nwin
            str = itoaf(iwin(h))
            outfile(h) = log1(1:len1(log1))//'_'//str
            ilen(h) = len1(outfile(h))
c
            call txtopen (llog(h), outfile(h), 'append', iostat)
            if (iostat.ne.0) then
              aline = 'Unable to open log file '//outfile(h)
              call bug ('f', aline)
            end if
            old(h) = .false.
         end do
      else
         do h = 1, nwin
            outfile(h) = ' '
            llog(h) = -1
         end do
      end if
c
      end
c
c
      subroutine logtit (nwin, llog, old)
c-----------------------------------------------------------------------
c     Write title for log files when not doing simulations
c----------------------------------------------------------------------
      implicit none
c
      integer nwin, llog(nwin)
      logical old(nwin)
cc
      integer h, len1, iostat, ilen
      character aline*80
c-----------------------------------------------------------------------
      aline = 'mode   eta     sigi        a      siga'//
     +        '        B (mG)      sigB       c'
      ilen = len1(aline)
c
      do h = 1, nwin
         if (.not.old(h)) then
            call txtwrite (llog(h), aline, ilen, iostat)
            if (iostat.ne.0) 
     +        call bug ('f', 'Error writing title to log file(s)')
         end if
      end do
c
      end
c
c
      subroutine tonsils (itype, isim, choff, ichan, nwin, maxy, nnbin, 
     +   nbin, xoff, yoff, iwin, bsize, nsdim, fftdim, nsigma, bemfft,
     +   noise, noisec, nruns, runs, obin, nblc, ntrc, nchan, nsp, 
     +   totsize, hat, pt, spect)
c-----------------------------------------------------------------------
c     Determine if a new noise image must be computed, if so generate
c     an image of uncorrelated noise, convolve it by the beam, and
c     bin it up as desired. Then add the binned noise into the noiseless
c     spectrum estimaets for the current channel
c
c     Input:
c        itype     i     1 if on I image
c                        2 if on V image
c        isim      i     Simulation number
c        ichan     i     Channel number (starts at 1).  Only channels
c                        starting at the first specified by the user 
c                        were loaded into the data arrays.
c        choff     i     True channel = ICHAN+CHOFF
c        nwin      i     Number of regions of interest
c        maxy      i     Maximum y-size from all regions
c        nnbin     i     Binned size of noise image (xy)
c        nbin      i     Binned size of regions of interest (vxy)
c        iwin      i     Array of region of interest identifying numbers
c                        as specified in the input file by the user.
c        bsize     i     Array of beam image dimensions
c        nsdim     i     Size of noise arrays
c        fftdim    i     Size of FFT array
c        nsigma    r     Standard deviation of uncorrelated noise image
c        bemfft    r     FFT of beam
c        nruns     i     nruns for blanking mask regions
c        runs      i     blanking mask
c        obin      i     x and y binning sizes 
c        nblc,trc  i     Window on noise image to bin. An integral 
c                        number of binned cells fits in it.
c        nchan     i     Number of channels in spectra
c        nsp       i     Number of spectra in each region of interest
c        totsize   i     Size of HAT and SPECT
c        hat       r     Estimate of the noiseless spectra after
c                        Hanning smoothing
c        pt        i     Points to the first memory location in SPECT
c                        and HATsm for the specified region.  Thus, pt(3)
c                        is the first location for region number 3
c     Input/output:
c        x,yoff    i     x,y offsets from (1,1) in the noise image 
c                        defining the BLC to use for first region of
c                        interest.  On output, incremented ready for
c                        the next window, but not checked for validity.
c                        This is done by MORENOIS after entering this 
c                        subroutine, which may reset them to 0 if a new
c                        noise image is created, or reset XOFF to 0 if 
c                        the right hand edge of the noise image reached
c        noise     r     Array to hold first uncorrelated noise and
c                        then binned correlated noise
c        noisec    r     Array to hold correlated noise
c     Output:
c        spect     r     Regenerated noisy spectra.  Only one channel
c                        done per pass through this subroutine
c
c-----------------------------------------------------------------------      
      implicit none
c
      integer isim, choff, ichan, nwin, maxy, nnbin(2), nbin(3,nwin),
     +xoff, yoff, iwin(nwin), bsize(2), nsdim, fftdim, nruns, pt(nwin),
     +runs(3,nruns), obin(2), nblc(2), ntrc(2), nchan, nsp(nwin),
     +totsize, itype
      real nsigma, bemfft(fftdim), noise(nsdim), noisec(nsdim),
     +hat(totsize), spect(totsize)
cc
      integer h
      real mean, sig
      logical more
      character aline*80
      character str(2)*3
c  
      data str /'I: ', 'V: '/
c-----------------------------------------------------------------------
c
c Loop over all regions of interest
c
      do h = 1, nwin
c
c Do we need another noise image ?  Force on first simulation
c
         if (isim.eq.1.and.ichan.eq.1.and.h.eq.1.and.itype.eq.1) then
            more = .true.
            xoff = 0
            yoff = 0
         else
            call morenois (maxy, nnbin, nbin(2,h), xoff, yoff, more)
         end if
c
         if (more) then
            write (aline,100) str(itype), isim, ichan+choff, iwin(h)
100         format (a, 'New noise image generated at simulation ',
     +              i3, ' channel ', i3, '  window', i2)
            call output (aline)
            call gencor (bsize, nsdim, fftdim, nsigma, bemfft,
     +                   noise, noisec, nruns, runs)
            call binoise (bsize(1), bsize(2), nsdim, nnbin, 
     +                    obin, nblc, ntrc, noisec, noise)
            call stats (nnbin(1)*nnbin(2), noise, mean, sig, .false.)
         end if
c
c Add the noise  for this channel
c
         call addnoise (nnbin(1), nnbin(2), nbin(2,h), ichan, nchan,
     +      nsp(h), xoff, yoff, hat(pt(h)), noise, spect(pt(h)))
         xoff = xoff + nbin(2,h)
      end do
c
      end
c
c
      subroutine genuncor (nchan, sigi, noise)
c---------------------------------------------------------------------
c     Generate an uncorrelated  noise spectrum
c
c     Input:
c        nchan     i     Number of channels
c        sigi      r     Sigma of noise 
c     Input/output:
c        noise     r     Array to put uncorrelated noise in
c---------------------------------------------------------------------
      implicit none
c
      integer nchan
      real noise(*), sigi
cc
      integer k
c---------------------------------------------------------------------
c
c Compute uncorrelated noise
c
      call gaus (noise, nchan)
      do k = 1, nchan
        noise(k) = sigi * noise(k)
      end do
c
      end
c
c
      subroutine addnoise (nnx, nny, n, ichan, nchan, nsp, xoff, 
     +                     yoff, hatspec, noise, spec)
c----------------------------------------------------------------------
c     Add the convolved image of noise to the estimates of the 
c     noiseless spectra from the current window and for the 
c     current channel.  
c
c     Input:
c       nnx,y       i     x and y sizes of the (binned) noise image
c       n           i     x,y sizes of the current (binned) spatial window 
c       ichan       i     Channel for which we are adding in spatially
c                         correlated noise to the noiseless estimates
c       nchan       i     Number of channels
c       nsp         i     Number of spectra in current window
c       hatspec     r     Noiseless estimate of the spectra from
c                         the current spatial window.  Spectra are
c                         stored end to end.  First location of hatspec
c                         passed in must be the locaiton of the first 
c                         channel from the first spectrum from the
c                         current window 
c       noise       r     Image of noise to add in
c     Input:
c       x,yoff      i     x,y offsets from (1,1) in the noise image 
c                         defining the BLC of the noise image to use 
c                         for this window.  Validity of X,YOFF 
c                         checked in MORENOISE
c     Output:
c       spec        r     Noisy spectra from current spatial window
c                         See comments on hatspec for locations.
c       
c-----------------------------------------------------------------------
      implicit none
c
      integer nnx, nny, n(2), nchan, nsp, ichan, xoff, yoff
      real hatspec(nchan,nsp), spec(nchan,nsp), noise(nnx,nny)
cc
      integer j, k, isp
c-----------------------------------------------------------------------
c
c Step through the spectra, selecting all the different SPATIAL
c locations for the current channel.  Add the correlated noise in
c during this stepping.
c
      isp = 1
      do k = 1, n(2)
        do j = 1, n(1)
          spec(ichan,isp) = hatspec(ichan,isp) + noise(j+xoff,k+yoff)
          isp = isp + 1
        end do
      end do
c
      end
c
c
      subroutine refit (aveop, mode, aa, sigaa, nsim, isim, nwin, iwin,
     +                  idim, pt, ispect, vspect, nchan, nsp, log1,
     +                  llog, outfile, ilen, delta, nsum, suma, sumsqa)
c-----------------------------------------------------------------------
c     Now refit the simulated noisy spectra with ZED, accumulate sums
c     and write log files and fiddle factor file at the end
c
c     Input:
c       aveop     c     Averaging option
c       mode      c     Fitting mode
c       aa        r     Original splitting fit for actual data
c       sigaa     r     Original error in the fitted splittings
c       nsim      i     Total number of simulations
c       isim      i     Simulation number
c       nwin      i     Number of regions of interest
c       iwin      i     Region numbers
c       idim      i     Size of I,VSPECT
c       pt        i     Pointers for regions to I,VSPECT
c       i,vspect  r     I and V spectra
c       nchan     i     Number of channels to fit
c       nsp       i     Number of spectra ineach region
c       log1      c     Log file root name
c       llog      i     Handles for log files
c       outfile   c     Log file names
c       ilen      i     Lebgths of output file names
c       delta     i     1 for 2-sided derivative, 0 for 1-sided
c     Input/output
c       nsum      i     Number of accumulatred converged simulations
c       suma      r     Accumulated sum of fitted splitting
c       sumsqa    r     Accumulated sum of squares of fitted splitting
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nwin, idim, pt(nwin), nchan, nsp(nwin), llog(nwin), nsim,
     +nsum(nwin), delta, isim, ilen(nwin), iwin(nwin)
      character*(*) mode, outfile(nwin), log1, aveop*1
      real ispect(idim), vspect(idim), aa(nwin), sigaa(nwin), 
     +suma(nwin), sumsqa(nwin)
cc
      integer h, len1, iostat
      real a, b, siga, sigb, sigi, eta2, etahat2, mean, var, sdev, ff
      logical convrg
      character aconv*1, aline*100, str*3, itoaf*3
c-----------------------------------------------------------------------
      do h = 1, nwin
         call zed (mode, ispect(pt(h)), vspect(pt(h)), nchan, nsp(h),
     +             a, b, siga, sigb, sigi, convrg)
         call zedeta (ispect(pt(h)), nchan, nsp(h), sigi, delta, eta2,
     +                etahat2)
         call accum (a, convrg, nsum(h), suma(h), sumsqa(h), aconv)
c
c Write current results to log file.   Close and reopen when
c summing to force buffers to be flushed.
c
         if (nsum(h).gt.0) then
           call facs (nsum(h), suma(h), sumsqa(h), mean, var, sdev)
           ff = sdev / sigaa(h)
         else
           ff = 0.0
         end if
c
         if (log1.ne.' ') then
            write (aline, 200) isim, a, siga, sigi, sqrt(eta2), 
     +                         ff, aconv
200         format (i3, 2x, 4(1pe11.4, 1x), 2x, 0pf6.3, 5x, a1)
            call txtwrite (llog(h), aline, len1(aline), iostat)
            if (iostat.ne.0) call bug ('w', 'Error writing to log file')
c
            if (aveop.ne.'a') then
               call txtclose (llog(h))
               call txtopen (llog(h), outfile(h)(1:ilen(h)),
     +                        'append', iostat)
               if (iostat.ne.0) 
     +            call bug ('f', 'Error re-opening log file')
            end if
         end if
c
c Tell user fiddle factors when on last simulation 
c
         if (isim.eq.nsim) then
           call output (' ')
           str = itoaf(iwin(h))
           call output 
     +       ('*** Results from all converged runs for window # '//str)
           call output (' ')
c
           if (nsum(h).gt.0) then
             call tell (nsum(h), aa(h), sigaa(h), mean, var, sdev)
             call output (' ')
           else
             call output 
     +         ('*** There were no converged runs. What a waste')
           end if
         end if
      end do
c
      end
c
c
      subroutine accum (a, convrg, nsum, suma, sumsqa, aconv)
c---------------------------------------------------------------------
c     Accumulate sums and sums of squares from current simulation
c---------------------------------------------------------------------
      implicit none
c
      real suma, sumsqa, a
      integer nsum
      logical convrg
      character aconv*1
c---------------------------------------------------------------------
      if (convrg) then
        aconv = 'y'
        suma = suma + a
        sumsqa = sumsqa + a**2
        nsum = nsum + 1
      else
        call bug ('w', 'Fitting algorithm did not converge')
        aconv = 'n'
      end if
c
      end
c
c
      subroutine facs (n, sum, sumsq, mean, var, sdev)
c----------------------------------------------------------------
c     Work out mean and standard deviation of list of numbers 
c----------------------------------------------------------------
      implicit none
c
      integer n
      real sum, sumsq, mean, var, sdev
c----------------------------------------------------------------
      mean = sum / n
      var = (sumsq/n) - mean**2
      if (var.gt.0.0) then
        sdev = sqrt(var)
      else
        sdev = 0.0
      end if
c
      end
c
c
      subroutine tell (n, a, siga, mean, var, sdev)
c----------------------------------------------------------------
c     Output fiddle factor results  to terminal
c---------------------------------------------------------------
      implicit none
c
      real a, siga, mean, var, sdev
      integer n
      character aline*80
c---------------------------------------------------------------
      write (aline,'(a,i7)') 
     +   'Number of simulations          ', n
      call output (aline)
c
      write (aline,'(a,1pe12.4,2x,1pe12.4)') 
     +   'Original a and siga:          ', a, siga
      call output (aline)
c
      write (aline,'(a,2(1pe12.4,2x),1pe12.4)')
     +   'Simulated a: mean, var, sdev: ', mean, var, sdev
      call output (aline)
c
      write (aline,'(a,f8.3)') 
     +   'Fiddle factor siga_sim/siga:  ', sdev/siga
      call output (aline)
c
      end
c
c
      subroutine stats (n, x, mean, sdev, wrt)
c-----------------------------------------------------------------------
c     Find mean and variance for array
c-----------------------------------------------------------------------
      implicit none
c
      integer n
      real x(n), mean, sdev
      logical wrt
cc
      integer i
      real sum, sumsq, var
      character aline*80
c-----------------------------------------------------------------------
      sum = 0.0
      sumsq = 0.0
      do i = 1, n
        sum = sum + x(i)
        sumsq = sumsq + x(i)**2
      end do
      call facs (n, sum, sumsq, mean, var, sdev)
c
      if (wrt) then
         write (aline,'(a,i6,3x,1pe12.4,3x,1pe12.4)')
     +     ' n, mean, sigma = ',n,mean,sdev
         call output (aline)
      end if
c
      end

