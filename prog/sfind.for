      program sfind

c= SFIND - Automatically or interactively find sources in images
c& amh
c: plotting
c+
c       SFIND has been updated to incorporate a new statistically
c       robust method for detecting source pixels, called FDR (which
c       stands for "False Discovery Rate"), as an alternative to
c       simple sigma-clipping, which formed the basis of the original
c       implementation. Details of the FDR method can be found in
c       Hopkins et al 2001, (astro-ph/0110570) and references
c       therein. The original implementation of sfind has been
c       preserved, and can be applied by specifying the option
c       "oldsfind".  In addition, when using SFIND in the new 'FDR'
c       mode, several new features are available. SFIND now provides the
c       option of outputing
c         1: a 'normalised' image (options=normimg) created by
c            subtracting a background and dividing by sigma (the
c            standard deviation),
c         2: a 'sigma' image (options=sigmaimg), created by sigma-
c            clipping the normalised image at a user-specified sigma
c            value,
c         3: an 'fdr' image (options=fdrimg) created similarly to the
c            sigma-image by clipping the normalised image at the FDR-
c            defined threshold.
c
c       In 'FDR' mode (the default), no interactive source detection
c       is possible, as is the case with the original version.  Instead,
c       the detected sources are drawn from a distribution of pixels
c       with a robustly known chance of being falsely drawn from the
c       background, thus more reliably characterising the fraction of
c       expected false sources than is possible with a sigma-clipping
c       criterion.
c       The process of source detection and measurement is slightly
c       different in 'FDR' mode compared to the original SFIND
c       implementation (see below).  In 'FDR' mode, the following steps
c       are performed:
c         1: The image is first normalised by estimating the background
c            (mean) and standard deviation (sigma) for the whole image
c            in uniformly distributed regions of size 'rmsbox' (a user
c            input).  This is done by fitting a gaussian to the pixel
c            histogram, (as is done in the task 'imsad') - if the fit is
c            poor, an interative method is used instead.  With these
c            values known, the image has the mean subtracted and is
c            divided by sigma to create a normalised image.  This is the
c            same as saying the normalised image has a gaussian mean of
c            0 and sigma of 1.  The normalised image is output as a
c            Miriad image called sfind.norm if 'options=normimg' is set.
c            The rms noise measured over the image can also be output as
c            sfind.rms if 'options=rmsimg' is set.
c         2: From the normalised image a sigma-clipped image (called
c            sfind.sig) may be output (if 'options=sigmaimg').  This is
c            simply an image with pixel values set to 100 if the pixel
c            value in the normalised image is greater than the user
c            specified value of 'xrms,' or 0 otherwise.
c         3: The FDR method is implemented using the normalised image.
c            Each pixel is assigned a p-value, a probability that it was
c            drawn from the background, and a cutoff p-value is
c            established based on the percentage of false rejections
c            (source pixels) that the user specifies with the parameter
c            'alpha'.  If 'options=fdrimg' is set, this cutoff p-value
c            threshold is used to create an 'fdr' image (called
c            sfind.fdr) in the same way as the sigma image above is
c            created.
c         4: With the FDR cutoff threshold established, sources may now
c            be detected and measured. Each pixel with a p-value lying
c            *below* the cutoff p-value (i.e. a low chance of being
c            drawn from the background) may be part of a source.  For
c            each such 'FDR-detected' pixel, a hill-climbing routine
c            finds a local peak from adjacent FDR-selected pixels.  This
c            is then used as the starting point for a routine that
c            selects contiguous monotonically decreasing adjacent pixels
c            from the FDR-selected ones, and to which a 2-D elliptical
c            gaussian is fit in the same way as the original SFIND (see
c            below).  The same parameters are returned as in the
c            original implementation, and the logfile has the same
c            format (see below).
c       Also, in FDR mode 'option=auto' from the original implementation
c       is assumed automatically, regardless of user input. This means
c       the inputs for 'type,' 'range,' 'device' etc are not relevant
c       and are ignored.
c
c       In the original implementation, SFIND displays an image via a
c       contour plot or a pixel map representation on a PGPLOT device.
c       The user is then provided with the opportunity to interactively
c       flag sources as real or not (indicated by a Y or N flag in a
c       log file).
c
c       Source positions are calculated by an algorithm which searches
c       for pixels brighter than the surrounding 24 pixels and then bi-
c       parabolically fitting positions and flux densities.  Once a
c       source such as this is detected, SFIND checks to see whether it
c       is brighter than the user set multiple of the background rms.
c       If so, a 2D elliptical gaussian fit is performed (using the same
c       routine as IMFIT) and the source parameters are displayed on the
c       terminal (and written to a log file after user input to
c       determine a flag, Y or N, to attach). The source parameters are
c       (in order):
c
c                 Quantity                        Notes
c              --------------                  -----------
c       Position                       RA and Dec. in standard Miriad
c                                      hms,dms format
c       Formal errors in RA and Dec.   (arcsec; treat judiciously)
c       Peak flux density              (mJy)
c       Formal error in peak flux      in mJy (generally not a good
c       density                        estimate of the true error)
c       Integrated flux density        (mJy)
c       Major and minor axes and       (arcseconds for axes, degrees for
c                                      PA)
c       position angle of source       Warning: these are not
c                                      deconvolved from the synthesized
c                                      beam
c       Local background rms (sigma)   (mJy) calculated from a Gaussian
c                                      fit to the pixel histogram, as
c                                      per imsad
c       rms of gaussian fit
c
c
c       Manipulation of the device colour lookup table is available
c       when you display with a pixel map representation.
c
c@ in
c       The input image.
c@ type
c       Specifies the type of the image in the IN keyword. Minimum match
c       is supported.   Choose from:
c
c       "contour"   (contour plot)
c       "pixel"     (pixel map)
c
c       It is strongly suggested that pixel maps be used for source
c       finding, as contour plots may be deceiving.  Default is "pixel".
c       Ignored in 'FDR' mode (the default).
c
c@ region
c       Region of interest.  Choose only one spatial region (bounding
c       box only supported), but as many spectral regions (i.e.,
c       multiple IMAGE specifications) as you like.  If you display a
c       3-D image, the cursor options are activated after each sub-plot
c       (channel or group of channels; see CHAN below) is drawn.
c       Default is full image.
c@ xybin
c       Upto 4 values.  These give the spatial increment and binning
c       size in pixels for the x and y axes to be applied to the
c       selected region.   If the binning size is not unity, it must be
c       equal to the increment.  For example, to bin up the image by 4
c       pixels in the x direction and to pick out every third pixel in
c       the y direction, set XYBIN=4,4,3,1.
c       Defaults are 1,XYBIN(1),XYBIN(1),XYBIN(3)
c@ chan
c       2 values. The first is the channel increment, the second is
c       the number of channels to average, for each sub-plot.  Thus
c       CHAN=5,3  would average groups of 3 channels together, starting
c       5 channels apart such as: 1:3, 6:8, 11:13 ...   The channels
c       available are those designated by the REGION keyword.  A new
c       group of channels (sub-plot) is started if there is a
c       discontinuity in the REGION selected channels (such as
c       IMAGE(10,20),IMAGE(22,30).
c
c       Defaults are 1,1
c@ slev
c       2 values.   First value is the type of contour level scale
c       factor.  "p" for percentage and "a" for absolute.   Second
c       value is the level to scale LEVS by.  Thus  SLEV=p,1  would
c       contour levels at LEVS * 1% of the image peak intensity.
c       Similarly, SLEV=a,1.4e-2   would contour levels at LEVS * 1.4E-2
c       Default is no additional scaling of LEVS.
c       Ignored in 'FDR' mode (the default).
c@ levs
c       Levels to contour for first image, are LEVS times SLEV
c       (either percentage of the image peak or absolute).
c       Defaults try to choose something sensible
c       Ignored in 'FDR' mode (the default).
c@ range
c       3 values. The pixel map range (background to foreground), and
c       transfer function type.  The transfer function type can be one
c       of "lin" (linear), "log" (logarithmic), "heq" (histogram equal-
c       ization), and "sqr" (square root).  See also OPTIONS=FIDDLE
c       which is in addition to the selections here.
c
c       Default is linear between the image minimum and maximum
c       If you wish to just give a transfer function type, set
c       range=0,0,heq   say.
c       Ignored in 'FDR' mode (the default).
c@ cutoff
c       Flux density below which possible sources are ignored.
c       Default is zero.
c       Ignored in 'FDR' mode (the default).
c@ rmsbox
c       In 'FDR' mode (the default) this is the size of the 'smoothing'
c       box used when estimating the background and standard deviation
c       of the image. It is suggested that this be several to many times
c       the beam size to prevent sources from artificially skewing the
c       background estimates. This may require some experimentation,
c       and 'options=normimg' may be useful in determining the
c       effectiveness of a particular rmsbox size.
c
c       In the original implementation (options=oldsfind), it is the
c       size (in pixels) of a box centred on each source within which
c       the background rms is calculated.  Only pixels outside the "beam
c       exclusion radius" (1.5 x BMAJ) are used in this calculation.
c       Default is 20 pixels.
c@ alpha
c       This (real) number is the *percentage* of false *pixels* which
c       can be accepted when applying the FDR method.  Alpha determines
c       the threshold set in selecting pixels which belong to sources
c       (as an alternative to a simple sigma-cut), prior to the source-
c       fitting and measuring step.  Must be a positive number.
c       Default is 2.0 percent.  Ignored if options=oldsfind.
c@ xrms
c       In 'FDR' mode (the default) this parameter defines the sigma-
c       cutoff for the creation of the image sfind.sig if
c       'options=sigmaimg' is set.  If not, it is ignored.  It has no
c       role in the detection or measurement of sources.  No default.
c       In the original implementation (options=oldsfind), it is the
c       multiple of the background rms value above which a source must
c       be before the user is given the choice of verifying it.
c       No default.
c@ device
c       The PGPLOT plot device, such as plot.plt/ps. No default.
c       Ignored in 'FDR' mode (the default).
c@ nxy
c       Number of sub-plots in the x and y directions on the page.
c       Defaults choose something sensible
c       Ignored in 'FDR' mode (the default).
c@ labtyp
c       Two values.  The spatial label type of the x and y axes.
c       Minimum match is active.  Select from:
c
c       "hms"       the label is in H M S (e.g. for RA)
c       "dms"       the label is in D M S (e.g. for DEC)
c       "arcsec"    the label is in arcsecond offsets
c       "arcmin"    the label is in arcminute offsets
c       "absdeg"    the label is in degrees
c       "reldeg"    the label is in degree offsets
c            The above assume the  pixel increment is in radians.
c       "abspix"    the label is in pixels
c       "relpix"    the label is in pixel offsets
c       "abskms"    the label is in Km/s
c       "relkms"    the label is in Km/s offsets
c       "absghz"    the label is in GHz
c       "relghz"    the label is in GHz offsets
c       "absnat"    the label is in linear coordinates as defined by
c                   the header.  You might call this the natural axis
c                   label.
c       "relnat"    the label is in offset natural coordinates
c
c       All offsets are from the reference pixel.
c       Defaults are "abspix", LABTYP(1) unless LABTYP(1)="hms"
c       whereupon LABTYP(2) defaults to "dms" (for RA and DEC).
c       Ignored in 'FDR' mode (the default).
c@ logfile
c       Log file name, default 'sfind.log'.
c@ options
c       Task enrichment options.  Minimum match is active.
c
c       "fiddle" means enter a routine to allow you to interactively
c         change the display lookup table.  You can cycle through b&w
c         and colour displays, as well as alter the transfer function by
c         the cursor location, or by selecting predefined transfer
c         functions such as histogram equalization, logarithmic, &
c         square root.
c       "wedge" means that if you are drawing a pixel map, also draw
c         and label a wedge to the right of the plot, showing the map
c         of intensity to colour
c
c       "3value"  means label each sub-plot with the appropriate value
c         of the third axis (e.g. velocity or frequency for an xyv
c         ordered cube, position for a vxy ordered cube).
c       "3pixel"  means label each sub-plot with the pixel value of the
c         the third axis.   Both "3pixel" and "3value" can appear, and
c         both will be written on the plot.  They are the average values
c         when the third axis is binned up with CHAN.  If the third axis
c         is not velocity or frequency, the units type for "3VALUE" will
c         be chosen to be the complement of any like axis in the first
c         two.  E.g., the cube is in vxy order and LABTYP=ABSKMS,ARCSEC
c         the units for the "3VALUE" label will be arcsec.  If
c         LABTYP=ABSKMS,HMS the "3VALUE" label will be DMS (if the third
c         [y] axis is declination).
c
c       "grid" means draw a coordinate grid on the plot rather than just
c         ticks.
c
c       "noerase"  Don't erase a snugly fitting rectangle into which the
c        "3-axis" value string is written.
c
c       "unequal" means draw plots with unequal scales in x and y. The
c        default is that the scales are equal.
c
c       "mark" When source has been found, and user has agreed that it
c         is real, mark it with a cross.
c
c       "nofit" Prevents the program from fitting elliptical gaussians
c         to each source.  The data given on each source will be that
c         from a bi-parabolic fit, as per the earlier version of sfind.
c         Note that flux densities from this fit are bi-parabolically
c         fitted *peak* flux densities, and the positions are to the
c         peak flux density position (which will always be within 1
c         pixel of the brightest pixel in the source).  This option is
c         useful for providing a starting point for groups of sources
c         which the gaussian fitting procedure hasn't taken a liking to.
c
c       "asciiart" During the interactive section of the program, an
c         ascii picture of each source is displayed, showing which
c         pixels have been used in the gaussian fitting procedure.  The
c         brightest pixel in the source is symbolised by a "O", the rest
c         by asterisks.  This option is ignored if "nofit" is being
c         used.
c
c       "auto" The interactive section of the program is bypassed, and
c         all detected sources are flagged as real. The image is not
c         displayed.
c         This is set automatically in 'FDR' mode (the default) and it
c         is only necessary to select it manually if using
c         'options=oldsfind' (see below).
c
c       "negative" The map is inverted before source detection and
c         fitting, i.e., positive pixels become negative and vice versa.
c         This is to enable detection of negative sources without
c         recourse to MATHS.  This feature may be used for detecting
c         sources in polarisation maps.
c
c       "pbcorr" Corrects the flux density value calculated for each
c         source for the effect of the primary beam attenuation.  This
c         is dealt with correctly for mosaics as well as single
c         pointings.
c
c       "oldsfind" Use this to run SFIND as the original implementation
c         for the interactive interface, or just consistency with
c         earlier measurements.
c
c       "fdrimage" An output image called sfind.fdr will be created
c         with pixel values of 100, if their p-values are below the FDR
c         threshold, or 0 otherwise.
c         Ignored if 'oldsfind' is present.
c
c       "sigmaimg" An output image called sfind.sig will be created
c         with pixel values of 100, if their sigma-values are above
c         the user specified threshold from 'xrms,' or 0 otherwise.
c         Ignored if 'oldsfind' is present.
c
c       "rmsimg" An output image called sfind.rms will be created
c         where the pixel values correspond to the rms noise level
c         calculated when normalising the image.
c         Ignored if 'oldsfind' is present.
c
c       "normimg" An output image called sfind.norm will be created
c         by subtracting a background mean from the input image and
c         dividing by the standard deviation. The mean and sigma are
c         calculated in regions of size 'rmsbox' tiled over the image.
c         Ignored if 'oldsfind' is present.
c
c       "kvannot" As well as the regular log file (always written)
c         create a kview format annotation file, called 'sfind.ann'
c         containing one ellipse per object, with the appropriate
c         location, size, and position angle.
c
c       "fdrpeak" The default for source measurement is to use only
c         pixels above the FDR threshold in measuring the properties of
c         sources. (This is analogous, in SExtractor, for example,
c         to having the detect and analyse thresholds at the same
c         level.) In some cases it may be desirable to allow fitting of
c         sources where the peak pixel is above the FDR threshold, but
c         other source pixels are not required to be. This is the case
c         for obtaining reasonable measurements of sources close to the
c         threshold. Selecting 'fdrpeak' allows this. If 'fdrpeak'
c         is selected, source pixels are still required to be contiguous
c         and monotonically decreasing from the peak pixel, but not
c         necessarily to be above the FDR threshold.
c
c       "allpix" Rather than selecting pixels for the gaussian fitting
c         by requiring they be monotonically decreasing away from the
c         peak pixel, this option allows all FDR-selected pixels
c         contiguous with the peak pixel to be fit for a source.
c         If this option is selected, the fdrpeak option is ignored.
c
c       "psfsize" Restricts the minimum fitted size of a detected source
c         to the size of the sythesised beam, i.e., the PSF.  Any source
c         fitted to have a smaller size than this has its FWHM and PA
c         set to those of the synthesised beam, and is refit for the
c         position and amplitude only.
c
c       Some common combinations of options I have used (for examples):
c       options=kva,fdri,norm,sig
c       options=old,mark,ascii
c       options=old,auto
c@ csize
c       Two values.  Character sizes in units of the PGPLOT default
c       (which is ~ 1/40 of the view surface height) for the plot axis
c       labels and the velocity/channel labels.
c       Defaults choose something sensible.
c       Ignored in 'FDR' mode (the default).
c
c  Known Bugs:
c       In FDR mode the code has problems with very large images.  I
c       think this is dependent on the available memory of the machine
c       running Sfind, but need to do more testing to be certain.  I
c       have confirmed that on a machine with 256MB of memory and 256MB
c       swap space, an image of 3600x3600 pixels will be analysed
c       correctly.  For larger images, the code will halt
c       unceremoniously at the first call to memfree in subroutine fdr.
c       I don't understand why this happens, although I am guessing it
c       may have to do with the call to memfree trying to free more
c       memory than is available.
c
c       The output is designed to print source fluxes in FORTRAN format
c       f8.3 and f9.3 for peak and integrated flux densities
c       respectively.  This means that if your source's peak flux
c       is > 9999.999 mJy, (i.e. 10 Jy) or its integrated flux
c       is > 99999.999 mJy (i.e., 100 Jy), then it will not be
c       displayed properly. People detecting very bright sources - you
c       have been warned!
c
c       If 'options=fdrimg', 'sigmaimg', or 'normimg' are used with a
c       subregion of the image (specified by the 'region' keyword), the
c       output images are made the size of the *full* input image.  This
c       retains the original masking information, has zeroes outside the
c       regular bounding box of the selected region of interest, and the
c       relevant output within.  This does not affect the analysis
c       (which is all performed within the bounding box of the regular
c       region of interest), only the output images.
c
c       The following comments refer to the original SFIND
c       implementation.  The FDR implementation is much more robust to
c       finding faint sources close to bright sources, however since the
c       gaussian fitting process is the same, the comments about noise
c       or morphology are still relevant.
c
c       The gaussian fitting procedure can at times be temperamental.
c       If the source lies in a noisy region of the map, or close to
c       another bright source, or is simply of a morphology poorly
c       suited to being fit by gaussians, firstly the source may not be
c       detected at all, and if it is, the quoted errors on position and
c       flux density can be extremely high (often displayed in the
c       output as a row of asterisks due to the vagaries of FORTRAN).
c
c       In many of these cases, the given values of flux density and
c       position are still quite reasonable, (perhaps with errors an
c       order of magnitude larger than would otherwise be typical), but
c       user discretion is advised.  No responsibility is taken by the
c       programmer(s) for loss of life or property caused by taking the
c       results of this program under these conditions too seriously, or
c       by frustration generated by the use of this program under any
c       conditions.
c
c       Additionally, for unresolved sources, the "integrated" flux
c       density quoted may be less than the peak flux density.  (This
c       occurs if the fitted size of the source, proportional to
c       bmaj x bmin, is a smaller gaussian volume than that of the
c       beam.)  In this situation it is suggested that the peak flux
c       density be used.
c
c     Suggestions for believing in a source or not:
c       If a source is close to being indistinguishable by eye from the
c       background there are a few rules of thumb to help determine
c       whether the gaussian fit is telling the truth about a source, or
c       whether the source is even real.
c       1) If the pixels used in the fit are widely scattered (as
c          opposed to comprising a nice contiguous group) the fit will
c          probably not be very good and/or will not be a good
c          description of the source.
c       2) Check the fwhms and the position angle, and compare it to the
c          pixels used in the fit.  (Remember these values are in arcsec
c          for the FWHM and degrees for the PA, while the ascii picture
c          is in pixels).  If these obviously do not agree, then the fit
c          was poor and the source is probably not real.
c       3) Check the rms of the background. If this is high then firstly
c          the fit may not be good (as per 1), and secondly the source
c          is in a noisy area and should be treated with caution anyway.
c
c$Id: sfind.for,v 1.15 2012/02/08 06:52:43 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c To do:
c
c       Add option to allow list of source positions to be read from
c       file and imfit-type fitting performed at those positions.  Also
c       include in this option an option to search for the brightest
c       pixel near the given position to do the fit, or alternatively
c       try to force the fit to occur at the specified position.
c
c       Add option to allow user to "point and click" at sources and
c       have a fit done to that position (or the nearest bright pixel,
c       by some criterion, perhaps the brightest pixel in a 10x10 box
c       centred on the cursor).  This could be used as well as or
c       instead of the current method.  Think about two seperate options
c       to indicate user's desire of which.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      real wedisp, wedwid, tfdisp
      integer maxlev, maxpos, nxdef, nydef, nbins
      parameter (maxlev = 50, maxpos = 50, nxdef = 4, nydef = 4,
     *   wedisp = 1.0, tfdisp = 0.5, wedwid = 0.05, nbins = 128)

      integer ipim, ipnim

      real levs(maxlev), pixr(2), tr(6), cs(2), pixr2(2), scale(2),
     *  tfvp(4), wdgvp(4), cumhis(nbins), dmm(3)
      real slev, vxmin, vymin, vymax, vx, vy, vxsize, vysize, vxgap,
     *  vygap, ydispb, xdispl, groff, blank, cut, xrms, alpha

      integer blc(3), trc(3), size(maxnax), win(maxnax), nc,
     *  grpbeg(maxchan), ngrp(maxchan), srtlev(maxlev), his(nbins)
      integer nx, ny, nlevs, lin, naxis, k, ierr, pgbeg, iostat, ipage,
     *  ibin(2), jbin(2), kbin(2), krng(2), nlast, ngrps,
     *  llog, jj, wedcod, labcol, poscol, statcol, regcol, rmsbox,
     *  npnt, nelc

      character in*128, labtyp(2)*6, levtyp*1, logfil*128, pdev*64,
     *  trfun*3, xlabel*40, ylabel*40

      logical do3val, do3pix, eqscale, doblnk, dotr, donxlab(2),
     *  donylab(2), dopixel, gaps, doabut, doaxlab, doaylab,
     *  mark, doerase, dowedge, dofid, grid, nofit, asciiart,
     *  auto, negative, pbcor, oldsfind, fdrimg, sigmaimg, normimg,
     *  kvannot, fdrpeak, allpix, psfsize, rmsimg

      data ipage, scale /0, 0.0, 0.0/
      data dmm /1e30, -1e30, -1.0/
      data gaps, doabut, dotr /.false., .false., .false./

      character versan*80, version*80
c-----------------------------------------------------------------------
      version = versan('sfind',
     *                 '$Revision: 1.15 $',
     *                 '$Date: 2012/02/08 06:52:43 $')
c
c Get user inputs
c
      call inputs(maxlev, in, ibin, jbin, kbin, levtyp, slev, levs,
     *   nlevs, pixr, trfun, pdev, labtyp, logfil, do3val, do3pix,
     *   eqscale, nx, ny, cs, dopixel, mark, doerase, dowedge, dofid,
     *   grid, cut, rmsbox, alpha, xrms, nofit, asciiart, auto,
     *   negative, pbcor, oldsfind, sigmaimg, rmsimg, fdrimg, normimg,
     *   kvannot, fdrpeak, allpix, psfsize)
c
c Open log files
c
      call txtopen(llog, logfil, 'append', iostat)
      nc = nelc(logfil)
      if (iostat.ne.0)
     *  call bug('f', 'Error opening log file "' // logfil(:nc) // '"')
      call output(' ')
      call output('*** Source list output to ' // logfil(:nc))
      call output(' ')
      call output('Now opening image...')
      call output(' ')
c
c Open image
c
      call opimcg(maxnax, in, lin, size, naxis)
      call coInit(lin)
      if (pbcor) call mosLoad(lin,npnt)
c
c Finish key inputs for region of interest now
c
      call region(in, naxis, size, ibin, jbin, kbin, blc, trc,
     *            win, ngrps, grpbeg, ngrp)
c
c Try to allocate memory for images.
c
      call memalloc(ipim,  win(1)*win(2), 'r')
      call memalloc(ipnim, win(1)*win(2), 'i')
c
c Work out coordinate transformation matrix
c
      call limitscg(blc, ibin, jbin, tr)
c
c If the source detection procedure is not to be automated, (ie,
c 'options=oldsfind' but not 'auto') then perform all
c the image opening, initialization, etc. Otherwise skip all this.
c
      if ((oldsfind) .and. (.not.auto)) then
c
c Compute contour levels or check pixel map for log offset
c
       if (dopixel) then
        call grfixcg(pixr, lin, naxis, size, trfun, pixr2,
     *               groff, blank)
       else
        call conlevcg(.false., maxlev, lin, levtyp, slev, nlevs,
     *                levs, srtlev)
        blank = -99999999.0
       endif
c
c Work out number of plots per page and number of plots
c
       call nxnycg(nxdef, nydef, ngrps, nx, ny, nlast)
c
c Work out if wedge outside or inside subplots. Also work out
c if plotting one wedge per subplot or one wedge for all
c
       call wedgincg('NO', dofid, dowedge, nx, ny, 1, trfun, wedcod)
c
c Work out default character sizes for axis and channel labels
c
       call defchrcg(nx, ny, cs)
c
c Open plot device
c
       ierr = pgbeg (0, pdev, 1, 1)
       if (ierr.ne.1) then
         call pgldev
         call bug('f', 'Error opening plot device')
       endif
c
       call pgpage
       call pgscf(2)
c
c Set line graphics colour indices
c
       call setlgc(labcol, poscol, statcol, regcol)
c
c Init OFM routines
c
       if (dopixel) call ofmini
c
c Set axis labels
c
       call setlabcg(lin, labtyp, .false., xlabel, ylabel)
c
c Set label displacements from axes
c
       call setdspcg(lin, labtyp, blc, trc, xdispl, ydispb)
c
c Work out view port encompassing all sub-plots. Also return
c the viewport size of sub-plots.
c
       call vpsizcg(.false., dofid, 0, ' ', ' ', 0, ' ', maxlev,
     *   nlevs, srtlev, levs, slev, nx, ny, cs, xdispl, ydispb,
     *   gaps, doabut, dotr, wedcod, wedwid, tfdisp, labtyp, vxmin,
     *   vymin, vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales
c requested or if scales provided by user
c
       call vpadjcg(lin, 'NO', eqscale, scale, vxmin, vymin, vymax,
     *   nx, ny, blc, trc, tfvp, wdgvp, vxsize, vysize)
c
c Set viewport location of first sub-plot
c
       vx = vxmin
       vy = vymax - vysize
c
c Loop over number of subplots
c
       do k = 1, ngrps
         if (mod(k,nx*ny).eq.1 .or. nx*ny.eq.1) ipage = ipage + 1
         jj = k - (ipage-1)*nx*ny
         krng(1) = grpbeg(k)
         krng(2) = ngrp(k)
c
c Set viewport and window for current sub-plot
c
         call pgsvp(vx, vx+vxsize, vy, vy+vysize)
         call pgswin(blc(1)-0.5, trc(1)+0.5, blc(2)-0.5, trc(2)+0.5)
c
c Read in image
c
         call readimcg(.true., blank, lin, ibin, jbin, krng, blc,
     *     trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
c
c Apply transfer function
c
         call pgsci(labcol)
         if (dopixel) then
           if (trfun.ne.'lin') call apptrfcg(pixr, trfun, groff,
     *        win(1)*win(2), memi(ipnim), memr(ipim), nbins,
     *        his, cumhis)
c
c Draw wedge
c
           if (wedcod.eq.1 .or. wedcod.eq.2) then
            call pgsch(cs(1))
            call wedgecg(wedcod, wedwid, jj, trfun, groff, nbins,
     *                   cumhis, wdgvp, pixr(1), pixr(2))
           endif
         endif
c
c Draw pixel map; set default b&w colour table first.
c
         call pgsci(labcol)
         if (dopixel) then
           if (k.eq.1) call ofmcol(1, pixr2(1), pixr2(2))
           call pgimag(memr(ipim), win(1), win(2), 1, win(1), 1,
     *                 win(2), pixr2(1), pixr2(2), tr)
         else
c
c Draw contours
c
           call conturcg(.false., blank, .false., win(1), win(2),
     *       doblnk, memr(ipim), nlevs, levs, tr, 0.0, 0, 0)
         endif
c
c Determine if the axes need ascii or numeric labelling
c for this subplot
c
         call pgsch(cs(1))
         call dolabcg(gaps, dotr, nx, ny, ngrps, nlast, k,
     *                labtyp, doaxlab, doaylab, donxlab, donylab)
c
c Write ascii axis labels
c
         call aaxlabcg(doaxlab, doaylab, xdispl, ydispb,
     *                 xlabel, ylabel)
c
c Draw frame, write numeric labels, ticks and optional grid
c
         call naxlabcg(lin, .true., blc, trc, krng, labtyp,
     *                 donxlab, donylab, .false., grid)
c
c Draw wedge inside subplots and overwrite label ticks
c
         if (wedcod.eq.3) then
           call pgsch(cs(1))
           call wedgecg(wedcod, wedwid, jj, trfun, groff, nbins,
     *                  cumhis, wdgvp, pixr(1), pixr(2))
         endif
c
c Modify lookup table
c
         if (dofid) call ofmmod(tfvp, win(1)*win(2), memr(ipim),
     *                          memi(ipnim), pixr2(1), pixr2(2))
c
c Write velocity or channel label
c
         if (do3val .or. do3pix) then
           call pgsch(cs(2))
           call pgsci(1)
           call lab3cg(lin, doerase, do3val, do3pix, labtyp,
     *                 grpbeg(k), ngrp(k))
         endif
c
c Interactive graphical source finding routine
c
         call search_old (lin, win(1), win(2), memr(ipim), memi(ipnim),
     *     blc, ibin, jbin, krng, llog, mark, cut, rmsbox, xrms,
     *     nofit, asciiart, auto, negative, pbcor, in, psfsize)
c
c Increment sub-plot viewport locations and row counter
c
         call subinccg(k, nx, ny, vxmin, vymax, vxsize, vysize,
     *                 vxgap, vygap, vx, vy)
c
c Page plot device
c
         if (jj.eq.nx*ny .and. k.lt.ngrps) call pgpage
       enddo
c
c Close up
c
       call memfree(ipim,  win(1)*win(2), 'r')
       call memfree(ipnim, win(1)*win(2), 'i')
c
       call coFin(lin)
       call xyclose(lin)
       call txtclose(llog)
       call pgend
c
c Now the image display section of code is passed, perform search
c automatically (non-interactively) if required.
c
      else
c
c Loop over groups of channels selected in "region"
c
       do k = 1, ngrps
        krng(1) = grpbeg(k)
        krng(2) = ngrp(k)
c
c Read in image
c
        call readimcg(.true., blank, lin, ibin, jbin, krng, blc,
     *     trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
c
c The source-detection subroutine.
c
        if (oldsfind) then
         call search_old (lin, win(1), win(2), memr(ipim), memi(ipnim),
     *     blc, ibin, jbin, krng, llog, mark, cut, rmsbox, xrms,
     *     nofit, asciiart, auto, negative, pbcor, in, psfsize)
        else
         call search(lin, win(1), win(2), memr(ipim), memi(ipnim),
     *     blc, ibin, jbin, krng, llog, rmsbox, alpha, xrms, auto,
     *     negative, pbcor, in, fdrimg, sigmaimg, rmsimg,
     *     normimg, kvannot, fdrpeak, allpix, psfsize, size)
        endif
       enddo
c
c Close up
c
       call memfree(ipim,  win(1)*win(2), 'r')
       call memfree(ipnim, win(1)*win(2), 'i')
c
       call xyclose(lin)
       call txtclose(llog)
      endif
c
      end
c
c
      subroutine cgcur(x, y, ans)
      real x, y
      character ans*1
      call pgupdt
      call pgcurs(x, y, ans)
      call pgupdt
      call ucase(ans)
c
      end
c
c
      subroutine search_old (lin, nx, ny, image, nimage, blc, ibin,
     *   jbin, krng, llog, mark, cut, rmsbox, xrms, nofit, asciiart,
     *   auto, negative, pbcor, in, psfsize)
c-----------------------------------------------------------------------
c  This is the master subroutine for the detecting of sources and the
c  interactive decision bit.  It detects bright pixels, determines
c  whether they are brighter than the surrounding 24 pixels (of which
c  none are permitted to be blanked) and if so, performs a bi-parabolic
c  fit to the peak.  This is then used to determine if the source is
c  above a user set multiple of the background rms around the source,
c  and if so an elliptical gaussian fit is carried out.  The results are
c  then written to the screen and the cursor moved to the position of
c  the object where it waits for the users yay or nay.  After user
c  input, the source parameters are written, along with a flag (Y or N),
c  to the log file.
c
c  Input:
c     lin       Image handle
c     nx,ny     Size of image
c     image     Image
c     nimage    blanking image (0 for blanked pixels)
c     blc       blc of window being displayed
c     i,jbin    Spatial pixel increment
c     krng      Start plane and number of planes averaged together
c               to make the current displayed plane
c     llog      Handle of log file
c     mark      True to mark cursor locations
c     cut       flux limit below which sources are ignored
c     rmsbox    size of box (in pixels) within which background rms is
c               first calculated
c     xrms      multiple of background rms above which source must be
c               before user is given option of saying yay or nay
c     nofit     True means don't do gaussian fitting for each source
c     asciiart  display ascii representations of each source during
c               interactive source selection
c     auto      skip all the interactive bits of source flagging.
c     negative  inverts image: positive pixels become negative and vice
c               versa
c     pbcor     true to correct fluxes by primary beam attenuation
c     in        image name
c
c-----------------------------------------------------------------------
c
      integer nx, ny, blc(2), llog, ibin, jbin, lin, nimage(nx,ny),
     *  krng(2)
      real image(nx,ny)
      logical mark, nofit, asciiart, auto, negative, pbcor, psfsize
      character in*(*)
cc
      double precision wa(2), posns(2)
      real ww(2), wsave(2), cut, xrms, peak, base0
      real xpos, ypos, pval, sigma, rms, mult
      real pkfl, intfl, amaj, amin, posa
      real xposerr, yposerr, pkflerr
      real bvol, bmaj, bmin, bpa, bvolp, bmajp, bminp, bpap
      real gain, mvlrms
      integer iostat, len1, iloc, bin(2), l, m, radeclen(2), rmsbox
      integer sources, ysources, k, i, j
      character cch*1, line*160, typei(2)*6, radec(2)*80, typeo(2)*6
      character line1*80, line2*160, line3*160, line4*160
      logical ok, nobeam, fitok
c-----------------------------------------------------------------------
      call output(' ')
      if (.not.auto) then
       call output('************************************')
       call output('Beginning Interactive Source Finding')
       call output('************************************')
       call output(' ')
       call output('Click left button   (enter A) to flag source as Y')
       call output
     *  ('Click middle button (enter D) to flag source as N')
       call output('Click right button  (enter X) to quit')
       call output(' ')
      else
       call output('****************************************')
       call output('Beginning Non-Interactive Source Finding')
       call output('****************************************')
       call output(' ')
       call output('Please be patient...')
      endif
c
c Initialize
c
      bin(1) = ibin
      bin(2) = jbin
      cch = ' '
      iloc = 0
      sources = 0
      ysources = 0
c
c Invert image if options=negative selected
c
      if (negative) then
       do l = 1, nx
        do m = 1, ny
         image(l,m) = -image(l,m)
        enddo
       enddo
      endif
c
c Write header for output to screen, unless "auto" option selected.
c
      write(line1,19) '# Sources from image ',in
19    format(a21,a59)
      if (.not.auto) call output(line1)
      if (nofit) then
        write(line2,20) '#','RA','DEC','flux','x','y','rms',
     *                  'flux/rms'
20      format(a,4x,a,11x,a,4x,a,7x,a,8x,a,7x,a,2x,a)
        if (.not.auto) call output(line2)
        line3 = '#                        mJy      pixels   '//
     *          'pixels    mJy'
        if (.not.auto) call output(line3)
        line4 = '#-----------------------------------------'//
     *          '-------------------------------------'
        if (.not.auto) call output(line4)
       else
        write(line2,30) '#','RA','DEC','err(RA)','err(DEC)','pk-flux',
     *                  'err','flux','bmaj','bmin','pa',
     *                  'rms(bg)','rms(fit)'
30      format(a,4x,a,9x,a,5x,a,1x,a,1x,a,3x,a,6x,
     *         a,3x,a,3x,a,3x,a,1x,a,1x,a)
        if (.not.auto) call output(line2)
        line3 = '#                       arcsec   arcsec   '//
     *          'mJy      mJy       mJy  arcsec arcsec '//
     *          'deg  mJy     mJy'
        if (.not.auto) call output(line3)
        line4 = '#-----------------------------------------'//
     *          '-------------------------------------'//
     *          '----------------'
        if (.not.auto) call output(line4)
      endif
c
c Write output header for log file
c
      call txtwrite(llog, line1, len1(line1), iostat)
      call txtwrite(llog, line2, len1(line2), iostat)
      call txtwrite(llog, line3, len1(line3), iostat)
      call txtwrite(llog, line4, len1(line4), iostat)
c
c Set the plane appropriate to the displayed image.  Since
c this may be a range of planes averaged together, take the
c integer nearest the average plane
c
      k = nint(real(krng(1) + krng(1) + krng(2) - 1)/2.0)
c
c Determine beam parameters, including determining if the beam doesn't
c exist.
c
      call BeamPar(lIn, k, bvol, bvolp, bmaj, bmin, bpa,
     *             bmajp, bminp, bpap, nobeam)

c Check that rmsbox is big enough.
      write(line, '(a,f6.2,a)') 'Beam exclusion radius:', 1.5*bmajp,
     *  ' pixels.'
      call output(' ')
      call output(line)

      if (rmsbox/2.lt.(1.5*bmajp/sqrt(2.0))) then
        call bug('f', 'rmsbox is contained within the beam ' //
     *           'exclusion radius.')
      else if (rmsbox/2.lt.(2.0*bmajp)) then
        call bug('w', 'rmsbox is barely big enough.')
      endif

c Loop over all pixels, searching for bright pixels and moving the
c cursor to that position.
c
      do l = 3, nx-3
       do m = 3, ny-3
        pval = image(l,m)
c
c Ignore pixels less than cut and check to see if pixel brighter than
c surrounding 24 pixels, making sure none are blanked pixels.
c
        if (pval.lt.cut) goto 60
        do i = l-2, l+2
          do j = m-2, m+2
            if (((i.ne.l .or. j.ne.m) .and. nimage(i,j).gt.0 .and.
     *          pval.le.image(i,j)) .or. nimage(i,j).eq.0) goto 60
          enddo
        enddo
c
c Bi-parabolic fit, base-level and background rms calculation and
c subtraction of base-level from peak.
c
        call peakfit(l, m, xpos, ypos, nx, ny, peak, image)
        call basecal_old(nx, ny, l, m, base0, sigma, rmsbox, image,
     *               nimage, 1.5*bmajp, ok)
        if (.not.ok) goto 60
c
c Ignore source if less than xrms times background rms
c
        peak = peak - base0
        if (peak.lt.xrms*sigma) goto 60
        mult = peak/sigma
c
c Convert binned, subimage pixels to unbinned full image pixels
c
        wa(1) = dble(xpos)
        wa(2) = dble(ypos)
        call ppconcg(2, blc(1), bin(1), wa(1))
        call ppconcg(2, blc(2), bin(2), wa(2))
c
c Optionally calculate integrated flux density for the source,
c and Gaussian major & minor axes and position angle.
c
        if (.not.nofit) then
          fitok = .true.
          call fitting_old(lIn, krng, sigma, nx, ny, image, rms, xpos,
     *      xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj, amin,
     *      posa, posns, blc, bin, l, m, asciiart, bvol, bvolp, bmajp,
     *      bminp, bpap, nobeam, nimage, fitok, auto, psfsize, base0)
          if (.not.fitok) goto 60
c
          typei(1) = 'hms'
          typei(2) = 'dms'
        else
          posns(1) = wa(1)
          posns(2) = wa(2)
          typei(1) = 'abspix'
          typei(2) = 'abspix'
        endif
        typeo(1) = 'hms'
        typeo(2) = 'dms'
c
c Convert location (peak or fitted) to formatted coordinate string
c
        call w2wfco(lin, 2, typei, ' ', posns,  typeo, ' ',
     *              .true., radec, radeclen)
c
c if 'pbcor' selected, correct the flux densities (peak and integrated)
c by the gain (primary beam attenuation) at the position of the source
c
        if (pbcor) then
         call mosVal(lin,'aw/aw',posns,gain,mvlrms)
         pkfl = pkfl/gain
         intfl = intfl/gain
c         sigma = sigma/gain
        endif
c
c Write output line to screen, but only if "auto" option not selected
c
        if (nofit) then
          if (negative) then
           write(line,40) -peak*1000.0, xpos, ypos,
     *                   -sigma*1000.0, mult
          else
           write(line,40) peak*1000.0, xpos, ypos,
     *                   sigma*1000.0, mult
          endif
40        format(f8.3,2x,f7.2,2x,f7.2,2x,f7.3,2x,f5.1)
        else
          if (negative) then
           write(line,50) xposerr,yposerr,-pkfl,pkflerr,-intfl,
     *        amaj,amin,posa,-sigma*1000.0,-rms*1000.0
          else
           write(line,50) xposerr,yposerr,pkfl,pkflerr,intfl,
     *        amaj,amin,posa,sigma*1000.0,rms*1000.0
          endif
50        format(1x,f6.3,3x,f5.2,2x,f8.3,1x,f6.3,1x,f9.3,1x,
     *           3(f5.1,1x),f6.3,2x,f6.3)
        endif
        line = radec(1)(1:radeclen(1))//' '//
     *         radec(2)(1:radeclen(2))//' '//line
        if (.not.auto) call output(line)
c
c increment number of sources detected
c
        sources = sources + 1
c
c Interactive bit: move cursor to position and wait for button,
c read cursor to get yay or nay or exit from user. This bit is skipped
c entirely if "auto" is selected.
c
        if (.not.auto) then
         wsave(1) = wa(1)
         wsave(2) = wa(2)
         cch = ' '
         do while (cch.ne.'A' .and. cch.ne.'D')
          ww(1) = wsave(1)
          ww(2) = wsave(2)
          call cgcur(ww(1), ww(2), cch)
c
c Action depending upon user's button press
c
55        if (cch.eq.'A') then
            iloc = iloc + 1
            ysources = ysources + 1
c
c Mark on plot if desired
c
            if (mark) then
              call pgslw(3)
              call pgsci(2)
              call pgpt(1, wsave(1), wsave(2), 2)
              call pgupdt
              call pgslw(1)
            endif
c
            line(len1(line)+1:len1(line)+6) = '     Y'
            call txtwrite(llog, line, len1(line), iostat)
          else if (cch.eq.'D') then
            if (mark) then
              call pgslw(3)
              call pgsci(5)
              call pgpt(1, wsave(1), wsave(2), 4)
              call pgupdt
              call pgslw(1)
            endif
c
            line(len1(line)+1:len1(line)+6) = '     N'
            call txtwrite(llog, line, len1(line), iostat)
c
c When user presses quit-type command, check to see that's really what
c they wanted to do, and if so do it. If not, accept the different
c button as the intended command.
c
          else if (cch.eq.'X') then
            call output('Are you sure you want to quit here?')
            call output('(press again to confirm, or other key '//
     *                  'for corresponding action)')
            call cgcur(ww(1), ww(2), cch)
            if (cch.eq.'X') then
             goto 70
            else
             goto 55
            endif
          else
            call output('  Commands are: A (yes), D (no), X (exit).')
          endif
         enddo
        else
c
c The "auto" procedure treats every source as if the user flagged it
c good (pressed button "A").
c
         iloc = iloc + 1
         ysources = ysources + 1
         line(len1(line)+1:len1(line)+6) = '     Y'
         call txtwrite(llog, line, len1(line), iostat)
        endif
60      continue
       enddo
      enddo
c
70    call output(' ')
      write(line,80) sources
80    format('Total number of sources detected:',i5)
      call output(line)
      write(line,90) ysources
90    format('Number of sources confirmed:',i5)
      call output(line)
      call output(' ')
c
c
      end
c
c
      subroutine peakfit(l, m, x, y, nx, ny, peak, image)
c-----------------------------------------------------------------------
c    Perform a bi-parabolic fit to the position l,m and return fitted
c    coordinates x,y and flux density, peak. Parabolas are fitted along
c    both the x and y axes, and the resulting position is the x,y coords
c    of the parabola's peak (always within one pixel of the brightest
c    pixel).  The resulting peak flux density is simply the flux density
c    the brightest pixel would have had if it had been centered at the
c    position x,y.
c
c     If            z = a x^2 + b x + c
c     Then      z_max = b*b/a
c     And    x(z_max) = -b/2a
c
c  Input:
c    l,m   Bright pixel position
c    image Image matrix with pixel values
c    nx,ny dimensions of image
c  Output:
c    x,y   fitted pixel position
c    peak  fitted flux density
c-----------------------------------------------------------------------
      integer l,m,nx,ny
      real x,y
      real z,a,b,t,peak,image(nx,ny)
c-----------------------------------------------------------------------
      x = l
      y = m
      z = 0.0
      t = image(l,m)
c
      b = image(l+1,m) - image(l-1,m)
      a = image(l+1,m) - 2*t + image(l-1,m)
      if ((abs(b).gt.abs(a)) .or. (a.ge.0.0)) goto 2010
      x = x - 0.5*b/a
      z = b*b/a
2010  continue
c
      b = image(l,m+1) - image(l,m-1)
      a = image(l,m+1) - 2*t + image(l,m-1)
      if ((abs(b).gt.abs(a)) .or. (a.ge.0.0)) goto 2015
c
      y = y - 0.5*b/a
      z = z + b*b/a
2015  peak = t - 0.125*z
c
      end
c
c
      subroutine basecal_old (nx, ny, l, m, base0, sigma, rmsbox,
     *                   image, nimage, boxsize, ok)
c-----------------------------------------------------------------------
c    Iteratively calculate the base level around the found source using
c    all pixels falling within a square of side length rmsbox pixels
c    centred on the peak, but excluding those falling within a circle
c    with radius of boxsize.
c
c  Input:
c    nx,ny   Size of image array
c    l,m     Bright pixel position
c    image   Image matrix with pixel values
c    nimage  blanking image
c    rmsbox  Side length of box within which the background rms is
c            calculated
c  Output:
c    base0   The base-level
c    sigma   The background rms
c    ok      If true, was able to determine base and sigma
c-----------------------------------------------------------------------
      integer ii,jj,lmn,lmx,mmn,mmx,nx,ny,kk,l,m,rmsbox, half
      real base0, base1, base2, basen, sigma, rx, rr
      real image(nx,ny), boxsize
      integer nimage(nx,ny)
      logical ok
c-----------------------------------------------------------------------
      half= rmsbox/2
      lmn = max(1,l-half)
      lmx = min(nx,l+half)
      mmn = max(1,m-half)
      mmx = min(ny,m+half)
      ok = .true.
c
c First estimate of sigma from all pixels
c
      base0 = 0.0
      base1 = 0.0
      base2 = 0.0
      basen = 0
      do jj = mmn, mmx
        do ii = lmn, lmx
          if (nimage(ii,jj).gt.0) then
            basen = basen + 1.0
            base1 = base1 + image(ii,jj)
            base2 = base2 + image(ii,jj)**2
          endif
        enddo
      enddo
      if (basen.gt.0) then
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
        endif
      else
        ok = .false.
      endif
      if (.not.ok) return
c
c Now iterate 3 times from this starting point
c
      do kk = 1, 3
        base1 = 0.0
        base2 = 0.0
        basen = 0.0
        do ii = lmn, lmx
          rx = (ii - l)**2
          do jj = mmn, mmx
            rr = rx + (jj-m)**2
            if (nimage(ii,jj).gt.0 .and. rr.ge.(boxsize**2) .and.
     *          abs(image(ii,jj)-base0).le.3*sigma) then
              basen = basen + 1.0
              base1 = base1 + image(ii,jj)
              base2 = base2 + image(ii,jj)**2
            endif
          enddo
        enddo
c
c Mean and sigma
c
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
          return
        endif
      enddo
c
c If on last iteration, had less than 10 pixels,
c set base level to zero and set ropy sigma
c
      if (basen.lt.10.0) then
        base0 = 0.0
        sigma = sqrt(base2/basen)
      endif
c
      end
c
c
      subroutine fitting_old (lIn, krng, sigma, nx, ny, image, rms,
     *   xpos, xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj,
     *   amin, posa, posns, blc, bin, lx, my, asciiart, bvol, bvolp,
     *   bmajp, bminp, bpap, nobeam, nimage, fitok, auto, psfsize,
     *   base0)
c-----------------------------------------------------------------------
c  Do elliptical Gaussian fit to source. First the pixels to be used in
c  the fitting are selected on the basis that they are monotonically
c  decreasing away from the "central" bright pixel, then the gaussian
c  fit is done as per the task "imfit".  If the size of the region
c  within which pixels were selected for the fit is smaller than 4 times
c  the fwhm of the major axis of the source, the procedure is iterated
c  with a larger region. If this procedure diverges, (detected by the
c  increasing region becoming larger than the image size), the source is
c  ignored.  Once the source size and parameters are finally accepted,
c  the background rms is recalculated (more accurately now the source
c  size is known) again, and used for the output.
c
c  Once the fit is done the source parameters are recorded and passed
c  back to subroutine "search" for it to output as appropriate.
c
c  Input
c    nx,ny    Size of image
c    image    Image
c    nimage   Normalised Image (for masking info)
c    sigma    the background rms around the source. Used to clip pixels.
c             It is recalculated after the source fitting, and is an
c             output as well.
c    nobeam   logical used to determine default beam size if not
c             detected in image header.
c    bmajp    pixel size of major axis of beam. Used for setting size of
c             box within which to detect pixels for gaussian fitting.
c    blc, bin image params used for switching between full image and
c             binned subimage pixels.
c    lx, my   integer pixel coord of brightest pixel in object. Used in
c             pixel selection routine.
c    asciiart logical. True if user wants ascii pictures of the objects
c             displayed.
c    auto     true if not using interactive mode.
c    bvol,    beam volume in world and pixel coords, resp.
c    bvolp
c
c  Output
c    posns            The object position in world coords.
c    xpos, xposerr    x-position and error of object.
c    ypos, yposerr    y-position and error of object.
c    pkfl, pkflerr    peak flux density (and error) for object.
c    intfl            integrated flux density of object.
c    amaj, amin, posa axes (arcsec) and position angle (degrees) of
c                     object.
c    rms              rms of gaussian fit to selected pixels.
c    fitok            logical. False if the fit failed for any reason.
c
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer MAXVAR
      parameter (MAXVAR=20)
c
      logical dofit, asciiart, nobeam, fitok, ok, auto, psfsize
      integer ifail1,ifail2,lIn, i, blc(2), bin(2), maxline, boxsz4
      integer k,m,nvar,lx,my, nx,ny, krng(2), nimage(nx,ny), fiterr
      real image(nx,ny), dumm
      real clip,xvar(MAXVAR),covar(MAXVAR*MAXVAR),rms
      real bvol,bvolp, xpos, ypos, pkfl, intfl
      real sigma, amaj, amin, posa, bmajp, bminp, bpap, boxsize
      real xposerr, yposerr, pkflerr, base0
      double precision posns(2)
      character ascpic(1000)*80
c
c  Externals.
c
      external FUNCTION
c-----------------------------------------------------------------------
c
c Initialise parameters
c
      vflux  = .true.
      vl0    = .true.
      vm0    = .true.
      vfwhm1 = .true.
      vfwhm2 = .true.
      vpa    = .true.
      fwhm1  = 1.0
      fwhm2  = 1.0
      l0     = 0.0
      m0     = 0.0

      clip   = sigma
      fitok  = .true.
c
c Set the plane appropriate to the displayed image.  Since
c this may be a range of planes averaged together, take the
c integer nearest the average plane
c
      k = nint(real(krng(1) + krng(1) + krng(2) - 1)/2.0)
c
c  Set the size of the region within which to select pixels to be used
c  in the Gaussian fitting procedure.
c
      if (nobeam) then
        boxsize = 5.0
      else
        boxsize = bmajp
      endif
c
c  Load the data. Ie, choose the pixels in the region of the source to
c  be used in the fitting procedure, and encode them in a format ready
c  to do so.
c
1200  call LoadDat_old (clip,m,nx,ny,xpos,ypos,blc,bin,image,
     *  boxsize,lx,my,nimage,ascpic,maxline,fitok,base0)
      if (.not.fitok) return
      if (m.eq.0) then
       fitok = .false.
       return
      endif
c
c  Convert the coordinates to pixels, and fill in defaults if necessary.
c
      call GetEst
c
c  Pack the variables into an array.
c
      call PackVar(xvar,nvar,MAXVAR)
      dofit = nvar.gt.0
      if (.not.dofit) then
        fitok = .false.
        return
      endif
      if (nvar.ge.m) then
        fitok = .false.
        return
      endif
c
c  Do the fitting process.
c
      fiterr = 0
      call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
      call UPackVar(xvar,nvar)
      if (ifail2.eq.0) call UpackCov(covar,nvar)
      if (ifail1.ne.0) fiterr = 1
      if (ifail2.ne.ifail1) fiterr = 2
c
c If 'psfsize' selected, check object size and if smaller than
c psf, set to psf size and fit again for peak and position.
c
      if (psfsize .and. (fwhm1.lt.bmajp .or. fwhm2.lt.bminp)) then
        vflux  = .true.
        vl0    = .true.
        vm0    = .true.
        vfwhm1 = .false.
        vfwhm2 = .false.
        vpa    = .false.
        fwhm1  = bmajp
        fwhm2  = bminp
        pa     = bpap

        fitok = .true.
c       Pack the variables into an array.
        call PackVar(xvar,nvar,MAXVAR)
        dofit = nvar.gt.0
        if (.not.dofit) then
          fitok = .false.
          return
        endif
        if (nvar.ge.m) then
          fitok = .false.
          return
        endif

c       Do the fitting process.
        fiterr = 0
        call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
        call UPackVar(xvar,nvar)
        if (ifail2.eq.0) call UpackCov(covar,nvar)
        if (ifail1.ne.0) fiterr = 1
        if (ifail2.ne.ifail1) fiterr = 2
      endif
c
c  Check to see whether the fitted size of the image is comparable to
c  the initial box size used in selecting pixels, and if so iterate the
c  procedure choosing a larger box size.
c
      if (boxsize.lt.max(abs(fwhm1),abs(fwhm2))) then
c
c  Checks for divergence - eg, noise being fitted by ever larger
c  gaussians. Stops and aborts fit if object fwhm is larger than one
c  quarter the smallest image dimension, or if the fitted fwhm has
c  increased to more than 60 pixels.  (At sizes > 60 pixels, the next
c  iteration takes quite a while, and leaves a big wait for nothing if
c  the object is then discarded on the next iteration.)  This
c  (moderately) arbitrary number is, however, an unsatisfactory way of
c  dealing with the problem, and a better one would be appreciated.
c
       if (max(abs(fwhm1),abs(fwhm2)).lt.min(nx,ny,240)/4) then
        boxsize = max(abs(fwhm1),abs(fwhm2))
        goto 1200
       else
        fitok = .false.
        return
       endif
      endif
c
c use basecal to calculate updated value for the background rms, using
c newly found size of object to determine central region to exclude and
c overall region to include (boxsize and 4*boxsize, respectively)
c (Note that here, we are no longer interested in base0 and it has been
c replaced by a dummy var. "dumm".)
c
      boxsz4 = 4*boxsize
      call basecal_old(nx,ny,lx,my,dumm,sigma,boxsz4,image,nimage,
     *             boxsize,ok)
c
c If the new value was unable to be calculated - too few pixels, etc,
c put back its original value, which has been stored as the variable
c clip.
c
      if (.not.ok) sigma = clip
c
c print out ascii art if required
c
      if (asciiart) then
       do i = maxline,1,-1
        if ((ascpic(i).ne.' ') .or. (i.eq.1) .or. (i.eq.maxline))
     *      call output(ascpic(i))
       enddo
      endif
c
c Print out warnings if there were problems with the fits, unless in
c 'auto'.
c
      if ((.not.auto) .and. (fiterr.gt.0)) then
       if (fiterr.eq.1) then
        call bug('w','Fitting failed to converge.')
       else
        call bug('w','Failed to determine covariance matrix.')
       endif
       call bug('w','Source may still be real, but parameters may'
     *          //' be incorrect.')
      endif

c     Convert the fit parameters to astronomical units.
      call gaucvt(lIn, k, bvol, bvolp, xposerr, yposerr,
     *   pkfl, pkflerr, intfl, amaj, amin, posa, posns)
c
      end
c
c
      subroutine LoadDat_old(clip,m,nx,ny,xpos,ypos,blc,bin,image,
     *     boxsize,lx,my,nimage,ascpic,maxline,fitok,base0)
c-----------------------------------------------------------------------
c  Load the relevant data for this plane.  The relevant data are those
c  pixels to which to fit the elliptical gaussians, and they are
c  selected by ...
c
c  Input:
c    clip      Clip level.
c  Output:
c    m            Number of points loaded.
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer m,nx,ny,lmn,lmx,mmn,mmx,nimage(nx,ny)
      integer i,xt,yt,ll,mm, blc(2),bin(2), lx,my, maxline
      real clip,xpos,ypos,image(nx,ny),boxsize,base0
      double precision wa(2)
      logical incirc,fainter,fitok
      character ascpic(1000)*80
c-----------------------------------------------------------------------
c
c calculate extremes of box around object, with size 4*bmaj (major axis
c of beam) in pixels (or larger if a later iteration).
c
      lmn = max(1,nint(xpos-2*boxsize))
      lmx = min(nx,nint(xpos+2*boxsize))
      mmn = max(1,nint(ypos-2*boxsize))
      mmx = min(ny,nint(ypos+2*boxsize))
c
c set number of lines to be used in ascii pic including blank
c line at start and end
c
      maxline = mmx - mmn + 3
      if (maxline.gt.1000) then
        call bug('w','Big object skipped - more than 1000 pixels'
     *           //'in one dimension')
        fitok = .false.
        return
      endif
      do i = 1, maxline
       ascpic(i) = ' '
      enddo
c
c  Choose only those pixels within a *circle* of radius 2*boxsize that
c  are also fainter than all the pixels between themselves and the
c  brightest pixel and which aren't blanked.  Store pixel values of
c  object and surrounding region in linear array, data, and fill out
c  the x,y, coordinate.
c
      i = 0
      xt = 0
      yt = 0
      do mm = mmn, mmx
       do ll = lmx,lmn,-1
        incirc = ((ll-xpos)**2 + (mm-ypos)**2).lt.(4*boxsize**2)
        call slopey(ll,mm,lx,my,nx,ny,image,fainter)
        if ((image(ll,mm).gt.clip) .and. incirc .and. fainter .and.
     *      (nimage(ll,mm).gt.0)) then
         if ((ll.eq.lx) .and. (mm.eq.my)) then
          ascpic(mm-mmn+2) = 'O'//ascpic(mm-mmn+2)(:79)
         else
          ascpic(mm-mmn+2) = '*'//ascpic(mm-mmn+2)(:79)
         endif
         i = i + 1
         data(i) = image(ll,mm) - base0
c
c convert ll,mm from binned subimage pixels to full image pixels
c
         wa(1) = dble(ll)
         wa(2) = dble(mm)
         call ppconcg(2, blc(1), bin(1), wa(1))
         call ppconcg(2, blc(2), bin(2), wa(2))
         x(i) = real(wa(1))
         y(i) = real(wa(2))
         xt = xt + x(i)
         yt = yt + y(i)
        else
         ascpic(mm-mmn+2) = ' '//ascpic(mm-mmn+2)(:79)
        endif
       enddo
      enddo
      ndata = i
      m = ndata
      xoff = xt / real(ndata)
      yoff = yt / real(ndata)
c
      end
c
c
      subroutine search(lin, nx, ny, image, nimage, blc, ibin, jbin,
     *   krng, llog, rmsbox, alpha, xrms, auto, negative, pbcor, in,
     *   fdrimg, sigmaimg, rmsimg, normimg, kvannot, fdrpeak, allpix,
     *   psfsize, size)
c-----------------------------------------------------------------------
c  This is the *new* master subroutine for the detecting of sources.
c  It runs subroutine fdr, which makes the normalised image, sigma image
c  and fdr image (if necessary), and most importantly, establishes the
c  fdr threshold. Then sources are measured, in routine fdrfit.
c
c  Input:
c     lin       Image handle
c     nx,ny     Size of image
c     image     Image
c     nimage    blanking image (0 for blanked pixels)
c     blc       blc of window being displayed
c     i,jbin    Spatial pixel increment
c     krng      Start plane and number of planes averaged together
c               to make the current displayed plane
c     llog      Handle of log file
c     rmsbox    size of box (in pixels) within which background mean and
c               rms are calculated for making normalised image.
c     alpha     FDR percentage of 'false discoveries' for setting
c               threshold when finding probable source pixels.
c     xrms      multiple of background rms above which source must be
c               before user is given option of saying yay or nay.
c     auto      true if not being run interactively
c     negative  inverts image: positive pixels become negative and vice
c               versa.
c     pbcor     true to correct fluxes by primary beam attenuation
c     in        image name
c     fdrimg    true to output FDR-image
c     sigmaimg  true to output sigma-image
c     rmsimg    true to output rms-image
c     normimg   true to output normalised image
c     kvannot   true if want to output a kview annotation file for the
c               detected objects
c     fdrpeak   true if want to allow fitting of all source pixels, not
c               just those above the fdr threshold.
c     allpix    true if want to use all FDR pixels, not just
c               monotonically decreasing ones.
c     psfsize   true to restrict minimum source size to that of psf
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'sfind.h'
c
      integer nx, ny, blc(2), llog, ibin, jbin, lin, nimage(nx,ny),
     *  krng(2), size(maxnax)
      integer ipim,ip2im,ip3im,ip4im
      real image(nx,ny), alpha, xrms
      logical negative, pbcor, fdrimg, sigmaimg, rmsimg, normimg,
     *        auto, kvannot, fdrpeak, allpix, psfsize
      character in*(*)
cc
      real bvol, bmaj, bmin, bpa, bvolp, bmajp, bminp, bpap
      real pcut
      integer iostat, len1, rmsbox, nfdrpix
      integer k, l, m, bin(2)
      character cch*1
      character line1*80, line2*160, line3*160, line4*160
      logical nobeam
c-----------------------------------------------------------------------
c
c Start by checking beam parameters, since will die if the beam doesn't
c exist.
c
      call BeamPar(lIn, k, bvol, bvolp, bmaj, bmin, bpa,
     *              bmajp, bminp, bpap, nobeam)
      if (nobeam) then
       call bug('f','No beam information found - FDR analysis'
     *      //' needs this to work')
      endif
c Now begin properly.
      call output(' ')
      call output('****************************************')
      call output('Beginning Non-Interactive Source Finding')
      call output('****************************************')
      call output(' ')
      call output('Please be patient...')
c
c Initialize
c
      bin(1) = ibin
      bin(2) = jbin
      cch = ' '
c
c Invert image if options=negative selected
c
      if (negative) then
       do l = 1, nx
        do m = 1, ny
         image(l,m) = -image(l,m)
        enddo
       enddo
      endif
c
c Write header for output
c
      write(line1,19) '# Sources from image ',in
19    format(a21,a59)
      write(line2,30) '#','RA','DEC','err(RA)','err(DEC)','pk-flux',
     *                'err','flux','bmaj','bmin','pa',
     *                'rms(bg)','rms(fit)'
30    format(a,4x,a,9x,a,5x,a,1x,a,1x,a,3x,a,6x,
     *       a,3x,a,3x,a,3x,a,1x,a,1x,a)
      line3 = '#                       arcsec   arcsec   '//
     *        'mJy      mJy       mJy  arcsec arcsec '//
     *        'deg  mJy     mJy'
      line4 = '#-----------------------------------------'//
     *        '-------------------------------------'//
     *        '----------------'
c
c Write output header for log file
c
      call txtwrite(llog, line1, len1(line1), iostat)
      call txtwrite(llog, line2, len1(line2), iostat)
      call txtwrite(llog, line3, len1(line3), iostat)
      call txtwrite(llog, line4, len1(line4), iostat)
c
c Set the plane appropriate to the displayed image.  Since
c this may be a range of planes averaged together, take the
c integer nearest the average plane
c
      k = nint(real(krng(1) + krng(1) + krng(2) - 1)/2.0)
c
c use FDR to establish P-values for pixels and what P_cut is
c
c try to allocate memory for plist...
      call memalloc(ipim,nx*ny,'r')
c and for image2 (p-value array)
      call memalloc(ip2im,nx*ny,'r')
c and for meanimg and sgimg
      call memalloc(ip3im,nx*ny,'r')
      call memalloc(ip4im,nx*ny,'r')
c
      call fdr(nx,ny,l,m,rmsbox,image,nimage,alpha,xrms,bmajp,bminp,
     *         pcut,lin,memr(ipim),memr(ip2im),memr(ip3im),
     *         memr(ip4im),fdrimg,sigmaimg,rmsimg,normimg,auto,
     *         blc,bin,size(1),size(2),nfdrpix)
c
c Now do the source fitting, in the original image, using the pixels
c selected by FDR to be above the background.
c
      call fdrfit(nx,ny,image,nimage,bvol,bpap,bvolp,bmajp,bminp,pcut,
     *  memr(ip2im),lin,krng,blc,bin,negative,pbcor,llog,kvannot,
     *  fdrpeak,allpix,psfsize,memr(ip3im),memr(ip4im))
c free meanimg and sgimg memory
      call memfree(ip3im,nx*ny,'r')
      call memfree(ip4im,nx*ny,'r')
c
      call memfree(ipim,nx*ny,'r')
      call memfree(ip2im,nx*ny,'r')
c
      call output(' ')
c
      end
c
c
      subroutine basecal(nx, ny, l, m, base0, sigma, rmsbox, image,
     *                    nimage, boxsize, ok, mask, image2, auto)
c-----------------------------------------------------------------------
c    Calculate the base level around the found source using "imsad's"
c    method of fitting a gaussian to the pixel histogram, using
c    all pixels falling within a square of side length rmsbox pixels
c    centred on the peak. If this fails, fall back on iterative method
c    used in the original version of sfind.
c
c  Input:
c    nx,ny   Size of image array
c    l,m     Bright pixel position
c    image   Image matrix with pixel values
c    nimage  blanking image
c    rmsbox  Side length of box within which the background rms is
c            calculated
c    boxsize 1.5*bmajp
c  Output:
c    base0   The base-level (the mean of the gaussian background)
c    sigma   The background rms
c    ok      If true, was able to determine base and sigma
c
c    mask  \ These are both passed here from calling routine so memory
c    image2/ can be allocated for them in advance, to stop crashes in
c            the case of large images being used.
c    auto    true if not being run interactively
c-----------------------------------------------------------------------
      include 'maxnax.h'
      integer ii,jj,lmn,lmx,mmn,mmx,nx,ny,kk,l,m,rmsbox, half, ptr
      real base0, base1, base2, basen, sigma, rx, rr
      real image(nx,ny), boxsize, rng(2), image2((rmsbox+1)*(rmsbox+1))
      real mean, sigmax
      integer nimage(nx,ny), blc(MAXNAX), trc(MAXNAX)
      logical ok,histok, mask((rmsbox+1)*(rmsbox+1)), auto
c-----------------------------------------------------------------------
      half= rmsbox/2
      lmn = max(0,l-half) + 1
      lmx = min(nx,(lmn+rmsbox-1))
      mmn = max(0,m-half) + 1
      mmx = min(ny,(mmn+rmsbox-1))
      ok = .true.
c
c calling subroutine hist (from imsad.for) to get histogram fitted sigma
c value for comparison with rms value calculated below.
      blc(1) = lmn
      blc(2) = mmn
      trc(1) = lmx
      trc(2) = mmx
      rng(1) = 1e30
      rng(2) = -1e30
      ptr = 0
      do jj = mmn, mmx
       do ii = lmn, lmx
        ptr = ptr + 1
        if (nimage(ii,jj).gt.0) then
         image2(ptr) = image(ii,jj)
         mask(ptr) = .true.
         rng(1) = min(rng(1),image(ii,jj))
         rng(2) = max(rng(2),image(ii,jj))
        else
         mask(ptr) = .false.
        endif
       enddo
      enddo
c Before the real "hist" call do it the hard way as a sanity check.
c
c First estimate of sigma from all pixels
      base0 = 0.0
      base1 = 0.0
      base2 = 0.0
      basen = 0
      do jj = mmn, mmx
       do ii = lmn, lmx
         if (nimage(ii,jj).gt.0) then
           basen = basen + 1.0
           base1 = base1 + image(ii,jj)
           base2 = base2 + image(ii,jj)**2
         endif
       enddo
      enddo
      if (basen.gt.0) then
       base0 = base1/basen
       if (base2/basen-base0**2.gt.0.0) then
         sigma = sqrt(base2/basen - base0**2)
       else
         ok =.false.
       endif
      else
       ok = .false.
      endif
      if (.not.ok) return
c
c Now iterate 3 times from this starting point
c
      do kk = 1, 3
       base1 = 0.0
       base2 = 0.0
       basen = 0.0
       do ii = lmn, lmx
         rx = (ii - l)**2
         do jj = mmn, mmx
           rr = rx + (jj-m)**2
           if (nimage(ii,jj).gt.0 .and. rr.ge.(boxsize**2) .and.
     *          abs(image(ii,jj)-base0).le.3*sigma) then
             basen = basen + 1.0
             base1 = base1 + image(ii,jj)
             base2 = base2 + image(ii,jj)**2
           endif
         enddo
       enddo
c
c Mean and sigma
c
       base0 = base1/basen
       if (base2/basen-base0**2.gt.0.0) then
         sigma = sqrt(base2/basen - base0**2)
       else
         ok =.false.
         return
       endif
      enddo
c
c If on last iteration, had less than 10 pixels,
c set base level to zero and set ropy sigma
c
      if (basen.lt.10.0) then
       base0 = 0.0
       sigma = sqrt(base2/basen)
      endif
c
c Now call hist, and use its output over the direct estimate if sane.
      sigmax = 0.0
      call hist(blc,trc,image2,mask,rng,histok,sigmax,mean,auto)
      if (histok .and. (sigmax.lt.(10*sigma))) then
       base0 = mean
       sigma = sigmax
      endif
      return
c
      end
c
c
      subroutine hist(blc,trc,image,mask,range,ok,trms,mean,auto)
c-----------------------------------------------------------------------
c     Compute image plane histogram and moments
c hacked from imsad.for by amh (2000/11/24)
c
c  Input
c    blc    x,y of blc of image region of interest
c    trc    x,y of trc of image region of interest
c    image  image pixel values, a 1-D array of these rather than 2-D
c    mask   mask (1-D logical array corresponding to 'image' derived
c            from nimage)
c    range  the min and max of the current plane
c  Output
c    trms   sigma of the gaussian background
c    mean   mean of the gaussian background
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'sfind.h'
c
      integer MAXBOX, MAXVAR, MAXBIN
      parameter (MAXBOX=1024, MAXVAR=20, MAXBIN=50)
c
      integer i, j, n, nd, ifail1, ifail2, nvar, ni, nj,
     *  blc(MAXNAX), trc(MAXNAX), id
      integer nwithinr
      real trms, mom(3), wmin, wmax, wid,
     *  rms, sum, squ, covar(MAXVAR*MAXVAR), xvar(MAXVAR), image(*),
     *  bmax, norm, range(2), drange, xtemp(MAXBIN), dtemp(MAXBIN)
      real mean
c
      logical mask(*), ok, auto
c
      external function
c-----------------------------------------------------------------------
      nd = MAXBIN
      ok = .true.
      nwithinr = 0
c
c Initialise some things
c
      sum = 0.0
      squ = 0.0
      do i = 1, nd
        data2(i) = 0
        xd(i) = 0.0
      enddo
c
c Read image data and compute moments
c
      ni = trc(1) - blc(1) + 1
      nj = trc(2) - blc(2) + 1
c
      n = 0
      do i = 1, ni*nj
        if (mask(i)) then
          n = n + 1
          sum = sum + image(i)
          squ = squ + image(i)**2
        endif
      enddo
c
      if (n.eq.0) then
        if (.not.auto) call bug('w', 'Image plane all blank')
        ok = .false.
        return
      endif
c
c Compute mean and standard deviation
c
      mom(1) = sum/n
      mom(2) = sqrt(squ/n - mom(1)**2)
c
c Set bin width and limits; try to exclude distant outliers
c
      drange = 2*min(abs(range(1)),abs(range(2)))
      if (drange.gt.0.0) then
        wmin = mom(1) - drange / 2.0
        wmax = mom(1) + drange / 2.0
      else
        wmin = range(1)
        wmax = range(2)
      endif
      wid = (wmax - wmin) / nd
c
c Form histogram
c
      do i = 1, ni*nj
        if (mask(i) .and. image(i).ge.wmin .and. image(i).le.wmax) then
          id = int((image(i) - wmin) / wid) + 1
          id = max(1,min(id,nd))
c
          data2(id) = data2(id) + 1.0
          xd(id) = xd(id) + image(i)
        endif
      enddo
c
c Work out some scaling factors and set the abcissa bin values by the
c average contributing to that bin
c
      norm = 0.0
      do i = 1, nd
        if (data2(i).gt.0.0) then
          xd(i) = xd(i) / data2(i)
          norm = norm + data2(i)
        endif
        xtemp(i) = xd(i)
        dtemp(i) = data2(i)
      enddo
c
c Remove empty bins from fit
c
      j = 1
      do i = 1, nd
        if (dtemp(i).gt.0.0) then
          data2(j) = dtemp(i)
          xd(j) = xtemp(i)
          j = j + 1
        endif
      enddo
      nd = j - 1
c
      if (nd.eq.0) then
c        call bug('w','No data in histogram')
        ok = .false.
        return
      endif
c
c Normalise histogram volume to 1.0
c
      bmax = -1e30
      do i = 1, nd
        data2(i) = data2(i) / norm
        bmax = max(bmax,data2(i))
      enddo
c
c Set Gaussian fits parameter estimates.
c 1: peak, 2: centre, 3: FWHM
c
c l0 = mean (mu), m0 = FWHM (and sigma=rms=m0/2sqrt(2ln2)).
      flux = bmax
      l0 = mom(1)
      m0 = mom(2) * sqrt(8.0 * log(2.0))
c
      fwhm1 = 0.0
      fwhm2 = 0.0
      pa    = 0.0
c
      vflux  = .true.
      vl0    = .true.
      vm0    = .true.
      vfwhm1 = .false.
      vfwhm2 = .false.
      vpa    = .false.
c
c Fit histogram with a Gaussian
c
      xoff = 0.0
      yoff = 0.0
      gdim = 1
      ndata = nd
c
      call packvar(xvar,nvar,MAXVAR)
      if (nvar.eq.0) call bug('f', 'HIST: Internal logic error')
c
      call lsqfit(FUNCTION,nd,nvar,xvar,covar,rms,ifail1,ifail2)
c
      call upackvar(xvar,nvar)
      if (ifail2.eq.0) call upackcov(covar,nvar)
      if (ifail1.ne.0 .or. ifail2.ne.0) then
        ok = .false.
        return
      endif
c
c Compute fitted image rms, and mean
c
      trms = abs(m0) / sqrt(8.0 * log(2.0))
      mean = l0
c
      end
c
c
      subroutine decopt(do3val, do3pix, eqscale, mark, doerase,
     *                  dowedge, dofid, grid, nofit, asciiart, auto,
     *                  negative, pbcor, oldsfind, fdrimg, sigmaimg,
     *                  rmsimg, normimg, kvannot, fdrpeak, allpix,
     *                  psfsize)
c-----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     do3val    True means label sub-plots with value of third axis
c     do3pix    True means label sub-plots with pixel of third axis
c     eqscale   True means plot with x and y scales
c     mark      Mark cursor location
c     doerase   Erase rectangle behind "3-axis" strings
c     dowedge   Draw wedge on pixel map
c     dofid     FIddle lookup table of pixel map
c     grid      Draw coordinate grid
c     nofit     True means don't do gaussian fitting for each source
c     asciiart  display ascii representations of each source during
c               interactive source selection
c     auto      True means skip all interactive bits, including
c               displaying image.
c     negative  inverts image: positive pixels become negative and vice
c               versa.
c     pbcor     true to correct fluxes by primary beam attenuation
c     oldsfind  true if want to use sfind in original mode (no fdr)
c     fdrimg    true if want to create fdr-selected output pixel image
c     sigmaimg  true if want to create sigma-clipped output pixel image
c     rmsimg    true if want to create rms-image
c     normimg   true if want to create 'normalised' output image
c     kvannot   true if want to output a kview annotation file for the
c               detected objects.
c     fdrpeak   true if want to allow fitting of all source pixels, not
c               just those above the fdr threshold.
c     allpix    true if want to use all pixels, not just monotonically
c               decreasing ones.
c     psfsize   true to restrict minimum source size to that of the psf.
c-----------------------------------------------------------------------
c
      logical do3val, do3pix, eqscale, mark, doerase, dofid,
     * dowedge, grid, nofit, asciiart, auto, negative, pbcor,
     * oldsfind, fdrimg, sigmaimg, rmsimg, normimg, kvannot,
     * fdrpeak, allpix, psfsize
cc
      integer maxopt
      parameter (maxopt = 22)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'3value  ', '3pixel  ', 'unequal ',
     *              'mark    ', 'noerase ', 'wedge   ',
     *              'fiddle  ', 'grid    ', 'nofit   ',
     *              'asciiart', 'auto    ', 'negative',
     *              'pbcorr  ', 'oldsfind', 'fdrimg  ',
     *              'sigmaimg', 'normimg ', 'kvannot ',
     *              'fdrpeak ', 'allpix  ', 'psfsize ',
     *              'rmsimg'/
c-----------------------------------------------------------------------
      call optcg('options', opshuns, present, maxopt)
c
      do3val   =      present(1)
      do3pix   =      present(2)
      eqscale  = .not.present(3)
      mark     =      present(4)
      doerase  = .not.present(5)
      dowedge  =      present(6)
      dofid    =      present(7)
      grid     =      present(8)
      nofit    =      present(9)
      asciiart =      present(10)
      auto     =      present(11)
      negative =      present(12)
      pbcor    =      present(13)
      oldsfind =      present(14)
      fdrimg   =      present(15)
      sigmaimg =      present(16)
      normimg  =      present(17)
      kvannot  =      present(18)
      fdrpeak  =      present(19)
      allpix   =      present(20)
      psfsize  =      present(21)
      rmsimg   =      present(22)
c make auto true for fdr-type analysis regardless of user input
      if (.not.oldsfind) auto = .true.
c
c circumvent possible irritating bug of ascii pictures being printed out
c in auto mode by overriding asciiart parameter.
c
      if (auto .and. asciiart) then
        asciiart = .FALSE.
        call output('Auto mode: Asciiart option being overridden.')
      endif
c
      end
c
c
      subroutine inputs(maxlev, in, ibin, jbin, kbin, levtyp, slev,
     *   levs, nlevs, pixr, trfun, pdev, labtyp, logfil, do3val, do3pix,
     *   eqscale, nx, ny, cs, dopixel, mark, doerase, dowedge, dofid,
     *   grid, cut, rmsbox, alpha, xrms, nofit, asciiart, auto,
     *   negative, pbcor, oldsfind, sigmaimg, rmsimg, fdrimg, normimg,
     *   kvannot, fdrpeak, allpix, psfsize)
c-----------------------------------------------------------------------
c     Get the unfortunate user's long list of inputs
c
c  Input:
c   maxlev     Maximum number of allowed contour levels
c  Output:
c   in         Image name.
c   i,j,kbin   X, y and z pixel increment and average
c   levtyp     Type of contour levels scale factor
c              'p'(ercentage) or 'a'(bsolute)
c   slev       Contour levels scale factors (absolute or percentage)
c   levs       Contour levels.  Will be scaled by SLEV for contouring
c   nlevs      Number of contour levels
c   pixr       Pixel map intensity range
c   trfun      Type of pixel map transfer function: 'log', 'lin',
c             'heq', or 'sqr'
c   pdev       PGPLOT plot device/type
c   labtyp     Type of labels for x and y axes
c   logfil     Log file name.
c   do3val     True means label sub-plots with value of third axis
c   do3pix     True means label sub-plots with pixel of third axis
c   eqscale    True means plot with x and y scales
c   nx,ny      Number of sub-plots per page
c   cs         PGPLOT character sizes for the plot axis labels and
c              velocity/channel label,
c   dopixel    True for pixel map, false for contour plot
c   mark       Mark cursor locations
c   doerase    Erase rectangle behind "3-axis" value
c   dofid      FIddle lookup tbale of pixel map
c   dowedge    Draw wedge with pixel map
c   grid       Draw coordinate grid
c   cut        flux limit below which sources are ignored
c   rmsbox     size of box (in pixels) within which background rms is
c              calculated
c   alpha      FDR percentage of 'maximum, on average' false detections
c   xrms       multiple of background rms above which source must be
c              before user is given option of saying yay or nay.
c   nofit      True means don't do gaussian fitting for each source
c   asciiart   display ascii representations of each source during
c              interactive source selection
c   auto       true means skip all interactive sections of program,
c              including image display.
c   negative   inverts image: positive pixels become negative and vice
c              versa.
c   pbcor      true to correct fluxes by primary beam attenuation
c   oldsfind   true if want to use sfind in original mode (no fdr)
c   fdrimg     true if want to create fdr-selected output pixel image
c   sigmaimg   true if want to create sigma-clipped output pixel image
c   rmsimg     true if want to create rms output pixel image
c   normimg    true if want to create 'normalised' output image
c   kvannot    true if want to output a kview annotation file for the
c              detected objects
c   fdrpeak    true if want to allow fitting of all source pixels, not
c              just those above the fdr threshold.
c   allpix     true if want to use all pixels, not just monotonically
c              decreasing ones
c   psfsize    true to restrict minimum source size to that of the psf
c-----------------------------------------------------------------------

      integer maxlev, nx, ny, nlevs, ibin(2), jbin(2), kbin(2), rmsbox
      real levs(maxlev), pixr(2), cs(2), slev, cut, xrms, alpha
      character*(*) in, labtyp(2), levtyp, logfil, pdev, trfun
      logical do3val, do3pix, eqscale, dopixel, mark, doerase,
     * dowedge, dofid, grid, nofit, asciiart, auto, negative, pbcor,
     * oldsfind, fdrimg, sigmaimg, rmsimg, normimg, kvannot, fdrpeak,
     * allpix, psfsize

      integer ntype, nlab, ntype2, nimtype
      parameter (ntype = 14, ntype2 = 3)
      character type(ntype)*6, imtype*7, type2(ntype2)*7
      data type  /'hms   ', 'dms   ', 'abspix', 'relpix',
     *            'arcsec', 'arcmin', 'absghz', 'relghz',
     *            'abskms', 'relkms', 'absnat', 'relnat',
     *            'absdeg', 'reldeg'/
      data type2 /'contour', 'pixel', 'grey'/
c-----------------------------------------------------------------------
      call keyini
      call keyf('in', in, ' ')
      if (in.eq.' ') call bug('f', 'No image specified')
      call keymatch('type', ntype2, type2, 1, imtype, nimtype)
      if (nimtype.eq.0) imtype = 'pixel'
      dopixel = .true.
      if (imtype.eq.'contour') dopixel = .false.

      call keyi('xybin', ibin(1), 1)
      call keyi('xybin', ibin(2), ibin(1))
      if (ibin(2).ne.1 .and. ibin(2).ne.ibin(1)) call bug('f',
     *  'Non-unit x spatial averaging must be equal to increment')
      ibin(1) = max(ibin(1), 1)
      ibin(2) = max(ibin(2), 1)
c
      call keyi('xybin', jbin(1), ibin(1))
      call keyi('xybin', jbin(2), jbin(1))
      if (jbin(2).ne.1 .and. jbin(2).ne.jbin(1)) call bug('f',
     *  'Non-unit y spatial averaging must be equal to increment')
      jbin(1) = max(jbin(1), 1)
      jbin(2) = max(jbin(2), 1)

      call keyi('chan', kbin(1), 1)
      call keyi('chan', kbin(2), 1)
      kbin(1) = max(kbin(1), 1)
      kbin(2) = max(kbin(2), 1)

      call keya('slev', levtyp, 'a')
      call keyr('slev', slev, 0.0)
      call lcase(levtyp)
      if (levtyp.ne.'p' .and. levtyp.ne.'a') call bug('f',
     *   'Unrecognized contour level scale type; must be "p" or "a"')

      call mkeyr('levs', levs, maxlev, nlevs)

      call keyr('range', pixr(1), 0.0)
      call keyr('range', pixr(2), 0.0)
      call keya('range', trfun, 'lin')
      call lcase(trfun)
      if (dopixel) then
        if (trfun.ne.'lin' .and. trfun.ne.'log' .and. trfun.ne.'heq'
     *      .and. trfun.ne.'sqr') call bug('f',
     *    'Unrecognized pixel map transfer function type')
      else
        trfun = ' '
      endif

      call decopt(do3val, do3pix, eqscale, mark, doerase, dowedge,
     *             dofid, grid, nofit, asciiart, auto, negative,
     *             pbcor, oldsfind, fdrimg, sigmaimg, rmsimg, normimg,
     *             kvannot, fdrpeak, allpix, psfsize)
      if (.not.dopixel) then
        dofid = .false.
        dowedge = .false.
      endif

      call keya('device', pdev, ' ')
      if ((pdev.eq.' ') .and. (.not.auto)) then
        call pgldev
        call bug('f', 'A PGPLOT device must be given')
      endif

      call keymatch('labtyp', ntype, type, 2, labtyp, nlab)
      if (nlab.eq.0) labtyp(1) = 'abspix'
      if (nlab.le.1) then
        labtyp(2) = labtyp(1)
        if (labtyp(1).eq.'hms') labtyp(2) = 'dms'
      endif

      call keya('logfile', logfil, ' ')
      if (logfil.eq.' ') logfil = 'sfind.log'

      if ((index(labtyp(1),'lin').ne.0  .and.
     *      index(labtyp(2),'lin').eq.0) .or.
     *     (index(labtyp(2),'lin').ne.0  .and.
     *      index(labtyp(1),'lin').eq.0)) then
        if (eqscale) call bug('i',
     *  'You might consider options=unequal with these axis types')
      endif

      call keyi('nxy', nx, 0)
      call keyi('nxy', ny, nx)

      call keyr('csize', cs(1), 0.0)
      call keyr('csize', cs(2), 0.0)

      call keyr('cutoff',cut,0.0)
      if (cut.lt.0.0)
     *  call bug('f','You must give a non-negative value for cutoff')
      call keyi('rmsbox',rmsbox,20)
      if (rmsbox.lt.20) then
        rmsbox = 20
        call bug('w',
     *    'rmsbox must be at least 20.  Setting it to 20 now')
      endif

      call keyr('alpha',alpha,2.0)
      if ((alpha.le.0.0) .and. (.not.oldsfind))
     *  call bug('f','You must use a positive value for alpha.')
      call keyr('xrms',xrms,0.0)
      if (xrms.le.0.0) then
        if ((oldsfind) .or. (sigmaimg)) then
          call bug('f','Invalid value for keyword "xrms"')
        endif
      endif

      end


      subroutine setlgc(labcol, poscol, statcol, regcol)
c-----------------------------------------------------------------------
c     Set line graphics colours
c
c  OUtput
c    colour indices to use
c-----------------------------------------------------------------------
      integer labcol, poscol, statcol, regcol
cc
      integer bgcol
c-----------------------------------------------------------------------
c
c See if black or white background
c
      call bgcolcg(bgcol)
c
c Labels first
c
      labcol = 7
      if (bgcol.eq.1) then
c
c White background
c
        labcol = 2
      else if (bgcol.eq.0) then
c
c Black background
c
        labcol = 7
      else
        call bug('w', 'Non black/white background colour on device')
        labcol = 7
      endif
c
c Now cursor options
c
      poscol = 3
      statcol = labcol
      regcol = 8
c
      end
c
c
      subroutine region(in, naxis, size, ibin, jbin, kbin, blc, trc,
     *                   win, ngrps, grpbeg, ngrp)
c-----------------------------------------------------------------------
c     Finish key routine inputs for region of interest now.
c
c  Input:
c    in            Image file name
c    naxis         Number of dimensions of image
c    size          Dimensions of image
c    i,j,kbin      Pixel increment and binning in x,yz directions
c  Output:
c    ngrps         Number of groups of channels.
c    grgbeg        List of start planes for each group of channels
c                  that are to be avearged together for each sub-plot
c                  A new group is begun at every interruption to the
c                  continuity of the selected channels, or if the
c                  channel increment is reached.
c    ngrp          Number of channels in each group of channel to
c                  be averaged together for each sub-plot.
c    blc,trc       3-D Hyper-rectangle surrounding region of interest
c    win           Size of BINNED region of interest for
c                  first 2 dimensions
c
c-----------------------------------------------------------------------
c
      integer naxis, size(naxis), blc(*), trc(*), win(2), ngrp(*),
     *  grpbeg(*), ngrps, ibin(2), jbin(2), kbin(2)
      character in*(*)
cc
      include 'maxdim.h'
      integer maxbox, i
      parameter (maxbox = 1024)
c
      integer boxes(maxbox)
c-----------------------------------------------------------------------
      call boxinput('region', in, boxes, maxbox)
      call boxset(boxes, naxis, size, 's')
      call keyfin
c
c Find hyper-rectangle surrounding region of interest
c
      call boxinfo(boxes, 3, blc, trc)
      do i = 1, min(3,naxis)
        blc(i) = max(1,blc(i))
        trc(i) = min(size(i),trc(i))
      enddo
c
c Adjust spatial window to fit an integral number of bins and
c find size of binned window
c
      call winfidcg(size(1), 1, ibin, blc(1), trc(1), win(1))
      call winfidcg(size(2), 2, jbin, blc(2), trc(2), win(2))
c
c Find list of start channels and number of channels for each group
c of channels selected.
c
      call chnselcg(blc, trc, kbin, maxbox, boxes, ngrps, grpbeg, ngrp)
c
      end
c
c
      subroutine fitting(lIn, krng, nx, ny, image, rms, xpos,
     *    xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj,
     *    amin, posa, posns, blc, bin, lx, my, bvol, bpap,
     *    bvolp, bmajp, bminp, nimage, boxsize, image2, pcut, fitok,
     *    usedpixels, meanimg, sgimg, fdrpeak, allpix, psfsize,
     *    dumcount)
c-----------------------------------------------------------------------
c  Do elliptical Gaussian fit to source. First the pixels to be used in
c  the fitting are selected on the basis that they are monotonically
c  decreasing away from the "central" bright pixel, then the Gaussian
c  fit is done as per the task "imfit". If the size of the region within
c  which pixels were selected for the fit is smaller than 4 times the
c  fwhm of the major axis of the source, the procedure is iterated with
c  a larger region.  If this procedure diverges, (detected by the
c  increasing region becoming larger than the image size), the source is
c  ignored.
c
c  Once the fit is done the source parameters are recorded and passed
c  back to subroutine "search" for it to output as appropriate.
c
c  Input
c    nx,ny     Size of image
c    image     Image
c    nimage    Normalised Image (for masking info)
c    bmajp,bminp
c              Pixel size of major/minor axis of beam.  Used for
c              defining 'area' covered by beam, to set minimum number of
c              FDR pixels needed before fitting a source.
c    bpap      orientation of beam (in pixel coords).
c    blc, bin  image params used for switching between full image and
c              binned subimage pixels.
c    lx, my    integer pixel coord of brightest pixel in object. Used in
c              pixel selection routine.
c    bvol,     beam volume in world and pixel coords, resp.
c    bvolp
c    image2    Image of p-values for each pixel
c    pcut      cutoff p-value below which pixels are likely to be in
c              source.
c    fdrpeak   true to use all source pixels in fit, not just those less
c              than pcut.
c    allpix    true to use all pixels, not just monotonically decreasing
c              ones
c    psfsize   true to limit minimum source size to that of the psf.
c
c  Output
c    posns            The object position in world coords.
c    xpos, xposerr    x-position and error of object.
c    ypos, yposerr    y-position and error of object.
c    pkfl, pkflerr    peak flux density (and error) for object.
c    intfl            integrated flux density of object.
c    amaj, amin, posa axes (arcsec) and position angle (degrees) of
c                     object.
c    rms              rms of gaussian fit to selected pixels.
c    fitok            logical. False if the fit failed for any reason.
c    usedpixels       number of pixels used in the fit
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'
      include 'sfind.h'
      integer MAXVAR,n
      parameter (MAXVAR=20, n=1000)
c
      logical dofit, fitok, fdrpeak, fdrpeakdum, allpix, psfsize
      integer ifail1,ifail2,lIn, i, blc(2), bin(2), maxline
      integer k,m,nvar,lx,my, nx,ny, krng(2), nimage(nx,ny), fiterr
      integer boxsize,ipim,ip2im,xpixused(n),ypixused(n), usedpixels
      integer nfdrused, dumcount
      real image(nx,ny), meanimg(nx,ny), sgimg(nx,ny)
      real xvar(MAXVAR),covar(MAXVAR*MAXVAR),rms
      real bvol,bvolp, xpos, ypos, pkfl, intfl
      real amaj, amin, posa, bmajp, bminp, bpap
      real xposerr, yposerr, pkflerr, image2(nx,ny), pcut
      double precision posns(2)
c
c  Externals.
c
      external FUNCTION
c-----------------------------------------------------------------------
c
c Initialise parameters
c
      vflux  = .true.
      vl0    = .true.
      vm0    = .true.
      vfwhm1 = .true.
      vfwhm2 = .true.
      vpa    = .true.
      fwhm1  = 1.0
      fwhm2  = 1.0
      l0     = 0.0
      m0     = 0.0

      fitok = .true.
c
c Set the plane appropriate to the displayed image.  Since
c this may be a range of planes averaged together, take the
c integer nearest the average plane
c
      k = nint(real(krng(1) + krng(1) + krng(2) - 1)/2.0)
c allocate memory for slopearry and connct
1200  call memalloc(ipim,(boxsize+1)*(boxsize+1),'l')
      call memalloc(ip2im,(boxsize+1)*(boxsize+1),'l')
c initialise x,ypixused
      do i = 1, n
       xpixused(i) = 0
       ypixused(i) = 0
      enddo
c
c  Load the data. Ie, choose the pixels in the region of the source to
c  be used in the fitting procedure, and encode them in a format ready
c  to do so.
c
      call LoadDat(m,nx,ny,xpos,ypos,blc,bin,image,boxsize,lx,my,
     *  nimage,maxline,image2,pcut,fitok,meml(ipim),meml(ip2im),
     *  xpixused,ypixused,meanimg,sgimg,nfdrused,fdrpeak,allpix)
      call memfree(ipim,(boxsize+1)*(boxsize+1),'l')
      call memfree(ip2im,(boxsize+1)*(boxsize+1),'l')
      if (.not.fitok) return
c reject source if number of FDR pixels used is less than 1/4 the
c area covered by beam, or no pixels at all were used.
      if (m.eq.0 .or. float(nfdrused).lt.sqrt(bmajp*bminp*PI)/4.0) then
c      if (m.eq.0) then
       fitok = .false.
       return
      endif
c
c  Convert the coordinates to pixels, and fill in defaults if necessary.
c
      call GetEst
c make sure it's a 2-D gaussian
      gdim = 2
c
c  Pack the variables into an array.
c
      call PackVar(xvar,nvar,MAXVAR)
      dofit = nvar.gt.0
      if (.not.dofit) then
        fitok = .false.
        return
      endif
      if (nvar.ge.m) then
        fitok = .false.
        return
      endif
c
c  Do the fitting process.
c
      fiterr = 0
      call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
      call UPackVar(xvar,nvar)
      if (ifail2.eq.0) call UpackCov(covar,nvar)
      if (ifail1.ne.0) fiterr = 1
      if (ifail2.ne.ifail1) fiterr = 2
c
c If fit is much larger than number of pixels used, trying fitting again
c with more pixels to refine the fit. m is the number of used pixels.
c
      if (m.lt.fwhm1*fwhm2*PI_4) then
         dumcount = dumcount+1
c allocate memory for slopearry and connct again
       call memalloc(ipim,(boxsize+1)*(boxsize+1),'l')
       call memalloc(ip2im,(boxsize+1)*(boxsize+1),'l')
c initialise x,ypixused
       do i = 1, n
        xpixused(i) = 0
        ypixused(i) = 0
       enddo
c
c  Load the data again, using more pixels than last time, by
c temporarily setting the fdrpeak variable to true.
c
       fdrpeakdum = .true.
       call LoadDat(m,nx,ny,xpos,ypos,blc,bin,image,boxsize,lx,my,
     *   nimage,maxline,image2,pcut,fitok,meml(ipim),meml(ip2im),
     *   xpixused,ypixused,meanimg,sgimg,nfdrused,fdrpeakdum,allpix)
       call memfree(ipim,(boxsize+1)*(boxsize+1),'l')
       call memfree(ip2im,(boxsize+1)*(boxsize+1),'l')
       if (.not.fitok) return
c reject source if number of FDR pixels used is less than 1/4 the
c area covered by beam, or no pixels at all were used.
       if (m.eq.0 .or. float(nfdrused).lt.sqrt(bmajp*bminp*PI)/4.0) then
         fitok = .false.
         return
       endif
c
c  Convert the coordinates to pixels, and fill in defaults if necessary.
c
       call GetEst
c make sure it's a 2-D gaussian
       gdim = 2
c
c  Pack the variables into an array.
c
       call PackVar(xvar,nvar,MAXVAR)
       dofit = nvar.gt.0
       if (.not.dofit) then
         fitok = .false.
         return
       endif
       if (nvar.ge.m) then
         fitok = .false.
         return
       endif
c
c  Do the fitting process.
c
       fiterr = 0
       call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
       call UPackVar(xvar,nvar)
       if (ifail2.eq.0) call UpackCov(covar,nvar)
       if (ifail1.ne.0) fiterr = 1
       if (ifail2.ne.ifail1) fiterr = 2
      endif

c
c If 'psfsize' selected, check object size and if smaller than
c psf, set to psf size and fit again for peak and position.
c
      if (psfsize .and. (fwhm1.lt.bmajp .or. fwhm2.lt.bminp)) then
        vflux  = .true.
        vl0    = .true.
        vm0    = .true.
        vfwhm1 = .false.
        vfwhm2 = .false.
        vpa    = .false.
        fwhm1  = bmajp
        fwhm2  = bminp
        pa     = bpap

        fitok = .true.

c       Pack the variables into an array.
        call PackVar(xvar,nvar,MAXVAR)
        dofit = nvar.gt.0
        if (.not.dofit) then
          fitok = .false.
          return
        endif
        if (nvar.ge.m) then
          fitok = .false.
          return
        endif

c       Do the fitting process.
        fiterr = 0
        call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
        call UPackVar(xvar,nvar)
        if (ifail2.eq.0) call UpackCov(covar,nvar)
        if (ifail1.ne.0) fiterr = 1
        if (ifail2.ne.ifail1) fiterr = 2
      endif
c
c  Check whether the fitted size of the image is comparable to the
c  initial box size used in selecting pixels, and if so iterate the
c  procedure choosing a larger box size.
c
      if (boxsize.lt.max(abs(fwhm1),abs(fwhm2))) then
c
c Checks for divergence - eg, noise being fitted by ever larger
c Gaussians.  Stops and aborts fit if object fwhm is larger than one
c quarter the smallest image dimension, or if the fitted fwhm has
c increased to more than 60 pixels. (At sizes > 60 pixels, the next
c iteration takes quite a while, and leaves a big wait for nothing if
c the object is then discarded on the next iteration.)  This
c (moderately) arbitrary number is, however, an unsatisfactory way of
c dealing with the problem, and a better one would be appreciated.
c
       if (max(abs(fwhm1),abs(fwhm2)).lt.min(nx,ny,240)/4) then
        boxsize = 3.0*max(abs(fwhm1),abs(fwhm2))
        goto 1200
       else
        fitok = .false.
        return
       endif
      endif
c Now we are confident source has been fit, it is ok to mess with the
c values in image2
      do i = 1, m
c jacking the pvalue of 'used' pixels up above the cutoff, so
c the same source can't be detected and fit multiple times
       image2(xpixused(i),ypixused(i)) = 1e10
      enddo
c record number of used pixels
      usedpixels = usedpixels + nfdrused

c     Convert to astronomical units.
      call gaucvt(lIn, k, bvol, bvolp, xposerr, yposerr,
     *   pkfl, pkflerr, intfl, amaj, amin, posa, posns)

      end


      subroutine GetEst
c-----------------------------------------------------------------------
c  Generate an initial estimate for a single component model.
c
c-----------------------------------------------------------------------
      include 'sfind.h'
      include 'mirconst.h'

      integer i
      double precision P,XP,YP,XYP,XXP,YYP,SP
      real t,fac
c-----------------------------------------------------------------------
      SP = 0D0
      P  = 0D0
      XP = 0D0
      YP = 0D0
      XYP = 0D0
      XXP = 0D0
      YYP = 0D0

      do i = 1, ndata
        SP  = SP + data(i)
        t   = abs(data(i))
        P   = P   + t
        XP  = XP  + t*x(i)
        YP  = YP  + t*y(i)
        XXP = XXP + t*x(i)*x(i)
        XYP = XYP + t*x(i)*y(i)
        YYP = YYP + t*y(i)*y(i)
      enddo

      XP  = XP / P
      YP  = YP / P
      XXP = XXP / P - XP*XP
      XYP = XYP / P - XP*YP
      YYP = YYP / P - YP*YP
      l0  = XP
      m0  = YP

      fac = 4.0*log(2.0)
      fwhm1 = sqrt(fac*(XXP + YYP + sqrt((XXP-YYP)**2 + 4*(XYP)**2)))
      fwhm2 = sqrt(fac*(XXP + YYP - sqrt((XXP-YYP)**2 + 4*(XYP)**2)))
      pa    = 0.5*atan2(2*XYP,YYP-XXP)
      flux  = sign(fac*P/(PI*fwhm1*fwhm2), SP)

      end


      subroutine LoadDat(m,nx,ny,xpos,ypos,blc,bin,image,boxsize,
     *    lx,my,nimage,maxline,image2,pcut,fitok,slopearry,connct,
     *    xpixused,ypixused,meanimg,sgimg,nfdrused,fdrpeak,allpix)
c-----------------------------------------------------------------------
c  Load the relevant data for this plane.  The relevant data are those
c  pixels to which to fit the elliptical gaussians. They are selected by
c  fulfilling three requirements:
c  1. Lie within a circle of radius boxsize/2 from peak pixel
c  2. FDR probability p-value must be below pcut (ie, pixel likely to be
c     source, not background)
c  3. Pixel value must be monotonically decreasing away from peak pixel,
c
c  Input:
c    nx,ny     Size of image
c    xpos,ypos position of source
c    blc       x,y of blc of region of interest
c    bin       binning factor of image
c    image     image pixel values
c    nimage    image mask values
c    boxsize   sidlength of region of interest
c    lx,my     x,y pixel values of peak pixel
c    maxline   no lines longer than this
c    image2    image of p-values from fdr
c    pcut      p-values cutoff value from fdr
c    fdrpeak   true to use all source pixels in fit, not just those less
c              than pcut.
c    allpix    true to use all pixels, not just monotonically decreasing
c              ones.
c  Output:
c    m         Number of points loaded
c    fitok     false if had problems
c    x,ypixused list of pixel positions to be 'blanked' if fit works,
c               to prevent multiple detections of the same source
c    nfdrused  number of FDR pixels used in the fit
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer m,nx,ny,lmn,lmx,mmn,mmx,nimage(nx,ny)
      integer i,j,xt,yt,ll,mm, blc(2),bin(2), lx,my, maxline
      integer boxsize,sideln,cenx,ceny
      integer xpixused(boxsize+1),ypixused(boxsize+1)
      integer nfdrused,tstx,tsty
      real xpos,ypos,image(nx,ny),image2(nx,ny)
      real sgimg(nx,ny),meanimg(nx,ny),pcut
      real half
      double precision wa(2)
      logical incirc,fainter,fitok,fdrpeak,allpix
      logical slopearry(boxsize+1,boxsize+1)
      logical connct(boxsize+1,boxsize+1)
c-----------------------------------------------------------------------
c
c calculate extremes of box around object, with size boxsize
c in pixels
c
      half = boxsize/2.0
      lmn = max(1,nint(xpos-half))
      lmx = min(nx,nint(xpos+half))
      mmn = max(1,nint(ypos-half))
      mmx = min(ny,nint(ypos+half))
c
c holdover from setting number of lines to be used in ascii pic
c including blank line at start and end - keep to bias against very
c large objects, no object > 1000 pixels in one dimension allowed.
c
      maxline = mmx - mmn + 3
      if (maxline.gt.1000) then
        fitok = .false.
        return
      endif
c make slopearry by testing slopey
c (and maybe whether the pixel is FDR-selected)
      do mm = mmn, mmx
       do ll = lmn, lmx
        i = ll-lmn+1
        j = mm-mmn+1
        if (allpix) then
         slopearry(i,j) = image2(ll,mm).le.pcut
        else
          call slopey(ll,mm,lx,my,nx,ny,image,slopearry(i,j))
c ensures that only pixels >= 1-sigma are used in fit
          if (slopearry(i,j)) then
            slopearry(i,j) = image(ll,mm).ge.sgimg(ll,mm)
          endif
c use this if *all* the pixels in the source are to be above the FDR
c threshold, not just the peak pixel.
          if (.not.fdrpeak) then
            if (slopearry(i,j)) slopearry(i,j) = image2(ll,mm).le.pcut
          endif
        endif
       enddo
      enddo
c make connct by testing annuli consecutively outward from peak pixel
c to check whether each pixel is connected to the peak pixel.
c start by initialising array, then defining peak pixel to be connected.
      do i = 1, boxsize+1
       do j = 1, boxsize+1
        connct(i,j) = .false.
       enddo
      enddo
      cenx = lx-lmn+1
      ceny = my-mmn+1
      connct(cenx,ceny) = .true.
c i is the 'annulus' index.
      do i = 1, boxsize/2
       sideln = 2*i+1
       do j = 1, sideln
c check top
        tstx = cenx + j - 1 - i
        if (tstx.lt.1) tstx = 1
        if (tstx.gt.boxsize) tstx = boxsize
        tsty = min(boxsize,ceny+i)
        if ((.not.connct(tstx,tsty)) .and. (slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx+1,tsty-1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx-1,tsty-1)
         if ((j.gt.1) .and. (j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx-1,tsty-1) .or.
     *         connct(tstx,tsty-1) .or. connct(tstx,tsty-1)
         endif
c check adjacent pixels
         if ((tstx.gt.1) .and. (connct(tstx,tsty))) then
          connct(tstx-1,tsty) = slopearry(tstx-1,tsty)
         endif
         if ((tstx.lt.boxsize) .and. (connct(tstx,tsty))) then
          connct(tstx+1,tsty) = slopearry(tstx+1,tsty)
         endif
        endif
c check bottom
        tstx = cenx + j - 1 - i
        if (tstx.lt.1) tstx = 1
        if (tstx.gt.boxsize) tstx = boxsize
        tsty = max(1,ceny-i)
        if ((.not.connct(tstx,tsty)) .and. (slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx+1,tsty+1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx-1,tsty+1)
         if ((j.gt.1) .and. (j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx-1,tsty+1) .or.
     *         connct(tstx,tsty+1) .or. connct(tstx,tsty+1)
         endif
c check adjacent pixels
         if ((tstx.gt.1) .and. (connct(tstx,tsty))) then
          connct(tstx-1,tsty) = slopearry(tstx-1,tsty)
         endif
         if ((tstx.lt.boxsize) .and. (connct(tstx,tsty))) then
          connct(tstx+1,tsty) = slopearry(tstx+1,tsty)
         endif
        endif
c check left side
        tstx = max(1,cenx-i)
        tsty = ceny + j - 1 - i
        if (tsty.lt.1) tsty = 1
        if (tsty.gt.boxsize) tsty = boxsize
        if ((.not.connct(tstx,tsty)) .and. (slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx+1,tsty+1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx+1,tsty-1)
         if ((j.gt.1) .and. (j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx+1,tsty-1) .or.
     *         connct(tstx+1,tsty) .or. connct(tstx+1,tsty+1)
         endif
c check adjacent pixels
         if ((tsty.gt.1) .and. (connct(tstx,tsty))) then
          connct(tstx,tsty-1) = slopearry(tstx,tsty-1)
         endif
         if ((tsty.lt.boxsize) .and. (connct(tstx,tsty))) then
          connct(tstx,tsty+1) = slopearry(tstx,tsty+1)
         endif
        endif
c check right side
        tstx = min(boxsize,cenx+i)
        tsty = ceny + j - 1 - i
        if (tsty.lt.1) tsty = 1
        if (tsty.gt.boxsize) tsty = boxsize
        if ((.not.connct(tstx,tsty)) .and. (slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx-1,tsty+1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx-1,tsty-1)
         if ((j.gt.1) .and. (j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx-1,tsty-1) .or.
     *         connct(tstx-1,tsty) .or. connct(tstx-1,tsty+1)
         endif
c check adjacent pixels
         if ((tsty.gt.1) .and. (connct(tstx,tsty))) then
          connct(tstx,tsty-1) = slopearry(tstx,tsty-1)
         endif
         if ((tsty.lt.boxsize) .and. (connct(tstx,tsty))) then
          connct(tstx,tsty+1) = slopearry(tstx,tsty+1)
         endif
        endif
       enddo
      enddo
cc
cc for troubleshooting - write out contents of connct in pseudo-
c  'asciiart' type format.
c
c        do i = mmx,mmn,-1
c         do j = lmn,lmx
c          if (connct(j-lmn+1,i-mmn+1)) then
c           write(*,'("*",$)')
c          else
c           write(*,'(" ",$)')
c          end if
c         end do
c         write(*,'("")')
c        end do
c
c Choose only those pixels within a *circle* of radius 'boxsize/2' which
c are also fainter than all the pixels between themselves and the
c brightest pixel and which aren't blanked.  Store pixel values of
c object and surrounding region in linear array, data, and fill out the
c x,y, coordinate.
c
      i = 0
      xt = 0
      yt = 0
      nfdrused = 0
      do mm = mmn, mmx
       do ll = lmx,lmn,-1
        incirc = ((ll-xpos)**2 + (mm-ypos)**2).lt.(0.25*boxsize**2)
        fainter = connct(ll-lmn+1,mm-mmn+1)
        if (incirc .and. fainter .and. (nimage(ll,mm).gt.0)) then
         i = i + 1
         if (image2(ll,mm).le.pcut) nfdrused = nfdrused + 1
         xpixused(i) = ll
         ypixused(i) = mm
         data(i) = image(ll,mm) - meanimg(ll,mm)
c
c convert ll,mm from binned subimage pixels to full image pixels
c
         wa(1) = dble(ll)
         wa(2) = dble(mm)
         call ppconcg(2, blc(1), bin(1), wa(1))
         call ppconcg(2, blc(2), bin(2), wa(2))
         x(i) = real(wa(1))
         y(i) = real(wa(2))
         xt = xt + x(i)
         yt = yt + y(i)
        endif
       enddo
      enddo
      ndata = i
      m = ndata
      xoff = xt / real(ndata)
      yoff = yt / real(ndata)
c
      end

c
      subroutine slopey(xx,yy,xc,yc,nx,ny,image,slpdwn)
c-----------------------------------------------------------------------
c this subroutine checks that a given pixel is less than all the pixels
c between it and the central (brightest) pixel.
c
c-----------------------------------------------------------------------
      integer xx,yy,xc,yc,nx,ny,closerx,closery
      integer furtherx,furthery,pp,qq,stepl,stepm
      real image(nx,ny)
      logical slpdwn
c-----------------------------------------------------------------------
      if (xx.gt.xc) then
       closerx = xx-1
       furtherx = xx+1
       stepl = -1
      endif
      if (xx.lt.xc) then
       closerx = xx+1
       furtherx = xx-1
       stepl = 1
      endif
      if (xx.eq.xc) then
       closerx = xx
       furtherx = xx
       stepl = 0
      endif
      if (yy.gt.yc) then
       closery = yy-1
       furthery = yy+1
       stepm = -1
      endif
      if (yy.lt.yc) then
       closery = yy+1
       furthery = yy-1
       stepm = 1
      endif
      if (yy.eq.yc) then
       closery = yy
       furthery = yy
       stepm = 0
      endif
c
c Checking to see if the closer/further pixels lie outside the bounds
c of the image, and fixing them if so.
c
      if (closerx.gt.nx)  closerx = nx
      if (closery.gt.ny)  closery = ny
      if (closerx.lt.1)   closerx = 1
      if (closery.lt.1)   closery = 1
      if (furtherx.gt.nx) furtherx = nx
      if (furthery.gt.ny) furthery = ny
      if (furtherx.lt.1)  furtherx = 1
      if (furthery.lt.1)  furthery = 1
      slpdwn = (image(xx,yy).le.image(closerx,closery)) .and.
     *          (image(xx,yy).ge.image(furtherx,furthery))
      if ((yy.ne.yc) .and. (xx.ne.xc) .and. slpdwn) then
       do qq = closery,yc,stepm
        do pp = closerx,xc,stepl
         slpdwn = slpdwn .and. (image(xx,yy).le.image(pp,qq))
        enddo
       enddo
      endif
c
      end
c
c
      subroutine PackVar(var,nvar,MAXVAR)
c-----------------------------------------------------------------------
c
c  Store all the things that we need to vary.
c
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer nvar,MAXVAR
      real var(MAXVAR)
      integer j,ncurr
      real tmp(6)
c-----------------------------------------------------------------------
      nvar = 0

      ncurr = 0
      if (vflux) then
        ncurr = ncurr + 1
        tmp(ncurr) = flux
      endif
      if (vl0) then
        ncurr = ncurr + 1
        tmp(ncurr) = l0 - xoff
      endif
      if (vm0) then
        ncurr = ncurr + 1
        tmp(ncurr) = m0 - yoff
      endif
      if (vfwhm1) then
        ncurr = ncurr + 1
        tmp(ncurr) = fwhm1
      endif
      if (vfwhm2) then
        ncurr = ncurr + 1
        tmp(ncurr) = fwhm2
      endif
      if (vpa) then
        ncurr = ncurr + 1
        tmp(ncurr) = pa
      endif
c
c  Copy the estimates to the variables.
c
      if (nvar+ncurr.gt.MAXVAR) call bug('f','Too many free parameters')
      do j = 1, ncurr
        nvar = nvar + 1
        var(nvar) = tmp(j)
      enddo
c
      end
c
c
      subroutine UpackCov(covar,nvar)
c-----------------------------------------------------------------------
c
c  Unpack the covariance matrix.
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer n, nvar
      real    covar(nvar,nvar)
c-----------------------------------------------------------------------
      n = 0

      if (vflux) then
        n = n + 1
        sflux = covar(n,n)
      endif

      if (vl0) then
        n = n + 1
        sl0 = covar(n,n)
      endif

      if (vm0) then
        n = n + 1
        sm0 = covar(n,n)
      endif

      if (vfwhm1) then
        n = n + 1
        sfwhm1 = covar(n,n)
      endif

      if (vfwhm2) then
        n = n + 1
        sfwhm2 = covar(n,n)
      endif

      if (vpa) then
        n = n + 1
        spa = covar(n,n)
      endif

      if (n.ne.nvar) call bug('f','Inconsistency in UPackCov')

      end


      subroutine UPackVar(var,nvar)
c-----------------------------------------------------------------------
c
c  Unpack all the things that we need to vary.
c
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer n, nvar
      real    var(nvar)
c-----------------------------------------------------------------------
      n = 0

      if (vflux) then
        n = n + 1
        flux = var(n)
      endif

      if (vl0) then
        n = n + 1
        l0 = var(n) + xoff
      endif

      if (vm0) then
        n = n + 1
        m0 = var(n) + yoff
      endif

      if (vfwhm1) then
        n = n + 1
        fwhm1 = var(n)
      endif

      if (vfwhm2) then
        n = n + 1
        fwhm2 = var(n)
      endif

      if (vpa) then
        n = n + 1
        pa = var(n)
      endif

      if (n.ne.nvar) call bug('f','Inconsistency in UPackVar')

      end


      subroutine FUNCTION(m,nvar,var,fvec,iflag)
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      include 'sfind.h'

      integer m,nvar,iflag
      real var(nvar)
      real fvec(m)
      integer i
c-----------------------------------------------------------------------
      if (m.ne.ndata) call bug('f','Inconsistency in FUNCTION')
c
c  Unpack the things that we are solving for.
c
      call UpackVar(var,nvar)
c
c  Evaluate the model. Assume it's a 2-D gaussian, unless otherwise
c specified.
c
      if (gdim.eq.1) then
        call Eval1(xd,fvec,m)
      else
        call Eval(x,y,fvec,m)
      endif
c
c  Compute the residuals now.
c
      do i = 1, m
        if (gdim.eq.1) then
          fvec(i) = data2(i) - fvec(i)
        else
          fvec(i) = data(i) - fvec(i)
        endif
      enddo

      end
c
c
      subroutine Eval1(x0,Model,n)
c-----------------------------------------------------------------------
c
c  Evaluate the current model at some pixels.
c
c  Input:
c    n            Number of points.
c    x0         Pixel coordinates at which to evaluate the model.
c  Output:
c    model      The evaluated model.
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer i, n
      real model(n), t, x0(n), xx, xscal
c-----------------------------------------------------------------------
c     Gaussian component.
      xscal = 4.0*log(2.0)/(m0*m0)

c     Loop over the various model types.
      do i = 1, n
        model(i) = 0
        xx = x0(i) - l0
        t  = xscal*(xx*xx)
        if (t.lt.70) model(i) = model(i) + flux * exp(-t)
      enddo

      end


      subroutine Eval(x0,y0,Model,n)
c-----------------------------------------------------------------------
c
c  Evaluate the current model at some pixels.
c
c  Input:
c    n            Number of points.
c    x0,y0      Pixel coordinates at which to evaluate the model.
c  Output:
c    model      The evaluated model.
c-----------------------------------------------------------------------
      include 'sfind.h'
      integer i, n
      real    cospa, model(n), sinpa, t, x0(n), xp, xscal, xx, y0(n),
     *        yp, yscal, yy
c-----------------------------------------------------------------------
c     Gaussian component.
      cospa = cos(pa)
      sinpa = sin(pa)
      xscal = 4.0*log(2.0)/(fwhm2*fwhm2)
      yscal = 4.0*log(2.0)/(fwhm1*fwhm1)

c     Loop over the various model types.
      do i = 1, n
        xx = x0(i) - l0
        yy = y0(i) - m0
        yp =  yy*cospa + xx*sinpa
        xp = -yy*sinpa + xx*cospa
        t = xscal*(xp*xp) + yscal*(yp*yp)

        if (t.lt.70) then
          model(i) = flux * exp(-t)
        else
          model(i) = 0.0
        endif
      enddo

      end


      subroutine gaucvt(lIn, k, bvol, bvolp, xposerr, yposerr, pkfl,
     *  pkflerr, intfl, amaj, amin, posa, awpos)
c-----------------------------------------------------------------------
c  Convert the source component parameters from pixel coordinates to
c  astronomical (angular) measures.
c
c  Input:
c    lIn            Handle of the coordinate system.
c
c  Output:
c    bvol
c    bvolp
c    xposerr        xposition error.
c    yposerr        yposition error.
c    pkfl, pkflerr  peak flux density of source, and its error.
c    intfl          integrated flux density of the source
c    amaj, amin     major and minor fwhm of source (arcsec)
c    posa           position angle of source (degrees E of N)
c    awpos          x and y positions in abs. world coords (radians)
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'sfind.h'

      integer   lIn, k
      real      amaj, amin, bmaj, bmin, bpa, bvol, bvolp, dx, dy, intfl,
     *          pkfl, pkflerr, posa, sfac, tmp, xposerr, yposerr
      double precision appos(2), awpos(2), cdelt(2), crpix(2), crval(2),
     *          x1(3)
      character ctype(2)*16
c-----------------------------------------------------------------------
c     Convert the gaussian parameters to world coordinates.
      x1(1) = l0
      x1(2) = m0
      x1(3) = k
      call coGauCvt(lIn, 'ap/ap/ap', x1,
     *  'p', fwhm1, fwhm2, pa, 'w', bmaj, bmin, bpa)

c     Convert the uncertainties.
      sfwhm1 = sfwhm1 * bmaj / fwhm1
      sfwhm2 = sfwhm2 * bmin / fwhm2
      if ((spa+sl0+sm0).gt.0.0) then
        call coLin(lIn,'ap/ap/ap',x1,2,ctype,crpix,crval,cdelt)
        dx = abs(cdelt(1))
        dy = abs(cdelt(2))
        sl0 = sl0 * dx
        sm0 = sm0 * dy
        spa = spa / ((dy/dx)*cos(pa)**2 + (dx/dy)*sin(pa)**2)
      endif

      fwhm1 = bmaj
      fwhm2 = bmin
      pa    = bpa

      if (bvolp.gt.0) then
        sfac = sqrt(bvolp)
      else
        sfac = 1
      endif

      pkfl = 1000.0*flux
      pkflerr = 1000.0*sfac*sflux
      if (bvol.gt.0.0) then
        intfl = 1000.0*flux * abs(fwhm1*fwhm2) * PI_4 / (log(2.0)*bvol)
      else
        intfl = 0.0
      endif

      appos(1) = l0
      appos(2) = m0
      call coCvt(lIn, 'ap/ap', appos, 'aw/aw', awpos)
      xposerr = sl0 * sfac * R2AS
      yposerr = sm0 * sfac * R2AS

c     Convert gaussian parameters to arcsec.
      amaj = abs(fwhm1) * R2AS
      amin = abs(fwhm2) * R2AS
      posa = pa * R2D

      if (amaj.lt.amin) then
        tmp  = amaj
        amaj = amin
        amin = tmp
        posa = posa + 90.0
      endif

      posa = mod(posa, 180.0)
      if (posa.lt.-90.0) then
        posa = posa + 180.0
      else if (posa.gt.90.0) then
        posa = posa - 180.0
      endif

      end


      subroutine BeamPar(lIn,k,bvol,bvolp,bmaj,bmin,bpa,bmajp,bminp,
     *                   bpap,nobeam)
c-----------------------------------------------------------------------
c  Get things dealing with units.
c
c  Input:
c    lIn      Handle of the input dataset.
c    k            Plane of interest.
c  Output:
c    bvol     Beam volume, in radians**2.  Set to zero if this cannot
c             be determined.
c    bvolp    Beam volume in pixels.
c    bmaj,bmin,bpa
c             Beam major, minor axes and position angle.
c    bmajp,bminp,bpap
c             Beam major, minor axes and position angle in pixel coords.
c-----------------------------------------------------------------------
      integer lIn,k
      real bvol,bvolp,bmaj,bmin,bpa
cc
      include 'mirconst.h'
      character bunit*16,ctype(2)*16
      real bmajp,bminp,bpap
      double precision crpix(2),crval(2),cdelt(2),x(3)
      logical nobeam
c-----------------------------------------------------------------------
      nobeam = .false.
c
c  Convert the beam to radians for the current plane.
c
      call rdhdr(lIn,'bmaj',bmaj,0.0)
      call rdhdr(lIn,'bmin',bmin,0.0)
      call rdhdr(lIn,'bpa',bpa,0.0)
      bpa =bpa * R2D
        if ((bmaj.eq.0.0) .or. (bmin.eq.0.0)) then
          call bug('w','Beam not detected in map -')
          call bug('w','Using arbitrary value of 5 pixels.')
          nobeam = .true.
          bvol = 0.0
          bvolp = 0.0
          goto 1111
        endif
c
      if (bmaj*bmin.gt.0.0) then
        x(1) = 0
        x(2) = 0
        x(3) = 0
        call coGauCvt(lIn,'op/op/op',x,'w',bmaj,bmin,bpa,
     *                'p',bmajp,bminp,bpap)
        x(3) = k
        call coGauCvt(lIn,'op/op/ap',x,'p',bmajp,bminp,bpap,
     *                'w',bmaj,bmin,bpa)
      endif
c
c  Determine the beam value, in radians**2
c
      call rdhda(lIn,'bunit',bunit,' ')
      call ucase(bunit)
      if (index(bunit,'/PIXEL').ne.0) then
        x(1) = 0
        x(2) = 0
        x(3) = k
        call coLin(lIn,'op/op/ap',x,2,ctype,crpix,crval,cdelt)
        bvol = abs(cdelt(1)*cdelt(2))
        bvolp = 1
      else if (index(bunit,'/BEAM').ne.0 .and. bmaj*bmin.gt.0.0) then
        bvol  = bmaj  * bmin  * PI_4 / log(2.0)
        bvolp = bmajp * bminp * PI_4 / log(2.0)
      else
        bvol = 0
        bvolp = 0
      endif
c
1111    continue
      end


      subroutine fdr(nx,ny,l,m,boxsize,image,nimage,alpha,xrms,
     *           bmajp,bminp,pcut,lin,plist,image2,meanimg,sgimg,
     *           fdrimg,sigmaimg,rmsimg,normimg,auto,blc,bin,
     *           maxx,maxy,nfdrpix)
c-----------------------------------------------------------------------
c Performs FDR statistical analysis of pixels in region of source.
c 1. Assigns a P-value to each pixel, based on its intensity
c    (given by P=1-gaussianpdf(image(p,q)) for pixel p,q).
c 2. Orders the list of P-values.
c 3. Using alpha, finds the P-value corresponding to the cutoff, which
c    is the point at which a line of slope alpha/log(Npix) intercepts
c    the ordered list of P-values.
c 4. sets pcut accordingly.
c
c  Input:
c    nx,ny      Size of image array
c    l,m        Bright pixel position
c    boxsize    Side length of box within which the background rms is
c               calculated
c    image      Image matrix with pixel values
c    nimage     blanking image
c    alpha      percentage of 'incorrect' pixels to allow to be included
c    xrms       number of sigma to use for creating the sigma image
c    bmajp,bminp  beam parameters
c    pcut       cutoff p-value from FDR
c    lin        handle of input image
c    plist      1-D list of p-values for the image pixels
c    image2     image of (briefly) sigma values, then p-values for the
c               image pixels
c    meanimg    image of background mean values
c    sgimg      image of background sigma values
c    fdrimg     true to output FDR-image
c    sigmaimg   true to output sigma-image
c    rmsimg     true to output rms-image
c    normimg    true to output normalised image
c    auto       true if not being run interactively
c    blc,bin    subimage blc and binning information
c    maxx,maxy  size of full image (not subregion, which is nx,ny)
c    nfdrpix    number of pixels detected by FDR
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'

      integer ii,jj,lmn,lmx,mmn,mmx,nx,ny,l,m,npix
      integer nn,lIn,maxx,maxy
      parameter (nn=20000)
      real sigma, xrms, alpha
      real image(nx,ny),pvalue,pline
      real plist(nx*ny),pcut
      real bmajp,bminp,bareap,ee,fdrdenom
      real errfun,base0
      integer boxsize, blc(2),bin(2)
      integer nimage(nx,ny)
c
      integer lOut,lOut1,lOut2,lOut3,lOut4,axes(2),nfdrpix,nblanks
      integer iii,jjj,ipim,ip2im
      real rrow(maxdim),image2(nx,ny),meanimg(nx,ny),sgimg(nx,ny)
      double precision wa(2)
      real fluxctoff,sigctoff
c
      logical ok,gotit,mskexst,hdprsnt,msk(maxdim)
      logical fdrimg, sigmaimg, rmsimg, normimg, auto
      character line*160
c-----------------------------------------------------------------------
      call output(' ')
      call output('*************************')
      call output('Beginning FDR analysis...')
      call output('*************************')

c     Does the input image have a mask?
      mskexst = hdprsnt(lIn,'mask')

c     Open output 'normalised' and 'segmentation image' datasets if
c     necessary.
      axes(1) = maxx
      axes(2) = maxy
      if (fdrimg)   call xyopen(lOut1,'sfind.fdr', 'new',2,axes)
      if (sigmaimg) call xyopen(lOut2,'sfind.sig', 'new',2,axes)
      if (normimg)  call xyopen(lOut3,'sfind.norm','new',2,axes)
      if (rmsimg)   call xyopen(lOut4,'sfind.rms', 'new',2,axes)

      do iii = 1, maxx
        rrow(iii) = 0.0
      enddo

c     Create headers for the output datasets if necessary.
      do ii = 1, 4
        if (ii.eq.1 .and. fdrimg) then
          lOut = lOut1
        else if (ii.eq.2 .and. sigmaimg) then
          lOut = lOut2
        else if (ii.eq.3 .and. normimg) then
          lOut = lOut3
        else if (ii.eq.4 .and. rmsimg) then
          lOut = lOut4
        else
          lOut = -1
        endif

        if (lOut.gt.0) then
c         Copy keywords and history.
          call headcp(lIn, lOut, 0, 0, 0, 0)

c         Copy the mask.
          if (mskexst) then
            do jj = 1, axes(2)
              call xyflgrd(lIn,  jj, msk)
              call xyflgwr(lOut, jj, msk)
            enddo
          endif

c         Fill the new images with zeroes to start.
          do jjj = 1, maxy
            call xywrite(lOut, jjj, rrow)
          enddo
        endif
      enddo

      if (normimg) call wrhda(lOut3,'bunit','SIGMA')

c     Catch possible problem of boxsize (rmsbox input) is bigger than
c     image
      boxsize = min(boxsize,nx,ny)

c     Allocate memory for mask and image2 array in basecal.
      call memalloc(ipim,(boxsize+1)*(boxsize+1),'l')
      call memalloc(ip2im,(boxsize+1)*(boxsize+1),'r')

c     Normalise the image: determine the mean and sigma in boxes of
c     size boxsize, subtract the mean and divide by sigma.  This makes
c     the FDR stuff work for images where sigma varies significantly
c     over the image.
      do ii = 1, nint(float(nx)/float(boxsize))+1
        do jj = 1, nint(float(ny)/float(boxsize))+1
          l = boxsize/2 + (ii-1)*boxsize
          m = boxsize/2 + (jj-1)*boxsize
          if ((l.le.(nx+boxsize/2)) .and. (m.le.(ny+boxsize/2))) then
            call basecal(nx, ny, l, m, base0, sigma, boxsize, image,
     *              nimage, 1.5*bmajp, ok, meml(ipim),memr(ip2im), auto)
            if (ok) then
              lmn = max(0,(l-boxsize/2)) + 1
              lmx = min(nx,(lmn+boxsize-1))
              mmn = max(0,(m-boxsize/2)) + 1
              mmx = min(ny,(mmn+boxsize-1))
              do iii = lmn, lmx
                do jjj = mmn, mmx
                  if (sigma.ne.0) then
                    image2(iii,jjj) = (image(iii,jjj) - base0)/sigma
                    meanimg(iii,jjj) = base0
                    sgimg(iii,jjj) = sigma
                  endif
                enddo
              enddo
            endif
          endif
        enddo
      enddo

c     Free memory from basecal arrays.
      call memfree(ip2im,(boxsize+1)*(boxsize+1),'r')
      call memfree(ipim,(boxsize+1)*(boxsize+1),'l')

c     Make normalised image from image2 if required.
      if (normimg) then
        do jjj = 1, ny
          do iii = 1, nx
c           cvt iii from binned subimage pixels to full image pixels.
            wa(1) = dble(iii)
            call ppconcg(2, blc(1), bin(1), wa(1))
            rrow(nint(wa(1))) = image2(iii,jjj)
          enddo

c         cvt jjj from binned subimage pixels to full image pixels.
          wa(2) = dble(jjj)
          call ppconcg(2, blc(2), bin(2), wa(2))
          call xywrite(lOut3,nint(wa(2)),rrow)
        enddo
        call xyclose(lOut3)
      endif

c     Make rms image for calculation of weighting corrections.
      if (rmsimg) then
        nfdrpix = 0
        nblanks = 0
        do m = 1, ny
          do l = 1, nx
c           cvt l from binned subimage pixels to full image pixels.
            wa(1) = dble(l)
            call ppconcg(2, blc(1), bin(1), wa(1))
            if (nimage(l,m).gt.0) then
              rrow(nint(wa(1))) = sgimg(l,m)
            else
              rrow(nint(wa(1))) = 0.0
            endif
          enddo

c         Have written the m'th row, now put it in the segmentation
c         image cvt m from binned subimage pixels to full image pixels.
          wa(2) = dble(m)
          call ppconcg(2, blc(2), bin(2), wa(2))
          call xywrite(lOut4,nint(wa(2)),rrow)
        enddo

c       Done writing to lOut4, close it.
        call xyclose(lOut4)
      endif


c     Make sigma-cut image for comparison with the upcoming FDR-based
c     segmentation image - note, after above normalisation, sigma = 1.
      if (sigmaimg) then
        nfdrpix = 0
        nblanks = 0
        do m = 1, ny
          do l = 1, nx
c           cvt l from binned subimage pixels to full image pixels
            wa(1) = dble(l)
            call ppconcg(2, blc(1), bin(1), wa(1))
            if (nimage(l,m).gt.0) then
              if (image2(l,m).lt.xrms) then
                rrow(nint(wa(1))) = 0.0
              else
                nfdrpix = nfdrpix + 1
                rrow(nint(wa(1))) = 100.0
              endif
            else
              rrow(nint(wa(1))) = 0.0
              nblanks = nblanks + 1
            endif
          enddo
c         Have written the m'th row, now put it in the segmentation
c         image cvt m from binned subimage pixels to full image pixels.
          wa(2) = dble(m)
          call ppconcg(2, blc(2), bin(2), wa(2))
          call xywrite(lOut2,nint(wa(2)),rrow)
        enddo

c       Done writing to lOut2, close it.
        call xyclose(lOut2)
        write(line,'("Of a total of ",i7," non-blanked pixels,")')
     *         nx*ny - nblanks
        call output(line)
        write(line,'(f5.1,"-sigma cut gives ",i7," pixels.")')
     *                xrms,nfdrpix
        call output(line)
      endif


c     Now (finally) to the actual FDR bit...
      lmn = 1
      lmx = nx
      mmn = 1
      mmx = ny

c     Calculate area covered by beam, in pixels.
      bareap = bmajp *bminp * PI_4 / log(2.0)

c     ee is constant e.
      ee = exp(1.0)

      npix = 0

c     Looping over pixels assigning pvalues - the actual FDR bit.
      do ii = lmn, lmx
        do jj = mmn, mmx
          if (nimage(ii,jj).ne.0) then
c           Note: Gaussian Probability Distribution Function (GPDF) is
c           related to the error function erf(x) by
c           GPDF(x) = 0.5(1+erf(x/sqrt(2)))
            pvalue = 0.5*(1+errfun(image2(ii,jj)/sqrt(2.0)))
            image2(ii,jj) = 1.0 - pvalue
            npix = npix + 1
            plist(npix) = 1.0 - pvalue
          endif
        enddo
      enddo

c     Sort plist into order.
      call sortr(plist,npix)

c     fdrdenom is the denominator of the slope (alpha/fdrdenom) for the
c     fdr line defined this way, it runs from 1 to sum_i=1^N (1/i), for
c     bareap=1 (uncorrelated) to N (fully correlated).
      if (bareap.lt.1.0) then
        fdrdenom = 1.0
      else if (bareap.gt.float(npix)) then
        fdrdenom = 0
        do ii = 1, npix
          fdrdenom = fdrdenom + 1/float(ii)
        enddo
      else
        fdrdenom = 0
        do ii = 1, int(bareap)
          fdrdenom = fdrdenom + 1/float(ii)
        enddo
      endif

c     Find crossing point.
      gotit = .false.
      pcut = 0.0
      do ii = npix,1,-1
        pline = (alpha/(100.0*fdrdenom))
     *              *(float(ii)/float(npix))
c       pline = (alpha/(100.*log(float(npix))))
c       pline = (alpha/100.)
        if ((pline.ge.plist(ii)) .and. (.not.gotit)) then
          pcut = pline
          gotit = .true.
        endif
      enddo

c     Loop over all pixels to test whether image2(l,m) < P_cut.
c     If image2(l,m) > pcut, then pixel is most likely background,
c     and if not, store it in the 'segmentation image' if required
      nfdrpix = 0
      fluxctoff = 1e9
      sigctoff = 1e9
      do m = 1, ny
        do l = 1, nx
c         cvt l from binned subimage pixels to full image pixels
          wa(1) = dble(l)
          call ppconcg(2, blc(1), bin(1), wa(1))
          if (nimage(l,m).gt.0) then
            if (image2(l,m).gt.pcut) then
              rrow(nint(wa(1))) = 0.0
            else
              fluxctoff=min(fluxctoff,image(l,m))
              sigctoff =min(sigctoff,(image(l,m)-meanimg(l,m))/
     *                        sgimg(l,m))
              nfdrpix  = nfdrpix + 1
              rrow(nint(wa(1))) = 100.0
            endif
          else
            rrow(nint(wa(1))) = 0.0
          endif
        enddo

c       have written the m'th row, now put it in the segmentation image.
        if (fdrimg) then
c         cvt m from binned subimage pixels to full image pixels.
          wa(2) = dble(m)
          call ppconcg(2, blc(2), bin(2), wa(2))
          call xywrite(lOut1,nint(wa(2)),rrow)
        endif
      enddo

c     Done writing to lOut1, close it if it was open.
      if (fdrimg) call xyclose(lOut1)

      if (.not.sigmaimg) then
        write(line,'("Of a total of ",i7," non-blanked pixels,")')
     *        nx*ny - nblanks
      endif
      call output(' ')
      write(line,'(a,f16.10,a)')
     *  'FDR selected a p-value threshold of ',pcut,'.'
      call output(line)
      write(line,'(a,f7.2,a)')
     *  'This corresponds to a threshold of ', sigctoff, ' sigma,'
      call output(line)
      write(line,'(a,f16.10,a)')
     *  'which means a minimum flux threshold of ',fluxctoff,' Jy.'
      call output(line)
      write(line,'(a)')
     *  '(If the noise is constant over the original image,'
      call output(line)
      write(line,'(a)')
     *  'this is the threshold over the whole image.)'
      call output(line)
      write(line,'("FDR detected ",i7," pixels.")') nfdrpix
      call output(line)

      return
      end


      subroutine fdrfit(nx,ny,image,nimage,bvol,bpap,bvolp,bmajp,bminp,
     *  pcut,image2,lin,krng,blc,bin,negative,pbcor,llog,kvannot,
     *  fdrpeak,allpix,psfsize,meanimg,sgimg)
c-----------------------------------------------------------------------
c
c  Input:
c    nx,ny      Size of image array
c    image      Image matrix with pixel values
c    nimage     blanking image
c    bvol,bvolp,bmajp,bminp   beam parameters
c    pcut       cutoff p-value from FDR
c    image2     image of (briefly) sigma values, then p-values for the
c               image pixels
c    lin        handle of input image
c    krng       plane of input image
c    blc,bin    binning info for input image
c    negative   true to look for negative sources
c    pbcor      true to make correction for primary beam shape
c    llog       handle for log file
c    kvannot    true to make annotation file output
c    fdrpeak    true to use all pixels in source fits
c    allpix     true to use all pixels, not just monotonically
c               decreasing ones
c    psfsize    true to restrict minimum source size to that of the psf
c    nfdrpix    number of pixels detected by FDR
c    meanimg    image of background mean values
c    sgimg      image of background sigma values
c-----------------------------------------------------------------------
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'
      include 'sfind.h'
c
      integer ii,jj,kk,nx,ny,l,m
      integer lin,llog,lann,krng(2),len1
      real image(nx,ny),pcut
      real bvol,bpap,bvolp,bmajp,bminp
      integer boxsize, dumcount
      integer nimage(nx,ny),bin(2),blc(2),imin,jmin,imax,jmax
      logical fitok,negative,pbcor,kvannot,fdrpeak,allpix,psfsize
c
      integer iii,jjj,pp,off,sources,radeclen(2),iostat, usedpixels
      real image2(nx,ny),meanimg(nx,ny),sgimg(nx,ny)
      real xpos, xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj
      real amin, posa, rms, gain, mvlrms
      double precision posns(2),wa(2)
c
      logical blnkannulus,atpeak
      character line*160,typei(2)*6,typeo(2)*6,radec(2)*80
c-----------------------------------------------------------------------
      call output(' ')
      call output('********************************')
      call output('Beginning source measurements...')
      call output('********************************')
      sources = 0
      dumcount = 0
      if (kvannot) then
        call txtopen(lann, 'sfind.ann', 'append', iostat)
        if (iostat.ne.0)
     *    call bug('f', 'Error opening text file "sfind.ann"')
        write(line,'("color green")')
        call txtwrite(lann, line, len1(line), iostat)
      endif
c
      usedpixels = 0
      do l = 1, nx
       do m = 1, ny
        if (nimage(l,m).gt.0) then
         if (image2(l,m).le.pcut) then
c climb hill to find local peak of source
          ii = l
          jj = m
          atpeak = .false.
230       imin = max(1,ii-1)
          imax = min(nx,ii+1)
          jmin = max(1,jj-1)
          jmax = min(ny,jj+1)
          if ((image(imin,jmin).le.image(ii,jj)) .and.
     *       (image(imin,jj).le.image(ii,jj)) .and.
     *       (image(imin,jmax).le.image(ii,jj)) .and.
     *       (image(ii,jmin).le.image(ii,jj)) .and.
     *       (image(ii,jmax).le.image(ii,jj)) .and.
     *       (image(imax,jmin).le.image(ii,jj)) .and.
     *       (image(imax,jj).le.image(ii,jj)) .and.
     *       (image(imax,jmax).le.image(ii,jj))) then
           atpeak = .true.
          else
           do kk = imin, imax
            do pp = jmin, jmax
             if ((kk.ne.ii) .or. (pp.ne.jj)) then
              if (image(kk,pp).gt.image(ii,jj)) then
               ii = kk
               jj = pp
              endif
             endif
            enddo
           enddo
          endif
          if (.not.atpeak) goto 230
c test increasing annuli around source to find size of region to
c be sent to fitting routine
          off = 1
233       blnkannulus = .true.
          do iii = ii-off, ii+off
           if (((jj-off).ge.1) .and. (iii.ge.1) .and. (iii.le.nx)) then
            if (nimage(iii,jj-off).gt.0)
     *      blnkannulus = blnkannulus .and. (image2(iii,jj-off).gt.pcut)
           endif
           if (((jj+off).le.ny) .and. (iii.ge.1) .and. (iii.le.nx)) then
            if (nimage(iii,jj+off).gt.0)
     *      blnkannulus = blnkannulus .and. (image2(iii,jj+off).gt.pcut)
           endif
          enddo
          do jjj = jj-off, jj+off
           if (((ii-off).ge.1) .and. (jjj.ge.1) .and. (jjj.le.ny)) then
            if (nimage(ii-off,jjj).gt.0)
     *      blnkannulus = blnkannulus .and. (image2(ii-off,jjj).gt.pcut)
           endif
           if (((ii+off).le.nx) .and. (jjj.ge.1) .and. (jjj.le.ny)) then
            if (nimage(ii+off,jjj).gt.0)
     *      blnkannulus = blnkannulus .and. (image2(ii+off,jjj).gt.pcut)
           endif
          enddo
          if (.not.blnkannulus) then
           off = off + 1
c don't go overboard
           if (off.gt.min(nx,ny)/4) goto 60
           goto 233
          endif
          boxsize = 2*off + 1
c convert binned, subimage pixels to unbinned full image pixels
          wa(1) = dble(ii)
          wa(2) = dble(jj)
          call ppconcg(2, blc(1), bin(1), wa(1))
          call ppconcg(2, blc(2), bin(2), wa(2))
c set xpos,ypos to initial peak pixel position
          xpos = ii
          ypos = jj
c
c Ok, have found the location and size of region to fit, now fit it
c
          call fitting(lIn, krng, nx, ny, image, rms, xpos, xposerr,
     *     ypos, yposerr, pkfl, pkflerr, intfl, amaj, amin, posa,
     *     posns, blc, bin, ii, jj, bvol, bpap, bvolp,
     *     bmajp, bminp, nimage, boxsize, image2, pcut, fitok,
     *     usedpixels, meanimg, sgimg, fdrpeak, allpix, psfsize,
     *     dumcount)
          if (.not.fitok) goto 60
c write out annotation file line if necessary
          if (kvannot) then
            write(line,49) posns(1)*R2D, posns(2)*R2D,
     *         amaj/3600.0, amin/3600.0, 90.0-posa
49          format("ellipse ",f16.10,f16.10,f16.10,f16.10,f8.2)
            call txtwrite(lann, line, len1(line), iostat)
          endif
c
c Convert location (peak or fitted) to formatted coordinate string
c
          typei(1) = 'hms'
          typei(2) = 'dms'
          typeo(1) = 'hms'
          typeo(2) = 'dms'
          call w2wfco(lin, 2, typei, ' ', posns,  typeo, ' ',
     *                .true., radec, radeclen)
c
c if 'pbcor' selected, correct the flux densities (peak and integrated)
c by the gain (primary beam attenuation) at the position of the source
c
          if (pbcor) then
           call mosVal(lin,'aw/aw',posns,gain,mvlrms)
           pkfl = pkfl/gain
           intfl = intfl/gain
cc should we correct the sigma as well?
c           sgimg(ii,jj) = sgimg(ii,jj)/gain
          endif
c
c Define output lines
c
          if (negative) then
           write(line,50) xposerr,yposerr,-pkfl,pkflerr,-intfl,
     *        amaj,amin,posa,-sgimg(ii,jj)*1000.0,-rms*1000.0
          else
           write(line,50) xposerr,yposerr,pkfl,pkflerr,intfl,
     *        amaj,amin,posa,sgimg(ii,jj)*1000.0,rms*1000.0
          endif
50        format(1x,f6.3,3x,f5.2,2x,f8.3,1x,f6.3,1x,f9.3,1x,
     *           3(f5.1,1x),f6.3,2x,f6.3)
          line = radec(1)(1:radeclen(1))//' '//
     *         radec(2)(1:radeclen(2))//' '//line
c
c write to log file, after appending 'Y' (as source confirmation, for
c consistency, since FDR mode is run in 'auto' mode, so all sources
c are defined to be real).
c
          line(len1(line)+1:len1(line)+6) = '     Y'
          call txtwrite(llog, line, len1(line), iostat)

cc undo change to sgimg just in case it affects something elsewhere
c          if (pbcor) then
c           sgimg(ii,jj) = sgimg(ii,jj)*gain
c          end if

c
c increment number of sources detected
c
          sources = sources + 1
c
60        continue
         endif
        endif
       enddo
      enddo
      if (kvannot) call txtclose(lann)
      call output(' ')
      write(line,'(a,i6,a)') 'Of the FDR pixels detected, a total of ',
     *     usedpixels,' were used in fitting sources.'
      call output(line)
      call output(' ')
      write(line,'("A total of ",i6," sources were detected.")') sources
      call output(line)

      end
