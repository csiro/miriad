      program sfind
c-----------------------------------------------------------------------
c= SFIND - Automatically or interactively find sources in images
c& nebk
c: plotting
c+
c       SFIND has been updated to incorporate a new statistically
c       robust method for detecting source pixels, called FDR (which
c       stands for "False Discovery Rate"), as an alternative to
c       simple sigma-clipping, which formed the basis of the original
c       implementation. Details of the FDR method can be found in
c       Hopkins et al 2001, (astro-ph/0110570) and references
c       therein. The original implementation of sfind has been
c       preserved, and can be applied by specifying the option "oldsfind".
c       In addition, when using SFIND in the new 'FDR' mode,
c       several new features are available. SFIND now provides the
c       option of outputing (1) a 'normalised' image (options=normimg)
c       created by subtracting a background and dividing by sigma
c       (the standard deviation); (2) a 'sigma' image (options=sigmaimg),
c       created by sigma-clipping the normalised image at a user-specified
c       sigma value; (3) an 'fdr' image (options=fdrimg) created similarly
c       to the sigma-image by clipping the normalised image at the FDR-
c       defined threshold.
c
c       In 'FDR' mode (the default), no interactive source detection
c       is possible, as is the case with the original version. Instead,
c       the detected sources are drawn from a distribution of pixels
c       with a robustly known chance of being falsely drawn from the
c       background, thus more reliably characterising the fraction of
c       expected false sources than is possible with a sigma-clipping
c       criterion.
c       The process of source detection and measurement is slightly
c       different in 'FDR' mode compared to the original SFIND
c       implementation (see below). In 'FDR' mode, the following steps
c       are performed:
c        1. The image is first normalised by estimating the background
c         (mean) and standard deviation (sigma) for the whole image in
c         uniformly distributed regions of size 'rmsbox' (a user input).
c         This is done by fitting a gaussian to the pixel histogram,
c         (as is done in the task 'imsad') - if the fit is poor, an
c         interative method is used instead. With these values known,
c         the image has the mean subtracted and is divided by sigma to
c         create a normalised image. This is the same as saying the
c         normalised image has a gaussian mean of 0 and sigma of 1.
c         The normalised image is output as a miriad image called
c         sfind.norm if 'options=normimg' is set. The rms noise measured
c         over the image can also be output as sfind.rms if
c         'options=rmsimg' is set.
c        2. From the normalised image a sigma-clipped image (called
c         sfind.sig) may be output (if 'options=sigmaimg'). This is
c         simply an image with pixel values set to 100 if the pixel
c         value in the normalised image is greater than the user
c         specified value of 'xrms,' or 0 otherwise.
c        3. The FDR method is implemented using the normalised image.
c         Each pixel is assigned a p-value, a probability that it was
c         drawn from the background, and a cutoff p-value is established
c         based on the percentage of false rejections (source pixels)
c         that the user specifies with the parameter 'alpha'. If
c         'options=fdrimg' is set, this cutoff p-value threshold is
c         used to create an 'fdr' image (called sfind.fdr) in the same
c         way as the sigma image above is created.
c        4. With the FDR cutoff threshold established, sources may now
c         be detected and measured. Each pixel with a p-value lying
c         *below* the cutoff p-value (i.e. a low chance of being drawn
c         from the background) may be part of a source. For each such
c         'FDR-detected' pixel, a hill-climbing routine finds a local
c         peak from adjacent FDR-selected pixels. This is then used as
c         the starting point for a routine which selects contiguous
c         monotonically decreasing adjacent pixels from the FDR-selected
c         ones, and to which a 2-D elliptical gaussian is fit in the
c         same way as the original SFIND (see below). The same parameters
c         are returned as in the original implementation, and the
c         logfile has the same format (see below).
c       Also, in FDR mode 'option=auto' from the original implementation
c       is assumed automatically, regardless of user input. This means
c       the inputs for 'type,' 'range,' 'device' etc are not relevant
c       and are ignored.
c
c       In the original implementation, SFIND displays an image via
c       a contour plot or a pixel map representation on a PGPLOT device.
c       The user is then provided with the opportunity to interactively
c       flag sources as real or not (indicated by a Y or N flag in a
c       log file).
c
c       Source positions are calculated by an algorithm which searches for
c       pixels brighter than the surrounding 24 pixels and then
c       bi-parabolically fitting positions and flux densities.
c       Once a source such as this is detected, SFIND checks to see whether
c       it is brighter than the user set multiple of the background rms.
c       If so, a 2D elliptical gaussian fit is performed (using the same
c       routine as IMFIT) and the source parameters are displayed on the
c       terminal (and written to a log file after user input to determine
c       a flag, Y or N, to attach). The source parameters are (in order):
c
c                 Quantity                        Notes
c              --------------                  -----------
c       Position                       RA and Dec. in standard miriad
c                                      hms,dms format
c       Formal errors in RA and Dec.   (arcsec; treat judiciously)
c       Peak flux density              (mJy)
c       Formal error in peak flux      in mJy (generally not a good
c       density                        estimate of the true error)
c       Integrated flux density        (mJy)
c       Major and minor axes and       (arcseconds for axes, degrees for PA)
c       position angle of source       Warning: these are not deconvolved
c                                      from the synthesized beam
c       Local background rms (sigma)   (mJy) calculated from a gaussian fit
c                                      to the pixel histogram, as per imsad
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
c       It is strongly suggested that pixel maps be used for source finding,
c       as contour plots may be deceiving. Default is "pixel".
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
c       size in pixels for the x and y axes to be applied to the selected
c       region.   If the binning size is not unity, it must be equal
c       to the increment.  For example, to bin up the image by 4 pixels in 
c       the x direction and to pick out every third pixel in the y 
c       direction, set XYBIN=4,4,3,1
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
c       ization), and "sqr" (square root).  See also OPTIONS=FIDDLE which
c       is in addition to the selections here.
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
c       In the original implementation (options=oldsfind), it is the
c       size (in pixels) of a box around each source within which the
c       background rms is calculated.
c       Default is 20 pixels.
c@ alpha
c       This (real) number is the *percentage* of false *pixels* which can
c       be accepted when applying the FDR method. Alpha determines the
c       threshold set in selecting pixels which belong to sources (as an
c       alternative to a simple sigma-cut), prior to the source-fitting
c       and measuring step.
c       Must be a positive number. Default is 2.0 percent.
c       Ignored if options=oldsfind.
c@ xrms
c       In 'FDR' mode (the default) this parameter defines the
c       sigma-cutoff for the creation of the image sfind.sig if
c       'options=sigmaimg' is set. If not, it is ignored. It has no
c       role in the detection or measurement of sources. No default.
c       In the original implementation (options=oldsfind), it is the
c       multiple of the background rms value above which a source must be
c       before the user is given the choice of verifying it.
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
c                   the header you might call this the natural axis label
c       "relnat"    the label is in offset natural coordinates
c       
c       All offsets are from the reference pixel.  
c       Defaults are "abspix", LABTYP(1) unless LABTYP(1)="hms"
c       whereupon LABTYP(2) defaults to "dms" (for RA and DEC).
c       Ignored in 'FDR' mode (the default).
c@ options
c       Task enrichment options.  Minimum match is active.
c
c       "fiddle" means enter a routine to allow you to interactively change
c         the display lookup table.  You can cycle through b&w and colour
c         displays, as well as alter the transfer function by the cursor 
c         location, or by selecting predefined transfer functions such as 
c         histogram equalization, logarithmic, & square root.
c       "wedge" means that if you are drawing a pixel map, also draw
c         and label a wedge to the right of the plot, showing the map 
c         of intensity to colour
c
c       "3value"  means label each sub-plot with the appropriate value
c         of the third axis (e.g. velocity or frequency for an xyv ordered 
c         cube, position for a vxy ordered cube).
c       "3pixel"  means label each sub-plot with the pixel value of the
c         the third axis.   Both "3pixel" and "3value" can appear, and both 
c         will be written on the plot.  They are the average values when
c         the third axis is binned up with CHAN.  If the third axis is
c         not velocity or frequency, the units type for "3VALUE" will be 
c         chosen to be the complement of any like axis in the first 2. 
c        E.g., the cube is in vxy order and LABTYP=ABSKMS,ARCSEC the units 
c        for the "3VALUE" label will be arcsec.  If LABTYP=ABSKMS,HMS the 
c        "3VALUE" label will be DMS (if the third [y] axis is declination).
c
c       "grid" means draw a coordinate grid on the plot rather than just ticks
c
c       "noerase"  Don't erase a snugly fitting rectangle into which the 
c        "3-axis" value string is written.
c      
c       "unequal" means draw plots with unequal scales in x and y. The
c        default is that the scales are equal.
c
c       "mark" When source has been found, and user has agreed that it is
c         real, mark it with a cross.
c
c       "nofit" Prevents the program from fitting elliptical gaussians to each
c         source. The data given on each source will be that from a
c         bi-parabolic fit, as per the earlier version of sfind. Note that
c         flux densities from this fit are bi-parabolically fitted *peak*
c         flux densities, and the positions are to the peak flux density
c         position (which will always be within 1 pixel of the brightest
c         pixel in the source). This option is useful for providing a starting
c         point for groups of sources which the gaussian fitting procedure 
c         hasn't taken a liking to.
c
c       "asciiart" During the interactive section of the program, an ascii
c         picture of each source is displayed, showing which pixels have
c         been used in the gaussian fitting procedure. The brightest pixel
c         in the source is symbolised by a "O", the rest by asterisks.
c         This option is ignored if "nofit" is being used.
c
c       "auto" The interactive section of the program is bypassed, and
c         all detected sources are flagged as real. The image is not
c         displayed.
c         This is set automatically in 'FDR' mode (the default) and it is
c         only necessary to select it manually if using 'options=oldsfind'
c         (see below).
c
c       "negative" The map is inverted before source detection and fitting,
c         ie, positive pixels become negative and vice versa. This is
c         to enable detection of negative sources without recourse to MATHS.
c         This feature may be used for detecting sources in polarisation
c         maps.
c
c       "pbcorr" Corrects the flux density value calculated for each source
c         for the effect of the primary beam attenuation. This is dealt with
c         correctly for mosaics as well as single pointings.
c
c       "oldsfind" Use this to run SFIND as the original implementation
c         for the interactive interface, or just consistency with earlier
c         measurements.
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
c       "kvannot" As well as the regular log file (sfind.log, always
c         written) create a kview format annotation file, called
c         'sfind.ann,' containing one ellipse per object, with the
c         appropriate location, size, and position angle.
c
c       "fdrpeak" The default for source measurement is to use only pixels
c         above the FDR threshold in measuring the properties of
c         sources. (This is analogous, in SExtractor, for example,
c         to having the detect and analyse thresholds at the same level.)
c         In some cases it may be desirable to allow fitting of sources
c         where the peak pixel is above the FDR threshold, but other
c         source pixels are not required to be. This is the case for
c         obtaining reasonable measurements of sources close to the
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
c       "psfsize" Restricts the minimum fitted size of a detected
c         source to the size of the sythesised beam, i.e., the PSF.
c         Any source fitted to have a smaller size than this has its
c         FWHMa and PA set to those of the synthesised beam, and is refit
c         for the position and amplitude only.
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
c       In FDR mode the code has problems with very large images. I
c       think this is dependent on the available memory of the machine
c       running Sfind, but need to do more testing to be certain. I
c       have confirmed that on a machine with 256MB of memory and 256MB
c       swap space, an image of 3600x3600 pixels will be analysed correctly.
c       For larger images, the code will halt unceremoniously at the
c       first call to memfree in subroutine fdr. I don't understand
c       why this happens, although I am guessing it may have to do
c       with the call to memfree trying to free more memory than is
c       available.
c
c       The output is designed to print source fluxes in FORTRAN format
c       f8.3 and f9.3 for peak and integrated flux densities respectively.
c       This means that if your source's peak flux is > 9999.999 mJy, (ie
c       10 Jy) or its integrated flux is > 99999.999 mJy (ie, 100 Jy),
c       then it will not be displayed properly. People detecting very bright
c       sources - you have been warned!
c
c       If 'options=fdrimg', 'sigmaimg', or 'normimg' are used with a subregion
c       of the image (specified by the 'region' keyword), the output images
c       are made the size of the *full* input image. This retains the original
c       masking information, has zeroes outside the regular bounding box
c       of the selected region of interest, and the relevant output within.
c       This does not affect the analysis (which is all performed within
c       the bounding box of the regular region of interest), only the
c       output images.
c
c       The following comments refer to the original SFIND implementation.
c       The FDR implementation is much more robust to finding faint sources
c       close to bright sources, however since the gaussian fitting process
c       is the same, the comments about noise or morphology are still relevant.
c
c       The gaussian fitting procedure can at times be temperamental. If the
c       source lies in a noisy region of the map, or close to another bright
c       source, or is simply of a morphology poorly suited to being fit by
c       gaussians, firstly the source may not be detected at all, and if it
c       is, the quoted errors on position and flux density can be extremely
c       high (often displayed in the output as a row of asterisks due to the
c       vagaries of FORTRAN).
c       In many of these cases, the given values of flux density and position
c       are still quite reasonable, (perhaps with errors an order of magnitude
c       larger than would otherwise be typical), but user discretion is
c       advised. No responsibility is taken by the programmer(s) for loss
c       of life or property caused by taking the results of this program
c       under these conditions too seriously, or by frustration generated
c       by the use of this program under any conditions.
c       Additionally, for unresolved sources, the "integrated" flux
c       density quoted may be less than the peak flux density. (This occurs if
c       the fitted size of the source, proportional to bmaj x bmin, is a
c       smaller gaussian volume than that of the beam.) In this situation it
c       is suggested that the peak flux density be used.
c
c     Suggestions for believing in a source or not:
c       If a source is close to being indistinguishable by eye from the
c       background there are a few rules of thumb to help determine whether
c       the gaussian fit is telling the truth about a source, or whether the
c       source is even real.
c       1) If the pixels used in the fit are widely scattered (as opposed to
c          comprising a nice contiguous group) the fit will probably not be
c          very good and/or will not be a good description of the source.
c       2) Check the fwhms and the position angle, and compare it to the
c          pixels used in the fit. (Remember these values are in arcsec for
c          the fwhm and degrees for the pa, while the ascii picture is in
c          pixels). If these obviously do not agree, then the fit was poor
c          and the source is probably not real.
c       3) Check the rms of the background. If this is high then firstly
c          the fit may not be good (as per 1), and secondly the source is in
c          a noisy area and should be treated with caution anyway.
c--
c
c  History:
c    amh  28sep95  Major hacks to turn into SFIND from CGCURS, using source
c                  finding algorithm from AIPS task SMFND (by Taisheng Ye).
c    amh  23jan96  Added routine from imfit to do elliptical gaussian source
c                  fitting of identified sources.
c    amh  24jan96  Added options nofit and asciiart, for avoiding gaussian
c                  fits and sending ascii images of sources to screen.
c    nebk 12feb96  Massaged to fix subroutine call mismatches, remove
c                  unused variables, remove hangovers from cgcurs that
c                  were not needed, gave unvalued variables a value,
c                  remove unused subroutines.  Made no default for "cutoff"
c                  and xrms. Put some protection into BASECAL for bad 
c                  rms values. Change structure of code in subroutine SEARCH
c                  deal with blanks in BASECAL. 
c    amh  05mar96  added iterative section to subroutine FITTING, whereby
c                  sources with fwhm comparable to area used for selecting
c                  pixels are re-calculated with a larger area. Also deal
c                  with blanks in SEARCH and LOADDAT (blanks now correctly
c                  dealt with throughout). Tidied up subroutine descriptions,
c                  etc.
c    rjs  18mar96  Some FORTRAN standardisation.
c    amh  21apr97  added "auto" option, to give user choice of interactive
c                  flagging of sources or not.
c    amh  28apr97  added "negative" option, to give user ability to detect
c                  negative sources without having to seperately use MATHS.
c    amh  14may97  fixed bug concerning accidental "exit" button press. If this
c                  was accidental, user previously had no way (other than
c                  rerunning program) of accepting source detected when button
c                  was accidentally pressed.
c    rjs  21aug97  Move calls to initco and finco.
c    amh  20apr98  Add line when writing to log to indicate which image is
c                  being analysed. Also added hash marks before each line of
c                  header output for ease of later file interpretation.
c    amh  16oct98  bug causing the 'region' option to give incorrect
c                  source parameters has been fixed.
c    amh  09nov98  added option 'pbcorr' to correct for primary beam
c                  attenuation in images, including mosaics. Uses the mosLoad
c                  and mosVal routines written by rjs.
c    amh  01dec00  Major changes, to incorporate the pixel histogram fitting
c                  routine from imsad (in the new 'basecal' routine) for
c                  providing more robust estimates of background mean and
c                  rms in the presence of nearby sources (actually the method
c                  used was surprisingly good, but this is still more robust).
c    amh  09feb01  Many changes between previous entry and here, primarily
c                  to implement the FDR formalism in choosing pixels
c                  to pass to the source fitting routine. The new parameter
c                  'alpha' has been added to facilitate this, as well as
c                  new options oldsfind, fdrimg, sigmaimg, normimg, and
c                  kvannot. Also, corresponding to the new 'img' options,
c                  several output image routines have been included.
c                  Regarding program structure, the oldsfind option has
c                  been implemented by retaining the original subroutines
c                  (with marginal alterations as needed) but renaming them
c                  as xxx_old (these being search_old, basecal_old,
c                  fitting_old and LoadDat_old). Old routines which are
c                  not modified (either not used in 'FDR' mode, or are
c                  unchanged in either mode) have not undergone this
c                  change.
c    amh  26aug01  Added 'fdrpeak' option to give user ability to choose
c                  whether to fit sources using all contiguous, monotonically
c                  decreasing pixels, rather than just those above the
c                  FDR threshold.
c    amh  26oct01  Minor fixes to correct untyped variables and remove
c                  unused ones.
c    amh  28oct01  Added printout of sigma corresponding to the p-value
c                  selected by FDR.
c   nebk  14nov01  Track change to readimcg interface
c    amh  08mar02  Fix bug with array sizes (boxsize-> boxsize+1) in
c                  subroutines fitting and LoadDat.
c    amh  18mar02  Added 'allpix' option to allow fitting of all FDR pixels,
c                  not just monotonically decreasing ones.
c    amh  20mar02  Added 'psfsize' option to restrict source minimum size
c                  to the size of the synthesised beam (PSF).
c    amh  08aug02  Added 'rmsimg' option for outputting rms image. Also
c                  fixed minor bug in the way the box sizes were calculated
c                  in subroutines fdr and basecal.
c    amh  29aug02  Fixed minor bug which could cause 'psfsize' point source
c                  fits to return peak and integrated fluxes too low for
c                  bright sources.
c    amh  10jun04  Fixed bug which caused erroneously low rms's in rmsboxes
c                  with blanked pixels at the bottom left, leading to many
c                  spurious sources at the edges of non-square images.
c
c
c To do:
c
c       Add option to allow list of source positions to be read from file
c       and imfit-type fitting performed at those positions. Also include
c       in this option an option to search for the brightest pixel near the
c       given position to do the fit, or alternatively try to force the fit
c       to occur at the specified position.
c
c       Add option to allow user to "point and click" at sources and have a
c       fit done to that position (or the nearest bright pixel, by some
c       criterion, perhaps the brightest pixel in a 10x10 box centred on the
c       cursor). This could be used as well as or instead of the current
c       method. Think about two seperate options to indicate user's desire
c       of which.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
c
      real wedisp, wedwid, tfdisp
      integer maxlev, maxpos, nxdef, nydef, nbins
      parameter (maxlev = 50, maxpos = 50, nxdef = 4, nydef = 4,
     +   wedisp = 1.0, tfdisp = 0.5, wedwid = 0.05, nbins = 128)
c
      integer ipim, ipnim
c
      real levs(maxlev), pixr(2), tr(6), cs(2), pixr2(2), scale(2), 
     +  tfvp(4), wdgvp(4), cumhis(nbins), dmm(3)
      real slev, vxmin, vymin, vymax, vx, vy, vxsize, vysize, vxgap, 
     +  vygap, ydispb, xdispl, groff, blank, cut, xrms
c
      integer blc(3), trc(3), size(maxnax), win(maxnax),
     +  grpbeg(maxchan), ngrp(maxchan), srtlev(maxlev), his(nbins)
      integer nx, ny, nlevs, lin, naxis, k, ierr, pgbeg, iostat, ipage,
     +  ibin(2), jbin(2), kbin(2), krng(2), nlast, ngrps,
     +  llog, jj, wedcod, labcol, poscol, statcol, regcol, rmsbox,
     +  npnt
c
      character labtyp(2)*6
      character in*64, pdev*64, xlabel*40, ylabel*40, trfun*3, levtyp*1
c
      logical do3val, do3pix, eqscale, doblnk, dotr, donxlab(2),
     +  donylab(2), dopixel, gaps, doabut, doaxlab, doaylab,
     +  mark, doerase, dowedge, dofid, grid, nofit, asciiart,
     +  auto, negative, pbcor, oldsfind, fdrimg, sigmaimg, normimg,
     +  kvannot, fdrpeak, allpix, psfsize, rmsimg
c
      data ipage, scale /0, 0.0, 0.0/
      data dmm /1.0e30, -1.0e30, -1.0/
      data gaps, doabut, dotr /.false., .false., .false./
c-----------------------------------------------------------------------
      call output (' ')
      call output ('Sfind: Version 10-Jun-2004')
      call output (' ')
c
c Get user inputs
c
      call inputs (maxlev, in, ibin, jbin, kbin, levtyp, slev, levs, 
     +   nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix, eqscale, 
     +   nx, ny, cs, dopixel, mark, doerase, dowedge, dofid, grid,
     +   cut, rmsbox, alpha, xrms, nofit, asciiart, auto, negative,
     +   pbcor, oldsfind, sigmaimg, rmsimg, fdrimg, normimg, kvannot,
     +   fdrpeak, allpix, psfsize)
      if (oldsfind) then
       call output ('Running as Sfind: version 1.4, 09-Nov-98')
       call output (' ')
      end if
c
c Open log files
c
      call txtopen (llog, 'sfind.log', 'append', iostat)
      if (iostat.ne.0) 
     +  call bug ('f', 'Error opening text file "sfind.log"')
      call output (' ')
      call output ('*** Source list output to sfind.log')
      call output (' ')
      call output ('Now opening image...')
      call output (' ')
c
c Open image
c
      call opimcg (maxnax, in, lin, size, naxis)
      call initco (lin)
      if (pbcor) call mosLoad(lin,npnt)
c
c Finish key inputs for region of interest now
c
      call region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +             win, ngrps, grpbeg, ngrp)
c
c Try to allocate memory for images.  
c
      call memalloc (ipim,  win(1)*win(2), 'r')
      call memalloc (ipnim, win(1)*win(2), 'i')
c
c Work out coordinate transformation matrix
c
      call limitscg (blc, ibin, jbin, tr)
c
c If the source detection procedure is not to be automated, (ie,
c 'options=oldsfind' but not 'auto') then perform all
c the image opening, initialization, etc. Otherwise skip all this.
c
      if ((oldsfind).and.(.not.auto)) then
c
c Compute contour levels or check pixel map for log offset
c
       if (dopixel) then
        call grfixcg (pixr, lin, naxis, size, trfun, pixr2,
     +                groff, blank)
       else
        call conlevcg (.false., maxlev, lin, levtyp, slev, nlevs,
     +                 levs, srtlev)
        blank = -99999999.0
       end if
c
c Work out number of plots per page and number of plots
c
       call nxnycg (nxdef, nydef, ngrps, nx, ny, nlast)
c
c Work out if wedge outside or inside subplots. Also work out
c if plotting one wedge per subplot or one wedge for all  
c       
       call wedgincg ('NO', dofid, dowedge, nx, ny, 1, trfun, wedcod)
c
c Work out default character sizes for axis and channel labels
c
       call defchrcg (nx, ny, cs)
c
c Open plot device
c
       ierr = pgbeg (0, pdev, 1, 1)
       if (ierr.ne.1)then
        call pgldev
        call bug ('f', 'Error opening plot device')
       endif
c
       call pgpage
       call pgscf(2)
c
c Set line graphics colour indices
c
       call setlgc (labcol, poscol, statcol, regcol)
c       
c Init OFM routines
c       
       if (dopixel) call ofmini
c
c Set axis labels
c
       call setlabcg (lin, labtyp, .false., xlabel, ylabel)
c 
c Set label displacements from axes
c
       call setdspcg (lin, labtyp, blc, trc, xdispl, ydispb)
c
c Work out view port encompassing all sub-plots. Also return 
c the viewport size of sub-plots.
c
       call vpsizcg (.false., dofid, 0, ' ', ' ', 0, ' ', maxlev,
     +   nlevs, srtlev, levs, slev, nx, ny, cs, xdispl, ydispb, 
     +   gaps, doabut, dotr, wedcod, wedwid, tfdisp, labtyp, vxmin,
     +   vymin, vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales
c requested or if scales provided by user
c
       call vpadjcg (lin, 'NO', eqscale, scale, vxmin, vymin, vymax,
     +   nx, ny, blc, trc, tfvp, wdgvp, vxsize, vysize)
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
         call pgsvp (vx, vx+vxsize, vy, vy+vysize)
         call pgswin (blc(1)-0.5, trc(1)+0.5, blc(2)-0.5, trc(2)+0.5)
c
c Read in image
c
         call readimcg (.true., blank, lin, ibin, jbin, krng, blc,
     +     trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
c
c Apply transfer function
c
         call pgsci (labcol)
         if (dopixel) then
           if (trfun.ne.'lin') call apptrfcg (pixr, trfun, groff, 
     +        win(1)*win(2), memi(ipnim), memr(ipim), nbins,
     +        his, cumhis)
c
c Draw wedge 
c
           if (wedcod.eq.1 .or. wedcod.eq.2) then
            call pgsch (cs(1))
            call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins,
     +                    cumhis, wdgvp, pixr(1), pixr(2))
           end if
         end if
c
c Draw pixel map; set default b&w colour table first.
c
         call pgsci (labcol)
         if (dopixel) then
           if (k.eq.1) call ofmcol (1, pixr2(1), pixr2(2))
           call pgimag (memr(ipim), win(1), win(2), 1, win(1), 1,
     +                  win(2), pixr2(1), pixr2(2), tr)
         else 
c
c Draw contours
c
           call conturcg (.false., blank, .false., win(1), win(2),
     +                    doblnk, memr(ipim), nlevs, levs, tr, 0.0)
         end if
c
c Determine if the axes need ascii or numeric labelling
c for this subplot
c
         call pgsch (cs(1))
         call dolabcg (gaps, dotr, nx, ny, ngrps, nlast, k,
     +                 labtyp, doaxlab, doaylab, donxlab, donylab)
c
c Write on ascii axis labels
c

         call aaxlabcg (doaxlab, doaylab, xdispl, ydispb,
     +                             xlabel, ylabel)
c
c Draw frame, write numeric labels, ticks and optional grid
c
         call naxlabcg (lin, .true., blc, trc, krng, labtyp,
     +                  donxlab, donylab, .false., grid)
c
c Draw wedge inside subplots and overwrite label ticks
c
         if (wedcod.eq.3) then
           call pgsch (cs(1))
           call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins,
     +                  cumhis, wdgvp, pixr(1), pixr(2))
         end if
c
c Modify lookup table
c
         if (dofid) call ofmmod (tfvp, win(1)*win(2), memr(ipim), 
     +                           memi(ipnim), pixr2(1), pixr2(2))
c
c Write velocity or channel label
c
         if (do3val .or. do3pix) then
           call pgsch (cs(2))
           call pgsci (1)
           call lab3cg (lin, doerase, do3val, do3pix, labtyp,
     +                  grpbeg(k), ngrp(k))
         end if
c
c Interactive graphical source finding routine
c
         call search_old (lin, win(1), win(2), memr(ipim), memi(ipnim),
     +     blc, ibin, jbin, krng, llog, mark, cut, rmsbox, xrms, 
     +     nofit, asciiart, auto, negative, pbcor, in, psfsize)
c
c Increment sub-plot viewport locations and row counter
c
         call subinccg (k, nx, ny, vxmin, vymax, vxsize, vysize, 
     +                  vxgap, vygap, vx, vy)
c
c Page plot device
c
         if (jj.eq.nx*ny .and. k.lt.ngrps) call pgpage
       end do
c
c Close up
c
       call memfree (ipim,  win(1)*win(2), 'r')
       call memfree (ipnim, win(1)*win(2), 'i')
c
       call finco (lin)
       call xyclose(lin)
       call txtclose(llog)
       call pgend
c
c Now the image display section of code is passed, perform search automatically
c (non-interactively) if required.
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
        call readimcg (.true., blank, lin, ibin, jbin, krng, blc,
     +     trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
c
c The source-detection subroutine.
c
        if (oldsfind) then
         call search_old (lin, win(1), win(2), memr(ipim), memi(ipnim),
     +     blc, ibin, jbin, krng, llog, mark, cut, rmsbox, xrms, 
     +     nofit, asciiart, auto, negative, pbcor, in, psfsize)
        else
         call search(lin, win(1), win(2), memr(ipim), memi(ipnim),
     +     blc, ibin, jbin, krng, llog, rmsbox, alpha, xrms, auto,
     +     negative, pbcor, in, fdrimg, sigmaimg, rmsimg,
     +     normimg, kvannot, fdrpeak, allpix, psfsize, size)
        end if
       end do
c
c Close up
c
       call memfree (ipim,  win(1)*win(2), 'r')
       call memfree (ipnim, win(1)*win(2), 'i')
c
       call xyclose(lin)
       call txtclose(llog)
      end if
c
      end
c
c
      subroutine cgcur (x, y, ans)
      implicit none
      real x, y
      character ans*1
      call pgupdt
      call pgcurs (x, y, ans)
      call pgupdt
      call ucase (ans)
c
      end
c
c
      subroutine search_old (lin, nx, ny, image, nimage, blc, ibin,
     +   jbin, krng, llog, mark, cut, rmsbox, xrms, nofit, asciiart,
     +   auto, negative, pbcor, in, psfsize)
c-----------------------------------------------------------------------
c  This is the master subroutine for the detecting of sources and the
c interactive decision bit. It detects bright pixels, determines whether they
c are brighter than the surrounding 24 pixels (of which none are permitted to
c be blanked) and if so, performs a bi-parabolic fit to the peak. This is
c then used to determine if the source is above a user set multiple of the
c background rms around the source, and if so an elliptical gaussian fit is
c carryed out. The results are then written to the screen and the cursor
c moved to the position of the object where it waits for the users yay or nay.
c After user input, the source parameters are written, along with a flag (Y
c or N), to the log file, sfind.log
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
c     xrms      multiple of background rms above which source must be before
c               user is given option of saying yay or nay
c     nofit     True means don't do gaussian fitting for each source
c     asciiart  display ascii representations of each source during
c               interactive source selection
c     auto      skip all the interactive bits of source flagging.
c     negative  inverts image: positive pixels become negative and vice versa
c     pbcor     true to correct fluxes by primary beam attenuation
c     in        image name
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nx, ny, blc(2), llog, ibin, jbin, lin, nimage(nx,ny),
     +  krng(2)
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
      call output (' ')
      if (.not.auto) then
       call output ('************************************')
       call output ('Beginning Interactive Source Finding')
       call output ('************************************')
       call output (' ')
       call output ('Click left button   (enter A) to flag source as Y')
       call output
     +  ('Click middle button (enter D) to flag source as N')
       call output ('Click right button  (enter X) to quit')
       call output (' ')
      else
       call output ('****************************************')
       call output ('Beginning Non-Interactive Source Finding')
       call output ('****************************************')
       call output (' ')
       call output ('Please be patient...')
      end if
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
       do l = 1,nx
        do m = 1,ny
         image(l,m) = -image(l,m)
        end do
       end do
      end if
c
c Write header for output to screen, unless "auto" option selected.
c
      write(line1,19) '# Sources from image ',in
19    format(a21,a59)
      if (.not.auto) call output (line1)
      if (nofit) then
        write(line2,20) '#','RA','DEC','flux','x','y','rms',
     +                  'flux/rms'
20      format(a,4x,a,11x,a,4x,a,7x,a,8x,a,7x,a,2x,a)
        if (.not.auto) call output(line2)
        line3 = '#                        mJy      pixels   '//
     +          'pixels    mJy'
        if (.not.auto) call output (line3)
        line4 = '#-----------------------------------------'//
     +          '-------------------------------------'
        if (.not.auto) call output (line4)
       else
        write(line2,30) '#','RA','DEC','err(RA)','err(DEC)','pk-flux',
     +                  'err','flux','bmaj','bmin','pa',
     +                  'rms(bg)','rms(fit)'
30      format(a,4x,a,9x,a,5x,a,1x,a,1x,a,3x,a,6x,
     +         a,3x,a,3x,a,3x,a,1x,a,1x,a)
        if (.not.auto) call output(line2)
        line3 = '#                       arcsec   arcsec   '//
     +          'mJy      mJy       mJy  arcsec arcsec '//
     +          'deg  mJy     mJy'
        if (.not.auto) call output (line3)
        line4 = '#-----------------------------------------'//
     +          '-------------------------------------'//
     +          '----------------'
        if (.not.auto) call output (line4)
      end if
c
c Write output header for log file
c
      call txtwrite (llog, line1, len1(line1), iostat)
      call txtwrite (llog, line2, len1(line2), iostat)
      call txtwrite (llog, line3, len1(line3), iostat)
      call txtwrite (llog, line4, len1(line4), iostat)
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
      call BeamPar (lIn, k, bvol, bvolp, bmaj, bmin, bpa,
     +              bmajp, bminp, bpap, nobeam)
c
c Loop over all pixels, searching for bright pixels and moving the cursor
c to that position.
c
      do l = 3,nx-3
       do m = 3,ny-3
        pval = image(l,m)
c
c Ignore pixels less than cut and check to see if pixel brighter than
c surrounding 24 pixels, making sure none are blanked pixels.
c
        if (pval.lt.cut) goto 60
        do i = l-2,l+2
          do j = m-2,m+2
            if( ((i.ne.l .or. j.ne.m) .and. nimage(i,j).gt.0 .and.
     +          pval.le.image(i,j)) .or. nimage(i,j).eq.0) goto 60
          end do
        end do
c
c Bi-parabolic fit, base-level and background rms calculation and subtraction
c of base-level from peak
c
        call peakfit(l, m, xpos, ypos, nx, ny, peak, image)
        call basecal_old(nx, ny, l, m, base0, sigma, rmsbox, image,
     +               nimage, 1.5*bmajp, ok)
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
     +     xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj, amin,
     +     posa, posns, blc, bin, l, m, asciiart, bvol, bvolp, bmajp,
     +     bminp, bpap, nobeam, nimage, fitok, auto, xrms, psfsize,
     +     base0)
          if (.not.fitok) goto 60
c
          typei(1) = 'hms'
          typei(2) = 'dms'
        else
          posns(1) = wa(1)
          posns(2) = wa(2)
          typei(1) = 'abspix'
          typei(2) = 'abspix'
        end if
        typeo(1) = 'hms'
        typeo(2) = 'dms'
c
c Convert location (peak or fitted) to formatted coordinate string
c
        call w2wfco (lin, 2, typei, ' ', posns,  typeo, ' ',
     +               .true., radec, radeclen)
c
c if 'pbcor' selected, correct the flux densities (peak and integrated)
c by the gain (primary beam attenuation) at the position of the source
c
        if (pbcor) then
         call mosVal(lin,'aw/aw',posns,gain,mvlrms)
         pkfl = pkfl/gain
         intfl = intfl/gain
c         sigma = sigma/gain
        end if
c
c Write output line to screen, but only if "auto" option not selected
c
        if (nofit) then
          if (negative) then
           write(line,40) -peak*1000., xpos, ypos,
     +                   -sigma*1000., mult
          else
           write(line,40) peak*1000., xpos, ypos,
     +                   sigma*1000., mult
          end if
40        format(f8.3,2x,f7.2,2x,f7.2,2x,f7.3,2x,f5.1)
        else
          if (negative) then
           write(line,50) xposerr,yposerr,-pkfl,pkflerr,-intfl,
     +        amaj,amin,posa,-sigma*1000.,-rms*1000.
          else
           write(line,50) xposerr,yposerr,pkfl,pkflerr,intfl,
     +        amaj,amin,posa,sigma*1000.,rms*1000.
          end if
50        format(1x,f6.3,3x,f5.2,2x,f8.3,1x,f6.3,1x,f9.3,1x,
     +           3(f5.1,1x),f6.3,2x,f6.3)
        end if
        line = radec(1)(1:radeclen(1))//' '//
     +         radec(2)(1:radeclen(2))//' '//line
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
          call cgcur (ww(1), ww(2), cch)
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
              call pgslw (3)
              call pgsci (2)
              call pgpt (1, wsave(1), wsave(2), 2)
              call pgupdt
              call pgslw (1)
            end if
c
            line(len1(line)+1:len1(line)+6) = '     Y'
            call txtwrite (llog, line, len1(line), iostat)
          else if (cch.eq.'D') then
            if (mark) then
              call pgslw (3)
              call pgsci (5)
              call pgpt (1, wsave(1), wsave(2), 4)
              call pgupdt
              call pgslw (1)
            end if
c
            line(len1(line)+1:len1(line)+6) = '     N'
            call txtwrite (llog, line, len1(line), iostat)
c
c When user presses quit-type command, check to see that's really what
c they wanted to do, and if so do it. If not, accept the different button
c as the intended command.
c
          else if (cch.eq.'X') then
            call output('Are you sure you want to quit here?')
            call output('(press again to confirm, or other key '//
     +                    'for corresponding action)')
            call cgcur (ww(1), ww(2), cch)
            if (cch.eq.'X') then
             goto 70
            else
             goto 55
            end if
          else
            call output ('  Commands are: A (yes), D (no), X (exit).')
          end if
         end do
        else
c
c The "auto" procedure treats every source as if the user flagged it good
c (pressed button "A").
c
         iloc = iloc + 1
         ysources = ysources + 1
         line(len1(line)+1:len1(line)+6) = '     Y'
         call txtwrite (llog, line, len1(line), iostat)
        end if
60      continue
       end do
      end do
c
70    call output (' ')
      write (line,80) sources
80    format('Total number of sources detected:',i5)
      call output(line)
      write (line,90) ysources
90    format('Number of sources confirmed:',i5)
      call output(line)
      call output(' ')
c
c
      end
c
c
      subroutine peakfit (l, m, x, y, nx, ny, peak, image)
c-----------------------------------------------------------------------
c    Perform a bi-parabolic fit to the position l,m and return fitted
c    coordinates x,y and flux density, peak. Parabolas are fitted along
c    both the x and y axes, and the resulting position is the x,y coords
c    of the parabola's peak (always within one pixel of the brightest pixel).
c    The resulting peak flux density is simply the flux density the brightest
c    pixel would have had if it had been centered at the position x,y.
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
      z = 0.
      t = image(l,m)
c
      b = image(l+1,m) - image(l-1,m)
      a = image(l+1,m) - 2*t + image(l-1,m)
      if ((abs(b).gt.abs(a)).or.(a.ge.0.)) goto 2010
      x = x - 0.5*b/a
      z = b*b/a
2010  continue
c
      b = image(l,m+1) - image(l,m-1)
      a = image(l,m+1) - 2*t + image(l,m-1)
      if ((abs(b).gt.abs(a)).or.(a.ge.0.)) goto 2015
c
      y = y - 0.5*b/a
      z = z + b*b/a
2015  peak = t - 0.125*z
c
      end
c
c
      subroutine basecal_old (nx, ny, l, m, base0, sigma, rmsbox,
     +                   image, nimage, boxsize, ok)
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
      base0 = 0.
      base1 = 0.0
      base2 = 0.0
      basen = 0
      do jj = mmn, mmx
        do ii = lmn, lmx
          if (nimage(ii,jj).gt.0) then
            basen = basen + 1.
            base1 = base1 + image(ii,jj)
            base2 = base2 + image(ii,jj)**2
          end if
        end do
      end do
      if (basen.gt.0) then
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
        end if
      else
        ok = .false.
      end if
      if (.not.ok) return
c
c Now iterate 3 times from this starting point
c
      do kk = 1,3
        base1 = 0.
        base2 = 0.
        basen = 0.
        do ii = lmn, lmx
          rx = (ii - l)**2
          do jj = mmn, mmx
            rr = rx + (jj-m)**2
            if (nimage(ii,jj).gt.0 .and. rr.ge.(boxsize**2) .and.
     +          abs(image(ii,jj)-base0).le.3*sigma) then
              basen = basen + 1.
              base1 = base1 + image(ii,jj)
              base2 = base2 + image(ii,jj)**2
            end if
          end do
        end do
c
c Mean and sigma
c
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
          return
        end if
      end do
c
c If on last iteration, had less than 10 pixels,
c set base level to zero and set ropy sigma
c
      if (basen.lt.10.0) then
        base0 = 0.
        sigma = sqrt(base2/basen)
      end if
c
      end
c
c
      subroutine fitting_old (lIn, krng, sigma, nx, ny, image, rms,
     +   xpos, xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj,
     +   amin, posa, posns, blc, bin, lx, my, asciiart, bvol, bvolp,
     +   bmajp, bminp, bpap, nobeam, nimage, fitok, auto, xrms, psfsize,
     +   base0)
c-----------------------------------------------------------------------
c  Do elliptical Gaussian fit to source. First the pixels to be used in
c  the fitting are selected on the basis that they are monotonically
c  decreasing away from the "central" bright pixel, then the gaussian
c  fit is done as per the task "imfit". If the size of the region within
c  which pixels were selected for the fit is smaller than 4 times the fwhm
c  of the major axis of the source, the procedure is iterated with a larger
c  region. If this procedure diverges, (detected by the increasing region
c  becoming larger than the image size), the source is ignored.
c  Once the source size and parameters are finally accepted, the background
c  rms is recalculated (more accurately now the source size is known) again,
c  and used for the output.
c
c  Once the fit is done the source parameters are recorded and passed back
c  to subroutine "search" for it to output as appropriate.
c
c  Input
c    nx,ny    Size of image
c    image    Image
c    nimage   Normalised Image (for masking info)
c    sigma    the background rms around the source. Used to clip pixels. It is
c             recalculated after the source fitting, and is an output as well.
c    nobeam   logical used to determine default beam size if not detected in
c             image header.
c    bmajp    pixel size of major axis of beam. Used for setting size of box
c             within which to detect pixels for gaussian fitting.
c    blc, bin image params used for switching between full image and binned
c             subimage pixels.
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
c    amaj, amin, posa axes (arcsec) and position angle (degrees) of object.
c    rms              rms of gaussian fit to selected pixels.
c    fitok            logical. False if the fit failed for any reason.
c
c-----------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer MAXVAR
      parameter(MAXVAR=20)
c
      logical dofit, asciiart, nobeam, fitok, ok, auto, psfsize
      integer ifail1,ifail2,lIn, i, blc(2), bin(2), maxline, boxsz4
      integer k,m,nvar,lx,my, nx,ny, krng(2), nimage(nx,ny), fiterr
      real image(nx,ny), dumm, xrms
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
      dumm = 0.
      nsrc = 1
      do i = 1,nsrc
        vflux(i) = .true.
        vl0(i) = .true.
        vm0(i) = .true.
        vfwhm1(i) = .true.
        vfwhm2(i) = .true.
        vpa(i) = .true.
        fwhm1(i) = 1.
        fwhm2(i) = 1.
        l0(i) = 0.
        m0(i) = 0.
      end do
      clip = sigma
      fitok = .true.
c
c Set the plane appropriate to the displayed image.  Since
c this may be a range of planes averaged together, take the
c integer nearest the average plane
c
      k = nint(real(krng(1) + krng(1) + krng(2) - 1)/2.0)
c
c  Set the size of the region within which to select pixels to be used in
c  the gaussian fitting procedure.
c
      if (nobeam) then
       boxsize = 5.
      else
       boxsize = bmajp
      end if
c
c  Load the data. Ie, choose the pixels in the region of the source to
c  be used in the fitting procedure, and encode them in a format ready
c  to do so.
c
1200  call LoadDat_old (clip,m,nx,ny,xpos,ypos,blc,bin,image,xrms,
     +  boxsize,lx,my,nimage,asciiart,ascpic,maxline,fitok,base0)
      if (.not.fitok) return
      if (m.eq.0) then
       fitok = .false.
       return
      end if
c
c  Convert the coordinates to pixels, and fill in defaults if necessary.
c
      call CoordFid(lIn,k,.true.)
      call GetEst
c
c  Pack the variables into an array.
c
      call PackVar(xvar,nvar,MAXVAR)
      dofit = nvar.gt.0
      if(.not.dofit) then
        fitok = .false.
        return
      end if
      if(nvar.ge.m) then
        fitok = .false.
        return
      end if
c
c  Do the fitting process.
c
      fiterr = 0
      call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
      call UPackVar(xvar,nvar)
      if(ifail2.eq.0)call UpackCov(covar,nvar)
      if(ifail1.ne.0) fiterr = 1
      if(ifail2.ne.ifail1) fiterr = 2
c
c If 'psfsize' selected, check object size and if smaller than
c psf, set to psf size and fit again for peak and position.
c
      if ((psfsize).and.((fwhm1(1).lt.bmajp).or.(fwhm2(1).lt.bminp)))
     +       then
       do i = 1,nsrc
         vflux(i) = .true.
         vl0(i) = .true.
         vm0(i) = .true.
         vfwhm1(i) = .false.
         vfwhm2(i) = .false.
         vpa(i) = .false.
         fwhm1(i) = bmajp
         fwhm2(i) = bminp
         pa(i) = bpap
       end do
       fitok = .true.
c  Pack the variables into an array.
       call PackVar(xvar,nvar,MAXVAR)
       dofit = nvar.gt.0
       if(.not.dofit) then
         fitok = .false.
         return
       end if
       if(nvar.ge.m) then
         fitok = .false.
         return
       end if
c  Do the fitting process.
       fiterr = 0
       call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
       call UPackVar(xvar,nvar)
       if(ifail2.eq.0)call UpackCov(covar,nvar)
       if(ifail1.ne.0) fiterr = 1
       if(ifail2.ne.ifail1) fiterr = 2
      end if
c
c  Check to see whether the fitted size of the image is comparable to the
c  initial box size used in selecting pixels, and if so iterate the
c  procedure choosing a larger box size.
c
      if (boxsize.lt.max(abs(fwhm1(1)),abs(fwhm2(1)))) then
c
c Checks for divergence - eg, noise being fitted by ever larger
c gaussians. Stops and aborts fit if object fwhm is larger than one quarter the
c smallest image dimension, or if the fitted fwhm has increased to more than
c 60 pixels. (At sizes > 60 pixels, the next iteration takes quite a while,
c and leaves a big wait for nothing if the object is then discarded on the
c next iteration.) This (moderately) arbitrary number is, however, an
c unsatisfactory way of dealing with the problem, and a better one would
c be appreciated.
c
       if (max(abs(fwhm1(1)),abs(fwhm2(1))).lt.min(nx,ny,240)/4) then
        boxsize = max(abs(fwhm1(1)),abs(fwhm2(1)))
        goto 1200
       else
        fitok = .false.
        return
       end if
      end if
c
c use basecal to calculate updated value for the background rms, using
c newly found size of object to determine central region to exclude and
c overall region to include (boxsize and 4*boxsize, respectively)
c (Note that here, we are no longer interested in base0 and it has been
c replaced by a dummy var. "dumm".)
c
      boxsz4 = 4*boxsize
      call basecal_old(nx,ny,lx,my,dumm,sigma,boxsz4,image,nimage,
     +             boxsize,ok)
c
c if the new value was unable to be calculated - too few pixels, etc,
c put back its original value, which has been stored as the variable clip.
c
      if (.not.ok) sigma = clip
c
c print out ascii art if required
c
      if (asciiart) then
       do i = maxline,1,-1
        if ((ascpic(i).ne.' ').or.(i.eq.1).or.(i.eq.maxline))
     +      call output(ascpic(i))
       end do
      end if
c
c Print out warnings if there were problems with the fits, unless in 'auto'
c
      if ((.not.auto).and.(fiterr.gt.0)) then
       if (fiterr.eq.1) then
        call bug('w','Fitting failed to converge.')
       else
        call bug('w','Failed to determine covariance matrix.')
       end if
       call bug('w','Source may still be real, but parameters may'
     +          //' be incorrect.')
      end if
c
c  Convert to astronomical units, and report - ie, finalise results.
c
      call CoordFid(lIn,k,.false.)
      call Report (bvol, bvolp, xposerr, yposerr,
     +   pkfl, pkflerr, intfl, amaj, amin, posa, posns, lIn)
c
      end
c
c
      subroutine LoadDat_old(clip,m,nx,ny,xpos,ypos,blc,bin,image,xrms,
     +     boxsize,lx,my,nimage,asciiart,ascpic,maxline,fitok,base0)
c-----------------------------------------------------------------------
c  Load the relevant data for this plane. The relevant data are those pixels
c  to which to fit the elliptical gaussians, and they are selected by ...
c
c  Input:
c    clip      Clip level.
c  Output:
c    m            Number of points loaded.
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer m,nx,ny,lmn,lmx,mmn,mmx,nimage(nx,ny)
      integer i,xt,yt,ll,mm, blc(2),bin(2), lx,my, maxline
      real clip,xpos,ypos,image(nx,ny),boxsize,xrms,base0
      double precision wa(2)
      logical incirc,fainter,asciiart,fitok
      character ascpic(1000)*80
c-----------------------------------------------------------------------
c
c calculate extremes of box around object, with size 4*bmaj (major axis of
c beam) in pixels (or larger if a later iteration).
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
     +           //'in one dimension')
        fitok = .false.
        return
      end if
      do i = 1,maxline
       ascpic(i) = ' '
      end do
c
c Choose only those pixels within a *circle* of radius 2*boxsize which are
c also fainter than all the pixels between themselves and the brightest
c pixel and which aren't blanked.
c Store pixel values of object and surrounding region in linear array, data,
c and fill out the x,y, coordinate.
c
      i = 0
      xt = 0
      yt = 0
      do mm = mmn,mmx
       do ll = lmx,lmn,-1
        incirc = ((ll-xpos)**2 + (mm-ypos)**2).lt.(4*boxsize**2)
        call slopey(ll,mm,lx,my,nx,ny,image,fainter)
c        if ((image(ll,mm).gt.xrms*clip).and.incirc.and.fainter.and.
        if ((image(ll,mm).gt.clip).and.incirc.and.fainter.and.
     +      (nimage(ll,mm).gt.0)) then
         if ((ll.eq.lx).and.(mm.eq.my)) then
          ascpic(mm-mmn+2) = 'O'//ascpic(mm-mmn+2)
         else
          ascpic(mm-mmn+2) = '*'//ascpic(mm-mmn+2)
         end if
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
         ascpic(mm-mmn+2) = ' '//ascpic(mm-mmn+2)
        end if
       end do
      end do
      ndata = i
      m = ndata
      xoff = xt / real(ndata)
      yoff = yt / real(ndata)
c
      end
c
c
      subroutine search (lin, nx, ny, image, nimage, blc, ibin, jbin,
     +   krng, llog, rmsbox, alpha, xrms, auto, negative, pbcor, in,
     +   fdrimg, sigmaimg, rmsimg, normimg, kvannot, fdrpeak, allpix,
     +   psfsize, size)
c-----------------------------------------------------------------------
c  This is the *new* master subroutine for the detecting of sources.
c It runs subroutine fdr, which makes the normalised image, sigma image
c and fdr image (if necessary), and most importantly, establishes the 
c fdr threshold. Then sources are measured, in routine fdrfit.
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
c     alpha     FDR percentage of 'false discoveries' for setting threshold
c               when finding probable source pixels.
c     xrms      multiple of background rms above which source must be before
c               user is given option of saying yay or nay
c     auto      true if not being run interactively
c     negative  inverts image: positive pixels become negative and vice versa
c     pbcor     true to correct fluxes by primary beam attenuation
c     in        image name
c     fdrimg    true to output FDR-image
c     sigmaimg  true to output sigma-image
c     rmsimg    true to output rms-image
c     normimg   true to output normalised image
c     kvannot   true if want to output a kview annotation file for the
c               detected objects
c     fdrpeak   true if want to allow fitting of all source pixels, not just
c               those above the fdr threshold
c     allpix    true if want to use all FDR pixels, not just monotonically
c               decreasing ones
c     psfsize   true to restrict minimum source size to that of psf
c
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'sfind.h'
c
      integer nx, ny, blc(2), llog, ibin, jbin, lin, nimage(nx,ny),
     +  krng(2), size(maxnax)
      integer ipim,ip2im,ip3im,ip4im
      real image(nx,ny), alpha, xrms
      logical negative, pbcor, fdrimg, sigmaimg, rmsimg, normimg,
     +        auto, kvannot, fdrpeak, allpix, psfsize
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
      call BeamPar (lIn, k, bvol, bvolp, bmaj, bmin, bpa, 
     +              bmajp, bminp, bpap, nobeam)
      if (nobeam) then
       call bug('f','No beam information found - FDR analysis'
     +      //' needs this to work')
      end if
c Now begin properly.
      call output (' ')  
      call output ('****************************************')
      call output ('Beginning Non-Interactive Source Finding')
      call output ('****************************************')
      call output (' ')
      call output ('Please be patient...')
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
       do l = 1,nx
        do m = 1,ny
         image(l,m) = -image(l,m)
        end do
       end do
      end if
c
c Write header for output
c
      write(line1,19) '# Sources from image ',in
19    format(a21,a59)
      write(line2,30) '#','RA','DEC','err(RA)','err(DEC)','pk-flux',
     +                'err','flux','bmaj','bmin','pa',
     +                'rms(bg)','rms(fit)'
30    format(a,4x,a,9x,a,5x,a,1x,a,1x,a,3x,a,6x,
     +       a,3x,a,3x,a,3x,a,1x,a,1x,a)
      line3 = '#                       arcsec   arcsec   '//
     +        'mJy      mJy       mJy  arcsec arcsec '//
     +        'deg  mJy     mJy'
      line4 = '#-----------------------------------------'//
     +        '-------------------------------------'//
     +        '----------------'
c
c Write output header for log file
c
      call txtwrite (llog, line1, len1(line1), iostat)
      call txtwrite (llog, line2, len1(line2), iostat)
      call txtwrite (llog, line3, len1(line3), iostat)
      call txtwrite (llog, line4, len1(line4), iostat)
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
     +         pcut,lin,memr(ipim),memr(ip2im),memr(ip3im),
     +         memr(ip4im),fdrimg,sigmaimg,rmsimg,normimg,auto,
     +         blc,bin,size(1),size(2),nfdrpix)
c
c Now do the source fitting, in the original image, using the pixels
c selected by FDR to be above the background.
c
      call fdrfit(nx,ny,image,nimage,bvol,bpap,bvolp,bmajp,
     +      bminp,pcut,memr(ip2im),lin,krng,blc,bin,negative,pbcor,
     +      llog,kvannot,fdrpeak,allpix,psfsize,alpha,nfdrpix,
     +      memr(ip3im),memr(ip4im))
c free meanimg and sgimg memory
      call memfree(ip3im,nx*ny,'r')
      call memfree(ip4im,nx*ny,'r')
c
      call memfree(ipim,nx*ny,'r')
      call memfree(ip2im,nx*ny,'r')
c
70    call output (' ')
c
      end
c
c
      subroutine basecal (nx, ny, l, m, base0, sigma, rmsbox, image,
     +                    nimage, boxsize, ok, mask, image2, auto)
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
      real mean
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
c calling subroutine hist (from imsad.for) to get histogram fitted sigma value
c for comparison with rms value calculated below.
      blc(1) = lmn
      blc(2) = mmn
      trc(1) = lmx
      trc(2) = mmx
      rng(1) = 1.0e30
      rng(2) = -1.0e30
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
        end if
       end do
      end do
      sigma = 0.
      base0 = 0.
      call hist(blc,trc,image2,mask,rng,histok,sigma,mean,auto)
      if (histok) then
       base0 = mean
      else
c
c If the histogram fit didn't work then do it the hard way.
c First estimate of sigma from all pixels
       base0 = 0.
       base1 = 0.0
       base2 = 0.0
       basen = 0
       do jj = mmn, mmx
        do ii = lmn, lmx
          if (nimage(ii,jj).gt.0) then
            basen = basen + 1.
            base1 = base1 + image(ii,jj)
            base2 = base2 + image(ii,jj)**2
          end if
        end do
       end do
       if (basen.gt.0) then
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
        end if
       else
        ok = .false.
       end if
       if (.not.ok) return
c
c Now iterate 3 times from this starting point
c
       do kk = 1,3
        base1 = 0.
        base2 = 0.
        basen = 0.
        do ii = lmn, lmx
          rx = (ii - l)**2
          do jj = mmn, mmx
            rr = rx + (jj-m)**2
            if (nimage(ii,jj).gt.0 .and. rr.ge.(boxsize**2) .and.
     +          abs(image(ii,jj)-base0).le.3*sigma) then
              basen = basen + 1.
              base1 = base1 + image(ii,jj)
              base2 = base2 + image(ii,jj)**2
            end if
          end do
        end do
c
c Mean and sigma
c
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
          return
        end if
       end do
c
c If on last iteration, had less than 10 pixels,
c set base level to zero and set ropy sigma
c
       if (basen.lt.10.0) then
        base0 = 0.
        sigma = sqrt(base2/basen)
       end if
      end if
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
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      include 'sfind.h'
c
      integer MAXBOX, MAXVAR, MAXBIN
      parameter (MAXBOX=1024, MAXVAR=20, MAXBIN=50)
c
      integer i, j, n, ifail1, ifail2, nvar, ni, nj,
     +  blc(MAXNAX), trc(MAXNAX), id
      integer nwithinr
      real trms, mom(3), wmin, wmax, wid,
     +  rms, sum, squ, covar(MAXVAR*MAXVAR), xvar(MAXVAR), image(*),
     +  bmax, norm, range(2), drange, xtemp(MAXBIN), dtemp(MAXBIN)
      real mean
c
      logical mask(*), ok, auto
c
      external function
c-----------------------------------------------------------------------
      ns = 1
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
      end do
c
c Read image data and compute moments
c
      ni = trc(1) - blc(1) + 1
      nj = trc(2) - blc(2) + 1
c
      n = 0
      do i = 1, ni*nj
        if(mask(i)) then
          n = n + 1
          sum = sum + image(i)
          squ = squ + image(i)**2
        end if
      end do
c
      if(n.eq.0) then
        if (.not.auto) call bug('w', 'Image plane all blank')
        ok = .false.
        return
      end if
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
      end if
      wid = (wmax - wmin) / nd
c
c Form histogram
c
      do i = 1, ni*nj
        if (mask(i) .and. image(i).ge.wmin.and.image(i).le.wmax) then
          id = int((image(i) - wmin) / wid) + 1
          id = max(1,min(id,nd))
c
          data2(id) = data2(id) + 1.0
          xd(id) = xd(id) + image(i)
        end if
      end do
c
c Work out some scaling factors and set the abcissa bin values by the
c average contributing to that bin
c
      norm = 0.0
      do i = 1, nd
        if(data2(i).gt.0.0) then
          xd(i) = xd(i) / data2(i)
          norm = norm + data2(i)
        end if
        xtemp(i) = xd(i)
        dtemp(i) = data2(i)
      end do
c
c Remove empty bins from fit
c
      j = 1
      do i = 1, nd
        if (dtemp(i).gt.0.0) then
          data2(j) = dtemp(i)
          xd(j) = xtemp(i)
          j = j + 1
        end if
      end do
      nd = j - 1
c
      if(nd.eq.0) then
c        call bug('w','No data in histogram')
        ok = .false.
        return
      end if
c     
c Normalise histogram volume to 1.0
c
      bmax = -1.0e30
      do i = 1, nd
        data2(i) = data2(i) / norm
        bmax = max(bmax,data2(i))
      end do
c
c Set Gaussian fits parameter estimates.
c 1: peak, 2: centre, 3: FWHM
c
      nsrc = 1
c l0 = mean (mu), m0 = FWHM (and sigma=rms=m0/2sqrt(2ln2)).
      do i = 1,nsrc
       flux(i) = bmax
       l0(i) = mom(1)
       m0(i) = mom(2) * sqrt(8.0 * log(2.0))
c
       fwhm1(i) = 0.0
       fwhm2(i) = 0.0
       pa(i) = 0.0
c
       vflux(i) = .true.
       vl0(i) = .true.
       vm0(i) = .true.
       vfwhm1(i) = .false.
       vfwhm2(i) = .false.
       vpa(i) = .false.
      end do
c
c Fit histogram with a Gaussian
c
      xoff = 0.0
      yoff = 0.0
      gdim = 1
      ndata = nd
c
      call packvar(xvar,nvar,MAXVAR)
      if(nvar.eq.0) call bug ('f', 'HIST: Internal logic error')
c
      call lsqfit(FUNCTION,nd,nvar,xvar,covar,rms,ifail1,ifail2)
c
      call upackvar(xvar,nvar)
      if(ifail2.eq.0) call upackcov(covar,nvar)
      if (ifail1.ne.0 .or. ifail2.ne.0) then
        ok = .false.
        return
      end if
c
c Compute fitted image rms, and mean
c
      trms = abs(m0(1)) / sqrt(8.0 * log(2.0))
      mean = l0(1)
c
      end
c
c
      subroutine decopt(do3val, do3pix, eqscale, mark, doerase, 
     +                  dowedge, dofid, grid, nofit, asciiart, auto,
     +                  negative, pbcor, oldsfind, fdrimg, sigmaimg,
     +                  rmsimg, normimg, kvannot, fdrpeak, allpix,
     +                  psfsize)
c----------------------------------------------------------------------
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
c     auto      True means skip all interactive bits, including displaying
c               image
c     negative  inverts image: positive pixels become negative and vice versa
c     pbcor     true to correct fluxes by primary beam attenuation
c     oldsfind  true if want to use sfind in original mode (no fdr)
c     fdrimg    true if want to create fdr-selected output pixel image
c     sigmaimg  true if want to create sigma-clipped output pixel image
c     rmsimg    true if want to create rms-image
c     normimg   true if want to create 'normalised' output image
c     kvannot   true if want to output a kview annotation file for the
c               detected objects
c     fdrpeak   true if want to allow fitting of all source pixels, not just
c               those above the fdr threshold
c     allpix    true if want to use all pixels, not just monotonically
c               decreasing ones
c     psfsize   true to restrict minimum source size to that of the psf
c-----------------------------------------------------------------------
      implicit none
c
      logical do3val, do3pix, eqscale, mark, doerase, dofid,
     + dowedge, grid, nofit, asciiart, auto, negative, pbcor,
     + oldsfind, fdrimg, sigmaimg, rmsimg, normimg, kvannot,
     + fdrpeak, allpix, psfsize
cc
      integer maxopt
      parameter (maxopt = 22)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'3value  ', '3pixel  ', 'unequal ',
     +              'mark    ', 'noerase ', 'wedge   ',
     +              'fiddle  ', 'grid    ', 'nofit   ',
     +              'asciiart', 'auto    ', 'negative',
     +              'pbcorr  ', 'oldsfind', 'fdrimg  ',
     +              'sigmaimg', 'normimg ', 'kvannot ',
     +              'fdrpeak ', 'allpix  ', 'psfsize ',
     +              'rmsimg'/
c-----------------------------------------------------------------------
      call optcg ('options', opshuns, present, maxopt)
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
      if (auto.and.asciiart) then
       asciiart = .FALSE.
       call output('Auto mode: Asciiart option being overridden.')
      end if
c
      end
c
c
      subroutine inputs (maxlev, in, ibin, jbin, kbin, levtyp, slev,
     +   levs, nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix, 
     +   eqscale, nx, ny, cs, dopixel, mark, doerase, dowedge, dofid,
     +   grid, cut, rmsbox, alpha, xrms, nofit, asciiart, auto,
     +   negative, pbcor, oldsfind, sigmaimg, rmsimg, fdrimg, normimg,
     +   kvannot, fdrpeak, allpix, psfsize)
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
c   xrms       multiple of background rms above which source must be before
c              user is given option of saying yay or nay
c   nofit      True means don't do gaussian fitting for each source
c   asciiart   display ascii representations of each source during
c              interactive source selection
c   auto       true means skip all interactive sections of program, including
c              image display
c   negative   inverts image: positive pixels become negative and vice versa
c   pbcor      true to correct fluxes by primary beam attenuation
c   oldsfind   true if want to use sfind in original mode (no fdr)
c   fdrimg     true if want to create fdr-selected output pixel image
c   sigmaimg   true if want to create sigma-clipped output pixel image
c   rmsimg     true if want to create rms output pixel image
c   normimg    true if want to create 'normalised' output image
c   kvannot    true if want to output a kview annotation file for the
c              detected objects
c   fdrpeak    true if want to allow fitting of all source pixels, not just
c              those above the fdr threshold
c   allpix     true if want to use all pixels, not just monotonically
c              decreasing ones
c   psfsize    true to restrict minimum source size to that of the psf
c-----------------------------------------------------------------------
      implicit none
c
      integer maxlev, nx, ny, nlevs, ibin(2), jbin(2), kbin(2), rmsbox
      real levs(maxlev), pixr(2), cs(2), slev, cut, xrms, alpha
      character*(*) labtyp(2), in, pdev, trfun, levtyp
      logical do3val, do3pix, eqscale, dopixel, mark, doerase,
     + dowedge, dofid, grid, nofit, asciiart, auto, negative, pbcor,
     + oldsfind, fdrimg, sigmaimg, rmsimg, normimg, kvannot, fdrpeak,
     + allpix, psfsize
cc
      integer ntype, nlab, ntype2, nimtype
      parameter (ntype = 14, ntype2 = 3)
      character type(ntype)*6, imtype*7, type2(ntype2)*7
      data type  /'hms   ', 'dms   ', 'abspix', 'relpix', 
     +            'arcsec', 'arcmin', 'absghz', 'relghz', 
     +            'abskms', 'relkms', 'absnat', 'relnat', 
     +            'absdeg', 'reldeg'/
      data type2 /'contour', 'pixel', 'grey'/
c-----------------------------------------------------------------------
      call keyini
      call keyf ('in', in, ' ')
      if (in.eq.' ') call bug ('f', 'No image specified')
      call keymatch ('type', ntype2, type2, 1, imtype, nimtype)
      if (nimtype.eq.0) imtype = 'pixel'
      dopixel = .true.
      if (imtype.eq.'contour') dopixel = .false.
c
      call keyi ('xybin', ibin(1), 1)
      call keyi ('xybin', ibin(2), ibin(1))
      if (ibin(2).ne.1 .and. ibin(2).ne.ibin(1)) call bug ('f',
     +  'Non-unit x spatial averaging must be equal to increment')
      ibin(1) = max(ibin(1), 1)
      ibin(2) = max(ibin(2), 1)
c
      call keyi ('xybin', jbin(1), ibin(1))
      call keyi ('xybin', jbin(2), jbin(1))
      if (jbin(2).ne.1 .and. jbin(2).ne.jbin(1)) call bug ('f',
     +  'Non-unit y spatial averaging must be equal to increment')
      jbin(1) = max(jbin(1), 1)
      jbin(2) = max(jbin(2), 1)
c
      call keyi ('chan', kbin(1), 1) 
      call keyi ('chan', kbin(2), 1) 
      kbin(1) = max(kbin(1), 1)
      kbin(2) = max(kbin(2), 1)
c
      call keya ('slev', levtyp, 'a')
      call keyr ('slev', slev, 0.0)
      call lcase (levtyp)
      if (levtyp.ne.'p' .and. levtyp.ne.'a') call bug ('f',
     +   'Unrecognized contour level scale type; must be "p" or "a"')
c
      call mkeyr ('levs', levs, maxlev, nlevs)
c
      call keyr ('range', pixr(1), 0.0)
      call keyr ('range', pixr(2), 0.0)
      call keya ('range', trfun, 'lin')
      call lcase (trfun)
      if (dopixel) then
        if (trfun.ne.'lin' .and. trfun.ne.'log' .and. trfun.ne.'heq' 
     +      .and. trfun.ne.'sqr') call bug ('f', 
     +    'Unrecognized pixel map transfer function type')
      else
        trfun = ' '
      end if
c
      call decopt (do3val, do3pix, eqscale, mark, doerase, dowedge,
     +             dofid, grid, nofit, asciiart, auto, negative,
     +             pbcor, oldsfind, fdrimg, sigmaimg, rmsimg, normimg,
     +             kvannot, fdrpeak, allpix, psfsize)
      if (.not.dopixel) then
        dofid = .false.
        dowedge = .false.
      end if
c
      call keya ('device', pdev, ' ')
      if ((pdev.eq.' ').and.(.not.auto)) then
        call pgldev
        call bug ('f', 'A PGPLOT device must be given')
      end if
c
      call keymatch ('labtyp', ntype, type, 2, labtyp, nlab)
      if (nlab.eq.0) labtyp(1) = 'abspix'
      if (nlab.le.1) then
        labtyp(2) = labtyp(1)
        if (labtyp(1).eq.'hms') labtyp(2) = 'dms'
      end if
c
      if ( (index(labtyp(1),'lin').ne.0  .and. 
     +      index(labtyp(2),'lin').eq.0) .or.
     +     (index(labtyp(2),'lin').ne.0  .and. 
     +      index(labtyp(1),'lin').eq.0) ) then
        if (eqscale) call bug ('i', 
     +  'You might consider options=unequal with these axis types')
      end if
c
      call keyi ('nxy', nx, 0)
      call keyi ('nxy', ny, nx)
c
      call keyr ('csize', cs(1), 0.0)
      call keyr ('csize', cs(2), 0.0)
c
      call keyr('cutoff',cut,0.0)
      if (cut.lt.0.0)
     +  call bug('f','You must give a non-negative value for cutoff')
      call keyi('rmsbox',rmsbox,20)
      if (rmsbox.lt.20) then
        rmsbox = 20
        call bug('w','rmsbox must be at least 20. Setting it to 20 now')
      end if
      call keyr('alpha',alpha,2.0)
      if ((alpha.le.0.0).and.(.not.oldsfind))
     +  call bug('f','You must use a positive value for alpha.')
      call keyr('xrms',xrms,0.0)
      if (xrms.le.0.0) then
       if ((oldsfind).or.(sigmaimg)) then
        call bug('f','Invalid value for keyword "xrms"')
       end if
      end if
c
      end
c
c       
      subroutine setlgc (labcol, poscol, statcol, regcol)
c-----------------------------------------------------------------------
c     Set line graphics colours
c
c  OUtput
c    colour indices to use
c-----------------------------------------------------------------------
      implicit none
      integer labcol, poscol, statcol, regcol
cc
      integer bgcol
c-----------------------------------------------------------------------
c
c See if black or white background
c
      call bgcolcg (bgcol)
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
        call bug ('w', 'Non black/white background colour on device')
        labcol = 7
      end if
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
      subroutine region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +                   win, ngrps, grpbeg, ngrp)
c----------------------------------------------------------------------
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
c----------------------------------------------------------------------
      implicit none
c
      integer naxis, size(naxis), blc(*), trc(*), win(2), ngrp(*), 
     +  grpbeg(*), ngrps, ibin(2), jbin(2), kbin(2)
      character in*(*)
cc
      include 'maxdim.h'
      integer maxbox, i
      parameter (maxbox = 1024)
c
      integer boxes(maxbox)
c----------------------------------------------------------------------
      call boxinput ('region', in, boxes, maxbox)
      call boxset (boxes, naxis, size, 's')
      call keyfin
c
c Find hyper-rectangle surrounding region of interest
c
      call boxinfo (boxes, 3, blc, trc)
      do i = 1, min(3,naxis)
        blc(i) = max(1,blc(i))
        trc(i) = min(size(i),trc(i))
      end do
c
c Adjust spatial window to fit an integral number of bins and
c find size of binned window
c
      call winfidcg (size(1), 1, ibin, blc(1), trc(1), win(1))
      call winfidcg (size(2), 2, jbin, blc(2), trc(2), win(2))
c
c Find list of start channels and number of channels for each group
c of channels selected.
c
      call chnselcg (blc, trc, kbin, maxbox, boxes, ngrps, grpbeg, ngrp)
c
      end
c
c
      subroutine fitting (lIn, krng, nx, ny, image, rms, xpos, 
     +    xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj,
     +    amin, posa, posns, blc, bin, lx, my, bvol, bpap,
     +    bvolp, bmajp, bminp, nimage, boxsize, image2, pcut, fitok,
     +    usedpixels, meanimg, sgimg, fdrpeak, allpix, psfsize,
     +    dumcount)
c-----------------------------------------------------------------------
c  Do elliptical Gaussian fit to source. First the pixels to be used in
c  the fitting are selected on the basis that they are monotonically
c  decreasing away from the "central" bright pixel, then the gaussian
c  fit is done as per the task "imfit". If the size of the region within
c  which pixels were selected for the fit is smaller than 4 times the fwhm
c  of the major axis of the source, the procedure is iterated with a larger
c  region. If this procedure diverges, (detected by the increasing region
c  becoming larger than the image size), the source is ignored.
c
c  Once the fit is done the source parameters are recorded and passed back
c  to subroutine "search" for it to output as appropriate.
c
c  Input
c    nx,ny     Size of image
c    image     Image
c    nimage    Normalised Image (for masking info)
c    bmajp,bminp pixel size of major/minor axis of beam. Used for defining
c              'area' covered by beam, to set minimum number of FDR pixels
c              needed before fitting a source.
c    bpap      orientation of beam (in pixel coords).
c    blc, bin  image params used for switching between full image and binned
c              subimage pixels.
c    lx, my    integer pixel coord of brightest pixel in object. Used in
c              pixel selection routine.
c    bvol,     beam volume in world and pixel coords, resp.
c    bvolp
c    image2    Image of p-values for each pixel
c    pcut      cutoff p-value below which pixels are likely to be in source
c    fdrpeak   true to use all source pixels in fit, not just those <pcut
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
c    amaj, amin, posa axes (arcsec) and position angle (degrees) of object.
c    rms              rms of gaussian fit to selected pixels.
c    fitok            logical. False if the fit failed for any reason.
c    usedpixels       number of pixels used in the fit
c
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'
      include 'sfind.h'
      integer MAXVAR,n
      parameter(MAXVAR=20, n=1000)
c
      logical dofit, fitok, fdrpeak, fdrpeakdum, allpix, psfsize
      integer ifail1,ifail2,lIn, i, blc(2), bin(2), maxline
      integer k,m,nvar,lx,my, nx,ny, krng(2), nimage(nx,ny), fiterr
      integer boxsize,ipim,ip2im,xpixused(n),ypixused(n), usedpixels
      integer nfdrused, dumcount
      real image(nx,ny), meanimg(nx,ny), sgimg(nx,ny), dumm
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
      dumm = 0.
      nsrc = 1
      do i = 1,nsrc
        vflux(i) = .true.
        vl0(i) = .true.
        vm0(i) = .true.
        vfwhm1(i) = .true.
        vfwhm2(i) = .true.
        vpa(i) = .true.
        fwhm1(i) = 1.
        fwhm2(i) = 1.
        l0(i) = 0.
        m0(i) = 0.
      end do
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
      do i = 1,n
       xpixused(i) = 0
       ypixused(i) = 0
      end do
c
c  Load the data. Ie, choose the pixels in the region of the source to
c  be used in the fitting procedure, and encode them in a format ready
c  to do so.
c
      call LoadDat (m,nx,ny,xpos,ypos,blc,bin,image,boxsize,lx,my,
     +  nimage,maxline,image2,pcut,fitok,meml(ipim),meml(ip2im),
     +  xpixused,ypixused,meanimg,sgimg,nfdrused,fdrpeak,allpix)
      call memfree(ipim,(boxsize+1)*(boxsize+1),'l')
      call memfree(ip2im,(boxsize+1)*(boxsize+1),'l')
      if (.not.fitok) return
c reject source if number of FDR pixels used is less than 1/4 the
c area covered by beam, or no pixels at all were used.
      if (((float(nfdrused)).lt.(sqrt(bmajp*bminp*pi)/4.)).or.
     +       (m.eq.0)) then
c      if (m.eq.0) then
       fitok = .false.
       return
      end if
c
c  Convert the coordinates to pixels, and fill in defaults if necessary.
c
      call CoordFid(lIn,k,.true.)
      call GetEst
c make sure it's a 2-D gaussian
      gdim = 2
c
c  Pack the variables into an array.
c
      call PackVar(xvar,nvar,MAXVAR)
      dofit = nvar.gt.0
      if(.not.dofit) then
        fitok = .false.
        return
      end if
      if(nvar.ge.m) then
        fitok = .false.
        return
      end if
c
c  Do the fitting process.
c
      fiterr = 0
      call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
      call UPackVar(xvar,nvar)
      if(ifail2.eq.0)call UpackCov(covar,nvar)
      if(ifail1.ne.0) fiterr = 1
      if(ifail2.ne.ifail1) fiterr = 2
c
c If fit is much larger than number of pixels used, trying fitting again
c with more pixels to refine the fit. m is the number of used pixels.
c
      if ((m).lt.(pi*fwhm1(1)*fwhm2(1)/4)) then
         dumcount = dumcount+1
c allocate memory for slopearry and connct again
       call memalloc(ipim,(boxsize+1)*(boxsize+1),'l')
       call memalloc(ip2im,(boxsize+1)*(boxsize+1),'l')
c initialise x,ypixused
       do i = 1,n
        xpixused(i) = 0
        ypixused(i) = 0
       end do
c
c  Load the data again, using more pixels than last time, by
c temporarily setting the fdrpeak variable to true.
c
       fdrpeakdum = .true.
       call LoadDat (m,nx,ny,xpos,ypos,blc,bin,image,boxsize,lx,my,
     +  nimage,maxline,image2,pcut,fitok,meml(ipim),meml(ip2im),
     +  xpixused,ypixused,meanimg,sgimg,nfdrused,fdrpeakdum,allpix)
       call memfree(ipim,(boxsize+1)*(boxsize+1),'l')
       call memfree(ip2im,(boxsize+1)*(boxsize+1),'l')
       if (.not.fitok) return
c reject source if number of FDR pixels used is less than 1/4 the
c area covered by beam, or no pixels at all were used.
       if (((float(nfdrused)).lt.(sqrt(bmajp*bminp*pi)/4.)).or.
     +       (m.eq.0)) then
        fitok = .false.
        return
       end if
c
c  Convert the coordinates to pixels, and fill in defaults if necessary.
c
       call CoordFid(lIn,k,.true.)
       call GetEst
c make sure it's a 2-D gaussian
       gdim = 2
c
c  Pack the variables into an array.
c
       call PackVar(xvar,nvar,MAXVAR)
       dofit = nvar.gt.0
       if(.not.dofit) then
         fitok = .false.
         return
       end if
       if(nvar.ge.m) then
         fitok = .false.
         return
       end if
c
c  Do the fitting process.
c
       fiterr = 0
       call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
       call UPackVar(xvar,nvar)
       if(ifail2.eq.0)call UpackCov(covar,nvar)
       if(ifail1.ne.0) fiterr = 1
       if(ifail2.ne.ifail1) fiterr = 2
      end if

c
c If 'psfsize' selected, check object size and if smaller than
c psf, set to psf size and fit again for peak and position.
c
      if ((psfsize).and.((fwhm1(1).lt.bmajp).or.(fwhm2(1).lt.bminp)))
     +       then
       do i = 1,nsrc
         vflux(i) = .true.
         vl0(i) = .true.
         vm0(i) = .true.
         vfwhm1(i) = .false.
         vfwhm2(i) = .false.
         vpa(i) = .false.
         fwhm1(i) = bmajp
         fwhm2(i) = bminp
         pa(i) = bpap
       end do
       fitok = .true.
c  Pack the variables into an array.
       call PackVar(xvar,nvar,MAXVAR)
       dofit = nvar.gt.0
       if(.not.dofit) then
         fitok = .false.
         return
       end if
       if(nvar.ge.m) then
         fitok = .false.
         return
       end if
c  Do the fitting process.
       fiterr = 0
       call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
       call UPackVar(xvar,nvar)
       if(ifail2.eq.0)call UpackCov(covar,nvar)
       if(ifail1.ne.0) fiterr = 1
       if(ifail2.ne.ifail1) fiterr = 2
      end if
c
c  Check to see whether the fitted size of the image is comparable to the
c  initial box size used in selecting pixels, and if so iterate the
c  procedure choosing a larger box size.
c
      if (boxsize.lt.max(abs(fwhm1(1)),abs(fwhm2(1)))) then
c
c Checks for divergence - eg, noise being fitted by ever larger
c gaussians. Stops and aborts fit if object fwhm is larger than one quarter the
c smallest image dimension, or if the fitted fwhm has increased to more than
c 60 pixels. (At sizes > 60 pixels, the next iteration takes quite a while,
c and leaves a big wait for nothing if the object is then discarded on the
c next iteration.) This (moderately) arbitrary number is, however, an
c unsatisfactory way of dealing with the problem, and a better one would
c be appreciated.
c
       if (max(abs(fwhm1(1)),abs(fwhm2(1))).lt.min(nx,ny,240)/4) then
        boxsize = 3.0*max(abs(fwhm1(1)),abs(fwhm2(1)))
        goto 1200
       else
        fitok = .false.
        return
       end if
      end if
c Now we are confident source has been fit, it is ok to mess with the
c values in image2
      do i = 1,m
c jacking the pvalue of 'used' pixels up above the cutoff, so
c the same source can't be detected and fit multiple times
       image2(xpixused(i),ypixused(i)) = 1.e10
      end do
c record number of used pixels
      usedpixels = usedpixels + nfdrused
c
c  Convert to astronomical units, and report - ie, finalise results.
c
      call CoordFid(lIn,k,.false.)
      call Report (bvol, bvolp, xposerr, yposerr, 
     +   pkfl, pkflerr, intfl, amaj, amin, posa, posns, lIn)
c
      end
c
c
      subroutine CoordFid(lIn,k,topix)
c------------------------------------------------------------------------
c
c  Convert coordinates between world and pixel coordinates.
c
c  Input:
c    lIn      Handle of the coordinate system.
c    k
c    topix
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer lIn,k
      logical topix
      double precision in(3),out(3)
      double precision crpix(2),crval(2),cdelt(2)
      character ctype(2)*16
      real bmaj,bmin,bpa,dx,dy
      integer i
c------------------------------------------------------------------------
      do i=1,nsrc
c
c  Convert the position.
c
          in(1) = l0(i)
          in(2) = m0(i)
          in(3) = k
          if(topix)then
            call coCvt(lIn,'ow/ow/ap',in,'ap/ap/ap',out)
          else
            call coCvt(lIn,'ap/ap/ap',in,'ow/ow/ap',out)
          endif
          l0(i) = out(1)
          m0(i) = out(2)
c
c  Convert the gaussian parameters.
c
          if(topix)then
            call coGauCvt(lIn,'ow/ow/ap',in,
     +              'w',fwhm1(i),fwhm2(i),pa(i),'p',bmaj,bmin,bpa)
          else
            call coGauCvt(lIn,'ap/ap/ap',in,
     +              'p',fwhm1(i),fwhm2(i),pa(i),'w',bmaj,bmin,bpa)
          endif
c
c  Convert the uncertainties.
c
          sfwhm1(i) = bmaj / fwhm1(i) * sfwhm1(i)
          sfwhm2(i) = bmin / fwhm2(i) * sfwhm2(i)
          if(spa(i)+sl0(i)+sm0(i).gt.0)then
            if(topix)then
            call coLin(lIn,'ow/ow/ap',in,2,ctype,crpix,crval,cdelt)
            dx = 1/abs(cdelt(1))
            dy = 1/abs(cdelt(2))
            else
            call coLin(lIn,'ap/ap/ap',in,2,ctype,crpix,crval,cdelt)
            dx = abs(cdelt(1))
            dy = abs(cdelt(2))
            endif
            sl0(i) = sl0(i) * dx
            sm0(i) = sm0(i) * dy
            spa(i) = spa(i) / ( dy/dx*cos(pa(i))**2 + 
     +                          dx/dy*sin(pa(i))**2 )
          endif
c
          fwhm1(i) = bmaj
          fwhm2(i) = bmin
          pa(i)    = bpa
      enddo
c
      end
c
c
      subroutine GetEst
c------------------------------------------------------------------------
c  Generate an initial estimate for a single component model.
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      include 'mirconst.h'
c
      integer i
      double precision P,XP,YP,XYP,XXP,YYP,SP
      real t,fac
c------------------------------------------------------------------------
      SP = 0
      P = 0
      XP = 0
      YP = 0
      XYP = 0
      XXP = 0
      YYP = 0
c
      do i=1,ndata
        SP  = SP + data(i)
        t = abs(data(i))
        P   = P   + t
        XP  = XP  + t*x(i)
        YP  = YP  + t*y(i)
        XYP = XYP + t*x(i)*y(i)
        XXP = XXP + t*x(i)*x(i)
        YYP = YYP + t*y(i)*y(i)
      enddo
c
      fac = 4*log(2.)
      XP  = XP / P
      YP  = YP / P
      XYP = XYP / P - XP*YP
      XXP = XXP / P - XP*XP
      YYP = YYP / P - YP*YP
      l0(1) = XP
      m0(1) = YP
      fwhm1(1) = sqrt(fac*(XXP + YYP +
     +            sqrt( (XXP-YYP)**2 + 4*(XYP)**2 )))
      fwhm2(1) = sqrt(fac*(XXP + YYP -
     +            sqrt( (XXP-YYP)**2 + 4*(XYP)**2 )))
      pa(1) = 0.5*atan2(2*XYP,YYP-XXP)
      flux(1) = sign(fac * P / ( pi * fwhm1(1) * fwhm2(1) ),SP)
c
      end
c
      subroutine LoadDat(m,nx,ny,xpos,ypos,blc,bin,image,boxsize,
     +    lx,my,nimage,maxline,image2,pcut,fitok,slopearry,connct,
     +    xpixused,ypixused,meanimg,sgimg,nfdrused,fdrpeak,allpix)
c-----------------------------------------------------------------------
c  Load the relevant data for this plane. The relevant data are those pixels
c  to which to fit the elliptical gaussians. They are selected by
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
c    fdrpeak   true to use all source pixels in fit, not just those <pcut
c    allpix    true to use all pixels, not just monotonically decreasing ones
c  Output:
c    m         Number of points loaded
c    fitok     false if had problems
c    x,ypixused list of pixel positions to be 'blanked' if fit works,
c               to prevent multiple detections of the same source
c    nfdrused  number of FDR pixels used in the fit
c------------------------------------------------------------------------
      implicit none
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
      half = boxsize/2.
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
      end if
c make slopearry by testing slopey
c (and maybe whether the pixel is FDR-selected)
      do mm = mmn,mmx
       do ll = lmn,lmx
        i = ll-lmn+1
        j = mm-mmn+1
        if (allpix) then
         slopearry(i,j) = image2(ll,mm).le.pcut
        else
         call slopey(ll,mm,lx,my,nx,ny,image,slopearry(i,j))
c ensures that only pixels >= 1-sigma are used in fit
         if (slopearry(i,j)) then
          slopearry(i,j) = image(ll,mm).ge.sgimg(ll,mm)
         end if
c use this if *all* the pixels in the source are to be above the FDR
c threshold, not just the peak pixel.
         if (.not.fdrpeak) then
          if (slopearry(i,j)) slopearry(i,j) = image2(ll,mm).le.pcut
         end if
        end if
       end do
      end do
c make connct by testing annuli consecutively outward from peak pixel
c to check whether each pixel is connected to the peak pixel.
c start by initialising array, then defining peak pixel to be connected.
      do i = 1,boxsize+1
       do j = 1,boxsize+1
        connct(i,j) = .false.
       end do
      end do
      cenx = lx-lmn+1
      ceny = my-mmn+1
      connct(cenx,ceny) = .true.
c i is the 'annulus' index.
      do i = 1,boxsize/2
       sideln = 2*i+1
       do j = 1,sideln
c check top
        tstx = cenx + j - 1 - i
        if (tstx.lt.1) tstx = 1
        if (tstx.gt.boxsize) tstx = boxsize
        tsty = min(boxsize,ceny+i)
        if ((.not.connct(tstx,tsty)).and.(slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx+1,tsty-1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx-1,tsty-1)
         if ((j.gt.1).and.(j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx-1,tsty-1).or.
     +         connct(tstx,tsty-1).or.connct(tstx,tsty-1)
         end if
c check adjacent pixels
         if ((tstx.gt.1).and.(connct(tstx,tsty))) then
          connct(tstx-1,tsty) = slopearry(tstx-1,tsty)
         end if
         if ((tstx.lt.boxsize).and.(connct(tstx,tsty))) then
          connct(tstx+1,tsty) = slopearry(tstx+1,tsty)
         end if
        end if
c check bottom
        tstx = cenx + j - 1 - i
        if (tstx.lt.1) tstx = 1
        if (tstx.gt.boxsize) tstx = boxsize
        tsty = max(1,ceny-i)
        if ((.not.connct(tstx,tsty)).and.(slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx+1,tsty+1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx-1,tsty+1)
         if ((j.gt.1).and.(j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx-1,tsty+1).or.
     +         connct(tstx,tsty+1).or.connct(tstx,tsty+1)
         end if
c check adjacent pixels
         if ((tstx.gt.1).and.(connct(tstx,tsty))) then
          connct(tstx-1,tsty) = slopearry(tstx-1,tsty)
         end if
         if ((tstx.lt.boxsize).and.(connct(tstx,tsty))) then
          connct(tstx+1,tsty) = slopearry(tstx+1,tsty)
         end if
        end if
c check left side
        tstx = max(1,cenx-i)
        tsty = ceny + j - 1 - i
        if (tsty.lt.1) tsty = 1
        if (tsty.gt.boxsize) tsty = boxsize
        if ((.not.connct(tstx,tsty)).and.(slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx+1,tsty+1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx+1,tsty-1)
         if ((j.gt.1).and.(j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx+1,tsty-1).or.
     +         connct(tstx+1,tsty).or.connct(tstx+1,tsty+1)
         end if
c check adjacent pixels
         if ((tsty.gt.1).and.(connct(tstx,tsty))) then
          connct(tstx,tsty-1) = slopearry(tstx,tsty-1)
         end if
         if ((tsty.lt.boxsize).and.(connct(tstx,tsty))) then
          connct(tstx,tsty+1) = slopearry(tstx,tsty+1)
         end if
        end if
c check right side
        tstx = min(boxsize,cenx+i)
        tsty = ceny + j - 1 - i
        if (tsty.lt.1) tsty = 1
        if (tsty.gt.boxsize) tsty = boxsize
        if ((.not.connct(tstx,tsty)).and.(slopearry(tstx,tsty))) then
         if (j.eq.1) connct(tstx,tsty) = connct(tstx-1,tsty+1)
         if (j.eq.sideln) connct(tstx,tsty) = connct(tstx-1,tsty-1)
         if ((j.gt.1).and.(j.lt.sideln)) then
          connct(tstx,tsty) = connct(tstx-1,tsty-1).or.
     +         connct(tstx-1,tsty).or.connct(tstx-1,tsty+1)
         end if
c check adjacent pixels
         if ((tsty.gt.1).and.(connct(tstx,tsty))) then
          connct(tstx,tsty-1) = slopearry(tstx,tsty-1)
         end if
         if ((tsty.lt.boxsize).and.(connct(tstx,tsty))) then
          connct(tstx,tsty+1) = slopearry(tstx,tsty+1)
         end if
        end if
       end do
      end do
cc
cc for troubleshooting - write out contents of connct in pseudo-'asciiart'
cc type format.
cc
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
c Choose only those pixels within a *circle* of radius 'boxsize/2' which are
c also fainter than all the pixels between themselves and the brightest
c pixel and which aren't blanked.
c Store pixel values of object and surrounding region in linear array, data,
c and fill out the x,y, coordinate.
c
      i = 0
      xt = 0
      yt = 0
      nfdrused = 0
      do mm = mmn,mmx
       do ll = lmx,lmn,-1
        incirc = ((ll-xpos)**2 + (mm-ypos)**2).lt.(0.25*boxsize**2)
        fainter = connct(ll-lmn+1,mm-mmn+1)
        if (incirc.and.fainter.and.(nimage(ll,mm).gt.0)) then
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
        end if
       end do
      end do
      ndata = i
      m = ndata
      xoff = xt / real(ndata)
      yoff = yt / real(ndata)
c
      end
 
c
      subroutine slopey(xx,yy,xc,yc,nx,ny,image,slpdwn)
c-----------------------------------------------------------------------------
c this subroutine checks that a given pixel is less than all the pixels
c between it and the central (brightest) pixel.
c
c-----------------------------------------------------------------------------
      integer xx,yy,xc,yc,nx,ny,closerx,closery
      integer furtherx,furthery,pp,qq,stepl,stepm
      real image(nx,ny)
      logical slpdwn
c-----------------------------------------------------------------------------
      if (xx.gt.xc) then
       closerx = xx-1
       furtherx = xx+1
       stepl = -1
      end if
      if (xx.lt.xc) then
       closerx = xx+1
       furtherx = xx-1
       stepl = 1
      end if
      if (xx.eq.xc) then
       closerx = xx
       furtherx = xx
       stepl = 0
      end if
      if (yy.gt.yc) then
       closery = yy-1
       furthery = yy+1
       stepm = -1
      end if
      if (yy.lt.yc) then
       closery = yy+1
       furthery = yy-1
       stepm = 1
      end if
      if (yy.eq.yc) then
       closery = yy
       furthery = yy
       stepm = 0
      end if
c
c Checking to see if the closer/further pixels lie outside the bounds of the
c image, and fixing them if so.
c
      if (closerx.gt.nx)  closerx = nx
      if (closery.gt.ny)  closery = ny
      if (closerx.lt.1)   closerx = 1
      if (closery.lt.1)   closery = 1
      if (furtherx.gt.nx) furtherx = nx
      if (furthery.gt.ny) furthery = ny
      if (furtherx.lt.1)  furtherx = 1
      if (furthery.lt.1)  furthery = 1
      slpdwn = (image(xx,yy).le.image(closerx,closery)).and.
     +          (image(xx,yy).ge.image(furtherx,furthery))
      if ((yy.ne.yc).and.(xx.ne.xc).and.slpdwn) then
       do qq = closery,yc,stepm
        do pp = closerx,xc,stepl
         slpdwn = slpdwn.and.(image(xx,yy).le.image(pp,qq))
        end do
       end do
      end if
c
      end
c
c
      subroutine PackVar(var,nvar,MAXVAR)
c-----------------------------------------------------------------------
c
c  Store all the things that we need to vary.
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer nvar,MAXVAR
      real var(MAXVAR)
      integer i,j,ncurr
      real tmp(6)
c-----------------------------------------------------------------------
      nvar = 0
      do i=1,nsrc
        ncurr = 0
        if(vflux(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = flux(i)
        endif
        if(vl0(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = l0(i) - xoff
        endif
        if(vm0(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = m0(i) - yoff
        endif
        if(vfwhm1(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = fwhm1(i)
        endif
        if(vfwhm2(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = fwhm2(i)
        endif
        if(vpa(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = pa(i)
        endif
c
c  Copy the estimates to the variables.
c
        if(nvar+ncurr.gt.MAXVAR)
     +        call bug('f','Too many free parameters')
        do j=1,ncurr
          nvar = nvar + 1
          var(nvar) = tmp(j)
        enddo
c
      enddo
c
      end
c
c
      subroutine UpackCov(covar,nvar)
c------------------------------------------------------------------------
c
c  Unpack the covariance matrix.
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer nvar
      real covar(nvar,nvar)
      integer i,n
c------------------------------------------------------------------------
      n = 0
      do i=1,nsrc
        if(vflux(i))then
          n = n + 1
          sflux(i) = covar(n,n)
        endif
        if(vl0(i))then
          n = n + 1
          sl0(i) = covar(n,n)
        endif
        if(vm0(i))then
          n = n + 1
          sm0(i) = covar(n,n)
        endif
        if(vfwhm1(i))then
          n = n + 1
          sfwhm1(i) = covar(n,n)
        endif
        if(vfwhm2(i))then
          n = n + 1
          sfwhm2(i) = covar(n,n)
        endif
        if(vpa(i))then
          n = n + 1
          spa(i) = covar(n,n)
        endif
      enddo
c
      if(n.ne.nvar)
     +        call bug('f','Inconsistency in UPackCov')
c
      end
c
c
      subroutine UPackVar(var,nvar)
c------------------------------------------------------------------------
c
c  Unpack all the things that we need to vary.
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer nvar
      real var(nvar)
      integer i,n
c------------------------------------------------------------------------
      n = 0
      do i=1,nsrc
        if(vflux(i))then
          n = n + 1
          flux(i) = var(n)
        endif
        if(vl0(i))then
          n = n + 1
          l0(i) = var(n) + xoff
        endif
        if(vm0(i))then
          n = n + 1
          m0(i) = var(n) + yoff
        endif
        if(vfwhm1(i))then
          n = n + 1
          fwhm1(i) = var(n)
        endif
        if(vfwhm2(i))then
          n = n + 1
          fwhm2(i) = var(n)
        endif
        if(vpa(i))then
          n = n + 1
          pa(i) = var(n)
        endif
      enddo
c
      if(n.ne.nvar)
     +        call bug('f','Inconsistency in UPackVar')
c
      end
c
c
      subroutine FUNCTION(m,nvar,var,fvec,iflag)
c------------------------------------------------------------------------
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer m,nvar,iflag
      real var(nvar)
      real fvec(m)
      integer i
c------------------------------------------------------------------------
      if(m.ne.ndata)call bug('f','Inconsistency in FUNCTION')
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
      end if
c
c  Compute the residuals now.
c
      do i=1,m
       if (gdim.eq.1) then
        model2(i) = fvec(i)
        fvec(i) = data2(i) - fvec(i)
       else
        fvec(i) = data(i) - fvec(i)
       end if
      enddo
c
      end      
c
c
      subroutine Eval1(x0,Model,n)
c-------------------------------------------------------------------------
c
c  Evaluate the current model at some pixels.
c
c  Input:
c    n            Number of points.
c    x0         Pixel coordinates at which to evaluate the model.
c  Output:
c    model      The evaluated model.
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer n
      real Model(n)
      real x0(n)
c      integer x0(n)
      integer i,j
      real t,xx,xscal
c-------------------------------------------------------------------------
c  Set the model to zero initially.
c
      do i=1,n
        model(i) = 0
      enddo
c
c  Loop over the various model types.
c
      do j=1,nsrc
c
c  Gaussian component.
c
          xscal = 4*log(2.)/(m0(j)**2)
          do i=1,n
            xx = x0(i) - l0(j)
            t = xscal*(xx*xx)
            if(t.lt.70)model(i) = model(i) + flux(j) * exp(-t)
          enddo
      enddo
c
      end
c
c
      subroutine Eval(x0,y0,Model,n)
c-------------------------------------------------------------------------
c
c  Evaluate the current model at some pixels.
c
c  Input:
c    n            Number of points.
c    x0,y0      Pixel coordinates at which to evaluate the model.
c  Output:
c    model      The evaluated model.
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer n
      real Model(n)
      real x0(n),y0(n)
c      integer x0(n),y0(n)
      integer i,j
      real cospa,sinpa,t,xx,yy,xp,yp,xscal,yscal
c-------------------------------------------------------------------------
c  Set the model to zero initially.
c
      do i=1,n
        model(i) = 0
      enddo
c
c  Loop over the various model types.
c
      do j=1,nsrc
c
c  Gaussian component.
c
          cospa = cos(pa(j))
          sinpa = sin(pa(j))
          xscal = 4*log(2.)/(fwhm2(j)*fwhm2(j))
          yscal = 4*log(2.)/(fwhm1(j)*fwhm1(j))
          do i=1,n
            xx = x0(i) - l0(j)
            yy = y0(i) - m0(j)
            yp =  yy*cospa + xx*sinpa
            xp = -yy*sinpa + xx*cospa
            t = xscal*(xp*xp) + yscal*(yp*yp)
            if(t.lt.70)model(i) = model(i) + flux(j) * exp(-t)
          enddo
      enddo
c
      end
c
c
      subroutine Report(bvol,bvolp,xposerr,yposerr,pkfl,pkflerr,
     +                    intfl,amaj,amin,posa,posns,lIn)
c-----------------------------------------------------------------------------
c  Report on the source component solution.
c
c  Input:
c    lIn            handle of image
c  Output:
c    posns          x and y positions in abs. world coords (radians)
c    xposerr        xposition error.
c    yposerr        yposition error.
c    pkfl, pkflerr  peak flux density of source, and its error.
c    intfl          integrated flux density of the source
c    amaj, amin     major and minor fwhm of source (arcsec)
c    posa           position angle of source (degrees E of N)
c------------------------------------------------------------------------
      implicit none
      include 'mirconst.h'
      include 'sfind.h'
c
      integer lin
      real bvol,bvolp
      real f1,f2,p,sf1,sf2,sp,tflux,sfac
        real xposerr, yposerr, pkfl, pkflerr, intfl
        real amaj, amin, posa
        double precision posns(2),newpos(2)
c-----------------------------------------------------------------------------
      if(bvolp.gt.0)then
        sfac = sqrt(bvolp)
      else
        sfac = 1
      endif
c
        pkfl = 1000.*flux(1)
        pkflerr = 1000.*sfac*sflux(1)
      if(bvol.gt.0.)then
        tflux = flux(1) * pi/4 * abs(fwhm1(1)) * abs(fwhm2(1))
        tflux = tflux / log(2.0)
        tflux = tflux / bvol
          intfl = 1000.*tflux
        else
          intfl = 0.
      endif
      posns(1) = l0(1)
      posns(2) = m0(1)
      call coCvt(lIn,'ow/ow',posns,'aw/aw',newpos)
      posns(1) = newpos(1)
      posns(2) = newpos(2)
      xposerr =3600*180/pi*sfac*sl0(1)
      yposerr =3600*180/pi*sfac*sm0(1)
      call GauFid(fwhm1(1),fwhm2(1),sfac*sfwhm1(1),
     +            sfac*sfwhm2(1),pa(1),sfac*spa(1),f1,f2,sf1,sf2,p,sp)
        amaj = f1
        amin = f2
        posa = p
c
      end
c
c
      subroutine GauFid(fwhm1,fwhm2,sfwhm1,sfwhm2,pa,spa,f1,f2,
     +                                          sf1,sf2,p,sp)
c-----------------------------------------------------------------------------
c
c  Convert the gaussian parameters to arcsec.
c------------------------------------------------------------------------
      implicit none
      include 'mirconst.h'
      real fwhm1,fwhm2,pa,f1,f2,p,sfwhm1,sfwhm2,spa,sf1,sf2,sp
      real t
c----------------------------------------------------------------------------
      f1 = 3600*180/pi * abs(fwhm1)
      f2 = 3600*180/pi * abs(fwhm2)
      sf1 = 3600*180/pi * sfwhm1
      sf2 = 3600*180/pi * sfwhm2
      p = 180./pi * pa
      sp = 180./pi * spa
      if(f1.lt.f2)then
        t = f1
        f1 = f2
        f2 = t
        t = sf1
        sf1 = sf2
        sf2 = t
        p = p + 90
      endif
      p = mod(p,180.)
      if(p.lt.-90) p = p + 180
      if(p.gt. 90) p = p - 180
      end
c
c
      subroutine BeamPar(lIn,k,bvol,bvolp,bmaj,bmin,bpa,bmajp,bminp,
     +                   bpap,nobeam)
c-----------------------------------------------------------------------
c  Get things dealing with units.
c
c  Input:
c    lIn      Handle of the input dataset.
c    k            Plane of interest.
c  Output:
c    bvol      Beam volume, in radians**2. Set to zero if this cannot
c            be determined.
c    bvolp      Beam volume in pixels.
c    bmaj,bmin,bpa Beam major, minor axes and position angle.
c    bmajp,bminp,bpap Beam major, minor axes and position angle in pix. coords.
c------------------------------------------------------------------------
      implicit none
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
      call rdhdr(lIn,'bmaj',bmaj,0.)
      call rdhdr(lIn,'bmin',bmin,0.)
      call rdhdr(lIn,'bpa',bpa,0.)
      bpa = pi/180 * bpa
        if ((bmaj.eq.0.).or.(bmin.eq.0.)) then
         call bug('w','Beam not detected in map -')
         call bug('w','Using arbitrary value of 5 pixels.')
         nobeam = .true.
         bvol = 0.
         bvolp = 0.
         goto 1111
        end if
c
      if(bmaj*bmin.gt.0)then
        x(1) = 0
        x(2) = 0
        x(3) = 0
        call coGauCvt(lIn,'op/op/op',x,'w',bmaj,bmin,bpa,
     +                               'p',bmajp,bminp,bpap)
        x(3) = k
        call coGauCvt(lIn,'op/op/ap',x,'p',bmajp,bminp,bpap,
     +                               'w',bmaj,bmin,bpa)
      endif
c
c  Determine the beam value, in radians**2
c
      call rdhda(lIn,'bunit',bunit,' ')
      call ucase(bunit)
      if(index(bunit,'/PIXEL').ne.0)then
        x(1) = 0
        x(2) = 0
        x(3) = k
        call coLin(lIn,'op/op/ap',x,2,ctype,crpix,crval,cdelt)
        bvol = abs(cdelt(1)*cdelt(2))
        bvolp = 1
      else if(index(bunit,'/BEAM').ne.0.and.bmaj*bmin.gt.0)then
        bvol = pi/4/log(2.0)*bmaj*bmin
        bvolp = pi/4/log(2.0)*bmajp*bminp
      else
        bvol = 0
        bvolp = 0
      endif
c
1111    continue
      end


      subroutine fdr(nx,ny,l,m,boxsize,image,nimage,alpha,xrms,
     +           bmajp,bminp,pcut,lin,plist,image2,meanimg,sgimg,
     +           fdrimg,sigmaimg,rmsimg,normimg,auto,blc,bin,
     +           maxx,maxy,nfdrpix)
c-----------------------------------------------------------------------
c Performs FDR statistical analysis of pixels in region of source.
c 1. Assigns a P-value to each pixel, based on its intensity
c    (given by P=1-gaussianpdf(image(p,q)) for pixel p,q).
c 2. Orders the list of P-values.
c 3. Using alpha, finds the P-value corresponding to the cutoff, which
c    is the point at which a line of slope alpha/log(Npix) intercepts the
c    ordered list of P-values.
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
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'
      integer ii,jj,lmn,lmx,mmn,mmx,nx,ny,l,m,npix
      integer nn,lin,maxx,maxy
      parameter (nn=20000)
      real sigma, xrms, alpha
      real image(nx,ny),pvalue,pline
      real plist(nx*ny),pcut
      real bmajp,bminp,bareap,ee,fdrdenom
      real errfun,base0
      integer boxsize, blc(2),bin(2)
      integer nimage(nx,ny)
c
      integer lOut,lOut2,lOut3,lOut4,lo,axes(2),nfdrpix,nblanks
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
c check if input image has a mask
      mskexst = hdprsnt(lin,'mask')
c
c open output 'normalised' and 'segmentation image' datasets if necessary
c
      axes(1) = maxx
      axes(2) = maxy
      if (fdrimg) call xyopen (lOut,'sfind.fdr','new',2,axes)
      if (sigmaimg) call xyopen (lOut2,'sfind.sig','new',2,axes)
      if (normimg) call xyopen (lOut3,'sfind.norm','new',2,axes)
      if (rmsimg) call xyopen (lOut4,'sfind.rms','new',2,axes)
c copy headers to output datasets if necessary
      do ii = 1,4
       if ((ii.eq.1).and.(fdrimg)) then
        lo = lOut
       else if ((ii.eq.2).and.(sigmaimg)) then
        lo = lOut2
       else if ((ii.eq.3).and.(normimg)) then
        lo = lOut3
       else if ((ii.eq.4).and.(rmsimg)) then
        lo = lOut4
       else
        lo = -1
       end if
       if (lo.gt.0) then
        call hdcopy (lIn,lo,'observer')
        call hdcopy (lIn,lo,'telescop')
        call hdcopy (lIn,lo,'object')
        call hdcopy (lIn,lo,'ctype1')
        call hdcopy (lIn,lo,'ctype2')
        call hdcopy (lIn,lo,'crval1')
        call hdcopy (lIn,lo,'crval2')
        call hdcopy (lIn,lo,'cdelt1')
        call hdcopy (lIn,lo,'cdelt2')
        call hdcopy (lIn,lo,'crpix1')
        call hdcopy (lIn,lo,'crpix2')
        call hdcopy (lIn,lo,'epoch')
        call hdcopy (lIn,lo,'obstime')
        call hdcopy (lIn,lo,'bunit')
        call hdcopy (lIn,lo,'btype')
        call hdcopy (lIn,lo,'bmaj')
        call hdcopy (lIn,lo,'bmin')
        call hdcopy (lIn,lo,'bpa')
        call hdcopy (lIn,lo,'history')
        if (mskexst) then
         do jj = 1,axes(2)
          call xyflgrd(lin,jj,msk)
          call xyflgwr(lo,jj,msk)
         end do
        end if
c fill in new images with zero data to start
        do jjj = 1,maxy
         do iii = 1,maxx
          rrow(iii) = 0.
         end do
         call xywrite(lo,jjj,rrow)
        end do
       end if
      end do
      if (normimg) call wrhda(lOut3,'bunit','SIGMA')

c catch possible problem of boxsize (rmsbox input) is bigger than image
      boxsize = min(boxsize,nx,ny)
c allocate memory for mask and image2 array in basecal
      call memalloc(ipim,(boxsize+1)*(boxsize+1),'l')
      call memalloc(ip2im,(boxsize+1)*(boxsize+1),'r')
c
c "Normalise" image, by determining mean and sigma in boxes of size boxsize,
c and subtracting mean and dividing by sigma, to make the fdr stuff work
c for images with images where sigma varies significantly over the image
c
      do ii = 1,nint(float(nx)/float(boxsize))+1
       do jj = 1,nint(float(ny)/float(boxsize))+1
        l = boxsize/2 + (ii-1)*boxsize
        m = boxsize/2 + (jj-1)*boxsize
        if ((l.le.(nx+boxsize/2)).and.(m.le.(ny+boxsize/2))) then
         call basecal(nx, ny, l, m, base0, sigma, boxsize, image,
     +           nimage, 1.5*bmajp, ok, meml(ipim),memr(ip2im), auto)
         if (ok) then
          lmn = max(0,(l-boxsize/2)) + 1
          lmx = min(nx,(lmn+boxsize-1))
          mmn = max(0,(m-boxsize/2)) + 1
          mmx = min(ny,(mmn+boxsize-1))
          do iii = lmn,lmx
           do jjj = mmn,mmx
            if (sigma.ne.0) then
             image2(iii,jjj) = (image(iii,jjj) - base0)/sigma
             meanimg(iii,jjj) = base0
             sgimg(iii,jjj) = sigma
            end if
           end do
          end do
         end if
        end if
       end do
      end do
c
c free memory from basecal arrays
      call memfree(ip2im,(boxsize+1)*(boxsize+1),'r')
      call memfree(ipim,(boxsize+1)*(boxsize+1),'l')
c
c make normalised image from image2 if required
c
      if (normimg) then
       do jjj = 1,ny
        do iii = 1,nx
c cvt iii from binned subimage pixels to full image pixels
         wa(1) = dble(iii)
         call ppconcg(2, blc(1), bin(1), wa(1))
         rrow(nint(wa(1))) = image2(iii,jjj)
        end do
c cvt jjj from binned subimage pixels to full image pixels
        wa(2) = dble(jjj)
        call ppconcg(2, blc(2), bin(2), wa(2))
        call xywrite(lOut3,nint(wa(2)),rrow)
       end do
       call xyclose (lOut3)
      end if
c
c make rms image, for calculation of weighting corrections
c
      if (rmsimg) then
       nfdrpix = 0
       nblanks = 0
       do m = 1,ny
        do l = 1,nx
c cvt l from binned subimage pixels to full image pixels
         wa(1) = dble(l)
         call ppconcg(2, blc(1), bin(1), wa(1))
         if (nimage(l,m).gt.0) then
          rrow(nint(wa(1))) = sgimg(l,m)
         else
          rrow(nint(wa(1))) = 0.
         end if
        end do
c ok, have written the m'th row, now put it in the segmentation image
c cvt m from binned subimage pixels to full image pixels
        wa(2) = dble(m)
        call ppconcg(2, blc(2), bin(2), wa(2))
        call xywrite(lOut4,nint(wa(2)),rrow)
       end do
c done writing to lOut4 - so close it.
       call xyclose (lOut4)
      end if

c
c make sigma-cut image, for comparison with the upcoming fdr-based
c segmentation image - note, after above normalisation, sigma = 1
c
      if (sigmaimg) then
       nfdrpix = 0
       nblanks = 0
       do m = 1,ny
        do l = 1,nx
c cvt l from binned subimage pixels to full image pixels
         wa(1) = dble(l)
         call ppconcg(2, blc(1), bin(1), wa(1))
         if (nimage(l,m).gt.0) then
          if (image2(l,m).lt.xrms) then
           rrow(nint(wa(1))) = 0.
          else
           nfdrpix = nfdrpix + 1
           rrow(nint(wa(1))) = 100.
          end if
         else
          rrow(nint(wa(1))) = 0.
          nblanks = nblanks + 1
         end if
        end do
c ok, have written the m'th row, now put it in the segmentation image
c cvt m from binned subimage pixels to full image pixels
        wa(2) = dble(m)
        call ppconcg(2, blc(2), bin(2), wa(2))
        call xywrite(lOut2,nint(wa(2)),rrow)
       end do
c done writing to lOut2 - so close it.
       call xyclose (lOut2)
       write(line,'("Of a total of ",i7," non-blanked pixels,")')
     +        nx*ny - nblanks
       call output(line)
       write(line,'(f5.1,"-sigma cut gives ",i7," pixels.")')
     +               xrms,nfdrpix
       call output(line)
      end if

c
c Now (finally) to the actual FDR bit...
c
      lmn = 1
      lmx = nx
      mmn = 1
      mmx = ny
c calculate area covered by beam, in pixels
      bareap = bmajp*bminp*pi/4./log(2.)
c ee is constant e.
      ee = exp(1.)
c
      npix = 0

c Don't have to manually define gaussian PDF since I discovered
c how to do it using a call to errfun(x). Doing it my old way is
c seriously much slower, but kept here commented out for historical
c reasons.
cc defining gaussianpdf for lookup table, setting mean and sigma to 0 and 1
cc since have 'normalised' the image above
c      mean = 0
c      sigma = 1
c      xmin = mean - 30.*sigma
c      xmax = mean + 30.*sigma
cc filling in a gaussian with area=1
c      dx = (xmax - xmin)/(nn-1)
c      do ii = 1,nn/2
c       gsabsc(ii) = xmin + (xmax-xmin)*(ii-1)/(nn-1)
c       gsord(ii) = exp(-0.5*(gsabsc(ii)-mean)**2/sigma**2)/
c     +              sqrt(2.*pi*sigma**2)
cc integrate
c       if (ii.gt.1) then
c        inty(ii) = inty(ii-1) + gsord(ii)*dx
c       else
c        inty(ii) = gsord(ii)*dx
c       end if
c       gsabsc(nn-ii+1) = xmax - (xmax-xmin)*(ii-1)/(nn-1)
c       gsord(nn-ii+1) = exp(-0.5*(gsabsc(nn-ii+1)-mean)**2/sigma**2)/
c     +              sqrt(2.*pi*sigma**2)
cc integrate
c       inty(nn-ii+1) = 1.-inty(ii)
c      end do

c looping over pixels assigning pvalues - the actual FDR bit
      do ii = lmn,lmx
       do jj = mmn,mmx
        if (nimage(ii,jj).ne.0) then
cc extreme values
c         if (image2(ii,jj).gt.xmax) then
c          pvalue = 1.
c         else if (image2(ii,jj).lt.xmin) then
c          pvalue = 0.
c         else
c          call gaussianpdf(nn,gsabsc,gsord,inty,image2(ii,jj),pvalue)
c         end if
c Note: Gaussian Probability Distribution Function (GPDF) is related to
c the error function erf(x) by GPDF(x) = 0.5(1+erf(x/sqrt(2)))
         pvalue = 0.5*(1+errfun(image2(ii,jj)/sqrt(2.)))
         image2(ii,jj) = 1. - pvalue
         npix = npix + 1
         plist(npix) = 1. - pvalue
        end if
       end do
      end do
c sort plist into order
      call sortr(plist,npix)
c fdrdenom is the denominator of the slope (alpha/fdrdenom) for the fdr line
c defined this way, it runs from 1 to sum_i=1^N (1/i), for
c bareap=1 (uncorrelated) to N (fully correlated)
      if (bareap.lt.1.) then
       fdrdenom = 1.
      else if (bareap.gt.float(npix)) then
       fdrdenom = 0
       do ii = 1,npix
        fdrdenom = fdrdenom + 1/float(ii)
       end do
      else
       fdrdenom = 0
       do ii = 1,int(bareap)
        fdrdenom = fdrdenom + 1/float(ii)
       end do
      end if
c find crossing point
      gotit = .false.
      pcut = 0.
c This is not approved miriad procedure - used by AMH for quick testing only
c      open(1,file='tt',status='unknown')
      do ii = npix,1,-1
       pline = (alpha/(100.*fdrdenom))
c       pline = (alpha/(100.*log(float(npix))))
c       pline = (alpha/100.)
     +              *(float(ii)/float(npix))
c This is not approved miriad procedure - used by AMH for quick testing only
c       write(1,*) float(ii)/float(npix),pline,plist(ii)
       if ((pline.ge.plist(ii)).and.(.not.gotit)) then
        pcut = pline
        gotit = .true.
       end if
      end do
c This is not approved miriad procedure - used by AMH for quick testing only
c      close(1)
c
c Loop over all pixels to test whether image2(l,m)<P_cut.
c If image2(l,m) > pcut, then pixel is most likely background,
c and if not, store it in the 'segmentation image' if required
c
      nfdrpix = 0
      fluxctoff = 1.e9
      sigctoff = 1.e9
      do m = 1,ny
       do l = 1,nx
c cvt l from binned subimage pixels to full image pixels
        wa(1) = dble(l)
        call ppconcg(2, blc(1), bin(1), wa(1))
        if (nimage(l,m).gt.0) then
         if (image2(l,m).gt.pcut) then
          rrow(nint(wa(1))) = 0.
         else
          fluxctoff=min(fluxctoff,image(l,m))
          sigctoff=min(sigctoff,(image(l,m)-meanimg(l,m))/sgimg(l,m))
          nfdrpix = nfdrpix + 1
          rrow(nint(wa(1))) = 100.
         end if
        else
         rrow(nint(wa(1))) = 0.
        end if
       end do
c ok, have written the m'th row, now put it in the segmentation image
       if (fdrimg) then
c cvt m from binned subimage pixels to full image pixels
        wa(2) = dble(m)
        call ppconcg(2, blc(2), bin(2), wa(2))
        call xywrite(lOut,nint(wa(2)),rrow)
       end if
      end do
c done writing to lOut - so close it, if it was open.
      if (fdrimg) call xyclose (lOut)
c
      if (.not.sigmaimg) then
       write(line,'("Of a total of ",i7," non-blanked pixels,")')
     +        nx*ny - nblanks
      end if
      call output(' ')
      write(line,'(a,f16.10,a)')
     +  'FDR selected a p-value threshold of ',pcut,'.'
      call output(line)
      write(line,'(a,f7.2,a)')
     +  'This corresponds to a threshold of ', sigctoff, ' sigma,'
      call output(line)
      write(line,'(a,f16.10,a)') 
     +  'which means a minimum flux threshold of ',fluxctoff,' Jy.'
      call output(line)
      write(line,'(a)')
     +  '(If the noise is constant over the original image,'
      call output(line)
      write(line,'(a)')
     +  'this is the threshold over the whole image.)'
      call output(line)
      write(line,'("FDR detected ",i7," pixels.")') nfdrpix
      call output(line)
c
      return
      end


      subroutine fdrfit(nx,ny,image,nimage,bvol,bpap,bvolp,
     +            bmajp,bminp,pcut,image2,lin,krng,blc,bin,negative,
     +            pbcor,llog,kvannot,fdrpeak,allpix,psfsize,alpha,
     +            nfdrpix,meanimg,sgimg)
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
c    allpix     true to use all pixels, not just monotonically decreasing ones
c    psfsize    true to restrict minimum source size to that of the psf
c    alpha      FDR percentage of false pixel detections
c    nfdrpix    number of pixels detected by FDR
c    meanimg    image of background mean values
c    sgimg      image of background sigma values
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'mirconst.h'
      include 'sfind.h'
c
      integer ii,jj,kk,nx,ny,l,m
      integer lin,llog,lann,krng(2),len1
      real image(nx,ny),pcut,alpha
      real bvol,bpap,bvolp,bmajp,bminp
      integer boxsize, nfdrpix, dumcount
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
c----------------------------------------------------------------------
      call output(' ')
      call output('********************************')
      call output('Beginning source measurements...')
      call output('********************************')
      sources = 0
      dumcount = 0
      if (kvannot) then
       call txtopen (lann, 'sfind.ann', 'append', iostat)
       if (iostat.ne.0) 
     +  call bug ('f', 'Error opening text file "sfind.ann"')
       write(line,'("color green")')
       call txtwrite (lann, line, len1(line), iostat)
      end if
c
      usedpixels = 0
      do l = 1,nx
       do m = 1,ny
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
          if ((image(imin,jmin).le.image(ii,jj)).and.
     +       (image(imin,jj).le.image(ii,jj)).and.
     +       (image(imin,jmax).le.image(ii,jj)).and.
     +       (image(ii,jmin).le.image(ii,jj)).and.
     +       (image(ii,jmax).le.image(ii,jj)).and.
     +       (image(imax,jmin).le.image(ii,jj)).and.
     +       (image(imax,jj).le.image(ii,jj)).and.
     +       (image(imax,jmax).le.image(ii,jj))) then
           atpeak = .true.
          else
           do kk = imin,imax
            do pp = jmin,jmax
             if ((kk.ne.ii).or.(pp.ne.jj)) then
              if (image(kk,pp).gt.image(ii,jj)) then
               ii = kk
               jj = pp
              end if
             end if
            end do
           end do
          end if
          if (.not.atpeak) goto 230
c test increasing annuli around source to find size of region to
c be sent to fitting routine
          off = 1
233       blnkannulus = .true.
          do iii = ii-off,ii+off
           if (((jj-off).ge.1).and.(iii.ge.1).and.(iii.le.nx)) then
            if (nimage(iii,jj-off).gt.0)
     +      blnkannulus = blnkannulus.and.(image2(iii,jj-off).gt.pcut)
           end if
           if (((jj+off).le.ny).and.(iii.ge.1).and.(iii.le.nx)) then
            if (nimage(iii,jj+off).gt.0)
     +      blnkannulus = blnkannulus.and.(image2(iii,jj+off).gt.pcut)
           end if
          end do
          do jjj = jj-off,jj+off
           if (((ii-off).ge.1).and.(jjj.ge.1).and.(jjj.le.ny)) then
            if (nimage(ii-off,jjj).gt.0)
     +      blnkannulus = blnkannulus.and.(image2(ii-off,jjj).gt.pcut)
           end if
           if (((ii+off).le.nx).and.(jjj.ge.1).and.(jjj.le.ny)) then
            if (nimage(ii+off,jjj).gt.0)
     +      blnkannulus = blnkannulus.and.(image2(ii+off,jjj).gt.pcut)
           end if
          end do
          if (.not.blnkannulus) then
           off = off + 1
c don't go overboard
           if (off.gt.min(nx,ny)/4) goto 60
           goto 233
          end if
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
          call fitting (lIn, krng, nx, ny, image, rms, xpos, xposerr,
     +     ypos, yposerr, pkfl, pkflerr, intfl, amaj, amin, posa,
     +     posns, blc, bin, ii, jj, bvol, bpap, bvolp,
     +     bmajp, bminp, nimage, boxsize, image2, pcut, fitok,
     +     usedpixels, meanimg, sgimg, fdrpeak, allpix, psfsize,
     +     dumcount)
          if (.not.fitok) goto 60
c write out annotation file line if necessary
          if (kvannot) then
           write(line,49) posns(1)*180/pi,posns(2)*180/pi,
     +        amaj/3600.,amin/3600.,90.-posa
49         format("ellipse ",f16.10,f16.10,f16.10,f16.10,f8.2)
           call txtwrite (lann, line, len1(line), iostat)
          end if
c
c Convert location (peak or fitted) to formatted coordinate string
c
          typei(1) = 'hms'
          typei(2) = 'dms'
          typeo(1) = 'hms'
          typeo(2) = 'dms'
          call w2wfco (lin, 2, typei, ' ', posns,  typeo, ' ',
     +                 .true., radec, radeclen)
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
          end if
c
c Define output lines
c
          if (negative) then
           write(line,50) xposerr,yposerr,-pkfl,pkflerr,-intfl,
     +        amaj,amin,posa,-sgimg(ii,jj)*1000.,-rms*1000.
          else
           write(line,50) xposerr,yposerr,pkfl,pkflerr,intfl,
     +        amaj,amin,posa,sgimg(ii,jj)*1000.,rms*1000.
          end if
50        format(1x,f6.3,3x,f5.2,2x,f8.3,1x,f6.3,1x,f9.3,1x,
     +           3(f5.1,1x),f6.3,2x,f6.3)
          line = radec(1)(1:radeclen(1))//' '//
     +         radec(2)(1:radeclen(2))//' '//line
c
c write to log file, after appending 'Y' (as source confirmation, for
c consistency, since FDR mode is run in 'auto' mode, so all sources 
c are defined to be real).
c
          line(len1(line)+1:len1(line)+6) = '     Y'
          call txtwrite (llog, line, len1(line), iostat)

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
         end if
        end if
       end do
      end do
      if (kvannot) call txtclose(lann)
      call output(' ')
      write(line,'(a,i6,a)') 'Of the FDR pixels detected, a total of ',
     +     usedpixels,' were used in fitting sources.'
      call output(line)
      call output(' ')
      write(line,'("A total of ",i6," sources were detected.")') sources
      call output(line)
c      write(line,'("giving an average of ",f5.1,
c     +      " pixels per source.")') float(usedpixels)/float(sources)
c      call output(line)
c      call output('On average, the maximum possible number of')
c      write(line,'("falsely-detected sources would be ",f5.1)')
c     +  min(
c     +  (float(nfdrpix)*alpha/100.)/(float(usedpixels)/float(sources)),
c     +  sources)
c      call output(line)
c      if ((float(nfdrpix)*alpha/100.).le.(nfdrpix-usedpixels)) then
c       call output('In the best-case scenario, none of the falsely')
c       call output('detected pixels were used in fitting sources.')
c      else
c       nflse = (float(nfdrpix)*alpha/100.) - float(nfdrpix-usedpixels)
c       nflse = nflse/(float(usedpixels)/float(sources))
c       call output('In the best-case scenario, (the maximum number')
c       call output('of falsely-detected pixels were excluded from')
c       call output('the source-fitting process):')
c       write(line,'(f5.1," sources may have been falsely-detected.")')
c     +              nflse
c       call output(line)
c      end if
      return
      end


      subroutine gaussianpdf(nn,gsabsc,gsord,inty,val,pv)
c-----------------------------------------------------------------------
c calculates integrated gaussian values, and assigns pvalue (pv) as
c the value corresponding to the intensity nearest 'val'
c     
c  Input:
c    nn         number of points in the gaussian pdf
c    gsabsc     abscissa values
c    gsord      ordinate values
c    inty       gaussian pdf values (integrated ordinate values)
c    val        pixel intensity value
c  Output:
c    pv         P-value of input pixel
c-----------------------------------------------------------------------
      implicit none
      integer rr,nn
      real val,pv,gsabsc(nn),gsord(nn),inty(nn)
c-----------------------------------------------------------------------
      do rr = 1,nn
c check to see if we've reached 'val' yet, and finish if so.
       if (gsabsc(rr).ge.val) then
        if (rr.gt.1) then
c linearly interpolate between bounding values
         pv = inty(rr-1) + (inty(rr)-inty(rr-1))*(val-gsabsc(rr-1))/
     +        (gsabsc(rr)-gsabsc(rr-1))
        else
         pv = inty(1)
        end if
        return
       end if
      end do
      end
