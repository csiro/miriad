      program cgspec
c-----------------------------------------------------------------------
c
c= CGSPEC - Overlay spectra on images with PGPLOT.
c& nebk
c: plotting
c+
c	CGSPEC overlays spectra at spatial locations (specified in a text 
c	file) on images (displayed via a colour pixel map representation
c	and/or contour plots) on a PGPLOT device.   
c
c@ in
c	You may input up to 10 images.  Some of these are used to make a
c	2-D spatial display (up to 3 displayed via contours and 1 
c	displayed via a colour pixel map representation).  Of the rest, 
c	upto 5 may have spectra extracted from them, and 1 may be used 
c	as a mask. The pixel map, contour, and mask images must be of 
c	identical dimensions and size. Each spectrum image can be of any 
c	dimensions, size, and order. The mask image blanking mask is 
c	logically ORed to the pixel map and and contour plot image masks 
c	before they are displayed. The mask image is not displayed.
c
c	These images can be input in any order (see TYPE).
c	Wild card expansion is supported.    No default.
c@ type
c	Specifies the type of each image given, respectively, in the 
c	IN keyword. Minimum match is supported (note that "pixel" was 
c	formerly "grey" [which is still supported]).   Choose from:
c
c	"contour"   (contour;     up to 3 of these)    xyv
c	"pixel"     (pixel map;   up to 1 of these)    xyv
c	"spectrum"  (spectrum;    up to 5 of these)    any order
c	"dspectrum" (spectrum derivative          )    any order
c	"mask"      (mask;        up to 1 of these)    xyv
c
c	The "spectrum" images can be in any order.  However, it will be
c	faster to have the spectrum on the first axis if the cube is
c	very large (i.e. the image should be in vxy order. Use REORDER
c	if necessary).  The "dspectrum" images are the same as "spectrum"
c	images, except that the derivative of the spectrum with channel 
c	is taken by CGSPEC before it is plotted.  This is useful for 
c	Zeeman enthusiasts.  You can have up to 5 "spectrum" and 
c	"dspectrum" images in total.
c@ region
c	Region of interest.   This applies only to, and equally to,
c	the pixel map, contour and mask images.  All the image planes
c	you select will be averaged before display.    Only the bounding
c	box of the selected region is supported.
c@ xybin
c	Upto 4 values.  These give the spatial increment and binning
c	size in pixels for the x and y axes to be applied to the selected
c	region.   If the binning size is not unity, it must equal the 
c	increment.  For example, to bin up the image by 4 pixels in 
c	the x direction and to pick out every third pixel in the y 
c	direction, set XYBIN=4,4,3,1
c	Defaults are 1,XYBIN(1),XYBIN(1),XYBIN(3)
c@ slev
c	Up to 3 pairs of values for each contour image. First value is 
c	the type of contour level scale factor;  "p" for percentage and 
c	"a" for absolute.   Second value is the factor to scale LEVS by. 
c	Thus, SLEV=p,1  would contour levels at LEVS * 1% of the image 
c	peak intensity.  Similarly, SLEV=a,1.4e-2 would contour levels 
c	at LEVS * 1.4E-2
c	Default is no additional scaling of LEVS (i.e., "a",1.0)
c@ levs1
c	The levels to contour for the first specified contour image are 
c	LEVS1 times SLEV (either percentage of the image peak or absolute).
c	Defaults try to choose something vaguely useful.
c@ levs2
c	LEVS for the second contour image.
c@ levs3
c	LEVS for the third contour image.
c@ grange
c       4 values. These are the image intensity range to display (min to max), 
c	the transfer function type and the colour lookup table for the displayed
c       pixel map image.  The transfer function type can be one of "lin" (linear), 
c	"sqr" (square root), "log" (logarithmic), and "heq" (histogram 
c	equalization).  The colour lookup table is an integer from 1 to 8 
c	specifying a lookup table. Valid values are 1 (b&w), 2 (rainbow), 
c	3 (linear pseudo colour), 4 (floating zero colour contours), 5 (fixed 
c	zero colour contours), 6 (rgb), 7 (background), 8 (heat), and 9 
c	(absolute b&w).  If you enter a negative integer, then the 
c	reversed lookup table is displayed.
c
c       The transfer function changes available with OPTIONS=FIDDLE are in
c       addition (on top of) to the selections here, but the colour lookup 
c	table selections will replace those selected here.
c
c       Default is linear between the image minimum and maximum with
c       a b&w lookup table.   You can default the intensity range with
c       zeros, viz. "range=0,0,log,-2" say.
c@ vrange
c	2 values. The velocity range, in km/s, to plot.  If the first
c	axis of the spectrum image(s) is not velocity (say Frequency), 
c	use the natural units of that axis.
c	Default is min to  max from all the spectrum images.
c@ irange
c	2 values. The intensity range to plot for the spectra.
c	Default is min to max from all the spectrum images.
c@ iscale
c	Up to 5 values. A factor for each spectrum image by which it is
c	multiplied before plotting.
c	Defaults are all 1.
c@ spsize
c	2 values.   These are the sizes of the spectra, in fractions
c	of the view-port, in the x- and y-directions.
c	Defaults are 0.1, 0.1
c@ stick
c	2 values.  Major tick mark increments on the spectrum axes or
c	frame for labelling purposes.  
c	No default.
c@ device
c	The PGPLOT plot device, such as plot.plt/ps 
c	No default.
c@ labtyp
c	2 values;  the spatial label types of the x and y axes.
c	Minimum match is active.  Select from:
c
c	"hms"     the label is in H M S.S (e.g. for RA)
c	"dms"     the label is in D M S.S (e.g. for DEC)
c	"arcsec"  the label is in arcsecond offsets
c	"arcmin"  the label is in arcminute offsets
c	"absdeg"  the label is in degrees
c	"reldeg"  the label is in degree offsets
c		  The above assume the pixel increment is in radians
c	"abspix"  the label is in pixels
c	"relpix"  the label is in pixel offsets
c	"absnat"  the label is in natural coordinates as defined by 
c	          the header. 
c	"relnat"  the label is in offset natural coordinates
c	"none"      no label and no numbers or ticks on the axis
c
c	All offsets are from the reference pixel of the contour/pixel map images.
c	Defaults are "relpix", LABTYP(1)   except if LABTYP(1)="hms" when
c	LABTYP(2) defaults to "dms"  (to give RA and DEC)
c
c@ options
c	Task enrichment options. Minimum match of all keywords is active.
c
c	"blconly" means that if you have asked for some kind of spectrum
c	  labelling (frame or axes), only draw the frame or axes for
c	  the spectrum in the bottom left hand corner of the plot
c	"colour" means make the axes the same colour as the first
c	  spectrum, else they are white.
c       "fiddle" means enter a routine to allow you to interactively change
c         the display lookup table.  You can cycle through a variety of   
c         colour lookup tables, as well as alter a linear transfer function
c         by the cursor location, or by selecting predefined transfer
c         functions (linear, square root, logarithmic, histogram  equalization)
c
c         For hard copy devices (e.g. postscript), a keyboard driven
c         fiddle is offered; you can cycle through different colour tables
c         and invoke the predefined transfer functions, but the linear
c         fiddler is not available.   In this way you can make colour
c         hardcopy plots.
c	"frame" means draw a frame to the left and bottom of each spectrum
c	  and put the numeric labels on that frame. The default is no 
c	  frame plotting.
c	"full" means do full plot annotation with contour levels, pixel
c	  map intensity range, image names, reference values, etc.  
c	  Otherwise more room for the plot is available. 
c       "grid" means draw a coordinate grid on the plot rather than just ticks
c	"mark" marks the spatial location of the spectrum position with
c	  a star.  The spectra are plotted so that the centre if the
c	  frame (which could be drawn with OPTIONS=FRAME) is at the
c	  specified spatial location.   This positioning is not very
c	  obvious without the frame.
c	"mirror" causes all specified contour levels for all contour
c         images to be multiplied by -1 and added to the list of contours
c	"naked" means don't write the numeric axis labels on the spectrum
c	  axes or frame so as to reduce clutter
c	"noaxes"  means don't draw the X=0 and Y=0 axes which would,
c	  by default, be drawn and have the numeric labels on them.
c	  If the X=0 or Y=0 axes are not in the X and Y axis ranges of 
c	  your plot, then a FRAME (see above) option will automatically 
c	  be turned on for that axis.
c	"noblank" means draw the spectra where requested even if all of
c	  the displayed 2-D images are blanked at that location.  By
c	  default, a spectrum is not displayed if all of the spatial
c	  pixels over which the spectrum is averaged are blanked in all 
c	  of the displayed 2-D images.  Otherwise you get to see it.
c	"noepoch" means don't write the Epoch into the spatial axis
c	  label strings
c	"noerase" Don't erase a rectangle into which the "number"
c	  string is written.
c	"normalize" This option makes each spectrum come out with a
c	  peak of 1.0. This normalization is done after application
c	  of ISCALE, so you could set ISCALE=-1 to make absorption 
c	  look like emssion and then normalize. 
c	"number" writes the number of the spectrum in the corner of
c	  the box surrounding the spectrum.  The number is just
c	  just the counter counting how many locations there are in
c	  the overlay file (see OLAY).
c	"relax" means issue warnings when image axis descriptors are
c	  inconsistent (e.g. different pixel increments) instead
c	  of a fatal error.  Applies to pixel map, contour and
c	  mask images only.
c	"solneg1" means make negative contours solid and positive 
c	  contours dashed for the first contour image. The default, 
c	  and usual convention is the reverse.
c	"solneg2" SOLNEG1 for the second contour image.
c	"solneg3" SOLNEG1 for the third contour image.
c	"unequal" means draw plots with unequal scales in x and y
c	  so that the plot surface is maximally filled.  The default
c	  is for equal scales in x and y.
c	"wedge" means that if you are making a pixel map display, also draw
c	  and label a wedge to the right of the plot, showing the map 
c	  of intensity to colour
c	"1sided" means that for a derivative spectrum image, take a 
c	  1-sided derivative instead of the default 2-sided derivative
c
c@ clines
c 	Up to 3 values.  The line widths for each contour image
c	as specified in the order of TYPE. These widths are integer 
c	multiples of 1.
c	Defaults are all 1 for interactive devices and 2 for
c	har copy devices.
c@ slines
c	Up to 5 pairs of values.  These are the line widths and types
c	to use for the spectra for each spectrum image.  Line types
c	can be 1 -> 5 (solid and a variety of dashed/dotted types).   
c	Widths are integer multiples of 1.
c	Defaults are all 1 for interactive devices, and 2,1 for 
c	hard copy devices.
c@ blines
c	Up to 2 values.  These are the line widths to use for 1) the border
c	and labels of the contour/pixel map display and 2) the border/axes
c	for the spectra.  Widths are integer multiples of 1.
c	Defaults are 1,1 for interactive devices, and 2,2 for
c	hard copy devices.
c@ break
c	Up to 3 values. The intensity levels for the break between
c	solid and dashed contours for each contour image. 
c	Defaults are 0.0,0.0,0.0
c@ csize
c	Up to two values. Character sizes in units of the PGPLOT default 
c	(1, which is ~ 1/40 of the view surface height) for the 
c	contour/pixel map labels and the spectrum labels.
c	Defaults try to do something useful.
c@ olay
c	You can either give one file name, or as many file names as there
c	are spectrum images.  These files describe the locations at which
c	the overlay spectra are to be drawn.   If you give one file only,
c	the locations described by it are applied to all the input spectrum
c	images.  If you give several files, each of these corresponds
c	to the spectrum image in the order they are given in keyword IN.
c	
c	Wild card expansion is active and there is no default.  
c
c	Entries in the overlay file can be white space or comma
c	delimitered or both. 
c	All lines beginning with # are ignored.
c
c	                **** DO NOT USE TABS **** 
c
c	Double quotes " are used below to indicate a string.  The "
c	should not be put in the file.   For all the string parameters
c	discussed below, you can abbreviate them with minimum match.
c
c	Miriad task "CGCURS" with OPTIONS=LOG,CGSPEC,CURSOR can be
c	used to prepare a file suitable as input to OLAY.
c
c
c	There are two formats, depending upon the first line.
c
c	----------------------
c	CASE 1; GRID LOCATIONS
c	----------------------
c
c	If the first line is
c	
c	GRID
c
c	There should be one further line in the file:
c
c	  XINC  YINC  XSIZ  YSIZ
c
c	XINC and YINC are the increments across the contour/pixel map image
c	in ARCSEC at which spectra are to be drawn starting from the
c	bottom left corner of the display (defined by the REGION keyword)
c
c	XSIZ and YSIZ are the spatial half-sizes in ARCSEC over which
c	each spectrum is spatially averaged.  These are optional and 
c	default to 0 (no binning, just a spectrum at each spatial pixel)
c
c
c	---------------------------
c	CASE 2; IRREGULAR LOCATIONS
c	---------------------------
c
c	If the first line is
c
c	IRREGULAR
c
c	Each successive line describes one overlay spectrum location
c	according to:
c
c	  XOTYPE YOTYPE  X   Y   XSIZ  YSIZ
c
c	XOTYPE and YOTYPE  give the units of the overlay location 
c	contained in the file for the x- and y-directions, respectively.
c	Choose from
c
c	 "hms", "dms", "hms", "dms", "abspix", "relpix", "arcsec", 
c	 "arcmin", "absdeg", "reldeg", "absnat", and "relnat"  as 
c	  described in the keyword LABTYP.  
c
c	Note that %OTYPE does not depend upon what you specified for LABTYP.
c
c	X,Y defines the center of the overlay in the nominated OTYPE
c	coordinate system (X and Y OTYPE can be different).  Note
c	that for coordinate systems other than "hms" and "dms", the
c	coordinates are with respect to the pixel map  & contour images
c	axis descriptors,  not those from the spectrum images.
c
c	For %OTYPE = "abspix ", "relpix", "arcsec", "arcmin",  "absnat", 
c	             "relnat", "absdeg", and "reldeg"  X & Y are single numbers.
c
c	For %OTYPE = "hms" or "dms", the X and/or Y location is/are replaced
c	by three numbers such as  HH MM SS.S or DD MM SS.S.  Thus if
c	XOTYPE=hms & YOTYPE=dms then the file should have lines like
c
c	 hms dms  HH MM SS.S DD MM SS.S  
c
c	XSIZ and YSIZ are the spatial half-sizes in ARCSEC over which
c	each spectrum is spatially averaged.  These are optional and 
c	default to 0 (no binning, just a spectrum at each spatial pixel)
c
c--
c
c  History:
c    nebk 04Jul92  Original version
c    nebk 10jul92  Make some cosmetic changes to appease Lauren and fix
c		   an indexing problem when a spectrum had blanked pixels
c		   Deal with multiple region=image commands.
c    nebk 17jul92  Better line width defaults.  Add op=colour and don't
c		   plot spectra that are all 0.  Implement use of xyzio
c		   routines for random order spectrum images.
c    nebk 06aug92  Fix problem with spectrum images of more than 3
c	   	   dimensions bombing in xyzsetup and remove mistaken
c		   assumption in specloc that spectrum and grey/contour
c		   images have the same reference value.
c    nebk 09aug92  Modify for new otopixcg call, strip code to strprpcg
c		   and omatchcg
c    nebk 02oct92  Use pghline instead of pgline to plot spectra
c    nebk 24nov92  In sub specloc, an i was a j in the call to wtopixcg
c    nebk 03dec92  wtopixcg and pixtowcg now called w2pixcg and pix2wcg
c    nebk 17dec92  Adapt to new fndaxnum
c    nebk 28jan93  Remove some unneccessary code to do with opening
c                  the same file twice
c    mjs  12mar93  Use maxnax.h file instead of setting own value.
c    mjs  13mar93  pgplot subr names have less than 7 chars.
c    nebk 20may93  Deal properly with spectra which do not straddle
c                  x=0 and/or y=0 when options=doaxes not invoked
c    nebk 29may93  Replace call to chtonvcg by new PG routine pgqcs
c    nebk 08jun93  Add options=blconly
c    nebk 23jun93  Remove need for vpaspcg by calls to new pgqcs
c                  and change for new call to vpadjcg
c    nebk 07jul93  Put missing y axis labels back
c    nebk 10aug93  Was not properly dealing with fully blanked spectra
c    nebk 25aug93  Add labtyp "absdeg" and "reldeg", options=noerase,noepoch
c    nebk 17nov93  's' flag to boxset, new call sequence to axlabcg
c    nebk 27nov93  Idiot bug getting pixel increments for grid olays
c    nebk 14dec93  Add type=mask, options=noblank & strip limits to limitscg
c    nebk 03jan94  New call to matchcg (formerly omatchcg)
c    nebk 09jan94  Convert crpix to double precision. Correct call to
c		   pghline for confusing gapfactor.
c    nebk 29jan94  Add options=wedge,fiddle and grange transfer function
c		   types "heq" amd "sqr". Add labtype='none'
c    nebk 02mar94  New call for SETLABCG. 
c    nebk 18mar94  Add "dspectrum" image type
c    nebk 03jun94  Clarify use of region keyword
c    nebk 21jun94  Open files with OPIMCG & check consistency with CHKIM
c    nebk 28aug94  Adapt so that irregular overlay locations in true world
c                  coordinates are converted to linear world coordinates
c                  For grid locations, treat increments as linear world
c                  coordinate increments.  Linearize axis descriptors
c                  at the centre of the displayed region
c    nebk 31oct94  Fix mix up with luns for scratch files
c    nebk 23dec94  Make sure selected region no bigger than image
c    nebk 05jan95  Use new PGIMAG in favour of PGGRAY adding support   
c                  for fiddling of lookup table for hardcopy devices 
c                  Use orginal not linearized axis descriptors for
c                  full annotation
c    nebk 20feb95  Make sure PGIMAG writes black on white for hardcopy.
c		   Ammend for new wedge call sequences.  Add lookuptable
c	           to "grange" keyword. Move to image type "pixel"
c		   instead of "grey"
c    nebk 10apr95  Add doc for new absolute b&w lookup table
c    nebk 03sep95  Add labtyp=arcmin, nonlinear ticks
c    nebk 12nov95  Change to deal internally in absolute pixels
c                  '*lin' -> '*nat'
c    nebk 29nov95  New call for CONTURCG
c    nebk 18dec95  New call for VPSIZCG (arg. DOABUT)
c    nebk 18jan95  Fix silly problem in SPECBLNK causing overlays
c                  to be ignored if there were blanks in spatial image
c    nebk 30jan96  New call for CHNSELCG
c    rjs  21jul97  Fiddles with calls to initco/finco.
c   nebk  14nov01  Track change to readimcg interface
c
c Ideas:
c  * Be cleverer for sub-cubes which have spectra partly all zero
c  * Add interactive cgcurs like option if olay unset ??  
c  * Remove need for equal sized contour/pixel map images if I can get
c    Bob to play ball with BOXES code.
c  * Try to swap line colour index to get white lines on black
c    and vice versa.  Have this vary over the image as appropriate.
c
c---
c--------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      real wedwid, tfdisp
      integer maxlev, maxpos, maxcon, maxspec, maxtyp, nbins
      parameter (maxlev = 50, maxpos = 256*256, maxtyp = 13,
     +   maxcon = 3, maxspec = 5, wedwid = 0.05, tfdisp = 0.5, 
     +   nbins = 128)
c
      integer ipim, ipnim, ipimb, ixsp, iysp, insp, ipsp, imsp, iwsp
c
      integer csize(maxnax,maxcon), gsize(maxnax), bsize(maxnax),
     +  ssize(maxnax), size(maxnax), cnaxis(maxcon), gnaxis, bnaxis,
     +  snaxis, lc(maxcon), lg, lb, ls, sgrps(2,maxdim), 
     +  grpbeg(maxchan), ngrp(maxchan), ibin(2), jbin(2), krng(2), 
     +  iside(maxspec), lh, lgn, lcn(maxcon)
      double precision opos(4,maxpos)
      logical maskb, solneg(maxcon), grid(maxspec)
      character*6 ltypes(maxtyp)
      character*64 cin(maxcon), gin, bin, spin(maxspec), hin
c
      real levs(maxlev,maxcon), pixr(2), tr(6), cs(2), pixr2(2), 
     +  slev(maxcon), break(maxcon), vrange(2), irange(2), tfvp(4),
     +  iscale(maxspec), scale(2), vpn(4), vpw(4), vfrac(2), tick(2),
     +  cumhis(nbins), wdgvp(4), gmm(3), cmm(3,maxcon)
      real vxmin, vymin, vymax, vx, vy, vxsize, vysize, ydispb, 
     +  xdispl, groff, blankg, blankc, vmin, vmax, vvmin, vvmax, 
     +  imin, imax, vxgap, vygap
c
      integer blc(3), trc(3), win(maxnax), clines(maxcon), 
     +  slines(2,maxspec), blines(2), srtlev(maxlev,maxcon),
     +  nlevs(maxcon), sblc(maxnax), strc(maxnax), nblnkc(3), 
     +  his(nbins)
      integer nofile, npos, ierr, pgbeg, ilen, ncon, i, j, nspec,
     +  sizespec, ngrps, defwid, npts, iblc, nblnkg, nblnkcs, coltab
      integer spax, virsiz(maxnax), vircsiz(maxnax)
      integer len1, tflen(0:2), labcol, bgcol
c
      character labtyp(2)*6, levtyp(maxcon)*1, ofile(maxspec)*64
      character pdev*64, xlabel*40, ylabel*40, hard*20, trfun*3, 
     +  aline*132, ofile2*64, axisname*1, txtfill(0:2)*19
c
      logical dofull, eqscale, doblnkc, doblnkg, doblnkb, relax, doaxes, 
     +  doframe, fits(2), mark, spnorm, naked, number, mirror, init, 
     +  imnorm, colour, allzero, blconly, doerase, doepoch, igblank,
     +  allgood, allblnk, dofid, dowedge, hdprsnt, gaps, dotr, doaxlab,
     +  doaylab, donxlab(2), donylab(2), miss, dogrid, doabut
c
      data blankc /-99999999.00/
      data cin, gin, bin /maxcon*' ', ' ', ' '/
      data nblnkcs /0/
      data gaps, doabut, dotr, scale /.false., .false., .false., 2*0.0/
      data lgn, lcn /0, maxcon*0/
      data vmin, vmax, imin, imax /1.0e30, -1.0e30, 1.0e30, -1.0e30/
      data ltypes /'hms   ', 'dms   ', 'arcsec', 'arcmin', 'absdeg', 
     +             'reldeg', 'abspix', 'relpix', 'absnat', 'relnat',
     +             'none', 'abslin', 'rellin'/
      data txtfill, tflen /'spectrum', 'derivative spectrum', 
     +                     'derivative spectrum', 8, 19, 19/
c-----------------------------------------------------------------------
      call output ('CgSpec: version 14-Nov-2001')
      call output (' ')
c
c Get user inputs
c
      call inputs (maxlev, maxcon, maxspec, maxtyp, ltypes, ncon, nspec,
     +   cin, gin, bin, spin, iside, levtyp, slev, levs, nlevs, pixr, 
     +   trfun, coltab, pdev, labtyp, dofull, eqscale, solneg, clines, 
     +   break, cs, ofile, nofile, relax, slines, vrange, vfrac, irange, 
     +   tick, doaxes, doframe, mark, iscale, spnorm, naked, blines, 
     +   number, mirror, colour, blconly, doerase, doepoch, igblank,
     +   dofid, dowedge, dogrid, ibin, jbin)
c
c First verify the existence of wanted files and get some extrema
c
      call opnchk (nspec, spin, iside, ncon, cin, gin, bin, nofile, 
     +             ofile, grid, vmin, vmax, imin, imax)
c
c*************************************
c
c Draw pixel map and contour images
c
c*************************************
c
c Open contour images as required 
c
      hin = ' '
      if (ncon.gt.0)  then
        do i = 1, ncon
          call opimcg (maxnax, cin(i), lc(i), csize(1,i), cnaxis(i))
	  call initco(lc(i))
          cmm(1,i) =  1.0e30
          cmm(2,i) = -1.0e30
          cmm(3,i) = -1.0
          call chkax (lc(i), .false., cin(i))
          if (hin.eq.' ') then
            hin = cin(i)
            lh = lc(i)
          end if
        end do
      end if
c
c Open pixel map image as required
c
      if (gin.ne.' ') then
        call opimcg (maxnax, gin, lg, gsize, gnaxis)
	call initco(lg)
        gmm(1) =  1.0e30
        gmm(2) = -1.0e30
        gmm(3) = -1.0
        call chkax (lg, .false., gin)
        if (hin.eq.' ') then
          hin = gin
          lh = lg
        end if
c
c Check pixel map range and deal with log transfer function
c
        call grfixcg (pixr, lg, gnaxis, gsize, trfun, pixr2, 
     +                groff, blankg)
      end if
c
c Open mask image as required
c
      if (bin.ne.' ') then
        call opimcg (maxnax, bin, lb, bsize, bnaxis)
	call initco(lb)
        call chkax (lb, .false., bin)
        maskb = hdprsnt (lb, 'mask')
        if (.not.maskb)  then
          call bug ('w', 'The mask image does not have a mask')
	  call finco(lb)
          call xyclose (lb)
          bin = ' '
        end if
      end if
c
c Check consistency of input images
c
      call chkim  (relax, ncon, cin, lc, gin, lg, bin, lb)
c
c Finish key inputs for region of interest
c
      call region (maxnax, cin, lc, csize, cnaxis, gin, lg, gsize, 
     +  gnaxis, ibin, jbin, blc, trc, win, ngrps, grpbeg, ngrp)
c
c Allocate memory for pixel map/contour and mask images
c
      call memalloc (ipim,  win(1)*win(2), 'r')
      call memalloc (ipnim, win(1)*win(2), 'i')
      if (bin.ne.' ') call memalloc (ipimb, win(1)*win(2), 'l')
c
c Compute contour levels for each contour image
c
      if (ncon.gt.0) then
        do i = 1, ncon
          call conlevcg (mirror, maxlev, lc(i), levtyp(i), slev(i), 
     +                   nlevs(i), levs(1,i), srtlev(1,i))
        end do
      end if
c
c Work out array index limits, coordinate transformation array & labels
c
      call limitscg (blc, ibin, jbin, tr)
c
c Open PGPLOT device, set nice font and default line width
c
      ierr = pgbeg (0, pdev, 1, 1)
      if (ierr.ne.1) then
        call pgldev
        call bug ('f', 'Error opening plot device') 
      end if
      call pgpage
c
      call pgscf (2)
      call pgqinf ('hardcopy', hard, ilen)
      defwid = 1
      if (hard.eq.'YES') defwid = 2
      call bgcolcg (bgcol)
      call setlgc (bgcol, labcol)
c
      do i = 1, ncon
        if (clines(i).eq.0) clines(i) = defwid
      end do
      do i = 1, nspec
        if (slines(1,i).eq.0) slines(1,i) = defwid
      end do
      if (blines(1).eq.0) blines(1) = defwid
      if (blines(2).eq.0) blines(2) = defwid
c       
c Init OFM routines
c       
      if (gin.ne.' ') call ofmini
c
c Set axis labels
c
      call setlabcg (lh, labtyp, doepoch, xlabel, ylabel)
c
c Set label displacements from axes
c 
      call setdspcg (lh, labtyp, blc, trc, xdispl, ydispb)
c
c Work out view port sizes and increments.
c   
      call vpsizcg (dofull, dofid, ncon, gin, ' ', nspec, ' ',
     +  maxlev, nlevs, srtlev, levs, slev, 1, 1, cs, xdispl, ydispb,
     +  gaps, doabut, dotr, 1, wedwid, tfdisp, labtyp, vxmin, vymin, 
     +  vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales 
c requested or if scales provided by user
c
      call vpadjcg (lh, hard, eqscale, scale, vxmin, vymin, vymax, 1, 1,
     +              blc, trc, tfvp, wdgvp, vxsize, vysize)
c
c Set viewport and window (absolute pixels)
c
      vx = vxmin
      vy = vymax - vysize
      call pgsvp (vx, vx+vxsize, vy, vy+vysize)
      call pgswin (blc(1)-0.5, trc(1)+0.5, blc(2)-0.5, trc(2)+0.5)
c
c Read in mask image as required.
c
      if (bin.ne.' ') then
        init = .true. 
        do i = 1, ngrps
          if (i.gt.1) init = .false.
           krng(1) = grpbeg(i)
           krng(2) = ngrp(i)
           call readbcg (init, lb, ibin, jbin, krng, blc, trc, 
     +                  meml(ipimb), doblnkb)
        end do
	call finco (lb)
        call xyclose (lb)
      end if
c
c Read in pixel map image as required, averaging all selected planes.
c
      if (gin.ne.' ') then
        init = .true.
        imnorm = .false.
        do i = 1, ngrps
          if (i.gt.1) init = .false.
          if (i.eq.ngrps) imnorm = .true.
          krng(1) = grpbeg(i)
          krng(2) = ngrp(i)
          call readimcg (init, blankg, lg, ibin, jbin, krng,
     +       blc, trc, imnorm, memi(ipnim), memr(ipim), doblnkg, gmm)
c
c Apply mask image mask
c
          if (bin.ne.' ' .and. doblnkb) then
            call maskorcg (blankg, win, meml(ipimb), memi(ipnim),
     +                     memr(ipim))
            doblnkg = .true.
          end if
        end do
c
c Apply transfer function to pixel map image if required
c
        if (trfun.ne.'lin') call apptrfcg (pixr, trfun, groff, 
     +    win(1)*win(2), memi(ipnim), memr(ipim), nbins, 
     +    his, cumhis)
c
c Deal with OFM modifications for harcdopy device before calling PGIMAG
c
        if (hard.eq.'YES') then
c
c Apply user given OFM or b&w as default
c
          call ofmcol (coltab, pixr2(1), pixr2(2))
c
c Interactive fiddle of OFM
c
          if (dofid) call ofmmod (tfvp, win(1)*win(2), memr(ipim), 
     +                            memi(ipnim), pixr2(1), pixr2(2))
c
c Take complement of b&w lookup tables
c
          if (bgcol.eq.1) call ofmcmp
        end if
c
c Draw image and apply user given OFM to interactive PGPLOT devices
c
        call pgimag (memr(ipim), win(1), win(2), 1, win(1),
     +               1, win(2), pixr2(1), pixr2(2), tr)
        if (hard.eq.'NO') call ofmcol (coltab, pixr2(1), pixr2(2))
c
c Draw optional wedge
c
        call pgslw (1)
        call pgsci (labcol)
        if (dowedge) call wedgecg (1, wedwid, 1, trfun, groff, nbins,
     +                             cumhis, wdgvp, pixr(1), pixr(2))
c
c Retake OFM b&w complement for hardcopy devices
c
        if (hard.eq.'YES' .and. bgcol.eq.1) call ofmcmp
c
c Save normalization image if there are some blanks
c
        if (.not.igblank .and. doblnkg) then
          call scropen (lgn)
          call mscwrit (lgn, blc, trc, memi(ipnim), nblnkg)
        end if
      end if
c
c Label and draw axes.  Forces pixel map to update on /xd as well
c
      call pgslw (blines(1))
      call pgsch (cs(1))
      call pgsci (labcol)
c
c Determine if the axes need ascii or numeric labelling
c for this subplot
c 
      call dolabcg (gaps, dotr, 1, 1, 1, 1, 1, labtyp,
     +              doaxlab, doaylab, donxlab, donylab)
c
c Write on ascii axis labels
c 
      call aaxlabcg (doaxlab, doaylab, xdispl, ydispb, xlabel, ylabel)
c 
c Draw frame, write numeric labels, ticks and optional grid   
c
       call naxlabcg (lh, .true., blc, trc, krng, labtyp,
     +                donxlab, donylab, .false., dogrid)
c
c Modify OFM for interactive devices here
c
      if (dofid .and. hard.eq.'NO') 
     +  call ofmmod (tfvp, win(1)*win(2), memr(ipim), 
     +               memi(ipnim), pixr2(1), pixr2(2))
c
c Draw contour plots
c
      if (ncon.gt.0) then
        do i = 1, ncon
          init = .true.
          imnorm = .false.
          do j = 1, ngrps
            if (j.gt.1) init = .false.
            if (j.eq.ngrps) imnorm = .true.
            krng(1) = grpbeg(j)
            krng(2) = ngrp(j)
            call readimcg (init, blankc, lc(i), ibin, jbin, krng, blc,
     +         trc, imnorm, memi(ipnim), memr(ipim), doblnkc, cmm(1,i))
          end do
c
c Apply mask
c
          if (bin.ne.' ' .and. doblnkb) then
            call maskorcg (blankc, win, meml(ipimb), memi(ipnim), 
     +                     memr(ipim))
            doblnkc = .true.
          end if
c
          call pgslw (clines(i))
          call pgsci (7+i-1)
          call conturcg (.false., blankc, solneg(i), win(1), win(2), 
     +                   doblnkc, memr(ipim), nlevs(i), levs(1,i),
     +                   tr, break(i))
c
c Save normalization image if there are some blanks
c
          if (.not.igblank .and. doblnkc) then
            call scropen (lcn(i))
            call mscwrit (lcn(i), blc, trc, memi(ipnim), 
     +                    nblnkc(i))
            nblnkcs = nblnkcs + nblnkc(i)
          end if
        end do
      end if
c
c Plot annotation 
c
      if (dofull) then
        call pgslw (1)
        call pgsci (labcol)
        call fullann (lh, ncon, cin, gin, nspec, spin, lc, lg, maxlev, 
     +       nlevs, levs, srtlev, slev, trfun, pixr, vymin, blc, trc, 
     +       cs, ydispb, iscale, labtyp, ibin, jbin, gmm, cmm)
      end if
c
c Close files and free up memory
c
      if (gin.ne.' ')then
	call finco(lg)
	call xyclose(lg)
      endif
      if (ncon.gt.0) then
        do i = 1, ncon
	  call finco(lc(i))
          call xyclose (lc(i))
        end do
      end if
      call memfree (ipim,  win(1)*win(2), 'r')
      call memfree (ipnim, win(1)*win(2), 'r')
      if (bin.ne.' ') call memfree (ipimb, win(1)*win(2), 'l')
c
c If there were some blanks in the displayed region merge normalization 
c images (which have already had the mask image mask applied) into a 
c mask and close scratch files
c
      if (igblank .or. (nblnkcs.eq.0 .and. nblnkg.eq.0)) then
        allgood = .true.
      else
        call memalloc (ipimb, win(1)*win(2), 'i')
        call mergn (maxcon, ncon, gin, nblnkg, nblnkc, lgn, lcn, win(1),
     +              win(2), memi(ipimb), allgood)
        if (allgood) call memfree (ipimb, win(1)*win(2), 'i')
      end if
      if (.not.igblank) then
        if (lgn.ne.0) call scrclose (lgn)
        do i = 1, maxcon
          if (lcn(i).ne.0) call scrclose (lcn(i))
        end do
      end if
      call pgupdt
c
c Reset image viewport (mucked up by plot annotation) for overlays
c
      call pgsvp (vx, vx+vxsize, vy, vy+vysize)
      call pgswin (blc(1)-0.5, trc(1)+0.5, blc(2)-0.5, trc(2)+0.5)
c
c
c**************************
c
c Now overlay the spectra
c
c**************************
c
c
c Read overlay positions & decode positions into contour/pixel map pixels
c or generate an automatic list.  If only one file, then it is common
c to all spectrum images.  Have to open/close file whose header we use
c for coordinate transformations in OLAYDEC because xyz and xy can
c cannot exist together
c
      call xyopen (lh, hin, 'old', maxnax, size)
      call initco(lh)
      if (nofile.eq.1) then
        if (grid(1)) then
          call genpos (lh, ofile(1), blc, trc, maxpos, npos, opos)
          iblc = 1
        else
          call olaydec (lh, krng(1), krng(2), maxtyp, ltypes, maxpos, 
     +                  ofile(1), npos, opos, iblc)
        end if
        ofile2 = ofile(1)
      end if
c
c Set spectrum plot range defaults
c
      if (vrange(1).eq.0.0 .and. vrange(2).eq.0.0) then
        if (vmax.eq.vmin)  call bug ('f',
     +    'Default velocity range to plot is degenerate')
c
        vrange(1) = vmin - 0.05*(vmax-vmin)
        vrange(2) = vmax + 0.05*(vmax-vmin)
c
        write (aline, 100) vrange(1), vrange(2)
100     format ('Default velocity range  = ', 1pe11.4, ' to ',
     +          1pe11.4, ' km/s')
        call output (aline)
      else
        vvmin = min(vrange(1), vrange(2))
        vvmax = max(vrange(1), vrange(2))
        vrange(1) = vvmin
        vrange(2) = vvmax
      end if
c
      if (irange(1).eq.0.0 .and. irange(2).eq.0.0) then
        if (imin.eq.imax) call bug ('f', 
     +     'Default intensity range to plot is degenerate')
c
        irange(1) = imin - 0.05*(imax-imin)
        irange(2) = imax + 0.05*(imax-imin)
c
        write (aline, 110) irange(1), irange(2)
110     format ('Default intensity range = ', 1pe11.4, ' to ',
     +          1pe11.4)
        call output (aline)
      end if
c
c Find current view-port in normalized device coordinates and
c in world coordinates
c
      call pgqvp (0, vpn(1), vpn(2), vpn(3), vpn(4))
      call pgqwin (vpw(1), vpw(2), vpw(3), vpw(4))
c
c Set character size
c
      if (cs(2).eq.0.0)then
        cs(2) = 3*min(vfrac(1),vfrac(2))
	write (aline, 120) cs(2)
120     format ('Default spectrum character size = ', f5.2)
        call output (aline)
      end if
      call pgsch (cs(2))
c
c Loop over number spectrum images
c
      do i = 1, nspec
c
c Read overlay locations if one file per spectrum image
c
        if (nofile.gt.1) then
          if (grid(i)) then
            call genpos (lh, ofile(i), blc, trc, maxpos, npos, opos)
            iblc = 1
          else
            call olaydec (lh, krng(1), krng(2), maxtyp, ltypes, 
     +        maxpos, ofile(i), npos, opos, iblc)
          end if
          ofile2 = ofile(i)
        end if
c
c Open image
c
        call opimxyz (maxnax, spin(i), ls, ssize, snaxis)
	call initco(ls)
        call chkax (ls, .true., spin(i))
c
c Find velocity/freq axis (again; checked to exist in OPNCHK)
c
        axisname = ' '
        call fndaxnum (ls, 'freq', axisname, spax)
c
c Allocate memory for binned spectrum. 
c
        call specsiz (ls, vrange, spax, sizespec)
        call memalloc (ixsp, sizespec, 'r')
        call memalloc (iysp, sizespec, 'r')
        call memalloc (insp, sizespec, 'r')
        call memalloc (ipsp, sizespec, 'r')
        call memalloc (imsp, sizespec, 'l')
        if (iside(i).gt.0) call memalloc (iwsp, sizespec, 'r')
c
c Loop over number of spectrum locations
c
        do j = 1, npos
c
c Locate desired sub-cube in spectrum cube
c
          call specloc (lh, ls, snaxis, ssize, opos(1,j), vrange,
     +                  spax, sblc, strc, fits)
c
c Continue if requested spectrum can be extracted from the cube
c
          if (fits(1) .and. fits(2)) then
c
c Is this spatial area blanked in pixel map and contour images ?
c
            allblnk  = .false.
            miss = .false.
            if (.not.igblank .and. .not.allgood)
     +        call specblnk (lh, opos(1,j), blc, trc, win(1), 
     +                       win(2), memi(ipimb), allblnk, miss)
c
c Spatial area not all blanked, continue
c
            if (allblnk) then
              write (aline, 130) j, ofile2(1:len1(ofile2))
130           format ('Overlay # ', i4, ' from file ', a, 
     +                ' is located on blanked spatial pixels')
              call bug ('w', aline)
            else if (miss) then
              if ((nofile.eq.1 .and. j.eq.1) .or. nofile.gt.1) then
                write (aline, 140) j, ofile2(1:len1(ofile2))
140             format ('Overlay # ', i4, ' from file ', a, 
     +                  ' is not on the pixel map/contour image(s)')
                call bug ('w', aline)
              end if
            else
c
c Set up for XYZIO call
c
              call xyzsetup (ls, axisname, sblc, strc, virsiz, vircsiz)
c
c Read and spatially bin up the sub-cube producing spectrum
c
              call specin (ls, snaxis, sblc, spax, virsiz, 
     +           spnorm, iscale(i), memr(ixsp), memr(iysp), 
     +           memi(insp), allzero, memr(ipsp), meml(imsp))
c
c Draw spectrum
c
              npts = strc(spax) - sblc(spax) + 1
              if (allzero) then
                write (aline, 150) j, ofile2(1:len1(ofile2))
150             format ('Spectrum # ', i4, ' from ', a,
     +                  ' is all blanked')
                call bug ('w', aline)
              else
                call pltspec (i, j, maxdim, sgrps, iside(i), opos(1,j),
     +             vfrac, npts, vrange, memr(ixsp), memr(iysp), 
     +             memi(insp), memr(iwsp), irange, vpn, vpw, tick, 
     +             doaxes, doframe, slines(1,i), mark, naked, 
     +             blines(2), number, doerase, nspec, nofile, colour, 
     +             blconly, iblc)
                 call pgupdt
              end if
            end if
          else
            if (.not.fits(1)) then
              write (aline, 200) j, ofile2(1:len1(ofile2)),
     +          txtfill(iside(i))(1:tflen(iside(i))),
     +          spin(i)(1:len1(spin(i)))
200           format ('Overlay # ', i4, ' (', a, ')',
     +                ' not in spatial range of ', a, ' image ', a)
              call output (aline)
            end if
            if (.not.fits(2)) then
              write (aline, 250) j, ofile2(1:len1(ofile2)),
     +          txtfill(iside(i))(1:tflen(iside(i))),
     +          spin(i)(1:len1(spin(i)))
250           format ('Overlay # ', i4, ' (', a, ')',
     +                ' not in spectral range of ', a, ' image ', a)
              call output (aline)
            end if
          end if
        end do
c
c Close spectrum image and free up memory for next image
c
        call memfree (ixsp, sizespec, 'r')
        call memfree (iysp, sizespec, 'r')
        call memfree (insp, sizespec, 'i')
        call memfree (ipsp, sizespec, 'r')
        call memfree (imsp, sizespec, 'l')
        if (iside(i).gt.0) call memfree (iwsp, sizespec, 'r')
	call finco(ls)
        call xyzclose (ls)
      end do
      call finco(lh)
      call xyclose (lh)
c
c Free up merged mask memory and close PGPLOT device
c
      if (.not.allgood) call memfree (ipimb, win(1)*win(2), 'i')
      call pgend
c
      end
c
c
      subroutine cgbox (xa, xb, xd, i1, i2)
c-----------------------------------------------------------------------
c     This routine is used to determine where to draw the tick marks on
c     an axis. The input arguments XA and XB are the world-coordinate
c     end points of the axis; XD is the tick interval. PGBOX1 returns
c     two integers, I1 and I2, such that the required tick marks are
c     to be placed at world-coordinates (I*XD), for I=I1,...,I2.
c     Normally I2 is greater than or equal to I1, but if there are no
c     values of I such that I*XD lies in the inclusive range (XA, XB),
c     then I2 will be 1 less than I1.
c
c     This is a direct copy of PGBOX1, which is not a user called
c     subroutine.
c
c  Input
c    xa,xb   world-coordinate end points of the axis. XA must
c            not be equal to XB.
c    xd      world-coordinate tick interval. XD may be positive
c            or negative, but may not be zero.
c  Output
c    i1,i2   tick marks should be drawn at world coords I*XD for 
c            I in the inclusive range  I1...I2 (see above).
c
c
c-----------------------------------------------------------------------
      implicit none
c
      real xa, xb, xd
      integer i1, i2
cc
      real xlo, xhi
c-----------------------------------------------------------------------
      xlo = min(xa/xd, xb/xd)
      xhi = max(xa/xd, xb/xd)
      i1 = nint(xlo)
      if (i1.lt.xlo) i1 = i1 + 1
      i2 = nint(xhi)
      if (i2.gt.xhi) i2 = i2 - 1
c
      end
c
c
      subroutine chkax (lh, dovel, in)
c-----------------------------------------------------------------------
c     Make sure image has radian pixel increments and maybe  a
c     velocity axis
c
c  Input
c    lh      Image handle
c    dovel   Look for velocity axis too
c    in      Image name
c
c-----------------------------------------------------------------------
      implicit none
c
      integer lh
      logical dovel
      character*(*) in
cc
      include 'maxnax.h'
      character line*80, itoaf*1
      integer iax, len1, ivax, i, naxis
c-----------------------------------------------------------------------
      call rdhdi (lh, 'naxis', naxis, 0)
      ivax = 0
      if (dovel) then
c
c Look for velocity axis
c
        call axfndco (lh, 'VELO', naxis, 0, ivax)
        if (ivax.eq.0) call axfndco (lh, 'FREQ', naxis, 0, ivax)
        if (ivax.eq.0) then
          line = 'Spectrum image '//in(1:len1(in))//
     +         ' doesn''t have a velocity or frequency axis'
          call bug ('f', line) 
        end if         
c
c Look for radian axes
c
        do i = 1, min(3,naxis)
          if (i.ne.ivax) then
            call axfndco (lh, 'RAD', 0, i, iax)
            if (iax.eq.0) then
              line = 'Spectrum image '//in(1:len1(in))//
     +               ' axis '//itoaf(i)//
     +               ' doesn''t have radian pixel increments'
              call bug ('f', line)
            end if
          end if
        end do
      else
c
c Just check radian increments on first two axes
c
        do i = 1, min(2,naxis)
          call axfndco (lh, 'RAD', 0, i, iax)
          if (iax.eq.0) then
            line = 'Image '//in(1:len1(in))//' axis '//itoaf(i)//
     +             ' doesn''t have radian pixel increments'
            call bug ('f', line)
          end if
        end do
      end if
c
      end
c
c
      subroutine chkdes (relax, l1, l2, im1, im2)
c-----------------------------------------------------------------------
c     Compare axis descriptors for the first three axes
c
c  Input:
c   l1,l2    Handles
c   im1,2    Images
c-----------------------------------------------------------------------
      implicit none
c
      integer l1, l2
      character*(*) im1, im2
      logical relax
cc
      double precision d1, d2
      real r1, r2
      integer i1, i2
      character a1*9, a2*9
c
      integer maxis, k, il1, il2, len1
      character line*130, itoaf*1
c-----------------------------------------------------------------------
      il1 = len1(im1)
      il2 = len1(im2)
c
      call rdhdr (l1, 'epoch', r1, 0.0) 
      call rdhdr (l2, 'epoch', r2, 0.0) 
      if (r1.ne.r2) then
        line = 'Unequal epochs for images '//
     +     im1(1:il1)//' & '//im2(1:il2)
        if (relax) then
          call bug ('w', line)
        else
          call bug ('f', line)
        end if
      end if
c
      maxis = 3
      call rdhdi (l1, 'naxis3', i1, 0)
      call rdhdi (l2, 'naxis3', i2, 0)
      if (i1.le.1 .and. i2.le.1) maxis = 2
      do k = 1, maxis
        call rdhdi (l1, 'naxis'//itoaf(k), i1, 0)
        call rdhdi (l2, 'naxis'//itoaf(k), i2, 0)
c        
        if (i1.ne.i2) then
          line = 'Unequal dimensions for images '//im1(1:il1)//
     +           ' & '//im2(1:il2)//' on axis '//itoaf(k)
          call bug ('f', line)
        end if
c
        call rdhdd (l1, 'crpix'//itoaf(k), d1, 0.0d0) 
        call rdhdd (l2, 'crpix'//itoaf(k), d2, 0.0d0)
        call chkdescg (relax, 'crpix', k, im1(1:il1), im2(1:il2), d1,d2)
c
        call rdhdd (l1, 'cdelt'//itoaf(k), d1, 0.0d0) 
        call rdhdd (l2, 'cdelt'//itoaf(k), d2, 0.0d0)
        call chkdescg (relax, 'cdelt', k, im1(1:il1), im2(1:il2), d1,d2)
c
        call rdhdd (l1, 'crval'//itoaf(k), d1, 0.0d0) 
        call rdhdd (l2, 'crval'//itoaf(k), d2, 0.0d0)
        call chkdescg (relax, 'crval', k, im1(1:il1), im2(1:il2), d1,d2)
c
        call rdhda (l1, 'ctype'//itoaf(k), a1, ' ')
        call rdhda (l2, 'ctype'//itoaf(k), a2, ' ')

        if (a1.ne.a2) then
          write (line, 10) im1(1:il1), im2(1:il2), k
10        format ('Unequal ctype for images ', a, ' & ', a, 
     +            ' on axis ', i1)
          if (relax) then
            call bug ('w', line)
          else
            call bug ('f', line)
          end if
        end if
      end do
c
      end
c
c
      subroutine chkim (relax, ncon, cin, lc, gin, lg, bin, lb)
c-----------------------------------------------------------------------
c     Check all the images for internal consistency
c
c   Input:
c     l*         Handles
c     ncon       Number of contour images
c     relax      Only warnings instead of fatal errror for inconsistent
c                axis descriptors
c     *in        Input image names
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      integer ncon, lc(ncon), lg, lb
      character*(*) cin(*), gin, bin
      logical relax
cc
      integer i, j
c-----------------------------------------------------------------------
c
c Check contour images for self consistency 
c
      if (ncon.gt.1) then
        do i = 1, ncon-1
          do j = i+1, ncon
            call chkdes (relax, lc(i), lc(j), cin(i), cin(j))
          end do
        end do
      end if
c
c Check first contour image for consistency with other images
c
      if (ncon.gt.0) then
        if (gin.ne.' ') call chkdes (relax, lc, lg, cin, gin)
        if (bin.ne.' ') call chkdes (relax, lc, lb, cin, bin)
      end if
c
c Check pixel map images for consistency with other images
c
      if (gin.ne.' ' .and.  bin.ne.' ') 
     +   call chkdes (relax, lg, lb, gin, bin)
c
      end
c
c
      subroutine decopt (dofull, eqscale, solneg, relax, doaxes, 
     +   doframe, mark, norm, naked, number, mirror, colour, 
     +   blconly, doerase, doepoch, igblank, dofid, dowedge, dotwo,
     +   dogrid)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     dofull    True means do full annotation of plot
c     eqscale   True means plot with x and y scales
c     solneg    True means plot negative contours with solid line
c               style and positive contours with dashed line style
c               One for each contour image
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error
c     doaxes    Draw x=y=0 axes and label then for each spectrum
c     doframe   Put frame around each spectrum and  label it
c     mark      Mark spectrum locations
c     norm      Normalize spectra to peak of 1.0
c     number    Number the overlays
c     doerase   Erase background for number to be written on
c     mirror    DUplicate negative of the contours
c     colour    COlour axes
c     blconly   Only put frame or axes on spectrum in BLC of plot
c     doepoch   Write epoch into axis labels
c     igblank   Ignore spatial blanks when drawing spectra
c     dofid     Fiddle lookup table
c     dowedge   Draw pixel map wedge
c     dotwo     Two sided derivative for "dspectrum" else 1 sided
c     dogrid    Draw coordinate grid
c-----------------------------------------------------------------------
      implicit none
c
      logical dofull, eqscale, solneg(*), relax, doaxes, doframe,
     +  mark, norm, naked, number, mirror, colour, blconly, doerase, 
     +  doepoch, igblank, dofid, dowedge, dotwo, dogrid
cc
      integer maxopt
      parameter (maxopt = 22)
c
      character opshuns(maxopt)*9
      logical present(maxopt)
      data opshuns /'full     ', 'unequal  ', 'solneg1  ', 'solneg2  ',
     +              'solneg3  ', 'relax    ', 'noaxes   ', 'frame    ',
     +              'mark     ', 'normalize', 'naked    ', 'number   ',
     +              'mirror   ', 'colour   ', 'blconly  ', 'noerase  ',
     +              'noepoch  ', 'noblank  ', 'fiddle   ', 'wedge    ',
     +              '1sided   ', 'grid     '/
c-----------------------------------------------------------------------
      call optcg ('options', opshuns, present, maxopt)
c
      dofull    =      present(1)
      eqscale   = .not.present(2)
      solneg(1) =      present(3)
      solneg(2) =      present(4)
      solneg(3) =      present(5)
      relax     =      present(6)
      doaxes    = .not.present(7)
      doframe   =      present(8)
      mark      =      present(9)
      norm      =      present(10)
      naked     =      present(11)
      number    =      present(12)
      mirror    =      present(13)
      colour    =      present(14)
      blconly   =      present(15)
      doerase   = .not.present(16)
      doepoch   = .not.present(17)
      igblank   =      present(18)
      dofid     =      present(19)
      dowedge   =      present(20)
      dotwo     = .not.present(21)
      dogrid    =      present(22)
c
      end
c
c
      subroutine deriv (side, n, spec, work)
c-----------------------------------------------------------------------
c     Take derivative of spectrum
c
c   Inputs:
c     side      1 or 2 for 1 or 2 sided derivative
c     n         Number of channels
c     work      Work array
c   Input/output
c     spec      SPectrum. On output contains derivative
c
c-----------------------------------------------------------------------
      implicit none
c
      integer n, side
      real spec(n), work(n)
cc
      integer i
c-----------------------------------------------------------------------
      if (side.eq.1) then
        do i = 2, n
          work(i) = spec(i) - spec(i-1)
        end do
      else 
        do i = 2, n-1
          work(i) = 0.5 * (spec(i+1) - spec(i-1))
        end do
c
c  Fudge end
c
        work(n) = work(n-1)      
      end if
c
c  Fudge beginning
c
      work(1) = work(2)
c
c  Copy
c
      do i = 1, n
        spec(i) = work(i)
      end do
c
      end
c
c
      subroutine fullann (lh, ncon, cin, gin, nspec, spin, lc, lg, 
     +   maxlev, nlevs, levs, srtlev, slev, trfun, pixr, vymin, blc, 
     +   trc, pcs, ydispb, iscale, labtyp, ibin, jbin, gmm, cmm)
c-----------------------------------------------------------------------
c     Full annotation of plot with contour levels, RA and DEC etc.
c
c     Input
c       ncon       Number of contour images
c       c,gin      Image names
c       nspec      Number of spectrum images
c       spin       Spectrum images
c       lc,g       Handles for images
c       nlevs      Number of contour levels for each image
c       levs       Contour levels for each image
c       srtlev     Indexing arrya to order contoures in increasing order
c       slev       Contour level scale factors for each image
c       trfun      'log' or 'lin' transfer function
c       pixr       pixel map intensity range
c       vymin      y viewsurface normalized device coordinate
c                  at which the lowest sub-plot x-axis is drawn
c       blc,trc    Image window in pixels
c       pcs        PGPLOT character size parameters for plot
c       ydispb     Displacement of x-axis label in character heights
c       iscale     Scale factors for each spectrum image
c       labtyp     Axis label types
c       i,jbin     Spatial inc/bin
c       *mm        Displayed min/max
c----------------------------------------------------------------------- 
      implicit none
c
      integer maxlev, ncon, nlevs(*), blc(*), trc(*), lc(*), lg, lh,
     +  nspec, srtlev(maxlev,*), ibin(2), jbin(2)
      real levs(maxlev,*), vymin, slev(*), pixr(2), pcs, ydispb, 
     +  iscale(nspec), gmm(*), cmm(3,*)
      character*(*) cin(*), gin, trfun, spin(nspec), labtyp(2)
cc
      real xpos, ypos, yinc
      integer i, kbin(2)
      data kbin /0, 0/
c-----------------------------------------------------------------------
c       
c Setup chores and and annotate with reference value.
c       
      call anninicg (lh, .true., vymin, pcs, ydispb, labtyp, 
     +               xpos, ypos, yinc)
c       
c Write spatial window in pixels and channel inc. if possible
c       
      call annwincg (lh, blc, trc, ibin, jbin, kbin, yinc, xpos, ypos)
c       
c Write pixel map information
c       
      if (gin.ne.' ') call anngrscg (lg, gin, 1, pixr, trfun, gmm, 
     +                               yinc, xpos, ypos)
c       
c Write contour image information
c       
      if (ncon.gt.0) then
        do i = 1, ncon
          call annconcg (lc(i), cin(i), slev(i), nlevs(i), levs(1,i),
     +       srtlev(1,i), cmm(1,i), yinc, xpos, ypos)
        end do
      end if
c
c Write spectrum image names; there will be at least one
c
      call annspccg (nspec, spin, iscale, yinc, xpos, ypos)
c       
      end
c
c
      subroutine genpos (lh, ofile, blc, trc, maxpos, npos, opos)
c-----------------------------------------------------------------------
c     Generate automatic list of positions at regular intervals
c     on the image
c
c   Inputs
c     lh        Handle of generic spatial image
c     ofile     Overlay file
c     blc,trc   COntour/pixel map window
c     maxpos    Max number of positions allowed
c   Output
c     npos      Number of positions
c     opos      Array containing overlay
c                 X   Y   XSIZ   YSIZ    where X and Y are in
c	       contour/pixel map unbined full image spatial pixels 
c              & XSIZ and YSIZ are in ARCSEC
c
c------------------------------------------------------------------------
      implicit none
c
      integer maxpos, npos, blc(2), trc(2), lh
      double precision opos(4,maxpos)
      character*(*) ofile
cc
      double precision cdelt
      integer lpos, i, iostat, len1, ilen, lo
      character aline*100, itoaf*1
      real pinc(2), inc(2), bin(2), x, y
c
      include 'mirconst.h'
      double precision rtoa
      parameter (rtoa = 3600.0 * 180.0 / dpi)
c------------------------------------------------------------------------
      inc(1) = 0.0
      inc(2) = 0.0
      lo = len1(ofile)
c
      call txtopen (lpos, ofile, 'old', iostat)
      aline = 'Error opening positions file '//ofile(1:lo)
      if (iostat.ne.0) call bug ('f', aline)
c
c Read and discard first line which was checked in OPNCHK
c to be either IRREGULAR or GRID (in this case).
c
      call txtread (lpos, aline, ilen, iostat) 
      aline = 'Error reading first line from overlay file '//ofile(1:lo)
      if (iostat.ne.0) call bug ('f', aline)
c
c Read grid information.  # means comment
c
      iostat = 0
      do while (iostat.ne.-1)
        aline = ' '
        call txtread (lpos, aline, ilen, iostat) 
        if (iostat.eq.0) then
          if (aline(1:1).ne.'#' .and. aline.ne.' ' .and.
     +         (index(aline,'GRID').eq.0 .or. 
     +          index(aline,'grid').eq.0) ) then
            ilen = len1(aline)
            call posdec2 (aline(1:ilen), inc, bin)
          end if
        else
          aline = 'Error reading from overlay file '//ofile(1:lo)
          if (iostat.ne.-1) call bug ('f', aline)
        end if
      end do
      call txtclose (lpos)
      aline = 'Invalid information in overlay file '//ofile(1:lo)
      if (inc(1)*inc(2).eq.0.0) call bug ('f', aline)
c
c Convert increment to pixels.  Axes checked to be in radians in
c subroutines OPNCHK
c
      do i = 1, 2
        call rdhdd (lh, 'cdelt'//itoaf(i), cdelt, 0.0d0)
        pinc(i) = inc(i) / abs(cdelt*rtoa)
        aline = 'Spectrum location spatial increment too large in '
     +           //ofile(1:lo)
        if (pinc(i).ge.trc(i)-blc(i)) call bug ('f', aline)
      end do
c
c Now loop over image and generate list
c
      npos = 0
      y = blc(2) + pinc(2)/2.0
      do while (y.lt.trc(2))
        x = blc(1) + pinc(1)/2.0
        do while (x.lt.trc(1))
          npos = npos + 1
          opos(1,npos) = x
          opos(2,npos) = y
          opos(3,npos) = bin(1)
          opos(4,npos) = bin(2)
c
          if (npos.eq.maxpos) then
            aline = 'Reached max. no. of allowed spectrum locations in '
     +              //ofile(1:lo)
            call bug ('w', aline)
            goto 100
          end if
c 
          x = x + pinc(1)
        end do
        y = y + pinc(2)
      end do
100   aline = 'No overlay locations were generated from '//ofile(1:lo)
      if (npos.eq.0) call bug ('f', aline)
c
      end
c
c
      subroutine inputs (maxlev, maxcon, maxspec, maxtyp, ltypes, ncon,
     +   nspec, cin, gin, bin, spin, iside, levtyp, slev, levs, nlevs, 
     +   pixr, trfun, coltab, pdev, labtyp, dofull, eqscale, solneg, 
     +   clines, break, cs, ofile, nofile, relax, slines, vrange, vfrac, 
     +   irange, tick, doaxes, doframe, mark, scale, norm, naked, 
     +   blines, number, mirror, colour, blconly, doerase, doepoch, 
     +   igblank, dofid, dowedge, dogrid, ibin, jbin)
c-----------------------------------------------------------------------
c     Get the unfortunate user's long list of inputs
c
c  Input:
c   maxlev     Maximum number of allowed contour levels
c   maxcon     Maximum number of contour images
c   maxspec    Maximum number of spectrum images allowed
c   maxtyp     Maximum number of potential label types
c   ltypes     Potential label types
c  Output:
c   ncon       Number of contour images
c   nspec      Number of spectrum images
c   c,g,b,spin Contour, oixel map, mask, and spectrum image names
c   iside      0 -> spectrum image, 1-> 1-sided derivative spectrum,
c              2 -> 2-sided derivative spectrum image
c   levtyp     Type of contour levels scale factors for each contour
c              image:  'p'(ercentage) or 'a'(bsolute)
c   slev       Contour levels scale factors (absolute or percentage)
c              for each contour image
c   levs       Contour levels for each contour image.   Will be scaled 
c              by SLEV for contouring
c   nlevs      Number of contour levels for each contour image
c   pixr       Pixel map intensity range
c   trfun      Type of pixel map transfer function: log,lin,sqr,or heq
c   coltab     Colour table to apply to device.  
c   pdev       PGPLOT plot device/type
c   labtyp     Type of labels for x and y axes
c   dofull     True means do full annotaiton of plot
c   eqscale    True means plot with x and y scales
c   solneg     True means plot negative contours with solid line
c              style and positive contours with dashed line style
c              One for each contour image
c   clines     PGPLOT line widths for each contour image
c   break      Level for break between solid and dashed contours
c              for each contour image
c   cs         PGPLOT character sizes for the plot axis labels and spectra
c   ofile      Overlay  file name(s)
c   nofile     Number of overlay files
c   relax      Only issue warnings instead of fatal eror when
c              axis descriptors don;t agree between images
c              the direction of increasing X and Y
c   slines     SPectrum line widths an dline styles
c   vrange     Velocity range to plot
c   vfrac      The fraction of the view-port that each spectrum takes up
c   irange     Intesnity range to plot
c   tick       Ticj increments on the X and Y axes for the spectra
c   doaxes     Draw x=0 y=0 axes on spectra and label them
c   doframe    Draw frame around each spectrum and label the frame
c   scale      Scale factors for each spectrum image
c   norm       Normalize all spectra to have a peak of 1.0
c   naked      DOn't write numeric axis labels
c   bline      Line widths for contour/gtey borders and spectra
c   number     Number the spectra
c   doerase    Erase rectangle for spectrum number
c   mirror     causes all specified contour levels for all images
c              to be multiplied by -1 and added to the list of contours
c   colour     Colour axes
c   blconly    Only put frame or axes on spectrum in blc of plot
c   doepoch    Write EPoch into axis labels
c   igblank    Ignore spatial blanks when drawing spectra
c   dofid      Fiddle lookup table
c   dowedge    Draw pixel map wedge
c   dogrid     Draw overlay grid
c   i,jbin     SPatial pixek increment and averaging in x and y directions
c-----------------------------------------------------------------------
      implicit none
c
      integer maxlev, maxcon, maxspec, maxtyp, ncon, nspec, nofile, 
     +coltab
      real levs(maxlev,maxcon), pixr(2), cs(2), slev(maxcon), 
     +  break(maxcon), vrange(2), vfrac(2), irange(2), tick(2),
     +  scale(maxspec)
      integer nlevs(maxcon), clines(maxcon), slines(2,maxspec),
     +  blines(2), ibin(2), jbin(2), iside(maxspec)
      character*(*) labtyp(2), cin(maxcon), gin, bin, spin(maxspec), 
     +  pdev, ofile(maxspec), trfun, levtyp(maxcon), ltypes(maxtyp)
      logical dofull, eqscale, solneg(maxcon), relax, doframe, doaxes,
     +  mark, norm, naked, number, mirror, colour, blconly, doerase,
     +  doepoch, igblank, dofid, dowedge, dogrid, dunw
cc
      integer nmaxim
      parameter (nmaxim = 10)
c
      integer nim, nimtype, i, j, nlab, len1
      logical same, dotwo
      character images(nmaxim)*64, imtype(nmaxim)*9, line*132
      character*1 str, itoaf
c
      integer ntype
      parameter (ntype = 6)
      character type(ntype)*9
      data type  /'contour', 'pixel', 'spectrum', 'mask', 
     +            'dspectrum', 'grey'/
      data dunw /.false./
c-----------------------------------------------------------------------
      call keyini
c
c Get options first
c
      call decopt (dofull, eqscale, solneg, relax, doaxes, doframe, 
     +   mark, norm, naked, number, mirror, colour, blconly,
     +   doerase, doepoch, igblank, dofid, dowedge, dotwo, dogrid)
c
c Sort out input images
c
      call mkeyf ('in', images, nmaxim, nim)
      if (nim.eq.0) call bug ('f', 'No images given')
      call keymatch ('type', ntype, type, nmaxim, imtype, nimtype)
c
      ncon = 0
      nspec = 0
      do i = 1, nim
        if (imtype(i).eq.' ') then
          line = images(i)(1:len1(images(i)))//
     +          ' defaulting to type spectrum'
          call output (line)
          imtype(i) = 'spectrum'
        end if
c
        if (imtype(i).eq.'contour') then
          if (ncon.ge.maxcon) then
            call bug ('f', 'Too many contour images given')
          else
            ncon = ncon + 1
            cin(ncon) = images(i)
          end if
        else if (imtype(i).eq.'pixel' .or. imtype(i).eq.'grey') then
          if (gin.ne.' ') then
            call bug ('f', 'More than one pixel map image given')
          else
            gin = images(i)
          end if
        else if (imtype(i).eq.'mask') then
          if (bin.ne.' ') then
            call bug ('f', 'More than one mask image given')
          else
            bin = images(i)
          end if
        else if (imtype(i).eq.'spectrum' .or. 
     +           imtype(i).eq.'dspectrum') then
          if (nspec.ge.maxspec) then
            call bug ('f', 'Too many spectrum images given')
          else
            nspec = nspec + 1
            spin(nspec) = images(i)
            iside(nspec) = 0
            if (imtype(i).eq.'dspectrum') then
              iside(nspec) = 1
              if (dotwo) iside(nspec) = 2
            end if
          end if
        else
          call bug ('f', 'Unrecognized image type')
        end if
      end do
      if (nspec.eq.0) call bug ('f', 'No spectrum images given')
      if (ncon.eq.0 .and. gin.eq.' ') call bug ('f',
     +  'No pixel map or contour images given; can''t draw axes')
c
      do i = 1, nspec-1
        do j = i+1, nspec
          same = iside(i)*iside(j).eq.2
          if (spin(i).eq.spin(j) .and. same) call bug ('f',
     +       'You have given the same spectrum image twice')
        end do
      end do
c
c Get on with the rest
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
      call keya ('slev', levtyp(1), 'a')
      call lcase (levtyp(1))
      if (levtyp(1).ne.'p' .and. levtyp(1).ne.'a') call bug ('f', 
     +   'Unrecognized contour level scale type; must be "p" or "a"')
      call keyr ('slev', slev(1), 0.0)
c      
      if (ncon.gt.1) then
        do i = 2, ncon
          call keya ('slev', levtyp(i), 'a')
          call lcase (levtyp(i))
          if (levtyp(i).ne.'p' .and. levtyp(i).ne.'a') call bug ('f', 
     +     'Unrecognized contour level scale type; must be "p" or "a"')
c
          call keyr ('slev', slev(i), 0.0)
        end do
      end if
      do i = 1, maxcon
        str = itoaf(i)
        call mkeyr ('levs'//str,  levs(1,i), maxlev, nlevs(i))
      end do
c
      call keyr ('grange', pixr(1), 0.0)
      call keyr ('grange', pixr(2), 0.0)
      call keya ('grange', trfun, 'lin')
      call keyi ('grange', coltab, 1)
      call lcase (trfun)
      if (gin.ne.' ' .and. trfun.ne.'lin' .and. trfun.ne.'log' .and.
     +    trfun.ne.'sqr' .and. trfun.ne.'heq') call bug ('f',
     +    'Unrecognized image transfer function type')
c
      call keyr ('spsize', vfrac(1), 0.1)
      call keyr ('spsize', vfrac(2), vfrac(1))
      if (vfrac(1).le.0.0 .or. vfrac(2).le.0.0 .or. vfrac(1).gt.1.0 .or.
     +    vfrac(2).gt.1.0) call bug ('f', 'Invalid SPSIZE')
c
      call keyr ('vrange', vrange(1), 0.0)
      call keyr ('vrange', vrange(2), 0.0)
      call keyr ('irange', irange(1), 0.0)
      call keyr ('irange', irange(2), 0.0)
c
      do i = 1, nspec
        call keyr ('iscale', scale(i), 1.0)
      end do
c
      call keyr ('stick', tick(1), 0.0)
      call keyr ('stick', tick(2), 0.0)
c
      call keya ('device', pdev, ' ')
c
      call keymatch ('labtyp', maxtyp, ltypes, 2, labtyp, nlab)
      if (nlab.eq.0) then
        labtyp(1) = 'relpix'
        labtyp(2) = 'relpix'
      else if (nlab.eq.1) then
        if (labtyp(1).eq.'hms') then
          labtyp(2) = 'dms'
        else
          labtyp(2) = labtyp(1)
        end if
      end if
      if (labtyp(1)(4:6).eq.'lin') then
        labtyp(1)(4:6) = 'nat'
        call bug ('w', 'Axis label types abslin and rellin are ')
        call bug ('w', 'deprecated in favour of absnat and relnat')
        dunw = .true. 
      end if
      if (labtyp(2)(4:6).eq.'lin') then
        labtyp(2)(4:6) = 'nat'
        if (.not.dunw) then
          call bug ('w', 'Axis label types abslin and rellin are ')
          call bug ('w', 'deprecated in favour of absnat and relnat')
        end if
      end if  
c
      if (doframe) doaxes = .false.
      if (.not. naked) then
        if (tick(1).eq.0.0) call bug ('f', 
     +    'You must specify the velocity tick increments')
        if (tick(2).eq.0.0) call bug ('f', 
     +    'You must specify the intensity tick increments')
      end if
      if (gin.eq.' ') then
        dowedge = .false.
        dofid = .false.
      end if
c
      call keyi ('clines', clines(1), 0)
      if (clines(1).lt.0) clines(i) = 0
      call keyr ('break', break(1), 0.0)
c
      if (ncon.gt.1) then
        do i = 2, ncon
          call keyi ('clines', clines(i), 0)
          if (clines(i).lt.0) clines(i) = 0
c        
          call keyr ('break', break(i), 0.0)
        end do
      end if
c
      do i = 1, nspec
        call keyi ('slines', slines(1,i), 0)
        call keyi ('slines', slines(2,i), 1)
        if (slines(1,i).lt.0) slines(1,i) = 0
        if (slines(2,i).le.0) slines(2,i) = 1
      end do
c
      do i = 1, 2
        call keyi ('blines', blines(i), 0)
        if (blines(i).lt.0) blines(i) = 0
      end do
c
      call keyr ('csize', cs(1), 0.0)
      if (cs(1).le.0.0) cs(1) = 1.3
      call keyr ('csize', cs(2), 0.0)
c
      call mkeyf ('olay', ofile, maxspec, nofile)
      if (nofile.eq.0) call bug ('f', 'You must give an overlay file')
      if (nofile.gt.1 .and. nofile.lt.nspec) call bug ('f',
     +  'You must specify an overlay file for each spectrum image')
c
      end
c
c
      subroutine mergn (maxcon, ncon, gin, nblnkg, nblnkc, lgn, lcn, 
     +                  is, js, mask, allgood)
c------------------------------------------------------------------------
c     Merge the normalization images stored in scratch files
c     into a mask
c
c  Input
c    maxcon  Max. no. of contour images allowed
c    ncon    Number of contour images displayed
c    gin     Name of pixel map image
c    nblnkg  Number of flagged pixels in the pixel map image in
c            the displayed region
c    nblnkc  Number of flagged pixels in each contour image in
c            the displayed region
c    lg,lc   Handles for scratch files containing the locations of
c            flagged pixels in pixel map  and contour images  If 0 then
c            there is no scratch file open for that image
c    is,js   Size of displayed region
c Output
c    mask    Merged mask.  0 means that all the displayed images are
c            blanked at that pixel. 1 means that some of them were
c            unblanked
c    allgood Means that none of the displayed spatial pixels had all
c            displayed images blanked.  So don't mess around with masks
c            anymore.
c------------------------------------------------------------------------
      implicit none
      integer maxcon, ncon, nblnkc(*), nblnkg, lgn, lcn(*), is, js, 
     +  mask(is,js)
      character*(*) gin
      logical allgood
cc
      integer i, j, k, l, off, nim, nblim
      real data(2)
c------------------------------------------------------------------------
c
c How many images did we display ?
c
      nim = 0
      if (gin.ne.' ') nim = nim + 1
      nim = nim + ncon
c
c See how many images had blanked pixels
c
      nblim = 0
      if (nblnkg.gt.0) nblim = nblim + 1
      do i = 1, maxcon
        if (nblnkc(i).gt.0) nblim = nblim + 1
      end do
c
c If the number of images with blanked pixels is less than the number
c of diplayed images, we can stop straight away, as this means that
c not all of the images can be blanked at a certain spatial pixel
c
      if (nblim.lt.nim) then
        allgood = .true.
      else
        allgood = .false.
c
c Initialize mask to all good (different convention to usual
c when 0 means a bad pixel)
c
        do j = 1, js
          do i = 1, is
            mask(i,j) = 0
          end do
        end do
c
c Read scratch files and populate
c
        if (lgn.ne.0) then
          off = 0
          do k = 1, nblnkg
            call scrread (lgn, data, off, 2)
            off = off + 2
c
            i = nint(data(1))
            j = nint(data(2))
            mask(i,j) = mask(i,j) + 1
          end do
        end if
c
        do l = 1, maxcon
          if (lcn(l).ne.0) then
            off = 0
            do k = 1, nblnkc(l)
              call scrread (lcn(l), data, off, 2)
              off = off + 2
c
              i = nint(data(1))
              j = nint(data(2))
              mask(i,j) = mask(i,j) + 1
            end do
          end if
        end do
c
c Now, we know if that a pixel is blanked in every 2-D image
c that was displayed, the value of mask should be NIM.  So
c go through the mask once more and set 0 = bad if all pixels
c were blanked, and 1 = good to put it in the usual convention
c
        do j = 1, js
          do i = 1, is
            if (mask(i,j).eq.nim) then
              mask(i,j) = 0
            else 
              mask(i,j) = 1
            end if
          end do
        end do
      end if
c
      end
c
c
      subroutine mscwrit (lh, blc, trc, norm, nbl)
c------------------------------------------------------------------------
c     Write normalization image out to scratch file
c
c     Input
c       norm       0 -> blanked
c------------------------------------------------------------------------
      implicit none
      integer lh, blc(2), trc(2), norm(*), nbl
cc
      real data(2)
      integer off, i, j, k, isiz, jsiz
c------------------------------------------------------------------------
      isiz = (trc(1)-blc(1)+1)
      jsiz = (trc(2)-blc(2)+1)
c
      nbl = 0
      k = 1
      off = 0
      do j = 1, jsiz
        do i = 1, isiz
          if (norm(k).eq.0) then
            data(1) = i
            data(2) = j
            call scrwrite (lh, data, off, 2)
            off = off + 2
            nbl = nbl + 1
          end if
          k = k + 1
        end do
      end do
c
      end
c
c
      subroutine nlabel (axis, tick, range)
c-----------------------------------------------------------------------
c     Write numeric labels on spectral axis a bit lower that normal and
c     don't write the labels at x=0 and y=0 to avoid clutter
c
c     This is based on some code from PGBOX
c
c-----------------------------------------------------------------------
      real tick, range(2)
      character*1 axis
cc
      real tint, xlen, ylen, xloc, yloc, yh
      integer np, nv, i1, i2, ntext, i
      character text*130
c-----------------------------------------------------------------------
c
c Set tick increments
c      
      call cgbox (range(1), range(2), tick, i1, i2)
      tint = sign(tick,range(2)-range(1))
      np = int(log10(abs(tint)))-4
      nv = nint(tint/10.0**np)
c
      if (axis.eq.'x') then
c
c Find height of one character in y direction in world coordinates
c
        call yhtwcg (yh)
c
c Set y location of labels
c
        yloc = -1.5*yh
c
c Write labels
c
        do i = i1, i2
          xloc = i*tick
          if (i.ne.0.0) then
            call pgnumb (i*nv, np, 0, text, ntext)
            call pgptxt (xloc, yloc, 0.0, 0.5, text(1:ntext))
          end if
        end do
      else if (axis.eq.'y') then
c
c Set x location of labels
c
        call pglen (4, 'X', xlen, ylen)
        xloc = -xlen
c
c Write labels
c      
        do i = i1, i2
          yloc = i*tick
          if (i.ne.0.0) then
            call pgnumb (i*nv, np, 0, text, ntext)
            call pgptxt (xloc, yloc, 90.0, 0.5, text(1:ntext))
          end if
        end do
      end if
c
      end
c
c
      subroutine olaydec (lun, pl1, npl, maxtyp, ltypes, maxpos, 
     +                    ofile, npos, opos, iblc)
c-----------------------------------------------------------------------
c     Read overlay positions list file and decode
c
c   Inputs
c     lun      Handele of pixel map/contour image
c     pl1,npl  Start plane and number of planes displayed
c     maxtyp   Maximum number of potential label types
c     ltypes   Potential label types
c     maxpos   Maximum number of allowed overlays
c     ofile    Overlay file name
c  Outputs
c     npos     Number of overlays
c     opos     Array containing overlay
c                 X   Y   XSIZ   YSIZ    where X and Y are in
c	       contour/pixel map spatial pixels & XSIZ and YSIZ 
c	       are in ARCSEC
c     iblc     Number of overlay which is the closest to the BLC 
c              of the plot
c
c------------------------------------------------------------------------
      implicit none
c
      integer maxpos, npos, iblc, maxtyp, lun, pl1, npl
      double precision opos(4,maxpos)
      character ofile*(*), ltypes(maxtyp)*6
cc
      double precision pix3
      integer iostat, ilen, lpos, lo, imin, jmin
      character aline*100
c
      integer len1
      character itoaf*4
c------------------------------------------------------------------------
      lo = len1(ofile)
      call txtopen (lpos, ofile, 'old', iostat)
      aline = 'Error opening positions file '//ofile(1:lo)
      if (iostat.ne.0) call bug ('f', aline)
c
c Read and discard first line which has already been found to contain 
c GRID or IRREGULAR in subroutine OPNCHK      
c
      call txtread (lpos, aline, ilen, iostat) 
      aline = 'Error reading first line from overlay file '//ofile(1:lo)
      if (iostat.ne.0) call bug ('f', aline)
c
c Read and decode locations.  # means comment
c
      imin = 1e5
      jmin = 1e5
      npos = 0
      iostat = 0
      pix3 = dble(2*pl1+npl-1) / 2.0
c
      do while (iostat.ne.-1)
        aline = ' '
        call txtread (lpos, aline, ilen, iostat) 
        if (iostat.eq.0) then
          if (aline(1:1).ne.'#' .and. aline.ne.' ') then
            if (npos.eq.maxpos) then
              call bug ('w', 'Reducing no. overlays to max. '//
     +                       'allowed = '//itoaf(maxpos))
              iostat = -1
            else
              npos = npos + 1
              ilen = len1(aline)
              call posdec (lun, pix3, maxtyp, ltypes, ofile(1:lo), npos,
     +                     aline(1:ilen), opos(1,npos))
c
              if (opos(1,npos).lt.imin .and. opos(2,npos).lt.jmin) then
                imin = opos(1,npos)
                jmin = opos(2,npos)
                iblc = npos
              end if
            end if
          end if
        else
          aline = 'Error reading from overlay file '//ofile(1:lo)
          if (iostat.ne.-1) call bug ('f', aline)
        end if
      end do
c
      call txtclose (lpos)
      aline = 'There were no locations in overlay file '//ofile(1:lo)
      if (npos.eq.0) call bug ('f', aline)
c
      end
c
c
      subroutine opnchk (nspec, spin, iside, ncon, cin, gin, bin, 
     +                   nofile, ofile, grid, vmin, vmax, imin, imax)
c-----------------------------------------------------------------------
c     Make check on the existence of the requested files.  ALso
c     fish out whether we have gridded or irregular overlays while
c     we have the file open
c
c  Input
c   nspec    No. spectrum images
c   spin     Spectrum images
c   iside    0 -> spectrum 1-> 1-sided derivative spectrum,
c            2-> 2-sided derivative spectrum
c   ncon     No. contour images
c   cin      Contour image
c   gin      pixel map image
c   bin      Mask image
c   nofile   No. overlay files
c   ofile    Overlay files
c   grid     Draw spectra at regular grtid locations as defined in OFILE
c   vmin,max Velocity min and max from all spectrum images
c   imin,max Min and max intensities from all spectrum images
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nspec, ncon, nofile, iside(nspec)
      character*(*) spin(nspec), cin(ncon), gin, bin, ofile(nofile)
      logical grid(*)
      real imin, imax, vmin, vmax
cc
      include 'maxnax.h'
      integer size(maxnax), lh, i, iostat, len1, ilen, iax, naxis
      double precision v1, v2
      real limin, limax, lvmin, lvmax
      character line*80
c-----------------------------------------------------------------------
      call output ('Begin file existence checks')
      if (ncon.gt.0) then
        do i = 1, ncon
          line = ' Verifying contour image             : '//
     +           cin(i)(1:len1(cin(i)))
          call output (line)
          call xyopen (lh, cin(i), 'old', maxnax, size)
          call xyclose (lh)
        end do
      end if
c
      if (gin.ne.' ') then
        line = ' Verifying pixel map image           : '//
     +         gin(1:len1(gin))
        call output (line)
        call xyopen (lh, gin, 'old', maxnax, size)
        call xyclose (lh)
      end if
c
      if (bin.ne.' ') then
        line = ' Verifying mask image                : '//
     +         bin(1:len1(bin))
        call output (line)
        call xyopen (lh, bin, 'old', maxnax, size)
        call xyclose (lh)
      end if
c
      do i = 1, nspec
        if (iside(i).gt.0) then
          line = ' Verifying derivative spectrum image : '//
     +           spin(i)(1:len1(spin(i)))
        else
          line = ' Verifying spectrum image            : '//
     +           spin(i)(1:len1(spin(i)))
        end if
        call output (line)
        call xyopen (lh, spin(i), 'old', maxnax, size)
	call initco(lh)
        call rdhdi (lh, 'naxis', naxis, 0)
c
        call imminmax (lh, naxis, size, limin, limax)
        imin = min(imin, limin)
        imax = max(imax, limax)
c
        call axfndco (lh, 'VELO', naxis, 0, iax)
        if (iax.eq.0) call axfndco (lh, 'FREQ', naxis, 0, iax)
        if (iax.eq.0) then
          line = 'Spectrum image '//spin(i)(1:len1(spin(i)))//
     +           ' doesn''t have a velocity or frequency axis'
          call bug ('f', line) 
        end if         
c
        call w2wsco  (lh, iax, 'abspix', ' ', 1.0d0, 'absnat', ' ', v1)
        call w2wsco  (lh, iax, 'abspix', ' ', dble(size(iax)), 
     +                'absnat', ' ', v2)
c
        lvmin = min(v1,v2)
        lvmax = max(v1,v2)
        vmin = min(vmin, lvmin)
        vmax = max(vmax, lvmax)
	call finco(lh)
        call xyclose (lh)
      end do
c
      do i = 1, nofile
        line = ' Verifying overlay file              : '//
     +         ofile(i)(1:len1(ofile(i)))
        call output (line)
        call txtopen (lh, ofile(i), 'old', iostat)
        if (iostat.ne.0) call bug ('f', 'Could not open overlay file')
c
c Gridded or irregular locations ?
c
        call txtread (lh, line, ilen, iostat) 
        if (iostat.eq.0) then
          if (index(line,'GRID').ne.0 .or. 
     +        index(line,'grid').ne.0) then
            grid(i) = .true.
          else if (index(line,'IRREG').ne.0 .or.
     +             index(line,'irreg').ne.0) then
            grid(i) = .false.
          else 
            line = 'Unrecognized location type in overlay file '//
     +             ofile(i)(1:len1(ofile(i)))//
     +             ' Should be GRID or IRREGULAR'
            call bug ('f', line)
          end if
        else
          line = 'Error reading location type from overlay file '//
     +            ofile(i)(1:len1(ofile(i)))
        end if
        call txtclose (lh)
      end do
      call output (' ')
c
      end
c
c
      subroutine opimxyz (maxnax, in, lin, size, naxis)
c-----------------------------------------------------------------------
c     Open an image with XYZOPEN and return some header descriptors 
c
c   Input:
c     maxnax  Maximum number of allowed dimenions for image
c     in      Image name
c   Output:
c     lin     Handle for image
c     size    Size of axes
c     naxis   Number of axes
c-----------------------------------------------------------------------
      implicit none
      integer maxnax, lin, size(maxnax), naxis
      character*(*) in
cc
      character line*130
      integer len1
c-----------------------------------------------------------------------
      naxis = maxnax
      call xyzopen (lin, in, 'old', naxis, size)
      if (naxis.eq.0) then
        line = in(1:len1(in))//' has zero dimensions !!'
        call bug ('f', line)
      end if
c
      end
c
c
      subroutine pltspec (ispec, ipos, maxgrp, sgrps, iside, pos, 
     +    vfrac, npts, vrange, xspec, yspec, zspec, work, irange, vpn,
     +    vpw, tick, doaxes, doframe, slines, mark, naked, bline, 
     +    number, doerase, nspec, nofile, colour, blconly, iblc)
c-----------------------------------------------------------------------
c     Plot the current spectrum at the correct location on the image
c
c   Input
c     maxgrp  Max. number of spectrum segments
c     sgrps   CHannel segments to plot
c     iside   0 -> spectrum, 1-> 1-sided derivative spectrum,
c             1-> 2-sided derivative spectrum
c     ipos    Overlay number
c     ispec   Spectrum image number
c     pos     Location of spectrum in contour/pixel map pixels
c     vfrac   Width of spectrum as a fraction on the plot view-port
c             in teh x and y directions
c     npts    Number of points to plot in spectrum
c     vrange  Velocity range for window (always vrange(1) <= vrange(2))
c     irange  Intensity range for window
c     x,yspec Spectrum
c     zspec   Normalization spectrum.  If any element is 0, this means
c             there is no valid value for that element (channel/velocity)
c     work    Work array for taking of derivatives
c     vpn     Corners of contour/pixel map view-port in ndc (x1,x2,y1,y2)
c     vpw     Corners of contour/pixel map view-port in world coords
c     tick    Tick increments for spectra in x and y
c     doaxes  Draw x=0=y and label
c     doframe Draw frame around spectrum and label
c     slines  Line width and line type for spectrum
c     mark    Mark spectrum location
c     naked   DOn't plot numeric labels
c     bline   Axis line width
c     number  Number the spectra
c     doerase Erase background for number
c     nspec   The number of spectrum images
c     nofile  The number of overlay files
c     colour  COlour axes same as first spectrum
c     blconly Only label overlay spectrum in BLC of plot
c     iblc    Number of overlay which is in the BLC
c-----------------------------------------------------------------------
c
      implicit none
c
      integer npts, maxgrp, sgrps(2,maxgrp), slines(2), ispec,
     +  bline, ipos, nspec, nofile, iblc, zspec(npts), iside
      double precision pos(2)
      real xspec(npts), yspec(npts), work(npts), vfrac(2), vpn(4),
     +  vpw(4), vrange(2), irange(2), tick(2)
      logical doaxes, doframe, mark, naked, number, colour, blconly,
     +  doerase
cc
      double precision x, y
      real vpnl(4), delvx, delvy, vx, vy, frwx, frwy, xbox(4), ybox(4),
     +  dv, di, mx, my, cs
      integer i, j, k, nsgrps, ipts, is
      character str*6, xstr*6, ystr*6
c-----------------------------------------------------------------------
c
c Find location of spectrum in view-port in ndcs
c
      x = pos(1)
      y = pos(2)
      frwx = (x - vpw(1)) / (vpw(2) - vpw(1))
      frwy = (y - vpw(3)) / (vpw(4) - vpw(3))
c
      vx = vpn(1) + frwx*(vpn(2) - vpn(1))
      vy = vpn(3) + frwy*(vpn(4) - vpn(3))
c
c Find width and height of spectrum on view-port in ndcs
c
      delvx = vfrac(1) * (vpn(2) - vpn(1))
      delvy = vfrac(2) * (vpn(4) - vpn(3))
c
c Find corners of local view-port for this spectrum
c
      vpnl(1) = vx - 0.5*delvx
      vpnl(2) = vx + 0.5*delvx
      vpnl(3) = vy - 0.5*delvy
      vpnl(4) = vy + 0.5*delvy
c
c Set view-port and window
c
      call pgsvp (vpnl(1), vpnl(2), vpnl(3), vpnl(4))
      call pgswin (vrange(1), vrange(2), irange(1), irange(2))
      dv = vrange(2) - vrange(1)
      di = irange(2) - irange(1)
c
c Write overlay number if requested. 
c
      if (number .and. (nofile.gt.1 .or. 
     +    (nofile.eq.1 .and. ispec.eq.nspec)) ) then
        call pgsci (ispec+1)
        call strfi (ipos, '(i6)', str, is)
c
c Find blc location of text
c
        call pgqtxt (0.0, 0.0, 0.0, 0.0, str(1:is), xbox, ybox)
        mx = vrange(2) - xbox(4) - dv/50.0
        my = irange(2) - ybox(2) - di/50.0
c
c Write string
c
        call strerscg (doerase, 0.0, str(1:is), mx, my)
      end if
c
c Label spectrum as requested.
c
      if (nofile.gt.1 .or. (nofile.eq.1 .and. ispec.eq.1)) then
c
c Draw spectrum marker if requested. 
c
        if (mark) then
          mx = vrange(1) + dv/2.0
          my = irange(1) + di/2.0
          call pgsci (5)
          call pgqch (cs)
          call pgsch (2.0)
          call pgpt (1, mx, my, 18)
          call pgsch (cs)
        end if
c
        call pgsci (1)
        if (colour) call pgsci (ispec+1)
        call pgslw (bline)
c
c Draw and label box according to type requested; frame or axes
c 
        if ((blconly .and. ipos.eq.iblc) .or. .not.blconly) then
          if (doframe) then
            if (naked) then
              call pgbox ('BST', tick(1), 2, 'BST', tick(2), 2)
            else
              call pgbox ('BNST', tick(1), 2, 'BNST', tick(2), 2)
            end if
          else if (doaxes) then
c
c If the user wants x=0 make sure it us going to appear on the plot.  
c Else, give them a DOFRAME like option
c
            if (vrange(1)*vrange(2).le.0.0) then
              ystr = 'AST'
              if (naked) ystr = 'AST'
            else
              ystr = 'BNST'
              if (naked) ystr = 'BST'
            end if
c
c Similarly for the y=0 request.
c
            if (irange(1)*irange(2).le.0.0) then
              xstr = 'AST'
              if (naked) xstr = 'AST'
            else
              xstr = 'BNST'
              if (naked) xstr = 'BST'
            end if
c
c Draw the axes and maybe label numerically
c                    
            call pgbox (xstr, tick(1), 2, ystr, tick(2), 2)
c
c Write numeric labels for x and y axes.  If 'N' is in the
c options string, pgbox already wrote the numeric labels.
c Note that 'B' and 'N' will occur together.
c
            if (index(xstr,'B').eq.0 .and. index(xstr,'N').eq.0 
     +         .and. .not.naked) call nlabel ('x', tick(1), vrange)
            if (index(ystr,'B').eq.0 .and. index(ystr,'N').eq.0 
     +         .and. .not.naked) call nlabel ('y', tick(2), irange)
          end if
        end if
      end if
      call pgupdt
c
c Plot spectrum, but don't plot any points that have no valid 
c spectrum value.
c
      call pgslw (slines(1))
      call pgsls (slines(2))
      call pgsci (ispec+1)
c
      j = 0
      i = 1
      do while (i.le.npts)
        if (zspec(i).ne.0) then
          j = j + 1
          sgrps(1,j) = i
c
          if (i.eq.npts) then
            sgrps(2,j) = sgrps(1,j)
            i = i + 1
          else
            k = i + 1
            do while (zspec(k).ne.0 .and. k.lt.npts) 
              k = k + 1
            end do
c
            if (k.eq.npts) then
              if (zspec(k).eq.0) then
                sgrps(2,j) = k - 1
              else 
                sgrps(2,j) = k
              end if
              i = npts + 1
            else
              sgrps(2,j) = k - 1
              i = k + 1
            end if
          end if
        else
          i = i + 1
        end if
      end do
      nsgrps = j
c
c Plot spectrum in chunks if necessary to avoid blanked channels
c
      call pgbbuf
      if (nsgrps.gt.0) then
        do i = 1, nsgrps
          ipts = sgrps(2,i) - sgrps(1,i) + 1
          if (iside.gt.0) call deriv (iside, ipts, yspec(sgrps(1,i)), 
     +                               work)
          call pghline (ipts, xspec(sgrps(1,i)), yspec(sgrps(1,i)), 2.0)
        end do        
      end if
      call pgebuf
c
      end
c
c
      subroutine posdec (lun, pix3, maxtyp, ltypes, ofile, iline, 
     +                   aline, opos)
c---------------------------------------------------------------------
c     Decode string into positions list
c
c     Input
c       lun      Handle of image for coordianet transforms
c       pix3     Value of pixel on third axis appropriate
c                for the displayed image
c       maxtyp   Maximum number of potential label types
c       ltypes   Potential label types
c       ofile    Overlay file name
c       tr       Transformation matrix
c       blc      BLC of displayed window
c       iline    Line number being decoded
c       aline    Input string
c     Output
c       opos     Overlay location, list of: 
c                         X  Y     XSIZ YSIZ 
c                        PIXELS     ARCSEC
c
c---------------------------------------------------------------------
      implicit none
c
      integer iline, maxtyp, lun
      double precision opos(4), pix3
      character*(*) aline, ofile, ltypes(maxtyp)
cc 
      double precision off(2)
      integer i, slen, lena, ipres, npt, nuse, dsign(2), spos
      logical ok
      character str*4, estr*80, otype(2)*6
c
      integer len1
      character itoaf*4
c
      include 'mirconst.h'
      double precision rd
      integer maxnum
c
      parameter (maxnum = 10, rd = 180.0/dpi)
      double precision nums(maxnum)
      integer icomm(maxnum)
c--------------------------------------------------------------------
c
c Prepare string for matodf
c
      str = itoaf(iline)
      slen = len1(str)
      call strprpcg (maxnum, aline, icomm, ipres, lena)
      if (ipres.lt.2) call bug ('f', 
     +  'Insufficient numbers in overlay file in POSDEC')
c
c Fish out XOTYPE and YOTYPE
c
      otype(1) = aline(1:icomm(1)-1)
      call matchcg (iline, 'XOTYPE', otype(1), 'overlay', maxtyp,ltypes)
      otype(2) = aline(icomm(1)+1:icomm(2)-1)
      call matchcg (iline, 'YOTYPE', otype(2), 'overlay', maxtyp,ltypes)
c
c Don't lose DEC sign; DEC could be on either axis
c
      dsign(1) = 1
      dsign(2) = 1
      if (otype(1).eq.'dms') then
        spos = 2
        if (aline(icomm(spos)+1:icomm(spos)+1).eq.'-') dsign(1) = -1
      end if
      if (otype(2).eq.'dms') then
        if (otype(1).eq.'hms') then
          spos = 5
        else
          spos = 3
        end if
        if (aline(icomm(spos)+1:icomm(spos)+1).eq.'-') dsign(2) = -1
      end if
c
c Extract the numeric part of the line which remains
c
      do i = 1, maxnum
        nums(i) = 0.0d0
      end do
      call matodf (aline(icomm(2)+1:lena), nums, ipres-2, ok)
      if (.not.ok) then
        estr = 'Error decoding overlay # '//str(1:slen)//
     +         ' in file '//ofile
        call bug ('f', estr)
      end if
c
c Convert the overlay locations in true coordinates to pixels
c
      off(1) = 0.0d0
      off(2) = 0.0d0
      npt = 1
      call ol2pixcg (lun, pix3, otype, off, dsign, nums(npt),
     +               opos, nuse)
      if (nuse.gt.ipres-2) then
        estr = 'Not enough fields for overlay # '//str(1:slen)//
     +         ' in file '//ofile
        call bug ('f', estr)
      end if
      npt = npt + nuse
c
c Do the binning half-sizes in arcseconds.  
c
      if (npt.le.ipres-2) then
        opos(3) = nums(npt)
        npt = npt + 1
      else
        opos(3) = 0.0
      end if
      if (Npt.le.ipres-2) then
        opos(4) = nums(npt)
      else
        opos(4) = 0.0
      end if
c
      end
c
c
      subroutine posdec2 (aline, inc, bin)
c---------------------------------------------------------------------
c     Decode string into positions information
c
c     Input
c       aline    Input string
c     Output
c       inc      Increment across image in arcsec in x and y
c       bin      SPatial binning size for each spectrum in arcsec
c                in x and y
c---------------------------------------------------------------------
      implicit none
c
      real inc(2), bin(2)
      character*(*) aline
cc 
      integer i, lena, ipres
      logical ok
c
      integer maxnum
      parameter (maxnum = 4)
c
      double precision nums(maxnum)
      integer icomm(maxnum)
c--------------------------------------------------------------------
c
c Prepare string for matodf
c
      call strprpcg (maxnum, aline, icomm, ipres, lena)
      if (ipres.lt.2) call bug ('f', 
     +  'Insufficient numbers in overlay file in POSDEC2')
c
c Now extract the numeric part of the line
c
      do i = 1, maxnum
        nums(i) = 0.0d0
      end do
      call matodf (aline(1:lena), nums, ipres, ok)
      if (.not.ok) call bug ('f', 'Error decoding overlay file')
c
      inc(1) = nums(1)
      inc(2) = nums(2)
      bin(1) = nums(3)
      bin(2) = nums(4)
c
c Bin half-size defaults to zero
c
      if (ipres.eq.2) then
        bin(1) = 0.0
        bin(2) = 0.0
      else if (ipres.eq.3) then
        bin(2) = 0.0
      end if
c
      end
c
c
      subroutine region (maxnax, cin, lc, csize, cnaxis, gin, lg, gsize,
     +  gnaxis, ibin, jbin, blc, trc, win, ngrps, grpbeg, ngrp)
c----------------------------------------------------------------------
c     Finish key routine inputs for region of interest now.    Also
c     return the header items for all further use when computing
c     axis value related quantities.  These are the first contour
c     or pixel map axis descriptors encountered.
c
c  Input:
c    maxnax        Maximum number of allowed dimenions for image
c    l*            Image handles
c    *in           Image names
c    *naxis        Numbers of axes
c    *size         Size of axes
c    i,jbin        Spatial increment and binning sizes in x and y
c  Output:
c    blc,trc       3-D Hyper-rectangle surrounding region of interest
c    win           Size of region of interest for each of up to
c                  3 dimensions.
c    ngrps         Number of groups of channels.
c    grpbeg        List of start planes for each group of planes
c                  that are all  to be avearged together. A new
c                  group is begun at every interruption to the
c                  continuity of the selected channels, or if the
c                  channel increment is reached.
c    ngrp          Number of channels in each group of channel to
c                  be averaged together for each sub-plot.
c
c----------------------------------------------------------------------
      implicit none
c     
      integer maxnax, cnaxis, gnaxis, csize(maxnax), gsize(maxnax), 
     +  blc(*), trc(*), win(2), ngrp(*), grpbeg(*), 
     +  ngrps, ibin(2), jbin(2), lc, lg
      character*(*) cin, gin
cc
      include 'maxdim.h'
      integer maxbox
      parameter (maxbox = 1024)
c
      integer boxes(maxbox), i, kbin(2), naxis, size(3), lh
      character line*80, itoaf*1
c----------------------------------------------------------------------
c
c The pixel map and contour images have all been checked to be 
c of the same dimensions.  Use any for 'region' keyword. There
c must be a pixel map or contour image.
c
      if (cin.ne.' ') then
        call boxinput ('region', cin, boxes, maxbox)
        call boxset (boxes, cnaxis, csize, ' ')
        naxis = cnaxis
        lh = lc
      else if (gin.ne.' ') then
        call boxinput ('region', gin, boxes, maxbox)
        call boxset (boxes, gnaxis, gsize, ' ')
        naxis = gnaxis
        lh = lg
      end if
      call keyfin
c
c Find hyper-rectangle surrounding region of interest from highest 
c dimension image involved (i.e., 2-D/3-D).
c
      call boxinfo (boxes, 3, blc, trc)
      do i = 1, min(3,naxis)
        call rdhdi (lh, 'naxis'//itoaf(i), size(i), 0)
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
c Find list of start planes and number of planes for all selected
c image planes which are to be averaged together.  Signal to
c CHNSELCG that we want, for each group of channels, the averaging
c number to be equal to the number of contiguous channels available
c in that group
c   
      kbin(1) = 0
      kbin(2) = 0
      call chnselcg (blc, trc, kbin, maxbox, boxes, ngrps, grpbeg, ngrp)
c     
c Tell user
c
      call output (' ')
      call output ('The following planes will be averaged for the '//
     +             'contour/pixel map images')
      do i = 1, ngrps
        write (line, 100) grpbeg(i), grpbeg(i)+ngrp(i)-1
100     format ('Planes ', i4, ' to ', i4)
        call output (line)
      end do
      call output (' ')
c
      end
c
c
      subroutine specblnk (lh, pos, blc, trc, nx, ny, mask, 
     +                     allblnk, miss)
c-----------------------------------------------------------------------
c     Find out if spatial pixels over which this spectrum is averaged
c     are blanked or not
c
c Input:
c   lh      Handle of spatial image
c   pos     Spectrum location, x, y (contour/pixel map spatial pixels) 
c           xsize, ysize (arcsec)
c   blc     BLC of displayed region in contour/pixel map pixels
c   trc     TRC of displayed region in contour/pixel map pixels
c   nx,ny   SIze of mask image
c   mask    Mask.  0 means blanked, 1 unblanked
c Output:
c   allblnk All pixels in the spectrum average area are blank in the mask
c   miss    This spectrum falls off the spatial image
c-----------------------------------------------------------------------
      implicit none
c
      integer lh, blc(2), trc(2), nx, ny, mask(nx,ny)
      double precision pos(4)
      logical allblnk, miss
cc
      integer i1, i2, j1, j2, i, j, bblc(2), ttrc(2)
c
      include 'mirconst.h'
      double precision  win(2), wout(2), wblc(2), wtrc(2)
      character typei(2)*6, typeo(2)*6
c-----------------------------------------------------------------------
c
c Positions that are centred off the edge of the pixel map/contour
c image are not displayed.  
c
      if (pos(1).lt.blc(1) .or. pos(1).gt.trc(1) .or. 
     +    pos(2).lt.blc(2) .or. pos(2).gt.trc(2)) then
        miss = .true.
        return
      end if
c
c Find spatial averaging region for this spectrum on pixel map or
c contour image.   Spatial pixel increment may be different from
c spectrum image, so this blanking check is approximate only.
c
      do i = 1, 2
        typei(i) = 'abspix'
        typeo(i) = 'arcsec'
        win(i) = pos(i)
      end do
      call w2wco (lh, 2, typei, ' ', win, typeo, ' ', wout)
c
      do i = 1, 2
        typei(i) = 'arcsec'
        typeo(i) = 'abspix'
        win(i) = wout(i) - pos(i+2)
      end do
      call w2wco (lh, 2, typei, ' ', win, typeo, ' ', wblc)
      do i = 1, 2
        win(i) = wout(i) + pos(i+2)
      end do
      call w2wco (lh, 2, typei, ' ', win, typeo, ' ', wtrc)
c
      i1 = wblc(1)
      j1 = wblc(2)
      i2 = wtrc(1)
      j2 = wtrc(2)
c
c Convert to subimage pixels
c
      bblc(1) = min(i1,i2) - blc(1) + 1
      bblc(2) = min(j1,j2) - blc(2) + 1
      ttrc(1) = max(i1,i2) - blc(1) + 1
      ttrc(2) = max(j1,j2) - blc(2) + 1
c
c For regions that straddle the displayed region boundaries,
c tuncate to boundaries.  
c
      bblc(1) = max(1,bblc(1))
      bblc(2) = max(1,bblc(2))
      ttrc(1) = min(nx,ttrc(1))
      ttrc(2) = min(ny,ttrc(2))
c
c Now see if region is all blanked
c
      allblnk = .true.
      do j = bblc(2), ttrc(2)
        do i = bblc(1), ttrc(1)
          if (mask(i,j).ne.0) then
            allblnk = .false.
            goto 999
          end if
        end do
      end do
999   continue
c
      end
c
c
      subroutine specin (ls, snaxis, sblc, velax, virsiz, norm, scale,
     +                   xspec, yspec, nspec, skip, prof, mprof)
c-----------------------------------------------------------------------
c     Read in the binned spectrum.
c
c  Input
c    ls       Handle for spectrum image
c    snaxis   Number of axes in image
c    masks    True if there are some blanked pixels in the image
c    sblc,trc blc and trc of sub-cube to read
c    velax    Axis nuimber of velocity axis
c    virsiz   Sizes of sub-cube axes, in order vxy
c    norm     Normalize peak of spectrum to 1.0
c    scale    Scale factor to apply to intensities
c  Input/output
c    prof     I/O array for profile
c    mprof    I/O array for mask
c  Output:
c    x,yspec  Arrays containing the x and y values of the spectrum
c    nspec    Array containing normalization factor for spectrum
c             0 means there were no points for that channel.
c    skip     SKip this one because all zero. 
c
c-----------------------------------------------------------------------
      implicit none
c
      integer ls, snaxis, sblc(snaxis), virsiz(snaxis), velax, nspec(*)
      real xspec(*), yspec(*), scale, prof(*)
      logical norm, skip, mprof(*)
cc
      include 'maxdim.h'
      double precision crval, crpix, cdelt
      real  ymax, ymin
      integer i, j, nprofs, nread
      character itoaf*1
      logical allblnk
c-----------------------------------------------------------------------     
c
c Initialize
c
      do i = 1, virsiz(1)
        nspec(i) = 0
        yspec(i) = 0.0
      end do
      allblnk = .true.
      skip = .false.
      nprofs = virsiz(2)*virsiz(3)
      nread = virsiz(1)
c
c Read and bin
c
      do j = 1, nprofs
        call xyzprfrd (ls, j, prof, mprof, nread)
c
        do i = 1, virsiz(1)
          if (mprof(i)) then
            nspec(i) = nspec(i) + 1
            yspec(i) = yspec(i) + prof(i)
          end if
        end do
      end do
c
c Normalize
c
      ymin =  1.0e30
      ymax = -1.0e30
      call rdhdd (ls, 'crpix'//itoaf(velax), crpix, 0.0d0)
      call rdhdd (ls, 'crval'//itoaf(velax), crval, 0.0d0)
      call rdhdd (ls, 'cdelt'//itoaf(velax), cdelt, 0.0d0)
      do i = 1, virsiz(1)
c
c Compute abcissa array
c
        j = i + sblc(velax) - 1
        xspec(i) = (dble(j)-crpix)*cdelt + crval
c
        if (nspec(i).ne.0) then
          yspec(i) = scale * yspec(i) / real(nspec(i))
          ymin = min(ymin,yspec(i))
          ymax = max(ymax,yspec(i))
          allblnk = .false.
        else
          yspec(i) = 0.0
        end if
      end do
c
c Renormalize
c
      if (allblnk) then
        skip = .true.
      else if (norm) then
        skip = .false.
        if (ymax.ne.0.0) then
          do i = 1, virsiz(1)
            yspec(i) = yspec(i) / ymax
          end do
        end if
      end if
c
      end
c
c
      subroutine specloc (lh, ls, snaxis, ssize, pos, vrange, velax, 
     +                    sblc, strc, fits)
c-----------------------------------------------------------------------
c     Work out the bounding box in absolute pixels for the sub-cube to
c     average for the current spectrum position. 
c
c Input:
c   l*      Image handles
c   snaxis  NUmber of axes in spectrum image
c   ssize   Size of spectrum image
c   pos     Spectrum location, x, y (contour/pixel map spatial pixels) 
c           xsize, ysize (arcsec)  x and y are full image unbinned
c           pixels not just the displayed region pixels
c   vrange  Velocity range of interest
c   velax   The axis corresponding to the velocity axis of the
c           current spectrum image
c Output:
c   sblc,trc
c           BLC and TRC of sub-cube to read in.
c   fits(2) True if spatial and spectral areas fit partly or wholly 
c           in spectrum image. 
c-----------------------------------------------------------------------
      implicit none
c
      integer snaxis, sblc(snaxis), strc(snaxis), ssize(snaxis), 
     +  velax, lh, ls
      double precision pos(4)
      real vrange(2)
      logical fits(2)
cc
      double precision win(3), wout(3), wcen(3)
      real dv
      integer i, j, pt(3), i1, i2, naxis
      character tpi(2)*4, tsi(3)*4, typei(3)*6, typeo(3)*6
c-----------------------------------------------------------------------
c
c Work out order of spectrum image spatial axes with respect 
c to spatial image spatial axes
c
c
      call axtypco (lh, 2, 0, tpi)
      call axtypco (ls, 3, 0, tsi)
      do i = 1, 3
        if (i.ne.velax) then
          if (tsi(i).eq.tpi(1)) then
            pt(i) = 1
          else if (tsi(i).eq.tpi(2)) then
            pt(i) = 2
          else
            call bug ('f', 'Spatial axes of spectrum image and '//
     +                'spatial image(s) are not similar')
          end if
        end if
      end do
c     
c Convert the spatial centre of the spectrum from pixel map/ contour 
c pixels to world
c 
      do i = 1, 2
        typei(i) = 'abspix'
        win(i) = pos(i)
        typeo(i) = 'absnat'
      end do
      call w2wco (lh, 2, typei, ' ', win, typeo, ' ', wout)
c
c Now work out the centre of the spectrum in spectrum image
c coordinates (linear for velocity, arcsec for spatial)
c
      naxis = min(3,snaxis)
      j = 1
      do i = 1, naxis
        if (i.eq.velax) then
          win(i) = (vrange(1) + vrange(2)) / 2.0
          typei(i) = 'absnat'
          typeo(i) = 'absnat'
        else
          win(i) = wout(pt(i))
          typei(i) = 'absnat'
          typeo(i) = 'arcsec'
          j = j + 1
        end if
      end do
      call w2wco (ls, naxis, typei, ' ', win, typeo, ' ', wcen)
c
c Now offset to find the BLC of the subcube
c
      dv = abs(vrange(2) - vrange(1)) / 2.0
      do i = 1, naxis
        if (i.eq.velax) then
          typei(i) = 'absnat'
          win(i) = wcen(i) - dv 
        else
          win(i) = wcen(i) - pos(pt(i)+2)
          typei(i) = 'arcsec'
        end if
        typeo(i) = 'abspix'
      end do
      call w2wco (ls, naxis, typei, ' ', win, typeo, ' ', wout)
      do i = 1, 3
        sblc(i) = nint(wout(i))
      end do
c
c Now offset to find the TRC of the subcube
c
      do i = 1, naxis
        if (i.eq.velax) then
          typei(i) = 'absnat'
          win(i) = wcen(i) + dv  
        else
          typei(i) = 'arcsec'
          win(i) = wcen(i) + pos(pt(i)+2)
        end if
        typeo(i) = 'abspix'
      end do
      call w2wco (ls, naxis, typei, ' ', win, typeo, ' ', wout)
      do i = 1, naxis
        strc(i) = nint(wout(i))
      end do
c
c Make sure BLC and TRC in increasing order
c
      do i = 1, 3
        i1 = min(sblc(i),strc(i))
        i2 = max(sblc(i),strc(i))
        sblc(i) = i1
        strc(i) = i2
      end do
c
c Make sure sub-cube fits in image
c
      fits(1) = .true.
      fits(2) = .true.
      do i = 1, 3
c
c Completely missed
c
        if ( (sblc(i).lt.1 .and. strc(i).lt.1) .or.
     +       (sblc(i).gt.ssize(i) .and. strc(i).gt.ssize(i))) then
          if (i.ne.velax) then  
            fits(1) = .false.
          else
            fits(2) = .false.
          end if
        else
c
c Partly fits; show it
c
          sblc(i) = min(ssize(i), max(1,sblc(i)))
          strc(i) = max(1, min(ssize(i),strc(i)))
        end if
      end do
c
c Fill in rest of subcube to keep XYZSETUP happy
c
      if (snaxis.gt.3) then
        do i = 4, snaxis
          sblc(i) = 1
          strc(i) = 1
        end do
      end if
c
      end
c
c
      subroutine specsiz (ls, vrange, iax, size)
c---------------------------------------------------------------------
c     WOrk out how many pixels long the spectrum for this spectrum
c     image for the desired velocity range so that we can allocate
c     memory dynamically
c
c  Input
c    ls        Handle of image
c    vrange    Velcoty range in km/s
c    iax       Spectral axis
c  Output
c    size      SIze of spectrum
c
c-----------------------------------------------------------------------
      implicit none
c
      real vrange(2)
      integer size, iax, ls
cc
      double precision cdelt      
      character itoaf*1
c-----------------------------------------------------------------------
      call rdhdd (ls, 'cdelt'//itoaf(iax), cdelt, 0.0d0)
      size = nint(abs((vrange(2) - vrange(1)) / cdelt) + 0.5)
c
c Add a couple to be sure.  This computation is not used critically
c
      size = size + 2
c
      end
c
c
      subroutine setlgc (bgcol, labcol)
c-----------------------------------------------------------------------
c     Set line graphics colours
c
c  Input
c    bgcol  colour of background 0-> black, 1->white
c  OUtput
c    colour indices to use
c-----------------------------------------------------------------------
      implicit none
      integer labcol, bgcol
c-----------------------------------------------------------------------
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
      end
