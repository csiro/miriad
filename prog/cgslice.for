      program cgslice
c-----------------------------------------------------------------------
c= CGSLICE - Display an image and interactively extract 1-D slices
c          
c& nebk
c: plotting
c+
c	CGSLICE displays an image via a contour plot or a pixel map
c	representation on a PGPLOT device.  The cursor (or a text file 
c	with slice positions) is then used to define the end points of 
c	1-D slices which are marked on the image, and then plotted. 
c
c	After the image has been displayed, use the mouse (any button)
c	or keyboard (enter any character) to define each end of the slice.
c	You can define many slices if you wish.  When you have marked
c	all the slices you want, click the right button of the mouse
c	(or enter 'X' from the keyboard).  You are then offered the 
c	choice to redo all the slices if you didn't like them (enter 
c	'R' from the keyboard) or to continue on and display the slices
c	(click the right button or enter 'X' from the keyboard).
c
c	Options to fit a Gaussian plus a baseline are available.  If
c	you invoke the fitting, it is activated after each of all the
c	slices defined is plotted.  With the cursor (any button of
c	a mouse or any characer from the keyboard) you define the initial
c	guesses for the Gaussian parameters.   The data, fitted model 
c	and residual are then plotted.   You can the redo the fitting
c	if you wish (right button of mouse or entering 'X' from keyboard)
c	before proceeding to fit the next slice that you defined.
c
c	Options to save the slice values, slice positions and slice
c	models are available.
c
c	If you ask CGSLICE to display several sub-plots (e.g. each a
c	different channel from a cube), the slicing is activated after
c	each sub-plot is drawn.
c
c	Blanked pixels are not displayed (or saved) and each slice is 
c	divided into segments with good points between blanked pixels. 
c
c	Manipulation of the device colour lookup table is available
c	when you display with a pixel map representation (formerly
c	called a "grey scale")
c
c@ in
c	The input image.
c@ type
c	Specifies the image type given in the IN keyword.  Minimum
c	match is supported (note that "pixel" was formerly "grey"
c	which is still supported).     Choose from:
c
c	"contour"  (contour)
c	"pixel"    (pixel map)
c
c	Default is "pixel"
c@ region
c	Region of interest.  Choose only one spatial region (bounding
c	box only supported), but as many spectral regions (i.e., 
c	multiple IMAGE specifications) as you like.  If you display 
c	3-D image, the slicing option is activated after each sub-plot 
c	(channel or group of channels; see CHAN below) is drawn.
c	Default is full image
c@ xybin
c	Upto 4 values.  These give the spatial increment and binning
c	size in pixels for the x and y axes to be applied to the selected
c	region.   If the binning size is not unity, it must equal the 
c	increment.  For example, to bin up the image by 4 pixels in 
c	the x direction and to pick out every third pixel in the y 
c	direction, set XYBIN=4,4,3,1
c	Defaults are 1,XYBIN(1),XYBIN(1),XYBIN(3)
c@ chan
c	2 values. The first is the channel increment, the second is
c	the number of planes to average, for each sub-plot.  Thus
c	CHAN=5,3  would average groups of 3 channels together, starting
c	5 channels apart such as: 1:3, 6:8, 11:13 ...   The channels
c	available are those designated by the REGION keyword.  A new
c	group of channels (sub-plot) is started if there is a
c	discontinuity in the REGION selected channels (such as
c	IMAGE(10,20),IMAGE(22,30).
c
c	Defaults are 1,1
c@ slev
c	2 values.   First value is the type of contour level scale
c	factor.  "p" for percentage and "a" for absolute.   Second
c	value is the level to scale LEVS by.  Thus  SLEV=p,1  would
c	contour levels at LEVS * 1% of the image peak intensity.
c	Similarly, SLEV=a,1.4e-2   would contour levels at LEVS * 1.4E-2
c	Default is no additional scaling of LEVS
c@ levs
c	Levels to contour for first image, are LEVS times SLEV
c	(either percentage of the image peak or absolute).
c	Defaults try to choose something sensible
c@ range
c       4 values. These are the image intensity range to display (min to max),
c       the transfer function type and the colour lookup table for the displayed
c       pixel map image.  The transfer function type can be one of "lin" (linear),
c       "sqr" (square root), "log" (logarithmic), and "heq" (histogram
c       equalization).  The colour lookup table is an integer from 1 to 8
c       specifying a lookup table. Valid values are 1 (b&w), 2 (rainbow),
c       3 (linear pseudo colour), 4 (floating zero colour contours), 5 (fixed
c       zero colour contours), 6 (rgb), 7 (background), 8 (heat) and 
c	9 (absolute b&w).  If you enter a negative integer, then the 
c	reversed lookup table is displayed.
c
c       The transfer function changes available with OPTIONS=FIDDLE are in
c       addition (on top of) to the selections here, but the colour lookup
c       table selections will replace those selected here.
c
c       Default is linear between the image minimum and maximum with
c       a b&w lookup table.   You can default the intensity range with
c       zeros, viz. "range=0,0,log,-2" say.
c@ xrange
c	The slice display x-axis range.  This may be useful if you use
c	OPTIONS=accum (see below).  Default is autoscale.
c@ yrange
c	The slice display y-axis range.  This may be useful if you use
c	OPTIONS=accum (see below).  Default is autoscale.
c@ device
c	The PGPLOT plot device, such as plot.plt/ps. No default.
c@ nxy
c	Number of sub-plots in the x and y directions on the page.
c	Defaults choose something sensible
c@ labtyp
c       Two values.  The spatial label type of the x and y axes.
c       Minimum match is active.  Select from:
c       
c	"hms"       the label is in H M S (e.g. for RA)
c	"dms"       the label is in D M S (e.g. for DEC)
c	"arcsec"    the label is in arcsecond offsets
c	"arcmin"    the label is in arcminute offsets
c	"absdeg"    the label is in degrees
c	"reldeg"    the label is in degree offsets
c		    The above assume the  pixel increment is in radians.
c	"abspix"    the label is in pixels
c	"relpix"    the label is in pixel offsets
c	"abskms"    the label is in Km/s
c	"relkms"    the label is in Km/s offsets
c	"absghz"    the label is in GHz
c	"relghz"    the label is in GHz offsets
c	"abslin"    the label is in linear coordinates as defined by 
c	 	    the header you might call this the natural axis label
c	"rellin"    the label is in offset linear coordinates
c	"none"      no labels or ticks on the axes
c       
c	All offsets are from the reference pixel.  
c	Defaults are "abspix", LABTYP(1) unless LABTYP(1)="hms"
c	whereupon LABTYP(2) defaults to "dms" (for RA and DEC).
c@ options
c	Task enrichment options.  Minimum match is active.
c
c	"accumulate" means accumulate slices from different sub-plots on 
c	  the same display.  By default, the slice display is cleared 
c	  before the slices from the current sub-plot are displayed.  
c	  The initial slice window extrema are defined from the first 
c	  sub-plot so slices from succeeding sub-plots may not fit 
c	  unless you use keywords XRANGE and YRANGE.
c
c	"noimage"  means do not generate the pixel map or contour plot
c	  display of the image.   Useful if you have specified the slice
c	  locations with a text file via the POSIN keyword and you don't
c	  want to see the slice locations displayed on the image. The region
c	  of the viewsurface used for the slice display is larger with 
c	  this option active.
c
c       "fiddle" means enter a routine to allow you to interactively change
c         the display lookup table.  You can cycle through a variety of   
c         colour lookup tables, as well as alter a linear transfer function
c         by the cursor location, or by selecting predefined transfer
c         functions (linear, square root, logarithmic, histogram equalization)
c       
c         For hard copy devices (e.g. postscript), a keyboard driven
c         fiddle is offered; you can cycle through different colour tables
c         and invoke the predefined transfer functions, but the linear
c         fiddler is not available.   In this way you can make colour
c         hardcopy plots.
c
c	"wedge" means that if you are drawing a pixel map, also draw
c	  and label a wedge to the right of the plot, showing the map 
c	  of intensity to colour.
c
c	"fit" means fit a Gaussian to each slice.  The cursor is used
c	  to make the initial estimates of the Gaussian parameters.
c
c	"baseline" means fit a baseline (offset and slope) as well as 
c	  a Gaussian when OPTIONS=fit.
c
c	"xrange" means when OPTIONS=fit, use the cursor to define an x-range
c	  outside of which pixels will be excluded from the fit.
c
c	"unequal"  means display image with unequal scales in x and y. The
c	  default is that the scales are equal.
c
c	"3value"   means label each sub-plot with the appropriate value
c	  of the third axis (e.g. velocity or frequency for an
c	  xyv ordered cube, position for a vxy ordered cube).
c	"3pixel"   means label each sub-plot with the pixel value of
c	  the third axis.
c
c         Both "3pixel" and "3value" can appear, and both will be written
c         on the plot.  They are the average values when the third axis is 
c	  binned up with CHAN.  If the third axis is not velocity or 
c	  frequency, the units type for "3VALUE" will be chosen to be the 
c	  complement of any like axis in the first 2. E.g., the cube is 
c	  in vxy order and LABTYP=abskms,arcsec the units for the "3VALUE" 
c	  label will be arcsec.  If LABTYP=abskms,hms the "3VALUE" label 
c	  will be DMS (if the third [y] axis is declination).
c
c	"noerase"  Don't erase a snugly fitting rectangle into which the 
c	  "3-axis" value string is written.
c
c	"grid" means overlay a  coordinate grid on the display	
c@ csize
c	Three values.  Character sizes in units of the PGPLOT default
c	(which is ~ 1/40 of the view surface height) for the plot axis
c	labels, the velocity/channel labels and the slice plot labels
c	Defaults choose something sensible.
c@ posin
c	The BLC and TRC of the slices can be defined in this text file
c	rather than being defined interactively with the cursor. The
c	slices defined in this file will be marked on the 2-D image
c	(unless you set OPTIONS=noimage) display and then the slices 
c	extracted, displayed and optionally fitted and saved.
c
c	Entries in this file can be white space or comma delimitered or 
c	both.  All lines beginning with # are ignored.
c
c	                **** DO NOT USE TABS **** 
c
c	Double quotes " are used below to indicate a string.  The "
c	should not be put in the file.   For the string parameters
c	discussed below, you can abbreviate them with minimum match.
c
c	Each line describes a slice and should be as follows:
c
c	 ##### The columns in each line must be
c
c	    1       2     3   4    5   6   7   8       Logical column
c	-------------------------------------------
c	 XOTYPE  YOTYPE   X1  Y1   X2  Y2  CS  CE      where
c
c
c	XOTYPE and YOTYPE  give the coordinate types of the slice BLC and
c	TRC in the file for the x- and y-directions, respectively.  
c	Choose from:
c
c	 "hms", "dms", "arcsec", "arcmin", "absdeg", "reldeg", "abspix", 
c	 "relpix", "abslin", "rellin", "absghz", "relghz", 
c	 "abskms", & "relkms"  as described in the keyword LABTYP.  
c	Note that %OTYPE does not depend upon what you specified for LABTYP.
c
c	X1,Y1 defines the BLC of the slice in the nominated OTYPE
c	coordinate system (X- and Y-OTYPE can be different).  
c	X2,Y2 defines the TRC of the slice in the nominated OTYPE
c	coordinate system (X- and Y-OTYPE can be different).  
c
c	  For %OTYPE = "abspix ", "relpix", "arcsec", "arcmin", "absdeg",
c		       "reldeg", "absghz", "relghz", "abskms", "relkms", 
c		       "abslin" and "rellin"   X1,Y1 and X2,Y2 are all 
c		        single numbers.
c
c	  For %OTYPE = "hms" or "dms", the X and/or Y location is/are replaced
c	  by three numbers such as  HH MM SS.S or DD MM SS.S.  Thus if
c	  XOTYPE=hms & YOTYPE=dms then the line should be structured like
c
c	   hms  dms  HH MM SS.S DD MM SS.S   HH MM SS.S DD MM SS.S  CS  CE
c	      or perhaps
c	   hms  relpix HH MM SS.S Y1    HH MM SS.S Y2   CS  CE
c
c	CS to CE is the channel range (image planes) from which the slice
c	is to be extracted.  If you specify only CS than the slice is 
c	extracted from that channel.  If CS=0 then the slice is extracted
c	from all channels.  If CS and CE are both omitted, the default is
c	to extract the slice from all channels.
c
c@ posout
c	An ascii file into which the BLC and TRC for each slice are saved.
c	The columns of the file are slice number, the slice BLC, TRC and
c	the start and end channels from the image that this slice came
c	from (you may have used the CHAN keyword).   The BLC and TRC are
c	in arcseconds from the reference pixel if the axes have radian
c	increments (RA/DEC etc).  Otherwise they are in offset pixels 
c	from the reference pixel.
c@ valout
c	An ascii file into which the slices are saved.  If the file already
c	exists, new slices are appended to it.  The columns of the file are
c	the slice number, the slice segment number, the slice segment point
c	number, the slice abcissa and the slice value.    
c@ modout
c	An ascii file into which the Gaussian models for the slices  are
c	saved (OPTIONS=fit or OPTIONS=fit,baseline).  If the file already
c	exists, new models are appended to it.  The columns of the file
c	are the slice number, the model peak, centre, FWHM, baseline offset 
c	and baseline slope.
c
c--
c
c  History:
c    nebk 16dec93  Original version. 
c    nebk 29jan94  Add "heq" and "sqr" to greyscale transfer functions.
c	           Add options=wedge,fiddle. Add labtype=none.  Add 
c                  ability to delete both slice ends.
c    nebk 02mar94  New call and location for SETLABCG
c    nebk 11mar94  Add spatial binning
c    nebk 03jun94  Clarify use of region keyword
c    nebk 21jun94  Use OPIMCG to open files
c    nebk 28aug94  Adapt to convert input true world coordinates (POSIN)
c                  to linear world coordinates.  Convert output linear 
c                  world coordinates (POSOUT) to true world coordinates.  
c                  Also call new LAB3CG which now labels true world 
c                  of third axis.    LInearize axis descriptors at 
c                  centre of displayed region
c    nebk 14oct94  Better cursor positioning in gaussian fitting
c    nebk 23dec94  Make sure selected region no bigger than image
c    nebk 05jan95  Use new PGIMAG in favour of PGGRAY adding support   
c                  for fiddling of lookup table for hardcopy devices.
c                  Make use of new PGBAND when defining slices.  Decouple
c                  slice and image window label displacements
c    nebk 20feb95  Make sure PGIMAG writes black on white for hardcopy.
c                  Ammend for new wedge call sequences.  Add lookuptable
c                  to "range" keyword. Move to image type "pixel"
c                  instead of "grey"
c    nebk 28mar95  Remove annoying restriction that slices cannot
c                  begin and end on blanked pixels
c    nebk 10apr95  Add doc for absolute b&w lookup table
c    nebk 03sep95  Detect black/white background, add non-linear 
c		   ticks and grid
c
c Notes:
c
c   SLice abcissa values are still in linear world coordiantes as
c   are gaussian fits.  Its all too hard.
c To do:
c
c  * add XTYPE and YTYPE to posout file ??  Maybe options=cgslice
c
c  * When slice horizontal or vertical can set abcissa units more
c    cleverly
c
c  * Scale data before fitting
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
c
      integer maxlev, nxdef, nydef, maxnsl, nltype, nbins
      real grid, wedwid, wedisp, tfdisp
      parameter (maxlev = 50, nxdef = 4, nydef = 4, maxnsl = 20,
     +   nbins = 128, grid = 0.25, nltype = 15, wedisp = 1.0, 
     +   wedwid = 0.05, tfdisp = 0.5)
c
      integer ipim, ipp, ipims, ipnim, ipslx(maxnsl), ipsly(maxnsl),
     +  ipsls(maxnsl), ipsle(maxnsl)
c
      double precision cdelt(maxnax), crval(maxnax), crpix(maxnax)
      real levs(maxlev), pixr(2), tr(6), cs(3), pixr2(2), scale(2),
     +  bound(4,maxnsl), vblc(2,2), vtrc(2,2), xrange(2), yrange(2),
     +  tfvp(4), wdgvp(4), cumhis(nbins), dmm(2)
      real slev, xmin, xmax, ymin, ymax, vx, vy, vxsize, vysize, ydispb,
     +  ydispbs, xdispl, xdispls, groff, blank, epoch, sxmin, sxmax, 
     +  symin, symax
c
      integer blc(3), trc(3), size(maxnax), win(maxnax), 
     +  grpbeg(maxchan), ngrp(maxchan), srtlev(maxlev),
     +  nslice, nslp(maxnsl), slsize(maxnsl),  slpos(6,maxnsl),
     +  seg(2,maxdim), nseg(maxnsl), his(nbins)
      integer nx, ny, nlevs, lin, naxis, ierr, pgbeg, iostat, ilen,
     +  nlast, ngrps, lval, lposi, lposo, lmod, i, j, k, jj, icol, 
     +  iax, ipage, wedcod, ibin(2), jbin(2), kbin(2), krng(2), 
     +  coltab, concol, labcol, slbcol, bgcol
c
      character ctype(maxnax)*9, labtyp(2)*6, ltype(nltype)*6
      character in*64, pdev*64, xlabel*40, ylabel*40, xlabel2*40, 
     +  ylabel2*40, xopts*20, yopts*20, hard*20, xxopts*22, yyopts*22,
     +  trfun*3, levtyp*1, fslval*80, fslposo*80, fslposi*80, fslmod*80,
     +  units*8
c
      logical do3val, do3pix, eqscale, doblnk, mask, dopixel,  gaps,
     +  doerase, redisp, accum, radians, none, noimage, dofit, dobord,
     +  dobase, doxrng, dofid, dowedge, first, dunsl, dogrid
c
      integer len1
c
      data ltype  /'hms   ', 'dms   ', 'abspix', 'relpix', 
     +            'arcsec', 'arcmin', 'absghz', 'relghz', 
     +            'abskms', 'relkms', 'abslin', 'rellin', 
     +            'absdeg', 'reldeg', 'none  '/
      data ipage, scale /0, 0.0, 0.0/
      data xrange, yrange /0.0, 0.0, 0.0, 0.0/
      data dunsl /.false./
      data xdispls, ydispbs /3.5, 3.5/
c-----------------------------------------------------------------------
      call output ('CgSlice: version 03-Sep-95')
      call output ('Non-linear coordinate labels now correctly handled')
      call output ('New options=grid to overlay coordinate grid')
      call output (' ')
c
c Get user inputs
c
      call inputs (nltype, ltype, maxlev, in, ibin, jbin, kbin, 
     +   levtyp, slev, levs, nlevs, pixr, trfun, coltab, pdev, labtyp,
     +   do3val, do3pix, eqscale, nx, ny, cs, dopixel, doerase, 
     +   accum, noimage, dofit, dobase, fslval, fslposi, fslposo, 
     +   fslmod, xrange, yrange, doxrng, dofid, dowedge, dogrid)
c
c Open image and see if axes in radians
c
      call opimcg (maxdim, maxnax, in, lin, naxis, size, epoch, 
     +             mask, crpix, cdelt, crval, ctype)
      call rdhda (lin, 'bunit', units, ' ')
      radians = .false.
      call axfndcg ('RAD', 1, ctype(1), iax)
      if (iax.eq.1) call axfndcg ('RAD', 1, ctype(2), iax)
      if (iax.eq.1) radians = .true.
c
c Finish key inputs for region of interest now
c
      call region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +             win, maxchan, grpbeg, ngrp, ngrps)
c
c Try to allocate memory for image
c
      call memalloc (ipim,  win(1)*win(2), 'r')
      call memalloc (ipnim, win(1)*win(2), 'i')
      if (.not.noimage .and. dopixel .and. trfun.ne.'lin') 
     +  call memalloc (ipims, win(1)*win(2), 'r')
c
c Open output text files
c
      call opento (units, radians, fslval, fslposo, fslmod, lval, 
     +             lposo, lmod)
c
c Compute contour levels or check pixel map for log/sqr offset
c
      if (dopixel) then
        call grfixcg (pixr, lin, naxis, size, trfun, pixr2,
     +                groff, blank)
      else
        call conlevcg (.false., maxlev, lin, naxis, size, levtyp,
     +                 slev, nlevs, levs, srtlev)
        blank = -99999999.0
      end if
c
c Linearize axis descriptors if non-pixel labels requested
c
      call linco (lin, labtyp, blc, trc, grpbeg, ngrp, ctype,
     +            crval, crpix, cdelt)
c
c Work out array index limits, coordinate transformation array and
c labels.   Also return header items.
c
      call limitscg (labtyp, blc, trc, naxis, epoch, crpix, cdelt, 
     +   crval, ctype, .false., xmin, xmax, ymin, ymax, ibin, jbin,
     +   tr, xlabel, ylabel)
c
c Work out number of plots per page and number of plots
c
      call nxnycg (nxdef, nydef, ngrps, nx, ny, nlast)
      gaps = .false.
      if (ngrps.eq.1 .or. nx*ny.eq.1) gaps = .true.
c
c Work out default character sizes for axis and channel labels
c
      call defchrcg (nx, ny, cs)
c
c Set slice labels
c
      if (units.eq.' ') then
        ylabel2 = 'Intensity'
      else 
        ylabel2 = 'Intensity ('//units(1:len1(units))//')'
      end if
c
      if (radians) then
        xlabel2 = 'offset (arcsec)'
      else
        xlabel2 = 'offset (pixels)'
      end if
c
c Open plot device
c
      ierr = pgbeg (0, pdev, 1, 1)
      if (ierr.ne.1)then
        call pgldev
        call bug ('f', 'Error opening plot device')
      endif
      call pgqinf ('hardcopy', hard, ilen)
      if (hard.eq.'YES' .and. fslposi.eq.' ') call bug ('f',
     +   'Must specify keyword "posin" for hardcopy device')
      call pgpage
      call pgscf(1)
c
c Find out if background white
c
      call bgcolcg (bgcol)
c
c Set line graphics colours
c
      call setlgc (bgcol, labcol, concol, slbcol)
c
c Init OFM routines
c       
      if (dopixel) call ofmini
c
c Work out if wedge outside or inside subplots. Also work out
c if plotting one wedge per subplot or one wedge for all
c
      call wedgincg (hard, dofid, dowedge, nx, ny, 1, trfun, wedcod)
c
c Set label displacements from axes and set PGTBOX labelling
c option strings
c
      call setlabcg (dogrid, labtyp, ymin, ymax, xdispl, ydispb, 
     +               xopts, yopts)
c
c Work out viewport encompassing all sub-plots and the viewport
c that defines the slice plotting region.   Also return the
c the viewport size of sub-plots, and the gap between sub-plots.
c
      call vpsiz (noimage, dofid, nx, ny, cs, xdispl, ydispb, xdispls,
     +  ydispbs, wedcod, wedisp, wedwid, tfdisp, vblc, vtrc, vxsize, 
     +  vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales
c requested or if scales provided by user
c
      if (.not.noimage) then
        call vpadjcg (hard, eqscale, scale, vblc(1,2), vblc(2,2),
     +   vtrc(2,2), nx, ny, blc, trc, naxis, crval, crpix, cdelt, 
     +   ctype, tfvp, wdgvp, vxsize, vysize)
c
c Set viewport location of first sub-plot
c
        vx = vblc(1,2)
        vy = vtrc(2,2) - vysize
      end if
c
c Loop over number of sub-plots
c
      do k = 1, ngrps
         if (mod(k,nx*ny).eq.1 .or. nx*ny.eq.1) ipage = ipage + 1
         jj = k - (ipage-1)*nx*ny
         krng(1) = grpbeg(k)
         krng(2) = ngrp(k)
c
c Set viewport and window for current sub-plot
c
         if (.not.noimage) then
           call pgsvp (vx, vx+vxsize, vy, vy+vysize)
           call pgswin (xmin, xmax, ymin, ymax)
         end if
c
c Read in image  
c
         call readimcg (.true., mask, blank, lin, ibin, jbin, krng,
     +         blc, trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
c
c Some imaging chores
c
         if (.not.noimage .and. dopixel) then
c
c Save image if needed
c
           if (trfun.ne.'lin') call copyimcg (win(1)*win(2), 
     +                          memr(ipim), memr(ipims))
c
c Apply transfer function directly to image if desired
c
           if (trfun.ne.'lin') call apptrfcg (pixr, trfun, groff, 
     +       win(1)*win(2), memi(ipnim), memr(ipim), nbins, his,
     +       cumhis)
c
c Apply user given OFM or b&w as default to hardcopy device
c
           if (hard.eq.'YES') call ofmcol (coltab, pixr2(1), pixr2(2))
c
c Draw wedge if needed
c
           if (wedcod.eq.1 .or. wedcod.eq.2) then
             call pgsci (labcol)
             call pgsch (cs(1))
             call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins,
     +                     cumhis, wdgvp, pixr(1), pixr(2))
           end if
         end if
c
c Loop over redisplay loop while user wants another go
c
         redisp = .true.
         first = .true.
         do while (redisp)
           if (.not.noimage) then
             if (dopixel) then
c
c Modify OFM for hardcopy devices here; must be done before PGIMAG
c called. Take complement of b&w transfer functions if background white
c 
               if (hard.eq.'YES') then
                 if (dofid) call ofmmod (tfvp, win(1)*win(2), 
     +             memr(ipim), memi(ipnim), pixr2(1), pixr2(2))
                 if (bgcol.eq.1) call ofmcmp
               end if
c
c Draw pixel map and apply user given OFM
c
               call pgimag (memr(ipim), win(1), win(2), 1, win(1), 1,
     +                      win(2), pixr2(1), pixr2(2), tr)
               if (hard.eq.'NO') call ofmcol (coltab, pixr2(1), 
     +                                        pixr2(2))
c
c Retake b&w complement for white background
c
               if (hard.eq.'YES' .and. bgcol.eq.1) call ofmcmp
             else 
c
c Draw contours
c
               call pgsci (concol)
               call conturcg (blank, .false., win(1), win(2), doblnk,
     +                        memr(ipim), nlevs, levs, tr, 0.0)
             end if
c
c Label if first time through redisplay loop; axes not erased
c
             call pgsch (cs(1))
             call pgsci (labcol)
             if (first) call axlabcg (.false., gaps, .false., nx, ny,
     +         ngrps, nlast, k, xopts, yopts, xdispl, ydispb, labtyp, 
     +         xlabel, ylabel, xxopts, yyopts)
c
c Draw frame, write numeric labels, ticks and optional grid
c
             call labaxcg (lin, first, blc, trc, krng, labtyp, 
     +                     xxopts, yyopts)
             call pgupdt
c
c Draw wedge if inside subplot
c
             if (wedcod.eq.3) then
               call pgsci (labcol)
               call pgsch (cs(1))
               call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins,
     +                       cumhis, wdgvp, pixr(1), pixr(2))
             end if
c
c Write velocity or channel label
c
             if (do3val .or. do3pix) then
               call pgsch (cs(2))
               call pgsci (1)
               call lab3cg (lin, doerase, do3val, do3pix, labtyp,
     +                      grpbeg(k), ngrp(k))
             end if
             call pgupdt
c
c Modify OFM for interactive devices here
c
             if (dofid .and. hard.eq.'NO') then
               call pgsch (cs(1))
               call ofmmod (tfvp, win(1)*win(2), memr(ipim),
     +                      memi(ipnim), pixr2(1), pixr2(2))
             end if
           end if
c
c Define slice ends with cursor or read from file
c
           redisp = .false.
           if (fslposi.eq.' ' .and. .not.noimage) then
             call curpos (win(1), win(2), labtyp, ibin, jbin,
     +         blc, naxis, cdelt, crpix, crval, ctype, 
     +         redisp, maxnsl, nslice, slpos)
c
c Erase subplot or write positions file if desired
c
             if (redisp) then
               call erswincg (xmin, xmax, ymin, ymax)
             else
               if (fslposo.ne.' ') call slposw (lin, lposo, krng,
     +           radians, blc, ibin, jbin, maxnsl, nslice, slpos)
             end if
           else
             call txtopen (lposi, fslposi, 'old', iostat)
             if (iostat.ne.0) 
     +       call bug ('f', 'Error opening input text file'//fslposi)
c
             call posdec (lin, krng, noimage, lposi, win(1), win(2), 
     +         blc, trc, ibin, jbin, nltype, ltype, maxnsl, size, 
     +         nslice, slpos)
             call txtclose (lposi)
           end if
         end do
c
c Generate slices 
c
         if (nslice.gt.0) then
           if (.not.dunsl .or. .not.accum) then
             sxmin =  1.0e32
             sxmax = -1.0e32
             symin =  1.0e32
             symax = -1.0e32
           end if
c
c Loop over number of slices
c
           none = .true.
           do i = 1, nslice
c
c Mark slice on display if given by input text file
c
             if (fslposi.ne.' ' .and..not.noimage) then
               call setcolcg (i, icol)
               call pgsci (icol)
               call slmark (blc, ibin, jbin, labtyp, slpos(1,i), 
     +                      naxis, crval, cdelt, crpix, ctype)
             end if
c
c Allocate memory for slice 
c
             call slsiz (slpos(1,i), grid, slsize(i))
             call memalloc (ipslx(i), slsize(i), 'r')
             call memalloc (ipsly(i), slsize(i), 'r')
c
c Generate slice. Use copy of image if transfer function applied.
c
             ipp = ipim
             if (.not.noimage .and. dopixel .and. trfun.ne.'lin') 
     +          ipp = ipims
             call slice (ibin, jbin, slpos(1,i), grid, win(1), win(2),
     +          memr(ipp), memi(ipnim), radians, naxis, cdelt, 
     +          slsize(i), memr(ipslx(i)), memr(ipsly(i)), nslp(i), 
     +          bound(1,i), nseg(i), seg)
c
c Allocate memory for slice segment pointers and copy in
c if slice not all blanked
c
             if (nslp(i).gt.0) then
               none = .false.
               call memalloc (ipsls(i), nseg(i), 'i')
               call memalloc (ipsle(i), nseg(i), 'i')
               do j = 1, nseg(i)
                 memi(ipsls(i)+j-1) = seg(1,j)
                 memi(ipsle(i)+j-1) = seg(2,j)
               end do
c
c Update extrema
c
               if (.not.dunsl .or. .not.accum) call exup 
     +           (bound(1,i), sxmin, sxmax, symin, symax)
             else
c
c If the slice was all blanked, free up its memory allocation now
c
               call memfree (ipslx(i), slsize(i), 'r')
               call memfree (ipsly(i), slsize(i), 'r')
             end if
           end do
c
c Slices are computed; setup for plotting if there is something to plot
c
           if (.not.none) then
             call pgsch (cs(3))
c
c Loop over slices
c
             do i = 1, nslice
c
c Erase erase old plots if desired
c
               if ( (i.eq.1 .and. dunsl .and. .not.accum)  .or. dofit)
     +           call serase (vtrc(2,1))
c
c Set new window and draw box if desired
c
               dobord = (i.eq.1.and.(.not.dunsl .or. .not.accum))
     +                   .or. dofit
               if (dofit) then
                 call drawbox (dobord, slbcol, vblc, vtrc, xrange, 
     +              yrange, bound(1,i), bound(3,i), bound(2,i), 
     +              bound(4,i), xlabel2, ylabel2, xdispls, ydispb)
               else
                 call drawbox (dobord, slbcol, vblc, vtrc, xrange, 
     +              yrange, sxmin, sxmax, symin, symax, xlabel2, 
     +              ylabel2, xdispls, ydispbs)
               end if
               dunsl = .true.
c
               if (nslp(i).gt.0) then
c
c Plot the slice
c
                 call slplot (i, nslp(i), nseg(i), memr(ipslx(i)), 
     +              memr(ipsly(i)), memi(ipsls(i)), memi(ipsle(i)))
c
c Save the slice if desired
c
                 if (fslval.ne.' ') 
     +             call slsave (lval, i, nseg(i), memr(ipslx(i)), 
     +               memr(ipsly(i)), memi(ipsls(i)), memi(ipsle(i)))
c
c Do Gaussian fit if desired
c
                 if (dofit) call gaufit (lmod, dobase, doxrng, i, 
     +             nslp(i), nseg(i),ipslx(i), ipsly(i),  ipsls(i), 
     +             ipsle(i), xdispls, ydispbs, xlabel2, ylabel2, slbcol)
               end if
             end do
           end if
         end if
c
c Free up memory. 
c
         do i = 1, nslice
           if (nslp(i).gt.0) then
             call memfree (ipslx(i), slsize(i), 'r')
             call memfree (ipsly(i), slsize(i), 'r')
             call memfree (ipsls(i), nseg(i), 'i')
             call memfree (ipsle(i), nseg(i), 'i')
           end if
         end do
c
c Increment sub-plot viewport locations 
c
         call subinccg (k, nx, ny, vblc(1,2), vtrc(2,2), vxsize, 
     +                  vysize, 0.0, 0.0, vx, vy)
c
c Page plot device
c
         if (mod(k,nx*ny).eq.0 .and. k.lt.ngrps) call pgpage
      end do
c
c Close up
c
      call memfree (ipim,  win(1)*win(2), 'r')
      call memfree (ipnim, win(1)*win(2), 'i')
      if (.not.noimage .and. dopixel .and. trfun.ne.'lin')
     +   call memfree (ipims, win(1)*win(2), 'i')
      call xyclose(lin)
      if (fslval.ne.' ') call txtclose (lval)
      if (fslposo.ne.' ') call txtclose (lposo)
      if (fslmod.ne.' ') call txtclose (lmod)
      call pgend
c
      end
c
c
      subroutine curget (ibin, jbin, blc, nx, ny, naxis, crval,
     +   cdelt, crpix, ctype, labtyp, ip, ipos, wpos, cch)
c-----------------------------------------------------------------------
c     Get one end of slice
c
c  Input
c    i,jbin  Pixel increments
c    blc     BLC of displayed image
c    nx,ny   x and y sizes of displayed subimage
c    naxis   NUmber of axes in image
c    cr*     Axis descriptors
c    labtyp  Axis label types
c    ip      Number of points accumulated so far.  SHould be 0 or 1
c  Output
c    wpos    Position in world coordinates for the point under cursor
c    ipos    Position in absolute binned subimage pixels for the point 
c            under cursor 
c    cch     Character read by cursor. 
c-----------------------------------------------------------------------
      implicit none
      integer naxis, nx, ny, blc(2), ibin, jbin, ipos(2),
     +  ip
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      real wpos(2)
      character cch*1, ctype(naxis)*(*), labtyp(2)*(*)
cc
      integer k, ci
      double precision pix(2)
      real wx, wy
      logical ok, more
c-----------------------------------------------------------------------
      call pgqci (ci)
      more = .true.
      wx = wpos(1)
      wy = wpos(2)
c
      do while (more)
        if (ip.eq.1) then
          call pgband (1, 1, wx, wy, wpos(1), wpos(2), cch)
        else
          call pgcurs (wpos(1), wpos(2), cch)
        end if
c
        call ucase (cch)
        if (cch.eq.'X') then
          more = .false.
        else if (cch.eq.'D') then
          more = .false.
        else if (cch.eq.'A') then
c
c Convert world coordinate to unbinned full image pixels 
c
          do k = 1, 2
            call w2pixcg (dble(wpos(k)), k, labtyp(k), naxis, crval, 
     +                    crpix, cdelt, ctype, pix(k), ok)
          end do
c
c Convert to nearest binned subimage pixels
c
          call ppconcg (1, blc(1), ibin, pix(1))
          call ppconcg (1, blc(2), jbin, pix(2))
          ipos(1) = nint(pix(1))
          ipos(2) = nint(pix(2))
c
          if (ipos(1).lt.1 .or. ipos(1).gt.nx .or.
     +        ipos(2).lt.1 .or. ipos(2).gt.ny) then
            call bug ('w', 'Cursor off image, try again')
          else
            more = .false.
          end if
        end if
      end do
c
      end
c
c
      subroutine curpos (nx, ny, labtyp, ibin, jbin, blc, naxis,
     +  cdelt, crpix, crval, ctype, redisp, maxnsl, nslice, slpos)
c-----------------------------------------------------------------------
c     Define slice locations with cursor
c
c  Input:
c     nx,ny   Size of image
c     i,jbin  PIxel increment sizes
c     labtyp  axis label types
c     blc     blc of window being displayed
c     naxis   Numebr of axes in image
c     cdelt   Pixel increments
c     crpix   Reference pixel
c     crval   Reference value
c     ctype   Axis types
c     maxnsl  Maximum number of slices to make
c Output:
c     redisp  Redisplay subplot and redo slices
c     nslice  Number of slices
c     slpos   BLC (xyz) and TRC (xyz) of slices.  x and y in absolute 
c             binned subimage pixels.  z in absolute image pixels. 
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nx, ny, blc(2), naxis, maxnsl, nslice,
     +  slpos(6,maxnsl), ibin, jbin
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      character*(*) labtyp(2), ctype(naxis)
      logical redisp
cc
      real wpos(2,2), xd, yd, cs, wldpos(2)
      integer icol, ip, ci, ipos(2,2), simpos(2)
      character cch*1, aline*132
      data wldpos /0.0, 0.0/
c-----------------------------------------------------------------------
      call output (' ')  
      call output ('Begin slicing')
      call output 
     +  ('To define slice end, click  left  button or enter "A"')
      call output 
     +  ('To delete slice end, click middle button or enter "D"')
      call output 
     +  ('To exit              click right  button or enter "X"')
      call output (' ')
c
      call pgqch (cs)
      call pgsch (0.7)
c
      call setcolcg (1, icol)
      call pgsci (icol)
c
      cch = ' '
      nslice = 0
      ip = 0
      do while (cch.ne.'X') 
c
c Make cursor selection
c
        call curget (ibin, jbin, blc, nx, ny, naxis, crval, 
     +     cdelt, crpix, ctype, labtyp, ip, simpos, wldpos, cch)
        if (cch.eq.'A') then
          if (ip.eq.0) then
            ip = ip + 1 
            call slput (simpos, wldpos, ipos(1,ip), wpos(1,ip))
            call pgpt (1, wpos(1,ip), wpos(2,ip), 17)
          else if (ip.eq.1) then
            if (simpos(1).eq.ipos(1,1) .and. simpos(2).eq.ipos(2,1))then
              call output ('Degenerate slice, enter second point again')
            else
              ip = ip + 1 
              call slput (simpos, wldpos, ipos(1,ip), wpos(1,ip))
              call pgpt (1, wpos(1,ip), wpos(2,ip), 17)
            end if
          else if (ip.eq.2) then
c
c We have two points, that makes a slice
c
            nslice = nslice + 1
            call slput2 (slpos(1,nslice), ipos)
c
c Join up the slice ends with an arrow
c
            call pgarro (wpos(1,1), wpos(2,1), wpos(1,2), wpos(2,2))
c
c If we have run out of space for more slices, stop now
c          
            if (nslice.eq.maxnsl) then
              call output ('Maximum number of slices allowed reached')
              cch = 'X'
            else
c
c Assign first end of next slice with the point in hand
c
              call setcolcg (nslice+1, icol)
              call pgsci (icol)
              ip = 1
              call slput (simpos, wldpos, ipos(1,ip), wpos(1,ip))
              call pgpt (1, wpos(1,ip), wpos(2,ip), 17)
            end if
          end if
        else if (cch.eq.'D') then
c
c Delete previous point
c
          if (ip.eq.0) then
            call output ('No points available to delete')
          else
            call pgqci (ci)
            call pgsci (0)
            call pgpt (1, wpos(1,ip), wpos(2,ip), 17)
            call pgsci (ci)
            ip = ip - 1
c
c Recover world coordinates of first point in slice (may not exist)
c
            wldpos(1) = wpos(1,1)
            wldpos(2) = wpos(2,1)
          end if
        else if (cch.eq.'X') then
c
c Flush out last slice
c
          if (ip.eq.2) then
            nslice = nslice + 1
            call slput2 (slpos(1,nslice), ipos)
            call pgarro (wpos(1,1), wpos(2,1), wpos(1,2), wpos(2,2))
          end if
        end if
      end do
c
c Redo or continue ?
c
      if (nslice.gt.0) then
        call output (' ')
        write (aline,100) nslice
100     format ('There are ', i4, ' slices defined')
        call output (aline)

        call output (' ')
        call output 
     +    ('To display   the slices click right button or   enter "X"')
        call output 
     +    ('To redisplay the (sub)image and redo the slices enter "R"')
        call output (' ')
c
        call pgcurs (xd, yd, cch)
        call ucase (cch)
        if (cch.eq.'R') then
          redisp = .true.
        else 
          redisp = .false.
        end if
      else
        redisp = .false.
      end if
      call pgsch (cs)
c
      end
c
c
      subroutine decopt  (do3val, do3pix, eqscale, doerase, accum,
     +   noimage, dofit, dobase, doxrng, dofid, dowedge, grid)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     do3val    True means label sub-plots with value of third axis
c     do3pix    True means label sub-plots with pixel of third axis
c     eqscale   True means plot with x and y scales
c     doerase   Erase rectangle behind "3-axis" strings
c     accum     Accumulate slice plots without clearing display
c               between sub-plots
c     noimage   Don't display the image
c     dofit     Fit Gaussians
c     dobase    Fit basline too
c     doxrng    Use cursor to define x range when fitting Gaussian
c     dofid     FIddle lookup table
c     dowedge   Draw pixel map wedge
c     grid      Overlay coordinate grid
c-----------------------------------------------------------------------
      implicit none
c
      logical do3val, do3pix, eqscale, doerase, accum, noimage, dofit,
     +  dobase, doxrng, dofid, dowedge, grid
cc
      integer maxopt
      parameter (maxopt = 12)
c
      character opshuns(maxopt)*10
      logical present(maxopt)
      data opshuns /'3value',     '3pixel  ', 'unequal ', 'noerase',
     +              'accumulate', 'noimage',  'fit',      'baseline',
     +              'xrange',     'fiddle',   'wedge',    'grid'/
c-----------------------------------------------------------------------
      call optcg ('options', opshuns, present, maxopt)
c
      do3val   =      present(1)
      do3pix   =      present(2)
      eqscale  = .not.present(3)
      doerase  = .not.present(4)
      accum    =      present(5)
      noimage  =      present(6)
      dofit    =      present(7)
      dobase   =      present(8)
      doxrng   =      present(9)
      dofid    =      present(10)
      dowedge  =      present(11)
      grid     =      present(12)
c
      end
c
c
      subroutine drawbox (dobord, slbcol, vblc, vtrc, xrange, yrange, 
     +   sxmin, sxmax, symin, symax, xlabel, ylabel, xdispl, ydispb)
c-----------------------------------------------------------------------
c     Set the viewport and draw the box for the slice display
c
c  Input:
c   dobord       Draw the border and label as well as setting 
c                viewport and window
c   slbcol       Colour index to draw frame with
c   vblc,trc     Viewport
c   x,yrange     User given plot extrema
c   sx,ymin,max  Auto plot extrema
c   x,ylabel     x- and y-axis labels
c   xdispl,ydispb
c                Y and x axis label offsets
c
c-----------------------------------------------------------------------
      implicit none
      real vblc(2), vtrc(2), xrange(2), yrange(2), sxmin, sxmax, symin,
     +  symax, ydispb, xdispl
      integer slbcol
      logical dobord
      character*(*) xlabel, ylabel
cc
      real lim(4)
c-----------------------------------------------------------------------
      call pgsvp (vblc(1), vtrc(1), vblc(2), vtrc(2))
      call fixlim (.true., xrange, yrange, sxmin, sxmax, symin, 
     +             symax, lim)
      call pgswin (lim(1), lim(3), lim(2), lim(4))
c
      if (dobord) then
        call pgsci (slbcol)
        call pgtbox ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        call pgmtxt ('B', ydispb, 0.5, 0.5, xlabel)
        call pgmtxt ('L', xdispl, 0.5, 0.5, ylabel)
        call pgupdt
      end if
c
      end
c
c
      subroutine exup (bound, sxmin, sxmax, symin, symax)
c-----------------------------------------------------------------------
c     Update extrema
c
c  Input
c    bound           Extrema from current slice. xmin,ymin,xmax,ymax
c  Input/output
c    sx,ymin,max     Extrema from all slices
c
c-----------------------------------------------------------------------
      implicit none
      real sxmin, sxmax, symin, symax, bound(4)
c-----------------------------------------------------------------------
      sxmin = min(sxmin,bound(1))
      sxmax = max(sxmax,bound(3))
      symin = min(symin,bound(2))
      symax = max(symax,bound(4))
c
      end
c
c
      subroutine fixlim (dostr, xrange, yrange, x1, x2, y1, y2, lim)
c-----------------------------------------------------------------------
c  Input:
c   dostr         Stretch by 5%
c   x,yrange      USer specified slice display extrema
c   x1,x2,y1,y2   Auto scaled extrema
c  Output
c   lim           Extrema to use for plot
c-----------------------------------------------------------------------
      implicit none
      real x1, x2, y1, y2, lim(4), xrange(2), yrange(2)
      logical dostr
c-----------------------------------------------------------------------
c
c Stretch autoscale limits by 5% and catch min=max.  User's
c min=max checked in subroutine inputs
c
      if (xrange(1).eq.0 .and. xrange(2).eq.0.0) then
        if (dostr) then
          call fixlm2 (x1, x2, lim(1), lim(3))
        else
          lim(1) = x1
          lim(3) = x2
        end if
        call fixlm3 (lim(1), lim(3))    
      else
        lim(1) = xrange(1)
        lim(3) = xrange(2)
      end if
c
      if (yrange(1).eq.0 .and. yrange(2).eq.0.0) then
        if (dostr) then
          call fixlm2 (y1, y2, lim(2), lim(4))
        else
          lim(2) = y1
          lim(4) = y2
        end if
        call fixlm3 (lim(2), lim(4))
      else
        lim(2) = yrange(1)
        lim(4) = yrange(2)
      end if
c
      end
c
c
      subroutine fixlm2 (dmin, dmax, dmin2, dmax2)
c-----------------------------------------------------------------------
c     Stretch extrema by 5%
c-----------------------------------------------------------------------
      implicit none
      real dmin, dmax, dmin2, dmax2
cc
      real absmax, delta
c     
      delta = 0.05 * (dmax - dmin)
      absmax = max(abs(dmax),abs(dmin))
      if (delta.le.1.0e-5*absmax) delta = 0.01 * absmax
      if (delta.eq.0.0) delta = 1
      dmin2 = dmin - delta
      dmax2 = dmax + delta
c
      end

      subroutine fixlm3 (dmin, dmax)
c-----------------------------------------------------------------------
c     Fix lim=max extrema
c-----------------------------------------------------------------------
      implicit none
      real dmin, dmax
c
      if (dmin.eq.dmax) then
        if (dmin.eq.0.0) then
          dmin = -0.05
          dmax =  0.05
        else
          dmin = dmin - 0.05*dmin
          dmax = dmax + 0.05*dmax
        end if
      end if
c
      end
c
c
      subroutine gauder (xsol, dfdx, npar, npts)
c-----------------------------------------------------------------------
c     Compute the derivatives of the data minus the gaussian
c
c   Input in common
c     ipx     The pointer to the MEMR memory buffer for the x values
c             of the slice
c   Input
c     npar    The number of parameters we are solving for
c     npts    The number of points in the slice
c   Input/output
c     xsol    The current guess for the peak, pos'n and fwhm
c     dfdx    The derivatives w.r.t. peak, pos'n and fwhm
c
c-----------------------------------------------------------------------
      implicit none
      integer npar, npts
      real xsol(npar), dfdx(npar,npts)
cc
      include 'maxdim.h'
      include 'mem.h'
      integer i, ipx, ipy
      double precision fac1, fac2, fac3, phisq, delx, dexpun
      common /trans/ ipx, ipy
c-----------------------------------------------------------------------
      phisq = xsol(3) * xsol(3)
      fac1 = -4.0d0 * log(2.0d0)
      fac2 = -2.0 * fac1
c
      do i = 1, npts
        delx = memr(ipx+i-1) - xsol(2)
        fac3 = dexpun(fac1*delx**2/phisq)
c
        dfdx(1,i) = -fac3
        dfdx(2,i) = -fac2 * xsol(1) * delx  / phisq * fac3
        dfdx(3,i) = -fac2 * delx**2 * xsol(1) / xsol(3)**3 * fac3
      end do
c
      end
c
c
      subroutine gauder2 (xsol, dfdx, npar, npts)
c-----------------------------------------------------------------------
c     Compute the derivatives of the data minus the gaussian + baseline
c
c   Input in common
c     ipx     The pointer to the MEMR memory buffer for the x values
c             of the slice
c   Input
c     npar    The number of parameters we are solving for
c     npts    The number of points in the slice
c   Input/output
c     xsol    The current guess for the peak, pos'n and fwhm, offset
c             and slope
c     dfdx    The derivatives w.r.t. peak, pos'n, fwhm, offset and slope
c
c-----------------------------------------------------------------------
      implicit none
      integer npar, npts
      real xsol(npar), dfdx(npar,npts)
cc
      include 'maxdim.h'
      include 'mem.h'
      integer i, ipx, ipy
      double precision fac1, fac2, fac3, phisq, delx, dexpun
      common /trans/ ipx, ipy
c-----------------------------------------------------------------------
      phisq = xsol(3) * xsol(3)
      fac1 = -4.0d0 * log(2.0d0)
      fac2 = -2.0 * fac1
c
      do i = 1, npts
        delx = memr(ipx+i-1) - xsol(2)
        fac3 = dexpun(fac1*delx**2/phisq)
c
        dfdx(1,i) = -fac3
        dfdx(2,i) = -fac2 * xsol(1) * delx  / phisq * fac3
        dfdx(3,i) = -fac2 * delx**2 * xsol(1) / xsol(3)**3 * fac3
        dfdx(4,i) = -1.0
        dfdx(5,i) = -memr(ipx+i-1)
      end do
c
      end
c
c
      subroutine gaufit (lmod, dobase, doxrng, islice, n, nseg, ipslx,
     +   ipsly, ipsls, ipsle, xdispl, ydispb, xlabel, ylabel, slbcol)
c-----------------------------------------------------------------------
c     Fit a Gaussian to the slice
c
c  Input:
c   lmod      Handle for model file (0 means don't write)
c   dobase    FIt baseline too
c   doxrng    Select x rage with cursor
c   islice    SLice number
c   n         Number of points in slice
c   nseg      Number of segments in slice
c   ipslx,y   Pointers to MEMR memory buffer for the x and y
c             values of the slice
c   ipsls     Pointer to MEMR buffer which gives index of first point of 
c             first segment of slice
c   ipsle     Pointer to MEMR buffer which gives index of last point of 
c             first segment of slice
c   x,ydispb,l
c          Label displacements from axes
c   x,ylabel
c          labels
c   slbcol    Colour index for slice plot frame
c-----------------------------------------------------------------------
      implicit none
      real xdispl, ydispb
      integer n, ipslx, ipsly, ipsls, ipsle, nseg, islice, lmod, slbcol
      character*(*) xlabel, ylabel
      logical dobase, doxrng
cc
      include 'maxdim.h'
      include 'mem.h'
      integer itmax
      real eps1, eps2
      parameter (itmax = 100, eps1 = 0.0, eps2 = 0.001)
c
      real wx1, wx2, wy1, wy2, xsol(5), h(5), dx(5), aa(25),
     +  wy1s, wy2s, xc, yc
      integer ipf, ipfp, ipdfdx, ipxx, ipyy, ifail, iostat, is, n2
      character aline*132
      logical more
c
      integer len1
      external gaufun, gauder, gaufun2, gauder2
c
c Transfer X values of slice to EXTERNAL functions via common
c
      common /trans/ ipxx, ipyy
      data h /5*0.1/
      data xc, yc /0.0, 0.0/
c-----------------------------------------------------------------------
      more = .true.
      do while (more)
        call output (' ')
        call pgqwin (wx1, wx2, wy1, wy2)
        wy1s = wy1
        wy2s = wy2
c
c Get initial guesses for peak, position and FWHM with cursor
c
        call getpeak (wx1, wx2, wy1, wy2, xsol, xc, yc)
        call getfwhm (wx1, wx2, wy1, wy2, xsol, xc, yc)
        xsol(4) = 0.0
        xsol(5) = 0.0
c
c Optionally get x range and set pointers passed out in common
c
        if (doxrng) then
          call getxrng (wx1, wx2, wy1, wy2, n, memr(ipslx), 
     +                  is, n2, xc, yc)
          ipxx = ipslx + is - 1
          ipyy = ipsly + is - 1
c          write (*,*) 'n2,is=', n2,is
        else
          n2 = n
          ipxx = ipslx
          ipyy = ipsly
        end if
c
c       xsol(1) = 0.5
c       xsol(2) = 50.0
c       xsol(3) = 20.0
c       xsol(4) = 0
c       xsol(5) = 0
c
c       write (*,*) 'Enter initial guess (pos)'
c        read (*,*) xsol(2)
c
c        write (*,*) 'peak,pos,fwhm=',xsol
c
c Now do fit
c
        call memalloc (ipf,  n2, 'r')
        call memalloc (ipfp, n2, 'r')
        if (dobase) then
          call memalloc (ipdfdx, 5*n2, 'r')
        else
          call memalloc (ipdfdx, 3*n2, 'r')
        end if
c
        if (.not.dobase) then
          call nllsqu (3, n2, xsol, h, itmax, eps1, eps2, .true.,
     +                 ifail, gaufun, gauder, memr(ipf), 
     +                 memr(ipfp), dx, memr(ipdfdx), aa)
        else
          call nllsqu (5, n2, xsol, h, itmax, eps1, eps2, .true., 
     +                 ifail, gaufun2, gauder2, memr(ipf), 
     +                 memr(ipfp), dx, memr(ipdfdx), aa)
        end if
        if (ifail.eq.1) call bug ('w', 'Fit failed: singular matrix')
c
        call memfree (ipf,  n2, 'r')
        call memfree (ipfp, n2, 'r')
        if (dobase) then
          call memfree (ipdfdx, 5*n2, 'r')
        else
          call memfree (ipdfdx, 3*n2, 'r')
        end if
c
c        write (*,*) 'ifail,peak,pos,fwhm=',ifail,xsol
c
c Plot model and residual
c
        if (ifail.ne.1)
     +    call plotm (dobase, islice, n, nseg, ipslx, ipsly, ipsls, 
     +      ipsle, xsol, xdispl, ydispb, xlabel, ylabel, slbcol)
c
c Tell user result; inside redo loop because they may fit multiple
c peaks in the one slice.
c
        call output (' ')
        call output ('Model fit:')
        if (dobase) then
          call output ('   PEAK          POSITION         FWHM'//
     +                 '         OFFSET          SLOPE')
          write (aline, 50) xsol(1), xsol(2), xsol(3), 
     +                      xsol(4), xsol(5)
50        format (5(1pe13.6,2x))
          call output (aline)
        else
          call output ('   PEAK           POSITION           FWHM')
          write (aline, 70) xsol(1), xsol(2), xsol(3)
70        format (3(1pe13.6,3x))
          call output (aline)
        end if
        call output (' ')
c
c Redraw data if redo
c
        call redo (more)
        if (more) call slerdraw (ydispb, xdispl, xlabel, ylabel, islice,
     +                           n, nseg, ipslx, ipsly, ipsls, ipsle, 
     +                           .true., wy1s, wy2s, slbcol)
      end do
c
c Save model in text file
c
      if (lmod.ne.0) then
        write (aline, 100) islice, xsol(1), xsol(2), xsol(3), 
     +                     xsol(4), xsol(5)
100     format (i3, 3x, 5(1pe13.6,1x))
        call txtwrite (lmod, aline, len1(aline), iostat)
        if (iostat.ne.0) then
          write (aline, 200) islice
200       format ('Error writing model for slice # ', i3, 
     +            ' to text file')
          call bug ('w', aline)
        end if
      end if
c
      end
c
c
      subroutine gaufun (xsol, y, npar, npts)
c-----------------------------------------------------------------------
c     Compute the residual of the data and the Gaussian
c
c   Input in common
c     ipx     The pointer to the MEMR memory buffer for the x values
c             of the slice
c   Input
c     npar    The number of parameters we are solving for
c     npts    The number of points in the slice
c   Input/output
c     xsol    The current guess for the peak, pos'n and FWHM
c     y       The Y data points of the slice
c     
c-----------------------------------------------------------------------
      implicit none
      integer npar, npts
      real xsol(npar), y(npts)
cc
      include 'maxdim.h'
      include 'mem.h'
      double precision fac, dexpun
      real x
      integer i, ipx, ipy, iter
      save iter
      common /trans/ ipx, ipy
      data iter /0/
c-----------------------------------------------------------------------
      iter = iter + 1
c      write (*,*) 'iter, xsol=',iter,xsol
      fac = -4.0d0 * log(2.0d0)
c
      do i = 1, npts
        x = memr(ipx+i-1)        
        y(i) = memr(ipy+i-1) -
     +         (xsol(1) * dexpun(fac*dble((x-xsol(2))/xsol(3))**2))
      end do
c
      end
c
c
      subroutine gaufun2 (xsol, y, npar, npts)
c-----------------------------------------------------------------------
c     Compute the residual of the data and the Gaussian + baseline
c
c   Input in common
c     ipx     The pointer to the MEMR memory buffer for the x values
c             of the slice
c   Input
c     npar    The number of parameters we are solving for
c     npts    The number of points in the slice
c   Input/output
c     xsol    The current guess for the peak, pos'n and FWHM, offset 
c             and slope
c     y       The Y data points of the slice
c     
c-----------------------------------------------------------------------
      implicit none
      integer npar, npts
      real xsol(npar), y(npts)
cc
      include 'maxdim.h'
      include 'mem.h'
      double precision fac, dexpun
      real x
      integer i, ipx, ipy, iter
      save iter
      common /trans/ ipx, ipy
      data iter /0/
c-----------------------------------------------------------------------
      iter = iter + 1
c      write (*,*) 'iter, xsol=',iter,xsol
      fac = -4.0d0 * log(2.0d0)
c
      do i = 1, npts
        x = memr(ipx+i-1)        
        y(i) = memr(ipy+i-1) - 
     +         (xsol(1) * dexpun(fac*dble((x-xsol(2))/xsol(3))**2)) -
     +          xsol(4) - x*xsol(5)
      end do
c
      end
c
c
      subroutine getfwhm (wx1, wx2, wy1, wy2, xsol, xc, yc)
c-----------------------------------------------------------------------
c     Get estimate of Gaussian peak and position
c
c  Input
c    wx,y1,2    World coordinates of window
c  Input/output
c    xsol       Solution vector, peak, pos, fwhm, offset, slope
c    xc,yc      ocation where cursor was last seen
c
c-----------------------------------------------------------------------
      implicit none
      real xsol(5), wx1, wx2, wy1, wy2, xc, yc
cc 
      real x, y
      character cch*1
      logical more, in
c-----------------------------------------------------------------------
      more = .true.
      do while (more)
        call output ('Mark Gaussian HWHM        by clicking '//
     +               'any button or entering "A"')
c
        x = xc
        y = yc
        call pgcurs (x, y, cch)
        call lcase (cch)
c
        call inrng (x, wx1, wx2, in)
        if (in) call inrng (y, wy1, wy2, in)
        if (in) then
          if (x.eq.xsol(2)) then
            call output (' ')
            call output ('A HWHM of 0 is no good, try again')
            call output (' ')
          else
            xsol(3) = 2.0*abs(xsol(2)-x)
            call pgpt (1, x, y, 17)
            call pgpt (1, xsol(2)-x+xsol(2), y, 17)
            call pgupdt
            more = .false.
          end if
        else
          call output (' ')
          call output ('Cursor off image, try again')
          call output (' ')
        end if
        xc = x
        yc = y     
      end do
c
      end
c
c
      subroutine getlim (x1, wx1, wx2, wy1, wy2, x, xc, yc)
c-----------------------------------------------------------------------
c     Get an x limit with the cursor
c-----------------------------------------------------------------------
      implicit none
      real x1, wx1, wx2, wy1, wy2, x, xc, yc
cc
      real xx(2), yy(2), y
      logical more, in
      character cch*1
c-----------------------------------------------------------------------
      more = .true.
      do while (more)
        x = xc
        y = yc
        call pgcurs (x, y, cch)
        call lcase (cch)
c
        call inrng (x, wx1, wx2, in)
        if (in) call inrng (y, wy1, wy2, in)
        if (in) then
          if (x.eq.x1) then
            call output ('Degenerate x-range, try again')
          else
            xx(1) = x
            xx(2) = x
            yy(1) = wy1
            yy(2) = wy2
            call pgline (2, xx, yy)
            call pgupdt
            more = .false. 
          end if
        else
          call output (' ')
          call output ('Cursor off image, try again')
          call output (' ')
        end if
        xc = x
        yc = y
      end do
c
      end
c
c
      subroutine getpeak (wx1, wx2, wy1, wy2, xsol, xc, yc)
c-----------------------------------------------------------------------
c     Get estimate of Gaussian peak and position
c
c  Input
c    wx,y1,2    World coordinates of window
c  Input/output
c    xsol       Solution vector, peak, pos, fwhm, offset, slope
c    xc,yc      Location where cursor was last seen
c-----------------------------------------------------------------------
      implicit none
      real xsol(5), wx1, wx2, wy1, wy2, xc, yc
cc 
      real x, y
      character cch*1
      logical more, in
c-----------------------------------------------------------------------
      more = .true.
      do while (more)
        call output ('Mark Gaussian centre/peak by clicking '//
     +               'any button or entering "A"')
c
        x = xc
        y = yc
        call pgcurs (x, y, cch)
        call lcase (cch)
c
        call inrng (x, wx1, wx2, in)
        if (in) call inrng (y, wy1, wy2, in)
        if (in) then
          xsol(1) = y
          xsol(2) = x
          call pgpt (1, x, y, 17)
          call pgupdt
          more = .false.
        else
          call output (' ')
          call output ('Cursor off image, try again')
          call output (' ')
        end if
        xc = x
        yc = y
      end do        
c
      end
c
c
      subroutine getxrng (wx1, wx2, wy1, wy2, n, x, is, n2, xc, yc)
c-----------------------------------------------------------------------
c     Get x range to fit with cursor
c
c  Input
c    wx,y1,2    World coordinates of window
c    n          Number of points in slice
c    x          Array of slice x values
c  Output
c    is         Start index of first x value wanted in range 
c               defined with cursor
c    n2         Number of points in selected x range
c-----------------------------------------------------------------------
      implicit none
      integer is, n, n2
      real wx1, wx2, wy1, wy2, x(n), xc, yc
cc 
      real x1, x2, xt
      integer i, ie
c-----------------------------------------------------------------------
      call output 
     +   ('Mark x limits by clicking any button or entering "A"')
c
      call getlim (-1.0e32, wx1, wx2, wy1, wy2, x1, xc, yc)
      call getlim (x1, wx1, wx2, wy1, wy2, x2, xc, yc)
c      write (*,*) 'Enter x1,x2'
c      read (*,*) x1,x2
c
      xt = x2
      x2 = max(x1,x2)
      x1 = min(x1,xt)
c
      if (x(n).ge.x(1)) then
        do i = 1, n
          if (x(i).le.x1) then
            is = i
          else if (x(i).le.x2) then
            ie = i
          end if
        end do
      else
        do i = n, 1, -1
          if (x(i).le.x1) then
            ie = i
          else if (x(i).le.x2) then
            is = i
          end if
        end do
      end if
      n2 = ie - is + 1
c
      end
c
c
      subroutine inputs (nltype, ltype, maxlev, in, ibin, jbin, kbin,
     +   levtyp, slev, levs, nlevs, pixr, trfun, coltab, pdev, labtyp,
     +   do3val, do3pix, eqscale, nx, ny, cs, dopixel, doerase, accum, 
     +   noimage,dofit, dobase, fslval, fslposi, fslposo, fslmod, 
     +   xrange, yrange, doxrng, dofid, dowedge, grid)
c-----------------------------------------------------------------------
c     Get the unfortunate user's long list of inputs
c
c  Input:
c   nltype     Number of possible label types
c   ltype      List of possible label types
c   maxlev     Maximum number of allowed contour levels
c  Output:
c   i,j,kbin   Pixel increments and binning values
c   in         Image name.
c   plinc      Increment in planes (channels) to select
c   plav       Number of planes to average
c   levtyp     Type of contour levels scale factor
c              'p'(ercentage) or 'a'(bsolute)
c   slev       Contour levels scale factors (absolute or percentage)
c   levs       Contour levels.  Will be scaled by SLEV for contouring
c   nlevs      Number of contour levels
c   pixr       Pixel map intensity range
c   trfun      Type of grtey scale transfer function: 'log' or 'lin'
c   coltab     User given colour table to apply to plot device
c   pdev       PGPLOT plot device/type
c   labtyp     Type of labels for x and y axes
c   do3val     True means label sub-plots with value of third axis
c   do3pix     True means label sub-plots with pixel of third axis
c   eqscale    True means plot with x and y scales
c   nx,ny      Number of sub-plots per page
c   cs         PGPLOT character sizes for the plot axis labels,
c              velocity/channel label, and slice plot
c   dopixel    True for pixel map, false for contour plot
c   doerase    Erase rectangle behind "3-axis" value
c   noimage    Don't display the image
c   dofit      Fit Gaussians
c   dobase     Git Guassian + slope
c   dofid      FIddle lookup table
c   dowedge    Draw pixel map wedge
c   accum      Accumulate slice plots without clearing between sub-plots
c   doxrng     Use cursor to define x range when fitting Gaussian
c   fslval     File to put slice values into
c   fslposi    File to get slice positions from
c   fslposo    File to put slice positions into
c   fslmod     File to put the slice models into
c   x,yrange   SLice display extrema
c   grid       Overlay coordinate grid
c-----------------------------------------------------------------------
      implicit none
c
      integer maxlev, nx, ny, nlevs, ibin(2), jbin(2), kbin(2), nltype,
     +  coltab
      real levs(maxlev), pixr(2), cs(3), slev, xrange(2), yrange(2)
      character*(*) labtyp(2), in, pdev, trfun, levtyp, fslval, fslposi,
     +  fslposo, fslmod, ltype(nltype)
      logical do3val, do3pix, eqscale, dopixel, doerase, accum, noimage,
     +  dofit, dobase, doxrng, dofid, dowedge, grid
cc
      integer nlab, ntype2, nimtype, nval
      parameter (ntype2 = 3)
      character imtype*7, type2(ntype2)*7
      data type2 /'contour', 'pixel', 'grey'/
c-----------------------------------------------------------------------
      call keyini
      call keyf ('in', in, ' ')
      if (in.eq.' ') call bug ('f', 'No image specified')
      call keymatch ('type', ntype2, type2, 1, imtype, nimtype)
      if (nimtype.eq.0) imtype = 'contour'
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
      if (kbin(2).gt.kbin(1)) kbin(2) = kbin(1)
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
      call keyi ('range', coltab, 1)
      call lcase (trfun)
      if (dopixel .and. trfun.ne.'lin' .and. trfun.ne.'log' .and. 
     +    trfun.ne.'sqr' .and. trfun.ne.'heq') call bug ('f',
     +    'Unrecognized pixel map transfer function type')
c
      call mkeyr ('xrange', xrange, 2, nval)
      if (nval.ne.0 .and. nval.ne.2) call bug ('f',
     +  'You must give two values for keyword "xrange"')
      if (nval.eq.2 .and. xrange(1).eq.xrange(2)) call bug ('f',
     +  'Your specified slice display x-range is degenerate')
c
      call mkeyr ('yrange', yrange, 2, nval)
      if (nval.ne.0 .and. nval.ne.2) call bug ('f',
     +  'You must give two values for keyword "yrange"')
      if (nval.eq.2 .and. yrange(1).eq.yrange(2)) call bug ('f',
     +  'Your specified slice display y-range is degenerate')
c
      call keya ('device', pdev, ' ')
      if (pdev.eq.' ') then
        call pgldev
        call bug ('f', 'A PGPLOT device must be given')
      end if
c
      call decopt (do3val, do3pix, eqscale, doerase, accum, noimage,
     +             dofit, dobase, doxrng, dofid, dowedge, grid)
      if (dobase .and. .not.dofit) dofit = .true.
      if (accum .and. dofit) accum = .false.
      if (.not.dopixel .or. noimage) then
        dofid = .false.
        dowedge = .false.
      end if
c
      call keymatch ('labtyp', nltype, ltype, 2, labtyp, nlab)
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
      call keyr ('csize', cs(3), 0.7)
c
      call keya ('posin',  fslposi, ' ')
      call keya ('posout', fslposo, ' ')
      call keya ('valout', fslval, ' ')
      call keya ('modout', fslmod, ' ')
c
      if (fslposi.ne.' ' .and. fslposo.ne.' ') call bug ('f',
     +  'There is no point to giving POSIN and POSOUT')
      if (fslposi.eq.' ' .and. noimage) call bug ('f',
     +  'You must give keyword "posin" with options=noimage')
      if (fslmod.ne.' ' .and. .not.dofit) fslmod = ' '
c
      end
c
c
      subroutine inrng (x, x1, x2, in)
c-----------------------------------------------------------------------
c     See if a value is within a range
c-----------------------------------------------------------------------
      implicit none
      real x, x1,x2
      logical in
c-----------------------------------------------------------------------
      in = .false.
      if ( (x.ge.x1 .and. x.le.x2) .or.
     +     (x.ge.x2 .and. x.le.x1) ) in = .true.
c
      end
c
c
      subroutine intwt (x, w, i1)
c-----------------------------------------------------------------------
c     Generate cubic convolution interpolation weights. See Robert Keys,
c     "Cubic Convolution", IEEE Trans ASSP, Dec, 1981
c
c  Input:
c    x    Pixel value to intgerpolate to. Must be positive and can be
c         fractional of course.  Pixels are treated as having their centre 
c         at N.0  so that the left hand edge is M.5, the centre N.0 and 
c         the right hand edge N.5 where N = M + 1
c  Output:
c    i1   The integer pixel value to which the first output
c         weight applies.
c    w    The weights to apply to the nearest pixels.  The output
c         interpolated value is given by
c
c         out = w(1)*v(i1) + w(2)*v(i1+1) + w(3)*v(i1+2) + w(4)*v(i1+3)
c
c         where v(i) is the value of the ith pixel.
c
c  Notes
c         This routine makes no attempt to deal with edges.  If you give
c         it, say, x=1.3, then i1=0 is returned.  You must deal with the
c         edges yourself in the calling routine
c
c------------------------------------------------------------------------
      implicit none
      real x, w(4)
      integer i1
cc
      integer jx
      real f
c-----------------------------------------------------------------------
      jx = x
      f = x - jx
      w(1) = ((-0.5*f+1.0)*f-0.5)*f
      w(2) = (( 1.5*f-2.5)*f    )*f + 1.0
      w(3) = ((-1.5*f+2.0)*f+0.5)*f
      w(4) = (( 0.5*f-0.5)*f    )*f
c  
      i1 = jx - 1
c
      end
c
c
      subroutine intxy (nx, ny, im, nim, wx, wy, i1, j1, out, blanked)
c-----------------------------------------------------------------------
c     Do 2-D cubic convolution interpolation.   The user passes
c     in a 4x4 section of an image.  The pixel values of this subimage
c     combined linearly according to the x and y weights.   This
c     gives the interpolated pixel value.
c
c  Input:
c    nx,ny  Size of images
c    im     Input image
c    nim    Normalization image.  0 -> blanked
c    wx,wy  Weights returnd by subroutine INTWT
c    i1,j1  Pointers to BLC of section of image of interest
c  Out:
c    out     Interpolated value
c    blanked Output pixel blanked if true
c-----------------------------------------------------------------------
      implicit none
      integer nx, ny, nim(nx,ny), i1, j1
      real im(nx,ny), wx(4), wy(4), out
      logical blanked
cc
      real w(4,4), sw, im2(4,4)
      integer i, j, io, jo
      logical blanks
c-----------------------------------------------------------------------
      sw = 0.0
      blanks = .false.
      do j = j1, j1+4-1
        jo = j - j1 + 1
c
        do i = i1, i1+4-1
          io = i - i1 + 1
c
          if (i.lt.1 .or. j.lt.1 .or. 
     +        i.gt.nx .or. j.gt.ny) then
            im2(io,jo) = 0.0
            w(io,jo) = 0.0
            blanks = .true.
          else if (nim(i,j).eq.0) then
            w(io,jo) = 0.0
            im2(io,jo) = 0.0
            blanks = .true.
          else
            w(io,jo) = wx(io) * wy(jo)
            im2(io,jo) = im(i,j)
          end if
          sw = sw + w(io,jo)
        end do
      end do
c
      if (sw.lt.0.5) then
        blanked = .true.
        out = 0.0
      else
        blanked = .false.
        call intxy1 (im2, w, out)
        if (blanks) out = out / sw
      end if
c
      end
c
c      
      subroutine intxy1 (im, w, out)
c-----------------------------------------------------------------------
      implicit none
      real im(16), w(16), out
cc
      integer i
c-----------------------------------------------------------------------
      out = 0.0
      do i = 1, 16
        out = out + im(i)*w(i)
      end do
c
      end
c
c
      subroutine mrplot (nseg, n, x, y, segs, sege)
c-----------------------------------------------------------------------
c     Plot the segments of the model or residual that were computed
c     where the slice was unblanked
c
c  Input
c   nseg       Number of segments
c   n          Number of points in slice
c   x,y        SLice model or residual  x and y values
c   segs       Start indices for segments
c   sege       End indices for segments
c
c-----------------------------------------------------------------------
      implicit none
      integer n, nseg, segs(nseg), sege(nseg)
      real x(n), y(n)
cc
      integer j, nsegp, ip
c-----------------------------------------------------------------------
c
c Loop over number of segments
c
      do j = 1, nseg
c
c Find number of points in segment
c
        nsegp = sege(j) - segs(j) + 1
c
c Find pointer to first point of this segment
c
        ip = segs(j)
c
c Plot the segment
c
        call pgline (nsegp, x(ip), y(ip))
      end do
      call pgupdt
c
      end
c
c
      subroutine serase (vtrc)
c-----------------------------------------------------------------------
c     Erase the slice display region. An extra character height
c     to account for the labels on the top of teh slice window
c
c  Input:
c   vtrc     y-axis TRC  of viewport of slice display region
c
c-----------------------------------------------------------------------
      implicit none
      real vtrc
cc
      real xht, yht
c-----------------------------------------------------------------------
      call pgqcs (0, xht, yht)
      call pgsvp (0.0, 1.0, 0.0, vtrc+yht)
      call pgswin (0.0, 1.0, 0.0, 1.0)       
      call pgsci (0)
      call pgsfs (1)
      call pgrect (0.0, 1.0, 0.0, 1.0)
      call pgupdt
c
      end
c
c
      subroutine opento (units, radians, fslval, fslposo, fslmod, lval, 
     +                   lposo, lmod)
c-----------------------------------------------------------------------
c     Open output text files
c
c-----------------------------------------------------------------------
      implicit none
      character*(*) fslval, fslposo, fslmod, units
      integer lval, lposo, lmod
      logical radians
cc
      logical exist
      integer iostat, len1
      character aline*132
c-----------------------------------------------------------------------
      lval = 0
      if (fslval.ne.' ') then
        inquire (file=fslval, exist=exist)
        call txtopen (lval, fslval, 'append', iostat)
        if (iostat.ne.0) then
          aline = 'Error opening output text file'//fslval
          call bug ('f', aline)
        end if
c
c Write header
c
        if (.not.exist) then
          aline = 'NSL   NSEG   NPNT          ABCISSA         VALUE'
          call txtwrite (lval, aline, len1(aline), iostat)
          if (iostat.ne.0) call bug ('f' ,
     +      'Error writing header to slice values file')
c
          if (radians) then
            aline = '                           arcsec         '//units
          else
            aline = '                          rel pix         '//units
          end if
          call txtwrite (lval, aline, len1(aline), iostat)
          if (iostat.ne.0) call bug ('f' ,
     +      'Error writing header to slice values file')
        end if
      end if
c
      lposo = 0
      if (fslposo.ne.' ') then
        inquire (file=fslposo, exist=exist)
        call txtopen (lposo, fslposo, 'append', iostat)
        if (iostat.ne.0) then
          aline = 'Error opening output text file'//fslposo
          call bug ('f', aline)
        end if
c
c Write header
c
        if (.not.exist) then
          aline = 'NSL     BLCX           BLCY         TRCX'
     +       //'          TRCY       CHANNEL RANGE'
          call txtwrite (lposo, aline, len1(aline), iostat)
          if (iostat.ne.0) call bug ('f' ,
     +      'Error writing header to slice positions file')
c
          if (radians) then
            aline = '       arcsec         arcsec       arcsec'
     +       //'         arcsec'
          else
            aline = '      rel pix        rel pix      rel pix'
     +       //'        rel pix'
          end if
          call txtwrite (lposo, aline, len1(aline), iostat)
          if (iostat.ne.0) call bug ('f' ,
     +      'Error writing header to slice positions file')
        end if
      end if
c
      lmod = 0
      if (fslmod.ne.' ') then
        inquire (file=fslmod, exist=exist)
        call txtopen (lmod, fslmod, 'append', iostat)
        if (iostat.ne.0) then
          aline = 'Error opening output text file'//fslmod
          call bug ('f', aline)
        end if
c
c Write header
c
        if (.not.exist) then
          aline = 'NSL       PEAK          CENTRE         '//
     +            'FWHM         OFFSET       SLOPE'
          call txtwrite (lmod, aline, len1(aline), iostat)
          if (iostat.ne.0) call bug ('f' ,
     +      'Error writing header to slice models file')
c
          if (radians) then
            aline = '                        arcsec       arcsec'
          else
            aline = '                       rel pix      rel pix'
          end if
          call txtwrite (lmod, aline, len1(aline), iostat)
          if (iostat.ne.0) call bug ('f' ,
     +      'Error writing header to slice models file')
        end if
      end if
c
      end
c
c
      subroutine plotm (dobase, islice, n, nseg, ipslx, ipsly, ipsls, 
     +  ipsle, xsol, xdispl, ydispb, xlabel, ylabel, slbcol)
c-----------------------------------------------------------------------
c     Plot data, model and residual
c
c  Input
c   dobase     Fit baseline
c   islice     SLice number
c   n          Number of points in slice
c   nseg       Number of segments in slice
c   ipslx      Pointer to MEMR buffer for first point of slice abcissa
c   ipsly      Pointer to MEMR buffer for first point of slice ordinate
c   ipsls      Pointer to MEMR buffer which gives index of first point of 
c              first segment
c   ipsle      Pointer to MEMR buffer which gives index of last point of 
c              first segment
c   xsol       Solution vector: peak, pos, fwhm, offset, slope
c   x,ydispl,b Label displacements
c   x,ylabel   Labels
c   slbcol     Colour index for slice plot frame
c
c-----------------------------------------------------------------------
      implicit none
      real xdispl, ydispb, xsol(5)
      integer n, ipslx, ipsly, ipsls, ipsle, nseg, islice, slbcol
      character*(*) xlabel, ylabel
      logical dobase
cc
      include 'maxdim.h'
      include 'mem.h'
c
      double precision fac, dexpun
      real x, y, ymin, ymax, coord, xl1, xl, yl
      integer ipmy, ipdy, i, ics, ic1, ic2
c-----------------------------------------------------------------------
      call memalloc (ipmy, n, 'r')
      call memalloc (ipdy, n, 'r')
      fac = -4.0d0 * log(2.0d0)
      ymin = 1.0e32
      ymax = -1.0e32
c
      do i = 1, n
        x = memr(ipslx+i-1)        
        y = memr(ipsly+i-1)
c
c Compute model and residual
c
        if (dobase) then
          memr(ipmy+i-1) = xsol(1) * 
     +                     dexpun(fac*dble((x-xsol(2))/xsol(3))**2) 
     +                     + xsol(4) + xsol(5)*x
        else
          memr(ipmy+i-1) = xsol(1) * 
     +                     dexpun(fac*dble((x-xsol(2))/xsol(3))**2)
        end if
        memr(ipdy+i-1) = y - memr(ipmy+i-1)
c
        ymin = min(ymin,memr(ipmy+i-1),memr(ipdy+i-1),y)
        ymax = max(ymax,memr(ipmy+i-1),memr(ipdy+i-1),y)
      end do
c
c Erase slice display and redraw slice
c
      call slerdraw (ydispb, xdispl, xlabel, ylabel, islice, n, nseg,
     +  ipslx, ipsly, ipsls, ipsle, .true., ymin, ymax, slbcol)
c
c Write title
c
      call pglen (5, 'ABC', xl1, yl)
      coord = xl1
      call pgmtxt ('T', 0.2, coord, 0.0, 'Slice')
      call pglen (5, 'Slice', xl, yl)
      call pgupdt
      coord = coord + xl + xl1
c
c Twiddle about with colours
c
      call pgqci (ics)
      if (ics.eq.2) then
        ic1 = 7
        ic2 = 3
      else
        ic1 = 2
        if (ics.eq.7) then
          ic2 = 3   
        else
          ic2 = 7
        end if
      end if
c
c Plot model
c
      call pgsls (1)
      call pgsci (ic1)
      call mrplot (nseg, n, memr(ipslx), memr(ipmy), memi(ipsls),
     +             memi(ipsle))
c
      call pgmtxt ('T', 0.2, coord, 0.0, 'Model')
      call pglen (5, 'Model', xl, yl)
      call pgupdt
      coord = coord + xl + xl1
c
c Plot residual
c
      call pgsls (3)
      call pgsci (ic2)
      call mrplot (nseg, n, memr(ipslx), memr(ipdy), memi(ipsls),
     +             memi(ipsle))
c
      call pgmtxt ('T', 0.2, coord, 0.0, 'Residual')
      call pgupdt
c
      call pgsls (1)
      call pgsci (ics)
c
      call memfree (ipmy, n, 'r')
      call memfree (ipdy, n, 'r')

      end
c
c
      subroutine posdec (lun, krng, noimage, lpos, nx, ny, blc, trc,
     +  ibin, jbin, nltype, ltype, maxnsl, size, nslice, slpos)
c-----------------------------------------------------------------------
c     Read slice positions list file and decode
c
c   Inputs:
c     lun      Handle of image
c     krng     STart plane and number of planes averaged for this subplot
c     noimage  Image not displayed
c     lpos     Handle for positions list file
c     nx,ny    Size of subimage displayed
c     blc,trc  BLC and TRC of image displayed
c     i,jbin   Pixel increment sizes to step through image
c     nltype   Maximum number of label types
c     ltype    Possible label types
c     maxnsl   Maximum number of allowed slices
c     size     Size of image
c  Outputs
c     nslice   Number of slices
c     slpos    BLC (xyz) and  TRC (xyz) of slices in absolute subimage
c              binned pixels. If z=0 then extract slice from all z pixels
c
c------------------------------------------------------------------------
      implicit none
c
      integer lun, lpos, maxnsl, nltype, nslice, blc(3), trc(3), nx, ny,
     +  size(*), slpos(6,maxnsl), ibin, jbin, krng(2)
      character ltype(nltype)*(*)
      logical noimage
cc
      integer iostat, ilen, iline
      double precision pos(6), pix3
      character aline*100
      logical noton, want, slwant
c
      integer len1
      character itoaf*2
c------------------------------------------------------------------------
c
c Read and decode locations.  # means comment
c
      iline = 0
      nslice = 0
      iostat = 0
      pix3 = dble(2*krng(1)+krng(2)-1)/2.0
      call initco (lun)
c
      do while (iostat.ne.-1)
        aline = ' '
        call txtread (lpos, aline, ilen, iostat) 
        if (iostat.eq.0) then
          if (aline(1:1).ne.'#' .and. aline.ne.' ') then
c
c Fish out slice coordinates and convert to absolute subimage pixels
c
            iline = iline + 1
            if (nslice.eq.maxnsl) then
              call bug ('w', 'Reducing no. slices to max. '//
     +                       'allowed = '//itoaf(maxnsl))
              iostat = -1
            else
              nslice = nslice + 1
              ilen = len1(aline)
              call posdec2 (lun, pix3, nltype, ltype, nslice, 
     +                      aline(1:ilen), pos)
c
c Convert to binned subimage pixels in x and y
c
              call ppconcg (1, blc(1), ibin, pos(1))
              slpos(1,nslice) = nint(pos(1))
c
              call ppconcg (1, blc(2), jbin, pos(2))
              slpos(2,nslice) = nint(pos(2))
c
              call ppconcg (1, blc(1), ibin, pos(3))
              slpos(4,nslice) = nint(pos(3))
c
              call ppconcg (1, blc(2), jbin, pos(4))
              slpos(5,nslice) = nint(pos(4))
c
c z direction binned differently
c
              slpos(3,nslice) = nint(pos(5))
              slpos(6,nslice) = nint(pos(6))
c 
c See if this one is on this subplot channel range
c
              want = slwant (trc(3), krng(1), krng(2), slpos(3,nslice),
     +                       slpos(6,nslice))
c
c If so, see if it fits spatially
c
              if (want) then
                noton = .false.
                if (noimage) then
                  if (slpos(1,nslice).lt.1 .or.
     +                slpos(1,nslice).gt.size(1) .or.
     +                slpos(2,nslice).lt.1 .or.
     +                slpos(2,nslice).gt.size(2) .or.
     +                slpos(4,nslice).lt.1 .or.
     +                slpos(4,nslice).gt.size(1) .or.
     +                slpos(5,nslice).lt.1 .or.
     +                slpos(5,nslice).gt.size(2)) noton = .true.
                else
                  if (slpos(1,nslice).lt.1.or.slpos(1,nslice).gt.nx.or.
     +                slpos(2,nslice).lt.1.or.slpos(2,nslice).gt.ny.or.
     +                slpos(4,nslice).lt.1.or.slpos(4,nslice).gt.nx.or.
     +                slpos(5,nslice).lt.1.or.slpos(5,nslice).gt.ny)
     +            noton = .true.
                end if
c
                if (noton) then
                  aline = 'Slice # '//itoaf(iline)//
     +                    ' does not fit on the image'
                  call bug ('w', aline)
                  nslice = nslice - 1
                end if
              else
                nslice = nslice - 1
              end if
            end if
          end if
        else
          if (iostat.ne.-1) call bug ('f', 
     +       'Error reading from input slice positions file')
        end if
      end do
      if (nslice.eq.0) call bug ('f', 
     +  'The input slice positions file had no valid locations')
      call finco (lun)
c
      end
c
c
      subroutine posdec2 (lun, pix3, nltype, ltype, nslice, aline, pos)
c---------------------------------------------------------------------
c     Decode string into positions list
c
c     Input:
c       lun      Handle of image
c       pix3     ABsolute pixel fo third axis for this subplot
c       nltype   Maximum number of axis types
c       ltype    possible label types
c       nslice   Number of slice being decoded
c       aline    Input string
c     Output
c       pos      X1 Y1 X2 Y2  CS CE    where the locations are in
c		 absolute image pixels
c
c---------------------------------------------------------------------
      implicit none
c
      integer nslice, nltype, lun
      double precision pos(6), pix3
      character*(*) aline, ltype(nltype)
cc 
      integer maxnum
      parameter (maxnum = 20)
c
      double precision nums(maxnum), off(2)
      integer j, slen, lena, inum, ipres, nextra, npt, emax, nuse,
     +  icomm(maxnum), dsign(2), spos
      logical ok
      character str*4, estr*80, otype(2)*6
c
      integer len1
      character itoaf*4
c--------------------------------------------------------------------
c
c Prepare string for matodf
c
      str = itoaf(nslice)
      slen = len1(str)
      call strprpcg (maxnum, aline, icomm, ipres, lena)
      if (ipres.lt.4) then
        estr = 'There are insufficient fields for slice # '//
     +          str(1:slen)
        call bug ('f', estr)
      end if
c
c Fish out XOTYPE and YOTYPE
c
      otype(1) = aline(1:icomm(1)-1)
      call matchcg (nslice, 'XOTYPE', otype(1), 'slice', nltype, ltype)
      otype(2) = aline(icomm(1)+1:icomm(2)-1)
      call matchcg (nslice, 'YOTYPE', otype(2), 'slice', nltype, ltype)
c
      ipres = ipres - 2
c
c How many numbers do we expect in string.  Minimum is:
c     X1,Y1 X2,Y2   CS CE optional
c
      inum = 0
      do j = 1, 2
        if (otype(j).eq.'hms' .or. otype(j).eq.'dms') then
          inum = inum + 6
        else
          inum = inum + 2
        end if
      end do
c
      if (ipres.lt.inum) then
        estr = 'Insufficient fields for slice # '//str(1:slen)
        call bug ('f', estr)
      end if
c
c Find DEC sign.  Could be on either axis
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
c Now extract the numeric part of the line which remains
c
      call matodf (aline(icomm(2)+1:lena), nums, ipres, ok)
      if (.not.ok) then
        estr = 'Error decoding slice # '//str(1:slen)
        call bug ('f', estr)
      end if
c
c Now convert the BLC and TRC in whatever units to image pixels
c
c Now convert the overlay locations in whatever unit to pixels
c
      off(1) = 0.0d0
      off(2) = 0.0d0
      npt = 1
      call ol2pixcg (lun, pix3, ' ', otype, off, dsign, nums(npt),
     +               pos, nuse)
      npt = nuse + 1
      call ol2pixcg (lun, pix3, ' ', otype, off, dsign, nums(npt), 
     +               pos(3), nuse)
      npt = npt + nuse
c
c We have done the mandatory columns, now deal with the optional CS and CE
c
      nextra = ipres - inum
      emax = 2
      if (nextra.gt.emax) call bug ('f', 
     +   'Too many fields for slice # '//str(1:slen))
c
      if (nextra.eq.0) then
        pos(5) = 0.0
        pos(6) = 0.0
      else if (nextra.eq.1) then
        pos(5) = nums(npt)
        pos(6) = pos(5)
      else if (nextra.eq.2) then
        pos(5) = nums(npt)
        pos(6) = nums(npt+1)
      end if
c
      end
c
c
      subroutine redo (more)
c-----------------------------------------------------------------------
c     Ask user if wants to redo model or finish
c-----------------------------------------------------------------------
      character cch*1
      real x, y
      logical more
c-----------------------------------------------------------------------
      call output (' ')
      call output ('To continue, click the right button or enter "X"')
      call output ('To redo the model fit                  enter "R"')
      cch = ' '
      do while (cch.ne.'x' .and. cch.ne.'r')
        call pgcurs (x, y, cch)
c        write (*,*) 'Enter cch'
c        read (*,*) cch
        call lcase (cch)
      end do
      more = .false.
      if (cch.eq.'r') more = .true.
c
      end
c
c
      subroutine region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +                   win, maxgrp, grpbeg, ngrp, ngrps)
c----------------------------------------------------------------------
c     Finish key routine inputs for region of interest now.
c
c  Input:
c    in            Image file name
c    naxis         Number of dimensions of image
c    size          Dimensions of image
c    i,j,kbin      Pixel increments and binning sizes
c    maxgrp        Maximum nuber of groups of channels
c  Output:
c    grgbeg        List of start planes for each group of channels
c                  that are to be avearged together for each sub-plot
c                  A new group is begun at every interruption to the
c                  continuity of the selected channels, or if the
c                  channel increment is reached.
c    ngrp          Number of channels in each group of channel to
c                  be averaged together for each sub-plot.
c    ngrps         Number of groups of channels.
c    blc,trc       3-D Hyper-rectangle surrounding region of interest
c    win           Size of region of interest for each of upto 3 Ds
c
c----------------------------------------------------------------------
      implicit none
c
      integer naxis, size(naxis), blc(*), trc(*), win(*), maxgrp,
     +  ngrp(maxgrp), grpbeg(maxgrp), ngrps, ibin(2), jbin(2), kbin(2)
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
      if (win(1).le.1 .or. win(2).le.1) call bug ('f',
     +   'Cannot display just one spatial pixel')
c
c Find list of start channels and number of channels for each group
c of channels selected.
c
      call chnselcg (blc, trc, kbin,  maxbox, boxes, maxgrp,
     +               grpbeg, ngrp, ngrps)
c
      end
c
c
      subroutine slerdraw (ydispb, xdispl, xlabel, ylabel, islice,
     +   n, nseg, ipslx, ipsly, ipsls, ipsle, usenew, ymin, ymax, 
     +   slbcol)
c-----------------------------------------------------------------------
c     Erase old slice display, redraw box and redraw slice
c
c  Input:
c  Input
c   x,ydispl,b Label displacements
c   x,ylabel   Labels
c   islice     SLice number
c   n          Number of points in slice
c   nseg       Number of segments in slice
c   ipslx      Pointer to MEMR buffer for first point of slice abcissa
c   ipsly      Pointer to MEMR buffer for first point of slice ordinate
c   ipsls      Pointer to MEMR buffer which gives index of first point of 
c              first segment
c   ipsle      Pointer to MEMR buffer which gives index of last point of 
c              first segment
c   usenew     Use the given ymin and ymax else use what it was before
c              These will be stretched 5% as usual
c   ymin,ymax  Optionally used Y extrema
c   slbcol     COlour index for frame
c-----------------------------------------------------------------------
      implicit none
      real ydispb, xdispl, ymin, ymax
      integer islice, nseg, ipslx, ipsly, ipsls, ipsle, n, slbcol
      logical usenew
      character*(*) xlabel, ylabel
cc
      include 'maxdim.h'
      include 'mem.h'
      real wblc(2), wtrc(2), vblc(2), vtrc(2), xrange(2), yrange(2)
c-----------------------------------------------------------------------
c
c Get current plot window and viewport
c
      call pgqwin (wblc(1), wtrc(1), wblc(2), wtrc(2))
      call pgqvp (0, vblc(1), vtrc(1), vblc(2), vtrc(2))
c
c Erase old plots
c
      call serase (vtrc(2))
c
c Redraw box
c
      xrange(1) = wblc(1)
      xrange(2) = wtrc(1)
      if (usenew) then
        yrange(1) = 0.0
        yrange(2) = 0.0
      else
        yrange(1) = wblc(2)
        yrange(2) = wtrc(2)
      end if
c
c Redraw box and labels
c
      call drawbox (.true., slbcol, vblc, vtrc, xrange, yrange,
     +   wblc(1), wtrc(1), ymin, ymax, xlabel, ylabel,
     +   xdispl, ydispb)
c
c Plot slice
c
      call slplot (islice, n, nseg, memr(ipslx), memr(ipsly), 
     +             memi(ipsls), memi(ipsle))
c
      end
c
c
      subroutine slice (ibin, jbin, slpos, grid, nx, ny, im, nim, 
     +   radians, naxis, cdelt, slsize, slx, sly, ip, bound, nseg, seg)
c-----------------------------------------------------------------------
c     Generate the slice
c
c  Input:
c    i,jbin   Pixel increments to step throught image
c    slpos    Pixels of slice ends.  BLC and TRC
c    grid     Increment along slice in pixels
c    nx,ny    Size of image
c    im,nim   Image and normalization image (0 -> blanked)
c    radians  Axis increments both in radians
c    naxis    Number of axes in image
c    c*       Axis descriptors
c    slsize   Memory allocated for slice 
c Output:
c    slx      Slice abcissa values
c    sly      Slice ordinate values
c    ip       Number of points in slice from all segments
c    bound    Extrema of slice:  xmin,ymin,xmax,ymax
c    nseg     Number of segments containing unblanked points in slice
c    seg      The start and end points of each segment
c
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      integer nx, ny, naxis, ip, slsize, nseg, seg(2,maxdim),
     +  nim(nx,ny), slpos(6), ibin, jbin
      double precision cdelt(naxis)
      real grid, im(nx,ny), slx(slsize), sly(slsize), bound(4)
      logical radians
cc
      include 'mirconst.h'
      double precision rtoa
      parameter (rtoa = 3600.0d0 * 180.0d0 / dpi)
      
      double precision theta, delx, dely, x, y, deld, d, facx, facy,
     +  dx, dy
      real wx(4), wy(4), out
      integer i1, j1, i
      logical bl, oldbl
c-----------------------------------------------------------------------
c
c Find pixel increments in x, y between slice points
c
      delx = slpos(4)-slpos(1) 
      dely = slpos(5)-slpos(2)
      deld = sqrt(delx**2 + dely**2)
      theta = atan2(delx,dely)
c
      delx = sin(theta) * grid
      dely = cos(theta) * grid
c
c See if we have radian axes
c
      if (radians) then
        facx = cdelt(1) * rtoa * ibin
        facy = cdelt(2) * rtoa * jbin
      else
        facx = 1.0
        facy = 1.0
      end if
c
c Initialize
c
      do i = 1, maxdim
        seg(1,i) = 0
        seg(2,i) = 0
      end do
      nseg = 0
      oldbl = .true.
c
      bound(1) =  1.0e32
      bound(2) =  1.0e32
      bound(3) = -1.0e32
      bound(4) = -1.0e32
c
      x = slpos(1) 
      y = slpos(2) 
      dx = 0.0
      dy = 0.0
      d = 0.0
      ip = 0
c
      do while (d.lt.deld)
c
c Compute interpolation weights and interpolate
c
        call intwt (real(x), wx, i1)
        call intwt (real(y), wy, j1)
        call intxy (nx, ny, im, nim, wx, wy, i1, j1, out, bl)
c
        if (.not.bl) then
          if (ip.eq.slsize) call bug ('f', 
     +      'Logic error: insufficient memory allocated for slice')
          ip = ip + 1
c
          slx(ip) = sqrt(dx**2 + dy**2)
          sly(ip) = out
c
          bound(1) = min(bound(1), slx(ip))
          bound(2) = min(bound(2), sly(ip))
          bound(3) = max(bound(3), slx(ip))
          bound(4) = max(bound(4), sly(ip))
c
c Check if new segment
c
          if(.not.oldbl) then
            seg(2,nseg) = ip
          else if (oldbl) then
            if (nseg.eq.maxdim) call bug ('f',
     +        'Insufficient storage for unblanked slice segments')
c
            nseg = nseg + 1
            seg(1,nseg) = ip
          end if
        end if
c
c Increment interpolation locations in pixels and arcsec if possible
c
        x = x + delx
        y = y + dely
        d = d + grid
c
        dx = dx + delx*facx
        dy = dy + dely*facy
c
        oldbl = bl
      end do
c
c If the last point started a new segment, finish it
c
      if (seg(2,nseg).eq.0) seg(2,nseg) = ip - 1
c
      end
c
c
      subroutine slmark (blc, ibin, jbin, labtyp, slpos, naxis, 
     +                   crval, cdelt, crpix, ctype)
c-----------------------------------------------------------------------
c     Mark slice in input text file on image
c-----------------------------------------------------------------------
      implicit none
      integer slpos(6), naxis, blc(2), ibin, jbin
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
      character*(*) ctype(naxis), labtyp(2)
cc
      double precision world, pix
      real wblc(2), wtrc(2)
      logical ok
c-----------------------------------------------------------------------
c
c Convert to full image unbinned pixels and then to world coordinates
c
      pix = slpos(1)
      call ppconcg (2, blc(1), ibin, pix)
      call pix2wcg (.false., pix, 1, labtyp(1), naxis, crval, 
     +              crpix, cdelt, ctype, world, ok)
      wblc(1) = world
c
      pix = slpos(2)
      call ppconcg (2, blc(2), jbin, pix)
      call pix2wcg (.false., pix, 2, labtyp(2), naxis, crval, 
     +              crpix, cdelt, ctype, world, ok)
      wblc(2) = world
c
      pix = slpos(4)
      call ppconcg (2, blc(1), ibin, pix)
      call pix2wcg (.false., pix, 1, labtyp(1), naxis, crval, 
     +              crpix, cdelt, ctype, world, ok)
      wtrc(1) = world
c
      pix = slpos(5)
      call ppconcg (2, blc(2), jbin, pix)
      call pix2wcg (.false., pix, 2, labtyp(2), naxis, crval, 
     +              crpix, cdelt, ctype, world, ok)
      wtrc(2) = world
c
      call pgsch (0.7)
      call pgarro (wblc(1), wblc(2), wtrc(1), wtrc(2))
c
      end
c
c
      subroutine slplot (islice, n, nseg, x, y, segs, sege)
c-----------------------------------------------------------------------
c     Plot the segments of the slice that are unblanked
c
c  Input
c   islice     SLice number
c   n          Number of points in slice
c   nseg       Number of segments
c   x,y        Slice x and y values
c   segs       Segment start indices
c   sege       Segment end indices
c
c-----------------------------------------------------------------------
      implicit none
      integer n, nseg, segs(nseg), sege(nseg), islice
      real x(n), y(n)
cc
      integer j, nsegp, icol, ip
c-----------------------------------------------------------------------
c
c Set colour index for this slice.  All blanked slices do not appear, but
c their colour is lost because it was used to mark the slice on the image
c		
      call setcolcg (islice, icol)
      call pgsci (icol)
c
c Loop over number of segments
c
      do j = 1, nseg
c
c Find number of points in segment
c
        nsegp = sege(j) - segs(j) + 1
c
c Find pointer to first point of this segment
c
         ip = segs(j)
c
c Plot the segment
c
        call pgline (nsegp, x(ip), y(ip))
      end do
      call pgupdt
c
      end
c
c
      subroutine slposw (lin, lpos, krng, radians, blc, ibin, jbin, 
     +                   maxnsl, nslice, slpos)
c-----------------------------------------------------------------------
c     Save the slice locations in a text file.  Coordiantes
c     are converted to true world coordinates
c
c-----------------------------------------------------------------------
      implicit none
      integer lin, lpos, krng(2), blc(2), maxnsl, nslice, ibin, jbin,
     +  slpos(6,maxnsl)
      logical radians
cc
      integer i, ilen, iostat
      double precision win(3), wout(3), blcx, blcy, trcx, trcy
      character aline*130, typei(3)*6, typeo(3)*6
c
      integer len1
c-----------------------------------------------------------------------
      typei(1) = 'abspix'
      typei(2) = 'abspix'
      typei(3) = 'abspix'
      if (radians) then
        typeo(1) = 'arcsec'
        typeo(2) = 'arcsec'
      else
        typeo(1) = 'abspix'
        typeo(2) = 'abspix'
      end if
      typeo(3) = 'abspix'
      win(3) = dble(2*krng(1)+krng(2)-1)/2.0
      call initco (lin)
c
      do i = 1, nslice
c
c Convert absolute pixels to unbinned full image true arcsecond offsets
c
        win(1) = slpos(1,i)
        call ppconcg (2, blc(1), ibin, win(1))
        win(2) = slpos(2,i)
        call ppconcg (2, blc(2), jbin, win(2))
        call w2wco (lin, 3, typei, ' ', win, typeo, ' ', wout)
        blcx = wout(1)
        blcy = wout(2)
c
        win(1) = slpos(4,i)
        call ppconcg (2, blc(1), ibin, win(1))
        win(2) = slpos(5,i)
        call ppconcg (2, blc(2), jbin, win(2))
        call w2wco (lin, 3, typei, ' ', win, typeo, ' ', wout)
        trcx = wout(1)
        trcy = wout(2)
c
        write (aline,100) i, blcx, blcy, trcx, trcy, krng(1), 
     +                    krng(1)+krng(2)-1
100     format (i3, 1x, 4(1pe13.6, 1x), i4, 1x, i4)
        ilen = len1(aline)
        call txtwrite (lpos, aline, ilen, iostat)
        if (iostat.ne.0) call bug ('f', 
     +     'Error writing slice positions file')
      end do
      call finco (lin)
c
      end
c
c
      subroutine slput (simpos, wldpos, ipos, wpos)
      implicit none
      integer simpos(2), ipos(2)
      real wldpos(2), wpos(2)
c
      ipos(1) = simpos(1)
      ipos(2) = simpos(2)
      wpos(1) = wldpos(1)       
      wpos(2) = wldpos(2)
c
      end
c
c
      subroutine slput2 (slpos, ipos)
      implicit none
      integer slpos(6), ipos(2,2)
c
      slpos(1) = ipos(1,1)
      slpos(2) = ipos(2,1)
      slpos(3) = 0
      slpos(4) = ipos(1,2)
      slpos(5) = ipos(2,2)
      slpos(6) = 0
c
      end
c
c
      subroutine slsave (lval, islice, nseg, x, y, segs, sege)
c-----------------------------------------------------------------------
c     Save the segments of the slice that are unblanked.  COnvert here
c     from linear offsets to true offsets
c
c  Input
c   lval       Handle for text file
c   islice     Slice number
c   nseg       Number of segments
c   x,y        Slice
c   segs,e     Segment start and end indices
c
c-----------------------------------------------------------------------
      implicit none
      real x(*), y(*)
      integer nseg, segs(nseg), sege(nseg), islice, lval
cc
      integer j, nsegp, ip
c-----------------------------------------------------------------------
c
c Loop over number of segments
c
      do j = 1, nseg
c
c Find number of points in segment
c
        nsegp = sege(j) - segs(j) + 1
c
c Find pointer to first point of this segment
c
        ip = segs(j)
c
c Write the segments into a text file
c
        call slvalw (lval, islice, j, nsegp, x(ip), y(ip))
      end do
c
      end
c
c
      subroutine slsiz (slpos, grid, slsize)
      implicit none
      integer slpos(6), slsize
      real grid

cc
      real delx, dely
c
      delx = slpos(4) - slpos(1)
      dely = slpos(5) - slpos(2)
c
      slsize = sqrt(delx**2 + dely**2) / grid
c
c Add a few to make sure
c
      slsize = slsize + 5
c
      end
c
c
      subroutine slvalw (lval, islice, iseg, npnts, x, y)
c-----------------------------------------------------------------------
c     Save the slice values into a text file
c-----------------------------------------------------------------------
      implicit none
      integer lval, islice, iseg, npnts
      real x(npnts), y(npnts)
cc
      integer i, ilen, len1, iostat
      character aline*130
c-----------------------------------------------------------------------
      do i = 1, npnts
        write (aline,100) islice, iseg, i, x(i), y(i)
100     format (i3, 3x, i4, 3x, i4, 6x, 1pe13.6, 3x, 1pe13.6)
c
        ilen = len1(aline)
        call txtwrite (lval, aline, ilen, iostat)
        if (iostat.ne.0) call bug ('f', 
     +     'Error writing slice values file')
      end do
c
      end
c
c
      logical function slwant (trc, zs, nz, z1, z2)
c-----------------------------------------------------------------------
c   Input:
c     trc    z-TRC of subimage
c     zs     Start z-pixel of displayed image
c     nz     Number of z pixels in diaplayed image
c     z1,z2  range of z-pixels on which this slice should be displayed
c-----------------------------------------------------------------------
      implicit none
      integer trc, zs, nz, z1, z2
cc
      integer i, j, ze
c-----------------------------------------------------------------------
      slwant = .false.
      if (z1.eq.0) then
        slwant = .true.
      else
        ze = min(trc,zs+nz-1)
        do i = zs, ze
          do j = z1, z2
            if (i.eq.j) then
              slwant = .true.
              return
            end if
          end do
        end do
      end if
c
      end
c
c
      subroutine vpsiz (noimage, dofid, nx, ny, pcs, xdispl, ydispb, 
     +  xdispls, ydispbs, wedcod, wedisp, wedwid, tfdisp, vblc, vtrc, 
     +  vxsize, vysize, tfvp, wdgvp)
c---------------------------------------------------------------------------
c     Work out viewports for image and slice display for unequal scales 
c     in x and y here. If user wants equal scales, adjust later.
c
c   Input
c     noimage     Image not displayed
c     nx,ny       Number of sub-plots in x and y
c     pcs         PGPLOT character sizes image labels, velocity labels
c                 and slice labels
c     xdispl      Displacement of y-axis label from axis in char hghts
c                 for image display
c     ydispb      Displacement of x-axis label from axis in char hghts
c                 for image display
c     xdispls     Displacement of y-axis label from axis in char hghts
c                 for slice display
c     ydispbs     Displacement of x-axis label from axis in char hghts
c                 for slice display
c     dofid       True for interactive fiddle
c     wedcod      1 -> one wedge to right of all subplots
c                 2 -> one wedge to right per subplot
c                 3 -> one wedge per subplot inside subplot
c     wedwid      Width of wedge as a fraction of the full x viewport
c	          for wedcod = 1
c     wedisp      Displacement of wedge from right axis in char heights
c     tfdisp      Displacement of transfer function plot from right axis 
c                 in char heights
c   Output
c     vblc        BLC of viewports; 1 is slice, 2 is image in ndc
c     vtrc        TRC of viewports; 1 is slice, 2 is image in ndc
c                 The image view port encompasses all subplots
c     vx,ysize    Size of viewport of each sub-plot in n.d.c.s in x & y
c     tfvp        Viewport coords in which to draw interactive fiddle plot
c     wdgvp       Viewport for wedge if wedcod = 1.  Other wedge type 
c                 viewports are worked out when the wedge is drawn in 
c---------------------------------------------------------------------------
      implicit none
c
      real vblc(2,2), vtrc(2,2), vxsize, vysize, pcs(3), ydispb, xdispl,
     +  tfvp(4), wdgvp(4), wedisp, wedwid, tfdisp, xdispls, ydispbs
      integer nx, ny, wedcod
      logical noimage, dofid
cc
c Fraction of viewsurface to use for interactive fiddle plot
c and its y displacement in chaeacter heights below image
c
      real tfvps, tfyd
      parameter (tfvps = 0.1, tfyd = 0.75)
c
c Fraction of viewport to use for slice display
c
      real yfrac
      parameter (yfrac = 0.3)
c
      real xhti, yhti, xhts, yhts, dvx, dvy, dvww, dvwd, dvtd, asp, 
     +  dvtfx, dvtfy, dvwl, dvltot, dvwtot, dvttot
      integer i
      logical dowedge
c---------------------------------------------------------------------------
c
c Work out character heights in ndc
c
c
      call pgsch (pcs(1))
      call pgqcs (0, xhti, yhti)
c
      call pgsch (pcs(3))
      call pgqcs (0, xhts, yhts)
c
c Define the slice and image display viewports
c
      if (noimage) then
c
c Use a big chunk of the viewsurface for the slice if not 
c displaying the image
c
        vblc(1,1) = (xdispls + 1.2) * xhts
        vblc(2,1) = 0.25
        vtrc(1,1) = 1.0 - xhts
        vtrc(2,1) = 0.75
c
c These should not be accessed for the no image case
c
        vblc(1,2) = 0.0
        vtrc(1,2) = 0.0
        vblc(2,2) = 0.0
        vtrc(2,2) = 0.0
c
        vxsize = 0.0
        vysize = 0.0
c
        do i = 1, 4
          tfvp(i) = 0.0
          wdgvp(i) = 0.0
        end do
      else
c
c Set BLC of slice viewport
c
        vblc(1,1) = (xdispls + 1.2) * xhts
        vblc(2,1) = (ydispbs + 0.5) * yhts
c
c Set x-TRC of slice viewport
c
        vtrc(1,1) = 1.0 - xhts
c
c Set x-BLC of image viewport
c
        vblc(1,2) = (xdispl + 1.2) * xhts
c
c Set y-TRC of image viewport
c
        vtrc(2,2) = 1.0 - 0.5*yhti
c
c Width of wedge and wedge label area in ndc. x-displacement in ndc 
c from right hand edge of image plots in ndc
c 
        dowedge = wedcod.eq.1 .or. wedcod.eq.2
        if (dowedge) then        
          dvww = wedwid
          dvwl = 2.0 * xhti
          dvwd = wedisp * xhti
c
          dvwtot = dvwd + dvww + dvwl 
        else
          dvwtot = 0.0
          do i = 1, 4
            wdgvp(i) = 0.0
          end do
        end if
c
c x-displacement and width of transfer function plot
c
      if (dofid) then
          dvtd = tfdisp * xhti
c
c We want the transfer function fiddle plot to be square on the 
c screen so find the width and height in ndc accordingly
c
          asp = yhti / xhti
          if (asp.ge.1.0) then
            dvtfx = tfvps / asp
            dvtfy = tfvps
          else
            dvtfx = tfvps 
            dvtfy = tfvps * asp
          end if
c
          dvttot = dvtd + dvtfx
        else
          dvttot = 0.0
          do i = 1, 4
            tfvp(i) = 0.0
          end do
        end if
c
c Work out space to leave to right of image plots for wedge
c and/or transfer function plots.
c
        dvx = max(dvwtot,dvttot)
c
c Set x-TRC of image viewport
c
        vtrc(1,2) = vtrc(1,1) - dvx
c
c Now deal with y displacement of transfer function fiddle plot.
c
        dvltot = ydispb*yhti
c
c If we are only drawing one image subplot per page, the slice 
c display region will always overwrite the fiddle plot region 
c and we don't need to wastes Y space on the fiddle plot
c
        if (nx*ny.gt.1) then
          if (dofid) then
            dvttot = dvtfy + tfyd*yhti
          else
            dvttot = 0.0
          end if
c
c Gap between slice plot and image plot boxes.  Allow 0.75 character
c heights between slice plot and bottom of transfer function plot 
c or image label to make visual space and to accound for the fact
c that SERASE, which erases the slice plot, lops off an extra 1/2
c character height at the top for protruding labels
c
          dvy = max(dvttot,dvltot) + 0.75*yhti
c
c Distribute the gap to slice viewport y TRC and image
c image viewport y BLC.  
c
          vtrc(2,1) = yfrac - 0.25*dvy
          vblc(2,2) = yfrac + 0.75*dvy
        else
c
c Just leave space for image x axis label.  There will
c be enough room for the tranfer function plot in the
c region later occupied by the slice display
c
          vtrc(2,1) = yfrac - 0.5*yhti
          vblc(2,2) = vtrc(2,1) + dvltot + yhti
        end if
c
c Set viewport for transfer function plot
c
        if (dofid) then
          tfvp(1) = vtrc(1,2) + dvtd
          tfvp(2) = vblc(2,2) - dvtfy - tfyd*yhti
          tfvp(3) = tfvp(1) + dvtfx
          tfvp(4) = tfvp(2) + dvtfy
        end if
c
c Set wedge viewport
c
        if (dowedge) then
          wdgvp(1) = vtrc(1,2) + dvwd
          wdgvp(2) = vblc(2,2)
          wdgvp(3) = wdgvp(1) + dvww
          wdgvp(4) = vtrc(2,2)
        end if
c
c Work out size of sub-plots
c
        vxsize = (vtrc(1,2) - vblc(1,2)) / nx
        vysize = (vtrc(2,2) - vblc(2,2)) / ny
      end if
c
      end
c
c
      subroutine setlgc (bgcol, labcol, concol, slbcol)
c-----------------------------------------------------------------------
c     Set line graphics colours
c
c  Input
c    bgcol  background colour. 0 -> black
c			       1 -> white
c			      -1 -> something else
c  OUtput
c    colour indices to use
c-----------------------------------------------------------------------
      implicit none
      integer labcol, concol, slbcol, bgcol
c-----------------------------------------------------------------------
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
c Now contours
c
      concol = 7
      if (bgcol.eq.1) concol = 2
c
c Slice display frame
c
      slbcol = 7
c
      end
