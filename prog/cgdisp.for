      program cgdisp
c-----------------------------------------------------------------------
c
c= CGDISP - displays and overlays images on a PGPLOT device
c& nebk
c: plotting
c+
c	CGDISP displays/overlays images via contour plots, pixel map
c	representations, vectors and scaled boxes on a PGPLOT device. 
c	Upto 3 contour plots, one pixel map, one vector plot and one box 
c	display may be overlaid in multi-panel plots of multi-channel 
c	images.  In addition overlay locations (plotted as boxes, stars,
c	lines or see-through) may be specified from an ascii text file.
c
c	Manipulation of the device colour lookup table is available
c	when you display with a pixel map representation (formerly
c	called a "grey scale")
c
c@ in
c	You may input up to 7 images.  Upto 3 of these can be displayed 
c	via contour plots and 1 can be displayed via a colour pixel map 
c	representation.  1 vector amplitude image and 1 vector position
c	angle image (degrees; positive N -> E) can together be used to
c	display a vector map (e.g. polarization vectors).  1 image can
c	be displayed as small scaled boxes (see below) and 1 image may be
c	used as a mask.  
c
c	The "box" image is displayed by drawing little boxes (solid and
c	hollow for positive and negative pixels) at the location of each
c	selected pixel.  The size of the box scales with the value of the
c	pixel.  This is a useful way to display rotation measure images 
c	for example. The mask image blanking mask is logically ORed to all
c	the other image masks before they are displayed. The mask image 
c	is not displayed.
c
c	If more than one image is specified, they must have identical 
c	first and second dimensions.  However, you can overlay combinations
c	of 2-D with 3-D images (e.g. multi-channel images with a continuum 
c	image) provided all the 3-D images have the same third dimension. 
c	These images can be input in any order (see TYPE).
c	Wild card expansion is supported.    No default.
c@ type
c	Specifies the type of each image listed in the IN keyword.
c	Minimum match is supported (note that "pixel" was formerly "grey"
c	which is still supported).   Choose from:
c
c	"contour"   (contour;            up to 3 of these)
c	"pixel"     (pixel map;          up to 1 of these)
c	"amplitude" (vector amplitude;   up to 1 of these)
c	"angle"     (vector pos'n angle; up to 1 of these)
c	"box"       (box;                up to 1 of these)
c	"mask"      (mask;               up to 1 of these)
c
c	You can't give one of "amplitude" or "angle" without the other.
c	Default is "pixel" for one image, "contour" if more than one.
c@ region
c	Region of interest.  Choose only one spatial region (bounding box
c	only supported), but as many spectral regions (i.e., multiple 
c	IMAGE specifications) as you like.   Each channel (or group of 
c	channels; see CHAN below) is drawn on a new sub-plot.  
c	NOTE: the region specification applies equally to all the 
c	input images.
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
c	2 values. The first is the channel increment to step through the
c	image in, the second is the number of channels to average, for 
c	each sub-plot.  Thus CHAN=5,3  would average groups of 3 channels 
c	together, starting 5 channels apart such as: 1:3, 6:8, 11:13 ...  
c 	The channels available are those designated by the REGION keyword.
c	A new group of channels (sub-plot) is started if there is a 
c	discontinuity in the REGION selected channels (such as 
c	IMAGE(10,20),IMAGE(22,30).  The combination of REGION and CHAN 
c	determines how many sub-plots there will be.
c
c	In the case that you have input some combination of 2-D and 3-D
c	images, CHAN refers to the 3-D image(s). Note that a channel
c	is defined to be a pixel on the third axis of a cube, regardless
c	of the cube's order (xyv or vxy say).
c	Defaults are 1,1
c@ slev
c	Up to 3 pairs of values, one for contour image. First value is 
c	the type of contour level scale factor.  "p" for percentage and 
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
c@ range
c	Upto 100 groups of 4 values (1 group per subplot). These are 
c	the image intensity range to display (min to max), the transfer 
c	function type and the colour lookup table for each subplot 
c	displayed.  The transfer function type can be one of "lin" 
c	(linear), "sqr" (square root), "log" (logarithmic), and "heq" 
c	(histogram equalization).  The colour lookup table is an integer 
c	from 1 to 8 specifying a lookup table. Valud values are 1 (b&w),
c	2 (rainbow), 3 (linear pseudo colour), 4 (floating zero colour 
c	contours), 5 (fixed zero colour contours), 6 (rgb), 7 (background)
c	and 8 (heat).  If you enter a negative integer, then the reversed 
c	lookup table is displayed.  
c
c	The transfer function changes available with OPTIONS=FIDDLE 
c	are in addition (on top of) to the selections here, but the 
c	colour lookup table selections will replace those selected here.
c
c	All subplots following the last one with a specified "range"
c	will use the "range" settings from the previous subplot. In
c	this way, one group of settings can be applied to all the 
c	subplots if desired.  The multiple subplot capability is useful
c	if you have used IMCAT to put unlike images into planes of
c	a cube and you wish to display them together.
c
c	Default is linear between the image minimum and maximum with
c	a b&w lookup table.   You can default the intensity range with
c	zeros, viz. "range=0,0,log,-2" say.
c@ vecfac
c	3 values.  A scale factor to multiply the vector image lengths
c	(or box image widths) by, and the x and y increments (in pixels)
c	across the image  at which to plot the vectors (or boxes).  If 
c	you have set non unit values of XYBIN, the increments here refer 
c	to the binned pixels.  When VECFAC(1)=1, the vectors (boxes) are 
c	scaled so that the maximum amplitude (width) takes 1/20 of the 
c	(sub)plot size.  
c	Defaults are 1.0, 2, VECFAC(2)
c@ boxfac
c	3 values.  A scale factor to multiply the box image widths by, 
c	and the x and y increments (in pixels) across the image at which
c	to plot the boxes).  If have set non unit values of XYBIN, the 
c	increments here refer to the binned pixels.  When BOXFAC(1)=1, 
c	the boxes are scaled so that there is a little bit of space
c	between adjacent boxes.
c	Defaults are 1.0, 2, BOXFAC(2)
c@ device
c	The PGPLOT plot device, such as plot.plt/ps 
c	No default.
c@ nxy
c	Number of sub-plots in the x and y directions on the page. 
c	Defaults choose something depending on your telescope.
c@ labtyp
c	Up to 2 values.  The spatial label type of the x and y axes.
c	Minimum match is active.  Select from:
c
c	"hms"       the label is in H M S.S (e.g. for RA)
c	"dms"       the label is in D M S.S (e.g. for DEC)
c	"arcsec"    the label is in arcsecond offsets
c	"absdeg"    the label is in degrees
c	"reldeg"    the label is in degree offsets
c		    The above assume the pixel increment is in radians.
c	"abspix"    the label is in pixels
c	"relpix"    the label is in pixel offsets
c	"abskms"    the label is in Km/s
c	"relkms"    the label is in Km/s offsets
c	"absghz"    the label is in GHz
c	"relghz"    the label is in GHz offsets
c	"abslin"    the label is in linear coordinates as defined by 
c	            the header. You might call this the natural axis label
c	"rellin"    the label is in offset linear coordinates
c	"none"      no label and no numbers or ticks on the axis
c
c	All offsets are from the reference pixel.  
c	Defaults are "relpix", LABTYP(1)   except if LABTYP(1)="hms" when
c	LABTYP(2) defaults to "dms"  (to give RA and DEC)
c@ options
c	Task enrichment options. Minimum match of all keywords is active.
c
c	"relax" means issue warnings when image axis descriptors are
c	  inconsistent (e.g. different pixel increments) instead
c	  of a fatal error.  Use at your peril.
c
c	"full" means do full plot annotation with contour levels, pixel
c	  displa range, file names, reference values, etc.  Otherwise 
c	  more room for the plot is available. 
c	"noepoch" means don't write the epoch value into the axis labels
c
c	"fiddle" means enter a routine to allow you to interactively change
c	  the display lookup table.  You can cycle through a variety of
c	  colour lookup tables, as well as alter a linear transfer function
c	  by the cursor location, or by selecting predefined transfer 
c	  functions (linear, square root, logarithmic, histogram equalization)
c	  
c	  For hard copy devices (e.g. postscript), a keyboard driven
c	  fiddle is offered; you can cycle through different colour tables
c	  and invoke the predefined transfer functions, but the linear
c	  fiddler is not available.   Note that if you are using "cgdisp"
c	  from a script, so that interactive fiddling is not appropriate,
c	  you can use the "range" keyword to specify the transfer
c	  function and colour lookup tables.
c	"single" means that when you have selected OPTIONS=FIDDLE and you
c	  you have more than one subplot per page, activate the fiddle
c	  option after each subplot rather than the default, which is
c	  to fiddle only at the end.  In the latter case, the histogram
c	  equalization, if invoked, will have been computed with the 
c	  image in the last subplot only.
c	"wedge" means that if you are drawing a pixel map, also draw
c	  and label a wedge to the right of the plot, showing the map 
c	  of intensity to colour.
c
c	"3value" means label each sub-plot with the appropriate 
c	  value of the third axis (e.g. velocity or frequency for an
c	  xyv ordered cube, position for a vxy ordered cube).
c	"3pixel" means label each sub-plot with the pixel value of
c	  the third axis.
c	  Both "3pixel" and "3value" can appear, and both will be 
c	  written on the plot.  They are the average values when
c	  the third axis is binned up with CHAN.  If the third axis
c	  is not velocity or frequency, the units type for "3VALUE" 
c	  will be chosen to be the complement of any like axis in the 
c	  first 2. E.g., the cube is in vxy order and LABTYP=ABSKMS,ARCSEC 
c	  the units for the "3VALUE" label will be arcsec.  If 
c	  LABTYP=ABSKMS,HMS the "3VALUE" label will be DMS (if the 
c	  third [y] axis is declination).
c	"noerase" means don't erase a rectangle into which the "3-axis"
c	  values and the overlay ID strings are written.
c	  
c	  
c	"beamAB", where "A" is one of "b" or "t" and 
c	                "B" is one of "l" or "r"
c	  means draw the beam FWHM on the plot in the corner indicated
c	  by the "AB" location.  The beams for all images displayed
c	  (contour and pixel map) will be drawn confocally.
c
c	"solneg1" means make negative contours solid and positive 
c	  contours dashed for the first contour image. The default, 
c	  and usual convention is the reverse.
c	"solneg2" SOLNEG1 for the second contour image.
c	"solneg3" SOLNEG1 for the third contour image.
c	"mirror" causes all specified contour levels for all images
c	  to be multiplied by -1 and added to the list of contours
c
c	"rot90" rotates vectors by an extra 90 degrees.  Useful
c	  to convert E-vectors into B-vectors
c	"signs"  Normally, when plotting vectors, CGDISP assumes that
c	  North is up and East to the left.  If OPTIONS=SIGNS, then
c	  it assumes that E and N are in the direction of increasing
c	  X and Y.
c
c	"unequal" means draw plots with unequal scales in x and y
c	  so that the plot surface is maximally filled.  The default
c	  is for equal scales in x and y.
c	"gaps" means leave gaps between sub-plots and label each 
c	  sub-plot, otherwise they will abut each other.
c@ lines
c 	Up to 6 values.  The line widths for the axes, each contour 
c       image (in the order of TYPE), the vector image, and any overlays.
c	If there are less than 3 contour images or no vector
c	image, the vector image/overlay line widths shift left.
c	Line widths must be integers.
c	Defaults are 1,1,1,1,1,1
c@ break
c	Up to 3 values. The intensity levels for the break between
c	solid and dashed contours for each contour image. 
c	Defaults are 0.0,0.0,0.0
c@ csize
c	Up to 3 values.  Character sizes in units of the PGPLOT default
c	(which is ~ 1/40 of the view surface height) for the plot axis
c	labels, the velocity/channel label, and the overlay ID string
c	(if option "write" in OLAY used) label.
c	Defaults choose something sensible.  Use 0.0 to default the 
c	first or second, but not the second or third, e.g., 0.0, 1.5
c@ scale
c	Up to 2 values.  Scales in linear axis units/mm with which to plot
c	in the 	x and y directions.  For example, if the increments 
c	per pixel are in radians, then this number would be radians/mm
c	(note that for RA axes you give radians on the sky per mm).
c	Although this choice of unit may be cumbersome, it makes no 
c	assumptions about the axis type, so is more flexible.   If you 
c	also chose OPTIONS=EQUAL then one of your scales, if you set 
c	both and differently, would be over-ruled.  If you give only 
c	one value, the second defaults to that.  
c	Defaults choose scales to fill the page optimally. To default 
c	the first but the second, use 0.0,scale(2)
c@ olay
c	The name of a file containing a list of overlay descriptions.
c	Wild card expansion is active and the default is no overlays.
c	
c	Miriad task CGCURS OPTIONS=CURSOR,LOG,CGDISP  can be used to
c	make an overlay file.
c
c	Entries in the overlay file can be white space or comma
c	delimitered or both.  All lines beginning with # are ignored.
c
c	                **** DO NOT USE TABS **** 
c
c	Double quotes " are used below to indicate a string.  The "
c	should not be put in the file.   For all the string parameters
c	discussed below, you can abbreviate them with minimum match.
c
c
c	Each line describes an overlay and should be as follows:
c
c	 ##### The first 5 columns in each line must be
c
c	  1      2       3     4    5        Column
c	--------------------------------
c	OFIG  XOTYPE  YOTYPE  ID  WRITE      where
c
c	OFIG is the type of overlay; choose from
c	 "star"    for stars (crosses; give centre and half-sizes)
c	 "circle"  for a filled in circle (give centre)
c	 "ocircle" for an open circle (give centre)
c	 "box"     for boxes (give centre and half-sizes)
c	 "line"    for line segments (give blc and trc)
c	 "clear"   for a see-through overlay -- thus you can write the
c	 	   overlay ID string (see below) without the overlay
c
c	XOTYPE and YOTYPE  give the units of the overlay location (and 
c	overlay half-sizes) contained in the file for the x- and y-
c	directions, respectively.  Choose from:
c	 "hms", "dms", "arcsec", "absdeg", "reldeg", "abspix", 
c	 "relpix", "abslin", "rellin", "absghz", "relghz", 
c	 "abskms", & "relkms"  as described in the keyword LABTYP.  
c	Note that OTYPE does not depend upon what you specified for LABTYP.
c
c	ID is an identifying overlay string which can be optionally
c	written on the overlay; it MUST be in the overlay file whether
c	you write it on the plot or not).  The ID string is written in the
c	corner for "star" and "box", in the centre for "clear" and
c	at the end for "line"
c
c	WRITE is "yes" or "no" to specify if the overlay ID is to be 
c	written in the corner of overlay figure or not.
c
c
c	 ##### Columns beyond number 5 depend upon OFIG, XOTYPE, and YOTYPE
c
c	6   7    8   9  10  11    Logical column
c	-----------------------
c	X   Y   XS  YS  CS  CE    for OFIG="box" and "star"
c	X1  Y1  X2  Y2  CS  CE    for OFIG="line"
c	X   Y   CS  CE            for OFIG="clear"
c	X   Y   S   CS  CE        for "circle" and "ocircle"
c
c	X,Y defines the center of the overlay in the nominated OTYPE
c	coordinate system (X- and Y-OTYPE can be different).  
c	(X1,Y1) & (X2,Y2) are the end points of the line segment in the
c	nominated OTYPE (mixed OTYPEs are supported here too).
c	For %OTYPE = "abspix ", "relpix", "arcsec", "absdeg", "reldeg",
c		     "absghz", "relghz", "abskms", "relkms", "abslin"
c		     and "rellin" X,Y,X1,Y1,X2,Y2 are single numbers.
c
c	For %OTYPE = "hms" or "dms", the X and/or Y location is/are replaced
c	by three numbers such as  HH MM SS.S or DD MM SS.S.  Thus if
c	XOTYPE=hms & YOTYPE=dms then the file should have lines like
c
c	  HH MM SS.S   DD MM SS.S   XS   YS  CHAN    for OFIG="box"
c
c
c	XS, YS are the overlay half-sizes in the following units.
c	%OTYPE = "abspix" and "relpix" in pixels
c		 "hms"    and "dms"    in arcseconds
c		 "arcsec"              in arcseconds
c		 "absdeg" and "reldeg" in degrees
c		 "absghz" and "relghz" in GHz
c		 "abskms" and "relkms" in Km/s
c	 	 "abslin" and "rellin" in linear coordinates
c	S is the radius of circle overlays.  It is in the units given
c	in the above list according to XOTYPE only.
c
c	CS to CE is the channel range (image planes) on which to put the 
c	overlays.  If you specify only CS than the overlay is put
c	on that channel.  If CS=0 then the overlays are put on all
c	channels. 
c
c	For OFIG="box" and "star", XS, YS are optional.  The defaults
c	are XS=2, YS=XS pixels.   In all cases, CS and CE  are optional
c	and the default is 0 (all channels)
c
c
c	#####  The OFFSET line
c
c	At any point in the overlay file, you can include an OFFSET
c	line in the format
c	
c	"OFFSET"   XOFF   YOFF
c
c	where the literal "OFFSET" (without the quotes) must appear
c	as the first thing in the line, followed by X and Y offsets,
c	which are applied to all succeeding overlay file locations.
c	       X = X + XOFF;   Y = Y + YOFF
c	These offsets must be in the same units as the %OTYPE that the
c	succeeding line(s) has(ve).  It is intended so that your overlay
c	locations can be in, say, arcsec relative to some location which
c	is not the reference pixel of the image (which is what CGDISP
c	ultimately wants).   You then specify, with the OFFSET line, the
c	offsets between the reference pixel of the contour/pixel map
c	images and the actual reference location of your overlay locations.
c
c	You can have as many OFFSET lines as you like in the file.  All
c	succeeding lines will apply these offsets until new ones are
c	defined.  If the line does not appear, naturally no additional
c	offsets are added.
c
c	The OFFSET line is not applied to ANY position fields in succeeding
c	lines that have %OTYPEs that are "hms" or "dms".    I am too lazy
c	to code it.
c
c--
c
c  History:
c    nebk 27Aug89  Original and near perfect version
c    rjs   4oct89  Fixed some nonstandard FORTRAN.
c    rjs  23oct89  Changed 'pdev' to 'device'.
c    rjs  15nov89  Changed call sequence to mkeyr.
c    pjt   2may90  Replaced maxchan by mxchan because of new maxdim.h
c    nebk  9sep90  Fixed some wrong code in dim. compat. checks.
c    nebk 12oct90  Add floating solid/dashed contour level break point
c    rjs  25oct90  Merges nebk's version with the BIMA version.
c    nebk 17dec90  Increase size of annot to 3 characters
c    nebk 09jan91  Combine plev and alev into slev. Replace chan,blc,trc
c                  by region and reduced chan. combine ofile and otype 
c		   into olay. Combine annot, aspect & part of lines into
c		   options. Interpret returned pgbegin status correctly.
c    nebk 14jan91  Deal with blanked pixels and redistribute top level code.
c    nebk 18jan91  Add second contour image
c    nebk 22jan91  Change default plot device to "?"
c    nebk 30jan91  Change data statement for contour blanking to a standard
c                  F77 syntax and speed up contouring for an unblanked image
c    nebk  5mar91  Change itoa to itoaf, atoi to atoif, atod to atodf
c                  and add epoch to annotation of plot.
c    mjs/nebk
c         10mar91  Removed concatenated char*(*) variables in sub calls
c    nebk 12mar91  Change keya to keyf for input files.
c    nebk 09apr91  Better memory management to allow bigger images.
c    nebk 24apr91  Adjust for new pgtime routines
c    nebk 05jun91  Adjust calls to pgpage
c    rjs  22jul91  Added 's' flag to BoxSet calls.
c    nebk 10aug91  Was not recognizing "lo" type overlays.
c    nebk 11aug91  Account for offset for log grey scales and cin=gin
c    nebk 04sep91  Deal with discontinously selected groups of channels.
c    nebk 11sep91  Add options=beam%% and rename from cgplot
c    nebk 20sep91  Stripped subroutines common with pgcurs into subspg.for
c    nebk/mjs
c         12nov91  Initialize ep2 if no second contour image
c    nebk/mjs
c         13mar92  Name change:  pgdisp -> cgdisp
c    nebk 07apr92  Adapt to memalloc subs. & add source name to annotation
c    nebk 29apr92  Fix problem with character size getting lost and
c                  rename *pg subroutines to *cg
c    nebk 05may92  Full annotation not showing for nxy=1 (introduced 29apr92)
c    nebk 18may92  Major road works to add extra contour & vector images
c    nebk 02jun92  Allow contour images to be the same, bring olay in-line
c		   with new labtyp & change call to chnselcg
c    nebk 06jun92  Combine overlay drawing into one subroutine and cause
c		   all lines in olay file begining with # to be ignored
c    nebk 22jun92  Put all overlay info into overlay file and impliment
c		   "line" and "clear" overlays. CHange options=equal
c		   to unequal.   
c    nebk 04jul92  Strip otopix, settr, linelev, conwrite, contents of
c		   fullann, vpchdef, chkds2 to cgsubs.for. add op=mirror
c		   & replace readc/g/v with readimcg.  Use deghmscg
c    nebk 10jul92  Go to optcg instead of options. Remove call to BOXMASK
c    nebk 17jul92  Add the overlay  offset line facility.
c    nebk 09aug92  Modify for new o2pixcg/w2pixcg code, strip code to
c		   strprpcg and omatchcg
c    nebk 01oct92  Overlay size decoding code was rubbish as of 09aug92.
c    nebk 16oct92  Add informational suggestion for options=unequal use
c    nebk 28nov92  Add velocity and frequency label types and change
c                  linear -> abslin.
c    nebk 28jan93  Remove some unnecessary code to do with opening
c                  the same file twice
c    mjs  12mar93  Use maxnax.h file instead of setting own value.
c    mjs  13mar93  pgplot subr names have less than 7 chars.
c    nebk 21apr93  Replace options=chan,vel by generic 3pix,3vel
c    nebk 20may93  Tell user to use options=relax when appropriate
c    nebk 29may93  Replace call to chtonvcg by new PG routine pgqcs
c    nebk 02jun93  Replace calls to vssizecg by new PG routine pgqvsz
c    nebk 23jun93  Add options=wedge. Use pgqcs to remove need for vpasp,
c                  change for new call to vpadjcg, rework beam plotting.
c                  Make "clear" overlays appear again !
c    nebk 15jul93  Try and make beams come out right way around again
c    nebk 24aug93  Convert overlay channel field to channel range. Add
c		   LABTYPs "absdeg" and "reldeg".  Add options=noerase
c                  and noepoch
c    nebk 17nov93  's' flag to boxset. Add labtyp="none".
c    nebk 14dec93  Add type=mask, ofig=circle. Strip limits to limitscg
c    nebk 03jan94  New call to matchcg (formerly omatchcg)
c    nebk 09jan94  Convert crpix to double precision
c    nebk 29jan94  Add options=fiddle and range transfer function types
c		   "heq" and "sqr".  Strip viewsize to vpsizcg. Allow
c                  clear overlays to fall off edge of image.
c    nebk 05feb04  Add 1 grey range/transfer f'n per subplot ability
c    nebk 02mar94  New call and location for SETLABCG
c    nebk 11mar94  Implement spatial binning and OPTIONS=SINGLE
c    nebk 16jun94  Clarify use of region keyword, add overlay type
c		   "ocircle" and change "circle" to include radius.
c		   Better locating of overlay ID string.
c    nebk 30jun94  Add image type "box", and add one more "line" 
c		   argument to make axes independent.
c    nebk 08aug94  Remove 's' from boxset which naughty robbie included 
c                  in 1991. This broke the ability to handle 
c                  discontinuous planes (for 3 years !)
c    nebk 26aug94  Change to convert overlay locations in true world
c                  coordinates to linear world coordinates for plotting.
c                  Linearize axis descriptors at the centre of the
c                  displayed region.  Call new LAB3CG which labels with true 
c                  world coords on third axis.
c    nebk 23dec94  Make sure selected region no bigger than image
c    nebk 05jan95  Use new PGIMAG in favour of PGGRAY adding support
c                  for fiddling of lookup table for hardcopy devices
c                  Make sure annotation writes reference location as 
c                  original, not linearized version
c    nebk 20feb95  Add colour table selection to keyword "range" and
c		   get pgimag to make black on white for hard copy.
c		   Move to image type "pixel" instead of "grey"
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      real wedwid, wedisp, tfdisp
      integer maxlev, maxpos, nxdef, nydef, maxcon, 
     +        maxtyp, nbins, maxgr
      parameter (maxlev = 50, maxpos = 1000, nxdef = 4, maxtyp = 14,
     +  nydef = 4, maxcon = 3, maxgr = 100, wedwid = 0.05,
     +  wedisp = 1.0, tfdisp = 0.5, nbins = 128)
c
      integer ipim, ipnim, ipim2, ipnim2, ipimm
c
      integer csize(maxnax,maxcon), gsize(maxnax), vsize(maxnax,2),
     +  msize(maxnax), bsize(maxnax), size(maxnax), cnaxis(maxcon), 
     +  gnaxis, vnaxis(2), mnaxis, bnaxis, naxis, lc(maxcon), lg, 
     +  lv(2), lm, lb, lhead
      real cepoch(maxcon), gepoch, vepoch(2), mepoch, bepoch, epoch
      double precision cdelt(maxnax), crval(maxnax),
     +  scdelt(maxnax), scrval(maxnax),
     +  ccdelt(maxnax,maxcon), ccrval(maxnax,maxcon), 
     +  gcdelt(maxnax), gcrval(maxnax), 
     +  vcdelt(maxnax,2), vcrval(maxnax,2), 
     +  mcdelt(maxnax), mcrval(maxnax),
     +  bcdelt(maxnax), bcrval(maxnax),
     +  crpix(maxnax), scrpix(maxnax), ccrpix(maxnax,maxcon), 
     +  gcrpix(maxnax), vcrpix(maxnax,2), mcrpix(maxnax),
     +  bcrpix(maxnax), opos(6,maxpos)
      logical maskc(maxcon), maskg, maskv(2), maskm, maskb
      character*9 ctype(maxnax), sctype(maxnax), cctype(maxnax,maxcon), 
     +  gctype(maxnax), vctype(maxnax,2), mctype(maxnax), bctype(maxnax)
      character cin(maxcon)*64, gin*64, vin(2)*64, mskin*64, bin*64,
     +  ltypes(maxtyp)*6
c
      real levs(maxlev,maxcon), pixr(2,maxgr), tr(6), bmin(maxcon+4), 
     +  bmaj(maxcon+4), bpa(maxcon+4), bxfac(maxcon+4),
     +  byfac(maxcon+4), scale(2), cs(3), pixr2(2), slev(maxcon),
     +  break(maxcon), vfac(2), bfac(5), tfvp(4), wdgvp(4), 
     +  cumhis(nbins), gmm(2), cmm(2,maxcon), dmm(2)
      real xmin, xmax, ymin, ymax, vxmin, vymin, vymax, vx, vy,
     +  vxsize, vysize, vxgap, vygap, ydispb, xdispl, groff, blankg,
     +  blankc, blankv, blankb, vecfac, boxfac
      real x1, x2, y1, y2
c
      integer blc(3), trc(3), win(2), lwid(maxcon+3), vecinc(2), 
     +  boxinc(2), srtlev(maxlev,maxcon), nlevs(maxcon), 
     +  grpbeg(maxchan), ngrp(maxchan), his(nbins), ibin(2), jbin(2), 
     +  kbin(2), krng(2), coltab(maxgr)
      integer  nx, ny, lpos, npos, ierr, pgbeg, ilen, igr, iofm,
     +  nlast, ngrps, ncon, i, j, nvec, ipage, jj, npixr, wedcod
c
      character ofig(maxpos)*10, posid(maxpos)*20, labtyp(2)*6, 
     +  levtyp(maxcon)*1
      character pdev*64, xlabel*40, ylabel*40, xopts*20, yopts*20,
     +  hard*20, ofile*64, xxopts*22, yyopts*22, trfun(maxgr)*3,
     +  aline*72
c
      logical solneg(maxcon), doblv(2), bemprs(maxcon+4), owrite(maxpos)
      logical do3val, do3pix, dofull, gaps, eqscale, doblc, doblg,
     +  dobeam, beaml, beamb, relax, rot90, signs, mirror, dowedge, 
     +  doerase, doepoch, bdone, doblb, doblm, dofid, dosing, reverse
c
      data blankc, blankv, blankb /-99999999.0, -99999999.0, 
     +                             -99999999.0/
      data lc, lg, lv, lb, lm /maxcon*0, 0, 2*0, 0, 0/
      data gin, vin, bin, mskin /' ', 2*' ', ' ', ' '/
      data bdone /.false./
      data ipage /0/
      data ltypes /'hms   ', 'dms   ', 'abspix', 'relpix', 'arcsec',
     +             'absghz', 'relghz', 'abskms', 'relkms', 'abslin', 
     +             'rellin', 'absdeg', 'reldeg', 'none'/
      data dmm /2*0.0/
c-----------------------------------------------------------------------
      call output ('CgDisp: version 20-Feb-95')
      call output ('Keyword "range" can now be used to specify the')
      call output ('colour lookup table as well the transfer function')
      call output (' ')
      call output ('Options=fiddle is now keyboard driven for '//
     +             'hard-copy devices')
      call output (' ')
c
c Get user inputs
c
      call inputs (maxgr, maxlev, maxcon, maxtyp, ltypes, ncon, cin, 
     +   gin, nvec, vin, bin, mskin, ibin, jbin, kbin, levtyp, slev, 
     +   levs, nlevs, npixr, pixr, trfun, coltab, vecfac, vecinc, 
     +   boxfac, boxinc, pdev, labtyp, dofull, do3val, do3pix, eqscale, 
     +   gaps, solneg, nx, ny, lwid, break, cs, scale, ofile, dobeam, 
     +   beaml, beamb, relax, rot90, signs, mirror, dowedge, doerase, 
     +   doepoch, dofid, dosing)
c
c Open images as required
c
      call sesame (relax, maxdim, maxnax, maxcon, ncon, cin, lc, cnaxis, 
     +  csize, cepoch, maskc, ccrpix, ccdelt, ccrval, cctype, gin, lg, 
     +  gnaxis, gsize, gepoch, maskg, gcrpix, gcdelt, gcrval, gctype,
     +  vin, lv, vnaxis, vsize, vepoch, maskv, vcrpix, vcdelt, vcrval,
     +  vctype, bin, lb, bnaxis, bsize, bepoch, maskb, bcrpix, bcdelt,
     +  bcrval, bctype, mskin, lm, mnaxis, msize, mepoch, maskm, mcrpix,
     +  mcdelt, mcrval, mctype, gmm, cmm)
c
c Finish key inputs for region of interest and return generic 
c axis descriptors
c
      call region (maxcon, maxnax, ncon, cin, gin, vin, bin, lc, lg,
     +   lv, lb, cnaxis, gnaxis, vnaxis, bnaxis, csize, gsize, vsize, 
     +   bsize, ccrval, gcrval, vcrval, bcrval, ccdelt, gcdelt, vcdelt,
     +   bcdelt, ccrpix, gcrpix, vcrpix, bcrpix, cctype, gctype, vctype,
     +   bctype, cepoch, gepoch, vepoch, bepoch, naxis, size, crval, 
     +   cdelt, crpix, ctype, epoch, ibin, jbin, kbin, blc, trc, win, 
     +   maxchan, grpbeg, ngrp, ngrps, lhead)
c
c Try to allocate memory for images
c
      call memalloc (ipim,  win(1)*win(2), 'r')
      call memalloc (ipnim, win(1)*win(2), 'i')
      if (vin(1).ne.' ' .and. vin(2).ne.' ') then
        call memalloc (ipim2,  win(1)*win(2), 'r')
        call memalloc (ipnim2, win(1)*win(2), 'i')
      end if
      if (mskin.ne.' ') call memalloc (ipimm,  win(1)*win(2), 'l')
c
c Compute contour levels for each contour image
c
      if (ncon.gt.0) then
        do i = 1, ncon
          call conlevcg (mirror, maxlev, lc(i), cnaxis(i), csize(1,i), 
     +       levtyp(i), slev(i), nlevs(i), levs(1,i), srtlev(1,i))
        end do
      end if
c
c Save and linearize axis descriptors if non-pixel labels requested
c
      call savdescg (naxis, ctype, crval, crpix, cdelt, sctype,
     +               scrval, scrpix, scdelt)
      call linco (lhead, labtyp, blc, trc, grpbeg, ngrp, ctype,
     +            crval, crpix, cdelt)
c
c Work out array index limits, coordinate transformation array & labels
c
      call limitscg (labtyp, blc, trc, naxis, epoch, crpix, cdelt,
     +   crval, ctype, doepoch, xmin, xmax, ymin, ymax, ibin, jbin,
     +   tr, xlabel, ylabel)
c
c Get beam information
c
      if (dobeam) 
     +   call getbeam (maxcon, cin, lc, gin, lg, vin, lv, bin, lb,
     +      labtyp, naxis, crval, crpix, cdelt, ctype, bmin, bmaj, bpa,
     +      bxfac, byfac, dobeam, bemprs)
c
c Work out number of plots per page and number of plots
c
      call nxnycg (nxdef, nydef, ngrps, nx, ny, nlast)
      if (ngrps.eq.1 .or. nx*ny.eq.1) gaps = .true.
      npixr = min(ngrps,npixr)
c
c Work out if wedge outside or inside subplots. Also work out
c if plotting one wedge per subplot or one wedge for all
c
      call wedgincg (dowedge, nx, ny, npixr, trfun, wedcod)
c
c Work out default character sizes for axis and channel labels
c
      call defchrcg (nx, ny, cs(1))
      write (aline, 100) cs(1), cs(2)
100   format ('Character sizes (axes & velocity) are: ', f3.1, ', ', 
     +         f3.1)
      call output (aline)
c
c Open plot device 
c
      ierr = pgbeg (0, pdev, 1, 1)
      if (ierr.ne.1) then
        call pgldev
        call bug ('f', 'Error opening plot device') 
      end if
      call pgpage
      call pgscf(2)
      call pgqinf ('hardcopy', hard, ilen)
c
c Init OFM routines 
c
      call ofmini
c  
c Set label displacements from axes and set PGTBOX labelling 
c option strings
c
      call setlabcg (labtyp, ymin, ymax, xdispl, ydispb, xopts, yopts)
c
c Work out view port sizes and increments.
c
      call vpsizcg (dofull, dofid, ncon, gin, vin, 0, bin, maxlev,
     +   nlevs, srtlev, levs, slev, nx, ny, cs, xdispl, ydispb, 
     +   gaps, wedcod, wedwid, wedisp, tfdisp, labtyp, vxmin, vymin, 
     +   vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales 
c requested or if scales provided by user
c
      call vpadjcg (hard, eqscale, scale, vxmin, vymin, vymax, nx, ny,
     +   blc, trc, naxis, crval, crpix, cdelt, ctype, tfvp, wdgvp, 
     +   vxsize, vysize)
c
c Set viewport location of first sub-plot
c
      vx = vxmin
      vy = vymax - vysize
c
c Loop over number of sub-plots
c
      do j = 1, ngrps
         if (mod(j,nx*ny).eq.1 .or. nx*ny.eq.1) ipage = ipage + 1
         jj = j - (ipage-1)*nx*ny
         if (hard.eq.'YES') then
            write (aline, '(a,i3)') 'Beginning plane ', grpbeg(j)
            call output (aline)
         end if
c
c Read overlay positions list and decode positions into pixels appropriate
c to the channel we are currently displaying
c
         call olaydec (lhead, grpbeg(j), ngrp(j),  maxtyp, ltypes, 
     +     maxpos, lpos, ofile, ofig, npos, opos, posid, owrite)
c
c Set viewport and window for current sub-plot.  
c
         call pgsvp (vx, vx+vxsize, vy, vy+vysize)
         call pgswin (xmin, xmax, ymin, ymax)
         call pgqvp (2, x1, x2, y1, y2)
         call pgsch (cs(1))
c
c Read in mask image as required
c
         if (mskin.ne.' ') then
           if (msize(3).gt.1) then
             krng(1) = grpbeg(j)
             krng(2) = ngrp(j)
             call readbcg (.true., lm, ibin, jbin, krng, blc, trc, 
     +                     meml(ipimm), doblm)
           else
             if (.not.bdone) then
               krng(1) = 1
               krng(2) = 1
               call readbcg (.true., lm, ibin, jbin, krng, blc, trc, 
     +                       meml(ipimm), doblm)
             else
               bdone = .true.
             end if
           end if
         end if
c
c Deal with pixel map image 
c
         if (gin.ne.' ') then
           if (gsize(3).gt.1) then
             krng(1) = grpbeg(j)
             krng(2) = ngrp(j)
           else
             krng(1) = 1
             krng(2) = 1
           end if
c
c Apply transfer function to pixel range
c
           igr = min(j,npixr)
           call grfixcg (pixr(1,igr), lg, gnaxis, gsize, 
     +                   trfun(igr), pixr2, groff, blankg)
c
c Read pixel map image and apply mask
c
           call readimcg (.true., maskg, blankg, lg, ibin, jbin, krng,
     +         blc, trc, .true., memi(ipnim), memr(ipim), doblg, gmm)
           if (mskin.ne.' ' .and. doblm) then
             call maskorcg (blankg, win, meml(ipimm), memi(ipnim),
     +                      memr(ipim))
             doblg = .true.
           end if
c
c Apply transfer function directly to image
c
           if (trfun(igr).ne.'lin') 
     +       call apptrfcg (pixr2, trfun(igr), groff, win(1)*win(2),
     +          memi(ipnim), memr(ipim), nbins, his, cumhis)
c
c Apply user specified OFM to device.   Here a b&w ofm will be 
c applied by default if user has not specified one with keyword "range".
c
           call ofmcol (coltab(igr), pixr2(1), pixr2(2))
c
c Interactive modification of OFM for hardcopy devices here; must be 
c done before PGIMAG called.  Any chnage of lookup table here will
c overwrite that done with call to ofmcol above
c
           if (hard.eq.'YES' .and. dofid .and. (jj.eq.1 .or. dosing))
     +       call ofmmod (tfvp, win(1)*win(2), memr(ipim), 
     +                    memi(ipnim), pixr2(1), pixr2(2))
c
c Draw image.  Note that for hardcopy devices we generally want
c black on white, not white on black.  So if no colour table has 
c been applied, make it so.  Yes Captain.
c
           reverse = .false.
           if (hard.eq.'YES') then
             call ofminq (iofm)
             if (iofm.eq.1) reverse = .true.
           end if
c
           if (reverse) then
             call pgimag (memr(ipim), win(1), win(2), 1, win(1), 
     +                    1, win(2), pixr2(2), pixr2(1), tr)
           else
             call pgimag (memr(ipim), win(1), win(2), 1, win(1), 
     +                    1, win(2), pixr2(1), pixr2(2), tr)
           end if
         end if
c
c Label and draw axes before fiddle else looks silly. Also forces
c update of pixel map on screen.
c
         call pgslw(lwid(1))
         call pgsci (7)
         if (hard.eq.'YES') call pgsci (2)
         call axlabcg (gaps, nx, ny, ngrps, nlast, j, xopts, yopts,
     +      xdispl, ydispb, labtyp, xlabel, ylabel, xxopts, yyopts)
         call pgtbox (xxopts, 0.0, 0, yyopts, 0.0, 0)
c
c Draw wedge now so that it overwrites axis label ticks when wedge
c drawn inside subplot
c
         if (dowedge) call wedgecg (reverse, wedcod, wedwid, jj, 
     +                  trfun(igr), groff, nbins, cumhis, 
     +                  wdgvp, pixr(1,igr), pixr(2,igr))
c
c Interactive modification of OFM for interactive devices here
c
         if (hard.eq.'NO' .and. dofid .and. 
     +       ((jj.eq.nx*ny .or. j.eq.ngrps) .or. dosing))
     +     call ofmmod (tfvp, win(1)*win(2), memr(ipim), 
     +                  memi(ipnim), pixr2(1), pixr2(2))
c
c Draw contour plots
c
         if (ncon.gt.0) then
           do i = 1, ncon
             if (csize(3,i).gt.1) then
               krng(1) = grpbeg(j)
               krng(2) = ngrp(j)
             else
               krng(1) = 1
               krng(2) = 1
             end if
             call readimcg (.true., maskc, blankc, lc(i), ibin, jbin, 
     +         krng, blc, trc, .true., memi(ipnim), memr(ipim), doblc,
     +         cmm(1,i))
             if (mskin.ne.' ' .and. doblm) then
               call maskorcg (blankc, win, meml(ipimm), memi(ipnim), 
     +                        memr(ipim))
               doblc = .true.
             end if
c
             call pgslw (lwid(i+1))
             call pgsci (7+i-1)
             call conturcg (blankc, solneg(i), win(1), win(2), doblc,
     +          memr(ipim), nlevs(i), levs(1,i), tr, break(i))
             call pgupdt
           end do
         end if
c
c Draw vector plots
c
         if (vin(1).ne.' ' .and. vin(2).ne.' ') then
           if (vsize(3,1).gt.1) then
             krng(1) = grpbeg(j)
             krng(2) = ngrp(j)
           else
             krng(1) = 1
             krng(2) = 1
           end if
           call readimcg (.true., maskv(1), blankv, lv(1), ibin, jbin,
     +                    krng, blc, trc, .true., memi(ipnim), 
     +                    memr(ipim), doblv(1), dmm)
           if (mskin.ne.' ' .and. doblm) then
             call maskorcg (blankv, win, meml(ipimm), memi(ipnim),
     +                      memr(ipim))
             doblv(1) = .true.
           end if
           call readimcg (.true., maskv(2), blankv, lv(2), ibin, jbin,
     +                    krng, blc, trc, .true., memi(ipnim2), 
     +                    memr(ipim2), doblv(2), dmm)
           if (mskin.ne.' ' .and. doblm) then
             call maskorcg (blankv, win, meml(ipimm), memi(ipnim2),
     +                      memr(ipim2))
             doblv(2) = .true.
           end if
c
           call pgslw (lwid(ncon+2))
           call pgsci (2)
c
           call drawvec (j, tr, vecfac, vecinc, win(1), win(2), 
     +        memr(ipim), memi(ipnim), memr(ipim2), memi(ipnim2),
     +        cdelt, scale, signs, rot90, vfac, nx, ny)
         end if
c
c Draw box plots
c
         if (bin.ne.' ') then
           if (bsize(3).gt.1) then
             krng(1) = grpbeg(j)
             krng(2) = ngrp(j)
           else
             krng(1) = 1
             krng(2) = 1
           end if
           call readimcg (.true., maskb, blankb, lb, ibin, jbin,
     +                    krng, blc, trc, .true., memi(ipnim), 
     +                    memr(ipim), doblb, dmm)
           if (mskin.ne.' ' .and. doblm) then
             call maskorcg (blankb, win, meml(ipimm), memi(ipnim),
     +                      memr(ipim))
             doblb = .true.
           end if
c
           call pgsci (6)
           call drawbox (j, tr, boxfac, boxinc, win(1), win(2), 
     +        memr(ipim), memi(ipnim), cdelt, scale, bfac)
         end if
c
c Write velocity or channel label
c
         if (do3val .or. do3pix) then
           call pgslw (1)       
           call pgsch (cs(2))
           call pgsci (1)
           call lab3cg (lhead, doerase, do3val, do3pix, labtyp,
     +                  grpbeg(j), ngrp(j))
         end if
c
c Draw overlay(s)
c
         if (npos.gt.0) then
           call pgsci (3)
           call pgslw (lwid(ncon+nvec+2))
           call overl (doerase, ofig, owrite, blc, trc, npos, opos, 
     +        posid, grpbeg(j), cs(3), labtyp, naxis, crval, crpix, 
     +        cdelt, ctype)
         end if
c
c Draw beam(s)
c
         if (dobeam) then
           call pgsci (4)
           call beampl (maxcon, beaml, beamb, bmin, bmaj, bpa,
     +                  bxfac, byfac, bemprs)
         end if
c
c Plot annotation
c
         if (dofull .and. (jj.eq.nx*ny .or. j.eq.ngrps)) then
           call pgslw (1)
           call pgsci (1)
           if (hard.eq.'NO') call pgsci (7)
           call fullann (ncon, cin, gin, vin, bin, lc, lg, lv, lb,
     +        maxlev, nlevs, levs, srtlev, slev, npixr, trfun, pixr, 
     +        vfac, bfac, naxis, size, scrval, scrpix, scdelt, sctype,
     +        vymin, blc, trc, cs, ydispb, ibin, jbin, kbin, 
     +        labtyp, gmm, cmm)
           call pgsci (1)
         end if  
c
c Increment sub-plot viewport locations and row counter and
c reinit data min and maxesc
c
         call subinccg (j, nx, ny, vxmin, vymax, vxsize, vysize, 
     +                  vxgap, vygap, vx, vy)
         call mmini (maxcon, gmm, cmm)
c
c Page plot device
c
         if (jj.eq.nx*ny .and. j.lt.ngrps) call pgpage
       end do
c
c Close up
c
      call pgend
c
      call memfree (ipim,  win(1)*win(2), 'r')
      call memfree (ipnim, win(1)*win(2), 'i')
      if (vin(1).ne.' '  .and. vin(2).ne.' ') then
        call memfree (ipim2,  win(1)*win(2), 'r')
        call memfree (ipnim2, win(1)*win(2), 'i')
      end if     
      if (mskin.ne.' ') call memfree (ipimm, win(1)*win(2), 'i')
c
      do i = 1, ncon
        call xyclose (lc(i))
      end do
      if (gin.ne.' ') call xyclose (lg)
      if (vin(1).ne.' ') call xyclose (lv(1))
      if (vin(2).ne.' ') call xyclose (lv(2))
      if (mskin.ne.' ') call xyclose (lm)
c
      end
c
c  
      subroutine beamfac (in, lin, labtyp, naxis, crval, crpix, cdelt,
     +                    ctype, bmin, bmaj, bpa, bxfac, byfac, pres)
c-----------------------------------------------------------------------
c     Work out factors to convert radians to world coordinates
c     for each axis for plotting of beam.
c
c  Input
c   in          Image name
c   lin         Image handle
c   labtyp      Label types
c   naxis       Number of axes in image
c   cdelt       Axis increments
c   crval       Reference values
c   ctype       Axis types
c   crpix       Axis reference pixels
c  Output
c   bmin        FWHMin of beam in image 
c   bmaj        FWHMax of beam
c   bpa         p.a. of beam
c               All in radians
c   bx,yfac     Factors to scale the x and y axes of the beam so that it
c               comes out plotted in world coordinates of the type
c               labeling the plot
c   pres        True if beam present in header
c-----------------------------------------------------------------------
      implicit none
c
      integer lin, naxis
      real bmin, bmaj, bpa, bxfac, byfac
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
      character*(*) labtyp(2), ctype(naxis), in
      logical pres
cc
      character line*80
      integer il, len1, irad1, irad2
c-----------------------------------------------------------------------
      call rdhdr (lin, 'bmin', bmin,  -1.0)
      call rdhdr (lin, 'bmaj', bmaj,  -1.0)
      call rdhdr (lin, 'bpa',  bpa,  999.0)
c
      if (bmin.gt.0.0 .and. bmaj.gt.0.0) then
c
c Find if axes are those that have radian pixel increments. These are
c the only ones for which we can convert the beam size in radians 
c to world coordinates
c
        call axfndcg ('RAD', 1, ctype(1), irad1)
        call axfndcg ('RAD', 1, ctype(2), irad2)
c
        if (irad1*irad2.ne.0) then
          call beamfc2 (1, labtyp(1), naxis, ctype, cdelt, crval, 
     +                  crpix, bxfac)
          call beamfc2 (2, labtyp(2), naxis, ctype, cdelt, crval, 
     +                  crpix, byfac)
          pres = .true.
        else
          il = len1(in)
          line = 'Axes for image '//in(1:il)//
     +           ' are not recognized as having'
          call bug ('w', line)
          call bug ('w', 'increments in radians. Cannot plot beam')
        end if
      end if
c
      end
c
c
      subroutine beamfc2 (iax, labtyp, naxis, ctype, cdelt, crval,
     +                    crpix, fac)
c-----------------------------------------------------------------------
c  Find factor that converts radians to world coordinates
c
c  Input
c    iax       Axis we are interested in
c    labtyp    Axis label type for this axis
c    naxis     Number of axes in image
c    ctype     Axis types
c    cdelt     Axis increment
c    crval     Reference values for all axes
c  Output
c    fac       factor to convert from radians to world coords
c
c-----------------------------------------------------------------------
      implicit none
c
      integer iax, naxis
      character*(*) labtyp, ctype(naxis)
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      real fac
cc
      double precision delw
      logical ok
c-----------------------------------------------------------------------
      call pixi2wcg (.false., iax, labtyp, naxis, crval, crpix,
     +               cdelt, ctype, delw, ok)
      fac = delw / cdelt(iax)
c
      end
c
c
      subroutine beampl (maxcon, beaml, beamb, bmin, bmaj, bpa, bxfac,
     +                   byfac, bemprs)
c-----------------------------------------------------------------------
c     Draw one beam for each image being displayed.  They are drawn
c     confocally with different line styles in the designated corner.
c
c  Input
c    maxcon          Maximum number of contour images
c    beaml,beamb     True if the beam is to be drawn on the left
c                    or at the bottom (else right and top)
c    bmin,maj,pa     Beam FWHMin, FWHMax and p.a. for pixel map and
c                    contour 1 and 2 images (rad).
c    bx,yfac         Factors converting radians to world coordinates 
c                    on the x and y axes
c    bemprs          True if beam present for maxcon contours, pixel map
c                    and 2 vector images
c
c-----------------------------------------------------------------------   
      implicit none
c
      integer maxcon
      logical beaml, beamb, bemprs(maxcon+4)
      real bmin(maxcon+4), bmaj(maxcon+4), bpa(maxcon+4), 
     +  bxfac(maxcon+4), byfac(maxcon+4)
cc
      integer i
      logical fill
      real xcen, ycen
c-----------------------------------------------------------------------
c
c Find location of centre of biggest beam.  They will be plotted
c with the same centre.
c
      call beamxy (maxcon, beaml, beamb, bemprs, bmin, bmaj, bpa, bxfac,
     +             byfac, xcen, ycen, fill)
c
c Draw the beam(s)
c
      do i = 1, maxcon+4
        if (bemprs(i)) then
          call beampl2 (xcen, ycen, bmin(i), bmaj(i), bpa(i),
     +                  bxfac(i), byfac(i), fill)
        end if
      end do
c
      end
c
c
      subroutine beampl2 (xcen, ycen, bmin, bmaj, bpa, bxfac,
     +                    byfac, fill)
c-----------------------------------------------------------------------
c     Draw one beam in the designated corner of the sub-plot.
c
c  Input
c    x,ycen          World coordinates of beam centre
c                    or at the bottom (else right and top)
c    bmin,maj,pa     Beam FWHMin, FWHMax and p.a. for pixel map and
c                    contour image (rad). 
c    bx,yfac         Factors converting radians to world coordinates 
c                    on the x and y axes
c    fill            If true fill beam polygon in
c-----------------------------------------------------------------------   
      implicit none
c
      logical fill
      real bmin, bmaj, bpa, bxfac, byfac, xcen, ycen
cc
      include 'mirconst.h'
      real xs(0:360), ys(0:360), pa, xx, yy, cp, sp, bbmin, bbmaj, bbpa
      integer i
c-----------------------------------------------------------------------
      bbmin = bmin / 2.0
      bbmaj = bmaj / 2.0
      bbpa = (90.0 + bpa) * dpi / 180.0
      cp = cos(bbpa)
      sp = sin(bbpa)
c
c Work out the beam polygon
c
      do i = 0, 360
        pa = i * dpi / 180.0 	  
        xx = bbmaj * cos(pa)
        yy = bbmin * sin(pa)
c
        xs(i) = xcen + ( xx*cp + yy*sp)*bxfac
        ys(i) = ycen + (-xx*sp + yy*cp)*byfac
      end do
c
c  Draw the beam with the desired line style and fill style
c
      if (fill) then
        call pgsfs (1)
      else
        call pgslw (2)
        call pgsfs (2)
      end if
c
      call pgpoly (361, xs(0), ys(0))
c
      end
c
c
      subroutine beamxy (maxcon, beaml, beamb, bemprs, bmin, bmaj, 
     +                   bpa, bxfac, byfac, xcen, ycen, fill)
c-----------------------------------------------------------------------
c     We want to draw the beams, if there are more than one, with
c     the same centre.  Find the biggest x and y offsets from all
c     the beams and compute the centre with that.
c
c  Input
c    maxcon          Maximum number of contour images
c    beaml,beamb     True if the beam is to be drawn on the left
c                    or at the bottom (else right and top)
c    bemprs          True if beam present for pixel map and contour images
c    bmin,maj,pa     Beam FWHMin, FWHMax and p.a. for pixel map and
c                    contour image (rad). 
c    bx,yfac         Factors converting radians to world coordinates 
c                    on the x and y axes
c  Output
c    x,ycen          World coordiate of beam centres
c    fill            If true fill in the beam patch, else just outline
c-----------------------------------------------------------------------   
      implicit none
c
      integer maxcon
      logical beaml, beamb, bemprs(maxcon+4), fill
      real bmin(maxcon+4), bmaj(maxcon+4), bpa(maxcon+4), 
     +  bxfac(maxcon+4), byfac(maxcon+4), xcen, ycen
cc
      include 'mirconst.h'
      real  x, y, xmin, xmax, ymin, ymax, xoff, yoff, pa, xx, yy,
     +  cp, sp, bbmin, bbmaj, bbpa, xlo, xhi, ylo, yhi, xwmax, ywmax,
     +  bmino, bmajo, bpao
      integer i, j, sx, sy
      logical const
c-----------------------------------------------------------------------
      xwmax = -1.0e30
      ywmax = -1.0e30
      const = .true.
c
c Find first beam
c
      do i = 1, maxcon+4
        if (bemprs(i)) then
          bmino = bmin(i)
          bmajo = bmaj(i)
          bpao  = bpa(i)
        end if
      end do
c
c  Loop over beams
c 
      do j = 1, maxcon+4
        if (bemprs(j)) then
c
c  Make check to see if all beams the same, so can set fill style
c
          if (bmin(j).ne.bmino .or. bmaj(j).ne.bmajo .or. 
     +        bpa(j).ne.bpao) const = .false.
c
c  Calculate useful factors for beam
c
          bbmin = bmin(j) / 2.0
          bbmaj = bmaj(j) / 2.0
          bbpa = (90.0 + bpa(j)) * dpi / 180.0
          cp = cos(bbpa)
          sp = sin(bbpa)
c
c  Find height and width of ellipse empirically, because it is
c  too late to do the algebra
c
          xmin = 1.0e30
          xmax = -1.0e30
          ymin = 1.0e30
          ymax = -1.e30
c
          do i = 0, 360, 4
            pa = i * dpi / 180.0 	  
            xx = bbmaj * cos(pa)
            yy = bbmin * sin(pa)
c
            x = ( xx*cp + yy*sp)*bxfac(j)
            y = (-xx*sp + yy*cp)*byfac(j)
c  
            xmin = min(x,xmin)
            xmax = max(x,xmax)
            ymin = min(y,ymin)
            ymax = max(y,ymax)
          end do
c
c  Work out biggest x,y sizes
c
          xwmax = max(abs(xmax - xmin),xwmax)
          ywmax = max(abs(ymax - ymin),ywmax)
c
c Update
c
          bmino = bmin(j)
          bmajo = bmaj(j)
          bpao  = bpa(j)
        end if
      end do
c
c Fill beam plot ?
c
      fill = .false.
      if (const) fill = .true.
c
c  Find world coordinates of plotted window
c
      call pgqwin (xlo, xhi, ylo, yhi)
      sx = 1
      if (xlo.gt.xhi) sx = -1
      sy = 1
      if (ylo.gt.yhi) sy = -1
c
c  Work out ellipse centre.  Ellipse comes no close than 2.5%
c  of the width of the plot from the boundary.
c
      xoff = abs(0.025*(xhi-xlo))
      yoff = abs(0.025*(yhi-ylo))
c
      if (beaml) then
        xcen = xlo + sx*(xoff + xwmax/2.0)
      else
        xcen = xhi - sx*(xoff + xwmax/2.0)
      end if
c
      if (beamb) then
        ycen = ylo + sy*(yoff + ywmax/2.0)
      else
        ycen = yhi - sy*(yoff + ywmax/2.0)
      end if
c
      end
c
c
      subroutine chkdes (relax, im1, im2, size1, size2, crpix1, crpix2,
     +   cdelt1, cdelt2, crval1, crval2, epoch1, epoch2, ctype1, ctype2)
c-----------------------------------------------------------------------
c     Compare axis descriptors for the first three axes
c
c  Input:
c   im1,2        Images
c   size1,2      Sizes of each dimension
c   crpix1,2     Reference pixels
c   cdelt1,2     Increments
c   crval1,2     Refernce values
c   ctype1,2     types of axes
c   epoch1,2     Epochs
c-----------------------------------------------------------------------
      implicit none
c
      integer size1(3), size2(3)
      character*(*) im1, im2, ctype1(3), ctype2(3)
      double precision crval1(3), crval2(3), cdelt1(3), cdelt2(3),
     +  crpix1(3), crpix2(3)
      real epoch1, epoch2
      logical relax
cc
      integer k, l1, l2, len1, maxis
      logical got23
      character line*130
c-----------------------------------------------------------------------
      l1 = len1(im1)
      l2 = len1(im2)
c
c Allow 2-D with 3-D, but two 3-D cubes must be the same size
c
      if (size1(1).ne.size2(1)) then
        line = 'Unequal dimensions for images '//im1(1:l1)//
     +         ' & '//im2(1:l2)//' on axis 1'
        call bug ('f', line)
      end if
      if (size1(2).ne.size2(2)) then
        line = 'Unequal dimensions for images '//im1(1:l1)//
     +         ' & '//im2(1:l2)//' on axis 2'
        call bug ('f', line)
      end if
      if (size1(3).gt.1.and.size2(3).gt.1.and.size1(3).ne.size2(3)) then
        line = 'Inconsistent dimensions for images '//im1(1:l1)//
     +         ' & '//im2(1:l2)//' on axis 3'
        call bug ('f', line)
      end if
c
      if (epoch1.ne.epoch2) then
        line = 'Unequal epochs for images '//im1(1:l1)//' & '//im2(1:l2)
        if (relax) then
          call bug ('w', line)
        else
          call bug ('i', 'Try, with care, options=relax')
          call bug ('f', line)
        end if
      end if
c
c See if we have 2-D with 3-D
c
      got23 = .false.
      if ( (size1(3).eq.1 .and. size2(3).gt.1) .or.
     +     (size1(3).gt.1 .and. size2(3).eq.1)) got23 = .true.
c
c Loop over axes of interest
c
      maxis = 3
      if (size1(3).eq.1 .and. size2(3).eq.1) maxis = 2
      do k = 1, maxis
        if ( (k.eq.3 .and. .not.got23) .or. k.le.2) then
          call chkdescg (relax, 'cdelt', k, im1(1:l1), im2(1:l2), 
     +                   cdelt1(k), cdelt2(k))
          call chkdescg (relax, 'crpix', k, im1(1:l1), im2(1:l2), 
     +                   crpix1(k), crpix2(k))
          call chkdescg (relax, 'crval', k, im1(1:l1), im2(1:l2), 
     +                   crval1(k), crval2(k))
c
          if (ctype1(k).ne.ctype2(k)) then
            write (line, 10) im1(1:l1), im2(1:l2), k
10          format ('Unequal ctype for images ', a, ' & ', a, 
     +              ' on axis ', i1)
            if (relax) then
              call bug ('w', line)
            else
              call bug ('i', 'Try, with care, options=relax')
              call bug ('f', line)
            end if
          end if
        end if
      end do
c
      end
c
c
      subroutine chkim (maxnax, ncon, cin, csize, cepoch, ccrpix, 
     +   ccdelt, ccrval, cctype, gin, gsize, gepoch, gcrpix, gcdelt,
     +   gcrval, gctype, vin, vsize, vepoch, vcrpix, vcdelt, vcrval,
     +   vctype, bin, bsize, bepoch, bcrpix, bcdelt, bcrval, bctype, 
     +   mskin, msize, mepoch, mcrpix, mcdelt, mcrval, mctype, relax)
c-----------------------------------------------------------------------
c     Check all the images for internal consistency
c
c   Input:
c     maxnax     Maximum number of allowed dimenions for image
c     ncon       Number of contour images
c     relax      Only warnings instead of fatal errror for inconsistent
c                axis descriptors
c     *in        Input image names
c     *size      Size of each dimensions of images
c     *epoch     Epochs of images
c     *crpix     Reference pixels
c     *cdelt     Increments
c     *crval     Reference values
c     *ctype     Axis types
c
c-----------------------------------------------------------------------
      implicit none
c
      integer maxnax, ncon, csize(maxnax,*), gsize(maxnax), 
     +  vsize(maxnax,2), bsize(maxnax), msize(maxnax)
      double precision 
     +  ccrval(maxnax,*), ccdelt(maxnax,*), ccrpix(maxnax,*),
     +  gcrval(maxnax),   gcdelt(maxnax),   gcrpix(maxnax),
     +  vcrval(maxnax,2), vcdelt(maxnax,2), vcrpix(maxnax,2),
     +  bcrval(maxnax),   bcdelt(maxnax),   bcrpix(maxnax),
     +  mcrval(maxnax),   mcdelt(maxnax),   mcrpix(maxnax,*)
      real cepoch(*), gepoch, vepoch(2), bepoch, mepoch
      character*(*) cin(*), gin, vin(2), bin, mskin, 
     +  cctype(maxnax,*), gctype(maxnax), vctype(maxnax,2), 
     +  bctype(maxnax), mctype(maxnax)
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
            call chkdes (relax, cin(i), cin(j), csize(1,i), csize(1,j),
     +         ccrpix(1,i), ccrpix(1,j), ccdelt(1,i), ccdelt(1,j), 
     +         ccrval(1,i), ccrval(1,j), cepoch(i), cepoch(j), 
     +         cctype(1,j), cctype(1,j))
          end do
        end do
      end if
c
c Check vector images for self consistency
c
      if (vin(1).ne.' ') call chkdes (relax, vin(1), vin(2), vsize(1,1),
     +   vsize(1,2), vcrpix(1,1), vcrpix(1,2), vcdelt(1,1), vcdelt(1,2),
     +   vcrval(1,1), vcrval(1,2), vepoch(1), vepoch(2), vctype(1,1), 
     +   vctype(1,2))
c
c Check first contour image for consistency with other images
c
      if (ncon.gt.0) then
        if (gin.ne.' ') call chkdes (relax, cin, gin, csize, gsize,
     +         ccrpix, gcrpix, ccdelt, gcdelt, ccrval, gcrval, 
     +         cepoch, gepoch, cctype, gctype)
        if (vin(1).ne.' ') call chkdes (relax, cin, vin, csize, vsize,
     +         ccrpix, vcrpix, ccdelt, vcdelt, ccrval, vcrval, 
     +         cepoch, vepoch, cctype, vctype)
        if (bin.ne.' ') call chkdes (relax, cin, bin, csize, bsize,
     +         ccrpix, bcrpix, ccdelt, bcdelt, ccrval, bcrval, 
     +         cepoch, bepoch, cctype, bctype)
        if (mskin.ne.' ') call chkdes (relax, cin, mskin, csize, msize,
     +         ccrpix, mcrpix, ccdelt, mcdelt, ccrval, mcrval, 
     +         cepoch, mepoch, cctype, mctype)
      end if
c
c Check pixel map images for consistency with other images
c
      if (gin.ne.' ') then
        if (vin(1).ne.' ') call chkdes (relax, gin, vin, gsize, vsize,
     +         gcrpix, vcrpix, gcdelt, vcdelt, gcrval, vcrval, 
     +         gepoch, vepoch, gctype, vctype)
        if (bin.ne.' ') call chkdes (relax, gin, bin, gsize, bsize,
     +         gcrpix, bcrpix, gcdelt, bcdelt, gcrval, bcrval, 
     +         gepoch, bepoch, gctype, bctype)
        if (mskin.ne.' ') call chkdes (relax, gin, mskin, gsize, msize,
     +         gcrpix, mcrpix, gcdelt, mcdelt, gcrval, mcrval, 
     +         gepoch, mepoch, gctype, mctype)
      end if
c
c Check vector images for consistency with other images
c
      if (vin(1).ne.' ') then
        if (bin.ne.' ') call chkdes (relax, vin, bin, vsize, bsize,
     +         vcrpix, bcrpix, vcdelt, bcdelt, vcrval, bcrval, 
     +         vepoch, bepoch, vctype, bctype)
        if (mskin.ne.' ') call chkdes (relax, vin, mskin, vsize, msize,
     +         vcrpix, mcrpix, vcdelt, mcdelt, vcrval, mcrval, 
     +         vepoch, mepoch, vctype, mctype)
      end if
c
c Check box image for consistency with other images
c
      if (bin.ne.' ') then
        if (mskin.ne.' ') call chkdes (relax, bin, mskin, bsize, msize,
     +         bcrpix, mcrpix, bcdelt, mcdelt, bcrval, mcrval, 
     +         bepoch, mepoch, bctype, mctype)
      end if
c
      end
c
c
      subroutine decopt  (dofull, do3val, do3pix, eqscale, gaps, solneg,
     +   beambl, beambr, beamtl, beamtr, relax, rot90, signs, 
     +   mirror, dowedge, doerase, doepoch, dofid, dosing)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     dofull    True means do full annotaiton of plot
c     do3val    True means label sub-plots with value of third axis
c     do3pix    True means label sub-plots with pixel of third axis
c     doerase   True means erase rectangle into which 3-axis label written
c     eqscale   True means plot with x and y scales
c     gaps      True menas put space bewteen adjacent sub-plots
c     solneg    True means plot negative contours with solid line
c               style and positive contours with dashed line style
c               One for each contour image
c     beam%%    Beam location
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error
c     rot90     Rotate vectors by 90 degrees
c     signs     WHen plotting vectors, assume N and E are in
c               the direction of increasing X and Y, else N and E
c               are to the top and left
c     mirror    Multiply contours by -1 and add to list
c     dowedge   Draw wedge on pixel map image
c     doepoch   Write epoch into axis labels
c     dofid     Interactive fiddle
c     dosing    FIddle after every subplot
c-----------------------------------------------------------------------
      implicit none
c
      logical dofull, do3val, do3pix, eqscale, gaps, solneg(*),
     +  beambl, beambr, beamtl, beamtr, relax, rot90, signs,
     +  mirror, dowedge, doerase, doepoch, dofid, dosing
cc
      integer maxopt
      parameter (maxopt = 21)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'full    ', '3value  ', '3pixel  ', 'unequal ', 
     +              'gaps    ', 'solneg1 ', 'solneg2 ', 'solneg3 ',
     +              'beambl  ', 'beambr  ', 'beamtl  ', 'beamtr  ',
     +              'relax   ', 'rot90   ', 'signs   ', 'mirror',
     +              'wedge   ', 'noerase ', 'noepoch ', 'fiddle',
     +              'single  '/
c-----------------------------------------------------------------------
      call optcg ('options', opshuns, present, maxopt)
c
      dofull    =      present(1)
      do3val    =      present(2)
      do3pix    =      present(3)
      eqscale   = .not.present(4) 
      gaps      =      present(5)
      solneg(1) =      present(6)
      solneg(2) =      present(7)
      solneg(3) =      present(8)
      beambl    =      present(9)
      beambr    =      present(10)
      beamtl    =      present(11)
      beamtr    =      present(12)
      relax     =      present(13)
      rot90     =      present(14)
      signs     =      present(15)
      mirror    =      present(16)
      dowedge   =      present(17)
      doerase   = .not.present(18)
      doepoch   = .not.present(19)
      dofid     =      present(20)
      dosing    =      present(21)
c
      end
c
c
      subroutine drawbox (iplot, tr, boxfac, boxinc, npixx, npixy, 
     +   image, nimage, cdelt, scale, bfac)
c-----------------------------------------------------------------------
c     Draw boxes.  The boxes will come out square only if the pixel
c     increment is the and the user has requested equal scales.
c
c  Input
c    iplot    sub-plot number.  Only work out vector length scale
c             factors from first sub-plot so that the correct number
c             get written into the plot annotation at the end
c             of each page
c    tr       Transformation matrix from pixels to world coordinates
c    boxfac   Multiply box widths by this factor after self-scaling
c    boxinc   Increment through image in these steps
c    npixx,y  Size of images in pixles
c    image    Image
c    nimage   Normalization image
c    cdelt    Array of pixel increments
c    scale    Scale in linear coords per mm.  For RA axes radians on
c             the sky per mm
c  Input/output:
c    bfac     (1)   Maximum value of pixles in region of first subplot
c             (2:3) Scale factors, in x and y, to convert pixel value
c                   into box width in world coordinates
c             (4:5) Scale factors, in x and y, giving box widths per mm
c	            E.g. if pixel units are rad/m/m, then these scale 
c                   factors you have b(4) & b(5) rad/m/m per mm
c
c-----------------------------------------------------------------------
      implicit none
c
      integer boxinc(2), iplot, npixx, npixy, nimage(npixx,npixy)
      double precision cdelt(*)
      real boxfac, image(npixx,npixy), tr(6), scale(2), bfac(5)
cc
      real xb(4), yb(4), x, y, x1, x2, y1, y2, vx1, vx2, vy1, vy2
      integer i, j
      real delx, dely, s
c-----------------------------------------------------------------------
      call pgqwin (x1, x2, y1, y2)
      call pgqvp (2, vx1, vx2, vy1, vy2)
c
c Find maximum selected pixel from first sub-plot
c
      if (iplot.eq.1) then
        bfac(1) = -1.0e30
        do j = 1, npixy, boxinc(2)
          do i = 1, npixx, boxinc(1)
            if (nimage(i,j).gt.0) bfac(1) = max(bfac(1),abs(image(i,j)))
          end do
        end do
c
c Make maximum box width on the plot equal to 99% of the selected
c pixel increment, multiplied by the users factor. 
c
         bfac(2) = boxfac * 0.99*boxinc(1) * abs(tr(2)/bfac(1))
         bfac(3) = boxfac * 0.99*boxinc(2) * abs(tr(6)/bfac(1))
c
c Find out the scale in world coordinates per mm in x and y and then
c work out the scale of the boxes in image units per mm 
c (rad/m/m per mm) in x and y
c
         s = tr(2) * scale(1) / cdelt(1)
         bfac(4) = abs(s / bfac(2))
c
         s = tr(6) * scale(2) / cdelt(2)
         bfac(5) = abs(s / bfac(3))
      end if
c
c Loop over image
c
      do j = 1, npixy, boxinc(2)
        do i = 1, npixx, boxinc(1)
          if (nimage(i,j).gt.0) then
c
c World coordinates of pixel
c
            x = tr(1) + tr(2)*i 
            y = tr(4) + tr(6)*j
c
c Find half width of box in world coordinates.
c
            delx =  bfac(2) * image(i,j) / 2.0
            dely =  bfac(3) * image(i,j) / 2.0
c
c Draw it. Solid boxes for positive values, hollow for negative
c 
            xb(1) = x - delx
            xb(2) = x + delx
            xb(3) = xb(2)
            xb(4) = xb(1)
            yb(1) = y - dely
            yb(2) = yb(1)
            yb(3) = y + dely
            yb(4) = yb(3)
c
            call pgsfs (2)
            if (image(i,j).gt.0) call pgsfs (1)
            call pgpoly (4, xb, yb) 
          end if
        end do
      end do
      call pgsfs (2)
c
      end
c
c
      subroutine drawvec (iplot, tr, vecfac, vecinc, npixx, npixy, amp,
     +   namp, pa, npa, cdelt, scale, signs, rot90, vfac, nx, ny)
c-----------------------------------------------------------------------
c     Draw vectors.   The vector position angle must come out
c     correctly on the plot regardless of the x and y scales,
c     it is not tied to the world coordinate scales.
c
c  Input
c    iplot    sub-plot number.  Only work out vector length scale
c             factors from first sub-plot so that the correct number
c             get written into the plot annotation at the end
c             of each page
c    tr       Transformation matrix from pixels to world coordinates
c    vecfac   Multiply amplitudes by this factor after self-scaling
c    vecinc   Increment through image in these steps
c    npixx,y  Size of images in pixles
c    amp,pa   Amplitude and position angle images
c    namp,npa Normalization images
c    cdelt    Array of pixel increments
c    scale    Scale in linear coords per mm.  For RA axes radians on
c             the sky per mm
c    signs    True means increasing X and Y = E and N, else
c             E and N to the left and top
c    rot90    Add 90 if true to position angle
c    nx,ny    Number of subplots in x and y directions
c  Input/output:
c    vfac     Maximum vector amplitude and the vector scale in
c             pixel units per mm (e.g. Jy/beam per mm).  Set on first
c             sub-plot
c
c-----------------------------------------------------------------------
      implicit none
c
      logical rot90, signs
      integer vecinc(2), nx, ny, iplot, npixx, npixy, namp(npixx,npixy),
     +  npa(npixx,npixy)
      double precision cdelt(*)
      real vecfac, amp(npixx,npixy), pa(npixx,npixy), tr(6), scale(2), 
     +  vfac(2)
cc
      include 'mirconst.h'
      real xv(2), yv(2), x, y
      integer i, j, pas
      real delx, dely, theta, sx, sy, x1, x2, y1, y2, vsizmax
      double precision dr
      parameter (dr = dpi / 180.0)
c-----------------------------------------------------------------------
c
c Find maximum selected vector amplitude for first sub-plot
c
      if (iplot.eq.1) then
        vfac(1) = -1.0e30
        do j = 1, npixy, vecinc(2)
          do i = 1, npixx, vecinc(1)
            if (namp(i,j).gt.0 .and. npa(i,j).gt.0) 
     +         vfac(1) = max(vfac(1), abs(amp(i,j)))
          end do
        end do
c
c Make maximum amplitude on the plot 1/20 of min(width,height)
c of the plot, multipled by the users factor.  Scale in mm.
c
        call pgqvsz (2, x1, x2, y1, y2)
        vsizmax = min((x2-x1)/nx, (y2-y1)/ny) / 20.0
        vfac(2) = abs(vfac(1) / vecfac / vsizmax)
      end if
c
c Convert scale in linear coords per mm to world coords per mm
c
      sx = tr(2) * abs(scale(1) / cdelt(1)) 
      sy = tr(6) * abs(scale(2) / cdelt(2))
c
c Which way are N and E ?   N up and E left same as N down E right
c N down E left same as N up E right as vectors have no arrow head.
c
      pas = 1
      if (signs .and. cdelt(1)*cdelt(2).gt.0.0) pas = -1
c
c Loop over image
c
      do j = 1, npixy, vecinc(2)
        do i = 1, npixx, vecinc(1)
          if (namp(i,j).gt.0 .and. npa(i,j).gt.0 .and.
     +        amp(i,j).gt.0.0) then
c
c World coordinates of pixel
c
            x = tr(1) + tr(2)*i 
            y = tr(4) + tr(6)*j
c
c Position angle of vectors in radians
c
            theta = pas * pa(i,j) * dr
            if (rot90) theta = theta + pi/2.0
c
c Find half size of vectors in world coordinates.
c
            delx = -amp(i,j) * sin(theta) * sx / 2.0 / vfac(2)
            dely =  amp(i,j) * cos(theta) * sy / 2.0 / vfac(2)
c
c Draw it
c 
            xv(1) = x - delx
            xv(2) = x + delx
            yv(1) = y - dely
            yv(2) = y + dely
            call pgline (2, xv, yv) 
          end if
        end do
      end do
c
      end
c
c
      subroutine fullann (ncon, cin, gin, vin, bin, lc, lg, lv, lb,
     +   maxlev, nlevs, levs, srtlev, slev, npixr, trfun, pixr, vfac, 
     +   bfac, naxis, size, crval, crpix, cdelt, ctype, vymin, blc, 
     +   trc, pcs, ydispb, ibin, jbin, kbin, labtyp, gmm, cmm)
c-----------------------------------------------------------------------
c     Full annotation of plot with contour levels, RA and DEC etc.
c
c     Input
c       ncon       Number of contour images
c       *in        Image names
c       l*         Handles for images
c       nlevs      Number of contour levels for each image
c       levs       Contour levels for each image
c       srtlev     Index array gvbing order of increasing contour levels
c       slev       Contour level scale factors for each image
c       trfun      Transfer function applied to image
c       npixr      Number of pixel map ranges
c       pixr       pixel map intensity range
c       vfac       Maximum vector amplitude and scale in mm/amp
c       bfac       Maximum box width and scale in mm/width
c       naxis      Number of axes 
c       size       Size of axes
c       crval      Array of image reference values
c       crpix      Array of reference pixels
c       cdelt      Array of pixel increments
c       ctype      Array of axis types
c       vymin      y viewsurface normalized device coordinate
c                  at which the lowest sub-plot x-axis is drawn
c       blc,trc    Image window in pixels
c       pcs        PGPLOT character size parameters for plot labels
c       ydispb     Displacement of x-axis label in character heights
c       i,jbin     Spatial inc and bin
c       kbin       CHannel increments and average
c       labtyp     Axis label types
c       *mm        Image min and max
c----------------------------------------------------------------------- 
      implicit none
c
      integer maxlev, ncon, nlevs(*), blc(*), trc(*), lc(*), lg, lv(2),
     +  lb, srtlev(maxlev,*), ibin(2), jbin(2), kbin(2), naxis, 
     +  size(naxis), npixr
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      real levs(maxlev,*), vymin, slev(*), pixr(2), pcs, ydispb, 
     +  vfac(2), bfac(5), gmm(2), cmm(2,*)
      character*(*) cin(*), gin, vin(2), bin, ctype(naxis), trfun, 
     +  labtyp(2)
cc
      real xpos, ypos, yinc
      integer i
c-----------------------------------------------------------------------
c
c Setup chores and and annotate with reference values
c
      call anninicg (.false., naxis, crpix, crval, cdelt, ctype, vymin,
     +               pcs, ydispb, labtyp, xpos, ypos, yinc)
c
c Write spatial window in pixels and channel inc. if possible
c
      call annwincg (blc, trc, ibin, jbin, kbin, naxis, size, cdelt,
     +               ctype, yinc, xpos, ypos)
c
c Write imaging information
c
      if (gin.ne.' ') call anngrscg (lg, gin, npixr, pixr, trfun, gmm,
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
c Write vector information
c
      if (vin(1).ne.' ' .and. vin(2).ne.' ') 
     +   call annveccg (lv, vin, vfac, yinc, xpos, ypos)
c
c Write box information
c
      if (bin.ne.' ') 
     +   call annboxcg (lb, bin, bfac, yinc, xpos, ypos)
c
      end
c
c
      subroutine getbeam (maxcon, cin, lc, gin, lg, vin, lv, bin, lb,
     +   labtyp, naxis, crval, crpix, cdelt, ctype, bmin, bmaj, bpa, 
     +   bxfac, byfac, dobeam, bemprs)
c-----------------------------------------------------------------------
c     Get beam information from headers
c
c  Input
c   maxcon     Maximum number of allowed contour images
c   *in        Image names
c   l*         Image handles
c   labtyp     Label types
c   naxis      NUmber of axes in image
c   crval      Axis reference values
c   cdelt      Axis increments
c  Output
c   bmin       FWHMin for maxcon contours, pixel map, two vectors
c              and box images
c   bmaj       FWHMax
c   bpa        p.a. 
c              These are all in radians
c   bx,yfac    Factors to scale the x and y axes so that the beam
c              comes out plotted in world coordinates of the type
c              labelling the plot
c   bemprs     True if beam present for that image
c Input/output:
c  dobeam      If no beams to plot, this is set to false
c-----------------------------------------------------------------------
      implicit none
c
      integer maxcon, naxis
      character*(*) labtyp(2), ctype(naxis), cin(maxcon), gin, vin(2), 
     +  bin
      logical dobeam, bemprs(maxcon+4)
      integer lc(maxcon), lg, lv(2), lb
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
      real bmin(maxcon+4), bmaj(maxcon+4), bpa(maxcon+4), 
     +  bxfac(maxcon+4), byfac(maxcon+4)
cc
      integer i
c-----------------------------------------------------------------------
c
c Contour images
c
      do i = 1, maxcon
       if (lc(i).ne.0) then
         call beamfac (cin(i), lc(i), labtyp, naxis, crval, crpix, 
     +      cdelt, ctype, bmin(i), bmaj(i), bpa(i), bxfac(i), 
     +      byfac(i), bemprs(i))
       else
         bemprs(i) = .false.
       end if
      end do
c
c Pixel map image
c
      i = maxcon + 1
      bemprs(i) = .false.
      if (lg.ne.0) call beamfac (gin, lg, labtyp, naxis, crval, crpix,
     +  cdelt, ctype, bmin(i),  bmaj(i), bpa(i), bxfac(i), 
     +  byfac(i), bemprs(i))
c
c Vector images
c
      i = maxcon + 2
      bemprs(i) = .false.
      bemprs(i+1) = .false.
      if (lv(1).ne.0 .and. lv(2).ne.0) then
        call beamfac (vin(1), lv(1), labtyp, naxis, crval, crpix, 
     +     cdelt, ctype, bmin(i), bmaj(i), bpa(i), bxfac(i), 
     +     byfac(i), bemprs(i))
c
        i = i + 1
        call beamfac (vin(2), lv(2), labtyp, naxis, crval, crpix, 
     +     cdelt, ctype, bmin(i), bmaj(i), bpa(i), bxfac(i), 
     +     byfac(i), bemprs(i))
      end if
c
c Box image 
c
      i = maxcon + 4
      bemprs(i) = .false.
      if (lb.ne.0) call beamfac (bin, lb, labtyp, naxis, crval, crpix,
     +  cdelt, ctype, bmin(i),  bmaj(i), bpa(i), bxfac(i), 
     +  byfac(i), bemprs(i))
c
      dobeam = .false.
      do i = 1, maxcon+4
        if (bemprs(i)) dobeam = .true.
      end do
      if (.not.dobeam) call bug ('w', 'No beam(s) to plot')
c
      end
c
c
      subroutine inputs (maxgr, maxlev, maxcon, maxtyp, ltypes, ncon, 
     +   cin, gin, nvec, vin, bin, mskin, ibin, jbin, kbin, levtyp, 
     +   slev, levs, nlevs, npixr, pixr, trfun, coltab, vecfac, vecinc, 
     +   boxfac, boxinc, pdev, labtyp, dofull, do3val, do3pix, eqscale, 
     +   gaps, solneg, nx, ny, lwid, break, cs, scale, ofile, dobeam, 
     +   beaml, beamb, relax, rot90, signs, mirror, dowedge, doerase, 
     +   doepoch, dofid, dosing)
c-----------------------------------------------------------------------
c     Get the unfortunate user's long list of inputs
c
c  Input:
c   maxgr      Maximum number of pixel map scale intensity ranges and 
c              transfer functions allowed.  The user can input one group 
c              per sub-plot up to this maximum so that differnt subplots 
c              can be displayed optimally.   If there are more subplots
c              that intebsity ranegs given, the extra ones use the values
c              for the previous subplot.
c   maxlev     Maximum number of allowed contour levels
c   maxcon     Maximum number of contour images
c   maxtyp     Maximum number of label types
c   ltypes     Possible label types
c  Output:
c   ncon       Number of contour images
c   nvec       Number of pairs of vector images, 0 or 1
c   c,g,v,b,msk-in 
c              Contour, pixel map, vector (amp & pa), box & mask image names
c   i,j,kbin   X, y and z pixel increment and average
c   levtyp     Type of contour levels scale factors for each contour
c              image:  'p'(ercentage) or 'a'(bsolute)
c   slev       Contour levels scale factors (absolute or percentage)
c              for each contour image
c   levs       Contour levels for each contour image.   Will be scaled 
c              by SLEV for contouring
c   nlevs      Number of contour levels for each contour image
c   npixr      Number of pixr/trfun groups returned.
c   pixr       Pixel map intensity range for each of the NPIXR subplot
c   trfun      Type of pixel map transfer function: 'log', 'lin',
c              'heq' or 'sqr' for each of the NPIXR subplots
c   coltab     COlour lookup table (1 -> 8)
c   vecfac     Vector amplitude scale factor and
c   vecinc     Vector x,y pixel incrememts
c   boxfac     Box width scale factor and
c   boxinc     Box x,y pixel incrememts
c   pdev       PGPLOT plot device/type
c   labtyp     Type of labels for x and y axes
c   dofull     True means do full annotaiton of plot
c   do3val     True means label sub-plots with value of third axis
c   do3pix     True means label sub-plots with pixel of third axis
c   doerase    Erase rectabngle into which 3-axis label written
c   eqscale    True means plot with x and y scales
c   gaps       True menas put space bewteen adjacent sub-plots
c   solneg     True means plot negative contours with solid line
c              style and positive contours with dashed line style
c              One for each contour image
c   nx,ny      Number of sub-plots per page
c   lwid       PGPLOT line widths 
c   break      Level for break between solid and dashed contours
c              for each contour image
c   cs         PGPLOT character sizes for the plot axis labels, the
c	       velocity/channel label, and the overlay ID string
c   scale      Scales for plot in x and y directions ( per mm)
c   ofile      Overlay box/star file name
c   dobeam     Draw the a little beam on each sub-plot
c   beaml      True if beam on left of sub-plot, else right
c   beamb      True if beam at bottom of sub-plot, else top
c   relax      Only issue warnings instead of fatal eror when
c              axis descriptors don;t agree between images
c   rot90      Rotate vectors by a further 90 degrees
c   signs      WHen plotting vectors, assume N and E are in
c              the direction of increasing X and Y
c   mirror     Multiply contours by -1 and add to list
c   dowedge    Draw a wedge on the pixel map
c   doepoch    Write epoch into axis labels
c   dofid      Interactive fiddle
c   dosing     Fiddle after each subplot
c-----------------------------------------------------------------------
      implicit none
c
      integer maxlev, maxcon, maxtyp, maxgr, ncon, nvec, npixr
      real levs(maxlev,maxcon), pixr(2,maxgr), scale(2), cs(3),
     +  slev(maxcon), break(maxcon), vecfac, boxfac
      integer nx, ny, nlevs(maxcon), lwid(maxcon+3), vecinc(2), 
     +  boxinc(2), ibin(2), jbin(2), kbin(2), coltab(maxgr)
      character*(*) labtyp(2), cin(maxcon), gin, vin(2), bin, mskin,
     +  pdev, ofile, trfun(maxgr), levtyp(maxcon), ltypes(maxtyp)
      logical do3val, do3pix, dofull, gaps, eqscale, solneg(maxcon),
     +  dobeam, beaml, beamb, relax, rot90, signs, mirror, dowedge,
     +  doerase, doepoch, dofid, dosing
cc
      integer nmaxim
      parameter (nmaxim = 8)
c
      integer nim, nimtype, i, j, nlab
      character images(nmaxim)*64, imtype(nmaxim)*9
      character*1 str, itoaf
      logical beambl, beambr, beamtl, beamtr, present, keyprsnt
c
      integer ntype
      parameter (ntype = 7)
      character type(ntype)*9
      data type  /'contour', 'pixel', 'amplitude', 'angle', 
     +            'box', 'mask', 'grey'/
c-----------------------------------------------------------------------
      call keyini
c
c Sort out input images
c
      call mkeyf ('in', images, nmaxim, nim)
      if (nim.eq.0) call bug ('f', 'No images given')
      call keymatch ('type', ntype, type, nmaxim, imtype, nimtype)
c
      ncon = 0
      nvec = 0
      do i = 1, nim
c
c Default is "pixel" if one image, else "contour"
c
        if (imtype(i).eq.' ') then
          if (nim.eq.1) then
            imtype(i) = 'pixel'
          else
            imtype(i) = 'contour'
          end if
        end if
c
c Find user given type of image
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
        else if (imtype(i).eq.'amplitude') then
          if (vin(1).ne.' ') then
            call bug ('f', 'More than one vector amplitude image given')
          else
            vin(1) = images(i)
          end if
        else if (imtype(i).eq.'angle') then
          if (vin(2).ne.' ') then
            call bug ('f', 
     +         'More than one vector position angle image given')
          else
            vin(2) = images(i)
            nvec = 1
          end if
        else if (imtype(i).eq.'box') then
          if (bin.ne.' ') then
            call bug ('f', 'More than one box image given')
          else
            bin = images(i)
          end if
        else if (imtype(i).eq.'mask') then
          if (mskin.ne.' ') then
            call bug ('f', 'More than one mask image given')
          else
            mskin = images(i)
          end if
        else
          call bug ('f', 'Unrecognized image type')
        end if
      end do
c
      if ( (vin(1).ne.' ' .and. vin(2).eq.' ') .or.
     +     (vin(1).eq.' ' .and. vin(2).ne.' ') ) call bug ('f', 
     +   'You must give both vector amplitude & position angle images')
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
      call keyi ('chan', kbin(1), 1) 
      call keyi ('chan', kbin(2), 1) 
      kbin(1) = max(kbin(1), 1)
      kbin(2) = max(kbin(2), 1)
      if (kbin(2).gt.kbin(1)) kbin(2) = kbin(1)
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
c Get pixel map ranges and transfer functions for each subplot
c
      pixr(1,1) = 0.0
      pixr(2,1) = 0.0
      trfun(1) = 'lin'
      coltab(1) = 1
      present = keyprsnt ('range')
      i = 0
c
      do while (present .and. i.lt.maxgr)
        present = keyprsnt ('range')
        if (present) then
c
c Get new group; just first value present is enough
c
          i = i + 1
          call keyr ('range', pixr(1,i), 0.0)
          call keyr ('range', pixr(2,i), 0.0)
          call keya ('range', trfun(i), 'lin')
          call lcase (trfun)
          call keyi ('range', coltab(i), 1)
c
          if (coltab(i).lt.-8 .or. coltab(i).gt.8 .or.
     +        coltab(i).eq.0) then
            coltab(i) = 1
            call bug ('w',
     +        'Unrecognized lookup table, setting b&w')
          end if
          if (gin.ne.' ' .and. trfun(i).ne.'lin' .and. 
     +        trfun(i).ne.'log' .and. trfun(i).ne.'heq' .and.
     +        trfun(i).ne.'sqr') then
            call bug ('w',
     +        'Unrecognized image transfer function, setting linear')
            trfun(i) = 'lin'
          end if
        end if
      end do
      npixr = max(i,1)
c
      call keyr ('vecfac', vecfac, 1.0)
      if (vecfac.le.0.0) vecfac = 1.0      
      call keyi ('vecfac', vecinc(1), 2)
      if (vecinc(1).le.0) vecinc(1) = 2
      call keyi ('vecfac', vecinc(2), vecinc(1))
      if (vecinc(2).le.0) vecinc(2) = 2
c
      call keyr ('boxfac', boxfac, 1.0)
      if (boxfac.le.0.0) boxfac = 1.0
      call keyi ('boxfac', boxinc(1), 2)
      if (boxinc(1).le.0) boxinc(1) = 2
      call keyi ('boxfac', boxinc(2), boxinc(1))
      if (boxinc(2).eq.0) boxinc(2) = 2
c
      call keya ('device', pdev, ' ')
c
      call decopt (dofull, do3val, do3pix, eqscale, gaps, solneg,
     +   beambl, beambr, beamtl, beamtr, relax, rot90, signs, 
     +   mirror, dowedge, doerase, doepoch, dofid, dosing)
c
      if (gin.eq.' ') then
        dowedge = .false.
        dofid = .false.
      end if
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
      if ( (index(labtyp(1),'lin').ne.0  .and. 
     +      index(labtyp(2),'lin').eq.0) .or.
     +     (index(labtyp(2),'lin').ne.0  .and. 
     +      index(labtyp(1),'lin').eq.0) .or.
     +     (index(labtyp(1),'kms').ne.0  .and. 
     +      index(labtyp(2),'kms').eq.0) .or.
     +     (index(labtyp(2),'ghz').ne.0  .and. 
     +      index(labtyp(1),'ghz').eq.0) ) then
        if (eqscale) call bug ('i', 
     +  'You might consider options=unequal with these axis LABTYPs')
      end if
c
      if (vin(1).ne.' ' .and. vin(2).ne.' ') then
        if (signs) then
          call output 
     +       ('Assuming E & N in the direction of increasing X & Y')
        else
          call output ('Assuming E & N to the left and top')
        end if
      end if
c
      dobeam = beambl .or. beambr .or. beamtl .or. beamtr
      if (beambl) then
        beamb = .true.
        beaml = .true.
      else if (beamtl) then
        beamb = .false.
        beaml = .true.
      else if (beambr) then
        beamb = .true.
        beaml = .false.
      else if (beamtr) then
        beamb = .false.
        beaml = .false.
      end if
c
      call keyf ('olay', ofile, ' ')
      if (ofile.ne.' ' .and. (labtyp(1).eq.'none' .or. 
     +    labtyp(2).eq.'none')) call bug ('f', 
     +    'Overlays not allowed with labtyp=none')
c
      call keyi ('nxy', nx, 0)
      call keyi ('nxy', ny, nx)
c
      call keyi ('lines', lwid(1), 1)
      if (lwid(1).le.0) lwid(1) = 1
c
      j = 2
      if (ncon.gt.0) then
        do i = 1, ncon
          call keyi ('lines', lwid(j), 1)
          if (lwid(j).le.0) lwid(j) = 1
          call keyr ('break', break(i), 0.0)
          j = j + 1
        end do
      end if
      if (vin(1).ne.' ') then
        call keyi ('lines', lwid(j), 1)
        j = j + 1
      end if
      if (ofile.ne.' ') call keyi ('lines', lwid(j), 1)
c
      call keyr ('csize', cs(1), 0.0)
      call keyr ('csize', cs(2), 0.0)
      call keyr ('csize', cs(3), 0.0)
c
      call keyr ('scale', scale(1), 0.0)
      call keyr ('scale', scale(2), scale(1))
      if (scale(1).lt.0.0) scale(1) = 0.0
      if (scale(2).lt.0.0) scale(2) = 0.0
c
      end
c
c
      subroutine mmini (maxcon, gmm, cmm)
c-----------------------------------------------------------------------
      implicit none
      integer maxcon
      real gmm(2), cmm(2,maxcon)
cc
      integer i
c-----------------------------------------------------------------------
      gmm(1) =  1.0e30
      gmm(2) = -1.0e30
      do i = 1, maxcon
        cmm(1,i) =  1.0e30
        cmm(2,i) = -1.0e30
      end do
c
      end
c
c
      subroutine olaydec (lun, pl1, npl, maxtyp, ltypes, maxpos, lpos,
     +                    ofile, ofig, npos, opos, posid, owrite)
c-----------------------------------------------------------------------
c     Read overlay positions list file and decode.  The positions in
c     true world coordinates are converted to absolute image pixels
c     taking into account what value of the third axis we have. This
c     is because cdelt is frequency dependent in Miriad so a source
c     moves with frequency through a cube.
c
c   Inputs
c     lun      Handle of image
c     pl1,npl  Start chan & number of chans displayed for this subplot
c     maxtyp   Maximum number of label types
c     ltypes   Possible label types
c     maxpos   Maximum number of allowed overlays
c     lpos     Handle for overlay positions list file
c     ofile    Overlay file name
c  Outputs
c     ofig     Overlay figure type for each overlay
c     npos     Number of overlays
c     opos     Array containing overlay locations
c     posid    Array containing overlay I.D. strings
c     owrite   Write overlay ID on overlay ?
c------------------------------------------------------------------------
      implicit none
c
      integer lpos, maxpos, maxtyp, npos, lun, pl1, npl
      double precision opos(6,maxpos)
      character ltypes(maxtyp)*(*), ofile*(*), posid(maxpos)*(*), 
     +  ofig(maxpos)*(*)
      logical owrite(maxpos)
cc
      integer iostat, ilen
      character aline*100
c
      integer len1
      character itoaf*4
      double precision xoff, yoff, pix3
c------------------------------------------------------------------------
      if (ofile.eq.' ') then
        npos = 0
      else
        call txtopen (lpos, ofile, 'old', iostat)
        if (iostat.ne.0) call bug ('f', 'Error opening positions file')
        call initco (lun)
c
c Read and decode locations.  # means comment
c
        xoff = 0.0
        yoff = 0.0
        npos = 0
        iostat = 0
        pix3 = dble(2*pl1+npl-1) / 2.0
c
        do while (iostat.ne.-1)
          aline = ' '
          call txtread (lpos, aline, ilen, iostat) 
          if (iostat.eq.0) then
            if (aline(1:1).ne.'#' .and. aline.ne.' ') then
              if (index(aline,'OFFSET').ne.0 .or. 
     +            index(aline,'offset').ne.0) then
c
c Fish out offset to be applied to all succeeding overlay locations
c
                call posdec1 (aline, xoff, yoff)
              else
c
c Fish out overlay location and type
c
                if (npos.eq.maxpos) then
                  call bug ('w', 'Reducing no. overlays to max. '//
     +                           'allowed = '//itoaf(maxpos))
                  iostat = -1
                else
                  npos = npos + 1
                  ilen = len1(aline)
                  call posdec2 (lun, pix3, maxtyp, ltypes, npos, 
     +              xoff, yoff, aline(1:ilen), ofig(npos), 
     +              opos(1,npos), posid(npos), owrite(npos))
                end if
              end if
            end if
          else
            if (iostat.ne.-1) call bug ('f', 
     +         'Error reading from overlay file')
          end if
        end do
c
        call finco (lun)
        call txtclose (lpos)
      end if
c
      end
c
c
      subroutine ols2pix (lun, otype, widthx, widthy, pos, widthp)
c-----------------------------------------------------------------------
c     Convert overlay widths into pixels.
c
c  Input
c   lun    Handle of image
c   otype  The overlay units type for this axis
c   widthx,y
c          The overlay widths in units specified by OTYPE
c   pos(1:)The centre of the overlay in absolute pixels
c  Output
c   widthp The overlay widths in pixels
c
c------------------------------------------------------------------------
      implicit none
      integer lun
      character*6 otype(2)
      double precision pos(2), widthx, widthy, widthp(2)
cc
      double precision win(2), wout(2), pixin(2)
      integer i
      character*6 typei(2), typeo(2)
c------------------------------------------------------------------------
c
c Convert centre of overlay to true offset coordinates of
c the type indicated by the OTYPE
c
      do i = 1, 2
        typei(i) = 'abspix'
        pixin(i) = pos(i)
        win(i) = pixin(i)
c
        if (otype(i).eq.'hms' .or. otype(i).eq.'dms' .or.
     +      otype(i).eq.'arcsec') then 
          typeo(i) = 'arcsec'
        else
          typeo(i) = 'rel'//otype(i)(4:6)
        end if
      end do
      call w2wco (lun, 2, typei, ' ', win, typeo, ' ', wout)
c
c Now add the overlay width to the offset coordinate and convert
c to absolute pixels. For circles we just have a radius so we
c only make the conversions for the x axis
c
      win(1) = wout(1) + widthx
      win(2) = wout(2) + widthy
      call w2wco (lun, 2, typeo, ' ', win, typei, ' ', wout)
c
c Subtract the centre of the overlay to get the overlay width in pixels.  
c
      do i = 1, 2
        widthp(i) = abs(wout(i) - pixin(i))
      end do
c
      end
c
c
      subroutine overid (doerase, ofig, x, y, xl, xr, yb, yt, str, 
     +                   csize)
c----------------------------------------------------------------------
c     Write the overlay identification string on the overlay
c
c   Input
c     doerase   True to erase background before writing string
c     ofig      Type of overlay; star, box, clear, line, 
c               circle, and ocircle
c               ID written in corner for star and box
c                             centre for clear, ocircle
c			      right  for circle and line
c     x,y       Centre of overlay in world coordinates
c     xl,xr     X left and right world coordinate of the overlay
c     yb,yt     Y bottom and top world coordinate of the overlay
c     str       Overlay identification string
c     csize     User supplied character size
c
c----------------------------------------------------------------------
      implicit none
c
      real xl, xr, yb, yt, csize, x, y
      character*(*) str, ofig
      logical doerase
cc
      real vpx1, vpx2, vpy1, vpy2, vsx1, vsx2, vsy1, vsy2, wx1, wx2, 
     +  wy1, wy2, xfr, yfr, mx, my, dx, dy, xbox(4), ybox(4), 
     +  dx2, dy2, just
      integer il
c
      integer len1
c----------------------------------------------------------------------
c
c Enquire about plot device characteristics; window in world
c coordinates, view-port in ndcs and view-surface in ndcs
c
      call pgqwin (wx1, wx2, wy1, wy2)
      call pgqvp (0, vpx1, vpx2, vpy1, vpy2)
      call pgqvsz (0, vsx1, vsx2, vsy1, vsy2)
c 
c Find the fraction of the view-surface taken up by the overlay
c        
      if (ofig.eq.'clear'.or.ofig.eq.'line') then
c
c No overlay size in these cases.  Use arbitrary fraction.
c
        xfr = 1.0 / 15.0
        yfr = xfr
      else
        xfr = abs((vpx2-vpx1) / (vsx2-vsx1) * (xr - xl) / (wx2-wx1))
        yfr = abs((vpy2-vpy1) / (vsy2-vsy1) * (yt - yb) / (wy2-wy1))
      end if
c
c Set character size so that it is 1/6 of the overlay size
c until it gets too big or small, or use value given by user
c
      if (csize.le.0.0) then
        csize = 40.0 * min(xfr,yfr) / 6.0
        csize = max(0.25, min(csize,20.0))
      end if
      il = len1(str)
      call pgsch (csize)
c
c Find widths of overlay ID string bounding box and overlay
c
      call pgqtxt (0.0, 0.0, 0.0, 0.0, str(1:il), xbox, ybox)
      dx = xbox(4) - xbox(1) 
      dy = ybox(2) - ybox(1)
      dx2 = xr - xl
      dy2 = yt - yb
c
      if (ofig.eq.'clear' .or. ofig.eq.'ocircle') then
c
c Write ID in centre of overlay; pgtext puts BLC of
c character string at (mx,my)
c
        mx = x
        my = y - dy/2.0 - ybox(1)
        just = 0.5
      else if (ofig.eq.'circle') then
c
c Write ID to side of overlay
c
        mx = xr + dx + dx2/50.0
        my = y - dy/2.0 - ybox(1)
        just = 1.0
      else if (ofig.eq.'line') then
c
c Write ID to side of overlay
c
        mx = xr + dx
        my = yt - dy
        just = 0.0
      else if (ofig.eq.'box' .or. ofig.eq.'star') then
c
c Write ID in top right corner of overlay
c
        mx = xr - xbox(4) - dx2/50.0
        my = yt - ybox(2) - dy2/50.0
        just = 0.0
      end if
c
c Optionally erase rectangle and write string
c
      call strerscg (doerase, just, str(1:il), mx, my)
c
      end
c
c
      subroutine overl (doerase, ofig, ow, blc, trc, npos, opos, posid,
     +   chan, csize, labtyp, naxis, crval, crpix, cdelt, ctype)
c--------------------------------------------------------------------------
c     Draw overlays
c
c     Input
c       doerase  Erase rectangle before writing overlay ID string
c       ofig     'star', 'box', 'clear, 'circle', 'line'
c       ow       If true write overlay ID in corner of overlay
c       blc      Blc of image being plotted in pixels
c       trc      Trc of image being plotted in pixels
c       npos     Number of overlays
c       opos     List of: 
c                       X  Y  XS YS CS CE      'box', 'star'
c                       X1 Y1 X2 Y2 CS CE      'line' 
c                       X Y ... ... CS CE      'clear'  (3&4 unused)
c                       X Y  S  ... CS CE      'circle', 'ocircle'
c                                               (4 unused)
c                All locations and half sizes are in unbinned
c		 full image pixels now except for circle overlays
c		 where the half size (radius) is in pixels according
c		 to the X axis increment.
c       posid    List of overlay I.D.'s
c       chan     Current plane being plotted
c       csize    Character size for overlay ID
c       labtyp   Axis label types
c       naxis    Number of axes
c       c*       Axis descriptors
c----------------------------------------------------------------------
      implicit none
c     
      integer npos, chan, blc(*), trc(*), naxis
      double precision opos(6,npos), crval(naxis), cdelt(naxis), 
     +  crpix(naxis)
      real csize
      character posid(npos)*(*), ofig(npos)*(*), ctype(naxis)*(*),
     +  labtyp(2)*(*)
      logical ow(npos), doerase
cc
      include 'mirconst.h'
      double precision d2r
      parameter (d2r = dpi/180.0)
c
      logical miss, ok
      character line*80
      integer i, j, k, cs, ce
      double precision x, y, xl, xr, yb, yt
      real xcirc(0:360), ycirc(0:360), rat, radx, rady
c----------------------------------------------------------------------
c
c Loop over overlays
c
      call pgsci (3)
      do i = 1, npos
c
c Only draw on specified channels
c
        cs = nint(opos(5,i))
        ce = nint(opos(6,i))
        if (cs.eq.0) ce = 0
        if (ce.eq.0) ce = cs
c
        if (cs.eq.0 .or. (chan.ge.cs .and. chan.le.ce)) then
c
c Extrema in world coordinates
c 
          if (ofig(i).eq.'line') then
            call pix2wcg (.true.,  dble(opos(1,i)), 1, labtyp(1), naxis,
     +                    crval, crpix, cdelt, ctype, xl, ok)
            call pix2wcg (.true.,  dble(opos(2,i)), 2, labtyp(2), naxis, 
     +                    crval, crpix, cdelt, ctype, yb, ok)
            call pix2wcg (.true.,  dble(opos(3,i)), 1, labtyp(1), naxis, 
     +                    crval, crpix, cdelt, ctype, xr, ok)
            call pix2wcg (.true.,  dble(opos(4,i)), 2, labtyp(2), naxis, 
     +                    crval, crpix, cdelt, ctype, yt, ok)
          else if (ofig(i).eq.'star' .or. ofig(i).eq.'box') then
            call pix2wcg (.true.,  dble(opos(1,i)-opos(3,i)), 1, 
     +         labtyp(1), naxis, crval, crpix, cdelt, ctype, xl, ok)
            call pix2wcg (.true.,  dble(opos(2,i)-opos(4,i)), 2, 
     +         labtyp(2), naxis, crval, crpix, cdelt, ctype, yb, ok)
            call pix2wcg (.true.,  dble(opos(1,i)+opos(3,i)), 1, 
     +         labtyp(1), naxis, crval, crpix, cdelt, ctype, xr, ok)
            call pix2wcg (.true.,  dble(opos(2,i)+opos(4,i)), 2, 
     +         labtyp(2), naxis, crval, crpix, cdelt, ctype, yt, ok)
          else if (ofig(i).eq.'circle' .or. ofig(i).eq.'ocircle') then
c
c Remember circle radius in x-axis pixels at this point
c
            rat = abs(cdelt(1) / cdelt(2))
            call pix2wcg (.true.,  dble(opos(1,i)-opos(3,i)), 1, 
     +         labtyp(1), naxis, crval, crpix, cdelt, ctype, xl, ok)
            call pix2wcg (.true.,  dble(opos(2,i)-rat*opos(3,i)), 2,
     +         labtyp(2), naxis, crval, crpix, cdelt, ctype, yb, ok)
            call pix2wcg (.true.,  dble(opos(1,i)+opos(3,i)), 1, 
     +         labtyp(1), naxis, crval, crpix, cdelt, ctype, xr, ok)
            call pix2wcg (.true.,  dble(opos(2,i)+rat*opos(3,i)), 2, 
     +         labtyp(2), naxis, crval, crpix, cdelt, ctype, yt, ok)
c
            radx = (xr - xl) / 2.0
            rady = (yt - yb) / 2.0
          end if
c
c Centre of overlay in world coordinates
c
          call pix2wcg (.true.,  dble(opos(1,i)), 1, labtyp(1), naxis,
     +                  crval, crpix, cdelt, ctype, x, ok)
          call pix2wcg (.true.,  dble(opos(2,i)), 2, labtyp(2), naxis,
     +                  crval, crpix, cdelt, ctype, y, ok)
c
c Draw desired type of overlay
c
          miss = .true.
          if (ofig(i).eq.'star') then
            if (opos(1,i).ge.blc(1).and.opos(1,i).le.trc(1).and.
     +          opos(2,i).ge.blc(2).and.opos(2,i).le.trc(2)) 
     +          miss = .false.
            call pgmove (real(x), real(yb))
            call pgdraw (real(x), real(yt))
            call pgmove (real(xl), real(y))
            call pgdraw (real(xr), real(y))
          else if (ofig(i).eq.'box') then
            if (opos(1,i).ge.blc(1).and.opos(1,i).le.trc(1).and.
     +          opos(2,i).ge.blc(2).and.opos(2,i).le.trc(2)) 
     +          miss = .false.
            call pgmove (real(xl), real(yb))
            call pgdraw (real(xr), real(yb))
            call pgdraw (real(xr), real(yt))
            call pgdraw (real(xl), real(yt))
            call pgdraw (real(xl), real(yb))
          else if (ofig(i).eq.'line') then
            if ((opos(1,i).ge.blc(1).and.opos(1,i).le.trc(1).and.
     +            opos(2,i).ge.blc(2).and.opos(2,i).le.trc(2)) .or.
     +           (opos(3,i).ge.blc(1).and.opos(3,i).le.trc(1).and.
     +            opos(4,i).ge.blc(2).and.opos(4,i).le.trc(2)))
     +          miss = .false.
            call pgmove (real(xl), real(yb))
            call pgdraw (real(xr), real(yt))
          else if (ofig(i).eq.'circle' .or. ofig(i).eq.'ocircle') then
            if (opos(1,i).ge.blc(1).and.opos(1,i).le.trc(1).and.
     +          opos(2,i).ge.blc(2).and.opos(2,i).le.trc(2)) 
     +          miss = .false.
c
c Generate poly-line coordinates
c
            k = 0
            do j = 0, 360
              xcirc(k) = radx*cos(real(j)*d2r) + x
              ycirc(k) = rady*sin(real(j)*d2r) + y
              k = k + 1
            end do
c
c Draw poly-line and fill if necessary
c
            call pgsfs (2)
            if (ofig(i).eq.'circle') call pgsfs (1)
            call pgpoly (k-1, xcirc(0), ycirc(0))
            call pgsfs (2)
          else if (ofig(i).eq.'clear') then
c
c Allow clear overlays anywhere
c
            miss = .false.
          end if
c
          if (miss) then
            write (line,100) i
100         format ('Overlay # ', i4, 
     +              ' does not fully fit on the image')
            call output (line)
          end if
c
c Write overlay identifying number
c
          if (ow(i)) call overid (doerase, ofig(i), real(x), real(y), 
     +                  real(xl), real(xr), real(yb), real(yt), 
     +                  posid(i), csize)
        end if
      end do
      call pgsci (1)
c
      end
c
c
      subroutine posdec1 (aline, xoff, yoff)
c---------------------------------------------------------------------
c     Decode OFFSET string into offsets
c
c     Input
c       aline    Input string
c     Output
c       x,yoff   Offsets
c
c---------------------------------------------------------------------
      implicit none
c
      double precision xoff, yoff
      character*(*) aline
cc 
      integer maxnum
      parameter (maxnum = 10)
c
      double precision nums(maxnum)
      integer lena, ipres, idx, icomm(maxnum)
      logical ok
c--------------------------------------------------------------------
c
c Find end of OFFSET string and start of numbers
c
      idx = index(aline,'OFFSET')
      if (idx.eq.0) idx = index(aline,'offset')
      if (idx.eq.0) call bug ('f', 
     +   'Error finding OFFSET in overlay offset line')
      idx = idx + 6
c
      call strprpcg (maxnum, aline(idx:), icomm, ipres, lena)
      if (ipres.lt.2) call bug ('f', 
     +   'There are insufficient fields for overlay offset line')
      lena = lena + idx - 1
c
c Now extract the numeric part of the line which remains
c
      call matodf (aline(idx:lena), nums, ipres, ok)
      if (.not.ok) then
        call bug ('f', 'Error decoding overlay offset line')
      else
        xoff = nums(1)
        yoff = nums(2)
      end if
c
      end
c
c
      subroutine posdec2 (lun, pix3, maxtyp, ltypes, iline, xoff, yoff,
     +                    aline, ofig, opos, posid, owrite)
c---------------------------------------------------------------------
c     Decode string into positions list
c
c     Input
c       lun      Handle of image
c       pix3     Pixel of third axis for subplot currently 
c                being displayed
c       maxtyp   Maximum number of axis types
c       ltypes   possible label types
c       iline    Line number being decoded
c       x,yoff   Offsets to add to decoded locations
c       aline    Input string
c     Output
c       ofig     Overlay type (star, box, clear, line)
c       opos     Overlay location, list of: 
c                         1  2   3    4   5  6
c			  --------------------
c                         X  Y  XSIZ YSIZ CS CE   'box', 'star'
c                         X1 Y1 X2   Y2   CS CE   'line'
c                         X  Y  ...  ...  CS CE   'clear' (3&4 unused)
c                         X  Y   S   ...  CS CE   'circle' and 'ocircle'
c                                                  (4 unused)
c                All locations and half sizes are in unbinned full
c		 image pixels now except for circle overlays. In this
c		 case, S is in pixels according to the X axis increment.
c       posid    Overlay ID string
c       owrite   True to write overlay ID on plot
c
c---------------------------------------------------------------------
      implicit none
c
      integer iline, maxtyp, lun
      double precision opos(6), xoff, yoff, pix3
      character*(*) aline, posid, ofig, ltypes(maxtyp)
      logical owrite
cc 
      include 'mirconst.h'
      double precision rd
      integer maxnum
      parameter (rd = 180.0/dpi, maxnum = 20)
c
      double precision nums(maxnum), off(2)
      integer i, j, slen, lena, inum, ipres, nextra, npt, ifac, emax,
     +  icomm(maxnum), dsign(2), spos, nuse
      logical ok
      character str*4, estr*80, wover*3, otype(2)*6
c
      integer len1
      character itoaf*4
c
      integer ntype1, ntype2
      parameter (ntype1 = 6, ntype2 = 2)
      character type1(ntype1)*7, type2(ntype2)*3
      data type1 /'box', 'star', 'line', 'clear', 'circle', 'ocircle'/
      data type2 /'yes', 'no'/
c----------------------------------------------------------------------
c
c Prepare string for matodf
c
      str = itoaf(iline)
      slen = len1(str)
      call strprpcg (maxnum, aline, icomm, ipres, lena)
      if (ipres.lt.7) then
        estr = 'There are insufficient fields for overlay # '//
     +          str(1:slen)
        call bug ('f', estr)
      end if
c
c Fish out OFIG, XOTYPE, YOTYPE, ID, WRITE
c
      ofig = aline(1:icomm(1)-1)
      call matchcg (iline, 'OFIG', ofig, 'overlay', ntype1, type1)
c
      otype(1) = aline(icomm(1)+1:icomm(2)-1)
      call matchcg (iline, 'XOTYPE', otype(1), 'overlay', 
     +               maxtyp, ltypes)
      otype(2) = aline(icomm(2)+1:icomm(3)-1)
      call matchcg (iline, 'YOTYPE', otype(2), 'overlay', 
     +               maxtyp, ltypes)
c
      posid = aline(icomm(3)+1:icomm(4)-1)
c
      wover = aline(icomm(4)+1:icomm(5)-1)
      call matchcg (iline, 'WRITE', wover, 'overlay', ntype2, type2)
      call ucase (wover)
      owrite = .true.
      if (wover.eq.'NO') owrite = .false.
      ipres = ipres - 5
c
c How many numbers do we expect in string.  Minimum is:
c  X  Y          for 'box' and  'star'  XS YS CS CE optional
c  X1,Y1 X2,Y2   for 'line' CS CE optional
c  X  Y          for 'clear' CS CE optional
c  X  Y S        for 'circle' and 'ocircle' CS CE optional
c
      inum = 0
      if (ofig.eq.'circle' .or. ofig.eq.'ocircle') inum = 1
      ifac = 1
      if (ofig.eq.'line') ifac = 2
      do j = 1, 2
        if (otype(j).eq.'hms' .or. otype(j).eq.'dms') then
          inum = inum + ifac*3
        else
          inum = inum + ifac*1
        end if
      end do
c
      if (ipres.lt.inum) then
        estr = 'Insufficient numbers for overlay # '//str(1:slen)
        call bug ('f', estr)
      end if
c
c Find DEC sign.  Could be on either axis
c
      dsign(1) = 1
      dsign(2) = 1
      if (otype(1).eq.'dms') then
        spos = 5
        if (aline(icomm(spos)+1:icomm(spos)+1).eq.'-') dsign(1) = -1
      end if
      if (otype(2).eq.'dms') then
        if (otype(1).eq.'hms') then
          spos = 8
        else
          spos = 6
        end if
        if (aline(icomm(spos)+1:icomm(spos)+1).eq.'-') dsign(2) = -1
      end if
c
c Now extract the numeric part of the line which remains
c
      call matodf (aline(icomm(5)+1:lena), nums, ipres, ok)
      if (.not.ok) then
        estr = 'Error decoding overlay # '//str(1:slen)
        call bug ('f', estr)
      end if
c
c Now convert the overlay locations in whatever unit to pixels
c
      off(1) = xoff
      off(2) = yoff
      call ol2pixcg (lun, pix3, ofig, otype, off, dsign, nums, 
     +               opos, nuse)
      npt = nuse + 1
c
c For circles we must fish out the mandatory radius too; we convert 
c it to X axis pixels
c
      if (ofig.eq.'circle' .or. ofig.eq.'ocircle') then
        call ols2pix (lun, otype, nums(npt), 0.0d0, opos(1), opos(3))
        opos(4) = 0.0
        npt = npt + 1
      end if
c
c We have done the mandatory columns, now deal with the optional
c  for 'box' and  'star'  XS YS CS CE optional
c  for 'line'                   CS CE optional
c  for 'clear'                  CS CE optional
c  for 'circle' and 'ocircle'   CS CE optional
c
      nextra = ipres - inum
      emax = 2
      if (ofig.eq.'box' .or. ofig.eq.'star') emax = 4
      if (nextra.gt.emax) call bug ('f', 
     +   'Too many numbers for overlay # '//str(1:slen))
c
      if (ofig.eq.'line' .or. ofig.eq.'clear') then
        if (nextra.eq.0) then
          opos(5) = 0.0
          opos(6) = 0.0
        else if (nextra.eq.1) then
          opos(5) = nums(npt)
          opos(6) = opos(5)
        else if (nextra.eq.2) then
          opos(5) = nums(npt)
          opos(6) = nums(npt+1)
        end if
      else if (ofig.eq.'box' .or. ofig.eq.'star') then
        do i = 3, 6
          opos(i) = 0.0
        end do
        if (nextra.eq.0) then
          opos(3) = 2.0
          opos(4) = 2.0
        else if (nextra.eq.1) then
          call ols2pix (lun, otype, nums(npt), 0.0d0, opos(1), opos(3))
          opos(4) = opos(3)
        else
          call ols2pix (lun, otype, nums(npt), nums(npt+1), 
     +                  opos(1), opos(3))
          if (nextra.eq.3) then
            opos(5) = nums(npt+2)
            opos(6) = opos(5)
          else if (nextra.eq.4) then
            opos(5) = nums(npt+2)
            opos(6) = nums(npt+3)
          end if
        end if
      else if (ofig.eq.'circle' .or. ofig.eq.'ocircle') then
        if (nextra.eq.0) then
          opos(5) = 0.0
          opos(6) = 0.0
        else if (nextra.eq.1) then
          opos(5) = nums(npt)
          opos(6) = opos(5)
        else if (nextra.eq.2) then
          opos(5) = nums(npt)
          opos(6) = nums(npt+1)
        end if
      end if
c
      end
c
c
      subroutine region (maxcon, maxnax, ncon, cin, gin, vin, bin, 
     +   lc, lg, lv, lb, cnaxis, gnaxis, vnaxis, bnaxis, csize, gsize, 
     +   vsize, bsize, ccrval, gcrval, vcrval, bcrval, ccdelt, gcdelt, 
     +   vcdelt, bcdelt, ccrpix, gcrpix, vcrpix, bcrpix, cctype, gctype,
     +   vctype, bctype, cepoch, gepoch, vepoch, bepoch, naxis, size, 
     +   crval, cdelt, crpix, ctype, epoch, ibin, jbin, kbin, blc, trc,
     +   win, maxgrp, grpbeg, ngrp, ngrps, lhead)
c----------------------------------------------------------------------
c     Finish key routie inputs for region of interest now.  Have to 
c     delay until here because of complexity added by mixed 2-D/3-D
c     capability.   The BOXINPUT routine must be associated with the 
c     file that, if any, has three dimensions.    Return also the
c     axis descriptors for all further positional use.  
c
c  Input:
c    maxcon        Maximum number of contour images allowed
c    maxnax        Maximum number of allowed dimenions for image
c    ncon          Number of contour images
c    *in           Image names
c    l*            Handles
c    *naxis        Number of dimensions for images
c    *size         Sizes of images
c    *crpix        Array of image reference pixels
c    *cdelt        Array of image increments 
c    *crval        Array of image reference values 
c    *ctype        Array of image axis types
c    *epoch        Array of epochs
c    i,j,kbin      x,y, and z pixel increment and binning sizes
c    maxgrp        Maximum number of allowed groups
c  Output:
c                  The following axis descriptors are used for all 
c                  subsequent axis information.  They come from whatever 
c                  cube we encounter first or the first 2-D image in 
c                  contour, pixel map, vector order.  
c    naxis         Number of axes
c    size          SIze of axes
c    crpix         Array of image reference pixels
c    cdelt         Array of image increments 
c    crval         Array of image reference values 
c    ctype         Array of image axis types
c    epoch         EPoch
c    blc,trc       3-D Hyper-rectangle surrounding region of interest
c                  in unbinned pixels
c    win           Size of BINNED region of interest for x and y directions
c    grgbeg        List of start planes for each group of channels
c                  that are to be avearged together for each sub-plot
c                  A new group is begun at every interruption to the
c                  continuity of the selected channels, or if the
c                  channel increment is reached.
c    ngrp          Number of channels in each group of channel to
c                  be averaged together for each sub-plot.
c    ngrps         Number of groups of channels.
c    lhead         This is a handle to be used by the COCVT coordinate 
c                  conversion routines (via the COSUBS interface).
c
c----------------------------------------------------------------------
      implicit none
c     
      integer maxcon, ncon, maxnax, naxis, cnaxis(maxcon), gnaxis, 
     +  vnaxis, bnaxis, size(maxnax), csize(maxnax,maxcon), 
     +  gsize(maxnax), vsize(maxnax), bsize(maxnax), blc(*), trc(*), 
     +  win(2), maxgrp, ngrp(maxgrp), grpbeg(maxgrp), ngrps, ibin(2), 
     +  jbin(2), kbin(2), lhead, lc(maxcon), lg, lv, lb
      double precision cdelt(maxnax), crval(maxnax), crpix(maxnax),
     +  ccdelt(maxnax,*), ccrval(maxnax,*), ccrpix(maxnax,*),
     +  gcdelt(maxnax),   gcrval(maxnax),   gcrpix(maxnax),
     +  bcdelt(maxnax),   bcrval(maxnax),   bcrpix(maxnax),
     +  vcdelt(maxnax),   vcrval(maxnax),   vcrpix(maxnax)
      real epoch, cepoch(*), gepoch, vepoch, bepoch
      character*(*) cin(maxcon), gin, vin, bin, ctype(maxnax),
     +  cctype(maxnax,*), gctype(maxnax), vctype(maxnax),
     +  bctype(maxnax)
cc
      include 'maxdim.h'
      integer maxbox
      parameter (maxbox = 1024)
c
      integer boxes(maxbox), i
c----------------------------------------------------------------------
c
c Use the first cube we find to set the rest of the box inputs.
c
      size(3) = 0
      if (ncon.gt.0) then
        do i = 1, ncon
          if (csize(3,i).gt.1 .and. size(3).eq.0) then
            call boxinput ('region', cin(i), boxes, maxbox)
            call boxset (boxes, cnaxis(i), csize(1,i), ' ')
            call setdescg (cnaxis(i), csize(1,i), ccrval(1,i), 
     +         ccdelt(1,i), ccrpix(1,i), cctype(1,i), cepoch(i),
     +         naxis, size, crval, cdelt, crpix, ctype, epoch)
            lhead = lc(i)
          end if
        end do
      end if
c
      if (gin.ne.' ' .and. size(3).eq.0) then
        if (gsize(3).gt.1) then
          call boxinput ('region', gin, boxes, maxbox)
          call boxset (boxes, gnaxis, gsize, ' ')
          call setdescg (gnaxis, gsize, gcrval, gcdelt, gcrpix, gctype,
     +       gepoch, naxis, size, crval, cdelt, crpix, ctype, epoch)
          lhead = lg
        end if
      end if
c
      if (vin.ne.' ' .and. size(3).eq.0) then
        if (vsize(3).gt.1) then
          call boxinput ('region', vin, boxes, maxbox)
          call boxset (boxes, vnaxis, vsize, ' ')
          call setdescg (vnaxis, vsize, vcrval, vcdelt, vcrpix, vctype,
     +       vepoch, naxis, size, crval, cdelt, crpix, ctype, epoch)
          lhead = lv
        end if
      end if
c
      if (bin.ne.' ' .and. size(3).eq.0) then
        if (bsize(3).gt.1) then
          call boxinput ('region', bin, boxes, maxbox)
          call boxset (boxes, bnaxis, bsize, ' ')
          call setdescg (bnaxis, bsize, bcrval, bcdelt, bcrpix, bctype,
     +       bepoch, naxis, size, crval, cdelt, crpix, ctype, epoch)
          lhead = lb
        end if
      end if
c
c If we didn't encounter a cube, then use any of the open
c 2-D images for the box routines.  They have all been
c checked for identical first and second dimensions.
c
      if (size(3).eq.0) then
        if (ncon.gt.0) then
          call boxinput ('region', cin, boxes, maxbox)
          call boxset (boxes, cnaxis, csize, ' ')
          call setdescg (cnaxis, csize, ccrval, ccdelt, ccrpix, cctype, 
     +       cepoch, naxis, size, crval, cdelt, crpix, ctype, epoch)
          lhead = lc(1)
        else if (gin.ne.' ') then
          call boxinput ('region', gin, boxes, maxbox)
          call boxset (boxes, gnaxis, gsize, ' ')
          call setdescg (gnaxis, gsize, gcrval, gcdelt, gcrpix, gctype,
     +       gepoch, naxis, size, crval, cdelt, crpix, ctype, epoch)
          lhead = lg
        else if (vin.ne.' ') then
          call boxinput ('region', vin, boxes, maxbox)
          call boxset (boxes, vnaxis, vsize, ' ')
          call setdescg (vnaxis, vsize, vcrval, vcdelt, vcrpix, vctype, 
     +       vepoch, naxis, size, crval, cdelt, crpix, ctype, epoch)
          lhead = lv
        else if (bin.ne.' ') then
          call boxinput ('region', bin, boxes, maxbox)
          call boxset (boxes, bnaxis, bsize, ' ')
          call setdescg (bnaxis, bsize, bcrval, bcdelt, bcrpix, bctype,
     +       bepoch, naxis, size, crval, cdelt, crpix, ctype, epoch)
          lhead = lb
        else
          call bug ('f', 'Internal logic error in REGION')
        end if
      end if
      call keyfin
c
c Find hyper-rectangle surrounding region of interest from highest 
c dimension image involved (i.e., 2-D/3-D).
c
      call boxinfo (boxes, 3, blc, trc)
      do i = 1, naxis
        blc(i) = max(1,blc(i))
        trc(i) = min(size(i),trc(i))
      end do
c
c Adjust spatial window to fit an integral number of bins and
c find size of binned window
c
      call winfidcg (size(1), 1, ibin, blc(1), trc(1), win(1))
      call winfidcg (size(2), 2, jbin, blc(2), trc(2), win(2))
c      if (win(1).le.1 .or. win(2).le.1) call bug ('f',
c     +   'Cannot display just one spatial pixel')
c
c Find list of start channels and number of channels for each group
c of channels selected.  The BOX routines do not easily, if at all,
c allow us to deal with multiple BOXes at once (say if there were
c two differently masked cubes being plotted), so we don't AND
c in the flagging mask.
c
      call chnselcg (blc, trc, kbin, maxbox, boxes, maxgrp,
     +               grpbeg, ngrp, ngrps)
c
      end
c
c
      subroutine sesame (relax, maxdim, maxnax, maxcon, ncon, cin, lc,
     +  cnaxis, csize, cepoch, maskc, ccrpix, ccdelt, ccrval, cctype, 
     +  gin, lg, gnaxis, gsize, gepoch, maskg, gcrpix, gcdelt, gcrval, 
     +  gctype, vin, lv, vnaxis, vsize, vepoch, maskv, vcrpix, vcdelt, 
     +  vcrval, vctype, bin, lb, bnaxis, bsize, bepoch, maskb, bcrpix, 
     +  bcdelt, bcrval, bctype, mskin, lm, mnaxis, msize, mepoch, maskm,
     +  mcrpix, mcdelt, mcrval, mctype, gmm, cmm)
c-----------------------------------------------------------------------
c  Open all required images, check their self consistency and
c  return their axis descriptors and handles
c
c  Input
c   relax     Warning only on axis inconsistencies, else fatal
c   maxdim    Max allowed size of axes
c   maxnax    Max allowed number of dimensions
c   maxcon    Max allowed number of contour images
c   ncon      Number of contour images
c   *in       Image names
c  Output
c   l*        Handles
c   *naxis    Number of axes
c   *size     Size of axes
c   *epoch    Epochs
c   mask*     Masks present ?
c   *crpix    Reference pixels
c   *crval    Reference values
c   *cdelt    Axis increments
c   *ctype    Axis types
c   *mm       Data min and max for each image initialized to +/-1e30
c   
c-----------------------------------------------------------------------
      implicit none
      integer maxdim, maxnax, maxcon, ncon, csize(maxnax,maxcon), 
     +  gsize(maxnax), vsize(maxnax,2), msize(maxnax), bsize(maxnax), 
     +  cnaxis(maxcon), gnaxis, vnaxis(2), mnaxis, bnaxis, lc(maxcon), 
     +  lg, lv(2), lm, lb
      real cepoch(maxcon), gepoch, vepoch(2), mepoch, bepoch,
     +  gmm(2), cmm(2,maxcon)
      double precision 
     +  ccdelt(maxnax,maxcon), ccrval(maxnax,maxcon), 
     +  gcdelt(maxnax), gcrval(maxnax), 
     +  vcdelt(maxnax,2), vcrval(maxnax,2),  
     +  mcdelt(maxnax), mcrval(maxnax),
     +  bcdelt(maxnax), bcrval(maxnax),
     +  ccrpix(maxnax,maxcon), gcrpix(maxnax), 
     +  vcrpix(maxnax,2), mcrpix(maxnax), bcrpix(maxnax)
      logical maskc(maxcon), maskg, maskv(2), maskm, maskb, relax
      character*(*) cin(maxcon), gin, vin(2), mskin, bin
      character*9 cctype(maxnax,maxcon), gctype(maxnax),
     +  vctype(maxnax,2), mctype(maxnax), bctype(maxnax)
cc
      integer i
c-----------------------------------------------------------------------
c
c Initialize data min and max
c
      call mmini (maxcon, gmm, cmm)
c
c
c Open contour images as required 
c
      if (ncon.gt.0)  then
        do i = 1, ncon
          call opimcg (maxdim, maxnax, cin(i), lc(i), cnaxis(i), 
     +      csize(1,i), cepoch(i), maskc(i), ccrpix(1,i), ccdelt(1,i),
     +      ccrval(1,i), cctype(1,i))
        end do
      end if
c
c Open pixel map image as required
c
      if (gin.ne.' ') then
        call opimcg (maxdim, maxnax, gin, lg, gnaxis, gsize, gepoch,
     +     maskg, gcrpix, gcdelt, gcrval, gctype)
      end if
c
c Open vector images as required
c
      if (vin(1).ne.' ' .and. vin(2).ne.' ') then
        do i = 1, 2
          call opimcg (maxdim, maxnax, vin(i), lv(i), vnaxis(i), 
     +      vsize(1,i), vepoch(i), maskv(i), vcrpix(1,i), vcdelt(1,i),
     +      vcrval(1,i), vctype(1,i))
        end do
      end if
c
c Open box image as required
c
      if (bin.ne.' ') then
        call opimcg (maxdim, maxnax, bin, lb, bnaxis, bsize, bepoch,
     +     maskb, bcrpix, bcdelt, bcrval, bctype)
      end if
c
c Open mask image as required
c
      if (mskin.ne.' ') then
        call opimcg (maxdim, maxnax, mskin, lm, mnaxis, msize, mepoch, 
     +     maskm, mcrpix, mcdelt, mcrval, mctype)
        if (.not.maskm)  then
          call bug ('w', 'The mask image does not have a mask')
          call xyclose (lm)
          mskin = ' '
        end if
      end if
c
c Check consistency of input images
c
      call chkim  (maxnax, ncon, cin, csize, cepoch, ccrpix, ccdelt, 
     +   ccrval, cctype, gin, gsize, gepoch, gcrpix, gcdelt, gcrval, 
     +   gctype, vin, vsize, vepoch, vcrpix, vcdelt, vcrval, vctype, 
     +   bin, bsize, bepoch, bcrpix, bcdelt, bcrval, bctype, mskin, 
     +   msize, mepoch, mcrpix, mcdelt, mcrval, mctype, relax)
c
      end
