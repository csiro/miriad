      program cgdisp

c= CGDISP - displays and overlays images on a PGPLOT device
c& nebk
c: plotting
c+
c       CGDISP displays/overlays images via contour plots, pixel map
c       representations, vectors and scaled boxes on a PGPLOT device.
c       Upto 3 contour plots, one pixel map, one vector plot and one box
c       display may be overlaid in multi-panel plots of multi-channel
c       images.  In addition overlay locations (plotted as crosses,
c       boxes, circles, lines or see-through) may be specified from an
c       ascii text file.
c
c       Manipulation of the device colour lookup table is available
c       when you display with a pixel map representation (formerly
c       called a "grey scale")
c
c@ in
c       You may input up to seven images.  Up to three of these can
c       be displayed via contour plots and one can be displayed via a
c       colour pixel map representation.  One vector amplitude image and
c       one vector position angle image (degrees; positive N -> E) can
c       together be used to display a vector map (e.g. polarization
c       vectors).  One image can be displayed as small scaled boxes (see
c       below) and one image may be used as a mask.
c
c       The "box" image is displayed by drawing little boxes (solid and
c       hollow for positive and negative pixels) at the location of each
c       selected pixel.  The size of the box scales with the value of
c       the pixel.  This is a useful way to display rotation measure
c       images for example. The mask image blanking mask is logically
c       ANDed to all the other image masks before they are displayed.
c       The mask image is not displayed.
c
c       If more than one image is specified, they must have identical
c       first and second dimensions.  However, you can overlay
c       combinations of 2-D with 3-D images (e.g. multi-channel images
c       with a continuum image) provided all the 3-D images have the
c       same third dimension.  These images can be input in any order
c       (see TYPE).  Wild card expansion is supported.  No default.
c@ type
c       Specifies the type of each image, respectively, listed in the IN
c       keyword. Minimum match is supported (note that "pixel" was
c       formerly "grey" [which is still supported]).   Choose from:
c
c        "contour"   (contour;            up to 3 of these)
c        "pixel"     (pixel map;          up to 1 of these)
c        "amplitude" (vector amplitude;   up to 1 of these)
c        "angle"     (vector pos'n angle; up to 1 of these)
c        "box"       (box;                up to 1 of these)
c        "mask"      (mask;               up to 1 of these)
c
c       You can't give one of "amplitude" or "angle" without the other.
c       Default is "pixel" for one image, "contour" if more than one.
c@ region
c       Region of interest.  Choose only one spatial region (bounding
c       box only supported), but as many spectral regions (i.e. multiple
c       IMAGE specifications) as you like.   Each channel (or group of
c       channels; see CHAN below) is drawn on a new sub-plot.
c       NOTE: the region specification applies equally to all the
c       input images.
c       Default is full image
c@ xybin
c       Upto 4 values.  These give the spatial increment and binning
c       size in pixels for the x and y axes to be applied to the
c       selected region.  If the binning size is not unity, it must
c       equal the increment.  For example, to bin up the image by 4
c       pixels in the x direction and to pick out every third pixel in
c       the y direction, set XYBIN=4,4,3,1
c       Defaults are 1,XYBIN(1),XYBIN(1),XYBIN(3)
c@ chan
c       2 values. The first is the channel increment to step through the
c       image in, the second is the number of channels to average, for
c       each sub-plot.  Thus CHAN=5,3  would average groups of 3
c       channels together, starting 5 channels apart such as: 1:3, 6:8,
c       11:13 ...  The channels available are those designated by the
c       REGION keyword.  A new group of channels (sub-plot) is started
c       if there is a discontinuity in the REGION selected channels
c       (such as IMAGE(10,20),IMAGE(22,30).  The combination of REGION
c       and CHAN determines how many sub-plots there will be.
c
c       In the case that you have input some combination of 2-D and 3-D
c       images, CHAN refers to the 3-D image(s). Note that a channel
c       is defined to be a pixel on the third axis of a cube, regardless
c       of the cube's order (xyv or vxy say).
c       Defaults are 1,1
c@ slev
c       Up to 3 pairs of values, one for contour image. First value is
c       the type of contour level scale factor.  "p" for percentage and
c       "a" for absolute.   Second value is the factor to scale LEVS by.
c       Thus, SLEV=p,1  would contour levels at LEVS * 1% of the image
c       peak intensity.  Similarly, SLEV=a,1.4e-2 would contour levels
c       at LEVS * 1.4E-2
c       Default is no additional scaling of LEVS (i.e., "a",1.0)
c@ levs1
c       The levels to contour for the first specified contour image are
c       LEVS1 times SLEV (either percentage of the image peak or
c       absolute).
c       Defaults try to choose something vaguely useful.
c@ levs2
c       Levels for the second contour image.
c@ levs3
c       Levels for the third contour image.
c@ cols1
c       PGPLOT colours for LEVS1 contours.  If one value is given it is
c       used for all contours.  PGPLOT colour indices are
c          0: background colour (black or white)
c          1: foreground colour (white or black)
c          2: red           3: green           4: blue
c          5: cyan          6: magenta         7: yellow
c          8: orange        9: lime           10: spring green
c         11: azure        12: violet         13: rose
c         14: dark grey    15: light grey
c@ cols2
c       Colours for the second contour image.  Defaults to those for the
c       first image.
c@ cols3
c       Colours for the third contour image.  Defaults to those for the
c       first image.
c@ range
c       Up to N groups of four values, one group per sub-plot, where N
c       is the maximum number of channels allowed by Miriad (currently
c       32768).  The four values are
c         - MINimum image intensity to display
c         - MAXimum image intensity to display
c         - Transfer function type:
c             lin: linear
c             sqr: square root
c             log: logarithmic
c             heq: histogram equalization
c         - Colour lookup table:
c               1: b&w
c               2: rainbow
c               3: linear pseudo colour
c               4: floating zero colour contours
c               5: fixed zero colour contours
c               6: rgb
c               7: background
c               8: heat
c               9: absolute b&w
c              10-19: cubehelix (by D. Green, google cubehelix for info)
c           Negate the table number to reverse the lookup table.
c
c       The transfer function changes available with OPTIONS=FIDDLE
c       are in addition to (on top of) the selections here, but the
c       colour lookup table selections will replace those selected here.
c
c       All subplots following the last one with a specified "range"
c       will use the "range" settings from the previous subplot. In
c       this way, one group of settings can be applied to all the
c       subplots if desired.  The multiple subplot capability is useful
c       if you have used IMCAT to put unlike images into planes of
c       a cube and you wish to display them together.
c
c       Default is linear between the image minimum and maximum with
c       a b&w lookup table.   You can default the intensity range with
c       zeros, viz. "range=0,0,log,-2" say.
c@ vecfac
c       3 or 4 values.  A scale factor to multiply the vector image
c       lengths (or box image widths) by, the x and y increments (in
c       pixels) across the image at which to plot the vectors (or
c       boxes), and optionally the length of the scale-bar vector
c       (unset for no scale-bar). If you have set non unit values of
c       XYBIN, the increments here refer to the binned pixels.  When
c       VECFAC(1)=1, the vectors (boxes) are scaled so that the maximum
c       amplitude (width) takes 1/20 of the (sub)plot size.
c
c       The scale-bar gives a graphical representation of the vector
c       lengths, which makes vector plots easier to interpret.  The
c       scale-bar is drawn in the corner specified by the BEAMTYP key
c       (defaulting to bottom-left if BEAMTYP is not specified). If
c       VECFAC(4)=0, the scale bar is drawn the length of the longest
c       vector; you can find out what this is using OPTIONS=FULL. For a
c       fractional polarization vector map, setting VECFAC(4)=1
c       corresponds to 100 per cent polarization. If VECFAC(1) >> 1,
c       this will give a very long vector. For polarization intensity
c       images, VECFAC(4) is specified in flux density.
c
c       Defaults are 1.0, 2, VECFAC(2)
c       Default is not to draw a scale-bar.
c@ boxfac
c       3 values.  A scale factor to multiply the box image widths by,
c       and the x and y increments (in pixels) across the image at which
c       to plot the boxes).  If have set non unit values of XYBIN, the
c       increments here refer to the binned pixels.  When BOXFAC(1)=1,
c       the boxes are scaled so that there is a little bit of space
c       between adjacent boxes.
c       Defaults are 1.0, 2, BOXFAC(2)
c@ device
c       The PGPLOT plot device, such as plot.plt/ps
c       No default.
c@ nxy
c       Number of sub-plots in the x and y directions on the page.
c       Defaults choose something depending on your telescope.
c@ labtyp
c       Up to 2 values.  The spatial label type of the x and y axes.
c       Minimum match is active.  Select from:
c
c        "hms"       the label is in H M S.S (e.g. for RA)
c        "dms"       the label is in D M S.S (e.g. for DEC)
c        "arcsec"    the label is in arcsecond offsets
c        "arcmin"    the label is in arcminute offsets
c        "arcmas"    the label is in milli-arcsec offsets
c        "absdeg"    the label is in degrees
c        "reldeg"    the label is in degree offsets
c                    The above assume the pixel increment is in radians.
c        "abspix"    the label is in pixels
c        "relpix"    the label is in pixel offsets
c        "abskms"    the label is in km/s
c        "relkms"    the label is in km/s offsets
c        "absghz"    the label is in GHz
c        "relghz"    the label is in GHz offsets
c        "absnat"    the label is in natural coordinates as defined by
c                    the header.
c        "relnat"    the label is in offset natural coordinates
c        "none"      no label and no numbers or ticks on the axis
c
c       All offsets are from the reference pixel.
c       Defaults are "relpix", LABTYP(1)   except if LABTYP(1)="hms"
c       when LABTYP(2) defaults to "dms" (to give RA and DEC)
c@ beamtyp
c     Up to 6 values. Set if you want a small polygon to be drawn to
c     represent the beam FWHM. Setting beamtyp to "b,l" is sufficient to
c     draw a solid beam; "b,l,4" will result in a cross-hatched
c     beam.  Use 'n' if you don't want a beam at all.
c     The six parameters are:
c
c     - Beam vertical positioning: can be "t" (top), or "b" (bottom). No
c       default.
c     - Beam horizontal positioning: can be "l" (left), or "r"
c       (right). Default "l"
c
c     The next four parameters apply only to the first image specified
c     with the "in" keyword.  If there are multiple, different beams to
c     draw (for example, if different uv data were used to produce
c     images with different beam shapes), all subsequent beams are drawn
c     as open polygons.
c
c     - Hatching style:
c        1    solid (default)
c        2    outline
c        3    hatched
c        4    cross-hatched
c     - Hatching angle (default 45 degrees).
c     - Hatching line separation (default 1).
c     - Line-width for outlines, hatching and cross-hatching (default 1)
c@ options
c       Task enrichment options. Minimum match of all keywords is
c       active.
c
c       "abut" means don't leave any white space between subplots.  The
c         default is to leave a little bit between subplots, and
c         OPTIONS=GAPS leaves a lot of space and labels eacg subplot
c         separately.
c       "beamAB", where "A" is one of "b" or "t" and
c                       "B" is one of "l" or "r"
c         means draw the beam FWHM on the plot in the corner indicated
c         by the "AB" location. This option is deprecated: use the
c         keyword "beamtyp" instead.
c       "blacklab" means that, if the device is white-background, draw
c         the axis labels in black. Default is red.
c       "conlabel" means label the contour values on the actual
c         contours.  The PGPLOT routine that does this is not very
c         bright.  You will probably get too many labels.  If you bin
c         the image up with keyword XYBIN, say, by a factor of 2, you
c         will get about 1/2 as many labels.  If desperate use the
c         overlay facility (keyword OLAY) to manually label contours.
c       "fiddle" means enter a routine to allow you to interactively
c         change the display lookup table.  You can cycle through a
c         variety of colour lookup tables, as well as alter a linear
c         transfer function by the cursor location, or by selecting
c         predefined transfer functions (linear, square root,
c         logarithmic, histogram equalization)
c
c         For hard copy devices (e.g. postscript), a keyboard driven
c         fiddle is offered; you can cycle through different colour
c         tables and invoke the predefined transfer functions, but the
c         linear fiddler is not available.   Note that if you are using
c         "cgdisp" from a script, so that interactive fiddling is not
c         appropriate, you can use the "range" keyword to specify the
c         transfer function and colour lookup tables.
c       "full" means do full plot annotation with contour levels, pixel
c         displa range, file names, reference values, etc.  Otherwise
c         more room for the plot is available.
c       "gaps" means leave large gaps between subplots and individually
c         label the axes of each subplot. By default, the subplots will
c         have a small amount of white space between each subplot and
c         they will only be labelled around the borders of the full
c         page.  See also OPTIONS=ABUT to eliminate the small amount of
c         white space.
c       "grid" means draw a coordinate grid on the plot rather than just
c         ticks
c       "mirror" causes all specified contour levels for all images
c         to be multiplied by -1 and added to the list of contours
c       "nodistort" means that angularly-defined overlays do not distort
c         with the coordinate grid.  If you are displaying a large area
c         of the sky, such that the non-linearities in the coordinate
c         system can be seen, then by default, the overlays (keyword
c         OLAY) will distort with the coordinate grid if you are using
c         angular units for the overlay locations and half sizes.  Thus
c         star overlays will rotate and stretch, circles will distort
c         similarly.  Overlays given in non-angular units will always be
c         undistorted.
c       "noepoch" means don't write the epoch value into the axis labels
c       "noerase" means don't erase a rectangle into which the "3-axis"
c         values and the overlay ID strings are written.
c       "nofirst" means don't write the first x-axis label on any
c         subplots except for the left-most one. This may avoid label
c         overwrite.
c       "corner" means only write labels in the lower left corner of any
c         subplot
c       "relax" means issue warnings when image axis descriptors are
c         inconsistent (e.g. different pixel increments) instead
c         of a fatal error.  Use at your peril.
c       "rot90" rotates vectors by an extra 90 degrees.  Useful
c         to convert E-vectors into B-vectors
c       "signs"  Normally, when plotting vectors, CGDISP assumes that
c         North is up and East to the left.  If OPTIONS=SIGNS, then
c         it assumes that E and N are in the direction of increasing
c         X and Y.
c       "single" means that when you have selected OPTIONS=FIDDLE and
c         you have more than one subplot per page, activate the fiddle
c         option after each subplot rather than the default, which is
c         to fiddle only at the end.  In the latter case, the histogram
c         equalization, if invoked, will have been computed with the
c         image in the last subplot only.
c       "solneg1" means make negative contours solid and positive
c         contours dashed for the first contour image. The default,
c         and usual convention is the reverse.
c       "solneg2" SOLNEG1 for the second contour image.
c       "solneg3" SOLNEG1 for the third contour image.
c       "trlab" means label the top and right axes as well as the bottom
c         and left ones.  This can be useful when non-linear coordinate
c         variation across the field makes the ticks misaligned
c       "unequal" means draw plots with unequal scales in x and y
c         so that the plot surface is maximally filled.  The default
c         is for equal scales in x and y.
c       "wedge" means that if you are drawing a pixel map, also draw
c         and label a wedge to the right of the plot, showing the map
c         of intensity to colour.
c       "3pixel" means label each sub-plot with the pixel value of
c         the third axis.
c       "3value" means label each sub-plot with the appropriate
c         value of the third axis (e.g. velocity or frequency for an
c         xyv ordered cube, position for a vxy ordered cube).
c         Both "3pixel" and "3value" can appear, and both will be
c         written on the plot.  They are the average values when
c         the third axis is binned up with CHAN.  If the third axis
c         is not velocity or frequency, the units type for "3VALUE"
c         will be chosen to be the complement of any like axis in the
c         first 2. E.g. the cube is in vxy order and
c         LABTYP=ABSKMS,ARCSEC the units for the "3VALUE" label will be
c         arcsec.  If LABTYP=ABSKMS,HMS the "3VALUE" label will be DMS
c         (if the third [y] axis is declination).  See also keyword
c         "3format" where you can input the format for the "3value"
c         labelling.
c@ 3format
c       If you ask for "3value" labelling, this keyword allows you
c       specify the FORTRAN format of the labelling.  I have given
c       up trying to invent a decent algorithm to choose this. Examples
c       are "1pe12.6", or "f5.2" etc   If you leave this blank cgdisp
c       will try something that you probably won't like.
c@ lines
c       Up to 6 values.  The line widths for the axes, each contour
c       image (in the order of TYPE), the vector image, and any
c       overlays.  If there are less than 3 contour images or no vector
c       image, the vector image/overlay line widths shift left.
c       Line widths must be integers.
c       Defaults are 1,1,1,1,1,1
c@ break
c       Up to 3 values. The intensity levels for the break between
c       solid and dashed contours for each contour image.
c       Defaults are 0.0,0.0,0.0
c@ csize
c       Up to 4 values.  Character sizes in units of the PGPLOT default
c       (which is ~ 1/40 of the view surface height) for the plot axis
c       labels, the velocity/channel label, the overlay ID string
c       (if option "write" in OLAY used) label, and the contour
c       value labels (see options=conlab).
c       Defaults try to choose something sensible.  Use 0.0 to default
c       any particular value. E.g., 1.4, 0, 0, 0.5
c@ scale
c       Up to 2 values.  Scales in natural axis units/mm with which to
c       plot in the x and y directions.  For example, if the increments
c       per pixel are in radians, then this number would be radians/mm
c       (note that for RA axes you give radians on the sky per mm).
c       Although this choice of unit may be cumbersome, it makes no
c       assumptions about the axis type, so is more flexible.   If you
c       also chose OPTIONS=EQUAL then one of your scales, if you set
c       both and differently, would be over-ruled.  If you give only
c       one value, the second defaults to that.
c       Defaults choose scales to fill the page optimally. To default
c       the first but the second, use 0.0,scale(2)
c@ olay
c       The name of a file containing a list of overlay descriptions.
c       Wild card expansion is active and the default is no overlays.
c
c       Miriad task CGCURS OPTIONS=CURSOR,LOG,CGDISP  can be used to
c       make an overlay file.
c
c       Entries in the overlay file can be white space or comma
c       delimitered or both.  All lines beginning with # are ignored.
c
c                       **** DO NOT USE TABS ****
c
c       Double quotes " are used below to indicate a string.  The "
c       should not be put in the file.   For all the string parameters
c       discussed below, you can abbreviate them with minimum match.
c
c
c       Each line describes an overlay and should be as follows:
c
c        ##### The first 5 parameters in each line must be
c
c         1      2       3     4    5
c        --------------------------------
c        OFIG  XOTYPE  YOTYPE  ID  WRITE
c
c      where
c
c       OFIG is the type of overlay; choose from
c        "sym"     pgplot symbol number (give centre, symbol, and size)
c        "star"    star (i.e. cross; give centre and half-sizes)
c        "box"     box (give centre and half-sizes)
c        "line"    line segment (give ends)
c        "vector"  directed line segment (give centre, length, and
c                  position angle)
c        "circle"  filled in circle (give centre and radius)
c        "ocircle" open circle (give centre and radius)
c        "ellipse" filled-in ellipse (give centre, half axes and p.a.)
c        "oellipse open ellipse (give centre, half axes and p.a.)
c        "clear"   nothing, so you can write the overlay ID string (see
c                  below) without the overlay
c
c       Also
c        "colour"  See below.
c        "lwid"    See below.
c        "offset"  See below.
c
c       XOTYPE and YOTYPE  give the units of the overlay location (and
c       overlay half-sizes) contained in the file for the x- and y-
c       directions, respectively.  Choose from:
c        "hms", "dms", "arcsec", "arcmin", "absdeg", "reldeg", "abspix",
c        "relpix", "absnat", "relnat", "absghz", "relghz",
c        "abskms", & "relkms"  as described in the keyword LABTYP.
c       Note that OTYPE does not depend upon what you specified for
c       LABTYP.
c
c       ID is an identifying overlay string which can be optionally
c       written on the overlay; it MUST be in the overlay file whether
c       you write it on the plot or not).  The ID string is written in
c       the corner for "star" and "box", at the end for "line", and in
c       the centre for "circle" and "clear".  The underscore character
c       "_" is treated a special case and is replaced by a blank before
c       plotting.  In this way, you can write several words as the
c       overlay ID; you connect them with underscores in the overlay
c       file, and cgdisp strips them out before plotting.
c
c       WRITE is "yes" or "no" to specify if the overlay ID is to be
c       written in the overlay figure or not.
c
c
c        ##### Parameters beyond number 5 depend upon OFIG, XOTYPE, and
c        YOTYPE
c
c        6   7    8   9  10  11  12  13  14  15          OFIG
c        --------------------------------------   -----------------
c        X   Y   SY  SS  CS  CE                   sym
c        X   Y   XS  YS  CS  CE                   star, box
c        X1  Y1  X2  Y2  CS  CE                   line
c        X   Y   VL  PA  SS  A1  A1  A3  CS  CE   vector
c        X   Y   R   CS  CE                       circle,  ocircle
c        X   Y   R1  R2  PA  CS  CE               ellipse, oellipse
c        X   Y   CS  CE                           clear
c
c       X,Y defines the center of the overlay in the nominated OTYPE
c       coordinate system (X- and Y-OTYPE can be different).
c       (X1,Y1) & (X2,Y2) are the end points of the line segment in the
c       nominated OTYPE (mixed OTYPEs are supported here too).
c       For %OTYPE = "abspix ", "relpix", "arcsec", "arcmin", "absdeg",
c                    "reldeg", "absghz", "relghz", "abskms", "relkms",
c                    "absnat" & "relnat" X,Y,X1,Y1,X2,Y2 are single
c                    numbers.
c
c       For %OTYPE = "hms" or "dms", the X and/or Y location is/are
c       replaced by three numbers such as HH MM SS.S or DD MM SS.S.
c       Thus, if XOTYPE = hms and YOTYPE = dms then the file for
c       OFIG=box, say, should have lines like
c
c         HH MM SS.S   DD MM SS.S   XS   YS  CHAN
c
c       XS, YS are the overlay half-sizes in the following units:
c       %OTYPE = "abspix" and "relpix" in pixels
c                "hms"    and "dms"    in arcseconds
c                "arcsec"              in arcseconds
c                "arcmin"              in arcminutes
c                "absdeg" and "reldeg" in degrees
c                "absghz" and "relghz" in GHz
c                "abskms" and "relkms" in km/s
c                "absnat" and "relnat" in natural coordinates
c       XS, YS are optional for OFIG="box" and "star".  The defaults
c       are XS = 2, YS = XS pixels.
c
c       CS to CE is the channel range (image planes) on which to put the
c       overlays.  If you specify only CS than the overlay is put on
c       that channel.  If CS = 0 the overlays are put on all channels.
c       In all cases, CS and CE are optional and the default is 0 (all
c       channels)
c
c       SY is the pgplot symbol to use for "sym".
c
c       SS is the pgplot character height to use for "sym" and "vector".
c       May be set to zero for vectors to omit the arrowhead.  Default
c       is the character height used for overlay string.
c
c       VL is the length of the vector in pixels.
c
c       PA is the position angle in degrees, positive N -> E.
c
c       A1, A2, and A3 are the PGPLOT arrowhead style parameters:
c       A1 is the fill-style, 1 (default) for filled or anything else
c          for outline.
c       A2 is the acute angle of the arrow point, in degrees.  Default
c          45.0.
c       A3 is the fraction of the triangular arrowhead that is cut away
c          from the back.  Default 0.3.
c
c       R is the radius of circle overlays in the units given in the
c       above list according to XOTYPE only.
c
c       R1 and R2 are the ellipse major and minor axes half-widths,
c       both in units according to XOTYPE.
c
c
c       ##### OFIG = COLOUR (or COLOR)
c
c       A COLOUR directive can be included at any point in the overlay
c       file in the format
c
c         COLOUR   INDEX
c
c       where the literal "COLOUR" or "COLOR" (without the quotes)
c       starts in column 1, followed by the PGPLOT colour index.  This
c       changes the graphics overlay colour until the next COLOUR
c       directive is processed.  PGPLOT colour indices are listed above
c       for the cols1 parameter.  The default colour index is 9.
c
c       ##### OFIG = LWID
c
c       An LWID directive can be included at any point in the overlay
c       file in the format
c
c         LWID   WIDTH
c
c       where the literal "LWID" (without the quotes) starts in column
c       1, followed by the PGPLOT line width in units of 0.005 inch
c       (about 0.13 mm) and must be an integer in the range 1-201.  This
c       changes the graphics line width until the next LWID directive is
c       processed.  The default width is 1.
c
c       ##### OFIG = OFFSET
c
c       An OFFSET directive can be included at any point in the overlay
c       file in the format
c
c         OFFSET   XOFF   YOFF
c
c       where the literal "OFFSET" (without the quotes) starts in
c       column 1, followed by X and Y offsets which are applied to all
c       succeeding overlay file locations.
c
c              X = X + XOFF;   Y = Y + YOFF
c
c       These offsets must be in the same units as the %OTYPE of
c       succeeding directives.  It is intended so that your overlay
c       locations can be in, say, arcsec relative to some location which
c       is not the reference pixel of the image (which is what CGDISP
c       ultimately wants).   You then specify, with the OFFSET
c       directive, the offsets between the reference pixel of the
c       contour/pixel map images and the actual reference location of
c       your overlay locations.
c
c       You can have as many OFFSET directive as you like in the file.
c       All succeeding directives will apply these offsets until new
c       ones are defined.  If the directive does not appear, naturally
c       no additional offsets are added.
c
c       The OFFSET directive is not applied to ANY position fields in
c       succeeding directives that have %OTYPEs that are "hms" or "dms".
c       I am too lazy to code it.
c
c$Id: cgdisp.for,v 1.24 2013/09/12 23:51:21 wie017 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c    pjt   2013jul24  committed ptrdiff based alloc/free
c    mhw   2013sep13  Add cubehelix colour scheme
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

c     Array sizes.
      integer MAXLEV, MAXCON, MAXTYP, NBINS
      parameter (MAXLEV = 50, MAXCON = 3, MAXTYP = 18, NBINS = 128)

c     Plotting parameters.
      integer NXDEF, NYDEF
      real    WEDWID, TFDISP
      parameter (NXDEF = 4, NYDEF = 4, WEDWID = 0.05, TFDISP = 0.5)

      ptrdiff ipim, ipnim, ipim2, ipnim2, ipimm
      integer csize(maxnax,MAXCON), gsize(maxnax), vsize(maxnax,2),
     *  msize(maxnax), bsize(maxnax), lc(MAXCON), lg, lv(2), lm, lb,
     *  lhead, concol(MAXCON), veccol, boxcol, bemcol, ovrcol, labcol
      logical doaxlab, doaylab, donxlab(2), donylab(2)
      character cin(MAXCON)*64, gin*64, vin(2)*64, mskin*64, bin*64,
     *  ltypes(MAXTYP)*6, versan*72, version*72

      real levs(MAXLEV,MAXCON), pixr(2,maxchan), tr(6), bmin(MAXCON+4),
     *  bmaj(MAXCON+4), bpa(MAXCON+4), scale(2), cs(4), pixr2(2),
     *  slev(MAXCON), break(MAXCON), vfac(2), bfac(5), tfvp(4),
     *  wdgvp(4), cumhis(NBINS), gmm(3), cmm(3,MAXCON), dmm(3), bmm(3)
      real vxmin, vymin, vymax, vx, vy, vxsize, vysize, vxgap, vygap,
     *  ydispb, xdispl, groff, blankg, blankc, blankv, blankb,
     *  vecfac, vecmax, vecmaxpix, boxfac, hs(3)

      integer blc(3), trc(3), win(2), lwid(MAXCON+3), veclwid,
     *  vecinc(2), boxinc(2), srtlev(MAXLEV,MAXCON), nlevs(MAXCON),
     *  grpbeg(maxchan), ngrp(maxchan), his(NBINS), ibin(2),
     *  jbin(2), kbin(2), krng(2), coltab(maxchan), gnaxis,
     *  cnaxis(MAXCON), vnaxis(2), bnaxis, mnaxis, cols(MAXLEV,MAXCON)
      integer  nx, ny, ierr, pgbeg, ilen, igr, nlast, ngrps,
     *  ncon, i, j, nvec, ipage, jj, npixr, wedcod, bgcol,
     *  ncols(MAXCON), jplot, fs, firstimage

      character labtyp(2)*6, levtyp(MAXCON)*1, trfun(maxchan)*3
      character pdev*132, xlabel*40, ylabel*40, hard*20, ofile*64,
     *  aline*72, val3form*20

      logical solneg(MAXCON), doblv(2), bemprs(MAXCON+4)
      logical do3val, do3pix, dofull, gaps, eqscale, doblc, doblg,
     *  dobeam, candobeam, beaml, beamb, relax, rot90, signs, mirror,
     *  dowedge, doerase, doepoch, bdone, doblb, doblm, dofid, dosing,
     *  nofirst, grid, dotr, dodist, conlab, doabut, getvsc, noflab,
     *  blacklab, docorner, donum

      data blankc, blankv, blankb /-99999999.0, -99999999.0,
     *                             -99999999.0/
      data lc, lg, lv, lb, lm /MAXCON*0, 0, 2*0, 0, 0/
      data gin, vin, bin, mskin /' ', 2*' ', ' ', ' '/
      data bdone /.false./
      data ipage /0/
      data ltypes /'hms   ', 'dms   ', 'abspix', 'relpix', 'arcsec',
     *             'arcmin', 'arcmas', 'absghz', 'relghz', 'abskms',
     *             'relkms', 'absnat', 'relnat', 'absdeg', 'reldeg',
     *              'none',  'abslin', 'rellin'/
      data dmm /1e30, -1e30, -1.0/
      data bmm /1e30, -1e30, -1.0/
      data coltab /maxchan*0/
      data lwid /1, MAXCON*1, 1, 1/
      data getvsc /.true./
c-----------------------------------------------------------------------
      version = versan ('cgdisp',
     *                  '$Revision: 1.24 $',
     *                  '$Date: 2013/09/12 23:51:21 $')

c     Get user inputs.
      call inputs(maxchan, MAXLEV, MAXCON, MAXTYP, ltypes, ncon, cin,
     *  gin, nvec, vin, bin, mskin, ibin, jbin, kbin, levtyp, slev,
     *  levs, nlevs, npixr, pixr, trfun, coltab, vecfac, vecmax,
     *  vecinc, boxfac, boxinc, pdev, labtyp, dofull, do3val, do3pix,
     *  eqscale, gaps, solneg, nx, ny, lwid, break, cs, scale,
     *  ofile, dobeam, beaml, beamb, relax, rot90, signs, mirror,
     *  dowedge, doerase, doepoch, dofid, dosing, nofirst, grid, dotr,
     *  dodist, conlab, doabut, docorner, val3form, ncols, cols, fs,
     *  hs, firstimage, blacklab)

c     Open images as required.
      call sesame(relax, maxnax, MAXCON, ncon, cin, lc, csize, cnaxis,
     *  gin, lg, gsize, gnaxis, vin, lv, vsize, vnaxis, bin, lb, bsize,
     *  bnaxis, mskin, lm, msize, mnaxis, cmm, gmm)

c     Finish key inputs for region of interest and return generic
c     axis descriptors.
      call region(MAXCON, maxnax, ncon, cin, gin, vin, bin, lc, lg,
     *   lv, lb, csize, gsize, vsize, bsize, cnaxis, gnaxis, vnaxis,
     *   bnaxis, lhead, ibin, jbin, kbin, blc, trc, win,
     *   ngrps, grpbeg, ngrp)

c     Try to allocate memory for images.
      call memallop(ipim,  win(1)*win(2), 'r')
      call memallop(ipnim, win(1)*win(2), 'i')
      if (vin(1).ne.' ' .and. vin(2).ne.' ') then
        call memallop(ipim2,  win(1)*win(2), 'r')
        call memallop(ipnim2, win(1)*win(2), 'i')
      endif
      if (mskin.ne.' ') call memallop(ipimm,  win(1)*win(2), 'l')

c     Compute contour levels for each contour image.
      if (ncon.gt.0) then
        do i = 1, ncon
          call conlevcg(mirror, MAXLEV, lc(i), levtyp(i), slev(i),
     *                   nlevs(i), levs(1,i), srtlev(1,i))
        enddo
      endif

c     Work out world coordinate limits and PGPLOT transformation matrix.
      call limitscg(blc, ibin, jbin, tr)

c     Get beam information.
      if (dobeam .or. (vecmax.ge.0.0)) then
        call getbeam(MAXCON, cin, lc, gin, lg, vin, lv,
     *       bin, lb, bmin, bmaj, bpa, candobeam, bemprs)
c       User might just be setting beam parameters
c       for the vector scale-bar.
        if (dobeam .and. .not.candobeam) then
          if (vecmax.lt.0.0)  call bug('w', 'No beam(s) to plot')
          dobeam = .false.
        endif
      endif

c     Work out number of plots per page and number of plots.
      call nxnycg(NXDEF, NYDEF, ngrps, nx, ny, nlast)
      npixr = min(ngrps,npixr)

c     Work out default character sizes for axis, 3-value, and
c     contour labels.
      call defchrcg(nx, ny, cs(1))
      write(aline, 100) cs(1), cs(2)
100   format('Character sizes (axes & velocity) are: ', f3.1, ', ',
     *         f3.1)
      call output(aline)
      if (cs(4).eq.0.0) cs(4) = 1.0

c     Open plot device.
      ierr = pgbeg (0, pdev, 1, 1)
      if (ierr.ne.1) then
        call pgldev
        call bug('f', 'Error opening plot device')
      endif
      call pgpage
      call pgscf(2)
      call pgqinf('hardcopy', hard, ilen)

c     Find colour of background.
      call bgcolcg(bgcol)

c     Set colours for line graphics.
      call setlgc(bgcol, labcol, concol, veccol, boxcol,
     *             ovrcol, bemcol, blacklab)

c     Init OFM routines.
      if (gin.ne.' ') call ofmini

c     Work out if wedge outside or inside subplots.  Also work out
c     if plotting one wedge per subplot or one wedge for all.
      call wedgincg(hard, dofid, dowedge, nx, ny, npixr, trfun, wedcod)

c     Set axis labels.
      call setlabcg(lhead, labtyp, doepoch, xlabel, ylabel)

c     Set label displacements from axes.
      call setdspcg(lhead, labtyp, blc, trc, xdispl, ydispb)

c     Work out view port sizes and increments.
      call vpsizcg(dofull, dofid, ncon, gin, vin, 0, bin, MAXLEV,
     *   nlevs, srtlev, levs, slev, nx, ny, cs, xdispl, ydispb,
     *   gaps, doabut, dotr, wedcod, WEDWID, TFDISP, labtyp, vxmin,
     *   vymin, vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)

c     Adjust viewport increments and start locations if equal scales
c     requested or if scales provided by user.
      call vpadjcg(lhead, hard, eqscale, scale, vxmin, vymin, vymax,
     *   nx, ny, blc, trc, tfvp, wdgvp, vxsize, vysize)

c     Find abs max of full image for box display.
      if (bin.ne.' ') then
        do j = 1, ngrps
          if (bsize(3).gt.1) then
            krng(1) = grpbeg(j)
            krng(2) = ngrp(j)
          else
            krng(1) = 1
            krng(2) = 1
          endif

          call readimcg(.true., blankb, lb, ibin, jbin, krng, blc,
     *      trc, .true., memi(ipnim), memr(ipim), doblb, bmm)
        enddo
        bfac(1) = bmm(3)
      endif

c     Set viewport location of first sub-plot.
      vx = vxmin
      vy = vymax - vysize

c     Loop over number of sub-plots.
      do j = 1, ngrps
        if (mod(j,nx*ny).eq.1 .or. nx*ny.eq.1) ipage = ipage + 1
        jj = j - (ipage-1)*nx*ny
        if (hard.eq.'YES') then
          write(aline, '(a,i3)') 'Beginning plane ', grpbeg(j)
          call output(aline)
        endif

c       Set viewport and window for current sub-plot.
        call pgsvp(vx, vx+vxsize, vy, vy+vysize)
        call pgswin(blc(1)-0.5, trc(1)+0.5, blc(2)-0.5, trc(2)+0.5)
        call pgsch(cs(1))

c       Read in mask image as required.
        if (mskin.ne.' ') then
          if (msize(3).le.1) then
            krng(1) = 1
            krng(2) = 1
          else
            krng(1) = grpbeg(j)
            krng(2) = ngrp(j)
          endif
          if (.not.bdone) then
            call readbcg(.true., lm, ibin, jbin, krng, blc, trc,
     *                    meml(ipimm), doblm)
            bdone = .true.
          endif
        endif

c       Deal with pixel map image.
        if (gin.ne.' ') then
          if (gsize(3).le.1) then
            krng(1) = 1
            krng(2) = 1
          else
            krng(1) = grpbeg(j)
            krng(2) = ngrp(j)
          endif

c         Apply transfer function to pixel range.  Apply the last
c         transfer function given if there aren't enough.
          igr = min(j,npixr)
          call grfixcg(pixr(1,igr), lg, gnaxis, gsize, trfun(igr),
     *                 pixr2, groff, blankg)

c         Read pixel map image and apply mask.
          call readimcg(.true., blankg, lg, ibin, jbin, krng,
     *        blc, trc, .true., memi(ipnim), memr(ipim), doblg, gmm)
          if (mskin.ne.' ' .and. doblm) then
            call maskorcg(blankg, win, meml(ipimm), memi(ipnim),
     *                    memr(ipim))
            doblg = .true.
          endif

c         Apply transfer function directly to image.
          if (trfun(igr).ne.'lin')
     *      call apptrfcg(pixr, trfun(igr), groff, win(1)*win(2),
     *         memi(ipnim), memr(ipim), NBINS, his, cumhis)

c         Apply specified OFM or do interactive fiddle to hardcopy
c         PGPLOT devices here.
          if (hard.eq.'YES') then
            call hardofm(coltab(j), pixr2, dofid, j,
     *        jj, dosing, tfvp, win, memr(ipim), memi(ipnim))

c           If we are going to use a b&w transfer function we must
c           account for the background colour of the device.  So make
c           the OFM the complement of itself here if the background is
c           going to be white.  Only works for b&w OFMs.
            if (bgcol.eq.1) call ofmcmp
          endif

c         Apply user specified OFM to PGPLOT device.
          if (hard.ne.'YES') call intofm(coltab(j), j, pixr)

c         Draw image.
          call pgimag(memr(ipim), win(1), win(2), 1, win(1),
     *                1, win(2), pixr2(1), pixr2(2), tr)
        endif

        call pgslw(lwid(1))
        call pgsci(labcol)

c       Determine if the axes need ascii or numeric labelling
c       for this subplot.
        call dolabcg(gaps, dotr, nx, ny, ngrps, nlast, j, labtyp,
     *               doaxlab, doaylab, donxlab, donylab)

c       Write on ascii axis labels.
        if (.not.docorner  .or.  jj.eq.(nx*ny-nx+1))
     *    call aaxlabcg(doaxlab, doaylab, xdispl, ydispb,
     *                  xlabel, ylabel)

c       Draw frame, write numeric labels, ticks and optional grid.
        krng(1) = grpbeg(j)
        krng(2) = ngrp(j)
        jplot = mod(j,nx*ny)
        noflab = nofirst .and. mod(jplot,nx).ne.1
        donum = .not.docorner  .or.  jj.eq.(nx*ny-nx+1)
        call naxlabcg(lhead, donum, blc, trc, krng, labtyp,
     *                donxlab, donylab, noflab, grid)

c       Draw wedge now so that it overwrites axis label ticks when
c       wedge drawn inside subplot.
        if (dowedge) call wedgecg(wedcod, WEDWID, jj, trfun(igr),
     *    groff, NBINS, cumhis, wdgvp, pixr(1,igr), pixr(2,igr))

c       Retake complement of OFM if needed (hardcopy/white backgrounds).
        if (hard.eq.'YES' .and. bgcol.eq.1) call ofmcmp

c       Interactive modification of OFM for interactive devices here.
        if (hard.eq.'NO' .and. dofid .and.
     *     ((jj.eq.nx*ny .or. j.eq.ngrps) .or. dosing))
     *    call ofmmod(tfvp, win(1)*win(2), memr(ipim),
     *                memi(ipnim), pixr2(1), pixr2(2))

c       Draw contour plots.
        if (ncon.gt.0) then
          do i = 1, ncon
            if (csize(3,i).gt.1) then
              krng(1) = grpbeg(j)
              krng(2) = ngrp(j)
            else
              krng(1) = 1
              krng(2) = 1
            endif
            call readimcg(.true., blankc, lc(i), ibin, jbin, krng,
     *        blc, trc, .true., memi(ipnim), memr(ipim),
     *        doblc, cmm(1,i))
            if (mskin.ne.' ' .and. doblm) then
              call maskorcg(blankc, win, meml(ipimm), memi(ipnim),
     *                      memr(ipim))
              doblc = .true.
            endif

            call pgslw(lwid(i+1))
            call pgsci(concol(i))
            call pgsch(cs(4))
            call conturcg(conlab, blankc, solneg(i), win(1), win(2),
     *        doblc, memr(ipim), nlevs(i), levs(1,i), tr, break(i),
     *        ncols(i), cols(1,i))
          enddo
        endif

c       Draw vector plots.
        if (vin(1).ne.' ' .and. vin(2).ne.' ') then
          if (vsize(3,1).gt.1) then
            krng(1) = grpbeg(j)
            krng(2) = ngrp(j)
          else
            krng(1) = 1
            krng(2) = 1
          endif
          call readimcg(.true., blankv, lv(1), ibin, jbin, krng,
     *       blc, trc, .true., memi(ipnim), memr(ipim), doblv(1), dmm)
          if (mskin.ne.' ' .and. doblm) then
            call maskorcg(blankv, win, meml(ipimm), memi(ipnim),
     *                    memr(ipim))
            doblv(1) = .true.
          endif
          call readimcg(.true., blankv, lv(2), ibin, jbin,
     *                  krng, blc, trc, .true., memi(ipnim2),
     *                  memr(ipim2), doblv(2), dmm)
          if (mskin.ne.' ' .and. doblm) then
            call maskorcg(blankv, win, meml(ipimm), memi(ipnim2),
     *                    memr(ipim2))
            doblv(2) = .true.
          endif

          veclwid=lwid(ncon+2)
          call pgslw(veclwid)
          call pgsci(veccol)

          call drawvec(lv, tr, vecfac, vecinc, win(1), win(2),
     *      memr(ipim), memi(ipnim), memr(ipim2), memi(ipnim2),
     *      scale, signs, rot90, nx, ny, getvsc, vfac, vecmax,
     *      vecmaxpix)
        endif

c       Draw box plots.
        if (bin.ne.' ') then
          if (bsize(3).gt.1) then
            krng(1) = grpbeg(j)
            krng(2) = ngrp(j)
          else
            krng(1) = 1
            krng(2) = 1
          endif

          call readimcg(.true., blankb, lb, ibin, jbin, krng, blc,
     *      trc, .true., memi(ipnim), memr(ipim), doblb, dmm)
          if (mskin.ne.' ' .and. doblm) then
            call maskorcg(blankb, win, meml(ipimm), memi(ipnim),
     *                    memr(ipim))
            doblb = .true.
          endif

          call pgsci(boxcol)
          call drawbox(lb, j, tr, boxfac, boxinc, win(1), win(2),
     *      memr(ipim), memi(ipnim), scale, bfac)
        endif

c       Write velocity or channel label.
        if (do3val .or. do3pix) then
          call pgslw(1)
          call pgsch(cs(2))
          call pgsci(1)
          call lab3cg(lhead, doerase, do3val, do3pix, labtyp,
     *                grpbeg(j), ngrp(j), val3form)
        endif

c       Read overlay positions list and decode positions into pixels
c       appropriate to the channel we are currently displaying.
        if (ofile.ne.' ') then
          call pgsci(ovrcol)
          call pgslw(lwid(ncon+nvec+2))
          call olay(dodist, doerase, ofile, grpbeg(j), ngrp(j),
     *              lhead, blc, trc, MAXTYP, ltypes, cs(3))
        endif

c       Draw beam(s).
        if (dobeam .or. (vecmax.ge.0.0)) then
          call pgsci(bemcol)
          call beampl(MAXCON, beaml, beamb, bmin, bmaj, bpa,
     *                bemprs, lc, lg, lv, lb, fs, hs, firstimage,
     *                vecmax, vecmaxpix, dobeam)
        endif

c       Plot annotation.
        if (dofull .and. (jj.eq.nx*ny .or. j.eq.ngrps)) then
          call pgslw(1)
          call pgsci(labcol)
          call fullann(MAXCON, ncon, cin, gin, vin, bin, lhead,
     *      lc, lg, lv, lb, MAXLEV, nlevs, levs, srtlev, slev, npixr,
     *      trfun, pixr, vfac, bfac, vymin, blc, trc, cs, ydispb,
     *      ibin, jbin, kbin, labtyp, gmm, cmm)
          call pgsci(1)
        endif

c       Increment sub-plot viewport locations and row counter and
c       reinit data min and maxes.
        call subinccg(j, nx, ny, vxmin, vymax, vxsize, vysize,
     *                vxgap, vygap, vx, vy)
        call mmini(MAXCON, gmm, cmm)

c       Page plot device.
        if (jj.eq.nx*ny .and. j.lt.ngrps) call pgpage
      enddo

c     Close down.
      call pgend

      call memfrep(ipim,  win(1)*win(2), 'r')
      call memfrep(ipnim, win(1)*win(2), 'i')
      if (vin(1).ne.' '  .and. vin(2).ne.' ') then
        call memfrep(ipim2,  win(1)*win(2), 'r')
        call memfrep(ipnim2, win(1)*win(2), 'i')
      endif
      if (mskin.ne.' ') call memfrep(ipimm, win(1)*win(2), 'i')

      do i = 1, ncon
        call coFin(lc(i))
        call xyclose(lc(i))
      enddo
      if (gin.ne.' ') then
        call coFin(lg)
        call xyclose(lg)
      endif
      if (vin(1).ne.' ') then
        call coFin(lv(1))
        call xyclose(lv(1))
      endif
      if (vin(2).ne.' ') then
        call coFin(lv(2))
        call xyclose(lv(2))
      endif
      if (mskin.ne.' ') then
        call coFin(lm)
        call xyclose(lm)
      endif

      end

c***********************************************************************

      subroutine beamfac(in, lin, bmin, bmaj, bpa, pres)

      character in*(*)
      integer   lIn
      real      bmin, bmaj, bpa
      logical   pres
c-----------------------------------------------------------------------
c  Drag the beam out of the header.
c
c  Input
c   in          Image name
c   lIn         Image handle
c  Output
c   bmin        FWHMin of beam in image
c   bmaj        FWHMax of beam
c   bpa         p.a. of beam
c               All in radians
c   pres        True if beam present in header
c-----------------------------------------------------------------------
      include 'mirconst.h'

      real      lrot
      character axtype*16, line*80, units*8, wtype*16

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      call rdhdr(lIn, 'bmin', bmin, -1.0)
      call rdhdr(lIn, 'bmaj', bmaj, -1.0)
      call rdhdr(lIn, 'bpa',  bpa,   0.0)
      call rdhdr(lIn, 'llrot',lrot,  0.0)
      bpa = bpa + lrot*DR2D

      if (bmin.gt.0.0 .and. bmaj.gt.0.0) then
c       Do the axes have radian units?  These are the only ones for
c       which we can convert the beam size in radians to world
c       coordinates.
        pres = .true.
        call coAxType(lIn, 1, axtype, wtype, units)
        if (units.ne.'rad') pres = .false.
        call coAxType(lIn, 2, axtype, wtype, units)
        if (units.ne.'rad') pres = .false.

        if (.not.pres) then
          line = 'Axes for image '//in(:len1(in))//
     *           ' are not recognized as having'
          call bug('w', line)
          call bug('w', 'increments in radians. Cannot plot beam')
        endif
      endif

      end

c***********************************************************************

      subroutine beampl(maxcon, beaml, beamb, bmin, bmaj, bpa, bemprs,
     *                  lc, lg, lv, lb, fs, hs, firstimage, vecmax,
     *                  vecmaxpix, dobeam)

      integer maxcon, lc(maxcon), lg, lv(2), lb
      logical beaml, beamb, bemprs(maxcon+4), dobeam
      real    bmin(maxcon+4), bmaj(maxcon+4), bpa(maxcon+4)
      integer fs, firstimage
      real    hs(3), vecmax, vecmaxpix
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
c    bemprs          True if beam present for maxcon contours, pixel map
c                    2 vector images and box image
c    l*              Handles
c    fs              PGPLOT fill style for first beam
c    hs              PGPLOT hatching style for first beam
c    firstimage      bemprs-style index of the beam with which to use
c                    fs and hs
c    vecmax          Length of vector scale-bar, in world coords
c                    (for annotation)
c    vecmaxpix       Length of vector scale-bar, in pixels
c    dobeam          We actually want to plot beams, not just
c                    the scale-bar!
c-----------------------------------------------------------------------
      integer i, luns(20)
      logical fill
      real xcen, ycen, sbxcen, sbycen
      real xv(2), yv(2), x, y
c-----------------------------------------------------------------------
c     Find location of centre of biggest beam.  They will be plotted
c     with the same centre.
      do i = 1, maxcon
        luns(i) = lc(i)
      enddo
      luns(maxcon+1) = lg
      luns(maxcon+2) = lv(1)
      luns(maxcon+3) = lv(2)
      luns(maxcon+4) = lb

      call beamxy(luns, maxcon, beaml, beamb, bemprs, bmin, bmaj,
     *   bpa, xcen, ycen, fill, sbxcen, sbycen, vecmax, vecmaxpix)

c     Draw the beam(s).
      if (dobeam) then
        do i = 1, maxcon+4
          if (bemprs(i)) then
            if (firstimage.eq.i) then
              call beampl2(luns(i), xcen, ycen, bmin(i), bmaj(i),
     *             bpa(i),.true.,fs,hs)
            else
              call beampl2(luns(i), xcen, ycen, bmin(i), bmaj(i),
     *             bpa(i),.false.,fs,hs)
            endif
          endif
        enddo
      endif

c     Draw the vector scale-bar, if required.
      if (vecmax.gt.0.0) then
        x = sbxcen
        y = sbycen

c       Draw it.
        xv(1) = x - vecmaxpix/2.0
        xv(2) = x + vecmaxpix/2.0
        yv(1) = y
        yv(2) = y
        call pgline(2, xv, yv)
      endif

      end

c***********************************************************************

      subroutine beampl2(lun, xcen, ycen, bmin, bmaj, bpa, fill,
     *                   fs, hs)

      integer lun
      real    bmin, bmaj, bpa, xcen, ycen
      logical fill
      integer fs
      real    hs(3)
c-----------------------------------------------------------------------
c  Draw one beam in the designated corner of the sub-plot.
c
c  Input
c    lun             Image handle
c    x,ycen          Coordinates of beam centre in absolute pixels
c    bmin,maj,pa     Beam FWHMin, FWHMax and p.a. for pixel map and
c                    contour image (rad).
c    fill            If true fill beam polygon in
c    fs              PGPLOT fill style
c    hs              PGPLOT hatching style
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   i, lw
      real      bbmaj, bbmin, bbpa, cp, pa, sp, xs(0:360), xx,
     *          ys(0:360), yy
      double precision win(2), wout(2)
      character typei(2)*6, typeo(2)*6
c-----------------------------------------------------------------------
      typei(1) = 'arcsec'
      typei(2) = 'arcsec'
      typeo(1) = 'relpix'
      typeo(2) = 'relpix'

      bbmin = bmin / 2.0
      bbmaj = bmaj / 2.0
      bbpa = (90.0 + bpa)*DD2R
      cp = cos(bbpa)
      sp = sin(bbpa)

c     Work out the beam polygon.
      do i = 0, 360
        pa = i*DD2R
        xx = bbmaj * cos(pa)
        yy = bbmin * sin(pa)

        win(1) = ( xx*cp + yy*sp)*DR2AS
        win(2) = (-xx*sp + yy*cp)*DR2AS

        call w2wco(lun, 2, typei, win, typeo, wout)

        xs(i) = wout(1) + xcen
        ys(i) = wout(2) + ycen
      enddo

c     Draw the beam with the desired line style and fill style.
      if (fill) then
        call pgsfs(fs)
        call pgqlw(lw)
        lw=int(lw*hs(3))
        call pgslw(lw)
        call pgshs(hs(1),hs(2),0)
      else
        call pgslw(2)
        call pgsfs(2)
      endif

      call pgpoly(361, xs(0), ys(0))

c     If we didn't do a filled poly, we'll want a border.
      if (((fs.eq.3) .or. (fs.eq.4)) .and. fill) then
          call pgsfs(2)
          call pgpoly(361, xs(0), ys(0))
        endif
      end

c***********************************************************************

      subroutine beamxy(luns, maxcon, beaml, beamb, bemprs, bmin,
     *     bmaj, bpa, xcen, ycen, fill, sbxcen,
     *     sbycen, vecmax, vecmaxpix)

      integer maxcon, luns(*)
      logical beaml, beamb, bemprs(maxcon+4), fill
      real    bmin(maxcon+4), bmaj(maxcon+4), bpa(maxcon+4),  xcen, ycen
      real    vecmax, vecmaxpix, sbxcen, sbycen
c-----------------------------------------------------------------------
c  We want to draw the beams, if there are more than one, with the same
c  centre.  Find the biggest x and y offsets from all the beams and
c  compute the centre with that.
c
c  Input
c    luns            Handles of image (contours, pixel map, vectors,
c                    box)
c    maxcon          Maximum number of contour images
c    beaml,beamb     True if the beam is to be drawn on the left
c                    or at the bottom (else right and top)
c    bemprs          True if beam present for pixel map and contour
c                    images
c    bmin,maj,pa     Beam FWHMin, FWHMax and p.a. for pixel map and
c                    contour image (rad).
c    vecmax          Length of scale-bar in world coord: .gt. 0.0
c                    if the bar is to be plotted
c    vecmaxpix       Length of scale-bar vector in pixels
c  Output
c    x,ycen          Absolute pixels of beam centres
c    sbxcen,sbycen   Absolute pixels of vector scale bar centre
c    fill            If true fill in the beam patch, else just outline
c-----------------------------------------------------------------------
      include 'mirconst.h'

      logical   const
      integer   i, j, sx, sy
      double precision win(2), wout(2)
      real      bbmaj, bbmin, bbpa, bmajo, bmino, bpao, cp, pa, sp, xhi,
     *          xlo, xmax, xmin, xoff, xwmax, xx, yhi, ylo, ymax, ymin,
     *          yoff, ywmax, yy
      character typei(3)*6, typeo(3)*6
c-----------------------------------------------------------------------
      typei(1) = 'arcsec'
      typei(2) = 'arcsec'
      typeo(1) = 'abspix'
      typeo(2) = 'abspix'

      xwmax = -1e30
      ywmax = -1e30
      const = .true.

c     Find first beam.
      do i = 1, maxcon+4
        if (bemprs(i)) then
          bmino = bmin(i)
          bmajo = bmaj(i)
          bpao  = bpa(i)
        endif
      enddo

c     Loop over beams.
      do j = 1, maxcon+4
        if (bemprs(j)) then
c         Check to see if all beams the same, so can set fill style.
          if (bmin(j).ne.bmino .or. bmaj(j).ne.bmajo .or.
     *        bpa(j).ne.bpao) const = .false.

c         Calculate useful factors for beam.
          bbmin = bmin(j) / 2.0
          bbmaj = bmaj(j) / 2.0
          bbpa = (90.0 + bpa(j))*DD2R
          cp = cos(bbpa)
          sp = sin(bbpa)

c         Find height and width of ellipse empirically, because it is
c         too late to do the algebra.
          xmin =  1e30
          xmax = -1e30
          ymin =  1e30
          ymax = -1e30

          do i = 0, 360, 4
            pa = i*DD2R
            xx = bbmaj * cos(pa)
            yy = bbmin * sin(pa)

c           x,y of ellipse locus in offset arcsec.
            win(1) = ( xx*cp + yy*sp)*DR2AS
            win(2) = (-xx*sp + yy*cp)*DR2AS

c           Convert to absolute pixels.
            call w2wco(luns(j), 2, typei, win, typeo, wout)

            xmin = min(wout(1),dble(xmin))
            xmax = max(wout(1),dble(xmax))
            ymin = min(wout(2),dble(ymin))
            ymax = max(wout(2),dble(ymax))
          enddo

c         Work out biggest x,y sizes.
          xwmax = max(abs(xmax - xmin),xwmax)
          ywmax = max(abs(ymax - ymin),ywmax)

c         Update.
          bmino = bmin(j)
          bmajo = bmaj(j)
          bpao  = bpa(j)
        endif
      enddo

c     Fill beam plot?
      fill = .false.
      if (const) fill = .true.

c     Find window (which is in absolute pixels).
      call pgqwin(xlo, xhi, ylo, yhi)
      sx = 1
      if (xlo.gt.xhi) sx = -1
      sy = 1
      if (ylo.gt.yhi) sy = -1

c     Work out ellipse centre.  Ellipse comes no closer than 2.5%
c     of the width of the plot from the boundary.
      xoff = abs(0.025*(xhi-xlo))
      yoff = abs(0.025*(yhi-ylo))

      if (beaml) then
        xcen = xlo + sx*(xoff + xwmax/2.0)
        sbxcen = xlo + sx*xoff + vecmaxpix/2.0
      else
        xcen = xhi - sx*(xoff + xwmax/2.0)
        sbxcen = xhi - sx*xoff - vecmaxpix/2.0
      endif

      if (beamb) then
        ycen = ylo + sy*(yoff + ywmax/2.0)
        if (vecmax.gt.0.0) ycen = ycen + sy*yoff
        sbycen = ylo + sy*yoff
      else
        ycen = yhi - sy*(yoff + ywmax/2.0)
        if (vecmax.gt.0.0) ycen = ycen - sy*yoff
        sbycen = yhi - sy*yoff
      endif

      end

c***********************************************************************

      subroutine chkdes(relax, im1, im2, size1, size2, lh1, lh2)

      logical relax
      character*(*) im1, im2
      integer size1(*), size2(*), lh1, lh2
c-----------------------------------------------------------------------
c  Compare axis descriptors for the first three axes.
c
c  Input:
c   relax        True for warnings only instead of fatal
c   im1,2        Images
c   size1,2      Sizes of each dimension
c   lh1,2        Handles
c-----------------------------------------------------------------------
      double precision desc1, desc2
      real epoch1, epoch2
      integer k, l1, l2, len1, maxis
      logical got23
      character line*130, itoaf*1, ctype1*9, ctype2*9, ks*1
c-----------------------------------------------------------------------
      l1 = len1(im1)
      l2 = len1(im2)

c     Allow 2-D with 3-D, but two 3-D cubes must be the same size.
      if (size1(1).ne.size2(1)) then
        line = 'Unequal dimensions for images '//im1(1:l1)//
     *         ' & '//im2(1:l2)//' on axis 1'
        call bug('f', line)
      endif
      if (size1(2).ne.size2(2)) then
        line = 'Unequal dimensions for images '//im1(1:l1)//
     *         ' & '//im2(1:l2)//' on axis 2'
        call bug('f', line)
      endif
      if (size1(3).gt.1 .and.
     *    size2(3).gt.1 .and.
     *    size1(3).ne.size2(3)) then
        line = 'Inconsistent dimensions for images '//im1(1:l1)//
     *         ' & '//im2(1:l2)//' on axis 3'
        call bug('f', line)
      endif

      call rdhdr(lh1, 'epoch', epoch1, 0.0)
      call rdhdr(lh2, 'epoch', epoch2, 0.0)
      if (epoch1.ne.epoch2) then
        line = 'Unequal epochs for images '//im1(1:l1)//' & '//im2(1:l2)
        if (relax) then
          call bug('w', line)
        else
          call bug('i', 'Try, with care, options=relax')
          call bug('f', line)
        endif
      endif

c     See if we have 2-D with 3-D.
      got23 = .false.
      if ((size1(3).eq.1 .and. size2(3).gt.1) .or.
     *     (size1(3).gt.1 .and. size2(3).eq.1)) got23 = .true.

c     Loop over axes of interest.
      maxis = 3
      if (size1(3).eq.1 .and. size2(3).eq.1) maxis = 2
      do k = 1, maxis
        if ((k.eq.3 .and. .not.got23) .or. k.le.2) then
          ks = itoaf(k)
          call rdhdd(lh1, 'cdelt'//ks, desc1, 0d0)
          call rdhdd(lh2, 'cdelt'//ks, desc2, 0d0)
          call chkdescg(relax, 'cdelt', k, im1(1:l1), im2(1:l2),
     *                   desc1, desc2)

          call rdhdd(lh1, 'crpix'//ks, desc1, 0d0)
          call rdhdd(lh2, 'crpix'//ks, desc2, 0d0)
          call chkdescg(relax, 'crpix', k, im1(1:l1), im2(1:l2),
     *                   desc1, desc2)

          call rdhdd(lh1, 'crval'//ks, desc1, 0d0)
          call rdhdd(lh2, 'crval'//ks, desc2, 0d0)
          call chkdescg(relax, 'crval', k, im1(1:l1), im2(1:l2),
     *                   desc1, desc2)

          call rdhda(lh1, 'ctype'//ks, ctype1, ' ')
          call rdhda(lh2, 'ctype'//ks, ctype2, ' ')
          if (ctype1.ne.ctype2) then
            write(line, 10) im1(1:l1), im2(1:l2), k
10          format('Unequal ctype for images ', a, ' & ', a,
     *              ' on axis ', i1)
            if (relax) then
              call bug('w', line)
            else
              call bug('i', 'Try, with care, options=relax')
              call bug('f', line)
            endif
          endif
        endif
      enddo

      end

c***********************************************************************

      subroutine chkim(maxnax, ncon, cin, lc, csize, gin, lg, gsize,
     *  vin, lv, vsize, bin, lb, bsize, mskin, lm, msize, relax)

      integer maxnax, ncon, csize(maxnax,*), gsize(maxnax),
     *  vsize(maxnax,2), bsize(maxnax), msize(maxnax), lc(*),
     *  lg, lv(2), lb, lm
      character*(*) cin(*), gin, vin(2), bin, mskin
      logical relax
c-----------------------------------------------------------------------
c  Check all the images for internal consistency.
c
c  Input:
c     maxnax     Maximum number of allowed dimenions for image
c     ncon       Number of contour images
c     relax      Only warnings instead of fatal errror for inconsistent
c                axis descriptors
c     *in        Input image names
c     *size      Size of each dimensions of images
c     l*         Image handles
c-----------------------------------------------------------------------
      integer i, j
c-----------------------------------------------------------------------
c     Check contour images for self consistency.
      if (ncon.gt.1) then
        do i = 1, ncon-1
          do j = i+1, ncon
            call chkdes(relax, cin(i), cin(j), csize(1,i), csize(1,j),
     *         lc(i), lc(j))
          enddo
        enddo
      endif

c     Check vector images for self consistency.
      if (vin(1).ne.' ') call chkdes(relax, vin(1), vin(2), vsize(1,1),
     *   vsize(1,2), lv(1), lv(2))

c     Check first contour image for consistency with other images.
      if (ncon.gt.0) then
        if (gin.ne.' ') call chkdes(relax, cin, gin, csize, gsize,
     *         lc, lg)
        if (vin(1).ne.' ') call chkdes(relax, cin, vin, csize, vsize,
     *         lc, lv)
        if (bin.ne.' ') call chkdes(relax, cin, bin, csize, bsize,
     *         lc, lb)
        if (mskin.ne.' ') call chkdes(relax, cin, mskin, csize, msize,
     *         lc, lm)
      endif

c     Check pixel map images for consistency with other images.
      if (gin.ne.' ') then
        if (vin(1).ne.' ') call chkdes(relax, gin, vin, gsize, vsize,
     *         lg, lv)
        if (bin.ne.' ') call chkdes(relax, gin, bin, gsize, bsize,
     *         lg, lb)
        if (mskin.ne.' ') call chkdes(relax, gin, mskin, gsize, msize,
     *         lg, lm)
      endif

c     Check vector images for consistency with other images.
      if (vin(1).ne.' ') then
        if (bin.ne.' ') call chkdes(relax, vin, bin, vsize, bsize,
     *         lv, lb)
        if (mskin.ne.' ') call chkdes(relax, vin, mskin, vsize, msize,
     *         lv, lm)
      endif

c     Check box image for consistency with other images.
      if (bin.ne.' ') then
        if (mskin.ne.' ') call chkdes(relax, bin, mskin, bsize, msize,
     *         lb, lm)
      endif

      end

c***********************************************************************

      subroutine decopt(dofull, do3val, do3pix, eqscale, gaps, solneg,
     *   beambl, beambr, beamtl, beamtr, relax, rot90, signs,
     *   mirror, dowedge, doerase, doepoch, dofid, dosing, nofirst,
     *   grid, dotr, dodist, conlab, doabut, blacklab, docorner)

      logical dofull, do3val, do3pix, eqscale, gaps, solneg(*),
     *  beambl, beambr, beamtl, beamtr, relax, rot90, signs,
     *  mirror, dowedge, doerase, doepoch, dofid, dosing, nofirst,
     *  grid, dotr, dodist, conlab, doabut, blacklab, docorner
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c     dofull    True means do full annotation of plot
c     do3val    True means label sub-plots with value of third axis
c     do3pix    True means label sub-plots with pixel of third axis
c     doerase   True means erase rectangle into which 3-axis label
c               written
c     eqscale   True means plot with x and y scales
c     gaps      True menas put space bewteen adjacent sub-plots
c     solneg    True means plot negative contours with solid line
c               style and positive contours with dashed line style
c               One for each contour image
c     beam%%    Beam location
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error
c     rot90     Rotate vectors by 90 degrees
c     signs     When plotting vectors, assume N and E are in
c               the direction of increasing X and Y, else N and E
c               are to the top and left
c     mirror    Multiply contours by -1 and add to list
c     dowedge   Draw wedge on pixel map image
c     doepoch   Write epoch into axis labels
c     dofid     Interactive fiddle
c     dosing    Fiddle after every subplot
c     nofirst   Don't put the first x-axis lable on any subplot
c               but the left most
c     grid      Extend ticks to grid
c     dotr      Label top and right as well as left and bottom axes
c     dodist    Distort overlays with grid
c     conlab    Label contours
c     doabut    No white space between subplots
c     blacklab  True if labels are black for white background devices
c     docorner  Only lower left corner gets labels
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 29)

      character opshuns(maxopt)*9
      logical present(maxopt)
      data opshuns /'full    ', '3value  ', '3pixel  ', 'unequal ',
     *              'gaps    ', 'solneg1 ', 'solneg2 ', 'solneg3 ',
     *              'beambl  ', 'beambr  ', 'beamtl  ', 'beamtr  ',
     *              'relax   ', 'rot90   ', 'signs   ', 'mirror',
     *              'wedge   ', 'noerase ', 'noepoch ', 'fiddle',
     *              'single  ', 'nofirst',  'grid    ', 'trlab',
     *              'nodistort', 'conlabel','abut    ', 'blacklab',
     *              'corner'/
c-----------------------------------------------------------------------
      call optcg('options', opshuns, present, maxopt)

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
      nofirst   =      present(22)
      grid      =      present(23)
      dotr      =      present(24)
      dodist    = .not.present(25)
      conlab    =      present(26)
      doabut    =      present(27)
      blacklab  =      present(28)
      docorner  =      present(29)

      end

c***********************************************************************

      subroutine drawbox(lh, iplot, tr, boxfac, boxinc, npixx, npixy,
     *   image, nimage, scale, bfac)

      integer boxinc(2), iplot, npixx, npixy, nimage(npixx,npixy), lh
      real boxfac, image(npixx,npixy), tr(6), scale(2), bfac(5)
c-----------------------------------------------------------------------
c  Draw boxes.  The boxes will come out square only if the pixel
c  increment is the and the user has requested equal scales.
c
c  Input
c    lh       Image handle
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
c    scale    Scale in natural coords per mm.  For RA axes radians on
c             the sky per mm
c  Input/output:
c    bfac     (1)   Maximum value of pixels in region of first subplot
c             (2:3) Scale factors, in x and y, to convert pixel value
c                   into box width in world coordinates
c             (4:5) Scale factors, in x and y, giving box widths per mm
c                   E.g. if pixel units are rad/m/m, then these scale
c                   factors you have b(4) & b(5) rad/m/m per mm
c-----------------------------------------------------------------------
      double precision cdelt(2)
      real xb(4), yb(4), x, y, x1, x2, y1, y2, vx1, vx2, vy1, vy2
      integer i, j
      real delx, dely, s
c-----------------------------------------------------------------------
c     Get pixel increments.
      call rdhdd(lh, 'cdelt1', cdelt(1), 0d0)
      call rdhdd(lh, 'cdelt2', cdelt(2), 0d0)

c     Get window and viewport.
      call pgqwin(x1, x2, y1, y2)
      call pgqvp(2, vx1, vx2, vy1, vy2)

c     Find maximum selected pixel from first sub-plot.
      if (iplot.eq.1) then
c       bfac(1) = -1.0e30
c       do j = 1, npixy, boxinc(2)
c         do i = 1, npixx, boxinc(1)
c           if (nimage(i,j).gt.0) bfac(1) = max(bfac(1),abs(image(i,j)))
c         enddo
c       enddo

c       Make maximum box width on the plot equal to 99% of the selected
c       pixel increment, multiplied by the users factor.
        bfac(2) = boxfac * 0.99*boxinc(1) * abs(tr(2)/bfac(1))
        bfac(3) = boxfac * 0.99*boxinc(2) * abs(tr(6)/bfac(1))

c       Find out the scale in world coordinates (really pixels per mm
c       now becase cgdisp works in pixels internally now) per mm in x
c       and y and then work out the scale of the boxes in image units
c       per mm (rad/m/m per mm) in x and y.
        s = tr(2) * scale(1) / cdelt(1)
        bfac(4) = abs(s / bfac(2))

        s = tr(6) * scale(2) / cdelt(2)
        bfac(5) = abs(s / bfac(3))
      endif

c     Loop over image.
      do j = 1, npixy, boxinc(2)
        do i = 1, npixx, boxinc(1)
          if (nimage(i,j).gt.0) then
c           Find unbinned pixel coordinates.
            x = tr(1) + tr(2)*i
            y = tr(4) + tr(6)*j

c           Find half width of box in pixel coordinates.
            delx =  bfac(2) * image(i,j) / 2.0
            dely =  bfac(3) * image(i,j) / 2.0

c           Draw it.  Solid boxes for +ve values, hollow for -ve.
            xb(1) = x - delx
            xb(2) = x + delx
            xb(3) = xb(2)
            xb(4) = xb(1)
            yb(1) = y - dely
            yb(2) = yb(1)
            yb(3) = y + dely
            yb(4) = yb(3)

            call pgsfs(2)
            if (image(i,j).gt.0) call pgsfs(1)
            call pgpoly(4, xb, yb)
          endif
        enddo
      enddo
      call pgsfs(2)

      end

c***********************************************************************

      subroutine drawvec(lv, tr, vecfac, vecinc, npixx, npixy,
     *    amp, namp, pa, npa, scale, signs, rot90, nx, ny,
     *    getvsc, vfac, vecmax, vecmaxpix)

      logical rot90, signs, getvsc
      integer vecinc(2), nx, ny, npixx, npixy, namp(npixx,npixy),
     *  npa(npixx,npixy), lv
      real vecfac, amp(npixx,npixy), pa(npixx,npixy), tr(6), scale(2),
     *  vfac(2), vecmax, vecmaxpix
c-----------------------------------------------------------------------
c  Draw and label vector scale bar.
c
c  Input
c    lv       Handle of image
c    tr       Transformation matrix from pixels to world coordinates
c    vecfac   Multiply amplitudes by this factor after self-scaling
c    vecinc   Increment through image in these steps
c    npixx,y  Size of images in pixles
c    amp,pa   Amplitude and position angle images
c    namp,npa Normalization images
c    scale    Scale in natural coords per mm.  For RA axes radians on
c             the sky per mm
c    signs    True means increasing X and Y = E and N, else
c             E and N to the left and top
c    rot90    Add 90 if true to position angle
c    nx,ny    Number of subplots in x and y directions
c    getvsc   If true, work out the automatic vector scaling factor
c             from this image if possible.
c    vfac     Maximum vector amplitude and the vector scale in
c             pixel units per mm (e.g. Jy/beam per mm).  Set on first
c             sub-plot
c  Input/Output
c    vecmax   Length of the scalebar vector. Input as:
c             -ve (no scale vector: output unchanged)
c             0   (for default length: output in world coord)
c             +ve (vector lenght specified by user: output unchanged)
c  Output
c    vecmaxpix  Length of the scalebar vector, in pixels
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision cdelt(2)
      real xv(2), yv(2), x, y
      integer i, j, pas
      real delx, dely, theta, sx, sy, x1, x2, y1, y2, vsizmax
      logical allbl
c-----------------------------------------------------------------------
c     Find pixel increments.
      call rdhdd(lv, 'cdelt1', cdelt(1), 0d0)
      call rdhdd(lv, 'cdelt2', cdelt(2), 0d0)

c     Find maximum selected vector amplitude for first partly unblanked
c     sub-plot.
      if (getvsc) then
        allbl = .true.
        vfac(1) = -1e30
        do j = 1, npixy, vecinc(2)
          do i = 1, npixx, vecinc(1)
            if (namp(i,j).gt.0 .and. npa(i,j).gt.0) then
              vfac(1) = max(vfac(1), abs(amp(i,j)))
              allbl = .false.
            endif
          enddo
        enddo

c       Make maximum amplitude on the plot 1/20 of min(width,height)
c       of the plot, multipled by the users factor.  Scale in mm.
        if (allbl) then
c         Only do this in case the user has asked for full annotation,
c         and is plotting one subplot per page.  It will look nicer
c         than seeing 1e30s.
          vfac(1) = 0
          vfac(2) = 0
        else
          getvsc = .false.
          call pgqvsz(2, x1, x2, y1, y2)
          vsizmax = min((x2-x1)/nx, (y2-y1)/ny) / 20.0
          vfac(2) = abs(vfac(1) / vecfac / vsizmax)
        endif
      else
c       We just assume there are some good pixels if we haven't looked
c       But if there aren't, nothing will be drawn anyway.
        allbl = .false.
      endif

c     If plane all blank, good bye. We will only know this if we
c     bothered to try and work out the scale factor from it though.
      if (allbl) return

c     Convert scale in natural coords per mm to world coords per mm
c     Because we now do everything internally in pixels, this is really
c     pixels per mm (tr(2) = ibin, tr(6) = jbin now).
      sx = tr(2) * abs(scale(1) / cdelt(1))
      sy = tr(6) * abs(scale(2) / cdelt(2))

c     Which way are N and E ?   N up and E left same as N down E right
c     N down E left same as N up E right as vectors have no arrowhead.
      pas = 1
      if (signs .and. cdelt(1)*cdelt(2).gt.0.0) pas = -1

c     Loop over image.
      do j = 1, npixy, vecinc(2)
        do i = 1, npixx, vecinc(1)
          if (namp(i,j).gt.0 .and. npa(i,j).gt.0 .and.
     *        amp(i,j).gt.0.0) then
c           Find unbinned pixel coordinates.
            x = tr(1) + tr(2)*i
            y = tr(4) + tr(6)*j

c           Position angle of vectors in radians.
            theta = pas * pa(i,j)*DD2R
            if (rot90) theta = theta + PI_2

c           Find half size of vectors in pixel coordinates.
            delx = -amp(i,j) * sin(theta) * sx / 2.0 / vfac(2)
            dely =  amp(i,j) * cos(theta) * sy / 2.0 / vfac(2)

c           Draw it.
            xv(1) = x - delx
            xv(2) = x + delx
            yv(1) = y - dely
            yv(2) = y + dely
            call pgline(2, xv, yv)
          endif
        enddo
      enddo

c     Set defaults for vector scale bar.
      if (vecmax.eq.0.0) vecmax = vfac(1)
      vecmaxpix =  vecmax * sx / vfac(2)

      end

c***********************************************************************

      subroutine drover(blc, trc, doerase, csize, iover, pix3, ofig,
     *  octrl, nVtx, xypts, oid, owrite, ochan)

      logical   doerase, owrite
      integer   blc(*), iover, nVtx, ochan(2), trc(*)
      real      csize, octrl(4), xypts(0:181,2)
      double precision pix3
      character ofig*(*), oid*(*)
c-----------------------------------------------------------------------
c  Draw overlays.
c
c  Input
c       blc      Blc of image being plotted in pixels.
c       trc      Trc of image being plotted in pixels.
c       doerase  Erase rectangle before writing overlay ID string.
c       csize    Character size for overlay ID.
c       pix3     Pixel value of third axis.
c       iover    Overlay number.
c       ofig     Overlay type.
c       octrl    Overlay PGPLOT parameters.
c       xypts    Up to 182 pairs describing overlays, in pixels.
c       oid      Overlay i.d.
c       owrite   If true write overlay ID in corner of overlay.
c       ochan    Overlay channel range.
c       xl,xr,yb,yt
c                Overlay extremeties in pixels.
c-----------------------------------------------------------------------
      logical   ok
      integer   isym
      real      ssize
      character line*80
c-----------------------------------------------------------------------
c     Only draw on specified channels.
      if (ochan(1).ne.0) then
        if (pix3.lt.ochan(1)) return

        if (ochan(2).ne.0) then
          if (pix3.gt.ochan(2)) return
        else
          if (pix3.gt.ochan(1)) return
        endif
      endif

      ok = xypts(0,1).ge.blc(1) .and. xypts(0,1).le.trc(1) .and.
     *     xypts(0,2).ge.blc(2) .and. xypts(0,2).le.trc(2)


      if (ofig.eq.'sym') then
c       PGPLOT symbol marker.
        isym = nint(octrl(1))
        if (isym.lt.0) isym = 12

        ssize = octrl(2)
        if (ssize.le.0.0) then
          if (csize.le.0.0) then
            ssize = 2.0
          else
            ssize = csize
          endif
        endif
        call pgsch(ssize)

        call pgpt(1, xypts(0,1), xypts(0,2), isym)

        xypts(1,1) = ssize

      else if (ofig.eq.'star') then
c       Cross defined by end points.
        call pgmove(xypts(1,1), xypts(1,2))
        call pgdraw(xypts(2,1), xypts(2,2))
        call pgmove(xypts(3,1), xypts(3,2))
        call pgdraw(xypts(4,1), xypts(4,2))

      else if (ofig.eq.'box') then
c       Box defined by four corners.
        call pgline(5, xypts(1,1), xypts(1,2))

      else if (ofig.eq.'line' .or. ofig.eq.'vector') then
c       Line interval defined by end points.
        if (ofig.eq.'line') then
          call pgline(2, xypts(1,1), xypts(1,2))
        else
          ssize = octrl(1)
          if (ssize.le.0.0) then
c           No arrowhead.
            call pgline(2, xypts(1,1), xypts(1,2))
          else
            call pgsch(ssize)
            call pgsah(nint(octrl(2)), octrl(3), octrl(4))
            call pgarro(xypts(1,1), xypts(1,2), xypts(2,1), xypts(2,2))
          endif
        endif

      else if (ofig.eq.'circle'  .or. ofig.eq.'ocircle' .or.
     *         ofig.eq.'ellipse' .or. ofig.eq.'oellipse') then
c       Draw poly-line and fill if necessary.
c       Set fill style.
        if (ofig(1:1).eq.'o') then
c         Outline.
          call pgsfs(2)
          call pgpoly(181, xypts(1,1), xypts(1,2))
        else
c         Filled.
          call pgsfs(1)
          call pgpoly(181, xypts(1,1), xypts(1,2))
          call pgsfs(2)
        endif

      else if (ofig.eq.'clear') then
c       Allow clear overlays anywhere.
        ok = .true.

      else
c       Unrecognized overlay type.
        write(line,10) ofig
 10     format('Unrecognized overlay type: ',a)
        call output(line)
        return
      endif

      if (.not.ok) then
        write(line,20) iover
 20     format('Overlay # ', i4, ' does not fully fit on the image')
        call output(line)
      endif

c     Write overlay identifying number.
      if (ok .and. owrite) then
        call overid(doerase, ofig, nVtx, xypts, oid, csize)
      endif

      end

c***********************************************************************

      subroutine fullann(maxcon, ncon, cin, gin, vin, bin, lh, lc,
     *   lg, lv, lb, maxlev, nlevs, levs, srtlev, slev, npixr,
     *   trfun, pixr, vfac, bfac, vymin, blc, trc, pcs, ydispb,
     *   ibin, jbin, kbin, labtyp, gmm, cmm)

      integer maxcon, maxlev, ncon, nlevs(maxcon), blc(*), trc(*),
     *  lc(maxcon), lg, lv(2), lb, srtlev(maxlev,maxcon), ibin(2),
     *  jbin(2), kbin(2), npixr, lh
      real levs(maxlev,maxcon), vymin, slev(maxcon), pixr(2), pcs,
     *  ydispb, vfac(2), bfac(5), gmm(2), cmm(2,maxcon)
      character*(*) cin(maxcon), gin, vin(2), bin, trfun, labtyp(2)
c-----------------------------------------------------------------------
c  Full annotation of plot with contour levels, RA and DEC etc.
c
c  Input
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
      real xpos, ypos, yinc
      integer i
c-----------------------------------------------------------------------
c     Setup chores and and annotate with reference values.
      call anninicg(lh, .false., vymin, pcs, ydispb, labtyp,
     *               xpos, ypos, yinc)

c     Write spatial window in pixels and channel inc. if possible.
      call annwincg(lh, blc, trc, ibin, jbin, kbin, yinc, xpos, ypos)

c     Write imaging information.
      if (gin.ne.' ') call anngrscg(lg, gin, npixr, pixr, trfun, gmm,
     *                               yinc, xpos, ypos)

c     Write contour image information.
      if (ncon.gt.0) then
        do i = 1, ncon
          call annconcg(lc(i), cin(i), slev(i), nlevs(i), levs(1,i),
     *       srtlev(1,i), cmm(1,i), yinc, xpos, ypos)
        enddo
      endif

c     Write vector information.
      if (vin(1).ne.' ' .and. vin(2).ne.' ')
     *   call annveccg(lv, vin, vfac, yinc, xpos, ypos)

c     Write box information.
      if (bin.ne.' ')
     *   call annboxcg(lb, bin, bfac, yinc, xpos, ypos)

      end

c***********************************************************************

      subroutine getbeam(maxcon, cin, lc, gin, lg, vin, lv, bin, lb,
     *   bmin, bmaj, bpa, dobeam, bemprs)

      integer maxcon
      character*(*) cin(maxcon), gin, vin(2), bin
      logical dobeam, bemprs(maxcon+4)
      integer lc(maxcon), lg, lv(2), lb
      real bmin(maxcon+4), bmaj(maxcon+4), bpa(maxcon+4)
c-----------------------------------------------------------------------
c  Get beam information from headers.
c
c  Input
c   maxcon     Maximum number of allowed contour images
c   *in        Image names
c   l*         Image handles
c  Output
c   bmin       FWHMin for maxcon contours, pixel map, two vectors
c              and box images
c   bmaj       FWHMax
c   bpa        p.a.
c              These are all in radians
c   bemprs     True if beam present for that image
c Input/output:
c   dobeam     If no beams to plot, this is set to false
c              COntour images, pixel map image, vector image and box
c              image
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
c     Contour images.
      do i = 1, maxcon
       if (lc(i).ne.0) then
         call beamfac(cin(i), lc(i), bmin(i), bmaj(i), bpa(i),
     *                 bemprs(i))
       else
         bemprs(i) = .false.
       endif
      enddo

c     Pixel map image.
      i = maxcon + 1
      bemprs(i) = .false.
      if (lg.ne.0) call beamfac(gin, lg, bmin(i),  bmaj(i), bpa(i),
     *                           bemprs(i))

c     Vector images.
      i = maxcon + 2
      bemprs(i) = .false.
      bemprs(i+1) = .false.
      if (lv(1).ne.0 .and. lv(2).ne.0) then
        call beamfac(vin(1), lv(1), bmin(i), bmaj(i), bpa(i),
     *                bemprs(i))

        i = i + 1
        call beamfac(vin(2), lv(2), bmin(i), bmaj(i), bpa(i),
     *                bemprs(i))
      endif

c     Box image.
      i = maxcon + 4
      bemprs(i) = .false.
      if (lb.ne.0) call beamfac(bin, lb, bmin(i),  bmaj(i), bpa(i),
     *                           bemprs(i))

      dobeam = .false.
      do i = 1, maxcon+4
        if (bemprs(i)) dobeam = .true.
      enddo

      end

c***********************************************************************

      subroutine hardofm(coltab, pixr2, dofid, j, jj, dosing, tfvp,
     *                    win, image, nimage)

      integer coltab, j, jj, win(2), nimage(*)
      real    tfvp(4), image(*), pixr2(2)
      logical dofid, dosing
c-----------------------------------------------------------------------
c  Apply user specified OFM to PGPLOT device.
c-----------------------------------------------------------------------
      if (coltab.eq.0) then
c       The user has not specified an OFM with the "range" keyword.  If
c       first subplot on first page, apply b&w as default.  Otherwise,
c       leave OFM at whatever it was last set to for the previous
c       subplot.
        if (j.eq.1) call ofmcol(1, pixr2(1), pixr2(2))

      else
c       The user has given an OFM with the "range" keyword for this
c       subplot.
        call ofmcol(coltab, pixr2(1), pixr2(2))
      endif

c     Interactive modification of OFM for hardcopy devices here; must be
c     done before PGIMAG called.  Any change of lookup table here will
c     overwrite that done with call to ofmcol above.
      if (dofid .and. (jj.eq.1 .or. dosing)) then
        write(*,*) 'fiddle on'
        call ofmmod(tfvp, win(1)*win(2), image, nimage,
     *               pixr2(1), pixr2(2))
      endif

      end

c***********************************************************************

      subroutine intofm(coltab, j, pixr2)

      integer coltab, j
      real pixr2(2)
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      if (coltab.eq.0) then
c       The user has not specified an OFM with the "range" keyword.  If
c       first subplot on first page, apply b&w as default.  Otherwise,
c       leave OFM at whatever it was last set to for the previous
c       subplot.
        if (j.eq.1) call ofmcol(1, pixr2(1), pixr2(2))
      else
c       The user has given an OFM with the "range" keyword for this
c       subplot.
        call ofmcol(coltab, pixr2(1), pixr2(2))
      endif

      end

c***********************************************************************

      subroutine inputs(maxgr, maxlev, maxcon, maxtyp, ltypes, ncon,
     *     cin, gin, nvec, vin, bin, mskin, ibin, jbin, kbin, levtyp,
     *     slev, levs, nlevs, npixr, pixr, trfun, coltab, vecfac,
     *     vecmax, vecinc, boxfac, boxinc, pdev, labtyp, dofull, do3val,
     *     do3pix, eqscale, gaps, solneg, nx, ny, lwid, break, cs,
     *     scale, ofile, dobeam, beaml, beamb, relax, rot90, signs,
     *     mirror, dowedge, doerase, doepoch, dofid, dosing, nofirst,
     *     grid, dotr, dodist, conlab, doabut, docorner, val3form,
     *     ncols, cols, fs, hs, firstimage, blacklab)

      integer maxlev, maxcon, maxtyp, maxgr, ncon, nvec, npixr
      real levs(maxlev,maxcon), pixr(2,maxgr), scale(2), cs(*),
     *  slev(maxcon), break(maxcon), vecfac, vecmax, boxfac, hs(3)
      integer nx, ny, nlevs(maxcon), lwid(maxcon+3), vecinc(2),
     *  boxinc(2), ibin(2), jbin(2), kbin(2), coltab(maxgr),
     *  cols(maxlev,maxcon), ncols(maxcon), fs, firstimage
      character*(*) labtyp(2), cin(maxcon), gin, vin(2), bin, mskin,
     *  pdev, ofile, trfun(maxgr), levtyp(maxcon), ltypes(maxtyp),
     *  val3form
      logical do3val, do3pix, dofull, gaps, eqscale, solneg(maxcon),
     *  dobeam, beaml, beamb, relax, rot90, signs, mirror, dowedge,
     *  doerase, doepoch, dofid, dosing, nofirst, grid, dotr,
     *  dodist, dunw, conlab, doabut, blacklab, docorner
c-----------------------------------------------------------------------
c  Get the unfortunate user's long list of inputs.
c
c  Input:
c   maxgr      Maximum number of pixel map scale intensity ranges and
c              transfer functions allowed.  The user can input one group
c              per sub-plot up to this maximum so that differnt subplots
c              can be displayed optimally.  If there are more sub-plots
c              than intensity ranges, the extras use the values for the
c              previous sub-plot.
c   maxlev     Maximum number of allowed contour levels
c   maxcon     Maximum number of contour images
c   maxtyp     Maximum number of label types
c   ltypes     Possible label types
c  Output:
c   ncon       Number of contour images
c   nvec       Number of pairs of vector images, 0 or 1
c   c,g,v,b,msk-in
c              Contour, pixel map, vector (amp & pa), box & mask image
c              names
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
c   coltab     Colour lookup table number
c   vecfac     Vector amplitude scale factor and
c   vecmax     Length of vector scale bar and
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
c              velocity/channel label, and the overlay ID string
c   scale      Scales for plot in x and y directions ( per mm)
c   ofile      Overlay box/star file name
c   dobeam     Draw the a little beam on each sub-plot
c   beaml      True if beam on left of sub-plot, else right
c   beamb      True if beam at bottom of sub-plot, else top
c   relax      Only issue warnings instead of fatal eror when
c              axis descriptors don;t agree between images
c   rot90      Rotate vectors by a further 90 degrees
c   signs      When plotting vectors, assume N and E are in
c              the direction of increasing X and Y
c   mirror     Multiply contours by -1 and add to list
c   dowedge    Draw a wedge on the pixel map
c   doepoch    Write epoch into axis labels
c   dofid      Interactive fiddle
c   dosing     Fiddle after each subplot
c   nofirst    Dont write first x-axis label on subplots except first
c   grid       Extend ticks to grid
c   dotr       Label top and right axes as well as bototm and left
c   dodist     Distort overlays with grid
c   conlab     Label contours
c   doabut     No white space bewteen subplots
c   docorner   Only draw labels with lower left subplot
c   val3form   Format for options=3val labelling
c   cols       Contour colours for each contour image.
c   ncols      Number of colours for each contour image.
c   fs         PGPLOT fill style
c   hs         PGPLOT hatching style
c   firstimage first image specified (used for beam plotting). Given
c              in bemprs format (see below)
c   blacklab   True if labels are black for white background devices
c-----------------------------------------------------------------------
      integer nmaxim
      parameter (nmaxim = 8)

      integer nim, nimtype, i, j, nlab
      character images(nmaxim)*64, imtype(nmaxim)*9
      character*1 str, itoaf
      character*2 newtb,newlr
      logical beambl, beambr, beamtl, beamtr, present, keyprsnt

      integer ntype
      parameter (ntype = 7)
      character type(ntype)*9
      data type  /'contour', 'pixel', 'amplitude', 'angle',
     *            'box', 'mask', 'grey'/
      data dunw /.false./
c-----------------------------------------------------------------------
      call keyini

c     Sort out input images.
      call mkeyf('in', images, nmaxim, nim)
      if (nim.eq.0) call bug('f', 'No images given')
      call keymatch('type', ntype, type, nmaxim, imtype, nimtype)

      ncon = 0
      nvec = 0
      do i = 1, nim
c       Default is "pixel" if one image, else "contour".
        if (imtype(i).eq.' ') then
          if (nim.eq.1) then
            imtype(i) = 'pixel'
          else
            imtype(i) = 'contour'
          endif
        endif

c       Find user given type of image.
        if (imtype(i).eq.'contour') then
          if (ncon.ge.maxcon) then
            call bug('f', 'Too many contour images given')
          else
            ncon = ncon + 1
            cin(ncon) = images(i)
          endif
        else if (imtype(i).eq.'pixel' .or. imtype(i).eq.'grey') then
          if (gin.ne.' ') then
            call bug('f', 'More than one pixel map image given')
          else
            gin = images(i)
          endif
        else if (imtype(i).eq.'amplitude') then
          if (vin(1).ne.' ') then
            call bug('f', 'More than one vector amplitude image given')
          else
            vin(1) = images(i)
          endif
        else if (imtype(i).eq.'angle') then
          if (vin(2).ne.' ') then
            call bug('f',
     *         'More than one vector position angle image given')
          else
            vin(2) = images(i)
            nvec = 1
          endif
        else if (imtype(i).eq.'box') then
          if (bin.ne.' ') then
            call bug('f', 'More than one box image given')
          else
            bin = images(i)
          endif
        else if (imtype(i).eq.'mask') then
          if (mskin.ne.' ') then
            call bug('f', 'More than one mask image given')
          else
            mskin = images(i)
          endif
        else
          call bug('f', 'Unrecognized image type')
        endif
      enddo

      if ((vin(1).ne.' ' .and. vin(2).eq.' ') .or.
     *    (vin(1).eq.' ' .and. vin(2).ne.' ')) call bug('f',
     *   'You must give both vector amplitude & position angle images')

c     Remember the first image given, in the bemprs numbering scheme:
c     maxcon contours, pixel map, 2 vector images and box image.
      if (imtype(1).eq.'contour') then
        firstimage=1
      else if (imtype(1).eq.'pixel' .or. imtype(1).eq.'grey') then
        firstimage=maxcon+1
      else if (imtype(1).eq.'amplitude') then
        firstimage=maxcon+2
      else if (imtype(1).eq.'angle') then
        firstimage=maxcon+2
      else if (imtype(1).eq.'box') then
        firstimage=maxcon+4
      else if (imtype(1).eq.'mask') then
c       This will work at the moment, because I test if
c       index .eq. firstimage, then use index. But this is
c       very slack programming. DPR.
        firstimage=0
      endif

c     Get on with the rest.
      call keyi('xybin', ibin(1), 1)
      call keyi('xybin', ibin(2), ibin(1))
      if (ibin(2).ne.1 .and. ibin(2).ne.ibin(1)) call bug('f',
     *  'Non-unit x spatial averaging must be equal to increment')
      ibin(1) = max(ibin(1), 1)
      ibin(2) = max(ibin(2), 1)

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

      call keya('slev', levtyp(1), 'a')
      call lcase(levtyp(1))
      if (levtyp(1).ne.'p' .and. levtyp(1).ne.'a') call bug('f',
     *   'Unrecognized contour level scale type; must be "p" or "a"')
      call keyr('slev', slev(1), 0.0)

      if (ncon.gt.1) then
        do i = 2, ncon
          call keya('slev', levtyp(i), 'a')
          call lcase(levtyp(i))
          if (levtyp(i).ne.'p' .and. levtyp(i).ne.'a') call bug('f',
     *     'Unrecognized contour level scale type; must be "p" or "a"')

          call keyr('slev', slev(i), 0.0)
        enddo
      endif

      do i = 1, maxcon
        str = itoaf(i)
        call mkeyr('levs'//str,  levs(1,i), maxlev, nlevs(i))

        ncols(i) = 0
        call mkeyi('cols'//str, cols(1,i), maxlev, ncols(i))
        if (ncols(i).gt.0 .and. ncols(i).lt.nlevs(i)) then
c         Fill the colours array by replicating the last entry.
          do j = ncols(i)+1, nlevs(i)
            cols(j,i) = cols(ncols(i),i)
          enddo
        else if (i.gt.1 .and. ncols(i).eq.0) then
c         Replicate colours from the first contour map.
          do j = 1, nlevs(i)
            cols(j,i) = cols(j,1)
          enddo
        endif
      enddo

c     Get pixel map ranges and transfer functions for each subplot.
      pixr(1,1) = 0.0
      pixr(2,1) = 0.0
      trfun(1) = 'lin'
      coltab(1) = 0
      present = keyprsnt ('range')
      i = 0

      do while (present .and. i.lt.maxgr)
        present = keyprsnt ('range')
        if (present) then
c         Get new group; just first value present is enough.
          i = i + 1
          call keyr('range', pixr(1,i), 0.0)
          call keyr('range', pixr(2,i), 0.0)
          call keya('range', trfun(i), 'lin')
          call lcase(trfun)
          call keyi('range', coltab(i), 0)

          if (gin.ne.' ' .and. trfun(i).ne.'lin' .and.
     *        trfun(i).ne.'log' .and. trfun(i).ne.'heq' .and.
     *        trfun(i).ne.'sqr') then
            call bug('w',
     *        'Unrecognized image transfer function, setting linear')
            trfun(i) = 'lin'
          endif
        endif
      enddo
      npixr = max(i,1)

      call keyr('vecfac', vecfac, 1.0)
      if (vecfac.le.0.0) vecfac = 1.0
      call keyi('vecfac', vecinc(1), 2)
      if (vecinc(1).le.0) vecinc(1) = 2
      call keyi('vecfac', vecinc(2), vecinc(1))
      if (vecinc(2).le.0) vecinc(2) = 2
      call keyr('vecfac', vecmax, -1.0)

      call keyr('boxfac', boxfac, 1.0)
      if (boxfac.le.0.0) boxfac = 1.0
      call keyi('boxfac', boxinc(1), 2)
      if (boxinc(1).le.0) boxinc(1) = 2
      call keyi('boxfac', boxinc(2), boxinc(1))
      if (boxinc(2).eq.0) boxinc(2) = 2

      call keya('device', pdev, ' ')

c     Get the beam parameters.
      call keya('beamtyp',newtb,'n')
      call keya('beamtyp',newlr,'l')
      call keyi('beamtyp',fs,1)
      call keyr('beamtyp',hs(1),45.0)
      call keyr('beamtyp',hs(2),1.0)
      call keyr('beamtyp',hs(3),1.0)

      call decopt(dofull, do3val, do3pix, eqscale, gaps, solneg,
     *   beambl, beambr, beamtl, beamtr, relax, rot90, signs,
     *   mirror, dowedge, doerase, doepoch, dofid, dosing, nofirst,
     *   grid, dotr, dodist, conlab, doabut, blacklab, docorner)

      call keya('3format', val3form, ' ')

      if (gin.eq.' ') then
        dowedge = .false.
        dofid = .false.
      endif

      if (gaps .and. doabut)
     *  call bug('f', 'options=gaps,abut is inconsistent')

      call keymatch('labtyp', maxtyp, ltypes, 2, labtyp, nlab)
      if (nlab.eq.0) then
        labtyp(1) = 'relpix'
        labtyp(2) = 'relpix'
      else if (nlab.eq.1) then
        if (labtyp(1).eq.'hms') then
          labtyp(2) = 'dms'
        else
          labtyp(2) = labtyp(1)
        endif
      endif
      if (labtyp(1)(4:6).eq.'lin') then
        labtyp(1)(4:6) = 'nat'
        call bug('w', 'Axis label types abslin and rellin are ')
        call bug('w', 'deprecated in favour of absnat and relnat')
        dunw = .true.
      endif
      if (labtyp(2)(4:6).eq.'lin') then
        labtyp(2)(4:6) = 'nat'
        if (.not.dunw) then
          call bug('w', 'Axis label types abslin and rellin are ')
          call bug('w', 'deprecated in favour of absnat and relnat')
        endif
      endif

      if ((index(labtyp(1),'nat').ne.0  .and.
     *      index(labtyp(2),'nat').eq.0) .or.
     *     (index(labtyp(1),'kms').ne.0  .and.
     *      index(labtyp(2),'kms').eq.0) .or.
     *     (index(labtyp(2),'ghz').ne.0  .and.
     *      index(labtyp(1),'ghz').eq.0)) then
        if (eqscale) call bug('i',
     *  'You might consider options=unequal with these axis LABTYPs')
      endif

      if (vin(1).ne.' ' .and. vin(2).ne.' ') then
        if (signs) then
          call output
     *       ('Assuming E & N in the direction of increasing X & Y')
        else
          call output('Assuming E & N to the left and top')
        endif
      endif

c     Old style beam specification.
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
      endif

c     New style beam specification.
      if (newtb.ne.'n') then
        dobeam = .true.
        beamb = (newtb.eq.'b')
        beaml = (newlr.eq.'l')
      endif

c     If user wanted a scale-bar, but didn't give a beam, then
c     give them bl.
      if ((vecmax.ge.0) .and. (.not.dobeam)) then
        beamb = .true.
        beaml = .true.
      endif

      call keyf('olay', ofile, ' ')
      if (ofile.ne.' ' .and. (labtyp(1).eq.'none' .or.
     *    labtyp(2).eq.'none')) call bug('f',
     *    'Overlays not allowed with labtyp=none')

      call keyi('nxy', nx, 0)
      call keyi('nxy', ny, nx)

      call keyi('lines', lwid(1), 1)
      if (lwid(1).le.0) lwid(1) = 1

      j = 2
      if (ncon.gt.0) then
        do i = 1, ncon
          call keyi('lines', lwid(j), 1)
          if (lwid(j).le.0) lwid(j) = 1
          call keyr('break', break(i), 0.0)
          j = j + 1
        enddo
      endif
      if (vin(1).ne.' ') then
        call keyi('lines', lwid(j), 1)
        j = j + 1
      endif
      if (ofile.ne.' ') call keyi('lines', lwid(j), 1)

      call keyr('csize', cs(1), 0.0)
      call keyr('csize', cs(2), 0.0)
      call keyr('csize', cs(3), 0.0)
      call keyr('csize', cs(4), 0.0)

      call keyr('scale', scale(1), 0.0)
      call keyr('scale', scale(2), scale(1))
      if (scale(1).lt.0.0) scale(1) = 0.0
      if (scale(2).lt.0.0) scale(2) = 0.0

      end

c***********************************************************************

      subroutine mmini(maxcon, gmm, cmm)

      integer maxcon
      real    gmm(2), cmm(2,maxcon)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      gmm(1) =  1e30
      gmm(2) = -1e30
      do i = 1, maxcon
        cmm(1,i) =  1e30
        cmm(2,i) = -1e30
      enddo

      end

c***********************************************************************

      subroutine olay(dodist, doerase, ofile, pl1, npl, lun, blc, trc,
     *                maxtyp, ltypes, csize)

      logical doerase, dodist
      integer maxtyp, lun, pl1, npl, blc(3), trc(3)
      real    csize
      character ltypes(maxtyp)*(*), ofile*(*)
c-----------------------------------------------------------------------
c  Read overlay positions list file, decode and plot.
c
c  Inputs
c     blc,trc  BLC and TRC of displayed imgae
c     doerase  If true erase background before writing overlay ID string
c     csize    PGPLOT character size
c     ofile    Overlay file name
c     pl1,npl  Start chan & number of chans displayed for this subplot
c     lun      Handle of image
c     maxtyp   Maximum number of label types
c     ltypes   Possible label types
c-----------------------------------------------------------------------
      logical ok, owrite
      integer i, icol, ilen, iostat, len1, lpos, lw, nVtx, ochan(2)
      real    octrl(4), xypts(0:181,2)
      double precision xoff, yoff, pix3
      character aline*300, id*8, ofig*8, oid*80

c     Externals.
      character itoaf*8
c-----------------------------------------------------------------------
      if (ofile.ne.' ') then
        call txtopen(lpos, ofile, 'old', iostat)
        if (iostat.ne.0) call bug('f', 'Error opening overlay file')

c       Initialize coordinate routines.
        xoff = 0.0
        yoff = 0.0
        i = 0
        iostat = 0
        pix3 = dble(2*pl1+npl-1) / 2.0

c       Loop over lines in file.
        do while (iostat.ne.-1)
          aline = ' '
          call txtread(lpos, aline, ilen, iostat)
          if (iostat.eq.0) then
            if (aline(1:1).ne.'#' .and. aline.ne.' ') then
              call lcase(aline(:6))
              if (aline(:7).eq.'colour' .or. aline(:6).eq.'color') then
c               Colour to be applied to succeeding overlays.
                call atoif(aline(7:), icol, ok)
                if (ok) then
                  call pgsci(icol)
                endif

              else if (aline(:5).eq.'lwid') then
c               Line thickness to be applied to succeeding overlays.
                call atoif(aline(6:), lw, ok)
                if (ok) then
                  call pgslw(lw)
                endif

              else if (aline(:7).eq.'offset') then
c               Offset to be applied to succeeding overlay locations.
                call posdec1(aline, xoff, yoff)

              else
c               Get overlay locations and convert to pixels.
                i = i + 1
                ilen = len1(aline)
                call posdec2(lun, pix3, maxtyp, ltypes, i, xoff, yoff,
     *            dodist, aline(1:ilen), ofig, octrl, oid, owrite,
     *            ochan, nVtx, xypts, ok)

c               Draw overlay.
                if (ok) then
                  call drover(blc, trc, doerase, csize, i, pix3, ofig,
     *              octrl, nVtx, xypts, oid, owrite, ochan)
                else
                  id = itoaf(i)
                  ilen = len1(id)
                  call bug('w',
     *              'Problem drawing overlay number ' // id(1:ilen))
                endif
              endif
            endif
          else
            if (iostat.ne.-1) call bug('f',
     *         'Error reading from overlay file')
          endif
        enddo

        call txtclose(lpos)
      endif

      end

c***********************************************************************

      subroutine overid(doerase, ofig, nVtx, xypts, str, csize)

      logical doerase
      integer nVtx
      real    csize, xypts(0:181,2)
      character*(*) str, ofig
c-----------------------------------------------------------------------
c  Write the overlay identification string on the overlay.
c
c  Input
c     doerase   True to erase background before writing string.
c     ofig      Type of overlay; sym, star, box, line, vector, circle,
c               ocircle, ellipse, oellipse, or clear.  ID written in
c                 corner for star and box,
c                 right  for sym, line, vector, circle,
c                            and ellipse.
c                 centre for ocircle, oellipse, and clear,
c     nVtx      Number of vertices in xypts.
c     xypts     Points describing the overlay vertices (in pixels).
c     str       Overlay identification string
c     csize     User supplied character size
c-----------------------------------------------------------------------
      integer ilen, j
      real    cDown, cHgt, cLeft, cMid, cRight, cSpan, cUp, cWid, dx,
     *        dy, just, oxspan, oyspan, sxbnd(4), sybnd(4), vlen, vpx1,
     *        vpx2, vpy1, vpy2, vsx1, vsx2, vsy1, vsy2, wx1, wx2, wy1,
     *        wy2, xfr, xs, xybnd(4), yfr, ys

      integer len1
      external len1
c-----------------------------------------------------------------------
c     Extremeties.
      if (nVtx.eq.0) then
        xybnd(1) = 0.0
        xybnd(2) = 0.0
        xybnd(3) = 0.0
        xybnd(4) = 0.0
      else
        xybnd(1) = xypts(1,1)
        xybnd(2) = xypts(1,1)
        xybnd(3) = xypts(1,2)
        xybnd(4) = xypts(1,2)

        do j = 2, nVtx
          xybnd(1) = min(xybnd(1), xypts(j,1))
          xybnd(2) = max(xybnd(2), xypts(j,1))
          xybnd(3) = min(xybnd(3), xypts(j,2))
          xybnd(4) = max(xybnd(4), xypts(j,2))
        enddo
      endif

c     Window extent in pixel coordinates.
      call pgqwin(wx1, wx2, wy1, wy2)

c     Extent of the overlay figure.
      oxspan = xybnd(2) - xybnd(1)
      oyspan = xybnd(4) - xybnd(3)

c     Set character size.
      if (csize.le.0.0) then
        if (ofig.eq.'sym') then
          xfr = 1.0 / 40.0
          yfr = xfr
        else if (ofig.eq.'clear') then
c         No overlay size in this case.  Use arbitrary fraction.
          xfr = 1.0 / 15.0
          yfr = xfr
        else
c         View-port and view-surface in normalized device coordinates.
          call pgqvp(0, vpx1, vpx2, vpy1, vpy2)
          call pgqvsz(0, vsx1, vsx2, vsy1, vsy2)
          xfr = abs((vpx2-vpx1) / (vsx2-vsx1) * oxspan / (wx2-wx1))
          yfr = abs((vpy2-vpy1) / (vsy2-vsy1) * oyspan / (wy2-wy1))
        endif

c       Set character size to 1/6 of the overlay size unless it is too
c       big or small, or use value given by user.
        csize = 40.0 * min(xfr,yfr) / 6.0
        csize = max(0.25, min(csize,20.0))
      endif
      call pgsch(csize)

c     Full character height (without descenders).
      call pgqtxt(0.0, 0.0, 0.0, 0.0, 'C', sxbnd, sybnd)
      cWid = sxbnd(4) - sxbnd(1)
      cHgt = sybnd(2) - sybnd(1)

c     Overlay ID string bounding box (pixels).
      ilen = len1(str)
      call pgqtxt(0.0, 0.0, 0.0, 0.0, str(1:ilen), sxbnd, sybnd)

c     Distance to move text to provide adequate clearance.
      cUp    = 0.3*cHgt - sybnd(1)
      cMid   = 0.3*cHgt
      cDown  = 0.3*cHgt + sybnd(2)
      cSpan  = sxbnd(4) - sxbnd(1)
      cLeft  = 0.3*cWid + cSpan
      cRight = 0.5*cWid

      if (ofig.eq.'sym') then
c       Write ID to right of symbol.
        xs = xypts(0,1) + 0.5*xypts(1,1)*(wy2-wy1)/40.0
        ys = xypts(0,2) - cMid
        just = 0.0

      else if (ofig.eq.'star') then
c       Write ID in top right corner.
        just = 0.0
        xs = max(xypts(0,1)+cRight, xybnd(2)-cSpan)
        if (xs.gt.xybnd(2)) xs = xybnd(2)
        ys = max(xypts(0,2)+cUp, xybnd(4)-sybnd(2))
        if (ys.gt.xybnd(4)) ys = xybnd(4)

      else if (ofig.eq.'box') then
c       Write ID in top right corner.
        just = 0.0
        xs = xybnd(2) - cLeft
        ys = xybnd(4) - cDown
        if (xs.lt.xybnd(1) .or. ys.lt.xybnd(3)) then
c         Too big!  Put it outside.
          xs = xybnd(1)
          ys = xybnd(4) + cUp
        endif

      else if (ofig.eq.'line') then
c       Write ID to one side of the line near the middle.
        dx = xypts(2,1) - xypts(1,1)
        dy = xypts(2,2) - xypts(1,2)

        if (dx.eq.0.0) then
          just = 0.0
          xs = xypts(1,1) + 0.5*dx + cRight
          ys = xypts(1,2) + 0.5*dy - cMid
        else if (dy/dx.lt.0.0) then
c         Put ID rightwards.
          just = 0.0
          xs = xypts(1,1) + 0.5*dx + cRight
          ys = xypts(1,2) + 0.5*dy + cUp
        else
c         Put ID leftwards.
          just = 1.0
          xs = xypts(1,1) + 0.5*dx - cRight
          ys = xypts(1,2) + 0.5*dy + cUp
        endif

      else if (ofig.eq.'vector') then
c       Write ID near tip of vector.
        dx = xypts(2,1) - xypts(1,1)
        dy = xypts(2,2) - xypts(1,2)

        if (abs(dx).lt.abs(dy)) then
          just = 0.5
          vlen = sqrt(dx*dx + dy*dy)
          if (dy.gt.0.0) then
c           Vector points upwards, put ID above.
            xs = xypts(2,1) + cUp * dx/vlen
            ys = xypts(2,2) + cUp * dy/vlen
          else
c           Vector points downwards, put ID below.
            xs = xypts(2,1) + cDown * dx/vlen
            ys = xypts(2,2) + cDown * dy/vlen
          endif

        else
          if (dx.ge.0.0) then
c           Vector points rightwards, put ID rightwards.
            just = 0.0
            xs = xypts(2,1) + cRight
            ys = xypts(2,2) - cMid

          else
c           Vector points leftwards, put ID leftwards.
            just = 1.0
            xs = xypts(2,1) - cRight
            ys = xypts(2,2) - cMid
          endif
        endif

      else if (ofig.eq.'circle') then
c       Put ID rightwards.
        just = 0.0
        xs = xybnd(2)   + cRight
        ys = xypts(0,2) - cMid

      else if (ofig.eq.'ocircle') then
        if (cSpan.lt.abs(2.0*(xypts(46,1)-xypts(0,1)))) then
c         Put ID at the centre.
          just = 0.5
          xs = xypts(0,1)
          ys = xypts(0,2) - cMid
        else
c         Put ID rightwards.
          just = 0.0
          xs = xybnd(2)   + cRight
          ys = xypts(0,2) - cMid
        endif

      else if (ofig.eq.'ellipse' .or. ofig.eq.'oellipse') then
        if (xypts(46,2).ge.xypts(0,2)) then
          xs = xypts(46,1)
          ys = xypts(46,2)
        else
          xs = xypts(136,1)
          ys = xypts(136,2)
        endif

        if (xs.ge.xypts(0,1)) then
c         Put ID rightwards.
          just = 0.0
          xs = xs + cRight
          ys = ys + cUp
        else
c         Put ID leftwards.
          just = 1.0
          xs = xs - cRight
          ys = ys + cUp
        endif

      else if (ofig.eq.'clear') then
c       Write ID in centre.
        xs = xypts(0,1)
        ys = xypts(0,2) - cMid
        just = 0.5
      endif

c     Optionally erase rectangle and write string.
      call strerscg(doerase, just, str(1:ilen), xs, ys)

      end

c***********************************************************************

      subroutine posdec1(aline, xoff, yoff)

      character*(*) aline
      double precision xoff, yoff
c-----------------------------------------------------------------------
c  Decode OFFSET string into offsets.
c
c  Input
c       aline    Input string
c  Output
c       x,yoff   Offsets
c-----------------------------------------------------------------------
      integer maxnum
      parameter (maxnum = 10)

      double precision nums(maxnum)
      integer lena, ipres, idx, icomm(maxnum)
      logical ok
c-----------------------------------------------------------------------
c     Find end of OFFSET string and start of numbers.
      idx = index(aline,'OFFSET')
      if (idx.eq.0) idx = index(aline,'offset')
      if (idx.eq.0) call bug('f',
     *   'Error finding OFFSET in overlay offset line')
      idx = idx + 6

      call strprpcg(maxnum, aline(idx:), icomm, ipres, lena)
      if (ipres.lt.2) call bug('f',
     *   'There are insufficient fields for overlay offset line')
      lena = lena + idx - 1

c     Extract the numeric part of the line which remains.
      call matodf(aline(idx:lena), nums, ipres, ok)
      if (.not.ok) then
        call bug('f', 'Error decoding overlay offset line')
      else
        xoff = nums(1)
        yoff = nums(2)
      endif

      end

c***********************************************************************

      subroutine posdec2(lun, pix3, maxtyp, ltypes, iline, xoff, yoff,
     *  dodist, aline, oFig, octrl, oid, owrite, ochan, nVtx, xypts, ok)

      logical   dodist, ok, owrite
      integer   iline, lun, maxtyp, ochan(2)
      real      octrl(4), xypts(0:181,2)
      double precision pix3, xoff, yoff
      character aline*(*), ltypes(maxtyp)*(*), oFig*(*), oid*(*)
c-----------------------------------------------------------------------
c  Decode string into positions list.
c
c  Input
c       lun      Handle of image.
c       pix3     Pixel of third axis for subplot currently being
c                displayed.
c       maxtyp   Maximum number of axis types.
c       ltypes   possible label types.
c       iline    Line number being decoded.
c       x,yoff   Offsets to add to decoded locations.
c       dodist   True to distort overlays with grid.
c       aline    Input string.
c  Output
c       oFig     Overlay figure type (star, box, clear, line etc).
c       oPix     Centre of overlay in unbinned full image pixels.
c       octrl    Overlay PGPLOT parameters used for 'sym' and 'vector'.
c       oid      The ID string to write.
c       owrite   True to write OID.
c       ochan    Start and end chans to display this overlay on.
c       nVtx     Number of vertices in xypts.
c       xypts    Points describing the overlay vertices (in pixels).
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer maxnum, nFigs
      parameter (maxnum = 20, nFigs = 10)

      integer   i, icomm(maxnum), ifac, il, ilat, ilng, ipt, isym, j,
     *          lena, naxis, nOpt, nPres, nReqd, nUsed, nVtx, sgn(2),
     *          slen, spos
      double precision cosrho, dpx, dpy, nums(maxnum), off(2), oPix(2),
     *          pa, phi, pix(3), r, rmaj, rmin, scl, sinrho, theta,
     *          vlen, wabs(3), wCen(3), width(2), wIn(3), x, y
      real      ssize
      character absdeg(3)*6, abspix(3)*6, oFigs(nFigs)*8, oType(2)*6,
     *          pType(3)*6, str*4, relpix(3)*6, wover*3, yesno(2)*3

c     Externals.
      integer len1
      character itoaf*4

      data absdeg /'absdeg', 'absdeg', 'abspix'/
      data abspix /'abspix', 'abspix', 'abspix'/
      data relpix /'relpix', 'relpix', 'abspix'/
      data oFigs  /'sym', 'star', 'box', 'line', 'vector', 'circle',
     *             'ocircle', 'ellipse', 'oellipse', 'clear'/
      data yesno  /'yes', 'no'/
c-----------------------------------------------------------------------
c     Prepare string for parsing.
      str  = itoaf(iline)
      slen = len1(str)
      call strprpcg(maxnum, aline, icomm, nPres, lena)
      if (nPres.lt.7) call bug('f',
     * 'Too few parameters for overlay # ' // str(1:slen))

c     Extract OFIG, XOTYPE, YOTYPE, ID, WRITE.
      oFig = aline(1:icomm(1)-1)
      call matchcg(iline, 'OFIG', oFig, 'overlay', nFigs, oFigs)

      oType(1) = aline(icomm(1)+1:icomm(2)-1)
      call matchcg(iline, 'XOTYPE', oType(1), 'overlay', maxtyp,
     *  ltypes)
      oType(2) = aline(icomm(2)+1:icomm(3)-1)
      call matchcg(iline, 'YOTYPE', oType(2), 'overlay', maxtyp,
     *  ltypes)

      oid = aline(icomm(3)+1:icomm(4)-1)
      il = len1(oid)
      do i = 1, il
        if (oid(i:i).eq.'_') oid(i:i) = ' '
      enddo

      wover = aline(icomm(4)+1:icomm(5)-1)
      call matchcg(iline, 'WRITE', wover, 'overlay', 2, yesno)
      call ucase(wover)
      owrite = .true.
      if (wover.eq.'NO') owrite = .false.
      nPres = nPres - 5

c     Mandatory columns:
c       X  Y               for sym, star, box, and clear
c       X1 Y1 X2 Y2        for line
c       X  Y  L  PA        for vector
c       X  Y  R            for circle  and ocircle
c       X  Y  R1 R2 PA     for ellipse and oellipse
c
c     Optional columns:
c       SY SS       CS CE  for sym,
c       XS YS       CS CE  for star and box,
c       SS FS A  B  CS CE  for vector
c                   CS CE  for all others.

c     Do we have the required number of parameters?
      nReqd = 0
      ifac = 1
      if (oFig.eq.'line') then
        ifac = 2
      else if (oFig.eq.'vector') then
        nReqd = 2
      else if (oFig.eq.'circle'  .or. oFig.eq.'ocircle') then
        nReqd = 1
      else if (oFig.eq.'ellipse' .or. oFig.eq.'oellipse') then
        nReqd = 3
      endif

      do j = 1, 2
        if (oType(j).eq.'hms' .or. oType(j).eq.'dms') then
          nReqd = nReqd + ifac*3
        else
          nReqd = nReqd + ifac
        endif
      enddo

      if (nPres.lt.nReqd) call bug('f',
     *  'Too few parameters for overlay # ' // str(1:slen))

c     Check that we have consistent overlay units and axes.
      pType(1) = oType(1)
      pType(2) = oType(2)
      pType(3) = 'abspix'

c     Want angular coords as offsets from the field centre, in arcsec.
      if (pType(1).eq.'hms' .or. pType(1).eq.'dms') then
        pType(1) = 'arcsec'
      endif
      if (pType(2).eq.'hms' .or. pType(2).eq.'dms') then
        pType(2) = 'arcsec'
      endif

      if (oFig.eq.'circle'  .or. oFig.eq.'ocircle' .or.
     *    oFig.eq.'ellipse' .or. oFig.eq.'oellipse') then
c       Must have angular units on both axes if radius was given in
c       angular units.
        pType(2) = pType(1)
      endif

      call chkaxco(lun, pType(1), 1)
      call chkaxco(lun, pType(2), 2)


c     Get overlay position in pixel coordinates.
      off(1) = xoff
      off(2) = yoff

c     Find DEC sign, could be on either axis.
      sgn(1) = 1
      sgn(2) = 1
      if (oType(1).eq.'dms') then
        spos = 5
        if (aline(icomm(spos)+1:icomm(spos)+1).eq.'-') sgn(1) = -1
      endif

      if (oType(2).eq.'dms') then
        if (oType(1).eq.'hms') then
          spos = 8
        else
          spos = 6
        endif
        if (aline(icomm(spos)+1:icomm(spos)+1).eq.'-') sgn(2) = -1
      endif

c     Extract the numeric part of the line that remains.
      call matodf(aline(icomm(5)+1:lena), nums, nPres, ok)
      if (.not.ok) return

c     Get the centre of the overlay in absolute pixel coordinates.
      ipt = 1
      call ol2pixcg(lun, pix3, oType, off, sgn, nums(ipt), oPix, nUsed)
      ipt = ipt + nUsed

      xypts(0,1) = oPix(1)
      xypts(0,2) = oPix(2)

      call rdhdi(lun, 'naxis', naxis, 0)
      naxis = min(3, naxis)

c     Get the centre of the overlay in convenient coordinates.  wIn is
c     used as a temporary variable for coordinate calculations.
      wIn(3) = pix3
      if (dodist) then
c       Shape distorts with grid.  Convert centre of overlay into
c       units given by X/YOTYPE.
        wIn(1) = oPix(1)
        wIn(2) = oPix(2)
        call w2wcov(lun, naxis, abspix, wIn, pType, wCen, ok)
        if (.not.ok) return

        dpx = 0d0
        dpy = 0d0
      else
c       Shape computed at the centre of the field and translated.
        wIn(1) = 0d0
        wIn(2) = 0d0
        call w2wcov(lun, naxis, relpix, wIn, pType, wCen, ok)
        if (.not.ok) return

        call w2wcov(lun, naxis, relpix, wIn, abspix, pix, ok)
        if (.not.ok) return
        dpx = oPix(1) - pix(1)
        dpy = oPix(2) - pix(2)
      endif


c     Process each overlay type.
      nOpt = nPres - nReqd
      nVtx = 0
      if (oFig.eq.'sym') then
        if (nOpt.gt.4) call bug('f',
     *    'Too many parameters for overlay # ' // str(1:slen))

c       Get optional parameters.
        isym  = -1
        ssize = 0.0
        if (ipt.le.nPres) then
          isym  = nint(nums(ipt))
          ipt = ipt + 1

          if (ipt.le.nPres) then
            ssize = real(nums(ipt))
            ipt = ipt + 1
          endif
        endif

c       Symbol size and type.
        octrl(1) = real(isym)
        octrl(2) = ssize

      else if (oFig.eq.'star' .or. oFig.eq.'box') then
        if (nOpt.gt.4) call bug('f',
     *     'Too many parameters for overlay # ' // str(1:slen))

c       Get optional parameters.
        width(1) = 0.0
        width(2) = 0.0
        if (ipt.le.nPres) then
          width(1) = nums(ipt)
          ipt = ipt + 1

          if (ipt.le.nPres) then
            width(2) = nums(ipt)
            ipt = ipt + 1
          else
            width(2) = width(1)
          endif
        endif

        if (oFig.eq.'star') then
          nVtx = 4

          wIn(1) = wCen(1) - width(1)
          wIn(2) = wCen(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(1,1) = pix(1) + dpx
          xypts(1,2) = pix(2) + dpy

          wIn(1) = wCen(1) + width(1)
          wIn(2) = wCen(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(2,1) = pix(1) + dpx
          xypts(2,2) = pix(2) + dpy

          wIn(1) = wCen(1)
          wIn(2) = wCen(2) - width(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(3,1) = pix(1) + dpx
          xypts(3,2) = pix(2) + dpy

          wIn(1) = wCen(1)
          wIn(2) = wCen(2) + width(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(4,1) = pix(1) + dpx
          xypts(4,2) = pix(2) + dpy
        else
          nVtx = 5
          wIn(1) = wCen(1) - width(1)
          wIn(2) = wCen(2) - width(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(1,1) = pix(1) + dpx
          xypts(1,2) = pix(2) + dpy

          wIn(1) = wCen(1) - width(1)
          wIn(2) = wCen(2) + width(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(2,1) = pix(1) + dpx
          xypts(2,2) = pix(2) + dpy

          wIn(1) = wCen(1) + width(1)
          wIn(2) = wCen(2) + width(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(3,1) = pix(1) + dpx
          xypts(3,2) = pix(2) + dpy

          wIn(1) = wCen(1) + width(1)
          wIn(2) = wCen(2) - width(2)
          call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
          if (.not.ok) return
          xypts(4,1) = pix(1) + dpx
          xypts(4,2) = pix(2) + dpy

          xypts(5,1) = xypts(1,1)
          xypts(5,2) = xypts(1,2)
        endif

      else if (oFig.eq.'line') then
        if (nOpt.gt.2) call bug('f',
     *     'Too many numbers for overlay # ' // str(1:slen))

c       Line end in absolute pixels.
        call ol2pixcg(lun, pix3, oType, off, sgn, nums(ipt), pix,
     *                 nUsed)
        ipt = ipt + nUsed

        nVtx = 2
        xypts(1,1) = oPix(1)
        xypts(1,2) = oPix(2)
        xypts(2,1) = pix(1)
        xypts(2,2) = pix(2)

      else if (oFig.eq.'vector') then
        if (nOpt.gt.5) call bug('f',
     *     'Too many numbers for overlay # ' // str(1:slen))

c       Get vector length and position angle.
        vlen = nums(ipt)
        pa   = nums(ipt+1)
        ipt  = ipt + 2

c       Do we have a longitude/latitude pair?
        call coFindAx(lun, 'longitude', ilng)
        call coFindAx(lun, 'latitude',  ilat)
        if (ilng.gt.2 .or. ilat.gt.2) ilng = 0

        if (ilng.ne.0) then
c         Celestial axis pair.
          call w2wcov(lun, naxis, pType, wCen, absdeg, wabs, ok)
          if (.not.ok) return

          call sphpad(1, wabs(ilng), wabs(ilat), 0.1d0, pa, wIn(ilng),
     *      wIn(ilat))

          pType(1) = 'absdeg'
          pType(2) = 'absdeg'
        else
c         Some other axis types.  rho is measured from the x-axis.
          cosrho = cos((90d0+pa)*DD2R)
          sinrho = sin((90d0+pa)*DD2R)
          wIn(1) = wCen(1) + 0.1d0*cosrho
          wIn(2) = wCen(2) - 0.1d0*sinrho
        endif

        call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
        if (.not.ok) return

        dpx = pix(1) - oPix(1)
        dpy = pix(2) - oPix(2)
        scl = vlen / sqrt(dpx*dpx + dpy*dpy)

        nVtx = 2
        xypts(1,1) = oPix(1)
        xypts(1,2) = oPix(2)
        xypts(2,1) = oPix(1) + dpx * scl
        xypts(2,2) = oPix(2) + dpy * scl

c       Optional parameters describe arrowhead style.
        octrl(1) =  0.5
        octrl(2) =  1.0
        octrl(3) = 45.0
        octrl(4) =  0.3
        if (ipt.le.nPres) then
          octrl(1) = real(nums(ipt))
          ipt = ipt + 1

          if (ipt.le.nPres) then
            octrl(2) = real(nums(ipt))
            ipt = ipt + 1

            if (ipt.le.nPres) then
              octrl(3) = real(nums(ipt))
              ipt = ipt + 1

              if (ipt.le.nPres) then
                octrl(4) = real(nums(ipt))
                ipt = ipt + 1
              endif
            endif
          endif
        endif

      else if (oFig.eq.'circle'  .or. oFig.eq.'ocircle' .or.
     *         oFig.eq.'ellipse' .or. oFig.eq.'oellipse') then
        if (nOpt.gt.2) call bug('f',
     *     'Too many numbers for overlay # ' // str(1:slen))

        if (oFig.eq.'circle' .or. oFig.eq.'ocircle') then
c         Radius - specified in XOTYPE units.
          rmaj = nums(ipt)
          ipt = ipt + 1

c         Circle treated as special case of an ellipse.
          rmin = rmaj
          pa   = 0d0

        else
c         Major and minor axis half-widths specified in XOTYPE units,
c         and position angle in degrees.
          rmaj = nums(ipt)
          rmin = nums(ipt+1)
          pa   = nums(ipt+2)
          ipt  = ipt + 3
        endif

c       Do we have a longitude/latitude pair?
        call coFindAx(lun, 'longitude', ilng)
        call coFindAx(lun, 'latitude',  ilat)
        if (ilng.gt.2 .or. ilat.gt.2) ilng = 0

c       Generate poly-line coordinates for ellipse.
        nVtx = 181
        if (ilng.ne.0) then
c         Celestial axis pair.
          if (pType(1).eq.'arcsec') then
            rmaj = rmaj / 3600d0
            rmin = rmin / 3600d0
          endif

          call w2wcov(lun, naxis, pType, wCen, absdeg, wabs, ok)
          if (.not.ok) return

          pType(1) = 'absdeg'
          pType(2) = 'absdeg'

          do j = 1, nVtx
            theta = dble(2*(j-1))*DD2R
            x = rmaj * cos(theta)
            y = rmin * sin(theta)
            r = sqrt(x*x + y*y)
            phi = pa + atan2(y, x)*DR2D

            call sphpad(1, wabs(ilng), wabs(ilat), r, phi, wIn(ilng),
     *        wIn(ilat))

            call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
            if (.not.ok) return
            xypts(j,1) = pix(1) + dpx
            xypts(j,2) = pix(2) + dpy
          enddo

        else
c         Some other axis types.  rho is measured from the x-axis.
          cosrho = cos((90d0+pa)*DD2R)
          sinrho = sin((90d0+pa)*DD2R)

          do j = 1, nVtx
            theta = dble(2*(j-1))*DD2R
            x = rmaj * cos(phi)
            y = rmin * sin(phi)
            wIn(1) = wCen(1) + x*cosrho + y*sinrho
            wIn(2) = wCen(2) - x*sinrho + y*cosrho

            call w2wcov(lun, naxis, pType, wIn, abspix, pix, ok)
            if (.not.ok) return
            xypts(j,1) = pix(1) + dpx
            xypts(j,2) = pix(2) + dpy
          enddo
        endif

      else if (oFig.eq.'clear') then
        if (nOpt.gt.2) call bug('f',
     *     'Too many numbers for overlay # ' // str(1:slen))
      endif


c     Get channel range.
      ochan(1) = 0
      ochan(2) = 0
      if (ipt.le.nPres) then
        ochan(1) = nint(nums(ipt))
        ipt = ipt + 1

        if (ipt.le.nPres) then
          ochan(2) = nint(nums(ipt))
        else
          ochan(2) = ochan(1)
        endif
      endif

      end

c***********************************************************************

      subroutine region(maxcon, maxnax, ncon, cin, gin, vin, bin,
     *   lc, lg, lv, lb, csize, gsize, vsize, bsize, cnaxis, gnaxis,
     *   vnaxis, bnaxis, lhead, ibin, jbin, kbin, blc, trc,
     *   win, ngrps, grpbeg, ngrp)

      integer maxcon, ncon, maxnax, csize(maxnax,maxcon),
     *  gsize(maxnax), vsize(maxnax), bsize(maxnax), blc(*),
     *  trc(*), cnaxis(maxcon), gnaxis, vnaxis(2), bnaxis,
     *  win(2), ngrp(*), grpbeg(*), ngrps, ibin(2), jbin(2), kbin(2),
     *  lhead, lc(maxcon), lg, lv, lb
      character*(*) cin(maxcon), gin, vin, bin
c-----------------------------------------------------------------------
c  Finish key routie inputs for region of interest now.  Have to
c  delay until here because of complexity added by mixed 2-D/3-D
c  capability.   The BOXINPUT routine must be associated with the
c  file that, if any, has three dimensions.    Return also the
c  axis descriptors for all further positional use.
c
c  Input:
c    maxcon        Maximum number of contour images allowed
c    maxnax        Maximum number of allowed dimenions for image
c    ncon          Number of contour images
c    *in           Image names
c    l*            Handles
c    *size         Sizes of images
c    *naxis        Number of axes
c    i,j,kbin      x,y, and z pixel increment and binning sizes
c  Output:
c    lhead         Handle of generic image
c    blc,trc       3-D Hyper-rectangle surrounding region of interest
c                  in unbinned pixels
c    win           Size of BINNED region of interest for x and y
c                  directions
c    ngrps         Number of groups of channels.
c    grgbeg        List of start planes for each group of channels
c                  that are to be avearged together for each sub-plot
c                  A new group is begun at every interruption to the
c                  continuity of the selected channels, or if the
c                  channel increment is reached.
c    ngrp          Number of channels in each group of channel to
c                  be averaged together for each sub-plot.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer maxbox
      parameter (maxbox = 1024)

      integer boxes(maxbox), i, naxis, size(3)
      character itoaf*1
c-----------------------------------------------------------------------
c     Use the first cube we find to set the rest of the box inputs.
      size(3) = 0
      if (ncon.gt.0) then
        do i = 1, ncon
          if (csize(3,i).gt.1 .and. size(3).eq.0) then
            naxis = cnaxis(i)
            call boxinput('region', cin(i), boxes, maxbox)
            call boxset(boxes, cnaxis(i), csize(1,i), ' ')
            lhead = lc(i)
            size(3) = csize(3,i)
          endif
        enddo
      endif

      if (gin.ne.' ' .and. size(3).eq.0) then
        if (gsize(3).gt.1) then
          naxis = gnaxis
          call boxinput('region', gin, boxes, maxbox)
          call boxset(boxes, gnaxis, gsize, ' ')
          lhead = lg
          size(3) = gsize(3)
        endif
      endif

      if (vin.ne.' ' .and. size(3).eq.0) then
        if (vsize(3).gt.1) then
          naxis = vnaxis(1)
          call boxinput('region', vin, boxes, maxbox)
          call boxset(boxes, vnaxis, vsize, ' ')
          lhead = lv
          size(3) = vsize(3)
        endif
      endif

      if (bin.ne.' ' .and. size(3).eq.0) then
        if (bsize(3).gt.1) then
          naxis = bnaxis
          call boxinput('region', bin, boxes, maxbox)
          call boxset(boxes, bnaxis, bsize, ' ')
          lhead = lb
          size(3) = bsize(3)
        endif
      endif

c     If we didn't encounter a cube, then use any of the open 2-D images
c     for the box routines.  They have all been checked for identical
c     first and second dimensions.
      if (size(3).eq.0) then
        if (ncon.gt.0) then
          naxis = cnaxis(1)
          call boxinput('region', cin, boxes, maxbox)
          call boxset(boxes, naxis, csize, ' ')
          lhead = lc(1)
        else if (gin.ne.' ') then
          naxis = gnaxis
          call boxinput('region', gin, boxes, maxbox)
          call boxset(boxes, naxis, gsize, ' ')
          lhead = lg
        else if (vin.ne.' ') then
          naxis = vnaxis(1)
          call boxinput('region', vin, boxes, maxbox)
          call boxset(boxes, naxis, vsize, ' ')
          lhead = lv
        else if (bin.ne.' ') then
          naxis = bnaxis
          call boxinput('region', bin, boxes, maxbox)
          call boxset(boxes, naxis, bsize, ' ')
          lhead = lb
        else
          call bug('f', 'Internal logic error in REGION')
        endif
      endif
      call keyfin

c     Find hyper-rectangle surrounding region of interest from highest
c     dimension image involved (i.e., 2-D/3-D).
      call boxinfo(boxes, 3, blc, trc)
      do i = 1, min(3,naxis)
         call rdhdi(lhead, 'naxis'//itoaf(i), size(i), 0)
         blc(i) = max(1,blc(i))
         trc(i) = min(size(i),trc(i))
      enddo

c     Adjust spatial window to fit an integral number of bins and
c     find size of binned window.
      call winfidcg(size(1), 1, ibin, blc(1), trc(1), win(1))
      call winfidcg(size(2), 2, jbin, blc(2), trc(2), win(2))

c     Find list of start channels and number of channels for each group
c     of channels selected.  The BOX routines do not easily, if at all,
c     allow us to deal with multiple BOXes at once (say if there were
c     two differently masked cubes being plotted), so we don't AND
c     in the flagging mask.
      call chnselcg(blc, trc, kbin, maxbox, boxes, ngrps, grpbeg, ngrp)

      end

c***********************************************************************

      subroutine sesame(relax, maxnax, maxcon, ncon, cin, lc, csize,
     *   cnaxis, gin, lg, gsize, gnaxis, vin, lv, vsize, vnaxis, bin,
     *   lb, bsize, bnaxis, mskin, lm, msize, mnaxis, cmm, gmm)

      integer maxnax, maxcon, ncon, csize(maxnax,maxcon),
     *  gsize(maxnax), vsize(maxnax,2), msize(maxnax), bsize(maxnax),
     *  lc(maxcon), lg, lv(2), lm, lb, cnaxis(maxcon), gnaxis,
     *  vnaxis(2), bnaxis, mnaxis
      real cmm(2,maxcon), gmm(2)
      logical maskm, relax
      character*(*) cin(maxcon), gin, vin(2), mskin, bin
c-----------------------------------------------------------------------
c  Open all required images, check their self consistency and
c  return their sizes and handles.
c
c  Input
c   relax     Warning only on axis inconsistencies, else fatal
c   maxnax    Max allowed number of dimensions
c   maxcon    Max allowed number of contour images
c   ncon      Number of contour images
c   *in       Image names
c  Output
c   l*        Handles
c   *size     Image sizes
c   *naxis    Number of axes in images
c   cmm       Data min and max for each contour image initialized
c             to +/-1e30
c   gmm       Data min and max for pixel map image initialized
c             to +/-1e30
c-----------------------------------------------------------------------
      logical hdprsnt
      integer i
c-----------------------------------------------------------------------
c     Open contour images as required.
      if (ncon.gt.0)  then
        do i = 1, ncon
          call opimcg(maxnax, cin(i), lc(i), csize(1,i), cnaxis(i))
          call coInit(lc(i))
          cmm(1,i) = 1e30
          cmm(2,i) = -1e30
        enddo
      endif

c     Open pixel map image as required.
      if (gin.ne.' ') then
        call opimcg(maxnax, gin, lg, gsize, gnaxis)
        call coInit(lg)
        gmm(1) = 1e30
        gmm(2) = -1e30
      endif

c     Open vector images as required.
      if (vin(1).ne.' ' .and. vin(2).ne.' ') then
        do i = 1, 2
          call opimcg(maxnax, vin(i), lv(i), vsize(1,i), vnaxis(i))
          call coInit(lv(i))
        enddo
      endif

c     Open box image as required.
      if (bin.ne.' ') then
        call opimcg(maxnax, bin, lb, bsize, bnaxis)
        call coInit(lb)
      endif

c     Open mask image as required.
      if (mskin.ne.' ') then
        call opimcg(maxnax, mskin, lm, msize, mnaxis)
        call coInit(lm)
        maskm = hdprsnt (lm, 'mask')
        if (.not.maskm)  then
          call bug('w', 'The mask image does not have a mask')
          call coFin(lm)
          call xyclose(lm)
          mskin = ' '
        endif
      endif

c     Check consistency of input images.
      call chkim(maxnax, ncon, cin, lc, csize, gin, lg, gsize, vin,
     *             lv, vsize, bin, lb, bsize, mskin, lm, msize, relax)

      end

c***********************************************************************

      subroutine setlgc(bgcol, labcol, concol, veccol, boxcol,
     *                  ovrcol, bemcol, blacklab)

      integer bgcol, concol(*), veccol, boxcol, ovrcol, bemcol, labcol
      logical blacklab
c-----------------------------------------------------------------------
c  Set line graphics colours.
c
c  Input
c    bgcol 0 -> background is black
c          1 ->               white
c         -1 ->               something else
c    blacklab - true if labels are to be black for white background
c               devices (default is red?!)
c  Output
c    colour indices to use
c-----------------------------------------------------------------------
c     Labels.
      labcol = 7
      if (bgcol.eq.1) then
c       White background.
        if (blacklab) then
          labcol = 1
        else
          labcol = 2
        endif

      else if (bgcol.eq.0) then
c       Black background.
        labcol = 7
      else
        call bug('w', 'Non black/white background colour on device')
        labcol = 7
      endif

c     Contours.
      concol(1) = 7
      concol(2) = 5
      concol(3) = 9
      if (bgcol.eq.1) concol(1) = 2

c     Vectors.
      veccol = 2
      if (bgcol.eq.1) veccol = 8

c     Boxes.
      boxcol = 6

c     Overlays.
      ovrcol = 9

c     Beams.
      bemcol = 4

      end
