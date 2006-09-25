\chapter{Displaying Images}\label{c:display}\index{image display}

\section{Introduction}
As discussed in Chapter~\ref{c:pltvcon}, there are two basic suites of
display software for \miriad\ data.  One uses the ATNF Visualisation
suite, and the other is PGPLOT-based

The ATNF visualisation software, which lies outside the domain of
\miriad, uses a X-windows based user interface, with pop up menus, etc.
It is largely interactive, and tends to be poorer when quantitative
information is needed. The PGPLOT-based software, uses the normal
\miriad\ user interface, tends to be better suited to hardcopy output,
quantitative work, and for more specialised astronomical applications
(e.g. plotting polarisation vectors).

\section{The ATNF Visualisation Software}
The ATNF Visualisation software is a suite of tools (outside \miriad) designed
to display images and data cubes (including \miriad\ images and cubes). See
Chapter~\ref{c:pltvcon} for more information.

The main visualisation tool of interest in image display is {\tt kvis}
This tool allows you to inspect one or two 2- or
3-dimensional datasets in several ways.  The simplest thing to use it
for displaying a dataset.  {\tt kvis}
gives flexible zooming, special colour
maps for velocity fields etc.  For 3-dimensional datasets one can play
the channels, as well as the RA-VEL and DEC-VEL slices, of the data
cube as a movie, in order to get an idea of the 3D structure of the
emission in the data cube.

Another application of {\tt kvis} is that one can load two datasets
-- one can then look at these two datasets simultaneously in several
ways, like blinking or contour one data set on top of the other.  The
advantage of {\tt kvis} is that one has interactive control over
zooming, contour levels etc., so it gives more flexibility than
e.g. \task{cgdisp}.  This mode of {\tt kvis} can be used for
overlaying a radio-continuum map on an optical image, or the channels
of an HI data cube on the continuum or an optical image and inspecting
this interactively.  Note that for the contouring, the two datasets do
not have to be on the same grid, they should have a proper coordinate
system defined.  Also note that the visualisation software can read
data in {\tt FITS} format so one can load images from e.g. 
\htmladdnormallink{{\tt skyview}}{http://skview.gsfc.nasa.gov/skyview.html}
directly and overlay \miriad\ datasets.

{\tt kvis} can also produce full colour postscript output of whatever
you display.


\section{PGPLOT Device Tasks}
\label{s:pgplot}
\index{image display, pixel map}
\index{image display, contour diagrams}
\index{image display, cursor}
\index{image display, region-of-interest}
\index{cgcurs}\index{cgdisp}


There are a number of tasks that display on a PGPLOT device.  These
devices include, of course, postscript disk files that you can spool to
a laser printer (see Chapter~\ref{c:pltvcon} for details of how to use
PGPLOT).  For image display, there is a suite of tasks called
\task{cginit}, \task{cgdisp}, \task{cgcurs}, \task{cgslice}, and
\task{cgspec}.  We will discuss the first four in this chapter, and
\task{cgspec} in Chapter~\ref{c:spectra}. 

The `\verb+CG+' tasks attempt to label the plots in the correct non-linear
axes. Provided the field is not too large, the labelling is good (use
\verb+options=grid+ to see the non-linearities more clearly). However for
for very large fields or where the non-linearity is particularly severe (e.g.
south celestial pole), the `\verb+CG+' tasks will fail to label adequately.

\subsection* {Common Keywords}
\label{s:cg_common} 

This suite has many common keywords and much common functionality.  We
describe briefly now some of the common keywords, although not all the
tasks in the suite use them in all possible ways.  Refer to the
individual help files to see what each task offers.  Specific uses by
\task{cgspec} will be described in Chapter~\ref{c:spectra}.  Note that
minimum match is usually accepted for the value of keywords.  The
keywords usually default to the same value in each task, but you
should again refer to the help files for details. 

\begin{itemize}

\item The keyword {\tt in} takes a list of input images as its arguments.
These images are displayed or used in the display in some way indicated
by the keyword {\tt type}.

\item The keyword {\tt type} indicates how the images given in the {\tt
in} keyword will be displayed.  These two keywords are in one to one
correspondence with each other.  Possible values are {\tt pixel}
(display image as a pixel map representation, formerly called a grey
scale, but now full colour is available), {\tt contour} (display image
as a contour map), {\tt box} (display image as little boxes, where the
size of the box is proportional to the value of the pixel, and it is
hollow for positive and solid for negative values), {\tt amplitude} and
{\tt angle} (display these two images as line vectors).  In addition,
{\tt type} can take on the value {\tt mask}.  In this case, the relevant
image given in the keyword {\tt in} is not displayed, but its pixel
blanking mask is applied to all the other images which are being
displayed.

\item The standard \miriad\ keyword {\tt region} is used to select the
region of the images for display.  It interacts with the keyword {\tt
chan} (see below) which allows you to group planes (or channels)
together onto separate subplots.  The region selected applies to all the
input images and they must all have the same size on the first 2 axes. 
However, you can input a mix of 3-D and 2-D images.  If you input more
than one 3-D image, they must all have the same size on the third axis. 
Any 3-D region selection indicated with the {\tt region} keyword is
applied only to the 3-D images that you input.  An example of this usage
would be overlaying a continuum image on each channel of a spectral line
cube. 

\item The keyword {\tt xybin} allows you to spatially bin up (average)
pixels, or to pick out pixels at regular increments.  This can be very
handy for displaying large images, especially if you are making a hard
copy on a postscript printer where the printer resolution does not merit
retaining the individual pixels.  Using this keyword can make the
postscript file much smaller and print much faster.    

\medskip You can bin up the two spatial axes of an image independently, and
you can enter up to 4 values, 2 for each axis specifying the increment
and binning size.  As an example, {\tt xybin=4,4,3,1} would bin up the
image by 4 pixels in the \MM{x} direction and pick out every third pixel
in the \MM{y} direction.  If the binning size is not unity, it must
equal the increment. 

\item The keyword {\tt chan} is used to allow you to group the planes
(or channels) that you selected with the keyword {\tt region} onto
separate subplots of the plotting page.  The first value you give is the
channel increment to step through the image in.  The second value is the
number of channels to average, for each sub-plot.  For example, {\tt
chan=5,3} would average groups of 3 planes together, starting 5 channels
apart such as 1-3, 6-8, 11-13 and so on. 

\medskip The channels available are those designated by the {\tt region}
keyword.  A new group of channels (sub-plot) is started if there is a
discontinuity in the {\tt region} selected channels (such as {\tt
region=image(10,20),image(22,30)}.  The combination of the {\tt region}
and {\tt chan} determines how many sub-plots there will be. 

\item The keywords {\tt slev} and {\tt levs} (or {\tt lev1, lev2, levs3}
in \task{cgdisp} -- it can draw up to 3 contour maps at once) indicate at
what values the contours should be drawn.  The first value of {\tt slev}
can be {\tt p} or {\tt a}, indicating percentage or absolute levels. 
The second value of {\tt slev} is a scale factor to multiply the contour
levels given in {\tt levs} by.  For example, {\tt slev=p,1} and {\tt
levs=-1,1,2,4,8,16} would draw contours at -1\%, 1\%, 2\%, 4\%,
8\% and 16\% of the peak value in the image.  On the other hand, {\tt
slev=a,0.001}, {\tt levs=3,5,10} would draw contours at values of
0.003, 0.005, 0.01. 

\item The keyword {\tt range} is used to control the way in which pixel
intensities are mapped onto the colour lookup table of the device for
pixel map representations ({\tt type=pixel}.  Its use varies somewhat
from task to task, but the basic functionality is the same; see the
individual help files for details.   See also {\tt options=fiddle}
which provides an interactive way to interact with the lookup tables.

\medskip The first two values indicate the range of pixel values
(intensities) to map onto the lookup table.  Pixels with values outside
this range will be represented with the colour of the nearest extremum. 
Thus, if the image was being displayed with a simple linear black and
white transfer function on an interactive device, {\tt range=\MM{-0.2},
2.0} would cause all pixels with values below \MM{-0.2} to come out
black, all pixels with values greater than 2.0 to come out white, and
all pixels in between that range to have shades of grey from black to
white. 

\medskip The third argument of {\tt range} allows you to specify a
transfer function so that the pixel values can be mapped onto the lookup
table in some way other than linearly.  Allowed values are {\tt
lin,sqr,log,heq} for linear, square root, logarithmic and histogram
equalisation transfer functions.  Histogram equalisation can be very
handy for images which have a lot of dynamic range.  What this does is
use the device colour levels for pixel values which occur the most
often. 

\medskip The fourth argument of {\tt range} is an integer between 1 and
9 specifying the type of lookup table.  The available tables are 1
(b\&w), 2 (spectrum colours), 3 (linear pseudo colour), 4 (floating zero
colour contours), 5 (fixed zero colour contours), 6 (rgb), 7
(background) 8 (heat) and 9 (absolute b\&w) .  If you enter a negative
integer, then the reversed lookup table is displayed.   

\medskip Fixed zero colour contours fix a colour boundary (blue-green)
at 0 intensity, with 4 colour pairs ([light blue, light green], [dark
blue, dark green], [purple,yellow], [black,orange]) distributed positive and
negative of 0.  There are then two more colours (read and white) for the
remaining positive intensity values.  Once you have arranged the colour
pairs so that they define the noise level, the red and white colours
quickly show you the true signal.  You need to use {\tt options=fiddle}
to get the scaling to the noise level right. 

\medskip Note that in \task{cgdisp}, you can enter a group of 4 values
for each subplot that is drawn.  This is useful for hardcopy output, in
that you can have an individual scaling and lookup tables for each
subplot.  For example, you may have made a ``cube'' with unlike
quantities in different planes (total intensity, polarised intensity,
fractional polarisation, rotation measure etc) and it would be
impossible to display them all with just one set of 4 values for the
{\tt range} keyword. 

\medskip The following figure shows the possible colour table types for
a simple image.  The colour bars or wedges show the differences most
clearly. 


\begin{latexonly}
\begin{figure}
\begin{center}\epsfysize=16cm\leavevmode\epsffile{colours.eps}\end{center}
\caption{Available Colour Lookup Tables}
\end{figure}
\end{latexonly}
\begin{rawhtml}
<P><IMG ALIGN=BOTTOM ALT="" SRC="colours.gif"><P>
\end{rawhtml}


\item The keyword {\tt device} indicates the PGPLOT device upon
which the plot will be drawn.  

\item The keyword {\tt nxy} takes two values, which are the number
of subplots in the \MM{x} and \MM{y} directions on the page (dictated
by the {\tt chan} keyword).

\item The keyword {\tt labtyp} specifies what units the axes will
be labelled in.    It can take two values and they can be different.

Possible values are:

\begin{itemize}

\item {\tt hms} for a label in H M S.S (e.g. for RA)

\item {\tt dms} for a label in D M S.S (e.g. for DEC)

\item {\tt arcsec} for a label in arcsecond offsets

\item {\tt arcmin} for a label in arcminute offsets

\item {\tt absdeg} for a label in degrees

\item {\tt reldeg} for a label in degree offsets

\item {\tt abspix} for a label in pixels

\item {\tt relpix} for a label in pixel offsets

\item {\tt abskms} for a label in \kms

\item {\tt relkms} for a label in \kms\ offsets

\item {\tt absghz} for a label in GHz

\item {\tt relghz} for a label in GHz offsets

\item {\tt absnat} for a label in natural units as given
by the axis label.  

\item {\tt relnat} for a label in offset natural coordinates

\item {\tt none} for no label and no numbers or ticks on the axis

\end{itemize}

\medskip All the offsets are with respect to the reference pixel.  Note
that you are expected to match your values with the order of the axes. 
For example, if you asked for {\tt labtyp=abskms,dms}, it is expected
that the first two axes of the image are velocity and declination.  You
will get rude messages if this is not the case. 

\item The keyword {\tt csize} gives you control over the size of
characters on the plot such as axis labels, or annotation text.
These are given as factors of the default PGPLOT character
size {\tt csize=1.0}, which gives a character about 1/40 of the
maximum dimension of the view surface.  See the help files for
details on what you can control in which task.

\item The keyword {\tt options} offers a variety of enrichment options
for each task.  There are a few that are common to most
of the tasks in this suite. 

\begin{itemize}


\item Option {\tt wedge} causes a labelled colour bar or wedge to be
drawn, showing the mapping between pixel intensity and colour. 

\item Option {\tt fiddle} invokes the interactive lookup table fiddler. 
This is mouse (or keyboard) driven, and
instructions are given the in the window from which you invoked the
task.  basically, it is tree structured, with one branch to select
different colour lookup tables, and one branch to modify the transfer
function.  You can swap between these branches. 

\medskip This option allows you to cycle through all of the same colour
tables offered by keyword {\tt range} (see above), and reverse them if
desired.  You can cycle through all of the same pre-defined transfer
functions that are available with keyword {\tt range} (see above). 

\medskip In addition, there is an interactive linear transfer function
fiddle mode, where you can change the slope and offset of the linear
transfer function.  When you invoke the different transfer function
modes, you get a little plot in the bottom right corner showing you what
the transfer function is doing. 

\medskip Note that if you are plotting on a hard-copy device such as a
postscript file, this option is then activated with keyboard inputs. 
You have the usual ability to cycle through lookup tables and transfer
functions, but you do not get the interactive linear transfer function
fiddler for obvious reasons.  This gives you the same functionality as
you could have obtained with the keyword {\tt range}. 

\item Option {\tt grid} extends the coordinate ticks into a 
full grid.  Minor ticks are excluded with this option. 

\item The option {\tt unequal} instructs the software to fill the
plotting page maximally.  By default, is tries to make the scales of the
\MM{x} and \MM{y} axes the same.  It is important that you use this
option when you are displaying unlike quantities such as a velocity-DEC
diagram, as the efforts to make equal scales will go wrong here.

\item Option {\tt relax} instructs these tasks not to be concerned
if the axis descriptors for the input images are different.  The axis
sizes still have to be the same though.  Use this one with great
care, and its only useful if you are labelling the axes with
{\tt abspix} or {\tt relpix} types.

\item When you are displaying multiple subplots, with each
subplot being a different channel or plane from a 3-D image,
the options {\tt 3value} and {\tt 3pixel} instruct the software
to write the coordinate value and pixel value of the displayed
subplot in its top left hand corner.  You can use either or both
of these.

\item The option {\tt full} will cause a full set of plot
annotation to be written at the bottom of each page of the plot.
This includes things like coordinate information, contour levels,
pixel map representation intensity ranges and so on.

\end{itemize}

\end{itemize}

\subsection* {Examples}

Let us continue with some examples of usage of these tasks.

\begin{itemize}

\item \task{cginit} initialises a PGPLOT device.  Its only input is the
keyword {\tt device}.  Most interactive PGPLOT devices are ephemeral, so
that \task{cginit} is not useful for them.  However, the PGPLOT {\tt
/xs} X window is persistent, so   \task{cginit} is useful for it.

\item \task{cgdisp} can display (simultaneously) contours, pixel map
representations (i.e.  images or ``grey scales''), vectors and boxes
(pixels are displayed as filled [positive values] or hollow [negative
values] boxes with size proportional to the pixel value) on a PGPLOT
device.  Up to three contour plots, one pixel map, one vector plot and
one box plot may be displayed in multi-panel plots of multi-channel
images.  It also offers a ``mask'' image type, which is not displayed,
but its blanking mask is applied to all the images that are. 
\task{cgdisp} can also display labelled overlay locations (plotted as
boxes, stars, circles, ellipses, lines or see-through) may be specified
from an ascii text file. 

We will just give a few examples of some common things that you might
like to do with it.

\begin{enumerate}

\item In the first example, \task{cgdisp} bins an image up by 2 pixels
in the x and y directions, and then displays the image as a pixel map
representation on a persistent X window.  The transfer function wedge is
drawn to the right and then you enter the interactive cursor driven
fiddle loop.  \task{cgdisp} then draws some overlay crosses on the
plot (the overlay file format is described in the help file) and draws
the beam size in the bottom right corner of the plot.  The plot is
annotated with useful information at the bottom and all four axes are
labelled in arcsecond offsets from the reference pixel. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CGDISP}} \\
\hline 
in=cena.icln                        & Input image \\
type=pixel                          & Display as a pixel map \\
region=quarter                      & Display central quarter of image \\
xybin=2                             & Bin up image spatially \\
chan                                & Unset; no channels to bin \\
slev                                & Unset; no contours \\
levs1                               & Unset \\
levs2                               & Unset \\
levs3                               & Unset \\
range=\#,\#                         & Intensity range or unset for full range \\
vecfac                              & Unset \\
boxfac                              & Unset \\
device=/xs                          & PGPLOT device; persistent X window here \\
nxy=1                               & One plot per page \\
labtyp=arcsec,arcsec                & Label with offset arcseconds \\
options=full,beambr,wedge,fiddle,trlab& Full plot annotation, \\
				    &draw beam in BRC \\
                                    & Draw wedge, interactive fiddle \\
				    & label top and right axes too \\
lines=1,2                           & Linewidths for labels and overlays \\
break                               & Unset; no contours \\
csize=1,1,0.8                       & Set overlay character size to 0.8 \\
scale                               & Unset to fill page \\
olay=stars.txt                      & Text file containing overlays \\
\hline 
\end{tabular}
\end{center}


\item The second example contours channels in the velocity range -100
to 100~\kms\ from two 3-D images (cubes); each channel is put on a
separate sub-plot (but both cubes go on the same sub-plot) on the page
and annotated with its velocity.  Only every third pixel from the
central 40 by 60~arcsec is displayed.  The two cubes must have the same
axis descriptors; {\it i.e.} the same axis dimensions and increments
etc.  The selected channels are binned up such that five channels are
averaged together.  However, the increment between the start group of
each set of binned channels is 10.  Thus, the displayed channel width
will be one half of the displayed channel increment (they could of
course be equal if you wish).  The first cube is contoured via
percentage levels, the second via absolute levels.  The second cube's
contours are drawn with a thicker linewidth.  The two cubes could be
from different spectral-lines, for example. 

Finally, a histogram equalised pixel map representation of a continuum
image is overlaid on each channel sub-plot.  As the output file is a
disk file (a colour post-script file) we have used the {\tt range}
keyword to specify the desired lookup table (rainbow colours; fourth
argument of keyword) and transfer function (histogram equalisation; third
argument of keyword).  Alternatively, we could still have used the
interactive fiddle to do these things as the dialogue is keyboard driven
for non-interactive devices.  In fact, if we had set a transfer function
via the {\tt range} keyword and then invoked the interactive fiddle, we
could have applied another transfer function to the already histogram
equalised image ! 

The {\tt options} keyword is used to ask for an intensity wedge so that
the map of colour to intensity can be seen.  It is also used to ask for
a full coordinate grid on the plot instead of just ticks as well as to
annotate the plot with information about the images.  Finally it is also
used to ask for the value of the third axis (usually velocity or
frequency) to be written in the corner of each subplot. 

When you ask \task{cgdisp} to display a mix of 2-D and 3-D images, the
region (keyword {\tt region}) that you specify applies equally to the
first two dimensions of all images.  However, any third axis region
information that you give ({\it e.g.} {\tt region=image(10,120)})
applies only to the 3-D images, and is ignored for the 2-D image. 
Finally, if you ask for more sub-plots than can be fitted on the page,
then the task will advance to a new page (you will be prompted to hit
the carriage return key if your are plotting on an interactive device)
when needed. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CGDISP}} \\
\hline 
in=oh.cube,hcn.cube,cont.icln       & Input images; two cubes and continuum \\
type=c,c,p                          & Two contours and a pixel map  \\
region=arcsec,kms,box(-20,-30,20,30)(-100,100) & Display central 40 by 60~arcsec \\
                                    & and -100 to 100 \kms\\
xybin=3,1,3,1                       & Display every third pixel \\
chan=10,5                           & Increment is 10 channels, width is 5 channels \\
slev=p,1,a,0.05                   & Percentage and absolute levels for cubes \\
levs1=-5,5,10,30,50,80            & Percentage levels for first cube \\
levs2=-3,3,10,20,30               & Absolute levels (by 0.05 for second cube) \\
levs3                               & Unset  \\
range=0,0,heq,2                    & Ask for histogram equalised display of \\
                                    & image with rainbow lookup table \\
				    & between min and max. \\
vecfac                              & Unset; no vectors \\
boxfac                              & Unset \\
device=plot/cps                     & PGPLOT device; disk postscript file here \\
nxy=4,3                             & 4 and 3 sub-plots in x and y directions \\
labtyp=hms,dms                      & RA and DEC labelling \\
options=wedge,grid,3val,full        & Wedge, grid, velocity labels \& \\
				    & full page annotation \\
lines=1,2                           & Linewidths for cubes 1 and 2 \\
break                               & Unset \\
csize=1,0.7                         & Character sizes for axis and velocity labels \\
scale                               & Unset to fill page \\
olay                                & Unset for no overlays \\
\hline 
\end{tabular}
\end{center}

\item This third example shows how to display a position-velocity image.
You could produce such an image by reordering a cube with \task{reorder}
into velocity-x-y order.  You could then look at one or several 
planes of this cube.   Normally, \task{cgdisp} tries to display
with equal \MM{x} and \MM{y} scales.  This equality is worked out
using the linear axis descriptors.   Equal scales are meaningless
when displaying unlike axes, so you should set {\tt options=unequal}.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CGDISP}} \\
\hline 
in=neon.vxy                     & Input vxy cube \\
type=p                          & Display as a pixel map \\
region=image(1,16)              & Select first 16 planes, say \\
xybin                           & Unset for all spatial pixels \\
chan                            & Unset for all spectral pixels \\
slev                            & Unset \\
levs1                           & Unset \\
levs2                           & Unset \\
levs3                           & Unset  \\
range=\#,\#                     & Image intensity range or unset for full range \\
vecfac                          & Unset \\
boxfac                              & Unset \\
device=/xs                      & X window \\
nxy=4,4                         & 16 sub-plots on the page \\
labtyp=relkms,arcsec            & Relative velocity and arcsecond labelling \\
options=unequal                 & Unequal scales in x and y \\
lines                           & Unset \\
break                           & Unset \\
csize                           & Unset \\
scale                           & Unset \\
olay				& Unset \\
\hline 
\end{tabular}
\end{center}

\item This fourth example is a polarimetry display.  It is common
practice to display these images by way of a pixel map representation
and/or contours for the total intensity, and vectors for the
(fractional) polarised intensity at the specified position angles.  The
length of the vectors are proportional to the (fractional) polarised
intensity.  Here is an example of how to do this sort of polarimetric
plot, where in this case the total intensity is displayed as both
contours and a grey scale.  The contours are displayed as multiples of
the rms of the noise (you input the rms) in this case, although you can
also use percentage-of-the-peak contours (this is controlled by the {\tt
slev} keyword -- see the help file).  You may need to fiddle with the
keyword {\tt vecfac} which allows you to scale the lengths of the
vectors, and also specifies whether you draw a vector for each pixel or
leave some of them out.  The default is to draw a vector every second
pixel and to scale the vectors so that the longest one takes up
\MM{1/20} of the plot.  You can display any combination of the images
({\it e.g.} vectors only), but you must give both the vector position
angle and amplitude images. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CGDISP}} \\
\hline 
in=0823.fp,0823.pa0,0823.i,0823.i   & Input images, order unimportant \\
type=amp,ang,c     ,p               & Image types \\
region=arcsec,box(-30,-30,30,30)& Display central 60 by 60~arcsec region \\
xybin                               & Unset \\
chan                                & Unset \\
slev=a,\#                           & Contour levels are to be multiplied by  \\
                                    & the specified number; use the noise rms \\
levs1=-5,-3,3,5,10,30,50,100    & Contour levels \\
levs2                               & Unset \\
levs3                               & Unset \\
range=\#,\#                         & Image intensity range or unset for full range \\
vecfac=\#                           & Defaults may be OK, or may require fiddling \\
boxfac                              & Unset \\
device=plot/ps                      & Postscript file \\
nxy=1                               & One plot per page \\
labtyp=hms,dms                      & Label with RA and DEC, say \\
options=full,beambl                 & Full plot annotation, draw beam in BLC \\
lines=1,2                           & Linewidth for contours and vectors \\
break                               & Unset \\
csize                               & Unset or fiddle with \\
scale                               & Unset to fill page \\
olay                                & Unset for no overlays \\
\hline 
\end{tabular}
\end{center}

\item The final example is also polarimetry display.  Displaying
rotation measure images as a pixel map is sometimes unsatisfying, as the
sign changes, if any, can be hard to see.  Similarly, contours can be
pretty useless as the RM distribution is often very unsmooth. 
\task{cgdisp} offers the image type ``box'' whereupon each pixel is
displayed as a small box.  Positive pixel values are shown as a solid
box, and negative pixel values as a frame only.  The size of the box is
proportional to the values of the pixel, so that this method is not so
useful if you have RMs close to zero.  By default, the boxes are scaled
so that there is a little gap between adjacent boxes.  You can control
the boz sizes with the keyword {\tt boxfac} which gives an extra scale
factor to multiply the box widths.  This keyword also gives the \MM{x} and
\MM{y} increments (in pixels) across the image at which to plot the boxes. 

In this example, we also overlay contours of total intensity.  In
addition, a global blanking mask is applied to both images. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CGDISP}} \\
\hline 
in=0823.i,0823.rm,0823.msk          & Input images, order unimportant \\
type=con,box,mask                   & Image types \\
region=quarter                      & Display central quarter \\
xybin                               & Unset \\
chan                                & Unset \\
slev=a,\#                           & Contour levels are to be multiplied by  \\
                                    & the specified number; use the noise rms \\
levs1=-5,-3,3,5,10,30,50,100    & Contour levels \\
levs2                               & Unset \\
levs3                               & Unset \\
range                               & Unset \\
vecfac=\#                           & Defaults may be OK, or may require fiddling \\
boxfac=1,3,3                        & Self scale box widths, and plot boxes \\
                                    & every 3 pixels in x and y \\
device=plot/ps                      & Postscript file \\
nxy=1                               & One plot per page \\
labtyp=hms,dms                      & Label with RA and DEC, say \\
options=full,beambl                 & Full plot annotation, draw beam in BLC \\
lines=1,2                           & Linewidth for contours and vectors \\
break                               & Unset \\
csize                               & Unset or fiddle with \\
scale                               & Unset to fill page \\
olay                                & Unset for no overlays \\
\hline 
\end{tabular}
\end{center}


\end{enumerate}

\item The task {\tt cgcurs}
is similar to {\tt cgdisp} except that it is less flexible in terms of
what it can display, but it does offer an interactive capability with
the cursor.  You can read image pixel locations and values, compute
statistics over a region defined by the cursor, and define polygonal
spatial regions that can be output into a text file for use in other
tasks (using the {\tt region=@{\it file}} facility).  In addition,
\task{cgcurs} can be used to generate text files in a format suitable
(almost) for input to \task{cgdisp} and \task{cgspec} as overlay
files. 

\task{cgcurs} only displays pixel maps or contour plots, and only one
image at a time.  You can invoke all of \task{cgcurs}' cursor options in
the one run if you wish.  You can also display many channels in the same
way as \task{cgdisp}; in this case, the cursor options are invoked after
each sub-plot (channel) is drawn.  The following example shows how to
display all pixels in the x direction, but only every third pixel in the
y direction (who knows why you might do this), activate the interactive
lookup table fiddler, read some image values with the cursor, mark their
locations on the plot, and output them into a text file (called
`cgcurs.cur') for use as an overlay file in \task{cgdisp}.  In addition,
the option to evaluate some statistics in a region defined by the cursor
is also activated. 

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf CGCURS}} \\ 
\hline 
in=neon.mom0                  & Input image \\ 
type=p                       & Display as a pixel map \\ 
region=arcsec,box(-10,-10,10,10) & Display central 20 by 20 arcsec \\
xybin=1,1,3,1                 & Pick out every third y pixel \\
chan                          & Unset \\ 
slev                          & Unset \\ 
levs                          & Unset \\
range=\#,\#                   & Image intensity range or unset for full range \\ 
device=/xs                    & PGPLOT device; must be interactive \\ 
nxy=1                         & Just one plot \\ 
labtyp=arcsec,arcsec          & Offset arcsecond labelling \\ 
options=fiddle,curs,stats,mark,log,cgdisp & Fiddle, cursor and statistics options \\ 
csize                         & Unset \\ 
\hline 
\end{tabular} 
\end{center}


\item The task \task{cgslice} allows you to generate 1-D cuts (slices)
through a 2-D image and then fit Gaussians to these slices.  You can
define many slices with the cursor or input their positions via a text
file.  In addition, you can display many channels from an image, and
generate slices differently from each channel.  Like \task{cgcurs},
\task{cgslice} only displays one image (which could be 3-D of course) at
a time via a pixel map or a contour plot.   You can save the slice
locations, values and model fits in text files.  

In the example, we make slices from 9 channels of a cube, one channel at
a time.  We then fit Gaussians plus a baseline to them and output the
fits into a text file.   

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf CGSLICE}} \\ 
\hline 
in=1333.icln                  & Input image \\ 
type=p                        & Display as a pixel map \\ 
region=quarter(1,9)           & Display central quarter of \\
                              & first 9 channels \\
xybin                         & Unset \\
chan                          & Unset \\ 
slev                          & Unset \\ 
levs                          & Unset \\
range=\#,\#                   & Image intensity range or unset for full range \\ 
xrange                        & Auto x-range scaling \\
yrange                        & Auto y-range scaling \\
device=/xs                    & PGDISP server \\
nxy=3,3                       & 9 plots on the page \\
labtyp=hms,dms                & RA and DEC labelling \\
options=fit,base              & Fit gaussian plus baseline \\
csize                         & Unset \\ 
posin                         & Define slices with cursor \\
posout                        & Do not save slice locations \\
valout                        & Do not save slice values \\
modout=1333.model             & Save Gaussian fits in this file \\
\hline 
\end{tabular} 
\end{center}

\end{itemize}
