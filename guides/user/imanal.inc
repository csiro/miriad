\chapter{Image Analysis}\label{c:imageanal}\index{image analysis}


\section{Image Statistics and Histograms}\index{histo}\index{imhist}\index{imstat}\index{ellint}

\begin{itemize}

\item Task \task{histo} is a fairly commonly used task, giving a number of
image statistics and a simple histogram.  Apart from the input image
(keyword {\tt in}), it can take a region of interest ({\tt region}), a
range to determine the histogram over ({\tt range}), and the number of
bins in the histogram ({\tt nbins}). 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf HISTO}} \\
\hline
in=vela.imap		& Input image  \\
region			& Select region of interest \\
range=-0.1,1          & Range for histogram. Default is image\\
			& min and max.\\
nbins=\#                & Number of bins in the histogram \\
                        & Unset for default of 20 \\
\hline 
\end{tabular}
\end{center}

\item If you do not like \task{histo}, or you would like a PGPLOT plot of
the histogram, you could use the task \task{imhist} instead.  This task
will also optionally draw, on the histogram, a Gaussian with the same
mean, rms and integral as the actual histogram.  This task, and its
friend \task{imstat} use the {\tt options} keyword atypically to input
numeric (rather than just logical) information -- see the help file and
the example below. 

Here we plot the histogram with 20 bins with a connected line style.  We
also do not allow any values below zero to contribute to this histogram. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMHIST}} \\
\hline
in=vela.imap		& Input image  \\
region=\#               & Select region of interest \\
options=nbin,20,style,connect & Specify desired options \\
cutoff=0.0              & Discard negative pixels \\
device=/xs              & Plot on X window \\
\hline 
\end{tabular}
\end{center}

\item Complementing \task{imhist} is \task{imstat}.  This task gives you
statistics about your image in the selected region.  It computes the
statistic either over a row or a plane of data.

\begin{enumerate}

\item For example, let us look at the rms value of a cube computed over each
RA--DEC plane.  Note that it does
not matter what order the cube is in.  For example, it might be in vxy
order.  \task{imstat} will average over the xy planes regardless.


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMSTAT}} \\
\hline
in=zeeman.vcub		& Input image cube \\
region=image(50,450)    & Select region of interest \\
plot=rms                & Plot rms \\
options                 & Unset \\
axes=ra,dec             & Compute statistic over RA-DEC plane \\
cutoff                  & Unset \\
device=/xs              & Plot on X window \\
log=numbers.log         & Write numbers to log file \\
\hline 
\end{tabular}
\end{center}


\item By setting the {\tt axes} keyword to a single axis, \task{imstat}
will compute the statistic over a single row. Be warned that this
can produce a lot of output.
For example,
if you wanted to see the sum along the velocity axis
plotted as a function of RA and DEC position in
your cube you could do


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMSTAT}} \\
\hline
in=zeeman.vcub		& Input image cube \\
plot=sum                & Compute the sum. \\
axes=velocity           & Compute statistic over the velocity axis. \\
device=/xs              & \\
\hline 
\end{tabular}
\end{center}

\end{enumerate}


\item You may wish to integrate your image in concentric ellipses, thus
producing an azimuthally averaged profile.  You can do this with
\task{ellint}.  Each plane of a cube is integrated separately.  There is
no graphical output, just some tables of numbers which you can
optionally write to a log file and plot with your favourite plotting
program.  In this example, we integrate 10 circular rings from 0 to
100~arcsec centred on a pixel offset from the reference pixel by -5
and 10.3~arcsec in RA and DEC.  We also make a primary beam correction
and write the results to a log file. 


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf ELLINT}} \\
\hline
in=ngc1313.mom0      & Input image \\
region               & Full region \\
center=-5,10.3     & Offset from reference pixel \\
pa                   & Unset for face-on \\
incline              & circular rings \\
radius=0,100,10      & Radii of rings \\
options=pbcorr       & Correct for primary beam \\
log=rings.1313       & Write the results to log file \\
\hline 
\end{tabular}
\end{center}


\end{itemize}


\section{Listing Image Values}\index{imlist}

You might like to list some of the pixel values
from a section of an image.  You can do this with the task
\task{imlist}.  Because it is always very difficult to format numbers in
a way which suits every application, \task{imlist} gives you this job. 
You do this via the {\tt format} keyword, into which you put a FORTRAN
format specifier such as `1pe11.4', or `f7.2' etc.

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf IMLIST}} \\ 
\hline 
in=ic4296.icln                    & Input image \\ 
options=data                      & Display pixel values \\
region=relpix,box(-10,-10,10,10)  & Select region \\
format=f8.3                       & Select display format \\
\hline 
\end{tabular} 
\end{center}

Any pixel which is blanked will be listed as ``...'' (ellipsis).

\section{Source Fitting and Positions}\index{maxfit}\index{imfit}\index{gaufit}\index{impos}

\begin{itemize}

\item \miriad\ has a couple of tasks available for finding positions of
sources (see also \task{cgcurs} in Chapter~\ref{c:display}).  The
simplest of these is \task{maxfit}.  It fits a 2-D parabola to the image
intensities in a \TT{3}{3} pixel box centred on the brightest
pixel in a specified region and reports the fitted location and
intensity. 

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf MAXFIT}} \\ 
\hline 
in=ic4296.icln                          & Input image \\ 
region=relpix,box(-20,-20,20,20)(1,10)  & Search region \\
log=ic4296.maxfit                       & Write results to log file \\
\hline 
\end{tabular} 
\end{center}


\item If you would like to do something slightly more sophisticated than
just find a good value for the peak and location of your source, you
might like to fit a Gaussian to it with \task{imfit}.  This
task can also fit levels and disks. 

In this example, we fit a Gaussian to a region of an image by inputting
the region in a file.  The file was generated interactively by
\task{cgcurs} (see Chapter~\ref{c:display}); you delineate a polygonal
region around the source of interest with a cursor.  The quality of your
fit can improve significantly by selecting a snug region, and/or using
the \verb+clip+ keyword to keep pixel values with little signal (e.g. 
sidelobes and noise) out of the fitting process. 


\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf IMFIT}} \\ 
\hline 
in=ic4296.icln                    & Input image \\ 
region=@cgcurs.region             & Fit region \\
clip=0.1                          & Only use pixels greater than 0.1\\
				  & \ in the fitting process\\
object=gaussian			  & Fit a single gaussian\\

spar                              & Unset to let task set initial guess \\
fix                               & Unset to let everything float \\
out=ic4296.model                  & Write residuals of fit out to a dataset \\
options=residual                  & Specify residuals \\
\hline 
\end{tabular} 
\end{center}

Task \task{imfit} can fit multiple components. In this case, you
will need to set good initial estimates of the source parameters with
the \verb+spar+ keyword, and good values for \verb+region+ and \verb+clip+
become more important.


\item There is a task called \task{gaufit} for fitting Gaussians
to profiles from cubes.  You are on your own here.


\item Tasks often report positional information in absolute image pixel
coordinates, but you may often find you would like to convert to some
other units.  For example, a task has told you that there is something
of interest at a certain pixel in your image, but you would like to know
its RA, DEC, and velocity.  You can use \task{impos} to convert for you. 
This task enables you to convert coordinates from one system to another. 
The keyword {\tt type} specifies the input units of the specified
coordinate, given by the keyword {\tt coord}.  The coordinate is then
converted to all useful types (e.g.  pixel to absolute and offset
coordinate). 

Note that if you specify a coordinate in \kms\ then the keyword
{\tt stype} indicates what convention that coordinate is in (radio or
optical).  You can specify the spectral-axis coordinate in any of
frequency and the two velocity conventions, regardless of what the
header says the spectral axis is; all spectral axis conversions are done
as needed.  In addition, any spectral-axis coordinate is converted to
each of frequency, and optical and radio convention velocities (if
possible). 



See the help file for all the choices. 

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf IMPOS}} \\ 
\hline 
in=ic4296.icln                    & Input image \\ 
coord=-20.3,234.5,2400.123        & Input coordinate \\
type=arcsec,abspix,abskms        & Input coordinate types \\
stype=radio                       & Velocity is in radio conventions \\
\hline 
\end{tabular} 
\end{center}

\end{itemize}


\section{Copying, Reordering and Regridding Images}\index{imsub}\index{reorder}
\index{regrid}\index{imframe}

\begin{itemize}

\item The task \task{imsub} allows you to extract out some region of one
image and copy it to a new image.  For example, let us copy every other
pixel from the central 20 by 20~arcsec from the first 20 planes of a
cube into a new file.  Naturally, \task{imsub} can also be used to make
an identical copy of your entire image. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMSUB}} \\
\hline
in=vela.imap                           & Input image  \\
region=arcsec,box(-10,-10,10,10)(1,20) & Select region of interest \\
incr=2,2,1                             & Pixel increments \\
out=vela-sub.imap                      & Output image \\
\hline 
\end{tabular}
\end{center}

\item The task \task{imcat} allows you to concatenate images along the
third dimension.  Thus you can build cubes from 2-D images, and bigger
cubes from smaller cubes.  One use, for example, is that you might like
to put dissimilar images (e.g.  2-D images from different spectral lines)
into the one cube and make a Post-Script plot of all of them together on
one plot (each plane in its own sub-plot) with \task{cgdisp}.   

When concatenating dissimilar images, you will probably want
to set {\tt options=relax}.  This tells \task{imcat} not to be
concerned if axis descriptors differ.  However, the image
dimensions must still be the same.

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMCAT}} \\
\hline
in=*.imap                     & Input images  \\
out=big.icub                  & Output image \\
options=relax                 & Ignore header discrepancies \\
\hline
\end{tabular}
\end{center}



\item The task \task{reorder} enables you to reorder the axes of an 
image.  For example, to reorder a cube in xyv order to vxy order
and flip the direction of the velocity axis you would do the following:

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf REORDER}} \\
\hline
in=ngc253.xyv                     & Input cube \\
out=ngc253.vxy                    & Output cube \\
mode=-312                       & Reordering instructions \\
\hline 
\end{tabular}
\end{center}


\item Similar to \task{reorder} is \task{imframe}.  It too can reorder
the axes , but generally we recommend you use \task{reorder} for that
application as its inputs are much simpler than those of \task{imframe}. 
\task{imframe} is useful if you want embed an image inside a larger one
and pad the border with zeros. 

In the example, we take an image of some size and pad it out to be 512
by 512 pixels.  Note that the {\tt frame} keyword which specifies the
output image size in relative pixels is in the non-standard order of
{\tt xmin,xmax,ymin,ymax} compared to the {\tt region} keyword which
expects {\tt xmin,ymin,xmax,ymax}. 


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMFRAME}} \\
\hline
in=cena.imem                    & Input image \\
out=cena-pad.imem               & Output image \\
region                          & Unset\\
box                             & Unset \\
frame=-256,256,-255,255         & Output size in relative pixels \\
goal                            & Unset \\
\hline 
\end{tabular}
\end{center}


\item When comparing images, it is often necessary to have them
on the same grid so that tasks such as \task{cgdisp} can 
overlay them graphically, or so that \task{maths} (see below) can 
evaluate some expression involving the two images.

The task \task{regrid} enables you to take an image, and regrid it via
a cubic interpolation scheme on a new grid.  This task can regrid any
combination of the the first three axes of an image.  The new grid can
be specified in one of two ways.  The first way (and generally
easiest) is to give a template image and make the
image of interest look like the template image.  The alternative way requires
you to specify the output header axis descriptors explicitly with the
keyword {\tt desc}.  You have to input the reference value, reference
pixel, pixel increment and number of pixels for each axis you want to
regrid -- pretty tedious stuff.

Note that \task{regrid} has comparatively recently been upgraded to handle
non-linear axes correctly (older versions used a linear approximation

In this simple example, we regrid the first two axes of an image
via a template image.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf REGRID}} \\
\hline
in=0336-vla.icln          & Input image \\
tin=0336-atca.icln        & Template image \\
out=0336-vla-atca.icln    & Regridded image \\
desc                      & Unset for template \\
axes=1,2                  & Regrid first two axes only please \\
\hline 
\end{tabular}
\end{center}

\end{itemize}

Task \task{regrid} is smart enough to correctly handle different projection
geometries, conversion between equatorial and galactic coordinates, equinox
conversion between B1950 and J2000, as well as conversions between different
velocity systems (radio vs optical, LSR vs barycentric).

If the template does not have quite the geometric grid that you
want, then \task{regrid} provides a number of options to fiddle what it
treats as the template coordinate system before it does the regridding.
Option {\tt galeqsw} causes the template to be
switched from galactic to equatorial, or visa versa before the regridding
operation, whereas the option {\tt equisw} switches the equinox from
B1950 to J2000 (or visa versa). The {\tt project} keyword can be used to
reset the projection geometry of the template. The {\tt noscale} option
causes the template coordinate system's cell size {\em not} to scale
with frequency (the images made by \task{invert} are such that the
cell size in inverse proportional to frequency).

Normally \task{regrid} makes the output exactly overlay the template or
the axis descriptors. However, if you really only want them in the same
coordinate system, with potentially different image sizes and offset perhaps
by an integral number of pixels, then use \verb+options=offset+.

Task \task{regrid} can be used to resample an image on its own. You
would use this to set a 
different projection geometry, to switch between galactic and
equatorial coordinates, or to change equinoxes. If both 
the template and axis descriptor are unset, \task{regrid} uses the main
input as the template. To do this use {\tt options=offset} along with
the other options or parameters to change the template coordinate system.

There is a potential trap when regridding ATCA images to equatorial coordinates:
ATCA images produced by \task{invert} are in the so-called ``NCP'' projection.
As this projection has a singularity at the equator, it is generally 
disastrous to regrid an ATCA image which is near or on the galactic
equator without also changing the projection. The ``SIN'' (sine) projection
is a reasonable choice for this.

\section{Image Arithmetic -- MATHS}\index{maths}\index{spectral index}\label{s:maths}

The task \task{maths} evaluates a mathematical expression at each pixel
of a set of input images.  The expression (given by the {\tt exp}
keyword) is given in a FORTRAN-like syntax, using all the normal
FORTRAN operators, real-valued functions (using the FORTRAN generic,
rather than specific, names), brackets and real constants.  Logical
operators are also handled with a number zero or less being {\tt
.FALSE.}, and a positive number being interpreted as {\tt .TRUE.} (the
results of a logical expression are always zero or one).  Dataset names
take the place of FORTRAN variables, and the expression is each pixel of
the input images.  For example, \task{maths} can evaluate expressions
such as
\begin{verbatim}
      exp=sqrt(vela.qmap**2+vela.umap**2)
\end{verbatim}
This will cause \task{maths} to take the squares of the pixels in {\tt
vela.qmap} and {\tt vela.umap}, add them, and then take the square root. 

Note that when there are multiple input datasets, \task{maths} insists
that they are {\em exactly} the same size. However it does not
check whether they align, or indeed whether they use the same axis system.
Task \task{maths} does not check coordinate types.

There are some restrictions on dataset names, however. To start with,
\task{maths} treats the names {\tt x}, {\tt y} and {\tt z} as special
(see below). Also there are some 
inherent ambiguities. For
example, \task{maths} will think {\tt 1934-638} is a subtraction of two
integers (not a dataset named after a source). Avoid dataset names which
start with numerics, or those containing special characters (e.g. plus
and minus signs). Also avoid extensions like {\tt .not} (there are situations
where this can introduce ambiguities). If you follow the normal FORTRAN
or C rules for variable names, ambiguities will not arise. If you insist on
using names which could be ambiguous, you can bracket them inside angular
braces. For example, \task{maths} will interpret \verb+<1934-638>+ as a dataset
{\tt 1934-638} (not a subtraction operation).

Another useful (or indeed needed) keyword is {\tt mask}. This gives a second
expression (generally a logical expression) indicating where the
expression given by {\tt exp} is to be evaluated. For example,
\begin{verbatim}
      exp=sqrt(vela.imap)
      mask=vela.imap.gt.1e-3
\end{verbatim}
indicates to take the square root of an image {\em only} when the image
value is greater than $10^{-3}$. Note that \task{maths} believes it is
your responsibility to protect against illegal mathematical operations
(square roots or logs of negative numbers, divide by zero, etc). You must
protect any potentially dangerous operations by appropriate masking.

As mentioned above, \task{maths} treats the names {\tt x}, {\tt y} and
{\tt z} in a special manner. They are taken to represent variables that
vary linearly across the image, in the x, y and z directions respectively.
The values that they take are set with the keywords {\tt xrange},
{\tt yrange} and {\tt zrange}. Expressions can potentially be constructed
with no input datasets. In this case, a image size needs to be given
via the {\tt imsize} keyword. For example, to generate a two
dimensional Gaussian in the range \MM{[-2,2]} on both the \MM{x} and \MM{y} axes, use
\begin{verbatim}
      exp=exp(-(x**2+y**2))
      xrange=-2,2
      yrange=-2,2
      imsize=128,128
\end{verbatim}
The only remaining keywords are {\tt out} (the output dataset) and {\tt region}
(region-of-interest in the input datasets).

Here is a simple example of how to use \task{maths} to make a
spectral-index image from two total intensity images.  For example,
consider two images ({\tt i1} and {\tt i2}) at frequencies 4.8 and
8.4~GHz.  You can compute the spectral-index image ($S_{\nu} \propto
\nu^{\alpha}$) from

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf MATHS}} \\
\hline 
exp=log(i1/i2)/log(4.8/8.4) & Divide log ratios \\
mask=(i1.gt.1e-4).and.(i2.gt.1e-4) & Blank below these values \\
region              & Leave unset to do full image \\
out=4.8-8.4.spin    & Output spectral index image \\
imsize              & Leave unset \\
xrange              & Leave unset \\
yrange              & Leave unset \\
zrange              & Leave unset \\
\hline 
\end{tabular}
\end{center}

In this example, we have computed the output image
only when both the input image pixel intensities
were above 0.1~mJy/beam (this might be 5-\ssigma\ or
the like).  

Alternatively, if you were very keen, you could also get \task{maths} to
create an error image by inserting the appropriate expression (from
propagation of errors).  Then you could make the spectral index image
and blank it according to the value of the error in the spectral-index
image via the {\tt mask} keyword. 


\section{Smoothing Images}\index{convol}\index{smooth}\index{imbin}

\miriad\ offers tasks to convolve images by Gaussians (\task{convol} and
\task{smooth}) and also to convolve images by other images
(\task{convol}).  It can also bin up (or block) an image (\task{imbin}).

\begin{itemize}

\item The fastest way to convolve an image by a Gaussian is with an FFT-based
algorithm.  This is implemented in task \task{convol}.  In
the example below we convolve an image by an elliptical Gaussian.

Note that FFT based algorithms cannot correctly deal with blanked pixels.
If your image has a blanked pixels, \task{convol} pretends that
they are zero when it does the convolution. However, the corresponding
pixels are blanked in the output image.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CONVOL}} \\
\hline 
map=1331-09.icln     & Input image \\
beam                 & Unset \\
fwhm=10,20           & FWHM of Gaussian \\
pa=45                & Position angle of Gaussian \\
out=1331-09.icln2    & Convolved image \\
\hline 
\end{tabular}
\end{center}

Task generally \task{convol} does its best to scale the output pixels so
that the output image units are Jy/beam.  If you are convolving an image
which is already in Jy/beam by Gaussian, then \task{convol} needs the
beam parameters ({\tt bmaj, bmin, bpa}) to be in the dataset header to
correctly determine the scale factor, and the output effective beam
parameters.  It will give you messages about what it thinks its doing as
far as scaling factors go.  If you know better than \task{convol}, you
can give your own scale factor, via the \verb+scale+ keyword. 

Often you know the output resolution that you want, rather than the resolution
of the Gaussian that you want to convolve with. In this case using
\verb+options=final+ causes \task{convol} to treat the parameters given by the \verb+fwhm+
and \verb+pa+ keywords as the desired final resolution, and to work backwards
to determine the Gaussian that it needs to convolve with to achieve this
result. Again, \task{convol} needs to know the beam parameters ({\tt bmaj, bmin, bpa}),
and does its best to maintain the intensity units in Jy/beam.

\item If your image does in fact contain a blanking mask and it is
important that they do not take part in the convolution, then you could
convolve your image by a Gaussian (or a boxcar if you so desired) with
task \task{smooth}.  This task will be significantly slower than
\task{convol} for any but the smallest images because it operates
entirely in the image domain.

As with \task{convol}, the scale of the output image is adjusted so that
it is in units of Jy/beam -- and you can override this by setting \verb+scale+
yourself.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf SMOOTH}} \\
\hline 
in=1331-09.icln      & Input image \\
out=1331-09.icln2    & Convolved image \\
fwhm=10,20	     & FWHM of Gaussian \\
pa=45                & Position angle of Gaussian \\
options              & Unset \\
scale                & Unset for auto scaling \\
\hline 
\end{tabular}
\end{center}


\item If you wish to convolve one image by another, you should
use task \task{convol}. Again scaling is as with the above two descriptions.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CONVOL}} \\
\hline 
map=1331-09.icln      & Input image \\
beam=1331-09.icln2    & Convolving image \\
region                & Unset for full image \\
out=1331-09.convol    & Output image \\
sigma                 & Unset \\
\hline 
\end{tabular}
\end{center}


\item Finally, there is \task{imbin}; it doesn't smooth an image,
rather, it bins up (averages) an image (like the keyword {\tt xybin} in
the \task{cg} suite of programs.  You can bin up pixels, and/or pick out
every Nth pixel along any of the first three axes; this is controlled
by the keyword {\tt bin}. 

In the first example, we bin up the first three dimensions of an image by
a factor of 2 along the x axis, a factor of 4 along the y axis
and a factor of 3 along the z axis.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMBIN}} \\
\hline 
in=gc.icln            & Input image \\
region                & Unset for full image \\
bin=2,2,4,4,3,3       & Bin and increments for each axis \\
out=gc.icln-rebin     & Output image \\
\hline 
\end{tabular}
\end{center}


In the second example, we pick out every 4th pixel along
the z axis.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMBIN}} \\
\hline 
in=gc.xyv      & Input image \\
region=quarter & Only write out inner quarter \\
bin=1,1,1,1,1,4 & Pick out every 4th pixel along third axis \\
out=gc-2.xyv     & Output image \\
\hline 
\end{tabular}
\end{center}


\end{itemize}


\section{Modifying Images by Models}\label{s:imgen}\index{imgen}

The task \task{imgen} can be used to create an image with a source of
some simple model disposition.  It can generate noise, point sources,
Gaussians, elliptical disks, Bessel functions and DC terms.  It can also
be used to modify an already existing image by a model.  In the
following example, we subtract an offset (by -1.5 and 3.5~arcsec in
\MM{x} and \MM{y} from the reference pixel) circular Gaussian model from an
image.  The Gaussian parameters are given in the keyword {\tt spar}
(height 0.87~Jy, FWHM of 1.7~arcsec).  Note that \task{imgen} cannot
create elliptical Gaussians unless they align with the \MM{x} and \MM{y} axes. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMGEN}} \\
\hline 
in=ic4296.icln      & Input image \\
out=ic4296-sub.icln & Output image \\
factor=1            & Multiply input by 1 \\
imsize              & Unset \\
object=gaussian     & Gaussian model \\
spar=-0.87,1.7,1.7   & Gaussian parameters \\
xy=-1.5,3.5       & Offset from reference pixel \\
cell                & Unset \\
\hline 
\end{tabular}
\end{center}

If you wish to create an entirely new image with \task{imgen},
you should leave {\tt in} unset, and specify {\tt cell}
and {\tt imsize}.

\section{Polarimetric Analysis Tasks}

\index{impol}\index{imrm}\index{polarimetry, fractional polarisation, rotation measure}

In this section we see how to make total polarised intensity and position
angle images (task \task{impol}), fractional polarisation images,
and rotation measure images (task \task{imrm}).

\begin{itemize}

\item Task \task{impol} is used to generate images such as the
total linearly polarised intensity and position angle (in degrees North
through East; see also {\tt options=radians}) from the primary Stokes \MM{Q}
and \MM{U} images that you produced with \task{invert}.  It can also be used
to make the fractional polarisation image if you input the Stokes \MM{I}
image as well.    It is convenient to make this image with \task{impol} rather
than, say, \task{maths}, because \task{impol} can be used to
generate error images as well.

It is generally necessary to deconvolve the \MM{Q} and \MM{U} images in
the same way that you do the Stokes \MM{I} image before combining them. 
Note however, that negative values in these images are as valid as
positive values; it just depends upon the position angle of the
polarised emission.  \task{impol} can optionally (and by default) debias
the total polarised intensity image with a first order algorithm (see
help file).  A polarised intensity image is positive definite
($\sqrt{(Q^2+U2)}$), even with no signal and the intensities of its
pixels follow the Ricean distribution.  At high signal-to-noise ratios,
this is the normal distribution.  At low signal-to-noise ratios, it is
skewed biased distribution. 

The ability of \task{impol} to produce error images is very important to
assist you in assessing what is real and what is not.  These are computed
via a simple propagation of errors, and you must specify the rms level
of the input image ($\sigma_{QU}$ is also needed for debiasing the
polarised intensity).  Tasks \task{histo} (see above) or \task{cgcurs}
(see Chapter~\ref{c:display}) can give you estimates of the relevant
statistical information of an image. 

\task{impol} offers the opportunity to blank the output images based
upon the signal-to-noise ratio of the polarised intensity image (keyword
{\tt sncut}), and also on the absolute value of the position angle error
images (keyword {\tt pacut}).

Interpreting polarised intensity images below about 2-\ssigma\ is pretty
deadly because the debiasing becomes inaccurate.  In general, it is
recommended that you blank the output images in the ways provided.  You
have the choice of whether you blank the polarised intensity images (not
the position-angle image) with zeros or via a bit in a mask.  It is
important that you use zero if you are planning to evaluate statistics
over a region that encompasses the blanked pixels.  This is because zero
is generally a better estimate of the polarised intensity of a blanked
pixel (which has a small signal) than just not contributing to the sums
with that pixel. 

A case where debiasing and blanking is not appropriate is a detection
polarisation experiment.    You should ignore \task{impol}
when it implores you to blank and debias in this case.

\task{impol} can also make a plot showing the effect of bias in the
polarised intensity; see the help file for details.  You can access this
plot even if you do not input any images.  The plot is made via a Monte
Carlo simulation.  It may take a while to run so be patient; just fill
in the {\tt device} to activate this option.  For extra discussion of
debiasing and blanking see the VLA scientific memorandum no 161.  by
Patrick Leahy. 

The following example takes CLEANed \MM{Q}, \MM{U} and \MM{I} images and
their rms noise value (assumed the same for \MM{Q} and \MM{U}), and
computes images of the linearly polarised position angle, the debiased
total polarised intensity, the fractional polarisation and their
associated errors.  These images are {\em all} blanked when the error in
the position angle image is greater than 10 degrees {\em or} when the
signal-to-noise ratio in the polarised intensity image is less than three.
Note that the error image for the polarised intensity is constant (and
equal the rms of the \MM{Q} and \MM{U} images that you input) so that
there is no output polarised intensity error blanking option.  The
choice of the blanking levels depends upon your particular data; do not
treat the ones given below as being necessarily appropriate.  


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMPOL}} \\
\hline 
in=qcln,ucln,icln	& Input \MM{Q}, \MM{U} and \MM{I} images respectively  \\
poli=p,ep               & Polarised intensity and  error images \\
polm=m,em               & Fractional polarisation and error images \\
pa=pa,epa	        & Position-angle and error images \\
sigma=0.003,0.004	& Set standard deviation of noise in \MM{Q} or \MM{U} and \MM{I}in Jy/beam\\
sncut=3			& Blank below 3-\ssigma\\
pacut=10		& Blank when p.a. error greater than 10\degrees\\
options=zero		& If evaluating  area statistics \\
rm			& Leave unset \\
device		        & No bias plot \\
\hline 
\end{tabular}
\end{center}


\item The position-angle image you generated with \task{impol} is the
orientation of the electric vectors following the traversal of the
photons through the interstellar medium.  The position angle is defined
to be positive North through East.  You can be sure that some Faraday
rotation of the electric vectors will have occurred in that
magneto-ionic medium.  We are ultimately interested in the orientation
of the magnetic field before Faraday rotation.  \task{imrm}
\index{polarimetry, rotation measure image} \index{imrm} can be used to
generate rotation measure (\MM{RM}) and zero-wavelength position-angle
($\chi_0$) images by fitting the equation $\chi = \chi_0 + RM\lambda^2$
to a series of position-angle images (in degrees) at different
frequencies.  The more frequencies that you can give it the better. 

By default, \task{imrm} tries to remove $N\pi$ ambiguities with the
assumption that there are none between the angles of the {\it first two}
given images (note this has changed from an earlier algorithm).  The
details of the algorithm are given in the help file.  Basically, it
estimates the rotation measure from these two images, and then estimates
the position angle expected at the other frequencies.  It then adds or
subtracts integral multiples of $\pi$~radians to make the measured values
as close to the predicted values as possible.  Then a least squares fit
is done to solve for the rotation measure. 

You can set {\tt options=ambiguous} to turn this ambiguity removing off,
but you should read the help file carefully, and examine the results
carefully (you should do this anyway) if you do this.  In principle,
if there are no ambiguities, the ambiguity removing algorithm should not
find any.

The units of the rotation measure, \MM{RM}, and position-angle images are
\radm\ and degrees, respectively. 

The keyword {\tt rmi} is very useful.  It enables you to specify an
initial estimate of the rotation measure.  For example, you may know the
Galactic contribution in the direction of your source.  Angle
appropriate to this amount is subtracted from the data before any
attempt to compute the rotation measure is made.  If it is substantial,
it will help quite substantially to reduce ambiguity problems. 

\task{imrm} offers three blanking options for the output images (all of
the blanking criteria are applied equally to the rotation measure and
position angle images).  First, you can blank the output images if any
of the input error images exceeds a certain value.  Second, you can
blank the images if the output errors exceed a certain value, and third
you can blank the images if the goodness of fit is less than a certain
value.  You should think about what these values should be.  Remember
that you are now generating a tertiary image, and that the errors
compound rapidly.  Unfortunately, it makes no sense to blank rotation
measure and position-angle images based upon a measure of the output
signal-to-noise ratio; a rotation measure or position angle of zero does
not necessarily imply little signal.  Probably blanking with the input
position-angle errors (keyword {\tt errcut}) is the best way to blank
the output. 

You can only compute the goodness of fit if you have position angle
images at more than two frequencies, {\em and} if you input their error
images.  If it cannot be computed, the goodness of fit is assumed to be
unity so that if you have more than 2 frequencies, but do not input the
error images, the output errors are computed with this assumption
(that is, you cannot get independent estimates of the
the output errors  and the goodness of fit).  

If the goodness of fit is above about 0.1, then the fit is
believable.  If it is as low as 0.001, the fit may be believable if
you have non-normally distributed errors in the position angle images. 
If it is below 0.001, the fit should probably be rejected; this is the
default value for the keyword {\tt qcut}.  Note that the distribution of
the noise in the position angle images is indeed non-normal at low
signal-to-noise ratios.  Below $P/\sigma \approx 2$ they are seriously
non-normal (but pixels such as these should already have been blanked in
\task{impol}).  Above $P/\sigma \approx 5$, the noise is approximated by
a normal distribution.  The grey area is between 2- and 5-\ssigma. 

If you set the keyword {\tt device}, then \task{imrm} will draw some
plots showing the fits to the data.  The plots show the data that the
least squares fit was done on (i.e.  with turns removed as needed), but
with the initial estimate {\tt rmi} added back in.   Set 
{\tt options=accumulate} to put all the plots on one panel, else
use {\tt nxy} to specify the number of sub-plots per page, with
each sub-plot containing the data fit for one spatial pixel.


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMRM}} \\
\hline 
in=pa.f1,pa.f2,pa.f3,pa.f4   & Input p.a. images at different frequencies \\
inerr=epa.f1,epa.f2,epa.f3,epa.f4 & Error images associated with the p.a. images \\
rmi=35             & A priori estimate of RM \\
rm=rm              &  Output RM image \\
pa0=pa0             & Output $\lambda=0$ position-angle image \\
qcut=\#             & Blank if goodness of fit below this value \\
errcut=\#            & Blank if any input errors greater than this \\
rmcut=\#             & Blank if  \MM{RM} error greater than this \\
pacut=\#             & Blank if position angle error greater than this \\
device=/xs           & Draw plots please \\
nxy=10,10            & 100 subplots per page \\
\hline 
\end{tabular}
\end{center}


\end{itemize}


%\section{Spectral Index and Rotation Measure from MFS Images}\index{multi-frequency synthesis, rotation measure}
%\index{polarimetry, polarised flux spectral index}

%For high dynamic range multi-frequency synthesis experiments, spectral
%index information is often solved for in the deconvolution step. This is
%performed by task \task{mfclean} (see Section~\ref{s:mfclean}). The
%spectral index information is saved by \task{mfclean} as the second plane
%of the CLEAN component image (the first plane is just the regular flux density
%CLEAN components). The second plane is not actually the spectral
%index -- it is the spectral index multiplied by the source flux density.
%Additionally it needs to be convolved with a gaussian or the like, to
%de-emphasise the tendency of CLEAN algorithms at rather poor extrapolation
%of high spatial frequencies. Task \task{mfspin} can be used to go this.
%
%Generally, if you are interested in spectral indices, multi-frequency
%synthesis is {\em not} the approach you would take. Multi-frequency
%synthesis is useful only over small spreads in frequency, whereas
%spectral indices are more reliably estimated over
%larger frequency
%ranges -- the domain where \task{mfspin} is useful is somewhat limited.
%
%Given a CLEAN component image
%produced by \task{mfclean}, \task{mfspin} convolves the two planes
%with a gaussian CLEAN beam (specified explicitly via the
%{\tt fwhm} and {\tt pa} keywords, or implicitly via the {\tt beam} 
%keyword -- the dirty beam), clips them at some threshold,
%and then performs the appropriate division (flagging the output when the
%division is undefined). The clip level should be a few times the rms background
%level in the restored image. Ideally this will be a the theoretical
%noise level. 
%
%When determining the spectral index over a field which is large relative to
%the primary beam, the variation of the primary beam with frequency
%mimics a spectral index. 
%This is because the the primary beam attenuation is a function of frequency,
%However it
%is not of consequence near the pointing centre and it is a predictable
%effect.  To estimate the effect, consider a gaussian model of the
%the primary beam response, \MM{P}:
%\[
% P(\theta,\nu) = \exp\left(-a \left(\frac{\theta}{\theta_0}\right)^2 
%                 \left(\frac{\nu}{\nu_0}\right)^2\right).
%\]
%Here $\theta$ is the angular distance from the pointing centre, and
%$\theta_0$ is the primary beam FWHM at the reference frequency $\nu_0$,
%and $a = 4\log(2)$.  This will result in an apparent spectral index of
%\begin{eqnarray*}
% \alpha &=& \frac{\nu}{P}\frac{\partial P}{\partial\nu} \\
%	&=&  -8\log(2)\left(\frac{\theta}{\theta_0}\right)^2
%				\left(\frac{\nu}{\nu_0}\right)^2
%\end{eqnarray*}
%At the half-power point ($\theta = \theta_0/2$), and at the reference
%frequency, this corresponds to an apparent spectral index of -1.4.
%
%Provided the fractional bandwidth is not too large, the apparent
%spectral index will be the sum of the true source spectral index and a
%part caused by the primary beam variation.  Because the primary beam
%effect is predictable, the apparent spectral index can be corrected to
%give the true spectral index. Use the option {\tt pbcorr} to perform this
%correction.
%
%Typical inputs to \task{mfspin} are:
%\begin{center}
%\begin{tabular}{|l|l|} 
%\hline 
%\multicolumn{2}{|c|}{{\bf MFSPIN}} \\
%\hline 
%model=vela.icmp			& CLEAN component images\\
%				& produced by MFCLEAN\\
%beam=vela.beam			& Let MFSPIN fit the gaussian beam\\
%fwhm				& Leave unset \\
%pa				& Leave unset\\
%clip=0.0005			& Set at a few times the noise level\\
%options				& Leave unset for no primary beam correction,\\
%options=pbcorr			& or set to {\tt pbcorr} to correct\\
%out=vela.spin,vela.rm		& Output spectral index\\
%\hline 
%\end{tabular}
%\end{center}
%
%When imaging Stokes \MM{Q} and \MM{U}, Faraday rotation will cause a spectral
%variation. For a rotation measure \MM{RM}, speed of light \MM{c},
%intrinsic polarisation angle $\chi_0$ and total linearly polarised intensity
%\MM{P} (a real number) with spectral index $\alpha$, then \MM{Q+iU} will
%vary as
%\[
%        Q(\nu)+iU(\nu) = P(\nu_0)(\frac{\nu}{\nu_0})^\alpha
%                \exp(i2(RM\frac{c^2}{\nu^2} + \chi_0)).
%\]
%It is useful to express this variation as an
%``apparent complex spectral index'', $\alpha_C$, which is the ratio
%\begin{eqnarray*}
%\displaystyle \alpha_C &=& \frac{\nu}{Q+iU}\frac{\partial(Q+iU)}{\partial\nu}\\
%\displaystyle          &=& \alpha - 4i\frac{c^2}{\nu^2}RM.
%\end{eqnarray*}
%This quantity can be readily computed from the results of the multi-frequency
%deconvolution of \MM{Q} and \MM{U} maps.
%For a rotation measure of 30 \radm\ and frequency of 4.7 GHz, this
%results in an imaginary part of the complex spectral index of \MM{-0.49i}.
%
%Again, multi-frequency synthesis is not the ideal approach to
%determine these. To start with, a
%linear approximation is made -- certainly $\pi$ ambiguities are
%not dealt with in the rotation measure.  However, the linear approximation
%should be reasonable, provided 
%\[
%\left|\frac{\Delta\nu}{\nu_0}\frac{c^2}{\nu_0^2}RM\right| < 0.2.  
%\]
%The \MM{Q} and \MM{U} images must also have high dynamic
%range ($ > 200$). As rotation measure and the intrinsic spectral index of the
%polarised intensity are measured in a coupled manner, errors in determining one
%of them will feed into the other. Consequently if one of the two terms is
%dominant, the other term is likely to be completely corrupted by errors.
%All the same, there are some instances where useful results can be derived.
%
%To determine rotation measure and spectral index of the polarised intensity,
%you will need CLEAN
%component images of \MM{Q} and \MM{U} produced by \task{mfclean} (via the {\tt model}
%keyword). Both \MM{Q} and \MM{U} should be given.
%To get both the spectral index and the rotation measure, you
%{\em must} give two output dataset names to the {\tt out} keyword. {\em If you give
%only one name, you will only get the polarised intensity spectral index}.
%Typical inputs are:
%
%\begin{center}
%\begin{tabular}{|l|l|} 
%\hline 
%\multicolumn{2}{|c|}{{\bf MFSPIN}} \\
%\hline 
%model=vela.qcmp,vela.ucmp	& \MM{Q} and \MM{U} CLEAN component images\\
%				& produced by MFCLEAN\\
%beam=vela.beam			& Let MFSPIN fit the gaussian beam\\
%fwhm				& Leave unset\\
%pa				& Leave unset\\
%clip=0.0005			& Set at a few times the noise level\\
%out=vela.pspin,vela.rm		& Output spectral index and \\
%				& rotation measure\\
%\hline 
%\end{tabular}
%\end{center}

\section{Miscellaneous Analysis Tasks}\index{imdiff}\index{fft}\index{impoly}
\index{imheq}\index{imblr}
\begin{itemize}

\item The task \task{imdiff} can be used to compare two images.  It
finds optimum parameters (in a maximum likelihood sense) for making one
image approximate another image.  The parameters are an amplitude scale
factor, dc offset, shifts in the x and y directions and an expansion
factor.  Any of these parameters can be fixed at a given
value, and the others allowed to vary. 

Note that this task is suited only to small shifts, expansions
and scale factors.
You can use the self-explanatory task \task{shifty} for large
integral pixel shifts.   The keyword {\tt in2} gives the image
which must be adjusted to look like the image given by keyword {\tt in1}.
Task \task{imdiff} can also output a residual image -- see the help
file for the definition of the residual, but it is basically
a combination of all the residuals of all the adjustable parameters.

To hold any particular parameter fixed, you simply specify the
appropriate {\tt options} string (see help file).  Otherwise,
\task{imdiff} will solve for that parameter.  Initial guesses can be
entered through the keyword for each parameter. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMDIFF}} \\
\hline 
in1=vela.atca      & Template image \\
in2=vela.vla       & Adjust this image \\
adjust=vela.vla-atca & Adjusted image \\
resid=vela.resid      & The residual image \\
region               & Unset for entire image \\
guard                & Unset for default OK \\
xshift               & Unset for initial guess of 0 \\
yshift               & Unset for initial guess of 0 \\
expand               & Unset for initial guess of 1 \\
amp                  & Unset for initial guess of 1 \\
offset               & Unset for initial guess of 0 \\
options=expand       & Unset to vary everything and x \\
                     & and y expansions separately \\
\hline 
\end{tabular}
\end{center}


\item  A very useful tool for diagnosing errors in images is
to Fourier Transform them.  The error may, under some conditions,
be easier to recognise in the Fourier plane.  \miriad\ offers
the task \task{fft} for this purpose.  It can Fourier Transform
real or complex images (you input there real and imaginary
parts with separate keywords).    Note that blanked pixels
in the input are treated as if their value was zero.

In the following example, we FFT a CLEANed image and form the amplitude
and phase images of the Fourier Transform. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf FFT}} \\
\hline 
rin=ic4296.icln      & Input real image \\
iin                  & Unset \\
sign=+1              & Forwards transform \\
center               & Use ref. pix. for centre of transform \\
rout                 & Unset \\
iout                 & Unset \\
mag=fftp.amp         & Amplitude image \\
phase=fft.phase      & Phase image \\
\hline 
\end{tabular}
\end{center}

Task \task{fft} produces gridded images,  so you can use all the usual
display tools to look at them (see Chapter~\ref{c:display}).


\item For those of you wrestling with optical data in \miriad, the task
\task{impoly} may be of benefit.  You may like to make a polynomial sky
subtraction.  \task{impoly} allows you to define a region over which a
2-D polynomial is fit.  The fit is the subtracted from the the full
image, not just the designated fit region. 

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMPOLY}} \\
\hline
in=cluster.deep      & Input image \\
out=cluster-sub.deep  & Output image \\
order=\#              & Order of polynomial \\
coeffs                & Unset not to see coefficients of fit \\
region=@cgcurs.region & Fit over this region \\
\hline
\end{tabular}
\end{center}

\item As is discussed in Chapter~\ref{c:display}, there are a variety
of transfer functions that you can apply to your image with the
display tasks.   One that is particularly useful is histogram
equalisation.   Essentially, this method assesses, via a histogram,
which image intensities have the most pixels.  It then optimally expends the
dynamic range of the device in displaying these pixels, rather than
the pixels which have intensities that occur seldom.  

It may be that you would like the image to be written out with histogram
equalisation applied, rather than it just being computed but not saved
by the display task.  The task \task{imheq} will do this for you.
You can specify the intensity range in which pixels will be included
when computing the histogram.  It is useful to use this if you
have outliers significantly from the bulk of the distribution.
\task{imheq} will equalised each plane of a cube, and by default, use
the image minimum and maximum of that plane.  If you set 
{\tt options=global} it will take the minimum and maximum from the whole
cube instead.

If you set the {\tt device} keyword, \task{imheq} will draw a plot
of the image histogram and discretized cumulative histogram (see
help file).  The latter is essentially the transfer function
that is applied.   This plot is drawn after every plane of
a cube.

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMHEQ}} \\
\hline
in=cluster.deep      & Input image \\
out=cluster-heq.deep & Output image \\
nbins                & Default for 128 histogram bins \\
range                & Default is image min to max \\
options              & Unset for plane by plane equalisation \\
device=/xs           & PGPLOT device for histogram \\
\hline
\end{tabular}
\end{center}


\item It is sometimes useful to replace the value of all image pixels
which are blanked by some number (often zero). Task \task{imblr}
offers this facility; it unblanks all blanked pixels and
replaces their value with the one specified. 

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMBLR}} \\
\hline
in=test.m1      & Input image \\
out=test-2.m1   & Output image \\
value=0.0       & Replacement value \\
\hline
\end{tabular}
\end{center}


\end{itemize}
