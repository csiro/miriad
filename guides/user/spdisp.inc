\chapter{Displaying Spectral Cubes}\label{c:spectra}\index{spectral line, display}

\section{Introduction}
For sophisticated
visualisation software, you are recommended to use the ATNF visualisation
suite (briefly described below).
On the other hand, \miriad\ contains tasks to display simple
spectra (\task{imspec}
and \task{imspect}) or spectra overlayed on an image (\task{cgspec}).

\input{viz.inc}

\section{Displaying spectra only}\index{imspect}\index{imspec}

The tasks we discuss here allow you to make a line plot plot of a
single spectrum averaged over some spatial region from a cube.  There
are two rather similar tasks with rather similar names; \task{imspect}
and \task{imspec}.  The latter offers more options, the former is easier
to drive if you want something basic.  We will give examples of both. 

\begin{itemize}

\item We begin with \task{imspect}.  The input cube can be in xyv or vxy
order.  This example plots a 3-point Hanning smoothed spectrum averaged
over a small spatial region centred on the reference pixel. 

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf IMSPECT}} \\ 
\hline 
in=neon.xyv                   & Input cube \\
region=relpix,box(-2,-3,2,3)  & Spatial region to average over \\
xaxis                         & Default to label x-axis in natural\\
                              & spectral units (\kms\ or GHz) \\
yaxis=average                 & Average over spatial region \\
yrange                        & Auto-scale \\
hann=3                        & Hanning smooth \\
options                       & Unset \\
device=plot/ps                & PGPLOT device \\
log                           & Unset; use to write spectrum to text file \\
comment                       & Unset \\
\hline 
\end{tabular} 
\end{center}


Note that \task{imspect} can also be used to plot the derivative
of a spectrum ({\tt options=1der} or {\tt options=2der}) which
may be useful for Zeeman enthusiasts.

Now for \task{imspec}, which offers wider flexibility than
\task{imspect}.  It also plots spectra from a cube, but the cube can be
in any order.  It offers options to average or sum the data in RA-DEC,
RA-VEL and DEC-VEL planes (keyword {\tt axes} -- see the help file for
details) so that you get profiles along the velocity, DEC, and RA planes
respectively.  You can also convert the units from Jy to brightness
temperature and write the spectrum to a text file.  \task{imspec} offers
Hanning and boxcar spectral smoothing as well as the derivative options
available in \task{imspect}.  You can also specify a cut-off level below
which data do not contribute to the spectrum and convert from Jy/beam to
Jy. 

Note that the region you specify with \task{imspec} is limited to
rectangular boxes only.   The following example takes a cube
in xyv order, computes the spectrum from channels 1 to 256 averaged
over the central 4 by 6~arcsec, and then 3-point Hanning smooths it
before displaying it on the local X-window.

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf IMSPEC}} \\ 
\hline 
in=neon.xyv                   & Input cube \\
region=relpix,box(-2,-3,2,3)(1,256)  & Region of interest \\
plot=average                  & Plot average \\
options=hann,3                & Hanning smooth \\
cutoff                        & Unset \\
beam                          & Unset \\
axes=ra,dec                   & Average over RA and DEC axes \\
device=/xs                    & PGPLOT device \\
log                           & Unset \\
\hline 
\end{tabular} 
\end{center}
\end{itemize}


\section{Overlaying spectra on images}\index{cgspec}

The final task in the {\tt cg} suite is \task{cgspec}.  It displays both
images and spectra, but its primary aim is spectral.    You are referred
to Section~\ref{s:cg_common} for extensive discussion of the many common
keywords in the {\tt cg} suite of software.

\task{cgspec} allows you to display an image, and then overlay small
plots of spectra at different locations on the image.  You can input up
to five different 3-D images (cubes) from which the spectra are
extracted ({\it e.g.} each one representing a different spectral-line),
as well as up to three contour images and one pixel map representation
image.  On colour devices, the spectra from different cubes are plotted
in different colours.  The spectral cubes can be of any size and be in
any order.  Neither do the pixel increments have to be the same between
different cubes.  However, the 2-D contour and pixel map images do have
to have the same axis descriptors.  The locations at which the spectra
are drawn can be either on a regular grid or irregularly spaced at
locations specified in a text file.  In addition, you can bin the
spectra up over some spatial region centred on the given locations. 

\task{cgspec} uses the value {\tt spectrum} for the keyword {\tt type}
to indicate that spectra are to be extracted from that image listed in
the keyword {\tt in}.  For Zeeman enthusiasts, you can also plot the
derivative of a spectrum image (with keyword {\tt type=dspectrum}).  See
the help file for details. 

The following example shows how to overlay spectra at irregular
locations from two cubes (which happened to have different axis orders)
on a grey scale.  Because spectra from different lines rarely have a
similar range of intensities, you can use the keyword {\tt iscale} to
scale the spectra from each cube by some number to bring them into the
same range.  Alternatively, you can normalise all spectra to a peak of
1.0 ({\tt options=normalize}).  You may need to specify the size of the
spectra as a fraction of the plot window taken up by the spatial image
({\it i.e.} the grey scale and contour plots), as the defaults of 0.1
may not be suitable.  

The only keyword amongst all the scaling keywords that there is no
default for is {\tt stick}.  This specifies the major tick mark
increments on the velocity and intensity axes for the spectra; it is
pretty much impossible to guess at a default. 

There are a variety of choices about to how you draw the spectra. 
You can draw labelled borders around them or not ({\tt options=frame}),
you can mark their spatial location with a cross or not ({\tt
options=mark}; it can be hard to know sometime exactly which pixel the
spectrum refers to), you can draw and label the \MM{x=0} and \MM{y=0} axes or
not ({\tt options=noaxes}), you can choose any of these axis options
but not put any numeric labels on them ({\tt options=naked}) as
things can get pretty cluttered, and you can write a spectrum `number'
in the corner of each spectrum ({\tt options=number}) which reflects 
the numeric location of the spectrum in the text file or the automatically
generated grid.  There are a few more options too, so see the help file
for more details.

\begin{center} 
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|} {{\bf CGSPEC}} \\ 
\hline 
in=oh.xyv,6cm.icln,hcn.vxy    & Input images \\ 
type=s,g,s                    & Images are spectrum, grey, spectrum \\
region=quarter                & Display central quarter of grey scale \\
xybin                         & Unset for no spatial binning \\
slev                          & Unset; no contours \\
levs1                         & Unset \\
levs2                         & Unset \\
levs3                         & Unset \\
grange=\#                     & Grey scale range or unset for auto-scale \\ 
vrange=\#                     & Velocity range or unset for auto-scale \\
irange=\#                     & Spectrum intensity range or unset for auto-scale \\
iscale=\#                     & Specify a scale factor for each cube or leave unset \\
spsize=\#                     & Specify size of each spectrum. Default may be OK \\
stick=100,0.5                 & Ticks at 100~\kms\ and 0.5 in intensity \\
device=/xd                    & PGDISP server \\
labtyp=arcsec,arcsec          & Offset arcsecond labelling for images \\ 
options=mark,norm,num         & Mark location, normalize and number spectra \\
clines                        & Unset \\
slines                        & Unset; set linewidths for spectra on non-colour devices \\
blines                        & Unset; set linewidths for axes on non-colour devices \\
break                         & Unset \\
csize                         & Unset; character sizes of axes may need fiddling \\
olay=locations.spc            & Specify locations of spectra in this file; see help \\
\hline 
\end{tabular} 
\end{center}

