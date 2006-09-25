\chapter{Primary Beams and Mosaicing}\label{c:mosaicing}\index{mosaicing}

\section{Primary Beams and Primary Beam Correction}\index{primary beam, models}
\index{primary beam, correction}

In interferometry, the image formed normally by the imaging and
deconvolution steps is a representation of the sky {\em multiplied
by the primary beam response} of the antennas. The primary beam is
typically similar to a Gaussian function, although it also has sidelobes.

\miriad\ tasks which require knowledge of the primary beam response of a
telescope use built-in models of the responses of various telescopes
(e.g. ATCA, VLA, Hat Creek, WSRT) -- the primary beam model used is
determined by the `\verb+pbtype+' or `\verb+telescop+' item or variable
(\verb+pbtype+, if present, takes precedence over \verb+telescop+).
Currently these models assume the primary beam is circularly symmetric and time
independent. The task \task{pbplot} produces some information and can
make a plot of the primary beam models. The model of the ATCA primary
beam is described in \htmladdnormallink{Mark Wieringa \& Mike Kesteven's memo}
{http://www.atnf.csiro.au/observers/memos/d97f7b\~{}1.pdf} (AT Memo 39.3/024).

If you wish to override \miriad's model, or if \miriad\ does not have a
model of the telescope of interest, you may set the primary beam
associated with an image or visibility dataset to be a particular
type or Gaussian of a
given size. This is done by setting the \verb+pbtype+ item using
\task{puthd}. \task{pbplot} (without any additional parameter) produces
a listing of the primary beam types known to \miriad.

Setting a Gaussian primary beam type differs from setting
other primary beams in that you also must give an additional parameter
enclosed in brackets) giving the 
FWHM of the primary beam, in arc
seconds, at the reference frequency (the `reference frequency' for a
visibility dataset is the frequency of the first channel imaged!!). For
example, to set the primary beam to be a Gaussian of the image \verb+lmc.map+, use:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf PUTHD}}\\
\hline
in=lmc.map/pbtype& Set \verb+pbfwhm+ of \verb+lmc.map+.\\
value=gaus(1200)	 & Set the primary beam FWHM to 20 arcmin (1200 arcsec). \\
\hline
\end{tabular}
\end{center}

Although there are a few exceptions (e.g. \task{mfspin} and \task{ellint}),
\miriad's analysis tasks do not correct for primary beam attenuation
automatically. The task to correct an image for the primary beam
is \task{linmos}. To use \task{linmos} for this function, you need only
set the input and output dataset names. For example

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf LINMOS}}\\
\hline
in=lmc.map	& Image to primary beam correct.\\
out=lmc.pbcorr	& Output, corrected, image.\\
options		& Leave unset.\\
\hline
\end{tabular}
\end{center}

\section{Mosaicing}\index{mosaicing}

The primary beam limits the size of an object that we can observe with a
conventional experiment. To circumvent this, a large object can be
observed using multiple pointings -- this is the practise known as
mosaicing. In interferometry, mosaicing is not simply the practise of
pasting together multiple tiles of the sky. In interferometry, the
adjacent pointings are not independent, and so we can get fundamentally
better images by processing the different pointings together. This is
particularly so for extended emission or when the signal-to-noise ratio
is low. A description of the theory behind mosaicing can be found in
the NRAO Synthesis Imaging Summer School (Lecture 15 -- Wide Field
Imaging III: Mosaicing -- by Tim Cornwell). Other notable references
are \htmladdnormallink{Cornwell (1988)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1988A\%26A...202..316C}
(A\&A, 143, 77)
and  \htmladdnormallink{Cornwell, Holdaway \& Uson (1993)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1993A\%26A...271..697C}
(A\&A, 271, 697).
\htmladdnormallink{Sault, Staveley-Smith and Brouw (1996)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1996A\%26AS..120..375S}
(A\&AS, 120, 376) describe some of the algorithms used by \miriad.

\section{Mosaicing Observing Strategies}\label{s:mosobs}\index{mosaicing, observing strategies}
The job of planning a mosaic experiment requires extra thought over
a simple conventional observation. Issues that you must decide in
the planning of an experiment include:

\begin{itemize}

\item Pointing grid pattern: In a mosaic experiment, you observe
a number of pointings -- possibly a few to several hundred, depending
on the size of the source of interest. To consider how dense the
sampling grid needs to be, consider the primary beam of an antenna.
In the \MM{u-v} plane, the Fourier transform
of the primary beam pattern is just the cross-correlation between
two antenna illumination patterns. For the 22-m ATCA dishes, the
Fourier transform of the primary beam pattern will be of finite and circular
extent, having a diameter of 44-m. Because it is of finite extent,
Nyquist's sampling theorem indicates that, provided we do not sample
in the sky domain coarser than some limit (i.e. provided the pointing
grid pattern is sufficiently fine), all information can be retrieved.
Assuming a standard, rectangular grid, the sky plane Nyquist sampling limit is
\[
\theta_{\rm rect} = \frac{\lambda}{2D}
\]
($\lambda$ is the wavelength, and \MM{D} is the dish diameter). For a
well-illuminated dish, this spacing corresponds roughly to
half-power point spacing between field centres. Because
the extent of the transform is circular, we can do somewhat better than
this, by using a so-called hexagonal grid. This grid places pointing
centres at the vertices of equilateral triangles -- packing six triangles
together gives a hexagon. An extension of Nyquist's theorem indicates that
\[
\theta_{\rm hex} = \frac{2}{\sqrt{3}}\frac{\lambda}{2D}.
\]
So a hexagonal grid allows a given area of the sky to be covered
in a smaller number of pointings (it does also require slightly longer drive
times between pointings -- see below -- which may occasionally be a
consideration).
Table~\ref{t:mosgrid} gives this
grid spacing for ATCA dishes.
\begin{table}
\caption{Mosaic grid spacing for ATCA dishes}
\label{t:mosgrid}
\begin{center}
\begin{tabular}{|c|c|}\hline
Frequency&Pointing Spacing\\
$\nu$ (GHz) &$\theta_{\rm hex}$ (arcmin) \\ \hline
1.384& 19.6\\
2.496& 10.9\\
4.800&5.6\\
8.640&3.1\\ \hline
\end{tabular}
\end{center}
\end{table}

\item Dwell time: Most mosaiced experiments will continually switch
between the different pointing centres (or a subset of them, if there
are too many pointing centres to visit in a single observation). 
Normally they will be visited in a raster scan fashion.  Switching to a
new pointing centre typically results in 0.5 to 4 seconds of `lost' time
while the antennas are slewing to the new pointing.  This time can be a
significant consideration in some experiments -- e.g.  if the
integration time was 10 seconds, and the pointing centre was switched
every integration, up to about 40\% of the observing time could be lost. 
To avoid this, you will want to dwell on a given pointing centre for as
long as reasonable.  This must, however, be traded against loss of
tangential \MM{u-v} coverage that occurs when each pointing is not
visited sufficiently frequently.  To determine the balance, recall that
a correlation does not measure the value of a single point in the
\MM{u-v} plane, but a region corresponding to twice the diameter of the
dishes.  At transit (when the projected baselines are changing fastest),
the time taken for a baseline to rotate to a completely independent
visibility point is

\[
\tau = \frac{86400}{2\pi}\frac{2D}{L} \hbox{\rm\ seconds}
\]
Here \MM{L} is the maximum baseline length of interest when imaging
and \MM{D} is the dish diameter.
Ideally you will want to sample twice as frequently as this, i.e. for
\MM{N} pointings, a dwell
time of $\tau/2N$ would be best. You may, however,
decide to suffer tangential holes in the \MM{u-v} coverage.

\item Field Naming Convention:\label{s:fnc} When preparing the observe files for an
ATCA  mosaic experiment, you
will create a `mosaic file'. This gives a field
offset, integration time and field name for each pointing centre. To
simplify a step in the reduction process (the splitting step only), it is 
recommended that field names
be composed of two parts, separated by an underscore character. This recommendation
is purely to simplify some steps in the data reduction in \miriad. The first
part should be common to all fields. Typically this will be the
name of the object being mosaiced. The
second part is unique to each field, typically being a field number. For
example, the field name for pointing 123 for a Large Magellanic Cloud
mosaic would be called \verb+lmc_123+.
\end{itemize}

\section{Visibility Processing}\index{mosaicing, calibration}
The flagging, splitting and calibration of a mosaic experiment
are rather similar to a conventional experiment. The following
concentrates on the differences only. This section will assume that,
as is usual, only a single phase calibrator is used for all pointings.

\begin{itemize}
\item {\em Flagging:}  Flagging differs only in that, when the instrument
is continually changing to a new pointing centre, it can be more difficult
to spot the outliers which indicate bad data.

In its processing, \task{tvflag} usually produces a gap in its display
every time the source being observed is changed. When mosaicing, this
can very quickly mount up to taking the entire display (and \task{tvflag}
will complain about not being able to determine an appropriate averaging
interval). In this case, \verb+options=nosrc+ can be used to suppress the
gap and allow \task{tvflag} to average across source changes.
\item {\em Splitting:} As with conventional reduction,
for calibration and imaging purposes, it is easiest in \miriad\
to split the data into datasets containing a `single source' and
single frequency band. For a mosaic experiment, all the pointings
from the object of interest should be considered a `single source'. As
the ATCA on-line system labels each pointing with a different field name,
\task{uvsplit} would normally break
the dataset into one file per pointing. To avoid this, use \task{uvsplit}'s
mosaic option. This causes \task{uvsplit} to use the source name up to
any underscore character. Thus, assuming you have used
the field naming convention of Section~\ref{s:mosobs}, \task{uvsplit}
will not split apart all the separate pointings.

If you failed to follow the naming convention, the \verb+select+ keyword of
\task{uvaver} and \task{uvsplit} can be used to split the dataset up. This
is left as an exercise for the reader.

For example, assuming a dataset containing calibrators and an observation
of the LMC (with appropriately named fields), the following inputs will
generate datasets for the calibrators and a single dataset for all the LMC
data. 
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf UVSPLIT}}\\
\hline
vis=mosaic.uv	& The input dataset.\\
options=mosaic	& Assume mosaic experiment, so create \\
		& multi-pointing output where necessary.\\
\hline
\end{tabular}
\end{center}

\item {\em Data Selection:} With mosaic visibility datasets, the \verb+select=ra+ 
and \verb+select=dec+ selection sub-commands can be useful to extract or
manipulate a subset of the visibility data.
\item {\em Calibration:} Determining the calibration solutions does not differ from that
described in Chapter~\ref{c:polarimetry}. You will then use \task{gpcopy}
to copy the calibration tables to
the multi-pointing dataset.
\end{itemize}

\section{Summary of Imaging Strategies}
There are two quite distinct methods for reducing a mosaic experiment -- the
so-called ``joint'' and ``individual'' approaches. Although hybrid approaches
are also conceivable, they will not be discussed.

The joint approach, which is the simplest, takes advantage of \miriad's
mosaicing software.
In this case, all pointings are handled simultaneously by the imaging
and deconvolution software.
With the individual approach, a mosaic experiment is
treated like a large
number of conventional observations, where each pointing is
imaged and deconvolved separately. In this case you, the user, are 
responsible for keeping track of all the pointings. Only as a final
step are all the pointings pieced together.

The advantage of the joint approach is speed and simplicity.  Also
because the deconvolution of all the pointings is done together, it can
produce fundamentally better deconvolutions.  This is particularly so
for low signal-to-noise ratio mosaics and for extended emission
(emission comparable in extent to the primary beam -- see Cornwell's
papers for the argument).  It is the approach normally used.  However
there are disadvantages -- the joint approach depends more critically on
the model of the primary beam.  Errors in the model of the primary beam
will tend to be amplified by this approach, particularly when the
\MM{u-v} coverage is poor.  Generally the joint approach will be limited
to dynamic ranges of several hundred or so. 

A more practical difference between the two approaches is that the joint
approach generally uses significantly less disk space (this can be an
order of magnitude or more for spectral line experiments). However,
because the joint
approach does all pointings simultaneously, it does use significantly more
computer memory in its reduction steps. With current computers, the joint
approach is not possible for full resolution ATCA images (6 km array) if
you have more that a few pointings.

\section{The Joint Approach}\index{mosaicing, joint approach}
\label{s:mosjoint}
For the joint approach, the reduction proceeds in a fashion which appears
very similar to conventional observations. First a dirty image is formed
(with associated point-spread function), and then a deconvolution algorithm
is used to `clean' this dirty image. Finally the `restore' step is performed.
There are, however, substantial differences -- although these are largely
hidden from the user.

The task to form the dirty image is still \task{invert}.  The dirty
image is formed by imaging (using a conventional algorithm) each of the
pointings separately.  These individual pointing images are them
combined in a linear mosaicing process.  This linear mosaicing simply
consists of a weighted average of the pixels in the individual
pointings, with the weights determined by the primary beam
response and the expected noise level.  The resultant output dirty image
is thus an image of the entire region mosaiced. 

The weights are computed to minimise the noise in the resultant image as well
as to correct for primary beam attenuation. The output image, \MM{I(\elll,m)},
is given by
\[
I(\ell,m) = g(\ell,m) \frac{\sum_i P(\ell-\ell_i,m-m_i)I_i(\ell,m)/\sigma_i^2}
		 {\sum_i P^2(\ell-\ell_i,m-m_i)/\sigma_i^2}.
\]
Here the summation, \MM{i}, is over the set of pointing centres, 
$(\ell_i,m_i)$. $I_i(\ell,m)$ is the image formed from the \MM{i}'th pointing,
and \MM{P(\elll,m)} is the primary beam pattern. The expected noise
variance in the \MM{i}'th field is $\sigma_i^2$.

Primary beam attenuation is only corrected for within limits.
Because there are large variations in sensitivity across a mosaiced
image (the edges of a mosaiced region will have low sensitivity), the
imaging software does not always attempt to fully correct for primary
beam attenuation.  Instead, it constrains the weights
so that the noise level does not exceed a certain limit (this
limit is based on the noise in individual pointings). This
results in some residual primary beam attenuation at the edges of
a mosaic (or in holes in the pointing grid). This is
done by the function \MM{g(\elll,m)}. This function normally has a
value of 1, but its value drops towards 0 at edges or holes.
In this way, the noise level across a mosaiced image is crudely uniform.

Task \task{invert} also applies geometric corrections to account for the fact 
that the sky is not a plane.
For an east-west array, such as the ATCA, these corrections are exact,
meaning that the coordinate geometry of the resultant images is also
(nominally) exact. For other array types, the corrections are optimal in
the sense that they are the best approximation that still results in
a convolution relationship (in the sense that such arrays obey a
convolution relationship!).

Because the \MM{u-v} coverage of the different pointings will not
be identical, the synthesised beam patterns will differ between pointings. This,
and the weighted average process, means that the point-spread function
of the resultant dirty image is position-dependent. As most
deconvolution algorithms assume a position-independent point-spread
function, a conventional
algorithm cannot be used. However the point-spread function from
the linear mosaicing process is still reasonably compactly described and
readily computed. The
beam dataset that \task{invert} produces is not a normal one; it is
a cube of beam patterns,
one for each pointing. Given this, and some information stored in an
auxiliary mosaic information table, the deconvolution tasks can
compute the true point-spread function at any position in the dirty
image. Being able to compute a point-spread function (or rather,
being able to compute a dirty
image, given a trial deconvolved image) is the difficult part of
writing a deconvolution task. A maximum entropy-based deconvolution
algorithm is readily implemented.

The practicalities of this processing are now described.

\subsection*{Imaging -- INVERT}\index{mosaicing, imaging}
Most of the inputs to \task{invert} are the same as with conventional
imaging. Only mosaic-specific considerations will be mentioned here.
See Chapter~\ref{c:imaging} for more information. Note that \task{invert}
supports multi-frequency synthesis, Stokes and spectral imaging.
\begin{itemize}
\item {\tt options=mosaic}: The most important thing to remember is
to invoke \task{invert}'s mosaic mode! This causes \task{invert} to
expect multiple pointings in the input visibility data, and to
perform the linear mosaicing steps and geometric corrections.
\item {\tt options=double}: If you intend to deconvolve, {\tt options=double}
should always be used. This is because the full field of the each
individual pointing is potentially filled with emission.
\item {\tt vis}: When mosaicing, \task{invert} handles input datasets
which contain multiple pointing centres.
\item {\tt imsize}: In mosaic mode, this is interpreted as the image
size, in pixels, of each subfield. There are two constraints that
are important if you wish to deconvolve. These can be relaxed, with
corresponding degradation in deconvolution.
\begin{itemize}
\item Ideally \verb+imsize+ should be large enough to contain all emission
in the main lobe of the primary beam (as \miriad\ only models the main
lobe, making it larger has no beneficial effect).
Table~\ref{t:mossize} gives these
sizes as a function of frequency for the ATCA. 
\begin{table}
\caption{Size of Main Lobe of the Primary Beam for ATCA dishes}
\label{t:mossize}
\begin{center}
\begin{tabular}{|c|c|}\hline
Frequency&Primary Beam Main Lobe Size\\
$\nu$ (GHz) &$\theta$ (arcmin) \\ \hline
1.384&70.0\\
2.496&42.4\\
4.800&20.6\\
8.640&12.2\\ \hline
\end{tabular}
\end{center}
\end{table}

\item The image size {\em should not} be a power of 2, or
a number within the range (approximately) $[0.9\times2^n,2^n]$ (note that
just 1 pixel more than a power of 2 is fine -- and indeed good). This
restriction is to help reduce the effects of the aliasing caused by
a ``grid-and-FFT'' imaging algorithm which \task{invert} uses.
\end{itemize}
\item {\tt sup}: Recall that \verb+sup+ gives the sidelobe
suppression region, and is a way of setting uniform, super-uniform and
sub-uniform weights. In mosaic mode, \task{invert}'s default is to suppress
the sidelobes in a region the size of each sub-image. However suppressing
sidelobes in an individual sub-images is not the same as suppressing them
in the linear mosaic of the sub-images. This is because
the linear mosaicing process weights down sidelobes
some distance from the pointing centre.
In mosaicing, minimum sidelobes
will be achieved by setting {\tt sup} equal to about twice the pointing
separation (which should be about the side of the primary beam FWHM).

As always, best point-source sensitivity results by setting {\tt sup}
equal to 0.
\item {\tt offset}: This has a different meaning in mosaic mode. It gives the
position on the sky (the so-called tangent point), in RA and DEC, which is
used for
geometry calculations. The value is given in the form {\it hh:mm:ss,dd:mm:ss},
or as decimal hours and degrees. Normally you can allow this to default, and
\task{invert} will choose a central pointing centre as the tangent point.
\end{itemize}
Typical inputs to \task{invert} would be:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf INVERT}}\\
\hline
vis=lmc.uv	& The input multi-pointing dataset.\\
options=mosaic,double	& Use mosaic mode and make large beam.\\
offset		& Usually can leave blank.\\
map=lmc.map & Output image name.\\
beam=lmc.beam & Output beam name. \\
cell=& Set cell size.\\
imsize=& Set output image size. \\
sup=2000& Set to approximately twice the pointing separation for\\
	& \ \ minimum sidelobes, or\\
sup=0	& \ \ set to zero for best point-source sensitivity.\\
\hline
\end{tabular}
\end{center}

\subsection*{Deconvolution and Restoration}\index{mosaicing, joint deconvolution}
\label{s:mosdeconv}
\miriad\ contains two tasks to deconvolve
the mosaiced dirty images produced by \task{invert}.
In terms of theory, practical use
and indeed internal implementation, these tasks are quite similar
to the deconvolution tasks described in Chapter~\ref{c:decon}. The major
difference is that the `convolution' operation (which turns a prospective
model into a dirty image) is somewhat more involved. Also account must
be made of the changing noise level across the dirty image.

The two mosaic deconvolution tasks are \task{mosmem}, which implements a
maximum-entropy-based deconvolution algorithm, and task \task{mossdi}, which
uses a Steer, Dewdney \& Ito (SDI) CLEAN algorithm. Generally \task{mosmem}
is superior, although \task{mossdi} can be better for images containing
point sources.
Note that, although you can
make mosaiced, multi-frequency synthesis images with \task{invert} (and, indeed,
produce a mosaiced, spectral dirty beam), there is no
mosaic equivalent to \task{mfclean}. In deconvolving a mosaiced,
multi-frequency image you will have to tactically assume that the
spectral index is 0. This should not be a problem -- primary beam model
errors are probably more significant than spectral errors in these
deconvolutions.

If you are deconvolving, note the recommendations for \task{invert}'s
\verb+imsize+ parameter, and the use of \verb+options=double+.

If you are familiar with the inputs to the conventional deconvolvers,
the inputs to \task{mosmem} and \task{mossdi} should be fairly
straightforward. In the case of the inputs to \task{mosmem} and \task{maxen},
apart from differences in the \verb+options+, the meaning of the \verb+flux+
keyword and the default \verb+region+, the only significant difference is in
specifying
the expected RMS noise level in the dirty image. Because the noise level
varies across the dirty image, \task{mosmem} uses the theoretically
expected noise level (which it computes) times a user-specified
{\em fudge factor}, \verb+rmsfac+. That is, if \verb+rmsfac+ is
set at \MM{1} (the default), then \task{mosmem} uses the theoretical
noise level when calculating its $\chi^2$ statistic.

Typical inputs to \task{mosmem} are:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MOSMEM}}\\
\hline
map=lmc.map	& Dirty image produced by \task{invert}.\\
beam=lmc.beam	& Beam dataset.\\
model		& An initial model estimate -- generally unset.\\
default		& The image that the solution should tend\\
		& towards -- generally unset.\\
out=lmc.model	& The output dataset.\\
niters=30	& Maximum number of iterations -- default is 30.\\
region=		& Region to deconvolve. The default is the entire image.\\
measure		& Leave unset gives you the Gull measure.\\
flux=		& Estimate of the total flux -- its best to give a value.\\
rmsfac=1	& RMS noise fudge factor. Default is 1.\\
q		& An initial estimate of the beams volume. Generally\\
		& you can leave this unset.\\
options		& Generally leave unset, or\\
options=doflux	& use \verb+doflux+ to enforce the flux constraint.\\
\hline
\end{tabular}
\end{center}

The inputs and use of \task{mossdi} should be equally simple
for someone familiar with \task{clean}.

Having produced a model, we generally want to convolve this
with a Gaussian CLEAN beam and add in the deconvolution
residuals. This is done by \task{restor}.
The inputs and use of \task{restor} is identical to a conventional observation
(\task{restor} is the only general task which is smart enough to recognise
a mosaiced experiment directly). Task \task{restor} uses a constant
CLEAN beam -- it is not a function of position. The only caveat is that,
when determining
a default CLEAN beam, \task{restor} fits a Gaussian to the synthesised
beam which corresponds to the first pointing. Provided the first
pointing is a fairly typical pointing, this will probably be adequate.
Otherwise
you may wish to use task \task{mospsf} (see Section~\ref{s:mostools} below)
to generate an actual point-spread function (at some position) and then
use \task{imfit} to determine Gaussian parameters for it.

Typical inputs to \task{restor} are:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf RESTOR}}\\
\hline
map=lmc.map	& Dirty image produced by \task{invert}.\\
beam=lmc.beam	& Beam dataset.\\
model=lmc.model	& Model produced by \task{mosmem}.\\
mode		& Leave unset to get restored image.\\
fwhm		& Beam size -- leave unset to let \task{restor}\\
		& fit it, but to the first pointing!\\
pa		& Again leave unset to let \task{restor} fit it.\\
\hline
\end{tabular}
\end{center}


\subsection*{Self-Calibration}\index{mosaicing, self-calibration}
The software to perform self-calibration is workable and reasonable
flexible, although it is rather inelegant. In part, this software has not
been upgraded from the time before the `joint approach' suite was
developed in \miriad.

The self-calibration process is performed in two main steps (there is a
minor third step). First the
task \task{demos} (``de-mosaic'') is used to break the model produced
by \task{mosmem} into models for individual pointings. That is,
\task{demos} produces many models each one of which corresponds to
the nominally true sky multiplied by the primary beam pattern at a
pointing. The second step is performed by task \task{selfcal}
(\task{gpscal} cannot cope with mosaiced observations). Task
\task{selfcal} takes all the models simultaneously and then, for
each visibility in the input visibility
dataset, it computes a model visibility using the model with
the same pointing centre.
The observed and model visibilities are then processed
by a conventional antenna-gain solver, to produce a table of
antenna gains vs time.

In reality, antenna gains will be a function of both time and pointing centre.
However \task{selfcal} 
assumes that the gains are purely a function of time -- not
pointing. In practice this should not be a great problem, as
time and pointing change together, and 
integrations that are close in time will also be close on the sky.
Note that, short of setting a self-calibration solution interval to
be smaller than the integration time, you cannot be sure that a
solution interval will contain data from a single pointing.

In the above process, only a subset of all pointings need be
used in the self-calibration process. If, for example, you have
a strong source in one pointing and all the other pointings have only
weak emission, it may well be appropriate to assume that the antenna gains
are completely independent of pointing. In this case, the gains can
be determined from the one strong field.

We now address the steps in more detail:
\begin{enumerate}
\item The \task{demos} step: This step consists of producing a
number of models, one for each pointing. The inputs are described
in turn.
\begin{itemize}
\item {\tt map}: This gives the name of the input model image (produced
by \task{mosmem}) to be de-mosaiced.
\item {\tt vis} This will usually be the visibility dataset to be
self-calibrated. This dataset is used to determine the pointings present
and the primary beam to be used. 
\item {\tt select}: This provides normal visibility selection. If only
a subset of pointings are being processed, it is convenient to select them here.
In this way, models are not generated for pointings that are not of
interest. Typically, if you wish to self-calibrate with only a subset of
pointings, you would use the {\tt ra}, {\tt dec} and/or {\tt source}
subcommands to select the appropriate ones.
\item {\tt out}: This gives a template for the names of the output de-mosaiced
models. Task \task{demos} will generate an output name by appending a
number to the template name. For example, the output template
\verb+lmc.dmos.+ would produce names such as \verb+lmc.dmos.1+,
\verb+lmc.dmos.2+, etc.
\item {\tt imsize}: This gives the maximum size of the output models.
Task \task{demos} may make the outputs smaller where needed. The default
used by \task{demos} is derived from the primary beam size and the input
model, and should be adequate (although if disk-space is tight, you might
set a smaller number than that chosen by \task{demos}).
\item {\tt pbtype}: This gives the primary beam type to use in the de-mosaicing
process. The default, which is determined from the {\tt vis} dataset,
should be adequate.
\item {\tt options}: You {\em must} invoke the option \verb+detaper+.
This causes \task{demos} to account for any residual primary beam
attenuation that \task{mosmem} has left.
\end{itemize}
Typical inputs to \task{demos} are:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf DEMOS}}\\
\hline
map=lmc.model	& Model produced by \task{mosmem}.\\
vis=lmc.uv	& The visibility dataset to be self-calibrated.\\
select		& Leave unset if self-calibrating with all pointings, or\\
select=source(lmc\_123,lmc\_124) & select just the fields to be used in
		the self-calibration process.\\
out=lmc.dmos.	& Output name template.\\
options=detaper	& Account for any residual primary beam attenuation.\\
pbtype		& Generally leave unset.\\
imsize		& Generally leave unset.\\
\hline
\end{tabular}
\end{center}

\item The \task{selfcal} step: In general, the inputs to \task{selfcal}
are fairly conventional -- see Chapter~\ref{c:selfcal} for more
information. There are, however, multiple input models (produced by
\task{demos}) corresponding to each
of the pointings to be used in the self-calibration. Note that wildcards will
generally make this easy. The other difference {\em which you
must remember} is to use \verb+options=mosaic+ to invoke the
mosaicing machinery. Note also that \task{selfcal} will not use
a visibility of a particular pointing if there is no model for this
pointing. Thus, if you are self-calibrating using only a few of the stronger
pointings, you do not have to explicitly select the data for these
pointings.

Typical inputs are:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf SELFCAL}}\\
\hline
model=lmc.dmos.* & Models produced by \task{demos}.\\
vis=lmc.uv	& The vis dataset to be self-calibrated.\\

select		& Set as with normal self-calibration.\\
options=mosaic,phase	& Use mosaic mode and phase self-calibration, or\\
options=mosaic,amp	& amplitude/phase self-calibration.\\
\hline
\end{tabular}
\end{center}

\item Fixing the interpolation tolerance: As noted in Section~\ref{s:inttol},
a \miriad\ gain table has an associated interpolation tolerance (the time
interval over which you can interpolate or extrapolate a gain). Task
\task{selfcal} will set this to the solution interval. If you are
self-calibrating with only a few pointing centres, you will want the
gains to apply to the entire cycle through the mosaic grid. In this
case, you may well want to increase the interpolation tolerance from
the default. See Section~\ref{s:inttol} for the details. In summary,
you use \task{puthd} with inputs like:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf PUTHD}}\\
\hline
in=lmc.uv/interval	& Set the `interval' item of a vis dataset.\\
value=0.1		& Set the tolerance to 2.4 hours (0.1 days).\\
\hline
\end{tabular}
\end{center}

\end{enumerate}

\subsection*{Some Additional Tools}\label{s:mostools}
We briefly describe some other useful tools.
\begin{itemize}
\item {Mosaic Coverage -- MOSLST:}\index{moslst}
In doing a mosaic observation, you will want to good Fourier sampling (coverage)
as well as even sampling of the different pointing centres. Task \task{moslst}
produces a simple plot of LST vs point number for mosaiced visibility
files. In doing this, it required that you follow the {\em field naming
convention} mentioned earlier (see Section~\ref{s:fnc}).
\item {Listing Mosaic Tables -- IMLIST:}\index{imlist}
Task \task{invert} stores information concerning its linear mosaic
operation in the item \verb+mostable+ (stored in both the \verb+map+
and \verb+beam+ datasets). The table can be printed
by \task{imlist}, using \verb+options=mosaic+.
\item {Mosaic Point-Spread Function -- MOSPSF:}\index{mospsf}
It is occasionally instructive to look at the point-spread function at
a particular position in a mosaic experiment. Task \task{mospsf} can compute
this. Apart from the input beam data-set, the user must specify a position and
frequency of interest (the point-spread function is also frequency
dependent).
\item {Mosaic Sensitivity and Gain Images -- MOSSEN:}\index{mossen}
Just as the point-spread function varies, so does the expected noise
level in an output mosaiced image. Additionally, as mentioned above,
\task{invert} does not attempt to completely correct for the primary
beam attenuation where there is too little data (i.e. some primary
beam attenuation is in the output image). The task \task{mossen}
can produce images of the expected rms noise and the remaining
primary beam attenuation given a mosaiced image.
\item {MOSTESS:} \task{mostess} implements a mosaicing algorithm
identical to the \aips\ \task{VTESS} program. It is more cumbersome
to use, and produces results which are no better than \task{mosmem}. Its
use is not recommended.
\item {PMOSMEM:} \task{pmosmem} can be used to do a joint deconvolution
of a polarimetric mosaic. The ``joint'' in this case applies to both
jointly processing the multiple Stokes parameters as well as
processing the different pointings. With polarised emission being potentially
positive and negative valued, the entropy measure for this is different
from a simple total intensity deconvolution. See
\htmladdnormallink{Sault et al. (1999)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1999A\%26AS..139..387S}
(A\&A 139, 387) for more background on joint polarimetric deconvolution.

\end{itemize}

\section{The Individual Approach}\index{mosaicing, individual approach}
The individual approach images, deconvolves, self-calibrates and restores
each pointing separately. It is only when you are happy with the individual
images that you would combine them -- using a linear mosaicing algorithm. 

As noted above, the individual approach is less automated and can produce
fundamentally poorer deconvolutions. However in some high-dynamic range
applications, it may be preferable.
The reason for this is that
the deconvolution process does not depend on the primary
beam model (nor do some errors such as a constant pointing error affect
the deconvolution). With the `individual approach', it is possible to
deconvolve sources (and thus eliminate their sidelobes -- which is the
important issue) beyond the limit
where \miriad's primary beam model gives up. To do this, however, you
must make images larger than the main primary beam lobe 
(see Table~\ref{t:mossize} above).

In the following, we assume that the `joint approach' section has been
read and understood.

\subsection*{Splitting and Imaging}
Although this is largely a matter of taste, it
may be convenient (particularly if self-calibration is to be used)
to split the multi-pointing visibility dataset into
single pointing ones. Task \task{uvsplit} (with no options, and only
the multi-pointing visibility dataset as input) will do this function.
It will also copy across any calibration tables associated with the input
dataset.

In the individual approach, you will run \task{invert} many times,
once for each pointing (you may have split the multi-pointing visibility
dataset into single pointing ones, or you could use selection  by source name
to select out the appropriate subset of data). Apart from possibly the names
of the input and output datasets, the parameters to \task{invert} should not
be changed between runs.

Even though you are imaging just a single pointing, you will still want to use
\task{invert}'s mosaic mode (\verb+options=mosaic+). This causes
\task{invert} to perform its geometry corrections and to create the
the images of the different pointings on the same
pixel grid.
In this way, no interpolation will be needed when the images from the different
pointing are finally combined. Consequently the artifacts and problems
associated with interpolation can be avoided.

To compute the geometry, however, {\em you must provide} a reference position
on the sky -- the tangent point. The default tangent point is the pointing centre
of the data being imaged -- {\em this is not appropriate} as it will vary
from pointing to pointing. You will want a tangent point which is
the same for all the pointings.
Although it
can be any arbitrary point, it is best to make it near the centre of the
source being imaged. If there is a point source which dominates the
image, you might choose its position as the tangent point to help reduce
deconvolution problems.
The tangent point is given through the \verb+offset+ keyword, in the format
{\it hh:mm:ss,dd:mm:ss} (or as decimal hours and degrees). 

As an example, consider an LMC observation, where we wish to image
field 123 (which has field name \verb+lmc_123+). Assuming we have a
multi-pointing dataset, and wish to use position (RA,DEC)=(4:30,-71:00)
as the tangent point. Typical inputs to \task{invert} would be:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf INVERT}}\\
\hline
vis=lmc.uv	& The input dataset.\\
select=source(lmc\_123) & Select a single pointing.\\
options=mosaic	& Use mosaic mode.\\
offset=4:30,-71:00& Set position for geometry computation.\\
map=lmc\_123.map & Output image name.\\
beam=lmc\_123.beam & Output beam name. \\
cell=& Set cell size.\\
imsize=& Set output image size. \\
\hline
\end{tabular}
\end{center}

\subsection*{Deconvolution, Restoration and Self-Calibration}
Deconvolution is no different to conventional
observing. When restoring, 
you may wish to use the same restoring beam size
for all fields. Otherwise treat each pointing as a separate observation.

Much of the discussion in
the self-calibration section of the `joint approach' applies equally
well here. The major difference is, of course, that because the
deconvolution step has been performed on the individual fields, the 
\task{demos} step is not required. If you have split the visibility
data into separate single-pointing datasets, you will not need to
use \verb+options=mosaic+ (although it will not hurt). Also for single-pointing
datasets, if you wish to use
just one or a few of the stronger fields for self-calibration, you
will need to copy the resultant antenna gain tables across to the
other datasets, using \task{gpcopy}.
\subsection*{Image Combination}\index{linmos}
When you are satisfied
with the deconvolution, restoration and self-calibration of
all the individual images, task \task{linmos} can be
used to combine them in a linear mosaic. Usually you will just combine the
restored images (if you are going to do quantitative analysis on the
composite image, it may be best to do a deep CLEAN and use the same
restoring beam for all pointings). Although \task{linmos} can interpolate
input images to align them, its algorithm, particularly for geometric
correction, is very poor, and so this
is {\em strongly}
discouraged. You should use \task{invert} to make all the input images on the
same grid, by setting a common tangent point (\verb+offset+ keyword).

Task \task{linmos} uses the same weighted sum of the input pointings as
the `joint approach' software (see Section~\ref{s:mosjoint}). Normally
the expected rms noise in the image is determined from the images
themselves (image item \verb+rms+). However if this item is missing,
or if you wish to override it to get a different weighting, you may enter
the expected rms noise via keyword \verb+rms+. Also note that \task{linmos},
by default, {\em fully corrects for the primary beam attenuation} even
when this excessively amplifies the noise. The \verb+taper+ option
can be used to reduce the correction at the edge of the field, and thus
avoid excessive noise amplification.

Typical inputs to \task{linmos} are:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf LINMOS}}\\
\hline
in=lmc\_*.cln	& Use wildcards to select all images.\\
out=lmc.mos	& The output linearly mosaiced image.\\
rms		& Generally left unset.\\
options		& Leave blank to fully correct primary beam,\\
options=taper	& \ or set to taper at the edge of the mosaic.\\
\hline
\end{tabular}
\end{center}

\subsection*{Miscellaneous}
In a similar way to \task{mossen}, \task{linmos} can also produce an image giving
the expected rms noise as a function of position, and a gain image --
see the help file on using the \verb+options+ keyword for these.

\section{Combining Mosaics and Single Dish Data}\index{single dish, combining with mosaics}
\index{mosaicing, combining single dish data}
For a good discussion of the theory and practice of combining single
dish and mosaic data, see the 
\htmladdnormallink{paper by Snezana Stanimirovic}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2002sdra.conf..375S}
(ASP Conf. Series, vol 278, p 375).


One of the benefits of mosaicing is that it partially recovers
spacings shorter than the shortest projected interferometer spacing. In principle,
the shortest spacing present in a mosaic can be the projected interferometer
spacing minus the antenna diameter, \MM{D} (see the references mentioned earlier for the
argument). When antennas are as closely packed as is physically possible,
the minimum physical spacing will be \MM{D}, and so in principle a mosaic can
reduce the effective minimum spacing almost down to the zero spacing.
In practise, mosaicing tends to reduce the effective minimum spacing by
about \MM{D/2}, rather than the theoretical \MM{D}. For the ATCA observations
of sources
appreciable far south, the effective minimum spacing in a mosaic
is about 20~m (the minimum physical interferometer spacing is 31~m).

So whereas mosaicing helps recover short spacings, there are invariably
some short spacing missing. To fill these spacings,
interferometer data must be augmented with single-dish data.
Just as there are two approaches to mosaicing (the joint and individual
approach), \miriad\ provides two approaches to single-dish
combination -- the linear and a non-linear
methods, using tasks \task{immerge} and \task{mosmem} respectively. Which
method produces best results is quite problem specific, and indeed it
is perhaps best to try both if possible. However,
\begin{itemize}
\item The non-linear combination method works on the dirty mosaic produced
by \task{invert}, whereas the linear method works from deconvolved
images. Thus the non-linear method cannot be used with the ``individual''
mosaicing approach.
\item The linear method can be more robust to the single-dish data failing
to satisfy some of the assumptions of the non-linear method.
\item The non-linear technique can perform better when there is emission right
to the edge of the sampled region of the sky.
\item The linear method assumes that the single-dish point-spread function is
a gaussian. For the non-linear method, you give a ``beam'' dataset, which can
be an actual measurement of the single-dish point-spread function (which
can be asymmetric).
\end{itemize}

Given that combining mosaic and single-dish data can be a bit of an art,
its worth doing a few checks of the result:
\begin{itemize}
\item The simplest check is to see that the total flux agrees with what
you expect. Task \task{histo} is the simplest way of measuring the
total flux in a continuum image (or plane of a spectral cube). Otherwise
\task{imstat} can be used.
\item The non-linear methods produce {\em model} outputs, that need to
go through a restore step. Task \task{restor} will also generate
residual images. You probably want to look at the residuals for both
the mosaic and single-dish images, which is readily done with
\task{restor} using {\tt mode=residual}.
\end{itemize}

In general, to combine single dish and interferometer data, you need to image
the complete region containing emission. In practise this means that
sidelobes, from
emission outside the region of interest, are not significant. This
applies both to the single-dish and interferometric observations. Both
observations should map the complete region of the emission.

\subsection*{Some Theory and Preparing the Single-Dish Data}
Both the linear and non-linear methods require a mosaic and a single-dish
image as inputs. \miriad\ has not tasks to form images from
single-dish data. Thus you must use another package to generate the
single-dish image, which you will then import into \miriad\ (presumably
via FITS -- see Section~\ref{s:fits}). Further massaging might then be
needed within \miriad\ to prepare the single-dish image for the combination
process. The steps needed are as follows:
\begin{enumerate}
\item Coordinate grids: The coordinate systems of the single-dish and
mosaic image {\em must be identical}. That is the image size, pixel
increment, equinox, projection geometry, etc, must be the same.
Often, the \task{regrid}
task can achieve this reasonably painlessly. Given an input image and a
template image, \task{regrid} resamples (by interpolation)
the input onto the grid of the template.
In doing so, it correctly handles different projection geometries,
equatorial/galactic coordinate conversion, equinox conversion and different
velocity definitions.

Note you should not regrid the mosaiced image. In regridding, the information
that describes the mosaicing process is lost, which will make life difficult
for you.

Typical inputs to \task{regrid} would be
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf REGRID}}\\
\hline
in=lmc.sd	& Input single dish image.\\
tin=lmc.mosaic  & The mosaic giving the coordinate system\\
		& that we want to regrid to.\\
out=lmc.sd\_regrid & The output, regridded, single-dish image. \\
\hline
\end{tabular}
\end{center}

For interpolation to produce a faithful output, the grid of the
input cannot be too coarse -- the number of pixels across the single-dish
beam should be at least 3. Although this is usually readily satisfied on the
spatial axes, spectral line users should be more wary: the number of channels
across the spectral axis response function is usually very small. In addition,
whereas the spatial resolution of the single dish will be poorer than the
mosaic, this is not necessarily true for the spectral axis. So, in addition
to regridding, some spectral smearing might need to be performed to reduce
the spectral resolution of the single-dish image to that of the mosaic.
Unfortunately, \miriad\ is poorly equipped to solve these problems; these
issues are left as a difficult exercise for the user.

\item Flux scale calibration:
The single-dish and mosaic images both should have flux units of Jy/beam. 
Although appropriate calibration during the observations is obviously the 
best method to ensuring that the flux scales of the mosaic and 
single-dish images are the same, this is not always
achieved. The flux scales of two images, that are nominally in the same units,
can differ by a modest, but appreciable, amount. 

A common approach to estimating flux calibration factor is to compare the
single-dish and mosaic images in that annulus of spatial frequencies where both
are sensitive. For example, a Parkes observation will sample spatial frequencies
from 0 to near 64~meters, and a ATCA mosaic (assuming the shortest ATCA
spacing is observed) will be sensitive from about 20~meters upwards. As the
reliability of the data near the extremes is suspect,
spatial frequencies from 25 to 40~meters should be modestly reliably represented
in both an ATCA mosaic and a Parkes image. Comparing in the overlap
annulus works very well when there is a simple source which is
well represented in this region (e.g. a dominant point source).

The tasks to perform the linear and non-linear combination both have parameters
for setting {\em and} deducing the flux calibration factor.

\item Point-spread function: In principle, knowing the point-spread function
(the ``beam'') of the single-dish image is just as important as it is for 
the mosaic.
It is best to `map' this at the time of the single-dish observations and to
account for any change in the beam that is caused by the single-dish imaging
process.

While it is possible to imagine algorithms that could deduce the
beamwidth based on the spatial frequency overlap between the single-dish
and mosaic data, in practise this is not possible (the overlap region
is not wide nor is the data reliable enough). In estimating beamwidth
parameters, the beamwidth and flux calibration factor are highly
coupled; an error in the beamwidth has much the same effect in the overlap region as
an error in the flux scale. Thus you cannot determine the beamwidth and flux
scale simultaneously.

The linear combination method assumes the single-dish point-spread
function is a gaussian form, whereas the non-linear method
takes a image dataset as the point-spread function (the point-spread
function need not be symmetric). If a point-spread function is not
readily available for the non-linear method, a gaussian dataset can be
produced.
\end{enumerate}

\subsection*{The Linear Method -- IMMERGE}
The linear method, sometimes known as feathering, can be thought of in terms 
of operations in the
Fourier (spatial frequency) domain of the images. If the single-dish 
image  is a good representation of the object at low spatial frequencies,
and if the mosaic
image is a good representation at mid to high spatial frequencies, then the
two sorts of data can be merged in the Fourier plane to form an image
accurate up to the resolution of the mosaic image. This is exactly what
\task{immerge} does.

The way that \task{immerge} normally combines the data is to give unit 
weight to the single-dish data
at all spatial frequencies, and to taper the low spatial frequencies of
the mosaic data. This tapering is such that the sum of single dish and tapered
mosaic data produces a gaussian beam equal to the
mosaic gaussian beam. Figure~\ref{fig:immerge} shows this process
for a point source. The Parkes data plus the tapered ATCA data neatly
add to give an overall response of a gaussian form.

\begin{figure}
\begin{center}
\leavevmode\epsffile{immerge.ps}
\end{center}
\caption{Spatial frequencies sampled by Parkes and the ATCA}
\label{fig:immerge}
\end{figure}

The inputs to \task{immerge} are moderately straightforward:
the {\tt in} parameter
gives the name of the input mosaic and single-dish image (in that
order), whereas the {\tt out} parameter gives the
name of the output dataset.
Because \task{immerge} assumes that the beams of both the single-dish and
mosaic images are gaussian forms,
you will want to use images which approximate this.
Whereas single-dish images usually approximate this reasonably, a dirty
mosaic does not. Instead, 
{\em you will want to use a deconvolved/restored mosaic}
in the \task{immerge} process.

Task \task{immerge} allows you to set a parameter giving the flux
calibration factor,
{\tt factor}: \task{immerge} multiplies the single-dish image by
this factor before further processing. If you believe your calibration,
you should set this parameter to 1 (or whatever number you believe the
calibration factor to be). Note that if the parameter is unset,
\task{immerge} attempts to deduce this factor (i.e. the default is not
simply to use a value of 1). It does this
by comparing data in the overlap annulus in the spatial frequency coverage
of the mosaic and single-dish images. In doing this \task{immerge} first
convolves the mosaic image to the same resolution as the single dish data
(including scaling fiddles to
account for the inputs being in Jy/beam in differing resolutions),
and it then performs a robust (L1) fit between the two sets of pixel
data. If you use \task{immerge} to determine the flux calibration factor,
you will want to consider setting the following parameters:
\begin{itemize}
\item {\tt uvrange} giving the range in the Fourier domain
where there is overlap. This parameter can take two or three values,
the first two being the numbers giving the inner and outer radius of
the overlap region, and the third parameter giving the units of the first two
values. The most useful units are {\tt meters} and {\tt klambda}
(kilowavelengths -- which is the default). For example, typically the overlap
between a Parkes and ATCA observation (assuming the shortest ATCA
spacing is included in the observation) would be
\begin{verbatim}
   uvrange=25,40,meters
\end{verbatim}
The default is to use all spatial frequencies.
\item {\tt region} parameter allows you to select those planes
used in determining the flux calibration factor. Unlike the normal behaviour
of {\tt region} parameter, {\em only plane selection} is allowed, with the
{\tt image} sub-command being the only really useful command.
Generally you would choose
planes with appreciable structure which is well represented
in both single-dish and mosaic data. The default is to include all planes.
\item {\tt device} sets the PGPLOT plotting device where plots are make of 
a comparison between the data in the overlap region. Two plots are produced.
For the first plot, the axes gives the 
value of a pixel (both real or imaginary values are included) in the mosaic
(\MM{x} axis) and single-dish data (\MM{y} axis). These pixel values are after
tapering, scale factor fiddles, and after applying the calibration scale factor that
has just be deduced. The data should follow a ``\MM{y=x}'' form (which is
also plotted in red). The second plot gives the residual difference
between the mosaic and single-dish data (i.e. the difference from \MM{y=x}) as 
a function of spatial frequency. A trend in these residuals might suggest 
that the single-dish beam parameters are grossly wrong.
Although the default is not to create a plot, there seems little reason
why you would not want to inspect it.
\end{itemize}

The gaussian parameters of the mosaic and single-dish beams are important
parameters in the processing that \task{immerge} performs. Task \task{immerge}
tries to determine these by first looking for the relevant gaussian beam
parameters within the image datasets. If it cannot find these, it will
attempt to estimate the beams, based on its knowledge of the telescope and
the observing
frequency. These estimates, however, are far from perfect -- {\em it is
far better to ensure that the beam parameters are within the dataset
before using \task{immerge}}. If you are unsure whether the beam
parameters have been set for a dataset, \task{prthd} includes these
(if found) in amongst the information it prints out.
For the  mosaic, normally \task{restor} will have
saved the beam parameters in the dataset. However it is likely that you
will need to add the single-dish beam parameters to the single-dish dataset.
The items you want to set are {\tt bmaj}, {\tt bmin} and possible {\tt bpa}
(beam major, minor axis size and position angle respectively), which are 
set using \task{puthd}.

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf PUTHD}}\\
\hline
in=lmc.sd/bmaj	& Set beam major axis.\\
value=18.8,arcmin & Width is 18.8 arcminutes.\\
\hline
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf PUTHD}}\\
\hline
in=lmc.sd/bmin	& Set beam minor axis.\\
value=18.8,arcmin & Width is 18.8 arcminutes.\\
\hline
\end{tabular}
\end{center}

When emission continues to the edge of the image (or near the edge),
algorithms such as that used by \task{immerge} can suffer edge artifacts.
If these are problematic, 
\task{immerge} has a some parameters which you might like to try to reduce
the problem.
Task \task{immerge} pads images with
a guard band before doing a Fourier transform. Edge effects may be lessened
by increasing this from the default value by setting the {\tt guard} parameter.
Task \task{immerge} also tries to avoid edge discontinuities by padding the
guard band with a mirror image of some of the data. If the image is mostly
zero, then zero padding (rather than than a reflection) might be better.
In this case use {\tt options=zero}.

Typical inputs for \task{immerge} are
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMMERGE}}\\
\hline
in=lmc.mosaic,lmc.sd	& Give the mosaic and single-dish inputs\\
out=lmc.combo		& Give output combination dataset.\\
factor=1		& Set to the correct flux scales, or \\
factor			& leave blank and\\
uvrange=25,40,meters	& specify the spatial frequency overlap annulus,\\
region=image(10,20)	& and use planes 10 to 20 inclusive,\\
device=/xs		& and let it plot the comparison. \\
\hline
\end{tabular}
\end{center}

\subsection*{The Non-Linear Method -- MOSMEM}
The non-linear path to combination is conceptually quite different to the
linear one, even though its success ultimately rests on the same data.
The non-linear
combination uses the task \task{mosmem} (and the maximum entropy principle)
to perform a joint deconvolution of the mosaic and single-dish images. It
finds the model which maximises the entropy, subject to two
separate $\chi^2$ constraints (one each for the mosaic and 
single-dish images).
Usage of \task{mosmem} is similar to that in Section~\ref{s:mosdeconv},
expect that there are now two parameter values for the {\tt map},
{\tt beam}, {\tt rmsfac} and {\tt q} keywords, with these
corresponding to the mosaic and single-dish values (in that order).

Unlike \task{immerge}, the non-linear approach works with dirty images.

Note that you have to give a single-dish {\tt beam} dataset. Unlike all the
other deconvolution tasks, the image size of this dataset can be
much smaller than the image being deconvolved (\task{mosmem} will zero pad to
make it the size that it requires), and it can be asymmetric (normally
beams are assumed to have ``even'' symmetry). If you do not have a beam dataset,
then probably the best thing you can do (apart from actually going out
and determining it through an observation) is to generate
a gaussian image, with the appropriate resolution, with \task{imgen}.
Task \task{imgen} takes a template image as its input, multiplies the template by a scale
factor, and then adds an ``object''. A convenient approach would be to use
the single-dish image as the template (or a single plane of this in spectral
line experiments), multiply this by 0 and then add a gaussian.
Typical inputs would be:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMGEN}}\\
\hline
in=lmc.sd		& Give the single-dish image as template.\\
factor=0		& Multiply template by 0.\\
object=gaussian		& Add a gaussian.\\
spar=1,0,0,1128,1128,0	& Gaussian is \TT{18.8}{18.8} arcmin\\
			& (1128 arcsec) at image centre and with amplitude 1.\\
out=lmc.sd\_beam	& Output is the beam dataset.\\
\hline
\end{tabular}
\end{center}

Task \task{mosmem} needs to know the rms noise in the two input
images. It determines this by multiplying the theoretical rms noise
(the theoretical noise is required to be present in the dataset) with 
the {\tt rmsfac} input parameter. While \task{invert} will have saved the 
required noise information in the mosaic dataset, you will most likely have to
add some estimate of the theoretical noise level to the single-dish
dataset. You could estimate this by using \task{cgcurs} 
(using {\tt options=stat}) to determine the rms level in some blank part 
of the sky (see Section~\ref{s:pgplot}). Task \task{puthd} can then be
used to set the appropriate item (named, surprisingly, {\tt rms})
in the single-dish image. For example:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf PUTHD}}\\
\hline
in=lmc.sd/rms		& Set the rms item in the single-dish dataset.\\
value=5.0		& Set the theoretical noise as 5.0 Jy/beam.\\
\hline
\end{tabular}
\end{center}
As you have probably set the {\tt rms} item in the dataset to the noise
level that you believe is true, the {\tt rmsfac} parameter for the
single-dish image that you input to \task{mosmem}
can usually be left unset (it will default to 1).

As with \task{immerge}, parameter {\tt factor} allows you to set the
flux calibration factor (the factor by which the single-dish is scaled)
as one of the inputs to \task{mosmem}. However, unlike \task{immerge},
if you do not set the parameter, \task{mosmem} assumes the flux
calibration factor is 1 -- that is, \task{mosmem} does not
determine this parameter by default. However \task{mosmem} still has the
ability to solve for the calibration factor -- use {\tt options=dofactor}
to turn this on. In this case, the {\tt factor} parameter is used as
an initial estimate (it should be a reasonably good estimate). Unlike
\task{immerge}, you do not need to tell \task{mosmem} what the overlap 
annulus is -- this information is implicit in the beam datasets. 

It might be instructive to compare the factor estimated by \task{mosmem} and
\task{immerge} -- they can differ significantly, and yet both seem to
be consistent with the data.

If you use {\tt options=dofactor} with a spectral cube, \task{mosmem} will
work out a separate flux calibration factor for each plane. This is not
what you probably want (particularly for planes that are predominantly noise!). 
In this case, its better to deduce the calibration factor beforehand, and
not to use {\tt options=dofactor} in \task{mosmem}.
Some approaches to deduce the calibration factor beforehand include:
\begin{itemize}
\item use \task{immerge}
(but recall that \task{immerge} requires a deconvolved mosaic), or
\item
use \task{mosmem} on an integrated intensity (zeroth-order moment)
image or on a plane with substantial emission.
\end{itemize}

Note that {\tt options=doflux} cannot be used when doing a joint mosaic/single-dish
deconvolution -- an integrated flux constraint is implicit in the
single-dish data.

Typical inputs to \task{mosmem} would be
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MOSMEM}}\\
\hline
map=lmc.mosaic,lmc.sd	& Inputs are the dirty mosaic and \\
			& single-dish datasets.\\
beam=lmc.beam,lmc.sd\_beam & Mosaic and single-dish beams.\\
niters=30		& Maximum number of iterations.\\
region=			& Region to deconvolve.\\
rmsfac=1,1		& RMS noise fudge factors.\\
out=lmc.model		& The output model.\\
factor=			& Set this to the calibration factor, or\\
			& it defaults to 1.\\
options			& Leave unset to fix the calibration factor, or\\
options=dofactor	& get MOSMEM to solve for this.\\
\hline
\end{tabular}
\end{center}

Having performed a joint deconvolved, you will want to use
\task{restor} with the model produced
by \task{mosmem} along with the mosaic dirty image and beam.

\subsection*{Some Alternative Non-Linear Methods}
Here we describe two ad hoc methods of combining single-dish and
mosaic images. To some extent, these are of historical
interest only -- the linear and non-linear methods mentioned before
are to be preferred.

These alternative methods are not implemented as tasks within \miriad, so
the steps to implement them are more hands-on. For example, if needed, you
will want to correct the flux calibration factor manually before using either
of these (e.g. \task{immerge} can do this for you).
Some examples of using these approaches are given
in the
\htmladdnormallink{paper by Snezana Stanimirovic}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=2002sdra.conf..375S}
(ASP Conf. Series, vol. 278, p. 375).

\begin{itemize}
\item An alternative combination technique using \task{mosmem} is to
use the single-dish image as the default image ({\tt default} keyword)
rather than as a separate $\chi^2$ constraint. That is, you use just
the mosaic image and beam with the {\tt map} and {\tt beam} parameters,
and set the single-dish image as the {\tt default} image. Strictly
the default image should be in units of Jy/pixel. However
\task{mosmem} is smart enough to divide the pixel values by the beam
volume (where necessary) to perform some sort of crude conversion from
Jy/beam to Jy/pixel. You can constrain the total flux of the resultant
model to agree with the single-dish total flux, by using {\tt options=doflux}.
Task \task{mosmem} is smart enough to use the default image as an flux
estimate when the {\tt doflux} option is used and when the {\tt flux}
is left unset.

\item You could form an image which is a simple weighted average
of the mosaic and single-dish dirty images (i.e. before any deconvolution),
and then pass this to \task{mosmem} as
if it was a normal mosaic. In doing this, you are
trying to fool \task{mosmem}
to some extent, so there are a number of fudges that you should do
to make the data better approximate what \task{mosmem} expects.

Before adding the mosaic and single-dish images, you should apply
the residual primary beam attenuation present in the mosaic
to the single-dish data. Task \task{mossen} will generate an
image of the residual primary beam attenuation (set the {\tt gain}
parameter).
Having generated a 
the residual primary beam attenuation, you can then apply this to
the single-dish image using \task{maths}.

The weights used to add the mosaic and single-dish
images should sum to 1, but the actual way of determining the
relative weights is not well defined. One good choice, advocated by
Stanimirovic et al., is to weight the images in inverse proportional to the
beam volumes. When the difference in resolution between the mosaic
and single-dish observations is appreciable (i.e. the normal situation),
this weighting reduces to nearly the same as that performed by
\task{immerge}. Normalisation the weights to add to 1 gives
\begin{eqnarray*}
w_{\rm mos} &=& \frac{\Omega_{\rm SD}}{\Omega_{\rm mos}+\Omega_{\rm SD}} \\
w_{\rm SD} &=& \frac{\Omega_{\rm mos}}{\Omega_{\rm mos}+\Omega_{\rm SD}}
\end{eqnarray*}
(where $\Omega_{\rm mos}$ and $\Omega_{\rm SD}$ are the beam volumes of 
the synthesised mosaic beam and the single-dish beam respectively).

To generate the effective beam dataset, you will also want to add
a component to the mosaic beam. Because of the process that
\task{mosmem} believes was used to generate a mosaic, the
component you add is not simply the single-dish beam -- the mosaicing process
narrows down the width of the beams from that of individual
pointings. If both the interferometer primary beam and the single-dish beam
are gaussian forms with widths $\theta_{\rm int}$ and $\theta_{\rm SD}$ 
respectively, then you will want to add a gaussian to the mosaic beam
dataset which has a width
\[
\theta = \sqrt{\frac{2\theta_{\rm int}^2\theta_{\rm SD}^2}{2\theta^2_{\rm int}-\theta^2_{\rm SD}}}.
\]
This gaussian will be somewhat broader than
the single-dish resolution (for an ATCA and Parkes combination, it will be about
7\% broader than the Parkes beam). You will want to add this gaussian and the
mosaic beam with the same weights that were used for the single-dish
and mosaic images.
Note that the mosaic beam dataset consists of one image per pointing, and that
you will want to add a gaussian to each plane.
Task \task{imgen} can be used to do this,
although it warns you that its handling of cubes
is crude!
With \task{imgen}, you can set the weighting by appropriately setting the 
scaling factor ({\tt factor} parameter) and the amplitude of the
gaussian ({\tt spar} parameter).

Note that this technique ignores some edge effects.
Also it uses the theoretical noise level derived for the 
mosaic observation. This will be close to what you want if the noise
levels for all pointings are about the same. 
\end{itemize}
