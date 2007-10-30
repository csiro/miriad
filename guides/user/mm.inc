%\nonstopmode
%\input{latexdefs.inc}
%\newcommand{\oneliner}[1]{\begin{flushleft}{\ \ \ #1}\end{flushleft}}
%\documentstyle[twoside,makeidx,html,epsf]{report}
%\setlength{\parindent}{0pt}
%\setlength{\parskip}{2.5mm}
%\setlength{\textheight}{245mm}
%\setlength{\textwidth}{160mm}
%\setlength{\oddsidemargin}{10mm}
%\setlength{\evensidemargin}{-10mm}
%\setlength{\topmargin}{-10mm}
%\begin{document}


\chapter{Reducing 12-mm ATCA observations}\label{c:12mm}\index{millimetre data}

\section{Loading data into \miriad: ATLOD}\label{s:atlod12mm}
ATCA millimetre observations are loaded into \miriad\ in the same fashion
as centimetre data: task \task{atlod} is used.

At 12mm, the opacity of the atmosphere can be significant: in mediocre
conditions, the signal loss is can be a 20\% at low elevations.  With
12-mm data, an opacity correction should be applied to the data during
the \task{atlod} step.  To apply this correction, \task{atlod} uses a
model of the atmosphere (gained from meteorological data contained
within the dataset) to compute a model opacity.  Note that this is just
that -- a model -- and so has its limitation.  To apply atmospheric
opacity correction, use \verb+options=opcorr+. 

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf ATLOD}} \\
\hline
options=birdie,reweight,xycorr,opcorr	& Options for typical 12-mm \\
					& continuum observation, or\\
options=birdie,opcorr,hanning,compress	& possible options for spectral\\
					& line observations.\\
\hline
\end{tabular}
\end{center}

Note that ATCA observations made before October 2003 failed to contain the meteorological
data, and so \verb+options=opcorr+ cannot be used. In this case, you will need
to use the scheme described in Section~\ref{s:atfix}.

\section{Initial ``fixes'' to the data}
Section~\ref{s:atfix} describes a collection of fixes to the data, using
task \task{atfix} that are
important at 3-mm wavelengths. Assuming opacity correction has been done
by \task{atlod}, these initial fixes are of limited relevance to 12-mm
observers. However some applications might find some of the fixes relevant.

\section{Calibration}
For 12-mm data, calibration is essentially identical to the centimetre bands.
Primary flux calibration can be done using \task{gpboot} and observations
of 1934-638, in much
the same fashion
as at the lower frequencies. 1934-638 is about 1 Jy at 12mm wavelengths: the
\miriad\ calibration software contains a built-in model of 1934-638's change
of flux density with frequency. For information about this flux scale,
see the \htmladdnormallink{memo by Bob Sault}
{http://www.narrabri.atnf.csiro.au/calibrators/data/1934-638/1934_12mm.pdf}.
An alternative flux calibration technique is to use a planet as the primary:
see Section~\ref{s:absmm} for more information.

Because atmospheric opacity affects the amplitude gain calibration step, it is
important to consider its effects. In the flux bootstrapping step,
\task{gpboot} determines the boot factor to be applied to the gains of
the secondary calibrator to make them the same as the primary. As the opacity
correction 
done by \task{atlod} (or \task{atfix}) is only correct to first order,
ideally the comparison between primary and secondary gains would be made
{\em at the same elevation and under the same weather conditions}.  Unfortunately
this is usually impossible. Selecting the range of times that the gains
should be compared is a more important issue at 12mm than at lower frequencies.
To select the time range of the secondary to compare, use \verb+time+ selection
in the \verb+select+ keyword.

Some judgement will need to be made about the set of amplitude gains of the secondary to
use in the comparison.
In stable weather conditions, it is probably best to select the time range of the
secondary where its elevation corresponds most closely to the elevation during the primary's
observation. When the weather changes significantly during the observation,
it is probably best to use a time range of the secondary which is close to the
primary's observation. Tasks \task{uvplt} and \task{varplt} can plot source
elevation as a function of time.

Typical inputs to \task{gpboot} are:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf GPBOOT}} \\
\hline
vis=vela.uv		& Input visibility dataset.\\
select=time(1:00,2:00)	& Select a time range in the ``vela.uv''\\
			& dataset where the amplitude gains should be\\
			& comparable to those in the primary calibrator.\\
cal=1934-638.18001	& Primary flux calibrator.\\  
\hline
\end{tabular}
\end{center}


\newchapter
\chapter{Reducing 3-mm ATCA observations}\label{c:3mm}\index{millimetre data}

Currently the ATCA has 3-mm receivers installed on three antennas.
This chapter lists some of the steps specific to this system.

\section{Differences at millimetre wavelengths}
Differences in handling millimetre data
come about because of the nature of the atmosphere and calibrators
at these wavelengths, the nature of preliminary system at 3mm, and as
a result of the greater technical challenges that observing at these
wavelengths entails.
Some differences include:
\begin{itemize}
\item The preliminary system at 3 mm does not have a noise source
calibration system. This means that a room temperature paddle is 
used as a calibration
load, using the so-called chopper-wheel method.
\item The antenna gain and beamshape is a significant function of elevation,
particularly at 3mm. The
gain is at a maximum at about $60^\circ$ elevation. 
\item For poorly understood reasons, the effective locations 
of are ATCA antennas are a function of
frequency. Currently the on-line system uses the centimetre solution for
the antenna location. You will
need to correct for this error in the data reduction process.
\item There is no such thing as a constant point source flux standard
at 3-mm wavelengths, and they are rare at 12-mm. Generally point sources should be
assumed to be variable.
Planets are usually used as the primary flux standard, although 1934-638 is still
a good primary calibrator at 12mm.
\end{itemize}

\section{Atmospheric opacity and system temperature at millimetre wavelengths}
To provide correct flux units for the correlations from an
interferometer, we need to scale the
measured correlation coefficient at the correlator ($\rho$) by a
system temperature $T_{\rm sys}$ and a telescope sensitivity
factor ($K$). The telescope sensitivity factor is determined by the
overall antenna size and system efficiency.
\[
V = KT_{\rm sys}\rho
\]

At millimetre wavelengths, the atmosphere can no longer be approximated
as perfectly transparent. It degrades overall sensitivity in two
ways: the atmosphere emits radiation, and so raises the
system temperature, and the atmosphere attenuates the astronomical signal.
For a zenith opacity $\tau$, observing at an elevation of $e$, and
uniform atmosphere at $T_{\rm 0}$, then the atmospheric contribution to
system temperature is
\[
T_{\rm sky} = T_{\rm 0}\left(1-\exp(-\tau/\sin e)\right)
\]
and the atmospheric attenuation factor is $\exp(-\tau/\sin e)$.

In converting correlation coefficients to visibility measurements, we
can notionally include the effect of the atmospheric 
attenuation either in the effective system efficiency
factor, or in an effective system temperature.
The approach used will depend on the calibration scheme
implemented at the telescope.
\begin{itemize}
\item To account for it in the system temperature, one uses the so-called
``above atmosphere'' system temperature rather than a physical system
temperature. At its most basic,
the above atmosphere system temperature is simply
the physical system
temperature divided by the atmospheric attenuation. Its significance, however,
is
that the above atmosphere system temperature can be measured directly: it
can be deduced
by periodically flipping an absorber at atmospheric
temperature in front of the feed system.
The technique to do this, the so-called ``chopper wheel''
method, was devised by 
(\htmladdnormallink{Ulich 1980}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1980ApL....21...21U}).
The ATCA uses the chopper wheel method in its 3-mm system
to determine an above atmosphere
system temperature.
\item
At the ATCA, the 12-mm system uses a noise diode calibration scheme, and
so measures the physical system temperature of the receiver - not an above
atmosphere
value. The correction for atmospheric attenuation for this receiver package
is generally handled in the \task{atlod} step.
\end{itemize}

\section{Loading data into \miriad: ATLOD}
ATCA millimetre observations are loaded into \miriad\ in the same fashion
as centimetre data: task \task{atlod} is used. Note \task{atlod} is
smart enough to switch off the checks of the 3-mm data that are not
possible because of the lack of noise source calibration. 

\section{Initial ``fixes'' to millimetre data}\label{s:atfix}
This section details corrections that are often initially performed on 3-mm data.
Generally they are less important or not relevant at 12-mm, and so can be skipped.

These first
steps after loading can be done using the \task{atfix} task. 
Steps that \task{atfix} performs are:
\begin{itemize}
\item {\tt dantpos:} {\em Generally applicable at both 3- and 12-mm:} This parameter is used to correct for the apparent
difference in antenna locations between the millimetre and centimetre
systems. This correction should be performed on 3-mm data. It may
also be helpful 12-mm data, although it is much less important.
The input is typically an antenna parameter file that has been
determined for each array configuration. A collection of these parameter
files is stored in the \miriad\ system directory \verb+$MIRCAT+,
and should be updated as these solutions are found (and \miriad\ updated!).
These parameter files have names of the form ``\verb+dantpos.+{\em yymmdd}''
where ``{\em yymmdd}'' is the date of the start of a new array configuration.
If a data file is present, you can instruct \task{atfix} to read this
directly using the indirect parameter input. For example, to read
parameters appropriate for a hypothetical array configuration
starting on 16 October 2002, use
\begin{verbatim}
     dantpos=@$MIRCAT/dantpos.021016
\end{verbatim}

\item {\tt array:} {\em Generally applicable at 3-mm only:} One of the flaws of the current ATCA datafiles is that
antenna locations are not saved in the RPFITS file when antennas are
off-line. While this might not seem a serious flaw, the off-line antennas
can still cause shadowing. This will be an issue when using the
3-antenna system in compact arrays, i.e. 3-mm observing. By giving a value to this parameter, atfix will fill in any
antenna locations that are missing from the input visibility file.
{\em NOTE}: This just fills in missing antenna locations, it does not perform
any flagging of shadowed data.

The value given to this parameter is either an array configuration name
(e.g. EW352 or 750A) or a list of six station names (e.g. W106 or N3).
When giving the station names, these must be in the order of the antennas
(i.e. CA01, CA02, CA03 etc), regardless of any possible array shuffles.

{\em NOTE}: When antennas are in a shuffled order, or for arrays using
the north spur, you should generally give the list of station
names, as the standard array configuration names assume the
standard antenna order (not the shuffled order).

If in doubt the \htmladdnormallink{on-line configuration history}{confighist.html}
should be consulted.

\item {\tt options=tsys:} {\em Generally applicable at 3-mm only:} When
observing at 3mm, it is common to disable the scaling of the visibility
data to account for varying system temperature.
{\tt options=tsys} applies the system temperature data where needed.

\item {\tt options=gainel:} {\em Generally applicable at 3mm:}
Gravitational deformations in the ATCA antennas become significant
at 3mm, and so the antenna efficiency is a function of elevation.
This option applies corrections to the data to account for the change
in antenna efficiency.

No data is yet available on the gain/elevation effects at 12mm. They
can be assumed to be negligible.

\item {\tt options=opcorr:} {\em Not relevant at 3mm:}
For 12mm observations, where the system temperature measurements
are the physical system temperature (not the {\em above atmosphere} value),
a correction needs to be made for atmospheric opacity. {\em Generally this step
will be done by using \verb+options=opcorr+ in \task{atlod}}. However if you
failed to do it there, or if you have an observation made before October 2003
where \task{atlod} cannot apply the opacity correction, then you will need
to use \task{atfix} to perform this function. Option \verb+opcorr+ invokes
this function. For observations before October 2003, you will also need
to set the \verb+mdata+ keyword (see below).

\item {\tt mdata:} {\em Not relevant at 3mm:} 
When applying an opacity correction to 12-mm data, meteorological
measurements is required. However for observations before October 2003, meteorological data was
not included within \miriad\ datasets. This parameter gives the name of a file containing
meteorological measurements at the ATCA.
 
Meteorological parameters are continuously measured at the ATCA. The data
files giving these measurements are available on the web, and are updated at
midnight (Narrabri time) each night. You will want to use your browser
to download the appropriate weather data from
\begin{latexonly}
\begin{verbatim}
    http://www.narrabri.atnf.csiro.au/cgi-bin/Weather/hisdata.cgi
\end{verbatim}
\end{latexonly}
\begin{htmlonly}
\htmladdnormallink{the web}{http://www.narrabri.atnf.csiro.au/cgi-bin/Weather/hisdata.cgi}.
\end{htmlonly}
This has monthly weather files, with the boundary at midnight at the end of
the month -- Australian Eastern Standard time. If you observe over
a month boundary, you will need to download both months and concatenate
them together before running \task{atfix}.

\end{itemize}

Typical inputs at 3mm are:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf ATFIX}} \\
\hline
in=vela.uv		& Input visibility dataset.\\
out=vela.fixed.uv	& Output corrected visibility dataset.\\  
arrray=ew214		& Allow antenna locations to allow \\
			& for shadowing computation.\\
dantpos=@\$MIRCAT/dantpos.030716 & Millimetre antenna location\\
			& correction.\\
options=tsys,gainel	& Correct for $T_{\rm sys}$ and gain/elevation\\
			& changes.\\
\hline
\end{tabular}
\end{center}

\section{Tracking errors}
With the ATCA beam at 3mm being comparatively small ($35''$), it is
possible in some
circumstances for
tracking errors to be significant. Significant
tracking errors may occur during windy weather (gusts can blow
the antennas off the directed tracking position). In additional
poor tuning of the antenna drive systems can sometimes cause poor tracking,
particiularly soon after a change in source.

Note that tracking errors should be distinguished from pointing errors.
Tracking errors correspond to the inability of the antenna drive system
to follow the requested path. Pointing errors correspond 
to the difference between the position that the astronomer
requests and the actual position. Tracking errors are caused by
flaws in the drive servo system, whereas pointing errors are a sum of
tracking errors and errors in the antenna pointing model.

Since October 2003, tracking errors are saved in the \miriad\ dataset.
The errors can be plotted and the visibility data can be flagged based on 
their value. The uv variables corresponding to the rms and 
maximum tracking
error in a cycle are \verb+axisrms+ and \verb+axismax+. 

Both the maximum and rms give two tracking error values for each antenna, nominally corresponding
to the tracking error in the azimuth and elevation axes. However the ATCA on-line
system produces a single composite value, which is replicated for the two
axes in the output.

Tracking errors can be plotted with \task{varplt}. 

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf VARPLT}} \\
\hline
in=vela.uv		& Input visibility dataset.\\
device=/xs		& PGPLOT plotting device.\\  
yaxis=axisrms		& Plot rms tracking error. \\
\hline
\end{tabular}
\end{center}

When the tracking error is less than $2'$, the ATCA on-line
system believes that this error sufficiently small to consider
that the antenna is tracking. However this tolerance is not
generally good enough for 3-mm observations.
To flag based upon rms tracking error, the task \task{uvflag} can
be used by selecting visibilities using the
\verb+pointing+ subcommand (note this is an inappropriate and misleading name).

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf UVFLAG}} \\
\hline
in=vela.uv		& Input visibility dataset.\\
flagval=flag		& Flag selected points.\\  
select=pointing(15,1000)& Flag when the rms tracking error \\
			& is between $15''$ and $1000''$. \\
\hline
\end{tabular}
\end{center}



\section{Calibration}
\subsection{Bandpass, gain and polarimetric calibration}
Observations at 12mm follow an the same steps as at the centimetre
wavelengths to determining bandpass, gain and polarimetric calibration.
There is no difference in the use of \task{mfcal} and \task{gpcal} - see
Chapter~\ref{c:cal}. 

Observations at 3-mm is, however, differ. Polarimetry is not currently
possible with the 3-mm system, and consequently only task \task{mfcal} is needed
to perform bandpass and antenna calibration. One performs calibration as one
would as if the XY and YX correlations were not present. See
Chapter~\ref{c:cal}
for more details.

\subsection{Absolute flux calibration: Flux boot-strapping using planets}
\label{s:absmm}
At wavelengths shorter than a few centimetres, extragalactic sources
prove to be generally too variable or too resolved to be useful as flux
calibrators. So at these wavelengths, the blackbody emission from
the planets are often used as
flux standards, with the two most commonly used planets being Mars and
Jupiter. Mars' lack of a thick atmosphere, and its near absence of phases when
seen from Earth makes it a particularly simple object to model. Jupiter's
lack of prominent rings and its internal heat source also make it attractive.

In a somewhat analagous way to using observations of 1934-638 and \task{gpboot}
at centimetre wavelengths to correct the flux scale of an observation, 
in a millimetre observation you can use observations of Jupiter or Mars and 
\task{plboot}. \task{plboot}
contains models of the planets (planet major and minor diameters, ephemerides
giving distance and orientation, and a model of the change in brightness temperature with
frequency), and so it can compute the expected visibility function of a
particular planetary observation.

However, the usefulness of Mars and Jupiter depends very much on the
array configuration, and their distance to the Earth:
\begin{itemize}
\item
The planets are large, and so are commonly resolved out on typical
ATCA baselines.
At 3-mm, Jupiter is always
resolved out, and so Jupiter is only useful as a 12-mm calibrator.
At its closest approach approach to Earth, Mars is also resolved out at 3mm on
all ATCA baselines.
\item
The distance between the Earth and Mars varies by a factor of 7, and
so the flux density varies by a factor of 50. At times, Mars can be too weak
to be useful at 12mm. Jupiter's variation is
more modest: the distance to Earth varies by 50\%, and so the flux density
varies by a factor of about 2.
\end{itemize}

\begin{figure}
\begin{center}
\epsfxsize=14cm\leavevmode\epsffile{mmcal.ps}
\end{center}
\caption{Flow chart for flux boot-strapping from a planet.}
\label{fig:mmcal}
\end{figure}

Using planets as flux calibrators is somewhat different to 1934-638. The
normal calibration tasks (\task{mfcal} and \task{gpcal}) handle
point sources only. As far as most of the calibration process goes, the
planet observation should be treated as a program source. The steps to
take to flux bootstrap from a planet (see Fig.~\ref{fig:mmcal})
are as follows:
\begin{itemize}
\item Derive calibration tables with your secondary calibrator (and
perhaps bandpass calibrator).
\item Copy these tables across to your planetary observation, using 
\task{gpcopy}.
\item Use \task{plboot} to correct the flux scale of the calibration
tables. Note that \task{plboot} can take multiple input
datasets. In this case, it takes all the data from these, computes the
needed factor to correct the flux scale, and then applies it to the gain tables
of all the datasets.
You could ask \task{plboot} to process either just your planet
dataset. Alternatively you could ask it to process both the planet and
secondary calibrator. As \task{plboot}
ignores sources it does not recognise as planets, there is no harm in
doing the latter - it has the advantage that the secondary is then
flux calibrated.
\item Having generated calibration tables with the right flux scale,
these can then be copied across to the program sources.
\end{itemize}

Given the observing time and frequency, task \task{plplt} plots the
expected visibility function for a planet.
You will most likely want to check this visibility function
before observing and discovering that the planet is resolved out!
{\bf NOTE} that \task{plboot} and \task{plplt} do not include the effect of the
ATCA's primary beam -- the planet is implicitly (and silently) assumed to be
small compared with the primary beam. This may not be the case at 3-mm. 
If there is a significant change in the visibility function between baseline
lengths of 0 and 22~metres,
then primary beam effects are important, and you probably do not
want to use the planet (its probably resolved out anyway).

Task \task{planets} can give you basic information about a planet, including
rise and set times (but note these are quoted at the horizon, not the ATCA's
elevation limit), the planet's angular size at a particular time, and its 
RA and DEC (note that the
ephemeris used for RA and DEC is not sufficiently accurate for pointing
the telescope).

Information on using the ATCA to observe a planet is available
\begin{latexonly}
from
\begin{verbatim}
  http://www.narrabri.atnf.csiro.au/software/on-line/user/sched/solsys.html
\end{verbatim}
\end{latexonly}
\begin{htmlonly}
\htmladdnormallink{on-line}{http://www.narrabri.atnf.csiro.au/software/on-line/user/sched/solsys.html}
\end{htmlonly}

\section{Handling large spectral bandwidths}
When using a single spectral window, and observing spectral lines, the
bandwidth of the ATCA can be problematically narrow, or the channel 
widths can be too coarse. This is most likely to be an issue when observing
spectral lines of extragalactic sources at high frequencies.

A partial work around for this situation is to use the two ATCA spectral
windows to observe in adjacent somewhat overlapping windows, and using the
same bandwidths and channel counts for the two windows. With the ATCA this
is possible with some correlator configurations when observing with
bandwidths of 64~MHz or 128~MHz. \miriad\ allows you to stitch these
two spectral windows together in a reasonably straightforward fashion (although
some quirks do become apparent). In
so doing, it handles any overlap region between the two windows in a sensible
fashion. The steps to achieve this are as follows:
\begin{itemize}
\item Make sure you set a rest frequency in \task{atlod} (or during the
subsequent processing). If you are really want to view the spectrum in
frequency (rather than velocity), then you might set a dummy rest frequency
as the average of the centre frequencies of the two spectral windows.
\item During the flagging process, flag any channels in the overlap region
between the two spectral windows which you {\em do not} wish to contribute
to the final output stitched spectrum. These would be channels in one spectral
window where the bandpass gain is quite low in one, but reasonable in
another spectrum. Task \task{uvflag}'s \verb+edge+
parameter is the easiest way to flag these channels.

\item When running \task{uvsplit}, use \verb+options=nowindow+. This ensures
that the two spectral windows are not split apart by \task{uvsplit}. The output
of \task{uvsplit} will be datasets which contain the two spectral windows.

\item Use tasks \task{mfcal}, \task{gpcal} and \task{uvlin} as normal. These
tasks all handle multiple spectral windows within a single dataset.

However you will want to give some thought when using task \task{uvlin}.
Task \task{uvlin} processes each spectral window individually. So if the
spectral of interest in near the edge of the individual spectra, you will want
to set the channels to use in \task{uvlin}'s fitting process carefully. You
may wish to use a zeroth order fit, and just fit using channels at the
edges away from the overlap region. Alternatively you may wish to run
\task{uvlin} after the two spectra are stitched together instead. Some
experimentation (and certainly thought) will be required.

\item It is nearly time to stitch the two spectral windows together.
To do this you will need to use the ``velocity linetype''
One of \miriad's quirks is that you cannot apply on-the-fly
bandpass calibration
and use a ``velocity linetype'' within the same task. You will need
to make a copy of the dataset with the bandpass (and presumably other
calibration) applied directly to the data. If you have used
\task{uvlin}, you will have done this already. If not, create a copy
of your dataset using \task{uvaver}.

\item With a dataset with calibration applied, you can now stitch the two
spectral windows together.
See Sections~\ref{s:line} and \ref{s:veloline} for information on
using velocity linetypes.
Do the copying and linetype conversion with \task{uvaver} (or possible \task{uvlin}
if you have not already used it) to generate a new stitched
copy of the data.
By using a velocity linetype, when creating an output spectrum,
\miriad\ copies channels by their velocity. Any channels that
overlap between the two spectral windows are averaged together.
\end{itemize}
%\end{document}
