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
model of the atmosphere (using meteorological data contained
within the dataset) to compute a model opacity.  Note that this is just
that -- a model -- and so has its limitation.  To apply atmospheric
opacity correction, use \verb+options=opcorr+. 

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf ATLOD}} \\
\hline
options=opcorr,birdie,reweight,xycorr	& Options for typical 12-mm \\
					& continuum observation, or\\
options=opcorr,hanning,compress		& possible options for spectral\\
					& line observations.\\
\hline
\end{tabular}
\end{center}

Please note the following:
\begin{itemize}
\item ATCA observations made before October 2003 failed to contain the
meteorological data, and so \verb+options=opcorr+ cannot be used. 
\item At 12mm, no self-generated RFI at multiples of 128~MHz is seen. Task
\task{atlod}'s \verb+birdie+ option for continuum data is
used to discard edge channels and every second channel (as noted earlier,
successive channels are not independent, and so there is no sensitivity
penalty in discarding every second channel).
\end{itemize}

\section{Calibration}
For 12-mm data, calibration is essentially identical to the centimetre bands.
Primary flux calibration can be done using \task{gpboot} and observations
of 1934-638, in much
the same fashion
as at the lower frequencies. 1934-638 is about 1 Jy at 12-mm wavelengths: the
\miriad\ calibration software contains a built-in model of 1934-638's change
of flux density with frequency. For information about this flux scale,
see the \htmladdnormallink{memo by Bob Sault}
{http://www.atnf.csiro.au/observers/memos/AT39.3_124.pdf}
``ATCA flux density scale at 12mm'' (2003).

As 1934-638 is comparatively week at 12-mm wavelength, it is not an
good bandpass calibrator. If you require a bandpass
calibration, you should take observations of a strong source such as
1921-293 or 1253-055.

Because atmospheric opacity affects the amplitude gain calibration step, it is
important to consider it when high precision flux calibration is required.
In the flux bootstrapping step,
\task{gpboot} determines the boot factor to be applied to the gains of
the secondary calibrator to make them the same as the primary. As the opacity
correction 
done by \task{atlod} is only correct to first order,
ideally the comparison between primary and secondary gains would be made
{\em at the same elevation and under the same weather conditions}.
Selecting the range of times that the gains
should be compared is a more important issue at 12mm than at lower frequencies.
To select the time range of the secondary to compare, use \verb+time+ selection
in the \verb+select+ keyword.

Some judgement will need to be made about the set of amplitude gains of
the secondary to use in the comparison.  In stable weather conditions,
it is probably best to select the time range of the secondary where its
elevation corresponds most closely to the elevation during the primary's
observation.  When the weather changes significantly during the
observation, it is probably best to use a time range of the secondary
which is close to the primary's observation.  Tasks \task{uvplt} and
\task{varplt} can plot source elevation as a function of time. 

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



%************************************************************************
%************************************************************************

\newchapter
\chapter{Reducing 3-mm ATCA observations}\label{c:3mm}\index{millimetre data}
By many measures, the ATCA 3-mm observing band is a completely different
domain to the other ATCA bands. The science is different, the weather is much
more important, and the mode of observing is quite different. Not surprisingly
many aspects of the data reduction are also different. This chapter reviews
the theory behind some of the differences, and then considers a practical
approach to calibration.

\section{Some theory}
\subsection{Atmospheric opacity and system temperature}
To provide correct flux density units for the visibilities, $V$, from an
interferometer, the
measured correlation coefficient at the correlator ($\rho$) needs to be
scaled by a
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
if the atmosphere is approximated as having a uniform temperature
$T_{\rm 0}$, then the atmospheric contribution to
system temperature is
\[
T_{\rm sky} = T_{\rm 0}\left(1-\exp(-\tau/\sin e)\right)
\]
and the atmospheric transmissivity is $\exp(-\tau/\sin e)$.

In converting correlation coefficients to visibility measurements, we
can notionally include the effect of the atmospheric 
attenuation either in the effective system efficiency
factor, or in an effective system temperature.
The approach used will depend on the calibration scheme
implemented at the telescope.

For the ATCA at 3-mm wavelength, it is most natural to include the effect of the
opacity in an effective system temperature -- the so-called
``above atmosphere'' system temperature.
The above atmosphere system temperature is simply
the physical system
temperature divided by the atmospheric transmissivity. The
widespread use of above atmosphere system temperatures in millimetre
astronomy
follows on because the above atmosphere system temperature can be measured 
directly! It
can be deduced
by periodically flipping an absorber at atmospheric
temperature in front of the feed system.
The technique to do this, the so-called ``chopper wheel''
method (e.g.
\htmladdnormallink{Ulich 1980}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1980ApL....21...21U}).
The ATCA uses the chopper wheel method (also call paddle or vane calibration)
in its 3-mm system
to determine an above atmosphere
system temperature.

This measurement will only be done occasionally. That is, unlike the
centimetre and 12-mm bands, continuous measurement of system temperature 
is not available at 3-mm wavelength. To partly remedy this, \miriad's \task{atfix}
includes an algorithm to ``interpolate'' the above-atmosphere
system temperature measurements
based on changes in telescope elevation and weather.

\subsection{Antenna gain considerations and flux calibration}
One characteristic of the ATCA dishes is that they are big by 3-mm
standards. Indeed \htmladdnormallink{van Hoerner (1967)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1967AJ.....72...35V}
has considered the limits to the size
of an antenna before various effects become significant.
For construction styles like the ATCA dishes, he suggests that 11m is the
maximum dish size for 3-mm observations before thermal effects become
significant.
As the ATCA dishes are twice this, it is not surprise that the
ATCA antenna gain and primary beam response varies as a result of thermal
distortions. Changes in temperature, the position of the sun, cloudiness and
shadows will all affect the gain of the ATCA antennas. Measurements to
date
tend to bear this out, with changes in antenna gain of 25\% noted that
appear to be related to thermal effects.
Distortion of the dishes by gravity is also significant
(eg \htmladdnormallink{Subrahmanyan 2005}
{http://ieeexplore.ieee.org/iel5/8/32084/01492610.pdf?tp=&arnumber=1492610&isnumber=32084}). 
A gain/elevation
correction will solve this - to first order at least.
Because of the way the ATCA panels are set, the antenna gain peaks at
about 60\degrees\ elevation. 

Note that gravitational
and thermal distortions coupled - a perfect correction for these effects
is never possible. 

To first order, the gain change will be calibrated out provided the 
secondary calibrator is very close to the target source.
Applying the standard gain/elevation curve to the data as the first step in
the data reduction will also help.
However these two steps will
not account for the change and the beamshape. This might be an important
effect for widefield imaging and mosaicing.

The best way to avoid thermally-induced gain variations is by observing
in the pre-dawn hours or on a calm cloudy day! The best way to avoid large
elevation changes in a synthesis is to observe in a hybrid array -- hybrid arrays
allow a synthesis to be performed without the need to track the source from
horizon to horizon.

The gain variations must be considered when bootstrapping the flux density 
scale. The best approach to ensuring a good flux scale is to ensure that the
secondary and the flux density calibrator are observed nearly
simultaneously and at the same elevation.
Although this may at first glance sound difficult if not impossible, generally
it is straightforward. Generally there should be a time when the secondary
and the flux
density calibrator are at the same elevation. It may be that one is rising
while the other is setting, and so they are at significantly different parts
of the sky (ie significantly different azimuths). Unless the sky is
cloudy, being at different azimuths is not important.

\subsection{Instrumental phase}
At the precision needed for 3-mm observing, the ATCA antennas are not identical
in terms of their physical construction. In particular the offset
between the elevation and azimuth axis will differ between antennas.
VLBI astronomers, who have used heterogeneous arrays of antennas, have
coped with this issue for years. For the ATCA at 3mm, what is presumed to
be a difference in this offset introduces a instrumental phase error, that
needs to be corrected.

\subsection{Atmospheric phase and phase calibration considerations}\label{s:weakcal}
Phase calibration at 3-mm wavelength is significantly more difficult than
at the longer wavelengths. This is because the atmospheric phase errors are
comparatively larger, the secondary calibrator grid is sparser and the
system sensitivity is poorer. To use a tolerably nearby calibrator, you will
need to use a calibrator that is weaker than you might wish - particularly if
you wish to observe in a narrow bandwidth. Alternatively you
might use a nearby strong SiO maser rather than continuum point source
phase calibrator.

These lead to a different approach to calibration compared to that
at longer wavelengths.
At 3-mm wavelengths, the dual frequency band capability of the ATCA can be used to
good effect in the calibration process. When sensitivity to calibrators
is an issue, one of the two bands can be specifically set aside
for calibration purposes. The
correlator configuration can be selected so that one of the bands
is either 128 MHz wide (and so as sensitive as possible), or suitable for
observing an SiO maser. This calibration band can also be used for
reference pointing during the course of the observation.

When using one of the bands for calibration purposes, the two
bands need to be jointly calibrated.

In a typical 3-mm observations, in addition to the target source,
your will observe a bandpass calibration, a secondary calibrator and
a flux density calibrator.

SiO masers should be used with caution: they are often
strongly polarised, and they may be structurally complex. Note, also, that
the ATCA limits the dual observing bands to be set within 2.7~GHz of each other.
If you use an SiO source as a calibrator, the part of the 3-mm band that is
accessible to the observation is limited.

\subsection{Flux density calibrators}
\label{s:absmm}
At wavelengths shorter than a few centimetres, extra-galactic sources generally
prove to be too variable to be useful as flux density
calibrators. So at these wavelengths, the blackbody emission from
the planets are often used as
flux standards. The two most commonly used planets for the ATCA at
3mm are Mars and Uranus. Note Jupiter is too large for use as an ATCA
flux density calibrator. Neptune would also be a possible flux density calibrator. However
Neptune is quite close to and half the strength of Uranus, and so does not offer
anything over Uranus.

Uranus is the preferred flux density calibrator for the ATCA.
Mars is less suitable for a number of reasons:
some models of Mars' average brightness temperature suggest
variations in the temperature of up to
10\% as a result of Mars' diurnal rotation, its distance from the
Sun and the viewing geometry of Earth-based observers. Definitive measurements
and confirmation of
these models are poorly studied in the literature. Depending
on its distance and the array configuration, structure on Mars is
also readily detected with the ATCA: Mars has hot
equatorial and cold polar regions.

Although Uranus or Mars (preferably Uranus) are the best flux density
calibrators, frequently they will not be accessible to an observation. To
account for this, the Narrabri Observatory regularly monitors some 
additional sources (usually 1253-055 and 1921-293) to allow them to be used
as flux density calibrators. Be warned though that these sources can
vary substantially on month timescales.
If Uranus and Mars are not accessible to your observation, you should enquire
about the next best source to be used for flux density bootstrapping.

\section{Practical loading and calibration}
When observing at two bands with the ATCA, it is generally sensible to
set the two bands so that they do not overlap. In terms
of sensitivity, there is 
nothing to be gained by setting the bands to the same frequency as the noise
will be common to the the bands.
However having the bands overlap complicates the data reduction 
process in \miriad.
The best approach is to avoid the problem, by setting the bands so that
they do not overlap at all, or overlap by a comparatively small amount.

\subsection{Loading data into \miriad: ATLOD}
ATCA millimetre observations are loaded into \miriad\ in much the same fashion
as centimetre data: task \task{atlod} is used.

One difference is that it is {\it generally desirable} to load both
observing bands together, to allow joint calibration. A shortcoming of
\miriad\ is that it is poor at handling multiple observing bands
{\it when the polarisation products differ between the bands}. However
the standard ATCA correlator configurations that combine narrowband
and wideband observations generally do measure different polarisation
products in the two bands. As polarimetry
with the ATCA 3-mm system is not possible anyway, the simplest approach
to ensuring the polarisation products of the two bands are the same is by
discarding polarisation products that are not needed. The
\task{atlod} option \verb+nopol+ causes \task{atlod} to discard any XY and YX
products.

Typical inputs to \task{atlod} are:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf ATLOD}} \\
\hline
in=2006-02-18\_1234.C123	& Input RPFITS file.\\
out=vela.uv		& Output \miriad\ dataset.\\  
options=nopol,birdie,reweight	& Typical options used.\\
\hline
\end{tabular}
\end{center}


The \verb+birdie+ option for continuum data is
used to discard edge channels and every second channel:
for the continuum ATCA correlator configuration, successive channels are
not independent and so there is no sensitivity penalty in discarding
every second channel.

\subsection{Initial ``fixes'' to millimetre data}\label{s:atfix} 
The
next step after loading is to apply a number of ``fixes'' that are
important for 3-mm data.  Note that there is no record in the data
that these steps have been performed. In particular the software
will allow you to apply the fixes twice, which would be just
as bad as not applying them at all.
The task to do these, \task{atfix},
performs the following functions:
\begin{itemize}

\item By default \task{atfix} applies the ``standard'' gain/elevation
model for the antennas.  You can disable this by using
\verb+options=nogainel+. 

\item By default \task{atfix} corrects for a ``standard'' instrumental phase
model.  You can disable this with \verb+options=noinst+.

\item As mentioned above, the ATCA uses the chopper wheel method to
measure an above atmosphere system temperature.  The nature of the
hardware means the measurement is done only typically every 5 to
30~minutes.  When observing, the ATCA on-line system can either 
apply a nominal system
temperature value to the visibility data, or it can apply the actual
measured system temperature values.  In applying these measured values, 
on-line system will apply the
most recent measurement. 

Task \task{atfix} includes an algorithm to interpolate measured
system temperature values, between measurements, and to correct the 
visibility data accordingly. In doing this it correctly handles whether
a nominal or the measured system temperature value applied to the
data on-line. The interpolation
algorithm takes into account changes in elevation and meteorological
conditions.

The parameter \verb+tsyscal+ can be used to change this default behaviour.
This parameter can take on values to revert the data back to the nominal
system temperature value, to make no changes to the system temperature scaling,
to use the most recent system temperature measurement, or to use an
extrapolation (as distinct from interpolation) algorithm in estimating
system temperatures.

\item Often you will want to apply a correction to your data to
better account for the exact locations of the antennas.

On the night of an ATCA reconfiguration, a solution for the antenna
positions is derived. The practise is to install this solution as
the ``standard solution''. The on-line system will then use this solution in
subsequent observations in that array configuration.
The quality of this solution is important for the 
ultimate phase calibration of 3mm data.

Experience has shown that often
the position solution derived at the time of the reconfiguration is not
necessarily the best possible. For example the solution will depend
critically on the atmospheric phase stability at the time of the solution,
and this might be less than ideal on the night of the reconfiguration.
Consequently improved solutions will often be derived at a later time. 
If improved
solutions become available, then 3-mm observers should correct their
data accordingly.

Updated solutions are stored in parameter files in the 
\miriad\ system directory \verb+$MIRCAT+. These solutions
are distributed via the \miriad\ update mechanism, and
have file names of the form ``\verb+dantpos.+{\em yymmdd}''. Here
where ``{\em yymmdd}'' is the date of the relevant array reconfiguration.

Task \task{atfix} corrects the data for an improved antenna location
solution via the {\tt dantpos} parameter.
If an improved solution file is present, you can instruct \task{atfix}\
to read this
directly using the indirect parameter input mechanism 
(see Section~\ref{s:atfile}). For example, to use the antenna location
solution appropriate for a hypothetical array reconfiguration
that happened on 16 October 2002, use
\begin{verbatim}
     dantpos=@$MIRCAT/dantpos.021016
\end{verbatim}

\item One of the flaws of the current ATCA  RPFITS files is that
antenna locations are not saved when antennas are
not in use. While this might not seem a serious flaw (who cares about
the antennas if they are not in use?), the unused antennas
can still cause shadowing -- true antenna locations are needed
for shadowing calculation. This was a particular issue with the interim
3-antenna 3-mm system when observing in compact arrays. 

By giving a value to the {\tt array} parameter, \task{atfix} will fill in any
antenna locations that are missing in the input visibility file.
{\em NOTE}: This just fills in missing antenna locations, it does not perform
any flagging of shadowed data.

The value given to this parameter is either an array configuration name
(e.g. EW352 or 750A) or a list of six station names (``W106'' and ``N3''
are examples of station names).
When giving the station names, these must be in the order of the antennas
(i.e. CA01, CA02, CA03 etc), regardless of any possible array shuffles.

{\em NOTE}: When antennas are in a shuffled order, or for arrays using
the north spur, you should generally give the list of station
names, as the standard array configuration names assume an
antenna orderwhich is probably not correct.

If in doubt the \htmladdnormallink{on-line configuration history}{confighist.html}
should be consulted.

\end{itemize}

Typical inputs to \task{atfix} are:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf ATFIX}} \\
\hline
in=vela.uv		& Input visibility dataset.\\
out=vela.fixed.uv	& Output corrected visibility dataset.\\  
dantpos=@\$MIRCAT/dantpos.021016 & Antenna location correction.\\
\hline
\end{tabular}
\end{center}


\subsection{Additional monitoring variables}
In addition to the plethora of uv variables that can be examined by
an observer, the following are particularly relevant for 3-mm data.
\begin{center}
\begin{tabular}{|l|l|}
\hline
smonrms		& ATCA seeing monitor rms (in microns) \\
refpnt		& Reference pointing solution (in arcseconds). \\
axisrms		& RMS tracking accuracy\\
axismax		& Maximum tracking accuracy\\
\hline
\end{tabular}
\end{center}

These variables can be plotted or listed with \task{varplt}, e.g.

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

\subsubsection{Reference pointing solutions}
The variable {\tt refpnt} gives the offset from the global solution,
in azimuth and elevation, of a reference pointing solution.

\subsubsection{Seeing monitor measurement}
The seeing monitor is an independent interferometer which constantly
measures the atmosopheric phase stability to a geostationary satellite.
The baseline length is 220m, and the satellite elevation
is 60\degrees. The measurement is expressed in microns of excess path.

\subsubsection{Tracking accuracy}
With the ATCA beam at 3mm being comparatively small ($35''$), it is
possible for
tracking errors to be significant. This
may occur during windy weather (gusts can blow
the antennas off the directed tracking position). In additional
poor tuning of the antenna drive systems can sometimes cause poor tracking,
particiularly soon after a change in source.

Note that tracking errors should be distinguished from pointing errors.
Tracking errors are the inability of the antenna drive system
to follow the requested path. Pointing errors correspond 
to the difference between the position that the astronomer
requests and the actual position. Tracking errors are caused by
flaws in the drive servo system, whereas pointing errors are a sum of
tracking errors and errors in the antenna pointing model.

Since October 2003, tracking errors have been saved in the \miriad\ dataset.
The uv variables corresponding to the rms and 
maximum tracking
error in a cycle are \verb+axisrms+ and \verb+axismax+. 

Both the maximum and rms give two tracking error values for each antenna, nominally corresponding
to the tracking error in the azimuth and elevation axes. However the ATCA on-line
system produces a single composite value, which is replicated for the two
axes in the dataset.

When the tracking error is less than $2'$, the ATCA on-line
system believes that this error sufficiently small to consider
that the antenna is tracking. However this tolerance is not
generally good enough for 3-mm observations.
The task \task{uvflag} can be used to flag based upon rms tracking error.
To do this, use the selection
\verb+pointing+ subcommand.

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

\subsection{Bandpass and antenna gain calibration}
When calibrating 3-mm data, it is possible to use a similar
approach as with lower frequency data  -- see
Chapter~\ref{c:cal}. In this case, because polarimetry is not currently
possible with the 3-mm system, only task \task{mfcal} is needed
to perform bandpass and antenna calibration. Flux density bootstrapping
can be accomplished using \task{mfboot} described below.

However, the recommended approach at 3mm is different from this.
As noted in Section~\ref{s:weakcal}, calibration at 3mm is more
difficult because of weak calibrators, poorer system sensitivity and
more challenging atmospheric conditions. This section outlines an
alternative calibration approach exploiting the dual frequency bands of the
ATCA. The steps are as follows:

\begin{enumerate}
\item The first step is not a step at all! Unlike the approach of
Chapter~\ref{c:cal}, the approach described here {\it does not} split the 
data into separate single source, single frequency files before
calibration. Rather the 
current approach assumes that you have a single \miriad\ dataset containing
bandpass calibrator, secondary calibrator, flux density calibrator and 
target source. We focus on data having two simultaneous frequency bands
here, but the approach works equally well for one or many simultaneous
frequency bands within the dataset.

\item The first real step is akin to a bandpass calibration step. When
dealing with two frequency bands, each with two polarisation products
(XX and YY), it is
generally appropriate to
assume that the bandpasses of the bands and the phase offsets between
bands and polarisations are constant with time. 

To determine the bandpasses and the phase offsets, you need
to use \task{mfcal} on an observation of a strong continuum source.
Typically this would be 
1253-055, 1921-293 or 0537-441. 
Typical inputs are as follows:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MFCAL}} \\
\hline
in=vela.fixed.uv	& Input multi-source, dual-band dataset.\\
select=source(1921-293)	& Select the strong continuum ``bandpass'' calibrator.\\  
\hline
\end{tabular}
\end{center}

The important output of this step is a bandpass calibration table which
will contain the relevant phase offsets. The output also contains
antenna gain calibration tables, but this will be overwritten below.

In doing this step, you may well want to avoid using some edge
channels in the bandpass calibration process. To achieve this, you
can use the {\tt edge} parameter of \task{mfcal}. The {\tt edge} parameter
sets the number of channels to drop for all bands equally. It does not
allow you to set different number of channels to drop in the different bands.
If the channels and bandwidth characteristics of the different bands differ
significantly, the {\tt edge} parameter probably does not give you
sufficient flexibility. In this case, you could use \task{uvflag} to
explicitly flag edge channels.

\item The next step is to determine the antenna gain calibration, again
using \task{mfcal}. To do this,
you will want to select those data for the secondary calibrator which
will give the best overall sensitivity appropriate to your observation.
\begin{itemize}
\item If the calibrator is a continuum source, you need only select this
source and use both bands. Task \task{mfcal} will weight the two bands
according to their relative bandwidth. Consequently if one of the bands
is quite narrow relative to the other, then it will contribute little to
the overall gain solution.
\item If the calibrator is an SiO maser, you will want to select only those
channels that contain strong SiO signal. 
\item With either a continuum calibrator or an SiO maser, it is possible to
either determine antenna gain calibrations for the two polarisation products 
independently, or to determine a
single joint solution for the two polarisations. The latter will give
a $\sqrt{2}$ improvement in sensitivity in the solution process.
The latter is also the
desirable approach if the calibrator is strongly polarised (some SiO 
masers are more than 50\% polarised). Doing a joint solution is
generally a reasonable approximation given that the antenna gains
are dominated by changes common to both polarisations: in amplitude, it is
from the physical antenna 
gain change, and in phase it is from the atmosphere and instrumental 
contributions common to both polarisations.
\end{itemize}

Note in this calibration scheme, it is implicitly assumed that the antenna 
gain changes are common to both frequency bands. This is a reasonable
approximation given that the frequency separation between the bands is
at most 2.7 GHz and potentially much less. Consequently the fractional
frequency separation in at most a few percent, and so the calibration change
between bands can be assumed to be minimal.
\end{enumerate}

Typical inputs for \task{mfcal} follow. This assumes a continuum
source as the secondary calibrator and does a joiubt solution for the
two polarisations:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MFCAL}} \\
\hline
vis=vela.fixed.uv	& Input multi-source, dual-band dataset.\\
select=source(1622-297)	& Select secondary calibrator.\\  
options=nopassol	& {\it Do not} solve for bandpass.\\
stokes=i		& Do a joint solution for both polarisations.\\
\hline
\end{tabular}
\end{center}
To derive a separate solution for the two polarisation products, the
{\tt stokes} keyword would be left unset. Note the use of \verb+options=nopassol+.
This causes \task{mfcal} to not attempt to solve for the
bandpass and phase offsets again, but rather to apply the previously 
determined bandpass and phase offset solutions.

Typical inputs when using an SiO maser would be as follows:
Typical inputs to \task{mfcal} are as follows:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MFCAL}} \\
\hline
vis=vela.fixed.uv	& Input multi-source, dual-band dataset.\\
select=source(oceti)	& Select SiO secondary calibrator.\\  
options=nopassol	& {\it Do not} solve for bandpass.\\
stokes=i		& Do a joint solution for both polarisations.\\
line=chan,20,45		& Select the range of channels where the SiO\\
			& signal is strong.\\
\hline
\end{tabular}
\end{center}

\subsection{Flux density bootstrapping}
The best task to correct the flux scale of an observation is \task{mfboot} -
\task{mfboot} performs an analagous function to \task{gpboot}. Task
\task{mfboot} also supercedes the now obsolete task \task{plboot}.
It works by scaling the antenna gain table so that the flux density
scale of the observed data agrees with some model.

Task \task{mfboot} models a planet as a simple disk with constant brightness 
temperature. It has built-in ephemerides of the planets, and so
knows their apparent size and orientation. It also has a model
of the brightness temperature variation with frequency. Hence \task{mfboot}
can generate a nominal visibility function of a planet.
This model for Mars is not
sophisticated, but should be accurate to better than 10\% if appropriately
used.

Task \task{mfboot} can also be used when bootstrapping from a point
source. Although \task{mfboot} contains models for sources such as 1934-638,
these models are not relevant for 3mm observers. If using a point source,
generally the user would need to give \task{mfboot}
the flux density of the source.

It is implicitly assumed in the bootstrapping approach described here
that the flux density calibrator is observed near simultaneously and at
the same elevation as the secondary calibrator.  This is required
because the antenna gain changes with both elevation and with thermal
changes.  The most reliable approach to tying the secondary and the flux
density calibrators flux scales together is through observations with
the same elevation and thermal environment. 

Task \task{mfboot} will apply the bandpass and antenna gain
solutions to the observed data before it does a comparison with the 
models. While it is not generally important that the flux density calibrator
data is phase calibrated, it is important that it has the antenna amplitude
calibration, derived from the secondary, is applied. This is critical in
tying the flux density scales of secondary and flux density calibrator.

We will discuss the various input parameters for \task{mfboot}:
\begin{itemize}
\item {\tt vis}: These give the input visibility dataset.
\item {\tt select} and {\tt line}: This selects the data to be used as the
flux density calibrator.
\item {\tt flux}: If using a point source as a flux density calibrator, use
this parameter to set the flux density of the source (in Jansky). If using
a planet, you may set this parameter (in Kelvin) 
to override the brightness temperature
value that
\task{mfboot} would use by default.
\item {\tt mode}: The comparison between the model and observed
data can be done using
either the scalar average (effectively the amplitude of the data), 
the vector average (effectively the
real part of the data), or the triple product of the data. The amplitude and
triple product are insensitive to phase errors, and so are normally the
most appropriate. The default is to use the triple product for point
sources and the amplitude for planets. For planets that are not
worse than mildly resolved, you might consider using the triple product
for planets.

When using scalar mode with significantly resolved planets, where there is
little flux on long spacings, you might consider using the {\tt clip}
parameter so as to exclude data that would simply contribute
noise bias.

\item {\tt clip}:
If Mars is used as a flux density calibrator, you will want to ignore
those visibilities where the deviation of the visibility function from
a simple disk model is significant. In practise, it is best to ignore
the visibility data outside the main central lobe of the visibility
function. The way to do this is by using the {\tt clip}
parameter.
For Mars it is recommended that you set \verb+clip=0.4+, which 
will ignore
data where the visibility function model is less than 40\% of the
model zero spacing value.

\item {\tt device}: \task{mfboot} can produce a plot of the the data
(after bootstrapping) and model visibilities to give you a quantitative
feel for the quality of the fit.
\item {\tt options}: The only available option is {\tt noapply}, which causes
\task{mfboot} to perform all steps {\em except} applying the bootstrap
factor.
\end{itemize}

In the general case, to use \task{mfboot} with the calibration scheme 
recommended above is
straightforward enough: you simply run \task{mfboot} on the dataset, having
selected the flux density calibrator. For example:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MFBOOT}} \\
\hline
vis=vela.fixed.uv	& Input multi-source, dual-band dataset.\\
select=source(uranus)	& Select flux density calibrator.\\  
\hline
\end{tabular}
\end{center}


\subsection{Information on planets}
Given the observing time and frequency, task \task{plplt} plots the
expected visibility function for a planet.
You will most likely want to check this visibility function
before observing and discovering that the planet is resolved out!
{\bf NOTE} that \task{mfboot} and \task{plplt} do not include the effect of the
ATCA's primary beam -- the planet is implicitly (and silently) assumed to be
small compared with the primary beam. This may not be the case at 3-mm. 
If there is a significant change in the visibility function between baseline
lengths of 0 and 22~metres,
then primary beam effects are important, and you probably do not
want to use the planet (most likely it would be resolved out anyway).

Task \task{planets} can give you basic information about a planet, including
rise and set times (but note these are quoted at the horizon, not the ATCA's
elevation limit), the planet's angular size at a particular time, and its 
RA and DEC (note that the
ephemeris used for RA and DEC is not sufficiently accurate to point or
phase track the telescope).

Information on using the ATCA to observe a planet is available
\begin{latexonly}
from
\begin{verbatim}
  http://www.narrabri.atnf.csiro.au/observing/sched/solsys.html
\end{verbatim}
\end{latexonly}
\begin{htmlonly}
\htmladdnormallink{on-line}{http://www.narrabri.atnf.csiro.au/observing/sched/solsys.html}
\end{htmlonly}

\section{Handling large spectral bandwidths}
When using a single spectral window, and observing spectral lines, the
bandwidth of the ATCA can be problematically narrow, or the channel 
widths can be too coarse. This is most likely to be an issue when observing
spectral lines of extragalactic sources at high frequencies.

A partial work around for this situation is to use the two ATCA 
frequency bands to observe in adjacent somewhat overlapping windows, and using the
same bandwidths and channel counts for the two windows. With the ATCA this
is possible with some correlator configurations when observing with
bandwidths of 16, 64 or 128~MHz. \miriad\ allows you to stitch these
two spectral windows together in a reasonably straightforward fashion (although
some quirks do become apparent). In
so doing, it handles any overlap region between the two windows in a sensible
fashion. The steps to achieve this are as follows:
\begin{enumerate}
\item Make sure you set a rest frequency in \task{atlod} (or during the
subsequent processing). If you really want to view the spectrum in
frequency (rather than velocity), then you might set a dummy rest frequency
as the average of the centre frequencies of the two spectral windows.
\item During the flagging process, flag any channels in the overlap region
between the two spectral windows which you {\em do not} wish to contribute
to the final output stitched spectrum. These would be channels
where the bandpass gain is quite low in one, but reasonable in
another band. Task \task{uvflag}'s \verb+edge+
parameter is the easiest way to flag these channels.

\item Perform the calibration as described above, keeping both bands
within the one file.

\item Subtract any continuum, using task \task{uvlin} as normal. Task
\task{uvlin} handles multiple spectral windows within a single dataset.

Task \task{uvlin} processes each frequency band separately.
You will want to give some thought when setting the range of
channels to use in the fitting of the continuum.
If the
spectral of interest in near the edge of the individual spectra, you will want
to set the channels to use in \task{uvlin}'s fitting process carefully. You
may wish to use a zeroth order fit, and just fit using channels at the
edges away from the overlap region. Alternatively you may wish to run
\task{uvlin} after the two spectra are stitched together instead. Some
experimentation (and certainly thought) will be required.

Note that
when handling multiple bands, both \miriad's \verb+line+ and \task{uvlin}'s
\verb+chans+ parmaters number channels consecutively from 1 to the total 
number of simultaneous channels. The distinction between simultaneous bands is
ignored in this numbering. But when fitting to the continuum, the
boundaries between bands act as a break point.

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
\end{enumerate}

%\end{document}
