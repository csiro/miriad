\chapter{UV Variables}
\label{app:uvvars}
% History:
%   Feb 27, 1989 -- Brian Sutin
%   Mar 26, 1989 -- Bob Sault -- Corrected comment on vsource.
%   May 2,  1989 -- Bob Sault -- Corrected descriptions of
%				 TIME, VSOURCE and VELDOP.
%   Oct 6,  1989 -- WH	      -- Added axisrms.
%   Oct 10, 1989 -- Bob Sault -- Added more comments about axisrms.
%				 Corrected comment about Julian date.
%   Oct 18, 1989 -- Bob Sault -- What is ATTEN??
%   Jan 31, 1990 -- Bob Sault -- Recorrected TIME description!
%   Feb  8, 1990 -- Bob Sault -- More comments for TELESCOP and WCORR.
%   Mar  8, 1990 -- Bob Sault -- Improved comments on "baseline".
%   Apr 25, 1990 -- Bob Sault -- Improved comments on "dra" and "ddec".
%
%   27-jun-90 pjt  started conversion to latex table; from UVVARS.TXT
%		   (a noble job - rjs).
%   7-aug-90  pjt  finished it off appropriately while at Hat Creek
%   6-nov-90  rjs  Added the history comments, and variables "wsystemp"
%		   and "on".
%  27-nov-90  rjs  Many minor corrections to Tex instructions. Got rid
%		   of the "table" environment setting (will pjt like this??).
%  27-nov-90  pjt  How about missing `evector' and `pbfwhm'
%		   Is it `npol' or `npols'????
%  20-jan-91  pjt  fixed a few non-alphabetic errors, included proposed
%                  polarization stuff from Mel's nov-30 mail message
%   5-feb-91  pjt  added item list for UV files, cleared up obstype item
%                  formally calling things RALINT variables, and not 
%                  'Hat Creek variables'
%   7-feb-91  pjt  Clarified a few more variables...
%  21-feb-91  pjt  added comments from Mel, fixed some margins stuff
%  26-feb-91  pjt  more Mel comments added, gains...
%   4-mar-91  rjs  Description of polarization and single dish variables.
%   3-apr-91  pjt  smallos
%  17-aug-92  rjs  many misc mods (m**3).
%   2-aug-93  rjs  Change units of corfin,corbw to MHz.
%  25-aug-93  rjs  Made some of the descriptions more precise.
%  19-nov-93  rjs  Some AT-specific variables.
%  xx-xxx-xx  rjs  I have been slack at keeping the history up-to-date.
%   4-jun-96  rjs  Added bin and nbin.
%  05-dec-96  rjs  Added delra,deldec,pntra,pntdec
%  20-mar-97  pjt  Added some recently added HatCreek variables, and "project"
%  21-jan-03  rjs  Added new baseline convention and pbtype.
%  30-dec-03  rjs  Add new weather variables.
%  20-feb-06  rjs  Added axismax, smonrms, refpnt, calcode, name

A \miriad\ uv dataset is composed of a collection of items and `\MM{u-v} variables'.
The variables are parameters that are known at the time of the observation,
and include measured data, and the description of the observation
set up ({\it e.g.} correlator set up and observing centres).

Table~\ref{t:vis-items} gives a list of the items that are used to build up a
\miriad\ uv dataset.
 

\begin{table}
\caption{\miriad\ items in a uv visibility dataset}
\label{t:vis-items}
\begin{center}
\begin{tabular}{|l|l|l|}\hline
Item name   & Type  &  Description \\ \hline
obstype     & ascii   & value: `{\tt cross}', `{\tt auto}' or `{\tt mixed}' \\
history     & text    & history text file (in principle editable) \\
vartable    & text    & lookup table for all uv variables (do not edit!) \\
visdata     & mixed   & data stream of uv variables \\ \hline
flags       & integer & optional flags for narrowband data \\
wflags      & integer & optional flags for wideband data \\ \hline
gains       & mixed   & antenna gain table \\
nfeeds	    & integer & number of feeds on each antenna \\
ntau	    & integer & Number of delay/spectral index terms per antenna\\
	    &	      & in `gains' \\
nsols       & integer & number of records in `gains' \\
ngains      & integer & number of antenna gains in each record of `gains'\\
interval    & double  & gain interpolation time tolerance (days)\\
leakage	    & complex & polarization leakage parameters\\
freq0	    & double  & reference frequency for delay terms\\
freqs	    & mixed   & frequency set up description table for `bandpass'\\
bandpass    & complex & bandpass function gains \\
nspect0     & integer & number of windows in the bandpass function\\
nchan0      & integer & total number of channels in the bandpass function\\
\hline\end{tabular}
\end{center}
\end{table}

\begin{latexonly}
The {\em Programmers Guide} contains more detailed 
information on how a visibility
dataset is constructed, this Appendix only reports which variables can
\index{visdata, visibility item}
be found in the item {\tt visdata}. The text item {\tt vartable}
\index{vartable, visibility item}
contains an ordered (for quick indexing) list of all the variables which 
exist in the {\tt visdata} item.

A list of all items in a visibility dataset
is summarised in Table~\ref{t:vis-items} below.
A list of all the uv variables can be obtained with the \miriad\ program
\task{uvlist}\index{uvlist}.

The storage {\bf types} (2nd column) in the table below are:

\footnotesize\begin{verbatim}
    A -- ascii (NULL terminated)
    R -- real (32 bit IEEE)
    D -- double (64 bit IEEE)
    C -- complex (2 * 32 bit IEEE)
    I -- integer (32 bit twos complement)
    J -- short (16 bit twos complement)
\end{verbatim}
\normalsize

They are the same as the data type in the first column of the
{\tt vartable} item in a \miriad\ uv dataset.

Variables with two
dimensions have the first dimension varying fastest, the usual FORTRAN
notation.

\newpage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\small\begin{tabbing}
\label{t:uvvars}
NameXXXXXXXXX  \=Tyxxxxxxx  \=unitsxxxx   \=Comments\kill
Name         \>Ty  \>Units     \>Comments\\
\\

airtemp         \>R     \>centigr.
\> Air temperature at observatory\\

antaz(nants)    \>D     \>deg.
\> azimuth of antennas\\

antdiam		\>R	\>meters
\> Antenna diameter\\

antel(nants)    \>D    \>deg.
\> elevation of antennas\\

antpos(nants, 3) \>D    \>nanosec
\>Antenna equatorial coordinates\\


atten(nants)   \>I     \>dB
\> Attenuator setting (Hat Ck)\\

axismax(2,nants) \>R    \>arcsec
\> Maximum tracking error in a cycle. \\
\>\>\> axismax(1,?) is azimuth error, \\
\>\>\> axismax(2,?) is the elevation error.\\


axisrms(2,nants) \>R    \>arcsec
\> RMS tracking error. \\
\>\>\> axisrms(1,?) is azimuth error, \\
\>\>\> axisrms(2,?) is the elevation error.\\

baseline        \>R     \>-
\>The current antenna baseline\\
\>\>\> Baseline is stored as \MM{256*ant1 + ant2} or \\
\>\>\> \MM{2048*ant1 + ant2 + 65536}\\
\>\>\> The uv coordinates are calculated as\\
\>\>\> \MM{uvw = xyz(ant2) - xyz(ant1)}.\\
\>\>\> Note that this is different from the AIPS/FITS convention\\
\>\>\> (where \MM{uvw = xyz(ant1) - xyz(ant2)}).\\
\>\>\> When writing this variable, software must ensure that\\
\>\>\> \MM{ant1 < ant2}. \\
\>\>\> {\tt baseline} is also known as {\tt preamble(4)}\\

bin		\>I	\>-
\> Pulsar bin number.\\

cable(nants)	\>D	\>nanosec
\> measured length of IF cable (Hat Ck)\\

calcode		\>A	\>-
\> ATCA calcode flag\\


chi		\>R	\>radians
\>Position angle of the X feed relative to the sky. This is the \\
\ \ or chi(nants)
\>\>\>sum of the parallactic angle and the {\tt evector} variable. \\
\>\>\>If only one value is present, all antennas are \\
\>\>\>assumed to have identical values.\\

coord(2)        \>D     \>nanosec
\> uv baseline coordinates\\
\>\>\> {\tt coord} is also known as {\tt preamble(1:2)}\\

corbit		\>R	\>-	
\>  Number of correlator bits (Hat Ck)\\

corbw(2)	\>R	\>MHz
\>	Correlator bandwidth setting (Hat Ck)\\
\>\>\> Must take the values \\
\>\>\> 1.25, 2.5, 5.0, 10.0, 20.0, 40.0 \& 80.0 MHz.\\


corfin(4)	 \>R	\>MHz
\> Correlator LO setting before Doppler tracking (Hat Ck)\\
\>\>\> This is the LO frequency at zero telescope velocity\\
\>\>\> Must be in the range 80 to 550 MHz.\\


%\end{tabbing}

%=================================== NEW PAGE ========================

%\begin{tabbing}
%NameXXXXXXXXX  \=Ty  \=unitsxxxx   \=Comments\kill

cormode		\>I	\>-
\>  Correlator mode (Hat Ck). Values are: \\
\>\>\>    1 : 1 window /sideband by 256 channels \\
\>\>\>    2 : 2 windows/sideband by 128 channels \\
\>\>\>    3 : 4 windows/sideband by  64 channels, single sideband \\
\>\>\>    4 : 4 windows/sideband by  64 channels, double sideband \\

coropt		\>I	\>-
\>	Correlator option (Hat Ck)\\
\>\>\>   0 means cross-correlation\\
\>\>\>   1 means auto-correlation\\
\>\>\>   Same as the {\tt obstype} item?\\

corr(nchan)	\>J or  \>-
\> Correlation data\\
\>R\>\> {\tt corr} is really a complex quantity, but the \\
\>\>\>  data stream variable can be stored otherwise \\
\>\>\>  for efficiency.\\

cortaper	\>R	\>-
\> On-line correlation taper (Hat Ck)\\
\>\>\>  This is the value at the edge of the window \\
\>\>\>  The value is from 0-1.\\

ddec		\>R	\>radians
\> Offset in declination from {\tt dec} in {\tt epoch} coordinates.\\
\>\>\>  The actual observed DEC is calculated as dec + ddec.\\

dec		\>R or	\>radians
\> Declination of the phase center/tangent point. See {\tt epoch} for coordinate\\
		\>D	\>
\> definition.\\

delay(nants)	\>D	\>nanosec
\> delay setting at beginning of integration (Hat Ck)\\

delay0(nants)	\>R	\>nanosec
\> delay offset for antennas (Hat Ck) \\


deldec		\>R or	\>radians
\> Declination of the delay tracking center. See {\tt epoch} for coordinate\\
		\>D	\>
\> definition.\\

delra		\>R or	\>radians
\> Right ascension of the delay tracking center. See {\tt epoch} \\
		\>D	\>
\> for coordinate definition.\\

dewpoint	\>R	\>centigr.
\> Dew point at weather station (Hat Ck)\\

dra		\>R	\>radians
\>Offset in right ascension from {\tt ra} in {\tt epoch} coords.\\
\>\>\>	The actual observed RA is calculated as  \\
\>\>\>        ra + dra/cos(dec).\\

epoch		\>R	\>years
\>A badly named variable -- this defines the mean equinox and \\
\>\>\> equator for the equatorial coordinates {\tt ra}, {\tt dec},\\
\>\>\> {\tt dra} and {\tt ddec}. The epoch of the coordinates is \\
\>\>\> actually the observing time. Values less than 1984.0 are \\
\>\>\> Besselian with coordinates in the FK4 system. Values greater \\
\>\>\> than 1984.0 are Julian with coordinates in the FK5 system.\\

evector		\>R	\>radians
\>Position angle of the X feed, to the local vertical.\\
\ \ or evector(nants)
\>\>\>If only one value is present, all antennas are \\
\>\>\>assumed to be identical.\\

focus(nants)	\>R	\>volts
\>  Focus setting (Hat Ck)\\

freq		\>D	\>GHz
\> Rest frequency of the primary line\\

freqif		\>D	\>GHz
\> ? (Hat Ck only?)\\

inttime		\>R	\>seconds
\>Integration time \\

ischan(nspect)	\>I	\>-
\> Starting channel of spectral window\\

ivalued(nants)	\>I	\>?
\> Delay step (Hat Ck)\\
\>\>\> Used in an attempt to calibrate amp and phase vs. delay.\\

jyperk		\>R	\>Jy/K
\> The efficiency Jy/K, \\
\>\>\> calculated during calibration\\

latitud		\>D	\>radians
\> Geodetic latitude of the observatory.\\

lo1		\>D	\>GHz
\> First local oscillator (Hat Ck)\\
\>\>\> {\tt lo1} is in the range 70 GHz - 115 GHz.\\

lo2		\>D	\>GHz	
\> Second local oscillator (Hat Ck)\\

longitu		\>D	\>radians
\>Longitude of the observatory. \\

lst		\>D     \>radians
\> Local apparent sidereal time.\\

mount		\>I	\>-
\> The type of antenna mounts. \\
\ or mount(nants)\>\>\>If only one value is given, all antennas\\
\>\>\>\ are assumed to be the same. Possible values are:\\
\>\>\>\ \ 0: Alt-az mount.\\
\>\>\>\ \ 1: Equatorial mount.\\
\>\>\>\ \ 2: X-Y.\\
\>\>\>\ \ 3: orbiting.\\
\>\>\>\ \ 4: bizarre.\\

name		\>A     \>-
\> ATCA raw RPFITS file name.\\

nants		\>I	\>-
\> The number of antennas \\
\>\>\> Following variables use a dimension of {\tt nants}:\\ 
\>\>\>    antpos(nants, 3)  \\ 
\>\>\>    focus(nants)  \\ 
\>\>\>    phaselo[1-2](nants) \\ 
\>\>\>    phasem1(nants) \\ 
\>\>\>    systemp(nants, nspect) \\ 
\>\>\>    wsystemp(nants, nwide) \\
\>\>\>    temp(nants, ntemp) \\ 
\>\>\>    tpower(nants, ntpower) \\ 
\>\>\>    axisrms(2,nants) \\ 
\>\>\> The antennas are always numbered starting at 1. \\ 

nbin		\>I	\>-
\> Total number of pulsar bins.\\

nchan		\>I	\>-
\>The total number of individual frequency channels\\
\>\>\> The following variables have the dimension of nchan:\\
\>\>\>   corr(nchan)\\

npol		\>I	\>-
\> The number of simultaneous polarisations\\

nschan(nspect)	\>I	\>-
\> Number of channels in spectral window\\

nspect		\>I	\>
\>Number of spectral windows\\
\>\>\> Following variables use a dimension of {\tt nspect}:\\
\>\>\>    ischan(nspect)\\
\>\>\>    nschan(nspect)\\
\>\>\>    restfreq(nspect)\\
\>\>\>    sdf(nspect)\\
\>\>\>    sfreq(nspect)\\
\>\>\>    systemp(nants, nspect)\\

ntemp			\>I	\>-
\>Number of antenna thermisters \\
\>\>\> Following variables use a dimension of {\tt ntemp}: \\
\>\>\>   temp(nants, ntemp) \\

ntpower 		\>I	\>-
\>Number of total power measurements \\
\>\>\> The following variable depends on {\tt ntpower}: \\
\>\>\>    tpower(nants,ntpower) \\
\>\>\> {\tt ntpower} is currently 1, could be more later.\\

nwide			\>I	\>-
\>Number of wideband channels \\
\>\>\> Variables which depend on {\tt nwide} are: \\
\>\>\>     wfreq(nwide) \\
\>\>\>     wwidth(nwide) \\
\>\>\>     wcorr(nwide) \\
\>\>\>     wsystemp(nants,nwide) \\

obsdec		\>D	\>radians
\> Apparent declination of the phase centre/tangent point \\
\>\>\>at time of observation.\\

observer(*)	\>A	\>-
\>The name of the observer\\

obsra		\>D	\>radians
\> Apparent right ascension of the phase centre/tangent point\\
\>\>\> at time of observation.\\

on		\>I	\>-
\>Either 0 or 1, for on and off pointing,\\
\>\>\>for auto-correlation data.\\

operator(*)	\>A	\>-
\>The name of the current operator\\

pbfwhm		\>R     \>arcsec
\>(Deprecated) Primary Beam at Full Width Half Maximum\\
\>\>\> For Hat Ck, it is approximately 11040.0/{\tt lo1}.\\

pbtype		\>A     \>-
\>Primary beam type to be used in imaging.\\

phaselo1(nants)	\>R	\>radians
\> Antenna phase offset (Hat Ck)\\

phaselo2(nants)	\>R	\>radians
\> Second LO phase offset (Hat Ck)\\

phasem1(nants)	\>R	\>radians
\> IF cable phase (Hat Ck)\\

plangle		\>R     \>degrees
\>Planet angle\\

plmaj		\>R	\>arcsec
\>Planet major axis (note units)\\

plmin		\>R	\>arcsec
\>Planet minor axis \\

pltb		\>R	\>Kelvin
\>Planet brightness\\

pntdec		\>R or	\>radians
\> Declination of the pointing center. See {\tt epoch} for coordinate\\
		\>D	\>
\> definition.\\

pntra		\>R or	\>radians
\> Right ascension of the pointing center. See {\tt epoch} \\
		\>D	\>
\> for coordinate definition.\\


pol		\>I	\>-	
\>  Polarization type of the correlation data. Values \\
\>\>\>follow the AIPS/FITS convention, viz:\\
\>\>\>\ \ 1: Stokes I\\
\>\>\>\ \ 2: Stokes Q\\
\>\>\>\ \ 3: Stokes U\\
\>\>\>\ \ 4: Stokes V\\
\>\>\>\ -1: Circular RR\\
\>\>\>\ -2: Circular LL\\
\>\>\>\ -3: Circular RL\\
\>\>\>\ -4: Circular LR\\
\>\>\>\ -5: Linear XX\\
\>\>\>\ -6: Linear YY\\
\>\>\>\ -7: Linear XY\\
\>\>\>\ -8: Linear YX\\

precipmm	\>R     \>mm
\> Mm of precipitable water vapour in the atmosphere.\\

pressmb		\>R     \>millibar
\> atmospheric pressure.\\

project(*)	\>A	\>-
\>The name of the current project\\

ra		\>R or	\>radians
\>Right ascension of the phase center/tangent point. See {\tt epoch} for\\
		\>D	\>
\>the definition of the coordinate system.\\

rain	\>R	\>mm
\>The current amount of water in the rain guage. The rain guage is emptied
at 9:00 AEST (ATCA).\\

refpnt(2,nants) \>R    \>arcsec
\> Reference pointing offsets. \\
\>\>\> refpnt(1,?) is azimuth offset, \\
\>\>\> refpnt(2,?) is the elevation offset.\\

relhumid	\>R     \>\%
\>Relative Humidity at observatory\\

restfreq(nspect) \>D	\>GHz
\>  Rest frequency for each spectral window.\\
\>\>\>  This may be zero for continuum observations.\\

sctype	\>A	\>-
\> Scan type\\

sdf(nspect)	\>D	\>GHz	
\> Change in frequency per channel\\

sfreq(nspect)	\>D	\>GHz
\> Sky frequency of first channel in window\\

smonrms		\>R	\>$\mu$m
\> ATCA seeing monitor rms value\\

source(*)	\>A	\>-	
\>The name of the source \\

systemp		\>R	\>Kelvin
\> Antenna system temperatures \\
\ \ or systemp(nants) \\
\ \ or systemp(nants,nspect)\\

telescop(*)	\>A	\>-
\>The telescope name. Some standard values are:\\
\>\>\> \verb+'ATCA'+ \\
\>\>\> \verb+'HATCREEK'+ \\
\>\>\> \verb+'VLA'+ \\
\>\>\> \verb+'WSRT'+ \\

temp			\>R	\>centigr.
\> Antenna thermistor temperatures (Hat Ck)\\
\ \ (nants, ntemp) \\

themt(nants)	\>R	\>Kelvin
\> temperature of the hemt amplifier (Hat Ck)\\

tif2(nants)	\>R	\>Kelvin
\> temperature of IF amplifier (Hat Ck)\\

time			\>D     \>days
\> The time (nominally UT1) stored as a Julian date.\\
\>\>\> For example, noon on Jan 1, 1980 is 2,444,240.0! \\
\>\>\> {\tt time} is also known as {\tt preamble(3)}\\

tpower		\>R	\>volts
\> Total power measurements (Hat Ck)\\
\ \ (nants, ntpower) \\

tscale		\>R	\>-
\> Optional correlation scale factor \\
\>\>\> Used only when {\tt corr} is stored as J (16 bits).\\

tsis(nants)	\>R	\>Kelvin
\> temperature of the SIS mixers (Hat Ck)\\

ut			\>D	\>radians
\> The time since midnight Universal time (nominally UT1).\\

veldop		\>R	\>\kms
\> The sum of the radial velocity of the observatory\\
\>\>\> (in the direction of the source, with respect to the rest\\
\>\>\> frame) and the nominal systemic radial velocity of the source.\\

veltype(*)		\>A	\>-
\>Velocity rest frame. Possible values for {\tt veltype} are:\\
\>\>\>   VELO-LSR: rest frame is the LSR \\
\>\>\>   VELO-HEL: rest frame is the barycentre \\
\>\>\>	 VELO-OBS: rest frame is the observatory\\
\>\>\>   FELO-LSR: rest frame is the LSR (deprecated)\\
\>\>\>   FELO-HEL: rest frame is the barycentre (deprecated) \\

version(*)		\>A	\>-
\>The current hardware/software version\\
\>\>\> Current options: oldhat, newhat\\

vsource		\>R	\>\kms
\> Nominal radial systemic velocity of source.\\
\>\>\> Positive velocity is away from observer.\\

wcorr(nwide)		\>C     \>-  
\> Wideband correlations. The current ordering is:\\
\>\>\>  wcorr(1:2) are the digital LSB and USB.\\
\>\>\>  wcorr(3:4) are the analog LSB and USB.\\

wfreq(nwide)	\>R	\>GHz
\> Wideband correlation average frequencies\\


wind		\>R     \>km/h
\> Wind speed in km/h\\

winddir		\>R     \>deg
\>Wind direction\\
\>\>\> (note: originally encoded as `N', `SE', `W', etc.)\\


windmph		\>R     \>mph
\> Wind speed - in imperial units!\\

wsystemp			\>R	\>K
\> System temperature for wide channels. \\
\ \ or wsystemp(nants) \\
\ \ or wsystemp(nants,nwide) \\

wwidth(nwide)			\>R	\>GHz
\> Wideband correlation bandwidths\\

xsampler			\>R	\>percent
\> X sampler statistics (ATCA). \\
\ \ (3,nants,nspect)\\

xtsys(nants,nspect)		\>R	\>Kelvin
\> System temperature of the X feed (ATCA).\\

xyamp(nants,nspect)		\>R	\>Jy
\> On-line \MM{XY} amplitude measurements (ATCA). \\

xyphase				\>R	\>radians
\> On-line \MM{XY} phase measurements (ATCA). \\
\ \ (nants,nspect)\\

ysampler			\>R	\>percent
\> Y sampler statistics (ATCA). \\
\ \ (3,nants,nspect)\\

ytsys(nants,nspect)		\>R	\>Kelvin
\> System temperature of the Y feed (ATCA).\\

\end{tabbing}\normalsize
\end{latexonly}
