\chapter{\miriad\ Datasets}\label{c:managing}

\section{Listing Datasets -- PRTHD}
\label{s:dataset}

\miriad\ uses the term `dataset' to refer to an image, a cube or a set of
visibility data. A \miriad\ dataset is made from a host-system directory,
{\it i.e.}, 
the host operating system sees a `directory' whenever \miriad\ sees a 
`dataset'.

The task \task{prthd} provides an astronomical summary of
a dataset. For visibility datasets, it will tell
you the number of visibilities, the number of spectral-channels,
the number of spectral-windows (IFs), the polarisations
present and some information about frequencies. Note that if the
visibility dataset contains multiple source or frequencies, it will only
give you information about the source and frequency of the first
record. For images, \task{prthd}
will give dimension and axis information, minimum and maximum,
etc. The
input to \task{prthd} is the dataset name via the keyword {\tt in}.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf PRTHD}} \\
\hline
in=gauss               & Input dataset.\\
\hline 
\end{tabular}
\end{center}


\section{Inside Datasets}
Generally the user does not need to understand much about the internal
structure of a dataset. A dataset contains several kinds of data, called
`items', \index{item, definition}.
Items can be quite small (e.g. a single number), intermediate in size
(e.g. the {\tt history} item, use to store history information), or very
large (e.g. the {\tt image} item in an image dataset, which is used to
store the pixel data, or the {\tt visdata} item in a visibility dataset,
which contains the correlations).
Large items are stored as a host system file, whereas all small items are
stored in a common file which is rather inappropriately called {\tt header}.
Indeed, there are a number of instances where the word ``header'' is used
where ``item'' or ``small item'' would be a more appropriate description.

The implementation of datasets as directories does complicate
some manipulations of your datasets, since your favourite image, etc, is not
just a file anymore.
On the other hand, as the host system sees a \miriad\ dataset as a normal
directory, all the
usual host commands to manipulate directories can be used. On UNIX a \verb+-r+
switch often has to be used with the command, to indicate that the operation
is to be applied `recursively' (i.e. to all files in the directory). For
example, to delete a dataset, use
\begin{verse}
{{\tt\%\ rm -r }{\it dataset}}
\end{verse}\index{datasets, deleting}
If you have aliased \unix{rm} to prompt you before deleting a file (as is common
in a number of the standard login scripts at Epping), you will be prompted
before deleting each individual file within a dataset. This can become
somewhat tedious, so you might want to make another alias to delete without
prompting. For example, insert
\begin{verbatim}
     alias rrm 'rm'
\end{verbatim}
in your {\tt .cshrc} file.
Similarly to copy a dataset, you would use
\begin{verse}
{{\tt\%\ cp -r }{\it dataset1 dataset2}}
\end{verse}

Generally the user is insulated from this internal organisation of a dataset
and can always think of them as a whole. However there are a few
\miriad\ utilities to manipulate at the item level. These tasks
do not contain any astronomical knowledge. Consequently they may seem
somewhat crude. These tasks include:
\begin{itemize}
\item \task{itemize}: List the items in a dataset.
\item \task{delhd}: Delete an item from a dataset.
\item \task{puthd}: Add an item which consists of a  single-value to a dataset.
\item \task{copyhd}: Copy an item from one dataset to another.
\item \task{gethd}: Print the value of an item. This is most useful in scripts.
\end{itemize}
Note the rather inappropriate use of `hd' in the preceding names, where
something suggesting items (rather than header) would have been more
appropriate.

As an example, consider the \task{itemize} task, which lists the items
in a dataset. For the test image dataset, {\tt gauss}
(created with \task{imgen} \index{imgen} in Chapter~\ref{c:iface})
\task{itemize} will tell us of the following items:
\begin{verbatim}
   % itemize in=gauss
   Itemize: version 1.1 4-mar-91
   naxis    = 2
   naxis1   = 256
   naxis2   = 256
   crpix1   = 129
   crpix2   = 129
   cdelt1   = -4.848137e-06
   cdelt2   = 4.848137e-06
   crval1   = 0
   crval2   = 0
   history    (text data, 38 elements)
   image      (real data, 65536 elements)
\end{verbatim}
Here the item {\tt naxis} consists of a single integer, having the value
of 2. The item {\tt image} is a larger item (being the pixel data) consisting
of 65536 (\TT{256}{256}) real numbers.

Note for images that many items have FITS-like names (although they are lower
case, and the units can be different from the FITS standard). A list of
the items in an image and visibility dataset are given in Appendices~\ref{app:image} and \ref{app:uvvars}.

\section{Scratch Files}\label{s:scratch-files}
Some \miriad\ tasks create scratch files to hold temporary results. These
files do not show up in directory listings, but they do take up real space
(these files are guaranteed to be deleted if a task dies).
\miriad\ will place these scratch files in the directory pointed to by
the environment variable \verb+TMPDIR+. If this is not set,
the current default directory is used.

\section{\miriad\ Data Disks at Epping}\index{datasets, managing}\index{disks}
As datasets are normal directories, they can be created wherever you can create
directories. Though it is possible to create them in your home directory,
given the quotas enforced on home directories you are unlikely to be able to
store much data there.

Epping users can use the \verb+/DATA+ disks (shared with \aips\ users) to store
their data for a few days to a few weeks. To do so, you should create
a directory {\tt /DATA/{\em host\verb+_+x}/{\em username}}, where
{\em host\verb+_+x} is a machine name and a number ({\it e.g.} {\tt ATLAS\verb+_+1})
and {\em username} is your login name. For example
\begin{verbatim}
     % mkdir /DATA/ATLAS_1/rsault 
\end{verbatim} 
creates a directory for \verb+rsault+ on the first data disk of machine
atlas.

All the disks attached to all the different workstations
are accessible from any machine. That is, the disks are
cross-mounted between the computers.  However there is a right way and a
wrong way to use this cross-mounting -- there is a large network penalty
(particularly to write) to a remote disk.  For example, there is a large
overhead if you write to a disk attached to APUS when you are logged
into CARINA.  The penalty is smaller if you read from a network mounted
disk, but is still substantial.  In general you should attempt to keep
your data on the disks of one workstation, and use that workstation for
all your compute and I/O intensive work.

There are two sorts of disks -- public and bookable. Public disks can
be used by anyone at anytime. However their contents 
{\em may be automatically cleared} at 5:00 am on Mondays. The bookable disks
provide a private allocation of disk space, which is not subject
to destruction on Monday morning. Note, however, when your booking has
expired, your data can be deleted from a bookable disk without
warning. Disks can be booked by e-mailing `bookings'.


\newchapter
\chapter{Visibility Data Concepts}\label{c:visconcepts}
\section{\miriad\ Visibility File Format}\label{s:files}

A visibility dataset in \miriad\ logically consists of two parts -- a 
stream of variables and calibration tables. The stream of variables
consists of parameters that are known at the time of the observation. They
describe the changing state of the telescope. Variables include
the observing frequency, the observing centre, the source name, (\MM{u,v})
coordinates, the baseline number, the polarisation parameter being
measured, and the actual measured correlations. The calibration tables
are parameters that are derived after the observation. They are
usually deduced from observations of a calibrator. Generally, \miriad\
tasks give a higher level view of the internals of a dataset, but it can
be useful sometimes to examine these building blocks of the dataset. 
A list of all the variables and calibration tables is given in
Appendix~\ref{app:uvvars}.

Because source names, frequencies, observing centre  etc., are stored as
variables, \miriad\ does not distinguish between a single-source file
and a file with multiple sources or frequencies, or a mosaiced observation.
For example, a visibility dataset containing multiple sources does not
differ structurally from one containing a single source. However, the
variables {\tt source, ra} and {\tt dec} will change several times within
a dataset which contains multiple sources, whereas they would remain
constant in a dataset with only a single source.

\section{\miriad's Calibration Model}\index{visibility datasets, calibration}
\index{calibration, general}
\index{calibration, calibration model}

\miriad's suite of calibration tasks generally produce or use items
which contain the relevant calibration tables and parameters.
The tasks to manipulate these calibration tables generally
have names start
with \verb+gp+, though there are a few exceptions such as \task{selfcal} and
\task{mfcal}.
The calibration parameters are antenna-based ones, with
the calibration data being used to derive a model of the response of each
antenna. The response of an antenna to radiation is modelled by
four sets of parameters:

\begin{itemize}
\item Antenna gains. These are complex-valued gains which vary with time, but
not frequency. These are predominantly atmospheric in origin, though there is
a significant instrumental component (the two cannot be distinguished
readily). \miriad\ stores these in the {\tt gains} item of a visibility
dataset.

\item Delay factors.  These are antenna-based parameters
which give a time delay, which will probably be a sum of atmospheric and
instrumental components.
This term is assumed to vary with time, but not frequency. It is
stored in the {\tt gains} item.

\item Antenna bandpass functions.  These are complex-valued gains
modelling the instrumental bandpass.  These vary with frequency but not
with time. These are largely instrumental in origin.
These are stored in the {\tt bandpass} item (and some associated
items).

\item Antenna leakages. These are complex-valued terms (which do not
vary with time or frequency) which model the leakage of one polarisation into
another. These are entirely instrumental in origin.
Current experience suggests that leakages may vary moderately with
frequency. These are stored in the {\tt leakage} item.

\end{itemize}

Ignoring leakage (which is discussed in too much detail in 
Chapter~\ref{c:polarimetry}), we model the composite gain function of an
antenna (representing both atmospheric and instrumental terms) as
\[
g(t)g_{P1}(\nu)	\exp(i2\pi\tau(t)(\nu-\nu_0))
\]
Here \MM{g(t)} is the frequency-independent part of the antenna gain,
$g_P(\nu)$ is the bandpass function,
$\tau(t)$ is a delay term. The delay is calculated with respect to a
reference frequency, $\nu_0$.
For dual polarisation systems the two polarisation bands are
assumed to have independent gains and band passes, although the delay
is common.

Note that \miriad\ datasets {\em cannot} contain more than one set of calibration
tables -- you cannot have multiple versions of calibration tables. 
Running a calibration task twice will result in the first calibration
table being overwritten.  This may be inconvenient if the dataset has
two sets of data that need to be calibrated separately. 

\section{On-the-Fly Calibration Correction}\index{calibration, application}
\label{s:onthefly}

Many tasks will apply calibration corrections `on-the-fly'.  This means
that often there is no need to form a calibrated dataset.  Normally this
calibration correction step is performed by default.  However, the correction
stage
can be disabled using some fairly standard switches in the
\verb+options+ parameter.  These switches are:

\begin{description}
\item[{\tt nocal\rm:}] Do not apply antenna gain and delay
corrections.
\item[{\tt nopol\rm:}] Do not apply polarisation leakage corrections.
\item[{\tt nopass\rm:}] Do not apply bandpass function corrections.
\end{description}

For example, to disable gain and bandpass correction in \task{uvspec},
use:
\begin{verbatim}
     % uvspec options=nopass,nocal
\end{verbatim}

It is always possible to form a calibrated dataset by using either of the
visibility copying tasks (\task{uvcat} or \task{uvaver}).  Whether or not
you choose to do this may be often a matter of personal taste.  However
there are three {\em VERY} important situations where you should form a
calibrated copy of your dataset ({\it i.e.} where `on-the-fly' calibration is
inadequate). 

\begin{itemize}

\item A still significant, but decreasing, number of tasks do not
perform `on-the-fly' calibration.  Tasks
which do apply calibration corrections will invariably have {\tt nocal,
nopol} and {\tt nopass} options.  Additionally all tasks which are
performing `on-the-fly' calibration will issue messages when they are
performing these steps. 

\item Many \miriad\ tasks allow `on-the-fly' averaging of spectral
channels (using the {\tt line} parameter with a width greater than 1 --
see section~\ref{s:line}) in addition to bandpass correction.  However
the technique used when simultaneously bandpass correcting and averaging
is only approximately correct.  For arcane reasons, rather than applying
the bandpass and then averaging the channels, the tasks correct the
average of the channels with the average of the bandpass.  This can be
significantly in error if the bandpass functions vary significantly over
the range of channels being averaged.  Tasks performing this dubious
operation issue warning messages to alert you of your possible folly. 
If this is a real problem, then it is best to form a copy of the
bandpass-corrected data, and then use these corrected data for channel
averaging.

\item The self-calibration tasks produce gain tables in the same format as
the calibration tables derived from observations of calibrators. Thus if
you self-calibrate a dataset containing you initial calibration, you
will overwrite your initial calibration -- a generally undesirable step.
Additionally the self-calibration tasks do not apply some calibration
corrections (e.g. bandpass and sometimes leakage corrections).
Generally it is best to
self-calibrate a dataset which has had the initial calibration applied.
\end{itemize}


\section{Channel Selection, Averaging and Doppler Correction -- UV Linetypes}
\label{s:line}
Many \miriad\ tasks support on-the-fly selection and averaging of the
channels to be processed. However, before launching into a description
of this, we will review some \miriad\ history.
\miriad\ was originally designed for a telescope which simultaneously
measured both spectral and
continuum data using separate correlators. Spectral data are generally narrowband, and the
\index{uv, linetypes} \index{line}
frequencies \index{linetype, uv}
are defined to high precision. Doppler tracking is often
employed. Continuum, or wideband, data have a much larger bandwidth, and
frequency
tolerances are not as great.`Channel-0' data (data formed by averaging
all spectral channels together) are also treated as continuum data.

For ATCA use, there is no real distinction between wideband (continuum)
and spectral data, and the \miriad\ distinction is not really
relevant. All ATCA data (even channel-0 data) are treated as spectral
data.

All the same, a \miriad\ visibility dataset can, in principle,
contain multiple spectral and wideband correlators.
The spectral data is described by a set of `spectral windows' (`IFs' or
`IF channels' in \aips\ terminology); each window
consists of a number of channels separated by a fixed increment in
sky frequency (though this increment can vary with time). Similarly there
can be several measured wideband correlations, simply called wideband channels.

It is quite common, when analysing, plotting or mapping visibility data,
that you will want to perform some averaging and selection of the desired
channels, and you might
wish to examine either the spectral or the wideband data. For the
spectral data, if the channel number does not correspond reasonably directly
with velocity ({\it e.g.}
if Doppler tracking was not used), then it might be
desirable to resample the spectral data at equal increments in velocity.

The ability to select a range of wideband or spectral channels,
to perform averaging, and to resample in velocity is provided by the
`line' parameter -- also called the linetype. If your data contain
multiple spectral windows, you should also refer to Section~\ref{s:window}
for more information on spectral channel selection.

The linetype parameter consists of a string followed by up to four numbers.
Defaults will be used for any trailing part of the linetype specification that
is missing. The string can be one of:
\begin{description}
\item[{\tt channel}] This gives raw or averaged spectral channels. This is
generally the default if spectral data are present. As all ATCA data
are treated as spectral data, this will be the most commonly used
linetype.
\index{channel, linetype}
\item[{\tt wide}] This gives raw or averaged wideband (continuum) data.
This is the default if only wideband data are present. Probably this will be
of no interest to ATCA users.
\index{wide, linetype}
\item[{\tt velocity}] This gives spectral data, that have been resampled at equal
increments in radio velocity (or equivalently frequency). The resampling operation is a
weighted average
of spectral channels. See Section~\ref{s:veloline} for more information.
\index{velocity,linetype}
\item[{\tt felocity}] This is like \verb+velocity+, but allows the velocity
parameters to be given using the optical definition. Note, however,
that the resampling operation is still in equal increments in {\em
frequency (or, equivalently, radio velocity)}. Because of the difference between
the radio and optical velocity definitions, equal increments in radio
velocity are not quite equal in optical velocity, and visa versa.
The velocity increment that you give is used as the optical
velocity increment of the first channel.
\end{description}

The accompanying four numbers are used to specify the range of input channels
selected and averaged to produce the output channels. The four numbers are:
\begin{verse}
{\it  nchan, start, width, step}
\end{verse}
For {\tt channel} and {\tt wide} linetypes, {\it start, width} and {\it step}
are channel numbers (channels are numbered from 1 to \MM{N}), whereas for
{\tt velocity} and {\tt felocity} linetypes these values are in \kms\ (the velocity is relative
to the rest frame -- usually LSR). These values are
\begin{description}
\item[{\it nchan}] The number of output channels produced. Generally it defaults
to the maximum number of channels that can be produced from the input
data. A value of zero can also be used to give you the default.	
\item[{\it start}] For {\tt channel} and {\tt wide} linetypes, the {\it start} value
is the first input channel to be selected. For {\tt velocity} and {\tt felocity}
linetypes, {\it start} is the centre velocity of the first output channel to
be formed. The default value is 1 channel.
\item[{\it width}] This value determines the width of the selected channels.
For {\tt channel} and {\tt wide} linetypes, this gives the
number of input channels to average together to produce a single output
channel. For {\tt velocity} and {\tt felocity} linetypes,
this gives the velocity width (in \kms) of
the output channels. The default value is again 1 channel.
\item[{\it step}] This parameter gives the increment between channels. For
{\tt channel} and {\tt wide} linetypes, this gives the increment between
selected input channels. For {\tt velocity} and {\tt felocity}
linetype this gives the
velocity increment between the output channels. This defaults to the same
value as {\it width}.
\end{description}

For example
\begin{verbatim}
   line=channel,10
\end{verbatim}
selects 10 output channels, being input spectral channels 1 to 10. Similarly
\begin{verbatim}
   line=channel,10,8,1,2
\end{verbatim}
again selects 10 output channels, starting at input spectral channel 8, and
skipping every second input channel. If you wanted to average together
every pair of channels (rather than skipping it), you would
use something like
\begin{verbatim}
   line=channel,10,8,2,2
\end{verbatim}
Finally a linetype of:
\begin{verbatim}
   line=velocity,10,1.5,1.0,3.0
\end{verbatim}
would return 10 `velocity' channels with velocities {\em centred} at 1.5,
4.5, 7.5, etc. \kms. Each channel would have a width of 1 \kms.

When using {\tt velocity}, {\tt felocity} or {\tt channel} linetypes on
datasets with multiple spectral windows, {\tt window} selection, as
described in the following sections, may be useful.

Some tasks require two linetypes, the first being the linetype of the
data, and the second the linetype of a single reference channel
(see {\it e.g.} \task{invert}). \index{invert} When specifying a reference
linetype, you do not give the {\it nchan} (it is always 1) or {\it step} (it
makes no sense for a single channel).

\section{Selecting UV Data in \miriad}
\label{s:select}
It is common to wish to process only a subset of the possible 
visibility data.
In \miriad, the visibility data to be selected are usually specified by one
parameter -- the {\tt select} keyword.
This parameter is constructed from
\index{select, \MM{u-v} data selection} \index{select}
a number of subcommands, each subcommand selecting or rejecting
visibility data which satisfies some condition. The subcommands, which can
be abbreviated to uniqueness, are as follows:
\begin{description}
\item[{\tt time({\it t1,t2})}] This selects visibilities observed between times
{\it t1} and {\it t2}. Times are given in UT. 
The second time ({\it t2}) is optional.
If missing, it is assumed to be 24 hours after {\it t1}.

The time is composed of a date and `time-of-day' portions, either of which
can be omitted (but not both at the same time!). When the date is omitted,
then the selection matches data, for the given time-of-day, regardless of
the day. This is most useful for files containing only a single day's data.
When the time-of-day is omitted, then 00:00 is assumed.

The times
\index{time, uv select}
{\it t1} and {\it t2} are given in the form:
\begin{verse}
{\it yymmmdd.fff}
\end{verse}
or
\begin{verse}
{\it yymmmdd:hh:mm:ss.s}
\end{verse}
Here {\it yy} is the year, {\it mmm} are the first three letters of the
month's name, {\it dd} is the day of the month, {\it fff} is a fraction of
a day, {\it hh} is the hour (24-hour clock), {\it mm} are the minutes, and
{\it ss.s} are seconds and fractions of a second.
The `time-of-day' portion can be abbreviated. For
example, the seconds part can be omitted. Indeed (provided a date is given)
the `time-of-day' portion can be totally omitted if desired.

Note that only a two-digit year is given. These two digits give a year
in the century 1940 -- 2040.


For example:
\begin{verbatim}
   90jan12:12:30
\end{verbatim}
is 12:30 on 12 January, 1990, whereas
\begin{verbatim}
   78jun03.5
\end{verbatim}
is midday on 3 June, 1978, and finally
\begin{verbatim}
   00apr01
\end{verbatim}
is April Fools Day in the year 2000.

To give some examples:
\begin{verbatim}
   select=time(91jan05:10:50,91jan05:17:20)
\end{verbatim}
will select data observed on 5th January, 1991, between 10:50 UT to 17:20 UT.

The form where no date is given:
\begin{verbatim}
   select=time(10:50,17:20)
\end{verbatim}
will select data observed on any day between 10:50 and 17:20.

To give an example where only one time is given, the following
\begin{verbatim}
   select=time(91jan03)
\end{verbatim}
will select all data observed on 3rd January, 1991. This form is only
useful for a file containing several days of data.

\item[{\tt antennae({\it a1,a2,...})({\it b1,b2...})}] This selects according to the
antennas and baseline. Here {\it a1,a2,...} is a list of antennas to select.
The second list, {\it b1,b2,...} is optional. If present, only baselines
corresponding to the antennas pair (a1,b1), (a1,b2), ..., (a2,b1), ..., etc
are selected. For example, to select all visibilities using antennas 1 or 3,
\index{antennae, uv select}
use
\begin{verbatim}
   select=antennae(1,3)
\end{verbatim}
To select all visibilities using baselines with antennas 1 and 2, or 3 and 2,
use
\begin{verbatim}
   select=antennae(1,3)(2)
\end{verbatim}
\item[{\tt uvrange({\it uvmin,uvmax})}] This selects visibilities whose \MM{u-v}
radius
is in the range {\it uvmin} to {\it uvmax}.
If only one value is given, then {\it uvmin} is
\index{uvrange, uv select}
taken as zero. The units of {\it uvmin} and {\it uvmax} are kilowavelengths.
\item[{\tt uvnrange({\it uvmin,uvmax})}] This is the same as the {\tt uvrange} subcommand, 
except that the units are nanoseconds.
\item[{\tt ha({\it h1,h2})}] This selected data based on the hour angle of 
the data. {\it h1} and {\it h2} are the hour angle (in decimal hours).
\item[{\tt elevation({\it e1,e2})}] This selects data based on the elevation
at which the data were observed. {\it e1} and {\it e2} give the elevation
range in degrees.
\item[{\tt lst({\it l1,l2})}] This selects data based on the LST of the
observation. l1 and l2 are the start and end LST values, and can be given
in the form hh:mm:ss  or as decimal hours.
\item[{\tt visibility({\it n1,n2})}] This selects visibilities numbered {\it n1}
\index{uvnrange, uv select}
to {\it n2} inclusive.
\item[{\tt increment({\it inc})}] This selects only every {\it inc}'th visibility.
\index{increment, uv select}
\item[{\tt ra({\it hh:mm:ss1,hh:mm:ss2})}] Select visibility data within
a particular range of right ascension. Right ascension is given in {\it hh:mm:ss}
format, or as decimal hours.
\item[{\tt dec({\it dd:mm:ss1,dd:mm:ss2})}] Select visibility data within
a particular range of declination. Declination is given in {\it dd:mm:ss}
format, or as decimal degrees.
\item[{\tt dra({\it p1,p2})}] For data files containing several pointing
centres, this
selects visibilities whose pointing centre is offset, in RA, from the main
\index{dra, uv select}
pointing centre, by between {\it p1} and {\it p2} arcseconds.
\item[{\tt ddec({\it p1,p2})}] For data files containing several pointing centres, this
selects visibilities whose pointing centre is offset, in DEC, from the main
\index{ddec, uv select}
pointing centre, by between {\it p1} and {\it p2} arcseconds.
\item[{\tt pointing({\it p1,p2})}] For data files containing rms pointing error data,
this selects visibilities with the rms pointing error in the range {\it p1}
\index{pointing, uv select}
to {\it p2} arcseconds. If only one number is given, {\it p1} is taken as 0.
The rms
pointing error of a visibility is taken as the maximum of the rms azimuth and
rms elevation pointing errors of the two antennas.
\item[{\tt source({\it srcnam1,srcnam2,...})}] For data files which contain multiple
sources, this selects data according to source name. Several source names
can be given, separated by commas. The source name can include an
asterisk, which acts as a wildcard.
\index{source, uv select}
\item[{\tt frequency({\it loval,hival})}] This selects data according to the sky
frequency of the first channel. The {\it loval} and {\it hival} values
give the permissible range, in GHz, of frequencies to accept. If only a single
value is given, then this is used as a mid-frequency, and frequencies within
a tolerance of 1\%\ are accepted.
\index{frequency, uv select}
\item[{\tt amplitude({\it x,y})}] Any correlation, where the amplitude
is between {\it x} and {\it y}, is processed. 
If only one value is given, it is assumed to be {\it x}, and
{\it y} is assumed to be infinity.
\index{amplitude, uv select}
\item[{\tt window({\it w1,w2,...})}] This can be used when the dataset contains
multiple spectral windows (or IF bands in \aips\ terminology). This is
discussed in Section~\ref{s:window}.
\index{window, uv select}
\item[{\tt shadow({\it d})}] This selects data that would be shadowed by an
antenna of diameter {\it d} meters. If {\it d} is zero, then the actual
diameter of the antennas (if known) is used. If some data is shadowed, it
is advisable to use an antenna diameter value greater than the physical
antenna size ({\it e.g.} 20\%\ larger).
\index{shadow, uv select}
\item[{\tt auto}] This selects auto-correlation data only. So the
negated form ({\tt -auto}) selects cross-correlation data only.
\index{auto, uv select}
\item[{\tt on}] This is intended for use with beam switched single dish
observations, and selects based on the `on' variable in the visibility 
dataset.
The `on' variable indicates whether the telescope is pointing on or off
source.
\index{on, uv select}
\item[{\tt bin({\it n1,n2})}] This selects visibilities whose pulsar
bin number is in the range {\it n1} to {\it n2} inclusive. If {\it n2}
is omitted, just bin {\it n1} is selected.
\item[{\tt polarization({\it a,b,c,...})}] This selects based on the polarization/Stokes
type of the visibility. The possible values for {\it a,b,c,} etc. are mnemonics
for the polarization type, as discussed in Section~\ref{s:polar}.
\index{polarization, uv select}
\item[{\tt or}] This is discussed below.
\index{or, uv select}
\end{description}
As noted before, all
subcommand names can be abbreviated to the minimum number of characters
needed to keep them unambiguous (minimum match).

Each subcommand can
be prefixed with a plus or minus sign (\verb|+| or
\verb|-|). A plus sign means to select the data given by the following
subcommand, whereas a minus sign means to discard the data. If neither
a plus nor a minus sign is present, a plus sign is assumed. For example
\begin{verbatim}
   select=uvrange(0,10)
\end{verbatim}
means to select all visibilities between 0 and 10 kilowavelengths, whereas
\begin{verbatim}
   select=-uvrange(0,10)
\end{verbatim}
selects all data {\it except} visibilities between 0 and 10 kilowavelengths.

Several subcommands can be combined on the same line, separated by commas.
When combining several subcommands of {\it different} types,
the visibility must be selected by all the
subcommands to be accepted (a logical AND).
When combining several subcommands of
the same type, then the visibility is accepted if it is selected
after sequentially examining each of the subcommands (a logical OR).
For example:
\begin{verbatim}
   select=uvrange(0,10),uvrange(20,30)
\end{verbatim}
selects data with a \MM{u-v} radius in the intervals 0 to 10 kilowavelengths as well
as 20 to 30 kilowavelengths. The following uses a `select then discard'
approach to selecting the same \MM{u-v} ranges as above:
\begin{verbatim}
   select=uvrange(0,30),-uvrange(10,20)
\end{verbatim}
The following selects the same \MM{u-v} ranges, but only for the baseline between
antennas 1 and 3.
\begin{verbatim}
   select=uvrange(0,10),uvrange(20,30),antennae(1)(3)
\end{verbatim}
The following selects all baselines, with the exception of 1-2, 5-7 and 6-7:
\begin{verbatim}
   select=-antennae(1)(2),-antennae(5,6)(7)
\end{verbatim}

Another way of combining subcommands, is with the {\tt or} subcommand.
This allows you to OR together two `clauses' of selection commands.
For example, to select spectral
windows 1 and 2 for the times 0:00 to 1:00, and spectral windows 4 and 5 for
times 2:00 to 3:00, use:
\begin{verbatim}
   select=time(0:00,1:00),window(1,2),or
          time(2:00,3:00),window(4,5)
\end{verbatim}

By combining the various subcommands and the {\tt or} subcommand, quite complex
selection criteria can be developed. For complex selections, an
\verb+@+ file (as described in Section~\ref{s:atfile}) may be preferred.

For example, consider a file, {\tt select.data}, containing the text:
\begin{verbatim}
   time(1:00,2:00),window(1,2),or
   time(2:00,3:00),window(1,2),-uvrange(50,100),-antennae(1)(3),or
   time(3:00,4:00),window(1,2,3,4)
\end{verbatim}
Then
\begin{verbatim}
   select=@select.data
\end{verbatim}
will use windows 1 and 2 for time 1:00 to 2:00, and windows 1, 2, 3 and 4 for
times 3:00 to 4:00. For time 2:00 to 3:00, it uses windows 1 and 2, but omits
data for baseline 1-3 which has a \MM{u-v} range of 50 to 100 kilowavelengths.
\index{or, uv select}

There are a few limitations on the use of {\tt amplitude},
{\tt polarization} and {\tt window} selection. Some of these limitations
are mentioned in following sections, whereas others are not.
Few of these limitations affect normal practice,
and the selection software will inform you if there is an problem.


\section{Spectral Window Selection}\label{s:window}
A \miriad\ visibility dataset can contain simultaneous observations at multiple
spectral bands, or spectral
windows. These are called `IF-bands' in \aips. When there are multiple
spectral windows, channels are still numbered from 1 to \MM{N}, where channel 1 is
the first channel of the first spectral window, and channel \MM{N} is the last
channel of the last spectral window. That is, channel numbering takes no
regard of spectral window boundaries.

When there are multiple spectral windows, it is often convenient to be able
to select based on the spectral window, rather than a channel range. This is
achieved with \verb+select=window+, which selects data from a set of
spectral windows. The general form is
\begin{verse}
{{\tt select=window(}{\it w1,w2,...}{\tt)}}
\end{verse}
where {\it w1, w2,} etc, are spectral window numbers. For example,
selecting the data from spectral windows 1, 2 and 3 would be achieved with
\begin{verse}
{\tt select=window(1,2,3)}
\end{verse}

Spectral window selection and {\tt velocity}/{\tt felocity} linetypes are
quite complementary.
The different spectral windows might correspond to different spectral
lines with different rest frequencies, and so it is desirable to select only
a subset of spectral windows (those corresponding to a given spectral
line) in forming the output velocity channels.

Spectral window selection is not particularly complementary to
{\tt channel} linetype processing -- both select a range of input channels.
When {\tt line=channel} and {\tt select=window} are used together, the
total apparent number of input channels is equal to the number of channels
in the selected spectral windows, and the {\it start} channel number of the
\verb+line+ parameter is relative to the first selected spectral
window. For example, if there are multiple spectral windows, then
\begin{verse}
{\tt select=window(2)}\\
{\tt line=channel,1}
\end{verse}

would select just the first channel of the second spectral window. Similarly
\begin{verse}
{\tt select=window(2)}\\
{\tt line=channel,10,8,1,2}
\end{verse}
would select 10 channels, being every second channel starting at the 8th
channel of the second spectral window.

There are some restrictions on the use of spectral window selection.
\begin{itemize}
\item For {\tt channel} linetype, the selected
spectral windows must be contiguous. For example, you cannot select just
windows 1 and 3. This restriction is relaxed by some tasks, most notably
\task{uvcat}.
\item For {\tt channel} linetype, {\tt window} selection cannot be used
with {\tt or} selection.
\item For {\tt velocity}/{\tt felocity} linetype, if both {\tt window} and {\tt or} 
selections are used, then there should be
a window selection within each {\tt or} clause. Additionally only
one of the {\tt or} clauses should be satisfiable for a given visibility.
For example, consider the following:
\begin{verbatim}
   select=time(0:00,1:00),window(1,2),or
          time(0:00,2:00),window(3,4)
\end{verbatim}
Here data in the times 0:00 to 1:00 could have been selected by either of
the two clauses. There is no guarantee as to which of the two window
subcommands will be used during this time interval.
\end{itemize}

\section{Selection and Multi-Source/Multi-Frequency Datasets}
Having no distinction between single- and multi-source files has the
advantage that all visibility tasks can manipulate any visibility
dataset.  However manipulating a dataset with, say, several sources can
require more care on the part of the user.  For example, it rarely makes
sense to make an image using data from several sources.  Generally the
user is given reasonable flexibility to perform whatever s/he deems
appropriate -- but this has some disadvantages.  For example, consider
determining calibration for an observation where the calibrator (a point
source) and the program source are within the one dataset.  In deriving
the antenna gains, you would want to select only the data from the
calibrator, whereas when imaging you would select only the data from the
program source.  The visibility selection mechanism (see
Section~\ref{s:select}) handles this situation.  But to forget to select
the appropriate data (calibrating with data including the program
source, or imaging including the calibrator) would result in a mess. 

Thus, for example, if
the calibrator was \MM{0823-500}, and the program source was {\tt vela},
one would use
\begin{verbatim}
       select=source(0823-500)
\end{verbatim}
and
\begin{verbatim}
       select=source(vela)
\end{verbatim}
to select the appropriate source.

The selection criteria most appropriate for datasets containing multiple
sources, frequencies and mosaiced pointing centres are {\tt source, frequency,
window, ra, dec, dra} and {\tt ddec}.  

Other examples of using the \verb+select+ keyword are given below:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf SELECT}} \\
\hline
select=window(2)     & Select only data from the 2nd IF		\\
select=freq(4.74)    & Select data where the frequency of the first	\\
		       & channel is 4.74 GHz ($\pm 1$\%)         	\\
select=ra(05:30,06:00) & Select visibilities with RA between 5:50 and 6:00 \\
		       & hours.\\
select=dra(0),ddec(0)  & Select data from only the central pointing (delta \\
		       & RA and DEC of 0) of a mosaiced observation.	\\
select=source(vela) &Take data for source vela only \\
select=window(2),freq(4.74)& Select data from the 2nd IF where its frequency\\
			     & is 4.74 GHz.				\\
\hline 
\end{tabular}
\end{center}

\section{Polarization/Stokes Handling (stokes and select)}\label{s:polar}
The visibility datasets possibly contain correlations for different polarizations
and Stokes parameters. There are two basic ways that polarization
\index{polarization} 
characteristics can be measured: firstly, two orthogonal feeds can be 
present on the \index{stokes}
antennas, and the four different polarization correlations from a baseline can
be taken and recorded. Alternatively only one feed may be present, which can
be rotated, or there are not enough correlators to calculate all four
polarization correlations simultaneously. The first approach, used by the ATCA,
allows all
polarization parameters to be measured simultaneously. The second forces
a `time-sharing' approach, where the feed or correlator must be switched
between measuring one polarization to another. With the four simultaneous
measurements, it is possible to convert a visibility from raw polarization
parameter to a Stokes parameter. With the time-shared mode, this is not
directly possible. These two different scenarios mean that two very different
suites of software are needed to obtain Stokes parameters.

In \miriad, each different polarization/Stokes measurement is treated as a
separate visibility (this differs from the \aips\ approach -- which does
not really support the time-shared approach to polarization measurement).
So if a baseline measures four simultaneous polarization correlations, then
four `visibilities' will be produced for this baseline, per integration
interval. In this case, each visibility will be tagged (with a \MM{u-v} variable
{\tt pol}) to indicate the polarization type.

There are two ways the user can determine which polarizations he or she
wants to process, either with the {\tt select} parameter, or with the
{\tt stokes} parameter.

The {\tt select} approach selects visibilities purely on the basis of their
polarization or Stokes parameter.
It is a normal part of the \MM{u-v} selection process, as described in the
previous section. The {\tt select} mechanism cannot
convert from raw polarization parameters to Stokes parameters. It just
selects visibilities, in the file, based on their polarization.
The general form is:
\begin{verse}
{{\tt select=polarization(}{\it a,b,c,...}{\tt)}}
\end{verse}
where {\it a,b,c} etc can be one of the mnemonics
\begin{itemize}
\item For Stokes parameters: {\tt i, q, u, v}
\item For circular polarization parameters: {\tt rr, ll, rl, lr }
\item For linear polarization parameters: {\tt xx, yy, xy and yx}
\end{itemize}

Using the {\tt stokes} approach is superficially similar, in that it causes
only certain polarization/Stokes parameters to be processed. However,
with the {\tt stokes} approach, the software can perform
conversion between raw polarizations and Stokes parameters if required.
It also insists that all the requested polarization/Stokes parameters are
calculable, at a given time, before it will allow any of them to be
processed.
For example, if the user requests Stokes  I and V, and the
dataset contains linear polarization data, then all four of 
{\tt xx, yy, xy} and {\tt yx}
polarizations must be present for I and V to be returned.

The general form of the {\tt stokes} approach is:
\begin{verse}
{{\tt stokes=}{\it a,b,c,...}}
\end{verse}
where {\it a,b,c} etc, can be one of the mnemonics given above
({\tt i, q, u, v, rr, ll} etc).

The {\tt stokes} can also take the values {\tt ii, qq} and
{\tt uu}:
\begin{itemize}
\item Mnemonic {\tt ii} returns Stokes-I {\em given the assumption that the
source is unpolarised}. For example \miriad\ would not return any data
if you requested \verb+stokes=i+ and the dataset contains only, say, {\tt xx} ({\tt xx}
is not Stokes-I for a polarised source). How if you requested \verb+stokes=ii+
in this situation, the {\tt xx} data would be passed through, because
you have told \miriad\ that the source is unpolarised. Many tasks use
\verb+stokes=ii+ as their default.
\item Mnemonic {\tt qq} and {\tt uu} returns Stokes-Q and Stokes-U {\em
given the assumption that the parallactic angle is 0}. These quantities
are rarely of astronomical interest, but they are useful to investigate some
instrumental effects.
\end{itemize}

The {\tt select} mechanism is usually used for time-shared polarization
measurements, whereas the {\tt stokes} mechanism is usually used where there are
simultaneous measurements. However this is not a hard and fast rule.
The two approaches generally cannot be used at the
same time. Some tasks will prohibit the use of the {\tt select}
approach altogether (they will give error messages if you attempt to),
whereas others are more lenient. The rules which determine whether you
can or cannot use the {\tt select} approach are quite arcane and can be
file dependent. Check the documentation for each task,
especially if both {\tt select} and {\tt stokes} parameters are present.
If {\tt polarization} selection is not allowed, then {\tt increment}
selection will also be prohibited (due to some arcane quirk).



\newchapter
\chapter{Image Data Concepts}\label{c:imconcepts}
\section{Image Datasets}
Most of the items which are used to build up an image follow a FITS-like
convention. For example the number of axes in an image is given by the
{\tt naxis} item, whereas the number of pixels along each axis is
given by item {\tt naxis}{\em i}. Three exceptions are the {\tt image},
{\tt mask} and {\tt history} item. The {\tt image} item contains the pixel
data, stored as floating point numbers. The {\tt mask} item, if present,
contains a bit-mask used for pixel blanking -- \miriad\ does not use
magic value blanking like \aips. Finally the {\tt history} item
contains a text file describing the history of the dataset.

A complete list of the items that are potentially contained within
an image dataset is given in Appendix~\ref{app:image}.

\section{Image Coordinate Systems}
\miriad\ correctly handles a variety of image
coordinate systems (the mapping between pixel and physical coordinates)
in a correct fashion.
The coordinates (and their
description within the dataset) are treated in a fashion similar
to \aips\ (see Eric Greisen's \aips\ memo No. 27,
\htmladdnormallink{``{\it Non-linear coordinate systems in \aips}''}
{ftp://ftp.aoc.nrao.edu/pub/software/aips/TEXT/PUBL/AIPSMEMO27.PS}).

\section{Image Region of Interest (region)}\label{s:region}
Most image-related tasks can process a subset of the pixels in an input
image. Depending on the task, the selected pixels may either be a fairly
arbitrary region, or only a regular subimage of the input image.
\index{region, of interest}

The task parameter, \verb+region+, which gives the region-of-interest
consists of a combination of subcommands. Each subcommand 
specifies either a subregion or the units of the coordinates used in subsequent
sub-commands.

The subregions selected by multiple subcommands are effectively `OR-ed'
together to form the overall region. That is, the overall region selected
is the `union' (not intersection) of the subregions.

For comparatively simple regions, combining subcommands is quite
adequate. However for complex regions, a cursor-based program, \task{cgcurs},
may be the most convenient for generating the subcommands.

Region specification is composed of one or more of the following subcommands.
Each subcommand can be abbreviated to uniqueness, and subcommands
are separated by a comma.
\begin{description}
\item[{\tt images({\it z1,z2})}] This selects the image planes {\it z1} to {\it z2} 
inclusive.
{\it z2} is optional, defaulting to the same value as {\it z1}.
\index{images, region of interest}
\item[{\tt quarter({\it z1,z2})}] This is somewhat like the {\tt images}
command, except that
it selects only the central quarter of each plane. Both {\it z1} and {\it z2}
are optional.
\item[{\tt box({\it xmin,ymin,xmax,ymax})({\it z1,z2})}]
This subcommand selects the pixels within a box whose corners are
{\it xmin, ymin,xmax} and {\it ymax}.
\index{boxes, region of interest}
{\it z1} and {\it z2} are optional, and
are the same as in  the {\tt image}
subcommand. If the {\tt({\it z1,z2})} part is missing, a default is used (generally
all planes are selected).
\item[{\tt polygon({\it x0,y0,x1,y1,x2,y2,...})({\it z1,z2})}]
This gives the vertices of a
polygon.
\begin{verse}
{{\tt box(}{\it xmin,ymin,xmax,ymax}{\tt)}}
\end{verse}
is equivalent to
\index{polygon, region of interest}
\begin{verse}
{{\tt poly(}{\it xmin,ymin,xmax,ymin,xmax,ymax,xmin,ymax}{\tt).}}
\end{verse}
{\it z1} and {\it z2} are the same as with the {\tt images} and {\tt boxes}
subcommands.
\item[{\tt mask({\it name})}]
This selects pixels according to the mask given by the mask item
\index{mask, region of interest} in the
dataset {\it name}.
\end{description}

The units used for the coordinates are controlled by the following
subcommands:
\begin{description}
\item[{\tt abspixel}] Subsequent coordinates are given as absolute pixel values
({\tt i.e.} values ranging from 1 to \MM{NAXISi} - see \task{prthd} or
\task{itemize}).
This affects image coordinates and
the coordinates along the third dimension. This is also the default.
\index{abspixel, image coordinate units}
\item[{\tt relpixel}] Subsequent image coordinates are relative to the reference
pixel, as defined by the header of the map of interest. Use
\index{prthd}
\task{prthd} to see the reference pixel is
({\it CRPIXi}).
\index{relpixel, image coordinate units}
\item[{\tt relcenter}] Subsequent image coordinates are relative to the central
pixel of the image. This is somewhat like the {\tt relpixel} command,
but used the image centre, rather than the reference pixel.
\index{relcenter, image coordinate units}
\item[{\tt arcsec}] Subsequent image coordinates are given in arcseconds, and
are relative to the reference pixel of the map of interest.
\index{arcsec, image coordinate units}
\item[{\tt kms}] Subsequent coordinates in the third dimension are given in
\kms.
\index{kms, image coordinate units}
\end{description}

For example, to specify a \TT{21}{21} region, centred on the reference
pixel, use:
\begin{verbatim}
   region=relpix,box(-10,-10,10,10)
\end{verbatim}
or to give a \TT{10}{10} region in the lower left corner of the image,
use
\begin{verbatim}
   region=box(1,1,10,10)
\end{verbatim}
If there are multiple maps in the file, use 
\begin{verbatim}
   region=box(1,1,10,10)(1,2)
\end{verbatim}
to select the first 2 maps.

The region-of-interest specifications can become
rather involved, when complex regions are used. As with visibility data selection,
\verb+@+ files (see Section~\ref{s:atfile}) 
are a convenient way to store these.


There are some warnings for those accustomed to the {\tt select}
visibility data selection method:
\begin{itemize}
\item The subcommands of a region of interest are logically
OR'ed together, whereas in the visibility data selection it is a logically
AND. For \MM{u-v} {\tt select}  the
logical OR must be explicitly given by use of the
{\tt or} subcommand.
\item The subcommands do not allow a plus or minus prefix, like
the visibility data selection does.
\end{itemize}


\newchapter
\chapter{Reduction Strategies}
Here we give an overview of the reduction process, and review some of the
basic decisions that you will have to make during the reduction of the data.
We now consider some questions that you should ask yourself before the
reduction process.

\begin{itemize}

\item {\em Is this observation more than a single-pointing,
continuum experiment?} Hopefully the answer to this is obvious!
This manual contains special chapters addressing the reduction
of spectral line (Chapter~\ref{c:lineobs}), mosaic (Chapter~\ref{c:mosaicing})
and pulsar-bin mode (Chapter~\ref{c:pulsar}) observations. You are
encouraged to review appropriate chapters {\em before} you start
reducing your data.

\item {\em Will I want to deconvolve my images?} Deconvolution is the process
of removing artifacts due to the incomplete sampling in the \MM{u-v} plane.
You will want to deconvolve for observations where the source is stronger
than a few times the noise limit. That is, unless you are doing a detection
experiment, you are likely to want to deconvolve. Deconvolution is addressed
in Chapter~\ref{c:decon}.

\item {\em Will I want to self-calibrate the data?} Self-calibration is
the process of determining the antenna-based gain function from the
source itself.  For this to be possible, your signal needs to be about 5
to 10 times stronger than the thermal noise when integrated over the
self-calibration solution interval (typically 15 seconds to 5 minutes). 
For the ATCA in continuum mode, this means a source which contains at
least 100 to 200 mJy in most baselines.  Self-calibration is discussed
in Chapter~\ref{c:selfcal}. 

\item {\em As I have a continuum experiment, do I want to use multi-frequency
techniques?} Even in continuum mode, the ATCA produces multiple channels
of data. As the fractional bandwidth can be quite significant,
you may be making a significant approximation if
you average all these channels into a single channel (the so-called
`channel-0' dataset). The result will be
poorer \MM{u-v} coverage and bandwidth smearing. If you are interested in
high dynamic range imaging, or if a good beam is important, and you are
observing at 21, 13 and possibly 6 cm,
then it is best {\em not} to average your data into a single
channel. Rather you can calibrate and form a single image directly using the
multichannel data. This practise is known as multi-frequency synthesis.

Multi-frequency synthesis can be taken a few steps further with the ATCA.
As two IFs can be measured simultaneously, these two IFs can be
combined in the imaging stage to further enhance both sensitivity and
\MM{u-v} coverage. As the ATCA can frequency switch rapidly, it is
possible to time share between different frequency settings. This involves a
trade-off between tangential and radial \MM{u-v} gaps.
%Various aspects of multi-frequency synthesis are covering in Chapter~\ref{c:mfs}.

\item {\em Should I average my data in time or frequency?} The reason to
average your data is to reduce you disk consumption and to increase the
speed of the various processing programs. If these practical considerations
are important, you may consider averaging your data in time and frequency.
In this case, you probably want to do this as early as possible in the
reduction process -- after the initial flagging.

Frequency averaging is generally only applicable for continuum experiments
and only if you are {\em not} going to be doing multi-frequency synthesis.
You might also think twice about averaging if interference was a problem
during the observation. You may have to go back and do some more flagging
later.

Averaging in time can be performed when observing with short arrays (i.e.
when the 6~km antenna is not used). A conservative rule is that you should
not average longer than \MM{90/d} seconds, where \MM{d} is the array length in km.
If you observe with a short
array and are not interested in long baselines for the program source, you
probably still want to use antenna 6 when determining the initial calibration.
Despite this, \MM{d} is the array length of interest for the program source,
not the calibrators.
Nor should you average for longer than the antenna-gain solution interval
(unless you have finished calibrating, and
are not going to self-calibrate). If you have a strong source, and the phase
stability is poor, you should not average in time.

Both averaging in frequency and time are performed by \task{uvaver}
(see Section~\ref{s:uvaver}).
\end{itemize}

\begin{figure}
\begin{center}
\epsfxsize=12cm\leavevmode\epsffile{strategy.ps}
\end{center}
\caption{ATCA Data Reduction Strategy}
\label{fig:strategy}
\end{figure}

Figure~\ref{fig:strategy} gives a flow chart of the normal steps involved
in reduction of ATCA data. Note that this is somewhat abbreviated by
necessity -- additional flow-charts in subsequent chapters give more
detail and describe some of the variations.

We will now consider each step in the flow-chart in turn.

\begin{enumerate}

\item Data from the ATCA is written in ``RPFITS'' format -- an ATNF-specific
format -- and a special task (\task{atlod}) is needed to read it.
Loading your data into \miriad\ is described in Chapter~\ref{c:inout}.

\item Data editing (flagging) can be very time consuming, especially if you do are
affected by interference. The \miriad\ flagging tasks are
described in Chapter~\ref{c:edit}.

\item The actual calibration steps can be the most confusing steps, as
there are a large variety of paths that can be followed. Only
the most frequently trodden paths are described in Chapter~\ref{c:polarimetry}.

\item The task  \task{invert} takes a visibility dataset and forms either
a single image or an image cube (for spectral-line observations). It also
produces a point-spread function (dirty beam), ready for deconvolution.
See Chapter~\ref{c:imaging}

\item The main deconvolution tasks are \task{clean}, \task{maxen} and
\task{mfclean}. Task \task{clean}, which is the most commonly used, attempts
to decompose your image into a number of delta functions. It can be
slow for images with a large amount of extended emission. An
alternative to \task{clean} is
\task{maxen}, a maximum entropy-based deconvolution task. It tends to
be less robust and more difficult to run correctly. A third alternative
is \task{mfclean}. This is a derivative of the CLEAN algorithm, and
simultaneously determines a flux density and spectral index image. It is
only appropriate for multi-frequency synthesis experiments when more than
one observing band has been used.

The deconvolution tasks are described in Chapter~\ref{c:decon}.

\item As noted above, self-calibration is useful for determining
antenna gains directly from a strong program source. It is described in
Chapter~\ref{c:selfcal}.
\item The deconvolution tasks produce an output image that is in units
of flux density per pixel. That is, the outputs are CLEAN component images.
Task \task{restor} converts these to flux density per CLEAN beam, and adds
back the residuals. This is covered in Chapter~\ref{c:decon}.
\item At last, you are ready to display and think about your images!
\end{enumerate}


\newchapter
\chapter{Getting Data In and Out of \miriad}\label{c:inout}

\section{{\tt ATLOD:} Reading RPFITS Files}\index{atlod}\index{RPFITS files}
\label{s:atlod}

ATCA data will be initially in RPFITS format.  This needs to be converted to
\miriad's format, by task \task{atlod}, before any further processing
can be done.
We will discuss the various input parameters to \miriad\ \task{atlod}. 

\begin{itemize}

\item The {\tt in} parameter gives the name of the input UNIX file, or files,
to load. Many files can be given, and wildcards are supported.

RPFITS files are commonly archived on CDROMs: a CDROM appears much like
a disk to the operating system, and \task{atlod} can load RPFITS files
directly off these. To use a CDROM drive,
first check that the drive is not in use (an orange light flashing). On Solaris
to load a CDROM, press the Open/Close on the front of the drive, place 
the CDROM in
and again press the Open/Close button. After about 10 seconds, the operating
system will mount the CDROM, and it can be accessed.
CDROMs written at the ATCA will have the RPFITS data in the
directory
\begin{verse}
{\tt /cdrom/cdrom{\it x}/DATA}
\end{verse}
where {\it x} is 0 for the first drive of a machine, 1 for the second, etc
(you can do a directory listing to see what you have -- its like a normal
file system!).
You can set the {\tt in} parameter to the RPFITS files in this directory.
For example, to select all files belonging to project C561 (assuming you are
using the first CDROM drive), use
\begin{verbatim}
  in=/cdrom/cdrom0/DATA/*.C561
\end{verbatim}
When you are done with a CDROM drive you eject it with the Solaris UNIX command
\begin{verbatim}
   eject cdrom
\end{verbatim}
(the Open/Close button will not work).

RPFITS files are (or at least were once) commonly written to exabyte
tapes. Unlike other \miriad\ tasks, \task{atlod} can read exabytes
directly. In this case you should use the physical device name as the
input name. This is usually
displayed on the exabyte drive. On the Epping Solaris systems, the device
name is usually \verb+/dev/rmt/0lbn+ or similar
(this is the so-called non-rewinding, raw, BSD compatibility interface).
On older Solaris systems, it may be \verb+/dev/nrst4+ or something similar
(non-rewinding, raw interface).


An alternative to directly reading from exabyte is to load the data to
disk with the Unix commands \unix{ansiread} or \unix{ansitape}, which are
available on a number of the ATNF systems.  See the
UNIX \unix{man} pages for more information. 


\item The {\tt out} parameter gives the name of the output \miriad\ visibility
dataset. There is no default.

\item The {\tt options} parameter gives miscellaneous processing options.
thatfairly It is important to 
Several values can be given, separated by commas.
As loading RPFITS data can be time-consuming (particularly when loading
from exabyte), and as the right options for \task{atlod} can save
considerable disk space, it is sensible to spend a few moments thinking
about the correct options to use.
A few sets of permutations of the options will be appropriate with most
observations. These include:
\begin{itemize}
\item For continuum: \verb+options=birdie,xycorr+ or 
\verb+options=birdie,reweight,xycorr+. See below for a discussion on
whether to use \verb+reweight+ or not.
\item For non-polarimetric line: \verb+options=birdie+ or
\verb+options=birdie,compress+ or \verb+options=birdie,hanning+
or \verb+options=birdie,compress,hanning+. See Section~\ref{s:specrpfits}
for more on using \task{atlod} with spectral line data and
Section~\ref{s:hires} for high time resolution bin-mode data.
\end{itemize}

Possibilities options are:

\begin{description}
\item[{\tt birdie}:] The ATCA suffers from self-interference at frequencies
which are a multiple of 128 MHz. The birdie option flags out the channels
affected by this self-interference. This option is strongly recommended.

Additionally, in the 33 channel/128 MHz mode, the birdie option
also discards some edge channels and every other channel. This operation
does not incur a sensitivity penalty, as correlator channels are not
independent in this mode. The net result is that
the output consists of either 13 or 14 good channels.
\item[{\tt xycorr}:] In polarimetric correlator configurations, the
ATCA makes an on-line measurement of the phase difference between the
\MM{X} and \MM{Y} channels. Although it is generally only a few
degrees (assuming the observation was correctly set-up), this
phase difference should be corrected when doing polarimetric work.
Since a hardware upgrade in November 1992, the recommendation has 
been that the on-line measurements should
be applied, without averaging, using the \verb+xycorr+ option in \task{atlod}.
For data prior to November 1992 -- seek expert advise.

\item[{\tt reweight}:] In the 33 channel/128 MHz mode, a Gibbs
phenomena (see Albert Bos in the {\it Indirect Imaging} proceedings)
affects the
ATCA. This introduces a non-closing error into the data.
The effect is moderately subtle, and is not significant for
dynamic ranges of less than about 500. The `reweight' option reweights
the visibility spectrum in the lag domain to eliminate this problem.

This option is recommended for high dynamic range work (more than
about 500). However, the option also reduces the effectiveness of the birdie
option to reject self-interference. This can be a significant effect for
20cm continuum observations, where the usual observing band straddles
the 1408 MHz birdie. Particularly at 20cm, the reweight option is {\em not
recommended} where dynamic ranges of less than 500 are expected.

\item[{\tt compress}:] Normally the correlation data are stored as
32-bit floating point numbers.
Alternatively, the {\tt compress} switch can be used to instruct
\task{atlod} to store the correlations as 16-bit integers,
with a scale factor associated with each spectrum. This approximately
halves the disk space occupied by a dataset, and so may be very
advantageous for large spectral-line observations.
\item[{\tt hanning}:] Hanning\index{hanning} smooth the data, and
discard every second
channel. This can be useful for spectral line experiments. Task \task{atlod}
will refuse to Hanning smooth data with 33 channels or less.
\item[{\tt bary}:] Use the barycentre (often called heliocentre
and usually used for extra-galactic work) as the rest frame when calculating
velocity information. The default is to compute velocity information
relative to the LSR frame (which is usually used for Galactic work).

\item[{\tt hires}] This option may be appropriate for observations
using the ATCA's high time resolution bin mode. See Chapter~\ref{c:pulsar}
for more information.

\item[{\tt opcorr}:] This option corrects the visibility data for
atmospheric opacity. It is since October 2003 that there is sufficient
information in the visibility dataset to allow this.
This options {\em should not} be used for 3mm data. Generally it is relevant
for 12mm observations only. See Sections~\ref{s:atlod12mm} and \ref{s:atfix}
for more information.
\item[{\tt samcorr}:] Correct the measured visibilities for bad
sampler statistics. Normally the samplers adjust their various threshold
levels to ensure optimal operation. However various transients can cause
the sampler levels to be wrong. All is not lost -- you can correct the
data for the imperfect sampler levels after the fact. Use the {\tt samcorr}
(sampler-correct) option for this, and is the recommended
approach for data observed before December 1993. Since December 1993,
sampler correction
has been done automatically on-line. For this data, the \verb+samcorr+ option
is (quietly) ignored.
\item[{\tt relax}:] This option is generally not recommended.
Normally \task{atlod} flags data if it has not
previously read the appropriate calibration record or if the information
in the calibration record suggest the data are bad.  Although calibration
records generally precede the data, there are some instances when this
is not the case, and the data are still good.  In this case, the
{\tt relax} option causes \task{atlod} to be more lenient, and not flag
the data.
\item[{\tt unflag}:] Normally \task{atlod} will discard a visibility record
(i.e. not write it to disk) if all the data in it are flagged bad.
Flagging usually indicates that there was 
significant reason to suspect the data --  for example the telescope was not on
source or was off-line, etc. The {\tt unflag} option causes all data to 
be saved to disk, although flagged
data is still marked as such. This option is generally not recommended,
although the disk-space penalty for using it is often modest.
\item[{\tt noauto}] and {\tt nocross} cause \task{atlod} to discard
any autocorrelation or cross-correlation data respectively. Its
fairly unlikely that you will want to discard the cross-correlation data!
\item[{\tt noif}:] When there are multiple frequency bands being
observed simultaneously, \miriad\ normally maps the frequency bands
to its spectral windows (IF axis in \aips\ terminology), and writes
them out in a single record. Alternatively,
using the {\tt noif} switch causes \task{atlod} to write the
simultaneous frequencies as sequential records, making it appear
somewhat like a frequency-switch. You will need to use the \verb+noif+
option if you have two spectral windows which sample different
polarization parameters.
\item[{\tt nopflag}:]If at least one polarization of a set of 2 or 4 polarimetric
spectra are bad, \task{atlod} normally flags all of the
polarizations. With this option only
the nominally bad spectrum is flagged.
\end{description}

\item {\tt nscans} gives two numbers which are the number of scans to skip
over (before processing), and the number of scans to save. The default is
to save all scans.

\item {\tt nfiles}, like {\tt nscans}, gives two numbers, which are the
number of files to skip and process. This parameter is only really
useful when reading from exabyte. The default is to process the first file
only. 
Note, however, that the mechanism used to skip files is rather
inefficient.  If you wish to skip a large number of files, you should
do this with the Unix command \unix{mt}.  A
complication is that every RPFITS file appears as three files to \unix{mt}\ 
-- so you will want to skip three times as many tape files as RPFITS
files.  For example, to skip 10 RPFITS files, you would use the Unix
command

\begin{verbatim}
   mt -f /dev/nrst4 fsf 30
\end{verbatim}

See the Unix \unix{man} page on \unix{mt} for more information.

\end{itemize}

Typical inputs for \task{atlod} are given below.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf ATLOD}} \\
\hline
in=/dev/nrst0               & Input is either exabyte name\\
in=92-06-10\_2110.C159	    & or RPFIT file.\\
out=0823.uv                 & Output visibility data set.\\
options=birdie,reweight,xycorr	    & Normal continuum mode options, or\\
options=birdie,hanning,compress	    & possible spectral line options.\\
nfiles=0,3		    & Skip 0, then read 3 files.\\
nscans			    & Unset to save all scans\\
\hline 
\end{tabular}
\end{center}


\miriad's \task{atlod} saves a number of
on-line measurements as visibility variables.  These measurements may by
helpful in analysing and flagging the data.  They can be plotted and
listed with task \task{varplt} (see Section~\ref{s:exavis}).  These
on-line parameter, and its \miriad\ variable name, are described below. 

\begin{description}
\item[{\tt xyphase}:] This is the on-line measurement of the \MM{XY} phase.
It is important to examine this measurement if you are doing polarimetry. 

\item[{\tt xyamp}:] This is the amplitude of the correlation between the
\MM{X} and \MM{Y} polarization channel of a given antenna.

\item[{\tt xsampler, ysampler}:] These variables give the sampler statistics for
the \\MM{X} and \MM{Y} polarization channels.  There are three numbers per
antenna per IF, which reflect the sampler levels.  They should have values of
\MM{17.3}, \MM{50} and \MM{17.3}.  Thus for six antennas, the {\tt xsampler} variable
will consist of 18 numbers per IF.

\item[{\tt xtsys, ytsys}:] These give the system temperature, in Kelvin, for the
\MM{X} and \MM{Y} polarization channels. Unfortunately \miriad\ tasks were
originally developed around a single polarization model of a telescope, and so
most tasks that concern themselves with system temperature do not handle
dual polarization systems. The partial workaround used in \miriad\ has been
to store the geometric mean of the \MM{X} and \MM{Y} system temperatures in the
variable {\tt systemp}.
It is this variable that \miriad\ tasks use when they need to use the
system temperature value.

\item[{\tt axisrms, axismax}:] These give the antenna rms and maximum tracking
errors in a particular cycle. The units are arcseconds.

\item[{\tt airtemp,pressmb,relhumid,wind,winddir}:] These give various
meterological measurements at the observatory as a function of time.

\end{description}

\section{FITS Tapes and \miriad}\index{FITS files}\label{s:fits}
\miriad\ has a single task, \task{fits}, to read and write FITS files --
both visibility and image data. One significant shortcoming of \task{fits}
is that it cannot work directly from a tape device -- the FITS file {\em must}
be on disk. The \miriad\ utility, \task{tpcp}, is needed to copy FITS files
between
disk and tape, whereas the UNIX command \unix{mt} might been needed to position
the tape. See the appropriate \miriad\ help or UNIX \unix{man} pages on these
commands. However,
to copy from tape to disk, one would use
\begin{verse}
{{\tt\%\ tpcp }{\it device fits-file}}
\end{verse}
Here {\it device} is the tape device. Typically it is something like
\verb+/dev/nrst0+. Note that you should always use the ``raw, non-rewinding
devices'' -- that is devices whose names start with {\tt nr}. The output is
{\it fits-file}.

To write a FITS file to tape, use
\begin{verse}
{{\tt\%\ tpcp -b 2880 }{\it fits-file device}}
\end{verse}
This causes an unblocked FITS file to be written (record length of 2880 bytes).

To move the tape to the end-of-tape, use
\begin{verse}
{{\tt\%\ mt -f }{\it device }{\tt eom}}
\end{verse}
To skip forward {\it n} files, use
\begin{verse}
{{\tt\%\ mt -f }{\it device }{\tt fsf }{\it n}}
\end{verse}
and to rewind, use
\begin{verse}
{{\tt\%\ mt -f }{\it device }{\tt rewind}}
\end{verse}

\section{Reading Visibility FITS Files}\index{FITS files, visibility data}

For visibility data, the current version of \task{fits} can cope with both
single- and multi-source files, with single or multiple antenna
configurations (earlier versions of \task{fits} were more limited). Flagging
(FG) tables are copied (if present), but the flagging specified by these
tables {\em is not performed}. An additional \miriad\ task, \task{fgflag}
(see Section~\ref{s:fgflag}), must be invoked to apply the flagging tables.
Task \task{fits} does not read or use \aips\ calibration tables
(SN, CL, BP, and BL).  Consequently if you want to
preserve your \aips\ calibration, you will need to apply the
relevant tables with \aips\ \task{SPLIT} before writing the FITS file.

\task{fits} can handle linearly and circularly polarized correlations,
as well as correlations that have been converted to Stokes parameters. 
In addition, it knows that the ATCA produces linearly polarized data. 
Because \aips\ does not handle linear polarizations well, it is common
practice to label the linear polarizations of ATCA data as if they were
circular.  If \task{fits} finds data from the ATCA which is
nominally circularly polarized, it issues a warning, and relabels the
correlations as linearly polarized. 

Alternately, you could correct the polarization labelling explicitly
within \aips\ by changing the reference value of the Stokes axis to
\MM{-5}.  Do this with \task{PUTHEAD}. It is necessary to do this after
{\tt SPLIT} because it puts them back to circulars !

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf AIPS/PUTHEAD}} \\
\hline
keyword = 'crval2'   &Select the Stokes axis \\
keyvalue=-5       &Change to -5 (\MM{XX}) \\
\hline 
\end{tabular}
\end{center}


By default the \aips\ task to write FITS files, {\tt FITTP}, writes to the
directory \verb+/DATA/FITS+.
This directory has a time-destroy limit of 1 day. Alternately you
can set up an environment variable to direct {\tt FITTP} to write the
FITS file to any directory. For example, after setting the environment
variable {\tt MYAREA} with the UNIX command
\begin{verbatim}
       % setenv MYAREA /DATA/ATLAS_1/miriad/rsault
\end{verbatim}
you can direct {\tt FITTP} to write into this directory, by
setting the {\tt OUTFILE} adverb appropriately. For example:
\begin{verbatim}
       > OUTFILE = 'MYAREA:MYUV.FITS'
\end{verbatim}
will direct {\tt FITTP} to write a FITS file \verb+MYUV.FITS+ in
\verb+/DATA/ATLAS_1/miriad/rsault+.

A quick way to set an environment variable to the current working directory
is with the {\tt pwd} command:
\begin{verbatim}
       % setenv MYAREA `pwd`
\end{verbatim}

Note that the environment variable name {\em must} be in upper case, though
the directory name itself need not be.

\miriad\ handles only the new binary table format ({\tt donewtab=1} in
{\tt FITTP}). Typical inputs to \aips\ {\tt FITTP} are:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf AIPS/FITTP}} \\
\hline
doall=-1                   &One file at a time \\
outfile='myarea:0823.fits'   &Specify area and file name or \\
outfile='0823.fits'          &default to \verb+/DATA/FITS+ directory \\
dostokes=-1                &No conversions \\
dotable=1                  &\miriad\ understands FITS tables \\
dotwo=-1                   &Single precision adequate \\
donewtab=1                 &Only the new tables can be handled \\
format=3                   &Floating point FITS. Values of 1 or 2 also OK\\
\hline 
\end{tabular}
\end{center}

Using {\tt format=1} (16-bit integers) for visibility datasets with
many channels will result in an output \miriad\ dataset in compressed
format.  It also produces a much smaller FITS file -- which may be
important if disk space is low. Note that FITS data compression is different
from \miriad \ and \aips\ data compression. FITS compression uses one scale factor for
the {\em entire} dataset, whereas \miriad\ and \aips\ compression uses one
scale factor per visibility record. 

In most cases converting the FITS file to \miriad\ format is straightforward.
Apart from the \verb+op+ parameter, which indicates the operation to
be performed by \task{fits} (\verb+op=uvin+ is used to read in a visibility FITS
file), you need only supply the names of an input FITS and output
\miriad\ dataset.
Note that \miriad\ follows the case conventions of the host
operating system.  Thus, under Unix, \miriad\ is case sensitive, and so
you must give the FITS file name in the correct case.  As it has most
likely come from \aips, the file name will usually be in upper case. 

For spectral line observations, the velocity reference frame used in
\miriad\ (and some other velocity information) must be set when the visibility
dataset is created. The default is to extract the appropriate information
from the FITS file. Alternatively, the \verb+velocity+ parameter can be
used to override or alter the velocity information given in the FITS file.
See Chapter~\ref{c:lineobs} for more information.


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf FITS}} \\
\hline
in=\$MYAREA/0823.FITS       &Re-use your environment variable\\
in=/DATA/FITS/0823.FITS    &or give \aips\ default directory; get case right \\
op=uvin                    &Read in visibility data \\
out=0823.uv                &\miriad\ dataset name \\
velocity=lsr		     & Compute info for LSR velocities, for\\
			     & spectral line observations.\\
\hline
\end{tabular}
\end{center}

Leave everything else {\tt unset}.  Once \task{fits} has
run to completion, you should see the new directory containing
the `items' describing your visibility data.

\subsection*{Information Missing or Incorrect in FITS Files}
\task{fits} knows enough about the ATCA and a select group of other
telescopes to provide some nominal values
for parameters that FITS does not store -- for example a system
temperature of 50 K and a system gain of 13 Jy/K are stored with ATCA
data.
\task{fits} also attempts to estimate the integration time. 
Unfortunately given the information present in a FITS file, this
integration time estimate is little better than an educated guess.  Task
\task{fits} gives a message informing you of the values it is using.  If
you know better values for system temperature, integration time, etc, than
those guessed by \task{fits}, you may wish to correct the stored values.
This is largely so that any noise estimates produced by later tasks are in more
meaningful units.  You can correct these with the task \task{puthd}\index{puthd}. 
Task \task{puthd} writes an item into the visibility dataset, which
overrides the value of a visibility variable with the same name.  The
following three parameter boxes show the inputs to \task{puthd} to
correct the system temperature to 80~K, the system gain to 11~Jy/K and
the integration time to 15~seconds.

\begin{center}
\begin{tabular}{|l|l|} 
\hline
\multicolumn{2}{|c|}{{\bf PUTHD}} \\
\hline
in=0823.uv/systemp      &Parameter is dataset name followed by `systemp'\\
value=80.0               &Set to 80 K. Express it as a real number. \\
\hline
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf PUTHD}} \\
\hline
in=0823.uv/jyperk   &Parameter is dataset name followed by `jyperk'\\
value=11.0            &Set to 11 Jy/K. Express it as a real number. \\
\hline
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf PUTHD}} \\
\hline
in=0823.uv/inttime   &Parameter is dataset name followed by `inttime'\\
value=15.0            &Set to 15 sec. Express it as a real number. \\
\hline
\end{tabular}
\end{center}

\aips\ FITS files from the ATCA sometimes contain other flaws which \miriad\
issues warnings about -- these can usually be safely ignored. These flaws
include visibilities with zero weight, incorrect apparent RA and DEC and
circular polarizations from the ATCA.

\subsection*{\aips\ Frequency Bugs}
There are a number of bugs in \aips\ which result in inconsistent or
incorrect frequency information in
the FITS file. These frequency bugs tend to become
apparent when using two simultaneous IFs, particularly when the IFs are in
different sidebands, or if one of the IFs is discarded at some stage.
After reading with \task{fits}, it is {\em strongly encouraged} that you
examine the frequencies that
\miriad\ determines from the FITS file. Indeed it would be best if you
checked these before writing a FITS file in \aips, though this is more
difficult because the frequency
information is spread rather widely in \aips.
In \miriad, the easiest way to examine the frequency information is
with task \task{uvindex} (see Section~\ref{s:vislist}).

Errors in the FITS file frequency definition may affect the \MM{u-v} coordinates,
and so may affect the RA/DEC scales on any images that might be produced from
faulty visibility datasets.

One fairly innocuous \aips\ frequency bug is that the bandwidth of `channel 0'
datasets can be wrong. Leaving this uncorrected will invalidate error
estimates generated by \miriad\ tasks -- which is not a major problem if
you ignore these anyway!

Another, more serious, problem is that two locations in the FITS file give the
``reference frequency'' -- the
header and the antenna table. In principle these should agree. However
there are instances when they will differ.
Task \task{fits} will issue a warning when this happens, and will use the
antenna table reference
frequency as the true reference frequency. In some cases this will be
the correct frequency. In others it will not.

There are two approaches to fixing the errors in the frequency definitions --
either correct them in \miriad\ (using \task{puthd} and \task{uvputhd})
after reading the FITS file,
or in \aips\ (using \task{PUTHEAD} and \task{TABED}) before writing the FITS
file. Which you will use will depend on your familiarity with the
\aips\ and \miriad\ visibility dataset structures and the complexity of your
dataset (how many frequencies, etc), and where the bug occurred (are the
\MM{u-v} coordinates correct?). It is best to seek help, both to fix your
data, and to aid tracking down the \aips\ bugs.

\subsection*{Time and FITS Files}
The time stored in a \miriad\ visibility dataset is nominally UT1 (although
the difference between UT1 and UTC is not significant for the purposes of
\miriad). As \miriad\ relies on correct time in a number of calculations
(e.g. computing the parallactic angle needed in polarization calibration and
conversion, computing velocity information in \task{fits} and \task{uvredo}
and recomputing \MM{u-v} coordinates done by \task{uvedit}),
task \task{fits} does its best to convert the time into UT1. \task{fits}
will give you messages about any time adjustments it makes.

When a FITS file contains multiple antenna tables,
the times stored in the FITS file have usually  been offset from the true
time by the \aips\ task {\tt DBCON}. Task
\task{fits} assumes that such an offset has been added, and corrects for it.

\section{Writing Visibility FITS Files}

Task \task{fits} can also be used to write a visibility FITS file. In addition
to the keywords used when reading visibility datasets, \task{fits} uses
keywords \verb+select+, \verb+stokes+ and \verb+options+ to control
extra processing performed on the visibilities.

Unfortunately \task{fits} is more sophisticated at reading FITS files
than writing -- note the following caveats carefully.

\begin{itemize}

\item \task{fits} can cope with only a single pointing of a single
source, with a single frequency setup and single antenna array
configuration (note that, unlike earlier versions, \task{fits} writes an
antenna table).
\task{fits} does quite limited checks to see
if these conditions are met, and quite happily will
use the relevant parameters of the first record.

\item
\task{fits} assumes that the data are a single IF
band, with a constant frequency increment between channels. If presented
with multiple spectral windows (multiple IFs), an average frequency and
increment will be used. This is {\em very} undesirable. If you do
this, \task{fits} will issue warnings about frequencies deviating from
linearity. These warnings should be taken seriously. To avoid this, use
the {\tt select} or {\tt line} parameters to force \task{fits} to write
out a single spectral window. 

\item \task{fits} does not save \miriad\ calibration tables. However
\task{fits} does give you the option of applying the calibration, as
it writes out the data (see the \verb+options+ keyword).

\item \miriad\ allows you flexibility in the choice of polarization
correlations that you can write out. On-the-fly polarization
conversion can be performed. However \aips\ will refuse to read in a
number of the possibilities that \miriad\ allows. In particular, \aips\
{\tt UVLOD} refuses to load visibility data where there are
three different sorts of polarization correlations (rather than the
more usual 1, 2 or 4 sorts).

\end{itemize}

Typical inputs follow:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf FITS}} \\
\hline
in=0823.uv		     &Name of input \miriad\ dataset.\\
op=uvout                   &Write out a visibility FITS file.\\
out=0823.FITS              &Output FITS file -- probably upper case for AIPS.\\
select			     &Select appropriate data.\\
stokes=i,q,u,v		     &Select appropriate polarizations.\\
option			     &Leave unset to apply calibration, \\
options=nopol,nocal,nopass   &or turn off all calibration.\\
\hline
\end{tabular}
\end{center}


\section{Reading and Writing FITS Images}\index{FITS files, images}

Task \task{fits} can be used to read and write FITS images. The
operation for \task{fits} (keyword {\tt op}) is now {\tt xyin} (read in
a FITS image) or {\tt xyout} (write out a FITS image). For images that are
partially blanked, \task{fits} correctly
converts between the \miriad-style blanking mask, and FITS-style magic
value blanking.
When reading a FITS image, CLEAN component (CC) tables are ignored.


\section{Archiving and Transporting \miriad\ Datasets}\label{s:tar}
\index{datasets, transporting}\index{datasets, archiving}
As \miriad\ datasets are
written in a machine-independent format (the bits stored on disk to represent a
particular value are independent of the host system, whether it be a VAX,
Sun or Cray, for example), no translation is needed when transporting
datasets between unlike architectures.

Instead of relying on any special tasks to write tapes for
archiving or transport, \miriad\ relies on the
facilities of the host system. Probably the most convenient archiving
format is {\tt tar}. This is universally supported on UNIX machines, and is
quite common on VAX/VMS machines.

The two most common commands used with {\tt tar} are to copy to and
from tape. To copy to tape, use
\begin{verse}
{{\tt\%\ tar -cvf }{\it device datasets}}
\end{verse}
where {\tt device} is a tape device name such as \verb+/dev/nrst0+
(the non-rewinding, raw interface, i.e. device names starting with \verb+nr+),
and {\it datasets} is a list of datasets and files to copy to tape
(If a dataset, or directory, name is given, {\tt tar} works recursively, copying
all the contents of this directory).
These can include wildcards. For example, to copy all datasets and
files from the current directory to \verb+/dev/nrst0+, use
\begin{verbatim}
      tar -cvf /dev/nrst0 *
\end{verbatim}

To retrieve data from tape, use the \unix{tar} command
\begin{verse}
{{\tt\%\ tar -xvf }{\it device}}
\end{verse}

Multiple {\tt tar} files can be stored on the one tape so that you do
not overwrite old data.
Again the UNIX \unix{mt} command can be used to
manipulate the tape position.

See the \unix{man} pages on the \unix{tar} and \unix{mt} commands
for more information.


\newchapter
\chapter{Generating Dummy Visibility Data}\label{c:dumvis}\index{visibility datasets, generating}
\index{uvgen}

\section{Making a Fake Observation}
The \miriad\ task \task{uvgen} can be used to simulate a variety of observing
modes. Additionally it can simulate a number of systematic and random
errors and non-ideal effects that a synthesis instrument will typically
suffer.

Simulating a telescope approximately, let alone in some detail, requires
a good many parameters. \task{uvgen} takes an appreciable number of
parameters and input files to describe the telescope, correlator and
source. Fortunately keyword and set-up files for some common situations
exist ({\it e.g.} simulating the ATCA and VLA). Also the defaults are usually
sensible.

Some inputs to \task{uvgen}, and typical values for the ATCA, are given
below. {\em Note that these are not the default values.}

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVGEN}} \\
\hline
source=			&Source description file -- see help file.\\
ant=			&Antenna location file -- see help file.\\
baseunit=-51.02		&Basic antenna increment (15m) in nanosec,\\
			&for coorinates in a topocentric system.\\
corr=0,1,0,104		&Correlator model -- see help file.\\
spectra=		&Spectral line model -- see help file.\\
ellim=12		&Telescope elevation limit. \\
telescop=atca		&Telescope name -- see help file.\\
stokes=xx,yy,xy,yx	&ATCA measures linear polarisations.\\
lat=-30			&Latitude of Narrabri.\\
radec=			& Source RA and declination, in hours and degrees.\\
systemp=50		&Typical receivers have a $T_{sys}$ of 50 K\\
pbfwhm=			&Primary beam size? Varies with frequency\\
leakage=2		&Typical polarisation leakage\\
jyperk=12.7		&System gain is about 12.7 Jy/K\\
pnoise=			&Rms antenna phase variation\\
gnoise=			&Rms antenna amplitude gain variation\\
\hline 
\end{tabular}
\end{center}

These typical values for the ATCA, and also the VLA, can be found in
in `keyword' files, found in the \verb+$MIRCAT+ directory.
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf Telescope Keyword Files}} \\
\hline
\verb+$MIRCAT/uvgen.def.atca+	& Typical \task{uvgen} parameters for the ATCA\\
\verb+$MIRCAT/uvgen.def.vla+	& Typical \task{uvgen} parameters for the VLA\\
\hline 
\end{tabular}
\end{center}
The parameters in these files can be loaded from within the \verb+miriad+
shell using the \verb+source+ command.
For example, to load typical parameters for the ATCA, use
\begin{verbatim}
      miriad% source $MIRCAT/uvgen.def.atca
\end{verbatim}

By leaving some of \task{uvgen}'s parameters unset ({\it e.g.} leakage, systemp,
pbfwhm) you get an idealised telescope (no polarisation leakage, no receiver
noise, no primary beam effects). Additionally there is no reason why you
cannot request an ATCA-like telescope to produce Stokes I or circular
polarisations!

\index{antenna configuration files, ATCA}
\index{antenna configuration files, VLA}
The source and antenna description files are described in the
help for \task{uvgen}, as are a number of other parameters needed in the
simulation process. There are, however, a collection of description
files for common situations. These are all kept in the \verb+$MIRCAT+
directory.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf Source Description File}} \\		\hline
File			&Description\\				\hline
\verb+no.source+	&Nothing! No source at all			  \\
\verb+point.source+	&A 1 Jy unpolarised point source, at field centre\\
\verb+poff.source+	&A point source offset from the  field centre	  \\
\verb+qpoint.source+	&A 1\%\ linearly polarised point source		  \\
\verb+spiral.source+	&A spiral\\				\hline 
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf Antenna Description File}} \\		\hline
File			&Description\\				\hline
\verb+h75.ant+		& ATCA configuration H75\\
\verb+h168.ant+		& ATCA configuration H168\\
\verb+h214.ant+		& ATCA configuration H214\\
\verb+0.122.ant+	& ATCA configuration 0.122A\\
\verb+ew214.ant+	& ATCA configuration EW214\\
\verb+ew352.ant+	& ATCA configuration EW352\\
\verb+ew367.ant+	& ATCA configuration EW367\\
\verb+0.375.ant+	& ATCA configuration 0.375\\
\verb+0.75a.ant+	& ATCA configuration 0.75A\\
\verb+0.75b.ant+	& ATCA configuration 0.75B\\
\verb+0.75c.ant+	& ATCA configuration 0.75C\\
\verb+0.75d.ant+	& ATCA configuration 0.75D\\
\verb+1.5a.ant+		& ATCA configuration 1.5A \\
\verb+1.5b.ant+		& ATCA configuration 1.5B \\
\verb+1.5c.ant+		& ATCA configuration 1.5C \\
\verb+1.5d.ant+		& ATCA configuration 1.5D \\
\verb+3.0a.ant+		& ATCA configuration 3.0A or 6.0A \\
\verb+3.0b.ant+		& ATCA configuration 3.0B or 6.0B \\
\verb+3.0c.ant+		& ATCA configuration 3.0C or 6.0C \\
\verb+3.0d.ant+		& ATCA configuration 3.0D or 6.0D \\
\verb+vla_a.ant+	& VLA configuration A\\
\verb+vla_b.ant+	& VLA configuration B\\
\verb+vla_c.ant+	& VLA configuration C\\
\verb+vla_d.ant+	& VLA configuration D\\			\hline 
\end{tabular}
\end{center}

Given the various standard keyword and description files kept in the
\verb+$MIRCAT+ directory, it is not particularly difficult to set up the
inputs to \task{uvgen}.
For example, to simulate the `spiral' source using configuration
\verb+3.0A+, in a continuum mode, you would give the following commands
to the \verb+miriad+ front-end:
\begin{verbatim}
  miriad% source $MIRCAT/uvgen.def.atca
  miriad% source = $MIRCAT/spiral.source
  miriad% ant = $MIRCAT/3.0a.ant
  miriad% inp uvgen
\end{verbatim}
and then set or unset other parameters as desired.

If you wish to model a particular telescope, the task \task{telepar}
contains a database of telescope characteristics.

For models which are more complex than those that can be readily
simulated with \task{uvgen}, one approach is to generate the visibilities
are the appropriate \MM{u-v} tracks, and then to use the task
\task{uvmodel} to compute the visibilities, on those tracks, of a 
particular image model.

\section{Generating Visibilities from a Model Image}

If you just want to generate visibilities corresponding to a particular
model image, and do not care whether they follow realistic tracks in the
\MM{uv} plane, then \task{im2uv} may suit you.  Task \task{im2uv} simply
Fourier transforms an image (whose units should be Jy/pixel) and writes
this transform into a visibility dataset.  So you end up with
visibilities sampled on a rectangular grid.  The task attempts to
produce a visibility dataset which looks reasonably valid, but the
output does have some odd characteristics (e.g.  all output visibilities
consists of a single baseline, measured at the same time!!). 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IM2UV}} \\
\hline
model=			&Name of the input model image.\\
out=			&Name of the output visibility dataset.\\
region=			&Only pixels within the region are used\\
			&to generate the model visibilities.\\
select=			&Normal visibility selection. Only visibilities\\
			&obeying the selection criteria are stored. Only\\
			&{\tt uvrange} selection is currently honoured.\\
\hline 
\end{tabular}
\end{center}


\newchapter
\chapter{Flagging, Manipulating and Examining Visibility Data}
\label{c:edit}


\section{Flagging Visibilities}\index{visibility datasets, flagging}

In \miriad, flagging a correlation means you set a bit in a mask. There
are no explicit weights like in \aips\ or ascii flagging tables. 
When you list visibilities with \task{uvlist} (see below), flagged 
correlations
are indicated by an asterisk to the right of the phase.

There are three main flagging tasks in \miriad:
\begin{itemize}
\item \task{blflag} -- a plot-based interactive flagger. This normally plots
one baseline at a time, with a variety of possible axes (e.g. time against
amplitude), and you click on bad data to flag it out. This is similar to 
tha \aips\ {\tt IBLED} task.
\item \task{tvflag} -- a ``TV''-based, interactive flagger. This displays
one baseline at a time on a ``TV'' display, with the axes of the display
being channel number and time. You select regions of bad data in the display.
This is similar to \aips\ task {\tt SPFLG}.
\item \task{uvflag} -- a non-interactive general flagger, where you
specify the data to be flagged by the {\tt select} keyword. This is like
the \aips\ task {\tt UVFLG}.
\end{itemize}
There are a number of other flagging tasks which occupy some niche
which are described in the next section.

Although there is some personal taste involved, a recommended scheme for 
flagging is as follows:
\begin{itemize}
\item For 20 and 13~cm observations {\em it is strongly recommended}
that you start the flagging process with \task{tvflag}. Narrowband 
interference is common at the wavelengths, and \task{tvflag} is a good
tool at finding and flagging this interference. \task{tvflag} is conveniently
performed on a multi-source file, straight after the data are loaded into
\miriad. NOTE: one shortcoming of \task{tvflag} is that it ignores any
calibration tables that happen to be present in a dataset.
\item Using \task{blflag} is generally quick compared to \task{tvflag},
and some forms of bad data are more apparent with \task{blflag}. 
However, bad data is not that apparent when attempting to use it
with datasets containing multiple sources. So we recommend that you 
split the data into single source datasets (using \task{uvsplit})
before using \task{blflag}. Using \task{blflag} is recommended
even if you use have already use \task{tvflag} on you data, because
\task{blflag} is relatively quick and painless.
\item Normally \task{blflag} will apply any calibration tables 
to the data before generating its plot. Thus it is just as useful after
calibration as before. It is useful to look at calibrators using \task{blflag}
both before and after solving for calibration parameters. With the program
source, you will probably only use \task{blflag} after you are happy
with the calibration.
\end{itemize}

\subsection*{Interactive Flagging -- BLFLAG}\index{blflag}
The task \task{blflag} plots visibility data (usually baseline at a time,
with axes such as amplitude, phase, real part, imaginary part, time, etc),
and allows the user to interactively flag that data.

The user positions
the plot cursor, and uses the mouse buttons or single keyboard
characters to perform an operation.
The interactive commands are as follows:
\begin{description}
\item[{\em Left-Button:}] Pressing the left mouse button flags the nearest visibility.
\item[{\em Right-Button:}] Pressing the right mouse button causes \task{blflag} 
to precede to the next baseline (or to finish up, if all baselines have
been cycled through).
\item[{\em Carriage-return:}] This gives a brief help message.
\item[{\tt a:}] Flag nearest visibility (same as left mouse button).
\item[{\tt c:}] Clear the flagging for this baseline, and redraw plot.
\item[{\tt h:}] Give help (same as carriage return).
\item[{\tt p:}]  Define a polygonal region, and flag visibilities
within this region. You define the vertices of the
polygon by moving the cursor and then hitting the
left mouse button (or \verb+a+). You finish defining the
polygon by hitting the right mouse button (or \verb+x+).
You can delete vertices with the middle mouse button (or \verb+d+).
\item[{\tt q:}] Abort completely. This exist without apply the flagging.
\item[{\tt r:}] Redraw plot.
\item[{\tt x:}] Move to next baseline (same as right mouse button).
\end{description}

The inputs to \task{blflag} are straightforward.
\begin{itemize}
\item {\tt vis:} The name of the visibility data-set to be flagged.
\item {\tt device:} The PGPLOT device to use for plotting. This needs to
be an interactive device.
\item {\tt line:} This gives the normal linetype parameter (see
Section~\ref{s:line}). \task{blflag} averages together all the selected
channels before plotting it. The default is to use all channels.
If you flag a data point on a plot, you flag all selected channels.
\item {\tt stokes:} Polarization/Stokes selection -- see Section~\ref{s:polar}.
If several are given, {\em these are averaged together} before plotting. So
normally you only select one. The default is `\verb+ii+'. If you flag
a point, you flag {\em all} polarizations (even if you did not select that
polarization).
\item {\tt axis:} This gives two values, being the X and Y axes of the plot.
Possible values are time, lst, uvdistance ($\sqrt{u^2+v^2}$), hangle
(hour angle), amplitude,
phase, real and imaginary. The default is time vs amplitude.
\item {\tt options:}
The \verb+options+ keyword give extra processing options. These include:
\begin{description}
\item[{\tt nobase}] Normally \task{blflag} plots each baseline separately,
and cycles over all baselines. The \verb+nobase+ option allows all baselines
to be displayed simultaneously. This is particularly useful for point
sources or when the visibilities are purely noise-like.
\item[{\tt noapply}] This causes the flagging operations not to be
actually applied to the dataset.
\item[{\tt selgen}] This option generates a text file, \verb+blflag.select+. This
file contains visibility data selection commands to select the flagged data.
\end{description}
\end{itemize}

The inputs are normally very simple:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf BLFLAG}} \\
\hline
vis=vela.line		& Specify visibility dataset. \\
device=/xs		& Specify an interactive PGPLOT device.\\
stokes			& Defaults to `{\tt ii}'.\\
line			& Defaults to all channels.\\
axis			& Defaults to time vs amplitude. \\
select			& Defaults to selecting all data.\\
\hline
\end{tabular}
\end{center}

For a point source (e.g. calibrator), a particularly useful way of 
flagging, at least after initial calibration, is to plot the
``scatter diagram'' -- real vs imaginary parts of the visibility,
with all baselines simultaneously. This should show points scattered around
the true flux of the source. An arc in such a plot indicates poor phase
stability. Typical inputs in this mode are

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf BLFLAG}} \\
\hline
vis=cal.uv		& Specify visibility dataset. \\
device=/xs		& Specify an interactive PGPLOT device.\\
stokes			& Defaults to `{\tt ii}'.\\
line			& Defaults to all channels.\\
axis=real,imag		& Plot real vs imaginary.\\
opptions=nobase		& Plot all baselines simultaneously.\\
\hline
\end{tabular}
\end{center}


\subsection*{Interactive Channel/Baseline Flagging -- TVFLAG}\index{tvflag}
The task \task{tvflag} is akin to the \aips\ {\tt SPFLG}: it
displays, on a TV device, one baseline at a time, the amplitude or phase
(or amplitude or phase difference from a running average computed over
some time). Task \task{tvflag} will cycle over the set of baselines
present, displaying each one, and giving you the opportunity to flag each
baseline.

Task \task{tvflag} is the last useful remnant of the ``TV'' suite of software
(an old package of display software within \miriad), which uses two X-windows based tools,
\verb+xmtv+ and \verb+xpanel+, for displaying. You will need to start up 
these tools before using \task{tvflag}. To start these, use commands
\begin{verbatim}
     % xmtv &
     % xpanel &
\end{verbatim}
in a terminal window of your local workstation (which need not be the machine
that you are running \task{tvflag} on).
To use these, you first have to ensure that you are correctly
setup to use X-windows programs -- see Chapter~\ref{c:pltvcon} for more
information on using X-windows. Using {\tt xmtv} and {\tt xpanel} is simple
enough when you can run them on your local workstation.
If you have to run
{\tt xmtv} and {\tt xpanel} from a compute server (e.g. the situation in
Narrabri, when using kaputar via an NT workstation), then there can be problems
arising from multiple users attempting to allocate a given TCP/IP port. See
Appendix~\ref{app:xmtv} for more information.

The input parameters to \task{tvflag} are pretty straightforward. We
mention only the more useful ones:
\begin{itemize}
\item {\tt vis:} The name of the visibility data-set to be flagged.
\item {\tt server:} The TV server parameter. This may seem a rather redundant
parameter. It is a parameter of the form
\begin{verse}
{\verb+xmtv@+{\it host}}
\end{verse}
Here {\it host} is
the host name of the workstation running {\tt xmtv} and {\tt xpanel}
For example
\begin{verbatim}
   server=xmtv@tucana   
\end{verbatim}
indicates that {\tt xmtv} and {\tt xpanel} are running on the machine \verb+tucana+.
There is no default to this parameter, however a value of \verb+xmtv@localhost+
will be adequate when you are running \task{tvflag}, {\tt xmtv} and {\tt xpanel}
all on the one machine.
\item {\tt select:} The usual visibility data selection parameter. One of
\task{tvflag}'s odd characteristics is in its handling of different
polarizations. Task \task{tvflag} can deal with only a single polarization
per run. If it finds that the input data-set contains several polarizations,
it will average these together when forming its display. While unusual, this
is moderately useful. Assuming the data-set contains all four linear
polarizations,
\begin{verbatim}
       select=pol(xx,yy)
\end{verbatim}
will produce a Stokes-I display. On the other hand, no selection (i.e.
allowing \task{tvflag} to average \MM{XX}, \MM{YY}, \MM{XY} and \MM{YX})
will produce a quantity which is not physically meaningful, but will still
allow you to make a quick check for interference and outliers.
\item {\tt line:} The normal visibility linetype parameter. Task \task{tvflag}
does not support spectral averaging.
\item {\tt mode:} This determines the quantity to be displayed. Possibilities
are {\tt amplitude}, {\tt phase}, {\tt real} and {\tt imaginary}.
\item {\tt taver:} This gives a time interval used for the running mean
calculation when displaying in ``DIFF'' mode (see below). This parameter
takes two values, both in minutes. The first gives the maximum amount of time
in an averaging interval, whereas the second gives the time gap between records
which causes an averaging interval to be ended. The default is 5 minutes for
both of these.
\end{itemize}

Typical inputs are:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf TVFLAG}} \\
\hline
vis=vela.line          & Specify visibility dataset \\
server=xmtv@lupus         & Specify TV server \\
mode=amp                  & Display amplitude \\
tvchan                    & Unset is channel 1 else specify \\
range                     & Unset for intensity auto-scale \\
tvcorn                    & Unset to centre the image \\
line                      & Unset for all channels \\
taver			  & Controls the computation of the \\
			  & running mean for the DIFF command.\\
\hline
\end{tabular}
\end{center}

After invoking \task{tvflag}, a control panel will pop up
and a baseline of data will be
displayed on the TV. The display shows channels along the x-direction and time
along the y-direction. A gap in time in the data is shown by a dark gap.
A wedge is displayed both above and to the right of the data. The top
wedge is the data averaged over time, whereas the wedge to the right is
the data averaged over the channels.

You now perform flagging operations by pressing ``buttons'' on the 
control panel. We review the meaning of some of the buttons:
\begin{itemize}
\item {\tt SELECT, SLCT CHAN} and {\tt SLCT TIME}: The first thing that you
will normally do is to select some set of data, with one of the three ``select''
buttons. {\tt SELECT} selects a rectangular region on the display (i.e. some
range of times and channels), whereas {\tt SLCT CHAN} selects all times
for a particular range of channels, and {\tt SLCT TIME} selects all channels
for a particular range of times. After pressing the appropriate ``select''
button,
\begin{itemize} 
\item move the mouse so that the cursor is at one corner of the region to
be selected,
\item depress the left mouse button,
\item drag the cursor to the other corner of the region to be selected,
\item release the mouse button.
\end{itemize}
\item {\tt FLAGOOD} and {\tt FLAGBAD}: Having selected some set of data with
the previous commands, this data can be either flagged as good or bad with
the {\tt FLAGOOD} and {\tt FLAGBAD} buttons respectively.
\item {\tt UNDO:} The undo button causes the last flagging command to be undone
and discarded.
\item {\tt RESCALE:} The rescale button changes the display scaling, so that
the data occupies the full greyscale or colour range. If you have some bad data
which has an extremely high amplitude, for example, you will want to flag
this and then rescale the display range so that you can see the detail in the
remaining  data.
\item {\tt DIFF:} The ``diff'' button causes a running mean to be subtracted
off the data. 
\item {\tt EXIT, QUIT} and {\tt ABORT:} The ``exit'' button causes the next
baseline to be loaded. The ``quit'' button also causes the next baseline
to be loaded, but discards any flagging commands performed on the current
baseline. Normally \task{tvflag} will terminate when it has gone
through all the baselines. It is only at this stage that it applies
the flagging operations to the actual data-set. Alternately the
``abort'' button causes \task{tvflag} to terminate at any time, and not
to apply any of the flagging operations to the data-set.
\item {\tt LIST:} This button lists the flagging operations that have been
performed to the current baseline.
\item {\tt VALUE:} This button allows you to find the value of pixels
on the display. It is somewhat cumbersome -- after pressing the ``value''
button, you move the mouse cursor to the pixel of interest, and press the
\verb+[F3]+ function key. You continue to move the cursor and
press \verb+[F3]+ until you have exhausted the pixels of interest. Then
press \verb+[F6]+ to return back to \task{tvflag}'s normal mode.
\end{itemize}

There are other buttons on the control panel -- but we do not suggest that you
use these. They reproduce (rather crudely) buttons on the TV display window.
The useful buttons on the TV display are:
\begin{itemize}
\item {\tt Panner}: Pressing this pops up a window that allows you to pan around the
displayed image, and to fiddle the colour lookup table. The panner window that it
pops up has one of two states -- {\tt pan} and {\tt fiddle}.
\item {\tt Luts}: This allows you to select a colour lookup table from a small
number of palettes.
\item {\tt +} and {\tt -}: These allow you to zoom in and out. For mysterious reasons,
these buttons are disabled when the {\tt Panner} window is in the {\tt fiddle} state.
\item {\tt Reset}: Reset the zooming, the lookup tables, etc.
\item {\tt Resize}: This allows the size of the display window to be toggled between
full-screen and a more modest size.
\item {\tt Quit}: Hopefully its use is obvious enough!
\end{itemize}

One shortcoming of \task{tvflag} is that it averages the data in time so that
it will fit on the
display. Unfortunately full time resolution display cannot be recovered.  To
display the data at full time resolution, you may need to select the
data in chunks (with {\tt select=time()}).

\subsection*{General \miriad\ Flagging -- UVFLAG}\label{s:uvflag}\index{uvflag}\index{visibility datasets, flagging}

In \task{uvflag}, you use the standard selection keywords to
select visibilities, or parts of visibilities for flagging.  The main
keywords are {\tt select} and {\tt line}.  \task{uvflag} can also be
used to unflag correlations, and to list the correlations that meet
the selection criteria without actually flagging the data.  There is a
keyword, {\tt edge}, that
provides a way to select the same group of channels in spectral-windows
(IFs).  This cannot be done with the standard {\tt line} and {\tt
select} keywords owing to the way spectral-windows are
stored (for more information, issue the command {\tt help windows}). 
Read the {\tt uvflag} help file if you need this facility.  It is
unlikely that you will. 

The following example flags some correlations between 1 and 2 hours on
all days involving all baselines with antenna 6 in channel 25 for all
polarizations and spectral-window (IF) 1.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVFLAG}} \\
\hline
vis=vela.line              & Specify visibility dataset \\
select=ant(6),time(1:00,2:00),win(1) & Select correlations \\
line=channel,1,25,1        & Select channel \\
edge                       & \\
flagval=flag               & Flag selected correlations \\
options=brief              & Summarise what happened \\
\ \ \ or                   & \\
options=noapply            & Summarise what could happen \\
log                        & Unset to report information to screen \\
\hline 
\end{tabular}
\end{center}


\section{Other Flagging Tasks}
\subsection*{\aips\ Flagging and \miriad -- FGFLAG}\label{s:fgflag}\index{fgflag}
\index{FITS files, FG tables}
If you flag in \aips, the flagging information will enter \miriad, via
FITS files, in two possible forms:
\begin{itemize}
\item As flagging (FG) tables (generally only multi-source files).
\item As negated visibility weights (both single- and multi-source files).
\end{itemize}
While \miriad's \task{fits} task flags correlations which have negative weights,
it {\em does not} apply any flagging tables. Instead
it copies across the flagging tables, and includes them within the
\miriad\ dataset. If you wish to use this flagging information, it is
essential that you then apply the appropriate flagging tables. The task
to do this job is \task{fgflag}. Its inputs are pretty simple -- typical
inputs are:

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf FGFLAG}} \\
\hline
vis=vela.line              & Specify visibility dataset \\
select			   & Extra selection -- normally unset\\
fgflag			   & Table version. Default is highest version.\\
\hline 
\end{tabular}
\end{center}
Note some caveats:
\begin{itemize}
\item The flagging tables written by \task{fits} cannot be copied to
another \miriad\ dataset. Additionally they are ignored by all \miriad\ tasks,
save \task{fgflag}.
\item Task \task{fits} does not copy across information which distinguishes
correlations by polarisation. If one polarisation is flagged in the FG table,
all polarisations will be flagged by \task{fgflag}.
\end{itemize}

\subsection*{Flagging Near a Set-Up Change -- QVACK}

\index{visibility datasets, flagging}
Another handy task is \task{qvack}\index{qvack}
(derivative of the \aips\ task
\task{quack} and so named for obscure reasons) for flagging a
number of correlations after the observing set-up has changed (i.e.,
the source changed, the pointing centre changed, or 
the frequency changed).  For example, it might be used if some evil-minded
child turned on the family microwave oven for 30~seconds every time s/he
saw the antennas start to move.  If you were observing at 13~cm, here is
how to get rid of such data.   Getting rid of the child would be the
best long-term solution.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf QVACK}} \\
\hline
vis=multi.uv              & Specify visibility dataset \\
select=freq(2.699,2.829)  & Select correlations in this range\\
interval=0.5              & Flag data for 30~s after change \\
force                     & Unset \\
mode=source               & Flag when source changes \\
\hline
\end{tabular}
\end{center}


\task{qvack} also has a keyword called {\tt force}.  This can be used
if the observing setup has not changed, but you still wish to periodically
flag some data.  For example, you may have split out
your observation and so 
you are left with a file with no observing change, but with
a distinct ``scan'' based regularity.  This keyword lets you flag
some data after a certain time has elapsed.


\subsection*{Flagging Based on a Template -- UVAFLAG}
\index{visibility datasets, flagging}
Often it is convenient to edit one visibility dataset (``the
template''), and then to apply the
flagging from this to another. For example, forming and flagging a
``channel-0'' dataset might be less painful than flagging a huge
spectral line dataset. Or you might want to flag a calibrated
dataset and then apply this to the raw data. The task that
can do these sorts of operations if \task{uvaflag} -- it matches
visibilities in the template and the input, and flags those visibilities
in the input that are flagged in the template.

The template dataset should be a subset of the dataset being flagged, and
the order of the records in the two datasets should be the same. Normally
correlations in the template and the input are matched by comparing time,
polarisation, baseline and frequency. However options {\tt nopol} and
{\tt nofreq} can turn off the matching of polarisation and frequency. Typical
inputs might be:

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVAFLAG}} \\
\hline
vis=multi.uv              & Dataset to be flagged.\\
tvis=multi.chan0	  & Template dataset, say a channel-0 one.\\
options=nofreq		  & Do not match on frequency, \\
			  & i.e. ignore the frequency in multi.chan0\\
\hline
\end{tabular}
\end{center}

\subsection*{Flagging a Sector of Data -- UVSECTOR}\index{uvsector}
The task \task{uvsector} flags a set of visibilities based on their
\MM{u-v} position angle. To understand the utility of this, consider
the samples measured in the \MM{u-v} plane
at a particular time instant. For an east-west array (i.e. the ATCA)
the samples will lie on a spoke of an ellipse. If there is a
glitch an a particular time, the bad data will
cause a stripe pattern in the resultant image which is at right angles to
the spoke. The position angle of
the stripe can be used to determine the \MM{u-v} plane position angle of
the glitch. From this you could determine the offending time and then
flag the appropriate data. Task \task{uvsector} performs these operations
for you.

Task \task{uvsector} will flag a sector of visibility data with a given
\MM{u-v} position angle (or hour angle). While you can give the position angle
of the sector to be flagged directly (via the {\tt angle} parameter), it is
more convenient to indicate the position angle indirectly via the stripe
direction in an image. How do you specify a stripe direction? You do
this by specifying a long, thin region in a image. Normally
you will generate the region by using \task{cgcurs}
(see Section~\ref{s:pgplot} for more information on \task{cgcurs}). With \task{cgcurs}, you display a greyscale
of the image, and then use a cursor to selection a long, thin, region
containing the crest of a single stripe. The selected region is then written
into a text file, {\tt cgcurs.region}. While this may be a rather odd
way to define a direction, it allows \task{uvsector} to use the
interactive facilities of \task{cgcurs}.

Typical inputs to use \task{cgcurs} are
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CGCURS}} \\
\hline
in=vela.icln		& Input image (probably CLEANed).\\
type=grey		& Greyscale plot.\\
range=			& Set the range to highlight the stripes.\\
device=/xs		& PGPLOT device -- Xwindows here.\\
options=region		& Define a region of interest.\\
\hline 
\end{tabular}
\end{center}

The width of the sector that \task{uvsector} flags
is given as a angle (in degrees) via the {\tt width} parameter.
Remember that 1 degree is equivalent to 4 minutes of observing time, so
do not set it to any value larger than you need.
Normally you would set this to a few degrees, based on your confidence in
the accuracy of the stripe direction. The default
is 5 degrees.

As \task{uvsector} flags a sector, if your
input visibility data-set contains multiple configurations, data within that
sector for all configurations will be flagged by default. Generally this is
{\em not} what you are likely to want. If you know which configuration contains the
bad data, you can use the {\tt select} keyword to ensure just that
configuration is affected. Selecting by {\tt time} is probably the easiest.

The breadth of a stripe also contains useful information -- a glitch in just
the short baselines will cause a broad stripe, whereas a glitch in just the
long baselines will cause a narrow stripe separation. A glitch in just one
baseline will give a two dimensional sinusoid.
Given the separation
between stripes, you could make an approximate calculation of the corresponding
\MM{u-v} radius of the bad data.
For a stripe separation (crest to crest) of \MM{x} arcseconds, the corresponding
\MM{u-v} radius is
\[
	r = \frac{206}{x} {\rm kilowavelengths}
\]
You may then use {\tt select=uvrange} so that only data at the appropriate
\MM{u-v} radius is flagged. For example if the crest-to-crest separation is
20 arcsec, we might flag data only with spacings of $10 \pm2$ kilowavelengths.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVSECTOR}} \\
\hline
vis=vela.uv		& Visibility data to be flagged.\\
angle			& Leave unset if defining position\\
			& angle by a stripe direction.\\
in=vela.icln		& The image containing stripes,\\
			&  used only for its coordinate information.\\
region=@cgcurs.region	& A long, thin, region along the crest\\
			& of a stripe.\\
width=3			& Flag data over 3 degrees (12 minutes of time)\\
select			& Leave unset  to flag all data in the sector,\\
\ \ \ or		&\\
select=uvrange(8,12)	& Flag data within the sector and between 8 to 12\\
			& kilolambda.\\
\hline 
\end{tabular}
\end{center}

\section{Listing Visibilities}\index{uvindex}\index{visibility datasets, listing}
\label{s:vislist}

\begin{enumerate}

\item The task \task{prthd}\index{prthd} summarizes some basic information about your
visibility dataset (it also summarizes images). It only reads the
first few records of the data, so if you have multiple frequency
setups or sources, it will only report the first setting.  It will tell
you the number of visibilities, the number of spectral-channels,
the number of spectral-windows (IFs), the polarizations
present and some information about frequencies.  Output can be directed
to a log file (the terminal is the default), and either a brief or
full listing can be given (the default is the full listing). Several
inputs can be given (wildcards are supported).

\item The task \task{uvindex} scans through the visibility dataset and reports
changes in the source, pointing centre, number of spectral-channels,
observing frequency, and polarization.  Thus, it builds a description of
your observation for general reference.  Give the visibility dataset
name with the keyword {\tt vis}.  The output can be directed to a text
file for printing via the keyword {\tt log}. 

\item The task \task{uvlist}\index{uvlist} can be used to list the values of the
correlations.  The generic \miriad\ keyword {\tt options} is used to
control exactly what is listed (see the help file).  The standard
keywords {\tt select} and {\tt line} are used to select the desired
visibility data and the desired part of the spectrum, respectively. 
Here is an example. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVLIST}} \\
\hline
vis=vela.uv                &Specify visibility dataset \\
options=data,brief         & Lists correlations (this is the default) \\
\ \ \ or                   & \\
options=spectra,brief      & Lists info. about the spectral-windows \\
select=pol(xx),ant(1)      & List \MM{XX} correlation for all \\
                           & baselines involving antenna 1 \\
line=channel,1,128,1,1     & List data for channel 128 only \\
recnum=100                 & List 100 records \\
log=xx.txt                 & Write results to this text file or \\
                           & leave unset to write to standard output \\
\hline 
\end{tabular}
\end{center}

\end{enumerate}

\section{Copying, Concatenating and Averaging Visibility Datasets}\label{s:uvaver}
\miriad\ contains a single task, \task{uvaver}\index{uvaver}, which
copies, concatenates and averages data (both in frequency and in time).
It can also apply calibration corrections and perform Stokes conversion.

The inputs are all fairly self-explanatory -- see the help file for more
information.  The default behaviour is to copy all the data, applying
calibration corrections as it goes.  If some averaging is being
performed, vector averaging is normally used both for time and
frequency.  However an option ({\tt options=ampscalar}) switches the
behaviour so that amplitude-scalar averaging is used for time averaging. 
Vector averaging is always used when averaging in frequency. 

A brief summary of the inputs are:
\begin{itemize}
\item {\tt vis}: a list of the input visibility datasets. Several
can be given. Wildcards are supported.
\item {\tt out}: The name of the output visibility dataset. 
The output will be the concatenation of all the selected
input data. No default.
\item {\tt select}: The standard visibility data selection parameter.
See Section~\ref{s:select}. The default is to select all data.
\item {\tt stokes}: The standard parameter to determine Stokes
conversion. The default is to copy the polarizations or Stokes parameters
as is (no conversion). See Section~\ref{s:polar}.
\item {\tt interval}: Time averaging interval, in minutes. The default is
to perform no time averaging.
\item {\tt line}: The standard {\tt line} parameter, which dictates
channel selection and frequency averaging in multi-channel datasets. The default is
to copy all channels unaveraged. See Section~\ref{s:line}.
\item {\tt options}: Extra processing options. The {\tt nocal}, {\tt nopass}
and {\tt nopol} options are used to disable correcting for antenna gains,
bandpass functions and polarization leakage (see Section~\ref{s:onthefly}).

There are also three options to determine the sort of averaging to be
performed. Possible options are {\tt vector}, {\tt scalar} and {\tt scavec}.
Note these options only affect the averaging in {\em time}. Vector averaging
is always use when averaging in {\em frequency}.
\begin{itemize}
\item
``Vector'' averaging is just a straightforward average of the complex-values
visibility data. This is the optimum thing to do if the data are corrupted
by just Gaussian noise. However it is common for the data to
be affected by fast-varying phase fluctuations. While the amplitude of the
data are good before averaging, a plain vector average will result in
some decorrelation.
\item  ``Scalar'' averaging avoids this by using the
average amplitude for the amplitude of the output quantity (the phase of the
output is still the phase determined by vector averaging). The problem with
scalar averaging is that it results in a noise bias when the signal-to-noise
ratio is small (less than a few) -- you should not use scalar averaging in
this regime.
\item For cases where phase stability is poor, and the signal-to-noise
ratio on the parallel-hand correlations (\MM{XX} and \MM{YY}) is good, but the
signal-to-noise ratio on the cross-hand correlations (\MM{XY} and \MM{YX} is
poor, \task{uvaver} provides the {\tt scavec} options. This performs
scalar averaging on the parallel-hand correlations, and vector
averaging on the cross-hand correlations.
\end{itemize}

\end{itemize}

For example, the inputs below show how to form a channel 0 dataset from
a data set with two spectral windows (IFs), each with 33 channels
(selecting the middle 25 channels from each window).  The two spectral
windows are essentially treated as 66 contiguous channels.  Thus, we
must use the {\tt line} keyword to select channels 4 to 28, and then
channels 37 to 61.  \task{uvaver} will correclty maintain the dual
window status (use \task{prthd} to verify this).

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVAVER}} \\
\hline
vis=vela.uv               & Input dual window 33 channel dataset \\
out=vela.uv.0		  & Output channel 0 dataset \\
line=channel,2,4,25,33	  & Average channels 4 to 28  in each window\\
\hline 
\end{tabular}
\end{center}

\section{Plotting Visibilities}\label{s:visplot}\index{visibility datasets, plotting}

\begin{enumerate} 

\item You may tire of all this listing of numbers, and would prefer to
make some plots.  The task \task{uvplt}\index{uvplt} provides a variety
of choices when plotting visibilities, and ancillary information
associated with the visibilities.  It has rather a lot of inputs, so
read the help file first for the details.  We discuss some of the
keywords below and then give a couple of examples. 

\begin{itemize}

\item {\tt vis} specifies the visibility datasets to plot.  It can take
multiple dataset names, each separated by a comma, and wildcard
expansion (via an asterisk) of file names is supported. 

\item {\tt line} and {\tt select} are the standard visibility selection
keywords.  The default for the {\tt line} parameter is the first channel,
so be careful if you are plotting a dataset with many channels; you would
need to explicitly select the channels you require, especially as in
multi-channel datasets the first few channels are either rubbish or
flagged.  The default for {\tt select}
is all the data.

\item Select the desired polarization or Stokes parameter with {\tt stokes}.
Conversions from linear (or circular) polarizations to Stokes parameters
are supported.  You can have any combination that you like on one plot,
but they will all be plotted with the same symbol.  This generally
does not matter as any signal will make the distinctions clear and colours are
used on devices that support it.

\item {\tt axis} is used to tell \task{uvplt} what to plot on each axis. 
You can put any of the allowed choices (see the help file) on either of
the axes.  For example, you could set {\tt axis=imag,real} to plot the
imaginary part of the visibility on the x-axis, and the real part on the
y-axis.  Such a plot is very useful to examine calibrator data.
The default is to plot visibility amplitude versus time. 

\item {\tt xrange} and {\tt yrange} specify the plot ranges for the
x- and y-axes.  If they are unset, the plot(s) are auto-scaled.
Generally, each of these keywords takes two values.  However,
if the corresponding axis is {\tt time}, then you must specify eight
values; a day, hour, minute and second for the start and stop
times.

\item You can average the plotted quantities in time by setting {\tt
average} in minutes (but see the {\tt options} keyword for exceptions to
this unit).  Baselines and polarizations are averaged separately (unless
{\tt options=avall}), but channels are not -- they are all lumped in
together. 

The complex visibilities are vector averaged by default, and the real
quantities are scalar averaged by default.  For example, if you ask for
an averaged amplitude or phase, the real and imaginary parts of the
visibilities are averaged, and then the amplitude or phase formed.  If
you ask for, say, \MM{u} versus time, \MM{u} is scalar averaged.  You can
override the vector averaging by setting {\tt options=scalar}; a scalar
averaged amplitude is useful if the phase is winding rapidly.  A scalar
averaged phase is pretty meaningless under most conditions.

Averaged points are plotted with filled-in circles rather than dots,
unless {\tt options=dots}.   

\item If you plot, say, amplitude or phase versus time, you may find the
application of Hanning smoothing useful to knock off the rough edges,
rather than applying box-car smoothing (which is what you get
when you average in time).  Set {\tt hann} to an odd number.

\item If you do not want to see all the selected points, you can set {\tt
inc} to plot every {\tt inc}th point that would normally have been
selected.  For example, it is useful for plotting \MM{u} versus \MM{v}.  Be
careful of a value of {\tt inc} that divides exactly into the number of
baselines in time-ordered data (you end up seeing the same baselines
over and over). 

\item {\tt uvplt} has a vast complement of {\tt options}.  They
are all described in the help file.  We will mention a few
of them here.

\begin{description}

\item Like most of the visibility tasks, it has {\tt nocal, nopol} and
{\tt nopass} to turn off the calibration tables, which, by default, are
applied if they exist.  \miriad\ will tell you when it applies any
calibration table.

\item By default, \task{uvplt} puts each baseline on a separate sub-plot on
the page (see {\tt nxy} below).  Setting {\tt options=nobase} means that
all baselines go on the same plot. 

\item {\tt options=scalar} instructs \task{uvplt} to do scalar averaging in
time. 

\item On occasion, you may not wish to time average baselines or
polarizations separately.  Setting {\tt options=avall} instructs
\task{uvplt} to average together everything that is to be plotted on a separate
sub-plot.

\item {\tt options=rms} instructs \task{uvplt} to plot error bars on
any time-averaged quantities.  However, note that error bars are not yet
implemented for vector averaging. 

\item By default, if you plot many sub-plots (one for each baseline), all the
sub-plots are displayed with the same axis ranges, which encompass the
data on all the sub-plots.  Setting {\tt options=xind} or {\tt
options=yind} (or both) scales that axis on each sub-plot independently. 

\item \task{uvplt} has an interactive mode invoked by setting
{\tt options=inter}.  This gives you a chance to redraw the plot
with different axis ranges, without having to read all the data in
again.  This option also prompts you for a new plot device, so 
that following display and range refinement on your workstation,
you can them make a hard copy without re-running \task{uvplt}.

\end{description}

\item Select the PGPLOT device to plot on with a command such as
{\tt device=/xs} (plot on local X-window) or {\tt device=amp.plt/ps}
(write plot to a PostScript file called `amp.plt').

\item Specify the number of sub-plots in the x- and y-directions
with a command such as {\tt nxy=4,3}.  

\end{itemize}

The following example plots \MM{u} versus \MM{v}, and \MM{-u} versus \MM{-v},
selecting channels at the start, middle and end of a band of 32 channels
(so that the \MM{u-v} coverage benefit obtained across the band can be
seen). 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVPLT}} \\
\hline
vis=vela*.uv               & Specify visibility dataset(s) \\
line=channel,3,2,1,15      & Plot channels 2,17,32 \\
select                     & Leave unset for all data \\
stokes=xx                  & \MM{u-v} coverage same for all polarizations \\
axis=uc,vc                 & Plot \MM{u-v} and conjugate \MM{u-v} plane \\
xrange                     & Auto-scale \\
yrange                     & Auto-scale \\
average                    & No averaging \\
hann                       & No Hanning \\
inc=1                      & Plot every point \\
options=nobase             & All baselines on one plot \\
device=/xs                 & Plot on local X-window \\
nxy=1                      & One sub-plot only please \\
size                       & Default character sizes \\
\hline 
\end{tabular}
\end{center}

The next example shows how to plot a scatter diagram (real vs imaginary)
from a single-channel dataset for the \MM{Q}, \MM{U} and \MM{V}. 
polarizations, with all baselines plotted on the one plot.  This is a
quite useful plot to make of calibrated calibrator data.  Because you
are asking for Stokes parameters, it is assumed that a calibration has
been made (the calibration tables will be applied by \task{uvplt} and a
reminder issued) or that you converted to Stokes parameters along the
way (e.g.  with the \aips\ task {\tt ATLOD} or with \task{uvaver}). 


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVPLT}} \\
\hline
vis=vela.uv                & Specify visibility dataset \\
line                       & Plot the first channel only \\
select                     & Leave unset for all data \\
stokes=q,u,v               & Select Stokes \MM{Q}, \MM{U} and \MM{V} \\
axis=real,imag             & Is the default \\
xrange                     & Auto-scale \\
yrange                     & Auto-scale \\
hann                       & No Hanning \\
inc                        & Plot all points \\
options=nobase             & Scalar averaging \\
device=/xs                 & Plot on local X-window \\
nxy=1                      & 1 plot per page \\
size                       & Default character sizes \\
\hline 
\end{tabular}
\end{center}

\item\index{visibility datasets, closure phase plotting}

Task \task{closure} is another useful plotting task. It is
most useful for plotting
the closure phases of point sources. The closure phase
of an object is the sum of the baseline phases around a triangle. For
example, for antennas 1, 2 and 3 we measure three baseline phases
$\phi_{12}$, $\phi_{23}$ and $\phi_{31}$. It can be shown that
the closure phase, which is the sum of these three baseline phases
\[
	\phi_{\rm closure} = \phi_{12} + \phi_{23} + \phi_{31}
\]
will be independent of any antenna-based gain errors (atmosphere and
instrumental). This quantity will not be affected by \miriad's
antenna calibration process. Additionally for a point
source (or any source with 180 degree rotational symmetry) the closure
phase should be zero. Plotting the closure phase of a calibrator
is thus a way of checking the quality of the data. If the closure
phase is large, the data are probably bad, or there is some calibration
error that is not accounted for in \miriad's antenna-based model.

Task \task{closure} plots averages of closure phases (actually it
averages together {\em triple products} and then takes the phase of
this). The averages are taken over the the different correlator
channels and polarizations, and optionally over time.

The inputs to \task{closure} are simple enough.
See the help file for more information. Typical inputs to
\task{closure} are:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CLOSURE}} \\
\hline
vis=vela*.uv            & Specify visibility dataset(s) \\
line=			& Set the channels of interest.\\
select                  & Leave unset for all data \\
stokes=xx               & Closure phase for XX\\
yrange                  & Auto-scale \\
options=notrip		& Plot all closure phases on one plot \\
device=/xs              & Plot on local X-window \\
nxy=1                   & One sub-plot only please \\
\hline 
\end{tabular}
\end{center}


\item\index{visibility datasets, spectral plotting}
If you are working with spectral data (line or continuum) you may
want to look at some spectra.  Use task \task{uvspec}\index{uvspec} for this.
The inputs to
\task{uvspec} are pretty much self-explanatory.  Here is an example of
how to plot the spectra (phase) of the \MM{XX} and \MM{YY} correlations
averaged over 15~minutes for all baselines involving antenna 1 with all
calibration turned off.  \task{uvspec} makes a new sub-plot every time
it begins a new averaging interval.  Assuming that there are five antennas
in this dataset, we have arranged for each baseline to occupy one column
of the plot page with time increasing down the page. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVSPEC}} \\
\hline
vis=vela.line              & Specify visibility dataset \\
select=ant(1)              & Baselines involving antenna 1 \\
line                       & Unset for all channels \\
stokes=xx,yy               & \MM{XX} and \MM{YY} correlations \\
interval=15                & 15 minute averages \\
options=nocal,nopol,nopass & Turn off calibration, \\
axis=freq,phase            & Plot phase versus frequency \\
yrange=-185,185            & Specify y-range for plot \\
device=/xs                 & Plot on local X-window \\
nxy=5,6                    & 6 time intervals in y directions \\
\hline 
\end{tabular}
\end{center}

\end{enumerate}

\section{Examining Visibility Variables}\index{visibility datasets, variables}
\label{s:exavis}

There are a variety of variables associated with the dataset that you
may like to examine.  For example, you may like to plot the system
temperature as a function of time; it is usually stored as a variable
(the visibilities are variables too).  A list of standard visibility variables
is given in Appendix~\ref{app:uvvars}. Some of these variables are specific to
the BIMA interferometer (Hat Creek), and so may be absent, or have no
relevance to ATCA data.

The task \task{varlist}\index{varlist} will tell you the names of the variables in your
dataset.  It will also tell you their data types (ascii, double
precision, single precision and integer) and lengths. 

After finding out the name of the variable of interest, you can plot it
with \task{varplt}\index{varplt}. 
The following example plots the on-line \MM{XY} phase differences, stored
in the variable {\tt xyphase} (which will be present if you used
\task{atlod} in \miriad).  You can also use
\task{varplt} to write the variable's values to a text file with the
standard {\tt log} keyword. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf VARPLT}}  \\
\hline
vis=vela.line              & Specify visibility dataset \\
device=/xs                 & Plot on local X-window \\
log=var.txt                & Write values to text file \\
xaxis=time                 & Put variable {\tt time} on x-axis \\
yaxis=xyphase              & Put variable {\tt xyphase} on y-axis \\
nxy=3,2                    & One sub-plot per antenna \\
xrange                     & Unset for auto-scale \\
yrange                     & Unset for auto-scale \\
options                    & Leave unset \\
\hline 
\end{tabular}
\end{center}

\section{Modifying Visibility Datasets}\index{visibility datasets, modifying}
The task \task{uvedit}\index{uvedit}
allows you to apply some specific sorts of
corrections or modifications to your visibility dataset.  For
example, there may be a clock error that needs fixing, or the antenna
coordinates were wrong so that \MM{u} and \MM{v} need to be recomputed, or the
phase centre was wrong requiring a phase rotation and \MM{u} and \MM{v}
recomputation.  Note that any data not selected are copied to the output
dataset unchanged.  

The following example changes the RA phase centre by 10~seconds of time
and also corrects a delay error of -3~ns on antenna 2 (by adding a
phase gradient across the band).  \task{uvedit} has the option to
turn off the recomputation of \MM{u} and \MM{v} which might otherwise be done
({\tt options=nouv}).  This can be useful for say, entering a delay
change (which does not need \MM{u} and \MM{v} to be recomputed) because
\miriad\ does not compute \MM{u} and \MM{v} as accurately as the on-line
ephemeris routines.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVEDIT}} \\
\hline
vis=multi.uv              & Input visibility dataset \\
source=0823-500         & Specify source; unset means all \\
antpos                    & No change to antenna coordinates \\
dantpos                   & \\
ra=10                     & Move RA phase centre \\
dec                       & No change to DEC \\
time                      & No clock offset \\
delay=0,-3,0,0,0,0        & Delay error on antenna 2 \\
out=multi\_corr.uv         & Output visibility dataset \\
options                   & Do the works \\
\hline 
\end{tabular}
\end{center}

The task \task{uvredo}\index{uvredo} also `edits' a visibility dataset. Unlike
\task{uvedit}, however, it only modifies ancillary data in the
dataset that are derivate from other information. For example, it
can recompute the sky feed angle (``chi'') and velocity
information.


\newchapter
\chapter{Calibration, the ATCA, and \miriad}\label{c:cal}


\section{Some Theory}\index{calibration, polarimetric}\index{linear feeds}
\index{Stokes parameters}

Much of the theory here has been borrowed from the memo
\htmladdnormallink{`AT Polarisation Calibration'}
{http://www.atnf.csiro.au/observers/memos/d97b7f\~{}1.pdf} (AT Memo 39.3/015).
See that memo for more details.
A more detailed description of polarimetric interferometry can be found in
\htmladdnormallink{Hamaker, Bregman \& Sault (1996)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1996A\%26AS..117..137H}
and \htmladdnormallink{Sault, Hamaker \& Bregman (1996)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1996A\%26AS..117..149S}
(A\&AS 117, 137 and A\&AS 117, 149).

Recall from Chapter~\ref{c:visconcepts} that \miriad\ models a feed
as having a composite gain of
\[
g(t)g_{P}(\nu)	\exp(i2\pi\tau(t)(\nu-\nu_0))
\]
where \MM{g(t)} is a time variable complex number (often loosely called the
antenna gain), $g_{P}(\nu)$ is the bandpass function, and $\tau$ is a
time-varying delay term.

Additionally, we have to consider the response of the feeds to polarised
emission.
Whether a feed is linearly or circularly polarised, its instantaneous
response to a signal is a linear combination of two of the four Stokes
parameters that describe the wave.  In the equatorial frame of the
source, ideal linear feeds respond according to

\begin{eqnarray*}
XX &=& I + Q \\
YY &=& I - Q \\
XY &=& U + iV \\
YX &=& U - iV, \\
\end{eqnarray*}
where the \MM{X} and \MM{Y} feeds are at position angles 0 and 90\degrees,
respectively.  Perfect circular feeds respond according to
\begin{eqnarray*}
RR &=& I + V \\
LL &=& I - V \\
RL &=& Q + iU \\
LR &=& Q - iU \\
\end{eqnarray*}

These equations show immediately why it is harder to calibrate an
instrument with linear feeds, such as the ATCA, compared with an
instrument with circular feeds, such as the VLA.  In the latter case,
one can make the excellent assumption that the calibrators, which are
used to determine the antenna gains, are not circularly polarised. 
Thus, the \MM{RR} and \MM{LL} visibilities are a direct measure of \MM{I}, for
which we have a good model ({\it i.e.} a point source of known flux density). 
Consequently they can be used to calibrate the gains with time.  On the
other hand, it is not necessarily a good assumption that a calibrator is
not linearly polarised, so that \MM{XX} and \MM{YY} correlations cannot always
be used as a direct measure of \MM{I}. 

In addition, for `alt-az' telescopes, the feeds rotate with respect to the
equatorial frame.  This causes the actual response of ideal linearly
polarised feeds to vary with the angle between the ``sky'' and the feed --
$\chi$ (which
varies with time, although non-linearly), according to

\begin{eqnarray*}
XX &=& I + Q \cos(2\chi) + U \sin(2\chi) \\
YY &=& I - Q \cos(2\chi) - U \sin(2\chi) \\
XY &=& iV - Q \sin(2\chi) + U \cos(2\chi) \\
YX &=& -iV - Q \sin(2\chi) + U \cos(2\chi) \\
\end{eqnarray*}

So far we have assumed that the feeds are ideal.  This is never the
case, and their departure from the ideal can be characterised by
leakage terms.  $D_{\rm x}$ is the leakage of the \MM{y} component of the
electric field into the \MM{X} feed, and $D_{\rm y}$ is the leakage of the \MM{x}
component of the electric field into the \MM{Y} feed.  Another way of
thinking of them is the combination of the ellipticity and error in the
position angle of the polarisation ellipses of each feed. 

Incorporating these leakage terms results in fairly gory equations -- see 
\htmladdnormallink{Hamaker, Bregman \& Sault (1996)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1996A\%26AS..117..137H}
and \htmladdnormallink{Sault, Hamaker \& Bregman (1996)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1996A\%26AS..117..149S}
(A\&AS 117, 137 and A\&AS 117, 149) for more details.

\section{Calibration Methods}
There are two distinct calibration situations with the ATCA, depending
on whether the correlator configuration produced all four polarisation
products (\MM{XX}, \MM{YY}, \MM{XY} and \MM{YX}) or just two (\MM{XX} and \MM{YY}). The former
is done in continuum mode and in some spectral line modes, whereas the latter
is true of other spectral line modes.

\subsection*{Calibration of Data with \MM{XX}, \MM{YY}, \MM{XY} and \MM{YX}}
If all four polarisation products are measured, a full polarimetric
calibration is possible. We generally solve for the leakage terms,
the gains, a bandpass function and calibrator \MM{Q}
and \MM{U}.

In the solution process, we must consider the time variability of the
various parameters. The gains vary fairly rapidly with time, and
we typically solve for them every hour or so, with the
assumption that they are steady over a solution interval of a few
minutes -- the calibrator scan.  On the other hand, the leakage terms
are assumed to be steady over the entire length of the observation.  Our
experience so far appears to support this. The bandpass function and
the calibrator \MM{Q} and \MM{U} can also be assumed to remain unchanged during
an observation.  It is not possible to solve for a calibrators
\MM{V} (circularly polarised flux density)
However, a calibrator's \MM{V} is unlikely to be significant ( $< 0.1$ \%). 

If the calibrator is weakly polarised ( $< 5$\%), one quantity that it
is difficult to solve for is the absolute \MM{XY} phase, absolute feed
alignment and absolute feed ellipticity.
Generally these quantities
on the reference antenna has to be either assumed, or measured, or derived
from observations of a strongly polarised calibrator.  Unfortunately most
calibrators are only weakly polarised. Fortunately, however, the
alignment and ellipticity errors are small (assuming Faraday rotation is
negligible). Additionally, since November 1992,
the ATCA makes a good measurement of the absolute \MM{XY} phase (accurate
to a few degrees).
If we assume that the absolute alignment and ellipticity errors of the
reference antenna are zero and that the \MM{XY} phase measurements are
accurate, a ``polarisation position angle
calibrator'' is not needed.

The recommended strategy for calibrating ATCA data is thus to observe
a primary flux density calibrator, normally 1934-638, as well as a secondary phase calibrator
close to the source. For polarimetric observations, as the polarisation of 
1934-638 is known (in fact it
is $< 0.2\%$), and the flux density is strong,
it is possible to derive polarisation leakages from it given only a short
observation (5-10 minutes). If you have good parallactic angle coverage of
the secondary, it is possible
to simultaneously determine the polarisation leakages as well as \MM{Q} and \MM{U}
for the secondary (``good parallactic angle coverage'' means
at least 5 observations spread over a parallactic angle range of $100^\circ$). This will be the normal case for a 12 hour observation of
one source. Alternatively if there is poor parallactic angle coverage
of the secondary (e.g. resulting from snapshot observations), the instrumental
polarisation
derived from 1934-638 can be used, and the \MM{Q} and \MM{U} of the secondary
can still be determined.

The above strategy differs from that in the previous version of this guide.
This is as prior to November 1992, the hardware to measure the \MM{XY} phase 
produced unreliable results, particularly at
20 and 13~cm. Thus an observation of a strongly polarised calibrator was 
strongly advisable to determine \MM{XY} phase. The strategy used for calibrating
data prior to November 1992 will be somewhat different to that presented here.

\subsection*{Calibration of Data with \MM{XX} and \MM{YY} Only}
Calibrating data containing \MM{XX} and \MM{YY} is somewhat simpler. You have
insufficient information to solve for instrumental polarisation and
the calibrator \MM{Q} and \MM{U}. You have no choice but to assume that these are
zero. This means that we can skip a number of steps in the calibration
process.

All these assumptions will result in some error in the calibration -- this
is fundamentally
unavoidable if only \MM{XX} and \MM{YY} are measured. Provided the secondary
calibrator is weakly polarised, the error is unlikely to be too substantial.


\section{Determining Calibration Solutions}\index{calibration, solvers}

There are currently four main tasks which solve for calibration parameters -- each
with their own strengths and weaknesses. These are:

\begin{itemize}

\item \task{mfcal}\index{mfcal}: This determines antenna gains, band passes
and delays
given a point-source calibrator.  Multi-frequency observations are
supported.  Although it can handle dual polarisation data, it assumes
the calibrator is unpolarised, and that the polarisation leakages are 0. This
assumption does not affect the bandpass or delay that it finds.

\item \task{gpcal}\index{gpcal}: This determines antenna gains and leakages given a
point-source calibrator. The polarisation properties of the calibrator can
be determined as part of the solution process. Gains and leakages are
assumed to be independent of frequency.

\item \task{selfcal}\index{selfcal}: This determines a single set of antenna gains
given a model of the visibility data.  The antenna gains are assumed to
be independent of all observing parameters other than time and antenna
number.  Consequently, its handling of multi-frequency and polarisation data
is fairly simple.  It can handle multiple pointings and sources ({\it e.g.} 
mosaiced observations). 

\item \task{gpscal}\index{gpscal}: This is useful when self-calibrating strongly
polarised sources measured with a telescope with linear feeds ({\it e.g.}  the
ATCA).  Given models of Stokes \MM{I}, \MM{Q}, \MM{U}, and \MM{V}, as well as the
leakage parameters, this solves for the appropriate antenna gains. 

\end{itemize}

We will now describe \task{mfcal} and \task{gpcal} in some detail. This is
largely for reference -- the following chapter will give a step-by-step
approach to the calibration process. The self-calibration tasks,
\task{selfcal} and \task{gpscal}, are discussed in Chapter~\ref{c:selfcal}.

\section{Determining Gains and Bandpass Functions -- MFCAL}\label{s:mfcal}
As noted above, \task{mfcal} solves for antenna gains and bandpass function.
It can also solve for the delay terms, but this is rarely useful. However
it does assume an unpolarised calibrator and that the polarisation leakage
terms are zero. If you have measured only the \MM{XX} and \MM{YY} correlations,
you {\em have to} make these assumptions. However if you have measured
all four polarisation products, then you can correct
for these assumptions by running \task{gpcal} after \task{mfcal}.

We will now discuss some of the inputs to \task{mfcal}.
\begin{itemize}
\item {\tt vis:} The input dataset -- the dataset containing the primary
calibrator in this case.
\item {\tt line:} The standard linetype parameter, used to select and
average channels. See the discussion of this in Section~\ref{s:line}.
Assuming the 128 MHz / 33 channel continuum system, you
will want to discard the first and last few channels, calibrating only
the central 25 channels of so. Otherwise you
will not normally want to perform any averaging.
\item {\tt edge:} This is an alternative way of indicating the channels
to discard. When working with single IF datasets, it is probably
easier to use the {\tt line} parameter.
\item {\tt interval:} This gives one or two numbers,
in minutes, being the maximum solution interval length, and
maximum solution gap, in minutes. The latter can usually be allowed to
default. The default maximum interval length is
5 minutes.
During stable observing
conditions (good phase stability) you should set the maximum solution length
to no more than the calibrator scan time.
Set it to a small value if phase stability is poor (e.g. 1 to 0.1 minutes).
If you set it to a smaller value, however, the solution process becomes
more sensitive to bad data -- make sure you flag it well. Additionally you
might run into program memory limitations, where it aborts due to ``buffer
overflows'' or the like.
\item {\tt select:} Normal data-selection.
\item {\tt refant:} The reference antenna. Set this to the antenna
which had the cleanest \MM{XY} phase. 
The default is 3 (which may not be very appropriate).
\item {\tt flux:} This gives the flux density and spectral index of the calibrator.
\item {\tt options:} There are a few extra options, which are not of
great use or interest.
\begin{description}
\item[{\tt delay}] Note that old versions of \task{mfcal} attempted
to solve for a delay by default. This is no longer the case. To solve
for delays (which is strongly discouraged when calibrating a single IF band),
you now have to explicitly use the {\tt delay} option.
\item[{\tt interpolate}] The {\tt interpolate}
option may be useful if you cannot determine the bandpass for some
completely flagged channels but still need an estimate of the bandpass
function.  With this option, the bandpass is spline
fit (in real and imaginary) and evaluated for the flagged channels.
Be careful if a substantial fraction of channels are bad -- the algorithm
is only implemented if less than 50\%\ of the channels are flagged bad.
\item[{\tt oldflux}] If you are calibrated data that are to be combined
or compared with ATCA data reduced before August 1994, you will generally
want to use the `{\tt oldflux}' option to select the pre-August 1994 ATCA
flux scale. See Section~\ref{s:1934flux} for more information.
\end{description}

\end{itemize}

\section{Determining Gains and Polarimetric Properties -- GPCAL}\label{s:gpcal}
Task \task{gpcal} is the main workhorse of the \miriad\ calibration
system, although it is only particularly useful if you have measured
all four polarisation products (\MM{XX}, \MM{YY}, \MM{XY} and \MM{YX}). It has
a plethora of options to turn on and off various solvers. Most of these
will be irrelevant to normal use -- particularly when calibrating a
source, such as 1934-638, which is known to be unpolarised. However it
does not determine a bandpass function. So, if you have not averaged
you data into a channel-0 dataset, you should precede \task{gpcal} with
\task{mfcal} -- \task{gpcal} will normally apply any bandpass function
it finds with the dataset before it performs it real work.

Do not be intimidated by the number of options -- we
give you advice on which ones to use in the following chapter. We now discuss
the various inputs.

\begin{itemize}
\item {\tt vis}: The input dataset.
\item {\tt line} can be used to select the range of channels, and the
averaging to be performed on a multi-channel dataset. See Section~\ref{s:line}
for more information. Generally you {\em should not} specify any
averaging. Task \task{gpcal} performs its own averaging in a fashion which
gives best results.
\item {\tt interval}: This {\tt interval} parameter has the same meaning
as that described in Section~\ref{s:mfcal} above, and the advice for setting
it is the same.
\item {\tt flux} gives the values of the four Stokes parameters for
your calibrator source.  If you choose to solve for \MM{Q} and \MM{U}, (see
{\tt options} below) then these are initial guesses only -- it is not
necessary to give them accurately, or at all, in this case.
Make sure you get the
signs of \MM{Q}, \MM{U} and \MM{V} right; these are real numbers, not visibility
amplitudes.  Note that \task{gpcal} does recognise a small group of
calibrators for which it knows the spectrum in \MM{I}, and sometimes \MM{Q}
and \MM{U} -- see the help file for details. The primary calibrator
1934-638 is amongst these. If \task{gpcal} does not
recognise the source, the default is an unpolarised source. This will
be quite inappropriate if you have a polarised source and do not solve
for \MM{Q} and \MM{U}. If no flux density is given, and the source is not known, then
\task{gpcal} assumes that the rms gain amplitudes are 1 -- and determines
the calibrator flux density accordingly. This will be a good approximation
if you equalised the gains at the start of the observation
(which is the normal practise). However, at this stage, this approximation
is largely a convenience -- any error in the assumption will be corrected later.

\item {\tt tol}: The iterative procedure converges when the solutions
are unchanged from the previous solutions by an amount {\tt tol}.  The
default is \MM{0.001}.  Make this smaller if you want more iterations. 

\item {\tt xyphase}:
\index{XY phase}\index{calibration, XY phases} This parameter is an
artifact of history -- it can be ignored when following the current
recommended procedure for calibrating ATCA data.
This parameter can be used as an
alternative way of specifying
the \MM{XY} phase of the antennas.
If the \MM{XY} phase on the reference antenna is constant to
good approximation, and provided you {\em have not} already applied
any \MM{XY} phase correction (i.e. neither in \aips\ or \miriad\ \task{atlod}
nor with
\task{atxy}), then you can give the \MM{XY} phases of the antennas here.
Unless you explicitly turn off solving for \MM{XY} phase in \task{gpcal}, then
the only important \MM{XY} phase value is that for the reference antenna.
All the same you should give values for all antennas.
One significant catch (for arcane reasons) is that the
\MM{XY} phase reported by \aips\ is not the \MM{XY} phase used by \miriad. 
They are related by:
\index{XY phase, convention difference}
\[
\phi_{miriad} = \left\{ \begin{array}{ll}
		\phi_{aips} - 180 & \mbox{if sideband indicator is 1}\\
		-\phi_{aips}- 180 & \mbox{if sideband indicator is -1}\\
		\end{array}\right.
\]
The value of the sideband indicator is noted
in the history generated by \task{ATLOD}. More simply,
however, it will also be the  sign of the frequency increment.

\item {\tt options} controls what \task{gpcal} solves for.  Several
options, separated by commas, can be given.  It is important that you
understand the different choices. 

\begin{description}
\item[{\tt oldflux}] If you are calibrated data that are to be combined
or compared with ATCA data reduced before August 1994, you will generally
want to use the `{\tt oldflux}' option to select the pre-August 1994 ATCA
flux scale. See Section~\ref{s:1934flux} for more information.

\item[{\tt qusolve}] means that \task{gpcal} will solve for \MM{Q} and \MM{U},
taking the model you gave in {\tt flux} as a starting point.  You need
many cuts of the calibrator with good parallactic angle coverage to do
this successfully. You should not attempt to solve for \MM{Q} and \MM{U} of
the primary, as you will invariably have too little data. Additionally
1934-638 is known to be unpolarised. 

\item[{\tt xyvary}:] If the telescope settings are not altered, the \MM{XY} phases
(which are purely instrumental) appear to remain constant to better than
a few degrees.
By default, \task{gpcal} assumes the \MM{XY} phases are constant. Because of the
reliability of the \MM{XY} phase measurement system, this assumption is now
known to be inappropriate. It is better to let \task{gpcal} solve for
the \MM{XY} phases as a function of time -- use option {\tt xyvary} for this.
Note that \task{gpcal} can do this for
all antennas {\em except} the reference antenna -- the reference antenna
is assumed to be constant and (generally) zero.

\item[{\tt xyref}] means that \task{gpcal} will solve for the \MM{XY} phase of the
reference antenna.  To do this, the source must be strongly polarised. 

\item[{\tt noxy}] means that \task{gpcal} will not solve for any \MM{XY} phases. 
By default it solves for all \MM{XY} phases, except for the
reference antenna. This is generally not an appropriate switch to use.

\item[{\tt polref}] means that \task{gpcal} solves for all the leakage 
parameters of the reference antenna.  By default it does
not attempt to solve for the misalignment and ellipticity of the \MM{X}
feed of the reference antenna.  It is only possible to use this option
if the source is strongly polarised (at least 5\%).  If you specify \MM{Q}
and \MM{U}, both these terms can be found.  However, if you ask for the
{\tt qusolve} option, then the misalignment term cannot be determined. 

\item[{\tt nopol}] means that \task{gpcal} does not solve for the polarisation
leakage terms.  You {\em must} use this option if you are calibrating
data without \MM{XY} or \MM{YX} correlations.

\item[{\tt noamphase}] means that \task{gpcal} does not solve for the antenna
gains.  This option is rarely useful. 
                           
\end{description}
\end{itemize}


\section{Copying Calibration Tables -- GPCOPY}\index{calibration, copying}
\index{gpcopy}

You will often wish to copy calibration tables from one dataset to
another ({\it e.g.}  between the calibrator dataset and the program source). 
This function is performed by \task{gpcopy}.  Task \task{gpcopy} is
quite simple, taking as inputs the input and output dataset names.  The
{\tt options} keyword can be used to inhibit the copying of various
tables using the {\tt nocal}, {\tt nopol} and {\tt nopass} switches.

\section{Displaying Calibration Tables -- GPPLT}\label{s:gpplt}
\index{calibration, plotting}
\index{gpplt}

Calibration corrections can be examined with the task \task{gpplt}.
This task can list and plot antenna gain, bandpass, delay and
leakage terms.  Most of the inputs are self-explanatory -- see the help
file for more information.  Only a few parameters will be described
here. 

\begin{itemize}
\item The input dataset is given by the normal {\tt vis} parameter.

\item The {\tt device} parameter specifies the PGPLOT device for the
plot, whereas the {\tt log} parameter gives the output logfile for the
listing.  Either {\tt device} or {\tt log} must be set. 

\item The {\tt options} parameter determines what quantities to plot or
list.  Several quantities can be given, separated by commas. 
Possibilities are {\tt gains} ({\it i.e.}  the antenna gains), {\tt
polarisation} (the polarisation leakage terms), {\tt delays} and {\tt bandpass}.  Additionally for dual polarisation
systems, there are two quantities derived from the gains, namely {\tt
xygains} and {\tt xbyygain}.  {\tt xygains} gives the \MM{X} polarisation
gain divided by the \MM{Y} polarisation gain (or \MM{R} divided by \MM{L} for
circular feeds).  The phase of this quantity is the much reviled \MM{XY}
phase of the ATCA.  {xbyygain} is the product of the \MM{X} and \MM{Y}
polarisation gains -- it is of more limited use. 

Two other options are sometimes useful.  Option {\tt dots} causes the
plots to use small dots rather than larger blobs.  This can
reduce the size of plot files significantly and reduce the printing
time.  Option {\tt dtime} causes the time axis to be plotted in decimal
hours (rather than the normal hh:mm:ss).  This can be useful for
listings which are to be put into another package ({\it e.g.}  a
spreadsheet) for further analysis. 

\item The {yaxis} parameter controls whether the amplitude, phase, real
or imaginary part of the gain is plotted.  Several values chosen from
{\tt amp}, {\tt phase}, {\tt real} or {\tt imag} can be given.  When
several values are given, then several plots will result from a given
option for a given baseline. 

\item The {\tt select} parameter behaves like the normal visibility
selection parameter (see Section~\ref{s:select}) -- except that you are
selecting gains rather than visibilities.  Currently only a subset of the
normal {\tt select} commands are supported, and these are not entirely
consistently handled.  {\tt Antenna} and {\tt time}
selection is supported for gains. {\tt Time} selection is supported for
delay and spectral correction. {\tt Antenna} and {\tt frequency} selection
is supported for band passes.

\end{itemize}

\section{Deleting Calibration Tables -- DELHD}\index{calibration, deleting}
\index{delhd}
Occasionally (or perhaps not so occasionally) you might produce a bad
set of calibration tables that you wish to delete. This might
be particularly important for instances where a calibration program
takes an old calibration table as a initial estimate of a new one
yet to be derived.

As mentioned above, there can be three important calibration tables in
a visibility dataset -- the {\tt gains}, {\tt leakage} and {\tt bandpass}
items. Although there are subsidiary items associated with each, these are
the main items, and by deleting any one of these you effectively prevent the
calibration operations associated with that table being attempted.

The task to delete items is \task{delhd}. For example, to delete the
{\tt gains} item of the dataset {\tt multi.uv}, use

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf DELHD}} \\
\hline
in=multi.uv/gains & Delete the {\tt gains} item of\\
		    & {\tt multi.uv}\\
\hline 
\end{tabular}
\end{center}

\newchapter
\chapter{Calibration Strategies}
\label{c:polarimetry}
\section{Introduction}
The calibration process can be somewhat complicated, with several
possible paths. We will describe only the main paths here. It is
{\em strongly recommended} that you follow them. If you
stray off them, there is usually a way ahead, but it may not be simple.

We will describe the calibration of data either with two (\MM{XX}, \MM{YY}) or
all four (\MM{XX}, \MM{YY}, \MM{XY}, \MM{YX}) polarisation products. However
when there are only two products, a number of the steps involved (usually
involving \task{gpcal}) can be skipped.

This chapter discusses calibration with ATCA data loaded using \miriad. There
are differences if the data were initially loaded with \aips\ -- see
Appendix~\ref{c:oldcal} for more details. \miriad's software
supports calibration of a fairly general interferometric model, including
VLA and WSRT polarimetric and non-polarimetric data. Calibration of
these is
not described here, and is left as an exercise for the enthusiastic
reader.

Figure~\ref{fig:polcal1} gives
a flow chart of the steps typically involved.
\index{calibration, strategy}

\begin{figure}
\begin{center}
\epsfxsize=7cm\leavevmode\epsffile{polcal1.ps}
\end{center}
\caption{Flow chart to prepare for calibration in \miriad.}
\label{fig:polcal1}
\end{figure}

\section{Preparing your Data}

Before you calibrate, you must prepare your data. This consists of
loading, flagging, perhaps converting to ``channel-0'' datasets, and then
splitting.

\begin{enumerate}
\item 
ATCA data will be initially in RPFITS format. \miriad's task to read
RPFITS files is described in Chapter~\ref{c:inout}. Alternatively, the
data may be in FITS format (e.g. VLA data). Again see Chapter~\ref{c:inout}
for information on loading data in this format. Generally you will want
to load all of an observation into a single \miriad\ file (i.e. use
\task{atlod} to read multiple RPFITS files into a single output if necessary).

\item You should now flag your data, and possibly convert to channel-0
(using \task{uvaver}).
Tasks for these operations are described in Chapter~\ref{c:edit}.

\item
\index{uvsplit}
As \miriad\ datasets can contain only a single set of calibration
tables, it is rather poor at handling the calibration of datasets
containing multiple sources and multiple frequency bands. For
calibration purposes, it is best to work on datasets containing a
single source and single frequency band. So, it is best to
break the multi-source, multi-band dataset into a collection of single-source,
single-band datasets. The best task to do this is \task{uvsplit}.
Task \task{uvsplit} generates the names of the output datasets itself,
forming these from the source name and the central frequency (in MHz) of
the data.
It is rather unforgiving if you already have a files with one of the names
that it wants to use. Make sure your directory is free of files that may
usurp \task{uvsplit}'s name choice. Task \task{uvsplit} allows you to
perform extra selection if you wish, which may be convenient if you
only want to deal with part of your observation at a time.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVSPLIT}} \\
\hline
vis=multi.uvxy     & The input dataset.\\
select		   & Extra selection.\\
\hline 
\end{tabular}
\end{center}

For large spectral line data-sets, if disk space is low, it may be useful not
to split off the program source. Rather you can split off the calibrators,
determine the calibration tables from them, copy the calibration back to
the multi-source file, and then image directly from the multi-source file.
This way you avoid making a second copy of your program source data.
For example, to avoid the source ``{\tt vela}'' from being split off, use
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVSPLIT}} \\
\hline
vis=multi.uv		& The input dataset.\\
select=-source(vela)	& Do not select {\tt vela} data.\\
\hline 
\end{tabular}
\end{center}

\end{enumerate}

\section{Calibration}
You are now ready to start the main calibration step. You
should have a large number of \miriad\ datasets, each containing
a single frequency band and source. The calibration process will
determine bandpass functions (assuming you have not already averaged your
data into a channel-0 dataset) and antenna gains as a function of time.
If you have measured all four polarisation products, the process will also
determine
instrumental polarisation and the polarisation of the secondary calibrator.
Assuming you have observations of
the primary flux density calibrator, 1934-638, a secondary phase
calibrator, and 
your program source, there are two avenues open to you. The first,
sketched in Figure~\ref{fig:polcal2}, uses the secondary to determine
all the calibration quantities except the absolute flux density scale. The
primary is used to determine the absolute flux density scale, and is
generally used to double check the other quantities. While this avenue is
the preferred path, it requires good parallactic
angle coverage of the secondary to disentangle the instrumental polarisation
and the secondary calibrator polarisation. For typical secondary calibrator
flux densities, it also requires a modest amount of integration time
to determine the bandpass to good accuracy.

The second avenue is to rely on the observation of the primary
calibrator, 1934-638, to determine instrumental polarisation and bandpass
function. This is possible as 1934-638 is strong, and its polarisation
is known (it is less than 0.2\%\ polarised). This approach is shown
in Figure~\ref{fig:polcal3}. Note that this approach relies on knowing
the polarisation of 1934-638. This approach cannot be used if an
alternative primary calibrator is used.

\begin{figure}
\begin{center}
\epsfxsize=14cm\leavevmode\epsffile{polcal2.ps}
\end{center}
\caption{Flow chart for polarisation calibration in \miriad\ when
there is sufficient secondary calibrator data to determine all calibration
parameters from the secondary.}
\label{fig:polcal2}
\end{figure}

\begin{figure}
\begin{center}
\epsfxsize=14cm\leavevmode\epsffile{polcal3.ps}
\end{center}
\caption{Flow chart for polarisation calibration in \miriad\ when
there is insufficient secondary calibrator data to determine some
calibration parameters.}
\label{fig:polcal3}
\end{figure}

\begin{enumerate}
\item \label{step:mfcal}\index{mfcal}\index{calibration, bandpass}
We start by calibrating 1934-638. Assuming that you have not
averaged the data into a channel-0 dataset, we start with bandpass
calibration. The task to perform this is \task{mfcal}. After having
solved for the bandpass function, \task{mfcal} writes a bandpass
table in the visibility dataset -- the data themselves are not
modified. Although it
is possible for \task{mfcal} to cope with datasets which contain
multiple IF bands, and which frequency switch with time, we recommend
that you {\em do not} use this feature. It is easier to stick with
datasets with a single frequency band. 

As well as determining a bandpass function, \task{mfcal} determines
antenna gains. These gains are based upon the assumption that
the source is unpolarised (which happens to be true for 1934-638) and that
the instrumental polarisation is zero (which is not true). While these
assumptions do not affect the quality of the bandpass, and provided you
have measured all four polarisation correlations, it is best to follow the
run of \task{mfcal} with task \task{gpcal} to correctly determine
instrumental polarisation. The task \task{mfcal} is described more
completely in Section~\ref{s:mfcal}

If your data are to be combined (or compared) with pre-August 1994 ATCA data,
you will probably also use the {\tt oldflux} 
option (see Section~\ref{s:1934flux}).

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf MFCAL}} \\
\hline
vis=1934-638.4800       & Specify primary calibrator. \\
flux                    & Leave unset. \\
line			& Select good channels.\\
refant=3                & Specify the reference antenna. \\
interval=5              & Solution interval for gain solutions (minutes). \\
options			& Normally leave  blank, but\\
options=oldflux		& set to {\tt oldflux} to get the pre-August 1994 scale.\\
\hline 
\end{tabular}
\end{center}

\item \index{calibration, polarimetric}\index{gpcal}
Skip this step if you have measured only the \MM{XX} and \MM{YY} correlations.
We now proceed
to determine the antenna gains and instrumental
polarisation from the primary calibrator. Task \task{gpcal} is used
for this (see Section~\ref{s:gpcal} for more information). Task \task{gpcal}
will generally use the bandpass determined by \task{mfcal} to partially correct
the data. When calibrating 1934-638, the only options that you
will normally turn on is for the \MM{XY} phases to be solved for as a function
of time; \verb+options=xyvary+.
If your data are to be combined (or compared) with pre-August 1994 ATCA data,
you will probably also use the {\tt oldflux} 
option (see Section~\ref{s:1934flux}).
Typical parameters for calibrating 1934-638 are thus

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPCAL}} \\
\hline
vis=1934-638.4800       &Specify primary calibrator \\
flux                    &Leave unset \\
refant=3                &Specify the reference antenna \\
interval=5              &Solution interval for gain solutions (minutes) \\
options=xyvary		& Solve for \MM{XY} phase as a function of time, or\\
options=xyvary,oldflux	& add {\tt oldflux} to get pre-August 1994 flux scale.\\
\hline
\end{tabular}
\end{center}

Task \task{gpcal} will report the instrumental polarisation parameters
(leakages) -- two complex numbers per feed. Typically these are 1 to 2\%,
although they can be 4\%\ under bad conditions. Typically these are quite
constant with time, with similar values resulting from observations
several months apart. However they are modestly frequency dependent.

\item Calibration of 1934-638 is
now complete. You should examine the calibration solutions for
reasonableness.
Generally the bandpass function will have an amplitude
of 1, plus or minus 15\%, and phase of a few degrees. Provided you equalised
the gains at the start of your observation, your antenna gains
should have an amplitude close to 1, although the phases should reflect
atmospheric phase stability. If you see glitches in the antenna gains
in time, it
is probably due to bad data. Go back and look at the data near this
time, and perhaps do some more flagging -- see Section~\ref{s:uvflag}.

Tasks appropriate to examine the effectiveness of the calibration are
\task{uvplt} (to generate plots of the calibrated data -- remember that many
\miriad\ tasks apply any available calibration by default) and \task{gpplt}
(to generate plots of the calibration tables -- both antenna gains
and bandpass functions). A particularly useful plot is the ``scatter diagram''
plot generated by \task{uvplt} with the following inputs

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVPLT}} \\
\hline
vis=1934-638.4800          &Specify primary calibrator \\
stokes=i,q,u,v		   & Plot all Stokes parameters.\\
axis=real,imag		   & Plot real vs imaginary\\
options=equal,nobase	   & Equal X and Y axes. Plot all baselines\\
			   & together.\\
device=/xs		   & Normal PGPLOT device -- Xwindows for example\\
\hline
\end{tabular}
\end{center}

This scatter diagram should show four concentrations of points -- one for
\MM{I} at the flux density of the calibrator, and the others for \MM{Q}, \MM{U} and \MM{V}
probably
near zero. If there are outliers, you probably need to do some more flagging.
If you see arcs, the phase calibration is probably bad -- you might try
decreasing the solution interval.

\item 
We now come to calibrating the secondary calibrator, and there are
three possible approaches to follow:
\begin{itemize}
\item If you have multi-channel data for the secondary (i.e. not channel-0),
but you do not have sufficient observations on the secondary to determine a bandpass
from it, you must use the second approach (Figure~\ref{fig:polcal3} and 
step 6 below).
\item If you have measured all four polarisation correlations and if you
have poor parallactic angle coverage of the secondary,
you must use the second approach (Figure~\ref{fig:polcal3} and step 6).
\item Otherwise you have sufficient data to use your secondary to
determine the appropriate calibration parameters, and you can use the first
approach (Figure~\ref{fig:polcal2} and step 5).
\item There is a third, hybrid, approach, which determines the bandpass from
the primary and the polarisation characteristics from the secondary. Although
this approach is no more complicated than the other two, we leave it as
an exercise for the reader to determine the correct sequence.
\end{itemize}

Good parallactic angle coverage enables you to disentangle the instrumental
and secondary polarisations from each other. ``Good coverage'' generally means
more than a few cuts. For sources near declination of -30\degrees, the
parallactic angle remains constant through much of the observation, except near
transit where it goes through a rapid change, whereas for sources near the
poles, parallactic angle changes linearly with time.
You can plot your parallactic angle coverage using the
task \task{varplt}. More strictly, it plots the angle between the
sky and the feed (i.e. parallactic angle plus 45\degrees\ for the
ATCA).


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf VARPLT}} \\
\hline
vis=0823-500.4800          &Specify secondary calibrator \\
xaxis			   & Time is the default x axis\\
yaxis=chi		   &Plot parallactic angle + 45 degrees\\
device=/xs		   &PGPLOT device -- xwindows in this case\\
\hline 
\end{tabular}
\end{center}

\item%[\arabic{enumi}a.]
{\em If you have sufficient secondary data:}
If you have not already averaged
the data into a channel-0 dataset, you should determine the bandpass
calibration of the secondary. This is identical to the determining the bandpass
calibration for the primary, except that the flux density, and its
variation with frequency, will generally not be known for the secondary.
In this case \task{mfcal} assumes a spectral index of 0 and a flux density to
make the rms antenna gains equal to 1. Again, if the atmospheric phase stability
is good, set the solution interval to the scan length. Otherwise, provided
the source is moderately strong, set the 
solution interval to a small value. Typical input parameters are

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf MFCAL}} \\
\hline
vis=0823-500.4800          &Specify secondary calibrator \\
flux                       &Leave unset \\
refant=3                   &Specify the reference antenna \\
interval=5                 &Solution interval for gain solutions (minutes) \\
\hline 
\end{tabular}
\end{center}

\index{calibration, polarimetric}
Skip the remainder of this step if you have only measured \MM{XX} and \MM{YY}
correlations, as the 
gains determined by \task{mfcal} are as good as you can do. Otherwise
we now come to solving for instrumental polarisation and the polarisation
of the secondary itself. Given good parallactic angle coverage, these
can be disentangled. For a given correlation, the instrumental polarisation
terms stay constant with time, whereas
the polarisation due to the source vary as sinusoids of the parallactic angle.
Inputs to \task{gpcal} are similar to the case of the primary, with
the very significant exception that we must turn on the switch to solve for
the secondary's polarisation. The appropriate
options to do this are {\tt options=qusolve,xyvary}. As with \task{mfcal}, the
flux density of the secondary will generally not be known, so \task{gpcal}
determines a flux density so that the rms antenna gains are 1. Again
the {\tt interval} parameter should be set to the calibrator scan length
if the phase stability is good. If this is not so, then it should be set to
a smaller value (1 to 0.1 minutes) with the caveats as noted in the
Section~\ref{s:mfcal} above. If you do set it so a small value,
there is an additional step (and decision) to be performed before you can
interpolate the gain solutions onto your program source -- see
Section~\ref{s:gpaver}.

Typical inputs for this case are

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPCAL}} \\
\hline
vis=0823-500.4800          &Specify secondary calibrator \\
flux                       &Leave unset \\
refant=3                   &Specify the reference antenna \\
interval=5                 &Solution interval for gain solutions (minutes) \\
options=qusolve,xyvary	   & Solve for calibrator \MM{Q} and \MM{U}, and\\
			   & allow \MM{XY} phase to vary with time.\\
\hline 
\end{tabular}
\end{center}
In the unlikely chance that your calibrator turns out to be more than
about 5\%\ polarised, you may wish to run \task{gpcal} again, but this
time you should add options \verb+xyref+ and \verb+polref+. This will solve
for an \MM{XY} phase offset on the reference antenna, as well as an
instrumental polarisation characteristic that cannot be determined from
a weakly polarised source.

\item%[\arabic{enumi}b.]
 {\em If you have little secondary data:}
If you have poor parallactic angle coverage of your secondary
and/or there are insufficient observations of the
secondary to determine a bandpass solution, then it is best to
rely on the instrumental polarisation and bandpass calibration
determined from 1934-638. The
suggested approach, sketched in Figure~\ref{fig:polcal3}, is to copy the
instrumental polarisation parameters (if present) and bandpass from 1934-648
to your secondary, and then solve for the antenna gains. Additionally
if all four polarisation correlations were measure, the polarisation of
the secondary is also solved for.

The task to copy the instrumental polarisation and bandpass function is
\task{gpcopy}. The inputs are very simple for this situation -- you give it 
the names of the input and output datasets. For example

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPCOPY}} \\
\hline
vis=1934-638.4800          &Specify 1934-638 dataset.\\
out=0823-500.4800	   &Specify the output secondary calibrator \\
options			   & Leave unset.\\
mode			   & Leave unset.\\
\hline 
\end{tabular}
\end{center}

This will also copy the antenna gains determined for 1934-638 -- this is
unimportant as we will now overwrite them by rerunning \task{gpcal}. We
want to solve for antenna gains and the \MM{XY} phases as a function of time.
Additionally, if we have measured all four polarisation correlations, we want
to solve for the polarisation of the secondary.
We {\em do not} have enough data to solve for
instrumental and secondary polarisation simultaneously, so we turn off
the solver for instrumental polarisation.
The relevant options are
thus {\tt options=xyvary,qusolve,nopol} if all four polarisation correlations
were measured, and {\tt options=xyvary,nopol} if only \MM{XX} and \MM{YY} were
measured. Again the solution
interval should be either the scan length (for observations with good
phase stability) or set to a small value (if phase stability
is poor). Typical inputs are 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPCAL}} \\
\hline
vis=0823-500.4800          &Specify secondary calibrator \\
flux                       &Leave unset \\
refant=3                   &Specify the reference antenna \\
interval=5                 &Solution interval for gain solutions (minutes) \\
options=xyvary,qusolve,nopol & If you measured \MM{XX},\MM{YY},\MM{XY} and \MM{YX}, or\\
options=xyvary,nopol	   & if you have only \MM{XX} and \MM{YY} correlations.\\
\hline 
\end{tabular}
\end{center}

\item \index{calibration, examining} The calibration of the secondary is almost complete. Before we
complete it, though we should examine
the quality of the solutions. Four tasks are recommended for this --
\task{uvflux} (how well
does the secondary data fit a point source model?),
\task{uvplt} (plot the calibrated secondary data), \task{gpplt} (plot
the gain solutions) and \task{gpnorm} (compare secondary solutions with those
determined from the primary or elsewhere).

The first, and simplest, check of the calibration process is to use
\index{uvflux}
\task{uvflux} to check how well the calibrated data fits a point source.
Task \task{uvflux} assumes the data represents a point source, and determines
the source flux density and the rms scatter about this point. It also prints out
the theoretical scatter based on thermal noise arguments. It will do this for
any of the Stokes parameters. Typical inputs are

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVFLUX}} \\
\hline
vis=0823-500.4800          & Specify secondary calibrator \\
stokes=i,q,u,v		   & Check all four Stokes parameters.\\
options			   & Leave unset (apply calibration).\\
\hline 
\end{tabular}
\end{center}

The first numeric column given for each Stokes parameter is the theoretical
scatter. Note if you are using the 33 channel / 128 MHz system, and if
no channel averaging has been performed, or the {\tt birdie} options
has not been used with \task{atlod}, then the
theoretical scatter printed by \task{uvflux} is a factor of $\sqrt{2}$
higher than the true theoretical value. This is as, for this correlator
configuration (and not for others), individual channels are not independent --
they overlap by a factor of exactly 2 (in noise bandwidth). Task \task{uvflux}
fails to take account of this. The second and third numeric columns
are the mean visibility
amplitude and phase. The phase should be near 0 for \MM{I}, but could be either
0 or 180 degrees for \MM{Q} and \MM{U} (assuming there is signal in these!).
\MM{V} should be noise. The fourth
column gives the actual scatter. This should be close to the theoretical
value. Do not be concerned if it is a factor of 2 or so bigger. If it is
more than a factor of a few greater than the theoretical, you probably
still have bad data or a bad gain solution.

Using \task{uvplt} to plot the calibrated data is also recommended. As
with the primary, a scatter diagram plot is quite useful and quick.
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVPLT}} \\
\hline
vis=0823-500.4800          &Specify secondary calibrator. \\
stokes=i,q,u,v		   & Plot all Stokes parameters.\\
axis=real,imag		   & Plot real vs imaginary\\
options=equal,nobase	   & Equal X and Y axes. Plot all baselines\\
			   & together.\\
device=/xs		   & Normal PGPLOT device -- Xwindows for example\\
\hline
\end{tabular}
\end{center}

Outliers in the scatter diagram probably indicate bad data -- you might
want to go back and flag some more, and redo some calibration steps. Note,
however, that the ultimate objective of the calibration process is to get
good calibration solutions -- you are not attempting to produce perfect
primary or secondary calibrator data. If you do not believe that outliers
affect the calibration solutions unduly, ignore them.

Another check is to plot the gain solutions and inspect them for
consistency -- use \task{gpplt} for this (see Section~\ref{s:gpplt}).
Typical inputs for plotting the solved-for \MM{XY}
phases are given below. These \MM{XY} phases are the difference between the
actual \MM{XY} phases and the \MM{XY} phase correction applied by \task{atlod}.
These should be constant (within the noise) and no more than a few degrees.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPPLT}} \\
\hline
vis=0823.uv                &Specify secondary calibrator \\
device=/xs		   &PGPLOT device -- X windows in this case \\
yaxis=phase		   &Plot the phase of the ratio \\
options=xygains		   & of the \MM{X} and \MM{Y} gains \\
\hline 
\end{tabular}
\end{center}

In addition to plotting these solved-for \MM{XY} phases, it is probably worth
while plotting antenna and bandpass gains and phases. If you see glitches
in the solutions, check the data again. This might be due to bad data
or interference. You might do some more flagging, and redo some of the
calibration again.

A final check is to compare the instrumental polarisation solution
(the leakage parameters). You can compare these with another independently
derived set of these solutions. These solutions are moderately time
independent (they are moderately consistent over months), although there
is significant frequency variation. The independent set of solutions
may have come from the primary (e.g. if you determined instrumental polarisation
for the primary and secondary independently -- which was recommended if
you had sufficient parallactic angle coverage), or from a previous
configuration, or possibly other secondary calibrators from the same
observing run. The task to compare the different solutions is \task{gpnorm}.
Apart from just taking some sort of difference between two sets of parameters,
it adjusts certain parameters to minimise the difference. The parameters
that it adjusts are the absolute feed orientation and ellipticity -- two
quantities that are not solved for in the preceding calibration process
(unless you had a strongly polarised calibrator, and used {\tt options=polref}
in \task{gpcal}). Task \task{gpnorm} can also deduce an error in the
absolute \MM{XY} phase between the two observations (an error in the absolute
\MM{XY} phase leaves a signature in the instrumental polarisation parameters).

Task \task{gpnorm} reports three numbers -- an \MM{XY} phase offset, the
offset in absolute orientation and ellipticity, and a residual rms error.
As the preceding calibration process should have corrected for the \MM{XY}
phase offset, this should be no more than a few degrees. The offset in
absolute orientation and ellipticity should be no more than 2-3\%.
The rms residual error should be no more than 0.005. You would
expect the agreement to degrade with time difference in the observations
used to derive the parameters. Agreement should be very good for
parameters derived from observations on the same day, whereas agreement
should be less good for observations several months apart.

Having solved for an offset in absolute \MM{XY} phase, orientation and
ellipticity, \task{gpnorm} can ``correct'' the calibration tables
for these. This is generally not advisable.

Typical inputs to \task{gpnorm} are
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPNORM}} \\
\hline
vis=0823-500.4800          & The leakages to check. \\
cal=1934-638.4800          & The `good' set of leakages. \\
options		           & Leave unset.\\
\hline 
\end{tabular}
\end{center}

\item \index{calibration, flux bootstrapping}\index{gpboot}
Having produced a good set of calibration tables for the secondary, all
that remains is to correct for the flux density of the secondary calibrator.
The task to do this is \task{gpboot}.
Recall that the flux density of the secondary was determined by assuming that
the rms gain amplitude was 1. This assumption will be in error, although if
the gains were equalised before the observation (which is the normal practice),
the error will typically be small. Only a small adjustment needs to be made. 
Given that we know the flux density of the primary, the correction for the
secondary's flux density can be determined. Then the gains of the secondary
can be corrected to reflect the true flux density of the secondary.

In principle, you should use observations of the secondary and primary
calibrators that were taken at the same time and elevation. For obvious
reasons this is not possible. However, you can use the {\tt select} keyword
to select data from the secondary when it was as close as possible to the
primary's elevation when it was observed.

Task \task{gpboot} can optionally correct the \MM{XY} phases of the
secondary, assuming that the \MM{XY} phases of the primary are correct,
and that the \MM{XY} phases are constant.  Given \MM{XY} phase strategy
that you should have already followed, this is not recommended. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPBOOT}} \\
\hline
vis=0823-500.4800          & Specify secondary calibrator.\\
cal=1934-638.4800	   & Specify the primary calibrator.\\
select			   & Use this to select the time range of\\
			   & the secondary.\\
options=noxy               & Do not make XY phase corrections \\
\hline 
\end{tabular}
\end{center}

\item All that remains is to transfer the calibration tables to the program source.
Again we use \task{gpcopy} to go this. Typical inputs are

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPCOPY}} \\
\hline
vis=0823-500.4800          &Specify secondary calibrator \\
out=vela.4800		   & Specify the program source dataset\\
options			   & Leave unset\\
mode			   & Leave unset\\
\hline 
\end{tabular}
\end{center}

\end{enumerate}

\section{The Interpolation Process}\index{calibration, interpolation}
You should now have a program source dataset containing the
appropriate calibration solutions.
The antenna gain solutions have been derived
from observations of a secondary
calibrator, near the program source, taken typically for 5 minutes every
hour or so.  The program source antenna gains are derived
by interpolating and extrapolating these gain solutions.
Although these can often be skipped, there are a few steps
that you might consider doing to improve the interpolation process.

\subsection*{Interpolation/Extrapolation Tolerance}\index{calibration, interpolation tolerance}\label{s:inttol}
Clearly there is some limit to the interval of time over which you can
sensibly interpolate or extrapolate gains. If the gap between the secondary
observations is too large, then the interpolation of the gain solutions may
have no resemblance to the true gains at a given time.
The software that interpolates or extrapolates between the
gain solutions has a tolerance which limits
the gaps in time that it will permit.
If the time differences between a source observation and two
gain solutions bounding it are both less than the tolerance, then the
program source gain is the interpolation of the two gain solutions.
If there is only one gain solution within
the time tolerance, then this is used without any
interpolation. If there are no gains within the time tolerance, the
corresponding program source data are marked as flagged.

This interpolation tolerance is stored as the {\tt interval} item
the visibility dataset. It is given as a double precision number with
units of days. Task \task{gpcal} sets its value as half a day ({\tt interval=0.5}). Though this is excessively generous, this will not be a
problem unless there are large gaps between calibrator scans (perhaps because
some calibrator scans had to be discarded). You can set the set the 
interpolation tolerance using \task{puthd}. For example

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf PUTHD}} \\
\hline
in=vela.4800/interval & Set the interpolation tolerance of\\
		       & dataset vela.4800. \\
value=0.1	       & Set the tolerance to 2.4 hours (0.1 days). \\
\hline 
\end{tabular}
\end{center}


\subsection*{Averaging Antenna Gains with Time -- GPAVER}\label{s:gpaver}
\index{calibration, gain solution averaging}\index{gpaver}

If the phase stability is bad during the observation
(the atmospheric conditions are poor), the
phase of the antenna gains can vary rapidly. A change of tens of degrees
or more during the calibrator scan is not uncommon. However the software that
determines the antenna gains assumes that the gains are constant during
a solution interval. So during periods of poor phase stability, it is often
desirable to make the solution interval of the calibration software quite
short. While the resultant gains probably track the phase during the
calibrator scan, what we are really interested in are the antenna gains for the
program
source. If you solved for a number of time intervals during the
calibrator scan, the
best guess at the antenna gains for the program source is derived by
interpolating between some average of the calibrator scan gains.
Thus after determining the gains at a fine time step in the calibrator
scan, you should average these gains together to get some representative
gain for the whole calibrator scan.
This is probably the best guess you can make (at least as far as correcting
the program source is concerned), although in times of truly
awful phase stability, its a pretty poor guess (self-calibration will
be needed in this case).

There are two ways to average your gains, either  the ``vector'' or ``scalar''
averages. Which is the most appropriate will depend on whether you want
good estimates of the gains or good estimates of the resultant images.
\begin{itemize}
\item Scalar averaging consists of averaging the amplitude of the gains
separately (the `average' phase is still determined by a traditional
(vector) average of the real and imaginary parts of the gains).
Assuming the
variation in gain is purely due to poor phase stability, a scalar average
will give you a good estimate of the amplitude of the gain. 
If you are going to self-calibrate
later, then scalar averages are probably the most appropriate.
This means that
when you come to self-calibrate, you only have to solve for the phase
(at least initially), because you already have a good amplitude estimate.
\item On the other hand, if you are not going to self-calibrate, you are more
interested in getting a good estimate of your image at this stage, and less
concerned about partially correct gains (the two do not necessarily go hand
in hand). If we
were to use vector averaged antenna gains (averaging the real and imaginary
parts), the poor
phase stability will cause partial decorrelation in both the program
source and calibrator. This will result in
apparent reduced flux densities of both of them. Assuming
that the decorrelation is approximately the same for both, then we could scale
up the program source by the decorrelation that we note in the calibrator.
This can be achieved by vector averaging the antenna gains.
\end{itemize}
To summarise the above discussion, when phase stability is poor, it is
best to use a very short solution interval when solving for the antenna
gains of the calibrator scan. The gains during a scan should then be
averaged before applying them to the program source. Scalar averaging is
appropriate if self-calibration is to be used later. Otherwise vector
averaging should be used.

The task \task{gpaver} can be used to perform averaging of antenna
gains. Its pretty straightforward.Typical inputs are

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPAVER}} \\
\hline
vis=vela.4800 & Average the gain table for the program source \\
interval=10 & Averaging interval (calibrator scan length)\\
options=scalar& Scalar average if self-calibrating later,\\
\ \ \ or&\\
options=vector& vector average otherwise\\
\hline 
\end{tabular}
\end{center}


\subsection*{Preventing Gain Interpolation -- GPBREAK}
\index{calibration, glitches}\index{gpbreak}

Up until now we have assumed that the antenna gains vary smoothly
with time, and so it is appropriate to interpolate between them.
However this is not always the case.
If there is a major instrumental glitch between
calibrator scans, then it may be more appropriate to attribute all the
antenna gain difference to this glitch.
In this case, interpolating the
gain solutions will probably give poorer results than assuming constant
gains before and after the glitch. 

The task \task{gpbreak} can be used to insert a `break-point' into the
antenna gains table, which indicates to the software which applies the
calibration, that interpolation across this point should not be
performed.  Rather the gain immediately before and after the break-point
should be the gain from the previous or next calibrator observation. 

Inserting a break-point modifies the antenna gains table, and so it is only
meaningfully performed after the antenna gains table has been determined.
To insert a break-point, you will need to know the exact time of the glitch.
You may well know this from the observing log, or you may be able to deduce it
from the program source data if you are observing a strong source.

Task \task{gpbreak} takes as its parameters the name of the visibility
dataset ({\tt vis} parameter), the times at which to insert break-points
({\tt break} parameter), and the antennas and feeds for which to insert the
break-points (parameters {\tt ants} and {\tt feeds} -- the default
is to insert break-points for all antennas and feeds).  The {\tt break}
parameter gives the time in normal \miriad\ format -- either {\it
hh}{\tt :}{\it mm}{\tt :}{\it ss} or
{\it yymmmdd}{\tt :}{\it hh}{\tt:}{\it mm}{\tt :}{\it ss}. 
The second form (giving year,
month and day) will generally not be required for a single day's
observing.  Several times can be given, separated by commas.  For
example: 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPBREAK}} \\
\hline
vis=vela.4800 & Insert break points into the program source.\\
break=20:30:10,21:15:45& Give the times of the glitches.\\
ants=1,2	 & Only antennas 1 and 2 were affected.\\
feeds=x	 & Only the X feeds were affected.\\
\hline 
\end{tabular}
\end{center}

Unfortunately break-points do not survive recalibration. If you re-determine
the antenna gains ({\it e.g.} re-run \task{gpcal} or the like), then you will
overwrite the old antenna gains table, and will need to re-insert the
break-points.

\section{High precision polarimetric calibration}
The ATCA potentially can produce polarimetric data to very high precision, with
polarimetric measurements down to the level of 1 part in 100000 of Stokes I.
The extra steps to attain this are beyond the scope of this manual.
\htmladdnormallink{David Rayner's memo}{http://wwww.atnf.csiro.au/observers/memos/circpolguide.pdf}
``Circular polarization user's guide'' (2000) contains much useful information on
this topic.

\section{Combining Pre- and Post-August 1994 Data}\label{s:1934flux}
In August 1994, the nominal flux density of the ATCA's primary flux
calibrator, 1934-638, was revised to give better agreement between
the ATCA's flux density scale and the flux scales used in the northern
hemisphere (the VLA in particular). For more discussion, see John
Reynolds' memo \htmladdnormallink{``A revised flux scale for the AT
Compact Array''}{http://www.atnf.csiro.au/observers/memos/d96783\~{}1.pdf}
(AT Memo 39.3/040).

This introduces a problem when mixing data (either combining or
just comparing) calibrated before and after this change. By default
\miriad\ tasks use the new flux scale. However if you use the option
`oldflux' in \task{gpcal} and \task{mfcal} when calibrating 1934-638,
the old flux scale will be propagated to the calibrated data.
In this way, new data can be calibrated
to have the old flux scale, and so can be combined/compared
directly with data calibrated before the revision.

\newchapter
\chapter{Imaging}\label{c:imaging}\index{imaging}\index{invert}
\section{Introduction}
The task \task{invert} is used to produce an image (or image cube)
given a collection of visibility datasets.

Even if you are only interested in continuum imaging, your data may
contain several frequencies. Unless you have averaged over frequency
channels earlier, your data still contains the 33 frequency channels produced
by the ATCA correlator. Additionally as
the ATCA can observe two frequency windows simultaneously, and if these two
windows are relatively close (within about 25\%\ of each other), then you
can get extra sensitivity and improved \MM{u-v} coverage by combining the
two sets of data during imaging. Indeed you can enhance your \MM{u-v} coverage
further (but not sensitivity)
by time-switching the frequency setup used, and so you might have
observations at several frequencies, perhaps from different configurations.
%See Chapter~\ref{c:mfs} for a discussion of the best strategies for these
%sorts of experiments.

The net result is that you possibly have data observed at multiple frequencies,
and you are faced with the question of how to make an image from these
data. Generally, unless perhaps your observation consists of a single frequency
band at 3 cm, now is {\em not} the time to do any more averaging. It is
better to let the imaging task, \task{invert}, do its job with the
UN-averaged data. By giving \task{invert} data that has not been averaged
in frequency, it can grid the individual channels at their correct location
in the \MM{u-v} plane (rather than some average location). In this way bandwidth
smearing is reduced and the better
\MM{u-v} coverage results in a better beam. The advantage of doing this
decrease with
the fractional bandwidth that your data occupies, and so the reasons for
not averaging are less compelling when imaging using a single frequency band
at 3 cm.
In this case, the small disadvantages of not doing the frequency averaging
(somewhat slower run time, and a larger scratch file) might
outweigh the advantages.

The technique of forming a single continuum image from a variety of
frequencies is called ``multi-frequency synthesis'' (MFS). It produces an
image with a frequency corresponding to some average of the frequencies
of the input data. There is nothing particularly magical about combining
different frequencies to produce a single continuum image. This is done
all the time (interferometers are never monochromatic). The advantage of
MFS is that it can take a greater fractional spread in frequencies before
artifacts become a problem. 

Eventually MFS will break down because of the variation in the sources
flux density with frequency.
If the fractional spread in frequencies is small ($<10\%$) or if
high dynamic range imaging is not required (dynamic ranges less than a few
hundred or so), then spectral index effects are unimportant. You can
make an MFS image, and deconvolve it using the conventional deconvolution
tasks (if deconvolution is required). Otherwise, spectral index effects are
likely to be important, and you should use a
special task (\task{mfclean}) for the deconvolution. Note that the decision
to use MFS in the imaging {\em does not} require you to use \task{mfclean}
in the deconvolution. While many ATCA continuum observations will benefit
from using MFS in the imaging, a much more select set will benefit from
accounting for spectral index effects in the deconvolution.

\section{Spectral Line Imaging Considerations}
Although you do not have to decide whether to use MFS or not for
spectral line observations (clearly you cannot), there are a few other
issues you must consider. These are discussed in Chapter~\ref{c:lineobs}
in more depth -- we only list some pointers here. Some questions to ask
yourself are:
\begin{itemize}
\item Do I need to remove the continuum from the visibility data?
\item Is the velocity information in the dataset correct? What
velocity system convention am I interested in?
\item Do I need to account for the Doppler effect caused by the Earth's
motion?
\end{itemize}

\section{Weighting}
Each visibility sample is given a weight in the imaging step. This weighting
can be used to account for differences in the density of sampling in
different parts of the \MM{u-v} plane, or to account for different noise
variances in different samples, or to improve sensitivity to extended
objects, etc. Here we briefly review some weighting schemes:
\begin{description}
\item[Natural weighting] This gives constant weights to all
visibilities (or, more strictly,
inversely proportional to the noise variance of a visibility). This weighting
gives optimum point-source sensitivity in an image. However the synthesised
beam-shape and sidelobe levels are usually poor.
\item[Uniform weighting] This gives a weight inversely proportional to
the sampling density function. This form of weighting minimises the sidelobe
level. However the noise level can be a factor of 2 worse than natural
weighting.
\item[Super- and sub-uniform weighting] Uniform weighting computes the
sampling density function on a grid that is the same size as the
gridded \MM{u-v} plane. This results in the synthesised beam sidelobes
being minimised over the same field-of-view as the region being
image. Surprisingly, making the field-of-view very large (bigger than
the primary beam size) or very small (comparable to the synthesised beam)
both cause uniform weighting to reduce to natural weighting.
Super- and sub-uniform weighting decouple the weighting from the field size
being imaged. Instead, the sidelobes in the synthesised image are minimised
over some arbitrary field size, with this field being either smaller or 
larger than the field being imaged (for super- or sub-uniform weights
respectively). 
\item[Robust weighting] Uniform weighting (including super- and sub-uniform
weighting) minimising sidelobes, whereas natural weighting minimises
the noise level. Robust weighting provides a compromise between the two,
doing so in an optimal sense (similar to Wiener optimisation).
See \htmladdnormallink{Dan Briggs' thesis}
{http://www.aoc.nrao.edu/dissertations/dbriggs/}
for more information.
\item[Tapering] In signal processing theory, the optimum way to
detect a signal of known form, which is buried in noise, is to convolve that
signal with a ``matched filter''. This filter has an impulse response which
is just the reverse of the form of the signal that is being detected.
Applying
this principle to detecting sources in radio interferometry, the optimum
weighting for detecting
a Gaussian source is to weight the visibility data
by a Gaussian. This is often called `tapering'. Using a Gaussian weight
will significantly increase the detectability
of an extended source. However it also degrades the resolution. Gaussian
weighting can be combined with any of the above weighting schemes to
achieve some form of balance between sidelobes and sensitivity.
\end{description}

\miriad\ gives good (excessive?) control over the visibility
weighting schemes, via three parameters and one option.
\begin{itemize}
\item {\tt fwhm} controls tapering of the data. Unlike \aips, this taper
is specified in the image domain, in arcseconds.
If you are interested in
features of a particular angular size, then the signal-to-noise ratio in the
resultant dirty image is optimised for these features if the taper FWHM is
the same as the source FWHM (or source scale size).

\item {\tt sup} is used to control super- and sub-uniform weighting.
The {\tt sup} parameter indicates the region of the {\em dirty beam}
(centred on the beam centre) where sidelobes are to be suppressed or
minimised.  Like the \verb+fwhm+ parameter, the \verb+sup+ parameter is
given in arcseconds.  The weights that \task{invert} calculates are
optimal, or near optimal, in a least-squares sense to minimising
the sidelobes in the specified region of {\em the beam}.  Although the
sidelobe suppression region is not a direct control of resolution and
signal-to-noise ratio, it does have an effect on these
characteristics. 

Suppressing sidelobes over the entire field of the dirty beam
corresponds to uniform weighting -- that is, we get uniform weighting if
{\tt sup} is set to the field size of the dirty beam; this is the
default.  Alternately making no attempt to
suppress sidelobes ({\tt sup=0}) corresponds to natural weighting. 

Increasing \verb+sup+ from 0 to the field size results initially in an
improvement in resolution until the value of \verb+sup+ is approximately
equal to the best resolution.  Increasing \verb+sup+ beyond this results
in a slow degradation in resolution.  The noise level varies in
a less regular way with \verb+sup+.  Apart from saying that \verb+sup=0+
(natural weighting) gives the optimum signal-to-noise ratio, it is not
possible to generalise.  However the noise level will be no worse than
a factor of a few from the optimum. 

The default value for \verb+sup+ is the field size ({\it i.e.}  uniform
weighting).

\item {\tt robust}: In \miriad, robust weighting is parameterised
by a ``robustness'' parameter. Values less than about -2 correspond
essentially to minimising sidelobe levels only, whereas values greater than
about +2 just minimising noise. A value of about 0.5
gives nearly the same sensitivity as natural weighting, but with
a significantly better beam.

\item {\tt options=systemp}: The basic weight of a visibility (the weight used
for natural weighting, or the weight used for a point in determining the
local sampling density function) is ideally $1/\sigma^2$, where $\sigma^2$ is
the noise variance of a visibility. Because historically the value for this
in a \miriad\ data-set has been of dubious reliability, \task{invert}
normally assumes the
noise variance is inversely proportional to the integration time of a
visibility. However ATCA data
loaded with \miriad\ \task{atlod} will contain good estimates of $\sigma^2$:
using the true $\sigma^2$ will result in some improvement in sensitivity in
the final image. In this case, the
\verb+systemp+ option can instruct \task{invert} to compute the basic
weights using $\sigma^2$. 
\end{itemize}

\section{Computation}

Task \task{invert} is a fairly conventional imaging program, which
produces a dirty image from a visibility dataset.  It normally does this
using a grid-and-FFT approach, although there are switches to use a
direct Fourier transform and a median algorithm.  Task \task{invert}
does not require the data to be sorted in any way.
Normally any calibration tables are applied
by \task{invert} on-the-fly (although this can be turned off with the
\verb+nocal+, \verb+nopol+ and \verb+nopass+ options). Both
continuum or spectral line observations are handled.

We describe the inputs to \task{invert}. For MFS imaging, note 
{\tt options=mfs} (and {\tt options=sdb}) options.

\begin{itemize}

\item {\tt vis} gives the name of the input visibility datasets. 
Several datasets can be given, as may be convenient when a source is
observed with multiple configurations or multiple frequencies.
The selected data are assumed to
be of the same object, with the same pointing centre.  Additionally,
when making spectral line cubes, the number of channels derived from each
dataset must be the same (this restriction
does not apply for MFS images).  Dataset names can include wildcards. 

\item {\tt map} is the name of the output image dataset(s). When several Stokes
parameters are being imaged, you need to give one name for each Stokes
type. There is no default.

\item {\tt beam} is the name of the output beam dataset.  The default is not
to create an output beam.  If you wish to deconvolve, then you must
create an output beam. Note that \task{invert} produces a single beam
which corresponds to all image planes and Stokes parameters.

\item {\tt cell} gives the image pixel size in arcseconds.  Either one or two
values can be given.  If only one value is given, the pixels are square. The
default is to choose a pixel size which samples the synthesised beam by
about a factor of 3 (i.e. the recommended sampling for deconvolution).

\item {\tt imsize} gives the size of the images in pixels. It need not
be a power of two. Generally the beam is also this size, but see
{\tt options=double} below.

\item {\tt line} is the normal data linetype selection.  The default linetype
is the first channel when performing normal imaging and all channels
when doing multi-frequency synthesis. Generally you should set this
parameter if you have more than one channel.

\item {\tt select} is the normal visibility data selection.  The default is
to select all data. 

\item {\tt stokes} gives the Stokes or polarisation types to be imaged.
Several values can be given, separated by commas. For example
\begin{verbatim}
     stokes=i,q,u,v 
\end{verbatim} 

will cause images of Stokes \MM{I}, \MM{Q}, \MM{U} and \MM{V} to be formed.  Note
that there needs to be a corresponding output file (see \verb+map+
above) for each Stokes parameter being imaged.  The default Stokes parameter
to image is `ii', which images Stokes-I, and assumes that the source is
otherwise unpolarised.


\item {\tt offset}: This gives an offset, in arcseconds on the sky, to shift the image
centre away from phase centre. Positive values result in the
image centre being to the north and east of the observing centre. If one value
is given, both RA and DEC are shifted by this amount. If two values are given,
then they are the RA and DEC shifts. The default is no shift.

\item {\tt options} gives extra processing options.  Several options can
be given (abbreviated to uniqueness), separated by commas.  Possible
values are:

\begin{description}
\item[{\tt mfs:}] This invokes multi-frequency synthesis imaging, as
described above. All selected channels and frequencies will be gridded
simultaneously to form a single output image.

\item[{\tt sdb:}]
This is used in MFS imaging to cause \task{invert} to produce a spectral
dirty beam (this is stored as a second plane of the beam data-set).
The spectral dirty beam is used by the 
deconvolution task \task{mfclean} in determining the spectral index of the
image. Generally if you think you {\em may} use \task{mfclean} to deconvolve
your image, then you should set {\tt options=sdb}.
See the discussion of multi-frequency synthesis in Section~\ref{s:mfclean}
for more information.

\item[{\tt double:}] Normally \task{invert} makes a beam which is the same
size as the image. However \miriad's deconvolution tasks generally require
a beam which is four times the area than the region being deconvolved.
The {\tt double} option causes \task{invert} to produce a beam which is
twice the linear extent (four times the area) of the image. In this way,
you can request \task{invert} to map just the region containing real
emission, plus a guard band (at least 5 pixels on each edge).

\item[{\tt systemp:}] See the description in the weighting section above.

\item[{\tt nocal:}] By default, \task{invert} applies any gain
calibration to the data before imaging.  The {\tt nocal} option turns
off this step. 

\item[{\tt nopass:}] By default, \task{invert} applies any bandpass
calibration to the data before imaging.  The {\tt nopass} option turns
off this step. 

\item[{\tt nopol:}] By default, \task{invert} applies any polarisation
leakage correction to the data before imaging.  The {\tt nopol} option
turns off this step. 

\item[{\tt mosaic:}] This invokes \task{invert}'s mosaic mode. See
Chapter~\ref{c:mosaicing} for more information.

\item[{\tt imaginary:}] This makes the `imaginary' image. This is
useful for non-Hermitian data (holography) or for investigating certain
instrumental errors.

\item[{\tt amplitude:}] Set the phase of each visibility to zero before
imaging. 

\item[{\tt phase:}] Set the amplitude of each visibility to unity before imaging.
\end{description}

\item {\tt mode:} This determines the imaging algorithm to be used. Possible
values are {\tt fft} (a conventional grid and FFT imaging algorithm),
{\tt dft} (a direct Fourier transform approach) and {\tt median} (a median
Fourier transform). The {\tt dft} approach produces fewer artifacts in the resultant
images, but at substantial computational expense. The {\tt median} approach is
more robust to bad data and sidelobes, but at an even more substantial
computational cost. The default mode is {\tt fft} -- this should be used
on all but the smallest datasets and images.

\item {\tt slop:} The slop factor. This is used in spectral-line
imaging. See Section~\ref{s:slinvert} for more information.
\end{itemize}

Typical inputs to \task{invert} for a continuum experiment are given below.
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf INVERT}} \\
\hline
vis=vela.uv.1,vela.uv.2	&Name of input visibility datasets\\
map=vela.imap,vela.vmap	&Name of the output images -- one per Stokes\\
beam=vela.beam		&Name of the output beam\\
cell=1		&Pixel size is 1 arcsec square\\
imsize=256,512	&Image size is \TT{256}{512} pixels\\
stokes=i,v		&Image Stokes \MM{I} and \MM{V} \\
sup			&Leave unset to get uniform weighting,\\
\ \ \ or		&\\
sup=0			&Set \verb+sup+ to zero for natural weighting\\
fwhm			&Specify desired resolution -- unset gives the max\\
			&resolution\\
options=mfs		& Use the MFS option for multi-frequency synthesis, or\\
options=mfs,sdb		& use the sbd option as well if using \task{mfclean}.\\
line			&Controls channel selection and averaging. Default\\
			& is all channels when {\tt options=mfs}.\\
\hline 
\end{tabular}
\end{center}

For a spectral line observation, typical inputs would be
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf INVERT}} \\
\hline
vis=vela.uv.1,vela.uv.2	&Name of input visibility datasets\\
map=vela.imap			&Name of the output image \\
beam=vela.beam		&Name of the output beam\\
cell=1		&Pixel size is 1 arcsec square\\
imsize=256,512	&Image size is 256 by 512 pixels\\
stokes=i		&Image Stokes \MM{I} only \\
sup			&Leave unset to get uniform weighting,\\
\ \ \ or		&\\
sup=0			&Set \verb+sup+ to zero for natural weighting\\
fwhm			&Specify desired resolution -- unset gives the max\\
			&resolution\\
line=velocity,10,1.5,1.0,3.0	& Map channels to velocities. Image 10 \\
			& velocities centred at 1.5, 4.5, 7.5 etc \kms \\
\hline 
\end{tabular}
\end{center}

In its computation \task{invert} determines the theoretical rms noise.
This noise is calculated assuming that the only source of
error is the system temperature of the front-end receiver.  No account
is made of calibration errors, sidelobes or any other `instrumental'
effects.  The calculation correctly accounts for the weighting scheme
used in the imaging process. This theoretical noise is the level you
can expect in a detection experiment (assuming no interference or
confusion), and it is the best one can hope for in high dynamic range
work (usually instrumental effects will limit you before the noise
in these sorts of experiments).

The noise calculation of \task{invert} (and all other \miriad\ tasks that
compute the variance of a correlation) is based on values of system
temperature, system gain, integration time and bandwidth stored in a dataset.
Unfortunately data loaded into \miriad\ using \task{fits} will have only nominal
system temperatures and system gains, and an educated guess is made for the
integration time. Data loaded using \miriad\ \task{atlod}, the
system temperatures are those measured on-line, and the integration time
will be correct.
See Chapter~\ref{c:inout} for a discussion of setting you dataset
up so that noise estimates are correct. The system gain, however, is still
a nominal figure. If system temperature, system gain or integration time
are incorrect by some factor, then the
theoretical rms noise will also be wrong by the same factor. 

In continuum mode (i.e.  the 33 channel/128 MHz mode), when all 33
channels are retained after \task{atlod} (i.e.  \verb+options=birdie+
was not used) there is another effect which will cause \task{invert}'s
noise estimate to be a factor of $\sqrt2$ too pessimistic (i.e.  the
actual noise is a factor of $\sqrt2$ lower than the printed value). 
With the ATCA correlator in this mode mode, the channel bandwidth is
twice as large as the separation between channels ({\it i.e.} the
channels are oversampled).  Unfortunately \task{invert} assumes that the
bandwidth is the same as the separation.  This will only affect you if
you are imaging individual correlator channels (i.e.  no frequency
averaging) and if you keep all channels.  It will not affect ``channel 0''
or multi-frequency synthesis imaging. 


\newchapter
\chapter{Image Deconvolution}\label{c:decon}
\index{deconvolution}\index{clean}\index{maxen}
\section{Introduction}
Because synthesis arrays sample the \MM{u-v} plane at discrete locations, there is
incomplete knowledge about the Fourier transform of the source intensity
distribution.   The measured visibility data can be thought of as the true
distribution, \MM{V(u,v)}, in the \MM{u-v} plane multiplied by some sampling function,
\MM{S(u,v)}.    The convolution theorem states that the Fourier transform of
the sampled distribution (the dirty image, $I_D$) is equal to the convolution of
the Fourier transform of the true source visibility distribution (the true image,
\MM{I}) and the Fourier transform of the sampling function (the dirty beam,
$B_0$):

\[
I_D(\ell,m) = I(\ell,m) \ast B_0(\ell,m) \rightleftharpoons V(u,v) \times S(u,v)
\]

where $\ast$ indicates convolution, and $\rightleftharpoons$ indicates
the Fourier transform.  Deconvolution algorithms attempt to account for
the UN-sampled regions of the \MM{u-v} plane.  If it was fully sampled, there
would be no sidelobes, since the sampling function would be a constant,
and the Fourier transform of a constant is a delta function; a perfect
beam.  Thus, deconvolution tries to remove the sidelobes of the dirty
beam that are present in the image.  It is important to realize that in
doing so, the algorithm is guessing at what the visibilities are in the
UN-sampled part of the \MM{u-v} plane.  The solution to the convolution
equation is not unique, and the problem of image reconstruction is
reduced to that of choosing a plausible image from the set of possible
solutions. 

You should be extremely cautious when deconvolving images formed from a
small number of snapshots.  In these cases, there will be large areas of
the \MM{u-v} plane that are unsampled because of the poor instantaneous \MM{u-v}
coverage of the ATCA.  If the source is complicated, the deconvolution
algorithm may go badly wrong in its guess of what the source really
looks like in the gaps.  The best way to make a decent image of an
object is to observe it, not to allow a deconvolution algorithm to guess
what it looks like. 

If you are using multi-frequency synthesis (MFS) in a situation where spectral index
effects are
significant (i.e. when the fractional spread in frequencies is $>10\%$ and/or
images with dynamic ranges better than a few hundred are required), then
the simple convolution relationship no longer applies -- see Section~\ref{s:mfclean}.

There are two techniques used commonly in radio astronomy; CLEAN and
maximum entropy (MEM).  CLEAN is rarely used outside of radio astronomy,
but MEM is more far reaching.  For detailed discussion on the `pros'
and `cons' of these algorithms, see the NRAO imaging workshops and
references therein.  Much blood has been spilt over their relative
merits in the last decade or so. 

It is probably fair to say that in general, CLEAN is easier to drive than MEM,
although using MEM can result in reduced processing times for large problems.
All dirty images produced by \task{invert} (continuum, line, MFS, any Stokes
parameters)
can be deconvolved with either \task{clean} (CLEAN) or \task{maxen} (MEM).
However to deconvolve an MFS image where spectral index effects are important,
a special version of CLEAN is used, \task{mfclean}.

After the deconvolution is finished, you have produced a the model of the
source. CLEAN produces a CLEAN component image (a collection of delta
functions), whereas MEM produces some more smooth model. The output images
of both CLEAN and MEM are in units of Jy/pixel, and are super-resolved
(i.e. they contain spatial frequencies beyond those measured).
Looking at the CLEAN component image is a sobering
experience.  While the MEM output is more visually appealing, it will generally
contain a positive bias. To improve the qualitative appearance of the models
produced by the deconvolution tasks, and to
suppress what is essentially unmeasured high spatial frequency structure,
it is common (essentially universal with CLEAN) to follow the 
deconvolution by a `restore' step.  This step involves convolving the model
sky with a Gaussian.  This convolved image is then added to
the residual image.  The Gaussian is chosen to match the main lobe of
the dirty beam, and it is generally called the CLEAN beam (regardless of
whether CLEAN or MEM was used) or the restoring beam.

\section{Deconvolution with CLEAN}
\label{s:clean}
\index{deconvolution, H\"ogbom CLEAN}

The CLEAN algorithm \htmladdnormallink{(H\"ogbom 1974)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1974A\%26AS...15..417H}
(A\&AS 15, 417)
represents the image as a number of point
sources in an otherwise empty field of view.  A simple iterative procedure is
used to find the locations and strengths of these point sources.

The H\"ogbom CLEAN algorithm looks for the brightest (in an absolute sense) pixel
(called a CLEAN component) in a specified region in the image (called the CLEAN
window which must include all the real emission in the image).
For Stokes \MM{I} images (but not \MM{Q}, \MM{U} or \MM{V}), ideally
only positive components would be encountered. However noise,
errors in previous CLEAN iterations and calibration errors will
mean that eventually negative
components will also be found. Often it is quite valid to CLEAN these negative
components. Having found this peak pixel, it subtracts a
fraction (called the loop gain, and generally about \MM{0.1} or less) of the dirty
beam from the dirty image at the location of that brightest pixel. This
subtracted image is called the residual image. The search and subtraction loop
is repeated until the sidelobes in the image are reduced to below the noise
level.   It is easy to see how CLEAN works if you just think about a single
point source.  For extended sources, one thinks of the emission as a collection
of point sources.
Except for point sources, the flux density units of the dirty image are not very
useful.  However, convolved CLEAN components do have meaningful units of
Jy per CLEAN beam area, which can be converted to Jy per unit area,
because the equivalent area of the CLEAN beam can be calculated. 

\subsection*{The Clark CLEAN}\index{deconvolution, Clark CLEAN}
The Clark CLEAN algorithm \htmladdnormallink{(Clark 1980)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1980A\%26A....89..377C}
(A\&A 26, 89)
is a variant of the H\"ogbom CLEAN,
and aims at improved speed for large images. The original H\"ogbom algorithm
spends a lot of time searching residuals which are negligible, and 
shifting and scaling the dirty beam. The Clark algorithm avoids these by
searching a list of only the largest residuals, working with only a sub-region
of the beam for much of the time, and by using FFTs to perform convolutions
when it has to resort to using the full beam. This leads to an algorithm which
is more efficient for large images (although a good deal more complicated).
For small images though, the H\"ogbom CLEAN may
still be faster.   

The Clark CLEAN plays a few tricks which you should know about.  It has
what are termed major and minor cycles.  In the minor cycle, CLEAN
components are searched for, and subtracted from the image, in a fashion
very similar to that of the H\"ogbom CLEAN.  The difference is that only
the central portion of the dirty beam is used for the subtraction, and a
list of the largest residuals are searched (not the full region being CLEANed).
The rationale is that for dirty beams that have a fairly good sidelobe
pattern, this will be good enough to find the CLEAN components.  At some
designated time, this minor cycle is terminated and a major cycle
computed.  This means that the list of CLEAN components from the current
cycle are `FFTed', and subtracted from the FFT of the residual image
that resulted from the {\em previous} major cycle.  In this way, errors
that might have accumulated in the subtraction phase of the minor cycle
with only a part of the beam are largely set to rights. It is also at this
stage that the list of the largest residuals is re-determined.

This use of a small beam patch in the Clark CLEAN is a potential danger
if the beam is poor.   Images made from ATCA snapshot data, even if there are
a few cuts, often have a beam in which the first sidelobe response is
quite strong and outside of the beam patch that the Clark CLEAN typically
uses.

\subsection*{The SDI CLEAN} \index{deconvolution, SDI CLEAN}

CLEAN sometimes runs into an instability when working on extended
sources of fairly uniform surface brightness.  It produces obviously
wrong `CLEAN stripes' in your image.  The Fourier transform of these
stripes tends to be found in UN-sampled parts of the \MM{u-v} plane.  This is
a complicated way of saying that the visibility data do not constrain
CLEAN's interpolation between the measured visibilities sufficiently
well to prevent it putting in badly wrong values. 

Another variant of the CLEAN algorithm which helps suppress CLEAN stripes,
is the SDI CLEAN \htmladdnormallink{(Steer, Dewdney, and Ito 1984)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1984A\%26A...137..159S}
(A\&A 137, 159).
In this variant
any point in the residual image greater than some factor times the
maximum residual point is taken as a CLEAN component.  The factor is less than
one.  Thus, when the residual image has become very smooth, this avoids
introducing ripples into it (which occur when subtracting the beam from just
one location) which lead to the stripes.  This algorithm can be quite
successful.

\subsection*{Region-of-Interest Considerations}
Note that the H\"ogbom, Clark, and SDI versions of CLEAN can only
properly deconvolve a region a quarter the area of your beam.
If your
the image and beam are the same size, which is the default behaviour of
\task{invert}, you can deconvolve only a quarter of
the image.  For example, consider images that
are 256 pixels in size (one dimension will do), with a point source
located at pixel 192.  A beam image centred on that point source will
extend only to pixel 64, so that the full image cannot be CLEANed. 

However there is a switch in \task{invert}, {\tt options=double}, to make
the beam
four times the area (twice the linear extent) of your image. In this
way, you can CLEAN to the edge of your image (you should make the
image at least 5 pixels larger in each direction than the real emission).

In practise, you can sometimes violate the above rule that the beam must be
four times the area of the CLEAN region, especially
if the field is very sparse (e.g. a central source, and some simple
confusing sources at a modest distance away). 
Generally the H\"ogbom CLEAN will be more forgiving if you attempt to use a
CLEAN region larger than a quarter the area of the beam. However 
there is a switch 
({\tt options=pad}) in \task{clean} to improve the stability for
Clark and SDI CLEANs as well.

Apart from restricting the area CLEANed to a quarter of the area of the
beam, CLEAN works much better if the region being CLEANed snugly
encompasses the real emission in the image.  At first, you may not know
where this is.  However, after some quick and dirty (sic) CLEANing, you
could redo it properly with a better idea of the CLEAN region. 
Essentially, you are giving CLEAN {\it a priori} information about the
source, which better constrains the deconvolution problem. 

\subsection*{The Cotton-Schwab CLEAN}\index{deconvolution, Cotton-Schwab CLEAN}

Yet another CLEAN variant, similar to a Clark CLEAN, is the MX algorithm
(Schwab 1984).  During the major cycle, the Fourier transformed CLEAN
components are subtracted from the {\it ungridded} residual visibility
data, instead of the residual gridded visibility data ({\it i.e.} the FFT of
the residual image).  This means that the subtraction is more accurate.
\miriad\ does not have a task which implements the MX
algorithm. 

\subsection*{CLEAN Stripes}\index{deconvolution, CLEAN stripes}

CLEAN stripes are, in part, introduced by the approximations made during the
minor cycles of a Clark CLEAN -- they are less common when using a H\"ogbom
algorithm. Thus striping problems can be alleviated by taming the minor
cycle approximations. There are two simple ways to tame the
approximations used in the minor cycle of a Clark CLEAN -- use a larger beam
patch, and relax the criteria to perform a major cycle ({\it i.e.} perform fewer
minor cycles per major cycle).

\htmladdnormallink{Cornwell (1983)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1983A\%26A...121..281C}
(A\&A 121, 281)
suggested that the CLEAN stripe
instability can be suppressed by the addition of a delta function to the
dirty beam.  For obvious reasons, this is popularly termed the `Prussian
helmet' CLEAN.  The spike is usually set at about \MM{0.3}.  Together with
this, one usually makes the loop gain smaller, to perhaps \MM{0.05} or even
\MM{0.01}.  However, recent wisdom suggests that in general, the Prussian
helmet is probably a red herring. 

\subsection*{Computation}

\miriad's CLEAN task -- surprisingly called \task{clean} -- implements
the H\"ogbom, Clark and SDI algorithms.  Given an input dirty image and
beam, it produces an output CLEAN component image which has flux density units
of Jy/pixel.  This CLEAN component image is CLEAN's best guess at what
the source really looks like.  Invariably its extrapolation at high
spatial frequencies is very poor (and probably a reason why \aips\ does
not make it easy to view it).  To reduce the undesirable effects of this
extrapolation, and to add in any emission remaining in the residuals, you
will need to use the task \task{restor}.  This gives you what is
normally thought of as the `CLEAN' or restored image. 

The various input parameters to \task{clean} are:

\begin{itemize}
\item {\tt map, beam}: These keywords give the names of the input image and beam
datasets -- as generally produced by \task{invert}.

\item {\tt out}: This gives the name of the output CLEAN component image.

\item {\tt mode}: \miriad\ \task{clean} can perform either H\"ogbom, Clark or SDI 
CLEANs. The \verb+mode+ parameter can be set to indicate the particular
algorithm to use. Possible values are `hogbom', `clark', `steer' or
`any' (the default). With `any', \task{clean} determines what it believes is
the best algorithm for your particular image. In this case, \task{clean} can
switch between different algorithms, as the nature of the residuals change.
This is particularly useful when CLEANing a large image which contains some
strong point sources and much lower brightness extended emission. In this case,
\task{clean} may well switch from a Clark or H\"ogbom algorithm to the SDI
algorithm when it finds that the residuals are becoming very smooth.

Generally you can allow \task{clean} to choose the algorithm. Task \task{clean}
will override your choice if you attempt to use the H\"ogbom
algorithm on an image that is too big.

\item {\tt region}: As noted above, it is important in \task{clean}
to limit the CLEAN region to an area one quarter that of the beam, and
to make it fit snugly around the real emission. The
\verb+region+ parameter can be used to describe quite complex CLEANing
regions.  See Section~\ref{s:region} on how to specify this.
Alternatively, you can use task \task{cgcurs} to describe
the region interactively from a display of an image on a PGPLOT device. 
Task \task{cgcurs} (see Chapter~\ref{s:pgplot}) can produce a text file,
\verb+cgcurs.region+, describing the region selected, which you can then
input to \task{clean} (see Section~\ref{s:atfile}).  For example:

\begin{verbatim}
     region=@cgcurs.region
\end{verbatim}
The default CLEAN region is the largest region (centred on the
dirty image centre) that can be safely deconvolved.

\item {\tt gain}: A good number for the loop gain is \MM{0.1} (the default).  If
CLEAN stripes occur, then try lowering the loop gain by a factor of
about 10. 

\item {\tt cutoff, niters, options=negstop}: You can tell \task{clean} when to stop in
three ways.  You can tell it to quit once the brightest pixel (in an
absolute sense) in the residual image reaches some cut-off.  This is
typically some multiple of the rms noise level (3--\ssigma\ or so). 
Specify this level with the parameter \verb+cutoff+. Alternatively, you
can tell \task{clean} to stop when it encounters the first
negative component, by using the {\tt options=negstop} switch. Otherwise,
CLEANing will proceed until {\tt niters} CLEAN iterations have been
performed.  When CLEANing a cube, \verb+niters+ is the number of
iterations per plane.  For small and simple sources, a few hundred
iterations are usually sufficient.  For complicated and large sources,
you can CLEAN forever. 

You can set all three of {\tt cutoff, options=negstop} and {\tt niters}.
Task \task{clean} will stop when any one of these stopping criteria
are satisfied.

\item {\tt phat}: This gives Cornwell's Prussian helmet parameter, which
is the value of
the spike to add to the central lobe of the beam. Using this parameter may
give some success at eliminating CLEAN stripes. When using a Prussian helmet,
typical values are 0.3 or so. Note that the loop gain should also be reduced
when using Prussian helmets.

\item {\tt model}: If you CLEAN an image, and find that you need to CLEAN some more,
\task{clean} can be restarted from where you left off.  Do this by 
setting the \verb+model+ parameter to the name of the old output CLEAN component
image, and setting the \verb+out+ parameter to a new name. Actually the
\verb+model+ image need not have been produced by \task{clean} -- it can be
any image with units of Jy/pixel. Of course, it should be a representation of
your source.

\item {\tt speed}: The `speedup factor' is a knob for the Clark algorithm which
controls how often major 
cycles are performed.   The more major cycles, the closer a Clark CLEAN
is to a H\"ogbom CLEAN. For point sources, this can be about 1.  For
extended sources, or if you are having trouble with CLEAN stripes, set
\verb+speed+ to some small negative number -- typically -1 is good.
The default is 0.

\item {\tt minpatch}: For Clark CLEANs, the parameter \verb+minpatch+ is the
minimum full width ({\em not} half-width, as it is in \aips) of the beam patch
used in the minor cycle. The default is 51. However, for ATCA work, where
the beam can often have large distant sidelobes, and so this value may be too
small.
Setting \verb+minpatch+ to larger values will slow the
algorithm, but may avoid CLEAN striping problems. The maximum value that
\task{clean} will accept is 257 -- larger values will be trimmed back to
257.

\item {\tt clip}: For SDI CLEANs, this parameter gives the `clip level'. 
During an SDI iteration, all pixels greater than \verb+clip+ times the
peak flux density are considered to represent true structure, and so are taken
as components for that iteration.  Typically the clip level is 0.9.  The
default that \task{clean} computes is image dependent, and will be a
function of how many pixels there are across the beam.  The default
value is usually adequate.

\item {\tt options}, as usual, gives several processing options. Possible
values are
\begin{description}
\item[{\tt negstop}] Stop CLEANing when the first negative component is
encountered.
\item[{\tt positive}] Apply a positivity constraint. This constrains the
component image to be non-negative. A side-effect of this is that CLEAN will
stop iterating if it cannot continue to ensure this. This does not have
any effect with SDI iterations. This option does not seem to produce
as much improvement as one might hope.
\item[{\tt asym}] Normally \task{clean} assumes that the beam has a 180
degree rotational symmetry, which is the norm in radio interferometry. 
Making such an assumption allows some optimisations. You should instruct
\task{clean} if this is not the case, by using the {\tt asym} switch.
\item[{\tt pad}] Double the beam size by padding it with zeros. This will
give better stability with Clark and Steer modes if you are daring enough
to CLEAN an area which is more than a quarter of the beam area.
\end{description}

\end{itemize}

Typical inputs are given below:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf CLEAN}} \\
\hline 
map=vela.imap		& Dirty image\\
beam=vela.ibem		& Dirty beam\\
out=vela.icmp		& Output CLEAN component image\\
mode			& Algorithm used -- let CLEAN decide\\
region			& Defaults to max area safely CLEANed.\\
gain=0.1		& Loop gain \\
phat			& Unset means no Prussian helmet \\
cutoff=0              & Terminate CLEAN at this residual level or \\
niters=500		& Specify total number of CLEAN components \\
speed			& Speedup factor; -1 for extended\\
			& sources, +1 for point sources \\
minpatch=127		& Minimum beam size for minor cycles \\
clip			& SDI clip level\\
\hline 
\end{tabular}
\end{center}


The total CLEANed flux density ({\it i.e.} the cumulative sum of the CLEAN
components) should eventually settle down to a roughly constant number.
This indicates that you
are just picking up noise, and that there are no sidelobes left to
remove.  If the total CLEANed flux density starts to decrease again, this
usually indicates that you have been a bit heavy handed, and CLEANed too
much.  You might also look at the result and see if you can see any
sidelobes left over.

Having completed \task{clean}, you will almost certainly want to ``restore''
your image -- see Section~\ref{s:restor}.

\section{Deconvolution with maximum entropy algorithms} 
\label{s:mem}\index{maxen}\index{deconvolution, maximum entropy}\index{maximum entropy}

This discussion was lifted from Tim Cornwell's article in the NRAO imaging
workshop (1988).

CLEAN approaches the deconvolution problem by using a {\it procedure} which
selects a plausible image from the set of feasible images. This makes a noise
analysis of CLEAN very difficult.  The Maximum Entropy Method (MEM) is not
procedural.   The image selected is that which fits the data, to within the
noise level, and also has maximum entropy. This has {\it nothing} to do with
physical entropy, it is just something that when maximised, produces a positive
image with a compressed range of pixel values.   The latter aspect forces the
MEM image to be smooth, and the positivity forces super-resolution on bright,
isolated objects.

One general-purpose definition of entropy (championed by Gull and Skilling) is 
\[
H = - \sum_k I_k \log \left (I_k \over {M_k}\right) 
\]
where $I_k$ is the brightness of the \MM{k}th pixel, and $M_k$ is some
default image.  An example might be a low-resolution image of the object.
This allows {\it a priori} information to be incorporated into the problem.
An alternate form, suggested by Cornwell (sometimes call the `maximum
emptiness' criterion) is
\[
H = - \sum_k \log( \cosh({I_k \over M_k})).
\]
This second form does not enforce positivity, and so can be used for
Stokes parameters other than \MM{I}.

The requirement that each visibility be fitted exactly by the model
usually invalidates the positivity constraint.  Therefore, data are incorporated
with the constraint that the fit, $\chi^2$, of the predicted visibility
to that observed, be close to the expected value:

\[
\chi^2 = \sum_r {|V(u_r,v_r) - \hat V(u_r,v_r)|^2 \over 
                {\sigma_{V(u_r,v_r)}^2}}.
\]

Maximising \MM{H} subject to the constraint that $\chi^2$ be equal to
its expected value leads to an image which fits the long spacings too well,
and the zero and short spacings poorly. To remedy this, an added constraint
can be added to the problem. Typically this is a flux constraint, which
ensures that the flux density in the maximum entropy image is correct. That is,
the flux density of the maximum entropy image,
\[
F = \sum_k I_k,
\]
is constrained to equal the known flux density for the source. This is quite a
useful constraint. Although the maximum entropy task, described below, does
not force you to give a flux constraint, you should attempt to, if at all
possible.


\subsection*{Computation}
Maximum entropy algorithms tend to be less robust and harder to drive than
CLEAN algorithms. The quality of the maximum entropy solution can depend
very strongly on the \verb+rms+ and \verb+flux+ parameters -- these parameters
should be set with some care, or some experimentation may be necessary.

Various parameters to the task \task{maxen} are described below:

\begin{itemize}

\item {\tt map, beam}: As with CLEAN, you give task \task{maxen} a dirty
image and a dirty beam.

\item {\tt default}: You also have the option of supplying a default
image.  The default image ($M_k$ in the equations above) is some
estimate of what the algorithm should tend towards.  It could be some
{\it a priori} model or low-resolution image of the source.  The default
default (sic) image is a uniform image.  The units of the \verb+default+
image are not too important, though it should be positive valued if the
Gull and Skilling entropy measure is used.

\item {\tt model}: The \verb+model+ is some initial estimate of the deconvolved
image (units of Jy/pixel). In principle,
the \verb+model+ image is simply a way to kick start \task{maxen} towards the
correct solution. In principle it should not affect convergence to the final
solution or its quality -- it should just speed up the process. In practice,
a good initial \verb+model+ can sometimes help.

\item {\tt region}: As with CLEAN, this gives the region in the dirty image
which contains all the source emission, and much the same can be restated
here about setting the parameter. In particular, you can
deconvolve an area no more than a quarter the area of the beam (but see
{\tt options=pad} if you fail to heed this advice).

\item {\tt measure}: This determines the entropy measure used; specify either
{\tt gull} or {\tt cornwell} (these  are the $p\log(p)$ and $\log(\cosh[p])$ forms,
respectively). The default is to use the Gull form, but if deconvolving
\MM{Q}, \MM{U} or \MM{V} images, the Cornwell form may be useful.

\item {\tt out}: This gives the name of the output image, which has units
of Jy/pixel. This is the equivalent image to \task{clean}'s CLEAN component
image. However, unlike CLEAN, maximum entropy techniques tend to be more
conservative in their extrapolation of high spatial frequencies, and so
this output is more commonly viewed and used as a final image than
the CLEAN component image would be. However it is just as valid an
operation to pass this output from \task{maxen} through \task{restor},
and so produce either a restored or a residual image. Those with some
courage may choose to look at the residual image. Unlike CLEAN, the
residuals tend to be strongly correlated with source structure.

\item {\tt niters}: Task \task{maxen} uses an iterative algorithm to
arrive at its solution, and terminates when it believes it has
converged.  The parameter \verb+niters+ gives the maximum number of
iterations that \task{maxen} will attempt before giving up if it does
not converge.  For low dynamic-range images, 10 iterations are usually
sufficient.  Higher dynamic-ranges (greater than 1000) can require 30
iterations to converge.  The default maximum number of iterations is 20. 

\item {\tt rms}: This is a crucial parameter.  It gives the rms noise in
Jy/beam.  The rms noise value printed by \task{invert} should be some
guide to setting this parameter, but see the caveats about this value
under \task{invert}.  An alternative way is to measure the rms in a
blank portion of the sky.  If the beam has few sidelobes you will
probably be able to measure this directly from the dirty image. 
Otherwise you would really need to CLEAN first! If \verb+rms+ is too
large, the output image will be too smooth.  If it is too small,
convergence will be prevented.  A useful trick is to underestimate
\verb+rms+ and then stop after a few iterations, and then reset
\verb+rms+ to the level achieved up to that point.  Do not leave
\verb+rms+ unset. 

\item {\tt flux}: The \verb+flux+ specifies how the zero-spacing flux density
is to be estimated.  There are three modes of use.  First, you specify a
known value which must be fitted to within 5\%.  Second, if you have no
idea what the zero-spacing flux density is, then leave {\bf flux} unset;
\task{maxen} will attempt to estimate it.  Third, if you have a rough
idea (within a factor of 2, say) then set \verb+flux+ to the negative of
your guess.  If at all possible, you should give some estimate of the
total flux density.

\item {\tt q}: This gives an estimate of the volume of the main lobe of the
dirty
beam in units of pixels. Typically it is 10 to 30, and the algorithm does
not depend critically on it. The default is determined from the data, and
generally will be adequate.

\item {\tt options}, as usual, gives several processing options. Possible
values are
\begin{description}
\item[{\tt quiet, verbose:}] This controls the verbosity of \task{maxen}'s
messages.
\item[{\tt asym:}] Normally \task{clean} assumes that the beam has a 180
degree rotational symmetry, which is the norm in radio interferometry. 
Making such an assumption allows some optimisations. You should instruct
\task{clean} if this is not the case, by using the {\tt asym} switch.
\item[{\tt pad:}] Double the beam size by padding it with zeros. This will
give better stability with Clark and Steer modes if you are daring enough
to CLEAN an area which is more than a quarter of the beam area.
\end{description}

\end{itemize}

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf MAXEN}} \\
\hline 
map=vela.imap		& Dirty image \\
beam=vela.ibem		& Dirty beam \\
model			& An initial solution solution -- generally unset\\
default			& The image that the solution should tend\\
			& towards -- generally unset \\
out=vela.ivm		& Output dataset\\
niters=20		& Maximum number of iterations\\
region=		& Region to be deconvolved\\
measure			& Leaving unset gives you the Gull measure\\
flux=			& Total source flux -- give some value\\
rms=			& Rms noise in the dirty image -- {\em IMPORTANT}!\\
q=			& AN estimate of the value of the beams main lobe.\\
\hline 
\end{tabular}
\end{center}

\section{Multi-frequency deconvolution -- MFCLEAN}\index{multi-frequency synthesis, deconvolution}
\index{deconvolution, multi-frequency synthesis}\index{mfclean}\label{s:mfclean}
If you have used multi-frequency synthesis in the imaging stage, you will
have to have considered a fly in the ointment -- the
flux density of the source will vary with frequency (as well as spatially).
For detection and low dynamic-range work, mild spectral
variation can generally be ignored.  For high dynamic range imaging, the
variation with frequency will cause a distortion in the resulting image. 

The \miriad\ deconvolution task \task{mfclean} can be used to
avoid this distortion.  Rather than just determining a flux density estimate,
\task{mfclean} simultaneously solves for both a flux density and spectral
variation for each source. To understand how this is possible,
consider a noiseless observation of a single point source at the phase centre.
Assume that it has a linear spectral variation. While this is obviously
an approximation, it is an adequate one for dynamic ranges up to several
thousand to one (assuming typical spectral variation and fractional spread in
frequencies of no more than 30\%). The visibility function
for this source can be described by two parameters: the source brightness at
some reference frequency, and the brightness derivative. 
\[
	V(\nu) = I(\nu_0) + \frac{\partial I}{\partial\nu}\Delta\nu
\]
If we temporarily assume that the derivative is zero, then the image resulting
from a multi-frequency synthesis observation of this source will just be the
normal point-spread function (beam pattern) scaled by $I(\nu_0)$. If we now
assume some non-zero
derivative, we will get a different image. Because
of the linearity of the system, the output image will be a sum of the two
terms. The first term is just the normal point spread function 
scaled by $I(\nu_0)$ as before, while the second term is caused by the
derivative.
The importance of the second term will be related to the size of the derivative
and the range of frequencies sampled.
This second term has been called the ``spectral dirty beam'' -- it is
a point-spread function resulting from a linear frequency variation. While it
may seem that the most logical definition of the spectral dirty beam is the
additional response resulting from a source with a flux density derivative
of 1, there is a more convenient scaling. If we assume that the flux density
variation is due to a spectral index, $\alpha$, where
\[
	I(\nu) = I(\nu_0)(\frac{\nu}{\nu_0})^\alpha
\]
then
\[
	I(\nu_0)\alpha = \nu_0\left.
\begin{array}{c}\displaystyle\frac{\partial I}{\partial\nu}\end{array}\right|_{\nu_0}
\]
Conventionally the spectral dirty beam is scaled so that
\begin{eqnarray*}
I_D(\ell,m) &=& I(\nu_0) B_0(\ell,m) + \nu_0
\left.\begin{array}{c}\displaystyle\frac{\partial I}{\partial\nu}\end{array}
\right|_{\nu_0}B_1(\ell,m)\\
	  &=& I(\nu_0) B_0(\ell,m) + (I(\nu_0)\alpha)B_1(\ell,m)
\end{eqnarray*}

For a general source, a sum of the two beams is replaced by a convolution
relation:
\[
I_D = I \ast B_0 + (I\alpha) \ast B_1
\]
(in the above equation, \MM{I} is evaluated at $\nu_0$).
The problem of multi-frequency deconvolution is to estimate 
\MM{I} and $I\alpha$ images simultaneously. A more detailed discussion of the
algorithm \task{mfclean} uses is
given in \htmladdnormallink{Sault \& Wieringa (1994)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1994A\%26AS..108..585S}
(A\&AS 108, 585).

But how important are the effects that spectral variation causes?
Generally you will want to ignore them if you can, because then
the deconvolution procedure has to solve only the 
standard convolution equation,
\[
I_D\approx I \ast B_0,
\]
and the other deconvolution tasks -- \task{clean} and \task{maxen} -- can
be used.

Surprisingly spectral effects are not very important in a wide variety
of problems. For fractional bandwidths of 10\%, spectral effects
need to be considered for dynamic ranges of better than 1000.  For
fractional bandwidths of 30\%, the effects become important for dynamic
ranges of a few hundred.  This assumes typical spectral variations
equivalent to spectral indices of order 1.  When imaging beyond the
half-power points of the primary beam, the frequency dependence of the
primary beam will be quite substantial, and so spectral variation will
need to be considered for lower dynamic range images. 

Generally you will need to consider at the imaging step (task \task{invert})
whether you {\em may} need to account for spectral variation in the
deconvolution step. If you {\em may}, then you need to form the spectral
dirty beam, $B_1$, as well as the normal dirty beam, $B_0$.
If so, you must have set the {\tt sdb} option in
\task{invert}. This produces the spectral dirty beam as a second
plane of the beam dataset. If
you did produce this spectral dirty beam plane, and then use
\task{clean} and \task{maxen}, they will issue warnings (which can be
safely ignored) about the existence of the second plane.

The inputs to \task{mfclean} are fairly similar to \task{clean}. Task
\task{mfclean} differs in that it does not have a Prussian helmet
or SDI CLEAN capability. Task \task{mfclean} does not support a number
of the {\tt options} given by CLEAN. More fundamental, though, is that,
whereas \task{clean}
can only CLEAN an area a quarter of the beam area, \task{mfclean} can strictly
speaking CLEAN an area only one ninth of the beam area. To explain the
reason for this, we must consider some of the details of \task{mfclean}. To
aid it in finding peaks in the flux density and spectral index domains, it
correlates the residuals with the beam and the spectral dirty beam. More
strictly it correlates the residuals with a portion of the beam and spectral
dirty beam -- the sub-beam. Strictly the sub-beam size should be the same
as the region being CLEANed. However in practise, provided it contains most
of the main sidelobes, a smaller portion is adequate. Task \task{mfclean}
will tell you the sub-beam size, and indeed may complain about it
being small. However provided it is either at least as big as the region
being CLEANed, or is 50-100 pixels, you are probably OK.
All the same, however, you should attempt to produce
images with larger guard bands if \task{mfclean} is to be used.

As with \task{clean}, you will almost certainly want to restore your
resultant image -- see Section~\ref{s:restor}.
The output from \task{mfclean} is a CLEAN component image with a difference --
it contains two planes. The first plane is the normal CLEAN component image
(like the one determined by \task{clean}), while the second plane contains the
estimate of $\alpha(\ell,m)I(\ell,m)$.
Both \task{restor} (Section~\ref{s:restor}) and a number of other tasks
use this second plane.

\section{Other deconvolution tasks}
The following two tasks are primarily written for multiple-pointing (mosaiced)
observations (see Chapter \ref{c:mosaicing}). However they also work equally well for a conventional single
pointing dataset (in \miriad, a single pointing observation is treated as
a degenerate mosaic). They both provide functionality not available
with the other single-pointing deconvolution tasks.
\begin{itemize}
\item \task{mosmem}: This is a maximum entropy deconvolution
task which can handle a joint
deconvolution with an interferometric and single dish observation. A joint
deconvolution of a single dish and single pointing interferometric observation
is not common: usually if the source is large enough to warrant a single
dish observation, then it is likely that the interferometric observation
would be a mosaic.
\item \task{pmosmem}: This is another maximum entropy deconvolution task.
It differs in that it can perform a joint deconvolution of the four
Stokes parameters simultaneously. With polarised emission being potentially
positive and negative valued, the entropy measure for this is different
from a simple total intensity deconvolution. See
\htmladdnormallink{Sault et al. (1999)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1999A\%26AS..139..387S}
(A\&A, 139, 387) for more background on joint polarimetric deconvolution.

Often joint polarimetric deconvolution does not do a better job than
doing the different polarisations individually (e.g.  by using
\task{clean}, or by using \task{maxen} and the maximum emptiness entropy
measure).
\end{itemize}

\section{Restoring an image}\label{s:restor}\index{deconvolution, restoring}
Having produced a model of the sky with either \task{clean}, \task{maxen}
or \task{mfclean}, you will probably want to use \task{restor} to
convolve this with a Gaussian CLEAN beam (restoring
beam) and add the remaining residuals. For models produced
by \task{mfclean}, \task{restor} convolves only the intensity (\MM{I}) plane
with the CLEAN beam, although it does use the $I\alpha$ plane when
determining the residuals.

Having the restoration as a separate step does have
some flexibility advantage, but the user then has to run another task
to produce the final image.  Task \task{restor}
optionally fits the dirty beam with a Gaussian CLEAN beam, and then
either produces a restored image or, alternatively, a residual image.  There
are a few other possibilities, but these are less common.  The inputs to
\task{restor} are:

\begin{itemize}

\item {\tt model}: This is normally the output CLEAN component image, but can be
any image that is a good representation of the source. It should have units
of Jy/pixel.

\item {\tt map, beam}: These are the input dirty image and beam.

\item {\tt mode}: This parameter determines the operation to be performed by
\task{restor}. The most common operations are to produce a restored
image (\verb+mode=clean+) or a residual image (\verb+mode=residual+).

\item {\tt out}: The output restored or residual image.

\item {\tt fwhm, pa}: Parameters of the CLEAN beam; \verb+fwhm+ gives the
size, in arcseconds, of the Gaussian beam to be used in the restoration. 
This will normally be two numbers, giving the full-width at half maximum
of the major and minor axes of the Gaussian.  If only one value is
given, then the Gaussian is assumed to be circularly symmetric. 
Parameter \verb+pa+ gives the position angle, in degrees, of the
restoring beam, measured east from north.  If no values are given for
\verb+fwhm+, \task{restor} first checks the beam dataset to see if it
contains Gaussian fit parameters.  If not, \task{restor} performs a fit
to the beam data.  Note that the fitting procedure will probably give
different values to the \aips\ \task{MX} and \task{APCLN} tasks. 
Generally the value computed by \task{restor} is to be preferred to the
\task{APCLN} and \task{MX} values. 

\end{itemize}

Typical inputs are given below:
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf RESTOR}} \\
\hline 
map=vela.imap		& Dirty image\\
beam=vela.ibem		& Dirty beam\\
model=vela.icmp		& Component image produced by CLEAN\\
out=vela.icln		& Output restored image (or residuals)\\
mode			& Leave unset to get restored image\\
fwhm			& Beam size -- Leave unset to let \task{restor}\\
			& fit it\\
pa			& Leave unset to let \task{restor} fit it\\
\hline 
\end{tabular}
\end{center}


\newchapter
\chapter{Self-Calibration}\label{c:selfcal}\index{self-calibration}

Because the basic interpolated calibrator gains do not
determine the antenna gains perfectly at each time stamp, the quality of the
resultant image suffers.  Often the technique of self-calibration is
used to make additional corrections to the antenna gains as a function of time. 
It is notionally very similar to the basic calibration. The main
difference is than the model of the source is generally more complex
than just
assuming its a point source at the phase centre.
The self-calibration technique finds antenna gains, $g_i$, which minimise
the difference between
the measured visibilities, $V_{ij}$, and the model visibilities,
$\hat{V}_{ij}$, viz
\[
  \epsilon^2 = \sum |V_{ij} - g_ig_j^{\ast}\hat{V}_{ij}|^2.
\]
Like basic calibration, these gains are normally described as `antenna gains'.
This, however, is somewhat of a misnomer when a telescope measures dual
polarisations. In this case, it would be better to describe the gains as
`feed gains', as the gain factors will generally differ between the two
antenna feeds. The above equation should include subscripts to indicate the
polarisation type of the correlation ({\it e.g.} $V_{XX}$) and the gain
({\it e.g.} $g_{X}$).

The model visibilities are generally derived from a model image (which should
have units of Jy/pixel). For a complex source, the model image will usually
be determined by the deconvolution tasks (the outputs of \task{clean},
\task{mfclean} or \task{maxen} {\em without} the restore step).
If the source is a point source, then a
point-source model could
be used, although the restriction that it be located at the phase centre
is lifted.

The self-calibration procedure is often performed iteratively, each time
with a better model, until finally the sequence converges and no
more improvement in the image quality can be made.

Self-calibration is not a technique that should be applied blindly.
This is especially true for ATCA data because, compared with the VLA, the
problem is only slightly over-determined.  This is because
the ATCA has, at most, 15 baselines to determine the complex gains for 6
antennas, whereas the VLA has, at most, 351 baselines to determine the
complex gains for 27 antennas. Because we can set the phase of one antenna
to zero, the problem reduces to finding
11 real numbers from 30 real equations.  With the 5-antenna compact
array, we must find 9 real numbers from 20 real equations.  This problem
is exacerbated when one antenna is absent from the data for a period
of time, or you have flagged it out because of poor data quality.

There are other problems to do with the east-west nature of the ATCA  -- see
Bob Sault's technical memo \htmladdnormallink{`Some simulations of 
self-calibration for the AT'}
{http://www.atnf.csiro.au/observers/memos/d96f87~{}1.pdf} (AT Memo 39.3/058).
One important point to keep in mind is that
self-calibration with the ATCA  depends  crucially on the initial
model that you start with -- much more so than with the VLA where you can
start with quite a poor model but arrive at the correct result after
just a few iterations.

Self-calibration, like basic calibration, requires that the
signal-to-noise ratio on each baseline be of the order of at least 5 or
so.  For weak sources, this may require a long solution interval, within
which the gains are assumed to be constant. 
If, in reality, the gains are changing on a time-scale significantly
shorter than your solution interval, and these changes are degrading
your image quality, then you will be unable to improve the image
quality.  The time-scale on which you will often find it necessary to
correct the gains is approximately one minute.  Thus, weak sources often
cannot be self-calibrated. For a continuum observation (100 MHz bandwidth)
with the ATCA, you will, typically, need a signal strength of 100 mJy
before self-calibration is possible.

Note that it is not the receivers that cause the gains to be unsteady
with time, but the atmosphere.  In a similar way to the degradation of
an optical image by the atmosphere, a radio image is defocused by the
phase (and amplitude) noise that atmospheric cells induce into the
wavefront. Self-calibration can be thought of as an off-line mimicry of
adaptive optics in optical astronomy. 

\section{Computation}\index{selfcal}\index{gpscal}

\miriad\ contains two self-calibration tasks -- \task{selfcal} and
\task{gpscal}. After some general comments, we describe the way in which
these tasks handle polarisation and multi-channel data. Finally,
the general inputs to the tasks are described.

Figure~\ref{fig:selfcal} gives a typical self-calibration flow chart.

\begin{figure}
\begin{center}
\epsfxsize=14cm\leavevmode\epsffile{selfcal.ps}
\end{center}
\caption{Typical self-calibration path}
\label{fig:selfcal}
\end{figure}

\subsection*{Apply Your Original Calibration Before Self-Calibrating}
Both \task{selfcal} and \task{gpscal} produce antenna gain tables in the same
format as the
calibration tasks previously described. Consequently, after running
these tasks, the gains they derive are applied to the data `on-the-fly'.
However, the new gain tables derived by the self-calibration
tasks will overwrite any old gain tables. Provided the self-calibration
produces an improvement, this is fine. However, if the self-calibration process
degrades the quality of the image (possibly because you gave it bad inputs
or a poor model,
or because you were attempting to calibrate a weak source), then you may have
lost your good set of gains. An additional shortcoming is that the 
self-calibration tasks do not apply bandpass corrections -- they
assume bandpass corrections have been previously applied. Finally,
if the primary gains have appreciable \MM{XY} phase (for any antenna
for \task{selfcal}, but for the reference antenna only for \task{gpscal}),
or significant amplitude gain mismatches (for \task{selfcal} only)
then these corrections should be applied to the data before self-calibration.
Consequently, {\em
it is usually wise, before starting to perform self-calibration
iterations, to form a calibrated dataset (apply bandpass
corrections and primary antenna gains).} Additionally if you are going to
use \task{selfcal}, as discussed below, you should convert to Stokes
parameters. The task used to apply calibration and convert to Stokes
parameters is \task{uvaver}.

Thus when using \task{selfcal}, use inputs to \task{uvaver} such as;
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVAVER}} \\
\hline
vis=vela.uv		& Raw visibility dataset, with primary\\
			& calibration tables.\\
out=vela.uv.cal		& Output with primary calibration applied.\\
stokes=i,q,u,v		& Perform Stokes conversion.\\
\hline 
\end{tabular}
\end{center}

If using \task{gpscal}, do not perform Stokes conversion.
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVAVER}} \\
\hline
vis=vela.uv		& Raw visibility dataset, with primary\\
			& calibration tables.\\
out=vela.uv.cal		& Output with primary calibration applied.\\
stokes			& Unset for no Stokes conversion.\\
\hline 
\end{tabular}
\end{center}

Once you have started self-calibrating, however, you probably
do not need to form a calibrated dataset after each iteration. Provided you
keep your best model of the image, if you do overwrite good gains with bad ones,
then they are simple enough to regenerate using self-calibration and the best
model.


\subsection*{Self-Calibrating with Multiple Models}
Both \task{selfcal} and \task{gpscal} can be given multiple input model
images. The input models can be cubes for multi-channel data. These different
models represent some different aspect of the data. For example, the data
could contain multiple pointing centres from a mosaiced observation or multiple
spectral lines. The self-calibration
tasks assume that the gains are not dependent on these different aspects of
the data ({\it e.g.} the gains are independent of frequency and pointing
centre). All the information will be used in a simultaneous solution of the
gains.
The self-calibration tasks will generally determine the appropriate correspondence
between data and model ({\it e.g.} it will associate the data from
a particular pointing with a model with the same pointing).
There are, however, some caveats described below.

\subsection*{Polarisation Handling}
The biggest difference between \task{selfcal} and \task{gpscal} is in the
handling of polarisation
and dual feeds.

Though \task{selfcal} can handle multiple pointings and frequency ranges,
it assumes that the antenna gain is independent of feed (i.e. the \MM{X}
and \MM{Y} gains are the same). It also only handles Stokes-I models
(and ignores visibilities that are unrelated to determining Stokes-I).

Task \task{gpscal} is intended for telescopes with dual feeds (either linear
or circular), such as the ATCA. Indeed, it will fail for other sorts of data.
\begin{itemize}
\item \task{gpscal} solves for the two feeds either totally independently, or
with the constraint that the \MM{XY} (or \MM{RL}) phase of an antenna must
be constant with time.
\item \task{gpscal} converts between Stokes parameters and raw polarisations.
It can take multiple input models for different Stokes parameters. However,
if a model of a particular Stokes parameter is not given, it is assumed to
be zero. For example, if you give it models of I, Q and U, but not V, then
the source is assumed to have no circular polarisation.
\item \task{gpscal} uses the antenna polarisation leakage table
if this information is present. If it is not present, leakages are assumed
to be zero.
\item Unlike \task{selfcal}, \task{gpscal} cannot take models from
multiple pointings or from different frequency ranges. All
models must correspond to the same pointing and same frequency range.
\end{itemize}

\subsection*{Frequency Handling}\index{self-calibration, frequency handling}

Both \task{selfcal} and \task{gpscal} assume that the gains are independent
of frequency. This allows them to use data from all channels in forming
gain solutions. However, as mentioned above, they do not apply bandpass
corrections. Consequently, if you have multi-channel data and models, you will 
probably want to form a bandpass-corrected visibility dataset before
doing self-calibration.

The assumption that the gains are independent of frequency should be
treated with some caution. If all the visibilities for a given baseline
go through one signal path, then the instrumental phase for the different
visibilities should be equal (assuming the signal path is well equalised).
However, atmospheric phase should vary linearly with frequency. Provided the
fractional bandwidth is small, it is still a reasonable approximation
that the gains are independent of frequency. Indeed this is the normal
approximation used in deriving calibration solutions from a `channel 0'
dataset.

If the visibilities for a given baseline go through multiple signal paths
({\it e.g.} the 2 IF system for the ATCA), then the instrumental phases for the
different paths could be significantly different. Tentative
results for the 2 IF system of the ATCA show that, apart from an offset,
the phase of the two signal paths track each other as the ratio of the
frequencies ({\it i.e.} the path length difference is constant). The offset
between them can be eliminated either by adjusting the phases
of the two paths to zero at the start of the observation, or by the primary
calibration process. Provided the offset has been eliminated, and provided
the frequencies are not too different, it
may be useful to approximate the gains as independent of frequency.

If the phases vary significantly between the frequencies, then the different
frequencies need to be self-calibrated individually. Ideally there would be a
self-calibration task which knows that the phase errors at different
frequencies tracked as the ratio of the frequencies.

When self-calibrating multi-channel visibility datasets, there are two
distinct modes of calculating the variation of the model with
frequency, depending on whether the model contains multiple planes,
or whether multi-frequency techniques were used. We discuss
these two possibilities in turn.

\subsubsection*{Multi-Plane Models}

When the model contains multiple planes, it should be noted that the
self-calibration
tasks {\em do not} perform any interpolation of models along the spectral
dimension.
The self-calibration tasks assume that there is a one-to-one correspondence
between a model plane and some average of the channels. The
correspondence is determined in a rather imperfect fashion -- the 
self-calibration tasks use the
standard {\tt line} parameter (see Section~\ref{s:line}).
This parameter determines the channels selected from the visibility
dataset, and the averaging performed on them. Unfortunately, the
self-calibration tasks only perform rudimentary checks on whether the
{\tt line} parameter makes sense ({\it i.e.} whether the selected channels
have the same frequencies as the model planes). Despite this, in
many instances, the self-calibration tasks will provide appropriate defaults
for the {\tt line} parameter.
Below we outline those instances where you can or should
allow the {\tt line} parameter to default, and those instances where you
should not.

\begin{itemize}

\item \miriad\ tasks normally maintain a record of the {\tt line}
parameter used to form an image.  The imaging task, \task{invert},
records the {\tt line} parameter it uses, and the deconvolution tasks
propagate it to their outputs.  The default {\tt line} parameter used by
the self-calibration tasks is that which was used to form the model. 
This is generally quite appropriate if the model was formed from the
data being self-calibrated.  However if the model was formed by some
other means, then the default will be quite inappropriate, and you
should set the {\tt line} parameter explicitly. 

\item \task{selfcal} can cope with the case of several models being
derived from different channel ranges from one visibility dataset. 
This might occur, for example, if multiple lines were being observed
simultaneously (as often happens at millimeter wavelengths, but rarely
with the ATCA).  In this case, the different models were formed with
different {\tt line} parameters.  \task{selfcal} {\em must} rely on the
record of the {\tt line} parameter stored in the model. 

\item If a point-source model is used ({\it i.e.} no input model dataset),
then you should give the {\tt line} parameter explicitly.

\item Similarly, if using the \verb+mfs+ option, you should also
set the {\tt line} parameter explicitly -- see the next subsection.

\end{itemize}

\subsubsection*{Multi-Frequency Models}

When using multi-frequency techniques for continuum work ({\it i.e.}
specifying {\tt options=mfs} to \task{invert}), the model will
generally consist of a single plane (if derived from \task{clean} or
\task{maxen}) or an intensity plane and flux scaled derivative plane
(\task{mfclean}). In these cases, the self-calibration tasks can
generate the model for all needed frequencies. However, you
{\em must} tell the self-calibration tasks that the model is a
multi-frequency one by specifying {\tt options=mfs}.

In the case of the models produced by \task{clean} and
\task{maxen} for multi-frequency images, as these deconvolution tasks
do not determine the derivative plane, the self-calibration tasks assume
that the spectral index is 0.

The {\tt line} parameter can still be used when performing multi-frequency
self-calibration. In this case, it selects out those channels in the
visibility dataset that are appropriate to the model. Indeed, the default
{\tt line} parameter can be rather inadequate, and you should give the
parameter explicitly.

\subsection*{Input Parameters to the Self-Calibration Tasks}

We now describe the input parameters to the self-calibration tasks. Given
the similarity of the two tasks, we will describe them together, noting
any differences as we go.

\begin{itemize}
\item {\tt vis}: This gives the name of the input dataset to be
self-calibrated.
\item {\tt model}: This gives the names of the input models. Several
models can be given. Task \task{selfcal} can take models from different
pointings and frequency ranges. Task \task{gpscal} can take
models from different polarisations only.

\item {\tt interval}: This gives the self-calibration solution interval time,
in minutes. This time must be long enough to integrate an appreciable
signal-to-noise ratio (5 or more) on visibilities. On the other hand, it
should be short enough so that the gains can be approximated as being constant
during this interval. The default is 5 minutes.

\item {\tt select}: This selects the \MM{u-v} data to be used in the self-calibration
process. For \task{selfcal}, you need not perform {\tt source}, {\tt dra}, or
{\tt ddec} selection if you use the {\tt selradec} option (see below).
\item {\tt minants}: The minimum number of antenna that must be present
before the tasks will attempt to find a solution. The default is 3 for
phase self-calibration, or 4 for amplitude self-calibration.
\item {\tt refant}: The reference antenna ({\it i.e.} the antenna which has
zero phase) to be used in the solution process. If this is not specified,
\task{selfcal} chooses a default for each solution. \task{gpscal}, however,
uses antenna 3 as the default reference antenna.
\item {\tt line}: The standard {\tt line} parameter, as described in 
Section~\ref{s:line}. This parameter is described in some detail above.
\item {\tt clip}: This parameter allows you to set the limit of the faintest
believable flux density in the model. For Stokes I or parallel-hand correlations, this
is a maximum limit -- flux densities below this limit are assumed to be
noise or error.
For Stokes Q, U and V, this is an absolute limit -- flux densities less that
$|\hbox{\tt clip}|$ are assumed to be noise or error.
\item {\tt offset}: The self-calibration tasks can be run assuming a 
point-source model. A point-source model will be assumed if no {\tt model}
parameter is given. In this case, {\tt offset} gives the offset, in arcsec, from the 
observation centre, of the point source. The default is to assume that the point
source is at the observation centre. Note that you should generally also
give the {\tt line} parameter when self-calibrating multi-channel data
with a point-source model.

\item {\tt flux}: For \task{selfcal}, when using a point-source model, this
gives the Stokes I flux density. The default is 1.
For \task{gpscal}, when using a point-source model, this
parameter gives the flux density of I, Q, U and V. The default is 1,0,0,0.
Also see the {\tt noscale} option below.
\item {\tt options}: The {\tt options} parameter, as usual, gives a number
of extra processing options. 
Options common to \task{selfcal} and \task{gpscal} are:

\begin{description}
\item[{\tt amplitude}] and {\tt phase}: This determines whether
the self-calibration tasks solve for just phase ({\tt options=phase}, which
is the default) or amplitude and phase solution ({\tt options=amplitude}).
\item[{\tt mfs\rm:}] This option is described in the section on frequency handling
(see above).
\item[{\tt noscale\rm:}] Normally the self-calibration tasks
scale the gains so that their
rms amplitude is 1. This preserves your primary flux density calibration.
The {\tt noscale} option prevents this scaling, thus allowing the absolute
flux density scale to be changed by the self-calibration process. You {\em will want}
to allow the flux density scale to change if you are self-calibrating several
datasets of the same source (i.e. using the same model) separately. For example,
you may have several datasets containing different configurations or
frequencies of the one source, and yet all these datasets are used to produce
the one output image. Using {\tt noscale}
ensures that the flux density scales resulting from the different
self-calibrations are the same. 
\end{description}

Options specific to \task{selfcal} are:

\begin{description}

\item[{\tt relax\rm:}] This relaxes the convergence criteria of gain solutions
somewhat, and may be helpful if there are a large number of solutions which
fail to converge. This option will be useful when the model is particularly
poor. This will be so in the early self-calibration iterations when
the primary calibration was quite poor.

\item[{\tt mosaic}:] This causes \task{selfcal} to select only those
visibilities whose observing centre is within plus or minus three
pixels of the model reference pixel. This is needed if the input visibility
dataset contains multiple pointings or sources. By default, no observing
centre selection is performed.
\end{description}

Task \task{gpscal} takes a few options to dictate how it handles 
\MM{XY} phases:

\begin{description}
\item[{\tt noxy\rm:}] Do not attempt to solve for \MM{XY} phase. By default
\task{gpscal} solves for a constant \MM{XY} phase on all antennas except
the reference antenna.
\item[{\tt xyref\rm:}] Solve for the \MM{XY} phase of the reference antenna
(as well as the \MM{XY} phase of the other antennas). This cannot
be used together with the {\tt noxy} option.
To use this option, the source should be strongly polarised
(at least 5\%), and Stokes Q and/or Stokes U models should be given.
\item[{\tt xyvary\rm:}] Solve for a variable \MM{XY} phase. It is 
recommended that you use this option, although this option does not
work for phase-only self-calibration. If used with{\tt options=xyref}
then \task{gpscal} additionally solves for a variable \MM{XY} phase on
the reference antenna.
\end{description}
\end{itemize}

\subsection*{Summary}

We finish with two examples, the first being for \task{selfcal}.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf SELFCAL}} \\
\hline
vis=vela.uv.cal		& Visibility data-set to be corrected,\\
			& Generally I,Q,U,V visibilities.\\
model=vela.icmp		& CLEAN component image\\
select			& Data to select.\\
interval=5		& Self-cal solution interval.\\
options=phase		& Phase self-calibration, or\\
options=amp		& Amplitude self-calibration, or\\
options=phase,mfs	& Phase-selfcal, MFS model, or\\
options=amp,mfs		& Amplitude self-cal, MFS model\\
line			& You need to specify this for MFS selfcal,\\
			& can generally default otherwise.\\
\hline 
\end{tabular}
\end{center}
The parameters for \task{gpscal} are very similar.
\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf GPSCAL}} \\
\hline
vis=vela.uv.cal		& Visibility data-set to be corrected,\\
			& Should be XX,YY,XY,YX visibilities.\\
model=vela.icmp,vela.qcmp,vela.ucmp	& CLEAN component images\\
select			& Data to select.\\
interval=5		& Self-cal solution interval.\\
options=phase		& Phase self-calibration, or\\
options=amp,xyvary	& Amplitude self-calibration, or\\
options=phase,mfs	& Phase-selfcal, MFS model, or\\
options=amp,mfs,xyvary	& Amplitude self-cal, MFS model\\
line			& You need to specify this for MFS selfcal,\\
			& can generally default otherwise.\\
\hline 
\end{tabular}
\end{center}




\newchapter
\chapter{Spectral Line Data Reduction}\label{c:lineobs}\index{spectral line}

\section{Velocities in \miriad}\index{spectral line, velocity definition}
Because of the Doppler effect, the observed frequency of a spectral line
can be related to the radial velocity between the
observer and the source. The relativistic
expression relating frequency to radial velocity is
\[
v = c\frac{\nu_0^2 -\nu^2}{\nu_0^2+\nu^2},
\]
where \MM{v} is the radial velocity, $\nu$ the observed frequency, $\nu_0$
the rest frequency, and \MM{c} is the speed of light.
For various reasons, astronomers usually approximate
this formula. There are two common approximations -- the ``radio
definition'',
\[
v_{\rm radio}=c(1-\frac{\nu}{\nu_0}),
\]
and the ``optical definition'',
\[
v_{\rm optical}=c(\frac{\nu_0}{\nu}-1).
\]
The radio definition has the advantage that points sampled at equal increments
in frequency translate to equal increments
in velocity. However the radio definition is now deprecated by the IAU.

In defining a velocity, a rest frame must also be given. Because of the
Earth's diurnal and annual motion (spin on its axis, and rotation around the
Sun), the Earth's surface is
not a good rest frame. The diurnal and annual motion have maximum
velocity components of approximately 0.5 \kms\ and 30 \kms\ respectively.
Two commonly used rest frames are the solar system barycentre (the
dynamical centre of the solar system) and the local standard of rest (LSR)
(which accounts for motion of the solar system relative to a
collection of local stars). The local standard of rest is generally
used as the rest frame for Galactic astronomy, whereas the barycentric
frame (often misnamed heliocentric frame -- which is slightly different)
is generally used for extragalactic work. \miriad\ uses the ``definition''
of the LSR that the solar system barycentre is moving at 20 \kms\  in the
direction of (RA,DEC) = 18 hours,+30 degrees (B1900) (this is a so-called
``kinematic'' definition of the LSR).

Because of the Earth's motion, the frequency that corresponds to a 
particular velocity of an astronomical source will change with time. When
observing spectral lines, many observatories continuously change the 
observing frequency to account for the effect of the Earth motion,
and thus make a particular
source velocity correspond to a single channel. This is known as 
Doppler tracking. 

{\em Note, however, that the ATCA does not Doppler track.}

This means that the channel that a particular source line corresponds to
varies with time. If the velocity resolution is coarse compared to the
diurnal velocity component of the Earth ( $< 0.5$ \kms), and only
a single configuration of data is being used, this effect can be ignored.
However when high velocity resolution is required or multiple configurations
are being combined, account must be made of the way the line wanders between
channels. That is, account must be taken of the changing radial velocity
of the observatory.

For a dataset from an observatory that does 
Doppler track (i.e. {\em not}  the ATCA), the frequency descriptors in a
dataset {\em should} vary with time. This will be the case with some
datasets, but not others -- the time varying Doppler frequency component has
been lost somewhere. Although this loss will affect the calculation of
\MM{u-v} coordinates, the error that it introduces is generally negligible
(see the discussion in the spectral-line processing chapter of Neil Killeen's
\htmladdnormallink{``Analysis of Australia Telescope Compact Array Data''}
{http://www.atnf.csiro.au/computing/software/atca_aips/atcal_html.html}).

Regardless of whether or not the observatory Doppler tracks, \miriad\ stores
the radial velocity of the observatory (with respect to the rest frame)
to account for the Earth's motion when determining source velocities.
This is stored in the visibility
variable \verb+veldop+.

\section{ATCA Spectral Line Correlator Configurations}
Many of the ATCA correlator configurations are specifically intended
for the spectral line observer. One aspect of the ATCA correlator
that you should note is that the spectral resolution is often
moderately different to the channel increment. For a full
description of this, see the \htmladdnormallink{memo by Neil Killeen}
{http://www.atnf.csiro.au/observers/memos/d98b8b\~{}1.pdf} (AT Memo 39.3/057).

\section{Spectral Line Processing and RPFITS files}\label{s:specrpfits}\index{atlod}
When using \task{atlod} to read in spectral line data, there are a few 
extra parameters that should be considered
\begin{description}
\item[{\tt ifsel}:] The ATCA allows you to observe two frequency setups
simultaneously\verb+ifsel+ allows you to load just one of them by
setting \verb+ifsel=1+ or \verb+ifsel=2+. The default is to load both
of them.
\item[{\tt restfreq}:] The RPFITS file does not contain the
rest frequency of the spectral line observed, so you need to give
this at some stage during data reduction -- the earlier the better.
The rest frequency can be entered (via \verb+restfreq+, in GHz) in
\task{atlod}.
\end{description}

There are a few {\tt options} in \miriad's \task{atlod} of particular
interest to spectral line observers:
\begin{description}
\item[{\tt bary:}] By default, task \task{atlod} uses the LSR as the rest
frame when computing velocity information.
However you can change
this to the barycentre by using {\tt options=bary}. Note that LSR
velocities are the standard for Galactic astronomy, whereas barycentric
are more commonly used by extragalactic astronomers. If you load with
the wrong velocity reference frame, you can change this later with
\task{uvredo} (for visibility datasets) or \task{velsw} (for images).
\item[{\tt birdie:}] The ATCA suffers self-interference at frequencies
which are multiples of 128 MHz (e.g. 1408 MHz). For spectral line
modes, the \verb+birdie+ option flags any channels that are affected
by the self-interference. This is strongly recommended.
\item[{\tt hanning}:] This option performs Hanning\index{hanning}
smoothing of the
spectra, and discards every second channel.
\item[{\tt compress}:] This causes the data to be written in a scaled 16-bit format,
thus reducing disk usage by a factor of two.
\item[{\tt noif}:] If you are loading the ATCA's two frequency setups
simultaneously, \task{atlod} normally tries to stack the two together
as ``spectral windows'' (IFs in \aips\ terminology). However this will
not be possible if the two setups sample different sets of polarisation
parameters. In this case, you will need to use \verb+options=noif+ to 
get \task{atlod} to store the different frequency setups as
different visibilities.
\end{description}

\section{Spectral Line Processing and FITS files}
\index{spectral line, fits and velocity definitions}
When \task{fits} reads a visibility FITS file, it can use one
of two schemes for computing the observatory's radial velocity.
Possible schemes are:
\begin{enumerate}
\item \task{fits} assumes that a velocity of a given channel remains constant.
This means that one of the following three conditions is satisfied:
\begin{itemize}
\item Correcting shifts have been performed on the spectra by offline
software to account for the Earth's motion (e.g. by \aips\ task \task{CVEL}
or various tasks in \miriad).
\item The velocity resolution is sufficiently coarse that Doppler tracking is
not required.
\item Doppler tracking has been performed during the observation (not ATCA data).
\end{itemize}
If any one of these is satisfied, then given the velocity of a particular
channel, \task{fits} can determine the corresponding
observatory radial velocity that this would imply.

You can either allow \task{fits}
to retrieve the needed velocity information from the FITS file, or you
can give this
information explicitly using the \verb+velocity+ keyword. The default
behaviour (if you do not set the \verb+velocity+ keyword at all) is to
use the information in the FITS file. You will need to explicitly enter the
velocity information to \task{fits} if the information in the FITS file
is incorrect.

The \verb+velocity+
keyword gives three values: the velocity definition and rest frame,
the velocity of a particular channel, and the channel velocity in \kms. Possible
values for the velocity definition and rest frame are {\tt lsr} (LSR
velocity, radio definition), {\tt bary} (barycentric velocity, radio
definition), {\tt optlsr} (LSR velocity, optical definition),
{\tt optbary} (barycentric velocity, optical definition) and {\tt obs} (velocity
relative to the observatory -- not of much use). For example, to indicate
that channel 257 has an LSR velocity (radio definition) of 4310 \kms, use
\begin{verbatim}
    velocity=lsr,4310,257
\end{verbatim}
Specifying the velocity of a channel for a multi-source FITS file is not
a particular meaningful thing to do (unless you are interested in only
one source).

\item \task{fits} assumes that the velocity of a particular channel varies
as a result of the Earth's motion. This will be the case for ATCA observations
and so should generally be used
(unless \task{CVEL} has been used in \aips, or similar corrections
performed in \miriad). In this case, \task{fits} will compute the Earth's
radial velocity as a function of time. The model of the Earth's motion 
used by \task{fits} is accurate to better than 0.005 \kms -- which is adequate
for most radio-astronomical applications. \task{fits} will compute
the radial velocity from its Earth model if a velocity frame
and definition is given in the
\verb+velocity+ keyword (either {\tt lsr, bary, optlsr, optbary} or
{\tt obs}, as before), but the velocity and reference channel are omitted. For
example, \task{fits} will use its Earth model to determine the observatory's
radial velocity with respect to the LSR if given:
\begin{verbatim}
    velocity=lsr
\end{verbatim}
\end{enumerate}
It is recommended that the second approach is used for ATCA data that has not
already been corrected for Earth motion. This is especially so if the velocity
information in the FITS file is incorrect.

\section{Listing and Plotting Velocity Information}
\index{spectral line, listing velocity information}
The task \task{uvlist} can be used to list information about the
frequency and velocity setup, with the \verb+options=spectral+ listing.
This prints out the velocities for a visibility record. It is suggested that
you do this before proceeding much further, to ensure that the velocity
and rest frequency information is what you expect.

The task \task{varplt} can be used to plot or list the observatory's
radial velocity as a function of time. Use the parameter setting
\verb+yaxis=veldop+.

\section{Recomputing Velocity Information}\index{spectral line, recomputing velocity information}\index{uvredo}
Although \task{atlod} or \task{fits} is the best place to set the velocity
information, this
can also be handled by the task \task{uvredo}. This task recomputes
a number of the `derived' quantities (such as observatory radial velocity)
which are stored in a visibility dataset. You have to select
\task{uvredo}'s {\tt velocity} option, and set the keyword {\tt velocity}
to indicate the rest frame to be used. Typical inputs are:

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVREDO}} \\
\hline
vis=line.uv		& Input dataset name\\
out=line2.uv		& Output dataset name\\
options=velocity	& Recompute velocity information\\
velocity=lsr		& Use the LSR as the rest frame\\
\hline 
\end{tabular}
\end{center}

\section{Setting the Rest Frequency}\index{spectral line, rest frequency}
The rest frequency of a line may sometimes need to be set or corrected
(e.g. you forgot to enter it in \task{atlod}, or the FITS file failed to
contain this information). 
This can be done with \task{puthd}.

\begin{center}
\begin{tabular}{|l|l|} 
\hline
\multicolumn{2}{|c|}{{\bf PUTHD}} \\
\hline
in=line.uv/restfreq      & Set the parameter \verb+restfreq+ of \\
			 & dataset \verb+line.uv+\\
value=1.420405752d0      & Rest frequency, in GHz. Express it as \\
			 & a double precision number. \\
\hline
\end{tabular}
\end{center}

\section{Velocity Linetype}\label{s:veloline}\index{velocity, linetype}
\index{spectral line, velocity linetype}
As mentioned in Section~\ref{s:line},
many \miriad\ tasks allow a range of channels to be given by
their velocities (rather than their channel numbers). Because these
channels often do not correspond to any channels that the correlator
might produce, they will be call ``velocity channels''. Velocity
channels are specified using the
\verb+line+ parameter. The velocities are given in \kms, in the radio
or optical definition,
with respect to the rest frame of the dataset.
The main reason for `velocity channels' is that they are corrected for
the Earth's motion -- a particular source
velocity component will remain in the same velocity channel regardless of
whether the observatory Doppler tracks or not.

The velocity specification is given in the form
\begin{verse}
{{\tt line=velocity,}{\it nchan}{\tt,}{\it start}{\tt,}{\it width}{\tt,}{\it step}}
\end{verse}
or
\begin{verse}
{{\tt line=felocity,}{\it nchan}{\tt,}{\it start}{\tt,}{\it width}{\tt,}{\it step}}
\end{verse}
where {\it nchan} is the number of velocity channels to select, {\it start}
is the centre velocity of the first channel selected, {\it width} is
the width of each channel, and {\it step} is the velocity step between
channels. With the \verb+velocity+ form, the radio velocity definition is
assumed, whereas the \verb+felocity+ form uses the optical definition.
Note, however, both forms produce channels at equal increments in
{\em radio} velocity ({\em even the \verb+felocity+ linetype}). See
Section~\ref{s:line} for more information.

For example
\begin{verse}
{\tt line=velocity,10,1.5,1.0,3.0}
\end{verse}
will specify 10 channels, having velocities (radio definition)
centred at 1.5, 4.5, 7.5, etc, \kms, with respect to the rest frame.
Each channel has a width of 1 \kms.

If the {\it start}, {\it width} and {\it step} values cause the velocity
channels not to map directly to the underlying correlator channels,
a weighted sum of correlator channels is used to determine a velocity
channel.
If the magnitude of the velocity channel width (the {\it width} value in
\kms) corresponds to correlator channel increment, i.e. if
\[
|width| = | c\frac{\Delta\nu}{\nu_0} |
\]
(where $\Delta\nu$ is the correlator channel increment) then the weighted sum
is equivalent to linear interpolation.

For telescopes that do not Doppler track, such as the ATCA, it
will rarely be the case that velocity channels map directly to correlator 
channels because of the continual change in the observatory's radial velocity.

There are some caveats when using velocity channels:
\begin{itemize}
\item The rms noise in each velocity channel will vary if there is not a 
one-to-one mapping between these and correlator channels. Also there
may be some noise correlation between consecutive channels. The theoretical
noise given by \miriad\ will generally be an overestimate. This is
worst for a long synthesis when the channel resolution is 0.5 \kms or
finer. In this case, the true theoretical noise will be, on average,
a factor of $\sqrt{3/2}$ less than that suggested by \miriad.
\item The ``weighted sum'' method of determining a velocity channel
is inferior to more sophisticated interpolation schemes (e.g. the FFT based
interpolation in \aips\ \task{CVEL}).
\end{itemize}

\section{Continuum Subtraction}\index{spectral line, continuum subtraction}
\index{uvlin}

Spectral line observations will always be ``corrupted'' by some
continuum emission.  If this is stronger or comparable with the spectral
line, then the final spectral images may be dominated by the continuum
and its artifacts (sidelobes, deconvolution errors, calibration errors,
etc).  If the final dynamic range of your image is dominated by
continuum errors, then weak spectral features will be completely
obscured.  It is possible to remove the continuum so that
the final dynamic range of the spectral image is dependent on thermal
noise and artifacts resulting from the spectral line (and {\em not}
artifacts from the continuum).  Consequently it is usually best to
subtract the continuum from the data.  A good description of the
algorithms to do this is given by
\htmladdnormallink{Cornwell, Uson and Haddad (1992)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1992A\%26A...258..583C}.
(A\&A 258, 583).
Neil Killeen's
\htmladdnormallink{``Analysis of Australia Telescope Compact Array Data''}
{http://www.atnf.csiro.au/computing/software/atca_aips/atcal_html.html}
also gives a good summary as well as describing the \aips\
possibilities.  The paper \htmladdnormallink{Sault (1994)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1994A\%26AS..107...55S}
(A\&AS 107, 55)
describes one of the algorithms in detail.
Yet another summary is given below. 

Although three approaches to continuum subtraction are possible in \miriad,
we recommend only one of these -- the {\tt UVLIN} approach. The other
two are briefly mentioned for completeness:
\begin{itemize}
\item[{\tt UVSUB}:] This is the most time consuming of the techniques, which
involves forming a continuum image from the line-free channels, deconvolving
this and subtracting it from the visibility data-set. In addition to the
normal imaging and deconvolution tasks, \task{uvmodel} will be used to perform
the actual subtraction of the continuum model from the visibility data.
The {\tt options=mfs} switch will be useful in this context.
No more details to this approach will be given here.

\item[{\tt IMLIN}:] This is an image-based technique, where a polynomial fit
of the continuum is done along the spectral axis at each pixel in the dirty
image. Although \miriad\ tasks \task{contsub} and \task{avmaths} can be used to
perform this, no more details will be given here.

\item[{\tt UVLIN}:] This is the recommended approach in \miriad\ to
perform continuum subtraction. It is implemented with the task called
(surprisingly) \task{uvlin}. 

In this scheme, each visibility spectrum is fitted by a polynomial.
Only the line-free channels of the spectrum are used to determine the fit
The polynomial can be taken to represent the continuum emission, and so
can be subtracted from the spectrum. Because bandwidths in spectral
line experiments are often narrow, the continuum emission is
usually well modelled by a fairly low order polynomial
(zeroth, first or second order).
The fit is performed to the real and imaginary parts of the data separately.
In general, this is better than fitting to the amplitude and phase, as the
whole process remains a linear one. A non-linear fit to amplitude and phase
couples together the errors on all sources, and produces an amplitude
bias at low signal-to-noise ratios.

\end{itemize}

The main advantage of {\tt UVLIN} over the other two approaches is that
it is generally the most robust to a large variety of systematic errors
(such as antenna gain errors). {\tt IMLIN} is also moderately robust to these
errors, whereas {\tt UVSUB} is not particularly robust at all. {\tt UVLIN},
along with {\tt IMLIN}, is substantially less expensive computationally
than {\tt UVSUB},
and they generally requires less care and intervention on the part of
the user.


{\tt UVLIN}, however, is not perfect. There will be some residual continuum
left behind, and the noise level in the output will be amplified in those
channels that were excluded from the fitting process. These drawbacks
are not unique to {\tt UVLIN} -- they will be a characteristic of any
continuum subtraction technique.
There are three parameters to control {\tt UVLIN} --
the set of channels to be
used in the fitting process (i.e. the set of channels that are assumed
to be line-free), the order of the polynomial to fit and an offset to be
applied to the data before the fit
is performed. The combination of these three parameters will determine
the residual continuum. They also determine the factor by which the noise
level increases in the data. We clearly want to keep this noise amplification
to an acceptable level.

Usually we have little direct control over the channels that
are line-free. Obviously, though, the more line-free channels we have,
the better the fitting process will be. {\em Do not discard any
line-free channels until after continuum subtraction.}

If there are line-free channels on only one side of the spectrum (i.e.
you have to extrapolate the continuum fit), then generally it is unwise
to use anything but a 0th order fit. This is because the residual continuum
is not greatly affected by the fit order, whereas noise amplification can
become extreme.

On the other hand, if there are line-free channels on both sides of the
spectrum, and provided the fraction of channels involved in the fitting
process is appreciable (at least 25\%), then higher order fits are quite
useful. The residual continuum depends on the continuum source flux
density and the parameter $\eta$. For a source
at location $(\ell_0,m_0)$ (with respect to the phase centre),
\[
  \eta = \frac{1}{2}\frac{\Delta\nu}{\nu}\frac{\sqrt{\ell_0^2+m_0^2}}{\theta}.
\]
Here $\Delta\nu$ is the total bandwidth and $\nu$ is the observing
frequency. For a point source $\theta$ is the resultant image resolution. For
resolved sources, $\theta$  is the angular extent of typical image features.
Unfortunately you cannot really determine $\eta$ without having formed
a continuum image.
The maximum residual continuum is approximately as given in Table~\ref{table:contsub}.
\begin{table}
\begin{center}
\begin{tabular}{cc}
\hline
Fit Order&Maximum Residual Continuum Error\\
\hline
0& $\displaystyle S\frac{\pi^2}{6}\eta$\vspace{1mm}\\
1& $\displaystyle S\frac{\pi^2}{9}\eta^2$\vspace{1mm}\\
2& $\displaystyle S\frac{\pi^4}{150}\eta^3$\vspace{1mm}\\
3& $\displaystyle S\frac{\pi^4}{525}\eta^4$\vspace{1mm}\\
\hline
\end{tabular}
\end{center}
\caption{Approximate residual error for visibility-based continuum subtraction.}
\label{table:contsub}
\end{table}
Because of the narrow bandwidth of most spectral line experiments,
$\eta < 1$, and so the residual continuum decreases rapidly as the fit
order is increased. A zeroth or first order fit will be quite adequate for many
spectral line experiments. Given a continuum image, the task \task{conterr}
can determine images which should be indicative of the residual. There should
{\em not} be viewed as strict error images. Rather they indicate the magnitude
and location of the residual errors.

As the fit order is increased, noise amplification also increases.
Generally, provided we have a significant
number of line-free channels (and you are not extrapolating) the noise
amplification is modest. You should be cautious of noise amplification
for fourth order fits if fewer than 50\%\ of the channels are
line-free, or for third order fits when fewer than 25\%\ of the channels are
line-free. \miriad\ task \task{contsen} can be used to determine the noise
amplification given the fitting order and the channels included in the
fitting process.

The fact that the residual continuum is a function of the distance of
the point source from the phase centre should not be a surprise.  As
\MM{u-v} coordinate is proportional to frequency, the channels in a
visibility spectrum are sampling slightly different locations in the
\MM{u-v} plane.  For a point source (which we assume to have a spectral
index of zero, for simplicity) will have a visibility function whose
real and imaginary parts are sinusoids.  The fraction of a period of
this sinusoid contained within the \MM{u-v} coordinates spanned by a
visibility spectrum will be directly proportional to the distance of the
point source from the phase centre.  The further the point source is
from the phase centre, the greater proportion of a sinusoid period is
present in the visibility spectrum, and the poorer the approximation of
the visibility spectrum by a low order polynomial. 

This leads us to another way of reducing the residual continuum -- we
can shift the phase centre to the location of the continuum source.
The amount of benefit derived from such a shift will depend strongly
on the skewness of the distribution of the continuum, and its distance
from the phase centre. The task \task{conterr} can suggest shifts
which will minimise the error (see {\tt options=shift}).

\subsection*{Computation}
For a relatively simple program to drive, \task{uvlin} has a rather large
number of input parameters. Task \task{uvlin} behaves somewhat like a
copying program, taking an input dataset (selecting, calibrating and
performing Stokes conversion, as appropriate), performing the continuum
subtraction, and writing out a resultant dataset. Input parameters are:
\begin{itemize}
\item {\tt vis}: The name of the input dataset.
\item {\tt select}: The subset of data to select.
\item {\tt line}: The normal line parameter.
\item {\tt chans}: This gives the set of channels to be used in the fitting
process. It consists of a number of pairs giving the start and end range
of the channels to include in the fitting process. Note that there channel
numbers are {\em relative} to the channels selected with the {\tt line}
parameter. 
\item {\tt out}: The output dataset.
\item {\tt order}: The fit order. The default is 1.
\item {\tt offset}: An offset to be applied to the data before the fit, and
``deapplied'' after the fit. The net result is that the phase centre of the
data is unchanged. However the fitting process is better behaved if
the phase centre during the fitting process is located at (or near) the
dominant continuum emission. The default is to do no shifting.
\item {\tt mode}: This determines what form of data are written to the output
dataset. Normally you will want to write the {\tt line} data (the data
after continuum subtraction). However you can also write out the fitted
continuum ({\tt mode=continuum}) or a fitted channel-0 dataset
({\tt mode=chan0}). Task \task{uvlin} also has a option where it can solve
for line shape (see {\tt options=lpropc} below). In this case, you can
also write out the fitted line ({\tt mode=fit}).

\item {\tt options}: The {\tt options} parameter gives the normal
ability to turn off calibration. Note that
you will generally want calibration
to be applied, as a copy of the dataset is being formed. It is particularly
important to apply bandpass correction. There are a few extra switches in
addition to the calibration switches:
\begin{description}
\item {\tt sun}: With this option, the {\tt offset} keyword is ignored,
and a shift appropriate for the Sun is determined instead. Thus,
if your continuum is dominated by the Sun, this will help eliminate it.
You will normally want to use the ``\verb+twofit+'' option with this (see
below).
Also see \htmladdnormallink{Sault \& Noordam (1995)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1995A\%26AS..109..593S}
(A\&AS 109, 593) for information and an example.
\item {\tt twofit}: If an ``\verb+offset+'' is given, or if the ``\verb+sun+''
option is used, \task{uvlin} normally fits for continuum just at the
offset or solar position. By using the ``\verb+twofit+'' option, \task{uvlin}
will fit for continuum at both the phase centre and the offset/solar
position.
\item {\tt relax}: Normally \task{uvlin} attempts to avoid overfitting the
data if it finds a significant number of flagged continuum channels (more
than 40\%\ bad). If it does find a large number of flagged channels, it starts
reducing its fit order. The {\tt relax} option tells \task{uvlin} to fit
the order you ask for regardless of large numbers of flagged channels.
\item {\tt lpropc}: In recombination line experiments, it might be a good
approximation that the line strength is proportional to the continuum. Using
this extra constraint, \task{uvlin} can simultaneously fit for both a
continuum spectrum (varies from visibility to visibility) and a line
spectrum (which is a function of the continuum strength). This results in
an iterative algorithm, which can take an appreciable time, with unfortunately
only modest improvement in results. You probably do not want to use this
option.
\end{description}
\end{itemize}

Typical inputs to \task{uvlin} are:
\begin{center}
\begin{tabular}{|l|l|} 
\hline
\multicolumn{2}{|c|}{{\bf UVLIN}} \\
\hline
in=line.uv		& Input dataset before continuum subtraction.\\
select			& Leave unset to select all data.\\
line			& Leave unset to select all channels.\\
chans=1,100,300,512	& Channels 1 to 100, and 300 to 512 inclusive\\
			& contain only continuum emission.\\
out=line.sub		& The output continuum subtracted dataset.\\
order=1			& Use first order fit (which is the default).\\
offset			& The offset to apply. Probably leave unset.\\
mode			& Leave unset to get line data after\\
			& continuum subtraction.\\
options			& Probably leave it unset\\
\hline
\end{tabular}
\end{center}


\section{Imaging and Image Velocity Definitions -- INVERT and VELSW}
\index{invert}
\index{spectral line, imaging}
\index{spectral line, image velocity axis}\label{s:slinvert}
A few comments are in order when using the imaging task, \task{invert},
to create spectral cubes (more information on \task{invert} can be found
in Chapter~\ref{c:imaging}):
\begin{itemize}
\item To conserve space, \task{invert} produces a single beam dataset for
all planes of a cube. For it to do this, the cell size of each plane is
scaled with frequency. For the small fractional bandwidths typical in
spectral-line experiments, this affect is small. Additionally the
coordinate-handling software accounts for this scaling when converting
between pixel and celestial coordinates.

For one beam to be valid for all planes, \task{invert} usually insists that
the \MM{u-v} coverage for all planes is the same. That is, \task{invert}
usually insists that no channels of a spectrum are flagged before that
spectrum can be used for imaging. This is required to ensure each image
plane has the same point-spread function.

This rule can be too restrictive
for some uses. For example if deconvolution is not envisaged or if one
channel (the birdie channel?) is always bad.
\task{invert}'s {\tt slop} keyword allows the rule to be relaxed.
The {\tt slop} keyword takes two values. The first is the fraction of channels
that \task{invert} will tolerate as being bad before a spectrum is
rejected. The default is 0 (i.e. the entire spectrum is rejected if even a
single channel is bad). A value of 1 indicates that \task{invert} will
accept a spectrum provided at least one channel is good.
The second value dictates the `replacement' method, i.e. what value to
assign for the flagged channel, when imaging with flagged channels.
Possible values are `{\tt interpolate}' and `{\tt zero}'.

Interpolation replacement would be appropriate if there are a small
number of channels that are always bad, and that you wish to replace them
with something for, essentially, aesthetic reasons. Note that the interpolation
is simple-minded linear interpolation.

Surprisingly zeroing the flagged correlation also has its advantages. When
zeroing, the true beam (point-spread function) will be channel dependent
and will differ from the computed beam.
Consequently deconvolution should not be attempted.
\task{invert} attempts to scale each plane so that the true beam
has a peak value of 1. This scaling is correct only for natural weighting
when no tapering is used (i.e. {\tt sup=0} and {\tt fwhm} unset). If this
is the case, the channel images can be astrophysically useful. For
other weightings and taperings, the peal value of the true beam will not be
exactly 1 -- the flux density scale will no longer be what is conventionally
understood by Jy/(dirty beam). Additionally the beam peak value can vary from
plane to plane, and so the flux density scale may not be comparable between
planes. Be warned.

\item \task{invert} checks that the velocity of visibilities gridded onto
a particular plane remains constant. The velocity will not remain
constant if the observatory does not Doppler track and velocity linetype
is not being used. If the velocity of visibilities gridded onto a plane
varies by more than 10\%\ of the plane separation, \task{invert} gives a warning.
\item Regardless of the \verb+line+ parameter,
\task{invert} labels the velocity axis using the radio definition.
However it is possible to switch the labelling of this
axis to be either frequency or velocity using the optical definition,
or indeed back to velocity using the radio convention. Note that for
the optical velocity definition, the velocity separation between planes is
not constant. The velocity increment stored in the header is that which
corresponds to the reference frequency.
The task \task{velsw}\index{velsw} performs this task. Its inputs are fairly simple:
\begin{center}
\begin{tabular}{|l|l|} 
\hline
\multicolumn{2}{|c|}{{\bf VELSW}} \\
\hline
in=line.map		& Input image.\\
axis=optical		& Label as velocity using optical definition,\\
axis=radio		& or label as velocity using radio definition,\\
axis=frequency		& or label as frequency.\\
\hline
\end{tabular}
\end{center}
\end{itemize}



% Image display
\newchapter
\input{imdisp.inc}

% Image Analysis
\newchapter
\input{imanal.inc}

% Spectral display
\newchapter
\input{spdisp.inc}

% Spectral analysis
\newchapter
\input{spanal.inc}

% Mosaicing
\newchapter
\input{mosaic.inc}

\newchapter
\input{mm.inc}

\newchapter
\input{pulsar.inc}

\newchapter
%\chapter{Multi-Frequency Synthesis Observing Strategies}\label{c:mfs}\index{multi-frequency synthesis}
%
%Multi-frequency, or bandwidth synthesis, is the practice of combining
%continuum data at several frequencies during the imaging stage to
%produce a single output image.  \MM{u} and \MM{v} for each correlation are
%calculated from its actual frequency (rather than some mean frequency),
%and each correlation is convolved onto the gridded \MM{u-v} plane as a
%separate visibility. 
%
%There are a number of advantages to this:
%\begin{itemize}
%
%\item The different frequencies give different \MM{u} and \MM{v} spacings
%giving better \MM{u-v} coverage. This is particularly important for an
%instrument like the ATCA, which has relatively few baselines.
%
%\item Multi-frequency techniques can be quite helpful for ATCA snapshots. 
%Given the regularity of the ATCA spacings, observing at a single
%frequency will result in quite large snapshot sidelobe responses -- at
%least at some points.  By observing multiple frequencies, these large
%sidelobes tend to be smeared out. 
%
%\item It allows a good image to be made from a wider bandwidth than would
%otherwise be acceptable. This wider bandwidth produces an image in which
%the thermal noise is reduced.
%
%\item Dividing a single wide bandwidth into many narrower channels, and
%gridding them at their correct \MM{u} and \MM{v} coordinates reduces bandwidth
%smearing. 
%
%\end{itemize}
%
%The ATCA is an excellent instrument for multi-frequency techniques.  A
%single observation could result in several frequencies in three ways. 
%Firstly the
%normal continuum mode is for the correlator to divide the 128-MHz
%bandwidth into 32 channels (although, due to bandpass effects, 
%usually only the central
%100 MHz is usable).  Indeed, the natural ATCA imaging mode
%should be to use multi-frequency synthesis. In the 20-cm band, the 128 MHz
%bandwidth provides an 8\%\ frequency spread. If all the channels are retained
%throughout data processing, this is enough to significantly improve
%the \MM{u-v} coverage.
%Secondly, the ATCA has the ability
%to observe two separate frequency bands simultaneously. These two bands
%can be combined into the one output image.
%Thirdly, the wide bandwidths of the front-ends and the frequency agility
%of the receivers means that observing frequencies can be switched
%rapidly -- indeed every integration.  In practice, because of problems
%with the hardware that measures system temperature, it is generally not
%advisable to shift frequencies more often than once every few minutes
%(see Russell Gough's note ``Transient Response of the Receiver System'').
%While switching frequencies with time does result in better \MM{u-v} coverage,
%it clearly does not improve sensitivity. It also involves a trade-off
%between tangential and radial holes in the \MM{u-v} plane.
%
%All three of the above approaches may be used together to maximise the number of
%frequencies observed in a single observation.  The applicability of each
%of them will depend on the objectives of the observation.  Observations
%where high dynamic-range images are desired will benefit from the
%improved \MM{u-v} coverage that results from frequent changes of the observing
%frequency.  Detection experiments are unlikely to benefit from this frequency
%switching, unless there are confusing sources that need to be imaged. 
%However, using the ATCA's ability to observe at two different bands will
%double the observing bandwidth, and consequently reduce the final noise
%by a factor of $\sqrt{2}$ (do not observe exactly the same band -- you will
%measure exactly the same signal, and noise, twice, and so you will not get
%a $\sqrt{2}$ improvement). 
%
%
%\section{Multi-Frequency Synthesis Data Reduction}
%The bulk of the tasks required to reduce multi-frequency synthesis observations
%are the same as for normal continuum images (e.g. calibration, imaging,
%deconvolution and self-calibration). Usually an extra switch has to
%be given to invoke multi-frequency processing (usually with {\tt options=mfs}).
%
%
%\section{ATCA Multi-Frequency Observing Strategies -- MFPLAN}
%\index{multi-frequency synthesis, planning}\index{mfplan}
%Given that you are going to perform a multi-frequency experiment, you
%will have determine the appropriate frequencies, and how many frequencies
%will be observed with each configuration. As the multi-frequency synthesis
%techniques in \miriad\ do not cope well with large spreads in frequencies
%(greater than about 30\%), your frequencies should be confined to one of the
%ATCA observing bands (i.e. either 3, 6, 13 or 20 cm). In the following, we
%will assume that you want to derive the best result at one frequency
%band, and so all frequencies will be confined to one ATCA observing band.
%
%There are some pretty simple rules to follow for the number of
%frequencies to observe per configuration:
%\begin{itemize}
%\item For detection experiments and the like, \MM{u-v} coverage is generally
%not important. The time wasted in switching frequencies means time lost
%integrating. It is best to observe at two frequencies, which are
%measured simultaneously.
%\item When observing in the 13 and 20 cm bands, the width of the bands 
%(relative to
%the 128-MHz continuum bandwidth) is comparatively small. There appears
%little benefit switching frequencies during the observation, as there are
%few places to switch to, and there is plenty of interference. Again it is
%probably best to observe at just two frequencies, which are measured
%simultaneously.
%\item The 3 and 6 cm bands are quite wide relative to the 128 MHz continuum
%bandwidth. It is advantageous to switch frequencies at approximately 5 minute
%intervals. Either 4 (usually) or 6 (occasionally) frequencies are appropriate.
%(i.e. 2 or 3 pairs of frequencies).
%As you will want to observe you calibrators in with the same frequency
%setup, a typical observing strategy would be to cycle through the frequency
%setup on your program source two or three times, and then cycle through them
%once on the secondary calibrator.
%\end{itemize}
%
%The above are the number of frequencies to use {\em per configuration}.
%In general, if you have multiple configurations, you will
%observe with different frequencies for each configuration. When two or
%three configurations are being used, and \MM{u-v} coverage out to 6 km is
%required, the best choice of configurations is a combination of 6.0
%with 1.5 and 0.75 configurations ({\em not} two or three 6.0
%sets).
%
%Given the number of frequencies to observe per configuration, and the 
%configurations to be used, the task \task{mfplan} can find frequencies
%which optimise the \MM{u-v} coverage. In doing so, it does not consider
%tangential holes -- effectively it assumes that the frequencies are
%switched sufficiently rapidly that these are not a major consideration.
%A description of the parameters of \task{mfplan} follows:
%\begin{itemize}
%
%\item {\tt freq}: This gives a number of pairs of frequency ranges.  Each pair
%gives a range over which observation is possible.  Generally you would
%set this to avoid observing near known interference sources.  This is
%important in the 20- and 13-cm bands, but less so in the 6- and 3-cm
%bands.  See the technical memo `A survey of the interference in the 13
%and 20 cm bands at the ATCA' by Mark Wieringa and Ravi Subrahmanyan. 
%
%\item {\tt nfreq}: The number of frequencies to observe (for each
%configuration). 
%Generally this will be 2, 4 or 6.  As the ATCA can observe two separate
%frequencies
%simultaneously, this means that it would cycle around 1 to 3 settings.
%
%\item {\tt nants}: The number of antennas to use. This can be set to 5 or 6,
%depending on whether you want to optimise \MM{u-v} coverage over 3~km or 6~km.
%
%\item {\tt config}: ATCA configuration identifier, such as {\tt 3.0a, 3.0b},
%etc. See the help file for the possible configurations. Several configurations
%can be given. The observing frequencies determined by \task{mfplan} will
%be optimised for the best \MM{u-v} coverage from the combination of data
%from all the configurations. 
%\item {\tt device}: Standard PGPLOT device, where a plot of the \MM{u-v} coverage
%will be produced. The default is no plot. This plot assumes that you are
%observing at the pole. Nor does it allow for frequency switching (i.e. it
%assumes all frequencies are observed simultaneously).
%\end{itemize}
%
%Task \task{mfplan} can take a while to run if there are many frequencies
%or configurations. As it iterates towards a solution, it prints out
%a ``temperature'' (a control parameter in the optimisation, which is
%not of much interest to you) and a ``goodness''. This ``goodness'', the
%parameter that \task{mfplan} is optimising, is
%the ratio of a measure of the goodness of the actual \MM{u-v} coverage to the
%ideal \MM{u-v} coverage that could be achieved given the number of baselines
%and number of frequencies. The goodness will always be less than 1, because
%\task{mfplan} has no control over the baselines (you, or the Time
%Assignment Committee and the ATCA itself, dictate these), and the frequency
%band is also constrained. If the number of configurations and frequencies
%are the same, the goodness measures are directly comparable. However the
%goodness measure does not give too much insight into the relative merits
%of using different numbers of configurations or frequencies.
%
%\begin{center}
%\begin{tabular}{|l|l|} 
%\hline 
%\multicolumn{2}{|c|}{{\bf MFPLAN}} \\
%\hline
%freq=4.6,6.4		&Frequency range being observed \\
%nfreq=6		&Number of frequencies to observe\\
%nants=		&Number of antennas to use; should be 5 or 6. Default is 6\\
%config=0.75c,3.0a	&Array configurations to be used.Here configurations\\
%			&0.75C and 3.0A will be used\\
%device=/xs		&PGPLOT plotting device to show \MM{u-v} coverage\\
%\hline 
%\end{tabular}
%\end{center}

