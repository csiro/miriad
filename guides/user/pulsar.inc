\chapter{ATCA Bin Mode Observations}\label{c:pulsar}\index{pulsar bin mode}

\section{ATCA Correlator Bin Modes}
The ATCA correlator has a mode whereby each correlator cycle (which is
normally the same as an integration cycle) can be
broken into a number of time bins. This capacity can be used in two
different modes:
\begin{itemize}
\item Pulsar bin mode: This mode is useful for a periodic signal with
a period much shorter than the correlator cycle, i.e. pulsars. In
pulsar bin mode, the bins produced by the correlator
correspond to the integration during a particular phase
of the pulsar cycle.
\item High time resolution bin mode: This mode is useful for observations
which, for some reason, require higher time resolution than the normal
correlator cycle time (the minimum correlator cycle time is about
10~s). In this mode, the correlator cycle is broken up into a number of
finer time bins, effectively working around the minimum correlator cycle
time limit.
\end{itemize}

\section{High time resolution bin-mode observations}\label{s:hires}
\miriad's \task{atlod} task has an option, \verb+options=hires+ which
converts a RPFITS file observed in high time resolution bin-mode into
an apparently normal dataset with fine time resolution. You should note
the following caveats:
\begin{itemize}
\item Although the timestamp of each correlation reflects the
observing time of that correlation, the \MM{u-v-w} coordinate is that
for the midpoint of the correlator cycle (i.e. the \MM{u-v-w}
coordinate is not recomputed at the high time resolution).
\item Certain \miriad\ tasks that need to detect the end of an
integration do this by checking whether consecutive timestamps differ by
some tolerance. In high resolution time mode, the time difference
between consecutive integrations might be smaller than this tolerance.
There may be some resulting confusion.
\end{itemize}


\section{Pulsar bin-mode observations}
When observing pulsars with the ATCA, the correlator is capable of sampling
synchronously with a pulsar. In this way, during a single integration,
the correlator will produce a number of {\em bins}, each of which sample
at a particular range in phase of the pulsar cycle. 
Much of the time, when reducing a bin-mode experiment, you will want to process all
the bins without regard to the bin number. In this case, to a large degree,
processing a bin-mode
observation differs little from a normal observation. In particular:
\begin{itemize}
\item No special action is needed in loading the data with \task{atlod}.
\item During the flagging process, tasks \task{tvflag} and \task{blflag}
will simply average together all the pulsar bins before display. When
you flag a point on the display as bad, all the bins which are used to
generate that point are flagged as bad.
\item Generally the calibrators in such an experiment will be
observed in a binning mode. The calibration software combines
the different bins together before determining their calibration solutions.
\end{itemize}

Obviously, however, there are times that you will want to distinguish between
the bins. The {\tt bin} visibility selection sub-command is the main
mechanism for this. The general form is:
\begin{verse}
{\tt select=bin(}{\it n1,n2}{\tt )}
\end{verse}
This selects bins {\it n1} to {\it n2} inclusive. If only a single bin
is given, then only that bin is selected -- numbering of the bins
starts at 1.

The most common places where you will want to select a subset of bins
is in the imaging and possibly self-calibration steps (the pulsar,
during its `on' phase, may make an excellent phase self-calibration source).

Although almost all visibility-data tasks support bin-mode observations,
there are a few exceptions. These give warnings when they are used on
bin-mode data. {\em These warnings should be heeded!}. The most notable task
which fails to properly handle bin-mode experiments is \task{uvaver} when
time averaging of the data is being performed.
When used to time-average, \task{uvaver} will average all the bins
together. Note, however, that \task{uvaver} works correctly (i.e. treating
the bins as separate) when no time averaging is involved.


\section{Tasks Specific to Pulsar Bin-Mode Observations}
We now mention a few bin-mode specific tasks.
\begin{itemize}
\item {Pulse profiles -- PSRPLT:}\index{psrplt}
Task \task{psrplt} produces a pulse profile (amplitude against bin number). It
can also produce a greyscale with axes of bin number and frequency and the
greyscale intensity being pulsar amplitude.

\item {De-dispersion, rebinning and timing correction -- PSRFIX:}\index{psrfix}
Task \task{psrfix} performs three related functions. De-dispersion consists of
a channel-dependent shift of bins.
\end{itemize}

