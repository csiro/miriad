\chapter{Preparing Your Data in AIPS}\label{c:oldcal}
If you feel that you would rather load and flag your data within \aips\
but calibrate within \miriad, a number of steps need to be performed:
\begin{enumerate}
\item
The \aips\ task to load RPFITS data is \task{ATLOD}. Copious information
on this is
given in Neil Killeen's `Analysis of Australia Telescope Compact Array Data'.
However, before loading your data into \aips, if
you have measured all four polarisation correlations, it is best to do a 
preliminary run of \task{ATLOD} using the {\tt optype ='sysc'} option.
\index{calibration, XY phases}\index{XY phases}
This run writes a
text file (\verb+XYPHS_+{\it xx}, where {\it xx} is your \aips\ number)
containing the \MM{XY} phase for each antenna into the FITS area ({\tt 
/DATA/FITS}). Text files of system temperature are also written.
Although the various selection parameters of \task{ATLOD} are still active,
you probably want to see all the data. Most of the other \task{ATLOD}
parameters are unimportant for this.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf AIPS/ATLOD}} \\
\hline
optype = 'sysc'  & Load the data \\
freqsel		   & Select all data \\
ifsel		   &  \\
source		   &  \\
timer         	   &  \\
\hline 
\end{tabular}
\end{center}

You should plot the phases (and the system temperatures) with the Unix program
{\tt pltsys} -- which prompts you for the name of the text file to plot as
well as other information. Examine these plots carefully to assess
their quality.  You should use these plots to choose your reference
antenna (for calibration purposes).  Choose the reference antenna to be the
antenna having the cleanest, most stable \MM{XY} phase measurements.

Determine some mean value of the \MM{XY} phase for each antenna from
the plots.  The command {\tt pltsys} prints out both the average and
median \MM{XY} phase. As there are often outliers, the median is more likely
to reflect the true \MM{XY} phase value. Getting a good value is only
important for the reference antenna. Do not be too concerned if there
are large jumps in the \MM{XY} phases on antennas other than the reference
antenna.

%\addtocounter{enumi}{1}
\item%[\arabic{enumi}a.]
{\em No \MM{XY} Phase Correction in {\tt ATLOD}:}
You are now confronted with the decision of where to correct the \MM{XY}
phase of the reference antenna. Your choice will depend on taste,
circumstances and the quality of the \MM{XY} phase measurements. There are
three main options:
\begin{itemize}
\item Correct using the \miriad\ task \task{atxy} (described later). This
is generally the best option.
This allows you to correct for the
variation of \MM{XY} phase with time. This will be essential if there is
significant time variability. To use this approach, you
will need to keep a copy of your \MM{XY} phase text file, \verb+XYPHS_+{\it xx}.
\item Correct in \aips\ {\tt ATLOD}. This has been the approach once
recommended, and is still useful if the \MM{XY} phase of at least
one antenna is quite constant with time. It also has the advantage of
getting the \MM{XY} correction step over and done with early. However the
user input can be error prone and tedious if the observation switches
frequency with time.
\item When only \MM{XX} and \MM{YY} correlations have been measured, absolute
\MM{XY} phase 
becomes irrelevant. Indeed it is not measured. In this case you do not
need to apply any \MM{XY} phase correction.
\end{itemize}
There are a number of other possibilities, which will not be described here.

If you are going to correct the \MM{XY} phases in \miriad, or if you are not
going to correct \MM{XY} phase at all, then
you should now load your data {\em without applying any \MM{XY} phase}.
It is probably worth your while to pretend that the polarisations are
circular rather than linear with the usual fudges, as not all the
\aips\ software will recognise linears (most of the calibration software
will). You must {\em not} convert to Stokes parameters. 
The appropriate {\tt ATLOD} parameters are

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf AIPS/ATLOD}} \\
\hline
optype = 'load'  & Load the data \\
aparm(1) =-1    & Label as circular \\
cparm(5) =0      & Do {\em not} apply any \MM{XY} phases.\\
\hline 
\end{tabular}
\end{center}

\item%[\arabic{enumi}b.]
\index{calibration, XY phases}\index{XY phases}
{\em Correcting \MM{XY} Phases with {\tt ATLOD}:} If you want to correct the \MM{XY} phases with \aips\ {\tt ATLOD},
the \MM{XY} phases on at least one antenna should be reasonably constant with
time (vary by no more than a few degrees). In this
case, give {\tt ATLOD} the values of the
\MM{XY} phases that you determined from the plots discussed above.
Input these values into {\tt ATLOD} with the {\tt xyphase} array.  You
must enter one value per antenna for each frequency.  If you have more
than one frequency, you must enter \MM{XY} phases for all six
antennas, even if you do not have antenna 6 in the array during the
observation (the \MM{XY} phase value is not important for this
antenna, of course).  Note that even if the values are close to zero,
you still should apply them. {\em Applying a value of
zero is different from not applying anything}. 
Again, you must {\em not} convert
to Stokes parameters. 

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf AIPS/ATLOD}} \\
\hline
optype = 'load'  & Load the data \\
aparm(1) =-1    & Label as circular \\
cparm(4) =1      & Use {\tt xyphase} array and \\
                   & not the on-line values \\
cparm(5) =1      & Apply \MM{XY} phase to \MM{Y} gains \\
xyphase            & Assign the \MM{XY} phases here \\
\hline 
\end{tabular}
\end{center}


\item Now flag the data in the way you would normally do with the
\aips\ tasks {\tt SPFLG}, {\tt TVFLG}, {\tt IBLED}, and {\tt UVFLG}.
Using {\tt SPFLG} is highly recommended, particularly at 20 and 13~cm, to
check for interference.

For continuum work,
to save disk space and to speed access to the data, you may consider
averaging your channels together to form ``channel-0''
datasets, using task {\tt AVSPC}. While this causes very little degradation
for
3 cm observations, forming ``channel-0'' results in bandwidth smearing
in 13 and 20 cm observations, and so is probably inadvisable for
high dynamic range work there. For high dynamic range work at 6~cm, it is
debatable whether averaging is detrimental. If in doubt, do not average. It
is always possible to form a channel-0 dataset later anyway.

One other consideration in determining whether or not to average is whether
or not you are going to apply \MM{XY} phase corrections with task \task{atxy}.
For obscure reasons, if you used \aips\ {\tt ATLOD},
\task{atxy} needs to know the ``sideband indicator''
of the data. The sideband indicator, which is $\pm 1$, is copiously
reported by {\tt ATLOD}, both in its output to the terminal, and in the
history file. The sideband indicator also happens to be the sign of the
channel frequency increment. This is how \task{atxy} normally determines
them.
However, if you form a channel-0 dataset, the sign
of the frequency increment is lost! So if you give \task{atxy} a channel-0
dataset, you will also have to tell it the sideband indicators. 
You must give it a sideband indicator for each IF. Provided the
sideband indicator remains constant with time, this is little more than
an annoyance. However if the sideband indicator varies with time, you
are in some trouble. Overall it is best not to form
channel-0 datasets if you used \aips\ {\tt ATLOD} and you are going to
use \task{atxy}.

After flagging (and possibly averaging), write
your data as a FITS file using {\tt FITTP}. It is probably most
convenient to
write out a multi-source file. At this stage you have no
calibration -- only flagging tables (which you can apply in \miriad\ later).

\item Read the data into \miriad\ using \task{fits}. Task
\task{fits} does {\em not} apply \aips\ flagging tables ({\tt FG} tables). Instead
you have to use another task to do this -- \task{fgflag}.
Tasks \task{fits} and \task{fgflag} are discussed in
Chapters~\ref{c:inout} and
\ref{c:edit} respectively, although they are usually fairly straight forward. An
exception is for spectral line observations, where the velocity system
should be defined with \task{fits} -- see Chapter~\ref{c:lineobs}.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf FITS}} \\
\hline
in=MULTI.FITS	   & FITS multi-source file to be loaded into \miriad\\
op=uvin	   & Read uv data in\\
out=multi.uv	   & The output \miriad\ dataset.\\
\hline 
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf FGFLAG}} \\
\hline
vis=multi.uv	   & Apply \aips\ flagging table to the data.\\
\hline 
\end{tabular}
\end{center}

At this stage it is worth running
\task{uvindex}. This produces a summary of your dataset, which
you should probably save in a log file. Inspect this summary
carefully, particularly the frequencies (especially in \task{fits}
complained about inconsistent frequency definitions). If the frequency
information looks incorrect, read Chapter~\ref{c:inout} more carefully
and/or seek help.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf UVINDEX}} \\
\hline
vis=multi.uv	   & Dataset to summarise. \\
log=multi.log   & Output log file.\\
\hline 
\end{tabular}
\end{center}


\item Skip \index{calibration, XY phases}\index{XY phases}\index{atxy}
this step if you have only measured two polarisation products.
Otherwise now is the time to apply your \MM{XY} phases to the data if you
have not already done so with \aips\ {\tt ATLOD}. As mentioned above,
the task to do this is
\task{atxy}. If you used \aips\ {\tt ATLOD}, you should also have
\MM{XY} phase text file that it produced. 
Let us discuss the various input parameters:
\begin{itemize}
\item {\tt vis:} The name of the input dataset. Generally this will
be a multi-source dataset. 
\item {\tt xyphase:} You need
to give the name of the \MM{XY} phase text file that \aips\ {\tt ATLOD}
produced.
Generally this will be of the form \verb+XYPHS_+{\it xx},
where {\it xx} is your \aips\ user number.
Task \task{atxy} can also be used to correct the \MM{XY} phases of
data loaded with \miriad\ \task{atlod} where \verb+options=xycorr+
was not used.
In this case, the on-line \MM{XY} phase measurements are contained
within the dataset (as the variable \verb+xyphase+), and you do not require
a input text file.
\item {\tt refant:}
It is best to correct for a time varying \MM{XY} phase on only one
antenna. All other antennas are assumed to have an \MM{XY} phase of zero (in
the \aips\ \MM{XY} phase convention -- see below).
The antenna which is corrected for a time varying \MM{XY} phase
should be the antenna
with the cleanest, most constant \MM{XY} phase (as determined by the {\tt pltsys}
or \task{varplt} 
plots). This will be the antenna you will use as the reference antenna for
calibration purposes. It is not necessarily the same as the antenna
used as the reference during the observation -- although it will often
happen that it is the same. You give this antenna via the {\tt refant} keyword.
Note that if you have broken a dataset up into sub-files before using
\task{atxy} (e.g. break it up into a calibrator dataset and a program
source dataset), then you {\em must} correct the same antennas in
all datasets.
\item {\tt interval:} This gives one or two numbers, both in minutes,
which determine
the length of a solution interval (the time interval over which an \MM{XY} phase
is solved for; should be comparable to the time scale on which the
\MM{XY} phases are constant over -- use \task{varplt}). The first number gives the maximum length of a solution
interval, whereas the second gives the maximum gap within a solution
interval. A new solution interval is started when either the maximum
time length is exceeded, or a gap larger than the maximum gap is encountered.
The default maximum length is 30 minutes. The default maximum gap is the
same as the maximum length.
\item {\tt break:} If significant steps in the \MM{XY} phase occurs during
the observation (usually caused by resetting the delays), then it is
best to prevent a solution interval spanning this time. To do this, you
list the times where there was a break in the \MM{XY} phase.
The times are given in the normal \miriad\ time format (i.e. either
{\it hh:mm:ss} or 
{\it yymmmdd:hh:mm:ss}, such as 93oct18:19:21:00, for 7:21 pm on 18 October,
1993).
\item {\tt sideband:} As noted above, \task{atxy} needs to know the
sideband indicator for for data
loaded with \aips\ {\tt ATLOD}. For multi-channel data, this will
be the sign of the frequency increment. However this sign will be
lost if you form a channel-0 dataset. In this is what you have done,
you will need to tell \task{atxy} the sideband indicators for each
IF band. Note that \task{atxy} cannot cope with a channel-0 
dataset if the sideband indicators change with time. 
\item {\tt out:} The name of the output dataset. Apart from application
of the \MM{XY} phase, this will be a copy of the input dataset.
\end{itemize}
Typical inputs to \task{atxy} are given below

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf ATXY}} \\
\hline
vis=multi.uv	   & The input dataset.\\
xyphase=XYPHS\_56 & The input \aips\ {\tt ATLOD} \MM{XY} \\
		   & phase text file, or\\
xyphase		   & leave unset if \miriad\ \task{atlod} was used.\\
refant=3	   & Correct antenna 3 in time varying way.\\
interval=\#	   & Solution interval. Default is 30 min\\
sideband	   & Sideband indicator. Leave unset for\\
		   & multi-channel data.\\
out=multi.uvxy	   & Output \MM{XY} phase corrected data.\\
\hline 
\end{tabular}
\end{center}

\end{enumerate}
