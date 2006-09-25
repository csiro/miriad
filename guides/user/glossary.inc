%% This file is to be included by latex in ug.tex
%
% Chapter:  Glossary
%
\chapter{Glossary}
\label{app:glossary}

The following is a loose list of terms used throughout the \miriad\ manuals,
and perhaps not always explained in detail.

\begin{description}

\item[{\bf @file}]
An @file (at-file) or `include file' is a text file containing
the value of a keyword. If multiple lines are
used, a newline is equivalent to a comma. \index{@file}

\item[{\bf breakpoint}]
A discontinuity (often in time or frequency), {\it e.g.} meant to 
signify that fitting data should be done in separate intervals.
Used in calibration software.

\item[{\bf BIMA}] Berkeley-Illinois-Maryland Association, but also:
the VAX in Maryland's Astronomy Program.

\item[{\bf C-shell}] In UNIX this is one of the possible shells that can
be used to communicate with UNIX. Other shells are {\tt tcsh}, the
T-shell, {\tt sh}, the Bourne Shell and {\tt bash}, the GNU Shell.
See also: {\bf script}. \index{script}

\item[{\bf dataset}] A \miriad\ dataset is a hierarchical
set of (data) items, and has been implemented as
a host system directory.

\item[{\bf deconvolution algorithm}] Algorithm by which remove instrumental
point spread function from an image.
{\it e.g.} {\tt clean} and {\tt maxen}). \index{clean} \index{maxen}

\item[{\bf doc file}] See `help file'.

\item[{\bf FITS}] Flexible Image Transport System. Standard data format to
interchange data between
different computers. Can be used for image as well as visibility
data. \index{fits}

\item[{\bf header variable}] Misnomer for a \miriad\ {\bf item}. Do not
confuse a header variable with a uv variable.

\item[{\bf help file}] File with extension {\tt .doc} that explains
workings of a \miriad\ task. Names of parameters and their defaults
are also in here. Help files live in directory {\bf \$MIRPDOC}.
The command {\tt mirhelp} can be used to get any help on \miriad.

\item[{\bf host}] Your local computer. The term {\it host}
is often used as in 
{\it host interpreter} and {\it host commands}, and is really meant to
warn you that in this context commands may differ depending on which
host/machine (often VMS vs. UNIX) you work.

\item[{\bf keyword file}] File containing {\it `keyword=value'}
assignments, one per line. Each user interface can read such files, although
the extension is often assumed to be {\tt .def}. See Chapter~\ref{c:iface}.

\item[{\bf image centre pixel}] This is the central pixel in an image. If
pixel numbers vary from 1 to \MM{N}, the centre pixel is pixel ${\rm int}(N/2)+1$.
This pixel should be distinguished from the `image reference pixel' (see
below).

\item[{\bf image reference pixel}] This is the pixel in an image which is used
as a reference point for the image coordinate system. If the image was produced
by task \task{invert}, or derived from an image produced by \task{invert},
the reference pixel will correspond to the same point in the sky as the
observing centre (see `observing centre' and `image centre pixel').

\item[{\bf include file}] Other name for @file

\item[{\bf item}] Name tagged data, often of the same datatype, that\index{item}
is part of a dataset. For images {\bf item}s are what we
commonly refer to as the `header variables', but also the
{\tt history} and actual {\tt image} data are items in a dataset.

\item[{\bf observing centre}] A \miriad\ uv file does not distinguish
between the pointing centre, phase centre and delay centre. All are
assumed to be the same point on the sky, and are fixed by the observation
process. Documentation refers to this point simply as the observing
centre.

\item[{\bf override}] When an item takes precedence over a uv variable.
Only applies to \miriad\ visibility datasets.

\item[{\bf PGPLOT}]  Package used to get graphical displays on
the screen or hardcopy device.\index{pgplot}

\item[{\bf PostScript file}] An ASCII text file in the
PostScript language. PostScript is a page description language, and
has become an industry standard for printing high-quality text and
graphics. One of the PGPLOT device drivers creates a PostScript
file, which can be sent to a printer to get hardcopy output.
\index{PostScript}

\item[{\bf script}] A set of commands bundled together in a text file,
which can be executed. Depending on the interpreter, command
flow and various other programming styles are possible.

\item[{\bf shell}] A supposedly easier to handle
front-end command processor used to communicate with
a lower level, and often more difficult to handle program or
operating system.

\item[{\bf spectral window}] See `window'.

\item[{\bf window}] A window, or spectral window, in \miriad\ usage, means a
sequence of frequency channels which are separated by the same frequency
increment. The Hat Creek correlator is capable of observing eight
spectral windows simultaneously, whereas the ATCA can observe
two simultaneously. This is somewhat analogous to the \aips\ `IF band'
concept.

\item[{\bf task}] A \miriad\ task is a regular executable in the
sense of the host operating system, but only accepts input parameters
via a set of {\tt keyword=value} assignments and/or {\tt -f keyfile}
keyword files, that cannot contain blanks. Help on the program can
be obtained via {\tt mirhelp} the command.

\item[{\bf uv}] 
Referring to the {\tt u}, {\tt v}, and formally {\tt w} variables
in synthesis observations, commonly called `uv'. Most often
this term is a shorthand notation where we could have used the
word `visibility'.

\item[{\bf uv variable}] 
Not to be confused with a header variable, a \MM{u-v} variable is a named
quantity with a certain type (integer, real, ...) and dimension, that
can change value during the course of an observation. 

\end{description}

