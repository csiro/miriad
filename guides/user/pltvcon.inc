\chapter{Plotting Concepts}\label{c:pltvcon}

\section{Introduction}
Display software for \miriad\ data falls into two main camps -- those that
are part of the ATNF Visualisation Suite (not strictly part of \miriad)
and those that 
use a PGPLOT device (line plots plus some image display capability).
Generally the Visualisation suite is better for interactive
work and the PGPLOT software is better for quantitative work. This
chapter will give an overview of using the PGPLOT software.

Historically there is a third camp -- the  ``TV'' software. There is
only one useful task left which uses this software, and so the ``TV''
software will be describedwith that task.

\section{The ATNF Visualisation Suite}
The ATNF Visualisation suite is a set of X-windows-based 
interactive tools which can display a wide variety of images and
cubes -- in particular \miriad's image format. You select images, display
options, etc, via pull-down menus (rather than setting parameter
values). 
\begin{latexonly}
More details can be found in the on-line documentation available on the ATNF
web pages \htmladdnormallink
{(http://www.atnf.csiro.au/computing/software/visualisation/)}
{http://www.atnf.csiro.au/computing/software/visualisation/}
or in a gzipped \htmladdnormallink{postscript}
{ftp://ftp.atnf.csiro.au/pub/software/sutra/vizdoc.ps.gz} document on the
ATNF
ftp server (vizdoc.ps.gz under pub/software/sutra, which is also available
through the ATNF web pages).
\end{latexonly}
\begin{htmlonly}
More details can be found in the on-line documentation available on the
\htmladdnormallink{ATNF web pages}{http://www.atnf.csiro.au/computing/software/visualisation/}, or
in a gzipped \htmladdnormallink{postscript}
{ftp://ftp.atnf.csiro.au/pub/software/sutra/vizdoc.ps.gz} document on the ATNF
ftp server.
\end{htmlonly}

See Appendix~\ref{app:setup} to set-up your account to use the visualisation
software.

To run a tool, type the program name, no arguments are
required. You do not need to be running the {\tt miriad} front-end
to use the tools.

\section{Using X Windows}
The ATNF Visualisation software and some of the PGPLOT devices
work via X-windows. For these to work, you must ensure that you have
setup your environment to allow communication with your X server.
In particular

\begin{itemize}
\item You should be working at a workstation running X windows!

\item You do not need to run the display tasks on the same
computer as the X-windows server.  However if you are running on a
different machine, then you will need to tell the display software
which machine is
running your X server.  To do this, issue the command
\begin{verse}
{{\tt\%\ setenv DISPLAY }{\it localhost}{\tt:0}}
\end{verse}
to the machine you are running
\miriad\ tasks on.  Here {\it localhost} is the name of the host machine
running X windows.  Normally this will be you local workstation. 

\item X windows has a simple security feature to prevent arbitrary
people drawing plots on your screen.  To authorise a particular machine
to send commands to your X server, you will need to issue the command
\begin{verse}
{{\tt\%\ xhost +}{\it remotehost}}
\end{verse}
to your local host.  Here
{\it remotehost} is the name of the host machine that you want to grant
permission to.  For \miriad\ applications, this will be the host you are
running the plotting task on.  

\end{itemize}


\section{PGPLOT Plotting Devices}
Tasks which use the PGPLOT software are always use the {\tt device}
keyword allow you to select a plotting device. Only a brief review of PGPLOT, from a users
prospective, is given here. A more complete description, both for users and
programmers, can be found in the
\htmladdnormallink{PGPLOT manual}{http://astro.caltech.edu/\~{}tjp/pgplot/}.

PGPLOT devices are specified in the standard PGPLOT manner, namely
\begin{verse}
  {\tt device=}{\it name}{\tt /}{\it type}
\end{verse}
The {\it name} part either gives a device name (usually for graphics
devices) or a file name (usually for hardcopy devices, such as
postscript printers).  In the case of a file name, any normal file name
can be given.  However if it contains a \verb+/+ character, then the
entire file name should be enclosed in double quotes (\verb+"+).  Once a
file has been created, you will generally have to issue operating system
commands to spool this to the appropriate output plotter, etc.  The {\it
name} part can often be left blank -- it defaults to something sensible. 
The most common case where you do not let it default is when you are
specifying a disk plot file. 

The {\it type} part tells PGPLOT what sort of graphics or hardcopy
device is being used. Minimum match is used. Some possible types are:

\begin{description}

\item[{\tt xs}] This is a re-sizable and persistent X window.

\item[{\tt xw}] This is a transient X window.

\item[{\tt xd}] This is the {\tt pgdisp} X windows server (deprecated). 

\item[{\tt ps}] Postscript. PGPLOT will write the plot as a disk file
in postscript form. At ATNF sites, you can print this postscript file with
the command:
\begin{verse}
{{\tt\%\ lp }{\it filename}}
\end{verse}

\item[{\tt vps}] Vertical postscript. The only difference between this and
{\tt ps}, is that {\tt vps} generates a plot in portrait mode, whereas
{\tt ps} is in landscape mode.

\item[{\tt cps}] Colour postscript in landscape mode.  

\item[{\tt vcps}] Colour postscript in portrait mode.

\item[{\tt null}] The null device.  Useful for debugging when you
do not have a display.

\item[{\tt re}] A VT100 with RETROgraphics card.

\item[{\tt v603}] A V603 terminal.

\item[{\tt krm3}] A Kermit 3 IBM-PC terminal emulator.

\item[{\tt tek}] A TEK 4010 compatible terminal (or window).


\end{description}


Examples of PGPLOT devices are:
\begin{itemize}
\item {\tt device=4/xs} directs output to the persistent X window number 4
(you can have many of these windows)
\item {\tt device=/xw} directs output to the transient X window.
\item {\tt device=plot.ps/ps} create a postscript plot file called {\tt plot.ps}
\item {\tt device=/tek} assumes you are working at a Tektronix 4014 terminal. Graphics
output is directed to your terminal.
\end{itemize}

Another useful pseudo-PGPLOT device is {\tt ?}. This causes a complete list
of PGPLOT types to be printed, and then the task will prompt you to give
a PGPLOT device.

When plotting in an X window, you must meet the conditions which allow
PGPLOT to communicate with your X server.

PGPLOT has two X windows servers, and as of PGPLOT V5.0, one of which
({\tt pgdisp}) is deprecated.  The PGPLOT device types {\tt /xs} and
{\tt /xd} are now mediated by a server (called {\tt pgxwin\_server}). 
This server is automatically started whenever either of these devices is
invoked.  The {\tt /xs} windows are permanent and resizable whereas the
fixed size {\tt /xw} windows disappear as the application terminates. 
This server allows multiple windows and you can choose to write to
whichever one you like (see above example).  The resources for these
windows are managed through the standard X windows resource file.

The deprecated server, {\tt pgdisp}, which creates a resizable
persistent X window is started by hand with a command such as

\begin{verbatim}
% pgdisp &
\end{verbatim}

from a workstation window (usually the console window). This window
is written to with the keyword {\tt device}, see below).  This
window can be used for line graphics as well as grey scales.  By
default, the {\tt pgdisp} window only has 16 colour levels, which is
insufficient for most image display applications, but fine for line
graphics. 

If you start it up with the command

\begin{verbatim}
   % pgdisp -lineColors 128  &
\end{verbatim}

then the window will be allocated 128 colours which is sufficient for
imaging applications.    This window is much slower than the windows 
mediated by the {\tt pgxwin\_server}. In addition you can only have
one.  


