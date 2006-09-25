\chapter{Using xmtv and xpanel}\label{app:xmtv}

\section{Using xmtv}
TV-related tasks use the {\tt server} keyword to indicate the TV
type and its host. For \verb+xmtv+, this is given in the form:
\begin{verse}
{\verb+xmtv@+{\it host}}
\end{verse}
Here {\it host} is
the host name of the workstation running {\tt xmtv}.
For example
\begin{verbatim}
   server=xmtv@tucana
\end{verbatim}
indicates that {\tt xmtv} is running on the machine \verb+tucana+.

The TV server display can be resized, zoomed, panned and also offers the
capability to change the lookup table and transfer function.  However,
the current \miriad\ TV server implementation is not very well
integrated with concepts such as extracting quantitative information
from the display.

Note that the computer where you are running \miriad\ tasks may differ
from the computer running {\tt xmtv} (e.g. you may be running \miriad\
tasks on a compute server such as \verb+kaputar+ or \verb+raptor+, and running {\tt xmtv} on
your local workstation).
The {\em host} part of the \verb+server+ parameter is the machine
running {\tt xmtv}.

As {\tt xmtv} uses a specific internet port, normally only a single instance of
{\tt xmtv} can run on a given machine at a given time. This is why you should start {\tt xmtv}
on your {\em local} workstation, rather than a compute server.
If this is not possible, then you should attempt to run it on the compute server.
If you get a message such as:
\begin{verbatim}
XMTV:MakeLink - bind error (INET): Address already in use
initializeSocket - MakeLink error: Address already in use
\end{verbatim}
then the internet port is already in use. In this case, you can tell {\tt xmtv} to use
a different port (e.g. port 5001) with
\begin{verbatim}
   % xmtv -port 5001 &
\end{verbatim}
(substitute another number for 5001 if it still fails). If you use a port number other than
the default, you will need to give this in the {\tt server} keyword, e.g. for port 5001 on
machine \verb+tucana+, use
\begin{verbatim}
   server=xmtv:5001@tucana
\end{verbatim}
  

\section{The Control Panel}
A few tasks (notably \task{tvflag}, but also \task{tvdisp} in its movie
mode) can use a ``control panel''. This is a panel of buttons that
pops up, with each button performing some interactive function associated
with the TV device. Although always used in unison with the TV device,
this control panel is strangely a  separate executable, which  must also
be started by hand. To do this, give the command
\begin{verbatim}
   % xpanel &
\end{verbatim}
to the same computer that you are using for {\tt xmtv}.

