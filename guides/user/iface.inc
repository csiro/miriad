\chapter{General Information}

\begin{latexonly}
This manual is also available, in hypertext form, on the World
Wide Web at 
\begin{verse}
{\verb+http://www.atnf.csiro.au/computing/software/miriad+.}
\end{verse}
This also contains information on retrieving and installing \miriad.
\end{latexonly}

This manual serves as a reference guide for the
\miriad\ package. It is assumed that the reader has some familiarity
with the underlying operating system. Before proceeding further you need
to read Appendix~\ref{app:setup} on how to set up your account in order
to use \miriad.

The manual is split into two basic parts.  The first part (Chapters 2 to 6)
introduces you to the \miriad\ package and its general concepts.  The second
part (Chapters 7 to 22), which
uses a cookbook approach, takes you through the steps of putting your data
on the local disk and reducing them. 


If this is your first encounter
with the \miriad\ package, we recommend that you read through Chapters
\ref{c:iface} and \ref{c:managing} carefully.

For those interested, an overview of some of the history and design
decisions of \miriad, the `standard reference' is: Sault R.J., Teuben P.J.,
Wright M.C.H., 1995, \htmladdnormallink{``A retrospective view of Miriad''}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1995ASPC...77..433S}
in {\it Astronomical Data Analysis Software and
Systems IV},
ed. R. Shaw, H.E. Payne, J.J.E. Hayes, ASP Conf. Ser., {\bf 77}, 433-436.

Please send any comments or suggestions to\\
\htmladdnormallink{\verb+miriad@atnf.csiro.au+}{mailto:miriad@atnf.csiro.au}.

\section*{Recommended Reading}
The following books tend to concentrate on the techniques of
radio astronomy, and particularly radio interferometry.

\begin{itemize}
\item Synthesis Imaging in Radio Astronomy II, G.B Taylor, C.L. Carilli, \& R.A. Perley eds,
Astronomical Society of the Pacific Conference Series Volume 180.

\item The Fourier Transform and its Applications, R.N. Bracewell.

\item Interferometry and Synthesis in Radio Astronomy, A.R. Thompson,
J.M. Moran \& G.W. Swenson.

\item Radio Astronomy, J.D. Kraus.

\item Radiotelescopes, W.N. Christiansen \& J.A. H\"ogbom.

\item Tools of Radio Astronomy, K. Rohlfs \& T.L. Wilson.

\end{itemize}

A general description of the science that can be done with
radio telescopes is given in the following books.
\begin{itemize}

\item Galactic and Extragalactic Radio Astronomy, G.L. Verschuur
\& K.I. Kellermann eds.

\item An Introduction to Radio Astronomy, B.F. Burke \& F. Graham-Smith.

\end{itemize}

The following are more specific to the ATCA and the practicalities 
of observing and data reduction.

\begin{itemize}
\item  
\htmladdnormallink{
Journal of Electrical and Electronics Engineering, Australia: Special
Issue}{http://www.narrabri.atnf.csiro.au/observing/iree.pdf}
-- The Australia Telescope, Volume 12, Number 2, June 1992.

\item Australia Telescope Compact Array User's Guide \\
\htmladdnormallink{
http://www.narrabri.atnf.csiro.au/observing/users\_guide/html/atug.html}
{http://www.narrabri.atnf.csiro.au/observing/users_guide/html/atug.html}

\end{itemize}
A large amount of can be found on the ATNF web sites
\begin{verse}
  \htmladdnormallink{http://www.atnf.csiro.au}{http://www.atnf.csiro.au}\\
  \htmladdnormallink{http://www.narrabri.atnf.csiro.au}{http://www.narrabri.atnf.csiro.au}
\end{verse}

\section*{Acknowledgements}
Most of this manual was written by 
Bob Sault (\htmladdnormallink{rsault@atnf.csiro.au}{mailto:bob.sault@csiro.au}) and
Neil Killeen (\htmladdnormallink{nkilleen@atnf.csiro.au}{mailto:neil.killeen@csiro.au}).
Other parts of this manual were written by 
Peter Teuben (\htmladdnormallink{teuben@astro.umd.edu}{mailto:teuben@astro.umd.edu}
-- University of Maryland),
Keith Jones (\htmladdnormallink{jones@galaxy.physics.uq.oz.au}{mailto:jones@galaxy.physics.uq.oz.au}
 -- University of Queensland) and 
Tom Oosterloo (\htmladdnormallink{oosterloo@nfra.nl}{mailto:oosterloo@nfra.nl} 
-- Netherlands Foundation for Research in Astronomy).

\newchapter
\chapter{The User Interface}
\label{c:iface}
\section{Introduction}
This chapter describes the user interfaces to 
\miriad.
Each \miriad\ task has a number of parameters which can be specified.  With
some parameters, you
must assign values to them when you run
the task.  With other parameters, if they are not set, a default value will
be used, which may or may not be sufficient to run the task successfully.

There are two approaches to running tasks: the command-line approach and
the front-end (or shell) approach.  
With the command-line approach, you give the \miriad\ command directly
at the system prompt, in the same way as any other operating system
command, such as {\tt ls}. 
With the front-end approach, you interact with another program, which
aids you in forming the command-line to a \miriad\ task.

First we will describe the {\tt miriad} front-end, then
command-line interface will be discussed
Finally we discuss using scripts with \miriad.

If this is your first encounter with \miriad, we suggest you work
your way through the examples starting in the next section.

\section{The {\tt miriad} Front-End}
For involved tasks, giving all the parameters are the hosts
command line becomes far too cumbersome. In this case,
you can use the {\tt miriad} front-end\index{miriad, front-end},
which has a mentality somewhat similar to the \aips\ user interface.
To invoke {\tt miriad}, type:

\begin{verbatim}
   % miriad
\end{verbatim}
The usual system prompt will be replaced by the {\tt miriad\%} prompt:
\begin{verbatim}
   miriad%
\end{verbatim}
and {\tt miriad} will read a keyword file, {\tt lastexit}, with 
\index{lastexit} the values of all
parameters saved when you last exited {\tt miriad} (see EXIT below).
This file will be created/read in from your current working directory.

We shall now describe all {\tt miriad} commands in more detail. A summary
of the commands, and their syntax, is given in Table~\ref{t:miriad1} at
the end of this section. Commands not known to {\tt miriad} are simply
passed to your host operating system. This means standard commands, such
as {\tt ls}, are still usable
and valid.

\subsection*{INP, GO and TASK} 

To inspect the inputs of a task, as well as to select the task, {\it e.g.}
{\tt histo} type
\begin{verbatim}
   miriad% inp histo
\end{verbatim}
{\tt miriad} will 
show the parameters of the task along with the values, if any, 
previously set.  For example, if, the first time you run {\tt miriad}, you type:
\begin{verbatim}
   miriad% inp histo
\end{verbatim}
{\tt miriad} will reply by writing
\begin{verbatim}
   Task:   histo
   in       =
   region   = 
   range    =  
   nbin     = 
\end{verbatim}
and will replace the {\tt miriad\%} prompt with a task prompt
\begin{verbatim}
   histo%
\end{verbatim}
indicating that you have chosen the task {\tt histo}.  You can also choose the
task {\tt histo} without using {\tt inp} by typing:
\begin{verbatim}
   miriad% task histo
\end{verbatim}
{\tt miriad} will then replace the {\tt miriad\%} prompt with the 
{\tt histo\%} prompt, but
the inputs will not be printed out.  Either way
a parameter can be set with:
\begin{verbatim}
   histo% in=gauss
\end{verbatim}
Retyping:
\begin{verbatim}
   histo% inp
\end{verbatim}
will result in {\tt miriad} replying:
\begin{verbatim}
   Task:   histo
   in       = gauss
   region   =  
   range    =  
   nbin     =      
\end{verbatim}
Tasks are run either by typing {\tt go} {\it taskname} at 
the {\tt miriad\%} prompt
(advisable only if you know you like the inputs) or by typing {\tt go} at the
{\it taskname}{\tt \%} prompt.  Thus, in the above example, typing:
\begin{verbatim}
   histo% go
\end{verbatim}
would result in the task {\tt histo} running with {\tt in=gauss} as the only
assigned parameter (all the other parameters would be set to their default
values).  

Any task can be run regardless of the chosen task, merely by typing:
\begin{verbatim}
   miriad% go itemize
\end{verbatim}
{\tt miriad} then executes the task {\tt itemize}
(using whatever input parameters it finds from the {\tt lastexit} file
or a previous run) and changes the default 
task and prompt to:
\begin{verbatim}
   itemize%
\end{verbatim}

Generally {\tt miriad} waits for the task to finish before it resumes. The
{\tt -b} can be used to tell the operating system to put the task in
the background\index{background}, and allow {\tt miriad} to be ready
immediately for new commands. However the output of the task just started
may still return output to the terminal. You can start as many jobs as the
operating system will allow.

\subsection*{HELP}

The help command is generally used to get information about a task.
The general format of the command is:
\begin{verse}
{{\tt miriad\%\ help }[{\tt -w}] {\it taskname } [{\tt -k }{\it keyword}]}
\end{verse}
which will give you information about the keyword of the given task.
If the \verb+-k+~{\it keyword} is omitted, information is given about all
parameters of the task. If the task is omitted, information is given
about the current task. For example:
\begin{verbatim}
   miriad% help histo
\end{verbatim}
will give you help information about the histogramming task, {\tt histo}.

The \verb+-w+ flag causes output to be directed to the {\tt netscape}
web browser.

In addition to tasks, {\tt help} will give you information about a few
other select topics. These include information on tasks in general
({\tt help tasks}), on {\tt miriad} itself ({\tt help miriad}) as well as a
number of commonly used keywords ({\tt device, line, options, region, select,
server, stokes}).

By default, help information is sent to your terminal. However you can
direct it to a web browser by setting the {\bf MIRPAGER} environment
variable to either \verb+netscape+ or \verb+lynx+. You can also set
{\bf MIRPAGER}
(or environment variable {\bf PAGER})
to your favourite text paging program (the default is
{\tt more}, but you may prefer {\tt less}). Note that the web version of
the documentation usually contains extra links to other relevant material.

\subsection*{EXIT and QUIT}

{\tt exit} exits {\tt miriad}, and, 
if any parameter values have been
changed from the previous time you ran {\tt miriad}, all parameter values are
saved in a keyword file {\tt lastexit}.  
The parameter values from this file are read
in automatically when you next run {\tt miriad}.  
{\tt quit} exits {\tt miriad} without
saving the present parameter values in {\tt lastexit}.  This command is useful
if you have changed parameter values you do not wish to save.

\subsection*{Command-Line Editing and ER}
{\tt Miriad} includes a reasonably sophisticated command-line editor.
The up- and down-arrow keys allow scrolling
through the history of previous commands, and the left- and right-arrow keys
allow cursor motion within a line. Pressing the TAB key causes an attempt
to perform file-name completion. Various control keys allow extra editing.
A summary is given in Table~\ref{t:lineedit}. For copious
information, see the GNU Readline library manual.
\begin{table}
\caption[{\tt Miriad} Command line editing commands]{{\tt miriad} Command Line Editing Commands}
\label{t:lineedit}

\begin{center}
\begin{tabular}{|l|l|}\hline
Key		& Description \\ \hline
UpArrow		& Previous command \\
DownArrow	& Next command \\
LeftArrow 	& Move cursor left \\
RightArrow	& Move cursor right \\
DEL		& Delete character to left of cursor \\
TAB		& File name completion \\
\verb+^+TAB	& List possible completions \\
\verb+^A+	& Move to beginning of line \\
\verb+^D+	& Delete character underneath cursor \\
\verb+^E+	& Move to end of line \\
\verb+^U+	& Delete to beginning of line \\
\hline \end{tabular}
\end{center}
\end{table}

The {\tt er} command can be used to perform command-line editing on the
current value of a parameter. It also obeys the left- and right-arrow keys,
the DEL key etc. For example, to edit the current value of the parameter
{\tt select}, use:
\begin{verbatim}
   miriad% er select
\end{verbatim}
The {\tt er} command is not as sophisticated as the normal command-line
editor.

\subsection*{Interrupting a Task}
Control-C can usually be used to interrupt the execution of a task, and
revert back to the {\tt miriad} shell.

\subsection*{UNSET}

It is often convenient to assign
the original default value to the parameter.  This\index{unset, miriad}
`assignment' is accomplished with the command {\tt unset}.  
For example, if the
task {\tt histo} had been run with inputs:
\begin{verbatim}
   in       = gauss
   region   =  
   range    = 0,1
   nbin     = 10
\end{verbatim}
but you wanted to run histo with the default for {\tt nbin}, you would type:
\begin{verbatim}
   histo% unset nbin
\end{verbatim}
Multiple parameters can be unset on the same line.  Thus, to assign both
{\tt range} and {\tt nbin} to their default values, you would type:
\begin{verbatim}
   histo% unset range nbin
\end{verbatim}
Then, typing:
\begin{verbatim}
   histo% inp
\end{verbatim}
will result in {\tt miriad} replying
\begin{verbatim}
   Task:   histo
   in       = gauss
   region   =  
   range    =  
   nbin     =
\end{verbatim}

\subsection*{SAVE and LOAD}

As noted above, when you exit {\tt miriad}, all your inputs will be saved in
the keyword file {\tt lastexit}. However, you can save and reload your current
parameters at any time. The\index{save, miriad} command {\tt save}
writes the current parameter values to a keyword file ({\it taskname}{\tt .def} 
if the task
{\it taskname} has been selected) or to a user-specified file. The parameter
settings in this keyword
file (or a user-specified file) can be retrieved using\index{load, miriad}
the command {\tt load}. 
Note that {\tt save} writes out {\bf all} parameter values, not just those for
the specific task chosen.
Example:
\begin{verbatim}
   histo% save
   histo% save histo1.pars
\end{verbatim}
In the first case the parameters are saved in a file {\tt histo.def}, in
the second case they are saved in a user-specified file {\tt histo1.pars}.

\subsection*{TPUT and TGET}

When you invoke a task, {\tt miriad} saves all the parameters associated
with this task in a keyword file, in the directory given by the {\bf MIRDEF}
environment variable (if {\bf MIRDEF} is not set, the current directory
will be used). The parameters will be saved in a file with name
{\it tasname}{\tt .def}. Additionally you can force parameters to
be saved using `{\tt tput} {\it taskname}'. If the taskname is omitted,
the parameters for the current task are saved. The {\tt tget} command is
used to retrieve the parameters at a later stage. For example, to save and
then restore parameters, use
\begin{verbatim}
   miriad% tput histo
   histo% tget histo
\end{verbatim}
{\tt tput} and {\tt tget} normally expect the keyword files to be in the
directory given by the {\bf MIRDEF} environment variable.
This is unlike {\tt lastexit} and the {\tt save} and {\tt load} commands,
which expect the keyword files to be in the current directory. The \verb+-l+
flag to {\tt tput} and {\tt tget} changes them to write/read the keyword
file from the current directory.

\begin{htmlonly}
\subsection*{Summary of {\tt miriad}\ commands}
\end{htmlonly}

\begin{table}
\caption[{\tt Miriad} shell command overview]{{\tt Miriad} shell command overview}
\label{t:miriad1}

\begin{center}
\begin{tabular}{|l|l|l|}\hline
Command   &  Syntax             & Comments \\ \hline
=       &  key = value		& assignment (see also SET) \\
cd      &  cd directory         & change directory \\
er	&  er key		& command line editing of a parameter. \\
exit    &  exit                 & exit {\tt miriad}, and save parameters in lastexit \\
go      &  go [-b] [taskname]   & start up task \\
help    &  help [taskname] [-k key]& on line help on task\\
inp     &  inp [taskname]       & overview inputs \\
source  &  source cmdfile       & read commands from a command file\\
load    &  load [keyfile]       & load task parameters from keyword file\\
quit    &  quit                 & quit {\tt miriad}, and does not save lastexit \\
save    &  save [keyfile]       & save task parameters to keyfile\\
set     &  set key value        & assigment \\
setenv	&  setenv var value	& set an environment variable\\
task    &  task taskname        & set new default taskname \\
tget	&  tget [-l] [taskname]	& load parameters for a particular task\\
tput	&  tput [-l] [taskname]	& save parameters for a particular task\\
unset   &  unset key1 key2 .... & unset variables (blank them) \\
unsetenv&  unsetenv var		& unset an environment variable\\
view    &  view [taskname]      & edit task parameters in a keyfile\\ \hline
\end{tabular}
\end{center}
\end{table}

\begin{table}
\caption{{\tt Miriad} Environment Variables}
\label{t:miriad2}
\begin{center}
\begin{tabular}{|l|l|}\hline
Environment Variable & Comments \\ \hline
VISUAL    & Editor for editing keyword files and bug reports.\\
EDITOR    & Editor for editing keyword files and bug reports.\\
MIRPAGER  & Web browser or pager for reading help documents\\
PAGER     & Pager for reading documents\\
MIRDEF    & Directory for keyword files \\
TMPDIR    & Directory for scratch files (see Section~~\ref{s:scratch-files}).
	\index{TMPDIR, environment} \\
\hline
\end{tabular}
\end{center}
\end{table}

\section{\miriad\ on the World Wide Web}
A hypertext version of this manual, as well as other \miriad\
information, is available on the World Wide Web. Its URL is
\begin{verse}
{\tt http://www.atnf.csiro.au/computing/software/miriad}
\end{verse}


\section{Bug Reports and Giving Feedback}
Needless to say, \miriad\ is not perfect. The \task{mirbug}
command can be used if you encounter a bug, have
a question about a task, or have a suggestion. The format of the command is
\begin{verse}
{{\tt\%\ mirbug }{\it taskname}}
\end{verse}
Here {\it taskname} is the task in question. Use the name
``\verb+general+'' if the bug report is not about a specific task.
The \verb+mirbug+ command places you in an editor (given by the \verb+EDITOR+
environment variable), with a template of a bug report, which includes
the parameters most recently used for that task. After you finish
editing the template, \verb+mirbug+ mails the report.

\section{Indirect Parameter Input}
\label{s:atfile}
It is possible that the length of a parameter value can become
large (indeed quite huge). In this case, it is convenient to store the 
parameter \index{@file} value
in a file and then to use the file name in place of the parameter 
value.

To do this, the file name should be preceded by an `at' sign (\verb+@+).
For example, assuming the file {\tt name} contains the text
{\tt chicyg} , then
\begin{verbatim}
   % histo in=@name
\end{verbatim}
is equivalent to
\begin{verbatim}
   % histo in=chicyg
\end{verbatim}
These \verb+@+ files can be nested, and multiple \verb+@+ files can appear on a
line (separated by commas). Indeed an \verb+@+ file and other parameter
values can be intermixed, separated by commas.

The \verb+@+ file can contain many lines. An end-of-line is treated like
a comma.  

As mentioned before in Section~\ref{s:cmdline}, parameter input can
also be made easier by using the {\tt -f} flag when \miriad\ commands are
given directly from the shell.

\section{Host Command-Line Interface}
\label{s:cmdline}
\miriad\ tasks can always be run by specifying their
parameters on the host command line.
A parameter is specified by equating its keyword to its
value. For example, {\tt out=gauss} sets the parameter {\tt out} to
the `value' {\tt gauss}.
As an example, we shall first create an image
dataset, {\tt gauss}, with the task \task{imgen}.
At the system prompt type
\begin{verbatim}
   % imgen out=gauss
\end{verbatim}
In this case, default values are used for all parameters, other 
than {\tt out}. The names of all parameters, and their default values, are
described in a help file (see below) which is always available on line.

Next, we will run a histogram task, \task{histo}, with the input image
dataset {\tt gauss} that we just created:
\begin{verbatim}
   % histo in=gauss
\end{verbatim}

Several parameters can be given on the command line, separated by spaces.
Often a parameter value consists of several numbers and/or 
strings. These values 
should be separated by commas. For example:
\begin{verbatim}
   % histo in=gauss range=0,1 nbins=10
\end{verbatim}

Strings will often be file names. These are the names used by the host
computer, and so they should obey the file-naming rules of the computer.
Some tasks support wildcard expansion for file names.

Whether strings are in lower or upper case is usually
significant. That is,
\begin{verbatim}
   % histo in=gaus
\end{verbatim}
is very different to
\begin{verbatim}
   % HISTO IN=GAUS
\end{verbatim}
Generally lower case is preferred.

A parameter value cannot contain a space. Often a parameter value will be
composed of one or more strings out of a fixed set. In this case, the
strings can be abbreviated to the minimum number of
characters required for uniqueness.

The host's shell may try to interpret special characters in your commands --
most notably the UNIX shells treat asterisks and brackets as special. This will
often result in the host shell issuing cryptic messages and failing to
invoke the \miriad\ task. For example, giving the command
\begin{verbatim}
   % histo in=gaus*
\end{verbatim}
will probably result in
\begin{verbatim}
   No match.
\end{verbatim}
Similarly
\begin{verbatim}
   % histo in=gaus region=quarter(1)
\end{verbatim}
will result in
\begin{verbatim}
   Badly placed ()'s.
\end{verbatim}
You can avoid this by escaping the special character or quoting the text
containing the special characters. For example:
\begin{verbatim}
   % histo in=gaus "region=quarter(1)"
\end{verbatim}

The command-line interface is quite appropriate for tasks that take relatively
few parameters, or in 
shell scripts. However, it is cumbersome when a task has many parameters.
Here the use of a keyword file comes in handy:

Keyword files \index{keyword, files} are files containing 
{\tt parameter=value} pairs, one per line, 
and can be created with an editor
(most front-ends to be discussed in the next sections create such {\tt .def}
files by themselves) and passed to a \miriad\ task using the {\tt -f}
\index{command line switch}
command line switch, or flag. Hence the previous example 
would be equivalent to:
\begin{verbatim}
   % histo -f histo.def
\end{verbatim}
where the file {\tt histo.def} would contain:
\begin{verbatim}
   in=gaus
   region=quarter(1)
\end{verbatim}
Unknown keywords are simply ignored (though you will get a warning). The
file should not use the same keyword twice. As parameter values do
not go through the host's shell, special characters do not
need to be escaped or quoted.

Help on a \miriad\ task can be obtained on the host command line with the
command {\tt mirhelp}.
Example:\index{mirhelp}
\index{help, mirhelp} 
\begin{verbatim}
   % mirhelp histo
\end{verbatim}
This will provide description of the task parameters and
their default values.

The {\tt mirhelp} command can be used to save help on a \miriad\ task as
a text file, or to get a print-out of a help file. In UNIX, you would
use the normal shell output redirection and piping. For example, to
redirect the help on task \task{histo} to a file {\tt histo.hlp}, use:
\begin{verbatim}
   % mirhelp histo > histo.hlp
\end{verbatim}
Alternately, to print out this help file, use
\begin{verbatim}
   % mirhelp histo | lw80
\end{verbatim}
where {\tt lw80} is your favourite print command.

\section{Writing Shell Scripts}
As \miriad\ commands can be invoked directly from the command line, scripts
and command procedures to run a sequence of \miriad\ commands can be
developed using the normal host's facilities. This is a somewhat
advanced topic -- you will probably want to be familiar with the shell
scripts and \miriad\ before you attempt to develop your own script.

There are numerous
books written of shell programming or the like -- a manual like
this cannot be expected to cover the subject in the depth that these books
go into. Instead a simple annotated example will be given using the
C-Shell commonly used on UNIX systems. To aid description,
line numbers are given on the left side of each line (these line numbers are
{\em not} part of the shell script).
\begin{verbatim}
  1:  #!/bin/csh
  2:
  3:  # Delete any datasets called "multi.uv".
  4:
  5:  rm -rf multi.uv
  6:
  7:  fits in=MULTI.UV op=uvin out=multi.uv
  8:
  9:  foreach srcnam ( 1934-638 0823-500 vela )
 10:    uvaver vis=multi.uv "select=source(${srcnam})" out=${srcnam}.uv
 11:  end
 12:
 13:  mfcal vis=1934-638.uv interval=10 refant=3
 14:  gpcal vis=1934-638.uv interval=10 options=xyvary refant=3
 15:  gpcopy vis=1934-638.uv out=0823-500.uv
 16:
 17:  gpcal vis=0823-500.uv interval=10 \
 18:                 options=nopol,xyvary,qusolve refant=3
 19:
 20:  gpboot vis=0834-500.uv cal=1934-638.uv
 21:  uvplt vis=0823-500 stokes=i,q,u,v axis=real,imag device=0823.ps/ps
 22:
 23:  lpr 0823.ps
\end{verbatim}
\begin{itemize}
\item Line 1: C-shell scripts {\em must always} start with the rather
cryptic \verb+#!/bin/csh+. This allows the system to determine the
appropriate interpreter for the script (i.e. the C-shell).
\item Line 3: Comments are introduced by a hash character.
\item Line 5: Delete a \miriad\ dataset called \verb+multi.uv+. The
\verb+r+ flag (recursive) is needed to delete a directory (which is
how \miriad\ stores a dataset). The \verb+f+ (force) flags causes \verb+rm+ to
not complain if it fails.
\item Line 7: Execute the \miriad\ task \task{fits}. This reads a visibility
FITS disk file (\verb+MULTI.UV+), and saves it as a \miriad\ dataset
(\verb+multi.uv+).
\item Lines 9 and 11: This is the C-shell's equivalent of a DO-loop. The
values listed inside the brackets (source names in this instance) are
progressively assigned to the control variable of the loop ({\tt srcnam}
here). The net result is that this loop will be executed three times,
with {\tt srcnam} being successively set to {\tt 1934-638}, {\tt 0823-500}
and {\tt vela}. The ``things'' inside the brackets are separate by spaces -- the
C-shell generally likes spaces to separate different components of a command.
\item Line 10: The body of the loop. The {\tt uvaver} command will be
executed three times. The shell substitutes the string \verb+${srcnam}+ 
with the current value of the \verb+srcnam+ variable.
Thus the {\tt select} parameter being successively
set with {\tt source(1934-638)}, {\tt source(0823-500)} and
{\tt source(vela)} on the three times through the loop. The output dataset of the command also changes.
Note that the {\tt select} parameter is quoted (\verb+"select=+\ldots
\verb+"+).
This is as brackets (\verb+()+) are special to the shell. Quoting them
prevents this interpretation. Note however that the \verb+${srcnam}+ is
still treated as special, even though it is in quotes (software does not have
to be consistent!).
\item Lines 13-15: Execute various \miriad\ commands on data-sets produced
by line 10.
\item Lines 17-18: Yet another command. However this one was too long to
fit on one line. In the C-shell, a line can be continued onto the next by
ending it with a backslash.
\item Lines 20 and 21: More of the same. Generate a postscript output
file, {\tt 0823.ps}, using task \task{uvplt}.
\item Line 23: Spool this postscript file to the laser printer.
\end{itemize}

On UNIX systems, after having developed a script, you will need
to change the ``file mode'' of the script to indicate that the script is
executable. For example, to mark the shell script {\tt calibrate} as
executable, use the UNIX command
\begin{verbatim}
     % chmod +x calibrate
\end{verbatim}
