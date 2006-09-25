\chapter{Trouble Shooting}
\label{app:trouble}

This appendix attempts to suggest a few ways of troubleshooting
while running \miriad.

\miriad\ programs usually announce trouble by starting a line with
the {\tt \#\#\#} characters. If a `Fatal Error' message follows,
the program is also aborted, although in case of a `Warning'
or `Informational' message, the task continues to run, at your
own risk.

Some messages are related to the fact that errors occur in low-level
subroutines, which have no built-in way to escape back to the user
level, and be descriptive about the problem. In such cases somewhat
obscure system-like messages like {\it `No such directory'} could
be encountered, and other error messages are just what we
sometimes jokingly call {\it `pilot errors'}, where you forgot to
supply the program with an essential piece of information.

\section{CAVEATS}
\label{s:caveats}

\begin{itemize}

\item Datasets are directories, and cannot be created when they
already exist in UNIX. You should not manipulate the component files inside
a \miriad\ dataset using the hosts normal utilities. Doing so
can potentially `corrupt' the dataset.

\item Parameter values cannot contain spaces.

\end{itemize}

\section{Error messages}

\begin{itemize}

\item {\it \#\#\# Warning: Parameter {\it vis} not used or not exhausted}.
\ The parameter {\it vis} to that particular task
has been given multiple values, probably separated
by commas, or the file-wild card expansion has been used, or from
a @file. The program, however, did not read and use all of them.
For file name the common error is that the program can only
handle one file at a time.

\item {\it \#\#\# Fatal Error: No such file or directory}.
\ A typical UNIX error message. Your program was either supplied with
a non-existing file name or dataset name (remember, datasets are
really directories). Also, and this is more obscure, it could be
that for strange reasons one of the files (or items) which is 
supposed to be in the dataset directory, is missing. The file
{\tt header} must always be present in a \miriad\ dataset directory.

\item  {\it \#\#\# Fatal Error: Keyini: Input parameter too long for buffer}
\ The text you supplied to this parameter had too many characters. Try
using an @ (include) file. If string space is still exhausted your system
manager may have to increase a buffer length in the {\tt key.for}
file and recompile at least the program it failed on.

\item  {\it \#\#\# go: Failed to exec the subprocess}
\ A typical UNIX response -- totally illegible for normal humans. This means
that while trying to execute your task, UNIX found it could not do it.
There could be a lot of reasons why this was so, one of them being
the wrong machine type of the executable, 
but also not enough memory to start the task etc.

\item {\it Command not found}.
\ See Section~\ref{s:nocommand} below.

\item {\it Found no documentation on task {\tt xxx}}
\ No .doc file; either the default task in \verb+miriad+ is wrong, or
someone did not install a document file. Check if the file
{\tt \$MIRPDOC/xxx.doc} exists, where {\tt xxx} is the taskname.

\end{itemize}

\section{Command not found}
\label{s:nocommand}
A very common error made by beginning users is that
\miriad\ has not been set up properly, resulting in a 
{\it `Command not found'} message.

On UNIX this means that the
{\tt \$MIRBIN} directory must be in your search path for executables. 
\index{MIRBIN, environment}
If any of the following commands produces an {\it Undefined variable}
message or the like, check your {\tt .login} file to see if it was
\index{.login}
set up properly:
\begin{verbatim}
    % echo $MIR            # check which MIR you are using
    % echo $MIRBIN         # see if the MIRBIN makes sense
    % echo $PATH           # see if MIRBIN is inside the PATH
    % rehash               # rehash the search (assume csh is used)
    % ls $MIRBIN           # see what is present in MIRBIN
\end{verbatim}
If not, check a {\tt .login} file from one of your colleagues to see how it
should be set up. Another possibility is
that the program has not been loaded into {\tt \$MIRBIN}, but the
documentation is present, or the other way around. In both cases
the \miriad\ system manager needs to be consulted.

\section{Not a directory}

Since \miriad\ datasets are implemented as directories, 
some of the system messages may complain when  you have mixed
up a file with a dataset.

