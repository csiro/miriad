\chapter{Analysing Spectral Cubes}

\section{Introduction}
\miriad's collection of tasks to analyse spectral cubes is
of mixed quality. This chapter describes what \miriad\ has to offer.
For some purposes, a better suite of software is available in
the ATNF visualisation suite. This suite was briefly outlined in
Section~\ref{s:visualisation}.

\section{Moment Analysis}
The task \task{moment} can be used to generate moment images from
a spectral-line cube.   The moment axis should be either the first
or third in the cube (e.g. \MM{vxy} or \MM{xyv} order).  You should use
\task{reorder}
if you want the moment of the second axis for some reason.
The moments (evaluated for each spatial pixel and along the
velocity axis)  are defined as

\begin{displaymath}
M_n = \int I(v) v^n dv
\end{displaymath}

where \MM{I(v)} is the intensity at a given velocity \MM{v}.  Thus, the
zeroth moment corresponds to the integrated intensity over velocity, the
first moment corresponds to the intensity weighted velocity, and
the second moment corresponds to the intensity weighted velocity dispersion
squared.

Note that \task{moment} is actually a little inconsistent in applying
this equation depending on which moment it is working out.  Here is what
it finds:

\begin{displaymath}
M_0 = \int I(v) dv 
\end{displaymath}
in units of Jy~\kms.

\begin{displaymath}
M_1 = {\int I(v) v dv  \over {\int I(v) dv}}
\end{displaymath}
in \kms.

\begin{displaymath}
M_2 = \sqrt{{\int I(v) (v-M_1)^2 dv \over {\int I(v) dv}}}
\end{displaymath}
in \kms.

In the example we compute the second moment of a cube in \MM{xyv}
order excluding all pixels below 3-\ssigma\ which happens
to be 2~mJy it seems; there is no point to adding noise
to our sums.  We select only the inner quarter of
each spatial plane and we select only planes 40 to 480 for
this analysis.


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf MOMENT}} \\
\hline 
in=ngc253.icln      & Input cube \\
region=quarter(40,480) & Select region \\             
out=ngc253.m2          & Output image \\
mom=2                  & Second moment \\
axis=3                 & Cube in xyv order \\
clip=0.002             & Clip below this value \\
\hline 
\end{tabular}
\end{center}


\section{Smoothing the Velocity Axis}
The task \task{hanning} allows you to Hanning
smooth the velocity of a cube (regardless of which axis it is)
and write out the smoothed data set.  Any blanked
pixels are treated as zeros.

\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf HANNING}} \\
\hline 
in=ngc253.icln      & Input cube \\
region              & Unset for full cube \\
out=ngc253.icln-hann  & Output image \\
width=3              & Standard Hanning smooth \\
\hline 
\end{tabular}
\end{center}

Having produced the smoothed cube, you could then drop
every other channel (as they are no longer independent)
with \task{imsub}.  In the example, we assume the
velocity axis is the third axis.


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf IMSUB}} \\
\hline 
in=ngc253.icln-hann     & Input cube \\
out=ngc253.icln-hann-ind  & Output image \\
region                   & Unset for full cube \\
incr=1,1,2               & Pick out every other pixel on third axis \\
\hline 
\end{tabular}
\end{center}


\section{Velplot}
\task{velplot} is a many faceted task for display and analysis of
velocity fields -- read the \miriad\ help file for \task{velplot}.
\task{velplot} differs in its
mode of operation from most other \miriad\ tasks in that a small portion only
of the inputs are given in the usual \miriad\ form. The main body of the inputs
are given by the user interactively after selection from an initial menu.

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf VELPLOT}}\\
\hline
in=data.cube		&Input data cube to be examined.\\
device=/xw		&Screen is the initial plot device\\
region=box(32,32,96,96)(5,60)	&Examine the centre of planes 5 to 60\\
log=data.log		&Text output where appropriate will go here\\
\hline
\end{tabular}
\end{center}

Now run \task{velplot} and the following main menu will appear:

\begin{verbatim}
         MAIN MENU
   Comment   Write comment into log
   List      List header and velocity information
   Integral  Integrated flux and statistics
   Options   Select plot parameters
   Pos-Vel   Plot versus position & velocity
   Spectra   Plot spectra
   Vel-map   Plot integrated velocity maps
   File      Write spectra & position-velocity file
   Read      Read spectra & position-velocity file
   Write     Write Miriad Image to disk file
   Exit      Exit from program

  Select option (type 1st character) :
\end{verbatim}

This selection is not case sensitive, and only the first character
is used. This is what the options are :
\begin{description}
\item[Comment] A comment will be written into the ASCII file. It goes
like this:
\begin{verbatim}
   Select option (type 1st character) : c
   Enter comment > It's time I got to work
         comment > I had cereal for breakfast
         comment >
\end{verbatim}
You need to get used to the \task{velplot} input style. You entered 2 lines
of comment in response to the \verb+>+ prompt. A carriage return on the third
line returned you to the \task{velplot} main menu. If you exit now and examine
the file data.log you will find it contains header information from
\verb+data.cube+ plus those two comment lines.

Also be aware that for many \task{velplot} inputs gives
a carriage return/line feed after the question and you are faced
with entering answers on a blank line. This can be quite disconcerting
if you do not anticipate it.
\item[List] List header and velocity information. The information is written
into \verb+data.log+ a 4 line summary about \verb+data.cube+
\item[Integral]  Integrated flux and statistics. This lists statistics for each
data plane specified in the \verb+region+ originally selected.
The headings are: plane (number), Velocity, Total Flux, Maximum, Minimum,
Average
and rms. Unfortunately this list is not written in to the log file.
\item[Options] Select plot parameters. This is what happens:
\begin{verbatim}
   Selection (type 1st character) :o
 
      OPTIONS MENU
   Select alternative options [1/2] for the following

   Units for display [J/K]..... J
   Negative contours [Y/N]..... Y
   Plot header [Y/N]........... Y
   Write map to file [Y/N]..... N
   Absolute coordinates [Y/N].. N
   Integer plot [Y/N].......... N
   Spectra positions [Y/N]..... Y
   Fit Gaussians [Y/N]......... N
   Overlay Gauss Fits [Y/N].... Y
   Gray Scale [Y/N]............ N
   Exit from options menu
   Contour levels:  percent
    level( 1) =    15.000
    level( 2) =    30.000
    level( 3) =    45.000
    level( 4) =    60.000
    level( 5) =    75.000
    level( 6) =    90.000

  Select option (type 1st character,<cr> for options) :u
  -plot units are now Janskys

  Select option (type 1st character, <cr> for options) :e
\end{verbatim}
{\em Now the main menu will reappear}. It is easy to get confused here between
the main menu and this options menu that selects plot parameters.

The contour setting option is of particular interest in making 
contour plots. This is the equivalent of the \verb+levs+ input to \miriad\ 
\task{cgdisp}.
\item[Pos-Vel] Plots a position velocity map. This is best described by walking
through an example. We want a position velocity map made along the major axis
of the galactic disk in data.cube. The position angle of the major
axis is 40 degrees (measured north to east).

Run \task{velplot} and the main menu appears.
Editorial comments below are in italics

\begin{verbatim}
   Select option (type 1st character) :p
   Plot intensity versus position & velocity along selected cuts
   >Type H for help, <cr> to continue :<cr>
   --- no current selection of cuts ---

   >List of cuts, RA-vel or DEC-vel ? [L]/X/Y):<cr>
\end{verbatim}
Here, \task{velplot} in its usual cryptic style is giving us a choice of
three options with the default option -- which we chose by just hitting
carriage return, is in square brackets.
\begin{verbatim}

   --- Enter list of pos-vel cuts to plot ---
   angles measured from north to east (0 to 180).
   >Type -n to delete cut n
         -99 to delete all
         L to list         (ie list the already chosen cuts)
         <cr> to use the current list.
   >Enter cut number, position and angle (n,x,y,pa):1,0,-60,40
\end{verbatim}
We think the galactic centre is at 0,-60 arcsec with respect to
the plot centre.
\begin{verse}
\verb+>Enter cut number, position and angle (n,x,y,pa):<cr>+
{\em No more cuts wanted}\\
\verb+>Enter convolving beam (major("),Minor("),pa(deg):<cr>+{\em No convolution requested}\\
\verb+Gaussian falls to 6% at edge of array+\\
\verb+	size of convolution array:  3 pixels,  90.000 arsecs+\\
\verb+Contour or Intensity plots ? [C]/I:<cr>+\\
\end{verse}
Here is where \task{velplot} can get very confusing. \verb+C+ and \verb+I+
are options for
quite different functions, not like grey scale or contour options for
the same plot. The function we want corresponds to the C option -
hence the carriage return.
\begin{verbatim}

   >Enter number of windows in x and y: [1,1]: 
   <cr>
\end{verbatim}
Notice a couple of things here. \task{velplot} inserted the carriage
return after the prompting
message and left us dangling there(slap on wrist to the programmer) but it
is waiting for input so we enter return to choose the \verb+[1,1]+ option,
i.e. single plot to the page (like \verb+nxy+ option in much of the
rest of \miriad) so something will happen. Something does happen,
the pgplot window with the position-velocity map appears on the screen.
After dismissing the pgplot window we see some text about the plot on the screen
and velplot continues as follows:
\begin{verse}
\verb+Another PGPLOT device ? (Y/[N])Y      +{\em The last Y is our response}\\
\verb+>Enter new PGPLOT device:pvplot.ps/ps +{\em Usual convention for a postscript plot}\\
\verb+>Enter line width [1]:<cr>            +{\em Single width lines}\\
\ \\
\verb+>Enter number of windows in x and y: [1,1]:<cr>+\\
\end{verse}
Now some \task{velplot} output about the plot appears on the screen. If
you want several
position-velocity cuts plotted on the one page you keep answering the
``Enter cut number etc'' question giving each successive cut a higher number
and \task{velplot} will make a sensible choice of page layout.
\begin{verse}
{\verb+Another PGPLOT device? (Y/[N])<cr>+ {\em We do not want any more}}
\end{verse}

Now we are finished and the main menu appears again. On exit we find the
postscript file \verb+pvplot.ps+ in our directory.


Still in the position-velocity option, let us 
go back to the question
\begin{verbatim}
   Contour or Intensity plots ?[C]/I:
\end{verbatim}
and answer \verb+I+. Instead of the standard position-velocity plot we are now
requesting plots of intensity versus position at position angles
{\em in certain \MM{(x,y)} image planes that will be requested}.
This is a very different
function from the standard position-velocity plot that comes from a 3-dimensional
slice through the cube. So suppose we have entered 4 cuts:
\begin{verbatim}
   1,0,0,40
   2,100,100,30
   3,-100,-100,80
   4,500,-500,70
\end{verbatim}
(to nominate an arbitrary set), the dialogue with \task{velplot} would be
\begin{verse}
\verb+Contour or Intensity plots ? [C]/I: I+\\
\verb+How many channels ? (5 max) :3       +{\em Data from 3 channels to be plotted}\\
\verb+Enter the channel numbers : 25,30,35 +{\em Channel numbers 25,30,35 only}\\
\verb+Fix the scales ? Y/[N] : <cr>        +{\em As before}\\
\verb+>Enter number of windows in x and y: [2,2]:+
\verb+<cr>+
\end{verse}
We asked for 4 cuts but the cuts are now in the x,y planes of the
specified channels. There will be 4 subplots and each subplot will have
3 lines plotted, one for each specified channel number. And now here is where it
gets confusing. The PGPLOT window appears but only one of the 4 subplots.The
PGPLOT window is hiding a question being asked by \task{velplot} of the form:
\begin{verbatim}
   >Enter name for ASCII file(<cr> to continue):
\end{verbatim}
So give a carriage return and keep answering with carriage returns
to the text window and watch successive subplots appear on the screen.
\task{velplot} will offer the chance to make a postscript file as before and you
will have to go through answering the question about the ASCII file
for each subplot. Cumbersome but that is the way it is.
The ASCII file would
be a text list of the values in the plots , if you wanted such.

\item[Spectra] Plot spectra. A spectrum is a plot of intensity vs position
along a line parallel to the \MM{z} (velocity) axis. This is an extremely useful
function of \task{velplot} but quite complicated. We will explain via an
example:
\begin{verbatim}
   Select option (type 1st character) :s

   Plot spectra at selected positions.
   (x,y) positions are in (HA,DEC) directions.
   >Type H for Help, E to Exit, <cr> to continue :<cr>
   --- no current selection of spectra ---

   >Enter G to enter a grid of positions,
          <cr> to edit or enter a list of positions :g
\end{verbatim}
So we have chosen to define a grid, the most complicated option.
Had we given just a carriage return we would have been offered the opportunity
to make a list of positions similar to what is done for Position-Velocity plots.
\begin{verbatim}

   >Enter tlc (x,y), and PA of grid:
   100,200,0
\end{verbatim}
So we are saying that the top left corner of the rectangular grid of
positions at which we want spectra is at HA=100arcsec and DEC=200arcsec
wrt to the centre of the plot. Our choice of PA=0 here means the grid has axes
parallel to the x,y axes. If we had chosen PA=45 the whole grid would have
been rotated through 45 degrees.
\begin{verbatim}

           (Grid PA is measured from N through E)
   >Enter interval(arcsec), nrows(along PA) and ncols:
   100,2,2
\end{verbatim}
So we are asking for a 2 X 2 grid with 100arcsec spacing between elements.
\begin{verbatim}
   --- current selection of spectra ---
    spectra(1) (x,y) arcsecs: (  100.000,  200.000)
    spectra(2) (x,y) arcsecs: (  200.000,  200.000)
    spectra(3) (x,y) arcsecs: (  100.000,  100.000)
    spectra(4) (x,y) arcsecs: (  200.000,  100.000)

\end{verbatim}\begin{verse}
\verb+selection OK? ([Y]/N):<cr>            +{\em Accept default- that is our list}\\
\verb+GAUSSIAN FITS TO SPECTRA+\\
\verb+>Enter convolving beam etc etc :<cr>  +{\em We do not want a convolving beam}\\
{\em Another 5 lines of text are output}\\
\verb+>Smooth spectra (H=Hanning, B=Boxcar, [None]) :<cr> +{\em No smoothing thanks}\\
\verb+>Omit plots and Gaussian fits ? (Y/[N]) :<cr> +{\em We want the plots}\\
\verb+>Fix scale for spectra ? (Y/[N] :<cr>+\\
\end{verse}
Had we said \verb+Y+ here we would have been offered the opportunity
to set the vertical scale in Jy - like the \verb+range+ in \task{cgdisp}
\begin{verbatim}

   >Enter number of windows in x and y: [2,2]:
   <cr>
\end{verbatim}
So \task{velplot} sensibly offers a 2 by 2 format for the 4 spectra we
have specified.
What follows next can be very confusing. The PGPLOT window appears and the
first spectrum is plotted in the top left corner as expected but nothing
more. Depending on your screen setup, the PGPLOT window is probably obscuring
your text output and you do not see that \task{velplot} has asked
\begin{verbatim}
   >Continue with gaussian fit ([Y]/N) :n
\end{verbatim}
and we say No because we do not want
a gaussian fit. We told it that before but still we have to go through this to
get all the spectral plots on the screen. The same question is asked repeatedly
by \task{velplot} and we answer No each time as we watch successive subplots
appear on the PGPLOT screen. Finally, after all the subplots have appeared the
same question is asked yet again and when we say No the PGPLOT screen disappears
and velplot continues as follows:
\begin{verse}
\verb+Another PGPLOT device? (Y/[N])Y    +{\em Yes, lets get a postscript file}\\
\verb+>Enter filename for spectra (<cr> to continue):+\\
\verb+spectra.ps/ps                      +{\em Call the file spectra.ps}\\
\end{verse}
So now we are finished this spectrum plotting exercise. You may get the main
menu again or you may be returned to \miriad\ and have to restart \task{velplot}
if you want. \verb+spectrum.ps+ can now be plotted.

Another example of plotting spectra:
Suppose you have a galactic disk data cube and you want to plot
spectra at positions along the major axis. Then the grid needs to be collapsed
into a line and rotated to the PA of the disk major axis (say 40 degrees).
You think the position x,y=-1200,1000 is on the major axis toward the top-left
corner 
of the x,y plane. You want 20 plots at 100arcsec intervals along the major
axis. So the inputs to \task{velplot}/\verb+spectra+ would be:
\begin{verbatim}
   choose grid- g
   >Enter tlc (x,y), and PA of grid :
   -1200,1000,40
   >Enter interval(arcsec), nrows(along PA) and ncols:
   100,20,1
\end{verbatim}

\task{velplot} would offer a 5 by 5 plot format but use only the top four rows
(for the 20 plots actually 5 by 4)

\item[Vel-map] Make plots of various moments of velocity averaged over
specified velocity ranges. This works like the position-velocity option
above. Type H for additional documentation. The most useful feature here
is that the cursor can be used to change the plot parameters, or to plot
the positions for spectra, or for position-velocity maps. These CURSOR OPTIONS
are only available if a single velocity interval is selected in Vel-map.
Here is what happens. The cursor appears in the PGPLOT window, and the CURSOR OPTIONS
must be entered by typing a single character in the PGPLOT window. The results, or
additional questions appear in the main window. It is easy to get confused here.
\begin{verbatim}
   >CURSOR OPTIONS: Type L to list options :
\end{verbatim}

so we type L in the PGPLOT window, and in the main window we see:
\begin{verbatim}
    Cursor options:
    A - absolute coordinates
    C - change contour interval
    D - clear spec/pos-vel stacks
    E - exit from plot
    X - exit from plot
    G - change grayscale
    H - header label
    J - Jy contours
    K - Kelvin contours
    N - negative contours
    P - define pos-vel cut
    Q - Plot spec/pos-vel positions
    I - integral and rms in box
    B - blc for integral
    T - trc for integral
    R - replot maps
    S - Spectra position
    V - value and cursor position
    W - write out this map
   (x,y) positions are in (HA,DEC) directions.
   (Position angle is measured from N through E)
   >CURSOR OPTIONS: Type L to list options :
\end{verbatim}

We can enter a spectra position with the cursor by typing S, and position-velocity cuts
by typing P at any two positions along the desired cut. The lists of spectral positions
and position-velocity cuts are stored and can be plotted after you return to the main
menu.

Typing Cursor option B and T to define a box, followed by I, prints out the
Total Flux, Maximum, Minimum, Average, and rms in the box in both Jy and Kelvin units.

Type E or X to exit from the cursor options.

\item[Write] Write \miriad\ image to disk file. It does just this after
prompting for a new name ie it gives you a copy of the region (or all) the
input data.

\item[File] Write file of spectra and position-velocity cuts.
Suppose you have been doing pos-vel and or spectra then there will exist
lists of cuts. This option enables you to save the lists to a named
file so you could refer to them on a subsequent running of \task{velplot}.

\item[Read] Read file of spectra and position-velocity cuts
You saved the lists in a previous WA and now you can retrieve them.

The File and Read options allow you to have several lists of cuts and
access them alternately without leaving \task{velplot}.
\end{description}

\section{Modelling galactic disks}
\subsection*{Fitting a rotation curve}
\task{velfit} is a task to fit a rotation curve to an isovelocity
image of a rotating disk. This is classical ``moment analysis''.

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf VELFIT}}\\
\hline
in=inten,veloc	& \\
centre=0,-60	& Rotation centre is 60 arcsec south of map centre. \\
pa=40		& Position angle of major axis. \\
incline=65	& The believed inclination of the disk. \\
vsys=440	& Systemic velocity of the galaxy. \\
log=rot.log	& File to hold results - defaults to terminal. \\
\hline
\end{tabular}
\end{center}
\verb+inten+ is the \MM{(x,y)} intensity distribution of \verb+data.cube+
integrated over the \MM{z} (velocity) axis. It is obtained using \task{moment}
as follows:
\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MOMENT}}\\
\hline
in=data.cube	& \\
out=iten	& \\
mon=-1		& Get average intensity. \\
axis=3		& Squash along the third axis\\
clip=-2.0,0.0	& Or whatever.\\
\hline
\end{tabular}
\end{center}
\verb+veloc+ is an \MM{(x,y)} model for the velocity values, a mean velocity
image. It would be obtained as follows:

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MOMENT}}\\
\hline
in=data.cube	& Input dataset. \\
out=veloc	& The output dataset.\\
mon=1		& First order moment.\\
axis=3		& Squash along the third axis\\
clip=-2.0,0.0	& Or whatever.\\
\hline
\end{tabular}
\end{center}

\subsection*{Fitting a tilted ring model to a warped galactic disk}
Here we illustrate typical use of the \miriad\ tasks
\task{velmodel} and \task{velimage}. To make the tilted ring model one must
utilise
also \task{imgen} and \task{maths} (see Sections~\ref{s:imgen} and
\ref{s:maths}). The warp is simulated by varying
the inclination and position angles of successive annuli (usually suggested
from various plots from \verb+data.cube+ such as closure of isovelocity contours
along the major axis and twists in channel maps showing the minor axis).
The velocity rotation profile in the plane of the disk may be suggested
by inspection of position-velocity maps made with velplot. For the purpose of
this exercise we will assume a flat rotation velocity profile in the plane
of the disk of 150 \kms. So we make the 3-dimensional data cube for each tilted
ring with \task{velimage} and add them together with \task{maths}.

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf VELIMAGE}}\\
\hline
in=aaa,vvv	& \\
sigma=10	& Velocity dispersion,mandatory if no 3rd image above\\
nchan=64	& Number of velocity channels in the output cube\\
start=230	&Velocity at channel 1\\
step=6.6	&Velocity increment between channels\\
out=model.cube	&The output cube\\
options=relax	&Usually necessary in this type of scheme.\\
\hline
\end{tabular}
\end{center}

Obviously, prior to running \task{velimage}, we must have generated
the input files \verb+aaa+ and \verb+vvv+ which contain our assumptions about
the variation in intensity and velocity in our model.

\verb+aaa+ is the \MM{(x,y)} intensity distribution of what will be
\verb+model.cube+
integrated over the \MM{z}-axis. In this example it is an elliptical annulus of
uniform intensity made by subtracting an inner disk from an outer disk using
\task{maths}, the disks having been made using \task{imgen} as follows :

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMGEN}}\\
\hline
out=aao		& Output dataset.\\
object=disk	& Disk model.\\
spar=0.01,0,-60,1000,500,30 & Source parameters, 10 mJy intensity annulus,\\
			    & centred 60 arcsec south of the image centre,\\
			    & major axis=1000 arcsec,minor axis=500 arcsec\\
			    & major axis pa=30 degrees\\
imsize=128,128		    & Image size.\\
cell=30,30		    & Cell size in arcsec.\\
\hline
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMGEN}}\\
\hline
out=aai		& The outer dataset.\\
object=disk	& Disk model.\\
spar=0.01,0,-60,1000,500,30 & Source parameters, 10 mJy intensity annulus,\\
			    & centred 60 arcsec south of the image centre,\\
			    & major axis=1000 arcsec,minor axis=500 arcsec\\
			    & major axis pa=30 degrees\\
imsize=128,128		    & Image size.\\
cell=30,30		    & Cell size in arcsec.\\
\hline
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf IMGEN}}\\
\hline
out=aai		& The inner disk.\\
object=disk	& Disk model again.\\
spar=0.01,0,-60,800,400,30 & Similar parameters, but smaller radii.\\
imsize=128,128	& Same image size and \\
cell=30,30	& resolution. \\
\hline
\end{tabular}
\end{center}

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf MATHS}}\\
\hline
exp=(aao-aai)	& Generate a annulus.\\
out=aaa		& Output dataset name.\\
\hline
\end{tabular}
\end{center}

\verb+vvv+ is a model for the velocities (\MM{z}-axis) made using \task{velmodel}.

\begin{center}
\begin{tabular}{|l|l|}
\hline
\multicolumn{2}{|c|}{{\bf VELMODEL}}\\
\hline
in=data.cube	& Template input.\\
pa=30		& \\
incline=65	& \\
radius=30	& \\
vrot=150	& \\
vsys=440	& \\
out=vvv		& \\
\hline
\end{tabular}
\end{center}

Note that the input (\verb+data.cube+ in the above) a template only but if
you use your actual data cube you will finally end up with a model
with much of the header information the same as the data, e.g. it will
appear to be in the same position in the sky.

One makes a number of such \verb+model.cubes+ and adds them together
with \task{maths} to obtain the final 3-dimensional tilted ring model.


\section{Zeeman Analysis}\index{zeestat}\index{zeesim}\index{Zeeman analysis}

There is a suite of tasks to help analyse Zeeman data.  These tasks
implement the techniques discussed in gruesome detail
by \htmladdnormallink{Sault, Killeen, Zmuidzinas \& Loushin (1990)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1990ApJS...74..437S}
(ApJS 74, 437) and by 
\htmladdnormallink{Killeen, Lo \& Crutcher (1992)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1992ApJ...385..585K}
(ApJ 385, 585). 


\begin{itemize}

\item \task{zeestat}:
In the small-splitting Zeeman analysis, our goal is generally 
to look for the characteristic ``S'' Zeeman signature
in the Stokes \MM{V} spectrum.  It can be shown that this
spectral signature is more accurately expected to follow the
equation

\begin{displaymath}
V(\nu) = k B \cos \theta {dI(\nu) \over {d\nu}}
\end{displaymath}

where \MM{k} is a constant depending upon the particular spectral line,
$B \cos \theta$ is the line-of-sight component of the magnetic field, and
$I(\nu)$ is the total intensity spectrum.  The procedure is then to fit
the derivative of the \MM{I} spectrum to the \MM{V} spectrum thus giving
$B \cos \theta$. 

Zeeman work can be very complicated, especially if the signal is weak
(and the signal-to-noise ratio parameter $\eta$ -- see Sault et al. -- is close
to the noise value), and a description of all the pros and cons of the
fitting techniques is beyond this guide.  We suggest you read the above
papers, and if you are still awake at the end, then run the software. 

Generally, the fit is done over some spatial and spectral region in
which it is assumed the splitting is constant.  In the spatial averaging
technique, all the spectra in the spatial region are averaged. 
However, if the spectra are not all similar in shape, this will average
down the signal as well as the noise.  Thus, the spatial summing
technique is also offered.  Here, all the spectra in the spatial area
are fit simultaneously so that the signal is not averaged down. 
However, one has to be concerned with the spatial correlation of the data
when predicting the errors in this case.  This is a very big concern
indeed (see \task{Zeesim} below).

The fitting procedure is implemented in the task \task{zeestat}.  This
task expects the \MM{I} (keyword {\tt iin}) and \MM{V} (keyword {\tt
vin}) input cubes to be ordered such that the first axis is the spectral
axis (frequency or velocity [radio or optical definition]) and the
second and third axes are the spatial axes (RA and DEC).  Use the
\task{reorder} if they are not.  The beam image (keyword {\tt beam}) is
used to determine the spatial correlation when using the spatial summing
technique in order to get a good estimate of the error in the result (it
does not affect the result itself much).  However, generally, this does
not work well and accurate errors for spatially summed data should be
obtained with a monte carlo technique implemented in the task
\task{zeesim} described below.  The beam is not needed when spatially
averaging. 

If the spectra are spectrally correlated, this information can be
transmitted to \task{zeestat} via the spectral correlation coefficient
assigned to the keyword {\tt rho}.  For Hanning smoothed spectra in
which every other channel has been dropped, this should be \MM{1/6}.  You
must specify with {\tt freq}, to the nearest MHz, the rest frequency
of the spectral line.  This is used to determine the conversion from
splitting in channels to a magnetic field.  \task{zeestat} only knows
about a few particular transitions.  If it does not know about yours,
come and tell us, and we will add it in for you. 

You can read endlessly in the above papers about the pros and cons of
least squares and maximum likelihood fitting techniques.  All the
fitting techniques are biased, in the sense that they give a splitting
biased towards zero in cases of poor signal-to-noise ratios.  The
maximum likelihood technique is the least biased, so we suggest you use
that ({\tt mode=m}).  It is also common to fit for a leakage term
allowing for a fraction of the \MM{I} spectrum in the \MM{V} spectrum ({\tt
mode=l}).  Because, in poor signal-to-noise ratio cases, there can be
spurious minima, we suggest you set {\tt mode=x} to perform extra checks
in finding the global minimum.  Generally, sharp edges in spectra
increase your chances of detection (because this gives a large
derivative).  If you set {\tt mode=2}, \task{zeestat} will work out a
two-sided derivative of the \MM{I} spectrum.  Otherwise it uses a one-sided
derivative.  The former gives a lower noise derivative but performs less
well for sharp spectral features.  In summary, we suggest you set {\tt
mode=mxl} or maybe {\tt mode=mxl2}. 

Set keyword {\tt aveop=s} for spatial summing and and {\tt aveop=a} for
spatial averaging.  \task{zeestat} is rather an elderly task coded at
the dawn of time.  It does not know about the {\tt region} keyword. 
Thus you input the spectral range to fit with the {\tt chan} keyword,
and the spatial area via the {\tt blc} (bottom left corner) and {\tt
trc} (top right corner) keywords.  These are all in absolute pixels. 
You can write the results to an ascii file with the {\tt out} keyword. 

Here is a simple example of a spatially summed fit of spectrally
uncorrelated OH main line spectra over channels 200 to 300 (only include
channels that have strong \MM{I} signal) and over some small
spatial region.  


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf ZEESTAT}} \\
\hline 
iin=sgra.Ivxy     & Input \MM{I} cube \\
vin=sgra.Vvxy     & Input \MM{V} cube \\
beam              & Unset as technique does not work \\
rho               & Unset \\
freq=1.667        & Rest frequency in GHz \\
mode=mxl          & Select mode \\
aveop=s           & Sum in spatial region \\
chan=200,300      & Channel range to fit \\
blc=240,255       & Spatial BLC to fit \\
trc=260,278       & Spatial TRC to fit \\
log               & Unset \\
out=log.out       & Write results to this file \\
\hline 
\end{tabular}
\end{center}


\item The task \task{zeesim} is used to get reliable estimates of the
error in the fitted magnetic field derived by \task{zeestat}.  See
\htmladdnormallink{Killeen, Lo \& Crutcher (1992)}
{http://adsabs.harvard.edu/cgi-bin/nph-bib_query?bibcode=1992ApJ...385..585K}
(ApJ, 385, 585) for gruesome details.  It is used when
you have invoked spatial averaging, or when the signal-to-noise ratio
is very poor for spatial averaging.  The technique involves taking the
actual spectra and fitting them, thus producing an estimate of the noise
free true spectra.  Then \task{zeesim} adds noise (correlated by the beam if
simulating spatial summing) back into the spectra and refits them to
get the splitting.  The noise addition and refitting is performed as
many times as is necessary to get a good histogram of the splitting. 
The width of that histogram gives a good error estimate. 

\task{zeesim} can work out the fiddle factor to apply to the error
estimates of \task{zeestat} for may spatial regions at once.  It is much
more efficient for spatial summing to use \task{zeesim} like this than
to do one run per spatial region.  This is because the step of producing
noise correlated by the beam is very expensive.  Multiple spatial
regions can be input via a text file assigned to the keyword {\tt
infile}.  A single spatial region can be entered through the usual {\tt
blc} and {\tt trc} keywords. 

In this example, we run the simulation appropriate to the example of
\task{zeestat} above.  You should do as many simulations as is necessary
to get a decent histogram or until your computer tires. 

It is important to use the {\tt nran} keyword if you restart the
simulations and want to use the results in conjunction with a previous
run.  Computers only generate semi-random numbers.  If you give the
generator the same seed you get the same random numbers.  {\tt nran}
allows you to throw some random numbers before beginning the simulation
procedure so you can get different noise to last time you ran the
program. 


\begin{center}
\begin{tabular}{|l|l|} 
\hline 
\multicolumn{2}{|c|}{{\bf ZEESIM}} \\
\hline 
iin=sgra.Ivxy     & Input \MM{I} cube \\
vin=sgra.Vvxy     & Input \MM{V} cube \\
beam=sgra.ivbem   & Give the beam \\
mode=mxl          & Select fitting mode \\
aveop=s           & Sum in spatial region \\
freq=1.667        & Rest frequency in GHz \\
chan=200,300      & Channel range to fit \\
blc=240,255       & Spatial BLC to fit \\
trc=260,278       & Spatial TRC to fit \\
bin               & Unset \\
split             & Unset \\
nruns=500         & Number of simulations \\
                  & How big is your computer \\
infile            & Unset \\
log=res,summ      & Log files for results \\
nran=\#            & Throw away random numbers before simulating \\
\hline 
\end{tabular}
\end{center}


\task{zeesim} gives you what it calls a ``fiddle factor''.  You
should multiply the error estimate from \task{zeestat} by this
factor to get the true error estimate.

\end{itemize}
