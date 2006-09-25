\section{The ATNF Visualisation Software}\label{s:visualisation} The
ATNF visualisation software is a suite of tools (outside \miriad)
specifically aimed at displaying spectral-line data cubes. See
Chapter~\ref{c:pltvcon} for more information.
The tools available are:

\begin{itemize}

\item {\tt kvis}: This tool allows you to inspect one or two 2- or
3-dimensional datasets in several ways.  This tool is partially
described in Chapter~\ref{c:display}.

\item {\tt kslice\_3d}: This tool allows you to interactively
inspect data cubes.  The RA-DEC, RA-VEL and DEC-VEL slices of the data
cube are shown simultaneously, and by moving the cursor one moves
through the data cube.

\item {\tt xray}: This tool does volume
rendering of data cubes.  This is very useful for getting an overview
of the emission in data cubes, as well as for finding small errors in
continuum subtraction or the cleaning of the cube.

\item {\tt kpvslice}: This tool allows you to interactively
display Position-Velocity slices through a datacube. It provides a
window in which you can load an image (say an optical image or a
moment map) and draw a slice across the sky. In the other window the
corresponding slice through the datacube is displayed. This slice may
be exported to \miriad\ for further analysis.

\item {\tt krenzo}: This tool will draw a contour map for every
channel in a datacube over an image, using a different colour for each
channel. This is a much better way of showing the velocity structure
than using 1st moment maps, since a 1st moment map cannot show
multiple peaks in a velocity profile.

\item {\tt kshell}: This tool computes and displays
Velocity-Radius images from a datacube, allowing you to
interactively browse for expanding shells. An expanding shell in a
datacube appears as an ellipse in a Velocity-Radius image. This is an
easy way of distinguishing between spherical shells and those which
have distorted shapes (i.e. a gas bubble which has blown out through a
galactic disc).

\item {\tt koords}: This tool allows you to easily add a
coordinate system to an image (say a CCD image), by using a reference
image (perhaps from the DSS). You select matching objects
(stars) in the two images, and then press a button to compute a
coordinate system for your target image.

\end{itemize}
