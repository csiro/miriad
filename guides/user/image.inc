%  LaTeX include file
%
%  Check VOBS: is that really V of the OBSERVATORY???
%
%  History:	xx-xxx-89  Bob Sault 	Original written
%		26-jul-90  pjt		Let's see how latex table looks???
%		19-feb-91  pjt		fixed date format and RA---SIN
%		15-dec-92  rjs		say crval and cdelt are doubles.
%		28-Oct-93  rjs		Comments about bpa.
%		13-jan-99  rjs		Added cellscal,pbtype,llrot
\chapter{Image Items}
\label{app:image}

In this Appendix we will describe the items that can be present
in an image dataset. \miriad\ utilities such as {\tt prthd}, {\tt imlist},
{\tt itemize}, {\tt puthd},
{\tt copyhd} and {\tt delhd} may
be used to browse or modify these `header variables'.

Note that the units used in \miriad\ are not always FITS-like, 
{\it e.g.} frequencies are stored in GHz in \miriad, whereas in FITS
one finds Hz. \index{FITS} For example, \miriad\ keeps
velocity in \kms, sky coordinates in radians and frequencies
in GHz.

\small
\begin{table}
\caption{Item names in \miriad\ image datasets}
\begin{tabular}{|l|l|c|l|} \hline
&&&\\
Item                & Type      & Units   & Description/Comments \\ 
&&&\\\hline
bmaj,bmin           & real      & radians & Beam major and minor axis FWHM.\\
bpa                 & real      & degrees & Beam position angle, measured\\
		    &		&	  & east from north.\\
bunit               & character &         & The units of the pixels. \\
btype		    & character &	  & The type of the image. Possible values:\\
		    &		&	  & \ \ \verb+intensity+ -- Normal map.\\
		    &		&	  & \ \ \verb+beam+ -- Point spread function.\\
		    &		&	  & \ \ \verb+depolarisation_ratio+  \\
		    &		&	  & \ \ \verb+fractional_polarisation+ \\
		    &		&	  & \ \ \verb+optical_depth+\\
		    &		&	  & \ \ \verb+polarised_intensity+\\
		    &		&	  & \ \ \verb+position_angle+\\
		    &		&	  & \ \ \verb+rotation_measure+\\
		    &		&	  & \ \ \verb+spectral_index+\\
cdelt1,cdelt2,...   & double    & \lookat\ ctype &The increment between pixels. \\
cellscal	    & character &         & Latitude/longitude cell scaling\\
		    &	        &	  & convention with frequency/velocity.\\
		    &		&	  & Possible values are:\\
		    &		&	  & \ \ \verb+CONSTANT+\\
		    &		&	  & \ \ \verb+1/F+\\
crpix1,crpix2,...   & real      & pixels  & The pixel coordinate of the reference pixel. \\
crval1,crval2,...   & double    &         & The coordinate value at the reference pixel. \\
                    &           & radians & \ \ if ctype represents a celestial coordinate.\\
                    &           & \kms    & \ \ if ctype {\tt VELO} or {\tt FELO}\\
                    &           & GHz     & \ \ if ctype {\tt FREQ}\\
ctype1,ctype2,...   & character &         & The type of the nth axis (as FITS CTYPE). \\
datamin,datamax     & real      & \lookat\ bunit & The minimum and maximum pixel values.\\
epoch               & real      & years & The epoch of the coordinate system.\\
history             & text      &       & A text item containing the history of \\
	            &           &       & \ \ processing performed to the data set.\\
image               & real      & \lookat\ bunit  & The pixel data.\\
llrot		    & double    & radians & Eastward rotation relative to north of the\\
		    &		&	  & sky grid relative to the pixel grid.\\
lstart,lstep,lwidth & real	&	& \\
ltype		    & character &	& The linetype used in making the map.\\
mask                & integer   & \#    & Bitmap used to determine which pixels in \\
                    &           &       & \ \ the image have been blanked. \\
mostable	    & binary	& 	& Mosaic information table.\\
naxis               & integer   & \#    & Number of dimensions. \\
naxis1,naxis2,...   & integer   & \#    & Number of pixels along each dimension. \\
niters              & integer   & \#    & The total number of deconvolution iterations\\
                    &           &       & \ \ performed on the image.\\
object              & character &       & source name \\
observer            & character &       & observers name\\
obsra,obsdec        & double    & radians & The apparent RA and DEC of the observation \\
                    &           &       & \ \ centre. \\
obstime		    & double	& Julian date&  The mean observing time.\\
pbfwhm		    & real	& arcsec& Gaussian primary beam size (deprecated).\\
pbtype		    & character &       & Primary beam type. Standard values include:\\
		    &		&	& \ \ {\tt HATCREEK, VLA, ATCA, WSRT}\\
restfreq            & double    & GHz   & The rest frequency of the observed data.\\
telescop            & character &       & Telescope name. Standard values include:\\
		    &		&	& \ \ {\tt HATCREEK, VLA, ATCA, WSRT}\\
vobs                & real      & \kms  & Velocity of the observatory \\
                    &           &       & \ \ with respect to the rest frame,\\
                    &           &       & \ \ during the observation. \\
\hline
\end{tabular}
\end{table}
\normalsize
 
